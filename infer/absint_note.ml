open Istd_note
open Core

module AbstractDomain = struct
  module Types = struct
    type 'astate bottom_lifted = Bottom | NonBottom of 'astate

    type 'astate top_lifted = Top | NonTop of 'astate

    type ('below, 'above) below_above = Below of 'below | Above of 'above
  end

  open Types

  exception Stop_analysis

  module type NoJoin = sig
    type t

    val ( <= ) : lhs:t -> rhs:t -> bool
  end

  module type S = sig
    include NoJoin

    val join : t -> t -> t

    val widen : prev:t -> next:t -> num_iters:int -> t
  end

  module Empty : S with type t = unit = struct
    type t = unit

    let ( <= ) ~lhs:() ~rhs:() = true

    let join () () = ()

    let widen ~prev:() ~next:() ~num_iters:_ = ()
  end

  module type WithBottom = sig
    include S

    val bottom : t

    val is_bottom : t -> bool
  end

  module type WithTop = sig
    include S

    val top : t

    val is_top : t -> bool
  end

  module BottomLiftedUtils = struct
    let ( <= ) ~le ~lhs ~rhs =
      if phys_equal lhs rhs then true
      else
        match (lhs, rhs) with
        | Bottom, _ ->
            true
        | _, Bottom ->
            false
        | NonBottom lhs, NonBottom rhs ->
            le ~lhs ~rhs

    let map ~f astate =
      match astate with
      | Bottom ->
          astate
      | NonBottom a ->
          let a' = f a in
          if phys_equal a' a then astate else NonBottom a'
  end

  module BottomLifted (Domain : WithBottom) = struct
    type t = Domain.t bottom_lifted

    let bottom = Bottom

    let is_bottom = function Bottom -> true | NonBottom _ -> false

    let ( <= ) = BottomLiftedUtils.( <= ) ~le:Domain.( <= )

    let join astate1 astate2 =
      if phys_equal astate1 astate2 then astate1
      else
        match (astate1, astate2) with
        | Bottom, _ ->
            astate2
        | _, Bottom ->
            astate1
        | NonBottom a1, NonBottom a2 ->
            PhysEqual.optim2 ~res:(NonBottom (Domain.join a1 a2)) astate1 astate2

    let widen ~prev:prev0 ~next:next0 ~num_iters =
      if phys_equal prev0 next0 then prev0
      else
        match (prev0, next0) with
        | Bottom, _ ->
            next0
        | _, Bottom ->
            prev0
        | NonBottom prev, NonBottom next ->
            PhysEqual.optim2 ~res:(NonBottom (Domain.widen ~prev ~next ~num_iters)) prev0 next0

    let map = BottomLiftedUtils.map
  end

  module TopLiftedUtils = struct
    let ( <= ) ~le ~lhs ~rhs =
      if phys_equal lhs rhs then true
      else
        match (lhs, rhs) with
        | _, Top ->
            true
        | Top, _ ->
            false
        | NonTop lhs, NonTop rhs ->
            le ~lhs ~rhs
  end

  module TopLifted (Domain : WithTop) = struct
    type t = Domain.t top_lifted

    let top = Top

    let is_top = function Top -> true | _ -> false

    let ( <= ) = TopLiftedUtils.( <= ) ~le:Domain.( <= )

    let join astate1 astate2 =
      if phys_equal astate1 astate2 then astate1
      else
        match (astate1, astate2) with
        | Top, _ | _, Top ->
            Top
        | NonTop a1, NonTop a2 ->
            PhysEqual.optim2 ~res:(NonTop (Domain.join a1 a2)) astate1 astate2

    let widen ~prev:prev0 ~next:next0 ~num_iters =
      if phys_equal prev0 next0 then prev0
      else
        match (prev0, next0) with
        | Top, _ | _, Top ->
            Top
        | NonTop prev, NonTop next ->
            PhysEqual.optim2 ~res:(NonTop (Domain.widen ~prev ~next ~num_iters)) prev0 next0
  end

  module Pair (Domain1 : S) (Domain2 : S) = struct
    type t = Domain1.t * Domain2.t

    let ( <= ) ~lhs ~rhs =
      if phys_equal lhs rhs then true
      else
        Domain1.( <= ) ~lhs:(fst lhs) ~rhs:(fst rhs)
        && Domain2.( <= ) ~lhs:(snd lhs) ~rhs:(snd rhs)

    let join astate1 astate2 =
      if phys_equal astate1 astate2 then astate1
      else
        PhysEqual.optim2
          ~res:(Domain1.join (fst astate1) (fst astate2), Domain2.join (snd astate1) (snd astate2))
          astate1 astate2

    let widen ~prev ~next ~num_iters =
      if phys_equal prev next then prev
      else
        PhysEqual.optim2
          ~res:
            ( Domain1.widen ~prev:(fst prev) ~next:(fst next) ~num_iters
            , Domain2.widen ~prev:(snd prev) ~next:(snd next) ~num_iters )
          prev next
  end

  module type Equatable = sig
    include S

    val equal : t -> t -> bool
  end

  module Flat (V : Equatable) = struct
    type t = Bot | V of V.t | Top

    let bottom = Bot

    let is_bottom = function Bot -> true | _ -> false

    let top = Top

    let is_top = function Top -> true | _ -> false

    let ( <= ) ~lhs ~rhs =
      phys_equal lhs rhs
      ||
      match (lhs, rhs) with
      | Bot, _ | _, Top ->
          true
      | Top, _ | _, Bot ->
          false
      | V v1, V v2 ->
          V.equal v1 v2

    let join a1 a2 =
      match (a1, a2) with
      | Top, _ | _, Top ->
          Top
      | Bot, a | a, Bot ->
          a
      | V v1, V v2 ->
          if V.equal v1 v2 then a1 else Top

    let widen ~prev ~next ~num_iters:_ = join prev next
  end

  module StackedUtils = struct
    let compare x1 x2 ~cmp_below ~cmp_above =
      if phys_equal x1 x2 then 0
      else
        match (x1, x2) with
        | Below b1, Below b2 ->
            cmp_below b1 b2
        | Below _, Above _ ->
            -1
        | Above _, Below _ ->
            1
        | Above a1, Above a2 ->
            cmp_above a1 a2

    let ( <= ) ~le_below ~le_above ~lhs ~rhs =
      phys_equal lhs rhs
      ||
      match (lhs, rhs) with
      | Below lhs, Below rhs ->
          le_below ~lhs ~rhs
      | Below _, Above _ ->
          true
      | Above _, Below _ ->
          false
      | Above lhs, Above rhs ->
          le_above ~lhs ~rhs

    let combine ~dir x1 x2 ~f_below ~f_above =
      match (x1, x2) with
      | Below b1, Below b2 ->
          Below (f_below b1 b2)
      | (Below _ as below), (Above _ as above) | (Above _ as above), (Below _ as below) -> (
        match dir with `Increasing -> above | `Decreasing -> below )
      | Above a1, Above a2 ->
          Above (f_above a1 a2)
  end

  module Stacked (Below : S) (Above : S) = struct
    type t = (Below.t, Above.t) below_above

    let ( <= ) = StackedUtils.( <= ) ~le_below:Below.( <= ) ~le_above:Above.( <= )

    let join = StackedUtils.combine ~dir:`Increasing ~f_below:Below.join ~f_above:Above.join

    let widen ~prev ~next ~num_iters =
      StackedUtils.combine ~dir:`Increasing prev next
        ~f_below:(fun prev next -> Below.widen ~prev ~next ~num_iters)
        ~f_above:(fun prev next -> Above.widen ~prev ~next ~num_iters)
  end
end
