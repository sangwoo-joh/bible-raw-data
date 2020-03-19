open Istd_note
open Core
open IR_note
open Base_note

module FormalMap = struct
  type t = int AccessPath.BaseMap.t
end

module Payloads = struct
  type t =
    { annot_map: unit
    ; biabduction: unit
    ; buffer_overrun_analysis: unit
    ; buffer_overrun_checker: unit
    ; class_loads: unit
    ; cost: unit
    ; lab_resource_leaks: unit
    ; litho: unit
    ; pulse: unit
    ; purity: unit
    ; quandary: unit
    ; racerd: unit
    ; siof: unit
    ; starvation: unit
    ; typestate: unit
    ; uninit: unit }
end

module Summary = struct
  (*** should move to backend *)

  module Stats = struct
    type t =
      { failure_kind: SymOp.failure_kind option
            (** what type of failure stopped the analysis (if any) *)
      ; symops: int  (** number of SYmOp's throughout the whole analysis of the function *)
      ; mutable nodes_visited: Int.Set.t }
  end

  module Status = struct
    type t =
      | Pending  (** Summary has been created by the procedure has not been analyzed yet *)
      | Analyzed
  end

  include struct
    type t =
      { payloads: Payloads.t
      ; mutable sessions: int
      ; stats: Stats.t
      ; status: Status.t
      ; proc_desc: Procdesc.t
      ; err_log: Errlog.t
      ; mutable callee_pnames: Typ.Procname.Set.t }
  end
end

module ProcData = struct
  type 'a t = {summary: Summary.t; tenv: Tenv.t; extras: 'a}

  type no_extras = unit
end

module ProcCfg = struct
  (** Control-flow graph for a single procedure (as opposed to cfg.ml, which represents a cfg for a file).
      Defines useful wrappers that allows us to do tricks like turn a forward cfg into a backward one, or view a cfg as having a single instruction per node. *)

  module type Node = sig
    type t

    type id

    val kind : t -> Procdesc.Node.nodekind

    val id : t -> id

    val hash : t -> int

    val loc : t -> Location.t

    val underlying_node : t -> Procdesc.Node.t

    val of_underlying_node : Procdesc.Node.t -> t

    val compare_id : id -> id -> int

    module IdMap : Caml.Map.S with type key = id

    module IdSet : Caml.Set.S with type elt = id
  end

  module type S = sig
    type t

    type instrs_dir

    module Node : Node

    val instrs : Node.t -> instrs_dir Instrs.t
    (** get the instructions from a node *)

    val fold_succs : t -> (Node.t, Node.t, 'accum) Container.fold

    val fold_preds : t -> (Node.t, Node.t, 'accum) Container.fold
    (** fold over all predecessors (normal and exceptional) *)

    val fold_normal_succs : t -> (Node.t, Node.t, 'accum) Container.fold
    (** fold over non-exceptional successors *)

    val fold_normal_preds : t -> (Node.t, Node.t, 'accum) Container.fold
    (** fold over non-exceptional predecessors *)

    val fold_exceptional_succs : t -> (Node.t, Node.t, 'accum) Container.fold
    (** fold over exceptional successors *)

    val fold_exceptional_preds : t -> (Node.t, Node.t, 'accum) Container.fold
    (** fold over exceptional predecessors *)

    val start_node : t -> Node.t

    val exit_node : t -> Node.t

    val proc_desc : t -> Procdesc.t

    val fold_nodes : (t, Node.t, 'accum) Container.fold

    val from_pdesc : Procdesc.t -> t

    val is_loop_head : Procdesc.t -> Node.t -> bool

    val wto : t -> Node.t WeakTopologicalOrder.Partition.t
  end
end

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

  module FiniteSetOfSet (S : Caml.Set.S) = struct
    include S

    let bottom = empty

    let is_bottom = is_empty

    let ( <= ) ~lhs ~rhs = if phys_equal lhs rhs then true else subset lhs rhs

    let join a1 a2 = if phys_equal a1 a2 then a1 else union a1 a2

    let widen ~prev ~next ~num_iters:_ = join prev next
  end

  module FiniteSet (Element : Caml.Set.OrderedType) = FiniteSetOfSet (Caml.Set.Make (Element))

  module MapOfMap (M : Caml.Map.S) (ValueDomain : S) = struct
    include (M : Caml.Map.S with type 'a t := 'a M.t and type key = M.key)

    type t = ValueDomain.t M.t

    type value = ValueDomain.t

    let bottom = empty

    let is_bottom = is_empty

    let ( <= ) ~lhs ~rhs =
      if phys_equal lhs rhs then true
      else
        M.for_all
          (fun k l_v ->
            try ValueDomain.( <= ) ~lhs:l_v ~rhs:(M.find k rhs) with Caml.Not_found -> false)
          lhs

    let increasing_union ~f a1 a2 =
      if phys_equal a1 a2 then a1
      else
        let eq1 = ref true in
        let eq2 = ref true in
        let merge_vals _ v1_opt v2_opt =
          match (v1_opt, v2_opt) with
          | Some v1, Some v2 ->
              let v = f v1 v2 in
              if not (phys_equal v v1) then eq1 := false ;
              if not (phys_equal v v2) then eq2 := false ;
              Some v
          | Some _, None ->
              eq2 := false ;
              v1_opt
          | None, Some _ ->
              eq1 := false ;
              v2_opt
          | None, None ->
              None
        in
        let res = M.merge merge_vals a1 a2 in
        if !eq1 then a1 else if !eq2 then a2 else res

    let join a1 a2 = increasing_union ~f:ValueDomain.join a1 a2

    let widen ~prev ~next ~num_iters =
      increasing_union prev next ~f:(fun prev next -> ValueDomain.widen ~prev ~next ~num_iters)
  end

  module Map (Key : Caml.Map.OrderedType) (ValueDomain : S) = struct
    module M = Caml.Map.Make (Key)
    include MapOfMap (M) (ValueDomain)
  end

  module BooleanAnd : S with type t = bool = struct
    type t = bool

    let ( <= ) ~lhs ~rhs = lhs || not rhs

    let join = ( && )

    let widen ~prev ~next ~num_iters:_ = join prev next
  end

  module BooleanOr : WithBottom with type t = bool = struct
    type t = bool

    let bottom = false

    let is_bottom a = not a

    let ( <= ) ~lhs ~rhs = (not lhs) || rhs

    let join = ( || )

    let widen ~prev ~next ~num_iters:_ = join prev next
  end
end

module TransferFunction = struct
  module type S = sig
    module CFG : ProcCfg.S

    module Domain : AbstractDomain.S

    type extras

    type instr

    val exec_instr : Domain.t -> extras ProcData.t -> CFG.Node.t -> instr -> Domain.t
  end
end

module AbstractInterpreter = struct
  type exec_node_schedule_result = ReachedFixPoint | DidNotReachFixPoint

  (** Maximum number of widens that can be performed before the analysis will intentionally crash. 
    Used to guard against divergence in the case that someone has implemented a bad widening operator *)
  let max_widens = 10000

  module VisitCount : sig
    type t = private int

    val first_time : t

    val succ : pdesc:Procdesc.t -> t -> t
  end = struct
    type t = int

    let first_time = 1

    let succ ~pdesc visit_count =
      let visit_count' = visit_count + 1 in
      if visit_count' > max_widens then failwith "Exceeded max widening threshold" ;
      visit_count'
  end

  module State = struct
    type 'a t = {pre: 'a; post: 'a; visit_count: VisitCount.t}
  end
end
