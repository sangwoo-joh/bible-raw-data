open Core

module ARList = struct
  (*
  Invariants:
  - elements of Concat are not empty (nor singletons)
  - arg of Rev is not empty, nor singleton, nor Rev
  - arg of Snoc is not empty
  ...ensure that:
  - an empty list is always represented by Empty,
  - a singleton is always represented by Cons(_, Empty)
  - the memory footprint is in Theta(length)

  Potential constructors to add later:
  - OfArray of 'a Array.t
  - Flatten of 'a t t
*)
  type +'a t =
    | Empty
    | Cons of 'a * 'a t
    | Snoc of 'a t * 'a
    | Concat of 'a t * 'a t
    | Rev of 'a t
end

module MaximumSharing = struct
  module ForHashtbl (H : Caml.Hashtbl.S) = struct end
end

module PhysEqual = struct
  let rec compare_fields ox oy i =
    i < 0 || (phys_equal (Obj.field ox i) (Obj.field oy i) && compare_fields ox oy (i - 1))

  let shallow_equal x y =
    phys_equal x y
    ||
    let ox = Obj.repr x in
    let oy = Obj.repr y in
    let tx = Obj.tag ox in
    let ty = Obj.tag oy in
    Int.equal tx ty && tx < Obj.no_scan_tag
    &&
    let sx = Obj.size ox in
    let sy = Obj.size oy in
    Int.equal sx sy && compare_fields ox oy (sx - 1)

  let optim1 ~res x = if shallow_equal res x then x else res

  let optim2 ~res x1 x2 =
    if shallow_equal res x1 then x1 else if shallow_equal res x2 then x2 else res
end
