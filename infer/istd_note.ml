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
  module ForHashtbl (H: Caml.Hashtbl.S) = struct
  end
end
