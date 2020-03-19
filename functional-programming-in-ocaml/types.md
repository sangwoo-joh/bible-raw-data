# OCaml Types

## One-of vs. Each-of
 - A variant type is *one of* as set of possibilities. (sum types)
   (disjoint union)
 - A value of a tuple/record type provides *each of* a set of
   possibilities. (product types) (Cartesian product)

## Advanced
### Type Synonyms
 - A *type synonym* is a **new name** for an already existing type.
 - The two are completely exchangeable for one another.

### Parameterized Variants
 - Variant types may be *parameterized* on other types.

``` ocaml
type 'a mylist = Nil | Cons of 'a * 'a mylist
```

 - `mylist` is a **type constructor** but not a type: there is no way
   to write a value of type `mylist`.
     - The one in a variant is called just **constructor**.
 - Think of a type constructor as being like a function, but one that
   maps types to types.
 - A value of type `'a mylist` could have many (poly) forms (morph),
   depending on the actual type `'a` (**parametric polymorphism**)
