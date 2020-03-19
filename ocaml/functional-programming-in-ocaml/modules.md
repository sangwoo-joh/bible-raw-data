# Modular Programming

## Module Systems
### Namespaces
 Enables the name `foo` in one namespace to have a **distinct**
 meaning from `foo` in another namespace.

 In OCaml, there is a lanugage feature called *structures* that is
 used to group names.

### Abstractions
 Hides some information while revealing other
 information. *encapsulation*, or *information hiding*.

 Abstractions describe relationships among modules: there might be
 many modules that could considered to satisfy a given abstraction.

 ... It also enables clients to be blissfully unaware of those
 details. :sweat_smile:

 In OCaml, there is a language feature called *signatures* that is
 used to abstract structures by hiding some of the structures' names.


##  OCaml Modules
### Structures
 + OCaml modules are implemented by `module` definitions.

``` ocaml
module ModuleName = struct
  (* definitions *)
end
```

 + Module names must begin with an uppercase letter.
 + The part of the module definition that is written `struct (*
   definition *) end` is called a *structure*.
 + The structure itself is anonymous - it has no name - until it is
   bound to a name by module definition.
 + Modules partition the namespace.

### Scope
 + You can access the names within the module using `.` operator.
 + The namespace can be exposed using `open`.
   + Opening a module is like writing a local definition for each name
     defined in the module.
 + There is a special module called `Stdlib` that is automatically
   opened in every OCaml program. It contains "built-in" functions and
   operators.
#### Opening a module in a limited scope
 + Any names defined later *shadow* names defined later.

 1. Inside any expression you can locally open a module, such that the
    module's names are in scope only the rest of that expression.

``` ocaml
let open Module in e
```

 2. Syntactic sugar for local opening: `Module.(e)`


### Signatures
 + Module types let us describe groups of related modules.

``` ocaml
module type ModuleTypeName = sig
  (* declarations *)
end
```

  + By convention, the module type name is capitalized, but it does
    not have to be.
  + The part of the module type that is written `sig (* declarations
    *) end` is called a *signature*.
  + The signature itself is anonymous - it has no name - until it is
    bound to a name by a module type definition.
  + The syntax `val id : t` means that there is a value named `id`
    whose type is `t`.
  + A structure *mathces* a signature if the structure provides
    definitions for **all the names** specified in the signature (and
    possibly more), and these definitions meet **the type
    requirements** given in the signature.


### Abstract Types
 + A type is *abstract* when OCaml says that there is a type name, but
   does not say what that type is defined to be.
 + A module that implements a signature (module type) must specify
   **concrete** types for the abstract types in the signature, and
   define all the names declared in the signature.
   + We say that structure is *sealed* by the signature: nothing
     except what is revealed in the signature may be accessed.
 + It is idiomatic OCaml to name the *primary representation type* of
   a data structure simply `t`.

### Sharing Constraints
 + OCaml lets you write *sharing constraints* that refine a signature
   by specifying equations that must hold on the abstract types in
   that signature. If `T` is a module type containing an abstract type
   `t`, then `T with type t = t'` is a new module type that is the
   same as `T`, except that `t` is known to be `t'`.

``` ocaml
module type Arith = sig
  type t
  val zero : t
  val one : t
  val (+) :t -> t -> t
end

(** Constraints not shared: t is abstract *)
module Ints : (Arith) = struct
  type t = int
  let zero = 0
  let one = 1
  let (+) = Stdlib.(+)
end


(** type t is shared as int *)
module Ints : (Arith with type t = int) = struct
  type t = int
  let zero = 0
  let one = 1
  let (+) = Stdlib.(+)
end


(* Both are legal when sharing constraints *)
Ints.(one + one)
Ints.(1 + 1)
```

 + We **don't** have to specify the sharing constraint in the
   *original definition* of module; instead, we can bind it to another
   module name with its types being either abstract or exposed.

``` ocaml
(* ... when Ints is abstract *)

module AbstractInts : Arith = Ints
AbstractInts.(1 + 1) (* illegal *)

module ExposedInts : (Arith with type t = int) = Ints
ExposedInts.(1 + 1) (* legal *)
```
