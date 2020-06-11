# Core ML

## Discovering Core ML

 Core ML is a small *functional* language. Functions are *first-class*
 **values**.

 Objects manipulated by programs are always *countable* (and *finite*
 in practice).

 Expressions of the lambda-calculus (`a`) are of three possible forms:
  1. variables `x` (elements of a countable set)
  2. functions `\x. a`
  3. applications `a1 a2`

 In addition, core ML has a distinguished construction `let x = a1 in
 a2` used to bind an expression `a1` to a variable `x` within an
 expression `a2`.

 Furthermore, it comes with primitive values such as integers, floats,
 strings, etc. (`c`)


## The Syntax of Core ML

 Numbers, strings, but also lists, pairs, etc. as well as operations
 on those values can all be treated as *constants* and *applications*
 of constants to values.

 A collection of constants `c` in `C` that are partitioned into
  - **constructors** `C` in `C+`: *passive*. they may take arguments,
    but should ignore their shape and simply build up larger values
    with their arguments embedded.
  - **primitives** `f` in `C-`: *active*. they may examine the shape
    of their arguments, operate on inner embedded values, and
    transform them.

 Constants come with an *arity*.


## The Dynamic Semantics of Core ML
 - An *operational semantics*: describing the computation process. It
   relates programs as *syntactic objects*. Focus on the evaluation
   process and its correctness.
 - An *denotational semantics*: building a mathematical structure
   whose objects (*domain*) are used to represent the meanings of
   programs.
 - *Values*: the subset of answers expected from *normal* evaluations.
 - A *reduction* semantics: a subset of programs and the semantic
   relation is defined as the transitive clousre of small-step
   internal binary relation (reduction) between programs. Also known
   as *small-step* style.
   
### Reduction Semantics
 - **Values** are either functions, constructed values, or partially
   applied constants.
 - A **constructed value** is a constructor applied to as many values
   as the arity of the constructor.
 - A **paritally applied constant** is either a primitive (0-arity) or
   a constructor applied to fewer values than the arity of the
   constant.
 - **Redexes** are partial functions from programs to programs.
 - The **evaluation contexts** E describe the occurrences inside
   programs where the reduction may actually occur.
