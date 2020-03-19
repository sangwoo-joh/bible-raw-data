# Documentation

 + It extracts specially formatted comments from source code and
   renders them as HTML, making it easy for programmers to read
   documentation.

## How to document

``` ocaml
(** [sum lst] is the sum of the elements of [lst]. *)
let rec sum lst = ...
```

 + The double asterisk is what causes the comment to be recognized as
   an OCamldoc comment that should be extracted.
 + The square brackets around parts of the comment mean that those
   parts should be rendered in HTML as `typewriter font` rather than
   the regular font.


### Documentation tags

 + `@author`
 + `@deprecated`
 + `@param`
 + `@return`
