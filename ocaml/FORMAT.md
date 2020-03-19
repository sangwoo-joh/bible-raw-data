# OCaml Format (Not ocamlformat)
 Pretty-printing engine.

## Principle
 - **Boxes**: a box is a logical pretty-printing unit, which defines a
   behaviour of the pretty-printing engine to display the material
   inside the box.
 - **Break hints**: a break hint s a directive to the pretty-printing
   engine that proposes to break the line here, if it's necessary to
   properly print the rest of the material. Otherwise never break.
 - **Indentation rules**: when a line break occurs, the
   pretty-printing engine fixes the indentation (or amount of leading
   spaces) of the new line using indentation rules:
   - A box can state the extra indentation of every new line opened in
     its scope. This extra indentation is **box breaking
     indentation**.
   - A break hint can also set the additional indentation of the new
     line it may fire. **Hint breaking indentation**.
   - If break hint `bh` fires a new line within box `b`, then the
     indentation of the new line is simply the sum of: the current
     indention of the box `b` + the additional box reaking
     indentation, as defined by box `b` + the additional hint breaking
     indentation, as defined by break hint `bh`.


## Boxes
 With an example `"--b--b--"` (`b` stands for the value of the break)

### Horizontal Box
 - `h` box
 - `open_hbox`
 - Example output

```
--b--b--
```

### Vertical Box
 - `v` box
 - `open_vbox`
 - Example output

```
--b
--b
--
```

### Vertical/Horizontal Box
 - `hv` box
 - `open_hvbox`
 - If there is enough room to print the box on the line

```
--b--b--
```

 - If it cannot fit on the line

```
--b
--b
--
```

### Vertical or Horizontal Box
 - `hov` box
 - `open_box` or `open_hovbox`
 - If there is enough room

```
--b--b--
```

 - If there is enough room

```
--b--b
--
```


## Practice
 Pretty-printing annotations are introduced by the `@` symbol,
 directly into the string format.

 + `@[` opens a box (`open_box 0`). e.g., `@[<hov n>]` is equivalent
   to `open_hovbox n`.
 + `@]` closes a box (`close_box ()`)
 + `@ ` puts a breakable space (`print_space ()`)
 + `@,` puts a "cut" break hint (`print_cut ()`)
 + `@;` puts a "full" break hint
 + `@;<n m>` puts a "full" break hint with `nspaces` and `offset`
   (`print_break n m`)
 + `@.` ends the pretty-printing, closing all the boxes still
   opened. (`print_newline ()`)
 + `@<n>` prints the following item as if it were of length
   `n`. e.g. `Format.printf "@<0>%s" arg` prints `arg` as a zero
   length string.
 + `@{` opens a semantic tag.
 + `@}` closes the most recently opened semantic tag.
 + `@?` flushes the pretty-printer as with `print_flush ()`
 + `@\n` forces a newline, as with `force_newline ()`


 For instance,

``` ocaml
Format.printf "@[<1>%s@ =@ %d@ %s@]@." "Prix TTC" 100 "Euros";;
Prix TTC = 100 Euors
```


 Or, more concretely,

``` ocaml
open_box 0;
print_string "x =";
print_space ();
print_int 1;
close_box ();
print_newline()
```

 is equal to

``` ocaml
Format.printf "@[%s@ %i@]@." "x =" 1
```
