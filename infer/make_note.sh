#!/bin/bash

rm *.cm*
ocamlfind ocamlc -bin-annot -package core -package zarith -linkpkg -thread base_note.ml
ocamlfind ocamlc -bin-annot -package core -package zarith -linkpkg -thread istd_note.ml
ocamlfind ocamlc -bin-annot -package core -package zarith -linkpkg -thread IR_note.ml
ocamlfind ocamlc -bin-annot -package core -package zarith -linkpkg -thread absint_note.ml

rm a.out *.cmo
