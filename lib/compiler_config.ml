open! Core
open! Import

let cflags =
  if Compiler_selection.with_frame_pointers
  then ["-fno-omit-frame-pointer"]
  else ["-fomit-frame-pointer"]

(* Summary of [disabled_warnings], as reported by [ocaml -warn-help]
  4 Fragile pattern matching: matching that will remain complete even
    if additional constructors are added to one of the variant types
    matched.
 29 Unescaped end-of-line in a string constant (non-portable code).
 40 Constructor or label name used out of scope.
 41 Ambiguous constructor or label name.
 42 Disambiguated constructor or label name.
 44 Open statement shadows an already defined identifier.
 45 Open statement shadows an already defined label or constructor.
 48 Implicit elimination of optional arguments.
 52 Fragile constant pattern
 56 Unreachable case in a pattern-matching (based on type information)
 58 Missing cmx file
 59 Assignment to non-mutable value
 60 Unused module declaration *)
let disabled_warnings =
  [ 4; 29; 40; 41; 42; 44; 45; 48; 52; 56; 58; 59; 60 ]

let arch_cflags =
  if Compiler_selection.m32
  then ["-m32"]
  else []
