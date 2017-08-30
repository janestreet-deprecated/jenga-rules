open! Core
open! Import

let cflags =
  if Compiler_selection.with_frame_pointers
  then ["-fno-omit-frame-pointer"]
  else ["-fomit-frame-pointer"]

(* The comments are the description the warnings, as reported by [ocamlc -warn-help] *)
let disabled_warnings =
  [ 4 (* Fragile pattern matching: matching that will remain complete even
         if additional constructors are added to one of the variant types
         matched. *)
  ; 29 (* Unescaped end-of-line in a string constant (non-portable code). *)
  ; 40 (* Constructor or label name used out of scope. *)
  ; 41 (* Ambiguous constructor or label name. *)
  ; 42 (* Disambiguated constructor or label name. *)
  ; 44 (* Open statement shadows an already defined identifier. *)
  ; 45 (* Open statement shadows an already defined label or constructor. *)
  ; 48 (* Implicit elimination of optional arguments. *)
  ; 52 (* Fragile constant pattern *)
  ; 56 (* Unreachable case in a pattern-matching (based on type information).
          We don't enable that one because it's too fiddly and it seems to
          catch almost no mistakes. If you write:
          let f = function
          | Ok v -> v
          | Error e -> Nothing.unreachable_code e

          it fires, but if you write:

          let f = function
          | Ok v -> v
          | Error _ -> .

          then the type information is missing. So now, you need to write instead:

          let f = function
          | Ok v -> v
          | Error (_ : Nothing.t) -> .

          So most of the time, it behaves as a style check, and one that our preprocessors
          would trip, if you give them a Nothing.t. *)
  ; 58 (* Missing cmx file *)
  ; 59 (* Assignment to non-mutable value *)
  ; 60 (* Unused module declaration *)
  ]

let arch_cflags =
  if Compiler_selection.m32
  then ["-m32"]
  else []
