(** Compilation of the ocamldoc-style comments into actual documentation (using odoc, not
    ocamldoc). *)

open! Core.Std
open Import
open Ocaml_types

val odoc_path : string

val alias : dir:Path.t -> Alias.t

val odoc_output_dir : Path.t
val html_output_dir : Path.t

val generate_http_server :
  src_dir:Path.t -> odoc_html_dir:Path.t -> LN.t -> Path.t * Rule.t

val setup
  : dir:Path.t
  -> lib_in_the_tree:Lib_in_the_tree.t
  -> lib_deps:Lib_dep.t list Dep.t
  -> [< `Compile | `Link ] -> Rule.t list Dep.t
