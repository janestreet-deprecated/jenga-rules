open! Import

val suf : string

(** [rule ~dir ~flags ~src:bytecode ~target:javascript_file] construct the rule to
    compile a bytecode program (or bytecode compilation unit) to JavaScript using
    the js_of_ocaml compiler.

    [options] are given to js_of_ocaml compiler (ie: --pretty) *)
val rule
   : build_info:Path.t option
  -> hg_version:Path.t option
  -> dir:Path.t
  -> flags:string list
  -> src:Path.t
  -> target:Path.t
  -> Rule.t
