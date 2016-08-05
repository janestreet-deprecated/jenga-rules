(** The implementation of this module is overridden in the public release

    It contains various configuration elements that are different in the opam world
*)

open Import

val putenv : (string * string option) list

val findlib_conf_default : string option

(** Directory containing the scripts required by the rules *)
val script_dir : Path.t

val command_lookup_path : [ `Replace | `Extend ]

val git_prog : string
val hg_prog : string

val extra_jane_kernel_ppx : string list
