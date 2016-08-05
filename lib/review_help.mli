(** Rules to write the help of an executable to a file, so it can be reviewed. *)

open! Import

val rule : Ocaml_mode.t -> dir:Path.t -> string -> Rule.t

val help_filename : dir:Path.t -> string -> Path.t
