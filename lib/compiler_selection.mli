(** This module decides which ocaml compiler to run, and defines various options that
    depend on the compiler version. *)

val major_version : string
val compiler_bin_dir : string
val compiler_stdlib_dir : string
val compiler_dir : string

val odoc_minor_version : string

val m32 : bool
val flambda : bool
val with_frame_pointers : bool
