(** This module decides which ocaml compiler to run, and defines various options that
    depend on the compiler version. *)

val major_version : string
val compiler_bin_dir : string
val compiler_stdlib_dir : string
val compiler_dir : string

(** This is written into .ocaml-major-version (which is commited) at the root of
    the tree.
    As this gets bumped when we switch to a new version of the compiler we can
    use it to make a "best guess" regarding which version of merlin to use
    when the .omake-ocaml-bin file is not present (i.e. when the tree hasn't
    been compiled yet). *)
val default_version : string

val odoc_minor_version : string

val m32 : bool
val flambda : bool
val with_frame_pointers : bool
val spacetime : bool
