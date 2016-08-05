(** The part of compiler_selection that's common for the internal build and the public
    release. *)

val disabled_warnings : int list

(** Options to [CC] and [CXX] compilers. *)
val cflags : string list

(** Same as [cflags] but for architecture-specific options that are unconditionally
    needed by [CC] and [CXX] compilers.
    These options should never need to be overridden by user-defined configuration.
    Currently used to select between 32-bit and 64-bit targets. *)
val arch_cflags : string list
