
open Jenga_lib.Api

val extract : dir:Path.t -> Rule.t list

(** [boot ~targets ~dir]
    Extract into [dir] a source.list & Makefile for a `bootable' version of [targets]
*)
val boot : targets:Path.t list -> dir:Path.t -> Scheme.t
