
open Jenga_lib.Api

(* [write ~dir] constructs rules to write `build-info' files into [dir], and provides
   alias .info to force their build. The files are not .DEFAULT targets.

   Currently there is just one `build-info' file: buildable_targets.list, which is the
   output of [Dep.buildable_targets]. It may be useful for debugging. *)

val write : dir:Path.t -> Rule.t list
