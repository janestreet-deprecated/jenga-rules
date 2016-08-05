(** Stale artifact deletion is the deletion of files that have been (presumably) produced
    by the build system previously but can't be built anymore.

    The point of this is to make incremental builds more reliable. If you create a
    dependency on say, a.orig (which can be created by hg), and a.orig exists in your
    working copy, a build system without stale artifact deletion would be happy to
    consider this file as a source file and succeed.
    But we would like that if the build succeeds (resp fails) in a repository, if you
    clone that repository, go at the same revision, and copy whatever changes were not
    committed yet, the build (which is from scratch) should succeed (resp fail) as
    well. But your new clone won't have a.orig since it's ignored so the build will
    rightly fail.
    A build system with stale artifact deletion will blow away a.orig when it sees a
    dependency on it, and will thus fail the same as the build from scratch.
*)

open Import

val delete_eagerly : (non_target:Path.t -> bool) Dep.t
val delete_if_depended_upon : (non_target:Path.t -> bool) Dep.t
