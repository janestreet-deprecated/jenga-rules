(** This module:
    - prevents tests from accessing the production catalog (for reliability and because
    the version in the tree might not talk the same version as production)
    - allows to wrap a command with a catalog service just for that command, for cases
    that do need catalog *)

open! Import

val deps : Jbuild_types.Uses_catalog.t -> unit Dep.t list

val invalid_environment : (string * string option) list

val wrap
  : Jbuild_types.Uses_catalog.t
  -> Action.process
  -> can_assume_env_is_setup:bool
  -> Action.process
