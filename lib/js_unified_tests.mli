(** Rules to run unified tests (the test framework of mercurial).
    We also provide rules to create a script run-unified-tests that people can run by
    hand, for convenience. *)

open! Import

type t =
  { target : string
  ; deps : unit Dep.t list
  ; setup_script : string option
  ; uses_catalog : Jbuild_types.Uses_catalog.t
  }

val rules : dir:Path.t -> t -> Rule.t list
