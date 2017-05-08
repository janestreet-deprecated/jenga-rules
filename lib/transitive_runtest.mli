(** This module implements the alias transitive-runtest, which given the directory of a
    library or executable will (try to) test that library and all its transitive
    dependencies.
    This is different from runtest, because runtest follows the directory structure
    rather than the dependency structure. *)

open! Core
open! Import
open Ocaml_types

val rules : dir:Path.t -> one_step_libdeps:Lib_dep.t list Dep.t -> Rule.t list
