(** Support for converting documentation files into confluence-style xml, doing global
    checks like absence of dead links across the tree (and then uploading such things
    to confluence though that bit is done outside of jenga). *)

open! Core.Std
open! Import

val rules_for_individual_files
  : dir:Path.t -> [ `Standard_formats | `Files of string list ] -> Scheme.t

val interpret_files_as_paths
  : dir:Path.t -> [ `Standard_formats | `Files of string list ] -> Path.t list Dep.t

val rules_for_the_root : dir:Path.t -> all_input_files:Path.t list Dep.t -> Rule.t list
