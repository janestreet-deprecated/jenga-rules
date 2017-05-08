(** Support for converting documentation files into confluence-style xml, doing global
    checks like absence of dead links across the tree (and then uploading such things
    to confluence though that bit is done outside of jenga). *)

open! Core
open! Import

type t

val create
  :  preview_subdirs_of : Path.t list
  -> upload_files : Path.t list
  -> upload_standard_formats_in : Path.t list
  -> t Dep.t

val rules_for_individual_files : dir : Path.t -> Jbuild_types.Wikipub_conf.t -> Scheme.t

val rules_for_the_root
  :  dir : Path.t
  -> t Dep.t
  -> Rule.t list
