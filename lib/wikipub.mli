(** Support for converting documentation files into confluence-style xml, doing global
    checks like absence of dead links across the tree (and then uploading such things
    to confluence though that bit is done outside of jenga). *)

open! Core
open! Import

val registered_files
  : dir : Path.t
  -> Jbuild_types.Wikipub_conf.sources
  -> Path.t list Dep.t

val wikipub_sources
  :  dir : Path.t
  -> Jbuild_types.Wikipub_conf.sources
  -> Scheme.t

val preview : dir : Path.t -> preview_root : Path.t -> Scheme.t

val upload
  :  dir              : Path.t
  -> registered_files : Path.t list Dep.t
  -> to_wiki_space    : string
  -> Scheme.t
