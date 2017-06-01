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

val upload
  :  dir              : Path.t
  (** N.B. Not all [preview] paths are in the subtree of [dir]. *)
  -> preview          : Path.t list Dep.t
  -> registered_files : Path.t list Dep.t
  -> to_wiki_space    : string
  -> Scheme.t
