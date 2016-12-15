(** Interface to the ocamlfind library manager

    This module allows jenga to use pre-installed libraries, by wrapping calls to
    [ocamlfind query] to get the include directories as well as the archives for a given
    findlib library.
*)

open! Core.Std
open! Import
open! Ocaml_types

(** List of packages known by findlib *)
val packages : Path.t Findlib_package_name.Map.t Dep.t

module Query : sig
  type 'a t
  val result : 'a t -> 'a Dep.t
  val rules : _ t -> Rule.t list
  val dummy : 'a -> 'a t

  (** Same as [Dep.both (result t, other)] but faster in case findlib is disabled *)
  val result_and : 'a t -> 'b Dep.t -> ('a * 'b) Dep.t
end

val include_flags
  :  dir:Path.t
  -> string
  -> Lib_dep.t list Dep.t
  -> string list Query.t

val archives
  :  Ocaml_mode.t
  -> dir:Path.t
  -> exe:string
  -> ?predicates:string list
  -> Lib_dep.t list Dep.t
  -> string list Query.t

val archives_full_path
  :  Ocaml_mode.t
  -> dir:Path.t
  -> exe:string
  -> Lib_dep.t list Dep.t
  -> string list Query.t

val javascript_linker_option
  :  Ocaml_mode.t
  -> dir:Path.t
  -> exe:string
  -> Lib_dep.t list Dep.t
  -> string list Query.t

(** The directory where the ocamlfind packages are installed, like .liblinks for us,
    if findlib is enabled. *)
val destdir : Path.t Dep.t option

val global_rules : Rule.t list

val use_findlib : bool
