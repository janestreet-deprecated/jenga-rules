(** [Named_artifact] provides a way to reference artifacts in jenga rules without having
    to hardcode their exact locations. These named artifacts will be looked up
    appropriately (in the tree, or for the public release, possibly in the PATH or in
    findlib). *)
open! Core
open Import
open Ocaml_types

module Store : sig
  type t
  val create
    :  artifacts:(Artifact_name.t * Path.t) list
    -> findlib_packages:Path.t Findlib_package_name.Map.t
    -> t
end

type t

val name : t -> Artifact_name.t

(** {1 Defining named artifact} *)

(** In the three following functions, the string argument matches the first argument of
    the [(provides ...)] stanza in the jbuild. *)

(** A named artifact that is looked up in the PATH if not found in the tree *)
val binary : string -> t

(** A named artifact that is looked up in the given findlib package if not found in the
    tree. Syntax is: ["<findlib_package>:<filename>"]. *)
val in_findlib : string -> t

(** An artifact that is not released on github. *)
val jane_street_only : string -> t

(** {1 Resolving named artifact} *)

val path : Store.t -> t -> Path.t Dep.t

val find : Store.t -> Artifact_name.t -> t option
