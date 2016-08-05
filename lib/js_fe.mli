(** Interaction of the build system with [fe]. *)

open! Import

module Make(Hg : sig
    val all_the_repos : Path.t list Dep.t
    val manifest_dirs : repo:Path.t -> Path.t list Dep.t
  end) : sig

  module Projection : sig
    type t
    val create : repo:Path.t -> name:string -> t
  end

  val dot_fe_sexp_basename : string
  val setup_projections_targets : Scheme.t

  val rule_for_projection_files : dir:Path.t -> Projection.t -> target:Path.t -> Rule.t

  module Projections_check : sig

    val rule_for_testing
       : target:Path.t
      -> exe:Path.t
      -> allowed_projections:string list
      -> Rule.t
     val error_msg_dep
       : dir:Path.t
      -> exe:Path.t
      -> allowed_projections:string list
      -> string option Dep.t

     val libs_in_projections_are_self_contained
       : projections:string list
      -> libs_by_dir:(Path.t -> Path.t list) Dep.t
      -> string option Dep.t
   end

end
