open! Import

module Flavor : sig
  type t = [`C | `Cxx]
  val prog : t -> string
end

(** returns (an overapproximation of) the list of header files used when preprocessing
    [source]. This includes transitive inclusions, even across buildable header files.
    The strings produced are (inferred) relative includes, as written in the
    #include directives. They are to be interpreted relative to the include search path
    as specified by [flags].

    It will bring any required header files up to date,
    but will not register action dependency on those files.
    Use [known_deps] to register action dependency.
*)
val deps
  :  dir:Path.t
  -> source:string
  -> flavor:Flavor.t
  -> flags:string list
  -> ([ `Includes of string list ] * [ `Search_path of string list ]) Dep.t

(**
   Establish dependencies on all potential locations for a known set of
   header file names. [includes] should be a list of header file names as they appear
   in the #include <...> directive (normally inferred by [deps]).
*)
val known_deps
  :  dir:Path.t
  -> flags:string list
  -> includes:string list
  -> unit Dep.t
