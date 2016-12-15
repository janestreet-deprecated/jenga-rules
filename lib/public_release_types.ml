(* This file is shared between jenga/root.ml and public-release/bin. Jenga passes the
   metadata by writing a .sexp file. *)

open Core.Std
open! String.Replace_polymorphic_compare

let opam_of_ocamlfind s =
  match Option.value_map (String.lsplit2 s ~on:'.') ~f:fst ~default:s with
  | "threads" -> "base-threads"
  | "findlib" -> "ocamlfind"
  | "bigarray"
  | "compiler-libs"
  | "num"
  | "unix"
  | "dynlink"
  | "str"
  | "graphics" -> "ocaml" (* "ocaml" is considered preinstalled *)
  | s -> s

module Package_dep = struct
 type t =
   | Internal of string
   | External of string
 [@@deriving compare, sexp]
end

module Lib_info = struct
  module Wrapper = struct
    type t =
      { name    : string
      ; modules : string list
      }
    [@@deriving compare, sexp]
  end

  type t =
    { internal     : bool
    ; opam_package : string
    ; public_name  : string sexp_option
    ; wrapper      : Wrapper.t option
    }
  [@@deriving compare, sexp]
end

module Package = struct
  module Hooks = struct
    type 'a t =
      { (** Executed just before creating the tarball *)
        pre_dist : 'a sexp_option
      ; (** Executed just before creating the opam file *)
        pre_opam : 'a sexp_option
      }
    [@@deriving compare, sexp]

    let none =
      { pre_dist = None
      ; pre_opam = None
      }
  end

  (** All filenames are relative to the repository root *)
  type t =
    { name                  : string
    ; synopsis              : string
    ; long_description      : string list
    ; copyright_start       : int
    ; file_list_filename    : string
    ; dir_mapping           : (string * string) list
    ; package_deps_filename : string
    (** Public version, of the form NNN.NN.NN. It is [None] for dev branches. *)
    ; stable_version        : string option
    ; hooks                 : string Hooks.t [@default Hooks.none]
    }
  [@@deriving compare, sexp]
end
