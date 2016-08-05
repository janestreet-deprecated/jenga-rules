
(* This file is shared between jenga/root.ml and public-release/bin. Jenga passes the
   metadata by writing a .sexp file. *)

open Core.Std

let opam_of_ocamlfind = function
  | "threads" -> "base-threads"
  | s ->
    match Option.value_map (String.lsplit2 s ~on:'.') ~f:fst ~default:s with
    | "bigarray"
    | "compiler-libs"
    | "num"
    | "unix"
    | "dynlink"
    | "str"
    | "graphics" -> "ocaml" (* "ocaml" is considered preinstalled *)
    | s -> s

module Library = struct
  module Kind = struct
    type t =
      | Normal
      | Ppx_rewriter         (** Need to build a standalone ppx executable *)
      | Ppx_type_conv_plugin (** Need to build a ppx_deriving plugin       *)
    [@@deriving bin_io, sexp]
  end

  module Oasis_kind = struct
    type t = Library | Object
    [@@deriving bin_io, sexp]
  end

  type t =
    { internal_name : string
    ; install_as    : string option
    ; kind          : Kind.t        [@default Normal]
    ; oasis_kind    : Oasis_kind.t
    (** Sub-directory inside the public repository *)
    ; c_files       : string list   [@default []]
    ; wrapped       : bool          [@default true]
    ; short_desc    : string option [@default None]
    ; modules       : string list   [@default []]
    ; path          : string
    }
  [@@deriving bin_io, sexp]
end

module Executable = struct
  module Mode = struct
    type t = Best | Both | Bytecode | Native
    [@@deriving bin_io, sexp]
  end

  type t =
    { main_module       : string
    (** name under which the executable is installed *)
    ; install_as        : string option
    ; path              : string
    ; mode              : Mode.t
    }
  [@@deriving bin_io, sexp]
end

(** Common build info for libraries and executables *)
module Buildable = struct
  module Dependency = struct
    module Context = struct
      type t = Build | Runtime | Both [@@deriving bin_io, sexp]
    end

    module Global = struct
      type t =
        { opam_package      : string
        ; ocamlfind_package : string
        }
      [@@deriving bin_io, sexp]

      let of_ocamlfind_package s =
        { ocamlfind_package = s
        ; opam_package      = opam_of_ocamlfind s
        }

      let t_of_sexp sexp =
        match sexp with
        | Sexp.Atom s -> of_ocamlfind_package s
        | Sexp.List _ -> t_of_sexp sexp

      let sexp_of_t t =
        if t.opam_package = opam_of_ocamlfind t.ocamlfind_package then
          Sexp.Atom t.ocamlfind_package
        else
          sexp_of_t t
    end

    module Local = struct
      type t =
        { internal_name     : string
        ; ocamlfind_package : string option
        }
      [@@deriving bin_io, sexp]
    end

    module Kind = struct
      type t =
        | Local  of Local.t
        | Global of Global.t
      [@@deriving bin_io, sexp]
    end

    type t =
      { kind     : Kind.t
      ; context  : Context.t [@default Both]
      (** Library is developped at janestreet *)
      ; internal : bool [@default false]
      ; lib_kind : Library.Kind.t
      (** Name of a wrapper module. This is for external libraries that are wrapped when
          imported in our tree. We need to generate a wrapper at use sites. *)
      ; wrapper  : (string * string list) option [@default None]
      }
    [@@deriving bin_io, sexp]
  end

  type t =
    { (** Sub-directory inside the public repository *)
      path                  : string
    (** Public names of dependencies *)
    ; dependencies          : Dependency.t list        [@default []]
    ; js_ppxs               : Dependency.Global.t list [@default []]
    ; preprocessor_deps     : string list              [@default []]
    }
  [@@deriving bin_io, sexp]
end

module Package = struct
  module Dependency = struct
    type t =
      { opam_package : string
      ; internal     : bool [@default false]
      ; context      : Buildable.Dependency.Context.t [@default Both]
      }
    [@@deriving bin_io, sexp]
  end

  module Install_item = struct
    (** This mimics the API of [Oasis2opam_install] *)

    module Section = struct
      type t =
        | Lib
        | Libexec
        | Bin
        | Sbin
        | Toplevel
        | Share
        | Share_root
        | Etc
        | Doc
        | Stublibs
        | Man
      [@@deriving sexp, bin_io, compare]
    end

    module Oasis_lib = struct
      type t =
        { tag : string }
      [@@deriving bin_io, sexp]
    end

    module Oasis_exe = struct
      type t =
        { tag     : string
        ; dst     : string sexp_option
        ; section : Section.t [@default Bin]
        }
      [@@deriving bin_io, sexp]
    end

    module File = struct
      type t =
        { src     : string
        ; dst     : string sexp_option
        ; section : Section.t
        }
      [@@deriving bin_io, sexp]
    end

    module Tree = struct
      type t =
        { src     : string
        ; dst     : string sexp_option
        ; section : Section.t
        }
      [@@deriving bin_io, sexp]
    end

    type t =
      | Oasis_lib of Oasis_lib.t
      | Oasis_obj of Oasis_lib.t
      | Oasis_exe of Oasis_exe.t
      | File      of File.t
      | Tree      of Tree.t
    [@@deriving bin_io, sexp]
  end

  (** All filenames are relative to the repository root *)
  type t =
    { name                : string
    ; synopsis            : string
    ; long_description    : string list
    ; copyright_start     : int
    ; libraries           : Library.t list
    ; executables         : Executable.t list
    (** Buildables by path *)
    ; buildables          : Buildable.t list
    ; file_list_filename  : string
    ; dir_mapping         : (string * string) list
    ; dependencies        : Dependency.t list
    ; install_extra       : Install_item.t list
    (** Public version, of the form NNN.NN.NN. It is [None] for dev branches. *)
    ; stable_version      : string option
    }
  [@@deriving bin_io, sexp]
end
