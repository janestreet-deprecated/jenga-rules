open! Core
open! Import

(** library names must be identifiers starting with a lowercase letter
    and not containing "__" anywhere in their name.
    [of_string] conversion takes care of uncapitalizing the first letter so it can
    be used to convert ocaml unit names to library names
*)
module Libname : sig
  include Identifiable.S
  val of_string_opt : string -> t option
  val suffixed : dir:Path.t -> t -> string -> Path.t
  val to_module : t -> string
  val prefix : t -> string
  val prefix_sep : string
end = struct
  (* double underscore has low probability of colliding with a library name *)
  let prefix_sep = "__"

  module T = struct
    type t = string [@@deriving compare, bin_io, hash]

    let of_string_opt s =
      if String.is_substring s ~substring:prefix_sep
      then None
      else
        (* [Libname.of_string "foo"] and [Libname.of_string "Foo"] refer to the same library,
           represented as the ocaml module [Foo], but written [foo] in .libdeps files *)
        Some (String.uncapitalize s)

    let of_string s =
      match of_string_opt s with
      | Some t -> t
      | None ->
        failwithf "Library names may not contain [%s] as a substring: [%s]"
          prefix_sep s ()

    let to_string = Fn.id
    let module_name = "Jenga_conf.Ocaml_types"
  end
  include T
  include Identifiable.Make(struct include T include Sexpable.Of_stringable(T) end)

  let suffixed = suffixed
  let to_module = String.capitalize
  let prefix t = t ^ prefix_sep
end
module LN = Libname

module Bare_module_name : sig
  include Identifiable.S
  val of_libname : LN.t -> t
  val file_words : Path.t -> t list Dep.t
  val is_lib : t -> libname:LN.t -> bool
  val suffixed : dir:Path.t -> t -> string -> Path.t
  val to_module : t -> string
end = struct
  include String
  let of_libname x = LN.to_string x
  let file_words = file_words
  let is_lib t ~libname = (t = LN.to_string libname)
  let suffixed = suffixed
  let to_module = String.capitalize
end
module BN = Bare_module_name

module Prefixed_module_name : sig
  include Identifiable.S
  val suffixed : dir:Path.t -> t -> string -> Path.t
  val to_module : t -> string
end = struct
  include String
  let suffixed = suffixed
  let to_module = String.capitalize
end
module PN = Prefixed_module_name

module Findlib_package_name : Identifiable.S = String

(** Name of a dependency; either a local library name, a library from the compiler
    distribution or a findlib package name. *)
module Libdep_name : sig
  include Identifiable.S
  val of_findlib_package_name : Findlib_package_name.t -> t
  val of_libname : LN.t -> t
end = struct
  include String
  let of_findlib_package_name = Findlib_package_name.to_string
  let of_libname = LN.to_string
end

module Artifact_name : Identifiable = String

module Lib_in_the_tree = struct
  type t =
    { name                    : LN.t
    ; source_path             : Path.t
    ; supported_in_javascript : bool
    ; public_name             : Findlib_package_name.t sexp_option
    }
  [@@deriving sexp, compare, fields]

  let libdep_name t = Libdep_name.of_libname t.name
  let suffixed t suffix = LN.suffixed ~dir:t.source_path t.name suffix
end

module Findlib_package = struct
  type t =
    { name : Findlib_package_name.t
    }
  [@@deriving sexp, compare]

  let libdep_name t = Libdep_name.of_findlib_package_name t.name
end

module From_compiler_distribution : sig
  type t [@@deriving sexp, compare, enumerate]

  val equal : t -> t -> bool
  val to_string : t -> string
  val libdep_name : t -> Libdep_name.t
  val artifact_dir_relative_to_stdlib_dir : t -> string option
  val search_path_dir : t -> string option
  val transitive_deps : t -> t list
  val supported_in_javascript : t -> bool
  val ocamlfind_package : t -> Findlib_package_name.t
  val cmis__partially_implemented : t -> stdlib_dir:Path.t -> Path.t list
  val lib_of_unit__partially_implemented : unit:string -> t option
  val cma : t -> stdlib_dir:Path.t -> Path.t
  val cmi : t -> stdlib_dir:Path.t -> unit:string -> Path.t

  val stdlib : t
  val ocamlcommon : t

end = struct
  type t =
    | Bigarray
    | Dynlink
    | Graphics
    | Nums
    | Ocamlcommon
    | Ocamlopttoplevel
    | Ocamltoplevel
    | Ocamlbytecomp
    | Ocamloptcomp
    | Stdlib
    | Str
    | Threads
    | Unix
  [@@deriving sexp, compare, enumerate, variants]

  let to_string = function
    | Bigarray         -> "bigarray"
    | Dynlink          -> "dynlink"
    | Graphics         -> "graphics"
    | Nums             -> "nums"
    | Ocamlcommon      -> "ocamlcommon"
    | Ocamlopttoplevel -> "ocamlopttoplevel"
    | Ocamltoplevel    -> "ocamltoplevel"
    | Ocamlbytecomp    -> "ocamlbytecomp"
    | Ocamloptcomp     -> "ocamloptcomp"
    | Stdlib           -> "stdlib"
    | Str              -> "str"
    | Threads          -> "threads"
    | Unix             -> "unix"

  let equal = [%compare.equal: t]

  let lib_of_unit__partially_implemented =
    let table = String.Table.create () in
    let list =
      [ Bigarray         , ["bigarray"]
      ; Dynlink          , ["dynlink"]
      ; Graphics         , ["graphics";"graphicsX11"]
      ; Nums             , ["int_misc";"nat";"big_int";"arith_flags";"ratio";"num";"arith_status"]
      ; Ocamlcommon      , []
      ; Ocamlopttoplevel , []
      ; Ocamltoplevel    , []
      ; Ocamlbytecomp    , []
      ; Ocamloptcomp     , []
      ; Str              , ["str"]
      ; Stdlib           , []
      ; Threads          , ["thread";"mutex";"condition";"event";"threadUnix"]
      ; Unix             , ["unix";"unixLabels"]
      ]
    in
    List.iter list ~f:(fun (lib, units) ->
      List.iter units ~f:(fun unit ->
        String.Table.add_exn table ~key:unit ~data:lib
      )
    );
    fun ~unit -> String.Table.find table unit

  let libdep_name t = Libdep_name.of_string (to_string t)

  let artifact_dir_relative_to_stdlib_dir = function
    | Bigarray | Dynlink | Graphics | Nums | Str | Stdlib | Unix -> None
    | Threads -> Some "threads"
    | Ocamlcommon | Ocamlopttoplevel | Ocamltoplevel | Ocamlbytecomp | Ocamloptcomp ->
      Some "compiler-libs"

  let search_path_dir t =
    match artifact_dir_relative_to_stdlib_dir t with
    | None -> None
    | Some s -> Some ("+" ^ s)

  let transitive_deps = function
    | Bigarray | Dynlink | Graphics | Nums | Str | Stdlib | Unix
    | Ocamlcommon | Ocamlbytecomp | Ocamloptcomp -> []
    | Threads -> [ Unix ]
    | Ocamltoplevel -> [ Ocamlcommon ; Ocamlbytecomp ]
    | Ocamlopttoplevel -> [ Ocamlcommon ; Ocamloptcomp ; Dynlink ]

  let supported_in_javascript = function
    | Ocamlcommon | Ocamlbytecomp | Ocamltoplevel
    | Bigarray | Nums | Stdlib
      -> true
    | Ocamloptcomp | Ocamlopttoplevel
    | Dynlink | Graphics | Str | Threads | Unix
      -> false
  ;;

  let ocamlfind_package t =
    let s =
      match t with
      | Bigarray         -> "bigarray"
      | Dynlink          -> "dynlink"
      | Graphics         -> "graphics"
      | Nums             -> "num"
      | Ocamlcommon      -> "compiler-libs.common"
      | Ocamltoplevel    -> "compiler-libs.toplevel"
      | Ocamlopttoplevel -> "compiler-libs.opttoplevel"
      | Ocamlbytecomp    -> "compiler-libs.bytecomp"
      | Ocamloptcomp     -> "compiler-libs.optcomp"
      | Str              -> "str"
      | Stdlib           -> "stdlib"
      | Threads          -> "threads"
      | Unix             -> "unix"
    in
    Findlib_package_name.of_string s
  ;;

  let cmis__partially_implemented t ~stdlib_dir =
    let dir_relative_to_stdlib_dir =
      match artifact_dir_relative_to_stdlib_dir t with
      | None -> ""
      | Some p -> p ^ "/"
    in
    match t with
    | Str -> [Path.relative ~dir:stdlib_dir (dir_relative_to_stdlib_dir ^ "str.cmi")]
    | _ -> []

  let artifact_path t ~stdlib_dir name =
    let dir_relative_to_stdlib_dir =
      match artifact_dir_relative_to_stdlib_dir t with
      | None -> ""
      | Some p -> p ^ "/"
    in
    Path.relative ~dir:stdlib_dir (dir_relative_to_stdlib_dir ^ name)

  let cma t ~stdlib_dir = artifact_path t ~stdlib_dir (to_string t ^ ".cma")

  let cmi t ~stdlib_dir ~unit = artifact_path t ~stdlib_dir (unit ^ ".cmi")
end

module Lib_dep : sig
  type t =
    | From_compiler_distribution of From_compiler_distribution.t
    | In_the_tree                of Lib_in_the_tree.t
    | Findlib_package            of Findlib_package.t
  [@@deriving sexp_of]

  include Comparable.S with type t := t

  val to_string : t -> string

  val of_lib_in_the_tree : Lib_in_the_tree.t -> t
  val to_lib_in_the_tree : t -> Lib_in_the_tree.t option

  val remove_dups_preserve_order : t list -> t list
  val remove_dups_and_sort : t list -> t list
end = struct
  module T = struct
    type t =
      | From_compiler_distribution of From_compiler_distribution.t
      | In_the_tree                of Lib_in_the_tree.t
      | Findlib_package            of Findlib_package.t
    [@@deriving sexp, compare]
  end
  include T
  include Comparable.Make(T)

  let of_lib_in_the_tree lib = In_the_tree lib
  let to_lib_in_the_tree = function
    | In_the_tree lib -> Some lib
    | From_compiler_distribution _
    | Findlib_package _ -> None

  let libdep_name = function
    | From_compiler_distribution x -> From_compiler_distribution.libdep_name x
    | In_the_tree                x -> Lib_in_the_tree.libdep_name            x
    | Findlib_package            x -> Findlib_package.libdep_name            x

  let to_string t = Libdep_name.to_string (libdep_name t)

  let remove_dups_preserve_order xs =
    let set = Libdep_name.Hash_set.create () in
    let rec loop acc = function
      | [] -> List.rev acc
      | x::xs ->
        let s = libdep_name x in
        if Hash_set.mem set s
        then loop acc xs
        else (Hash_set.add set s; loop (x::acc) xs)
    in
    loop [] xs

  let remove_dups_and_sort xs = Set.of_list xs |> Set.to_list
end
