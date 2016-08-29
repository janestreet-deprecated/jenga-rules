open Core.Std
open Import
open Jbuild_types
open Ocaml_types

(* THE control for whether wrapping is used in bin-dirs *)

let wrapped_bindirs = false

module List = struct
  include List
  let concat_cartesian_product l1 l2 =
    List.map (List.cartesian_product l1 l2) ~f:(fun (x, y) -> x ^ y)
end

let dummy_position path =
  { Lexing.pos_fname = Path.to_string path; pos_cnum = 0; pos_bol = 0; pos_lnum = 1 }
;;
let failposf : pos:Lexing.position -> ('a, unit, string, unit -> 'b) format4 -> 'a =
  fun ~pos fmt ->
    let {Lexing.pos_fname; pos_lnum; pos_cnum; pos_bol} = pos in
    let col = pos_cnum - pos_bol in
    ksprintf (fun s ->
      let fname =
        Path.to_absolute_string (Path.relative_or_absolute ~dir:Path.the_root pos_fname)
      in
      printf "File %S, line %d, characters %d-%d:"
        fname pos_lnum col col;
      printf "Error: %s" s;
      failwithf "%s: %s" pos_fname s ();
    ) fmt

let failheref : Lexing.position -> ('a, unit, string, unit -> 'b) format4 -> 'a =
  fun here fmt ->
    let pos = {here with pos_fname = "jenga/root.ml"} in
    failposf ~pos fmt

let path_remove_dups_and_sort xs =
  Path.Set.to_list (Path.Set.of_list xs)

let simple_rule ~targets ~deps ~action =
  Rule.create ~targets (
    Dep.all_unit deps *>>| fun () -> action
  )

let relative_rule ~dir ~targets ~deps ~non_relative_deps monadic_action =
  let targets = List.map targets ~f:(fun name -> Path.relative ~dir name) in
  let deps =
    non_relative_deps @
      List.map deps ~f:(fun name -> Dep.path (Path.relative ~dir name))
  in
  Rule.create ~targets
    (Dep.both (Dep.all_unit deps) monadic_action
     *>>| fun ((), action) -> action)

let alias_dot_filename_hack ~dir dot_name =
  (* Sadly names given on the command line which begin with a dot (i.e. ".merlin") are
     currently always interpreted as a reference to an alias. Workaround this problem for
     specific instances by creating an alias to the dot-filename, named the same as the
     filename (minus the dot). *)
  let name = String.chop_prefix_exn dot_name ~prefix:"." in
  Rule.alias (Alias.create ~dir name) [Dep.path (relative ~dir dot_name)]

module Glob = struct
  include Glob
  let of_string ~dir s =
    let path_ish =
      (* This is not actually a path, but a relative glob-string.  We use use [relative]
         to prepend [dir] onto any leading relative-path in the string [s], before
         splitting into the dirname/basename components. *)
      Path.relative ~dir s
    in
    Glob.create ~dir:(dirname path_ish) (basename path_ish)
end

let uncomment_line s =
  match String.lsplit2 s ~on:'#' with None -> s | Some (s,_comment) -> s

let uncomment s =
  String.concat ~sep:"\n" (List.map ~f:uncomment_line (String.split ~on:'\n' s))

let file_words_allow_commments path =
  Dep.contents path *>>| uncomment *>>| words_of_string

module Alias1 = struct
  include Alias
  let of_string ~dir s =
    let path_ish =
      (* This is not actually a path, but a relative alias-string.  We use use [relative]
         to prepend [dir] onto any leading relative-path in the string [s], before
         splitting into the dirname/basename components. *)
      Path.relative ~dir s
    in
    Alias.create ~dir:(dirname path_ish) (basename path_ish)
end

module Alias = struct
  include Alias1
  let c ~dir = Alias.create ~dir "c"
  let default ~dir = Alias.create ~dir "DEFAULT"
  let runtest ~dir = Alias.create ~dir "runtest"
  let qtest ~dir = Alias.create ~dir "qtest"
  let pp ~dir = Alias.create ~dir "pp"
  let libdeps ~dir = Alias.create ~dir "libdeps"
  let merlin ~dir = Alias.create ~dir "merlin"
  let unused_libs ~dir = Alias.create ~dir "unused-libs"
  let utop ~dir = Alias.create ~dir "utop"
  (* aliases not intended to be recursive.. *)
  let lib_artifacts ~dir = Alias.create ~dir "lib_artifacts"
  (* These aliases are used to group and share the dependencies on *.cmi or *.cmx for a
     given library, because we need a lot of these dependencies (one per ml/mli), and they
     are big (number of modules in all the libraries the source file depends on). *)
  let submodule_cmis ~dir = Alias.create ~dir "submodule_cmis"
  let submodule_cmxs ~dir = Alias.create ~dir "submodule_cmxs"
  let api ~dir = Alias.create ~dir "api"
  let save_benchmarks ~dir = Alias.create ~dir "save_benchmarks"
end

let alias_dot_directory_hack ~dir dot_name =
  let name = String.chop_prefix_exn dot_name ~prefix:"." in
  Rule.alias (Alias.create ~dir name) [Dep.alias (Alias.default ~dir:(relative ~dir dot_name))]

(** Returns a list of options you need to give to ocaml compiler
    so it passes [args] to the c compiler at link-time. *)
let link_time_args_for_c_compiler = function
  | [] -> []
  | _ :: _ as args ->
    (* note that [-cclib], unlike [-ccopt], expects a whitespace-separated list of
       arguments, not a shell command string, so we can't use [concat_quoted]. *)
    assert (not (List.exists args ~f:(String.exists ~f:(Char.is_whitespace))));
    [ "-cclib"; String.concat ~sep:" " args ]

let use_compiler_flavor flavor =
  (* note that [-cc] and [-ccopt] expect shell syntax, which is why we do quoting here *)
  "-cc" :: quote (C.Flavor.prog flavor)
  :: ccopts Compiler_config.arch_cflags

(*----------------------------------------------------------------------
 getenv...
----------------------------------------------------------------------*)


let build_profile =
  match
    Var.peek
      (Var.register_enumeration
         "BUILD_PROFILE"
         ~choices:(String.Map.of_alist_exn
                     [ "fast-build", `Fast_build
                     ; "fast-exe", `Fast_exe
                     ; "default", `Default ])
         ~default:"default"
         ~fallback:(fun _ -> None))
  with
  | Ok a -> a
  | Error (`Bad s) -> failwithf "invalid BUILD_PROFILE %s" s ()

let version_util_support =
  Var.register_bool "VERSION_UTIL_SUPPORT"
    ~default:(match build_profile with
              | `Fast_build -> false
              | `Fast_exe | `Default -> true)

let stable_build_info =
  Var.peek_register_bool "STABLE_BUILD_INFO"
    ~default:(match build_profile with
              | `Fast_build -> true
              | `Fast_exe | `Default -> false)

let link_executables =
  Var.peek_register_bool "LINK_EXECUTABLES" ~default:true

let x_library_inlining =
  Var.peek_register_bool "X_LIBRARY_INLINING"
    ~default:(match build_profile with
              | `Fast_build | `Default -> false
              | `Fast_exe -> true)

let dynlinkable_code =
  Var.peek_register_bool "DYNLINKABLE_CODE"
    ~default:(match build_profile with
             | `Fast_build | `Default -> true
             | `Fast_exe -> false)

let bin_annot =
  Var.peek_register_bool "BIN_ANNOT" ~default:true

let transitive_runners =
  Var.peek_register_bool "TRANSITIVE_RUNNERS" ~default:false

let use_new_sqml =
  Var.peek_register_bool "USE_NEW_SQML" ~default:false

let build_info_app_fields =
  Option.map (Var.peek (Var.register "BUILD_INFO_APP_FIELDS")) ~f:(fun s ->
    Sexp.of_string_conv_exn s [%of_sexp: Sexp.t String.Map.t])

let drop_test =
  Var.peek_register_bool "DROP_TEST" ~default:false

let drop_bench =
  Var.peek_register_bool "DROP_BENCH" ~default:false

let inline_test_color =
  Var.peek_register_bool "INLINE_TEST_COLOR" ~default:true

let inline_test_in_place =
  Var.peek_register_bool "INLINE_TEST_IN_PLACE" ~default:false

let allow_hardware_specialization =
  (* Whether or not the build should make use of hardware features that are not available
     everywhere. This should be unset when sharing build artifacts across machines (at
     least if you don't want to recompile them). *)
  Var.peek_register_bool "ALLOW_HARDWARE_SPECIALIZATION" ~default:true

let o3 = Var.peek_register_bool "WITH_O3" ~default:true

let for_javascript_development =
  (* Select the right options / compilation mode to provide
     a faster development loop and
     a better debugging experience. *)
  Var.peek_register_bool "FOR_JS_DEVEL" ~default:false

let unbox_closures = Var.peek_register_bool "WITH_UNBOX_CLOSURES" ~default:true

let alias_for_inline_runners ~skip_from_default ~dir =
  if transitive_runners then
    Some (Alias.lib_artifacts ~dir)
  else if skip_from_default then
    None
  else
    Some (Alias.default ~dir)

let cmi_maybe_cmx =
  if x_library_inlining
  then [".cmi"; ".cmx"]
  else [".cmi"]

module Top = struct

  let ocamlcflags =
    Var.peek (Var.register_args "OCAMLCFLAGS" ~default:["-g"])

  let ocamloptflags =
    let default =
      List.concat
        [ [ "-g" ]
        ; (if dynlinkable_code then [] else ["-nodynlink"])
        ; match Compiler_selection.flambda, o3 with
        | false, _ -> [ "-inline"; "20" ]
        | true, false -> [ "-Oclassic" ]  (* Prefer fast compilation time. *)
        | true, true -> "-O3" :: (if unbox_closures then ["-unbox-closures"] else [])
        ]
    in
    let flags = Var.peek (Var.register_args "OCAMLOPTFLAGS" ~default) in
    if dynlinkable_code && List.mem flags "-nodynlink"
    then failwith "OCAMLOPTFLAGS shouldn't contain -nodynlink when DYNLINKABLE_CODE is set to true";
    flags

  let bin_annot_flag =
    if bin_annot
    then ["-bin-annot"]
    else []

  let default_common_flags ~disabled_warnings =
    let ocamlwarnings =
      "@a" ^ String.concat (List.map disabled_warnings ~f:(fun n -> "-" ^ Int.to_string n))
    in
    [
      "-w"; ocamlwarnings;
      "-strict-sequence";
      "-short-paths"
    ]

  let default_merlinflags = default_common_flags

  let default_ocamlflags ~disabled_warnings =
    List.concat [
      ["-I"; "+camlp4"];
      default_common_flags ~disabled_warnings;
      ["-strict-formats"];
      bin_annot_flag;
      ["-thread"]
    ]
end

let common_cflags = [
  "-pipe";
  "-g";
  "-fPIC";
  "-DPIC";
  "-Wall";
  "-pedantic";
  "-Wextra";
  "-Wunused";
  "-Werror";
  "-Wno-long-long";
  "-DCAML_NAME_SPACE";
  "-O2";
]

let default_cflags = common_cflags @ Compiler_config.cflags
let default_cxxflags = default_cflags

let ocaml_bin   = Compiler_selection.compiler_bin_dir
let ocaml_where = Compiler_selection.compiler_stdlib_dir
let ocaml_where_path = Path.absolute ocaml_where

let ocamldep_path   = ocaml_bin ^/ "ocamldep.opt"
let ocamlc_path     = ocaml_bin ^/ "ocamlc.opt"
let ocamlopt_path   = ocaml_bin ^/ "ocamlopt.opt"
let ocamlcomp_path (module Mode : Ocaml_mode.S) =
  match Mode.which with
  | `Byte -> ocamlc_path
  | `Native -> ocamlopt_path
let camlp4orf_path  = ocaml_bin ^/ "camlp4orf"
let ocamllex_path   = ocaml_bin ^/ "ocamllex"
let ocamlyacc_path  = ocaml_bin ^/ "ocamlyacc"
let ocaml_path      = ocaml_bin ^/ "ocaml"

let camlp4o_path         = root_relative "app/camlp4_with_compiler_libs/camlp4o.exe"
let camlp4o_path_non_opt = root_relative "app/camlp4_with_compiler_libs/camlp4o-byte.exe"

let ocamlobjinfo_path = ocaml_bin ^/ "ocamlobjinfo"

let expand_dollar_vars_between ~left ~right ~lookup orig =
  match String.lsplit2 orig ~on:'$' with
  | None -> orig (* no dollars, do nothing *)
  | Some (before_first_dollar, after_first_dollar) ->
    let translate after_dollar =
      match (
        match String.chop_prefix after_dollar ~prefix:(String.make 1 left) with
        | None -> None
        | Some after_lp ->
          match String.lsplit2 after_lp ~on:right with
          | None -> None
          | Some (var_name, after_rp) ->
            match (lookup ~var_name) with
            | None -> None
            | Some x -> Some (x, after_rp)
      ) with
      | None -> "$" ^ after_dollar (* cant translate - leave the string as it is*)
      | Some (expansion, after_rp) -> expansion ^ after_rp
    in
    let rec loop acc = function
      | [] -> assert false
      | [last] -> String.concat (List.rev (translate last::acc))
      | after_dollar::xs -> loop (translate after_dollar :: acc) xs
    in
    loop [before_first_dollar] (String.split after_first_dollar ~on:'$')

let expand_dollar_vars ~lookup s =
  let s = expand_dollar_vars_between ~left:'(' ~right:')' ~lookup s in
  let s = expand_dollar_vars_between ~left:'{' ~right:'}' ~lookup s in
  s

let table_to_lookup ~table =
  match String.Table.of_alist table with
  | `Ok h -> fun ~var_name -> Hashtbl.find h var_name
  | `Duplicate_key var ->
    failwithf "duplicate binding for $-var: %S" var ()

(* Expand some $-vars within action strings of rules defined in jbuild files *)
let root_var_table = [
  "-verbose"       , ""; (*"-verbose";*)
  "CPP"            , "cpp";
  "PA_CPP"         , concat_quoted ["cpp"; "-undef"; "-traditional"; "-Werror"];
  "CC"             , concat_quoted
                       (C.Flavor.prog `C :: Compiler_config.arch_cflags);
  "CXX"            , concat_quoted
                       (C.Flavor.prog `Cxx :: Compiler_config.arch_cflags);
  "ocaml_bin"      , ocaml_bin;
  "OCAML"          , ocaml_path;
  "OCAMLC"         , ocamlc_path;
  "OCAMLOPT"       , ocamlopt_path;
  "camlp4orf"      , camlp4orf_path;
  "ocaml_version"  , Compiler_selection.major_version;
  "ocaml_where"    , ocaml_where;
  "ARCH_SIXTYFOUR" , Bool.to_string (not Compiler_selection.m32);
  "USE_NEW_SQML"   , Bool.to_string use_new_sqml;
  "ALLOW_HARDWARE_SPECIALIZATION", Bool.to_string allow_hardware_specialization;
] @ External_apis.root_var_table

let root_var_lookup =
  let lookup = table_to_lookup ~table:root_var_table in
  fun ~dir ~var_name ->
    match var_name with
    | "ROOT" -> Some (reach_from ~dir Path.the_root)
    | _ -> lookup ~var_name

let expand_vars ~dir s =
  expand_dollar_vars ~lookup:(root_var_lookup ~dir) s

(*----------------------------------------------------------------------
 bash
----------------------------------------------------------------------*)

module Bash : sig

  type t
  val create : prog:string -> args:string list -> target:string option -> t
  val action: dir:Path.t -> t list -> Action.t

end = struct

  type t = string

  let create ~prog ~args ~target =
    let com = concat_quoted (prog :: args) in
    match target with
    | None -> com
    | Some target -> sprintf !"%s > %{quote}" com target

  let action ~dir ts =
    let command_string = String.concat ~sep:"; " ts in
    bash ~dir command_string

end

let bash1 ?target prog args = Bash.create ~prog ~args ~target

let write_string_action ?chmod_x string ~target =
  Action.save ?chmod_x (string^"\n") ~target

let write_string_rule ?chmod_x string ~target =
  Rule.create ~targets:[target] (return (write_string_action ?chmod_x string ~target))

let write_names_rule names ~target =
  write_string_rule (String.concat ~sep:" " names) ~target

(*----------------------------------------------------------------------
 remove_target_and_fail
----------------------------------------------------------------------*)

let remove_target_and_fail ~target ~filename_for_error fmt =
  (* Construct an action which when run will echo an error message in a format suitable
     for omake-server. The action then removes the target file & exits 1. *)
  ksprintf (fun mes ->
    bashf ~dir:(dirname target) !"%s rm -f -- %{quote}; exit 1" (
      String.concat (List.map ~f:(fun text -> sprintf "echo %s;" (Shell.escape text)) [
        sprintf "File %S, line 1, characters 0-0:" filename_for_error;
        sprintf "Error: %s" mes;
        sprintf !"when building target: %S" (basename target);
      ]))
      (basename target)
  ) fmt

(*----------------------------------------------------------------------
 jbuild-ignore
----------------------------------------------------------------------*)

let ignore_filter ~dir =
  let path = relative ~dir "jbuild-ignore" in
  Dep.file_exists path *>>= function
  | false -> return (fun _ -> false) (* no subdir is ignored *)
  | true ->
    file_words_allow_commments path *>>| fun words ->
    List.iter words ~f:(fun word ->
      if String.mem word '/' then
        failwithf "%S: %S can't be a directory basename"
          (Path.to_string path) word ()
    );
    let set = String.Hash_set.of_list words in
    fun path -> Hash_set.mem set (basename path)

let unignored_subdirs ~dir =
  Dep.subdirs ~dir *>>= fun paths ->
  ignore_filter ~dir *>>| fun ignore_p ->
  List.filter paths ~f:(fun path -> not (ignore_p path))

let deep_unignored_subdirs ~dir =
  let rec traverse dir =
    unignored_subdirs ~dir *>>= fun dirs ->
    (Dep.all (List.map dirs ~f:traverse) *>>| List.concat) *>>| fun dirs ->
    dir::dirs
  in
  traverse dir

let does_ignore ~dir path =
  ignore_filter ~dir
  *>>| fun ignore_p ->
  ignore_p path

let rec is_ignored dir =
  if Path.(=) dir Path.the_root
  then Dep.return false
  else
    is_ignored (Path.dirname dir)
    *>>= function
    | true -> Dep.return true
    | false ->
      does_ignore ~dir:(Path.dirname dir) dir

(*----------------------------------------------------------------------
 all_the_repos
----------------------------------------------------------------------*)

let all_the_repos =
  begin
    Dep.subdirs ~dir:Path.the_root *>>| fun xs -> Path.the_root :: xs
  end
  *>>= fun candidate_dirs ->
  Dep.List.concat_map candidate_dirs ~f:(fun dir ->
    Dep.file_exists (relative ~dir ".hg") *>>| function
    | true -> [dir]
    | false -> []
  )

(*----------------------------------------------------------------------
 hg manifest
----------------------------------------------------------------------*)

let hg_prog = Config.hg_prog

let manifest_dirs_filename = ".manifest.dirs"
let manifest_dirs_path ~repo = relative ~dir:repo manifest_dirs_filename

let manifest_dirs_rule ~repo =
  let target = manifest_dirs_path ~repo in
  Rule.create ~targets:[target] (
    Dep.all_unit [
      (*
         This lists files in the hg equivalent of git index.
         The set of such files does not depend on tree contents, but only on dirstate. *)
      Dep.path (Path.absolute hg_prog);
      Dep.path (Path.relative ~dir:repo ".hg/dirstate");
    ] *>>| fun () ->
    (* Here and in the various calls to hg, we ignore stderr because otherwise we can get
       random failures because hg outputs messages about taking the lock. Unfortunately,
       there seems to be no way to silence these messages. *)
    bashf ~ignore_stderr:true ~dir:repo
      !"%{quote} status -acdmn | sed 's|^|./|' | rev | cut -d/ -f2- | rev | sort -u > %{quote}"
      hg_prog (basename target)
  )

let setup_manifest ~dir =
  Scheme.dep (
    all_the_repos *>>| fun repos ->
    if not (List.mem repos dir) then Scheme.empty else
      Scheme.rules [
        manifest_dirs_rule ~repo:dir;
        alias_dot_filename_hack ~dir manifest_dirs_filename;
      ])

let manifest_dirs ~repo =
  Dep.contents (manifest_dirs_path ~repo) *>>| fun s ->
  List.map (lines_of_string s) ~f:(relative ~dir:repo)

module Fe = Js_fe.Make(struct
    let all_the_repos = all_the_repos
    let manifest_dirs = manifest_dirs
  end)

let public_release_files_path = root_relative ".public-release.files"

let public_release_files_rule =
  Fe.rule_for_projection_files ~dir:Path.the_root
    (Fe.Projection.create ~repo:Path.the_root ~name:"public-release")
    ~target:public_release_files_path

(*----------------------------------------------------------------------
 Libmap
----------------------------------------------------------------------*)

module From_compiler_distribution = Ocaml_types.From_compiler_distribution
module Lib_dep                    = Ocaml_types.Lib_dep
module Lib_in_the_tree            = Ocaml_types.Lib_in_the_tree
module Findlib_package            = Ocaml_types.Findlib_package

module Libmap : sig
  type t
  val create_exn : Lib_in_the_tree.t list * Findlib_package_name.Set.t -> t
  val exists_in_the_tree : t -> lib_in_the_tree:LN.t -> bool
  val reverse_look : t -> (Path.t -> Lib_in_the_tree.t list) Staged.t

  val fold : t -> init:'a -> f:(Lib_dep.t -> 'a -> 'a) -> 'a

  val resolve_libname     : t -> lib_in_the_tree:LN.t -> Lib_in_the_tree.t option
  val resolve_libname_exn : t -> lib_in_the_tree:LN.t -> Lib_in_the_tree.t

  val resolve_libdep_name      : t -> Libdep_name.t      -> Lib_dep.t option
  val resolve_libdep_name_exn  : t -> Libdep_name.t      -> Lib_dep.t
  val resolve_libdep_names_exn : t -> Libdep_name.t list -> Lib_dep.t list

  (** Resolve a dependency name given as a string.

      This is for library names that are hard-coded in the jenga rules. Given that this
      must work in the public release where the various libraries are split into multiple
      separate packages, you must specify the public name of the library, except if this
      library doesn't have a public name. *)
  val resolve_string_exn : t -> string -> Lib_dep.t

  val load_lib_deps : t -> Path.t -> Lib_dep.t list Dep.t
end = struct
  type t = Lib_dep.t Libdep_name.Table.t

  let create_exn ((libs : Lib_in_the_tree.t list), findlib_packages) =
    let t : Lib_dep.t Libdep_name.Table.t = Libdep_name.Table.create () in
    (* If a lib is both and in the tree and in findlib, the lib in the tree takes
       precedence. *)
    List.iter From_compiler_distribution.all ~f:(fun compiler_lib ->
      let data : Lib_dep.t =
        (* When using findlib, compiler packages must be treated as findlib
           packages. Otherwise they might overlap with transitive dependencies from
           findlib packages *)
        if Findlib.use_findlib then
          Findlib_package
            { name = From_compiler_distribution.ocamlfind_package compiler_lib }
        else
          From_compiler_distribution compiler_lib
      in
      Hashtbl.set t ~key:(From_compiler_distribution.libdep_name compiler_lib) ~data);
    Set.iter findlib_packages ~f:(fun pkg ->
      Hashtbl.set t ~key:(Libdep_name.of_findlib_package_name pkg)
        ~data:(Findlib_package { name = pkg }));
    Map.iteri (List.map libs ~f:(fun lib -> (lib.name, lib)) |> LN.Map.of_alist_multi)
      ~f:(fun ~key:libname ~data ->
        match data with
        | [(lib : Lib_in_the_tree.t)] ->
          (* Internal libraries can be specified by their public name as well. This
             simplify the handling of libraries hardcoded in the jengaroot. *)
          Option.iter lib.public_name ~f:(fun pkg ->
            Hashtbl.set t ~key:(Libdep_name.of_findlib_package_name pkg)
              ~data:(In_the_tree lib));
          Hashtbl.set t ~key:(Libdep_name.of_libname lib.name)
            ~data:(In_the_tree lib)
        | _ ->
          failwithf !"Duplicate definition of library '%{LN}' in %s"
            libname (String.concat ~sep:" and " (List.map data ~f:(fun lib ->
              sprintf "%S" (Path.to_string lib.source_path)))) ());
    t

  let resolve_libdep_name (t : t) dep = Hashtbl.find t dep
  let resolve_libdep_name_exn t dep =
    match Hashtbl.find t dep with
    | Some x -> x
    | None -> failposf ~pos:[%here] !"unknown library %{Libdep_name}" dep ()
  let resolve_libdep_names_exn t deps = List.map deps ~f:(resolve_libdep_name_exn t)

  let resolve_libname_exn t ~lib_in_the_tree:libname =
    match resolve_libdep_name t (Libdep_name.of_libname libname) with
    | None -> failheref [%here] !"dont know about library: %{LN}" libname ()
    | Some (In_the_tree lib) -> lib
    | Some (Findlib_package _ | From_compiler_distribution _) ->
      failheref [%here] !"library is not part of the tree: %{LN}" libname ()

  let resolve_libname t ~lib_in_the_tree:libname =
    match resolve_libdep_name t (Libdep_name.of_libname libname) with
    | Some (In_the_tree lib) -> Some lib
    | None | Some (Findlib_package _ | From_compiler_distribution _) -> None

  let resolve_string_exn t name =
    let lib_dep : Lib_dep.t = resolve_libdep_name_exn t (Libdep_name.of_string name) in
    match lib_dep with
    | Findlib_package _ | From_compiler_distribution _ -> lib_dep
    | In_the_tree lib ->
      match lib.public_name with
      | None -> lib_dep
      | Some public_name ->
        if Findlib_package_name.to_string public_name = name then
          lib_dep
        else
          failheref [%here]
            !"library %{LN} has a public name: %{Findlib_package_name}.\n\
              You must refer to it in the jenga rules by its public name, \
              otherwise things won't work properly in the public release"
            lib.name public_name
            ()

  let exists_in_the_tree t ~lib_in_the_tree =
    Option.is_some (resolve_libname t ~lib_in_the_tree)

  let fold t ~init ~f =
    Hashtbl.fold t ~init ~f:(fun ~key:_ ~data acc -> f data acc)

  let reverse_look t =
    let table =
      Hashtbl.to_alist t
      |> List.filter_map ~f:(fun (_, x) ->
        match (x : Lib_dep.t) with
        | In_the_tree lib -> Some (lib.source_path, lib)
        | Findlib_package _ | From_compiler_distribution _ -> None)
      |> Path.Table.of_alist_multi
    in
    Staged.stage (fun a -> Option.value (Hashtbl.find table a) ~default:[])
  ;;

  let load_lib_deps t path =
    Dep.contents path *>>| fun s ->
    words_of_string s
    |> List.map ~f:Libdep_name.of_string
    |> resolve_libdep_names_exn t
end

(*----------------------------------------------------------------------
 Use types to capture different varieties of module names
----------------------------------------------------------------------*)

(**
   Name of a single module within a library, with its first letter matching the case of
   the first letter of the .ml/i files containing the module code.

   Both bare names 'foo' and 'Foo' refer to the same module, but the case of the first
   letter is used to determine the file names of .ml, .mli and build artifacts.

   Bare module name that matches the name of the library is treated specially:
   users are not allowed to write such a module in wrapped libraries
   and users can only write such module in unwrapped libraries.
**)


module Prefixed_module_name : sig
  include module type of struct include Prefixed_module_name end

  val of_barename : wrapped:bool -> libname:LN.t -> BN.t -> t
end = struct
  include PN

  let of_barename ~wrapped ~libname name =
    assert (wrapped ==> not (BN.is_lib ~libname name));
    of_string (
      if wrapped then
        LN.prefix libname ^ (String.capitalize (BN.to_string name))
      else
        BN.to_string name
    )
end
module PN = Prefixed_module_name

(*----------------------------------------------------------------------
 end - names
----------------------------------------------------------------------*)

module User_or_gen_config : sig

  val load : dir: Path.t -> Jbuild.t list Dep.t
  val source_file : dir:Path.t -> Path.t
  val libnames : dir: Path.t -> (LN.t * Findlib_package_name.t option) list Dep.t

end = struct

  let source_file ~dir = relative ~dir "jbuild"
  let load ~dir =
    let jbuild = source_file ~dir in
    Dep.file_exists jbuild *>>= function
    | false -> return []
    | true ->
      Dep.contents jbuild
      *>>| fun contents ->
      (* We should try to move some of this nonsense into sexplib:
         - of_string_conv_exn should give errors with proper location on failure
         - we should have list_of_string_conv_exn that parses a list of sexps *)
      let fail line char_start char_end exn =
        let file = Path.to_string jbuild in
        printf "File %S, line %d, characters %d-%d: %s\n"
          file line char_start char_end (Exn.to_string exn);
        failwithf "%s:%d:%d: sexp conversion" file line char_start ()
      in
      match
        Sexp.of_string_conv ("(" ^ contents ^ "\n)")
          [%of_sexp: Jbuild_with_if.t list]
      with
      | exception exn -> fail 1 0 0 exn
      | `Result r ->
        List.concat_map r ~f:(function
          | #Jbuild.t as t -> [t]
          | `if_ocaml_code_is_dynlinkable { if_dynlinkable; if_not_dynlinkable } ->
            if dynlinkable_code then if_dynlinkable else if_not_dynlinkable)
      | `Error (exn, annot_sexp) ->
        let range = Sexp.Annotated.get_range annot_sexp in
        fail range.start_pos.line range.start_pos.col
          (range.start_pos.col + range.end_pos.offset - range.start_pos.offset) exn
  ;;

  let the_real_libnames_for_libmap : Jbuild.t -> _ = function
    | `ocamllex _ -> []
    | `ocamlyacc _ -> []
    | `preprocessor x -> [(Preprocessor_conf.name x, None)]
    | `library x -> [(Library_conf.name x, Library_conf.public_name x)]
    | `executables _ -> []
    | `embed _ -> []
    | `jane_script _ -> []
    | `compile_c _ -> []
    | `rule _ -> []
    | `alias _ -> []
    | `no_utop -> []
    | `unified_tests _ -> []
    | `toplevel_expect_tests _ -> []
    | `requires_camlp4 -> []
    | `public_repo _ -> []
    | `html _ -> []

  let libnames ~dir =
    load ~dir *>>| List.concat_map ~f:the_real_libnames_for_libmap

end

let prefix_args ~wrapped ~libname ~name =
  assert (wrapped ==> not (BN.is_lib ~libname name));
  if not wrapped
  then []
  else ["-o"; PN.to_string (PN.of_barename ~wrapped ~libname name)]

(*----------------------------------------------------------------------
 generate/compile renaming file (replacement for -pack)
----------------------------------------------------------------------*)

let dash_ml_gen = ".ml-gen"

let gen_renaming_file ~dir ~libname ~modules =
  (* Generate renaming file: [mylib.ml-gen] *)
  let wrapped = true in
  let target = LN.suffixed ~dir libname dash_ml_gen in
  let ml_text =
    String.concat ~sep:"\n" (List.map modules ~f:(fun name ->
      let prefixed_name = PN.of_barename ~wrapped ~libname name in
      sprintf "module %s = %s"
        (String.capitalize (BN.to_string name))
        (String.capitalize (PN.to_string prefixed_name))
    ))
  in
  (* [No_such_module] is a name for a module that (hopefully) doesn't exist,
     but we can still make an alias to it as long as the alias stays unused.
     This makes the error messages slightly better in rare cases
     compared to if we used [struct end] *)
  let shadow =
    sprintf !"module No_direct_access_to_%{LN} = struct\
              \n  module %s = No_such_module\
              \n%s\
              \nend\
              \n\n"
      libname (LN.to_module libname)
      (String.concat ~sep:"\n" (List.map modules ~f:(fun name ->
         let prefixed_name = PN.of_barename ~wrapped ~libname name in
         sprintf "  module %s = No_such_module"
           (String.capitalize (PN.to_string prefixed_name))
       )))
  in
  Rule.create ~targets:[target] (
    return (
      write_string_action (shadow ^ ml_text) ~target
    )
  )

(* -intf-suffix is used to make the compiler look for a file other than the .mli to find
   out whether there is a .cmi. We abuse it a bit by passing ".ml" (which will exist) to
   force the compiler to read the cmi, or some random suffix to force the compiler to not
   read the .cmi. *)
let read_or_create_cmi v ml_suf =
  match v with
  | `Read -> [ "-intf-suffix"; ml_suf ]
  | `Create -> [ "-intf-suffix"; ".no-mli" ]

let compile_renaming (module Mode : Ocaml_mode.S) ~libname ~dir =
  (* Special compile rule for renaming file: [mylib.ml-gen]. *)
  let name = BN.of_libname libname in
  let suffixed = BN.suffixed ~dir name in
  let ml_gen = suffixed dash_ml_gen in
  let cmi = suffixed ".cmi" in
  let cmt = suffixed ".cmt" in
  let targets = List.map ~f:suffixed Mode.cmx_and_o in
  let deps = [Dep.path ml_gen] in
  let more_targets, more_deps, bin_annot_flag, cmi_action =
    match Mode.which with
    | `Native -> [cmt; cmi], [], Top.bin_annot_flag, `Create
    | `Byte -> [], [Dep.path cmi], [], `Read
  in
  Rule.create ~targets:(targets @ more_targets) (
    Dep.all_unit (deps @ more_deps) *>>| fun () ->
    Action.process
      ~dir
      (ocamlcomp_path (module Mode))
      (List.concat [
        bin_annot_flag;
        read_or_create_cmi cmi_action dash_ml_gen;
        ["-w"; "@a-49"]; (* warning 49: Absent cmi file when looking up module alias. *)
        ["-no-alias-deps"];
        ["-c"];
        ["-impl"; basename ml_gen];
      ])
  )

let renaming_rules ~dir ~libname ~modules =
  [
    gen_renaming_file ~dir ~libname ~modules;
    compile_renaming Ocaml_mode.native ~libname ~dir;
    compile_renaming Ocaml_mode.byte ~libname ~dir;
  ]

(*----------------------------------------------------------------------
 liblinks
----------------------------------------------------------------------*)

let link_to_remote ~remote ~local =
  let dir = dirname local in
  let deps = [Dep.path remote] in
  Rule.create ~targets:[local] (
    Dep.all_unit deps *>>= fun () ->
    return (
      Bash.action ~dir [
        bash1 "rm" ["-f"; basename local];
        bash1 "ln" ["-s"; reach_from ~dir remote; basename local];
      ]
    )
  )

module Lib_modules : sig
  (** A representation of the list of modules in a library, other than the module
      named after the library itself. *)
  type t =
    { impls : BN.t list
    ; intfs : BN.t list
    ; impls_and_intfs : BN.t list
    ; bin_annot : bool
    }
  [@@deriving fields]
  val empty : t
  val rule : dir:Path.t -> libname:LN.t -> t -> Rule.t
  val load : dir:Path.t -> libname:LN.t -> t Dep.t
end = struct
  type t =
    { impls : BN.t list
    ; intfs : BN.t list
    ; impls_and_intfs : BN.t list
    ; bin_annot : bool
    }
  [@@deriving sexp, fields]

  (* bin_annot is irrelevant when the rest is empty *)
  let empty = { impls = []; intfs = []; impls_and_intfs = []; bin_annot = false }

  let file ~dir ~libname = LN.suffixed ~dir libname ".modules"

  let rule ~dir ~libname t =
    let t =
      { impls = List.sort ~cmp:BN.compare t.impls
      ; intfs = List.sort ~cmp:BN.compare t.intfs
      ; impls_and_intfs = List.sort ~cmp:BN.compare t.impls_and_intfs
      ; bin_annot = t.bin_annot;
      }
    in
    write_string_rule
      (t |> sexp_of_t |> Sexp.to_string_hum)
      ~target:(file ~dir ~libname)

  let load ~dir ~libname =
    Dep.contents (file ~dir ~libname)
    *>>| fun str -> Sexp.of_string_conv_exn (String.strip str) t_of_sexp
end

let pack_order_file ~dir ~libname = LN.suffixed ~dir libname ".pack-order"
let stub_names_file ~dir ~libname = LN.suffixed ~dir libname ".stub.names"

let stub_names_rule ~dir ~libname ~stub_names =
  write_names_rule stub_names ~target:(stub_names_file ~dir ~libname)

let stubs_archive_file name = "lib" ^ name ^ "_stubs.a"
let stubs_dependencies ~dir ~libname =
  file_words (LN.suffixed ~dir libname ".stub.names") *>>= fun stub_names ->
  Dep.all_unit (List.map stub_names ~f:(fun name ->
    Dep.path (Path.relative ~dir (stubs_archive_file name))
  ))

module LL : sig

  (** For compiling *)
  val include_flags : dir:Path.t -> Lib_dep.t list -> string list
  val interface_deps : Libmap.t -> Lib_dep.t -> Lib_dep.t list Dep.t

  (** For linking

      These functions handle libraries that are fully known by jenga: libraries in the
      tree and from the compiler distribution. For external packages, one must use the
      [Findlib] module. *)
  val transitive_deps : Libmap.t -> Lib_dep.t -> Lib_dep.t list Dep.t
  val link_flags : dir:Path.t -> Lib_dep.t -> cmxa:string -> string list
  val dep_on_stubs : Lib_dep.t list -> unit Dep.t list
  val dep_on_default_alias : Lib_dep.t list -> unit Dep.t list

  (** For both compiling and linking *)
  val dep_on_ocaml_artifacts : Lib_dep.t list -> suffixes:string list -> unit Dep.t list

  (** Direct access to the artifacts (camlp4, merlin, ocaml-plugin etc) *)
  val path_to_ocaml_artifact : lib_in_the_tree:LN.t -> suf:string -> Path.t
  val submodule_cmi_paths : Lib_dep.t -> Path.t list Dep.t
  val in_the_tree_library_dir : LN.t -> Path.t

  (** How to build the liblinks directories. *)
  val api_rule : dir:Path.t -> Libmap.t -> Rule.t
  val rules : dir:Path.t -> Libmap.t -> Rule.t list Dep.t

  val dir : Path.t
end = struct

  module In_the_tree = struct
    let liblinks_dirname = ".liblinks"
    let liblinks_dir = root_relative liblinks_dirname
    let liblink_dir ~libname = relative ~dir:liblinks_dir (LN.to_string libname)
    let liblink_refname ~libname ~name = relative ~dir:(liblink_dir ~libname) name
    let liblink_ref ~libname ~suf =
      liblink_refname ~libname ~name:(LN.to_string libname ^ suf)

    let liblink_deps ~libname ~suffixes =
      List.map suffixes ~f:(fun suf ->
        Dep.all_unit [
          Dep.path (liblink_ref ~libname ~suf);
          begin match suf with
          | ".cmi" ->
            (* This alias includes a dependency on the list *.cmx. We setup a glob dependency
               on *.cmx (the list, not the files themselves) to ensure rebuilds when
               X_LIBRARY_INLINING transitions from true -> false *)
            Dep.alias (Alias.submodule_cmis ~dir:(liblink_dir ~libname))
          | ".cmx" ->
            Dep.alias (Alias.submodule_cmxs ~dir:(liblink_dir ~libname))
          | _ -> Dep.return ()
            end;
        ])

    let liblink_default ~libname =
      Dep.alias (Alias.default ~dir:(liblink_dir ~libname))

    let liblink_interfaces libmap ~libname =
      Libmap.load_lib_deps libmap (liblink_ref ~libname ~suf:".interface.deps")

    let liblinks_stubs ~libname =
      stubs_dependencies ~dir:(liblink_dir ~libname) ~libname

    let make_liblink_rule ~(lib:Lib_in_the_tree.t) name =
      let remote = relative ~dir:lib.source_path name in
      let local = liblink_refname ~libname:lib.name ~name in
      link_to_remote ~remote ~local

    let libname_from_liblink_path ~dir = LN.of_string (basename dir)

    (* Weirdly, we create symlinks for the .cmt/.cmti of the main module even though the
       target may not be buildable. When we get rid of packing, we should consider
       - removing the cmt from here
       - using the same -bin-annot to compile the main module than for the other modules
       - including the main module in the Lib_modules.t
       so that we only have rules creating symlinks to existing files. *)
    let the_liblink_suffixes =
      cmi_maybe_cmx
      @ [".cmxs"; ".cmo";".cma";".cmxa";".a";".libdeps";".interface.deps";
         ".stub.names"; ".cmti"; ".cmt";
         Js_of_ocaml.cma_suf; Js_of_ocaml.jsdeps_suf
        ]

    let submodule_names ~(lib : Lib_in_the_tree.t) =
      Lib_modules.load ~dir:lib.source_path ~libname:lib.name

    let liblink_submodule_cmi_paths ~lib =
      submodule_names ~lib *>>| fun subs ->
      liblink_refname ~libname:lib.name ~name:(LN.to_string lib.name ^ ".cmi") ::
        List.filter_map subs.impls_and_intfs ~f:(fun sub ->
          if BN.is_lib ~libname:lib.name sub
          then None
          else
            let wrapped = true in
            let pn = PN.to_string (PN.of_barename ~wrapped ~libname:lib.name sub) ^ ".cmi" in
            Some (liblink_refname ~libname:lib.name ~name:pn)
        )

    let main_api ~(lib:Lib_in_the_tree.t) =
      let cmi_name = LN.to_string lib.name ^ ".cmi" in
      let cmti_name = LN.to_string lib.name ^ ".cmti" in
      let cmti_path = relative ~dir:lib.source_path cmti_name in
      let cmt_name = LN.to_string lib.name ^ ".cmt" in
      let cmt_path = relative ~dir:lib.source_path cmt_name in
      Dep.both
        (Dep.file_exists cmti_path)
        (Dep.file_exists cmt_path) *>>| fun (cmti, cmt) ->
      List.concat [
        [cmi_name];
        if cmti then [cmti_name] else [];
        if cmt then [cmt_name] else []
      ]

    let api_rule ~dir libmap =
      let deps =
        Libmap.fold libmap ~init:[] ~f:(fun lib_dep acc ->
          match lib_dep with
          | In_the_tree lib ->
            Dep.alias (Alias.api ~dir:(liblink_dir ~libname:lib.name)) :: acc
          | Findlib_package _ | From_compiler_distribution _ -> acc)
      in
      Rule.alias (Alias.api ~dir) deps

    let rules ~dir libmap =
      let lib =
        Libmap.resolve_libname_exn libmap
          ~lib_in_the_tree:(libname_from_liblink_path ~dir)
      in
      let remote_stub_names_file = stub_names_file ~dir:lib.source_path ~libname:lib.name in
      file_words remote_stub_names_file *>>= fun stub_names ->
      submodule_names ~lib *>>| fun modules ->
      let files_for bns ~suffix =
        List.filter_map bns ~f:(fun mod_ ->
          if BN.is_lib ~libname:lib.name mod_
          then None
          else
            let wrapped = true in
            Some (PN.to_string (PN.of_barename ~wrapped ~libname:lib.name mod_) ^ suffix)
        )
      in
      let cmis = files_for modules.impls_and_intfs ~suffix:".cmi" in
      let cmxs =
        if x_library_inlining
        then files_for modules.impls ~suffix:".cmx"
        else []
      in
      let cmtis =
        if modules.bin_annot
        then files_for modules.intfs ~suffix:".cmti"
        else []
      in
      let cmts =
        if modules.bin_annot then
          let ml_no_mli =
            List.filter
              ~f:(fun m -> not (List.mem modules.intfs m))
              modules.impls
          in
          files_for ml_no_mli ~suffix:".cmt"
        else []
      in
      let submodule_files_to_link = List.concat [ cmis; cmxs; cmts; cmtis ] in
      let submodule_link_rules =
        List.map submodule_files_to_link ~f:(make_liblink_rule ~lib)
      in
      let submodule_alias_rules =
        let make_alias ?(dep = Dep.return ()) alias_in_dir names =
          let dependencies =
            dep :: List.map names ~f:(fun name -> Dep.path (relative ~dir name))
          in
          Rule.alias (alias_in_dir ~dir)
              [Dep.group_dependencies (Dep.all_unit dependencies)]
        in
        [
          make_alias Alias.submodule_cmis cmis
            ~dep:(Dep.glob_change (Glob.create ~dir "*.cmx")) ;
          make_alias Alias.submodule_cmxs cmxs;
        ]
      in
      let default_rule =
          (* Setup a .DEFAULT alias in the lib-links directory, to indirect to the
             .lib_artifacts alias in the directory where the library code is actually found.
             This .DEFAULT alias is for the use of remote references to the library (i.e. when
             linking .exe) to force the entire default build, including inline_tests *)
        Rule.default ~dir:(liblink_dir ~libname:lib.name) [
          Dep.alias (Alias.lib_artifacts ~dir:lib.source_path)
        ]
      in
      let api_rule =
        Rule.alias (Alias.api ~dir:(liblink_dir ~libname:lib.name)) [
          main_api ~lib *>>= fun main_artifacts ->
          let api = List.concat [ main_artifacts; cmtis; cmts; cmis ] in
          Dep.all_unit
            (List.map api ~f:(fun name ->
               Dep.path (relative ~dir:(liblink_dir ~libname:lib.name) name)));
        ]
      in
      let linked_names =
        List.map the_liblink_suffixes ~f:(fun suf -> LN.to_string lib.name ^ suf)
        @ List.map stub_names ~f:(fun name -> stubs_archive_file name)
      in
      let link_rules = List.map linked_names ~f:(make_liblink_rule ~lib) in
      List.concat [
        link_rules;
        submodule_link_rules;
        submodule_alias_rules;
        [api_rule; default_rule];
      ]
  end

  (* We don't handle findlib dependencies in the following functions for two reasons:

     - the transitive dependencies and artifacts for a given library are not constant;
     they depends on what predicates are in use for a given target

     - for the Makefile generation in our open-source packages, we need this information
     to be obtained dynamically, after the Makefile is generated *)

  let transitive_deps libmap : Lib_dep.t -> _ = function
    | From_compiler_distribution compiler_lib ->
      return (List.map (From_compiler_distribution.transitive_deps compiler_lib)
                ~f:(fun lib -> Lib_dep.From_compiler_distribution lib))
    | In_the_tree lib ->
      Libmap.load_lib_deps libmap
        (In_the_tree.liblink_refname
           ~libname:lib.name ~name:(LN.to_string lib.name ^ ".libdeps"))
    | Findlib_package _ ->
      return []

  let dep_on_ocaml_artifacts libs ~suffixes =
    List.concat_map libs ~f:(function
      | Lib_dep.In_the_tree lib -> In_the_tree.liblink_deps ~libname:lib.name ~suffixes
      | From_compiler_distribution _ -> []
      | Findlib_package _ -> [])
  ;;

  let path_to_ocaml_artifact ~lib_in_the_tree:libname ~suf =
    In_the_tree.liblink_ref ~libname ~suf

  let dep_on_stubs libs =
    List.filter_map libs ~f:(function
      | Lib_dep.In_the_tree lib -> Some (In_the_tree.liblinks_stubs ~libname:lib.name)
      | From_compiler_distribution _ | Findlib_package _ -> None)
  ;;

  let link_flags ~dir lib ~cmxa =
    match lib with
    | Lib_dep.In_the_tree lib ->
      [ "-I"; reach_from ~dir (In_the_tree.liblink_dir ~libname:lib.name)
      ; LN.to_string lib.name ^ cmxa ]
    | From_compiler_distribution compiler_lib -> begin
        let archive = From_compiler_distribution.to_string compiler_lib ^ cmxa in
        match From_compiler_distribution.search_path_dir compiler_lib with
        | None -> [ archive ]
        | Some dir -> [ "-I"; dir; archive ]
      end
    | Findlib_package _ ->
      []
  ;;

  let include_flags ~dir libs =
    List.concat_map libs ~f:(function
      | Lib_dep.In_the_tree lib ->
        [ "-I"; reach_from ~dir (In_the_tree.liblink_dir ~libname:lib.name) ]
      | From_compiler_distribution compiler_lib -> begin
          match From_compiler_distribution.search_path_dir compiler_lib with
          | None -> []
          | Some dir -> [ "-I"; dir ]
        end
      | Findlib_package _ -> [])
  ;;

  let dep_on_default_alias libs =
    List.filter_map libs ~f:(function
      | Lib_dep.In_the_tree lib -> Some (In_the_tree.liblink_default ~libname:lib.name)
      | From_compiler_distribution _ | Findlib_package _ -> None)
  ;;

  let interface_deps libmap (lib : Lib_dep.t) =
    match lib with
    | In_the_tree lib -> In_the_tree.liblink_interfaces libmap ~libname:lib.name
    | From_compiler_distribution _ | Findlib_package _ -> return []

  let submodule_cmi_paths (lib : Lib_dep.t) =
    match lib with
    | In_the_tree lib -> In_the_tree.liblink_submodule_cmi_paths ~lib
    | From_compiler_distribution name ->
      return (From_compiler_distribution.cmis__partially_implemented
                name ~stdlib_dir:ocaml_where_path)
    | Findlib_package _ ->
      (* We might want to implement that, to embed findlib packages? *)
      return []
  ;;

  let in_the_tree_library_dir libname = In_the_tree.liblink_dir ~libname

  let api_rule = In_the_tree.api_rule
  let rules = In_the_tree.rules
  let dir = In_the_tree.liblinks_dir
end

(*----------------------------------------------------------------------
 code_style
----------------------------------------------------------------------*)

type code_style =

| Requires_camlp4
  (* ML source code is in camlp4-style, i.e. "with sexp".
     But is not suitable for on-the-fly translation to ppx-style. *)

| Switched_to_ppx_style
  (* ML source code is in ppx-style, i.e. [@@deriving sexp]
     So there is no way to run with camlp4-style preprocessing.
     And so no diff rules are setup. *)

(*----------------------------------------------------------------------
 directory context (dc)
----------------------------------------------------------------------*)

module DC = struct
  type t = {
    code_style : code_style;
    dir : Path.t;
    link_flags : string list;
    merlinflags : string list;
    ocamlflags : string list;
    ocamlcflags : string list;
    ocamloptflags : string list;
    xlibnames : LN.t list; (* broader set: from preprocessor/library configs
                              and includes "bin" if there is an executables config *)

    ocaml_plugin_libraries : (string -> Lib_dep.t list option);
    no_utop_alias : bool;
    libmap : Libmap.t;
    impls : BN.t list;
    intfs : BN.t list;
    impl_is_buildable : (BN.t -> bool);
    intf_is_buildable : (BN.t -> bool);
  } [@@deriving fields]
end

let libs_transitive_closure libmap libs =
  Dep.List.concat_map libs ~f:(LL.transitive_deps libmap)
  *>>| fun additional_libs ->
  Lib_dep.remove_dups_preserve_order (additional_libs @ libs)
;;

(*----------------------------------------------------------------------
 local_dependencies
----------------------------------------------------------------------*)

let dep_append ys xsd =
  xsd *>>| fun xs -> ys @ xs

(* Ml compilation requires we setup .cmi/.cmx dependencies for modules listed in the
   corresponding .d file, AND a dependency on the .d file to ensure recompilation in
   case a referenced .ml file has been removed (and so no longer listed in the .d).

   If the .d dependency is missing, we may (incorrectly) fail to rerun the
   compilation, because jenga regards a strict decrease in dependencies as not
   sufficient cause to trigger an action.

*)
let local_dependencies : (
  [`ml | `mli] -> Ocaml_mode.t -> DC.t -> wrapped:bool -> libname:LN.t
  -> BN.t -> unit Dep.t
) =
  fun ml_kind (module Mode : Ocaml_mode.S) dc ~wrapped ~libname x ->
    begin
      let {DC.dir;impl_is_buildable;_} = dc in
      let artifact name suf =
        PN.suffixed ~dir (PN.of_barename ~wrapped ~libname name) suf
      in
      let artifacts_ml name =
        let sufs = [".cmi"] @ (
          if Mode.compilation_depends_on_cmx
          then
            if impl_is_buildable name then [Mode.cmx] else []
          else [])
        in
        List.map ~f:(artifact name) sufs
      in
      let artifacts_mli name =
        let sufs = [".cmi"] in
        List.map ~f:(artifact name) sufs
      in
      let dsuf,artifacts =
        match ml_kind with
        | `mli -> ".mli.d", artifacts_mli
        | `ml ->  ".ml.d", artifacts_ml
      in
      let xd = BN.suffixed ~dir x dsuf in
      dep_append [xd] (
        BN.file_words xd *>>= fun ys ->
        Dep.List.concat_map ys ~f:(fun y ->
          let yd = BN.suffixed ~dir y ".cmi.deps" in
          dep_append (yd :: artifacts y) (
            BN.file_words yd *>>| fun zs ->
            List.concat_map zs ~f:artifacts)))
      *>>| path_remove_dups_and_sort
    end
    *>>= fun paths ->
    Dep.all_unit (List.map paths ~f:Dep.path)

(*----------------------------------------------------------------------
 objdeps/libdeps
----------------------------------------------------------------------*)

let gen_transitive_deps
  : to_string:('v -> string)
 -> one_step : 'v list Dep.t
 -> read_deps : ('v -> 'v list Dep.t)
 -> target : Path.t
 -> Rule.t =
  fun ~to_string ~one_step ~read_deps ~target ->
    Rule.create ~targets:[target] (
      one_step *>>= fun names1 ->
      Dep.List.concat_map names1 ~f:read_deps *>>| fun namesM ->
      let names = List.map (namesM @ names1) ~f:to_string in
      let names = remove_dups_preserve_order names in
      write_string_action (String.concat ~sep:" " names) ~target
    )

let moduledeps ~dir name = BN.file_words (BN.suffixed ~dir name ".moduledeps")
let gen_moduledeps ~dc name =
  (* x.moduledeps is the list of local modules referenced transitively by x. This differs
     from objdeps, because that one doesn't consider dependencies on mli, and cmi.deps
     because that doesn't consider dependencies on ml files that have mlis. *)
  let { DC.dir; intf_is_buildable; _ } = dc in
  let suf = ".moduledeps" in
  let target = BN.suffixed ~dir name suf in
  let one_step =
    Dep.both
      (file_words (BN.suffixed ~dir name ".ml.d"))
      (if intf_is_buildable name
       then file_words (BN.suffixed ~dir name ".mli.d")
       else return [])
    *>>| fun (bns1, bns2) ->
    remove_dups_preserve_order (bns1 @ bns2)
  in
  gen_transitive_deps
    ~to_string:Fn.id
    ~one_step
    ~read_deps:(fun x -> file_words (suffixed ~dir x suf))
    ~target
;;

let gen_objdeps ~dir name ~exists_ml =
  let suf = ".objdeps" in (* transitive closure of .ml.d *)
  let target = BN.suffixed ~dir name suf in
  if exists_ml
  then
    let one_step = BN.file_words (BN.suffixed ~dir name ".ml.d") in
    gen_transitive_deps
      ~to_string:BN.to_string
      ~one_step
      ~read_deps:(fun x -> BN.file_words (suffixed ~dir (BN.to_string x) suf))
      ~target
  else
    Rule.create ~targets:[target] (return (
      remove_target_and_fail
        ~target
        ~filename_for_error:(BN.to_string name ^ ".mli")
        !"%{BN}.ml is missing" name))
;;

let gen_cmideps dc name =
  (* [foo.cmi.deps], lists for module [Foo]
     the cmis which might be read when compiling an ml/mli which refers to Foo *)
  let {DC.dir;intf_is_buildable;_} = dc in
  let suf = ".cmi.deps" in
  let one_step =
    let dsuf = (if intf_is_buildable name then ".mli.d" else ".ml.d") in
    BN.file_words (BN.suffixed ~dir name dsuf)
  in
  gen_transitive_deps
    ~to_string:BN.to_string
    ~one_step
    ~read_deps:(fun x -> BN.file_words (suffixed ~dir (BN.to_string x) suf))
    ~target:(BN.suffixed ~dir name suf)

let get_inferred_1step_deps libmap ~dir ~libname =
  Libmap.load_lib_deps libmap (LN.suffixed ~dir libname ".inferred-1step.deps")

module Objinfo : sig

  type t
  val interface_names : t -> string list
  val parse : string -> t

end = struct

  type crc = Crc of string | Weak
  type name = string
  type import = crc * name
  type t = {
    interfaces : import list;
  } [@@deriving fields]

  let interface_names t = List.map t.interfaces ~f:snd
  let weak_crc = String.make 32 '-'

  let read_import_line line =
    match String.split line ~on:'\t' with
      ["";crc;name] ->
        assert (String.length crc = 32);
        let crc = if String.(=) crc weak_crc then Weak else Crc crc in
        (crc,name)
    | _ -> failwith "read_import_line"

  let rec skip_to_interface_banner = function
    | [] -> failwith "skip_to_interface_banner"
    | line::lines ->
      if String.(line = "Interfaces imported:")
      then lines
      else skip_to_interface_banner lines

  let parse stdout =
    let lines = lines_of_string stdout in
    let lines = skip_to_interface_banner lines in
    let interfaces = List.map lines ~f:read_import_line in
    { interfaces }

end

let gen_interface_deps_from_objinfo (dc : DC.t) ~dir ~wrapped ~libname ~libraries_written_by_user =
  let self = libname in
  let target = LN.suffixed ~dir libname ".interface.deps" in
  Rule.create ~targets:[target] (
    Lib_modules.load ~dir ~libname
    *>>= fun { impls_and_intfs; impls = _; intfs = _; bin_annot = _; } ->
    let names = LN.to_string libname :: (List.map impls_and_intfs ~f:(fun bn ->
      PN.to_string (PN.of_barename ~wrapped ~libname bn)
    )) in
    Dep.List.concat_map names ~f:(fun name ->
      let cmi = suffixed ~dir name ".cmi" in
      Dep.action_stdout (
        Dep.path cmi *>>| fun () ->
        bashf ~dir !"%{quote} %{quote} | fgrep -v -- %{quote}"
          ocamlobjinfo_path (basename cmi) LN.prefix_sep
      ) *>>| fun stdout ->
      let obi = Objinfo.parse stdout in
      let words = Objinfo.interface_names obi in
      let words = List.map ~f:String.uncapitalize words in
      words
    ) *>>| fun words ->
    let candidate_libs =
      (* Filters any word which is not permitted as a libname.
         In particular, any word containing the [library_prefix_sep] *)
      List.filter_map words ~f:LN.of_string_opt
    in
    let libs =
      List.filter_map candidate_libs ~f:(fun libname ->
        match Libmap.resolve_libname dc.libmap ~lib_in_the_tree:libname with
        | None -> None
        | Some lib ->
          if libname <> self then
            Some (Lib_dep.In_the_tree lib)
          else
            None)
    in
    let libs =
      libs @
      List.filter_map libraries_written_by_user ~f:(fun lib ->
        match (lib : Lib_dep.t) with
        | Findlib_package _ -> Some lib
        | In_the_tree _
        | From_compiler_distribution _ -> None)
    in
    let libs = Lib_dep.remove_dups_and_sort libs in
    let words = List.map libs ~f:Lib_dep.to_string in
    write_string_action (String.concat ~sep:" " words) ~target
  )

let gen_libdeps libmap ~dir ~libs libname =
  [
    gen_transitive_deps
      ~to_string:Lib_dep.to_string
      ~one_step:(return libs)
      ~target:(LN.suffixed ~dir libname ".inferred-1step.deps")
      ~read_deps:(LL.interface_deps libmap);

    gen_transitive_deps
      ~to_string:Lib_dep.to_string
      ~one_step:(return libs)
      ~target:(LN.suffixed ~dir libname ".libdeps")
      ~read_deps:(LL.transitive_deps libmap);

    Rule.alias (Alias.libdeps ~dir) [
      Dep.path (LN.suffixed ~dir libname ".libdeps")
    ];
  ]

(*----------------------------------------------------------------------
 Rule_conf, Alias_conf (user rules)
----------------------------------------------------------------------*)

let standard_c_include_search_path_from ~dir = [
  ".";
  ocaml_where;
  reach_from ~dir (root_relative "include")
]

let ppx_dir = root_relative "ppx"

let expand_vars_in_rule ~dir ~targets ~deps orig =
  let dep_exn name = function
    | Some dep -> dep
    | None -> failwithf "Cannot use ${%s} with files_recursively_in" name ()
  in
  let lookup ~var_name =
    match var_name with
    | "@" -> Some (String.concat ~sep:" " targets)
    | "<" -> Some (match deps with [] -> "" | dep1::_ -> dep_exn var_name dep1)
    | "^" ->
      let deps = List.map deps ~f:(dep_exn var_name) in
      Some (String.concat ~sep:" " deps)
    | _ -> root_var_lookup ~dir ~var_name
  in
  expand_dollar_vars ~lookup orig

module Dep_conf_interpret = struct

  include Dep_conf

  let to_depends ~dir = function
    | File s -> Dep.path (Path.relative_or_absolute ~dir (expand_vars ~dir s))
    | Alias s -> Dep.alias (Alias.of_string ~dir (expand_vars ~dir s))
    | Glob_files s ->
      Dep.glob_listing (Glob.of_string ~dir (expand_vars ~dir s)) *>>= fun paths ->
      (* Add deps on the contents of all files matching the glob-string [s], including
         source files AND buildable files. *)
      Dep.all_unit (
        (* Hack around stale artifact deletion not working well enough (apparently jenga
           relies on self-triggering). *)
        List.filter_map paths ~f:(fun path ->
          match Path.basename path with
          | "inline_tests_runner.ml"
          | "inline_benchmarks_runner.ml" -> None
          | _ -> Some (Dep.path path)))
    | Files_recursively_in spec_dir ->

      (* Add deps on the recursive list of files, *and* their contents. Only works
         if the directory only contains source files, otherwise we wouldn't depend
         on what is buildable but not built. *)
      let spec_dir = relative ~dir (expand_vars ~dir spec_dir) in
      deep_unignored_subdirs ~dir:spec_dir *>>= fun dirs ->
      Dep.all_unit (
        List.map dirs ~f:(fun dir ->
          Dep.glob_listing (Glob.create ~dir ~kinds:[`File] "*") *>>= fun paths ->
          Dep.all_unit (List.map paths ~f:Dep.source_if_it_exists)
        )
      )

  let list_to_depends ~dir ts =
    List.map ts ~f:(to_depends ~dir)

  let only_plain_file ~dir = function
    | File s -> Some (expand_vars ~dir s)
    | Alias _ -> None
    | Glob_files _ -> None
    | Files_recursively_in _ -> None
end

let expanded_action ~dir ~targets deps action =
  let command_string =
    let deps = List.map deps ~f:(Dep_conf_interpret.only_plain_file ~dir) in
    expand_vars_in_rule ~dir ~targets ~deps (Unexpanded_command.to_string action)
  in
  bash ~dir command_string

let rule_conf_to_rule ~dir conf =
  let {Rule_conf. targets; deps; action} = conf in
  let action = expanded_action ~dir ~targets deps action in
  simple_rule
    ~targets:(List.map targets ~f:(relative ~dir))
    ~deps:(Dep_conf_interpret.list_to_depends ~dir deps)
    ~action

let alias_conf_to_rule ~dir conf =
  let {Alias_conf. name; deps; action} = conf in
  let action = Option.map action ~f:(expanded_action ~dir ~targets:[] deps) in
  let deps = Dep_conf_interpret.list_to_depends ~dir deps in
  let deps =
    match action with
    | None -> deps
    | Some action -> [Dep.action (Dep.all_unit deps *>>| fun () -> action)]
  in
  Rule.alias (Alias.create ~dir name) deps


module Lib_clients = struct

  let clients ~dir          = Alias.create ~dir "clients"
  let clients_runtest ~dir  = Alias.create ~dir "clients-runtest"

  let aliases = [clients;clients_runtest]

  module Cache = struct
    let file = ".lib-clients-cache"
    let path = root_relative file
    type t = Path.Set.t LN.Map.t [@@deriving sexp]

    let rule ~libmap_dep =
      Rule.create ~targets:[path]
        begin
          Dep.both
            libmap_dep
            (deep_unignored_subdirs ~dir:Path.the_root *>>= fun dirs ->
             Dep.all (List.map dirs ~f:(fun dir ->
               (* If we wanted to check the build of the immediate clients only (instead
                  of the transitive clients) we would use .inferred-1step.deps instead of
                  .libdeps *)
               Dep.glob_listing (Glob.create ~dir "*.libdeps")
             )))
          *>>= fun (libmap, paths) ->
          let paths = List.concat paths in
          Dep.all (
            List.map paths ~f:(fun path ->
              let dir = Path.dirname path in
              Libmap.load_lib_deps libmap path *>>| List.filter_map ~f:(function
                | Lib_dep.In_the_tree lib -> Some (lib.name, dir)
                | From_compiler_distribution _ | Findlib_package _ -> None)
          )) *>>| fun client_dir_by_lib ->
          (* Map and Set give consistent ordering across reexecution of the Dep.t. *)
          let client_dir_by_lib = List.concat client_dir_by_lib in
          let t = LN.Map.of_alist_fold ~init:Path.Set.empty ~f:Set.add client_dir_by_lib in
          Action.save ~target:path (Sexp.to_string_hum (sexp_of_t t))
        end
  end

  let rules ~dir ~libname:sought =
    List.map
      [ (clients ~dir, Alias.default)
      ; (clients_runtest ~dir, Alias.runtest)
      ]
      ~f:(fun (alias, what_to_build) ->
        Rule.alias alias [
          Dep.contents Cache.path *>>= fun contents ->
          let map = Sexp.of_string_conv_exn contents [%of_sexp: Cache.t] in
          let dirs = Option.value ~default:Path.Set.empty (LN.Map.find map sought) in
          Dep.all_unit (List.map (Set.to_list dirs) ~f:(fun dir ->
            Dep.alias (what_to_build ~dir)))
        ])

end

(*----------------------------------------------------------------------
 c/cxx compilation
----------------------------------------------------------------------*)

let compile_c_or_cxx ~dir ~include_search_path ~non_include_flags ~suf ~flavor name =
  let source = name ^ suf in
  let include_search_path =
    include_search_path @ standard_c_include_search_path_from ~dir
  in
  (* shared [flags] for gcc -MM and compile *)
  let flags =
    let include_flags =
      List.concat_map include_search_path ~f:(fun path -> ["-I"; path])
    in
    non_include_flags @ include_flags
  in
  let deps = suffixed ~dir name ".deps" in
  let o = suffixed ~dir name ".o" in
  [
    Rule.create ~targets:[deps] (
      C.deps ~dir ~source ~flavor ~flags
      *>>| fun (`Includes includes, `Search_path search_path) ->
      let search_path_in_comments =
        String.concat (List.map search_path ~f:(sprintf "# %s\n"))
      in
      let includes = String.concat ~sep:"\n" includes in
      write_string_action (search_path_in_comments ^ includes) ~target:deps
    );
    Rule.create ~targets:[o] (
      file_words_allow_commments deps *>>= fun includes ->
      C.known_deps ~dir ~flags ~includes
      *>>| fun () ->
      Action.process ~dir
        (C.Flavor.prog flavor)
        (Compiler_config.arch_cflags @ flags @ ["-c"; source; "-o"; basename o])
    );
    Rule.alias (Alias.c ~dir) [Dep.path o];
  ]

let compile_c ~dir ~include_search_path ~cflags name =
  compile_c_or_cxx ~dir ~include_search_path
    ~flavor:`C
    ~non_include_flags:cflags
    ~suf:".c"
    name

let compile_cxx ~dir ~include_search_path ~cxxflags ~cxx_suf name =
  compile_c_or_cxx ~dir
    ~include_search_path
    ~flavor:`Cxx
    ~non_include_flags:cxxflags
    ~suf:cxx_suf
    name

let static_archive_c ~dir ~o_names ~target =
  let o_files = List.map o_names ~f:(fun file -> file ^ ".o") in
  let deps = List.map ~f:(fun x -> Dep.path (relative ~dir x)) o_files in
  simple_rule ~deps ~targets:[relative ~dir target]
    ~action:(Bash.action ~dir [
      bash1 "rm" ["-f"; target];
      bash1 "ar" (["Drc"; target] @ o_files);
      bash1 "ranlib" [target];
    ])

let user_configured_compile_c_rules ~dir { Compile_c_conf. names; c_flags; includes; } =
  let cflags = Ordered_set_lang.eval_with_standard c_flags ~standard:default_cflags in
  List.concat_map names ~f:(fun name ->
    compile_c ~dir ~cflags ~include_search_path:includes name
  )

(*----------------------------------------------------------------------
 ocamllex/ocamlyacc
----------------------------------------------------------------------*)

let ocamllex_rule ~dir name =
  let suf x = suffixed ~dir name x in
  let ml = suf ".ml" in
  let mll = suf ".mll" in
  simple_rule
    ~deps:[Dep.path mll]
    ~targets:[ml]
    ~action:(
      Action.process ~dir ocamllex_path ["-q"; basename mll]
    )

let ocamlyacc_rule ~dir name =
  let suf x = suffixed ~dir name x in
  let ml = suf ".ml" in
  let mli = suf ".mli" in
  let mly = suf ".mly" in
  simple_rule
    ~deps:[Dep.path mly]
    ~targets:[ml;mli]
    ~action:(
      Action.process ~dir ocamlyacc_path ["-q"; basename mly]
    )

(*----------------------------------------------------------------------
 ml/mli
----------------------------------------------------------------------*)

type ml_kind = ML | MLI

let ml_kind_to_suf = function
  | ML -> ".ml"
  | MLI -> ".mli"

let ml_kind_to_flag = function
  | ML -> "-impl"
  | MLI -> "-intf"

(*----------------------------------------------------------------------
 Preprocessor_name (PP)
----------------------------------------------------------------------*)

module PP : sig
  include Identifiable.S
  val remove_dups_preserve_order : t list -> t list
  val to_libname : t -> LN.t
end = struct
  include String
  let remove_dups_preserve_order = remove_dups_preserve_order
  let to_libname = LN.of_string
end

(*----------------------------------------------------------------------
 preprocessing ocaml file (pps)
----------------------------------------------------------------------*)

module Preprocessors = struct
  (** The subset of [jane] that doesn't introduce dependencies on non portable stuff. *)
  let jane_kernel = List.map ~f:PP.of_string ([
    "pa_type_conv";
    "pa_sexp_conv";
    "pa_bin_prot";
    "pa_fields_conv";
    "pa_variants_conv";
    "pa_typerep_conv";
    "pa_compare";
    "pa_pipebang";
    "pa_here";
    "pa_custom_printf";
    "pa_test";
    "pa_enumerate";
    "pa_fail";
    "pa_structural_sexp";
    "ppx_let";
    "ppx_sexp_message";
    "ppx_inline_test";
    "ppx_bench";
    "ppx_hash";
  ] @ Config.extra_jane_kernel_ppx)

  let jane =
    jane_kernel
    @ List.map ~f:PP.of_string [
      "ppx_expect";
    ]
  ;;

  let sexp_conv = List.map ~f:PP.of_string [
    "pa_type_conv";
    "pa_sexp_conv";
  ]

  let bitstring =
    List.map ~f:PP.of_string
    [
      "bitstring_types";
      "bitstring_config";
      "bitstring";
      "bitstring_persistent";
      "pa_bitstring";
    ]
end

let expand_pps names =
  let names : Unexpanded_pp.t list :> string list = names in
  let flags, pp_names = List.partition_tf names ~f:(String.is_prefix ~prefix:"-") in
  let pps =
    PP.remove_dups_preserve_order (
      List.map ~f:PP.of_string ["pa_untabbed"] @
      List.concat_map pp_names ~f:(function
      | "JANE" -> Preprocessors.jane
      | "JANE_KERNEL" -> Preprocessors.jane_kernel
      | "SEXP_CONV" | "pa_sexp_conv" -> Preprocessors.sexp_conv
      | "BITSTRING" -> Preprocessors.bitstring
      | name -> [PP.of_string name]))
  in
  pps, flags

let eval_names_spec ~dc names_spec =
  let {DC.intfs; intf_is_buildable; impls; impl_is_buildable; _} = dc in
  let filter_out_inline_modules l =
    List.filter l ~f:(function
      | "inline_tests_runner"
      | "inline_benchmarks_runner" -> false
      | _ -> true)
  in
  let iis = List.map ~f:BN.to_string (impls @ intfs) in
  let standard = remove_dups_preserve_order (filter_out_inline_modules iis) in
  let xs = Ordered_set_lang.eval_with_standard (Some names_spec) ~standard in
  let xs = List.map ~f:BN.of_string xs in
  List.filter xs ~f:intf_is_buildable,
  List.filter xs ~f:impl_is_buildable,
  (* Do not filter all names given by user, to allow detection of any names for which
     there is neither a .ml or .mli *)
  xs

(*----------------------------------------------------------------------
 PPXset
----------------------------------------------------------------------*)

let remap_pa_string =
  (* convert camlp4 preprocessor names (pa_) to replacement ppx version *)
  function
  | "pa_untabbed" (* for compatibility with ppx *)
  | "pa_macro"    (* provided by optcomp, which is linked in ppx_driver *)
    -> None
  | "pa_test" -> Some "ppx_assert"
  | "pa_structural_sexp" -> Some "ppx_sexp_value"
  (* default: replace "pa_" with "ppx_" *)
  |  s ->
    match String.chop_prefix s ~prefix:"pa_" with
    | Some s -> Some ("ppx_" ^ s)
    | None -> Some s

let remap_pa_flag = function
  | "-pa-ounit-drop" -> "-inline-test-drop"
  | s -> s

let remap_pa_names pps =
  pps
  |> List.map ~f:PP.to_string
  |> List.filter_map ~f:remap_pa_string


module Standard_pp_sets : sig

  val extract : string list -> string list
  val expand : string list -> string list

end = struct

  let standard_sets = String.Map.of_alist_exn [
    "JANE", (remap_pa_names Preprocessors.jane);
    "JANE_KERNEL", (remap_pa_names Preprocessors.jane_kernel);
    "SEXP", ["ppx_type_conv";"ppx_sexp_conv"];
  ]

  let (--) xs ys = (* preserves order *)
    List.filter xs ~f:(fun x -> not (List.mem ys x))

  let extract names =
    let names,extracted =
      Map.fold standard_sets ~init:(names,[]) ~f:(
        fun ~key:tag ~data:potential_extraction (names,extracted) ->
          match (potential_extraction -- names) with
          | _::_ -> names,extracted
          | [] ->
            let remaining = names -- potential_extraction in
            remaining, tag :: extracted)
    in
    List.rev extracted @ names

  let expand names =
    List.concat_map names ~f:(fun name ->
      Option.value (Map.find standard_sets name) ~default:[name])

end

let ppx_cache_dir = root_relative ".ppx"

module PPXset : sig

  type t
  val create : PP.t list -> t
  val to_libs : t -> Libmap.t -> Lib_dep.t list
  val parse_subdir_name : dir:Path.t -> t option
  val exe_path : t -> Path.t

end = struct

  type t = { sorted : string list }

  let to_libs t libmap =
    List.map t.sorted ~f:(fun s ->
      Libmap.resolve_libdep_name_exn libmap (Libdep_name.of_string s))

  let create1 names = {
    sorted = remove_dups_and_sort names;
  }

  let create pps = create1 (remap_pa_names pps)

  let subdir_name t =
    match Standard_pp_sets.extract t.sorted with
    | [] -> "NONE"
    | _ :: _ as names -> String.concat names ~sep:"+"

  let parse_subdir_name ~dir =
    let basename = basename dir in
    let names =
      match basename with
      | "NONE" -> []
      | _ as base -> Standard_pp_sets.expand (String.split base ~on:'+')
    in
    let result = create1 names in
    if String.(=) (subdir_name result) basename
    then Some result
    else None

  let exe_path t =
    relative ~dir:(relative ~dir:ppx_cache_dir (subdir_name t)) "ppx.exe"

end

let ppx_executable pps = PPXset.exe_path (PPXset.create pps)


let metaquot_exe = (* -ppx *)
  root_relative "external/ppx_tools_0.99.2/syntax/standalone/main.exe"

(*----------------------------------------------------------------------
 pp kind
----------------------------------------------------------------------*)

module PP_style = struct

  type t =
  | Nothing
  | Command of string
  | Metaquot (* ML source code uses ppx-style meta-quotations: i.e. [%expr ],. [%e ]
                (probably to implement a pre-processor) *)
  | PP of PP.t list * string list * code_style

  let of_kind dc (kind:Preprocess_kind.t) =
    let {DC. dir; code_style; _} = dc in
    match kind with
    | `no_preprocessing -> Nothing
    | `command s        -> Command (expand_vars ~dir (Unexpanded_command.to_string s))
    | `metaquot         -> Metaquot
    | `pps names ->
      let pps, flags = expand_pps names in
      PP (pps, flags, code_style)

end

(*----------------------------------------------------------------------
 ML compilation context (mc)
----------------------------------------------------------------------*)

module MC = struct
  type t = {
    dc : DC.t;
    dir : Path.t;
    libname : LN.t;

    can_setup_inline_runners : bool;
    x_libs : Lib_dep.t list;
    pp_style : PP_style.t;
    preprocessor_deps : Dep_conf.t list;

    exists_ml : bool;
    exists_mli : bool;
    wrapped : bool;
    must_be_sharable : bool;
    findlib_include_flags : string list Findlib.Query.t;
  }
end

(*----------------------------------------------------------------------
 lookup_pp
----------------------------------------------------------------------*)

let lookup_pp dc ~default_pp ~preprocess_spec =
  let map =
    BN.Map.of_alist_exn (List.concat_map preprocess_spec ~f:(fun (kind, names_spec) ->
      let _, _, modules = eval_names_spec ~dc names_spec in
      let pp_style = PP_style.of_kind dc kind in
      List.map modules ~f:(fun name -> (name, pp_style))
    ))
  in
  let default : PP_style.t =
    match default_pp with
    | None -> Nothing
    | Some s -> Command s
  in
  stage (fun name -> Option.value (Map.find map name) ~default)

(*----------------------------------------------------------------------
 local ppx.exe
----------------------------------------------------------------------*)

let link_quietly = relative ~dir:Config.script_dir "link-quietly"

let ppx_driver_runner libmap = Libmap.resolve_string_exn libmap "ppx_driver.runner"

let link_deps_of_unforced_libs (module Mode : Ocaml_mode.S) ~libs =
  LL.dep_on_ocaml_artifacts libs ~suffixes:Mode.cmxa_and_a
  @ LL.dep_on_stubs libs

let generate_ppx_exe_rules libmap ~dir ~link_flags =
  match PPXset.parse_subdir_name ~dir with
  | None -> []
  | Some ppxset ->
    let target = PPXset.exe_path ppxset in
    let exe = basename target in
    let libs = PPXset.to_libs ppxset libmap in
    let ppx_driver_runner = ppx_driver_runner libmap in
    let libs = libs @ [ppx_driver_runner] in
    let lib_deps = libs_transitive_closure libmap libs in
    let fl_archives =
      Findlib.archives Ocaml_mode.native ~dir ~exe lib_deps
        ~predicates:["custom_ppx"; "ppx_driver"]
    in
    let fl_include_flags = Findlib.include_flags ~dir exe lib_deps in
    Rule.create ~targets:[target] (
      lib_deps *>>= fun libs ->
      (* Make sure we do link ppx_driver_runner last, otherwise we have a problem. *)
      [%test_eq: Lib_dep.t option] (List.last libs) (Some ppx_driver_runner);
      let deps =
        Dep.path link_quietly ::
        link_deps_of_unforced_libs ~libs Ocaml_mode.native
      in
      Findlib.Query.result_and fl_archives
        (Findlib.Query.result_and fl_include_flags
           (Dep.all_unit deps))
      *>>| fun (external_archives, (external_include_flags, ())) ->
      Action.process ~dir
        (reach_from ~dir link_quietly)
        (List.concat
           [ [ ocamlopt_path
             ; "-g"
             ; "-linkall"
             ]
           ; external_include_flags
           ; external_archives
           ; link_flags
           ; [ "-thread" ]
           ; List.concat_map libs ~f:(LL.link_flags ~dir ~cmxa:".cmxa")
           ; [ "-o"; basename target ]
           ]))
    :: List.concat_map [fl_archives; fl_include_flags] ~f:Findlib.Query.rules

(*----------------------------------------------------------------------
 pp libs
----------------------------------------------------------------------*)

let libs_for_code_generated_by_pp libmap pp =
  begin match remap_pa_string (PP.to_string pp) with
    (* These must resolve to findlib package names *)
    | Some "ppx_sexp_conv"     -> [ "sexplib" ]
    | Some "ppx_bin_prot"      -> [ "bin_prot" ]
    | Some "ppx_fields_conv"   -> [ "fieldslib" ]
    | Some "ppx_variants_conv" -> [ "variantslib" ]
    | Some "ppx_assert"        -> [ "ppx_assert.runtime-lib" ]
    | Some "ppx_bench"         -> [ "ppx_bench.runtime-lib" ]
    | Some "ppx_expect"        -> [ "ppx_expect.collector" ]
    | Some "ppx_hash"          -> [ "ppx_hash.runtime-lib" ]
    | Some "ppx_inline_test"   -> [ "ppx_inline_test.runtime-lib" ]
    | Some "ppx_typerep_conv"  -> [ "typerep" ]
    | _                        -> []
  end |> List.map ~f:(Libmap.resolve_string_exn libmap)

let eval_preprocess_style_libs libmap style : Lib_dep.t list =
  match style with
  | PP_style.Nothing | Command _ | Metaquot -> []
  | PP (pps, _flags, _style) ->
    List.concat_map pps ~f:(libs_for_code_generated_by_pp libmap)

let get_pp_libs ~mc : Lib_dep.t list =
  let {MC. dc; x_libs; pp_style; _} = mc in
  x_libs @ eval_preprocess_style_libs dc.libmap pp_style

(*----------------------------------------------------------------------
 pp deps
----------------------------------------------------------------------*)

let camlp4_pps pps =
  List.filter pps ~f:(fun pp ->
    not (String.is_prefix ~prefix:"ppx" (PP.to_string pp)))
;;

(* this expresses the dependencies of the commands produced by [get_camlp4_com] *)
let camlp4_deps_without_user_deps libmap (pps:PP.t list) : unit Dep.t list =
  let pps = camlp4_pps pps in
  Dep.path camlp4o_path_non_opt
  :: List.concat_map pps ~f:(fun pp ->
    match PP.to_string pp with
    | "pa_macro" -> []
    | _ -> LL.dep_on_ocaml_artifacts [Libmap.resolve_string_exn libmap (PP.to_string pp)]
             ~suffixes:[".cmo"])
;;

let ppx_deps_without_user_deps (pps:PP.t list) : unit Dep.t list =
  [Dep.path (ppx_executable pps)]

let get_pp_user_deps ~mc : unit Dep.t list =
  let {MC. dir; preprocessor_deps; _} = mc in
  Dep_conf_interpret.list_to_depends ~dir preprocessor_deps

let get_pp_deps ~mc : unit Dep.t list =
  let {MC. dc; pp_style; _} = mc in
  get_pp_user_deps ~mc @
    begin match pp_style with
    | PP_style.Nothing | Command _ -> []
    | Metaquot -> [Dep.path metaquot_exe]
    | PP (pps, _flags, Requires_camlp4)       -> camlp4_deps_without_user_deps dc.libmap pps
    | PP (pps, _flags, Switched_to_ppx_style) -> ppx_deps_without_user_deps pps
    end

(*----------------------------------------------------------------------
  pp com
----------------------------------------------------------------------*)

module Camlp4_com : Stringable = String

let get_camlp4_com ~dir ~libname ~can_setup_inline_runners ~flags kind (pps:PP.t list) =
  ignore (can_setup_inline_runners, libname);
  let pps = camlp4_pps pps in
  Camlp4_com.of_string
    (concat_quoted (
       reach_from ~dir camlp4o_path_non_opt ::
        List.concat_map pps ~f:(fun pp ->
          let path ~suf =
            reach_from ~dir (LL.path_to_ocaml_artifact ~lib_in_the_tree:(PP.to_libname pp) ~suf)
          in
          match PP.to_string pp with
          | "pa_macro" -> [ "pa_macro.cmo" ]
          | "pa_here" -> [ path ~suf:".cmo"; "-pa-here-dirname"; Path.to_string dir ]
          | _ -> [path ~suf:".cmo"]
        ) @ flags @ [ml_kind_to_flag kind]))

let get_ppx_command
    ~kind
    ~dir
    ~libname
    ~can_setup_inline_runners
    ~flags (pps:PP.t list)
    ~name
  : (string * string list) =
  let exe = ppx_executable pps in
  let prog = reach_from ~dir exe in
  let kind_flag =
    match kind with
    | None -> [] (* will be inferred from file extension *)
    | Some kind -> [ml_kind_to_flag kind]
  in
  let uses_inline_test = ref false in
  let uses_inline_bench = ref false in
  let uses_here = ref false in
  List.iter pps ~f:(fun pp ->
    match remap_pa_string (PP.to_string pp) with
    | Some ("ppx_here" | "ppx_assert") -> uses_here := true
    | Some "ppx_expect" -> uses_inline_test := true; uses_here := true
    | Some "ppx_inline_test" -> uses_inline_test := true
    | Some "ppx_bench" -> uses_inline_bench := true
    | _ -> ());
  let args =
    List.concat
      [ (if !uses_here
         then ["-dirname"; Path.to_string dir]
         else [])
      ; (if !uses_inline_test && drop_test
         then [ "-inline-test-drop-with-deadcode" ]
         else [])
      ; (if !uses_inline_bench && drop_bench
         then [ "-bench-drop-with-deadcode" ]
         else [])
      ; (if can_setup_inline_runners
         && (!uses_inline_test || !uses_inline_bench)
         then [ "-inline-test-lib"; LN.to_string libname ^ ":" ^ BN.to_string name ]
         else [])
      ; List.map flags ~f:remap_pa_flag
      ; kind_flag
      ]
  in
  prog, args


(* Preprocessing, with either [-pp] or [-ppx].

   A [-ppx] command should expect two additional arguments: name of the input file with
   marshalled AST and name of the output file.

   A [-pp] command should each expect one additional argument: name of the
   source file, and should write the preprocessed output to stdout.

   This command depends on [get_pp_deps ~mc ~name].
*)
let get_pp_com_args ~(kind:ml_kind) ~mc ~name : string list =
  let {MC. dir; libname; can_setup_inline_runners; pp_style; _} = mc in
  match pp_style with
  | PP_style.Nothing -> []
  | Metaquot -> ["-ppx"; reach_from ~dir metaquot_exe ]
  | Command string -> ["-pp"; string]
  | PP (pps, flags, code_style) ->
    match code_style with
    | Requires_camlp4 ->
      ["-pp"; Camlp4_com.to_string (get_camlp4_com ~dir ~libname ~flags ~can_setup_inline_runners kind pps)]
    | Switched_to_ppx_style ->
      let prog, args = get_ppx_command ~name ~kind:(Some kind) ~dir ~libname ~can_setup_inline_runners ~flags pps in
      let args =
        ["-dump-ast";
         "-loc-filename"; (BN.to_string name ^ ml_kind_to_suf kind)
        ] @ args
      in
      ["-pp"; concat_quoted (prog :: args)]

(*----------------------------------------------------------------------
 generate .pp
----------------------------------------------------------------------*)

let generate_pp_using_ppx mc ~kind ~name ~pps ~flags =
  let {MC. dir; libname; can_setup_inline_runners; _} = mc in
  let prog, args = get_ppx_command ~name ~kind:(Some kind) ~dir ~libname ~can_setup_inline_runners ~flags pps in
  let pp_deps = get_pp_user_deps ~mc @ ppx_deps_without_user_deps pps in
  let suf = ml_kind_to_suf kind in
  let source = BN.suffixed ~dir name (suf) in
  let deps = [Dep.path source] @ pp_deps in
  let target = BN.suffixed ~dir name (suf ^ ".pp") in
  let action =
    let args =
      args @ [
        "-loc-filename"; basename (BN.suffixed ~dir name suf);
        (ml_kind_to_flag kind); reach_from ~dir source;
        "-o"; reach_from ~dir target
      ]
    in
    Action.process ~dir prog args
  in [
    simple_rule ~deps ~targets:[target] ~action;
    Rule.alias (Alias.pp ~dir) [Dep.path target];
  ]

let generate_pp_using_camlp4 mc kind ~name ~pps ~flags =
  let {MC. dc; dir; libname; can_setup_inline_runners; _} = mc in
  let com = Camlp4_com.to_string (get_camlp4_com ~dir ~libname ~can_setup_inline_runners ~flags kind pps) in
  let pp_deps = get_pp_user_deps ~mc @ camlp4_deps_without_user_deps dc.libmap pps in
  let expander = relative ~dir:ppx_dir "validate/expand_camlp4/bin/expand_camlp4.exe" in
  let suf = ml_kind_to_suf kind in
  let source = BN.suffixed ~dir name suf in
  let target = BN.suffixed ~dir name (suf ^ ".pp") in
  [simple_rule
     ~deps:([Dep.path expander; Dep.path source] @ pp_deps)
     ~targets:[target]
     ~action:(bashf ~dir !"%{quote} %{quote} %{quote} > %{quote}"
                (reach_from ~dir expander)
                com
                (basename source)
                (basename target));
   Rule.alias (Alias.pp ~dir) [Dep.path target];
  ]

let generate_pp mc ~kind ~name =
  let {MC. pp_style; _} = mc in
  match pp_style with
  | PP_style.Nothing ->
    generate_pp_using_ppx mc ~kind ~name ~pps:[]
      ~flags:["-no-check"; "-no-optcomp"]

  | Command com ->
    generate_pp_using_ppx mc ~kind ~name ~pps:[]
      ~flags:["-no-check"; "-no-optcomp"; "-pp"; com]

  | Metaquot ->
    generate_pp_using_ppx mc ~kind ~name ~pps:[PP.of_string "ppx_metaquot"]
      ~flags:["-no-check"; "-no-optcomp"]

  | PP (pps, flags, Switched_to_ppx_style) ->
    generate_pp_using_ppx mc ~kind ~name ~pps ~flags

  | PP (pps, flags, Requires_camlp4) ->
    generate_pp_using_camlp4 mc kind ~name ~pps ~flags

(*----------------------------------------------------------------------
 ocamldep / .d files
----------------------------------------------------------------------*)

let gen_dependencies kind ~mc ~name ~actual_modules =
  let parse_ocamldep_output_exn ~filename s = match String.lsplit2 ~on:':' s with
    | Some (before_colon, res) when String.(=) before_colon filename ->
      words_of_string res
    | _ ->
      failwithf
        "ocamldep failed to produce dependency information for %S. The output was: %S."
        (BN.to_string name) s ()
  in
  let {MC. dir; _ } = mc in
  let pp_deps = get_pp_deps ~mc in
  let pp_args = get_pp_com_args ~kind ~mc ~name in
  let suf = ml_kind_to_suf kind in
  let source = BN.suffixed ~dir name suf in
  let action =
    let prog = ocamldep_path in
    let args =
      ["-modules"] @ pp_args @ [ml_kind_to_flag kind] @ [basename source]
    in
    Action.process ~dir prog args
  in
  Dep.action_stdout
    (Dep.all_unit (Dep.path source :: pp_deps) *>>| fun () -> action)
  *>>| fun output ->
  let is_actual_dep name' =
    name <> name' && Set.mem (force actual_modules) name' in
  let potential_dependencies =
    output
    |> parse_ocamldep_output_exn ~filename:(basename source)
    |> List.sort ~cmp:String.compare
    (* can depend on both a.ml and A.ml, depending on which one exists *)
    |> List.concat_map ~f:(fun x ->
      (* Consider both [x] and an uncapitalised version.
         Following call to [is_actual_dep] will choose the correct one. *)
      [x; String.uncapitalize x;]
    )
    |> List.map  ~f:BN.of_string
  in
  List.filter potential_dependencies ~f:is_actual_dep
;;

let gen_dfile kind ~disallowed_module_dep mc ~name ~actual_modules =
  let {MC. dir; _ } = mc in
  let suf = ml_kind_to_suf kind in
  let dsuf = suf ^ ".d" in
  let dfile = BN.suffixed ~dir name dsuf in
  let targets = [dfile] in
  Rule.create ~targets (
    gen_dependencies kind ~mc ~name ~actual_modules
    *>>| fun actual_dependencies ->
    match
      List.filter_map actual_dependencies ~f:(fun name ->
        Option.map (disallowed_module_dep name) ~f:(fun msg -> name, msg))
    with
    | (bad_bn, _) :: _ as errors ->
      let bad_module = BN.to_module bad_bn in
      let error_messages = List.map errors ~f:snd in
      remove_target_and_fail ~target:dfile ~filename_for_error:(BN.to_string name ^ suf)
        "this module apparently has forbidden dependencies: %s. If these are not \
         actual dependencies of your module, you can make ocamldep understand it by \
         qualifying module paths: for instance instead of %s.something, using \
         Library.Std.%s.something."
        (String.concat ~sep:", " error_messages) bad_module bad_module
    | [] ->

      Action.save (String.concat ~sep:" " (List.map actual_dependencies ~f:BN.to_string))
        ~target:dfile
  )
;;

(*----------------------------------------------------------------------
 ocaml compilation
----------------------------------------------------------------------*)

(** returns the additional compiler arguments to compile in the presence of
    the renaming module if any,
    and includes the corresponding dependencies into the dep list *)
let open_renaming deps mc =
  let {MC. dir; libname; wrapped; _ } = mc in
  if not wrapped
  then deps, []
  else begin
    let renaming_deps =
      List.map [".cmi";".cmx"] ~f:(fun suf ->
        Dep.path (suffixed ~dir (LN.to_string libname) suf))
    in
    let deps = renaming_deps @ deps in
    let args = [ "-open"; LN.to_module libname; "-open"; "No_direct_access_to_" ^ LN.to_string libname ] in
    deps, args
  end

let conditional = function
  | true -> fun x -> [x]
  | false -> fun _ -> []

let compile_mli mc ~name =
  let {MC. dc; dir; libname; wrapped; _ } = mc in
  let kind = MLI in
  let pp_libs = get_pp_libs ~mc in
  let pp_deps = get_pp_deps ~mc in
  let pp_args = get_pp_com_args ~kind ~mc ~name in
  let {DC. ocamlflags; ocamlcflags; _} = dc in
  let prefix_args = prefix_args ~wrapped ~libname ~name in
  let prefixed_name = PN.of_barename ~wrapped ~libname name in
  let flags = ocamlflags @ ocamlcflags in
  let mli = BN.suffixed ~dir name ".mli" in
  let cmi = PN.suffixed ~dir prefixed_name ".cmi" in
  let targets =
    List.concat
      [ [cmi]
      ; conditional (List.mem flags "-bin-annot")
          (PN.suffixed ~dir prefixed_name ".cmti")
      ]
  in
  Rule.create ~targets (
    get_inferred_1step_deps dc.libmap ~dir ~libname *>>= fun libs ->
    let libs = pp_libs @ libs in
    let libdeps = LL.dep_on_ocaml_artifacts libs ~suffixes:[".cmi"] in
    let deps =
      [Dep.path mli; local_dependencies `mli Ocaml_mode.byte dc ~wrapped ~libname name]
      @ pp_deps @ libdeps
    in
    let deps,open_renaming_args = open_renaming deps mc in
    Findlib.Query.result_and mc.findlib_include_flags (Dep.all_unit deps)
    *>>| fun (external_include_flags, ()) ->
    Action.process
      ~dir
      ocamlc_path
      (List.concat [
        flags;
        pp_args;
        external_include_flags;
        LL.include_flags ~dir libs;
        prefix_args;
        ["-no-alias-deps"];
        open_renaming_args;
        ["-o"; basename cmi];
        [ "-c"; "-intf"; basename mli]
      ])
  )

let remove_nodynlink =
  List.filter ~f:(fun x -> x <> "-nodynlink")

let native_compile_ml mc ~name =
  let {MC. dc; dir; libname; wrapped; exists_mli; must_be_sharable; _ } = mc in
  let kind = ML in
  let pp_libs = get_pp_libs ~mc in
  let pp_deps = get_pp_deps ~mc in
  let pp_args = get_pp_com_args ~kind ~mc ~name in
  let {DC. ocamlflags; ocamloptflags; _} = dc in
  let prefix_args = prefix_args ~wrapped ~libname ~name in
  let prefixed_name = PN.of_barename ~wrapped ~libname name in
  let ml = BN.suffixed ~dir name ".ml" in
  let o = PN.suffixed ~dir prefixed_name ".o" in
  let cmx = PN.suffixed ~dir prefixed_name ".cmx" in
  let cmi = PN.suffixed ~dir prefixed_name ".cmi" in
  let flags = ocamlflags @ ocamloptflags in
  let flags = if must_be_sharable then remove_nodynlink flags else flags in
  let targets =
    List.concat
      [ [o; cmx]
      ; conditional (not exists_mli) cmi
      ; conditional (List.mem flags "-bin-annot")
          (PN.suffixed ~dir prefixed_name ".cmt")
      ; conditional (List.mem flags "-S")
          (PN.suffixed ~dir prefixed_name ".s")
      ; conditional (List.mem flags "-dtypes" || List.mem flags "-annot")
          (PN.suffixed ~dir prefixed_name ".annot")
      ]
  in
  Rule.create ~targets (
    get_inferred_1step_deps dc.libmap ~dir ~libname *>>= fun libs ->
    let libs = Lib_dep.remove_dups_preserve_order (pp_libs @ libs) in
    let libdeps = LL.dep_on_ocaml_artifacts libs ~suffixes:cmi_maybe_cmx in
    let deps =
      [ Dep.path ml; local_dependencies `ml Ocaml_mode.native dc ~wrapped ~libname name ]
      @ pp_deps
      @ libdeps
    in
    let deps =
      if exists_mli
      then deps @ [ Dep.path cmi ]
      else deps in
    let deps,open_renaming_args = open_renaming deps mc in
    Findlib.Query.result_and mc.findlib_include_flags (Dep.all_unit deps)
    *>>| fun (external_include_flags, ()) ->
    Action.process
      ~dir
      ocamlopt_path
      (List.concat [
        flags;
        pp_args;
        external_include_flags;
        LL.include_flags ~dir libs;
        prefix_args;
        read_or_create_cmi (if exists_mli then `Read else `Create) ".ml";
        ["-no-alias-deps"];
        open_renaming_args;
        ["-o"; basename cmx];
        ["-c"; "-impl"; basename ml];
      ])
  )

let byte_compile_ml mc ~name =
  let {MC. dc; dir; libname; wrapped; _ } = mc in
  let kind = ML in
  let pp_libs = get_pp_libs ~mc in
  let pp_deps = get_pp_deps ~mc in
  let pp_args = get_pp_com_args ~kind ~mc ~name in
  let {DC. ocamlflags; ocamlcflags; _} = dc in
  let ocamlflags = List.filter ocamlflags ~f:(function "-bin-annot" -> false | _ -> true) in
  let ml = BN.suffixed ~dir name ".ml" in
  let prefixed_name = PN.of_barename ~wrapped ~libname name in
  let cmi = PN.suffixed ~dir prefixed_name ".cmi" in
  let cmo = PN.suffixed ~dir prefixed_name ".cmo" in
  (* We run the byte compiler with an extended "-o" prefix so the generated file names are
     different from the native compile. In particular we care about generated .cmi files,
     which are written by both native and byte compilers when there is no .mli file.
     After byte compilation, the .cmo is [mv]ed back to the original prefixed name. *)
  let prefix_args = prefix_args ~wrapped ~libname ~name in
  let targets =
    [ cmo ]
  in
  Rule.create ~targets (
    get_inferred_1step_deps dc.libmap ~dir ~libname *>>= fun libs ->
    let libs = Lib_dep.remove_dups_preserve_order (pp_libs @ libs) in
    let libdeps = LL.dep_on_ocaml_artifacts libs ~suffixes:[".cmi"] in
    let deps =
      [ Dep.path ml;
        local_dependencies `ml Ocaml_mode.byte dc ~wrapped ~libname name ]
      @ pp_deps @ libdeps
    in
    let deps = deps @ [Dep.path cmi] in
    let deps,open_renaming_args = open_renaming deps mc in
    Findlib.Query.result_and mc.findlib_include_flags (Dep.all_unit deps)
    *>>| fun (external_include_flags, ()) ->
    Action.process
      ~dir
      ocamlc_path
      (List.concat [
        ocamlflags; ocamlcflags;
        pp_args;
        external_include_flags;
        LL.include_flags ~dir libs;
        prefix_args;
        read_or_create_cmi `Read ".ml";
        ["-no-alias-deps"];
        open_renaming_args;
        ["-c"; "-impl"; basename ml];
      ])
  )

let js_compile_cmo mc ~js_of_ocaml ~name =
  let {MC. dir; libname; wrapped; _ } = mc in
  let prefixed_name = PN.of_barename ~wrapped ~libname name in
  let cmo = PN.suffixed ~dir prefixed_name ".cmo" in
  let js = PN.suffixed ~dir prefixed_name Js_of_ocaml.cmo_suf in
  Js_of_ocaml.rule
    ~build_info:None ~hg_version:None ~dir
    ~flags:js_of_ocaml.Js_of_ocaml_conf.flags
    ~js_files:(Dep.return [])
    ~src:cmo ~target:js

let infer_mli_auto mc ~name =
  let {MC. dc; dir; libname; wrapped; _ } = mc in
  let kind = ML in
  let pp_deps = get_pp_deps ~mc in
  let pp_args = get_pp_com_args ~kind ~mc ~name in
  let {DC. ocamlflags; _} = dc in
  let prefix_args = prefix_args ~wrapped ~libname ~name in
  let ml = BN.suffixed ~dir name ".ml" in
  let mli_auto = BN.suffixed ~dir name ".mli.auto" in
  Rule.create ~targets:[mli_auto] (
    get_inferred_1step_deps dc.libmap ~dir ~libname *>>= fun libs ->
    let libdeps = LL.dep_on_ocaml_artifacts libs ~suffixes:[".cmi"] in
    let deps =
      [Dep.path ml; local_dependencies `ml Ocaml_mode.byte dc ~wrapped ~libname name]
      @ pp_deps @ libdeps in
    let deps,open_renaming_args = open_renaming deps mc in
    Findlib.Query.result_and mc.findlib_include_flags (Dep.all_unit deps)
    *>>| fun (external_include_flags, ()) ->
    Bash.action ~dir [
      bash1 ocamlc_path (List.concat [
        ["-i"];
        ocamlflags;
        pp_args;
        external_include_flags;
        LL.include_flags ~dir libs;
        prefix_args;
        ["-no-alias-deps"];
        open_renaming_args;
        [ "-c"; "-impl"; basename ml];
      ])
        ~target:(basename mli_auto);
    ]
  )

(*----------------------------------------------------------------------
 rules for a directory of ml
----------------------------------------------------------------------*)

let mem_of_list l =
  let set = BN.Hash_set.of_list l in
  fun x -> Hash_set.mem set x

let setup_ml_compile_rules
      ?(disallowed_module_dep = fun _ -> None)
      ~js_of_ocaml
      dc ~dir ~libname ~wrapped ~for_executable ~can_setup_inline_runners
      ~preprocessor_deps ~preprocess_spec ~names_spec ~libraries_written_by_user
  =
  let default_pp = None in
  let names_spec_intfs, names_spec_impls, names_spec_modules =
    eval_names_spec ~dc names_spec
  in
  if not for_executable then
    begin
      List.iter names_spec_modules ~f:(fun name ->
        if wrapped
        then
          (if BN.is_lib ~libname name then
             failwithf
               "Library %S has a module with the same name %S in it. \
                This is not allowed for wrapped libraries."
               (LN.to_string libname)
               (BN.to_string name)
               ())
        else
          (if not (BN.is_lib ~libname name) then
             failwithf
               "Library %S has a module with a different name %S in it. \
                This is not allowed for non-wrapped libraries."
               (LN.to_string libname)
               (BN.to_string name)
               ()))
    end;
  let names_spec_has_impl = mem_of_list names_spec_impls in
  let names_spec_has_intf = mem_of_list names_spec_intfs in
  let lookup_pp = unstage (lookup_pp dc ~default_pp ~preprocess_spec) in
  let actual_modules = lazy (BN.Set.of_list (dc.impls @ dc.intfs)) in
  let findlib_include_flags =
    Findlib.include_flags ~dir (LN.to_string libname)
      (get_inferred_1step_deps dc.libmap ~dir ~libname)
  in
  let mc name : MC.t =
    {
      dc;
      dir;
      libname;
      can_setup_inline_runners;
      x_libs = [];
      pp_style = lookup_pp name;
      preprocessor_deps;
      exists_ml = names_spec_has_impl name;
      exists_mli = names_spec_has_intf name;
      wrapped;
      must_be_sharable = false;
      findlib_include_flags;
    }
  in
  let compilation_rules =
    List.concat_map names_spec_modules ~f:(fun name ->
      let { MC.exists_ml; exists_mli; _ } as mc = mc name in
      match exists_ml with
      | false ->
        if exists_mli
        then failposf ~pos:(dummy_position (BN.suffixed ~dir name ".mli"))
               "this .mli doesn't have a corresponding .ml" ()
        else failposf ~pos:(dummy_position (User_or_gen_config.source_file ~dir))
               !"there is neither .ml nor .mli for module %{BN}" name ()
      | true ->
        let disallowed_module_dep x =
          if names_spec_has_impl x || names_spec_has_intf x
          then disallowed_module_dep x
          else Some (sprintf !"%{BN} is not part of the library/executable spec" x)
        in
        let javascript_rule =
          match js_of_ocaml with
          | None -> []
          | Some js_of_ocaml -> [js_compile_cmo mc ~js_of_ocaml ~name]
        in
        List.concat [
          [gen_cmideps dc name];
          (if for_executable
           then [gen_objdeps ~dir name ~exists_ml; gen_moduledeps ~dc name]
           else []);
          (if exists_ml then List.concat [
             generate_pp mc ~kind:ML ~name;
             javascript_rule;
             [ gen_dfile ML ~disallowed_module_dep mc ~name ~actual_modules;
               byte_compile_ml mc ~name;
               native_compile_ml mc ~name;
               infer_mli_auto mc ~name];
           ]
           else []);
          (if exists_mli then List.concat [
             generate_pp mc ~kind:MLI ~name;
             [gen_dfile MLI ~disallowed_module_dep mc ~name ~actual_modules;
              compile_mli mc ~name;]
           ] else []);
        ]
    )
  in
  let unused_libs_rules = [
    Rule.alias (Alias.unused_libs ~dir) [
      Dep.path (LN.suffixed ~dir libname ".unused-libs");
    ];
    Rule.create ~targets:[LN.suffixed ~dir libname ".unused-libs"] (
      let libraries_written_by_user =
        List.filter_map libraries_written_by_user ~f:Lib_dep.to_lib_in_the_tree
        |> List.map ~f:Lib_in_the_tree.name
      in
      let actual_modules =
        lazy (BN.Set.of_list (List.map ~f:BN.of_libname libraries_written_by_user))
      in
      Dep.List.concat_map names_spec_modules ~f:(fun name ->
        let mc = mc name in
        Dep.List.concat_map [ mc.exists_ml, ML; mc.exists_mli, MLI ]
          ~f:(fun (exists, kind) ->
            if not exists
            then return []
            else
              gen_dependencies kind ~mc ~name ~actual_modules
              *>>| fun actual_dependencies ->
              List.map actual_dependencies ~f:(fun dep ->
                (LN.of_string (BN.to_string dep), BN.to_string name ^ ml_kind_to_suf kind)))
      )
      *>>| fun all_uses ->
      let all_uses = LN.Map.of_alist_multi all_uses in
      let libraries_written_by_user =
        List.sort ~cmp:LN.compare libraries_written_by_user
      in
      let bad, good =
        List.partition_map libraries_written_by_user ~f:(fun lib ->
          match Map.find all_uses lib with
          | None -> `Fst (sprintf !"%{LN}\n" lib)
          | Some used ->
            `Snd (sprintf !"# %{LN} used by %s\n" lib (String.concat ~sep:" " used))
        )
      in
      Action.save (String.concat (bad @ good))
        ~target:(LN.suffixed ~dir libname ".unused-libs")
    );
  ]
  in
  List.concat
    [ unused_libs_rules
    ; compilation_rules
    ; Findlib.Query.rules findlib_include_flags
    ]
;;

(*----------------------------------------------------------------------
 preprocessor_conf
----------------------------------------------------------------------*)

(* .cmx/.o -> .cmxs *)
let share_preprocessor dc ~dir (name : BN.t) : Rule.t =
  let {DC. ocamlflags; ocamloptflags; _} = dc in
  let file suf = BN.suffixed ~dir name suf in
  let cmx = file ".cmx" in
  let o = file ".o" in
  let cmxs = file ".cmxs" in
  let must_be_sharable = true in
  let flags = ocamlflags @ ocamloptflags in
  let flags = if must_be_sharable then remove_nodynlink flags else flags in
  simple_rule ~deps:[Dep.path cmx; Dep.path o] ~targets:[cmxs] ~action:(
    Action.process ~dir ocamlopt_path
      (flags @ ["-shared"; "-o"; basename cmxs; basename cmx])
  )

module Preprocessor_conf_interpret = struct

  include Preprocessor_conf

  let disabled_warnings t =
    Compiler_config.disabled_warnings @ t.extra_disabled_warnings

  let ocamlflags t =
    ["-I"; "+compiler-libs"] @
    Top.default_ocamlflags ~disabled_warnings:(disabled_warnings t)

  let extend_dc t dc =
    let ocamlflags = ocamlflags t in
    {dc with DC.
      ocamlflags;
    }

end

let preprocessor_rules dc ~dir conf =
  let wrapped = false in
  let dc = Preprocessor_conf_interpret.extend_dc conf dc in
  let libname = Preprocessor_conf.name conf in
  let can_setup_inline_runners = false in
  let preprocess_spec = Preprocessor_conf.preprocess conf in
  let default_pp = Some camlp4orf_path in
  let name = BN.of_libname libname in
  let pp_style = unstage (lookup_pp dc ~default_pp ~preprocess_spec) name in
  let {DC.impl_is_buildable; intf_is_buildable;_} = dc in
  let exists_ml = impl_is_buildable name in
  let exists_mli = intf_is_buildable name in
  let must_be_sharable = true in
  let mc = {MC.
    dc;
    dir;
    libname;
    can_setup_inline_runners;
    x_libs = [];
    pp_style;
    preprocessor_deps = [];
    exists_ml;
    exists_mli;
    wrapped;
    must_be_sharable;
    findlib_include_flags = Findlib.Query.dummy []
  }
  in
  assert (BN.is_lib name ~libname);
  let actual_modules = lazy (BN.Set.of_list (dc.impls @ dc.intfs)) in
  let default_rule =
    let suffixes = [".cmi";".cmo"] in
    Rule.default ~dir (
      List.map suffixes ~f:(fun suf ->
        Dep.path (BN.suffixed ~dir name suf))
    )
  in
  List.concat [
    [gen_interface_deps_from_objinfo dc ~dir ~wrapped ~libname
       ~libraries_written_by_user:(Libmap.resolve_libdep_names_exn dc.libmap conf.libraries)];
    [Lib_modules.rule ~dir ~libname Lib_modules.empty];
    [gen_cmideps dc name];
    [
      gen_dfile ML ~disallowed_module_dep:(fun _ -> None) mc ~name ~actual_modules;
      stub_names_rule ~stub_names:[] ~dir ~libname;
      byte_compile_ml mc ~name;
      native_compile_ml mc ~name ;
      share_preprocessor dc ~dir name;
      default_rule;
    ];
    generate_pp mc ~kind:ML ~name;
    [infer_mli_auto mc ~name];
    if exists_mli then ([
      gen_dfile MLI ~disallowed_module_dep:(fun _ -> None) mc ~name ~actual_modules;
      compile_mli mc ~name;
    ] @ generate_pp mc ~kind:MLI ~name)
    else [];
  ]

(*----------------------------------------------------------------------
 pack ordering
----------------------------------------------------------------------*)

module Ordering = struct
  let find_shortest_cycle_using_floyd_warshal
        ~dir (graph : (BN.t * BN.t list) list) : BN.t list =
    (* cycles (especially in core) are already confusing enough that there is no need to
       put unrelated modules in there *)
    let to_int, of_int =
      let h1 = Int.Table.create () in
      let h2 = BN.Table.create () in
      List.iteri graph ~f:(fun i (name, _) ->
        Hashtbl.add_exn h1 ~key:i ~data:name;
        Hashtbl.add_exn h2 ~key:name ~data:i;
      );
      (fun name ->
         match Hashtbl.find h2 name with
         | Some x -> x
         | None ->
           failwithf !"The library in %S doesn't contain the module %S \
                       but depends on it" dir (BN.to_string name) ()),
      (fun i -> Hashtbl.find_exn h1 i)
    in
    let n = List.length graph in
    let dist = Array.init n ~f:(fun _ -> Array.create ~len:n 100000) in
    let next = Array.init n ~f:(fun _ -> Array.create ~len:n None) in
    List.iter graph ~f:(fun (name, deps) ->
      List.iter deps ~f:(fun dep ->
        dist.(to_int name).(to_int dep) <- 1
      )
    );
    for k = 0 to n - 1 do
      for i = 0 to n - 1 do
        for j = 0 to n - 1 do
          if dist.(i).(k) + dist.(k).(j) < dist.(i).(j) then begin
            dist.(i).(j) <- dist.(i).(k) + dist.(k).(j);
            next.(i).(j) <- Some k
          end
        done;
      done;
    done;
    let min_index = ref (-1) in
    let min_len = ref 100000000 in
    for i = 0 to n - 1; do
      if dist.(i).(i) < !min_len then begin
        min_len := dist.(i).(i);
        min_index := i
      end
    done;
    let rec build_cycle acc i j =
      match next.(i).(j) with
      | None -> acc
      | Some k -> build_cycle (of_int k :: build_cycle acc i k) k j
    in
    build_cycle [of_int !min_index] !min_index !min_index

  let pack ~(module_and_dfiles : (BN.t*Path.t)list) ~target =
    let rec loop acc = function
      | [] -> return (List.rev acc)
      | (name, dfile) :: tl ->
        BN.file_words dfile *>>= fun words ->
        loop ((name, words) :: acc) tl
    in
    loop [] module_and_dfiles *>>| fun l ->
    let alist_from_module_to_pack_dep =
      (* regroup the dependencies from the mli.d and the ml.d together *)
      Map.to_alist (BN.Map.of_alist_fold l ~init:[] ~f:(@)) in
    let rec sort rev_acc alist =
      (* returns a deterministic list: the smallest permutation of the list of modules
         wrt to lexicographic comparison on the list of string that respects the
         dependencies *)
      if List.is_empty alist then List.rev rev_acc else begin
        match List.find_map alist ~f:(fun (mod_, deps) ->
          if List.is_empty deps then Some mod_ else None)
        with
        | None ->
          let target = Path.to_string target in
          let dir = Filename.dirname target in
          let base = Filename.basename target in
          let cycle = find_shortest_cycle_using_floyd_warshal ~dir alist in
          (* putting the output if whatever format will please the omake-mode *)
          printf "\n- build %s %s" dir base;
          printf "File \"jbuild\", line 1, characters 1-1:";
          printf "Error: dependency cycle: %s"
            (String.concat ~sep:" " (List.map cycle ~f:BN.to_string));
          printf "- exit %s %s" dir base;
          raise Exit
        | Some mod_ ->
          let alist = List.filter alist ~f:(fun (mod', _) -> BN.(<>) mod_ mod') in
          let alist = List.map alist ~f:(fun (mod1, deps) ->
            (mod1, List.filter deps ~f:(fun dep -> BN.(<>) mod_ dep))
          ) in
          let rev_acc = mod_ :: rev_acc in
          sort rev_acc alist
      end
    in
    sort [] alist_from_module_to_pack_dep

  let comparison ~wrapped ~dir ~libname =
    let order_file = pack_order_file ~dir ~libname in
    BN.file_words order_file *>>| fun ordered_list ->
    let ordered_list = List.map ordered_list ~f:(PN.of_barename ~wrapped ~libname) in
    let ordered_list = List.map ordered_list ~f:PN.to_string in
    let _, map =
      List.fold ~init:(0, String.Map.empty) ordered_list ~f:(fun (count, map) elt ->
        (count + 1, Map.add map ~key:elt ~data:count)
      ) in
    fun elt1 elt2 ->
      match Map.find map elt1 with
      | None ->
        failwithf "can't find %s in %s" elt1 (String.concat ~sep:" " ordered_list) ()
      | Some count1 ->
        match Map.find map elt2 with
        | None ->
          failwithf "can't find %s in %s" elt2 (String.concat ~sep:" " ordered_list) ()
        | Some count2 -> Int.compare count1 count2

  let sort ~wrapped ~dir ~libname unsorted_cmxs =
    comparison ~wrapped ~dir ~libname *>>| fun comparison ->
    List.sort unsorted_cmxs ~cmp:(fun cmx1 cmx2 ->
      let mod1 = Filename.chop_extension cmx1 in
      let mod2 = Filename.chop_extension cmx2 in
      comparison mod1 mod2)

end

(* rules that generate libname.pack-order, that contains the modules of the libraries
   sorted in topological order *)
let library_module_order ~dir ~impls ~intfs ~libname =
  let module_and_dfiles =
    List.map impls ~f:(fun impl ->
      impl,
      BN.suffixed ~dir impl ".ml.d"
    )
    @ List.map intfs ~f:(fun intf ->
      intf,
      BN.suffixed ~dir intf ".mli.d"
    )
  in
  let target = pack_order_file ~dir ~libname in
  Rule.create ~targets:[target] (
    Ordering.pack ~module_and_dfiles ~target *>>| fun sorted_modules ->
    write_string_action (String.concat ~sep:" " (List.map sorted_modules ~f:BN.to_string))
      ~target
  )

(*----------------------------------------------------------------------
 building ocaml libraries - packing/arching
----------------------------------------------------------------------*)

let ocaml_library_archive dc ~dir ~impls ~wrapped ~libname ~flags =
  let {DC. ocamlflags; ocamlcflags; ocamloptflags; _} = dc in
  assert (not wrapped ==> Int.(List.length impls = 0));
  let impls = List.map impls ~f:(PN.of_barename ~wrapped ~libname) in
  let impls = List.map impls ~f:PN.to_string in
  List.concat_map [ [], LN.to_string libname
                  ; [ "-linkall" ], LN.to_string libname ^ ".linkall"
                  ]
    ~f:(fun (linkall, cmxa_without_suf) ->
      List.map
        [ ocamlc_path, ocamlcflags, ".cma", [], ".cmo", []
        ; ocamlopt_path, ocamloptflags, ".cmxa", [".a"], ".cmx", [".o"]
        ] ~f:(fun (ocamlcomp, ocamlcompflags, lib, lib_implicit, mod_, mod_implicit) ->
          let targets = List.concat_cartesian_product [cmxa_without_suf] (lib :: lib_implicit) in
          let deps =
            let impls = impls @ [LN.to_string libname] in
            List.concat_cartesian_product impls (mod_ :: mod_implicit)
          in
          let unsorted_mod_args = List.concat_cartesian_product impls [mod_] in
          relative_rule ~dir ~targets ~deps ~non_relative_deps:[] (
            (begin match unsorted_mod_args with
             | [] | [_] -> return unsorted_mod_args
             | _ :: _ :: _ -> Ordering.sort ~wrapped ~dir ~libname unsorted_mod_args
             end *>>| fun sorted_mod_args ->
             let sorted_mod_args = sorted_mod_args @ [LN.to_string libname ^ mod_] in
             Action.process ~dir ocamlcomp
               (ocamlflags
                @ ocamlcompflags
                @ sorted_mod_args
                @ flags
                @ linkall
                @ ["-a"; "-o"; cmxa_without_suf ^ lib ])))))
;;

(*----------------------------------------------------------------------
 hg_version
----------------------------------------------------------------------*)

(* Given that version_util is made available in core, there is little point in adding
   version util (same thing for build_info) to executables without access to it. This way
   we avoid the preprocessor executables constantly changing and causing jenga to do
   spurious work. *)
let hg_version_required_by libmap = Libmap.resolve_string_exn libmap "core"

let hg_dirstate_suffix =
  Dep.deferred (fun () ->
    let open Async.Std in
    run_action_now_stdout (
      Action.process ~ignore_stderr:true ~dir:Path.the_root
        hg_prog ["showconfig"; "jhg.omake-dirstate-suffix";]
    ) >>| String.strip)

let hg_version_out = root_relative "hg_version.out"

let hg_version_out_rule =
  Rule.create ~targets:[hg_version_out] (
    begin
      Dep.getenv version_util_support *>>= function
      | false -> return "NO_VERSION_UTIL"
      | true ->
        Dep.both hg_dirstate_suffix all_the_repos
        *>>= fun (dirstate_suffix, repos) ->
        Dep.all (List.map repos ~f:(fun repo ->
          Dep.action_stdout (
            Dep.path (Path.relative ~dir:repo (".hg/dirstate" ^ dirstate_suffix))
            *>>| fun () ->
            bash ~ignore_stderr:true ~dir:repo
              "echo -n \"$(hg showconfig 'paths.default')_$(hg id -i)\""
          )
        ))
        *>>| String.concat ~sep:"\n"
    end
    *>>| write_string_action ~target:hg_version_out)

let hg_version_base ~base = base ^ ".hg_version"

let generate_static_string_c_code_sh =
  relative ~dir:Config.script_dir "generate_static_string_c_code.sh"

let hg_version_rules ~dir ~exe =
  let base = hg_version_base ~base:exe in
  let c = relative ~dir (base ^ ".c") in
  let o = relative ~dir (base ^ ".o") in
  let o_rule =
    simple_rule ~targets:[o] ~deps:[Dep.path c]
      ~action:(
        Action.process ~dir ocamlc_path [basename c; "-o"; basename o;]
      )
  in
  let c_rule =
    simple_rule ~targets:[c]
      ~deps:[Dep.path generate_static_string_c_code_sh; Dep.path hg_version_out]
      ~action:(
        bashf ~dir !"%{quote} generated_hg_version < %{quote} > %{quote}"
          (reach_from ~dir generate_static_string_c_code_sh)
          (reach_from ~dir hg_version_out)
          (basename c))
  in
  [c_rule; o_rule]

(*----------------------------------------------------------------------
 build_info
----------------------------------------------------------------------*)

let build_info_required_by libmap = Libmap.resolve_string_exn libmap "core"

let build_info_base ~base = base ^ ".build_info"

let build_info_sh = relative ~dir:Config.script_dir "build_info.sh"

let link_deps_of_forced_lib (module Mode : Ocaml_mode.S) ~dir libname =
  (* Not going through liblinks, because this is only when linking a library from the
     current directory. Also we can't go through liblink_default, otherwise we create
     dependency cycles with TRANSITIVE_RUNNERS. *)
  let linkall_cmxa = LN.to_string libname ^ ".linkall" in
  List.map Mode.cmxa_and_a ~f:(fun suf ->
    Dep.path (relative ~dir (linkall_cmxa ^ suf)))
  @ [stubs_dependencies ~dir ~libname]
;;

let link_deps_of_libs mode ~dir ~libs_maybe_forced ~force_link ~build_libs_DEFAULT =
  (* Both the rules which generate the .exe and which generate the .build_info
     depend on these link_deps *)
  let unforced ~libs =
    link_deps_of_unforced_libs mode ~libs
    @ (if build_libs_DEFAULT then LL.dep_on_default_alias libs else [])
  in
  (match force_link with
  | None -> unforced ~libs:libs_maybe_forced
  | Some lib ->
    unforced ~libs:(List.filter libs_maybe_forced ~f:((<>) (Lib_dep.In_the_tree lib)))
    @ link_deps_of_forced_lib mode ~dir lib.name)
;;

let link_deps_of_version_util dc (module Mode : Ocaml_mode.S) ~dir ~suppress_version_util ~libs exe =
  if not suppress_version_util && List.mem libs (hg_version_required_by dc) then
    [ Dep.path (relative ~dir
                  (hg_version_base ~base:(exe ^ Mode.exe) ^ ".o"))
    ]
  else
    []
;;

let link_deps_of_objs (module Mode : Ocaml_mode.S) objs =
  List.concat_map objs ~f:(fun (dir, base) ->
    List.map Mode.cmx_and_o ~f:(fun suf ->
      Dep.path (Path.relative ~dir (base ^ suf))))
;;

let build_info_rules ~dir ~exe ~suf ~sexp_dep =
  let base = build_info_base ~base:(exe ^ suf) in
  let sexp_file = relative ~dir (base ^ ".sexp") in
  let o = relative ~dir (base ^ ".o") in
  let o_rule =
    (* The .c is not a separate rule, so dependencies on *.c (like in
       external/lacaml/lib) do not depend spuriously on this C file for
       the inline benchmarks, creating a dependency cycle. *)
    simple_rule ~targets:[o]
      ~deps:[Dep.path generate_static_string_c_code_sh; Dep.path sexp_file]
      ~action:(
        bashf ~dir !"tmp=$(mktemp --tmpdir build_infoXXXXXX.c) && \
                     %{quote} generated_build_info < %{quote} > \"$tmp\" && \
                     %{quote} \"$tmp\" && \
                     mv \"$(basename \"$tmp\" .c)\".o %{quote}"
          (reach_from ~dir generate_static_string_c_code_sh)
          (basename sexp_file)
          ocamlc_path
          (basename o))
  in
  let sexp_rule =
    Rule.create ~targets:[sexp_file] (
      let build_info () =
        let application_specific_fields =
          Option.value build_info_app_fields ~default:String.Map.empty
          |> Map.add ~key:"use_new_sql" ~data:(Bool.sexp_of_t use_new_sqml)
        in
        [%sexp
          { x_library_inlining = (x_library_inlining : bool);
            ocaml_version = (Compiler_selection.compiler_dir : string);
            executable_path = (suffixed ~dir exe suf : Path.t);
            build_system = "jenga";
            dynlinkable_code = (dynlinkable_code : bool);
            application_specific_fields =
              (application_specific_fields : Sexp.t String.Map.t);
          }
        ]
      in
      match stable_build_info with
      | true ->
        return () *>>| fun () ->
        Action.save ~target:sexp_file (Sexp.to_string_mach (build_info ()))
      | false ->
        Dep.all_unit [ sexp_dep; Dep.path build_info_sh ] *>>| fun () ->
        let build_info =
          match build_info () with
          | Sexp.Atom _ -> assert false
          | Sexp.List l -> String.concat ~sep:" " (List.map l ~f:Sexp.to_string)
        in
        bashf ~dir !"%{quote} %{quote} > %{quote}"
          (reach_from ~dir build_info_sh) build_info (basename sexp_file)
    )
  in
  [sexp_rule; o_rule]

(*----------------------------------------------------------------------
  Rules that check if libraries define tests or benchs
----------------------------------------------------------------------*)
let fgrep_rule ~dir ~filename ~macros ~impls =
  assert (not (List.is_empty macros));
  let target = relative ~dir filename in
  let sources = List.map impls ~f:(fun impl -> BN.suffixed ~dir impl ".ml") in
  simple_rule
    ~targets:[target]
    ~deps:(List.map sources ~f:Dep.path)
    ~action:(
      match sources with
      | [] ->
        bashf ~dir !"echo -n > %{quote}" (basename target)
      | _ :: _ ->
        bashf ~dir !"{ fgrep -l -w%s %{concat_quoted} || true; } > %{quote}"
          (String.concat (List.map macros ~f:(sprintf !" -e %{quote}")))
          (List.map ~f:basename sources)
          (basename target)
    )

let fgrep_inline_test_filename = "fgrep_inline_tests.out"
let letp = "let%" (* this is so the grep doesn't think there are tests or benches in the
                     jengaroot *)
let inline_test_macros = [letp ^ "test"; letp ^ "test_unit"; letp ^ "test_module"]

let fgrep_expect_test_filename = "fgrep_expect_tests.out"
let expect_test_macros = [letp ^ "expect_test" ]

let fgrep_bench_filename = "fgrep_bench.out"
let bench_macros = [letp ^ "bench"; letp ^ "bench_fun"; letp ^ "bench_module"]

(*----------------------------------------------------------------------
 check_ldd_deps
----------------------------------------------------------------------*)

module Check_ldd_dependencies : sig

  val check :
    allowed:Ordered_set_lang.t
    -> target:Path.t -> (Path.t -> Action.t Dep.t) -> Rule.t list

end = struct

  let atomic_copy_action ~source ~target =
    let dir = dirname target in
    bashf ~dir
      !"tmp=\"$(mktemp --tmpdir=./)\"; cp -p -- %{quote} \"$tmp\"; mv -- \"$tmp\" %{quote}"
      (reach_from ~dir source)
      (basename target)

  (* This list contains dynamic libraries that we know are present on all prod boxes. It
     is useful because if you restrict yourself to this standard set of dynamic
     dependencies, your executable should run on all prod machines.
     So if you want to extend this list, check with the sysadmins whether the new library
     you want satisfies this condition. *)
  let standard = [
    "libc.so.6";
    "libcares.so.2";
    "libdl.so.2";
    "libgcc_s.so.1";
    "libgmp.so.3";
    "libjswrap.so.1";
    "libm.so.6";
    "libnsl.so.1";
    "libpcre.so.0";
    "libpthread.so.0";
    "librt.so.1";
    "libstdc++.so.6";
    "libtevent.so.0";
    "libtinfo.so.5";
    "libz.so.1";
    "linux-vdso.so.1";
  ]

  let dynamic_lib_deps_sh = relative ~dir:Config.script_dir "dynamic-lib-deps.sh"

  let check ~allowed ~target mk_action =
    let allowed = Ordered_set_lang.eval_with_standard (Some allowed) ~standard in
    let is_unexpected =
      let set = String.Hash_set.of_list allowed in
      fun name -> not (Hash_set.mem set name)
    in
    let dir = dirname target in
    let unchecked = suffixed ~dir (basename target) ".unchecked-do-not-deploy" in
    [
      Rule.create ~targets:[unchecked] (mk_action unchecked);
      Rule.create ~targets:[target] (
        (* The unchecked .exe is a dependency for the checking-action AND the cp/rm *)
        Dep.path unchecked *>>= fun () ->
        Dep.action_stdout (
          Dep.all_unit [Dep.path dynamic_lib_deps_sh; Dep.path unchecked] *>>| fun () ->
          Action.process ~dir:Path.the_root
            (Path.to_string dynamic_lib_deps_sh)
            [Path.to_string unchecked]
        ) *>>| fun stdout ->
        let actual = words_of_string stdout in
        let unexpected = List.filter ~f:is_unexpected actual in
        match unexpected with
        | [] ->
          atomic_copy_action ~source:unchecked ~target
        | _ :: _ ->
          printf "Removing executable with unexpected ldd deps: %s" (Path.to_string target);
          printf "Allowed   : %s" (String.concat ~sep:" " allowed);
          printf "Actual    : %s" (String.concat ~sep:" " actual);
          remove_target_and_fail ~target ~filename_for_error:"jbuild"
            "Unexpected dynamic dependencies: %s"
            (String.concat ~sep:" " unexpected)
      );
    ]

end


(*----------------------------------------------------------------------
 link_native
----------------------------------------------------------------------*)

let ocaml_plugin_handling dc ~dir name =
  let ocaml_plugin_o = BN.to_string name ^ ".ocaml_plugin.o" in
  match dc.DC.ocaml_plugin_libraries (BN.to_string name) with
  | None -> `Not_a_plugin
  | Some libs_for_plugins ->
    (* This linkall flag, together with the libs for plugins we add, means we link in the
       transitive implementations of the embedded library signatures, which is enough to
       guarantee that any code that types using these signatures will be loadable. *)
    `Plugin
      (libs_for_plugins,
       Dep.path (relative ~dir ocaml_plugin_o),
       ["-linkall"; ocaml_plugin_o])

let get_libs_for_exe libmap ~link_libdeps_of ~libs_for_plugins ~force_link =
  Dep.both
    (Dep.all (List.map link_libdeps_of ~f:(fun (dir, libname) ->
       Libmap.load_lib_deps libmap (LN.suffixed ~dir libname ".libdeps"))))
    (libs_transitive_closure libmap libs_for_plugins)
  *>>| fun (normal_libss, libs_for_plugins) ->
  Lib_dep.remove_dups_preserve_order (List.concat normal_libss @ libs_for_plugins @
                                      List.map (Option.to_list force_link)
                                        ~f:Lib_dep.of_lib_in_the_tree)
;;


module Js_of_ocaml_jsdeps = struct

  let rule ~dir libname files =
    let target = LN.suffixed ~dir libname Js_of_ocaml.jsdeps_suf in
    let files = List.map files ~f:Path.to_string in
    write_string_rule ~target (String.concat ~sep:" " files)

  let all_deps_for_libs (libs : Lib_dep.t list) : Path.t list Dep.t =
    Dep.List.concat_map libs ~f:(function
      | From_compiler_distribution _
      | Findlib_package _ -> Dep.return []
      | In_the_tree lib ->
        let path =
          LL.path_to_ocaml_artifact ~lib_in_the_tree:lib.name ~suf:Js_of_ocaml.jsdeps_suf
        in
        file_words path *>>| List.map ~f:Path.root_relative
    )
end

let link (module Mode : Ocaml_mode.S) (dc : DC.t) ~dir
      ~more_deps
      ~link_flags
      ~allowed_ldd_dependencies
      ~ocaml_plugin_handling
      ~link_libdeps_of
      ~suppress_build_info
      ~suppress_version_util
      ~build_libs_DEFAULT
      ~compute_objs
      ~exe
      ~force_link
      ~test_or_bench
      ~js_of_ocaml =
  let ocamlopt_path, ocamloptflags =
    match Mode.which with
    | `Byte -> ocamlc_path, dc.ocamlcflags
    | `Native -> ocamlopt_path, dc.ocamloptflags
  in
  let hg_version_o = relative ~dir (hg_version_base ~base:(exe ^ Mode.exe) ^ ".o") in
  let build_info_o = relative ~dir (build_info_base ~base:(exe ^ Mode.exe) ^ ".o") in

  let maybe_ldd_check_rules ~target mk_action =
    match allowed_ldd_dependencies with
    | None -> [Rule.create ~targets:[target] (mk_action target)] (* no check *)
    | Some allowed -> Check_ldd_dependencies.check ~allowed ~target mk_action
  in

  let libs_for_plugins, ocaml_plugin_deps, ocaml_plugin_flags =
    match ocaml_plugin_handling with
    | `Not_a_plugin -> [], [], []
    | `Plugin (libs_for_plugins, flags_dep, flags) ->
      libs_for_plugins, [flags_dep], flags
  in

  let libs_maybe_forced_dep =
    link_libdeps_of
    *>>= fun link_libdeps_of ->
    get_libs_for_exe dc.libmap ~link_libdeps_of ~libs_for_plugins ~force_link
  in

  let findlib_archives =
    Findlib.archives (module Mode) ~dir ~exe libs_maybe_forced_dep
  in
  let findlib_archives_full_path =
    Findlib.archives_full_path (module Mode) ~dir ~exe libs_maybe_forced_dep
  in
  let findlib_include_flags =
    Findlib.include_flags ~dir (exe ^ Mode.exe) libs_maybe_forced_dep
  in

  let exe_rules =
    let target = suffixed ~dir exe Mode.exe in
    maybe_ldd_check_rules ~target (fun target ->
      let exe_maybe_tmp = basename target in
      libs_maybe_forced_dep
      *>>= fun libs_maybe_forced ->
      compute_objs *>>= fun objs ->
      let build_info_dep, build_info_args =
        if not suppress_build_info &&
           List.mem libs_maybe_forced (build_info_required_by dc.libmap)
        then [Dep.path build_info_o],
             [ basename build_info_o ]
        else [], []
      in
      let version_util_args =
        if not suppress_version_util &&
           List.mem libs_maybe_forced (hg_version_required_by dc.libmap)
        then [ basename hg_version_o ]
        else []
      in
      let deps =
        link_deps_of_libs (module Mode) ~dir ~libs_maybe_forced ~force_link
           ~build_libs_DEFAULT
        @ link_deps_of_version_util dc.libmap (module Mode) ~dir ~suppress_version_util
            ~libs:libs_maybe_forced exe
        @ link_deps_of_objs (module Mode) objs
        @ build_info_dep
        @ [Dep.path link_quietly]
        @ ocaml_plugin_deps
        @ more_deps
      in
      Findlib.Query.result_and findlib_include_flags
        (Findlib.Query.result_and findlib_archives
           (Dep.all_unit deps))
      *>>| fun (external_include_flags, (external_archives, ())) ->
      let sub_cmxs_in_correct_order =
        List.map objs ~f:(fun (obj_dir, base) ->
          Path.reach_from ~dir (Path.relative ~dir:obj_dir (base ^ Mode.cmx))) in
      let lib_cmxas =
        List.concat_map libs_maybe_forced ~f:(fun lib_dep ->
          match lib_dep, force_link with
          | In_the_tree lib, Some lib' when LN.equal lib.name lib'.name ->
            [ "-I"; "."; LN.to_string lib.name ^ ".linkall" ^ Mode.cmxa]
          | _ ->
            LL.link_flags ~dir lib_dep ~cmxa:Mode.cmxa)
      in
      let link_flags =
        if String.is_suffix Mode.exe ~suffix:".so"
        || String.is_suffix Mode.exe ~suffix:".o"
        then
          [ "-ccopt"; "-ldl"
          ; "-ccopt"; "-lm"
          ; "-ccopt"; "--shared"
          ; "-ccopt"; "-fPIC"
          ; "-runtime-variant"; "_pic"
          ; "-output-complete-obj"
          ] @ link_flags
        else link_flags
      in
      Action.process ~dir
        (reach_from ~dir link_quietly)
        (List.concat [
          [ocamlopt_path];
          dc.ocamlflags; ocamloptflags;
          dc.link_flags;
          (* The assumption is that code in the tree depends on external packages but not
             the opposite, so external packages must come first *)
          external_include_flags;
          external_archives;
          link_flags;
          (* Using g++ instead of gcc to link in the c++ stdlib if needed. *)
          use_compiler_flavor `Cxx;
          (* We would use --as-needed in bytecode as well, but it doesn't work because
             the configure script doesn't properly detect which libraries define what
             symbols (http://caml.inria.fr/mantis/view.php?id=7164) *)
          (match Mode.which with
           | `Native -> ["-ccopt"; "-Wl,--as-needed"]
           | `Byte -> ["-custom"]);
          build_info_args;
          version_util_args;
          ocaml_plugin_flags;
          lib_cmxas;
          sub_cmxs_in_correct_order;
          ["-o"; exe_maybe_tmp];
        ])
    )
  in

  let js_of_ocaml_rules =
    match Mode.which, js_of_ocaml with
    | `Byte, Some { Js_of_ocaml_conf.flags; javascript_files } when Compiler_selection.m32 ->
      let target_js = suffixed ~dir exe Js_of_ocaml.exe_suf in
      let javascript_files = List.map ~f:(Path.relative_or_absolute ~dir) javascript_files in
      let js_files : Path.t list Dep.t =
        libs_maybe_forced_dep *>>= fun libs_maybe_forced ->
        Js_of_ocaml_jsdeps.all_deps_for_libs libs_maybe_forced
        *>>| fun js_files_from_libs ->
        js_files_from_libs @ javascript_files
      in
      let hg_version =
        if suppress_version_util
        then None
        else Some hg_version_out
      in
      let build_info =
        if suppress_build_info || for_javascript_development
        then None
        else Some (relative ~dir (build_info_base ~base:(exe ^ Mode.exe) ^ ".sexp"))
      in
      let flags =
        if test_or_bench
        then flags
        else [ "--setenv"; "FORCE_DROP_INLINE_TEST=true"
             ; "--setenv"; "FORCE_DROP_BENCH=true" ]
             @ flags
      in
      let runtime_js = suffixed ~dir exe Js_of_ocaml.runtime_suf in
      let bc_dot_js_rule =
        if not for_javascript_development
        then begin
          let bytecode_exe = suffixed ~dir exe Mode.exe in
          Js_of_ocaml.rule ~build_info ~hg_version ~dir ~flags ~js_files ~src:bytecode_exe ~target:target_js
        end else begin
          Rule.create ~targets:[target_js] (
            let findlib_archives =
              Findlib.archives_full_path (module Mode) ~dir ~exe libs_maybe_forced_dep
            in
            libs_maybe_forced_dep
            *>>= fun libs_maybe_forced ->
            compute_objs
            *>>= fun objs ->
            js_files *>>= fun js_files ->
            Findlib.Query.result findlib_archives
            *>>= fun archives_list ->
            Js_of_ocaml.from_external_archives
              ~ocaml_where:ocaml_where_path
              (List.map ~f:Path.absolute archives_list)
            *>>= fun js_archives_list ->
            let sub_cmos_in_correct_order =
              List.map objs ~f:(fun (obj_dir, base) ->
                Path.relative ~dir:obj_dir (base ^ Js_of_ocaml.cmo_suf)) in
            let libs_cma_js =
              Js_of_ocaml.stdlib_from_compiler_distribution ::
              List.concat_map libs_maybe_forced ~f:(fun lib_dep ->
                match lib_dep with
                | In_the_tree lib ->
                  [ LL.path_to_ocaml_artifact ~lib_in_the_tree:lib.name ~suf:Js_of_ocaml.cma_suf ]
                | From_compiler_distribution dst ->
                  [ Js_of_ocaml.from_compiler_distribution dst ]
                | Findlib_package _pkg -> []
              )
            in
            let all_files =
              List.concat
                [ [ runtime_js ]
                ; js_files
                ; js_archives_list
                ; libs_cma_js
                ; sub_cmos_in_correct_order
                ]
            in
            Js_of_ocaml.link_js_files ~dir ~files:all_files ~target:target_js
          )
        end
      in
      [ bc_dot_js_rule
      ; Js_of_ocaml.rule_for_standalone_runtime ~build_info ~hg_version ~dir ~flags ~js_files ~target:runtime_js ]
    | (`Byte | `Native), _ -> []
  in

  let sexp_dep =
    link_libdeps_of
    *>>= fun link_libdeps_of ->
    get_libs_for_exe dc.libmap ~link_libdeps_of ~libs_for_plugins ~force_link
    *>>= fun libs_maybe_forced ->
    compute_objs *>>= fun objs ->
    Dep.all_unit (List.concat [
      link_deps_of_libs (module Mode) ~dir ~libs_maybe_forced ~force_link
        ~build_libs_DEFAULT;
      link_deps_of_version_util dc.libmap (module Mode) ~dir ~suppress_version_util
        ~libs:libs_maybe_forced exe;
      link_deps_of_objs (module Mode) objs;
      [Dep.path build_info_sh];
      ocaml_plugin_deps;
    ])
  in
  List.concat [
    exe_rules;
    js_of_ocaml_rules;
    List.concat_map ~f:Findlib.Query.rules
      [ findlib_archives
      ; findlib_include_flags
      ; findlib_archives_full_path ];
    (if suppress_version_util
     then []
     else hg_version_rules ~dir ~exe:(exe ^ Mode.exe));
    (if suppress_build_info
     then []
     else build_info_rules ~dir ~exe ~suf:Mode.exe ~sexp_dep);
  ]

(*----------------------------------------------------------------------
 executable_rules
----------------------------------------------------------------------*)

module Executables_conf_interpret = struct

  include Executables_conf

  let disabled_warnings t =
    Compiler_config.disabled_warnings @ t.extra_disabled_warnings

  let ocamlflags t =
    let ocamlflags = Top.default_ocamlflags ~disabled_warnings:(disabled_warnings t) in
    Ordered_set_lang.eval_with_standard t.flags ~standard:ocamlflags

  let ocamlcflags t =
    Ordered_set_lang.eval_with_standard t.ocamlc_flags ~standard:Top.ocamlcflags

  let ocamloptflags t =
    Ordered_set_lang.eval_with_standard t.ocamlopt_flags ~standard:Top.ocamloptflags

  let extend_dc t dc =
    let ocamlflags = ocamlflags t in
    let ocamlcflags = ocamlcflags t in
    let ocamloptflags = ocamloptflags t in
    {dc with DC.
      ocamlflags;
      ocamlcflags;
      ocamloptflags;
    }

  let libraries (t : t) = t.libraries

end

let bin_prefix =
  let wrapped = wrapped_bindirs in
  if not wrapped
  then "bin-" (* not critical, but reduces diff from current packing rules *)
  else
    (* Must be a valid OCaml identifier prefix.
       We use "__" here as well because it's unlikely to occur in
       a legitimate library name  *)
    "bin__"

let fake_libname_of_exes names =
  match names with
  | [] -> failwith "executable declarations with no executables aren't allowed"
  | first_name :: _ -> LN.of_string (bin_prefix ^ first_name)

let objdeps ~dir name =
  BN.file_words (BN.suffixed ~dir name ".objdeps")
;;

let exe_artifact_in_std_aliases ~only_shared_object =
  match only_shared_object, dynlinkable_code with
  | true, false -> ".cmx", `Cmx
  | true, true  -> ".so", `So
  | false, _    -> ".exe", `Exe
;;

let replace_exe_suffix mode ~only_shared_object : Ocaml_mode.t =
  if only_shared_object
  then
    (module struct
      include (val mode : Ocaml_mode.S)
      let exe =
        match which with
        | `Byte -> ".bc.so"
        | `Native -> ".so"
    end)
  else mode
;;

let link_executable (module Mode : Ocaml_mode.S) (dc : DC.t) ~dir ~wrapped ~libname
      ~link_flags ~projections_check ~allowed_ldd_dependencies ~js_of_ocaml
      ~only_shared_object name =
  let (module Mode : Ocaml_mode.S) =
    replace_exe_suffix (module Mode) ~only_shared_object
  in
  let link_rules =
    link (module Mode) (dc : DC.t) ~dir
        ~more_deps:[]
        ~link_flags
        ~build_libs_DEFAULT:true
        ~allowed_ldd_dependencies
        ~ocaml_plugin_handling:(ocaml_plugin_handling dc ~dir name)
        ~suppress_build_info:false
        ~suppress_version_util:false
        ~link_libdeps_of:(return [dir, libname])
        ~compute_objs:
          (objdeps ~dir name
           *>>| fun bns ->
           List.map (bns @ [name]) ~f:(fun name ->
             dir, PN.to_string (PN.of_barename ~wrapped ~libname name)))
        ~exe:(BN.to_string name)
        ~force_link:None
        ~test_or_bench:false
        ~js_of_ocaml
  in
  let projections_check_rules =
    (* We only care about native code, since that's how we roll production executables.
       It shouldn't matter now, but if we start needing such checks on executable_conf
       items that define several executables, we could have a single rule that checks the
       dependencies of all of them at the same time. *)
    match projections_check, Mode.which with
    | None, _ | _, `Byte -> []
    | Some { Jbuild_types.Projections_check.allow; output_result_to }, `Native ->
      let exe = BN.suffixed ~dir name Mode.exe in
      let rule =
        match output_result_to with
        | Some file ->
          Fe.Projections_check.rule_for_testing
            ~target:(relative ~dir file)
            ~exe
            ~allowed_projections:allow
        | None ->
          (* This goes in a .runtest alias because jenga takes ~4s to run the [Dep.t] at
             the end of any build that runs it, which would slow down people working on
             such executable for no good reason, given that these is not a likely failure,
             and hydra will catch it anyway since it always runs .runtest. *)
          Rule.alias (Alias.runtest ~dir) [
            (Fe.Projections_check.error_msg_dep ~dir ~exe ~allowed_projections:allow
             *>>| Option.iter ~f:(fun error_msg ->
               let source = User_or_gen_config.source_file ~dir in
               failposf ~pos:(dummy_position source) "%s" error_msg ()));
          ]
      in
      [rule]
  in
  link_rules @ projections_check_rules
;;

let executables_rules dc ~dir e_conf =
  let { Executables_conf.
        projections_check;
        allowed_ldd_dependencies;
        names;
        libraries = libraries_written_by_user;
        preprocess = preprocess_spec;
        preprocessor_deps;
        link_flags;
        modules = names_spec;
        review_help;
        js_of_ocaml;
        only_shared_object;
        _} = e_conf in
  let libname = fake_libname_of_exes names in
  let names = List.map names ~f:BN.of_string in
  let dc = Executables_conf_interpret.extend_dc e_conf dc in
  let wrapped = wrapped_bindirs in
  let for_executable = true in
  let can_setup_inline_runners = false in
  let names_set = BN.Set.of_list names in
  let libraries_written_by_user =
    Libmap.resolve_libdep_names_exn dc.libmap libraries_written_by_user
  in
  let compile_rules =

    setup_ml_compile_rules
      ~js_of_ocaml
      dc ~dir ~libname ~wrapped ~for_executable ~can_setup_inline_runners
      ~preprocessor_deps ~preprocess_spec ~names_spec ~libraries_written_by_user
      ~disallowed_module_dep:(fun x ->
        if Set.mem names_set x
        then Some (sprintf !"depending on an executable (%{BN}) doesn't make sense" x)
        else None)
  in
  let check_no_dead_code =
    let _, _, modules = eval_names_spec ~dc names_spec in
    let diff list set = List.filter list ~f:(fun elt -> not (Set.mem set elt)) in
    match diff modules names_set with
    | [] -> []
    | _ :: _ as modules_except_executables ->
      [
        (* We have to take into account both the files that are mentioned in
           implementations (obviously, ie objdeps) and mentioned only in mlis (with
           cmideps, because even though such files will not be linked in, they are
           still part of the build). *)
        Dep.List.concat_map names ~f:(moduledeps ~dir)
        *>>| fun moduledeps ->
        let deps = BN.Set.of_list moduledeps in
        match diff modules_except_executables deps with
        | [] -> ()
        | _ :: _ as dead_modules ->
          failposf ~pos:(dummy_position (User_or_gen_config.source_file ~dir))
            !"modules %s are not referenced by any executable"
            (String.concat ~sep:", " (List.map ~f:BN.to_string dead_modules))
            ()
      ]
  in
  let default_rule =
    Rule.default ~dir (
      if e_conf.skip_from_default then
        []
      else (
        check_no_dead_code @
        List.concat_map names ~f:(fun name ->
          let prefixed_name = PN.of_barename ~wrapped ~libname name in
          let exe_suf, _ = exe_artifact_in_std_aliases ~only_shared_object in
          let not_exe_sufs, exe_sufs =
            if Compiler_selection.m32 && Option.is_some js_of_ocaml
            then [".cmo"; ".cmx"], [Js_of_ocaml.exe_suf; exe_suf]
            else [".cmx"], [exe_suf]
          in
          if link_executables && e_conf.link_executables
          then List.map exe_sufs
                 ~f:(fun suf -> Dep.path (BN.suffixed ~dir name suf))
          else List.map not_exe_sufs
                 ~f:(fun suf -> Dep.path (PN.suffixed ~dir prefixed_name suf))
        )
      )
    )
  in
  let link_rules =
    List.concat_map names ~f:(fun name ->
      List.concat_map Ocaml_mode.all ~f:(fun ext ->
        link_executable ext dc ~dir ~wrapped ~libname ~link_flags ~projections_check
          ~allowed_ldd_dependencies ~js_of_ocaml ~only_shared_object name))
  in
  let review_help_rules =
    if review_help
    then
      List.concat_map names ~f:(fun name ->
        let name = BN.to_string name in
        [ Review_help.rule Ocaml_mode.native ~dir name
        ; Rule.default ~dir [ Dep.path (Review_help.help_filename ~dir name) ]
        ])
    else []
  in
  List.concat [
    (
      if wrapped
      then
        let _intfs, _impls, modules = eval_names_spec ~dc names_spec in
        renaming_rules ~dir ~libname ~modules
      else []
    );
    compile_rules;
    [default_rule];
    link_rules;
    review_help_rules;
  ]


(*----------------------------------------------------------------------
 top levels - utop and toplevel_expect_test
----------------------------------------------------------------------*)

(* Toplevel dependencies *)
module Toplevel = struct
  let includes = [ "+ocamldoc" ]

  let ppx () =
    let path = PPXset.create [PP.of_string "JANE"] |> PPXset.exe_path in
    [%test_result: Path.t]
      (* this path is hard-coded in [lib/js_utop/src/main.ml]: *)
      ~expect:(root_relative ".ppx/JANE/ppx.exe") path;
    path

end

module Toplevel_expect_tests_interpret = struct
  let ppx_jane = Libdep_name.of_string "ppx_jane"

    (* Expect-tests toplevels are different from normal, because preprocessors are linked
     straight into them (rather than being an other executable called by them). *)
  let libraries (conf : Toplevel_expect_tests.t) =
    if conf.no_ppx_jane || List.mem conf.libraries ppx_jane then
      conf.libraries
    else
      ppx_jane :: conf.libraries
end

let toplevel_runtime_libs libmap ~dir ~libname_for_libdeps
      ~(extra_lib : Lib_in_the_tree.t option) =
  Libmap.load_lib_deps libmap (LN.suffixed ~dir libname_for_libdeps ".libdeps")
  *>>| fun libs ->
  libs @ (Option.to_list extra_lib |> List.map ~f:Lib_dep.of_lib_in_the_tree)
;;

let toplevel_info_rules libmap ~dir ~libname_for_libdeps ~target ~extra_lib =
  let base = target ^ ".toplevel.info" in
  let c = relative ~dir (base ^ ".c") in
  let o = relative ~dir (base ^ ".o") in
  let o_rule =
    simple_rule ~targets:[o] ~deps:[Dep.path c]
      ~action:(
        Action.process ~dir ocamlc_path [basename c; "-o"; basename o;]
      )
  in
  let c_rule =
    Rule.create ~targets:[c] (
      Dep.both
        (toplevel_runtime_libs libmap ~dir ~libname_for_libdeps ~extra_lib)
        (Dep.path generate_static_string_c_code_sh)
      *>>| fun (libs, ()) ->
      let libs =
        List.filter_map libs ~f:Lib_dep.to_lib_in_the_tree
        |> List.map ~f:Lib_in_the_tree.name
      in
      let sexp =
        [%sexp
           { deps = (libs : LN.t list)
           ; dir  = (dir  : Path.t   )
           }
        ]
      in
      bashf ~dir !"echo -n %{quote} | %{quote} generated_toplevel_info > %{quote}"
        (Sexp.to_string_mach sexp)
        (reach_from ~dir generate_static_string_c_code_sh)
        (basename c)
      )
  in
  [c_rule; o_rule]
;;

let toplevel_rules dc ~dir ~libname_for_libdeps ~(extra_lib : Lib_in_the_tree.t option)
      ~target ~main ~more_deps =
  let toplevel_info = relative ~dir (target ^ ".toplevel.info.o") in
  let native_with_unsuffixed_exe =
    (module struct
      include (val Ocaml_mode.native : Ocaml_mode.S)
      let exe = ""
    end : Ocaml_mode.S)
  in
  let link_rule =
    link native_with_unsuffixed_exe dc ~dir
      ~more_deps:(Dep.path toplevel_info :: more_deps)
      ~link_flags:(["-linkall"; basename toplevel_info]
                   @ List.concat_map Toplevel.includes ~f:(fun dir -> ["-I"; dir]))
      ~allowed_ldd_dependencies:None
      ~ocaml_plugin_handling:`Not_a_plugin
      ~link_libdeps_of:(
        Dep.List.concat [
          return [ (dir, libname_for_libdeps) ];
          return [ (fst main, fake_libname_of_exes [snd main]) ];
        ])
      ~suppress_build_info:true
      ~suppress_version_util:true
      ~build_libs_DEFAULT:false
      ~compute_objs:(return [main])
      ~exe:target
      ~force_link:extra_lib
      ~test_or_bench:false
      ~js_of_ocaml:None
  in
  List.concat
    [ link_rule
    ; toplevel_info_rules dc.libmap ~dir ~libname_for_libdeps ~extra_lib ~target
    ]
;;

let utop_rules (dc : DC.t) ~lib_in_the_tree =
  let { Lib_in_the_tree. name = libname_for_libdeps; source_path = dir; _ } =
    lib_in_the_tree
  in
  let extra_lib = Some lib_in_the_tree in
  List.concat
    [ toplevel_rules dc
        ~more_deps:[ Dep.path (Toplevel.ppx ())
                   ; toplevel_runtime_libs dc.libmap ~dir ~libname_for_libdeps ~extra_lib
                     *>>= fun libs ->
                     Dep.all_unit (LL.dep_on_ocaml_artifacts libs ~suffixes:[".cmi"])
                   ]
        ~dir
        ~target:"utop"
        ~main:(root_relative "lib/js_utop/src", "main")
        ~libname_for_libdeps
        ~extra_lib
    ; [ Rule.alias (Alias.utop ~dir) (
          if dc.no_utop_alias then [] else
            [Dep.path (relative ~dir "utop")]
        );
      ]
    ]

let toplevel_expect_tests_rules (dc : DC.t) ~dir (conf : Toplevel_expect_tests.t) =
  let libname_for_libdeps = LN.of_string "ocaml-expect" in
  let extra_lib = None in
  let exe = relative ~dir "ocaml-expect" in
  let runtest =
    Dep.both
      (Dep.glob_listing (Glob.create ~dir "*.mlt"))
      (toplevel_runtime_libs dc.libmap ~dir ~libname_for_libdeps ~extra_lib)
    *>>= fun (mlt_files, libs) ->
    Dep.all_unit
      (List.map mlt_files ~f:(fun mlt_file ->
         Dep.action
           (Dep.all_unit
              (Dep.path mlt_file ::
               Dep.path exe      ::
               LL.dep_on_ocaml_artifacts libs ~suffixes:[".cmi"])
            *>>| fun () ->
            bashf ~dir !"./ocaml-expect %s %s %{quote}"
              (if inline_test_color    then "" else "-no-color")
              (if inline_test_in_place then "-in-place" else "")
              (Path.basename mlt_file))))
  in
  List.concat
    [ toplevel_rules dc ~dir
        ~more_deps:[]
        ~target:"ocaml-expect"
        ~main:(root_relative "lib/toplevel_expect_test/bin", "main")
        ~libname_for_libdeps
        ~extra_lib
    ; gen_libdeps dc.libmap ~dir ~libs:(Toplevel_expect_tests_interpret.libraries conf
                                        |> Libmap.resolve_libdep_names_exn dc.libmap)
        libname_for_libdeps
    ; [ Rule.alias (Alias.runtest ~dir) [runtest] ]
    ; (if link_executables then [ Rule.alias (Alias.default ~dir) [Dep.path exe] ] else [])
    ]

(*----------------------------------------------------------------------
 embedding for ocaml plugin
----------------------------------------------------------------------*)

let time_limit = relative ~dir:Config.script_dir "time_limit"

let ocaml_plugin_dir = root_relative "lib/ocaml_plugin"
let embedder = relative ~dir:ocaml_plugin_dir "bin/ocaml_embed_compiler.exe"

let stdlib_cmis () =
  (* We don't force the stdlib to be linked, so potentially plugins could build
     but not dynlink. We don't expect people to call the stdlib though, but rather
     they should use core, so it seems fine. *)
  Dep.action_stdout
    (return (Action.process ~dir:Path.the_root
               ocamlobjinfo_path [Filename.concat ocaml_where "stdlib.cmxa"]))
  *>>| fun output ->
  let lines = String.split_lines output in
  List.filter_map lines ~f:(fun line ->
    match String.chop_prefix line ~prefix:"Name: " with
    | None -> None
    | Some module_ ->
      Some (Filename.concat ocaml_where (String.uncapitalize module_ ^ ".cmi"))
  )
;;

let embed_rules dc ~dir conf =
  let wrapped = wrapped_bindirs in
  let {DC. xlibnames; _} = dc in
  let libname =
    match xlibnames with
    | [libname] -> libname
    | _ -> failwith "embed config requires exactly one executables or library config"
  in
  let {Embed_conf. names; libraries; cmis; pps=pp_names; code_style} = conf in
  let ppx_exe, camlp4_pps =
    let pps, flags = expand_pps pp_names in
    assert (List.is_empty flags);
    ppx_executable pps, camlp4_pps pps
  in
  List.concat_map names ~f:(fun prog ->
    let plugin_name = prog ^ ".ocaml_plugin" in
    let camlp4_spec, camlp4_dep_paths =
      let camlp4_pps = List.map camlp4_pps ~f:(fun pp ->
        LL.path_to_ocaml_artifact ~lib_in_the_tree:(PP.to_libname pp) ~suf:".cmxs"
      ) in
      match camlp4_pps with
      | [] -> [], []
      | _::_ ->
        List.concat
          [ [ "-pp" ; reach_from ~dir camlp4o_path ]
          ; List.concat_map camlp4_pps ~f:(fun pp -> [ "-pa-cmxs"; reach_from ~dir pp ])],
        camlp4o_path :: camlp4_pps
    in
    let preprocessing_spec, preprocessing_dep_paths =
      let ppx_spec = [ "-ppx"; reach_from ~dir ppx_exe ] in
      let ppx_dep_paths = [ppx_exe] in
      match code_style with
      | No_preprocessing -> [], []
      | Ppx -> ppx_spec, ppx_dep_paths
      | Bilingual -> camlp4_spec @ ppx_spec, camlp4_dep_paths @ ppx_dep_paths
    in
    let libraries =
      Libmap.resolve_libdep_names_exn dc.libmap libraries
      @ [Libmap.resolve_string_exn dc.libmap "ocaml_plugin"]
    in
    let gen_plugin_c =
      Rule.create
        ~targets:[relative ~dir (plugin_name ^ ".c")] (
        Dep.both
          (stdlib_cmis ())
          (Dep.List.concat_map libraries ~f:(fun lib ->
             LL.interface_deps dc.libmap lib *>>| fun deps -> lib :: deps
           ) *>>= fun libraries ->
           let libraries = Lib_dep.remove_dups_and_sort libraries in
           let local_cmis =
             List.map cmis ~f:(fun name ->
               let prefixed_name = PN.of_barename ~wrapped ~libname (BN.of_string name) in
               PN.suffixed ~dir prefixed_name ".cmi"
             )
           in
           Dep.List.concat_map libraries ~f:LL.submodule_cmi_paths
           *>>| fun library_cmis ->
           local_cmis @ library_cmis)
        *>>= fun (stdlib_cmis, cmis) ->
        let dep_paths = cmis @ preprocessing_dep_paths @ [ time_limit; embedder ] in
        Dep.all_unit (List.map ~f:Dep.path dep_paths) *>>| fun () ->
        (* Be careful here: there is a limit of MAX_ARG_STRLEN (130kB)  on any argument
           (note that this is distinct from ARG_MAX, which is bigger),
           so we can't use [bash -c], as the script could go over the limit,
           given how many cmis we can potentially have. *)
        Action.process ~dir
          (reach_from ~dir time_limit)
          (List.concat
            [ [ "300"
              ; reach_from ~dir embedder
              ]
            ; preprocessing_spec
            ; [ "-cc"
              ; ocamlopt_path
              ; "-ocamldep"
              ; ocamldep_path
              ]
            ; stdlib_cmis
            ; List.map ~f:(reach_from ~dir) cmis
            ; [ "-o"; plugin_name ^ ".c" ]
            ])
      )
    in
    let include_search_path = [] in
    List.concat [
      [gen_plugin_c];
      compile_c ~dir ~cflags:[] ~include_search_path plugin_name;
    ]
  )

(*----------------------------------------------------------------------
 jane-script specific
----------------------------------------------------------------------*)

let jane_script_rules dc
      { Jane_script_conf. libraries = public_libraries; pps } =
  let {DC. dir; _ } = dc in
  let pps, flags = expand_pps pps in
  assert (List.is_empty flags);
  let ppx_path = ppx_executable pps in
  let all_libraries =
    (Lib_dep.remove_dups_preserve_order
       (List.concat_map ~f:(libs_for_code_generated_by_pp dc.libmap) pps @
        List.map public_libraries ~f:(fun libname ->
          Lib_dep.of_lib_in_the_tree (Libmap.resolve_libname_exn dc.libmap
                                        ~lib_in_the_tree:libname)))
     |> libs_transitive_closure dc.libmap)
    *>>| List.filter_map ~f:(function
      (* jane-script currently supports library in the tree, doesn't care about
         libraries from compiler distribution, and doesn't support findlib packages. *)
      | Lib_dep.In_the_tree lib -> Some lib
      | From_compiler_distribution _ -> None
      | Findlib_package pkg ->
        failposf ~pos:(dummy_position dir)
          !"Embedding findlib package in jane-script is not currently supported \
            (trying to embed: %{sexp:Findlib_package.t})" pkg ())
  in
  let dep_for_libraries =
    all_libraries *>>= fun all_libraries ->
    (* Don't care about the cmx files. *)
    Dep.all_unit (LL.dep_on_ocaml_artifacts
                    (List.map all_libraries ~f:Lib_dep.of_lib_in_the_tree)
                    ~suffixes:[".cmi"; ".cmxs"])
  in
  let cfg_target = Path.relative ~dir "jane-script.cfg" in
  let cfg_rule =
    Rule.create ~targets:[cfg_target] (
      all_libraries *>>| fun all_libraries ->
      let sexp =
        let module Cfg = struct
          (* comments for the exact meaning of the fields are in
             app/jane-script/lib/static_data.mli *)
          type t = {
            compiler_dir_path : string;
            ppx_path          : string;
            stdlib_dir_path   : string;
            libs_dir_path     : string;
            public_libraries     : LN.t list;
            all_libraries        : LN.t list;
          } [@@deriving sexp_of]
        end in
        Cfg.sexp_of_t {
          compiler_dir_path = ocaml_bin;
          ppx_path = Path.reach_from ~dir ppx_path;
          stdlib_dir_path = ocaml_where;
          libs_dir_path = Path.reach_from ~dir LL.dir;
          public_libraries;
          all_libraries = List.map all_libraries ~f:Lib_in_the_tree.name;
        }
      in
      Action.save (Sexp.to_string_hum sexp) ~target:cfg_target
    )
  in
  let jane_script_alias = Alias.create ~dir "jane-script" in
  [ Rule.default ~dir [ Dep.alias jane_script_alias ]
  ; Rule.alias jane_script_alias
      [ Dep.path cfg_target;
        dep_for_libraries;
        Dep.path ppx_path;
      ]
  ; cfg_rule
  ;
  ]

(*----------------------------------------------------------------------
 inline_tests & benchmarks
----------------------------------------------------------------------*)

let inline_tests_script_rule ~dir ~libname ~runtime_environment ~script:target ~flags =
  let nodejs = "/j/office/app/nodejs/prod/v4.4.7/bin/node" in
  let emacs  = "/j/igm/app/emacs/builds/25.1/bin/emacs"    in
  let run =
    if drop_test
    then "echo >&2 'Tests have been disabled'; exit 1"
    else
      let command, args =
        match runtime_environment with
        | `Javascript ->
          nodejs ^ " ./inline_tests_runner" ^ Js_of_ocaml.exe_suf, " -drop-tag no-js"
        | `Emacs ->
          emacs ^ " -Q -L . -batch -l inline_tests_runner -- ", " -drop-tag js-only"
        | `Exe -> "./inline_tests_runner.exe", " -drop-tag js-only"
      in
      sprintf !{|exec %s inline-test-runner %{quote}%s %{concat_quoted} "$@"|}
        command (LN.to_string libname) args flags
  in
  (* We [export TZ] so that tests do not depend on the local timezone. *)
  write_string_rule ~chmod_x:() ~target:(relative ~dir target) (
{|#!/bin/sh
# This file was generated by jenga
cd "$(dirname "$(readlink -f "$0")")"
export TZ='America/New_York'
|} ^ run)
;;

let inline_bench_script_rule ~dir ~libname =
  let run =
    if drop_bench
    then "echo >&2 'Benches have been disabled'; exit 1"
    else
      sprintf
        !"BENCHMARKS_RUNNER=TRUE \
          BENCH_LIB=%{quote} \
          BENCH_CONFIG_PATH=%{quote} \
          exec ./inline_benchmarks_runner.exe \"$@\""
        (LN.to_string libname)
        (Path.reach_from ~dir
           (root_relative
             "lib/core_bench/inline_benchmarks_runner_lib_internal/etc/config.sexp"))
  in
  write_string_rule ~chmod_x:() ~target:(relative ~dir "inline_benchmarks_runner") (
{|#!/bin/sh
# This file was generated by jenga
cd "$(dirname "$(readlink -f "$0")")"
|} ^ run)
;;

let run_inline_action ~dir ~user_deps ~exe_deps ~flags ~runtime_deps filename =
  let sources = List.map ~f:(relative ~dir) (filename :: exe_deps) in
  (Dep.all_unit (List.map (time_limit :: sources) ~f:Dep.path
                 @ runtime_deps
                 @ Dep_conf_interpret.list_to_depends ~dir user_deps)
   *>>| fun () ->
   let args =
     (* Longer timeout for the javascript tests, which are sometimes much slower. *)
     [ (if Compiler_selection.m32 then "90" else "60")
     ; "./" ^ filename
     ]
     @ flags
     @ (if inline_test_color    then [] else ["-no-color"])
     @ (if inline_test_in_place then ["-in-place"] else [])
   in
   Action.process ~dir (reach_from ~dir time_limit) args)

let all_whitespace s = String.for_all s ~f:Char.is_whitespace

let non_empty_file ~dir ~filename =
  let path = relative ~dir filename in
  Dep.contents path *>>| fun s ->
  not (all_whitespace s)

let has_expect_tests ~dir =
  non_empty_file ~dir ~filename:fgrep_expect_test_filename

type sources_with_tests =
  { expect_tests : string list
  ; inline_test : unit
  }

let sources_with_tests ~dir =
  Dep.both
    (Dep.contents (relative ~dir fgrep_expect_test_filename))
    (Dep.contents (relative ~dir fgrep_inline_test_filename))
  *>>| fun (s1, s2) ->
  if all_whitespace s1 && all_whitespace s2
  then None
  else Some { expect_tests = String.split_lines s1
            ; inline_test = ()
            }

let has_benchmarks dc ~dir =
  let libmap = dc.DC.libmap in
  (* jaxbuilds strips [inline_benchmarks], as it's not part of the critical-path. Thus it
     shouldn't attempt to build the benchmarks. *)
  if Libmap.exists_in_the_tree libmap ~lib_in_the_tree:(LN.of_string "inline_benchmarks_internal")
  then non_empty_file ~dir ~filename:fgrep_bench_filename
  else return false

module Build_and_run = struct
  type t = Inline_tests.build_and_run
  let should_run : t -> _ = function
    | `build_and_run -> true
    | `dont_build_dont_run | `build_but_dont_run -> false
  let should_build = function
    | `build_but_dont_run | `build_and_run -> true
    | `dont_build_dont_run -> false
end

let inline_test_runner_dir, final_test_object =
  root_relative "ppx/ppx_inline_test/runner", "ppx_inline_test_runner"
let expect_runner_dir, expect_runner =
  root_relative "ppx/ppx_expect/evaluator", "ppx_expect_evaluator"
let inline_tests_rules dc ~skip_from_default ~lib_in_the_tree
      ~(user_config : Inline_tests.t) ~js_of_ocaml =
  let { Lib_in_the_tree. source_path = dir; name = libname; _ } = lib_in_the_tree in
  let only_shared_object = user_config.only_shared_object in
  let exe = "inline_tests_runner" in
  let exe_js = exe ^ "_js" in
  let if_expect_tests value =
    has_expect_tests ~dir
    *>>| function
    | true -> value
    | false -> []
  in
  let exe_suf, exe_artifact = exe_artifact_in_std_aliases ~only_shared_object in
  List.concat [
    List.concat_map Ocaml_mode.all ~f:(fun mode ->
      let mode = replace_exe_suffix mode ~only_shared_object in
      link mode dc ~dir
        ~suppress_build_info:true
        ~suppress_version_util:true
        ~build_libs_DEFAULT:false
        ~more_deps:[]
        ~link_flags:[]
        ~allowed_ldd_dependencies:None
        ~ocaml_plugin_handling:`Not_a_plugin
        ~link_libdeps_of:
          (Dep.List.concat [
             return [(dir, libname)];
             if_expect_tests [expect_runner_dir, fake_libname_of_exes [expect_runner]];
             return [inline_test_runner_dir,
                     fake_libname_of_exes [final_test_object]];
           ])
        ~force_link:(Some lib_in_the_tree)
        ~test_or_bench:true
        ~compute_objs:
          (Dep.List.concat [
             if_expect_tests [expect_runner_dir, expect_runner];
             return [inline_test_runner_dir, final_test_object];
           ])
        ~js_of_ocaml
        ~exe)
  ; [ inline_tests_script_rule ~dir ~libname ~flags:user_config.flags
         ~script:exe_js ~runtime_environment:`Javascript;
      inline_tests_script_rule ~dir ~libname ~flags:user_config.flags
        ~script:exe ~runtime_environment:(if only_shared_object then `Emacs else `Exe) ]
  ; (match alias_for_inline_runners ~dir ~skip_from_default with
    | None -> []
    | Some alias ->
      [ Rule.alias alias [
          sources_with_tests ~dir *>>= function
          | None -> return ()
          | Some { inline_test = (); expect_tests = _ } ->
            if link_executables
            && (Build_and_run.should_build user_config.native
                || (Build_and_run.should_build user_config.javascript
                    && Compiler_selection.m32))
            then begin
              (* If building an inline test runner, also build its runtime dependencies so
                 it's ready to be run manually. *)
              let names =
                List.concat [
                  if Build_and_run.should_build user_config.native
                  then [exe; exe ^ exe_suf]
                  else [];
                  if Build_and_run.should_build user_config.javascript
                  && Compiler_selection.m32
                  then [exe_js; exe ^ ".bc.js"]
                  else [];
                ]
              in
              Dep.all_unit (
                List.map names ~f:(fun name -> Dep.path (relative ~dir name))
                @ Dep_conf_interpret.list_to_depends ~dir user_config.deps
              )
            end
            else return ()
        ];
      ])
  ; if (Build_and_run.should_run user_config.javascript && Compiler_selection.m32)
    || (Build_and_run.should_run user_config.native
        && match exe_artifact with `Cmx -> false | `Exe | `So -> true) then
      let alias =
        match user_config.alias with
        | None -> Alias.runtest ~dir
        | Some name -> Alias.create ~dir name
      in
      [ Rule.alias alias [
          sources_with_tests ~dir *>>= function
          | None -> return ()
          | Some { inline_test = (); expect_tests = sources } ->
            let sources = lazy (String.Set.of_list sources) in
            let run ~exe_deps exe =
              Dep.action_stdout
                (run_inline_action ~dir ~exe_deps exe
                   ~user_deps:[]
                   ~runtime_deps:[]
                   ~flags:["-list-partitions"]
                )
              *>>= fun output ->
              let partitions = String.split_lines output in
              Dep.all_unit (List.map partitions ~f:(fun p ->
                Dep.action
                  (run_inline_action ~dir ~exe_deps exe
                     ~user_deps:user_config.deps
                     ~runtime_deps:
                       (let source = p ^ ".ml" in
                        if Set.mem (force sources) source
                        then [ Dep.path (relative ~dir source) ]
                        else [])
                     ~flags:["-partition"; p]
                  )))
            in
            Dep.all_unit
              [ if Build_and_run.should_run user_config.native
                then run ~exe_deps:[exe ^ exe_suf] exe
                else return ()
              ; if Build_and_run.should_run user_config.javascript && Compiler_selection.m32
                then run ~exe_deps:[exe ^ Js_of_ocaml.exe_suf] exe_js
                else return ()
              ]
        ]]
    else
      [];
  ]

let bench_runner_dir =
  (* inline_benchmarks_internal is not compatible with 32bit architectures. *)
  if Compiler_selection.m32
  then root_relative "lib/core_bench/inline_benchmarks_runner_lib_public/bin"
  else root_relative "lib/core_bench/inline_benchmarks_runner_lib_internal/bin"

let inline_bench_rules dc ~skip_from_default ~lib_in_the_tree =
  let { Lib_in_the_tree. source_path = dir; name = libname; _ } = lib_in_the_tree in
  let exe = "inline_benchmarks_runner" in
  List.concat [
    List.concat_map Ocaml_mode.all ~f:(fun mode ->
      link mode dc ~dir
        ~suppress_build_info:false
        ~suppress_version_util:true
        ~build_libs_DEFAULT:false
        ~link_flags:[]
        ~more_deps:[]
        ~allowed_ldd_dependencies:None
        ~ocaml_plugin_handling:`Not_a_plugin
        ~link_libdeps_of:
          (return
             [ dir, libname
             ; bench_runner_dir, fake_libname_of_exes ["runner"]
             ])
        ~force_link:(Some lib_in_the_tree)
        ~test_or_bench:true
        ~compute_objs:(return [(bench_runner_dir, "runner")])
        ~exe
        ~js_of_ocaml:None
    );
    [ inline_bench_script_rule ~dir ~libname ];
    match alias_for_inline_runners ~dir ~skip_from_default with
    | None -> []
    | Some alias ->
      [ Rule.alias alias [
          has_benchmarks dc ~dir *>>= function
          | false -> return ()
          | true ->
            let names =
              if link_executables
              then [exe; exe ^ ".exe"]
              else []
            in
            Dep.all_unit (List.map names ~f:(fun name -> Dep.path (relative ~dir name)))
        ];
      ]
  ]

(*----------------------------------------------------------------------
 generate_dep_rules
----------------------------------------------------------------------*)

let ocaml_libraries : [< Jbuild.t ] -> _ = function
  | `ocamllex _ -> []
  | `ocamlyacc _ -> []
  | `preprocessor x -> Preprocessor_conf.libraries x
  | `library x -> Library_conf.libraries x
  | `executables x -> Executables_conf_interpret.libraries x
  | `embed _ -> []
  | `jane_script _ -> []
  | `compile_c _ -> []
  | `rule _ -> []
  | `alias _ -> []
  | `no_utop -> []
  | `requires_camlp4 -> []
  | `unified_tests _ -> []
  | `toplevel_expect_tests x -> Toplevel_expect_tests_interpret.libraries x
  | `public_repo _ -> []
  | `html _ -> []
;;

let resolved_ocaml_libraries (dc : DC.t) jbuild =
  ocaml_libraries jbuild |> Libmap.resolve_libdep_names_exn dc.libmap

let xlibnames : Jbuild.t -> _ = function
  | `ocamllex _ -> []
  | `ocamlyacc _ -> []
  | `preprocessor x -> [Preprocessor_conf.name x]
  | `library x -> [Library_conf.name x]
  | `executables { Executables_conf_interpret.names; _ } -> [fake_libname_of_exes names]
  | `embed _ -> []
  | `jane_script _ -> []
  | `compile_c _ -> []
  | `rule _ -> []
  | `alias _ -> []
  | `no_utop -> []
  | `requires_camlp4 -> []
  | `unified_tests _ -> []
  | `toplevel_expect_tests _ -> []
  | `public_repo _ -> []
  | `html _ -> []
;;

let extra_disabled_warnings : Jbuild.t -> _ = function
  | `ocamllex _ -> []
  | `ocamlyacc _ -> []
  | `preprocessor x -> x.Preprocessor_conf.extra_disabled_warnings
  | `library x -> x.Library_conf.extra_disabled_warnings
  | `executables x -> x.Executables_conf_interpret.extra_disabled_warnings
  | `embed _ -> []
  | `jane_script _ -> []
  | `compile_c _ -> []
  | `rule _ -> []
  | `alias _ -> []
  | `no_utop -> []
  | `requires_camlp4 -> []
  | `unified_tests _ -> []
  | `toplevel_expect_tests _ -> []
  | `public_repo _ -> []
  | `html _ -> []
;;

let pps_of_jbuild (dc : DC.t) jbuild_item =
  let of_specs (xs : Preprocess_spec.t list) =
    List.concat_map xs ~f:(fun (kind,__names_spec) ->
      eval_preprocess_style_libs dc.libmap (PP_style.of_kind dc kind))
  in
  match jbuild_item with
  | `preprocessor x -> of_specs x.Preprocessor_conf.preprocess
  | `library x -> of_specs x.Library_conf.preprocess
  | `executables x -> of_specs x.Executables_conf.preprocess
  | `ocamllex _
  | `ocamlyacc _
  | `embed _
  | `jane_script _
  | `compile_c _
  | `rule _
  | `alias _
  | `no_utop
  | `requires_camlp4
  | `unified_tests _
  | `toplevel_expect_tests _
  | `public_repo _
  | `html _
    -> []

let generate_dep_rules dc ~dir jbuilds =
  List.concat_map jbuilds ~f:(fun jbuild ->
    List.concat_map (xlibnames jbuild) ~f:(fun libname ->
      let pp_libs = pps_of_jbuild dc jbuild in
      let user_libraries = resolved_ocaml_libraries dc jbuild in
      let libs = pp_libs @ user_libraries in
      gen_libdeps dc.libmap ~dir ~libs libname
    )
  )

(*----------------------------------------------------------------------
 merlin rules
----------------------------------------------------------------------*)

let merlin_1step_libs dc ~dir =
  Dep.List.concat_map dc.DC.xlibnames ~f:(fun libname ->
    Libmap.load_lib_deps dc.libmap (LN.suffixed ~dir libname ".libdeps")
  ) *>>| Lib_dep.remove_dups_preserve_order
;;

let command_for_merlin args =
  String.concat [ "'"; String.concat args ~sep:" "; "'" ]
;;

let merlin_ppx_directives ~dir (jbuilds : Jbuild_types.Jbuild.t list) =
  let merge_approx approx1 approx2 =
    match approx1, approx2 with
    | `No_code, v | v, `No_code -> v
    | `Cant_express, _ | _, `Cant_express -> `Cant_express
    | `Metaquot, `Metaquot -> `Metaquot
    | _, `Metaquot | `Metaquot, _ -> `Cant_express
    | `No_preprocessing, `No_preprocessing -> `No_preprocessing
    | `No_preprocessing, _ | _, `No_preprocessing -> `Cant_express
    | `Pps (pps1, flags1), `Pps (pps2, flags2) ->
      if pps1 = pps2 && flags1 = flags2 then approx1 else `Cant_express
  in
  (* We approximate by telling merlin to use preprocessors even the jbuild uses them on a
     subset of files. *)
  let preprocess_spec = function
    | [(`pps pps, _)] -> `Pps (expand_pps pps)
    | [(`metaquot, _)] -> `Metaquot
    | [] | [(`no_preprocessing, _)] -> `No_preprocessing
    | _ :: _ :: _ | [(`command _, _)] -> `Cant_express
  in
  let approx =
    List.fold jbuilds ~init:`No_code ~f:(fun acc jbuild_item ->
      merge_approx acc
        (match jbuild_item with
         | `library l -> preprocess_spec l.preprocess
         | `executables e -> preprocess_spec e.preprocess
         | `preprocessor p -> preprocess_spec p.preprocess
         | _ -> `No_code))
  in
  match approx with
  | `No_code | `No_preprocessing -> []
  | `Metaquot -> [ sprintf "FLG -ppx %s"
                     (command_for_merlin [Path.to_absolute_string metaquot_exe]) ]
  | `Pps _ | `Cant_express as approx ->
    let pps, flags =
      match approx with
      | `Pps (pps, flags) ->
        let flags =
          List.filter flags ~f:(function
            | "-ite-check" -> false
            | _ -> true
          )
        in
        pps, flags
      | `Cant_express -> Preprocessors.jane, []
    in
    let _relative_exe, args =
      get_ppx_command ~name:(BN.of_string "fake") ~kind:None ~dir ~can_setup_inline_runners:true
        ~flags ~libname:(LN.of_string "fake_for_merlin") pps
    in
    let exe = Path.to_absolute_string (ppx_executable pps) in
    [ sprintf "FLG -ppx %s" (command_for_merlin (exe :: "-as-ppx" :: args)) ]
;;

let merlin_rules dc ~dir (jbuilds : Jbuild_types.Jbuild.t list) =
  let target = relative ~dir ".merlin" in
  (* don't create .merlin files in all the directories when we don't need them *)
  if dc.DC.xlibnames = [] && dir <> Path.the_root then [] else [
    Rule.create ~targets:[target] (
      let preprocessor_directives =
        if List.exists jbuilds ~f:(function
            | `requires_camlp4 -> true
            | _ -> false
        ) then [
          "EXT here";
          "EXT ounit";
          "EXT custom_printf";
        ] else
          merlin_ppx_directives ~dir jbuilds
      in
      merlin_1step_libs dc ~dir *>>= fun libs ->
      let dot_merlin_contents =
        String.concat ~sep:"\n" (
          ("STDLIB " ^ ocaml_where)
          :: ("FLG " ^ (String.concat ~sep:" " dc.DC.merlinflags))
          :: "FLG -attributes-allowed" :: preprocessor_directives @ (
            (* When we use -open on the command line, we need to give it to merlin as
               well, otherwise it won't be able to type. In theory this should also be
               done if people add -open themselves to the list of flags in their jbuild,
               but in practice that doesn't happen. *)
            List.concat_map jbuilds ~f:(fun jbuild ->
              let wrapped = match jbuild with
                | `library conf -> Library_conf.wrapped conf
                | `executables _ -> wrapped_bindirs
                | _ -> false
              in
              if wrapped
              then List.map (xlibnames jbuild) ~f:(sprintf !"FLG -open %{LN.to_module}")
              else []
            )
          ) @
          (* We tell merlin to load cmi from the liblinks dir because that's what the
             compiler sees. But we tell merlin to load cmt from the source dir since they
             aren't installed. *)
          List.concat_map libs ~f:(function
            | In_the_tree lib ->
              let library_dir = LL.in_the_tree_library_dir lib.name in
              let src = reach_from ~dir lib.source_path in
              let cmi = reach_from ~dir library_dir in
              [
                sprintf "S %s" src;
                sprintf "CMT %s" src;
                sprintf "CMI %s" cmi;
              ]
            | From_compiler_distribution lib ->
              if String.is_prefix (From_compiler_distribution.to_string lib)
                   ~prefix:"ocaml" then
                [ "PKG compiler-libs" ]
              else
                []
            | Findlib_package _ ->
              []
          )
        )
      in
      let dependencies_dot_merlins =
        List.filter_map libs ~f:(function
          | In_the_tree lib ->
            Some (Dep.path (Path.relative ~dir:lib.source_path ".merlin"))
          | From_compiler_distribution _ | Findlib_package _ -> None
        )
      in
      Dep.all_unit dependencies_dot_merlins *>>| fun () ->
      write_string_action dot_merlin_contents ~target
    );
    Rule.alias (Alias.lib_artifacts ~dir) [Dep.path target];
    Rule.alias (Alias.runtest ~dir) [Dep.path target];
    alias_dot_filename_hack ~dir ".merlin";
  ]

(*----------------------------------------------------------------------
 library_rules
----------------------------------------------------------------------*)

module Library_conf_interpret = struct

  include Library_conf

  let disabled_warnings t =
    Compiler_config.disabled_warnings @ t.extra_disabled_warnings

  let o_names ~dir t = List.map t.o_names ~f:(expand_vars ~dir)

  let ocamlflags t =
    let ocamlflags = Top.default_ocamlflags ~disabled_warnings:(disabled_warnings t) in
    Ordered_set_lang.eval_with_standard t.flags ~standard:ocamlflags

  let ocamlcflags t =
    Ordered_set_lang.eval_with_standard t.ocamlc_flags ~standard:Top.ocamlcflags

  let ocamloptflags t =
    Ordered_set_lang.eval_with_standard t.ocamlopt_flags ~standard:Top.ocamloptflags

  let extend_dc t dc =
    let ocamlflags = ocamlflags t in
    let ocamlcflags = ocamlcflags t in
    let ocamloptflags = ocamloptflags t in
    {dc with DC.
      ocamlflags;
      ocamlcflags;
      ocamloptflags
    }

  let cflags t =
    Ordered_set_lang.eval_with_standard t.c_flags ~standard:default_cflags

  let cxxflags t =
    Ordered_set_lang.eval_with_standard t.cxx_flags ~standard:default_cxxflags

end

let library_rules_javascript ~libmap ~dir ~js_of_ocaml ~libname =
  match js_of_ocaml with
  | None -> []
  | Some js_of_ocaml ->
    let { Js_of_ocaml_conf.javascript_files; flags; _ } = js_of_ocaml in
    let check_dependencies =
      Rule.alias (Alias.default ~dir) [
        (Libmap.load_lib_deps libmap (LN.suffixed ~dir libname ".libdeps")
         *>>| fun libs ->
         let bad_libs =
           List.filter libs ~f:(function
             | In_the_tree _ ->
               false
             | From_compiler_distribution v ->
               not (From_compiler_distribution.supported_in_javascript v)
             | Findlib_package _ ->
               false)
         in
         if not (List.is_empty bad_libs)
         then
           failposf ~pos:(dummy_position (User_or_gen_config.source_file ~dir))
             !"%{LN} is supposed to work in javascript but it has \
               problematic dependencies: %s" libname
             (String.concat ~sep:" " (List.map bad_libs ~f:Lib_dep.to_string)) ())
      ]
    in
    let javascript_files_dep =
      let files = List.map javascript_files ~f:(Path.relative_or_absolute ~dir) in
      Js_of_ocaml_jsdeps.rule ~dir libname files
    in
    let js_compile_cma =
      let cma_without_suf = LN.to_string libname in
      let src    = Path.relative ~dir (cma_without_suf ^ ".cma") in
      let target = Path.relative ~dir (cma_without_suf ^ Js_of_ocaml.cma_suf) in
      let js_files = Dep.return [] in
      Js_of_ocaml.rule ~build_info:None ~hg_version:None ~js_files ~dir ~flags ~src ~target
    in
    [ check_dependencies; javascript_files_dep; js_compile_cma ]

let library_rules dc ~dir library_conf =
  let dc = Library_conf_interpret.extend_dc library_conf dc in
  let libname = Library_conf.name library_conf in
  let wrapped = Library_conf.wrapped library_conf in
  let preprocessor_deps = Library_conf.preprocessor_deps library_conf in
  let preprocess_spec = Library_conf.preprocess library_conf in
  let names_spec = Library_conf.modules library_conf in
  let () =
    let {Library_conf. wrapped; _ } = library_conf in
    if not wrapped then (
      let __intfs, impls, __modules = eval_names_spec ~dc names_spec in
      let ok =
        match impls with
        | [impl1] -> BN.is_lib impl1 ~libname
        | _ -> false
      in
      if not ok then (
        failwithf "Problem with unwrapped library `%s', in %s. Fails check: An unwrapped \
                   library must contain exactly one module, with the same name as the library"
          (LN.to_string libname) (Path.to_string dir) ()
      )
    )
  in
  let for_executable = false in
  let can_setup_inline_runners = true in
  let libraries_written_by_user =
    Libmap.resolve_libdep_names_exn dc.libmap library_conf.libraries
  in
  let compile_rules =
    setup_ml_compile_rules
      dc ~dir ~libname ~wrapped ~for_executable ~can_setup_inline_runners
      ~preprocessor_deps ~preprocess_spec ~names_spec
      ~libraries_written_by_user
      (* Don't setup [.cmo -> .cmo.js] rule for libraries.
         It's not needed by separate compilation. *)
      ~js_of_ocaml:None
  in
  let c_names = Library_conf.c_names library_conf in
  let o_names = Library_conf_interpret.o_names ~dir library_conf in
  let cxx_names = Library_conf_interpret.cxx_names library_conf in
  let cxxflags = Library_conf_interpret.cxxflags library_conf in
  let include_search_path = Library_conf_interpret.includes library_conf in
  let include_search_path = List.map include_search_path ~f:(expand_vars ~dir) in
  let cflags = List.map (Library_conf_interpret.cflags library_conf) ~f:(expand_vars ~dir) in
  let cxxflags = List.map cxxflags ~f:(expand_vars ~dir) in
  let cxx_suf =
    match Library_conf_interpret.cxx_suf library_conf with
    | None -> ".cpp"
    | Some suf -> "." ^ suf
  in
  let compile_c_rules =
    List.concat_map c_names ~f:(compile_c ~dir ~cflags ~include_search_path)
  in
  let compile_cxx_rules =
    List.concat_map cxx_names ~f:(compile_cxx ~dir ~cxxflags ~include_search_path ~cxx_suf)
  in
  let o_names = c_names @ cxx_names @
                o_names (*objects to link but not compile*)
  in
  let intfs, impls, modules = eval_names_spec ~dc names_spec in
  let stub_names, stub_rules =
    match Library_conf_interpret.self_build_stubs_archive library_conf with
    | Some name -> [name], []
    | None ->
      match o_names with
      | [] -> [], []
      | _::_ ->
        let target_a = stubs_archive_file (LN.to_string libname) in
        [LN.to_string libname], [static_archive_c ~dir ~o_names ~target:target_a]
  in
  let lib_flags =
    List.concat [
      link_time_args_for_c_compiler (
        List.map stub_names ~f:(sprintf "-l%s_stubs"));
      List.map (Library_conf_interpret.library_flags library_conf) ~f:(expand_vars ~dir);
      link_time_args_for_c_compiler (
        List.map (Library_conf_interpret.cclibs library_conf) ~f:(sprintf "-l%s"));
    ]
  in
  let shared_rule =
    let dynlink_dep () =
      if List.mem dc.ocamloptflags "-nodynlink" then
        failwithf
          !"Trying to build a .cmxs for library %{LN} but '-nodynlink' is set."
          libname ();
      Async.Std.Deferred.unit
    in
    let deps_paths =
      let suffixes = [".cmxa"; ".a"] in
      List.map suffixes ~f:(LN.suffixed ~dir libname)
      @ List.map stub_names ~f:(fun name -> relative ~dir (stubs_archive_file name))
    in
    let deps = Dep.deferred dynlink_dep :: List.map deps_paths ~f:Dep.path in
    let target = LN.to_string libname ^ ".cmxs" in
    let action =
      (* -linkall is needed since we build the .cmxs from a .cmxa, otherwise the .cmxs
         will just be empty. The other possibility would be to create cmxs for each
         module, which would keep the benefit of not packing libraries, including less
         crap to load and run the toplevel of.

         The stubs need to be linked in statically into the cmxs as usual, but although
         the compiler does pass them to the C compiler, they can get dropped, presumably
         because the linker doesn't see any use of some symbols in the current library. We
         pass -whole-archive to ld to turn off this dead code elimination. *)
      Action.process ~dir
        ocamlopt_path
        (List.concat
           [ [ "-linkall"
             ; "-I"; "."
             ; "-ccopt"; quote "-Wl,-whole-archive"
             ]
           ; ccopts (List.map stub_names ~f:(sprintf "-l%s_stubs"))
           ; [ "-ccopt"; quote "-Wl,-no-whole-archive"
             ; LN.to_string libname ^ ".cmxa"
             ; "-o"; target
             ; "-shared"
             ] ])
    in
    simple_rule ~targets:[ Path.relative ~dir target ] ~deps ~action
  in
  let modules_file_rule =
    let has_bin_annot =
      List.mem dc.ocamlflags "-bin-annot"
      || List.mem dc.ocamloptflags "-bin-annot"
    in
    Lib_modules.rule ~dir ~libname
      { impls_and_intfs = modules ; impls; intfs;
        bin_annot = has_bin_annot; }
  in
  let pack_maybe_archive_rules =
    if not wrapped
    then ocaml_library_archive dc ~wrapped ~dir ~libname ~flags:lib_flags ~impls:[]
    else
      renaming_rules ~dir ~libname ~modules
      @ ocaml_library_archive dc ~wrapped ~dir ~libname ~flags:lib_flags ~impls
  in
  let js_rules =
    library_rules_javascript
      ~dir ~libmap:dc.libmap ~libname
      ~js_of_ocaml:library_conf.js_of_ocaml
  in
  let skip_from_default = Library_conf_interpret.skip_from_default library_conf in
  let default_targets =
    (if skip_from_default then []
     else
       let suffixes = [".cmi";".cmxa";".a"] in
       List.map suffixes ~f:(fun suf -> LN.suffixed ~dir libname suf)
    ) @
    List.map stub_names ~f:(fun name -> relative ~dir (stubs_archive_file name))
  in
  let default_rules =
    [Rule.alias (Alias.lib_artifacts ~dir) (List.map default_targets ~f:Dep.path)]
  in
  let lib_in_the_tree : Lib_in_the_tree.t =
    { name        = library_conf.name
    ; public_name = library_conf.public_name
    ; source_path = dir
    }
  in
  let doc_alias =
    let odoc_dir = relative ~dir:Odoc.odoc_output_dir (LN.to_string libname) in
    let html_dir = relative ~dir:Odoc.html_output_dir (LN.to_string libname) in
    Rule.alias (Odoc.alias ~dir)
      [ Dep.alias (Odoc.alias ~dir:odoc_dir)
      ; Dep.alias (Odoc.alias ~dir:html_dir) ]
  in
  List.concat [
    [gen_interface_deps_from_objinfo dc ~dir ~wrapped ~libname ~libraries_written_by_user];
    Lib_clients.rules ~dir ~libname;
    [library_module_order ~dir ~impls ~intfs ~libname];
    [modules_file_rule];
    pack_maybe_archive_rules;
    compile_c_rules;
    compile_cxx_rules;
    [stub_names_rule ~dir ~libname ~stub_names];
    stub_rules;
    [shared_rule];
    compile_rules;
    default_rules;
    utop_rules dc ~lib_in_the_tree;
    [fgrep_rule ~dir ~filename:fgrep_inline_test_filename ~macros:inline_test_macros ~impls];
    [fgrep_rule ~dir ~filename:fgrep_expect_test_filename ~macros:expect_test_macros ~impls];
    [fgrep_rule ~dir ~filename:fgrep_bench_filename ~macros:bench_macros ~impls];
    inline_tests_rules dc ~lib_in_the_tree ~skip_from_default
      ~js_of_ocaml:library_conf.js_of_ocaml
      ~user_config:(Library_conf_interpret.inline_tests library_conf);
    js_rules;
    inline_bench_rules dc ~skip_from_default ~lib_in_the_tree;
    [doc_alias];
  ]

(*----------------------------------------------------------------------
public_repo_rules
----------------------------------------------------------------------*)

module Public_release = Public_release.Make(struct
    module LL = LL
    module BN = BN
    module DC = DC
    module Libmap = Libmap
    module Lib_modules = Lib_modules
    module Standard_pp_sets = Standard_pp_sets
    module PP = PP
    module User_or_gen_config = User_or_gen_config
    let public_release_files = public_release_files_path
    let deep_unignored_subdirs = deep_unignored_subdirs
    let libs_for_code_generated_by_pp = libs_for_code_generated_by_pp
    let ocaml_libraries = ocaml_libraries
    let pps_of_jbuild = pps_of_jbuild
    let expand_pps = expand_pps
    let remap_pa_names = remap_pa_names
  end)

let jbuild_rules_with_directory_context dc ~dir jbuilds =
  List.concat_map jbuilds ~f:(fun (j : Jbuild_types.Jbuild.t) ->
    match j with
    | `ocamllex _ | `ocamlyacc _ | `rule _ -> []
    | `preprocessor conf -> preprocessor_rules dc ~dir conf
    | `library conf -> library_rules dc ~dir conf
    | `executables conf -> executables_rules dc ~dir conf
    | `embed conf -> embed_rules dc ~dir conf
    | `jane_script conf -> jane_script_rules dc conf
    | `compile_c conf -> user_configured_compile_c_rules ~dir conf
    | `alias conf -> [alias_conf_to_rule ~dir conf]
    | `no_utop -> []
    | `requires_camlp4 -> []
    | `unified_tests conf ->
      Js_unified_tests.rules ~dir
        { target = conf.target
        ; setup_script = Option.map conf.setup_script ~f:(expand_vars ~dir)
        ; deps = Dep_conf_interpret.list_to_depends ~dir conf.deps
        }
    | `toplevel_expect_tests conf ->
      toplevel_expect_tests_rules dc ~dir conf
    | `public_repo conf -> Public_release.rules dc ~dir conf
    | `html conf -> Html.rules ~dir conf
  )

let rules_with_directory_context dc ~dir jbuilds =
  Scheme.rules
    (List.concat [
       [
         Rule.alias (Alias.lib_artifacts ~dir) [];
         Rule.default ~dir [Dep.alias (Alias.lib_artifacts ~dir)];
       ];
       jbuild_rules_with_directory_context dc ~dir jbuilds;
       generate_dep_rules dc ~dir jbuilds;
       merlin_rules dc ~dir jbuilds;
       Info_files.write ~dir;
       Public_release.Package_map.global_rules ~dir;
     ])

let rules_without_directory_context ~dir jbuilds =
  Scheme.rules
    (List.concat_map jbuilds ~f:(fun (j : Jbuild_types.Jbuild.t) ->
       match j with
       | `ocamllex l -> List.map l ~f:(ocamllex_rule ~dir)
       | `ocamlyacc l -> List.map l ~f:(ocamlyacc_rule ~dir)
       | `rule conf -> [rule_conf_to_rule ~dir conf]
       | `preprocessor _
       | `library _
       | `executables _
       | `embed _
       | `jane_script _
       | `compile_c _
       | `alias _
       | `no_utop
       | `requires_camlp4
       | `unified_tests _
       | `toplevel_expect_tests _
       | `public_repo _
       | `html _ -> []))
;;

(*----------------------------------------------------------------------
 libmap.sexp
----------------------------------------------------------------------*)

module Libmap_sexp : sig

  val rules : Rule.t list
  val get : Libmap.t Dep.t

end = struct

  type t = Lib_in_the_tree.t list * Findlib_package_name.Set.t
  [@@deriving sexp]

  (* fe and emacs read libmap.sexp, so now we can't change its format. *)
  let stable_sexp_of_t ((libs_in_the_tree, _) : t) =
    List.map libs_in_the_tree ~f:(fun l -> l.name, l.source_path)
    |> [%sexp_of: (LN.t * Path.t) list]
  ;;

  let stable_libmap_sexp_path = root_relative "libmap.sexp"
  let libmap_sexp_path = root_relative "libmap_unstable.sexp"

  let compute () : t Dep.t =
    Dep.both
      (deep_unignored_subdirs ~dir:Path.the_root *>>= fun dirs ->
       Dep.all
         (List.map dirs ~f:(fun dir ->
            User_or_gen_config.libnames ~dir *>>| fun libnames ->
            List.map libnames ~f:(fun (name, public_name) ->
              { Lib_in_the_tree.
                name
              ; source_path = dir
              ; public_name
              }))))
      Findlib.packages
    *>>| fun (libs, findlib_packages) ->
    (List.concat libs, findlib_packages)
  ;;

  let rules =
    (* Making libmap_unstable.sexp depend on libmap.sexp otherwise libmap.sexp would not
       be built, since all its readers are outside of jenga. *)
    [
      Rule.create ~targets:[libmap_sexp_path] (
        Dep.both (Dep.path stable_libmap_sexp_path) (compute ()) *>>| fun ((), t) ->
        write_string_action (Sexp.to_string_hum (sexp_of_t t)) ~target:libmap_sexp_path
      );
      Rule.create ~targets:[stable_libmap_sexp_path] (
        compute () *>>| fun t ->
        write_string_action (Sexp.to_string_hum (stable_sexp_of_t t)) ~target:stable_libmap_sexp_path
      );
    ]

  let get =
    let cache =
      ref ("", Or_error.try_with
                 (fun () -> Libmap.create_exn ([], Findlib_package_name.Set.empty)))
    in
    Dep.contents_cutoff libmap_sexp_path
    *>>| fun contents ->
    (* This test is expected to work because jenga shares the Dep.contents. *)
    if phys_equal contents (fst !cache)
    then ok_exn (snd !cache)
    else begin
      let res =
        Or_error.try_with (fun () ->
          let t = Sexp.of_string_conv_exn (String.rstrip contents) t_of_sexp in
          Libmap.create_exn t)
      in
      cache := (contents, res);
      ok_exn res
    end

end

let setup_liblinks_dir ~dir =
  Scheme.rules_dep (
    Libmap_sexp.get *>>| fun libmap ->
    [LL.api_rule ~dir libmap]
  )

let top_api_rule =
  Rule.alias (Alias.api ~dir:Path.the_root)
    [Dep.alias (Alias.api ~dir:LL.dir)]

let setup_liblinks ~dir =
  Scheme.rules_dep (
    Libmap_sexp.get *>>= fun libmap ->
    LL.rules ~dir libmap
  )

let setup_ppx_cache ~dir =
  Scheme.rules_dep (
    Centos.link_flags *>>= fun link_flags ->
    Libmap_sexp.get *>>| fun libmap ->
    generate_ppx_exe_rules libmap ~dir ~link_flags
  )

(*----------------------------------------------------------------------
 boot jenga
----------------------------------------------------------------------*)

module Boot = struct

  let dot_boot = ".boot"

  let parent_dir = root_relative dot_boot

  let boots : (string * (dir:Path.t -> Scheme.t)) list = [
    "jenga", Makefile.boot ~targets:[root_relative "app/jenga/bin/jenga.exe"]
  ]

  let setup_dir ~dir =
    match List.Assoc.find boots (Path.basename dir) with
    | Some mk_scheme -> mk_scheme ~dir
    | None -> Scheme.rules []

  let setup_parent_dir ~dir =
    assert (dir = parent_dir);
    Scheme.rules [
      Rule.default ~dir
        (List.map boots ~f:(fun (sub,_) ->
          Dep.alias (Alias.default ~dir:(Path.relative ~dir sub))))
    ]

end

(*----------------------------------------------------------------------
  autogen: determine from rule targets (& ocamllex/yacc)
----------------------------------------------------------------------*)

let filter_drop ~suffix xs =
  List.filter_map xs ~f:(fun x ->
    String.chop_suffix x ~suffix
  )

(* Check that a projection can be built without files not coming from the projection, as
   a way of checking that any executables building against base would be able to build
   only against base. This is a bit approximate because we could miss dependencies of link
   time dependencies for instance, but that has been working just fine in practice. *)
let self_contained_projections ~dir alias projections =
  let (module Mode) = Ocaml_mode.native in
  Rule.alias (Alias.create ~dir alias) [
    let libs_by_dir =
      Libmap_sexp.get *>>| fun libmap ->
      let f = Staged.unstage (Libmap.reverse_look libmap) in
      fun dir -> List.map (f dir) ~f:(fun lib -> Lib_in_the_tree.suffixed lib Mode.cmxa)
    in
    Fe.Projections_check.libs_in_projections_are_self_contained ~projections ~libs_by_dir
    *>>| Option.iter ~f:(fun error_msg ->
      let source = User_or_gen_config.source_file ~dir in
      failposf ~pos:(dummy_position source) "%s" error_msg ())
  ]
;;

(*----------------------------------------------------------------------
 create_directory_context, setup_main
----------------------------------------------------------------------*)

let create_directory_context ~dir jbuilds k =
  Centos.link_flags *>>= fun link_flags ->
  Libmap_sexp.get *>>| fun libmap ->
  Scheme.glob (Glob.create ~dir "*.ml{,i}") (fun ml_and_mlis ->
    (* These dependencies could/should be run in parallel *)
    List.iter jbuilds ~f:(fun jbuild ->
      List.iter (ocaml_libraries jbuild) ~f:(fun lib ->
        match Libmap.resolve_libdep_name libmap lib with
        | Some _ -> ()
        | None ->
          let pos = dummy_position (User_or_gen_config.source_file ~dir) in
          failposf ~pos !"unknown library %{Libdep_name}" lib ()));
    let merlinflags =
      let extra_disabled_warnings = List.concat_map jbuilds ~f:extra_disabled_warnings in
      let disabled_warnings = Compiler_config.disabled_warnings @ extra_disabled_warnings in
      Top.default_merlinflags ~disabled_warnings
    in
    let ocamlflags =
      Top.default_ocamlflags ~disabled_warnings:Compiler_config.disabled_warnings
    in
    let xlibnames = List.concat_map jbuilds ~f:xlibnames in
    let ocaml_plugin_libraries name =
      List.find_map jbuilds ~f:(function
      | `embed { Embed_conf.names; libraries; _ } ->
        if List.mem names name
        then Some (Libmap.resolve_libdep_names_exn libmap libraries)
        else None
      | _ -> None
      )
    in
    let no_utop_alias =
      Path.is_descendant ~dir:ppx_dir dir ||
      List.exists jbuilds ~f:(fun j -> match j with | `no_utop -> true | _ -> false)
    in
    let ml_and_mlis = List.map ml_and_mlis ~f:basename in
    let impls =
      (* Sort, so list order is stable when autogen files appear,
         and so action is unchanged *)
      List.map ~f:BN.of_string
        (remove_dups_and_sort (filter_drop ~suffix:".ml" ml_and_mlis))
    in
    let intfs =
      List.map ~f:BN.of_string
        (remove_dups_and_sort (filter_drop ~suffix:".mli" ml_and_mlis))
    in
    let impl_is_buildable = mem_of_list impls in
    let intf_is_buildable = mem_of_list intfs in
    (* select code_style uniformly for a directory *)
    let code_style =
      if List.exists jbuilds ~f:(function `requires_camlp4 -> true | _ -> false)
      then Requires_camlp4
      else Switched_to_ppx_style
    in
    k {DC.
      code_style;
      dir;
      link_flags;
      merlinflags;
      ocamlflags;
      ocamlcflags = Top.ocamlcflags;
      ocamloptflags = Top.ocamloptflags;
      xlibnames;
      ocaml_plugin_libraries;
      no_utop_alias;
      libmap;
      impls;
      intfs;
      impl_is_buildable;
      intf_is_buildable;
    }
  )

let setup_main ~dir =
  Scheme.dep (
    User_or_gen_config.load ~dir *>>| fun jbuilds ->
    Scheme.all
      [ rules_without_directory_context ~dir jbuilds
      ; Scheme.dep
          (create_directory_context ~dir jbuilds (fun dc ->
             rules_with_directory_context dc ~dir jbuilds))
      ]
  )

(*----------------------------------------------------------------------
 env
----------------------------------------------------------------------*)

let tmpdir = ".jenga.tmp"

let delete_and_recreate_tmpdir_action =
  (* We delete and recreate the tmpdir when jenga starts, and each time a polling build
   * restarts. ("*** jenga: rebuilding"). So we run this action in build_begin. *)
  bashf ~dir:Path.the_root "chmod -R +w %s &>/dev/null || true; rm -rf %s; mkdir -p %s" tmpdir tmpdir tmpdir

let putenv () = (* setup external actions *)
  [
    (* /tmp partitions are small, which causes issues (for instance many simultaneous
       ocaml-plugin runs can go over the 1GB limit, especially if /tmp already contains
       stuff). So we stick the tmp directory on the local disk, which is much harder to
       fill, is easier to clean automatically, and makes admins happy. *)
    ("TMPDIR", Some (Path.to_absolute_string (relative ~dir:Path.the_root tmpdir)));
    (* Comparisons in the shell depend on the locale (eg [[ ! /j < "4.01" ]]) so let's use
       the same one for everyone *)
    ("LANG", Some "C");
  ] @ Config.putenv

let call_hg_showconfig_to_trigger_dirstate_change () =
  let open Async.Std in
  Sys.readdir (Path.to_absolute_string Path.the_root)
  >>= fun subdirs ->
  Deferred.List.iter ("." :: Array.to_list subdirs) ~f:(fun dir ->
    Sys.file_exists (Filename.concat dir ".hg")
    >>= function
    | `No | `Unknown -> Deferred.unit
    | `Yes ->
      (* We rely on this action not touching the dirstate file in the case it is unchanged.
         If it did, we could not unconditionally call this function from build_end without
         causing a continuously looping build *)
      run_action_now (bash ~dir:(Path.root_relative dir) "hg showconfig 2>&1 > /dev/null")
  )

let build_begin () =
  Async.Std.don't_wait_for (call_hg_showconfig_to_trigger_dirstate_change ());
  run_action_now (
    delete_and_recreate_tmpdir_action
  )

let build_end () =
  Async.Std.don't_wait_for (call_hg_showconfig_to_trigger_dirstate_change ());
  Async.Std.Deferred.unit

let command_lookup_path () =
  let script_dir sub_dir =
    relative ~dir:Config.script_dir sub_dir
    |> Path.to_absolute_string
  in
  let dirs =
    [ (* Intercept calls made to "ranlib" from the ocaml compiler, and fix them to be
         deterministic by adding a 'D' modifier.
         - i.e. calling "ar -Ds" instead of "ar -s"
         We also now intercept calls to ar (from the ocaml compiler).
         And convert: "ar rc" -> "ar Drc" *)
      script_dir "deterministic-ranlib"
    ; script_dir "cpp_quietly"
    ]
  in
  match Config.command_lookup_path with
  | `Replace -> `Replace dirs
  | `Extend  -> `Extend  dirs

let rec under segment dir =
  (* Return true if [segment] is found anywhere in the path [dir].
     [dir] must be a descendant of [Path.the_root].
  *)
  if (dir = Path.the_root) then false else
    Path.basename dir = segment || under segment (Path.dirname dir)

(*----------------------------------------------------------------------
  recursive aliases
  ----------------------------------------------------------------------*)

let recursive_alias_list = [
  Alias.default;
  Alias.runtest;
  Alias.qtest;
  Alias.pp;
  Alias.libdeps;
  Alias.merlin;
  Alias.utop;
  Alias.c;
  Alias.unused_libs;
  Alias.save_benchmarks;
  Alias.create "empty";
  Odoc.alias;
] @ Lib_clients.aliases

let setup_recursive_aliases ~dir =
  Scheme.rules (
    List.map recursive_alias_list ~f:(fun make_alias ->
      Rule.alias (make_alias ~dir) [
        unignored_subdirs ~dir *>>= fun subs ->
        Dep.all_unit (
          List.map subs ~f:(fun sub -> Dep.alias (make_alias ~dir:sub))
        )]))

let empty_recursive_aliases ~dir =
  Scheme.rules (
    List.map recursive_alias_list ~f:(fun make_alias ->
      Rule.alias (make_alias ~dir) []))

let setup_odoc ~dir step =
  Scheme.rules_dep (
    Libmap_sexp.get *>>= fun libmap ->
    let libname = LN.of_string (basename dir) in
    let lib = Libmap.resolve_libname_exn libmap ~lib_in_the_tree:libname in
    let lib_deps =
      Libmap.load_lib_deps libmap (Lib_in_the_tree.suffixed lib ".libdeps")
    in
    Odoc.setup ~dir ~lib_in_the_tree:lib ~lib_deps step
  )

let scheme ~dir =
  (* Construct the rule scheme for given [dir] *)
  Scheme.all
    [ Scheme.sources [ relative ~dir Fe.dot_fe_sexp_basename
                     ; relative ~dir "jbuild"
                     ; relative ~dir "jbuild-ignore" ]
    ; begin
      (* Never build or call [artifacts] within any subtree of an .hg, .git or .fe *)
      if List.exists [".hg"; ".fe"; ".git"] ~f:(fun fn -> under fn dir)
      then Scheme.empty
      else
        let common_rules_except_for_liblinks =
          Scheme.all [
            setup_manifest ~dir;
            setup_recursive_aliases ~dir;
            Scheme.rules (Makefile.extract ~dir);
          ]
        in
        (* First, special cases for the root and directories created by the jengaroot. *)
        if dir = Path.the_root then
          Scheme.all [
            Scheme.rules (List.concat
              [ Libmap_sexp.rules;
                [ hg_version_out_rule;
                  Lib_clients.Cache.rule ~libmap_dep:Libmap_sexp.get;
                  alias_dot_filename_hack ~dir Lib_clients.Cache.file;
                  top_api_rule;
                  self_contained_projections ~dir "base-is-self-contained" ["base"];
                  public_release_files_rule;
                  alias_dot_directory_hack ~dir Boot.dot_boot;
                ];
                Findlib.global_rules;
              ]);
            Fe.setup_projections_targets;
            common_rules_except_for_liblinks;
          ]
        else
          let parent_dir = dirname dir in
          if dir = Boot.parent_dir then
            Boot.setup_parent_dir ~dir
          else if parent_dir = Boot.parent_dir then
            Boot.setup_dir ~dir
          else if parent_dir = LL.dir then
            setup_liblinks ~dir
          else if dir = LL.dir then
            setup_liblinks_dir ~dir
          else if parent_dir = ppx_cache_dir then
            setup_ppx_cache ~dir
          else if parent_dir = Odoc.odoc_output_dir then
            setup_odoc ~dir `Compile
          else if parent_dir = Odoc.html_output_dir then
            setup_odoc ~dir `Link
          else if Path.is_descendant ~dir:Js_of_ocaml.dot_js_dir dir then
            Js_of_ocaml.setup_dot_js_dir ~ocaml_where:ocaml_where_path dir
          (* Otherwise, the directory is a normal "source" directory. *)
          else
            Scheme.dep (
              is_ignored dir *>>| function
              | true ->
                (* Empty aliases that hydra can ask for .DEFAULT or .runtest even in ignored
                   places. *)
                empty_recursive_aliases ~dir
              | false ->
                Scheme.all [
                  common_rules_except_for_liblinks;
                  setup_main ~dir;
                ]
            )
      end
    ]

let ocaml_bin_file = ".omake-ocaml-bin"
let ocaml_bin_file_path = root_relative ocaml_bin_file
let ocaml_bin_file_rule = write_string_rule ocaml_bin ~target:ocaml_bin_file_path

let scheme ~dir =
  (* Wrapper to force build of [ocaml_bin_file] at scheme setup time *)
  Scheme.all [
    Scheme.rules (if not (dir = Path.the_root) then [] else [
      ocaml_bin_file_rule;
      alias_dot_filename_hack ~dir ocaml_bin_file;
    ]);
    Scheme.dep (
      Dep.path ocaml_bin_file_path *>>| fun () ->
      scheme ~dir)
  ]

let env () = Env.create
  ~putenv:(putenv ())
  ~command_lookup_path:(command_lookup_path ())
  ~build_begin
  ~build_end
  ~delete_eagerly: Js_stale_artifact_deletion.delete_eagerly
  ~delete_if_depended_upon: Js_stale_artifact_deletion.delete_if_depended_upon
  scheme

let check_compiler_exists () =
  let (>>|) = Async.Std.(>>|) in
  Async.Std.Sys.is_directory_exn
    Compiler_selection.compiler_bin_dir ~follow_symlinks:true
  >>| function
  | true -> ()
  | false ->
    List.iter ~f:(printf "%s")
      [ "The following compiler is not installed:"
      ; "  " ^ Compiler_selection.compiler_dir
      ; ""
      ];
    failwith "compiler not available"

let setup () =
  let open Async.Std in
  check_compiler_exists () >>| env
