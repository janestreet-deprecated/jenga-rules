open Core.Std

module Unexpanded_string () : sig
  type t = private string [@@deriving of_sexp]
  val to_string : t -> string
  val of_string : string -> t
end = String

module Unexpanded_command = Unexpanded_string ()

module Unexpanded_pp = Unexpanded_string ()

module Names_spec = struct
  type old =
    | List of string list
    | All
  [@@deriving of_sexp]

  type t = Ordered_set_lang.t
  let t_of_sexp sexp =
    match old_of_sexp sexp with
    | exception _ -> Ordered_set_lang.t_of_sexp sexp
    | All -> Ordered_set_lang.standard
    | List l -> Ordered_set_lang.t_of_sexp ([%sexp_of: string list] l)
end

module Preprocess_kind = struct
  type t = [
  | `no_preprocessing
  | `command            of Unexpanded_command.t
  | `metaquot
  | `pps                of Unexpanded_pp.t list
  ]
  [@@deriving of_sexp]
end

module Preprocess_spec = struct
  type t = Preprocess_kind.t * Names_spec.t
  [@@deriving of_sexp]
end

let unexpanded_pps_default = [(Unexpanded_pp.of_string "JANE")]
let preprocess_default = [
  (`pps unexpanded_pps_default, Ordered_set_lang.standard)
]

module Libname = Ocaml_types.Libname
module Libdep_name = Ocaml_types.Libdep_name
module Findlib_package_name = Ocaml_types.Findlib_package_name

module Js_of_ocaml_conf = struct
  type t =
    { flags : string sexp_list }
  [@@deriving of_sexp]
end

module Preprocessor_conf = struct
  type t = {
    name : Libname.t;
    libraries : Libdep_name.t sexp_list;
    extra_disabled_warnings : int sexp_list;
    preprocess : Preprocess_spec.t list [@default preprocess_default];
  } [@@deriving of_sexp, fields]
end

module Dep_conf = struct
  type t =
    | File of string
    | Alias of string
    | Glob_files of string
    | Files_recursively_in of string
  [@@deriving of_sexp]
  let t_of_sexp = function
    | Sexp.Atom s -> File s
    | Sexp.List _ as sexp -> t_of_sexp sexp
end

(** Configuration of the inline_tests_runner *)
module Inline_tests = struct
  type build_and_run =
    [ `dont_build_dont_run
    | `build_but_dont_run
    | `build_and_run
    ] [@@deriving of_sexp]
  type t = {
    (** The dependencies of running the inline tests *)
    deps : Dep_conf.t sexp_list;
    (** Flags to pass to the inline test runner *)
    flags : string sexp_list;
    (** Should inline_tests_runner be built and run in native environment *)
    native     : build_and_run [@default `build_and_run];
    (** Should inline_tests_runner be built and run in javascript environment *)
    javascript : build_and_run [@default `dont_build_dont_run];
  }
  [@@deriving of_sexp]

  let t_of_sexp sexp =
    let t = t_of_sexp sexp in
    if not (List.is_empty t.deps)
    && not (t.native = `build_and_run || t.javascript = `build_and_run)
    then
      Sexplib.Conv.of_sexp_error
        "inline tests deps only apply when running as part of runtest"
        sexp;
    t
  ;;

  let default : t = {
    deps = [];
    flags = [];
    native = `build_and_run;
    javascript = `dont_build_dont_run;
  }
end

module Rule_conf = struct
  type t = {
    targets : string list;
    deps : Dep_conf.t list;
    action : Unexpanded_command.t;
  } [@@deriving of_sexp, fields]
end

module Alias_conf = struct
  type t = {
    name : string;
    deps : Dep_conf.t list;
    action : Unexpanded_command.t sexp_option;
  } [@@deriving of_sexp, fields]
end

module Compile_c_conf = struct
  type t = {
    names : string list;
    c_flags : Ordered_set_lang.t sexp_option;
    includes : string sexp_list;
  } [@@deriving of_sexp]
end

module Embed_conf = struct
  type style = No_preprocessing | Ppx | Bilingual [@@deriving of_sexp]
  type t = {
    names : string list;
    libraries : Libdep_name.t sexp_list;
    cmis : string sexp_list;
    pps : Unexpanded_pp.t list [@default unexpanded_pps_default];
    code_style : style [@default Ppx];
  } [@@deriving of_sexp]
end

module Library_conf = struct
  module Public_release = struct
    module T = Public_release_types

    type bool_option = True | False | Default [@@deriving of_sexp]

    type extra_dep =
      { context : T.Buildable.Dependency.Context.t
      ; package : T.Buildable.Dependency.Global.t
      }
    [@@deriving of_sexp]

    let extra_dep_of_sexp (sexp : Sexp.t) =
      match sexp with
      | Atom _ ->
        { context = Both
        ; package = T.Buildable.Dependency.Global.t_of_sexp sexp }
      | _ -> extra_dep_of_sexp sexp

    (* Information for the public release *)
    type internal_package =
      { desc       : string sexp_option
      (* This is only needed because threads and co are hardcoded in jenga/root.ml *)
      ; extra_deps : extra_dep list  [@default []]
      (* Dependencies that only exist at the package level *)
      ; extra_opam_deps : T.Package.Dependency.t list [@default []]
      ; kind       : T.Library.Kind.t     [@default Normal]
      ; libraries  : Ordered_set_lang.t sexp_option;
      }
    [@@deriving of_sexp]

    type external_package =
      { opam_package      : string sexp_option
      ; virtual_opam_deps : string list [@default []]
      ; wrapped           : bool_option [@default Default]
      }
    [@@deriving of_sexp]

    type t =
      | Internal of internal_package
      | External of external_package
    [@@deriving of_sexp]

    let t_of_sexp (sexp : Sexp.t) =
      match sexp with
      | List (Atom ("external" | "External") :: _) -> t_of_sexp sexp
      | _ -> Internal (internal_package_of_sexp sexp)

    let default = Internal (internal_package_of_sexp (Sexp.List []))
  end

  type t = {
    (* [name] is the name of the library; the name must be distinct from the name of any
       module contained by the library.  The only mandatory field in the config. *)
    name : Libname.t;

    public_name : Findlib_package_name.t sexp_option;

    public_release : Public_release.t [@default Public_release.default];

    (* [libraries] are libraries required for the compilation of this library
       Defaults to the empty list; but very unusual for this field to be omitted. *)
    libraries : Libdep_name.t sexp_list;

    (* The following fields are all optional; the defaults are often fine... *)

    (* [wrapped] selects if the library should be built such that the modules are wrapped
       within an extra level of namespace, named for the library.  This field used to be
       named [packed].
       How this is achieved depends on the setting of [PACKING] env-var
         true ->
            Use the -pack/-for-pack flags of the ocaml compiler.
         false ->
            Dont use the -pack/-for-pack flags.
            Generate renaming file (.ml-gen) from list of modules.
            Use combination of -open and -o arg of 4.02 compiler. *)
    wrapped : bool [@default true];

    (* [modules] lists the names of all modules included in this library.  The names are
       written as lower case, corresponding to filenames containing the ML module
       definition & interface, but with the .ml/.mli suffix truncated.
       By default, the library contain modules for all .ml/.mli files in the directory
       containing the library. *)
    modules : Names_spec.t [@default Ordered_set_lang.standard];

    (* [extra_disabled_warnings] contains a list of warnings numbers, in addition to those
       listed by [disabled_warning] in jenga/root.ml which are not to be treated as
       compilation errors. It is very uncommon to need to extend this set.*)
    extra_disabled_warnings : int sexp_list;

    (* [flags], [ocamlc_flags] and [ocamlopt_flags] are used to modify the flags passed to
       the ocaml compilers.

       [ocamlc_flags] for the byte compiler only.
       [ocamlopt_flags] for the native compiler only.
       [flags] for both.

       These fields are interpreted w.r.t standard settings defined in module [Top] in
       jenga/root.ml and can make use of extension and filtering features provided by
       [Ordered_set_lang.t]

       Examples:
           (ocamlopt_flags (-inline 0 -nodynlink))  // replace standard settings
           (flags (:standard -I +ocamldoc))         // append standard
           (flags (-I +ocamldoc :standard))         // prepend standard
           (flags (:standard \ -strict-sequence))   // filter standard
    *)
    flags : Ordered_set_lang.t sexp_option;
    ocamlc_flags : Ordered_set_lang.t sexp_option;
    ocamlopt_flags : Ordered_set_lang.t sexp_option;

    (* Modify the flags passed to the c/c++ compiler. See the documentation for [flags]
       above. *)
    c_flags : Ordered_set_lang.t sexp_option;
    cxx_flags : Ordered_set_lang.t sexp_option;

    (* [cxx_suf] overrides the suffix expected for C++ files. The default is .cpp *)
    cxx_suf : string option [@default None];

    (* [self_build_stubs_archive] - not for casual use.
       Only used in base/re2/lib/jbuild *)
    self_build_stubs_archive : string option [@default None];

    (* Whether the lib is supposed to build in javascript. *)
    js_of_ocaml : Js_of_ocaml_conf.t sexp_option;


    includes : string sexp_list;
    library_flags : string sexp_list;

    c_names : string sexp_list;
    cxx_names : string sexp_list;
    o_names : string sexp_list;

    preprocess : Preprocess_spec.t list [@default preprocess_default];
    preprocessor_deps : Dep_conf.t sexp_list;
    (** Configuration for building and running inline tests *)
    inline_tests : Inline_tests.t [@default Inline_tests.default];
    cclibs : string sexp_list;
    skip_from_default : bool [@default false];
  } [@@deriving of_sexp, fields]

  let t_of_sexp sexp =
    let t = t_of_sexp sexp in
    begin match t.inline_tests.javascript, t.js_of_ocaml with
    | (`build_but_dont_run | `build_and_run), None ->
      Sexplib.Conv.of_sexp_error
        "cannot enable javascript tests because the library is not supported in \
         javascript (the js_of_ocaml field is missing)"
        sexp
    | `dont_build_dont_run, None
    | (`dont_build_dont_run | `build_but_dont_run | `build_and_run), Some _ -> ()
    end;
    t

end

module Projections_check = struct
  type t =
    { allow : string list
    ; output_result_to : string sexp_option
    }
  [@@deriving of_sexp]
end

module Executables_conf = struct
  module Public_release = struct
    type exe =
      { name       : string
      ; install_as : string option
      ; mode       : Public_release_types.Executable.Mode.t
      }

    let exe_of_sexp (sexp : Sexp.t) =
      match sexp with
      | Atom name ->
        { name; install_as = None; mode = Best }
      | List [Atom name; Atom install_as] ->
        { name; install_as = Some install_as; mode = Best }
      | List [Atom name; Atom install_as; Atom ":mode"; s] ->
        let mode = Public_release_types.Executable.Mode.t_of_sexp s in
        { name; install_as = Some install_as; mode }
      | _ ->
        Sexplib.Conv.of_sexp_error
          "invalid format, <name> or (<name> <install-as> :mode <mode>) expected"
          sexp

    type t =
      { build             : exe list [@default []]
      ; build_and_install : exe list [@default []]
      ; build_and_install_as_objects : exe list [@default []]
      ; extra_deps        : Library_conf.Public_release.extra_dep list  [@default []]
      }
    [@@deriving of_sexp]

    let default = t_of_sexp (List [])
  end

  type t = {
    (* Each element of [names] is an executable, without the ".exe" suffix. *)
    names : string list;
    link_executables : bool [@default true];
    public_release : Public_release.t [@default Public_release.default];
    projections_check : Projections_check.t sexp_option;
    allowed_ldd_dependencies : Ordered_set_lang.t sexp_option;
    extra_disabled_warnings : int sexp_list;
    flags : Ordered_set_lang.t sexp_option;
    ocamlc_flags : Ordered_set_lang.t sexp_option;
    ocamlopt_flags : Ordered_set_lang.t sexp_option;
    (* Immediate dependencies.  It is not necessary to name transitive dependencies or
       modules defined in the same directory.  (For the latter, see [modules] below.) *)
    libraries : Libdep_name.t sexp_list;
    preprocess : Preprocess_spec.t list [@default preprocess_default];
    preprocessor_deps : Dep_conf.t sexp_list;
    link_flags : string sexp_list;
    js_of_ocaml : Js_of_ocaml_conf.t sexp_option;
    (* Modules in this directory to include in these executables.  By default all modules
       are included.  If a single jbuild has multiple Executables_conf values, they must
       each specify a list of [modules], and those lists must be disjoint. *)
    modules : Names_spec.t [@default Ordered_set_lang.standard];
    (* [review_help] dumps the help output into a file so it can be checked in and
       reviewed.  It must default to false because the build system should not run random
       executables.  Not all executables follow the help conventions of Command.t.
       [review_help] is implemented using app/command-inspector; as of 2016-01 that makes
       repeated calls to the executable to generate the help, one call per subcommand.

       *)
    review_help : bool [@default false];
    only_shared_object : bool [@default false];
    skip_from_default : bool [@default false];
  } [@@deriving of_sexp]
end

module Jane_script_conf = struct
  type t = {
    libraries : Libname.t sexp_list;
    pps : Unexpanded_pp.t sexp_list;
  } [@@deriving of_sexp]
end

module Unified_tests = struct
  (* Jenga will generate a "run-unified-tests" script that can be used to manually run
     your tests. [setup_script] will be sourced before running the tests. The tests must
     be named test-XXX.t  *)
  type t =
    { target : string [@default "runtest"]
    ; deps : Dep_conf.t list
    ; setup_script : string sexp_option
    }
  [@@deriving of_sexp]
end

module Toplevel_expect_tests = struct
  type t =
    { libraries   : Libdep_name.t sexp_list
    ; no_ppx_jane : bool [@default false]
    }
  [@@deriving of_sexp]
end

module Public_repo = struct
  module T = Public_release_types

  type t =
    { dirs                : (string * Import.Path.t) list
    ; copyright_start     : int
    (* Additional files to copy. They must be from one of the directory in [dirs]. This is
       for when we want to export a generated file. *)
    ; additional_files    : Import.Path.t sexp_list
    (* Extra items to install. The filenames are relative to the external repository. *)
    ; install_extra       : T.Package.Install_item.t sexp_list
    }
  [@@deriving of_sexp]
end

module Jbuild = struct
  (* [Jbuild.t] describes the various kinds of build configuration descriptions.
     A jbuild file contains the sexp-representation of a list of [Jbuild.t]

     The most common items are [library] and [executables], example of which look like:

     (library
      ((name mylib)
       (libraries (core async))))

     (executables
      ((names (prog1 prog2))
       (libraries (core async mylib))))
  *)
  type t = [
  | `ocamllex of string list
  | `ocamlyacc of string list
  | `library of Library_conf.t
  | `executables of Executables_conf.t
  | `preprocessor of Preprocessor_conf.t
  | `embed of Embed_conf.t
  | `jane_script of Jane_script_conf.t
  | `compile_c of Compile_c_conf.t
  | `rule of Rule_conf.t
  | `alias of Alias_conf.t
  | `no_utop
  | `unified_tests of Unified_tests.t
  | `toplevel_expect_tests of Toplevel_expect_tests.t
  | `requires_camlp4
  | `public_repo of Public_repo.t
  ]
  [@@deriving of_sexp]
end

module If_ocaml_code_is_dynlinkable = struct
  type t =
    { if_dynlinkable     : Jbuild.t sexp_list
    ; if_not_dynlinkable : Jbuild.t sexp_list
    } [@@deriving of_sexp]
end

module Jbuild_with_if = struct
  type t = [
    | Jbuild.t
    | `if_ocaml_code_is_dynlinkable of If_ocaml_code_is_dynlinkable.t
  ] [@@deriving of_sexp]
end
