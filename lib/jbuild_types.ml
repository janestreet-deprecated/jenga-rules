open Core
open! Import

module PP : sig
  include Identifiable.S
  val remove_dups_and_sort : t list -> t list
  val to_libdep_name : t -> Ocaml_types.Libdep_name.t
  val jane : t
end = struct
  include String
  let remove_dups_and_sort = remove_dups_and_sort
  let to_libdep_name t = Ocaml_types.Libdep_name.of_string t

  let of_string s =
    assert (not (String.is_prefix s ~prefix:"-"));
    s

  let t_of_sexp sexp =
    let s = string_of_sexp sexp in
    if String.is_prefix s ~prefix:"-" then
      of_sexp_error "flag not allowed here" sexp;
    of_string s

  let jane = "ppx_jane"
end

module Pp_or_flag = struct
  type t =
    | PP   of PP.t
    | Flag of string

  let of_string s =
    if String.is_prefix s ~prefix:"-" then
      Flag s
    else
      PP (PP.of_string s)

  let t_of_sexp sexp = of_string (string_of_sexp sexp)

  let split l =
    List.partition_map l ~f:(function
      | PP pp  -> `Fst pp
      | Flag s -> `Snd s)
end

module User_action = struct
  module Mini_shexp = struct
    module Syntax = struct
      type 'a t =
        | Run    of 'a sexp_list
        | Chdir  of 'a * 'a t
        | Setenv of 'a * 'a * 'a t
      [@@deriving of_sexp]
    end
    type 'a t =
      | Run    of 'a * 'a list
      | Chdir  of 'a * 'a t
      | Setenv of 'a * 'a * 'a t

    let rec of_syntax (syntax : _ Syntax.t) sexp =
      match syntax with
      | Run [] -> of_sexp_error "run constructor needs at least one element" sexp
      | Run (prog :: args) -> Run (prog, args)
      | Chdir (a, b) -> Chdir (a, of_syntax b sexp)
      | Setenv (a, b, c) -> Setenv (a, b, of_syntax c sexp)

    let t_of_sexp a_of_sexp sexp =
      of_syntax (Syntax.t_of_sexp a_of_sexp sexp) sexp

    let rec map t ~f =
      match t with
      | Run (prog, args) -> Run (f prog, List.map args ~f)
      | Chdir (fn, t) -> Chdir (f fn, map t ~f)
      | Setenv (var, value, t) -> Setenv (f var, f value, map t ~f)

    let rec fold t ~init:acc ~f =
      match t with
      | Run (prog, args) -> List.fold_left args ~init:(f acc prog) ~f
      | Chdir (fn, t) -> fold t ~init:(f acc fn) ~f
      | Setenv (var, value, t) -> fold t ~init:(f (f acc var) value) ~f

    let to_action ?sandbox ~dir (t : string t) =
      let rec loop env dir = function
        | Chdir (fn, t) ->
          loop env (Path.relative ~dir fn) t
        | Setenv (var, value, t) ->
          loop ((var, value) :: env) dir t
        | Run (prog, args) ->
          Action.process ?sandbox ~env ~dir prog args
      in
      loop [] dir t
  end

  module T = struct
    type 'a t =
      | Bash of 'a
      | Shexp of 'a Mini_shexp.t

    let t_of_sexp a_of_sexp (sexp : Sexp.t) =
      match sexp with
      | Atom _ -> Bash  ([%of_sexp: a             ] sexp)
      | List _ -> Shexp ([%of_sexp: a Mini_shexp.t] sexp)

    let map t ~f =
      match t with
      | Bash x -> Bash (f x)
      | Shexp x -> Shexp (Mini_shexp.map x ~f)

    let fold t ~init ~f =
      match t with
      | Bash x -> f init x
      | Shexp x -> Mini_shexp.fold x ~init ~f
  end

  include T

  module Unexpanded = String_with_vars.Lift(T)
end

module Names_spec = struct
  type old =
    | List of string list
    | All
  [@@deriving of_sexp]

  type t = Ordered_set_lang.t
  let t_of_sexp sexp =
    match old_of_sexp sexp with
    | exception _ -> Ordered_set_lang.t_of_sexp sexp
    | All ->
          of_sexp_error "\
This is using the old syntax for name set specifications.
Replace by (:standard) or the whole preprocess field by (preprocess <kind>)"
            sexp
    | List _ ->
          of_sexp_error "\
This is using the old syntax for name set specifications.
Replace (list (<names>)) by (<names>)."
            sexp
end

module Preprocess_kind = struct
  type t = [
  | `no_preprocessing
  | `command            of String_with_vars.t
  | `pps                of Pp_or_flag.t list
  ]
  [@@deriving of_sexp]

  let t_of_sexp = function
    | Sexp.Atom "metaquot" as sexp ->
      of_sexp_error "\
\"metaquot\" is no longer supported. For code in the external/ directory, replace it by:

  (pps (ppx_tools_metaquot))

which is equivalent to the old \"metaquot\". Otherwise replace it by:

  (pps (ppx_metaquot))

The main difference between the two is that \"ppx_metaquot\" doesn't understand
[@@@metaloc ...] annotations and always uses a \"loc\" variable rather than a global
reference."
        sexp
    | sexp -> t_of_sexp sexp
end

module Preprocess_spec = struct
  type t = Preprocess_kind.t * Names_spec.t
  [@@deriving of_sexp]
end

module Preprocess_specs = struct
  type t = Preprocess_spec.t list

  let t_of_sexp (sexp : Sexp.t) =
    match sexp with
    | List (Atom "per_file" :: l) -> List.map l ~f:Preprocess_spec.t_of_sexp
    | sexp ->
      match Preprocess_kind.t_of_sexp sexp with
      | kind -> [(kind, Ordered_set_lang.standard)]
      | exception exn ->
        match [%of_sexp: Preprocess_spec.t list] sexp with
        | exception _ -> raise exn
        | _ ->
          of_sexp_error "\
This is using the old syntax for (preprocess ...).
If you have multiple subsets in your 'preprocess' field, write:

  (preprocess (per_file (<kind1> <names-spec>) (<kind2> <names-spec>) ...))

Otherwise simply write (preprocess <kind>) which means to use <kind> for all modules."
            sexp
end

let pps_default = [PP.jane]
let preprocess_default = [
  (`pps (List.map pps_default ~f:(fun pp -> Pp_or_flag.PP pp)),
   Ordered_set_lang.standard)
]

module Lint_spec = struct
  type t = [ `pps of Pp_or_flag.t list ] [@@deriving of_sexp]
end

module Alias_basename = struct
  type t = string [@@deriving of_sexp]
  let t_of_sexp sexp =
    let t = t_of_sexp sexp in
    if String.(=) t "qtest"
    then of_sexp_error "the qtest alias is deprecated, it won't get run" sexp
    else t
end

module Artifact_name = Ocaml_types.Artifact_name
module Libname = Ocaml_types.Libname
module Libdep_name = Ocaml_types.Libdep_name
module Findlib_package_name = Ocaml_types.Findlib_package_name

module Js_of_ocaml_conf = struct
  type t =
    { flags            : string sexp_list;
      javascript_files : string sexp_list;
      toplevel         : Libdep_name.t sexp_list }
  [@@deriving of_sexp]
end

module Dep_conf = struct
  type t =
    | File of String_with_vars.t
    | Alias of String_with_vars.t
    | Glob_files of String_with_vars.t
    | Files_recursively_in of String_with_vars.t
  [@@deriving of_sexp]
  let t_of_sexp = function
    | Sexp.Atom s -> File (String_with_vars.of_string s)
    | Sexp.List _ as sexp -> t_of_sexp sexp
end

module Uses_catalog = struct
  (** This determines how jenga should wrap commands: *)
  type t =
    | No (** prevent it from reaching out to catalog *)
    | Yes (** create a temporary instance for it *)
    | Yes_but_exempt_from_sandboxing  (** let it do what it wants. This should only be
                                          used for the tests of catalog itself. *)

  [@@deriving sexp]
end

module Sandbox_conf = struct
  include Jenga_lib.Api.Sandbox
  let t_of_sexp : Sexp.t -> t = function
    | Atom "none" -> none
    | Atom "hardlink" | List [Atom "hardlink"] -> hardlink
    | Atom "copy" | List [Atom "copy"] -> copy
    | Atom "ignore_targets" | List [Atom "ignore_targets"] -> hardlink_ignore_targets
    | List [Atom "hardlink"; Atom "ignore_targets"] -> hardlink_ignore_targets
    | List [Atom "copy"; Atom "ignore_targets"] -> copy_ignore_targets
    | sexp ->
      Sexplib.Conv.of_sexp_error "invalid sandbox kind" sexp
end

let validate_test_timeout ~sexp ~alias_field_name ~default_timeout alias timeout =
  let fail msg = Sexplib.Conv.of_sexp_error msg sexp in
  match alias, timeout with
  | None, Some timeout when Time.Span.(>) timeout default_timeout ->
    fail (
      sprintf "Test timeout increased without specifying the [%s]. \
               Please consider moving slow tests to [feature-subtree-build] alias and/or \
               think about how your slow tests will affect other people. \
               Also consider splitting up your test into multiple files because timeout \
               is per-file. This is not just a way of tricking the timeout mechanism: it \
               also increases parallelism, which might make tests faster."
        alias_field_name)
  | _ -> ()

(** Configuration of the inline_tests_runner *)
module Inline_tests = struct
  type build_and_run =
    [ `dont_build_dont_run
    | `build_but_dont_run
    | `build_and_run
    ] [@@deriving of_sexp]

  let should_run ~default : build_and_run option -> _ = function
    | Some `build_and_run -> true
    | Some (`dont_build_dont_run | `build_but_dont_run) -> false
    | None -> default

  let should_build ~default : build_and_run option -> _ = function
    | Some (`build_but_dont_run | `build_and_run) -> true
    | Some `dont_build_dont_run -> false
    | None -> default

  type t = {
    (** The dependencies of running the inline tests *)
    deps : Dep_conf.t sexp_list;
    (** Per-file test timeout *)
    timeout : Time.Span.t sexp_option;
    (** Flags to pass to the inline test runner *)
    flags : string sexp_list;
    (** The alias that runs the tests runners in *)
    alias : Alias_basename.t sexp_option;
    (** Should inline_tests_runner be built and run in native environment *)
    native     : build_and_run sexp_option;
    (** Should inline_tests_runner be built and run in javascript environment *)
    javascript : build_and_run sexp_option;
    (** Should inline_tests_runner be built as a .exe or as a .so? *)
    only_shared_object : bool [@default false];
    (** Does the inline test use catalog? *)
    uses_catalog : Uses_catalog.t [@default No];
    (** Sandbox to use for running the tests *)
    sandbox : Sandbox_conf.t [@default Sandbox_conf.hardlink];
  }
  [@@deriving of_sexp]

  type what_tests_to_build_or_run =
    { build_native     : bool
    ; run_native       : bool
    ; build_javascript : bool
    ; run_javascript   : bool
    }

  let what_tests_to_build_or_run t ~has_js_of_ocaml ~javascript_enabled =
    { build_native = should_build ~default:true t.native
    ; run_native   = should_run   ~default:true t.native
    ; build_javascript =
      javascript_enabled && should_build ~default:has_js_of_ocaml t.javascript
    ; run_javascript =
      javascript_enabled && should_run   ~default:has_js_of_ocaml t.javascript
    }

  let default_timeout ~for_javascript =
    (* Longer timeout for the javascript tests, which are sometimes much slower. *)
    if for_javascript then Time.Span.of_sec 120. else Time.Span.of_sec 60.

  let validate t ~sexp ~has_js_of_ocaml =
    let w = what_tests_to_build_or_run t ~has_js_of_ocaml ~javascript_enabled:true in
    let fail msg = Sexplib.Conv.of_sexp_error msg sexp in
    if not w.build_native && not w.build_javascript then begin
      if not (List.is_empty t.deps)
      then fail "inline tests deps only apply to the default way of building tests";
    end;
    if not w.run_native && not w.run_javascript then begin
      if not (Option.is_none t.alias)
      then fail "inline tests alias only applies to the default way of running tests";
      if not (Option.is_none t.timeout)
      then fail "timeout only applies to the default way of running tests";
    end;
    validate_test_timeout ~sexp
      t.alias ~alias_field_name:"alias"
      t.timeout ~default_timeout:(default_timeout ~for_javascript:false);
    if (w.run_javascript || w.build_javascript) && not has_js_of_ocaml then begin
      fail "cannot enable javascript tests because the library is not supported in \
            javascript (the js_of_ocaml field is missing)"
    end;
  ;;

  let t_of_sexp sexp =
    let t = t_of_sexp sexp in
    validate t ~sexp ~has_js_of_ocaml:true; (* conservative check, for better error *)
    t
  ;;

  let default : t = {
    deps = [];
    timeout = None;
    flags = [];
    alias = None;
    native = None;
    javascript = None;
    only_shared_object = false;
    uses_catalog = No;
    sandbox = Sandbox_conf.hardlink;
  }
end

(** The timeout of user commands. *)
let default_timeout = Time.Span.of_min 5.

module Rule_conf = struct
  type t = {
    targets : string list;
    deps : Dep_conf.t list;
    action : User_action.Unexpanded.t;
    sandbox : Sandbox_conf.t [@default Sandbox_conf.hardlink];
    uses_catalog : Uses_catalog.t [@default No];
    timeout : Time.Span.t option [@default Some default_timeout];
  } [@@deriving of_sexp, fields]
end

module Alias_conf = struct
  type t = {
    name : Alias_basename.t;
    deps : Dep_conf.t list;
    action : User_action.Unexpanded.t sexp_option;
    sandbox : Sandbox_conf.t [@default Sandbox_conf.hardlink];
    uses_catalog : Uses_catalog.t [@default No];
    timeout : Time.Span.t option [@default Some default_timeout];
  } [@@deriving of_sexp, fields]
end

module Compile_c_conf = struct
  type t = {
    names : string list;
    c_flags : Ordered_set_lang.Unexpanded.t sexp_option;
    includes : string sexp_list;
  } [@@deriving of_sexp]
end

module Embed_conf = struct
  type style = No_preprocessing | Ppx [@@deriving of_sexp]
  type t = {
    names : string list;
    libraries : Libdep_name.t sexp_list;
    cmis : string sexp_list;
    pps : PP.t list [@default pps_default];
    code_style : style [@default Ppx];
  } [@@deriving of_sexp]
end

module Library_conf = struct
  module External = struct
    (* Description of external libraries imported into Jane *)
    type t =
      { (* [true] if we are repackaging the library internally. i.e. if externally the
           library exports several toplevel modules and internally we are repackaging it
           under a single module.

           When [true], we need to produce a wrapper module in every library/executables
           directory using this library since the internal toplevel wrapper doesn't
           exist. *)
        repackaged : bool [@default false]
      ; (* Normally the opam package name is inferred from the [public_name] field,
           however some libraries do not follow this convention. In this case this field
           specify which opam package defines the library. *)
        opam_package : string sexp_option
      } [@@deriving of_sexp]
  end

  type t = {
    (* [name] is the name of the library; the name must be distinct from the name of any
       module contained by the library.  The only mandatory field in the config. *)
    name : Libname.t;

    (* Public release: name under which this library is referred once installed on the
       system. If this field is missing the library won't be installed and can only be
       used to build executables or for tests/benchmarks.  *)
    public_name : Findlib_package_name.t sexp_option;

    (* List of header files that are meant to be used by other libraries. This is
       currently only implemented in the public release. *)
    install_c_headers : string sexp_list;

    (* Runtime deps when the library is a ppx plugin. *)
    ppx_runtime_libraries : Libdep_name.t sexp_list;

    (* Set for external libraries imported in our tree. This is used for the public
       release, so that we know how to handle dependencies on this library. *)
    external_lib : External.t sexp_option;

    (* [libraries] are libraries required for the compilation of this library
       Defaults to the empty list; but very unusual for this field to be omitted. *)
    libraries : Libdep_name.t sexp_list;

    (* The following fields are all optional; the defaults are often fine... *)

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
    c_flags : Ordered_set_lang.Unexpanded.t sexp_option;
    cxx_flags : Ordered_set_lang.Unexpanded.t sexp_option;

    (* [cxx_suf] overrides the suffix expected for C++ files. The default is .cpp *)
    cxx_suf : string option [@default None];

    (* [self_build_stubs_archive] - not for casual use.
       Only used in base/re2/lib/jbuild *)
    self_build_stubs_archive : string option [@default None];

    (* Whether the lib is supposed to build in javascript. *)
    js_of_ocaml : Js_of_ocaml_conf.t sexp_option;


    includes : String_with_vars.t sexp_list;
    library_flags : String_with_vars.t sexp_list;

    c_names : string sexp_list;
    cxx_names : string sexp_list;
    o_names : String_with_vars.t sexp_list;
    c_libraries : string sexp_list;
    c_library_flags : Ordered_set_lang.Unexpanded.t sexp_option;

    preprocess : Preprocess_specs.t [@default preprocess_default];
    preprocessor_deps : Dep_conf.t sexp_list;

    (* Call a command on all ocaml modules to enforce coding conventions *)
    lint : Lint_spec.t sexp_option;

    (** Configuration for building and running inline tests *)
    inline_tests : Inline_tests.t [@default Inline_tests.default];

    skip_from_default : bool [@default false];
  } [@@deriving of_sexp, fields]

  let t_of_sexp sexp =
    let t = t_of_sexp sexp in
    Inline_tests.validate t.inline_tests ~sexp
      ~has_js_of_ocaml:(Option.is_some t.js_of_ocaml);
    (match t.external_lib, t.public_name with
     | Some _, None ->
       of_sexp_error "external libraries must have a (public_name ...) field" sexp
     | _ -> ());
    t

  let to_lib_in_the_tree ~dir t : Ocaml_types.Lib_in_the_tree.t =
    { name                    = t.name
    ; public_name             = t.public_name
    ; supported_in_javascript = Option.is_some t.js_of_ocaml
    ; source_path             = dir
    }
end

module Projections_check = struct
  type t =
    { allow : string list
    ; output_result_to : string sexp_option
    }
  [@@deriving of_sexp]
end

module Executables_conf = struct
  type t = {
    (* Each element of [names] is an executable, without the ".exe" suffix. *)
    names : string list;
    (* This is meant as a temporary workaround, don't start using it. *)
    wrapped : bool [@default true];
    link_executables : bool [@default true];
    projections_check : Projections_check.t sexp_option;
    allowed_ldd_dependencies : Ordered_set_lang.t sexp_option;
    extra_disabled_warnings : int sexp_list;
    flags : Ordered_set_lang.t sexp_option;
    ocamlc_flags : Ordered_set_lang.t sexp_option;
    ocamlopt_flags : Ordered_set_lang.t sexp_option;
    (* Immediate dependencies.  It is not necessary to name transitive dependencies or
       modules defined in the same directory.  (For the latter, see [modules] below.) *)
    libraries : Libdep_name.t sexp_list;
    preprocess : Preprocess_specs.t [@default preprocess_default];
    preprocessor_deps : Dep_conf.t sexp_list;
    lint : Lint_spec.t sexp_option;
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
    pps : PP.t sexp_list;
  } [@@deriving of_sexp]
end

module Unified_tests = struct
  (* We stick with the default from run-tests.py (not that we have to). *)
  let default_timeout = Time.Span.of_sec 180.

  (* Jenga will generate a "run-unified-tests" script that can be used to manually run
     your tests. [setup_script] will be sourced before running the tests. The tests must
     be named test-XXX.t  *)
  type t =
    { target : string sexp_option
    ; deps : Dep_conf.t list
    ; timeout : Time.Span.t [@default default_timeout]
    ; setup_script : String_with_vars.t sexp_option
    ; sandbox : Sandbox_conf.t [@default Sandbox_conf.hardlink]
    ; uses_catalog : Uses_catalog.t [@default No]
    }
  [@@deriving of_sexp]

  let target t = Option.value t.target ~default:"runtest"

  let validate ~sexp t =
    validate_test_timeout ~sexp
      t.target ~alias_field_name:"target"
      (Some t.timeout) ~default_timeout
  ;;

  let t_of_sexp sexp =
    let t = t_of_sexp sexp in
    validate ~sexp t;
    t

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

  type ascii_string = string

  let ascii_string_of_sexp sexp =
    let s = string_of_sexp sexp in
    if String.exists s ~f:(fun ch -> Int.(>=) (Char.to_int ch) 128) then
      of_sexp_error
        "This field must contain only ASCII characters"
        sexp;
    s

  type t =
    { dirs                : (string * Import.Path.t) list
    ; copyright_start     : int
    (* Additional files to copy. They must be from one of the directory in [dirs]. This is
       for when we want to export a generated file. *)
    ; additional_files    : Import.Path.t sexp_list
    (* Extra items to install. The filenames are relative to the external repository. *)
    ; synopsis            : ascii_string
    ; description         : ascii_string
    ; deps                : Dep_conf.t sexp_list
    ; hooks               : String_with_vars.t T.Package.Hooks.t
                              [@default T.Package.Hooks.none]
    }
  [@@deriving of_sexp]
end

module Html_conf = struct
  type t =
    { orgs : string list;
      css : string sexp_option; }
  [@@deriving of_sexp]
end

module Provides_conf = struct
  type t =
    { name : Artifact_name.t
    ; file : string
    }

  let t_of_sexp : Sexp.t -> t = function
    | Atom s ->
      { name = Artifact_name.of_string s
      ; file =
          match String.lsplit2 s ~on:':' with
          | None        -> s
          | Some (_, s) -> s
      }
    | List [Atom s; List [Atom "file"; Atom file]] ->
      { name = Artifact_name.of_string s
      ; file
      }
    | sexp ->
      of_sexp_error "[<name>] or [<name> (file <file>)] expected" sexp
end

module Enforce_style_conf = struct
  type t =
    { exceptions : String.Set.t [@default String.Set.empty] }
  [@@deriving of_sexp]
end

module Wikipub_conf = struct
  (* These stanzas should be used in directories containing wikipub documents. They define
     what documents wikipub should upload to the wiki. *)
  type sources =
    (* Upload exactly the listed files, which should be in the local directory. *)
    [ `Files of string list
    (* Upload all files in this directory which wikipub knows how to process. *)
    | `Standard_formats
    ] [@@deriving of_sexp]

  type t =
    (* [Preview_subtree _] should be used in [jenga/start/jbuild], e.g.,
       (wikipub (Preview_subtree "${ROOT}/app/wikipub/doc"))
       It instructs jenga to upload the documents to the user's personal space in the dev
       wiki. It must be used in conjunction with [wikipub_sources].

       2017-03-28: Wikipub doesn't support partial upload yet, so in fact `Preview_subtree
       causes a full upload every time regardless of which files changed.
    *)
    [ `Preview_subtree of String_with_vars.t
    | `Upload_to of string
    | sources
    ] [@@deriving of_sexp]
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
  | `embed of Embed_conf.t
  | `jane_script of Jane_script_conf.t
  | `compile_c of Compile_c_conf.t
  | `rule of Rule_conf.t
  | `alias of Alias_conf.t
  | `no_utop
  | `unified_tests of Unified_tests.t
  | `toplevel_expect_tests of Toplevel_expect_tests.t
  | `public_repo of Public_repo.t
  | `html of Html_conf.t
  | `provides of Provides_conf.t sexp_list

  (* [enforce_style] opts in the [jbuild]'s directory to the requirement that (some of)
     the directory's files are correctly styled according to [bin/apply-style].
     Currently, style is enforced only for OCaml files ([*.ml], [*.mli]) and the only
     enforcement is that the indentation agrees with ocp-indent.  We may add additional
     file types and additional styling in the future.  If an enforced file is not styled
     correctly, this causes a build-time error; one must either style the file (using
     [apply-style]) or opt out of the requirement by listing the file in [exceptions].
     [enforce_style] writes the list of enforced files to [.files-to-style], which can be
     used by tools other than jenga, e.g. an Emacs [after-save-hook], to automatically
     run [apply-style]. *)
  | `enforce_style of Enforce_style_conf.t
  | `wikipub of Wikipub_conf.t
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

(* Filter out (public_release ...) forms and inline (js_only ...) ones. Externally, we do
   the opposite.

   The type of the contents of the (public_release ...) forms is in
   app/jbuilder/src/jbuild_types.ml *)
let filtered_of_sexp t_of_sexp sexp =
  (* To recover the location of a sub-sexp, we do a search using physical equality in
     [sexp]. Since we modify the input sexp, we must keep track of all the
     replacements. This is only used in case of error. *)
  let substs = ref [] in
  let rec filter_sexp (sexp : Sexp.t) =
    match sexp with
    | Atom _ -> sexp
    | List l ->
      let new_l = filter_sexps l in
      if phys_equal new_l l then
        sexp
      else begin
        let new_sexp = Sexp.List new_l in
        substs := (sexp, new_sexp) :: !substs;
        new_sexp
      end
  and filter_sexps (sexps : Sexp.t list) =
    match sexps with
    | [] -> []
    | head :: tail ->
      match head with
      | List (Atom ":public_release_only" :: _) ->
        filter_sexps tail
      | List (Atom ":js_only" :: l) ->
        l @ filter_sexps tail
      | _ ->
        let new_head = filter_sexp  head in
        let new_tail = filter_sexps tail in
        if phys_equal head new_head &&
           phys_equal tail new_tail then
          sexps
        else
          new_head :: new_tail
  in
  let new_sexps = filter_sexps [sexp] in
  try
    List.map new_sexps ~f:t_of_sexp
  with Sexp.Of_sexp_error (exn, sub) ->
    let sub =
      List.find_map !substs ~f:(fun (old_sexp, new_sexp) ->
        if phys_equal sub new_sexp then
          Some old_sexp
        else
          None)
      |> Option.value ~default:sub
    in
    raise (Sexp.Of_sexp_error (exn, sub))
;;
