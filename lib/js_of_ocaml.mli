(** Compilation from ocaml to javascript, using js_of_ocaml. This module supports both
    - compiling a single bytecode executable to javascript (using [rule])
    - compiling each cmo/cma to javascript (using [rule]), and linking them. *)
open! Import

val exe_suf : string
val cma_suf : string
val cmo_suf : string
val jsdeps_suf : string

(** [check_libs_exn ~dir ~required_by libs] checks that every lib in [libs] is compatible
    with JavaScript and raises if it is not the case. *)
val check_libs_exn : dir:Path.t -> required_by:string -> Ocaml_types.Lib_dep.t list -> unit

(** [rule_for_compilation_unit] creates the rule to compile a bytecode compilation unit to
    JavaScript using the js_of_ocaml compiler.

    - [sourcemap] build with sourcemap.
    - [devel]     build with development mode (eg: it enables pretty by default)
    - [flags]     list of flags to pass to js_of_ocaml (eg: --pretty)
    - [src]       bytecode unit (cmo, cma)
    - [target]    javascript target
*)
val rule_for_compilation_unit
  :  artifacts:Named_artifact.Store.t
  -> dir:Path.t
  -> sourcemap : bool
  -> devel : bool
  -> flags:string list
  -> src:Path.t
  -> target:Path.t
  -> Rule.t

(** [rules_for_executable] creates the rules to compile a JavaScript executable
    using the js_of_ocaml compiler.
    There exists two modes of compilation
    1) Whole program compilation, which uses a bytecode executable.
    2) Separate compilation, which uses individuals units compiled to javascript.
    Whole program compilation is slower to compile but generate faster and smaller code.

    - [separate_compilation] enable separate compilation.

    - [js_files]     list of extra javascript files.
    - [flags]        list of flags to pass to js_of_ocaml (eg: --pretty)

    - [libs_dep]     libraries to link. This is also used to determined
                     library specific javascript runtimes.
    - [compute_objs] other units to link

    - [devel]        build with development mode
    - [toplevel]     build a toplevel with the provided libraries in scope.
                     This will disable separate compilation.
    - [exe]          name of the byte code executable. (even for separate compilation)

    - [drop_test]    This will try to drop unit tests and benchmarks from the generated code.
                     It is most certainly only efficient for whole whole program compilation.
*)
val rules_for_executable
  :  artifacts:Named_artifact.Store.t
  -> dir:Path.t
  -> ocaml_where:Import.Path.t
  -> js_files:Path.t List.t
  -> libs_dep:Ocaml_types.Lib_dep.t List.t Dep.t
  -> compute_objs:(Path.t * Ocaml_types.PN.t) List.t Dep.t
  -> toplevel:(Ocaml_types.Lib_dep.t List.t * unit Dep.t list) option
  -> hg_version:Path.t option
  -> build_info:Path.t option
  -> separate_compilation:bool
  -> sourcemap:bool
  -> devel:bool
  -> exe:string
  -> drop_test_and_bench: bool
  -> path_to_ocaml_artifact:(lib_in_the_tree:Ocaml_types.LN.t -> suf:string -> Import.Path.t)
  -> flags:String.t List.t
  -> Rule.t List.t

val rule_for_library_jsdeps : dir:Path.t -> Ocaml_types.Libname.t -> Path.t list -> Rule.t

val dot_js_dir : Path.t

val setup_dot_js_dir
  :  artifacts:Named_artifact.Store.t
  -> sourcemap : bool
  -> devel : bool
  -> ocaml_where:Path.t
  -> Path.t
  -> Jenga_lib.Api.Scheme.t

val gen_html_for_inline_tests
  :  libname: Ocaml_types.Libname.t
  -> argv: string list
  -> drop_test: bool
  -> exe: string
  -> string
