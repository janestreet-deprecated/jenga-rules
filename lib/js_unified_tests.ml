open Core
open Import

type t =
  { target : string
  ; deps : unit Dep.t list
  ; timeout : Time.Span.t
  ; setup_script : string option
  ; sandbox : Sandbox.t
  ; uses_catalog : Jbuild_types.Uses_catalog.t
  ; runtime_deps_alias : Alias.t
  }

let ocaml_style_varname = "UNIFIED_TESTS_OCAML_STYLE_ERRORS"
let ocaml_style = Var.peek_register_bool ocaml_style_varname
                    ~default:(Var.peek_register_bool "UNIFIED_TESTS_OMAKE_STYLE_ERRORS"
                                ~default:false)
let ascii_diffs_varname = "UNIFIED_TESTS_ASCII_DIFFS"
let ascii_diffs = Var.peek_register_bool ascii_diffs_varname ~default:false

let rules ~dir
      { target; deps; timeout; setup_script; sandbox; uses_catalog; runtime_deps_alias } =
  let script_basename = "run-unified-tests" in
  let script = Path.relative ~dir script_basename in
  let unified_tests_script, unified_tests_script_runtime_deps =
    let dir = Path.root_relative "external/unified-tests" in
    Path.relative ~dir "run-tests.sh", Alias.create ~dir "runtime-deps-of:run-tests.sh"
  in
  let run_the_tests =
    let { Action. prog; args; dir = dir' } =
      let prog = Path.reach_from ~dir unified_tests_script in
      let args =
        let timeout_in_sec = Float.iround_up_exn (Time.Span.to_sec timeout) in
        ["--timeout"; Int.to_string timeout_in_sec]
      in
      Catalog_sandbox.wrap uses_catalog ~can_assume_env_is_setup:false
        { prog; dir; args; }
    in
    assert (Path.(=) dir dir');
    sprintf !{|%{concat_quoted} "$@"|} (prog :: args)
  in
  let rule_to_create_script =
    Rule.create ~targets:[script]
      (return (
         Action.save
           ~chmod_x:()
           ~target:script
           (sprintf !"\
#!/bin/bash
set -e -u -o pipefail

export %s=%{Bool}
export %s=%{Bool}

%s
%s
"
              ocaml_style_varname ocaml_style
              ascii_diffs_varname ascii_diffs
              (match setup_script with
               | None -> "# no specified file to source"
               | Some setup_script -> sprintf !"source %{quote}" setup_script)
              run_the_tests)))
  in
  let create_script_by_default =
    Rule.alias (Alias.create ~dir "DEFAULT") [ Dep.path script ]
  in
  let common_runtime_deps =
    List.concat
      [ [ Dep.path script
        ; Dep.alias unified_tests_script_runtime_deps
        ]
      ; (match setup_script with
         | None -> []
         | Some setup_script -> [Dep.path (Path.relative ~dir setup_script)])
      ; deps
      ; Catalog_sandbox.deps uses_catalog
      ]
  in
  let rule_of_runtime_deps_alias =
    Rule.alias runtime_deps_alias common_runtime_deps
  in
  let rule_to_run_script =
    Rule.alias (Alias.create ~dir target) [
      Dep.glob_listing (Glob.create ~dir "*.t") *>>= fun t_files ->
      Dep.all_unit
        (List.map t_files ~f:(fun t_file ->
           let test_name = Path.basename t_file in
           ((* this reflects the weird behaviour in
               external/unified-tests/run-tests.py, l.837 (rev. 881875aaa08f)*)
             if not (String.is_prefix ~prefix:"test-" test_name)
                || String.contains test_name '~'
             then
               failwithf
                 !"unified-test %{quote} doesn't obey test naming rule: test-[^~]*\\.t"
                 test_name ());
           Dep.action
             (Dep.all_unit (Dep.path t_file :: common_runtime_deps)
              *>>| fun () ->
              Action.process ~sandbox ~dir ("./" ^ script_basename) [ test_name ])))
    ]
  in
  [ rule_to_create_script
  ; create_script_by_default
  ; rule_to_run_script
  ; rule_of_runtime_deps_alias
  ]
;;
