open! Core.Std
open! Import

let wrapper_dir = Path.root_relative "app/catalog/wrapper/bin"
let wrapper_exe = relative ~dir:wrapper_dir "catalog_wrapper.exe"

let deps : Jbuild_types.Uses_catalog.t -> _ = function
  | Yes -> [ Dep.path wrapper_exe ]
  | Yes_but_exempt_from_sandboxing | No -> []
;;

(* This string is an invalid sexp in a special way - when present, it causes
   the error message printed by the catalog library to not display the
   usage information for CATALOG_CONFIG, and instead reports that
   something testing-related went wrong. *)
let invalid_sexp = "))invalid_sexp_must_use_wrapper"

let invalid_environment =
  (* By setting CATALOG_CONFIG to an invalid sexp, any program
     that tries to use the catalog library will end up having a
     [Shutdown.shutdown] invoked. *)
  [("CATALOG_CONFIG", Some invalid_sexp)]
;;

let wrap config (process : Action.process) ~can_assume_env_is_setup =
  match (config : Jbuild_types.Uses_catalog.t) with
  | No ->
    if can_assume_env_is_setup
    then process
    else { prog = "/usr/bin/env"
         ; args = (sprintf "CATALOG_CONFIG=%s" invalid_sexp
                   :: process.prog :: process.args)
         ; dir = process.dir
         }
  | Yes_but_exempt_from_sandboxing ->
    { prog = "/usr/bin/env"
    ; args = ("--unset=CATALOG_CONFIG" :: process.prog :: process.args)
    ; dir = process.dir
    }
  | Yes ->
    { prog = "/usr/bin/env"
    ; args =
        [ "--unset=CATALOG_CONFIG"
        ; reach_from ~dir:process.dir wrapper_exe
        ; "create-environment-and-run"
        ; "-new-directory-in-tmpdir"
        ; process.prog
        ; "--"
        ] @ process.args
    ; dir = process.dir
    }
