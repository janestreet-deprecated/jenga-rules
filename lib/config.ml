open Core
open Import

let putenv ~tmpdir:_ = []

let script_dir =
  let ic = Unix.open_process_in "ocamlfind query jenga-rules" in
  protect ~finally:(fun () ->
    Unix.close_process_in ic
    |> Unix.Exit_or_signal.or_error
    |> ok_exn)
    ~f:(fun () ->
      Path.absolute
        (match In_channel.input_line ic with
         | None -> Sys.getcwd ()
         | Some path -> path ^/ "scripts"))

let public = true

let command_lookup_path = `Extend

let findlib_conf_default = Some "()"

let path =
  let path_sep = if Sys.os_type = "Win32" then ';' else ':' in
  match Sys.getenv "PATH" with
  | None -> []
  | Some s -> String.split s ~on:path_sep

let find_prog name =
  List.find_map path ~f:(fun dir ->
    if String.is_empty dir
    then None
    else
      let fn = dir ^/ name in
      match Sys.file_exists fn with
      | `Yes -> Some fn
      | `No | `Unknown -> None)
  |> Option.value ~default:name

let hg_prog = find_prog "hg"
let git_prog = find_prog "git"
let nodejs_prog = find_prog "node"
let emacs_prog = find_prog "emacs"
let opam_prog = find_prog "opam"

let extra_jane_kernel_ppx = []

let public_release_build_servers_default = "()"
