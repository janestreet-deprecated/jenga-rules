open Core.Std
open Import

let putenv = []

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

let command_lookup_path = `Extend

let findlib_conf_default = Some "()"

let path_sep = if Sys.os_type = "Win32" then ';' else ':'

let split_path str =
  let len = String.length str in
  let rec aux i =
    if i >= len then
      []
    else
      let j = Option.value (String.index_from str i path_sep) ~default:len in
      String.sub str i (j - i) :: aux (j + 1)
  in
  aux 0

let path =
  match Sys.getenv "PATH" with
  | None -> []
  | Some s -> split_path s

let find_prog name =
  List.find_map path ~f:(fun dir ->
    let fn = dir ^/ name in
    if Sys.file_exists fn = `Yes then
      Some fn
    else
      None)
  |> Option.value ~default:name

let hg_prog = find_prog "hg"
let git_prog = find_prog "git"

let extra_jane_kernel_ppx = []
