open Core
open Import

let config =
  let ic = Unix.open_process_in "ocamlc.opt -config" in
  protect ~finally:(fun () ->
    Unix.close_process_in ic
    |> Unix.Exit_or_signal.or_error
    |> ok_exn)
    ~f:(fun () -> In_channel.input_lines ic)
  |> List.map ~f:(fun s -> Scanf.sscanf s "%[^:]: %s" (fun k v -> (k, v)))
  |> String.Map.of_alist_exn

let get_conf key = Map.find_exn config key
let get_bool key = match Map.find config key with
  | Some "true"   -> true
  | Some _ | None -> false


let major_version = get_conf "version"
let default_version = major_version

let compiler_bin_dir = Filename.dirname (get_conf "standard_runtime")
let compiler_stdlib_dir = get_conf "standard_library_default"
let compiler_dir = Filename.dirname compiler_bin_dir

let flambda = get_bool "flambda"
let with_frame_pointers = get_bool "with_frame_pointers"
let spacetime = get_bool "spacetime"

let m32 = List.mem ["i386"] (get_conf "architecture") ~equal:String.equal

let pa_macro_flags = []

let odoc_minor_version = ""

let vanilla_major_version = major_version
