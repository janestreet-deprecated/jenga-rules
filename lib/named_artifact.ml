open! Core
open Import
open Ocaml_types

module Store = struct
  type t = { artifacts: Path.t Ocaml_types.Artifact_name.Map.t
           ; findlib_packages : Path.t Findlib_package_name.Map.t
           }

  let create ~artifacts ~findlib_packages : t =
    let artifacts =
      Map.mapi (Artifact_name.Map.of_alist_multi artifacts)
        ~f:(fun ~key:name ~data ->
          match data with
          | [(data : Path.t)] -> data
          | _ ->
            failwithf !"Duplicate definition of artifact '%{Artifact_name}' in %s"
              name (String.concat ~sep:" and " (List.map data ~f:(fun a ->
                sprintf !"%{Path}" a))) ()) in
    { artifacts; findlib_packages }
end

(* Fallback in case the artifact is not found in the tree *)
type fallback =
  | PATH
  | In_findlib_package of
      Findlib_package_name.t *
      string (* File name inside the findlib package *)
  | No_fallback of string option  (* Optional reason for not having fallback *)
[@@deriving sexp, compare]

type t =
  { name     : Artifact_name.t
  ; fallback : fallback
  }
[@@deriving sexp_of, compare]

let name t = t.name

let binary name =
  { name     = Artifact_name.of_string name
  ; fallback = PATH
  }

let in_findlib name =
  match String.lsplit2 name ~on:':' with
  | None -> raise_s [%message [%here] "Named_artifact.in_findlib got name with ':'"
                                (name : string)]
  | Some (pkg, fname) ->
    { name     = Artifact_name.of_string name
    ; fallback = In_findlib_package (Findlib_package_name.of_string pkg, fname)
    }

let jane_street_only name =
  { name     = Artifact_name.of_string name
  ; fallback = No_fallback (Some "This file has not been publicly released by Jane Street.")
  }

let find_prog =
  let path =
    let path_sep = if Sys.os_type = "Win32" then ';' else ':' in
    match Sys.getenv "PATH" with
    | None -> []
    | Some s -> String.split s ~on:path_sep
  in
  let open Async in
  fun name ->
    Deferred.List.find_map path ~f:(fun dir ->
      let fn = dir ^/ name in
      Sys.file_exists_exn fn
      >>| function
      | true  -> Some (Path.absolute fn) (* relative paths in the PATH should be rare *)
      | false -> None)

let path {Store.artifacts; findlib_packages} { name; fallback } : Path.t Dep.t =
  match Artifact_name.Map.find artifacts name with
  | Some a -> Dep.return a
  | None ->
    if not Config.public
    then
      Dep.return () *>>| fun () ->
      failwithf
        !"%{Artifact_name} is not defined in the tree."
        name ()
    else begin
      match fallback with
      | PATH -> begin
          Dep.deferred (fun () -> find_prog (Artifact_name.to_string name))
          *>>| function
          | None ->
            failwithf
              !"Could not find program `%{Artifact_name}` in PATH"
              name ()
          | Some path -> path
        end
      | In_findlib_package (pkg, fname) ->
        begin match Map.find findlib_packages pkg with
        | None ->
          Dep.return () *>>| fun () ->
          if Option.is_none Config.findlib_conf_default
          then
            failwithf
              !"Could not find findlib package %{Findlib_package_name}. \
                Findlib is not enabled." pkg ()
          else
            failwithf
              !"Could not find findlib package `%{Findlib_package_name}`. \
                Try to install it with `opam install %{Findlib_package_name}`."
              pkg pkg ()
        | Some path -> Dep.return (Path.relative ~dir:path fname)
        end
      | No_fallback reason_opt ->
        Dep.return () *>>| fun () ->
        let reason = match reason_opt with
          | None   -> ""
          | Some r -> sprintf " %s" r
        in
        failwithf
          !"Could not find `%{Artifact_name}`.%s" name reason ()
    end


let find t name =
  if Artifact_name.Map.mem t.Store.artifacts name
  then Some {name; fallback = No_fallback None}
  else None
