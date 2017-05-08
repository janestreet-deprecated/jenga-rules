open! Core
open! Import

let delete_eagerly =
  return (fun ~non_target ->
    match String.rsplit2 (Path.basename non_target) ~on:'.' with
    | Some (_, ("cmi" | "cmx" | "cmxa" | "cma" | "cmo" | "exe" | "cmt" | "cmti"
               | "o" | "a")) -> true
    | _ -> false)
;;

let raise_located ~hgignore msg =
  Located_error.raise
    ~loc:{ source = File hgignore; line = 1; start_col = 0; end_col = 0 }
    msg
;;

let delete_if_depended_upon =
  Dep.fs_glob_listing (Glob.create ~dir:Path.the_root "*" ~kinds:[`Directory])
  *>>= fun dirs ->
  let ignore_or_not dir =
    Dep.fs_glob_listing (Glob.create ~dir "{.hg,.hgignore}")
    *>>= function
    | [_; _] ->
      let hgignore = Path.relative ~dir ".hgignore" in
      Dep.action_stdout
        (Dep.source_if_it_exists hgignore
         *>>| fun () ->
         (* silence hg's garbage output about locks *)
         Action.process ~ignore_stderr:true ~dir "hg" [ "debugignore" ])
      *>>| fun stdout ->
      let stdout = Option.value ~default:stdout (String.chop_suffix stdout ~suffix:"\n") in
      (* avoid considering everything is ignored if the hgignore is empty *)
      let stdout = if String.is_empty stdout then "a^" else stdout in
      let re =
        try Re.compile (Re_pcre.re ("^" ^ stdout))
        with e -> raise_located ~hgignore (Exn.to_string e)
      in
      begin
        let path_that_shouldnt_be_matched = "lib/foo/src/file.ml" in
        if Re.execp re path_that_shouldnt_be_matched
        then raise_located ~hgignore
               (sprintf "the hgignore seems to ignore way too many files, \
                         given that it matches %s (conflict markers maybe?)"
                  path_that_shouldnt_be_matched)
      end;
      Some (Path.basename dir, re)
    | _ -> return None
  in
  Dep.both
    (Dep.all (List.map dirs ~f:ignore_or_not))
    (ignore_or_not Path.the_root)
  *>>| fun (list, opt) ->
  let table = String.Table.of_alist_exn (List.filter_opt list) in
  let find_in_root path =
    match opt with
    | None -> false
    | Some (_, re) ->
      Re.execp re path
  in
  fun ~non_target:path ->
    let path = Path.to_string path in
    match path with
    | "jenga/start/jbuild" ->
      (* this file is special because it's both ignored by hg and a source file. *)
      false
    | _ ->
      match String.lsplit2 path ~on:'/' with
      | None -> find_in_root path
      | Some (main, rest) ->
        match Hashtbl.find table main with
        | Some re -> Re.execp re rest
        | None -> find_in_root path
;;
