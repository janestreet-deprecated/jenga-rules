open! Core.Std
open! Import

let delete_eagerly =
  return (fun ~non_target ->
    match String.rsplit2 (Path.basename non_target) ~on:'.' with
    | Some (_, ("cmi" | "cmx" | "cmxa" | "cma" | "cmo" | "exe" | "cmt" | "cmti"
               | "o" | "a")) -> true
    | _ -> false)
;;

let delete_if_depended_upon =
  Dep.fs_glob_listing (Glob.create ~dir:Path.the_root "*" ~kinds:[`Directory])
  *>>= fun dirs ->
  let ignore_or_not dir =
    Dep.fs_glob_listing (Glob.create ~dir "{.hg,.hgignore}")
    *>>= function
    | [_; _] ->
      Dep.action_stdout
        (Dep.source_if_it_exists (Path.relative ~dir ".hgignore")
         *>>| fun () ->
         (* silence hg's garbage output about locks *)
         Action.process ~ignore_stderr:true ~dir "hg" [ "debugignore" ])
      *>>| fun stdout ->
      let stdout = Option.value ~default:stdout (String.chop_suffix stdout ~suffix:"\n") in
      Some (Path.basename dir, Re2.Std.Re2.create_exn ("^" ^ stdout))
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
      Re2.Std.Re2.matches re path
  in
  fun ~non_target:path ->
    Path.is_descendant ~dir:Path.the_root path &&
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
        | Some re -> Re2.Std.Re2.matches re rest
        | None -> find_in_root path
;;
