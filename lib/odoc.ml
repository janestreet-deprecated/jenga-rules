open Core.Std
open Import

open Ocaml_types

let odoc_path =
  let major_version =
    match String.rsplit2 ~on:'-' Compiler_selection.major_version with
    | None -> Compiler_selection.major_version
    | Some (s, _) -> s
  in
  sprintf "/j/office/app/codoc/dev/%s/odoc%s" major_version
    Compiler_selection.odoc_minor_version

let alias ~dir = Alias.create ~dir "doc"

let odoc_output_dir = root_relative ".odoc"
let html_output_dir = root_relative ".odoc_html"

let dash_Is ~dir dirs =
  List.concat_map dirs ~f:(fun path -> ["-I"; Path.reach_from ~dir path])

let odoc_compile_rules ~dir ~search_paths ~libname ~remote_dir =
  Dep.both
    (Dep.glob_listing (Glob.create ~dir:remote_dir "*.cmt"))
    (Dep.glob_listing (Glob.create ~dir:remote_dir "*.cmti"))
  *>>| fun (cmts, cmtis) ->
  let bn_to_path =
    (* [cmts @ cmtis] ensures that we always keep the path to the cmti in the
       map when both a .cmt and .cmti is present. *)
    List.fold (cmts @ cmtis) ~init:String.Map.empty ~f:(fun bns path ->
      let str_name = Filename.chop_extension (Path.basename path) in
      let bn = PN.of_string str_name in
      Map.add bns ~key:(PN.to_module bn) ~data:(bn, path)
    )
  in
  let targets, rules =
    Map.fold bn_to_path ~init:([], []) ~f:(
      fun ~key:current_module ~data:(_, path) (targets, rules) ->
        let target = Filename.chop_extension (Path.basename path) ^ ".odoc" in
        let target = Path.relative ~dir target in
        let rule =
          Rule.create ~targets:[target] (
            Dep.action_stdout
              (Dep.path path *>>| fun () ->
               Action.process ~dir odoc_path ["compile-deps"; Path.reach_from ~dir path])
            *>>= fun deps_stdout ->
            let deps =
              Dep.path path ::
              List.filter_map (String.split_lines deps_stdout) ~f:(fun line ->
                let this_module, digest_hex = String.lsplit2_exn line ~on:' ' in
                if current_module = this_module
                then None
                else
                  let _digest = Digest.from_hex digest_hex in
                  match Map.find bn_to_path this_module with
                  | None -> None
                  | Some (bn, _) -> Some (Dep.path @@ PN.suffixed ~dir bn ".odoc")
              )
            in
            Dep.all_unit
              (List.concat
                 [ deps
                 ; List.map search_paths ~f:(fun dir -> Dep.alias (alias ~dir))
                 ])
            *>>| fun () ->
            Action.process ~ignore_stderr:true ~dir odoc_path (
              [ "compile"; "--pkg"; LN.to_string libname; "-o"; "."]
              @ dash_Is ~dir (dir :: search_paths)
              @ [Path.reach_from ~dir path]
            )
          )
        in
        target :: targets, rule :: rules
    )
  in
  Rule.alias (alias ~dir) (List.map targets ~f:Dep.path) :: rules

let link_targets_file_of_input ~dir input =
  let basename = Filename.chop_extension (Path.basename input) in
  Path.relative ~dir (basename ^ ".odoc-targets")

let link_deps ~dir path =
  Dep.path path
  *>>| fun () ->
  Action.process ~dir odoc_path ["link-deps"; Path.reach_from ~dir path]

let link_rules_deps ~dir ~inputs_as_module_map ~search_paths input =
  Dep.action_stdout (link_deps ~dir input)
  *>>= fun out ->
  let unit_deps = String.split_lines out in
  let deps =
    Dep.path input ::
    List.filter_map unit_deps ~f:(fun this_module ->
      match Map.find inputs_as_module_map this_module with
      | None -> None
      | Some path -> Some (Dep.path path))
  in
  let common_deps =
    List.concat
      [ deps
      ; List.map search_paths ~f:(fun dir -> Dep.alias (alias ~dir))
      ]
  in
  Dep.all_unit common_deps

let odoc_link_targets_rule ~dir ~search_paths ~inputs_as_module_map ~odoc_dir input =
  let target = link_targets_file_of_input ~dir input in
  Rule.create ~targets:[target] (
    Dep.action_stdout (
      (* might be too many dependencies, but at least we know we're not missing any
         since actually producing the html files requires no more than this *)
      link_rules_deps ~dir ~inputs_as_module_map ~search_paths input
      *>>| fun () ->
      Action.process ~ignore_stderr:true ~dir odoc_path (
        ["html-targets"]
        @ dash_Is ~dir (odoc_dir :: search_paths)
        @ ["-o"; "."; Path.reach_from ~dir input]
      )
    )
    *>>| fun out ->
    Action.save out ~target
  )

let odoc_link_rules ~dir ~search_paths ~libname =
  let odoc_dir = Path.relative ~dir:odoc_output_dir (LN.to_string libname) in
  Dep.glob_listing (Glob.create ~dir:odoc_dir "*.odoc")
  *>>| fun inputs ->
  let inputs_as_module_map =
    String.Map.of_alist_exn
      (List.map inputs ~f:(fun path ->
         String.capitalize (Filename.chop_extension (Path.basename path)), path))
  in
  let targets, rules =
    List.fold inputs ~init:([], []) ~f:(fun (archives, rules) input ->
      let archive = Filename.chop_extension (Path.basename input) ^ ".tgz" in
      let rule =
        Rule.create ~targets:[Path.relative ~dir archive] (
          let targets_file = link_targets_file_of_input ~dir input in
          Dep.both
            (link_rules_deps ~dir ~inputs_as_module_map ~search_paths input)
            (Dep.path targets_file)
          *>>| fun ((), ()) ->
          bashf ~ignore_stderr:true ~dir
            !"%{quote} html %{concat_quoted} -o . %{quote} && tar -czf %{quote} -T %{quote}"
            odoc_path
            (dash_Is ~dir (odoc_dir :: search_paths))
            (reach_from ~dir input)
            archive
            (reach_from ~dir targets_file)
        )
      in
      (Path.relative ~dir archive :: archives,
       odoc_link_targets_rule ~dir ~search_paths ~inputs_as_module_map ~odoc_dir
         input :: rule :: rules)
    )
  in
  let css_target, copy_css =
    let target = Path.relative ~dir "odoc.css" in
    let rule =
      Rule.simple ~targets:[target] ~deps:[]
        ~action:(Action.process ~dir odoc_path [ "css"; "-o"; "." ])
    in
    target, rule
  in
  Rule.alias (alias ~dir) (List.map (css_target :: targets) ~f:Dep.path)
  :: copy_css :: rules

let generate_http_server ~src_dir ~odoc_html_dir libname =
  let script =
    sprintf !{|#!/bin/bash
cd "$(dirname "${BASH_SOURCE[0]}")"/%{quote}
python -m SimpleHTTPServer ${1-8000} &
server_pid=$!

function kill_server() {
  kill $server_pid 2> /dev/null
  exit 0
}

trap kill_server INT
echo "Go to http://$(hostname):${1-8000}/%s/%s"
while true; do
  sleep 10000
done
|}
      (Path.reach_from ~dir:src_dir html_output_dir)
      (LN.to_string libname)
      (LN.to_module libname)
  in
  let target = Path.relative ~dir:src_dir "serve_doc" in
  target, Rule.create ~targets:[target] (
    Dep.alias (alias ~dir:odoc_html_dir)
    *>>| fun () ->
    Action.save ~chmod_x:() script ~target
  )

let setup ~dir ~lib_in_the_tree:(lib:Lib_in_the_tree.t) ~lib_deps step =
  lib_deps *>>= fun libraries ->
  let libraries =
    List.filter_map libraries ~f:(function
      | Lib_dep.In_the_tree lib -> Some lib.name
      | From_compiler_distribution _ | Findlib_package _ -> None
    )
  in
  let search_paths =
    List.map libraries ~f:(fun lib ->
      Path.relative ~dir:odoc_output_dir (LN.to_string lib))
  in
  match step with
  | `Compile ->
    odoc_compile_rules ~dir ~search_paths ~libname:lib.name ~remote_dir:lib.source_path
  | `Link ->
    let html_aliases_for_deps =
      List.map libraries ~f:(fun l ->
        Dep.alias
          (alias ~dir:(Path.relative ~dir:html_output_dir (LN.to_string l)))
      )
    in
    odoc_link_rules ~dir ~search_paths ~libname:lib.name
    *>>| fun rules ->
    Rule.alias (alias ~dir) html_aliases_for_deps :: rules
