open Core
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
  let search_path_deps =
    Dep.memoize ~name:"search-path-deps"
      (Dep.group_dependencies
         (Dep.all_unit
            (List.map search_paths ~f:(fun dir -> Dep.alias (alias ~dir)))))
  in
  let targets, rules =
    Map.fold bn_to_path ~init:([], []) ~f:(
      fun ~key:current_module ~data:(_, path) (targets, rules) ->
        let target_basename = Filename.chop_extension (Path.basename path) ^ ".odoc" in
        let target = Path.relative ~dir target_basename in
        let rule =
          Rule.create ~targets:[target] (
            Dep.action_stdout (
              Dep.path path *>>| fun () ->
              bashf ~ignore_stderr:true ~dir
                !"%{quote} compile-deps %{quote} | { grep \"^%s\" || true; }"
                odoc_path (Path.reach_from ~dir path) (LN.to_module libname)
            )
            *>>= fun deps_stdout ->
            let deps =
              Dep.path path ::
              List.filter_map (String.split_lines deps_stdout) ~f:(fun line ->
                let this_module, digest_hex = String.lsplit2_exn line ~on:' ' in
                if current_module = this_module then
                  None
                else
                  let _digest = Digest.from_hex digest_hex in
                  match Map.find bn_to_path this_module with
                  | None -> None
                  | Some (bn, _) -> Some (Dep.path @@ PN.suffixed ~dir bn ".odoc")
              )
            in
            Dep.all_unit (search_path_deps :: deps)
            *>>| fun () ->
            (* We run the process from the parent directory as the file will be
               written in a "pkg" subdirectory of the directory where odoc is
               run from. *)
            Action.process ~ignore_stderr:true ~dir:(Path.dirname dir) odoc_path (
              [ "compile"; "--pkg"; LN.to_string libname ]
              @ dash_Is ~dir:(Path.dirname dir) (dir :: search_paths)
              @ [ "-o"; Path.reach_from ~dir:(Path.dirname dir) target
                ; Path.reach_from ~dir:(Path.dirname dir) path ]
            )
          )
        in
        target :: targets, rule :: rules
    )
  in
  Rule.alias (alias ~dir) (List.map targets ~f:Dep.path) ::
  rules

let html_targets_file_of_input ~dir input =
  let basename = Filename.chop_extension (Path.basename input) in
  Path.relative ~dir (basename ^ ".odoc-targets")

let html_deps ~dir path =
  Dep.path path
  *>>| fun () ->
  Action.process ~ignore_stderr:true ~dir odoc_path ["html-deps"; Path.reach_from ~dir path]

let _html_rules_deps ~dir ~search_paths input =
  Dep.action_stdout (html_deps ~dir input)
  *>>| fun out ->
  let package_deps = String.(Set.of_list @@ split_lines out) in
  List.unzip @@ List.filter_map search_paths ~f:(fun dir ->
    if Set.mem package_deps (basename dir) then
      Some (dir, Dep.alias (alias ~dir))
    else
      None
  )

let _odoc_html_targets_rule ~dir ~search_paths input =
  let target = html_targets_file_of_input ~dir input in
  Rule.create ~targets:[target] (
    Dep.action_stdout (
      (* might be too many dependencies, but at least we know we're not missing any
         since actually producing the html files requires no more than this *)
      _html_rules_deps ~dir ~search_paths input
      *>>= fun (include_paths, aliases) ->
      let dir = dirname dir in
      Dep.all_unit aliases
      *>>| fun () ->
      Action.process ~ignore_stderr:true ~dir odoc_path (
        ["html-targets"]
        @ dash_Is ~dir include_paths
        @ ["-o"; "."; Path.reach_from ~dir input]
      )
    )
    *>>| fun out ->
    Action.save out ~target
  )

let mld_file_rule ~dir ~lib:(lib : Lib_in_the_tree.t) inputs_as_module_map =
  let sln = LN.to_module lib.name in
  let mld_filename = LN.to_string lib.name ^ ".mld" in
  let target = Path.relative ~dir mld_filename in
  let generate () =
    let mld_file_content, deps =
      if Map.mem inputs_as_module_map (sln ^ "__") then (
        (* [Foo__] present means [Foo] was written by the user, link to that. *)
        sprintf "The entry point for this library is module {!module:%s}." sln,
        [Map.find_exn inputs_as_module_map sln]
      ) else
        match Map.find inputs_as_module_map (sln ^ "__Std") with
        | Some path ->
          sprintf "The entry point for this library is module {!module:%s.Std}." sln,
          [path]
        | None ->
          sprintf "This library doesn't follow the usual convention.\n\
                   Here are the modules it exposes: {!modules: %s}"
            (String.concat ~sep:" " (Map.keys inputs_as_module_map)),
          Map.data inputs_as_module_map
    in
    let mld_file_content =
      sprintf "{%%html:<nav><a href=\"..\">Up</a></nav>%%}\n\
               {1 Library %s}\n%s"
        sln mld_file_content
    in
    let rule =
      Rule.simple ~targets:[target] ~deps:[]
        ~action:(Action.save mld_file_content ~target)
    in
    target, rule, deps
  in
  let copy source_mld_file =
    let deps = Map.data inputs_as_module_map in
    let rule =
      Rule.create ~targets:[target] (
        Dep.contents source_mld_file
        *>>| fun mld_file_content ->
        let mld_file_content =
          sprintf "{%%html:<nav><a href=\"..\">Up</a></nav>%%}\n%s\n"
            mld_file_content
        in
        Action.save mld_file_content ~target
      )
    in
    target, rule, deps
  in
  let source_mld_file = Path.relative ~dir:lib.source_path mld_filename in
  Dep.file_exists source_mld_file
  *>>| function
  | true  -> copy source_mld_file
  | false -> generate ()

let odoc_html_rules ~dir ~search_paths (lib : Lib_in_the_tree.t) =
  let odoc_dir = Path.relative ~dir:odoc_output_dir (LN.to_string lib.name) in
  let search_paths = odoc_dir :: search_paths in
  Dep.glob_listing (Glob.create ~dir:odoc_dir "*.odoc")
  *>>= fun inputs ->
  let inputs_as_module_map =
    String.Map.of_alist_exn
      (List.map inputs ~f:(fun path ->
         String.capitalize (Filename.chop_extension (Path.basename path)), path))
  in
  let search_paths_deps =
    Dep.memoize ~name:"search-paths-deps"
      (Dep.group_dependencies
         (Dep.all_unit
            (List.map search_paths ~f:(fun dir -> Dep.alias (alias ~dir)))))
  in
  let targets, rules =
    List.fold inputs ~init:([], []) ~f:(fun (archives, rules) input ->
      let archive = Filename.chop_extension (Path.basename input) ^ ".tgz" in
      let archive_path = Path.relative ~dir archive in
      let input_html_dir_path =
        Filename.chop_extension (Path.basename input)
        |> String.capitalize
        |> Path.relative ~dir
      in
      let rule =
        Rule.create ~targets:[archive_path] (
          (*
          html_rules_deps ~dir ~search_paths input
          *>>= fun (include_paths, aliases) ->
          *)
          search_paths_deps
          *>>| fun () ->
          let dir = dirname dir in
          bashf ~ignore_stderr:true ~dir
            !"%{quote} html %{concat_quoted} -o . %{quote} \
              --closed-details && tar -czf %{quote} %{quote}"
            odoc_path
            (dash_Is ~dir search_paths)
            (reach_from ~dir input)
            (reach_from ~dir archive_path)
            (* (reach_from ~dir targets_file) *)
            (reach_from ~dir input_html_dir_path)
        )
      in
      (archive_path :: archives,
       (* odoc_html_targets_rule ~dir ~search_paths ~odoc_dir input :: *)
       rule :: rules)
    )
  in
  let css_target = Path.relative ~dir:(dirname dir) "odoc.css" in
  let index_target = Path.relative ~dir "index.html" in
  mld_file_rule ~dir ~lib inputs_as_module_map
  *>>| fun (mld_file, mld_rule, mld_deps) ->
  let index_rule =
    Rule.simple ~targets:[index_target]
      ~deps:(List.map ~f:Dep.path (mld_file :: mld_deps))
      ~action:(
        Action.process ~ignore_stderr:true ~dir:(Path.dirname dir) odoc_path (
          [ "html"; "-o"; "."]
          @ (dash_Is ~dir:(Path.dirname dir) search_paths)
          @ [ "--index-for"; Path.basename dir
            ; Path.reach_from ~dir:(Path.dirname dir) mld_file
            ]
        )
      )
  in
  Rule.alias (alias ~dir)
    (List.map (css_target :: index_target :: targets) ~f:Dep.path)
  :: index_rule
  :: mld_rule
  :: rules

let copy_css_rule ~dir =
  let target = Path.relative ~dir "odoc.css" in
  Rule.simple ~targets:[target] ~deps:[]
    ~action:(Action.process ~ignore_stderr:true ~dir odoc_path [ "css"; "-o"; "." ])

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
  | `Html ->
    let html_aliases_for_deps =
      List.map libraries ~f:(fun l ->
        Dep.alias
          (alias ~dir:(Path.relative ~dir:html_output_dir (LN.to_string l)))
      )
    in
    odoc_html_rules ~dir ~search_paths lib
    *>>| fun rules ->
    Rule.alias (alias ~dir) html_aliases_for_deps :: rules
