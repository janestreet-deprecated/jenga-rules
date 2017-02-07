open! Core.Std
open! Import

let wikipub = Path.root_relative "app/wikipub/bin/main.exe"
let target ~dir ~base ~suf = relative ~dir (Filename.chop_extension base ^ suf)

let interpret_files ~dir files ~k =
  match files with
  | `Files l -> k l
  | `Standard_formats ->
    Scheme.glob (Glob.create ~dir "*.{org,md}") (fun l -> k (List.map ~f:Path.basename l))

let interpret_files_as_paths ~dir = function
  | `Files l -> return (List.map l ~f:(relative ~dir))
  | `Standard_formats -> Dep.glob_listing (Glob.create ~dir "*.{org,md}")

let rules_for_individual_files ~dir files =
  interpret_files ~dir files ~k:(fun files ->
    let rules =
      List.map files ~f:(fun file ->
        Rule.create
          ~targets:[ target ~dir ~base:file ~suf:".confluence_xml"
                   ; target ~dir ~base:file ~suf:".confluence_metadata" ]
          (Dep.all_unit [ Dep.path (relative ~dir file); Dep.path wikipub ]
           *>>| fun () ->
           Action.process ~dir (Path.reach_from ~dir wikipub) [ "compile-page"; file ]))
    in
    Scheme.rules rules
  )
;;

let final_target_base = ".wikipub"
let final_target ~dir = relative ~dir final_target_base
let rule_for_global_check ~dir ~all_files =
  let actual_rule =
    Rule.create ~targets:[final_target ~dir]
      (all_files
       *>>= fun all_files ->
       let all_metadata_files =
         List.map all_files ~f:(fun file ->
           target ~dir:(Path.dirname file) ~base:(Path.basename file)
             ~suf:".confluence_metadata")
       in
       Dep.all_unit (List.map (wikipub :: all_metadata_files) ~f:Dep.path)
       *>>| fun () ->
       Action.process ~dir (Path.reach_from ~dir wikipub)
         ("build-index"
          :: "-output" :: final_target_base
          :: List.map all_metadata_files ~f:(reach_from ~dir)))
  in
  let dot_hack = alias_dot_filename_hack ~dir final_target_base in
  [ actual_rule; dot_hack ]
;;
