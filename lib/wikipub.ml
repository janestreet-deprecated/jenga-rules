open! Core.Std
open! Import

let wikipub    = Path.absolute "/j/office/app/wikipub/bin/2017-03-30_2b80d050a62d"
let template   = root_relative "app/wikipub/data/template.html"
let mime_types = root_relative ".wikipub-mime-types.sexp"

let update_wiki_args ~dir ~mode ~space ~wikipub_file =
  [ "update-wiki"
  ; "-acknowledge-danger"
  ; "-space"; space
  ; "-mode"; mode
  ; "-mime-types"; Path.reach_from ~dir mime_types
  ; wikipub_file
  ]

let target ~dir ~base ~suf = relative ~dir (Filename.chop_extension base ^ suf)

let interpret_files ~dir files ~k =
  match files with
  | `Files l -> k l
  | `Standard_formats ->
    Scheme.glob (Glob.create ~dir "*.{org,md,mkd}") (fun l ->
      k (List.map ~f:Path.basename l))

let interpret_files_as_paths ~dir = function
  | `Files l -> return (List.map l ~f:(relative ~dir))
  | `Standard_formats -> Dep.glob_listing (Glob.create ~dir "*.{org,md,mkd}")

let rules_for_individual_files ~dir files =
  interpret_files ~dir files ~k:(fun files ->
    let rules =
      List.map files ~f:(fun file ->
        Rule.create
          ~targets:[ target ~dir ~base:file ~suf:".confluence_xml"
                   ; target ~dir ~base:file ~suf:".confluence_metadata" ]
          (Dep.all_unit [ Dep.path (relative ~dir file)
                        ; Dep.path wikipub
                        ; Dep.path template
                        ]
           *>>| fun () ->
           Action.process ~dir (Path.reach_from ~dir wikipub)
             [ "compile-page"; file; "-template"; reach_from ~dir template ]
          ))
    in
    Scheme.rules rules
  )
;;

let derived_files input_files ~suf =
  List.map input_files ~f:(fun file ->
    target ~dir:(Path.dirname file) ~base:(Path.basename file) ~suf)
;;

let final_target_base = ".wikipub"
let final_target = relative ~dir:Path.the_root final_target_base
let rule_for_global_check ~all_input_files =
  let dir = Path.the_root in
  let actual_rule =
    Rule.create ~targets:[final_target]
      (all_input_files
       *>>= fun all_input_files ->
       let all_metadata_files = derived_files all_input_files ~suf:".confluence_metadata" in
       Dep.all_unit (List.map (wikipub :: all_metadata_files) ~f:Dep.path)
       *>>| fun () ->
       Action.process ~dir (Path.reach_from ~dir wikipub)
         ("build-index"
          :: "-output" :: Path.reach_from ~dir final_target
          :: List.map all_metadata_files ~f:(reach_from ~dir)))
  in
  let dot_hack = alias_dot_filename_hack ~dir final_target_base in
  [ actual_rule; dot_hack ]
;;

let upload_script_target = relative ~dir:Path.the_root "wikipub-update-prod-wiki.sh"

let rule_for_upload_script =
  let dir = Path.the_root in
  Rule.write_string ~chmod_x:() ~target:upload_script_target
    (sprintf !"#!/bin/bash\n\n%{quote} %{concat_quoted}"
       (Path.reach_from ~dir wikipub)
       (update_wiki_args ~dir ~mode:"prod" ~space:"JD"
          ~wikipub_file:(Path.reach_from ~dir final_target)))
;;

let rules_for_the_root ~dir ~all_input_files =
  [%test_eq: Path.t] dir Path.the_root;
  rule_for_upload_script
  :: rule_for_global_check ~all_input_files
;;
