open! Core
open! Import

(* We register this variable so that [jenga env set] can change it. This lets us push
   valid Kerberos credentials out to a running jenga process. *)
let krb5ccname = Var.register "KRB5CCNAME"

let wikipub    = Path.absolute "/j/office/app/wikipub/bin/2017-11-14_e54ae6b3c10a"
(* uncomment this for testing. *)
(* let () = ignore wikipub
 * let wikipub    = root_relative "app/wikipub/bin/main.exe" *)
let pandoc_query =
  Path.absolute "/j/office/app/pandoc/prod/pandoc-query/2017-05-22_4cac8ef0ca35/pandoc-query"
let template   = root_relative "app/wikipub/data/template.html"
let mime_types = root_relative ".wikipub-mime-types.sexp"

let global_metadata_target_base = ".wikipub"

let global_metadata_target ~dir = relative ~dir global_metadata_target_base

let runtime_deps_of_upload ~dir = Alias.create ~dir "wikipub-runtime-deps-of-upload"

let update_wiki_args ~dir ~mode ~space =
  [ "update-wiki"
  ; "-acknowledge-danger"
  ; "-space"     ; space
  ; "-mime-types"; Path.reach_from ~dir mime_types
  ; "-mode"      ; mode
  ; Path.reach_from ~dir (global_metadata_target ~dir)
  ]

let confluence_xml_suffix = ".confluence_xml"
let confluence_metadata_suffix = ".confluence_metadata"

let replace_extension_if_any path ~suf =
  let basename = Path.basename path in
  let without_ext = try Filename.chop_extension basename with _ -> basename in
  relative ~dir:(Path.dirname path) (without_ext ^ suf)
;;

let standard_formats_glob_string = "*.{org,md,mkd,mlt}"
let standard_formats_glob ~dir = Glob.create ~dir standard_formats_glob_string

let registered_files ~dir = function
  | `Standard_formats -> Dep.glob_listing (standard_formats_glob ~dir)
  | `Files basenames -> Dep.return (List.map basenames ~f:(Path.relative ~dir))
;;

let list_images ~dir file =
  let format =
    match snd (Filename.split_extension (Path.basename file)) with
    | Some "mkd" | Some "md" -> "--markdown"
    | Some "org" | Some "mlt" -> "--org"
    | None | Some _ ->
      raise_s [%sexp ("Wikipub only supports files matching "
                      ^ standard_formats_glob_string : string), ~~(file : Path.t)]
  in
  Dep.action_stdout (
    Dep.path file
    *>>| fun () ->
    Action.process ~dir ~env:[ "LC_ALL", "en_US.UTF-8" ]
      (Path.reach_from ~dir pandoc_query)
      [ "list-images"; format; Path.reach_from ~dir file ]
  ) *>>| fun images ->
  List.map (String.split_lines images) ~f:(Path.relative ~dir)
;;

let wikipub_sources ~dir wikipub_sources =
  let rule_for_individual_file file =
    let dir = Path.dirname file in
    Rule.create
      ~targets:[ replace_extension_if_any file ~suf:confluence_xml_suffix
               ; replace_extension_if_any file ~suf:confluence_metadata_suffix
               ]
      (Dep.all_unit
         [ Dep.path file
         ; Dep.path wikipub
         ; Dep.path template
         ; (list_images ~dir file *>>= Fn.compose Dep.all_unit (List.map ~f:Dep.path))
         ]
       *>>| fun () ->
       Action.process ~dir (Path.reach_from ~dir wikipub)
         [ "compile-page"; Path.reach_from ~dir file
         ; "-template"; reach_from ~dir template
         ])
  in
  match wikipub_sources with
  | `Files basenames ->
    Scheme.rules (List.map basenames ~f:(fun basename ->
      rule_for_individual_file (Path.relative ~dir basename)))
  | `Standard_formats ->
    Scheme.glob (standard_formats_glob ~dir) (fun paths ->
      Scheme.rules (List.map paths ~f:rule_for_individual_file))
;;

let rules_for_global_metadata ~dir ~registered_files =
  let action =
    registered_files
    *>>= fun registered_files ->
    let all_metadata_files =
      List.map registered_files ~f:(replace_extension_if_any ~suf:confluence_metadata_suffix)
    in
    Dep.all_unit (List.map ~f:Dep.path (wikipub :: all_metadata_files))
    *>>| fun () ->
    Action.process ~dir (Path.reach_from ~dir wikipub)
      ("build-index"
       :: "-output" :: Path.reach_from ~dir (global_metadata_target ~dir)
       :: List.map all_metadata_files ~f:(reach_from ~dir))
  in
  [ Rule.create ~targets:[global_metadata_target ~dir] action
  ; alias_dot_filename_hack ~dir global_metadata_target_base
  ; Rule.alias (runtime_deps_of_upload ~dir)
      [ (registered_files
         *>>= fun registered_files ->
         let xml_files =
           List.map registered_files
             ~f:(replace_extension_if_any ~suf:confluence_xml_suffix)
         in
         let paths = wikipub :: global_metadata_target ~dir :: mime_types :: xml_files in
         Dep.all_unit (List.map ~f:Dep.path paths))
      ]
  ]
;;

let rule_for_preview ~dir _preview_roots =
  let action =
    Dep.both
      (* 2017-03-28: Partial upload isn't implemented yet, so we ignore the specified
         preview paths and do a full upload whenever anything changes. *)
      (Dep.alias (runtime_deps_of_upload ~dir))
      (Dep.getenv krb5ccname)
    *>>| fun ((), krb5ccname) ->
    let env =
      Option.to_list (Option.map krb5ccname ~f:(fun v -> "KRB5CCNAME", v))
    in
    Action.process ~dir ~env
      (Path.reach_from ~dir wikipub)
      [ "preview"
      ; "-mime-types"; Path.reach_from ~dir mime_types
      ; Path.reach_from ~dir (global_metadata_target ~dir)
      ]
  in
  Rule.alias (Alias.create ~dir "wikipub-check") [ Dep.action action ]
;;

let upload_script_target ~dir = relative ~dir "wikipub-update-prod-wiki.sh"

let rule_for_upload_script ~dir ~to_wiki_space:space =
  Rule.write_string ~chmod_x:() ~target:(upload_script_target ~dir)
    (sprintf !"#!/bin/bash\n\n%{quote} %{concat_quoted}"
       (Path.reach_from ~dir wikipub)
       (update_wiki_args ~dir ~mode:"prod" ~space))
;;

let upload ~dir ~preview ~registered_files ~to_wiki_space =
  Scheme.rules
    (rule_for_preview ~dir preview
     :: rule_for_upload_script ~dir ~to_wiki_space
     :: rules_for_global_metadata ~dir ~registered_files)
