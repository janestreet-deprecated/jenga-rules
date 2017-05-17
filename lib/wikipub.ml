open! Core
open! Import

(* We register this variable so that [jenga env set] can change it. This lets us push
   valid Kerberos credentials out to a running jenga process. *)
let krb5ccname = Var.register "KRB5CCNAME"

let wikipub    = Path.absolute "/j/office/app/wikipub/bin/2017-05-12_3b1ac2b3307b"
(* uncomment this for testing. *)
(* let () = ignore wikipub
 * let wikipub    = root_relative "app/wikipub/bin/main.exe" *)
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

let standard_formats_glob ~dir = Glob.create ~dir "*.{org,md,mkd}"

let registered_files ~dir = function
  | `Standard_formats -> Dep.glob_listing (standard_formats_glob ~dir)
  | `Files basenames -> Dep.return (List.map basenames ~f:(Path.relative ~dir))
;;

let wikipub_sources ~dir wikipub_sources =
  let rule_for_individual_file file =
    let dir = Path.dirname file in
      Rule.create
        ~targets:[ replace_extension_if_any file ~suf:confluence_xml_suffix
                 ; replace_extension_if_any file ~suf:confluence_metadata_suffix
                 ]
        (Dep.all_unit [ Dep.path file
                      ; Dep.path wikipub
                      ; Dep.path template
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

(* list has parent before grandparent *)
let ancestors_with_global_metadata ~dir =
  Sequence.unfold ~init:(Some dir) ~f:(Option.map ~f:(fun dir ->
    if Path.(=) Path.the_root dir
    then (dir, None)
    else (dir, Some (Path.dirname dir))))
  |> Sequence.to_list
  |> Dep.List.concat_map ~f:(fun dir ->
    Dep.glob_listing (Glob.create ~dir global_metadata_target_base)
    *>>| function
    | _ :: _ -> [dir]
    | [] -> [])
;;

let preview ~dir ~preview_root =
  let action =
    ancestors_with_global_metadata ~dir:preview_root
    *>>= function
    | [] -> raise_s [%sexp ~~(preview_root : Path.t), "is not under any Upload_to"]
    | preview_root :: _ ->
      Dep.both
        (* 2017-03-28: Partial upload isn't implemented yet, so we ignore the specified
           preview paths and do a full upload whenever anything changes. *)
        (Dep.alias (runtime_deps_of_upload ~dir:preview_root))
        (Dep.getenv krb5ccname)
      *>>| fun ((), krb5ccname) ->
      let env =
        Option.to_list (Option.map krb5ccname ~f:(fun v -> "KRB5CCNAME", v))
      in
      Action.process ~dir ~env
        (Path.reach_from ~dir wikipub)
        [ "preview"
        ; "-mime-types"; Path.reach_from ~dir mime_types
        ; Path.reach_from ~dir (global_metadata_target ~dir:preview_root)
        ]
  in
  [ Rule.alias (Alias.create ~dir "wikipub-check") [ Dep.action action ] ]
  |> Scheme.rules
;;

let upload_script_target ~dir = relative ~dir "wikipub-update-prod-wiki.sh"

let rule_for_upload_script ~dir ~to_wiki_space:space =
  Rule.write_string ~chmod_x:() ~target:(upload_script_target ~dir)
    (sprintf !"#!/bin/bash\n\n%{quote} %{concat_quoted}"
       (Path.reach_from ~dir wikipub)
       (update_wiki_args ~dir ~mode:"prod" ~space))
;;

let upload ~dir ~registered_files ~to_wiki_space =
  Scheme.rules
    (rule_for_upload_script ~dir ~to_wiki_space
     :: rules_for_global_metadata ~dir ~registered_files)
