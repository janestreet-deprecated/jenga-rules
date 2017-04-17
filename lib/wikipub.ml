open! Core.Std
open! Import

(* We register this variable so that [jenga env set] can change it. This lets us push
   valid Kerberos credentials out to a running jenga process. *)
let krb5ccname = Var.register "KRB5CCNAME"

let wikipub    = Path.absolute "/j/office/app/wikipub/bin/2017-04-10_f58eda87aed3"
(* uncomment this for testing. *)
(* let () = ignore wikipub
 * let wikipub    = root_relative "app/wikipub/bin/main.exe" *)
let template   = root_relative "app/wikipub/data/template.html"
let mime_types = root_relative ".wikipub-mime-types.sexp"

let global_metadata_target_base = ".wikipub"

let global_metadata_target = relative ~dir:Path.the_root global_metadata_target_base

let update_wiki_args ~dir ~mode ~space =
  [ "update-wiki"
  ; "-acknowledge-danger"
  ; "-space"     ; space
  ; "-mime-types"; Path.reach_from ~dir mime_types
  ; "-mode"      ; mode
  ; Path.reach_from ~dir global_metadata_target
  ]

let confluence_xml_suffix = ".confluence_xml"
let confluence_metadata_suffix = ".confluence_metadata"

type t =
  { preview_subdirs_of : Path.t list
  ; upload_files : Path.t list
  }
[@@deriving fields, sexp]

let replace_extension_if_any path ~suf =
  let basename = Path.basename path in
  let without_ext = try Filename.chop_extension basename with _ -> basename in
  relative ~dir:(Path.dirname path) (without_ext ^ suf)
;;

let standard_formats ~dir = Glob.create ~dir "*.{org,md,mkd}"

let create ~preview_subdirs_of ~upload_files ~upload_standard_formats_in =
  Dep.List.concat_map upload_standard_formats_in ~f:(fun dir ->
    Dep.glob_listing (standard_formats ~dir))
  *>>| fun upload_files' ->
  { preview_subdirs_of
  ; upload_files = upload_files @ upload_files'
  }
;;

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
;;

let rules_for_individual_files ~dir = function
  | `Files l ->
    List.map l ~f:(fun basename -> rule_for_individual_file (relative ~dir basename))
    |> Scheme.rules
  | `Preview_subtree _ -> Scheme.empty
  | `Standard_formats ->
    Scheme.glob (standard_formats ~dir) (fun l ->
      List.map l ~f:rule_for_individual_file |> Scheme.rules)
;;

let preview_files t =
  List.filter t.upload_files ~f:(fun file ->
    List.exists t.preview_subdirs_of ~f:(fun dir -> Path.is_descendant ~dir file))
;;


let rules_for_global_metadata t =
  let dir = Path.the_root in
  let action =
    t
    *>>= fun t ->
    let all_metadata_files =
      List.map t.upload_files ~f:(replace_extension_if_any ~suf:confluence_metadata_suffix)
    in
    Dep.all_unit (List.map ~f:Dep.path (wikipub :: all_metadata_files))
    *>>| fun () ->
    Action.process ~dir (Path.reach_from ~dir wikipub)
      ("build-index"
       :: "-output" :: Path.reach_from ~dir global_metadata_target
       :: List.map all_metadata_files ~f:(reach_from ~dir))
  in
  [ Rule.create ~targets:[global_metadata_target] action
  ; alias_dot_filename_hack ~dir global_metadata_target_base
  ]
;;

let rule_for_preview t =
  let dir = Path.the_root in
  let action =
    Dep.both t (Dep.path global_metadata_target)
    *>>= fun (t, ()) ->
    match preview_files t with
    | [] -> Dep.return (Action.process ~dir "true" [])
    | _ :: _ ->
      (* 2017-03-28: Partial upload isn't implemented yet, so we ignore the specified
         preview paths and do a full upload whenever anything changes. *)
      let xml_files =
        List.map t.upload_files ~f:(replace_extension_if_any ~suf:confluence_xml_suffix)
      in
      Dep.both
        (Dep.all_unit (List.map ~f:Dep.path (wikipub :: mime_types :: xml_files)))
        (Dep.getenv krb5ccname)
      *>>| fun ((), krb5ccname) ->
      let env =
        Option.to_list (Option.map krb5ccname ~f:(fun v -> "KRB5CCNAME", v))
      in
      Action.process ~dir ~env
        (Path.reach_from ~dir wikipub)
        [ "preview"
        ; "-mime-types"; Path.reach_from ~dir mime_types
        ; Path.reach_from ~dir global_metadata_target
        ]
  in
  Rule.alias (Alias.create ~dir "wikipub-check") [ Dep.action action ]
;;

let upload_script_target = relative ~dir:Path.the_root "wikipub-update-prod-wiki.sh"

let rule_for_upload_script =
  let dir = Path.the_root in
  Rule.write_string ~chmod_x:() ~target:upload_script_target
    (sprintf !"#!/bin/bash\n\n%{quote} %{concat_quoted}"
       (Path.reach_from ~dir wikipub)
       (update_wiki_args ~dir ~mode:"prod" ~space:"JD"))
;;

let rules_for_the_root ~dir t =
  [%test_eq: Path.t] dir Path.the_root;
  rule_for_preview t
  :: rule_for_upload_script
  :: rules_for_global_metadata t
;;
