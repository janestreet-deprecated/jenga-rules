
open Core.Std

open Jbuild_types
open Import

let html_alias ~dir =
  Alias.create ~dir "html"

let pandoc_path = "/usr/bin/pandoc"

let pandoc_from_org_opts = ["-r"; "org"]

let pandoc_to_html_opts = ["-w"; "html"]

let rules ~dir ({ orgs; css } : Html_conf.t) =
  let orgs =
    List.map orgs
      ~f:(fun base ->
        let org_file = base ^ ".org" in
        let html_file = base ^ ".gen.html" in
        let org_path = Path.relative ~dir org_file in
        let html_path = Path.relative ~dir html_file in
        (org_path, html_path))
  in
  let css_opts =
    match css with
    | None -> []
    | Some name -> ["-c"; name]
  in
  let rules =
    List.map orgs
      ~f:(fun (org, html) ->
           Rule.create ~targets:[html]
             (Dep.path org
              *>>| fun () ->
              Action.process ~dir pandoc_path
                (List.concat [
                   pandoc_from_org_opts;
                   pandoc_to_html_opts;
                   css_opts;
                   [reach_from ~dir org];
                   ["-o"; reach_from ~dir html];
                 ])
             )
      )
  in
  let alias_rule =
    Rule.alias (html_alias ~dir)
      (List.map orgs ~f:(fun (_, html) -> Dep.path html))
  in
  alias_rule :: rules
