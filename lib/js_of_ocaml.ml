open Core.Std
open Import

let exe_suf = ".bc.js"
let cma_suf = ".cma.js"
let cmo_suf = ".cmo.js"
let sourcemap_suf = ".map"
let runtime_suf = ".runtime.js"
let jsdeps_suf = ".jsdeps"

let with_sourcemap_suf s =
  match String.chop_suffix ~suffix:".js" s with
  | None -> failwithf "Js_of_ocaml.with_sourcemap_suf: invalid extension" ()
  | Some prefix ->
    prefix ^ sourcemap_suf

(* Some libraries outside the tree might need to be compiled to javascript
   to support separate compilation. We store theses compiled javascript in
   a directory structure similar to the one in `ocamlfind printconf destdir`.

   That allows us to have a bidirectional mapping between external archives and
   locally compiled javascript file

   e.g.

   `ocamlfind printconf destdir`/${lib}/${name}.cma <-> .js/${lib}/${name}.cma.js
   `ocamlc -where`/${lib}.cma                       <-> .js/ocaml/${lib}.cma.js

*)
let dot_js_dir = Path.root_relative ".js"
let dot_js_dir_ocamlwhere = Path.relative ~dir:dot_js_dir "ocaml"
let js_of_ocaml_compiler = Named_artifact.binary "js_of_ocaml"
let js_of_ocaml_linker = Named_artifact.binary "jsoo_link"
let js_of_ocaml_runtime fname =
  Named_artifact.in_findlib ("js_of_ocaml:" ^ fname)
let runtime_file_path artifacts f =
  Named_artifact.path artifacts (js_of_ocaml_runtime f)

let compiler_distribution_file f = Path.relative ~dir:dot_js_dir_ocamlwhere f

let runtime_files artifacts =
  Dep.all (
    List.map ~f:(runtime_file_path artifacts)
      [
        "runtime.js";
        "weak.js";
      ]
  )
let runtime_files_for_standalone_runtime artifacts =
  Dep.both
    (runtime_file_path artifacts "predefined_exceptions.js")
    (runtime_files artifacts)
  *>>| fun (a,b) -> a :: b

let runtime_files_for_lib_in_compiler_distribution ~artifacts t =
  match Ocaml_types.From_compiler_distribution.to_string t with
  | "graphics" -> [runtime_file_path artifacts "graphics.js"]
  | "nums"     -> [runtime_file_path artifacts "nat.js"]
  | _          -> []

let stdlib_from_compiler_distribution = compiler_distribution_file ("stdlib" ^ cma_suf)

let from_compiler_distribution lib =
  let name = Ocaml_types.From_compiler_distribution.to_string lib in
  let name =
    match Ocaml_types.From_compiler_distribution.artifact_dir_relative_to_stdlib_dir lib with
    | None     -> String.concat ~sep:"" [        name;cma_suf]
    | Some dir -> String.concat ~sep:"" [dir;"/";name;cma_suf]
  in
  compiler_distribution_file name

let map_one_path ~src ~dst path =
  if Path.is_descendant ~dir:src path then
    let extra = Path.reach_from ~dir:src path in
    Some (Path.relative ~dir:dst extra)
  else None

let map_path mapping path =
  List.find_map ~f:(fun (src,dst) -> map_one_path ~src ~dst path) mapping

let destdir_opt =
  match Findlib.destdir with
  | Some destdir -> destdir *>>| Option.return
  | None -> Dep.return None

let mapping_out_to_in ~destdir_opt ~ocaml_where : (Path.t * Path.t) list =
  List.filter_map ~f:(fun x -> x)
    [ Option.map destdir_opt ~f:(fun destdir -> destdir, dot_js_dir)
    ; Some (ocaml_where, dot_js_dir_ocamlwhere) ]

let mapping_in_to_out ~destdir_opt ~ocaml_where =
  mapping_out_to_in ~destdir_opt ~ocaml_where
  |> List.map ~f:(fun (x,y) -> (y,x))

let from_external_archives ~ocaml_where paths =
  destdir_opt *>>| fun destdir_opt ->
  let mapping = mapping_out_to_in ~destdir_opt ~ocaml_where in
  List.map paths ~f:(fun path ->
    let path = Option.value (map_path mapping path) ~default:path in
    let dir = Path.dirname path in
    let base = Path.basename path in
    Path.relative ~dir (base ^ ".js")
  )

let source_map_path p =
  Path.relative ~dir:(Path.dirname p) (with_sourcemap_suf (Path.basename p))

let has_separate_sourcemap_file = function
  | "--sourcemap"  | "-sourcemap"
  | "--source-map" | "-source-map" -> true
  | _ -> false

let exists_in_list ~pattern =
  let rec loop l =
    if List.is_prefix ~prefix:pattern ~equal:String.equal l
    then true
    else match l with
      | [] -> false
      | _ :: xs -> loop xs
  in
  loop

let sourcemap_flag ~sourcemap = if sourcemap then ["--source-map-inline"] else []

let normalize_flags ~sourcemap devel flags =
  let extra =
    if devel
    then
      [ ["--enable";"with-js-error"]
      ; ["--pretty"]
      ; sourcemap_flag ~sourcemap
      ]
    else
      [ ["--enable";"with-js-error"]
      ]
  in
  let extra = List.map extra ~f:(fun l ->
    if exists_in_list ~pattern:l flags
    then []
    else l
  )
  in
  List.concat (flags :: extra)

let rule_aux ~artifacts ~sourcemap ~devel ~build_info ~hg_version ~dir ~flags ~findlib_flags ~target ~js_files rest_dep rest =
  let flags = normalize_flags ~sourcemap devel flags in
  let targets =
    if List.exists flags ~f:has_separate_sourcemap_file
    then [target; source_map_path target]
    else [target]
  in
  Rule.create ~targets
    (Dep.both
       (Dep.both
          (Named_artifact.path artifacts js_of_ocaml_compiler)
          js_files)
       findlib_flags
     *>>= fun ((compiler, js_files), findlib_flags) ->
     Dep.all_unit
       [ Dep.path compiler
       ; Dep.all_unit (List.map ~f:Dep.path js_files)
       ; Dep.all_unit (List.filter_map ~f:(Option.map ~f:Dep.path) [build_info; hg_version])
       ; rest_dep
       ]
     *>>| fun () ->
     let build_info_flags =
       match build_info with
       | None -> []
       | Some build_info -> [sprintf "--file=%s:/static/build_info.sexp" (reach_from ~dir build_info)]
     in
     let hg_version_flags =
       match hg_version with
       | None -> []
       | Some hg_version -> [sprintf "--file=%s:/static/hg_version.out" (reach_from ~dir hg_version)]
     in
     let flags =
       [ "--no-runtime"
       ; "-I"; "."
       ; "-o"; reach_from ~dir target ]
       @ flags @ hg_version_flags @ build_info_flags
     in
     let args =
       flags
       @ List.map ~f:(fun x -> reach_from ~dir x) js_files
       @ findlib_flags
       @ rest
     in
     Action.process ~dir (reach_from ~dir compiler) args
    )

let rule ~artifacts ~sourcemap ~devel ~build_info ~hg_version ~dir ~flags ~findlib_flags ~js_files ~src ~target =
  let rest_dep = Dep.path src in
  let rest = [ reach_from ~dir src ] in
  let js_files =
    Dep.both js_files (runtime_files artifacts)
    *>>| fun (a,b) -> a @ b
  in
  rule_aux ~artifacts ~sourcemap ~devel ~build_info ~hg_version ~dir ~flags ~findlib_flags ~target ~js_files rest_dep rest

let rule_for_standalone_runtime ~artifacts ~sourcemap ~devel ~build_info ~hg_version ~dir ~flags ~findlib_flags ~js_files ~target =
  let rest_dep = Dep.return () in
  let rest = [ "--runtime-only"; "dummy_source" ] in
  let js_files =
    Dep.both (runtime_files_for_standalone_runtime artifacts) js_files
    *>>| fun (a,b) -> a @ b
  in
  rule_aux ~artifacts ~sourcemap ~devel ~build_info ~hg_version ~dir ~flags ~findlib_flags ~target ~js_files  rest_dep rest

let setup_aux ~artifacts ~sourcemap ~devel ~src ~dst =
  let flags = [] in
  let findlib_flags = Dep.return [] in
  let js_files = runtime_files artifacts in
  Dep.glob_listing (Glob.create ~dir:src "*.{cma,cmo}")
  *>>| fun paths ->
  List.map paths ~f:(fun path ->
    let rest_dep = Dep.path path in
    let rest = [ reach_from ~dir:dst path ] in
    let target = Path.relative ~dir:dst (Path.basename path ^ ".js") in
    rule_aux ~artifacts ~sourcemap ~devel ~build_info:None ~hg_version:None ~dir:dst ~flags ~findlib_flags ~target ~js_files  rest_dep rest
  )

let setup_dot_js_dir ~artifacts ~sourcemap ~devel ~ocaml_where (dir : Path.t) : Scheme.t =
  if Path.(=) dir dot_js_dir then
    Scheme.empty
  else begin
    assert (Path.is_descendant ~dir:dot_js_dir dir);
    Scheme.dep
      (destdir_opt *>>| fun destdir_opt ->
       let mapping = mapping_in_to_out ~destdir_opt ~ocaml_where in
       match map_path mapping dir with
       | Some src -> Scheme.rules_dep (setup_aux ~artifacts ~sourcemap ~devel ~src ~dst:dir)
       | None -> Scheme.empty
      )
  end

let link_js_files ~artifacts ~sourcemap ~dir ~files ~target =
  let flags = sourcemap_flag ~sourcemap in
  Named_artifact.path artifacts js_of_ocaml_linker *>>= fun linker ->
  Dep.all_unit (List.map ~f:Dep.path (linker :: files)) *>>| fun () ->
  bashf ~dir !"%{quote} %{concat_quoted} -o %{quote} %{concat_quoted}"
    (reach_from ~dir linker)
    (List.map files ~f:(reach_from ~dir))
    (reach_from ~dir target)
    flags


let gen_html_for_inline_tests ~libname ~argv ~drop_test ~exe =
  let make_script ?src content =
    let src = match src with
      | None -> ""
      | Some src -> sprintf "src='%s'" src
    in
    sprintf "<script type='text/javascript' %s> %s </script>" src content
  in
  let run =
    if drop_test
    then make_script "console.log('Tests have been disabled');"
    else
      let argv =
        let all = "dummy" :: argv in
        String.concat ~sep:", " (List.map ~f:(sprintf "'%s'") all)
      in
      (* We [export TZ] so that tests do not depend on the local timezone. *)
      String.concat ~sep:"\n"
        [ make_script
            (sprintf "var process = { argv : [%s],
                                      env  : {'TZ':'America/New_York'},
                                      exit : (function (code){ throw ('exit with code ' + code)}) };" argv);
          make_script ~src:exe ""
        ]
  in
  (sprintf "<!DOCTYPE html>
     <html>
       <head>
         <meta charset='UTF-8'>
         <title>inline_tests_runner for %s </title>
       </head>
       <body>
       </body>
       %s
     </html>" (Ocaml_types.Libname.to_string libname) run)
