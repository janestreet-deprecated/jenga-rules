open Core.Std
open Import

let exe_suf = ".bc.js"
let cma_suf = ".cma.js"
let cmo_suf = ".cmo.js"
let runtime_suf = ".runtime.js"
let jsdeps_suf = ".jsdeps"

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

let base = Path.relative ~dir:Path.the_root "external/js_of_ocaml"
let compiler = Path.relative ~dir:base "compiler/js_of_ocaml"
let runtime_file_path f = Path.relative ~dir:base ("runtime/" ^ f)

let compiler_distribution_file f = Path.relative ~dir:dot_js_dir_ocamlwhere f

let runtime_files =
  List.map ~f:runtime_file_path
    [
      "runtime.js";
      "bigstring.js";
      "bin_prot.js";
      "core_kernel.js";
      "weak.js";
      "nat.js";
      "strftime.js"
    ]

let runtime_files_for_standalone_runtime =
  runtime_file_path "predefined_exceptions.js"
  :: runtime_files

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

let setup_aux ~src ~dst =
  Dep.glob_listing (Glob.create ~dir:src "*.{cma,cmo}")
  *>>| fun paths ->
  List.map paths ~f:(fun path ->
    let target = Path.relative ~dir:dst (Path.basename path ^ ".js") in
    Rule.simple
      ~targets:[target]
      ~deps:[ Dep.path compiler
            ; Dep.all_unit (List.map ~f:Dep.path runtime_files)
            ; Dep.path path ]
      ~action:(Action.process ~dir:dst (reach_from ~dir:dst compiler)
                 (List.map runtime_files ~f:(reach_from ~dir:dst)
                  @
                  [ reach_from ~dir:dst path
                  ; "--no-runtime"
                  ; "-o"; reach_from ~dir:dst target
                  ]))
  )

let setup_dot_js_dir ~ocaml_where (dir : Path.t) : Scheme.t =
  if dir = dot_js_dir then
    Scheme.empty
  else begin
    assert (Path.is_descendant ~dir:dot_js_dir dir);
    Scheme.dep
      (destdir_opt *>>| fun destdir_opt ->
       let mapping = mapping_in_to_out ~destdir_opt ~ocaml_where in
       match map_path mapping dir with
       | Some src -> Scheme.rules_dep (setup_aux ~src ~dst:dir)
       | None -> Scheme.empty
      )
  end

let source_map_path =
  let chop_extension s =
    try Filename.chop_extension s with Invalid_argument _ -> s
  in
  fun p ->
    let dir = Path.dirname p in
    let base = chop_extension (Path.basename p) in
    Path.relative ~dir (base ^ ".map")

let source_map_enabled = function
  | "--sourcemap"  | "-sourcemap"
  | "--source-map" | "-source-map" -> true
  | _ -> false

let rule_aux ~build_info ~hg_version ~dir ~flags ~target ~js_files rest_dep rest =
  let targets =
    if List.exists flags ~f:source_map_enabled
    then [target; source_map_path target]
    else [target]
  in
  Rule.create ~targets
    (js_files *>>= fun js_files ->
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
       | Some build_info -> [sprintf "--file=%s:/build_info.sexp" (reach_from ~dir build_info)]
     in
     let hg_version_flags =
       match hg_version with
       | None -> []
       | Some hg_version -> [sprintf "--file=%s:/hg_version.out" (reach_from ~dir hg_version)]
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
       @ rest
     in
     Action.process ~dir (reach_from ~dir compiler) args
    )

let rule ~build_info ~hg_version ~dir ~flags ~js_files ~src ~target =
  let rest_dep = Dep.path src in
  let rest = [ reach_from ~dir src ] in
  let js_files =
    js_files *>>| fun files -> files @ runtime_files
  in
  rule_aux ~build_info ~hg_version ~dir ~flags ~target ~js_files rest_dep rest

let rule_for_standalone_runtime ~build_info ~hg_version ~dir ~flags ~js_files ~target =
  let rest_dep = Dep.return () in
  let rest = [ "--runtime-only"; "dummy_source" ] in
  let js_files =
    js_files *>>| fun files -> files @ runtime_files_for_standalone_runtime
  in
  rule_aux ~build_info ~hg_version ~dir ~flags ~target ~js_files rest_dep rest

let link_js_files ~dir ~files ~target =
  Dep.all_unit (List.map ~f:Dep.path files) *>>| fun () ->
  bashf ~dir !"cat -- %{concat_quoted} > %{quote}"
    (List.map files ~f:(reach_from ~dir))
    (reach_from ~dir target)
