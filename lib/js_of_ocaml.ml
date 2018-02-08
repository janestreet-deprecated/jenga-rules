open Core
open Import
open Ocaml_types

module From_compiler_distribution = Ocaml_types.From_compiler_distribution

let exe_suf = ".bc.js"
let cma_suf = ".cma.js"
let cmo_suf = ".cmo.js"
let sourcemap_suf = ".map"
let runtime_suf = ".runtime.js"
let jsdeps_suf = ".jsdeps"
let export_suf = ".bc.export"
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

let js_of_ocaml_compiler  = Named_artifact.binary "js_of_ocaml"
let js_of_ocaml_linker    = Named_artifact.binary "jsoo_link"
let js_of_ocaml_mkcmis    = Named_artifact.binary "jsoo_mkcmis"
let js_of_ocaml_listunits = Named_artifact.binary "jsoo_listunits"

let js_of_ocaml_runtime fname =
  Named_artifact.in_findlib ("js_of_ocaml:" ^ fname)
let runtime_file_path artifacts f =
  Named_artifact.path artifacts (js_of_ocaml_runtime f)

let compiler_distribution_file f = Path.relative ~dir:dot_js_dir_ocamlwhere f

let runtime_files artifacts =
  Dep.all [runtime_file_path artifacts "runtime.js"]

let runtime_files_for_lib ~artifacts t =
  match t with
  | "graphics"      -> [runtime_file_path artifacts "graphics.js"]
  | "ocamlbytecomp" -> [runtime_file_path artifacts "toplevel.js";
                        runtime_file_path artifacts "dynlink.js"]
  | "nums_flat"
  | "nums"          -> [runtime_file_path artifacts "nat.js"]
  | _               -> []

let from_compiler_distribution lib =
  let name = From_compiler_distribution.to_string lib in
  let name =
    match From_compiler_distribution.artifact_dir_relative_to_stdlib_dir lib with
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

let check_libs_exn ~dir ~required_by libs =
  let bad_libs =
    List.filter libs ~f:(function
      | Lib_dep.In_the_tree lib ->
        not lib.supported_in_javascript
      | From_compiler_distribution v ->
        not (From_compiler_distribution.supported_in_javascript v)
      | Findlib_package _ ->
        false)
  in
  if not (List.is_empty bad_libs)
  then
    Located_error.raisef ~loc:(dummy_position (relative ~dir "jbuild"))
      !"%s is supposed to work in javascript but it has \
        problematic dependencies: %s" required_by
      (String.concat ~sep:" " (List.map bad_libs ~f:Lib_dep.to_string)) ()

let source_map_path p =
  Path.relative ~dir:(Path.dirname p) (with_sourcemap_suf (Path.basename p))

let has_separate_sourcemap_file = function
  | "--sourcemap"  | "-sourcemap"
  | "--source-map" | "-source-map" -> true
  | _ -> false

let source_map_disabled = function
  | "--no-sourcemap" | "--no-source-map" -> true
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
  let common =
      [ ["--enable";"with-js-error"]
      ; sourcemap_flag ~sourcemap
      ]
  in
  let devel =
    if devel
    then [ ["--pretty"] ]
    else []
  in
  let extra = common @ devel in
  let extra = List.map extra ~f:(fun l ->
    if exists_in_list ~pattern:l flags
    then []
    else l
  )
  in
  List.concat (flags :: extra)


let rule_aux ~artifacts ~sourcemap ~devel ~build_info ~hg_version ~dir ~flags ~other_flags ~target ~js_files rest_dep rest =
  let sourcemap = not (List.exists flags ~f:source_map_disabled) && sourcemap in
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
       other_flags
     *>>= fun ((compiler, js_files), other_flags) ->
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
       @ other_flags
       @ rest
     in
     Action.process ~dir (reach_from ~dir compiler) args
    )

let rule_for_compilation_unit ~artifacts ~dir ~sourcemap ~devel ~flags ~src ~target =
  let rest_dep = Dep.path src in
  let rest = [ reach_from ~dir src ] in
  let js_files = runtime_files artifacts in
  rule_aux ~artifacts ~sourcemap ~devel ~build_info:None ~hg_version:None
    ~dir ~flags ~other_flags:(Dep.return []) ~target ~js_files rest_dep rest

let rule_for_standalone_runtime ~artifacts ~sourcemap ~devel ~build_info ~hg_version ~dir ~flags ~other_flags ~js_files ~target =
  let rest_dep = Dep.return () in
  let flags = "--runtime-only" :: flags in
  let js_files =
    Dep.both (runtime_files artifacts) js_files
    *>>| fun (a,b) -> a @ b
  in
  rule_aux ~artifacts ~sourcemap ~devel ~build_info ~hg_version ~dir ~flags ~other_flags ~target ~js_files  rest_dep []

let setup_aux ~artifacts ~sourcemap ~devel ~src ~dst =
  let flags = [] in
  let other_flags = Dep.return [] in
  let js_files = runtime_files artifacts in
  Dep.glob_listing (Glob.create ~dir:src "*.{cma,cmo}")
  *>>| fun paths ->
  List.map paths ~f:(fun path ->
    let rest_dep = Dep.path path in
    let rest = [ reach_from ~dir:dst path ] in
    let target = Path.relative ~dir:dst (Path.basename path ^ ".js") in
    rule_aux ~artifacts ~sourcemap ~devel ~build_info:None ~hg_version:None ~dir:dst ~flags ~other_flags ~target ~js_files  rest_dep rest
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

let rules_for_toplevel_export ~dir ~ocaml_where ~artifacts ~archives_to_export ~export_file ~cmis_file ~cmi_deps =
  [ Rule.create ~targets:[cmis_file] (
      archives_to_export *>>= fun archives ->
      let toplevel_cmis =
        List.map [ "outcometree"; "topdirs"; "toploop" ]
          ~f:(fun unit ->
            From_compiler_distribution.cmi From_compiler_distribution.ocamlcommon
              ~stdlib_dir:ocaml_where ~unit)
      in
      let archives = toplevel_cmis @ archives in
      Dep.both
        (runtime_files artifacts)
        (Named_artifact.path artifacts js_of_ocaml_mkcmis) *>>= fun (runtime_files,mkcmis) ->
      Dep.all_unit (List.map ~f:Dep.path (mkcmis :: runtime_files @ archives) @ cmi_deps) *>>| fun () ->
      Action.process ~dir (reach_from ~dir mkcmis)
        (List.concat [
           ["--no-runtime"];
           List.map runtime_files ~f:(reach_from ~dir);
           List.map archives      ~f:(reach_from ~dir);
           ["-o"];
           [reach_from ~dir cmis_file]
         ]))
  ; Rule.create ~targets:[export_file]
      (Dep.both
         archives_to_export
         (Named_artifact.path artifacts js_of_ocaml_listunits)
       *>>= fun (archives, listunits) ->
       Dep.all_unit (List.map ~f:Dep.path (listunits :: archives) @ cmi_deps) *>>| fun () ->
       Action.process ~dir (reach_from ~dir listunits)
         (List.map archives ~f:(reach_from ~dir)
          @ ["-o"; (reach_from ~dir export_file)]))
  ]

let rule_for_library_jsdeps ~dir libname files =
  let target = LN.suffixed ~dir libname jsdeps_suf in
  let files = List.map files ~f:Path.to_string in
  Rule.write_names files ~target

let all_deps_for_libs ~artifacts ~path_to_ocaml_artifact (libs : Lib_dep.t list) : Path.t list Dep.t =
  Dep.List.concat_map libs ~f:(function
    | From_compiler_distribution l ->
      runtime_files_for_lib ~artifacts (From_compiler_distribution.to_string l)
      |> Dep.all
    | Findlib_package _ -> Dep.return []
    | In_the_tree {supported_in_javascript = false; _} -> Dep.return []
    | In_the_tree lib ->
      let path =
        path_to_ocaml_artifact ~lib_in_the_tree:lib.name ~suf:jsdeps_suf
      in
      Dep.both
        (runtime_files_for_lib ~artifacts (LN.to_string lib.name) |> Dep.all)
        (file_words path)
      *>>| fun (runtime_files,deps) ->
      runtime_files @ List.map ~f:Path.root_relative deps
  )

let rules_for_executable
      ~artifacts
      ~dir
      ~ocaml_where
      ~js_files
      ~libs_dep
      ~compute_objs
      ~toplevel
      ~hg_version
      ~build_info
      ~separate_compilation
      ~sourcemap
      ~devel
      ~exe
      ~drop_test_and_bench
      ~path_to_ocaml_artifact
      ~flags =
  (* Don't include build_info with separate_compilation.
     It otherwise forces the javascript runtime to be
     rebuilt every time and adds undesirable delays to the
     compilation loop. *)
  let build_info = if separate_compilation then None else build_info in
  let with_toplevel = Option.is_some toplevel in
  let module Mode = (val Ocaml_mode.byte) in
  let target_js = suffixed ~dir exe exe_suf in
  let export_file = Option.some_if with_toplevel (suffixed ~dir exe export_suf) in
  let cmis_file = suffixed ~dir exe ".cmis.js" in
  let libs_dep =
    libs_dep *>>| fun libs -> check_libs_exn ~dir ~required_by:exe libs; libs
  in
  let findlib_flags_query =
    Findlib.javascript_linker_option (module Mode) ~dir ~exe libs_dep
  in
  let flags =
    if drop_test_and_bench
    then [ "--setenv"; "FORCE_DROP_INLINE_TEST=true"
         ; "--setenv"; "FORCE_DROP_BENCH=true" ]
         @ flags
    else flags
  in
  let js_files : Path.t list Dep.t =
    libs_dep *>>= fun libs ->
    all_deps_for_libs ~artifacts ~path_to_ocaml_artifact libs
    *>>| fun js_files_from_libs ->
    js_files_from_libs @ js_files
  in
  let runtime_js = suffixed ~dir exe runtime_suf in
  let findlib_flags = Findlib.Query.result findlib_flags_query in
  let other_flags = findlib_flags in
  let with_toplevel = Option.is_some toplevel in
  let bc_dot_js_rule =
    if with_toplevel || not separate_compilation
    then begin
      let flags = match export_file, with_toplevel with
        | None, false -> flags
        | None, true | Some _, false -> assert false
        | Some export_file, true ->
          "--export" :: reach_from ~dir export_file ::
          "--toplevel" :: "--no-cmis" ::
          flags
      in
      let bytecode_exe = suffixed ~dir exe Mode.exe in
      let rest_dep =
        Dep.all_unit [ Dep.path bytecode_exe
                     ; Option.value_map export_file ~default:(Dep.return ()) ~f:Dep.path ]
      in
      let rest = [ reach_from ~dir bytecode_exe ] in
      let js_files =
        Dep.both js_files (runtime_files artifacts)
        *>>| fun (a,b) -> a @ b
      in
      rule_aux
        ~artifacts
        ~sourcemap:(sourcemap && not with_toplevel)
        ~devel:(devel && not with_toplevel)
        ~build_info ~hg_version ~dir
        ~flags ~other_flags
        ~js_files ~target:target_js
        rest_dep rest
    end else begin
      Rule.create ~targets:[target_js] (
        let findlib_archives =
          Findlib.archives_full_path (module Mode) ~dir ~exe libs_dep
        in
        libs_dep
        *>>= fun libs ->
        let libs =
          let stdlib = Lib_dep.From_compiler_distribution From_compiler_distribution.stdlib in
          if List.mem ~equal:Lib_dep.equal libs stdlib
          then libs
          else stdlib :: libs
        in
        compute_objs
        *>>= fun objs ->
        Findlib.Query.result findlib_archives
        *>>= fun archives_list ->
        from_external_archives
          ~ocaml_where
          (List.map ~f:Path.absolute archives_list)
        *>>= fun js_archives_list ->
        let sub_cmos_in_correct_order =
          List.map objs ~f:(fun (obj_dir, base) ->
            PN.suffixed ~dir:obj_dir base cmo_suf) in
        let libs_cma_js =
          List.concat_map libs ~f:(fun lib_dep ->
            match lib_dep with
            | In_the_tree lib ->
              [ path_to_ocaml_artifact ~lib_in_the_tree:lib.name ~suf:cma_suf ]
            | From_compiler_distribution dst ->
              [ from_compiler_distribution dst ]
            | Findlib_package _pkg -> []
          )
        in
        let all_files =
          List.concat
            [ [ runtime_js ]
            ; js_archives_list
            ; libs_cma_js
            ; sub_cmos_in_correct_order
            ]
        in
        link_js_files
          ~artifacts
          ~sourcemap
          ~dir ~files:all_files ~target:target_js
      )
    end
  in
  let extra_rules_for_toplevel =
    match toplevel,export_file with
    | None, None -> []
    | None, _ | _, None -> assert false
    | Some (libdeps, cmi_deps), Some export_file ->
      begin
        let archives_from_findlib =
          let findlib_archives =
            Findlib.archives_full_path (module Mode) ~dir ~exe (Dep.return libdeps)
          in
          Findlib.Query.result findlib_archives
          *>>| fun archives_list ->
          List.map ~f:Path.absolute archives_list
        in
        let archives =
          List.filter_map libdeps ~f:(function
            | In_the_tree lib ->
              Some (path_to_ocaml_artifact ~lib_in_the_tree:lib.name ~suf:".cma")
            | From_compiler_distribution dst ->
              Some (From_compiler_distribution.cma dst ~stdlib_dir:ocaml_where)
            | Findlib_package _ -> None)
        in
        let archives_to_export =
          archives_from_findlib *>>| fun archives_from_findlib ->
          archives_from_findlib @ archives
        in
        rules_for_toplevel_export
          ~dir ~ocaml_where:ocaml_where ~artifacts
          ~archives_to_export ~export_file ~cmis_file ~cmi_deps
      end
  in
  extra_rules_for_toplevel @
  Findlib.Query.rules findlib_flags_query @
  [ bc_dot_js_rule
  ; rule_for_standalone_runtime
      ~artifacts
      ~sourcemap
      ~devel
      ~other_flags
      ~build_info ~hg_version ~dir ~flags ~js_files ~target:runtime_js ]

let gen_html_for_inline_tests ~libname ~argv ~drop_test ~exe
  ~inline_test_runner_env_clearing_directives =
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
      let env =
        List.map inline_test_runner_env_clearing_directives ~f:(fun (k,v) ->
          sprintf "'%s':'%s'" k v
        )
        |> String.concat ~sep:","
      in
      String.concat ~sep:"\n"
        [ make_script
            (sprintf "var process = { argv : [%s],
                                      env  : {%s},
                                      exit : (function (code){ throw ('exit with code ' + code)}) };" argv env);
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
