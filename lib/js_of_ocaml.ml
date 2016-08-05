open Core.Std
open Import

let suf = ".bc.js"

let base = Path.relative ~dir:Path.the_root "external/js_of_ocaml"
let compiler = Path.relative ~dir:base "compiler/js_of_ocaml"
let runtime =
  List.map ~f:(fun f -> Path.relative ~dir:base ("runtime/" ^ f))
    [ "bigstring.js"
    ; "bin_prot.js"
    ; "core_kernel.js"
    ; "runtime.js"
    ; "weak.js"
    ; "nat.js"
    ; "strftime.js" ]

let rule ~build_info ~hg_version ~dir ~flags ~src ~target =
  Rule.create ~targets:[target]
    (Dep.all_unit
       [ Dep.path compiler
       ; Dep.all_unit (List.map ~f:Dep.path runtime)
       ; Dep.all_unit (List.filter_map ~f:(Option.map ~f:Dep.path) [build_info; hg_version])
       ; Dep.path src ]
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
       @ List.map ~f:(fun x -> reach_from ~dir x) runtime
       @ [ reach_from ~dir src ] in
     Action.process ~dir (reach_from ~dir compiler) args
    )
