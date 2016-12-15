open! Core.Std
open! Import
open Ocaml_types

let target_directory_heuristics path =
  match Path.split path with
  | topdir :: project :: _ ->
    let projects =
      match String.chop_suffix project ~suffix:"_kernel" with
      | Some rest -> [ project; rest ]
      | None -> [ project ]
    in
    List.map projects ~f:(fun project ->
      List.fold_left ~init:Path.the_root [ topdir; project ]
        ~f:(fun acc v -> Path.relative ~dir:acc v))
  | _ -> [ path ]

let rules ~dir ~one_step_libdeps =
  [
    Rule.alias (Alias.create ~dir "transitive-runtest")
      [ Dep.return ()
        *>>= fun () ->
        Dep.all_unit
          (List.map (target_directory_heuristics dir) ~f:(fun dir ->
             Dep.alias (Alias.create "runtest" ~dir)))
      ];
    Rule.alias (Alias.create ~dir "transitive-runtest") [
      Dep.return ()
      *>>= fun () ->
      one_step_libdeps *>>= fun libs ->
      let dirs =
        List.filter_map libs ~f:(fun lib ->
          match Lib_dep.to_lib_in_the_tree lib with
          | None -> None
          | Some l -> Some l.source_path)
      in
      Dep.all_unit
        (List.map dirs ~f:(fun dir -> Dep.alias (Alias.create ~dir "transitive-runtest")))
    ];
  ]
;;
