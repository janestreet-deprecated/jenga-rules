open Core.Std
open Import
open Ocaml_types
open Jbuild_types

type findlib_conf =
  { opam_root         : string sexp_option
  ; opam_switch       : string [@default "system"]
  ; portable_makefile : bool [@default false]
  }
[@@deriving of_sexp]

let findlib_conf =
  Var.map (Var.register "FINDLIB_CONF") ~f:(fun x ->
    let x = Option.first_some x Config.findlib_conf_default in
    Option.map x ~f:(fun s ->
      Sexp.of_string_conv_exn (String.strip s) [%of_sexp: findlib_conf]))

let use_findlib, portable_makefile =
  match Var.peek findlib_conf with
  | None -> (false, false)
  | Some conf -> (true, conf.portable_makefile)

let opam = Path.absolute "/j/office/app/opam/prod/bin/opam"
let opam_bin_path = root_relative ".opam-bin"

let opam_bin_rule =
  Rule.create ~targets:[opam_bin_path] (
    Dep.getenv findlib_conf
    *>>= function
    | None | Some { opam_root = None; _ } ->
      return (Action.save ~target:opam_bin_path "")
    | Some { opam_root = Some opam_root; opam_switch; _ } ->
      Dep.path opam *>>| fun () ->
      let dir = Path.the_root in
      bashf ~dir
        !"%{quote} config var bin --root %{quote} --switch %{quote} > %{quote}"
        (Path.reach_from ~dir opam) opam_root opam_switch (Path.basename opam_bin_path)
  )

let ocamlfind =
  Dep.contents opam_bin_path *>>| fun s ->
  match String.strip s with
  | "" -> "ocamlfind"
  | s  -> s ^/ "ocamlfind"

let package_listing_path = root_relative ".findlib-packages"

let package_listing_rule =
  Rule.create ~targets:[package_listing_path] (
    ocamlfind *>>= fun ocamlfind ->
    (* Watch the global package installation directory so that we are notified of new
       packages *)
    Dep.action_stdout
      (return (Action.process ~dir:Path.the_root ocamlfind ["printconf"; "destdir"]))
    *>>= fun destdir ->
    Dep.glob_change (Glob.create ~kinds:[`Directory] ~dir:(Path.absolute destdir) "*")
    *>>| fun () ->
    bashf ~dir:Path.the_root
      !"%{quote} list | cut -d' ' -f1 > %{quote}"
      ocamlfind (Path.basename package_listing_path)
  )

let global_rules =
  if use_findlib then
    [ package_listing_rule
    ; opam_bin_rule
    ]
  else
    []

let packages =
  if use_findlib then
    Dep.contents package_listing_path
    *>>| fun s ->
    String.split_lines s
    |> List.map ~f:Findlib_package_name.of_string
    |> Findlib_package_name.Set.of_list
  else
    return Findlib_package_name.Set.empty

let default_predicates = ["mt"; "mt_posix"]

module Query = struct
  type 'a real =
    { result : 'a Dep.t
    ; rules  : Rule.t list
    }

  type 'a t =
    | Dummy of 'a
    | Real  of 'a real

  let dummy x = Dummy x

  let result = function
    | Dummy x -> return x
    | Real x -> x.result

  let result_and t other =
    match t with
    | Dummy x -> other *>>| fun y -> (x, y)
    | Real  x -> Dep.both x.result other

  let rules = function
    | Dummy _ -> []
    | Real x -> x.rules

  let create ~dir ?(predicates = []) ?(process_output=Fn.id) base deps ~format ~suffix =
    if not use_findlib
    then Dummy []
    else begin
      let predicates = predicates @ default_predicates in
      let flags_path = suffixed ~dir base suffix in
      let rule =
        Rule.create ~targets:[flags_path] (
          Dep.both ocamlfind deps *>>| fun (ocamlfind, (lib_deps : Lib_dep.t list)) ->
          match
            List.filter_map lib_deps ~f:(function
              | Findlib_package { name } -> Some name
              | _ -> None)
          with
          | [] -> Action.save "" ~target:flags_path
          | pkgs ->
            let predicates =
              match predicates with
              | [] -> ""
              | l  -> sprintf "-predicates %s" (String.concat l ~sep:",")
            in
            bashf ~dir !"%{quote} query -r -format %{quote} %s %s > %{quote}"
              ocamlfind
              format predicates
              (List.map pkgs ~f:Findlib_package_name.to_string |> String.concat ~sep:" ")
              (Path.reach_from ~dir flags_path)
        )
      in
      if portable_makefile then
        let result =
          Dep.path flags_path *>>| fun () -> [sprintf "`cat %s`" (basename flags_path)]
        in
        Real { result; rules = [rule] }
      else
        let result =
          Dep.contents flags_path *>>| fun s -> String.split_lines s |> process_output
        in
        Real { result; rules = [rule] }
    end
end

let archives_suffix     = ".external-archives"
let include_dirs_suffix = ".external-include-flags"

let archives (module M : Ocaml_mode.S) ~dir ~exe ?(predicates = []) deps =
  Query.create ~dir (exe ^ M.exe) deps ~format:"%a" ~suffix:archives_suffix
    ~predicates:(M.which_str :: predicates)

let include_flags ~dir base lib_deps =
  Query.create ~dir base lib_deps ~format:"%d"
    ~suffix:include_dirs_suffix ~process_output:(fun s ->
      remove_dups_preserve_order s |> List.concat_map ~f:(fun s -> ["-I"; s]))
