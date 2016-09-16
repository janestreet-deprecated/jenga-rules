open Core.Std
open Import
open Jbuild_types
open Ocaml_types

let opam_switches = ["4.03.0"; "4.03.0+32bit"; "osx-4.03.0"]

module Make(Jenga_root : sig
    module Libmap : sig
      type t
      val resolve_libdep_names_exn : t -> Libdep_name.t list -> Lib_dep.t list
    end

    module DC : sig
      type t
      val libmap : t -> Libmap.t
    end

    module BN : sig
      include Identifiable.S
      val to_module : t -> string
    end

    module Lib_modules : sig
      type t
      val impls_and_intfs : t -> BN.t list
      val load : dir:Path.t -> libname:LN.t -> t Dep.t
    end

    module Standard_pp_sets : sig
      val extract : string list -> string list
      val expand : string list -> string list
    end

    module PP : sig
      include Identifiable.S
    end

    module User_or_gen_config : sig
      val load : dir: Path.t -> Jbuild.t list Dep.t
    end

    val public_release_files : Path.t
    val deep_unignored_subdirs : dir:Path.t -> Path.t list Dep.t
    val libs_for_code_generated_by_pp : inside_base:bool -> Libmap.t -> PP.t -> Lib_dep.t list
    val ocaml_libraries : [< Jbuild.t ] -> Libdep_name.t list
    val pps_of_jbuild : DC.t -> [< Jbuild.t ] -> Lib_dep.t list
    val expand_pps : Unexpanded_pp.t list -> PP.t list * string list
    val remap_pa_names : PP.t list -> string list
  end) : sig
  open Jenga_root

  val rules : DC.t -> dir:Path.t -> Public_repo.t -> Rule.t list

  module Package_map : sig
    (** Rules to build public-release/repos/.package-map.sexp *)
    val global_rules : dir:Path.t -> Rule.t list
  end

end = struct
  open Jenga_root

  module T = Public_release_types

  (* Where the descriptions of external packages are *)
  let repos = root_relative "public-release/repos"

  (* Path to an external package *)
  let repo_path name = relative ~dir:repos name

  (* Files included in all packages *)
  let public_release_common_files = root_relative "public-release/common-files"

  (* The final tarball *)
  let tarball ~dir = relative ~dir "dist.tar.gz"

  (* Checksum of files inside [tarball ~dir]. We can't just take the tarball checksums due
     to times. *)
  let dist_checksum ~dir = relative ~dir "dist.checksum"

  (* List of files to copy in the given package *)
  let files_to_copy_filename = ".files-to-copy"
  let files_to_copy_path ~dir = relative ~dir files_to_copy_filename

  let build_servers_config = root_relative "public-release/build-servers.cfg"

  let escape_for_egrep =
    let module S = struct type t = Ok | Invalid | Escape end in
    let char_status : char -> S.t = function
      | '.' | '?' | '*' | '+' | '[' | ']' | '\\' | '{' | '}' | '(' | ')' | '$' | '^' | '|'
        -> Escape
      | '\033' .. '\127' -> Ok
      | _ -> Invalid
    in
    fun word ->
      if String.for_all word ~f:(fun ch ->
        match char_status ch with Ok -> true | Invalid | Escape -> false
      ) then
        word
      else
        String.concat_map word ~f:(fun ch ->
          match char_status ch with
          | Escape  -> sprintf "\\%c" ch
          | Ok      -> String.of_char ch
          | Invalid -> raise_s [%sexp "can't escape this word", ~~(word : string)])

  let or_regexp_for_grep words =
    List.map words ~f:escape_for_egrep
    |> String.concat ~sep:"|"

  (* Map directories from the jane source tree to the public package they are part of (if
     any). *)
  module Package_map : sig
    val load : string Path.Map.t Dep.t

    (* List of directories in the jane repo that will be exported in the given package. *)
    val dirs_exported_by_package : package:string -> Path.t list Dep.t

    val jbuilds_rule : dir:Path.t -> conf:Public_repo.t -> Rule.t
    val global_rules : dir:Path.t -> Rule.t list
  end = struct
    (* List of jbuilds that a given external packages looks at *)
    let jbuilds_filename = ".jbuilds"
    let jbuilds_path ~dir = relative ~dir jbuilds_filename
    let jbuilds_rule ~dir ~(conf : Public_repo.t) =
      let target = jbuilds_path ~dir in
      Rule.create ~targets:[target] (
        Dep.both
          (Dep.path public_release_files)
          (Dep.List.concat_map conf.dirs ~f:(fun (_, dir) -> deep_unignored_subdirs ~dir))
        *>>| fun ((), unignored_subdirs) ->
        bashf ~dir:Path.the_root
          !"egrep %{quote} %{quote} > %{quote}"
          (List.map unignored_subdirs ~f:Path.to_string
           |> or_regexp_for_grep
           |> sprintf "^(%s)/jbuild$")
          (Path.to_string public_release_files)
          (Path.to_string target)
      )

    let dirs_exported_by_package ~package =
      Dep.contents (jbuilds_path ~dir:(repo_path package))
      *>>| fun s ->
      String.split_lines s
      |> List.map ~f:root_relative
      |> List.map ~f:Path.dirname

    let package_map_file = relative ~dir:repos ".package-map.sexp"

    let load =
      Dep.contents package_map_file
      *>>| fun s ->
      Sexp.of_string_conv_exn (String.strip s) [%of_sexp: string Path.Map.t]

    let all_repos_file = relative ~dir:repos ".repos"

    let global_rules ~dir =
      if not (Path.equal dir repos) then [] else begin
        let create_all_repos_file =
          Dep.path public_release_files
          *>>| fun () ->
          bashf ~dir:Path.the_root
            !"sed -nr %{quote} %{quote} > %{quote}"
            (Path.to_string repos
             |> escape_for_egrep
             |> sprintf "s|^(%s/[^/]*)/jbuild$|\\1|p")
            (Path.to_string public_release_files)
            (Path.to_string all_repos_file)
        in
        let create_package_map =
          Dep.contents all_repos_file
          *>>= fun s ->
          let all_packages =
            List.map (String.split_lines s) ~f:Filename.basename
          in
          Dep.all (List.map all_packages ~f:(fun package ->
            dirs_exported_by_package ~package
            *>>| fun dirs ->
            List.map dirs ~f:(fun dir -> (dir, package))))
          *>>| fun l ->
          let l = List.concat l in
          let map =
            match Path.Map.of_alist l with
            | `Ok map -> map
            | `Duplicate_key dir ->
              let pkgs =
                List.filter_map l ~f:(fun (d, p) ->
                  if Path.equal dir d then Some p else None)
              in
              failwiths "directory present in several packages"
                [%sexp
                  { dir      = (dir  : Path.t     )
                  ; packages = (pkgs : string list)
                  }
                ] Fn.id
          in
          Action.save ~target:package_map_file
            (Sexp.to_string_hum ([%sexp_of: string Path.Map.t] map))
        in
        [ Rule.create ~targets:[package_map_file] create_package_map
        ; Rule.create ~targets:[all_repos_file  ] create_all_repos_file
        ]
      end
  end

  (* Build the .metadata.sexp file, which is used both for creating the tarballs and get
     find dependencies between external packages. *)
  module Metadata : sig
    val path : package:string -> Path.t
    val load : package:string -> T.Package.t Dep.t
    val rule
      :  DC.t
      -> package:string
      -> conf:Public_repo.t
      -> Rule.t
  end = struct
    let filename = ".metadata.sexp"
    let path ~package = relative ~dir:(repo_path package) filename

    let lib_conf (lib : Lib_in_the_tree.t) =
      User_or_gen_config.load ~dir:lib.source_path *>>| fun jbuilds ->
      List.find_map_exn jbuilds
        ~f:(function
          | `library ({ name; _ } as lib) when LN.(=) name lib.name -> Some lib
          | _ -> None)

    let fail_at file ?(line=1) fmt =
      ksprintf (fun msg ->
        printf !"File \"%{Path}\", line %d:\n\
                 Error: %s\n"
          file line msg;
        failwithf !"%{Path}: %s" file msg ())
        fmt

    let in_the_tree_dependency ~of_pkg ~of_dir package_map context lib =
      lib_conf lib *>>= fun lib_conf ->
      let (kind, internal) : T.Buildable.Dependency.Kind.t * bool =
        let ocamlfind_package =
          Option.map lib.public_name ~f:Findlib_package_name.to_string
        in
        match lib_conf.public_release with
        | External { opam_package; _ } -> begin
            match ocamlfind_package with
            | None ->
              fail_at (Path.relative ~dir:of_dir "jbuild")
                !"Missing (public_name ...) in external library:\n\
                 \  %{LN} (%{Path}/jbuild)"
                lib.name lib.source_path
            | Some ocamlfind_package ->
              let opam_package =
                match opam_package with
                | None -> T.opam_of_ocamlfind ocamlfind_package
                | Some p -> p
              in
              (Global { ocamlfind_package; opam_package }, false)
          end
        | Internal _ ->
          match Map.find package_map lib.source_path with
          | None ->
            fail_at (Path.relative ~dir:of_dir "jbuild")
              !"Library required but not part of any released package:\n\
               \  %{LN} (%{Path}/jbuild)"
              lib.name lib.source_path
          | Some opam_package ->
            if opam_package = of_pkg then
              (Local { internal_name = LN.to_string lib.name
                     ; ocamlfind_package
                     },
               true)
            else begin
              match ocamlfind_package with
              | Some ocamlfind_package ->
                (Global { ocamlfind_package; opam_package }, true)
              | None ->
                fail_at (Path.relative ~dir:of_dir "jbuild")
                  !"Library required from different package but not installed \
                    by the package\n\
                   \  %{LN} from package %s (%{Path}/jbuild)"
                  lib.name opam_package lib.source_path
            end
      in
      let lib_kind =
        match lib_conf.public_release with
        | Internal x -> x.kind
        | External _ -> Normal
      in
      let dep : T.Buildable.Dependency.t =
        { kind
        ; context
        ; internal
        ; lib_kind
        ; wrapper = None
        }
      in
      let virtual_opam_deps =
        match lib_conf.public_release with
        | External e -> List.map e.virtual_opam_deps ~f:(fun x ->
          { T.Package.Dependency.
            opam_package = x
          ; internal = false
          ; context
          })
        | Internal _ -> []
      in
      let externally_wrapped =
        match lib_conf.public_release with
        | Internal _ | External { wrapped = Default; _ } -> lib_conf.wrapped
        | External { wrapped = True ; _ } -> true
        | External { wrapped = False; _ } -> false
      in
      match lib_conf.wrapped, externally_wrapped with
      | true, true | false, false -> return (dep, virtual_opam_deps)
      | false, true ->
        fail_at (Path.relative ~dir:lib.source_path "jbuild")
          !"This internal/external wrapped pattern is not supported:\n\
            - library:            %{LN}\n\
            - internally_wrapped: %B\n\
            - externally_wrapped: %B"
          lib.name lib_conf.wrapped externally_wrapped
      | true, false ->
        Lib_modules.load ~dir:lib.source_path ~libname:lib.name *>>| fun modules ->
        let wrapper =
          Some (LN.to_string lib.name,
                List.map (Lib_modules.impls_and_intfs modules) ~f:BN.to_module)
        in
        ({ dep with wrapper }, virtual_opam_deps)

    let dependency ~of_pkg ~of_dir package_map context (lib_dep : Lib_dep.t) =
      match lib_dep with
      | In_the_tree lib ->
        in_the_tree_dependency ~of_pkg ~of_dir package_map context lib
      | From_compiler_distribution compiler_lib ->
        let ocamlfind_package =
          From_compiler_distribution.ocamlfind_package compiler_lib
          |> Findlib_package_name.to_string
        in
        return
          ({ T.Buildable.Dependency.
             kind = Global (T.Buildable.Dependency.Global.of_ocamlfind_package
                              ocamlfind_package)
           ; context
           ; internal = false
           ; lib_kind = Normal
           ; wrapper = None
           }, [])
      | Findlib_package pkg ->
        let pkg = Findlib_package_name.to_string pkg.name in
        return
          ({ T.Buildable.Dependency.
             kind = Global (T.Buildable.Dependency.Global.of_ocamlfind_package pkg)
           ; context
           ; internal = false
           ; lib_kind = Normal
           ; wrapper = None
           }, [])

    let sub_dir_in_dest ~(conf : Public_repo.t) path =
      List.find_map_exn conf.dirs ~f:(fun (sub_dir, dir) ->
        if Path.is_descendant ~dir path then
          Some (let d = sub_dir ^/ reach_from ~dir path in
                let d =
                  if String.is_prefix d ~prefix:"./" then
                    String.slice d 2 (String.length d)
                  else
                    d
                in
                if String.is_suffix d ~suffix:"/." then
                  String.slice d 0 (String.length d - 2)
                else
                  d)
        else
          None)

    type thing_to_export =
      | Library    of T.Library.t
      | Executable of T.Executable.t

    let make_buildable dc ~package_map ~pkg_name
          ~(jbuild     : [ `library     of Library_conf.t
                         | `executables of Executables_conf.t ])
          ?libraries_override
          ~(extra_deps : Library_conf.Public_release.extra_dep list)
          ?(extra_opam_deps=[])
          ?(needs_include=false) (* Do we need the global include/ directory? *)
          ~(conf       : Public_repo.t) ~dir () =
      let module D = T.Buildable.Dependency in

      (* If it is a ppx, find its runtime deps *)
      let rt_deps =
        match jbuild with
        | `library { name; _ } ->
          libs_for_code_generated_by_pp ~inside_base:false (DC.libmap dc)
            (PP.of_string (LN.to_string name))
        | `executables _ -> []
      in

      let dependency = dependency ~of_dir:dir ~of_pkg:pkg_name package_map in
      let deps =
        let libraries =
          let standard = List.map (ocaml_libraries jbuild) ~f:Libdep_name.to_string in
          Ordered_set_lang.eval_with_standard libraries_override ~standard
          |> List.map ~f:Libdep_name.of_string
          |> Libmap.resolve_libdep_names_exn (DC.libmap dc)
        in
        (libraries @ pps_of_jbuild dc jbuild)
        |> Lib_dep.remove_dups_and_sort
      in
      Dep.all (List.map deps    ~f:(dependency D.Context.Both   ) @
               List.map rt_deps ~f:(dependency D.Context.Runtime))
      *>>= fun deps_and_virtual_opam_deps ->
      let dependencies, virtual_opam_deps =
        Tuple.T2.map_snd (List.unzip deps_and_virtual_opam_deps) ~f:List.concat
      in

      let preprocess, preprocessor_deps =
        match jbuild with
        | `library     { preprocess; preprocessor_deps; _ }
        | `executables { preprocess; preprocessor_deps; _ } ->
          (preprocess, preprocessor_deps)
      in

      let has_ppx_jane, other_ppxs =
        match preprocess with
        | [ (`pps pps, _) ] ->
          let pps =
            expand_pps pps
            |> fst
            |> remap_pa_names
            |> Standard_pp_sets.extract
          in
          let has_ppx_jane, others =
            if List.mem pps "JANE" then
              (true, List.filter pps ~f:((<>) "JANE"))
            else if List.mem pps "JANE_KERNEL" then
              (true, List.filter pps ~f:((<>) "JANE_KERNEL"))
            else
              (false, pps)
          in
          (has_ppx_jane,
           Standard_pp_sets.expand others
           |> List.filter ~f:((<>) "ppx_js_style")
           |> List.map ~f:Libdep_name.of_string
           |> Libmap.resolve_libdep_names_exn (DC.libmap dc))
        | _ -> (false, [])
      in
      Dep.all (List.map other_ppxs ~f:(dependency D.Context.Build))
      *>>| fun ppxs ->
      let ppxs =
        List.map ppxs ~f:(fun (ppx, virt_deps) -> assert (List.is_empty virt_deps); ppx)
      in
      let js_ppxs =
        List.filter_map ppxs ~f:(fun dep ->
          match dep.kind with
          | Local _  -> None
          | Global g -> Some g)
      in
      let js_ppxs =
        if not has_ppx_jane then
          js_ppxs
        else
          { ocamlfind_package = "ppx_jane"; opam_package = "ppx_jane" }
          :: js_ppxs
      in

      let extra_deps =
        let ext_dep pkg = D.Kind.Global (D.Global.of_ocamlfind_package pkg) in
        List.concat
          [ List.map extra_deps ~f:(fun { context; package } ->
              (context, D.Kind.Global package))
          ; if List.exists preprocess ~f:(fun (k, _) -> Pervasives.(=) k `metaquot) then
              [ (Build, ext_dep "ppx_tools.metaquot") ]
            else
              []
          ]
        |> List.map ~f:(fun (context, kind) ->
          { T.Buildable.Dependency.
            kind
          ; context
          ; lib_kind = Normal
          ; internal = false
          ; wrapper  = None
          })
      in
      let dependencies = dependencies @ extra_deps in

      let dependencies =
        if not needs_include then
          dependencies
        else
          { kind     = Global (T.Buildable.Dependency.Global.of_ocamlfind_package
                                 "jane-street-headers")
          ; context  = Build
          ; internal = true
          ; lib_kind = Normal
          ; wrapper  = None
          } :: dependencies
      in

      let preprocessor_deps =
        List.filter_map preprocessor_deps ~f:(function
          | File fn -> Some (Filename.basename fn)
          | _       -> None)
      in

      ({ T.Buildable.
         path = sub_dir_in_dest ~conf dir
       ; dependencies
       ; js_ppxs
       ; preprocessor_deps
       },
       extra_opam_deps @ virtual_opam_deps)


    let check_install_as ~libname ~pkg_name ~ocamlfind_package =
      if T.opam_of_ocamlfind ocamlfind_package <> pkg_name then
        failwiths
          "Cannot install a library as an ocamlfind package that \
           is not a children of the public package name"
          [%sexp
            { library           = (libname           : LN.t  )
            ; ocamlfind_package = (ocamlfind_package : string)
            ; package           = (pkg_name          : string)
            }]
          Fn.id

    let export_library dc ~dir ~package_map ~(conf : Public_repo.t)
          ~jbuild ~(info : Library_conf.Public_release.internal_package) ~pkg_name =
      let (`library { Library_conf. name; c_names; public_name; wrapped; _ }) =
        jbuild
      in

      let public_name =
        Option.map public_name ~f:(fun ocamlfind_package ->
          let ocamlfind_package = Findlib_package_name.to_string ocamlfind_package in
          check_install_as ~libname:name ~pkg_name ~ocamlfind_package;
          ocamlfind_package)
      in

      Dep.glob_listing (Glob.create ~dir ~kinds:[`File] "*.h")
      *>>= fun h_files ->
      let h_files =
        List.map h_files ~f:Path.basename
        |> List.filter ~f:(fun s -> not (String.is_prefix ~prefix:"utop." s))
      in
      let c_files = h_files @ List.map c_names ~f:(fun s -> s ^ ".c") in
      (* Special case for re2 *)
      let is_re2 = LN.to_string name = "re2" in
      let c_files = if is_re2 then [] else c_files in

      (* Detect if we need the global include/ directory *)
      (if is_re2 then
         return false
       else
         Dep.action_stdout
           (let deps_files = List.map c_names ~f:(fun s -> s ^ ".deps") in
            Dep.all_unit (List.map deps_files ~f:(fun fn ->
              Dep.path (Path.relative ~dir fn)))
            *>>| fun () ->
            bashf ~dir "grep -Eq '^([.][.]/)*include/' -- %s || echo -n not-found"
              (List.map deps_files ~f:quote |> String.concat ~sep:" "))
         *>>| function
         | "not-found" -> false
         | "" -> true
         | output -> raise_s [%message [%here] "unexpected output from grep"
                                         (output : string)]
      ) *>>= fun needs_include ->

      Lib_modules.load ~dir ~libname:name
      *>>= fun modules ->
      let modules = Lib_modules.impls_and_intfs modules in

      make_buildable dc ~conf ~package_map ~dir ~pkg_name
        ~jbuild:(jbuild :> [ `executables of _ | `library of _ ])
        ~extra_deps:info.extra_deps ~extra_opam_deps:info.extra_opam_deps
        ~needs_include
        ?libraries_override:info.libraries ()
      *>>| fun (buildable, virtual_opam_deps) ->
      (buildable, virtual_opam_deps,
       [Library
          { internal_name = LN.to_string name
          ; kind          = info.kind
          ; oasis_kind    = Library
          ; wrapped       = wrapped
          ; short_desc    = info.desc
          ; modules       = List.map modules ~f:BN.to_module
          ; path          = buildable.path
          ; install_as    = public_name
          ; c_files
          }])

    let things_to_export dc ~package_map ~pkg_dir ~(conf : Public_repo.t) =
      let pkg_name = Path.basename pkg_dir in
      Package_map.dirs_exported_by_package ~package:pkg_name
      *>>= fun exported_dirs ->
      Dep.List.concat_map exported_dirs ~f:(fun dir ->
        User_or_gen_config.load ~dir *>>| fun jbuilds ->
        List.map jbuilds ~f:(fun jbuild -> (dir, jbuild)))
      *>>= fun jbuilds ->
      (Dep.all @@ List.filter_map jbuilds ~f:(fun (dir, jbuild) ->
         match jbuild with
         | `library { name; public_release = External _; _ }  ->
           failwiths "External libraries cannot be released"
             [%sexp { library = (name     : LN.t)
                    ; package = (pkg_name : string)
                    }]
             Fn.id
         | `library { public_release = Internal info; public_name = Some _; _ }
           as jbuild ->
           Some (export_library dc ~package_map ~dir ~conf ~info ~pkg_name ~jbuild)
         | `executables { public_release = { build = []
                                           ; build_and_install = []
                                           ; build_and_install_as_objects = []
                                           ; _ }
                        ; _ } ->
           None
         | `executables { public_release = info
                        ; _ } as jbuild -> Some (
           make_buildable dc ~conf ~dir ~pkg_name ~jbuild ~package_map
             ~extra_deps:info.extra_deps ()
           *>>| fun (buildable, virtual_opam_deps) ->
           let normal_binaries =
             (info.build @ info.build_and_install)
             |> List.map ~f:(fun exe ->
               Executable
                 { main_module = exe.name
                 ; install_as  = exe.install_as
                 ; path        = buildable.path
                 ; mode        = exe.mode
                 })
           in
           let object_binaries =
             List.map info.build_and_install_as_objects ~f:(fun exe ->
               Option.iter exe.install_as ~f:(fun ocamlfind_package ->
                 let libname =
                   Printf.ksprintf LN.of_string !"%{Path}/%s.exe" dir exe.name
                 in
                 check_install_as ~libname ~pkg_name ~ocamlfind_package);
               Library
                 { internal_name = exe.name
                 ; kind          = Normal
                 ; oasis_kind    = Object
                 ; wrapped       = false
                 ; short_desc    = None
                 ; modules       = [String.capitalize exe.name]
                 ; path          = buildable.path
                 ; install_as    = exe.install_as
                 ; c_files       = []
                 })
           in
           (buildable, virtual_opam_deps,
            normal_binaries @ object_binaries))
         | _ -> None))
      *>>= fun res ->
      let concat_result res =
        List.fold res ~init:([], [], []) ~f:(fun (acc1, acc2, acc3) (x1, x2, x3) ->
          (x1 :: acc1, x2 @ acc2, x3 @ acc3))
      in
      let buildables, virtual_opam_deps, things = concat_result res in
      let internal_libs_to_build =
        List.concat_map buildables ~f:(fun b -> b.dependencies)
        |> List.filter_map ~f:(fun dep ->
          match dep.kind with
          | Global _ -> None
          | Local x -> Some (LN.of_string x.internal_name))
        |> LN.Set.of_list
      in
      (Dep.all @@ List.filter_map jbuilds ~f:(fun (dir, jbuild) ->
         match jbuild with
         | `library { name; public_release = Internal info; public_name = None; _ }
           as jbuild when Set.mem internal_libs_to_build name ->
           Some (export_library dc ~package_map ~dir ~conf ~info ~pkg_name ~jbuild)
         | _ -> None))
      *>>= fun res ->
      let buildables', virtual_opam_deps', things' = concat_result res in
      return (buildables @ buildables',
              virtual_opam_deps @ virtual_opam_deps',
              things @ things')

    let stable_version_file = Path.root_relative "public-release/version"
    let stable_version =
      Dep.file_exists stable_version_file *>>= function
      | false -> return None
      | true  ->
        Dep.contents stable_version_file
        *>>| fun s ->
        Some (String.strip s)

    let rule dc ~package ~conf =
      let file = path ~package in
      let dir = repo_path package in
      Rule.create ~targets:[file] (
        Package_map.load
        *>>= fun package_map ->
        Dep.both
          (things_to_export dc ~package_map ~pkg_dir:dir ~conf)
          (Dep.both
             (Dep.contents (relative ~dir "descr"))
             stable_version)
        *>>| fun ((buildables, virtual_opam_deps, things), (descr, stable_version)) ->
        let libraries, executables =
          List.partition_map things ~f:(function
            | Library    lib -> `Fst lib
            | Executable exe -> `Snd exe)
        in
        let libraries =
          List.sort libraries ~cmp:(fun (a : T.Library.t) b ->
            String.compare a.internal_name b.internal_name)
        in
        let synopsis, long_description =
          match String.split_lines descr with
          | x :: l -> (x, l)
          | [] -> ("<no description>", [])
        in
        let buildables =
          List.map buildables ~f:(fun b -> (b.path, b))
          |> String.Map.of_alist_reduce ~f:(fun b1 b2 ->
            if [%compare.equal: T.Buildable.t] b1 b2 then
              b1
            else
              failwiths
                "Incompatible buildables for path"
                [%sexp
                  { package    = (package : string)
                  ; buildable1 = (b1 : T.Buildable.t)
                  ; buildable2 = (b2 : T.Buildable.t)
                  }]
                Fn.id)
          |> Map.data
        in
        let dependencies =
          List.concat_map buildables ~f:(fun b ->
            List.filter_map b.dependencies ~f:(fun dep ->
              match dep.kind with
              | Local _ -> None
              | Global { opam_package; _ } ->
                Some { T.Package.Dependency.
                       opam_package
                     ; context  = Both (* Runtime as well for For the .runtime-lib libs *)
                     ; internal = dep.internal }) @
            List.map b.js_ppxs      ~f:(fun ppx ->
              { T.Package.Dependency.
                opam_package = ppx.opam_package
              ; context      = Both
              ; internal     = ppx.opam_package <> "js_of_ocaml" }))
        in
        let dependencies =
          if List.for_all buildables ~f:(fun b -> List.is_empty b.js_ppxs) then
            dependencies
          else
            { opam_package = "ppx_driver"
            ; context      = Both (* it is sometimes a runtime dep as well
                                     (toplevel_expect_tests for instance) *)
            ; internal     = true } :: dependencies
        in
        let name = Path.basename dir in
        let dependencies =
          if name = "js-build-tools" then
            dependencies
          else
            { opam_package = "js-build-tools"
            ; context      = Build
            ; internal     = true } :: dependencies
        in
        let dependencies = dependencies @ virtual_opam_deps in
        let dependencies =
          let module S =
            Set.Make(struct
              type t = T.Package.Dependency.t [@@deriving sexp]
              let compare (a : t) (b : t) =
                let d = String.compare a.opam_package b.opam_package in
                if Int.(=) d 0 then begin
                  assert (Bool.(=) a.internal b.internal);
                  assert ([%compare.equal: T.Buildable.Dependency.Context.t] a.context b.context);
                end;
                d
            end)
          in
          S.of_list dependencies
          |> S.to_list
          |> List.filter ~f:(fun dep -> dep.opam_package <> "ocaml")
        in
        let pkg : T.Package.t =
          { name
          ; synopsis
          ; long_description
          ; libraries
          ; executables
          ; buildables
          ; dependencies
          ; stable_version
          ; copyright_start     = conf.copyright_start
          ; dir_mapping         = List.map conf.dirs ~f:(Tuple.T2.map_snd ~f:Path.to_string)
          ; file_list_filename  = files_to_copy_path ~dir      |> Path.to_absolute_string
          ; install_extra       = conf.install_extra
          }
        in
        Action.save ~target:file (pkg |> T.Package.sexp_of_t |> Sexp.to_string_hum)
      )

    let load ~package =
      Dep.contents (path ~package)
      *>>| fun s ->
      Sexp.of_string_conv_exn (String.strip s) [%of_sexp: T.Package.t]
  end

  let sed_script = root_relative "public-release/sed-manifest-files"

  let files_to_copy_rule ~dir ~(conf : Public_repo.t) =
    let target = files_to_copy_path ~dir in
    Rule.create ~targets:[target] (
      Dep.all_unit
        [ Dep.path sed_script
        ; Dep.path public_release_files
        ]
      *>>| fun () ->
      bashf ~dir:Path.the_root
        !"{ egrep %{quote} %{quote} | sed -rf %{quote}; printf '%%s' %{quote}; } > %{quote}"
        (List.map conf.dirs ~f:(fun (_, p) -> Path.to_string p) |> or_regexp_for_grep
         |> sprintf "^(%s)/")
        (Path.to_string public_release_files)
        (Path.to_string sed_script)
        (List.map conf.additional_files ~f:(fun p -> Path.to_string p ^ "\n")
         |> String.concat)
        (Path.to_string target)
    )

  let bin = root_relative "public-release/bin"
  let fe_bin = Path.absolute "/j/office/app/fe/prod/bin/fe"
  let create_tarball = relative ~dir:bin "create_tarball.exe"
  let forbidden_regexps = root_relative "public-release/forbidden-regexps"

  let bin_tarball_filename ~switch = sprintf "bin.%s.lzo" switch

  let bin_checksum_filename ~switch = bin_tarball_filename ~switch ^ ".md5sum"
  let bin_checksum_path ~dir ~switch = relative ~dir (bin_checksum_filename ~switch)

  let build_repo = root_relative "public-release/bin/build-pkg/client/build_pkg.exe"

  let load_build_server_config =
    Dep.contents build_servers_config
    *>>| fun contents ->
    Sexp.of_string_conv_exn
      (String.strip contents) [%of_sexp: (string * Host_and_port.t) list]

  let depends_on_file_but_don't_care_about_changes path =
    Dep.action (Dep.path path *>>= fun () ->
                return (Action.process ~dir:Path.the_root "/bin/true" []))

  let build_rule ~dir ~switch =
    let checksum_path = bin_checksum_path ~dir:(repo_path (basename dir)) ~switch in
    Rule.create ~targets:[checksum_path] (
      Dep.both
        (Metadata.load ~package:(basename dir))
        load_build_server_config
      *>>= fun (pkg, cfg) ->
      let filter_deps packages =
        String.Set.of_list packages
        |> Set.elements
        |> List.filter ~f:((<>) pkg.name)
      in
      let internal_deps =
        List.filter_map pkg.dependencies ~f:(fun d ->
          if d.internal
          then Some d.opam_package
          else None)
      in
      let internal_deps = filter_deps internal_deps in
      Dep.all_unit
        (List.concat
           [ [ Dep.path (dist_checksum ~dir)
             (* We need the build_repo exe, but don't want to trigger recompilation of
                everything everytime we hack on it. If the user do want to force
                recompilation, they can always delete the executable by hand *)
             ; depends_on_file_but_don't_care_about_changes build_repo
             ]
           ; List.map internal_deps ~f:(fun pkg ->
               (* Depend on the checksum file rather than the file itself as the archive
                  contains file times. *)
               Dep.path (bin_checksum_path ~dir:(repo_path pkg) ~switch))
           ])
      *>>| fun () ->
      let host_and_port = List.Assoc.find_exn cfg ~equal:String.equal switch in
      Action.process ~dir:Path.the_root (Path.to_string build_repo)
        ["build";
         "-host"; Host_and_port.host host_and_port;
         "-port"; Host_and_port.port host_and_port |> Int.to_string;
         repo_path pkg.name |> Path.to_string; "-checksum-file";
         checksum_path |> Path.to_string]
    )

  let rules dc ~dir (conf : Public_repo.t) =
    let tarball = tarball ~dir in
    let dist_checksum = dist_checksum ~dir in
    let conf =
      { conf with
        dirs =
          List.concat
            [ [ (".", public_release_common_files) ]
            ; conf.dirs
            ; [ (".", dir) ]
            ]
      }
    in
    let pkg_name = basename dir in
    let create_tarball =
      Rule.create ~targets:[tarball; dist_checksum] (
        let files_to_copy = files_to_copy_path ~dir in
        Dep.all_unit
          [ Dep.path create_tarball
          ; Dep.path fe_bin
          ; Dep.path (Metadata.path ~package:pkg_name)
          ; Dep.path forbidden_regexps
          ]
        *>>= fun () ->
        Dep.contents files_to_copy
        *>>= fun s ->
        Dep.all_unit (List.map (String.split_lines s) ~f:(fun s ->
          Dep.path (root_relative s)))
        *>>| fun () ->
        bashf ~dir:Path.the_root
          !"%{quote} %{quote}"
          (Path.to_string create_tarball)
          (Path.to_string (Metadata.path ~package:pkg_name))
      )
    in
    List.concat
      [ [ create_tarball
        ; files_to_copy_rule ~dir ~conf
        ; Package_map.jbuilds_rule ~dir ~conf
        ; Metadata.rule dc ~package:pkg_name ~conf
        ]
      ; List.map opam_switches ~f:(fun switch -> build_rule ~dir ~switch)
      ]
end
