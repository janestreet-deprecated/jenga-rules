open Core
open Import
open Jbuild_types
open Ocaml_types

let build_servers =
  let var = "PUBLIC_RELEASE_BUILD_SERVERS" in
  Var.peek
    (Var.map (Var.register var) ~f:(fun x ->
       let s = Option.value x ~default:Config.public_release_build_servers_default in
       Sexp.of_string_conv_exn ~source:(Other ("variable " ^ var))
         s [%of_sexp: (string * Host_and_port.t) list]))

let opam_switches = List.map build_servers ~f:fst

module Make(Jenga_root : sig
    module Lib_modules : sig
      type t
      val impls_and_intfs : t -> BN.t list
      val load : dir:Path.t -> libname:LN.t -> t Dep.t
    end

    module User_or_gen_config : sig
      val load : dir: Path.t -> Jbuild.t list Dep.t
    end

    val public_release_files : Path.t
    val deep_unignored_subdirs : dir:Path.t -> Path.t list Dep.t
    val expand_user_action
      :  User_action.Unexpanded.t
      -> artifacts:Named_artifact.Store.t
      -> dir:Path.t
      -> targets:string list
      -> deps:Dep_conf.t list
      -> string User_action.t Dep.t
    val deps_conf_to_deps : dir:Path.t -> Dep_conf.t list -> unit Dep.t list
  end) : sig

  val rules
    :  artifacts:Named_artifact.Store.t
    -> dir:Path.t
    -> Public_repo.t
    -> Rule.t list

  module Public_libmap : sig
    (** Rules to build public-release/repos/public-libmap.sexp *)
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

  (* Dependencies computed while creating the tarball *)
  let package_deps ~dir = relative ~dir "package-deps.sexp"

  (* Dependencies computed while creating the tarball *)
  let load_package_deps ~dir =
    Dep.contents (package_deps ~dir)
    *>>| fun s ->
    [%of_sexp: T.Package_dep.t list] (Sexp.of_string (String.strip s))

  (* List of files to copy in the given package *)
  let files_to_copy_filename = "files-to-copy"
  let files_to_copy_path ~dir = relative ~dir files_to_copy_filename

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
  module Public_libmap : sig
    val jbuilds_rule : dir:Path.t -> conf:Public_repo.t -> Rule.t
    val global_rules : dir:Path.t -> Rule.t list

    val public_libmap : Path.t
  end = struct
    (* List of jbuilds that a given external packages looks at *)
    let jbuilds_filename = "jbuilds"
    let jbuilds_path ~dir = relative ~dir jbuilds_filename
    let jbuilds_rule ~dir ~(conf : Public_repo.t) =
      let target = jbuilds_path ~dir in
      Rule.create ~targets:[target] (
        Dep.both
          (Dep.path public_release_files)
          (Dep.List.concat_map conf.dirs ~f:(fun (_, dir) -> deep_unignored_subdirs ~dir))
        *>>| fun ((), unignored_subdirs) ->
        bashf ~dir ~sandbox:Sandbox.hardlink
          !"{ egrep %{quote} %{quote} || true; } > %{quote}"
          (List.map unignored_subdirs ~f:Path.to_string
           |> or_regexp_for_grep
           |> sprintf "^(%s)/jbuild$")
          (reach_from ~dir public_release_files)
          (Path.basename target)
      )

    let dirs_exported_by_package ~package =
      Dep.contents (jbuilds_path ~dir:(repo_path package))
      *>>| fun s ->
      String.split_lines s
      |> List.map ~f:root_relative
      |> List.map ~f:Path.dirname

    (* The package_map file contains a mapping from directories to package exporting this
       directory *)
    let package_map_file = relative ~dir:repos "package-map.sexp"
    let all_repos_file   = relative ~dir:repos ".repos"
    let public_libmap    = relative ~dir:repos "public-libmap.sexp"

    let create_all_repos_file ~dir =
      Dep.path public_release_files
      *>>| fun () ->
      bashf ~dir ~sandbox:Sandbox.hardlink
        !"sed -nr %{quote} %{quote} > %{quote}"
        (Path.to_string repos
         |> escape_for_egrep
         |> sprintf "s|^(%s/[^/]*)/jbuild$|\\1|p")
        (Path.reach_from ~dir public_release_files)
        (Path.basename all_repos_file)

    let all_jbuilds_path = relative ~dir:repos jbuilds_filename
    let all_jbuilds =
      Dep.contents all_jbuilds_path
      *>>| fun s ->
      List.map (String.split_lines s) ~f:Path.root_relative
    let all_jbuilds_rule =
      Rule.create ~targets:[all_jbuilds_path] (
        Dep.contents all_repos_file
        *>>= fun s ->
        let all_packages =
          List.map (String.split_lines s) ~f:Filename.basename
        in
        Dep.List.concat_map all_packages ~f:(fun package ->
          dirs_exported_by_package ~package)
        *>>| fun dirs ->
        Action.save ~target:all_jbuilds_path
          (String.concat ~sep:"\n"
             (List.map dirs ~f:(fun dir ->
                Path.to_string (Path.relative ~dir "jbuild")))))

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
        (Sexp.to_string ([%sexp_of: string Path.Map.t] map))

    let load_package_map =
      Dep.contents package_map_file
      *>>| fun s ->
      Sexp.of_string_conv_exn ~source:(File package_map_file)
        s [%of_sexp: string Path.Map.t]

    let create_public_libmap =
      Dep.both
        (deep_unignored_subdirs ~dir:Path.the_root)
        load_package_map
      *>>= fun (dirs, package_map) ->
      Dep.List.concat_map dirs ~f:(fun dir ->
        User_or_gen_config.load ~dir
        *>>= Dep.List.concat_map ~f:(function
          | `library (lib : Library_conf.t) -> begin
              let name = LN.to_string lib.name in
              match lib.external_lib with
              | None -> begin
                  match Map.find package_map dir with
                  | None ->
                    return [(name, T.Lib_info.Js_not_released)]
                  | Some opam_package ->
                    let public_name =
                      Option.map lib.public_name ~f:Findlib_package_name.to_string
                    in
                    let info =
                      T.Lib_info.Js_released
                        { opam_package
                        ; public_name
                        }
                    in
                    return [(name, info)]
                end
              | Some { repackaged; opam_package } ->
                (* We check that this field is set when [external_lib] is set in
                   [jbuild_types.ml] *)
                let public_name =
                  Findlib_package_name.to_string (Option.value_exn lib.public_name)
                in
                let opam_package =
                  match opam_package with
                  | None -> T.opam_of_ocamlfind public_name
                  | Some s -> s
                in
                if not repackaged then begin
                  let info =
                    T.Lib_info.External
                      { opam_package
                      ; public_name
                      ; wrapper = None
                      }
                  in
                  return [(name, info)]
                end else begin
                  Lib_modules.load ~dir ~libname:lib.name
                  *>>| fun lib_modules ->
                  let modules =
                    List.map (Lib_modules.impls_and_intfs lib_modules) ~f:BN.to_module
                    |> List.filter ~f:(fun s -> not (String.is_empty s))
                  in
                  let info =
                    T.Lib_info.External
                      { opam_package
                      ; public_name
                      ; wrapper =
                          Some
                            { name = LN.to_module lib.name
                            ; modules
                            }
                      }
                  in
                  [(name, info)]
                end
            end
          | _ -> return []))
      *>>| fun l ->
      let l =
        l @
        List.map From_compiler_distribution.all ~f:(fun compiler_pkg ->
          let name = From_compiler_distribution.to_string compiler_pkg in
          let public_name =
            From_compiler_distribution.ocamlfind_package compiler_pkg
            |> Findlib_package_name.to_string
          in
          let opam_package =
            match name with
            | "threads" -> "base-threads"
            | _ -> "ocaml"
          in
          let info : T.Lib_info.t =
            External { opam_package; public_name; wrapper = None }
          in
          (name, info))
      in
      let l =
        List.fold_left l ~init:[] ~f:(fun acc (name, info) ->
          match T.Lib_info.public_name info with
          | Some pname when pname <> name -> (name, info) :: (pname, info) :: acc
          | _ -> (name, info) :: acc)
      in
      Action.save ~target:public_libmap
        (Sexp.to_string [%sexp (l : (string * T.Lib_info.t) list)])
    ;;

    let global_rules ~dir =
      if not (Path.equal dir repos) then [] else begin
        let all_repos =
          Dep.contents all_repos_file
          *>>| fun s ->
          List.map (String.split_lines s) ~f:Path.root_relative
        in
        let bins =
          Dep.alias
            (Alias.create ~dir:(Path.root_relative "public-release/bin") "DEFAULT")
        in
        List.concat
          [ [ all_jbuilds_rule
            ; Rule.create ~targets:[package_map_file] create_package_map
            ; Rule.create ~targets:[all_repos_file  ] (create_all_repos_file ~dir)
            ; Rule.create ~targets:[public_libmap   ] create_public_libmap
            (* Note: the .tarballs alias here is invoked by hydra, don't change it! *)
            ; Rule.alias (Alias.create ~dir "tarballs")
                [ bins
                ; all_repos *>>= fun paths ->
                  Dep.all_unit
                    (List.map paths ~f:(fun dir ->
                       Dep.path (Path.relative ~dir "dist.tar.gz")))
                ]
            ; Rule.alias (Alias.create ~dir "metadata")
                [ bins
                ; all_repos *>>= fun paths ->
                  Dep.all_unit
                    (List.map paths ~f:(fun dir ->
                       Dep.path (Path.relative ~dir ".metadata.sexp")))
                ]
            ; Rule.alias (Odoc.alias ~dir)
                [ bins
                ; all_jbuilds *>>= fun jbuilds ->
                  Dep.all_unit
                    (List.map jbuilds ~f:(fun jbuild ->
                       Dep.alias (Odoc.alias ~dir:(Path.dirname jbuild))))
                ]
            ; Rule.alias (Alias.create ~dir "binaries")
                (List.map opam_switches ~f:(fun sw ->
                   Dep.alias (Alias.create ~dir ("binaries." ^ sw))))
            ]
          ; List.map opam_switches ~f:(fun sw ->
              Rule.alias (Alias.create ~dir ("binaries." ^ sw))
                [ bins
                ; all_repos *>>= fun paths ->
                  Dep.all_unit (List.map paths ~f:(fun dir ->
                    Dep.path (Path.relative ~dir (sprintf "bin.%s.lzo.md5sum" sw))))
                ])
          ]
      end
  end

  (* Build the .metadata.sexp file, which is used both for creating the tarballs and get
     find dependencies between external packages. *)
  module Metadata : sig
    val path : package:string -> Path.t
    val load : package:string -> T.Package.t Dep.t
    val rule
      :  artifacts:Named_artifact.Store.t
      -> package:string
      -> conf:Public_repo.t
      -> Rule.t
  end = struct
    let filename = "metadata.sexp"
    let path ~package = relative ~dir:(repo_path package) filename

    let stable_version_file = Path.root_relative "public-release/version"
    let stable_version =
      Dep.file_exists stable_version_file *>>= function
      | false -> return None
      | true  ->
        Dep.contents stable_version_file
        *>>| fun s ->
        Some (String.strip s)

    let expand_hook ~artifacts hook =
      match hook with
      | None -> return None
      | Some action ->
        expand_user_action
          (Bash action)
          (* The action is executed in .jenga.tmp/public-release-PACKAGE-XXX/PACKAGE *)
          ~dir:(Path.root_relative ".jenga.tmp/\000/\000")
          ~artifacts
          ~deps:[]
          ~targets:[]
        *>>| function
        | Bash cmd -> Some cmd
        | Shexp _ -> assert false

    let rule ~artifacts ~package ~(conf : Public_repo.t) =
      let file = path ~package in
      let dir = repo_path package in
      Rule.create ~targets:[file] (
        Dep.both
          stable_version
          (Dep.both
             (expand_hook ~artifacts conf.hooks.pre_dist)
             (expand_hook ~artifacts conf.hooks.pre_opam))
        *>>| fun (stable_version, (pre_dist, pre_opam)) ->
        let name = Path.basename dir in
        let dir_mapping = List.map conf.dirs ~f:(Tuple.T2.map_snd ~f:Path.to_string) in
        let pkg : T.Package.t =
          { name                  = name
          ; synopsis              = conf.synopsis
          ; long_description      = String.split_lines conf.description
          ; stable_version        = stable_version
          ; copyright_start       = conf.copyright_start
          ; dir_mapping           = dir_mapping
          ; file_list_filename    = Path.to_absolute_string (files_to_copy_path ~dir)
          ; package_deps_filename = Path.to_absolute_string (package_deps ~dir)
          ; hooks                 = { pre_dist; pre_opam }
          }
        in
        Action.save ~target:file (pkg |> T.Package.sexp_of_t |> Sexp.to_string)
      )

    let load ~package =
      let path = path ~package in
      Dep.contents path
      *>>| fun s ->
      Sexp.of_string_conv_exn ~source:(File path)
        s [%of_sexp: T.Package.t]
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
      bashf ~dir ~sandbox:Sandbox.hardlink
        !"{ egrep %{quote} %{quote} | sed -rf %{quote}; printf '%%s' %{quote}; } > %{quote}"
        (List.map conf.dirs ~f:(fun (_, p) -> Path.to_string p) |> or_regexp_for_grep
         |> sprintf "^(%s)/")
        (reach_from ~dir public_release_files)
        (reach_from ~dir sed_script)
        (List.map conf.additional_files ~f:(fun p -> Path.to_string p ^ "\n")
         |> String.concat)
        (reach_from ~dir target)
    )

  let bin = root_relative "public-release/bin"
  let fe_bin = Path.absolute "/j/office/app/fe/prod/bin/fe"
  let create_tarball = relative ~dir:bin "create_tarball.exe"
  let forbidden_regexps = root_relative "public-release/forbidden-regexps"

  let bin_tarball_filename ~switch = sprintf "bin.%s.lzo" switch

  let bin_checksum_filename ~switch = bin_tarball_filename ~switch ^ ".md5sum"
  let bin_checksum_path ~dir ~switch = relative ~dir (bin_checksum_filename ~switch)

  let build_repo = root_relative "public-release/bin/build-pkg/client/build_pkg.exe"

  let depends_on_file_but_don't_care_about_changes path =
    Dep.action (Dep.path path *>>= fun () ->
                return (Action.process ~dir:Path.the_root "/bin/true" []))

  let build_rule ~dir ~switch =
    let checksum_path = bin_checksum_path ~dir:(repo_path (basename dir)) ~switch in
    Rule.create ~targets:[checksum_path] (
      Dep.both
        (Metadata.load ~package:(basename dir))
        (load_package_deps ~dir)
      *>>= fun (pkg, pkg_deps) ->
      let filter_deps packages =
        String.Set.of_list packages
        |> Set.elements
        |> List.filter ~f:((<>) pkg.name)
      in
      let internal_deps =
        List.filter_map pkg_deps ~f:(function
          | Internal pkg -> Some pkg
          | External _   -> None)
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
      let host_and_port = List.Assoc.find_exn build_servers ~equal:String.equal switch in
      Action.process ~dir:Path.the_root (Path.to_string build_repo)
        [ "build"
        ; "-host"; Host_and_port.host host_and_port
        ; "-port"; Host_and_port.port host_and_port |> Int.to_string
        ; repo_path pkg.name |> Path.to_string
        ; "-checksum-file"; checksum_path |> Path.to_string
        ]
    )

  let rules ~artifacts ~dir (conf : Public_repo.t) =
    let tarball = tarball ~dir in
    let dist_checksum = dist_checksum ~dir in
    let package_deps = package_deps ~dir in
    let conf =
      { conf with
        dirs =
          List.concat
            [ [ (".", public_release_common_files) ]
            ; conf.dirs
            ; [ (".", Path.relative ~dir "files" ) ]
            ]
      }
    in
    let pkg_name = basename dir in
    let create_tarball =
      Rule.create ~targets:[tarball; dist_checksum; package_deps] (
        let files_to_copy = files_to_copy_path ~dir in
        Dep.all_unit
          (Dep.path create_tarball
          :: Dep.path fe_bin
          :: Dep.path (Metadata.path ~package:pkg_name)
          :: Dep.path forbidden_regexps
          :: Dep.path Public_libmap.public_libmap
          :: Dep.path (root_relative ".fe/obligations-global.sexp")
          :: (Dep.contents files_to_copy
              *>>= fun s ->
              Dep.all_unit (List.map (String.split_lines s) ~f:(fun s ->
                Dep.path (root_relative s))))
          :: deps_conf_to_deps ~dir conf.deps)
        *>>| fun () ->
        bashf ~dir ~sandbox:Sandbox.hardlink
          !"%{quote} %{quote} -ocaml-bin %{quote} -relative-root %s"
          (Path.reach_from ~dir create_tarball)
          (Path.reach_from ~dir (Metadata.path ~package:pkg_name))
          Compiler_selection.compiler_bin_dir
          (Path.reach_from ~dir Path.the_root)
      )
    in
    List.concat
      [ [ create_tarball
        ; files_to_copy_rule ~dir ~conf
        ; Public_libmap.jbuilds_rule ~dir ~conf
        ; Metadata.rule ~artifacts ~package:pkg_name ~conf
        ]
      ; List.map opam_switches ~f:(fun switch -> build_rule ~dir ~switch)
      ]
end
