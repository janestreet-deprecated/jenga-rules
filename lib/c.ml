open Core
open Import

(* Terminology note:
   - [include] is the string in quotes or angle brackets given to #include
   - [include path] are paths given to the -I flag of c compilers

   Computing c dependencies is tricky. The main problem is that gcc just goes around the
   filesystem looking for files, and we can't tell in advance where those files are
   (because you can #include files are arbitrary relative or absolute locations).
   So what we do instead is call gcc with the dependencies we know so far, from its output
   guess what #include the files contained and overapproximate what gcc may have read, and
   call it again with all those files up to date until we reach a fix point.
   When included files are not generated, this is mostly ok. The only problem is that
   included files could contain syntax errors that cpp chokes on. But this should be
   really rare, and since we don't cache failures, restarting jenga puts it back in a
   good state.
   In the presence of generated files, things are more complicated. cpp could be loading
   wrong files (if a generated file is missing and a match is found later in the include
   paths) or stale files (if a generated file exists but isn't up to date, because we
   don't know what gcc reads).

   Known problems right now are:
   1. jenga won't force generated files that are #include with a path relative to the file
      (unless the file happens to be in the search path), to be built.  Occurrences of
      this can be fixed manually by including the file relative to the search path.

   2. syntax errors combined with generated files means we can include the wrong files,
      and never force the correct file to be built.

      Consider

       $ mkdir a
       $ mkdir b
       $ echo "#inclu" > a/x.h
       $ echo "#include <x.h>" | cpp -M -MG -I b -I a
       In file included from <stdin>:1:
       a/x.h:1:2: error: invalid preprocessing directive #inclu

      But then

       $ touch b/x.h
       $ echo "#include <x.h>" | cpp -M -MG -I b -I a
       -:  b/x.h

      If b/x.h is a file that is buildable but missing, I think we will never recover.

      As we discussed, error message parsing is complicated and brittle and doesn't
      completely solve the problem:

       echo '#define X' > a.h
       cat > b.h <<EOF
       #ifdef X
       #include <c.h>
       #else
       #endif
       EOF
       echo '#include <a.h>' >> x.c
       echo '#include <b.h>' >> x.c

      Here, [gcc -I . -M x.c] will not tell you anything about [a.h], although [a.h] might
      be stale and the correct behaviour would be to rebuild it.

      I'm not sure what to do about it.
      I think the thing you describe did happen to me in the everything-buildable world of
      32-artifacts-directory feature, so it's not a purely hypothetical bug.

   3. A given call to gcc tells us more dependencies, but we don't know when that becomes
      stale, so we can have too many dependencies. Because we only depend on files we can
      find, we don't have errors "no rule or source found for target". But we can create
      cyclic dependency (or needless rebuilds) this way although it's hard to imagine a
      practical problem it creates.

   4. If some files to be included are symlinks to regular files, instead of regular
      files, we will not set up dependencies on them.

   Among some ideas that we had, in case we more problems down the line:
   - we can look at the file names in the error messages, when cpp fails, to know what we
     should depend on. That's brittle and probably compiler-dependent.
   - we could run gcc without -MG, because the error messages sometimes contain more
     information than the actual output. It also contains less information in other cases,
     so we'd have to run both with -MG and without and somehow combine the results.
   - strace/ldd magic, to inspect what files/dirs gcc reads
*)

module Flavor = struct
  module T = struct
    type t = [`C | `Cxx] [@@deriving sexp, compare]
    let hash = Hashtbl.hash
  end

  include T
  include Comparable.Make(T)
  include Hashable.Make(T)

  let prog = function
    | `C -> "gcc"
    | `Cxx -> "g++"

  let language = function
    | `C -> "c"
    | `Cxx -> "c++"
end

let parse_gcc_MM_output s =
  let _target, dependencies =
    Option.value_exn (String.lsplit2 s ~on:':')
      ~message:"gcc produced no dependency information"
  in
  let dependencies = String.map dependencies ~f:(function '\n' -> ' ' | c -> c) in
  let words = String.split dependencies ~on:' ' in
  List.filter words ~f:(function
  | "" | "\\" -> false
  | _ -> true)

let () =
  let some_gcc_output =
"bigstring_stubs.o: bigstring_stubs.c config.h ocaml_utils.h jane_common.h \\
 unix_utils.h ../kernel/lib/params.h socketaddr.h
"
  in
  [%test_eq: string list] (parse_gcc_MM_output some_gcc_output)
    [ "bigstring_stubs.c"; "config.h"; "ocaml_utils.h"; "jane_common.h";
      "unix_utils.h"; "../kernel/lib/params.h"; "socketaddr.h" ]

let include_paths_of_cflags_as_strings flags =
  let rec loop ~acc = function
    | [] -> List.rev acc
    | "-I" :: path :: xs ->
      loop ~acc:(path :: acc) xs
    | flag :: xs ->
      match String.chop_prefix flag ~prefix:"-I" with
      | None ->
        loop ~acc xs
      | Some path ->
        loop ~acc:(path :: acc) xs
  in
  loop ~acc:[] flags

let include_paths_of_cflags ~dir flags =
  include_paths_of_cflags_as_strings flags
  |> List.map ~f:(Path.relative_or_absolute ~dir)

let (//) xs ys = (* items in result are ordered like xs *)
  let set = String.Hash_set.of_list ys in
  List.filter xs ~f:(fun x -> not (Hash_set.mem set x))

let file_exists_or_is_buildable path =
  Dep.glob_listing (Glob.create_from_path ~kinds:(Some [`File]) path)
  *>>| function [] -> false | _ :: _ -> true

let file_existence_or_buildability path =
  Dep.glob_change (Glob.create_from_path ~kinds:(Some [`File]) path)

let path_and_its_existence p =
  file_existence_or_buildability p *>>= fun () ->
  file_exists_or_is_buildable p *>>= function
  | false -> Dep.return ()
  | true -> Dep.path p

(* [relative_that_stays_below_the_root ~dir s] follows the '/'-separated path components in [s] starting
   from [dir]. Returns None if there is a ".." trying to go outside the root.
*)
let starts_with_slash s = String.is_prefix ~prefix:"/" s
let relative_that_stays_below_the_root =
  let rec go dir l = match l with
    | [] -> Some dir
    | ".." :: l ->
      (match Path.precise_dirname dir with
       | `Root_or_repo_root -> None
       | `Ok parent -> go parent l)
    | ("." | "") :: l -> go dir l
    | seg :: l -> go (relative ~dir seg) l
  in
  fun ~dir s ->
    assert (not (starts_with_slash s));
    go dir (String.split s ~on:'/')
;;

let known_deps ~dir ~flags ~includes =
  let include_search_path = include_paths_of_cflags ~dir flags in
  Dep.all_unit (List.concat_map includes ~f:(fun s ->
    if starts_with_slash s then [path_and_its_existence (Path.absolute s)]
    else
      List.map include_search_path ~f:(fun include_dir ->
        match relative_that_stays_below_the_root ~dir:include_dir s with
        | Some p -> path_and_its_existence p
        | None -> Dep.return ()
      )
  ))

let get_system_include_dirs =
  let open Async in
  let f flavor =
    run_action_now_stdout (
      bashf ~dir:Path.the_root
        !"true | cpp -x %{quote} -Wp,-v |& \
          sed -r -e '1,/^#include <\\.\\.\\.> search starts here/d' | \
          sed -r -e '/End of search list/,$d'"
        (Flavor.language flavor))
  in
  Deferred.Memo.general (module Flavor) f

let system_include_dirs flavor =
  Dep.deferred (fun () -> get_system_include_dirs flavor)
  *>>| fun contents ->
  String.split_lines contents
  |> List.map ~f:String.lstrip

(* Takes the list of paths produced by gcc -MM and converts it to what we think was
   written in the corresponding #include directives (a path relative to include search
   path).
   There might be several possibilities, eg if you have a and a/b in the search path, and
   gcc outputs a/b/c.h, the source could say b/c.h or c.h. In this case, we
   overapproximate and return them all.
   We want to know what is written in the source because gcc will tell us only about the
   files it did find, not the files it didn't find. Think of a search path of a/ and b/,
   and a file that #include "c.h". Even if gcc tells us b/c.h, it looked first in a/, so
   we need to force a/c.h to exist if it can be built but isn't built, or recompute
   dependencies if it is manually created.

*)
let interpret_gcc_MM_output flavor include_paths gcc_output =
  system_include_dirs flavor
  *>>| fun system_include_dirs ->
  (* The additional "." here is a work-around to ensure we build a.h when there is a
     '#include "a.h"' in the C file. *)
  let include_paths = include_paths @ system_include_dirs @ ["."] in
  (* gcc normalizes the front of the paths, for instance a file relative to "-I .///./"
     is returned as just the relative path, so we replicate the behavior. *)
  let rec normalize_include_path p =
    match String.chop_prefix ~prefix:"./" p with
    | Some p -> normalize_include_path (String.lstrip ~drop:(Char.(=) '/') p)
    | None ->
      match p with
      | "." -> ""
      | p -> if String.is_suffix ~suffix:"/" p then p else p ^ "/"
  in
  let include_paths = List.map include_paths ~f:normalize_include_path in
  (* We assume that if path [p] is on the [include_path] and [gcc] reports [p/q],
     this means there might have been a '#include <q>' in the source code. *)
  List.concat_map gcc_output ~f:(fun path_from_gcc ->
    match List.filter_map include_paths ~f:(fun include_path ->
      match String.chop_prefix ~prefix:include_path path_from_gcc with
      | None -> None
      | Some s ->
         if String.is_prefix ~prefix:"/" s
         then None (* avoid saying absolute paths are relative (to "." presumably) *)
         else Some s)
    with
    | [] ->
      (* Can happen when files include absolute paths, or when gcc just tells you the
         contents of an include for a file it can't find, or the file is relative to
         [.]. *)
      [path_from_gcc]
    | l ->  l)
  |> String.Set.of_list
  |> String.Set.to_list

let deps ~dir ~source ~flavor ~flags =
  (* Use "gcc -M" to discover dependencies for c/cxx compilation,
     using same flags as for the compilation *)
  let include_search_path = include_paths_of_cflags_as_strings flags in
  let rec gcc_mm_loop i last_includes =
    (* This loop should terminate because (assuming no change to the filesystem other than
       jenga):
       - the output of gcc is deterministic
       - each new iteration either drops a bad dependency, or adds a good dependency, or
         we stop. So we're getting closer to the goal.

       The other question is: over time, can this chain grow in length and cause us
       to go over this max number? Well I think not, because when the file X changes,
       it's as if we're computing the dependendencies the first time and X is stale
       and we are making it up to date. *)
    if Int.(>=) i 50 then (
      (* In case we encounter pathological cases *)
      failwiths "gcc -MM cannot seem to reach a fixpoint"
        (`On (sprintf !"%{Path}/%s" dir source), `Includes last_includes)
        [%sexp_of: [ `On of string ] * [ `Includes of string list ]]
    );
    Dep.action_stdout (
      known_deps ~dir ~flags ~includes:last_includes
      *>>| fun () ->
      (* gcc -Werror fails on the #warning directive. Unfortunately, such warnings can be
         triggered when running gcc -M, if we are missing some headers. So we turn them
         off while looking for dependencies, and if it is a genuine error, it will be
         triggered when compiling. *)
      (* We add the iteration number to the command, because otherwise we would run the
         exact same job at each iteration, which means jenga would only store the result
         of the last one. And the result of the last one can't be reused for the previous
         ones because jenga no longer knows to avoid rerunning actions when their
         dependencies shrink.
         Because we always need >= 2 iterations, this results in jenga
         rerunning all the calls to gcc -MM on restart. *)
      let noop_that_depends_on_i = "-U__" ^ Int.to_string i in
      Action.process ~dir (Flavor.prog flavor)
        (flags @ [noop_that_depends_on_i; "-Wno-error"; "-M"; "-MG"; source])
    ) *>>= fun stdout ->
    let gcc_MM_output = parse_gcc_MM_output stdout in
    interpret_gcc_MM_output flavor include_search_path gcc_MM_output
    *>>= fun current_includes ->
    let previously_unknown_includes = current_includes // last_includes in
    match previously_unknown_includes with
    | [] -> return (`Includes last_includes, `Search_path include_search_path)
    | _::_ ->
      (*
        It is possible for "gcc -MM" to discover a false dependency, if:
        - there is a generated header file; which is currently missing
        - the generated header causes a following #include to be suppressed

        The false dependency would be avoided if we add only the first new dependency on
        each iteration of the loop, [gcc_mm_loop (i+1) (last_includes @ [x1])], but this
        would cause "gcc -MM" to be called N times each time the .c is touched (for N =
        the number of headers included).

        The current solution causes "gcc -MM" to normally be called just twice (at least
        in the absence of generated files):
        - the first call knows only the .c source as a dependency.
        - the second call call knows all the dependencies.
        The loop then terminates because [previously_unknown_includes] is empty.

        These false dependencies can cause two problems:
        - we may depend on a file that we don't need, and may not even be buildable
        - compilation can work incrementally but not from scratch, since if all headers
          exists, we only get the right dependencies.
      *)
      gcc_mm_loop (i+1) current_includes
  in
  gcc_mm_loop 0 [source]
