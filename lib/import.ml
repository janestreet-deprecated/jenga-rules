open Core
include (Jenga_lib.Api
         : module type of struct include Jenga_lib.Api end
         with module Var := Jenga_lib.Api.Var
         with module Path := Jenga_lib.Api.Path
         with module Action := Jenga_lib.Api.Action
         with module Rule := Jenga_lib.Api.Rule)
include String.Replace_polymorphic_compare

module List = struct
  include List
  let mem l elt ~equal = mem l elt ~equal (* make ~eq mandatory *)
  let mem_string l elt = mem l elt ~equal:String.equal
end

module Sexp = struct
  include Sexp

  let handle_result ~source : ('a, Parsexp.Conv_error.t) Result.t -> 'a = function
    | Ok x -> x
    | Error (Parse_error err) ->
      let pos = Parsexp.Parse_error.position err in
      Located_error.raise
        ~loc:{ source
             ; line      = pos.line
             ; start_col = pos.col
             ; end_col   = pos.col
             }
        (Parsexp.Parse_error.message err)
    | Error (Of_sexp_error err) ->
      let loc : Located_error.Loc.t =
        match Parsexp.Of_sexp_error.location err with
        | None -> { source; line = 1; start_col = 0; end_col = 0 }
        | Some loc ->
          { source
          ; line      = loc.start_pos.line
          ; start_col = loc.start_pos.col
          ; end_col   = loc.end_pos.offset - loc.start_pos.offset + loc.start_pos.col
          }
      in
      Located_error.raise ~loc
        (match Parsexp.Of_sexp_error.user_exn err with
         | Failure s -> s
         | exn -> to_string_hum (Exn.sexp_of_t exn))

  let of_string_conv_exn ~source s conv =
    handle_result ~source (Parsexp.Conv_single.parse_string s conv)

  let many_of_string_conv_exn ~source s conv =
    handle_result ~source (Parsexp.Conv_many.parse_string s conv)
end

module Var : sig

  include module type of struct include Jenga_lib.Api.Var end

  val register_bool : string -> default:bool        -> bool        t

  val register_enumeration :
    string ->
    choices  : 'a String.Map.t ->
    default  : string ->
    fallback : (string -> 'a option) ->
    ('a, [`Bad of string]) Result.t t

  val peek_register_bool : string -> default:bool -> bool

end = struct

  include Jenga_lib.Api.Var

  let printf_if_verbose ~or_:cond fmt =
    if cond then printf fmt
    else printf_verbose fmt

  let show_registration ?(choices = []) name ~default var =
    let value = peek ~dont_trigger:() var in
    let changed = String.(<>) value default in
    printf_if_verbose ~or_:changed "%s = %s%s" name value
      (match choices with
       | [] -> ""
       | _ :: _ -> sprintf " # choices: %s" (String.concat ~sep:" " choices))

  let register_with_default ?choices name ~default =
    let var = register_with_default ?choices name ~default in
    show_registration ?choices name ~default var;
    var

  let register_bool name ~default =
    register_with_default name ~default:(Bool.to_string default) ~choices:["true";"false"]
    |> map ~f:(function "false" | "0" -> false | _ -> true)

  let register_enumeration name ~choices:choice_map ~default ~fallback =
    (* We don't [assert (Map.mem choice_map default)] so [default] can be resolved by
       [choices] or [fallback] - the same resolution as used when the env-var has a value.
       When a value (default or over-ridden) is unresolved, the caller is notified using
       the [`Bad] result type *)
    let choices = String.Map.keys choice_map in
    let f string =
      match Map.find choice_map string with
      | Some v -> Ok v
      | None ->
         match fallback string with
         | Some v -> Ok v
         | None -> Error (`Bad string)
    in
    register_with_default name ~default ~choices
    |> map ~f

  let peek_register_bool  name ~default = peek (register_bool name ~default)

end

let force (lazy x) = x

module Path = struct
  include Jenga_lib.Api.Path
  let hash_fold_t acc t = hash_fold_string acc (to_string t)
  let precise_dirname path =
    let dir = dirname path in
    if dir = path
    then `Root_or_repo_root
    else `Ok dir

  let split =
    let rec split_acc acc path =
      if path = the_root
      then acc
      else split_acc (basename path :: acc) (dirname path)
    in
    fun path -> split_acc [] path
end

let relative = Path.relative
let reach_from = Path.reach_from
let root_relative = Path.root_relative
let basename = Path.basename
let dirname = Path.dirname
let suffixed ~dir name suf = relative ~dir (name ^ suf)

let return = Dep.return
let ( *>>= ) t f = Dep.bind t ~f
let ( *>>| ) = Dep.map

let quote = Shell.escape

let concat_quoted l = String.concat ~sep:" " (List.map ~f:quote l)

let ccopts = function
  | [] -> []
  | _ :: _ as l -> [ "-ccopt"; concat_quoted l ]
;;

let bash_prog = "bash"
let bash_args = [ "-e"; "-u"; "-o"; "pipefail"; "-c" ]

let bash ?sandbox ?ignore_stderr ~dir command_string =
  Jenga_lib.Api.Action.process ?sandbox ?ignore_stderr ~dir
    ~prog:bash_prog ~args:(bash_args @ [ command_string ]) ()

let bashf ?sandbox ?ignore_stderr ~dir fmt =
  ksprintf (fun str -> bash ?sandbox ?ignore_stderr ~dir str) fmt

let lines_of_string string =
  let lines = String.split string ~on:'\n' in
  List.filter lines ~f:(function "" -> false | _ -> true)

let words_of_string string =
  let words = String.split_on_chars string ~on:[' '; '\n'] in
  List.filter words ~f:(function "" -> false | _ -> true)

let file_words path =
  Dep.contents path *>>| words_of_string

let remove_dups_preserve_order xs =
  let set = String.Hash_set.create () in
  let rec loop acc = function
    | [] -> List.rev acc
    | x::xs ->
      if Hash_set.mem set x
      then loop acc xs
      else (Hash_set.add set x; loop (x::acc) xs)
  in
  loop [] xs

let remove_dups_and_sort xs =
  String.Set.to_list (String.Set.of_list xs)

module Action = struct
  type process =
    { prog : string
    ; args : string list
    ; dir : Path.t
    }
  let bash_process ~dir ~cmd =
    { dir; prog = bash_prog; args = bash_args @ [ cmd ] }

  include Jenga_lib.Api.Action

  let process ?env ?sandbox ?ignore_stderr ~dir prog args =
    let prog, args =
      match env with
      | None -> prog, args
      | Some env ->
        [%test_eq: bool] (String.mem prog '=') false;
        let env = List.map env ~f:(fun (key, data) -> sprintf "%s=%s" key data) in
        "/usr/bin/env", List.concat [ env; prog :: args; ]
    in
    process ?sandbox ?ignore_stderr ~dir ~prog ~args ()
  ;;

  let process' ?env ?sandbox ?ignore_stderr { dir; prog; args; } =
    process ?env ?sandbox ?ignore_stderr ~dir prog args
  ;;

  let process_with_redirected_stdout ~to_:target ~dir prog args =
    bashf ~dir !"tmp=\"$(mktemp --tmpdir)\"; \
                 %{concat_quoted} > \"$tmp\"; \
                 mv \"$tmp\" %{quote} \
                " (* 2015-01-11: could chmod -w at some point, but if did that
                     now, these read-only files would break old builds *)
      (prog :: args) (Path.reach_from ~dir target)
  ;;

  let write_string ?chmod_x string ~target =
    save ?chmod_x (string^"\n") ~target

end


module Rule = struct
  include Jenga_lib.Api.Rule

  let write_string ?chmod_x string ~target =
    create ~targets:[target] (return (Action.write_string ?chmod_x string ~target))

  let write_names names ~target =
    write_string (String.concat ~sep:" " names) ~target

end

let alias_dot_filename_hack ~dir dot_name =
  (* Sadly names given on the command line which begin with a dot (i.e. ".merlin") are
     currently always interpreted as a reference to an alias. Workaround this problem for
     specific instances by creating an alias to the dot-filename, named the same as the
     filename (minus the dot). *)
  let name = String.chop_prefix_exn dot_name ~prefix:"." in
  Rule.alias (Alias.create ~dir name) [Dep.path (relative ~dir dot_name)]

let dummy_position path =
  { Lexing.pos_fname = Path.to_string path; pos_cnum = 0; pos_bol = 0; pos_lnum = 1 }
;;
let failposf : pos:Lexing.position -> ('a, unit, string, unit -> 'b) format4 -> 'a =
  fun ~pos fmt ->
    let {Lexing.pos_fname; pos_lnum; pos_cnum; pos_bol} = pos in
    let col = pos_cnum - pos_bol in
    Located_error.raisef
      ~loc:{ source    = File (Path.relative_or_absolute ~dir:Path.the_root pos_fname)
           ; line      = pos_lnum
           ; start_col = col
           ; end_col   = col
           }
      fmt
