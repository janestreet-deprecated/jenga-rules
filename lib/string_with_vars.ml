open! Core

type var_syntax = Parens | Braces [@@deriving sexp_of, compare]

type item =
  | Text of string
  | Var of var_syntax * string
[@@deriving sexp_of]

type t = item list
[@@deriving sexp_of]

module Token = struct
  type t =
    | String of string
    | Open   of var_syntax
    | Close  of var_syntax

  let tokenise s =
    let len = String.length s in
    let sub i j = String.sub s ~pos:i ~len:(j - i) in
    let cons_str token_start i acc =
      if token_start = i then acc else String (sub token_start i) :: acc
    in
    let rec loop token_start i =
      if i = len
      then cons_str token_start i []
      else
        match s.[i] with
        | '}' -> cons_str token_start i (Close Braces :: loop (i + 1) (i + 1))
        | ')' -> cons_str token_start i (Close Parens :: loop (i + 1) (i + 1))
        | '$' when i + 1 < len -> begin
            match s.[i + 1] with
            | '{' -> cons_str token_start i (Open Braces :: loop (i + 2) (i + 2))
            | '(' -> cons_str token_start i (Open Parens :: loop (i + 2) (i + 2))
            | _   -> loop token_start (i + 1)
          end
        | _ -> loop token_start (i + 1)
    in
    loop 0 0

  let to_string = function
    | String s     -> s
    | Open  Braces -> "${"
    | Open  Parens -> "$("
    | Close Braces -> "}"
    | Close Parens -> ")"
end

let rec of_tokens : Token.t list -> t = function
  | [] -> []
  | Open a :: String s :: Close b :: rest when [%compare.equal: var_syntax] a b ->
    Var (a, s) :: of_tokens rest
  | token :: rest ->
    let s = Token.to_string token in
    match of_tokens rest with
    | Text s' :: l -> Text (s ^ s') :: l
    | l -> Text s :: l

let of_string s = of_tokens (Token.tokenise s)

let () =
  [%test_result: t] (of_string "$(cat ${x})")
    ~expect:[Text "$(cat "; Var (Braces, "x"); Text ")"];
  [%test_result: t] (of_string "$") ~expect:[Text "$"];
;;

let t_of_sexp sexp = of_string (string_of_sexp sexp)

let fold t ~init ~f =
  List.fold_left t ~init ~f:(fun acc item ->
    match item with
    | Text _ -> acc
    | Var (_, v) -> f acc v)

let expand t ~f =
  List.map t ~f:(function
    | Text s -> s
    | Var (syntax, v) ->
      match f v with
      | Some x -> x
      | None ->
        match syntax with
        | Parens -> sprintf "$(%s)" v
        | Braces -> sprintf "${%s}" v)
  |> String.concat ~sep:""

module type Container = sig
  type 'a t [@@deriving of_sexp]

  val map : 'a t -> f:('a -> 'b) -> 'b t
  val fold : 'a t -> init:'b -> f:('b -> 'a -> 'b) -> 'b
end

module Lift(M : Container) = struct
  type nonrec t = t M.t [@@deriving of_sexp]

  let fold t ~init ~f =
    M.fold t ~init ~f:(fun acc x -> fold x ~init:acc ~f)

  let expand t ~f = M.map t ~f:(expand ~f)
end

