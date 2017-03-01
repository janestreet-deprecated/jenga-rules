open Core.Std
open! Import

type t = Sexp.t [@@deriving of_sexp]

let eval t ~special_values =
  let rec of_sexp = function
    | Sexp.Atom "\\" -> failwith "unexpected \\"
    | Sexp.Atom s ->
      begin match String.chop_prefix s ~prefix:":" with
      | None -> [s]
      | Some name ->
        match List.Assoc.find special_values name ~equal:String.equal with
        | Some l -> l
        | None -> failwithf "undefined symbol %s" s ()
      end
    | Sexp.List sexps -> of_sexps [] sexps
  and of_sexps acc = function
    | Sexp.Atom "\\" :: sexps -> of_sexps_negative acc sexps
    | elt :: sexps ->
      let elts = of_sexp elt in
      of_sexps (List.rev_append elts acc) sexps
    | [] -> List.rev acc
  and of_sexps_negative acc = function
    | Sexp.Atom "\\" :: sexps -> of_sexps_negative acc sexps
    | elt :: sexps ->
      let elts = of_sexp elt in
      let acc = List.filter acc ~f:(fun acc_elt -> not (List.mem_string elts acc_elt)) in
      of_sexps_negative acc sexps
    | [] -> List.rev acc
  in
  of_sexp t

let eval_with_standard t ~standard =
  eval t ~special_values:[("standard", standard)]

let eval_opt_with_standard t_opt ~standard =
  match t_opt with
  | None -> standard
  | Some t -> eval_with_standard t ~standard

let standard = sexp_of_string ":standard"

module Unexpanded = struct
  type t = Sexp.t [@@deriving of_sexp]

  let files t =
    let rec loop acc : t -> _ = function
      | Atom _ -> acc
      | List [Atom ":include"; Atom fn] -> Set.add acc fn
      | List l -> List.fold_left l ~init:acc ~f:loop
    in
    loop String.Set.empty t

  let rec expand (t : t) ~files_contents =
    match t with
    | Atom _ -> t
    | List [Atom ":include"; Atom fn] -> Map.find_exn files_contents fn
    | List l -> List (List.map l ~f:(expand ~files_contents))
end
