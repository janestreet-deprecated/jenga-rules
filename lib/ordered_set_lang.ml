
open Core.Std

type t = Sexp.t [@@deriving of_sexp]

let eval t ~special_values =
  let rec of_sexp = function
    | Sexp.Atom "\\" -> failwith "unexpected \\"
    | Sexp.Atom s ->
      begin match String.chop_prefix s ~prefix:":" with
      | None -> [s]
      | Some name ->
        match List.Assoc.find special_values name with
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
      let acc = List.filter acc ~f:(fun acc_elt -> not (List.mem elts acc_elt)) in
      of_sexps_negative acc sexps
    | [] -> List.rev acc
  in
  of_sexp t

let eval_with_standard t_opt ~standard =
  match t_opt with
  | None -> standard
  | Some t -> eval t ~special_values:[("standard", standard)]

let standard = sexp_of_string ":standard"
