open! Core
open! Import

module Let_syntax = struct
  type t =
    { exceptions : String.Set.t [@default String.Set.empty] [@sexp_drop_if Set.is_empty]
    }
  [@@deriving sexp]
end

module Ocamlformat = struct
  type t =
    { exceptions : String.Set.t [@default String.Set.empty] [@sexp_drop_if Set.is_empty]
    }
  [@@deriving sexp]
end

type t =
  { enabled     : bool         [@default true]             [@sexp_drop_default]
  ; exceptions  : String.Set.t [@default String.Set.empty] [@sexp_drop_if Set.is_empty]
  ; let_syntax  : Let_syntax.t sexp_option
  ; ocamlformat : Ocamlformat.t sexp_option
  }
[@@deriving sexp]

let default =
  { enabled = true
  ; exceptions = String.Set.empty
  ; let_syntax = None
  ; ocamlformat = None
  }
;;

let t_of_sexp sexp =
  match (sexp : Sexp.t) with
  | List [] ->
    of_sexp_error
      "[enforce_style] is now enabled by default, so you should delete \
       [(enforce_style ())].  If you want to be explicit about opting into \
       [enforce_style], you can say [(enforce_style ((enabled true)))]."
      sexp;
  | _ -> t_of_sexp sexp
;;
