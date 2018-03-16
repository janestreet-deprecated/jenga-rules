open! Core
open! Import

val filename : string

module Let_syntax : sig
  type t =
    { exceptions : String.Set.t
    }
  [@@deriving sexp]
end

module Ocamlformat : sig
  type t =
    { exceptions : String.Set.t
    }
  [@@deriving sexp]
end

type t =
  { enabled     : bool
  ; exceptions  : String.Set.t
  ; let_syntax  : Let_syntax.t option
  ; ocamlformat : Ocamlformat.t option
  }
[@@deriving sexp]

val default : t
