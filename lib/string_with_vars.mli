(** String with variables of the form ${...} or $(...).

    Variables cannot contain "${", "$(", ")" or "}". For instance in "$(cat ${x})", only
    "${x}" will be considered a variable, the rest is text. *)

open! Core

type t [@@deriving of_sexp]

val of_string : string -> t

val fold : t -> init:'a -> f:('a -> string -> 'a) -> 'a

val expand : t -> f:(string -> string option) -> string

module type Container = sig
  type 'a t [@@deriving of_sexp]

  val map : 'a t -> f:('a -> 'b) -> 'b t
  val fold : 'a t -> init:'b -> f:('b -> 'a -> 'b) -> 'b
end

module Lift(M : Container) : sig
  type nonrec t = t M.t [@@deriving of_sexp]

  val fold : t -> init:'a -> f:('a -> string -> 'a) -> 'a

  val expand : t -> f:(string -> string option) -> string M.t
end
