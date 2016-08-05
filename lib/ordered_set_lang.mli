
(* [Ordered_set_lang.t] is a sexp-based representation for an ordered list of strings,
   with some set like operations. *)

type t [@@deriving of_sexp]
val eval_with_standard : t option -> standard:string list -> string list
val standard : t
