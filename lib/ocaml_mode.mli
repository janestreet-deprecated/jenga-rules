
(** An experiment to implement bytecode rules using the terminology of
    native compilation and the code of native rules. *)

open! Core.Std

module type S = sig
  val cmx        : string
  val cmx_and_o  : string list
  val cmxa       : string
  val cmxa_and_a : string list
  val exe        : string
  val compilation_depends_on_cmx : bool
  val which      : [ `Byte | `Native ]
  val which_str  : string
end
type t = (module S)
val native : t
val byte : t
val all : t list
