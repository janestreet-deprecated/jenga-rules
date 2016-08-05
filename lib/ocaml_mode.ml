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

module Make(Mode : sig
    val which : [ `Byte | `Native ]
    val which_str : string
    val cmx : string
    val o : string option
    val cmxa : string
    val a : string option
    val exe : string
    val compilation_depends_on_cmx : bool
end) = struct
  include Mode
  let cmx_and_o = cmx :: Option.to_list o
  let cmxa_and_a = cmxa :: Option.to_list a
end

type t = (module S)

let byte : t = (module Make(struct
    let which = `Byte
    let which_str = "byte"
    let cmx  = ".cmo"
    let o    = None
    let cmxa = ".cma"
    let a    = None
    let exe  = ".bc"
    let compilation_depends_on_cmx = false
  end))

let native : t = (module Make(struct
    let which = `Native
    let which_str = "native"
    let cmx  = ".cmx"
    let o    = Some ".o"
    let cmxa = ".cmxa"
    let a    = Some ".a"
    let exe  = ".exe"
    let compilation_depends_on_cmx = true
  end))

let all = [byte; native]
