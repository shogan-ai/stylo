module type S = sig
  type t

  val create : int -> t
  val newline : t -> unit
  val add_spaces : t -> int -> unit
  val add_string : t -> string -> unit
  val contents : t -> string

  val open_alignment_context : t -> unit
  val close_alignment_context : t -> unit
  val new_alignment_hint : t -> unit
end

