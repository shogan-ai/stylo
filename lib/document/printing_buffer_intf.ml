
(** Where the printer writes its output. *)
module type S = sig
  type t

  val create : int -> t
  val newline : t -> unit
  val add_spaces : t -> int -> unit
  val add_string : t -> string -> unit
  val contents : t -> string

  (** {2 Vertical alignment}

      Cf. [Core.vertically_aligned]. A buffer is free to ignore these. *)

  (** [enabled] is [false] when the rows are printed in flat mode. *)
  val open_alignment_context : t -> enabled:bool -> unit

  val close_alignment_context : t -> unit
  val start_alignment_row : t -> unit
  val end_alignment_row : t -> unit
  val alignment_point : t -> unit
end
