
(** Where the printer writes its output. *)
module type S = sig
  type t

  val create : int -> t
  val newline : t -> unit

  (** Spaces are delayed: the ones which are not followed by some text on their
      line are dropped. *)
  val add_spaces : t -> int -> unit

  (** The string is never empty, and doesn't contain newlines. *)
  val add_string : t -> string -> unit

  val contents : t -> string

  (** {2 Vertical alignment}

      Cf. [Core.vertically_aligned]. Each opening call is matched by the
      corresponding closing one, and they are well nested. Cells can also appear
      outside of any context or row, in which case they are not aligned.

      A buffer is free to ignore these. *)

  (** [enabled] is [false] when the rows are printed in flat mode. *)
  val open_alignment_context : t -> enabled:bool -> unit

  val close_alignment_context : t -> unit
  val start_alignment_row : t -> unit
  val end_alignment_row : t -> unit

  (** Delimit a {!Core.cell} with the given alignment. *)
  val open_cell : t -> align:Core.alignment -> unit

  val close_cell : t -> unit
end
