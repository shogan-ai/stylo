
(** Output buffer of the printer, with whitespace buffering. *)
type t

val create : int -> t
val newline : t -> unit
val add_spaces : t -> int -> unit
val add_string : t -> string -> unit
val contents : t -> string
