(** String literales printing. *)

val pp : string -> Document.t
(** wraps when possible to avoid going over the line max width *)
