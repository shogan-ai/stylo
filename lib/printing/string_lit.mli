(** String literales printing. *)

(** wraps when possible to avoid going over the line max width *)
val pp : string -> Document.t
