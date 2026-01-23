(** Check that two strings parse to the same AST (modulo locations) *)

val ignore_docstrings : bool ref

module Check_same_ast : sig
  type t := string -> int -> string -> string -> bool

  val implementation : t
  val interface : t
  val use_file : t
end
