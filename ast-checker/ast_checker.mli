(** Check that two strings parse to the same AST (modulo locations) *)

exception Failed_to_parse_source of exn

val ignore_docstrings : bool ref

val check_same_ast : string -> int -> impl:bool -> string -> string -> bool
