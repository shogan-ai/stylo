(** Check that two strings parse to the same AST (modulo locations) *)

module Location = Location
module Syntaxerr = Syntaxerr

val ignore_docstrings : bool ref

val check_same_ast : string -> int -> impl:bool -> string -> string -> bool
