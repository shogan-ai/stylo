(** Check that two strings parse to the same AST (modulo locations) *)

val ignore_docstrings : bool ref

val check_same_ast
  : fname:string -> start_line:int -> impl:bool -> string -> string -> unit

exception Ast_changed

val report : unit -> unit
