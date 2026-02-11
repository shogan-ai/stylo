(** Check that two strings parse to the same AST (modulo locations) *)

val ignore_docstrings : bool ref

type error = [
  | `Input_parse_error of exn
  | `Output_parse_error of exn
  | `Ast_changed of string
]

val check_same_ast
  : fname:string -> start_line:int -> impl:bool -> string -> string
  -> (unit, [> error ]) result

val pp_error : error -> unit
