type _ input_kind =
  | Impl : Ocaml_syntax.Parsetree.structure  input_kind
  | Intf : Ocaml_syntax.Parsetree.signature input_kind

type 'a input = {
  fname : string;
  start_line : int;
  source : string;
  kind : 'a input_kind;
}


(** Check that two strings parse to the same AST (modulo locations) *)

val ignore_docstrings : bool ref

val check_same_ast : 'cst -> 'cst input -> (unit, [> Errors.t ]) result
