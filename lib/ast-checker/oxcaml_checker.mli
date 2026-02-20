type _ input_kind =
  | Impl : Oxcaml_frontend.Parsetree.structure  input_kind
  | Intf : Oxcaml_frontend.Parsetree.signature input_kind

type 'a input = {
  fname : string;
  start_line : int;
  source : string;
  kind : 'a input_kind;
}

(** Check that two strings parse to the same AST (modulo locations) *)

val parse : 'ast input -> ('ast, [> `Input_parse_error of Errors.parser * exn ]) result

val check_same_ast : 'cst -> 'cst input -> (unit, [> Errors.t ]) result
