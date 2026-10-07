(** {!https://ocaml.org/manual/5.4/expr.html#ss:precedence-and-associativity} *)

open Ocaml_syntax.Parsetree

val of_infix_op : expression -> int
