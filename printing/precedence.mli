open Ocaml_syntax

val is_star : string -> pos:int -> bool
val of_infix_op : Parsetree.expression -> int
