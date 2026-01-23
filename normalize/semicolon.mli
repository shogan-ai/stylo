open Ocaml_syntax

val normalize_struct_semisemi : Parsetree.structure -> Parsetree.structure
val exp_no_trailing : Parsetree.expression -> Parsetree.expression
val pat_no_trailing : Parsetree.pattern -> Parsetree.pattern

val constructor_arguments
  :  Parsetree.constructor_arguments
  -> Parsetree.constructor_arguments

val type_kind_no_trailing : Parsetree.type_kind -> Parsetree.type_kind
