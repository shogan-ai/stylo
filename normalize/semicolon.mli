open Ocaml_syntax

val normalize_struct_semisemi : Parsetree.structure -> Parsetree.structure
val normalize_use_file_semisemi : Parsetree.use_file -> Parsetree.use_file
val exp_no_trailing : Parsetree.expression -> Parsetree.expression
val pat_no_trailing : Parsetree.pattern -> Parsetree.pattern

val constructor_arguments
  :  Parsetree.constructor_arguments
  -> Parsetree.constructor_arguments

val type_kind_no_trailing : Parsetree.type_kind -> Parsetree.type_kind
