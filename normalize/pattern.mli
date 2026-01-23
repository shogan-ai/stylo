open Ocaml_syntax

val map_desc
  :  Context.parent Ast_mapper.mapper
  -> Context.parent
  -> Parsetree.pattern_desc
  -> Parsetree.pattern_desc

val map
  :  Context.parent Ast_mapper.mapper
  -> Context.parent
  -> Parsetree.pattern
  -> Parsetree.pattern
