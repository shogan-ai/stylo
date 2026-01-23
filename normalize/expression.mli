open Ocaml_syntax

val map_desc
  :  Context.parent Ast_mapper.mapper
  -> Context.parent
  -> Parsetree.expression_desc
  -> Parsetree.expression_desc

val map
  :  Context.parent Ast_mapper.mapper
  -> Context.parent
  -> Parsetree.expression
  -> Parsetree.expression

val insert_pipe_if_missing
  :  after:Parser_tokens.token
  -> Tokens.seq
  -> Tokens.seq
