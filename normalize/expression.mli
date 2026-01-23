open Ocaml_syntax

val map_desc
  :  recur:
       (Context.parent -> Parsetree.expression_desc -> Parsetree.expression_desc)
  -> Context.parent
  -> Parsetree.expression_desc
  -> Parsetree.expression_desc

val map
  :  recur:(Context.parent -> Parsetree.expression -> Parsetree.expression)
  -> Context.parent
  -> Parsetree.expression
  -> Parsetree.expression

val insert_pipe_if_missing
  :  after:Parser_tokens.token
  -> Tokens.seq
  -> Tokens.seq
