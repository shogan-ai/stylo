open Ocaml_syntax

val list_map_last : f:('a -> 'a) -> 'a list -> 'a list
val split : on:Parser_tokens.token -> Tokens.seq -> Tokens.seq * Tokens.seq
val split_on_child : Tokens.seq -> Tokens.seq * Tokens.seq

val search_and_replace
  :  (Parser_tokens.token * Parser_tokens.token) list
  -> Tokens.seq
  -> Tokens.seq

val without : token:Parser_tokens.token -> Tokens.seq -> Tokens.seq
