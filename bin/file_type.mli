type _ t =
  | Implementation : Ocaml_syntax.Parsetree.structure t
  | Interface : Ocaml_syntax.Parsetree.signature t
  | Use_file : Ocaml_syntax.Parsetree.use_file t

type packed = P : _ t -> packed [@@unboxed]

val of_filename : string -> packed
val check_same_ast : _ t -> string -> int -> string -> string -> bool
val parse : 'a t -> Lexing.lexbuf -> 'a
val normalize : 'a t -> 'a -> 'a
val pp : 'a t -> 'a -> Document.t
val tokens_of_tree : 'a t -> 'a -> Ocaml_syntax.Tokens.seq
