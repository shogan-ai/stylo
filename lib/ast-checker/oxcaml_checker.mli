open Ocaml_syntax

(** An AST as produced by the upstream OxCaml parser. *)
type ast

val parse
  :  Source.t
  -> ( ast
     , [> `Input_parse_error of
          Errors.parser * Lexing.position * Lexing.position * exn
       ] )
       result

(** Check that the output reparses to the same AST, modulo locations. *)
val check_same_ast : ast -> Source.t -> (unit, [> Errors.t ]) result
