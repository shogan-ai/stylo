(** A checker for stylo's pipeline, built on the upstream OxCaml frontend.

    Satisfies [Stylo.Checker]. *)

open Ocaml_syntax
open Ast_checker

type options =
  { syntax_quotations : bool
       (** whether the upstream lexer enables quotations by default *)
  ; erase_jane_syntax : bool
       (** whether Jane Street syntax should be erased from the input's AST
           before comparing it with the output's *)
  ; debug : bool
       (** dump the ASTs next to the input file when they differ, and the output
           when it doesn't parse *)
  }

(** An AST as produced by the upstream OxCaml parser. *)
type ast

val parse
  :  options
  -> Source.t
  -> (ast, Errors.parser * Lexing.position * Lexing.position * exn) result

(** Check that the output reparses to the same AST, modulo locations. *)
val check_same_ast : options -> ast -> Source.t -> (unit, [> Errors.t ]) result
