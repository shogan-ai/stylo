open Ocaml_syntax

(** A reference parser against which stylo's output is checked.

    It plays two roles in {!Pipeline.run}:
    - a guard: normalisation only happens on inputs it accepts;
    - a checker: when normalisation happened (and [--ast-check] was passed),
      [check_same_ast input_tree output] verifies that the output parses back to
      the same tree as the input.

    When normalisation is disabled, or when the input is rejected and no AST
    check was requested, stylo falls back to comparing its own CSTs. Errors are
    tagged with an [Ast_checker.Errors.Reference] parser. *)
module type Checker = sig
  type ast

  val parse
    :  Source.t
    -> ( ast
       , [> `Input_parse_error of
            Ast_checker.Errors.parser * Lexing.position * Lexing.position * exn
         ] )
         result

  val check_same_ast
    :  ast
    -> Source.t
    -> (unit, [> Ast_checker.Errors.t ]) result
end

module Check : sig
  open Ast_checker

  (** What the output is checked against, decided by {!Pipeline.run}. *)
  type checker_input

  val same_ast : checker_input -> string -> (unit, [> Errors.t ]) result

  open Tokenisation_check

  val retokenisation
    :  (Tokens.seq, 'a) result lazy_t
    -> (unit, [> Ordering.error ] as 'a) result

  val normalization_kept_comments
    :  (Tokens.seq, 'a) result lazy_t
    -> (Tokens.seq, 'a) result lazy_t
    -> (unit, [> Comments_comparison.error ] as 'a) result

  type error =
    [ | Ordering.error
    | Comments_comparison.error
    | Errors.t
    ]
end

module Pipeline : sig
  val parse
    :  Source.t
    -> ( Cst.t
       , [> `Input_parse_error of
            Ast_checker.Errors.parser * Lexing.position * Lexing.position * exn
         ] )
         result

  val normalize : Cst.t -> Cst.t
  val tokens_of_tree : Cst.t -> (Tokens.seq, [> Tokens_of_tree.Error.t ]) result
  val build_doc : Cst.t -> Document.t
  val print_doc : Document.t -> string

  type error =
    [ | Tokens_of_tree.Error.t
    | Check.error
    | Comments.Insert.error
    ]

  val run
    :  ?normalize:bool
    -> checker:(module Checker)
    -> Source.t
    -> (string, error) result

  val pp_error : Format.formatter -> string -> error -> unit
end

val style_file
  :  checker:(module Checker)
  -> Source.kind
  -> fname:string
  -> ?lnum:int
  -> ?normalize:bool
  -> string
  -> (string, [> Pipeline.error ]) result

val split_fuzzer_line : string -> bool * string

val style_fuzzer_line
  :  checker:(module Checker)
  -> lnum:int
  -> fname:string
  -> string
  -> (string, Pipeline.error) result
