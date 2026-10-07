open Ocaml_syntax

module type Checker = sig
  type options
  type ast

  val parse
    :  options
    -> Source.t
    -> (ast, Checks.Errors.parser * Lexing.position * Lexing.position * exn)
         result

  val check_same_ast
    :  options
    -> ast
    -> Source.t
    -> (unit, [> Checks.Errors.t ]) result
end

module Check : sig
  open Checks

  (** Which checks to run. *)
  module Options : sig
    type t =
      { same_ast : bool
           (** the output parses back to the same tree as the input *)
      ; retokenisation : bool
           (** the tokens retrieved from the CST are in the same order as in the
               source *)
      ; normalization_kept_comments : bool
           (** normalisation didn't drop any comment *)
      }

    val none : t
  end

  open Tokenisation_check

  val retokenisation
    :  Options.t
    -> (Tokens.seq, 'a) result lazy_t
    -> (unit, [> Ordering.error ] as 'a) result

  val normalization_kept_comments
    :  Options.t
    -> (Tokens.seq, 'a) result lazy_t
    -> (Tokens.seq, 'a) result lazy_t
    -> (unit, [> Comments_comparison.error ] as 'a) result

  type error =
    [ | Ordering.error
    | Comments_comparison.error
    | Errors.t
    ]
end

module type Style = sig
  type options

  val normalize : options -> Cst.t -> Cst.t

  val doc_of_cst
    :  options
    -> format_code_block:(string -> Document.t option)
    -> Cst.t
    -> Document.t

  (** Helper for automatic comment insertion.

      @param start_pos is the comment's position in the source. *)
  val render_comment
    :  options
    -> format_code_block:(string -> Document.t option)
    -> start_pos:Lexing.position
    -> string
    -> Document.t
end

module Without_normalization (S : Style) : Style with type options = S.options

module Cst_checker :
  Checker with type options = Parse.Options.t and type ast = Cst.t

module Pipeline : sig
  val parse
    :  Parse.Options.t
    -> Source.t
    -> ( Cst.t
       , [> `Input_parse_error of
            Checks.Errors.parser * Lexing.position * Lexing.position * exn
         ] )
         result

  val tokens_of_tree : Cst.t -> (Tokens.seq, [> Tokens_of_tree.Error.t ]) result
  val print_doc : width:int -> Document.t -> string

  type error =
    [ | Tokens_of_tree.Error.t
    | Check.error
    | Comments.Insert.error
    ]

  val pp_error : Format.formatter -> string -> error -> unit
end

module Make (S : Style) (C : Checker) : sig
  type options =
    { width : int
    ; parse : Parse.Options.t
    ; checks : Check.Options.t
    ; debug : bool
    ; style : S.options
    ; checker : C.options
    }

  val run : options -> Source.t -> (string, Pipeline.error) result
end
