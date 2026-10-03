open Ocaml_syntax

(** A reference parser against which stylo's output is checked.

    It plays two roles in {!Make.run}:
    - a guard: normalisation only happens on inputs it accepts;
    - a checker: when normalisation happened (and [--ast-check] was passed),
      [check_same_ast input_tree output] verifies that the output parses back to
      the same tree as the input.

    When the input is rejected and no AST check was requested, stylo falls back
    to comparing its own CSTs (cf. {!Cst_checker}). Errors are tagged with an
    [Ast_checker.Errors.Reference] parser. *)
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

  (** What the output is checked against, decided by {!Make.run}. *)
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

(** A formatting style: the parts of the pipeline which are a matter of taste.
    Parsing, token bookkeeping, comment insertion, the layout engine, and the
    checks are shared by all styles. *)
module type Style = sig
  type options

  (** Rewrites the CST into the style's canonical form.

      Contract: rewrites must update the token sequences attached to the CST
      nodes in lockstep, so that {!Pipeline.tokens_of_tree} on the result still
      accounts for every token and comment of the input. Tokens the style wants
      to make optional must be marked as such.

      Only called on inputs accepted by the checker, never on OCaml code blocks
      found in docstrings. *)
  val normalize : options -> Cst.t -> Cst.t

  (** Turns a (normalised) CST into a document.

      Contract:
      - the leaves of the document must correspond, one-to-one and in order, to
        the tokens {!Pipeline.tokens_of_tree} produces for the same tree;
        optional tokens must be printed with [Document.opt_token];
      - docstrings placed explicitly must be emitted with the id the lexer gave
        them, otherwise comment insertion will print them a second time.

      [format_code_block] formats an OCaml snippet (e.g. a [{[ ... ]}] block in
      a docstring) with this same style. It returns [None] if the snippet
      doesn't parse, or cannot be formatted. *)
  val build_doc
    :  options
    -> format_code_block:(string -> Document.t option)
    -> Cst.t
    -> Document.t

  (** Renders the comments placed by comment insertion (i.e. those [build_doc]
      did not place itself). [start_pos] is the comment's position in the
      source. *)
  val render_comment
    :  options
    -> format_code_block:(string -> Document.t option)
    -> start_pos:Lexing.position
    -> string
    -> Document.t
end

(** [S], minus the normalisation. *)
module Without_normalization (S : Style) : Style with type options = S.options

(** Uses stylo's own parser as the reference. *)
module Cst_checker : Checker with type ast = Cst.t

(** The style-independent stages of the pipeline. *)
module Pipeline : sig
  val parse
    :  Source.t
    -> ( Cst.t
       , [> `Input_parse_error of
            Ast_checker.Errors.parser * Lexing.position * Lexing.position * exn
         ] )
         result

  val tokens_of_tree : Cst.t -> (Tokens.seq, [> Tokens_of_tree.Error.t ]) result
  val print_doc : Document.t -> string

  type error =
    [ | Tokens_of_tree.Error.t
    | Check.error
    | Comments.Insert.error
    ]

  val pp_error : Format.formatter -> string -> error -> unit
end

module Make (S : Style) (_ : Checker) : sig
  val run : S.options -> Source.t -> (string, Pipeline.error) result
end
