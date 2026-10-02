open Ocaml_syntax

module Check : sig
  open Ast_checker

  type checker_input =
    | Ast of Source.t * Oxcaml_checker.ast
    | Cst of Source.t * Cst.t

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

  val run : ?normalize:bool -> Source.t -> (string, error) result
  val pp_error : Format.formatter -> string -> error -> unit
end

val style_file
  :  Source.kind
  -> fname:string
  -> ?lnum:int
  -> ?normalize:bool
  -> string
  -> (string, [> Pipeline.error ]) result

val split_fuzzer_line : string -> bool * string

val style_fuzzer_line
  :  lnum:int
  -> fname:string
  -> string
  -> (string, Pipeline.error) result
