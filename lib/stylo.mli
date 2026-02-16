open Ocaml_syntax

module Cst := Ocaml_syntax.Parsetree
module Ast := Oxcaml_frontend.Parsetree

type (_, _) input_kind =
  | Impl : (Cst.structure, Ast.structure) input_kind
  | Intf : (Cst.signature, Ast.signature) input_kind

type ('a, 'b) input = {
  fname : string;
  start_line : int;
  source : string;
  kind : ('a, 'b) input_kind;
}

module Check : sig
  open Ast_checker

  type (_, _) checker_input =
    | Ast : ('cst, 'ast) input * 'ast -> ('cst, 'ast) checker_input
    | Cst : ('cst, 'ast) input * 'cst -> ('cst, 'ast) checker_input

  val same_ast
    : _ checker_input -> string -> (unit, [> Errors.t ]) result

  open Tokenisation_check

  val retokenisation : Tokens.seq lazy_t -> (unit, [> Ordering.error]) result

  val normalization_kept_comments
    :  Tokens.seq lazy_t
    -> Tokens.seq lazy_t
    -> (unit, [> Comments_comparison.error]) result

  type error = [
    | Ordering.error
    | Comments_comparison.error
    | Errors.t
  ]
end

module Pipeline : sig
  val parse
    :  ('cst, _) input
    -> ('cst, [> `Input_parse_error of Ast_checker.Errors.parser * exn]) result

  val normalize : ('cst, _) input_kind -> 'cst -> 'cst

  val tokens_of_tree : ('cst, _) input_kind -> 'cst -> Tokens.seq

  val build_doc : ('cst, _) input_kind -> 'cst -> Document.t

  val print_doc : Document.t -> string

  type error = [
    | Check.error
    | Comments.Insert.error
  ]

  val run : ?normalize:bool -> _ input -> (string, error) result

  val pp_error : string -> error -> unit
end

val style_file : _ input_kind -> string -> (string, Pipeline.error) result

val style_fuzzer_line
  : lnum:int -> fname:string -> string -> (string, Pipeline.error) result
