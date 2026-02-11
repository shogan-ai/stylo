open Ocaml_syntax

type _ input_kind =
  | Impl : Parsetree.structure input_kind
  | Intf : Parsetree.signature input_kind

type 'a input = {
  fname : string;
  start_line : int;
  source : string;
  kind : 'a input_kind;
}

module Check : sig
  open Ast_checker

  val same_ast : _ input -> string -> (unit, [> Oxcaml_checker.error ]) result

  open Tokenisation_check

  val retokenisation : Tokens.seq lazy_t -> (unit, [> Ordering.error]) result

  val normalization_kept_comments : Tokens.seq lazy_t -> Tokens.seq ->
    (unit, [> Comments_comparison.error]) result

  type error = [
    | Ordering.error
    | Comments_comparison.error
    | Oxcaml_checker.error
  ]
end

module Pipeline : sig
  val parse : 'cst input -> ('cst, [> `Cst_parser_error of exn]) result

  val normalize : 'cst input_kind -> 'cst -> 'cst

  val tokens_of_tree : 'cst input_kind -> 'cst -> Tokens.seq

  val build_doc : 'cst input_kind -> 'cst -> Document.t

  val print_doc : Document.t -> string

  type error = [
    | `Cst_parser_error of exn
    | Check.error
    | Comments.Insert.error
  ]

  val run : _ input -> (string, error) result

  val pp_error : string -> error -> unit
end

val style_file : _ input_kind -> string -> (string, Pipeline.error) result

val style_fuzzer_line
  : lnum:int -> fname:string -> string -> (string, Pipeline.error) result
