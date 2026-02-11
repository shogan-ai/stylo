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
  val retokenisation : Tokens.seq lazy_t -> unit
  (** @raise Ast_checker.Tokenisation_check.Ordering.Error *)

  val normalization_kept_comments : Tokens.seq lazy_t -> Tokens.seq -> unit
  (** @raise Ast_checker.Tokenisation_check.Comments_comparison.Error *)

  val same_ast : _ input -> string -> unit
  (** @raise Ast_checker.Oxcaml_checker.Ast_changed *)
end

module Pipeline : sig
  val parse : 'cst input -> 'cst

  val normalize : 'cst input_kind -> 'cst -> 'cst

  val tokens_of_tree : 'cst input_kind -> 'cst -> Tokens.seq

  val build_doc : 'cst input_kind -> 'cst -> Document.t

  val print_doc : Document.t -> string

  val run : _ input -> string
end

val style_file : _ input_kind -> string -> string

val style_fuzzer_line : lnum:int -> fname:string -> string -> string
