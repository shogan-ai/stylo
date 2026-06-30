(** {1 Flattening to a sequence of tokens } *)

type tokens_from_subtree = {
  origin: string;
  tokens: Tokens.seq;
}

module Error : sig
  type context = private
    { node_kind: string
    ; pos: Lexing.position }

  type t = [
    | `CST_tokens_mismatch of context * Tokens.seq * tokens_from_subtree list
  ]

  val pp : Format.formatter -> t -> unit
end

open Parsetree

val structure : structure -> (Tokens.seq, [> Error.t ]) result
val signature : signature -> (Tokens.seq, [> Error.t ]) result

(**/**)

(* Used during normalization when subtrees are erased but we want to keep the
   comments inside them: we retrieve the tokens of the deleted subtree, and keep
   only the comments (which we'll attach to the new node, or parent). *)

val modes : modes -> (Tokens.seq, [> Error.t ]) result
val jkind_annotation : jkind_annotation -> (Tokens.seq, [> Error.t ]) result
val signature_item : signature_item -> (Tokens.seq, [> Error.t ]) result
val structure_item : structure_item -> (Tokens.seq, [> Error.t ]) result
val with_constraint : with_constraint -> (Tokens.seq, [> Error.t ]) result
