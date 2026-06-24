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

val structure : Parsetree.structure -> (Tokens.seq, [> Error.t ]) result
val signature : Parsetree.signature -> (Tokens.seq, [> Error.t ]) result
