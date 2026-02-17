(** {1 Flattening to a sequence of tokens } *)

module Error : sig
  type context = private
    { node_kind: string
    ; pos: Lexing.position }

  type t = [
    | `Missing_children of context * Lexing.position
    | `Extra_children of context * Tokens.seq list
  ]

  val pp : Format.formatter -> t -> unit
end

val structure : Parsetree.structure -> (Tokens.seq, [> Error.t ]) result
val signature : Parsetree.signature -> (Tokens.seq, [> Error.t ]) result
