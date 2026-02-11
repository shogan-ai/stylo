(** Automatic comments insertion *)

module Error : sig
  type t

  val pp : Format.formatter -> t -> unit
end

type error = [ `Comment_insertion_error of Error.t ]

val from_tokens : Ocaml_syntax.Tokens.seq -> Document.t ->
  (Document.t, [> error ]) result
(** [from_tokens seq doc] will insert any comments present in [seq] in the
    appropriate place in [doc], assuming [doc] has been produced from CST which
    corresponds to [seq]. *)
