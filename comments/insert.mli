(** Automatic comments insertion *)

val from_tokens : Ocaml_syntax.Tokens.seq -> Document.t -> Document.t
(** [from_tokens seq doc] will insert any comments present in [seq] in the
    appropriate place in [doc], assuming [doc] has been produced from CST which
    corresponds to [seq]. *)

module Error : sig
  type t

  val pp : Format.formatter -> t -> unit
end

exception Error of Error.t
