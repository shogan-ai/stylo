type error

val pp_error : Format.formatter -> error -> unit

exception Error of error

val from_tokens : Ocaml_syntax.Tokens.seq -> Wrapprint.t -> Wrapprint.t
