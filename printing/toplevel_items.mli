open Ocaml_syntax

module Struct : sig
  val pp_grouped_keeping_semi
    :  (Parsetree.structure_item -> Document.t)
    -> Parsetree.structure
    -> Document.t
end

module Sig : sig
  val pp_grouped_keeping_semi
    :  (Parsetree.signature_item -> Document.t)
    -> Parsetree.signature_item list * Tokens.seq
    -> Document.t
end

val pp_keeping_semi : ('a -> Document.t) -> 'a list * Tokens.seq -> Document.t
