(** The result of parsing a {!Source.t}. *)

type t =
  | Structure of Parsetree.structure
  | Signature of Parsetree.signature
