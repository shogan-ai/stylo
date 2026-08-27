(** Pretty-printer of OxCaml's CST *)

open Ocaml_syntax.Parsetree

module Doc = Docstring

module Structure : sig
  val pp_implementation : structure -> Document.t
end

module Signature : sig
  val pp_interface : signature -> Document.t
end
