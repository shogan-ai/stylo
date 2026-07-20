open Ocaml_syntax
open Parsetree

module Modes : sig
  val remove_from_tokens : modes -> Tokens.seq -> Tokens.seq
  val remove_from_name : modes module_name -> modes module_name
end

module Modalities : sig
  val remove_from_tokens : modalities -> Tokens.seq -> Tokens.seq
  val remove_from_name : modalities module_name -> modalities module_name
end
