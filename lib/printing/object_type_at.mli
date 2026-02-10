open Ocaml_syntax
open Parsetree

module End : sig
  val of_core_type : core_type -> bool
  val of_structure : structure -> bool
  val of_signature : signature -> bool
end

module Start : sig
  val of_core_type : core_type -> bool
end
