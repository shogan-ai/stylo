open Ocaml_syntax

(** Check that the output reparses to the same CST, modulo locations. *)

val check_same_ast : Cst.t -> Source.t -> (unit, [> Errors.t ]) result
