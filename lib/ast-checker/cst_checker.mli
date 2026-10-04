(** A checker using stylo's own parser as the reference: the output must parse
    to the same CST as the input.

    Satisfies [Stylo.Checker]. Used as a guard it accepts everything stylo
    accepts. *)

open Ocaml_syntax

type options = Parse.Options.t
type ast = Cst.t

val parse
  :  options
  -> Source.t
  -> (ast, Errors.parser * Lexing.position * Lexing.position * exn) result

(** Check that the output reparses to the same CST, modulo locations. *)
val check_same_ast : options -> ast -> Source.t -> (unit, [> Errors.t ]) result
