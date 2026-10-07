(** A style aiming to match ocamlformat's "janestreet" profile, together with
    the checker it is used with ([Oxcaml_checker]). *)

open Ocaml_syntax

type style_options = { normalize : Normalize.Options.t }

type options =
  { width : int
  ; parse : Parse.Options.t
  ; checks : Stylo.Check.Options.t
  ; debug : bool
  ; style : style_options
  ; checker : Oxcaml_checker.options
  }

val run : options -> Source.t -> (string, Stylo.Pipeline.error) result

(**/**)

(* Used for fuzzing the printer. *)

module Without_normalization : sig
  type options =
    { width : int
    ; parse : Parse.Options.t
    ; checks : Stylo.Check.Options.t
    ; debug : bool
    ; style : style_options
    ; checker : Cst_checker.options
    }

  val run : options -> Source.t -> (string, Stylo.Pipeline.error) result
end

(**/**)
