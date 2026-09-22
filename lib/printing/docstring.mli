(** Docstrings pretty printing *)

open Ocaml_syntax

(**/**)

module Odoc : sig

  (** Filled in from stylo.ml when the pipeline is constructed. *)
  val process_ocaml_block : (string -> Document.t option) ref
end

(** Used externally only by the comment insertion engine. *)
val as_odoc_markup_if_no_warnings
  :  id:int
       (** docstrings have unique identifiers that are propagated all the way
           through the printer. *)
  -> kind:[ `Docstring | `Regular_comment ]
  -> start_pos:Lexing.position
  -> string
  -> Document.t

(**/**)

val pp : Parsetree.doc -> Document.t
val pp_floating : Parsetree.doc -> Document.t

val attach
  :  ?possibly_ambiguous:bool
  -> ?extra_nest:(Document.t -> Document.t)
  -> ?text:Parsetree.doc list (** docstring between items of the same block *)
  -> ?pre_doc:Parsetree.doc (** docstring before the item *)
  -> ?post_doc:Parsetree.doc (** docstring after the item *)
  -> Document.t
  -> Document.t
