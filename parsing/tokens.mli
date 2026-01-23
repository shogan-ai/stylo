(** Wrapper around [Parser_tokens].

    While {!token} is the actual type of tokens as produced by the lexer/consumed by the
    parser, {!seq} is a list of pseudo tokens (of {!elt}s) meant to be attached to the
    parsetree.

    Each sequence of elements attached to a given node consists of the elements "consumed"
    by the grammar's production which was reduced to produce that node. The terminals
    recognized by the production are represented by the underlying tokens, the
    non-terminals by a {!Child_node} and any comment present between those symbols is also
    part of the sequence (as a {!Comment} element). *)

type token := Parser_tokens.token

type attachment =
  | Before
  | After
  | Floating
    (** A comment is either attached to the token before/after it, or left floating. This
      attachement decision is made by the lexer (cf {!Lexer.Staged_comments}). *)

type comment =
  { text : string
  ; attachement : attachment
  ; explicitely_inserted : bool ref
       (** this is used by {!Insert_comments} to not duplicate comments that were already
      explicitely inserted (e.g. docstrings). *)
  }

type desc =
  | Token of token * bool
    (** the bool marks whether the token is optional or not. Fresh from the lexer the value
      will always be [false], but this can be changed by a "normalisation" function (which
      can also just synthesize such optional nodes).

      This optional status is matched by a twin flag on document leaves (cf
      [../print/document/core.mli]). *)
  | Comment of comment
  | Child_node

type elt =
  { desc : desc
  ; pos : Lexing.position
  }

type seq = elt list

val is_child : elt -> bool
val is_comment : elt -> bool
val is_token : ?which:token -> elt -> bool

(** {1 Attaching to the Parsetree} *)

val add : pos:Lexing.position -> desc -> unit

(** returns all the tokens whose position falls in the given range *)
val at : Lexing.position * Lexing.position -> seq

(** use on parser entry points to collect comments lying outside the parsed symbol's
    position. *)
val attach_leading_and_trailing : seq -> seq

val replace_first_child : subst:seq -> seq -> seq

(** clear up state before another run of the lexer *)
val reset : unit -> unit

module Raw : sig
  val to_string : token -> string
  val equals : token -> token -> bool
end

(**/**)

(* For debug purposes *)

val pp_elt : Format.formatter -> elt -> unit
val pp_seq : Format.formatter -> seq -> unit
