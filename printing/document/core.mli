module Requirement : sig
  type t

  val infinity : t

  val of_int : int -> t
  val to_int : t -> int

  val ( + ) : t -> t -> t

  val to_int_including_indent : current_indent:int -> t -> int
end

type flatness = private bool ref

val flatness_tracker : unit -> flatness

module Condition : sig
  type t

  val always : t

  val (&&) : t -> t -> t

  val flat : flatness -> t

  val check : t option -> bool (* false when None *)
end

type softness =
  | Hard (** Always introduce a line break *)
  | Soft (** Vanishes after blank lines, adds a break otherwise *)
  | Softest (** Vanishes after a line break *)

type whitespace =
  | Line_break of softness (** Cf {!softness} *)
  | Break of int * softness
  (** [Break (n, softness)] prints as [n] spaces in flat mode, and behaves
      as a [Line_break softness] otherwise. *)
  | Non_breakable (** a simple space. *)

type 'a can_vanish = {
  vanishing_cond: Condition.t option;
  value: 'a;
}
(** Some leaves of the document can "vanish" under certain conditions.
    For instances parentheses around a tuple might be present if the tuple is
    spread over several lines, but absent if it fits on one line.*)

type t = private
  | Empty
  | Token of pseudo_token can_vanish
  (** N.B. if the [vanishing_cond] is not [None], then [Token] must correspond
      to an "optional" token. *)
  | Comment of pseudo_token
  | Comments_flushing_hint of {
      cmts_were_flushed: bool ref;
      floating_cmts_allowed: bool;
      pull_cmts_attached_before_hint: bool;
      ws_before: t;
      ws_after: t;
    }
  | Whitespace of whitespace can_vanish
  | Cat of Requirement.t * t * t
  | Nest of Requirement.t * int * Condition.t option * t
  | Group of Requirement.t * int * flatness option * t

and pseudo_token = private
  | Trivial of Requirement.t * string
  | Verbatim of Requirement.t * string * int
  | Complex of Requirement.t * t

val requirement : t -> Requirement.t

(** {1 Atomic documents} *)

val empty : t

val is_empty : t -> bool

(** {2 Token documents} *)

val string : string -> t
(** A single token.
    Must not contain linebreaks. *)

val fancy_string : string -> t
(** For tokens (e.g. string literals) which might contain linebreaks. *)

val formatted_string : t -> t
(** For complex subtrees which map to a single input token.
    For instance string literals that have been specially formatted (e.g.
    wrapped, etc). *)

(** {2 Whitespace documents}

    Refer to {!softness} and {!whitespace} for more information about the
    various kind of whitespaces. *)

val nbsp : t
val break : int -> t
val soft_break : int -> t
val hardline : t
val softline : t
val softest_line : t
val softest_break : t (* used between docstrings. *)

(** {2 Optional documents} *)

val vanishing_whitespace : Condition.t -> t -> t
(** Expects a non vanishing whitespace document as input, and return the same
    kind of document which additionally vanishes when the condition is met. *)

val opt_token : ?ws_before:t -> ?ws_after:t -> Condition.t -> string -> t
(** Produces a [Token] document which vanishes when the condition is met.

    The optional parameters are here for convenience: when given they are added
    before/after the document and vanish in the same situation. *)

(** {2 Comment documents} *)

val flush_comments
  :  pull_preceeding_comments:bool
  -> floating_allowed:bool
  -> ws_before:t
  -> ws_after:t
  -> Condition.t * t
(** An explicit hint for the comments insertion algorithm to flush comments at
    this point: it allows for floating comments to be displayed as such if
    desirable, whereas they would usually be attached to what follows them.

    Likewise it can also "pull" comments that would otherwise be attached the
    preceeding token.

    Renders as [empty] if there are no comments to insert.

    The [Condition.t] will evaluate to true if some comments were inserted,
    false otherwise.
    N.B. the condition will necessarily get it's final value before the printing
    engine runs, so it can be used to make elements preceeding the comments
    vanish, not just elements who follow them. *)

val comment : string -> t
(** [comment s] wraps [s] with "(*" "*)" and produces a [Comment].*)

val docstring : string -> t
(** As [comment] but wraps with "(**" "*)" instead. *)

(** {1 Structured documents} *)

val (^^) : t -> t -> t
(** [a ^^ b] is the concatenation of [a] and [b] *)

val nest : ?vanish:Condition.t -> int -> t -> t
(** Instruct the engine to change the indentation level for the given document.

    The [vanish] parameter is used to make the nest node (but not its content)
    vanish.
    An situation where this is useful is if the node is between optional
    parentheses. One wants to increase the indentation level when they are
    present, but not when they vanish. This can be achieved by passing the same
    [Condition.t] to all three nodes. *)

val group : ?margin:int -> ?flatness:flatness -> t -> t
(** If the given doc fits on the current, then it is printed on one line (i.e.
    the printing engine enters its "flat" mode) and all the groups inside are
    ignored (as we will already be in flat mode), otherwise printing continues
    in normal mode.

    [margin] is used to extend document's requirement when deciding
    whether to enter flat mode or not, but it doesn't change the requirement of
    the group itself (and so doesn't propagate nor affect the parent's
    flatness).

    The value of [flatness] will be decided by the printer when traversing the
    group and can then be part of a [Condition.t].
    N.B. if the condition is checked before the group is traversed, it will be
    assumed to be non-flat. *)

(**/**)

val pp_pseudo : Format.formatter -> pseudo_token -> unit
