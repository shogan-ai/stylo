module Requirement : sig
  type t

  val infinity : t

  val of_int : int -> t
  val to_int : t -> int

  val ( + ) : t -> t -> t

  val to_int_including_indent : current_indent:int -> t -> int
end

type flatness = private bool ref

module Condition : sig
  type t

  val always : t

  val (&&) : t -> t -> t

  val flat : flatness -> t

  val check : t option -> bool (* false when None *)
end

type softness =
  | Hard (** always introduce a line break *)
  | Soft (** Vanishes after blank lines, adds a break otherwise *)
  | Softest (** Vanishes after a line break *)

type whitespace =
  | Line_break of softness (** Cf {!softness} *)
  | Break of int * softness
  (** [Break (n, softness)] prints as [n] spaces in flat mode, and behaves
      as a [Line_break softness] otherwise. *)
  | Non_breakable (** a simple space. *)

type t = private
  | Empty
  | Token of pseudo_token
  | Optional of {
      vanishing_cond: Condition.t;
      token: pseudo_token;
    }
  | Comment of pseudo_token
  | Whitespace of Condition.t option * bool * whitespace
  | Cat of Requirement.t * t * t
  | Nest of Requirement.t * int * Condition.t option * t
  | Group of Requirement.t * int * flatness option * t

and pseudo_token = private
  | Trivial of Requirement.t * string
  | Complex of Requirement.t * t

val requirement : t -> Requirement.t

val empty : t
val string : string -> t

val opt_token
  : ?ws_before:whitespace -> ?ws_after:whitespace -> Condition.t -> string -> t

val break : int -> t
val soft_break : int -> t
val hardline : t
val softline : t
val softest_line : t
val softest_break : t
(** [Break (1, Softest)], used between docstrings. *)

val nbsp : t
val vanishing_space : Condition.t -> t

val triple_when_followed_by_comment : t -> t
(** @raises Invalid_argument unless called on a single whitespace *)

val comment : string -> t
val docstring : string -> t

val (^^) : t -> t -> t
val nest : ?vanish:Condition.t -> int -> t -> t
val group : ?margin:int -> ?flatness:flatness -> t -> t
(** [group ~margin t] is used to extend the [t]'s requirement when deciding
    whether to enter flat mode or not, but it doesn't change the requirement of
    the group itself (and so doesn't propagate nor affect the parent's
    flatness).

    The value of [flatness] will be decided by the printer when traversing the
    group and can then be part of a [Condition.t].
    N.B. if the condition is checked before the group is traversed, it will be
    assumed to be non-flat. *)

val flatness_tracker : unit -> flatness

val pp_pseudo : Format.formatter -> pseudo_token -> unit

val fancy_string : string -> t

val formatted_string : t -> t

val is_empty : t -> bool
