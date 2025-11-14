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
  val not : t -> t

  val flat : flatness -> t

  val check : t -> bool
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
  | Vanishing_space of Condition.t
  (** Prints as a space when the condition is not met, vanishes otherwise. *)

type t = private
  | Empty
  | Token of string
  | Optional of {
      vanishing_cond: Condition.t;
      before: whitespace option;
      token: string;
      after: whitespace option;
    }
  | Comment of string
  | Whitespace of whitespace
  | Cat of Requirement.t * t * t
  | Nest of Requirement.t * int * Condition.t * t
  | Group of Requirement.t * flatness option * t

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

val comment : string -> t
val docstring : string -> t

val (^^) : t -> t -> t
val nest : ?vanish:Condition.t -> int -> t -> t
val group : ?flatness:flatness -> t -> t

val flatness_tracker : unit -> flatness
