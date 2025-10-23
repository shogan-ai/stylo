module Requirement : sig
  type t

  val infinity : t

  val of_int : int -> t
  val to_int : t -> int

  val ( + ) : t -> t -> t
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
  | Vanishing_space
  (** Prints as a space in flat mode when the parent is not flat, and vanished
      otherwise. *)

type t = private
  | Empty
  | Token of string
  | Comment of string
  | Whitespace of whitespace
  | Cat of Requirement.t * t * t
  | Nest of Requirement.t * int * t
  | Relative_nest of Requirement.t * int * t
  | Group of Requirement.t * t

val requirement : t -> Requirement.t

val empty : t
val string : string -> t

val break : int -> t
val soft_break : int -> t
val hardline : t
val softline : t
val softest_line : t
val softest_break : t
(** [Break (1, Softest)], used between docstrings. *)

val vanishing_space : t

val comment : string -> t
val docstring : string -> t

val (^^) : t -> t -> t
val nest : int -> t -> t
val relative_nest : int -> t -> t
val group : t -> t
