module Requirement : sig
  type t

  val infinity : t

  val of_int : int -> t
  val to_int : t -> int

  val ( + ) : t -> t -> t
end

type whitespace =
  | Line_break of { soft: bool }
  (** Vanishes after a blank line when [soft = true].
      Introduces a line break in the output otherwise. *)
  | Break of { spaces: int; soft: bool }
  (** [Break { spaces = n; soft }] prints as n spaces in flat mode, and behaves
      as a [Line_break { soft }] otherwise. *)
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

val vanishing_space : t

val comment : string -> t
val docstring : string -> t

val (^^) : t -> t -> t
val nest : int -> t -> t
val relative_nest : int -> t -> t
val group : t -> t
