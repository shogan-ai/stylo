module Requirement : sig
  type t

  val infinity : t

  val of_int : int -> t
  val to_int : t -> int

  val ( + ) : t -> t -> t
end

type whitespace =
  | Break of int
  (** [Break n] prints as n spaces in flat mode, as a line break otherwise *)
  | Hard_line
  (** Always a line break *)
  | Blank_line (* FIXME: find a better name, [Max_one_blank_line]? *)
  (** Vanishes in flat mode and after a blank line, same as hardline in other
      cases. *)

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
val hardline : t
val blank_line : t

val comment : string -> t
val docstring : string -> t

val (^^) : t -> t -> t
val nest : int -> t -> t
val relative_nest : int -> t -> t
val group : t -> t
