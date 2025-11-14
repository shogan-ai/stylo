type t

type nest_fun := Document.t -> Document.t

val implied_nest : t option -> nest_fun

val mk : Document.t -> indent:int -> t
val extend : t option -> Document.t -> indent:int -> t * nest_fun

val group_with : t option -> Document.t -> Document.t * nest_fun
