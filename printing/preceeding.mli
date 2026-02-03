type t

type nest_fun := Document.t -> Document.t

val implied_nest : t option -> nest_fun

val mk : ?indent:int -> Document.t -> Document.t -> t

val tight : ?indent:int -> Document.t -> t
val spaced : ?indent:int -> Document.t -> t

val ( + ) : t option -> t -> t * nest_fun

val group_with : t option -> Document.t -> Document.t * nest_fun
