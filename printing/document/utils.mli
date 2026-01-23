open Core

val ( ^?^ ) : t -> t -> t
val ( ^/^ ) : t -> t -> t
val ( ^?/^ ) : t -> t -> t
val foldli : (int -> 'acc -> 'elt -> 'acc) -> 'acc -> 'elt list -> 'acc
val flow : t -> t list -> t
val separate : t -> t list -> t
val flow_map : t -> ('a -> t) -> 'a list -> t
val separate_map : t -> ('a -> t) -> 'a list -> t
val prefix : ?indent:int -> ?extra_indent:int -> ?spaces:int -> t -> t -> t
