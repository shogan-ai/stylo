(** tries to print the document so that no line goes past [width] columns *)
val to_string : width:int -> Core.t -> string

