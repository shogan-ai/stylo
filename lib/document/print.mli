
(** tries to print the document so that no line goes past [width] columns.

    Vertical alignment (cf. [Core.vertically_aligned]) is only done when
    [vertically_aligned] is [true] (defaults to [false]). *)
val to_string : ?vertically_aligned:bool -> width:int -> Core.t -> string
