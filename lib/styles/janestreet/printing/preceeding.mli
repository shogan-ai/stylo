type t
type nest_fun := Document.t -> Document.t

val implied_nest : t option -> nest_fun

(** [mk doc space]: [doc] followed by [space]. [after_comments] (by default
    [break 1]) separates comments after [doc] from what follows. *)
val mk : ?indent:int -> ?after_comments:Document.t -> Document.t -> Document.t -> t

val tight : ?indent:int -> Document.t -> t
val spaced : ?indent:int -> Document.t -> t

(** [;] followed by a non-breaking space, for separators in lists, arrays and
    records. *)
val semi : t

(** A line break if the group tracked by [flatness] is broken, a space
    otherwise. *)
val line_if_broken : Document.flatness -> Document.t

(** Like [semi], but a field after a comment starts a new line if the record
    (whose group is tracked by [flatness]) is broken. *)
val field_semi : flatness:Document.flatness -> t

val ( + ) : t option -> t -> t * nest_fun
val group_with : t option -> Document.t -> Document.t * nest_fun

(** Whether there's no line break possible in the preceeding tokens (ignoring
    comments). *)
val never_breaks : t option -> bool
