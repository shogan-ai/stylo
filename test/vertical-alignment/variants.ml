type t =
  | Int of int (** foo *)
  | Float of float (** bar *)
  | None (** baz *)

type u = A | Bbbb of int | C of string * int

(* A single constructor. *)
type v = Single of int

type w =
  | Short
  | Mid of int [@attr]
  | Longer_name of int * string
