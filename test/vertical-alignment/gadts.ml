type _ t =
  | Trivial : int t (** lol *)
  | Complex : int -> bool t (** wow *)
  | Regular of string
