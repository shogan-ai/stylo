type t =
  | Heartbeat
  | Unsupported of (Bork.Message.t[@sexp.opaque])
[@@deriving sexp_of]

let box : (Elt.t List.t[@kind k]) -> t = Fn.id
