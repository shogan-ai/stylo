module Requirement : sig
  type t

  val infinity : t

  val of_int : int -> t
  val to_int : t -> int

  val ( + ) : t -> t -> t
end = struct
  type t = int

  let infinity = max_int

  let of_int i = i
  let to_int i = i

  let (+) a b = if a = infinity || b = infinity then infinity else a + b
end

module Req = Requirement

type whitespace =
  | Line_break of { soft: bool }
  | Break of { spaces: int; soft: bool }
  | Vanishing_space

(* FIXME: comments and strings can contain newlines, they should be represented
   by something other than "string". *)
type t =
  | Empty
  | Token of string
  | Comment of string
  | Whitespace of whitespace
  | Cat of Req.t * t * t
  | Nest of Req.t * int * t
  | Relative_nest of Req.t * int * t
  | Group of Req.t * t

let requirement = function
  | Empty -> Req.of_int 0
  | Token s
  | Comment s -> Req.of_int (String.length s)
  | Whitespace Break { spaces; soft = _ } -> Req.of_int spaces
  | Whitespace Line_break _ -> Req.infinity
  | Whitespace Vanishing_space -> Req.of_int 0
  | Cat (r, _, _)
  | Nest (r, _, _)
  | Relative_nest (r, _, _)
  | Group (r, _) -> r

let empty = Empty
let string s = Token s
let break spaces = Whitespace (Break { spaces; soft = false })
let soft_break spaces = Whitespace (Break { spaces; soft = true })
let hardline = Whitespace (Line_break { soft = false })
let softline = Whitespace (Line_break { soft = true })

let vanishing_space = Whitespace Vanishing_space

(* FIXME *)
let comment s = Comment ("(*" ^ s ^ "*)")
let docstring s = Comment ("(**" ^ s ^ "*)")

let (^^) t1 t2 =
  match t1, t2 with
  | Empty, t
  | t, Empty -> t
  | _ ->
    let req = Req.(requirement t1 + requirement t2) in
    Cat (req, t1, t2)

let nest i = function
  | Empty -> Empty
  | t -> Nest (requirement t, i, t)

let relative_nest i = function
  | Empty -> Empty
  | t -> Relative_nest (requirement t, i, t)

let group = function
  | Empty -> Empty
  | t -> Group (requirement t, t)
