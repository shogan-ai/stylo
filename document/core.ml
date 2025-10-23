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

type softness =
  | Hard (** always introduce a line break *)
  | Soft (** Vanishes after blank lines, adds a break otherwise *)
  | Softest (** Vanishes after a line break *)

type whitespace =
  | Line_break of softness
  | Break of int * softness
  | Non_breakable
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
  | Whitespace Break (spaces, _) -> Req.of_int spaces
  | Whitespace Line_break _ -> Req.infinity
  | Whitespace Non_breakable -> Req.of_int 1
  | Whitespace Vanishing_space -> Req.of_int 0 (* FIXME: really? *)
  | Cat (r, _, _)
  | Nest (r, _, _)
  | Relative_nest (r, _, _)
  | Group (r, _) -> r

let empty = Empty
let string s = Token s
let break spaces = Whitespace (Break (spaces, Hard))
let soft_break spaces = Whitespace (Break (spaces, Soft))
let hardline = Whitespace (Line_break Hard)
let softline = Whitespace (Line_break Soft)
let softest_line = Whitespace (Line_break Softest)
let softest_break = Whitespace (Break (1, Softest))

let nbsp = Whitespace Non_breakable
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
