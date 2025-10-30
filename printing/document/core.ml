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

(* TODO: currently the tracker could be associated with several groups, we could
   add a dynamic check to prevent that. *)
type flatness = bool ref

module Condition = struct
  type t = bool lazy_t

  let always = lazy true

  let (!!) = Lazy.force

  let flat r = lazy !r
  let not t = lazy (not !!t)
  let (&&) t1 t2 = lazy (!!t1 && !!t2)

  let check = (!!)
end

type softness =
  | Hard (** always introduce a line break *)
  | Soft (** Vanishes after blank lines, adds a break otherwise *)
  | Softest (** Vanishes after a line break *)

type whitespace =
  | Line_break of softness
  | Break of int * softness
  | Non_breakable
  | Vanishing_space of Condition.t

(* FIXME: comments and strings can contain newlines, they should be represented
   by something other than "string". *)
type t =
  | Empty
  | Token of string
  | Optional of {
      vanishing_cond: Condition.t;
      before: whitespace option;
      token: string;
      after: whitespace option;
    }
  | Comment of string
  | Whitespace of whitespace
  | Cat of Req.t * t * t
  | Nest of Req.t * int * t
  | Relative_nest of Req.t * int * t
  | Group of Req.t * flatness option * t

let requirement = function
  | Empty -> Req.of_int 0
  | Token s
  | Comment s -> Req.of_int (String.length s)
  | Optional _ -> Req.of_int 0 (* vanishes if flat so ... *)
  | Whitespace Break (spaces, _) -> Req.of_int spaces
  | Whitespace Line_break _ -> Req.infinity
  | Whitespace Non_breakable -> Req.of_int 1
  | Whitespace Vanishing_space _ -> Req.of_int 0 (* FIXME: really? *)
  | Cat (r, _, _)
  | Nest (r, _, _)
  | Relative_nest (r, _, _)
  | Group (r, _, _) -> r

let empty = Empty
let string s = Token s
let break spaces = Whitespace (Break (spaces, Hard))
let soft_break spaces = Whitespace (Break (spaces, Soft))
let hardline = Whitespace (Line_break Hard)
let softline = Whitespace (Line_break Soft)
let softest_line = Whitespace (Line_break Softest)
let softest_break = Whitespace (Break (1, Softest))

let nbsp = Whitespace Non_breakable
let vanishing_space lvl = Whitespace (Vanishing_space lvl)

let opt_token ?ws_before ?ws_after vanishing_cond tok =
  match ws_before, ws_after with
  | Some _, Some _ -> invalid_arg "Document.opt_token"
  | _ ->
    Optional
      { vanishing_cond; before = ws_before; after = ws_after; token = tok }

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

let group ?flatness = function
  | Empty -> Empty
  | t -> Group (requirement t, flatness, t)

let flatness_tracker () = ref false
