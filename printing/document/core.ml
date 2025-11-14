module Requirement : sig
  type t

  val infinity : t

  val of_int : int -> t
  val to_int : t -> int

  val ( + ) : t -> t -> t
  (** Beware: not commutative! *)

  val nest : int -> t -> t

  val to_int_including_indent : current_indent:int -> t -> int
end = struct
  type t = {
    compact_width: int;
    potential_extra_indent: int;
  }

  let of_int i = { compact_width = i; potential_extra_indent = 0 }
  let to_int i = i.compact_width

  let infinity = of_int max_int

  let to_int_including_indent ~current_indent t =
    if t = infinity
    then t.compact_width
    else
      let indent = current_indent + t.potential_extra_indent in
      t.compact_width + indent

  let nest i t =
    if t = infinity
    then infinity
    else
      let indent = i + t.potential_extra_indent in
      { t with potential_extra_indent = indent }

  let (+) a b =
    if a = infinity || b = infinity
    then infinity
    else { a with compact_width = a.compact_width + b.compact_width }
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
  | Nest of Req.t * int * Condition.t * t
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
  | Nest (r, _, _, _)
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

let nest ?vanish i t =
  match i, t with
  | 0, _
  | _, Empty -> t
  | _ ->
    let cond = Option.value vanish ~default:Condition.(not always) in
    Nest (Req.nest i (requirement t), i, cond, t)


let group ?flatness = function
  | Empty -> Empty
  | t -> Group (requirement t, flatness, t)

let flatness_tracker () = ref false
