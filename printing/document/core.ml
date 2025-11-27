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
  let (&&) t1 t2 = lazy (!!t1 && !!t2)

  let check = function
    | None -> false
    | Some t -> !!t
end

type softness =
  | Hard (** always introduce a line break *)
  | Soft (** Vanishes after blank lines, adds a break otherwise *)
  | Softest (** Vanishes after a line break *)

type whitespace =
  | Line_break of softness
  | Break of int * softness
  | Non_breakable
(* FIXME: comments and strings can contain newlines, they should be represented
   by something other than "string". *)
type t =
  | Empty
  | Token of pseudo_token
  | Optional of {
      vanishing_cond: Condition.t;
      token: pseudo_token;
    }
  | Comment of pseudo_token
  | Whitespace of Condition.t option * whitespace
  | Cat of Req.t * t * t
  | Nest of Req.t * int * Condition.t option * t
  | Group of Req.t * int * flatness option * t

and pseudo_token =
  | Trivial of Req.t * string
  | Complex of Req.t * t

let ws_req = function
  | Break (spaces, _) -> Req.of_int spaces
  | Line_break _ -> Req.infinity
  | Non_breakable -> Req.of_int 1

let strlen s =
  let byte_len = String.length s in
  let rec aux len pos =
    if pos >= byte_len
    then len
    else
      let d = String.get_utf_8_uchar s pos in
      let nb_bytes = Uchar.utf_decode_length d in
      aux (len + 1) (pos + nb_bytes)
  in
  aux 0 0

let pseudo_token_req = function
  | Trivial (req, _)
  | Complex (req, _) -> req

let requirement = function
  | Empty -> Req.of_int 0
  | Token pt
  | Comment pt -> pseudo_token_req pt
  | Optional _ -> Req.of_int 0 (* vanishes if flat so ... *)
  | Whitespace (Some _, _) -> Req.of_int 0
  | Whitespace (_, ws) -> ws_req ws
  | Cat (r, _, _)
  | Nest (r, _, _, _)
  | Group (r, _, _, _) -> r

let ws ws = Whitespace (None, ws)

let empty = Empty
let string s = Token (Trivial (Req.of_int (strlen s), s))
let break spaces = ws (Break (spaces, Hard))
let soft_break spaces = ws (Break (spaces, Soft))
let hardline = ws (Line_break Hard)
let softline = ws (Line_break Soft)
let softest_line = ws (Line_break Softest)
let softest_break = ws (Break (1, Softest))

let nbsp = ws Non_breakable
let vanishing_space cond = Whitespace (Some cond, Non_breakable)

(* FIXME *)
let comment s = Comment (Trivial (Req.of_int (strlen s), s))
let docstring s = comment @@ "(**" ^ s ^ "*)"
let comment s = comment @@ "(*" ^ s ^ "*)"

let (^^) t1 t2 =
  match t1, t2 with
  | Empty, t
  | t, Empty -> t
  | _ ->
    let req = Req.(requirement t1 + requirement t2) in
    Cat (req, t1, t2)

let opt_token ?ws_before ?ws_after vanishing_cond tok =
  match ws_before, ws_after with
  | Some _, Some _ -> invalid_arg "Document.opt_token"
  | _ ->
    let ws = function
      | None -> empty
      | Some ws -> Whitespace (Some vanishing_cond, ws)
    in
    ws ws_before ^^
    Optional
      { vanishing_cond; token = Trivial (Req.of_int (strlen tok), tok) } ^^
    ws ws_after

let nest ?vanish i t =
  match i, t with
  | 0, _
  | _, Empty -> t
  | _ -> Nest (Req.nest i (requirement t), i, vanish, t)

let group ?(margin=0) ?flatness = function
  | Empty -> Empty
  | t -> Group (requirement t, margin, flatness, t)

let flatness_tracker () = ref false

let pp_pseudo ppf pt =
  let s =
    match pt with
    | Trivial (_, s) -> s
    | Complex _ -> "<complex>"
  in
  Format.pp_print_string ppf s

let fancy_string s =
  let leaf = Token (Trivial (Req.of_int (strlen s) ,s)) in
  if String.contains s '\n'
  then Token (Complex (Requirement.infinity, leaf))
  else leaf

let formatted_string t =
  Token (Complex (requirement t, t))

let is_empty = function Empty -> true | _ -> false
