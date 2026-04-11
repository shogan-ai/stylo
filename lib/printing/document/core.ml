(* TODO: currently the tracker could be associated with several groups, we could
   add a dynamic check to prevent that. *)
type flatness = bool ref

module Condition = struct
  type t =
    | True
    | False
    | Var of bool ref
    | And of t * t
    | Or of t * t

  let rec eval = function
    | True -> true
    | False -> false
    | Var r -> !r
    | And (a, b) -> eval a && eval b
    | Or (a, b) -> eval a || eval b

  let always = True

  let flat r = Var r
  let (&&) t1 t2 = And (t1, t2)
  let (||) t1 t2 = Or (t1, t2)

  let check = eval
end

module Requirement (* : sig
  type t

  val infinity : t

  val of_int : int -> t
  val to_int : t -> int

  val ( + ) : t -> t -> t
  (** Beware: not commutative!
      Keeps the indent of the first operand. *)

  val nest : int -> t -> t

  val to_int_including_indent : current_indent:int -> t -> int
end *) = struct
  type expr =
    | Infinity
    | Const of int
    | If of Condition.t * expr * expr
    | Add of expr * expr

  let add a b =
    match a, b with
    | Infinity, _
    | _, Infinity -> Infinity
    | Const a, Const b -> Const (a + b)
    | _ -> Add (a, b)

  let rec eval = function
    | Infinity -> max_int
    | Const i -> i
    | Add (a, b) ->
      let a = eval a in
      let b = eval b in
      if a = max_int || b = max_int
      then max_int
      else a + b
    | If (c, then_, else_) ->
      eval (if Condition.eval c then then_ else else_)

  type t = { compact_req: expr; potential_extra_indent: int }

  let if_ (c : Condition.t) ~then_ ~else_ =
    let compact_req =
      match c with
      | True -> then_
      | False -> else_
      | _ -> If (c, then_, else_)
    in
    { compact_req; potential_extra_indent = 0 }

  let of_int i = { compact_req = Const i; potential_extra_indent = 0 }

  let to_int { compact_req; _ } = eval compact_req

  let infinity = { compact_req = Infinity; potential_extra_indent = 0 }

  let to_int_including_indent ~current_indent e =
    let req = eval e.compact_req in
    if req = max_int
    then max_int
    else req + current_indent + e.potential_extra_indent

  let nest i e =
    let indent = i + e.potential_extra_indent in
    { e with potential_extra_indent = indent }

  let (+) a b =
    let compact_req = add a.compact_req b.compact_req in
    { a with compact_req }
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

type 'a can_vanish = {
  vanishing_cond: Condition.t;
  value: 'a;
}

type t =
  | Empty
  | Directive of pseudo_token
  | Token of pseudo_token can_vanish
  | Comment of { source_comment_id: int; doc: pseudo_token }
  | Comments_flushing_hint of {
      cmts_were_flushed: bool ref;
      floating_cmts_allowed: bool;
      pull_cmts_attached_before_hint: bool;
      ws_before: t;
      ws_after: t;
    }
  | Whitespace of whitespace can_vanish
  | Cat of Req.t * t * t
  | Nest of Req.t * int * Condition.t * t
  | Group of Req.t * int * flatness option * t

and pseudo_token =
  | Trivial of Req.t * string
  | Verbatim of Req.t * string * int
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
  | Verbatim (req, _, _)
  | Complex (req, _) -> req

let requirement = function
  | Empty -> Req.of_int 0
  | Directive pt
  | Comment { doc = pt; _ } -> pseudo_token_req pt
  | Token { vanishing_cond; value = pt } ->
    let pt_req = pseudo_token_req pt in
    Req.if_ vanishing_cond ~then_:(Const 0)
      ~else_:pt_req.compact_req
  | Whitespace { vanishing_cond; value = ws } ->
    let ws_req = ws_req ws in
    Req.if_ vanishing_cond ~then_:(Const 0)
      ~else_:ws_req.compact_req
  | Comments_flushing_hint _ -> Req.of_int 0
  | Cat (r, _, _)
  | Nest (r, _, _, _)
  | Group (r, _, _, _) -> r

let ws value = Whitespace { vanishing_cond = False; value }

let empty = Empty
let string s =
  let value = Trivial (Req.of_int (strlen s), s) in
  Token { vanishing_cond = False; value }

let break spaces = ws (Break (spaces, Hard))
let soft_break spaces = ws (Break (spaces, Soft))
let hardline = ws (Line_break Hard)
let softline = ws (Line_break Soft)
let softest_line = ws (Line_break Softest)
let softest_break = ws (Break (1, Softest))

let nbsp = ws Non_breakable

let vanishing_whitespace cond = function
  | Empty -> Empty
  | Whitespace { vanishing_cond = other_cond; value } ->
    Whitespace
      { vanishing_cond = Condition.(cond || other_cond) ; value }
  | _ -> invalid_arg "Document.vanishing_whitespace"

let flush_comments ~pull_preceeding_comments:pull ~floating_allowed:float
      ~ws_before ~ws_after =
  let cmts_were_flushed = ref false in
  let cond : Condition.t = Var cmts_were_flushed in
  let hint =
    Comments_flushing_hint {
      cmts_were_flushed; ws_before; ws_after;
      floating_cmts_allowed = float;
      pull_cmts_attached_before_hint = pull;
    }
  in
  cond, hint

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
    let ws ws =
      Option.map (vanishing_whitespace vanishing_cond) ws
      |> Option.value ~default:empty
    in
    let value = Trivial (Req.of_int (strlen tok), tok) in
    ws ws_before ^^ Token { vanishing_cond; value } ^^ ws ws_after

let nest ?(vanish=Condition.False) i t =
  match i, t with
  | 0, _
  | _, Empty -> t
  | _ -> Nest (Req.nest i (requirement t), i, vanish, t)

let group ?(margin=0) ?flatness = function
  | Empty -> Empty
  | t -> Group (requirement t, margin, flatness, t)

let flatness_tracker () =
  (* Setting this value to true initially let us assume flatness during the
     requirement computation, so as to not count the elements that would vanish
     in that mode, giving us an accurate measure. *)
  ref true

let pp_pseudo ppf pt =
  let s =
    match pt with
    | Trivial (_, s)
    | Verbatim (_, s, _) -> s
    | Complex _ -> "<complex>"
  in
  Format.pp_print_string ppf s

let pseudo_of_string s =
  if String.contains s '\n'
  then
    let lines = String.split_on_char '\n' s in
    let last = List.hd @@ List.rev lines in
    Verbatim (Req.infinity, s, String.length last)
  else
    Trivial (Req.of_int (strlen s), s)


let fancy_string s =
  Token
    { vanishing_cond = False
    ; value = pseudo_of_string s }

let formatted_string t =
  Token
    { vanishing_cond = False
    ; value = Complex (requirement t, t) }

let directive t =
  let t = softline ^^ group t ^^ softest_line in
  Directive (Complex (requirement t, t))


let as_comment ?id:(source_comment_id=(-1)) d =
  let doc = Complex (requirement d, d) in
  Comment { source_comment_id; doc }

let is_empty = function Empty -> true | _ -> false
