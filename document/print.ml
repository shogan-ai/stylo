open Core

type line_info =
  | Has_text
  | Is_empty
  | Follows_blank_line

type state = {
  max_width: int;
  column: int;
  line_indent: int;
  line: line_info;
}

let incr_col st i =
  { st with column = st.column + i }

let has_text st = { st with line = Has_text }

(* Wrapper with whitespace buffering *)
module Buffer : sig
  type t

  val create : int -> t
  val newline : indent:int -> t -> unit
  val add_spaces : t -> int -> unit
  val add_string : t -> string -> unit
  val contents : t -> string
end = struct

  type t = {
    buf: Buffer.t;
    mutable delayed_ws: int;
  }

  let flush_ws =
    let spaces = String.make 80 ' ' in
    let rec aux t n =
      if n <= 0 then
        t.delayed_ws <- 0
      else
        let nb = min t.delayed_ws 80 in
        Buffer.add_substring t.buf spaces 0 nb;
        aux t (n - nb)
    in
    fun t ->
      aux t t.delayed_ws


  let create n = { buf = Buffer.create n; delayed_ws = 0 }

  let newline ~indent t =
    Buffer.add_char t.buf '\n';
    t.delayed_ws <- indent

  let add_spaces t n =
    t.delayed_ws <- t.delayed_ws + n

  let add_string t s =
    flush_ws t;
    Buffer.add_string t.buf s

  let contents t = Buffer.contents t.buf
end

let newline st indent t =
  Buffer.newline t ~indent;
  let line =
    match st.line with
    | Has_text -> Is_empty
    | _ -> Follows_blank_line
  in
  { st with
    column = indent;
    line_indent = indent;
    line;
  }

let rec pretty buf state indent flat = function
  | Empty -> state
  | Token s
  | Comment s ->
    Buffer.add_string buf s;
    incr_col state (String.length s)
    |> has_text
  | Whitespace Break n ->
    if flat then (
      Buffer.add_spaces buf  n;
      incr_col state n
    ) else
      newline state indent buf
  | Whitespace Blank_line ->
    if flat || state.line = Follows_blank_line then (
      state
    ) else
      newline state indent buf
  | Whitespace Hard_line ->
    assert (not flat);
    newline state indent buf
  | Cat (_, t1, t2) ->
    let state' = pretty buf state indent flat t1 in
    pretty buf state' indent flat t2
  | Nest (_, i, t) ->
    pretty buf state (indent + i) flat t
  | Relative_nest (_, rel_indent, t) ->
    let indent =
      (* we don't want to decrease the indent, we just don't want to increase it
         too much. *)
      max (state.line_indent + rel_indent) indent
    in
    pretty buf state indent flat t
  | Group (req, t) ->
    let flat =
      flat
      || Requirement.(to_int @@ req + of_int state.column) <= state.max_width
    in
    pretty buf state indent flat t

let to_string ~width d =
  let sz = min (1024 * 1024) (Requirement.to_int @@ requirement d) in
  let buf = Buffer.create sz in
  let init =
    { max_width = width
    ; column = 0
    ; line_indent = 0
    ; line = Is_empty
    }
  in
  let _final_state = pretty buf init 0 false d in
  Buffer.contents buf
