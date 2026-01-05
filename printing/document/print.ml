open Core

type line_info =
  | Has_text
  | Is_empty
  | Follows_blank_line

type state = {
  max_width: int;
  column: int;
  line_indent: int; (* might refer to the previous line! *)
  line: line_info;
}

let incr_col st i =
  { st with column = st.column + i }

let has_text st = { st with line = Has_text }

(* Wrapper with whitespace buffering *)
module Buffer : sig
  type t

  val create : int -> t
  val newline : t -> unit
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

  let newline t =
    t.delayed_ws <- 0;
    Buffer.add_char t.buf '\n'

  let add_spaces t n =
    t.delayed_ws <- t.delayed_ws + n

  let add_string t s =
    flush_ws t;
    Buffer.add_string t.buf s

  let contents t = Buffer.contents t.buf
end

let newline st soft t =
  match soft, st.line with
  | Soft, Follows_blank_line
  | Softest, (Is_empty | Follows_blank_line) -> st
  | _ ->
    Buffer.newline t;
    let line =
      match st.line with
      | Has_text -> Is_empty
      | _ -> Follows_blank_line
    in
    { st with
      column = 0;
      line;
    }

let text buf state indent len s =
  let state =
    match state.line with
    | Has_text -> state
    | _ ->
      Buffer.add_spaces buf indent;
      { state with column = indent; line_indent = indent }
  in
  Buffer.add_string buf s;
  begin match len with
  | `Full slen -> incr_col state (Requirement.to_int slen)
  | `Last len -> { state with column = len }
  end;
  |> has_text

let add_spaces buf state indent n =
  let n =
    match state.line with
    | Has_text -> n
    | _ -> indent + n
  in
  Buffer.add_spaces buf n;
  incr_col state n
  |> has_text

let whitespace buf state indent flat = function
  | Break (spaces, soft) ->
    if flat then (
      Buffer.add_spaces buf spaces;
      incr_col state spaces
    ) else
      newline state soft buf
  | Line_break soft ->
    assert (not flat);
    newline state soft buf
  | Non_breakable ->
    add_spaces buf state indent 1

let rec pretty buf state indent flat = function
  | Empty
  | Comments_flushing_hint _ -> state
  | Token pseudo
  | Comment pseudo -> pp_pseudo buf state indent flat pseudo
  | Optional { vanishing_cond; token } ->
    if Condition.check (Some vanishing_cond) then
      state
    else
      pp_pseudo buf state indent flat token
  | Whitespace (vanishing_cond, ws) ->
    if Condition.check vanishing_cond then
      state
    else
      whitespace buf state indent flat ws
  | Cat (_, t1, t2) ->
    let state' = pretty buf state indent flat t1 in
    pretty buf state' indent flat t2
  | Nest (_, i, vanish, t) ->
    let indent = indent + if Condition.check vanish then 0 else i in
    pretty buf state indent flat t
  | Group (req, margin, flat_track_opt, t) ->
    let flat =
      flat ||
      let gp_req =
        let open Requirement in
        match state.line with
        | Has_text -> to_int (req + of_int state.column)
        | _ ->
          assert (state.column = 0);
          to_int_including_indent ~current_indent:indent req
      in
      gp_req <= (state.max_width - margin)

    in
    Option.iter (fun flatness ->
      (flatness : flatness :> bool ref) := flat
    ) flat_track_opt;
    pretty buf state indent flat t

and pp_pseudo buf state indent flat = function
  | Trivial (len, s) -> text buf state indent (`Full len) s
  | Verbatim (_, s, len) -> text buf state indent (`Last len) s
  | Complex (_, t) -> pretty buf state indent flat t

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
