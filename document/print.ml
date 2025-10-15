open Core

let spaces = String.make 80 ' '

type state = {
  max_width: int;
  column: int;
  line_indent: int;
}

let incr_col st i =
  { st with column = st.column + i }

let newline st indent buf =
  Buffer.add_char buf '\n';
  Buffer.add_substring buf spaces 0 indent;
  { st with
    column = indent;
    line_indent = indent;
  }

let rec pretty buf state indent flat = function
  | Empty -> state
  | Token s
  | Comment s ->
    Buffer.add_string buf s;
    incr_col state (String.length s)
  | Whitespace Break n ->
    if flat then (
      Buffer.add_substring buf spaces 0 n;
      incr_col state n
    ) else
      newline state indent buf
  | Whitespace HardLine ->
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
      || Req.(to_int @@ req + of_int state.column) <= state.max_width
    in
    pretty buf state indent flat t

let to_string ~width d =
  let sz = min (1024 * 1024) (Req.to_int @@ requirement d) in
  let buf = Buffer.create sz in
  let init =
    { max_width = width
    ; column = 0
    ; line_indent = 0
    }
  in
  let _final_state = pretty buf init 0 false d in
  Buffer.contents buf
