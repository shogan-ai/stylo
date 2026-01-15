
(* Wrapper around [Stdlib.Buffer] with whitespace buffering *)

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

(* No alignement here *)
let open_alignment_context _ = ()
let close_alignment_context _ = ()
let new_alignment_hint _ = ()
