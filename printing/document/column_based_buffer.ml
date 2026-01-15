
let spaces = String.make 80 ' '

module Alignment_context = struct
  type t = {
    mutable column_max_size: int array;
    mutable current_row: Buffer.t array;
    mutable current_column: int;
    mutable current_column_size: int;
    buffered_rows: Buffer.t array Queue.t;
    output: Buffer.t;
  }

  let fresh output =
    { column_max_size = Array.make 10 0
    ; current_row = Array.init 10 (fun _ -> Buffer.create 10)
    ; current_column = 0
    ; current_column_size = 0
    ; buffered_rows = Queue.create ()
    ; output }

  let flush_row output sizes =
    Array.iteri (fun col buf ->
      let extra_spaces = sizes.(col) - Buffer.length buf in
      Buffer.add_buffer output buf;
      let rec aux n =
        if n > 0 then
          let nb = min n 80 in
          Buffer.add_substring output spaces 0 nb;
          aux (n - nb)
      in
      aux extra_spaces
    )

  let flush_buffered_rows t =
    Queue.iter (fun row ->
      flush_row t.output t.column_max_size row;
      Buffer.add_char t.output '\n'
    ) t.buffered_rows;
    Queue.clear t.buffered_rows

  let flush t =
    flush_buffered_rows t;
    flush_row t.output t.column_max_size t.current_row

  let add_substring t s pos len =
    let buffer = t.current_row.(t.current_column) in
    Buffer.add_substring buffer s pos len;
    t.current_column_size <- t.current_column_size + len

  let add_string t s =
    let buffer = t.current_row.(t.current_column) in
    Buffer.add_string buffer s;
    t.current_column_size <- t.current_column_size + String.length s

  let newline t =
    let row = t.current_row in
    Queue.add row t.buffered_rows;
    t.current_column <- 0;
    t.current_column_size <- 0;
    t.current_row <- Array.init (Array.length row) (fun _ -> Buffer.create 10)

  let next_col t =
    (* update column size if current is larger than previous ones *)
    if t.current_column_size > t.column_max_size.(t.current_column)
    then t.column_max_size.(t.current_column) <- t.current_column_size;
    (* resize array if necessary *)
    let next_col = t.current_column + 1 in
    let nb_cols = Array.length t.column_max_size in
    if next_col >= nb_cols then (
      let max_sizes =
        Array.init (nb_cols * 2) (fun i ->
          if i < nb_cols then t.column_max_size.(i) else 0
        )
      in
      let columns =
        Array.init (nb_cols * 2) (fun i ->
          if i < nb_cols then t.current_row.(i) else Buffer.create 10
        )
      in
      t.column_max_size <- max_sizes;
      t.current_row <- columns
    );
    t.current_column <- next_col;
    t.current_column_size <- 0
end

type t = {
  buf: Buffer.t;
  ctxts: Alignment_context.t Stack.t;
  mutable delayed_ws: int;
}

let create n =
  { buf = Buffer.create n
  ; ctxts = Stack.create ()
  ; delayed_ws = 0 }

let flush_ws t =
  let add_substring =
    match Stack.top_opt t.ctxts with
    | Some ctxt -> Alignment_context.add_substring ctxt
    | None -> Buffer.add_substring t.buf
  in
  let rec aux n =
    if n > 0 then
      let nb = min n 80 in
      add_substring spaces 0 nb;
      aux (n - nb)
  in
  aux t.delayed_ws;
  t.delayed_ws <- 0

let newline t =
  t.delayed_ws <- 0;
  match Stack.top_opt t.ctxts with
  | Some ctxt -> Alignment_context.newline ctxt
  | None -> Buffer.add_char t.buf '\n'

let add_spaces t n =
  t.delayed_ws <- t.delayed_ws + n

let add_string t s =
  flush_ws t;
  match Stack.top_opt t.ctxts with
  | Some ctxt -> Alignment_context.add_string ctxt s
  | None -> Buffer.add_string t.buf s

let open_alignment_context t =
  let buf =
    match Stack.top_opt t.ctxts with
    | Some ctxt -> ctxt.current_row.(ctxt.current_column)
    | None -> t.buf
  in
  Stack.push (Alignment_context.fresh buf) t.ctxts

let close_alignment_context t =
  let ctxt = Stack.pop t.ctxts in
  Alignment_context.flush ctxt

let new_alignment_hint t =
  let ctxt = Stack.top t.ctxts in
  Alignment_context.next_col ctxt

let contents t =
  Stack.iter Alignment_context.flush t.ctxts;
  Buffer.contents t.buf
