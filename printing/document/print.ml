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

module type Buffer = sig
  type t

  val create : int -> t
  val newline : t -> unit
  val add_spaces : t -> int -> unit
  val add_string : t -> string -> unit
  val contents : t -> string

  val open_alignment_context : t -> unit
  val close_alignment_context : t -> unit
  val new_alignment_hint : t -> unit
end

(* Wrapper with whitespace buffering *)
module Regular_buffer : Buffer = struct

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
end

let spaces = String.make 80 ' '

module Column_based_buffer : Buffer = struct

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
end

module Printer(Buffer : Buffer) = struct
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
    | Token { vanishing_cond = Some cond; _ }
    | Whitespace { vanishing_cond = Some cond; _ }
      when Condition.check (Some cond) ->
      state
    | Token { value = pseudo; _ }
    | Comment pseudo -> pp_pseudo buf state indent flat pseudo
    | Whitespace { value = ws; _ } -> whitespace buf state indent flat ws
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
    | Alignement_context (_, t) ->
      Buffer.open_alignment_context buf;
      let state = pretty buf state indent flat t in
      Buffer.close_alignment_context buf;
      state
    | Alignement_point ->
      Buffer.new_alignment_hint buf;
      state

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
end

module Regular = Printer(Regular_buffer)

module Vertically_aligned = Printer(Column_based_buffer)

let to_string ?(vertically_aligned=false) ~width d =
  if vertically_aligned
  then Vertically_aligned.to_string ~width d
  else Regular.to_string ~width d
