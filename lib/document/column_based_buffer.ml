let spaces = String.make 80 ' '

let rec add_spaces buf n =
  if n > 0
  then (
    let nb = min n 80 in
    Buffer.add_substring buf spaces 0 nb;
    add_spaces buf (n - nb))
;;

(** The lines printed inside a [Core.vertically_aligned] document, kept until
    the end of the document so that the width of each column is known. *)
module Context = struct
  type line =
    | Cells of string list
      (** The first line of a row, split at alignment points. *)
    | Plain of
        { text : string
        ; between_rows : bool
             (** [true] for lines that are outside of any row (e.g. comments
                 inserted between rows). *)
        }

  type row_state =
    | Outside_row
    | On_first_line
      (** Only the first line of a row gets aligned: if the row starts with a
          multi-line pattern, its alignment points are on the following lines
          and are ignored. *)
    | After_first_line
    | Ended_on_first_line
      (** The row ended, but the line it was on didn't yet. *)

  type t =
    { mutable enabled : bool
    ; mutable lines : line list (* reversed *)
    ; mutable cells : Buffer.t list
    (* cells of the current line, reversed: the head is the current one *)
    ; mutable row : row_state
    }

  (* Maximum number of lines between two aligned lines (e.g. comments) for them
     to still be part of the same block. *)
  let max_lines_between_rows = 2

  let create ~enabled =
    { enabled; lines = []; cells = [ Buffer.create 16 ]; row = Outside_row }
  ;;

  let current_cell t = List.hd t.cells
  let disable t = t.enabled <- false
  let start_row t = t.row <- On_first_line

  let end_row t =
    match t.row with
    | On_first_line -> t.row <- Ended_on_first_line
    | After_first_line -> t.row <- Outside_row
    | Outside_row | Ended_on_first_line -> assert false
  ;;

  let alignment_point t =
    match t.row with
    | On_first_line -> t.cells <- Buffer.create 16 :: t.cells
    | Outside_row | After_first_line | Ended_on_first_line -> ()
  ;;

  let end_line t =
    let text () =
      let buf = Buffer.create 80 in
      List.iter (Buffer.add_buffer buf) (List.rev t.cells);
      Buffer.contents buf
    in
    let line =
      match t.row with
      | On_first_line ->
        t.row <- After_first_line;
        Cells (List.rev_map Buffer.contents t.cells)
      | Ended_on_first_line ->
        t.row <- Outside_row;
        Cells (List.rev_map Buffer.contents t.cells)
      | Outside_row -> Plain { text = text (); between_rows = true }
      | After_first_line -> Plain { text = text (); between_rows = false }
    in
    t.lines <- line :: t.lines;
    t.cells <- [ Buffer.create 16 ]
  ;;

  (* Index of the last non empty cell: cells before it get padded, the following
     ones are empty and do not. *)
  let last_non_empty cells =
    List.fold_left
      (fun (i, last) cell -> i + 1, if cell = "" then last else i)
      (0, -1)
      cells
    |> snd
  ;;

  (* Splits the lines in blocks of lines aligned with each other. *)
  let blocks lines =
    let rec aux blocks block lines_between = function
      | [] -> List.rev (List.rev block :: blocks) |> List.filter (( <> ) [])
      | (Cells _ as line) :: lines ->
        if lines_between > max_lines_between_rows
        then (
          (* We want the lines between the rows in the previous block. *)
          let between = Std.List.take lines_between block in
          let block = Std.List.drop lines_between block in
          aux (List.rev block :: blocks) (line :: between) 0 lines)
        else aux blocks (line :: block) 0 lines
      | (Plain { between_rows; _ } as line) :: lines ->
        let lines_between =
          if between_rows then lines_between + 1 else lines_between
        in
        aux blocks (line :: block) lines_between lines
    in
    aux [] [] 0 lines
  ;;

  let column_widths block =
    let widths = Hashtbl.create 8 in
    List.iter
      (function
        | Plain _ -> ()
        | Cells cells ->
          let last = last_non_empty cells in
          List.iteri
            (fun i cell ->
              if i < last
              then (
                let w = Option.value ~default:0 (Hashtbl.find_opt widths i) in
                Hashtbl.replace widths i (max w (Core.strlen cell))))
            cells)
      block;
    fun i -> Hashtbl.find widths i
  ;;

  let render_block buf ~first block =
    let width = column_widths block in
    List.iteri
      (fun i line ->
        if not (first && i = 0) then Buffer.add_char buf '\n';
        match line with
        | Plain { text; _ } -> Buffer.add_string buf text
        | Cells cells ->
          let last = last_non_empty cells in
          List.iteri
            (fun i cell ->
              Buffer.add_string buf cell;
              if i < last then add_spaces buf (width i - Core.strlen cell))
            cells)
      block
  ;;

  (** The (aligned) content of the context. Does not end with a newline. *)
  let contents t =
    end_line t;
    let lines = List.rev t.lines in
    let blocks =
      if t.enabled
      then blocks lines
      else
        (* No alignment: a single block with no cells. *)
        [ List.map
            (function
              | Cells cells ->
                Plain { text = String.concat "" cells; between_rows = false }
              | line -> line)
            lines
        ]
    in
    let buf = Buffer.create 1024 in
    List.iteri (fun i block -> render_block buf ~first:(i = 0) block) blocks;
    Buffer.contents buf
  ;;
end

type t =
  { out : Buffer.t
  ; mutable contexts : Context.t list
  ; mutable delayed_ws : int
  ; mutable line_has_text : bool
  }

let create n =
  { out = Buffer.create n
  ; contexts = []
  ; delayed_ws = 0
  ; line_has_text = false
  }
;;

let current_buffer t =
  match t.contexts with
  | [] -> t.out
  | ctxt :: _ -> Context.current_cell ctxt
;;

let raw_newline t =
  match t.contexts with
  | [] -> Buffer.add_char t.out '\n'
  | ctxt :: _ -> Context.end_line ctxt
;;

(* Adds a string which might contain newlines, without changing the state of the
   current line. *)
let raw_add_string t s =
  match t.contexts with
  | [] -> Buffer.add_string t.out s
  | _ ->
    List.iteri
      (fun i line ->
        if i > 0 then raw_newline t;
        Buffer.add_string (current_buffer t) line)
      (String.split_on_char '\n' s)
;;

let flush_ws t =
  add_spaces (current_buffer t) t.delayed_ws;
  t.delayed_ws <- 0
;;

let newline t =
  t.delayed_ws <- 0;
  t.line_has_text <- false;
  raw_newline t
;;

let add_spaces t n = t.delayed_ws <- t.delayed_ws + n

let add_string t s =
  if s <> ""
  then (
    flush_ws t;
    raw_add_string t s;
    t.line_has_text
    <- (match String.rindex_opt s '\n' with
        | None -> true
        | Some i -> i < String.length s - 1))
;;

let open_alignment_context t ~enabled =
  t.contexts <- Context.create ~enabled :: t.contexts
;;

let close_alignment_context t =
  match t.contexts with
  | [] -> invalid_arg "Column_based_buffer.close_alignment_context"
  | ctxt :: contexts ->
    t.contexts <- contexts;
    raw_add_string t (Context.contents ctxt)
;;

let start_alignment_row t =
  match t.contexts with
  | [] -> assert false
  | ctxt :: _ ->
    (* Rows sharing a line (or starting after some text) cannot be aligned. *)
    if t.line_has_text then Context.disable ctxt;
    Context.start_row ctxt
;;

let end_alignment_row t =
  match t.contexts with
  | [] -> assert false
  | ctxt :: _ -> Context.end_row ctxt
;;

let alignment_point t =
  match t.contexts with
  | [] -> ()
  | ctxt :: _ -> Context.alignment_point ctxt
;;

let contents t =
  assert (t.contexts = []);
  Buffer.contents t.out
;;
