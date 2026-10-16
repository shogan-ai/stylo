(* The content of tables (enabled alignment contexts) is recorded as a tree,
   which is rendered when the table is closed, and then replayed into the
   enclosing context (or the output) as if it had been printed with its padding.
   *)

module Line_segment = struct
  type t =
    | Text of string
    | Spaces of int

  let has_text =
    List.exists (function
      | Text _ -> true
      | Spaces _ -> false)
  ;;

  let width =
    List.fold_left
      (fun acc -> function
        | Text s -> acc + Core.strlen s
        | Spaces n -> acc + n)
      0
  ;;
end

type item =
  | Seg of Line_segment.t
  | Newline
  | Row of item list
  | Cell of Core.alignment * item list

(** {1 Rendering of a table} *)

(* A cell of a row, its anonymous cells (see below) being represented as
   ['anon]s.

   The sub-cells of a subdivided cell are its explicit cells (the recorded
   [Cell]s), and the pieces of text around them: these are "anonymous" cells,
   aligned just like the explicit ones, but which can't be subdivided. They
   alternate, starting and ending with an anonymous cell (possibly empty):
   {v a0 c1 a1 ... cn an v}
   which is represented as [first = a0] and [rest = [(c1, a1); ...; (cn, an)]],
   [rest] being non-empty. The alternation is what makes the sub-cells of
   different rows line up.

   A cell without explicit cells is atomic: its content is a single anonymous
   cell, which spans the whole cell, and is aligned within it. The content of a
   subdivided cell can't be aligned: its sub-cells are. *)
type 'anon cell =
  | Atomic of Core.alignment * 'anon
  | Subdivided of
      { first : 'anon
      ; rest : ('anon cell * 'anon) list
      }

type anon =
  { segs : Line_segment.t list
  ; followed : bool
  }

type line =
  | Plain of Line_segment.t list
  | Aligned of anon cell (** The first line of a row. *)

module Lines : sig
  val of_items : item list -> line list
  val blocks : line list -> line list list
end = struct

  (* [split_first_line align items] is the cell made of the [items] preceding
     the first newline, and the items following it, if there is one. *)
  let rec split_first_line align items =
    let add_cell c (first, rev_rest) = first, (c, []) :: rev_rest in
    let add_seg s = function
      | rev_first, [] -> s :: rev_first, []
      | rev_first, (c, rev_a) :: rev_rest ->
        rev_first, (c, s :: rev_a) :: rev_rest
    in
    let to_cell align = function
      | rev_segs, [] -> Atomic (align, List.rev rev_segs)
      | rev_first, rev_rest ->
        Subdivided
          { first = List.rev rev_first
          ; rest = List.rev_map (fun (c, rev_a) -> c, List.rev rev_a) rev_rest
          }
    in
    let rec loop acc = function
      | Row _ :: _ ->
        (* Rows are only recorded directly in tables. *)
        assert false
      | [] -> to_cell align acc, None
      | Seg s :: items -> loop (add_seg s acc) items
      | Newline :: items ->
        (* Only the beginning of the cell is on the line, it can't be aligned to
           the right. *)
        to_cell Core.Left acc, Some items
      | Cell (cell_align, cell_items) :: items ->
        let cell, next_lines = split_first_line cell_align cell_items in
        let acc = add_cell cell acc in
        match next_lines with
        | None -> loop acc items
        | Some next_items -> to_cell Core.Left acc, Some (next_items @ items)
    in
    loop ([], []) items
  ;;

  let prepend_spaces spaces cell =
    let spaces = Line_segment.Spaces spaces in
    match cell with
    | Atomic (align, a) -> Atomic (align, spaces :: a)
    | Subdivided cell -> Subdivided { cell with first = spaces :: cell.first }
  ;;

  let append_segments segs = function
    | Atomic (align, a) -> Atomic (align, a @ segs)
    | Subdivided cell ->
      Subdivided
        { cell with
          rest = Std.List.map_last ~f:(fun (c, a) -> c, a @ segs) cell.rest
        }
  ;;

  (* [with_followed row] is [row], its anonymous cells telling whether some text
     follows them on their line. *)
  let with_followed row =
    let rec aux ~followed = function
      | Atomic (align, segs) ->
        ( Atomic (align, { segs; followed })
        , followed || Line_segment.has_text segs )
      | Subdivided cell ->
        let rest, first_followed =
          List.fold_right
            (fun (c, a) (rest, followed) ->
              let a = { segs = a; followed } in
              let c, followed =
                aux ~followed:(followed || Line_segment.has_text a.segs) c
              in
              (c, a) :: rest, followed)
            cell.rest
            ([], followed)
        in
        ( Subdivided
            { first = { segs = cell.first; followed = first_followed }; rest }
        , first_followed || Line_segment.has_text cell.first )
    in
    fst (aux ~followed:false row)
  ;;

  let of_items items =
    let aligned row = Aligned (with_followed row) in
    let mk_line row rev_segs =
      let segs = List.rev rev_segs in
      match row with
      | None -> Plain segs
      | Some row -> aligned (append_segments segs row)
    in
    let rec at_bol rev_lines spaces = function
      | Seg (Line_segment.Spaces s) :: items ->
        at_bol rev_lines (s + spaces) items
      | Row row_items :: items ->
        let row, next = split_first_line Core.Left row_items in
        let row = prepend_spaces spaces row in
        (match next with
         | None -> rest_of_line rev_lines (Some row) [] items
         | Some next -> at_bol (aligned row :: rev_lines) 0 (next @ items))
      | otherwise ->
        rest_of_line rev_lines None [ Line_segment.Spaces spaces ] otherwise
    and rest_of_line rev_lines row rev_segs = function
      | [] -> List.rev (mk_line row rev_segs :: rev_lines)
      | Seg s :: items -> rest_of_line rev_lines row (s :: rev_segs) items
      | Newline :: items -> at_bol (mk_line row rev_segs :: rev_lines) 0 items
      | (Row content | Cell (_, content)) :: items ->
        (* Rows which can't be aligned, and the cells of the following lines of
           rows: only their content matters. *)
        rest_of_line rev_lines row rev_segs (content @ items)
    in
    at_bol [] 0 items
  ;;

  (* Maximum number of lines between two aligned lines (e.g. comments, or the
     following lines of a multi-line row) for them to still be part of the same
     block. *)
  let max_lines_between_rows = 2

  let blocks lines =
    let ( ++ ) rb rbs = List.rev rb :: rbs in
    let rec aux rev_blocks rev_block lines_between = function
      | [] -> List.rev (rev_block ++ rev_blocks)
      | line :: lines ->
        match line with
        | Plain _ ->
          aux rev_blocks (line :: rev_block) (lines_between + 1) lines
        | Aligned _ ->
          if lines_between > max_lines_between_rows
          then aux (rev_block ++ rev_blocks) [ line ] 0 lines
          else aux rev_blocks (line :: rev_block) 0 lines
    in
    aux [] [] 0 lines
  ;;
end

module Layout : sig

  (* The widths of the cells of the rows of a block, with the same shape as the
     cells ([cell]): the cells at the same position in the rows get the same
     width. The [width] of a subdivided cell is at least the width of its
     sub-cells, more if the cell is wider in the rows where it is atomic. *)
  type t =
    | Atomic of int
    | Subdivided of
        { width : int
        ; first : int
        ; rest : (t * int) list
        }

  val of_block : line list -> t
  val width : t -> int
end = struct
  type t =
    | Atomic of int
    | Subdivided of
        { width : int
        ; first : int
        ; rest : (t * int) list
        }

  let empty = Atomic 0

  let width = function
    | Atomic width | Subdivided { width; _ } -> width
  ;;

  let rest_width =
    List.fold_left (fun acc (layout, a) -> acc + width layout + a) 0
  ;;

  let subdivided ?(min_width = 0) first rest =
    Subdivided { width = max min_width (first + rest_width rest); first; rest }
  ;;

  let anon_width a = if a.followed then Line_segment.width a.segs else 0

  (* Cells which are not followed by any text don't constrain the layout: they
     don't need to be padded. Unless they are right aligned. *)
  let rec of_cell : anon cell -> t = function
    | Atomic (Core.Right, a) -> Atomic (Line_segment.width a.segs)
    | Atomic (Core.Left, a) -> Atomic (anon_width a)
    | Subdivided { first; rest; _ } ->
      subdivided
        (anon_width first)
        (List.map (fun (c, a) -> of_cell c, anon_width a) rest)
  ;;

  let of_line = function
    | Plain _ -> empty
    | Aligned row -> of_cell row
  ;;

  let rec zip_with f xs ys =
    match xs, ys with
    | [], l | l, [] -> l
    | x :: xs, y :: ys -> f x y :: zip_with f xs ys
  ;;

  let rec merge l1 l2 =
    match l1, l2 with
    | Atomic w1, Atomic w2 -> Atomic (max w1 w2)
    | Atomic w, Subdivided l | Subdivided l, Atomic w ->
      (* The atomic cell spans the whole cell, the sub-cells aren't affected. *)
      Subdivided { l with width = max w l.width }
    | Subdivided l1, Subdivided l2 ->
      (* N.B. the merged sub-cells are at least as wide as the ones of [l1] and
         of [l2], so the widths of [l1] and [l2] are enough to account for the
         rows where the cell is atomic. *)
      subdivided
        ~min_width:(max l1.width l2.width)
        (max l1.first l2.first)
        (zip_with
           (fun (l1, a1) (l2, a2) -> merge l1 l2, max a1 a2)
           l1.rest
           l2.rest)
  ;;

  let of_block =
    List.fold_left (fun layout line -> merge layout (of_line line)) empty
  ;;
end

module Render : sig

  (* The content of a table, aligned. Only made of [Seg]s and [Newline]s. *)
  val table : item list -> item list
end = struct

  (* The rendered items are accumulated in reverse order, in [acc]. *)

  let add_segs segs acc = List.fold_left (fun acc s -> Seg s :: acc) acc segs

  let add_spaces n acc =
    if n > 0 then Seg (Line_segment.Spaces n) :: acc else acc
  ;;

  let render_anonymous a width acc =
    let acc = add_segs a.segs acc in
    if a.followed
    then add_spaces (width - Line_segment.width a.segs) acc
    else acc
  ;;

  let rec render_cell cell layout acc =
    match cell, layout with
    | Atomic (Core.Left, a), _ -> render_anonymous a (Layout.width layout) acc
    | Atomic (Core.Right, a), _ ->
      acc
      |> add_spaces (Layout.width layout - Line_segment.width a.segs)
      |> add_segs a.segs
    | Subdivided _, Layout.Atomic _ ->
      (* The layout was computed from this cell, among others. *)
      assert false
    | Subdivided cell, Layout.Subdivided l ->
      (* [remaining] is the width of the cell after the sub-cells rendered so
         far, [followed] tells whether the last of them is followed: once all
         the sub-cells are rendered, whether [cell] is. If it is, so are all the
         sub-cells, which were therefore padded to their widths. *)
      let rec aux ~followed ~remaining rest rest_layouts acc =
        match rest, rest_layouts with
        | (c, a) :: rest, (c_layout, w) :: rest_layouts ->
          acc
          |> render_cell c c_layout
          |> render_anonymous a w
          |> aux
               ~followed:a.followed
               ~remaining:(remaining - Layout.width c_layout - w)
               rest
               rest_layouts
        | [], _ -> if followed then add_spaces remaining acc else acc
        | _ :: _, [] ->
          (* The layout was computed from this cell, among others. *)
          assert false
      in
      acc
      |> render_anonymous cell.first l.first
      |> aux
           ~followed:cell.first.followed
           ~remaining:(l.width - l.first)
           cell.rest
           l.rest
  ;;

  let render_blocks blocks =
    (* Each line is preceded by a newline, including the first one. *)
    let render_block acc block =
      let layout = Layout.of_block block in
      List.fold_left
        (fun acc line ->
          let acc = Newline :: acc in
          match line with
          | Plain segs -> add_segs segs acc
          | Aligned row -> render_cell row layout acc)
        acc
        block
    in
    match List.rev (List.fold_left render_block [] blocks) with
    | Newline :: items -> items
    | _ -> (* There is always at least one line. *) assert false
  ;;

  let table items = Lines.of_items items |> Lines.blocks |> render_blocks
end

(** {1 Recording} *)

type kind =
  | Table
  | Aligned_row
  | Aligned_cell of Core.alignment
  | Transparent
    (** Alignment contexts printed in flat mode (their rows are all on the same
        line, so they wouldn't be aligned anyway), rows which can't be aligned,
        and cells which are not part of an aligned row: their content is kept,
        but not their structure. *)

type context =
  { kind : kind
  ; mutable rev_items : item list
  }

type t =
  { out : Regular_buffer.t (* Written to when [alignment_context] is empty. *)
  ; alignment_context : context Stack.t
  ; mutable line_has_text : bool
  }

let create n =
  { out = Regular_buffer.create n
  ; alignment_context = Stack.create ()
  ; line_has_text = false
  }
;;

let contents t =
  assert (Stack.is_empty t.alignment_context);
  Regular_buffer.contents t.out
;;

let record context item = context.rev_items <- item :: context.rev_items

let add_spaces t n =
  if n > 0
  then (
    match Stack.top_opt t.alignment_context with
    | None -> Regular_buffer.add_spaces t.out n
    | Some
        ({ rev_items = Seg (Line_segment.Spaces m) :: rev_items; _ } as context)
      -> context.rev_items <- Seg (Line_segment.Spaces (m + n)) :: rev_items
    | Some context -> record context (Seg (Line_segment.Spaces n)))
;;

let newline t =
  t.line_has_text <- false;
  match Stack.top_opt t.alignment_context with
  | None -> Regular_buffer.newline t.out
  | Some context -> record context Newline
;;

let add_string t s =
  t.line_has_text <- true;
  match Stack.top_opt t.alignment_context with
  | None -> Regular_buffer.add_string t.out s
  | Some context -> record context (Seg (Line_segment.Text s))
;;

let push_context t kind =
  Stack.push { kind; rev_items = [] } t.alignment_context
;;

let pop t =
  let context = Stack.pop t.alignment_context in
  List.rev context.rev_items, context.kind
;;

(* Adds [items], which only contain [Seg]s and [Newline]s, to the enclosing
   context (or the output). That is the case of the rendered tables, and of the
   content of the contexts which don't keep their structure: the rows and cells
   they contain don't keep their structure either, and tables are rendered when
   they are closed. *)
let splice t items =
  match Stack.top_opt t.alignment_context with
  | None ->
    List.iter
      (function
        | Seg (Line_segment.Text s) -> add_string t s
        | Seg (Line_segment.Spaces n) -> add_spaces t n
        | Newline -> newline t
        | Row _ | Cell _ -> assert false)
      items
  | Some parent -> parent.rev_items <- List.rev_append items parent.rev_items
;;

(** {1 Alignment API} *)

let open_alignment_context t ~enabled =
  push_context t (if enabled then Table else Transparent)
;;

let close_alignment_context t =
  match pop t with
  | items, Table -> splice t (Render.table items)
  | items, Transparent -> splice t items
  | _ -> invalid_arg "Tabular_buffer.close_alignment_context"
;;

let start_alignment_row t =
  let kind =
    (* Rows starting after some text (e.g. sharing a line with another row) are
       not aligned. *)
    match t.line_has_text, Stack.top_opt t.alignment_context with
    | false, Some { kind = Table; _ } -> Aligned_row
    | _ -> Transparent
  in
  push_context t kind
;;

let end_alignment_row t =
  match pop t with
  | items, Aligned_row -> record (Stack.top t.alignment_context) (Row items)
  | items, Transparent -> splice t items
  | _ -> invalid_arg "Tabular_buffer.end_alignment_row"
;;

let open_cell t ~align =
  let kind =
    (* Cells are only aligned in aligned rows: the innermost context is enough
       to tell. *)
    match Stack.top_opt t.alignment_context with
    | Some { kind = Aligned_row | Aligned_cell _; _ } -> Aligned_cell align
    | _ -> Transparent
  in
  push_context t kind
;;

let close_cell t =
  match pop t with
  | items, Aligned_cell align ->
    record (Stack.top t.alignment_context) (Cell (align, items))
  | items, Transparent -> splice t items
  | _ -> invalid_arg "Tabular_buffer.close_cell"
;;
