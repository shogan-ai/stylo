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

  (** The width without the trailing spaces, which are not printed. *)
  let visible_width segs =
    List.fold_left
      (fun (visible, width) -> function
        | Text s ->
          let width = width + Core.strlen s in
          width, width
        | Spaces n -> visible, width + n)
      (0, 0)
      segs
    |> fst
  ;;
end

module Tree = struct
  type ('a, 'b) t =
    | Leaf of 'a * 'b list
    | Node of 'a * ('a, 'b) t list

  (** Iterates over the elements of the leaves, from left to right. *)
  let rec iter_elements f = function
    | Leaf (_, l) -> List.iter f l
    | Node (_, children) -> List.iter (iter_elements f) children
  ;;
end

module Cells : sig
  type alignment =
    | Right
    | Left of { followed : bool }

  (* The children of a node alternate between anonymous cells (the text around
     explicit cells) and explicit cells, starting and ending with an anonymous
     one (possibly empty):
     {v a0 c1 a1 ... cn an v}
     That is what makes the cells of different rows line up. *)
  type t = (alignment, Line_segment.t) Tree.t

  val of_core_alignment : Core.alignment -> alignment

  (** An anonymous cell. *)
  val anon : Line_segment.t list -> t

  (** {2 Reversed cells}

      Cells are accumulated reversed: the lists of children and segments are in
      reverse order, the head child of a node being the anonymous cell currently
      being filled. *)

  val add_seg : Line_segment.t -> t -> t
  val add_cell : t -> t -> t

  (** Cells cut by a newline: only their beginning is on the line, they can't be
      aligned to the right. *)
  val cut : t -> t

  (** [finish rev_cells] is the cells of a complete line, in order, each of them
      telling whether some text follows it on the line. The alignment of
      subdivided cells doesn't matter. *)
  val finish : t -> t
end = struct
  type alignment =
    | Right
    | Left of { followed : bool }

  type t = (alignment, Line_segment.t) Tree.t

  let of_core_alignment : Core.alignment -> alignment = function
    | Left -> Left { followed = false }
    | Right -> Right
  ;;

  let anon segs = Tree.Leaf (Left { followed = false }, segs)

  let add_seg_children seg = function
    | Tree.Leaf (align, segs) :: children ->
      Tree.Leaf (align, seg :: segs) :: children
    | _ -> assert false
  ;;

  let add_cell_children cell children = anon [] :: cell :: children

  let add_seg seg = function
    | Tree.Leaf (align, segs) -> Tree.Leaf (align, seg :: segs)
    | Node (align, children) -> Node (align, add_seg_children seg children)
  ;;

  let add_cell cell = function
    | Tree.Leaf (align, segs) ->
      Tree.Node (align, add_cell_children cell [ anon segs ])
    | Node (align, children) -> Node (align, add_cell_children cell children)
  ;;

  let cut = function
    | Tree.Leaf (_, segs) -> Tree.Leaf (Left { followed = false }, segs)
    | Node (_, children) -> Node (Left { followed = false }, children)
  ;;

  let finish rev_cells =
    let rec aux followed = function
      | Tree.Leaf (align, rev_segs) ->
        let align =
          match align with
          | Right -> Right
          | Left _ -> Left { followed }
        in
        ( Tree.Leaf (align, List.rev rev_segs)
        , followed || Line_segment.has_text rev_segs )
      | Node (_, rev_children) ->
        let children, first_followed =
          List.fold_left
            (fun (children, followed) c ->
              let c, followed = aux followed c in
              c :: children, followed)
            ([], followed)
            rev_children
        in
        Node (Left { followed }, children), first_followed
    in
    fst (aux false rev_cells)
  ;;
end

module Layout : sig
  type width =
    { when_followed : int (** used to compute required padding *)
    ; longest_content : int
         (** used when deciding whether some line goes over the max width. *)
    }

  (* The width of an internal node encompasses the width of its children: the
     [longest_content] of the root is the width of the widest line. *)
  type t = (width, unit) Tree.t

  val empty : t
  val width : t -> width
  val of_cells : Cells.t -> t

  (** as fine grained as both inputs. each node being as wide as the widest of
      the inputs corresponding nodes *)
  val merge : t -> t -> t
end = struct
  type width =
    { when_followed : int
    ; longest_content : int
    }

  type t = (width, unit) Tree.t

  let zero = { when_followed = 0; longest_content = 0 }
  let empty = Tree.Leaf (zero, [])

  let width = function
    | Tree.Leaf (w, _) | Node (w, _) -> w
  ;;

  let max_width w1 w2 =
    { when_followed = max w1.when_followed w2.when_followed
    ; longest_content = max w1.longest_content w2.longest_content
    }
  ;;

  (* [min] accounts for the rows where the node is a leaf. The content of a
     child extends from the sum of the widths of the preceding ones. *)
  let node ?(min = zero) children =
    let sum, longest =
      List.fold_left
        (fun (offset, longest) c ->
          let w = width c in
          (offset + w.when_followed, max longest (offset + w.longest_content)))
        (0, 0)
        children
    in
    Tree.Node
      ( { when_followed = max min.when_followed sum
        ; longest_content = max min.longest_content longest
        }
      , children )
  ;;

  (* Cells which are not followed by any text don't need to be padded. Unless
     they are right aligned. *)
  let rec of_cells : Cells.t -> t = function
    | Tree.Leaf (align, segs) ->
      let when_followed =
        match align with
        | Cells.Right | Left { followed = true } -> Line_segment.width segs
        | Left { followed = false } -> 0
      in
      Tree.Leaf
        ( { when_followed; longest_content = Line_segment.visible_width segs }
        , [] )
    | Node (_, children) -> node (List.map of_cells children)
  ;;

  let rec zip_with f xs ys =
    match xs, ys with
    | [], l | l, [] -> l
    | x :: xs, y :: ys -> f x y :: zip_with f xs ys
  ;;

  let rec merge l1 l2 =
    match l1, l2 with
    | Tree.Leaf (w1, _), Tree.Leaf (w2, _) -> Tree.Leaf (max_width w1 w2, [])
    | Leaf (w, _), Node (w', children) | Node (w', children), Leaf (w, _) ->
      (* The leaf spans the whole node, the children aren't affected. *)
      Node (max_width w w', children)
    | Node (w1, children1), Node (w2, children2) ->
      (* N.B. the merged children are at least as wide as the ones of [l1] and
         of [l2], so [w1] and [w2] are enough to account for the rows where the
         node is a leaf. *)
      node ~min:(max_width w1 w2) (zip_with merge children1 children2)
  ;;
end

module Line = struct
  type t =
    | First_of_row of Cells.t
    | Plain of Line_segment.t list
    | Aligned of Cells.t
      (** A [First_of_row] padded according to the layout of its block. Only
          produced by [Block.add]. *)

end

module Block : sig
  type t = Layout.t * Line.t list
  type incomplete

  val create : max_width:int -> max_consecutive_plain_lines:int -> incomplete
  val add : incomplete -> Line.t -> (incomplete, t * incomplete) Either.t
  val close : incomplete -> t

  (* [emit_nl] is called before each line. *)
  val render
    :  t
    -> emit_seg:(Line_segment.t -> unit)
    -> emit_nl:(unit -> unit)
    -> unit
end = struct
  type t = Layout.t * Line.t list

  type incomplete =
    { lines : Line.t list
    ; nb_plain_since_last_aligned : int
    ; max_width : int
    ; max_consecutives_plain : int
    ; layout : Layout.t
    }

  let create ~max_width ~max_consecutive_plain_lines =
    { lines = []
    ; nb_plain_since_last_aligned = 0
    ; max_width
    ; max_consecutives_plain = max_consecutive_plain_lines
    ; layout = Layout.empty
    }
  ;;

  let close b = b.layout, List.rev b.lines

  let add_plain b line =
    let nb_plain = b.nb_plain_since_last_aligned + 1 in
    if nb_plain > b.max_consecutives_plain
    then
      Either.Right
        ( close b
        , { b with
            lines = [ line ]
          ; nb_plain_since_last_aligned = 1
          ; layout = Layout.empty
          } )
    else
      Either.Left
        { b with
          lines = line :: b.lines
        ; nb_plain_since_last_aligned = nb_plain
        }
  ;;

  let add b (line : Line.t) =
    match line with
    | Line.Plain _ -> add_plain b line
    | Aligned _ -> invalid_arg "Tabular_buffer.Block.add"
    | First_of_row cells ->
      let layout = Layout.merge b.layout (Layout.of_cells cells) in
      (* N.B. the lines which are too wide without any padding are not aligned
         either: padding them wouldn't make them fit, but could make the other
         lines go past [max_width]. *)
      if (Layout.width layout).longest_content > b.max_width
      then add_plain b line
      else
        Either.Left
          { b with
            lines = Aligned cells :: b.lines
          ; nb_plain_since_last_aligned = 0
          ; layout
          }
  ;;

  let emit_spaces ~emit_seg n = if n > 0 then emit_seg (Line_segment.Spaces n)

  let rec render_cells ~emit_seg (cells : Cells.t) layout =
    let width = (Layout.width layout).when_followed in
    match cells, layout with
    | Tree.Leaf (Cells.Left { followed }, segs), _ ->
      List.iter emit_seg segs;
      if followed then emit_spaces ~emit_seg (width - Line_segment.width segs)
    | Leaf (Right, segs), _ ->
      emit_spaces ~emit_seg (width - Line_segment.width segs);
      List.iter emit_seg segs
    | Node _, Tree.Leaf _ ->
      (* The layout was computed from these cells, among others. *)
      assert false
    | Node (align, children), Tree.Node (_, layouts) ->
      (* If the node is followed, so are all its children, which were therefore
         padded to their widths. *)
      let rec aux children layouts used =
        match children, layouts with
        | [], _ ->
          (match align with
           | Left { followed = true } -> emit_spaces ~emit_seg (width - used)
           | _ -> ())
        | c :: children, l :: layouts ->
          render_cells ~emit_seg c l;
          aux children layouts (used + (Layout.width l).when_followed)
        | _ :: _, [] -> assert false
      in
      aux children layouts 0
  ;;

  let render (layout, lines) ~emit_seg ~emit_nl =
    List.iter
      (fun line ->
        emit_nl ();
        match (line : Line.t) with
        | Plain segs -> List.iter emit_seg segs
        | First_of_row cells -> Tree.iter_elements emit_seg cells
        | Aligned cells -> render_cells ~emit_seg cells layout)
      lines
  ;;
end

(* Maximum number of lines between two aligned lines (e.g. comments, or the
   following lines of a multi-line row) for them to still be part of the same
   block. *)
let max_consecutive_plain_lines = 2

(* The rendered lines of a block are written to [parent]. *)
type block =
  { mutable b : Block.incomplete
  ; mutable current_line : Line.t
       (** Reversed: [Plain] segments, or the [First_of_row] cells of a row
           which ended on this line (cf. {!Cells}), the rest of the line going
           into their last anonymous cell. Never [Aligned]. *)
  ; mutable started : bool (** Some line was written to [parent]. *)
  ; parent : destination
  }
and destination =
  | Block of block
  | Buffer
  | Cells of cells
    (** The (anonymous) cell being filled, in the first line of an aligned row.
        *)

(* Reversed, cf. {!Cells}. *)
and cells = { mutable rev_cells : Cells.t }

(* The first line of an aligned row, and its explicit cells (the cells opened
   directly in the row or in another explicit cell) are [Accumulating]. The
   other frames are [Passthrough]: in the first line of an aligned row, they are
   not aligned (no other row can start on that line), only their content is
   kept. *)
type alignment_frame =
  | Accumulating of cells
  | Passthrough of destination

type alignement_context =
  | Row of alignment_frame
  | Cell of alignment_frame
  | Table of int option * destination
    (** The maximal width of the lines of enabled tables. Enabled tables only
        have a block when they are not in the first line of an aligned row. *)

type t =
  { output : Regular_buffer.t
  ; context : alignement_context Stack.t
  ; mutable line_has_text : bool
  }

let create n =
  { output = Regular_buffer.create n
  ; context = Stack.create ()
  ; line_has_text = false
  }
;;

let contents t =
  assert (Stack.is_empty t.context);
  Regular_buffer.contents t.output
;;

(** {1 Destinations} *)

let new_block ~parent ~width =
  { b = Block.create ~max_width:width ~max_consecutive_plain_lines
  ; current_line = Line.Plain []
  ; started = false
  ; parent
  }
;;

let add_segment_to t dest seg =
  match dest, (seg : Line_segment.t) with
  | Buffer, Text s -> Regular_buffer.add_string t.output s
  | Buffer, Spaces n -> Regular_buffer.add_spaces t.output n
  | Block blk, _ ->
    blk.current_line
    <- (match blk.current_line with
        | Plain segs -> Plain (seg :: segs)
        | First_of_row cells -> First_of_row (Cells.add_seg seg cells)
        | Aligned _ -> assert false)
  | Cells c, _ -> c.rev_cells <- Cells.add_seg seg c.rev_cells
;;

let rec newline_to t = function
  | Buffer -> Regular_buffer.newline t.output
  | Cells _ ->
    (* Blocks are never written to the first line of an aligned row. *)
    assert false
  | Block blk ->
    let line : Line.t =
      match blk.current_line with
      | Plain rev_segs -> Plain (List.rev rev_segs)
      | First_of_row rev_cells -> First_of_row (Cells.finish rev_cells)
      | Aligned _ -> assert false
    in
    blk.current_line <- Plain [];
    add_line t blk line

and add_line t blk line =
  match Block.add blk.b line with
  | Either.Left b -> blk.b <- b
  | Either.Right (block, b) ->
    blk.b <- b;
    render t blk block

and render t blk block =
  Block.render block ~emit_seg:(add_segment_to t blk.parent) ~emit_nl:(fun () ->
    if blk.started then newline_to t blk.parent else blk.started <- true)
;;

(* Adds the current line (which isn't followed by a newline) to the block, and
   renders it. *)
let flush t blk =
  newline_to t (Block blk);
  render t blk (Block.close blk.b)
;;

(** {1 Frames} *)

let destination t =
  match Stack.top_opt t.context with
  | None -> Buffer
  | Some (Row (Passthrough d) | Cell (Passthrough d) | Table (_, d)) -> d
  | Some (Row (Accumulating c) | Cell (Accumulating c)) -> Cells c
;;

(* A newline in the first line of an aligned row: the explicit cells are folded
   into the row, which is then added to its table, and the frames become
   [Passthrough] the destination of the row. *)
let cut t =
  let rec pop_frames popped =
    let frame = Stack.pop t.context in
    match frame, Stack.top_opt t.context with
    | Row (Accumulating c), Some (Table (_, Block blk)) ->
      blk.current_line <- First_of_row c.rev_cells;
      Stack.push (Row (Passthrough (Block blk))) t.context;
      List.fold_left
        (fun dest frame ->
          let frame, dest =
            match frame with
            | Row _ -> Row (Passthrough dest), dest
            | Cell _ -> Cell (Passthrough dest), dest
            | Table ((None as width), _) -> Table (width, dest), dest
            | Table ((Some w as width), _) ->
              let dest = Block (new_block ~parent:dest ~width:w) in
              Table (width, dest), dest
          in
          Stack.push frame t.context;
          dest)
        (Block blk)
        popped
      |> ignore
    | ( Cell (Accumulating c)
      , Some (Row (Accumulating parent) | Cell (Accumulating parent)) ) ->
      parent.rev_cells
      <- Cells.add_cell (Cells.cut c.rev_cells) parent.rev_cells;
      pop_frames (frame :: popped)
    | (Row (Passthrough _) | Cell (Passthrough _) | Table _), _ ->
      pop_frames (frame :: popped)
    | (Row (Accumulating _) | Cell (Accumulating _)), _ -> assert false
  in
  pop_frames []
;;

(** {1 Printing buffer API} *)

let add_spaces t n =
  if n > 0 then add_segment_to t (destination t) (Line_segment.Spaces n)
;;

let add_string t s =
  t.line_has_text <- true;
  add_segment_to t (destination t) (Line_segment.Text s)
;;

let newline t =
  t.line_has_text <- false;
  match destination t with
  | Cells _ ->
    cut t;
    newline_to t (destination t)
  | dest -> newline_to t dest
;;

let open_alignment_context t ~enabled ~width =
  let dest = destination t in
  let table =
    match dest, enabled with
    | (Block _ | Buffer), true ->
      Table (Some width, Block (new_block ~parent:dest ~width))
    | Cells _, true -> Table (Some width, dest)
    | _, false -> Table (None, dest)
  in
  Stack.push table t.context
;;

let close_alignment_context t =
  match Stack.pop t.context with
  | Table (Some _, Block blk) -> flush t blk
  | Table (Some _, Cells _) | Table (None, _) -> ()
  | Table (Some _, Buffer) | Row _ | Cell _ ->
    invalid_arg "Tabular_buffer.close_alignment_context"
;;

let start_alignment_row t =
  let frame =
    match Stack.top_opt t.context with
    | Some (Table (Some _, Block ({ current_line = Plain segs; _ } as blk)))
      when not t.line_has_text ->
      (* Rows starting after some text (e.g. sharing a line with another row)
         are not aligned. The spaces preceding the row are part of its first
         cell. *)
      blk.current_line <- Plain [];
      Accumulating { rev_cells = Cells.anon segs }
    | _ -> Passthrough (destination t)
  in
  Stack.push (Row frame) t.context
;;

let end_alignment_row t =
  match Stack.pop t.context with
  | Row (Passthrough _) -> ()
  | Row (Accumulating c) ->
    (match Stack.top t.context with
     | Table (_, Block blk) -> blk.current_line <- First_of_row c.rev_cells
     | _ -> assert false)
  | Table _ | Cell _ -> invalid_arg "Tabular_buffer.end_alignment_row"
;;

let open_cell t ~align =
  let frame =
    match Stack.top_opt t.context with
    | Some (Row (Accumulating _) | Cell (Accumulating _)) ->
      Accumulating { rev_cells = Tree.Leaf (Cells.of_core_alignment align, []) }
    | _ -> Passthrough (destination t)
  in
  Stack.push (Cell frame) t.context
;;

let close_cell t =
  match Stack.pop t.context with
  | Cell (Passthrough _) -> ()
  | Cell (Accumulating c) ->
    (match Stack.top t.context with
     | Row (Accumulating parent) | Cell (Accumulating parent) ->
       parent.rev_cells <- Cells.add_cell c.rev_cells parent.rev_cells
     | _ -> assert false)
  | Table _ | Row _ -> invalid_arg "Tabular_buffer.close_cell"
;;
