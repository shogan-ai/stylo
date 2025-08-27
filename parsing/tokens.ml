
type token = Parser_tokens.token

type elt =
  | Token of token
  | Comment of string
  | Child_node

type seq = elt list

module Indexed_list : sig
  type t

  val create : unit -> t
  val append : t -> pos:Lexing.position -> elt -> unit
  val consume : t -> Lexing.position -> Lexing.position -> seq

  val global : t
  val reset_global : unit -> unit
end = struct
  open Lexing

  type cell =
    | Empty
    | Node of {
        pos: position;
        mutable value: elt;
        mutable next: cell;
      }

  module Tbl =
    Hashtbl.Make(struct
      type t = position
      let equal t1 t2 = Int.equal t1.pos_cnum t2.pos_cnum
      let hash t = Hashtbl.hash t.pos_cnum
    end)

  type t = {
    tbl: cell Tbl.t;
    mutable last: cell;
  }
  let create () : t = { tbl = Tbl.create 42; last = Empty }

  let append t ~pos elt =
    let node = Node { pos; value = elt; next = Empty } in
    Tbl.add t.tbl pos node;
    begin match t.last with
    | Empty -> ()
    | Node n -> n.next <- node
    end;
    t.last <- node

  let consume t start stop =
    let rec aux = function
      | Empty -> invalid_arg "Tokens.consume"
      | Node n as curr ->
        if n.pos >= stop then (
          (* Stop is an endpos, and we index by startpos *)
          [], curr
        ) else (
          Tbl.remove t.tbl n.pos;
          let seq, after = aux n.next in
          n.value :: seq, after
        )
    in
    match Tbl.find t.tbl start with
    | Empty -> invalid_arg "Tokens.consume"
    | Node start_node as cell->
      let seq, after = aux cell in
      start_node.value <- Child_node;
      start_node.next <- after;
      Tbl.replace t.tbl start cell; (* has been removed by aux... *)
      Tbl.replace t.tbl stop cell;
      seq

  let global = create ()
  let reset_global () =
    Tbl.clear global.tbl;
    global.last <- Empty
end

let at (startpos,endpos) =
  Indexed_list.(consume global startpos endpos)
