
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
end

type tree_node =
  | Tok of token
  | Cmt of string
  | Inlined of consumable ref
  | Non_terminal of consumable ref

and consumable = Consumed | Available of tree

and tree = tree_node list

let rec flatten : tree -> seq = function
  | [] -> []
  | Tok t :: rest -> Token t :: flatten rest
  | Cmt s :: rest -> Comment s :: flatten rest
  | Inlined { contents = Consumed } :: rest -> Child_node :: flatten rest
  | Inlined { contents = Available subtree } :: rest ->
    flatten subtree @ flatten rest
  | Non_terminal { contents = Consumed } :: rest -> Child_node :: flatten rest
  | Non_terminal { contents = Available subtree } :: rest ->
    flatten subtree @ flatten rest

let of_production_ref : (unit -> seq) ref =
  ref (fun () -> failwith "Initialization error")

let of_production () = !of_production_ref ()

let between_ref : (start:Lexing.position -> stop:Lexing.position -> seq) ref =
  ref (fun ~start:_ ~stop:_ -> failwith "Initialization error")

let at (startpos,endpos) = !between_ref ~start:startpos ~stop:endpos

open Format

let rec pp_tree ppf = function
  | Consumed -> pp_print_string ppf "<Consumed>"
  | Available lst ->
    let pp_lst =
      pp_print_list
        ~pp_sep:(fun ppf () -> fprintf ppf ",@ ")
        pp_tree_node
    in
    fprintf ppf "{@[<hov 2>@ %a" pp_lst lst;
    fprintf ppf "@]@,}"

and pp_tree_node ppf = function
  | Tok _ -> pp_print_string ppf "Token"
  | Cmt _ -> pp_print_string ppf "Comment"
  | Inlined c_ref -> pp_tree ppf !c_ref
  | Non_terminal c_ref -> pp_print_string ppf "*"; pp_tree ppf !c_ref
