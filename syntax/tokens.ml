
type token = Parser_tokens.token

type elt =
  | Token of token
  | Comment of string
  | Child_node

type tree_node =
  | Tok of token
  | Cmt of string
  | Inlined of consumable ref

and consumable = Consumed | Available of tree

and tree = tree_node list

type seq = elt list

let of_production_ref : (unit -> seq) ref =
  ref (fun () -> failwith "Initialization error")

let of_production () = !of_production_ref ()
