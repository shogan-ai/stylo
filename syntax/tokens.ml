
type token = Parser_tokens.token

type elt =
  | Token of token
  | Comment of string
  | Child_node

type seq = elt list

let of_production_ref : (unit -> seq) ref =
  ref (fun () -> failwith "Initialization error")

let of_production () = !of_production_ref ()
