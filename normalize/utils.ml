open Ocaml_syntax
open Tokens

let is_comment tok =
  match tok.desc with
  | Comment _ -> true
  | _ -> false

(** splits the list before the first element on which the predicate returns
    false. *)
let list_split_at p l =
  let rec aux = function
    | x::l when p x ->
      let taken, dropped = aux l in
      x::taken, dropped
    | rest -> [], rest
  in
  aux l

let split ~on:target =
  list_split_at (fun tok ->
    match tok.desc with
    | Token t
    | Opt_token t when Raw.equals t target -> false
    | _ -> true
  )

let split_on_child =
  list_split_at (fun tok ->
    match tok.desc with
    | Child_node -> false
    | _ -> true
  )

let search_and_replace pairs =
  let replace rt =
    try List.assoc rt pairs
    with Not_found -> rt
  in
  List.map (fun tok ->
    match tok.desc with
    | Token t -> { tok with desc = Token (replace t) }
    | Opt_token t -> { tok with desc = Opt_token (replace t) }
    | Comment _ | Child_node -> tok
  )
