open Ocaml_syntax
open Tokens

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

let rec list_map_last ~f = function
  | [] -> []
  | [ x ] -> [ f x ]
  | x :: xs -> x :: list_map_last ~f xs


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

(** removes all occurences of [token] from the given list of tokens *)
let without ~token =
  List.filter (fun t ->
    match t.desc with
    | Token rt | Opt_token rt -> not (Raw.equals rt token)
    | _ -> true
  )
