
type token = Parser_tokens.token

type elt =
  | Token of token
  | Comment of string
  | Child_node

type tree_node =
  | Tok of token
  | Cmt of string
  | Inlined of consumable ref
  | Non_terminal of consumable ref

and consumable = Consumed | Available of tree

and tree = tree_node list

type seq = elt list

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
