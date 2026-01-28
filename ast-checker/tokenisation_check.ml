open Ocaml_syntax

let column (pos : Lexing.position) = pos.pos_cnum - pos.pos_bol

let compare_pos p1 p2 =
  match compare p1.Lexing.pos_lnum p2.Lexing.pos_lnum with
  | 0 -> compare (column p1) (column p2)
  | n -> n

let rec ensure_order_is_preserved : Tokens.seq -> unit = function
  | []
  | [ _ ] -> ()
  | { desc = Child_node; pos } :: _ ->
    Format.eprintf
      "@[<hov>@[<h>File %s, line %d,@ column %d:@]@ \
       Incomplete retokenisation of CST.@."
      pos.pos_fname pos.pos_lnum (column pos);
    exit 1
  | curr :: (next :: _ as remaining_tokens) ->
    if compare_pos curr.pos next.pos <= 0
    then ensure_order_is_preserved remaining_tokens
    else (
      Format.eprintf
        "@[<hov>@[<h>File %s, line %d,@ column %d:@]@ \
         Tokens %a and %a have been swapped.@."
        next.pos.pos_fname next.pos.pos_lnum (column next.pos)
        Tokens.pp_elt curr
        Tokens.pp_elt next;
      exit 1
    )
