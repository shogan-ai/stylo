open Ocaml_syntax

let column (pos : Lexing.position) = pos.pos_cnum - pos.pos_bol

let compare_pos p1 p2 =
  match compare p1.Lexing.pos_lnum p2.Lexing.pos_lnum with
  | 0 -> compare (column p1) (column p2)
  | n -> n

module Ordering = struct
  type error =
    | Incomplete_flattening
    | Reordered of Tokens.elt * Tokens.elt

  let pp_error (pos : Lexing.position) = function
    | Incomplete_flattening ->
      Format.eprintf
        "@[<hov>@[<h>File %s, line %d,@ column %d:@]@ \
         Incomplete retokenisation of CST.@."
        pos.pos_fname pos.pos_lnum (column pos)
    | Reordered (t1, t2) ->
      Format.eprintf
        "@[<hov>@[<h>File %s, line %d,@ column %d:@]@ \
         Tokens %a and %a have been swapped.@."
        pos.pos_fname pos.pos_lnum (column pos)
        Tokens.pp_elt t1
        Tokens.pp_elt t2

  exception Error of error * Lexing.position

  let rec ensure_preserved : Tokens.seq -> unit = function
    | []
    | [ _ ] -> ()
    | { desc = Child_node; pos } :: _ ->
      raise (Error (Incomplete_flattening, pos))
    | curr :: (next :: _ as remaining_tokens) ->
      if compare_pos curr.pos next.pos <= 0
      then ensure_preserved remaining_tokens
      else raise (Error (Reordered (curr, next), next.pos))

end

module Comments_comparison = struct
  exception Error

  (* TODO: improve reporting *)
  let report () =
    Format.eprintf "Some comments were dropped during normalization"

  let same_number before after =
    let incr_if_cmt nb t = if Tokens.is_comment t then nb + 1 else nb in
    let nb_before = List.fold_left incr_if_cmt 0 before in
    let nb_after = List.fold_left incr_if_cmt 0 after in
    if nb_before <> nb_after then
      raise Error
end
