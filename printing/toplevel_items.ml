open Document
open Document.Utils

open Ocaml_syntax.Tokens


let add_item doc item =
  match doc with
  | Empty -> group (item ^^ break 0)
  | _ ->
    (* flat items don't force blank lines, only non-flat ones do. *)
    (* Notice the [nest] trick here: without it any comment "attached" to the
       item would be inserted outside the group (because we don't want to
       unflatten things).
       As a result, in situations such as
       {[
         flat_item

         (* cmt *)
         non_flat_item
       ]}
       the following would happen:
       1. [flat_item] is printed (and is flat)
       2. the [break 0] that follows vanishes
       3. [hardline] is printed
       4. the comment is inserted (with no extra spacing as it is between two
       spaces/breaks already)
       5. a hardline is inserted because of [blank_line]
       That is: we have no blank line, and the item the comment refers to
       becomes unclear.

       The nest allows the comment insertion code to traverse the group,
       delaying the comment until after the [nest 0 blankline], and producing
       the output we expect. *)
    doc ^^ hardline ^^ group (nest 0 blank_line ^^ item ^^ break 0)

let rec advance_tokens = function
  | [] -> false, []
  | tok :: tokens ->
    match tok.desc with
    | Child_node
    | Token EOF -> false, tokens
    | Token SEMISEMI -> true, tokens
    | Token _ -> assert false
    | Comment _ -> advance_tokens tokens

(* We keep the list of items in sync with the list of "tokens" of the
   structure (each [Child_node] is a structure item).
   That tells us where to insert [;;]. *)
let pp_keeping_semi pp_item (items, tokens) =
  let rec perhaps_semi doc items tokens =
    let semi_first, tokens = advance_tokens tokens in
    if semi_first
    then perhaps_semi (doc ^?^ Syntax.semisemi) items tokens
    else expect_item doc items tokens
  and expect_item doc items tokens =
    match items, tokens with
    | [], [] -> doc
    | item :: items, tokens ->
      let item = pp_item item in
      let semi, tokens = advance_tokens tokens in
      if semi
      then perhaps_semi (add_item doc @@ item ^/^ Syntax.semisemi) items tokens
      else expect_item (add_item doc item) items tokens
    | _ -> assert false
  in
  perhaps_semi empty items tokens
