open Document
open Document.Utils

open Ocaml_syntax.Tokens

let add_item ?flatness last_in_group doc item =
  let post_break = if last_in_group then empty else break 0 in
  match doc with
  | Empty -> group ?flatness (item ^^ post_break)
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
       2. the [break 0] that follows it vanishes
       3. [softline] adds a \n
       4. the comment is inserted (with no extra spacing as it is between two
       spaces/breaks already)
       5. a \n is inserted because of [soft_break 0] (since item is not flat)

       As a result we have no blank line, and the item to which the comment
       refers becomes unclear.

       The nest allows the comment insertion code to traverse the group,
       delaying the comment until after the [nest soft_break], and producing
       the output we expect.

       N.B. there is an extra level of trickery: [nest n doc] is a smart
       constructor that just returns [doc] if [n = 0] (and there's no
       [extra_indent]).
       Here we want to force the insertion of a [Nest] node, without changing
       the indentation of [doc], ... so we additionally give an [~extra_indent],
       also set to 0. *)
    doc ^^ softline ^^
    group ?flatness (
      nest 1 (nest (-1) (soft_break 0)) ^^
      item ^^ post_break
    )

type cont =
  | Semi_followed_by of seq
  | Opt_semi_followed_by of seq
  | Child_followed_by of seq
  | Done

let opt_semisemi_doc vanishing_level =
  Document.opt_token ~ws_before:(Line_break Softest) vanishing_level ";;"

let rec advance_tokens = function
  | [] -> Done
  | tok :: tokens ->
    match tok.desc with
    | Child_node -> Child_followed_by tokens
    | Token EOF -> assert (tokens = []); Done
    | Token SEMISEMI -> Semi_followed_by tokens
    | Opt_token SEMISEMI -> Opt_semi_followed_by tokens
    | Opt_token _ | Token _ -> assert false
    | Comment _ -> advance_tokens tokens

(* We keep the list of items in sync with the list of "tokens" of the
   structure (each [Child_node] is an item).
   That tells us where to insert [;;]. *)
let pp_keeping_semi pp_item =
  let rec perhaps_semi doc items tokens =
    match advance_tokens tokens with
    | Semi_followed_by tokens ->
      perhaps_semi (doc ^?^ Syntax.semisemi) items tokens
    | Opt_semi_followed_by tokens ->
      (* This one is optional, and will always disappear since we are not
         actually tracking the flatness of any group.
         Pretty sure this branch is dead code, and we could [assert false] here,
         but why bother? *)
      perhaps_semi (doc ^^ opt_semisemi_doc Condition.always) items tokens
    | Child_followed_by _ as cont ->
      expect_item doc items cont
    | Done -> doc, Done
  and expect_item doc items cont =
    match items, cont with
    | [], _ -> doc, cont
    | item :: items, Child_followed_by tokens ->
      let item = pp_item item in
      let last = items = [] in
      begin match advance_tokens tokens with
      | Semi_followed_by tokens ->
        let item = item ^/^ Syntax.semisemi in
        perhaps_semi (add_item last doc item) items tokens
      | Opt_semi_followed_by tokens ->
        let flatness = flatness_tracker () in
        let item = item ^^ opt_semisemi_doc (Condition.flat flatness) in
        perhaps_semi (add_item ~flatness last doc item) items tokens
      | cont ->
        expect_item (add_item last doc item) items cont
      end
    | _ -> assert false
  in
  perhaps_semi

let pp_grouped_keeping_semi pp_item groups tokens =
  let tokens_of_cont =
    (* Meh. *)
    function
    | Done -> []
    | Semi_followed_by tokens ->
      { desc = Token SEMISEMI; pos = Lexing.dummy_pos } :: tokens
    | Opt_semi_followed_by tokens ->
      { desc = Opt_token SEMISEMI; pos = Lexing.dummy_pos } :: tokens
    | Child_followed_by tokens ->
      { desc = Child_node; pos = Lexing.dummy_pos } :: tokens
  in
  match
    List.fold_left_map (fun tokens group ->
      let doc, cont = pp_keeping_semi pp_item empty group tokens in
      tokens_of_cont cont, doc
    ) tokens groups
  with
  | [], groups -> separate (softline ^^ softline) groups
  | tokens, [] ->
    (* Here we have an empty struct/sig with potential semis.
       This code could be simplified for the special case, but meh. *)
    begin match pp_keeping_semi Fun.id empty [] tokens with
    | doc, Done -> doc
    | _ -> assert false
    end
  | _ -> assert false

let rec group_by_desc same_group acc = function
  | [] -> [ List.rev acc ]
  | i :: is ->
    if same_group i (List.hd acc) then
      group_by_desc same_group (i :: acc) is
    else
      List.rev acc :: group_by_desc same_group [ i ] is

module Struct = struct
  open Ocaml_syntax.Parsetree

  let same_group d1 d2 =
    match d1.pstr_desc, d2.pstr_desc with
    | Pstr_value _, Pstr_value _
    | Pstr_primitive _, Pstr_primitive _
    | Pstr_type _, Pstr_type _
    | Pstr_typext _, Pstr_typext _
    | Pstr_exception _, Pstr_exception _
    | Pstr_module _, Pstr_module _
    | Pstr_recmodule _, Pstr_recmodule _
    | Pstr_modtype _, Pstr_modtype _
    | Pstr_open _, Pstr_open _
    | Pstr_class _, Pstr_class _
    | Pstr_class_type _, Pstr_class_type _
    | Pstr_include _, Pstr_include _
    | Pstr_extension _, Pstr_extension _
    | Pstr_kind_abbrev _, Pstr_kind_abbrev _
    | Pstr_docstring _, Pstr_docstring _
      ->
      true
    | Pstr_attribute _, Pstr_attribute _ ->
      (* FIXME: don't group docstrings with regular attributes *)
      true
    | (* Keeping this match non-fragile to better track language updates. *)
      ( Pstr_eval _ (* never grouping those *)
      | Pstr_value _
      | Pstr_primitive _
      | Pstr_type _
      | Pstr_typext _
      | Pstr_exception _
      | Pstr_module _
      | Pstr_recmodule _
      | Pstr_modtype _
      | Pstr_open _
      | Pstr_class _
      | Pstr_class_type _
      | Pstr_include _
      | Pstr_attribute _
      | Pstr_extension _
      | Pstr_kind_abbrev _
      | Pstr_docstring _), _ ->
      false

  let group_by_desc = function
    | [] -> []
    | item :: items -> group_by_desc same_group [ item ] items

  let pp_grouped_keeping_semi pp_item (items, tokens) =
    let groups = group_by_desc items in
    pp_grouped_keeping_semi pp_item groups tokens
end

let pp_keeping_semi pp (items, tokens) =
  match pp_keeping_semi pp empty items tokens with
  | t, Done -> t
  | _ -> assert false
