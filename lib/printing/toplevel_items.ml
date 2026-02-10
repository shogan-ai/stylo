open Document
open Document.Utils

open Ocaml_syntax.Tokens

(* All toplevel flush hints "allow" floating comments, but none want to pull
   comments attached before them. *)
let flush_hint ~ws_before ~ws_after =
  flush_comments ~pull_preceeding_comments:false ~floating_allowed:true
    ~ws_before ~ws_after

let add_item ?flatness last_in_group doc item =
  let post_break = if last_in_group then empty else break 0 in
  match doc with
  | Empty ->
    let _, hint = flush_hint ~ws_before:empty ~ws_after:softline in
    hint ^^
    group ?flatness (item ^^ post_break)
  | _ ->
    let comments_inserted, hint =
      flush_hint ~ws_before:softline ~ws_after:softline
    in
    doc ^^ softline ^^
    hint ^^
    group ?flatness (
      (* flat items don't force blank lines, only non-flat ones do. *)
      vanishing_whitespace comments_inserted (soft_break 0) ^^
      item ^^ post_break
    )

type cont =
  | Semi_followed_by of bool * seq
  | Child_followed_by of seq
  | Done

let opt_semisemi_doc vanishing_level =
  Document.opt_token ~ws_before:softest_line vanishing_level ";;"

let rec advance_tokens = function
  | [] -> Done
  | tok :: tokens ->
    match tok.desc with
    | Child_node -> Child_followed_by tokens
    | Token (EOF, _) -> assert (tokens = []); Done
    | Token (SEMISEMI, optional) -> Semi_followed_by (optional, tokens)
    | Token _ -> assert false
    | Comment _
    | Lexer_directive _ -> advance_tokens tokens

(* We keep the list of items in sync with the list of "tokens" of the
   structure (each [Child_node] is an item).
   That tells us where to insert [;;]. *)
let pp_keeping_semi pp_item =
  let rec perhaps_semi doc items tokens =
    match advance_tokens tokens with
    | Semi_followed_by (false, tokens) ->
      perhaps_semi (doc ^?^ Syntax.semisemi) items tokens
    | Semi_followed_by (true, tokens) ->
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
      | Semi_followed_by (false, tokens) ->
        let item = item ^/^ Syntax.semisemi in
        perhaps_semi (add_item last doc item) items tokens
      | Semi_followed_by (true, tokens) ->
        let flatness = flatness_tracker () in
        let item = item ^^ opt_semisemi_doc (Condition.flat flatness) in
        perhaps_semi (add_item ~flatness last doc item) items tokens
      | cont ->
        expect_item (add_item last doc item) items cont
      end
    | _ -> assert false
  in
  perhaps_semi

let rec separate_groups = function
  | [] -> empty
  | [ group ] -> group
  | g :: gs ->
    let _, hint = flush_hint ~ws_before:softline ~ws_after:softline in
    (* We want a single blank line between groups. *)
    g ^^ softline ^^ softline ^^ hint ^^ separate_groups gs

let pp_grouped_keeping_semi pp_item groups tokens =
  let tokens_of_cont =
    (* Meh. *)
    function
    | Done -> []
    | Semi_followed_by (optional, tokens) ->
      { desc = Token (SEMISEMI, optional); pos = Lexing.dummy_pos } :: tokens
    | Child_followed_by tokens ->
      { desc = Child_node; pos = Lexing.dummy_pos } :: tokens
  in
  match
    List.fold_left_map (fun tokens group ->
      let doc, cont = pp_keeping_semi pp_item empty group tokens in
      tokens_of_cont cont, doc
    ) tokens groups
  with
  | [], groups -> separate_groups groups
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
    | (Pstr_module _
      | Pstr_recmodule _
      | Pstr_open _
      | Pstr_include _),
      (Pstr_module _
      | Pstr_recmodule _
      | Pstr_open _
      | Pstr_include _)
    | Pstr_modtype _, Pstr_modtype _
    | Pstr_class _, Pstr_class _
    | Pstr_class_type _, Pstr_class_type _
    | Pstr_extension _, Pstr_extension _
    | Pstr_kind_abbrev _, Pstr_kind_abbrev _
    | Pstr_docstring _, Pstr_docstring _
    | Pstr_attribute _, Pstr_attribute _ ->
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

module Sig = struct
  open Ocaml_syntax.Parsetree

  let same_group d1 d2 =
    match d1.psig_desc, d2.psig_desc with
    | Psig_value _, Psig_value _
    | (Psig_type _
      | Psig_typesubst _),
      (Psig_type _
      | Psig_typesubst _)
    | Psig_typext _, Psig_typext _
    | Psig_exception _, Psig_exception _
    | (Psig_module _
      | Psig_modsubst _
      | Psig_recmodule _
      | Psig_open _
      | Psig_include _),
      (Psig_module _
      | Psig_modsubst _
      | Psig_recmodule _
      | Psig_open _
      | Psig_include _)
    | (Psig_modtype _
      | Psig_modtypesubst _),
      (Psig_modtype _
      | Psig_modtypesubst _)
    | Psig_class _, Psig_class _
    | Psig_class_type _, Psig_class_type _
    | Psig_extension _, Psig_extension _
    | Psig_kind_abbrev _, Psig_kind_abbrev _
    | Psig_docstring _, Psig_docstring _
    | Psig_attribute _, Psig_attribute _ ->
      true
    | (* Keeping this match non-fragile to better track language updates. *)
      ( Psig_value _
      | Psig_type _
      | Psig_typesubst _
      | Psig_typext _
      | Psig_exception _
      | Psig_module _
      | Psig_modsubst _
      | Psig_recmodule _
      | Psig_modtype _
      | Psig_modtypesubst _
      | Psig_open _
      | Psig_class _
      | Psig_class_type _
      | Psig_include _
      | Psig_attribute _
      | Psig_extension _
      | Psig_kind_abbrev _
      | Psig_docstring _), _ ->
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
