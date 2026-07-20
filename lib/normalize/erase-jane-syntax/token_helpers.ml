open Ocaml_syntax

let without_child ?at:pos flattened_child_tokens tokens =
  match Tokens.Seq.split_on_child ?pos tokens with
  | _, [] ->
    (* We expected to find a Child_node at the given position, otherwise where
       did we get [flattened_child_tokens] from? *)
    assert false
  | before, _child :: after ->
    let cmts_of_child = List.filter Tokens.is_comment flattened_child_tokens in
    before @ cmts_of_child @ after

module Synced_progress : sig
  (** Synced traversal of a list of CST nodes end the corresponding tokens, so
      individual items can be removed from the list.

      This can be used when removing signature/structure items for instance. *)

  val filter
    :  drop:('a -> Tokens.seq option)
    -> 'a list
    -> Tokens.seq
    -> 'a list * Tokens.seq

end = struct

  (* not using Tokens.Seq.split_on_child as it's more efficient to accumulate
     tokens in reverse order and call reverse once at the end. *)
  let next_child =
    let rec aux acc lst =
      match lst with
      | [] -> acc, []
      | x :: _ when Tokens.is_child x -> acc, lst
      | x :: xs -> aux (x :: acc) xs
    in
    aux []

  let filter ~drop items tokens =
    let rec aux rev_items_prefix rev_tokens_prefix tokens items =
      match items, tokens with
      | _ :: _, [] -> assert false
      | [], _ ->
        List.rev rev_items_prefix, List.rev_append rev_tokens_prefix tokens
      | item :: items, curr_child :: following_tokens ->
        let rev_before_next, tail = next_child following_tokens in
        begin match drop item with
        | Some cmt_tokens_of_removed_subtree ->
          let rev_before_next, tail = next_child following_tokens in
          let rev_prefix =
            rev_tokens_prefix
            |> List.rev_append cmt_tokens_of_removed_subtree
            |> List.append rev_before_next
          in
          aux rev_items_prefix rev_prefix tail items
        | None ->
          aux (item :: rev_items_prefix)
            (rev_before_next @ curr_child :: rev_tokens_prefix)
            tail items
        end
    in
    let rev_tokens_prefix, tokens_from_first_item = next_child tokens in
    aux [] rev_tokens_prefix tokens_from_first_item items
end

module Attributes : sig
  open Parsetree

  val mk_empty_payload
    : attr_loc:Location.t
    -> name_loc:Location.t
    -> string * Tokens.seq
    -> attribute

  val add : Tokens.seq -> attributes -> attributes -> Tokens.seq * attributes
  val merge : Tokens.seq -> attributes -> attributes -> Tokens.seq * attributes

end = struct
  let synthesize_tokens ~(loc : Location.t) attr_name_tokens =
    let open Tokens in
    { desc = Token (LBRACKETATAT, false); pos = loc.loc_start } ::
    attr_name_tokens @
    { desc = Child_node (* empty payload *); pos = loc.loc_end } ::
    { desc = Token (RBRACKET, false); pos = loc.loc_end } :: []

  open Parsetree

  let mk_empty_payload ~attr_loc ~name_loc (name, name_tokens) =
    let empty_payload : structure =
      { pst_items = []; pst_loc = name_loc; pst_tokens = [] }
    in
    let tokens = synthesize_tokens ~loc:name_loc name_tokens in
    Ast_helper.Attr.mk ~loc:attr_loc ~tokens
      (Location.mkloc [name] name_loc) (PStr empty_payload)

  let add parent_tokens parent_attrs attrs =
    match parent_attrs, attrs with
    | Attributes _, _
    | _, No_attributes -> assert false
    | No_attributes, Attributes a ->
      (* Parent didn't have any attributes, we need to add a Child_node. *)
      parent_tokens @ [{ Tokens.desc = Child_node; pos = a.loc.loc_start }],
      Attributes a

  let merge parent_tokens attrs1 attrs2 =
    match attrs1, attrs2 with
    | No_attributes, attrs
    | attrs, No_attributes ->
      parent_tokens, attrs
    | Attributes a1, Attributes a2 ->
      (* We're taking two subtrees and merging them into one, so we need to
         remove one Child_node from the parent's tokens. *)
      let tokens =
        let rev_toks = List.rev parent_tokens in
        let rev_tail, rev_head = Tokens.Seq.split_on_child rev_toks in
        let rev_head_no_child = List.tl rev_head in
        List.rev (rev_tail @ rev_head_no_child)
      in
      let attrs =
        let attributes = a1.attributes @ a2.attributes in
        let loc = { a1.loc with loc_end = a2.loc.loc_end } in
        let tokens = a1.tokens @ a2.tokens in
        Attributes { attributes; loc; tokens }
      in
      tokens, attrs
end


module Unboxed = struct
  (** Patterns and expressions are lexically the same for these constructions,
      so we share the token stream rewriters. *)

  (* A single CST node [P..._unboxed_tuple _] is transformed into two nested
      CST nodes: [P...parens (P..._tuple _)].

      So here we transform the single tokens sequence
      {v
        ..x1.. #( ..x2.. ) ..x3..
      v}
      into two:
      - {v ..x1.. ( Child_node ) ..x3.. v}
      - {v ..x2.. v}
   *)
  let tuple_tokens ~loc tokens =
    match Tokens.Seq.split ~on:HASHLPAREN tokens with
    | before_hlp, hlp :: after_hlp ->
      let inner_tokens, lp_and_after = Tokens.Seq.split ~on:RPAREN after_hlp in
      let outer_tokens =
          before_hlp @ { hlp with desc = Token (LPAREN, false) } ::
          { desc = Child_node; pos = loc.Location.loc_start } :: lp_and_after
      in
      inner_tokens, outer_tokens
    | _ -> assert false

  (* The tree and nesting of tokens is different between the boxed and unboxed
     versions of units and bools...

     Perhaps the cst should have a Pexp_unit? meh. *)

  (* A single CST node [P..._unboxed_unit] is transformed into two nested
      CST nodes: [P...construct (Lident "()", None)].

      So here we transform the single tokens sequence
      {v
        ..x1.. #( ..x2.. ) ..x3..      // where ..x2.. is a list of comments
      v}
      into two:
      - {v ..x1.. ( Child_node ) ..x3.. v}
      - {v ..x2.. v}
   *)
  let unit ~loc tokens =
    let tokens, lid_tokens =
      let open Tokens.Seq in
      (* N.B. a ppx to write the following as:
         {[
           match%tokens e.pexp_tokens with
           | before, HASHLPAREN, between, RPAREN, after ->
             ...
         ]}
         would be pretty easy to implement, and the resulting code would be much
         more legible.
      *)
      match split ~on:HASHLPAREN tokens with
      | before, lparen :: rem ->
        begin match split ~on:RPAREN rem with
        | between, rparen :: after ->
          before @ { desc = Child_node; pos = lparen.pos } :: after,
          { lparen with desc = Token (LPAREN, false) } :: between @ [rparen]
        | _ -> assert false
        end
      | _ -> assert false
    in
    let lid = { Longident.desc = Lident (Str "()"); tokens = lid_tokens } in
    let lid_loc = Location.mkloc lid loc in
    lid_loc, tokens

  (* Likewise: one CST node/token sequence into two.
     But slightly simpler because there can't be comments. *)
  let bool ~loc tokens b =
    let unboxed, boxed, name =
      let open Parser_tokens in
      if b
      then HASHTRUE, TRUE, "true"
      else HASHFALSE, FALSE, "false"
    in
    let tokens, lid_tokens =
      match Tokens.Seq.split tokens ~on:unboxed with
      | before, tok :: after ->
        before @ { desc = Child_node; pos = tok.pos } :: after,
        [{ tok with desc = Token (boxed, false) }]
      | _ -> assert false
    in
    let lid = { Longident.desc = Lident (Str name); tokens = lid_tokens } in
    let lid_loc = Location.mkloc lid loc in
    lid_loc, tokens
end
