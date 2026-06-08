open Ocaml_syntax
open Parsetree

let merge_attrs parent_tokens attrs1 attrs2 =
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

let without_child ?at:pos flattened_child_tokens tokens =
  match Tokens.Seq.split_on_child ?pos tokens with
  | _, [] ->
    (* We expected to find a Child_node at the given position, otherwise where
       did we get [flattened_child_tokens] from? *)
    assert false
  | before, _child :: after ->
    let cmts_of_child = List.filter Tokens.is_comment flattened_child_tokens in
    before @ cmts_of_child @ after

let no_ext_attrs = function
  | { pea_ext = None; pea_attrs = No_attributes } -> true
  | _ -> false

let fold_into ~parent parent_kw_tok child =
  assert (no_ext_attrs parent.pexp_ext_attr);
  let parent_tokens, merged_attributes =
    merge_attrs parent.pexp_tokens parent.pexp_attributes child.pexp_attributes
  in
  let tokens =
    parent_tokens
    |> Tokens.Seq.without ~token:parent_kw_tok
    |> Tokens.replace_first_child ~subst:child.pexp_tokens
  in
  { child with pexp_tokens = tokens; pexp_attributes = merged_attributes }

let dummy_pos_lid pos =
  let mk_ident_tok ?(uppercase=true) s =
    let tok : Parser_tokens.token = if uppercase then UIDENT s else LIDENT s in
    { Tokens.desc = Token (tok, false); pos }
  in
  let mk_ldot ?uppercase lid s =
    let tokens =
      [ { Tokens.desc = Child_node; pos }
      ; { desc = Token (DOT, false); pos }
      ; mk_ident_tok ?uppercase s ]
    in
    { Longident.desc = Ldot (lid, Str s); tokens }
  in
  let stdlib =
    let str = "Stdlib" in
    { Longident.desc = Lident (Str str); tokens = [mk_ident_tok str] }
  in
  let lexing = mk_ldot stdlib "Lexing" in
  mk_ldot ~uppercase:false lexing "dummy_pos"

let token_of_legacy_mode (m : mode Location.loc) : Parser_tokens.token =
  match m.txt with
  | Mode "local" -> LOCAL
  | Mode "unique" -> UNIQUE
  | Mode "once" -> ONCE
  | _ -> assert false

let expression e =
  match e.pexp_desc with
  | Pexp_mode_legacy (m, me) -> fold_into ~parent:e (token_of_legacy_mode m) me
  | Pexp_stack se -> fold_into ~parent:e STACK se
  | Pexp_exclave ee -> fold_into ~parent:e EXCLAVE ee
  | Pexp_borrow be -> fold_into ~parent:e BORROW be
  | Pexp_unboxed_unit ->
    (* The tree and nesting of tokens is different between the boxed and unboxed
       version...

       Perhaps the cst should have a Pexp_unit? *)
    let exp_tokens, lid_tokens =
      let open Tokens.Seq in
      (* TODO: a ppx to write the following as:
         {[
           match%tokens e.pexp_tokens with
           | before, HASHLPAREN, between, RPAREN, after ->
             ...
         ]} *)
      match split ~on:HASHLPAREN e.pexp_tokens with
      | before, lparen :: rem ->
        begin match split ~on:RPAREN rem with
        | between, rparen :: after ->
          before @ { lparen with desc = Child_node } :: after,
          { lparen with desc = Token (LPAREN, false) } :: between @ [rparen]
        | _ -> assert false
        end
      | _ -> assert false
    in
    let lid = { Longident.desc = Lident (Str "()"); tokens = lid_tokens } in
    let lid_loc = Location.mkloc lid e.pexp_loc in
    { e with
      pexp_desc = Pexp_construct (lid_loc, None);
      pexp_tokens = exp_tokens }
  | Pexp_unboxed_bool b ->
    let unboxed, boxed, name =
      let open Parser_tokens in
      if b
      then HASHTRUE, TRUE, "true"
      else HASHFALSE, FALSE, "false"
    in
    let exp_tokens, lid_tokens =
      match Tokens.Seq.split e.pexp_tokens ~on:unboxed with
      | before, tok :: after ->
        before @ { tok with desc = Child_node } :: after,
        [{ tok with desc = Token (boxed, false) }]
      | _ -> assert false
    in
    let lid = { Longident.desc = Lident (Str name); tokens = lid_tokens } in
    let lid_loc = Location.mkloc lid e.pexp_loc in
    { e with
      pexp_desc = Pexp_construct (lid_loc, None);
      pexp_tokens = exp_tokens }
  | Pexp_unboxed_tuple fields ->
    { e with
      pexp_desc = Pexp_tuple fields;
      pexp_tokens =
        Tokens.Seq.search_and_replace [HASHLPAREN, LPAREN] e.pexp_tokens }
  | Pexp_record_unboxed_product (re, fields) ->
    { e with
      pexp_desc = Pexp_record (re, fields);
      pexp_tokens =
        Tokens.Seq.search_and_replace [HASHLBRACE, LBRACE] e.pexp_tokens }
  | Pexp_unboxed_field (e, fn) ->
    { e with
      pexp_desc = Pexp_field (e, fn);
      pexp_tokens =
        Tokens.Seq.search_and_replace [DOTHASH, DOT] e.pexp_tokens }
  | Pexp_extension
      ({ txt = ["src_pos"]; _ },
       PStr { pst_items = []; pst_tokens; _ },
       ext_tokens) ->
    let comments =
      List.filter Tokens.is_comment pst_tokens @
      List.filter Tokens.is_comment ext_tokens
    in
    let pos = e.pexp_loc.loc_start in
    let lid = dummy_pos_lid pos in
    let lid_loc = Location.mkloc lid e.pexp_loc in
    let tokens = { Tokens.desc = Child_node; pos } :: comments in
    { e with
      pexp_desc = Pexp_ident lid_loc;
      pexp_tokens = tokens }
  | _ ->
    e

let pattern p =
  match p.ppat_desc with
  | Ppat_unboxed_unit ->
    (* The tree and nesting of tokens is different between the boxed and unboxed
       version...

       Perhaps the cst should have a Pexp_unit? *)
    let exp_tokens, lid_tokens =
      let open Tokens.Seq in
      (* TODO: a ppx to write the following as:
         {[
           match%tokens e.pexp_tokens with
           | before, HASHLPAREN, between, RPAREN, after ->
             ...
         ]} *)
      match split ~on:HASHLPAREN p.ppat_tokens with
      | before, lparen :: rem ->
        begin match split ~on:RPAREN rem with
        | between, rparen :: after ->
          before @ { lparen with desc = Child_node } :: after,
          { lparen with desc = Token (LPAREN, false) } :: between @ [rparen]
        | _ -> assert false
        end
      | _ -> assert false
    in
    let lid = { Longident.desc = Lident (Str "()"); tokens = lid_tokens } in
    let lid_loc = Location.mkloc lid p.ppat_loc in
    { p with
      ppat_desc = Ppat_construct (lid_loc, None);
      ppat_tokens = exp_tokens }
  | Ppat_unboxed_bool b ->
    let unboxed, boxed, name =
      let open Parser_tokens in
      if b
      then HASHTRUE, TRUE, "true"
      else HASHFALSE, FALSE, "false"
    in
    let pat_tokens, lid_tokens =
      match Tokens.Seq.split p.ppat_tokens ~on:unboxed with
      | before, tok :: after ->
        before @ { tok with desc = Child_node } :: after,
        [{ tok with desc = Token (boxed, false) }]
      | _ -> assert false
    in
    let lid = { Longident.desc = Lident (Str name); tokens = lid_tokens } in
    let lid_loc = Location.mkloc lid p.ppat_loc in
    { p with
      ppat_desc = Ppat_construct (lid_loc, None);
      ppat_tokens = pat_tokens }
  | Ppat_unboxed_tuple (fields, cf) ->
    { p with
      ppat_desc = Ppat_tuple (fields, cf);
      ppat_tokens =
        Tokens.Seq.search_and_replace [HASHLPAREN, LPAREN] p.ppat_tokens }
  | Ppat_record_unboxed_product (fields, cf) ->
    { p with
      ppat_desc = Ppat_record (fields, cf);
      ppat_tokens =
        Tokens.Seq.search_and_replace [HASHLBRACE, LBRACE] p.ppat_tokens }
  | _ ->
    p

(* TODO: - modes on arrow arguments! *)

let get_modes_tokens m = Result.get_ok (Tokens_of_tree.modes m)

let argument a =
  (* Legacy modes, when present, are the first Child_node of the argument
     tokens. (Cf. parser.mly) *)
  match a.parg_desc with
  | Parg_unlabelled ({ legacy_modes = Modes _; _ } as arg_info) ->
    let modes_tokens = get_modes_tokens arg_info.legacy_modes in
    { a with
      parg_desc = Parg_unlabelled { arg_info with legacy_modes = No_modes };
      parg_tokens = without_child modes_tokens a.parg_tokens }
  | Parg_labelled ({ legacy_modes = Modes _; _ } as arg_info) ->
    let modes_tokens = get_modes_tokens arg_info.legacy_modes in
    { a with
      parg_desc = Parg_labelled { arg_info with legacy_modes = No_modes };
      parg_tokens = without_child modes_tokens a.parg_tokens }
  | _ -> a

let value_binding vb =
  match vb.pvb_legacy_modes with
  | No_modes -> vb
  | Modes m as modes ->
    let modes_toks = get_modes_tokens modes in
    { vb with
      pvb_legacy_modes = No_modes;
      pvb_tokens = without_child ~at:m.loc.loc_start modes_toks vb.pvb_tokens }

let is_curry_attr attr = attr.attr_name.txt = ["extension"; "curry"]
let has_curry_attr = function
  | No_attributes -> false
  | Attributes a -> List.exists is_curry_attr a.attributes

let without_curry_attr attrs =
  match attrs with
  | No_attributes -> attrs
  | Attributes { attributes; loc; tokens } ->
    match List.find_opt is_curry_attr attributes with
    | None -> attrs
    | Some { attr_loc; _ } ->
      let not_curry a = not (is_curry_attr a) in
      let attributes = List.filter not_curry attributes in
      let tokens =
        (* N.B. there could a comment inside the [@extension.curry] attribute
           ... but I think we're ok with dropping it. *)
        let before, rem =
          Tokens.Seq.split_on_child ~pos:attr_loc.loc_start tokens
        in
        before @ List.tl rem
      in
      Attributes { attributes; loc; tokens }

let get_jkind_annotation_tokens jk =
  Result.get_ok (Tokens_of_tree.jkind_annotation jk)

let core_type ct =
  match ct.ptyp_desc with
  | Ptyp_arrow _ when has_curry_attr ct.ptyp_attributes ->
    let attrs = without_curry_attr ct.ptyp_attributes in
    { ct with ptyp_attributes = attrs }
  | Ptyp_unboxed_tuple cts ->
    { ct with
      ptyp_desc = Ptyp_tuple cts;
      ptyp_tokens =
        Tokens.Seq.search_and_replace [HASHLPAREN, LPAREN] ct.ptyp_tokens }
  | Ptyp_any Some jk ->
    let jk_toks = get_jkind_annotation_tokens jk in
    { ct with
      ptyp_desc = Ptyp_any None;
      ptyp_tokens =
        without_child ~at:jk.pjka_loc.loc_start jk_toks ct.ptyp_tokens }
  | Ptyp_var (name, Some jk) ->
    let jk_toks = get_jkind_annotation_tokens jk in
    { ct with
      ptyp_desc = Ptyp_var (name, None);
      ptyp_tokens =
        without_child ~at:jk.pjka_loc.loc_start jk_toks ct.ptyp_tokens }
  | Ptyp_alias (aliased_ty, None, Some erasable_jkind) ->
    (* N.B. with the current grammar, there can't be attributes on alias_type,
       so we can just return the child node.

       We do take care to keep the comments (they all move to the end). *)
    let alias_comments = List.filter Tokens.is_comment ct.ptyp_tokens in
    let jk_comments =
      List.filter Tokens.is_comment (get_jkind_annotation_tokens erasable_jkind)
    in
    { aliased_ty with
      ptyp_tokens = aliased_ty.ptyp_tokens @ alias_comments @ jk_comments }
  | _ ->
    (* FIXME: [Ptyp_of_kind] *)
    ct

let bound_ty_var bv =
  match bv.pbtv_kind with
  | None -> bv
  | Some jk ->
    let jk_toks = get_jkind_annotation_tokens jk in
    let tokens =
      bv.pbtv_tokens
      |> Tokens.Seq.without ~token:LPAREN
      |> Tokens.Seq.without ~token:RPAREN
      |> without_child ~at:jk.pjka_loc.loc_start jk_toks
    in
    { bv with pbtv_kind = None; pbtv_tokens = tokens }

let jkind_to_attr jk =
  let rec desc_to_attr = function
    | Pjk_parens desc -> desc_to_attr desc
    | Pjk_abbreviation
        ({txt =
            { desc = Lident Str ("immediate" | "immediate64" as s)
            ; tokens = lid_toks }; loc}, []) ->
      let attr =
        let empty_payload : structure =
          { pst_items = []; pst_loc = loc; pst_tokens = [] }
        in
        let tokens = Tokens.replace_first_child ~subst:lid_toks jk.pjka_tokens in
        Ast_helper.Attr.mk ~loc:jk.pjka_loc ~tokens
          (Location.mkloc [s] loc) (PStr empty_payload)
      in
      Some attr
    | _ -> None
  in
  desc_to_attr jk.pjka_desc

let type_declaration td =
  match td.ptype_jkind_annotation with
  | None -> td
  | Some jk ->
    match jkind_to_attr jk with
    | None ->
      (* Easy case: remove the subtree, keeping its comments *)
      let jk_tokens = get_jkind_annotation_tokens jk in
      let tokens =
        without_child ~at:jk.pjka_loc.loc_start jk_tokens td.ptype_tokens
      in
      { td with ptype_tokens = tokens }
    | Some attr ->
      (* More work: converted to an attr, add it to the list.

         [Comment] tokens from the jkind are now stored in [attr] so we can
         blindly remove the [Child_node] corresponding to [jk]. *)
      let tokens = without_child ~at:jk.pjka_loc.loc_start [] td.ptype_tokens in
      let attrs, tokens =
        let child =
          { Tokens.desc = Child_node; pos = attr.attr_loc.loc_start }
        in
        match td.ptype_attributes with
        | Attributes { attributes; loc; tokens = attrs_tokens } ->
          Attributes {
            attributes = attr :: attributes;
            loc;
            tokens = child :: attrs_tokens
          }, tokens
        | No_attributes ->
          Attributes
            { attributes = [attr]; loc = attr.attr_loc; tokens = [child] },
          tokens @ [child]
      in
      { td with ptype_attributes = attrs; ptype_tokens = tokens }
