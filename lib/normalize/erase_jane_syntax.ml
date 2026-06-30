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

module Modes = struct
  let remove_from_tokens modes tokens =
    match modes with
    | No_modes -> tokens
    | Modes { loc; tokens = modes_tokens; _ } ->
      without_child ~at:loc.loc_start
        (List.filter Tokens.is_comment modes_tokens) tokens
end

module Modalities = struct
  let remove_from_tokens modas tokens =
    match modas with
    | No_modalities -> tokens
    | Modalities { loc; tokens = modes_tokens; _ } ->
      without_child ~at:loc.loc_start
        (List.filter Tokens.is_comment modes_tokens) tokens
end

module Implicit_source_pos = struct
  let mk_lexing_lident ~pos lident =
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
    mk_ldot ~uppercase:false lexing lident

  let dummy_pos_lid pos = mk_lexing_lident ~pos "dummy_pos"
  let position_lid pos = mk_lexing_lident ~pos "position"

  let is_call_pos : extension -> bool = function
    | { txt = ["call_pos"]; _ }, PStr { pst_items = []; _ }, _ -> true
    | _ -> false

  let is_src_pos : extension -> bool = function
    | { txt = ["src_pos"]; _ }, PStr { pst_items = []; _ }, _ -> true
    | _ -> false

  let mk_typ typ =
    match typ.ptyp_desc with
    | Ptyp_extension
        ({ txt = ["call_pos"]; _ },
         PStr { pst_items = []; pst_tokens; _ },
         ext_tokens) ->
      let comments =
        List.filter Tokens.is_comment pst_tokens @
        List.filter Tokens.is_comment ext_tokens
      in
      let pos = typ.ptyp_loc.loc_start in
      let lid = position_lid pos in
      let lid_loc = Location.mkloc lid typ.ptyp_loc in
      let tokens = { Tokens.desc = Child_node; pos } :: comments in
      { typ with
        ptyp_desc = Ptyp_constr ([], lid_loc);
        ptyp_tokens = tokens }
    | _ -> assert false

  let mk_default ~loc ~attrs pst_tokens ext_tokens =
    let comments =
      List.filter Tokens.is_comment pst_tokens @
      List.filter Tokens.is_comment ext_tokens
    in
    let pos = loc.Location.loc_start in
    let lid = dummy_pos_lid pos in
    let lid_loc = Location.mkloc lid loc in
    let tokens = { Tokens.desc = Child_node; pos } :: comments in
    { pexp_ext_attr = { pea_ext = None; pea_attrs = No_attributes };
      pexp_desc = Pexp_ident lid_loc;
      pexp_loc = loc;
      pexp_attributes = attrs;
      pexp_tokens = tokens }

  let default_of_exp e =
    match e.pexp_desc with
    | Pexp_extension (_, PStr { pst_tokens; _ }, ext_tokens) ->
      mk_default ~loc:e.pexp_loc ~attrs:e.pexp_attributes pst_tokens ext_tokens
    | _ -> assert false

  let default_of_typ typ =
    match typ.ptyp_desc with
    | Ptyp_extension (_, PStr { pst_tokens; _ }, ext_tokens) ->
      mk_default ~loc:typ.ptyp_loc ~attrs:typ.ptyp_attributes
        pst_tokens ext_tokens
    | _ -> assert false
end

module Constant = struct
  let rewrite parent_tokens = function
    | Pconst_unboxed_float (sign, lit, modifier) ->
      Pconst_float (sign, lit, modifier),
      Tokens.Seq.search_and_replace
        [ HASH_FLOAT (lit, modifier)
        , FLOAT (lit, modifier) ]
        parent_tokens
    | Pconst_unboxed_integer (sign, lit, modifier) ->
      Pconst_integer (sign, lit, Some modifier),
      Tokens.Seq.search_and_replace
        [ HASH_INT (lit, Some modifier)
        , INT (lit, Some modifier) ]
        parent_tokens
    | c -> c, parent_tokens
end

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
  | Pexp_constant c ->
    let boxed_c, tokens = Constant.rewrite e.pexp_tokens c in
    { e with
      pexp_desc = Pexp_constant boxed_c;
      pexp_tokens = tokens }
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
    (* For the sake of simplicity: keep it in parens *)
    begin match Tokens.Seq.split ~on:HASHLPAREN e.pexp_tokens with
    | before_hlp, hlp :: after_hlp ->
      let inner_tokens, lp_and_after = Tokens.Seq.split ~on:RPAREN after_hlp in
      let inner =
        { pexp_desc = Pexp_tuple fields
        ; pexp_loc = e.pexp_loc
        ; pexp_ext_attr = { pea_ext = None; pea_attrs = No_attributes }
        ; pexp_attributes = No_attributes
        ; pexp_tokens = inner_tokens }
      in
      { e with
        pexp_desc = Pexp_parens { exp = inner; optional = false };
        pexp_tokens =
          before_hlp @ { hlp with desc = Token (LPAREN, false) } ::
          { desc = Child_node; pos = e.pexp_loc.loc_start } :: lp_and_after }
    | _ -> assert false
    end
  | Pexp_record_unboxed_product (re, fields) ->
    { e with
      pexp_desc = Pexp_record (re, fields);
      pexp_tokens =
        Tokens.Seq.search_and_replace [HASHLBRACE, LBRACE] e.pexp_tokens }
  | Pexp_unboxed_field (re, fn) ->
    { e with
      pexp_desc = Pexp_field (re, fn);
      pexp_tokens =
        Tokens.Seq.search_and_replace [DOTHASH, DOT] e.pexp_tokens }
  | Pexp_extension ext when Implicit_source_pos.is_src_pos ext ->
    Implicit_source_pos.default_of_exp e
  | Pexp_constraint (ce, None, modes) ->
    { e with
      pexp_desc = Pexp_parens { exp = ce; optional = false };
      pexp_tokens =
        Modes.remove_from_tokens modes e.pexp_tokens
        |> Tokens.Seq.without ~token:COLON }
  | Pexp_constraint (ce, ct, modes) ->
    { e with
      pexp_desc = Pexp_constraint (ce, ct, No_modes);
      pexp_tokens = Modes.remove_from_tokens modes e.pexp_tokens }
  | Pexp_function (params, cstrt, body) ->
    { e with
      pexp_desc =
        Pexp_function
          (params, { cstrt with ret_mode_annotations = No_modes }, body);
      pexp_tokens =
        Modes.remove_from_tokens cstrt.ret_mode_annotations e.pexp_tokens }
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
  | Ppat_constant c ->
    let boxed_c, tokens = Constant.rewrite p.ppat_tokens c in
    { p with
      ppat_desc = Ppat_constant boxed_c;
      ppat_tokens = tokens }
  | Ppat_interval (c1, c2) ->
    (* N.B. we thread [tokens] as the code might be ill-typed, e.g. the pattern
       could be [#1 .. #3.14]. *)
    let boxed_c1, tokens = Constant.rewrite p.ppat_tokens c1 in
    let boxed_c2, tokens = Constant.rewrite tokens c2 in
    { p with
      ppat_desc = Ppat_interval (boxed_c1, boxed_c2);
      ppat_tokens = tokens }
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
    (* As in expressions, we keep parentheses *)
    begin match Tokens.Seq.split ~on:HASHLPAREN p.ppat_tokens with
    | before_hlp, hlp :: after_hlp ->
      let inner_tokens, lp_and_after = Tokens.Seq.split ~on:RPAREN after_hlp in
      let inner =
        { ppat_desc = Ppat_tuple (fields, cf)
        ; ppat_loc = p.ppat_loc
        ; ppat_ext_attr = { pea_ext = None; pea_attrs = No_attributes }
        ; ppat_attributes = No_attributes
        ; ppat_tokens = inner_tokens }
      in
      { p with
        ppat_desc = Ppat_parens { pat = inner; optional = false };
        ppat_tokens =
          before_hlp @ { hlp with desc = Token (LPAREN, false) } ::
          { desc = Child_node; pos = p.ppat_loc.loc_start } :: lp_and_after }
    | _ -> assert false
    end
  | Ppat_record_unboxed_product (fields, cf) ->
    { p with
      ppat_desc = Ppat_record (fields, cf);
      ppat_tokens =
        Tokens.Seq.search_and_replace [HASHLBRACE, LBRACE] p.ppat_tokens }
  | Ppat_constraint (subp, tc, modes) ->
    { p with
      ppat_desc = Ppat_constraint (subp, tc, No_modes);
      ppat_tokens = Modes.remove_from_tokens modes p.ppat_tokens }
  | _ ->
    p

let get_modes_tokens m = Result.get_ok (Tokens_of_tree.modes m)

module Argument = struct
  let erase_modes a =
    let desc, legacy_modes, modes =
      match a.parg_desc with
      | Parg_unlabelled arg ->
        Parg_unlabelled { arg with legacy_modes = No_modes; modes = No_modes },
        arg.legacy_modes,
        arg.modes
      | Parg_labelled arg ->
        Parg_labelled { arg with legacy_modes = No_modes; modes = No_modes },
        arg.legacy_modes,
        arg.modes
    in
    let tokens =
      Modes.remove_from_tokens legacy_modes a.parg_tokens
      |> Modes.remove_from_tokens modes
    in
    { a with parg_desc = desc; parg_tokens = tokens }

  let cleanup_parens a =
    (* FIXME: incorrect when the pattern is non-trivial, e.g. (Some x). *)
    match a.parg_desc with
    | Parg_unlabelled
        { legacy_modes=No_modes; typ_constraint=None; modes=No_modes; _ }
    | Parg_labelled
        { legacy_modes=No_modes; typ_constraint=None; modes=No_modes;
          default=None; _ }
        ->
      { a with
        parg_tokens =
          a.parg_tokens
          |> Tokens.Seq.without ~token:LPAREN
          |> Tokens.Seq.without ~token:RPAREN }
    | _ -> a

  let rewrite_call_pos_ext a =
    match a.parg_desc with
    | Parg_labelled
        ({ optional = false
         ; typ_constraint =
             Some Pconstraint ({ ptyp_desc = Ptyp_extension ext; _ } as typ)
         ; maybe_punned = None
         ; _ } as arg_info)
      when Implicit_source_pos.is_call_pos ext ->
      let default = Implicit_source_pos.default_of_typ typ in
      let parg_desc =
        Parg_labelled
          { arg_info with
            optional = true
          ; typ_constraint = None
          ; default = Some default }
      in
      let parg_tokens =
        (* Not strictly necessary, but cleaner. Also might be helpful when
           looking at the rewritten tokens stream during debug sessions. *)
        Tokens.Seq.search_and_replace
          [ TILDE, QUESTION
          ; COLON, EQUAL ]
          a.parg_tokens
      in
      { a with parg_desc; parg_tokens }
    | Parg_labelled
        ({ optional = false
         ; typ_constraint = None
         ; maybe_punned =
             Some
               ({ ppat_desc =
                    Ppat_constraint
                      (p, Some ({ ptyp_desc = Ptyp_extension ext; _} as typ), m)
                ; _ } as pat)
         ; _ } as arg_info)
      when Implicit_source_pos.is_call_pos ext ->
      let default = Implicit_source_pos.default_of_typ typ in
      let not_punned, extruded_child_node =
        match Tokens.Seq.split ~on:COLON pat.ppat_tokens with
        | sub_pat_tokens, _colon :: child_node :: following_tokens ->
          { pat with
            ppat_desc = Ppat_constraint (p, None, m)
          ; ppat_tokens = sub_pat_tokens @ following_tokens },
          child_node
        | _ -> assert false
      in
      let parg_desc =
        Parg_labelled
          { arg_info with
            optional = true
          ; maybe_punned = Some not_punned
          ; default = Some default }
      in
      let parg_tokens =
        let open Tokens in
        match
          a.parg_tokens
          |> Seq.search_and_replace
               (* Not strictly necessary, but cleaner. Also might be helpful when
                  looking at the rewritten tokens stream during debug sessions. *)
               [ LABEL arg_info.name, OPTLABEL arg_info.name
               ; TILDE, QUESTION ]
          |> Seq.split_on_child ~pos:pat.ppat_loc.loc_start
        with
        | before, pat_child_node :: after ->
          before @
          { desc = Token (LPAREN, false); pos = pat.ppat_loc.loc_start } ::
          pat_child_node ::
          { desc = Token (EQUAL, false); pos = extruded_child_node.pos } ::
          extruded_child_node ::
          { desc = Token (RPAREN, false); pos = pat.ppat_loc.loc_end } ::
          after
        | _ -> assert false
      in
      { a with parg_desc; parg_tokens }
    | _ -> a

  let generic_erase a =
    erase_modes a
    |> cleanup_parens

  (* Additionnaly remove [%call_pos] from:
     - Pcl_fun
     - pci_value_params
     - Pparam_val *)
  let erase_function_param a =
    erase_modes a
    |> rewrite_call_pos_ext
    |> cleanup_parens
end

let value_binding vb =
  let tokens_without_pat_modes =
    match vb.pvb_modes with
    | No_modes -> vb.pvb_tokens
    | Modes _ as m ->
      Modes.remove_from_tokens m vb.pvb_tokens
      |> Tokens.Seq.without ~token:LPAREN
      |> Tokens.Seq.without ~token:RPAREN
  in
  { vb with
    pvb_legacy_modes = No_modes;
    pvb_modes = No_modes;
    pvb_ret_modes = No_modes;
    pvb_tokens =
      tokens_without_pat_modes
      |> Modes.remove_from_tokens vb.pvb_legacy_modes
      |> Modes.remove_from_tokens vb.pvb_ret_modes
  }

let is_curry_attr attr = attr.attr_name.txt = ["extension"; "curry"]

let without_curry_attr attrs =
  match attrs with
  | Attributes { attributes; loc; tokens }
    when List.exists is_curry_attr attributes ->
    begin match List.find_opt is_curry_attr attributes with
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
    end
  | _ -> attrs

let get_jkind_annotation_tokens jk =
  Result.get_ok (Tokens_of_tree.jkind_annotation jk)

let get_jkind_annotation_comments jk =
  get_jkind_annotation_tokens jk
  |> List.filter Tokens.is_comment

module Arrow_arg = struct
  let rewrite_implicit_src_pos aa =
    let typ = aa.aa_type in
    match aa.aa_lbl, typ.ptyp_desc with
    | Labelled lbl, Ptyp_extension ext
      when Implicit_source_pos.is_call_pos ext ->
      let aa_type = Implicit_source_pos.mk_typ typ in
      { aa with
        aa_type;
        aa_lbl = Optional lbl;
        aa_tokens =
          { Tokens.desc = Token (QUESTION, false); pos = aa.aa_loc.loc_start }
          :: aa.aa_tokens }
    | _ ->
      aa

  let erase_modes aa =
    let aa_tokens =
      Modes.remove_from_tokens aa.aa_legacy_modes aa.aa_tokens
      |> Modes.remove_from_tokens aa.aa_modes
    in
    { aa with aa_legacy_modes = No_modes; aa_modes = No_modes; aa_tokens }

  let erase aa =
    rewrite_implicit_src_pos aa
    |> erase_modes
end

let rec unboxed_type { Longident.desc; tokens } : Longident.t =
  match desc with
  | Lident Str_trailing_hash s ->
    { desc = Lident (Str s);
      tokens = Tokens.Seq.without ~token:HASH_SUFFIX tokens }
  | Ldot (lid, Str_trailing_hash s) ->
    let lid = unboxed_type lid in
    { desc = Ldot (lid, Str s);
      tokens = Tokens.Seq.without ~token:HASH_SUFFIX tokens }
  | Lident _ -> { desc; tokens }
  | Ldot (lid, s) -> { desc = Ldot (unboxed_type lid, s); tokens }
  | Lapply (l1, l2) ->
    { desc = Lapply (unboxed_type l1, unboxed_type l2); tokens }

let core_type ct =
  match ct.ptyp_desc with
  | Ptyp_arrow at ->
    let tokens =
      Modes.remove_from_tokens at.codom_legacy_modes ct.ptyp_tokens
      |> Modes.remove_from_tokens at.codom_modes
    in
    let attrs = without_curry_attr ct.ptyp_attributes in
    { ct with
      ptyp_desc =
        Ptyp_arrow
          { at with codom_legacy_modes = No_modes; codom_modes = No_modes };
      ptyp_attributes = attrs;
      ptyp_tokens = tokens }
  | Ptyp_unboxed_tuple cts ->
    begin match Tokens.Seq.split ~on:HASHLPAREN ct.ptyp_tokens with
    | before_hlp, hlp :: after_hlp ->
      let inner_tokens, lp_and_after = Tokens.Seq.split ~on:RPAREN after_hlp in
      let inner =
        { ptyp_desc = Ptyp_tuple cts
        ; ptyp_loc = ct.ptyp_loc
        ; ptyp_attributes = No_attributes
        ; ptyp_tokens = inner_tokens }
      in
      { ct with
        ptyp_desc = Ptyp_parens inner;
        ptyp_tokens =
          before_hlp @ { hlp with desc = Token (LPAREN, false) } ::
          { desc = Child_node; pos = ct.ptyp_loc.loc_start } :: lp_and_after }
    | _ -> assert false
    end
  | Ptyp_any Some jk ->
    let jk_coms = get_jkind_annotation_comments jk in
    { ct with
      ptyp_desc = Ptyp_any None;
      ptyp_tokens =
        without_child ~at:jk.pjka_loc.loc_start jk_coms ct.ptyp_tokens
        |> Tokens.Seq.without ~token:COLON }
  | Ptyp_var (name, Some jk) ->
    let jk_coms = get_jkind_annotation_comments jk in
    { ct with
      ptyp_desc = Ptyp_var (name, None);
      ptyp_tokens =
        without_child ~at:jk.pjka_loc.loc_start jk_coms ct.ptyp_tokens
        |> Tokens.Seq.without ~token:COLON }
  | Ptyp_alias (aliased_ty, None, Some erasable_jkind) ->
    (* N.B. with the current grammar, there can't be attributes on alias_type,
       so we can just return the child node.

       We do take care to keep the comments (they all move to the end). *)
    let alias_comments = List.filter Tokens.is_comment ct.ptyp_tokens in
    let jk_comments = get_jkind_annotation_comments erasable_jkind in
    { aliased_ty with
      ptyp_tokens = aliased_ty.ptyp_tokens @ alias_comments @ jk_comments }
  | Ptyp_alias (aliased_ty, alias, Some jk) ->
    let jk_coms = get_jkind_annotation_comments jk in
    { ct with
      ptyp_desc = Ptyp_alias (aliased_ty, alias, None);
      ptyp_tokens =
        without_child ~at:jk.pjka_loc.loc_start jk_coms ct.ptyp_tokens
        |> Tokens.Seq.without ~token:LPAREN
        |> Tokens.Seq.without ~token:RPAREN
        |> Tokens.Seq.without ~token:COLON }
  | Ptyp_constr (params, t) ->
    { ct with ptyp_desc = Ptyp_constr (params, Location.map unboxed_type t) }
  | _ ->
    (* FIXME: [Ptyp_of_kind] *)
    ct

let bound_ty_var bv =
  match bv.pbtv_kind with
  | None -> bv
  | Some jk ->
    let tokens =
      let base =
        if List.exists (Tokens.is_token ~which:TYPE) bv.pbtv_tokens
        then (* newtype, keep parens *) bv.pbtv_tokens
        else
          bv.pbtv_tokens
          |> Tokens.Seq.without ~token:LPAREN
          |> Tokens.Seq.without ~token:RPAREN
      in
      Tokens.Seq.without ~token:COLON base
      |> without_child ~at:jk.pjka_loc.loc_start
           (get_jkind_annotation_comments jk)
    in
    { bv with pbtv_kind = None; pbtv_tokens = tokens }

let ptype_param p =
  match p.ptp_jkind with
  | None -> p
  | Some jk ->
    let tokens =
      Tokens.Seq.without ~token:COLON p.ptp_tokens
      |> without_child ~at:jk.pjka_loc.loc_start
           (get_jkind_annotation_comments jk)
    in
    { p with ptp_jkind = None; ptp_tokens = tokens }

module Attr = struct
  let synthesize_tokens ~(loc : Location.t) attr_name_tokens =
    let open Tokens in
    { desc = Token (LBRACKETATAT, false); pos = loc.loc_start } ::
    attr_name_tokens @
    { desc = Child_node (* empty payload *); pos = loc.loc_end } ::
    { desc = Token (RBRACKET, false); pos = loc.loc_end } :: []

  let mk_empty_payload ~attr_loc ~name_loc (name, name_tokens) =
    let empty_payload : structure =
      { pst_items = []; pst_loc = name_loc; pst_tokens = [] }
    in
    let tokens = synthesize_tokens ~loc:name_loc name_tokens in
    Ast_helper.Attr.mk ~loc:attr_loc ~tokens
      (Location.mkloc [name] name_loc) (PStr empty_payload)
end

let jkind_to_attr jk =
  let rec desc_to_attr = function
    | Pjk_parens desc -> desc_to_attr desc
    | Pjk_abbreviation
        ({txt =
            { desc = Lident Str ("immediate" | "immediate64" as s)
            ; tokens = lid_toks }; loc}, []) ->
      let attr =
        Attr.mk_empty_payload ~attr_loc:jk.pjka_loc ~name_loc:loc
          (s, Tokens.replace_first_child ~subst:lid_toks jk.pjka_tokens)
      in
      Some attr
    | _ -> None
  in
  desc_to_attr jk.pjka_desc

module Type_declaration = struct
  let erase_jkind_annot td =
    match td.ptype_jkind_annotation with
    | None -> td
    | Some jk ->
      match jkind_to_attr jk with
      | None ->
        (* Easy case: remove the subtree, keeping its comments *)
        let jk_comments = get_jkind_annotation_comments jk in
        let tokens =
          without_child ~at:jk.pjka_loc.loc_start jk_comments td.ptype_tokens
          |> Tokens.Seq.without ~token:COLON
        in
        { td with ptype_jkind_annotation = None; ptype_tokens = tokens }
      | Some attr ->
        (* More work: converted to an attr, add it to the list.

           [Comment] tokens from the jkind are now stored in [attr] so we can
           blindly remove the [Child_node] corresponding to [jk]. *)
        let tokens =
          without_child ~at:jk.pjka_loc.loc_start [] td.ptype_tokens
          |> Tokens.Seq.without ~token:COLON
        in
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
        { td with
          ptype_jkind_annotation = None;
          ptype_attributes = attrs;
          ptype_tokens = tokens }

  let no_unboxed_rec td =
    match td.ptype_kind with
    | Ptype_record_unboxed_product lbls ->
      { td with
        ptype_kind = Ptype_record lbls;
        ptype_tokens =
          Tokens.Seq.search_and_replace [HASHLBRACE, LBRACE] td.ptype_tokens }
    | _ -> td

  let erase td =
    no_unboxed_rec td
    |> erase_jkind_annot
end

let value_description vd =
  { vd with
    pval_modalities = No_modalities;
    pval_tokens =
      Modalities.remove_from_tokens vd.pval_modalities vd.pval_tokens }

module Globalized = struct
  let of_global_ global_typ parent_tokens =
    match Tokens.Seq.split ~on:GLOBAL parent_tokens with
    | before_global, global :: after_global ->
      let tokens = before_global @ after_global in
      let typ_loc = global_typ.ptyp_loc in
      let globalized_attr =
        let loc = { typ_loc with loc_start = typ_loc.loc_end } in
        let attr =
          Attr.mk_empty_payload
            ~attr_loc:loc ~name_loc:loc
            ("globalized", [global (* that's a lie, correct = LIDENT *)])
        in
        Attributes {
          attributes = [attr];
          loc;
          tokens = [{ desc = Child_node; pos = loc.loc_start }]
        }
      in
      let globalized_typ =
        let tokens, attrs =
          merge_attrs global_typ.ptyp_tokens global_typ.ptyp_attributes
            globalized_attr
        in
        { global_typ with ptyp_attributes = attrs; ptyp_tokens = tokens }
      in
      let parens_typ =
        Ast_helper.Typ.mk (Ptyp_parens globalized_typ)
          ~tokens:Tokens.[
            { desc = Token (LPAREN, false); pos = typ_loc.loc_start };
            { desc = Child_node; pos = typ_loc.loc_start }; (* ctyp *)
            { desc = Token (RPAREN, false); pos = typ_loc.loc_end };
          ]
      in
      parens_typ, tokens
    | _ -> assert false
end

module Label_declaration = struct
  let erase_modalities lbl =
    { lbl with
      pld_modalities = No_modalities;
      pld_tokens =
        Modalities.remove_from_tokens lbl.pld_modalities lbl.pld_tokens }

  let global_to_at_globalized lbl =
    if not lbl.pld_global then
      lbl
    else
      let typ, tokens = Globalized.of_global_ lbl.pld_type lbl.pld_tokens in
      { lbl with
        pld_global = false;
        pld_type = typ;
        pld_tokens = tokens }

  let erase ca =
    erase_modalities ca
    |> global_to_at_globalized
end

module Constructor_argument = struct
  let erase_modalities ca =
    { ca with
      pca_modalities = No_modalities;
      pca_tokens =
        Modalities.remove_from_tokens ca.pca_modalities ca.pca_tokens }

  let global_to_at_globalized ca =
    if not ca.pca_global then
      ca
    else
      let typ, tokens = Globalized.of_global_ ca.pca_type ca.pca_tokens in
      { ca with
        pca_global = false;
        pca_type = typ;
        pca_tokens = tokens }

  let erase ca =
    erase_modalities ca
    |> global_to_at_globalized
end

module Synced_progress = struct
  (** Synced traversal of sig/struct items and the corresponding tokens.
      So individual items can be removed from a sig/struct. *)


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

let signature sg =
  let tokens = Modalities.remove_from_tokens sg.psg_modalities sg.psg_tokens in
  let items, tokens =
    Synced_progress.filter sg.psg_items tokens ~drop:(fun si ->
      match si.psig_desc with
      | Psig_jkind _ ->
        Tokens_of_tree.signature_item si
        |> Result.get_ok
        |> List.filter Tokens.is_comment
        |> Option.some
      | _ -> None
    )
  in
  { sg with
    psg_items = items;
    psg_modalities = No_modalities;
    psg_tokens = tokens }

let no_kind_constraint wc toks =
  let remove_stale_ands toks =
    (* annoying case:
       {[
         S with kind (* cmt0 *) and type
            and kind (* cmt1 *) and type
            and (* cmt2 *) kind
       ]}
       once the kind constraints are removed we end up with:
       {[
         S with (* cmt0 *) and type and (* cmt1 *) and type and (* cmt2 *)
       ]}

       We could do a fwd pass and then remove a potential last dangling and, but
       that would give us
       {[
         S with (* cmt0 *) type and (* cmt1 *) type (* cmt2 *)
       ]}

       Whereas the following gives:
       {[
         S with (* cmt0 *) type (* cmt1 *) and type (* cmt2 *)
       ]}
       (which I believe to be more correct).

       Admittedly, this is splitting hairs.
    *)
    let rec aux first = function
      | [] -> true, []
      | t :: ts ->
        match t.Tokens.desc with
        | Child_node ->
          let _, ts = aux false ts in
          false, t :: ts
        | Token (AND, _) ->
          let drop, ts = aux first ts in
          true, if drop || first then ts else t :: ts
        | _ ->
          let drop, ts = aux first ts in
          drop, t :: ts
    in
    let _, tokens = aux true toks in
    tokens
  in
  let constrs, tokens =
    Synced_progress.filter ~drop:(fun (wc : with_constraint) ->
      match wc.wc_desc with
      | Pwith_jkind _
      | Pwith_jkindsubst _ ->
        Tokens_of_tree.with_constraint wc
        |> Result.get_ok
        |> List.filter Tokens.is_comment
        |> Option.some
      | _ ->
        None
    ) wc toks
  in
  constrs, remove_stale_ands tokens

let module_type mt =
  match mt.pmty_desc with
  | Pmty_with (mty, wcs) ->
    begin match Tokens.Seq.split ~on:WITH mt.pmty_tokens with
    | _, [] -> assert false
    | pre, with_ :: suff ->
      match no_kind_constraint wcs suff with
      | [], suff ->
        let tokens, merged_attributes =
          merge_attrs (pre @ suff) mt.pmty_attributes mty.pmty_attributes
        in
        { mty with
          pmty_attributes = merged_attributes;
          pmty_tokens =
            Tokens.replace_first_child ~subst:mty.pmty_tokens tokens }
      | wcs, suff ->
        { mt with
          pmty_desc = Pmty_with (mty, wcs);
          pmty_tokens = pre @ with_ :: suff }
    end
  | _ -> mt

let structure st =
  let items, tokens =
    Synced_progress.filter st.pst_items st.pst_tokens ~drop:(fun si ->
      match si.pstr_desc with
      | Pstr_jkind _ ->
        Tokens_of_tree.structure_item si
        |> Result.get_ok
        |> List.filter Tokens.is_comment
        |> Option.some
      | _ -> None
    )
  in
  { st with
    pst_items = items;
    pst_tokens = tokens }
