(** Simplified version of [lib/normalize/erase_jane_syntax/]: to be used from an
    AST mapper.

    Notable differences:
    - no tokens fiddling
    - as a consequence of the above: more nodes are handled directly rather than
    from their parents (e.g. constants, modes, modalities, etc.)
    - some syntaxes which do not have an "explicit" repr but are stored as
    modes, modalities, etc. are handled "for free" (e.g. "global" decls)
*)

open Oxcaml_frontend
open Parsetree

let constant = function
  | Pconst_unboxed_integer (lit, modif) ->
    Pconst_integer (lit, Some modif)
  | Pconst_unboxed_float (lit, modif) ->
    Pconst_float (lit, modif)
  | Pconst_untagged_char c ->
    Pconst_char c
  | c -> c

let modes _ = []
let modalities _ = []

let is_exclave e =
  match e.pexp_desc with
  | Pexp_extension ({ txt = "extension.exclave"; loc = _ }, PStr []) ->
    true
  | _ -> false

let expression e =
  let fold_into ~parent child =
    let merged_attributes = parent.pexp_attributes @ child.pexp_attributes in
    { child with pexp_attributes = merged_attributes }
  in
  match e.pexp_desc with
  | Pexp_stack child
  | Pexp_borrow child -> fold_into ~parent:e child
  | Pexp_apply (maybe_exclave, [Nolabel, child])
    when is_exclave maybe_exclave ->
    fold_into ~parent:e child
  | Pexp_unboxed_unit ->
    let unit_lid = Location.mkloc (Longident.Lident "()") e.pexp_loc in
    { e with pexp_desc = Pexp_construct (unit_lid, None) }
  | Pexp_unboxed_bool b ->
    let bool_lid =
      Location.mkloc (Longident.Lident (string_of_bool b)) e.pexp_loc
    in
    { e with pexp_desc = Pexp_construct (bool_lid, None) }
  | Pexp_unboxed_tuple fields ->
    { e with pexp_desc = Pexp_tuple fields }
  | Pexp_record_unboxed_product (re, fields) ->
    { e with pexp_desc = Pexp_record (re, fields) }
  | Pexp_unboxed_field (re, fn) ->
    { e with pexp_desc = Pexp_field (re, fn) }
  | Pexp_extension ({ txt = "src_pos"; _ }, PStr []) ->
    let lid_loc =
      Location.mkloc
        Longident.(Ldot (Ldot (Lident "Stdlib", "Lexing"), "dummy_pos"))
        e.pexp_loc
    in
    { e with pexp_desc = Pexp_ident lid_loc }
  | Pexp_constraint (ce, None, _modes) ->
    (* explicit handling to erase the node in the absence of constraints *)
    fold_into ~parent:e ce
  | Pexp_newtype (t, Some _jkind, child) ->
    { e with pexp_desc = Pexp_newtype (t, None, child)}
  | _ -> e

let pattern p =
  match p.ppat_desc with
  | Ppat_unboxed_unit ->
    let unit_lid = Location.mkloc (Longident.Lident "()") p.ppat_loc in
    { p with ppat_desc = Ppat_construct (unit_lid, None) }
  | Ppat_unboxed_bool b ->
    let bool_lid =
      Location.mkloc (Longident.Lident (string_of_bool b)) p.ppat_loc
    in
    { p with ppat_desc = Ppat_construct (bool_lid, None) }
  | Ppat_unboxed_tuple (fields, cf) ->
    { p with ppat_desc = Ppat_tuple (fields, cf) }
  | Ppat_record_unboxed_product (fields, cf) ->
    { p with ppat_desc = Ppat_record (fields, cf) }
  | Ppat_constraint (cp, None, _modes) ->
    (* explicit handling to erase the node in the absence of constraints *)
    let merged_attributes = p.ppat_attributes @ cp.ppat_attributes in
    { cp with ppat_attributes = merged_attributes }
  | _ ->
    p

let rec unboxed_type lid =
  let without_hash s =
    let len = String.length s in
    if s <> "" && String.get s (len - 1) = '#'
    then String.sub s 0 (len - 1)
    else s
  in
  let open Longident in
  match lid with
  | Lident s -> Lident (without_hash s)
  | Ldot (lid, s) -> Ldot (lid, without_hash s)
  | Lapply (l1, l2) -> Lapply (unboxed_type l1, unboxed_type l2)

let is_call_pos ct =
  match ct.ptyp_desc with
  | Ptyp_extension ({ Location.txt = "call_pos"; loc = _ }, PStr []) -> true
  | _ -> false

let core_type ct =
  match ct.ptyp_desc with
  | Ptyp_arrow (lbl, dom, codom, dms, cdms) ->
    let attrs =
      List.filter (fun a -> a.attr_name.txt <> "extension.curry")
        ct.ptyp_attributes
    in
    let ptyp_desc =
      match lbl with
      | Labelled lbl when is_call_pos dom ->
        let lid_loc =
          Location.mkloc
            Longident.(Ldot (Ldot (Lident "Stdlib", "Lexing"), "position"))
            dom.ptyp_loc
        in
        let dom = { dom with ptyp_desc = Ptyp_constr (lid_loc, []) } in
        Ptyp_arrow (Optional lbl, dom, codom, dms, cdms)
      | _ -> ct.ptyp_desc
    in
    { ct with ptyp_desc; ptyp_attributes = attrs }
  | Ptyp_unboxed_tuple cts ->
    { ct with ptyp_desc = Ptyp_tuple cts }
  | Ptyp_any Some _ ->
    { ct with ptyp_desc = Ptyp_any None }
  | Ptyp_var (name, Some _) ->
    { ct with ptyp_desc = Ptyp_var (name, None) }
  | Ptyp_alias (aliased_ty, None, Some _) ->
    (* N.B. with the current grammar, there can't be attributes on alias_type,
       so we can just return the child node. *)
    aliased_ty
  | Ptyp_alias (aliased_ty, alias, Some _) ->
    { ct with ptyp_desc = Ptyp_alias (aliased_ty, alias, None) }
  | Ptyp_constr (t, params) ->
    { ct with ptyp_desc = Ptyp_constr (Location.map unboxed_type t, params) }
  | Ptyp_poly (vars, t) ->
    let vars = List.map (fun (v, _) -> (v, None)) vars in
    { ct with ptyp_desc = Ptyp_poly (vars, t) }
  | _ ->
    (* FIXME: [Ptyp_of_kind] *)
    ct

let function_param_desc = function
  | Pparam_newtype (t, Some _jkind) -> Pparam_newtype (t, None)
  | Pparam_val
      (Labelled lbl, None,
       ({ ppat_desc = Ppat_constraint (p, Some ct, modes) ; _ } as cp))
    when is_call_pos ct ->
    let default =
      let loc = ct.ptyp_loc in
      let lid_loc =
        Location.mkloc
          Longident.(Ldot (Ldot (Lident "Stdlib", "Lexing"), "dummy_pos"))
          loc
      in
      Ast_helper.Exp.ident ~loc lid_loc
    in
    let p = { cp with ppat_desc = Ppat_constraint (p, None, modes) } in
    Pparam_val (Optional lbl, Some default, p)
  | fp -> fp

let is_global md = md.Location.txt = Modality "global"
let globalized { Location.loc; _ } =
  Ast_helper.Attr.mk ~loc (Location.mkloc "globalized" loc) (PStr [])

let label_declaration lbl =
  match List.find is_global lbl.pld_modalities with
  | exception Not_found -> lbl
  | md_loc ->
    let ct =
      { lbl.pld_type with
        ptyp_attributes = globalized md_loc :: lbl.pld_type.ptyp_attributes }
    in
    { lbl with pld_type = ct }

let constructor_argument c =
  match List.find is_global c.pca_modalities with
  | exception Not_found -> c
  | md_loc ->
    let ct =
      { c.pca_type with
        ptyp_attributes = globalized md_loc :: c.pca_type.ptyp_attributes }
    in
    { c with pca_type = ct }

let constructor_declaration cd =
  let vars = List.map (fun (v, _) -> (v, None)) cd.pcd_vars in
  { cd with pcd_vars = vars }

let extension_constructor_kind = function
  | Pext_decl (vars, args, ret_ty) ->
    let vars = List.map (fun (v, _) -> (v, None)) vars in
    Pext_decl (vars, args, ret_ty)
  | eck -> eck

let type_kind = function
  | Ptype_record_unboxed_product lbls -> Ptype_record lbls
  | tk -> tk

let jkind_to_attr jk =
  match jk.pjka_desc with
  | Pjk_abbreviation
      ({txt = Lident ("immediate" | "immediate64" as s); loc}, []) ->
    let attr = Ast_helper.Attr.mk ~loc (Location.mkloc s loc) (PStr []) in
    Some attr
  | _ -> None

let type_declaration td =
  let ptype_attributes =
    match Option.bind td.ptype_jkind_annotation jkind_to_attr with
    | None -> td.ptype_attributes
    | Some a -> a :: td.ptype_attributes
  in
  { td with ptype_attributes; ptype_jkind_annotation = None }

let module_type mty =
  match mty.pmty_desc with
  | Pmty_with (child, cstrs) ->
    begin match
      List.filter (function
        | Pwith_jkind _ | Pwith_jkindsubst _ -> false
        | _ -> true
      ) cstrs
    with
    | [] ->
      { child with
        pmty_attributes = mty.pmty_attributes @ child.pmty_attributes }
    | cstrs -> { mty with pmty_desc = Pmty_with (child, cstrs) }
    end
  | _ -> mty

let module_expr me =
  match me.pmod_desc with
  | Pmod_constraint (child, None, _) ->
    (* explicit handling to erase the node in the absence of constraints *)
    let merged_attributes = me.pmod_attributes @ child.pmod_attributes in
    { child with pmod_attributes = merged_attributes }
  | _ -> me

let signature s =
  let items =
    List.filter (fun si ->
      match si.psig_desc with
      | Psig_jkind _ -> false
      | _ -> true
    ) s.psg_items
  in
  { s with psg_items = items }

let structure s =
  List.filter (fun si ->
    match si.pstr_desc with
    | Pstr_jkind _ -> false
    | _ -> true
  ) s
