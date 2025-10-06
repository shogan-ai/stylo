(** {1 Flattening to a sequence of tokens } *)

let is_source_token_of_attached_docstring (tok : Tokens.elt) =
  match tok.desc with
  | Comment _ ->
    begin match Hashtbl.find Docstrings.docs_attr_tokens tok.pos with
    | ds -> not (List.memq tok ds)
    | exception Not_found -> false
    end
  | _ -> false

let rec combine_children (top : Tokens.seq) children =
  match top, children with
  | [], [] -> [] (* done *)
  | [], _ -> assert false (* children left behind *)
  | { desc = Child_node; _ } :: _, [] -> assert false (* missing child *)
  | { desc = Child_node; _ } :: tokens, child :: children ->
    child @ combine_children tokens children
  | tok :: tokens, _ ->
    if is_source_token_of_attached_docstring tok then
      (* When attaching a docstring an attribute is synthezised. When that
         happens, we assign a fresh [Comment] to its [pattr_tokens] field.

         We need to do that because docstrings are fetched from outside a symbol
         location in the parser, so we in general won't have access to the
         source token.
         However, that means we now have two tokens for a given docstring,
         that's one too many.

         Here, we "drop" source tokens which correspond to a docstring (even
         though it might have been assigned to a different node). *)
      combine_children tokens children
    else
      tok :: combine_children tokens children

(* dbg printing *)
let pp_child ppf =
  let open Format in
  fprintf ppf "@[<hov 2>%a@]" Tokens.pp_seq

let pp_children =
  let open Format in
  pp_print_list
    ~pp_sep:(fun ppf () -> fprintf ppf ";@ ")
    pp_child

let combine_children ~loc top children =
  try [combine_children top children]
  with Assert_failure _ as exn ->
    let start_pos = loc.Location.loc_start in
    let stop_pos = loc.Location.loc_end in
    dprintf
      "@[<h>loc:@ %d:%d - %d:%d@]@\n\
       tokens:@[<hov 2>@ %a@]@\n\
       children:@[<v 2>@ {%a}@]@."
      start_pos.pos_lnum (start_pos.pos_cnum - start_pos.pos_bol)
      stop_pos.pos_lnum (stop_pos.pos_cnum - stop_pos.pos_bol)
      Tokens.pp_seq top
      pp_children children;
    raise exn

class to_tokens = object
  method zero : Tokens.seq list = []
  method plus = (@)

  inherit [_] Parsetree.reduce as super

  method! visit_longident env l =
    let sub_tokens = super#visit_longident env l in
    let node_toks = l.tokens in
    combine_children ~loc:Location.none node_toks sub_tokens

  method! visit_attribute env a =
    match a.attr_name.txt with
    | ["ocaml"; ("doc"|"text")] -> [a.attr_tokens]
    | _ ->
      let sub_tokens = super#visit_attribute env a in
      combine_children ~loc:a.attr_loc a.attr_tokens sub_tokens

  method! visit_extension env e =
    let sub_tokens = super#visit_extension env e in
    let (name,_,node_toks) = e in
    combine_children ~loc:name.loc node_toks sub_tokens

  method! visit_arrow_arg env aa =
    let sub_tokens = super#visit_arrow_arg env aa in
    let node_toks = aa.aa_tokens in
    combine_children ~loc:aa.aa_loc node_toks sub_tokens

  method! visit_core_type env ct =
    let sub_tokens = super#visit_core_type env ct in
    let node_toks = ct.ptyp_tokens in
    combine_children ~loc:ct.ptyp_loc node_toks sub_tokens

  method! visit_row_field env p =
    let sub_tokens = super#visit_row_field env p in
    let node_toks = p.prf_tokens in
    combine_children ~loc:p.prf_loc node_toks sub_tokens

  method! visit_pattern env p =
    let sub_tokens = super#visit_pattern env p in
    let node_toks = p.ppat_tokens in
    combine_children ~loc:p.ppat_loc node_toks sub_tokens

  method! visit_expression env e =
    let sub_tokens = super#visit_expression env e in
    let node_toks = e.pexp_tokens in
    combine_children ~loc:e.pexp_loc node_toks sub_tokens

  method! visit_argument visit_elt env a =
    let sub_tokens = super#visit_argument visit_elt env a in
    let node_toks = a.parg_tokens in
    combine_children ~loc:Location.none node_toks sub_tokens

  method! visit_function_body env fb =
    let sub_tokens = super#visit_function_body env fb in
    let node_toks = fb.pfb_tokens in
    combine_children ~loc:fb.pfb_loc node_toks sub_tokens

  method! visit_value_description env vd =
    let sub_tokens = super#visit_value_description env vd in
    let node_toks = vd.pval_tokens in
    combine_children ~loc:vd.pval_loc node_toks sub_tokens

  method! visit_ptype_param env tp =
    let sub_tokens = super#visit_ptype_param env tp in
    let node_toks = tp.ptp_tokens in
    combine_children ~loc:Location.none node_toks sub_tokens

  method! visit_type_declaration env td =
    let sub_tokens = super#visit_type_declaration env td in
    let node_toks = td.ptype_tokens in
    combine_children ~loc:td.ptype_loc node_toks sub_tokens

  method! visit_label_declaration env ld =
    let sub_tokens = super#visit_label_declaration env ld in
    let node_toks = ld.pld_tokens in
    combine_children ~loc:ld.pld_loc node_toks sub_tokens

  method! visit_constructor_declaration env cd =
    let sub_tokens = super#visit_constructor_declaration env cd in
    let node_toks = cd.pcd_tokens in
    combine_children ~loc:cd.pcd_loc node_toks sub_tokens

  method! visit_type_extension env te =
    let sub_tokens = super#visit_type_extension env te in
    let node_toks = te.ptyext_tokens in
    combine_children ~loc:te.ptyext_loc node_toks sub_tokens

  method! visit_extension_constructor env ec =
    let sub_tokens = super#visit_extension_constructor env ec in
    let node_toks = ec.pext_tokens in
    combine_children ~loc:ec.pext_loc node_toks sub_tokens

  method! visit_type_exception env exn =
    let sub_tokens = super#visit_type_exception env exn in
    let node_toks = exn.ptyexn_tokens in
    combine_children ~loc:exn.ptyexn_loc node_toks sub_tokens

  method! visit_class_type env cty =
    let sub_tokens = super#visit_class_type env cty in
    let node_toks = cty.pcty_tokens in
    combine_children ~loc:cty.pcty_loc node_toks sub_tokens

  method! visit_class_type_field env ctf =
    let sub_tokens = super#visit_class_type_field env ctf in
    let node_toks = ctf.pctf_tokens in
    combine_children ~loc:ctf.pctf_loc node_toks sub_tokens

  method! visit_class_infos visit_elt env ci =
    let sub_tokens = super#visit_class_infos visit_elt env ci in
    let node_toks = ci.pci_tokens in
    combine_children ~loc:ci.pci_loc node_toks sub_tokens

  method! visit_class_field env cf =
    let sub_tokens = super#visit_class_field env cf in
    let node_toks = cf.pcf_tokens in
    combine_children ~loc:cf.pcf_loc node_toks sub_tokens

  method! visit_module_type env mty =
    let sub_tokens = super#visit_module_type env mty in
    let node_toks = mty.pmty_tokens in
    combine_children ~loc:mty.pmty_loc node_toks sub_tokens

  method! visit_signature env sg =
    let sub_tokens = super#visit_signature env sg in
    let node_toks = sg.psg_tokens in
    combine_children ~loc:sg.psg_loc node_toks sub_tokens

  method! visit_signature_item env si =
    let sub_tokens = super#visit_signature_item env si in
    let node_toks = si.psig_tokens in
    combine_children ~loc:si.psig_loc node_toks sub_tokens

  method! visit_module_declaration env md =
    let sub_tokens = super#visit_module_declaration env md in
    let node_toks = md.pmd_tokens in
    combine_children ~loc:md.pmd_loc node_toks sub_tokens

  method! visit_module_substitution env ms =
    let sub_tokens = super#visit_module_substitution env ms in
    let node_toks = ms.pms_tokens in
    combine_children ~loc:ms.pms_loc node_toks sub_tokens

  method! visit_module_type_declaration env mtd =
    let sub_tokens = super#visit_module_type_declaration env mtd in
    let node_toks = mtd.pmtd_tokens in
    combine_children ~loc:mtd.pmtd_loc node_toks sub_tokens

  method! visit_open_infos visit_elt env opn =
    let sub_tokens = super#visit_open_infos visit_elt env opn in
    let node_toks = opn.popen_tokens in
    combine_children ~loc:opn.popen_loc node_toks sub_tokens

  method! visit_include_infos visit_elt env incl =
    let sub_tokens = super#visit_include_infos visit_elt env incl in
    let node_toks = incl.pincl_tokens in
    combine_children ~loc:incl.pincl_loc node_toks sub_tokens

  method! visit_module_expr env me =
    let sub_tokens = super#visit_module_expr env me in
    let node_toks = me.pmod_tokens in
    combine_children ~loc:me.pmod_loc node_toks sub_tokens

  method! visit_structure env s =
    let sub_tokens = super#visit_structure env s in
    let node_toks = snd s in
    combine_children ~loc:Location.none node_toks sub_tokens

  method! visit_structure_item env si =
    let sub_tokens = super#visit_structure_item env si in
    let node_toks = si.pstr_tokens in
    combine_children ~loc:si.pstr_loc node_toks sub_tokens

  method! visit_value_binding env vb =
    let sub_tokens = super#visit_value_binding env vb in
    let node_toks = vb.pvb_tokens in
    combine_children ~loc:vb.pvb_loc node_toks sub_tokens

  method! visit_module_binding env mb =
    let sub_tokens = super#visit_module_binding env mb in
    let node_toks = mb.pmb_tokens in
    combine_children ~loc:mb.pmb_loc node_toks sub_tokens
end

