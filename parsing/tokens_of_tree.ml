(** {1 Flattening to a sequence of tokens } *)

let rec combine_children (top : Tokens.seq) children =
  match top, children with
  | [], [] -> [] (* done *)
  | [], _ -> assert false (* children left behind *)
  | { desc = Child_node; _ } :: _, [] -> assert false (* missing child *)
  | { desc = Child_node; _ } :: tokens, child :: children ->
    child @ combine_children tokens children
  | tok :: tokens, _ ->
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

(* TODO: should be generated. *)
let tokenizer =
  let super = Ast_reduce.mk_default_reducer [] (@) in
  let reduce_longident reducer env l =
    let sub_tokens = super.longident reducer env l in
    let node_toks = l.tokens in
    combine_children ~loc:Location.none node_toks sub_tokens
  in
  let reduce_attribute reducer env (a : Parsetree.attribute) =
    let sub_tokens = super.attribute reducer env a in
    combine_children ~loc:a.attr_loc a.attr_tokens sub_tokens
  in
  let reduce_extension reducer env e =
    let sub_tokens = super.extension reducer env e in
    let (name,_,node_toks) = e in
    combine_children ~loc:name.loc node_toks sub_tokens
  in
  let reduce_arrow_arg reducer env aa =
    let sub_tokens = super.arrow_arg reducer env aa in
    let node_toks = aa.aa_tokens in
    combine_children ~loc:aa.aa_loc node_toks sub_tokens
  in
  let reduce_core_type reducer env ct =
    let sub_tokens = super.core_type reducer env ct in
    let node_toks = ct.ptyp_tokens in
    combine_children ~loc:ct.ptyp_loc node_toks sub_tokens
  in
  let reduce_row_field reducer env p =
    let sub_tokens = super.row_field reducer env p in
    let node_toks = p.prf_tokens in
    combine_children ~loc:p.prf_loc node_toks sub_tokens
  in
  let reduce_object_field reducer env p =
    let sub_tokens = super.object_field reducer env p in
    let node_toks = p.pof_tokens in
    combine_children ~loc:p.pof_loc node_toks sub_tokens
  in
  let reduce_pattern reducer env p =
    let sub_tokens = super.pattern reducer env p in
    let node_toks = p.ppat_tokens in
    combine_children ~loc:p.ppat_loc node_toks sub_tokens
  in
  let reduce_expression reducer env e =
    let sub_tokens = super.expression reducer env e in
    let node_toks = e.pexp_tokens in
    combine_children ~loc:e.pexp_loc node_toks sub_tokens
  in
  let reduce_case reducer env c =
    let sub_tokens = super.case reducer env c in
    let node_toks = c.pc_tokens in
    combine_children ~loc:Location.none node_toks sub_tokens
  in
  let reduce_argument reducer visit_elt env a =
    let sub_tokens = super.argument reducer visit_elt env a in
    let node_toks = a.parg_tokens in
    combine_children ~loc:Location.none node_toks sub_tokens
  in
  let reduce_function_body reducer env fb =
    let sub_tokens = super.function_body reducer env fb in
    let node_toks = fb.pfb_tokens in
    combine_children ~loc:fb.pfb_loc node_toks sub_tokens
  in
  let reduce_value_description reducer env vd =
    let sub_tokens = super.value_description reducer env vd in
    let node_toks = vd.pval_tokens in
    combine_children ~loc:vd.pval_loc node_toks sub_tokens
  in
  let reduce_ptype_param reducer env tp =
    let sub_tokens = super.ptype_param reducer env tp in
    let node_toks = tp.ptp_tokens in
    combine_children ~loc:Location.none node_toks sub_tokens
  in
  let reduce_type_declaration reducer env td =
    let sub_tokens = super.type_declaration reducer env td in
    let node_toks = td.ptype_tokens in
    combine_children ~loc:td.ptype_loc node_toks sub_tokens
  in
  let reduce_label_declaration reducer env ld =
    let sub_tokens = super.label_declaration reducer env ld in
    let node_toks = ld.pld_tokens in
    combine_children ~loc:ld.pld_loc node_toks sub_tokens
  in
  let reduce_constructor_declaration reducer env cd =
    let sub_tokens = super.constructor_declaration reducer env cd in
    let node_toks = cd.pcd_tokens in
    combine_children ~loc:cd.pcd_loc node_toks sub_tokens
  in
  let reduce_type_extension reducer env te =
    let sub_tokens = super.type_extension reducer env te in
    let node_toks = te.ptyext_tokens in
    combine_children ~loc:te.ptyext_loc node_toks sub_tokens
  in
  let reduce_extension_constructor reducer env ec =
    let sub_tokens = super.extension_constructor reducer env ec in
    let node_toks = ec.pext_tokens in
    combine_children ~loc:ec.pext_loc node_toks sub_tokens
  in
  let reduce_type_exception reducer env exn =
    let sub_tokens = super.type_exception reducer env exn in
    let node_toks = exn.ptyexn_tokens in
    combine_children ~loc:exn.ptyexn_loc node_toks sub_tokens
  in
  let reduce_class_type reducer env cty =
    let sub_tokens = super.class_type reducer env cty in
    let node_toks = cty.pcty_tokens in
    combine_children ~loc:cty.pcty_loc node_toks sub_tokens
  in
  let reduce_class_type_field reducer env ctf =
    let sub_tokens = super.class_type_field reducer env ctf in
    let node_toks = ctf.pctf_tokens in
    combine_children ~loc:ctf.pctf_loc node_toks sub_tokens
  in
  let reduce_class_infos reducer visit_elt env ci =
    let sub_tokens = super.class_infos reducer visit_elt env ci in
    let node_toks = ci.pci_tokens in
    combine_children ~loc:ci.pci_loc node_toks sub_tokens
  in
  let reduce_class_field reducer env cf =
    let sub_tokens = super.class_field reducer env cf in
    let node_toks = cf.pcf_tokens in
    combine_children ~loc:cf.pcf_loc node_toks sub_tokens
  in
  let reduce_module_type reducer env mty =
    let sub_tokens = super.module_type reducer env mty in
    let node_toks = mty.pmty_tokens in
    combine_children ~loc:mty.pmty_loc node_toks sub_tokens
  in
  let reduce_signature reducer env sg =
    let sub_tokens = super.signature reducer env sg in
    let node_toks = sg.psg_tokens in
    combine_children ~loc:sg.psg_loc node_toks sub_tokens
  in
  let reduce_signature_item reducer env si =
    let sub_tokens = super.signature_item reducer env si in
    let node_toks = si.psig_tokens in
    combine_children ~loc:si.psig_loc node_toks sub_tokens
  in
  let reduce_module_declaration reducer env md =
    let sub_tokens = super.module_declaration reducer env md in
    let node_toks = md.pmd_tokens in
    combine_children ~loc:md.pmd_loc node_toks sub_tokens
  in
  let reduce_module_substitution reducer env ms =
    let sub_tokens = super.module_substitution reducer env ms in
    let node_toks = ms.pms_tokens in
    combine_children ~loc:ms.pms_loc node_toks sub_tokens
  in
  let reduce_module_type_declaration reducer env mtd =
    let sub_tokens = super.module_type_declaration reducer env mtd in
    let node_toks = mtd.pmtd_tokens in
    combine_children ~loc:mtd.pmtd_loc node_toks sub_tokens
  in
  let reduce_open_infos reducer visit_elt env opn =
    let sub_tokens = super.open_infos reducer visit_elt env opn in
    let node_toks = opn.popen_tokens in
    combine_children ~loc:opn.popen_loc node_toks sub_tokens
  in
  let reduce_include_infos reducer visit_elt env incl =
    let sub_tokens = super.include_infos reducer visit_elt env incl in
    let node_toks = incl.pincl_tokens in
    combine_children ~loc:incl.pincl_loc node_toks sub_tokens
  in
  let reduce_with_constraint reducer env wc =
    let sub_tokens = super.with_constraint reducer env wc in
    let node_toks = wc.wc_tokens in
    combine_children ~loc:wc.wc_loc node_toks sub_tokens
  in
  let reduce_module_expr reducer env me =
    let sub_tokens = super.module_expr reducer env me in
    let node_toks = me.pmod_tokens in
    combine_children ~loc:me.pmod_loc node_toks sub_tokens
  in
  let reduce_structure reducer env s =
    let sub_tokens = super.structure reducer env s in
    let node_toks = snd s in
    combine_children ~loc:Location.none node_toks sub_tokens
  in
  let reduce_structure_item reducer env si =
    let sub_tokens = super.structure_item reducer env si in
    let node_toks = si.pstr_tokens in
    combine_children ~loc:si.pstr_loc node_toks sub_tokens
  in
  let reduce_value_binding reducer env vb =
    let sub_tokens = super.value_binding reducer env vb in
    let node_toks = vb.pvb_tokens in
    combine_children ~loc:vb.pvb_loc node_toks sub_tokens
  in
  let reduce_module_binding reducer env mb =
    let sub_tokens = super.module_binding reducer env mb in
    let node_toks = mb.pmb_tokens in
    combine_children ~loc:mb.pmb_loc node_toks sub_tokens
  in
  { super with
    longident = reduce_longident
  ; attribute = reduce_attribute
  ; extension = reduce_extension
  ; arrow_arg = reduce_arrow_arg
  ; core_type = reduce_core_type
  ; row_field = reduce_row_field
  ; object_field = reduce_object_field
  ; pattern = reduce_pattern
  ; expression = reduce_expression
  ; case = reduce_case
  ; argument = reduce_argument
  ; function_body = reduce_function_body
  ; value_description = reduce_value_description
  ; ptype_param = reduce_ptype_param
  ; type_declaration = reduce_type_declaration
  ; label_declaration = reduce_label_declaration
  ; constructor_declaration = reduce_constructor_declaration
  ; type_extension = reduce_type_extension
  ; extension_constructor = reduce_extension_constructor
  ; type_exception = reduce_type_exception
  ; class_type = reduce_class_type
  ; class_type_field = reduce_class_type_field
  ; class_infos = reduce_class_infos
  ; class_field = reduce_class_field
  ; module_type = reduce_module_type
  ; signature = reduce_signature
  ; signature_item = reduce_signature_item
  ; module_declaration = reduce_module_declaration
  ; module_substitution = reduce_module_substitution
  ; module_type_declaration = reduce_module_type_declaration
  ; open_infos = reduce_open_infos
  ; include_infos = reduce_include_infos
  ; with_constraint = reduce_with_constraint
  ; module_expr = reduce_module_expr
  ; structure = reduce_structure
  ; structure_item = reduce_structure_item
  ; value_binding = reduce_value_binding
  ; module_binding = reduce_module_binding
  }

let structure str =
  tokenizer.structure tokenizer () str
  |> List.flatten

let signature sg =
  tokenizer.signature tokenizer () sg
  |> List.flatten
