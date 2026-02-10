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
let tokenizer = object
  inherit [Tokens.seq list] Traversals.lift as super

  (* The actual tokens. *)
  method tokens list = [ list ]

  (* Leaf types. *)
  method bool _ = []
  method char _ = []
  method int _ = []
  method string _ = []
  method position _ = []

  (* Polymorphic types. *)
  method ref f x = f !x
  method tuple = List.concat
  method record alist = List.concat_map snd alist
  method option f = function
    | None -> []
    | Some x -> f x
  method list f = List.concat_map f
  method constr _ = List.concat

  (* Replacing [Child_node]s by actual children *)
  method! longident l =
    let sub_tokens = super#longident l in
    let node_toks = l.tokens in
    combine_children ~loc:Location.none node_toks sub_tokens

  method! attribute (a : Parsetree.attribute) =
    let sub_tokens = super#attribute a in
    combine_children ~loc:a.attr_loc a.attr_tokens sub_tokens

  method! extension e =
    let sub_tokens = super#extension e in
    let (name,_,node_toks) = e in
    combine_children ~loc:name.loc node_toks sub_tokens

  method! arrow_arg aa =
    let sub_tokens = super#arrow_arg aa in
    let node_toks = aa.aa_tokens in
    combine_children ~loc:aa.aa_loc node_toks sub_tokens

  method! core_type ct =
    let sub_tokens = super#core_type ct in
    let node_toks = ct.ptyp_tokens in
    combine_children ~loc:ct.ptyp_loc node_toks sub_tokens

  method! row_field p =
    let sub_tokens = super#row_field p in
    let node_toks = p.prf_tokens in
    combine_children ~loc:p.prf_loc node_toks sub_tokens

  method! object_field p =
    let sub_tokens = super#object_field p in
    let node_toks = p.pof_tokens in
    combine_children ~loc:p.pof_loc node_toks sub_tokens

  method! pattern p =
    let sub_tokens = super#pattern p in
    let node_toks = p.ppat_tokens in
    combine_children ~loc:p.ppat_loc node_toks sub_tokens

  method! expression e =
    let sub_tokens = super#expression e in
    let node_toks = e.pexp_tokens in
    combine_children ~loc:e.pexp_loc node_toks sub_tokens

  method! case c =
    let sub_tokens = super#case c in
    let node_toks = c.pc_tokens in
    combine_children ~loc:Location.none node_toks sub_tokens

  method! argument visit_elt a =
    let sub_tokens = super#argument visit_elt a in
    let node_toks = a.parg_tokens in
    combine_children ~loc:Location.none node_toks sub_tokens

  method! function_body fb =
    let sub_tokens = super#function_body fb in
    let node_toks = fb.pfb_tokens in
    combine_children ~loc:fb.pfb_loc node_toks sub_tokens

  method! comprehension_clause_binding cb =
    let sub_tokens = super#comprehension_clause_binding cb in
    let node_toks = cb.pcomp_cb_tokens in
    combine_children ~loc:Location.none node_toks sub_tokens

  method! comprehension c =
    let sub_tokens = super#comprehension c in
    let node_toks = c.pcomp_tokens in
    combine_children ~loc:Location.none node_toks sub_tokens

  method! value_description vd =
    let sub_tokens = super#value_description vd in
    let node_toks = vd.pval_tokens in
    combine_children ~loc:vd.pval_loc node_toks sub_tokens

  method! ptype_param tp =
    let sub_tokens = super#ptype_param tp in
    let node_toks = tp.ptp_tokens in
    combine_children ~loc:Location.none node_toks sub_tokens

  method! type_declaration td =
    let sub_tokens = super#type_declaration td in
    let node_toks = td.ptype_tokens in
    combine_children ~loc:td.ptype_loc node_toks sub_tokens

  method! label_declaration ld =
    let sub_tokens = super#label_declaration ld in
    let node_toks = ld.pld_tokens in
    combine_children ~loc:ld.pld_loc node_toks sub_tokens

  method! constructor_declaration cd =
    let sub_tokens = super#constructor_declaration cd in
    let node_toks = cd.pcd_tokens in
    combine_children ~loc:cd.pcd_loc node_toks sub_tokens

  method! type_extension te =
    let sub_tokens = super#type_extension te in
    let node_toks = te.ptyext_tokens in
    combine_children ~loc:te.ptyext_loc node_toks sub_tokens

  method! extension_constructor ec =
    let sub_tokens = super#extension_constructor ec in
    let node_toks = ec.pext_tokens in
    combine_children ~loc:ec.pext_loc node_toks sub_tokens

  method! type_exception exn =
    let sub_tokens = super#type_exception exn in
    let node_toks = exn.ptyexn_tokens in
    combine_children ~loc:exn.ptyexn_loc node_toks sub_tokens

  method! class_type cty =
    let sub_tokens = super#class_type cty in
    let node_toks = cty.pcty_tokens in
    combine_children ~loc:cty.pcty_loc node_toks sub_tokens

  method! class_type_field ctf =
    let sub_tokens = super#class_type_field ctf in
    let node_toks = ctf.pctf_tokens in
    combine_children ~loc:ctf.pctf_loc node_toks sub_tokens

  method! class_infos visit_elt ci =
    let sub_tokens = super#class_infos visit_elt ci in
    let node_toks = ci.pci_tokens in
    combine_children ~loc:ci.pci_loc node_toks sub_tokens

  method! class_field cf =
    let sub_tokens = super#class_field cf in
    let node_toks = cf.pcf_tokens in
    combine_children ~loc:cf.pcf_loc node_toks sub_tokens

  method! module_type mty =
    let sub_tokens = super#module_type mty in
    let node_toks = mty.pmty_tokens in
    combine_children ~loc:mty.pmty_loc node_toks sub_tokens

  method! signature sg =
    let sub_tokens = super#signature sg in
    let node_toks = sg.psg_tokens in
    combine_children ~loc:sg.psg_loc node_toks sub_tokens

  method! signature_item si =
    let sub_tokens = super#signature_item si in
    let node_toks = si.psig_tokens in
    combine_children ~loc:si.psig_loc node_toks sub_tokens

  method! module_declaration md =
    let sub_tokens = super#module_declaration md in
    let node_toks = md.pmd_tokens in
    combine_children ~loc:md.pmd_loc node_toks sub_tokens

  method! module_substitution ms =
    let sub_tokens = super#module_substitution ms in
    let node_toks = ms.pms_tokens in
    combine_children ~loc:ms.pms_loc node_toks sub_tokens

  method! module_type_declaration mtd =
    let sub_tokens = super#module_type_declaration mtd in
    let node_toks = mtd.pmtd_tokens in
    combine_children ~loc:mtd.pmtd_loc node_toks sub_tokens

  method! open_infos visit_elt opn =
    let sub_tokens = super#open_infos visit_elt opn in
    let node_toks = opn.popen_tokens in
    combine_children ~loc:opn.popen_loc node_toks sub_tokens

  method! include_infos visit_elt incl =
    let sub_tokens = super#include_infos visit_elt incl in
    let node_toks = incl.pincl_tokens in
    combine_children ~loc:incl.pincl_loc node_toks sub_tokens

  method! with_constraint wc =
    let sub_tokens = super#with_constraint wc in
    let node_toks = wc.wc_tokens in
    combine_children ~loc:wc.wc_loc node_toks sub_tokens

  method! module_expr me =
    let sub_tokens = super#module_expr me in
    let node_toks = me.pmod_tokens in
    combine_children ~loc:me.pmod_loc node_toks sub_tokens

  method! structure s =
    let sub_tokens = super#structure s in
    let node_toks = snd s in
    combine_children ~loc:Location.none node_toks sub_tokens

  method! structure_item si =
    let sub_tokens = super#structure_item si in
    let node_toks = si.pstr_tokens in
    combine_children ~loc:si.pstr_loc node_toks sub_tokens

  method! value_binding vb =
    let sub_tokens = super#value_binding vb in
    let node_toks = vb.pvb_tokens in
    combine_children ~loc:vb.pvb_loc node_toks sub_tokens

  method! module_binding mb =
    let sub_tokens = super#module_binding mb in
    let node_toks = mb.pmb_tokens in
    combine_children ~loc:mb.pmb_loc node_toks sub_tokens

  method! jkind_annotation jk =
    let sub_tokens = super#jkind_annotation jk in
    let node_toks = jk.pjkind_tokens in
    combine_children ~loc:Location.none node_toks sub_tokens
end

let structure str =
  tokenizer#structure str
  |> List.flatten

let signature sg =
  tokenizer#signature sg
  |> List.flatten
