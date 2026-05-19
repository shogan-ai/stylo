(** {1 Flattening to a sequence of tokens } *)

type tokens_from_subtree = {
  origin: string;
  tokens: Tokens.seq;
}

module Error = struct
  type context =
    { node_kind: string
    ; pos: Lexing.position }

  let pp_context ppf { node_kind; pos } =
    Format.fprintf ppf "File %s, on %s at line %d column %d:"
      pos.pos_fname
      node_kind
      pos.pos_lnum (pos.pos_cnum - pos.pos_bol)

  type t = [
    | `CST_tokens_mismatch of context * Tokens.seq * tokens_from_subtree list
  ]

  let pp_bounded_tokens ~limit ppf seq =
    let did_truncate, seq =
      if List.compare_length_with seq limit > 0
      then true, Std.List.take limit seq
      else false, seq
    in
    let open Format in
    fprintf ppf "@[<hov>%a%a@]"
      (pp_print_list ~pp_sep:pp_print_space Tokens.pp_elt) seq
      (fun ppf () ->
         (if did_truncate then fprintf else ifprintf) ppf "@ ...") ()

  let pp_tokens_from_subtree ppf t =
    Format.fprintf ppf "@[<hov 4>%a@ (for %S subtree)@]"
      (pp_bounded_tokens ~limit:5) t.tokens t.origin

  let pp_subtrees ~limit ppf l =
    let did_truncate, l =
      if List.compare_length_with l limit > 0
      then true, Std.List.take limit l
      else false, l
    in
    let open Format in
    pp_print_list ~pp_sep:pp_print_space
      (fun ppf -> fprintf ppf "- %a" pp_tokens_from_subtree)
      ppf l;
    if did_truncate then fprintf ppf "@ - ..."

  let pp ppf : t -> unit = function
    | `CST_tokens_mismatch (ctxt, node_tokens, subtokens) ->
      let expected_children =
        List.fold_left (fun nb t -> if Tokens.is_child t then nb + 1 else nb)
          0 node_tokens
      in
      let limit = 10 in
      Format.fprintf ppf
        "@[<v>%a:@ @[<hov>Mismatch between@ this node's tokens skeleton@ \
         and the tokens retrieved@ from the subtrees.@]@;\
         @[<hov 2>Current node tokens skeleton@ (%d expected subtrees):@;%a@]@;\
         @[<hov 2>Tokens sequences@ for %d subtrees:@;@[<v>%a@]@]@."
        pp_context ctxt
        expected_children
        (pp_bounded_tokens ~limit) node_tokens
        (List.length subtokens)
        (pp_subtrees ~limit) subtokens
end

exception Abort
exception Error of Error.t

let rec combine_children (top : Tokens.seq) children =
  match top, children with
  | [], [] -> [] (* done *)
  | [], _ -> raise Abort
  | { desc = Child_node; _ } :: _, [] -> raise Abort
  | { desc = Child_node; _ } :: tokens, child :: children ->
    child.tokens @ combine_children tokens children
  | tok :: tokens, _ ->
    tok :: combine_children tokens children

let combine_children node_kind ~loc top children =
  try [{ origin = node_kind; tokens = combine_children top children }]
  with Abort ->
    let ctxt = { Error.node_kind ; pos = loc.Location.loc_start } in
    raise (Error (`CST_tokens_mismatch (ctxt, top, children)))

(* TODO: should be generated. *)
let tokenizer = object
  inherit [tokens_from_subtree list] Traversals.lift as super

  (* The actual tokens. *)
  method tokens list = [ list ]

  (* Ppxlib.lift internal structure *)
  method record alist = List.concat_map snd alist
  method constr _ = List.concat

  (* Leaf types. *)
  method bool _ = []
  method char _ = []
  method int _ = []
  method string _ = []
  method position _ = []

  (* Polymorphic types. *)
  method ref f x = f !x
  method tuple = List.concat
  method option f = function
    | None -> []
    | Some x -> f x
  method list f = List.concat_map f
  (* Replacing [Child_node]s by actual children *)
  method! longident l =
    let sub_tokens = super#longident l in
    let node_toks = l.tokens in
    combine_children "longident" ~loc:Location.none node_toks sub_tokens

  method! modes = function
    | No_modes -> []
    | Modes m as modes ->
      let sub_tokens = super#modes modes in
      combine_children "modes" ~loc:m.loc m.tokens sub_tokens

  method! modalities = function
    | No_modalities -> []
    | Modalities m as modalities ->
      let sub_tokens = super#modalities modalities in
      combine_children "modalities" ~loc:m.loc m.tokens sub_tokens

  method! attribute a =
    let sub_tokens = super#attribute a in
    combine_children "attribute" ~loc:a.attr_loc a.attr_tokens sub_tokens

  method! attributes = function
    | No_attributes -> []
    | Attributes a as attributes ->
      let sub_tokens = super#attributes attributes in
      combine_children "attributes" ~loc:a.loc a.tokens sub_tokens

  method! extension e =
    let sub_tokens = super#extension e in
    let (name,_,node_toks) = e in
    combine_children "extension" ~loc:name.loc node_toks sub_tokens

  method! arrow_arg aa =
    let sub_tokens = super#arrow_arg aa in
    let node_toks = aa.aa_tokens in
    combine_children "arrow_arg" ~loc:aa.aa_loc node_toks sub_tokens

  method! core_type ct =
    let sub_tokens = super#core_type ct in
    let node_toks = ct.ptyp_tokens in
    combine_children "core_type" ~loc:ct.ptyp_loc node_toks sub_tokens

  method! bound_ty_var bv =
    let sub_tokens = super#bound_ty_var bv in
    let node_toks = bv.pbtv_tokens in
    combine_children "bound_ty_var" ~loc:bv.pbtv_loc node_toks sub_tokens

  method! row_field p =
    let sub_tokens = super#row_field p in
    let node_toks = p.prf_tokens in
    combine_children "row_field" ~loc:p.prf_loc node_toks sub_tokens

  method! object_field p =
    let sub_tokens = super#object_field p in
    let node_toks = p.pof_tokens in
    combine_children "object_field" ~loc:p.pof_loc node_toks sub_tokens

  method! pattern p =
    let sub_tokens = super#pattern p in
    let node_toks = p.ppat_tokens in
    combine_children "pattern" ~loc:p.ppat_loc node_toks sub_tokens

  method! expression e =
    let sub_tokens = super#expression e in
    let node_toks = e.pexp_tokens in
    combine_children "expression" ~loc:e.pexp_loc node_toks sub_tokens

  method! case c =
    let sub_tokens = super#case c in
    let node_toks = c.pc_tokens in
    combine_children "case" ~loc:c.pc_loc node_toks sub_tokens

  method! argument visit_elt a =
    let sub_tokens = super#argument visit_elt a in
    let node_toks = a.parg_tokens in
    combine_children "argument" ~loc:a.parg_loc node_toks sub_tokens

  method! function_body fb =
    let sub_tokens = super#function_body fb in
    let node_toks = fb.pfb_tokens in
    combine_children "function_body" ~loc:fb.pfb_loc node_toks sub_tokens

  method! comprehension_clause_binding cb =
    let sub_tokens = super#comprehension_clause_binding cb in
    let node_toks = cb.pcomp_cb_tokens in
    combine_children "comprehension_clause_binding"
      ~loc:cb.pcomp_cb_loc node_toks sub_tokens

  method! comprehension c =
    let sub_tokens = super#comprehension c in
    let node_toks = c.pcomp_tokens in
    combine_children "comprehension" ~loc:c.pcomp_loc node_toks sub_tokens

  method! value_description vd =
    let sub_tokens = super#value_description vd in
    let node_toks = vd.pval_tokens in
    combine_children "value_description" ~loc:vd.pval_loc node_toks sub_tokens

  method! ptype_param tp =
    let sub_tokens = super#ptype_param tp in
    let node_toks = tp.ptp_tokens in
    combine_children "ptype_param" ~loc:Location.none node_toks sub_tokens

  method! type_declaration td =
    let sub_tokens = super#type_declaration td in
    let node_toks = td.ptype_tokens in
    combine_children "type_declaration" ~loc:td.ptype_loc node_toks sub_tokens

  method! label_declaration ld =
    let sub_tokens = super#label_declaration ld in
    let node_toks = ld.pld_tokens in
    combine_children "label_declaration" ~loc:ld.pld_loc node_toks sub_tokens

  method! constructor_declaration cd =
    let sub_tokens = super#constructor_declaration cd in
    let node_toks = cd.pcd_tokens in
    combine_children "constructor_declaration" ~loc:cd.pcd_loc node_toks sub_tokens

  method! type_extension te =
    let sub_tokens = super#type_extension te in
    let node_toks = te.ptyext_tokens in
    combine_children "type_extension" ~loc:te.ptyext_loc node_toks sub_tokens

  method! extension_constructor ec =
    let sub_tokens = super#extension_constructor ec in
    let node_toks = ec.pext_tokens in
    combine_children "extension_constructor" ~loc:ec.pext_loc node_toks sub_tokens

  method! type_exception exn =
    let sub_tokens = super#type_exception exn in
    let node_toks = exn.ptyexn_tokens in
    combine_children "type_exception" ~loc:exn.ptyexn_loc node_toks sub_tokens

  method! class_type cty =
    let sub_tokens = super#class_type cty in
    let node_toks = cty.pcty_tokens in
    combine_children "class_type" ~loc:cty.pcty_loc node_toks sub_tokens

  method! class_type_field ctf =
    let sub_tokens = super#class_type_field ctf in
    let node_toks = ctf.pctf_tokens in
    combine_children "class_type_field" ~loc:ctf.pctf_loc node_toks sub_tokens

  method! class_infos visit_elt ci =
    let sub_tokens = super#class_infos visit_elt ci in
    let node_toks = ci.pci_tokens in
    combine_children "class_infos" ~loc:ci.pci_loc node_toks sub_tokens

  method! class_expr ce =
    let sub_tokens = super#class_expr ce in
    let node_toks = ce.pcl_tokens in
    combine_children "class_expr" ~loc:ce.pcl_loc node_toks sub_tokens

  method! class_field cf =
    let sub_tokens = super#class_field cf in
    let node_toks = cf.pcf_tokens in
    combine_children "class_field" ~loc:cf.pcf_loc node_toks sub_tokens

  method! module_type mty =
    let sub_tokens = super#module_type mty in
    let node_toks = mty.pmty_tokens in
    combine_children "module_type" ~loc:mty.pmty_loc node_toks sub_tokens

  method! signature sg =
    let sub_tokens = super#signature sg in
    let node_toks = sg.psg_tokens in
    combine_children "signature" ~loc:sg.psg_loc node_toks sub_tokens

  method! signature_item si =
    let sub_tokens = super#signature_item si in
    let node_toks = si.psig_tokens in
    combine_children "signature_item" ~loc:si.psig_loc node_toks sub_tokens

  method! module_declaration md =
    let sub_tokens = super#module_declaration md in
    let node_toks = md.pmd_tokens in
    combine_children "module_declaration" ~loc:md.pmd_loc node_toks sub_tokens

  method! module_substitution ms =
    let sub_tokens = super#module_substitution ms in
    let node_toks = ms.pms_tokens in
    combine_children "module_substitution" ~loc:ms.pms_loc node_toks sub_tokens

  method! module_type_declaration mtd =
    let sub_tokens = super#module_type_declaration mtd in
    let node_toks = mtd.pmtd_tokens in
    combine_children "module_type_declaration" ~loc:mtd.pmtd_loc node_toks sub_tokens

  method! open_infos visit_elt opn =
    let sub_tokens = super#open_infos visit_elt opn in
    let node_toks = opn.popen_tokens in
    combine_children "open_infos" ~loc:opn.popen_loc node_toks sub_tokens

  method! include_infos visit_elt incl =
    let sub_tokens = super#include_infos visit_elt incl in
    let node_toks = incl.pincl_tokens in
    combine_children "include_infos" ~loc:incl.pincl_loc node_toks sub_tokens

  method! with_constraint wc =
    let sub_tokens = super#with_constraint wc in
    let node_toks = wc.wc_tokens in
    combine_children "with_constraint" ~loc:wc.wc_loc node_toks sub_tokens

  method! module_expr me =
    let sub_tokens = super#module_expr me in
    let node_toks = me.pmod_tokens in
    combine_children "module_expr" ~loc:me.pmod_loc node_toks sub_tokens

  method! structure s =
    let sub_tokens = super#structure s in
    let node_toks = s.pst_tokens in
    combine_children "structure" ~loc:s.pst_loc node_toks sub_tokens

  method! structure_item si =
    let sub_tokens = super#structure_item si in
    let node_toks = si.pstr_tokens in
    combine_children "structure_item" ~loc:si.pstr_loc node_toks sub_tokens

  method! value_binding vb =
    let sub_tokens = super#value_binding vb in
    let node_toks = vb.pvb_tokens in
    combine_children "value_binding" ~loc:vb.pvb_loc node_toks sub_tokens

  method! module_binding mb =
    let sub_tokens = super#module_binding mb in
    let node_toks = mb.pmb_tokens in
    combine_children "module_binding" ~loc:mb.pmb_loc node_toks sub_tokens

  method! jkind_annotation jk =
    let sub_tokens = super#jkind_annotation jk in
    let node_toks = jk.pjka_tokens in
    combine_children "jkind_annotation" ~loc:jk.pjka_loc node_toks sub_tokens

  method! jkind_declaration jk =
    let sub_tokens = super#jkind_declaration jk in
    let node_toks = jk.pjkind_tokens in
    combine_children "jkind_declaration" ~loc:jk.pjkind_loc node_toks sub_tokens
end

let mk_error : Error.t -> _ = function
  | (`CST_tokens_mismatch _) as e -> Result.error e

let flatten_or_error
  : 'a. ('a -> tokens_from_subtree list) -> 'a -> (Tokens.seq, _) result = fun fn elt ->
  match fn elt with
  | exception Error err -> mk_error err
  | res -> Ok (List.concat_map (fun t -> t.tokens) res)

let structure = flatten_or_error tokenizer#structure
let signature = flatten_or_error tokenizer#signature

let modes = flatten_or_error tokenizer#modes
let jkind_annotation = flatten_or_error tokenizer#jkind_annotation
