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

  method! visit_structure_item env si =
    let sub_tokens = super#visit_structure_item env si in
    let node_toks = si.pstr_tokens in
    combine_children ~loc:si.pstr_loc node_toks sub_tokens

  method! visit_structure env s =
    let sub_tokens = super#visit_structure env s in
    let node_toks = snd s in
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

  method! visit_core_type env ct =
    let sub_tokens = super#visit_core_type env ct in
    let node_toks = ct.ptyp_tokens in
    combine_children ~loc:ct.ptyp_loc node_toks sub_tokens

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

  method! visit_value_binding env vb =
    let sub_tokens = super#visit_value_binding env vb in
    let node_toks = vb.pvb_tokens in
    combine_children ~loc:vb.pvb_loc node_toks sub_tokens

  method! visit_module_binding env mb =
    let sub_tokens = super#visit_module_binding env mb in
    let node_toks = mb.pmb_tokens in
    combine_children ~loc:mb.pmb_loc node_toks sub_tokens
end

