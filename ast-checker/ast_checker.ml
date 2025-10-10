open Parsetree
open Ast_helper

let sort_attributes : attributes -> attributes = List.sort compare

let normalize_cmt_spaces doc =
  String.split_on_char ' ' doc
  |> List.filter ((<>) "")
  |> String.concat " "

let ignore_docstrings = ref true

let mapper =
  (* remove locations *)
  let location _ _ = Location.none in
  let attribute (m : Ast_mapper.mapper) (attr : attribute) =
    let attr =
      match attr.attr_name.txt with
      | "ocaml.doc" | "ocaml.text" ->
        let attr_payload =
          match attr.attr_payload with
          | PStr [ {
              pstr_desc =
                Pstr_eval
                  ({ pexp_desc= Pexp_constant Pconst_string (doc, loc, None)
                   ; _ } as inner_exp,[]);
              _
            } as str ] ->
            let doc = normalize_cmt_spaces doc in
            let inner' =
              { inner_exp with
                pexp_desc = Pexp_constant (Pconst_string (doc, loc, None))
              }
            in
            PStr [ { str with pstr_desc = Pstr_eval (inner', []) }]
          | _ -> assert false
        in
        { attr with attr_payload }
      | _ -> attr
    in
    Ast_mapper.default_mapper.attribute m attr
  in
  let attributes (m : Ast_mapper.mapper) (attrs : attribute list) =
    let attrs =
      if not !ignore_docstrings then
        attrs
      else
        List.filter (function
          | { attr_name = { txt = ("ocaml.doc" | "ocaml.text"); _ }; _ } ->
            false
          | _ -> true
        ) attrs
    in
    (* sort attributes *)
    Ast_mapper.default_mapper.attributes m (sort_attributes attrs)
  in
  let expr (m : Ast_mapper.mapper) exp =
    let exp = {exp with pexp_loc_stack= []} in
    let {pexp_desc; pexp_attributes; _} = exp in
    match pexp_desc with
    (* convert [(c1; c2); c3] to [c1; (c2; c3)] *)
    | Pexp_sequence
        ({pexp_desc= Pexp_sequence (e1, e2); pexp_attributes= []; _}, e3) ->
        m.expr m
          (Exp.sequence e1 (Exp.sequence ~attrs:pexp_attributes e2 e3))
    | _ -> Ast_mapper.default_mapper.expr m exp
  in
  let pat (m : Ast_mapper.mapper) pat =
    let pat = {pat with ppat_loc_stack= []} in
    let {ppat_desc; ppat_loc= loc1; ppat_attributes= attrs1; _} = pat in
    (* normalize nested or patterns *)
    match ppat_desc with
    | Ppat_or
        ( pat1
        , { ppat_desc= Ppat_or (pat2, pat3)
          ; ppat_loc= loc2
          ; ppat_attributes= attrs2
          ; _ } ) ->
        m.pat m
          (Pat.or_ ~loc:loc1 ~attrs:attrs1
             (Pat.or_ ~loc:loc2 ~attrs:attrs2 pat1 pat2)
             pat3)
    | _ -> Ast_mapper.default_mapper.pat m pat
  in
  let typ (m : Ast_mapper.mapper) typ =
    let typ = {typ with ptyp_loc_stack= []} in
    Ast_mapper.default_mapper.typ m typ
  in
  let structure_item (m : Ast_mapper.mapper) (si : structure_item) =
    match si.pstr_desc with
    | Pstr_eval ({pexp_desc= Pexp_extension e; _}, []) ->
        let e = m.extension m e in
        let pstr_loc = m.location m si.pstr_loc in
        {pstr_desc= Pstr_extension (e, []); pstr_loc}
    | _ -> Ast_mapper.default_mapper.structure_item m si
  in
  { Ast_mapper.default_mapper with
    location
  ; attribute
  ; attributes
  ; expr
  ; pat
  ; typ
  ; structure_item }

let report_error lex exn =
  match Location.error_of_exn exn with
  | None ->
    let loc =
      { Location.loc_start = lex.Lexing.lex_start_p
      ; loc_end = lex.Lexing.lex_curr_p
      ; loc_ghost = true }
    in
    Format.eprintf "%a@ Unexpected exn: %s@."
      Location.print_loc loc
      (Printexc.to_string exn);
    exit 1
  | Some `Already_displayed -> assert false
  | Some `Ok report ->
    Format.eprintf "%a@."
      Location.print_report report;
    exit 1

exception Failed_to_parse_source of exn

let struct_or_error ?(bail_out=false) lex =
  match Parse.implementation lex with
  | ast -> mapper.structure mapper ast
  | exception exn ->
    if bail_out then
      raise (Failed_to_parse_source exn)
    else
      report_error lex exn

let sig_or_error ?(bail_out=false) lex =
  match Parse.interface lex with
  | ast -> mapper.signature mapper ast
  | exception exn ->
    if bail_out then
      raise (Failed_to_parse_source exn)
    else
      report_error lex exn

let check_same_ast fn line ~impl s1 s2 =
  let pos =
    { Lexing.pos_fname = fn
    ; pos_lnum = line
    ; pos_bol = 0
    ; pos_cnum = 0 }
  in
  let lex1 = Lexing.from_string s1 in
  Lexing.set_position lex1 pos;
  let lex2 = Lexing.from_string s2 in
  Lexing.set_position lex2 pos;
  Lexing.set_filename lex2 (fn ^ ".out");
  Location.input_name := fn;
  if impl then
    let ast1 = struct_or_error ~bail_out:true lex1 in
    Location.input_name := (fn ^ ".out");
    let ast2 = struct_or_error lex2 in
    ast1 = ast2
  else
    let ast1 = sig_or_error ~bail_out:true lex1 in
    Location.input_name := (fn ^ ".out");
    let ast2 = sig_or_error lex2 in
    ast1 = ast2
