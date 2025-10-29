open Ocaml_syntax
open Parsetree

let from_docstring attr =
  match attr.attr_name.txt with
  | "ocaml" :: ("doc" | "text") :: [] -> true
  | _ -> false

let lparen_child_rparen ~optional:opt pos =
  let open Tokens in
  let mk desc = { pos; desc } in
  let mk_tok tok = { pos; desc = if opt then Opt_token tok else Token tok } in
  [ mk_tok LPAREN
  ; mk Child_node
  ; mk_tok RPAREN ]

let parens_pat ?(optional=false) pat =
  { ppat_desc = Ppat_parens { pat; optional }
  ; ppat_tokens = lparen_child_rparen ~optional pat.ppat_loc.loc_start
  ; ppat_loc = pat.ppat_loc
  ; ppat_attributes = []
  ; ppat_ext_attr = { pea_ext = None; pea_attrs = [] } }

open Ast_mapper

let super = default_mapper

type parent = Constr | Parens | Other

let normalizer =
  let map_attribute mapper env attr =
    if from_docstring attr then
      attr
    else
      super.attribute mapper env attr
  in
  let map_pattern mapper parent pat =
    let pat = super.pattern mapper parent pat in
    match parent, pat.ppat_desc with
    (* Add parens where necessary *)
    | Constr, Ppat_construct (_, Some _) -> parens_pat pat
    | Other, Ppat_tuple _ -> parens_pat ~optional:true pat
    | _ -> pat
  in
  let map_pattern_desc mapper _ desc =
    let parent =
      match desc with
      | Ppat_construct _ -> Constr
      | Ppat_parens _ -> Parens
      | _ -> Other
    in
    super.pattern_desc mapper parent desc
  in
  let map_structure mapper env (items, tokens) =
    let semisemi ~optional pos =
      let open Tokens in
      let desc =
        if optional
        then Opt_token SEMISEMI
        else Token SEMISEMI
      in
      { desc; pos }
    in
    let tokens_no_semi =
      List.filter (fun tok -> tok.Tokens.desc <> Token SEMISEMI) tokens
    in
    let rec walk_both items tokens =
      match tokens with
      | [] -> assert (items = []); []
      | t :: tokens ->
        match t.Tokens.desc with
        | Token EOF (* TODO: filter this out earlier in the pipeline... *)
        | Comment _ ->
          t :: walk_both items tokens
        | Token _ | Opt_token _ ->
          (* No tokens appart from SEMISEMI at this level, and we removed them
             already. *)
          assert false
        | Child_node ->
          match items with
          | [] -> assert false
          | item :: ({ pstr_desc = Pstr_eval _ ; _ } :: _ as items) ->
            t :: semisemi ~optional:false item.pstr_loc.loc_end ::
            walk_both items tokens
          | item :: items ->
            match item.pstr_desc with
            | Pstr_value _ ->
              t :: semisemi ~optional:true item.pstr_loc.loc_end ::
              walk_both items tokens
            | _ -> t :: walk_both items tokens
    in
    let tokens_with_minimal_semi = walk_both items tokens_no_semi in
    super.structure mapper env (items, tokens_with_minimal_semi)
  in
  { super with
    attribute = map_attribute
  ; pattern = map_pattern
  ; pattern_desc = map_pattern_desc
  ; structure = map_structure
  }

let structure = normalizer.structure normalizer Other
