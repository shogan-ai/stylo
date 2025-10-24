open Ocaml_syntax
open Parsetree

let from_docstring attr =
  match attr.attr_name.txt with
  | "ocaml" :: ("doc" | "text") :: [] -> true
  | _ -> false

let lparen_child_lparen pos =
  let mk desc = { Tokens.desc ; pos } in
  [ mk (Token LPAREN)
  ; mk Child_node
  ; mk (Token RPAREN) ]

let parens_pat pat =
  { ppat_desc = Ppat_parens pat
  ; ppat_tokens = lparen_child_lparen pat.ppat_loc.loc_start
  ; ppat_loc = pat.ppat_loc
  ; ppat_attributes = []
  ; ppat_ext_attr = { pea_ext = None; pea_attrs = [] } }

class normalize = object

  inherit [_] Parsetree.map as super

  method! visit_attribute env attr =
    if from_docstring attr then
      attr
    else
      super#visit_attribute env attr

  method! visit_pattern_desc env desc =
    let desc =
      match desc with
      | Ppat_construct
          (lid, Some (vars, ({ ppat_desc = Ppat_construct (_, Some _)
                             ; _ } as sub_pat))) ->
        Ppat_construct (lid, Some (vars, parens_pat sub_pat))
      | _ -> desc
    in
    super#visit_pattern_desc env desc

  method! visit_structure env (items, tokens) =
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
    super#visit_structure env (items, tokens_with_minimal_semi)
end
