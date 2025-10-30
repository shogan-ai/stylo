open Ocaml_syntax
open Parsetree

let from_docstring attr =
  match attr.attr_name.txt with
  | "ocaml" :: ("doc" | "text") :: [] -> true
  | _ -> false

open Ast_mapper

let super = default_mapper

let map_attribute mapper env attr =
  if from_docstring attr then
    attr
  else
    super.attribute mapper env attr

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

let default_vb_passing_context mapper _ vb =
  let parent_for_recursive_calls = Context.Value_binding in
  super.value_binding mapper parent_for_recursive_calls vb

let default_arg_passing_context mapper f _ arg =
  let parent_for_recursive_calls = Context.Fun_param_or_arg in
  super.argument_desc mapper f parent_for_recursive_calls arg

let normalizer =
  { super with
    attribute = map_attribute
  ; pattern = Pattern.map
  ; pattern_desc = Pattern.map_desc
  ; expression = Expression.map
  ; expression_desc = Expression.map_desc
  ; argument_desc = default_arg_passing_context
  ; value_binding = default_vb_passing_context
  ; structure = map_structure
  }

let structure = normalizer.structure normalizer Other
