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

let map_structure_item mapper _parent str_item =
  let parent_for_recursive_calls = Context.Str_item str_item.pstr_desc in
  super.structure_item mapper parent_for_recursive_calls str_item

let map_constructor_decl mapper env cd =
  let first_tok =
    List.find (function
      | { Tokens.desc = (Token _ | Opt_token _); _ } -> true
      | _ -> false
    ) cd.pcd_tokens
  in
  let cd =
    match first_tok.desc with
    | Token BAR
    | Opt_token BAR -> cd
    | _ ->
      let bar = { first_tok with desc = Token BAR } in
      { cd with pcd_tokens = bar :: cd.pcd_tokens }
  in
  super.constructor_declaration mapper env cd

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
    | [] ->
      let is_ds item =
        match item.pstr_desc with
        | Pstr_docstring _ -> true
        | _ ->
          let pos = item.pstr_loc.loc_start in
          dprintf "unexpected item at %d:%d@."
            pos.pos_lnum
            (pos.pos_cnum - pos.pos_bol);
          false
      in
      assert (List.for_all is_ds items); []
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
  ; function_body = Expression.map_function_body
  ; argument_desc = default_arg_passing_context
  ; value_binding = default_vb_passing_context
  ; constructor_declaration = map_constructor_decl
  ; structure_item = map_structure_item
  ; structure = map_structure
  }

let structure = normalizer.structure normalizer Other
