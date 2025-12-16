open Ocaml_syntax
open Parsetree

let semisemi ~optional pos =
  let open Tokens in
  let desc =
    if optional
    then Opt_token SEMISEMI
    else Token SEMISEMI
  in
  { desc; pos }

let normalize_struct_semisemi (items, tokens) =
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
  let tokens_no_semi = Utils.without ~token:SEMISEMI tokens in
  let tokens_with_minimal_semi = walk_both items tokens_no_semi in
  items, tokens_with_minimal_semi

let exp_no_trailing e =
  match e.pexp_desc with
  | Pexp_seq_empty e ->
    (* Can't attach attrs without parens here *)
    assert (e.pexp_attributes = []);
    e
  | _ -> e
