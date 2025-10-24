open Ocaml_syntax
open Parsetree

let from_docstring attr =
  match attr.attr_name.txt with
  | "ocaml" :: ("doc" | "text") :: [] -> true
  | _ -> false

class normalize = object

  inherit [_] Parsetree.map as super

  method! visit_attribute env attr =
    if from_docstring attr then
      attr
    else
      super#visit_attribute env attr

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
    let pos =
      match items with
       | { pstr_loc = l; _ } :: _ -> l.loc_start
       | _ -> Lexing.dummy_pos
    in
    dprintf
      "---@\npos: %d:%d@\n\
       items: %d@\ntokens: %d@\ntokens without semis: @[%a@]@."
      pos.pos_lnum (pos.pos_cnum - pos.pos_bol)
      (List.length items)
      (List.length tokens)
      Tokens.pp_seq tokens_no_semi;
    let rec walk_both items tokens =
      match tokens with
      | [] -> assert (items = []); []
      | t :: tokens ->
        match t.Tokens.desc with
        | Token EOF
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
