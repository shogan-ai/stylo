open Ocaml_syntax
open Parsetree

let lparen_child_rparen ~optional:opt pos =
  let open Tokens in
  let mk desc = { pos; desc } in
  let mk_tok tok = { pos; desc = Token (tok, opt) } in
  [ mk_tok LPAREN
  ; mk Child_node
  ; mk_tok RPAREN ]

let parens_pat ?(optional=false) pat =
  { ppat_desc = Ppat_parens { pat; optional }
  ; ppat_tokens = lparen_child_rparen ~optional pat.ppat_loc.loc_start
  ; ppat_loc = pat.ppat_loc
  ; ppat_attributes = []
  ; ppat_ext_attr = { pea_ext = None; pea_attrs = [] } }

let simple_pattern p =
  p.ppat_attributes = [] &&
  match p.ppat_desc with
  | Ppat_var _
  | Ppat_parens _
  | Ppat_record _
  | Ppat_record_unboxed_product _
  | Ppat_list _
  | Ppat_array _
  | Ppat_unboxed_tuple _
  | Ppat_unpack _
  | Ppat_any
  | Ppat_interval _
  | Ppat_construct (_, None)
  | Ppat_variant (_, None)
  | Ppat_type _
  | Ppat_open _
  | Ppat_extension _
  | Ppat_constraint _ ->
    true
  | _ ->
    false

let remove_parens p =
  match p.ppat_desc with
  | Ppat_parens { pat = p'; optional = _ } ->
    (* FIXME: I'm pretty sure we are dropping attributes here *)
    let comments_before_p' =
      List.take_while (fun t -> not (Tokens.is_child t)) p.ppat_tokens
      |> List.filter Tokens.is_comment
    in
    let comments_after_p' =
      List.drop_while (fun t -> not (Tokens.is_child t)) p.ppat_tokens
      |> List.tl
      |> List.filter Tokens.is_comment
    in
    { p' with
      ppat_tokens =
        comments_before_p' @ p'.ppat_tokens @ comments_after_p'
    }
  | _ ->
    assert false

let make_parens_optional p =
  match p.ppat_desc with
  | Ppat_parens { pat = _; optional = true } -> p
  | Ppat_parens { pat; optional = false } ->
    let tokens =
      List.map Tokens.(function
        | { pos; desc = Token (LPAREN | RPAREN as tok, false)} ->
          { pos; desc = Token (tok, true) }
        | token -> token
      ) p.ppat_tokens
    in
    { p with
      ppat_desc = Ppat_parens { pat; optional = true }
    ; ppat_tokens = tokens }
  | _ -> assert false

let try_removing_parens parent p =
  match p with
  | { ppat_attributes = _ :: _; _ } ->
    (* let's assume parens are necessary.
       TODO: push to child when possible. *)
    p
  | { ppat_desc = Ppat_parens { pat = p'; optional = _ }; _ } ->
    begin match parent with
    | Ppat_open _
    | Ppat_list _ -> p
    | Ppat_exception _
    | Ppat_alias _
    | Ppat_lazy _
    | Ppat_tuple _
    | Ppat_construct _
    | Ppat_variant _
    | Ppat_cons _ when not (simple_pattern p') -> p
    | _ ->
      (* parens are not mandatory but we might still optionally keep them.
         Removing them and letting recursive calls reintroduce them would be
         correct, but might move comments from one outside the parentheses to
         inside. *)
      begin match p'.ppat_desc with
      | Ppat_tuple _ -> make_parens_optional p
      | _ -> remove_parens p
      end
    end
  | _ -> assert false

let map_desc ~recur _ desc =
  let parent_for_recursive_calls = Context.Pat desc in
  recur parent_for_recursive_calls desc

let map ~recur (parent : Context.parent) pat =
  let pat =
    match parent, pat.ppat_desc with
    (* Remove parens if they are not mandatory *)
    | Pat parent, Ppat_parens _ -> try_removing_parens parent pat
    | Value_binding vb, Ppat_parens { pat = sub; _ } ->
      begin match sub.ppat_desc with
      | Ppat_tuple _
        when Option.is_none vb.pvb_constraint && vb.pvb_ret_modes = [] ->
        make_parens_optional pat
      | _ when simple_pattern sub -> remove_parens pat
      | _ -> pat
      end
    | Expr _, Ppat_parens { pat = { ppat_desc = Ppat_or _; _ }; _ }
      when pat.ppat_attributes = [] ->
      remove_parens pat
    (* Nothing special to do if parent is parens. *)
    | Pat Ppat_parens _, _ -> pat
    (* Add parens as necessary *)
    | Pat Ppat_construct _, Ppat_construct (_, Some _)
    | Pat Ppat_alias _, (Ppat_or _ | Ppat_tuple _) ->
      parens_pat pat
    | _, Ppat_tuple _ ->
      parens_pat ~optional:true pat
    (* Nothing to do in the general case. *)
    | _ -> pat
  in
  recur parent pat
