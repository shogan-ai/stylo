open Ocaml_syntax
open Parsetree

let lparen_child_rparen ~optional:opt pos =
  let open Tokens in
  let mk desc = { pos; desc } in
  let mk_tok tok = { pos; desc = Token (tok, opt) } in
  [ mk_tok LPAREN; mk Child_node; mk_tok RPAREN ]
;;

let parens_exp ?(optional = false) exp =
  { pexp_desc = Pexp_parens { exp; optional }
  ; pexp_tokens = lparen_child_rparen ~optional exp.pexp_loc.loc_start
  ; pexp_loc = exp.pexp_loc
  ; pexp_attributes = []
  ; pexp_ext_attr = { pea_ext = None; pea_attrs = [] }
  }
;;

let map_desc ~recur _ desc =
  let parent_for_recursive_calls = Context.Expr desc in
  recur parent_for_recursive_calls desc
;;

let insert_pipe_if_missing ~after:kw =
  let open Tokens in
  let rec insert_before_child = function
    | [] -> assert false
    | tok :: toks ->
      (match tok.desc with
       | Child_node -> { tok with desc = Token (BAR, false) } :: tok :: toks
       | Token (BAR, _) -> tok :: toks
       | _ -> tok :: insert_before_child toks)
  in
  let rec seek = function
    | [] -> assert false
    | tok :: toks ->
      tok
      :: (match tok.desc with
          | Token (t, _) when Tokens.Raw.equals t kw -> insert_before_child toks
          | _ -> seek toks)
  in
  seek
;;

let map ~recur (parent : Context.parent) exp =
  (* local changes first *)
  let exp = Semicolon.exp_no_trailing exp in
  let exp =
    match exp with
    | { pexp_desc = Pexp_begin_end (Some e)
      ; pexp_ext_attr =
          (* we can't attach ext_attrs to parens, so this is only valid when there are
             none *)
          { pea_ext = None; pea_attrs = [] }
      ; _
      } ->
      let pexp_desc = Pexp_parens { exp = e; optional = false } in
      let pexp_tokens =
        Utils.search_and_replace
          [ BEGIN, LPAREN; END, RPAREN ]
          exp.pexp_tokens
      in
      { exp with pexp_desc; pexp_tokens }
    | _ -> exp
  in
  (* context dependent changes *)
  let exp =
    match parent, exp.pexp_desc with
    (* Nothing special to do if parent is parens or Pstr_eval. *)
    | Expr (Pexp_parens _), _ | Str_item (Pstr_eval _), _ -> exp
    (* Add parens as necessary *)
    | _, Pexp_tuple _ -> parens_exp ~optional:true exp
    (* Nothing to do in the general case. *)
    | _ -> exp
  in
  recur parent exp
;;
