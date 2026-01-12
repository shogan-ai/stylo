open Ocaml_syntax
open Parsetree
open Ast_mapper

let lparen_child_rparen ~optional:opt pos =
  let open Tokens in
  let mk desc = { pos; desc } in
  let mk_tok tok = { pos; desc = if opt then Opt_token tok else Token tok } in
  [ mk_tok LPAREN
  ; mk Child_node
  ; mk_tok RPAREN ]

let parens_exp ?(optional=false) exp =
  { pexp_desc = Pexp_parens { exp; optional }
  ; pexp_tokens = lparen_child_rparen ~optional exp.pexp_loc.loc_start
  ; pexp_loc = exp.pexp_loc
  ; pexp_attributes = []
  ; pexp_ext_attr = { pea_ext = None; pea_attrs = [] } }

let super = default_mapper

let map_desc mapper _ desc =
  let parent_for_recursive_calls = Context.Expr desc in
  super.expression_desc mapper parent_for_recursive_calls desc

let insert_pipe_if_missing ~after:kw =
  let open Tokens in
  let rec insert_before_child = function
    | [] -> assert false
    | tok :: toks ->
      match tok.desc with
      | Child_node ->
        { tok with desc = Token BAR } :: tok :: toks
      | Token BAR -> tok :: toks
      | _ -> tok :: insert_before_child toks
  in
  let rec seek = function
    | [] -> assert false
    | tok :: toks ->
      tok ::
      (match tok.desc with
       | Opt_token t
       | Token t when Tokens.Raw.equals t kw -> insert_before_child toks
       | _ -> seek toks)
  in
  seek

let map mapper (parent : Context.parent) exp =
  (* local changes first *)
  let exp = Semicolon.exp_no_trailing exp in
  let exp =
    match exp.pexp_desc with
    | Pexp_begin_end Some e ->
      let pexp_desc = Pexp_parens { exp = e; optional = false } in
      let pexp_tokens =
        Utils.search_and_replace [BEGIN, LPAREN; END, RPAREN] exp.pexp_tokens
      in
      { exp with pexp_desc; pexp_tokens }
    | _ -> exp
  in
  (* context dependent changes *)
  let exp =
    match parent, exp.pexp_desc with
    (* Nothing special to do if parent is parens or Pstr_eval. *)
    | Expr Pexp_parens _, _
    | Str_item Pstr_eval _, _ -> exp
    (* Add parens as necessary *)
    | _, Pexp_tuple _ ->
      parens_exp ~optional:true exp
    (* Nothing to do in the general case. *)
    | _ -> exp
  in
  super.expression mapper parent exp
