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

let map mapper (parent : Context.parent) exp =
  let exp =
    match parent, exp.pexp_desc with
    (* Nothing special to do if parent is parens. *)
    | Expr Pexp_parens _, _ -> exp
    (* Add parens as necessary *)
    | _, Pexp_tuple _ ->
      parens_exp ~optional:true exp
    (* Nothing to do in the general case. *)
    | _ -> exp
  in
  super.expression mapper parent exp
