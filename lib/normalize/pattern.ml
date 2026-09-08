open Ocaml_syntax
open Parsetree

let parens_pat ?(optional=false) pat =
  { ppat_desc = Ppat_parens { pat; optional }
  ; ppat_tokens = Utils.lparen_child_rparen ~optional pat.ppat_loc.loc_start
  ; ppat_loc = pat.ppat_loc
  ; ppat_attributes = No_attributes
  ; ppat_ext_attr = { pea_ext = None; pea_attrs = No_attributes } }

(* TODO: this code should be used by/moved to parentheses.ml *)
(*
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
*)

let map_desc ~recur _ desc =
  let parent_for_recursive_calls = Context.Pat desc in
  recur parent_for_recursive_calls desc

let map ~recur (parent : Context.parent) pat =
  let pat =
    match parent, pat.ppat_desc with
    (* Nothing special to do if parent is parens. *)
    | Pat Ppat_parens _, _ -> pat
    | _, Ppat_tuple _ ->
      (* add optional parens around non parenthesized tuples *)
      parens_pat ~optional:true pat
    (* Nothing to do in the general case. *)
    | _ -> pat
  in
  recur parent pat
