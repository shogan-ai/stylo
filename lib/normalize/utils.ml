open Ocaml_syntax

let lparen_child_rparen ~optional:opt pos =
  let open Tokens in
  let mk desc = { pos; desc } in
  let mk_tok tok = { pos; desc = Token (tok, opt) } in
  [ mk_tok LPAREN
  ; mk Child_node
  ; mk_tok RPAREN ]

let token_exists seq ~f =
  let open Tokens in
  List.exists (function
      | {desc = Token (tok, _opt); pos = _} -> f tok
      | _ -> false
    ) seq
