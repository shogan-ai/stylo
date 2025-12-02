open Ocaml_syntax

(* {!https://ocaml.org/manual/5.4/expr.html#ss:precedence-and-associativity} *)

let is_star s ~pos =
  match String.get s pos with
  | '*' -> true
  | _
  | exception Invalid_argument _ -> false

let of_infix_op name =
  match String.get name 0, name with
  | _, ("lsl" | "lsr" | "asr") -> 6
  | '*', _ when is_star name ~pos:1 -> 6
  | ('*' | '/' | '%'), _
  | _, ("mod" | "land" | "lor" | "lxor") -> 7
  | ('+' | '-'), _ -> 8
  | _, "::" -> 9
  | ('@' | '^'), _ -> 10
  | (* needs to be checked before the line below, because the patterns overlap
       ... *)
    _, ("&" | "&&") -> 12
  | ('=' | '<' | '>' | '|' | '&' | '$'), _
  | _, "!=" -> 11
  | _, ("or" | "||") -> 13
  | _, ":=" -> 15
  | _ -> 99 (* pfffrt *)

let of_infix_op exp =
  match exp.Parsetree.pexp_desc with
  | Pexp_ident { txt = { desc = Lident Op s; _ }; _ } -> of_infix_op s
  | _ -> assert false
