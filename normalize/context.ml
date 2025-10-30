open Ocaml_syntax.Parsetree

type parent =
  | Other
  | Pat of pattern_desc
  | Expr of expression_desc
  | Value_binding
  | Fun_param_or_arg
