open Ocaml_syntax.Parsetree

type parent =
  | Other
  | Str_item of structure_item_desc
  | Pat of pattern_desc
  | Expr of expression_desc
  | Value_binding of value_binding
  | Fun_param_or_arg
