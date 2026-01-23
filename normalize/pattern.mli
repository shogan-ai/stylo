open Ocaml_syntax

val map_desc
  :  recur:(Context.parent -> Parsetree.pattern_desc -> Parsetree.pattern_desc)
  -> Context.parent
  -> Parsetree.pattern_desc
  -> Parsetree.pattern_desc

val map
  :  recur:(Context.parent -> Parsetree.pattern -> Parsetree.pattern)
  -> Context.parent
  -> Parsetree.pattern
  -> Parsetree.pattern
