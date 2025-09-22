(* These are types that are used by the parser. They need to be placed in a separate
   module (ie., not parser.mly) because adding the --inspection flag, the parser's
   interface refers to the result type of each parsing rule, so the types need to be
   available outside of just parser.ml. *)

open Asttypes
open Parsetree
open Docstrings

type let_binding_body =
  { lbb_modes: modes;
    lbb_pat: pattern;
    lbb_params: function_param list;
    lbb_constraint: value_constraint option;
    lbb_ret_modes: modes;
    lbb_expr: expression option }

type let_binding =
  { lb_ext_attrs: ext_attribute;
    lb_body: let_binding_body;
    lb_attributes: attributes;
    lb_docs: docs Lazy.t;
    lb_text: text Lazy.t;
    lb_loc: Location.t;
    lb_toks: Tokens.seq; }

type let_bindings =
  { lbs_bindings: let_binding list;
    lbs_mutable: mutable_flag;
    lbs_rec: rec_flag; }
