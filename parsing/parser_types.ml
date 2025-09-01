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
  { lb_body: let_binding_body;
    lb_attributes: attributes;
    lb_docs: docs Lazy.t;
    lb_text: text Lazy.t;
    lb_loc: Location.t;
    lb_toks: Tokens.seq; }

type let_bindings =
  { lbs_bindings: let_binding list;
    lbs_rec: rec_flag;
    lbs_extension: string Asttypes.loc option }
