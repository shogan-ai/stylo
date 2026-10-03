(* Read by the lexer. This mirrors [vendor/oxcaml-frontend/config.ml], which
   keeps [lexer.mll] identical to its vendored counterpart in that respect.

   Only meant to be set by [Parse], from its options. *)

let syntax_quotations = ref false
