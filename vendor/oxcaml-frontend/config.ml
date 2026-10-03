(* Local shim (not an upstream file): the subset of the compiler's [Config]
   needed by the vendored files. Set by stylo's [Oxcaml_checker], from its
   options. *)

let syntax_quotations = ref false
