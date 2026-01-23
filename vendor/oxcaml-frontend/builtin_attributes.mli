type current_phase =
  | Parser
  | Invariant_check

val register_attr : current_phase -> string Location.loc -> unit
val mark_payload_attrs_used : Parsetree.payload -> unit
val has_curry : Parsetree.attributes -> bool
val has_or_null_reexport : Parsetree.attributes -> bool
val curry_attr : Location.t -> Parsetree.attribute
