type t =
  | Basic
  [@ocaml.doc
    "Includes only the basic metadata fields (e.g., deviceId, model, status, type, and \
     status)"]
  | Full [@ocaml.doc "Includes all metadata fields"]
  | Unknown_enum_value of string
