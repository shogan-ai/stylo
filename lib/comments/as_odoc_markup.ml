let to_document ~startp text =
  let doc =
    Odoc_parser.parse_comment ~location:startp ~text
    |> Odoc_parser.ast
    |> Print.Doc.Odoc.pp_ast
  in
  Document.(
    string "(* " ^^ nest 3 (doc ^^ group (break 1 ^^ string "*)"))
  )
