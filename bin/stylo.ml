open Ocaml_syntax

let () =
  In_channel.with_open_text Sys.argv.(1) @@ fun ic ->
  let lexbuf = Lexing.from_channel ic in
  let ast = Parse.structure lexbuf in
  let doc = Print.Structure.pp_implementation ast in
  let tokenizer = new Parsetree.to_tokens in
  let tokens =
    tokenizer # visit_structure () ast
    |> List.flatten (* FIXME: shouldn't be needed when tokenizer is completed *)
  in
  Dbg_print.dprintf "%a@." Tokens.pp_seq tokens;
  let doc =
    Insert_comments.from_tokens tokens doc
    |> Wrapprint.to_document
  in
  PPrint.ToChannel.pretty 1. 80 stdout doc;
  print_newline ();
  flush stdout
