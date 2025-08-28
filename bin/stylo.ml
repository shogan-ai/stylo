open Ocaml_syntax

let () =
  In_channel.with_open_text Sys.argv.(1) @@ fun ic ->
  let lexbuf = Lexing.from_channel ic in
  let ast = Parse.structure lexbuf in
  let doc =
    Print.Structure.pp_implementation ast
    |> Wrapprint.to_document
  in
  PPrint.ToChannel.pretty 1. 80 stdout doc;
  print_newline ();
  flush stdout
