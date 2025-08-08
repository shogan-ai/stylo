let () =
  In_channel.with_open_text Sys.argv.(1) @@ fun ic ->
  let lexbuf = Lexing.from_channel ic in
  let ast = Parse.(Parser.implementation Lexer.token) lexbuf in
  let doc = Print.implementation ast in
  PPrint.ToChannel.pretty 1. 80 stdout doc
