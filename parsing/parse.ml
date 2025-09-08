let lex_and_save lexbuf =
  let open Tokens.Indexed_list in
  let next_tok = Lexer.token lexbuf in
  Queue.iter (fun (c, loc) ->
    append global ~pos:loc.Location.loc_start (Comment c)
  ) Lexer.comments;
  Queue.clear Lexer.comments;
  append global ~pos:lexbuf.lex_start_p (Token next_tok);
  next_tok

let structure lb =
  Lexer.init ();
  Tokens.Indexed_list.reset_global ();
  Parser.implementation lex_and_save lb
