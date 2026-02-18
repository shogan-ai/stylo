let implementation lb =
  Lexer.init ();
  Tokens.reset ();
  let str =
    Parser.implementation Lexer.token_updating_indexed_list lb in
  let all_tokens = Tokens.attach_leading_and_trailing str.pst_tokens in
  { str with pst_tokens = all_tokens }

let interface lb =
  Lexer.init ();
  Tokens.reset ();
  let sg = Parser.interface Lexer.token_updating_indexed_list lb in
  let with_cmts = Tokens.attach_leading_and_trailing sg.psg_tokens in
  { sg with psg_tokens = with_cmts }
