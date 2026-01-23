let implementation lb =
  Lexer.init ();
  Tokens.reset ();
  let str, struct_tokens =
    Parser.implementation Lexer.token_updating_indexed_list lb
  in
  let all_tokens = Tokens.attach_leading_and_trailing struct_tokens in
  str, all_tokens
;;

let interface lb =
  Lexer.init ();
  Tokens.reset ();
  let sg = Parser.interface Lexer.token_updating_indexed_list lb in
  let with_cmts = Tokens.attach_leading_and_trailing sg.psg_tokens in
  { sg with psg_tokens = with_cmts }
;;

let use_file lb =
  Lexer.init ();
  Tokens.reset ();
  let phrases, use_file_tokens =
    Parser.use_file Lexer.token_updating_indexed_list lb
  in
  let all_tokens = Tokens.attach_leading_and_trailing use_file_tokens in
  phrases, all_tokens
;;
