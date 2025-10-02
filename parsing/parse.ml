let implementation lb =
  Lexer.init ();
  Tokens.Indexed_list.reset_global ();
  let (str, struct_tokens) =
    Parser.implementation Lexer.token_updating_indexed_list lb in
  let all_tokens = Tokens.Indexed_list.(consume_all global) in
  str, Tokens.replace_first_child ~subst:struct_tokens all_tokens

let interface lb =
  Lexer.init ();
  Tokens.Indexed_list.reset_global ();
  let sg = Parser.interface Lexer.token_updating_indexed_list lb in
  let all_tokens = Tokens.Indexed_list.(consume_all global) in
  { sg with
    psg_tokens = Tokens.replace_first_child ~subst:sg.psg_tokens all_tokens }
