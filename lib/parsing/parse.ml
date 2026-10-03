module Options = struct
  type t = { syntax_quotations : bool (** enable quotations by default *) }

  let default = { syntax_quotations = false }
end

let init (options : Options.t) =
  Config.syntax_quotations := options.syntax_quotations;
  Lexer.init ();
  Tokens.reset ();
  Docstrings.init ()
;;

let implementation options lb =
  init options;
  let str = Parser.implementation Lexer.token_updating_indexed_list lb in
  let all_tokens = Tokens.attach_leading_and_trailing str.pst_tokens in
  { str with pst_tokens = all_tokens }
;;

let interface options lb =
  init options;
  let sg = Parser.interface Lexer.token_updating_indexed_list lb in
  let with_cmts = Tokens.attach_leading_and_trailing sg.psg_tokens in
  { sg with psg_tokens = with_cmts }
;;
