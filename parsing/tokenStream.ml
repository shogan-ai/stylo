open Ocaml_syntax

module Make () = struct
  let indexed_list = Tokens.Indexed_list.create ()

  let lex_and_save lexbuf =
    let next_tok = Lexer.token lexbuf in
    Queue.iter (fun (c, _loc) ->
      Tokens.Indexed_list.append indexed_list
        ~pos:_loc.Location.loc_start (Comment c)
    ) Lexer.comments;
    Queue.clear Lexer.comments;
    Tokens.Indexed_list.append indexed_list
      ~pos:lexbuf.lex_start_p (Token next_tok);
    next_tok
end


