open Ocaml_syntax

type stack_elt =
  | Terminal of Tokens.token
  | Comment of string
  | Non_terminal of Tokens.seq

module Make () = struct
  let tree_state : stack_elt Stack.t = Stack.create ()
  let lookahead : Tokens.token Stack.t = Stack.create ()

  let lex_and_save lexbuf =
    Option.iter (fun t -> Stack.push (Terminal t) tree_state)
      (Stack.pop_opt lookahead);
    let next_tok = Lexer.token lexbuf in
    Queue.iter (fun (c, _loc) ->
      Stack.push (Comment c) tree_state;
    ) Lexer.comments;
    Queue.clear Lexer.comments;
    Stack.push next_tok lookahead;
    next_tok

  let pop_until what =
    let rec aux acc =
      match Stack.top_opt tree_state with
      | None -> acc
      | Some _ ->
        let acc, stop =
          match Stack.pop tree_state with
          | Comment c -> Tokens.Comment c :: acc, false
          | Terminal t -> Tokens.Token t :: acc, what = `terminal
          | Non_terminal seq -> seq @ acc, what = `non_terminal
        in
        if stop
        then acc
        else aux acc
    in
    aux []

  let replace_top () =
    Stack.drop tree_state;
    Stack.push (Non_terminal [Child_node]) tree_state
end


