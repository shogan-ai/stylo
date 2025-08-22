open Ocaml_syntax

type stack_elt =
  | Terminal of Tokens.token
  | Comment of string
  | Non_terminal of Tokens.consumable ref

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
      match what, Stack.pop_opt tree_state with
      | _, None -> acc
      | `terminal, Some (Terminal _ as elt)
      | `non_terminal, Some (Non_terminal _ as elt) -> elt :: acc
      | _, Some elt -> aux (elt :: acc)
    in
    aux []

  (*
  let replace_top () =
    Stack.drop tree_state;
    Stack.push (Non_terminal [Child_node]) tree_state
     *)
end


