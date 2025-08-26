module Tokens = Ocaml_syntax.Tokens

module TS = TokenStream.Make ()

open Ocaml_syntax.Parser.MenhirInterpreter


let consumables : Tokens.consumable ref Queue.t =
  Queue.create ()

let () =
  let open Tokens in
  of_production_ref := (fun () ->
    let c_r = Queue.pop consumables in
    match !c_r with
    | Consumed -> failwith "Tokens already accessed"
    | Available tree ->
      Format.eprintf "ACCESSING\n%a\n%!" Tokens.pp_tree !c_r;
      c_r := Consumed;
      flatten tree
  )

let flatten_stack_elts =
  List.map (function
    | TokenStream.Terminal t -> Tokens.Tok t
    | Comment s -> Cmt s
    | Non_terminal nt -> Inlined nt
  )

let rec pop_token_stream (syms : Symbol_tree.t) =
  match syms with
  | [] -> []
  | Terminal :: rest ->
    let tail = pop_token_stream rest in
    let prefix = TS.pop_until `terminal in
    flatten_stack_elts prefix @ tail
  | Nonterminal :: rest ->
    let tail = pop_token_stream rest in
    let prefix = TS.pop_until `non_terminal in
    flatten_stack_elts prefix @ tail
  | Inlined sub :: rest ->
    let tail = pop_token_stream rest in
    let sub_tree = ref @@ Tokens.Available (pop_token_stream sub) in
    Tokens.Inlined sub_tree :: tail

let rec enqueue_subtrees root =
  let open Tokens in
  match !root with
  | Consumed ->
    (* We encounter an already consumed node when a previous reduction accessed
       subtrees (corresponding to inlined rules) of the token tree built at the
       time, but didn't access the tokens of the "main" production (i.e. the
       non-inlined one being reduced). *)
    ()
  | Available tree ->
    List.iter (function
      | Tok _ | Cmt _ -> ()
      | Inlined consumable ->
        (* Enqueue children first *)
        enqueue_subtrees consumable;
        Queue.add consumable consumables
    ) tree;
    Queue.add root consumables

let reduce_token_stream prod =
  let symbols = Symbol_tree.of_production prod in
  Format.eprintf "PROD TREE:\n%a\n%!"
    Symbol_tree.pp symbols;
  let tree = ref @@ Tokens.Available (pop_token_stream symbols) in
  Format.eprintf "BEFORE ENQUEUING:\n%a\n%!"
    Tokens.pp_tree !tree;
  enqueue_subtrees tree;
  Stack.push (TokenStream.Non_terminal tree) TS.tree_state

let rec loop supplier checkpoint =
  match checkpoint with
  | Accepted value -> value
  | Rejected -> failwith "Parse error"
  | InputNeeded _ -> loop supplier (offer checkpoint (supplier ()))
  | AboutToReduce (_, prod) ->
    Queue.clear consumables;
    Format.eprintf "AboutToReduce %s\n"
      (Symbol_tree.xsym_to_string @@ lhs prod);
    reduce_token_stream prod;
    loop supplier (resume checkpoint)
  | _ -> loop supplier (resume checkpoint)

let structure lb =
  let supplier = lexer_lexbuf_to_supplier TS.lex_and_save lb in
  let init = Ocaml_syntax.Parser.Incremental.implementation lb.lex_curr_p in
  loop supplier init
