module Tokens = Ocaml_syntax.Tokens

module TS = TokenStream.Make ()

open Ocaml_syntax.Parser.MenhirInterpreter


(* FIXME: we want a table of Tokens.consumable indexed by location, because we
   have no guarantee all the inlined semantic actions will access the tokens
   (indeed most won't).
   This table would be cleared on each AboutToReduce checkpoint.

   TODO: location arithmetic in TokenStream ... *)
let consumables : Tokens.consumable ref Stack.t =
  Stack.create ()
(*
let pop_token_stream xsymbol seq =
  begin match xsymbol with
  | X T _ -> TS.pop_until `terminal
  | X N _ -> TS.pop_until `non_terminal end
  @ seq

let current_reduction_symbols : Tokens.seq ref = ref []

let () =
  Tokens.of_production_ref :=
    (fun () ->
      let symbols = !current_reduction_symbols in
      TS.replace_top ();
      symbols)

let reduce_token_stream xsymbols =
  let seq = List.fold_right pop_token_stream xsymbols [] in
  Stack.push (TokenStream.Non_terminal seq) TS.tree_state;
  seq
   *)

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
    (* FIXME: don't push here, the visiting order doesn't match the order in
       which actions are inlined/call.
       We need to create the whole tree, and then push on the way up from a
       DF-traversal *)
    Stack.push sub_tree consumables;
    Tokens.Inlined sub_tree :: tail

let reduce_token_stream prod =
  let symbols = Symbol_tree.of_production prod in
  let tree = ref @@ Tokens.Available (pop_token_stream symbols) in
  Stack.push tree consumables;
  Stack.push (TokenStream.Non_terminal tree) TS.tree_state

let rec loop supplier checkpoint =
  match checkpoint with
  | Accepted value -> value
  | Rejected -> failwith "Parse error"
  | InputNeeded _ -> loop supplier (offer checkpoint (supplier ()))
  | AboutToReduce (_, prod) ->
    reduce_token_stream prod;
    loop supplier (resume checkpoint)
  | _ -> loop supplier (resume checkpoint)

let structure lb =
  let supplier = lexer_lexbuf_to_supplier TS.lex_and_save lb in
  let init = Ocaml_syntax.Parser.Incremental.implementation lb.lex_curr_p in
  loop supplier init
