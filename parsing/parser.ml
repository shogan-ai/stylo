module Tokens = Ocaml_syntax.Tokens

module TS = TokenStream.Make ()

open Ocaml_syntax.Parser.MenhirInterpreter

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

let rec loop supplier checkpoint =
  match checkpoint with
  | Accepted value -> value
  | Rejected -> failwith "Parse error"
  | InputNeeded _ -> loop supplier (offer checkpoint (supplier ()))
  | AboutToReduce (_, prod) ->
    let symbols = rhs prod in
    let lst = reduce_token_stream symbols in
    current_reduction_symbols := lst;
    loop supplier (resume checkpoint)
  | _ -> loop supplier (resume checkpoint)

let structure lb =
  let supplier = lexer_lexbuf_to_supplier TS.lex_and_save lb in
  let init = Ocaml_syntax.Parser.Incremental.implementation lb.lex_curr_p in
  loop supplier init
