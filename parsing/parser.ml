module Tokens = Ocaml_syntax.Tokens

module TS = TokenStream.Make ()

open Ocaml_syntax.Parser.MenhirInterpreter

let () =
  let open Tokens in
  between_ref := (fun ~start ~stop ->
    Indexed_list.consume TS.indexed_list start stop
  )

let rec loop supplier checkpoint =
  match checkpoint with
  | Accepted value -> value
  | Rejected -> failwith "Parse error"
  | InputNeeded _ -> loop supplier (offer checkpoint (supplier ()))
  | AboutToReduce (_, _) ->
    loop supplier (resume checkpoint)
  | _ -> loop supplier (resume checkpoint)

let structure lb =
  let supplier = lexer_lexbuf_to_supplier TS.lex_and_save lb in
  let init = Ocaml_syntax.Parser.Incremental.implementation lb.lex_curr_p in
  loop supplier init
