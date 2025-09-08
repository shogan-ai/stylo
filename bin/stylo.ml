open Ocaml_syntax

let style_file fn =
  In_channel.with_open_text fn @@ fun ic ->
  let lexbuf = Lexing.from_channel ic in
  let ast = Parse.structure lexbuf in
  let doc = Print.Structure.pp_implementation ast in
  let tokenizer = new Tokens_of_tree.to_tokens in
  let tokens =
    tokenizer # visit_structure () ast
    |> List.flatten (* FIXME: shouldn't be needed when tokenizer is completed *)
  in
  Dbg_print.dprintf "%a@." Tokens.pp_seq tokens;
  Insert_comments.from_tokens tokens doc
  |> Wrapprint.to_document

let get_tokens lexbuf =
  Lexer.init ();
  let rec aux acc =
    match Lexer.token_with_comments lexbuf with
    | exception _ | EOF -> acc
    | EOL -> aux acc
    | tok -> aux (tok :: acc)
  in
  List.rev (aux [])


let lex_and_compare input_fn doc =
  let output_tokens =
    let buf = Buffer.create 42 in
    PPrint.ToBuffer.compact buf doc;
    let s = Buffer.contents buf in
    Dbg_print.dprintf "output %S@." s;
    Lexing.from_string s
    |> get_tokens
  in
  let input_lexbuf =
    let input = In_channel.(with_open_text input_fn input_all) in
    Dbg_print.dprintf "input %S@." input;
    Lexing.from_string input
  in
  Lexer.init ();
  let rec iterate_tokens out_toks =
    let pos = input_lexbuf.lex_curr_p in
    match Lexer.token_with_comments input_lexbuf with
    | exception _ | EOF ->
      begin match out_toks with
      | [] -> (* tokens match, we're done. *) ()
      | _ -> Format.eprintf "%s extra tokens at the end of output@." input_fn
      end
    | EOL -> iterate_tokens out_toks
    | next_tok ->
      match out_toks with
      | [] ->
        Format.eprintf "%s: output ended unexpectedly at position %d:%d@."
            input_fn pos.pos_lnum (pos.pos_cnum - pos.pos_bol)
      | ot :: ots ->
        if Tokens.Raw.equals next_tok ot then
          iterate_tokens ots
        else
          Format.eprintf
            "%s: output differs from input at position %d:%d:@\n\
             input is token %s,@ output is token %s@."
            input_fn pos.pos_lnum (pos.pos_cnum - pos.pos_bol)
            (Tokens.Raw.to_string next_tok) (Tokens.Raw.to_string ot)
  in
  iterate_tokens output_tokens

let inputs = ref []
let check = ref false

let () =
  Arg.parse
    ["-only-check", Arg.Set check, "Compare result with input, no printing"]
    (fun fn -> inputs := fn :: !inputs)
    "stylo.exe [-only-check] FILENAME*"

let () =
  List.iter (fun fn ->
    let doc = style_file fn in
    if !check then
      lex_and_compare fn doc
    else (
      PPrint.ToChannel.pretty 1. 80 stdout doc;
      print_newline ();
      flush stdout
    )
  ) !inputs
