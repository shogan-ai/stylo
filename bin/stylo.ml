open Ocaml_syntax

let tokenizer = new Tokens_of_tree.to_tokens

let style_file fn =
  In_channel.with_open_text fn @@ fun ic ->
  let lexbuf = Lexing.from_channel ic in
  Location.init lexbuf fn;
  let doc, tokens =
    if Filename.check_suffix fn ".mli" then (
      let sg = Parse.interface lexbuf in
      Print.Signature.pp_interface sg, tokenizer # visit_signature () sg
    ) else (
      let str = Parse.implementation lexbuf in
      Print.Structure.pp_implementation str, tokenizer # visit_structure () str
    )
  in
  let tokens =
    (* FIXME: shouldn't be needed when tokenizer is completed *)
    List.flatten tokens
  in
  Dbg_print.dprintf "%a@." Tokens.pp_seq tokens;
  Insert_comments.from_tokens tokens doc
  |> Wrapprint.to_document

let fuzzer_batch fn =
  let has_errors = ref false in
  let has_parse_errors = ref false in
  let styled = Buffer.create 42 in
  (* Only create the file when there are parse errors *)
  let parse_error_oc = lazy (Out_channel.open_text (fn ^ ".parse-errors")) in
  let rec loop_lines i ic =
    Buffer.clear styled;
    match In_channel.input_line ic with
    | None -> () (* done *)
    | Some entrypoint_and_src ->
      let intf = String.starts_with ~prefix:"interface:" entrypoint_and_src in
      let src =
        let prefix_len =
          90 + String.length (if intf then "interface:" else "implementation:")
        in
        try
        String.sub entrypoint_and_src prefix_len
          (String.length entrypoint_and_src - prefix_len)
        with Invalid_argument _ -> "" (* woooooooh *)
      in
      let lexbuf = Lexing.from_string src in
      lexbuf.lex_curr_p <-
        { Lexing.pos_fname = fn; pos_lnum = i; pos_bol = 0; pos_cnum = 0 };
      match
        if intf then (
          let sg = Parse.interface lexbuf in
          Print.Signature.pp_interface sg, tokenizer # visit_signature () sg
        ) else (
          let str = Parse.implementation lexbuf in
          Print.Structure.pp_implementation str, tokenizer # visit_structure () str
        )
      with
      | exception Parser.Error ->
        (* ignoring entries that don't parse *)
        let oc = Lazy.force parse_error_oc in
        Out_channel.output_string oc entrypoint_and_src;
        Out_channel.output_char oc '\n';
        has_parse_errors := true;
        loop_lines (i + 1) ic

      | exception (Parser_types.Failwith _) ->
        (* ignoring error thrown from semantic actions. *)
        loop_lines (i + 1) ic

      | exception (Failure s)
        when String.ends_with s ~suffix:"for use in runtime metaprog" ->
        (* same as above *)
        loop_lines (i + 1) ic

      | exception exn ->
        Format.eprintf "%s, line %d: uncaught exception:@." fn i;
        raise exn

      | doc, tokens ->
        (* we stop at the first error in the batch
           Eventually we might want to go further, but while we try to fix the
           errors, there's not much point. *)
        begin match Insert_comments.from_tokens (List.flatten tokens) doc with
        | exception Insert_comments.Error e ->
          Format.eprintf "%s, line %d: ERROR: %a@."
            fn i Insert_comments.pp_error e;
          has_errors := true

        | exception exn ->
          Format.eprintf "%s, line %d: uncaught exception:@." fn i;
          raise exn

        | with_comments ->
          let styled =
            with_comments
            |> Wrapprint.to_document
            |> PPrint.ToBuffer.compact styled
            |> fun () -> Buffer.contents styled
          in
          match Ast_checker.check_same_ast ~impl:(not intf) src styled with
          | false ->
            Format.eprintf "%s, line %d: ast changed@." fn i;
            has_errors := true
          | true
          | exception _ (* FIXME: shouldn't user compiler-libs parser.. *)
            ->
            loop_lines (i + 1) ic
        end
  in
  In_channel.with_open_text fn (loop_lines 1);
  if !has_parse_errors then (
    Format.eprintf "Parse errors collected in %s.parse-errors@." fn;
    Out_channel.close (Lazy.force parse_error_oc)
  );
  !has_errors

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
let ast_check = ref false
let fuzzing = ref false

let () =
  Arg.parse
    ["-only-check", Arg.Set check, "Compare result with input, no printing"
    ;"-ast-check", Arg.Set ast_check, "Compare official AST after reparsing"
    ;"-fuzzing", Arg.Set fuzzing, "-ast-check on each line separately"]
    (fun fn -> inputs := fn :: !inputs)
    "stylo.exe [-only-check] FILENAME*"

let () =
  let has_error = ref false in
  List.iter (fun fn ->
    if !fuzzing then (
      assert (List.length !inputs = 1);
      has_error := fuzzer_batch fn
    ) else
      match style_file fn with
      | exception Insert_comments.Error e ->
        Format.eprintf "%s: ERROR: %a@." fn Insert_comments.pp_error e;
        has_error := true
      | exception exn ->
        let bt = Printexc.get_backtrace () in
        Format.eprintf "%s: %s" fn (Printexc.to_string exn);
        if Dbg_print.dbg then
          Format.eprintf "@\n%s@." bt
        else
          Format.eprintf "@.";
        has_error := true
      | doc ->
        if !check then
          lex_and_compare fn doc
        else if !ast_check then (
          let b = Buffer.create 42 in
          PPrint.ToBuffer.pretty 1. 80 b doc;
          let source = In_channel.(with_open_text fn input_all) in
          let reprinted = Buffer.contents b in
          if Ast_checker.check_same_ast ~impl:true source reprinted then
            ()
          else
            (* TODO: location, etc *)
            Format.eprintf "%s: ast changed@." fn
        ) else (
          PPrint.ToChannel.pretty 1. 80 stdout doc;
          print_newline ();
          flush stdout
        )
  ) !inputs;
  if !has_error then
    exit 1
