open Ocaml_syntax

let style_file fn =
  In_channel.with_open_text fn @@ fun ic ->
  let lexbuf = Lexing.from_channel ic in
  Location.init lexbuf fn;
  let doc, tokens =
    if Filename.check_suffix fn ".mli" then (
      let sg = Parse.interface lexbuf in
      Print.Signature.pp_interface sg, Tokens_of_tree.signature sg
    ) else (
      let str = Parse.implementation lexbuf in
      let str = Normalize.structure str in
      Print.Structure.pp_implementation str, Tokens_of_tree.structure str
    )
  in
  Dbg_print.dprintf "%a@." Tokens.pp_seq tokens;
  Insert_comments.from_tokens tokens doc

let width = ref 90

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
          String.length (if intf then "interface:" else "implementation:")
        in
        String.sub entrypoint_and_src prefix_len
          (String.length entrypoint_and_src - prefix_len)
      in
      let lexbuf = Lexing.from_string src in
      lexbuf.lex_curr_p <-
        { Lexing.pos_fname = fn; pos_lnum = i; pos_bol = 0; pos_cnum = 0 };
      match
        if intf then (
          let sg = Parse.interface lexbuf in
          Print.Signature.pp_interface sg, Tokens_of_tree.signature sg
        ) else (
          let str = Parse.implementation lexbuf in
          Print.Structure.pp_implementation str, Tokens_of_tree.structure str
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
        begin match Insert_comments.from_tokens tokens doc with
        | exception Insert_comments.Error e ->
          Format.eprintf "%s, line %d: ERROR: %a@\n@\n%s@\n@."
            fn i Insert_comments.pp_error e entrypoint_and_src;
          has_errors := true

        | exception exn ->
          Format.eprintf "%s, line %d: uncaught exception:@." fn i;
          raise exn

        | with_comments ->
          let styled = Document.Print.to_string ~width:!width with_comments in
          match Ast_checker.check_same_ast fn i ~impl:(not intf) src styled with
          | exception _ ->
            Format.eprintf "%s, line %d: syntax error in output@\n%s@\n@."
              fn i entrypoint_and_src;
            has_errors := true
          | false ->
            Format.eprintf "%s, line %d: ast changed@\n%s@\n@." fn i
              entrypoint_and_src;
            has_errors := true
          | true ->
            loop_lines (i + 1) ic
        end
  in
  In_channel.with_open_text fn (loop_lines 1);
  if !has_parse_errors then (
    Format.eprintf "Parse errors collected in %s.parse-errors@." fn;
    Out_channel.close (Lazy.force parse_error_oc)
  );
  !has_errors

let inputs = ref []
let ast_check = ref false
let fuzzing = ref false
let inplace = ref false

let () =
  Arg.parse
    ["-ast-check", Arg.Set ast_check,
     "Check the formatted code parses back to the same AST"
    ;"-fuzzing", Arg.Set fuzzing, "-ast-check on each line separately"
    ;"-i", Arg.Set inplace, "style file in place"
    ;"-width", Arg.Set_int width, ""]
    (fun fn -> inputs := fn :: !inputs)
    "stylo.exe [-ast-check] [-i] FILENAME*"

let () =
  let has_error = ref false in
  if !fuzzing then (
    assert (List.length !inputs = 1);
    has_error := fuzzer_batch (List.hd !inputs)
  ) else
    List.iter (fun fn ->
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
        let width = !width in
        if !ast_check &&
          let reprinted = Document.Print.to_string ~width doc in
          let source = In_channel.(with_open_text fn input_all) in
          not @@
          Ast_checker.check_same_ast fn 0
            ~impl:(Filename.check_suffix fn ".ml") source reprinted
        then
          (* TODO: location, etc *)
          Format.eprintf "%s: ast changed@." fn
        else (
          let pp oc =
            let reprinted = Document.Print.to_string ~width doc in
            output_string oc reprinted;
            output_char oc '\n';
            flush oc
          in
          if not !inplace then
            pp stdout
          else
            Out_channel.with_open_text fn pp
        )
    ) !inputs;
  if !has_error then
    exit 1
