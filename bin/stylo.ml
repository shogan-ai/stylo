open Ocaml_syntax

let style_file contents ~filename =
  let lexbuf = Lexing.from_string contents in
  Location.init lexbuf filename;
  let doc, tokens =
    if Filename.check_suffix filename ".mli"
    then (
      let sg = Parse.interface lexbuf in
      let sg = Normalize.signature sg in
      Print.Signature.pp_interface sg, Tokens_of_tree.signature sg)
    else (
      let str = Parse.implementation lexbuf in
      let str = Normalize.structure str in
      Print.Structure.pp_implementation str, Tokens_of_tree.structure str)
  in
  Dbg_print.dprintf "%a@." Tokens.pp_seq tokens;
  Comments.Insert.from_tokens tokens doc
;;

let fuzzer_batch fn ~width =
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
        String.sub
          entrypoint_and_src
          prefix_len
          (String.length entrypoint_and_src - prefix_len)
      in
      let lexbuf = Lexing.from_string src in
      lexbuf.lex_curr_p
      <- { Lexing.pos_fname = fn; pos_lnum = i; pos_bol = 0; pos_cnum = 0 };
      (match
         if intf
         then (
           let sg = Parse.interface lexbuf in
           Print.Signature.pp_interface sg, Tokens_of_tree.signature sg)
         else (
           let str = Parse.implementation lexbuf in
           Print.Structure.pp_implementation str, Tokens_of_tree.structure str)
       with
       | exception Parser.Error ->
         (* ignoring entries that don't parse *)
         let oc = Lazy.force parse_error_oc in
         Out_channel.output_string oc entrypoint_and_src;
         Out_channel.output_char oc '\n';
         has_parse_errors := true;
         loop_lines (i + 1) ic
       | exception Parser_types.Failwith _ ->
         (* ignoring error thrown from semantic actions. *)
         loop_lines (i + 1) ic
       | exception Failure s
         when String.ends_with s ~suffix:"for use in runtime metaprog" ->
         (* same as above *)
         loop_lines (i + 1) ic
       | exception exn ->
         Format.eprintf "%s, line %d: uncaught exception:@." fn i;
         raise exn
       | doc, tokens ->
         (* we stop at the first error in the batch Eventually we might want to go
            further, but while we try to fix the errors, there's not much point. *)
         (match Comments.Insert.from_tokens tokens doc with
          | exception Comments.Insert.Error e ->
            Format.eprintf
              "%s, line %d: ERROR: %a@\n@\n%s@\n@."
              fn
              i
              Comments.Insert.Error.pp
              e
              entrypoint_and_src;
            has_errors := true
          | exception exn ->
            Format.eprintf "%s, line %d: uncaught exception:@." fn i;
            raise exn
          | with_comments ->
            let styled = Document.Print.to_string ~width with_comments in
            (match
               Ast_checker.Cst_checker.check_same_ast
                 fn
                 i
                 ~impl:(not intf)
                 src
                 styled
             with
             | exception _ ->
               Format.eprintf
                 "%s, line %d: syntax error in output@\n%s@\n@."
                 fn
                 i
                 entrypoint_and_src;
               has_errors := true
             | false ->
               Format.eprintf
                 "%s, line %d: ast changed@\n%s@\n@."
                 fn
                 i
                 entrypoint_and_src;
               has_errors := true
             | true -> loop_lines (i + 1) ic)))
  in
  In_channel.with_open_text fn (loop_lines 1);
  if !has_parse_errors
  then (
    Format.eprintf "Parse errors collected in %s.parse-errors@." fn;
    Out_channel.close (Lazy.force parse_error_oc));
  !has_errors
;;

let width_param =
  let open Command.Param in
  flag_optional_with_default_doc_string
    "width"
    int
    Int.to_string
    ~default:90
    ~doc:""
;;

let command_fuzz =
  Command.basic
    ~summary:"-ast-check on each line separately"
    (let%map_open.Command () = return ()
     and input = anon ("FILE" %: string)
     and width = width_param in
     fun () -> if fuzzer_batch input ~width then exit 1)
;;

let format_one contents ~filename ~width ~ast_check ~in_place ~debug =
  match style_file contents with
  | exception Comments.Insert.Error e ->
    Format.eprintf "%s: ERROR: %a@." filename Comments.Insert.Error.pp e;
    Error ()
  | exception exn ->
    let bt = Printexc.get_backtrace () in
    Format.eprintf "%s: %s" filename (Printexc.to_string exn);
    if Dbg_print.dbg then Format.eprintf "@\n%s@." bt else Format.eprintf "@.";
    Error ()
  | doc ->
    let reprinted = Document.Print.to_string ~width (doc ~filename) in
    let ast_check_res =
      if not ast_check
      then Ok ()
      else
        Ast_checker.Oxcaml_checker.check_same_ast
          filename
          0
          ~impl:(Filename.check_suffix filename ".ml")
          ~debug
          contents
          reprinted
    in
    (match ast_check_res with
     | Error print_debug ->
       (* TODO: location, etc *)
       Format.eprintf "%s: ast changed@." filename;
       print_debug ();
       Error ()
     | Ok () ->
       let pp oc =
         output_string oc reprinted;
         output_char oc '\n';
         flush oc
       in
       if not in_place
       then pp stdout
       else Out_channel.with_open_text filename pp;
       Ok ())
;;

module Input = struct
  type t =
    | File of string
    | Stdin of string

  let make inputs ~stdin ~in_place =
    match stdin with
    | None -> Ok (List.map (fun filename -> File filename) inputs)
    | Some filename ->
      if List.length inputs > 0
      then Error "positional files and -stdin are incompatible"
      else if in_place
      then Error "-in-place and -stdin are incompatible"
      else Ok [ Stdin filename ]
  ;;

  let load = function
    | File filename -> In_channel.(with_open_text filename input_all)
    | Stdin _ -> In_channel.(input_all stdin)
  ;;

  let filename = function
    | File filename | Stdin filename -> filename
  ;;
end

let command_format =
  Command.basic
    ~summary:"format files"
    (let%map_open.Command () = return ()
     and ast_check =
       flag
         "ast-check"
         no_arg
         ~doc:"Check the formatted code parses back to the same AST"
     and in_place = flag "i" no_arg ~doc:"style file in place"
     and width = width_param
     and debug = flag "debug" no_arg ~doc:"print extra error information"
     and stdin =
       flag
         "stdin"
         (optional string)
         ~doc:"take file in via /dev/stdin; pass a name to use"
     and inputs = anon (sequence ("FILE" %: string)) in
     fun () ->
       match Input.make inputs ~stdin ~in_place with
       | Error error ->
         Format.eprintf "Error: %s\n%!" error;
         exit 1
       | Ok inputs ->
         let results =
           List.map
             (fun input ->
               let filename = Input.filename input in
               let contents = Input.load input in
               format_one contents ~filename ~width ~ast_check ~in_place ~debug)
             inputs
         in
         if List.exists Result.is_error results then exit 1)
;;

let command =
  Command.group
    ~summary:"stylo"
    [ "format", command_format; "fuzz", command_fuzz ]
;;

let () = Command_unix.run command
