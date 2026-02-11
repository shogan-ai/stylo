open Ocaml_syntax

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
      match Stylo.style_fuzzer_line ~fname:fn ~lnum:i entrypoint_and_src with
      | Ok _ -> loop_lines (i + 1) ic

      | Error `Cst_parser_error (Parser_types.Failwith _) ->
        (* ignoring error thrown from semantic actions. *)
        loop_lines (i + 1) ic

      | Error `Cst_parser_error _
      | Error `Input_parse_error _ ->
        (* ignoring entries that don't parse *)
        let oc = Lazy.force parse_error_oc in
        Out_channel.output_string oc entrypoint_and_src;
        Out_channel.output_char oc '\n';
        has_parse_errors := true;
        loop_lines (i + 1) ic

      | Error e ->
        (* we stop at the first error in the batch
           Eventually we might want to go further, but while we try to fix the
           errors, there's not much point. *)
        let fname = fn ^ ":" ^ string_of_int i in
        Stylo.Pipeline.pp_error fname e;
        has_errors := true

      | exception exn ->
        Format.eprintf "%s, line %d: uncaught exception:@." fn i;
        raise exn
  in
  In_channel.with_open_text fn (loop_lines 1);
  if !has_parse_errors then (
    Format.eprintf "Parse errors collected in %s.parse-errors@." fn;
    Out_channel.close (Lazy.force parse_error_oc)
  );
  !has_errors

let inputs = ref []
let fuzzing = ref false
let inplace = ref false

let () =
  Arg.parse
    ["-ast-check", Arg.Set Config.check_same_ast,
     "Check the formatted code parses back to the same AST"
    ;"-syntax-quotations", Arg.Set Config.syntax_quotations,
     "oxcaml specifig: enable quotations by default"
    ;"-fuzzing", Arg.Set fuzzing, "-ast-check on each line separately"
    ;"-i", Arg.Set inplace, "style file in place"
    ;"-width", Arg.Set_int Config.width, ""]
    (fun fn -> inputs := fn :: !inputs)
    "stylo.exe [-ast-check] [-i] FILENAME*"

let () =
  let has_error = ref false in
  if !fuzzing then (
    assert (List.length !inputs = 1);
    Config.check_same_ast := true;
    has_error := fuzzer_batch (List.hd !inputs)
  ) else
    List.iter (fun fn ->
      let is_mli = Filename.check_suffix fn ".mli" in
      match
        if is_mli
        then Stylo.style_file Intf fn
        else Stylo.style_file Impl fn
      with
      | exception exn ->
        let bt = Printexc.get_backtrace () in
        Format.eprintf "%s: %s" fn (Printexc.to_string exn);
        if Dbg_print.dbg then
          Format.eprintf "@\n%s@." bt
        else
          Format.eprintf "@.";
        has_error := true
      | Error e ->
        Stylo.Pipeline.pp_error fn e;
        has_error := true
      | Ok output ->
        let pp oc =
          output_string oc output;
          output_char oc '\n';
          flush oc
        in
        if not !inplace then
          pp stdout
        else
          Out_channel.with_open_text fn pp
    ) !inputs;
  if !has_error then
    exit 1
