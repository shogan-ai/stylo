open Cmdliner
open Cmdliner.Term.Syntax

module Arg = struct
  open Arg

  let single_file =
    pos 0 (some filepath) None (info [])
    |> required

  let files =
    pos_all filepath [] (info [])
    |> value

  let stdin_arg_name = "stdin"
  let stdin =
    info [stdin_arg_name]
      ~doc:"Take file via stdin; pass a name (with extension) to use."
    |> opt (some filepath) None
    |> value

  let ast_check_arg_name = "ast-check"
  let ast_check =
    info [ast_check_arg_name]
      ~doc:"Check the formatted code parses back to the same syntax tree."
    |> flag
    |> value

  let tokens_checks =
    info ["tokens-checks"]
      ~doc:"Performs various tokens-related sanity checks."
    |> flag
    |> value

  let syntax_quotations =
    info ["syntax-quotations"]
      ~doc:"OxCaml specific: enable quotations by default."
    |> flag
    |> value

  let inplace_arg_name = "in-place"
  let inplace =
    info [inplace_arg_name; "i"]
      ~doc:"Replace the file's content with stylo's output."
    |> flag
    |> value

  let width =
    info ["width"; "w"]
    |> opt int 80
    |> value

  let debug =
    let doc =
      Format.sprintf
        "Dumps stylo's internal structures at various stages of the pipeline.\
         For a given input file FILE the files which can appear are: \
         $(i,FOO.parser-tokens), $(i,FOO.normalized-tokens), \
         $(i,FOO.input-tree) and $(i,FOO.output-tree). These last two only \
         appear if $(b,--%s) has been passed and the syntax tree changed as a \
         result of styling. Finally, $(i,FOO.out) can also appear when \
         $(b,--%s) was passed and the output fails to parse."
        ast_check_arg_name ast_check_arg_name
    in
    info ~doc ["debug"]
    |> flag
    |> value
end

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

      | Error `Input_parse_error (_, Ocaml_syntax.Parser_types.Failwith _) ->
        (* ignoring error thrown from semantic actions. *)
        loop_lines (i + 1) ic

      | Error `Input_parse_error _ ->
        (* ignoring entries that don't parse *)
        let oc = Lazy.force parse_error_oc in
        Out_channel.output_string oc entrypoint_and_src;
        Out_channel.output_char oc '\n';
        has_parse_errors := true;
        loop_lines (i + 1) ic

      | Error `Output_parse_error _ ->
        Format.eprintf
          "File %s, line %d: error while parsing stylo's output@\n%s@."
          fn i entrypoint_and_src;
        has_errors := true

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
  if !has_errors
  then Cmd.Exit.some_error
  else Cmd.Exit.ok

type file_kind = Regular | Stdin

let style_input fkind fname =
  let is_mli = Filename.check_suffix fname ".mli" in
  let source =
    match fkind with
    | Regular -> In_channel.(with_open_text fname input_all)
    | Stdin -> In_channel.input_all stdin
  in
  if is_mli
  then Stylo.style_file Intf ~fname source
  else Stylo.style_file Impl ~fname source

let style_files inplace fns =
  let has_error = ref false in
  List.iter (fun (fkind, fn) ->
    match style_input fkind fn with
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
      if not inplace then
        pp stdout
      else
        Out_channel.with_open_text fn pp
  ) fns;
  if !has_error
  then Cmd.Exit.some_error
  else Cmd.Exit.ok

let fuzz_cmd =
  Cmd.make (Cmd.info "fuzz") @@
  let+ fn = Arg.single_file
  and+ quotations = Arg.syntax_quotations in
  Config.(
    check_same_ast := true;
    check_retokenisation := true;
    syntax_quotations := quotations;
  );
  fuzzer_batch fn

let style_cmd =
  Cmd.make (Cmd.info "style") @@
  let open Arg in
  let+ files
  and+ stdin
  and+ inplace
  and+ ast_check
  and+ tokens_checks
  and+ debug
  and+ w = width in
  Config.(
    width := w;
    check_same_ast := ast_check;
    dbg_dump := debug;
    if tokens_checks then (
      check_retokenisation := true;
      check_normalization_kept_comments := true;
    );
  );
  let files = List.map (fun fn -> Regular, fn) files in
  match stdin, inplace with
  | Some _, true ->
    Format.eprintf "Can't pass both --%s and --%s.@."
      stdin_arg_name inplace_arg_name;
    Cmd.Exit.cli_error
  | None, _ -> style_files inplace files
  | Some fn, _ -> style_files inplace @@ (Stdin, fn) :: files

let main () =
  Cmd.group (Cmd.info "stylo") [fuzz_cmd; style_cmd]
  |> Cmd.eval'

let () = exit (main ())
