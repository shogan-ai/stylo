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

  let idempotence_check_arg_name = "idempotence-check"
  let idempotence_check =
    info [idempotence_check_arg_name]
      ~doc:"Check that calling the tool again on the output produces the same \
            result."
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

  let ignore_syntax_errors =
    info ["ignore-output-syntax-errors"]
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

  let erase_jst_syntax =
    info ["erase-jane-syntax"]
      ~doc:"Erase OxCaml extensions from the output"
    |> flag
    |> value

  let remove_parentheses =
    info ["remove-parentheses"]
      ~doc:"Remove unnecessary parentheses from the output"
    |> flag
    |> value

  let insert_parentheses =
    info ["insert-parentheses"]
      ~doc:"Ensure all expressions are parenthesized in the output"
    |> flag
    |> value
end

let do_style is_mli fname ?normalize ?lnum source =
  if is_mli
  then Stylo.style_file Intf ~fname ?normalize ?lnum source
  else Stylo.style_file Impl ~fname ?normalize ?lnum source

let fuzzer_batch ~quiet ~idempotence_check fn =
  let has_errors = ref false in
  let parse_errors = ref 0 in
  let entries_checked = ref 0 in
  (* Only create the file when there are parse errors *)
  let parse_error_oc = lazy (Out_channel.open_text (fn ^ ".parse-errors")) in
  (* Failing entries are saved, one file per entry, in a [fn ^ ".failures"]
     directory; entries are stripped of their entrypoint prefix and get a
     [.ml] or [.mli] extension so that they can be replayed with
     [stylo style --idempotence-check FILE] directly. *)
  let failures_dir = fn ^ ".failures" in
  let failure_count = ref 0 in
  let save_failure entrypoint_and_src =
    incr failure_count;
    (try Unix.mkdir failures_dir 0o750 with
     | Unix.Unix_error (Unix.EEXIST, _, _) -> ());
    let intf, source = Stylo.split_fuzzer_line entrypoint_and_src in
    let fname =
      Printf.sprintf "%s/%04d%s" failures_dir !failure_count
        (if intf then ".mli" else ".ml")
    in
    Out_channel.with_open_text fname (fun oc ->
      Out_channel.output_string oc source;
      if source <> "" && source.[String.length source - 1] <> '\n' then
        Out_channel.output_char oc '\n'
    );
    has_errors := true
  in
  let rec loop_entries lnum ic =
    match Std.read_input ic with
    | None -> () (* done *)
    | Some bytes ->
      let entrypoint_and_src = Bytes.unsafe_to_string bytes in
      incr entries_checked;
      let next_lnum =
        String.fold_left (fun ln c -> ln + if c = '\n' then 1 else 0)
          lnum entrypoint_and_src
      in
      match Stylo.style_fuzzer_line ~fname:fn ~lnum entrypoint_and_src with
      | Ok fst_round ->
        if idempotence_check then begin
          let intf, _ = Stylo.split_fuzzer_line entrypoint_and_src in
          match do_style intf fn ~normalize:false ~lnum fst_round with
          | Ok snd_round when fst_round = snd_round -> ()
          | _ -> save_failure entrypoint_and_src
        end;
        loop_entries next_lnum ic

      | Error `Input_parse_error (_, _, _, Ocaml_syntax.Parser_types.Failwith _) ->
        (* ignoring error thrown from semantic actions. *)
        loop_entries next_lnum ic

      | Error `Input_parse_error _ ->
        (* ignoring entries that don't parse *)
        let oc = Lazy.force parse_error_oc in
        Out_channel.output_string oc entrypoint_and_src;
        Out_channel.output_char oc '\n';
        incr parse_errors;
        loop_entries next_lnum ic

      | Error `Output_parse_error _ ->
        (* when [--ignore-output-syntax-errors] is passed we explicitely
           skip those as we're only interested in idempotence issues when the
           flag is passed. *)
        if not quiet then
          save_failure entrypoint_and_src;
        loop_entries next_lnum ic

      | Error e ->
        let fname = fn ^ ":" ^ string_of_int lnum in
        Stylo.Pipeline.pp_error Format.err_formatter fname e;
        save_failure entrypoint_and_src;
        loop_entries next_lnum ic

      | exception exn ->
        Format.eprintf "%s, line %d: uncaught exception: %s@." fn lnum
          (Printexc.to_string exn);
        if Dbg_print.dbg then Printexc.print_backtrace stderr;
        save_failure entrypoint_and_src;
        loop_entries next_lnum ic
  in
  In_channel.with_open_text fn (loop_entries 1);
  if !parse_errors > 0 then (
    Format.eprintf "Parse errors collected in %s.parse-errors@." fn;
    Out_channel.close (Lazy.force parse_error_oc)
  );
  if !failure_count > 0 then
    Format.eprintf
      "Checked %d entries from %s: %d did not parse, %d failing entries \
       saved in %s@."
      !entries_checked fn !parse_errors !failure_count failures_dir
  else
    Format.eprintf "Checked %d entries from %s: %d did not parse@."
      !entries_checked fn !parse_errors;
  if !has_errors
  then Cmd.Exit.some_error
  else Cmd.Exit.ok

type file_kind = Regular | Stdin

let style_input check_idempotence fkind fname =
  let is_mli = Filename.check_suffix fname ".mli" in
  let source =
    match fkind with
    | Regular -> In_channel.(with_open_text fname input_all)
    | Stdin -> In_channel.input_all stdin
  in
  let result = do_style is_mli fname source in
  if not check_idempotence then
    result
  else
    let (let*) = Result.bind in
    let* fst_round = result in
    if fst_round = source then
      (* input might already have been formatted *)
      result
    else (* general case, we styled the input, next round ought to be a noop *)
      let* snd_round = do_style is_mli fname fst_round in
      if fst_round = snd_round
      then result
      else Error `Not_idempotent

let style_files check_idempotence inplace fns =
  let has_error = ref false in
  List.iter (fun (fkind, fn) ->
    match style_input check_idempotence fkind fn with
    | exception exn ->
      let bt = Printexc.get_backtrace () in
      Format.eprintf "%s: %s" fn (Printexc.to_string exn);
      if Dbg_print.dbg then
        Format.eprintf "@\n%s@." bt
      else
        Format.eprintf "@.";
      has_error := true
    | Error `Not_idempotent ->
      Format.eprintf
        "%s: `--idempotence-check` failure: output is not stable@." fn;
      has_error := true
    | Error (#Stylo.Pipeline.error as e) ->
      Stylo.Pipeline.pp_error Format.err_formatter fn e;
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
  let open Arg in
  let+ fn = single_file
  and+ quiet = ignore_syntax_errors
  and+ idempotence_check
  and+ quotations = syntax_quotations in
  Config.(
    check_same_ast := true;
    check_retokenisation := true;
    syntax_quotations := quotations;
  );
  fuzzer_batch ~quiet ~idempotence_check fn

let style_cmd =
  Cmd.make (Cmd.info "style") @@
  let open Arg in
  let+ files
  and+ stdin
  and+ inplace
  and+ ast_check
  and+ idempotence_check
  and+ tokens_checks
  and+ debug
  and+ erase_jst_syntax
  and+ remove_parentheses
  and+ insert_parentheses
  and+ quotations = Arg.syntax_quotations
  and+ w = width in
  Config.(
    width := w;
    check_same_ast := ast_check;
    dbg_dump := debug;
    erase_jane_syntax := erase_jst_syntax;
    parentheses_insert := insert_parentheses;
    parentheses_remove := remove_parentheses;
    syntax_quotations := quotations;
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
  | None, _ -> style_files idempotence_check inplace files
  | Some fn, _ -> style_files idempotence_check inplace @@ (Stdin, fn) :: files

let main () =
  Cmd.group (Cmd.info "stylo") [fuzz_cmd; style_cmd]
  |> Cmd.eval'

let () = exit (main ())
