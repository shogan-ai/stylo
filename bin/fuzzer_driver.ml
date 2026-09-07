(* Copied from ocamlgrammarfuzzer and poorly adapted to local needs: saving
   failing sentences (comments included) to their own files. *)

open Cmdliner
open Cmdliner.Term.Syntax

let default_batch_size = 800
let default_cmd = "stylo"

let batch_by ~size seq =
  assert (size > 0);
  let rec take acc n seq =
    if n = 0 then
      Seq.Cons (List.rev acc, start seq)
    else
      match seq () with
      | Seq.Nil -> Seq.Cons (List.rev acc, Seq.empty)
      | Seq.Cons (x, xs) -> take (x :: acc) (n - 1) xs
  and start seq () =
    match seq () with
    | Seq.Nil -> Seq.Nil
    | Seq.Cons (x, xs) ->
      take [x] (size - 1) xs
  in
  start seq

type 'a pure_queue = {
  head: 'a list;
  tail: 'a list;
}

let empty = {head = []; tail = []}

let push xs x = {xs with tail = x :: xs.tail}

let pop = function
  | {head = x :: xs; tail} -> Some (x, {head = xs; tail})
  | {head = []; tail} ->
    match List.rev tail with
    | [] -> None
    | x :: xs -> Some (x, {head = xs; tail = []})

(* Poor man's work queue: force the sequence [jobs] item ahead *)
let overlapping_force jobs seq =
  let rec initialize queue seq = function
    | 0 -> queue, seq
    | n ->
      match seq () with
      | Seq.Nil -> queue, Seq.empty
      | Seq.Cons (x, seq') -> initialize (push queue x) seq' (n - 1)
  in
  let queue, seq = initialize empty seq jobs in
  let rec reconstruct queue seq () =
    match pop queue with
    | None -> seq ()
    | Some (x, queue') ->
      match seq () with
      | Seq.Nil -> Seq.Cons (x, reconstruct queue' Seq.empty)
      | Seq.Cons (x', seq') ->
        Seq.Cons (x, reconstruct (push queue' x') seq')
  in
  reconstruct queue seq

let consume_batch = function
  | None -> ()
  | Some (fn, pid) ->
    match Unix.waitpid [] pid with
    | _, WEXITED 0 ->
      let no_unix_err f x = try f x with Unix.Unix_error _ -> () in
      (* clean the batch and its sidecar files, if any *)
      no_unix_err Unix.unlink fn;
      no_unix_err Unix.unlink (fn ^ ".parse-errors");
      no_unix_err Unix.rmdir (fn ^ ".failures");
    | _ -> () (* exited abnormally, keeping the file and its sidecars *)

(* Unlike [Filename.get_temp_dir_name () ^ "/formatpinata"], this directory
   is unique to this run: previous runs, possibly concurrent, cannot
   interfere with it. *)
let make_temp_dir ~syntax_quotations =
  let root = Filename.get_temp_dir_name () in
  let prefix =
    if syntax_quotations then "formatpinata-quotations" else "formatpinata"
  in
  let base =
    Printf.sprintf "%s-%d" (Filename.concat root prefix) (Unix.getpid ())
  in
  let rec create n =
    let candidate = if n = 0 then base else Printf.sprintf "%s-%d" base n in
    match Unix.mkdir candidate 0o700 with
    | () -> candidate
    | exception Unix.Unix_error (Unix.EEXIST, _, _) -> create (n + 1)
  in
  create 0

let check ~cmd ~syntax_quotations ~ignore_output_syntax_errors
    ~idempotence_check ~jobs ~batch_size seq =
  let temp_dir = make_temp_dir ~syntax_quotations in
  let temp_path id =
    Filename.concat temp_dir (Printf.sprintf "formatpinata_%d.mls" id)
  in
  let batch_ids = ref 0 in
  let start_batch = function
    | [] -> None
    | inputs ->
      let id = !batch_ids in
      incr batch_ids;
      let path = temp_path id in
      Out_channel.with_open_text path (fun oc ->
        List.iter (fun str ->
          Out_channel.output_string oc str;
          Out_channel.output_char oc '\000'
        ) inputs
      );
      (* Flags are passed through verbatim: no option here has any effect
         on the flags sent to [cmd], other than its own. *)
      let flags =
        (if syntax_quotations then ["--syntax-quotations"] else [])
        @ (if ignore_output_syntax_errors then
            ["--ignore-output-syntax-errors"]
          else [])
        @ (if idempotence_check then ["--idempotence-check"] else [])
      in
      let argv = Array.of_list (cmd :: "fuzz" :: flags @ [path]) in
      let pid = Unix.create_process cmd argv Unix.stdin Unix.stdout Unix.stderr in
      Some (path, pid)
  in
  seq
  |> (* Group by batches of appropriate size *)
  batch_by ~size:batch_size
  |> (* Launch a process for each batch *)
  Seq.map start_batch
  |> (* Force sequence enough items ahead to kick [jobs] processes ahead *)
  overlapping_force jobs
  |> (* Collect the results *)
  Seq.iter consume_batch;
  (* Clean up, unless some batch files were kept because of a failure. *)
  (try Unix.rmdir temp_dir with _ -> ())

let run ~cmd ~batch_size ~jobs ~syntax_quotations ~ignore_output_syntax_errors
    ~idempotence_check input_file =
  In_channel.with_open_text input_file @@ fun ic ->
  let seq =
    Seq.of_dispenser
      (fun () -> Option.map Bytes.unsafe_to_string (Std.read_input ic))
  in
  check ~cmd ~batch_size ~jobs ~syntax_quotations ~ignore_output_syntax_errors
    ~idempotence_check seq

let driver_cmd =
  let open Arg in
  let+ cmd =
    info ["cmd"]
      ~doc:"Command to run on each batch of sentences. It must support \
            stylo's $(b,fuzz) subcommand, as it is invoked as \
            $(b,CMD fuzz [flags] BATCH_FILE)."
      ~docv:"CMD"
    |> opt string default_cmd
    |> value
  and+ batch_size =
    info ["size"; "batch-size"] ~doc:"Number of sentences per batch."
      ~docv:"N"
    |> opt int default_batch_size
    |> value
  and+ jobs =
    info ["jobs"; "j"] ~doc:"Number of stylo processes to run in parallel."
      ~docv:"N"
    |> opt int 1
    |> value
  and+ syntax_quotations =
    info ["syntax-quotations"]
      ~doc:"Pass $(b,--syntax-quotations) to stylo."
    |> flag
    |> value
  and+ ignore_output_syntax_errors =
    info ["ignore-output-syntax-errors"]
      ~doc:"Pass $(b,--ignore-output-syntax-errors) to stylo."
    |> flag
    |> value
  and+ idempotence_check =
    info ["idempotence-check"]
      ~doc:"Pass $(b,--idempotence-check) to stylo: check that formatting \
            the output again is a no-op."
    |> flag
    |> value
  and+ input_file =
    info [] ~doc:"File holding the corpus of NUL-separated sentences to \
                  check."
    |> pos 0 (some filepath) None
    |> required
  in
  run ~cmd ~batch_size ~jobs ~syntax_quotations ~ignore_output_syntax_errors
    ~idempotence_check input_file

let () =
  let info =
    Cmd.info "fuzzer_driver"
      ~doc:"Feed fuzzed sentences to stylo's $(b,fuzz) subcommand."
      ~man:[
        `S "DESCRIPTION";
        `P ("fuzzer_driver reads a corpus of NUL-separated sentences from \
            $(b,FILE), splits it into batches and invokes stylo's \
            $(b,fuzz) subcommand on each batch, in parallel. Batches for \
            which stylo reports a failure are kept on disk for \
            inspection, along with the offending entries, saved one per \
            file in a $(i,<batch>.failures) directory; the others are \
            deleted.")
      ]
  in
  exit (Cmd.eval (Cmd.v info driver_cmd))
