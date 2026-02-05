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


let default_batch_size = 800
let syntax_quotations = ref false

let batch_size = ref default_batch_size
let cmd = ref "ocamlformat"
let jobs = ref 0
let input_file = ref ""

let () =
  Arg.parse
    ["--cmd", Arg.Set_string cmd, "default = ocamlformat"
    ;"--size", Arg.Set_int batch_size, "default = 800"
    ;"--syntax-quotations", Arg.Set syntax_quotations, ""
    ;"-j", Arg.Set_int jobs, "number of parallel jobs, default = 1"]
    ((:=) input_file)
    "fuzzer_driver.exe"


let temp_dir =
  let root = Filename.get_temp_dir_name () in
  let sub =
    Filename.concat root
      (if !syntax_quotations then "formatpinata-quotations" else "formatpinata")
  in
  Unix.mkdir sub 0o770;
  sub

let temp_path id =
  Filename.concat temp_dir (Printf.sprintf "formatpinata_%d.mls" id)

(*
let temp_path_index name =
  let name = Filename.remove_extension (Filename.basename name) in
  Scanf.sscanf_opt name "formatpinata_%d-%d" (fun _id index -> index)
*)

let batch_ids = ref 0

let start_batch ~ocamlformat_command = function
  | [] -> None
  | inputs ->
    let id = !batch_ids in
    incr batch_ids;
    let path = temp_path id in
    Out_channel.with_open_text path (fun oc ->
      List.iter (fun src ->
        Out_channel.output_string oc src;
        Out_channel.output_char oc '\n'
      ) inputs
    );
    let process =
      let args =
        if !syntax_quotations
        then [|ocamlformat_command; "-fuzzing"; "-syntax-quotations"; path|]
        else [|ocamlformat_command; "-fuzzing"; path|]
      in
      Unix.create_process ocamlformat_command args
        Unix.stdin Unix.stdout Unix.stderr
    in
    Some (path, process)

let consume_batch = function
  | None -> ()
  | Some (fn, pid) ->
    match Unix.waitpid [] pid with
    | _, WEXITED 0 ->
      begin try Unix.unlink fn with _ -> () end;
    | _ -> () (* exited abnormally, keeping the file *)

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

let check ~ocamlformat_command ~jobs ~batch_size seq =
  seq
  |> (* Group by batches of appropriate size *)
  batch_by ~size:batch_size
  |> (* Launch a process for each batch *)
  Seq.map (start_batch ~ocamlformat_command)
  |> (* Force sequence enough items ahead to kick [jobs] processes ahead *)
  overlapping_force jobs
  |> (* Collect the results *)
  Seq.iter consume_batch

let () =
  assert (!input_file <> "");
  In_channel.with_open_text !input_file @@ fun ic ->
  let seq = Seq.of_dispenser (fun () -> In_channel.input_line ic) in
  check ~ocamlformat_command:!cmd ~batch_size:!batch_size ~jobs:!jobs seq
