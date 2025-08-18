let ic = In_channel.open_text Sys.argv.(1)
let () = at_exit (fun () -> In_channel.close ic)

let comments =
  let open Ocaml_syntax in
  Lexer.init ();
  let lexbuf = Lexing.from_channel ic in
  while Lexer.token lexbuf <> EOF do () done;
  Lexer.comments ()

let max_chunk_sz = 4096
let buffer = Bytes.create max_chunk_sz

let rec do_until ~transfer ic oc stop_offset curr_offset =
  if curr_offset + max_chunk_sz <= stop_offset then (
    let read = In_channel.input ic buffer 0 max_chunk_sz in
    if transfer then
      Out_channel.output oc buffer 0 read;
    if read <> 0 then (* 0 means EOF *)
      do_until ~transfer ic oc stop_offset (curr_offset + read)
  ) else (
    let chunk_sz = stop_offset - curr_offset in
    let read = In_channel.input ic buffer 0 chunk_sz in
    if transfer then
      Out_channel.output oc buffer 0 read
  )

let transfer_until = do_until ~transfer:true
let skip_until = do_until ~transfer:false

let rec strip_comments ic oc offset = function
  | [] ->
    (* no more comments, transfer til EOF *)
    transfer_until ic oc max_int offset
  | (_, (next_cmt_loc : Ocaml_syntax.Location.t)) :: cmts ->
    let cmt_start = next_cmt_loc.loc_start.pos_cnum in
    let cmt_stop = next_cmt_loc.loc_end.pos_cnum in
    transfer_until ic oc cmt_start offset;
    skip_until ic oc cmt_stop cmt_start;
    strip_comments ic oc cmt_stop cmts

let () =
  In_channel.seek ic 0L;
  strip_comments ic stdout 0 comments
