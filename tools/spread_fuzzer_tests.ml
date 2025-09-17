let fuzzer_output = Sys.argv.(1)
let basename = Filename.chop_extension fuzzer_output

let _nb_lines =
  In_channel.with_open_text fuzzer_output @@
  In_channel.fold_lines (fun nb line ->
    let fn = Printf.sprintf "%s-%d" basename nb in
    Out_channel.with_open_text fn @@ fun oc ->
    Out_channel.output_string oc line;
    nb + 1
  ) 0
