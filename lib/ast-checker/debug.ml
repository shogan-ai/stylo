type tokens_source =
  | Parser
  | Normalization

let dump_to_file fname ~or_ to_ppf =
  if not !Config.dbg_dump then or_ else (
    Out_channel.with_open_text fname @@ fun oc ->
    let ppf = Format.formatter_of_out_channel oc in
    let res = to_ppf ppf in
    Format.pp_print_flush ppf ();
    res
  )

let dump_tokens input_name ~src tokens_lazy =
  let fname =
    input_name ^
    match src with
    | Parser -> ".parser-tokens"
    | Normalization -> ".normalized-tokens"
  in
  dump_to_file fname ~or_:(Ok ()) (fun ppf ->
    Lazy.force tokens_lazy
    |> Result.map (Ocaml_syntax.Tokens.dump ppf)
  )
