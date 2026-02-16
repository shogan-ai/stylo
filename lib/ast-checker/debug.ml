type tokens_source =
  | Parser
  | Normalization

let dump_to_file fname to_ppf =
  if !Config.dbg_dump then (
    Out_channel.with_open_text fname @@ fun oc ->
    let ppf = Format.formatter_of_out_channel oc in
    to_ppf ppf;
    Format.pp_print_flush ppf ()
  )

let dump_tokens input_name ~src tokens_lazy =
  let fname =
    input_name ^
    match src with
    | Parser -> ".parser-tokens"
    | Normalization -> ".normalized-tokens"
  in
  dump_to_file fname (fun ppf ->
    Lazy.force tokens_lazy
    |> Ocaml_syntax.Tokens.dump ppf
  )
