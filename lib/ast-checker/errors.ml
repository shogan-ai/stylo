type parser =
  | Stylo's
  | Oxcaml's

type t = [
  | `Input_parse_error of parser * exn
  | `Output_parse_error of parser * exn
  | `Ast_changed of parser * string
]

let report_parse_error ppf exn =
  let module Loc = Oxcaml_frontend.Location in
  match Loc.error_of_exn exn with
  | Some `Already_displayed -> ()
  | Some `Ok report ->
    Format.fprintf ppf "%a" Loc.print_report report
  | None ->
    Format.fprintf ppf "%s" (Printexc.to_string exn)

let pp_error : t -> _ = function
  | `Ast_changed (parser, fname) ->
    Format.eprintf "%s: %cst changed@." fname
      (if parser = Stylo's then 'c' else 'a')
  | `Input_parse_error (Stylo's, exn) ->
    Format.eprintf "@[<v>Input doesn't parse:@;%s@]@."
      (Printexc.to_string exn)
  | `Input_parse_error (Oxcaml's, exn) ->
    Format.eprintf
      "@[<v>Input doesn't parse with upstream's parser:@;@[<hov 2>%a@]@]@."
      report_parse_error exn
  | `Output_parse_error (Stylo's, exn) ->
    Format.eprintf "@[<v>Output doesn't reparse:@;%s@]@."
      (Printexc.to_string exn)
  | `Output_parse_error (Oxcaml's, exn) ->
    Format.eprintf
      "@[<v>Error while parsing the output with upstream's parser:@;\
       @[<hov 2>%a@]@]@."
      report_parse_error exn

