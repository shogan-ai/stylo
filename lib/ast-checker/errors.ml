type parser =
  | Stylo's
  | Oxcaml's

type t = [
  | `Input_parse_error of parser * Lexing.position * Lexing.position * exn
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

let gnu_position startp endp =
  let column pos =
    if pos.Lexing.pos_cnum > -1 then
      1 + pos.pos_cnum - pos.pos_bol
    else
      1
  in
  if startp = Lexing.dummy_pos then
    "stylo"
  else if endp <> Lexing.dummy_pos && endp <> startp then (
    if startp.pos_fname <> endp.pos_fname then
      Printf.sprintf "%s:%d.%d-%s:%d.%d"
        startp.pos_fname startp.pos_lnum (column startp)
        endp.pos_fname endp.pos_lnum (column endp)
    else if startp.pos_lnum = endp.pos_lnum then
      Printf.sprintf "%s:%d.%d-%d"
        startp.pos_fname startp.pos_lnum (column startp) (column endp)
    else if startp.pos_cnum > -1 && endp.pos_cnum > -1 then
      Printf.sprintf "%s:%d.%d-%d.%d"
        startp.pos_fname startp.pos_lnum (column startp)
        endp.pos_lnum (column endp)
    else
      Printf.sprintf "%s:%d-%d" startp.pos_fname startp.pos_lnum endp.pos_lnum
  ) else (
    if startp.pos_cnum > -1 then
      Printf.sprintf "%s:%d.%d" startp.pos_fname startp.pos_lnum (column startp)
    else if startp.pos_lnum > 0 then
      Printf.sprintf "%s:%d" startp.pos_fname startp.pos_lnum
    else
      Printf.sprintf "%s:1" startp.pos_fname
  )


let pp_error ppf fname : t -> _ = function
  | `Ast_changed (parser, fname) ->
    Format.fprintf ppf "%s: %cst changed@." fname
      (if parser = Stylo's then 'c' else 'a')
  | `Input_parse_error (Stylo's, startp, endp, exn) ->
    Format.fprintf ppf "%s: @[<v>Input doesn't parse:@;%s@]@."
      (gnu_position startp endp) (Printexc.to_string exn)
  | `Input_parse_error (Oxcaml's, startp, endp, exn) ->
    Format.fprintf ppf
      "%s: @[<v>Input doesn't parse with upstream's parser:@;@[<hov 2>%a@]@]@."
      (gnu_position startp endp) report_parse_error exn
  | `Output_parse_error (Stylo's, exn) ->
    Format.fprintf ppf "%s: @[<v>Output doesn't reparse:@;%s@]@."
      fname (Printexc.to_string exn)
  | `Output_parse_error (Oxcaml's, exn) ->
    Format.fprintf ppf
      "%s: @[<v>Error while parsing the output with upstream's parser:@;\
       @[<hov 2>%a@]@]@."
      fname report_parse_error exn
