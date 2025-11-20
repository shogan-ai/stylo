open! Document
open! Document.Utils

let split_lines s =
  let accumulate_merging_blank rev_lines = function
    | "" ->
      (* FIXME: ocamlformat does not merge the last line if it is blank but
         keeps it the list.
         Why? *)
      begin match rev_lines with
      | [] -> [""]
      | last :: before -> (last ^ "\\n") :: before
      end
    | line -> line :: rev_lines
  in
  String.split_on_char '\n' s
  |> List.fold_left accumulate_merging_blank []
  |> List.rev

let split_words s = String.split_on_char ' ' s

let pp_words =
  let add_word sentence word =
    let word = string word in
    match sentence with
    | Empty -> word
    | _ ->
      let flatness = flatness_tracker () in
      let fits = Condition.flat flatness in
      sentence ^^ group ~flatness (
        (* If we are flat, then the {| \|} disappears, otherwise it stays and
           will be followed a linebreak. *)
        opt_token ~ws_before:Non_breakable fits "\\" ^^
        break 1 ^^
        word
      )
  in
  List.fold_left add_word empty

let pp_line s =
  split_words s
  |> pp_words
  |> nest 1

let pp s =
  let lines =
    (* we add the closing dquote here, so it makes it into the same [group] as
       the last word. *)
    split_lines (s ^ "\"")
  in
  let str = string "\"" ^^ separate_map hardline pp_line lines in
  formatted_string str
