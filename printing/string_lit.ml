open! Document
open! Document.Utils

(* A "good enough" approximation of what ocamlformat does.

   Notable differences:
   - we don't have a "preserve" mode
   - we don't interpret format hints (e.g. "@," and "@;")
   - empty lines don't get folded as a '\n' at the end of the previous line:
   that makes ocamlformat go past the width limit in certain cases. *)

let pp_words ?(last_line=false) ?(prefix=empty) words =
  let add_word ?(last=false) sentence word =
    let margin =
      if not last
      then 2 (* we might insert a space and backslash *)
      else if not last_line
      then 1 (* pp_lines might insert a backslash *)
      else 0
    in
    let word =
      nest 1 @@ string (
        if not last
        then word
        else if not last_line
        then word ^ "\\n"
        else word ^ "\""
      )
    in
    match sentence with
    | Empty -> word
    | _ ->
      let flatness = flatness_tracker () in
      let fits = Condition.flat flatness in
      (* A margin of 2 implies that if we are flat, the next word will have
         space for to insert a space and backslash before breaking if it needs
         to.
         If we are not flat because of the margin but would be otherwise, then
         no further word would have fit on the line anyway. So breaking here was
         actually correct. *)
      sentence ^^ group ~margin ~flatness (
        (* If we are flat, then the {| \|} disappears, otherwise it stays and
           will be followed a linebreak. *)
        opt_token ~ws_before:Non_breakable fits "\\" ^^
        break 1 ^^
        word
      )
  in
  let rec aux acc = function
    | [] -> acc
    | [ x ] -> add_word ~last:true acc x
    | x :: xs -> aux (add_word acc x) xs
  in
  aux prefix words

let pp_lines lines =
  let rec aux fits_on_one_line acc = function
    | [] -> acc
    | line :: lines ->
      let first_line = Document.is_empty acc in
      let last_line = List.is_empty lines in
      let words = String.split_on_char ' ' line in
      let acc =
        if first_line
        then
          string "\"" ^^ pp_words ~last_line words
        else
          let prefix =
            if line <> "" && String.get line 0 = ' '
            then opt_token fits_on_one_line "\\"
            else empty
          in
          acc ^^
          opt_token fits_on_one_line "\\" ^^
          break 0 ^^
          pp_words ~last_line ~prefix words
      in
      aux fits_on_one_line acc lines
  in
  let flatness = flatness_tracker () in
  let fits = Condition.flat flatness in
  group ~flatness (aux fits empty lines)

let pp s =
  String.split_on_char '\n' s
  |> pp_lines
  |> formatted_string
