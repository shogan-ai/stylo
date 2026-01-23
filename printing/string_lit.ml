open! Document
open! Document.Utils

(* A "good enough" approximation of what ocamlformat does.

   Notable differences:
   - we don't have a "preserve" mode
   - we don't interpret format hints (e.g. "@," and "@;")
   - empty lines don't always get folded as a '\n' at the end of the previous line as that
     would sometimes go past the width limit. We only fold the empty line when there's
     enough space for it.
*)

let pp_words ?(last_line = false) words =
  let add_word ?(last = false) sentence word =
    let margin =
      if not last
      then 2 (* we might insert a space and backslash *)
      else if not last_line
      then 1 (* pp_lines might insert a backslash *)
      else 0
    in
    let was_space = word = "" in
    let word =
      nest 1
      @@ string
           (if not last
            then word
            else if not last_line
            then word ^ "\\n"
            else word ^ "\"")
    in
    match sentence with
    | Empty -> word
    | _ ->
      let flatness = flatness_tracker () in
      let fits = Condition.flat flatness in
      let potential_escape =
        if last_line || last || not was_space
        then empty
        else opt_token fits "\\"
      in
      (* If we are not flat because of the margin but would be otherwise, then no further
         word would have fit on the line anyway. So breaking here was actually correct. *)
      sentence
      ^^ group
           ~margin
           ~flatness
           ((* If we are flat, then the backslash disappears, otherwise it stays and will
               be followed a linebreak. *)
            opt_token
              ~ws_before:nbsp
              fits
              "\\"
            ^^ break 1
            ^^ potential_escape
            ^^ word)
  in
  let rec aux acc = function
    | [] -> acc
    | [ x ] -> add_word ~last:true acc x
    | x :: xs -> aux (add_word acc x) xs
  in
  aux empty words
;;

let pp_lines lines =
  let rec aux fits_on_one_line acc = function
    | [] -> acc
    | line :: lines ->
      let first_line = Document.is_empty acc in
      let last_line = List.is_empty lines in
      let words = String.split_on_char ' ' line in
      let acc =
        if first_line
        then string "\"" ^^ pp_words ~last_line words
        else if line = ""
        then (
          let flatness = flatness_tracker () in
          let folded_with_previous = Condition.flat flatness in
          (* margin needed because we might be followed by '\\' or '"' *)
          acc
          ^^ group
               ~flatness
               ~margin:1
               (opt_token folded_with_previous "\\"
                ^^ break 0
                ^^ pp_words ~last_line words))
        else (
          let prefix =
            if String.get line 0 = ' '
            then opt_token fits_on_one_line "\\"
            else empty
          in
          acc
          ^^ opt_token fits_on_one_line "\\"
          ^^ break 0
          ^^ prefix
          ^^ pp_words ~last_line words)
      in
      aux fits_on_one_line acc lines
  in
  let flatness = flatness_tracker () in
  let fits = Condition.flat flatness in
  group ~flatness (aux fits empty lines)
;;

let pp s = String.split_on_char '\n' s |> pp_lines |> formatted_string
let pp s = pp s
