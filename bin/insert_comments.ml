open Ocaml_syntax
open Dbg_print

module T = Tokens
module Doc = Wrapprint

type error =
  | Output_longer_than_input of Doc.document
  | Desynchronized of Lexing.position
  | Missing_token of Lexing.position

let pp_error ppf = function
  | Output_longer_than_input doc ->
    Format.fprintf ppf "Output longer than the input.";
    dprintf "remaining doc: << %a >>@."
      PPrint.ToFormatter.compact (Wrapprint.to_document doc)
  | Desynchronized pos ->
    Format.fprintf ppf "desynchronized at position %d:%d."
      pos.pos_lnum (pos.pos_cnum - pos.pos_bol);
  | Missing_token pos ->
    Format.fprintf ppf
      "token at position %d:%d absent from the output."
      pos.pos_lnum (pos.pos_cnum - pos.pos_bol);

exception Error of error

let append_trailing_comments (tokens, doc, _) =
  let rec aux doc = function
    | []
    | [ T.{ desc = Token EOF; _ } ] -> doc
    | tok :: toks ->
      match tok.T.desc with
      | Comment (c, _) -> aux Doc.(doc ^?^ comment c) toks
      | Token _ -> raise (Error (Missing_token tok.pos))
      | Child_node -> assert false
  in
  aux doc tokens

let rec consume_leading_comments acc = function
  | [] -> acc, []
  | first :: rest ->
    match first.T.desc with
    | Comment (c, _) -> consume_leading_comments Doc.(acc ^?^ comment c) rest
    | Token _ -> acc, rest
    | Child_node -> assert false

let rec consume_only_leading_comments acc = function
  | [] -> acc, []
  | first :: rest ->
    match first.T.desc with
    | Comment (c, _) -> consume_only_leading_comments Doc.(acc ^?^ comment c) rest
    | Token _ -> acc, first :: rest
    | Child_node -> assert false

let rec first_is_comment = function
  | Doc.Comment _ -> `yes
  | Token _ | Token_let -> `no
  | Group d | Align d | Nest (_, d) | Relative_nest (_, d) -> first_is_comment d
  | Empty | Whitespace _ -> `maybe
  | Cat (d1, d2) ->
    match first_is_comment d1 with
    | `maybe -> first_is_comment d2
    | res -> res

let first_is_comment d = first_is_comment d = `yes

let rec attach_before_comments space_before_next tokens doc =
  match tokens with
  | [] -> tokens, doc, space_before_next
  | first :: rest ->
    match first.T.desc with
    | T.Comment (c, Before) ->
      (* FIXME: make sure the comment is not already inserted!! *)
      (* FIXME: if we're attaching several comments, they'll be separated by two
         spaces each.
         Use [consume_leading_comments] instead. *)
      let doc = Doc.(group (doc ^/^ comment c)) in
      attach_before_comments true rest doc
    | _ -> tokens, doc, space_before_next

let rec walk_both ?(space_before_next=false) ?(at_end_of_group=false) seq doc =
  match seq with
  | [] ->
    (* Some extra tokens or comments were synthesized *)
    raise (Error (Output_longer_than_input doc))

  | first :: rest ->
    match first.T.desc, doc with
    (*** Stricter synchro, for debugging purposes ***)
    | T.Token LET, Doc.Token_let ->
      dprintf "synced at %d:%d with LET@."
        first.pos.pos_lnum
        (first.pos.pos_cnum - first.pos.pos_bol);
      let doc = if space_before_next then Doc.(break 1 ^^ doc) else doc in
      if at_end_of_group then
        rest, doc, false
      else
        attach_before_comments false rest doc

    (*** Stricter sync check part 2 ***)
    | T.Token LET, Doc.Token _ ->
      raise (Error (Desynchronized first.pos))

    | (* FIXME: the comment that was explicitely inserted might be the nth one
         from a list of several comments.
         If we drop the first and advance, we will reinsert the token that is
         already there...
         So here we have this shitty check to see whether the comment should be
         skipped or not.

         TODO: improve *)
      T.Comment (c, _), Doc.Comment _
      when Doc.comment c <> doc && Doc.docstring c <> doc ->
      let space_before_doc =
        if space_before_next then Doc.break 1 else Doc.empty
      in
      let rest, doc, add_space_before_next = walk_both rest doc in
      let doc = Doc.(space_before_doc ^^ comment c ^/^ doc) in
      rest, doc, add_space_before_next

    (* Synchronized, advance *)
    | T.Token _, Doc.Token p
    | T.Comment _, Doc.Comment p ->
      dprintf "assume %a synced at %d:%d with << %a >>@."
        Tokens.pp_elt first
        first.pos.pos_lnum
        (first.pos.pos_cnum - first.pos.pos_bol)
        PPrint.ToFormatter.compact p;
      let doc = if space_before_next then Doc.(break 1 ^^ doc) else doc in
      if at_end_of_group then
        rest, doc, false
      else
        attach_before_comments false rest doc

    (* Skip "whitespaces" *)
    | _, Doc.Empty -> seq, doc, space_before_next
    | _, Doc.Whitespace _ -> seq, doc, false

    (* Comments missing in the doc, insert them *)
    | T.Comment _, Doc.(Token_let | Token _) ->
      let space_before_doc =
        if space_before_next then Doc.break 1 else Doc.empty
      in
      let to_prepend, rest = consume_leading_comments Doc.empty seq in
      let doc = Doc.(space_before_doc ^^ to_prepend ^/^ doc) in
      rest, doc, false

    | T.Comment _, Doc.Group d when not (first_is_comment d) ->
      let space_before_doc =
        if space_before_next then Doc.break 1 else Doc.empty
      in
      let to_prepend, rest = consume_only_leading_comments Doc.empty seq in
      let rest, doc, space_before_next =
        walk_both ~at_end_of_group:true rest doc
      in
      let doc = Doc.(space_before_doc ^^ to_prepend ^/^ doc) in
      if at_end_of_group then
        rest, doc, space_before_next
      else
        attach_before_comments space_before_next rest doc

    (* Traverse document structure *)
    | _, Doc.Cat (left, right) ->
      let restl, left, space_before_next =
        walk_both ~space_before_next seq left in
      let restr, right, space_before_next =
        walk_both ~space_before_next ~at_end_of_group restl right in
      restr, Cat (left, right), space_before_next

    (* We're resetting [at_end_of_group] here because, even though we generally
       don't want to append to a group (as that risks breaking its flatness), we
       still want to keep comments in the same nest group as the token they are
       attached to. *)
    | _, Doc.Nest (i, doc) ->
      let rest, doc, space_before_next = walk_both ~space_before_next seq doc in
      rest, Nest (i, doc), space_before_next
    | _, Doc.Relative_nest (i, doc) ->
      let rest, doc, space_before_next = walk_both ~space_before_next seq doc in
      rest, Relative_nest (i, doc), space_before_next

    | _, Doc.Group doc ->
      let space_before_doc =
        if space_before_next then Doc.break 1 else Doc.empty
      in
      let rest, doc, space_before_next =
        walk_both ~at_end_of_group:true seq doc in
      let doc = Doc.(space_before_doc ^^ group doc) in
      if at_end_of_group then
        rest, doc, space_before_next
      else
        attach_before_comments space_before_next rest doc
    | _, Doc.Align doc ->
      let rest, doc, space_before_next =
        walk_both ~space_before_next ~at_end_of_group seq doc in
      rest, Align doc, space_before_next

    (* Token missing from the document: this is a hard error. *)
    | T.Token _, Doc.Comment _ ->
      raise (Error (Missing_token first.pos))

    (*** Stricter sync check part 3 ***)
    | _, Doc.Token_let ->
      raise (Error (Desynchronized first.pos))

    (* [Child_node] doesn't appear in linearized token stream *)
    | T.Child_node, _ -> assert false

let from_tokens tokens doc =
  walk_both tokens doc
  |> append_trailing_comments
