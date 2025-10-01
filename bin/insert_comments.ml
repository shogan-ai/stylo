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

let rec consume_only_leading_comments ?(restrict_to_before=false) acc = function
  | [] -> acc, []
  | first :: rest ->
    match first.T.desc with
    | Comment (c, attached) ->
      if restrict_to_before && attached <> Before then
        acc, first :: rest
      else
        consume_only_leading_comments Doc.(acc ^?^ comment c) rest
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

type state = {
  space_needed_before_next: bool;
  (** Whether a space should be inserted before the next leaf node (token or
      comment).

      This flag is set when a comment is inserted, and it is reset when a space
      is inserted (either manually, or because we encountered a [Whitespace]
      node).

      FIXME: we also need the "reverse": we need to know when a space needs to
      be added before a comment we're inserting... *)

  at_end_of_a_group: bool;
  (** This is [true] for the rightmost branch under a [Group] node.
      In that situation we delay comment insertion: appending at the end of a
      group might make it non flat. *)

  nesting_difference_with_previous_token: int;
  (** When we "attach" a comment to the token that preceeds it, we expect it to
      be at the same nesting level as that token.
      However, since we sometimes delay comment insertion (c.f.
      {!at_end_of_group}) we might have gone out of the [Nest] subtree when we
      finally insert the comment.
      Keeping track of the nesting level of the previous token enables us to
      recreate the correct [Nest] node to place the comment under.

      FIXME: symmetrically, we sometimes eagerly insert a comment before going
      under a group (and the subsequent nests).
      But if the comment is "attached" to the next token, then we need to keep
      track of the nesting difference with that token. *)
}

let init_state =
  { space_needed_before_next = false
  ; at_end_of_a_group = false
  ; nesting_difference_with_previous_token = 0 }

let no_space st = { st with space_needed_before_next = false }

let incr_nesting st n =
  { st with
    nesting_difference_with_previous_token =
      st.nesting_difference_with_previous_token + n }

let decr_nesting st n =
  { st with
    nesting_difference_with_previous_token =
      max 0 (st.nesting_difference_with_previous_token - n) }

let reset_nesting st = { st with nesting_difference_with_previous_token = 0 }

let is_comment_attaching_before elt =
  match elt.T.desc with
  | Comment (_, Before) -> true
  | _ -> false

let attach_before_comments state tokens doc =
  if state.at_end_of_a_group then
    tokens, doc, state
  else
    let () = dprintf "not at end of group, attaching!@." in
    match List.take_while is_comment_attaching_before tokens with
    | [] ->
      (* no comment to attach *)
      tokens, doc, reset_nesting state
    | to_append ->
      let tokens = List.drop_while is_comment_attaching_before tokens in
      let doc =
        let open Doc in
        dprintf "attaching to preceeding!@.";
        group @@ List.fold_left (fun acc cmt ->
          match cmt.T.desc with
          | Comment (c, _) ->
            acc ^^
            nest state.nesting_difference_with_previous_token
              (group (break 1 ^^ comment c))
          | _ -> assert false
        ) doc to_append
      in
      tokens, doc,
      { state with
        space_needed_before_next = true;
        nesting_difference_with_previous_token = 0 }

let insert_space_if_required state doc =
  if state.space_needed_before_next
  then Doc.(break 1 ^^ doc)
  else doc

let rec walk_both state seq doc =
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
      let doc = insert_space_if_required state doc in
      attach_before_comments (no_space state) rest doc

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
      let rest, doc, state' = walk_both (no_space state) rest doc in
      (* FIXME: nesting?! *)
      let doc = insert_space_if_required state Doc.(comment c ^/^ doc) in
      rest, doc, state'

    (* Synchronized, advance *)
    | T.Token _, Doc.Token p
    | T.Comment _, Doc.Comment p ->
      dprintf "assume %a synced at %d:%d with << %a >>@."
        Tokens.pp_elt first
        first.pos.pos_lnum
        (first.pos.pos_cnum - first.pos.pos_bol)
        PPrint.ToFormatter.compact p;
      let doc = insert_space_if_required state doc in
      attach_before_comments (no_space state) rest doc

    (* Skip "whitespaces" *)
    | _, Doc.Empty -> seq, doc, state
    | _, Doc.Whitespace _ -> seq, doc, no_space state

    (* Comments missing in the doc, insert them *)
    | T.Comment _, Doc.(Token_let | Token _) ->
      let to_prepend, rest = consume_leading_comments Doc.empty seq in
      dprintf "inserting before token!!@.";
      let doc = insert_space_if_required state Doc.(to_prepend ^/^ doc) in
      rest, doc, no_space state

    | T.Comment _, Doc.Group d when not (first_is_comment d) ->
      let to_prepend, rest = consume_only_leading_comments Doc.empty seq in
      dprintf "inserting before group!!@.";
      let rest, doc, state' =
        walk_both
          { state with
            space_needed_before_next = false;
            at_end_of_a_group = true }
          rest doc
      in
      let doc = insert_space_if_required state Doc.(to_prepend ^/^ doc) in
      attach_before_comments state' rest doc

    (* Traverse document structure *)
    | _, Doc.Cat (left, right) ->
      let restl, left, mid_state =
        walk_both { state with at_end_of_a_group = false } seq left
      in
      let restr, right, final_state =
        walk_both { mid_state with at_end_of_a_group = state.at_end_of_a_group }
          restl right
      in
      restr, Cat (left, right), final_state

    | _, Doc.Nest (i, doc) ->
      let rest, doc, state = walk_both (decr_nesting state i) seq doc in
      rest, Nest (i, doc), incr_nesting state i
    | _, Doc.Relative_nest (i, doc) ->
      let rest, doc, state = walk_both (decr_nesting state i) seq doc in
      rest, Relative_nest (i, doc), incr_nesting state i

    | _, Doc.Group doc ->
      let rest, doc, state' =
        walk_both
          { state with
            space_needed_before_next = false;
            at_end_of_a_group = true }
          seq doc
      in
      let return_state =
        { state' with at_end_of_a_group = state.at_end_of_a_group }
      in
      let doc = insert_space_if_required state (Doc.group doc) in
      attach_before_comments return_state rest doc

    | _, Doc.Align doc ->
      let rest, doc, state = walk_both state seq doc in
      rest, Align doc, state

    (* Token missing from the document: this is a hard error. *)
    | T.Token _, Doc.Comment _ ->
      raise (Error (Missing_token first.pos))

    (*** Stricter sync check part 3 ***)
    | _, Doc.Token_let ->
      raise (Error (Desynchronized first.pos))

    (* [Child_node] doesn't appear in linearized token stream *)
    | T.Child_node, _ -> assert false

let from_tokens tokens doc =
  walk_both init_state tokens doc
  |> append_trailing_comments
