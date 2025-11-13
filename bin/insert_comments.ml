open Ocaml_syntax
open Dbg_print

module T = Tokens
module Doc = Document

type error =
  | Output_longer_than_input of Doc.t
  | Missing_token of Lexing.position
  | Optional_mismatch of Lexing.position

let pp_error ppf = function
  | Output_longer_than_input doc ->
    Format.fprintf ppf "Output longer than the input.";
    dprintf "remaining doc: << %s >>@."
      (Document.Print.to_string ~width:80 doc)
  | Missing_token pos ->
    Format.fprintf ppf
      "token at position %d:%d absent from the output."
      pos.pos_lnum (pos.pos_cnum - pos.pos_bol);
  | Optional_mismatch pos ->
    Format.fprintf ppf
      "printer changed optional status of token at position %d:%d."
      pos.pos_lnum (pos.pos_cnum - pos.pos_bol);

exception Error of error

let append_trailing_comments (tokens, doc, _) =
  let rec aux doc = function
    | []
    | [ T.{ desc = Token EOF; _ } ] -> doc
    | tok :: toks ->
      match tok.T.desc with
      | Comment c ->
        let doc =
          if !(c.explicitely_inserted)
          then doc
          else Doc.Utils.(doc ^?^ Doc.comment c.text)
        in
        aux doc toks
      | Token _
      | Opt_token _ -> raise (Error (Missing_token tok.pos))
      | Child_node -> assert false
  in
  aux doc tokens

let rec consume_leading_comments acc = function
  | [] -> acc, []
  | first :: rest ->
    match first.T.desc with
    | Comment c when not !(c.explicitely_inserted) ->
      consume_leading_comments Doc.Utils.(acc ^?^ Doc.comment c.text) rest
    | Comment _
    | Token _
    | Opt_token _ -> acc, rest
    | Child_node -> assert false

let rec consume_only_leading_comments ?(restrict_to_before=false) acc = function
  | [] -> acc, []
  | first :: rest ->
    match first.T.desc with
    | Comment c ->
      if !(c.explicitely_inserted) ||
         restrict_to_before && c.attachement <> Before
      then
        acc, first :: rest
      else
        consume_only_leading_comments Doc.Utils.(acc ^?^ Doc.comment c.text) rest
    | Token _
    | Opt_token _ -> acc, first :: rest
    | Child_node -> assert false

let rec first_is_comment = function
  | Doc.Comment _ -> `yes
  | Token _ | Optional _ -> `no
  | Group (_, _, d) | Nest (_, _, d) ->
    first_is_comment d
  | Empty | Whitespace _ -> `maybe
  | Cat (_, d1, d2) ->
    match first_is_comment d1 with
    | `maybe -> first_is_comment d2
    | res -> res

let first_is_comment d = first_is_comment d = `yes

let rec first_is_space = function
  | Doc.Whitespace _ -> `yes
  | Token _ | Comment _ -> `no
  | Optional o -> if Option.is_some o.before then `yes else `no
  | Group (_, _, d) | Nest (_, _, d) ->
    first_is_space d
  | Empty -> `maybe
  | Cat (_, d1, d2) ->
    match first_is_space d1 with
    | `maybe -> first_is_space d2
    | res -> res

let first_is_space d = first_is_space d = `yes

let rec nest_before_leaf = function
  | Doc.Nest _ -> `yes
  | Token _ | Optional _ | Comment _ | Whitespace _ -> `no
  | Group (_, _, d) -> nest_before_leaf d
  | Empty -> `maybe
  | Cat (_, d1, d2) ->
    match nest_before_leaf d1 with
    | `maybe -> nest_before_leaf d2
    | res -> res

let nest_before_leaf d = nest_before_leaf d = `yes

type space_needed = No | Always | Only_if_inserting_comment

type state = {
  space_needed_before_next: space_needed;
  (** Whether a space should be inserted before the next leaf node (token or
      comment).

      This flag is set when a comment is inserted, and it is reset when a space
      is inserted (either manually, or because we encountered a [Whitespace]
      node). *)

  at_end_of_a_group: bool;
  (** This is [true] for the rightmost branch under a [Group] node.
      In that situation we delay comment insertion: appending at the end of a
      group might make it non flat. *)
}

let init_state =
  { space_needed_before_next = No
  ; at_end_of_a_group = false }

let under_nest st = { st with at_end_of_a_group = false }
let exit_nest prev st = { st with at_end_of_a_group = prev.at_end_of_a_group }

let no_space st = { st with space_needed_before_next = No }
let saw_leaf st =
  { st with space_needed_before_next = Only_if_inserting_comment }

let is_comment_attaching_before elt =
  match elt.T.desc with
  | Comment c -> c.attachement = Before
  | _ -> false

let attach_before_comments state tokens doc =
  if state.at_end_of_a_group then
    tokens, doc, state
  else
    match List.take_while is_comment_attaching_before tokens with
    | [] ->
      (* no comment to attach *)
      tokens, doc, state
    | to_append ->
      let tokens = List.drop_while is_comment_attaching_before tokens in
      let doc =
        let open Doc in
        group @@ List.fold_left (fun acc cmt ->
          match cmt.T.desc with
          | Comment c ->
            if !(c.explicitely_inserted)
            then acc
            else acc ^^ group (break 1 ^^ comment c.text)
          | _ -> assert false
        ) doc to_append
      in
      tokens, doc, { state with space_needed_before_next = Always }

let insert_space_if_required ?(inserting_comment=false) state doc =
  let sn = state.space_needed_before_next in
  if sn = Always || (sn = Only_if_inserting_comment && inserting_comment)
  then Doc.(break 1 ^^ doc)
  else doc

let rec walk_both state seq doc =
  match seq with
  | [] ->
    (* Some extra tokens or comments were synthesized *)
    raise (Error (Output_longer_than_input doc))

  | first :: rest ->
    match first.T.desc, doc with
    (* Synchronized, advance *)
    | T.Token _, Doc.Token p
    | T.Opt_token _, Doc.Optional { token = p; _ } ->
      dprintf "assume %a synced at %d:%d with << %s >>@."
        Tokens.pp_elt first
        first.pos.pos_lnum
        (first.pos.pos_cnum - first.pos.pos_bol)
        p;
      let doc = insert_space_if_required state doc in
      attach_before_comments (saw_leaf state) rest doc

    (* Skip "whitespaces" *)
    | _, Doc.Empty -> seq, doc, state
    | _, Doc.Whitespace _ -> seq, doc, no_space state

    (* Skip explicitely inserted comment *)
    | _, Doc.Comment _ -> seq, doc, state
    | T.Comment { explicitely_inserted; _ }, _ when !explicitely_inserted ->
      walk_both state rest doc

    (* Comments missing in the doc, insert them *)
    | T.Comment _, Doc.(Token _ | Optional _) ->
      let to_prepend, rest = consume_leading_comments Doc.empty seq in
      let doc =
        insert_space_if_required ~inserting_comment:true state
          Doc.Utils.(to_prepend ^/^ doc)
      in
      rest, doc, no_space state

    | T.Comment _, Doc.Group (_, _, d)
      when
        not (nest_before_leaf d) (* traverse group to reach correct nesting *)
        && not (first_is_comment d) (* might be the same comment *) ->
      let to_prepend, rest = consume_only_leading_comments Doc.empty seq in
      let rest, doc, state' =
        walk_both { space_needed_before_next = No; at_end_of_a_group = true }
          rest doc
      in
      let doc =
        let doc =
          if first_is_space doc
          then Doc.(to_prepend ^^ doc)
          else Doc.Utils.(to_prepend ^/^ doc)
        in
        insert_space_if_required ~inserting_comment:true state doc
      in
      attach_before_comments state' rest doc

    (* Traverse document structure *)
    | _, Doc.Cat (_, left, right) ->
      let restl, left, mid_state =
        walk_both { state with at_end_of_a_group = false } seq left
      in
      let restr, right, final_state =
        walk_both { mid_state with at_end_of_a_group = state.at_end_of_a_group }
          restl right
      in
      restr, Doc.(left ^^ right), final_state

    | _, Doc.Nest (_, i, doc) ->
      let rest, doc, state' = walk_both (under_nest state) seq doc in
      rest, Doc.nest ~extra_indent:i 0 doc, exit_nest state state'

    | _, Doc.Group (_, flatness, doc) ->
      let rest, doc, state' =
        walk_both { space_needed_before_next = No; at_end_of_a_group = true }
          seq doc
      in
      let return_state =
        { state' with at_end_of_a_group = state.at_end_of_a_group }
      in
      let doc =
        (* Inserting now rather than in the group so as to not break it, but...
           group might start with a space. *)
        if first_is_space doc
        then Doc.group ?flatness doc
        else insert_space_if_required state (Doc.group ?flatness doc)
      in
      attach_before_comments return_state rest doc

    | T.Token _, Doc.Optional _
    | T.Opt_token _, Doc.Token _ ->
      raise (Error (Optional_mismatch first.pos))

    (* [Child_node] doesn't appear in linearized token stream *)
    | T.Child_node, _ -> assert false

let from_tokens tokens doc =
  walk_both init_state tokens doc
  |> append_trailing_comments
