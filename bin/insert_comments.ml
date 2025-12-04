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

let consume_leading_comments =
  let rec aux (floating, attached_after as acc) = function
    | [] -> acc, []
    | first :: rest ->
      match first.T.desc with
      | Child_node -> assert false
      | Comment c when not !(c.explicitely_inserted) ->
        let acc =
          let cmt = Doc.comment c.text in
          match c.attachement with
          (* It looks like we might reorder comment if some [Floating] comments
             follow some [After] ones. But that cannot happen, by construction.
          *)
          | After -> floating, Doc.Utils.(attached_after ^?^ cmt)
          | Floating -> Doc.Utils.(floating ^?^ cmt), attached_after
          (* Ideally we'd [assert false] here: [Before] comments have
             necessarily been consumed already.

             ... however, comments at the beginning of the file (before anycode)
             currently get marked as [Before] by the lexer.
             This is should be fixed eventually (because some might actually be
             [After]!) but for now we treat them as floating here. *)
          | Before -> Doc.Utils.(floating ^?^ cmt), attached_after
        in
        aux acc rest
      | Comment _
      | Token _
      | Opt_token _ -> acc, first :: rest
  in
  aux Doc.(empty, empty)

let rec first_is_space = function
  | Doc.Whitespace _ -> `yes
  | Token _ | Comment _ | Optional _ -> `no
  | Group (_, _, _, d) | Nest (_, _, _, d) ->
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
  | Group (_, _, _, d) -> nest_before_leaf d
  | Empty -> `maybe
  | Cat (_, d1, d2) ->
    match nest_before_leaf d1 with
    | `maybe -> nest_before_leaf d2
    | res -> res

let nest_before_leaf d = nest_before_leaf d = `yes

type special_space_treatement =
   | Nothing_special
   | Remove_if_first_leaf
   | Insert_before_leaf
   | Insert_before_inserting_comment
   | Wrap_comment_with of Document.t

type state = {
  space_handling: special_space_treatement;
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
  { space_handling = Nothing_special
  ; at_end_of_a_group = false }

let under_nest st = { st with at_end_of_a_group = false }
let exit_nest prev st = { st with at_end_of_a_group = prev.at_end_of_a_group }

let no_space st = { st with space_handling = Nothing_special }
let saw_leaf st =
  { st with space_handling = Insert_before_inserting_comment }

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
      tokens, doc, { state with space_handling = Insert_before_leaf }

let insert_space_if_required ?(inserting_comment=false) state doc =
  let brk =
    match state.space_handling, inserting_comment with
    | Insert_before_leaf, _
    | Insert_before_inserting_comment, true -> Doc.break 1
    | Wrap_comment_with brk, true -> brk
    | _ -> Doc.empty
  in
  Doc.(brk ^^ doc)

let prepend_comments_to_doc state comments doc =
  let brk =
    match state.space_handling with
    | Wrap_comment_with ws -> ws
    | _ when first_is_space doc -> Doc.empty
    | _ -> Doc.break 1
  in
  let comments =
    match comments with
    | Doc.Empty, comments
    | comments, Doc.Empty -> comments
    | floating, attached_after ->
      (* FIXME: we want to duplicate brk when [Wrap_comment_with] *)
      Doc.(floating ^^ brk ^^ attached_after)
  in
  let doc = Doc.(comments ^^ brk ^^ doc) in
  insert_space_if_required ~inserting_comment:true state doc

let process_whitespace state duplicable ws =
  if state.space_handling = Remove_if_first_leaf then
    Doc.empty, no_space state
  else if duplicable then
    ws, { state with space_handling = Wrap_comment_with ws }
  else
    ws, no_space state

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
      dprintf "assume %a synced at %d:%d with << %a >>@."
        Tokens.pp_elt first
        first.pos.pos_lnum
        (first.pos.pos_cnum - first.pos.pos_bol)
        Document.pp_pseudo p;
      let doc = insert_space_if_required state doc in
      attach_before_comments (saw_leaf state) rest doc

    (* Whitespace: don't consume token *)
    | _, Doc.Empty -> seq, doc, state
    | _, Doc.Whitespace (_, duplicable, _) ->
      let doc, state' = process_whitespace state duplicable doc in
      seq, doc, state'

    (* Skip explicitely inserted comment *)
    | _, Doc.Comment _ -> seq, doc, state
    | T.Comment { explicitely_inserted; _ }, _ when !explicitely_inserted ->
      walk_both state rest doc

    (* Comments missing in the doc, insert them *)
    | T.Comment _, Doc.(Token _ | Optional _) ->
      insert_comments_before_token seq state doc

    | T.Comment _, Doc.Group (_, margin, flatness, d)
      when not (nest_before_leaf d) ->
      (* we can insert comments outside the group as they'll be at the same
         nesting level as the next word. *)
      insert_comments_before_group seq state margin flatness d

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

    | _, Doc.Nest (_, i, vanish, doc) ->
      let rest, doc, state' = walk_both (under_nest state) seq doc in
      rest, Doc.nest ?vanish i doc, exit_nest state state'

    | _, Doc.Group (_, margin, flatness, doc) ->
      traverse_group seq state margin flatness doc

    | T.Token _, Doc.Optional { token = p; _ }
    | T.Opt_token _, Doc.Token p ->
      dprintf "OPTIONAL MISMATCH %a with %a@."
        T.pp_elt first
        Doc.pp_pseudo p;
      raise (Error (Optional_mismatch first.pos))

    (* [Child_node] doesn't appear in linearized token stream *)
    | T.Child_node, _ -> assert false

and traverse_group tokens state margin flatness grouped_doc =
  let rest, d, state' =
    walk_both { space_handling = Nothing_special; at_end_of_a_group = true }
      tokens grouped_doc
  in
  let return_state =
    { state' with at_end_of_a_group = state.at_end_of_a_group }
  in
  let doc =
    (* Inserting now rather than in the group so as to not break it, but...
       group might start with a space. *)
    if first_is_space d
    then Doc.group ~margin ?flatness d
    else insert_space_if_required state (Doc.group ~margin ?flatness d)
  in
  attach_before_comments return_state rest doc

and insert_comments_before_group tokens state margin flatness grouped_doc =
  let to_prepend, rest = consume_leading_comments tokens in
  let rest, d, state' =
    let space_handling =
      (* we behave differently from [traverse_group] here: if [space_handling]
         when reaching the group specifies we need to wrap comment it also
         implies that we need to remove the whitespace already in the doc that
         would have followed the comment.

         This is very ad-hoc and makes me sad. *)
      match state.space_handling with
      | Wrap_comment_with _ -> Remove_if_first_leaf
      | _ -> Nothing_special
    in
    walk_both { space_handling; at_end_of_a_group = true } rest grouped_doc
  in
  let doc = Doc.group ~margin ?flatness d in
  let doc = prepend_comments_to_doc state to_prepend doc in
  attach_before_comments state' rest doc

and insert_comments_before_token tokens state leaf_doc =
  let to_prepend, rest = consume_leading_comments tokens in
  let rest, leaf_doc, state' = walk_both (no_space state) rest leaf_doc in
  let doc = prepend_comments_to_doc state to_prepend leaf_doc in
  rest, doc, state'

let from_tokens tokens doc =
  walk_both init_state tokens doc
  |> append_trailing_comments
