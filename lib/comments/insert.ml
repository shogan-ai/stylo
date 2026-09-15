open Ocaml_syntax

module T = Tokens
module Doc = Document

(* [softline] disappears after a blank line. Emitting two of them forces the
   presence of a blank line, but ensure that even if it follows some
   breaks/hardline we won't increase the number of blank lines.

   N.B. that second bit is only valid when [blank_line] follows another
   whitespace, but not when it preceeds one. *)
let blank_line = Doc.(softline ^^ softline)

let fmt_comment txt =
  Print.Doc.as_odoc_markup_if_no_warnings ~id:(-1) ~kind:`Regular_comment txt

module Error = struct
  type t =
    | Output_longer_than_input of Doc.t
    | Missing_token of Lexing.position

  let pp ppf : t -> unit = function
    | Output_longer_than_input doc ->
      Format.fprintf ppf "Output longer than the input.";
      dprintf "remaining doc: << %s >>@."
        (Doc.Print.to_string ~width:80 doc)
    | Missing_token pos ->
      Format.fprintf ppf
        "token at position %d:%d absent from the output."
        pos.pos_lnum (pos.pos_cnum - pos.pos_bol);
end

exception Error of Error.t

type corresponding_doc =
  | Absent
  | Present_not_seen_yet
  | Already_seen

let already_seen : (int, unit) Hashtbl.t = Hashtbl.create 42

let mark_as_seen id = Hashtbl.replace already_seen id ()

let corresponding_doc_state cmt =
  let id = !(cmt.T.corresponding_document_id) in
  if id < 0
  then Absent
  else if Hashtbl.mem already_seen id
  then Already_seen
  else Present_not_seen_yet

let explicitely_inserted cmt = !(cmt.T.corresponding_document_id) >= 0

let consume_leading_comments =
  let rec aux last_blank_after acc = function
    | [] -> (acc, last_blank_after), []
    | first :: rest ->
      match first.T.desc with
      | Child_node -> assert false
      | Comment c when not (explicitely_inserted c) ->
        let cmt = fmt_comment ~start_pos:first.pos c.text in
        let sep =
          if c.blank_line_before || last_blank_after
          then blank_line
          else if Doc.is_empty acc
          then Doc.empty
          else Doc.break 1
        in
        aux c.blank_line_after Doc.(acc ^^ sep ^^ cmt) rest
      | Comment _
      | Token _
      | Lexer_directive _ ->
        (* Should we consume directives too here? *)
        (acc, last_blank_after), first :: rest
  in
    aux false Doc.empty

let rec first_is_space = function
  | Doc.Whitespace _ -> `yes
  | Token _ | Comment _ | Directive _ -> `no
  | Group (_, _, _, d) | Nest (_, _, _, d) ->
    first_is_space d
  | Empty
  | Comments_flushing_hint _ -> `maybe
  | Cat (_, d1, d2) ->
    match first_is_space d1 with
    | `maybe -> first_is_space d2
    | res -> res

let first_is_space d = first_is_space d = `yes

let rec first_is_flushhint ?pulls_before = function
  | Doc.Comments_flushing_hint fh ->
    begin match pulls_before with
    | Some value when value <> fh.pull_cmts_attached_before_hint -> `no
    | _ -> `yes
    end
  | Token _ | Comment _ | Whitespace _ | Directive _ -> `no
  | Group (_, _, _, d) | Nest (_, _, _, d) ->
    first_is_flushhint ?pulls_before d
  | Empty -> `maybe
  | Cat (_, d1, d2) ->
    match first_is_flushhint ?pulls_before d1 with
    | `maybe -> first_is_flushhint ?pulls_before d2
    | res -> res

let first_is_flushhint ?pulls_before d =
  first_is_flushhint ?pulls_before d = `yes

let rec nest_before_leaf = function
  | Doc.Nest _ -> `yes
  | Token _ | Comment _ | Directive _ -> `no
  | Group (_, _, _, d) -> nest_before_leaf d
  | Empty
  | Whitespace _
  | Comments_flushing_hint _ -> `maybe
  | Cat (_, d1, d2) ->
    match nest_before_leaf d1 with
    | `maybe -> nest_before_leaf d2
    | res -> res

let nest_before_leaf d = nest_before_leaf d = `yes

type special_space_treatement =
  | Nothing_special
  | Insert_before_leaf
  | Insert_blank_line_before_leaf
  | Next_before_leaf_is_blank_line
  | Insert_before_inserting_comment

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

  next_is_pulling_flush_hint: bool;
  (** [true] if the the next leaf is a [Comments_flushing_hint fh] which pulls
      comments that would otherwise be attached to the token preceeding it. *)
}

let init_state =
  { space_handling = Nothing_special
  ; at_end_of_a_group = false
  ; next_is_pulling_flush_hint = false }

let under_nest st = { st with at_end_of_a_group = false }
let exit_nest prev st = { st with at_end_of_a_group = prev.at_end_of_a_group }

let no_space st = { st with space_handling = Nothing_special }
let saw_leaf st =
  { st with space_handling = Insert_before_inserting_comment }

let format_directive (ldir : Lexer_directive.t) =
  let open Doc in
  directive @@
  match ldir with
  | Hash_syntax (mode, toggle) ->
    Utils.separate_map nbsp string
      ["#syntax"; mode; if toggle then "on" else "off"]
  | Line_directive (path, line_num) ->
    string "#" ^^ string (string_of_int line_num) ^^ nbsp ^^
    string (Printf.sprintf "%S" path)


let insert_directive doc ldir =
  dprintf "reinserting lexer directive@.";
  Doc.(format_directive ldir ^^ doc)

let is_comment_attaching_before elt =
  match elt.T.desc with
  | Comment c ->
    if explicitely_inserted c
    then corresponding_doc_state c = Already_seen
    else c.attachement = Before
  | _ -> false

let attach_before_comments state tokens doc =
  if state.at_end_of_a_group || state.next_is_pulling_flush_hint then
    (* delay until flush hint or having left the group. *)
    tokens, doc, state
  else
    match Std.List.split_at is_comment_attaching_before tokens with
    | [], _ ->
      (* no comment to attach *)
      tokens, doc, state
    | to_append, tokens ->
      let doc, actually_inserted, last_blank_after =
        List.fold_left (fun (acc, actually_inserted, last_blank_after) cmt ->
          match cmt.T.desc with
          | Comment c ->
            if explicitely_inserted c
            then acc, actually_inserted, last_blank_after
            else
              (* A blank line between this comment and the previous one must
                 be reproduced, whether the source marks it before this
                 comment or after the previous one. *)
              let sep =
                if c.blank_line_before || last_blank_after
                then blank_line
                else Doc.break 1
              in
              let cmt =
                Doc.(group (sep ^^ fmt_comment ~start_pos:cmt.pos c.text))
              in
              Doc.(acc ^^ cmt), true, c.blank_line_after
          | _ -> assert false
        ) (doc, false, false) to_append
      in
      if not actually_inserted then
        tokens, doc, state
      else
        (* If the last comment was followed by a blank line, reproduce it before
           the token the comments are attached to. *)
        let space_handling =
          if last_blank_after
          then Insert_blank_line_before_leaf
          else Insert_before_leaf
        in
        tokens, Doc.group doc, { state with space_handling }

let insert_space_if_required ?(inserting_comment=false) state doc =
  let brk =
    match state.space_handling, inserting_comment with
    | Insert_blank_line_before_leaf, _ -> blank_line
    | Insert_before_leaf, _
    | Insert_before_inserting_comment, true -> Doc.break 1
    | Insert_before_inserting_comment, false
    | Next_before_leaf_is_blank_line, _
    | Nothing_special, _ -> Doc.empty
  in
  Doc.(brk ^^ doc)

let prepend_comments_to_doc state comments ~blank_line_after doc =
  let doc =
    if first_is_space doc
    then Doc.(comments ^^ doc)
    else if blank_line_after
    then Doc.(comments ^^ blank_line ^^ doc)
    else Doc.Utils.(comments ^/^ doc)
  in
  insert_space_if_required ~inserting_comment:true state doc

let flush_comments tokens ~before:ws_b ~after:ws_a state =
  let (to_prepend, last_blank_after), rest = consume_leading_comments tokens in
  (* A comment followed by a blank line in the source gets one in the output
     too, instead of the hint's own whitespace. *)
  let ws_after_comments =
    if last_blank_after then blank_line else ws_a
  in
  let doc =
    if Doc.is_empty to_prepend
    then Doc.(ws_b ^^ ws_a)
    else Doc.(ws_b ^^ to_prepend ^^ ws_after_comments)
  in
  rest, doc, { state with space_handling = Nothing_special }

(** Traverse the document and sequence of tokens simultaneously, extending the
    document's structure with a subdocument for any comment that might be
    missing.

    We take some care to make these insertions fit in a nice way in the whole
    document:
    - we add some spacing so they aren't just spliced before/after the token
    they attach to, while being careful to not add spaces where there are
    already some
    - we try to insert the comments outside of [Doc.Group]s, as this often
    negatively impacts the layout of the code.

    However, we also try to respect comment's "attachement" (cf.
    [Tokens.attachement]), which roughly means having them on the same line or
    at the same indentation level as the token they attach to.
    This sometimes implies that we will insert comments inside a group, so we
    can reach the correct indentation/nesting level.
    Refer to the lexer the actual rules regarding attachement.
*)
let rec walk_both state seq doc =
  match seq with
  | [] ->
    (* Some extra tokens or comments were synthesized *)
    raise (Error (Output_longer_than_input doc))

  | first :: rest ->
    match first.T.desc, doc with
    (* Synchronized, advance *)
    | T.Token _, Doc.Token { value = p; _ } ->
      dprintf "assume %a synced at %d:%d with << %a >>@."
        Tokens.pp_elt first
        first.pos.pos_lnum
        (first.pos.pos_cnum - first.pos.pos_bol)
        Document.pp_pseudo p;
      let doc = insert_space_if_required state doc in
      attach_before_comments (saw_leaf state) rest doc

    (* Whitespace: don't consume token *)
    | _, Doc.Empty -> seq, doc, state
    | _, Doc.Whitespace _ ->
      begin match state.space_handling with
      | Insert_blank_line_before_leaf
      | Next_before_leaf_is_blank_line ->
        if not state.at_end_of_a_group then
          (* Now is a good opportunity to materialise the blank line. *)
          seq, Doc.(doc ^^ blank_line), no_space state
        else
          (* But if we're at the end of a group, we delay further *)
          seq, doc, state
      | _ -> seq, doc, no_space state
      end

    (* Skip explicitely inserted comments *)
    | _, Doc.Comment d ->
      mark_as_seen d.source_comment_id;
      seq, doc, state

    | T.Comment c, Doc.Token _ when explicitely_inserted c ->
      walk_both state rest doc

    (* Comments flushing hint take precedence over attachement and nesting
       considerations. *)
    | T.Comment c,
      Doc.Comments_flushing_hint fh ->
      begin match corresponding_doc_state c with
      | Already_seen ->
        (* skip the first comment and loop back, there might be others
           following it that can be flushed. *)
        walk_both state rest doc
      | Present_not_seen_yet ->
        (* [c] is already present in the document, but we haven't encountered it
           yet.
           We do not want to flush any other comment before seeing it as that
           would lead to reordering. *)
        fh.cmts_were_flushed := false;
        seq, Doc.empty, state
      | Absent ->
        (* [c] (and perhaps the following comments) can be flushed. *)
        fh.cmts_were_flushed := true;
        flush_comments seq ~before:fh.ws_before ~after:fh.ws_after state
      end

    | _, Doc.Comments_flushing_hint fh ->
      (* No comments to insert, the hint vanishes. *)
      fh.cmts_were_flushed := false;
      seq, Doc.empty, state

    (* Comments missing in the doc, insert them *)
    | T.Comment _, Doc.Token _ ->
      insert_comments_before_subtree seq state doc

    | T.Comment c, Doc.Group (_, _, _, d)
      when not (explicitely_inserted c) &&
           not (nest_before_leaf d) &&
           not (first_is_flushhint d) ->
      (* we can insert comments outside the group as they'll be at the same
         nesting level as the next word and there's no hint that comments should
         be inside the group. *)
      insert_comments_before_subtree seq state doc

    (* Lexer directives are to be inserted in a similar way to comments, except:
       - they are not attached to anything (so we don't care about nesting,
         grouping, etc)
       - they cannot have been explicitely inserted already *)
    | T.Lexer_directive ldir, _ ->
      let rest, doc, state = walk_both (no_space state) rest doc in
      rest, insert_directive doc ldir, state

    (* Traverse document structure *)
    | _, Doc.Cat (_, left, right) ->
      let next_is_pulling_flush_hint =
        first_is_flushhint ~pulls_before:true right
      in
      let restl, left, mid_state =
        walk_both
          { state with at_end_of_a_group = false; next_is_pulling_flush_hint }
          seq left
      in
      let restr, right, final_state =
        walk_both { mid_state with at_end_of_a_group = state.at_end_of_a_group }
          restl right
      in
      restr, Doc.(left ^^ right), final_state

    | _, Doc.Nest (_, i, vanish, doc) ->
      let rest, doc, state' = walk_both (under_nest state) seq doc in
      rest, Doc.nest ~vanish i doc, exit_nest state state'

    | _, Doc.Group (_, margin, flatness, doc) ->
      traverse_group seq state margin flatness doc

    | (* [Child_node] doesn't appear in linearized token stream *)
      T.Child_node, _
    | (* No directives have been inserted prior to reaching us. *)
      _, Doc.Directive _ -> assert false

and traverse_group tokens state margin flatness grouped_doc =
  let rest, d, state' =
    walk_both
      { state with
        space_handling =
          (* Do not force the insertion of space inside the group, we'd rather
             insert it ourself outside (see below).
             However if one is inserted, and a blank line is needed, we make
             sure that the place where the insertion happens knows that
             requirement. *)
          (match state.space_handling with
           | Insert_blank_line_before_leaf | Next_before_leaf_is_blank_line ->
             Next_before_leaf_is_blank_line
           | _ -> Nothing_special);
        at_end_of_a_group = true }
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

and insert_comments_before_subtree tokens state doc =
  let (to_prepend, last_blank_after), rest = consume_leading_comments tokens in
  let rest, doc, state' = walk_both (no_space state) rest doc in
  let doc =
    prepend_comments_to_doc state to_prepend ~blank_line_after:last_blank_after doc
  in
  attach_before_comments state' rest doc

let append_trailing_comments (tokens, doc, _) =
  let rec aux doc = function
    | []
    | [ T.{ desc = Token (EOF, _); _ } ] -> doc
    | tok :: toks ->
      match tok.T.desc with
      | Lexer_directive d ->
        aux Doc.Utils.(doc ^?^ format_directive d) toks
      | Comment c ->
        let doc =
          if explicitely_inserted c
          then doc
          else
            let cmt = fmt_comment ~start_pos:tok.pos c.text in
            let sep =
              if c.blank_line_before then blank_line else Doc.break 1
            in
            Doc.(if is_empty doc then cmt else doc ^^ sep ^^ cmt)
        in
        aux doc toks
      | Token _ -> raise (Error (Missing_token tok.pos))
      | Child_node -> assert false
  in
  aux doc tokens

type error = [ `Comment_insertion_error of Error.t ]

let from_tokens tokens doc =
  Hashtbl.clear already_seen;
  try
    Ok (
      walk_both init_state tokens doc
      |> append_trailing_comments
    )
  with Error e ->
    Result.Error (`Comment_insertion_error e)
