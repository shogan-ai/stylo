open Ocaml_syntax

module T = Tokens
module Doc = Document

module Error = struct
  type t =
    | Output_longer_than_input of Doc.t
    | Missing_token of Lexing.position
    | Optional_mismatch of Lexing.position

  let pp ppf : t -> unit = function
    | Output_longer_than_input doc ->
      Format.fprintf ppf "Output longer than the input.";
      dprintf "remaining doc: << %s >>@."
        (Doc.Print.to_string ~width:80 doc)
    | Missing_token pos ->
      Format.fprintf ppf
        "token at position %d:%d absent from the output."
        pos.pos_lnum (pos.pos_cnum - pos.pos_bol);
    | Optional_mismatch pos ->
      Format.fprintf ppf
        "printer changed optional status of token at position %d:%d."
        pos.pos_lnum (pos.pos_cnum - pos.pos_bol);
end

exception Error of Error.t

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
      | Token _ -> acc, first :: rest
      | Lexer_directive _ ->
        (* Should we consume directives too here? *)
        acc, first :: rest
  in
  aux Doc.(empty, empty)

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
    first_is_flushhint d
  | Empty -> `maybe
  | Cat (_, d1, d2) ->
    match first_is_flushhint d1 with
    | `maybe -> first_is_flushhint d2
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
  | Comment c -> c.attachement = Before && not !(c.explicitely_inserted)
  | _ -> false

let attach_before_comments state tokens doc =
  if state.at_end_of_a_group || state.next_is_pulling_flush_hint then
    (* delay until flush hint or having left the group. *)
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
    | _ -> Doc.empty
  in
  Doc.(brk ^^ doc)

let prepend_comments_to_doc state (floating, attached_after) doc =
  let comments = Doc.Utils.(floating ^?^ attached_after) in
  let doc =
    if first_is_space doc
    then Doc.(comments ^^ doc)
    else Doc.Utils.(comments ^/^ doc)
  in
  insert_space_if_required ~inserting_comment:true state doc

let flush_comments tokens floating_allowed ~before:ws_b ~after:ws_a state =
  let to_prepend, rest = consume_leading_comments tokens in
  let after_floating = Doc.(if floating_allowed then ws_a ^^ ws_a else ws_a) in
  let doc =
    match to_prepend with
    | Empty, after -> Doc.(ws_b ^^ after ^^ ws_a)
    | floating, Empty -> Doc.(ws_b ^^ floating ^^ after_floating)
    | floating, after ->
      Doc.(ws_b ^^ floating ^^ after_floating ^^ after ^^ ws_a)
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
    | T.Token (_, false), Doc.Token { vanishing_cond = None; value = p }
    | T.Token (_, true),  Doc.Token { vanishing_cond = Some _; value = p } ->
      dprintf "assume %a synced at %d:%d with << %a >>@."
        Tokens.pp_elt first
        first.pos.pos_lnum
        (first.pos.pos_cnum - first.pos.pos_bol)
        Document.pp_pseudo p;
      let doc = insert_space_if_required state doc in
      attach_before_comments (saw_leaf state) rest doc

    (* Whitespace: don't consume token *)
    | _, Doc.Empty -> seq, doc, state
    | _, Doc.Whitespace _ -> seq, doc, no_space state

    (* Skip explicitely inserted comments *)
    | T.Comment { explicitely_inserted; _ }, Doc.Comment _
      when !explicitely_inserted ->
      rest, doc, state

    | _, Doc.Comment _ -> seq, doc, state

    | T.Comment { explicitely_inserted; _ }, Doc.Token _
      when !explicitely_inserted ->
      walk_both state rest doc

    (* Comments flushing hint take precedence over attachement and nesting
       considerations. *)
    | T.Comment { explicitely_inserted; _ },
      Doc.Comments_flushing_hint fh
      when not !explicitely_inserted ->
      fh.cmts_were_flushed := true;
      flush_comments seq fh.floating_cmts_allowed ~before:fh.ws_before
        ~after:fh.ws_after state

    | _, Doc.Comments_flushing_hint _ ->
      (* No comments to insert, the hint vanishes. *)
      seq, Doc.empty, state

    (* Comments missing in the doc, insert them *)
    | T.Comment _, Doc.Token _ ->
      insert_comments_before_subtree seq state doc

    | T.Comment { explicitely_inserted; _ }, Doc.Group (_, _, _, d)
      when not !explicitely_inserted &&
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
      rest, Doc.nest ?vanish i doc, exit_nest state state'

    | _, Doc.Group (_, margin, flatness, doc) ->
      traverse_group seq state margin flatness doc

    | T.Token (_, false), Doc.Token { vanishing_cond = Some _; value = p }
    | T.Token (_, true),  Doc.Token { vanishing_cond = None; value = p } ->
      dprintf "OPTIONAL MISMATCH %a with %a@."
        T.pp_elt first
        Doc.pp_pseudo p;
      raise (Error (Optional_mismatch first.pos))


    | (* [Child_node] doesn't appear in linearized token stream *)
      T.Child_node, _
    | (* No directives have been inserted prior to reaching us. *)
      _, Doc.Directive _ -> assert false

and traverse_group tokens state margin flatness grouped_doc =
  let rest, d, state' =
    walk_both
      { state with space_handling = Nothing_special; at_end_of_a_group = true }
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
  let to_prepend, rest = consume_leading_comments tokens in
  let rest, doc, state' = walk_both (no_space state) rest doc in
  let doc = prepend_comments_to_doc state to_prepend doc in
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
          if !(c.explicitely_inserted)
          then doc
          else Doc.Utils.(doc ^?^ Doc.comment c.text)
        in
        aux doc toks
      | Token _ -> raise (Error (Missing_token tok.pos))
      | Child_node -> assert false
  in
  aux doc tokens

type error = [ `Comment_insertion_error of Error.t ]

let from_tokens tokens doc =
  try
    Ok (
      walk_both init_state tokens doc
      |> append_trailing_comments
    )
  with Error e ->
    Result.Error (`Comment_insertion_error e)
