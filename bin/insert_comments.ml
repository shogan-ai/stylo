open Ocaml_syntax
open Dbg_print

module T = Tokens
module Doc = Wrapprint

type error =
  | Output_longer_than_input of Doc.document
  | Output_shorter_than_input of T.seq
  | Desynchronized of Lexing.position
  | Missing_token of Lexing.position

let pp_error ppf = function
  | Output_longer_than_input doc ->
    Format.fprintf ppf "Output longer than the input.";
    dprintf "remaining doc: << %a >>@."
      PPrint.ToFormatter.compact (Wrapprint.to_document doc)
  | Output_shorter_than_input tokens ->
    Format.eprintf "Output shorter than the input.";
    dprintf "remaining:@ @[<hov 2>%a@]@." T.pp_seq tokens
  | Desynchronized pos ->
    Format.fprintf ppf "desynchronized at position %d:%d."
      pos.pos_lnum (pos.pos_cnum - pos.pos_bol);
  | Missing_token pos ->
    Format.fprintf ppf
      "token at position %d:%d absent from the output."
      pos.pos_lnum (pos.pos_cnum - pos.pos_bol);

exception Error of error

let rec consume_leading_comments acc = function
  | [] -> acc, []
  | first :: rest ->
    match first.T.desc with
    | Comment c -> consume_leading_comments Doc.(acc ^?^ comment c) rest
    | Token _ -> acc, rest
    | Child_node -> assert false

let rec consume_only_leading_comments acc = function
  | [] -> acc, []
  | first :: rest ->
    match first.T.desc with
    | Comment c -> consume_only_leading_comments Doc.(acc ^?^ comment c) rest
    | Token _ -> acc, first :: rest
    | Child_node -> assert false

let rec first_is_comment = function
  | Doc.Comment _ -> `yes
  | Token _ | Token_let -> `no
  | Group d | Align d | Nest (_, d) -> first_is_comment d
  | Empty | Whitespace _ -> `maybe
  | Cat (d1, d2) ->
    match first_is_comment d1 with
    | `maybe -> first_is_comment d2
    | res -> res

let first_is_comment d = first_is_comment d = `yes

let rec walk_both seq doc =
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
      rest, doc

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
      T.Comment c, Doc.Comment _
      when Doc.comment c <> doc && Doc.docstring c <> doc ->
      let rest, doc = walk_both rest doc in
      rest, Doc.(comment c ^/^ doc)

    (* Synchronized, advance *)
    | T.Token _, Doc.Token p
    | T.Comment _, Doc.Comment p ->
      dprintf "assume %a synced at %d:%d with << %a >>@."
        Tokens.pp_elt first
        first.pos.pos_lnum
        (first.pos.pos_cnum - first.pos.pos_bol)
        PPrint.ToFormatter.compact p;
      rest, doc

    (* Skip "whitespaces" *)
    (* FIXME: we might want to insert a comment before whitespaces. Currently
       we're always placing comments just before the next token/group, but we
       might want to keep them close to the preceeding one. *)
    | _, Doc.(Empty | Whitespace _) -> seq, doc

    (* Comments missing in the doc, insert them *)
    | T.Comment _, Doc.(Token_let | Token _) ->
      let to_prepend, rest = consume_leading_comments Doc.empty seq in
      rest, Doc.(to_prepend ^/^ doc)

    | T.Comment _, Doc.Group d when not (first_is_comment d) ->
      let to_prepend, rest = consume_only_leading_comments Doc.empty seq in
      let rest, doc = walk_both rest doc in
      rest, Doc.(to_prepend ^/^ doc)

    (* Traverse document structure *)
    | _, Doc.Cat (left, right) ->
      let restl, left = walk_both seq left in
      let restr, right = walk_both restl right in
      restr, Cat (left, right)
    | _, Doc.Nest (i, doc) ->
      let rest, doc = walk_both seq doc in
      rest, Nest (i, doc)
    | _, Doc.Group doc ->
      let rest, doc = walk_both seq doc in
      rest, Group doc
    | _, Doc.Align doc ->
      let rest, doc = walk_both seq doc in
      rest, Align doc

    (* Token missing from the document: this is a hard error. *)
    | T.Token _, Doc.Comment _ ->
      raise (Error (Missing_token first.pos))

    (*** Stricter sync check part 3 ***)
    | _, Doc.Token_let ->
      raise (Error (Desynchronized first.pos))

    (* [Child_node] doesn't appear in linearized token stream *)
    | T.Child_node, _ -> assert false

let from_tokens tokens doc =
  match walk_both tokens doc with
  | [], doc
  | [ { desc = Token EOF; _ } ], doc -> doc
  | tokens, _ -> raise (Error (Output_shorter_than_input tokens))
