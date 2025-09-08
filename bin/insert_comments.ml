open Ocaml_syntax
open Dbg_print

module T = Tokens
module Doc = Wrapprint

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
    Format.eprintf "ERROR: Output longer than the input.@.";
    dprintf "remaining doc: << %a >>@."
      PPrint.ToFormatter.compact (Wrapprint.to_document doc);
    exit 1
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
      Format.eprintf
        "ERROR: desynchronized at position %d:%d@."
        first.pos.pos_lnum (first.pos.pos_cnum - first.pos.pos_bol);
      exit 1

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
      Format.eprintf
        "ERROR: token at position %d:%d absent from the output.@."
        first.pos.pos_lnum (first.pos.pos_cnum - first.pos.pos_bol);
      exit 1

    (*** Stricter sync check part 3 ***)
    | _, Doc.Token_let ->
      Format.eprintf
        "ERROR: desynchronized at position %d:%d@."
        first.pos.pos_lnum (first.pos.pos_cnum - first.pos.pos_bol);
      exit 1

    (* [Child_node] doesn't appear in linearized token stream *)
    | T.Child_node, _ -> assert false

let from_tokens tokens doc =
  match walk_both tokens doc with
  | [], doc -> doc
  | l, _shortened_doc ->
    Format.eprintf "ERROR: Output shorter than the input.@.";
    dprintf "remaining:@ @[<hov 2>%a@]@." T.pp_seq l;
    (* we do not exit, because the diff is more useful to catch such errors *)
    _shortened_doc
