open Ocaml_syntax

module T = Tokens
module Doc = Wrapprint

let rec consume_leading_comments acc = function
  | T.Comment c :: rest -> consume_leading_comments Doc.(acc ^/^ comment c) rest
  | T.Token _ :: rest -> acc, rest
  | T.Child_node :: _ -> assert false
  | [] -> acc, []

let rec walk_both seq doc =
  match seq, doc with
  (* Synchronized, advance *)
  | T.Token _ :: rest, Doc.Token _
  | T.Comment _ :: rest, Doc.Comment _ -> rest, doc

  (* Skip "whitespaces" *)
  | _, Doc.(Empty | Whitespace _) -> seq, doc

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

  (* Comments missing in the doc, insert them *)
  | T.Comment _ :: _, Doc.Token _ ->
    let to_prepend, rest = consume_leading_comments Doc.empty seq in
    rest, Doc.(to_prepend ^/^ doc)

  (* Token missing from the document: this is a hard error. *)
  | T.Token _ :: _, Doc.Comment _ ->
    Format.eprintf "ERROR: A token disappeared from the input.@.";
    exit 1

  (* Some extra tokens or comments were synthesized *)
  | [], _ ->
    Format.eprintf "ERROR: Output longer than the input.@.";
    exit 1

  (* [Child_node] doesn't appear in linearized token stream *)
  | T.Child_node :: _, _ -> assert false

let from_tokens tokens doc =
  match walk_both tokens doc with
  | [], doc -> doc
  | _ ->
    Format.eprintf "ERROR: Output shorter than the input.@.";
    exit 1
