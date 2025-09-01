open Ocaml_syntax
open Wrapprint

let rec advance_stream_and_insert_comments seq doc =
  match seq with
  | [] -> seq, doc
  | elt :: seq ->
    match elt with
    | Tokens.Child_node -> assert false
    | Token _ -> seq, doc
    | Comment c ->
      let c = "(*" ^ c ^ "*)" in
      advance_stream_and_insert_comments seq (doc ^/^ string c)

let rec insert_comments_after seq doc =
  match doc with
  (* Return all the leaves as is *)
  | Token _ -> advance_stream_and_insert_comments seq doc
  | Empty
  | Whitespace _ ->
    seq, doc
  | Cat (t1, t2) ->
    let stream', t1' = insert_comments_after seq t1 in
    let stream', t2' = insert_comments_after stream' t2 in
    stream',
    if t1 == t1' && t2 == t2'
    then doc
    else Cat (t1', t2')
  | Nest (i, t) ->
    let stream', t' = insert_comments_after seq t in
    stream',
    if t == t'
    then doc
    else Nest (i, t')
  | Group t ->
    let stream', t' = insert_comments_after seq t in
    stream',
    if t == t'
    then doc
    else Group t'
  | Align t ->
    let stream', t' = insert_comments_after seq t in
    stream',
    if t == t'
    then doc
    else Align t'

let rec from_tokens seq doc =
  match seq with
  | [] -> doc
  | elt :: seq ->
    match elt with
    | Tokens.Child_node -> assert false
    | Token _ ->
      let remaining, doc = insert_comments_after seq doc in
      assert (remaining = []);
      doc
    | Comment c ->
      let c = "(*" ^ c ^ "*)" in
      Wrapprint.(string c ^/^ from_tokens seq doc)
