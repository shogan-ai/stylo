open Document

type nest_fun = t -> t

type chunk = {
  pre_doc: t;
  space: t;
  nest: nest_fun;
}

type t = chunk list * nest_fun

let implied_nest = function
  | None -> Fun.id
  | Some (_, nest) -> nest

let mk ?indent doc space =
  let indent =
    match indent with
    | None ->
      Requirement.to_int (requirement doc) +
      if is_empty space then 0 else 1
    | Some i -> i
  in
  let vanish =
    match doc with
    | Cat (_, Token { vanishing_cond; _}, _)
    | Cat (_, _, Token { vanishing_cond; _})
    | Token { vanishing_cond; _ } -> vanishing_cond
    | _ -> None
  in
  let chunk = { pre_doc = doc; space; nest = nest ?vanish indent } in
  [chunk], chunk.nest

let tight ?indent doc = mk doc empty ?indent
let spaced ?indent doc = mk doc (break 1) ?indent

let preceed ~by:t doc =
  let inserted, hint =
    flush_comments ~pull_preceeding_comments:true ~floating_allowed:false
      ~ws_before:t.space ~ws_after:(break 1)
  in
  let space = vanishing_whitespace inserted t.space in
  t.pre_doc ^^ t.nest (hint ^^ space ^^ doc)

let ( + ) fst_opt snd =
  match fst_opt with
  | None -> snd, Fun.id
  | Some (fst_chunks, fst_nest) ->
    let (snd_chunks, snd_nest) = snd in
    let chunks = snd_chunks @ fst_chunks in
    let nest doc = fst_nest (snd_nest doc) in
    (chunks, nest), fst_nest

let group_with t doc =
  match t with
  | None -> doc, Fun.id
  | Some (chunks, nest) ->
    List.fold_left (fun doc chunk ->
      group (preceed doc ~by:chunk)
    ) doc chunks, nest
