open Document

type nest_fun = t -> t
type nonrec t = t * t * nest_fun

let implied_nest = function
  | None -> Fun.id
  | Some (_, _, f) -> f
;;

let mk ?(space = empty) doc ~indent =
  let vanish =
    match doc with
    | Cat (_, Token { vanishing_cond; _ }, _)
    | Cat (_, _, Token { vanishing_cond; _ })
    | Token { vanishing_cond; _ } -> vanishing_cond
    | _ -> None
  in
  doc, space, nest ?vanish indent
;;

let join preceeding preceeding_space nest doc =
  let inserted, hint =
    flush_comments ~ws_before:preceeding_space ~ws_after:(break 1) ()
  in
  let space =
    vanishing_whitespace ~assume_present:true inserted preceeding_space
  in
  preceeding ^^ nest (hint ^^ space ^^ doc)
;;

let extend t_opt ?(space = empty) doc ~indent =
  let ((_, space, nest) as suf) = mk ~space doc ~indent in
  match t_opt with
  | None -> suf, Fun.id
  | Some (preceeding, preceeding_space, previous_nest) ->
    ( ( join preceeding preceeding_space previous_nest doc
      , space
      , Fun.compose nest previous_nest )
    , previous_nest )
;;

let group_with t doc =
  match t with
  | None -> doc, Fun.id
  | Some (t, space, nest) -> group (join t space nest doc), nest
;;
