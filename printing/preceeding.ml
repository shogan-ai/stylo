open Document

type nest_fun = t -> t
type nonrec t = t * nest_fun

let implied_nest = function
  | None -> Fun.id
  | Some (_, f) -> f
;;

let mk doc ~indent =
  let vanish =
    match doc with
    | Cat (_, Token { vanishing_cond; _ }, _)
    | Cat (_, _, Token { vanishing_cond; _ })
    | Token { vanishing_cond; _ } -> vanishing_cond
    | _ -> None
  in
  doc, nest ?vanish indent
;;

let extend t_opt doc ~indent =
  let ((_, nest) as suf) = mk doc ~indent in
  match t_opt with
  | None -> suf, Fun.id
  | Some (preceeding, previous_nest) ->
    ( (preceeding ^^ previous_nest doc, Fun.compose nest previous_nest)
    , previous_nest )
;;

let group_with t doc =
  match t with
  | None -> doc, Fun.id
  | Some (t, nest) -> group (t ^^ nest doc), nest
;;
