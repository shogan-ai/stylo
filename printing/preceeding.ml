open Document

type nonrec t = (t * int lazy_t)

let implied_nest = function
  | None -> Fun.id
  | Some (_, i) -> nest ~extra_indent:i 0

let mk doc ~indent = (doc, lazy indent)

let extend t_opt doc ~indent =
  let indent =
    match doc with
    | Document.Optional { vanishing_cond; _ } ->
      lazy (if Condition.check vanishing_cond then 0 else indent)
    | _ -> lazy indent
  in
  match t_opt with
  | None -> (doc, indent), Fun.id
  | Some (preceeding, previous_indent) ->
    (preceeding ^^ doc, lazy (Lazy.force previous_indent + Lazy.force indent)),
    nest ~extra_indent:previous_indent 0

let group_with t doc =
  match t with
  | None -> doc, Fun.id
  | Some (t, indent) ->
    let nest = nest ~extra_indent:indent 0 in
    group (t ^^ nest doc), nest
