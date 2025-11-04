open Core

let foldli f init l =
  snd @@ List.fold_left (fun (i, acc) elt ->
      let acc = f i acc elt in
      (succ i, acc)
  ) (0, init) l

let (^/^) t1 t2 = t1 ^^ break 1 ^^ t2
let (^?^) d1 d2 =
  match d1, d2 with
  | Empty, d
  | d, Empty -> d
  | _ -> d1 ^/^ d2

let (^?/^) d1 d2 =
  match d1, d2 with
  | Empty, d
  | d, Empty -> d
  | _ -> d1 ^^ hardline ^^ d2

let separate_map sep f xs =
  foldli (fun i accu x ->
    if i = 0 then
      f x
    else
      accu ^^ sep ^^ f x
  ) empty xs

let separate sep docs =
  separate_map sep Fun.id docs

let flow_map sep f docs =
  foldli (fun i accu doc ->
    if i = 0 then
      f doc
    else
      accu ^^
      (* This idiom allows beginning a new line if [doc] does not
        fit on the current line. *)
      group (sep ^^ f doc)
  ) empty docs

let flow sep docs =
  flow_map sep (fun x -> x) docs

let prefix ?indent:(n=2) ?(extra_indent=0) ?(spaces=1) left right =
  let indent = n + extra_indent in
  match left, right with
  | Empty, doc
  | doc, Empty -> doc
  | _ ->
    group (left ^^ nest indent (break spaces ^^ right))
