open Core

let optional f v_opt =
  match v_opt with
  | None -> empty
  | Some v -> f v
;;

let foldli f init l =
  snd
  @@ List.fold_left
       (fun (i, acc) elt ->
         let acc = f i acc elt in
         (succ i, acc))
       (0, init)
       l
;;

let ( ^/^ ) t1 t2 = t1 ^^ break 1 ^^ t2

(* Documents which print nothing, but which can't be dropped as they might
   matter for vertical alignment (e.g. empty cells). *)
let rec prints_nothing = function
  | Empty -> true
  | Alignment_cell (_, _, d) -> prints_nothing d
  | Cat (_, d1, d2) -> prints_nothing d1 && prints_nothing d2
  | _ -> false
;;

let ( ^?^ ) d1 d2 =
  if prints_nothing d1 || prints_nothing d2 then d1 ^^ d2 else d1 ^/^ d2
;;

let ( ^?/^ ) d1 d2 =
  if prints_nothing d1 || prints_nothing d2
  then d1 ^^ d2
  else d1 ^^ hardline ^^ d2
;;

let separate_map sep f xs =
  foldli (fun i accu x -> if i = 0 then f x else accu ^^ sep ^^ f x) empty xs
;;

let separate sep docs = separate_map sep Fun.id docs

let flow_map sep f docs =
  foldli
    (fun i accu doc ->
      let doc = f doc in
      if i = 0
      then doc
      else
        match doc with
        | Empty -> accu
        | _ ->
          accu
          ^^ (* This idiom allows beginning a new line if [doc] does not fit on
                the current line. *)
             group
               (sep ^^ doc))
    empty
    docs
;;

let flow sep docs = flow_map sep (fun x -> x) docs

let prefix ?indent:(n = 2) ?(extra_indent = 0) ?(spaces = 1) left right =
  let indent = n + extra_indent in
  match left, right with
  | Empty, doc | doc, Empty -> doc
  | _ -> group (left ^^ nest indent (break spaces ^^ right))
;;
