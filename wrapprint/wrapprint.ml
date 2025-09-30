(* Primitives *)

type t =
  | Empty
  | Token_let
  | Token of PPrint.document
  | Comment of PPrint.document
    (** docstrings and explicitely inserted comments *)
  | Whitespace of PPrint.document
  | Cat of t * t
  | Nest of int * t
  | Relative_nest of int * t
  | Group of t
  | Align of t

type document = t

let rec requirement = function
  | Empty -> 0
  | Token_let -> 3
  | Token d
  | Comment d
  | Whitespace d -> PPrint.requirement d
  | Cat (t1, t2) -> requirement t1 + requirement t2
  | Nest (_, t)
  | Relative_nest (_, t)
  | Group t
  | Align t -> requirement t


let empty = Empty
let char c = Token (PPrint.char c)
let string s = Token (PPrint.string s)
let blank n = Whitespace (PPrint.blank n)
let hardline = Whitespace PPrint.hardline
let break n = Whitespace (PPrint.break  n)

let (^^) t1 t2 =
  match t1, t2 with
  | Empty, t
  | t, Empty -> t
  | _ -> Cat (t1, t2)

let nest i t = if t = Empty then t else Nest (i, t)
let relative_nest i t = if t = Empty then t else Relative_nest (i, t)
let group t = if t = Empty then t else Group t
let align t = if t = Empty then t else Align t

let comment s =
  Comment PPrint.(string "(*" ^^ string s ^^ string "*)")

let docstring s =
  Comment PPrint.(string "(**" ^^ string s ^^ string "*)")

(* Useful combinators *)

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

let prefix_gen n b left right =
  group (left ^^ nest n (break b ^^ right))

let prefix = prefix_gen 2 1

let prefix_nonempty l r =
  match l, r with
  | Empty, d
  | d, Empty -> d
  | _ -> prefix l r

(* To document. *)

open PPrint

let relative_nest_custom i d = object
  method requirement = requirement d

  method pretty out state indent flat =
    (* we don't want to decrease the indent, we just don't want to increase it
       too much. *)
    let indent = max (state.last_indent + i) indent in
    pretty out state indent flat d

  method compact out = compact out d
end

let rec to_document : t -> document = function
  | Empty -> PPrint.empty
  | Token_let -> PPrint.string "let"
  | Comment t | Token t | Whitespace t -> t
  | Cat (t1, t2) -> to_document t1 ^^ to_document t2
  | Nest (i, t) -> nest i (to_document t)
  | Relative_nest (i, t) -> custom (relative_nest_custom i (to_document t))
  | Group t -> group (to_document t)
  | Align t -> align (to_document t)
