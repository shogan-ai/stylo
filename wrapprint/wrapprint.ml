(* Primitives *)

type t =
  | Empty
  | Token of PPrint.document
  | Comment of PPrint.document
    (** docstrings and explicitely inserted comments *)
  | Whitespace of PPrint.document
  | Cat of t * t
  | Nest of int * t
  | Group of t
  | Align of t

type document = t

let empty = Empty
let char c = Token (PPrint.char c)
let string s = Token (PPrint.string s)
let blank n = Whitespace (PPrint.blank n)
let hardline = Whitespace PPrint.hardline
let break n = Whitespace (PPrint.break  n)
let (^^) t1 t2 = Cat (t1, t2)
let nest i t = Nest (i, t)
let group t = Group t
let align t = Align t

let comment s =
  Comment PPrint.(string "(*" ^^ string s ^^ string "*)")

(* Useful combinators *)

let foldli f init l =
  snd @@ List.fold_left (fun (i, acc) elt ->
      let acc = f i acc elt in
      (succ i, acc)
  ) (0, init) l

let (^/^) t1 t2 = t1 ^^ break 1 ^^ t2
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

let prefix n b left right =
  group (left ^^ nest n (break b ^^ right))

(* To document. *)

open PPrint

let rec to_document : t -> document = function
  | Empty -> PPrint.empty
  | Comment t | Token t | Whitespace t -> t
  | Cat (t1, t2) -> to_document t1 ^^ to_document t2
  | Nest (i, t) -> nest i (to_document t)
  | Group t -> group (to_document t)
  | Align t -> align (to_document t)
