let f = function
  | A -> 1
  (* one line *)
  | Bbbbbbbb -> 2
  (* two

     lines *)
  | Cc -> 3
  (* three

     lines *)
  | Ddddddddddddd -> 4
  | E -> 5 (* trailing *)
  | Ff -> 6
;;

type t =
  | A (* trailing *)
  (* between constructors *)
  | Bbbbbbbbbb of int
  | (* inside *) Cc of string
