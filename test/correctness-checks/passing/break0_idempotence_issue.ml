;;
fst
(* C0 *) , snd

;;
{ fst
(* C1 *) ; snd }

;; function
fst
(* C2 *) , snd -> ()

;; function
{ fst
(* C3 *) ; snd } -> ()

;;

open%foo.
(* C4 *) bar List

;;

type t = F(
  (* C5 *) X
).t

type (_
  (* C6 *), _) t

class [_
      (* C7 *), _] c = object end

type id = { f: 'a
(* C8 *) . 'a -> 'a }
