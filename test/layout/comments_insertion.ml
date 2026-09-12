let flat = 1

(* floating *)

let foo =
  if whatever
  then (* some comment *)
    smtg
  else (
    match not_flat with
    | () -> ()
  )

(* floating2 *)

(* a series of floating comments close to each other *)
(* here is the second, not particularly interesting, one *)
(* the last one even has
   {[
     a code block
   ]} *)

(* floating3 *)

let foo2 =
  if whatever2
  then
    (* some comment2 *)
    smtg2
  else (
    match not_flat2 with
    | () -> ()
  )

(** floating comments between match cases perhaps? *)
let foo = function
  (* not floating, but natural placing *)
  | Some pattern
  | Or (Another pattern) -> ignore pattern

  (* now, that one floats, to inform us that the next one's won't float but be
     attached. *)

  (* right? *)
  | Yes right -> right

  (* same as well, except longer because we want to test that too *)
  | I_am talking -> myself

(* ... *)
