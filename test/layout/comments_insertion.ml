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

(* The next items are aimed at the attachment decision. One big implicit
   invariant is that once a comment starts attaching to the token AFTER it, all
   the comments between it and said token have to attach to it as well (i.e.
   they attach "AFTER").

   Currently this is done by grouping successive comments together and making
   the attachement decision once per group.
   There can however be successive groups as well, a group ends at the first
   blank line or non-comment token.
*)

let foo = (* before: same line *)
  (* after: same indent as exp *)
(* also after, despite the confusing indentation! *)
  exp


(* slight variation on the above, to illustrate *)
let foo = (* before: same line *)
(* before: same indent *)
(* this time around this one also attaches before! *)
  exp

(* The same two items as above but with a blank line between each comment
   currently cause stylo to not be idempotent.
   Saved at test/correctness-checks/failing/before_after_before_comments.ml *)

let foo = (* before: same line *)

  (* after: same indent as exp *)

(* also after, despite the confusing indentation! *)
  exp


(* slight variation on the above, to illustrate *)
let foo = (* before: same line *)

(* before: same indent *)

(* this time around this one also attaches before! *)
  exp
