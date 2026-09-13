(* TODO: move back to test/layout/comments_insertion.ml once stable *)

let foo = (* before: same line *)

  (* after: same indent as exp *)

(* also after, despite the confusing indentation! *)
  exp


(* slight variation on the above, to illustrate *)
let foo = (* before: same line *)

(* before: same indent *)

(* this time around this one also attaches before! *)
  exp
