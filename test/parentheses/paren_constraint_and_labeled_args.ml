(* Two cases that are not really about precedence at all: *)

(* A type constraint `(e : t)` or coercion `(e :> t)` is not "redundant
   parens around an expression" - the parens are the mandatory concrete
   syntax of the construct itself. There is no unparenthesized form: bare
   `a : int` is not a valid expression on its own (it only appears
   unparenthesized after `let x : t = ...`, a completely different
   grammar position). Removing these parens is never safe. *)

let a = 1

let necessary_1 = (a : int) (* `a : int` alone does not parse as an expression *)
let necessary_2 = (a :> int) (* likewise for coercion *)

(* A labeled argument `~label:e` parses [e] at a tight precedence that
   excludes infix operators - much like a plain application argument. So
   `~x:a + b` does NOT mean `~x:(a + b)`; it means `(f ~x:a) + b`. *)

let f ~x c = x + c
let b = 2
let c = 3

let necessary_3 = f ~x:(a + b) c (* without parens: (f ~x:a) + (b c), utterly different *)
let redundant_1 = f ~x:(a) c (* the argument's own parens, if any, are always optional -
                                 f ~x:a c already means the same thing *)
