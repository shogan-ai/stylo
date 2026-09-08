(* `&&` and `||` are each *right*-associative in OCaml (easy to assume
   left-associative by analogy with `+`/`-` - that assumption is wrong).
   `&&` binds tighter than `||`. Comparison operators (`=`,`<`,`>`,`<>`, ...)
   all sit at one precedence level, tighter than `&&`, and are technically
   left-associative even though chaining them is rarely meaningful. *)

let a = true
let b = true
let c = true

(* `&&` binds tighter than `||` *)
let redundant_1 = a || (b && c) (* || already lets && bind first *)
let necessary_1 = (a || b) && c (* without parens: a || b && c = a || (b && c) *)

(* both operators are right-associative, not left *)
let redundant_2 = a && (b && c) (* a && b && c already groups this way *)
let necessary_2 = (a && b) && c (* without parens: a && (b && c), a different grouping *)
let redundant_3 = a || (b || c)
let necessary_3 = (a || b) || c

(* `not` is ordinary prefix application, so it binds as tightly as any
   function call - it does NOT distribute over `&&` *)
let necessary_4 = not (a && b) (* without parens: (not a) && b *)
let redundant_5 = not (a) (* not is just application; the arg's own parens are optional *)

(* comparisons bind tighter than `&&`/`||` *)
let x = 1
let y = 2
let redundant_6 = (x < y) && c (* x < y && c already means this *)
let necessary_5 = x < (y && true) (* without parens: (x < y) && true - and wouldn't typecheck *)

(* comparisons chain left-to-right at their own precedence level; chaining
   them is rarely what you want (`x = y = z` compares `x = y` against `z`).
   In `(x = y) = (y = x)`, only the LEFT pair's parens are redundant
   (left-assoc, so grouping the left pair first is already the default) -
   the right pair's parens remain necessary, for the same reason as
   `necessary_6` below: `x = y = y = x` (fully unparenthesized) is a
   different, flat 4-way chain, not the same as `(x = y) = (y = x)`. *)
let redundant_7 = x = y = (y = x) (* the left pair needs no parens *)
let necessary_6 = x = (y = x) (* without parens: (x = y) = x, a different grouping *)
