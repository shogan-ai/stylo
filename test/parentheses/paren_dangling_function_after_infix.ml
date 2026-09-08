(* `function` has exactly the same "swallows every following `|` clause"
   hazard as `match ... with` (see paren_dangling_match_cases.ml) - and it
   shows up constantly in real code as `expr >>= function | ... | ...` used
   as one branch of an outer match (monadic-bind style code: Lwt, Async,
   Result, ...). The infix operator (`>>=` here) isn't the cause; `function`
   alone already has the problem. What's specific to this combination is
   that there are now TWO different places parens can go to fix it, and
   both give the exact same result. Verified against `ocamlc -dparsetree`. *)

let g = 0
let ( >>= ) a f = f a

(* the bare hazard, without any infix operator at all *)
let necessary_1 x =
  match x with
  | 0 -> (function
          | 0 -> 1
          | _ -> 2)
  | _ -> 3

(* without parens, `| _ -> 3` is absorbed into the `function`'s cases,
   leaving the outer match with a single case - same failure mode as the
   plain nested-match case, just via `function` instead of `match ... with` *)

(* two equally valid fixes once an infix operator is involved: *)

(* fix A - parenthesize just the "inner" function *)
let necessary_2a x =
  match x with
  | 0 -> g >>= (function
                | 0 -> 1
                | _ -> 2)
  | _ -> 3

(* fix B - parenthesize the "outer" combination (the operator application
   together with its function argument) instead; this closes off the
   function's case list at exactly the same point, since a closing paren is
   just as much "not a `|`" as the outer match's own following `|` would be *)
let necessary_2b x =
  match x with
  | 0 -> (g >>= function
          | 0 -> 1
          | _ -> 2)
  | _ -> 3

(* fix A and fix B parse to the identical tree - pick whichever reads
   better; what matters is that at least one of them is present *)

(* the same hazard, once more with `fun y -> match y with ...` instead of
   `function` directly - the underlying cause is identical: an unparenthesized
   match nested inside a case body *)
let necessary_3 x =
  match x with
  | 0 -> g >>= (fun y ->
      match y with
      | 0 -> 1
      | _ -> 2)
  | _ -> 3
