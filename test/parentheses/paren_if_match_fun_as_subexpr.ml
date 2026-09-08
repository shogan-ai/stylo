(* `if`, `match`, `fun`, `try`, and `let ... in` are all "extend as far right
   as possible" constructs. Where parens are needed around them depends on
   *where* they sit, and the rule has three distinct cases - not two:

   - as the function-application ARGUMENT to another expression: parens are
     always mandatory (a hard parse error without them), because
     application arguments are parsed at a tight "simple expression" level
     that excludes all of these constructs outright.
   - as the RIGHT-hand operand of an infix operator: parens are redundant,
     because infix operators parse their right operand at full expression
     level, which does include these constructs.
   - as the LEFT-hand operand of an infix operator (or the receiver of an
     application, `(fun x -> x) a`): parens are mandatory, because parsing
     proceeds left-to-right and would otherwise swallow the operator into
     the construct's own body/branches.

   Verified against `ocamlc -dparsetree`. *)

let b = true
let x = 1
let y = 2

(* as an application argument: always mandatory *)
let necessary_1 = ignore (if b then x else y) (* `ignore if b then x else y` doesn't parse *)
let necessary_2 = ignore (match x with 0 -> x | _ -> y) (* likewise *)
let necessary_3 = List.map (fun n -> n + 1) [ x; y ] (* likewise *)

(* as the right operand of an infix operator: redundant *)
let redundant_1 = 1 + (if b then x else y) (* `1 + if b then x else y` already means this *)
let redundant_2 = 1 + (match x with 0 -> x | _ -> y)
let redundant_3 = ignore (fun () -> 1) (* app-argument case again, kept for contrast *)

(* as the left operand of an infix operator: mandatory *)
let necessary_4 = (if b then x else y) + 1 (* without parens: if b then x else (y + 1) *)
let necessary_5 = (fun n -> n) x (* without parens: fun n -> (n x), a different function *)
let necessary_6 = (match x with 0 -> x | _ -> y) + 1 (* without parens: the [+ 1] attaches
                                                          only to the [_ -> y] branch *)
