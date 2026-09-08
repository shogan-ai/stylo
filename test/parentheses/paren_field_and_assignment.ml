(* Field/array access (`.`, `.()`, `.[]`) binds tighter than *everything*
   else, including function application - so `a b.x` means `a (b.x)`, not
   `(a b).x`. Assignment (`:=`, `<-`) is right-associative and binds looser
   than almost everything, similar to `if`/`;` - it naturally extends over
   the whole right-hand side. Verified against `ocamlc -dparsetree`. *)

type r = { mutable x : int }

let f (r : r) a b = r.x <- (a + b) (* REDUNDANT: `<-` already extends over the whole
                                       right-hand side; `r.x <- a + b` means the same *)
let g (r : r) a b = (r.x <- a) + b (* NECESSARY: without parens, `r.x <- a + b` assigns
                                       `a + b` to [r.x], not [a] followed by [+ b] *)

let set_indexed arr a b = arr.(a + b) <- 1 (* REDUNDANT: the index expression [a + b]
                                               needs no parens, [.( )] already delimits it *)

(* field access is tighter than application: `a b.x` reads `.x` off of
   [b] first, then applies [a] to that - the reverse needs explicit parens *)
let apply_then_field a b = (a b).x (* NECESSARY: without parens this is [a (b.x)] *)
let field_then_apply a b = a (b.x) (* REDUNDANT: `a b.x` already means this *)
