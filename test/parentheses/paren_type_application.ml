(* Type-constructor application (`'a list`, `(int, string) result`) binds
   tighter than both `*` and `->`, and chains left-to-right. Verified
   against `ocamlc -dparsetree`. *)

type t1
type t2

(* application binds tighter than `*`: parenthesizing the applied type is
   redundant, but wrapping a tuple to be the constructor's argument is
   mandatory *)
type redundant_1 = t1 * (t2 list)
type necessary_1 = (t1 * t2) list (* without parens: t1 * (t2 list), a tuple whose
                                      second component is a list, not a list of pairs *)

(* application binds tighter than `->` too *)
type redundant_2 = t1 -> t2 list

(* the `(arg1, arg2, ...) constr` form for a multi-parameter type
   constructor is not "redundant parens around a tuple" - it is the
   mandatory concrete syntax for multi-argument application. There is no
   unparenthesized spelling: bare `int, string result` does not parse at
   all (a plain `,` is not valid there without the enclosing parens). *)
type ('a, 'b) pair_of

type necessary_2 = (t1, t2) pair_of

(* single-argument application chains left-to-right, postfix-style, and
   parenthesizing an intermediate step is always redundant *)
type redundant_3 = ('a option) list
type also_redundant_3 = 'a option list (* means the same: (('a option) list) *)
