(* A few more type-level parenthesization facts, each checked against
   `ocamlc -dparsetree` or a direct parse check. *)

type t1
type t2

(* `as` in a type has very low precedence, like `as` in a pattern - it
   grabs the whole preceding type, so wrapping that type in parens first is
   redundant *)
type redundant_1 = < area : float > as 'a
type also_redundant_1 = (< area : float >) as 'a (* means the same thing *)

(* a package type `(module S)` is not "redundant parens around a module
   type" - the parens are mandatory concrete syntax, exactly like a type
   constraint `(e : t)` on the expression side. Bare `module S` is not a
   valid type at all. *)
module type S = sig end

type necessary_1 = (module S)

(* a class-type reference `#c` binds like any other type-constructor
   application: tight, with no parens needed around it *)
class type comparable = object
  method compare : comparable -> int
end

type redundant_2 = #comparable -> #comparable -> int

(* a polymorphic record field `'a. t` needs no additional parens around
   its body, even though it turns the field type "inside out" compared to
   an ordinary field *)
type mapper = { apply : 'a. 'a list -> 'a list }
