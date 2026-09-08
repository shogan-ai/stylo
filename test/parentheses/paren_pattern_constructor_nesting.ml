(* A commonly assumed rule - "a constructor pattern nested inside another
   constructor's argument always needs parens" - is only half true. Verified
   against `ocamlc -dparsetree`; this file exists because the first draft of
   this gallery got it wrong by trusting the assumption instead of checking. *)

type t = A of int

(* on its own, nesting is REDUNDANT: a constructor's argument position is
   unambiguous (a constructor takes at most one argument, so there's no
   application-style flattening to worry about, unlike expressions), and
   this holds for polymorphic variants too *)
let redundant_1 = function
  | Some (A n) -> n
  | _ -> 0

let redundant_2 = function
  | Some A n -> n (* parses identically to [Some (A n)] *)
  | _ -> 0

let redundant_3 = function
  | `Foo (`Bar n) -> n
  | _ -> 0

let redundant_4 = function
  | `Foo `Bar n -> n (* parses identically to [`Foo (`Bar n)] *)
  | _ -> 0

(* stylo's normalizer (lib/normalize/pattern.ml) always adds these parens
   anyway when a constructor-with-argument is nested in another
   constructor's argument slot - not because it's required here, but
   because it's the SAFE default: as soon as a looser construct (`::`, `,`,
   `|`) is adjacent, the same-looking nesting stops being redundant and the
   parens become load-bearing (see below). Blanket-adding them avoids
   having to prove, case by case, that no looser construct is nearby. *)

(* as soon as `::` sits right after the nested constructor's argument, the
   parens become MANDATORY: without them, `::` binds looser than the whole
   constructor chain and attaches at the outer level instead *)
let necessary_1 = function
  | Some (A n :: rest) -> ignore rest; n
  | _ -> 0

let necessary_1_without_parens_means_this = function
  | Some A n :: rest -> ignore rest; n (* really: (Some (A n)) :: rest *)
  | _ -> 0
