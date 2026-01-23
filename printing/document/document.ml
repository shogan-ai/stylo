(** A pretty printing library in the style of
    {{:https://homepages.inf.ed.ac.uk/wadler/papers/prettier/prettier.pdf} Prettier} and
    {{:https://github.com/fpottier/pprint} PPrint}, but special cased for stylo's needs.

    In particular it doesn't expose an equivalent PPrint's [IfFlat] operator, as that is
    in general not well behaved wrt. automatic comment insertion. Instead it provides
    extra primitives which build such choices in but only at the level of leaves. *)

include Core
module Print = Print
module Utils = Utils
