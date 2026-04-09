(** A regular docstring *)

(*_ "ignored" comments start with _ (no space before!) to please ppx_js_style
  --check-doc-comments. *)

module Nest : sig
  val foo : int
  (*= There are also
 verbatim
          comments *)
end
