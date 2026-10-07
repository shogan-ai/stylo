open Ocaml_syntax.Parsetree

module Options : sig
  type t =
    { erase_jane_syntax : bool (** erase OxCaml extensions *)
    ; insert_parentheses : bool (** parenthesize all expressions *)
    ; remove_parentheses : bool (** remove unnecessary parentheses *)
    }
end

val structure : Options.t -> structure -> structure
val signature : Options.t -> signature -> signature
