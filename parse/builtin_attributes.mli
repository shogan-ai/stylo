(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                         Alain Frisch, LexiFi                           *)
(*                                                                        *)
(*   Copyright 2012 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** [has_attribute name attrs] is true if an attribute with name [name] or
    ["ocaml." ^ name] is present in [attrs].  It marks that attribute used for
    the purposes of misplaced attribute warnings. *)
val has_attribute : string -> Parsetree.attributes -> bool

(** [attr_equals_builtin attr s] is true if the name of the attribute is [s] or
    ["ocaml." ^ s].  This is useful for manually inspecting attribute names, but
    note that doing so will not result in marking the attribute used for the
    purpose of warning 53, so it is usually preferrable to use [has_attribute]
    or [select_attributes]. *)
val attr_equals_builtin : Parsetree.attribute -> string -> bool

(** The attribute placed on the inner [Ptyp_arrow] node in [x -> (y -> z)]
    (meaning the [y -> z] node) to indicate parenthesization. This is relevant
    for locals, as [local_ x -> (y -> z)] is different than
    [local_ x -> y -> z].
*)
val curry_attr_name : string
val curry_attr : Location.t -> Parsetree.attribute
val has_curry: Parsetree.attributes -> bool
