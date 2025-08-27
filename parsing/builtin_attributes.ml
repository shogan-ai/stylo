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

let attr_equals_builtin {Parsetree.attr_name = {txt; _}; _} s =
  (* Check for attribute s or ocaml.s.  Avoid allocating a fresh string. *)
  txt = s ||
  (   String.length txt = 6 + String.length s
   && String.starts_with ~prefix:"ocaml." txt
   && String.ends_with ~suffix:s txt)

let has_attribute nm attrs =
  List.exists (fun a -> attr_equals_builtin a nm) attrs

let curry_attr_name = "extension.curry"

let has_curry attrs =
  has_attribute curry_attr_name attrs
  || has_attribute "curry" attrs

let curry_attr loc =
  Ast_helper.Attr.mk ~loc:Location.none (Location.mkloc curry_attr_name loc) (PStr [])
;;
