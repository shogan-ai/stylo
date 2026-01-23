(**************************************************************************)
(* *)
(* OCaml *)
(* *)
(* Thomas Gazagnaire, OCamlPro *)
(* Fabrice Le Fessant, INRIA Saclay *)
(* Hongbo Zhang, University of Pennsylvania *)
(* *)
(* Copyright 2007 Institut National de Recherche en Informatique et *)
(* en Automatique. *)
(* *)
(* All rights reserved. This file is distributed under the terms of *)
(* the GNU Lesser General Public License version 2.1, with the *)
(* special exception on linking described in the file LICENSE. *)
(* *)
(**************************************************************************)

let tyvar_of_name s =
  if String.length s >= 2 && s.[1] = '\''
  then (* without the space, this would be parsed as a character literal *)
    "' " ^ s
  else if Lexer.is_keyword s
  then "'\\#" ^ s
  else if String.equal s "_"
  then s
  else "'" ^ s
;;

let tyvar ppf s = Format.fprintf ppf "%s" (tyvar_of_name s)
