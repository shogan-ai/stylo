(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Long identifiers, used in parsetree.

  {b Warning:} this module is unstable and part of
  {{!Compiler_libs}compiler-libs}.

  To print a longident, see {!Pprintast.longident}, using
    {!Format.asprintf} to convert to a string.

*)

type str_or_op =
  | Str of string
  | Str_trailing_hash of string (* FIXME? *)
  | Op of string
  | DotOp of string * [ `Paren | `Brace | `Bracket ] * string * bool

type desc =
    Lident of str_or_op
  | Ldot of t * str_or_op
  | Lapply of t * t

and t = { desc: desc; tokens: Tokens.seq }

val last: t -> str_or_op

class virtual ['self] reduce : object('self)
  method virtual visit_tokens : 'env. 'env -> Tokens.seq -> 'a
  method virtual zero : 'a
  method virtual plus : 'a -> 'a -> 'a

  method visit_longident : 'env. 'env -> t -> 'a
end
