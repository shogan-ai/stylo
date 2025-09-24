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

class virtual ['self] reduce = object(self : 'self)
  method virtual visit_tokens : 'env. 'env -> Tokens.seq -> 'a
  method virtual zero : 'a
  method virtual plus : 'a -> 'a -> 'a

  method private visit_desc : 'env. 'env -> desc -> 'a = fun env ->
    function
    | Lident _ -> self#zero
    | Ldot (t, _) -> self#visit_longident env t
    | Lapply (t1, t2) ->
      self#plus (self#visit_longident env t1) (self#visit_longident env t2)

  method visit_longident : 'env. 'env -> t -> 'a = fun env t ->
    self#plus (self#visit_desc env t.desc) (self#visit_tokens env t.tokens)
end

let last t =
  match t.desc with
    Lident s -> s
  | Ldot(_, s) -> s
  | Lapply(_, _) -> failwith "Longident.last"
