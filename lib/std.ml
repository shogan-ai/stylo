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


module List = struct

  (* Import from recent version of stdlib *)

  let take n l =
    let[@tail_mod_cons] rec aux n l =
      match n, l with
      | 0, _ | _, [] -> []
      | n, x::l -> x::aux (n - 1) l
    in
    if n <= 0 then [] else aux n l

  let drop n l =
    let rec aux i = function
      | _x::l when i < n -> aux (i + 1) l
      | rest -> rest
    in
    if n <= 0 then l else aux 0 l

  let take_while p l =
    let[@tail_mod_cons] rec aux = function
      | x::l when p x -> x::aux l
      | _rest -> []
    in
    aux l

  let rec drop_while p = function
    | x::l when p x -> drop_while p l
    | rest -> rest

  (* local additions *)

  (** splits the list before the first element on which the predicate returns
      false. *)
  let split_at p l =
    let rec aux = function
      | x::l when p x ->
        let taken, dropped = aux l in
        x::taken, dropped
      | rest -> [], rest
    in
    aux l

  let rec map_last ~f = function
    | [] -> []
    | [ x ] -> [ f x ]
    | x :: xs -> x :: map_last ~f xs
end
