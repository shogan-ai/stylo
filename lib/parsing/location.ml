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

open Lexing

type t =
  { loc_start: position; loc_end: position; loc_ghost: bool }

let compare_position : position -> position -> int =
  fun
    { pos_fname = pos_fname_1
    ; pos_lnum = pos_lnum_1
    ; pos_bol = pos_bol_1
    ; pos_cnum = pos_cnum_1
    }
    { pos_fname = pos_fname_2
    ; pos_lnum = pos_lnum_2
    ; pos_bol = pos_bol_2
    ; pos_cnum = pos_cnum_2
    }
  ->
    match String.compare pos_fname_1 pos_fname_2 with
    | 0 -> begin match Int.compare pos_lnum_1 pos_lnum_2 with
      | 0 -> begin match Int.compare pos_bol_1 pos_bol_2 with
        | 0 -> Int.compare pos_cnum_1 pos_cnum_2
        | i -> i
      end
      | i -> i
    end
    | i -> i
;;

let compare
      { loc_start = loc_start_1
      ; loc_end = loc_end_1
      ; loc_ghost = loc_ghost_1 }
      { loc_start = loc_start_2
      ; loc_end = loc_end_2
      ; loc_ghost = loc_ghost_2 }
  =
  match compare_position loc_start_1 loc_start_2 with
  | 0 -> begin match compare_position loc_end_1 loc_end_2 with
    | 0 -> Bool.compare loc_ghost_1 loc_ghost_2
    | i -> i
  end
  | i -> i
;;

let in_file s =
  let pos = { Lexing.dummy_pos with pos_fname = s } in
  { loc_start = pos; loc_end = pos; loc_ghost = true }

let none = in_file "_none_"
let is_none l = (l = none)

let curr lexbuf = {
  loc_start = lexbuf.lex_start_p;
  loc_end = lexbuf.lex_curr_p;
  loc_ghost = false
}

let init lexbuf ?(lnum=1) fname =
  lexbuf.lex_curr_p <- {
    pos_fname = fname;
    pos_lnum = lnum;
    pos_bol = 0;
    pos_cnum = 0;
  }

let ghostify l =
  if l.loc_ghost
  then l
  else { l with loc_ghost = true }

let symbol_rloc () = {
  loc_start = Parsing.symbol_start_pos ();
  loc_end = Parsing.symbol_end_pos ();
  loc_ghost = false;
}

let symbol_gloc () = {
  loc_start = Parsing.symbol_start_pos ();
  loc_end = Parsing.symbol_end_pos ();
  loc_ghost = true;
}

let rhs_loc n = {
  loc_start = Parsing.rhs_start_pos n;
  loc_end = Parsing.rhs_end_pos n;
  loc_ghost = false;
}

let rhs_interval m n = {
  loc_start = Parsing.rhs_start_pos m;
  loc_end = Parsing.rhs_end_pos n;
  loc_ghost = false;
}

(* return file, line, char from the given position *)
let get_pos_info pos =
  (pos.pos_fname, pos.pos_lnum, pos.pos_cnum - pos.pos_bol)

let merge ?(ghost = true) locs =
  let hd, tl =
    match locs with
    | hd :: tl -> hd, tl
    | [] -> failwith "Compiler bug: Called [Location.merge] with an empty list"
  in
  List.fold_left
    (fun acc x ->
      let loc_start =
        if compare_position x.loc_start acc.loc_start < 0
        then x.loc_start else acc.loc_start
      in
      let loc_end =
        if compare_position x.loc_end acc.loc_end > 0
        then x.loc_end else acc.loc_end
      in
      let loc_ghost = x.loc_ghost || acc.loc_ghost in
      { loc_start; loc_end; loc_ghost })
    { hd with loc_ghost = hd.loc_ghost || ghost }
    tl

type 'a loc = {
  txt : 'a;
  loc : t;
}

let mkloc txt loc = { txt ; loc }
let mknoloc txt = mkloc txt none
let get_txt { txt } = txt
let get_loc { loc } = loc
let map f { txt; loc} = {txt = f txt; loc}
let compare_txt f { txt=t1 } { txt=t2 } = f t1 t2

(******************************************************************************)
(* Input info *)

let input_name = ref "_none_"
let input_lexbuf = ref (None : lexbuf option)
