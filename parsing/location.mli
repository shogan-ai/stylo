(**************************************************************************)
(* *)
(* OCaml *)
(* *)
(* Xavier Leroy, projet Cristal, INRIA Rocquencourt *)
(* *)
(* Copyright 1996 Institut National de Recherche en Informatique et *)
(* en Automatique. *)
(* *)
(* All rights reserved. This file is distributed under the terms of *)
(* the GNU Lesser General Public License version 2.1, with the *)
(* special exception on linking described in the file LICENSE. *)
(* *)
(**************************************************************************)

(** Source code locations (ranges of positions), used in parsetree.

    {b Warning:} this module is unstable and part of {{!Compiler_libs} compiler-libs}. *)

(* loc_ghost: Ghost expressions and patterns: expressions and patterns that do not appear
   explicitly in the source file they have the loc_ghost flag set to true. Then the
   profiler will not try to instrument them and the -annot option will not try to display
   their type.

   Every grammar rule that generates an element with a location must make at most one
   non-ghost element, the topmost one.

   How to tell whether your location must be ghost: A location corresponds to a range of
   characters in the source file. If the location contains a piece of code that is
   syntactically valid (according to the documentation), and corresponds to the AST node,
   then the location must be real; in all other cases, it must be ghost.
*)

type t =
  { loc_start : Lexing.position
  ; loc_end : Lexing.position
  ; loc_ghost : bool
  }

(** Note on the use of Lexing.position in this module. If [pos_fname = ""], then use
    [!input_name] instead. If [pos_lnum = -1], then [pos_bol = 0]. Use [pos_cnum] and
    re-parse the file to get the line and character numbers. Else all fields are correct. *)

(** Strict comparison: Compares all fields of the two locations, irrespective of whether
    or not they happen to refer to the same place. For fully-defined locations within the
    same file, is guaranteed to return them in source order; otherwise, or if given two
    locations that differ only in ghostiness, is just guaranteed to produce a consistent
    order, but which one is unspecified. *)
val compare : t -> t -> int

val to_string : t -> string

(** An arbitrary value of type [t]; describes an empty ghost range. *)
val none : t

(** True for [Location.none], false any other location *)
val is_none : t -> bool

(** Return an empty ghost range located in a given file. *)
val in_file : string -> t

(** Set the file name and line number of the [lexbuf] to be the start of the named file. *)
val init : Lexing.lexbuf -> string -> unit

(** Get the location of the current token from the [lexbuf]. *)
val curr : Lexing.lexbuf -> t

(** Return a version of the location with [loc_ghost = true] *)
val ghostify : t -> t

val symbol_rloc : unit -> t
val symbol_gloc : unit -> t

(** [rhs_loc n] returns the location of the symbol at position [n], starting at 1, in the
    current parser rule. *)
val rhs_loc : int -> t

val rhs_interval : int -> int -> t

(** file, line, char *)
val get_pos_info : Lexing.position -> string * int * int

(** [merge locs] returns the location covering all locations from [locs]. It raises if
    [locs] is empty, and the result only makes sense if all the locations are from the
    same file. If [~ghost:false] is passed, the result location will only be ghost if one
    of the input locations was ghost. The default is [~ghost:true], which causes the
    result location to be ghost no matter what *)
val merge : ?ghost:bool -> t list -> t

type 'a loc =
  { txt : 'a
  ; loc : t
  }

val mknoloc : 'a -> 'a loc
val mkloc : 'a -> t -> 'a loc
val get_txt : 'a loc -> 'a
val get_loc : 'a loc -> t
val map : ('a -> 'b) -> 'a loc -> 'b loc
val compare_txt : ('a -> 'b -> 'c) -> 'a loc -> 'b loc -> 'c

(** {1 Input info} *)

val input_name : string ref
val input_lexbuf : Lexing.lexbuf option ref
