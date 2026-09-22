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

(* This is a copy of the bits of the compiler's [utils/misc.mli] that are needed
   for parsing. *)

module Utf8_lexeme : sig
  type t = string

  (** Normalize the given UTF-8 encoded string. Invalid UTF-8 sequences results
      in a error and are replaced by U+FFFD. Identifier characters are put in
      NFC normalized form. Other Unicode characters are left unchanged. *)
  val normalize : string -> (t, t) Result.t

  (** Like [normalize], but if the string starts with a lowercase identifier
      character, it is replaced by the corresponding uppercase character.
      Subsequent characters are not changed. *)
  val capitalize : string -> (t, t) Result.t

  (** Like [normalize], but if the string starts with an uppercase identifier
      character, it is replaced by the corresponding lowercase character.
      Subsequent characters are not changed. *)
  val uncapitalize : string -> (t, t) Result.t

  (** Returns [true] if the given normalized string starts with an uppercase
      identifier character, [false] otherwise. May return wrong results if the
      string is not normalized. *)
  val is_capitalized : t -> bool

  (** Check whether the given normalized string is a valid OCaml identifier:
      - all characters are identifier characters
      - it does not start with a digit or a single quote *)
  val is_valid_identifier : t -> bool

  (** Returns [true] if the given normalized string only contains lowercase
      identifier character, [false] otherwise. May return wrong results if the
      string is not normalized. *)
  val is_lowercase : t -> bool

  type validation_result =
    | Valid
    | Invalid_character of Uchar.t (** Character not allowed *)
    | Invalid_beginning of Uchar.t (** Character not allowed as first char *)

  (** Like [is_valid_identifier], but returns a more detailed error code. Dots
      can be allowed to extend support to path-like identifiers. *)
  val validate_identifier : ?with_dot:bool -> t -> validation_result

  (** Checks whether the given normalized string starts with an identifier
      character other than a digit or a single quote. Subsequent characters are
      not checked. *)
  val starts_like_a_valid_identifier : t -> bool
end
