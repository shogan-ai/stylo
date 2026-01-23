module Stdlib : sig
  module String : sig
    include module type of struct
      include String
    end

    module Set : Set.S with type elt = t
  end

  module Int : sig
    include module type of struct
      include Int
    end

    val min : t -> t -> t
    val max : t -> t -> t
  end
end

val fatal_error : string -> _
val fatal_errorf : ('a, Format.formatter, unit, 'b) format4 -> 'a
val create_hashtable : int -> ('a * 'b) list -> ('a, 'b) Hashtbl.t
val print_see_manual : Format.formatter -> int list -> unit

val pp_two_columns
  :  ?sep:string
  -> ?max_lines:int
  -> Format.formatter
  -> (string * string) list
  -> unit

val normalise_eol : string -> string

type ref_and_value = R : 'a ref * 'a -> ref_and_value

val protect_refs : ref_and_value list -> (unit -> 'a) -> 'a

type (_, _) eq = Refl : ('a, 'a) eq

module Color : sig
  type setting =
    | Auto
    | Always
    | Never

  val should_enable_color : unit -> bool
  val default_setting : setting
  val enabled : bool ref
end

module Error_style : sig
  type setting =
    | Contextual
    | Short

  val default_setting : setting
end

module Style : sig
  type color =
    | Black
    | Red
    | Green
    | Yellow
    | Blue
    | Magenta
    | Cyan
    | White

  type style =
    | FG of color (* foreground *)
    | BG of color (* background *)
    | Bold
    | Reset

  type Format.stag += Style  of style list

  type tag_style =
    { ansi : style list
    ; text_open : string
    ; text_close : string
    }

  type styles =
    { error : tag_style
    ; warning : tag_style
    ; loc : tag_style
    ; hint : tag_style
    ; inline_code : tag_style
    }

  val setup : Color.setting option -> unit
  val get_styles : unit -> styles
  val set_styles : styles -> unit
  val inline_code : Format.formatter -> string -> unit

  val as_inline_code
    :  (Format.formatter -> 'a -> unit)
    -> Format.formatter
    -> 'a
    -> unit

  val as_clflag
    :  string
    -> (Format.formatter -> 'a -> unit)
    -> Format.formatter
    -> 'a
    -> unit
end
