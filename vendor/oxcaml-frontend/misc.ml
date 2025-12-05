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

(* Errors *)

exception Fatal_error

let fatal_errorf fmt =
  Format.kfprintf
    (fun _ -> raise Fatal_error)
    Format.err_formatter
    ("@?>> Fatal error: " ^^ fmt ^^ "@.")

let fatal_error msg = fatal_errorf "%s" msg

(* ... *)

type ref_and_value = R : 'a ref * 'a -> ref_and_value

let protect_refs =
  let set_refs l = List.iter (fun (R (r, v)) -> r := v) l in
  fun refs f ->
    let backup = List.map (fun (R (r, _)) -> R (r, !r)) refs in
    set_refs refs;
    Fun.protect ~finally:(fun () -> set_refs backup) f

(* ... *)

let create_hashtable size init =
  let tbl = Hashtbl.create size in
  List.iter (fun (key, data) -> Hashtbl.add tbl key data) init;
  tbl

(* ... *)

module Stdlib = struct
  module String = struct
    include String
    module Set = Set.Make(String)
  end

  module Int = struct
    include Int
    let min (a : int) (b : int) = min a b
    let max (a : int) (b : int) = max a b
  end
end

(* ... *)

module Color = struct
  external isatty : out_channel -> bool = "caml_sys_isatty"

  (* reasonable heuristic on whether colors should be enabled *)
  let should_enable_color () =
    let term = try Sys.getenv "TERM" with Not_found -> "" in
    term <> "dumb"
    && term <> ""
    && isatty stderr

  type setting = Auto | Always | Never

  let default_setting = Auto
  let enabled = ref true

end

(* Terminal styling handling *)
module Style = struct
  (* use ANSI color codes, see https://en.wikipedia.org/wiki/ANSI_escape_code *)
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

  let ansi_of_color = function
    | Black -> "0"
    | Red -> "1"
    | Green -> "2"
    | Yellow -> "3"
    | Blue -> "4"
    | Magenta -> "5"
    | Cyan -> "6"
    | White -> "7"

  let code_of_style = function
    | FG c -> "3" ^ ansi_of_color c
    | BG c -> "4" ^ ansi_of_color c
    | Bold -> "1"
    | Reset -> "0"

  let ansi_of_style_l l =
    let s = match l with
      | [] -> code_of_style Reset
      | [s] -> code_of_style s
      | _ -> String.concat ";" (List.map code_of_style l)
    in
    "\x1b[" ^ s ^ "m"


  type Format.stag += Style of style list

  type tag_style ={
    ansi: style list;
    text_open:string;
    text_close:string
  }

  type styles = {
    error: tag_style;
    warning: tag_style;
    loc: tag_style;
    hint: tag_style;
    inline_code: tag_style;
  }

  let no_markup stl = { ansi = stl; text_close = ""; text_open = "" }

  let default_styles = {
      warning = no_markup [Bold; FG Magenta];
      error = no_markup [Bold; FG Red];
      loc = no_markup [Bold];
      hint = no_markup [Bold; FG Blue];
      inline_code= { ansi=[Bold]; text_open = {|"|}; text_close = {|"|} }
    }

  let cur_styles = ref default_styles
  let get_styles () = !cur_styles
  let set_styles s = cur_styles := s

  (* map a tag to a style, if the tag is known.
     @raise Not_found otherwise *)
  let style_of_tag s = match s with
    | Format.String_tag "error" ->  (!cur_styles).error
    | Format.String_tag "warning" ->(!cur_styles).warning
    | Format.String_tag "loc" -> (!cur_styles).loc
    | Format.String_tag "hint" -> (!cur_styles).hint
    | Format.String_tag "inline_code" -> (!cur_styles).inline_code
    | Style s -> no_markup s
    | _ -> raise Not_found


  let as_inline_code printer ppf x =
    Format.pp_open_stag ppf (Format.String_tag "inline_code");
    printer ppf x;
    Format.pp_close_stag ppf ()

  let inline_code ppf s = as_inline_code Format.pp_print_string ppf s

  let as_clflag flag printer ppf x =
    Format.fprintf ppf "@{<inline_code>%s %a@}" flag printer x

  (* either prints the tag of [s] or delegates to [or_else] *)
  let mark_open_tag ~or_else s =
    try
      let style = style_of_tag s in
      if !Color.enabled then ansi_of_style_l style.ansi else style.text_open
    with Not_found -> or_else s

  let mark_close_tag ~or_else s =
    try
      let style = style_of_tag s in
      if !Color.enabled then ansi_of_style_l [Reset] else style.text_close
    with Not_found -> or_else s

  (* add tag handling to formatter [ppf] *)
  let set_tag_handling ppf =
    let open Format in
    let functions = pp_get_formatter_stag_functions ppf () in
    let functions' = {functions with
      mark_open_stag=(mark_open_tag ~or_else:functions.mark_open_stag);
      mark_close_stag=(mark_close_tag ~or_else:functions.mark_close_stag);
    } in
    pp_set_mark_tags ppf true; (* enable tags *)
    pp_set_formatter_stag_functions ppf functions';
    ()

  let setup =
    let first = ref true in (* initialize only once *)
    let formatter_l =
      [Format.std_formatter; Format.err_formatter; Format.str_formatter]
    in
    let enable_color = function
      | Color.Auto -> Color.should_enable_color ()
      | Color.Always -> true
      | Color.Never -> false
    in
    fun o ->
      if !first then (
        first := false;
        Format.set_mark_tags true;
        List.iter set_tag_handling formatter_l;
        Color.enabled := (match o with
          | Some s -> enable_color s
          | None -> enable_color Color.default_setting)
      );
      ()
end

(* ... *)

module Error_style = struct
  type setting =
    | Contextual
    | Short

  let default_setting = Contextual
end

let normalise_eol s =
  let b = Buffer.create 80 in
    for i = 0 to String.length s - 1 do
      if s.[i] <> '\r' then Buffer.add_char b s.[i]
    done;
    Buffer.contents b

(* ... *)

let pp_two_columns ?(sep = "|") ?max_lines ppf (lines: (string * string) list) =
  let left_column_size =
    List.fold_left (fun acc (s, _) -> Int.max acc (String.length s)) 0 lines in
  let lines_nb = List.length lines in
  let ellipsed_first, ellipsed_last =
    match max_lines with
    | Some max_lines when lines_nb > max_lines ->
        let printed_lines = max_lines - 1 in (* the ellipsis uses one line *)
        let lines_before = printed_lines / 2 + printed_lines mod 2 in
        let lines_after = printed_lines / 2 in
        (lines_before, lines_nb - lines_after - 1)
    | _ -> (-1, -1)
  in
  Format.fprintf ppf "@[<v>";
  List.iteri (fun k (line_l, line_r) ->
    if k = ellipsed_first then Format.fprintf ppf "...@,";
    if ellipsed_first <= k && k <= ellipsed_last then ()
    else Format.fprintf ppf "%*s %s %s@," left_column_size line_l sep line_r
  ) lines;
  Format.fprintf ppf "@]"

(* ... *)

let print_see_manual ppf manual_section =
  let open Format in
  fprintf ppf "(see manual section %a)"
    (pp_print_list ~pp_sep:(fun f () -> pp_print_char f '.') pp_print_int)
    manual_section

(* ... *)

type (_, _) eq = Refl : ('a, 'a) eq
