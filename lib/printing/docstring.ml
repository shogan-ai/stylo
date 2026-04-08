open Document
open Document.Utils
open Ocaml_syntax
open Parsetree

open Jst_odoc_parser

let striped_lines s =
  let strip s =
    let is_ws = function
      | ' ' | '\t' | '\r' | '\n' -> true
      | _ -> false
    in
    let loop_until_non_ws ~stop_at ~dir s =
      let first, next_idx, past_end =
        match dir with
        | `From_start -> 0, succ, (fun i -> i >= stop_at)
        | `From_end -> String.length s - 1, pred, (fun i -> i < stop_at)
      in
      let rec aux i =
        if not (past_end i) && is_ws (String.get s i)
        then aux (next_idx i)
        else i
      in
      aux first
    in
    let end_ = loop_until_non_ws ~stop_at:0 ~dir:`From_end s in
    let start = loop_until_non_ws ~stop_at:end_ ~dir:`From_start s in
    String.sub s start (end_ - start + 1)
  in
  String.split_on_char '\n' s
  |> Std.List.drop_while ((=) "")
  |> List.rev_map strip
  |> Std.List.drop_while ((=) "")
  |> List.rev

module Odoc = struct
  let process_ocaml_block : (string -> t option) ref =
    ref (fun _ -> failwith "Odoc.process_ocaml_block: use before init")

  open Odoc_parser.Ast

  module Loc = Odoc_parser.Loc

  let located f e = f e.Loc.value

  let escape_lone_brackets s =
    let rec find_unmatched acc i =
      if i >= String.length s then
        List.rev acc
      else
        let acc =
          match s.[i] with
          | '[' -> (`Open, i) :: acc
          | ']' ->
            begin match acc with
            | (`Open, _) :: acc -> acc
            | _ -> (`Close, i) :: acc
            end
          | _ -> acc
        in
        find_unmatched acc (i + 1)
    in
    let to_escape = find_unmatched [] 0 in
    match to_escape with
    | [] -> s (* nothing to do *)
    | _ ->
      let buf = Buffer.create (String.length s + List.length to_escape) in
      let last_unmatched_pos =
        List.fold_left (fun start_pos (_, end_pos) ->
          let len = end_pos - start_pos in
          Buffer.add_substring buf s start_pos len;
          Buffer.add_char buf '\\';
          end_pos
        ) 0 to_escape
      in
      Buffer.add_substring buf s last_unmatched_pos
        (String.length s - last_unmatched_pos);
      Buffer.contents buf

  let code_span s =
    (* FIXME: necessarily on one line then? *)
    string "[" ^^ string (escape_lone_brackets s) ^^ string "]"

  let math_span s =
    group (string "{m" ^/^ string s ^^ string "}")

  let raw_markup target s =
    string "{%" ^^
    optional (fun t -> string (t ^ ":")) target ^^
    fancy_string s ^^
    string "%}"

  type inline_no_space =
    [ `Word of string
    | `Code_span of string
    | `Raw_markup of string option * string
    | `Styled of style * inline_element with_location list
    | `Reference of
        reference_kind * string with_location * inline_element with_location list
    | `Link of string * inline_element with_location list
    | `Math_span of string  (** @since 2.0.0 *) ]

  let don't_break_before : inline_no_space -> bool = function
    | `Word ("-" | "+") -> true (* would start a list if at BOL *)
    | _ -> false (* TODO: improve? *)

  let split_on_space (lst : inline_element with_location list) =
    let rec aux = function
      | [] -> [], []
      | { Loc.value = `Space _; _ } :: rest ->
        let g, gs = aux rest in
        [], g :: gs
      | { Loc.value =
            ( `Word _ | `Code_span _ | `Raw_markup _ | `Styled _ | `Reference _
            | `Link _ | `Math_span _) as elt ; _ } :: rest ->
        let g, gs = aux rest in
        elt :: g, gs
    in
    let fst_group, groups = aux lst in
    fst_group :: groups

  let rec inline_elements elts =
    match split_on_space elts with
    | [] -> empty
    | groups ->
      let spaced_groups = List.filter ((<>) []) groups in
      let append_group (space_before, acc) elts =
        let space =
          if not space_before then
            empty
          else if don't_break_before (List.hd elts (* we've filtered [] *)) then
            nbsp
          else
            break 1
        in
        let doc =
          List.fold_left (fun acc elt -> acc ^^ inline_no_space elt) empty elts
        in
        true, acc ^^ group (space ^^ doc)
      in
      List.fold_left append_group (false, empty) spaced_groups
      |> snd

  and inline_no_space : inline_no_space -> _ = function
    | `Word s -> string s
    | `Code_span cs -> code_span cs
    | `Math_span ms -> math_span ms
    | `Reference (kind, ref, alt) -> reference kind ref alt
    | `Link (url, alt) -> link url alt
    | `Styled (style, elts) -> styled style elts
    | `Raw_markup (markup_target, s) -> raw_markup markup_target s

  and link url alt =
    let url = string "{:" ^^ string url ^^ string "}" in
    group (
      match alt with
      | [] -> url
      | elts -> string "{" ^^ url ^^ inline_elements elts ^^ string "}"
    )

  and styled style text =
    let text = inline_elements text in
    let style =
      match style with
      | `Bold -> "b"
      | `Italic -> "i"
      | `Emphasis -> "e"
      | `Superscript -> "^"
      | `Subscript -> "_"
    in
    group (string "{" ^^ string style ^/^ text ^^ string "}")

  and reference kind ref alt =
    let ref = string "{!" ^^ string (Loc.value ref) ^^ string "}" in
    group (
      match kind with
      | `Simple -> ref
      | `With_text ->
        let text = inline_elements alt in
        string "{" ^^ ref ^^ text ^^ string "}"
    )

  let cb_meta_lang (lang, _metadata) = Loc.value lang

  let maybe_ocaml_code_block meta_opt content =
    let is_ocaml =
      Option.map cb_meta_lang meta_opt
      |> Option.value ~default:"ocaml"
      |> String.lowercase_ascii
      |> (=) "ocaml"
    in
    let source = Loc.value content in
    if is_ocaml then !process_ocaml_block source else None

  let possibly_multiline_verbatim_string (loc : Loc.span) s =
    let multiline = loc.start.line < loc.end_.line in
    if not multiline
    then break 1, fancy_string s
    else
      (* HACK: adding a \n in [s] means we bypass the printing engine and that
         line doesn't get indented. *)
      empty, fancy_string ("\n" ^ s)

  let code_block (meta_opt, content) =
    let meta = (* FIXME: tags! *)
      optional (fun meta -> string ("@" ^ cb_meta_lang meta)) meta_opt
    in
    let break, content =
      match maybe_ocaml_code_block meta_opt content with
      | Some res -> break 1, nest 2 (group res)
      | None ->
        let loc = Loc.location content in
        possibly_multiline_verbatim_string loc (Loc.value content)
    in
    group (string "{" ^^ meta ^^ string "[") ^^ break ^^
    content ^/^
    group (string "]" ^^ string "}")

  let verbatim ~loc s =
    let break, s = possibly_multiline_verbatim_string loc s in
    group (string "{v" ^^ break ^^ s ^/^ string "v}")

  let modules lst =
    group (
      string "{!modules:" ^^ break 0 ^^
      nest 2 (separate_map (break 1) (located string) lst) ^^
      string "}"
    )

  let math_block s =
    let content = separate_map hardline string (striped_lines s) in
    group (string "{math" ^/^ nest 2 content ^^ break 0 ^^ string "}")

  let media ref_kind href alt kind =
    let media_ref =
      let kind =
        match kind with
        | `Audio -> "audio"
        | `Video -> "video"
        | `Image -> "image"
      in
      let href =
        match Loc.value href with
        | `Reference s -> string "!" ^^ break 0 ^^ string s
        | `Link s -> string ":" ^^ break 0 ^^ string s
      in
      group (string "{" ^^ string kind ^^ href ^^ string "}")
    in
    match ref_kind with
    | `Simple -> media_ref
    | `With_text -> group (string "{" ^^ media_ref ^^ string alt ^^ string "}")

  let rec nestable_block_elements = function
    | [ elt ] -> nestable_block_element empty elt
    | elts -> separate_map hardline (nestable_block_element softline) elts

  and nestable_block_element extra_line elt =
    match Loc.value elt with
    | `Paragraph text -> inline_elements text ^^ extra_line
    | `Code_block cb -> code_block cb
    | `Verbatim vb -> verbatim ~loc:(Loc.location elt) vb
    | `Modules mods -> modules mods
    | `List (kind, `Heavy, elts) -> heavy_list kind elts
    | `List (kind, `Light, elts) -> light_list kind elts ^^ extra_line
    | `Table ((rows, align), `Heavy) -> heavy_table rows align
    | `Table ((rows, align), `Light) -> light_table rows align
    | `Math_block mb -> math_block mb
    | `Media (kind, href, alt, media_kind) -> media kind href alt media_kind

  and heavy_list kind elts =
    let kind, item =
      match kind with
      | `Ordered _ -> "ol", string "{li" (* should this become "{+"? meh. *)
      | `Unordered -> "ul", string "{-"
    in
    let pp_elt e =
      group (item ^/^ nest 2 (nestable_block_elements e) ^^ string "}")
    in
    group (
      string "{" ^^ string kind ^/^
      nest 1 (separate_map (break 1) pp_elt elts) ^^
      string "}"
    )

  and light_list kind elts =
    let bullet item_num =
      let str =
        match kind with
        | `Unordered -> "-"
        | `Ordered None -> "+"
        | `Ordered (Some (punct, num, _spacious)) ->
          let letter i = Char.chr (Char.code 'a' + i) in
          let num =
            match num with
            | `Number _ -> `Number (item_num + 1)
            | `Lower_case _ -> `Lower_case (letter item_num)
            | `Upper_case _ ->
              `Upper_case (Char.uppercase_ascii (letter item_num))
          in
          print_list_number punct num
      in
      string (str ^ " ")
    in
    let pp_elt i e = group (bullet i ^^ nest 2 @@ nestable_block_elements e) in
    let sep =
      match kind with
      | `Ordered (Some (_, _, true)) -> hardline ^^ hardline
      | _ -> hardline
    in
    List.mapi pp_elt elts
    |> separate sep

  and heavy_table rows _align_infos_opt =
    let pp_cell (elts, kind) =
      let kind =
        match kind with
        | `Header -> "th"
        | `Data -> "td"
      in
      group (
        string "{" ^^ string kind ^/^
        nest 2 (nestable_block_elements elts) ^^
        string "}"
      )
    in
    let pp_row cells =
      group (
        string "{tr" ^/^
        nest 2 (separate_map (break 1) pp_cell cells) ^^
        string "}"
      )
    in
    group (
      string "{table" ^/^
      nest 2 (separate_map hardline pp_row rows) ^^
      string "}"
    )

  and light_table rows align_info_opt =
    (* TODO: keep the light syntax once vertical alignment is implemented.
       In the meantime, normalize to heavy syntax. *)
    heavy_table rows align_info_opt

  let internal_tag = function
    | `Canonical sloc -> string "@canonical" ^/^ located string sloc
    | `Inline -> string "@inline"
    | `Open -> string "@open"
    | `Closed -> string "@closed"
    | `Hidden -> string "@hidden"
    | `Children_order order ->
      string "@children_order" ^/^ nestable_block_elements order
    | `Toc_status status ->
      string "@toc_status" ^/^ nestable_block_elements status
    | `Order_category _nestable_block_elem_loc_list -> string "TODO"
    | `Short_title _nestable_block_elem_loc_list -> string "TODO"

  let ocamldoc_tag = function
    | `Author s -> string "@author" ^/^ string s
    | `Deprecated text -> string "@deprecated" ^/^ nestable_block_elements text
    | `Param (id, text) ->
      string "@param" ^/^ string id ^/^ nestable_block_elements text
    | `Raise (exc, text) ->
      string "@raise" ^/^ string exc ^/^ nestable_block_elements text
    | `Return text -> string "@return" ^/^ nestable_block_elements text
    | `See (kind, ref, text) ->
      let ref =
        let left, right =
          match kind with
          | `Url -> "<", ">"
          | `File -> "'", "'"
          | `Document -> "\"", "\""
        in
        string left ^^ string ref ^^ string right
      in
      string "@see" ^/^ ref ^/^ nestable_block_elements text
    | `Since s -> string "@since" ^/^ string s
    | `Before (version, text) ->
      string "@before" ^/^ string version ^/^ nestable_block_elements text
    | `Version s -> string "@version" ^/^ string s

  let tag t =
    group (
      match t with
      | #ocamldoc_tag as ot -> ocamldoc_tag ot
      | #internal_tag as it -> internal_tag it
    )

  let heading (lvl, lbl, elems) =
    group (
      string "{" ^^
      string (string_of_int lvl) ^^
      optional (fun lbl -> string ":" ^^ string lbl) lbl ^/^
      inline_elements elems ^^
      string "}"
    )

  let block_element extra_line located =
    match Loc.value located with
    | `Heading hd -> heading hd ^^ extra_line
    | `Tag t -> tag t
    | #nestable_block_element as nbe ->
      nestable_block_element extra_line Loc.(at (location located) nbe)

  let pp_ast elts =
    let rec aux = function
      | [] -> empty
      | [ elt ] -> block_element empty elt
      | elt :: elts ->
        block_element softline elt ^^
        hardline ^^
        aux elts
    in
    aux elts

  let try_parse ~start_pos:location text =
    let res = Odoc_parser.parse_comment ~location ~text in
    match Odoc_parser.warnings res with
    | [] -> Some (Odoc_parser.ast res)
    | _ -> None
end

let as_odoc_markup_if_no_warnings ~kind ~(start_pos:Lexing.position) = function
  | "" -> as_comment (string "(**)")
  | text ->
    let opening, indent =
      match kind with
      | `Docstring -> "(**", 4
      | `Regular_comment -> "(*", 3
    in
    let doc =
      let start_pos =
        { start_pos with pos_cnum = start_pos.pos_cnum + indent - 1 }
      in
      match Odoc.try_parse ~start_pos text with
      | None -> string opening ^^ fancy_string text ^^ string "*)"
      | Some ast ->
        (* odoc-parser strips leading whitespaces. *)
        string (opening ^ " ") ^^ nest indent (
          Odoc.pp_ast ast ^^
          group (break 1 ^^ string "*)")
        )
    in
    as_comment (group doc)

let docstring ~start_pos txt =
  as_odoc_markup_if_no_warnings ~kind:`Docstring ~start_pos txt

let pp (Docstring (s, start_pos)) = docstring ~start_pos s
let pp_floating s =
  softline ^^ pp s ^^ softline

let pp_pre ds = softline ^^ softline ^^ pp ds

let attach ?(possibly_ambiguous=true) ?(extra_nest=Fun.id)
      ?(text = []) ?pre_doc ?post_doc t =
  extra_nest (
    begin match text with
    | [] -> empty
    | text ->
      softline ^^ softline ^^
      separate_map (break 1) pp text ^^
      softline ^^ softline
    end ^^
    optional pp_pre pre_doc
  ) ^?/^
  match post_doc with
  | None -> t
  | Some s ->
    group (t ^^ softest_break ^^ extra_nest (pp s)) ^^
    if possibly_ambiguous then softline else empty
