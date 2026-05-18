open Ocaml_syntax

type range =
  | Around of (int * int)
  | Between of { start: int * int; stop: int * int }

(* Assumption: there is no overlap of the form {v [   (  ]  ) v}. *)
type location_wrt_range =
  | Disjoint
  | Encloses
  | Inside

let pos_to_lc (pos : Lexing.position) =
  pos.pos_lnum, pos.pos_cnum - pos.pos_bol

let relate_cursor ~loc line col =
  let (start_l, start_c) = pos_to_lc loc.Location.loc_start in
  let (end_l, end_c) = pos_to_lc loc.Location.loc_end in
  if start_l > line || end_l < line
   || (start_l = line && start_c > col)
   || (end_l = line && end_c < col)
  then Disjoint
  else Encloses

let relate_selection ~loc (sel_start_l, sel_start_c) (sel_end_l, sel_end_c) =
  let (node_start_l, node_start_c) = pos_to_lc loc.Location.loc_start in
  let (node_end_l, node_end_c) = pos_to_lc loc.Location.loc_end in
  if node_start_l > sel_end_l || node_end_l < sel_start_l
   || (node_start_l = sel_end_l && node_start_c > sel_end_c)
   || (node_end_l = sel_start_l && node_end_c < sel_start_c)
  then Disjoint
  else if node_start_l > sel_start_l || node_end_l < sel_end_l
          || (node_start_l = sel_start_l && node_start_c > sel_start_c)
          || (node_end_l = sel_end_l && node_end_c < sel_end_c)
  then Inside
  else Encloses

let relate ~loc = function
  | Around (line, col) -> relate_cursor ~loc line col
  | Between { start; stop } -> relate_selection ~loc start stop

let recurse range super node node_loc node_toks =
  match relate ~loc:node_loc range with
  | Disjoint | Inside -> None
  | Encloses ->
    match super node with
    | None -> Some node_toks
    | some -> some

let get_tokens =
  object(self)
    val mutable range = Around (-1, -1)

    inherit [Tokens.seq option] Traversals.lift as super

    (* Ppxlib.lift internal structure *)
    method record alist = List.find_map snd alist
    method constr _ = List.find_map Fun.id

    (* Leaf types. *)
    method bool _ = None
    method char _ = None
    method int _ = None
    method string _ = None
    method position _ = None

    (* Polymorphic types. *)
    method ref f x = f !x
    method tuple = List.find_map Fun.id
    method option f x = Option.bind x f
    method list f = List.find_map f

    method! expression e =
      recurse range super#expression e e.pexp_loc e.pexp_tokens

    method! value_binding vb =
      recurse range super#value_binding vb vb.pvb_loc vb.pvb_tokens

    method! structure_item si =
      recurse range super#structure_item si si.pstr_loc si.pstr_tokens

    method from_structure r cst =
      range <- r;
      self#structure cst
  end

module PP = struct
  open Document
  open Utils

  let token { Tokens.desc ; pos } =
    match desc with
    | Token (t, optional) ->
      let tok = string (Tokens.Raw.to_string t) in
      tok ^^ if optional then string "?" else empty
    | Lexer_directive _ -> string "#lexer_directive"
    | Comment { text; _ } ->
      let cmt =
        if String.length text > 10
        then string (String.sub text 0 6) ^/^ string "..."
        else string text
      in
      string "(*" ^/^ cmt ^/^ string "*)"
    | Child_node ->
      let int s = string (string_of_int s) in
      let lnum, col = pos_to_lc pos in
      string "child@" ^^ int lnum ^^ string ":" ^^ int col

  let seq = flow_map (break 1) token
end

let run start stop source =
  match
    Stylo.Pipeline.parse
      { kind = Impl; fname = "buffer"; source; start_line = 0 }
  with
  | Ok cst ->
    let range =
      match stop with
      | None -> Around start
      | Some stop -> Between { start; stop }
    in
    begin match get_tokens#from_structure range cst with
    | Some tokens ->
      PP.seq tokens
      |> Document.Print.to_string ~width:80
      |> Printf.printf "%s\n%!"
    | None -> Printf.printf "<no tokens at given location>\n%!"
    end;
    Ok ()
  | Error e ->
    Stylo.Pipeline.pp_error Format.err_formatter "buffer" e;
    Error ""

open Cmdliner
open Cmdliner.Term.Syntax

let cmd =
  Cmd.make (Cmd.info "dump-tokens") @@
  let open Arg in
  let pos_conv =
    let parser s =
      match String.split_on_char ':' s with
      | [ l; c ] -> Ok (int_of_string l, int_of_string c)
      | _ -> Error (Printf.sprintf "invalid position %S" s)
    in
    let pp ppf (l, c) = Format.fprintf ppf "%d:%d" l c in
    Conv.make ~docv:"LINE:COL" ~parser ~pp ()
  in
  let+ start =
    info ["start"]
    |> opt (some' pos_conv) None
    |> required
  and+ stop =
    info ["stop"]
    |> opt (some' pos_conv) None
    |> value
  in
  In_channel.input_all Stdlib.stdin
  |> run start stop

let () = exit (Cmd.eval_result cmd)
