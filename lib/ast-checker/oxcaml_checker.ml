open Oxcaml_frontend
open Parsetree

let sort_attributes : attributes -> attributes = List.sort compare

let normalize_cmt_spaces doc =
  String.split_on_char ' ' doc
  |> List.filter ((<>) "")
  |> String.concat " "

let ignore_docstrings = ref true

let cleaner =
  let from_docstring attr =
    match attr.attr_name.txt with
    | "ocaml.doc" | "ocaml.txt"  -> true
    | _ -> false
  in
  object
    method unit () () = ()
    method format__formatter () x = x (* eww. *)

    inherit [unit] Traversals_helpers.map_with_context
    inherit [unit] Ast_mapper.map_with_context as super

    method location () _ = Location.none
    method! location_stack () _ = []

    method! attribute () attr =
      let attr_payload =
        if not (from_docstring attr) then
          attr.attr_payload
        else
          match attr.attr_payload with
          | PStr [ {
            pstr_desc =
              Pstr_eval
                ({ pexp_desc= Pexp_constant Pconst_string (doc, loc, None)
                 ; _ } as inner_exp,[]);
            _
          } as str ] ->
            let doc = normalize_cmt_spaces doc in
            let inner' =
              { inner_exp with
                pexp_desc = Pexp_constant (Pconst_string (doc, loc, None))
              }
            in
            PStr [ { str with pstr_desc = Pstr_eval (inner', []) }]
          | _ -> assert false
      in
      super#attribute () { attr with attr_payload }

    method! attributes () attrs =
      let attrs =
        if not !ignore_docstrings
        then attrs
        else List.filter (fun a -> not (from_docstring a)) attrs
      in
      super#attributes () ((* FIXME: why? *) sort_attributes attrs)

    method! pattern () p =
      let p =
        match p.ppat_desc with
        | Ppat_or
            (p1, { ppat_desc = Ppat_or (p2, p3); ppat_attributes; ppat_loc; _ })
          ->
          Ast_helper.Pat.or_
            ~loc:p.ppat_loc
            ~attrs:p.ppat_attributes
            (Ast_helper.Pat.or_ ~loc:ppat_loc ~attrs:ppat_attributes p1 p2)
            p3
        | _ -> p
      in
      super#pattern () p
  end

  (*
  method! visit_expression env exp =
    let {pexp_desc; pexp_attributes; _} = exp in
    match pexp_desc with
    (* convert [(c1; c2); c3] to [c1; (c2; c3)] *)
    | Pexp_sequence
        ({pexp_desc= Pexp_sequence (e1, e2); pexp_attributes= []; _}, e3) ->
      (* FIXME: what about ext_attrs?! *)
      self#visit_expression env
        (Exp.sequence e1
           (Exp.sequence ~attrs:pexp_attributes e2 e3))
    | _ -> super#visit_expression env exp

  method! visit_pattern env pat =
    let {ppat_desc; ppat_loc= loc1; ppat_attributes= attrs1; _} = pat in
    (* normalize nested or patterns *)
    match ppat_desc with
    | Ppat_or
        ( pat1
        , { ppat_desc= Ppat_or (pat2, pat3)
          ; ppat_loc= loc2
          ; ppat_attributes= attrs2
          ; _ } ) ->
        self#visit_pattern env
          (Pat.or_ ~loc:loc1 ~attrs:attrs1
             (Pat.or_ ~loc:loc2 ~attrs:attrs2 pat1 pat2)
             pat3)
    | _ -> super#visit_pattern env pat
     *)


type error = [
  | `Input_parse_error of exn
  | `Output_parse_error of exn
  | `Ast_changed of string
]

let report_parse_error ppf exn =
  match Location.error_of_exn exn with
  | Some `Already_displayed -> ()
  | Some `Ok report ->
    Format.fprintf ppf "%a" Location.print_report report
  | None ->
    Format.fprintf ppf "%s" (Printexc.to_string exn)

let pp_error : error -> _ = function
  | `Ast_changed fname -> Format.eprintf "%s: ast changed@." fname
  | `Input_parse_error exn ->
    Format.eprintf "@[<v>Error while parsing the input:@;@[<hov 2>%a@]@]@."
      report_parse_error exn
  | `Output_parse_error exn ->
    Format.eprintf "@[<v>Error while parsing the output:@;@[<hov 2>%a@]@]@."
      report_parse_error exn

type _ input_kind =
  | Impl : Parsetree.structure input_kind
  | Intf : Parsetree.signature input_kind

type 'a input = {
  fname : string;
  start_line : int;
  source : string;
  kind : 'a input_kind;
}

let parse (type a) (input : a input) wrap_exn : (a, _) result =
  let pos =
    { Lexing.pos_fname = input.fname
    ; pos_lnum = input.start_line
    ; pos_bol = 0
    ; pos_cnum = 0 }
  in
  let lb = Lexing.from_string input.source in
  Lexing.set_position lb pos;
  try
    Ok (
      match input.kind with
      | Impl -> Parse.implementation lb
      | Intf -> Parse.interface lb
    )
  with exn ->
    Error (wrap_exn exn)

let clean (type a) (kind : a input_kind) (ast : a) : a =
  match kind with
  | Impl -> cleaner#structure () ast
  | Intf -> cleaner#signature () ast

let input_wrap exn = `Input_parse_error exn
let output_wrap exn = `Output_parse_error exn

let (let*) = Result.bind

let check_same_ast input output =
  let* input_ast = parse input input_wrap in
  let input_ast = clean input.kind input_ast in
  let output =
    { input with
      fname = input.fname ^ ".out";
      source = output }
  in
  let* output_ast = parse output output_wrap in
  let output_ast = clean output.kind output_ast in
  if input_ast = output_ast
  then Ok ()
  else Error (`Ast_changed input.fname)

let check_same_ast ~fname ~start_line ~impl input output =
  if impl
  then check_same_ast { fname; start_line; kind = Impl; source = input} output
  else check_same_ast { fname; start_line; kind = Intf; source = input} output

