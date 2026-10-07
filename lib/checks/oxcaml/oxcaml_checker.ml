open Oxcaml_frontend
open Checks
open Parsetree

let sort_attributes : attributes -> attributes = List.sort compare

let cleaner erase =
  let from_docstring attr =
    match attr.attr_name.txt with
    | "ocaml.doc" | "ocaml.text" -> true
    | _ -> false
  in
  let do_erase : 'a. ('a -> 'a) -> 'a -> 'a =
    fun eraser v -> if erase then eraser v else v
  in
  object
    method unit () () = ()

    method format__formatter () x = x (* eww. *)

    method format_doc__t () x = x

    inherit [unit] Traversals_helpers.map_with_context

    inherit [unit] Ast_mapper.map_with_context as super

    method location () _ = Location.none

    method! location_stack () _ = []

    method! modes () m = do_erase Erase_for_checker.modes m |> super#modes ()

    method! modalities () m =
      do_erase Erase_for_checker.modalities m |> super#modalities ()

    method! attribute () attr =
      let attr_payload =
        if not (from_docstring attr)
        then attr.attr_payload
        else
          (* By turning each docstring into an empty string, we still check that
             docstrings are attached at the same place, while ignoring the
             actual content (which has been reformated). *)
          let open Ast_helper in
          let loc = Location.none in
          let e_string = Exp.constant ~loc @@ Const.string ~loc "" in
          PStr [ Str.eval ~loc e_string ]
      in
      super#attribute () { attr with attr_payload }

    method! attributes () attrs =
      sort_attributes attrs (* FIXME: why? *) |> super#attributes ()

    method! constant_desc () c =
      do_erase Erase_for_checker.constant_desc c |> super#constant_desc ()

    method! expression () e =
      do_erase Erase_for_checker.expression e |> super#expression ()

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
      do_erase Erase_for_checker.pattern p |> super#pattern ()

    method! function_param_desc () fp =
      do_erase Erase_for_checker.function_param_desc fp
      |> super#function_param_desc ()

    method! core_type () ct =
      do_erase Erase_for_checker.core_type ct |> super#core_type ()

    method! label_declaration () lbl =
      do_erase Erase_for_checker.label_declaration lbl
      |> super#label_declaration ()

    method! constructor_argument () c =
      do_erase Erase_for_checker.constructor_argument c
      |> super#constructor_argument ()

    method! constructor_declaration () c =
      do_erase Erase_for_checker.constructor_declaration c
      |> super#constructor_declaration ()

    method! extension_constructor_kind () eck =
      do_erase Erase_for_checker.extension_constructor_kind eck
      |> super#extension_constructor_kind ()

    method! type_kind () tk =
      do_erase Erase_for_checker.type_kind tk |> super#type_kind ()

    method! type_declaration () td =
      do_erase Erase_for_checker.type_declaration td
      |> super#type_declaration ()

    method! module_type () m =
      do_erase Erase_for_checker.module_type m |> super#module_type ()

    method! module_expr () m =
      do_erase Erase_for_checker.module_expr m |> super#module_expr ()

    method! signature () s =
      do_erase Erase_for_checker.signature s |> super#signature ()

    method! structure () s =
      do_erase Erase_for_checker.structure s |> super#structure ()
  end
;;

type options =
  { syntax_quotations : bool
  ; erase_jane_syntax : bool
  ; debug : bool
  }

type ast =
  | Structure of Parsetree.structure
  | Signature of Parsetree.signature

let parse options (input : Ocaml_syntax.Source.t) wrap_exn : (ast, _) result =
  Oxcaml_frontend.Config.syntax_quotations := options.syntax_quotations;
  let pos =
    { Lexing.pos_fname = input.fname
    ; pos_lnum = input.start_line
    ; pos_bol = 0
    ; pos_cnum = 0
    }
  in
  let lb = Lexing.from_string input.source in
  Lexing.set_position lb pos;
  try
    Ok
      (match input.kind with
       | Impl -> Structure (Parse.implementation lb)
       | Intf -> Signature (Parse.interface lb))
  with
  | exn -> Error (wrap_exn lb.lex_start_p lb.lex_curr_p exn)
;;

let clean ~erase_jane_syntax = function
  | Structure str -> Structure ((cleaner erase_jane_syntax)#structure () str)
  | Signature sg -> Signature ((cleaner erase_jane_syntax)#signature () sg)
;;

let report_parse_error ppf exn =
  match Location.error_of_exn exn with
  | Some `Already_displayed -> ()
  | Some (`Ok report) -> Format.fprintf ppf "%a" Location.print_report report
  | None -> Format.fprintf ppf "%s" (Printexc.to_string exn)
;;

let parser : Errors.parser =
  Reference { name = "upstream"; report_exn = report_parse_error }
;;

let input_wrap startp endp exn = parser, startp, endp, exn
let output_wrap _ _ exn = `Output_parse_error (parser, exn)
let ( let* ) = Result.bind

type ast_source =
  | Input
  | Stylo

let dump_ast options (input : Ocaml_syntax.Source.t) ~src ast =
  let fname =
    input.fname
    ^
    match src with
    | Input -> ".input-tree"
    | Stylo -> ".output-tree"
  in
  Debug.dump_to_file
    ~enabled:options.debug
    ~or_:()
    fname
    Oxcaml_frontend.Printast.(
      fun ppf ->
        match ast with
        | Structure str -> implementation ppf str
        | Signature sg -> interface ppf sg)
;;

let dump_out options (output : Ocaml_syntax.Source.t) =
  Debug.dump_to_file ~enabled:options.debug output.fname (fun ppf ->
    Format.pp_print_string ppf output.source)
;;

let check_same_ast options input_ast (output : Ocaml_syntax.Source.t) =
  let input_ast =
    clean ~erase_jane_syntax:options.erase_jane_syntax input_ast
  in
  let* output_ast =
    let output = { output with fname = output.fname ^ ".out" } in
    parse options output output_wrap
    |> Result.map_error (fun err ->
      dump_out options ~or_:() output;
      err)
  in
  let output_ast = clean ~erase_jane_syntax:false output_ast in
  if input_ast = output_ast
  then Ok ()
  else (
    dump_ast options output ~src:Input input_ast;
    dump_ast options output ~src:Stylo output_ast;
    Error (`Ast_changed (parser, output.fname)))
;;

let parse options i = parse options i input_wrap
