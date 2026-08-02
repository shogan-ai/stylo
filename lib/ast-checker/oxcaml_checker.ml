open Oxcaml_frontend
open Parsetree

let sort_attributes : attributes -> attributes = List.sort compare

let cleaner erase =
  let from_docstring attr =
    match attr.attr_name.txt with
    | "ocaml.doc" | "ocaml.text"  -> true
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

    method! modes () m =
      do_erase Erase_jane_syntax.modes m
      |> super#modes ()

    method! modalities () m =
      do_erase Erase_jane_syntax.modalities m
      |> super#modalities ()

    method! attribute () attr =
      let attr_payload =
        if not (from_docstring attr) then
          attr.attr_payload
        else
          (* By turning each docstring into an empty string, we still check that
             docstrings are attached at the same place, while ignoring the
             actual content (which has been reformated). *)
          let open Ast_helper in
          let loc = Location.none in
          let e_string = Exp.constant ~loc @@ Const.string ~loc "" in
          PStr [Str.eval ~loc e_string]
      in
      super#attribute () { attr with attr_payload }

    method! attributes () attrs =
      sort_attributes attrs (* FIXME: why? *)
      |> super#attributes ()

    method! constant_desc () c =
      do_erase Erase_jane_syntax.constant_desc c
      |> super#constant_desc ()

    method! expression () e =
      do_erase Erase_jane_syntax.expression e
      |> super#expression ()

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
      do_erase Erase_jane_syntax.pattern p
      |> super#pattern ()

    method! function_param_desc () fp =
      do_erase Erase_jane_syntax.function_param_desc fp
      |> super#function_param_desc ()

    method! core_type () ct =
      do_erase Erase_jane_syntax.core_type ct
      |> super#core_type ()

    method! label_declaration () lbl =
      do_erase Erase_jane_syntax.label_declaration lbl
      |> super#label_declaration ()

    method! constructor_argument () c =
      do_erase Erase_jane_syntax.constructor_argument c
      |> super#constructor_argument ()

    method! constructor_declaration () c =
      do_erase Erase_jane_syntax.constructor_declaration c
      |> super#constructor_declaration ()

    method! extension_constructor_kind () eck =
      do_erase Erase_jane_syntax.extension_constructor_kind eck
      |> super#extension_constructor_kind ()

    method! type_kind () tk =
      do_erase Erase_jane_syntax.type_kind tk
      |> super#type_kind ()

    method! type_declaration () td =
      do_erase Erase_jane_syntax.type_declaration td
      |> super#type_declaration ()

    method! module_type () m =
      do_erase Erase_jane_syntax.module_type m
      |> super#module_type ()

    method! module_expr () m =
      do_erase Erase_jane_syntax.module_expr m
      |> super#module_expr ()

    method! signature () s =
      do_erase Erase_jane_syntax.signature s
      |> super#signature ()

    method! structure () s =
      do_erase Erase_jane_syntax.structure s
      |> super#structure ()
  end

type _ input_kind =
  | Impl : Parsetree.structure  input_kind
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
    Error (wrap_exn lb.lex_start_p lb.lex_curr_p exn)

let clean (type a) ~erase_jane_syntax (kind : a input_kind) (ast : a) : a =
  match kind with
  | Impl -> (cleaner erase_jane_syntax)#structure () ast
  | Intf -> (cleaner erase_jane_syntax)#signature () ast

let input_wrap startp endp exn = `Input_parse_error (Errors.Oxcaml's, startp, endp, exn)
let output_wrap _ _ exn = `Output_parse_error (Errors.Oxcaml's, exn)

let (let*) = Result.bind

type ast_source =
  | Input
  | Stylo

let dump_ast (type a) (input : a input) ~src (ast : a) =
  let fname =
    input.fname ^
    match src with
    | Input -> ".input-tree"
    | Stylo -> ".output-tree"
  in
  Debug.dump_to_file ~or_:() fname Oxcaml_frontend.Printast.(fun ppf ->
    match input.kind with
    | Impl -> implementation ppf ast
    | Intf -> interface ppf ast
  )

let dump_out output =
  Debug.dump_to_file output.fname
    (fun ppf -> Format.pp_print_string ppf output.source)

let check_same_ast (type a) (input_ast : a) (output : a input) =
  let input_ast =
    clean ~erase_jane_syntax:!Config.erase_jane_syntax output.kind input_ast
  in
  let* output_ast =
    let output = { output with fname = output.fname ^ ".out" } in
    parse output output_wrap
    |> Result.map_error (fun err -> dump_out ~or_:() output; err)
  in
  let output_ast = clean ~erase_jane_syntax:false output.kind output_ast in
  if input_ast = output_ast
  then Ok ()
  else (
    dump_ast output ~src:Input input_ast;
    dump_ast output ~src:Stylo output_ast;
    Error (`Ast_changed (Errors.Oxcaml's, output.fname))
  )

let parse i = parse i input_wrap
