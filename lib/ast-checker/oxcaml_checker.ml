open Oxcaml_frontend
open Parsetree

let sort_attributes : attributes -> attributes = List.sort compare

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
          (* By turning each docstring into an empty string, we still check that
             docstrings are attached at the same place, while ignoring the
             actual content (which will eventually have been reformated). *)
          let open Ast_helper in
          let loc = Location.none in
          let e_string = Exp.constant ~loc @@ Const.string ~loc "" in
          PStr [Str.eval ~loc e_string]
      in
      super#attribute () { attr with attr_payload }

    method! attributes () attrs =
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
    Error (wrap_exn exn)

let clean (type a) (kind : a input_kind) (ast : a) : a =
  match kind with
  | Impl -> cleaner#structure () ast
  | Intf -> cleaner#signature () ast

let input_wrap exn = `Input_parse_error (Errors.Oxcaml's, exn)
let output_wrap exn = `Output_parse_error (Errors.Oxcaml's, exn)

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
  let input_ast = clean output.kind input_ast in
  let* output_ast =
    let output = { output with fname = output.fname ^ ".out" } in
    parse output output_wrap
    |> Result.map_error (fun err -> dump_out ~or_:() output; err)
  in
  let output_ast = clean output.kind output_ast in
  if input_ast = output_ast
  then Ok ()
  else (
    dump_ast output ~src:Input input_ast;
    dump_ast output ~src:Stylo output_ast;
    Error (`Ast_changed (Errors.Oxcaml's, output.fname))
  )

let parse i = parse i input_wrap
