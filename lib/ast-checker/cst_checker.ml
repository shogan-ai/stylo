open Ocaml_syntax
open Parsetree


let cleaner =
  object
    inherit [unit] Traversals_helpers.map_with_context
    inherit [unit] Traversals.map_with_context as super

    method position () x = x

    (* Actual cleanup. *)

    method! location () _ = Location.none

    (* TODO: rename [Tokens.seq] to [Tokens.t], so the method gets the name
       [tokens] *)
    method! seq () _tokens = []

    method! attributes () attrs =
      super#attributes () ((* FIXME: why? *) sort_attributes attrs)
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

let output_wrap exn = `Output_parse_error (Errors.Stylo's, exn)

let (let*) = Result.bind

let check_same_ast (type a) (input_cst : a) (output : a input) =
  let input_cst = clean output.kind input_cst in
  let output = { output with fname = output.fname ^ ".out" } in
  let* output_cst = parse output output_wrap in
  let output_cst = clean output.kind output_cst in
  if input_cst = output_cst
  then Ok ()
  else Error (`Ast_changed (Errors.Stylo's, output.fname))
