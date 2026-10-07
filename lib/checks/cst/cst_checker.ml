open Ocaml_syntax
open Checks
open Parsetree

let sort_attributes : attributes -> attributes = function
  | No_attributes -> No_attributes
  | Attributes a ->
    Attributes { a with attributes = List.sort compare a.attributes }
;;

let cleaner =
  object
    inherit [unit] Traversals_helpers.map_with_context

    inherit [unit] Traversals.map_with_context as super

    method position () x = x

    (* Actual cleanup. *)

    method! location () _ = Location.none

    method! doc () _ =
      (* By turning each docstring into an empty string, we still check that
         docstrings are attached at the same place, while ignoring the actual
         content (which will eventually have been reformated). *)
      Docstring { id = -1; text = ""; start_pos = Lexing.dummy_pos }

    (* TODO: rename [Tokens.seq] to [Tokens.t], so the method gets the name
       [tokens] *)
    method! seq () _tokens = []

    method! attributes () attrs =
      super#attributes () ((* FIXME: why? *) sort_attributes attrs)
  end
;;

(* method! visit_expression env exp = let [{pexp_desc; pexp_attributes; _}] =
   exp in match pexp_desc with (* convert [(c1; c2); c3] to [c1; (c2; c3)] *) |
   Pexp_sequence ([{pexp_desc= Pexp_sequence (e1, e2); pexp_attributes= []; _}],
   e3) -> (* FIXME: what about ext_attrs?! *) self#visit_expression env
   (Exp.sequence e1 (Exp.sequence ~attrs:pexp_attributes e2 e3)) | _ ->
   super#visit_expression env exp

   method! visit_pattern env pat = let
   [{ppat_desc; ppat_loc= loc1; ppat_attributes= attrs1; _}] = pat in (*
   normalize nested or patterns *) match ppat_desc with | Ppat_or ( pat1 ,
   [{ ppat_desc= Ppat_or (pat2, pat3) ; ppat_loc= loc2 ; ppat_attributes= attrs2 ; _ }] )
   -> self#visit_pattern env (Pat.or_ ~loc:loc1 ~attrs:attrs1 (Pat.or_ ~loc:loc2
   ~attrs:attrs2 pat1 pat2) pat3) | _ -> super#visit_pattern env pat *)

type options = Parse.Options.t
type ast = Cst.t

let parse options (input : Source.t) wrap_exn : (ast, _) result =
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
       | Impl -> Structure (Parse.implementation options lb)
       | Intf -> Signature (Parse.interface options lb))
  with
  | exn -> Error (wrap_exn lb.lex_start_p lb.lex_curr_p exn)
;;

let clean : Cst.t -> Cst.t = function
  | Structure str -> Structure (cleaner#structure () str)
  | Signature sg -> Signature (cleaner#signature () sg)
;;

let input_wrap startp endp exn = Errors.Stylo's, startp, endp, exn
let output_wrap _ _ exn = `Output_parse_error (Errors.Stylo's, exn)
let ( let* ) = Result.bind

let check_same_ast options input_cst (output : Source.t) =
  let input_cst = clean input_cst in
  let output = { output with fname = output.fname ^ ".out" } in
  let* output_cst = parse options output output_wrap in
  let output_cst = clean output_cst in
  if input_cst = output_cst
  then Ok ()
  else Error (`Ast_changed (Errors.Stylo's, output.fname))
;;

let parse options i = parse options i input_wrap
