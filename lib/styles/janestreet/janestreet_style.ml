(* A style aiming to match ocamlformat's "janestreet" profile. *)

open Ocaml_syntax

type options = { normalize : Normalize.Options.t }

let normalize options : Cst.t -> Cst.t = function
  | Structure str -> Structure (Normalize.structure options.normalize str)
  | Signature sg -> Signature (Normalize.signature options.normalize sg)
;;

(* tying the knot between code and docstring's printer. *)
let set_code_block_hook format_code_block =
  Print.Doc.Odoc.process_ocaml_block := format_code_block
;;

let doc_of_cst _ ~format_code_block : Cst.t -> Document.t =
  set_code_block_hook format_code_block;
  function
  | Structure str -> Print.Structure.pp_implementation str
  | Signature sg -> Print.Signature.pp_interface sg
;;

let render_comment _ ~format_code_block =
  set_code_block_hook format_code_block;
  Print.Doc.as_odoc_markup_if_no_warnings
    ~id:(-1) (* regular comments / unattached docstrings do not have ids *)
    ~kind:`Regular_comment
;;
