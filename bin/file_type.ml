type _ t =
  | Implementation : Ocaml_syntax.Parsetree.structure t
  | Interface : Ocaml_syntax.Parsetree.signature t
  | Use_file : Ocaml_syntax.Parsetree.use_file t

type packed = P : _ t -> packed [@@unboxed]

let default = P Implementation

let of_filename fn =
  if Filename.check_suffix fn ".ml"
  then P Implementation
  else if Filename.check_suffix fn ".mli"
  then P Interface
  else if Filename.check_suffix fn ".mlt"
  then P Use_file
  else default
;;

let check_same_ast (type a) (t : a t) =
  let open Ast_checker.Oxcaml_checker.Check_same_ast in
  match t with
  | Implementation -> implementation
  | Interface -> interface
  | Use_file -> use_file
;;

let parse (type a) (t : a t) =
  let open Ocaml_syntax.Parse in
  match t with
  | Implementation -> (implementation : _ -> a)
  | Interface -> interface
  | Use_file -> use_file
;;

let normalize (type a) (t : a t) =
  let open Normalize in
  match t with
  | Implementation -> (structure : a -> a)
  | Interface -> signature
  | Use_file -> use_file
;;

let pp (type a) (t : a t) =
  let open Print in
  match t with
  | Implementation -> (Structure.pp_implementation : a -> _)
  | Interface -> Signature.pp_interface
  | Use_file -> Toplevel_phrase.pp_use_file
;;

let tokens_of_tree (type a) (t : a t) =
  let open Ocaml_syntax.Tokens_of_tree in
  match t with
  | Implementation -> (structure : a -> _)
  | Interface -> signature
  | Use_file -> use_file
;;
