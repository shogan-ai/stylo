open Ocaml_syntax
open Parsetree

let from_docstring attr =
  match attr.attr_name.txt with
  | [ "ocaml"; ("doc" | "text") ] -> true
  | _ -> false
;;

open Ast_mapper

let super = default_mapper

let map_attribute mapper env attr =
  if from_docstring attr then attr else super.attribute mapper env attr
;;

let map_structure_item mapper _parent str_item =
  let parent_for_recursive_calls = Context.Str_item str_item.pstr_desc in
  super.structure_item mapper parent_for_recursive_calls str_item
;;

let map_constructor_arguments mapper env ca =
  let ca = Semicolon.constructor_arguments ca in
  super.constructor_arguments mapper env ca
;;

let add_pipe_if_missing ?(mk_optional = false) tokens =
  let open Tokens in
  let desc = Token (BAR, mk_optional) in
  match Utils.split ~on:BAR tokens with
  | tokens, [] ->
    (* no pipe was present, insert one at the front *)
    { desc; pos = (List.hd tokens).pos } :: tokens
  | leading_comments, pipe :: following_tokens ->
    (* ensure the pipe is optional/mandatory as requested *)
    leading_comments @ ({ pipe with desc } :: following_tokens)
;;

let map_case mapper env case =
  let pc_tokens = add_pipe_if_missing case.pc_tokens in
  super.case mapper env { case with pc_tokens }
;;

let map_type_kind mapper env tk =
  let tk = Semicolon.type_kind_no_trailing tk in
  let tk =
    match tk with
    | Ptype_variant [ cd ] ->
      let pcd_tokens = add_pipe_if_missing ~mk_optional:true cd.pcd_tokens in
      Ptype_variant [ { cd with pcd_tokens } ]
    | Ptype_variant (cd :: cds) ->
      let pcd_tokens = add_pipe_if_missing cd.pcd_tokens in
      Ptype_variant ({ cd with pcd_tokens } :: cds)
    | _ -> tk
  in
  super.type_kind mapper env tk
;;

let map_structure mapper env str =
  Semicolon.normalize_struct_semisemi str |> super.structure mapper env
;;

let default_vb_passing_context mapper _ vb =
  let parent_for_recursive_calls = Context.Value_binding vb in
  super.value_binding mapper parent_for_recursive_calls vb
;;

let default_arg_passing_context mapper f _ arg =
  let parent_for_recursive_calls = Context.Fun_param_or_arg in
  super.argument_desc mapper f parent_for_recursive_calls arg
;;

let normalizer =
  { super with
    attribute = map_attribute
  ; pattern = Pattern.map
  ; pattern_desc = Pattern.map_desc
  ; expression = Expression.map
  ; expression_desc = Expression.map_desc
  ; argument_desc = default_arg_passing_context
  ; value_binding = default_vb_passing_context
  ; case = map_case
  ; constructor_arguments = map_constructor_arguments
  ; type_kind = map_type_kind
  ; structure_item = map_structure_item
  ; structure = map_structure
  }
;;

let structure = normalizer.structure normalizer Other
let signature = normalizer.signature normalizer Other
