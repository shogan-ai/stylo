open Ocaml_syntax
open Parsetree

let from_docstring attr =
  match attr.attr_name.txt with
  | "ocaml" :: ("doc" | "text") :: [] -> true
  | _ -> false

let add_pipe_if_missing ?(mk_optional=false) tokens =
  let open Tokens in
  let desc = Token (BAR, mk_optional) in
  match Tokens.Seq.split ~on:BAR tokens with
  | tokens, [] ->
    (* no pipe was present, insert one at the front *)
    { desc; pos = (List.hd tokens).pos } :: tokens
  | leading_comments, pipe :: following_tokens ->
    (* ensure the pipe is optional/mandatory as requested *)
    leading_comments @ { pipe with desc } :: following_tokens

let normalizer = object
  inherit [Context.parent] Traversals_helpers.map_with_context
  inherit [Context.parent] Traversals.map_with_context as super

  method position _ p = p

  method! attribute parent attr =
    if from_docstring attr
    then attr
    else super#attribute parent attr

  method! structure_item _parent str_item =
    let parent_for_recursive_calls = Context.Str_item str_item.pstr_desc in
    super#structure_item parent_for_recursive_calls str_item

  method! constructor_arguments env ca =
    let ca = Semicolon.constructor_arguments ca in
    super#constructor_arguments env ca

  method! case env case =
    let pc_tokens = add_pipe_if_missing case.pc_tokens in
    super#case env { case with pc_tokens }

  method! pattern = Pattern.map ~recur:super#pattern
  method! pattern_desc = Pattern.map_desc ~recur:super#pattern_desc
  method! expression = Expression.map ~recur:super#expression
  method! expression_desc = Expression.map_desc ~recur:super#expression_desc

  method! type_kind env tk =
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
    super#type_kind env tk

  method! structure env str =
    Semicolon.normalize_struct_semisemi str
    |> super#structure env

  method! value_binding _ vb =
    let vb = Docstring_placement.value_binding vb in
    let parent_for_recursive_calls = Context.Value_binding vb in
    super#value_binding parent_for_recursive_calls vb

  method! value_description env vd =
    Docstring_placement.value_description vd
    |> super#value_description env

  method! argument_desc f _ arg =
    let parent_for_recursive_calls = Context.Fun_param_or_arg in
    super#argument_desc f parent_for_recursive_calls arg

  method! type_declaration env td =
    Docstring_placement.type_declaration td
    |> super#type_declaration env

  method! type_extension env ext =
    Docstring_placement.type_extension ext
    |> super#type_extension env

  method! type_exception env exn =
    Docstring_placement.type_exception exn
    |> super#type_exception env

  method! class_type_field env cf =
    Docstring_placement.class_type_field cf
    |> super#class_type_field env

  method! class_description env cd =
    Docstring_placement.class_description cd
    |> super#class_description env

  method! class_declaration env cd =
    Docstring_placement.class_declaration cd
    |> super#class_declaration env

  method! class_type_declaration env cd =
    Docstring_placement.class_type_declaration cd
    |> super#class_type_declaration env

  method! class_field env x =
    Docstring_placement.class_field x
    |> super#class_field env

  method! module_declaration env x =
    Docstring_placement.module_declaration x
    |> super#module_declaration env

  method! module_substitution env x =
    Docstring_placement.module_substitution x
    |> super#module_substitution env

  method! module_type_declaration env x =
    Docstring_placement.module_type_declaration x
    |> super#module_type_declaration env

  method! open_description env x =
    Docstring_placement.open_description x
    |> super#open_description env

  method! open_declaration env x =
    Docstring_placement.open_declaration x
    |> super#open_declaration env

  method! include_description env x =
    Docstring_placement.include_description x
    |> super#include_description env

  method! include_declaration env x =
    Docstring_placement.include_declaration x
    |> super#include_declaration env

  method! module_binding env x =
    Docstring_placement.module_binding x
    |> super#module_binding env
end

let structure = normalizer#structure Other
let signature = normalizer#signature Other
