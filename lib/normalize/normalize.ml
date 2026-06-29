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

class style_normalizer = object
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

class eraser = object
  inherit style_normalizer as super

  method! expression ctxt expr =
    Erase_jane_syntax.expression expr
    |> super#expression ctxt

  method! pattern ctxt pat =
    Erase_jane_syntax.pattern pat
    |> super#pattern ctxt

  method! argument f ctxt arg =
    Erase_jane_syntax.Argument.generic_erase arg
    |> super#argument f ctxt

  (* TODO: dedicated helper funs in [Erase_jane_syntax] for the next 3. *)
  method! function_param_desc ctxt d =
    let d =
      match d with
      | Pparam_val a ->
        Pparam_val (Erase_jane_syntax.Argument.erase_function_param a)
      | _ -> d
    in
    super#function_param_desc ctxt d

  method! class_infos f ctxt ci =
    super#class_infos f ctxt
      { ci with
        pci_value_params =
          List.map Erase_jane_syntax.Argument.erase_function_param
            ci.pci_value_params }

  method! class_expr_desc ctxt ced =
    let ced =
      match ced with
      | Pcl_fun (ps, ce) ->
        let ps = List.map Erase_jane_syntax.Argument.erase_function_param ps in
        Pcl_fun (ps, ce)
      | _ -> ced
    in
    super#class_expr_desc ctxt ced

  method! value_binding ctxt vb =
    Erase_jane_syntax.value_binding vb
    |> super#value_binding ctxt

  method! arrow_arg ctxt aa =
    Erase_jane_syntax.Arrow_arg.erase aa
    |> super#arrow_arg ctxt

  method! core_type ctxt ct =
    Erase_jane_syntax.core_type ct
    |> super#core_type ctxt

  method! bound_ty_var ctxt bv =
    Erase_jane_syntax.bound_ty_var bv
    |> super#bound_ty_var ctxt

  method! ptype_param ctxt p =
    Erase_jane_syntax.ptype_param p
    |> super#ptype_param ctxt

  method! type_declaration ctxt td =
    Erase_jane_syntax.Type_declaration.erase td
    |> super#type_declaration ctxt

  method! value_description ctxt vd =
    Erase_jane_syntax.value_description vd
    |> super#value_description ctxt

  method! constructor_argument ctxt ca =
    Erase_jane_syntax.Constructor_argument.erase ca
    |> super#constructor_argument ctxt

  method! label_declaration ctxt lbl =
    Erase_jane_syntax.Label_declaration.erase lbl
    |> super#label_declaration ctxt

  method! signature ctxt sg =
    Erase_jane_syntax.signature sg
    |> super#signature ctxt

  method! structure ctxt st =
    Erase_jane_syntax.structure st
    |> super#structure ctxt
end

let style_normalizer = new style_normalizer
let eraser = new eraser

let normalizer () =
  if !Config.erase_jane_syntax
  then eraser
  else style_normalizer

let structure st = (normalizer ())#structure Other st
let signature sg = (normalizer ())#signature Other sg
