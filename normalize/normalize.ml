open Ocaml_syntax
open Parsetree

let from_docstring attr =
  match attr.attr_name.txt with
  | [ "ocaml"; ("doc" | "text") ] -> true
  | _ -> false
;;

let normalizer =
  object(self)
    inherit [Context.parent] Ast_traverse.map_with_context as super

    method! attribute env attr =
      if from_docstring attr then attr else super#attribute env attr

    method! pattern = Pattern.map ~recur:super#pattern

    method! pattern_desc = Pattern.map_desc ~recur:super#pattern_desc

    method! expression = Expression.map ~recur:super#expression

    method! expression_desc = Expression.map_desc ~recur:super#expression_desc

    method! structure_item _parent str_item =
      let parent_for_recursive_calls = Context.Str_item str_item.pstr_desc in
      super#structure_item parent_for_recursive_calls str_item

    method! constructor_arguments env ca =
      let ca = Semicolon.constructor_arguments ca in
      super#constructor_arguments env ca

    method private add_pipe_if_missing ?(mk_optional = false) tokens =
      let open Tokens in
      let desc = Token (BAR, mk_optional) in
      match Utils.split ~on:BAR tokens with
      | tokens, [] ->
        (* no pipe was present, insert one at the front *)
        { desc; pos = (List.hd tokens).pos } :: tokens
      | leading_comments, pipe :: following_tokens ->
        (* ensure the pipe is optional/mandatory as requested *)
        leading_comments @ ({ pipe with desc } :: following_tokens)

    method! case env case =
      let pc_tokens = self#add_pipe_if_missing case.pc_tokens in
      super#case env { case with pc_tokens }

    method! type_kind env tk =
      let tk = Semicolon.type_kind_no_trailing tk in
      let tk =
        match tk with
        | Ptype_variant [ cd ] ->
          let pcd_tokens =
            self#add_pipe_if_missing ~mk_optional:true cd.pcd_tokens
          in
          Ptype_variant [ { cd with pcd_tokens } ]
        | Ptype_variant (cd :: cds) ->
          let pcd_tokens = self#add_pipe_if_missing cd.pcd_tokens in
          Ptype_variant ({ cd with pcd_tokens } :: cds)
        | _ -> tk
      in
      super#type_kind env tk

    method! structure env str =
      Semicolon.normalize_struct_semisemi str |> super#structure env

    method! value_binding _ vb =
      let parent_for_recursive_calls = Context.Value_binding vb in
      super#value_binding parent_for_recursive_calls vb

    method! argument_desc f _ arg =
      let parent_for_recursive_calls = Context.Fun_param_or_arg in
      super#argument_desc f parent_for_recursive_calls arg
  end
;;

let structure = normalizer#structure Other
let signature = normalizer#signature Other
