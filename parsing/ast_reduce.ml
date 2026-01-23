open Parsetree
open Asttypes

type ('reducer_env, 'result) reducer =
  { __init : 'result
  ; __combine : 'result -> 'result -> 'result
  ; class_expr_desc :
      ('reducer_env, 'result) reducer
      -> 'reducer_env
      -> class_expr_desc
      -> 'result
  ; modes : ('reducer_env, 'result) reducer -> 'reducer_env -> modes -> 'result
  ; package_type :
      ('reducer_env, 'result) reducer
      -> 'reducer_env
      -> package_type
      -> 'result
  ; class_signature :
      ('reducer_env, 'result) reducer
      -> 'reducer_env
      -> class_signature
      -> 'result
  ; tokens_comment :
      ('reducer_env, 'result) reducer
      -> 'reducer_env
      -> Tokens.comment
      -> 'result
  ; directive_argument :
      ('reducer_env, 'result) reducer
      -> 'reducer_env
      -> directive_argument
      -> 'result
  ; pattern :
      ('reducer_env, 'result) reducer -> 'reducer_env -> pattern -> 'result
  ; constructor_declaration :
      ('reducer_env, 'result) reducer
      -> 'reducer_env
      -> constructor_declaration
      -> 'result
  ; module_expr_desc :
      ('reducer_env, 'result) reducer
      -> 'reducer_env
      -> module_expr_desc
      -> 'result
  ; structure_item :
      ('reducer_env, 'result) reducer
      -> 'reducer_env
      -> structure_item
      -> 'result
  ; override_flag :
      ('reducer_env, 'result) reducer
      -> 'reducer_env
      -> override_flag
      -> 'result
  ; extension :
      ('reducer_env, 'result) reducer -> 'reducer_env -> extension -> 'result
  ; class_description :
      ('reducer_env, 'result) reducer
      -> 'reducer_env
      -> class_description
      -> 'result
  ; core_type :
      ('reducer_env, 'result) reducer -> 'reducer_env -> core_type -> 'result
  ; function_constraint :
      ('reducer_env, 'result) reducer
      -> 'reducer_env
      -> function_constraint
      -> 'result
  ; extension_constructor_kind :
      ('reducer_env, 'result) reducer
      -> 'reducer_env
      -> extension_constructor_kind
      -> 'result
  ; structure :
      ('reducer_env, 'result) reducer -> 'reducer_env -> structure -> 'result
  ; toplevel_directive :
      ('reducer_env, 'result) reducer
      -> 'reducer_env
      -> toplevel_directive
      -> 'result
  ; module_declaration_body :
      ('reducer_env, 'result) reducer
      -> 'reducer_env
      -> module_declaration_body
      -> 'result
  ; module_declaration :
      ('reducer_env, 'result) reducer
      -> 'reducer_env
      -> module_declaration
      -> 'result
  ; variance :
      ('reducer_env, 'result) reducer -> 'reducer_env -> variance -> 'result
  ; signature_item_desc :
      ('reducer_env, 'result) reducer
      -> 'reducer_env
      -> signature_item_desc
      -> 'result
  ; binding_op :
      ('reducer_env, 'result) reducer -> 'reducer_env -> binding_op -> 'result
  ; record_field :
      'a
      .  ('reducer_env, 'result) reducer
      -> ('reducer_env -> 'a -> 'result)
      -> 'reducer_env
      -> 'a record_field
      -> 'result
  ; arg_label :
      ('reducer_env, 'result) reducer -> 'reducer_env -> arg_label -> 'result
  ; comprehension_clause :
      ('reducer_env, 'result) reducer
      -> 'reducer_env
      -> comprehension_clause
      -> 'result
  ; class_field_desc :
      ('reducer_env, 'result) reducer
      -> 'reducer_env
      -> class_field_desc
      -> 'result
  ; longident_str_or_op :
      ('reducer_env, 'result) reducer
      -> 'reducer_env
      -> Longident.str_or_op
      -> 'result
  ; core_type_desc :
      ('reducer_env, 'result) reducer
      -> 'reducer_env
      -> core_type_desc
      -> 'result
  ; ptype_param :
      ('reducer_env, 'result) reducer -> 'reducer_env -> ptype_param -> 'result
  ; virtual_flag :
      ('reducer_env, 'result) reducer
      -> 'reducer_env
      -> virtual_flag
      -> 'result
  ; module_type_desc :
      ('reducer_env, 'result) reducer
      -> 'reducer_env
      -> module_type_desc
      -> 'result
  ; row_field :
      ('reducer_env, 'result) reducer -> 'reducer_env -> row_field -> 'result
  ; class_type :
      ('reducer_env, 'result) reducer -> 'reducer_env -> class_type -> 'result
  ; include_description :
      ('reducer_env, 'result) reducer
      -> 'reducer_env
      -> include_description
      -> 'result
  ; modality :
      ('reducer_env, 'result) reducer -> 'reducer_env -> modality -> 'result
  ; object_field :
      ('reducer_env, 'result) reducer
      -> 'reducer_env
      -> object_field
      -> 'result
  ; structure_item_desc :
      ('reducer_env, 'result) reducer
      -> 'reducer_env
      -> structure_item_desc
      -> 'result
  ; row_field_desc :
      ('reducer_env, 'result) reducer
      -> 'reducer_env
      -> row_field_desc
      -> 'result
  ; type_exception :
      ('reducer_env, 'result) reducer
      -> 'reducer_env
      -> type_exception
      -> 'result
  ; mutable_flag :
      ('reducer_env, 'result) reducer
      -> 'reducer_env
      -> mutable_flag
      -> 'result
  ; direction_flag :
      ('reducer_env, 'result) reducer
      -> 'reducer_env
      -> direction_flag
      -> 'result
  ; block_access :
      ('reducer_env, 'result) reducer
      -> 'reducer_env
      -> block_access
      -> 'result
  ; constructor_argument :
      ('reducer_env, 'result) reducer
      -> 'reducer_env
      -> constructor_argument
      -> 'result
  ; class_expr :
      ('reducer_env, 'result) reducer -> 'reducer_env -> class_expr -> 'result
  ; label_declaration :
      ('reducer_env, 'result) reducer
      -> 'reducer_env
      -> label_declaration
      -> 'result
  ; signature :
      ('reducer_env, 'result) reducer -> 'reducer_env -> signature -> 'result
  ; tokens_seq :
      ('reducer_env, 'result) reducer -> 'reducer_env -> Tokens.seq -> 'result
  ; class_field_kind :
      ('reducer_env, 'result) reducer
      -> 'reducer_env
      -> class_field_kind
      -> 'result
  ; toplevel_extension :
      ('reducer_env, 'result) reducer
      -> 'reducer_env
      -> toplevel_extension
      -> 'result
  ; ptype_constraint :
      ('reducer_env, 'result) reducer
      -> 'reducer_env
      -> ptype_constraint
      -> 'result
  ; type_constraint :
      ('reducer_env, 'result) reducer
      -> 'reducer_env
      -> type_constraint
      -> 'result
  ; comprehension_clause_binding :
      ('reducer_env, 'result) reducer
      -> 'reducer_env
      -> comprehension_clause_binding
      -> 'result
  ; tokens_elt :
      ('reducer_env, 'result) reducer -> 'reducer_env -> Tokens.elt -> 'result
  ; object_field_desc :
      ('reducer_env, 'result) reducer
      -> 'reducer_env
      -> object_field_desc
      -> 'result
  ; expression_desc :
      ('reducer_env, 'result) reducer
      -> 'reducer_env
      -> expression_desc
      -> 'result
  ; modalities :
      ('reducer_env, 'result) reducer -> 'reducer_env -> modalities -> 'result
  ; class_infos :
      'a
      .  ('reducer_env, 'result) reducer
      -> ('reducer_env -> 'a -> 'result)
      -> 'reducer_env
      -> 'a class_infos
      -> 'result
  ; argument :
      'a
      .  ('reducer_env, 'result) reducer
      -> ('reducer_env -> 'a -> 'result)
      -> 'reducer_env
      -> 'a argument
      -> 'result
  ; module_substitution :
      ('reducer_env, 'result) reducer
      -> 'reducer_env
      -> module_substitution
      -> 'result
  ; label : ('reducer_env, 'result) reducer -> 'reducer_env -> label -> 'result
  ; open_infos :
      'a
      .  ('reducer_env, 'result) reducer
      -> ('reducer_env -> 'a -> 'result)
      -> 'reducer_env
      -> 'a open_infos
      -> 'result
  ; constructor_arguments :
      ('reducer_env, 'result) reducer
      -> 'reducer_env
      -> constructor_arguments
      -> 'result
  ; module_type :
      ('reducer_env, 'result) reducer -> 'reducer_env -> module_type -> 'result
  ; injectivity :
      ('reducer_env, 'result) reducer -> 'reducer_env -> injectivity -> 'result
  ; mode : ('reducer_env, 'result) reducer -> 'reducer_env -> mode -> 'result
  ; payload :
      ('reducer_env, 'result) reducer -> 'reducer_env -> payload -> 'result
  ; value_binding :
      ('reducer_env, 'result) reducer
      -> 'reducer_env
      -> value_binding
      -> 'result
  ; unboxed_access :
      ('reducer_env, 'result) reducer
      -> 'reducer_env
      -> unboxed_access
      -> 'result
  ; functor_parameter :
      ('reducer_env, 'result) reducer
      -> 'reducer_env
      -> functor_parameter
      -> 'result
  ; longident_lid_desc :
      ('reducer_env, 'result) reducer
      -> 'reducer_env
      -> Longident.lid_desc
      -> 'result
  ; jkind_annotation :
      ('reducer_env, 'result) reducer
      -> 'reducer_env
      -> jkind_annotation
      -> 'result
  ; longident :
      ('reducer_env, 'result) reducer -> 'reducer_env -> Longident.t -> 'result
  ; constant :
      ('reducer_env, 'result) reducer -> 'reducer_env -> constant -> 'result
  ; class_type_desc :
      ('reducer_env, 'result) reducer
      -> 'reducer_env
      -> class_type_desc
      -> 'result
  ; rec_flag :
      ('reducer_env, 'result) reducer -> 'reducer_env -> rec_flag -> 'result
  ; longident_dotop_delims :
      ('reducer_env, 'result) reducer
      -> 'reducer_env
      -> Longident.dotop_delims
      -> 'result
  ; module_type_declaration :
      ('reducer_env, 'result) reducer
      -> 'reducer_env
      -> module_type_declaration
      -> 'result
  ; tokens_desc :
      ('reducer_env, 'result) reducer -> 'reducer_env -> Tokens.desc -> 'result
  ; class_type_field :
      ('reducer_env, 'result) reducer
      -> 'reducer_env
      -> class_type_field
      -> 'result
  ; letop : ('reducer_env, 'result) reducer -> 'reducer_env -> letop -> 'result
  ; location :
      ('reducer_env, 'result) reducer -> 'reducer_env -> Location.t -> 'result
  ; function_param :
      ('reducer_env, 'result) reducer
      -> 'reducer_env
      -> function_param
      -> 'result
  ; ptype_params :
      ('reducer_env, 'result) reducer
      -> 'reducer_env
      -> ptype_params
      -> 'result
  ; open_declaration :
      ('reducer_env, 'result) reducer
      -> 'reducer_env
      -> open_declaration
      -> 'result
  ; jkind_annotation_desc :
      ('reducer_env, 'result) reducer
      -> 'reducer_env
      -> jkind_annotation_desc
      -> 'result
  ; toplevel_phrase :
      ('reducer_env, 'result) reducer
      -> 'reducer_env
      -> toplevel_phrase
      -> 'result
  ; arrow_arg :
      ('reducer_env, 'result) reducer -> 'reducer_env -> arrow_arg -> 'result
  ; value_constraint :
      ('reducer_env, 'result) reducer
      -> 'reducer_env
      -> value_constraint
      -> 'result
  ; paren_kind :
      ('reducer_env, 'result) reducer -> 'reducer_env -> paren_kind -> 'result
  ; case : ('reducer_env, 'result) reducer -> 'reducer_env -> case -> 'result
  ; type_extension :
      ('reducer_env, 'result) reducer
      -> 'reducer_env
      -> type_extension
      -> 'result
  ; signature_item :
      ('reducer_env, 'result) reducer
      -> 'reducer_env
      -> signature_item
      -> 'result
  ; closed_flag :
      ('reducer_env, 'result) reducer -> 'reducer_env -> closed_flag -> 'result
  ; type_kind :
      ('reducer_env, 'result) reducer -> 'reducer_env -> type_kind -> 'result
  ; include_infos :
      'a
      .  ('reducer_env, 'result) reducer
      -> ('reducer_env -> 'a -> 'result)
      -> 'reducer_env
      -> 'a include_infos
      -> 'result
  ; include_kind :
      ('reducer_env, 'result) reducer
      -> 'reducer_env
      -> include_kind
      -> 'result
  ; ext_attribute :
      ('reducer_env, 'result) reducer
      -> 'reducer_env
      -> ext_attribute
      -> 'result
  ; value_description :
      ('reducer_env, 'result) reducer
      -> 'reducer_env
      -> value_description
      -> 'result
  ; function_body :
      ('reducer_env, 'result) reducer
      -> 'reducer_env
      -> function_body
      -> 'result
  ; class_field :
      ('reducer_env, 'result) reducer -> 'reducer_env -> class_field -> 'result
  ; include_declaration :
      ('reducer_env, 'result) reducer
      -> 'reducer_env
      -> include_declaration
      -> 'result
  ; attributes :
      ('reducer_env, 'result) reducer -> 'reducer_env -> attributes -> 'result
  ; class_type_declaration :
      ('reducer_env, 'result) reducer
      -> 'reducer_env
      -> class_type_declaration
      -> 'result
  ; open_description :
      ('reducer_env, 'result) reducer
      -> 'reducer_env
      -> open_description
      -> 'result
  ; pattern_desc :
      ('reducer_env, 'result) reducer
      -> 'reducer_env
      -> pattern_desc
      -> 'result
  ; tokens_attachment :
      ('reducer_env, 'result) reducer
      -> 'reducer_env
      -> Tokens.attachment
      -> 'result
  ; module_expr :
      ('reducer_env, 'result) reducer -> 'reducer_env -> module_expr -> 'result
  ; module_binding :
      ('reducer_env, 'result) reducer
      -> 'reducer_env
      -> module_binding
      -> 'result
  ; location_loc :
      'a
      .  ('reducer_env, 'result) reducer
      -> ('reducer_env -> 'a -> 'result)
      -> 'reducer_env
      -> 'a Location.loc
      -> 'result
  ; argument_desc :
      'a
      .  ('reducer_env, 'result) reducer
      -> ('reducer_env -> 'a -> 'result)
      -> 'reducer_env
      -> 'a argument_desc
      -> 'result
  ; expression :
      ('reducer_env, 'result) reducer -> 'reducer_env -> expression -> 'result
  ; comprehension :
      ('reducer_env, 'result) reducer
      -> 'reducer_env
      -> comprehension
      -> 'result
  ; type_declaration :
      ('reducer_env, 'result) reducer
      -> 'reducer_env
      -> type_declaration
      -> 'result
  ; function_param_desc :
      ('reducer_env, 'result) reducer
      -> 'reducer_env
      -> function_param_desc
      -> 'result
  ; function_body_desc :
      ('reducer_env, 'result) reducer
      -> 'reducer_env
      -> function_body_desc
      -> 'result
  ; with_constraint :
      ('reducer_env, 'result) reducer
      -> 'reducer_env
      -> with_constraint
      -> 'result
  ; with_constraint_desc :
      ('reducer_env, 'result) reducer
      -> 'reducer_env
      -> with_constraint_desc
      -> 'result
  ; directive_argument_desc :
      ('reducer_env, 'result) reducer
      -> 'reducer_env
      -> directive_argument_desc
      -> 'result
  ; attribute :
      ('reducer_env, 'result) reducer -> 'reducer_env -> attribute -> 'result
  ; comprehension_expression :
      ('reducer_env, 'result) reducer
      -> 'reducer_env
      -> comprehension_expression
      -> 'result
  ; private_flag :
      ('reducer_env, 'result) reducer
      -> 'reducer_env
      -> private_flag
      -> 'result
  ; class_structure :
      ('reducer_env, 'result) reducer
      -> 'reducer_env
      -> class_structure
      -> 'result
  ; comprehension_iterator :
      ('reducer_env, 'result) reducer
      -> 'reducer_env
      -> comprehension_iterator
      -> 'result
  ; index_kind :
      ('reducer_env, 'result) reducer -> 'reducer_env -> index_kind -> 'result
  ; class_declaration :
      ('reducer_env, 'result) reducer
      -> 'reducer_env
      -> class_declaration
      -> 'result
  ; class_type_field_desc :
      ('reducer_env, 'result) reducer
      -> 'reducer_env
      -> class_type_field_desc
      -> 'result
  ; extension_constructor :
      ('reducer_env, 'result) reducer
      -> 'reducer_env
      -> extension_constructor
      -> 'result
  }

open struct
  let ret_init (r : ('env, 'res) reducer) (_ : 'env) _ : 'res = r.__init

  let reduce_option reducer f env = function
    | None -> reducer.__init
    | Some x -> f env x
  ;;

  let reduce_list reducer f env l =
    List.fold_left
      (fun acc x -> reducer.__combine acc (f env x))
      reducer.__init
      l
  ;;

  let reduce_class_expr_desc reducer env class_expr_desc =
    match class_expr_desc with
    | Pcl_constr (_arg_0, _arg_1) ->
      reducer.location_loc reducer (reducer.longident reducer) env _arg_0
      |> fun x ->
      reducer.__combine
        x
        (reduce_list reducer (reducer.core_type reducer) env _arg_1)
    | Pcl_structure _arg_0 -> reducer.class_structure reducer env _arg_0
    | Pcl_fun (_arg_0, _arg_1) ->
      reduce_list
        reducer
        (reducer.argument reducer (reducer.pattern reducer))
        env
        _arg_0
      |> fun x -> reducer.__combine x (reducer.class_expr reducer env _arg_1)
    | Pcl_apply (_arg_0, _arg_1) ->
      reducer.class_expr reducer env _arg_0
      |> fun x ->
      reducer.__combine
        x
        (reduce_list
           reducer
           (reducer.argument reducer (reducer.expression reducer))
           env
           _arg_1)
    | Pcl_let (_arg_0, _arg_1, _arg_2) ->
      reducer.rec_flag reducer env _arg_0
      |> fun x ->
      reducer.__combine
        x
        (reduce_list reducer (reducer.value_binding reducer) env _arg_1)
      |> fun x -> reducer.__combine x (reducer.class_expr reducer env _arg_2)
    | Pcl_constraint (_arg_0, _arg_1) ->
      reducer.class_expr reducer env _arg_0
      |> fun x -> reducer.__combine x (reducer.class_type reducer env _arg_1)
    | Pcl_extension _arg_0 -> reducer.extension reducer env _arg_0
    | Pcl_open (_arg_0, _arg_1) ->
      reducer.open_description reducer env _arg_0
      |> fun x -> reducer.__combine x (reducer.class_expr reducer env _arg_1)
    | Pcl_parens _arg_0 -> reducer.class_expr reducer env _arg_0
  ;;

  let reduce_modes reducer env modes =
    reduce_list
      reducer
      (reducer.location_loc reducer (reducer.mode reducer))
      env
      modes
  ;;

  let reduce_package_type reducer env package_type =
    reducer.module_type reducer env package_type
  ;;

  let reduce_class_signature reducer env class_signature =
    reduce_option
      reducer
      (reducer.core_type reducer)
      env
      class_signature.pcsig_self
    |> fun x ->
    reducer.__combine
      x
      (reduce_list
         reducer
         (reducer.class_type_field reducer)
         env
         class_signature.pcsig_fields)
  ;;

  let reduce_tokens_comment reducer env comment =
    ret_init reducer env comment.Tokens.text
    |> fun x ->
    reducer.__combine
      x
      (reducer.tokens_attachment reducer env comment.Tokens.attachement)
    |> fun x ->
    reducer.__combine
      x
      ((fun reducer _reduce_content -> ret_init reducer)
         reducer
         (ret_init reducer)
         env
         comment.Tokens.explicitely_inserted)
  ;;

  let reduce_directive_argument reducer env directive_argument =
    reducer.directive_argument_desc reducer env directive_argument.pdira_desc
    |> fun x ->
    reducer.__combine
      x
      (reducer.location reducer env directive_argument.pdira_loc)
  ;;

  let reduce_pattern reducer env pattern =
    reducer.ext_attribute reducer env pattern.ppat_ext_attr
    |> fun x ->
    reducer.__combine x (reducer.pattern_desc reducer env pattern.ppat_desc)
    |> fun x ->
    reducer.__combine x (reducer.location reducer env pattern.ppat_loc)
    |> fun x ->
    reducer.__combine x (reducer.attributes reducer env pattern.ppat_attributes)
    |> fun x ->
    reducer.__combine x (reducer.tokens_seq reducer env pattern.ppat_tokens)
  ;;

  let reduce_constructor_declaration reducer env constructor_declaration =
    reducer.location_loc
      reducer
      (reducer.longident_str_or_op reducer)
      env
      constructor_declaration.pcd_name
    |> fun x ->
    reducer.__combine
      x
      (reduce_list
         reducer
         (fun env __arg ->
           let _0, _1 = __arg in
           reducer.location_loc reducer (ret_init reducer) env _0
           |> fun x ->
           reducer.__combine
             x
             (reduce_option reducer (reducer.jkind_annotation reducer) env _1))
         env
         constructor_declaration.pcd_vars)
    |> fun x ->
    reducer.__combine
      x
      (reducer.constructor_arguments
         reducer
         env
         constructor_declaration.pcd_args)
    |> fun x ->
    reducer.__combine
      x
      (reduce_option
         reducer
         (reducer.core_type reducer)
         env
         constructor_declaration.pcd_res)
    |> fun x ->
    reducer.__combine
      x
      (reducer.location reducer env constructor_declaration.pcd_loc)
    |> fun x ->
    reducer.__combine
      x
      (reducer.attributes reducer env constructor_declaration.pcd_attributes)
    |> fun x ->
    reducer.__combine
      x
      (reduce_option
         reducer
         (ret_init reducer)
         env
         constructor_declaration.pcd_doc)
    |> fun x ->
    reducer.__combine
      x
      (reducer.tokens_seq reducer env constructor_declaration.pcd_tokens)
  ;;

  let reduce_module_expr_desc reducer env module_expr_desc =
    match module_expr_desc with
    | Pmod_ident _arg_0 ->
      reducer.location_loc reducer (reducer.longident reducer) env _arg_0
    | Pmod_structure (_arg_0, _arg_1) ->
      reducer.attributes reducer env _arg_0
      |> fun x -> reducer.__combine x (reducer.structure reducer env _arg_1)
    | Pmod_functor (_arg_0, _arg_1, _arg_2) ->
      reducer.attributes reducer env _arg_0
      |> fun x ->
      reducer.__combine
        x
        (reduce_list reducer (reducer.functor_parameter reducer) env _arg_1)
      |> fun x -> reducer.__combine x (reducer.module_expr reducer env _arg_2)
    | Pmod_apply (_arg_0, _arg_1) ->
      reducer.module_expr reducer env _arg_0
      |> fun x -> reducer.__combine x (reducer.module_expr reducer env _arg_1)
    | Pmod_apply_unit _arg_0 -> reducer.module_expr reducer env _arg_0
    | Pmod_constraint (_arg_0, _arg_1, _arg_2) ->
      reducer.module_expr reducer env _arg_0
      |> fun x ->
      reducer.__combine
        x
        (reduce_option reducer (reducer.module_type reducer) env _arg_1)
      |> fun x -> reducer.__combine x (reducer.modes reducer env _arg_2)
    | Pmod_unpack (_arg_0, _arg_1, _arg_2) ->
      reducer.expression reducer env _arg_0
      |> fun x ->
      reducer.__combine
        x
        (reduce_option reducer (reducer.package_type reducer) env _arg_1)
      |> fun x ->
      reducer.__combine
        x
        (reduce_option reducer (reducer.package_type reducer) env _arg_2)
    | Pmod_extension _arg_0 -> reducer.extension reducer env _arg_0
    | Pmod_parens _arg_0 -> reducer.module_expr reducer env _arg_0
  ;;

  let reduce_structure_item reducer env structure_item =
    reducer.structure_item_desc reducer env structure_item.pstr_desc
    |> fun x ->
    reducer.__combine x (reducer.location reducer env structure_item.pstr_loc)
    |> fun x ->
    reducer.__combine
      x
      (reducer.tokens_seq reducer env structure_item.pstr_tokens)
  ;;

  let reduce_override_flag reducer env override_flag =
    match override_flag with
    | Override -> reducer.__init
    | Fresh -> reducer.__init
  ;;

  let reduce_extension reducer env extension =
    let _0, _1, _2 = extension in
    reducer.location_loc reducer (reduce_list reducer (ret_init reducer)) env _0
    |> fun x ->
    reducer.__combine x (reducer.payload reducer env _1)
    |> fun x -> reducer.__combine x (reducer.tokens_seq reducer env _2)
  ;;

  let reduce_class_description reducer env class_description =
    reducer.class_infos
      reducer
      (reducer.class_type reducer)
      env
      class_description
  ;;

  let reduce_core_type reducer env core_type =
    reducer.core_type_desc reducer env core_type.ptyp_desc
    |> fun x ->
    reducer.__combine x (reducer.location reducer env core_type.ptyp_loc)
    |> fun x ->
    reducer.__combine
      x
      (reducer.attributes reducer env core_type.ptyp_attributes)
    |> fun x ->
    reducer.__combine x (reducer.tokens_seq reducer env core_type.ptyp_tokens)
  ;;

  let reduce_function_constraint reducer env function_constraint =
    reducer.modes reducer env function_constraint.ret_mode_annotations
    |> fun x ->
    reducer.__combine
      x
      (reduce_option
         reducer
         (reducer.type_constraint reducer)
         env
         function_constraint.ret_type_constraint)
  ;;

  let reduce_extension_constructor_kind reducer env extension_constructor_kind =
    match extension_constructor_kind with
    | Pext_decl (_arg_0, _arg_1, _arg_2) ->
      reduce_list
        reducer
        (fun env __arg ->
          let _0, _1 = __arg in
          reducer.location_loc reducer (ret_init reducer) env _0
          |> fun x ->
          reducer.__combine
            x
            (reduce_option reducer (reducer.jkind_annotation reducer) env _1))
        env
        _arg_0
      |> fun x ->
      reducer.__combine x (reducer.constructor_arguments reducer env _arg_1)
      |> fun x ->
      reducer.__combine
        x
        (reduce_option reducer (reducer.core_type reducer) env _arg_2)
    | Pext_rebind _arg_0 ->
      reducer.location_loc reducer (reducer.longident reducer) env _arg_0
  ;;

  let reduce_structure reducer env structure =
    let _0, _1 = structure in
    reduce_list reducer (reducer.structure_item reducer) env _0
    |> fun x -> reducer.__combine x (reducer.tokens_seq reducer env _1)
  ;;

  let reduce_toplevel_directive reducer env toplevel_directive =
    reducer.location_loc
      reducer
      (ret_init reducer)
      env
      toplevel_directive.pdir_name
    |> fun x ->
    reducer.__combine
      x
      (reduce_option
         reducer
         (reducer.directive_argument reducer)
         env
         toplevel_directive.pdir_arg)
    |> fun x ->
    reducer.__combine
      x
      (reducer.location reducer env toplevel_directive.pdir_loc)
  ;;

  let reduce_module_declaration_body reducer env module_declaration_body =
    match module_declaration_body with
    | With_params (_arg_0, _arg_1, _arg_2) ->
      reduce_list reducer (reducer.functor_parameter reducer) env _arg_0
      |> fun x ->
      reducer.__combine x (reducer.module_type reducer env _arg_1)
      |> fun x -> reducer.__combine x (reducer.modes reducer env _arg_2)
    | Without_params (_arg_0, _arg_1) ->
      reducer.module_type reducer env _arg_0
      |> fun x -> reducer.__combine x (reducer.modalities reducer env _arg_1)
  ;;

  let reduce_module_declaration reducer env module_declaration =
    reduce_list reducer (ret_init reducer) env module_declaration.pmd_pre_text
    |> fun x ->
    reducer.__combine
      x
      (reduce_option
         reducer
         (ret_init reducer)
         env
         module_declaration.pmd_pre_doc)
    |> fun x ->
    reducer.__combine
      x
      (reducer.ext_attribute reducer env module_declaration.pmd_ext_attrs)
    |> fun x ->
    reducer.__combine
      x
      (let _0, _1 = module_declaration.pmd_name in
       reducer.location_loc
         reducer
         (reduce_option reducer (ret_init reducer))
         env
         _0
       |> fun x -> reducer.__combine x (reducer.modalities reducer env _1))
    |> fun x ->
    reducer.__combine
      x
      (reducer.module_declaration_body reducer env module_declaration.pmd_body)
    |> fun x ->
    reducer.__combine
      x
      (reducer.attributes reducer env module_declaration.pmd_attributes)
    |> fun x ->
    reducer.__combine
      x
      (reduce_option
         reducer
         (ret_init reducer)
         env
         module_declaration.pmd_post_doc)
    |> fun x ->
    reducer.__combine
      x
      (reducer.location reducer env module_declaration.pmd_loc)
    |> fun x ->
    reducer.__combine
      x
      (reducer.tokens_seq reducer env module_declaration.pmd_tokens)
  ;;

  let reduce_variance reducer env variance =
    match variance with
    | Covariant -> reducer.__init
    | Contravariant -> reducer.__init
    | NoVariance -> reducer.__init
  ;;

  let reduce_signature_item_desc reducer env signature_item_desc =
    match signature_item_desc with
    | Psig_value _arg_0 -> reducer.value_description reducer env _arg_0
    | Psig_type (_arg_0, _arg_1) ->
      reducer.rec_flag reducer env _arg_0
      |> fun x ->
      reducer.__combine
        x
        (reduce_list reducer (reducer.type_declaration reducer) env _arg_1)
    | Psig_typesubst _arg_0 ->
      reduce_list reducer (reducer.type_declaration reducer) env _arg_0
    | Psig_typext _arg_0 -> reducer.type_extension reducer env _arg_0
    | Psig_exception _arg_0 -> reducer.type_exception reducer env _arg_0
    | Psig_module _arg_0 -> reducer.module_declaration reducer env _arg_0
    | Psig_modsubst _arg_0 -> reducer.module_substitution reducer env _arg_0
    | Psig_recmodule _arg_0 ->
      reduce_list reducer (reducer.module_declaration reducer) env _arg_0
    | Psig_modtype _arg_0 -> reducer.module_type_declaration reducer env _arg_0
    | Psig_modtypesubst _arg_0 ->
      reducer.module_type_declaration reducer env _arg_0
    | Psig_open _arg_0 -> reducer.open_description reducer env _arg_0
    | Psig_include (_arg_0, _arg_1) ->
      reducer.include_description reducer env _arg_0
      |> fun x -> reducer.__combine x (reducer.modalities reducer env _arg_1)
    | Psig_class _arg_0 ->
      reduce_list reducer (reducer.class_description reducer) env _arg_0
    | Psig_class_type _arg_0 ->
      reduce_list reducer (reducer.class_type_declaration reducer) env _arg_0
    | Psig_attribute _arg_0 -> reducer.attribute reducer env _arg_0
    | Psig_extension _arg_0 -> reducer.toplevel_extension reducer env _arg_0
    | Psig_kind_abbrev (_arg_0, _arg_1) ->
      reducer.location_loc reducer (ret_init reducer) env _arg_0
      |> fun x ->
      reducer.__combine x (reducer.jkind_annotation reducer env _arg_1)
    | Psig_docstring _arg_0 -> ret_init reducer env _arg_0
  ;;

  let reduce_binding_op reducer env binding_op =
    reducer.location_loc reducer (ret_init reducer) env binding_op.pbop_op
    |> fun x ->
    reducer.__combine
      x
      (reducer.value_binding reducer env binding_op.pbop_binding)
    |> fun x ->
    reducer.__combine x (reducer.location reducer env binding_op.pbop_loc)
  ;;

  let reduce_record_field reducer reduce_'a env record_field =
    reducer.location_loc
      reducer
      (reducer.longident reducer)
      env
      record_field.field_name
    |> fun x ->
    reducer.__combine
      x
      (reduce_option
         reducer
         (reducer.type_constraint reducer)
         env
         record_field.typ)
    |> fun x ->
    reducer.__combine x (reduce_option reducer reduce_'a env record_field.value)
  ;;

  let reduce_arg_label reducer env arg_label =
    match arg_label with
    | Nolabel -> reducer.__init
    | Labelled _arg_0 -> ret_init reducer env _arg_0
    | Optional _arg_0 -> ret_init reducer env _arg_0
  ;;

  let reduce_comprehension_clause reducer env comprehension_clause =
    match comprehension_clause with
    | Pcomp_for _arg_0 ->
      reduce_list
        reducer
        (reducer.comprehension_clause_binding reducer)
        env
        _arg_0
    | Pcomp_when _arg_0 -> reducer.expression reducer env _arg_0
  ;;

  let reduce_class_field_desc reducer env class_field_desc =
    match class_field_desc with
    | Pcf_inherit (_arg_0, _arg_1, _arg_2) ->
      reducer.override_flag reducer env _arg_0
      |> fun x ->
      reducer.__combine x (reducer.class_expr reducer env _arg_1)
      |> fun x ->
      reducer.__combine
        x
        (reduce_option
           reducer
           (reducer.location_loc reducer (ret_init reducer))
           env
           _arg_2)
    | Pcf_val _arg_0 ->
      let _0, _1, _2 = _arg_0 in
      reducer.location_loc reducer (ret_init reducer) env _0
      |> fun x ->
      reducer.__combine x (reducer.mutable_flag reducer env _1)
      |> fun x -> reducer.__combine x (reducer.class_field_kind reducer env _2)
    | Pcf_method _arg_0 ->
      let _0, _1, _2 = _arg_0 in
      reducer.location_loc reducer (ret_init reducer) env _0
      |> fun x ->
      reducer.__combine x (reducer.private_flag reducer env _1)
      |> fun x -> reducer.__combine x (reducer.class_field_kind reducer env _2)
    | Pcf_constraint _arg_0 ->
      let _0, _1 = _arg_0 in
      reducer.core_type reducer env _0
      |> fun x -> reducer.__combine x (reducer.core_type reducer env _1)
    | Pcf_initializer _arg_0 -> reducer.expression reducer env _arg_0
    | Pcf_attribute _arg_0 -> reducer.attribute reducer env _arg_0
    | Pcf_extension _arg_0 -> reducer.extension reducer env _arg_0
    | Pcf_docstring _arg_0 -> ret_init reducer env _arg_0
  ;;

  let reduce_longident_str_or_op reducer env str_or_op =
    match str_or_op with
    | Longident.Str _arg_0 -> ret_init reducer env _arg_0
    | Longident.Str_trailing_hash _arg_0 -> ret_init reducer env _arg_0
    | Longident.Op _arg_0 -> ret_init reducer env _arg_0
    | Longident.DotOp (_arg_0, _arg_1, _arg_2, _arg_3) ->
      ret_init reducer env _arg_0
      |> fun x ->
      reducer.__combine x (reducer.longident_dotop_delims reducer env _arg_1)
      |> fun x ->
      reducer.__combine x (ret_init reducer env _arg_2)
      |> fun x -> reducer.__combine x (ret_init reducer env _arg_3)
  ;;

  let reduce_core_type_desc reducer env core_type_desc =
    match core_type_desc with
    | Ptyp_any _arg_0 ->
      reduce_option reducer (reducer.jkind_annotation reducer) env _arg_0
    | Ptyp_var (_arg_0, _arg_1) ->
      ret_init reducer env _arg_0
      |> fun x ->
      reducer.__combine
        x
        (reduce_option reducer (reducer.jkind_annotation reducer) env _arg_1)
    | Ptyp_arrow _arg ->
      reducer.arrow_arg reducer env _arg.domain
      |> fun x ->
      reducer.__combine x (reducer.modes reducer env _arg.codom_legacy_modes)
      |> fun x ->
      reducer.__combine x (reducer.core_type reducer env _arg.codom_type)
      |> fun x ->
      reducer.__combine x (reducer.modes reducer env _arg.codom_modes)
    | Ptyp_tuple _arg_0 ->
      reduce_list
        reducer
        (fun env __arg ->
          let _0, _1 = __arg in
          reduce_option reducer (ret_init reducer) env _0
          |> fun x -> reducer.__combine x (reducer.core_type reducer env _1))
        env
        _arg_0
    | Ptyp_unboxed_tuple _arg_0 ->
      reduce_list
        reducer
        (fun env __arg ->
          let _0, _1 = __arg in
          reduce_option reducer (ret_init reducer) env _0
          |> fun x -> reducer.__combine x (reducer.core_type reducer env _1))
        env
        _arg_0
    | Ptyp_constr (_arg_0, _arg_1) ->
      reduce_list reducer (reducer.core_type reducer) env _arg_0
      |> fun x ->
      reducer.__combine
        x
        (reducer.location_loc reducer (reducer.longident reducer) env _arg_1)
    | Ptyp_object (_arg_0, _arg_1) ->
      reduce_list reducer (reducer.object_field reducer) env _arg_0
      |> fun x -> reducer.__combine x (reducer.closed_flag reducer env _arg_1)
    | Ptyp_class (_arg_0, _arg_1) ->
      reducer.location_loc reducer (reducer.longident reducer) env _arg_0
      |> fun x ->
      reducer.__combine
        x
        (reduce_list reducer (reducer.core_type reducer) env _arg_1)
    | Ptyp_alias (_arg_0, _arg_1, _arg_2) ->
      reducer.core_type reducer env _arg_0
      |> fun x ->
      reducer.__combine
        x
        (reduce_option
           reducer
           (reducer.location_loc reducer (ret_init reducer))
           env
           _arg_1)
      |> fun x ->
      reducer.__combine
        x
        (reduce_option reducer (reducer.jkind_annotation reducer) env _arg_2)
    | Ptyp_variant (_arg_0, _arg_1, _arg_2) ->
      reduce_list reducer (reducer.row_field reducer) env _arg_0
      |> fun x ->
      reducer.__combine x (reducer.closed_flag reducer env _arg_1)
      |> fun x ->
      reducer.__combine
        x
        (reduce_option
           reducer
           (reduce_list reducer (reducer.label reducer))
           env
           _arg_2)
    | Ptyp_poly (_arg_0, _arg_1) ->
      reduce_list
        reducer
        (fun env __arg ->
          let _0, _1 = __arg in
          reducer.location_loc reducer (ret_init reducer) env _0
          |> fun x ->
          reducer.__combine
            x
            (reduce_option reducer (reducer.jkind_annotation reducer) env _1))
        env
        _arg_0
      |> fun x -> reducer.__combine x (reducer.core_type reducer env _arg_1)
    | Ptyp_package (_arg_0, _arg_1) ->
      reducer.ext_attribute reducer env _arg_0
      |> fun x -> reducer.__combine x (reducer.package_type reducer env _arg_1)
    | Ptyp_open (_arg_0, _arg_1) ->
      reducer.location_loc reducer (reducer.longident reducer) env _arg_0
      |> fun x -> reducer.__combine x (reducer.core_type reducer env _arg_1)
    | Ptyp_quote _arg_0 -> reducer.core_type reducer env _arg_0
    | Ptyp_splice _arg_0 -> reducer.core_type reducer env _arg_0
    | Ptyp_of_kind _arg_0 -> reducer.jkind_annotation reducer env _arg_0
    | Ptyp_extension _arg_0 -> reducer.extension reducer env _arg_0
    | Ptyp_parens _arg_0 -> reducer.core_type reducer env _arg_0
  ;;

  let reduce_ptype_param reducer env ptype_param =
    reducer.core_type reducer env ptype_param.ptp_typ
    |> fun x ->
    reducer.__combine
      x
      (let _0, _1 = ptype_param.ptp_infos in
       reducer.variance reducer env _0
       |> fun x -> reducer.__combine x (reducer.injectivity reducer env _1))
    |> fun x ->
    reducer.__combine x (reducer.tokens_seq reducer env ptype_param.ptp_tokens)
  ;;

  let reduce_virtual_flag reducer env virtual_flag =
    match virtual_flag with
    | Virtual -> reducer.__init
    | Concrete -> reducer.__init
  ;;

  let reduce_module_type_desc reducer env module_type_desc =
    match module_type_desc with
    | Pmty_ident _arg_0 ->
      reducer.location_loc reducer (reducer.longident reducer) env _arg_0
    | Pmty_signature _arg_0 -> reducer.signature reducer env _arg_0
    | Pmty_functor (_arg_0, _arg_1, _arg_2, _arg_3) ->
      reducer.attributes reducer env _arg_0
      |> fun x ->
      reducer.__combine
        x
        (reduce_list reducer (reducer.functor_parameter reducer) env _arg_1)
      |> fun x ->
      reducer.__combine x (reducer.module_type reducer env _arg_2)
      |> fun x -> reducer.__combine x (reducer.modes reducer env _arg_3)
    | Pmty_functor_type (_arg_0, _arg_1, _arg_2) ->
      reduce_list reducer (reducer.functor_parameter reducer) env _arg_0
      |> fun x ->
      reducer.__combine x (reducer.module_type reducer env _arg_1)
      |> fun x -> reducer.__combine x (reducer.modes reducer env _arg_2)
    | Pmty_with (_arg_0, _arg_1) ->
      reducer.module_type reducer env _arg_0
      |> fun x ->
      reducer.__combine
        x
        (reduce_list reducer (reducer.with_constraint reducer) env _arg_1)
    | Pmty_typeof (_arg_0, _arg_1) ->
      reducer.attributes reducer env _arg_0
      |> fun x -> reducer.__combine x (reducer.module_expr reducer env _arg_1)
    | Pmty_extension _arg_0 -> reducer.extension reducer env _arg_0
    | Pmty_alias _arg_0 ->
      reducer.location_loc reducer (reducer.longident reducer) env _arg_0
    | Pmty_strengthen (_arg_0, _arg_1) ->
      reducer.module_type reducer env _arg_0
      |> fun x ->
      reducer.__combine
        x
        (reducer.location_loc reducer (reducer.longident reducer) env _arg_1)
    | Pmty_parens _arg_0 -> reducer.module_type reducer env _arg_0
  ;;

  let reduce_row_field reducer env row_field =
    reducer.row_field_desc reducer env row_field.prf_desc
    |> fun x ->
    reducer.__combine x (reducer.location reducer env row_field.prf_loc)
    |> fun x ->
    reducer.__combine
      x
      (reducer.attributes reducer env row_field.prf_attributes)
    |> fun x ->
    reducer.__combine
      x
      (reduce_option reducer (ret_init reducer) env row_field.prf_doc)
    |> fun x ->
    reducer.__combine x (reducer.tokens_seq reducer env row_field.prf_tokens)
  ;;

  let reduce_class_type reducer env class_type =
    reducer.class_type_desc reducer env class_type.pcty_desc
    |> fun x ->
    reducer.__combine x (reducer.location reducer env class_type.pcty_loc)
    |> fun x ->
    reducer.__combine
      x
      (reducer.attributes reducer env class_type.pcty_attributes)
    |> fun x ->
    reducer.__combine x (reducer.tokens_seq reducer env class_type.pcty_tokens)
  ;;

  let reduce_include_description reducer env include_description =
    reducer.include_infos
      reducer
      (reducer.module_type reducer)
      env
      include_description
  ;;

  let reduce_modality reducer env modality =
    match modality with
    | Modality _arg_0 -> ret_init reducer env _arg_0
  ;;

  let reduce_object_field reducer env object_field =
    reducer.object_field_desc reducer env object_field.pof_desc
    |> fun x ->
    reducer.__combine x (reducer.location reducer env object_field.pof_loc)
    |> fun x ->
    reducer.__combine
      x
      (reducer.attributes reducer env object_field.pof_attributes)
    |> fun x ->
    reducer.__combine
      x
      (reduce_option reducer (ret_init reducer) env object_field.pof_doc)
    |> fun x ->
    reducer.__combine x (reducer.tokens_seq reducer env object_field.pof_tokens)
  ;;

  let reduce_structure_item_desc reducer env structure_item_desc =
    match structure_item_desc with
    | Pstr_eval (_arg_0, _arg_1) ->
      reducer.expression reducer env _arg_0
      |> fun x -> reducer.__combine x (reducer.attributes reducer env _arg_1)
    | Pstr_value (_arg_0, _arg_1) ->
      reducer.rec_flag reducer env _arg_0
      |> fun x ->
      reducer.__combine
        x
        (reduce_list reducer (reducer.value_binding reducer) env _arg_1)
    | Pstr_primitive _arg_0 -> reducer.value_description reducer env _arg_0
    | Pstr_type (_arg_0, _arg_1) ->
      reducer.rec_flag reducer env _arg_0
      |> fun x ->
      reducer.__combine
        x
        (reduce_list reducer (reducer.type_declaration reducer) env _arg_1)
    | Pstr_typext _arg_0 -> reducer.type_extension reducer env _arg_0
    | Pstr_exception _arg_0 -> reducer.type_exception reducer env _arg_0
    | Pstr_module _arg_0 -> reducer.module_binding reducer env _arg_0
    | Pstr_recmodule _arg_0 ->
      reduce_list reducer (reducer.module_binding reducer) env _arg_0
    | Pstr_modtype _arg_0 -> reducer.module_type_declaration reducer env _arg_0
    | Pstr_open _arg_0 -> reducer.open_declaration reducer env _arg_0
    | Pstr_class _arg_0 ->
      reduce_list reducer (reducer.class_declaration reducer) env _arg_0
    | Pstr_class_type _arg_0 ->
      reduce_list reducer (reducer.class_type_declaration reducer) env _arg_0
    | Pstr_include _arg_0 -> reducer.include_declaration reducer env _arg_0
    | Pstr_attribute _arg_0 -> reducer.attribute reducer env _arg_0
    | Pstr_extension _arg_0 -> reducer.toplevel_extension reducer env _arg_0
    | Pstr_kind_abbrev (_arg_0, _arg_1) ->
      reducer.location_loc reducer (ret_init reducer) env _arg_0
      |> fun x ->
      reducer.__combine x (reducer.jkind_annotation reducer env _arg_1)
    | Pstr_docstring _arg_0 -> ret_init reducer env _arg_0
  ;;

  let reduce_row_field_desc reducer env row_field_desc =
    match row_field_desc with
    | Rtag (_arg_0, _arg_1, _arg_2) ->
      reducer.location_loc reducer (ret_init reducer) env _arg_0
      |> fun x ->
      reducer.__combine x (ret_init reducer env _arg_1)
      |> fun x ->
      reducer.__combine
        x
        (reduce_list reducer (reducer.core_type reducer) env _arg_2)
    | Rinherit _arg_0 -> reducer.core_type reducer env _arg_0
  ;;

  let reduce_type_exception reducer env type_exception =
    reduce_option reducer (ret_init reducer) env type_exception.ptyexn_pre_doc
    |> fun x ->
    reducer.__combine
      x
      (reducer.ext_attribute reducer env type_exception.ptyexn_ext_attrs)
    |> fun x ->
    reducer.__combine
      x
      (reducer.extension_constructor
         reducer
         env
         type_exception.ptyexn_constructor)
    |> fun x ->
    reducer.__combine x (reducer.location reducer env type_exception.ptyexn_loc)
    |> fun x ->
    reducer.__combine
      x
      (reducer.attributes reducer env type_exception.ptyexn_attributes)
    |> fun x ->
    reducer.__combine
      x
      (reduce_option
         reducer
         (ret_init reducer)
         env
         type_exception.ptyexn_post_doc)
    |> fun x ->
    reducer.__combine
      x
      (reducer.tokens_seq reducer env type_exception.ptyexn_tokens)
  ;;

  let reduce_mutable_flag reducer env mutable_flag =
    match mutable_flag with
    | Immutable -> reducer.__init
    | Mutable -> reducer.__init
  ;;

  let reduce_direction_flag reducer env direction_flag =
    match direction_flag with
    | Upto -> reducer.__init
    | Downto -> reducer.__init
  ;;

  let reduce_block_access reducer env block_access =
    match block_access with
    | Baccess_field _arg_0 ->
      reducer.location_loc reducer (reducer.longident reducer) env _arg_0
    | Baccess_array (_arg_0, _arg_1, _arg_2) ->
      reducer.mutable_flag reducer env _arg_0
      |> fun x ->
      reducer.__combine x (reducer.index_kind reducer env _arg_1)
      |> fun x -> reducer.__combine x (reducer.expression reducer env _arg_2)
    | Baccess_block (_arg_0, _arg_1) ->
      reducer.mutable_flag reducer env _arg_0
      |> fun x -> reducer.__combine x (reducer.expression reducer env _arg_1)
  ;;

  let reduce_constructor_argument reducer env constructor_argument =
    ret_init reducer env constructor_argument.pca_global
    |> fun x ->
    reducer.__combine
      x
      (reducer.core_type reducer env constructor_argument.pca_type)
    |> fun x ->
    reducer.__combine
      x
      (reducer.modalities reducer env constructor_argument.pca_modalities)
    |> fun x ->
    reducer.__combine
      x
      (reducer.location reducer env constructor_argument.pca_loc)
  ;;

  let reduce_class_expr reducer env class_expr =
    reducer.ext_attribute reducer env class_expr.pcl_ext_attrs
    |> fun x ->
    reducer.__combine
      x
      (reducer.class_expr_desc reducer env class_expr.pcl_desc)
    |> fun x ->
    reducer.__combine x (reducer.location reducer env class_expr.pcl_loc)
    |> fun x ->
    reducer.__combine
      x
      (reducer.attributes reducer env class_expr.pcl_attributes)
  ;;

  let reduce_label_declaration reducer env label_declaration =
    reducer.location_loc
      reducer
      (ret_init reducer)
      env
      label_declaration.pld_name
    |> fun x ->
    reducer.__combine
      x
      (reducer.mutable_flag reducer env label_declaration.pld_mutable)
    |> fun x ->
    reducer.__combine x (ret_init reducer env label_declaration.pld_global)
    |> fun x ->
    reducer.__combine
      x
      (reducer.modalities reducer env label_declaration.pld_modalities)
    |> fun x ->
    reducer.__combine
      x
      (reducer.core_type reducer env label_declaration.pld_type)
    |> fun x ->
    reducer.__combine x (reducer.location reducer env label_declaration.pld_loc)
    |> fun x ->
    reducer.__combine
      x
      (reducer.attributes reducer env label_declaration.pld_attributes)
    |> fun x ->
    reducer.__combine
      x
      (reduce_option reducer (ret_init reducer) env label_declaration.pld_doc)
    |> fun x ->
    reducer.__combine
      x
      (reducer.tokens_seq reducer env label_declaration.pld_tokens)
  ;;

  let reduce_signature reducer env signature =
    reducer.modalities reducer env signature.psg_modalities
    |> fun x ->
    reducer.__combine
      x
      (reduce_list
         reducer
         (reducer.signature_item reducer)
         env
         signature.psg_items)
    |> fun x ->
    reducer.__combine x (reducer.location reducer env signature.psg_loc)
    |> fun x ->
    reducer.__combine x (reducer.tokens_seq reducer env signature.psg_tokens)
  ;;

  let reduce_tokens_seq reducer env seq =
    reduce_list reducer (reducer.tokens_elt reducer) env seq
  ;;

  let reduce_class_field_kind reducer env class_field_kind =
    match class_field_kind with
    | Cfk_virtual _arg_0 -> reducer.core_type reducer env _arg_0
    | Cfk_concrete (_arg_0, _arg_1) ->
      reducer.override_flag reducer env _arg_0
      |> fun x -> reducer.__combine x (reducer.value_binding reducer env _arg_1)
  ;;

  let reduce_toplevel_extension reducer env toplevel_extension =
    reduce_option reducer (ret_init reducer) env toplevel_extension.te_pre_doc
    |> fun x ->
    reducer.__combine
      x
      (reducer.extension reducer env toplevel_extension.te_ext)
    |> fun x ->
    reducer.__combine
      x
      (reducer.attributes reducer env toplevel_extension.te_attrs)
    |> fun x ->
    reducer.__combine
      x
      (reduce_option
         reducer
         (ret_init reducer)
         env
         toplevel_extension.te_post_doc)
  ;;

  let reduce_ptype_constraint reducer env ptype_constraint =
    let _0, _1, _2 = ptype_constraint in
    reducer.core_type reducer env _0
    |> fun x ->
    reducer.__combine x (reducer.core_type reducer env _1)
    |> fun x -> reducer.__combine x (reducer.location reducer env _2)
  ;;

  let reduce_type_constraint reducer env type_constraint =
    match type_constraint with
    | Pconstraint _arg_0 -> reducer.core_type reducer env _arg_0
    | Pcoerce (_arg_0, _arg_1) ->
      reduce_option reducer (reducer.core_type reducer) env _arg_0
      |> fun x -> reducer.__combine x (reducer.core_type reducer env _arg_1)
  ;;

  let reduce_comprehension_clause_binding
    reducer
    env
    comprehension_clause_binding
    =
    reduce_option
      reducer
      (reducer.location_loc reducer (reducer.mode reducer))
      env
      comprehension_clause_binding.pcomp_cb_mode
    |> fun x ->
    reducer.__combine
      x
      (reducer.pattern reducer env comprehension_clause_binding.pcomp_cb_pattern)
    |> fun x ->
    reducer.__combine
      x
      (reducer.comprehension_iterator
         reducer
         env
         comprehension_clause_binding.pcomp_cb_iterator)
    |> fun x ->
    reducer.__combine
      x
      (reducer.attributes
         reducer
         env
         comprehension_clause_binding.pcomp_cb_attributes)
    |> fun x ->
    reducer.__combine
      x
      (reducer.tokens_seq
         reducer
         env
         comprehension_clause_binding.pcomp_cb_tokens)
  ;;

  let reduce_tokens_elt reducer env elt =
    reducer.tokens_desc reducer env elt.Tokens.desc
    |> fun x -> reducer.__combine x (ret_init reducer env elt.Tokens.pos)
  ;;

  let reduce_object_field_desc reducer env object_field_desc =
    match object_field_desc with
    | Otag (_arg_0, _arg_1) ->
      reducer.location_loc reducer (ret_init reducer) env _arg_0
      |> fun x -> reducer.__combine x (reducer.core_type reducer env _arg_1)
    | Oinherit _arg_0 -> reducer.core_type reducer env _arg_0
  ;;

  let reduce_expression_desc reducer env expression_desc =
    match expression_desc with
    | Pexp_ident _arg_0 ->
      reducer.location_loc reducer (reducer.longident reducer) env _arg_0
    | Pexp_constant _arg_0 -> reducer.constant reducer env _arg_0
    | Pexp_let (_arg_0, _arg_1, _arg_2, _arg_3) ->
      reducer.mutable_flag reducer env _arg_0
      |> fun x ->
      reducer.__combine x (reducer.rec_flag reducer env _arg_1)
      |> fun x ->
      reducer.__combine
        x
        (reduce_list reducer (reducer.value_binding reducer) env _arg_2)
      |> fun x -> reducer.__combine x (reducer.expression reducer env _arg_3)
    | Pexp_function (_arg_0, _arg_1, _arg_2) ->
      reduce_list reducer (reducer.function_param reducer) env _arg_0
      |> fun x ->
      reducer.__combine x (reducer.function_constraint reducer env _arg_1)
      |> fun x -> reducer.__combine x (reducer.function_body reducer env _arg_2)
    | Pexp_prefix_apply (_arg_0, _arg_1) ->
      reducer.expression reducer env _arg_0
      |> fun x -> reducer.__combine x (reducer.expression reducer env _arg_1)
    | Pexp_add_or_sub (_arg_0, _arg_1) ->
      ret_init reducer env _arg_0
      |> fun x -> reducer.__combine x (reducer.expression reducer env _arg_1)
    | Pexp_infix_apply _arg ->
      reducer.expression reducer env _arg.arg1
      |> fun x ->
      reducer.__combine x (reducer.expression reducer env _arg.op)
      |> fun x -> reducer.__combine x (reducer.expression reducer env _arg.arg2)
    | Pexp_apply (_arg_0, _arg_1) ->
      reducer.expression reducer env _arg_0
      |> fun x ->
      reducer.__combine
        x
        (reduce_list
           reducer
           (reducer.argument reducer (reducer.expression reducer))
           env
           _arg_1)
    | Pexp_match (_arg_0, _arg_1) ->
      reducer.expression reducer env _arg_0
      |> fun x ->
      reducer.__combine
        x
        (reduce_list reducer (reducer.case reducer) env _arg_1)
    | Pexp_try (_arg_0, _arg_1) ->
      reducer.expression reducer env _arg_0
      |> fun x ->
      reducer.__combine
        x
        (reduce_list reducer (reducer.case reducer) env _arg_1)
    | Pexp_tuple _arg_0 ->
      reduce_list
        reducer
        (reducer.argument reducer (reducer.expression reducer))
        env
        _arg_0
    | Pexp_unboxed_tuple _arg_0 ->
      reduce_list
        reducer
        (reducer.argument reducer (reducer.expression reducer))
        env
        _arg_0
    | Pexp_construct (_arg_0, _arg_1) ->
      reducer.location_loc reducer (reducer.longident reducer) env _arg_0
      |> fun x ->
      reducer.__combine
        x
        (reduce_option reducer (reducer.expression reducer) env _arg_1)
    | Pexp_variant (_arg_0, _arg_1) ->
      reducer.label reducer env _arg_0
      |> fun x ->
      reducer.__combine
        x
        (reduce_option reducer (reducer.expression reducer) env _arg_1)
    | Pexp_record (_arg_0, _arg_1) ->
      reduce_option reducer (reducer.expression reducer) env _arg_0
      |> fun x ->
      reducer.__combine
        x
        (reduce_list
           reducer
           (reducer.record_field reducer (reducer.expression reducer))
           env
           _arg_1)
    | Pexp_record_unboxed_product (_arg_0, _arg_1) ->
      reduce_option reducer (reducer.expression reducer) env _arg_0
      |> fun x ->
      reducer.__combine
        x
        (reduce_list
           reducer
           (reducer.record_field reducer (reducer.expression reducer))
           env
           _arg_1)
    | Pexp_field (_arg_0, _arg_1) ->
      reducer.expression reducer env _arg_0
      |> fun x ->
      reducer.__combine
        x
        (reducer.location_loc reducer (reducer.longident reducer) env _arg_1)
    | Pexp_unboxed_field (_arg_0, _arg_1) ->
      reducer.expression reducer env _arg_0
      |> fun x ->
      reducer.__combine
        x
        (reducer.location_loc reducer (reducer.longident reducer) env _arg_1)
    | Pexp_setfield (_arg_0, _arg_1, _arg_2) ->
      reducer.expression reducer env _arg_0
      |> fun x ->
      reducer.__combine
        x
        (reducer.location_loc reducer (reducer.longident reducer) env _arg_1)
      |> fun x -> reducer.__combine x (reducer.expression reducer env _arg_2)
    | Pexp_array (_arg_0, _arg_1) ->
      reducer.mutable_flag reducer env _arg_0
      |> fun x ->
      reducer.__combine
        x
        (reduce_list reducer (reducer.expression reducer) env _arg_1)
    | Pexp_idx (_arg_0, _arg_1) ->
      reducer.block_access reducer env _arg_0
      |> fun x ->
      reducer.__combine
        x
        (reduce_list reducer (reducer.unboxed_access reducer) env _arg_1)
    | Pexp_ifthenelse (_arg_0, _arg_1, _arg_2) ->
      reducer.expression reducer env _arg_0
      |> fun x ->
      reducer.__combine x (reducer.expression reducer env _arg_1)
      |> fun x ->
      reducer.__combine
        x
        (reduce_option reducer (reducer.expression reducer) env _arg_2)
    | Pexp_sequence (_arg_0, _arg_1) ->
      reducer.expression reducer env _arg_0
      |> fun x -> reducer.__combine x (reducer.expression reducer env _arg_1)
    | Pexp_seq_empty _arg_0 -> reducer.expression reducer env _arg_0
    | Pexp_while (_arg_0, _arg_1) ->
      reducer.expression reducer env _arg_0
      |> fun x -> reducer.__combine x (reducer.expression reducer env _arg_1)
    | Pexp_for (_arg_0, _arg_1, _arg_2, _arg_3, _arg_4) ->
      reducer.pattern reducer env _arg_0
      |> fun x ->
      reducer.__combine x (reducer.expression reducer env _arg_1)
      |> fun x ->
      reducer.__combine x (reducer.expression reducer env _arg_2)
      |> fun x ->
      reducer.__combine x (reducer.direction_flag reducer env _arg_3)
      |> fun x -> reducer.__combine x (reducer.expression reducer env _arg_4)
    | Pexp_constraint (_arg_0, _arg_1, _arg_2) ->
      reducer.expression reducer env _arg_0
      |> fun x ->
      reducer.__combine
        x
        (reduce_option reducer (reducer.core_type reducer) env _arg_1)
      |> fun x -> reducer.__combine x (reducer.modes reducer env _arg_2)
    | Pexp_coerce (_arg_0, _arg_1, _arg_2) ->
      reducer.expression reducer env _arg_0
      |> fun x ->
      reducer.__combine
        x
        (reduce_option reducer (reducer.core_type reducer) env _arg_1)
      |> fun x -> reducer.__combine x (reducer.core_type reducer env _arg_2)
    | Pexp_send (_arg_0, _arg_1) ->
      reducer.expression reducer env _arg_0
      |> fun x ->
      reducer.__combine
        x
        (reducer.location_loc reducer (ret_init reducer) env _arg_1)
    | Pexp_new _arg_0 ->
      reducer.location_loc reducer (reducer.longident reducer) env _arg_0
    | Pexp_setvar (_arg_0, _arg_1) ->
      reducer.location_loc reducer (ret_init reducer) env _arg_0
      |> fun x -> reducer.__combine x (reducer.expression reducer env _arg_1)
    | Pexp_override _arg_0 ->
      reduce_list
        reducer
        (fun env __arg ->
          let _0, _1 = __arg in
          reducer.location_loc reducer (ret_init reducer) env _0
          |> fun x ->
          reducer.__combine
            x
            (reduce_option reducer (reducer.expression reducer) env _1))
        env
        _arg_0
    | Pexp_letmodule (_arg_0, _arg_1) ->
      reducer.module_binding reducer env _arg_0
      |> fun x -> reducer.__combine x (reducer.expression reducer env _arg_1)
    | Pexp_letexception (_arg_0, _arg_1) ->
      reducer.extension_constructor reducer env _arg_0
      |> fun x -> reducer.__combine x (reducer.expression reducer env _arg_1)
    | Pexp_assert _arg_0 -> reducer.expression reducer env _arg_0
    | Pexp_lazy _arg_0 -> reducer.expression reducer env _arg_0
    | Pexp_object _arg_0 -> reducer.class_structure reducer env _arg_0
    | Pexp_pack (_arg_0, _arg_1) ->
      reducer.module_expr reducer env _arg_0
      |> fun x ->
      reducer.__combine
        x
        (reduce_option reducer (reducer.package_type reducer) env _arg_1)
    | Pexp_dot_open (_arg_0, _arg_1) ->
      reducer.location_loc reducer (reducer.longident reducer) env _arg_0
      |> fun x -> reducer.__combine x (reducer.expression reducer env _arg_1)
    | Pexp_let_open (_arg_0, _arg_1) ->
      reducer.open_declaration reducer env _arg_0
      |> fun x -> reducer.__combine x (reducer.expression reducer env _arg_1)
    | Pexp_letop _arg_0 -> reducer.letop reducer env _arg_0
    | Pexp_extension _arg_0 -> reducer.extension reducer env _arg_0
    | Pexp_unreachable -> reducer.__init
    | Pexp_stack _arg_0 -> reducer.expression reducer env _arg_0
    | Pexp_comprehension _arg_0 ->
      reducer.comprehension_expression reducer env _arg_0
    | Pexp_overwrite (_arg_0, _arg_1) ->
      reducer.expression reducer env _arg_0
      |> fun x -> reducer.__combine x (reducer.expression reducer env _arg_1)
    | Pexp_quote _arg_0 -> reducer.expression reducer env _arg_0
    | Pexp_splice _arg_0 -> reducer.expression reducer env _arg_0
    | Pexp_hole -> reducer.__init
    | Pexp_index_op _arg ->
      reducer.paren_kind reducer env _arg.kind
      |> fun x ->
      reducer.__combine
        x
        (reduce_option
           reducer
           (fun env __arg ->
             let _0, _1 = __arg in
             reduce_option reducer (reducer.longident reducer) env _0
             |> fun x -> reducer.__combine x (ret_init reducer env _1))
           env
           _arg.op)
      |> fun x ->
      reducer.__combine x (reducer.expression reducer env _arg.seq)
      |> fun x ->
      reducer.__combine
        x
        (reduce_list reducer (reducer.expression reducer) env _arg.indices)
      |> fun x ->
      reducer.__combine
        x
        (reduce_option reducer (reducer.expression reducer) env _arg.assign)
    | Pexp_parens _arg ->
      reducer.expression reducer env _arg.exp
      |> fun x -> reducer.__combine x (ret_init reducer env _arg.optional)
    | Pexp_begin_end _arg_0 ->
      reduce_option reducer (reducer.expression reducer) env _arg_0
    | Pexp_list _arg_0 ->
      reduce_list reducer (reducer.expression reducer) env _arg_0
    | Pexp_cons (_arg_0, _arg_1) ->
      reducer.expression reducer env _arg_0
      |> fun x -> reducer.__combine x (reducer.expression reducer env _arg_1)
    | Pexp_exclave _arg_0 -> reducer.expression reducer env _arg_0
    | Pexp_mode_legacy (_arg_0, _arg_1) ->
      reducer.location_loc reducer (reducer.mode reducer) env _arg_0
      |> fun x -> reducer.__combine x (reducer.expression reducer env _arg_1)
  ;;

  let reduce_modalities reducer env modalities =
    reduce_list
      reducer
      (reducer.location_loc reducer (reducer.modality reducer))
      env
      modalities
  ;;

  let reduce_class_infos reducer reduce_'a env class_infos =
    reduce_list reducer (ret_init reducer) env class_infos.pci_pre_text
    |> fun x ->
    reducer.__combine
      x
      (reduce_option reducer (ret_init reducer) env class_infos.pci_pre_doc)
    |> fun x ->
    reducer.__combine x (reducer.virtual_flag reducer env class_infos.pci_virt)
    |> fun x ->
    reducer.__combine
      x
      (reducer.ext_attribute reducer env class_infos.pci_ext_attrs)
    |> fun x ->
    reducer.__combine
      x
      (reduce_list
         reducer
         (reducer.ptype_param reducer)
         env
         class_infos.pci_params)
    |> fun x ->
    reducer.__combine
      x
      (reducer.location_loc reducer (ret_init reducer) env class_infos.pci_name)
    |> fun x ->
    reducer.__combine
      x
      (reduce_list
         reducer
         (reducer.argument reducer (reducer.pattern reducer))
         env
         class_infos.pci_value_params)
    |> fun x ->
    reducer.__combine
      x
      (reduce_option
         reducer
         (reducer.class_type reducer)
         env
         class_infos.pci_constraint)
    |> fun x ->
    reducer.__combine x (reduce_'a env class_infos.pci_expr)
    |> fun x ->
    reducer.__combine x (reducer.location reducer env class_infos.pci_loc)
    |> fun x ->
    reducer.__combine
      x
      (reducer.attributes reducer env class_infos.pci_attributes)
    |> fun x ->
    reducer.__combine
      x
      (reduce_option reducer (ret_init reducer) env class_infos.pci_post_doc)
    |> fun x ->
    reducer.__combine x (reducer.tokens_seq reducer env class_infos.pci_tokens)
  ;;

  let reduce_argument reducer reduce_'a env argument =
    reducer.argument_desc reducer reduce_'a env argument.parg_desc
    |> fun x ->
    reducer.__combine x (reducer.tokens_seq reducer env argument.parg_tokens)
  ;;

  let reduce_module_substitution reducer env module_substitution =
    reduce_option reducer (ret_init reducer) env module_substitution.pms_pre_doc
    |> fun x ->
    reducer.__combine
      x
      (reducer.ext_attribute reducer env module_substitution.pms_ext_attrs)
    |> fun x ->
    reducer.__combine
      x
      (reducer.location_loc
         reducer
         (ret_init reducer)
         env
         module_substitution.pms_name)
    |> fun x ->
    reducer.__combine
      x
      (reducer.location_loc
         reducer
         (reducer.longident reducer)
         env
         module_substitution.pms_manifest)
    |> fun x ->
    reducer.__combine
      x
      (reducer.attributes reducer env module_substitution.pms_attributes)
    |> fun x ->
    reducer.__combine
      x
      (reduce_option
         reducer
         (ret_init reducer)
         env
         module_substitution.pms_post_doc)
    |> fun x ->
    reducer.__combine
      x
      (reducer.location reducer env module_substitution.pms_loc)
    |> fun x ->
    reducer.__combine
      x
      (reducer.tokens_seq reducer env module_substitution.pms_tokens)
  ;;

  let reduce_label reducer env label = ret_init reducer env label

  let reduce_open_infos reducer reduce_'a env open_infos =
    reduce_option reducer (ret_init reducer) env open_infos.popen_pre_doc
    |> fun x ->
    reducer.__combine
      x
      (reducer.ext_attribute reducer env open_infos.popen_ext_attrs)
    |> fun x ->
    reducer.__combine x (reduce_'a env open_infos.popen_expr)
    |> fun x ->
    reducer.__combine
      x
      (reducer.override_flag reducer env open_infos.popen_override)
    |> fun x ->
    reducer.__combine x (reducer.location reducer env open_infos.popen_loc)
    |> fun x ->
    reducer.__combine
      x
      (reducer.attributes reducer env open_infos.popen_attributes)
    |> fun x ->
    reducer.__combine
      x
      (reduce_option reducer (ret_init reducer) env open_infos.popen_post_doc)
    |> fun x ->
    reducer.__combine x (reducer.tokens_seq reducer env open_infos.popen_tokens)
  ;;

  let reduce_constructor_arguments reducer env constructor_arguments =
    match constructor_arguments with
    | Pcstr_tuple _arg_0 ->
      reduce_list reducer (reducer.constructor_argument reducer) env _arg_0
    | Pcstr_record _arg_0 ->
      reduce_list reducer (reducer.label_declaration reducer) env _arg_0
  ;;

  let reduce_module_type reducer env module_type =
    reducer.module_type_desc reducer env module_type.pmty_desc
    |> fun x ->
    reducer.__combine x (reducer.location reducer env module_type.pmty_loc)
    |> fun x ->
    reducer.__combine
      x
      (reducer.attributes reducer env module_type.pmty_attributes)
    |> fun x ->
    reducer.__combine x (reducer.tokens_seq reducer env module_type.pmty_tokens)
  ;;

  let reduce_injectivity reducer env injectivity =
    match injectivity with
    | Injective -> reducer.__init
    | NoInjectivity -> reducer.__init
  ;;

  let reduce_mode reducer env mode =
    match mode with
    | Mode _arg_0 -> ret_init reducer env _arg_0
  ;;

  let reduce_payload reducer env payload =
    match payload with
    | PStr _arg_0 -> reducer.structure reducer env _arg_0
    | PSig _arg_0 -> reducer.signature reducer env _arg_0
    | PTyp _arg_0 -> reducer.core_type reducer env _arg_0
    | PPat (_arg_0, _arg_1) ->
      reducer.pattern reducer env _arg_0
      |> fun x ->
      reducer.__combine
        x
        (reduce_option reducer (reducer.expression reducer) env _arg_1)
    | PString (_arg_0, _arg_1) ->
      ret_init reducer env _arg_0
      |> fun x -> reducer.__combine x (ret_init reducer env _arg_1)
  ;;

  let reduce_value_binding reducer env value_binding =
    reduce_list reducer (ret_init reducer) env value_binding.pvb_pre_text
    |> fun x ->
    reducer.__combine
      x
      (reduce_option reducer (ret_init reducer) env value_binding.pvb_pre_doc)
    |> fun x ->
    reducer.__combine
      x
      (reducer.ext_attribute reducer env value_binding.pvb_ext_attrs)
    |> fun x ->
    reducer.__combine
      x
      (reducer.modes reducer env value_binding.pvb_legacy_modes)
    |> fun x ->
    reducer.__combine x (reducer.pattern reducer env value_binding.pvb_pat)
    |> fun x ->
    reducer.__combine x (reducer.modes reducer env value_binding.pvb_modes)
    |> fun x ->
    reducer.__combine
      x
      (reduce_list
         reducer
         (reducer.function_param reducer)
         env
         value_binding.pvb_params)
    |> fun x ->
    reducer.__combine
      x
      (reduce_option
         reducer
         (reducer.value_constraint reducer)
         env
         value_binding.pvb_constraint)
    |> fun x ->
    reducer.__combine
      x
      (reduce_option
         reducer
         (reducer.expression reducer)
         env
         value_binding.pvb_expr)
    |> fun x ->
    reducer.__combine x (reducer.modes reducer env value_binding.pvb_ret_modes)
    |> fun x ->
    reducer.__combine
      x
      (reducer.attributes reducer env value_binding.pvb_attributes)
    |> fun x ->
    reducer.__combine
      x
      (reduce_option reducer (ret_init reducer) env value_binding.pvb_post_doc)
    |> fun x ->
    reducer.__combine x (reducer.location reducer env value_binding.pvb_loc)
    |> fun x ->
    reducer.__combine
      x
      (reducer.tokens_seq reducer env value_binding.pvb_tokens)
  ;;

  let reduce_unboxed_access reducer env unboxed_access =
    match unboxed_access with
    | Uaccess_unboxed_field _arg_0 ->
      reducer.location_loc reducer (reducer.longident reducer) env _arg_0
  ;;

  let reduce_functor_parameter reducer env functor_parameter =
    match functor_parameter with
    | Unit -> reducer.__init
    | Named (_arg_0, _arg_1, _arg_2) ->
      reducer.location_loc
        reducer
        (reduce_option reducer (ret_init reducer))
        env
        _arg_0
      |> fun x ->
      reducer.__combine x (reducer.module_type reducer env _arg_1)
      |> fun x -> reducer.__combine x (reducer.modes reducer env _arg_2)
    | Unnamed (_arg_0, _arg_1) ->
      reducer.module_type reducer env _arg_0
      |> fun x -> reducer.__combine x (reducer.modes reducer env _arg_1)
  ;;

  let reduce_longident_lid_desc reducer env lid_desc =
    match lid_desc with
    | Longident.Lident _arg_0 -> reducer.longident_str_or_op reducer env _arg_0
    | Longident.Ldot (_arg_0, _arg_1) ->
      reducer.longident reducer env _arg_0
      |> fun x ->
      reducer.__combine x (reducer.longident_str_or_op reducer env _arg_1)
    | Longident.Lapply (_arg_0, _arg_1) ->
      reducer.longident reducer env _arg_0
      |> fun x -> reducer.__combine x (reducer.longident reducer env _arg_1)
  ;;

  let reduce_jkind_annotation reducer env jkind_annotation =
    reducer.location reducer env jkind_annotation.pjkind_loc
    |> fun x ->
    reducer.__combine
      x
      (reducer.jkind_annotation_desc reducer env jkind_annotation.pjkind_desc)
    |> fun x ->
    reducer.__combine
      x
      (reducer.tokens_seq reducer env jkind_annotation.pjkind_tokens)
  ;;

  let reduce_longident reducer env t =
    reducer.longident_lid_desc reducer env t.Longident.desc
    |> fun x ->
    reducer.__combine x (reducer.tokens_seq reducer env t.Longident.tokens)
  ;;

  let reduce_constant reducer env constant =
    match constant with
    | Pconst_integer (_arg_0, _arg_1, _arg_2) ->
      reduce_option reducer (ret_init reducer) env _arg_0
      |> fun x ->
      reducer.__combine x (ret_init reducer env _arg_1)
      |> fun x ->
      reducer.__combine x (reduce_option reducer (ret_init reducer) env _arg_2)
    | Pconst_unboxed_integer (_arg_0, _arg_1, _arg_2) ->
      reduce_option reducer (ret_init reducer) env _arg_0
      |> fun x ->
      reducer.__combine x (ret_init reducer env _arg_1)
      |> fun x -> reducer.__combine x (ret_init reducer env _arg_2)
    | Pconst_char (_arg_0, _arg_1) ->
      ret_init reducer env _arg_0
      |> fun x -> reducer.__combine x (ret_init reducer env _arg_1)
    | Pconst_untagged_char (_arg_0, _arg_1) ->
      ret_init reducer env _arg_0
      |> fun x -> reducer.__combine x (ret_init reducer env _arg_1)
    | Pconst_string (_arg_0, _arg_1, _arg_2) ->
      ret_init reducer env _arg_0
      |> fun x ->
      reducer.__combine x (reducer.location reducer env _arg_1)
      |> fun x ->
      reducer.__combine x (reduce_option reducer (ret_init reducer) env _arg_2)
    | Pconst_float (_arg_0, _arg_1, _arg_2) ->
      reduce_option reducer (ret_init reducer) env _arg_0
      |> fun x ->
      reducer.__combine x (ret_init reducer env _arg_1)
      |> fun x ->
      reducer.__combine x (reduce_option reducer (ret_init reducer) env _arg_2)
    | Pconst_unboxed_float (_arg_0, _arg_1, _arg_2) ->
      reduce_option reducer (ret_init reducer) env _arg_0
      |> fun x ->
      reducer.__combine x (ret_init reducer env _arg_1)
      |> fun x ->
      reducer.__combine x (reduce_option reducer (ret_init reducer) env _arg_2)
  ;;

  let reduce_class_type_desc reducer env class_type_desc =
    match class_type_desc with
    | Pcty_constr (_arg_0, _arg_1) ->
      reducer.location_loc reducer (reducer.longident reducer) env _arg_0
      |> fun x ->
      reducer.__combine
        x
        (reduce_list reducer (reducer.core_type reducer) env _arg_1)
    | Pcty_signature _arg_0 -> reducer.class_signature reducer env _arg_0
    | Pcty_arrow (_arg_0, _arg_1) ->
      reducer.arrow_arg reducer env _arg_0
      |> fun x -> reducer.__combine x (reducer.class_type reducer env _arg_1)
    | Pcty_extension _arg_0 -> reducer.extension reducer env _arg_0
    | Pcty_open (_arg_0, _arg_1) ->
      reducer.open_description reducer env _arg_0
      |> fun x -> reducer.__combine x (reducer.class_type reducer env _arg_1)
  ;;

  let reduce_rec_flag reducer env rec_flag =
    match rec_flag with
    | Nonrecursive -> reducer.__init
    | Recursive -> reducer.__init
  ;;

  let reduce_longident_dotop_delims reducer env dotop_delims =
    match dotop_delims with
    | Longident.Paren -> reducer.__init
    | Longident.Brace -> reducer.__init
    | Longident.Bracket -> reducer.__init
  ;;

  let reduce_module_type_declaration reducer env module_type_declaration =
    reduce_option
      reducer
      (ret_init reducer)
      env
      module_type_declaration.pmtd_pre_doc
    |> fun x ->
    reducer.__combine
      x
      (reducer.ext_attribute reducer env module_type_declaration.pmtd_ext_attrs)
    |> fun x ->
    reducer.__combine
      x
      (reducer.location_loc
         reducer
         (ret_init reducer)
         env
         module_type_declaration.pmtd_name)
    |> fun x ->
    reducer.__combine
      x
      (reduce_option
         reducer
         (reducer.module_type reducer)
         env
         module_type_declaration.pmtd_type)
    |> fun x ->
    reducer.__combine
      x
      (reducer.attributes reducer env module_type_declaration.pmtd_attributes)
    |> fun x ->
    reducer.__combine
      x
      (reduce_option
         reducer
         (ret_init reducer)
         env
         module_type_declaration.pmtd_post_doc)
    |> fun x ->
    reducer.__combine
      x
      (reducer.location reducer env module_type_declaration.pmtd_loc)
    |> fun x ->
    reducer.__combine
      x
      (reducer.tokens_seq reducer env module_type_declaration.pmtd_tokens)
  ;;

  let reduce_tokens_desc reducer env desc =
    match desc with
    | Tokens.Token (_arg_0, _arg_1) ->
      ret_init reducer env _arg_0
      |> fun x -> reducer.__combine x (ret_init reducer env _arg_1)
    | Tokens.Comment _arg_0 -> reducer.tokens_comment reducer env _arg_0
    | Tokens.Child_node -> reducer.__init
  ;;

  let reduce_class_type_field reducer env class_type_field =
    reduce_option reducer (ret_init reducer) env class_type_field.pctf_pre_doc
    |> fun x ->
    reducer.__combine
      x
      (reducer.class_type_field_desc reducer env class_type_field.pctf_desc)
    |> fun x ->
    reducer.__combine x (reducer.location reducer env class_type_field.pctf_loc)
    |> fun x ->
    reducer.__combine
      x
      (reducer.attributes reducer env class_type_field.pctf_attributes)
    |> fun x ->
    reducer.__combine
      x
      (reduce_option
         reducer
         (ret_init reducer)
         env
         class_type_field.pctf_post_doc)
    |> fun x ->
    reducer.__combine
      x
      (reducer.tokens_seq reducer env class_type_field.pctf_tokens)
  ;;

  let reduce_letop reducer env letop =
    reducer.binding_op reducer env letop.let_
    |> fun x ->
    reducer.__combine
      x
      (reduce_list reducer (reducer.binding_op reducer) env letop.ands)
    |> fun x -> reducer.__combine x (reducer.expression reducer env letop.body)
  ;;

  let reduce_location reducer env t =
    ret_init reducer env t.Location.loc_start
    |> fun x ->
    reducer.__combine x (ret_init reducer env t.Location.loc_end)
    |> fun x -> reducer.__combine x (ret_init reducer env t.Location.loc_ghost)
  ;;

  let reduce_function_param reducer env function_param =
    reducer.location reducer env function_param.pparam_loc
    |> fun x ->
    reducer.__combine
      x
      (reducer.function_param_desc reducer env function_param.pparam_desc)
  ;;

  let reduce_ptype_params reducer env ptype_params =
    reduce_list reducer (reducer.ptype_param reducer) env ptype_params
  ;;

  let reduce_open_declaration reducer env open_declaration =
    reducer.open_infos
      reducer
      (reducer.module_expr reducer)
      env
      open_declaration
  ;;

  let reduce_jkind_annotation_desc reducer env jkind_annotation_desc =
    match jkind_annotation_desc with
    | Pjk_default -> reducer.__init
    | Pjk_abbreviation _arg_0 -> ret_init reducer env _arg_0
    | Pjk_mod (_arg_0, _arg_1) ->
      reducer.jkind_annotation reducer env _arg_0
      |> fun x -> reducer.__combine x (reducer.modes reducer env _arg_1)
    | Pjk_with (_arg_0, _arg_1, _arg_2) ->
      reducer.jkind_annotation reducer env _arg_0
      |> fun x ->
      reducer.__combine x (reducer.core_type reducer env _arg_1)
      |> fun x -> reducer.__combine x (reducer.modalities reducer env _arg_2)
    | Pjk_kind_of _arg_0 -> reducer.core_type reducer env _arg_0
    | Pjk_product _arg_0 ->
      reduce_list reducer (reducer.jkind_annotation reducer) env _arg_0
    | Pjk_parens _arg_0 -> reducer.jkind_annotation_desc reducer env _arg_0
  ;;

  let reduce_toplevel_phrase reducer env toplevel_phrase =
    match toplevel_phrase with
    | Ptop_def _arg_0 -> reducer.structure reducer env _arg_0
    | Ptop_dir _arg_0 -> reducer.toplevel_directive reducer env _arg_0
  ;;

  let reduce_arrow_arg reducer env arrow_arg =
    reducer.arg_label reducer env arrow_arg.aa_lbl
    |> fun x ->
    reducer.__combine x (reducer.modes reducer env arrow_arg.aa_legacy_modes)
    |> fun x ->
    reducer.__combine x (reducer.core_type reducer env arrow_arg.aa_type)
    |> fun x ->
    reducer.__combine x (reducer.modes reducer env arrow_arg.aa_modes)
    |> fun x ->
    reducer.__combine
      x
      (reduce_option reducer (ret_init reducer) env arrow_arg.aa_doc)
    |> fun x ->
    reducer.__combine x (reducer.location reducer env arrow_arg.aa_loc)
    |> fun x ->
    reducer.__combine x (reducer.tokens_seq reducer env arrow_arg.aa_tokens)
  ;;

  let reduce_value_constraint reducer env value_constraint =
    match value_constraint with
    | Pvc_constraint _arg ->
      reduce_list
        reducer
        (fun env __arg ->
          let _0, _1 = __arg in
          reducer.location_loc reducer (ret_init reducer) env _0
          |> fun x ->
          reducer.__combine
            x
            (reduce_option reducer (reducer.jkind_annotation reducer) env _1))
        env
        _arg.locally_abstract_univars
      |> fun x -> reducer.__combine x (reducer.core_type reducer env _arg.typ)
    | Pvc_coercion _arg ->
      reduce_option reducer (reducer.core_type reducer) env _arg.ground
      |> fun x ->
      reducer.__combine x (reducer.core_type reducer env _arg.coercion)
  ;;

  let reduce_paren_kind reducer env paren_kind =
    match paren_kind with
    | Paren -> reducer.__init
    | Brace -> reducer.__init
    | Bracket -> reducer.__init
  ;;

  let reduce_case reducer env case =
    reducer.pattern reducer env case.pc_lhs
    |> fun x ->
    reducer.__combine
      x
      (reduce_option reducer (reducer.expression reducer) env case.pc_guard)
    |> fun x ->
    reducer.__combine x (reducer.expression reducer env case.pc_rhs)
    |> fun x ->
    reducer.__combine x (reducer.tokens_seq reducer env case.pc_tokens)
  ;;

  let reduce_type_extension reducer env type_extension =
    reduce_option reducer (ret_init reducer) env type_extension.ptyext_pre_doc
    |> fun x ->
    reducer.__combine
      x
      (reducer.ext_attribute reducer env type_extension.ptyext_ext_attrs)
    |> fun x ->
    reducer.__combine
      x
      (reducer.location_loc
         reducer
         (reducer.longident reducer)
         env
         type_extension.ptyext_path)
    |> fun x ->
    reducer.__combine
      x
      (reduce_list
         reducer
         (reducer.ptype_param reducer)
         env
         type_extension.ptyext_params)
    |> fun x ->
    reducer.__combine
      x
      (reduce_list
         reducer
         (reducer.extension_constructor reducer)
         env
         type_extension.ptyext_constructors)
    |> fun x ->
    reducer.__combine
      x
      (reducer.private_flag reducer env type_extension.ptyext_private)
    |> fun x ->
    reducer.__combine x (reducer.location reducer env type_extension.ptyext_loc)
    |> fun x ->
    reducer.__combine
      x
      (reducer.attributes reducer env type_extension.ptyext_attributes)
    |> fun x ->
    reducer.__combine
      x
      (reduce_option
         reducer
         (ret_init reducer)
         env
         type_extension.ptyext_post_doc)
    |> fun x ->
    reducer.__combine
      x
      (reducer.tokens_seq reducer env type_extension.ptyext_tokens)
  ;;

  let reduce_signature_item reducer env signature_item =
    reducer.signature_item_desc reducer env signature_item.psig_desc
    |> fun x ->
    reducer.__combine x (reducer.location reducer env signature_item.psig_loc)
    |> fun x ->
    reducer.__combine
      x
      (reducer.tokens_seq reducer env signature_item.psig_tokens)
  ;;

  let reduce_closed_flag reducer env closed_flag =
    match closed_flag with
    | Closed -> reducer.__init
    | Open -> reducer.__init
  ;;

  let reduce_type_kind reducer env type_kind =
    match type_kind with
    | Ptype_abstract -> reducer.__init
    | Ptype_variant _arg_0 ->
      reduce_list reducer (reducer.constructor_declaration reducer) env _arg_0
    | Ptype_record _arg_0 ->
      reduce_list reducer (reducer.label_declaration reducer) env _arg_0
    | Ptype_record_unboxed_product _arg_0 ->
      reduce_list reducer (reducer.label_declaration reducer) env _arg_0
    | Ptype_open -> reducer.__init
  ;;

  let reduce_include_infos reducer reduce_'a env include_infos =
    reduce_option reducer (ret_init reducer) env include_infos.pincl_pre_doc
    |> fun x ->
    reducer.__combine
      x
      (reducer.include_kind reducer env include_infos.pincl_kind)
    |> fun x ->
    reducer.__combine
      x
      (reducer.ext_attribute reducer env include_infos.pincl_ext_attrs)
    |> fun x ->
    reducer.__combine x (reduce_'a env include_infos.pincl_mod)
    |> fun x ->
    reducer.__combine x (reducer.location reducer env include_infos.pincl_loc)
    |> fun x ->
    reducer.__combine
      x
      (reducer.attributes reducer env include_infos.pincl_attributes)
    |> fun x ->
    reducer.__combine
      x
      (reduce_option reducer (ret_init reducer) env include_infos.pincl_post_doc)
    |> fun x ->
    reducer.__combine
      x
      (reducer.tokens_seq reducer env include_infos.pincl_tokens)
  ;;

  let reduce_include_kind reducer env include_kind =
    match include_kind with
    | Structure -> reducer.__init
    | Functor -> reducer.__init
  ;;

  let reduce_ext_attribute reducer env ext_attribute =
    reduce_option
      reducer
      (reducer.location_loc reducer (reduce_list reducer (ret_init reducer)))
      env
      ext_attribute.pea_ext
    |> fun x ->
    reducer.__combine x (reducer.attributes reducer env ext_attribute.pea_attrs)
  ;;

  let reduce_value_description reducer env value_description =
    reduce_option reducer (ret_init reducer) env value_description.pval_pre_doc
    |> fun x ->
    reducer.__combine
      x
      (reducer.ext_attribute reducer env value_description.pval_ext_attrs)
    |> fun x ->
    reducer.__combine
      x
      (reducer.location_loc
         reducer
         (reducer.longident_str_or_op reducer)
         env
         value_description.pval_name)
    |> fun x ->
    reducer.__combine
      x
      (reducer.core_type reducer env value_description.pval_type)
    |> fun x ->
    reducer.__combine
      x
      (reducer.modalities reducer env value_description.pval_modalities)
    |> fun x ->
    reducer.__combine
      x
      (reduce_list reducer (ret_init reducer) env value_description.pval_prim)
    |> fun x ->
    reducer.__combine
      x
      (reducer.attributes reducer env value_description.pval_attributes)
    |> fun x ->
    reducer.__combine
      x
      (reduce_option
         reducer
         (ret_init reducer)
         env
         value_description.pval_post_doc)
    |> fun x ->
    reducer.__combine
      x
      (reducer.location reducer env value_description.pval_loc)
    |> fun x ->
    reducer.__combine
      x
      (reducer.tokens_seq reducer env value_description.pval_tokens)
  ;;

  let reduce_function_body reducer env function_body =
    reducer.function_body_desc reducer env function_body.pfb_desc
    |> fun x ->
    reducer.__combine x (reducer.location reducer env function_body.pfb_loc)
    |> fun x ->
    reducer.__combine
      x
      (reducer.tokens_seq reducer env function_body.pfb_tokens)
  ;;

  let reduce_class_field reducer env class_field =
    reduce_option reducer (ret_init reducer) env class_field.pcf_pre_doc
    |> fun x ->
    reducer.__combine
      x
      (reducer.class_field_desc reducer env class_field.pcf_desc)
    |> fun x ->
    reducer.__combine x (reducer.location reducer env class_field.pcf_loc)
    |> fun x ->
    reducer.__combine
      x
      (reducer.attributes reducer env class_field.pcf_attributes)
    |> fun x ->
    reducer.__combine
      x
      (reduce_option reducer (ret_init reducer) env class_field.pcf_post_doc)
    |> fun x ->
    reducer.__combine x (reducer.tokens_seq reducer env class_field.pcf_tokens)
  ;;

  let reduce_include_declaration reducer env include_declaration =
    reducer.include_infos
      reducer
      (reducer.module_expr reducer)
      env
      include_declaration
  ;;

  let reduce_attributes reducer env attributes =
    reduce_list reducer (reducer.attribute reducer) env attributes
  ;;

  let reduce_class_type_declaration reducer env class_type_declaration =
    reducer.class_infos
      reducer
      (reducer.class_type reducer)
      env
      class_type_declaration
  ;;

  let reduce_open_description reducer env open_description =
    reducer.open_infos
      reducer
      (reducer.location_loc reducer (reducer.longident reducer))
      env
      open_description
  ;;

  let reduce_pattern_desc reducer env pattern_desc =
    match pattern_desc with
    | Ppat_any -> reducer.__init
    | Ppat_var _arg_0 ->
      reducer.location_loc
        reducer
        (reducer.longident_str_or_op reducer)
        env
        _arg_0
    | Ppat_alias (_arg_0, _arg_1) ->
      reducer.pattern reducer env _arg_0
      |> fun x ->
      reducer.__combine
        x
        (reducer.location_loc
           reducer
           (reducer.longident_str_or_op reducer)
           env
           _arg_1)
    | Ppat_constant _arg_0 -> reducer.constant reducer env _arg_0
    | Ppat_interval (_arg_0, _arg_1) ->
      reducer.constant reducer env _arg_0
      |> fun x -> reducer.__combine x (reducer.constant reducer env _arg_1)
    | Ppat_tuple (_arg_0, _arg_1) ->
      reduce_list
        reducer
        (reducer.argument reducer (reducer.pattern reducer))
        env
        _arg_0
      |> fun x -> reducer.__combine x (reducer.closed_flag reducer env _arg_1)
    | Ppat_unboxed_tuple (_arg_0, _arg_1) ->
      reduce_list
        reducer
        (reducer.argument reducer (reducer.pattern reducer))
        env
        _arg_0
      |> fun x -> reducer.__combine x (reducer.closed_flag reducer env _arg_1)
    | Ppat_construct (_arg_0, _arg_1) ->
      reducer.location_loc reducer (reducer.longident reducer) env _arg_0
      |> fun x ->
      reducer.__combine
        x
        (reduce_option
           reducer
           (fun env __arg ->
             let _0, _1 = __arg in
             reduce_list
               reducer
               (fun env __arg ->
                 let _0, _1 = __arg in
                 reducer.location_loc reducer (ret_init reducer) env _0
                 |> fun x ->
                 reducer.__combine
                   x
                   (reduce_option
                      reducer
                      (reducer.jkind_annotation reducer)
                      env
                      _1))
               env
               _0
             |> fun x -> reducer.__combine x (reducer.pattern reducer env _1))
           env
           _arg_1)
    | Ppat_variant (_arg_0, _arg_1) ->
      reducer.label reducer env _arg_0
      |> fun x ->
      reducer.__combine
        x
        (reduce_option reducer (reducer.pattern reducer) env _arg_1)
    | Ppat_record (_arg_0, _arg_1) ->
      reduce_list
        reducer
        (reducer.record_field reducer (reducer.pattern reducer))
        env
        _arg_0
      |> fun x -> reducer.__combine x (reducer.closed_flag reducer env _arg_1)
    | Ppat_record_unboxed_product (_arg_0, _arg_1) ->
      reduce_list
        reducer
        (reducer.record_field reducer (reducer.pattern reducer))
        env
        _arg_0
      |> fun x -> reducer.__combine x (reducer.closed_flag reducer env _arg_1)
    | Ppat_array (_arg_0, _arg_1) ->
      reducer.mutable_flag reducer env _arg_0
      |> fun x ->
      reducer.__combine
        x
        (reduce_list reducer (reducer.pattern reducer) env _arg_1)
    | Ppat_or (_arg_0, _arg_1) ->
      reducer.pattern reducer env _arg_0
      |> fun x -> reducer.__combine x (reducer.pattern reducer env _arg_1)
    | Ppat_constraint (_arg_0, _arg_1, _arg_2) ->
      reducer.pattern reducer env _arg_0
      |> fun x ->
      reducer.__combine
        x
        (reduce_option reducer (reducer.core_type reducer) env _arg_1)
      |> fun x -> reducer.__combine x (reducer.modes reducer env _arg_2)
    | Ppat_type _arg_0 ->
      reducer.location_loc reducer (reducer.longident reducer) env _arg_0
    | Ppat_lazy _arg_0 -> reducer.pattern reducer env _arg_0
    | Ppat_unpack (_arg_0, _arg_1) ->
      reducer.location_loc
        reducer
        (reduce_option reducer (ret_init reducer))
        env
        _arg_0
      |> fun x ->
      reducer.__combine
        x
        (reduce_option reducer (reducer.package_type reducer) env _arg_1)
    | Ppat_exception _arg_0 -> reducer.pattern reducer env _arg_0
    | Ppat_extension _arg_0 -> reducer.extension reducer env _arg_0
    | Ppat_open (_arg_0, _arg_1) ->
      reducer.location_loc reducer (reducer.longident reducer) env _arg_0
      |> fun x -> reducer.__combine x (reducer.pattern reducer env _arg_1)
    | Ppat_parens _arg ->
      reducer.pattern reducer env _arg.pat
      |> fun x -> reducer.__combine x (ret_init reducer env _arg.optional)
    | Ppat_list _arg_0 ->
      reduce_list reducer (reducer.pattern reducer) env _arg_0
    | Ppat_cons (_arg_0, _arg_1) ->
      reducer.pattern reducer env _arg_0
      |> fun x -> reducer.__combine x (reducer.pattern reducer env _arg_1)
  ;;

  let reduce_tokens_attachment reducer env attachment =
    match attachment with
    | Tokens.Before -> reducer.__init
    | Tokens.After -> reducer.__init
    | Tokens.Floating -> reducer.__init
  ;;

  let reduce_module_expr reducer env module_expr =
    reducer.module_expr_desc reducer env module_expr.pmod_desc
    |> fun x ->
    reducer.__combine x (reducer.location reducer env module_expr.pmod_loc)
    |> fun x ->
    reducer.__combine
      x
      (reducer.attributes reducer env module_expr.pmod_attributes)
    |> fun x ->
    reducer.__combine x (reducer.tokens_seq reducer env module_expr.pmod_tokens)
  ;;

  let reduce_module_binding reducer env module_binding =
    reduce_list reducer (ret_init reducer) env module_binding.pmb_pre_text
    |> fun x ->
    reducer.__combine
      x
      (reduce_option reducer (ret_init reducer) env module_binding.pmb_pre_doc)
    |> fun x ->
    reducer.__combine
      x
      (reducer.ext_attribute reducer env module_binding.pmb_ext_attrs)
    |> fun x ->
    reducer.__combine
      x
      (let _0, _1 = module_binding.pmb_name in
       reducer.location_loc
         reducer
         (reduce_option reducer (ret_init reducer))
         env
         _0
       |> fun x -> reducer.__combine x (reducer.modes reducer env _1))
    |> fun x ->
    reducer.__combine
      x
      (reduce_list
         reducer
         (reducer.functor_parameter reducer)
         env
         module_binding.pmb_params)
    |> fun x ->
    reducer.__combine
      x
      (reduce_option
         reducer
         (reducer.module_type reducer)
         env
         module_binding.pmb_constraint)
    |> fun x ->
    reducer.__combine x (reducer.modes reducer env module_binding.pmb_modes)
    |> fun x ->
    reducer.__combine
      x
      (reducer.module_expr reducer env module_binding.pmb_expr)
    |> fun x ->
    reducer.__combine
      x
      (reducer.attributes reducer env module_binding.pmb_attributes)
    |> fun x ->
    reducer.__combine
      x
      (reduce_option reducer (ret_init reducer) env module_binding.pmb_post_doc)
    |> fun x ->
    reducer.__combine x (reducer.location reducer env module_binding.pmb_loc)
    |> fun x ->
    reducer.__combine
      x
      (reducer.tokens_seq reducer env module_binding.pmb_tokens)
  ;;

  let reduce_location_loc reducer reduce_'a env loc =
    reduce_'a env loc.Location.txt
    |> fun x ->
    reducer.__combine x (reducer.location reducer env loc.Location.loc)
  ;;

  let reduce_argument_desc reducer reduce_'a env argument_desc =
    match argument_desc with
    | Parg_unlabelled _arg ->
      reducer.modes reducer env _arg.legacy_modes
      |> fun x ->
      reducer.__combine x (reduce_'a env _arg.arg)
      |> fun x ->
      reducer.__combine
        x
        (reduce_option
           reducer
           (reducer.type_constraint reducer)
           env
           _arg.typ_constraint)
      |> fun x -> reducer.__combine x (reducer.modes reducer env _arg.modes)
    | Parg_labelled _arg ->
      ret_init reducer env _arg.optional
      |> fun x ->
      reducer.__combine x (reducer.modes reducer env _arg.legacy_modes)
      |> fun x ->
      reducer.__combine x (ret_init reducer env _arg.name)
      |> fun x ->
      reducer.__combine
        x
        (reduce_option reducer reduce_'a env _arg.maybe_punned)
      |> fun x ->
      reducer.__combine
        x
        (reduce_option
           reducer
           (reducer.type_constraint reducer)
           env
           _arg.typ_constraint)
      |> fun x ->
      reducer.__combine x (reducer.modes reducer env _arg.modes)
      |> fun x ->
      reducer.__combine
        x
        (reduce_option reducer (reducer.expression reducer) env _arg.default)
  ;;

  let reduce_expression reducer env expression =
    reducer.ext_attribute reducer env expression.pexp_ext_attr
    |> fun x ->
    reducer.__combine
      x
      (reducer.expression_desc reducer env expression.pexp_desc)
    |> fun x ->
    reducer.__combine x (reducer.location reducer env expression.pexp_loc)
    |> fun x ->
    reducer.__combine
      x
      (reducer.attributes reducer env expression.pexp_attributes)
    |> fun x ->
    reducer.__combine x (reducer.tokens_seq reducer env expression.pexp_tokens)
  ;;

  let reduce_comprehension reducer env comprehension =
    reducer.expression reducer env comprehension.pcomp_body
    |> fun x ->
    reducer.__combine
      x
      (reduce_list
         reducer
         (reducer.comprehension_clause reducer)
         env
         comprehension.pcomp_clauses)
    |> fun x ->
    reducer.__combine
      x
      (reducer.tokens_seq reducer env comprehension.pcomp_tokens)
  ;;

  let reduce_type_declaration reducer env type_declaration =
    reduce_list reducer (ret_init reducer) env type_declaration.ptype_pre_text
    |> fun x ->
    reducer.__combine
      x
      (reduce_option
         reducer
         (ret_init reducer)
         env
         type_declaration.ptype_pre_doc)
    |> fun x ->
    reducer.__combine
      x
      (reducer.ext_attribute reducer env type_declaration.ptype_ext_attrs)
    |> fun x ->
    reducer.__combine
      x
      (reducer.location_loc
         reducer
         (ret_init reducer)
         env
         type_declaration.ptype_name)
    |> fun x ->
    reducer.__combine
      x
      (reducer.ptype_params reducer env type_declaration.ptype_params)
    |> fun x ->
    reducer.__combine
      x
      (reduce_option
         reducer
         (reducer.jkind_annotation reducer)
         env
         type_declaration.ptype_jkind_annotation)
    |> fun x ->
    reducer.__combine
      x
      (reducer.private_flag reducer env type_declaration.ptype_private)
    |> fun x ->
    reducer.__combine
      x
      (reduce_option
         reducer
         (reducer.core_type reducer)
         env
         type_declaration.ptype_manifest)
    |> fun x ->
    reducer.__combine
      x
      (reducer.type_kind reducer env type_declaration.ptype_kind)
    |> fun x ->
    reducer.__combine
      x
      (reduce_list
         reducer
         (reducer.ptype_constraint reducer)
         env
         type_declaration.ptype_cstrs)
    |> fun x ->
    reducer.__combine
      x
      (reducer.attributes reducer env type_declaration.ptype_attributes)
    |> fun x ->
    reducer.__combine
      x
      (reduce_option
         reducer
         (ret_init reducer)
         env
         type_declaration.ptype_post_doc)
    |> fun x ->
    reducer.__combine
      x
      (reducer.location reducer env type_declaration.ptype_loc)
    |> fun x ->
    reducer.__combine
      x
      (reducer.tokens_seq reducer env type_declaration.ptype_tokens)
  ;;

  let reduce_function_param_desc reducer env function_param_desc =
    match function_param_desc with
    | Pparam_val _arg_0 ->
      reducer.argument reducer (reducer.pattern reducer) env _arg_0
    | Pparam_newtype (_arg_0, _arg_1) ->
      reducer.location_loc reducer (ret_init reducer) env _arg_0
      |> fun x ->
      reducer.__combine
        x
        (reduce_option reducer (reducer.jkind_annotation reducer) env _arg_1)
    | Pparam_newtypes _arg_0 ->
      reduce_list
        reducer
        (fun env __arg ->
          let _0, _1 = __arg in
          reducer.location_loc reducer (ret_init reducer) env _0
          |> fun x ->
          reducer.__combine
            x
            (reduce_option reducer (reducer.jkind_annotation reducer) env _1))
        env
        _arg_0
  ;;

  let reduce_function_body_desc reducer env function_body_desc =
    match function_body_desc with
    | Pfunction_body _arg_0 -> reducer.expression reducer env _arg_0
    | Pfunction_cases (_arg_0, _arg_1) ->
      reduce_list reducer (reducer.case reducer) env _arg_0
      |> fun x -> reducer.__combine x (reducer.ext_attribute reducer env _arg_1)
  ;;

  let reduce_with_constraint reducer env with_constraint =
    reducer.with_constraint_desc reducer env with_constraint.wc_desc
    |> fun x ->
    reducer.__combine x (reducer.location reducer env with_constraint.wc_loc)
    |> fun x ->
    reducer.__combine
      x
      (reducer.tokens_seq reducer env with_constraint.wc_tokens)
  ;;

  let reduce_with_constraint_desc reducer env with_constraint_desc =
    match with_constraint_desc with
    | Pwith_type (_arg_0, _arg_1, _arg_2, _arg_3, _arg_4) ->
      reducer.ptype_params reducer env _arg_0
      |> fun x ->
      reducer.__combine
        x
        (reducer.location_loc reducer (reducer.longident reducer) env _arg_1)
      |> fun x ->
      reducer.__combine x (reducer.private_flag reducer env _arg_2)
      |> fun x ->
      reducer.__combine x (reducer.core_type reducer env _arg_3)
      |> fun x ->
      reducer.__combine
        x
        (reduce_list reducer (reducer.ptype_constraint reducer) env _arg_4)
    | Pwith_module (_arg_0, _arg_1) ->
      reducer.location_loc reducer (reducer.longident reducer) env _arg_0
      |> fun x ->
      reducer.__combine
        x
        (reducer.location_loc reducer (reducer.longident reducer) env _arg_1)
    | Pwith_modtype (_arg_0, _arg_1) ->
      reducer.location_loc reducer (reducer.longident reducer) env _arg_0
      |> fun x -> reducer.__combine x (reducer.module_type reducer env _arg_1)
    | Pwith_modtypesubst (_arg_0, _arg_1) ->
      reducer.location_loc reducer (reducer.longident reducer) env _arg_0
      |> fun x -> reducer.__combine x (reducer.module_type reducer env _arg_1)
    | Pwith_typesubst (_arg_0, _arg_1, _arg_2) ->
      reducer.ptype_params reducer env _arg_0
      |> fun x ->
      reducer.__combine
        x
        (reducer.location_loc reducer (reducer.longident reducer) env _arg_1)
      |> fun x -> reducer.__combine x (reducer.core_type reducer env _arg_2)
    | Pwith_modsubst (_arg_0, _arg_1) ->
      reducer.location_loc reducer (reducer.longident reducer) env _arg_0
      |> fun x ->
      reducer.__combine
        x
        (reducer.location_loc reducer (reducer.longident reducer) env _arg_1)
  ;;

  let reduce_directive_argument_desc reducer env directive_argument_desc =
    match directive_argument_desc with
    | Pdir_string _arg_0 -> ret_init reducer env _arg_0
    | Pdir_int (_arg_0, _arg_1) ->
      ret_init reducer env _arg_0
      |> fun x ->
      reducer.__combine x (reduce_option reducer (ret_init reducer) env _arg_1)
    | Pdir_ident _arg_0 -> reducer.longident reducer env _arg_0
    | Pdir_bool _arg_0 -> ret_init reducer env _arg_0
  ;;

  let reduce_attribute reducer env attribute =
    reducer.location_loc
      reducer
      (reduce_list reducer (ret_init reducer))
      env
      attribute.attr_name
    |> fun x ->
    reducer.__combine x (reducer.payload reducer env attribute.attr_payload)
    |> fun x ->
    reducer.__combine x (reducer.location reducer env attribute.attr_loc)
    |> fun x ->
    reducer.__combine x (reducer.tokens_seq reducer env attribute.attr_tokens)
  ;;

  let reduce_comprehension_expression reducer env comprehension_expression =
    match comprehension_expression with
    | Pcomp_list_comprehension _arg_0 ->
      reducer.comprehension reducer env _arg_0
    | Pcomp_array_comprehension (_arg_0, _arg_1) ->
      reducer.mutable_flag reducer env _arg_0
      |> fun x -> reducer.__combine x (reducer.comprehension reducer env _arg_1)
  ;;

  let reduce_private_flag reducer env private_flag =
    match private_flag with
    | Private -> reducer.__init
    | Public -> reducer.__init
  ;;

  let reduce_class_structure reducer env class_structure =
    reducer.pattern reducer env class_structure.pcstr_self
    |> fun x ->
    reducer.__combine
      x
      (reduce_list
         reducer
         (reducer.class_field reducer)
         env
         class_structure.pcstr_fields)
  ;;

  let reduce_comprehension_iterator reducer env comprehension_iterator =
    match comprehension_iterator with
    | Pcomp_range _arg ->
      reducer.expression reducer env _arg.start
      |> fun x ->
      reducer.__combine x (reducer.expression reducer env _arg.stop)
      |> fun x ->
      reducer.__combine x (reducer.direction_flag reducer env _arg.direction)
    | Pcomp_in _arg_0 -> reducer.expression reducer env _arg_0
  ;;

  let reduce_index_kind reducer env index_kind =
    match index_kind with
    | Index_int -> reducer.__init
    | Index_unboxed_int64 -> reducer.__init
    | Index_unboxed_int32 -> reducer.__init
    | Index_unboxed_int16 -> reducer.__init
    | Index_unboxed_int8 -> reducer.__init
    | Index_unboxed_nativeint -> reducer.__init
  ;;

  let reduce_class_declaration reducer env class_declaration =
    reducer.class_infos
      reducer
      (reducer.class_expr reducer)
      env
      class_declaration
  ;;

  let reduce_class_type_field_desc reducer env class_type_field_desc =
    match class_type_field_desc with
    | Pctf_inherit _arg_0 -> reducer.class_type reducer env _arg_0
    | Pctf_val _arg_0 ->
      let _0, _1, _2, _3 = _arg_0 in
      reducer.location_loc reducer (ret_init reducer) env _0
      |> fun x ->
      reducer.__combine x (reducer.mutable_flag reducer env _1)
      |> fun x ->
      reducer.__combine x (reducer.virtual_flag reducer env _2)
      |> fun x -> reducer.__combine x (reducer.core_type reducer env _3)
    | Pctf_method _arg_0 ->
      let _0, _1, _2, _3 = _arg_0 in
      reducer.location_loc reducer (ret_init reducer) env _0
      |> fun x ->
      reducer.__combine x (reducer.private_flag reducer env _1)
      |> fun x ->
      reducer.__combine x (reducer.virtual_flag reducer env _2)
      |> fun x -> reducer.__combine x (reducer.core_type reducer env _3)
    | Pctf_constraint _arg_0 ->
      let _0, _1 = _arg_0 in
      reducer.core_type reducer env _0
      |> fun x -> reducer.__combine x (reducer.core_type reducer env _1)
    | Pctf_attribute _arg_0 -> reducer.attribute reducer env _arg_0
    | Pctf_extension _arg_0 -> reducer.extension reducer env _arg_0
    | Pctf_docstring _arg_0 -> ret_init reducer env _arg_0
  ;;

  let reduce_extension_constructor reducer env extension_constructor =
    reducer.location_loc
      reducer
      (reducer.longident_str_or_op reducer)
      env
      extension_constructor.pext_name
    |> fun x ->
    reducer.__combine
      x
      (reducer.extension_constructor_kind
         reducer
         env
         extension_constructor.pext_kind)
    |> fun x ->
    reducer.__combine
      x
      (reducer.location reducer env extension_constructor.pext_loc)
    |> fun x ->
    reducer.__combine
      x
      (reducer.attributes reducer env extension_constructor.pext_attributes)
    |> fun x ->
    reducer.__combine
      x
      (reduce_option
         reducer
         (ret_init reducer)
         env
         extension_constructor.pext_doc)
    |> fun x ->
    reducer.__combine
      x
      (reducer.tokens_seq reducer env extension_constructor.pext_tokens)
  ;;
end [@ocaml.warning "-27"]

let mk_default_reducer __init __combine =
  { __init
  ; __combine
  ; class_expr_desc = reduce_class_expr_desc
  ; modes = reduce_modes
  ; package_type = reduce_package_type
  ; class_signature = reduce_class_signature
  ; tokens_comment = reduce_tokens_comment
  ; directive_argument = reduce_directive_argument
  ; pattern = reduce_pattern
  ; constructor_declaration = reduce_constructor_declaration
  ; module_expr_desc = reduce_module_expr_desc
  ; structure_item = reduce_structure_item
  ; override_flag = reduce_override_flag
  ; extension = reduce_extension
  ; class_description = reduce_class_description
  ; core_type = reduce_core_type
  ; function_constraint = reduce_function_constraint
  ; extension_constructor_kind = reduce_extension_constructor_kind
  ; structure = reduce_structure
  ; toplevel_directive = reduce_toplevel_directive
  ; module_declaration_body = reduce_module_declaration_body
  ; module_declaration = reduce_module_declaration
  ; variance = reduce_variance
  ; signature_item_desc = reduce_signature_item_desc
  ; binding_op = reduce_binding_op
  ; record_field = reduce_record_field
  ; arg_label = reduce_arg_label
  ; comprehension_clause = reduce_comprehension_clause
  ; class_field_desc = reduce_class_field_desc
  ; longident_str_or_op = reduce_longident_str_or_op
  ; core_type_desc = reduce_core_type_desc
  ; ptype_param = reduce_ptype_param
  ; virtual_flag = reduce_virtual_flag
  ; module_type_desc = reduce_module_type_desc
  ; row_field = reduce_row_field
  ; class_type = reduce_class_type
  ; include_description = reduce_include_description
  ; modality = reduce_modality
  ; object_field = reduce_object_field
  ; structure_item_desc = reduce_structure_item_desc
  ; row_field_desc = reduce_row_field_desc
  ; type_exception = reduce_type_exception
  ; mutable_flag = reduce_mutable_flag
  ; direction_flag = reduce_direction_flag
  ; block_access = reduce_block_access
  ; constructor_argument = reduce_constructor_argument
  ; class_expr = reduce_class_expr
  ; label_declaration = reduce_label_declaration
  ; signature = reduce_signature
  ; tokens_seq = reduce_tokens_seq
  ; class_field_kind = reduce_class_field_kind
  ; toplevel_extension = reduce_toplevel_extension
  ; ptype_constraint = reduce_ptype_constraint
  ; type_constraint = reduce_type_constraint
  ; comprehension_clause_binding = reduce_comprehension_clause_binding
  ; tokens_elt = reduce_tokens_elt
  ; object_field_desc = reduce_object_field_desc
  ; expression_desc = reduce_expression_desc
  ; modalities = reduce_modalities
  ; class_infos = reduce_class_infos
  ; argument = reduce_argument
  ; module_substitution = reduce_module_substitution
  ; label = reduce_label
  ; open_infos = reduce_open_infos
  ; constructor_arguments = reduce_constructor_arguments
  ; module_type = reduce_module_type
  ; injectivity = reduce_injectivity
  ; mode = reduce_mode
  ; payload = reduce_payload
  ; value_binding = reduce_value_binding
  ; unboxed_access = reduce_unboxed_access
  ; functor_parameter = reduce_functor_parameter
  ; longident_lid_desc = reduce_longident_lid_desc
  ; jkind_annotation = reduce_jkind_annotation
  ; longident = reduce_longident
  ; constant = reduce_constant
  ; class_type_desc = reduce_class_type_desc
  ; rec_flag = reduce_rec_flag
  ; longident_dotop_delims = reduce_longident_dotop_delims
  ; module_type_declaration = reduce_module_type_declaration
  ; tokens_desc = reduce_tokens_desc
  ; class_type_field = reduce_class_type_field
  ; letop = reduce_letop
  ; location = reduce_location
  ; function_param = reduce_function_param
  ; ptype_params = reduce_ptype_params
  ; open_declaration = reduce_open_declaration
  ; jkind_annotation_desc = reduce_jkind_annotation_desc
  ; toplevel_phrase = reduce_toplevel_phrase
  ; arrow_arg = reduce_arrow_arg
  ; value_constraint = reduce_value_constraint
  ; paren_kind = reduce_paren_kind
  ; case = reduce_case
  ; type_extension = reduce_type_extension
  ; signature_item = reduce_signature_item
  ; closed_flag = reduce_closed_flag
  ; type_kind = reduce_type_kind
  ; include_infos = reduce_include_infos
  ; include_kind = reduce_include_kind
  ; ext_attribute = reduce_ext_attribute
  ; value_description = reduce_value_description
  ; function_body = reduce_function_body
  ; class_field = reduce_class_field
  ; include_declaration = reduce_include_declaration
  ; attributes = reduce_attributes
  ; class_type_declaration = reduce_class_type_declaration
  ; open_description = reduce_open_description
  ; pattern_desc = reduce_pattern_desc
  ; tokens_attachment = reduce_tokens_attachment
  ; module_expr = reduce_module_expr
  ; module_binding = reduce_module_binding
  ; location_loc = reduce_location_loc
  ; argument_desc = reduce_argument_desc
  ; expression = reduce_expression
  ; comprehension = reduce_comprehension
  ; type_declaration = reduce_type_declaration
  ; function_param_desc = reduce_function_param_desc
  ; function_body_desc = reduce_function_body_desc
  ; with_constraint = reduce_with_constraint
  ; with_constraint_desc = reduce_with_constraint_desc
  ; directive_argument_desc = reduce_directive_argument_desc
  ; attribute = reduce_attribute
  ; comprehension_expression = reduce_comprehension_expression
  ; private_flag = reduce_private_flag
  ; class_structure = reduce_class_structure
  ; comprehension_iterator = reduce_comprehension_iterator
  ; index_kind = reduce_index_kind
  ; class_declaration = reduce_class_declaration
  ; class_type_field_desc = reduce_class_type_field_desc
  ; extension_constructor = reduce_extension_constructor
  }
;;
