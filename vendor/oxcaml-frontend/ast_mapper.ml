open Parsetree
open Asttypes

type 'mapper_env mapper =
  { class_expr_desc :
      'mapper_env mapper -> 'mapper_env -> class_expr_desc -> class_expr_desc
  ; injectivity :
      'mapper_env mapper -> 'mapper_env -> injectivity -> injectivity
  ; mode : 'mapper_env mapper -> 'mapper_env -> mode -> mode
  ; modes : 'mapper_env mapper -> 'mapper_env -> modes -> modes
  ; package_type :
      'mapper_env mapper -> 'mapper_env -> package_type -> package_type
  ; class_signature :
      'mapper_env mapper -> 'mapper_env -> class_signature -> class_signature
  ; payload : 'mapper_env mapper -> 'mapper_env -> payload -> payload
  ; directive_argument :
      'mapper_env mapper
      -> 'mapper_env
      -> directive_argument
      -> directive_argument
  ; value_binding :
      'mapper_env mapper -> 'mapper_env -> value_binding -> value_binding
  ; pattern : 'mapper_env mapper -> 'mapper_env -> pattern -> pattern
  ; unboxed_access :
      'mapper_env mapper -> 'mapper_env -> unboxed_access -> unboxed_access
  ; functor_parameter :
      'mapper_env mapper
      -> 'mapper_env
      -> functor_parameter
      -> functor_parameter
  ; constructor_declaration :
      'mapper_env mapper
      -> 'mapper_env
      -> constructor_declaration
      -> constructor_declaration
  ; jkind_annotation :
      'mapper_env mapper -> 'mapper_env -> jkind_annotation -> jkind_annotation
  ; longident : 'mapper_env mapper -> 'mapper_env -> Longident.t -> Longident.t
  ; location_msg :
      'mapper_env mapper -> 'mapper_env -> Location.msg -> Location.msg
  ; location_error :
      'mapper_env mapper -> 'mapper_env -> Location.error -> Location.error
  ; constant : 'mapper_env mapper -> 'mapper_env -> constant -> constant
  ; module_expr_desc :
      'mapper_env mapper -> 'mapper_env -> module_expr_desc -> module_expr_desc
  ; structure_item :
      'mapper_env mapper -> 'mapper_env -> structure_item -> structure_item
  ; override_flag :
      'mapper_env mapper -> 'mapper_env -> override_flag -> override_flag
  ; class_type_desc :
      'mapper_env mapper -> 'mapper_env -> class_type_desc -> class_type_desc
  ; extension : 'mapper_env mapper -> 'mapper_env -> extension -> extension
  ; rec_flag : 'mapper_env mapper -> 'mapper_env -> rec_flag -> rec_flag
  ; class_description :
      'mapper_env mapper
      -> 'mapper_env
      -> class_description
      -> class_description
  ; core_type : 'mapper_env mapper -> 'mapper_env -> core_type -> core_type
  ; function_constraint :
      'mapper_env mapper
      -> 'mapper_env
      -> function_constraint
      -> function_constraint
  ; extension_constructor_kind :
      'mapper_env mapper
      -> 'mapper_env
      -> extension_constructor_kind
      -> extension_constructor_kind
  ; module_instance :
      'mapper_env mapper -> 'mapper_env -> module_instance -> module_instance
  ; structure : 'mapper_env mapper -> 'mapper_env -> structure -> structure
  ; toplevel_directive :
      'mapper_env mapper
      -> 'mapper_env
      -> toplevel_directive
      -> toplevel_directive
  ; module_declaration :
      'mapper_env mapper
      -> 'mapper_env
      -> module_declaration
      -> module_declaration
  ; module_type_declaration :
      'mapper_env mapper
      -> 'mapper_env
      -> module_type_declaration
      -> module_type_declaration
  ; variance : 'mapper_env mapper -> 'mapper_env -> variance -> variance
  ; class_type_field :
      'mapper_env mapper -> 'mapper_env -> class_type_field -> class_type_field
  ; letop : 'mapper_env mapper -> 'mapper_env -> letop -> letop
  ; location : 'mapper_env mapper -> 'mapper_env -> Location.t -> Location.t
  ; function_param :
      'mapper_env mapper -> 'mapper_env -> function_param -> function_param
  ; signature_item_desc :
      'mapper_env mapper
      -> 'mapper_env
      -> signature_item_desc
      -> signature_item_desc
  ; binding_op : 'mapper_env mapper -> 'mapper_env -> binding_op -> binding_op
  ; open_declaration :
      'mapper_env mapper -> 'mapper_env -> open_declaration -> open_declaration
  ; jkind_annotation_desc :
      'mapper_env mapper
      -> 'mapper_env
      -> jkind_annotation_desc
      -> jkind_annotation_desc
  ; toplevel_phrase :
      'mapper_env mapper -> 'mapper_env -> toplevel_phrase -> toplevel_phrase
  ; arg_label : 'mapper_env mapper -> 'mapper_env -> arg_label -> arg_label
  ; location_stack :
      'mapper_env mapper -> 'mapper_env -> location_stack -> location_stack
  ; value_constraint :
      'mapper_env mapper -> 'mapper_env -> value_constraint -> value_constraint
  ; location_report :
      'mapper_env mapper -> 'mapper_env -> Location.report -> Location.report
  ; comprehension_clause :
      'mapper_env mapper
      -> 'mapper_env
      -> comprehension_clause
      -> comprehension_clause
  ; class_field_desc :
      'mapper_env mapper -> 'mapper_env -> class_field_desc -> class_field_desc
  ; core_type_desc :
      'mapper_env mapper -> 'mapper_env -> core_type_desc -> core_type_desc
  ; virtual_flag :
      'mapper_env mapper -> 'mapper_env -> virtual_flag -> virtual_flag
  ; module_type_desc :
      'mapper_env mapper -> 'mapper_env -> module_type_desc -> module_type_desc
  ; row_field : 'mapper_env mapper -> 'mapper_env -> row_field -> row_field
  ; case : 'mapper_env mapper -> 'mapper_env -> case -> case
  ; type_extension :
      'mapper_env mapper -> 'mapper_env -> type_extension -> type_extension
  ; class_type : 'mapper_env mapper -> 'mapper_env -> class_type -> class_type
  ; signature_item :
      'mapper_env mapper -> 'mapper_env -> signature_item -> signature_item
  ; include_description :
      'mapper_env mapper
      -> 'mapper_env
      -> include_description
      -> include_description
  ; closed_flag :
      'mapper_env mapper -> 'mapper_env -> closed_flag -> closed_flag
  ; type_kind : 'mapper_env mapper -> 'mapper_env -> type_kind -> type_kind
  ; modality : 'mapper_env mapper -> 'mapper_env -> modality -> modality
  ; object_field :
      'mapper_env mapper -> 'mapper_env -> object_field -> object_field
  ; include_infos :
      'a
      .  'mapper_env mapper
      -> ('mapper_env -> 'a -> 'a)
      -> 'mapper_env
      -> 'a include_infos
      -> 'a include_infos
  ; structure_item_desc :
      'mapper_env mapper
      -> 'mapper_env
      -> structure_item_desc
      -> structure_item_desc
  ; include_kind :
      'mapper_env mapper -> 'mapper_env -> include_kind -> include_kind
  ; row_field_desc :
      'mapper_env mapper -> 'mapper_env -> row_field_desc -> row_field_desc
  ; location_report_printer :
      'mapper_env mapper
      -> 'mapper_env
      -> Location.report_printer
      -> Location.report_printer
  ; value_description :
      'mapper_env mapper
      -> 'mapper_env
      -> value_description
      -> value_description
  ; type_exception :
      'mapper_env mapper -> 'mapper_env -> type_exception -> type_exception
  ; function_body :
      'mapper_env mapper -> 'mapper_env -> function_body -> function_body
  ; class_field :
      'mapper_env mapper -> 'mapper_env -> class_field -> class_field
  ; include_declaration :
      'mapper_env mapper
      -> 'mapper_env
      -> include_declaration
      -> include_declaration
  ; attributes : 'mapper_env mapper -> 'mapper_env -> attributes -> attributes
  ; class_type_declaration :
      'mapper_env mapper
      -> 'mapper_env
      -> class_type_declaration
      -> class_type_declaration
  ; open_description :
      'mapper_env mapper -> 'mapper_env -> open_description -> open_description
  ; mutable_flag :
      'mapper_env mapper -> 'mapper_env -> mutable_flag -> mutable_flag
  ; direction_flag :
      'mapper_env mapper -> 'mapper_env -> direction_flag -> direction_flag
  ; pattern_desc :
      'mapper_env mapper -> 'mapper_env -> pattern_desc -> pattern_desc
  ; block_access :
      'mapper_env mapper -> 'mapper_env -> block_access -> block_access
  ; module_expr :
      'mapper_env mapper -> 'mapper_env -> module_expr -> module_expr
  ; module_binding :
      'mapper_env mapper -> 'mapper_env -> module_binding -> module_binding
  ; location_loc :
      'a
      .  'mapper_env mapper
      -> ('mapper_env -> 'a -> 'a)
      -> 'mapper_env
      -> 'a Location.loc
      -> 'a Location.loc
  ; constructor_argument :
      'mapper_env mapper
      -> 'mapper_env
      -> constructor_argument
      -> constructor_argument
  ; class_expr : 'mapper_env mapper -> 'mapper_env -> class_expr -> class_expr
  ; label_declaration :
      'mapper_env mapper
      -> 'mapper_env
      -> label_declaration
      -> label_declaration
  ; signature : 'mapper_env mapper -> 'mapper_env -> signature -> signature
  ; expression : 'mapper_env mapper -> 'mapper_env -> expression -> expression
  ; class_field_kind :
      'mapper_env mapper -> 'mapper_env -> class_field_kind -> class_field_kind
  ; comprehension :
      'mapper_env mapper -> 'mapper_env -> comprehension -> comprehension
  ; type_declaration :
      'mapper_env mapper -> 'mapper_env -> type_declaration -> type_declaration
  ; function_param_desc :
      'mapper_env mapper
      -> 'mapper_env
      -> function_param_desc
      -> function_param_desc
  ; type_constraint :
      'mapper_env mapper -> 'mapper_env -> type_constraint -> type_constraint
  ; with_constraint :
      'mapper_env mapper -> 'mapper_env -> with_constraint -> with_constraint
  ; comprehension_clause_binding :
      'mapper_env mapper
      -> 'mapper_env
      -> comprehension_clause_binding
      -> comprehension_clause_binding
  ; object_field_desc :
      'mapper_env mapper
      -> 'mapper_env
      -> object_field_desc
      -> object_field_desc
  ; expression_desc :
      'mapper_env mapper -> 'mapper_env -> expression_desc -> expression_desc
  ; directive_argument_desc :
      'mapper_env mapper
      -> 'mapper_env
      -> directive_argument_desc
      -> directive_argument_desc
  ; location_report_kind :
      'mapper_env mapper
      -> 'mapper_env
      -> Location.report_kind
      -> Location.report_kind
  ; modalities : 'mapper_env mapper -> 'mapper_env -> modalities -> modalities
  ; attribute : 'mapper_env mapper -> 'mapper_env -> attribute -> attribute
  ; comprehension_expression :
      'mapper_env mapper
      -> 'mapper_env
      -> comprehension_expression
      -> comprehension_expression
  ; class_infos :
      'a
      .  'mapper_env mapper
      -> ('mapper_env -> 'a -> 'a)
      -> 'mapper_env
      -> 'a class_infos
      -> 'a class_infos
  ; private_flag :
      'mapper_env mapper -> 'mapper_env -> private_flag -> private_flag
  ; class_structure :
      'mapper_env mapper -> 'mapper_env -> class_structure -> class_structure
  ; comprehension_iterator :
      'mapper_env mapper
      -> 'mapper_env
      -> comprehension_iterator
      -> comprehension_iterator
  ; module_substitution :
      'mapper_env mapper
      -> 'mapper_env
      -> module_substitution
      -> module_substitution
  ; label : 'mapper_env mapper -> 'mapper_env -> label -> label
  ; open_infos :
      'a
      .  'mapper_env mapper
      -> ('mapper_env -> 'a -> 'a)
      -> 'mapper_env
      -> 'a open_infos
      -> 'a open_infos
  ; index_kind : 'mapper_env mapper -> 'mapper_env -> index_kind -> index_kind
  ; class_declaration :
      'mapper_env mapper
      -> 'mapper_env
      -> class_declaration
      -> class_declaration
  ; constructor_arguments :
      'mapper_env mapper
      -> 'mapper_env
      -> constructor_arguments
      -> constructor_arguments
  ; class_type_field_desc :
      'mapper_env mapper
      -> 'mapper_env
      -> class_type_field_desc
      -> class_type_field_desc
  ; module_type :
      'mapper_env mapper -> 'mapper_env -> module_type -> module_type
  ; extension_constructor :
      'mapper_env mapper
      -> 'mapper_env
      -> extension_constructor
      -> extension_constructor
  }

open struct
  let id_map _ x = x
  let map_option f env o = Option.map (f env) o
  let map_list f env o = List.map (f env) o

  let map_class_expr_desc mapper env class_expr_desc =
    match class_expr_desc with
    | Pcl_constr (_arg_0, _arg_1) ->
      Pcl_constr
        ( mapper.location_loc mapper (mapper.longident mapper) env _arg_0
        , map_list (mapper.core_type mapper) env _arg_1 )
    | Pcl_structure _arg_0 ->
      Pcl_structure (mapper.class_structure mapper env _arg_0)
    | Pcl_fun (_arg_0, _arg_1, _arg_2, _arg_3) ->
      Pcl_fun
        ( mapper.arg_label mapper env _arg_0
        , map_option (mapper.expression mapper) env _arg_1
        , mapper.pattern mapper env _arg_2
        , mapper.class_expr mapper env _arg_3 )
    | Pcl_apply (_arg_0, _arg_1) ->
      Pcl_apply
        ( mapper.class_expr mapper env _arg_0
        , map_list
            (fun env __arg ->
              let _0, _1 = __arg in
              mapper.arg_label mapper env _0, mapper.expression mapper env _1)
            env
            _arg_1 )
    | Pcl_let (_arg_0, _arg_1, _arg_2) ->
      Pcl_let
        ( mapper.rec_flag mapper env _arg_0
        , map_list (mapper.value_binding mapper) env _arg_1
        , mapper.class_expr mapper env _arg_2 )
    | Pcl_constraint (_arg_0, _arg_1) ->
      Pcl_constraint
        ( mapper.class_expr mapper env _arg_0
        , mapper.class_type mapper env _arg_1 )
    | Pcl_extension _arg_0 -> Pcl_extension (mapper.extension mapper env _arg_0)
    | Pcl_open (_arg_0, _arg_1) ->
      Pcl_open
        ( mapper.open_description mapper env _arg_0
        , mapper.class_expr mapper env _arg_1 )
  ;;

  let map_injectivity mapper env injectivity =
    match injectivity with
    | Injective -> Injective
    | NoInjectivity -> NoInjectivity
  ;;

  let map_mode mapper env mode =
    match mode with
    | Mode _arg_0 -> Mode (id_map env _arg_0)
  ;;

  let map_modes mapper env modes =
    map_list (mapper.location_loc mapper (mapper.mode mapper)) env modes
  ;;

  let map_package_type mapper env package_type =
    let _0, _1 = package_type in
    ( mapper.location_loc mapper (mapper.longident mapper) env _0
    , map_list
        (fun env __arg ->
          let _0, _1 = __arg in
          ( mapper.location_loc mapper (mapper.longident mapper) env _0
          , mapper.core_type mapper env _1 ))
        env
        _1 )
  ;;

  let map_class_signature mapper env class_signature =
    let pcsig_self = mapper.core_type mapper env class_signature.pcsig_self in
    let pcsig_fields =
      map_list (mapper.class_type_field mapper) env class_signature.pcsig_fields
    in
    { pcsig_self; pcsig_fields }
  ;;

  let map_payload mapper env payload =
    match payload with
    | PStr _arg_0 -> PStr (mapper.structure mapper env _arg_0)
    | PSig _arg_0 -> PSig (mapper.signature mapper env _arg_0)
    | PTyp _arg_0 -> PTyp (mapper.core_type mapper env _arg_0)
    | PPat (_arg_0, _arg_1) ->
      PPat
        ( mapper.pattern mapper env _arg_0
        , map_option (mapper.expression mapper) env _arg_1 )
  ;;

  let map_directive_argument mapper env directive_argument =
    let pdira_desc =
      mapper.directive_argument_desc mapper env directive_argument.pdira_desc
    in
    let pdira_loc = mapper.location mapper env directive_argument.pdira_loc in
    { pdira_desc; pdira_loc }
  ;;

  let map_value_binding mapper env value_binding =
    let pvb_pat = mapper.pattern mapper env value_binding.pvb_pat in
    let pvb_expr = mapper.expression mapper env value_binding.pvb_expr in
    let pvb_constraint =
      map_option
        (mapper.value_constraint mapper)
        env
        value_binding.pvb_constraint
    in
    let pvb_modes = mapper.modes mapper env value_binding.pvb_modes in
    let pvb_attributes =
      mapper.attributes mapper env value_binding.pvb_attributes
    in
    let pvb_loc = mapper.location mapper env value_binding.pvb_loc in
    { pvb_pat; pvb_expr; pvb_constraint; pvb_modes; pvb_attributes; pvb_loc }
  ;;

  let map_pattern mapper env pattern =
    let ppat_desc = mapper.pattern_desc mapper env pattern.ppat_desc in
    let ppat_loc = mapper.location mapper env pattern.ppat_loc in
    let ppat_loc_stack =
      mapper.location_stack mapper env pattern.ppat_loc_stack
    in
    let ppat_attributes =
      mapper.attributes mapper env pattern.ppat_attributes
    in
    { ppat_desc; ppat_loc; ppat_loc_stack; ppat_attributes }
  ;;

  let map_unboxed_access mapper env unboxed_access =
    match unboxed_access with
    | Uaccess_unboxed_field _arg_0 ->
      Uaccess_unboxed_field
        (mapper.location_loc mapper (mapper.longident mapper) env _arg_0)
  ;;

  let map_functor_parameter mapper env functor_parameter =
    match functor_parameter with
    | Unit -> Unit
    | Named (_arg_0, _arg_1, _arg_2) ->
      Named
        ( mapper.location_loc mapper (map_option id_map) env _arg_0
        , mapper.module_type mapper env _arg_1
        , mapper.modes mapper env _arg_2 )
  ;;

  let map_constructor_declaration mapper env constructor_declaration =
    let pcd_name =
      mapper.location_loc mapper id_map env constructor_declaration.pcd_name
    in
    let pcd_vars =
      map_list
        (fun env __arg ->
          let _0, _1 = __arg in
          ( mapper.location_loc mapper id_map env _0
          , map_option (mapper.jkind_annotation mapper) env _1 ))
        env
        constructor_declaration.pcd_vars
    in
    let pcd_args =
      mapper.constructor_arguments mapper env constructor_declaration.pcd_args
    in
    let pcd_res =
      map_option (mapper.core_type mapper) env constructor_declaration.pcd_res
    in
    let pcd_loc = mapper.location mapper env constructor_declaration.pcd_loc in
    let pcd_attributes =
      mapper.attributes mapper env constructor_declaration.pcd_attributes
    in
    { pcd_name; pcd_vars; pcd_args; pcd_res; pcd_loc; pcd_attributes }
  ;;

  let map_jkind_annotation mapper env jkind_annotation =
    let pjkind_loc = mapper.location mapper env jkind_annotation.pjkind_loc in
    let pjkind_desc =
      mapper.jkind_annotation_desc mapper env jkind_annotation.pjkind_desc
    in
    { pjkind_loc; pjkind_desc }
  ;;

  let map_longident mapper env t =
    match t with
    | Longident.Lident _arg_0 -> Longident.Lident (id_map env _arg_0)
    | Longident.Ldot (_arg_0, _arg_1) ->
      Longident.Ldot (mapper.longident mapper env _arg_0, id_map env _arg_1)
    | Longident.Lapply (_arg_0, _arg_1) ->
      Longident.Lapply
        (mapper.longident mapper env _arg_0, mapper.longident mapper env _arg_1)
  ;;

  let map_location_msg mapper env msg =
    mapper.location_loc mapper id_map env msg
  ;;

  let map_location_error mapper env error =
    mapper.location_report mapper env error
  ;;

  let map_constant mapper env constant =
    match constant with
    | Pconst_integer (_arg_0, _arg_1) ->
      Pconst_integer (id_map env _arg_0, map_option id_map env _arg_1)
    | Pconst_unboxed_integer (_arg_0, _arg_1) ->
      Pconst_unboxed_integer (id_map env _arg_0, id_map env _arg_1)
    | Pconst_char _arg_0 -> Pconst_char (id_map env _arg_0)
    | Pconst_untagged_char _arg_0 -> Pconst_untagged_char (id_map env _arg_0)
    | Pconst_string (_arg_0, _arg_1, _arg_2) ->
      Pconst_string
        ( id_map env _arg_0
        , mapper.location mapper env _arg_1
        , map_option id_map env _arg_2 )
    | Pconst_float (_arg_0, _arg_1) ->
      Pconst_float (id_map env _arg_0, map_option id_map env _arg_1)
    | Pconst_unboxed_float (_arg_0, _arg_1) ->
      Pconst_unboxed_float (id_map env _arg_0, map_option id_map env _arg_1)
  ;;

  let map_module_expr_desc mapper env module_expr_desc =
    match module_expr_desc with
    | Pmod_ident _arg_0 ->
      Pmod_ident
        (mapper.location_loc mapper (mapper.longident mapper) env _arg_0)
    | Pmod_structure _arg_0 ->
      Pmod_structure (mapper.structure mapper env _arg_0)
    | Pmod_functor (_arg_0, _arg_1) ->
      Pmod_functor
        ( mapper.functor_parameter mapper env _arg_0
        , mapper.module_expr mapper env _arg_1 )
    | Pmod_apply (_arg_0, _arg_1) ->
      Pmod_apply
        ( mapper.module_expr mapper env _arg_0
        , mapper.module_expr mapper env _arg_1 )
    | Pmod_apply_unit _arg_0 ->
      Pmod_apply_unit (mapper.module_expr mapper env _arg_0)
    | Pmod_constraint (_arg_0, _arg_1, _arg_2) ->
      Pmod_constraint
        ( mapper.module_expr mapper env _arg_0
        , map_option (mapper.module_type mapper) env _arg_1
        , mapper.modes mapper env _arg_2 )
    | Pmod_unpack _arg_0 -> Pmod_unpack (mapper.expression mapper env _arg_0)
    | Pmod_extension _arg_0 ->
      Pmod_extension (mapper.extension mapper env _arg_0)
    | Pmod_instance _arg_0 ->
      Pmod_instance (mapper.module_instance mapper env _arg_0)
  ;;

  let map_structure_item mapper env structure_item =
    let pstr_desc =
      mapper.structure_item_desc mapper env structure_item.pstr_desc
    in
    let pstr_loc = mapper.location mapper env structure_item.pstr_loc in
    { pstr_desc; pstr_loc }
  ;;

  let map_override_flag mapper env override_flag =
    match override_flag with
    | Override -> Override
    | Fresh -> Fresh
  ;;

  let map_class_type_desc mapper env class_type_desc =
    match class_type_desc with
    | Pcty_constr (_arg_0, _arg_1) ->
      Pcty_constr
        ( mapper.location_loc mapper (mapper.longident mapper) env _arg_0
        , map_list (mapper.core_type mapper) env _arg_1 )
    | Pcty_signature _arg_0 ->
      Pcty_signature (mapper.class_signature mapper env _arg_0)
    | Pcty_arrow (_arg_0, _arg_1, _arg_2) ->
      Pcty_arrow
        ( mapper.arg_label mapper env _arg_0
        , mapper.core_type mapper env _arg_1
        , mapper.class_type mapper env _arg_2 )
    | Pcty_extension _arg_0 ->
      Pcty_extension (mapper.extension mapper env _arg_0)
    | Pcty_open (_arg_0, _arg_1) ->
      Pcty_open
        ( mapper.open_description mapper env _arg_0
        , mapper.class_type mapper env _arg_1 )
  ;;

  let map_extension mapper env extension =
    let _0, _1 = extension in
    mapper.location_loc mapper id_map env _0, mapper.payload mapper env _1
  ;;

  let map_rec_flag mapper env rec_flag =
    match rec_flag with
    | Nonrecursive -> Nonrecursive
    | Recursive -> Recursive
  ;;

  let map_class_description mapper env class_description =
    mapper.class_infos mapper (mapper.class_type mapper) env class_description
  ;;

  let map_core_type mapper env core_type =
    let ptyp_desc = mapper.core_type_desc mapper env core_type.ptyp_desc in
    let ptyp_loc = mapper.location mapper env core_type.ptyp_loc in
    let ptyp_loc_stack =
      mapper.location_stack mapper env core_type.ptyp_loc_stack
    in
    let ptyp_attributes =
      mapper.attributes mapper env core_type.ptyp_attributes
    in
    { ptyp_desc; ptyp_loc; ptyp_loc_stack; ptyp_attributes }
  ;;

  let map_function_constraint mapper env function_constraint =
    let mode_annotations =
      mapper.modes mapper env function_constraint.mode_annotations
    in
    let ret_mode_annotations =
      mapper.modes mapper env function_constraint.ret_mode_annotations
    in
    let ret_type_constraint =
      map_option
        (mapper.type_constraint mapper)
        env
        function_constraint.ret_type_constraint
    in
    { mode_annotations; ret_mode_annotations; ret_type_constraint }
  ;;

  let map_extension_constructor_kind mapper env extension_constructor_kind =
    match extension_constructor_kind with
    | Pext_decl (_arg_0, _arg_1, _arg_2) ->
      Pext_decl
        ( map_list
            (fun env __arg ->
              let _0, _1 = __arg in
              ( mapper.location_loc mapper id_map env _0
              , map_option (mapper.jkind_annotation mapper) env _1 ))
            env
            _arg_0
        , mapper.constructor_arguments mapper env _arg_1
        , map_option (mapper.core_type mapper) env _arg_2 )
    | Pext_rebind _arg_0 ->
      Pext_rebind
        (mapper.location_loc mapper (mapper.longident mapper) env _arg_0)
  ;;

  let map_module_instance mapper env module_instance =
    let pmod_instance_head = id_map env module_instance.pmod_instance_head in
    let pmod_instance_args =
      map_list
        (fun env __arg ->
          let _0, _1 = __arg in
          id_map env _0, mapper.module_instance mapper env _1)
        env
        module_instance.pmod_instance_args
    in
    { pmod_instance_head; pmod_instance_args }
  ;;

  let map_structure mapper env structure =
    map_list (mapper.structure_item mapper) env structure
  ;;

  let map_toplevel_directive mapper env toplevel_directive =
    let pdir_name =
      mapper.location_loc mapper id_map env toplevel_directive.pdir_name
    in
    let pdir_arg =
      map_option
        (mapper.directive_argument mapper)
        env
        toplevel_directive.pdir_arg
    in
    let pdir_loc = mapper.location mapper env toplevel_directive.pdir_loc in
    { pdir_name; pdir_arg; pdir_loc }
  ;;

  let map_module_declaration mapper env module_declaration =
    let pmd_name =
      mapper.location_loc
        mapper
        (map_option id_map)
        env
        module_declaration.pmd_name
    in
    let pmd_type = mapper.module_type mapper env module_declaration.pmd_type in
    let pmd_modalities =
      mapper.modalities mapper env module_declaration.pmd_modalities
    in
    let pmd_attributes =
      mapper.attributes mapper env module_declaration.pmd_attributes
    in
    let pmd_loc = mapper.location mapper env module_declaration.pmd_loc in
    { pmd_name; pmd_type; pmd_modalities; pmd_attributes; pmd_loc }
  ;;

  let map_module_type_declaration mapper env module_type_declaration =
    let pmtd_name =
      mapper.location_loc mapper id_map env module_type_declaration.pmtd_name
    in
    let pmtd_type =
      map_option
        (mapper.module_type mapper)
        env
        module_type_declaration.pmtd_type
    in
    let pmtd_attributes =
      mapper.attributes mapper env module_type_declaration.pmtd_attributes
    in
    let pmtd_loc =
      mapper.location mapper env module_type_declaration.pmtd_loc
    in
    { pmtd_name; pmtd_type; pmtd_attributes; pmtd_loc }
  ;;

  let map_variance mapper env variance =
    match variance with
    | Covariant -> Covariant
    | Contravariant -> Contravariant
    | NoVariance -> NoVariance
  ;;

  let map_class_type_field mapper env class_type_field =
    let pctf_desc =
      mapper.class_type_field_desc mapper env class_type_field.pctf_desc
    in
    let pctf_loc = mapper.location mapper env class_type_field.pctf_loc in
    let pctf_attributes =
      mapper.attributes mapper env class_type_field.pctf_attributes
    in
    { pctf_desc; pctf_loc; pctf_attributes }
  ;;

  let map_letop mapper env letop =
    let let_ = mapper.binding_op mapper env letop.let_ in
    let ands = map_list (mapper.binding_op mapper) env letop.ands in
    let body = mapper.expression mapper env letop.body in
    { let_; ands; body }
  ;;

  let map_location mapper env t =
    let loc_start = id_map env t.Location.loc_start in
    let loc_end = id_map env t.Location.loc_end in
    let loc_ghost = id_map env t.Location.loc_ghost in
    { Location.loc_start; loc_end; loc_ghost }
  ;;

  let map_function_param mapper env function_param =
    let pparam_loc = mapper.location mapper env function_param.pparam_loc in
    let pparam_desc =
      mapper.function_param_desc mapper env function_param.pparam_desc
    in
    { pparam_loc; pparam_desc }
  ;;

  let map_signature_item_desc mapper env signature_item_desc =
    match signature_item_desc with
    | Psig_value _arg_0 ->
      Psig_value (mapper.value_description mapper env _arg_0)
    | Psig_type (_arg_0, _arg_1) ->
      Psig_type
        ( mapper.rec_flag mapper env _arg_0
        , map_list (mapper.type_declaration mapper) env _arg_1 )
    | Psig_typesubst _arg_0 ->
      Psig_typesubst (map_list (mapper.type_declaration mapper) env _arg_0)
    | Psig_typext _arg_0 ->
      Psig_typext (mapper.type_extension mapper env _arg_0)
    | Psig_exception _arg_0 ->
      Psig_exception (mapper.type_exception mapper env _arg_0)
    | Psig_module _arg_0 ->
      Psig_module (mapper.module_declaration mapper env _arg_0)
    | Psig_modsubst _arg_0 ->
      Psig_modsubst (mapper.module_substitution mapper env _arg_0)
    | Psig_recmodule _arg_0 ->
      Psig_recmodule (map_list (mapper.module_declaration mapper) env _arg_0)
    | Psig_modtype _arg_0 ->
      Psig_modtype (mapper.module_type_declaration mapper env _arg_0)
    | Psig_modtypesubst _arg_0 ->
      Psig_modtypesubst (mapper.module_type_declaration mapper env _arg_0)
    | Psig_open _arg_0 -> Psig_open (mapper.open_description mapper env _arg_0)
    | Psig_include (_arg_0, _arg_1) ->
      Psig_include
        ( mapper.include_description mapper env _arg_0
        , mapper.modalities mapper env _arg_1 )
    | Psig_class _arg_0 ->
      Psig_class (map_list (mapper.class_description mapper) env _arg_0)
    | Psig_class_type _arg_0 ->
      Psig_class_type
        (map_list (mapper.class_type_declaration mapper) env _arg_0)
    | Psig_attribute _arg_0 ->
      Psig_attribute (mapper.attribute mapper env _arg_0)
    | Psig_extension (_arg_0, _arg_1) ->
      Psig_extension
        ( mapper.extension mapper env _arg_0
        , mapper.attributes mapper env _arg_1 )
    | Psig_kind_abbrev (_arg_0, _arg_1) ->
      Psig_kind_abbrev
        ( mapper.location_loc mapper id_map env _arg_0
        , mapper.jkind_annotation mapper env _arg_1 )
  ;;

  let map_binding_op mapper env binding_op =
    let pbop_op = mapper.location_loc mapper id_map env binding_op.pbop_op in
    let pbop_pat = mapper.pattern mapper env binding_op.pbop_pat in
    let pbop_exp = mapper.expression mapper env binding_op.pbop_exp in
    let pbop_loc = mapper.location mapper env binding_op.pbop_loc in
    { pbop_op; pbop_pat; pbop_exp; pbop_loc }
  ;;

  let map_open_declaration mapper env open_declaration =
    mapper.open_infos mapper (mapper.module_expr mapper) env open_declaration
  ;;

  let map_jkind_annotation_desc mapper env jkind_annotation_desc =
    match jkind_annotation_desc with
    | Pjk_default -> Pjk_default
    | Pjk_abbreviation _arg_0 -> Pjk_abbreviation (id_map env _arg_0)
    | Pjk_mod (_arg_0, _arg_1) ->
      Pjk_mod
        ( mapper.jkind_annotation mapper env _arg_0
        , mapper.modes mapper env _arg_1 )
    | Pjk_with (_arg_0, _arg_1, _arg_2) ->
      Pjk_with
        ( mapper.jkind_annotation mapper env _arg_0
        , mapper.core_type mapper env _arg_1
        , mapper.modalities mapper env _arg_2 )
    | Pjk_kind_of _arg_0 -> Pjk_kind_of (mapper.core_type mapper env _arg_0)
    | Pjk_product _arg_0 ->
      Pjk_product (map_list (mapper.jkind_annotation mapper) env _arg_0)
  ;;

  let map_toplevel_phrase mapper env toplevel_phrase =
    match toplevel_phrase with
    | Ptop_def _arg_0 -> Ptop_def (mapper.structure mapper env _arg_0)
    | Ptop_dir _arg_0 -> Ptop_dir (mapper.toplevel_directive mapper env _arg_0)
  ;;

  let map_arg_label mapper env arg_label =
    match arg_label with
    | Nolabel -> Nolabel
    | Labelled _arg_0 -> Labelled (id_map env _arg_0)
    | Optional _arg_0 -> Optional (id_map env _arg_0)
  ;;

  let map_location_stack mapper env location_stack =
    map_list (mapper.location mapper) env location_stack
  ;;

  let map_value_constraint mapper env value_constraint =
    match value_constraint with
    | Pvc_constraint _arg ->
      let locally_abstract_univars =
        map_list
          (mapper.location_loc mapper id_map)
          env
          _arg.locally_abstract_univars
      in
      let typ = mapper.core_type mapper env _arg.typ in
      Pvc_constraint { locally_abstract_univars; typ }
    | Pvc_coercion _arg ->
      let ground = map_option (mapper.core_type mapper) env _arg.ground in
      let coercion = mapper.core_type mapper env _arg.coercion in
      Pvc_coercion { ground; coercion }
  ;;

  let map_location_report mapper env report =
    let kind = mapper.location_report_kind mapper env report.Location.kind in
    let main = mapper.location_msg mapper env report.Location.main in
    let sub = map_list (mapper.location_msg mapper) env report.Location.sub in
    { Location.kind; main; sub }
  ;;

  let map_comprehension_clause mapper env comprehension_clause =
    match comprehension_clause with
    | Pcomp_for _arg_0 ->
      Pcomp_for
        (map_list (mapper.comprehension_clause_binding mapper) env _arg_0)
    | Pcomp_when _arg_0 -> Pcomp_when (mapper.expression mapper env _arg_0)
  ;;

  let map_class_field_desc mapper env class_field_desc =
    match class_field_desc with
    | Pcf_inherit (_arg_0, _arg_1, _arg_2) ->
      Pcf_inherit
        ( mapper.override_flag mapper env _arg_0
        , mapper.class_expr mapper env _arg_1
        , map_option (mapper.location_loc mapper id_map) env _arg_2 )
    | Pcf_val _arg_0 ->
      Pcf_val
        (let _0, _1, _2 = _arg_0 in
         ( mapper.location_loc mapper (mapper.label mapper) env _0
         , mapper.mutable_flag mapper env _1
         , mapper.class_field_kind mapper env _2 ))
    | Pcf_method _arg_0 ->
      Pcf_method
        (let _0, _1, _2 = _arg_0 in
         ( mapper.location_loc mapper (mapper.label mapper) env _0
         , mapper.private_flag mapper env _1
         , mapper.class_field_kind mapper env _2 ))
    | Pcf_constraint _arg_0 ->
      Pcf_constraint
        (let _0, _1 = _arg_0 in
         mapper.core_type mapper env _0, mapper.core_type mapper env _1)
    | Pcf_initializer _arg_0 ->
      Pcf_initializer (mapper.expression mapper env _arg_0)
    | Pcf_attribute _arg_0 -> Pcf_attribute (mapper.attribute mapper env _arg_0)
    | Pcf_extension _arg_0 -> Pcf_extension (mapper.extension mapper env _arg_0)
  ;;

  let map_core_type_desc mapper env core_type_desc =
    match core_type_desc with
    | Ptyp_any _arg_0 ->
      Ptyp_any (map_option (mapper.jkind_annotation mapper) env _arg_0)
    | Ptyp_var (_arg_0, _arg_1) ->
      Ptyp_var
        ( id_map env _arg_0
        , map_option (mapper.jkind_annotation mapper) env _arg_1 )
    | Ptyp_arrow (_arg_0, _arg_1, _arg_2, _arg_3, _arg_4) ->
      Ptyp_arrow
        ( mapper.arg_label mapper env _arg_0
        , mapper.core_type mapper env _arg_1
        , mapper.core_type mapper env _arg_2
        , mapper.modes mapper env _arg_3
        , mapper.modes mapper env _arg_4 )
    | Ptyp_tuple _arg_0 ->
      Ptyp_tuple
        (map_list
           (fun env __arg ->
             let _0, _1 = __arg in
             map_option id_map env _0, mapper.core_type mapper env _1)
           env
           _arg_0)
    | Ptyp_unboxed_tuple _arg_0 ->
      Ptyp_unboxed_tuple
        (map_list
           (fun env __arg ->
             let _0, _1 = __arg in
             map_option id_map env _0, mapper.core_type mapper env _1)
           env
           _arg_0)
    | Ptyp_constr (_arg_0, _arg_1) ->
      Ptyp_constr
        ( mapper.location_loc mapper (mapper.longident mapper) env _arg_0
        , map_list (mapper.core_type mapper) env _arg_1 )
    | Ptyp_object (_arg_0, _arg_1) ->
      Ptyp_object
        ( map_list (mapper.object_field mapper) env _arg_0
        , mapper.closed_flag mapper env _arg_1 )
    | Ptyp_class (_arg_0, _arg_1) ->
      Ptyp_class
        ( mapper.location_loc mapper (mapper.longident mapper) env _arg_0
        , map_list (mapper.core_type mapper) env _arg_1 )
    | Ptyp_alias (_arg_0, _arg_1, _arg_2) ->
      Ptyp_alias
        ( mapper.core_type mapper env _arg_0
        , map_option (mapper.location_loc mapper id_map) env _arg_1
        , map_option (mapper.jkind_annotation mapper) env _arg_2 )
    | Ptyp_variant (_arg_0, _arg_1, _arg_2) ->
      Ptyp_variant
        ( map_list (mapper.row_field mapper) env _arg_0
        , mapper.closed_flag mapper env _arg_1
        , map_option (map_list (mapper.label mapper)) env _arg_2 )
    | Ptyp_poly (_arg_0, _arg_1) ->
      Ptyp_poly
        ( map_list
            (fun env __arg ->
              let _0, _1 = __arg in
              ( mapper.location_loc mapper id_map env _0
              , map_option (mapper.jkind_annotation mapper) env _1 ))
            env
            _arg_0
        , mapper.core_type mapper env _arg_1 )
    | Ptyp_package _arg_0 ->
      Ptyp_package (mapper.package_type mapper env _arg_0)
    | Ptyp_open (_arg_0, _arg_1) ->
      Ptyp_open
        ( mapper.location_loc mapper (mapper.longident mapper) env _arg_0
        , mapper.core_type mapper env _arg_1 )
    | Ptyp_quote _arg_0 -> Ptyp_quote (mapper.core_type mapper env _arg_0)
    | Ptyp_splice _arg_0 -> Ptyp_splice (mapper.core_type mapper env _arg_0)
    | Ptyp_of_kind _arg_0 ->
      Ptyp_of_kind (mapper.jkind_annotation mapper env _arg_0)
    | Ptyp_extension _arg_0 ->
      Ptyp_extension (mapper.extension mapper env _arg_0)
  ;;

  let map_virtual_flag mapper env virtual_flag =
    match virtual_flag with
    | Virtual -> Virtual
    | Concrete -> Concrete
  ;;

  let map_module_type_desc mapper env module_type_desc =
    match module_type_desc with
    | Pmty_ident _arg_0 ->
      Pmty_ident
        (mapper.location_loc mapper (mapper.longident mapper) env _arg_0)
    | Pmty_signature _arg_0 ->
      Pmty_signature (mapper.signature mapper env _arg_0)
    | Pmty_functor (_arg_0, _arg_1, _arg_2) ->
      Pmty_functor
        ( mapper.functor_parameter mapper env _arg_0
        , mapper.module_type mapper env _arg_1
        , mapper.modes mapper env _arg_2 )
    | Pmty_with (_arg_0, _arg_1) ->
      Pmty_with
        ( mapper.module_type mapper env _arg_0
        , map_list (mapper.with_constraint mapper) env _arg_1 )
    | Pmty_typeof _arg_0 -> Pmty_typeof (mapper.module_expr mapper env _arg_0)
    | Pmty_extension _arg_0 ->
      Pmty_extension (mapper.extension mapper env _arg_0)
    | Pmty_alias _arg_0 ->
      Pmty_alias
        (mapper.location_loc mapper (mapper.longident mapper) env _arg_0)
    | Pmty_strengthen (_arg_0, _arg_1) ->
      Pmty_strengthen
        ( mapper.module_type mapper env _arg_0
        , mapper.location_loc mapper (mapper.longident mapper) env _arg_1 )
  ;;

  let map_row_field mapper env row_field =
    let prf_desc = mapper.row_field_desc mapper env row_field.prf_desc in
    let prf_loc = mapper.location mapper env row_field.prf_loc in
    let prf_attributes =
      mapper.attributes mapper env row_field.prf_attributes
    in
    { prf_desc; prf_loc; prf_attributes }
  ;;

  let map_case mapper env case =
    let pc_lhs = mapper.pattern mapper env case.pc_lhs in
    let pc_guard = map_option (mapper.expression mapper) env case.pc_guard in
    let pc_rhs = mapper.expression mapper env case.pc_rhs in
    { pc_lhs; pc_guard; pc_rhs }
  ;;

  let map_type_extension mapper env type_extension =
    let ptyext_path =
      mapper.location_loc
        mapper
        (mapper.longident mapper)
        env
        type_extension.ptyext_path
    in
    let ptyext_params =
      map_list
        (fun env __arg ->
          let _0, _1 = __arg in
          ( mapper.core_type mapper env _0
          , let _0, _1 = _1 in
            mapper.variance mapper env _0, mapper.injectivity mapper env _1 ))
        env
        type_extension.ptyext_params
    in
    let ptyext_constructors =
      map_list
        (mapper.extension_constructor mapper)
        env
        type_extension.ptyext_constructors
    in
    let ptyext_private =
      mapper.private_flag mapper env type_extension.ptyext_private
    in
    let ptyext_loc = mapper.location mapper env type_extension.ptyext_loc in
    let ptyext_attributes =
      mapper.attributes mapper env type_extension.ptyext_attributes
    in
    { ptyext_path
    ; ptyext_params
    ; ptyext_constructors
    ; ptyext_private
    ; ptyext_loc
    ; ptyext_attributes
    }
  ;;

  let map_class_type mapper env class_type =
    let pcty_desc = mapper.class_type_desc mapper env class_type.pcty_desc in
    let pcty_loc = mapper.location mapper env class_type.pcty_loc in
    let pcty_attributes =
      mapper.attributes mapper env class_type.pcty_attributes
    in
    { pcty_desc; pcty_loc; pcty_attributes }
  ;;

  let map_signature_item mapper env signature_item =
    let psig_desc =
      mapper.signature_item_desc mapper env signature_item.psig_desc
    in
    let psig_loc = mapper.location mapper env signature_item.psig_loc in
    { psig_desc; psig_loc }
  ;;

  let map_include_description mapper env include_description =
    mapper.include_infos
      mapper
      (mapper.module_type mapper)
      env
      include_description
  ;;

  let map_closed_flag mapper env closed_flag =
    match closed_flag with
    | Closed -> Closed
    | Open -> Open
  ;;

  let map_type_kind mapper env type_kind =
    match type_kind with
    | Ptype_abstract -> Ptype_abstract
    | Ptype_variant _arg_0 ->
      Ptype_variant
        (map_list (mapper.constructor_declaration mapper) env _arg_0)
    | Ptype_record _arg_0 ->
      Ptype_record (map_list (mapper.label_declaration mapper) env _arg_0)
    | Ptype_record_unboxed_product _arg_0 ->
      Ptype_record_unboxed_product
        (map_list (mapper.label_declaration mapper) env _arg_0)
    | Ptype_open -> Ptype_open
  ;;

  let map_modality mapper env modality =
    match modality with
    | Modality _arg_0 -> Modality (id_map env _arg_0)
  ;;

  let map_object_field mapper env object_field =
    let pof_desc = mapper.object_field_desc mapper env object_field.pof_desc in
    let pof_loc = mapper.location mapper env object_field.pof_loc in
    let pof_attributes =
      mapper.attributes mapper env object_field.pof_attributes
    in
    { pof_desc; pof_loc; pof_attributes }
  ;;

  let map_include_infos mapper map_'a env include_infos =
    let pincl_kind = mapper.include_kind mapper env include_infos.pincl_kind in
    let pincl_mod = map_'a env include_infos.pincl_mod in
    let pincl_loc = mapper.location mapper env include_infos.pincl_loc in
    let pincl_attributes =
      mapper.attributes mapper env include_infos.pincl_attributes
    in
    { pincl_kind; pincl_mod; pincl_loc; pincl_attributes }
  ;;

  let map_structure_item_desc mapper env structure_item_desc =
    match structure_item_desc with
    | Pstr_eval (_arg_0, _arg_1) ->
      Pstr_eval
        ( mapper.expression mapper env _arg_0
        , mapper.attributes mapper env _arg_1 )
    | Pstr_value (_arg_0, _arg_1) ->
      Pstr_value
        ( mapper.rec_flag mapper env _arg_0
        , map_list (mapper.value_binding mapper) env _arg_1 )
    | Pstr_primitive _arg_0 ->
      Pstr_primitive (mapper.value_description mapper env _arg_0)
    | Pstr_type (_arg_0, _arg_1) ->
      Pstr_type
        ( mapper.rec_flag mapper env _arg_0
        , map_list (mapper.type_declaration mapper) env _arg_1 )
    | Pstr_typext _arg_0 ->
      Pstr_typext (mapper.type_extension mapper env _arg_0)
    | Pstr_exception _arg_0 ->
      Pstr_exception (mapper.type_exception mapper env _arg_0)
    | Pstr_module _arg_0 ->
      Pstr_module (mapper.module_binding mapper env _arg_0)
    | Pstr_recmodule _arg_0 ->
      Pstr_recmodule (map_list (mapper.module_binding mapper) env _arg_0)
    | Pstr_modtype _arg_0 ->
      Pstr_modtype (mapper.module_type_declaration mapper env _arg_0)
    | Pstr_open _arg_0 -> Pstr_open (mapper.open_declaration mapper env _arg_0)
    | Pstr_class _arg_0 ->
      Pstr_class (map_list (mapper.class_declaration mapper) env _arg_0)
    | Pstr_class_type _arg_0 ->
      Pstr_class_type
        (map_list (mapper.class_type_declaration mapper) env _arg_0)
    | Pstr_include _arg_0 ->
      Pstr_include (mapper.include_declaration mapper env _arg_0)
    | Pstr_attribute _arg_0 ->
      Pstr_attribute (mapper.attribute mapper env _arg_0)
    | Pstr_extension (_arg_0, _arg_1) ->
      Pstr_extension
        ( mapper.extension mapper env _arg_0
        , mapper.attributes mapper env _arg_1 )
    | Pstr_kind_abbrev (_arg_0, _arg_1) ->
      Pstr_kind_abbrev
        ( mapper.location_loc mapper id_map env _arg_0
        , mapper.jkind_annotation mapper env _arg_1 )
  ;;

  let map_include_kind mapper env include_kind =
    match include_kind with
    | Structure -> Structure
    | Functor -> Functor
  ;;

  let map_row_field_desc mapper env row_field_desc =
    match row_field_desc with
    | Rtag (_arg_0, _arg_1, _arg_2) ->
      Rtag
        ( mapper.location_loc mapper (mapper.label mapper) env _arg_0
        , id_map env _arg_1
        , map_list (mapper.core_type mapper) env _arg_2 )
    | Rinherit _arg_0 -> Rinherit (mapper.core_type mapper env _arg_0)
  ;;

  let map_location_report_printer mapper env report_printer =
    let pp = id_map env report_printer.Location.pp in
    let pp_report_kind = id_map env report_printer.Location.pp_report_kind in
    let pp_main_loc = id_map env report_printer.Location.pp_main_loc in
    let pp_main_txt = id_map env report_printer.Location.pp_main_txt in
    let pp_submsgs = id_map env report_printer.Location.pp_submsgs in
    let pp_submsg = id_map env report_printer.Location.pp_submsg in
    let pp_submsg_loc = id_map env report_printer.Location.pp_submsg_loc in
    let pp_submsg_txt = id_map env report_printer.Location.pp_submsg_txt in
    { Location.pp
    ; pp_report_kind
    ; pp_main_loc
    ; pp_main_txt
    ; pp_submsgs
    ; pp_submsg
    ; pp_submsg_loc
    ; pp_submsg_txt
    }
  ;;

  let map_value_description mapper env value_description =
    let pval_name =
      mapper.location_loc mapper id_map env value_description.pval_name
    in
    let pval_type = mapper.core_type mapper env value_description.pval_type in
    let pval_modalities =
      mapper.modalities mapper env value_description.pval_modalities
    in
    let pval_prim = map_list id_map env value_description.pval_prim in
    let pval_attributes =
      mapper.attributes mapper env value_description.pval_attributes
    in
    let pval_loc = mapper.location mapper env value_description.pval_loc in
    { pval_name
    ; pval_type
    ; pval_modalities
    ; pval_prim
    ; pval_attributes
    ; pval_loc
    }
  ;;

  let map_type_exception mapper env type_exception =
    let ptyexn_constructor =
      mapper.extension_constructor mapper env type_exception.ptyexn_constructor
    in
    let ptyexn_loc = mapper.location mapper env type_exception.ptyexn_loc in
    let ptyexn_attributes =
      mapper.attributes mapper env type_exception.ptyexn_attributes
    in
    { ptyexn_constructor; ptyexn_loc; ptyexn_attributes }
  ;;

  let map_function_body mapper env function_body =
    match function_body with
    | Pfunction_body _arg_0 ->
      Pfunction_body (mapper.expression mapper env _arg_0)
    | Pfunction_cases (_arg_0, _arg_1, _arg_2) ->
      Pfunction_cases
        ( map_list (mapper.case mapper) env _arg_0
        , mapper.location mapper env _arg_1
        , mapper.attributes mapper env _arg_2 )
  ;;

  let map_class_field mapper env class_field =
    let pcf_desc = mapper.class_field_desc mapper env class_field.pcf_desc in
    let pcf_loc = mapper.location mapper env class_field.pcf_loc in
    let pcf_attributes =
      mapper.attributes mapper env class_field.pcf_attributes
    in
    { pcf_desc; pcf_loc; pcf_attributes }
  ;;

  let map_include_declaration mapper env include_declaration =
    mapper.include_infos
      mapper
      (mapper.module_expr mapper)
      env
      include_declaration
  ;;

  let map_attributes mapper env attributes =
    map_list (mapper.attribute mapper) env attributes
  ;;

  let map_class_type_declaration mapper env class_type_declaration =
    mapper.class_infos
      mapper
      (mapper.class_type mapper)
      env
      class_type_declaration
  ;;

  let map_open_description mapper env open_description =
    mapper.open_infos
      mapper
      (mapper.location_loc mapper (mapper.longident mapper))
      env
      open_description
  ;;

  let map_mutable_flag mapper env mutable_flag =
    match mutable_flag with
    | Immutable -> Immutable
    | Mutable -> Mutable
  ;;

  let map_direction_flag mapper env direction_flag =
    match direction_flag with
    | Upto -> Upto
    | Downto -> Downto
  ;;

  let map_pattern_desc mapper env pattern_desc =
    match pattern_desc with
    | Ppat_any -> Ppat_any
    | Ppat_var _arg_0 -> Ppat_var (mapper.location_loc mapper id_map env _arg_0)
    | Ppat_alias (_arg_0, _arg_1) ->
      Ppat_alias
        ( mapper.pattern mapper env _arg_0
        , mapper.location_loc mapper id_map env _arg_1 )
    | Ppat_constant _arg_0 -> Ppat_constant (mapper.constant mapper env _arg_0)
    | Ppat_interval (_arg_0, _arg_1) ->
      Ppat_interval
        (mapper.constant mapper env _arg_0, mapper.constant mapper env _arg_1)
    | Ppat_tuple (_arg_0, _arg_1) ->
      Ppat_tuple
        ( map_list
            (fun env __arg ->
              let _0, _1 = __arg in
              map_option id_map env _0, mapper.pattern mapper env _1)
            env
            _arg_0
        , mapper.closed_flag mapper env _arg_1 )
    | Ppat_unboxed_tuple (_arg_0, _arg_1) ->
      Ppat_unboxed_tuple
        ( map_list
            (fun env __arg ->
              let _0, _1 = __arg in
              map_option id_map env _0, mapper.pattern mapper env _1)
            env
            _arg_0
        , mapper.closed_flag mapper env _arg_1 )
    | Ppat_construct (_arg_0, _arg_1) ->
      Ppat_construct
        ( mapper.location_loc mapper (mapper.longident mapper) env _arg_0
        , map_option
            (fun env __arg ->
              let _0, _1 = __arg in
              ( map_list
                  (fun env __arg ->
                    let _0, _1 = __arg in
                    ( mapper.location_loc mapper id_map env _0
                    , map_option (mapper.jkind_annotation mapper) env _1 ))
                  env
                  _0
              , mapper.pattern mapper env _1 ))
            env
            _arg_1 )
    | Ppat_variant (_arg_0, _arg_1) ->
      Ppat_variant
        ( mapper.label mapper env _arg_0
        , map_option (mapper.pattern mapper) env _arg_1 )
    | Ppat_record (_arg_0, _arg_1) ->
      Ppat_record
        ( map_list
            (fun env __arg ->
              let _0, _1 = __arg in
              ( mapper.location_loc mapper (mapper.longident mapper) env _0
              , mapper.pattern mapper env _1 ))
            env
            _arg_0
        , mapper.closed_flag mapper env _arg_1 )
    | Ppat_record_unboxed_product (_arg_0, _arg_1) ->
      Ppat_record_unboxed_product
        ( map_list
            (fun env __arg ->
              let _0, _1 = __arg in
              ( mapper.location_loc mapper (mapper.longident mapper) env _0
              , mapper.pattern mapper env _1 ))
            env
            _arg_0
        , mapper.closed_flag mapper env _arg_1 )
    | Ppat_array (_arg_0, _arg_1) ->
      Ppat_array
        ( mapper.mutable_flag mapper env _arg_0
        , map_list (mapper.pattern mapper) env _arg_1 )
    | Ppat_or (_arg_0, _arg_1) ->
      Ppat_or
        (mapper.pattern mapper env _arg_0, mapper.pattern mapper env _arg_1)
    | Ppat_constraint (_arg_0, _arg_1, _arg_2) ->
      Ppat_constraint
        ( mapper.pattern mapper env _arg_0
        , map_option (mapper.core_type mapper) env _arg_1
        , mapper.modes mapper env _arg_2 )
    | Ppat_type _arg_0 ->
      Ppat_type
        (mapper.location_loc mapper (mapper.longident mapper) env _arg_0)
    | Ppat_lazy _arg_0 -> Ppat_lazy (mapper.pattern mapper env _arg_0)
    | Ppat_unpack _arg_0 ->
      Ppat_unpack (mapper.location_loc mapper (map_option id_map) env _arg_0)
    | Ppat_exception _arg_0 -> Ppat_exception (mapper.pattern mapper env _arg_0)
    | Ppat_extension _arg_0 ->
      Ppat_extension (mapper.extension mapper env _arg_0)
    | Ppat_open (_arg_0, _arg_1) ->
      Ppat_open
        ( mapper.location_loc mapper (mapper.longident mapper) env _arg_0
        , mapper.pattern mapper env _arg_1 )
  ;;

  let map_block_access mapper env block_access =
    match block_access with
    | Baccess_field _arg_0 ->
      Baccess_field
        (mapper.location_loc mapper (mapper.longident mapper) env _arg_0)
    | Baccess_array (_arg_0, _arg_1, _arg_2) ->
      Baccess_array
        ( mapper.mutable_flag mapper env _arg_0
        , mapper.index_kind mapper env _arg_1
        , mapper.expression mapper env _arg_2 )
    | Baccess_block (_arg_0, _arg_1) ->
      Baccess_block
        ( mapper.mutable_flag mapper env _arg_0
        , mapper.expression mapper env _arg_1 )
  ;;

  let map_module_expr mapper env module_expr =
    let pmod_desc = mapper.module_expr_desc mapper env module_expr.pmod_desc in
    let pmod_loc = mapper.location mapper env module_expr.pmod_loc in
    let pmod_attributes =
      mapper.attributes mapper env module_expr.pmod_attributes
    in
    { pmod_desc; pmod_loc; pmod_attributes }
  ;;

  let map_module_binding mapper env module_binding =
    let pmb_name =
      mapper.location_loc mapper (map_option id_map) env module_binding.pmb_name
    in
    let pmb_expr = mapper.module_expr mapper env module_binding.pmb_expr in
    let pmb_attributes =
      mapper.attributes mapper env module_binding.pmb_attributes
    in
    let pmb_loc = mapper.location mapper env module_binding.pmb_loc in
    { pmb_name; pmb_expr; pmb_attributes; pmb_loc }
  ;;

  let map_location_loc mapper map_'a env loc =
    let txt = map_'a env loc.Location.txt in
    let loc = mapper.location mapper env loc.Location.loc in
    { Location.txt; loc }
  ;;

  let map_constructor_argument mapper env constructor_argument =
    let pca_modalities =
      mapper.modalities mapper env constructor_argument.pca_modalities
    in
    let pca_type = mapper.core_type mapper env constructor_argument.pca_type in
    let pca_loc = mapper.location mapper env constructor_argument.pca_loc in
    { pca_modalities; pca_type; pca_loc }
  ;;

  let map_class_expr mapper env class_expr =
    let pcl_desc = mapper.class_expr_desc mapper env class_expr.pcl_desc in
    let pcl_loc = mapper.location mapper env class_expr.pcl_loc in
    let pcl_attributes =
      mapper.attributes mapper env class_expr.pcl_attributes
    in
    { pcl_desc; pcl_loc; pcl_attributes }
  ;;

  let map_label_declaration mapper env label_declaration =
    let pld_name =
      mapper.location_loc mapper id_map env label_declaration.pld_name
    in
    let pld_mutable =
      mapper.mutable_flag mapper env label_declaration.pld_mutable
    in
    let pld_modalities =
      mapper.modalities mapper env label_declaration.pld_modalities
    in
    let pld_type = mapper.core_type mapper env label_declaration.pld_type in
    let pld_loc = mapper.location mapper env label_declaration.pld_loc in
    let pld_attributes =
      mapper.attributes mapper env label_declaration.pld_attributes
    in
    { pld_name; pld_mutable; pld_modalities; pld_type; pld_loc; pld_attributes }
  ;;

  let map_signature mapper env signature =
    let psg_modalities =
      mapper.modalities mapper env signature.psg_modalities
    in
    let psg_items =
      map_list (mapper.signature_item mapper) env signature.psg_items
    in
    let psg_loc = mapper.location mapper env signature.psg_loc in
    { psg_modalities; psg_items; psg_loc }
  ;;

  let map_expression mapper env expression =
    let pexp_desc = mapper.expression_desc mapper env expression.pexp_desc in
    let pexp_loc = mapper.location mapper env expression.pexp_loc in
    let pexp_loc_stack =
      mapper.location_stack mapper env expression.pexp_loc_stack
    in
    let pexp_attributes =
      mapper.attributes mapper env expression.pexp_attributes
    in
    { pexp_desc; pexp_loc; pexp_loc_stack; pexp_attributes }
  ;;

  let map_class_field_kind mapper env class_field_kind =
    match class_field_kind with
    | Cfk_virtual _arg_0 -> Cfk_virtual (mapper.core_type mapper env _arg_0)
    | Cfk_concrete (_arg_0, _arg_1) ->
      Cfk_concrete
        ( mapper.override_flag mapper env _arg_0
        , mapper.expression mapper env _arg_1 )
  ;;

  let map_comprehension mapper env comprehension =
    let pcomp_body = mapper.expression mapper env comprehension.pcomp_body in
    let pcomp_clauses =
      map_list
        (mapper.comprehension_clause mapper)
        env
        comprehension.pcomp_clauses
    in
    { pcomp_body; pcomp_clauses }
  ;;

  let map_type_declaration mapper env type_declaration =
    let ptype_name =
      mapper.location_loc mapper id_map env type_declaration.ptype_name
    in
    let ptype_params =
      map_list
        (fun env __arg ->
          let _0, _1 = __arg in
          ( mapper.core_type mapper env _0
          , let _0, _1 = _1 in
            mapper.variance mapper env _0, mapper.injectivity mapper env _1 ))
        env
        type_declaration.ptype_params
    in
    let ptype_cstrs =
      map_list
        (fun env __arg ->
          let _0, _1, _2 = __arg in
          ( mapper.core_type mapper env _0
          , mapper.core_type mapper env _1
          , mapper.location mapper env _2 ))
        env
        type_declaration.ptype_cstrs
    in
    let ptype_kind = mapper.type_kind mapper env type_declaration.ptype_kind in
    let ptype_private =
      mapper.private_flag mapper env type_declaration.ptype_private
    in
    let ptype_manifest =
      map_option (mapper.core_type mapper) env type_declaration.ptype_manifest
    in
    let ptype_attributes =
      mapper.attributes mapper env type_declaration.ptype_attributes
    in
    let ptype_jkind_annotation =
      map_option
        (mapper.jkind_annotation mapper)
        env
        type_declaration.ptype_jkind_annotation
    in
    let ptype_loc = mapper.location mapper env type_declaration.ptype_loc in
    { ptype_name
    ; ptype_params
    ; ptype_cstrs
    ; ptype_kind
    ; ptype_private
    ; ptype_manifest
    ; ptype_attributes
    ; ptype_jkind_annotation
    ; ptype_loc
    }
  ;;

  let map_function_param_desc mapper env function_param_desc =
    match function_param_desc with
    | Pparam_val (_arg_0, _arg_1, _arg_2) ->
      Pparam_val
        ( mapper.arg_label mapper env _arg_0
        , map_option (mapper.expression mapper) env _arg_1
        , mapper.pattern mapper env _arg_2 )
    | Pparam_newtype (_arg_0, _arg_1) ->
      Pparam_newtype
        ( mapper.location_loc mapper id_map env _arg_0
        , map_option (mapper.jkind_annotation mapper) env _arg_1 )
  ;;

  let map_type_constraint mapper env type_constraint =
    match type_constraint with
    | Pconstraint _arg_0 -> Pconstraint (mapper.core_type mapper env _arg_0)
    | Pcoerce (_arg_0, _arg_1) ->
      Pcoerce
        ( map_option (mapper.core_type mapper) env _arg_0
        , mapper.core_type mapper env _arg_1 )
  ;;

  let map_with_constraint mapper env with_constraint =
    match with_constraint with
    | Pwith_type (_arg_0, _arg_1) ->
      Pwith_type
        ( mapper.location_loc mapper (mapper.longident mapper) env _arg_0
        , mapper.type_declaration mapper env _arg_1 )
    | Pwith_module (_arg_0, _arg_1) ->
      Pwith_module
        ( mapper.location_loc mapper (mapper.longident mapper) env _arg_0
        , mapper.location_loc mapper (mapper.longident mapper) env _arg_1 )
    | Pwith_modtype (_arg_0, _arg_1) ->
      Pwith_modtype
        ( mapper.location_loc mapper (mapper.longident mapper) env _arg_0
        , mapper.module_type mapper env _arg_1 )
    | Pwith_modtypesubst (_arg_0, _arg_1) ->
      Pwith_modtypesubst
        ( mapper.location_loc mapper (mapper.longident mapper) env _arg_0
        , mapper.module_type mapper env _arg_1 )
    | Pwith_typesubst (_arg_0, _arg_1) ->
      Pwith_typesubst
        ( mapper.location_loc mapper (mapper.longident mapper) env _arg_0
        , mapper.type_declaration mapper env _arg_1 )
    | Pwith_modsubst (_arg_0, _arg_1) ->
      Pwith_modsubst
        ( mapper.location_loc mapper (mapper.longident mapper) env _arg_0
        , mapper.location_loc mapper (mapper.longident mapper) env _arg_1 )
  ;;

  let map_comprehension_clause_binding mapper env comprehension_clause_binding =
    let pcomp_cb_pattern =
      mapper.pattern mapper env comprehension_clause_binding.pcomp_cb_pattern
    in
    let pcomp_cb_iterator =
      mapper.comprehension_iterator
        mapper
        env
        comprehension_clause_binding.pcomp_cb_iterator
    in
    let pcomp_cb_attributes =
      mapper.attributes
        mapper
        env
        comprehension_clause_binding.pcomp_cb_attributes
    in
    { pcomp_cb_pattern; pcomp_cb_iterator; pcomp_cb_attributes }
  ;;

  let map_object_field_desc mapper env object_field_desc =
    match object_field_desc with
    | Otag (_arg_0, _arg_1) ->
      Otag
        ( mapper.location_loc mapper (mapper.label mapper) env _arg_0
        , mapper.core_type mapper env _arg_1 )
    | Oinherit _arg_0 -> Oinherit (mapper.core_type mapper env _arg_0)
  ;;

  let map_expression_desc mapper env expression_desc =
    match expression_desc with
    | Pexp_ident _arg_0 ->
      Pexp_ident
        (mapper.location_loc mapper (mapper.longident mapper) env _arg_0)
    | Pexp_constant _arg_0 -> Pexp_constant (mapper.constant mapper env _arg_0)
    | Pexp_let (_arg_0, _arg_1, _arg_2, _arg_3) ->
      Pexp_let
        ( mapper.mutable_flag mapper env _arg_0
        , mapper.rec_flag mapper env _arg_1
        , map_list (mapper.value_binding mapper) env _arg_2
        , mapper.expression mapper env _arg_3 )
    | Pexp_function (_arg_0, _arg_1, _arg_2) ->
      Pexp_function
        ( map_list (mapper.function_param mapper) env _arg_0
        , mapper.function_constraint mapper env _arg_1
        , mapper.function_body mapper env _arg_2 )
    | Pexp_apply (_arg_0, _arg_1) ->
      Pexp_apply
        ( mapper.expression mapper env _arg_0
        , map_list
            (fun env __arg ->
              let _0, _1 = __arg in
              mapper.arg_label mapper env _0, mapper.expression mapper env _1)
            env
            _arg_1 )
    | Pexp_match (_arg_0, _arg_1) ->
      Pexp_match
        ( mapper.expression mapper env _arg_0
        , map_list (mapper.case mapper) env _arg_1 )
    | Pexp_try (_arg_0, _arg_1) ->
      Pexp_try
        ( mapper.expression mapper env _arg_0
        , map_list (mapper.case mapper) env _arg_1 )
    | Pexp_tuple _arg_0 ->
      Pexp_tuple
        (map_list
           (fun env __arg ->
             let _0, _1 = __arg in
             map_option id_map env _0, mapper.expression mapper env _1)
           env
           _arg_0)
    | Pexp_unboxed_tuple _arg_0 ->
      Pexp_unboxed_tuple
        (map_list
           (fun env __arg ->
             let _0, _1 = __arg in
             map_option id_map env _0, mapper.expression mapper env _1)
           env
           _arg_0)
    | Pexp_construct (_arg_0, _arg_1) ->
      Pexp_construct
        ( mapper.location_loc mapper (mapper.longident mapper) env _arg_0
        , map_option (mapper.expression mapper) env _arg_1 )
    | Pexp_variant (_arg_0, _arg_1) ->
      Pexp_variant
        ( mapper.label mapper env _arg_0
        , map_option (mapper.expression mapper) env _arg_1 )
    | Pexp_record (_arg_0, _arg_1) ->
      Pexp_record
        ( map_list
            (fun env __arg ->
              let _0, _1 = __arg in
              ( mapper.location_loc mapper (mapper.longident mapper) env _0
              , mapper.expression mapper env _1 ))
            env
            _arg_0
        , map_option (mapper.expression mapper) env _arg_1 )
    | Pexp_record_unboxed_product (_arg_0, _arg_1) ->
      Pexp_record_unboxed_product
        ( map_list
            (fun env __arg ->
              let _0, _1 = __arg in
              ( mapper.location_loc mapper (mapper.longident mapper) env _0
              , mapper.expression mapper env _1 ))
            env
            _arg_0
        , map_option (mapper.expression mapper) env _arg_1 )
    | Pexp_field (_arg_0, _arg_1) ->
      Pexp_field
        ( mapper.expression mapper env _arg_0
        , mapper.location_loc mapper (mapper.longident mapper) env _arg_1 )
    | Pexp_unboxed_field (_arg_0, _arg_1) ->
      Pexp_unboxed_field
        ( mapper.expression mapper env _arg_0
        , mapper.location_loc mapper (mapper.longident mapper) env _arg_1 )
    | Pexp_setfield (_arg_0, _arg_1, _arg_2) ->
      Pexp_setfield
        ( mapper.expression mapper env _arg_0
        , mapper.location_loc mapper (mapper.longident mapper) env _arg_1
        , mapper.expression mapper env _arg_2 )
    | Pexp_array (_arg_0, _arg_1) ->
      Pexp_array
        ( mapper.mutable_flag mapper env _arg_0
        , map_list (mapper.expression mapper) env _arg_1 )
    | Pexp_idx (_arg_0, _arg_1) ->
      Pexp_idx
        ( mapper.block_access mapper env _arg_0
        , map_list (mapper.unboxed_access mapper) env _arg_1 )
    | Pexp_ifthenelse (_arg_0, _arg_1, _arg_2) ->
      Pexp_ifthenelse
        ( mapper.expression mapper env _arg_0
        , mapper.expression mapper env _arg_1
        , map_option (mapper.expression mapper) env _arg_2 )
    | Pexp_sequence (_arg_0, _arg_1) ->
      Pexp_sequence
        ( mapper.expression mapper env _arg_0
        , mapper.expression mapper env _arg_1 )
    | Pexp_while (_arg_0, _arg_1) ->
      Pexp_while
        ( mapper.expression mapper env _arg_0
        , mapper.expression mapper env _arg_1 )
    | Pexp_for (_arg_0, _arg_1, _arg_2, _arg_3, _arg_4) ->
      Pexp_for
        ( mapper.pattern mapper env _arg_0
        , mapper.expression mapper env _arg_1
        , mapper.expression mapper env _arg_2
        , mapper.direction_flag mapper env _arg_3
        , mapper.expression mapper env _arg_4 )
    | Pexp_constraint (_arg_0, _arg_1, _arg_2) ->
      Pexp_constraint
        ( mapper.expression mapper env _arg_0
        , map_option (mapper.core_type mapper) env _arg_1
        , mapper.modes mapper env _arg_2 )
    | Pexp_coerce (_arg_0, _arg_1, _arg_2) ->
      Pexp_coerce
        ( mapper.expression mapper env _arg_0
        , map_option (mapper.core_type mapper) env _arg_1
        , mapper.core_type mapper env _arg_2 )
    | Pexp_send (_arg_0, _arg_1) ->
      Pexp_send
        ( mapper.expression mapper env _arg_0
        , mapper.location_loc mapper (mapper.label mapper) env _arg_1 )
    | Pexp_new _arg_0 ->
      Pexp_new (mapper.location_loc mapper (mapper.longident mapper) env _arg_0)
    | Pexp_setvar (_arg_0, _arg_1) ->
      Pexp_setvar
        ( mapper.location_loc mapper (mapper.label mapper) env _arg_0
        , mapper.expression mapper env _arg_1 )
    | Pexp_override _arg_0 ->
      Pexp_override
        (map_list
           (fun env __arg ->
             let _0, _1 = __arg in
             ( mapper.location_loc mapper (mapper.label mapper) env _0
             , mapper.expression mapper env _1 ))
           env
           _arg_0)
    | Pexp_letmodule (_arg_0, _arg_1, _arg_2) ->
      Pexp_letmodule
        ( mapper.location_loc mapper (map_option id_map) env _arg_0
        , mapper.module_expr mapper env _arg_1
        , mapper.expression mapper env _arg_2 )
    | Pexp_letexception (_arg_0, _arg_1) ->
      Pexp_letexception
        ( mapper.extension_constructor mapper env _arg_0
        , mapper.expression mapper env _arg_1 )
    | Pexp_assert _arg_0 -> Pexp_assert (mapper.expression mapper env _arg_0)
    | Pexp_lazy _arg_0 -> Pexp_lazy (mapper.expression mapper env _arg_0)
    | Pexp_poly (_arg_0, _arg_1) ->
      Pexp_poly
        ( mapper.expression mapper env _arg_0
        , map_option (mapper.core_type mapper) env _arg_1 )
    | Pexp_object _arg_0 ->
      Pexp_object (mapper.class_structure mapper env _arg_0)
    | Pexp_newtype (_arg_0, _arg_1, _arg_2) ->
      Pexp_newtype
        ( mapper.location_loc mapper id_map env _arg_0
        , map_option (mapper.jkind_annotation mapper) env _arg_1
        , mapper.expression mapper env _arg_2 )
    | Pexp_pack _arg_0 -> Pexp_pack (mapper.module_expr mapper env _arg_0)
    | Pexp_open (_arg_0, _arg_1) ->
      Pexp_open
        ( mapper.open_declaration mapper env _arg_0
        , mapper.expression mapper env _arg_1 )
    | Pexp_letop _arg_0 -> Pexp_letop (mapper.letop mapper env _arg_0)
    | Pexp_extension _arg_0 ->
      Pexp_extension (mapper.extension mapper env _arg_0)
    | Pexp_unreachable -> Pexp_unreachable
    | Pexp_stack _arg_0 -> Pexp_stack (mapper.expression mapper env _arg_0)
    | Pexp_comprehension _arg_0 ->
      Pexp_comprehension (mapper.comprehension_expression mapper env _arg_0)
    | Pexp_overwrite (_arg_0, _arg_1) ->
      Pexp_overwrite
        ( mapper.expression mapper env _arg_0
        , mapper.expression mapper env _arg_1 )
    | Pexp_quote _arg_0 -> Pexp_quote (mapper.expression mapper env _arg_0)
    | Pexp_splice _arg_0 -> Pexp_splice (mapper.expression mapper env _arg_0)
    | Pexp_hole -> Pexp_hole
  ;;

  let map_directive_argument_desc mapper env directive_argument_desc =
    match directive_argument_desc with
    | Pdir_string _arg_0 -> Pdir_string (id_map env _arg_0)
    | Pdir_int (_arg_0, _arg_1) ->
      Pdir_int (id_map env _arg_0, map_option id_map env _arg_1)
    | Pdir_ident _arg_0 -> Pdir_ident (mapper.longident mapper env _arg_0)
    | Pdir_bool _arg_0 -> Pdir_bool (id_map env _arg_0)
  ;;

  let map_location_report_kind mapper env report_kind =
    match report_kind with
    | Location.Report_error -> Location.Report_error
    | Location.Report_warning _arg_0 ->
      Location.Report_warning (id_map env _arg_0)
    | Location.Report_warning_as_error _arg_0 ->
      Location.Report_warning_as_error (id_map env _arg_0)
    | Location.Report_alert _arg_0 -> Location.Report_alert (id_map env _arg_0)
    | Location.Report_alert_as_error _arg_0 ->
      Location.Report_alert_as_error (id_map env _arg_0)
  ;;

  let map_modalities mapper env modalities =
    map_list
      (mapper.location_loc mapper (mapper.modality mapper))
      env
      modalities
  ;;

  let map_attribute mapper env attribute =
    let attr_name = mapper.location_loc mapper id_map env attribute.attr_name in
    let attr_payload = mapper.payload mapper env attribute.attr_payload in
    let attr_loc = mapper.location mapper env attribute.attr_loc in
    { attr_name; attr_payload; attr_loc }
  ;;

  let map_comprehension_expression mapper env comprehension_expression =
    match comprehension_expression with
    | Pcomp_list_comprehension _arg_0 ->
      Pcomp_list_comprehension (mapper.comprehension mapper env _arg_0)
    | Pcomp_array_comprehension (_arg_0, _arg_1) ->
      Pcomp_array_comprehension
        ( mapper.mutable_flag mapper env _arg_0
        , mapper.comprehension mapper env _arg_1 )
  ;;

  let map_class_infos mapper map_'a env class_infos =
    let pci_virt = mapper.virtual_flag mapper env class_infos.pci_virt in
    let pci_params =
      map_list
        (fun env __arg ->
          let _0, _1 = __arg in
          ( mapper.core_type mapper env _0
          , let _0, _1 = _1 in
            mapper.variance mapper env _0, mapper.injectivity mapper env _1 ))
        env
        class_infos.pci_params
    in
    let pci_name = mapper.location_loc mapper id_map env class_infos.pci_name in
    let pci_expr = map_'a env class_infos.pci_expr in
    let pci_loc = mapper.location mapper env class_infos.pci_loc in
    let pci_attributes =
      mapper.attributes mapper env class_infos.pci_attributes
    in
    { pci_virt; pci_params; pci_name; pci_expr; pci_loc; pci_attributes }
  ;;

  let map_private_flag mapper env private_flag =
    match private_flag with
    | Private -> Private
    | Public -> Public
  ;;

  let map_class_structure mapper env class_structure =
    let pcstr_self = mapper.pattern mapper env class_structure.pcstr_self in
    let pcstr_fields =
      map_list (mapper.class_field mapper) env class_structure.pcstr_fields
    in
    { pcstr_self; pcstr_fields }
  ;;

  let map_comprehension_iterator mapper env comprehension_iterator =
    match comprehension_iterator with
    | Pcomp_range _arg ->
      let start = mapper.expression mapper env _arg.start in
      let stop = mapper.expression mapper env _arg.stop in
      let direction = mapper.direction_flag mapper env _arg.direction in
      Pcomp_range { start; stop; direction }
    | Pcomp_in _arg_0 -> Pcomp_in (mapper.expression mapper env _arg_0)
  ;;

  let map_module_substitution mapper env module_substitution =
    let pms_name =
      mapper.location_loc mapper id_map env module_substitution.pms_name
    in
    let pms_manifest =
      mapper.location_loc
        mapper
        (mapper.longident mapper)
        env
        module_substitution.pms_manifest
    in
    let pms_attributes =
      mapper.attributes mapper env module_substitution.pms_attributes
    in
    let pms_loc = mapper.location mapper env module_substitution.pms_loc in
    { pms_name; pms_manifest; pms_attributes; pms_loc }
  ;;

  let map_label mapper env label = id_map env label

  let map_open_infos mapper map_'a env open_infos =
    let popen_expr = map_'a env open_infos.popen_expr in
    let popen_override =
      mapper.override_flag mapper env open_infos.popen_override
    in
    let popen_loc = mapper.location mapper env open_infos.popen_loc in
    let popen_attributes =
      mapper.attributes mapper env open_infos.popen_attributes
    in
    { popen_expr; popen_override; popen_loc; popen_attributes }
  ;;

  let map_index_kind mapper env index_kind =
    match index_kind with
    | Index_int -> Index_int
    | Index_unboxed_int64 -> Index_unboxed_int64
    | Index_unboxed_int32 -> Index_unboxed_int32
    | Index_unboxed_int16 -> Index_unboxed_int16
    | Index_unboxed_int8 -> Index_unboxed_int8
    | Index_unboxed_nativeint -> Index_unboxed_nativeint
  ;;

  let map_class_declaration mapper env class_declaration =
    mapper.class_infos mapper (mapper.class_expr mapper) env class_declaration
  ;;

  let map_constructor_arguments mapper env constructor_arguments =
    match constructor_arguments with
    | Pcstr_tuple _arg_0 ->
      Pcstr_tuple (map_list (mapper.constructor_argument mapper) env _arg_0)
    | Pcstr_record _arg_0 ->
      Pcstr_record (map_list (mapper.label_declaration mapper) env _arg_0)
  ;;

  let map_class_type_field_desc mapper env class_type_field_desc =
    match class_type_field_desc with
    | Pctf_inherit _arg_0 -> Pctf_inherit (mapper.class_type mapper env _arg_0)
    | Pctf_val _arg_0 ->
      Pctf_val
        (let _0, _1, _2, _3 = _arg_0 in
         ( mapper.location_loc mapper (mapper.label mapper) env _0
         , mapper.mutable_flag mapper env _1
         , mapper.virtual_flag mapper env _2
         , mapper.core_type mapper env _3 ))
    | Pctf_method _arg_0 ->
      Pctf_method
        (let _0, _1, _2, _3 = _arg_0 in
         ( mapper.location_loc mapper (mapper.label mapper) env _0
         , mapper.private_flag mapper env _1
         , mapper.virtual_flag mapper env _2
         , mapper.core_type mapper env _3 ))
    | Pctf_constraint _arg_0 ->
      Pctf_constraint
        (let _0, _1 = _arg_0 in
         mapper.core_type mapper env _0, mapper.core_type mapper env _1)
    | Pctf_attribute _arg_0 ->
      Pctf_attribute (mapper.attribute mapper env _arg_0)
    | Pctf_extension _arg_0 ->
      Pctf_extension (mapper.extension mapper env _arg_0)
  ;;

  let map_module_type mapper env module_type =
    let pmty_desc = mapper.module_type_desc mapper env module_type.pmty_desc in
    let pmty_loc = mapper.location mapper env module_type.pmty_loc in
    let pmty_attributes =
      mapper.attributes mapper env module_type.pmty_attributes
    in
    { pmty_desc; pmty_loc; pmty_attributes }
  ;;

  let map_extension_constructor mapper env extension_constructor =
    let pext_name =
      mapper.location_loc mapper id_map env extension_constructor.pext_name
    in
    let pext_kind =
      mapper.extension_constructor_kind
        mapper
        env
        extension_constructor.pext_kind
    in
    let pext_loc = mapper.location mapper env extension_constructor.pext_loc in
    let pext_attributes =
      mapper.attributes mapper env extension_constructor.pext_attributes
    in
    { pext_name; pext_kind; pext_loc; pext_attributes }
  ;;
end [@ocaml.warning "-27"]

let default_mapper =
  { class_expr_desc = map_class_expr_desc
  ; injectivity = map_injectivity
  ; mode = map_mode
  ; modes = map_modes
  ; package_type = map_package_type
  ; class_signature = map_class_signature
  ; payload = map_payload
  ; directive_argument = map_directive_argument
  ; value_binding = map_value_binding
  ; pattern = map_pattern
  ; unboxed_access = map_unboxed_access
  ; functor_parameter = map_functor_parameter
  ; constructor_declaration = map_constructor_declaration
  ; jkind_annotation = map_jkind_annotation
  ; longident = map_longident
  ; location_msg = map_location_msg
  ; location_error = map_location_error
  ; constant = map_constant
  ; module_expr_desc = map_module_expr_desc
  ; structure_item = map_structure_item
  ; override_flag = map_override_flag
  ; class_type_desc = map_class_type_desc
  ; extension = map_extension
  ; rec_flag = map_rec_flag
  ; class_description = map_class_description
  ; core_type = map_core_type
  ; function_constraint = map_function_constraint
  ; extension_constructor_kind = map_extension_constructor_kind
  ; module_instance = map_module_instance
  ; structure = map_structure
  ; toplevel_directive = map_toplevel_directive
  ; module_declaration = map_module_declaration
  ; module_type_declaration = map_module_type_declaration
  ; variance = map_variance
  ; class_type_field = map_class_type_field
  ; letop = map_letop
  ; location = map_location
  ; function_param = map_function_param
  ; signature_item_desc = map_signature_item_desc
  ; binding_op = map_binding_op
  ; open_declaration = map_open_declaration
  ; jkind_annotation_desc = map_jkind_annotation_desc
  ; toplevel_phrase = map_toplevel_phrase
  ; arg_label = map_arg_label
  ; location_stack = map_location_stack
  ; value_constraint = map_value_constraint
  ; location_report = map_location_report
  ; comprehension_clause = map_comprehension_clause
  ; class_field_desc = map_class_field_desc
  ; core_type_desc = map_core_type_desc
  ; virtual_flag = map_virtual_flag
  ; module_type_desc = map_module_type_desc
  ; row_field = map_row_field
  ; case = map_case
  ; type_extension = map_type_extension
  ; class_type = map_class_type
  ; signature_item = map_signature_item
  ; include_description = map_include_description
  ; closed_flag = map_closed_flag
  ; type_kind = map_type_kind
  ; modality = map_modality
  ; object_field = map_object_field
  ; include_infos = map_include_infos
  ; structure_item_desc = map_structure_item_desc
  ; include_kind = map_include_kind
  ; row_field_desc = map_row_field_desc
  ; location_report_printer = map_location_report_printer
  ; value_description = map_value_description
  ; type_exception = map_type_exception
  ; function_body = map_function_body
  ; class_field = map_class_field
  ; include_declaration = map_include_declaration
  ; attributes = map_attributes
  ; class_type_declaration = map_class_type_declaration
  ; open_description = map_open_description
  ; mutable_flag = map_mutable_flag
  ; direction_flag = map_direction_flag
  ; pattern_desc = map_pattern_desc
  ; block_access = map_block_access
  ; module_expr = map_module_expr
  ; module_binding = map_module_binding
  ; location_loc = map_location_loc
  ; constructor_argument = map_constructor_argument
  ; class_expr = map_class_expr
  ; label_declaration = map_label_declaration
  ; signature = map_signature
  ; expression = map_expression
  ; class_field_kind = map_class_field_kind
  ; comprehension = map_comprehension
  ; type_declaration = map_type_declaration
  ; function_param_desc = map_function_param_desc
  ; type_constraint = map_type_constraint
  ; with_constraint = map_with_constraint
  ; comprehension_clause_binding = map_comprehension_clause_binding
  ; object_field_desc = map_object_field_desc
  ; expression_desc = map_expression_desc
  ; directive_argument_desc = map_directive_argument_desc
  ; location_report_kind = map_location_report_kind
  ; modalities = map_modalities
  ; attribute = map_attribute
  ; comprehension_expression = map_comprehension_expression
  ; class_infos = map_class_infos
  ; private_flag = map_private_flag
  ; class_structure = map_class_structure
  ; comprehension_iterator = map_comprehension_iterator
  ; module_substitution = map_module_substitution
  ; label = map_label
  ; open_infos = map_open_infos
  ; index_kind = map_index_kind
  ; class_declaration = map_class_declaration
  ; constructor_arguments = map_constructor_arguments
  ; class_type_field_desc = map_class_type_field_desc
  ; module_type = map_module_type
  ; extension_constructor = map_extension_constructor
  }
;;
