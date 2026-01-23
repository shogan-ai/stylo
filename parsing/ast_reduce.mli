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

val mk_default_reducer
  :  'result
  -> ('result -> 'result -> 'result)
  -> ('reducer_env, 'result) reducer
