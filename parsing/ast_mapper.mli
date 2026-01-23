open Parsetree
open Asttypes

type 'mapper_env mapper =
  { class_expr_desc :
      'mapper_env mapper -> 'mapper_env -> class_expr_desc -> class_expr_desc
  ; modes : 'mapper_env mapper -> 'mapper_env -> modes -> modes
  ; package_type :
      'mapper_env mapper -> 'mapper_env -> package_type -> package_type
  ; class_signature :
      'mapper_env mapper -> 'mapper_env -> class_signature -> class_signature
  ; tokens_comment :
      'mapper_env mapper -> 'mapper_env -> Tokens.comment -> Tokens.comment
  ; directive_argument :
      'mapper_env mapper
      -> 'mapper_env
      -> directive_argument
      -> directive_argument
  ; pattern : 'mapper_env mapper -> 'mapper_env -> pattern -> pattern
  ; constructor_declaration :
      'mapper_env mapper
      -> 'mapper_env
      -> constructor_declaration
      -> constructor_declaration
  ; module_expr_desc :
      'mapper_env mapper -> 'mapper_env -> module_expr_desc -> module_expr_desc
  ; structure_item :
      'mapper_env mapper -> 'mapper_env -> structure_item -> structure_item
  ; override_flag :
      'mapper_env mapper -> 'mapper_env -> override_flag -> override_flag
  ; extension : 'mapper_env mapper -> 'mapper_env -> extension -> extension
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
  ; structure : 'mapper_env mapper -> 'mapper_env -> structure -> structure
  ; toplevel_directive :
      'mapper_env mapper
      -> 'mapper_env
      -> toplevel_directive
      -> toplevel_directive
  ; module_declaration_body :
      'mapper_env mapper
      -> 'mapper_env
      -> module_declaration_body
      -> module_declaration_body
  ; module_declaration :
      'mapper_env mapper
      -> 'mapper_env
      -> module_declaration
      -> module_declaration
  ; variance : 'mapper_env mapper -> 'mapper_env -> variance -> variance
  ; signature_item_desc :
      'mapper_env mapper
      -> 'mapper_env
      -> signature_item_desc
      -> signature_item_desc
  ; binding_op : 'mapper_env mapper -> 'mapper_env -> binding_op -> binding_op
  ; record_field :
      'a
      .  'mapper_env mapper
      -> ('mapper_env -> 'a -> 'a)
      -> 'mapper_env
      -> 'a record_field
      -> 'a record_field
  ; arg_label : 'mapper_env mapper -> 'mapper_env -> arg_label -> arg_label
  ; comprehension_clause :
      'mapper_env mapper
      -> 'mapper_env
      -> comprehension_clause
      -> comprehension_clause
  ; class_field_desc :
      'mapper_env mapper -> 'mapper_env -> class_field_desc -> class_field_desc
  ; longident_str_or_op :
      'mapper_env mapper
      -> 'mapper_env
      -> Longident.str_or_op
      -> Longident.str_or_op
  ; core_type_desc :
      'mapper_env mapper -> 'mapper_env -> core_type_desc -> core_type_desc
  ; ptype_param :
      'mapper_env mapper -> 'mapper_env -> ptype_param -> ptype_param
  ; virtual_flag :
      'mapper_env mapper -> 'mapper_env -> virtual_flag -> virtual_flag
  ; module_type_desc :
      'mapper_env mapper -> 'mapper_env -> module_type_desc -> module_type_desc
  ; row_field : 'mapper_env mapper -> 'mapper_env -> row_field -> row_field
  ; class_type : 'mapper_env mapper -> 'mapper_env -> class_type -> class_type
  ; include_description :
      'mapper_env mapper
      -> 'mapper_env
      -> include_description
      -> include_description
  ; modality : 'mapper_env mapper -> 'mapper_env -> modality -> modality
  ; object_field :
      'mapper_env mapper -> 'mapper_env -> object_field -> object_field
  ; structure_item_desc :
      'mapper_env mapper
      -> 'mapper_env
      -> structure_item_desc
      -> structure_item_desc
  ; row_field_desc :
      'mapper_env mapper -> 'mapper_env -> row_field_desc -> row_field_desc
  ; type_exception :
      'mapper_env mapper -> 'mapper_env -> type_exception -> type_exception
  ; mutable_flag :
      'mapper_env mapper -> 'mapper_env -> mutable_flag -> mutable_flag
  ; direction_flag :
      'mapper_env mapper -> 'mapper_env -> direction_flag -> direction_flag
  ; block_access :
      'mapper_env mapper -> 'mapper_env -> block_access -> block_access
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
  ; tokens_seq : 'mapper_env mapper -> 'mapper_env -> Tokens.seq -> Tokens.seq
  ; class_field_kind :
      'mapper_env mapper -> 'mapper_env -> class_field_kind -> class_field_kind
  ; toplevel_extension :
      'mapper_env mapper
      -> 'mapper_env
      -> toplevel_extension
      -> toplevel_extension
  ; ptype_constraint :
      'mapper_env mapper -> 'mapper_env -> ptype_constraint -> ptype_constraint
  ; type_constraint :
      'mapper_env mapper -> 'mapper_env -> type_constraint -> type_constraint
  ; comprehension_clause_binding :
      'mapper_env mapper
      -> 'mapper_env
      -> comprehension_clause_binding
      -> comprehension_clause_binding
  ; tokens_elt : 'mapper_env mapper -> 'mapper_env -> Tokens.elt -> Tokens.elt
  ; object_field_desc :
      'mapper_env mapper
      -> 'mapper_env
      -> object_field_desc
      -> object_field_desc
  ; expression_desc :
      'mapper_env mapper -> 'mapper_env -> expression_desc -> expression_desc
  ; modalities : 'mapper_env mapper -> 'mapper_env -> modalities -> modalities
  ; class_infos :
      'a
      .  'mapper_env mapper
      -> ('mapper_env -> 'a -> 'a)
      -> 'mapper_env
      -> 'a class_infos
      -> 'a class_infos
  ; argument :
      'a
      .  'mapper_env mapper
      -> ('mapper_env -> 'a -> 'a)
      -> 'mapper_env
      -> 'a argument
      -> 'a argument
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
  ; constructor_arguments :
      'mapper_env mapper
      -> 'mapper_env
      -> constructor_arguments
      -> constructor_arguments
  ; module_type :
      'mapper_env mapper -> 'mapper_env -> module_type -> module_type
  ; injectivity :
      'mapper_env mapper -> 'mapper_env -> injectivity -> injectivity
  ; mode : 'mapper_env mapper -> 'mapper_env -> mode -> mode
  ; payload : 'mapper_env mapper -> 'mapper_env -> payload -> payload
  ; value_binding :
      'mapper_env mapper -> 'mapper_env -> value_binding -> value_binding
  ; unboxed_access :
      'mapper_env mapper -> 'mapper_env -> unboxed_access -> unboxed_access
  ; functor_parameter :
      'mapper_env mapper
      -> 'mapper_env
      -> functor_parameter
      -> functor_parameter
  ; longident_lid_desc :
      'mapper_env mapper
      -> 'mapper_env
      -> Longident.lid_desc
      -> Longident.lid_desc
  ; jkind_annotation :
      'mapper_env mapper -> 'mapper_env -> jkind_annotation -> jkind_annotation
  ; longident : 'mapper_env mapper -> 'mapper_env -> Longident.t -> Longident.t
  ; constant : 'mapper_env mapper -> 'mapper_env -> constant -> constant
  ; class_type_desc :
      'mapper_env mapper -> 'mapper_env -> class_type_desc -> class_type_desc
  ; rec_flag : 'mapper_env mapper -> 'mapper_env -> rec_flag -> rec_flag
  ; longident_dotop_delims :
      'mapper_env mapper
      -> 'mapper_env
      -> Longident.dotop_delims
      -> Longident.dotop_delims
  ; module_type_declaration :
      'mapper_env mapper
      -> 'mapper_env
      -> module_type_declaration
      -> module_type_declaration
  ; tokens_desc :
      'mapper_env mapper -> 'mapper_env -> Tokens.desc -> Tokens.desc
  ; class_type_field :
      'mapper_env mapper -> 'mapper_env -> class_type_field -> class_type_field
  ; letop : 'mapper_env mapper -> 'mapper_env -> letop -> letop
  ; location : 'mapper_env mapper -> 'mapper_env -> Location.t -> Location.t
  ; function_param :
      'mapper_env mapper -> 'mapper_env -> function_param -> function_param
  ; ptype_params :
      'mapper_env mapper -> 'mapper_env -> ptype_params -> ptype_params
  ; open_declaration :
      'mapper_env mapper -> 'mapper_env -> open_declaration -> open_declaration
  ; jkind_annotation_desc :
      'mapper_env mapper
      -> 'mapper_env
      -> jkind_annotation_desc
      -> jkind_annotation_desc
  ; toplevel_phrase :
      'mapper_env mapper -> 'mapper_env -> toplevel_phrase -> toplevel_phrase
  ; arrow_arg : 'mapper_env mapper -> 'mapper_env -> arrow_arg -> arrow_arg
  ; value_constraint :
      'mapper_env mapper -> 'mapper_env -> value_constraint -> value_constraint
  ; paren_kind : 'mapper_env mapper -> 'mapper_env -> paren_kind -> paren_kind
  ; case : 'mapper_env mapper -> 'mapper_env -> case -> case
  ; type_extension :
      'mapper_env mapper -> 'mapper_env -> type_extension -> type_extension
  ; signature_item :
      'mapper_env mapper -> 'mapper_env -> signature_item -> signature_item
  ; closed_flag :
      'mapper_env mapper -> 'mapper_env -> closed_flag -> closed_flag
  ; type_kind : 'mapper_env mapper -> 'mapper_env -> type_kind -> type_kind
  ; include_infos :
      'a
      .  'mapper_env mapper
      -> ('mapper_env -> 'a -> 'a)
      -> 'mapper_env
      -> 'a include_infos
      -> 'a include_infos
  ; include_kind :
      'mapper_env mapper -> 'mapper_env -> include_kind -> include_kind
  ; ext_attribute :
      'mapper_env mapper -> 'mapper_env -> ext_attribute -> ext_attribute
  ; value_description :
      'mapper_env mapper
      -> 'mapper_env
      -> value_description
      -> value_description
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
  ; pattern_desc :
      'mapper_env mapper -> 'mapper_env -> pattern_desc -> pattern_desc
  ; tokens_attachment :
      'mapper_env mapper
      -> 'mapper_env
      -> Tokens.attachment
      -> Tokens.attachment
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
  ; argument_desc :
      'a
      .  'mapper_env mapper
      -> ('mapper_env -> 'a -> 'a)
      -> 'mapper_env
      -> 'a argument_desc
      -> 'a argument_desc
  ; expression : 'mapper_env mapper -> 'mapper_env -> expression -> expression
  ; comprehension :
      'mapper_env mapper -> 'mapper_env -> comprehension -> comprehension
  ; type_declaration :
      'mapper_env mapper -> 'mapper_env -> type_declaration -> type_declaration
  ; function_param_desc :
      'mapper_env mapper
      -> 'mapper_env
      -> function_param_desc
      -> function_param_desc
  ; function_body_desc :
      'mapper_env mapper
      -> 'mapper_env
      -> function_body_desc
      -> function_body_desc
  ; with_constraint :
      'mapper_env mapper -> 'mapper_env -> with_constraint -> with_constraint
  ; with_constraint_desc :
      'mapper_env mapper
      -> 'mapper_env
      -> with_constraint_desc
      -> with_constraint_desc
  ; directive_argument_desc :
      'mapper_env mapper
      -> 'mapper_env
      -> directive_argument_desc
      -> directive_argument_desc
  ; attribute : 'mapper_env mapper -> 'mapper_env -> attribute -> attribute
  ; comprehension_expression :
      'mapper_env mapper
      -> 'mapper_env
      -> comprehension_expression
      -> comprehension_expression
  ; private_flag :
      'mapper_env mapper -> 'mapper_env -> private_flag -> private_flag
  ; class_structure :
      'mapper_env mapper -> 'mapper_env -> class_structure -> class_structure
  ; comprehension_iterator :
      'mapper_env mapper
      -> 'mapper_env
      -> comprehension_iterator
      -> comprehension_iterator
  ; index_kind : 'mapper_env mapper -> 'mapper_env -> index_kind -> index_kind
  ; class_declaration :
      'mapper_env mapper
      -> 'mapper_env
      -> class_declaration
      -> class_declaration
  ; class_type_field_desc :
      'mapper_env mapper
      -> 'mapper_env
      -> class_type_field_desc
      -> class_type_field_desc
  ; extension_constructor :
      'mapper_env mapper
      -> 'mapper_env
      -> extension_constructor
      -> extension_constructor
  }

val default_mapper : _ mapper
