open Ocaml_syntax
open Parsetree

module Argument : sig
  val generic_erase : 'a argument -> 'a argument

  val erase_function_param : pattern argument -> pattern argument
end

module Arrow_arg : sig
  val erase : arrow_arg -> arrow_arg
end

module Type_declaration : sig
  val erase : type_declaration -> type_declaration
end

module Constructor_argument : sig
  val erase : constructor_argument -> constructor_argument
end

module Label_declaration : sig
  val erase : label_declaration -> label_declaration
end

val expression : expression -> expression
val pattern : pattern -> pattern
val value_binding : value_binding -> value_binding
val core_type : core_type -> core_type
val bound_ty_var : bound_ty_var -> bound_ty_var
val ptype_param : ptype_param -> ptype_param
val value_description : value_description -> value_description
val signature : signature -> signature
val signature_item : signature_item -> signature_item
val structure : structure -> structure
val module_type : module_type -> module_type
val module_declaration : module_declaration -> module_declaration
val module_binding : module_binding -> module_binding
val module_expr : module_expr -> module_expr
val functor_parameter : functor_parameter -> functor_parameter
