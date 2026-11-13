(rule
  (target attributes__attribute_on_a_constructor_that_doesnt_fit.mli.out)
  (action
    (with-stdout-to %{target}
      (run %{project_root}/bin/main.exe style --idempotence-check --width 90 %{dep:attributes__attribute_on_a_constructor_that_doesnt_fit.mli}))))

(rule
  (alias runtest)
  (action (diff %{dep:attributes__attribute_on_a_constructor_that_doesnt_fit.mli.stylo} %{dep:attributes__attribute_on_a_constructor_that_doesnt_fit.mli.out})))

(rule
  (target attributes__attribute_on_a_type_that_doesnt_fit.ml.out)
  (action
    (with-stdout-to %{target}
      (run %{project_root}/bin/main.exe style --idempotence-check --width 90 %{dep:attributes__attribute_on_a_type_that_doesnt_fit.ml}))))

(rule
  (alias runtest)
  (action (diff %{dep:attributes__attribute_on_a_type_that_doesnt_fit.ml.stylo} %{dep:attributes__attribute_on_a_type_that_doesnt_fit.ml.out})))

(rule
  (target attributes__attribute_on_an_argument.ml.out)
  (action
    (with-stdout-to %{target}
      (run %{project_root}/bin/main.exe style --idempotence-check --width 90 %{dep:attributes__attribute_on_an_argument.ml}))))

(rule
  (alias runtest)
  (action (diff %{dep:attributes__attribute_on_an_argument.ml.stylo} %{dep:attributes__attribute_on_an_argument.ml.out})))

(rule
  (target attributes__attribute_right_after_a_type_in_parentheses.ml.out)
  (action
    (with-stdout-to %{target}
      (run %{project_root}/bin/main.exe style --idempotence-check --width 90 %{dep:attributes__attribute_right_after_a_type_in_parentheses.ml}))))

(rule
  (alias runtest)
  (action (diff %{dep:attributes__attribute_right_after_a_type_in_parentheses.ml.stylo} %{dep:attributes__attribute_right_after_a_type_in_parentheses.ml.out})))

(rule
  (target attributes__attributes_on_record_fields_and_constructors.ml.out)
  (action
    (with-stdout-to %{target}
      (run %{project_root}/bin/main.exe style --idempotence-check --width 90 %{dep:attributes__attributes_on_record_fields_and_constructors.ml}))))

(rule
  (alias runtest)
  (action (diff %{dep:attributes__attributes_on_record_fields_and_constructors.ml.stylo} %{dep:attributes__attributes_on_record_fields_and_constructors.ml.out})))

(rule
  (target attributes__record_fields_attribute_that_doesnt_fit.ml.out)
  (action
    (with-stdout-to %{target}
      (run %{project_root}/bin/main.exe style --idempotence-check --width 90 %{dep:attributes__record_fields_attribute_that_doesnt_fit.ml}))))

(rule
  (alias runtest)
  (action (diff %{dep:attributes__record_fields_attribute_that_doesnt_fit.ml.stylo} %{dep:attributes__record_fields_attribute_that_doesnt_fit.ml.out})))

(rule
  (target blank_lines__comment_at_the_start_of_a_struct.ml.out)
  (action
    (with-stdout-to %{target}
      (run %{project_root}/bin/main.exe style --idempotence-check --width 90 %{dep:blank_lines__comment_at_the_start_of_a_struct.ml}))))

(rule
  (alias runtest)
  (action (diff %{dep:blank_lines__comment_at_the_start_of_a_struct.ml.stylo} %{dep:blank_lines__comment_at_the_start_of_a_struct.ml.out})))

(rule
  (target blank_lines__doc_comment_at_the_start_of_a_file.ml.out)
  (action
    (with-stdout-to %{target}
      (run %{project_root}/bin/main.exe style --idempotence-check --width 90 %{dep:blank_lines__doc_comment_at_the_start_of_a_file.ml}))))

(rule
  (alias runtest)
  (action (diff %{dep:blank_lines__doc_comment_at_the_start_of_a_file.ml.stylo} %{dep:blank_lines__doc_comment_at_the_start_of_a_file.ml.out})))

(rule
  (target blank_lines__doc_comment_at_the_start_of_a_sig.ml.out)
  (action
    (with-stdout-to %{target}
      (run %{project_root}/bin/main.exe style --idempotence-check --width 90 %{dep:blank_lines__doc_comment_at_the_start_of_a_sig.ml}))))

(rule
  (alias runtest)
  (action (diff %{dep:blank_lines__doc_comment_at_the_start_of_a_sig.ml.stylo} %{dep:blank_lines__doc_comment_at_the_start_of_a_sig.ml.out})))

(rule
  (target blank_lines__doc_comment_on_the_last_constructor_before_deriving.mli.out)
  (action
    (with-stdout-to %{target}
      (run %{project_root}/bin/main.exe style --idempotence-check --width 90 %{dep:blank_lines__doc_comment_on_the_last_constructor_before_deriving.mli}))))

(rule
  (alias runtest)
  (action (diff %{dep:blank_lines__doc_comment_on_the_last_constructor_before_deriving.mli.stylo} %{dep:blank_lines__doc_comment_on_the_last_constructor_before_deriving.mli.out})))

(rule
  (target blank_lines__doc_comment_that_is_only_a_tag_after_the_only_item.mli.out)
  (action
    (with-stdout-to %{target}
      (run %{project_root}/bin/main.exe style --idempotence-check --width 90 %{dep:blank_lines__doc_comment_that_is_only_a_tag_after_the_only_item.mli}))))

(rule
  (alias runtest)
  (action (diff %{dep:blank_lines__doc_comment_that_is_only_a_tag_after_the_only_item.mli.stylo} %{dep:blank_lines__doc_comment_that_is_only_a_tag_after_the_only_item.mli.out})))

(rule
  (target blank_lines__empty_file.ml.out)
  (action
    (with-stdout-to %{target}
      (run %{project_root}/bin/main.exe style --idempotence-check --width 90 %{dep:blank_lines__empty_file.ml}))))

(rule
  (alias runtest)
  (action (diff %{dep:blank_lines__empty_file.ml.stylo} %{dep:blank_lines__empty_file.ml.out})))

(rule
  (target blank_lines__file_of_blank_lines.ml.out)
  (action
    (with-stdout-to %{target}
      (run %{project_root}/bin/main.exe style --idempotence-check --width 90 %{dep:blank_lines__file_of_blank_lines.ml}))))

(rule
  (alias runtest)
  (action (diff %{dep:blank_lines__file_of_blank_lines.ml.stylo} %{dep:blank_lines__file_of_blank_lines.ml.out})))

(rule
  (target blank_lines__list_inside_a_comment.ml.out)
  (action
    (with-stdout-to %{target}
      (run %{project_root}/bin/main.exe style --idempotence-check --width 90 %{dep:blank_lines__list_inside_a_comment.ml}))))

(rule
  (alias runtest)
  (action (diff %{dep:blank_lines__list_inside_a_comment.ml.stylo} %{dep:blank_lines__list_inside_a_comment.ml.out})))

(rule
  (target blankline_preservation.ml.out)
  (action
    (with-stdout-to %{target}
      (run %{project_root}/bin/main.exe style --idempotence-check --width 90 %{dep:blankline_preservation.ml}))))

(rule
  (alias runtest)
  (action (diff %{dep:blankline_preservation.ml.stylo} %{dep:blankline_preservation.ml.out})))

(rule
  (target case_and_comment.40.ml.out)
  (action
    (with-stdout-to %{target}
      (run %{project_root}/bin/main.exe style --idempotence-check --width 40 %{dep:case_and_comment.40.ml}))))

(rule
  (alias runtest)
  (action (diff %{dep:case_and_comment.40.ml.stylo} %{dep:case_and_comment.40.ml.out})))

(rule
  (target cinaps.ml.out)
  (action
    (with-stdout-to %{target}
      (run %{project_root}/bin/main.exe style --idempotence-check --width 90 %{dep:cinaps.ml}))))

(rule
  (alias runtest)
  (action (diff %{dep:cinaps.ml.stylo} %{dep:cinaps.ml.out})))

(rule
  (target comment_and_case.14.ml.out)
  (action
    (with-stdout-to %{target}
      (run %{project_root}/bin/main.exe style --idempotence-check --width 14 %{dep:comment_and_case.14.ml}))))

(rule
  (alias runtest)
  (action (diff %{dep:comment_and_case.14.ml.stylo} %{dep:comment_and_case.14.ml.out})))

(rule
  (target comments__comment_after_a_floating_attribute.ml.out)
  (action
    (with-stdout-to %{target}
      (run %{project_root}/bin/main.exe style --idempotence-check --width 90 %{dep:comments__comment_after_a_floating_attribute.ml}))))

(rule
  (alias runtest)
  (action (diff %{dep:comments__comment_after_a_floating_attribute.ml.stylo} %{dep:comments__comment_after_a_floating_attribute.ml.out})))

(rule
  (target comments__comment_after_a_labelled_argument.ml.out)
  (action
    (with-stdout-to %{target}
      (run %{project_root}/bin/main.exe style --idempotence-check --width 90 %{dep:comments__comment_after_a_labelled_argument.ml}))))

(rule
  (alias runtest)
  (action (diff %{dep:comments__comment_after_a_labelled_argument.ml.stylo} %{dep:comments__comment_after_a_labelled_argument.ml.out})))

(rule
  (target comments__comment_after_a_labelled_argument_whose_value_is_a_function.ml.out)
  (action
    (with-stdout-to %{target}
      (run %{project_root}/bin/main.exe style --idempotence-check --width 90 %{dep:comments__comment_after_a_labelled_argument_whose_value_is_a_function.ml}))))

(rule
  (alias runtest)
  (action (diff %{dep:comments__comment_after_a_labelled_argument_whose_value_is_a_function.ml.stylo} %{dep:comments__comment_after_a_labelled_argument_whose_value_is_a_function.ml.out})))

(rule
  (target comments__comment_after_a_record_field_before_a_semicolon.ml.out)
  (action
    (with-stdout-to %{target}
      (run %{project_root}/bin/main.exe style --idempotence-check --width 90 %{dep:comments__comment_after_a_record_field_before_a_semicolon.ml}))))

(rule
  (alias runtest)
  (action (diff %{dep:comments__comment_after_a_record_field_before_a_semicolon.ml.stylo} %{dep:comments__comment_after_a_record_field_before_a_semicolon.ml.out})))

(rule
  (target comments__comment_after_a_record_fields_attribute.ml.out)
  (action
    (with-stdout-to %{target}
      (run %{project_root}/bin/main.exe style --idempotence-check --width 90 %{dep:comments__comment_after_a_record_fields_attribute.ml}))))

(rule
  (alias runtest)
  (action (diff %{dep:comments__comment_after_a_record_fields_attribute.ml.stylo} %{dep:comments__comment_after_a_record_fields_attribute.ml.out})))

(rule
  (target comments__comment_after_a_record_fields_value.ml.out)
  (action
    (with-stdout-to %{target}
      (run %{project_root}/bin/main.exe style --idempotence-check --width 90 %{dep:comments__comment_after_a_record_fields_value.ml}))))

(rule
  (alias runtest)
  (action (diff %{dep:comments__comment_after_a_record_fields_value.ml.stylo} %{dep:comments__comment_after_a_record_fields_value.ml.out})))

(rule
  (target comments__comment_attached_to_an_or_pattern_alternative.ml.out)
  (action
    (with-stdout-to %{target}
      (run %{project_root}/bin/main.exe style --idempotence-check --width 90 %{dep:comments__comment_attached_to_an_or_pattern_alternative.ml}))))

(rule
  (alias runtest)
  (action (diff %{dep:comments__comment_attached_to_an_or_pattern_alternative.ml.stylo} %{dep:comments__comment_attached_to_an_or_pattern_alternative.ml.out})))

(rule
  (target comments__comment_before_a_closing_parenthesis.ml.out)
  (action
    (with-stdout-to %{target}
      (run %{project_root}/bin/main.exe style --idempotence-check --width 90 %{dep:comments__comment_before_a_closing_parenthesis.ml}))))

(rule
  (alias runtest)
  (action (diff %{dep:comments__comment_before_a_closing_parenthesis.ml.stylo} %{dep:comments__comment_before_a_closing_parenthesis.ml.out})))

(rule
  (target comments__comment_before_a_match.ml.out)
  (action
    (with-stdout-to %{target}
      (run %{project_root}/bin/main.exe style --idempotence-check --width 90 %{dep:comments__comment_before_a_match.ml}))))

(rule
  (alias runtest)
  (action (diff %{dep:comments__comment_before_a_match.ml.stylo} %{dep:comments__comment_before_a_match.ml.out})))

(rule
  (target comments__comment_before_a_mutable_record_field.ml.out)
  (action
    (with-stdout-to %{target}
      (run %{project_root}/bin/main.exe style --idempotence-check --width 90 %{dep:comments__comment_before_a_mutable_record_field.ml}))))

(rule
  (alias runtest)
  (action (diff %{dep:comments__comment_before_a_mutable_record_field.ml.stylo} %{dep:comments__comment_before_a_mutable_record_field.ml.out})))

(rule
  (target comments__comment_before_a_record_field.ml.out)
  (action
    (with-stdout-to %{target}
      (run %{project_root}/bin/main.exe style --idempotence-check --width 90 %{dep:comments__comment_before_a_record_field.ml}))))

(rule
  (alias runtest)
  (action (diff %{dep:comments__comment_before_a_record_field.ml.stylo} %{dep:comments__comment_before_a_record_field.ml.out})))

(rule
  (target comments__comment_before_a_record_field_in_a_record_that_fits_on_one_line.ml.out)
  (action
    (with-stdout-to %{target}
      (run %{project_root}/bin/main.exe style --idempotence-check --width 90 %{dep:comments__comment_before_a_record_field_in_a_record_that_fits_on_one_line.ml}))))

(rule
  (alias runtest)
  (action (diff %{dep:comments__comment_before_a_record_field_in_a_record_that_fits_on_one_line.ml.stylo} %{dep:comments__comment_before_a_record_field_in_a_record_that_fits_on_one_line.ml.out})))

(rule
  (target comments__comment_before_an_if.ml.out)
  (action
    (with-stdout-to %{target}
      (run %{project_root}/bin/main.exe style --idempotence-check --width 90 %{dep:comments__comment_before_an_if.ml}))))

(rule
  (alias runtest)
  (action (diff %{dep:comments__comment_before_an_if.ml.stylo} %{dep:comments__comment_before_an_if.ml.out})))

(rule
  (target comments__comment_just_before_end.ml.out)
  (action
    (with-stdout-to %{target}
      (run %{project_root}/bin/main.exe style --idempotence-check --width 90 %{dep:comments__comment_just_before_end.ml}))))

(rule
  (alias runtest)
  (action (diff %{dep:comments__comment_just_before_end.ml.stylo} %{dep:comments__comment_just_before_end.ml.out})))

(rule
  (target comments__comment_that_starts_a_labelled_argument.ml.out)
  (action
    (with-stdout-to %{target}
      (run %{project_root}/bin/main.exe style --idempotence-check --width 90 %{dep:comments__comment_that_starts_a_labelled_argument.ml}))))

(rule
  (alias runtest)
  (action (diff %{dep:comments__comment_that_starts_a_labelled_argument.ml.stylo} %{dep:comments__comment_that_starts_a_labelled_argument.ml.out})))

(rule
  (target comments_insertion.ml.out)
  (action
    (with-stdout-to %{target}
      (run %{project_root}/bin/main.exe style --idempotence-check --width 90 %{dep:comments_insertion.ml}))))

(rule
  (alias runtest)
  (action (diff %{dep:comments_insertion.ml.stylo} %{dep:comments_insertion.ml.out})))

(rule
  (target disappearing_and_requirement_update.15.ml.out)
  (action
    (with-stdout-to %{target}
      (run %{project_root}/bin/main.exe style --idempotence-check --width 15 %{dep:disappearing_and_requirement_update.15.ml}))))

(rule
  (alias runtest)
  (action (diff %{dep:disappearing_and_requirement_update.15.ml.stylo} %{dep:disappearing_and_requirement_update.15.ml.out})))

(rule
  (target doc_comments__doc_comment_after_a_constructor.mli.out)
  (action
    (with-stdout-to %{target}
      (run %{project_root}/bin/main.exe style --idempotence-check --width 90 %{dep:doc_comments__doc_comment_after_a_constructor.mli}))))

(rule
  (alias runtest)
  (action (diff %{dep:doc_comments__doc_comment_after_a_constructor.mli.stylo} %{dep:doc_comments__doc_comment_after_a_constructor.mli.out})))

(rule
  (target doc_comments__doc_comment_after_a_record_field.mli.out)
  (action
    (with-stdout-to %{target}
      (run %{project_root}/bin/main.exe style --idempotence-check --width 90 %{dep:doc_comments__doc_comment_after_a_record_field.mli}))))

(rule
  (alias runtest)
  (action (diff %{dep:doc_comments__doc_comment_after_a_record_field.mli.stylo} %{dep:doc_comments__doc_comment_after_a_record_field.mli.out})))

(rule
  (target docstring_placement.ml.out)
  (action
    (with-stdout-to %{target}
      (run %{project_root}/bin/main.exe style --idempotence-check --width 90 %{dep:docstring_placement.ml}))))

(rule
  (alias runtest)
  (action (diff %{dep:docstring_placement.ml.stylo} %{dep:docstring_placement.ml.out})))

(rule
  (target else_if_comment.ml.out)
  (action
    (with-stdout-to %{target}
      (run %{project_root}/bin/main.exe style --idempotence-check --width 90 %{dep:else_if_comment.ml}))))

(rule
  (alias runtest)
  (action (diff %{dep:else_if_comment.ml.stylo} %{dep:else_if_comment.ml.out})))

(rule
  (target expressions_patterns_and_types__empty_immutable_array_pattern.ml.out)
  (action
    (with-stdout-to %{target}
      (run %{project_root}/bin/main.exe style --idempotence-check --width 90 %{dep:expressions_patterns_and_types__empty_immutable_array_pattern.ml}))))

(rule
  (alias runtest)
  (action (diff %{dep:expressions_patterns_and_types__empty_immutable_array_pattern.ml.stylo} %{dep:expressions_patterns_and_types__empty_immutable_array_pattern.ml.out})))

(rule
  (target expressions_patterns_and_types__format_string.ml.out)
  (action
    (with-stdout-to %{target}
      (run %{project_root}/bin/main.exe style --idempotence-check --width 90 %{dep:expressions_patterns_and_types__format_string.ml}))))

(rule
  (alias runtest)
  (action (diff %{dep:expressions_patterns_and_types__format_string.ml.stylo} %{dep:expressions_patterns_and_types__format_string.ml.out})))

(rule
  (target expressions_patterns_and_types__fun_parameters_that_dont_fit.ml.out)
  (action
    (with-stdout-to %{target}
      (run %{project_root}/bin/main.exe style --idempotence-check --width 90 %{dep:expressions_patterns_and_types__fun_parameters_that_dont_fit.ml}))))

(rule
  (alias runtest)
  (action (diff %{dep:expressions_patterns_and_types__fun_parameters_that_dont_fit.ml.stylo} %{dep:expressions_patterns_and_types__fun_parameters_that_dont_fit.ml.out})))

(rule
  (target expressions_patterns_and_types__fun_parameters_with_a_type_annotation_that_dont_fit.ml.out)
  (action
    (with-stdout-to %{target}
      (run %{project_root}/bin/main.exe style --idempotence-check --width 90 %{dep:expressions_patterns_and_types__fun_parameters_with_a_type_annotation_that_dont_fit.ml}))))

(rule
  (alias runtest)
  (action (diff %{dep:expressions_patterns_and_types__fun_parameters_with_a_type_annotation_that_dont_fit.ml.stylo} %{dep:expressions_patterns_and_types__fun_parameters_with_a_type_annotation_that_dont_fit.ml.out})))

(rule
  (target expressions_patterns_and_types__js_of_ocaml_operators.ml.out)
  (action
    (with-stdout-to %{target}
      (run %{project_root}/bin/main.exe style --idempotence-check --width 90 %{dep:expressions_patterns_and_types__js_of_ocaml_operators.ml}))))

(rule
  (alias runtest)
  (action (diff %{dep:expressions_patterns_and_types__js_of_ocaml_operators.ml.stylo} %{dep:expressions_patterns_and_types__js_of_ocaml_operators.ml.out})))

(rule
  (target expressions_patterns_and_types__labelled_fun_with_parameters_and_a_return_type.ml.out)
  (action
    (with-stdout-to %{target}
      (run %{project_root}/bin/main.exe style --idempotence-check --width 90 %{dep:expressions_patterns_and_types__labelled_fun_with_parameters_and_a_return_type.ml}))))

(rule
  (alias runtest)
  (action (diff %{dep:expressions_patterns_and_types__labelled_fun_with_parameters_and_a_return_type.ml.stylo} %{dep:expressions_patterns_and_types__labelled_fun_with_parameters_and_a_return_type.ml.out})))

(rule
  (target expressions_patterns_and_types__local_fun_as_the_last_argument.ml.out)
  (action
    (with-stdout-to %{target}
      (run %{project_root}/bin/main.exe style --idempotence-check --width 90 %{dep:expressions_patterns_and_types__local_fun_as_the_last_argument.ml}))))

(rule
  (alias runtest)
  (action (diff %{dep:expressions_patterns_and_types__local_fun_as_the_last_argument.ml.stylo} %{dep:expressions_patterns_and_types__local_fun_as_the_last_argument.ml.out})))

(rule
  (target expressions_patterns_and_types__method_name_in_an_object_type.mli.out)
  (action
    (with-stdout-to %{target}
      (run %{project_root}/bin/main.exe style --idempotence-check --width 90 %{dep:expressions_patterns_and_types__method_name_in_an_object_type.mli}))))

(rule
  (alias runtest)
  (action (diff %{dep:expressions_patterns_and_types__method_name_in_an_object_type.mli.stylo} %{dep:expressions_patterns_and_types__method_name_in_an_object_type.mli.out})))

(rule
  (target expressions_patterns_and_types__one_line_top_level_let_with_a_doc_comment.ml.out)
  (action
    (with-stdout-to %{target}
      (run %{project_root}/bin/main.exe style --idempotence-check --width 90 %{dep:expressions_patterns_and_types__one_line_top_level_let_with_a_doc_comment.ml}))))

(rule
  (alias runtest)
  (action (diff %{dep:expressions_patterns_and_types__one_line_top_level_let_with_a_doc_comment.ml.stylo} %{dep:expressions_patterns_and_types__one_line_top_level_let_with_a_doc_comment.ml.out})))

(rule
  (target expressions_patterns_and_types__refutation_case.ml.out)
  (action
    (with-stdout-to %{target}
      (run %{project_root}/bin/main.exe style --idempotence-check --width 90 %{dep:expressions_patterns_and_types__refutation_case.ml}))))

(rule
  (alias runtest)
  (action (diff %{dep:expressions_patterns_and_types__refutation_case.ml.stylo} %{dep:expressions_patterns_and_types__refutation_case.ml.out})))

(rule
  (target expressions_patterns_and_types__split_or_pattern_in_parentheses.ml.out)
  (action
    (with-stdout-to %{target}
      (run %{project_root}/bin/main.exe style --idempotence-check --width 90 %{dep:expressions_patterns_and_types__split_or_pattern_in_parentheses.ml}))))

(rule
  (alias runtest)
  (action (diff %{dep:expressions_patterns_and_types__split_or_pattern_in_parentheses.ml.stylo} %{dep:expressions_patterns_and_types__split_or_pattern_in_parentheses.ml.out})))

(rule
  (target expressions_patterns_and_types__split_tuple_type_in_a_gadt_constructor.ml.out)
  (action
    (with-stdout-to %{target}
      (run %{project_root}/bin/main.exe style --idempotence-check --width 90 %{dep:expressions_patterns_and_types__split_tuple_type_in_a_gadt_constructor.ml}))))

(rule
  (alias runtest)
  (action (diff %{dep:expressions_patterns_and_types__split_tuple_type_in_a_gadt_constructor.ml.stylo} %{dep:expressions_patterns_and_types__split_tuple_type_in_a_gadt_constructor.ml.out})))

(rule
  (target expressions_patterns_and_types__split_tuple_type_with_labels.mli.out)
  (action
    (with-stdout-to %{target}
      (run %{project_root}/bin/main.exe style --idempotence-check --width 90 %{dep:expressions_patterns_and_types__split_tuple_type_with_labels.mli}))))

(rule
  (alias runtest)
  (action (diff %{dep:expressions_patterns_and_types__split_tuple_type_with_labels.mli.stylo} %{dep:expressions_patterns_and_types__split_tuple_type_with_labels.mli.out})))

(rule
  (target expressions_patterns_and_types__stack.ml.out)
  (action
    (with-stdout-to %{target}
      (run %{project_root}/bin/main.exe style --idempotence-check --width 90 %{dep:expressions_patterns_and_types__stack.ml}))))

(rule
  (alias runtest)
  (action (diff %{dep:expressions_patterns_and_types__stack.ml.stylo} %{dep:expressions_patterns_and_types__stack.ml.out})))

(rule
  (target ext_attributes.ml.out)
  (action
    (with-stdout-to %{target}
      (run %{project_root}/bin/main.exe style --idempotence-check --width 90 %{dep:ext_attributes.ml}))))

(rule
  (alias runtest)
  (action (diff %{dep:ext_attributes.ml.stylo} %{dep:ext_attributes.ml.out})))

(rule
  (target flat_non_flat_groups_and_comments.ml.out)
  (action
    (with-stdout-to %{target}
      (run %{project_root}/bin/main.exe style --idempotence-check --width 90 %{dep:flat_non_flat_groups_and_comments.ml}))))

(rule
  (alias runtest)
  (action (diff %{dep:flat_non_flat_groups_and_comments.ml.stylo} %{dep:flat_non_flat_groups_and_comments.ml.out})))

(rule
  (target flushhint_induced_reorderings.ml.out)
  (action
    (with-stdout-to %{target}
      (run %{project_root}/bin/main.exe style --idempotence-check --width 90 %{dep:flushhint_induced_reorderings.ml}))))

(rule
  (alias runtest)
  (action (diff %{dep:flushhint_induced_reorderings.ml.stylo} %{dep:flushhint_induced_reorderings.ml.out})))

(rule
  (target jst_comment_kinds.mli.out)
  (action
    (with-stdout-to %{target}
      (run %{project_root}/bin/main.exe style --idempotence-check --width 90 %{dep:jst_comment_kinds.mli}))))

(rule
  (alias runtest)
  (action (diff %{dep:jst_comment_kinds.mli.stylo} %{dep:jst_comment_kinds.mli.out})))

(rule
  (target lexer_directives.ml.out)
  (action
    (with-stdout-to %{target}
      (run %{project_root}/bin/main.exe style --idempotence-check --width 90 %{dep:lexer_directives.ml}))))

(rule
  (alias runtest)
  (action (diff %{dep:lexer_directives.ml.stylo} %{dep:lexer_directives.ml.out})))

(rule
  (target modules_and_functors__first_class_module_expression.ml.out)
  (action
    (with-stdout-to %{target}
      (run %{project_root}/bin/main.exe style --idempotence-check --width 90 %{dep:modules_and_functors__first_class_module_expression.ml}))))

(rule
  (alias runtest)
  (action (diff %{dep:modules_and_functors__first_class_module_expression.ml.stylo} %{dep:modules_and_functors__first_class_module_expression.ml.out})))

(rule
  (target modules_and_functors__first_class_module_pattern.ml.out)
  (action
    (with-stdout-to %{target}
      (run %{project_root}/bin/main.exe style --idempotence-check --width 90 %{dep:modules_and_functors__first_class_module_pattern.ml}))))

(rule
  (alias runtest)
  (action (diff %{dep:modules_and_functors__first_class_module_pattern.ml.stylo} %{dep:modules_and_functors__first_class_module_pattern.ml.out})))

(rule
  (target modules_and_functors__first_class_module_type.ml.out)
  (action
    (with-stdout-to %{target}
      (run %{project_root}/bin/main.exe style --idempotence-check --width 90 %{dep:modules_and_functors__first_class_module_type.ml}))))

(rule
  (alias runtest)
  (action (diff %{dep:modules_and_functors__first_class_module_type.ml.stylo} %{dep:modules_and_functors__first_class_module_type.ml.out})))

(rule
  (target modules_and_functors__functor_application_in_a_type_path.ml.out)
  (action
    (with-stdout-to %{target}
      (run %{project_root}/bin/main.exe style --idempotence-check --width 90 %{dep:modules_and_functors__functor_application_in_a_type_path.ml}))))

(rule
  (alias runtest)
  (action (diff %{dep:modules_and_functors__functor_application_in_a_type_path.ml.stylo} %{dep:modules_and_functors__functor_application_in_a_type_path.ml.out})))

(rule
  (target modules_and_functors__functor_application_with_one_argument.ml.out)
  (action
    (with-stdout-to %{target}
      (run %{project_root}/bin/main.exe style --idempotence-check --width 90 %{dep:modules_and_functors__functor_application_with_one_argument.ml}))))

(rule
  (alias runtest)
  (action (diff %{dep:modules_and_functors__functor_application_with_one_argument.ml.stylo} %{dep:modules_and_functors__functor_application_with_one_argument.ml.out})))

(rule
  (target modules_and_functors__functor_application_with_several_arguments_that_fits.ml.out)
  (action
    (with-stdout-to %{target}
      (run %{project_root}/bin/main.exe style --idempotence-check --width 90 %{dep:modules_and_functors__functor_application_with_several_arguments_that_fits.ml}))))

(rule
  (alias runtest)
  (action (diff %{dep:modules_and_functors__functor_application_with_several_arguments_that_fits.ml.stylo} %{dep:modules_and_functors__functor_application_with_several_arguments_that_fits.ml.out})))

(rule
  (target modules_and_functors__functor_application_with_several_arguments_the_last_a_struct.ml.out)
  (action
    (with-stdout-to %{target}
      (run %{project_root}/bin/main.exe style --idempotence-check --width 90 %{dep:modules_and_functors__functor_application_with_several_arguments_the_last_a_struct.ml}))))

(rule
  (alias runtest)
  (action (diff %{dep:modules_and_functors__functor_application_with_several_arguments_the_last_a_struct.ml.stylo} %{dep:modules_and_functors__functor_application_with_several_arguments_the_last_a_struct.ml.out})))

(rule
  (target modules_and_functors__functor_applied_to_unit.ml.out)
  (action
    (with-stdout-to %{target}
      (run %{project_root}/bin/main.exe style --idempotence-check --width 90 %{dep:modules_and_functors__functor_applied_to_unit.ml}))))

(rule
  (alias runtest)
  (action (diff %{dep:modules_and_functors__functor_applied_to_unit.ml.stylo} %{dep:modules_and_functors__functor_applied_to_unit.ml.out})))

(rule
  (target modules_and_functors__functor_definition_whose_header_doesnt_fit_with_struct.ml.out)
  (action
    (with-stdout-to %{target}
      (run %{project_root}/bin/main.exe style --idempotence-check --width 90 %{dep:modules_and_functors__functor_definition_whose_header_doesnt_fit_with_struct.ml}))))

(rule
  (alias runtest)
  (action (diff %{dep:modules_and_functors__functor_definition_whose_header_doesnt_fit_with_struct.ml.stylo} %{dep:modules_and_functors__functor_definition_whose_header_doesnt_fit_with_struct.ml.out})))

(rule
  (target modules_and_functors__functor_definition_with_a_sig_parameter.ml.out)
  (action
    (with-stdout-to %{target}
      (run %{project_root}/bin/main.exe style --idempotence-check --width 90 %{dep:modules_and_functors__functor_definition_with_a_sig_parameter.ml}))))

(rule
  (alias runtest)
  (action (diff %{dep:modules_and_functors__functor_definition_with_a_sig_parameter.ml.stylo} %{dep:modules_and_functors__functor_definition_with_a_sig_parameter.ml.out})))

(rule
  (target modules_and_functors__include_functor_applied_to_several_arguments.ml.out)
  (action
    (with-stdout-to %{target}
      (run %{project_root}/bin/main.exe style --idempotence-check --width 90 %{dep:modules_and_functors__include_functor_applied_to_several_arguments.ml}))))

(rule
  (alias runtest)
  (action (diff %{dep:modules_and_functors__include_functor_applied_to_several_arguments.ml.stylo} %{dep:modules_and_functors__include_functor_applied_to_several_arguments.ml.out})))

(rule
  (target modules_and_functors__local_open_of_a_list.ml.out)
  (action
    (with-stdout-to %{target}
      (run %{project_root}/bin/main.exe style --idempotence-check --width 90 %{dep:modules_and_functors__local_open_of_a_list.ml}))))

(rule
  (alias runtest)
  (action (diff %{dep:modules_and_functors__local_open_of_a_list.ml.stylo} %{dep:modules_and_functors__local_open_of_a_list.ml.out})))

(rule
  (target modules_and_functors__local_open_of_a_list_in_a_pattern.ml.out)
  (action
    (with-stdout-to %{target}
      (run %{project_root}/bin/main.exe style --idempotence-check --width 90 %{dep:modules_and_functors__local_open_of_a_list_in_a_pattern.ml}))))

(rule
  (alias runtest)
  (action (diff %{dep:modules_and_functors__local_open_of_a_list_in_a_pattern.ml.stylo} %{dep:modules_and_functors__local_open_of_a_list_in_a_pattern.ml.out})))

(rule
  (target modules_and_functors__module_x_f_struct_and_include_f_struct.ml.out)
  (action
    (with-stdout-to %{target}
      (run %{project_root}/bin/main.exe style --idempotence-check --width 90 %{dep:modules_and_functors__module_x_f_struct_and_include_f_struct.ml}))))

(rule
  (alias runtest)
  (action (diff %{dep:modules_and_functors__module_x_f_struct_and_include_f_struct.ml.stylo} %{dep:modules_and_functors__module_x_f_struct_and_include_f_struct.ml.out})))

(rule
  (target modules_and_functors__with_type_constraints_after_a_module_type.ml.out)
  (action
    (with-stdout-to %{target}
      (run %{project_root}/bin/main.exe style --idempotence-check --width 90 %{dep:modules_and_functors__with_type_constraints_after_a_module_type.ml}))))

(rule
  (alias runtest)
  (action (diff %{dep:modules_and_functors__with_type_constraints_after_a_module_type.ml.stylo} %{dep:modules_and_functors__with_type_constraints_after_a_module_type.ml.out})))

(rule
  (target modules_and_functors__with_type_constraints_that_dont_fit.ml.out)
  (action
    (with-stdout-to %{target}
      (run %{project_root}/bin/main.exe style --idempotence-check --width 90 %{dep:modules_and_functors__with_type_constraints_that_dont_fit.ml}))))

(rule
  (alias runtest)
  (action (diff %{dep:modules_and_functors__with_type_constraints_that_dont_fit.ml.stylo} %{dep:modules_and_functors__with_type_constraints_that_dont_fit.ml.out})))

(rule
  (target odoc_markup.mli.out)
  (action
    (with-stdout-to %{target}
      (run %{project_root}/bin/main.exe style --idempotence-check --width 90 %{dep:odoc_markup.mli}))))

(rule
  (alias runtest)
  (action (diff %{dep:odoc_markup.mli.stylo} %{dep:odoc_markup.mli.out})))

(rule
  (target odoc_markup_no_docstring.mli.out)
  (action
    (with-stdout-to %{target}
      (run %{project_root}/bin/main.exe style --idempotence-check --width 90 %{dep:odoc_markup_no_docstring.mli}))))

(rule
  (alias runtest)
  (action (diff %{dep:odoc_markup_no_docstring.mli.stylo} %{dep:odoc_markup_no_docstring.mli.out})))

(rule
  (target odoc_markup_split.mli.out)
  (action
    (with-stdout-to %{target}
      (run %{project_root}/bin/main.exe style --idempotence-check --width 90 %{dep:odoc_markup_split.mli}))))

(rule
  (alias runtest)
  (action (diff %{dep:odoc_markup_split.mli.stylo} %{dep:odoc_markup_split.mli.out})))

(rule
  (target preceeding_and_comments.ml.out)
  (action
    (with-stdout-to %{target}
      (run %{project_root}/bin/main.exe style --idempotence-check --width 90 %{dep:preceeding_and_comments.ml}))))

(rule
  (alias runtest)
  (action (diff %{dep:preceeding_and_comments.ml.stylo} %{dep:preceeding_and_comments.ml.out})))

(rule
  (target relative_nest.ml.out)
  (action
    (with-stdout-to %{target}
      (run %{project_root}/bin/main.exe style --idempotence-check --width 90 %{dep:relative_nest.ml}))))

(rule
  (alias runtest)
  (action (diff %{dep:relative_nest.ml.stylo} %{dep:relative_nest.ml.out})))

(rule
  (target single_case_optional_pipe.ml.out)
  (action
    (with-stdout-to %{target}
      (run %{project_root}/bin/main.exe style --idempotence-check --width 90 %{dep:single_case_optional_pipe.ml}))))

(rule
  (alias runtest)
  (action (diff %{dep:single_case_optional_pipe.ml.stylo} %{dep:single_case_optional_pipe.ml.out})))

(rule
  (target string_lits.ml.out)
  (action
    (with-stdout-to %{target}
      (run %{project_root}/bin/main.exe style --idempotence-check --width 90 %{dep:string_lits.ml}))))

(rule
  (alias runtest)
  (action (diff %{dep:string_lits.ml.stylo} %{dep:string_lits.ml.out})))

(rule
  (target tokens_alone_on_a_line__default_value_of_an_optional_parameter.ml.out)
  (action
    (with-stdout-to %{target}
      (run %{project_root}/bin/main.exe style --idempotence-check --width 90 %{dep:tokens_alone_on_a_line__default_value_of_an_optional_parameter.ml}))))

(rule
  (alias runtest)
  (action (diff %{dep:tokens_alone_on_a_line__default_value_of_an_optional_parameter.ml.stylo} %{dep:tokens_alone_on_a_line__default_value_of_an_optional_parameter.ml.out})))

(rule
  (target tokens_alone_on_a_line__functor_parameters_with_constraints.ml.out)
  (action
    (with-stdout-to %{target}
      (run %{project_root}/bin/main.exe style --idempotence-check --width 90 %{dep:tokens_alone_on_a_line__functor_parameters_with_constraints.ml}))))

(rule
  (alias runtest)
  (action (diff %{dep:tokens_alone_on_a_line__functor_parameters_with_constraints.ml.stylo} %{dep:tokens_alone_on_a_line__functor_parameters_with_constraints.ml.out})))

(rule
  (target tokens_alone_on_a_line__record_field_in_sexp.ml.out)
  (action
    (with-stdout-to %{target}
      (run %{project_root}/bin/main.exe style --idempotence-check --width 90 %{dep:tokens_alone_on_a_line__record_field_in_sexp.ml}))))

(rule
  (alias runtest)
  (action (diff %{dep:tokens_alone_on_a_line__record_field_in_sexp.ml.stylo} %{dep:tokens_alone_on_a_line__record_field_in_sexp.ml.out})))

(rule
  (target tokens_alone_on_a_line__record_field_whose_name_nearly_fills_the_line.ml.out)
  (action
    (with-stdout-to %{target}
      (run %{project_root}/bin/main.exe style --idempotence-check --width 90 %{dep:tokens_alone_on_a_line__record_field_whose_name_nearly_fills_the_line.ml}))))

(rule
  (alias runtest)
  (action (diff %{dep:tokens_alone_on_a_line__record_field_whose_name_nearly_fills_the_line.ml.stylo} %{dep:tokens_alone_on_a_line__record_field_whose_name_nearly_fills_the_line.ml.out})))

(rule
  (target tokens_alone_on_a_line__typed_labelled_parameter.ml.out)
  (action
    (with-stdout-to %{target}
      (run %{project_root}/bin/main.exe style --idempotence-check --width 90 %{dep:tokens_alone_on_a_line__typed_labelled_parameter.ml}))))

(rule
  (alias runtest)
  (action (diff %{dep:tokens_alone_on_a_line__typed_labelled_parameter.ml.stylo} %{dep:tokens_alone_on_a_line__typed_labelled_parameter.ml.out})))

(rule
  (target tokens_alone_on_a_line__typed_labelled_parameter_with_a_mode.ml.out)
  (action
    (with-stdout-to %{target}
      (run %{project_root}/bin/main.exe style --idempotence-check --width 90 %{dep:tokens_alone_on_a_line__typed_labelled_parameter_with_a_mode.ml}))))

(rule
  (alias runtest)
  (action (diff %{dep:tokens_alone_on_a_line__typed_labelled_parameter_with_a_mode.ml.stylo} %{dep:tokens_alone_on_a_line__typed_labelled_parameter_with_a_mode.ml.out})))

(rule
  (target toplevel_directives.mlt.out)
  (action
    (with-stdout-to %{target}
      (run %{project_root}/bin/main.exe style --idempotence-check --width 90 %{dep:toplevel_directives.mlt}))))

(rule
  (alias runtest)
  (action (diff %{dep:toplevel_directives.mlt.stylo} %{dep:toplevel_directives.mlt.out})))

(rule
  (target tuples.ml.out)
  (action
    (with-stdout-to %{target}
      (run %{project_root}/bin/main.exe style --idempotence-check --width 90 %{dep:tuples.ml}))))

(rule
  (alias runtest)
  (action (diff %{dep:tuples.ml.stylo} %{dep:tuples.ml.out})))

