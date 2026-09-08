(rule
  (target paren_application.ml.out)
  (action
    (with-stdout-to %{target}
      (run %{project_root}/bin/main.exe style --idempotence-check --width 90 %{dep:paren_application.ml}))))

(rule
  (alias runtest)
  (action (diff %{dep:paren_application.ml.stylo} %{dep:paren_application.ml.out})))

(rule
  (target paren_application.ml.remove.out)
  (action
    (with-stdout-to %{target}
      (run %{project_root}/bin/main.exe style --idempotence-check --remove-parentheses --width 90 %{dep:paren_application.ml}))))

(rule
  (alias runtest)
  (action (diff %{dep:paren_application.ml.stylo-remove} %{dep:paren_application.ml.remove.out})))

(rule
  (target paren_application.ml.normalize.out)
  (action
    (with-stdout-to %{target}
      (run %{project_root}/bin/main.exe style --idempotence-check --remove-parentheses --insert-parentheses --width 90 %{dep:paren_application.ml}))))

(rule
  (alias runtest)
  (action (diff %{dep:paren_application.ml.stylo-normalize} %{dep:paren_application.ml.normalize.out})))

(rule
  (target paren_arith_precedence.ml.out)
  (action
    (with-stdout-to %{target}
      (run %{project_root}/bin/main.exe style --idempotence-check --width 90 %{dep:paren_arith_precedence.ml}))))

(rule
  (alias runtest)
  (action (diff %{dep:paren_arith_precedence.ml.stylo} %{dep:paren_arith_precedence.ml.out})))

(rule
  (target paren_arith_precedence.ml.remove.out)
  (action
    (with-stdout-to %{target}
      (run %{project_root}/bin/main.exe style --idempotence-check --remove-parentheses --width 90 %{dep:paren_arith_precedence.ml}))))

(rule
  (alias runtest)
  (action (diff %{dep:paren_arith_precedence.ml.stylo-remove} %{dep:paren_arith_precedence.ml.remove.out})))

(rule
  (target paren_arith_precedence.ml.normalize.out)
  (action
    (with-stdout-to %{target}
      (run %{project_root}/bin/main.exe style --idempotence-check --remove-parentheses --insert-parentheses --width 90 %{dep:paren_arith_precedence.ml}))))

(rule
  (alias runtest)
  (action (diff %{dep:paren_arith_precedence.ml.stylo-normalize} %{dep:paren_arith_precedence.ml.normalize.out})))

(rule
  (target paren_boolean_and_comparison.ml.out)
  (action
    (with-stdout-to %{target}
      (run %{project_root}/bin/main.exe style --idempotence-check --width 90 %{dep:paren_boolean_and_comparison.ml}))))

(rule
  (alias runtest)
  (action (diff %{dep:paren_boolean_and_comparison.ml.stylo} %{dep:paren_boolean_and_comparison.ml.out})))

(rule
  (target paren_boolean_and_comparison.ml.remove.out)
  (action
    (with-stdout-to %{target}
      (run %{project_root}/bin/main.exe style --idempotence-check --remove-parentheses --width 90 %{dep:paren_boolean_and_comparison.ml}))))

(rule
  (alias runtest)
  (action (diff %{dep:paren_boolean_and_comparison.ml.stylo-remove} %{dep:paren_boolean_and_comparison.ml.remove.out})))

(rule
  (target paren_boolean_and_comparison.ml.normalize.out)
  (action
    (with-stdout-to %{target}
      (run %{project_root}/bin/main.exe style --idempotence-check --remove-parentheses --insert-parentheses --width 90 %{dep:paren_boolean_and_comparison.ml}))))

(rule
  (alias runtest)
  (action (diff %{dep:paren_boolean_and_comparison.ml.stylo-normalize} %{dep:paren_boolean_and_comparison.ml.normalize.out})))

(rule
  (target paren_constraint_and_labeled_args.ml.out)
  (action
    (with-stdout-to %{target}
      (run %{project_root}/bin/main.exe style --idempotence-check --width 90 %{dep:paren_constraint_and_labeled_args.ml}))))

(rule
  (alias runtest)
  (action (diff %{dep:paren_constraint_and_labeled_args.ml.stylo} %{dep:paren_constraint_and_labeled_args.ml.out})))

(rule
  (target paren_constraint_and_labeled_args.ml.remove.out)
  (action
    (with-stdout-to %{target}
      (run %{project_root}/bin/main.exe style --idempotence-check --remove-parentheses --width 90 %{dep:paren_constraint_and_labeled_args.ml}))))

(rule
  (alias runtest)
  (action (diff %{dep:paren_constraint_and_labeled_args.ml.stylo-remove} %{dep:paren_constraint_and_labeled_args.ml.remove.out})))

(rule
  (target paren_constraint_and_labeled_args.ml.normalize.out)
  (action
    (with-stdout-to %{target}
      (run %{project_root}/bin/main.exe style --idempotence-check --remove-parentheses --insert-parentheses --width 90 %{dep:paren_constraint_and_labeled_args.ml}))))

(rule
  (alias runtest)
  (action (diff %{dep:paren_constraint_and_labeled_args.ml.stylo-normalize} %{dep:paren_constraint_and_labeled_args.ml.normalize.out})))

(rule
  (target paren_custom_and_assoc_operators.ml.out)
  (action
    (with-stdout-to %{target}
      (run %{project_root}/bin/main.exe style --idempotence-check --width 90 %{dep:paren_custom_and_assoc_operators.ml}))))

(rule
  (alias runtest)
  (action (diff %{dep:paren_custom_and_assoc_operators.ml.stylo} %{dep:paren_custom_and_assoc_operators.ml.out})))

(rule
  (target paren_custom_and_assoc_operators.ml.remove.out)
  (action
    (with-stdout-to %{target}
      (run %{project_root}/bin/main.exe style --idempotence-check --remove-parentheses --width 90 %{dep:paren_custom_and_assoc_operators.ml}))))

(rule
  (alias runtest)
  (action (diff %{dep:paren_custom_and_assoc_operators.ml.stylo-remove} %{dep:paren_custom_and_assoc_operators.ml.remove.out})))

(rule
  (target paren_custom_and_assoc_operators.ml.normalize.out)
  (action
    (with-stdout-to %{target}
      (run %{project_root}/bin/main.exe style --idempotence-check --remove-parentheses --insert-parentheses --width 90 %{dep:paren_custom_and_assoc_operators.ml}))))

(rule
  (alias runtest)
  (action (diff %{dep:paren_custom_and_assoc_operators.ml.stylo-normalize} %{dep:paren_custom_and_assoc_operators.ml.normalize.out})))

(rule
  (target paren_dangling_function_after_infix.ml.out)
  (action
    (with-stdout-to %{target}
      (run %{project_root}/bin/main.exe style --idempotence-check --width 90 %{dep:paren_dangling_function_after_infix.ml}))))

(rule
  (alias runtest)
  (action (diff %{dep:paren_dangling_function_after_infix.ml.stylo} %{dep:paren_dangling_function_after_infix.ml.out})))

(rule
  (target paren_dangling_function_after_infix.ml.remove.out)
  (action
    (with-stdout-to %{target}
      (run %{project_root}/bin/main.exe style --idempotence-check --remove-parentheses --width 90 %{dep:paren_dangling_function_after_infix.ml}))))

(rule
  (alias runtest)
  (action (diff %{dep:paren_dangling_function_after_infix.ml.stylo-remove} %{dep:paren_dangling_function_after_infix.ml.remove.out})))

(rule
  (target paren_dangling_function_after_infix.ml.normalize.out)
  (action
    (with-stdout-to %{target}
      (run %{project_root}/bin/main.exe style --idempotence-check --remove-parentheses --insert-parentheses --width 90 %{dep:paren_dangling_function_after_infix.ml}))))

(rule
  (alias runtest)
  (action (diff %{dep:paren_dangling_function_after_infix.ml.stylo-normalize} %{dep:paren_dangling_function_after_infix.ml.normalize.out})))

(rule
  (target paren_dangling_match_cases.ml.out)
  (action
    (with-stdout-to %{target}
      (run %{project_root}/bin/main.exe style --idempotence-check --width 90 %{dep:paren_dangling_match_cases.ml}))))

(rule
  (alias runtest)
  (action (diff %{dep:paren_dangling_match_cases.ml.stylo} %{dep:paren_dangling_match_cases.ml.out})))

(rule
  (target paren_dangling_match_cases.ml.remove.out)
  (action
    (with-stdout-to %{target}
      (run %{project_root}/bin/main.exe style --idempotence-check --remove-parentheses --width 90 %{dep:paren_dangling_match_cases.ml}))))

(rule
  (alias runtest)
  (action (diff %{dep:paren_dangling_match_cases.ml.stylo-remove} %{dep:paren_dangling_match_cases.ml.remove.out})))

(rule
  (target paren_dangling_match_cases.ml.normalize.out)
  (action
    (with-stdout-to %{target}
      (run %{project_root}/bin/main.exe style --idempotence-check --remove-parentheses --insert-parentheses --width 90 %{dep:paren_dangling_match_cases.ml}))))

(rule
  (alias runtest)
  (action (diff %{dep:paren_dangling_match_cases.ml.stylo-normalize} %{dep:paren_dangling_match_cases.ml.normalize.out})))

(rule
  (target paren_expr_tuple_nesting.ml.out)
  (action
    (with-stdout-to %{target}
      (run %{project_root}/bin/main.exe style --idempotence-check --width 90 %{dep:paren_expr_tuple_nesting.ml}))))

(rule
  (alias runtest)
  (action (diff %{dep:paren_expr_tuple_nesting.ml.stylo} %{dep:paren_expr_tuple_nesting.ml.out})))

(rule
  (target paren_expr_tuple_nesting.ml.remove.out)
  (action
    (with-stdout-to %{target}
      (run %{project_root}/bin/main.exe style --idempotence-check --remove-parentheses --width 90 %{dep:paren_expr_tuple_nesting.ml}))))

(rule
  (alias runtest)
  (action (diff %{dep:paren_expr_tuple_nesting.ml.stylo-remove} %{dep:paren_expr_tuple_nesting.ml.remove.out})))

(rule
  (target paren_expr_tuple_nesting.ml.normalize.out)
  (action
    (with-stdout-to %{target}
      (run %{project_root}/bin/main.exe style --idempotence-check --remove-parentheses --insert-parentheses --width 90 %{dep:paren_expr_tuple_nesting.ml}))))

(rule
  (alias runtest)
  (action (diff %{dep:paren_expr_tuple_nesting.ml.stylo-normalize} %{dep:paren_expr_tuple_nesting.ml.normalize.out})))

(rule
  (target paren_field_and_assignment.ml.out)
  (action
    (with-stdout-to %{target}
      (run %{project_root}/bin/main.exe style --idempotence-check --width 90 %{dep:paren_field_and_assignment.ml}))))

(rule
  (alias runtest)
  (action (diff %{dep:paren_field_and_assignment.ml.stylo} %{dep:paren_field_and_assignment.ml.out})))

(rule
  (target paren_field_and_assignment.ml.remove.out)
  (action
    (with-stdout-to %{target}
      (run %{project_root}/bin/main.exe style --idempotence-check --remove-parentheses --width 90 %{dep:paren_field_and_assignment.ml}))))

(rule
  (alias runtest)
  (action (diff %{dep:paren_field_and_assignment.ml.stylo-remove} %{dep:paren_field_and_assignment.ml.remove.out})))

(rule
  (target paren_field_and_assignment.ml.normalize.out)
  (action
    (with-stdout-to %{target}
      (run %{project_root}/bin/main.exe style --idempotence-check --remove-parentheses --insert-parentheses --width 90 %{dep:paren_field_and_assignment.ml}))))

(rule
  (alias runtest)
  (action (diff %{dep:paren_field_and_assignment.ml.stylo-normalize} %{dep:paren_field_and_assignment.ml.normalize.out})))

(rule
  (target paren_if_match_fun_as_subexpr.ml.out)
  (action
    (with-stdout-to %{target}
      (run %{project_root}/bin/main.exe style --idempotence-check --width 90 %{dep:paren_if_match_fun_as_subexpr.ml}))))

(rule
  (alias runtest)
  (action (diff %{dep:paren_if_match_fun_as_subexpr.ml.stylo} %{dep:paren_if_match_fun_as_subexpr.ml.out})))

(rule
  (target paren_if_match_fun_as_subexpr.ml.remove.out)
  (action
    (with-stdout-to %{target}
      (run %{project_root}/bin/main.exe style --idempotence-check --remove-parentheses --width 90 %{dep:paren_if_match_fun_as_subexpr.ml}))))

(rule
  (alias runtest)
  (action (diff %{dep:paren_if_match_fun_as_subexpr.ml.stylo-remove} %{dep:paren_if_match_fun_as_subexpr.ml.remove.out})))

(rule
  (target paren_if_match_fun_as_subexpr.ml.normalize.out)
  (action
    (with-stdout-to %{target}
      (run %{project_root}/bin/main.exe style --idempotence-check --remove-parentheses --insert-parentheses --width 90 %{dep:paren_if_match_fun_as_subexpr.ml}))))

(rule
  (alias runtest)
  (action (diff %{dep:paren_if_match_fun_as_subexpr.ml.stylo-normalize} %{dep:paren_if_match_fun_as_subexpr.ml.normalize.out})))

(rule
  (target paren_pattern_constructor_nesting.ml.out)
  (action
    (with-stdout-to %{target}
      (run %{project_root}/bin/main.exe style --idempotence-check --width 90 %{dep:paren_pattern_constructor_nesting.ml}))))

(rule
  (alias runtest)
  (action (diff %{dep:paren_pattern_constructor_nesting.ml.stylo} %{dep:paren_pattern_constructor_nesting.ml.out})))

(rule
  (target paren_pattern_constructor_nesting.ml.remove.out)
  (action
    (with-stdout-to %{target}
      (run %{project_root}/bin/main.exe style --idempotence-check --remove-parentheses --width 90 %{dep:paren_pattern_constructor_nesting.ml}))))

(rule
  (alias runtest)
  (action (diff %{dep:paren_pattern_constructor_nesting.ml.stylo-remove} %{dep:paren_pattern_constructor_nesting.ml.remove.out})))

(rule
  (target paren_pattern_constructor_nesting.ml.normalize.out)
  (action
    (with-stdout-to %{target}
      (run %{project_root}/bin/main.exe style --idempotence-check --remove-parentheses --insert-parentheses --width 90 %{dep:paren_pattern_constructor_nesting.ml}))))

(rule
  (alias runtest)
  (action (diff %{dep:paren_pattern_constructor_nesting.ml.stylo-normalize} %{dep:paren_pattern_constructor_nesting.ml.normalize.out})))

(rule
  (target paren_pattern_misc.ml.out)
  (action
    (with-stdout-to %{target}
      (run %{project_root}/bin/main.exe style --idempotence-check --width 90 %{dep:paren_pattern_misc.ml}))))

(rule
  (alias runtest)
  (action (diff %{dep:paren_pattern_misc.ml.stylo} %{dep:paren_pattern_misc.ml.out})))

(rule
  (target paren_pattern_misc.ml.remove.out)
  (action
    (with-stdout-to %{target}
      (run %{project_root}/bin/main.exe style --idempotence-check --remove-parentheses --width 90 %{dep:paren_pattern_misc.ml}))))

(rule
  (alias runtest)
  (action (diff %{dep:paren_pattern_misc.ml.stylo-remove} %{dep:paren_pattern_misc.ml.remove.out})))

(rule
  (target paren_pattern_misc.ml.normalize.out)
  (action
    (with-stdout-to %{target}
      (run %{project_root}/bin/main.exe style --idempotence-check --remove-parentheses --insert-parentheses --width 90 %{dep:paren_pattern_misc.ml}))))

(rule
  (alias runtest)
  (action (diff %{dep:paren_pattern_misc.ml.stylo-normalize} %{dep:paren_pattern_misc.ml.normalize.out})))

(rule
  (target paren_pattern_or_as.ml.out)
  (action
    (with-stdout-to %{target}
      (run %{project_root}/bin/main.exe style --idempotence-check --width 90 %{dep:paren_pattern_or_as.ml}))))

(rule
  (alias runtest)
  (action (diff %{dep:paren_pattern_or_as.ml.stylo} %{dep:paren_pattern_or_as.ml.out})))

(rule
  (target paren_pattern_or_as.ml.remove.out)
  (action
    (with-stdout-to %{target}
      (run %{project_root}/bin/main.exe style --idempotence-check --remove-parentheses --width 90 %{dep:paren_pattern_or_as.ml}))))

(rule
  (alias runtest)
  (action (diff %{dep:paren_pattern_or_as.ml.stylo-remove} %{dep:paren_pattern_or_as.ml.remove.out})))

(rule
  (target paren_pattern_or_as.ml.normalize.out)
  (action
    (with-stdout-to %{target}
      (run %{project_root}/bin/main.exe style --idempotence-check --remove-parentheses --insert-parentheses --width 90 %{dep:paren_pattern_or_as.ml}))))

(rule
  (alias runtest)
  (action (diff %{dep:paren_pattern_or_as.ml.stylo-normalize} %{dep:paren_pattern_or_as.ml.normalize.out})))

(rule
  (target paren_pattern_tuple_cons.ml.out)
  (action
    (with-stdout-to %{target}
      (run %{project_root}/bin/main.exe style --idempotence-check --width 90 %{dep:paren_pattern_tuple_cons.ml}))))

(rule
  (alias runtest)
  (action (diff %{dep:paren_pattern_tuple_cons.ml.stylo} %{dep:paren_pattern_tuple_cons.ml.out})))

(rule
  (target paren_pattern_tuple_cons.ml.remove.out)
  (action
    (with-stdout-to %{target}
      (run %{project_root}/bin/main.exe style --idempotence-check --remove-parentheses --width 90 %{dep:paren_pattern_tuple_cons.ml}))))

(rule
  (alias runtest)
  (action (diff %{dep:paren_pattern_tuple_cons.ml.stylo-remove} %{dep:paren_pattern_tuple_cons.ml.remove.out})))

(rule
  (target paren_pattern_tuple_cons.ml.normalize.out)
  (action
    (with-stdout-to %{target}
      (run %{project_root}/bin/main.exe style --idempotence-check --remove-parentheses --insert-parentheses --width 90 %{dep:paren_pattern_tuple_cons.ml}))))

(rule
  (alias runtest)
  (action (diff %{dep:paren_pattern_tuple_cons.ml.stylo-normalize} %{dep:paren_pattern_tuple_cons.ml.normalize.out})))

(rule
  (target paren_rightmost_dangling.ml.out)
  (action
    (with-stdout-to %{target}
      (run %{project_root}/bin/main.exe style --idempotence-check --width 90 %{dep:paren_rightmost_dangling.ml}))))

(rule
  (alias runtest)
  (action (diff %{dep:paren_rightmost_dangling.ml.stylo} %{dep:paren_rightmost_dangling.ml.out})))

(rule
  (target paren_rightmost_dangling.ml.remove.out)
  (action
    (with-stdout-to %{target}
      (run %{project_root}/bin/main.exe style --idempotence-check --remove-parentheses --width 90 %{dep:paren_rightmost_dangling.ml}))))

(rule
  (alias runtest)
  (action (diff %{dep:paren_rightmost_dangling.ml.stylo-remove} %{dep:paren_rightmost_dangling.ml.remove.out})))

(rule
  (target paren_rightmost_dangling.ml.normalize.out)
  (action
    (with-stdout-to %{target}
      (run %{project_root}/bin/main.exe style --idempotence-check --remove-parentheses --insert-parentheses --width 90 %{dep:paren_rightmost_dangling.ml}))))

(rule
  (alias runtest)
  (action (diff %{dep:paren_rightmost_dangling.ml.stylo-normalize} %{dep:paren_rightmost_dangling.ml.normalize.out})))

(rule
  (target paren_sequence.ml.out)
  (action
    (with-stdout-to %{target}
      (run %{project_root}/bin/main.exe style --idempotence-check --width 90 %{dep:paren_sequence.ml}))))

(rule
  (alias runtest)
  (action (diff %{dep:paren_sequence.ml.stylo} %{dep:paren_sequence.ml.out})))

(rule
  (target paren_sequence.ml.remove.out)
  (action
    (with-stdout-to %{target}
      (run %{project_root}/bin/main.exe style --idempotence-check --remove-parentheses --width 90 %{dep:paren_sequence.ml}))))

(rule
  (alias runtest)
  (action (diff %{dep:paren_sequence.ml.stylo-remove} %{dep:paren_sequence.ml.remove.out})))

(rule
  (target paren_sequence.ml.normalize.out)
  (action
    (with-stdout-to %{target}
      (run %{project_root}/bin/main.exe style --idempotence-check --remove-parentheses --insert-parentheses --width 90 %{dep:paren_sequence.ml}))))

(rule
  (alias runtest)
  (action (diff %{dep:paren_sequence.ml.stylo-normalize} %{dep:paren_sequence.ml.normalize.out})))

(rule
  (target paren_type_alias_and_misc.ml.out)
  (action
    (with-stdout-to %{target}
      (run %{project_root}/bin/main.exe style --idempotence-check --width 90 %{dep:paren_type_alias_and_misc.ml}))))

(rule
  (alias runtest)
  (action (diff %{dep:paren_type_alias_and_misc.ml.stylo} %{dep:paren_type_alias_and_misc.ml.out})))

(rule
  (target paren_type_alias_and_misc.ml.remove.out)
  (action
    (with-stdout-to %{target}
      (run %{project_root}/bin/main.exe style --idempotence-check --remove-parentheses --width 90 %{dep:paren_type_alias_and_misc.ml}))))

(rule
  (alias runtest)
  (action (diff %{dep:paren_type_alias_and_misc.ml.stylo-remove} %{dep:paren_type_alias_and_misc.ml.remove.out})))

(rule
  (target paren_type_alias_and_misc.ml.normalize.out)
  (action
    (with-stdout-to %{target}
      (run %{project_root}/bin/main.exe style --idempotence-check --remove-parentheses --insert-parentheses --width 90 %{dep:paren_type_alias_and_misc.ml}))))

(rule
  (alias runtest)
  (action (diff %{dep:paren_type_alias_and_misc.ml.stylo-normalize} %{dep:paren_type_alias_and_misc.ml.normalize.out})))

(rule
  (target paren_type_application.ml.out)
  (action
    (with-stdout-to %{target}
      (run %{project_root}/bin/main.exe style --idempotence-check --width 90 %{dep:paren_type_application.ml}))))

(rule
  (alias runtest)
  (action (diff %{dep:paren_type_application.ml.stylo} %{dep:paren_type_application.ml.out})))

(rule
  (target paren_type_application.ml.remove.out)
  (action
    (with-stdout-to %{target}
      (run %{project_root}/bin/main.exe style --idempotence-check --remove-parentheses --width 90 %{dep:paren_type_application.ml}))))

(rule
  (alias runtest)
  (action (diff %{dep:paren_type_application.ml.stylo-remove} %{dep:paren_type_application.ml.remove.out})))

(rule
  (target paren_type_application.ml.normalize.out)
  (action
    (with-stdout-to %{target}
      (run %{project_root}/bin/main.exe style --idempotence-check --remove-parentheses --insert-parentheses --width 90 %{dep:paren_type_application.ml}))))

(rule
  (alias runtest)
  (action (diff %{dep:paren_type_application.ml.stylo-normalize} %{dep:paren_type_application.ml.normalize.out})))

(rule
  (target paren_type_arrow_tuple.ml.out)
  (action
    (with-stdout-to %{target}
      (run %{project_root}/bin/main.exe style --idempotence-check --width 90 %{dep:paren_type_arrow_tuple.ml}))))

(rule
  (alias runtest)
  (action (diff %{dep:paren_type_arrow_tuple.ml.stylo} %{dep:paren_type_arrow_tuple.ml.out})))

(rule
  (target paren_type_arrow_tuple.ml.remove.out)
  (action
    (with-stdout-to %{target}
      (run %{project_root}/bin/main.exe style --idempotence-check --remove-parentheses --width 90 %{dep:paren_type_arrow_tuple.ml}))))

(rule
  (alias runtest)
  (action (diff %{dep:paren_type_arrow_tuple.ml.stylo-remove} %{dep:paren_type_arrow_tuple.ml.remove.out})))

(rule
  (target paren_type_arrow_tuple.ml.normalize.out)
  (action
    (with-stdout-to %{target}
      (run %{project_root}/bin/main.exe style --idempotence-check --remove-parentheses --insert-parentheses --width 90 %{dep:paren_type_arrow_tuple.ml}))))

(rule
  (alias runtest)
  (action (diff %{dep:paren_type_arrow_tuple.ml.stylo-normalize} %{dep:paren_type_arrow_tuple.ml.normalize.out})))

(rule
  (target paren_unary_minus.ml.out)
  (action
    (with-stdout-to %{target}
      (run %{project_root}/bin/main.exe style --idempotence-check --width 90 %{dep:paren_unary_minus.ml}))))

(rule
  (alias runtest)
  (action (diff %{dep:paren_unary_minus.ml.stylo} %{dep:paren_unary_minus.ml.out})))

(rule
  (target paren_unary_minus.ml.remove.out)
  (action
    (with-stdout-to %{target}
      (run %{project_root}/bin/main.exe style --idempotence-check --remove-parentheses --width 90 %{dep:paren_unary_minus.ml}))))

(rule
  (alias runtest)
  (action (diff %{dep:paren_unary_minus.ml.stylo-remove} %{dep:paren_unary_minus.ml.remove.out})))

(rule
  (target paren_unary_minus.ml.normalize.out)
  (action
    (with-stdout-to %{target}
      (run %{project_root}/bin/main.exe style --idempotence-check --remove-parentheses --insert-parentheses --width 90 %{dep:paren_unary_minus.ml}))))

(rule
  (alias runtest)
  (action (diff %{dep:paren_unary_minus.ml.stylo-normalize} %{dep:paren_unary_minus.ml.normalize.out})))

