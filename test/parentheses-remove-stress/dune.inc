(rule
 (alias runtest)
 (action
  (ignore-stdout
   (run ../../bin/main.exe style --idempotence-check --remove-parentheses --ast-check %{dep:alias_pat.ml}))))

(rule
 (alias runtest)
 (action
  (ignore-stdout
   (run ../../bin/main.exe style --idempotence-check --remove-parentheses --ast-check %{dep:alias_underlying.ml}))))

(rule
 (alias runtest)
 (action
  (ignore-stdout
   (run ../../bin/main.exe style --idempotence-check --remove-parentheses --ast-check %{dep:app_arg_first.ml}))))

(rule
 (alias runtest)
 (action
  (ignore-stdout
   (run ../../bin/main.exe style --idempotence-check --remove-parentheses --ast-check %{dep:app_arg_second.ml}))))

(rule
 (alias runtest)
 (action
  (ignore-stdout
   (run ../../bin/main.exe style --idempotence-check --remove-parentheses --ast-check %{dep:app_fun_pos.ml}))))

(rule
 (alias runtest)
 (action
  (ignore-stdout
   (run ../../bin/main.exe style --idempotence-check --remove-parentheses --ast-check %{dep:array_elt.ml}))))

(rule
 (alias runtest)
 (action
  (ignore-stdout
   (run ../../bin/main.exe style --idempotence-check --remove-parentheses --ast-check %{dep:arrow_codomain.ml}))))

(rule
 (alias runtest)
 (action
  (ignore-stdout
   (run ../../bin/main.exe style --idempotence-check --remove-parentheses --ast-check %{dep:arrow_domain.ml}))))

(rule
 (alias runtest)
 (action
  (ignore-stdout
   (run ../../bin/main.exe style --idempotence-check --remove-parentheses --ast-check %{dep:assert_arg.ml}))))

(rule
 (alias runtest)
 (action
  (ignore-stdout
   (run ../../bin/main.exe style --idempotence-check --remove-parentheses --ast-check %{dep:assign_rhs.ml}))))

(rule
 (alias runtest)
 (action
  (ignore-stdout
   (run ../../bin/main.exe style --idempotence-check --remove-parentheses --ast-check %{dep:coerce_lhs.ml}))))

(rule
 (alias runtest)
 (action
  (ignore-stdout
   (run ../../bin/main.exe style --idempotence-check --remove-parentheses --ast-check %{dep:cons_head.ml}))))

(rule
 (alias runtest)
 (action
  (ignore-stdout
   (run ../../bin/main.exe style --idempotence-check --remove-parentheses --ast-check %{dep:cons_left.ml}))))

(rule
 (alias runtest)
 (action
  (ignore-stdout
   (run ../../bin/main.exe style --idempotence-check --remove-parentheses --ast-check %{dep:cons_right.ml}))))

(rule
 (alias runtest)
 (action
  (ignore-stdout
   (run ../../bin/main.exe style --idempotence-check --remove-parentheses --ast-check %{dep:cons_tail.ml}))))

(rule
 (alias runtest)
 (action
  (ignore-stdout
   (run ../../bin/main.exe style --idempotence-check --remove-parentheses --ast-check %{dep:constr_arg.ml}))))

(rule
 (alias runtest)
 (action
  (ignore-stdout
   (run ../../bin/main.exe style --idempotence-check --remove-parentheses --ast-check %{dep:constraint_lhs.ml}))))

(rule
 (alias runtest)
 (action
  (ignore-stdout
   (run ../../bin/main.exe style --idempotence-check --remove-parentheses --ast-check %{dep:construct_arg.ml}))))

(rule
 (alias runtest)
 (action
  (ignore-stdout
   (run ../../bin/main.exe style --idempotence-check --remove-parentheses --ast-check %{dep:constructor_arg.ml}))))

(rule
 (alias runtest)
 (action
  (ignore-stdout
   (run ../../bin/main.exe style --idempotence-check --remove-parentheses --ast-check %{dep:constructor_two_levels.ml}))))

(rule
 (alias runtest)
 (action
  (ignore-stdout
   (run ../../bin/main.exe style --idempotence-check --remove-parentheses --ast-check %{dep:field_base.ml}))))

(rule
 (alias runtest)
 (action
  (ignore-stdout
   (run ../../bin/main.exe style --idempotence-check --remove-parentheses --ast-check %{dep:for_body.ml}))))

(rule
 (alias runtest)
 (action
  (ignore-stdout
   (run ../../bin/main.exe style --idempotence-check --remove-parentheses --ast-check %{dep:for_from.ml}))))

(rule
 (alias runtest)
 (action
  (ignore-stdout
   (run ../../bin/main.exe style --idempotence-check --remove-parentheses --ast-check %{dep:for_to.ml}))))

(rule
 (alias runtest)
 (action
  (ignore-stdout
   (run ../../bin/main.exe style --idempotence-check --remove-parentheses --ast-check %{dep:function_last_case.ml}))))

(rule
 (alias runtest)
 (action
  (ignore-stdout
   (run ../../bin/main.exe style --idempotence-check --remove-parentheses --ast-check %{dep:function_nonlast_case.ml}))))

(rule
 (alias runtest)
 (action
  (ignore-stdout
   (run ../../bin/main.exe style --idempotence-check --remove-parentheses --ast-check %{dep:if_cond.ml}))))

(rule
 (alias runtest)
 (action
  (ignore-stdout
   (run ../../bin/main.exe style --idempotence-check --remove-parentheses --ast-check %{dep:if_else.ml}))))

(rule
 (alias runtest)
 (action
  (ignore-stdout
   (run ../../bin/main.exe style --idempotence-check --remove-parentheses --ast-check %{dep:if_then_noelse.ml}))))

(rule
 (alias runtest)
 (action
  (ignore-stdout
   (run ../../bin/main.exe style --idempotence-check --remove-parentheses --ast-check %{dep:if_then_withelse.ml}))))

(rule
 (alias runtest)
 (action
  (ignore-stdout
   (run ../../bin/main.exe style --idempotence-check --remove-parentheses --ast-check %{dep:index_assign_rhs.ml}))))

(rule
 (alias runtest)
 (action
  (ignore-stdout
   (run ../../bin/main.exe style --idempotence-check --remove-parentheses --ast-check %{dep:infix_left_and.ml}))))

(rule
 (alias runtest)
 (action
  (ignore-stdout
   (run ../../bin/main.exe style --idempotence-check --remove-parentheses --ast-check %{dep:infix_left_cmp.ml}))))

(rule
 (alias runtest)
 (action
  (ignore-stdout
   (run ../../bin/main.exe style --idempotence-check --remove-parentheses --ast-check %{dep:infix_left_or.ml}))))

(rule
 (alias runtest)
 (action
  (ignore-stdout
   (run ../../bin/main.exe style --idempotence-check --remove-parentheses --ast-check %{dep:infix_left_plus.ml}))))

(rule
 (alias runtest)
 (action
  (ignore-stdout
   (run ../../bin/main.exe style --idempotence-check --remove-parentheses --ast-check %{dep:infix_left_pow.ml}))))

(rule
 (alias runtest)
 (action
  (ignore-stdout
   (run ../../bin/main.exe style --idempotence-check --remove-parentheses --ast-check %{dep:infix_left_star.ml}))))

(rule
 (alias runtest)
 (action
  (ignore-stdout
   (run ../../bin/main.exe style --idempotence-check --remove-parentheses --ast-check %{dep:infix_right_and.ml}))))

(rule
 (alias runtest)
 (action
  (ignore-stdout
   (run ../../bin/main.exe style --idempotence-check --remove-parentheses --ast-check %{dep:infix_right_at.ml}))))

(rule
 (alias runtest)
 (action
  (ignore-stdout
   (run ../../bin/main.exe style --idempotence-check --remove-parentheses --ast-check %{dep:infix_right_caret.ml}))))

(rule
 (alias runtest)
 (action
  (ignore-stdout
   (run ../../bin/main.exe style --idempotence-check --remove-parentheses --ast-check %{dep:infix_right_cmp.ml}))))

(rule
 (alias runtest)
 (action
  (ignore-stdout
   (run ../../bin/main.exe style --idempotence-check --remove-parentheses --ast-check %{dep:infix_right_custom.ml}))))

(rule
 (alias runtest)
 (action
  (ignore-stdout
   (run ../../bin/main.exe style --idempotence-check --remove-parentheses --ast-check %{dep:infix_right_custom_cmp.ml}))))

(rule
 (alias runtest)
 (action
  (ignore-stdout
   (run ../../bin/main.exe style --idempotence-check --remove-parentheses --ast-check %{dep:infix_right_or.ml}))))

(rule
 (alias runtest)
 (action
  (ignore-stdout
   (run ../../bin/main.exe style --idempotence-check --remove-parentheses --ast-check %{dep:infix_right_plus.ml}))))

(rule
 (alias runtest)
 (action
  (ignore-stdout
   (run ../../bin/main.exe style --idempotence-check --remove-parentheses --ast-check %{dep:infix_right_pow.ml}))))

(rule
 (alias runtest)
 (action
  (ignore-stdout
   (run ../../bin/main.exe style --idempotence-check --remove-parentheses --ast-check %{dep:infix_right_star.ml}))))

(rule
 (alias runtest)
 (action
  (ignore-stdout
   (run ../../bin/main.exe style --idempotence-check --remove-parentheses --ast-check %{dep:labeled_arg.ml}))))

(rule
 (alias runtest)
 (action
  (ignore-stdout
   (run ../../bin/main.exe style --idempotence-check --remove-parentheses --ast-check %{dep:lazy_arg.ml}))))

(rule
 (alias runtest)
 (action
  (ignore-stdout
   (run ../../bin/main.exe style --idempotence-check --remove-parentheses --ast-check %{dep:let_binding_rhs.ml}))))

(rule
 (alias runtest)
 (action
  (ignore-stdout
   (run ../../bin/main.exe style --idempotence-check --remove-parentheses --ast-check %{dep:let_body.ml}))))

(rule
 (alias runtest)
 (action
  (ignore-stdout
   (run ../../bin/main.exe style --idempotence-check --remove-parentheses --ast-check %{dep:list_elt.ml}))))

(rule
 (alias runtest)
 (action
  (ignore-stdout
   (run ../../bin/main.exe style --idempotence-check --remove-parentheses --ast-check %{dep:match_last_case.ml}))))

(rule
 (alias runtest)
 (action
  (ignore-stdout
   (run ../../bin/main.exe style --idempotence-check --remove-parentheses --ast-check %{dep:match_nonlast_case.ml}))))

(rule
 (alias runtest)
 (action
  (ignore-stdout
   (run ../../bin/main.exe style --idempotence-check --remove-parentheses --ast-check %{dep:match_nonlast_case_of_3.ml}))))

(rule
 (alias runtest)
 (action
  (ignore-stdout
   (run ../../bin/main.exe style --idempotence-check --remove-parentheses --ast-check %{dep:match_scrutinee.ml}))))

(rule
 (alias runtest)
 (action
  (ignore-stdout
   (run ../../bin/main.exe style --idempotence-check --remove-parentheses --ast-check %{dep:nested_deep_infix_seqfirst.ml}))))

(rule
 (alias runtest)
 (action
  (ignore-stdout
   (run ../../bin/main.exe style --idempotence-check --remove-parentheses --ast-check %{dep:nested_if_in_match_nonlast.ml}))))

(rule
 (alias runtest)
 (action
  (ignore-stdout
   (run ../../bin/main.exe style --idempotence-check --remove-parentheses --ast-check %{dep:nested_infix_then_seq.ml}))))

(rule
 (alias runtest)
 (action
  (ignore-stdout
   (run ../../bin/main.exe style --idempotence-check --remove-parentheses --ast-check %{dep:nested_match_in_if_then.ml}))))

(rule
 (alias runtest)
 (action
  (ignore-stdout
   (run ../../bin/main.exe style --idempotence-check --remove-parentheses --ast-check %{dep:nested_match_in_match_last.ml}))))

(rule
 (alias runtest)
 (action
  (ignore-stdout
   (run ../../bin/main.exe style --idempotence-check --remove-parentheses --ast-check %{dep:nested_match_in_match_nonlast.ml}))))

(rule
 (alias runtest)
 (action
  (ignore-stdout
   (run ../../bin/main.exe style --idempotence-check --remove-parentheses --ast-check %{dep:or_left.ml}))))

(rule
 (alias runtest)
 (action
  (ignore-stdout
   (run ../../bin/main.exe style --idempotence-check --remove-parentheses --ast-check %{dep:or_right.ml}))))

(rule
 (alias runtest)
 (action
  (ignore-stdout
   (run ../../bin/main.exe style --idempotence-check --remove-parentheses --ast-check %{dep:record_field_value.ml}))))

(rule
 (alias runtest)
 (action
  (ignore-stdout
   (run ../../bin/main.exe style --idempotence-check --remove-parentheses --ast-check %{dep:send_receiver.ml}))))

(rule
 (alias runtest)
 (action
  (ignore-stdout
   (run ../../bin/main.exe style --idempotence-check --remove-parentheses --ast-check %{dep:seq_first.ml}))))

(rule
 (alias runtest)
 (action
  (ignore-stdout
   (run ../../bin/main.exe style --idempotence-check --remove-parentheses --ast-check %{dep:seq_last.ml}))))

(rule
 (alias runtest)
 (action
  (ignore-stdout
   (run ../../bin/main.exe style --idempotence-check --remove-parentheses --ast-check %{dep:setfield_rhs.ml}))))

(rule
 (alias runtest)
 (action
  (ignore-stdout
   (run ../../bin/main.exe style --idempotence-check --remove-parentheses --ast-check %{dep:try_last_case.ml}))))

(rule
 (alias runtest)
 (action
  (ignore-stdout
   (run ../../bin/main.exe style --idempotence-check --remove-parentheses --ast-check %{dep:try_nonlast_case.ml}))))

(rule
 (alias runtest)
 (action
  (ignore-stdout
   (run ../../bin/main.exe style --idempotence-check --remove-parentheses --ast-check %{dep:tuple_first.ml}))))

(rule
 (alias runtest)
 (action
  (ignore-stdout
   (run ../../bin/main.exe style --idempotence-check --remove-parentheses --ast-check %{dep:tuple_last.ml}))))

(rule
 (alias runtest)
 (action
  (ignore-stdout
   (run ../../bin/main.exe style --idempotence-check --remove-parentheses --ast-check %{dep:tuple_mid.ml}))))

(rule
 (alias runtest)
 (action
  (ignore-stdout
   (run ../../bin/main.exe style --idempotence-check --remove-parentheses --ast-check %{dep:unary_minus.ml}))))

(rule
 (alias runtest)
 (action
  (ignore-stdout
   (run ../../bin/main.exe style --idempotence-check --remove-parentheses --ast-check %{dep:while_body.ml}))))

(rule
 (alias runtest)
 (action
  (ignore-stdout
   (run ../../bin/main.exe style --idempotence-check --remove-parentheses --ast-check %{dep:while_cond.ml}))))

