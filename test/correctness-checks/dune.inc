(rule
  (alias runtest)
  (action
    (ignore-stdout
      (run ../../bin/main.exe style --idempotence-check --ast-check %{dep:docstrings.ml}))))

(rule
  (alias runtest)
  (action
    (ignore-stdout
      (run ../../bin/main.exe style --idempotence-check --ast-check %{dep:flush_hints_idempotence_issue.mli}))))

(rule
  (alias runtest)
  (action
    (ignore-stdout
      (run ../../bin/main.exe style --idempotence-check --ast-check %{dep:minimal_comments_idempotence.ml}))))

(rule
  (alias runtest)
  (action
    (ignore-stdout
      (run ../../bin/main.exe style --idempotence-check --ast-check %{dep:trailing_semi.ml}))))

