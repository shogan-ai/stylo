(rule
  (alias runtest)
  (action
    (ignore-stdout
      (run ../../../bin/main.exe style --idempotence-check --ast-check %{dep:code_commenting_idempotence_issues.ml}))))

(rule
  (alias runtest)
  (action
    (ignore-stdout
      (run ../../../bin/main.exe style --idempotence-check --ast-check %{dep:docstrings.ml}))))

(rule
  (alias runtest)
  (action
    (ignore-stdout
      (run ../../../bin/main.exe style --idempotence-check --ast-check %{dep:empty_seq_erasure.ml}))))

(rule
  (alias runtest)
  (action
    (ignore-stdout
      (run ../../../bin/main.exe style --idempotence-check --ast-check %{dep:flush_hints_idempotence_issue.mli}))))

(rule
  (alias runtest)
  (action
    (ignore-stdout
      (run ../../../bin/main.exe style --idempotence-check --ast-check %{dep:minimal_comments_idempotence.ml}))))

(rule
  (alias runtest)
  (action
    (ignore-stdout
      (run ../../../bin/main.exe style --idempotence-check --ast-check %{dep:multiline_preceeding.ml}))))

(rule
  (alias runtest)
  (action
    (ignore-stdout
      (run ../../../bin/main.exe style --idempotence-check --ast-check %{dep:trailing_semi.ml}))))

