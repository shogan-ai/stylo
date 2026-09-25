(rule
  (alias runtest)
  (action
    (ignore-stdout
      (run ../../../bin/main.exe style --idempotence-check --ast-check
          --width 90 %{dep:blank_before_fst_comment.ml}))))

(rule
  (alias runtest)
  (action
    (ignore-stdout
      (run ../../../bin/main.exe style --idempotence-check --ast-check
          --width 90 %{dep:blank_lines.ml}))))

(rule
  (alias runtest)
  (action
    (ignore-stdout
      (run ../../../bin/main.exe style --idempotence-check --ast-check
          --width 90 %{dep:break0_idempotence_issue.ml}))))

(rule
  (alias runtest)
  (action
    (ignore-stdout
      (run ../../../bin/main.exe style --idempotence-check --ast-check
          --width 90 %{dep:code_commenting_idempotence_issues.ml}))))

(rule
  (alias runtest)
  (action
    (ignore-stdout
      (run ../../../bin/main.exe style --idempotence-check --ast-check
          --width 90 %{dep:docstrings.ml}))))

(rule
  (alias runtest)
  (action
    (ignore-stdout
      (run ../../../bin/main.exe style --idempotence-check --ast-check
          --width 90 %{dep:empty_seq_erasure.ml}))))

(rule
  (alias runtest)
  (action
    (ignore-stdout
      (run ../../../bin/main.exe style --idempotence-check --ast-check
          --width 90 %{dep:flush_hints_idempotence_issue.mli}))))

(rule
  (alias runtest)
  (action
    (ignore-stdout
      (run ../../../bin/main.exe style --idempotence-check --ast-check
          --width 90 %{dep:minimal_comments_idempotence.ml}))))

(rule
  (alias runtest)
  (action
    (ignore-stdout
      (run ../../../bin/main.exe style --idempotence-check --ast-check
          --width 90 %{dep:moving-comments_1.ml}))))

(rule
  (alias runtest)
  (action
    (ignore-stdout
      (run ../../../bin/main.exe style --idempotence-check --ast-check
          --width 90 %{dep:moving-comments_2.ml}))))

(rule
  (alias runtest)
  (action
    (ignore-stdout
      (run ../../../bin/main.exe style --idempotence-check --ast-check
          --width 90 %{dep:moving-comments_3.ml}))))

(rule
  (alias runtest)
  (action
    (ignore-stdout
      (run ../../../bin/main.exe style --idempotence-check --ast-check
          --width 90 %{dep:moving-comments_4.ml}))))

(rule
  (alias runtest)
  (action
    (ignore-stdout
      (run ../../../bin/main.exe style --idempotence-check --ast-check
          --width 90 %{dep:multiline_preceeding.ml}))))

(rule
  (alias runtest)
  (action
    (ignore-stdout
      (run ../../../bin/main.exe style --idempotence-check --ast-check
          --width 90 %{dep:post_unreachable_spacing.ml}))))

(rule
  (alias runtest)
  (action
    (ignore-stdout
      (run ../../../bin/main.exe style --idempotence-check --ast-check
          --width 14 %{dep:string_split.14.ml}))))

(rule
  (alias runtest)
  (action
    (ignore-stdout
      (run ../../../bin/main.exe style --idempotence-check --ast-check
          --width 14 %{dep:string_split_bb.14.ml}))))

(rule
  (alias runtest)
  (action
    (ignore-stdout
      (run ../../../bin/main.exe style --idempotence-check --ast-check
          --width 16 %{dep:string_split_trailing.16.ml}))))

(rule
  (alias runtest)
  (action
    (ignore-stdout
      (run ../../../bin/main.exe style --idempotence-check --ast-check
          --width 90 %{dep:trailing_semi.ml}))))

