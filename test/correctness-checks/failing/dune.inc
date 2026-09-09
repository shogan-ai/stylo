(rule
  (alias runtest)
  (action
    (with-accepted-exit-codes 123
      (ignore-stdout
      (ignore-stderr
        (run ../../../bin/main.exe style --idempotence-check --ast-check
            %{dep:moving-comments_1.ml}))))))

(rule
  (alias runtest)
  (action
    (with-accepted-exit-codes 123
      (ignore-stdout
      (ignore-stderr
        (run ../../../bin/main.exe style --idempotence-check --ast-check
            %{dep:moving-comments_2.ml}))))))

(rule
  (alias runtest)
  (action
    (with-accepted-exit-codes 123
      (ignore-stdout
      (ignore-stderr
        (run ../../../bin/main.exe style --idempotence-check --ast-check
            %{dep:moving-comments_3.ml}))))))

(rule
  (alias runtest)
  (action
    (with-accepted-exit-codes 123
      (ignore-stdout
      (ignore-stderr
        (run ../../../bin/main.exe style --idempotence-check --ast-check
            %{dep:moving-comments_4.ml}))))))

