(rule
  (alias runtest)
  (action
    (with-accepted-exit-codes 123
      (ignore-stdout
      (ignore-stderr
        (run ../../../bin/main.exe style --idempotence-check --ast-check
            %{dep:blank_lines.ml}))))))

