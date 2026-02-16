(rule
  (alias runtest)
  (action
    (with-stdout-to null
      (run ../../bin/main.exe style --ast-check %{dep:docstrings.ml}))))

