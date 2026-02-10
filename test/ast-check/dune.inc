(rule
  (alias runtest)
  (action
    (with-stdout-to null
      (run ../../bin/main.exe -ast-check %{dep:docstrings.ml}))))

