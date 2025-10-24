(rule
  (alias runtest)
  (action
    (with-stdout-to null
      (run ../../bin/stylo.exe -ast-check %{dep:docstrings.ml}))))

