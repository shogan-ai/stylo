(rule
  (alias runtest)
  (action
    (run ../../bin/stylo.exe -ast-check %{dep:docstrings.ml})))

