(rule
  (target tuples.out)
  (action
    (with-stdout-to tuples.out
      (run ../../bin/stylo.exe %{dep:tuples.ml}))))

(rule
  (alias runtest)
  (action (diff %{dep:tuples.ref} %{dep:tuples.out})))

