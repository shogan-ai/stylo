(rule
  (target flat_non_flat_groups_and_comments.out)
  (action
    (with-stdout-to flat_non_flat_groups_and_comments.out
      (run ../../bin/stylo.exe %{dep:flat_non_flat_groups_and_comments.ml}))))

(rule
  (alias runtest)
  (action (diff %{dep:flat_non_flat_groups_and_comments.ref} %{dep:flat_non_flat_groups_and_comments.out})))

(rule
  (target tuples.out)
  (action
    (with-stdout-to tuples.out
      (run ../../bin/stylo.exe %{dep:tuples.ml}))))

(rule
  (alias runtest)
  (action (diff %{dep:tuples.ref} %{dep:tuples.out})))

