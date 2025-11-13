(rule
  (target comments_insertion.out)
  (action
    (with-stdout-to %{target}
      (run ../../bin/stylo.exe %{dep:comments_insertion.ml}))))

(rule
  (target comments_insertion.snd-run)
  (action
    (with-stdout-to %{target}
      (run ../../bin/stylo.exe %{dep:comments_insertion.out}))))

(rule
  (alias runtest)
  (action (diff %{dep:comments_insertion.ref} %{dep:comments_insertion.out})))

; idempotence check
(rule
  (alias runtest)
  (action (diff %{dep:comments_insertion.out} %{dep:comments_insertion.snd-run})))

(rule
  (target flat_non_flat_groups_and_comments.out)
  (action
    (with-stdout-to %{target}
      (run ../../bin/stylo.exe %{dep:flat_non_flat_groups_and_comments.ml}))))

(rule
  (target flat_non_flat_groups_and_comments.snd-run)
  (action
    (with-stdout-to %{target}
      (run ../../bin/stylo.exe %{dep:flat_non_flat_groups_and_comments.out}))))

(rule
  (alias runtest)
  (action (diff %{dep:flat_non_flat_groups_and_comments.ref} %{dep:flat_non_flat_groups_and_comments.out})))

; idempotence check
(rule
  (alias runtest)
  (action (diff %{dep:flat_non_flat_groups_and_comments.out} %{dep:flat_non_flat_groups_and_comments.snd-run})))

(rule
  (target relative_nest.out)
  (action
    (with-stdout-to %{target}
      (run ../../bin/stylo.exe %{dep:relative_nest.ml}))))

(rule
  (target relative_nest.snd-run)
  (action
    (with-stdout-to %{target}
      (run ../../bin/stylo.exe %{dep:relative_nest.out}))))

(rule
  (alias runtest)
  (action (diff %{dep:relative_nest.ref} %{dep:relative_nest.out})))

; idempotence check
(rule
  (alias runtest)
  (action (diff %{dep:relative_nest.out} %{dep:relative_nest.snd-run})))

(rule
  (target tuples.out)
  (action
    (with-stdout-to %{target}
      (run ../../bin/stylo.exe %{dep:tuples.ml}))))

(rule
  (target tuples.snd-run)
  (action
    (with-stdout-to %{target}
      (run ../../bin/stylo.exe %{dep:tuples.out}))))

(rule
  (alias runtest)
  (action (diff %{dep:tuples.ref} %{dep:tuples.out})))

; idempotence check
(rule
  (alias runtest)
  (action (diff %{dep:tuples.out} %{dep:tuples.snd-run})))

