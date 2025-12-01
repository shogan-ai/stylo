(rule
  (target comments_insertion.out)
  (action
    (with-stdout-to %{target}
      (run ../../../bin/stylo.exe %{dep:comments_insertion.ml}))))

(rule
  (target comments_insertion.snd-run)
  (action
    (with-stdout-to %{target}
      (run ../../../bin/stylo.exe %{dep:comments_insertion.out}))))

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
      (run ../../../bin/stylo.exe %{dep:flat_non_flat_groups_and_comments.ml}))))

(rule
  (target flat_non_flat_groups_and_comments.snd-run)
  (action
    (with-stdout-to %{target}
      (run ../../../bin/stylo.exe %{dep:flat_non_flat_groups_and_comments.out}))))

(rule
  (alias runtest)
  (action (diff %{dep:flat_non_flat_groups_and_comments.ref} %{dep:flat_non_flat_groups_and_comments.out})))

; idempotence check
(rule
  (alias runtest)
  (action (diff %{dep:flat_non_flat_groups_and_comments.out} %{dep:flat_non_flat_groups_and_comments.snd-run})))

