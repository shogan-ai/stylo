(rule
  (target ext_attributes.out)
  (action
    (with-stdout-to %{target}
      (run ../../../bin/main.exe -width 90 %{dep:ext_attributes.ml}))))

(rule
  (target ext_attributes.snd-run)
  (action
    (with-stdout-to %{target}
      (run ../../../bin/main.exe -width 90 %{dep:ext_attributes.out}))))

(rule
  (alias runtest)
  (action (diff %{dep:ext_attributes.ref} %{dep:ext_attributes.out})))

; idempotence check
(rule
  (alias runtest)
  (action (diff %{dep:ext_attributes.out} %{dep:ext_attributes.snd-run})))

(rule
  (target preceeding_and_comments.out)
  (action
    (with-stdout-to %{target}
      (run ../../../bin/main.exe -width 90 %{dep:preceeding_and_comments.ml}))))

(rule
  (target preceeding_and_comments.snd-run)
  (action
    (with-stdout-to %{target}
      (run ../../../bin/main.exe -width 90 %{dep:preceeding_and_comments.out}))))

(rule
  (alias runtest)
  (action (diff %{dep:preceeding_and_comments.ref} %{dep:preceeding_and_comments.out})))

; idempotence check
(rule
  (alias runtest)
  (action (diff %{dep:preceeding_and_comments.out} %{dep:preceeding_and_comments.snd-run})))

(rule
  (target relative_nest.out)
  (action
    (with-stdout-to %{target}
      (run ../../../bin/main.exe -width 90 %{dep:relative_nest.ml}))))

(rule
  (target relative_nest.snd-run)
  (action
    (with-stdout-to %{target}
      (run ../../../bin/main.exe -width 90 %{dep:relative_nest.out}))))

(rule
  (alias runtest)
  (action (diff %{dep:relative_nest.ref} %{dep:relative_nest.out})))

; idempotence check
(rule
  (alias runtest)
  (action (diff %{dep:relative_nest.out} %{dep:relative_nest.snd-run})))

(rule
  (target single_case_optional_pipe.out)
  (action
    (with-stdout-to %{target}
      (run ../../../bin/main.exe -width 90 %{dep:single_case_optional_pipe.ml}))))

(rule
  (target single_case_optional_pipe.snd-run)
  (action
    (with-stdout-to %{target}
      (run ../../../bin/main.exe -width 90 %{dep:single_case_optional_pipe.out}))))

(rule
  (alias runtest)
  (action (diff %{dep:single_case_optional_pipe.ref} %{dep:single_case_optional_pipe.out})))

; idempotence check
(rule
  (alias runtest)
  (action (diff %{dep:single_case_optional_pipe.out} %{dep:single_case_optional_pipe.snd-run})))

(rule
  (target string_lits.out)
  (action
    (with-stdout-to %{target}
      (run ../../../bin/main.exe -width 90 %{dep:string_lits.ml}))))

(rule
  (target string_lits.snd-run)
  (action
    (with-stdout-to %{target}
      (run ../../../bin/main.exe -width 90 %{dep:string_lits.out}))))

(rule
  (alias runtest)
  (action (diff %{dep:string_lits.ref} %{dep:string_lits.out})))

; idempotence check
(rule
  (alias runtest)
  (action (diff %{dep:string_lits.out} %{dep:string_lits.snd-run})))

(rule
  (target tuples.out)
  (action
    (with-stdout-to %{target}
      (run ../../../bin/main.exe -width 90 %{dep:tuples.ml}))))

(rule
  (target tuples.snd-run)
  (action
    (with-stdout-to %{target}
      (run ../../../bin/main.exe -width 90 %{dep:tuples.out}))))

(rule
  (alias runtest)
  (action (diff %{dep:tuples.ref} %{dep:tuples.out})))

; idempotence check
(rule
  (alias runtest)
  (action (diff %{dep:tuples.out} %{dep:tuples.snd-run})))

