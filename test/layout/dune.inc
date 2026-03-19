(rule
  (target comments_insertion.ml.out)
  (action
    (with-stdout-to %{target}
      (run %{project_root}/bin/main.exe style --width 90 %{dep:comments_insertion.ml}))))

(rule
  (target comments_insertion.ml.snd-run)
  (action
    (with-stdin-from %{dep:comments_insertion.ml.out}
      (with-stdout-to %{target}
        (run %{project_root}/bin/main.exe style --width 90 --stdin comments_insertion.ml)))))

(rule
  (alias runtest)
  (action (diff %{dep:comments_insertion.ml.stylo} %{dep:comments_insertion.ml.out})))

; idempotence check
(rule
  (alias runtest)
  (action (diff %{dep:comments_insertion.ml.out} %{dep:comments_insertion.ml.snd-run})))

(rule
  (target ext_attributes.ml.out)
  (action
    (with-stdout-to %{target}
      (run %{project_root}/bin/main.exe style --width 90 %{dep:ext_attributes.ml}))))

(rule
  (target ext_attributes.ml.snd-run)
  (action
    (with-stdin-from %{dep:ext_attributes.ml.out}
      (with-stdout-to %{target}
        (run %{project_root}/bin/main.exe style --width 90 --stdin ext_attributes.ml)))))

(rule
  (alias runtest)
  (action (diff %{dep:ext_attributes.ml.stylo} %{dep:ext_attributes.ml.out})))

; idempotence check
(rule
  (alias runtest)
  (action (diff %{dep:ext_attributes.ml.out} %{dep:ext_attributes.ml.snd-run})))

(rule
  (target flat_non_flat_groups_and_comments.ml.out)
  (action
    (with-stdout-to %{target}
      (run %{project_root}/bin/main.exe style --width 90 %{dep:flat_non_flat_groups_and_comments.ml}))))

(rule
  (target flat_non_flat_groups_and_comments.ml.snd-run)
  (action
    (with-stdin-from %{dep:flat_non_flat_groups_and_comments.ml.out}
      (with-stdout-to %{target}
        (run %{project_root}/bin/main.exe style --width 90 --stdin flat_non_flat_groups_and_comments.ml)))))

(rule
  (alias runtest)
  (action (diff %{dep:flat_non_flat_groups_and_comments.ml.stylo} %{dep:flat_non_flat_groups_and_comments.ml.out})))

; idempotence check
(rule
  (alias runtest)
  (action (diff %{dep:flat_non_flat_groups_and_comments.ml.out} %{dep:flat_non_flat_groups_and_comments.ml.snd-run})))

(rule
  (target lexer_directives.ml.out)
  (action
    (with-stdout-to %{target}
      (run %{project_root}/bin/main.exe style --width 90 %{dep:lexer_directives.ml}))))

(rule
  (target lexer_directives.ml.snd-run)
  (action
    (with-stdin-from %{dep:lexer_directives.ml.out}
      (with-stdout-to %{target}
        (run %{project_root}/bin/main.exe style --width 90 --stdin lexer_directives.ml)))))

(rule
  (alias runtest)
  (action (diff %{dep:lexer_directives.ml.stylo} %{dep:lexer_directives.ml.out})))

; idempotence check
(rule
  (alias runtest)
  (action (diff %{dep:lexer_directives.ml.out} %{dep:lexer_directives.ml.snd-run})))

(rule
  (target odoc_markup.mli.out)
  (action
    (with-stdout-to %{target}
      (run %{project_root}/bin/main.exe style --width 90 %{dep:odoc_markup.mli}))))

(rule
  (target odoc_markup.mli.snd-run)
  (action
    (with-stdin-from %{dep:odoc_markup.mli.out}
      (with-stdout-to %{target}
        (run %{project_root}/bin/main.exe style --width 90 --stdin odoc_markup.mli)))))

(rule
  (alias runtest)
  (action (diff %{dep:odoc_markup.mli.stylo} %{dep:odoc_markup.mli.out})))

; idempotence check
(rule
  (alias runtest)
  (action (diff %{dep:odoc_markup.mli.out} %{dep:odoc_markup.mli.snd-run})))

(rule
  (target odoc_markup_no_docstring.mli.out)
  (action
    (with-stdout-to %{target}
      (run %{project_root}/bin/main.exe style --width 90 %{dep:odoc_markup_no_docstring.mli}))))

(rule
  (target odoc_markup_no_docstring.mli.snd-run)
  (action
    (with-stdin-from %{dep:odoc_markup_no_docstring.mli.out}
      (with-stdout-to %{target}
        (run %{project_root}/bin/main.exe style --width 90 --stdin odoc_markup_no_docstring.mli)))))

(rule
  (alias runtest)
  (action (diff %{dep:odoc_markup_no_docstring.mli.stylo} %{dep:odoc_markup_no_docstring.mli.out})))

; idempotence check
(rule
  (alias runtest)
  (action (diff %{dep:odoc_markup_no_docstring.mli.out} %{dep:odoc_markup_no_docstring.mli.snd-run})))

(rule
  (target preceeding_and_comments.ml.out)
  (action
    (with-stdout-to %{target}
      (run %{project_root}/bin/main.exe style --width 90 %{dep:preceeding_and_comments.ml}))))

(rule
  (target preceeding_and_comments.ml.snd-run)
  (action
    (with-stdin-from %{dep:preceeding_and_comments.ml.out}
      (with-stdout-to %{target}
        (run %{project_root}/bin/main.exe style --width 90 --stdin preceeding_and_comments.ml)))))

(rule
  (alias runtest)
  (action (diff %{dep:preceeding_and_comments.ml.stylo} %{dep:preceeding_and_comments.ml.out})))

; idempotence check
(rule
  (alias runtest)
  (action (diff %{dep:preceeding_and_comments.ml.out} %{dep:preceeding_and_comments.ml.snd-run})))

(rule
  (target relative_nest.ml.out)
  (action
    (with-stdout-to %{target}
      (run %{project_root}/bin/main.exe style --width 90 %{dep:relative_nest.ml}))))

(rule
  (target relative_nest.ml.snd-run)
  (action
    (with-stdin-from %{dep:relative_nest.ml.out}
      (with-stdout-to %{target}
        (run %{project_root}/bin/main.exe style --width 90 --stdin relative_nest.ml)))))

(rule
  (alias runtest)
  (action (diff %{dep:relative_nest.ml.stylo} %{dep:relative_nest.ml.out})))

; idempotence check
(rule
  (alias runtest)
  (action (diff %{dep:relative_nest.ml.out} %{dep:relative_nest.ml.snd-run})))

(rule
  (target single_case_optional_pipe.ml.out)
  (action
    (with-stdout-to %{target}
      (run %{project_root}/bin/main.exe style --width 90 %{dep:single_case_optional_pipe.ml}))))

(rule
  (target single_case_optional_pipe.ml.snd-run)
  (action
    (with-stdin-from %{dep:single_case_optional_pipe.ml.out}
      (with-stdout-to %{target}
        (run %{project_root}/bin/main.exe style --width 90 --stdin single_case_optional_pipe.ml)))))

(rule
  (alias runtest)
  (action (diff %{dep:single_case_optional_pipe.ml.stylo} %{dep:single_case_optional_pipe.ml.out})))

; idempotence check
(rule
  (alias runtest)
  (action (diff %{dep:single_case_optional_pipe.ml.out} %{dep:single_case_optional_pipe.ml.snd-run})))

(rule
  (target string_lits.ml.out)
  (action
    (with-stdout-to %{target}
      (run %{project_root}/bin/main.exe style --width 90 %{dep:string_lits.ml}))))

(rule
  (target string_lits.ml.snd-run)
  (action
    (with-stdin-from %{dep:string_lits.ml.out}
      (with-stdout-to %{target}
        (run %{project_root}/bin/main.exe style --width 90 --stdin string_lits.ml)))))

(rule
  (alias runtest)
  (action (diff %{dep:string_lits.ml.stylo} %{dep:string_lits.ml.out})))

; idempotence check
(rule
  (alias runtest)
  (action (diff %{dep:string_lits.ml.out} %{dep:string_lits.ml.snd-run})))

(rule
  (target tuples.ml.out)
  (action
    (with-stdout-to %{target}
      (run %{project_root}/bin/main.exe style --width 90 %{dep:tuples.ml}))))

(rule
  (target tuples.ml.snd-run)
  (action
    (with-stdin-from %{dep:tuples.ml.out}
      (with-stdout-to %{target}
        (run %{project_root}/bin/main.exe style --width 90 --stdin tuples.ml)))))

(rule
  (alias runtest)
  (action (diff %{dep:tuples.ml.stylo} %{dep:tuples.ml.out})))

; idempotence check
(rule
  (alias runtest)
  (action (diff %{dep:tuples.ml.out} %{dep:tuples.ml.snd-run})))

