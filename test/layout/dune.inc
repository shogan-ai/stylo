(rule
  (target comments_insertion.ml.out)
  (action
    (with-stdout-to %{target}
      (run %{project_root}/bin/main.exe style --idempotence-check --width 90 %{dep:comments_insertion.ml}))))

(rule
  (alias runtest)
  (action (diff %{dep:comments_insertion.ml.stylo} %{dep:comments_insertion.ml.out})))

(rule
  (target ext_attributes.ml.out)
  (action
    (with-stdout-to %{target}
      (run %{project_root}/bin/main.exe style --idempotence-check --width 90 %{dep:ext_attributes.ml}))))

(rule
  (alias runtest)
  (action (diff %{dep:ext_attributes.ml.stylo} %{dep:ext_attributes.ml.out})))

(rule
  (target flat_non_flat_groups_and_comments.ml.out)
  (action
    (with-stdout-to %{target}
      (run %{project_root}/bin/main.exe style --idempotence-check --width 90 %{dep:flat_non_flat_groups_and_comments.ml}))))

(rule
  (alias runtest)
  (action (diff %{dep:flat_non_flat_groups_and_comments.ml.stylo} %{dep:flat_non_flat_groups_and_comments.ml.out})))

(rule
  (target lexer_directives.ml.out)
  (action
    (with-stdout-to %{target}
      (run %{project_root}/bin/main.exe style --idempotence-check --width 90 %{dep:lexer_directives.ml}))))

(rule
  (alias runtest)
  (action (diff %{dep:lexer_directives.ml.stylo} %{dep:lexer_directives.ml.out})))

(rule
  (target odoc_markup.mli.out)
  (action
    (with-stdout-to %{target}
      (run %{project_root}/bin/main.exe style --idempotence-check --width 90 %{dep:odoc_markup.mli}))))

(rule
  (alias runtest)
  (action (diff %{dep:odoc_markup.mli.stylo} %{dep:odoc_markup.mli.out})))

(rule
  (target odoc_markup_no_docstring.mli.out)
  (action
    (with-stdout-to %{target}
      (run %{project_root}/bin/main.exe style --idempotence-check --width 90 %{dep:odoc_markup_no_docstring.mli}))))

(rule
  (alias runtest)
  (action (diff %{dep:odoc_markup_no_docstring.mli.stylo} %{dep:odoc_markup_no_docstring.mli.out})))

(rule
  (target odoc_markup_split.mli.out)
  (action
    (with-stdout-to %{target}
      (run %{project_root}/bin/main.exe style --idempotence-check --width 90 %{dep:odoc_markup_split.mli}))))

(rule
  (alias runtest)
  (action (diff %{dep:odoc_markup_split.mli.stylo} %{dep:odoc_markup_split.mli.out})))

(rule
  (target preceeding_and_comments.ml.out)
  (action
    (with-stdout-to %{target}
      (run %{project_root}/bin/main.exe style --idempotence-check --width 90 %{dep:preceeding_and_comments.ml}))))

(rule
  (alias runtest)
  (action (diff %{dep:preceeding_and_comments.ml.stylo} %{dep:preceeding_and_comments.ml.out})))

(rule
  (target relative_nest.ml.out)
  (action
    (with-stdout-to %{target}
      (run %{project_root}/bin/main.exe style --idempotence-check --width 90 %{dep:relative_nest.ml}))))

(rule
  (alias runtest)
  (action (diff %{dep:relative_nest.ml.stylo} %{dep:relative_nest.ml.out})))

(rule
  (target single_case_optional_pipe.ml.out)
  (action
    (with-stdout-to %{target}
      (run %{project_root}/bin/main.exe style --idempotence-check --width 90 %{dep:single_case_optional_pipe.ml}))))

(rule
  (alias runtest)
  (action (diff %{dep:single_case_optional_pipe.ml.stylo} %{dep:single_case_optional_pipe.ml.out})))

(rule
  (target string_lits.ml.out)
  (action
    (with-stdout-to %{target}
      (run %{project_root}/bin/main.exe style --idempotence-check --width 90 %{dep:string_lits.ml}))))

(rule
  (alias runtest)
  (action (diff %{dep:string_lits.ml.stylo} %{dep:string_lits.ml.out})))

(rule
  (target tuples.ml.out)
  (action
    (with-stdout-to %{target}
      (run %{project_root}/bin/main.exe style --idempotence-check --width 90 %{dep:tuples.ml}))))

(rule
  (alias runtest)
  (action (diff %{dep:tuples.ml.stylo} %{dep:tuples.ml.out})))

