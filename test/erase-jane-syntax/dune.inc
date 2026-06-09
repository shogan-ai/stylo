(rule
  (target abstract_kinds.ml.out)
  (action
    (with-stdout-to %{target}
      (run %{project_root}/bin/main.exe style --erase-jane-syntax %{dep:abstract_kinds.ml}))))

(rule
  (alias diff-abstract_kinds.ml)
  (action (diff %{dep:abstract_kinds.ml.ref} %{dep:abstract_kinds.ml.out})))

(alias
  (name runtest)
  (deps (alias diff-abstract_kinds.ml)))

(rule
  (target borrow.ml.out)
  (action
    (with-stdout-to %{target}
      (run %{project_root}/bin/main.exe style --erase-jane-syntax %{dep:borrow.ml}))))

(rule
  (alias diff-borrow.ml)
  (action (diff %{dep:borrow.ml.ref} %{dep:borrow.ml.out})))

(alias
  (name runtest)
  (deps (alias diff-borrow.ml)))

(rule
  (target default_modalities.ml.out)
  (action
    (with-stdout-to %{target}
      (run %{project_root}/bin/main.exe style --erase-jane-syntax %{dep:default_modalities.ml}))))

(rule
  (alias diff-default_modalities.ml)
  (action (diff %{dep:default_modalities.ml.ref} %{dep:default_modalities.ml.out})))

(alias
  (name runtest)
  (deps (alias diff-default_modalities.ml)))

(rule
  (target default_modalities.mli.out)
  (action
    (with-stdout-to %{target}
      (run %{project_root}/bin/main.exe style --erase-jane-syntax %{dep:default_modalities.mli}))))

(rule
  (alias diff-default_modalities.mli)
  (action (diff %{dep:default_modalities.mli.ref} %{dep:default_modalities.mli.out})))

(alias
  (name runtest)
  (deps (alias diff-default_modalities.mli)))

(rule
  (target default_modalities2.mli.out)
  (action
    (with-stdout-to %{target}
      (run %{project_root}/bin/main.exe style --erase-jane-syntax %{dep:default_modalities2.mli}))))

(rule
  (alias diff-default_modalities2.mli)
  (action (diff %{dep:default_modalities2.mli.ref} %{dep:default_modalities2.mli.out})))

(alias
  (name runtest)
  (deps (alias diff-default_modalities2.mli)))

(rule
  (target default_modalities3.mli.out)
  (action
    (with-stdout-to %{target}
      (run %{project_root}/bin/main.exe style --erase-jane-syntax %{dep:default_modalities3.mli}))))

(rule
  (alias diff-default_modalities3.mli)
  (action (diff %{dep:default_modalities3.mli.ref} %{dep:default_modalities3.mli.out})))

(alias
  (name runtest)
  (deps (alias diff-default_modalities3.mli)))

(rule
  (target default_modalities4.mli.out)
  (action
    (with-stdout-to %{target}
      (run %{project_root}/bin/main.exe style --erase-jane-syntax %{dep:default_modalities4.mli}))))

(rule
  (alias diff-default_modalities4.mli)
  (action (diff %{dep:default_modalities4.mli.ref} %{dep:default_modalities4.mli.out})))

(alias
  (name runtest)
  (deps (alias diff-default_modalities4.mli)))

(rule
  (target implicit_source_position.ml.out)
  (action
    (with-stdout-to %{target}
      (run %{project_root}/bin/main.exe style --erase-jane-syntax %{dep:implicit_source_position.ml}))))

(rule
  (alias diff-implicit_source_position.ml)
  (action (diff %{dep:implicit_source_position.ml.ref} %{dep:implicit_source_position.ml.out})))

(alias
  (name runtest)
  (deps (alias diff-implicit_source_position.ml)))

(rule
  (target kind_substitutions.ml.out)
  (action
    (with-stdout-to %{target}
      (run %{project_root}/bin/main.exe style --erase-jane-syntax %{dep:kind_substitutions.ml}))))

(rule
  (alias diff-kind_substitutions.ml)
  (action (diff %{dep:kind_substitutions.ml.ref} %{dep:kind_substitutions.ml.out})))

(alias
  (name runtest)
  (deps (alias diff-kind_substitutions.ml)))

(rule
  (target layout_annotation.ml.out)
  (action
    (with-stdout-to %{target}
      (run %{project_root}/bin/main.exe style --erase-jane-syntax %{dep:layout_annotation.ml}))))

(rule
  (alias diff-layout_annotation.ml)
  (action (diff %{dep:layout_annotation.ml.ref} %{dep:layout_annotation.ml.out})))

(alias
  (name runtest)
  (deps (alias diff-layout_annotation.ml)))

(rule
  (target local.ml.out)
  (action
    (with-stdout-to %{target}
      (run %{project_root}/bin/main.exe style --erase-jane-syntax %{dep:local.ml}))))

(rule
  (alias diff-local.ml)
  (action (diff %{dep:local.ml.ref} %{dep:local.ml.out})))

(alias
  (name runtest)
  (deps (alias diff-local.ml)))

(rule
  (target local_erased.ml.out)
  (action
    (with-stdout-to %{target}
      (run %{project_root}/bin/main.exe style --erase-jane-syntax %{dep:local_erased.ml}))))

(rule
  (alias diff-local_erased.ml)
  (action (diff %{dep:local_erased.ml.ref} %{dep:local_erased.ml.out})))

(alias
  (name runtest)
  (deps (alias diff-local_erased.ml)))

(rule
  (target modes.ml.out)
  (action
    (with-stdout-to %{target}
      (run %{project_root}/bin/main.exe style --erase-jane-syntax %{dep:modes.ml}))))

(rule
  (alias diff-modes.ml)
  (action (diff %{dep:modes.ml.ref} %{dep:modes.ml.out})))

(alias
  (name runtest)
  (deps (alias diff-modes.ml)))

(rule
  (target stack.ml.out)
  (action
    (with-stdout-to %{target}
      (run %{project_root}/bin/main.exe style --erase-jane-syntax %{dep:stack.ml}))))

(rule
  (alias diff-stack.ml)
  (action (diff %{dep:stack.ml.ref} %{dep:stack.ml.out})))

(alias
  (name runtest)
  (deps (alias diff-stack.ml)))

(rule
  (target unboxed_literals.ml.out)
  (action
    (with-stdout-to %{target}
      (run %{project_root}/bin/main.exe style --erase-jane-syntax %{dep:unboxed_literals.ml}))))

(rule
  (alias diff-unboxed_literals.ml)
  (action (diff %{dep:unboxed_literals.ml.ref} %{dep:unboxed_literals.ml.out})))

(alias
  (name runtest)
  (deps (alias diff-unboxed_literals.ml)))

(rule
  (target unboxed_record.ml.out)
  (action
    (with-stdout-to %{target}
      (run %{project_root}/bin/main.exe style --erase-jane-syntax %{dep:unboxed_record.ml}))))

(rule
  (alias diff-unboxed_record.ml)
  (action (diff %{dep:unboxed_record.ml.ref} %{dep:unboxed_record.ml.out})))

(alias
  (name runtest)
  (deps (alias diff-unboxed_record.ml)))

(rule
  (target unboxed_records_cmts_attrs.ml.out)
  (action
    (with-stdout-to %{target}
      (run %{project_root}/bin/main.exe style --erase-jane-syntax %{dep:unboxed_records_cmts_attrs.ml}))))

(rule
  (alias diff-unboxed_records_cmts_attrs.ml)
  (action (diff %{dep:unboxed_records_cmts_attrs.ml.ref} %{dep:unboxed_records_cmts_attrs.ml.out})))

(alias
  (name runtest)
  (deps (alias diff-unboxed_records_cmts_attrs.ml)))

(rule
  (target unboxed_tuples.ml.out)
  (action
    (with-stdout-to %{target}
      (run %{project_root}/bin/main.exe style --erase-jane-syntax %{dep:unboxed_tuples.ml}))))

(rule
  (alias diff-unboxed_tuples.ml)
  (action (diff %{dep:unboxed_tuples.ml.ref} %{dep:unboxed_tuples.ml.out})))

(alias
  (name runtest)
  (deps (alias diff-unboxed_tuples.ml)))

(rule
  (target unboxed_tuples_cmts_attrs.ml.out)
  (action
    (with-stdout-to %{target}
      (run %{project_root}/bin/main.exe style --erase-jane-syntax %{dep:unboxed_tuples_cmts_attrs.ml}))))

(rule
  (alias diff-unboxed_tuples_cmts_attrs.ml)
  (action (diff %{dep:unboxed_tuples_cmts_attrs.ml.ref} %{dep:unboxed_tuples_cmts_attrs.ml.out})))

(alias
  (name runtest)
  (deps (alias diff-unboxed_tuples_cmts_attrs.ml)))

(rule
  (target unboxed_types.ml.out)
  (action
    (with-stdout-to %{target}
      (run %{project_root}/bin/main.exe style --erase-jane-syntax %{dep:unboxed_types.ml}))))

(rule
  (alias diff-unboxed_types.ml)
  (action (diff %{dep:unboxed_types.ml.ref} %{dep:unboxed_types.ml.out})))

(alias
  (name runtest)
  (deps (alias diff-unboxed_types.ml)))

(rule
  (target unboxed_types2.ml.out)
  (action
    (with-stdout-to %{target}
      (run %{project_root}/bin/main.exe style --erase-jane-syntax %{dep:unboxed_types2.ml}))))

(rule
  (alias diff-unboxed_types2.ml)
  (action (diff %{dep:unboxed_types2.ml.ref} %{dep:unboxed_types2.ml.out})))

(alias
  (name runtest)
  (deps (alias diff-unboxed_types2.ml)))

