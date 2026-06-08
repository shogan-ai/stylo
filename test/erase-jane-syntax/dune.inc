(rule
  (target abstract_kinds.ml.out)
  (action
    (with-stdout-to %{target}
      (run %{project_root}/bin/main.exe style --erase-jane-syntax %{dep:abstract_kinds.ml}))))

(rule
  (alias runtest)
  (action (diff %{dep:abstract_kinds.ml.ref} %{dep:abstract_kinds.ml.out})))

(rule
  (target borrow.ml.out)
  (action
    (with-stdout-to %{target}
      (run %{project_root}/bin/main.exe style --erase-jane-syntax %{dep:borrow.ml}))))

(rule
  (alias runtest)
  (action (diff %{dep:borrow.ml.ref} %{dep:borrow.ml.out})))

(rule
  (target default_modalities.ml.out)
  (action
    (with-stdout-to %{target}
      (run %{project_root}/bin/main.exe style --erase-jane-syntax %{dep:default_modalities.ml}))))

(rule
  (alias runtest)
  (action (diff %{dep:default_modalities.ml.ref} %{dep:default_modalities.ml.out})))

(rule
  (target default_modalities.mli.out)
  (action
    (with-stdout-to %{target}
      (run %{project_root}/bin/main.exe style --erase-jane-syntax %{dep:default_modalities.mli}))))

(rule
  (alias runtest)
  (action (diff %{dep:default_modalities.mli.ref} %{dep:default_modalities.mli.out})))

(rule
  (target default_modalities2.mli.out)
  (action
    (with-stdout-to %{target}
      (run %{project_root}/bin/main.exe style --erase-jane-syntax %{dep:default_modalities2.mli}))))

(rule
  (alias runtest)
  (action (diff %{dep:default_modalities2.mli.ref} %{dep:default_modalities2.mli.out})))

(rule
  (target default_modalities3.mli.out)
  (action
    (with-stdout-to %{target}
      (run %{project_root}/bin/main.exe style --erase-jane-syntax %{dep:default_modalities3.mli}))))

(rule
  (alias runtest)
  (action (diff %{dep:default_modalities3.mli.ref} %{dep:default_modalities3.mli.out})))

(rule
  (target default_modalities4.mli.out)
  (action
    (with-stdout-to %{target}
      (run %{project_root}/bin/main.exe style --erase-jane-syntax %{dep:default_modalities4.mli}))))

(rule
  (alias runtest)
  (action (diff %{dep:default_modalities4.mli.ref} %{dep:default_modalities4.mli.out})))

(rule
  (target implicit_source_position.ml.out)
  (action
    (with-stdout-to %{target}
      (run %{project_root}/bin/main.exe style --erase-jane-syntax %{dep:implicit_source_position.ml}))))

(rule
  (alias runtest)
  (action (diff %{dep:implicit_source_position.ml.ref} %{dep:implicit_source_position.ml.out})))

(rule
  (target kind_substitutions.ml.out)
  (action
    (with-stdout-to %{target}
      (run %{project_root}/bin/main.exe style --erase-jane-syntax %{dep:kind_substitutions.ml}))))

(rule
  (alias runtest)
  (action (diff %{dep:kind_substitutions.ml.ref} %{dep:kind_substitutions.ml.out})))

(rule
  (target layout_annotation.ml.out)
  (action
    (with-stdout-to %{target}
      (run %{project_root}/bin/main.exe style --erase-jane-syntax %{dep:layout_annotation.ml}))))

(rule
  (alias runtest)
  (action (diff %{dep:layout_annotation.ml.ref} %{dep:layout_annotation.ml.out})))

(rule
  (target local.ml.out)
  (action
    (with-stdout-to %{target}
      (run %{project_root}/bin/main.exe style --erase-jane-syntax %{dep:local.ml}))))

(rule
  (alias runtest)
  (action (diff %{dep:local.ml.ref} %{dep:local.ml.out})))

(rule
  (target local_erased.ml.out)
  (action
    (with-stdout-to %{target}
      (run %{project_root}/bin/main.exe style --erase-jane-syntax %{dep:local_erased.ml}))))

(rule
  (alias runtest)
  (action (diff %{dep:local_erased.ml.ref} %{dep:local_erased.ml.out})))

(rule
  (target modes.ml.out)
  (action
    (with-stdout-to %{target}
      (run %{project_root}/bin/main.exe style --erase-jane-syntax %{dep:modes.ml}))))

(rule
  (alias runtest)
  (action (diff %{dep:modes.ml.ref} %{dep:modes.ml.out})))

(rule
  (target stack.ml.out)
  (action
    (with-stdout-to %{target}
      (run %{project_root}/bin/main.exe style --erase-jane-syntax %{dep:stack.ml}))))

(rule
  (alias runtest)
  (action (diff %{dep:stack.ml.ref} %{dep:stack.ml.out})))

(rule
  (target unboxed_literals.ml.out)
  (action
    (with-stdout-to %{target}
      (run %{project_root}/bin/main.exe style --erase-jane-syntax %{dep:unboxed_literals.ml}))))

(rule
  (alias runtest)
  (action (diff %{dep:unboxed_literals.ml.ref} %{dep:unboxed_literals.ml.out})))

(rule
  (target unboxed_record.ml.out)
  (action
    (with-stdout-to %{target}
      (run %{project_root}/bin/main.exe style --erase-jane-syntax %{dep:unboxed_record.ml}))))

(rule
  (alias runtest)
  (action (diff %{dep:unboxed_record.ml.ref} %{dep:unboxed_record.ml.out})))

(rule
  (target unboxed_records_cmts_attrs.ml.out)
  (action
    (with-stdout-to %{target}
      (run %{project_root}/bin/main.exe style --erase-jane-syntax %{dep:unboxed_records_cmts_attrs.ml}))))

(rule
  (alias runtest)
  (action (diff %{dep:unboxed_records_cmts_attrs.ml.ref} %{dep:unboxed_records_cmts_attrs.ml.out})))

(rule
  (target unboxed_tuples.ml.out)
  (action
    (with-stdout-to %{target}
      (run %{project_root}/bin/main.exe style --erase-jane-syntax %{dep:unboxed_tuples.ml}))))

(rule
  (alias runtest)
  (action (diff %{dep:unboxed_tuples.ml.ref} %{dep:unboxed_tuples.ml.out})))

(rule
  (target unboxed_tuples_cmts_attrs.ml.out)
  (action
    (with-stdout-to %{target}
      (run %{project_root}/bin/main.exe style --erase-jane-syntax %{dep:unboxed_tuples_cmts_attrs.ml}))))

(rule
  (alias runtest)
  (action (diff %{dep:unboxed_tuples_cmts_attrs.ml.ref} %{dep:unboxed_tuples_cmts_attrs.ml.out})))

(rule
  (target unboxed_types.ml.out)
  (action
    (with-stdout-to %{target}
      (run %{project_root}/bin/main.exe style --erase-jane-syntax %{dep:unboxed_types.ml}))))

(rule
  (alias runtest)
  (action (diff %{dep:unboxed_types.ml.ref} %{dep:unboxed_types.ml.out})))

(rule
  (target unboxed_types2.ml.out)
  (action
    (with-stdout-to %{target}
      (run %{project_root}/bin/main.exe style --erase-jane-syntax %{dep:unboxed_types2.ml}))))

(rule
  (alias runtest)
  (action (diff %{dep:unboxed_types2.ml.ref} %{dep:unboxed_types2.ml.out})))

