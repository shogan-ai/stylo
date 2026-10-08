(rule
  (target cases.ml.out)
  (action
    (with-stdout-to %{target}
      (run %{project_root}/bin/main.exe style --vertically-aligned --idempotence-check --ast-check --width 90 %{dep:cases.ml}))))

(rule
  (alias runtest)
  (action (diff %{dep:cases.ml.stylo} %{dep:cases.ml.out})))

(rule
  (target comments.ml.out)
  (action
    (with-stdout-to %{target}
      (run %{project_root}/bin/main.exe style --vertically-aligned --idempotence-check --ast-check --width 90 %{dep:comments.ml}))))

(rule
  (alias runtest)
  (action (diff %{dep:comments.ml.stylo} %{dep:comments.ml.out})))

(rule
  (target gadts.ml.out)
  (action
    (with-stdout-to %{target}
      (run %{project_root}/bin/main.exe style --vertically-aligned --idempotence-check --ast-check --width 90 %{dep:gadts.ml}))))

(rule
  (alias runtest)
  (action (diff %{dep:gadts.ml.stylo} %{dep:gadts.ml.out})))

(rule
  (target nested.ml.out)
  (action
    (with-stdout-to %{target}
      (run %{project_root}/bin/main.exe style --vertically-aligned --idempotence-check --ast-check --width 90 %{dep:nested.ml}))))

(rule
  (alias runtest)
  (action (diff %{dep:nested.ml.stylo} %{dep:nested.ml.out})))

(rule
  (target overflow.40.ml.out)
  (action
    (with-stdout-to %{target}
      (run %{project_root}/bin/main.exe style --vertically-aligned --idempotence-check --ast-check --width 40 %{dep:overflow.40.ml}))))

(rule
  (alias runtest)
  (action (diff %{dep:overflow.40.ml.stylo} %{dep:overflow.40.ml.out})))

(rule
  (target variants.ml.out)
  (action
    (with-stdout-to %{target}
      (run %{project_root}/bin/main.exe style --vertically-aligned --idempotence-check --ast-check --width 90 %{dep:variants.ml}))))

(rule
  (alias runtest)
  (action (diff %{dep:variants.ml.stylo} %{dep:variants.ml.out})))

(rule
  (target verbatim.ml.out)
  (action
    (with-stdout-to %{target}
      (run %{project_root}/bin/main.exe style --vertically-aligned --idempotence-check --ast-check --width 90 %{dep:verbatim.ml}))))

(rule
  (alias runtest)
  (action (diff %{dep:verbatim.ml.stylo} %{dep:verbatim.ml.out})))

(rule
  (target wrapping.60.ml.out)
  (action
    (with-stdout-to %{target}
      (run %{project_root}/bin/main.exe style --vertically-aligned --idempotence-check --ast-check --width 60 %{dep:wrapping.60.ml}))))

(rule
  (alias runtest)
  (action (diff %{dep:wrapping.60.ml.stylo} %{dep:wrapping.60.ml.out})))

