(rule
  (with-stdout-to printed-docstrings.ml
    (run ../bin/stylo.exe %{dep:docstrings.ml})))

(rule
  (alias docstrings)
  (action (diff %{dep:docstrings.ml} %{dep:printed-docstrings.ml})))

(alias
  (name all-tests)
  (deps (alias docstrings)))

;; ---

