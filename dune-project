(lang dune 3.15)

(name stylo)

(using menhir 3.0)

(generate_opam_files)

(package
  (name stylo)
  (synopsis "Styling O(x)Caml code")
  (depends
    cmdliner
    menhir
    ppxlib ; TODO: only require it in dev mode
    ))
