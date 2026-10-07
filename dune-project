(lang dune 3.15)

(name stylo)

(using menhir 3.0)

(generate_opam_files)

(package
 (name stylo-lib)
 (synopsis "Core libraries of the stylo code formatter for O(x)Caml")
 (depends
  (menhir
   (>= 20260209))
  ppxlib ; TODO: only require it in dev mode
  ))

(package
 (name stylo-jane)
 (synopsis "Styling O(x)Caml code, Jane Street style")
 (depends
  (stylo-lib
   (= :version))
  (cmdliner
   (>= 2.1.0))
  (menhir
   (>= 20260209))
  astring
  camlp-streams))
