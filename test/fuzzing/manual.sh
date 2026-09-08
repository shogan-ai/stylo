#!/bin/bash

report=$(mktemp /tmp/stylo_fuzzing.XXXXX)

ocamlgrammarfuzzer -0 \
  --check stylo --check-command _build/default/bin/main.exe \
  --cmly _build/default/lib/parsing/parser.cmly \
  --count 10000 --reduce \
  --comments-randomize-space --comments-randomize-count \
  --avoid METAOCAML_ESCAPE --avoid METAOCAML_BRACKET_OPEN \
  --avoid DOLLAR --avoid LESSLBRACKET --avoid RBRACKETGREATER \
  --print-entrypoint --entrypoint implementation \
  --comments-default-expectation 0.1 \
  --comments-token-expectation "1.5 EQUAL MINUSGREATER BAR" \
  --terminal "BORROW=borrow_" \
  --terminal "HASH_CHAR=#'a'" \
  --terminal "HASHFALSE=#false" \
  --terminal "HASHTRUE=#true" \
  --terminal "KIND=kind_" \
  --terminal "LEXER_DIRECTIVE=#syntax foo on" \
  --terminal "ONCE=once_" \
  --terminal "POLY=poly_" \
  --terminal "REPR=repr_" \
  --terminal "UNIQUE=unique_" \
  > $report

echo "Check $report"
