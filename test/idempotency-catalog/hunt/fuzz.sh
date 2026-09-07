#!/bin/bash

sentences=$(mktemp stylo_idempotence_fuzz.XXXXX)

# comments are not printed in the report in "check" mode, so instead we
# just save the sentences and input them manually
# --check stylo --check-command _build/default/bin/main.exe \

# Also temporarily disabled:
# --comments-randomize-length \
ocamlgrammarfuzzer -0 \
  --cmly _build/default/lib/parsing/parser.cmly \
  --count 1000 --comments \
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
  > $sentences

BIN=./_build/default/bin
$BIN/fuzzer_driver.exe --idempotence-check --ignore-output-syntax-errors \
    --cmd $BIN/main.exe -j 8 $sentences

rm $sentences
