#!/bin/bash

BIN=./_build/default/bin

sentences=$(mktemp stylo_idempotence_fuzz.XXXXX)

fuzzer_flags=(
  ## comments are not printed in the report in "check" mode, so instead we
  ## just save the sentences and input them manually
  # --check stylo --check-command $BIN/main.exe \
  --cmly _build/default/lib/parsing/parser.cmly
  ## To properly fuzz comment's attachement the generated sentences need to
  ## contain newlines, so we ask the fuzzer to separate entries with NUL:
  -0
  ## Keep runs short, with the right expectation biases issues pop up quickly
  --count 50
  --comments --comments-randomize-space
  --comments-randomize-count # --comments-randomize-length
  ## We want to see very few comments (1 every 10 tokens), excepts for some
  ## specific tokens where we want more than 1 (on average).
  --comments-default-expectation 0.1
  --comments-token-expectation "1.5 EQUAL MINUSGREATER BAR"
  ## Arbitrary choice: focus on structure / expressions for now
  --print-entrypoint
  --entrypoint implementation
  ## Skip/avoid quotations and metaocaml syntaxes
  --avoid METAOCAML_ESCAPE
  --avoid METAOCAML_BRACKET_OPEN
  --avoid DOLLAR
  --avoid LESSLBRACKET
  --avoid RBRACKETGREATER
  ## Usual noise: ocamlgrammarfuzzer doesn't know about all the tokens, so we
  ## tell it how to print them
  --terminal "BORROW=borrow_"
  --terminal "HASH_CHAR=#'a'"
  --terminal "HASHFALSE=#false"
  --terminal "HASHTRUE=#true"
  --terminal "KIND=kind_"
  --terminal "LEXER_DIRECTIVE=#syntax foo on"
  --terminal "ONCE=once_"
  --terminal "POLY=poly_"
  --terminal "REPR=repr_"
  --terminal "UNIQUE=unique_"
)

ocamlgrammarfuzzer "${fuzzer_flags[@]}" > $sentences

failures=$(mktemp -d /tmp/stylo_idempotence_failures.XXXXX)

stylo_flags=(
  --cmd $BIN/main.exe -j 8
  --failures-dir $failures
  --idempotence-check
  --ignore-output-syntax-errors # we only want idempotency bugs to show up
)

$BIN/fuzzer_driver.exe "${stylo_flags[@]}" $sentences

rm $sentences

script_dir=$(cd "$(dirname "$0")" && pwd)
for f in "$failures"/*.ml "$failures"/*.mli; do
  [ -e "$f" ] || continue
  python3 "$script_dir/minimize.py" "$f"
done
