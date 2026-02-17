#!/bin/sh

set -e

function do_diff {
    if [[ -x patdiff ]]; then
        patdiff $1 $2
    else
        git diff --no-index $1 $2
    fi
}

STYLOS="lib/parsing/raw_grammar.txt"
OXCAMLS="vendor/oxcaml-frontend/raw_grammar.txt"

BUILD_DIR="_build/default/"

dune build $STYLOS $OXCAMLS

do_diff "$BUILD_DIR$OXCAMLS" "$BUILD_DIR$STYLOS"
