#!/bin/sh

set -e

function hide_no_modes_no_modalities_ws_change {
  # empty inlined rules whose semantic actions return default values at the
  # right type.

  local at_or_atat="\(at_mode_expr\|atat_modalities_expr\)"
  sed -e "s/module_name_modal($at_or_atat,)/module_name_modal(\1)/g"
}

INLINE="
attrs_as_extattrs
empty_list
exp_unreachable
ext_attributes
ext_noattrs
inherit_field
inherit_field_semi
labeled_tuple_element_noprec
noext_attributes
no_modalities
no_modes
str_not_op
xlist
optional_structure_standalone_expression
"

function print_rules {
    dune exec ./tools/print-grammar/print_grammar.exe -- --inline "$INLINE" "$@"
}

function accept_diff {
  print_rules $1 | \
    hide_no_modes_no_modalities_ws_change
}

function do_diff {
    accept_diff $1 > $1.txt
    accept_diff $2 > $2.txt
    if (which patdiff &>/dev/null); then
        patdiff -alt-prev "$1" -alt-next "$2" \
            $1.txt $2.txt | \
            less -r
    else
        git diff --no-index $1.txt $2.txt
    fi
}

STYLOS="lib/parsing/parser.cmly"
OXCAMLS="vendor/oxcaml-frontend/parser.cmly"

BUILD_DIR="_build/default/"

dune build $STYLOS $OXCAMLS

do_diff "$BUILD_DIR$OXCAMLS" "$BUILD_DIR$STYLOS"
