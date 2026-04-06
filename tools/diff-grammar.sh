#!/bin/sh

set -e

function accept_match_cases {
  # TODO: upstream that one

  local initial="reversed_preceded_or_separated_nonempty_llist_BAR_match_case_"
  local updated="reversed_bar_llist_match_case_"
  sed -E -e "s/($initial|$updated)/<match cases>/g"
}

function hide_str_not_op {
  # just an inlined wrapper of the returned value, no impact on grammar
  # (see str_not_op in parser.mly)

  sed -E -e "s/_str_not_op(_[UL]IDENT_|_ident_)_/\1/g"
}

function hide_menhir_anonymous_fun_gensym_shifts {
  local modpat="(module_declaration_body___anonymous_)[58](_)"
  local menhir_attr="(\[@name .o.e___anonymous_)..(\])"
  local mod_ext_lid="(mk_longident_mod_ext_longident___anonymous_)5[01](_)"
  sed -E -e "s/($modpat|$menhir_attr|$mod_ext_lid)/$2X$3/g"
}

function accept_diff {
  cat $1 | \
    accept_match_cases | \
    hide_str_not_op | \
    hide_menhir_anonymous_fun_gensym_shifts
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

function do_diff {
    print_rules $1 > $1.txt
    print_rules $2 > $2.txt
    if (which patdiff &>/dev/null); then
        patdiff -alt-prev "$1" -alt-next "$2" \
            $1.txt $2.txt | \
            less -r
    else
        git diff --no-index --patience --word-diff=color $1.txt $2.txt
    fi
}

STYLOS="lib/parsing/parser.cmly"
OXCAMLS="vendor/oxcaml-frontend/parser.cmly"

BUILD_DIR="_build/default/"

dune build $STYLOS $OXCAMLS

do_diff "$BUILD_DIR$OXCAMLS" "$BUILD_DIR$STYLOS"
