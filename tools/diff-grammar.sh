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

function do_diff {
    if [[ -x patdiff ]]; then
        patdiff <(accept_diff $1) <(accept_diff $2)
    else
        git diff --no-index <(accept_diff $1) <(accept_diff $2)
    fi
}

STYLOS="lib/parsing/raw_grammar.txt"
OXCAMLS="vendor/oxcaml-frontend/raw_grammar.txt"

BUILD_DIR="_build/default/"

dune build $STYLOS $OXCAMLS

do_diff "$BUILD_DIR$OXCAMLS" "$BUILD_DIR$STYLOS"
