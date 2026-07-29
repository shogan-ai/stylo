#!/usr/bin/env bash
# Upgrade the vendored oxcaml frontend and merge the upstream delta into
# stylo's parser (lib/parsing), leaving conflict markers where local
# adaptations collide with upstream changes.
#
# See README.md in this directory for the full workflow, including how to
# resolve the conflicts this script leaves behind.
set -euo pipefail

usage() {
  echo "usage: $0 <path-to-oxcaml-checkout> [<rev>]" >&2
  echo "  <rev> defaults to HEAD. Both <rev> and the commit recorded in" >&2
  echo "  upstream-commit.txt must be present in the checkout." >&2
  exit 2
}

die() { echo "repatch: error: $*" >&2; exit 1; }

[ $# -ge 1 ] && [ $# -le 2 ] || usage

SCRIPT_DIR=$(cd "$(dirname "$0")" && pwd)
STYLO=$(cd "$SCRIPT_DIR/../.." && pwd)
VENDOR_REL=vendor/oxcaml-frontend
LIB_REL=lib/parsing

# ----------------------------------------------------------------------------
# File lists (single source of truth).
#
# TRACKED: upstream files vendored in this directory, as paths relative to the
# oxcaml repository root; the basename is the filename used here.  Files of
# this directory NOT in this list are local and never touched by this script:
# the shims clflags.ml, misc.ml and pprintast.ml (hand-trimmed stand-ins that
# carry just enough for the library to build), plus dune, README.md,
# upstream-commit.txt and the generated ast_mapper.ml.
TRACKED=(
  parsing/ast_helper.ml
  parsing/ast_helper.mli
  parsing/asttypes.mli
  parsing/builtin_attributes.ml
  parsing/docstrings.ml
  parsing/docstrings.mli
  parsing/language_extension.ml
  parsing/language_extension.mli
  utils/language_extension_kernel.ml
  utils/language_extension_kernel.mli
  parsing/lexer.mli
  parsing/lexer.mll
  parsing/location.ml
  parsing/location.mli
  parsing/longident.ml
  parsing/longident.mli
  parsing/parse.ml
  parsing/parse.mli
  parsing/parser.mly
  parsing/parser_types.ml
  parsing/parser_types.mli
  parsing/parsetree.mli
  parsing/printast.ml
  parsing/printast.mli
  parsing/syntaxerr.ml
  parsing/syntaxerr.mli
  utils/terminfo.ml
  utils/terminfo.mli
  utils/warnings.ml
  utils/warnings.mli
)

# LIB_MERGE: the lib/parsing files that are CST-adapted copies of the vendor
# files of the same name; the upstream delta is merged into them, leaving
# conflict markers where it collides with the CST adaptations.  The other
# lib/parsing files are either stylo-only (parser_tokens.mly, tokens.ml,
# tokens_of_tree.ml, docstring.ml, lexer_directive.ml, ...) or share a name
# with an upstream file but have diverged too far to track it line-by-line
# (see LIB_SKIPPED below).
LIB_MERGE=(
  asttypes.mli
  docstrings.ml
  docstrings.mli
  lexer.mli
  lexer.mll
  parser.mly
  parser_types.ml
  parser_types.mli
  parsetree.mli
)

# Upstream files whose lib/parsing namesakes have diverged too far to merge
# (location.*/longident.* are truncated copies, parse.ml is a from-scratch
# rewrite, and ast_helper.* is edited on nearly every line, so line-level
# merges degenerate into giant conflicts).  Not merged; upstream changes to
# them are only reported, for review by hand.
LIB_SKIPPED=(
  parsing/ast_helper.ml
  parsing/ast_helper.mli
  parsing/location.ml
  parsing/location.mli
  parsing/longident.ml
  parsing/longident.mli
  parsing/parse.ml
)

# ----------------------------------------------------------------------------
# Validate arguments and revisions.

[ -d "$1" ] || die "no such directory: $1"
OXCAML=$(git -C "$1" rev-parse --show-toplevel 2>/dev/null) \
  || die "$1 is not a git checkout"
NEW=$(git -C "$OXCAML" rev-parse --verify --quiet "${2:-HEAD}^{commit}") \
  || die "cannot resolve revision '${2:-HEAD}' in $OXCAML"
OLD=$(cat "$STYLO/$VENDOR_REL/upstream-commit.txt")
git -C "$OXCAML" cat-file -e "$OLD^{commit}" 2>/dev/null \
  || die "last-import commit $OLD (from upstream-commit.txt) is not in \
$OXCAML (shallow clone? try: git -C $OXCAML fetch --unshallow)"

if [ "$OLD" = "$NEW" ]; then
  echo "repatch: already at $NEW; nothing to do."
  exit 0
fi

# Refuse to run on locally-modified trees: after (or during) a repatch, the
# way back is `git checkout -- $VENDOR_REL $LIB_REL`, which must not eat
# unrelated work.
if [ -n "$(git -C "$STYLO" status --porcelain --untracked-files=no \
             -- "$VENDOR_REL" "$LIB_REL")" ]; then
  die "uncommitted changes under $VENDOR_REL or $LIB_REL; commit or stash \
them first"
fi

# The grammar baseline is the only step that needs a build; make sure it will
# get a dune that works for stylo.
if [ -d "$STYLO/_opam/bin" ]; then
  PATH=$STYLO/_opam/bin:$PATH
fi
command -v dune > /dev/null \
  || die "no dune on PATH, and no project switch at $STYLO/_opam/bin"
(cd "$STYLO" && dune --version > /dev/null 2>&1) \
  || die "the dune on PATH ($(command -v dune)) does not work in $STYLO \
(jane's version-dispatched dune? broken switch?); stylo expects its own \
dune, normally from $STYLO/_opam/bin"

# ----------------------------------------------------------------------------
# Workspaces and cleanup.

TMP=$(mktemp -d)                                          # deleted on exit
REPORT=$(mktemp -d /tmp/stylo-repatch.XXXXXX) # kept for the user
mkdir -p "$TMP/old" "$TMP/new"
MODIFIED=0

on_exit() {
  status=$?
  rm -rf "$TMP"
  if [ "$status" -ne 0 ] && [ "$MODIFIED" -eq 1 ]; then
    echo >&2
    echo "repatch: failed part-way; the tree may be partially modified." >&2
    echo "Restore it with:" >&2
    echo "  git -C $STYLO checkout -- $VENDOR_REL $LIB_REL" >&2
  fi
}
trap on_exit EXIT

# ----------------------------------------------------------------------------
# Grammar baseline (best effort, before anything is modified).  After the
# upgrade, tools/diff-grammar.sh should show this same diff again, modulo
# genuinely new upstream syntax.

echo "Capturing grammar-diff baseline (may take a while; runs dune build)..."
if (cd "$STYLO" \
    && tools/diff-grammar.sh > "$REPORT/grammar-diff-before.txt" \
                            2> "$REPORT/grammar-baseline.log"); then
  echo "  saved to $REPORT/grammar-diff-before.txt"
else
  echo "  WARNING: baseline capture failed (see $REPORT/grammar-baseline.log)."
  echo "  The tree is still clean, so you can regenerate the baseline from"
  echo "  git history later if you need it."
fi

# ----------------------------------------------------------------------------
# Phase 1: extract old (merge ancestor) and new (merge theirs) versions of
# every tracked file from the oxcaml history.  Nothing in the stylo tree has
# been modified yet, so a stale TRACKED list fails cleanly here.

echo "Extracting upstream files at old ${OLD:0:12} and new ${NEW:0:12}..."
for path in "${TRACKED[@]}"; do
  base=$(basename "$path")
  git -C "$OXCAML" show "$OLD:$path" > "$TMP/old/$base" 2>/dev/null \
    || die "$path does not exist at $OLD; if upstream moved it, update the \
TRACKED list in $0"
  git -C "$OXCAML" show "$NEW:$path" > "$TMP/new/$base" 2>/dev/null \
    || die "$path does not exist at ${NEW:0:12}; if upstream moved or \
deleted it, update the TRACKED list in $0"
done

# ----------------------------------------------------------------------------
# Phase 2: three-way merges.
#
# For each tracked file, merge (ancestor: old upstream) -> (theirs: new
# upstream) into the vendor copy, and -- for the LIB_MERGE subset -- into the
# lib/parsing copy.  A pristine vendor copy makes the merge degenerate to a
# plain copy of the new upstream file; local edits are preserved, or turned
# into conflicts when upstream touched the same lines.

CONFLICTS=()

merge_into() {
  local target_rel=$1 base_blob=$2 new_blob=$3 target rc n
  target=$STYLO/$target_rel
  [ -f "$target" ] || die "missing $target_rel (file lists out of date?)"
  rc=0
  git merge-file --diff3 \
      -L "$target_rel (stylo)" \
      -L "${OLD:0:12} (old oxcaml)" \
      -L "${NEW:0:12} (new oxcaml)" \
      "$target" "$base_blob" "$new_blob" || rc=$?
  # merge-file exits with the number of conflicts (truncated to 127 if there
  # are more), or 255 on real errors.
  [ "$rc" -le 127 ] || die "git merge-file failed on $target_rel"
  if [ "$rc" -gt 0 ]; then
    n=$rc
    if [ "$rc" -eq 127 ]; then n="127+"; fi
    CONFLICTS+=("$(printf '%4s  %s' "$n" "$target_rel")")
  fi
}

echo "Merging..."
MODIFIED=1
for path in "${TRACKED[@]}"; do
  base=$(basename "$path")
  merge_into "$VENDOR_REL/$base" "$TMP/old/$base" "$TMP/new/$base"
  for f in "${LIB_MERGE[@]}"; do
    if [ "$f" = "$base" ]; then
      merge_into "$LIB_REL/$base" "$TMP/old/$base" "$TMP/new/$base"
    fi
  done
done

git -C "$OXCAML" rev-parse "$NEW" > "$STYLO/$VENDOR_REL/upstream-commit.txt"

# ----------------------------------------------------------------------------
# Phase 3: report.

echo
echo "=============================================================================="
echo "repatch: imported oxcaml ${OLD:0:12} -> ${NEW:0:12}"
echo

if [ "${#CONFLICTS[@]}" -eq 0 ]; then
  echo "No conflicts; all files merged cleanly."
else
  echo "Conflicts to resolve (count, file):"
  printf '%s\n' "${CONFLICTS[@]}"
fi

added_deleted=$(git -C "$OXCAML" diff --name-status --diff-filter=ADR \
  "$OLD" "$NEW" -- parsing/ \
  utils/language_extension_kernel.ml utils/language_extension_kernel.mli \
  utils/terminfo.ml utils/terminfo.mli utils/warnings.ml utils/warnings.mli)
if [ -n "$added_deleted" ]; then
  echo
  echo "Upstream added/deleted/renamed frontend files since the last import"
  echo "(consider updating the TRACKED list in $0):"
  echo "$added_deleted" | sed 's/^/  /'
fi

skipped=$(git -C "$OXCAML" diff --stat "$OLD" "$NEW" -- "${LIB_SKIPPED[@]}")
if [ -n "$skipped" ]; then
  echo
  echo "Upstream changed files whose $LIB_REL namesakes have diverged too far"
  echo "to merge. These were merged into $VENDOR_REL only; review by hand if"
  echo "relevant:"
  echo "$skipped" | sed 's/^/  /'
fi

echo
echo "Next steps (see $VENDOR_REL/README.md for guidance):"
echo "  1. resolve conflicts:  grep -rn '^<<<<<<<' $VENDOR_REL $LIB_REL"
echo "  2. dune build"
echo "  3. tools/diff-grammar.sh"
echo "     and compare with the baseline: $REPORT/grammar-diff-before.txt"
echo "  4. dune runtest    (dune promote to accept legitimate changes)"
