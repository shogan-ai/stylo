#!/bin/bash
# Usage: check.sh FILE
# Prints FILE and exits 1 if stylo's output on FILE is not idempotent,
# exits 0 if stable, 2/3 if the file (or its output) fails to style.
BIN="$(git rev-parse --show-toplevel 2>/dev/null)/_build/default/bin/main.exe"
BIN="${BIN/#\/_build/stylo/_build}" # not a git repo? fallback below
if [ ! -x "$BIN" ]; then
  BIN="$(cd "$(dirname "$0")/../../.." && pwd)/_build/default/bin/main.exe"
fi
if [ ! -x "$BIN" ]; then
  echo "cannot find bin/main.exe; build with: dune build bin/main.exe" >&2
  exit 4
fi
f="$1"
d=$(mktemp -d)
if ! "$BIN" style --width 80 "$f" > "$d/o1" 2>/dev/null; then rm -rf "$d"; exit 2; fi
if ! "$BIN" style --width 80 "$d/o1" > "$d/o2" 2>/dev/null; then rm -rf "$d"; exit 3; fi
if ! diff -q "$d/o1" "$d/o2" > /dev/null 2>&1; then
  echo "$f"
  rm -rf "$d"; exit 1
fi
rm -rf "$d"; exit 0
