#!/bin/bash

BIN=./_build/default/bin

src="$1"
shift

name=$(basename "$src")
fst="/tmp/fst_$name"
snd="/tmp/snd_$name"

$BIN/main.exe style "$@" "$src" > "$fst"
$BIN/main.exe style "$@" "$fst" > "$snd"

git diff --no-index --no-ext-diff "$fst" "$snd"

rm "$fst"
rm "$snd"
