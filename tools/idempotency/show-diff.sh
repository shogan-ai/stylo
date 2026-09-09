#!/bin/bash

BIN=./_build/default/bin

name=$(basename "$1")
fst="/tmp/fst_$name"
snd="/tmp/snd_$name"

$BIN/main.exe style "$1" > "$fst"
$BIN/main.exe style "$fst" > "$snd"

git diff --no-index --no-ext-diff "$fst" "$snd"

rm "$fst"
rm "$snd"
