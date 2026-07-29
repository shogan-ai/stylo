This directory contains a vendored subset of the oxcaml compiler frontend, built
as the `oxcaml_frontend` library. Stylo does not use it at runtime; it exists as

- the grammar reference that `tools/diff-grammar.sh` compares stylo's parser
  against, and
- the input from which dune generates `ast_mapper.ml` (via
  `lib/traversals/gen.exe`).

`upstream-commit.txt` records the oxcaml commit the files were last imported
from.

Most files here track their upstream counterparts (from oxcaml's `parsing/` and
`utils/` directories), possibly with small local edits needed to build
standalone. Three files are hand-maintained shims, not upstream copies:
`clflags.ml`, `misc.ml` and `pprintast.ml` are fake or heavily trimmed stand-ins
carrying just enough definitions for the library to build.

## Upgrading

```
./vendor/oxcaml-frontend/repatch.sh <path-to-oxcaml-checkout> [<rev>]
```

`<rev>` defaults to `HEAD`. Prerequisites:

- no uncommitted changes under `vendor/oxcaml-frontend/` or `lib/parsing/`
- the oxcaml checkout must contain both `<rev>` and the commit in
  `upstream-commit.txt`
- a `dune` that works in this project: the script puts `./_opam/bin` first on
  `PATH` if present, and aborts if the `dune` it finds still doesn't run
  (e.g. it found only jane street's internal version of dune)

The script:

1. captures a baseline of `tools/diff-grammar.sh` (the acceptable
   vendor-vs-stylo grammar delta before the upgrade);
2. three-way-merges each tracked vendor file: ancestor = the file at the commit
   in `upstream-commit.txt`, theirs = the file at `<rev>`, ours = the vendor
   copy.
3. runs the same merge into the `lib/parsing/` files that are CST-adapted copies
   of vendor files (`parser.mly`, `parsetree.mli`, `lexer.mll`, ...), so the
   upstream delta lands there with conflict markers wherever it collides with
   the CST adaptations;
4. updates `upstream-commit.txt` and prints a per-file conflict summary, plus
   notes about upstream file additions/deletions and about upstream changes to
   files stylo has rewritten (see below).

Tips:

- Read the upstream log before you start; it gives far more context than the
  merged diff alone:
  ```
  git -C $OXCAML log $(cat vendor/oxcaml-frontend/upstream-commit.txt)..<rev> -- parsing/
  ```
- After a long gap, upgrade in steps (`repatch.sh $OXCAML <intermediate-rev>`
  repeatedly, resolving as you go) to keep the conflict volume manageable.

## Resolving conflicts in vendor/

Almost always take the upstream side. The only legitimate local content is the
minimal edits needed for this library to build on its own (it links against only
`menhirLib` and stylo's `config`). If the new code needs more from `Misc`,
`Clflags` or `Pprintast`, extend those shims by hand.

## Resolving conflicts in lib/parsing/

`lib/parsing/` defines a CST, not the compiler's AST: nodes carry extra
constructors and fields to keep everything the compiler discards (parentheses,
tokens, ...; see `HACKING.md`). The merge deliberately leaves you conflicts
exactly where upstream changed something stylo had to adapt. But these may not
be the only things you need to change - new upstream additions may merge cleanly
into the parser, but their actions will need to be updated to produce the CST
rather than the AST.

**Files the script does not merge into lib/parsing**: `ast_helper.*`,
`location.*`, `longident.*` and `parse.ml` share names with upstream files but
have diverged in ways that make a line-level merge useless. The script prints a
note when upstream changed these files; apply it manually. In particular,
whatever you decided for a `parsetree.mli` change usually needs mirroring in
`lib/parsing/ast_helper.*` by hand.

## Verifying the upgrade

Build and run tests. You will probably have to update the styling code, and will
want to add tests for any new language constructs you are supporting.

## Manual fallback

The script is just a loop around `git merge-file`; the same operation for a
single file is:

```
old=$(cat vendor/oxcaml-frontend/upstream-commit.txt)
git -C $OXCAML show $old:parsing/parser.mly > /tmp/parser-old.mly
git -C $OXCAML show <rev>:parsing/parser.mly > /tmp/parser-new.mly
git merge-file --diff3 lib/parsing/parser.mly /tmp/parser-old.mly /tmp/parser-new.mly
```

The list of tracked files and their upstream paths lives at the top of
`repatch.sh`.
