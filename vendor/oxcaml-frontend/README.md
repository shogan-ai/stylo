# Upgrade workflow

## Upgrading the vendored frontend

- Pull oxcaml.
- Get targeted log/diff since the last sync:
  ```
  git log $(cat $STYLO/vendor/oxcaml-frontend/upstream-commit.txt)..HEAD -- \
    parsing/{parsetree.mli,parser.mly,lexer.mll}
  ```
  Getting the log from oxcaml gives more context than just diffing after having
  reimported the files.
- report upstream changes to stylo's `vendor/` directory.
  The best method (whole file `cp`, `patch`, manual edits, ...) is left to the
  appreciation of the updater although the following often works well enough
  ```
  cd $OXCAML
  git diff -p $(cat $STYLO/vendor/oxcaml-frontend/upstream-commit.txt)..HEAD \
    -- parsing/{parsetree.mli,parser.mly,lexer.mll,ast_helper.*} \
    > /tmp/parsing.patch
  cd $STYLO
  patch -d vendor/oxcaml-frontend -p2 < /tmp/parsing.patch
  ```

## Upgrading stylo

### AST vs CST

Changes to the parsetree can sometimes be imported directly, but will most
likely need some tweaking: `lib/parsing/parsetree.mli` defines a CST, there are
more nodes and fields than in `vendor/oxcaml-frontend/parsetree.mli`.

If we oversimplify things a bit, we can assume there are only two kinds of
changes to the AST:
1. a new constructor is added to an already defined syntactic category, e.g. the
   addition of unboxed tuple patterns `Ppat_unboxed_tuple of ...`
2. a new syntactic category is defined, e.g . list/array comprehensions

(1) can usually be imported as is, as long as the concrete syntax can be
unambiguously reconstructed from the information in the parsetree.
There have historically been a few cases where that was not directly possible,
for instance the `Pexp_open` could be obtained from two distinct concrete forms:
`let open M in ...` and `M.(...)`, in that case the AST constructor was replaced
by two CST contructors `Pexp_let_open` and `Pexp_dot_open`.

(2) will pretty much always require some changes, but they are fairly
regular/mechanical. Mainly, one needs to make sure that these new CST nodes
carry with them the tokens used for their construction (cf. `HACKING.md`).
Which roughly means:
- making sure the record contains a `pwhatever_tokens` field
- calling `Tokens.at <some loc>` from the parser when constructing values of
  that type
- updating `lib/parsing/tokens_of_tree.ml` to visit the new nodes when
  extracting tokens from the CST.

### Parser changes

`lib/parsing/parser.mly` is probably the place where most manual interventions
will happen.

Roughly, you want to import verbatim the changes to the grammar itself but adapt
the content of the semantic actions.

`tools/diff-grammar.sh` can be used to produce a (mostly) noise-free diff of
stylo's grammar with the one in `vendor/oxcaml-frontend`. A good approximation
your goal during an update is that this diff should be the same before you start
the upgrade, and after you're done upgrading everything.
