Base command (avoiding constructions that trigger semantic checks):

```
ocamlgrammarfuzzer --check stylo --check-command _build/default/bin/main.exe \
  --comments-randomize-space --comments-randomize-count --comments-randomize-length \
  --avoid METAOCAML_ESCAPE --avoid METAOCAML_BRACKET_OPEN --avoid EFFECT \
  --avoid '_* TYPE _* NONREC _* PLUSEQ _*' --avoid '_* TYPE _* NONREC _* COLONEQUAL _*' \
  --avoid 'pattern: EXCEPTION _*'
```

Check basic coverage of grammar:

```
base-command --exhaust > report.md
```

Check handling of comments:

```
base-command --exhaust --comments > report.md
```

Check on random contents:

```
base-command --count 1000 > report.md
```

Check OxCaml grammar: add `--oxcaml` to previous commands.
Reduce examples before reporting: add `--reduce`

Check specific constructions:

```
base-command --focus '_* LPAREN _* RPAREN _*'
```
