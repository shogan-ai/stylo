#!/bin/bash

rlwrap ocamldebug \
    -I _build/default/bin/.main.eobjs/byte \
    -I _build/default/lib/ast-checker/.ast_checker.objs/byte \
    -I _build/default/lib/comments/.comments.objs/byte \
    -I _build/default/lib/config/.config.objs/byte \
    -I _build/default/lib/dbg_print/.dbg_print.objs/byte \
    -I _build/default/lib/normalize/erase-jane-syntax/.erase_jane_syntax.objs/byte \
    -I _build/default/lib/normalize/.normalize.objs/byte \
    -I _build/default/lib/parsing/.ocaml_syntax.objs/byte \
    -I _build/default/lib/printing/document/.document.objs/byte \
    -I _build/default/lib/printing/.print.objs/byte \
    -I _build/default/lib/.std.objs/byte \
    -I _build/default/lib/.stylo.objs/byte \
    -I _build/default/lib/traversals/.gen.eobjs/byte \
    -I _build/default/lib/traversals/.traversals_helpers.objs/byte \
    -I _build/default/vendor/jst-odoc-parser/.jst_odoc_parser.objs/byte \
    -I _build/default/vendor/oxcaml-frontend/.oxcaml_frontend.objs/byte \
    ./_build/default/bin/main.bc style $@
