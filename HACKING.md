# The design

Stylo formats OCaml code by
1. parsing its input into a Concrete Syntax Tree.
   Which is roughly the compiler's Parsetree, with extra constructors to keep
   all the information the compiler discards (e.g. the presence of parentheses)
2. pretty printing this CST.
   A good introduction to the design of the pretty printing engine used by stylo is [Wadler's paper](https://homepages.inf.ed.ac.uk/wadler/papers/prettier/prettier.pdf).
   N.B. OCaml's standard implementation is of this model [PPrint](https://github.com/fpottier/pprint).

## Pretty printing comments

Comments in OCaml can appear in between two tokens, and are usually not stored
in the AST.

In stylo we leverage the algebraic/concatenative structure of the documents
built by the pretty printer to handle comments.

A key observation is that the leaves of a our documents are either a whitespace
or a string corresponding to a token in the source.
So, if we were to do a left-handed DFS traversal of our document, collecting the
non-whitespace leaves, we'd end up with a list matching the list of tokens as
produced by the lexer.

So in stylo, we build a document using only the CST, without any comments. And
then we traverse that document in sync with the list of tokens (comments
included) as produced by the lexer, and simply insert new nodes in the document
whenever we encounter a comment in the token stream.

This has the nice property of keeping comments in the same relative position in
the output as in the input, that is between the same two tokens.

Further reading:
- the above only explains how to print comments, not how to "pretty" print.
  Refer to the comments in [`Comments.Insert`](./lib/comments/insert.ml)
  for details regarding spacing and layout.
- the above is simplistic and can lead to a combinatorial explosion in presence
  of PPrint's [`ifflat`](https://cambium.inria.fr/~fpottier/pprint/doc/pprint/PPrint/index.html#val-ifflat) operator.
  See the comments in [`lib/printing/document/`](./lib/printing/document/document.ml) for more details.

## Code style normalisation

By normalisation we mean: taking away the choice between the different but semantically equivalent syntaxes available to users, and consolidating around one of the options for each choice/context.

With an expressive/complete enough CST, normalisations should be expressible as
functions mapping a CST value to another.

However, given our handling of comments, stopping there would break the
correspondance between our document and the input's token stream (as indeed the
normalisation breaks the correspondance between the CST and the token stream).

To retain that correspondance, we require our CST mappers to also map the
underlying portion of the token stream (the relevant code is in
[`normalize`](./normalize)).
To facilitate that rewrite, the attaches to each CST node the list of tokens it
"consumed" to produce that particular node, and the tokens used to produce the
node's children are represented using [`Tokens.Child_node`](parsing/tokens.ml).

For comment's insertion purposes instead of using the input tokens, we just
flatten / stitch back together these token lists which we retrieve from the
(potentially rewritten) CST.
