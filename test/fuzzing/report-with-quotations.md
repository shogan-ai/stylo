## Input doesn't parse: (6 errors)

### Item `alias_type: . function_type` (in 4 errors)

- Derivation (1 occurrence):
  ```
  generic_type_declaration(no_nonrec_flag,type_subst_kind): TYPE ext nonempty_list(attribute) NONREC type_parameters LIDENT option(jkind_constraint) COLONEQUAL . nonempty_type_kind reversed_llist(preceded(CONSTRAINT,constrain))
    nonempty_type_kind: . core_type
      core_type: . alias_type
        alias_type: . function_type
          function_type: . tuple_type
            tuple_type: . atomic_type
              atomic_type: . delimited_type
                delimited_type: . delimited_type_supporting_local_open
                  delimited_type_supporting_local_open: . LESSLBRACKET core_type RBRACKETGREATER
  ```
  Sample sentence (implementation):
  ```ocaml
  [%% local_ : type [@ true ] nonrec x1 := <[ _ ]> ]
  ```
- Derivation (1 occurrence):
  ```
  generic_type_declaration(no_nonrec_flag,type_subst_kind): TYPE ext NONREC type_parameters LIDENT option(jkind_constraint) COLONEQUAL . nonempty_type_kind reversed_llist(preceded(CONSTRAINT,constrain))
    nonempty_type_kind: . core_type
      core_type: . alias_type
        alias_type: . function_type
          function_type: . tuple_type
            tuple_type: . atomic_type
              atomic_type: . delimited_type
                delimited_type: . delimited_type_supporting_local_open
                  delimited_type_supporting_local_open: . LESSLBRACKET core_type RBRACKETGREATER
  ```
  Sample sentence (implementation):
  ```ocaml
  [%% to : type nonrec x1 := <[ {%ext|s2|} ]> ]
  ```
- Derivation (1 occurrence):
  ```
  generic_type_declaration(no_nonrec_flag,type_subst_kind): TYPE ext nonempty_list(attribute) NONREC type_parameters LIDENT option(jkind_constraint) COLONEQUAL . nonempty_type_kind reversed_llist(preceded(CONSTRAINT,constrain))
    nonempty_type_kind: . core_type
      core_type: . alias_type
        alias_type: . function_type
          function_type: . tuple_type
            tuple_type: . atomic_type
              atomic_type: . DOLLAR spliceable_type
  ```
  Sample sentence (implementation):
  ```ocaml
  [%% if : type [@ x1 ] nonrec x2 := $ x3 ]
  ```
- Derivation (1 occurrence):
  ```
  generic_type_declaration(no_nonrec_flag,type_subst_kind): TYPE ext NONREC type_parameters LIDENT option(jkind_constraint) COLONEQUAL . nonempty_type_kind reversed_llist(preceded(CONSTRAINT,constrain))
    nonempty_type_kind: . core_type
      core_type: . alias_type
        alias_type: . function_type
          function_type: . tuple_type
            tuple_type: . atomic_type
              atomic_type: . DOLLAR spliceable_type
  ```
  Sample sentence (implementation):
  ```ocaml
  [%% layout_ : type nonrec x1 := $ x2 ]
  ```


### Item `block_access: . DOT ident LPAREN seq_expr RPAREN` (in 2 errors)

- Derivation (2 occurrences):
  ```
  simple_expr: LPAREN . block_access reversed_llist(unboxed_access) RPAREN
    block_access: . DOT ident LPAREN seq_expr RPAREN
  ```
  Sample sentence (implementation):
  ```ocaml
  ( . X1 ( $ x2 ) )
  ```


