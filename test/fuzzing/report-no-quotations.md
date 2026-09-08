## Error while parsing the output with upstream's parser: (17 errors)

### Item `fun_expr: . simple_expr` (in 12 errors)

- Derivation (6 occurrences):
  ```
  implementation: . structure EOF
    structure: . seq_expr list(structure_element)
      seq_expr: . fun_seq_expr
        fun_seq_expr: . fun_expr
          fun_expr: . simple_expr
            simple_expr: . OBJECT ext class_self_pattern list(text_cstr(class_field)) END
  ```
  Sample sentence (implementation):
  ```ocaml
  object ( false | false ) end
  ```
- Derivation (3 occurrences):
  ```
  reversed_labeled_tuple_body: TILDE LIDENT COMMA . fun_expr
    fun_expr: . simple_expr
      simple_expr: . constr_longident
        constr_longident: . mod_longident
          mod_longident: . mk_longident(mod_longident,str_not_op(UIDENT))
            mk_longident(mod_longident,str_not_op(UIDENT)): . UIDENT
  ```
  Sample sentence (implementation):
  ```ocaml
  - ~ x1 , X2 , ~label3: #'a'
  ```
- Derivation (2 occurrences):
  ```
  reversed_labeled_tuple_body: LABEL simple_expr COMMA . fun_expr
    fun_expr: . simple_expr
      simple_expr: . constr_longident
        constr_longident: . mod_longident
          mod_longident: . mk_longident(mod_longident,str_not_op(UIDENT))
            mk_longident(mod_longident,str_not_op(UIDENT)): . UIDENT
  ```
  Sample sentence (implementation):
  ```ocaml
  - ~label1: #'a' , X2
  ```
- Derivation (1 occurrence):
  ```
  implementation: . structure EOF
    structure: . seq_expr list(structure_element)
      seq_expr: . fun_seq_expr
        fun_seq_expr: . fun_expr
          fun_expr: . simple_expr
            simple_expr: . LPAREN seq_expr RPAREN
  ```
  Sample sentence (implementation):
  ```ocaml
  ( - ~ x1 , X2 , ~label3: #'a' )
  ```


### Item `labeled_tuple_pattern(pattern): . reversed_labeled_tuple_pattern(pattern)` (in 5 errors)

- Derivation (5 occurrences):
  ```
  pattern: pattern BAR . pattern
    pattern: . labeled_tuple_pattern(pattern)
      labeled_tuple_pattern(pattern): . reversed_labeled_tuple_pattern(pattern)
        reversed_labeled_tuple_pattern(pattern): . labeled_tuple_pat_element_list(pattern)
  ```
  Sample sentence (implementation):
  ```ocaml
  object ( false | false , false ) end
  ```


## Input doesn't parse: (5205 errors)

### Item `ident: . LIDENT` (in 4353 errors)

- Derivation (3326 occurrences):
  ```
  block_access: DOT . ident LPAREN seq_expr RPAREN
    ident: . LIDENT
  ```
  Sample sentence (implementation):
  ```ocaml
  ( . x1 ( X2 #'a' ) )
  ```
- Derivation (308 occurrences):
  ```
  reversed_nonempty_llist(typevar_repr): LPAREN REPR QUOTE . ident RPAREN
    ident: . LIDENT
  ```
  Sample sentence (interface):
  ```ocaml
  type nonrec x1 := ( ( repr_ ' x2 ) . {%ext|s3|} ) -> {%ext|s4|}
  ```
- Derivation (308 occurrences):
  ```
  reversed_nonempty_llist(typevar): QUOTE . ident
    ident: . LIDENT
  ```
  Sample sentence (interface):
  ```ocaml
  type nonrec x1 := ( ' x2 . {%ext|s3|} ) -> {%ext|s4|}
  ```
- Derivation (98 occurrences):
  ```
  alias_type: alias_type AS QUOTE . ident
    ident: . LIDENT
  ```
  Sample sentence (interface):
  ```ocaml
  type nonrec x1 := {%ext|s2|} * {%ext|s3|} as ' x4
  ```
- Derivation (8 occurrences):
  ```
  strict_function_or_labeled_tuple_type: nonempty_list(mode_legacy) LPAREN LAYOUT . reversed_nonempty_llist(mkrhs(ident)) DOT core_type RPAREN at_mode_expr MINUSGREATER nonempty_list(mode_legacy) tuple_type
    reversed_nonempty_llist(mkrhs(ident)): . ident
      ident: . LIDENT
  ```
  Sample sentence (interface):
  ```ocaml
  type nonrec x1 := local_ ( layout_ x2 . {%ext|s3|} ) @ x4 -> local_ {%ext|s5|}
  ```
- Derivation (8 occurrences):
  ```
  strict_function_or_labeled_tuple_type: nonempty_list(mode_legacy) LPAREN LAYOUT . reversed_nonempty_llist(mkrhs(ident)) DOT core_type RPAREN at_mode_expr MINUSGREATER tuple_type
    reversed_nonempty_llist(mkrhs(ident)): . ident
      ident: . LIDENT
  ```
  Sample sentence (interface):
  ```ocaml
  type nonrec x1 := local_ ( layout_ x2 . {%ext|s3|} ) @ x4 -> {%ext|s5|}
  ```
- Derivation (8 occurrences):
  ```
  strict_function_or_labeled_tuple_type: nonempty_list(mode_legacy) LPAREN LAYOUT . reversed_nonempty_llist(mkrhs(ident)) DOT core_type RPAREN MINUSGREATER nonempty_list(mode_legacy) tuple_type
    reversed_nonempty_llist(mkrhs(ident)): . ident
      ident: . LIDENT
  ```
  Sample sentence (interface):
  ```ocaml
  type nonrec x1 := local_ ( layout_ x2 . {%ext|s3|} ) -> local_ {%ext|s4|}
  ```
- Derivation (8 occurrences):
  ```
  strict_function_or_labeled_tuple_type: nonempty_list(mode_legacy) LPAREN LAYOUT . reversed_nonempty_llist(mkrhs(ident)) DOT core_type RPAREN MINUSGREATER tuple_type
    reversed_nonempty_llist(mkrhs(ident)): . ident
      ident: . LIDENT
  ```
  Sample sentence (interface):
  ```ocaml
  type nonrec x1 := local_ ( layout_ x2 . {%ext|s3|} ) -> {%ext|s4|}
  ```
- Derivation (8 occurrences):
  ```
  strict_function_or_labeled_tuple_type: LIDENT COLON nonempty_list(mode_legacy) LPAREN LAYOUT . reversed_nonempty_llist(mkrhs(ident)) DOT core_type RPAREN at_mode_expr MINUSGREATER nonempty_list(mode_legacy) tuple_type
    reversed_nonempty_llist(mkrhs(ident)): . ident
      ident: . LIDENT
  ```
  Sample sentence (interface):
  ```ocaml
  type nonrec x1 := x2 : local_ ( layout_ x3 . {%ext|s4|} ) @ x5 -> local_ {%ext|s6|}
  ```
- Derivation (8 occurrences):
  ```
  strict_function_or_labeled_tuple_type: LIDENT COLON nonempty_list(mode_legacy) LPAREN LAYOUT . reversed_nonempty_llist(mkrhs(ident)) DOT core_type RPAREN at_mode_expr MINUSGREATER tuple_type
    reversed_nonempty_llist(mkrhs(ident)): . ident
      ident: . LIDENT
  ```
  Sample sentence (interface):
  ```ocaml
  type nonrec x1 := x2 : local_ ( layout_ x3 . {%ext|s4|} ) @ x5 -> {%ext|s6|}
  ```
- Derivation (8 occurrences):
  ```
  strict_function_or_labeled_tuple_type: LIDENT COLON nonempty_list(mode_legacy) LPAREN LAYOUT . reversed_nonempty_llist(mkrhs(ident)) DOT core_type RPAREN MINUSGREATER nonempty_list(mode_legacy) tuple_type
    reversed_nonempty_llist(mkrhs(ident)): . ident
      ident: . LIDENT
  ```
  Sample sentence (interface):
  ```ocaml
  type nonrec x1 := x2 : local_ ( layout_ x3 . {%ext|s4|} ) -> local_ {%ext|s5|}
  ```
- Derivation (8 occurrences):
  ```
  strict_function_or_labeled_tuple_type: LIDENT COLON nonempty_list(mode_legacy) LPAREN LAYOUT . reversed_nonempty_llist(mkrhs(ident)) DOT core_type RPAREN MINUSGREATER tuple_type
    reversed_nonempty_llist(mkrhs(ident)): . ident
      ident: . LIDENT
  ```
  Sample sentence (interface):
  ```ocaml
  type nonrec x1 := x2 : local_ ( layout_ x3 . {%ext|s4|} ) -> {%ext|s5|}
  ```
- Derivation (8 occurrences):
  ```
  strict_function_or_labeled_tuple_type: LIDENT COLON LPAREN LAYOUT . reversed_nonempty_llist(mkrhs(ident)) DOT core_type RPAREN at_mode_expr MINUSGREATER nonempty_list(mode_legacy) tuple_type
    reversed_nonempty_llist(mkrhs(ident)): . ident
      ident: . LIDENT
  ```
  Sample sentence (interface):
  ```ocaml
  type nonrec x1 := x2 : ( layout_ x3 . {%ext|s4|} ) @ x5 -> local_ {%ext|s6|}
  ```
- Derivation (8 occurrences):
  ```
  strict_function_or_labeled_tuple_type: LIDENT COLON LPAREN LAYOUT . reversed_nonempty_llist(mkrhs(ident)) DOT core_type RPAREN at_mode_expr MINUSGREATER tuple_type
    reversed_nonempty_llist(mkrhs(ident)): . ident
      ident: . LIDENT
  ```
  Sample sentence (interface):
  ```ocaml
  type nonrec x1 := x2 : ( layout_ x3 . {%ext|s4|} ) @ x5 -> {%ext|s6|}
  ```
- Derivation (8 occurrences):
  ```
  strict_function_or_labeled_tuple_type: LIDENT COLON LPAREN LAYOUT . reversed_nonempty_llist(mkrhs(ident)) DOT core_type RPAREN MINUSGREATER nonempty_list(mode_legacy) tuple_type
    reversed_nonempty_llist(mkrhs(ident)): . ident
      ident: . LIDENT
  ```
  Sample sentence (interface):
  ```ocaml
  type nonrec x1 := x2 : ( layout_ x3 . {%ext|s4|} ) -> local_ {%ext|s5|}
  ```
- Derivation (8 occurrences):
  ```
  strict_function_or_labeled_tuple_type: LIDENT COLON LPAREN LAYOUT . reversed_nonempty_llist(mkrhs(ident)) DOT core_type RPAREN MINUSGREATER tuple_type
    reversed_nonempty_llist(mkrhs(ident)): . ident
      ident: . LIDENT
  ```
  Sample sentence (interface):
  ```ocaml
  type nonrec x1 := x2 : ( layout_ x3 . {%ext|s4|} ) -> {%ext|s5|}
  ```
- Derivation (8 occurrences):
  ```
  strict_function_or_labeled_tuple_type: optlabel nonempty_list(mode_legacy) LPAREN LAYOUT . reversed_nonempty_llist(mkrhs(ident)) DOT core_type RPAREN at_mode_expr MINUSGREATER nonempty_list(mode_legacy) tuple_type
    reversed_nonempty_llist(mkrhs(ident)): . ident
      ident: . LIDENT
  ```
  Sample sentence (interface):
  ```ocaml
  type nonrec x1 := ?label: local_ ( layout_ x2 . {%ext|s3|} ) @ x4 -> local_ {%ext|s5|}
  ```
- Derivation (8 occurrences):
  ```
  strict_function_or_labeled_tuple_type: optlabel nonempty_list(mode_legacy) LPAREN LAYOUT . reversed_nonempty_llist(mkrhs(ident)) DOT core_type RPAREN at_mode_expr MINUSGREATER tuple_type
    reversed_nonempty_llist(mkrhs(ident)): . ident
      ident: . LIDENT
  ```
  Sample sentence (interface):
  ```ocaml
  type nonrec x1 := ?label: local_ ( layout_ x2 . {%ext|s3|} ) @ x4 -> {%ext|s5|}
  ```
- Derivation (8 occurrences):
  ```
  strict_function_or_labeled_tuple_type: optlabel nonempty_list(mode_legacy) LPAREN LAYOUT . reversed_nonempty_llist(mkrhs(ident)) DOT core_type RPAREN MINUSGREATER nonempty_list(mode_legacy) tuple_type
    reversed_nonempty_llist(mkrhs(ident)): . ident
      ident: . LIDENT
  ```
  Sample sentence (interface):
  ```ocaml
  type nonrec x1 := ?label: local_ ( layout_ x2 . {%ext|s3|} ) -> local_ {%ext|s4|}
  ```
- Derivation (8 occurrences):
  ```
  strict_function_or_labeled_tuple_type: optlabel nonempty_list(mode_legacy) LPAREN LAYOUT . reversed_nonempty_llist(mkrhs(ident)) DOT core_type RPAREN MINUSGREATER tuple_type
    reversed_nonempty_llist(mkrhs(ident)): . ident
      ident: . LIDENT
  ```
  Sample sentence (interface):
  ```ocaml
  type nonrec x1 := ?label: local_ ( layout_ x2 . {%ext|s3|} ) -> {%ext|s4|}
  ```
- Derivation (8 occurrences):
  ```
  strict_function_or_labeled_tuple_type: optlabel LPAREN LAYOUT . reversed_nonempty_llist(mkrhs(ident)) DOT core_type RPAREN at_mode_expr MINUSGREATER nonempty_list(mode_legacy) tuple_type
    reversed_nonempty_llist(mkrhs(ident)): . ident
      ident: . LIDENT
  ```
  Sample sentence (interface):
  ```ocaml
  type nonrec x1 := ?label: ( layout_ x2 . {%ext|s3|} ) @ x4 -> local_ {%ext|s5|}
  ```
- ...


### Item `alias_type: . function_type` (in 637 errors)

- Derivation (79 occurrences):
  ```
  simple_expr: LPAREN seq_expr COLON . core_type COLONGREATER tuple_type at_mode_expr RPAREN
    core_type: . alias_type
      alias_type: . function_type
        function_type: . tuple_type
          tuple_type: . atomic_type
            atomic_type: . delimited_type
              delimited_type: . extension_type
                extension_type: . extension
                  extension: . QUOTED_STRING_EXPR
  ```
  Sample sentence (implementation):
  ```ocaml
  ( ( X1 : {%ext|s2|} :> {%ext|s3|} @ x4 ) )
  ```
- Derivation (8 occurrences):
  ```
  type_constraint: COLONGREATER . core_type
    core_type: . alias_type
      alias_type: . function_type
        function_type: . tuple_type
          tuple_type: . atomic_type
            atomic_type: . delimited_type
              delimited_type: . extension_type
                extension_type: . extension
                  extension: . QUOTED_STRING_EXPR
  ```
  Sample sentence (implementation):
  ```ocaml
  object val x1 :> {%ext|s2|} = ( X3 :> {%ext|s4|} @ x5 ) end
  ```
- Derivation (6 occurrences):
  ```
  reversed_llist(preceded(CONSTRAINT,constrain)): reversed_llist(preceded(CONSTRAINT,constrain)) CONSTRAINT . core_type EQUAL core_type
    core_type: . alias_type
      alias_type: . function_type
        function_type: . tuple_type
          tuple_type: . atomic_type
            atomic_type: . delimited_type
              delimited_type: . extension_type
                extension_type: . extension
                  extension: . QUOTED_STRING_EXPR
  ```
  Sample sentence (interface):
  ```ocaml
  ;; type nonrec x1 := false constraint {%ext|s2|} = {%ext|s3|}
  ```
- Derivation (3 occurrences):
  ```
  generic_type_declaration(no_nonrec_flag,type_subst_kind): TYPE ext nonempty_list(attribute) NONREC type_parameters LIDENT option(jkind_constraint) COLONEQUAL . nonempty_type_kind reversed_llist(preceded(CONSTRAINT,constrain))
    nonempty_type_kind: . core_type EQUAL PRIVATE constructor_declarations
      core_type: . alias_type
        alias_type: . function_type
          function_type: . tuple_type
            tuple_type: . atomic_type
              atomic_type: . delimited_type
                delimited_type: . extension_type
                  extension_type: . extension
                    extension: . QUOTED_STRING_EXPR
  ```
  Sample sentence (interface):
  ```ocaml
  type [@ and ] nonrec x1 := {%ext|s2|} = private false
  ```
- Derivation (3 occurrences):
  ```
  generic_type_declaration(no_nonrec_flag,type_subst_kind): TYPE ext NONREC type_parameters LIDENT option(jkind_constraint) COLONEQUAL . nonempty_type_kind reversed_llist(preceded(CONSTRAINT,constrain))
    nonempty_type_kind: . core_type EQUAL PRIVATE constructor_declarations
      core_type: . alias_type
        alias_type: . function_type
          function_type: . tuple_type
            tuple_type: . atomic_type
              atomic_type: . delimited_type
                delimited_type: . extension_type
                  extension_type: . extension
                    extension: . QUOTED_STRING_EXPR
  ```
  Sample sentence (interface):
  ```ocaml
  type nonrec x1 := {%ext|s2|} = private false
  ```
- Derivation (3 occurrences):
  ```
  generic_type_declaration(no_nonrec_flag,type_subst_kind): TYPE ext nonempty_list(attribute) NONREC type_parameters LIDENT option(jkind_constraint) COLONEQUAL . nonempty_type_kind reversed_llist(preceded(CONSTRAINT,constrain))
    nonempty_type_kind: . core_type EQUAL constructor_declarations
      core_type: . alias_type
        alias_type: . function_type
          function_type: . tuple_type
            tuple_type: . atomic_type
              atomic_type: . delimited_type
                delimited_type: . extension_type
                  extension_type: . extension
                    extension: . QUOTED_STRING_EXPR
  ```
  Sample sentence (interface):
  ```ocaml
  type [@ and ] nonrec x1 := {%ext|s2|} = false
  ```
- Derivation (3 occurrences):
  ```
  generic_type_declaration(no_nonrec_flag,type_subst_kind): TYPE ext NONREC type_parameters LIDENT option(jkind_constraint) COLONEQUAL . nonempty_type_kind reversed_llist(preceded(CONSTRAINT,constrain))
    nonempty_type_kind: . core_type EQUAL constructor_declarations
      core_type: . alias_type
        alias_type: . function_type
          function_type: . tuple_type
            tuple_type: . atomic_type
              atomic_type: . delimited_type
                delimited_type: . extension_type
                  extension_type: . extension
                    extension: . QUOTED_STRING_EXPR
  ```
  Sample sentence (interface):
  ```ocaml
  type nonrec x1 := {%ext|s2|} = false
  ```
- Derivation (2 occurrences):
  ```
  generic_type_declaration(no_nonrec_flag,type_subst_kind): TYPE ext nonempty_list(attribute) NONREC type_parameters LIDENT option(jkind_constraint) COLONEQUAL . nonempty_type_kind reversed_llist(preceded(CONSTRAINT,constrain))
    nonempty_type_kind: . core_type
      core_type: . alias_type
        alias_type: . function_type
          function_type: . strict_function_or_labeled_tuple_type
            strict_function_or_labeled_tuple_type: . LIDENT COLON nonempty_list(mode_legacy) atomic_type STAR reversed_separated_nonempty_llist(STAR,labeled_tuple_typ_element) at_mode_expr MINUSGREATER nonempty_list(mode_legacy) tuple_type
  ```
  Sample sentence (interface):
  ```ocaml
  type [@ and ] nonrec x1 := x2 : local_ {%ext|s3|} * {%ext|s4|} @ x5 -> local_ {%ext|s6|}
  ```
- Derivation (2 occurrences):
  ```
  generic_type_declaration(no_nonrec_flag,type_subst_kind): TYPE ext NONREC type_parameters LIDENT option(jkind_constraint) COLONEQUAL . nonempty_type_kind reversed_llist(preceded(CONSTRAINT,constrain))
    nonempty_type_kind: . core_type
      core_type: . alias_type
        alias_type: . function_type
          function_type: . strict_function_or_labeled_tuple_type
            strict_function_or_labeled_tuple_type: . LIDENT COLON nonempty_list(mode_legacy) atomic_type STAR reversed_separated_nonempty_llist(STAR,labeled_tuple_typ_element) at_mode_expr MINUSGREATER nonempty_list(mode_legacy) tuple_type
  ```
  Sample sentence (interface):
  ```ocaml
  type nonrec x1 := x2 : local_ {%ext|s3|} * {%ext|s4|} @ x5 -> local_ {%ext|s6|}
  ```
- Derivation (2 occurrences):
  ```
  generic_type_declaration(no_nonrec_flag,type_subst_kind): TYPE ext nonempty_list(attribute) NONREC type_parameters LIDENT option(jkind_constraint) COLONEQUAL . nonempty_type_kind reversed_llist(preceded(CONSTRAINT,constrain))
    nonempty_type_kind: . core_type
      core_type: . alias_type
        alias_type: . function_type
          function_type: . strict_function_or_labeled_tuple_type
            strict_function_or_labeled_tuple_type: . LIDENT COLON nonempty_list(mode_legacy) atomic_type STAR reversed_separated_nonempty_llist(STAR,labeled_tuple_typ_element) at_mode_expr MINUSGREATER tuple_type
  ```
  Sample sentence (interface):
  ```ocaml
  type [@ and ] nonrec x1 := x2 : local_ {%ext|s3|} * {%ext|s4|} @ x5 -> {%ext|s6|}
  ```
- Derivation (2 occurrences):
  ```
  generic_type_declaration(no_nonrec_flag,type_subst_kind): TYPE ext NONREC type_parameters LIDENT option(jkind_constraint) COLONEQUAL . nonempty_type_kind reversed_llist(preceded(CONSTRAINT,constrain))
    nonempty_type_kind: . core_type
      core_type: . alias_type
        alias_type: . function_type
          function_type: . strict_function_or_labeled_tuple_type
            strict_function_or_labeled_tuple_type: . LIDENT COLON nonempty_list(mode_legacy) atomic_type STAR reversed_separated_nonempty_llist(STAR,labeled_tuple_typ_element) at_mode_expr MINUSGREATER tuple_type
  ```
  Sample sentence (interface):
  ```ocaml
  type nonrec x1 := x2 : local_ {%ext|s3|} * {%ext|s4|} @ x5 -> {%ext|s6|}
  ```
- Derivation (2 occurrences):
  ```
  generic_type_declaration(no_nonrec_flag,type_subst_kind): TYPE ext nonempty_list(attribute) NONREC type_parameters LIDENT option(jkind_constraint) COLONEQUAL . nonempty_type_kind reversed_llist(preceded(CONSTRAINT,constrain))
    nonempty_type_kind: . core_type
      core_type: . alias_type
        alias_type: . function_type
          function_type: . strict_function_or_labeled_tuple_type
            strict_function_or_labeled_tuple_type: . LIDENT COLON nonempty_list(mode_legacy) atomic_type STAR reversed_separated_nonempty_llist(STAR,labeled_tuple_typ_element) MINUSGREATER nonempty_list(mode_legacy) tuple_type
  ```
  Sample sentence (interface):
  ```ocaml
  type [@ and ] nonrec x1 := x2 : local_ {%ext|s3|} * {%ext|s4|} -> local_ {%ext|s5|}
  ```
- Derivation (2 occurrences):
  ```
  generic_type_declaration(no_nonrec_flag,type_subst_kind): TYPE ext NONREC type_parameters LIDENT option(jkind_constraint) COLONEQUAL . nonempty_type_kind reversed_llist(preceded(CONSTRAINT,constrain))
    nonempty_type_kind: . core_type
      core_type: . alias_type
        alias_type: . function_type
          function_type: . strict_function_or_labeled_tuple_type
            strict_function_or_labeled_tuple_type: . LIDENT COLON nonempty_list(mode_legacy) atomic_type STAR reversed_separated_nonempty_llist(STAR,labeled_tuple_typ_element) MINUSGREATER nonempty_list(mode_legacy) tuple_type
  ```
  Sample sentence (interface):
  ```ocaml
  type nonrec x1 := x2 : local_ {%ext|s3|} * {%ext|s4|} -> local_ {%ext|s5|}
  ```
- Derivation (2 occurrences):
  ```
  generic_type_declaration(no_nonrec_flag,type_subst_kind): TYPE ext nonempty_list(attribute) NONREC type_parameters LIDENT option(jkind_constraint) COLONEQUAL . nonempty_type_kind reversed_llist(preceded(CONSTRAINT,constrain))
    nonempty_type_kind: . core_type
      core_type: . alias_type
        alias_type: . function_type
          function_type: . strict_function_or_labeled_tuple_type
            strict_function_or_labeled_tuple_type: . LIDENT COLON nonempty_list(mode_legacy) atomic_type STAR reversed_separated_nonempty_llist(STAR,labeled_tuple_typ_element) MINUSGREATER tuple_type
  ```
  Sample sentence (interface):
  ```ocaml
  type [@ and ] nonrec x1 := x2 : local_ {%ext|s3|} * {%ext|s4|} -> {%ext|s5|}
  ```
- Derivation (2 occurrences):
  ```
  generic_type_declaration(no_nonrec_flag,type_subst_kind): TYPE ext NONREC type_parameters LIDENT option(jkind_constraint) COLONEQUAL . nonempty_type_kind reversed_llist(preceded(CONSTRAINT,constrain))
    nonempty_type_kind: . core_type
      core_type: . alias_type
        alias_type: . function_type
          function_type: . strict_function_or_labeled_tuple_type
            strict_function_or_labeled_tuple_type: . LIDENT COLON nonempty_list(mode_legacy) atomic_type STAR reversed_separated_nonempty_llist(STAR,labeled_tuple_typ_element) MINUSGREATER tuple_type
  ```
  Sample sentence (interface):
  ```ocaml
  type nonrec x1 := x2 : local_ {%ext|s3|} * {%ext|s4|} -> {%ext|s5|}
  ```
- Derivation (2 occurrences):
  ```
  generic_type_declaration(no_nonrec_flag,type_subst_kind): TYPE ext nonempty_list(attribute) NONREC type_parameters LIDENT option(jkind_constraint) COLONEQUAL . nonempty_type_kind reversed_llist(preceded(CONSTRAINT,constrain))
    nonempty_type_kind: . core_type
      core_type: . alias_type
        alias_type: . function_type
          function_type: . strict_function_or_labeled_tuple_type
            strict_function_or_labeled_tuple_type: . LIDENT COLON atomic_type STAR reversed_separated_nonempty_llist(STAR,labeled_tuple_typ_element) at_mode_expr MINUSGREATER nonempty_list(mode_legacy) tuple_type
  ```
  Sample sentence (interface):
  ```ocaml
  type [@ and ] nonrec x1 := x2 : {%ext|s3|} * {%ext|s4|} @ x5 -> local_ {%ext|s6|}
  ```
- Derivation (2 occurrences):
  ```
  generic_type_declaration(no_nonrec_flag,type_subst_kind): TYPE ext NONREC type_parameters LIDENT option(jkind_constraint) COLONEQUAL . nonempty_type_kind reversed_llist(preceded(CONSTRAINT,constrain))
    nonempty_type_kind: . core_type
      core_type: . alias_type
        alias_type: . function_type
          function_type: . strict_function_or_labeled_tuple_type
            strict_function_or_labeled_tuple_type: . LIDENT COLON atomic_type STAR reversed_separated_nonempty_llist(STAR,labeled_tuple_typ_element) at_mode_expr MINUSGREATER nonempty_list(mode_legacy) tuple_type
  ```
  Sample sentence (interface):
  ```ocaml
  type nonrec x1 := x2 : {%ext|s3|} * {%ext|s4|} @ x5 -> local_ {%ext|s6|}
  ```
- Derivation (2 occurrences):
  ```
  generic_type_declaration(no_nonrec_flag,type_subst_kind): TYPE ext nonempty_list(attribute) NONREC type_parameters LIDENT option(jkind_constraint) COLONEQUAL . nonempty_type_kind reversed_llist(preceded(CONSTRAINT,constrain))
    nonempty_type_kind: . core_type
      core_type: . alias_type
        alias_type: . function_type
          function_type: . strict_function_or_labeled_tuple_type
            strict_function_or_labeled_tuple_type: . LIDENT COLON atomic_type STAR reversed_separated_nonempty_llist(STAR,labeled_tuple_typ_element) at_mode_expr MINUSGREATER tuple_type
  ```
  Sample sentence (interface):
  ```ocaml
  type [@ and ] nonrec x1 := x2 : {%ext|s3|} * {%ext|s4|} @ x5 -> {%ext|s6|}
  ```
- Derivation (2 occurrences):
  ```
  generic_type_declaration(no_nonrec_flag,type_subst_kind): TYPE ext NONREC type_parameters LIDENT option(jkind_constraint) COLONEQUAL . nonempty_type_kind reversed_llist(preceded(CONSTRAINT,constrain))
    nonempty_type_kind: . core_type
      core_type: . alias_type
        alias_type: . function_type
          function_type: . strict_function_or_labeled_tuple_type
            strict_function_or_labeled_tuple_type: . LIDENT COLON atomic_type STAR reversed_separated_nonempty_llist(STAR,labeled_tuple_typ_element) at_mode_expr MINUSGREATER tuple_type
  ```
  Sample sentence (interface):
  ```ocaml
  type nonrec x1 := x2 : {%ext|s3|} * {%ext|s4|} @ x5 -> {%ext|s6|}
  ```
- Derivation (2 occurrences):
  ```
  generic_type_declaration(no_nonrec_flag,type_subst_kind): TYPE ext nonempty_list(attribute) NONREC type_parameters LIDENT option(jkind_constraint) COLONEQUAL . nonempty_type_kind reversed_llist(preceded(CONSTRAINT,constrain))
    nonempty_type_kind: . core_type
      core_type: . alias_type
        alias_type: . function_type
          function_type: . strict_function_or_labeled_tuple_type
            strict_function_or_labeled_tuple_type: . LIDENT COLON atomic_type STAR reversed_separated_nonempty_llist(STAR,labeled_tuple_typ_element) MINUSGREATER nonempty_list(mode_legacy) tuple_type
  ```
  Sample sentence (interface):
  ```ocaml
  type [@ and ] nonrec x1 := x2 : {%ext|s3|} * {%ext|s4|} -> local_ {%ext|s5|}
  ```
- Derivation (2 occurrences):
  ```
  generic_type_declaration(no_nonrec_flag,type_subst_kind): TYPE ext NONREC type_parameters LIDENT option(jkind_constraint) COLONEQUAL . nonempty_type_kind reversed_llist(preceded(CONSTRAINT,constrain))
    nonempty_type_kind: . core_type
      core_type: . alias_type
        alias_type: . function_type
          function_type: . strict_function_or_labeled_tuple_type
            strict_function_or_labeled_tuple_type: . LIDENT COLON atomic_type STAR reversed_separated_nonempty_llist(STAR,labeled_tuple_typ_element) MINUSGREATER nonempty_list(mode_legacy) tuple_type
  ```
  Sample sentence (interface):
  ```ocaml
  type nonrec x1 := x2 : {%ext|s3|} * {%ext|s4|} -> local_ {%ext|s5|}
  ```
- ...


### Item `attr_id: . single_attr_id` (in 99 errors)

- Derivation (39 occurrences):
  ```
  attribute: LBRACKETAT . attr_id attr_payload RBRACKET
    attr_id: . single_attr_id
      single_attr_id: . AND
  ```
  Sample sentence (interface):
  ```ocaml
  type nonrec x1 := false [@ and ]
  ```
- Derivation (3 occurrences):
  ```
  post_item_attribute: LBRACKETATAT . attr_id attr_payload RBRACKET
    attr_id: . single_attr_id
      single_attr_id: . AND
  ```
  Sample sentence (interface):
  ```ocaml
  ;; type nonrec x1 := false [@@ and ]
  ```
- Derivation (1 occurrence):
  ```
  let_bindings(noext_attributes): LET PERCENT . attr_id mutable_flag rec_flag let_binding_body
    attr_id: . single_attr_id
      single_attr_id: . WITH
  ```
  Sample sentence (implementation):
  ```ocaml
  class x1 = let % with x2 in x3
  ```
- Derivation (1 occurrence):
  ```
  let_bindings(noext_attributes): LET PERCENT . attr_id mutable_flag rec_flag let_binding_body
    attr_id: . single_attr_id
      single_attr_id: . WHILE
  ```
  Sample sentence (implementation):
  ```ocaml
  class x1 = let % while x2 in x3
  ```
- Derivation (1 occurrence):
  ```
  let_bindings(noext_attributes): LET PERCENT . attr_id mutable_flag rec_flag let_binding_body
    attr_id: . single_attr_id
      single_attr_id: . WHEN
  ```
  Sample sentence (implementation):
  ```ocaml
  class x1 = let % when x2 in x3
  ```
- Derivation (1 occurrence):
  ```
  let_bindings(noext_attributes): LET PERCENT . attr_id mutable_flag rec_flag let_binding_body
    attr_id: . single_attr_id
      single_attr_id: . VIRTUAL
  ```
  Sample sentence (implementation):
  ```ocaml
  class x1 = let % virtual x2 in x3
  ```
- Derivation (1 occurrence):
  ```
  let_bindings(noext_attributes): LET PERCENT . attr_id mutable_flag rec_flag let_binding_body
    attr_id: . single_attr_id
      single_attr_id: . VAL
  ```
  Sample sentence (implementation):
  ```ocaml
  class x1 = let % val x2 in x3
  ```
- Derivation (1 occurrence):
  ```
  let_bindings(noext_attributes): LET PERCENT . attr_id mutable_flag rec_flag let_binding_body
    attr_id: . single_attr_id
      single_attr_id: . TYPE
  ```
  Sample sentence (implementation):
  ```ocaml
  class x1 = let % type x2 in x3
  ```
- Derivation (1 occurrence):
  ```
  let_bindings(noext_attributes): LET PERCENT . attr_id mutable_flag rec_flag let_binding_body
    attr_id: . single_attr_id
      single_attr_id: . TRY
  ```
  Sample sentence (implementation):
  ```ocaml
  class x1 = let % try x2 in x3
  ```
- Derivation (1 occurrence):
  ```
  let_bindings(noext_attributes): LET PERCENT . attr_id mutable_flag rec_flag let_binding_body
    attr_id: . single_attr_id
      single_attr_id: . TRUE
  ```
  Sample sentence (implementation):
  ```ocaml
  class x1 = let % true x2 in x3
  ```
- Derivation (1 occurrence):
  ```
  let_bindings(noext_attributes): LET PERCENT . attr_id mutable_flag rec_flag let_binding_body
    attr_id: . single_attr_id
      single_attr_id: . TO
  ```
  Sample sentence (implementation):
  ```ocaml
  class x1 = let % to x2 in x3
  ```
- Derivation (1 occurrence):
  ```
  let_bindings(noext_attributes): LET PERCENT . attr_id mutable_flag rec_flag let_binding_body
    attr_id: . single_attr_id
      single_attr_id: . THEN
  ```
  Sample sentence (implementation):
  ```ocaml
  class x1 = let % then x2 in x3
  ```
- Derivation (1 occurrence):
  ```
  let_bindings(noext_attributes): LET PERCENT . attr_id mutable_flag rec_flag let_binding_body
    attr_id: . single_attr_id
      single_attr_id: . STRUCT
  ```
  Sample sentence (implementation):
  ```ocaml
  class x1 = let % struct x2 in x3
  ```
- Derivation (1 occurrence):
  ```
  let_bindings(noext_attributes): LET PERCENT . attr_id mutable_flag rec_flag let_binding_body
    attr_id: . single_attr_id
      single_attr_id: . SIG
  ```
  Sample sentence (implementation):
  ```ocaml
  class x1 = let % sig x2 in x3
  ```
- Derivation (1 occurrence):
  ```
  let_bindings(noext_attributes): LET PERCENT . attr_id mutable_flag rec_flag let_binding_body
    attr_id: . single_attr_id
      single_attr_id: . REPR
  ```
  Sample sentence (implementation):
  ```ocaml
  class x1 = let % repr_ x2 in x3
  ```
- Derivation (1 occurrence):
  ```
  let_bindings(noext_attributes): LET PERCENT . attr_id mutable_flag rec_flag let_binding_body
    attr_id: . single_attr_id
      single_attr_id: . REC
  ```
  Sample sentence (implementation):
  ```ocaml
  class x1 = let % rec x2 in x3
  ```
- Derivation (1 occurrence):
  ```
  let_bindings(noext_attributes): LET PERCENT . attr_id mutable_flag rec_flag let_binding_body
    attr_id: . single_attr_id
      single_attr_id: . PRIVATE
  ```
  Sample sentence (implementation):
  ```ocaml
  class x1 = let % private x2 in x3
  ```
- Derivation (1 occurrence):
  ```
  let_bindings(noext_attributes): LET PERCENT . attr_id mutable_flag rec_flag let_binding_body
    attr_id: . single_attr_id
      single_attr_id: . POLY
  ```
  Sample sentence (implementation):
  ```ocaml
  class x1 = let % poly_ x2 in x3
  ```
- Derivation (1 occurrence):
  ```
  let_bindings(noext_attributes): LET PERCENT . attr_id mutable_flag rec_flag let_binding_body
    attr_id: . single_attr_id
      single_attr_id: . OR
  ```
  Sample sentence (implementation):
  ```ocaml
  class x1 = let % or x2 in x3
  ```
- Derivation (1 occurrence):
  ```
  let_bindings(noext_attributes): LET PERCENT . attr_id mutable_flag rec_flag let_binding_body
    attr_id: . single_attr_id
      single_attr_id: . OPEN
  ```
  Sample sentence (implementation):
  ```ocaml
  class x1 = let % open x2 in x3
  ```
- Derivation (1 occurrence):
  ```
  let_bindings(noext_attributes): LET PERCENT . attr_id mutable_flag rec_flag let_binding_body
    attr_id: . single_attr_id
      single_attr_id: . OF
  ```
  Sample sentence (implementation):
  ```ocaml
  class x1 = let % of x2 in x3
  ```
- ...


### Item `at_mode_expr: . AT nonempty_list(mode)` (in 65 errors)

- Derivation (64 occurrences):
  ```
  simple_expr: LPAREN seq_expr COLONGREATER tuple_type . at_mode_expr RPAREN
    at_mode_expr: . AT nonempty_list(mode)
  ```
  Sample sentence (implementation):
  ```ocaml
  ( ( X1 :> {%ext|s2|} @ x3 ) )
  ```
- Derivation (1 occurrence):
  ```
  let_binding_body_no_punning: LPAREN val_ident . at_mode_expr RPAREN at_mode_expr EQUAL seq_expr
    at_mode_expr: . AT nonempty_list(mode)
  ```
  Sample sentence (implementation):
  ```ocaml
  let ( x1 @ x2 ) @ x3 = ( X4 :> {%ext|s5|} @ x6 )
  ```


### Item `generic_constructor_declaration(epsilon): . constr_ident generalized_constructor_arguments` (in 27 errors)

- Derivation (4 occurrences):
  ```
  signature_item: TYPE ext NONREC type_parameters type_longident PLUSEQ private_flag . reversed_bar_llist(extension_constructor_declaration)
    reversed_bar_llist(extension_constructor_declaration): . generic_constructor_declaration(epsilon)
      generic_constructor_declaration(epsilon): . constr_ident generalized_constructor_arguments
        constr_ident: . constr_extra_nonprefix_ident
          constr_extra_nonprefix_ident: . FALSE
  ```
  Sample sentence (interface):
  ```ocaml
  {%%ext|s1|} type nonrec x2 += false
  ```
- Derivation (2 occurrences):
  ```
  nonempty_type_kind: PRIVATE . constructor_declarations
    constructor_declarations: . reversed_bar_llist(constructor_declaration)
      reversed_bar_llist(constructor_declaration): . generic_constructor_declaration(epsilon)
        generic_constructor_declaration(epsilon): . constr_ident generalized_constructor_arguments
          constr_ident: . constr_extra_nonprefix_ident
            constr_extra_nonprefix_ident: . FALSE
  ```
  Sample sentence (interface):
  ```ocaml
  type nonrec x1 := private false
  ```
- Derivation (2 occurrences):
  ```
  generic_type_declaration(no_nonrec_flag,type_subst_kind): TYPE ext NONREC type_parameters LIDENT option(jkind_constraint) COLONEQUAL . nonempty_type_kind reversed_llist(preceded(CONSTRAINT,constrain))
    nonempty_type_kind: . constructor_declarations
      constructor_declarations: . reversed_bar_llist(constructor_declaration)
        reversed_bar_llist(constructor_declaration): . generic_constructor_declaration(epsilon)
          generic_constructor_declaration(epsilon): . constr_ident generalized_constructor_arguments
            constr_ident: . constr_extra_nonprefix_ident
              constr_extra_nonprefix_ident: . FALSE
  ```
  Sample sentence (interface):
  ```ocaml
  ;; type nonrec x1 := false
  ```
- Derivation (1 occurrence):
  ```
  signature_item: TYPE ext nonempty_list(attribute) NONREC type_parameters type_longident PLUSEQ private_flag . reversed_bar_llist(extension_constructor_declaration)
    reversed_bar_llist(extension_constructor_declaration): . generic_constructor_declaration(epsilon)
      generic_constructor_declaration(epsilon): . constr_ident generalized_constructor_arguments
        constr_ident: . LPAREN COLONCOLON RPAREN
  ```
  Sample sentence (interface):
  ```ocaml
  type [@ and ] nonrec x1 += ( :: )
  ```
- Derivation (1 occurrence):
  ```
  generic_type_declaration(no_nonrec_flag,type_subst_kind): TYPE ext nonempty_list(attribute) NONREC type_parameters LIDENT option(jkind_constraint) COLONEQUAL . nonempty_type_kind reversed_llist(preceded(CONSTRAINT,constrain))
    nonempty_type_kind: . constructor_declarations
      constructor_declarations: . reversed_bar_llist(constructor_declaration)
        reversed_bar_llist(constructor_declaration): . generic_constructor_declaration(epsilon)
          generic_constructor_declaration(epsilon): . constr_ident generalized_constructor_arguments
            constr_ident: . LPAREN COLONCOLON RPAREN
  ```
  Sample sentence (interface):
  ```ocaml
  type [@ and ] nonrec x1 := ( :: )
  ```
- Derivation (1 occurrence):
  ```
  generic_type_declaration(no_nonrec_flag,type_subst_kind): TYPE ext NONREC type_parameters LIDENT option(jkind_constraint) COLONEQUAL . nonempty_type_kind reversed_llist(preceded(CONSTRAINT,constrain))
    nonempty_type_kind: . constructor_declarations
      constructor_declarations: . reversed_bar_llist(constructor_declaration)
        reversed_bar_llist(constructor_declaration): . generic_constructor_declaration(epsilon)
          generic_constructor_declaration(epsilon): . constr_ident generalized_constructor_arguments
            constr_ident: . LPAREN COLONCOLON RPAREN
  ```
  Sample sentence (interface):
  ```ocaml
  type nonrec x1 := ( :: )
  ```
- Derivation (1 occurrence):
  ```
  signature_item: TYPE ext nonempty_list(attribute) NONREC type_parameters type_longident PLUSEQ private_flag . reversed_bar_llist(extension_constructor_declaration)
    reversed_bar_llist(extension_constructor_declaration): . generic_constructor_declaration(epsilon)
      generic_constructor_declaration(epsilon): . constr_ident generalized_constructor_arguments
        constr_ident: . UIDENT
  ```
  Sample sentence (interface):
  ```ocaml
  type [@ and ] nonrec x1 += X2
  ```
- Derivation (1 occurrence):
  ```
  signature_item: TYPE ext NONREC type_parameters type_longident PLUSEQ private_flag . reversed_bar_llist(extension_constructor_declaration)
    reversed_bar_llist(extension_constructor_declaration): . generic_constructor_declaration(epsilon)
      generic_constructor_declaration(epsilon): . constr_ident generalized_constructor_arguments
        constr_ident: . UIDENT
  ```
  Sample sentence (interface):
  ```ocaml
  type nonrec x1 += X2
  ```
- Derivation (1 occurrence):
  ```
  structure_item: TYPE ext nonempty_list(attribute) NONREC type_parameters type_longident PLUSEQ private_flag . reversed_bar_llist(extension_constructor)
    reversed_bar_llist(extension_constructor): . generic_constructor_declaration(epsilon)
      generic_constructor_declaration(epsilon): . constr_ident generalized_constructor_arguments
        constr_ident: . UIDENT
  ```
  Sample sentence (implementation):
  ```ocaml
  type [@ and ] nonrec x1 += X2
  ```
- Derivation (1 occurrence):
  ```
  signature_item: TYPE ext nonempty_list(attribute) NONREC type_parameters type_longident PLUSEQ private_flag . reversed_bar_llist(extension_constructor_declaration)
    reversed_bar_llist(extension_constructor_declaration): . generic_constructor_declaration(epsilon)
      generic_constructor_declaration(epsilon): . constr_ident generalized_constructor_arguments
        constr_ident: . constr_extra_nonprefix_ident
          constr_extra_nonprefix_ident: . TRUE
  ```
  Sample sentence (interface):
  ```ocaml
  type [@ and ] nonrec x1 += true
  ```
- Derivation (1 occurrence):
  ```
  signature_item: TYPE ext NONREC type_parameters type_longident PLUSEQ private_flag . reversed_bar_llist(extension_constructor_declaration)
    reversed_bar_llist(extension_constructor_declaration): . generic_constructor_declaration(epsilon)
      generic_constructor_declaration(epsilon): . constr_ident generalized_constructor_arguments
        constr_ident: . constr_extra_nonprefix_ident
          constr_extra_nonprefix_ident: . TRUE
  ```
  Sample sentence (interface):
  ```ocaml
  type nonrec x1 += true
  ```
- Derivation (1 occurrence):
  ```
  structure_item: TYPE ext nonempty_list(attribute) NONREC type_parameters type_longident PLUSEQ private_flag . reversed_bar_llist(extension_constructor)
    reversed_bar_llist(extension_constructor): . generic_constructor_declaration(epsilon)
      generic_constructor_declaration(epsilon): . constr_ident generalized_constructor_arguments
        constr_ident: . constr_extra_nonprefix_ident
          constr_extra_nonprefix_ident: . TRUE
  ```
  Sample sentence (implementation):
  ```ocaml
  type [@ and ] nonrec x1 += true
  ```
- Derivation (1 occurrence):
  ```
  generic_type_declaration(no_nonrec_flag,type_subst_kind): TYPE ext nonempty_list(attribute) NONREC type_parameters LIDENT option(jkind_constraint) COLONEQUAL . nonempty_type_kind reversed_llist(preceded(CONSTRAINT,constrain))
    nonempty_type_kind: . constructor_declarations
      constructor_declarations: . reversed_bar_llist(constructor_declaration)
        reversed_bar_llist(constructor_declaration): . generic_constructor_declaration(epsilon)
          generic_constructor_declaration(epsilon): . constr_ident generalized_constructor_arguments
            constr_ident: . constr_extra_nonprefix_ident
              constr_extra_nonprefix_ident: . TRUE
  ```
  Sample sentence (interface):
  ```ocaml
  type [@ and ] nonrec x1 := true
  ```
- Derivation (1 occurrence):
  ```
  generic_type_declaration(no_nonrec_flag,type_subst_kind): TYPE ext NONREC type_parameters LIDENT option(jkind_constraint) COLONEQUAL . nonempty_type_kind reversed_llist(preceded(CONSTRAINT,constrain))
    nonempty_type_kind: . constructor_declarations
      constructor_declarations: . reversed_bar_llist(constructor_declaration)
        reversed_bar_llist(constructor_declaration): . reversed_bar_llist(constructor_declaration) generic_constructor_declaration(BAR)
          reversed_bar_llist(constructor_declaration): . reversed_bar_llist(constructor_declaration) generic_constructor_declaration(BAR)
            reversed_bar_llist(constructor_declaration): . generic_constructor_declaration(epsilon)
              generic_constructor_declaration(epsilon): . constr_ident generalized_constructor_arguments
                constr_ident: . constr_extra_nonprefix_ident
                  constr_extra_nonprefix_ident: . FALSE
  ```
  Sample sentence (interface):
  ```ocaml
  type nonrec x1 := false | false | false
  ```
- Derivation (1 occurrence):
  ```
  generic_type_declaration(no_nonrec_flag,type_subst_kind): TYPE ext NONREC type_parameters LIDENT option(jkind_constraint) COLONEQUAL . nonempty_type_kind reversed_llist(preceded(CONSTRAINT,constrain))
    nonempty_type_kind: . constructor_declarations
      constructor_declarations: . reversed_bar_llist(constructor_declaration)
        reversed_bar_llist(constructor_declaration): . reversed_bar_llist(constructor_declaration) generic_constructor_declaration(BAR)
          reversed_bar_llist(constructor_declaration): . generic_constructor_declaration(epsilon)
            generic_constructor_declaration(epsilon): . constr_ident generalized_constructor_arguments
              constr_ident: . constr_extra_nonprefix_ident
                constr_extra_nonprefix_ident: . FALSE
  ```
  Sample sentence (interface):
  ```ocaml
  type nonrec x1 := false | false and x2 := false
  ```
- Derivation (1 occurrence):
  ```
  signature_item: TYPE ext nonempty_list(attribute) NONREC type_parameters type_longident PLUSEQ private_flag . reversed_bar_llist(extension_constructor_declaration)
    reversed_bar_llist(extension_constructor_declaration): . generic_constructor_declaration(epsilon)
      generic_constructor_declaration(epsilon): . constr_ident generalized_constructor_arguments
        constr_ident: . constr_extra_nonprefix_ident
          constr_extra_nonprefix_ident: . LPAREN RPAREN
  ```
  Sample sentence (interface):
  ```ocaml
  type [@ and ] nonrec x1 += ( )
  ```
- Derivation (1 occurrence):
  ```
  signature_item: TYPE ext NONREC type_parameters type_longident PLUSEQ private_flag . reversed_bar_llist(extension_constructor_declaration)
    reversed_bar_llist(extension_constructor_declaration): . generic_constructor_declaration(epsilon)
      generic_constructor_declaration(epsilon): . constr_ident generalized_constructor_arguments
        constr_ident: . constr_extra_nonprefix_ident
          constr_extra_nonprefix_ident: . LPAREN RPAREN
  ```
  Sample sentence (interface):
  ```ocaml
  type nonrec x1 += ( )
  ```
- Derivation (1 occurrence):
  ```
  generic_type_declaration(no_nonrec_flag,type_subst_kind): TYPE ext nonempty_list(attribute) NONREC type_parameters LIDENT option(jkind_constraint) COLONEQUAL . nonempty_type_kind reversed_llist(preceded(CONSTRAINT,constrain))
    nonempty_type_kind: . constructor_declarations
      constructor_declarations: . reversed_bar_llist(constructor_declaration)
        reversed_bar_llist(constructor_declaration): . generic_constructor_declaration(epsilon)
          generic_constructor_declaration(epsilon): . constr_ident generalized_constructor_arguments
            constr_ident: . constr_extra_nonprefix_ident
              constr_extra_nonprefix_ident: . LPAREN RPAREN
  ```
  Sample sentence (interface):
  ```ocaml
  type [@ and ] nonrec x1 := ( )
  ```
- Derivation (1 occurrence):
  ```
  signature_item: TYPE ext nonempty_list(attribute) NONREC type_parameters type_longident PLUSEQ private_flag . reversed_bar_llist(extension_constructor_declaration)
    reversed_bar_llist(extension_constructor_declaration): . generic_constructor_declaration(epsilon)
      generic_constructor_declaration(epsilon): . constr_ident generalized_constructor_arguments
        constr_ident: . constr_extra_nonprefix_ident
          constr_extra_nonprefix_ident: . LBRACKET RBRACKET
  ```
  Sample sentence (interface):
  ```ocaml
  type [@ and ] nonrec x1 += [ ]
  ```
- Derivation (1 occurrence):
  ```
  signature_item: TYPE ext NONREC type_parameters type_longident PLUSEQ private_flag . reversed_bar_llist(extension_constructor_declaration)
    reversed_bar_llist(extension_constructor_declaration): . generic_constructor_declaration(epsilon)
      generic_constructor_declaration(epsilon): . constr_ident generalized_constructor_arguments
        constr_ident: . constr_extra_nonprefix_ident
          constr_extra_nonprefix_ident: . LBRACKET RBRACKET
  ```
  Sample sentence (interface):
  ```ocaml
  type nonrec x1 += [ ]
  ```
- Derivation (1 occurrence):
  ```
  structure_item: TYPE ext nonempty_list(attribute) NONREC type_parameters type_longident PLUSEQ private_flag . reversed_bar_llist(extension_constructor)
    reversed_bar_llist(extension_constructor): . generic_constructor_declaration(epsilon)
      generic_constructor_declaration(epsilon): . constr_ident generalized_constructor_arguments
        constr_ident: . constr_extra_nonprefix_ident
          constr_extra_nonprefix_ident: . LBRACKET RBRACKET
  ```
  Sample sentence (implementation):
  ```ocaml
  type [@ and ] nonrec x1 += [ ]
  ```
- ...


### Item `generic_type_declaration(no_nonrec_flag,type_subst_kind): TYPE ext nonempty_list(attribute) NONREC type_parameters LIDENT option(jkind_constraint) COLONEQUAL . nonempty_type_kind reversed_llist(preceded(CONSTRAINT,constrain))` (in 11 errors)

- Derivation (3 occurrences):
  ```
  generic_type_declaration(no_nonrec_flag,type_subst_kind): TYPE ext nonempty_list(attribute) NONREC type_parameters LIDENT option(jkind_constraint) COLONEQUAL . nonempty_type_kind reversed_llist(preceded(CONSTRAINT,constrain))
    nonempty_type_kind: . PRIVATE constructor_declarations
  ```
  Sample sentence (interface):
  ```ocaml
  type [@ and ] nonrec x1 := private false
  ```
- Derivation (2 occurrences):
  ```
  generic_type_declaration(no_nonrec_flag,type_subst_kind): TYPE ext nonempty_list(attribute) NONREC type_parameters LIDENT option(jkind_constraint) COLONEQUAL . nonempty_type_kind reversed_llist(preceded(CONSTRAINT,constrain))
    nonempty_type_kind: . PRIVATE core_type
  ```
  Sample sentence (interface):
  ```ocaml
  type [@ and ] nonrec x1 := private {%ext|s2|}
  ```
- Derivation (1 occurrence):
  ```
  generic_type_declaration(no_nonrec_flag,type_subst_kind): TYPE ext nonempty_list(attribute) NONREC type_parameters LIDENT option(jkind_constraint) COLONEQUAL . nonempty_type_kind reversed_llist(preceded(CONSTRAINT,constrain))
    nonempty_type_kind: . PRIVATE HASHLBRACE label_declarations RBRACE
  ```
  Sample sentence (interface):
  ```ocaml
  type [@ and ] nonrec x1 := private #{ x2 : {%ext|s3|} }
  ```
- Derivation (1 occurrence):
  ```
  generic_type_declaration(no_nonrec_flag,type_subst_kind): TYPE ext nonempty_list(attribute) NONREC type_parameters LIDENT option(jkind_constraint) COLONEQUAL . nonempty_type_kind reversed_llist(preceded(CONSTRAINT,constrain))
    nonempty_type_kind: . HASHLBRACE label_declarations RBRACE
  ```
  Sample sentence (interface):
  ```ocaml
  type [@ and ] nonrec x1 := #{ x2 : {%ext|s3|} }
  ```
- Derivation (1 occurrence):
  ```
  generic_type_declaration(no_nonrec_flag,type_subst_kind): TYPE ext nonempty_list(attribute) NONREC type_parameters LIDENT option(jkind_constraint) COLONEQUAL . nonempty_type_kind reversed_llist(preceded(CONSTRAINT,constrain))
    nonempty_type_kind: . PRIVATE LBRACE label_declarations RBRACE
  ```
  Sample sentence (interface):
  ```ocaml
  type [@ and ] nonrec x1 := private { x2 : {%ext|s3|} }
  ```
- Derivation (1 occurrence):
  ```
  generic_type_declaration(no_nonrec_flag,type_subst_kind): TYPE ext nonempty_list(attribute) NONREC type_parameters LIDENT option(jkind_constraint) COLONEQUAL . nonempty_type_kind reversed_llist(preceded(CONSTRAINT,constrain))
    nonempty_type_kind: . LBRACE label_declarations RBRACE
  ```
  Sample sentence (interface):
  ```ocaml
  type [@ and ] nonrec x1 := { x2 : {%ext|s3|} }
  ```
- Derivation (1 occurrence):
  ```
  generic_type_declaration(no_nonrec_flag,type_subst_kind): TYPE ext nonempty_list(attribute) NONREC type_parameters LIDENT option(jkind_constraint) COLONEQUAL . nonempty_type_kind reversed_llist(preceded(CONSTRAINT,constrain))
    nonempty_type_kind: . PRIVATE DOTDOT
  ```
  Sample sentence (interface):
  ```ocaml
  type [@ and ] nonrec x1 := private ..
  ```
- Derivation (1 occurrence):
  ```
  generic_type_declaration(no_nonrec_flag,type_subst_kind): TYPE ext nonempty_list(attribute) NONREC type_parameters LIDENT option(jkind_constraint) COLONEQUAL . nonempty_type_kind reversed_llist(preceded(CONSTRAINT,constrain))
    nonempty_type_kind: . DOTDOT
  ```
  Sample sentence (interface):
  ```ocaml
  type [@ and ] nonrec x1 := ..
  ```


### Item `mutable_flag: . MUTABLE` (in 6 errors)

- Derivation (1 occurrence):
  ```
  let_bindings(noext_attributes): LET PERCENT attr_id nonempty_list(attribute) . mutable_flag rec_flag let_binding_body
    mutable_flag: . MUTABLE
  ```
  Sample sentence (implementation):
  ```ocaml
  class x1 = let % and [@ and ] mutable x2 in x3
  ```
- Derivation (1 occurrence):
  ```
  let_bindings(noext_attributes): LET PERCENT attr_id . mutable_flag rec_flag let_binding_body
    mutable_flag: . MUTABLE
  ```
  Sample sentence (implementation):
  ```ocaml
  class x1 = let % and mutable x2 in x3
  ```
- Derivation (1 occurrence):
  ```
  let_bindings(noext_attributes): LET nonempty_list(attribute) . mutable_flag rec_flag let_binding_body
    mutable_flag: . MUTABLE
  ```
  Sample sentence (implementation):
  ```ocaml
  class x1 = let [@ and ] mutable x2 in x3
  ```
- Derivation (1 occurrence):
  ```
  let_bindings(noext_attributes): LET . mutable_flag rec_flag let_binding_body
    mutable_flag: . MUTABLE
  ```
  Sample sentence (implementation):
  ```ocaml
  class x1 = let mutable x2 in x3
  ```
- Derivation (1 occurrence):
  ```
  let_bindings(ext_attributes): LET ext nonempty_list(attribute) . mutable_flag rec_flag let_binding_body
    mutable_flag: . MUTABLE
  ```
  Sample sentence (implementation):
  ```ocaml
  let [@ and ] mutable x1
  ```
- Derivation (1 occurrence):
  ```
  let_bindings(ext_attributes): LET ext . mutable_flag rec_flag let_binding_body
    mutable_flag: . MUTABLE
  ```
  Sample sentence (implementation):
  ```ocaml
  let mutable x1
  ```


### Item `private_flag: . PRIVATE` (in 3 errors)

- Derivation (1 occurrence):
  ```
  structure_item: TYPE ext nonempty_list(attribute) NONREC type_parameters type_longident PLUSEQ . private_flag reversed_bar_llist(extension_constructor)
    private_flag: . PRIVATE
  ```
  Sample sentence (implementation):
  ```ocaml
  type [@ and ] nonrec x1 += private false
  ```
- Derivation (1 occurrence):
  ```
  signature_item: TYPE ext nonempty_list(attribute) NONREC type_parameters type_longident PLUSEQ . private_flag reversed_bar_llist(extension_constructor_declaration)
    private_flag: . PRIVATE
  ```
  Sample sentence (interface):
  ```ocaml
  type [@ and ] nonrec x1 += private false
  ```
- Derivation (1 occurrence):
  ```
  signature_item: TYPE ext NONREC type_parameters type_longident PLUSEQ . private_flag reversed_bar_llist(extension_constructor_declaration)
    private_flag: . PRIVATE
  ```
  Sample sentence (interface):
  ```ocaml
  type nonrec x1 += private false
  ```


### Item `let_binding_body: . poly_flag val_ident` (in 2 errors)

- Derivation (1 occurrence):
  ```
  let_bindings(noext_attributes): LET PERCENT attr_id nonempty_list(attribute) mutable_flag rec_flag . let_binding_body
    let_binding_body: . poly_flag val_ident
      poly_flag: . POLY
  ```
  Sample sentence (implementation):
  ```ocaml
  class x1 = let % and [@ and ] poly_ x2 in x3
  ```
- Derivation (1 occurrence):
  ```
  let_bindings(noext_attributes): LET PERCENT attr_id mutable_flag rec_flag . let_binding_body
    let_binding_body: . poly_flag val_ident
      poly_flag: . POLY
  ```
  Sample sentence (implementation):
  ```ocaml
  class x1 = let % and poly_ x2 in x3
  ```


### Item `rec_flag: . REC` (in 2 errors)

- Derivation (1 occurrence):
  ```
  let_bindings(noext_attributes): LET PERCENT attr_id nonempty_list(attribute) mutable_flag . rec_flag let_binding_body
    rec_flag: . REC
  ```
  Sample sentence (implementation):
  ```ocaml
  class x1 = let % and [@ and ] rec x2 in x3
  ```
- Derivation (1 occurrence):
  ```
  let_bindings(noext_attributes): LET PERCENT attr_id mutable_flag . rec_flag let_binding_body
    rec_flag: . REC
  ```
  Sample sentence (implementation):
  ```ocaml
  class x1 = let % and rec x2 in x3
  ```


