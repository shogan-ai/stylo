## ast changed (4546 errors)

### Item `fun_seq_expr: . fun_expr` (in 4461 errors)

- Derivation (920 occurrences):
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
  object initializer exclave_ X1 ; := X2 end
  ```
- Derivation (347 occurrences):
  ```
  implementation: . structure EOF
    structure: . seq_expr list(structure_element)
      seq_expr: . fun_seq_expr
        fun_seq_expr: . fun_expr
          fun_expr: . BORROW simple_expr
  ```
  Sample sentence (implementation):
  ```ocaml
  borrow_ #'a' .+ { exclave_ X1 ; := X2 }
  ```
- Derivation (335 occurrences):
  ```
  implementation: . structure EOF
    structure: . seq_expr list(structure_element)
      seq_expr: . fun_seq_expr
        fun_seq_expr: . fun_expr
          fun_expr: . simple_expr
            simple_expr: . LBRACKETCOLON fun_expr reversed_nonempty_llist(comprehension_clause) COLONRBRACKET
  ```
  Sample sentence (implementation):
  ```ocaml
  [: X1 when exclave_ X2 ; := X3 :]
  ```
- Derivation (244 occurrences):
  ```
  implementation: . structure EOF
    structure: . seq_expr list(structure_element)
      seq_expr: . fun_seq_expr
        fun_seq_expr: . fun_expr
          fun_expr: . simple_expr
            simple_expr: . LBRACKETCOLON separated_or_terminated_nonempty_list(SEMI,expr) COLONRBRACKET
  ```
  Sample sentence (implementation):
  ```ocaml
  [: exclave_ X1 ; := X2 :]
  ```
- Derivation (124 occurrences):
  ```
  implementation: . structure EOF
    structure: . seq_expr list(structure_element)
      seq_expr: . fun_seq_expr
        fun_seq_expr: . fun_expr
          fun_expr: . LETOP letop_bindings IN seq_expr
  ```
  Sample sentence (implementation):
  ```ocaml
  let* false = exclave_ X1 ; := X2 in X3
  ```
- Derivation (93 occurrences):
  ```
  implementation: . structure EOF
    structure: . seq_expr list(structure_element)
      seq_expr: . fun_seq_expr
        fun_seq_expr: . fun_expr
          fun_expr: . FOR ext nonempty_list(attribute) pattern EQUAL seq_expr direction_flag seq_expr DO seq_expr DONE
  ```
  Sample sentence (implementation):
  ```ocaml
  for [@ and ] false = X1 downto X2 do exclave_ X3 ; := X4 done
  ```
- Derivation (80 occurrences):
  ```
  implementation: . structure EOF
    structure: . seq_expr list(structure_element)
      seq_expr: . fun_seq_expr
        fun_seq_expr: . fun_expr
          fun_expr: . WHILE ext nonempty_list(attribute) seq_expr DO seq_expr DONE
  ```
  Sample sentence (implementation):
  ```ocaml
  while [@ and ] local_ X1 ; land X2 do X3 done
  ```
- Derivation (67 occurrences):
  ```
  implementation: . structure EOF
    structure: . seq_expr list(structure_element)
      seq_expr: . fun_seq_expr
        fun_seq_expr: . fun_expr
          fun_expr: . FOR ext pattern EQUAL seq_expr direction_flag seq_expr DO seq_expr DONE
  ```
  Sample sentence (implementation):
  ```ocaml
  for false = X1 downto X2 do exclave_ X3 ; := X4 done
  ```
- Derivation (63 occurrences):
  ```
  implementation: . structure EOF
    structure: . seq_expr list(structure_element)
      seq_expr: . fun_seq_expr
        fun_seq_expr: . fun_expr
          fun_expr: . simple_expr
            simple_expr: . mod_longident DOT LPAREN seq_expr RPAREN
              mod_longident: . mk_longident(mod_longident,str_not_op(UIDENT))
                mk_longident(mod_longident,str_not_op(UIDENT)): . UIDENT
  ```
  Sample sentence (implementation):
  ```ocaml
  X1 . ( exclave_ X2 ; := X3 )
  ```
- Derivation (57 occurrences):
  ```
  implementation: . structure EOF
    structure: . seq_expr list(structure_element)
      seq_expr: . fun_seq_expr
        fun_seq_expr: . fun_expr
          fun_expr: . simple_expr
            simple_expr: . BEGIN ext seq_expr END
  ```
  Sample sentence (implementation):
  ```ocaml
  begin exclave_ X1 ; := X2 end
  ```
- Derivation (55 occurrences):
  ```
  implementation: . structure EOF
    structure: . seq_expr list(structure_element)
      seq_expr: . fun_seq_expr
        fun_seq_expr: . fun_expr
          fun_expr: . simple_expr
            simple_expr: . BEGIN ext nonempty_list(attribute) seq_expr END
  ```
  Sample sentence (implementation):
  ```ocaml
  begin [@ and ] exclave_ X1 ; := X2 end
  ```
- Derivation (49 occurrences):
  ```
  implementation: . structure EOF
    structure: . seq_expr list(structure_element)
      seq_expr: . fun_seq_expr
        fun_seq_expr: . fun_expr
          fun_expr: . simple_expr
            simple_expr: . simple_expr DOT mod_longident DOTOP LPAREN separated_or_terminated_nonempty_list(SEMI,expr) RPAREN
              simple_expr: . constr_longident
                constr_longident: . constr_extra_nonprefix_ident
                  constr_extra_nonprefix_ident: . FALSE
  ```
  Sample sentence (implementation):
  ```ocaml
  false . X1 .+ ( exclave_ X2 ; := X3 )
  ```
- Derivation (48 occurrences):
  ```
  implementation: . structure EOF
    structure: . seq_expr list(structure_element)
      seq_expr: . fun_seq_expr
        fun_seq_expr: . fun_expr
          fun_expr: . simple_expr
            simple_expr: . simple_expr DOT LBRACKET seq_expr RBRACKET
              simple_expr: . constr_longident
                constr_longident: . constr_extra_nonprefix_ident
                  constr_extra_nonprefix_ident: . FALSE
  ```
  Sample sentence (implementation):
  ```ocaml
  false . [ exclave_ X1 ; := X2 ]
  ```
- Derivation (44 occurrences):
  ```
  implementation: . structure EOF
    structure: . seq_expr list(structure_element)
      seq_expr: . fun_seq_expr
        fun_seq_expr: . fun_expr
          fun_expr: . IF ext seq_expr THEN fun_expr
  ```
  Sample sentence (implementation):
  ```ocaml
  if exclave_ X1 ; := X2 then X3
  ```
- Derivation (41 occurrences):
  ```
  implementation: . structure EOF
    structure: . seq_expr list(structure_element)
      seq_expr: . fun_seq_expr
        fun_seq_expr: . fun_expr
          fun_expr: . simple_expr
            simple_expr: . mod_longident DOT LBRACKET separated_or_terminated_nonempty_list(SEMI,expr) RBRACKET
              mod_longident: . mk_longident(mod_longident,str_not_op(UIDENT))
                mk_longident(mod_longident,str_not_op(UIDENT)): . UIDENT
  ```
  Sample sentence (implementation):
  ```ocaml
  X1 . [ local_ X2 ; land X3 ]
  ```
- Derivation (40 occurrences):
  ```
  implementation: . structure EOF
    structure: . seq_expr list(structure_element)
      seq_expr: . fun_seq_expr
        fun_seq_expr: . fun_expr
          fun_expr: . simple_expr
            simple_expr: . mod_longident DOT LBRACKETBAR separated_or_terminated_nonempty_list(SEMI,expr) BARRBRACKET
              mod_longident: . mk_longident(mod_longident,str_not_op(UIDENT))
                mk_longident(mod_longident,str_not_op(UIDENT)): . UIDENT
  ```
  Sample sentence (implementation):
  ```ocaml
  X1 . [| local_ X2 ; land X3 |]
  ```
- Derivation (40 occurrences):
  ```
  implementation: . structure EOF
    structure: . seq_expr list(structure_element)
      seq_expr: . fun_seq_expr
        fun_seq_expr: . fun_expr
          fun_expr: . simple_expr
            simple_expr: . simple_expr DOTOP LBRACE separated_or_terminated_nonempty_list(SEMI,expr) RBRACE
              simple_expr: . constr_longident
                constr_longident: . mod_longident
                  mod_longident: . mk_longident(mod_longident,str_not_op(UIDENT))
                    mk_longident(mod_longident,str_not_op(UIDENT)): . UIDENT
  ```
  Sample sentence (implementation):
  ```ocaml
  X1 .+ { local_ X2 ; land X3 }
  ```
- Derivation (40 occurrences):
  ```
  implementation: . structure EOF
    structure: . seq_expr list(structure_element)
      seq_expr: . fun_seq_expr
        fun_seq_expr: . fun_expr
          fun_expr: . WHILE ext seq_expr DO seq_expr DONE
  ```
  Sample sentence (implementation):
  ```ocaml
  while X1 do local_ X2 ; land X3 done
  ```
- Derivation (40 occurrences):
  ```
  implementation: . structure EOF
    structure: . seq_expr list(structure_element)
      seq_expr: . fun_seq_expr
        fun_seq_expr: . fun_expr
          fun_expr: . OVERWRITE ext seq_expr WITH fun_expr
  ```
  Sample sentence (implementation):
  ```ocaml
  overwrite_ local_ X1 ; land X2 with X3
  ```
- Derivation (40 occurrences):
  ```
  implementation: . structure EOF
    structure: . seq_expr list(structure_element)
      seq_expr: . fun_seq_expr
        fun_seq_expr: . fun_expr
          fun_expr: . TRY ext seq_expr WITH reversed_bar_llist(match_case)
  ```
  Sample sentence (implementation):
  ```ocaml
  try local_ X1 ; land X2 with false -> X3
  ```
- Derivation (40 occurrences):
  ```
  implementation: . structure EOF
    structure: . seq_expr list(structure_element)
      seq_expr: . fun_seq_expr
        fun_seq_expr: . fun_expr
          fun_expr: . fun_
            fun_: . FUN ext fun_params optional_atomic_constraint_ MINUSGREATER fun_body
  ```
  Sample sentence (implementation):
  ```ocaml
  fun ? ( x1 = local_ X2 ; land X3 ) -> X4
  ```
- ...


### Item `fun_expr: . fun_` (in 84 errors)

- Derivation (4 occurrences):
  ```
  simple_expr: mod_longident DOT LBRACKETBAR . separated_or_terminated_nonempty_list(SEMI,expr) BARRBRACKET
    separated_or_terminated_nonempty_list(SEMI,expr): . fun_expr
      fun_expr: . reversed_labeled_tuple_body
        reversed_labeled_tuple_body: . fun_expr COMMA fun_expr
          fun_expr: . fun_
  ```
  Sample sentence (implementation):
  ```ocaml
  X1 . [| fun false -> X2 ; , X3 |]
  ```
- Derivation (4 occurrences):
  ```
  comprehension_iterator: EQUAL FUNCTION ext reversed_bar_llist(match_case) direction_flag . fun_expr
    fun_expr: . reversed_labeled_tuple_body
      reversed_labeled_tuple_body: . fun_expr COMMA fun_expr
        fun_expr: . fun_
  ```
  Sample sentence (implementation):
  ```ocaml
  [: X1 for false = function false -> X2 downto fun false -> X3 ; , X4 :]
  ```
- Derivation (4 occurrences):
  ```
  simple_expr: mod_longident DOT LBRACKETBAR . separated_or_terminated_nonempty_list(SEMI,expr) BARRBRACKET
    separated_or_terminated_nonempty_list(SEMI,expr): . fun_expr
      fun_expr: . fun_expr COLONCOLON fun_expr
        fun_expr: . fun_
  ```
  Sample sentence (implementation):
  ```ocaml
  X1 . [| fun false -> X2 ; :: X3 |]
  ```
- Derivation (4 occurrences):
  ```
  comprehension_iterator: EQUAL FUNCTION ext reversed_bar_llist(match_case) direction_flag . fun_expr
    fun_expr: . fun_expr COLONCOLON fun_expr
      fun_expr: . fun_
  ```
  Sample sentence (implementation):
  ```ocaml
  [: X1 for false = function false -> X2 downto fun false -> X3 ; :: X4 :]
  ```
- Derivation (4 occurrences):
  ```
  simple_expr: mod_longident DOT LBRACKETBAR . separated_or_terminated_nonempty_list(SEMI,expr) BARRBRACKET
    separated_or_terminated_nonempty_list(SEMI,expr): . fun_expr
      fun_expr: . fun_expr COLONEQUAL fun_expr
        fun_expr: . fun_
  ```
  Sample sentence (implementation):
  ```ocaml
  X1 . [| fun false -> X2 ; := X3 |]
  ```
- Derivation (4 occurrences):
  ```
  comprehension_iterator: EQUAL FUNCTION ext reversed_bar_llist(match_case) direction_flag . fun_expr
    fun_expr: . fun_expr COLONEQUAL fun_expr
      fun_expr: . fun_
  ```
  Sample sentence (implementation):
  ```ocaml
  [: X1 for false = function false -> X2 downto fun false -> X3 ; := X4 :]
  ```
- Derivation (4 occurrences):
  ```
  simple_expr: mod_longident DOT LBRACKETBAR . separated_or_terminated_nonempty_list(SEMI,expr) BARRBRACKET
    separated_or_terminated_nonempty_list(SEMI,expr): . fun_expr
      fun_expr: . fun_expr AMPERAMPER fun_expr
        fun_expr: . fun_
  ```
  Sample sentence (implementation):
  ```ocaml
  X1 . [| fun false -> X2 ; && X3 |]
  ```
- Derivation (4 occurrences):
  ```
  comprehension_iterator: EQUAL FUNCTION ext reversed_bar_llist(match_case) direction_flag . fun_expr
    fun_expr: . fun_expr AMPERAMPER fun_expr
      fun_expr: . fun_
  ```
  Sample sentence (implementation):
  ```ocaml
  [: X1 for false = function false -> X2 downto fun false -> X3 ; && X4 :]
  ```
- Derivation (4 occurrences):
  ```
  simple_expr: mod_longident DOT LBRACKETBAR . separated_or_terminated_nonempty_list(SEMI,expr) BARRBRACKET
    separated_or_terminated_nonempty_list(SEMI,expr): . fun_expr
      fun_expr: . fun_expr BARBAR fun_expr
        fun_expr: . fun_
  ```
  Sample sentence (implementation):
  ```ocaml
  X1 . [| fun false -> X2 ; || X3 |]
  ```
- Derivation (4 occurrences):
  ```
  comprehension_iterator: EQUAL FUNCTION ext reversed_bar_llist(match_case) direction_flag . fun_expr
    fun_expr: . fun_expr BARBAR fun_expr
      fun_expr: . fun_
  ```
  Sample sentence (implementation):
  ```ocaml
  [: X1 for false = function false -> X2 downto fun false -> X3 ; || X4 :]
  ```
- Derivation (4 occurrences):
  ```
  simple_expr: mod_longident DOT LBRACKETBAR . separated_or_terminated_nonempty_list(SEMI,expr) BARRBRACKET
    separated_or_terminated_nonempty_list(SEMI,expr): . fun_expr
      fun_expr: . fun_expr EQUAL fun_expr
        fun_expr: . fun_
  ```
  Sample sentence (implementation):
  ```ocaml
  X1 . [| fun false -> X2 ; = X3 |]
  ```
- Derivation (4 occurrences):
  ```
  comprehension_iterator: EQUAL FUNCTION ext reversed_bar_llist(match_case) direction_flag . fun_expr
    fun_expr: . fun_expr EQUAL fun_expr
      fun_expr: . fun_
  ```
  Sample sentence (implementation):
  ```ocaml
  [: X1 for false = function false -> X2 downto fun false -> X3 ; = X4 :]
  ```
- Derivation (4 occurrences):
  ```
  simple_expr: mod_longident DOT LBRACKETBAR . separated_or_terminated_nonempty_list(SEMI,expr) BARRBRACKET
    separated_or_terminated_nonempty_list(SEMI,expr): . fun_expr
      fun_expr: . fun_expr INFIXOP4 fun_expr
        fun_expr: . fun_
  ```
  Sample sentence (implementation):
  ```ocaml
  X1 . [| fun false -> X2 ; ** X3 |]
  ```
- Derivation (4 occurrences):
  ```
  comprehension_iterator: EQUAL FUNCTION ext reversed_bar_llist(match_case) direction_flag . fun_expr
    fun_expr: . fun_expr INFIXOP4 fun_expr
      fun_expr: . fun_
  ```
  Sample sentence (implementation):
  ```ocaml
  [: X1 for false = function false -> X2 downto fun false -> X3 ; ** X4 :]
  ```
- Derivation (4 occurrences):
  ```
  simple_expr: mod_longident DOT LBRACKETBAR . separated_or_terminated_nonempty_list(SEMI,expr) BARRBRACKET
    separated_or_terminated_nonempty_list(SEMI,expr): . fun_expr
      fun_expr: . fun_expr INFIXOP3 fun_expr
        fun_expr: . fun_
  ```
  Sample sentence (implementation):
  ```ocaml
  X1 . [| fun false -> X2 ; land X3 |]
  ```
- Derivation (4 occurrences):
  ```
  simple_expr: mod_longident DOT LBRACKETBAR . separated_or_terminated_nonempty_list(SEMI,expr) BARRBRACKET
    separated_or_terminated_nonempty_list(SEMI,expr): . fun_expr
      fun_expr: . fun_expr AT fun_expr
        fun_expr: . fun_
  ```
  Sample sentence (implementation):
  ```ocaml
  X1 . [| fun false -> X2 ; @ X3 |]
  ```
- Derivation (4 occurrences):
  ```
  comprehension_iterator: EQUAL FUNCTION ext reversed_bar_llist(match_case) direction_flag . fun_expr
    fun_expr: . fun_expr AT fun_expr
      fun_expr: . fun_
  ```
  Sample sentence (implementation):
  ```ocaml
  [: X1 for false = function false -> X2 downto fun false -> X3 ; @ X4 :]
  ```
- Derivation (1 occurrence):
  ```
  paren_module_expr: LPAREN VAL . fun_expr RPAREN
    fun_expr: . reversed_labeled_tuple_body
      reversed_labeled_tuple_body: . fun_expr COMMA fun_expr
        fun_expr: . fun_
          fun_: . FUN ext nonempty_list(attribute) fun_params optional_atomic_constraint_ MINUSGREATER fun_body
  ```
  Sample sentence (implementation):
  ```ocaml
  include ( val fun [@ and ] false -> X1 ; , X2 )
  ```
- Derivation (1 occurrence):
  ```
  paren_module_expr: LPAREN VAL . fun_expr RPAREN
    fun_expr: . fun_expr COLONCOLON fun_expr
      fun_expr: . fun_
        fun_: . FUN ext nonempty_list(attribute) fun_params optional_atomic_constraint_ MINUSGREATER fun_body
  ```
  Sample sentence (implementation):
  ```ocaml
  include ( val fun [@ and ] false -> X1 ; :: X2 )
  ```
- Derivation (1 occurrence):
  ```
  paren_module_expr: LPAREN VAL . fun_expr RPAREN
    fun_expr: . fun_expr COLONEQUAL fun_expr
      fun_expr: . fun_
        fun_: . FUN ext nonempty_list(attribute) fun_params optional_atomic_constraint_ MINUSGREATER fun_body
  ```
  Sample sentence (implementation):
  ```ocaml
  include ( val fun [@ and ] false -> X1 ; := X2 )
  ```
- Derivation (1 occurrence):
  ```
  paren_module_expr: LPAREN VAL . fun_expr RPAREN
    fun_expr: . fun_expr AMPERAMPER fun_expr
      fun_expr: . fun_
        fun_: . FUN ext nonempty_list(attribute) fun_params optional_atomic_constraint_ MINUSGREATER fun_body
  ```
  Sample sentence (implementation):
  ```ocaml
  include ( val fun [@ and ] false -> X1 ; && X2 )
  ```
- ...


### Item `attr_id: . single_attr_id` (in 1 error)

- Derivation (1 occurrence):
  ```
  attribute: LBRACKETAT . attr_id attr_payload RBRACKET
    attr_id: . single_attr_id
      single_attr_id: . AND
  ```
  Sample sentence (implementation):
  ```ocaml
  include ( val [@ and ] stack_ function | false -> X1 ; , X2 )
  ```

## Error while parsing the output with upstream's parser: (53 errors)

### Item `fun_expr: . simple_expr` (in 48 errors)

- Derivation (38 occurrences):
  ```
  implementation: . structure EOF
    structure: . seq_expr list(structure_element)
      seq_expr: . fun_seq_expr
        fun_seq_expr: . fun_expr
          fun_expr: . simple_expr
            simple_expr: . HASHLPAREN reversed_labeled_tuple_body RPAREN
  ```
  Sample sentence (implementation):
  ```ocaml
  #( local_ X1 ; , X2 )
  ```
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
- Derivation (2 occurrences):
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


