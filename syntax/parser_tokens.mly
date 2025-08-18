/* Tokens */

/* The alias that follows each token is used by Menhir when it needs to
   produce a sentence (that is, a sequence of tokens) in concrete syntax. */

/* Some tokens represent multiple concrete strings. In most cases, an
   arbitrary concrete string can be chosen. In a few cases, one must
   be careful: e.g., in PREFIXOP and INFIXOP2, one must choose a concrete
   string that will not trigger a syntax error; see how [not_expecting]
   is used in the definition of [type_variance]. */

%token AMPERAMPER             "&&"
%token AMPERSAND              "&"
%token AND                    "and"
%token AS                     "as"
%token ASSERT                 "assert"
%token BACKQUOTE              "`"
%token BANG                   "!"
%token BAR                    "|"
%token BARBAR                 "||"
%token BARRBRACKET            "|]"
%token BEGIN                  "begin"
%token <char> CHAR            "'a'" (* just an example *)
%token CLASS                  "class"
%token COLON                  ":"
%token COLONCOLON             "::"
%token COLONEQUAL             ":="
%token COLONGREATER           ":>"
%token COLONRBRACKET          ":]"
%token COMMA                  ","
%token CONSTRAINT             "constraint"
%token DO                     "do"
%token DONE                   "done"
%token DOT                    "."
%token DOTDOT                 ".."
%token DOTHASH                ".#"
%token DOWNTO                 "downto"
%token ELSE                   "else"
%token END                    "end"
%token EOF                    ""
%token EQUAL                  "="
%token EXCEPTION              "exception"
%token EXCLAVE                "exclave_"
%token EXTERNAL               "external"
%token FALSE                  "false"
%token <string * char option> FLOAT       "42.0" (* just an example *)
%token <string * char option> HASH_FLOAT "#42.0" (* just an example *)
%token FOR                    "for"
%token FUN                    "fun"
%token FUNCTION               "function"
%token FUNCTOR                "functor"
%token GLOBAL                 "global_"
%token GREATER                ">"
%token GREATERRBRACE          ">}"
%token GREATERRBRACKET        ">]"
%token HASHLPAREN             "#("
%token HASHLBRACE             "#{"
%token IF                     "if"
%token IN                     "in"
%token INCLUDE                "include"
%token <string> INFIXOP0      "!="   (* just an example *)
%token AT                     "@"    (* mode expression *)
%token ATAT                   "@@"   (* mode expression *)
%token <string> INFIXOP1      "^"    (* just an example *)
%token <string> INFIXOP2      "+!"   (* chosen with care; see above *)
%token <string> INFIXOP3      "land" (* just an example *)
%token <string> INFIXOP4      "**"   (* just an example *)
%token <string> DOTOP         ".+"
%token <string> LETOP         "let*" (* just an example *)
%token <string> ANDOP         "and*" (* just an example *)
%token INHERIT                "inherit"
%token INITIALIZER            "initializer"
%token <string * char option> INT      "42"  (* just an example *)
%token <string * char option> HASH_INT "#42l" (* just an example *)
%token KIND_ABBREV            "kind_abbrev_"
%token KIND_OF                "kind_of_"
%token <string> LABEL         "~label:" (* just an example *)
%token LAZY                   "lazy"
%token LBRACE                 "{"
%token LBRACELESS             "{<"
%token LBRACKET               "["
%token LBRACKETBAR            "[|"
%token LBRACKETCOLON          "[:"
%token LBRACKETLESS           "[<"
%token LBRACKETGREATER        "[>"
%token LBRACKETPERCENT        "[%"
%token LBRACKETPERCENTPERCENT "[%%"
%token LESS                   "<"
%token LESSMINUS              "<-"
%token LET                    "let"
%token <string> LIDENT        "lident" (* just an example *)
%token LOCAL                  "local_"
%token LPAREN                 "("
%token LBRACKETAT             "[@"
%token LBRACKETATAT           "[@@"
%token LBRACKETATATAT         "[@@@"
%token MATCH                  "match"
%token METHOD                 "method"
%token MINUS                  "-"
%token MINUSDOT               "-."
%token MINUSGREATER           "->"
%token MOD                    "mod"
%token MODULE                 "module"
%token MUTABLE                "mutable"
%token NEW                    "new"
%token NONREC                 "nonrec"
%token OBJECT                 "object"
%token OF                     "of"
%token ONCE                   "once_"
%token OPEN                   "open"
%token <string> OPTLABEL      "?label:" (* just an example *)
%token OR                     "or"
%token OVERWRITE              "overwrite_"
/* %token PARSER              "parser" */
%token PERCENT                "%"
%token PLUS                   "+"
%token PLUSDOT                "+."
%token PLUSEQ                 "+="
%token <string> PREFIXOP      "!+" (* chosen with care; see above *)
%token PRIVATE                "private"
%token QUESTION               "?"
%token QUOTE                  "'"
%token RBRACE                 "}"
%token RBRACKET               "]"
%token REC                    "rec"
%token RPAREN                 ")"
%token SEMI                   ";"
%token SEMISEMI               ";;"
%token HASH                   "#"
%token HASH_SUFFIX            "# "
%token <string> HASHOP        "##" (* just an example *)
%token SIG                    "sig"
%token STACK                  "stack_"
%token STAR                   "*"
%token <string * Location.t * string option>
       STRING                 "\"hello\"" (* just an example *)
%token <string * Location.t * string * Location.t * string option>
       QUOTED_STRING_EXPR     "{%hello|world|}"  (* just an example *)
%token <string * Location.t * string * Location.t * string option>
       QUOTED_STRING_ITEM     "{%%hello|world|}" (* just an example *)
%token STRUCT                 "struct"
%token THEN                   "then"
%token TILDE                  "~"
%token TO                     "to"
%token TRUE                   "true"
%token TRY                    "try"
%token TYPE                   "type"
%token <string> UIDENT        "UIdent" (* just an example *)
%token UNDERSCORE             "_"
%token UNIQUE                 "unique_"
%token VAL                    "val"
%token VIRTUAL                "virtual"
%token WHEN                   "when"
%token WHILE                  "while"
%token WITH                   "with"
%token <string * Location.t> COMMENT    "(* comment *)"
%token <Docstring.t> DOCSTRING "(** documentation *)"

%token EOL                    "\\n"      (* not great, but EOL is unused *)

%%
