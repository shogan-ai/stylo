(**************************************************************************)
(* *)
(* OCaml *)
(* *)
(* Xavier Leroy, projet Cristal, INRIA Rocquencourt *)
(* *)
(* Copyright 1996 Institut National de Recherche en Informatique et *)
(* en Automatique. *)
(* *)
(* All rights reserved. This file is distributed under the terms of *)
(* the GNU Lesser General Public License version 2.1, with the *)
(* special exception on linking described in the file LICENSE. *)
(* *)
(**************************************************************************)

(** Abstract syntax tree produced by parsing

    {b Warning:} this module is unstable and part of {{!Compiler_libs} compiler-libs}. *)

(**************************************************************)
(* CR trefis: Longident and Asttypes (or parts of them) were inlined and every type has
   been made mutually recursive. Is there a way with ppx_traverse to generate keep the
   files separate (so it's easier to track upstream changes by diffing file by file),
   generate several traversals and use inheritance to bring it all together? *)

type token = Parser_tokens.token

type position = Lexing.position =
  { pos_fname : string
  ; pos_lnum : int
  ; pos_bol : int
  ; pos_cnum : int
  }
and location = Location.t =
  { loc_start : position
  ; loc_end : position
  ; loc_ghost : bool
  }
and longident_dotop_delims = Longident.dotop_delims =
  | Paren
  | Brace
  | Bracket
and longident_str_or_op = Longident.str_or_op =
  | Str of string
  | Str_trailing_hash of string (* FIXME? *)
  | Op of string
  | DotOp of string * longident_dotop_delims * string * bool
and longident_lid_desc = Longident.lid_desc =
  | Lident of longident_str_or_op
  | Ldot of longident * longident_str_or_op
  | Lapply of longident * longident
and longident = Longident.t =
  { desc : longident_lid_desc
  ; tokens : token_seq
  }
and attachment = Tokens.attachment =
  | Before
  | After
  | Floating
and comment = Tokens.comment =
  { text : string
  ; attachement : attachment
  ; explicitely_inserted : bool ref
  }
and token_desc = Tokens.desc =
  | Token of token * bool
  | Comment of comment
  | Child_node
and token_elt = Tokens.elt =
  { desc : token_desc
  ; pos : position
  }
and token_seq = token_elt list
and rec_flag =
  | Nonrecursive
  | Recursive
and direction_flag =
  | Upto
  | Downto
(* Order matters, used in polymorphic comparison *)
and private_flag =
  | Private
  | Public
and mutable_flag =
  | Immutable
  | Mutable
and virtual_flag =
  | Virtual
  | Concrete
and override_flag =
  | Override
  | Fresh
and closed_flag =
  | Closed
  | Open
and label = string
(** This is used only in the Parsetree. *)
and arg_label =
  | Nolabel
  | Labelled of string (** [label:T -> ...] *)
  | Optional of string (** [?label:T -> ...] *)

and 'a loc = 'a Location.loc =
  { txt : 'a
  ; loc : location
  }
and variance =
  | Covariant
  | Contravariant
  | NoVariance
and injectivity =
  | Injective
  | NoInjectivity
and index_kind =
  | Index_int
  | Index_unboxed_int64
  | Index_unboxed_int32
  | Index_unboxed_int16
  | Index_unboxed_int8
  | Index_unboxed_nativeint
and paren_kind =
  | Paren
  | Brace
  | Bracket
and constant =
  | Pconst_integer of string option * string * char option
    (** Integer constants such as [3] [-3] [-3l] [3L] [3n].

      Suffixes [[g-z][G-Z]] are accepted by the parser. Suffixes except ['l'], ['L'] and
      ['n'] are rejected by the typechecker *)
  | Pconst_unboxed_integer of string option * string * char
    (** Integer constants such as [#3] [#3l] [#3L] [#3n].

      A suffix [[g-z][G-Z]] is required by the parser. Suffixes except ['l'], ['L'] and
      ['n'] are rejected by the typechecker *)
  | Pconst_char of char * string (** Character such as ['c']. *)
  | Pconst_untagged_char of char * string (** Character such as [#'c']. *)
  | Pconst_string of string * location * string option
    (** Constant string such as ["constant"] or [{delim|other constant|delim}].

      The location span the content of the string, without the delimiters. *)
  | Pconst_float of string option * string * char option
    (** Float constant such as [3.4], [2e5] or [1.4e-4].

      Suffixes [g-z][G-Z] are accepted by the parser. Suffixes except ['s'] are rejected
      by the typechecker. *)
  | Pconst_unboxed_float of string option * string * char option
    (** Float constant such as [#3.4], [#2e5] or [#1.4e-4].

      Suffixes [g-z][G-Z] are accepted by the parser. Suffixes except ['s'] are rejected
      by the typechecker. *)

and modality = Modality of string [@@unboxed]
and modalities = modality loc list
and mode = Mode of string [@@unboxed]
and modes = mode loc list
and include_kind =
  | Structure
  | Functor

(** {1 Extension points} *)

(** Attributes such as [[\@id ARG]] and [[\@\@id ARG]].

    Metadata containers passed around within the AST. The compiler ignores unknown
    attributes. *)
and attribute =
  { attr_name : string list loc
  ; attr_payload : payload
  ; attr_loc : location
  ; attr_tokens : token_seq
  }
(** Extension points such as [[%id ARG] and [%%id ARG]].

    Sub-language placeholder -- rejected by the typechecker. *)
and extension = string list loc * payload * token_seq
(** [[%%id]] (** docstrings *) *)
and toplevel_extension =
  { te_pre_doc : string option
  ; te_ext : extension
  ; te_attrs : attributes
  ; te_post_doc : string option
  }
and attributes = attribute list
and payload =
  | PStr of structure
  | PSig of signature (** [: SIG] in an attribute or an extension point *)
  | PTyp of core_type (** [: T] in an attribute or an extension point *)
  | PPat of pattern * expression option
    (** [? P] or [? P when E], in an attribute or an extension point *)
  | PString of string * string
and ext_attribute =
  { pea_ext : string list loc option
  ; pea_attrs : attributes
  }

(** {1 Core language} *)
(** {2 Type expressions} *)


and core_type =
  { ptyp_desc : core_type_desc
  ; ptyp_loc : location
  ; ptyp_attributes : attributes (** [... [\@id1] [\@id2]] *)
  ; ptyp_tokens : token_seq
  }
and arrow_arg =
  { aa_lbl : arg_label
  ; aa_legacy_modes : modes
  ; aa_type : core_type
  ; aa_modes : modes
  ; aa_doc : string option
  ; aa_loc : location
  ; aa_tokens : token_seq
  }
and core_type_desc =
  | Ptyp_any of jkind_annotation option (** [_] or [_ : k] *)
  | Ptyp_var of string * jkind_annotation option
    (** A type variable such as ['a] or ['a : k] *)
  | Ptyp_arrow of
      { domain : arrow_arg
      ; codom_legacy_modes : modes
      ; codom_type : core_type
      ; codom_modes : modes
      }
    (** [Ptyp_arrow(lbl, T1, M1, T2, M2)] represents:
      - [T1 @ M1 -> T2 @ M2] when [lbl] is {{!arg_label.Nolabel} [Nolabel]},
      - [~l:(T1 @ M1) -> (T2 @ M2)] when [lbl] is {{!arg_label.Labelled} [Labelled]},
      - [?l:(T1 @ M1) -> (T2 @ M2)] when [lbl] is {{!arg_label.Optional} [Optional]}. *)
  | Ptyp_tuple of (string option * core_type) list
    (** [Ptyp_tuple(tl)] represents a product type:
      - [T1 * ... * Tn] when [tl] is [(None,T1);...;(None,Tn)]
      - [L1:T1 * ... * Ln:Tn] when [tl] is [(Some L1,T1);...;(Some Ln,Tn)]
      - A mix, e.g. [L1:T1 * T2] when [tl] is [(Some L1,T1);(None,T2)]

      Invariant: [n >= 2]. *)
  | Ptyp_unboxed_tuple of (string option * core_type) list
    (** Unboxed tuple types: [Ptyp_unboxed_tuple([(Some l1,P1);...;(Some l2,Pn)]] represents
      a product type [#(l1:T1 * ... * l2:Tn)], and the labels are optional.

      Invariant: [n >= 2]. *)
  | Ptyp_constr of core_type list * longident loc
    (** [Ptyp_constr(lident, l)] represents:
      - [tconstr] when [l=[]],
      - [T tconstr] when [l=[T]],
      - [(T1, ..., Tn) tconstr] when [l=[T1 ; ... ; Tn]]. *)
  | Ptyp_object of object_field list * closed_flag
    (** [Ptyp_object([ l1:T1; ...; ln:Tn ], flag)] represents:
      - [< l1:T1; ...; ln:Tn >] when [flag] is {{!Asttypes.closed_flag.Closed} [Closed]},
      - [< l1:T1; ...; ln:Tn; .. >] when [flag] is {{!Asttypes.closed_flag.Open} [Open]}. *)
  | Ptyp_class of longident loc * core_type list
    (** [Ptyp_class(tconstr, l)] represents:
      - [#tconstr] when [l=[]],
      - [T #tconstr] when [l=[T]],
      - [(T1, ..., Tn) #tconstr] when [l=[T1 ; ... ; Tn]]. *)
  | Ptyp_alias of core_type * string loc option * jkind_annotation option
    (** [T as 'a] or [T as ('a : k)] or [T as (_ : k)].

      Invariant: the name or jkind annotation is non-None. *)
  | Ptyp_variant of row_field list * closed_flag * label list option
    (** [Ptyp_variant([`A;`B], flag, labels)] represents:
      - [[ `A|`B ]] when [flag] is {{!Asttypes.closed_flag.Closed} [Closed]}, and [labels]
        is [None],
      - [[> `A|`B ]] when [flag] is {{!Asttypes.closed_flag.Open} [Open]}, and [labels] is
        [None],
      - [[< `A|`B ]] when [flag] is {{!Asttypes.closed_flag.Closed} [Closed]}, and
        [labels] is [Some []],
      - [[< `A|`B > `X `Y ]] when [flag] is {{!Asttypes.closed_flag.Closed} [Closed]}, and
        [labels] is [Some ["X";"Y"]]. *)
  | Ptyp_poly of (string loc * jkind_annotation option) list * core_type
    (** ['a1 ... 'an. T] [('a1 : k1) ... ('an : kn). T]

      Can only appear in the following context:

      - As the {!core_type} of a {{!pattern_desc.Ppat_constraint} [Ppat_constraint]} node
        corresponding to a constraint on a let-binding:
        {[
          let x : 'a1 ... 'an. T = e ...
        ]}

      - Under {{!class_field_kind.Cfk_virtual} [Cfk_virtual]} for methods (not values).

      - As the {!core_type} of a {{!class_type_field_desc.Pctf_method} [Pctf_method]}
        node.

      - As the {{!label_declaration.pld_type} [pld_type]} field of a {!label_declaration}.

      - As a {!core_type} of a {{!core_type_desc.Ptyp_object} [Ptyp_object]} node.

      - As the {{!value_description.pval_type} [pval_type]} field of a
        {!value_description}.

      - As the {!core_type} of a {{!function_param_desc.Pparam_val} [Param_val]}. *)
  | Ptyp_package of ext_attribute * package_type (** [(module S)]. *)
  | Ptyp_open of longident loc * core_type (** [M.(T)] *)
  | Ptyp_quote of core_type (** [<[T]>] *)
  | Ptyp_splice of core_type (** [$T] *)
  | Ptyp_of_kind of jkind_annotation (** [(type : k)] *)
  | Ptyp_extension of extension (** [[%id]]. *)
  | Ptyp_parens of core_type
and package_type = module_type
and row_field =
  { prf_desc : row_field_desc
  ; prf_loc : location
  ; prf_attributes : attributes
  ; prf_doc : string option
  ; prf_tokens : token_seq
  }
and row_field_desc =
  | Rtag of string loc * bool * core_type list
    (** [Rtag(`A, b, l)] represents:
      - [`A] when [b] is [true] and [l] is [[]],
      - [`A of T] when [b] is [false] and [l] is [[T]],
      - [`A of T1 & .. & Tn] when [b] is [false] and [l] is [[T1;...Tn]],
      - [`A of & T1 & .. & Tn] when [b] is [true] and [l] is [[T1;...Tn]].

      - The [bool] field is true if the tag contains a constant (empty) constructor.
      - [&] occurs when several types are used for the same constructor (see 4.2 in the
        manual) *)
  | Rinherit of core_type (** [[ | t ]] *)

and object_field =
  { pof_desc : object_field_desc
  ; pof_loc : location
  ; pof_attributes : attributes
  ; pof_doc : string option
  ; pof_tokens : token_seq
  }
and object_field_desc =
  | Otag of string loc * core_type
  | Oinherit of core_type

(** {2 Patterns} *)


and pattern =
  { ppat_ext_attr : ext_attribute
  ; ppat_desc : pattern_desc
  ; ppat_loc : location
  ; ppat_attributes : attributes (** [... [\@id1] [\@id2]] *)
  ; ppat_tokens : token_seq
  }
and pattern_desc =
  | Ppat_any (** The pattern [_]. *)
  | Ppat_var of longident_str_or_op loc (** A variable pattern such as [x] *)
  | Ppat_alias of pattern * longident_str_or_op loc
    (** An alias pattern such as [P as 'a] *)
  | Ppat_constant of constant
    (** Patterns such as [1], ['a'], ["true"], [1.0], [1l], [1L], [1n] *)
  | Ppat_interval of constant * constant
    (** Patterns such as ['a'..'z'].

      Other forms of interval are recognized by the parser but rejected by the
      type-checker. *)
  | Ppat_tuple of pattern argument list * closed_flag
    (** [Ppat_tuple(pl, Closed)] represents
      - [(P1, ..., Pn)] when [pl] is [(None, P1);...;(None, Pn)]
      - [(~L1:P1, ..., ~Ln:Pn)] when [pl] is [(Some L1, P1);...;(Some Ln, Pn)]
      - A mix, e.g. [(~L1:P1, P2)] when [pl] is [(Some L1, P1);(None, P2)]
      - If pattern is open, then it also ends in a [..]

      Invariant:
      - If Closed, [n >= 2].
      - If Open, [n >= 1]. *)
  | Ppat_unboxed_tuple of pattern argument list * closed_flag
    (** Unboxed tuple patterns: [#(l1:P1, ..., ln:Pn)] is
      [([(Some l1,P1);...;(Some l2,Pn)], Closed)], and the labels are optional. An [Open]
      pattern ends in [..].

      Invariant:
      - If Closed, [n >= 2]
      - If Open, [n >= 1] *)
  | Ppat_construct of
      longident loc
      *
      ((string loc * jkind_annotation option) list * pattern) option
    (** [Ppat_construct(C, args)] represents:
      - [C] when [args] is [None],
      - [C P] when [args] is [Some ([], P)]
      - [C (P1, ..., Pn)] when [args] is [Some ([], Ppat_tuple [P1; ...; Pn])]
      - [C (type a b) P] when [args] is [Some ([a, None; b, None], P)]
      - [C (type (a : k) b) P] when [args] is [Some ([a, Some k; b, None], P)] *)
  | Ppat_variant of label * pattern option
    (** [Ppat_variant(`A, pat)] represents:
      - [`A] when [pat] is [None],
      - [`A P] when [pat] is [Some P] *)
  | Ppat_record of pattern record_field list * closed_flag
    (** [Ppat_record([(l1, P1) ; ... ; (ln, Pn)], flag)] represents:
      - [{ l1=P1; ...; ln=Pn }] when [flag] is {{!Asttypes.closed_flag.Closed} [Closed]}
      - [{ l1=P1; ...; ln=Pn; _}] when [flag] is {{!Asttypes.closed_flag.Open} [Open]}

      Invariant: [n > 0] *)
  | Ppat_record_unboxed_product of pattern record_field list * closed_flag
    (** [Ppat_record_unboxed_product([(l1, P1) ; ... ; (ln, Pn)], flag)] represents:
      - [#{ l1=P1; ...; ln=Pn }] when [flag] is {{!Asttypes.closed_flag.Closed} [Closed]}
      - [#{ l1=P1; ...; ln=Pn; _}] when [flag] is {{!Asttypes.closed_flag.Open} [Open]}

      Invariant: [n > 0] *)
  | Ppat_array of mutable_flag * pattern list
    (** Pattern [[| P1; ...; Pn |]] or [[: P1; ...; Pn :]] *)
  | Ppat_or of pattern * pattern (** Pattern [P1 | P2] *)
  | Ppat_constraint of pattern * core_type option * modes
    (** [Ppat_constraint(tyopt, modes)] represents:
      - [(P : ty @@ modes)] when [tyopt] is [Some ty]
      - [(P @ modes)] when [tyopt] is [None] *)
  | Ppat_type of longident loc (** Pattern [#tconst] *)
  | Ppat_lazy of pattern (** Pattern [lazy P] *)
  | Ppat_unpack of string option loc * package_type option
    (** [Ppat_unpack(s)] represents:
      - [(module P)] when [s] is [Some "P"]
      - [(module _)] when [s] is [None] *)
  | Ppat_exception of pattern (** Pattern [exception P] *)
  | Ppat_extension of extension (** Pattern [[%id]] *)
  | Ppat_open of longident loc * pattern (** Pattern [M.(P)] *)
  | Ppat_parens of
      { pat : pattern
      ; optional : bool
      }
  | Ppat_list of pattern list
  | Ppat_cons of pattern * pattern

(** {2 Value expressions} *)


and expression =
  { pexp_ext_attr : ext_attribute
  ; pexp_desc : expression_desc
  ; pexp_loc : location
  ; pexp_attributes : attributes (** [... [\@id1] [\@id2]] *)
  ; pexp_tokens : token_seq
  }
and expression_desc =
  | Pexp_ident of longident loc (** Identifiers such as [x] and [M.x] *)
  | Pexp_constant of constant
    (** Expressions constant such as [1], ['a'], ["true"], [1.0], [1l], [1L], [1n] *)
  | Pexp_let of mutable_flag * rec_flag * value_binding list * expression
    (** [Pexp_let(flag, [(P1,E1) ; ... ; (Pn,En)], E)] represents:
      - [let P1 = E1 and ... and Pn = EN in E] when [flag] is
        {{!Asttypes.rec_flag.Nonrecursive} [Nonrecursive]},
      - [let rec P1 = E1 and ... and Pn = EN in E] when [flag] is
        {{!Asttypes.rec_flag.Recursive} [Recursive]}. *)
  | Pexp_function of function_param list * function_constraint * function_body
    (** [Pexp_function ([P1; ...; Pn], C, body)] represents any construct involving [fun] or
      [function], including:
      - [fun P1 ... Pn -> E] when [body = Pfunction_body E]
      - [fun P1 ... Pn -> function p1 -> e1 | ... | pm -> em] when
        [body = Pfunction_cases [ p1 -> e1; ...; pm -> em ]] [C] represents a type
        constraint or coercion placed immediately before the arrow, e.g.
        [fun P1 ... Pn : ty -> ...] when [C = Some (Pconstraint ty)].

      A function must have parameters. [Pexp_function (params, _, body)] must have
      non-empty [params] or a [Pfunction_cases _] body. *)
  | Pexp_prefix_apply of expression * expression
  | Pexp_add_or_sub of string * expression
  | Pexp_infix_apply of
      { arg1 : expression
      ; op : expression
      ; arg2 : expression
      }
  | Pexp_apply of expression * expression argument list
    (** [Pexp_apply(E0, [(l1, E1) ; ... ; (ln, En)])] represents [E0 ~l1:E1 ... ~ln:En]

      [li] can be {{!arg_label.Nolabel} [Nolabel]} (non labeled argument),
      {{!arg_label.Labelled} [Labelled]} (labelled arguments) or {{!arg_label.Optional}
      [Optional]} (optional argument).

      Invariant: [n > 0] *)
  | Pexp_match of expression * case list
    (** [match E0 with P1 -> E1 | ... | Pn -> En] *)
  | Pexp_try of expression * case list
    (** [try E0 with P1 -> E1 | ... | Pn -> En] *)
  | Pexp_tuple of expression argument list
    (** [Pexp_tuple(el)] represents
      - [(E1, ..., En)] when [el] is [(None, E1);...;(None, En)]
      - [(~L1:E1, ..., ~Ln:En)] when [el] is [(Some L1, E1);...;(Some Ln, En)]
      - A mix, e.g.: [(~L1:E1, E2)] when [el] is [(Some L1, E1); (None, E2)]

      Invariant: [n >= 2] *)
  | Pexp_unboxed_tuple of expression argument list
    (** Unboxed tuple expressions: [Pexp_unboxed_tuple([(Some l1,P1);...;(Some l2,Pn)])]
      represents [#(l1:E1, ..., ln:En)], and the labels are optional.

      Invariant: [n >= 2] *)
  | Pexp_construct of longident loc * expression option
    (** [Pexp_construct(C, exp)] represents:
      - [C] when [exp] is [None],
      - [C E] when [exp] is [Some E],
      - [C (E1, ..., En)] when [exp] is [Some (Pexp_tuple[E1;...;En])] *)
  | Pexp_variant of label * expression option
    (** [Pexp_variant(`A, exp)] represents
      - [`A] when [exp] is [None]
      - [`A E] when [exp] is [Some E] *)
  | Pexp_record of expression option * expression record_field list
    (** [Pexp_record(exp0, [(l1,P1) ; ... ; (ln,Pn)])] represents
      - [{ l1=P1; ...; ln=Pn }] when [exp0] is [None]
      - [{ E0 with l1=P1; ...; ln=Pn }] when [exp0] is [Some E0]

      Invariant: [n > 0] *)
  | Pexp_record_unboxed_product of
      expression option
      *
      expression record_field list
    (** [Pexp_record_unboxed_product(exp0, [(l1,P1) ; ... ; (ln,Pn)])] represents
      - [#{ l1=P1; ...; ln=Pn }] when [exp0] is [None]
      - [#{ E0 with l1=P1; ...; ln=Pn }] when [exp0] is [Some E0]

      Invariant: [n > 0] *)
  | Pexp_field of expression * longident loc (** [E.l] *)
  | Pexp_unboxed_field of expression * longident loc (** [E.#l] *)
  | Pexp_setfield of expression * longident loc * expression (** [E1.l <- E2] *)
  | Pexp_array of mutable_flag * expression list
    (** [[| E1; ...; En |]] or [[: E1; ...; En :]] *)
  | Pexp_idx of block_access * unboxed_access list
    (** [(BA1 UA1 UA2 ...)] e.g. [(.foo.#bar.#baz)] Above, BA1=.foo, UA1=.#bar, and
      UA2=#.baz *)
  | Pexp_ifthenelse of expression * expression * expression option
    (** [if E1 then E2 else E3] *)
  | Pexp_sequence of expression * expression (** [E1; E2] *)
  | Pexp_seq_empty of expression (** [E1;] *)
  | Pexp_while of expression * expression (** [while E1 do E2 done] *)
  | Pexp_for of pattern * expression * expression * direction_flag * expression
    (** [Pexp_for(i, E1, E2, direction, E3)] represents:
      - [for i = E1 to E2 do E3 done] when [direction] is {{!Asttypes.direction_flag.Upto}
        [Upto]}
      - [for i = E1 downto E2 do E3 done] when [direction] is
        {{!Asttypes.direction_flag.Downto} [Downto]} *)
  | Pexp_constraint of expression * core_type option * modes
    (** [(E : T @@ modes)] *)
  | Pexp_coerce of expression * core_type option * core_type
    (** [Pexp_coerce(E, from, T)] represents
      - [(E :> T)] when [from] is [None],
      - [(E : T0 :> T)] when [from] is [Some T0]. *)
  | Pexp_send of expression * string loc (** [E # m] *)
  | Pexp_new of longident loc (** [new M.c] *)
  | Pexp_setvar of string loc * expression (** [x <- 2] *)
  | Pexp_override of (string loc * expression option) list
    (** [{< x1 = E1; ...; xn = En >}] *)
  | Pexp_letmodule of module_binding * expression
    (** [let module M = ME in E] *)
  | Pexp_letexception of extension_constructor * expression
    (** [let exception C in E] *)
  | Pexp_assert of expression
    (** [assert E].

      Note: [assert false] is treated in a special way by the type-checker. *)
  | Pexp_lazy of expression (** [lazy E] *)
  | Pexp_object of class_structure (** [object ... end] *)
  | Pexp_pack of module_expr * package_type option
    (** [(module ME)].

      [(module ME : S)] is represented as [Pexp_constraint(Pexp_pack ME, Ptyp_package S)] *)
  | Pexp_dot_open of longident loc * expression (** - [M.(E)] *)
  | Pexp_let_open of open_declaration * expression
    (** - [let open M in E]
      - [let open! M in E] *)
  | Pexp_letop of letop
    (** - [let* P = E0 in E1]
                              - [let* P0 = E00 and* P1 = E01 in E1] *)
  | Pexp_extension of extension (** [[%id]] *)
  | Pexp_unreachable (** [.] *)
  | Pexp_stack of expression (** stack_ exp *)
  | Pexp_comprehension of comprehension_expression
    (** [[? BODY ...CLAUSES... ?]], where:
      - [?] is either [""] (list), [:] (immutable array), or [|] (array).
      - [BODY] is an expression.
      - [CLAUSES] is a series of [comprehension_clause]. *)
  | Pexp_overwrite of expression * expression (** overwrite_ exp with exp *)
  | Pexp_quote of expression (** runtime metaprogramming quotations <[E]> *)
  | Pexp_splice of expression (** runtime metaprogramming splicing $(E) *)
  | Pexp_hole (** _ *)
  | Pexp_index_op of
      { kind : paren_kind
      ; op : (longident option * string) option
      ; seq : expression
      ; indices : expression list
      ; assign : expression option
      }
  | Pexp_parens of
      { exp : expression
      ; optional : bool
      }
  | Pexp_begin_end of expression option
  | Pexp_list of expression list
  | Pexp_cons of expression * expression
  | Pexp_exclave of expression
  | Pexp_mode_legacy of mode loc * expression
and 'a record_field =
  { field_name : longident loc
  ; typ : type_constraint option
  ; value : 'a option
  }
(** Values of type {!case} represents [(P -> E)] or [(P when E0 -> E)] *)
and case =
  { pc_lhs : pattern
  ; pc_guard : expression option
  ; pc_rhs : expression
  ; pc_tokens : token_seq
  }
and letop =
  { let_ : binding_op
  ; ands : binding_op list
  ; body : expression
  }
and binding_op =
  { pbop_op : string loc
  ; pbop_binding : value_binding
  ; pbop_loc : location
  }
and 'a argument_desc =
  | Parg_unlabelled of
      { legacy_modes : modes
      ; arg : 'a
      ; typ_constraint : type_constraint option
      ; modes : modes
      }
  | Parg_labelled of
      { optional : bool
      ; legacy_modes : modes
      ; name : string
      ; maybe_punned : 'a option
      ; typ_constraint : type_constraint option
      ; modes : modes
      ; default : expression option
      }
and 'a argument =
  { parg_desc : 'a argument_desc
  ; parg_tokens : token_seq
  }
and function_param_desc =
  | Pparam_val of pattern argument
    (** [Pparam_val (lbl, exp0, P)] represents the parameter:
      - [P] when [lbl] is {{!Asttypes.arg_label.Nolabel} [Nolabel]} and [exp0] is [None]
      - [~l:P] when [lbl] is {{!Asttypes.arg_label.Labelled} [Labelled l]} and [exp0] is
        [None]
      - [?l:P] when [lbl] is {{!Asttypes.arg_label.Optional} [Optional l]} and [exp0] is
        [None]
      - [?l:(P = E0)] when [lbl] is {{!Asttypes.arg_label.Optional} [Optional l]} and
        [exp0] is [Some E0]

      Note: If [E0] is provided, only {{!Asttypes.arg_label.Optional} [Optional]} is
      allowed. *)
  | Pparam_newtype of string loc * jkind_annotation option
  | Pparam_newtypes of (string loc * jkind_annotation option) list
    (** [Pparam_newtype x] represents the parameter [(type x)]. [x] carries the location of
      the identifier, whereas the [pparam_loc] on the enclosing [function_param] node is
      the location of the [(type x)] as a whole.

      Multiple parameters [(type a b c)] are represented as multiple [Pparam_newtype]
      nodes, let's say:

      {[
        [ { pparam_kind = Pparam_newtype a; pparam_loc = loc1 }
        ; { pparam_kind = Pparam_newtype b; pparam_loc = loc2 }
        ; { pparam_kind = Pparam_newtype c; pparam_loc = loc3 }
        ]
      ]}

      Here, the first loc [loc1] is the location of [(type a b c)], and the subsequent
      locs [loc2] and [loc3] are the same as [loc1], except marked as ghost locations. The
      locations on [a], [b], [c], correspond to the variables [a], [b], and [c] in the
      source code. *)

and function_param =
  { pparam_loc : location
  ; pparam_desc : function_param_desc
  }
and function_body =
  { pfb_desc : function_body_desc
  ; pfb_loc : location
  ; pfb_tokens : token_seq
  }
(** See the comment on {{!expression_desc.Pexp_function} [Pexp_function]}. *)
and function_body_desc =
  | Pfunction_body of expression
  | Pfunction_cases of case list * ext_attribute
    (** In [Pfunction_cases (_, loc, attrs)], the location extends from the start of the
      [function] keyword to the end of the last case. The compiler will only use
      typechecking-related attributes from [attrs], e.g. enabling or disabling a warning. *)

and type_constraint =
  | Pconstraint of core_type
  | Pcoerce of core_type option * core_type
    (** See the comment on {{!expression_desc.Pexp_function} [Pexp_function]}. *)

(** See the comment on {{!expression_desc.Pexp_function} [Pexp_function]}. *)
and function_constraint =
  { ret_mode_annotations : modes
       (** The mode annotation placed on a function's body, e.g.
      [let f x : int -> int @@ local = ...]. This field constrains the mode of function's
      body. *)
  ; ret_type_constraint : type_constraint option
       (** The type constraint placed on a function's body. *)
  }
and block_access =
  | Baccess_field of longident loc (** [.foo] *)
  | Baccess_array of mutable_flag * index_kind * expression
    (** Mutable array accesses: [.(E)], [.L(E)], [.l(E)], [.n(E)] Immutable array accesses:
      [.:(E)], [.:L(E)], [.:l(E)], [.:n(E)]

      Indexed by [int], [int64#], [int32#], or [nativeint#], respectively. *)
  | Baccess_block of mutable_flag * expression
    (** Access using another block index: [.idx_imm(E)], [.idx_mut(E)] (usually followed by
      unboxed accesses, to deepen the index). *)

and unboxed_access =
  | Uaccess_unboxed_field of longident loc (** [.#foo] *)

and comprehension_iterator =
  | Pcomp_range of
      { start : expression
      ; stop : expression
      ; direction : direction_flag
      }
    (** "= START to STOP" (direction = Upto) "= START downto STOP" (direction = Downto) *)
  | Pcomp_in of expression (** "in EXPR" *)

(** [@...] PAT (in/=) ... *)
and comprehension_clause_binding =
  { pcomp_cb_mode : mode loc option
  ; pcomp_cb_pattern : pattern
  ; pcomp_cb_iterator : comprehension_iterator
  ; pcomp_cb_attributes : attributes
  ; pcomp_cb_tokens : token_seq
  }
and comprehension_clause =
  | Pcomp_for of comprehension_clause_binding list
    (** "for PAT (in/=) ... and PAT (in/=) ... and ..."; must be nonempty *)
  | Pcomp_when of expression (** "when EXPR" *)

and comprehension =
  { pcomp_body : expression (** The body/generator of the comprehension *)
  ; pcomp_clauses : comprehension_clause list
       (** The clauses of the comprehension; must be nonempty *)
  ; pcomp_tokens : token_seq
  }
and comprehension_expression =
  | Pcomp_list_comprehension of comprehension (** [[BODY ...CLAUSES...]] *)
  | Pcomp_array_comprehension of mutable_flag * comprehension
    (** [[|BODY ...CLAUSES...|]] (flag = Mutable) [[:BODY ...CLAUSES...:]] (flag =
      Immutable) (only allowed with [-extension immutable_arrays]) *)

(** {2 Value descriptions} *)

(** Values of type {!value_description} represents:
    - [val x: T], when {{!value_description.pval_prim} [pval_prim]} is [[]]
    - [external x: T = "s1" ... "sn"] when {{!value_description.pval_prim} [pval_prim]} is
      [["s1";..."sn"]] *)
and value_description =
  { pval_pre_doc : string option
  ; pval_ext_attrs : ext_attribute
  ; pval_name : longident_str_or_op loc
  ; pval_type : core_type
  ; pval_modalities : modalities
  ; pval_prim : string list
  ; pval_attributes : attributes (** [... [\@\@id1] [\@\@id2]] *)
  ; pval_post_doc : string option
  ; pval_loc : location
  ; pval_tokens : token_seq
  }

(** {2 Type declarations} *)


and ptype_param =
  { ptp_typ : core_type
  ; ptp_infos : variance * injectivity
  ; ptp_tokens : token_seq
  }
and ptype_params = ptype_param list
and ptype_constraint = core_type * core_type * location
(** Here are type declarations and their representation, for various
    {{!type_declaration.ptype_kind} [ptype_kind]} and {{!type_declaration.ptype_manifest}
    [ptype_manifest]} values:
    - [type t] when [type_kind] is {{!type_kind.Ptype_abstract} [Ptype_abstract]}, and
      [manifest] is [None],
    - [type t = T0] when [type_kind] is {{!type_kind.Ptype_abstract} [Ptype_abstract]},
      and [manifest] is [Some T0],
    - [type t = C of T | ...] when [type_kind] is {{!type_kind.Ptype_variant}
      [Ptype_variant]}, and [manifest] is [None],
    - [type t = T0 = C of T | ...] when [type_kind] is {{!type_kind.Ptype_variant}
      [Ptype_variant]}, and [manifest] is [Some T0],
    - [type t = {l: T; ...}] when [type_kind] is {{!type_kind.Ptype_record}
      [Ptype_record]}, and [manifest] is [None],
    - [type t = T0 = {l : T; ...}] when [type_kind] is {{!type_kind.Ptype_record}
      [Ptype_record]}, and [manifest] is [Some T0],
    - [type t = ..] when [type_kind] is {{!type_kind.Ptype_open} [Ptype_open]}, and
      [manifest] is [None]. *)
and type_declaration =
  { ptype_pre_text : string list
  ; ptype_pre_doc : string option
  ; ptype_ext_attrs : ext_attribute
  ; ptype_name : string loc
  ; ptype_params : ptype_params (** [('a1,...'an) t] *)
  ; ptype_jkind_annotation : jkind_annotation option (** for [: jkind] *)
  ; ptype_private : private_flag (** for [= private ...] *)
  ; ptype_manifest : core_type option (** represents [= T] *)
  ; ptype_kind : type_kind
  ; ptype_cstrs : ptype_constraint list
       (** [... constraint T1=T1'  ... constraint Tn=Tn'] *)
  ; ptype_attributes : attributes (** [... [\@\@id1] [\@\@id2]] *)
  ; ptype_post_doc : string option
  ; ptype_loc : location
  ; ptype_tokens : token_seq
  }
and type_kind =
  | Ptype_abstract
  | Ptype_variant of constructor_declaration list
  | Ptype_record of label_declaration list (** Invariant: non-empty list *)
  | Ptype_record_unboxed_product of label_declaration list
    (** Invariant: non-empty list *)
  | Ptype_open
(** - [{ ...; l: T; ... }] when {{!label_declaration.pld_mutable} [pld_mutable]} is
      {{!Asttypes.mutable_flag.Immutable} [Immutable]},
    - [{ ...; mutable l: T; ... }] when {{!label_declaration.pld_mutable} [pld_mutable]}
      is {{!Asttypes.mutable_flag.Mutable} [Mutable]}.

    Note: [T] can be a {{!core_type_desc.Ptyp_poly} [Ptyp_poly]}. *)
and label_declaration =
  { pld_name : string loc
  ; pld_mutable : mutable_flag
  ; pld_global : bool
  ; pld_modalities : modalities
  ; pld_type : core_type
  ; pld_loc : location
  ; pld_attributes : attributes (** [l : T [\@id1] [\@id2]] *)
  ; pld_doc : string option
  ; pld_tokens : token_seq
  }
and constructor_declaration =
  { pcd_name : longident_str_or_op loc
  ; pcd_vars : (string loc * jkind_annotation option) list
       (** jkind annotations are [C : ('a : kind1) ('a2 : kind2). ...] *)
  ; pcd_args : constructor_arguments
  ; pcd_res : core_type option
  ; pcd_loc : location
  ; pcd_attributes : attributes (** [C of ... [\@id1] [\@id2]] *)
  ; pcd_doc : string option
  ; pcd_tokens : token_seq
  }
and constructor_argument =
  { pca_global : bool
  ; pca_type : core_type
  ; pca_modalities : modalities
  ; pca_loc : location
  }
and constructor_arguments =
  | Pcstr_tuple of constructor_argument list
  | Pcstr_record of label_declaration list
    (** Values of type {!constructor_declaration} represents the constructor arguments of:
      - [C of T1 * ... * Tn] when [res = None], and [args = Pcstr_tuple [T1; ... ; Tn]],
      - [C: T0] when [res = Some T0], and [args = Pcstr_tuple []],
      - [C: T1 * ... * Tn -> T0] when [res = Some T0], and
        [args = Pcstr_tuple [T1; ... ; Tn]],
      - [C of {...}] when [res = None], and [args = Pcstr_record [...]],
      - [C: {...} -> T0] when [res = Some T0], and [args = Pcstr_record [...]]. *)

(** Definition of new extensions constructors for the extensive sum type [t]
    ([type t += ...]). *)
and type_extension =
  { ptyext_pre_doc : string option
  ; ptyext_ext_attrs : ext_attribute
  ; ptyext_path : longident loc
  ; ptyext_params : ptype_param list
  ; ptyext_constructors : extension_constructor list
  ; ptyext_private : private_flag
  ; ptyext_loc : location
  ; ptyext_attributes : attributes (** ... [\@\@id1] [\@\@id2] *)
  ; ptyext_post_doc : string option
  ; ptyext_tokens : token_seq
  }
and extension_constructor =
  { pext_name : longident_str_or_op loc
  ; pext_kind : extension_constructor_kind
  ; pext_loc : location
  ; pext_attributes : attributes (** [C of ... [\@id1] [\@id2]] *)
  ; pext_doc : string option
  ; pext_tokens : token_seq
  }
(** Definition of a new exception ([exception E]). *)
and type_exception =
  { ptyexn_pre_doc : string option
  ; ptyexn_ext_attrs : ext_attribute
  ; ptyexn_constructor : extension_constructor
  ; ptyexn_loc : location
  ; ptyexn_attributes : attributes (** [... [\@\@id1] [\@\@id2]] *)
  ; ptyexn_post_doc : string option
  ; ptyexn_tokens : token_seq
  }
and extension_constructor_kind =
  | Pext_decl of
      (string loc * jkind_annotation option) list
      *
      constructor_arguments
      *
      core_type option
    (** [Pext_decl(existentials, c_args, t_opt)] describes a new extension constructor. It
      can be:
      - [C of T1 * ... * Tn] when:
        - [existentials] is [[]],
        - [c_args] is [[T1; ...; Tn]],
        - [t_opt] is [None].
      - [C: T0] when
        - [existentials] is [[]],
        - [c_args] is [[]],
        - [t_opt] is [Some T0].
      - [C: T1 * ... * Tn -> T0] when
        - [existentials] is [[]],
        - [c_args] is [[T1; ...; Tn]],
        - [t_opt] is [Some T0].
      - [C: ('a : k)... . T1 * ... * Tn -> T0] when
        - [existentials] is [[('a : k);...]],
        - [c_args] is [[T1; ... ; Tn]],
        - [t_opt] is [Some T0]. *)
  | Pext_rebind of longident loc
    (** [Pext_rebind(D)] re-export the constructor [D] with the new name [C] *)

(** {1 Class language} *)
(** {2 Type expressions for the class language} *)


and class_type =
  { pcty_desc : class_type_desc
  ; pcty_loc : location
  ; pcty_attributes : attributes (** [... [\@id1] [\@id2]] *)
  ; pcty_tokens : token_seq
  }
and class_type_desc =
  | Pcty_constr of longident loc * core_type list
    (** - [c]
                                                        - [['a1, ..., 'an] c] *)
  | Pcty_signature of class_signature (** [object ... end] *)
  | Pcty_arrow of arrow_arg * class_type
    (** [Pcty_arrow(lbl, T, CT)] represents:
      - [T -> CT] when [lbl] is {{!arg_label.Nolabel} [Nolabel]},
      - [~l:T -> CT] when [lbl] is {{!arg_label.Labelled} [Labelled l]},
      - [?l:T -> CT] when [lbl] is {{!arg_label.Optional} [Optional l]}. *)
  | Pcty_extension of extension (** [%id] *)
  | Pcty_open of open_description * class_type (** [let open M in CT] *)

(** Values of type [class_signature] represents:
    - [object('selfpat) ... end]
    - [object ... end] when {{!class_signature.pcsig_self} [pcsig_self]} is
      {{!core_type_desc.Ptyp_any} [Ptyp_any]} *)
and class_signature =
  { pcsig_self : core_type option
  ; pcsig_fields : class_type_field list
  }
and class_type_field =
  { pctf_pre_doc : string option
  ; pctf_desc : class_type_field_desc
  ; pctf_loc : location
  ; pctf_attributes : attributes (** [... [\@\@id1] [\@\@id2]] *)
  ; pctf_post_doc : string option
  ; pctf_tokens : token_seq
  }
and class_type_field_desc =
  | Pctf_inherit of class_type (** [inherit CT] *)
  | Pctf_val of (string loc * mutable_flag * virtual_flag * core_type)
    (** [val x: T] *)
  | Pctf_method of (string loc * private_flag * virtual_flag * core_type)
    (** [method x: T]

      Note: [T] can be a {{!core_type_desc.Ptyp_poly} [Ptyp_poly]}. *)
  | Pctf_constraint of (core_type * core_type) (** [constraint T1 = T2] *)
  | Pctf_attribute of attribute (** [[\@\@\@id]] *)
  | Pctf_extension of extension (** [[%%id]] *)
  | Pctf_docstring of string
(** Values of type [class_expr class_infos] represents:
    - [class c = ...]
    - [class ['a1,...,'an] c = ...]
    - [class virtual c = ...]

    They are also used for "class type" declaration. *)
and 'a class_infos =
  { pci_pre_text : string list
  ; pci_pre_doc : string option
  ; pci_virt : virtual_flag
  ; pci_ext_attrs : ext_attribute
  ; pci_params : ptype_param list
  ; pci_name : string loc
  ; pci_value_params : pattern argument list
  ; pci_constraint : class_type option
  ; pci_expr : 'a
  ; pci_loc : location
  ; pci_attributes : attributes (** [... [\@\@id1] [\@\@id2]] *)
  ; pci_post_doc : string option
  ; pci_tokens : token_seq
  }
and class_description = class_type class_infos
and class_type_declaration = class_type class_infos

(** {2 Value expressions for the class language} *)


and class_expr =
  { pcl_ext_attrs : ext_attribute
  ; pcl_desc : class_expr_desc
  ; pcl_loc : location
  ; pcl_attributes : attributes (** [... [\@id1] [\@id2]] *)
  }
and class_expr_desc =
  | Pcl_constr of longident loc * core_type list
    (** [c] and [['a1, ..., 'an] c] *)
  | Pcl_structure of class_structure (** [object ... end] *)
  | Pcl_fun of pattern argument list * class_expr
    (** [Pcl_fun(lbl, exp0, P, CE)] represents:
      - [fun P -> CE] when [lbl] is {{!arg_label.Nolabel} [Nolabel]} and [exp0] is [None],
      - [fun ~l:P -> CE] when [lbl] is {{!arg_label.Labelled} [Labelled l]} and [exp0] is
        [None],
      - [fun ?l:P -> CE] when [lbl] is {{!arg_label.Optional} [Optional l]} and [exp0] is
        [None],
      - [fun ?l:(P = E0) -> CE] when [lbl] is {{!arg_label.Optional} [Optional l]} and
        [exp0] is [Some E0]. *)
  | Pcl_apply of class_expr * expression argument list
    (** [Pcl_apply(CE, [(l1,E1) ; ... ; (ln,En)])] represents [CE ~l1:E1 ... ~ln:En]. [li]
      can be empty (non labeled argument) or start with [?] (optional argument).

      Invariant: [n > 0] *)
  | Pcl_let of rec_flag * value_binding list * class_expr
    (** [Pcl_let(rec, [(P1, E1); ... ; (Pn, En)], CE)] represents:
      - [let P1 = E1 and ... and Pn = EN in CE] when [rec] is
        {{!Asttypes.rec_flag.Nonrecursive} [Nonrecursive]},
      - [let rec P1 = E1 and ... and Pn = EN in CE] when [rec] is
        {{!Asttypes.rec_flag.Recursive} [Recursive]}. *)
  | Pcl_constraint of class_expr * class_type (** [(CE : CT)] *)
  | Pcl_extension of extension (** [[%id]] *)
  | Pcl_open of open_description * class_expr (** [let open M in CE] *)
  | Pcl_parens of class_expr
(** Values of type {!class_structure} represents:
    - [object(selfpat) ... end]
    - [object ... end] when {{!class_structure.pcstr_self} [pcstr_self]} is
      {{!pattern_desc.Ppat_any} [Ppat_any]} *)
and class_structure =
  { pcstr_self : pattern
  ; pcstr_fields : class_field list
  }
and class_field =
  { pcf_pre_doc : string option
  ; pcf_desc : class_field_desc
  ; pcf_loc : location
  ; pcf_attributes : attributes (** [... [\@\@id1] [\@\@id2]] *)
  ; pcf_post_doc : string option
  ; pcf_tokens : token_seq
  }
and class_field_desc =
  | Pcf_inherit of override_flag * class_expr * string loc option
    (** [Pcf_inherit(flag, CE, s)] represents:
      - [inherit CE] when [flag] is {{!Asttypes.override_flag.Fresh} [Fresh]} and [s] is
        [None],
      - [inherit CE as x] when [flag] is {{!Asttypes.override_flag.Fresh} [Fresh]} and [s]
        is [Some x],
      - [inherit! CE] when [flag] is {{!Asttypes.override_flag.Override} [Override]} and
        [s] is [None],
      - [inherit! CE as x] when [flag] is {{!Asttypes.override_flag.Override} [Override]}
        and [s] is [Some x] *)
  | Pcf_val of (string loc * mutable_flag * class_field_kind)
    (** [Pcf_val(x,flag, kind)] represents:
      - [val x = E] when [flag] is {{!Asttypes.mutable_flag.Immutable} [Immutable]} and
        [kind] is {{!class_field_kind.Cfk_concrete} [Cfk_concrete(Fresh, E)]}
      - [val virtual x: T] when [flag] is {{!Asttypes.mutable_flag.Immutable} [Immutable]}
        and [kind] is {{!class_field_kind.Cfk_virtual} [Cfk_virtual(T)]}
      - [val mutable x = E] when [flag] is {{!Asttypes.mutable_flag.Mutable} [Mutable]}
        and [kind] is {{!class_field_kind.Cfk_concrete} [Cfk_concrete(Fresh, E)]}
      - [val mutable virtual x: T] when [flag] is {{!Asttypes.mutable_flag.Mutable}
        [Mutable]} and [kind] is {{!class_field_kind.Cfk_virtual} [Cfk_virtual(T)]} *)
  | Pcf_method of (string loc * private_flag * class_field_kind)
    (** - [method x = E]
      - [method virtual x: T] ([T] can be a {{!core_type_desc.Ptyp_poly} [Ptyp_poly]}) *)
  | Pcf_constraint of (core_type * core_type) (** [constraint T1 = T2] *)
  | Pcf_initializer of expression (** [initializer E] *)
  | Pcf_attribute of attribute (** [[\@\@\@id]] *)
  | Pcf_extension of extension (** [[%%id]] *)
  | Pcf_docstring of string
and class_field_kind =
  | Cfk_virtual of core_type
  | Cfk_concrete of override_flag * value_binding
and class_declaration = class_expr class_infos

(** {1 Module language} *)
(** {2 Type expressions for the module language} *)


and module_type =
  { pmty_desc : module_type_desc
  ; pmty_loc : location
  ; pmty_attributes : attributes (** [... [\@id1] [\@id2]] *)
  ; pmty_tokens : token_seq
  }
and module_type_desc =
  | Pmty_ident of longident loc (** [Pmty_ident(S)] represents [S] *)
  | Pmty_signature of signature (** [sig ... end] *)
  | Pmty_functor of attributes * functor_parameter list * module_type * modes
    (** [functor(X : MT1 @@ modes) -> MT2 @ modes] *)
  | Pmty_functor_type of functor_parameter list * module_type * modes
  | Pmty_with of module_type * with_constraint list (** [MT with ...] *)
  | Pmty_typeof of attributes * module_expr (** [module type of ME] *)
  | Pmty_extension of extension (** [[%id]] *)
  | Pmty_alias of longident loc (** [(module M)] *)
  (*_ [Pmty_strengthen] might be a better fit for [with_constraint] *)
  | Pmty_strengthen of module_type * longident loc (** [MT with S] *)
  | Pmty_parens of module_type
and functor_parameter =
  | Unit (** [()] *)
  | Named of string option loc * module_type * modes
    (** [Named(name, MT)] represents:
      - [(X : MT @@ modes)] when [name] is [Some X],
      - [(_ : MT @@ modes)] when [name] is [None] *)
  | Unnamed of module_type * modes (* only appears in module types *)
and signature =
  { psg_modalities : modalities
  ; psg_items : signature_item list
  ; psg_loc : location
  ; psg_tokens : token_seq
  }
and signature_item =
  { psig_desc : signature_item_desc
  ; psig_loc : location
  ; psig_tokens : token_seq
  }
and signature_item_desc =
  | Psig_value of value_description
    (** - [val x: T]
                                          - [external x: T = "s1" ... "sn"] *)
  | Psig_type of rec_flag * type_declaration list
    (** [type t1 = ... and ... and tn  = ...] *)
  | Psig_typesubst of type_declaration list
    (** [type t1 := ... and ... and tn := ...] *)
  | Psig_typext of type_extension (** [type t1 += ...] *)
  | Psig_exception of type_exception (** [exception C of T] *)
  | Psig_module of module_declaration (** [module X = M] and [module X : MT] *)
  | Psig_modsubst of module_substitution (** [module X := M] *)
  | Psig_recmodule of module_declaration list
    (** [module rec X1 : MT1 and ... and Xn : MTn] *)
  | Psig_modtype of module_type_declaration
    (** [module type S = MT] and [module type S] *)
  | Psig_modtypesubst of module_type_declaration (** [module type S :=  ...] *)
  | Psig_open of open_description (** [open X] *)
  | Psig_include of include_description * modalities (** [include MT] *)
  | Psig_class of class_description list
    (** [class c1 : ... and ... and cn : ...] *)
  | Psig_class_type of class_type_declaration list
    (** [class type ct1 = ... and ... and ctn = ...] *)
  | Psig_attribute of attribute (** [[\@\@\@id]] *)
  | Psig_extension of toplevel_extension
  | Psig_kind_abbrev of string loc * jkind_annotation
    (** [kind_abbrev_ name = k] *)
  | Psig_docstring of string
and module_declaration_body =
  | With_params of functor_parameter list * module_type * modes
  | Without_params of module_type * modalities
(** Values of type [module_declaration] represents [S : MT] *)
and module_declaration =
  { pmd_pre_text : string list
  ; pmd_pre_doc : string option
  ; pmd_ext_attrs : ext_attribute
  ; pmd_name : string option loc * modalities
  ; pmd_body : module_declaration_body
  ; pmd_attributes : attributes (** [... [\@\@id1] [\@\@id2]] *)
  ; pmd_post_doc : string option
  ; pmd_loc : location
  ; pmd_tokens : token_seq
  }
(** Values of type [module_substitution] represents [S := M] *)
and module_substitution =
  { pms_pre_doc : string option
  ; pms_ext_attrs : ext_attribute
  ; pms_name : string loc
  ; pms_manifest : longident loc
  ; pms_attributes : attributes (** [... [\@\@id1] [\@\@id2]] *)
  ; pms_post_doc : string option
  ; pms_loc : location
  ; pms_tokens : token_seq
  }
(** Values of type [module_type_declaration] represents:
    - [S = MT],
    - [S] for abstract module type declaration, when {{!module_type_declaration.pmtd_type}
      [pmtd_type]} is [None]. *)
and module_type_declaration =
  { pmtd_pre_doc : string option
  ; pmtd_ext_attrs : ext_attribute
  ; pmtd_name : string loc
  ; pmtd_type : module_type option
  ; pmtd_attributes : attributes (** [... [\@\@id1] [\@\@id2]] *)
  ; pmtd_post_doc : string option
  ; pmtd_loc : location
  ; pmtd_tokens : token_seq
  }
(** Values of type ['a open_infos] represents:
    - [open! X] when {{!open_infos.popen_override} [popen_override]} is
      {{!Asttypes.override_flag.Override} [Override]} (silences the "used identifier
      shadowing" warning)
    - [open  X] when {{!open_infos.popen_override} [popen_override]} is
      {{!Asttypes.override_flag.Fresh} [Fresh]} *)
and 'a open_infos =
  { popen_pre_doc : string option
  ; popen_ext_attrs : ext_attribute
  ; popen_expr : 'a
  ; popen_override : override_flag
  ; popen_loc : location
  ; popen_attributes : attributes
  ; popen_post_doc : string option
  ; popen_tokens : token_seq
  }
(** Values of type [open_description] represents:
    - [open M.N]
    - [open M(N).O] *)
and open_description = longident loc open_infos
(** Values of type [open_declaration] represents:
    - [open M.N]
    - [open M(N).O]
    - [open struct ... end] *)
and open_declaration = module_expr open_infos
and 'a include_infos =
  { pincl_pre_doc : string option
  ; pincl_kind : include_kind
  ; pincl_ext_attrs : ext_attribute
  ; pincl_mod : 'a
  ; pincl_loc : location
  ; pincl_attributes : attributes
  ; pincl_post_doc : string option
  ; pincl_tokens : token_seq
  }
(** Values of type [include_description] represents [include MT] *)
and include_description = module_type include_infos
(** Values of type [include_declaration] represents [include ME] *)
and include_declaration = module_expr include_infos
and with_constraint =
  { wc_desc : with_constraint_desc
  ; wc_loc : location
  ; wc_tokens : token_seq
  }
and with_constraint_desc =
  | Pwith_type of
      ptype_params
      *
      longident loc
      *
      private_flag
      *
      core_type
      *
      ptype_constraint list
    (** [with type X.t = ...]

      Note: the last component of the longident must match the name of the
      type_declaration. *)
  | Pwith_module of longident loc * longident loc (** [with module X.Y = Z] *)
  | Pwith_modtype of longident loc * module_type
    (** [with module type X.Y = Z] *)
  | Pwith_modtypesubst of longident loc * module_type
    (** [with module type X.Y := sig end] *)
  | Pwith_typesubst of ptype_params * longident loc * core_type
    (** [with type X.t := ..., same format as [Pwith_type]] *)
  | Pwith_modsubst of longident loc * longident loc
    (** [with module X.Y := Z] *)

(** {2 Value expressions for the module language} *)


and module_expr =
  { pmod_desc : module_expr_desc
  ; pmod_loc : location
  ; pmod_attributes : attributes (** [... [\@id1] [\@id2]] *)
  ; pmod_tokens : token_seq
  }
and module_expr_desc =
  | Pmod_ident of longident loc (** [X] *)
  | Pmod_structure of attributes * structure (** [struct[@attrs] ... end] *)
  | Pmod_functor of attributes * functor_parameter list * module_expr
    (** [functor [@attr] (X : MT1) (X2 : MT2) -> ME] *)
  | Pmod_apply of module_expr * module_expr (** [ME1(ME2)] *)
  | Pmod_apply_unit of module_expr (** [ME1()] *)
  | Pmod_constraint of module_expr * module_type option * modes
    (** - [(ME : MT @@ modes)]
      - [(ME @ modes)]
      - [(ME : MT)] *)
  | Pmod_unpack of expression * package_type option * package_type option
    (** [(val E)] *)
  | Pmod_extension of extension (** [[%id]] *)
  | Pmod_parens of
      module_expr
      (* | Pmod_instance of module_instance (**
   [Foo(Param1)(Arg1(Param2)(Arg2)) [@jane.non_erasable.instances]]

   The name of an instance module. Gets converted to [Global.Name.t] in the
   flambda-backend compiler. *)

   and module_instance =
   [{ pmod_instance_head : string; pmod_instance_args : (string * module_instance) list }]
   (** [M(P1)(MI1)...(Pn)(MIn)] *)
*)
and structure = structure_item list * token_seq
and structure_item =
  { pstr_desc : structure_item_desc
  ; pstr_loc : location
  ; pstr_tokens : token_seq
  }
and structure_item_desc =
  | Pstr_eval of expression * attributes (** [E] *)
  | Pstr_value of rec_flag * value_binding list
    (** [Pstr_value(rec, [(P1, E1 ; ... ; (Pn, En))])] represents:
      - [let P1 = E1 and ... and Pn = EN] when [rec] is {{!Asttypes.rec_flag.Nonrecursive}
        [Nonrecursive]},
      - [let rec P1 = E1 and ... and Pn = EN ] when [rec] is
        {{!Asttypes.rec_flag.Recursive} [Recursive]}. *)
  | Pstr_primitive of value_description
    (** - [val x: T]
        - [external x: T = "s1" ... "sn" ] *)
  | Pstr_type of rec_flag * type_declaration list
    (** [type t1 = ... and ... and tn = ...] *)
  | Pstr_typext of type_extension (** [type t1 += ...] *)
  | Pstr_exception of type_exception
    (** - [exception C of T]
                                           - [exception C = M.X] *)
  | Pstr_module of module_binding (** [module X = ME] *)
  | Pstr_recmodule of module_binding list
    (** [module rec X1 = ME1 and ... and Xn = MEn] *)
  | Pstr_modtype of module_type_declaration (** [module type S = MT] *)
  | Pstr_open of open_declaration (** [open X] *)
  | Pstr_class of class_declaration list
    (** [class c1 = ... and ... and cn = ...] *)
  | Pstr_class_type of class_type_declaration list
    (** [class type ct1 = ... and ... and ctn = ...] *)
  | Pstr_include of include_declaration (** [include ME] *)
  | Pstr_attribute of attribute (** [[\@\@\@id]] *)
  | Pstr_extension of toplevel_extension
  | Pstr_kind_abbrev of string loc * jkind_annotation
    (** [kind_abbrev_ name = k] *)
  | Pstr_docstring of string
and value_constraint =
  | Pvc_constraint of
      { locally_abstract_univars : (string loc * jkind_annotation option) list
      ; typ : core_type
      }
  | Pvc_coercion of
      { ground : core_type option
      ; coercion : core_type
      }
    (** - [Pvc_constraint { locally_abstract_univars=[]; typ}] is a simple type constraint
        on a value binding: [ let x : typ]
      - More generally, in [Pvc_constraint { locally_abstract_univars; typ}]
        [locally_abstract_univars] is the list of locally abstract type variables in
        [ let x: type a ... . typ ]
      - [Pvc_coercion { ground=None; coercion }] represents [let x :> typ]
      - [Pvc_coercion { ground=Some g; coercion }] represents [let x : g :> typ] *)

(** [let modes pat params : type_constraint @@ ret_modes = exp] *)
and value_binding =
  { pvb_pre_text : string list
  ; pvb_pre_doc : string option
  ; pvb_ext_attrs : ext_attribute
  ; pvb_legacy_modes : modes
  ; pvb_pat : pattern
  ; pvb_modes : modes
  ; pvb_params : function_param list
  ; pvb_constraint : value_constraint option
  ; pvb_expr : expression option
  ; pvb_ret_modes : modes
  ; pvb_attributes : attributes
  ; pvb_post_doc : string option
  ; pvb_loc : location
  ; pvb_tokens : token_seq
  }
(** Values of type [module_binding] represents [module X = ME] *)
and module_binding =
  { pmb_pre_text : string list
  ; pmb_pre_doc : string option
  ; pmb_ext_attrs : ext_attribute
  ; pmb_name : string option loc * modes
  ; pmb_params : functor_parameter list
  ; pmb_constraint : module_type option
  ; pmb_modes : modes
  ; pmb_expr : module_expr
  ; pmb_attributes : attributes
  ; pmb_post_doc : string option
  ; pmb_loc : location
  ; pmb_tokens : token_seq
  }
and jkind_annotation_desc =
  | Pjk_default
  | Pjk_abbreviation of string
  (* XXX layouts v2.8: [mod] can have only layouts on the left, not full kind annotations.
     We may want to narrow this type some. *)
  | Pjk_mod of jkind_annotation * modes
  | Pjk_with of jkind_annotation * core_type * modalities
  | Pjk_kind_of of core_type
  | Pjk_product of jkind_annotation list
  | Pjk_parens of jkind_annotation_desc
and jkind_annotation =
  { pjkind_loc : location
  ; pjkind_desc : jkind_annotation_desc
  ; pjkind_tokens : token_seq
  }

(** {1 Toplevel} *)
(** {2 Toplevel phrases} *)


and use_file = toplevel_phrase list * token_seq
and toplevel_phrase =
  | Ptop_def of structure
  | Ptop_dir of toplevel_directive (** [#use], [#load] ... *)
  | Ptop_lex of lexer_directive
and toplevel_directive =
  { pdir_name : string loc
  ; pdir_arg : directive_argument option
  ; pdir_loc : location
  ; pdir_tokens : token_seq
  }
and directive_argument =
  { pdira_desc : directive_argument_desc
  ; pdira_loc : location
  }
and directive_argument_desc =
  | Pdir_string of string
  | Pdir_int of string * char option
  | Pdir_ident of longident
  | Pdir_bool of bool
(** Lexer directives: ugly hack to avoid their deletion *)
and syntax_directive =
  { psyn_mode : string loc (* which syntax feature e.g. quotations *)
  ; psyn_toggle : bool (* on/off *)
  }
and lexer_directive_desc = Plex_syntax of syntax_directive
and lexer_directive =
  { plex_desc : lexer_directive_desc
  ; plex_loc : location
  ; plex_tokens : token_seq
  }
[@@deriving_inline traverse]

class virtual map =
  object(self)
    method virtual bool : bool -> bool

    method virtual char : char -> char

    method virtual int : int -> int

    method virtual list : 'a. ('a -> 'a) -> 'a list -> 'a list

    method virtual option : 'a. ('a -> 'a) -> 'a option -> 'a option

    method virtual ref : 'a. ('a -> 'a) -> 'a ref -> 'a ref

    method virtual string : string -> string

    method virtual token : token -> token

    method position : position -> position =
      fun { pos_fname; pos_lnum; pos_bol; pos_cnum } ->
      let pos_fname = self#string pos_fname in
      let pos_lnum = self#int pos_lnum in
      let pos_bol = self#int pos_bol in
      let pos_cnum = self#int pos_cnum in
      { pos_fname; pos_lnum; pos_bol; pos_cnum }

    method location : location -> location =
      fun { loc_start; loc_end; loc_ghost } ->
      let loc_start = self#position loc_start in
      let loc_end = self#position loc_end in
      let loc_ghost = self#bool loc_ghost in
      { loc_start; loc_end; loc_ghost }

    method longident_dotop_delims
      : longident_dotop_delims -> longident_dotop_delims
      =
      fun x -> x

    method longident_str_or_op : longident_str_or_op -> longident_str_or_op =
      fun x ->
      match x with
      | Str a ->
        let a = self#string a in
        Str a
      | Str_trailing_hash a ->
        let a = self#string a in
        Str_trailing_hash a
      | Op a ->
        let a = self#string a in
        Op a
      | DotOp (a, b, c, d) ->
        let a = self#string a in
        let b = self#longident_dotop_delims b in
        let c = self#string c in
        let d = self#bool d in
        DotOp (a, b, c, d)

    method longident_lid_desc : longident_lid_desc -> longident_lid_desc =
      fun x ->
      match x with
      | Lident a ->
        let a = self#longident_str_or_op a in
        Lident a
      | Ldot (a, b) ->
        let a = self#longident a in
        let b = self#longident_str_or_op b in
        Ldot (a, b)
      | Lapply (a, b) ->
        let a = self#longident a in
        let b = self#longident b in
        Lapply (a, b)

    method longident : longident -> longident =
      fun { desc; tokens } ->
      let desc = self#longident_lid_desc desc in
      let tokens = self#token_seq tokens in
      { desc; tokens }

    method attachment : attachment -> attachment = fun x -> x

    method comment : comment -> comment =
      fun { text; attachement; explicitely_inserted } ->
      let text = self#string text in
      let attachement = self#attachment attachement in
      let explicitely_inserted = self#ref self#bool explicitely_inserted in
      { text; attachement; explicitely_inserted }

    method token_desc : token_desc -> token_desc =
      fun x ->
      match x with
      | Token (a, b) ->
        let a = self#token a in
        let b = self#bool b in
        Token (a, b)
      | Comment a ->
        let a = self#comment a in
        Comment a
      | Child_node -> Child_node

    method token_elt : token_elt -> token_elt =
      fun { desc; pos } ->
      let desc = self#token_desc desc in
      let pos = self#position pos in
      { desc; pos }

    method token_seq : token_seq -> token_seq = self#list self#token_elt

    method rec_flag : rec_flag -> rec_flag = fun x -> x

    method direction_flag : direction_flag -> direction_flag = fun x -> x

    method private_flag : private_flag -> private_flag = fun x -> x

    method mutable_flag : mutable_flag -> mutable_flag = fun x -> x

    method virtual_flag : virtual_flag -> virtual_flag = fun x -> x

    method override_flag : override_flag -> override_flag = fun x -> x

    method closed_flag : closed_flag -> closed_flag = fun x -> x

    method label : label -> label = self#string

    method arg_label : arg_label -> arg_label =
      fun x ->
      match x with
      | Nolabel -> Nolabel
      | Labelled a ->
        let a = self#string a in
        Labelled a
      | Optional a ->
        let a = self#string a in
        Optional a

    method loc : 'a. ('a -> 'a) -> 'a loc -> 'a loc =
      fun _a { txt; loc } ->
      let txt = _a txt in
      let loc = self#location loc in
      { txt; loc }

    method variance : variance -> variance = fun x -> x

    method injectivity : injectivity -> injectivity = fun x -> x

    method index_kind : index_kind -> index_kind = fun x -> x

    method paren_kind : paren_kind -> paren_kind = fun x -> x

    method constant : constant -> constant =
      fun x ->
      match x with
      | Pconst_integer (a, b, c) ->
        let a = self#option self#string a in
        let b = self#string b in
        let c = self#option self#char c in
        Pconst_integer (a, b, c)
      | Pconst_unboxed_integer (a, b, c) ->
        let a = self#option self#string a in
        let b = self#string b in
        let c = self#char c in
        Pconst_unboxed_integer (a, b, c)
      | Pconst_char (a, b) ->
        let a = self#char a in
        let b = self#string b in
        Pconst_char (a, b)
      | Pconst_untagged_char (a, b) ->
        let a = self#char a in
        let b = self#string b in
        Pconst_untagged_char (a, b)
      | Pconst_string (a, b, c) ->
        let a = self#string a in
        let b = self#location b in
        let c = self#option self#string c in
        Pconst_string (a, b, c)
      | Pconst_float (a, b, c) ->
        let a = self#option self#string a in
        let b = self#string b in
        let c = self#option self#char c in
        Pconst_float (a, b, c)
      | Pconst_unboxed_float (a, b, c) ->
        let a = self#option self#string a in
        let b = self#string b in
        let c = self#option self#char c in
        Pconst_unboxed_float (a, b, c)

    method modality : modality -> modality =
      fun x ->
      match x with
      | Modality a ->
        let a = self#string a in
        Modality a

    method modalities : modalities -> modalities =
      self#list (self#loc self#modality)

    method mode : mode -> mode =
      fun x ->
      match x with
      | Mode a ->
        let a = self#string a in
        Mode a

    method modes : modes -> modes = self#list (self#loc self#mode)

    method include_kind : include_kind -> include_kind = fun x -> x

    method attribute : attribute -> attribute =
      fun { attr_name; attr_payload; attr_loc; attr_tokens } ->
      let attr_name = self#loc (self#list self#string) attr_name in
      let attr_payload = self#payload attr_payload in
      let attr_loc = self#location attr_loc in
      let attr_tokens = self#token_seq attr_tokens in
      { attr_name; attr_payload; attr_loc; attr_tokens }

    method extension : extension -> extension =
      fun (a, b, c) ->
      let a = self#loc (self#list self#string) a in
      let b = self#payload b in
      let c = self#token_seq c in
      a, b, c

    method toplevel_extension : toplevel_extension -> toplevel_extension =
      fun { te_pre_doc; te_ext; te_attrs; te_post_doc } ->
      let te_pre_doc = self#option self#string te_pre_doc in
      let te_ext = self#extension te_ext in
      let te_attrs = self#attributes te_attrs in
      let te_post_doc = self#option self#string te_post_doc in
      { te_pre_doc; te_ext; te_attrs; te_post_doc }

    method attributes : attributes -> attributes = self#list self#attribute

    method payload : payload -> payload =
      fun x ->
      match x with
      | PStr a ->
        let a = self#structure a in
        PStr a
      | PSig a ->
        let a = self#signature a in
        PSig a
      | PTyp a ->
        let a = self#core_type a in
        PTyp a
      | PPat (a, b) ->
        let a = self#pattern a in
        let b = self#option self#expression b in
        PPat (a, b)
      | PString (a, b) ->
        let a = self#string a in
        let b = self#string b in
        PString (a, b)

    method ext_attribute : ext_attribute -> ext_attribute =
      fun { pea_ext; pea_attrs } ->
      let pea_ext = self#option (self#loc (self#list self#string)) pea_ext in
      let pea_attrs = self#attributes pea_attrs in
      { pea_ext; pea_attrs }

    method core_type : core_type -> core_type =
      fun { ptyp_desc; ptyp_loc; ptyp_attributes; ptyp_tokens } ->
      let ptyp_desc = self#core_type_desc ptyp_desc in
      let ptyp_loc = self#location ptyp_loc in
      let ptyp_attributes = self#attributes ptyp_attributes in
      let ptyp_tokens = self#token_seq ptyp_tokens in
      { ptyp_desc; ptyp_loc; ptyp_attributes; ptyp_tokens }

    method arrow_arg : arrow_arg -> arrow_arg =
      fun
        { aa_lbl
        ; aa_legacy_modes
        ; aa_type
        ; aa_modes
        ; aa_doc
        ; aa_loc
        ; aa_tokens
        }
        ->
      let aa_lbl = self#arg_label aa_lbl in
      let aa_legacy_modes = self#modes aa_legacy_modes in
      let aa_type = self#core_type aa_type in
      let aa_modes = self#modes aa_modes in
      let aa_doc = self#option self#string aa_doc in
      let aa_loc = self#location aa_loc in
      let aa_tokens = self#token_seq aa_tokens in
      { aa_lbl; aa_legacy_modes; aa_type; aa_modes; aa_doc; aa_loc; aa_tokens }

    method core_type_desc : core_type_desc -> core_type_desc =
      fun x ->
      match x with
      | Ptyp_any a ->
        let a = self#option self#jkind_annotation a in
        Ptyp_any a
      | Ptyp_var (a, b) ->
        let a = self#string a in
        let b = self#option self#jkind_annotation b in
        Ptyp_var (a, b)
      | Ptyp_arrow { domain; codom_legacy_modes; codom_type; codom_modes } ->
        let domain = self#arrow_arg domain in
        let codom_legacy_modes = self#modes codom_legacy_modes in
        let codom_type = self#core_type codom_type in
        let codom_modes = self#modes codom_modes in
        Ptyp_arrow { domain; codom_legacy_modes; codom_type; codom_modes }
      | Ptyp_tuple a ->
        let a =
          self
          #
          list
            (fun (a, b) ->
              let a = self#option self#string a in
              let b = self#core_type b in
              a, b)
            a
        in
        Ptyp_tuple a
      | Ptyp_unboxed_tuple a ->
        let a =
          self
          #
          list
            (fun (a, b) ->
              let a = self#option self#string a in
              let b = self#core_type b in
              a, b)
            a
        in
        Ptyp_unboxed_tuple a
      | Ptyp_constr (a, b) ->
        let a = self#list self#core_type a in
        let b = self#loc self#longident b in
        Ptyp_constr (a, b)
      | Ptyp_object (a, b) ->
        let a = self#list self#object_field a in
        let b = self#closed_flag b in
        Ptyp_object (a, b)
      | Ptyp_class (a, b) ->
        let a = self#loc self#longident a in
        let b = self#list self#core_type b in
        Ptyp_class (a, b)
      | Ptyp_alias (a, b, c) ->
        let a = self#core_type a in
        let b = self#option (self#loc self#string) b in
        let c = self#option self#jkind_annotation c in
        Ptyp_alias (a, b, c)
      | Ptyp_variant (a, b, c) ->
        let a = self#list self#row_field a in
        let b = self#closed_flag b in
        let c = self#option (self#list self#label) c in
        Ptyp_variant (a, b, c)
      | Ptyp_poly (a, b) ->
        let a =
          self
          #
          list
            (fun (a, b) ->
              let a = self#loc self#string a in
              let b = self#option self#jkind_annotation b in
              a, b)
            a
        in
        let b = self#core_type b in
        Ptyp_poly (a, b)
      | Ptyp_package (a, b) ->
        let a = self#ext_attribute a in
        let b = self#package_type b in
        Ptyp_package (a, b)
      | Ptyp_open (a, b) ->
        let a = self#loc self#longident a in
        let b = self#core_type b in
        Ptyp_open (a, b)
      | Ptyp_quote a ->
        let a = self#core_type a in
        Ptyp_quote a
      | Ptyp_splice a ->
        let a = self#core_type a in
        Ptyp_splice a
      | Ptyp_of_kind a ->
        let a = self#jkind_annotation a in
        Ptyp_of_kind a
      | Ptyp_extension a ->
        let a = self#extension a in
        Ptyp_extension a
      | Ptyp_parens a ->
        let a = self#core_type a in
        Ptyp_parens a

    method package_type : package_type -> package_type = self#module_type

    method row_field : row_field -> row_field =
      fun { prf_desc; prf_loc; prf_attributes; prf_doc; prf_tokens } ->
      let prf_desc = self#row_field_desc prf_desc in
      let prf_loc = self#location prf_loc in
      let prf_attributes = self#attributes prf_attributes in
      let prf_doc = self#option self#string prf_doc in
      let prf_tokens = self#token_seq prf_tokens in
      { prf_desc; prf_loc; prf_attributes; prf_doc; prf_tokens }

    method row_field_desc : row_field_desc -> row_field_desc =
      fun x ->
      match x with
      | Rtag (a, b, c) ->
        let a = self#loc self#string a in
        let b = self#bool b in
        let c = self#list self#core_type c in
        Rtag (a, b, c)
      | Rinherit a ->
        let a = self#core_type a in
        Rinherit a

    method object_field : object_field -> object_field =
      fun { pof_desc; pof_loc; pof_attributes; pof_doc; pof_tokens } ->
      let pof_desc = self#object_field_desc pof_desc in
      let pof_loc = self#location pof_loc in
      let pof_attributes = self#attributes pof_attributes in
      let pof_doc = self#option self#string pof_doc in
      let pof_tokens = self#token_seq pof_tokens in
      { pof_desc; pof_loc; pof_attributes; pof_doc; pof_tokens }

    method object_field_desc : object_field_desc -> object_field_desc =
      fun x ->
      match x with
      | Otag (a, b) ->
        let a = self#loc self#string a in
        let b = self#core_type b in
        Otag (a, b)
      | Oinherit a ->
        let a = self#core_type a in
        Oinherit a

    method pattern : pattern -> pattern =
      fun
        { ppat_ext_attr; ppat_desc; ppat_loc; ppat_attributes; ppat_tokens } ->
      let ppat_ext_attr = self#ext_attribute ppat_ext_attr in
      let ppat_desc = self#pattern_desc ppat_desc in
      let ppat_loc = self#location ppat_loc in
      let ppat_attributes = self#attributes ppat_attributes in
      let ppat_tokens = self#token_seq ppat_tokens in
      { ppat_ext_attr; ppat_desc; ppat_loc; ppat_attributes; ppat_tokens }

    method pattern_desc : pattern_desc -> pattern_desc =
      fun x ->
      match x with
      | Ppat_any -> Ppat_any
      | Ppat_var a ->
        let a = self#loc self#longident_str_or_op a in
        Ppat_var a
      | Ppat_alias (a, b) ->
        let a = self#pattern a in
        let b = self#loc self#longident_str_or_op b in
        Ppat_alias (a, b)
      | Ppat_constant a ->
        let a = self#constant a in
        Ppat_constant a
      | Ppat_interval (a, b) ->
        let a = self#constant a in
        let b = self#constant b in
        Ppat_interval (a, b)
      | Ppat_tuple (a, b) ->
        let a = self#list (self#argument self#pattern) a in
        let b = self#closed_flag b in
        Ppat_tuple (a, b)
      | Ppat_unboxed_tuple (a, b) ->
        let a = self#list (self#argument self#pattern) a in
        let b = self#closed_flag b in
        Ppat_unboxed_tuple (a, b)
      | Ppat_construct (a, b) ->
        let a = self#loc self#longident a in
        let b =
          self
          #
          option
            (fun (a, b) ->
              let a =
                self
                #
                list
                  (fun (a, b) ->
                    let a = self#loc self#string a in
                    let b = self#option self#jkind_annotation b in
                    a, b)
                  a
              in
              let b = self#pattern b in
              a, b)
            b
        in
        Ppat_construct (a, b)
      | Ppat_variant (a, b) ->
        let a = self#label a in
        let b = self#option self#pattern b in
        Ppat_variant (a, b)
      | Ppat_record (a, b) ->
        let a = self#list (self#record_field self#pattern) a in
        let b = self#closed_flag b in
        Ppat_record (a, b)
      | Ppat_record_unboxed_product (a, b) ->
        let a = self#list (self#record_field self#pattern) a in
        let b = self#closed_flag b in
        Ppat_record_unboxed_product (a, b)
      | Ppat_array (a, b) ->
        let a = self#mutable_flag a in
        let b = self#list self#pattern b in
        Ppat_array (a, b)
      | Ppat_or (a, b) ->
        let a = self#pattern a in
        let b = self#pattern b in
        Ppat_or (a, b)
      | Ppat_constraint (a, b, c) ->
        let a = self#pattern a in
        let b = self#option self#core_type b in
        let c = self#modes c in
        Ppat_constraint (a, b, c)
      | Ppat_type a ->
        let a = self#loc self#longident a in
        Ppat_type a
      | Ppat_lazy a ->
        let a = self#pattern a in
        Ppat_lazy a
      | Ppat_unpack (a, b) ->
        let a = self#loc (self#option self#string) a in
        let b = self#option self#package_type b in
        Ppat_unpack (a, b)
      | Ppat_exception a ->
        let a = self#pattern a in
        Ppat_exception a
      | Ppat_extension a ->
        let a = self#extension a in
        Ppat_extension a
      | Ppat_open (a, b) ->
        let a = self#loc self#longident a in
        let b = self#pattern b in
        Ppat_open (a, b)
      | Ppat_parens { pat; optional } ->
        let pat = self#pattern pat in
        let optional = self#bool optional in
        Ppat_parens { pat; optional }
      | Ppat_list a ->
        let a = self#list self#pattern a in
        Ppat_list a
      | Ppat_cons (a, b) ->
        let a = self#pattern a in
        let b = self#pattern b in
        Ppat_cons (a, b)

    method expression : expression -> expression =
      fun
        { pexp_ext_attr; pexp_desc; pexp_loc; pexp_attributes; pexp_tokens } ->
      let pexp_ext_attr = self#ext_attribute pexp_ext_attr in
      let pexp_desc = self#expression_desc pexp_desc in
      let pexp_loc = self#location pexp_loc in
      let pexp_attributes = self#attributes pexp_attributes in
      let pexp_tokens = self#token_seq pexp_tokens in
      { pexp_ext_attr; pexp_desc; pexp_loc; pexp_attributes; pexp_tokens }

    method expression_desc : expression_desc -> expression_desc =
      fun x ->
      match x with
      | Pexp_ident a ->
        let a = self#loc self#longident a in
        Pexp_ident a
      | Pexp_constant a ->
        let a = self#constant a in
        Pexp_constant a
      | Pexp_let (a, b, c, d) ->
        let a = self#mutable_flag a in
        let b = self#rec_flag b in
        let c = self#list self#value_binding c in
        let d = self#expression d in
        Pexp_let (a, b, c, d)
      | Pexp_function (a, b, c) ->
        let a = self#list self#function_param a in
        let b = self#function_constraint b in
        let c = self#function_body c in
        Pexp_function (a, b, c)
      | Pexp_prefix_apply (a, b) ->
        let a = self#expression a in
        let b = self#expression b in
        Pexp_prefix_apply (a, b)
      | Pexp_add_or_sub (a, b) ->
        let a = self#string a in
        let b = self#expression b in
        Pexp_add_or_sub (a, b)
      | Pexp_infix_apply { arg1; op; arg2 } ->
        let arg1 = self#expression arg1 in
        let op = self#expression op in
        let arg2 = self#expression arg2 in
        Pexp_infix_apply { arg1; op; arg2 }
      | Pexp_apply (a, b) ->
        let a = self#expression a in
        let b = self#list (self#argument self#expression) b in
        Pexp_apply (a, b)
      | Pexp_match (a, b) ->
        let a = self#expression a in
        let b = self#list self#case b in
        Pexp_match (a, b)
      | Pexp_try (a, b) ->
        let a = self#expression a in
        let b = self#list self#case b in
        Pexp_try (a, b)
      | Pexp_tuple a ->
        let a = self#list (self#argument self#expression) a in
        Pexp_tuple a
      | Pexp_unboxed_tuple a ->
        let a = self#list (self#argument self#expression) a in
        Pexp_unboxed_tuple a
      | Pexp_construct (a, b) ->
        let a = self#loc self#longident a in
        let b = self#option self#expression b in
        Pexp_construct (a, b)
      | Pexp_variant (a, b) ->
        let a = self#label a in
        let b = self#option self#expression b in
        Pexp_variant (a, b)
      | Pexp_record (a, b) ->
        let a = self#option self#expression a in
        let b = self#list (self#record_field self#expression) b in
        Pexp_record (a, b)
      | Pexp_record_unboxed_product (a, b) ->
        let a = self#option self#expression a in
        let b = self#list (self#record_field self#expression) b in
        Pexp_record_unboxed_product (a, b)
      | Pexp_field (a, b) ->
        let a = self#expression a in
        let b = self#loc self#longident b in
        Pexp_field (a, b)
      | Pexp_unboxed_field (a, b) ->
        let a = self#expression a in
        let b = self#loc self#longident b in
        Pexp_unboxed_field (a, b)
      | Pexp_setfield (a, b, c) ->
        let a = self#expression a in
        let b = self#loc self#longident b in
        let c = self#expression c in
        Pexp_setfield (a, b, c)
      | Pexp_array (a, b) ->
        let a = self#mutable_flag a in
        let b = self#list self#expression b in
        Pexp_array (a, b)
      | Pexp_idx (a, b) ->
        let a = self#block_access a in
        let b = self#list self#unboxed_access b in
        Pexp_idx (a, b)
      | Pexp_ifthenelse (a, b, c) ->
        let a = self#expression a in
        let b = self#expression b in
        let c = self#option self#expression c in
        Pexp_ifthenelse (a, b, c)
      | Pexp_sequence (a, b) ->
        let a = self#expression a in
        let b = self#expression b in
        Pexp_sequence (a, b)
      | Pexp_seq_empty a ->
        let a = self#expression a in
        Pexp_seq_empty a
      | Pexp_while (a, b) ->
        let a = self#expression a in
        let b = self#expression b in
        Pexp_while (a, b)
      | Pexp_for (a, b, c, d, e) ->
        let a = self#pattern a in
        let b = self#expression b in
        let c = self#expression c in
        let d = self#direction_flag d in
        let e = self#expression e in
        Pexp_for (a, b, c, d, e)
      | Pexp_constraint (a, b, c) ->
        let a = self#expression a in
        let b = self#option self#core_type b in
        let c = self#modes c in
        Pexp_constraint (a, b, c)
      | Pexp_coerce (a, b, c) ->
        let a = self#expression a in
        let b = self#option self#core_type b in
        let c = self#core_type c in
        Pexp_coerce (a, b, c)
      | Pexp_send (a, b) ->
        let a = self#expression a in
        let b = self#loc self#string b in
        Pexp_send (a, b)
      | Pexp_new a ->
        let a = self#loc self#longident a in
        Pexp_new a
      | Pexp_setvar (a, b) ->
        let a = self#loc self#string a in
        let b = self#expression b in
        Pexp_setvar (a, b)
      | Pexp_override a ->
        let a =
          self
          #
          list
            (fun (a, b) ->
              let a = self#loc self#string a in
              let b = self#option self#expression b in
              a, b)
            a
        in
        Pexp_override a
      | Pexp_letmodule (a, b) ->
        let a = self#module_binding a in
        let b = self#expression b in
        Pexp_letmodule (a, b)
      | Pexp_letexception (a, b) ->
        let a = self#extension_constructor a in
        let b = self#expression b in
        Pexp_letexception (a, b)
      | Pexp_assert a ->
        let a = self#expression a in
        Pexp_assert a
      | Pexp_lazy a ->
        let a = self#expression a in
        Pexp_lazy a
      | Pexp_object a ->
        let a = self#class_structure a in
        Pexp_object a
      | Pexp_pack (a, b) ->
        let a = self#module_expr a in
        let b = self#option self#package_type b in
        Pexp_pack (a, b)
      | Pexp_dot_open (a, b) ->
        let a = self#loc self#longident a in
        let b = self#expression b in
        Pexp_dot_open (a, b)
      | Pexp_let_open (a, b) ->
        let a = self#open_declaration a in
        let b = self#expression b in
        Pexp_let_open (a, b)
      | Pexp_letop a ->
        let a = self#letop a in
        Pexp_letop a
      | Pexp_extension a ->
        let a = self#extension a in
        Pexp_extension a
      | Pexp_unreachable -> Pexp_unreachable
      | Pexp_stack a ->
        let a = self#expression a in
        Pexp_stack a
      | Pexp_comprehension a ->
        let a = self#comprehension_expression a in
        Pexp_comprehension a
      | Pexp_overwrite (a, b) ->
        let a = self#expression a in
        let b = self#expression b in
        Pexp_overwrite (a, b)
      | Pexp_quote a ->
        let a = self#expression a in
        Pexp_quote a
      | Pexp_splice a ->
        let a = self#expression a in
        Pexp_splice a
      | Pexp_hole -> Pexp_hole
      | Pexp_index_op { kind; op; seq; indices; assign } ->
        let kind = self#paren_kind kind in
        let op =
          self
          #
          option
            (fun (a, b) ->
              let a = self#option self#longident a in
              let b = self#string b in
              a, b)
            op
        in
        let seq = self#expression seq in
        let indices = self#list self#expression indices in
        let assign = self#option self#expression assign in
        Pexp_index_op { kind; op; seq; indices; assign }
      | Pexp_parens { exp; optional } ->
        let exp = self#expression exp in
        let optional = self#bool optional in
        Pexp_parens { exp; optional }
      | Pexp_begin_end a ->
        let a = self#option self#expression a in
        Pexp_begin_end a
      | Pexp_list a ->
        let a = self#list self#expression a in
        Pexp_list a
      | Pexp_cons (a, b) ->
        let a = self#expression a in
        let b = self#expression b in
        Pexp_cons (a, b)
      | Pexp_exclave a ->
        let a = self#expression a in
        Pexp_exclave a
      | Pexp_mode_legacy (a, b) ->
        let a = self#loc self#mode a in
        let b = self#expression b in
        Pexp_mode_legacy (a, b)

    method record_field : 'a. ('a -> 'a) -> 'a record_field -> 'a record_field =
      fun _a { field_name; typ; value } ->
      let field_name = self#loc self#longident field_name in
      let typ = self#option self#type_constraint typ in
      let value = self#option _a value in
      { field_name; typ; value }

    method case : case -> case =
      fun { pc_lhs; pc_guard; pc_rhs; pc_tokens } ->
      let pc_lhs = self#pattern pc_lhs in
      let pc_guard = self#option self#expression pc_guard in
      let pc_rhs = self#expression pc_rhs in
      let pc_tokens = self#token_seq pc_tokens in
      { pc_lhs; pc_guard; pc_rhs; pc_tokens }

    method letop : letop -> letop =
      fun { let_; ands; body } ->
      let let_ = self#binding_op let_ in
      let ands = self#list self#binding_op ands in
      let body = self#expression body in
      { let_; ands; body }

    method binding_op : binding_op -> binding_op =
      fun { pbop_op; pbop_binding; pbop_loc } ->
      let pbop_op = self#loc self#string pbop_op in
      let pbop_binding = self#value_binding pbop_binding in
      let pbop_loc = self#location pbop_loc in
      { pbop_op; pbop_binding; pbop_loc }

    method argument_desc
      : 'a. ('a -> 'a) -> 'a argument_desc -> 'a argument_desc
      =
      fun _a x ->
      match x with
      | Parg_unlabelled { legacy_modes; arg; typ_constraint; modes } ->
        let legacy_modes = self#modes legacy_modes in
        let arg = _a arg in
        let typ_constraint = self#option self#type_constraint typ_constraint in
        let modes = self#modes modes in
        Parg_unlabelled { legacy_modes; arg; typ_constraint; modes }
      | Parg_labelled
          { optional
          ; legacy_modes
          ; name
          ; maybe_punned
          ; typ_constraint
          ; modes
          ; default
          } ->
        let optional = self#bool optional in
        let legacy_modes = self#modes legacy_modes in
        let name = self#string name in
        let maybe_punned = self#option _a maybe_punned in
        let typ_constraint = self#option self#type_constraint typ_constraint in
        let modes = self#modes modes in
        let default = self#option self#expression default in
        Parg_labelled
          { optional
          ; legacy_modes
          ; name
          ; maybe_punned
          ; typ_constraint
          ; modes
          ; default
          }

    method argument : 'a. ('a -> 'a) -> 'a argument -> 'a argument =
      fun _a { parg_desc; parg_tokens } ->
      let parg_desc = self#argument_desc _a parg_desc in
      let parg_tokens = self#token_seq parg_tokens in
      { parg_desc; parg_tokens }

    method function_param_desc : function_param_desc -> function_param_desc =
      fun x ->
      match x with
      | Pparam_val a ->
        let a = self#argument self#pattern a in
        Pparam_val a
      | Pparam_newtype (a, b) ->
        let a = self#loc self#string a in
        let b = self#option self#jkind_annotation b in
        Pparam_newtype (a, b)
      | Pparam_newtypes a ->
        let a =
          self
          #
          list
            (fun (a, b) ->
              let a = self#loc self#string a in
              let b = self#option self#jkind_annotation b in
              a, b)
            a
        in
        Pparam_newtypes a

    method function_param : function_param -> function_param =
      fun { pparam_loc; pparam_desc } ->
      let pparam_loc = self#location pparam_loc in
      let pparam_desc = self#function_param_desc pparam_desc in
      { pparam_loc; pparam_desc }

    method function_body : function_body -> function_body =
      fun { pfb_desc; pfb_loc; pfb_tokens } ->
      let pfb_desc = self#function_body_desc pfb_desc in
      let pfb_loc = self#location pfb_loc in
      let pfb_tokens = self#token_seq pfb_tokens in
      { pfb_desc; pfb_loc; pfb_tokens }

    method function_body_desc : function_body_desc -> function_body_desc =
      fun x ->
      match x with
      | Pfunction_body a ->
        let a = self#expression a in
        Pfunction_body a
      | Pfunction_cases (a, b) ->
        let a = self#list self#case a in
        let b = self#ext_attribute b in
        Pfunction_cases (a, b)

    method type_constraint : type_constraint -> type_constraint =
      fun x ->
      match x with
      | Pconstraint a ->
        let a = self#core_type a in
        Pconstraint a
      | Pcoerce (a, b) ->
        let a = self#option self#core_type a in
        let b = self#core_type b in
        Pcoerce (a, b)

    method function_constraint : function_constraint -> function_constraint =
      fun { ret_mode_annotations; ret_type_constraint } ->
      let ret_mode_annotations = self#modes ret_mode_annotations in
      let ret_type_constraint =
        self#option self#type_constraint ret_type_constraint
      in
      { ret_mode_annotations; ret_type_constraint }

    method block_access : block_access -> block_access =
      fun x ->
      match x with
      | Baccess_field a ->
        let a = self#loc self#longident a in
        Baccess_field a
      | Baccess_array (a, b, c) ->
        let a = self#mutable_flag a in
        let b = self#index_kind b in
        let c = self#expression c in
        Baccess_array (a, b, c)
      | Baccess_block (a, b) ->
        let a = self#mutable_flag a in
        let b = self#expression b in
        Baccess_block (a, b)

    method unboxed_access : unboxed_access -> unboxed_access =
      fun x ->
      match x with
      | Uaccess_unboxed_field a ->
        let a = self#loc self#longident a in
        Uaccess_unboxed_field a

    method comprehension_iterator
      : comprehension_iterator -> comprehension_iterator
      =
      fun x ->
      match x with
      | Pcomp_range { start; stop; direction } ->
        let start = self#expression start in
        let stop = self#expression stop in
        let direction = self#direction_flag direction in
        Pcomp_range { start; stop; direction }
      | Pcomp_in a ->
        let a = self#expression a in
        Pcomp_in a

    method comprehension_clause_binding
      : comprehension_clause_binding -> comprehension_clause_binding
      =
      fun
        { pcomp_cb_mode
        ; pcomp_cb_pattern
        ; pcomp_cb_iterator
        ; pcomp_cb_attributes
        ; pcomp_cb_tokens
        }
        ->
      let pcomp_cb_mode = self#option (self#loc self#mode) pcomp_cb_mode in
      let pcomp_cb_pattern = self#pattern pcomp_cb_pattern in
      let pcomp_cb_iterator = self#comprehension_iterator pcomp_cb_iterator in
      let pcomp_cb_attributes = self#attributes pcomp_cb_attributes in
      let pcomp_cb_tokens = self#token_seq pcomp_cb_tokens in
      { pcomp_cb_mode
      ; pcomp_cb_pattern
      ; pcomp_cb_iterator
      ; pcomp_cb_attributes
      ; pcomp_cb_tokens
      }

    method comprehension_clause : comprehension_clause -> comprehension_clause =
      fun x ->
      match x with
      | Pcomp_for a ->
        let a = self#list self#comprehension_clause_binding a in
        Pcomp_for a
      | Pcomp_when a ->
        let a = self#expression a in
        Pcomp_when a

    method comprehension : comprehension -> comprehension =
      fun { pcomp_body; pcomp_clauses; pcomp_tokens } ->
      let pcomp_body = self#expression pcomp_body in
      let pcomp_clauses = self#list self#comprehension_clause pcomp_clauses in
      let pcomp_tokens = self#token_seq pcomp_tokens in
      { pcomp_body; pcomp_clauses; pcomp_tokens }

    method comprehension_expression
      : comprehension_expression -> comprehension_expression
      =
      fun x ->
      match x with
      | Pcomp_list_comprehension a ->
        let a = self#comprehension a in
        Pcomp_list_comprehension a
      | Pcomp_array_comprehension (a, b) ->
        let a = self#mutable_flag a in
        let b = self#comprehension b in
        Pcomp_array_comprehension (a, b)

    method value_description : value_description -> value_description =
      fun
        { pval_pre_doc
        ; pval_ext_attrs
        ; pval_name
        ; pval_type
        ; pval_modalities
        ; pval_prim
        ; pval_attributes
        ; pval_post_doc
        ; pval_loc
        ; pval_tokens
        }
        ->
      let pval_pre_doc = self#option self#string pval_pre_doc in
      let pval_ext_attrs = self#ext_attribute pval_ext_attrs in
      let pval_name = self#loc self#longident_str_or_op pval_name in
      let pval_type = self#core_type pval_type in
      let pval_modalities = self#modalities pval_modalities in
      let pval_prim = self#list self#string pval_prim in
      let pval_attributes = self#attributes pval_attributes in
      let pval_post_doc = self#option self#string pval_post_doc in
      let pval_loc = self#location pval_loc in
      let pval_tokens = self#token_seq pval_tokens in
      { pval_pre_doc
      ; pval_ext_attrs
      ; pval_name
      ; pval_type
      ; pval_modalities
      ; pval_prim
      ; pval_attributes
      ; pval_post_doc
      ; pval_loc
      ; pval_tokens
      }

    method ptype_param : ptype_param -> ptype_param =
      fun { ptp_typ; ptp_infos; ptp_tokens } ->
      let ptp_typ = self#core_type ptp_typ in
      let ptp_infos =
        (fun (a, b) ->
          let a = self#variance a in
          let b = self#injectivity b in
          a, b)
          ptp_infos
      in
      let ptp_tokens = self#token_seq ptp_tokens in
      { ptp_typ; ptp_infos; ptp_tokens }

    method ptype_params : ptype_params -> ptype_params =
      self#list self#ptype_param

    method ptype_constraint : ptype_constraint -> ptype_constraint =
      fun (a, b, c) ->
      let a = self#core_type a in
      let b = self#core_type b in
      let c = self#location c in
      a, b, c

    method type_declaration : type_declaration -> type_declaration =
      fun
        { ptype_pre_text
        ; ptype_pre_doc
        ; ptype_ext_attrs
        ; ptype_name
        ; ptype_params
        ; ptype_jkind_annotation
        ; ptype_private
        ; ptype_manifest
        ; ptype_kind
        ; ptype_cstrs
        ; ptype_attributes
        ; ptype_post_doc
        ; ptype_loc
        ; ptype_tokens
        }
        ->
      let ptype_pre_text = self#list self#string ptype_pre_text in
      let ptype_pre_doc = self#option self#string ptype_pre_doc in
      let ptype_ext_attrs = self#ext_attribute ptype_ext_attrs in
      let ptype_name = self#loc self#string ptype_name in
      let ptype_params = self#ptype_params ptype_params in
      let ptype_jkind_annotation =
        self#option self#jkind_annotation ptype_jkind_annotation
      in
      let ptype_private = self#private_flag ptype_private in
      let ptype_manifest = self#option self#core_type ptype_manifest in
      let ptype_kind = self#type_kind ptype_kind in
      let ptype_cstrs = self#list self#ptype_constraint ptype_cstrs in
      let ptype_attributes = self#attributes ptype_attributes in
      let ptype_post_doc = self#option self#string ptype_post_doc in
      let ptype_loc = self#location ptype_loc in
      let ptype_tokens = self#token_seq ptype_tokens in
      { ptype_pre_text
      ; ptype_pre_doc
      ; ptype_ext_attrs
      ; ptype_name
      ; ptype_params
      ; ptype_jkind_annotation
      ; ptype_private
      ; ptype_manifest
      ; ptype_kind
      ; ptype_cstrs
      ; ptype_attributes
      ; ptype_post_doc
      ; ptype_loc
      ; ptype_tokens
      }

    method type_kind : type_kind -> type_kind =
      fun x ->
      match x with
      | Ptype_abstract -> Ptype_abstract
      | Ptype_variant a ->
        let a = self#list self#constructor_declaration a in
        Ptype_variant a
      | Ptype_record a ->
        let a = self#list self#label_declaration a in
        Ptype_record a
      | Ptype_record_unboxed_product a ->
        let a = self#list self#label_declaration a in
        Ptype_record_unboxed_product a
      | Ptype_open -> Ptype_open

    method label_declaration : label_declaration -> label_declaration =
      fun
        { pld_name
        ; pld_mutable
        ; pld_global
        ; pld_modalities
        ; pld_type
        ; pld_loc
        ; pld_attributes
        ; pld_doc
        ; pld_tokens
        }
        ->
      let pld_name = self#loc self#string pld_name in
      let pld_mutable = self#mutable_flag pld_mutable in
      let pld_global = self#bool pld_global in
      let pld_modalities = self#modalities pld_modalities in
      let pld_type = self#core_type pld_type in
      let pld_loc = self#location pld_loc in
      let pld_attributes = self#attributes pld_attributes in
      let pld_doc = self#option self#string pld_doc in
      let pld_tokens = self#token_seq pld_tokens in
      { pld_name
      ; pld_mutable
      ; pld_global
      ; pld_modalities
      ; pld_type
      ; pld_loc
      ; pld_attributes
      ; pld_doc
      ; pld_tokens
      }

    method constructor_declaration
      : constructor_declaration -> constructor_declaration
      =
      fun
        { pcd_name
        ; pcd_vars
        ; pcd_args
        ; pcd_res
        ; pcd_loc
        ; pcd_attributes
        ; pcd_doc
        ; pcd_tokens
        }
        ->
      let pcd_name = self#loc self#longident_str_or_op pcd_name in
      let pcd_vars =
        self
        #
        list
          (fun (a, b) ->
            let a = self#loc self#string a in
            let b = self#option self#jkind_annotation b in
            a, b)
          pcd_vars
      in
      let pcd_args = self#constructor_arguments pcd_args in
      let pcd_res = self#option self#core_type pcd_res in
      let pcd_loc = self#location pcd_loc in
      let pcd_attributes = self#attributes pcd_attributes in
      let pcd_doc = self#option self#string pcd_doc in
      let pcd_tokens = self#token_seq pcd_tokens in
      { pcd_name
      ; pcd_vars
      ; pcd_args
      ; pcd_res
      ; pcd_loc
      ; pcd_attributes
      ; pcd_doc
      ; pcd_tokens
      }

    method constructor_argument : constructor_argument -> constructor_argument =
      fun { pca_global; pca_type; pca_modalities; pca_loc } ->
      let pca_global = self#bool pca_global in
      let pca_type = self#core_type pca_type in
      let pca_modalities = self#modalities pca_modalities in
      let pca_loc = self#location pca_loc in
      { pca_global; pca_type; pca_modalities; pca_loc }

    method constructor_arguments
      : constructor_arguments -> constructor_arguments
      =
      fun x ->
      match x with
      | Pcstr_tuple a ->
        let a = self#list self#constructor_argument a in
        Pcstr_tuple a
      | Pcstr_record a ->
        let a = self#list self#label_declaration a in
        Pcstr_record a

    method type_extension : type_extension -> type_extension =
      fun
        { ptyext_pre_doc
        ; ptyext_ext_attrs
        ; ptyext_path
        ; ptyext_params
        ; ptyext_constructors
        ; ptyext_private
        ; ptyext_loc
        ; ptyext_attributes
        ; ptyext_post_doc
        ; ptyext_tokens
        }
        ->
      let ptyext_pre_doc = self#option self#string ptyext_pre_doc in
      let ptyext_ext_attrs = self#ext_attribute ptyext_ext_attrs in
      let ptyext_path = self#loc self#longident ptyext_path in
      let ptyext_params = self#list self#ptype_param ptyext_params in
      let ptyext_constructors =
        self#list self#extension_constructor ptyext_constructors
      in
      let ptyext_private = self#private_flag ptyext_private in
      let ptyext_loc = self#location ptyext_loc in
      let ptyext_attributes = self#attributes ptyext_attributes in
      let ptyext_post_doc = self#option self#string ptyext_post_doc in
      let ptyext_tokens = self#token_seq ptyext_tokens in
      { ptyext_pre_doc
      ; ptyext_ext_attrs
      ; ptyext_path
      ; ptyext_params
      ; ptyext_constructors
      ; ptyext_private
      ; ptyext_loc
      ; ptyext_attributes
      ; ptyext_post_doc
      ; ptyext_tokens
      }

    method extension_constructor
      : extension_constructor -> extension_constructor
      =
      fun
        { pext_name
        ; pext_kind
        ; pext_loc
        ; pext_attributes
        ; pext_doc
        ; pext_tokens
        }
        ->
      let pext_name = self#loc self#longident_str_or_op pext_name in
      let pext_kind = self#extension_constructor_kind pext_kind in
      let pext_loc = self#location pext_loc in
      let pext_attributes = self#attributes pext_attributes in
      let pext_doc = self#option self#string pext_doc in
      let pext_tokens = self#token_seq pext_tokens in
      { pext_name; pext_kind; pext_loc; pext_attributes; pext_doc; pext_tokens }

    method type_exception : type_exception -> type_exception =
      fun
        { ptyexn_pre_doc
        ; ptyexn_ext_attrs
        ; ptyexn_constructor
        ; ptyexn_loc
        ; ptyexn_attributes
        ; ptyexn_post_doc
        ; ptyexn_tokens
        }
        ->
      let ptyexn_pre_doc = self#option self#string ptyexn_pre_doc in
      let ptyexn_ext_attrs = self#ext_attribute ptyexn_ext_attrs in
      let ptyexn_constructor = self#extension_constructor ptyexn_constructor in
      let ptyexn_loc = self#location ptyexn_loc in
      let ptyexn_attributes = self#attributes ptyexn_attributes in
      let ptyexn_post_doc = self#option self#string ptyexn_post_doc in
      let ptyexn_tokens = self#token_seq ptyexn_tokens in
      { ptyexn_pre_doc
      ; ptyexn_ext_attrs
      ; ptyexn_constructor
      ; ptyexn_loc
      ; ptyexn_attributes
      ; ptyexn_post_doc
      ; ptyexn_tokens
      }

    method extension_constructor_kind
      : extension_constructor_kind -> extension_constructor_kind
      =
      fun x ->
      match x with
      | Pext_decl (a, b, c) ->
        let a =
          self
          #
          list
            (fun (a, b) ->
              let a = self#loc self#string a in
              let b = self#option self#jkind_annotation b in
              a, b)
            a
        in
        let b = self#constructor_arguments b in
        let c = self#option self#core_type c in
        Pext_decl (a, b, c)
      | Pext_rebind a ->
        let a = self#loc self#longident a in
        Pext_rebind a

    method class_type : class_type -> class_type =
      fun { pcty_desc; pcty_loc; pcty_attributes; pcty_tokens } ->
      let pcty_desc = self#class_type_desc pcty_desc in
      let pcty_loc = self#location pcty_loc in
      let pcty_attributes = self#attributes pcty_attributes in
      let pcty_tokens = self#token_seq pcty_tokens in
      { pcty_desc; pcty_loc; pcty_attributes; pcty_tokens }

    method class_type_desc : class_type_desc -> class_type_desc =
      fun x ->
      match x with
      | Pcty_constr (a, b) ->
        let a = self#loc self#longident a in
        let b = self#list self#core_type b in
        Pcty_constr (a, b)
      | Pcty_signature a ->
        let a = self#class_signature a in
        Pcty_signature a
      | Pcty_arrow (a, b) ->
        let a = self#arrow_arg a in
        let b = self#class_type b in
        Pcty_arrow (a, b)
      | Pcty_extension a ->
        let a = self#extension a in
        Pcty_extension a
      | Pcty_open (a, b) ->
        let a = self#open_description a in
        let b = self#class_type b in
        Pcty_open (a, b)

    method class_signature : class_signature -> class_signature =
      fun { pcsig_self; pcsig_fields } ->
      let pcsig_self = self#option self#core_type pcsig_self in
      let pcsig_fields = self#list self#class_type_field pcsig_fields in
      { pcsig_self; pcsig_fields }

    method class_type_field : class_type_field -> class_type_field =
      fun
        { pctf_pre_doc
        ; pctf_desc
        ; pctf_loc
        ; pctf_attributes
        ; pctf_post_doc
        ; pctf_tokens
        }
        ->
      let pctf_pre_doc = self#option self#string pctf_pre_doc in
      let pctf_desc = self#class_type_field_desc pctf_desc in
      let pctf_loc = self#location pctf_loc in
      let pctf_attributes = self#attributes pctf_attributes in
      let pctf_post_doc = self#option self#string pctf_post_doc in
      let pctf_tokens = self#token_seq pctf_tokens in
      { pctf_pre_doc
      ; pctf_desc
      ; pctf_loc
      ; pctf_attributes
      ; pctf_post_doc
      ; pctf_tokens
      }

    method class_type_field_desc
      : class_type_field_desc -> class_type_field_desc
      =
      fun x ->
      match x with
      | Pctf_inherit a ->
        let a = self#class_type a in
        Pctf_inherit a
      | Pctf_val a ->
        let a =
          (fun (a, b, c, d) ->
            let a = self#loc self#string a in
            let b = self#mutable_flag b in
            let c = self#virtual_flag c in
            let d = self#core_type d in
            a, b, c, d)
            a
        in
        Pctf_val a
      | Pctf_method a ->
        let a =
          (fun (a, b, c, d) ->
            let a = self#loc self#string a in
            let b = self#private_flag b in
            let c = self#virtual_flag c in
            let d = self#core_type d in
            a, b, c, d)
            a
        in
        Pctf_method a
      | Pctf_constraint a ->
        let a =
          (fun (a, b) ->
            let a = self#core_type a in
            let b = self#core_type b in
            a, b)
            a
        in
        Pctf_constraint a
      | Pctf_attribute a ->
        let a = self#attribute a in
        Pctf_attribute a
      | Pctf_extension a ->
        let a = self#extension a in
        Pctf_extension a
      | Pctf_docstring a ->
        let a = self#string a in
        Pctf_docstring a

    method class_infos : 'a. ('a -> 'a) -> 'a class_infos -> 'a class_infos =
      fun
        _a
        { pci_pre_text
        ; pci_pre_doc
        ; pci_virt
        ; pci_ext_attrs
        ; pci_params
        ; pci_name
        ; pci_value_params
        ; pci_constraint
        ; pci_expr
        ; pci_loc
        ; pci_attributes
        ; pci_post_doc
        ; pci_tokens
        }
        ->
      let pci_pre_text = self#list self#string pci_pre_text in
      let pci_pre_doc = self#option self#string pci_pre_doc in
      let pci_virt = self#virtual_flag pci_virt in
      let pci_ext_attrs = self#ext_attribute pci_ext_attrs in
      let pci_params = self#list self#ptype_param pci_params in
      let pci_name = self#loc self#string pci_name in
      let pci_value_params =
        self#list (self#argument self#pattern) pci_value_params
      in
      let pci_constraint = self#option self#class_type pci_constraint in
      let pci_expr = _a pci_expr in
      let pci_loc = self#location pci_loc in
      let pci_attributes = self#attributes pci_attributes in
      let pci_post_doc = self#option self#string pci_post_doc in
      let pci_tokens = self#token_seq pci_tokens in
      { pci_pre_text
      ; pci_pre_doc
      ; pci_virt
      ; pci_ext_attrs
      ; pci_params
      ; pci_name
      ; pci_value_params
      ; pci_constraint
      ; pci_expr
      ; pci_loc
      ; pci_attributes
      ; pci_post_doc
      ; pci_tokens
      }

    method class_description : class_description -> class_description =
      self#class_infos self#class_type

    method class_type_declaration
      : class_type_declaration -> class_type_declaration
      =
      self#class_infos self#class_type

    method class_expr : class_expr -> class_expr =
      fun { pcl_ext_attrs; pcl_desc; pcl_loc; pcl_attributes } ->
      let pcl_ext_attrs = self#ext_attribute pcl_ext_attrs in
      let pcl_desc = self#class_expr_desc pcl_desc in
      let pcl_loc = self#location pcl_loc in
      let pcl_attributes = self#attributes pcl_attributes in
      { pcl_ext_attrs; pcl_desc; pcl_loc; pcl_attributes }

    method class_expr_desc : class_expr_desc -> class_expr_desc =
      fun x ->
      match x with
      | Pcl_constr (a, b) ->
        let a = self#loc self#longident a in
        let b = self#list self#core_type b in
        Pcl_constr (a, b)
      | Pcl_structure a ->
        let a = self#class_structure a in
        Pcl_structure a
      | Pcl_fun (a, b) ->
        let a = self#list (self#argument self#pattern) a in
        let b = self#class_expr b in
        Pcl_fun (a, b)
      | Pcl_apply (a, b) ->
        let a = self#class_expr a in
        let b = self#list (self#argument self#expression) b in
        Pcl_apply (a, b)
      | Pcl_let (a, b, c) ->
        let a = self#rec_flag a in
        let b = self#list self#value_binding b in
        let c = self#class_expr c in
        Pcl_let (a, b, c)
      | Pcl_constraint (a, b) ->
        let a = self#class_expr a in
        let b = self#class_type b in
        Pcl_constraint (a, b)
      | Pcl_extension a ->
        let a = self#extension a in
        Pcl_extension a
      | Pcl_open (a, b) ->
        let a = self#open_description a in
        let b = self#class_expr b in
        Pcl_open (a, b)
      | Pcl_parens a ->
        let a = self#class_expr a in
        Pcl_parens a

    method class_structure : class_structure -> class_structure =
      fun { pcstr_self; pcstr_fields } ->
      let pcstr_self = self#pattern pcstr_self in
      let pcstr_fields = self#list self#class_field pcstr_fields in
      { pcstr_self; pcstr_fields }

    method class_field : class_field -> class_field =
      fun
        { pcf_pre_doc
        ; pcf_desc
        ; pcf_loc
        ; pcf_attributes
        ; pcf_post_doc
        ; pcf_tokens
        }
        ->
      let pcf_pre_doc = self#option self#string pcf_pre_doc in
      let pcf_desc = self#class_field_desc pcf_desc in
      let pcf_loc = self#location pcf_loc in
      let pcf_attributes = self#attributes pcf_attributes in
      let pcf_post_doc = self#option self#string pcf_post_doc in
      let pcf_tokens = self#token_seq pcf_tokens in
      { pcf_pre_doc
      ; pcf_desc
      ; pcf_loc
      ; pcf_attributes
      ; pcf_post_doc
      ; pcf_tokens
      }

    method class_field_desc : class_field_desc -> class_field_desc =
      fun x ->
      match x with
      | Pcf_inherit (a, b, c) ->
        let a = self#override_flag a in
        let b = self#class_expr b in
        let c = self#option (self#loc self#string) c in
        Pcf_inherit (a, b, c)
      | Pcf_val a ->
        let a =
          (fun (a, b, c) ->
            let a = self#loc self#string a in
            let b = self#mutable_flag b in
            let c = self#class_field_kind c in
            a, b, c)
            a
        in
        Pcf_val a
      | Pcf_method a ->
        let a =
          (fun (a, b, c) ->
            let a = self#loc self#string a in
            let b = self#private_flag b in
            let c = self#class_field_kind c in
            a, b, c)
            a
        in
        Pcf_method a
      | Pcf_constraint a ->
        let a =
          (fun (a, b) ->
            let a = self#core_type a in
            let b = self#core_type b in
            a, b)
            a
        in
        Pcf_constraint a
      | Pcf_initializer a ->
        let a = self#expression a in
        Pcf_initializer a
      | Pcf_attribute a ->
        let a = self#attribute a in
        Pcf_attribute a
      | Pcf_extension a ->
        let a = self#extension a in
        Pcf_extension a
      | Pcf_docstring a ->
        let a = self#string a in
        Pcf_docstring a

    method class_field_kind : class_field_kind -> class_field_kind =
      fun x ->
      match x with
      | Cfk_virtual a ->
        let a = self#core_type a in
        Cfk_virtual a
      | Cfk_concrete (a, b) ->
        let a = self#override_flag a in
        let b = self#value_binding b in
        Cfk_concrete (a, b)

    method class_declaration : class_declaration -> class_declaration =
      self#class_infos self#class_expr

    method module_type : module_type -> module_type =
      fun { pmty_desc; pmty_loc; pmty_attributes; pmty_tokens } ->
      let pmty_desc = self#module_type_desc pmty_desc in
      let pmty_loc = self#location pmty_loc in
      let pmty_attributes = self#attributes pmty_attributes in
      let pmty_tokens = self#token_seq pmty_tokens in
      { pmty_desc; pmty_loc; pmty_attributes; pmty_tokens }

    method module_type_desc : module_type_desc -> module_type_desc =
      fun x ->
      match x with
      | Pmty_ident a ->
        let a = self#loc self#longident a in
        Pmty_ident a
      | Pmty_signature a ->
        let a = self#signature a in
        Pmty_signature a
      | Pmty_functor (a, b, c, d) ->
        let a = self#attributes a in
        let b = self#list self#functor_parameter b in
        let c = self#module_type c in
        let d = self#modes d in
        Pmty_functor (a, b, c, d)
      | Pmty_functor_type (a, b, c) ->
        let a = self#list self#functor_parameter a in
        let b = self#module_type b in
        let c = self#modes c in
        Pmty_functor_type (a, b, c)
      | Pmty_with (a, b) ->
        let a = self#module_type a in
        let b = self#list self#with_constraint b in
        Pmty_with (a, b)
      | Pmty_typeof (a, b) ->
        let a = self#attributes a in
        let b = self#module_expr b in
        Pmty_typeof (a, b)
      | Pmty_extension a ->
        let a = self#extension a in
        Pmty_extension a
      | Pmty_alias a ->
        let a = self#loc self#longident a in
        Pmty_alias a
      | Pmty_strengthen (a, b) ->
        let a = self#module_type a in
        let b = self#loc self#longident b in
        Pmty_strengthen (a, b)
      | Pmty_parens a ->
        let a = self#module_type a in
        Pmty_parens a

    method functor_parameter : functor_parameter -> functor_parameter =
      fun x ->
      match x with
      | Unit -> Unit
      | Named (a, b, c) ->
        let a = self#loc (self#option self#string) a in
        let b = self#module_type b in
        let c = self#modes c in
        Named (a, b, c)
      | Unnamed (a, b) ->
        let a = self#module_type a in
        let b = self#modes b in
        Unnamed (a, b)

    method signature : signature -> signature =
      fun { psg_modalities; psg_items; psg_loc; psg_tokens } ->
      let psg_modalities = self#modalities psg_modalities in
      let psg_items = self#list self#signature_item psg_items in
      let psg_loc = self#location psg_loc in
      let psg_tokens = self#token_seq psg_tokens in
      { psg_modalities; psg_items; psg_loc; psg_tokens }

    method signature_item : signature_item -> signature_item =
      fun { psig_desc; psig_loc; psig_tokens } ->
      let psig_desc = self#signature_item_desc psig_desc in
      let psig_loc = self#location psig_loc in
      let psig_tokens = self#token_seq psig_tokens in
      { psig_desc; psig_loc; psig_tokens }

    method signature_item_desc : signature_item_desc -> signature_item_desc =
      fun x ->
      match x with
      | Psig_value a ->
        let a = self#value_description a in
        Psig_value a
      | Psig_type (a, b) ->
        let a = self#rec_flag a in
        let b = self#list self#type_declaration b in
        Psig_type (a, b)
      | Psig_typesubst a ->
        let a = self#list self#type_declaration a in
        Psig_typesubst a
      | Psig_typext a ->
        let a = self#type_extension a in
        Psig_typext a
      | Psig_exception a ->
        let a = self#type_exception a in
        Psig_exception a
      | Psig_module a ->
        let a = self#module_declaration a in
        Psig_module a
      | Psig_modsubst a ->
        let a = self#module_substitution a in
        Psig_modsubst a
      | Psig_recmodule a ->
        let a = self#list self#module_declaration a in
        Psig_recmodule a
      | Psig_modtype a ->
        let a = self#module_type_declaration a in
        Psig_modtype a
      | Psig_modtypesubst a ->
        let a = self#module_type_declaration a in
        Psig_modtypesubst a
      | Psig_open a ->
        let a = self#open_description a in
        Psig_open a
      | Psig_include (a, b) ->
        let a = self#include_description a in
        let b = self#modalities b in
        Psig_include (a, b)
      | Psig_class a ->
        let a = self#list self#class_description a in
        Psig_class a
      | Psig_class_type a ->
        let a = self#list self#class_type_declaration a in
        Psig_class_type a
      | Psig_attribute a ->
        let a = self#attribute a in
        Psig_attribute a
      | Psig_extension a ->
        let a = self#toplevel_extension a in
        Psig_extension a
      | Psig_kind_abbrev (a, b) ->
        let a = self#loc self#string a in
        let b = self#jkind_annotation b in
        Psig_kind_abbrev (a, b)
      | Psig_docstring a ->
        let a = self#string a in
        Psig_docstring a

    method module_declaration_body
      : module_declaration_body -> module_declaration_body
      =
      fun x ->
      match x with
      | With_params (a, b, c) ->
        let a = self#list self#functor_parameter a in
        let b = self#module_type b in
        let c = self#modes c in
        With_params (a, b, c)
      | Without_params (a, b) ->
        let a = self#module_type a in
        let b = self#modalities b in
        Without_params (a, b)

    method module_declaration : module_declaration -> module_declaration =
      fun
        { pmd_pre_text
        ; pmd_pre_doc
        ; pmd_ext_attrs
        ; pmd_name
        ; pmd_body
        ; pmd_attributes
        ; pmd_post_doc
        ; pmd_loc
        ; pmd_tokens
        }
        ->
      let pmd_pre_text = self#list self#string pmd_pre_text in
      let pmd_pre_doc = self#option self#string pmd_pre_doc in
      let pmd_ext_attrs = self#ext_attribute pmd_ext_attrs in
      let pmd_name =
        (fun (a, b) ->
          let a = self#loc (self#option self#string) a in
          let b = self#modalities b in
          a, b)
          pmd_name
      in
      let pmd_body = self#module_declaration_body pmd_body in
      let pmd_attributes = self#attributes pmd_attributes in
      let pmd_post_doc = self#option self#string pmd_post_doc in
      let pmd_loc = self#location pmd_loc in
      let pmd_tokens = self#token_seq pmd_tokens in
      { pmd_pre_text
      ; pmd_pre_doc
      ; pmd_ext_attrs
      ; pmd_name
      ; pmd_body
      ; pmd_attributes
      ; pmd_post_doc
      ; pmd_loc
      ; pmd_tokens
      }

    method module_substitution : module_substitution -> module_substitution =
      fun
        { pms_pre_doc
        ; pms_ext_attrs
        ; pms_name
        ; pms_manifest
        ; pms_attributes
        ; pms_post_doc
        ; pms_loc
        ; pms_tokens
        }
        ->
      let pms_pre_doc = self#option self#string pms_pre_doc in
      let pms_ext_attrs = self#ext_attribute pms_ext_attrs in
      let pms_name = self#loc self#string pms_name in
      let pms_manifest = self#loc self#longident pms_manifest in
      let pms_attributes = self#attributes pms_attributes in
      let pms_post_doc = self#option self#string pms_post_doc in
      let pms_loc = self#location pms_loc in
      let pms_tokens = self#token_seq pms_tokens in
      { pms_pre_doc
      ; pms_ext_attrs
      ; pms_name
      ; pms_manifest
      ; pms_attributes
      ; pms_post_doc
      ; pms_loc
      ; pms_tokens
      }

    method module_type_declaration
      : module_type_declaration -> module_type_declaration
      =
      fun
        { pmtd_pre_doc
        ; pmtd_ext_attrs
        ; pmtd_name
        ; pmtd_type
        ; pmtd_attributes
        ; pmtd_post_doc
        ; pmtd_loc
        ; pmtd_tokens
        }
        ->
      let pmtd_pre_doc = self#option self#string pmtd_pre_doc in
      let pmtd_ext_attrs = self#ext_attribute pmtd_ext_attrs in
      let pmtd_name = self#loc self#string pmtd_name in
      let pmtd_type = self#option self#module_type pmtd_type in
      let pmtd_attributes = self#attributes pmtd_attributes in
      let pmtd_post_doc = self#option self#string pmtd_post_doc in
      let pmtd_loc = self#location pmtd_loc in
      let pmtd_tokens = self#token_seq pmtd_tokens in
      { pmtd_pre_doc
      ; pmtd_ext_attrs
      ; pmtd_name
      ; pmtd_type
      ; pmtd_attributes
      ; pmtd_post_doc
      ; pmtd_loc
      ; pmtd_tokens
      }

    method open_infos : 'a. ('a -> 'a) -> 'a open_infos -> 'a open_infos =
      fun
        _a
        { popen_pre_doc
        ; popen_ext_attrs
        ; popen_expr
        ; popen_override
        ; popen_loc
        ; popen_attributes
        ; popen_post_doc
        ; popen_tokens
        }
        ->
      let popen_pre_doc = self#option self#string popen_pre_doc in
      let popen_ext_attrs = self#ext_attribute popen_ext_attrs in
      let popen_expr = _a popen_expr in
      let popen_override = self#override_flag popen_override in
      let popen_loc = self#location popen_loc in
      let popen_attributes = self#attributes popen_attributes in
      let popen_post_doc = self#option self#string popen_post_doc in
      let popen_tokens = self#token_seq popen_tokens in
      { popen_pre_doc
      ; popen_ext_attrs
      ; popen_expr
      ; popen_override
      ; popen_loc
      ; popen_attributes
      ; popen_post_doc
      ; popen_tokens
      }

    method open_description : open_description -> open_description =
      self#open_infos (self#loc self#longident)

    method open_declaration : open_declaration -> open_declaration =
      self#open_infos self#module_expr

    method include_infos
      : 'a. ('a -> 'a) -> 'a include_infos -> 'a include_infos
      =
      fun
        _a
        { pincl_pre_doc
        ; pincl_kind
        ; pincl_ext_attrs
        ; pincl_mod
        ; pincl_loc
        ; pincl_attributes
        ; pincl_post_doc
        ; pincl_tokens
        }
        ->
      let pincl_pre_doc = self#option self#string pincl_pre_doc in
      let pincl_kind = self#include_kind pincl_kind in
      let pincl_ext_attrs = self#ext_attribute pincl_ext_attrs in
      let pincl_mod = _a pincl_mod in
      let pincl_loc = self#location pincl_loc in
      let pincl_attributes = self#attributes pincl_attributes in
      let pincl_post_doc = self#option self#string pincl_post_doc in
      let pincl_tokens = self#token_seq pincl_tokens in
      { pincl_pre_doc
      ; pincl_kind
      ; pincl_ext_attrs
      ; pincl_mod
      ; pincl_loc
      ; pincl_attributes
      ; pincl_post_doc
      ; pincl_tokens
      }

    method include_description : include_description -> include_description =
      self#include_infos self#module_type

    method include_declaration : include_declaration -> include_declaration =
      self#include_infos self#module_expr

    method with_constraint : with_constraint -> with_constraint =
      fun { wc_desc; wc_loc; wc_tokens } ->
      let wc_desc = self#with_constraint_desc wc_desc in
      let wc_loc = self#location wc_loc in
      let wc_tokens = self#token_seq wc_tokens in
      { wc_desc; wc_loc; wc_tokens }

    method with_constraint_desc : with_constraint_desc -> with_constraint_desc =
      fun x ->
      match x with
      | Pwith_type (a, b, c, d, e) ->
        let a = self#ptype_params a in
        let b = self#loc self#longident b in
        let c = self#private_flag c in
        let d = self#core_type d in
        let e = self#list self#ptype_constraint e in
        Pwith_type (a, b, c, d, e)
      | Pwith_module (a, b) ->
        let a = self#loc self#longident a in
        let b = self#loc self#longident b in
        Pwith_module (a, b)
      | Pwith_modtype (a, b) ->
        let a = self#loc self#longident a in
        let b = self#module_type b in
        Pwith_modtype (a, b)
      | Pwith_modtypesubst (a, b) ->
        let a = self#loc self#longident a in
        let b = self#module_type b in
        Pwith_modtypesubst (a, b)
      | Pwith_typesubst (a, b, c) ->
        let a = self#ptype_params a in
        let b = self#loc self#longident b in
        let c = self#core_type c in
        Pwith_typesubst (a, b, c)
      | Pwith_modsubst (a, b) ->
        let a = self#loc self#longident a in
        let b = self#loc self#longident b in
        Pwith_modsubst (a, b)

    method module_expr : module_expr -> module_expr =
      fun { pmod_desc; pmod_loc; pmod_attributes; pmod_tokens } ->
      let pmod_desc = self#module_expr_desc pmod_desc in
      let pmod_loc = self#location pmod_loc in
      let pmod_attributes = self#attributes pmod_attributes in
      let pmod_tokens = self#token_seq pmod_tokens in
      { pmod_desc; pmod_loc; pmod_attributes; pmod_tokens }

    method module_expr_desc : module_expr_desc -> module_expr_desc =
      fun x ->
      match x with
      | Pmod_ident a ->
        let a = self#loc self#longident a in
        Pmod_ident a
      | Pmod_structure (a, b) ->
        let a = self#attributes a in
        let b = self#structure b in
        Pmod_structure (a, b)
      | Pmod_functor (a, b, c) ->
        let a = self#attributes a in
        let b = self#list self#functor_parameter b in
        let c = self#module_expr c in
        Pmod_functor (a, b, c)
      | Pmod_apply (a, b) ->
        let a = self#module_expr a in
        let b = self#module_expr b in
        Pmod_apply (a, b)
      | Pmod_apply_unit a ->
        let a = self#module_expr a in
        Pmod_apply_unit a
      | Pmod_constraint (a, b, c) ->
        let a = self#module_expr a in
        let b = self#option self#module_type b in
        let c = self#modes c in
        Pmod_constraint (a, b, c)
      | Pmod_unpack (a, b, c) ->
        let a = self#expression a in
        let b = self#option self#package_type b in
        let c = self#option self#package_type c in
        Pmod_unpack (a, b, c)
      | Pmod_extension a ->
        let a = self#extension a in
        Pmod_extension a
      | Pmod_parens a ->
        let a = self#module_expr a in
        Pmod_parens a

    method structure : structure -> structure =
      fun (a, b) ->
      let a = self#list self#structure_item a in
      let b = self#token_seq b in
      a, b

    method structure_item : structure_item -> structure_item =
      fun { pstr_desc; pstr_loc; pstr_tokens } ->
      let pstr_desc = self#structure_item_desc pstr_desc in
      let pstr_loc = self#location pstr_loc in
      let pstr_tokens = self#token_seq pstr_tokens in
      { pstr_desc; pstr_loc; pstr_tokens }

    method structure_item_desc : structure_item_desc -> structure_item_desc =
      fun x ->
      match x with
      | Pstr_eval (a, b) ->
        let a = self#expression a in
        let b = self#attributes b in
        Pstr_eval (a, b)
      | Pstr_value (a, b) ->
        let a = self#rec_flag a in
        let b = self#list self#value_binding b in
        Pstr_value (a, b)
      | Pstr_primitive a ->
        let a = self#value_description a in
        Pstr_primitive a
      | Pstr_type (a, b) ->
        let a = self#rec_flag a in
        let b = self#list self#type_declaration b in
        Pstr_type (a, b)
      | Pstr_typext a ->
        let a = self#type_extension a in
        Pstr_typext a
      | Pstr_exception a ->
        let a = self#type_exception a in
        Pstr_exception a
      | Pstr_module a ->
        let a = self#module_binding a in
        Pstr_module a
      | Pstr_recmodule a ->
        let a = self#list self#module_binding a in
        Pstr_recmodule a
      | Pstr_modtype a ->
        let a = self#module_type_declaration a in
        Pstr_modtype a
      | Pstr_open a ->
        let a = self#open_declaration a in
        Pstr_open a
      | Pstr_class a ->
        let a = self#list self#class_declaration a in
        Pstr_class a
      | Pstr_class_type a ->
        let a = self#list self#class_type_declaration a in
        Pstr_class_type a
      | Pstr_include a ->
        let a = self#include_declaration a in
        Pstr_include a
      | Pstr_attribute a ->
        let a = self#attribute a in
        Pstr_attribute a
      | Pstr_extension a ->
        let a = self#toplevel_extension a in
        Pstr_extension a
      | Pstr_kind_abbrev (a, b) ->
        let a = self#loc self#string a in
        let b = self#jkind_annotation b in
        Pstr_kind_abbrev (a, b)
      | Pstr_docstring a ->
        let a = self#string a in
        Pstr_docstring a

    method value_constraint : value_constraint -> value_constraint =
      fun x ->
      match x with
      | Pvc_constraint { locally_abstract_univars; typ } ->
        let locally_abstract_univars =
          self
          #
          list
            (fun (a, b) ->
              let a = self#loc self#string a in
              let b = self#option self#jkind_annotation b in
              a, b)
            locally_abstract_univars
        in
        let typ = self#core_type typ in
        Pvc_constraint { locally_abstract_univars; typ }
      | Pvc_coercion { ground; coercion } ->
        let ground = self#option self#core_type ground in
        let coercion = self#core_type coercion in
        Pvc_coercion { ground; coercion }

    method value_binding : value_binding -> value_binding =
      fun
        { pvb_pre_text
        ; pvb_pre_doc
        ; pvb_ext_attrs
        ; pvb_legacy_modes
        ; pvb_pat
        ; pvb_modes
        ; pvb_params
        ; pvb_constraint
        ; pvb_expr
        ; pvb_ret_modes
        ; pvb_attributes
        ; pvb_post_doc
        ; pvb_loc
        ; pvb_tokens
        }
        ->
      let pvb_pre_text = self#list self#string pvb_pre_text in
      let pvb_pre_doc = self#option self#string pvb_pre_doc in
      let pvb_ext_attrs = self#ext_attribute pvb_ext_attrs in
      let pvb_legacy_modes = self#modes pvb_legacy_modes in
      let pvb_pat = self#pattern pvb_pat in
      let pvb_modes = self#modes pvb_modes in
      let pvb_params = self#list self#function_param pvb_params in
      let pvb_constraint = self#option self#value_constraint pvb_constraint in
      let pvb_expr = self#option self#expression pvb_expr in
      let pvb_ret_modes = self#modes pvb_ret_modes in
      let pvb_attributes = self#attributes pvb_attributes in
      let pvb_post_doc = self#option self#string pvb_post_doc in
      let pvb_loc = self#location pvb_loc in
      let pvb_tokens = self#token_seq pvb_tokens in
      { pvb_pre_text
      ; pvb_pre_doc
      ; pvb_ext_attrs
      ; pvb_legacy_modes
      ; pvb_pat
      ; pvb_modes
      ; pvb_params
      ; pvb_constraint
      ; pvb_expr
      ; pvb_ret_modes
      ; pvb_attributes
      ; pvb_post_doc
      ; pvb_loc
      ; pvb_tokens
      }

    method module_binding : module_binding -> module_binding =
      fun
        { pmb_pre_text
        ; pmb_pre_doc
        ; pmb_ext_attrs
        ; pmb_name
        ; pmb_params
        ; pmb_constraint
        ; pmb_modes
        ; pmb_expr
        ; pmb_attributes
        ; pmb_post_doc
        ; pmb_loc
        ; pmb_tokens
        }
        ->
      let pmb_pre_text = self#list self#string pmb_pre_text in
      let pmb_pre_doc = self#option self#string pmb_pre_doc in
      let pmb_ext_attrs = self#ext_attribute pmb_ext_attrs in
      let pmb_name =
        (fun (a, b) ->
          let a = self#loc (self#option self#string) a in
          let b = self#modes b in
          a, b)
          pmb_name
      in
      let pmb_params = self#list self#functor_parameter pmb_params in
      let pmb_constraint = self#option self#module_type pmb_constraint in
      let pmb_modes = self#modes pmb_modes in
      let pmb_expr = self#module_expr pmb_expr in
      let pmb_attributes = self#attributes pmb_attributes in
      let pmb_post_doc = self#option self#string pmb_post_doc in
      let pmb_loc = self#location pmb_loc in
      let pmb_tokens = self#token_seq pmb_tokens in
      { pmb_pre_text
      ; pmb_pre_doc
      ; pmb_ext_attrs
      ; pmb_name
      ; pmb_params
      ; pmb_constraint
      ; pmb_modes
      ; pmb_expr
      ; pmb_attributes
      ; pmb_post_doc
      ; pmb_loc
      ; pmb_tokens
      }

    method jkind_annotation_desc
      : jkind_annotation_desc -> jkind_annotation_desc
      =
      fun x ->
      match x with
      | Pjk_default -> Pjk_default
      | Pjk_abbreviation a ->
        let a = self#string a in
        Pjk_abbreviation a
      | Pjk_mod (a, b) ->
        let a = self#jkind_annotation a in
        let b = self#modes b in
        Pjk_mod (a, b)
      | Pjk_with (a, b, c) ->
        let a = self#jkind_annotation a in
        let b = self#core_type b in
        let c = self#modalities c in
        Pjk_with (a, b, c)
      | Pjk_kind_of a ->
        let a = self#core_type a in
        Pjk_kind_of a
      | Pjk_product a ->
        let a = self#list self#jkind_annotation a in
        Pjk_product a
      | Pjk_parens a ->
        let a = self#jkind_annotation_desc a in
        Pjk_parens a

    method jkind_annotation : jkind_annotation -> jkind_annotation =
      fun { pjkind_loc; pjkind_desc; pjkind_tokens } ->
      let pjkind_loc = self#location pjkind_loc in
      let pjkind_desc = self#jkind_annotation_desc pjkind_desc in
      let pjkind_tokens = self#token_seq pjkind_tokens in
      { pjkind_loc; pjkind_desc; pjkind_tokens }

    method use_file : use_file -> use_file =
      fun (a, b) ->
      let a = self#list self#toplevel_phrase a in
      let b = self#token_seq b in
      a, b

    method toplevel_phrase : toplevel_phrase -> toplevel_phrase =
      fun x ->
      match x with
      | Ptop_def a ->
        let a = self#structure a in
        Ptop_def a
      | Ptop_dir a ->
        let a = self#toplevel_directive a in
        Ptop_dir a
      | Ptop_lex a ->
        let a = self#lexer_directive a in
        Ptop_lex a

    method toplevel_directive : toplevel_directive -> toplevel_directive =
      fun { pdir_name; pdir_arg; pdir_loc; pdir_tokens } ->
      let pdir_name = self#loc self#string pdir_name in
      let pdir_arg = self#option self#directive_argument pdir_arg in
      let pdir_loc = self#location pdir_loc in
      let pdir_tokens = self#token_seq pdir_tokens in
      { pdir_name; pdir_arg; pdir_loc; pdir_tokens }

    method directive_argument : directive_argument -> directive_argument =
      fun { pdira_desc; pdira_loc } ->
      let pdira_desc = self#directive_argument_desc pdira_desc in
      let pdira_loc = self#location pdira_loc in
      { pdira_desc; pdira_loc }

    method directive_argument_desc
      : directive_argument_desc -> directive_argument_desc
      =
      fun x ->
      match x with
      | Pdir_string a ->
        let a = self#string a in
        Pdir_string a
      | Pdir_int (a, b) ->
        let a = self#string a in
        let b = self#option self#char b in
        Pdir_int (a, b)
      | Pdir_ident a ->
        let a = self#longident a in
        Pdir_ident a
      | Pdir_bool a ->
        let a = self#bool a in
        Pdir_bool a

    method syntax_directive : syntax_directive -> syntax_directive =
      fun { psyn_mode; psyn_toggle } ->
      let psyn_mode = self#loc self#string psyn_mode in
      let psyn_toggle = self#bool psyn_toggle in
      { psyn_mode; psyn_toggle }

    method lexer_directive_desc : lexer_directive_desc -> lexer_directive_desc =
      fun x ->
      match x with
      | Plex_syntax a ->
        let a = self#syntax_directive a in
        Plex_syntax a

    method lexer_directive : lexer_directive -> lexer_directive =
      fun { plex_desc; plex_loc; plex_tokens } ->
      let plex_desc = self#lexer_directive_desc plex_desc in
      let plex_loc = self#location plex_loc in
      let plex_tokens = self#token_seq plex_tokens in
      { plex_desc; plex_loc; plex_tokens }
  end

class virtual iter =
  object(self)
    method virtual bool : bool -> unit

    method virtual char : char -> unit

    method virtual int : int -> unit

    method virtual list : 'a. ('a -> unit) -> 'a list -> unit

    method virtual option : 'a. ('a -> unit) -> 'a option -> unit

    method virtual ref : 'a. ('a -> unit) -> 'a ref -> unit

    method virtual string : string -> unit

    method virtual token : token -> unit

    method position : position -> unit =
      fun { pos_fname; pos_lnum; pos_bol; pos_cnum } ->
      self#string pos_fname;
      self#int pos_lnum;
      self#int pos_bol;
      self#int pos_cnum

    method location : location -> unit =
      fun { loc_start; loc_end; loc_ghost } ->
      self#position loc_start;
      self#position loc_end;
      self#bool loc_ghost

    method longident_dotop_delims : longident_dotop_delims -> unit = fun _ -> ()

    method longident_str_or_op : longident_str_or_op -> unit =
      fun x ->
      match x with
      | Str a -> self#string a
      | Str_trailing_hash a -> self#string a
      | Op a -> self#string a
      | DotOp (a, b, c, d) ->
        self#string a;
        self#longident_dotop_delims b;
        self#string c;
        self#bool d

    method longident_lid_desc : longident_lid_desc -> unit =
      fun x ->
      match x with
      | Lident a -> self#longident_str_or_op a
      | Ldot (a, b) ->
        self#longident a;
        self#longident_str_or_op b
      | Lapply (a, b) ->
        self#longident a;
        self#longident b

    method longident : longident -> unit =
      fun { desc; tokens } ->
      self#longident_lid_desc desc;
      self#token_seq tokens

    method attachment : attachment -> unit = fun _ -> ()

    method comment : comment -> unit =
      fun { text; attachement; explicitely_inserted } ->
      self#string text;
      self#attachment attachement;
      self#ref self#bool explicitely_inserted

    method token_desc : token_desc -> unit =
      fun x ->
      match x with
      | Token (a, b) ->
        self#token a;
        self#bool b
      | Comment a -> self#comment a
      | Child_node -> ()

    method token_elt : token_elt -> unit =
      fun { desc; pos } ->
      self#token_desc desc;
      self#position pos

    method token_seq : token_seq -> unit = self#list self#token_elt

    method rec_flag : rec_flag -> unit = fun _ -> ()

    method direction_flag : direction_flag -> unit = fun _ -> ()

    method private_flag : private_flag -> unit = fun _ -> ()

    method mutable_flag : mutable_flag -> unit = fun _ -> ()

    method virtual_flag : virtual_flag -> unit = fun _ -> ()

    method override_flag : override_flag -> unit = fun _ -> ()

    method closed_flag : closed_flag -> unit = fun _ -> ()

    method label : label -> unit = self#string

    method arg_label : arg_label -> unit =
      fun x ->
      match x with
      | Nolabel -> ()
      | Labelled a -> self#string a
      | Optional a -> self#string a

    method loc : 'a. ('a -> unit) -> 'a loc -> unit =
      fun _a { txt; loc } ->
      _a txt;
      self#location loc

    method variance : variance -> unit = fun _ -> ()

    method injectivity : injectivity -> unit = fun _ -> ()

    method index_kind : index_kind -> unit = fun _ -> ()

    method paren_kind : paren_kind -> unit = fun _ -> ()

    method constant : constant -> unit =
      fun x ->
      match x with
      | Pconst_integer (a, b, c) ->
        self#option self#string a;
        self#string b;
        self#option self#char c
      | Pconst_unboxed_integer (a, b, c) ->
        self#option self#string a;
        self#string b;
        self#char c
      | Pconst_char (a, b) ->
        self#char a;
        self#string b
      | Pconst_untagged_char (a, b) ->
        self#char a;
        self#string b
      | Pconst_string (a, b, c) ->
        self#string a;
        self#location b;
        self#option self#string c
      | Pconst_float (a, b, c) ->
        self#option self#string a;
        self#string b;
        self#option self#char c
      | Pconst_unboxed_float (a, b, c) ->
        self#option self#string a;
        self#string b;
        self#option self#char c

    method modality : modality -> unit =
      fun x ->
      match x with
      | Modality a -> self#string a

    method modalities : modalities -> unit = self#list (self#loc self#modality)

    method mode : mode -> unit =
      fun x ->
      match x with
      | Mode a -> self#string a

    method modes : modes -> unit = self#list (self#loc self#mode)

    method include_kind : include_kind -> unit = fun _ -> ()

    method attribute : attribute -> unit =
      fun { attr_name; attr_payload; attr_loc; attr_tokens } ->
      self#loc (self#list self#string) attr_name;
      self#payload attr_payload;
      self#location attr_loc;
      self#token_seq attr_tokens

    method extension : extension -> unit =
      fun (a, b, c) ->
      self#loc (self#list self#string) a;
      self#payload b;
      self#token_seq c

    method toplevel_extension : toplevel_extension -> unit =
      fun { te_pre_doc; te_ext; te_attrs; te_post_doc } ->
      self#option self#string te_pre_doc;
      self#extension te_ext;
      self#attributes te_attrs;
      self#option self#string te_post_doc

    method attributes : attributes -> unit = self#list self#attribute

    method payload : payload -> unit =
      fun x ->
      match x with
      | PStr a -> self#structure a
      | PSig a -> self#signature a
      | PTyp a -> self#core_type a
      | PPat (a, b) ->
        self#pattern a;
        self#option self#expression b
      | PString (a, b) ->
        self#string a;
        self#string b

    method ext_attribute : ext_attribute -> unit =
      fun { pea_ext; pea_attrs } ->
      self#option (self#loc (self#list self#string)) pea_ext;
      self#attributes pea_attrs

    method core_type : core_type -> unit =
      fun { ptyp_desc; ptyp_loc; ptyp_attributes; ptyp_tokens } ->
      self#core_type_desc ptyp_desc;
      self#location ptyp_loc;
      self#attributes ptyp_attributes;
      self#token_seq ptyp_tokens

    method arrow_arg : arrow_arg -> unit =
      fun
        { aa_lbl
        ; aa_legacy_modes
        ; aa_type
        ; aa_modes
        ; aa_doc
        ; aa_loc
        ; aa_tokens
        }
        ->
      self#arg_label aa_lbl;
      self#modes aa_legacy_modes;
      self#core_type aa_type;
      self#modes aa_modes;
      self#option self#string aa_doc;
      self#location aa_loc;
      self#token_seq aa_tokens

    method core_type_desc : core_type_desc -> unit =
      fun x ->
      match x with
      | Ptyp_any a -> self#option self#jkind_annotation a
      | Ptyp_var (a, b) ->
        self#string a;
        self#option self#jkind_annotation b
      | Ptyp_arrow { domain; codom_legacy_modes; codom_type; codom_modes } ->
        self#arrow_arg domain;
        self#modes codom_legacy_modes;
        self#core_type codom_type;
        self#modes codom_modes
      | Ptyp_tuple a ->
        self
        #
        list
          (fun (a, b) ->
            self#option self#string a;
            self#core_type b)
          a
      | Ptyp_unboxed_tuple a ->
        self
        #
        list
          (fun (a, b) ->
            self#option self#string a;
            self#core_type b)
          a
      | Ptyp_constr (a, b) ->
        self#list self#core_type a;
        self#loc self#longident b
      | Ptyp_object (a, b) ->
        self#list self#object_field a;
        self#closed_flag b
      | Ptyp_class (a, b) ->
        self#loc self#longident a;
        self#list self#core_type b
      | Ptyp_alias (a, b, c) ->
        self#core_type a;
        self#option (self#loc self#string) b;
        self#option self#jkind_annotation c
      | Ptyp_variant (a, b, c) ->
        self#list self#row_field a;
        self#closed_flag b;
        self#option (self#list self#label) c
      | Ptyp_poly (a, b) ->
        self
        #
        list
          (fun (a, b) ->
            self#loc self#string a;
            self#option self#jkind_annotation b)
          a;
        self#core_type b
      | Ptyp_package (a, b) ->
        self#ext_attribute a;
        self#package_type b
      | Ptyp_open (a, b) ->
        self#loc self#longident a;
        self#core_type b
      | Ptyp_quote a -> self#core_type a
      | Ptyp_splice a -> self#core_type a
      | Ptyp_of_kind a -> self#jkind_annotation a
      | Ptyp_extension a -> self#extension a
      | Ptyp_parens a -> self#core_type a

    method package_type : package_type -> unit = self#module_type

    method row_field : row_field -> unit =
      fun { prf_desc; prf_loc; prf_attributes; prf_doc; prf_tokens } ->
      self#row_field_desc prf_desc;
      self#location prf_loc;
      self#attributes prf_attributes;
      self#option self#string prf_doc;
      self#token_seq prf_tokens

    method row_field_desc : row_field_desc -> unit =
      fun x ->
      match x with
      | Rtag (a, b, c) ->
        self#loc self#string a;
        self#bool b;
        self#list self#core_type c
      | Rinherit a -> self#core_type a

    method object_field : object_field -> unit =
      fun { pof_desc; pof_loc; pof_attributes; pof_doc; pof_tokens } ->
      self#object_field_desc pof_desc;
      self#location pof_loc;
      self#attributes pof_attributes;
      self#option self#string pof_doc;
      self#token_seq pof_tokens

    method object_field_desc : object_field_desc -> unit =
      fun x ->
      match x with
      | Otag (a, b) ->
        self#loc self#string a;
        self#core_type b
      | Oinherit a -> self#core_type a

    method pattern : pattern -> unit =
      fun
        { ppat_ext_attr; ppat_desc; ppat_loc; ppat_attributes; ppat_tokens } ->
      self#ext_attribute ppat_ext_attr;
      self#pattern_desc ppat_desc;
      self#location ppat_loc;
      self#attributes ppat_attributes;
      self#token_seq ppat_tokens

    method pattern_desc : pattern_desc -> unit =
      fun x ->
      match x with
      | Ppat_any -> ()
      | Ppat_var a -> self#loc self#longident_str_or_op a
      | Ppat_alias (a, b) ->
        self#pattern a;
        self#loc self#longident_str_or_op b
      | Ppat_constant a -> self#constant a
      | Ppat_interval (a, b) ->
        self#constant a;
        self#constant b
      | Ppat_tuple (a, b) ->
        self#list (self#argument self#pattern) a;
        self#closed_flag b
      | Ppat_unboxed_tuple (a, b) ->
        self#list (self#argument self#pattern) a;
        self#closed_flag b
      | Ppat_construct (a, b) ->
        self#loc self#longident a;
        self
        #
        option
          (fun (a, b) ->
            self
            #
            list
              (fun (a, b) ->
                self#loc self#string a;
                self#option self#jkind_annotation b)
              a;
            self#pattern b)
          b
      | Ppat_variant (a, b) ->
        self#label a;
        self#option self#pattern b
      | Ppat_record (a, b) ->
        self#list (self#record_field self#pattern) a;
        self#closed_flag b
      | Ppat_record_unboxed_product (a, b) ->
        self#list (self#record_field self#pattern) a;
        self#closed_flag b
      | Ppat_array (a, b) ->
        self#mutable_flag a;
        self#list self#pattern b
      | Ppat_or (a, b) ->
        self#pattern a;
        self#pattern b
      | Ppat_constraint (a, b, c) ->
        self#pattern a;
        self#option self#core_type b;
        self#modes c
      | Ppat_type a -> self#loc self#longident a
      | Ppat_lazy a -> self#pattern a
      | Ppat_unpack (a, b) ->
        self#loc (self#option self#string) a;
        self#option self#package_type b
      | Ppat_exception a -> self#pattern a
      | Ppat_extension a -> self#extension a
      | Ppat_open (a, b) ->
        self#loc self#longident a;
        self#pattern b
      | Ppat_parens { pat; optional } ->
        self#pattern pat;
        self#bool optional
      | Ppat_list a -> self#list self#pattern a
      | Ppat_cons (a, b) ->
        self#pattern a;
        self#pattern b

    method expression : expression -> unit =
      fun
        { pexp_ext_attr; pexp_desc; pexp_loc; pexp_attributes; pexp_tokens } ->
      self#ext_attribute pexp_ext_attr;
      self#expression_desc pexp_desc;
      self#location pexp_loc;
      self#attributes pexp_attributes;
      self#token_seq pexp_tokens

    method expression_desc : expression_desc -> unit =
      fun x ->
      match x with
      | Pexp_ident a -> self#loc self#longident a
      | Pexp_constant a -> self#constant a
      | Pexp_let (a, b, c, d) ->
        self#mutable_flag a;
        self#rec_flag b;
        self#list self#value_binding c;
        self#expression d
      | Pexp_function (a, b, c) ->
        self#list self#function_param a;
        self#function_constraint b;
        self#function_body c
      | Pexp_prefix_apply (a, b) ->
        self#expression a;
        self#expression b
      | Pexp_add_or_sub (a, b) ->
        self#string a;
        self#expression b
      | Pexp_infix_apply { arg1; op; arg2 } ->
        self#expression arg1;
        self#expression op;
        self#expression arg2
      | Pexp_apply (a, b) ->
        self#expression a;
        self#list (self#argument self#expression) b
      | Pexp_match (a, b) ->
        self#expression a;
        self#list self#case b
      | Pexp_try (a, b) ->
        self#expression a;
        self#list self#case b
      | Pexp_tuple a -> self#list (self#argument self#expression) a
      | Pexp_unboxed_tuple a -> self#list (self#argument self#expression) a
      | Pexp_construct (a, b) ->
        self#loc self#longident a;
        self#option self#expression b
      | Pexp_variant (a, b) ->
        self#label a;
        self#option self#expression b
      | Pexp_record (a, b) ->
        self#option self#expression a;
        self#list (self#record_field self#expression) b
      | Pexp_record_unboxed_product (a, b) ->
        self#option self#expression a;
        self#list (self#record_field self#expression) b
      | Pexp_field (a, b) ->
        self#expression a;
        self#loc self#longident b
      | Pexp_unboxed_field (a, b) ->
        self#expression a;
        self#loc self#longident b
      | Pexp_setfield (a, b, c) ->
        self#expression a;
        self#loc self#longident b;
        self#expression c
      | Pexp_array (a, b) ->
        self#mutable_flag a;
        self#list self#expression b
      | Pexp_idx (a, b) ->
        self#block_access a;
        self#list self#unboxed_access b
      | Pexp_ifthenelse (a, b, c) ->
        self#expression a;
        self#expression b;
        self#option self#expression c
      | Pexp_sequence (a, b) ->
        self#expression a;
        self#expression b
      | Pexp_seq_empty a -> self#expression a
      | Pexp_while (a, b) ->
        self#expression a;
        self#expression b
      | Pexp_for (a, b, c, d, e) ->
        self#pattern a;
        self#expression b;
        self#expression c;
        self#direction_flag d;
        self#expression e
      | Pexp_constraint (a, b, c) ->
        self#expression a;
        self#option self#core_type b;
        self#modes c
      | Pexp_coerce (a, b, c) ->
        self#expression a;
        self#option self#core_type b;
        self#core_type c
      | Pexp_send (a, b) ->
        self#expression a;
        self#loc self#string b
      | Pexp_new a -> self#loc self#longident a
      | Pexp_setvar (a, b) ->
        self#loc self#string a;
        self#expression b
      | Pexp_override a ->
        self
        #
        list
          (fun (a, b) ->
            self#loc self#string a;
            self#option self#expression b)
          a
      | Pexp_letmodule (a, b) ->
        self#module_binding a;
        self#expression b
      | Pexp_letexception (a, b) ->
        self#extension_constructor a;
        self#expression b
      | Pexp_assert a -> self#expression a
      | Pexp_lazy a -> self#expression a
      | Pexp_object a -> self#class_structure a
      | Pexp_pack (a, b) ->
        self#module_expr a;
        self#option self#package_type b
      | Pexp_dot_open (a, b) ->
        self#loc self#longident a;
        self#expression b
      | Pexp_let_open (a, b) ->
        self#open_declaration a;
        self#expression b
      | Pexp_letop a -> self#letop a
      | Pexp_extension a -> self#extension a
      | Pexp_unreachable -> ()
      | Pexp_stack a -> self#expression a
      | Pexp_comprehension a -> self#comprehension_expression a
      | Pexp_overwrite (a, b) ->
        self#expression a;
        self#expression b
      | Pexp_quote a -> self#expression a
      | Pexp_splice a -> self#expression a
      | Pexp_hole -> ()
      | Pexp_index_op { kind; op; seq; indices; assign } ->
        self#paren_kind kind;
        self
        #
        option
          (fun (a, b) ->
            self#option self#longident a;
            self#string b)
          op;
        self#expression seq;
        self#list self#expression indices;
        self#option self#expression assign
      | Pexp_parens { exp; optional } ->
        self#expression exp;
        self#bool optional
      | Pexp_begin_end a -> self#option self#expression a
      | Pexp_list a -> self#list self#expression a
      | Pexp_cons (a, b) ->
        self#expression a;
        self#expression b
      | Pexp_exclave a -> self#expression a
      | Pexp_mode_legacy (a, b) ->
        self#loc self#mode a;
        self#expression b

    method record_field : 'a. ('a -> unit) -> 'a record_field -> unit =
      fun _a { field_name; typ; value } ->
      self#loc self#longident field_name;
      self#option self#type_constraint typ;
      self#option _a value

    method case : case -> unit =
      fun { pc_lhs; pc_guard; pc_rhs; pc_tokens } ->
      self#pattern pc_lhs;
      self#option self#expression pc_guard;
      self#expression pc_rhs;
      self#token_seq pc_tokens

    method letop : letop -> unit =
      fun { let_; ands; body } ->
      self#binding_op let_;
      self#list self#binding_op ands;
      self#expression body

    method binding_op : binding_op -> unit =
      fun { pbop_op; pbop_binding; pbop_loc } ->
      self#loc self#string pbop_op;
      self#value_binding pbop_binding;
      self#location pbop_loc

    method argument_desc : 'a. ('a -> unit) -> 'a argument_desc -> unit =
      fun _a x ->
      match x with
      | Parg_unlabelled { legacy_modes; arg; typ_constraint; modes } ->
        self#modes legacy_modes;
        _a arg;
        self#option self#type_constraint typ_constraint;
        self#modes modes
      | Parg_labelled
          { optional
          ; legacy_modes
          ; name
          ; maybe_punned
          ; typ_constraint
          ; modes
          ; default
          } ->
        self#bool optional;
        self#modes legacy_modes;
        self#string name;
        self#option _a maybe_punned;
        self#option self#type_constraint typ_constraint;
        self#modes modes;
        self#option self#expression default

    method argument : 'a. ('a -> unit) -> 'a argument -> unit =
      fun _a { parg_desc; parg_tokens } ->
      self#argument_desc _a parg_desc;
      self#token_seq parg_tokens

    method function_param_desc : function_param_desc -> unit =
      fun x ->
      match x with
      | Pparam_val a -> self#argument self#pattern a
      | Pparam_newtype (a, b) ->
        self#loc self#string a;
        self#option self#jkind_annotation b
      | Pparam_newtypes a ->
        self
        #
        list
          (fun (a, b) ->
            self#loc self#string a;
            self#option self#jkind_annotation b)
          a

    method function_param : function_param -> unit =
      fun { pparam_loc; pparam_desc } ->
      self#location pparam_loc;
      self#function_param_desc pparam_desc

    method function_body : function_body -> unit =
      fun { pfb_desc; pfb_loc; pfb_tokens } ->
      self#function_body_desc pfb_desc;
      self#location pfb_loc;
      self#token_seq pfb_tokens

    method function_body_desc : function_body_desc -> unit =
      fun x ->
      match x with
      | Pfunction_body a -> self#expression a
      | Pfunction_cases (a, b) ->
        self#list self#case a;
        self#ext_attribute b

    method type_constraint : type_constraint -> unit =
      fun x ->
      match x with
      | Pconstraint a -> self#core_type a
      | Pcoerce (a, b) ->
        self#option self#core_type a;
        self#core_type b

    method function_constraint : function_constraint -> unit =
      fun { ret_mode_annotations; ret_type_constraint } ->
      self#modes ret_mode_annotations;
      self#option self#type_constraint ret_type_constraint

    method block_access : block_access -> unit =
      fun x ->
      match x with
      | Baccess_field a -> self#loc self#longident a
      | Baccess_array (a, b, c) ->
        self#mutable_flag a;
        self#index_kind b;
        self#expression c
      | Baccess_block (a, b) ->
        self#mutable_flag a;
        self#expression b

    method unboxed_access : unboxed_access -> unit =
      fun x ->
      match x with
      | Uaccess_unboxed_field a -> self#loc self#longident a

    method comprehension_iterator : comprehension_iterator -> unit =
      fun x ->
      match x with
      | Pcomp_range { start; stop; direction } ->
        self#expression start;
        self#expression stop;
        self#direction_flag direction
      | Pcomp_in a -> self#expression a

    method comprehension_clause_binding : comprehension_clause_binding -> unit =
      fun
        { pcomp_cb_mode
        ; pcomp_cb_pattern
        ; pcomp_cb_iterator
        ; pcomp_cb_attributes
        ; pcomp_cb_tokens
        }
        ->
      self#option (self#loc self#mode) pcomp_cb_mode;
      self#pattern pcomp_cb_pattern;
      self#comprehension_iterator pcomp_cb_iterator;
      self#attributes pcomp_cb_attributes;
      self#token_seq pcomp_cb_tokens

    method comprehension_clause : comprehension_clause -> unit =
      fun x ->
      match x with
      | Pcomp_for a -> self#list self#comprehension_clause_binding a
      | Pcomp_when a -> self#expression a

    method comprehension : comprehension -> unit =
      fun { pcomp_body; pcomp_clauses; pcomp_tokens } ->
      self#expression pcomp_body;
      self#list self#comprehension_clause pcomp_clauses;
      self#token_seq pcomp_tokens

    method comprehension_expression : comprehension_expression -> unit =
      fun x ->
      match x with
      | Pcomp_list_comprehension a -> self#comprehension a
      | Pcomp_array_comprehension (a, b) ->
        self#mutable_flag a;
        self#comprehension b

    method value_description : value_description -> unit =
      fun
        { pval_pre_doc
        ; pval_ext_attrs
        ; pval_name
        ; pval_type
        ; pval_modalities
        ; pval_prim
        ; pval_attributes
        ; pval_post_doc
        ; pval_loc
        ; pval_tokens
        }
        ->
      self#option self#string pval_pre_doc;
      self#ext_attribute pval_ext_attrs;
      self#loc self#longident_str_or_op pval_name;
      self#core_type pval_type;
      self#modalities pval_modalities;
      self#list self#string pval_prim;
      self#attributes pval_attributes;
      self#option self#string pval_post_doc;
      self#location pval_loc;
      self#token_seq pval_tokens

    method ptype_param : ptype_param -> unit =
      fun { ptp_typ; ptp_infos; ptp_tokens } ->
      self#core_type ptp_typ;
      (fun (a, b) ->
        self#variance a;
        self#injectivity b)
        ptp_infos;
      self#token_seq ptp_tokens

    method ptype_params : ptype_params -> unit = self#list self#ptype_param

    method ptype_constraint : ptype_constraint -> unit =
      fun (a, b, c) ->
      self#core_type a;
      self#core_type b;
      self#location c

    method type_declaration : type_declaration -> unit =
      fun
        { ptype_pre_text
        ; ptype_pre_doc
        ; ptype_ext_attrs
        ; ptype_name
        ; ptype_params
        ; ptype_jkind_annotation
        ; ptype_private
        ; ptype_manifest
        ; ptype_kind
        ; ptype_cstrs
        ; ptype_attributes
        ; ptype_post_doc
        ; ptype_loc
        ; ptype_tokens
        }
        ->
      self#list self#string ptype_pre_text;
      self#option self#string ptype_pre_doc;
      self#ext_attribute ptype_ext_attrs;
      self#loc self#string ptype_name;
      self#ptype_params ptype_params;
      self#option self#jkind_annotation ptype_jkind_annotation;
      self#private_flag ptype_private;
      self#option self#core_type ptype_manifest;
      self#type_kind ptype_kind;
      self#list self#ptype_constraint ptype_cstrs;
      self#attributes ptype_attributes;
      self#option self#string ptype_post_doc;
      self#location ptype_loc;
      self#token_seq ptype_tokens

    method type_kind : type_kind -> unit =
      fun x ->
      match x with
      | Ptype_abstract -> ()
      | Ptype_variant a -> self#list self#constructor_declaration a
      | Ptype_record a -> self#list self#label_declaration a
      | Ptype_record_unboxed_product a -> self#list self#label_declaration a
      | Ptype_open -> ()

    method label_declaration : label_declaration -> unit =
      fun
        { pld_name
        ; pld_mutable
        ; pld_global
        ; pld_modalities
        ; pld_type
        ; pld_loc
        ; pld_attributes
        ; pld_doc
        ; pld_tokens
        }
        ->
      self#loc self#string pld_name;
      self#mutable_flag pld_mutable;
      self#bool pld_global;
      self#modalities pld_modalities;
      self#core_type pld_type;
      self#location pld_loc;
      self#attributes pld_attributes;
      self#option self#string pld_doc;
      self#token_seq pld_tokens

    method constructor_declaration : constructor_declaration -> unit =
      fun
        { pcd_name
        ; pcd_vars
        ; pcd_args
        ; pcd_res
        ; pcd_loc
        ; pcd_attributes
        ; pcd_doc
        ; pcd_tokens
        }
        ->
      self#loc self#longident_str_or_op pcd_name;
      self
      #
      list
        (fun (a, b) ->
          self#loc self#string a;
          self#option self#jkind_annotation b)
        pcd_vars;
      self#constructor_arguments pcd_args;
      self#option self#core_type pcd_res;
      self#location pcd_loc;
      self#attributes pcd_attributes;
      self#option self#string pcd_doc;
      self#token_seq pcd_tokens

    method constructor_argument : constructor_argument -> unit =
      fun { pca_global; pca_type; pca_modalities; pca_loc } ->
      self#bool pca_global;
      self#core_type pca_type;
      self#modalities pca_modalities;
      self#location pca_loc

    method constructor_arguments : constructor_arguments -> unit =
      fun x ->
      match x with
      | Pcstr_tuple a -> self#list self#constructor_argument a
      | Pcstr_record a -> self#list self#label_declaration a

    method type_extension : type_extension -> unit =
      fun
        { ptyext_pre_doc
        ; ptyext_ext_attrs
        ; ptyext_path
        ; ptyext_params
        ; ptyext_constructors
        ; ptyext_private
        ; ptyext_loc
        ; ptyext_attributes
        ; ptyext_post_doc
        ; ptyext_tokens
        }
        ->
      self#option self#string ptyext_pre_doc;
      self#ext_attribute ptyext_ext_attrs;
      self#loc self#longident ptyext_path;
      self#list self#ptype_param ptyext_params;
      self#list self#extension_constructor ptyext_constructors;
      self#private_flag ptyext_private;
      self#location ptyext_loc;
      self#attributes ptyext_attributes;
      self#option self#string ptyext_post_doc;
      self#token_seq ptyext_tokens

    method extension_constructor : extension_constructor -> unit =
      fun
        { pext_name
        ; pext_kind
        ; pext_loc
        ; pext_attributes
        ; pext_doc
        ; pext_tokens
        }
        ->
      self#loc self#longident_str_or_op pext_name;
      self#extension_constructor_kind pext_kind;
      self#location pext_loc;
      self#attributes pext_attributes;
      self#option self#string pext_doc;
      self#token_seq pext_tokens

    method type_exception : type_exception -> unit =
      fun
        { ptyexn_pre_doc
        ; ptyexn_ext_attrs
        ; ptyexn_constructor
        ; ptyexn_loc
        ; ptyexn_attributes
        ; ptyexn_post_doc
        ; ptyexn_tokens
        }
        ->
      self#option self#string ptyexn_pre_doc;
      self#ext_attribute ptyexn_ext_attrs;
      self#extension_constructor ptyexn_constructor;
      self#location ptyexn_loc;
      self#attributes ptyexn_attributes;
      self#option self#string ptyexn_post_doc;
      self#token_seq ptyexn_tokens

    method extension_constructor_kind : extension_constructor_kind -> unit =
      fun x ->
      match x with
      | Pext_decl (a, b, c) ->
        self
        #
        list
          (fun (a, b) ->
            self#loc self#string a;
            self#option self#jkind_annotation b)
          a;
        self#constructor_arguments b;
        self#option self#core_type c
      | Pext_rebind a -> self#loc self#longident a

    method class_type : class_type -> unit =
      fun { pcty_desc; pcty_loc; pcty_attributes; pcty_tokens } ->
      self#class_type_desc pcty_desc;
      self#location pcty_loc;
      self#attributes pcty_attributes;
      self#token_seq pcty_tokens

    method class_type_desc : class_type_desc -> unit =
      fun x ->
      match x with
      | Pcty_constr (a, b) ->
        self#loc self#longident a;
        self#list self#core_type b
      | Pcty_signature a -> self#class_signature a
      | Pcty_arrow (a, b) ->
        self#arrow_arg a;
        self#class_type b
      | Pcty_extension a -> self#extension a
      | Pcty_open (a, b) ->
        self#open_description a;
        self#class_type b

    method class_signature : class_signature -> unit =
      fun { pcsig_self; pcsig_fields } ->
      self#option self#core_type pcsig_self;
      self#list self#class_type_field pcsig_fields

    method class_type_field : class_type_field -> unit =
      fun
        { pctf_pre_doc
        ; pctf_desc
        ; pctf_loc
        ; pctf_attributes
        ; pctf_post_doc
        ; pctf_tokens
        }
        ->
      self#option self#string pctf_pre_doc;
      self#class_type_field_desc pctf_desc;
      self#location pctf_loc;
      self#attributes pctf_attributes;
      self#option self#string pctf_post_doc;
      self#token_seq pctf_tokens

    method class_type_field_desc : class_type_field_desc -> unit =
      fun x ->
      match x with
      | Pctf_inherit a -> self#class_type a
      | Pctf_val a ->
        (fun (a, b, c, d) ->
          self#loc self#string a;
          self#mutable_flag b;
          self#virtual_flag c;
          self#core_type d)
          a
      | Pctf_method a ->
        (fun (a, b, c, d) ->
          self#loc self#string a;
          self#private_flag b;
          self#virtual_flag c;
          self#core_type d)
          a
      | Pctf_constraint a ->
        (fun (a, b) ->
          self#core_type a;
          self#core_type b)
          a
      | Pctf_attribute a -> self#attribute a
      | Pctf_extension a -> self#extension a
      | Pctf_docstring a -> self#string a

    method class_infos : 'a. ('a -> unit) -> 'a class_infos -> unit =
      fun
        _a
        { pci_pre_text
        ; pci_pre_doc
        ; pci_virt
        ; pci_ext_attrs
        ; pci_params
        ; pci_name
        ; pci_value_params
        ; pci_constraint
        ; pci_expr
        ; pci_loc
        ; pci_attributes
        ; pci_post_doc
        ; pci_tokens
        }
        ->
      self#list self#string pci_pre_text;
      self#option self#string pci_pre_doc;
      self#virtual_flag pci_virt;
      self#ext_attribute pci_ext_attrs;
      self#list self#ptype_param pci_params;
      self#loc self#string pci_name;
      self#list (self#argument self#pattern) pci_value_params;
      self#option self#class_type pci_constraint;
      _a pci_expr;
      self#location pci_loc;
      self#attributes pci_attributes;
      self#option self#string pci_post_doc;
      self#token_seq pci_tokens

    method class_description : class_description -> unit =
      self#class_infos self#class_type

    method class_type_declaration : class_type_declaration -> unit =
      self#class_infos self#class_type

    method class_expr : class_expr -> unit =
      fun { pcl_ext_attrs; pcl_desc; pcl_loc; pcl_attributes } ->
      self#ext_attribute pcl_ext_attrs;
      self#class_expr_desc pcl_desc;
      self#location pcl_loc;
      self#attributes pcl_attributes

    method class_expr_desc : class_expr_desc -> unit =
      fun x ->
      match x with
      | Pcl_constr (a, b) ->
        self#loc self#longident a;
        self#list self#core_type b
      | Pcl_structure a -> self#class_structure a
      | Pcl_fun (a, b) ->
        self#list (self#argument self#pattern) a;
        self#class_expr b
      | Pcl_apply (a, b) ->
        self#class_expr a;
        self#list (self#argument self#expression) b
      | Pcl_let (a, b, c) ->
        self#rec_flag a;
        self#list self#value_binding b;
        self#class_expr c
      | Pcl_constraint (a, b) ->
        self#class_expr a;
        self#class_type b
      | Pcl_extension a -> self#extension a
      | Pcl_open (a, b) ->
        self#open_description a;
        self#class_expr b
      | Pcl_parens a -> self#class_expr a

    method class_structure : class_structure -> unit =
      fun { pcstr_self; pcstr_fields } ->
      self#pattern pcstr_self;
      self#list self#class_field pcstr_fields

    method class_field : class_field -> unit =
      fun
        { pcf_pre_doc
        ; pcf_desc
        ; pcf_loc
        ; pcf_attributes
        ; pcf_post_doc
        ; pcf_tokens
        }
        ->
      self#option self#string pcf_pre_doc;
      self#class_field_desc pcf_desc;
      self#location pcf_loc;
      self#attributes pcf_attributes;
      self#option self#string pcf_post_doc;
      self#token_seq pcf_tokens

    method class_field_desc : class_field_desc -> unit =
      fun x ->
      match x with
      | Pcf_inherit (a, b, c) ->
        self#override_flag a;
        self#class_expr b;
        self#option (self#loc self#string) c
      | Pcf_val a ->
        (fun (a, b, c) ->
          self#loc self#string a;
          self#mutable_flag b;
          self#class_field_kind c)
          a
      | Pcf_method a ->
        (fun (a, b, c) ->
          self#loc self#string a;
          self#private_flag b;
          self#class_field_kind c)
          a
      | Pcf_constraint a ->
        (fun (a, b) ->
          self#core_type a;
          self#core_type b)
          a
      | Pcf_initializer a -> self#expression a
      | Pcf_attribute a -> self#attribute a
      | Pcf_extension a -> self#extension a
      | Pcf_docstring a -> self#string a

    method class_field_kind : class_field_kind -> unit =
      fun x ->
      match x with
      | Cfk_virtual a -> self#core_type a
      | Cfk_concrete (a, b) ->
        self#override_flag a;
        self#value_binding b

    method class_declaration : class_declaration -> unit =
      self#class_infos self#class_expr

    method module_type : module_type -> unit =
      fun { pmty_desc; pmty_loc; pmty_attributes; pmty_tokens } ->
      self#module_type_desc pmty_desc;
      self#location pmty_loc;
      self#attributes pmty_attributes;
      self#token_seq pmty_tokens

    method module_type_desc : module_type_desc -> unit =
      fun x ->
      match x with
      | Pmty_ident a -> self#loc self#longident a
      | Pmty_signature a -> self#signature a
      | Pmty_functor (a, b, c, d) ->
        self#attributes a;
        self#list self#functor_parameter b;
        self#module_type c;
        self#modes d
      | Pmty_functor_type (a, b, c) ->
        self#list self#functor_parameter a;
        self#module_type b;
        self#modes c
      | Pmty_with (a, b) ->
        self#module_type a;
        self#list self#with_constraint b
      | Pmty_typeof (a, b) ->
        self#attributes a;
        self#module_expr b
      | Pmty_extension a -> self#extension a
      | Pmty_alias a -> self#loc self#longident a
      | Pmty_strengthen (a, b) ->
        self#module_type a;
        self#loc self#longident b
      | Pmty_parens a -> self#module_type a

    method functor_parameter : functor_parameter -> unit =
      fun x ->
      match x with
      | Unit -> ()
      | Named (a, b, c) ->
        self#loc (self#option self#string) a;
        self#module_type b;
        self#modes c
      | Unnamed (a, b) ->
        self#module_type a;
        self#modes b

    method signature : signature -> unit =
      fun { psg_modalities; psg_items; psg_loc; psg_tokens } ->
      self#modalities psg_modalities;
      self#list self#signature_item psg_items;
      self#location psg_loc;
      self#token_seq psg_tokens

    method signature_item : signature_item -> unit =
      fun { psig_desc; psig_loc; psig_tokens } ->
      self#signature_item_desc psig_desc;
      self#location psig_loc;
      self#token_seq psig_tokens

    method signature_item_desc : signature_item_desc -> unit =
      fun x ->
      match x with
      | Psig_value a -> self#value_description a
      | Psig_type (a, b) ->
        self#rec_flag a;
        self#list self#type_declaration b
      | Psig_typesubst a -> self#list self#type_declaration a
      | Psig_typext a -> self#type_extension a
      | Psig_exception a -> self#type_exception a
      | Psig_module a -> self#module_declaration a
      | Psig_modsubst a -> self#module_substitution a
      | Psig_recmodule a -> self#list self#module_declaration a
      | Psig_modtype a -> self#module_type_declaration a
      | Psig_modtypesubst a -> self#module_type_declaration a
      | Psig_open a -> self#open_description a
      | Psig_include (a, b) ->
        self#include_description a;
        self#modalities b
      | Psig_class a -> self#list self#class_description a
      | Psig_class_type a -> self#list self#class_type_declaration a
      | Psig_attribute a -> self#attribute a
      | Psig_extension a -> self#toplevel_extension a
      | Psig_kind_abbrev (a, b) ->
        self#loc self#string a;
        self#jkind_annotation b
      | Psig_docstring a -> self#string a

    method module_declaration_body : module_declaration_body -> unit =
      fun x ->
      match x with
      | With_params (a, b, c) ->
        self#list self#functor_parameter a;
        self#module_type b;
        self#modes c
      | Without_params (a, b) ->
        self#module_type a;
        self#modalities b

    method module_declaration : module_declaration -> unit =
      fun
        { pmd_pre_text
        ; pmd_pre_doc
        ; pmd_ext_attrs
        ; pmd_name
        ; pmd_body
        ; pmd_attributes
        ; pmd_post_doc
        ; pmd_loc
        ; pmd_tokens
        }
        ->
      self#list self#string pmd_pre_text;
      self#option self#string pmd_pre_doc;
      self#ext_attribute pmd_ext_attrs;
      (fun (a, b) ->
        self#loc (self#option self#string) a;
        self#modalities b)
        pmd_name;
      self#module_declaration_body pmd_body;
      self#attributes pmd_attributes;
      self#option self#string pmd_post_doc;
      self#location pmd_loc;
      self#token_seq pmd_tokens

    method module_substitution : module_substitution -> unit =
      fun
        { pms_pre_doc
        ; pms_ext_attrs
        ; pms_name
        ; pms_manifest
        ; pms_attributes
        ; pms_post_doc
        ; pms_loc
        ; pms_tokens
        }
        ->
      self#option self#string pms_pre_doc;
      self#ext_attribute pms_ext_attrs;
      self#loc self#string pms_name;
      self#loc self#longident pms_manifest;
      self#attributes pms_attributes;
      self#option self#string pms_post_doc;
      self#location pms_loc;
      self#token_seq pms_tokens

    method module_type_declaration : module_type_declaration -> unit =
      fun
        { pmtd_pre_doc
        ; pmtd_ext_attrs
        ; pmtd_name
        ; pmtd_type
        ; pmtd_attributes
        ; pmtd_post_doc
        ; pmtd_loc
        ; pmtd_tokens
        }
        ->
      self#option self#string pmtd_pre_doc;
      self#ext_attribute pmtd_ext_attrs;
      self#loc self#string pmtd_name;
      self#option self#module_type pmtd_type;
      self#attributes pmtd_attributes;
      self#option self#string pmtd_post_doc;
      self#location pmtd_loc;
      self#token_seq pmtd_tokens

    method open_infos : 'a. ('a -> unit) -> 'a open_infos -> unit =
      fun
        _a
        { popen_pre_doc
        ; popen_ext_attrs
        ; popen_expr
        ; popen_override
        ; popen_loc
        ; popen_attributes
        ; popen_post_doc
        ; popen_tokens
        }
        ->
      self#option self#string popen_pre_doc;
      self#ext_attribute popen_ext_attrs;
      _a popen_expr;
      self#override_flag popen_override;
      self#location popen_loc;
      self#attributes popen_attributes;
      self#option self#string popen_post_doc;
      self#token_seq popen_tokens

    method open_description : open_description -> unit =
      self#open_infos (self#loc self#longident)

    method open_declaration : open_declaration -> unit =
      self#open_infos self#module_expr

    method include_infos : 'a. ('a -> unit) -> 'a include_infos -> unit =
      fun
        _a
        { pincl_pre_doc
        ; pincl_kind
        ; pincl_ext_attrs
        ; pincl_mod
        ; pincl_loc
        ; pincl_attributes
        ; pincl_post_doc
        ; pincl_tokens
        }
        ->
      self#option self#string pincl_pre_doc;
      self#include_kind pincl_kind;
      self#ext_attribute pincl_ext_attrs;
      _a pincl_mod;
      self#location pincl_loc;
      self#attributes pincl_attributes;
      self#option self#string pincl_post_doc;
      self#token_seq pincl_tokens

    method include_description : include_description -> unit =
      self#include_infos self#module_type

    method include_declaration : include_declaration -> unit =
      self#include_infos self#module_expr

    method with_constraint : with_constraint -> unit =
      fun { wc_desc; wc_loc; wc_tokens } ->
      self#with_constraint_desc wc_desc;
      self#location wc_loc;
      self#token_seq wc_tokens

    method with_constraint_desc : with_constraint_desc -> unit =
      fun x ->
      match x with
      | Pwith_type (a, b, c, d, e) ->
        self#ptype_params a;
        self#loc self#longident b;
        self#private_flag c;
        self#core_type d;
        self#list self#ptype_constraint e
      | Pwith_module (a, b) ->
        self#loc self#longident a;
        self#loc self#longident b
      | Pwith_modtype (a, b) ->
        self#loc self#longident a;
        self#module_type b
      | Pwith_modtypesubst (a, b) ->
        self#loc self#longident a;
        self#module_type b
      | Pwith_typesubst (a, b, c) ->
        self#ptype_params a;
        self#loc self#longident b;
        self#core_type c
      | Pwith_modsubst (a, b) ->
        self#loc self#longident a;
        self#loc self#longident b

    method module_expr : module_expr -> unit =
      fun { pmod_desc; pmod_loc; pmod_attributes; pmod_tokens } ->
      self#module_expr_desc pmod_desc;
      self#location pmod_loc;
      self#attributes pmod_attributes;
      self#token_seq pmod_tokens

    method module_expr_desc : module_expr_desc -> unit =
      fun x ->
      match x with
      | Pmod_ident a -> self#loc self#longident a
      | Pmod_structure (a, b) ->
        self#attributes a;
        self#structure b
      | Pmod_functor (a, b, c) ->
        self#attributes a;
        self#list self#functor_parameter b;
        self#module_expr c
      | Pmod_apply (a, b) ->
        self#module_expr a;
        self#module_expr b
      | Pmod_apply_unit a -> self#module_expr a
      | Pmod_constraint (a, b, c) ->
        self#module_expr a;
        self#option self#module_type b;
        self#modes c
      | Pmod_unpack (a, b, c) ->
        self#expression a;
        self#option self#package_type b;
        self#option self#package_type c
      | Pmod_extension a -> self#extension a
      | Pmod_parens a -> self#module_expr a

    method structure : structure -> unit =
      fun (a, b) ->
      self#list self#structure_item a;
      self#token_seq b

    method structure_item : structure_item -> unit =
      fun { pstr_desc; pstr_loc; pstr_tokens } ->
      self#structure_item_desc pstr_desc;
      self#location pstr_loc;
      self#token_seq pstr_tokens

    method structure_item_desc : structure_item_desc -> unit =
      fun x ->
      match x with
      | Pstr_eval (a, b) ->
        self#expression a;
        self#attributes b
      | Pstr_value (a, b) ->
        self#rec_flag a;
        self#list self#value_binding b
      | Pstr_primitive a -> self#value_description a
      | Pstr_type (a, b) ->
        self#rec_flag a;
        self#list self#type_declaration b
      | Pstr_typext a -> self#type_extension a
      | Pstr_exception a -> self#type_exception a
      | Pstr_module a -> self#module_binding a
      | Pstr_recmodule a -> self#list self#module_binding a
      | Pstr_modtype a -> self#module_type_declaration a
      | Pstr_open a -> self#open_declaration a
      | Pstr_class a -> self#list self#class_declaration a
      | Pstr_class_type a -> self#list self#class_type_declaration a
      | Pstr_include a -> self#include_declaration a
      | Pstr_attribute a -> self#attribute a
      | Pstr_extension a -> self#toplevel_extension a
      | Pstr_kind_abbrev (a, b) ->
        self#loc self#string a;
        self#jkind_annotation b
      | Pstr_docstring a -> self#string a

    method value_constraint : value_constraint -> unit =
      fun x ->
      match x with
      | Pvc_constraint { locally_abstract_univars; typ } ->
        self
        #
        list
          (fun (a, b) ->
            self#loc self#string a;
            self#option self#jkind_annotation b)
          locally_abstract_univars;
        self#core_type typ
      | Pvc_coercion { ground; coercion } ->
        self#option self#core_type ground;
        self#core_type coercion

    method value_binding : value_binding -> unit =
      fun
        { pvb_pre_text
        ; pvb_pre_doc
        ; pvb_ext_attrs
        ; pvb_legacy_modes
        ; pvb_pat
        ; pvb_modes
        ; pvb_params
        ; pvb_constraint
        ; pvb_expr
        ; pvb_ret_modes
        ; pvb_attributes
        ; pvb_post_doc
        ; pvb_loc
        ; pvb_tokens
        }
        ->
      self#list self#string pvb_pre_text;
      self#option self#string pvb_pre_doc;
      self#ext_attribute pvb_ext_attrs;
      self#modes pvb_legacy_modes;
      self#pattern pvb_pat;
      self#modes pvb_modes;
      self#list self#function_param pvb_params;
      self#option self#value_constraint pvb_constraint;
      self#option self#expression pvb_expr;
      self#modes pvb_ret_modes;
      self#attributes pvb_attributes;
      self#option self#string pvb_post_doc;
      self#location pvb_loc;
      self#token_seq pvb_tokens

    method module_binding : module_binding -> unit =
      fun
        { pmb_pre_text
        ; pmb_pre_doc
        ; pmb_ext_attrs
        ; pmb_name
        ; pmb_params
        ; pmb_constraint
        ; pmb_modes
        ; pmb_expr
        ; pmb_attributes
        ; pmb_post_doc
        ; pmb_loc
        ; pmb_tokens
        }
        ->
      self#list self#string pmb_pre_text;
      self#option self#string pmb_pre_doc;
      self#ext_attribute pmb_ext_attrs;
      (fun (a, b) ->
        self#loc (self#option self#string) a;
        self#modes b)
        pmb_name;
      self#list self#functor_parameter pmb_params;
      self#option self#module_type pmb_constraint;
      self#modes pmb_modes;
      self#module_expr pmb_expr;
      self#attributes pmb_attributes;
      self#option self#string pmb_post_doc;
      self#location pmb_loc;
      self#token_seq pmb_tokens

    method jkind_annotation_desc : jkind_annotation_desc -> unit =
      fun x ->
      match x with
      | Pjk_default -> ()
      | Pjk_abbreviation a -> self#string a
      | Pjk_mod (a, b) ->
        self#jkind_annotation a;
        self#modes b
      | Pjk_with (a, b, c) ->
        self#jkind_annotation a;
        self#core_type b;
        self#modalities c
      | Pjk_kind_of a -> self#core_type a
      | Pjk_product a -> self#list self#jkind_annotation a
      | Pjk_parens a -> self#jkind_annotation_desc a

    method jkind_annotation : jkind_annotation -> unit =
      fun { pjkind_loc; pjkind_desc; pjkind_tokens } ->
      self#location pjkind_loc;
      self#jkind_annotation_desc pjkind_desc;
      self#token_seq pjkind_tokens

    method use_file : use_file -> unit =
      fun (a, b) ->
      self#list self#toplevel_phrase a;
      self#token_seq b

    method toplevel_phrase : toplevel_phrase -> unit =
      fun x ->
      match x with
      | Ptop_def a -> self#structure a
      | Ptop_dir a -> self#toplevel_directive a
      | Ptop_lex a -> self#lexer_directive a

    method toplevel_directive : toplevel_directive -> unit =
      fun { pdir_name; pdir_arg; pdir_loc; pdir_tokens } ->
      self#loc self#string pdir_name;
      self#option self#directive_argument pdir_arg;
      self#location pdir_loc;
      self#token_seq pdir_tokens

    method directive_argument : directive_argument -> unit =
      fun { pdira_desc; pdira_loc } ->
      self#directive_argument_desc pdira_desc;
      self#location pdira_loc

    method directive_argument_desc : directive_argument_desc -> unit =
      fun x ->
      match x with
      | Pdir_string a -> self#string a
      | Pdir_int (a, b) ->
        self#string a;
        self#option self#char b
      | Pdir_ident a -> self#longident a
      | Pdir_bool a -> self#bool a

    method syntax_directive : syntax_directive -> unit =
      fun { psyn_mode; psyn_toggle } ->
      self#loc self#string psyn_mode;
      self#bool psyn_toggle

    method lexer_directive_desc : lexer_directive_desc -> unit =
      fun x ->
      match x with
      | Plex_syntax a -> self#syntax_directive a

    method lexer_directive : lexer_directive -> unit =
      fun { plex_desc; plex_loc; plex_tokens } ->
      self#lexer_directive_desc plex_desc;
      self#location plex_loc;
      self#token_seq plex_tokens
  end

class virtual ['acc] fold =
  object(self)
    method virtual bool : bool -> 'acc -> 'acc

    method virtual char : char -> 'acc -> 'acc

    method virtual int : int -> 'acc -> 'acc

    method virtual list : 'a. ('a -> 'acc -> 'acc) -> 'a list -> 'acc -> 'acc

    method
    virtual
    option
    :
    'a. ('a -> 'acc -> 'acc) -> 'a option -> 'acc -> 'acc

    method virtual ref : 'a. ('a -> 'acc -> 'acc) -> 'a ref -> 'acc -> 'acc

    method virtual string : string -> 'acc -> 'acc

    method virtual token : token -> 'acc -> 'acc

    method position : position -> 'acc -> 'acc =
      fun { pos_fname; pos_lnum; pos_bol; pos_cnum } acc ->
      let acc = self#string pos_fname acc in
      let acc = self#int pos_lnum acc in
      let acc = self#int pos_bol acc in
      let acc = self#int pos_cnum acc in
      acc

    method location : location -> 'acc -> 'acc =
      fun { loc_start; loc_end; loc_ghost } acc ->
      let acc = self#position loc_start acc in
      let acc = self#position loc_end acc in
      let acc = self#bool loc_ghost acc in
      acc

    method longident_dotop_delims : longident_dotop_delims -> 'acc -> 'acc =
      fun _ acc -> acc

    method longident_str_or_op : longident_str_or_op -> 'acc -> 'acc =
      fun x acc ->
      match x with
      | Str a -> self#string a acc
      | Str_trailing_hash a -> self#string a acc
      | Op a -> self#string a acc
      | DotOp (a, b, c, d) ->
        let acc = self#string a acc in
        let acc = self#longident_dotop_delims b acc in
        let acc = self#string c acc in
        let acc = self#bool d acc in
        acc

    method longident_lid_desc : longident_lid_desc -> 'acc -> 'acc =
      fun x acc ->
      match x with
      | Lident a -> self#longident_str_or_op a acc
      | Ldot (a, b) ->
        let acc = self#longident a acc in
        let acc = self#longident_str_or_op b acc in
        acc
      | Lapply (a, b) ->
        let acc = self#longident a acc in
        let acc = self#longident b acc in
        acc

    method longident : longident -> 'acc -> 'acc =
      fun { desc; tokens } acc ->
      let acc = self#longident_lid_desc desc acc in
      let acc = self#token_seq tokens acc in
      acc

    method attachment : attachment -> 'acc -> 'acc = fun _ acc -> acc

    method comment : comment -> 'acc -> 'acc =
      fun { text; attachement; explicitely_inserted } acc ->
      let acc = self#string text acc in
      let acc = self#attachment attachement acc in
      let acc = self#ref self#bool explicitely_inserted acc in
      acc

    method token_desc : token_desc -> 'acc -> 'acc =
      fun x acc ->
      match x with
      | Token (a, b) ->
        let acc = self#token a acc in
        let acc = self#bool b acc in
        acc
      | Comment a -> self#comment a acc
      | Child_node -> acc

    method token_elt : token_elt -> 'acc -> 'acc =
      fun { desc; pos } acc ->
      let acc = self#token_desc desc acc in
      let acc = self#position pos acc in
      acc

    method token_seq : token_seq -> 'acc -> 'acc = self#list self#token_elt

    method rec_flag : rec_flag -> 'acc -> 'acc = fun _ acc -> acc

    method direction_flag : direction_flag -> 'acc -> 'acc = fun _ acc -> acc

    method private_flag : private_flag -> 'acc -> 'acc = fun _ acc -> acc

    method mutable_flag : mutable_flag -> 'acc -> 'acc = fun _ acc -> acc

    method virtual_flag : virtual_flag -> 'acc -> 'acc = fun _ acc -> acc

    method override_flag : override_flag -> 'acc -> 'acc = fun _ acc -> acc

    method closed_flag : closed_flag -> 'acc -> 'acc = fun _ acc -> acc

    method label : label -> 'acc -> 'acc = self#string

    method arg_label : arg_label -> 'acc -> 'acc =
      fun x acc ->
      match x with
      | Nolabel -> acc
      | Labelled a -> self#string a acc
      | Optional a -> self#string a acc

    method loc : 'a. ('a -> 'acc -> 'acc) -> 'a loc -> 'acc -> 'acc =
      fun _a { txt; loc } acc ->
      let acc = _a txt acc in
      let acc = self#location loc acc in
      acc

    method variance : variance -> 'acc -> 'acc = fun _ acc -> acc

    method injectivity : injectivity -> 'acc -> 'acc = fun _ acc -> acc

    method index_kind : index_kind -> 'acc -> 'acc = fun _ acc -> acc

    method paren_kind : paren_kind -> 'acc -> 'acc = fun _ acc -> acc

    method constant : constant -> 'acc -> 'acc =
      fun x acc ->
      match x with
      | Pconst_integer (a, b, c) ->
        let acc = self#option self#string a acc in
        let acc = self#string b acc in
        let acc = self#option self#char c acc in
        acc
      | Pconst_unboxed_integer (a, b, c) ->
        let acc = self#option self#string a acc in
        let acc = self#string b acc in
        let acc = self#char c acc in
        acc
      | Pconst_char (a, b) ->
        let acc = self#char a acc in
        let acc = self#string b acc in
        acc
      | Pconst_untagged_char (a, b) ->
        let acc = self#char a acc in
        let acc = self#string b acc in
        acc
      | Pconst_string (a, b, c) ->
        let acc = self#string a acc in
        let acc = self#location b acc in
        let acc = self#option self#string c acc in
        acc
      | Pconst_float (a, b, c) ->
        let acc = self#option self#string a acc in
        let acc = self#string b acc in
        let acc = self#option self#char c acc in
        acc
      | Pconst_unboxed_float (a, b, c) ->
        let acc = self#option self#string a acc in
        let acc = self#string b acc in
        let acc = self#option self#char c acc in
        acc

    method modality : modality -> 'acc -> 'acc =
      fun x acc ->
      match x with
      | Modality a -> self#string a acc

    method modalities : modalities -> 'acc -> 'acc =
      self#list (self#loc self#modality)

    method mode : mode -> 'acc -> 'acc =
      fun x acc ->
      match x with
      | Mode a -> self#string a acc

    method modes : modes -> 'acc -> 'acc = self#list (self#loc self#mode)

    method include_kind : include_kind -> 'acc -> 'acc = fun _ acc -> acc

    method attribute : attribute -> 'acc -> 'acc =
      fun { attr_name; attr_payload; attr_loc; attr_tokens } acc ->
      let acc = self#loc (self#list self#string) attr_name acc in
      let acc = self#payload attr_payload acc in
      let acc = self#location attr_loc acc in
      let acc = self#token_seq attr_tokens acc in
      acc

    method extension : extension -> 'acc -> 'acc =
      fun (a, b, c) acc ->
      let acc = self#loc (self#list self#string) a acc in
      let acc = self#payload b acc in
      let acc = self#token_seq c acc in
      acc

    method toplevel_extension : toplevel_extension -> 'acc -> 'acc =
      fun { te_pre_doc; te_ext; te_attrs; te_post_doc } acc ->
      let acc = self#option self#string te_pre_doc acc in
      let acc = self#extension te_ext acc in
      let acc = self#attributes te_attrs acc in
      let acc = self#option self#string te_post_doc acc in
      acc

    method attributes : attributes -> 'acc -> 'acc = self#list self#attribute

    method payload : payload -> 'acc -> 'acc =
      fun x acc ->
      match x with
      | PStr a -> self#structure a acc
      | PSig a -> self#signature a acc
      | PTyp a -> self#core_type a acc
      | PPat (a, b) ->
        let acc = self#pattern a acc in
        let acc = self#option self#expression b acc in
        acc
      | PString (a, b) ->
        let acc = self#string a acc in
        let acc = self#string b acc in
        acc

    method ext_attribute : ext_attribute -> 'acc -> 'acc =
      fun { pea_ext; pea_attrs } acc ->
      let acc = self#option (self#loc (self#list self#string)) pea_ext acc in
      let acc = self#attributes pea_attrs acc in
      acc

    method core_type : core_type -> 'acc -> 'acc =
      fun { ptyp_desc; ptyp_loc; ptyp_attributes; ptyp_tokens } acc ->
      let acc = self#core_type_desc ptyp_desc acc in
      let acc = self#location ptyp_loc acc in
      let acc = self#attributes ptyp_attributes acc in
      let acc = self#token_seq ptyp_tokens acc in
      acc

    method arrow_arg : arrow_arg -> 'acc -> 'acc =
      fun
        { aa_lbl
        ; aa_legacy_modes
        ; aa_type
        ; aa_modes
        ; aa_doc
        ; aa_loc
        ; aa_tokens
        } acc
        ->
      let acc = self#arg_label aa_lbl acc in
      let acc = self#modes aa_legacy_modes acc in
      let acc = self#core_type aa_type acc in
      let acc = self#modes aa_modes acc in
      let acc = self#option self#string aa_doc acc in
      let acc = self#location aa_loc acc in
      let acc = self#token_seq aa_tokens acc in
      acc

    method core_type_desc : core_type_desc -> 'acc -> 'acc =
      fun x acc ->
      match x with
      | Ptyp_any a -> self#option self#jkind_annotation a acc
      | Ptyp_var (a, b) ->
        let acc = self#string a acc in
        let acc = self#option self#jkind_annotation b acc in
        acc
      | Ptyp_arrow { domain; codom_legacy_modes; codom_type; codom_modes } ->
        let acc = self#arrow_arg domain acc in
        let acc = self#modes codom_legacy_modes acc in
        let acc = self#core_type codom_type acc in
        let acc = self#modes codom_modes acc in
        acc
      | Ptyp_tuple a ->
        self
        #
        list
          (fun (a, b) acc ->
            let acc = self#option self#string a acc in
            let acc = self#core_type b acc in
            acc)
          a
          acc
      | Ptyp_unboxed_tuple a ->
        self
        #
        list
          (fun (a, b) acc ->
            let acc = self#option self#string a acc in
            let acc = self#core_type b acc in
            acc)
          a
          acc
      | Ptyp_constr (a, b) ->
        let acc = self#list self#core_type a acc in
        let acc = self#loc self#longident b acc in
        acc
      | Ptyp_object (a, b) ->
        let acc = self#list self#object_field a acc in
        let acc = self#closed_flag b acc in
        acc
      | Ptyp_class (a, b) ->
        let acc = self#loc self#longident a acc in
        let acc = self#list self#core_type b acc in
        acc
      | Ptyp_alias (a, b, c) ->
        let acc = self#core_type a acc in
        let acc = self#option (self#loc self#string) b acc in
        let acc = self#option self#jkind_annotation c acc in
        acc
      | Ptyp_variant (a, b, c) ->
        let acc = self#list self#row_field a acc in
        let acc = self#closed_flag b acc in
        let acc = self#option (self#list self#label) c acc in
        acc
      | Ptyp_poly (a, b) ->
        let acc =
          self
          #
          list
            (fun (a, b) acc ->
              let acc = self#loc self#string a acc in
              let acc = self#option self#jkind_annotation b acc in
              acc)
            a
            acc
        in
        let acc = self#core_type b acc in
        acc
      | Ptyp_package (a, b) ->
        let acc = self#ext_attribute a acc in
        let acc = self#package_type b acc in
        acc
      | Ptyp_open (a, b) ->
        let acc = self#loc self#longident a acc in
        let acc = self#core_type b acc in
        acc
      | Ptyp_quote a -> self#core_type a acc
      | Ptyp_splice a -> self#core_type a acc
      | Ptyp_of_kind a -> self#jkind_annotation a acc
      | Ptyp_extension a -> self#extension a acc
      | Ptyp_parens a -> self#core_type a acc

    method package_type : package_type -> 'acc -> 'acc = self#module_type

    method row_field : row_field -> 'acc -> 'acc =
      fun { prf_desc; prf_loc; prf_attributes; prf_doc; prf_tokens } acc ->
      let acc = self#row_field_desc prf_desc acc in
      let acc = self#location prf_loc acc in
      let acc = self#attributes prf_attributes acc in
      let acc = self#option self#string prf_doc acc in
      let acc = self#token_seq prf_tokens acc in
      acc

    method row_field_desc : row_field_desc -> 'acc -> 'acc =
      fun x acc ->
      match x with
      | Rtag (a, b, c) ->
        let acc = self#loc self#string a acc in
        let acc = self#bool b acc in
        let acc = self#list self#core_type c acc in
        acc
      | Rinherit a -> self#core_type a acc

    method object_field : object_field -> 'acc -> 'acc =
      fun { pof_desc; pof_loc; pof_attributes; pof_doc; pof_tokens } acc ->
      let acc = self#object_field_desc pof_desc acc in
      let acc = self#location pof_loc acc in
      let acc = self#attributes pof_attributes acc in
      let acc = self#option self#string pof_doc acc in
      let acc = self#token_seq pof_tokens acc in
      acc

    method object_field_desc : object_field_desc -> 'acc -> 'acc =
      fun x acc ->
      match x with
      | Otag (a, b) ->
        let acc = self#loc self#string a acc in
        let acc = self#core_type b acc in
        acc
      | Oinherit a -> self#core_type a acc

    method pattern : pattern -> 'acc -> 'acc =
      fun
        { ppat_ext_attr; ppat_desc; ppat_loc; ppat_attributes; ppat_tokens } acc
        ->
      let acc = self#ext_attribute ppat_ext_attr acc in
      let acc = self#pattern_desc ppat_desc acc in
      let acc = self#location ppat_loc acc in
      let acc = self#attributes ppat_attributes acc in
      let acc = self#token_seq ppat_tokens acc in
      acc

    method pattern_desc : pattern_desc -> 'acc -> 'acc =
      fun x acc ->
      match x with
      | Ppat_any -> acc
      | Ppat_var a -> self#loc self#longident_str_or_op a acc
      | Ppat_alias (a, b) ->
        let acc = self#pattern a acc in
        let acc = self#loc self#longident_str_or_op b acc in
        acc
      | Ppat_constant a -> self#constant a acc
      | Ppat_interval (a, b) ->
        let acc = self#constant a acc in
        let acc = self#constant b acc in
        acc
      | Ppat_tuple (a, b) ->
        let acc = self#list (self#argument self#pattern) a acc in
        let acc = self#closed_flag b acc in
        acc
      | Ppat_unboxed_tuple (a, b) ->
        let acc = self#list (self#argument self#pattern) a acc in
        let acc = self#closed_flag b acc in
        acc
      | Ppat_construct (a, b) ->
        let acc = self#loc self#longident a acc in
        let acc =
          self
          #
          option
            (fun (a, b) acc ->
              let acc =
                self
                #
                list
                  (fun (a, b) acc ->
                    let acc = self#loc self#string a acc in
                    let acc = self#option self#jkind_annotation b acc in
                    acc)
                  a
                  acc
              in
              let acc = self#pattern b acc in
              acc)
            b
            acc
        in
        acc
      | Ppat_variant (a, b) ->
        let acc = self#label a acc in
        let acc = self#option self#pattern b acc in
        acc
      | Ppat_record (a, b) ->
        let acc = self#list (self#record_field self#pattern) a acc in
        let acc = self#closed_flag b acc in
        acc
      | Ppat_record_unboxed_product (a, b) ->
        let acc = self#list (self#record_field self#pattern) a acc in
        let acc = self#closed_flag b acc in
        acc
      | Ppat_array (a, b) ->
        let acc = self#mutable_flag a acc in
        let acc = self#list self#pattern b acc in
        acc
      | Ppat_or (a, b) ->
        let acc = self#pattern a acc in
        let acc = self#pattern b acc in
        acc
      | Ppat_constraint (a, b, c) ->
        let acc = self#pattern a acc in
        let acc = self#option self#core_type b acc in
        let acc = self#modes c acc in
        acc
      | Ppat_type a -> self#loc self#longident a acc
      | Ppat_lazy a -> self#pattern a acc
      | Ppat_unpack (a, b) ->
        let acc = self#loc (self#option self#string) a acc in
        let acc = self#option self#package_type b acc in
        acc
      | Ppat_exception a -> self#pattern a acc
      | Ppat_extension a -> self#extension a acc
      | Ppat_open (a, b) ->
        let acc = self#loc self#longident a acc in
        let acc = self#pattern b acc in
        acc
      | Ppat_parens { pat; optional } ->
        let acc = self#pattern pat acc in
        let acc = self#bool optional acc in
        acc
      | Ppat_list a -> self#list self#pattern a acc
      | Ppat_cons (a, b) ->
        let acc = self#pattern a acc in
        let acc = self#pattern b acc in
        acc

    method expression : expression -> 'acc -> 'acc =
      fun
        { pexp_ext_attr; pexp_desc; pexp_loc; pexp_attributes; pexp_tokens } acc
        ->
      let acc = self#ext_attribute pexp_ext_attr acc in
      let acc = self#expression_desc pexp_desc acc in
      let acc = self#location pexp_loc acc in
      let acc = self#attributes pexp_attributes acc in
      let acc = self#token_seq pexp_tokens acc in
      acc

    method expression_desc : expression_desc -> 'acc -> 'acc =
      fun x acc ->
      match x with
      | Pexp_ident a -> self#loc self#longident a acc
      | Pexp_constant a -> self#constant a acc
      | Pexp_let (a, b, c, d) ->
        let acc = self#mutable_flag a acc in
        let acc = self#rec_flag b acc in
        let acc = self#list self#value_binding c acc in
        let acc = self#expression d acc in
        acc
      | Pexp_function (a, b, c) ->
        let acc = self#list self#function_param a acc in
        let acc = self#function_constraint b acc in
        let acc = self#function_body c acc in
        acc
      | Pexp_prefix_apply (a, b) ->
        let acc = self#expression a acc in
        let acc = self#expression b acc in
        acc
      | Pexp_add_or_sub (a, b) ->
        let acc = self#string a acc in
        let acc = self#expression b acc in
        acc
      | Pexp_infix_apply { arg1; op; arg2 } ->
        let acc = self#expression arg1 acc in
        let acc = self#expression op acc in
        let acc = self#expression arg2 acc in
        acc
      | Pexp_apply (a, b) ->
        let acc = self#expression a acc in
        let acc = self#list (self#argument self#expression) b acc in
        acc
      | Pexp_match (a, b) ->
        let acc = self#expression a acc in
        let acc = self#list self#case b acc in
        acc
      | Pexp_try (a, b) ->
        let acc = self#expression a acc in
        let acc = self#list self#case b acc in
        acc
      | Pexp_tuple a -> self#list (self#argument self#expression) a acc
      | Pexp_unboxed_tuple a -> self#list (self#argument self#expression) a acc
      | Pexp_construct (a, b) ->
        let acc = self#loc self#longident a acc in
        let acc = self#option self#expression b acc in
        acc
      | Pexp_variant (a, b) ->
        let acc = self#label a acc in
        let acc = self#option self#expression b acc in
        acc
      | Pexp_record (a, b) ->
        let acc = self#option self#expression a acc in
        let acc = self#list (self#record_field self#expression) b acc in
        acc
      | Pexp_record_unboxed_product (a, b) ->
        let acc = self#option self#expression a acc in
        let acc = self#list (self#record_field self#expression) b acc in
        acc
      | Pexp_field (a, b) ->
        let acc = self#expression a acc in
        let acc = self#loc self#longident b acc in
        acc
      | Pexp_unboxed_field (a, b) ->
        let acc = self#expression a acc in
        let acc = self#loc self#longident b acc in
        acc
      | Pexp_setfield (a, b, c) ->
        let acc = self#expression a acc in
        let acc = self#loc self#longident b acc in
        let acc = self#expression c acc in
        acc
      | Pexp_array (a, b) ->
        let acc = self#mutable_flag a acc in
        let acc = self#list self#expression b acc in
        acc
      | Pexp_idx (a, b) ->
        let acc = self#block_access a acc in
        let acc = self#list self#unboxed_access b acc in
        acc
      | Pexp_ifthenelse (a, b, c) ->
        let acc = self#expression a acc in
        let acc = self#expression b acc in
        let acc = self#option self#expression c acc in
        acc
      | Pexp_sequence (a, b) ->
        let acc = self#expression a acc in
        let acc = self#expression b acc in
        acc
      | Pexp_seq_empty a -> self#expression a acc
      | Pexp_while (a, b) ->
        let acc = self#expression a acc in
        let acc = self#expression b acc in
        acc
      | Pexp_for (a, b, c, d, e) ->
        let acc = self#pattern a acc in
        let acc = self#expression b acc in
        let acc = self#expression c acc in
        let acc = self#direction_flag d acc in
        let acc = self#expression e acc in
        acc
      | Pexp_constraint (a, b, c) ->
        let acc = self#expression a acc in
        let acc = self#option self#core_type b acc in
        let acc = self#modes c acc in
        acc
      | Pexp_coerce (a, b, c) ->
        let acc = self#expression a acc in
        let acc = self#option self#core_type b acc in
        let acc = self#core_type c acc in
        acc
      | Pexp_send (a, b) ->
        let acc = self#expression a acc in
        let acc = self#loc self#string b acc in
        acc
      | Pexp_new a -> self#loc self#longident a acc
      | Pexp_setvar (a, b) ->
        let acc = self#loc self#string a acc in
        let acc = self#expression b acc in
        acc
      | Pexp_override a ->
        self
        #
        list
          (fun (a, b) acc ->
            let acc = self#loc self#string a acc in
            let acc = self#option self#expression b acc in
            acc)
          a
          acc
      | Pexp_letmodule (a, b) ->
        let acc = self#module_binding a acc in
        let acc = self#expression b acc in
        acc
      | Pexp_letexception (a, b) ->
        let acc = self#extension_constructor a acc in
        let acc = self#expression b acc in
        acc
      | Pexp_assert a -> self#expression a acc
      | Pexp_lazy a -> self#expression a acc
      | Pexp_object a -> self#class_structure a acc
      | Pexp_pack (a, b) ->
        let acc = self#module_expr a acc in
        let acc = self#option self#package_type b acc in
        acc
      | Pexp_dot_open (a, b) ->
        let acc = self#loc self#longident a acc in
        let acc = self#expression b acc in
        acc
      | Pexp_let_open (a, b) ->
        let acc = self#open_declaration a acc in
        let acc = self#expression b acc in
        acc
      | Pexp_letop a -> self#letop a acc
      | Pexp_extension a -> self#extension a acc
      | Pexp_unreachable -> acc
      | Pexp_stack a -> self#expression a acc
      | Pexp_comprehension a -> self#comprehension_expression a acc
      | Pexp_overwrite (a, b) ->
        let acc = self#expression a acc in
        let acc = self#expression b acc in
        acc
      | Pexp_quote a -> self#expression a acc
      | Pexp_splice a -> self#expression a acc
      | Pexp_hole -> acc
      | Pexp_index_op { kind; op; seq; indices; assign } ->
        let acc = self#paren_kind kind acc in
        let acc =
          self
          #
          option
            (fun (a, b) acc ->
              let acc = self#option self#longident a acc in
              let acc = self#string b acc in
              acc)
            op
            acc
        in
        let acc = self#expression seq acc in
        let acc = self#list self#expression indices acc in
        let acc = self#option self#expression assign acc in
        acc
      | Pexp_parens { exp; optional } ->
        let acc = self#expression exp acc in
        let acc = self#bool optional acc in
        acc
      | Pexp_begin_end a -> self#option self#expression a acc
      | Pexp_list a -> self#list self#expression a acc
      | Pexp_cons (a, b) ->
        let acc = self#expression a acc in
        let acc = self#expression b acc in
        acc
      | Pexp_exclave a -> self#expression a acc
      | Pexp_mode_legacy (a, b) ->
        let acc = self#loc self#mode a acc in
        let acc = self#expression b acc in
        acc

    method record_field
      : 'a. ('a -> 'acc -> 'acc) -> 'a record_field -> 'acc -> 'acc
      =
      fun _a { field_name; typ; value } acc ->
      let acc = self#loc self#longident field_name acc in
      let acc = self#option self#type_constraint typ acc in
      let acc = self#option _a value acc in
      acc

    method case : case -> 'acc -> 'acc =
      fun { pc_lhs; pc_guard; pc_rhs; pc_tokens } acc ->
      let acc = self#pattern pc_lhs acc in
      let acc = self#option self#expression pc_guard acc in
      let acc = self#expression pc_rhs acc in
      let acc = self#token_seq pc_tokens acc in
      acc

    method letop : letop -> 'acc -> 'acc =
      fun { let_; ands; body } acc ->
      let acc = self#binding_op let_ acc in
      let acc = self#list self#binding_op ands acc in
      let acc = self#expression body acc in
      acc

    method binding_op : binding_op -> 'acc -> 'acc =
      fun { pbop_op; pbop_binding; pbop_loc } acc ->
      let acc = self#loc self#string pbop_op acc in
      let acc = self#value_binding pbop_binding acc in
      let acc = self#location pbop_loc acc in
      acc

    method argument_desc
      : 'a. ('a -> 'acc -> 'acc) -> 'a argument_desc -> 'acc -> 'acc
      =
      fun _a x acc ->
      match x with
      | Parg_unlabelled { legacy_modes; arg; typ_constraint; modes } ->
        let acc = self#modes legacy_modes acc in
        let acc = _a arg acc in
        let acc = self#option self#type_constraint typ_constraint acc in
        let acc = self#modes modes acc in
        acc
      | Parg_labelled
          { optional
          ; legacy_modes
          ; name
          ; maybe_punned
          ; typ_constraint
          ; modes
          ; default
          } ->
        let acc = self#bool optional acc in
        let acc = self#modes legacy_modes acc in
        let acc = self#string name acc in
        let acc = self#option _a maybe_punned acc in
        let acc = self#option self#type_constraint typ_constraint acc in
        let acc = self#modes modes acc in
        let acc = self#option self#expression default acc in
        acc

    method argument : 'a. ('a -> 'acc -> 'acc) -> 'a argument -> 'acc -> 'acc =
      fun _a { parg_desc; parg_tokens } acc ->
      let acc = self#argument_desc _a parg_desc acc in
      let acc = self#token_seq parg_tokens acc in
      acc

    method function_param_desc : function_param_desc -> 'acc -> 'acc =
      fun x acc ->
      match x with
      | Pparam_val a -> self#argument self#pattern a acc
      | Pparam_newtype (a, b) ->
        let acc = self#loc self#string a acc in
        let acc = self#option self#jkind_annotation b acc in
        acc
      | Pparam_newtypes a ->
        self
        #
        list
          (fun (a, b) acc ->
            let acc = self#loc self#string a acc in
            let acc = self#option self#jkind_annotation b acc in
            acc)
          a
          acc

    method function_param : function_param -> 'acc -> 'acc =
      fun { pparam_loc; pparam_desc } acc ->
      let acc = self#location pparam_loc acc in
      let acc = self#function_param_desc pparam_desc acc in
      acc

    method function_body : function_body -> 'acc -> 'acc =
      fun { pfb_desc; pfb_loc; pfb_tokens } acc ->
      let acc = self#function_body_desc pfb_desc acc in
      let acc = self#location pfb_loc acc in
      let acc = self#token_seq pfb_tokens acc in
      acc

    method function_body_desc : function_body_desc -> 'acc -> 'acc =
      fun x acc ->
      match x with
      | Pfunction_body a -> self#expression a acc
      | Pfunction_cases (a, b) ->
        let acc = self#list self#case a acc in
        let acc = self#ext_attribute b acc in
        acc

    method type_constraint : type_constraint -> 'acc -> 'acc =
      fun x acc ->
      match x with
      | Pconstraint a -> self#core_type a acc
      | Pcoerce (a, b) ->
        let acc = self#option self#core_type a acc in
        let acc = self#core_type b acc in
        acc

    method function_constraint : function_constraint -> 'acc -> 'acc =
      fun { ret_mode_annotations; ret_type_constraint } acc ->
      let acc = self#modes ret_mode_annotations acc in
      let acc = self#option self#type_constraint ret_type_constraint acc in
      acc

    method block_access : block_access -> 'acc -> 'acc =
      fun x acc ->
      match x with
      | Baccess_field a -> self#loc self#longident a acc
      | Baccess_array (a, b, c) ->
        let acc = self#mutable_flag a acc in
        let acc = self#index_kind b acc in
        let acc = self#expression c acc in
        acc
      | Baccess_block (a, b) ->
        let acc = self#mutable_flag a acc in
        let acc = self#expression b acc in
        acc

    method unboxed_access : unboxed_access -> 'acc -> 'acc =
      fun x acc ->
      match x with
      | Uaccess_unboxed_field a -> self#loc self#longident a acc

    method comprehension_iterator : comprehension_iterator -> 'acc -> 'acc =
      fun x acc ->
      match x with
      | Pcomp_range { start; stop; direction } ->
        let acc = self#expression start acc in
        let acc = self#expression stop acc in
        let acc = self#direction_flag direction acc in
        acc
      | Pcomp_in a -> self#expression a acc

    method comprehension_clause_binding
      : comprehension_clause_binding -> 'acc -> 'acc
      =
      fun
        { pcomp_cb_mode
        ; pcomp_cb_pattern
        ; pcomp_cb_iterator
        ; pcomp_cb_attributes
        ; pcomp_cb_tokens
        } acc
        ->
      let acc = self#option (self#loc self#mode) pcomp_cb_mode acc in
      let acc = self#pattern pcomp_cb_pattern acc in
      let acc = self#comprehension_iterator pcomp_cb_iterator acc in
      let acc = self#attributes pcomp_cb_attributes acc in
      let acc = self#token_seq pcomp_cb_tokens acc in
      acc

    method comprehension_clause : comprehension_clause -> 'acc -> 'acc =
      fun x acc ->
      match x with
      | Pcomp_for a -> self#list self#comprehension_clause_binding a acc
      | Pcomp_when a -> self#expression a acc

    method comprehension : comprehension -> 'acc -> 'acc =
      fun { pcomp_body; pcomp_clauses; pcomp_tokens } acc ->
      let acc = self#expression pcomp_body acc in
      let acc = self#list self#comprehension_clause pcomp_clauses acc in
      let acc = self#token_seq pcomp_tokens acc in
      acc

    method comprehension_expression : comprehension_expression -> 'acc -> 'acc =
      fun x acc ->
      match x with
      | Pcomp_list_comprehension a -> self#comprehension a acc
      | Pcomp_array_comprehension (a, b) ->
        let acc = self#mutable_flag a acc in
        let acc = self#comprehension b acc in
        acc

    method value_description : value_description -> 'acc -> 'acc =
      fun
        { pval_pre_doc
        ; pval_ext_attrs
        ; pval_name
        ; pval_type
        ; pval_modalities
        ; pval_prim
        ; pval_attributes
        ; pval_post_doc
        ; pval_loc
        ; pval_tokens
        } acc
        ->
      let acc = self#option self#string pval_pre_doc acc in
      let acc = self#ext_attribute pval_ext_attrs acc in
      let acc = self#loc self#longident_str_or_op pval_name acc in
      let acc = self#core_type pval_type acc in
      let acc = self#modalities pval_modalities acc in
      let acc = self#list self#string pval_prim acc in
      let acc = self#attributes pval_attributes acc in
      let acc = self#option self#string pval_post_doc acc in
      let acc = self#location pval_loc acc in
      let acc = self#token_seq pval_tokens acc in
      acc

    method ptype_param : ptype_param -> 'acc -> 'acc =
      fun { ptp_typ; ptp_infos; ptp_tokens } acc ->
      let acc = self#core_type ptp_typ acc in
      let acc =
        (fun (a, b) acc ->
          let acc = self#variance a acc in
          let acc = self#injectivity b acc in
          acc)
          ptp_infos
          acc
      in
      let acc = self#token_seq ptp_tokens acc in
      acc

    method ptype_params : ptype_params -> 'acc -> 'acc =
      self#list self#ptype_param

    method ptype_constraint : ptype_constraint -> 'acc -> 'acc =
      fun (a, b, c) acc ->
      let acc = self#core_type a acc in
      let acc = self#core_type b acc in
      let acc = self#location c acc in
      acc

    method type_declaration : type_declaration -> 'acc -> 'acc =
      fun
        { ptype_pre_text
        ; ptype_pre_doc
        ; ptype_ext_attrs
        ; ptype_name
        ; ptype_params
        ; ptype_jkind_annotation
        ; ptype_private
        ; ptype_manifest
        ; ptype_kind
        ; ptype_cstrs
        ; ptype_attributes
        ; ptype_post_doc
        ; ptype_loc
        ; ptype_tokens
        } acc
        ->
      let acc = self#list self#string ptype_pre_text acc in
      let acc = self#option self#string ptype_pre_doc acc in
      let acc = self#ext_attribute ptype_ext_attrs acc in
      let acc = self#loc self#string ptype_name acc in
      let acc = self#ptype_params ptype_params acc in
      let acc = self#option self#jkind_annotation ptype_jkind_annotation acc in
      let acc = self#private_flag ptype_private acc in
      let acc = self#option self#core_type ptype_manifest acc in
      let acc = self#type_kind ptype_kind acc in
      let acc = self#list self#ptype_constraint ptype_cstrs acc in
      let acc = self#attributes ptype_attributes acc in
      let acc = self#option self#string ptype_post_doc acc in
      let acc = self#location ptype_loc acc in
      let acc = self#token_seq ptype_tokens acc in
      acc

    method type_kind : type_kind -> 'acc -> 'acc =
      fun x acc ->
      match x with
      | Ptype_abstract -> acc
      | Ptype_variant a -> self#list self#constructor_declaration a acc
      | Ptype_record a -> self#list self#label_declaration a acc
      | Ptype_record_unboxed_product a -> self#list self#label_declaration a acc
      | Ptype_open -> acc

    method label_declaration : label_declaration -> 'acc -> 'acc =
      fun
        { pld_name
        ; pld_mutable
        ; pld_global
        ; pld_modalities
        ; pld_type
        ; pld_loc
        ; pld_attributes
        ; pld_doc
        ; pld_tokens
        } acc
        ->
      let acc = self#loc self#string pld_name acc in
      let acc = self#mutable_flag pld_mutable acc in
      let acc = self#bool pld_global acc in
      let acc = self#modalities pld_modalities acc in
      let acc = self#core_type pld_type acc in
      let acc = self#location pld_loc acc in
      let acc = self#attributes pld_attributes acc in
      let acc = self#option self#string pld_doc acc in
      let acc = self#token_seq pld_tokens acc in
      acc

    method constructor_declaration : constructor_declaration -> 'acc -> 'acc =
      fun
        { pcd_name
        ; pcd_vars
        ; pcd_args
        ; pcd_res
        ; pcd_loc
        ; pcd_attributes
        ; pcd_doc
        ; pcd_tokens
        } acc
        ->
      let acc = self#loc self#longident_str_or_op pcd_name acc in
      let acc =
        self
        #
        list
          (fun (a, b) acc ->
            let acc = self#loc self#string a acc in
            let acc = self#option self#jkind_annotation b acc in
            acc)
          pcd_vars
          acc
      in
      let acc = self#constructor_arguments pcd_args acc in
      let acc = self#option self#core_type pcd_res acc in
      let acc = self#location pcd_loc acc in
      let acc = self#attributes pcd_attributes acc in
      let acc = self#option self#string pcd_doc acc in
      let acc = self#token_seq pcd_tokens acc in
      acc

    method constructor_argument : constructor_argument -> 'acc -> 'acc =
      fun { pca_global; pca_type; pca_modalities; pca_loc } acc ->
      let acc = self#bool pca_global acc in
      let acc = self#core_type pca_type acc in
      let acc = self#modalities pca_modalities acc in
      let acc = self#location pca_loc acc in
      acc

    method constructor_arguments : constructor_arguments -> 'acc -> 'acc =
      fun x acc ->
      match x with
      | Pcstr_tuple a -> self#list self#constructor_argument a acc
      | Pcstr_record a -> self#list self#label_declaration a acc

    method type_extension : type_extension -> 'acc -> 'acc =
      fun
        { ptyext_pre_doc
        ; ptyext_ext_attrs
        ; ptyext_path
        ; ptyext_params
        ; ptyext_constructors
        ; ptyext_private
        ; ptyext_loc
        ; ptyext_attributes
        ; ptyext_post_doc
        ; ptyext_tokens
        } acc
        ->
      let acc = self#option self#string ptyext_pre_doc acc in
      let acc = self#ext_attribute ptyext_ext_attrs acc in
      let acc = self#loc self#longident ptyext_path acc in
      let acc = self#list self#ptype_param ptyext_params acc in
      let acc = self#list self#extension_constructor ptyext_constructors acc in
      let acc = self#private_flag ptyext_private acc in
      let acc = self#location ptyext_loc acc in
      let acc = self#attributes ptyext_attributes acc in
      let acc = self#option self#string ptyext_post_doc acc in
      let acc = self#token_seq ptyext_tokens acc in
      acc

    method extension_constructor : extension_constructor -> 'acc -> 'acc =
      fun
        { pext_name
        ; pext_kind
        ; pext_loc
        ; pext_attributes
        ; pext_doc
        ; pext_tokens
        } acc
        ->
      let acc = self#loc self#longident_str_or_op pext_name acc in
      let acc = self#extension_constructor_kind pext_kind acc in
      let acc = self#location pext_loc acc in
      let acc = self#attributes pext_attributes acc in
      let acc = self#option self#string pext_doc acc in
      let acc = self#token_seq pext_tokens acc in
      acc

    method type_exception : type_exception -> 'acc -> 'acc =
      fun
        { ptyexn_pre_doc
        ; ptyexn_ext_attrs
        ; ptyexn_constructor
        ; ptyexn_loc
        ; ptyexn_attributes
        ; ptyexn_post_doc
        ; ptyexn_tokens
        } acc
        ->
      let acc = self#option self#string ptyexn_pre_doc acc in
      let acc = self#ext_attribute ptyexn_ext_attrs acc in
      let acc = self#extension_constructor ptyexn_constructor acc in
      let acc = self#location ptyexn_loc acc in
      let acc = self#attributes ptyexn_attributes acc in
      let acc = self#option self#string ptyexn_post_doc acc in
      let acc = self#token_seq ptyexn_tokens acc in
      acc

    method extension_constructor_kind
      : extension_constructor_kind -> 'acc -> 'acc
      =
      fun x acc ->
      match x with
      | Pext_decl (a, b, c) ->
        let acc =
          self
          #
          list
            (fun (a, b) acc ->
              let acc = self#loc self#string a acc in
              let acc = self#option self#jkind_annotation b acc in
              acc)
            a
            acc
        in
        let acc = self#constructor_arguments b acc in
        let acc = self#option self#core_type c acc in
        acc
      | Pext_rebind a -> self#loc self#longident a acc

    method class_type : class_type -> 'acc -> 'acc =
      fun { pcty_desc; pcty_loc; pcty_attributes; pcty_tokens } acc ->
      let acc = self#class_type_desc pcty_desc acc in
      let acc = self#location pcty_loc acc in
      let acc = self#attributes pcty_attributes acc in
      let acc = self#token_seq pcty_tokens acc in
      acc

    method class_type_desc : class_type_desc -> 'acc -> 'acc =
      fun x acc ->
      match x with
      | Pcty_constr (a, b) ->
        let acc = self#loc self#longident a acc in
        let acc = self#list self#core_type b acc in
        acc
      | Pcty_signature a -> self#class_signature a acc
      | Pcty_arrow (a, b) ->
        let acc = self#arrow_arg a acc in
        let acc = self#class_type b acc in
        acc
      | Pcty_extension a -> self#extension a acc
      | Pcty_open (a, b) ->
        let acc = self#open_description a acc in
        let acc = self#class_type b acc in
        acc

    method class_signature : class_signature -> 'acc -> 'acc =
      fun { pcsig_self; pcsig_fields } acc ->
      let acc = self#option self#core_type pcsig_self acc in
      let acc = self#list self#class_type_field pcsig_fields acc in
      acc

    method class_type_field : class_type_field -> 'acc -> 'acc =
      fun
        { pctf_pre_doc
        ; pctf_desc
        ; pctf_loc
        ; pctf_attributes
        ; pctf_post_doc
        ; pctf_tokens
        } acc
        ->
      let acc = self#option self#string pctf_pre_doc acc in
      let acc = self#class_type_field_desc pctf_desc acc in
      let acc = self#location pctf_loc acc in
      let acc = self#attributes pctf_attributes acc in
      let acc = self#option self#string pctf_post_doc acc in
      let acc = self#token_seq pctf_tokens acc in
      acc

    method class_type_field_desc : class_type_field_desc -> 'acc -> 'acc =
      fun x acc ->
      match x with
      | Pctf_inherit a -> self#class_type a acc
      | Pctf_val a ->
        (fun (a, b, c, d) acc ->
          let acc = self#loc self#string a acc in
          let acc = self#mutable_flag b acc in
          let acc = self#virtual_flag c acc in
          let acc = self#core_type d acc in
          acc)
          a
          acc
      | Pctf_method a ->
        (fun (a, b, c, d) acc ->
          let acc = self#loc self#string a acc in
          let acc = self#private_flag b acc in
          let acc = self#virtual_flag c acc in
          let acc = self#core_type d acc in
          acc)
          a
          acc
      | Pctf_constraint a ->
        (fun (a, b) acc ->
          let acc = self#core_type a acc in
          let acc = self#core_type b acc in
          acc)
          a
          acc
      | Pctf_attribute a -> self#attribute a acc
      | Pctf_extension a -> self#extension a acc
      | Pctf_docstring a -> self#string a acc

    method class_infos
      : 'a. ('a -> 'acc -> 'acc) -> 'a class_infos -> 'acc -> 'acc
      =
      fun
        _a
        { pci_pre_text
        ; pci_pre_doc
        ; pci_virt
        ; pci_ext_attrs
        ; pci_params
        ; pci_name
        ; pci_value_params
        ; pci_constraint
        ; pci_expr
        ; pci_loc
        ; pci_attributes
        ; pci_post_doc
        ; pci_tokens
        } acc
        ->
      let acc = self#list self#string pci_pre_text acc in
      let acc = self#option self#string pci_pre_doc acc in
      let acc = self#virtual_flag pci_virt acc in
      let acc = self#ext_attribute pci_ext_attrs acc in
      let acc = self#list self#ptype_param pci_params acc in
      let acc = self#loc self#string pci_name acc in
      let acc = self#list (self#argument self#pattern) pci_value_params acc in
      let acc = self#option self#class_type pci_constraint acc in
      let acc = _a pci_expr acc in
      let acc = self#location pci_loc acc in
      let acc = self#attributes pci_attributes acc in
      let acc = self#option self#string pci_post_doc acc in
      let acc = self#token_seq pci_tokens acc in
      acc

    method class_description : class_description -> 'acc -> 'acc =
      self#class_infos self#class_type

    method class_type_declaration : class_type_declaration -> 'acc -> 'acc =
      self#class_infos self#class_type

    method class_expr : class_expr -> 'acc -> 'acc =
      fun { pcl_ext_attrs; pcl_desc; pcl_loc; pcl_attributes } acc ->
      let acc = self#ext_attribute pcl_ext_attrs acc in
      let acc = self#class_expr_desc pcl_desc acc in
      let acc = self#location pcl_loc acc in
      let acc = self#attributes pcl_attributes acc in
      acc

    method class_expr_desc : class_expr_desc -> 'acc -> 'acc =
      fun x acc ->
      match x with
      | Pcl_constr (a, b) ->
        let acc = self#loc self#longident a acc in
        let acc = self#list self#core_type b acc in
        acc
      | Pcl_structure a -> self#class_structure a acc
      | Pcl_fun (a, b) ->
        let acc = self#list (self#argument self#pattern) a acc in
        let acc = self#class_expr b acc in
        acc
      | Pcl_apply (a, b) ->
        let acc = self#class_expr a acc in
        let acc = self#list (self#argument self#expression) b acc in
        acc
      | Pcl_let (a, b, c) ->
        let acc = self#rec_flag a acc in
        let acc = self#list self#value_binding b acc in
        let acc = self#class_expr c acc in
        acc
      | Pcl_constraint (a, b) ->
        let acc = self#class_expr a acc in
        let acc = self#class_type b acc in
        acc
      | Pcl_extension a -> self#extension a acc
      | Pcl_open (a, b) ->
        let acc = self#open_description a acc in
        let acc = self#class_expr b acc in
        acc
      | Pcl_parens a -> self#class_expr a acc

    method class_structure : class_structure -> 'acc -> 'acc =
      fun { pcstr_self; pcstr_fields } acc ->
      let acc = self#pattern pcstr_self acc in
      let acc = self#list self#class_field pcstr_fields acc in
      acc

    method class_field : class_field -> 'acc -> 'acc =
      fun
        { pcf_pre_doc
        ; pcf_desc
        ; pcf_loc
        ; pcf_attributes
        ; pcf_post_doc
        ; pcf_tokens
        } acc
        ->
      let acc = self#option self#string pcf_pre_doc acc in
      let acc = self#class_field_desc pcf_desc acc in
      let acc = self#location pcf_loc acc in
      let acc = self#attributes pcf_attributes acc in
      let acc = self#option self#string pcf_post_doc acc in
      let acc = self#token_seq pcf_tokens acc in
      acc

    method class_field_desc : class_field_desc -> 'acc -> 'acc =
      fun x acc ->
      match x with
      | Pcf_inherit (a, b, c) ->
        let acc = self#override_flag a acc in
        let acc = self#class_expr b acc in
        let acc = self#option (self#loc self#string) c acc in
        acc
      | Pcf_val a ->
        (fun (a, b, c) acc ->
          let acc = self#loc self#string a acc in
          let acc = self#mutable_flag b acc in
          let acc = self#class_field_kind c acc in
          acc)
          a
          acc
      | Pcf_method a ->
        (fun (a, b, c) acc ->
          let acc = self#loc self#string a acc in
          let acc = self#private_flag b acc in
          let acc = self#class_field_kind c acc in
          acc)
          a
          acc
      | Pcf_constraint a ->
        (fun (a, b) acc ->
          let acc = self#core_type a acc in
          let acc = self#core_type b acc in
          acc)
          a
          acc
      | Pcf_initializer a -> self#expression a acc
      | Pcf_attribute a -> self#attribute a acc
      | Pcf_extension a -> self#extension a acc
      | Pcf_docstring a -> self#string a acc

    method class_field_kind : class_field_kind -> 'acc -> 'acc =
      fun x acc ->
      match x with
      | Cfk_virtual a -> self#core_type a acc
      | Cfk_concrete (a, b) ->
        let acc = self#override_flag a acc in
        let acc = self#value_binding b acc in
        acc

    method class_declaration : class_declaration -> 'acc -> 'acc =
      self#class_infos self#class_expr

    method module_type : module_type -> 'acc -> 'acc =
      fun { pmty_desc; pmty_loc; pmty_attributes; pmty_tokens } acc ->
      let acc = self#module_type_desc pmty_desc acc in
      let acc = self#location pmty_loc acc in
      let acc = self#attributes pmty_attributes acc in
      let acc = self#token_seq pmty_tokens acc in
      acc

    method module_type_desc : module_type_desc -> 'acc -> 'acc =
      fun x acc ->
      match x with
      | Pmty_ident a -> self#loc self#longident a acc
      | Pmty_signature a -> self#signature a acc
      | Pmty_functor (a, b, c, d) ->
        let acc = self#attributes a acc in
        let acc = self#list self#functor_parameter b acc in
        let acc = self#module_type c acc in
        let acc = self#modes d acc in
        acc
      | Pmty_functor_type (a, b, c) ->
        let acc = self#list self#functor_parameter a acc in
        let acc = self#module_type b acc in
        let acc = self#modes c acc in
        acc
      | Pmty_with (a, b) ->
        let acc = self#module_type a acc in
        let acc = self#list self#with_constraint b acc in
        acc
      | Pmty_typeof (a, b) ->
        let acc = self#attributes a acc in
        let acc = self#module_expr b acc in
        acc
      | Pmty_extension a -> self#extension a acc
      | Pmty_alias a -> self#loc self#longident a acc
      | Pmty_strengthen (a, b) ->
        let acc = self#module_type a acc in
        let acc = self#loc self#longident b acc in
        acc
      | Pmty_parens a -> self#module_type a acc

    method functor_parameter : functor_parameter -> 'acc -> 'acc =
      fun x acc ->
      match x with
      | Unit -> acc
      | Named (a, b, c) ->
        let acc = self#loc (self#option self#string) a acc in
        let acc = self#module_type b acc in
        let acc = self#modes c acc in
        acc
      | Unnamed (a, b) ->
        let acc = self#module_type a acc in
        let acc = self#modes b acc in
        acc

    method signature : signature -> 'acc -> 'acc =
      fun { psg_modalities; psg_items; psg_loc; psg_tokens } acc ->
      let acc = self#modalities psg_modalities acc in
      let acc = self#list self#signature_item psg_items acc in
      let acc = self#location psg_loc acc in
      let acc = self#token_seq psg_tokens acc in
      acc

    method signature_item : signature_item -> 'acc -> 'acc =
      fun { psig_desc; psig_loc; psig_tokens } acc ->
      let acc = self#signature_item_desc psig_desc acc in
      let acc = self#location psig_loc acc in
      let acc = self#token_seq psig_tokens acc in
      acc

    method signature_item_desc : signature_item_desc -> 'acc -> 'acc =
      fun x acc ->
      match x with
      | Psig_value a -> self#value_description a acc
      | Psig_type (a, b) ->
        let acc = self#rec_flag a acc in
        let acc = self#list self#type_declaration b acc in
        acc
      | Psig_typesubst a -> self#list self#type_declaration a acc
      | Psig_typext a -> self#type_extension a acc
      | Psig_exception a -> self#type_exception a acc
      | Psig_module a -> self#module_declaration a acc
      | Psig_modsubst a -> self#module_substitution a acc
      | Psig_recmodule a -> self#list self#module_declaration a acc
      | Psig_modtype a -> self#module_type_declaration a acc
      | Psig_modtypesubst a -> self#module_type_declaration a acc
      | Psig_open a -> self#open_description a acc
      | Psig_include (a, b) ->
        let acc = self#include_description a acc in
        let acc = self#modalities b acc in
        acc
      | Psig_class a -> self#list self#class_description a acc
      | Psig_class_type a -> self#list self#class_type_declaration a acc
      | Psig_attribute a -> self#attribute a acc
      | Psig_extension a -> self#toplevel_extension a acc
      | Psig_kind_abbrev (a, b) ->
        let acc = self#loc self#string a acc in
        let acc = self#jkind_annotation b acc in
        acc
      | Psig_docstring a -> self#string a acc

    method module_declaration_body : module_declaration_body -> 'acc -> 'acc =
      fun x acc ->
      match x with
      | With_params (a, b, c) ->
        let acc = self#list self#functor_parameter a acc in
        let acc = self#module_type b acc in
        let acc = self#modes c acc in
        acc
      | Without_params (a, b) ->
        let acc = self#module_type a acc in
        let acc = self#modalities b acc in
        acc

    method module_declaration : module_declaration -> 'acc -> 'acc =
      fun
        { pmd_pre_text
        ; pmd_pre_doc
        ; pmd_ext_attrs
        ; pmd_name
        ; pmd_body
        ; pmd_attributes
        ; pmd_post_doc
        ; pmd_loc
        ; pmd_tokens
        } acc
        ->
      let acc = self#list self#string pmd_pre_text acc in
      let acc = self#option self#string pmd_pre_doc acc in
      let acc = self#ext_attribute pmd_ext_attrs acc in
      let acc =
        (fun (a, b) acc ->
          let acc = self#loc (self#option self#string) a acc in
          let acc = self#modalities b acc in
          acc)
          pmd_name
          acc
      in
      let acc = self#module_declaration_body pmd_body acc in
      let acc = self#attributes pmd_attributes acc in
      let acc = self#option self#string pmd_post_doc acc in
      let acc = self#location pmd_loc acc in
      let acc = self#token_seq pmd_tokens acc in
      acc

    method module_substitution : module_substitution -> 'acc -> 'acc =
      fun
        { pms_pre_doc
        ; pms_ext_attrs
        ; pms_name
        ; pms_manifest
        ; pms_attributes
        ; pms_post_doc
        ; pms_loc
        ; pms_tokens
        } acc
        ->
      let acc = self#option self#string pms_pre_doc acc in
      let acc = self#ext_attribute pms_ext_attrs acc in
      let acc = self#loc self#string pms_name acc in
      let acc = self#loc self#longident pms_manifest acc in
      let acc = self#attributes pms_attributes acc in
      let acc = self#option self#string pms_post_doc acc in
      let acc = self#location pms_loc acc in
      let acc = self#token_seq pms_tokens acc in
      acc

    method module_type_declaration : module_type_declaration -> 'acc -> 'acc =
      fun
        { pmtd_pre_doc
        ; pmtd_ext_attrs
        ; pmtd_name
        ; pmtd_type
        ; pmtd_attributes
        ; pmtd_post_doc
        ; pmtd_loc
        ; pmtd_tokens
        } acc
        ->
      let acc = self#option self#string pmtd_pre_doc acc in
      let acc = self#ext_attribute pmtd_ext_attrs acc in
      let acc = self#loc self#string pmtd_name acc in
      let acc = self#option self#module_type pmtd_type acc in
      let acc = self#attributes pmtd_attributes acc in
      let acc = self#option self#string pmtd_post_doc acc in
      let acc = self#location pmtd_loc acc in
      let acc = self#token_seq pmtd_tokens acc in
      acc

    method open_infos
      : 'a. ('a -> 'acc -> 'acc) -> 'a open_infos -> 'acc -> 'acc
      =
      fun
        _a
        { popen_pre_doc
        ; popen_ext_attrs
        ; popen_expr
        ; popen_override
        ; popen_loc
        ; popen_attributes
        ; popen_post_doc
        ; popen_tokens
        } acc
        ->
      let acc = self#option self#string popen_pre_doc acc in
      let acc = self#ext_attribute popen_ext_attrs acc in
      let acc = _a popen_expr acc in
      let acc = self#override_flag popen_override acc in
      let acc = self#location popen_loc acc in
      let acc = self#attributes popen_attributes acc in
      let acc = self#option self#string popen_post_doc acc in
      let acc = self#token_seq popen_tokens acc in
      acc

    method open_description : open_description -> 'acc -> 'acc =
      self#open_infos (self#loc self#longident)

    method open_declaration : open_declaration -> 'acc -> 'acc =
      self#open_infos self#module_expr

    method include_infos
      : 'a. ('a -> 'acc -> 'acc) -> 'a include_infos -> 'acc -> 'acc
      =
      fun
        _a
        { pincl_pre_doc
        ; pincl_kind
        ; pincl_ext_attrs
        ; pincl_mod
        ; pincl_loc
        ; pincl_attributes
        ; pincl_post_doc
        ; pincl_tokens
        } acc
        ->
      let acc = self#option self#string pincl_pre_doc acc in
      let acc = self#include_kind pincl_kind acc in
      let acc = self#ext_attribute pincl_ext_attrs acc in
      let acc = _a pincl_mod acc in
      let acc = self#location pincl_loc acc in
      let acc = self#attributes pincl_attributes acc in
      let acc = self#option self#string pincl_post_doc acc in
      let acc = self#token_seq pincl_tokens acc in
      acc

    method include_description : include_description -> 'acc -> 'acc =
      self#include_infos self#module_type

    method include_declaration : include_declaration -> 'acc -> 'acc =
      self#include_infos self#module_expr

    method with_constraint : with_constraint -> 'acc -> 'acc =
      fun { wc_desc; wc_loc; wc_tokens } acc ->
      let acc = self#with_constraint_desc wc_desc acc in
      let acc = self#location wc_loc acc in
      let acc = self#token_seq wc_tokens acc in
      acc

    method with_constraint_desc : with_constraint_desc -> 'acc -> 'acc =
      fun x acc ->
      match x with
      | Pwith_type (a, b, c, d, e) ->
        let acc = self#ptype_params a acc in
        let acc = self#loc self#longident b acc in
        let acc = self#private_flag c acc in
        let acc = self#core_type d acc in
        let acc = self#list self#ptype_constraint e acc in
        acc
      | Pwith_module (a, b) ->
        let acc = self#loc self#longident a acc in
        let acc = self#loc self#longident b acc in
        acc
      | Pwith_modtype (a, b) ->
        let acc = self#loc self#longident a acc in
        let acc = self#module_type b acc in
        acc
      | Pwith_modtypesubst (a, b) ->
        let acc = self#loc self#longident a acc in
        let acc = self#module_type b acc in
        acc
      | Pwith_typesubst (a, b, c) ->
        let acc = self#ptype_params a acc in
        let acc = self#loc self#longident b acc in
        let acc = self#core_type c acc in
        acc
      | Pwith_modsubst (a, b) ->
        let acc = self#loc self#longident a acc in
        let acc = self#loc self#longident b acc in
        acc

    method module_expr : module_expr -> 'acc -> 'acc =
      fun { pmod_desc; pmod_loc; pmod_attributes; pmod_tokens } acc ->
      let acc = self#module_expr_desc pmod_desc acc in
      let acc = self#location pmod_loc acc in
      let acc = self#attributes pmod_attributes acc in
      let acc = self#token_seq pmod_tokens acc in
      acc

    method module_expr_desc : module_expr_desc -> 'acc -> 'acc =
      fun x acc ->
      match x with
      | Pmod_ident a -> self#loc self#longident a acc
      | Pmod_structure (a, b) ->
        let acc = self#attributes a acc in
        let acc = self#structure b acc in
        acc
      | Pmod_functor (a, b, c) ->
        let acc = self#attributes a acc in
        let acc = self#list self#functor_parameter b acc in
        let acc = self#module_expr c acc in
        acc
      | Pmod_apply (a, b) ->
        let acc = self#module_expr a acc in
        let acc = self#module_expr b acc in
        acc
      | Pmod_apply_unit a -> self#module_expr a acc
      | Pmod_constraint (a, b, c) ->
        let acc = self#module_expr a acc in
        let acc = self#option self#module_type b acc in
        let acc = self#modes c acc in
        acc
      | Pmod_unpack (a, b, c) ->
        let acc = self#expression a acc in
        let acc = self#option self#package_type b acc in
        let acc = self#option self#package_type c acc in
        acc
      | Pmod_extension a -> self#extension a acc
      | Pmod_parens a -> self#module_expr a acc

    method structure : structure -> 'acc -> 'acc =
      fun (a, b) acc ->
      let acc = self#list self#structure_item a acc in
      let acc = self#token_seq b acc in
      acc

    method structure_item : structure_item -> 'acc -> 'acc =
      fun { pstr_desc; pstr_loc; pstr_tokens } acc ->
      let acc = self#structure_item_desc pstr_desc acc in
      let acc = self#location pstr_loc acc in
      let acc = self#token_seq pstr_tokens acc in
      acc

    method structure_item_desc : structure_item_desc -> 'acc -> 'acc =
      fun x acc ->
      match x with
      | Pstr_eval (a, b) ->
        let acc = self#expression a acc in
        let acc = self#attributes b acc in
        acc
      | Pstr_value (a, b) ->
        let acc = self#rec_flag a acc in
        let acc = self#list self#value_binding b acc in
        acc
      | Pstr_primitive a -> self#value_description a acc
      | Pstr_type (a, b) ->
        let acc = self#rec_flag a acc in
        let acc = self#list self#type_declaration b acc in
        acc
      | Pstr_typext a -> self#type_extension a acc
      | Pstr_exception a -> self#type_exception a acc
      | Pstr_module a -> self#module_binding a acc
      | Pstr_recmodule a -> self#list self#module_binding a acc
      | Pstr_modtype a -> self#module_type_declaration a acc
      | Pstr_open a -> self#open_declaration a acc
      | Pstr_class a -> self#list self#class_declaration a acc
      | Pstr_class_type a -> self#list self#class_type_declaration a acc
      | Pstr_include a -> self#include_declaration a acc
      | Pstr_attribute a -> self#attribute a acc
      | Pstr_extension a -> self#toplevel_extension a acc
      | Pstr_kind_abbrev (a, b) ->
        let acc = self#loc self#string a acc in
        let acc = self#jkind_annotation b acc in
        acc
      | Pstr_docstring a -> self#string a acc

    method value_constraint : value_constraint -> 'acc -> 'acc =
      fun x acc ->
      match x with
      | Pvc_constraint { locally_abstract_univars; typ } ->
        let acc =
          self
          #
          list
            (fun (a, b) acc ->
              let acc = self#loc self#string a acc in
              let acc = self#option self#jkind_annotation b acc in
              acc)
            locally_abstract_univars
            acc
        in
        let acc = self#core_type typ acc in
        acc
      | Pvc_coercion { ground; coercion } ->
        let acc = self#option self#core_type ground acc in
        let acc = self#core_type coercion acc in
        acc

    method value_binding : value_binding -> 'acc -> 'acc =
      fun
        { pvb_pre_text
        ; pvb_pre_doc
        ; pvb_ext_attrs
        ; pvb_legacy_modes
        ; pvb_pat
        ; pvb_modes
        ; pvb_params
        ; pvb_constraint
        ; pvb_expr
        ; pvb_ret_modes
        ; pvb_attributes
        ; pvb_post_doc
        ; pvb_loc
        ; pvb_tokens
        } acc
        ->
      let acc = self#list self#string pvb_pre_text acc in
      let acc = self#option self#string pvb_pre_doc acc in
      let acc = self#ext_attribute pvb_ext_attrs acc in
      let acc = self#modes pvb_legacy_modes acc in
      let acc = self#pattern pvb_pat acc in
      let acc = self#modes pvb_modes acc in
      let acc = self#list self#function_param pvb_params acc in
      let acc = self#option self#value_constraint pvb_constraint acc in
      let acc = self#option self#expression pvb_expr acc in
      let acc = self#modes pvb_ret_modes acc in
      let acc = self#attributes pvb_attributes acc in
      let acc = self#option self#string pvb_post_doc acc in
      let acc = self#location pvb_loc acc in
      let acc = self#token_seq pvb_tokens acc in
      acc

    method module_binding : module_binding -> 'acc -> 'acc =
      fun
        { pmb_pre_text
        ; pmb_pre_doc
        ; pmb_ext_attrs
        ; pmb_name
        ; pmb_params
        ; pmb_constraint
        ; pmb_modes
        ; pmb_expr
        ; pmb_attributes
        ; pmb_post_doc
        ; pmb_loc
        ; pmb_tokens
        } acc
        ->
      let acc = self#list self#string pmb_pre_text acc in
      let acc = self#option self#string pmb_pre_doc acc in
      let acc = self#ext_attribute pmb_ext_attrs acc in
      let acc =
        (fun (a, b) acc ->
          let acc = self#loc (self#option self#string) a acc in
          let acc = self#modes b acc in
          acc)
          pmb_name
          acc
      in
      let acc = self#list self#functor_parameter pmb_params acc in
      let acc = self#option self#module_type pmb_constraint acc in
      let acc = self#modes pmb_modes acc in
      let acc = self#module_expr pmb_expr acc in
      let acc = self#attributes pmb_attributes acc in
      let acc = self#option self#string pmb_post_doc acc in
      let acc = self#location pmb_loc acc in
      let acc = self#token_seq pmb_tokens acc in
      acc

    method jkind_annotation_desc : jkind_annotation_desc -> 'acc -> 'acc =
      fun x acc ->
      match x with
      | Pjk_default -> acc
      | Pjk_abbreviation a -> self#string a acc
      | Pjk_mod (a, b) ->
        let acc = self#jkind_annotation a acc in
        let acc = self#modes b acc in
        acc
      | Pjk_with (a, b, c) ->
        let acc = self#jkind_annotation a acc in
        let acc = self#core_type b acc in
        let acc = self#modalities c acc in
        acc
      | Pjk_kind_of a -> self#core_type a acc
      | Pjk_product a -> self#list self#jkind_annotation a acc
      | Pjk_parens a -> self#jkind_annotation_desc a acc

    method jkind_annotation : jkind_annotation -> 'acc -> 'acc =
      fun { pjkind_loc; pjkind_desc; pjkind_tokens } acc ->
      let acc = self#location pjkind_loc acc in
      let acc = self#jkind_annotation_desc pjkind_desc acc in
      let acc = self#token_seq pjkind_tokens acc in
      acc

    method use_file : use_file -> 'acc -> 'acc =
      fun (a, b) acc ->
      let acc = self#list self#toplevel_phrase a acc in
      let acc = self#token_seq b acc in
      acc

    method toplevel_phrase : toplevel_phrase -> 'acc -> 'acc =
      fun x acc ->
      match x with
      | Ptop_def a -> self#structure a acc
      | Ptop_dir a -> self#toplevel_directive a acc
      | Ptop_lex a -> self#lexer_directive a acc

    method toplevel_directive : toplevel_directive -> 'acc -> 'acc =
      fun { pdir_name; pdir_arg; pdir_loc; pdir_tokens } acc ->
      let acc = self#loc self#string pdir_name acc in
      let acc = self#option self#directive_argument pdir_arg acc in
      let acc = self#location pdir_loc acc in
      let acc = self#token_seq pdir_tokens acc in
      acc

    method directive_argument : directive_argument -> 'acc -> 'acc =
      fun { pdira_desc; pdira_loc } acc ->
      let acc = self#directive_argument_desc pdira_desc acc in
      let acc = self#location pdira_loc acc in
      acc

    method directive_argument_desc : directive_argument_desc -> 'acc -> 'acc =
      fun x acc ->
      match x with
      | Pdir_string a -> self#string a acc
      | Pdir_int (a, b) ->
        let acc = self#string a acc in
        let acc = self#option self#char b acc in
        acc
      | Pdir_ident a -> self#longident a acc
      | Pdir_bool a -> self#bool a acc

    method syntax_directive : syntax_directive -> 'acc -> 'acc =
      fun { psyn_mode; psyn_toggle } acc ->
      let acc = self#loc self#string psyn_mode acc in
      let acc = self#bool psyn_toggle acc in
      acc

    method lexer_directive_desc : lexer_directive_desc -> 'acc -> 'acc =
      fun x acc ->
      match x with
      | Plex_syntax a -> self#syntax_directive a acc

    method lexer_directive : lexer_directive -> 'acc -> 'acc =
      fun { plex_desc; plex_loc; plex_tokens } acc ->
      let acc = self#lexer_directive_desc plex_desc acc in
      let acc = self#location plex_loc acc in
      let acc = self#token_seq plex_tokens acc in
      acc
  end

class virtual ['acc] fold_map =
  object(self)
    method virtual bool : bool -> 'acc -> bool * 'acc

    method virtual char : char -> 'acc -> char * 'acc

    method virtual int : int -> 'acc -> int * 'acc

    method
    virtual
    list
    :
    'a. ('a -> 'acc -> 'a * 'acc) -> 'a list -> 'acc -> 'a list * 'acc

    method
    virtual
    option
    :
    'a. ('a -> 'acc -> 'a * 'acc) -> 'a option -> 'acc -> 'a option * 'acc

    method
    virtual
    ref
    :
    'a. ('a -> 'acc -> 'a * 'acc) -> 'a ref -> 'acc -> 'a ref * 'acc

    method virtual string : string -> 'acc -> string * 'acc

    method virtual token : token -> 'acc -> token * 'acc

    method position : position -> 'acc -> position * 'acc =
      fun { pos_fname; pos_lnum; pos_bol; pos_cnum } acc ->
      let pos_fname, acc = self#string pos_fname acc in
      let pos_lnum, acc = self#int pos_lnum acc in
      let pos_bol, acc = self#int pos_bol acc in
      let pos_cnum, acc = self#int pos_cnum acc in
      { pos_fname; pos_lnum; pos_bol; pos_cnum }, acc

    method location : location -> 'acc -> location * 'acc =
      fun { loc_start; loc_end; loc_ghost } acc ->
      let loc_start, acc = self#position loc_start acc in
      let loc_end, acc = self#position loc_end acc in
      let loc_ghost, acc = self#bool loc_ghost acc in
      { loc_start; loc_end; loc_ghost }, acc

    method longident_dotop_delims
      : longident_dotop_delims -> 'acc -> longident_dotop_delims * 'acc
      =
      fun x acc -> x, acc

    method longident_str_or_op
      : longident_str_or_op -> 'acc -> longident_str_or_op * 'acc
      =
      fun x acc ->
      match x with
      | Str a ->
        let a, acc = self#string a acc in
        Str a, acc
      | Str_trailing_hash a ->
        let a, acc = self#string a acc in
        Str_trailing_hash a, acc
      | Op a ->
        let a, acc = self#string a acc in
        Op a, acc
      | DotOp (a, b, c, d) ->
        let a, acc = self#string a acc in
        let b, acc = self#longident_dotop_delims b acc in
        let c, acc = self#string c acc in
        let d, acc = self#bool d acc in
        DotOp (a, b, c, d), acc

    method longident_lid_desc
      : longident_lid_desc -> 'acc -> longident_lid_desc * 'acc
      =
      fun x acc ->
      match x with
      | Lident a ->
        let a, acc = self#longident_str_or_op a acc in
        Lident a, acc
      | Ldot (a, b) ->
        let a, acc = self#longident a acc in
        let b, acc = self#longident_str_or_op b acc in
        Ldot (a, b), acc
      | Lapply (a, b) ->
        let a, acc = self#longident a acc in
        let b, acc = self#longident b acc in
        Lapply (a, b), acc

    method longident : longident -> 'acc -> longident * 'acc =
      fun { desc; tokens } acc ->
      let desc, acc = self#longident_lid_desc desc acc in
      let tokens, acc = self#token_seq tokens acc in
      { desc; tokens }, acc

    method attachment : attachment -> 'acc -> attachment * 'acc =
      fun x acc -> x, acc

    method comment : comment -> 'acc -> comment * 'acc =
      fun { text; attachement; explicitely_inserted } acc ->
      let text, acc = self#string text acc in
      let attachement, acc = self#attachment attachement acc in
      let explicitely_inserted, acc =
        self#ref self#bool explicitely_inserted acc
      in
      { text; attachement; explicitely_inserted }, acc

    method token_desc : token_desc -> 'acc -> token_desc * 'acc =
      fun x acc ->
      match x with
      | Token (a, b) ->
        let a, acc = self#token a acc in
        let b, acc = self#bool b acc in
        Token (a, b), acc
      | Comment a ->
        let a, acc = self#comment a acc in
        Comment a, acc
      | Child_node -> Child_node, acc

    method token_elt : token_elt -> 'acc -> token_elt * 'acc =
      fun { desc; pos } acc ->
      let desc, acc = self#token_desc desc acc in
      let pos, acc = self#position pos acc in
      { desc; pos }, acc

    method token_seq : token_seq -> 'acc -> token_seq * 'acc =
      self#list self#token_elt

    method rec_flag : rec_flag -> 'acc -> rec_flag * 'acc = fun x acc -> x, acc

    method direction_flag : direction_flag -> 'acc -> direction_flag * 'acc =
      fun x acc -> x, acc

    method private_flag : private_flag -> 'acc -> private_flag * 'acc =
      fun x acc -> x, acc

    method mutable_flag : mutable_flag -> 'acc -> mutable_flag * 'acc =
      fun x acc -> x, acc

    method virtual_flag : virtual_flag -> 'acc -> virtual_flag * 'acc =
      fun x acc -> x, acc

    method override_flag : override_flag -> 'acc -> override_flag * 'acc =
      fun x acc -> x, acc

    method closed_flag : closed_flag -> 'acc -> closed_flag * 'acc =
      fun x acc -> x, acc

    method label : label -> 'acc -> label * 'acc = self#string

    method arg_label : arg_label -> 'acc -> arg_label * 'acc =
      fun x acc ->
      match x with
      | Nolabel -> Nolabel, acc
      | Labelled a ->
        let a, acc = self#string a acc in
        Labelled a, acc
      | Optional a ->
        let a, acc = self#string a acc in
        Optional a, acc

    method loc
      : 'a. ('a -> 'acc -> 'a * 'acc) -> 'a loc -> 'acc -> 'a loc * 'acc
      =
      fun _a { txt; loc } acc ->
      let txt, acc = _a txt acc in
      let loc, acc = self#location loc acc in
      { txt; loc }, acc

    method variance : variance -> 'acc -> variance * 'acc = fun x acc -> x, acc

    method injectivity : injectivity -> 'acc -> injectivity * 'acc =
      fun x acc -> x, acc

    method index_kind : index_kind -> 'acc -> index_kind * 'acc =
      fun x acc -> x, acc

    method paren_kind : paren_kind -> 'acc -> paren_kind * 'acc =
      fun x acc -> x, acc

    method constant : constant -> 'acc -> constant * 'acc =
      fun x acc ->
      match x with
      | Pconst_integer (a, b, c) ->
        let a, acc = self#option self#string a acc in
        let b, acc = self#string b acc in
        let c, acc = self#option self#char c acc in
        Pconst_integer (a, b, c), acc
      | Pconst_unboxed_integer (a, b, c) ->
        let a, acc = self#option self#string a acc in
        let b, acc = self#string b acc in
        let c, acc = self#char c acc in
        Pconst_unboxed_integer (a, b, c), acc
      | Pconst_char (a, b) ->
        let a, acc = self#char a acc in
        let b, acc = self#string b acc in
        Pconst_char (a, b), acc
      | Pconst_untagged_char (a, b) ->
        let a, acc = self#char a acc in
        let b, acc = self#string b acc in
        Pconst_untagged_char (a, b), acc
      | Pconst_string (a, b, c) ->
        let a, acc = self#string a acc in
        let b, acc = self#location b acc in
        let c, acc = self#option self#string c acc in
        Pconst_string (a, b, c), acc
      | Pconst_float (a, b, c) ->
        let a, acc = self#option self#string a acc in
        let b, acc = self#string b acc in
        let c, acc = self#option self#char c acc in
        Pconst_float (a, b, c), acc
      | Pconst_unboxed_float (a, b, c) ->
        let a, acc = self#option self#string a acc in
        let b, acc = self#string b acc in
        let c, acc = self#option self#char c acc in
        Pconst_unboxed_float (a, b, c), acc

    method modality : modality -> 'acc -> modality * 'acc =
      fun x acc ->
      match x with
      | Modality a ->
        let a, acc = self#string a acc in
        Modality a, acc

    method modalities : modalities -> 'acc -> modalities * 'acc =
      self#list (self#loc self#modality)

    method mode : mode -> 'acc -> mode * 'acc =
      fun x acc ->
      match x with
      | Mode a ->
        let a, acc = self#string a acc in
        Mode a, acc

    method modes : modes -> 'acc -> modes * 'acc =
      self#list (self#loc self#mode)

    method include_kind : include_kind -> 'acc -> include_kind * 'acc =
      fun x acc -> x, acc

    method attribute : attribute -> 'acc -> attribute * 'acc =
      fun { attr_name; attr_payload; attr_loc; attr_tokens } acc ->
      let attr_name, acc = self#loc (self#list self#string) attr_name acc in
      let attr_payload, acc = self#payload attr_payload acc in
      let attr_loc, acc = self#location attr_loc acc in
      let attr_tokens, acc = self#token_seq attr_tokens acc in
      { attr_name; attr_payload; attr_loc; attr_tokens }, acc

    method extension : extension -> 'acc -> extension * 'acc =
      fun (a, b, c) acc ->
      let a, acc = self#loc (self#list self#string) a acc in
      let b, acc = self#payload b acc in
      let c, acc = self#token_seq c acc in
      (a, b, c), acc

    method toplevel_extension
      : toplevel_extension -> 'acc -> toplevel_extension * 'acc
      =
      fun { te_pre_doc; te_ext; te_attrs; te_post_doc } acc ->
      let te_pre_doc, acc = self#option self#string te_pre_doc acc in
      let te_ext, acc = self#extension te_ext acc in
      let te_attrs, acc = self#attributes te_attrs acc in
      let te_post_doc, acc = self#option self#string te_post_doc acc in
      { te_pre_doc; te_ext; te_attrs; te_post_doc }, acc

    method attributes : attributes -> 'acc -> attributes * 'acc =
      self#list self#attribute

    method payload : payload -> 'acc -> payload * 'acc =
      fun x acc ->
      match x with
      | PStr a ->
        let a, acc = self#structure a acc in
        PStr a, acc
      | PSig a ->
        let a, acc = self#signature a acc in
        PSig a, acc
      | PTyp a ->
        let a, acc = self#core_type a acc in
        PTyp a, acc
      | PPat (a, b) ->
        let a, acc = self#pattern a acc in
        let b, acc = self#option self#expression b acc in
        PPat (a, b), acc
      | PString (a, b) ->
        let a, acc = self#string a acc in
        let b, acc = self#string b acc in
        PString (a, b), acc

    method ext_attribute : ext_attribute -> 'acc -> ext_attribute * 'acc =
      fun { pea_ext; pea_attrs } acc ->
      let pea_ext, acc =
        self#option (self#loc (self#list self#string)) pea_ext acc
      in
      let pea_attrs, acc = self#attributes pea_attrs acc in
      { pea_ext; pea_attrs }, acc

    method core_type : core_type -> 'acc -> core_type * 'acc =
      fun { ptyp_desc; ptyp_loc; ptyp_attributes; ptyp_tokens } acc ->
      let ptyp_desc, acc = self#core_type_desc ptyp_desc acc in
      let ptyp_loc, acc = self#location ptyp_loc acc in
      let ptyp_attributes, acc = self#attributes ptyp_attributes acc in
      let ptyp_tokens, acc = self#token_seq ptyp_tokens acc in
      { ptyp_desc; ptyp_loc; ptyp_attributes; ptyp_tokens }, acc

    method arrow_arg : arrow_arg -> 'acc -> arrow_arg * 'acc =
      fun
        { aa_lbl
        ; aa_legacy_modes
        ; aa_type
        ; aa_modes
        ; aa_doc
        ; aa_loc
        ; aa_tokens
        } acc
        ->
      let aa_lbl, acc = self#arg_label aa_lbl acc in
      let aa_legacy_modes, acc = self#modes aa_legacy_modes acc in
      let aa_type, acc = self#core_type aa_type acc in
      let aa_modes, acc = self#modes aa_modes acc in
      let aa_doc, acc = self#option self#string aa_doc acc in
      let aa_loc, acc = self#location aa_loc acc in
      let aa_tokens, acc = self#token_seq aa_tokens acc in
      ( { aa_lbl; aa_legacy_modes; aa_type; aa_modes; aa_doc; aa_loc; aa_tokens }
      , acc )

    method core_type_desc : core_type_desc -> 'acc -> core_type_desc * 'acc =
      fun x acc ->
      match x with
      | Ptyp_any a ->
        let a, acc = self#option self#jkind_annotation a acc in
        Ptyp_any a, acc
      | Ptyp_var (a, b) ->
        let a, acc = self#string a acc in
        let b, acc = self#option self#jkind_annotation b acc in
        Ptyp_var (a, b), acc
      | Ptyp_arrow { domain; codom_legacy_modes; codom_type; codom_modes } ->
        let domain, acc = self#arrow_arg domain acc in
        let codom_legacy_modes, acc = self#modes codom_legacy_modes acc in
        let codom_type, acc = self#core_type codom_type acc in
        let codom_modes, acc = self#modes codom_modes acc in
        Ptyp_arrow { domain; codom_legacy_modes; codom_type; codom_modes }, acc
      | Ptyp_tuple a ->
        let a, acc =
          self
          #
          list
            (fun (a, b) acc ->
              let a, acc = self#option self#string a acc in
              let b, acc = self#core_type b acc in
              (a, b), acc)
            a
            acc
        in
        Ptyp_tuple a, acc
      | Ptyp_unboxed_tuple a ->
        let a, acc =
          self
          #
          list
            (fun (a, b) acc ->
              let a, acc = self#option self#string a acc in
              let b, acc = self#core_type b acc in
              (a, b), acc)
            a
            acc
        in
        Ptyp_unboxed_tuple a, acc
      | Ptyp_constr (a, b) ->
        let a, acc = self#list self#core_type a acc in
        let b, acc = self#loc self#longident b acc in
        Ptyp_constr (a, b), acc
      | Ptyp_object (a, b) ->
        let a, acc = self#list self#object_field a acc in
        let b, acc = self#closed_flag b acc in
        Ptyp_object (a, b), acc
      | Ptyp_class (a, b) ->
        let a, acc = self#loc self#longident a acc in
        let b, acc = self#list self#core_type b acc in
        Ptyp_class (a, b), acc
      | Ptyp_alias (a, b, c) ->
        let a, acc = self#core_type a acc in
        let b, acc = self#option (self#loc self#string) b acc in
        let c, acc = self#option self#jkind_annotation c acc in
        Ptyp_alias (a, b, c), acc
      | Ptyp_variant (a, b, c) ->
        let a, acc = self#list self#row_field a acc in
        let b, acc = self#closed_flag b acc in
        let c, acc = self#option (self#list self#label) c acc in
        Ptyp_variant (a, b, c), acc
      | Ptyp_poly (a, b) ->
        let a, acc =
          self
          #
          list
            (fun (a, b) acc ->
              let a, acc = self#loc self#string a acc in
              let b, acc = self#option self#jkind_annotation b acc in
              (a, b), acc)
            a
            acc
        in
        let b, acc = self#core_type b acc in
        Ptyp_poly (a, b), acc
      | Ptyp_package (a, b) ->
        let a, acc = self#ext_attribute a acc in
        let b, acc = self#package_type b acc in
        Ptyp_package (a, b), acc
      | Ptyp_open (a, b) ->
        let a, acc = self#loc self#longident a acc in
        let b, acc = self#core_type b acc in
        Ptyp_open (a, b), acc
      | Ptyp_quote a ->
        let a, acc = self#core_type a acc in
        Ptyp_quote a, acc
      | Ptyp_splice a ->
        let a, acc = self#core_type a acc in
        Ptyp_splice a, acc
      | Ptyp_of_kind a ->
        let a, acc = self#jkind_annotation a acc in
        Ptyp_of_kind a, acc
      | Ptyp_extension a ->
        let a, acc = self#extension a acc in
        Ptyp_extension a, acc
      | Ptyp_parens a ->
        let a, acc = self#core_type a acc in
        Ptyp_parens a, acc

    method package_type : package_type -> 'acc -> package_type * 'acc =
      self
      #
      module_type

    method row_field : row_field -> 'acc -> row_field * 'acc =
      fun { prf_desc; prf_loc; prf_attributes; prf_doc; prf_tokens } acc ->
      let prf_desc, acc = self#row_field_desc prf_desc acc in
      let prf_loc, acc = self#location prf_loc acc in
      let prf_attributes, acc = self#attributes prf_attributes acc in
      let prf_doc, acc = self#option self#string prf_doc acc in
      let prf_tokens, acc = self#token_seq prf_tokens acc in
      { prf_desc; prf_loc; prf_attributes; prf_doc; prf_tokens }, acc

    method row_field_desc : row_field_desc -> 'acc -> row_field_desc * 'acc =
      fun x acc ->
      match x with
      | Rtag (a, b, c) ->
        let a, acc = self#loc self#string a acc in
        let b, acc = self#bool b acc in
        let c, acc = self#list self#core_type c acc in
        Rtag (a, b, c), acc
      | Rinherit a ->
        let a, acc = self#core_type a acc in
        Rinherit a, acc

    method object_field : object_field -> 'acc -> object_field * 'acc =
      fun { pof_desc; pof_loc; pof_attributes; pof_doc; pof_tokens } acc ->
      let pof_desc, acc = self#object_field_desc pof_desc acc in
      let pof_loc, acc = self#location pof_loc acc in
      let pof_attributes, acc = self#attributes pof_attributes acc in
      let pof_doc, acc = self#option self#string pof_doc acc in
      let pof_tokens, acc = self#token_seq pof_tokens acc in
      { pof_desc; pof_loc; pof_attributes; pof_doc; pof_tokens }, acc

    method object_field_desc
      : object_field_desc -> 'acc -> object_field_desc * 'acc
      =
      fun x acc ->
      match x with
      | Otag (a, b) ->
        let a, acc = self#loc self#string a acc in
        let b, acc = self#core_type b acc in
        Otag (a, b), acc
      | Oinherit a ->
        let a, acc = self#core_type a acc in
        Oinherit a, acc

    method pattern : pattern -> 'acc -> pattern * 'acc =
      fun
        { ppat_ext_attr; ppat_desc; ppat_loc; ppat_attributes; ppat_tokens } acc
        ->
      let ppat_ext_attr, acc = self#ext_attribute ppat_ext_attr acc in
      let ppat_desc, acc = self#pattern_desc ppat_desc acc in
      let ppat_loc, acc = self#location ppat_loc acc in
      let ppat_attributes, acc = self#attributes ppat_attributes acc in
      let ppat_tokens, acc = self#token_seq ppat_tokens acc in
      { ppat_ext_attr; ppat_desc; ppat_loc; ppat_attributes; ppat_tokens }, acc

    method pattern_desc : pattern_desc -> 'acc -> pattern_desc * 'acc =
      fun x acc ->
      match x with
      | Ppat_any -> Ppat_any, acc
      | Ppat_var a ->
        let a, acc = self#loc self#longident_str_or_op a acc in
        Ppat_var a, acc
      | Ppat_alias (a, b) ->
        let a, acc = self#pattern a acc in
        let b, acc = self#loc self#longident_str_or_op b acc in
        Ppat_alias (a, b), acc
      | Ppat_constant a ->
        let a, acc = self#constant a acc in
        Ppat_constant a, acc
      | Ppat_interval (a, b) ->
        let a, acc = self#constant a acc in
        let b, acc = self#constant b acc in
        Ppat_interval (a, b), acc
      | Ppat_tuple (a, b) ->
        let a, acc = self#list (self#argument self#pattern) a acc in
        let b, acc = self#closed_flag b acc in
        Ppat_tuple (a, b), acc
      | Ppat_unboxed_tuple (a, b) ->
        let a, acc = self#list (self#argument self#pattern) a acc in
        let b, acc = self#closed_flag b acc in
        Ppat_unboxed_tuple (a, b), acc
      | Ppat_construct (a, b) ->
        let a, acc = self#loc self#longident a acc in
        let b, acc =
          self
          #
          option
            (fun (a, b) acc ->
              let a, acc =
                self
                #
                list
                  (fun (a, b) acc ->
                    let a, acc = self#loc self#string a acc in
                    let b, acc = self#option self#jkind_annotation b acc in
                    (a, b), acc)
                  a
                  acc
              in
              let b, acc = self#pattern b acc in
              (a, b), acc)
            b
            acc
        in
        Ppat_construct (a, b), acc
      | Ppat_variant (a, b) ->
        let a, acc = self#label a acc in
        let b, acc = self#option self#pattern b acc in
        Ppat_variant (a, b), acc
      | Ppat_record (a, b) ->
        let a, acc = self#list (self#record_field self#pattern) a acc in
        let b, acc = self#closed_flag b acc in
        Ppat_record (a, b), acc
      | Ppat_record_unboxed_product (a, b) ->
        let a, acc = self#list (self#record_field self#pattern) a acc in
        let b, acc = self#closed_flag b acc in
        Ppat_record_unboxed_product (a, b), acc
      | Ppat_array (a, b) ->
        let a, acc = self#mutable_flag a acc in
        let b, acc = self#list self#pattern b acc in
        Ppat_array (a, b), acc
      | Ppat_or (a, b) ->
        let a, acc = self#pattern a acc in
        let b, acc = self#pattern b acc in
        Ppat_or (a, b), acc
      | Ppat_constraint (a, b, c) ->
        let a, acc = self#pattern a acc in
        let b, acc = self#option self#core_type b acc in
        let c, acc = self#modes c acc in
        Ppat_constraint (a, b, c), acc
      | Ppat_type a ->
        let a, acc = self#loc self#longident a acc in
        Ppat_type a, acc
      | Ppat_lazy a ->
        let a, acc = self#pattern a acc in
        Ppat_lazy a, acc
      | Ppat_unpack (a, b) ->
        let a, acc = self#loc (self#option self#string) a acc in
        let b, acc = self#option self#package_type b acc in
        Ppat_unpack (a, b), acc
      | Ppat_exception a ->
        let a, acc = self#pattern a acc in
        Ppat_exception a, acc
      | Ppat_extension a ->
        let a, acc = self#extension a acc in
        Ppat_extension a, acc
      | Ppat_open (a, b) ->
        let a, acc = self#loc self#longident a acc in
        let b, acc = self#pattern b acc in
        Ppat_open (a, b), acc
      | Ppat_parens { pat; optional } ->
        let pat, acc = self#pattern pat acc in
        let optional, acc = self#bool optional acc in
        Ppat_parens { pat; optional }, acc
      | Ppat_list a ->
        let a, acc = self#list self#pattern a acc in
        Ppat_list a, acc
      | Ppat_cons (a, b) ->
        let a, acc = self#pattern a acc in
        let b, acc = self#pattern b acc in
        Ppat_cons (a, b), acc

    method expression : expression -> 'acc -> expression * 'acc =
      fun
        { pexp_ext_attr; pexp_desc; pexp_loc; pexp_attributes; pexp_tokens } acc
        ->
      let pexp_ext_attr, acc = self#ext_attribute pexp_ext_attr acc in
      let pexp_desc, acc = self#expression_desc pexp_desc acc in
      let pexp_loc, acc = self#location pexp_loc acc in
      let pexp_attributes, acc = self#attributes pexp_attributes acc in
      let pexp_tokens, acc = self#token_seq pexp_tokens acc in
      { pexp_ext_attr; pexp_desc; pexp_loc; pexp_attributes; pexp_tokens }, acc

    method expression_desc : expression_desc -> 'acc -> expression_desc * 'acc =
      fun x acc ->
      match x with
      | Pexp_ident a ->
        let a, acc = self#loc self#longident a acc in
        Pexp_ident a, acc
      | Pexp_constant a ->
        let a, acc = self#constant a acc in
        Pexp_constant a, acc
      | Pexp_let (a, b, c, d) ->
        let a, acc = self#mutable_flag a acc in
        let b, acc = self#rec_flag b acc in
        let c, acc = self#list self#value_binding c acc in
        let d, acc = self#expression d acc in
        Pexp_let (a, b, c, d), acc
      | Pexp_function (a, b, c) ->
        let a, acc = self#list self#function_param a acc in
        let b, acc = self#function_constraint b acc in
        let c, acc = self#function_body c acc in
        Pexp_function (a, b, c), acc
      | Pexp_prefix_apply (a, b) ->
        let a, acc = self#expression a acc in
        let b, acc = self#expression b acc in
        Pexp_prefix_apply (a, b), acc
      | Pexp_add_or_sub (a, b) ->
        let a, acc = self#string a acc in
        let b, acc = self#expression b acc in
        Pexp_add_or_sub (a, b), acc
      | Pexp_infix_apply { arg1; op; arg2 } ->
        let arg1, acc = self#expression arg1 acc in
        let op, acc = self#expression op acc in
        let arg2, acc = self#expression arg2 acc in
        Pexp_infix_apply { arg1; op; arg2 }, acc
      | Pexp_apply (a, b) ->
        let a, acc = self#expression a acc in
        let b, acc = self#list (self#argument self#expression) b acc in
        Pexp_apply (a, b), acc
      | Pexp_match (a, b) ->
        let a, acc = self#expression a acc in
        let b, acc = self#list self#case b acc in
        Pexp_match (a, b), acc
      | Pexp_try (a, b) ->
        let a, acc = self#expression a acc in
        let b, acc = self#list self#case b acc in
        Pexp_try (a, b), acc
      | Pexp_tuple a ->
        let a, acc = self#list (self#argument self#expression) a acc in
        Pexp_tuple a, acc
      | Pexp_unboxed_tuple a ->
        let a, acc = self#list (self#argument self#expression) a acc in
        Pexp_unboxed_tuple a, acc
      | Pexp_construct (a, b) ->
        let a, acc = self#loc self#longident a acc in
        let b, acc = self#option self#expression b acc in
        Pexp_construct (a, b), acc
      | Pexp_variant (a, b) ->
        let a, acc = self#label a acc in
        let b, acc = self#option self#expression b acc in
        Pexp_variant (a, b), acc
      | Pexp_record (a, b) ->
        let a, acc = self#option self#expression a acc in
        let b, acc = self#list (self#record_field self#expression) b acc in
        Pexp_record (a, b), acc
      | Pexp_record_unboxed_product (a, b) ->
        let a, acc = self#option self#expression a acc in
        let b, acc = self#list (self#record_field self#expression) b acc in
        Pexp_record_unboxed_product (a, b), acc
      | Pexp_field (a, b) ->
        let a, acc = self#expression a acc in
        let b, acc = self#loc self#longident b acc in
        Pexp_field (a, b), acc
      | Pexp_unboxed_field (a, b) ->
        let a, acc = self#expression a acc in
        let b, acc = self#loc self#longident b acc in
        Pexp_unboxed_field (a, b), acc
      | Pexp_setfield (a, b, c) ->
        let a, acc = self#expression a acc in
        let b, acc = self#loc self#longident b acc in
        let c, acc = self#expression c acc in
        Pexp_setfield (a, b, c), acc
      | Pexp_array (a, b) ->
        let a, acc = self#mutable_flag a acc in
        let b, acc = self#list self#expression b acc in
        Pexp_array (a, b), acc
      | Pexp_idx (a, b) ->
        let a, acc = self#block_access a acc in
        let b, acc = self#list self#unboxed_access b acc in
        Pexp_idx (a, b), acc
      | Pexp_ifthenelse (a, b, c) ->
        let a, acc = self#expression a acc in
        let b, acc = self#expression b acc in
        let c, acc = self#option self#expression c acc in
        Pexp_ifthenelse (a, b, c), acc
      | Pexp_sequence (a, b) ->
        let a, acc = self#expression a acc in
        let b, acc = self#expression b acc in
        Pexp_sequence (a, b), acc
      | Pexp_seq_empty a ->
        let a, acc = self#expression a acc in
        Pexp_seq_empty a, acc
      | Pexp_while (a, b) ->
        let a, acc = self#expression a acc in
        let b, acc = self#expression b acc in
        Pexp_while (a, b), acc
      | Pexp_for (a, b, c, d, e) ->
        let a, acc = self#pattern a acc in
        let b, acc = self#expression b acc in
        let c, acc = self#expression c acc in
        let d, acc = self#direction_flag d acc in
        let e, acc = self#expression e acc in
        Pexp_for (a, b, c, d, e), acc
      | Pexp_constraint (a, b, c) ->
        let a, acc = self#expression a acc in
        let b, acc = self#option self#core_type b acc in
        let c, acc = self#modes c acc in
        Pexp_constraint (a, b, c), acc
      | Pexp_coerce (a, b, c) ->
        let a, acc = self#expression a acc in
        let b, acc = self#option self#core_type b acc in
        let c, acc = self#core_type c acc in
        Pexp_coerce (a, b, c), acc
      | Pexp_send (a, b) ->
        let a, acc = self#expression a acc in
        let b, acc = self#loc self#string b acc in
        Pexp_send (a, b), acc
      | Pexp_new a ->
        let a, acc = self#loc self#longident a acc in
        Pexp_new a, acc
      | Pexp_setvar (a, b) ->
        let a, acc = self#loc self#string a acc in
        let b, acc = self#expression b acc in
        Pexp_setvar (a, b), acc
      | Pexp_override a ->
        let a, acc =
          self
          #
          list
            (fun (a, b) acc ->
              let a, acc = self#loc self#string a acc in
              let b, acc = self#option self#expression b acc in
              (a, b), acc)
            a
            acc
        in
        Pexp_override a, acc
      | Pexp_letmodule (a, b) ->
        let a, acc = self#module_binding a acc in
        let b, acc = self#expression b acc in
        Pexp_letmodule (a, b), acc
      | Pexp_letexception (a, b) ->
        let a, acc = self#extension_constructor a acc in
        let b, acc = self#expression b acc in
        Pexp_letexception (a, b), acc
      | Pexp_assert a ->
        let a, acc = self#expression a acc in
        Pexp_assert a, acc
      | Pexp_lazy a ->
        let a, acc = self#expression a acc in
        Pexp_lazy a, acc
      | Pexp_object a ->
        let a, acc = self#class_structure a acc in
        Pexp_object a, acc
      | Pexp_pack (a, b) ->
        let a, acc = self#module_expr a acc in
        let b, acc = self#option self#package_type b acc in
        Pexp_pack (a, b), acc
      | Pexp_dot_open (a, b) ->
        let a, acc = self#loc self#longident a acc in
        let b, acc = self#expression b acc in
        Pexp_dot_open (a, b), acc
      | Pexp_let_open (a, b) ->
        let a, acc = self#open_declaration a acc in
        let b, acc = self#expression b acc in
        Pexp_let_open (a, b), acc
      | Pexp_letop a ->
        let a, acc = self#letop a acc in
        Pexp_letop a, acc
      | Pexp_extension a ->
        let a, acc = self#extension a acc in
        Pexp_extension a, acc
      | Pexp_unreachable -> Pexp_unreachable, acc
      | Pexp_stack a ->
        let a, acc = self#expression a acc in
        Pexp_stack a, acc
      | Pexp_comprehension a ->
        let a, acc = self#comprehension_expression a acc in
        Pexp_comprehension a, acc
      | Pexp_overwrite (a, b) ->
        let a, acc = self#expression a acc in
        let b, acc = self#expression b acc in
        Pexp_overwrite (a, b), acc
      | Pexp_quote a ->
        let a, acc = self#expression a acc in
        Pexp_quote a, acc
      | Pexp_splice a ->
        let a, acc = self#expression a acc in
        Pexp_splice a, acc
      | Pexp_hole -> Pexp_hole, acc
      | Pexp_index_op { kind; op; seq; indices; assign } ->
        let kind, acc = self#paren_kind kind acc in
        let op, acc =
          self
          #
          option
            (fun (a, b) acc ->
              let a, acc = self#option self#longident a acc in
              let b, acc = self#string b acc in
              (a, b), acc)
            op
            acc
        in
        let seq, acc = self#expression seq acc in
        let indices, acc = self#list self#expression indices acc in
        let assign, acc = self#option self#expression assign acc in
        Pexp_index_op { kind; op; seq; indices; assign }, acc
      | Pexp_parens { exp; optional } ->
        let exp, acc = self#expression exp acc in
        let optional, acc = self#bool optional acc in
        Pexp_parens { exp; optional }, acc
      | Pexp_begin_end a ->
        let a, acc = self#option self#expression a acc in
        Pexp_begin_end a, acc
      | Pexp_list a ->
        let a, acc = self#list self#expression a acc in
        Pexp_list a, acc
      | Pexp_cons (a, b) ->
        let a, acc = self#expression a acc in
        let b, acc = self#expression b acc in
        Pexp_cons (a, b), acc
      | Pexp_exclave a ->
        let a, acc = self#expression a acc in
        Pexp_exclave a, acc
      | Pexp_mode_legacy (a, b) ->
        let a, acc = self#loc self#mode a acc in
        let b, acc = self#expression b acc in
        Pexp_mode_legacy (a, b), acc

    method record_field
      :
        'a
        .  ('a -> 'acc -> 'a * 'acc)
        -> 'a record_field
        -> 'acc
        -> 'a record_field * 'acc
      =
      fun _a { field_name; typ; value } acc ->
      let field_name, acc = self#loc self#longident field_name acc in
      let typ, acc = self#option self#type_constraint typ acc in
      let value, acc = self#option _a value acc in
      { field_name; typ; value }, acc

    method case : case -> 'acc -> case * 'acc =
      fun { pc_lhs; pc_guard; pc_rhs; pc_tokens } acc ->
      let pc_lhs, acc = self#pattern pc_lhs acc in
      let pc_guard, acc = self#option self#expression pc_guard acc in
      let pc_rhs, acc = self#expression pc_rhs acc in
      let pc_tokens, acc = self#token_seq pc_tokens acc in
      { pc_lhs; pc_guard; pc_rhs; pc_tokens }, acc

    method letop : letop -> 'acc -> letop * 'acc =
      fun { let_; ands; body } acc ->
      let let_, acc = self#binding_op let_ acc in
      let ands, acc = self#list self#binding_op ands acc in
      let body, acc = self#expression body acc in
      { let_; ands; body }, acc

    method binding_op : binding_op -> 'acc -> binding_op * 'acc =
      fun { pbop_op; pbop_binding; pbop_loc } acc ->
      let pbop_op, acc = self#loc self#string pbop_op acc in
      let pbop_binding, acc = self#value_binding pbop_binding acc in
      let pbop_loc, acc = self#location pbop_loc acc in
      { pbop_op; pbop_binding; pbop_loc }, acc

    method argument_desc
      :
        'a
        .  ('a -> 'acc -> 'a * 'acc)
        -> 'a argument_desc
        -> 'acc
        -> 'a argument_desc * 'acc
      =
      fun _a x acc ->
      match x with
      | Parg_unlabelled { legacy_modes; arg; typ_constraint; modes } ->
        let legacy_modes, acc = self#modes legacy_modes acc in
        let arg, acc = _a arg acc in
        let typ_constraint, acc =
          self#option self#type_constraint typ_constraint acc
        in
        let modes, acc = self#modes modes acc in
        Parg_unlabelled { legacy_modes; arg; typ_constraint; modes }, acc
      | Parg_labelled
          { optional
          ; legacy_modes
          ; name
          ; maybe_punned
          ; typ_constraint
          ; modes
          ; default
          } ->
        let optional, acc = self#bool optional acc in
        let legacy_modes, acc = self#modes legacy_modes acc in
        let name, acc = self#string name acc in
        let maybe_punned, acc = self#option _a maybe_punned acc in
        let typ_constraint, acc =
          self#option self#type_constraint typ_constraint acc
        in
        let modes, acc = self#modes modes acc in
        let default, acc = self#option self#expression default acc in
        ( Parg_labelled
            { optional
            ; legacy_modes
            ; name
            ; maybe_punned
            ; typ_constraint
            ; modes
            ; default
            }
        , acc )

    method argument
      :
        'a
        .  ('a -> 'acc -> 'a * 'acc)
        -> 'a argument
        -> 'acc
        -> 'a argument * 'acc
      =
      fun _a { parg_desc; parg_tokens } acc ->
      let parg_desc, acc = self#argument_desc _a parg_desc acc in
      let parg_tokens, acc = self#token_seq parg_tokens acc in
      { parg_desc; parg_tokens }, acc

    method function_param_desc
      : function_param_desc -> 'acc -> function_param_desc * 'acc
      =
      fun x acc ->
      match x with
      | Pparam_val a ->
        let a, acc = self#argument self#pattern a acc in
        Pparam_val a, acc
      | Pparam_newtype (a, b) ->
        let a, acc = self#loc self#string a acc in
        let b, acc = self#option self#jkind_annotation b acc in
        Pparam_newtype (a, b), acc
      | Pparam_newtypes a ->
        let a, acc =
          self
          #
          list
            (fun (a, b) acc ->
              let a, acc = self#loc self#string a acc in
              let b, acc = self#option self#jkind_annotation b acc in
              (a, b), acc)
            a
            acc
        in
        Pparam_newtypes a, acc

    method function_param : function_param -> 'acc -> function_param * 'acc =
      fun { pparam_loc; pparam_desc } acc ->
      let pparam_loc, acc = self#location pparam_loc acc in
      let pparam_desc, acc = self#function_param_desc pparam_desc acc in
      { pparam_loc; pparam_desc }, acc

    method function_body : function_body -> 'acc -> function_body * 'acc =
      fun { pfb_desc; pfb_loc; pfb_tokens } acc ->
      let pfb_desc, acc = self#function_body_desc pfb_desc acc in
      let pfb_loc, acc = self#location pfb_loc acc in
      let pfb_tokens, acc = self#token_seq pfb_tokens acc in
      { pfb_desc; pfb_loc; pfb_tokens }, acc

    method function_body_desc
      : function_body_desc -> 'acc -> function_body_desc * 'acc
      =
      fun x acc ->
      match x with
      | Pfunction_body a ->
        let a, acc = self#expression a acc in
        Pfunction_body a, acc
      | Pfunction_cases (a, b) ->
        let a, acc = self#list self#case a acc in
        let b, acc = self#ext_attribute b acc in
        Pfunction_cases (a, b), acc

    method type_constraint : type_constraint -> 'acc -> type_constraint * 'acc =
      fun x acc ->
      match x with
      | Pconstraint a ->
        let a, acc = self#core_type a acc in
        Pconstraint a, acc
      | Pcoerce (a, b) ->
        let a, acc = self#option self#core_type a acc in
        let b, acc = self#core_type b acc in
        Pcoerce (a, b), acc

    method function_constraint
      : function_constraint -> 'acc -> function_constraint * 'acc
      =
      fun { ret_mode_annotations; ret_type_constraint } acc ->
      let ret_mode_annotations, acc = self#modes ret_mode_annotations acc in
      let ret_type_constraint, acc =
        self#option self#type_constraint ret_type_constraint acc
      in
      { ret_mode_annotations; ret_type_constraint }, acc

    method block_access : block_access -> 'acc -> block_access * 'acc =
      fun x acc ->
      match x with
      | Baccess_field a ->
        let a, acc = self#loc self#longident a acc in
        Baccess_field a, acc
      | Baccess_array (a, b, c) ->
        let a, acc = self#mutable_flag a acc in
        let b, acc = self#index_kind b acc in
        let c, acc = self#expression c acc in
        Baccess_array (a, b, c), acc
      | Baccess_block (a, b) ->
        let a, acc = self#mutable_flag a acc in
        let b, acc = self#expression b acc in
        Baccess_block (a, b), acc

    method unboxed_access : unboxed_access -> 'acc -> unboxed_access * 'acc =
      fun x acc ->
      match x with
      | Uaccess_unboxed_field a ->
        let a, acc = self#loc self#longident a acc in
        Uaccess_unboxed_field a, acc

    method comprehension_iterator
      : comprehension_iterator -> 'acc -> comprehension_iterator * 'acc
      =
      fun x acc ->
      match x with
      | Pcomp_range { start; stop; direction } ->
        let start, acc = self#expression start acc in
        let stop, acc = self#expression stop acc in
        let direction, acc = self#direction_flag direction acc in
        Pcomp_range { start; stop; direction }, acc
      | Pcomp_in a ->
        let a, acc = self#expression a acc in
        Pcomp_in a, acc

    method comprehension_clause_binding
      :
        comprehension_clause_binding
        -> 'acc
        -> comprehension_clause_binding * 'acc
      =
      fun
        { pcomp_cb_mode
        ; pcomp_cb_pattern
        ; pcomp_cb_iterator
        ; pcomp_cb_attributes
        ; pcomp_cb_tokens
        } acc
        ->
      let pcomp_cb_mode, acc =
        self#option (self#loc self#mode) pcomp_cb_mode acc
      in
      let pcomp_cb_pattern, acc = self#pattern pcomp_cb_pattern acc in
      let pcomp_cb_iterator, acc =
        self#comprehension_iterator pcomp_cb_iterator acc
      in
      let pcomp_cb_attributes, acc = self#attributes pcomp_cb_attributes acc in
      let pcomp_cb_tokens, acc = self#token_seq pcomp_cb_tokens acc in
      ( { pcomp_cb_mode
        ; pcomp_cb_pattern
        ; pcomp_cb_iterator
        ; pcomp_cb_attributes
        ; pcomp_cb_tokens
        }
      , acc )

    method comprehension_clause
      : comprehension_clause -> 'acc -> comprehension_clause * 'acc
      =
      fun x acc ->
      match x with
      | Pcomp_for a ->
        let a, acc = self#list self#comprehension_clause_binding a acc in
        Pcomp_for a, acc
      | Pcomp_when a ->
        let a, acc = self#expression a acc in
        Pcomp_when a, acc

    method comprehension : comprehension -> 'acc -> comprehension * 'acc =
      fun { pcomp_body; pcomp_clauses; pcomp_tokens } acc ->
      let pcomp_body, acc = self#expression pcomp_body acc in
      let pcomp_clauses, acc =
        self#list self#comprehension_clause pcomp_clauses acc
      in
      let pcomp_tokens, acc = self#token_seq pcomp_tokens acc in
      { pcomp_body; pcomp_clauses; pcomp_tokens }, acc

    method comprehension_expression
      : comprehension_expression -> 'acc -> comprehension_expression * 'acc
      =
      fun x acc ->
      match x with
      | Pcomp_list_comprehension a ->
        let a, acc = self#comprehension a acc in
        Pcomp_list_comprehension a, acc
      | Pcomp_array_comprehension (a, b) ->
        let a, acc = self#mutable_flag a acc in
        let b, acc = self#comprehension b acc in
        Pcomp_array_comprehension (a, b), acc

    method value_description
      : value_description -> 'acc -> value_description * 'acc
      =
      fun
        { pval_pre_doc
        ; pval_ext_attrs
        ; pval_name
        ; pval_type
        ; pval_modalities
        ; pval_prim
        ; pval_attributes
        ; pval_post_doc
        ; pval_loc
        ; pval_tokens
        } acc
        ->
      let pval_pre_doc, acc = self#option self#string pval_pre_doc acc in
      let pval_ext_attrs, acc = self#ext_attribute pval_ext_attrs acc in
      let pval_name, acc = self#loc self#longident_str_or_op pval_name acc in
      let pval_type, acc = self#core_type pval_type acc in
      let pval_modalities, acc = self#modalities pval_modalities acc in
      let pval_prim, acc = self#list self#string pval_prim acc in
      let pval_attributes, acc = self#attributes pval_attributes acc in
      let pval_post_doc, acc = self#option self#string pval_post_doc acc in
      let pval_loc, acc = self#location pval_loc acc in
      let pval_tokens, acc = self#token_seq pval_tokens acc in
      ( { pval_pre_doc
        ; pval_ext_attrs
        ; pval_name
        ; pval_type
        ; pval_modalities
        ; pval_prim
        ; pval_attributes
        ; pval_post_doc
        ; pval_loc
        ; pval_tokens
        }
      , acc )

    method ptype_param : ptype_param -> 'acc -> ptype_param * 'acc =
      fun { ptp_typ; ptp_infos; ptp_tokens } acc ->
      let ptp_typ, acc = self#core_type ptp_typ acc in
      let ptp_infos, acc =
        (fun (a, b) acc ->
          let a, acc = self#variance a acc in
          let b, acc = self#injectivity b acc in
          (a, b), acc)
          ptp_infos
          acc
      in
      let ptp_tokens, acc = self#token_seq ptp_tokens acc in
      { ptp_typ; ptp_infos; ptp_tokens }, acc

    method ptype_params : ptype_params -> 'acc -> ptype_params * 'acc =
      self#list self#ptype_param

    method ptype_constraint
      : ptype_constraint -> 'acc -> ptype_constraint * 'acc
      =
      fun (a, b, c) acc ->
      let a, acc = self#core_type a acc in
      let b, acc = self#core_type b acc in
      let c, acc = self#location c acc in
      (a, b, c), acc

    method type_declaration
      : type_declaration -> 'acc -> type_declaration * 'acc
      =
      fun
        { ptype_pre_text
        ; ptype_pre_doc
        ; ptype_ext_attrs
        ; ptype_name
        ; ptype_params
        ; ptype_jkind_annotation
        ; ptype_private
        ; ptype_manifest
        ; ptype_kind
        ; ptype_cstrs
        ; ptype_attributes
        ; ptype_post_doc
        ; ptype_loc
        ; ptype_tokens
        } acc
        ->
      let ptype_pre_text, acc = self#list self#string ptype_pre_text acc in
      let ptype_pre_doc, acc = self#option self#string ptype_pre_doc acc in
      let ptype_ext_attrs, acc = self#ext_attribute ptype_ext_attrs acc in
      let ptype_name, acc = self#loc self#string ptype_name acc in
      let ptype_params, acc = self#ptype_params ptype_params acc in
      let ptype_jkind_annotation, acc =
        self#option self#jkind_annotation ptype_jkind_annotation acc
      in
      let ptype_private, acc = self#private_flag ptype_private acc in
      let ptype_manifest, acc =
        self#option self#core_type ptype_manifest acc
      in
      let ptype_kind, acc = self#type_kind ptype_kind acc in
      let ptype_cstrs, acc = self#list self#ptype_constraint ptype_cstrs acc in
      let ptype_attributes, acc = self#attributes ptype_attributes acc in
      let ptype_post_doc, acc = self#option self#string ptype_post_doc acc in
      let ptype_loc, acc = self#location ptype_loc acc in
      let ptype_tokens, acc = self#token_seq ptype_tokens acc in
      ( { ptype_pre_text
        ; ptype_pre_doc
        ; ptype_ext_attrs
        ; ptype_name
        ; ptype_params
        ; ptype_jkind_annotation
        ; ptype_private
        ; ptype_manifest
        ; ptype_kind
        ; ptype_cstrs
        ; ptype_attributes
        ; ptype_post_doc
        ; ptype_loc
        ; ptype_tokens
        }
      , acc )

    method type_kind : type_kind -> 'acc -> type_kind * 'acc =
      fun x acc ->
      match x with
      | Ptype_abstract -> Ptype_abstract, acc
      | Ptype_variant a ->
        let a, acc = self#list self#constructor_declaration a acc in
        Ptype_variant a, acc
      | Ptype_record a ->
        let a, acc = self#list self#label_declaration a acc in
        Ptype_record a, acc
      | Ptype_record_unboxed_product a ->
        let a, acc = self#list self#label_declaration a acc in
        Ptype_record_unboxed_product a, acc
      | Ptype_open -> Ptype_open, acc

    method label_declaration
      : label_declaration -> 'acc -> label_declaration * 'acc
      =
      fun
        { pld_name
        ; pld_mutable
        ; pld_global
        ; pld_modalities
        ; pld_type
        ; pld_loc
        ; pld_attributes
        ; pld_doc
        ; pld_tokens
        } acc
        ->
      let pld_name, acc = self#loc self#string pld_name acc in
      let pld_mutable, acc = self#mutable_flag pld_mutable acc in
      let pld_global, acc = self#bool pld_global acc in
      let pld_modalities, acc = self#modalities pld_modalities acc in
      let pld_type, acc = self#core_type pld_type acc in
      let pld_loc, acc = self#location pld_loc acc in
      let pld_attributes, acc = self#attributes pld_attributes acc in
      let pld_doc, acc = self#option self#string pld_doc acc in
      let pld_tokens, acc = self#token_seq pld_tokens acc in
      ( { pld_name
        ; pld_mutable
        ; pld_global
        ; pld_modalities
        ; pld_type
        ; pld_loc
        ; pld_attributes
        ; pld_doc
        ; pld_tokens
        }
      , acc )

    method constructor_declaration
      : constructor_declaration -> 'acc -> constructor_declaration * 'acc
      =
      fun
        { pcd_name
        ; pcd_vars
        ; pcd_args
        ; pcd_res
        ; pcd_loc
        ; pcd_attributes
        ; pcd_doc
        ; pcd_tokens
        } acc
        ->
      let pcd_name, acc = self#loc self#longident_str_or_op pcd_name acc in
      let pcd_vars, acc =
        self
        #
        list
          (fun (a, b) acc ->
            let a, acc = self#loc self#string a acc in
            let b, acc = self#option self#jkind_annotation b acc in
            (a, b), acc)
          pcd_vars
          acc
      in
      let pcd_args, acc = self#constructor_arguments pcd_args acc in
      let pcd_res, acc = self#option self#core_type pcd_res acc in
      let pcd_loc, acc = self#location pcd_loc acc in
      let pcd_attributes, acc = self#attributes pcd_attributes acc in
      let pcd_doc, acc = self#option self#string pcd_doc acc in
      let pcd_tokens, acc = self#token_seq pcd_tokens acc in
      ( { pcd_name
        ; pcd_vars
        ; pcd_args
        ; pcd_res
        ; pcd_loc
        ; pcd_attributes
        ; pcd_doc
        ; pcd_tokens
        }
      , acc )

    method constructor_argument
      : constructor_argument -> 'acc -> constructor_argument * 'acc
      =
      fun { pca_global; pca_type; pca_modalities; pca_loc } acc ->
      let pca_global, acc = self#bool pca_global acc in
      let pca_type, acc = self#core_type pca_type acc in
      let pca_modalities, acc = self#modalities pca_modalities acc in
      let pca_loc, acc = self#location pca_loc acc in
      { pca_global; pca_type; pca_modalities; pca_loc }, acc

    method constructor_arguments
      : constructor_arguments -> 'acc -> constructor_arguments * 'acc
      =
      fun x acc ->
      match x with
      | Pcstr_tuple a ->
        let a, acc = self#list self#constructor_argument a acc in
        Pcstr_tuple a, acc
      | Pcstr_record a ->
        let a, acc = self#list self#label_declaration a acc in
        Pcstr_record a, acc

    method type_extension : type_extension -> 'acc -> type_extension * 'acc =
      fun
        { ptyext_pre_doc
        ; ptyext_ext_attrs
        ; ptyext_path
        ; ptyext_params
        ; ptyext_constructors
        ; ptyext_private
        ; ptyext_loc
        ; ptyext_attributes
        ; ptyext_post_doc
        ; ptyext_tokens
        } acc
        ->
      let ptyext_pre_doc, acc = self#option self#string ptyext_pre_doc acc in
      let ptyext_ext_attrs, acc = self#ext_attribute ptyext_ext_attrs acc in
      let ptyext_path, acc = self#loc self#longident ptyext_path acc in
      let ptyext_params, acc = self#list self#ptype_param ptyext_params acc in
      let ptyext_constructors, acc =
        self#list self#extension_constructor ptyext_constructors acc
      in
      let ptyext_private, acc = self#private_flag ptyext_private acc in
      let ptyext_loc, acc = self#location ptyext_loc acc in
      let ptyext_attributes, acc = self#attributes ptyext_attributes acc in
      let ptyext_post_doc, acc = self#option self#string ptyext_post_doc acc in
      let ptyext_tokens, acc = self#token_seq ptyext_tokens acc in
      ( { ptyext_pre_doc
        ; ptyext_ext_attrs
        ; ptyext_path
        ; ptyext_params
        ; ptyext_constructors
        ; ptyext_private
        ; ptyext_loc
        ; ptyext_attributes
        ; ptyext_post_doc
        ; ptyext_tokens
        }
      , acc )

    method extension_constructor
      : extension_constructor -> 'acc -> extension_constructor * 'acc
      =
      fun
        { pext_name
        ; pext_kind
        ; pext_loc
        ; pext_attributes
        ; pext_doc
        ; pext_tokens
        } acc
        ->
      let pext_name, acc = self#loc self#longident_str_or_op pext_name acc in
      let pext_kind, acc = self#extension_constructor_kind pext_kind acc in
      let pext_loc, acc = self#location pext_loc acc in
      let pext_attributes, acc = self#attributes pext_attributes acc in
      let pext_doc, acc = self#option self#string pext_doc acc in
      let pext_tokens, acc = self#token_seq pext_tokens acc in
      ( { pext_name
        ; pext_kind
        ; pext_loc
        ; pext_attributes
        ; pext_doc
        ; pext_tokens
        }
      , acc )

    method type_exception : type_exception -> 'acc -> type_exception * 'acc =
      fun
        { ptyexn_pre_doc
        ; ptyexn_ext_attrs
        ; ptyexn_constructor
        ; ptyexn_loc
        ; ptyexn_attributes
        ; ptyexn_post_doc
        ; ptyexn_tokens
        } acc
        ->
      let ptyexn_pre_doc, acc = self#option self#string ptyexn_pre_doc acc in
      let ptyexn_ext_attrs, acc = self#ext_attribute ptyexn_ext_attrs acc in
      let ptyexn_constructor, acc =
        self#extension_constructor ptyexn_constructor acc
      in
      let ptyexn_loc, acc = self#location ptyexn_loc acc in
      let ptyexn_attributes, acc = self#attributes ptyexn_attributes acc in
      let ptyexn_post_doc, acc = self#option self#string ptyexn_post_doc acc in
      let ptyexn_tokens, acc = self#token_seq ptyexn_tokens acc in
      ( { ptyexn_pre_doc
        ; ptyexn_ext_attrs
        ; ptyexn_constructor
        ; ptyexn_loc
        ; ptyexn_attributes
        ; ptyexn_post_doc
        ; ptyexn_tokens
        }
      , acc )

    method extension_constructor_kind
      : extension_constructor_kind -> 'acc -> extension_constructor_kind * 'acc
      =
      fun x acc ->
      match x with
      | Pext_decl (a, b, c) ->
        let a, acc =
          self
          #
          list
            (fun (a, b) acc ->
              let a, acc = self#loc self#string a acc in
              let b, acc = self#option self#jkind_annotation b acc in
              (a, b), acc)
            a
            acc
        in
        let b, acc = self#constructor_arguments b acc in
        let c, acc = self#option self#core_type c acc in
        Pext_decl (a, b, c), acc
      | Pext_rebind a ->
        let a, acc = self#loc self#longident a acc in
        Pext_rebind a, acc

    method class_type : class_type -> 'acc -> class_type * 'acc =
      fun { pcty_desc; pcty_loc; pcty_attributes; pcty_tokens } acc ->
      let pcty_desc, acc = self#class_type_desc pcty_desc acc in
      let pcty_loc, acc = self#location pcty_loc acc in
      let pcty_attributes, acc = self#attributes pcty_attributes acc in
      let pcty_tokens, acc = self#token_seq pcty_tokens acc in
      { pcty_desc; pcty_loc; pcty_attributes; pcty_tokens }, acc

    method class_type_desc : class_type_desc -> 'acc -> class_type_desc * 'acc =
      fun x acc ->
      match x with
      | Pcty_constr (a, b) ->
        let a, acc = self#loc self#longident a acc in
        let b, acc = self#list self#core_type b acc in
        Pcty_constr (a, b), acc
      | Pcty_signature a ->
        let a, acc = self#class_signature a acc in
        Pcty_signature a, acc
      | Pcty_arrow (a, b) ->
        let a, acc = self#arrow_arg a acc in
        let b, acc = self#class_type b acc in
        Pcty_arrow (a, b), acc
      | Pcty_extension a ->
        let a, acc = self#extension a acc in
        Pcty_extension a, acc
      | Pcty_open (a, b) ->
        let a, acc = self#open_description a acc in
        let b, acc = self#class_type b acc in
        Pcty_open (a, b), acc

    method class_signature : class_signature -> 'acc -> class_signature * 'acc =
      fun { pcsig_self; pcsig_fields } acc ->
      let pcsig_self, acc = self#option self#core_type pcsig_self acc in
      let pcsig_fields, acc =
        self#list self#class_type_field pcsig_fields acc
      in
      { pcsig_self; pcsig_fields }, acc

    method class_type_field
      : class_type_field -> 'acc -> class_type_field * 'acc
      =
      fun
        { pctf_pre_doc
        ; pctf_desc
        ; pctf_loc
        ; pctf_attributes
        ; pctf_post_doc
        ; pctf_tokens
        } acc
        ->
      let pctf_pre_doc, acc = self#option self#string pctf_pre_doc acc in
      let pctf_desc, acc = self#class_type_field_desc pctf_desc acc in
      let pctf_loc, acc = self#location pctf_loc acc in
      let pctf_attributes, acc = self#attributes pctf_attributes acc in
      let pctf_post_doc, acc = self#option self#string pctf_post_doc acc in
      let pctf_tokens, acc = self#token_seq pctf_tokens acc in
      ( { pctf_pre_doc
        ; pctf_desc
        ; pctf_loc
        ; pctf_attributes
        ; pctf_post_doc
        ; pctf_tokens
        }
      , acc )

    method class_type_field_desc
      : class_type_field_desc -> 'acc -> class_type_field_desc * 'acc
      =
      fun x acc ->
      match x with
      | Pctf_inherit a ->
        let a, acc = self#class_type a acc in
        Pctf_inherit a, acc
      | Pctf_val a ->
        let a, acc =
          (fun (a, b, c, d) acc ->
            let a, acc = self#loc self#string a acc in
            let b, acc = self#mutable_flag b acc in
            let c, acc = self#virtual_flag c acc in
            let d, acc = self#core_type d acc in
            (a, b, c, d), acc)
            a
            acc
        in
        Pctf_val a, acc
      | Pctf_method a ->
        let a, acc =
          (fun (a, b, c, d) acc ->
            let a, acc = self#loc self#string a acc in
            let b, acc = self#private_flag b acc in
            let c, acc = self#virtual_flag c acc in
            let d, acc = self#core_type d acc in
            (a, b, c, d), acc)
            a
            acc
        in
        Pctf_method a, acc
      | Pctf_constraint a ->
        let a, acc =
          (fun (a, b) acc ->
            let a, acc = self#core_type a acc in
            let b, acc = self#core_type b acc in
            (a, b), acc)
            a
            acc
        in
        Pctf_constraint a, acc
      | Pctf_attribute a ->
        let a, acc = self#attribute a acc in
        Pctf_attribute a, acc
      | Pctf_extension a ->
        let a, acc = self#extension a acc in
        Pctf_extension a, acc
      | Pctf_docstring a ->
        let a, acc = self#string a acc in
        Pctf_docstring a, acc

    method class_infos
      :
        'a
        .  ('a -> 'acc -> 'a * 'acc)
        -> 'a class_infos
        -> 'acc
        -> 'a class_infos * 'acc
      =
      fun
        _a
        { pci_pre_text
        ; pci_pre_doc
        ; pci_virt
        ; pci_ext_attrs
        ; pci_params
        ; pci_name
        ; pci_value_params
        ; pci_constraint
        ; pci_expr
        ; pci_loc
        ; pci_attributes
        ; pci_post_doc
        ; pci_tokens
        } acc
        ->
      let pci_pre_text, acc = self#list self#string pci_pre_text acc in
      let pci_pre_doc, acc = self#option self#string pci_pre_doc acc in
      let pci_virt, acc = self#virtual_flag pci_virt acc in
      let pci_ext_attrs, acc = self#ext_attribute pci_ext_attrs acc in
      let pci_params, acc = self#list self#ptype_param pci_params acc in
      let pci_name, acc = self#loc self#string pci_name acc in
      let pci_value_params, acc =
        self#list (self#argument self#pattern) pci_value_params acc
      in
      let pci_constraint, acc =
        self#option self#class_type pci_constraint acc
      in
      let pci_expr, acc = _a pci_expr acc in
      let pci_loc, acc = self#location pci_loc acc in
      let pci_attributes, acc = self#attributes pci_attributes acc in
      let pci_post_doc, acc = self#option self#string pci_post_doc acc in
      let pci_tokens, acc = self#token_seq pci_tokens acc in
      ( { pci_pre_text
        ; pci_pre_doc
        ; pci_virt
        ; pci_ext_attrs
        ; pci_params
        ; pci_name
        ; pci_value_params
        ; pci_constraint
        ; pci_expr
        ; pci_loc
        ; pci_attributes
        ; pci_post_doc
        ; pci_tokens
        }
      , acc )

    method class_description
      : class_description -> 'acc -> class_description * 'acc
      =
      self#class_infos self#class_type

    method class_type_declaration
      : class_type_declaration -> 'acc -> class_type_declaration * 'acc
      =
      self#class_infos self#class_type

    method class_expr : class_expr -> 'acc -> class_expr * 'acc =
      fun { pcl_ext_attrs; pcl_desc; pcl_loc; pcl_attributes } acc ->
      let pcl_ext_attrs, acc = self#ext_attribute pcl_ext_attrs acc in
      let pcl_desc, acc = self#class_expr_desc pcl_desc acc in
      let pcl_loc, acc = self#location pcl_loc acc in
      let pcl_attributes, acc = self#attributes pcl_attributes acc in
      { pcl_ext_attrs; pcl_desc; pcl_loc; pcl_attributes }, acc

    method class_expr_desc : class_expr_desc -> 'acc -> class_expr_desc * 'acc =
      fun x acc ->
      match x with
      | Pcl_constr (a, b) ->
        let a, acc = self#loc self#longident a acc in
        let b, acc = self#list self#core_type b acc in
        Pcl_constr (a, b), acc
      | Pcl_structure a ->
        let a, acc = self#class_structure a acc in
        Pcl_structure a, acc
      | Pcl_fun (a, b) ->
        let a, acc = self#list (self#argument self#pattern) a acc in
        let b, acc = self#class_expr b acc in
        Pcl_fun (a, b), acc
      | Pcl_apply (a, b) ->
        let a, acc = self#class_expr a acc in
        let b, acc = self#list (self#argument self#expression) b acc in
        Pcl_apply (a, b), acc
      | Pcl_let (a, b, c) ->
        let a, acc = self#rec_flag a acc in
        let b, acc = self#list self#value_binding b acc in
        let c, acc = self#class_expr c acc in
        Pcl_let (a, b, c), acc
      | Pcl_constraint (a, b) ->
        let a, acc = self#class_expr a acc in
        let b, acc = self#class_type b acc in
        Pcl_constraint (a, b), acc
      | Pcl_extension a ->
        let a, acc = self#extension a acc in
        Pcl_extension a, acc
      | Pcl_open (a, b) ->
        let a, acc = self#open_description a acc in
        let b, acc = self#class_expr b acc in
        Pcl_open (a, b), acc
      | Pcl_parens a ->
        let a, acc = self#class_expr a acc in
        Pcl_parens a, acc

    method class_structure : class_structure -> 'acc -> class_structure * 'acc =
      fun { pcstr_self; pcstr_fields } acc ->
      let pcstr_self, acc = self#pattern pcstr_self acc in
      let pcstr_fields, acc = self#list self#class_field pcstr_fields acc in
      { pcstr_self; pcstr_fields }, acc

    method class_field : class_field -> 'acc -> class_field * 'acc =
      fun
        { pcf_pre_doc
        ; pcf_desc
        ; pcf_loc
        ; pcf_attributes
        ; pcf_post_doc
        ; pcf_tokens
        } acc
        ->
      let pcf_pre_doc, acc = self#option self#string pcf_pre_doc acc in
      let pcf_desc, acc = self#class_field_desc pcf_desc acc in
      let pcf_loc, acc = self#location pcf_loc acc in
      let pcf_attributes, acc = self#attributes pcf_attributes acc in
      let pcf_post_doc, acc = self#option self#string pcf_post_doc acc in
      let pcf_tokens, acc = self#token_seq pcf_tokens acc in
      ( { pcf_pre_doc
        ; pcf_desc
        ; pcf_loc
        ; pcf_attributes
        ; pcf_post_doc
        ; pcf_tokens
        }
      , acc )

    method class_field_desc
      : class_field_desc -> 'acc -> class_field_desc * 'acc
      =
      fun x acc ->
      match x with
      | Pcf_inherit (a, b, c) ->
        let a, acc = self#override_flag a acc in
        let b, acc = self#class_expr b acc in
        let c, acc = self#option (self#loc self#string) c acc in
        Pcf_inherit (a, b, c), acc
      | Pcf_val a ->
        let a, acc =
          (fun (a, b, c) acc ->
            let a, acc = self#loc self#string a acc in
            let b, acc = self#mutable_flag b acc in
            let c, acc = self#class_field_kind c acc in
            (a, b, c), acc)
            a
            acc
        in
        Pcf_val a, acc
      | Pcf_method a ->
        let a, acc =
          (fun (a, b, c) acc ->
            let a, acc = self#loc self#string a acc in
            let b, acc = self#private_flag b acc in
            let c, acc = self#class_field_kind c acc in
            (a, b, c), acc)
            a
            acc
        in
        Pcf_method a, acc
      | Pcf_constraint a ->
        let a, acc =
          (fun (a, b) acc ->
            let a, acc = self#core_type a acc in
            let b, acc = self#core_type b acc in
            (a, b), acc)
            a
            acc
        in
        Pcf_constraint a, acc
      | Pcf_initializer a ->
        let a, acc = self#expression a acc in
        Pcf_initializer a, acc
      | Pcf_attribute a ->
        let a, acc = self#attribute a acc in
        Pcf_attribute a, acc
      | Pcf_extension a ->
        let a, acc = self#extension a acc in
        Pcf_extension a, acc
      | Pcf_docstring a ->
        let a, acc = self#string a acc in
        Pcf_docstring a, acc

    method class_field_kind
      : class_field_kind -> 'acc -> class_field_kind * 'acc
      =
      fun x acc ->
      match x with
      | Cfk_virtual a ->
        let a, acc = self#core_type a acc in
        Cfk_virtual a, acc
      | Cfk_concrete (a, b) ->
        let a, acc = self#override_flag a acc in
        let b, acc = self#value_binding b acc in
        Cfk_concrete (a, b), acc

    method class_declaration
      : class_declaration -> 'acc -> class_declaration * 'acc
      =
      self#class_infos self#class_expr

    method module_type : module_type -> 'acc -> module_type * 'acc =
      fun { pmty_desc; pmty_loc; pmty_attributes; pmty_tokens } acc ->
      let pmty_desc, acc = self#module_type_desc pmty_desc acc in
      let pmty_loc, acc = self#location pmty_loc acc in
      let pmty_attributes, acc = self#attributes pmty_attributes acc in
      let pmty_tokens, acc = self#token_seq pmty_tokens acc in
      { pmty_desc; pmty_loc; pmty_attributes; pmty_tokens }, acc

    method module_type_desc
      : module_type_desc -> 'acc -> module_type_desc * 'acc
      =
      fun x acc ->
      match x with
      | Pmty_ident a ->
        let a, acc = self#loc self#longident a acc in
        Pmty_ident a, acc
      | Pmty_signature a ->
        let a, acc = self#signature a acc in
        Pmty_signature a, acc
      | Pmty_functor (a, b, c, d) ->
        let a, acc = self#attributes a acc in
        let b, acc = self#list self#functor_parameter b acc in
        let c, acc = self#module_type c acc in
        let d, acc = self#modes d acc in
        Pmty_functor (a, b, c, d), acc
      | Pmty_functor_type (a, b, c) ->
        let a, acc = self#list self#functor_parameter a acc in
        let b, acc = self#module_type b acc in
        let c, acc = self#modes c acc in
        Pmty_functor_type (a, b, c), acc
      | Pmty_with (a, b) ->
        let a, acc = self#module_type a acc in
        let b, acc = self#list self#with_constraint b acc in
        Pmty_with (a, b), acc
      | Pmty_typeof (a, b) ->
        let a, acc = self#attributes a acc in
        let b, acc = self#module_expr b acc in
        Pmty_typeof (a, b), acc
      | Pmty_extension a ->
        let a, acc = self#extension a acc in
        Pmty_extension a, acc
      | Pmty_alias a ->
        let a, acc = self#loc self#longident a acc in
        Pmty_alias a, acc
      | Pmty_strengthen (a, b) ->
        let a, acc = self#module_type a acc in
        let b, acc = self#loc self#longident b acc in
        Pmty_strengthen (a, b), acc
      | Pmty_parens a ->
        let a, acc = self#module_type a acc in
        Pmty_parens a, acc

    method functor_parameter
      : functor_parameter -> 'acc -> functor_parameter * 'acc
      =
      fun x acc ->
      match x with
      | Unit -> Unit, acc
      | Named (a, b, c) ->
        let a, acc = self#loc (self#option self#string) a acc in
        let b, acc = self#module_type b acc in
        let c, acc = self#modes c acc in
        Named (a, b, c), acc
      | Unnamed (a, b) ->
        let a, acc = self#module_type a acc in
        let b, acc = self#modes b acc in
        Unnamed (a, b), acc

    method signature : signature -> 'acc -> signature * 'acc =
      fun { psg_modalities; psg_items; psg_loc; psg_tokens } acc ->
      let psg_modalities, acc = self#modalities psg_modalities acc in
      let psg_items, acc = self#list self#signature_item psg_items acc in
      let psg_loc, acc = self#location psg_loc acc in
      let psg_tokens, acc = self#token_seq psg_tokens acc in
      { psg_modalities; psg_items; psg_loc; psg_tokens }, acc

    method signature_item : signature_item -> 'acc -> signature_item * 'acc =
      fun { psig_desc; psig_loc; psig_tokens } acc ->
      let psig_desc, acc = self#signature_item_desc psig_desc acc in
      let psig_loc, acc = self#location psig_loc acc in
      let psig_tokens, acc = self#token_seq psig_tokens acc in
      { psig_desc; psig_loc; psig_tokens }, acc

    method signature_item_desc
      : signature_item_desc -> 'acc -> signature_item_desc * 'acc
      =
      fun x acc ->
      match x with
      | Psig_value a ->
        let a, acc = self#value_description a acc in
        Psig_value a, acc
      | Psig_type (a, b) ->
        let a, acc = self#rec_flag a acc in
        let b, acc = self#list self#type_declaration b acc in
        Psig_type (a, b), acc
      | Psig_typesubst a ->
        let a, acc = self#list self#type_declaration a acc in
        Psig_typesubst a, acc
      | Psig_typext a ->
        let a, acc = self#type_extension a acc in
        Psig_typext a, acc
      | Psig_exception a ->
        let a, acc = self#type_exception a acc in
        Psig_exception a, acc
      | Psig_module a ->
        let a, acc = self#module_declaration a acc in
        Psig_module a, acc
      | Psig_modsubst a ->
        let a, acc = self#module_substitution a acc in
        Psig_modsubst a, acc
      | Psig_recmodule a ->
        let a, acc = self#list self#module_declaration a acc in
        Psig_recmodule a, acc
      | Psig_modtype a ->
        let a, acc = self#module_type_declaration a acc in
        Psig_modtype a, acc
      | Psig_modtypesubst a ->
        let a, acc = self#module_type_declaration a acc in
        Psig_modtypesubst a, acc
      | Psig_open a ->
        let a, acc = self#open_description a acc in
        Psig_open a, acc
      | Psig_include (a, b) ->
        let a, acc = self#include_description a acc in
        let b, acc = self#modalities b acc in
        Psig_include (a, b), acc
      | Psig_class a ->
        let a, acc = self#list self#class_description a acc in
        Psig_class a, acc
      | Psig_class_type a ->
        let a, acc = self#list self#class_type_declaration a acc in
        Psig_class_type a, acc
      | Psig_attribute a ->
        let a, acc = self#attribute a acc in
        Psig_attribute a, acc
      | Psig_extension a ->
        let a, acc = self#toplevel_extension a acc in
        Psig_extension a, acc
      | Psig_kind_abbrev (a, b) ->
        let a, acc = self#loc self#string a acc in
        let b, acc = self#jkind_annotation b acc in
        Psig_kind_abbrev (a, b), acc
      | Psig_docstring a ->
        let a, acc = self#string a acc in
        Psig_docstring a, acc

    method module_declaration_body
      : module_declaration_body -> 'acc -> module_declaration_body * 'acc
      =
      fun x acc ->
      match x with
      | With_params (a, b, c) ->
        let a, acc = self#list self#functor_parameter a acc in
        let b, acc = self#module_type b acc in
        let c, acc = self#modes c acc in
        With_params (a, b, c), acc
      | Without_params (a, b) ->
        let a, acc = self#module_type a acc in
        let b, acc = self#modalities b acc in
        Without_params (a, b), acc

    method module_declaration
      : module_declaration -> 'acc -> module_declaration * 'acc
      =
      fun
        { pmd_pre_text
        ; pmd_pre_doc
        ; pmd_ext_attrs
        ; pmd_name
        ; pmd_body
        ; pmd_attributes
        ; pmd_post_doc
        ; pmd_loc
        ; pmd_tokens
        } acc
        ->
      let pmd_pre_text, acc = self#list self#string pmd_pre_text acc in
      let pmd_pre_doc, acc = self#option self#string pmd_pre_doc acc in
      let pmd_ext_attrs, acc = self#ext_attribute pmd_ext_attrs acc in
      let pmd_name, acc =
        (fun (a, b) acc ->
          let a, acc = self#loc (self#option self#string) a acc in
          let b, acc = self#modalities b acc in
          (a, b), acc)
          pmd_name
          acc
      in
      let pmd_body, acc = self#module_declaration_body pmd_body acc in
      let pmd_attributes, acc = self#attributes pmd_attributes acc in
      let pmd_post_doc, acc = self#option self#string pmd_post_doc acc in
      let pmd_loc, acc = self#location pmd_loc acc in
      let pmd_tokens, acc = self#token_seq pmd_tokens acc in
      ( { pmd_pre_text
        ; pmd_pre_doc
        ; pmd_ext_attrs
        ; pmd_name
        ; pmd_body
        ; pmd_attributes
        ; pmd_post_doc
        ; pmd_loc
        ; pmd_tokens
        }
      , acc )

    method module_substitution
      : module_substitution -> 'acc -> module_substitution * 'acc
      =
      fun
        { pms_pre_doc
        ; pms_ext_attrs
        ; pms_name
        ; pms_manifest
        ; pms_attributes
        ; pms_post_doc
        ; pms_loc
        ; pms_tokens
        } acc
        ->
      let pms_pre_doc, acc = self#option self#string pms_pre_doc acc in
      let pms_ext_attrs, acc = self#ext_attribute pms_ext_attrs acc in
      let pms_name, acc = self#loc self#string pms_name acc in
      let pms_manifest, acc = self#loc self#longident pms_manifest acc in
      let pms_attributes, acc = self#attributes pms_attributes acc in
      let pms_post_doc, acc = self#option self#string pms_post_doc acc in
      let pms_loc, acc = self#location pms_loc acc in
      let pms_tokens, acc = self#token_seq pms_tokens acc in
      ( { pms_pre_doc
        ; pms_ext_attrs
        ; pms_name
        ; pms_manifest
        ; pms_attributes
        ; pms_post_doc
        ; pms_loc
        ; pms_tokens
        }
      , acc )

    method module_type_declaration
      : module_type_declaration -> 'acc -> module_type_declaration * 'acc
      =
      fun
        { pmtd_pre_doc
        ; pmtd_ext_attrs
        ; pmtd_name
        ; pmtd_type
        ; pmtd_attributes
        ; pmtd_post_doc
        ; pmtd_loc
        ; pmtd_tokens
        } acc
        ->
      let pmtd_pre_doc, acc = self#option self#string pmtd_pre_doc acc in
      let pmtd_ext_attrs, acc = self#ext_attribute pmtd_ext_attrs acc in
      let pmtd_name, acc = self#loc self#string pmtd_name acc in
      let pmtd_type, acc = self#option self#module_type pmtd_type acc in
      let pmtd_attributes, acc = self#attributes pmtd_attributes acc in
      let pmtd_post_doc, acc = self#option self#string pmtd_post_doc acc in
      let pmtd_loc, acc = self#location pmtd_loc acc in
      let pmtd_tokens, acc = self#token_seq pmtd_tokens acc in
      ( { pmtd_pre_doc
        ; pmtd_ext_attrs
        ; pmtd_name
        ; pmtd_type
        ; pmtd_attributes
        ; pmtd_post_doc
        ; pmtd_loc
        ; pmtd_tokens
        }
      , acc )

    method open_infos
      :
        'a
        .  ('a -> 'acc -> 'a * 'acc)
        -> 'a open_infos
        -> 'acc
        -> 'a open_infos * 'acc
      =
      fun
        _a
        { popen_pre_doc
        ; popen_ext_attrs
        ; popen_expr
        ; popen_override
        ; popen_loc
        ; popen_attributes
        ; popen_post_doc
        ; popen_tokens
        } acc
        ->
      let popen_pre_doc, acc = self#option self#string popen_pre_doc acc in
      let popen_ext_attrs, acc = self#ext_attribute popen_ext_attrs acc in
      let popen_expr, acc = _a popen_expr acc in
      let popen_override, acc = self#override_flag popen_override acc in
      let popen_loc, acc = self#location popen_loc acc in
      let popen_attributes, acc = self#attributes popen_attributes acc in
      let popen_post_doc, acc = self#option self#string popen_post_doc acc in
      let popen_tokens, acc = self#token_seq popen_tokens acc in
      ( { popen_pre_doc
        ; popen_ext_attrs
        ; popen_expr
        ; popen_override
        ; popen_loc
        ; popen_attributes
        ; popen_post_doc
        ; popen_tokens
        }
      , acc )

    method open_description
      : open_description -> 'acc -> open_description * 'acc
      =
      self#open_infos (self#loc self#longident)

    method open_declaration
      : open_declaration -> 'acc -> open_declaration * 'acc
      =
      self#open_infos self#module_expr

    method include_infos
      :
        'a
        .  ('a -> 'acc -> 'a * 'acc)
        -> 'a include_infos
        -> 'acc
        -> 'a include_infos * 'acc
      =
      fun
        _a
        { pincl_pre_doc
        ; pincl_kind
        ; pincl_ext_attrs
        ; pincl_mod
        ; pincl_loc
        ; pincl_attributes
        ; pincl_post_doc
        ; pincl_tokens
        } acc
        ->
      let pincl_pre_doc, acc = self#option self#string pincl_pre_doc acc in
      let pincl_kind, acc = self#include_kind pincl_kind acc in
      let pincl_ext_attrs, acc = self#ext_attribute pincl_ext_attrs acc in
      let pincl_mod, acc = _a pincl_mod acc in
      let pincl_loc, acc = self#location pincl_loc acc in
      let pincl_attributes, acc = self#attributes pincl_attributes acc in
      let pincl_post_doc, acc = self#option self#string pincl_post_doc acc in
      let pincl_tokens, acc = self#token_seq pincl_tokens acc in
      ( { pincl_pre_doc
        ; pincl_kind
        ; pincl_ext_attrs
        ; pincl_mod
        ; pincl_loc
        ; pincl_attributes
        ; pincl_post_doc
        ; pincl_tokens
        }
      , acc )

    method include_description
      : include_description -> 'acc -> include_description * 'acc
      =
      self#include_infos self#module_type

    method include_declaration
      : include_declaration -> 'acc -> include_declaration * 'acc
      =
      self#include_infos self#module_expr

    method with_constraint : with_constraint -> 'acc -> with_constraint * 'acc =
      fun { wc_desc; wc_loc; wc_tokens } acc ->
      let wc_desc, acc = self#with_constraint_desc wc_desc acc in
      let wc_loc, acc = self#location wc_loc acc in
      let wc_tokens, acc = self#token_seq wc_tokens acc in
      { wc_desc; wc_loc; wc_tokens }, acc

    method with_constraint_desc
      : with_constraint_desc -> 'acc -> with_constraint_desc * 'acc
      =
      fun x acc ->
      match x with
      | Pwith_type (a, b, c, d, e) ->
        let a, acc = self#ptype_params a acc in
        let b, acc = self#loc self#longident b acc in
        let c, acc = self#private_flag c acc in
        let d, acc = self#core_type d acc in
        let e, acc = self#list self#ptype_constraint e acc in
        Pwith_type (a, b, c, d, e), acc
      | Pwith_module (a, b) ->
        let a, acc = self#loc self#longident a acc in
        let b, acc = self#loc self#longident b acc in
        Pwith_module (a, b), acc
      | Pwith_modtype (a, b) ->
        let a, acc = self#loc self#longident a acc in
        let b, acc = self#module_type b acc in
        Pwith_modtype (a, b), acc
      | Pwith_modtypesubst (a, b) ->
        let a, acc = self#loc self#longident a acc in
        let b, acc = self#module_type b acc in
        Pwith_modtypesubst (a, b), acc
      | Pwith_typesubst (a, b, c) ->
        let a, acc = self#ptype_params a acc in
        let b, acc = self#loc self#longident b acc in
        let c, acc = self#core_type c acc in
        Pwith_typesubst (a, b, c), acc
      | Pwith_modsubst (a, b) ->
        let a, acc = self#loc self#longident a acc in
        let b, acc = self#loc self#longident b acc in
        Pwith_modsubst (a, b), acc

    method module_expr : module_expr -> 'acc -> module_expr * 'acc =
      fun { pmod_desc; pmod_loc; pmod_attributes; pmod_tokens } acc ->
      let pmod_desc, acc = self#module_expr_desc pmod_desc acc in
      let pmod_loc, acc = self#location pmod_loc acc in
      let pmod_attributes, acc = self#attributes pmod_attributes acc in
      let pmod_tokens, acc = self#token_seq pmod_tokens acc in
      { pmod_desc; pmod_loc; pmod_attributes; pmod_tokens }, acc

    method module_expr_desc
      : module_expr_desc -> 'acc -> module_expr_desc * 'acc
      =
      fun x acc ->
      match x with
      | Pmod_ident a ->
        let a, acc = self#loc self#longident a acc in
        Pmod_ident a, acc
      | Pmod_structure (a, b) ->
        let a, acc = self#attributes a acc in
        let b, acc = self#structure b acc in
        Pmod_structure (a, b), acc
      | Pmod_functor (a, b, c) ->
        let a, acc = self#attributes a acc in
        let b, acc = self#list self#functor_parameter b acc in
        let c, acc = self#module_expr c acc in
        Pmod_functor (a, b, c), acc
      | Pmod_apply (a, b) ->
        let a, acc = self#module_expr a acc in
        let b, acc = self#module_expr b acc in
        Pmod_apply (a, b), acc
      | Pmod_apply_unit a ->
        let a, acc = self#module_expr a acc in
        Pmod_apply_unit a, acc
      | Pmod_constraint (a, b, c) ->
        let a, acc = self#module_expr a acc in
        let b, acc = self#option self#module_type b acc in
        let c, acc = self#modes c acc in
        Pmod_constraint (a, b, c), acc
      | Pmod_unpack (a, b, c) ->
        let a, acc = self#expression a acc in
        let b, acc = self#option self#package_type b acc in
        let c, acc = self#option self#package_type c acc in
        Pmod_unpack (a, b, c), acc
      | Pmod_extension a ->
        let a, acc = self#extension a acc in
        Pmod_extension a, acc
      | Pmod_parens a ->
        let a, acc = self#module_expr a acc in
        Pmod_parens a, acc

    method structure : structure -> 'acc -> structure * 'acc =
      fun (a, b) acc ->
      let a, acc = self#list self#structure_item a acc in
      let b, acc = self#token_seq b acc in
      (a, b), acc

    method structure_item : structure_item -> 'acc -> structure_item * 'acc =
      fun { pstr_desc; pstr_loc; pstr_tokens } acc ->
      let pstr_desc, acc = self#structure_item_desc pstr_desc acc in
      let pstr_loc, acc = self#location pstr_loc acc in
      let pstr_tokens, acc = self#token_seq pstr_tokens acc in
      { pstr_desc; pstr_loc; pstr_tokens }, acc

    method structure_item_desc
      : structure_item_desc -> 'acc -> structure_item_desc * 'acc
      =
      fun x acc ->
      match x with
      | Pstr_eval (a, b) ->
        let a, acc = self#expression a acc in
        let b, acc = self#attributes b acc in
        Pstr_eval (a, b), acc
      | Pstr_value (a, b) ->
        let a, acc = self#rec_flag a acc in
        let b, acc = self#list self#value_binding b acc in
        Pstr_value (a, b), acc
      | Pstr_primitive a ->
        let a, acc = self#value_description a acc in
        Pstr_primitive a, acc
      | Pstr_type (a, b) ->
        let a, acc = self#rec_flag a acc in
        let b, acc = self#list self#type_declaration b acc in
        Pstr_type (a, b), acc
      | Pstr_typext a ->
        let a, acc = self#type_extension a acc in
        Pstr_typext a, acc
      | Pstr_exception a ->
        let a, acc = self#type_exception a acc in
        Pstr_exception a, acc
      | Pstr_module a ->
        let a, acc = self#module_binding a acc in
        Pstr_module a, acc
      | Pstr_recmodule a ->
        let a, acc = self#list self#module_binding a acc in
        Pstr_recmodule a, acc
      | Pstr_modtype a ->
        let a, acc = self#module_type_declaration a acc in
        Pstr_modtype a, acc
      | Pstr_open a ->
        let a, acc = self#open_declaration a acc in
        Pstr_open a, acc
      | Pstr_class a ->
        let a, acc = self#list self#class_declaration a acc in
        Pstr_class a, acc
      | Pstr_class_type a ->
        let a, acc = self#list self#class_type_declaration a acc in
        Pstr_class_type a, acc
      | Pstr_include a ->
        let a, acc = self#include_declaration a acc in
        Pstr_include a, acc
      | Pstr_attribute a ->
        let a, acc = self#attribute a acc in
        Pstr_attribute a, acc
      | Pstr_extension a ->
        let a, acc = self#toplevel_extension a acc in
        Pstr_extension a, acc
      | Pstr_kind_abbrev (a, b) ->
        let a, acc = self#loc self#string a acc in
        let b, acc = self#jkind_annotation b acc in
        Pstr_kind_abbrev (a, b), acc
      | Pstr_docstring a ->
        let a, acc = self#string a acc in
        Pstr_docstring a, acc

    method value_constraint
      : value_constraint -> 'acc -> value_constraint * 'acc
      =
      fun x acc ->
      match x with
      | Pvc_constraint { locally_abstract_univars; typ } ->
        let locally_abstract_univars, acc =
          self
          #
          list
            (fun (a, b) acc ->
              let a, acc = self#loc self#string a acc in
              let b, acc = self#option self#jkind_annotation b acc in
              (a, b), acc)
            locally_abstract_univars
            acc
        in
        let typ, acc = self#core_type typ acc in
        Pvc_constraint { locally_abstract_univars; typ }, acc
      | Pvc_coercion { ground; coercion } ->
        let ground, acc = self#option self#core_type ground acc in
        let coercion, acc = self#core_type coercion acc in
        Pvc_coercion { ground; coercion }, acc

    method value_binding : value_binding -> 'acc -> value_binding * 'acc =
      fun
        { pvb_pre_text
        ; pvb_pre_doc
        ; pvb_ext_attrs
        ; pvb_legacy_modes
        ; pvb_pat
        ; pvb_modes
        ; pvb_params
        ; pvb_constraint
        ; pvb_expr
        ; pvb_ret_modes
        ; pvb_attributes
        ; pvb_post_doc
        ; pvb_loc
        ; pvb_tokens
        } acc
        ->
      let pvb_pre_text, acc = self#list self#string pvb_pre_text acc in
      let pvb_pre_doc, acc = self#option self#string pvb_pre_doc acc in
      let pvb_ext_attrs, acc = self#ext_attribute pvb_ext_attrs acc in
      let pvb_legacy_modes, acc = self#modes pvb_legacy_modes acc in
      let pvb_pat, acc = self#pattern pvb_pat acc in
      let pvb_modes, acc = self#modes pvb_modes acc in
      let pvb_params, acc = self#list self#function_param pvb_params acc in
      let pvb_constraint, acc =
        self#option self#value_constraint pvb_constraint acc
      in
      let pvb_expr, acc = self#option self#expression pvb_expr acc in
      let pvb_ret_modes, acc = self#modes pvb_ret_modes acc in
      let pvb_attributes, acc = self#attributes pvb_attributes acc in
      let pvb_post_doc, acc = self#option self#string pvb_post_doc acc in
      let pvb_loc, acc = self#location pvb_loc acc in
      let pvb_tokens, acc = self#token_seq pvb_tokens acc in
      ( { pvb_pre_text
        ; pvb_pre_doc
        ; pvb_ext_attrs
        ; pvb_legacy_modes
        ; pvb_pat
        ; pvb_modes
        ; pvb_params
        ; pvb_constraint
        ; pvb_expr
        ; pvb_ret_modes
        ; pvb_attributes
        ; pvb_post_doc
        ; pvb_loc
        ; pvb_tokens
        }
      , acc )

    method module_binding : module_binding -> 'acc -> module_binding * 'acc =
      fun
        { pmb_pre_text
        ; pmb_pre_doc
        ; pmb_ext_attrs
        ; pmb_name
        ; pmb_params
        ; pmb_constraint
        ; pmb_modes
        ; pmb_expr
        ; pmb_attributes
        ; pmb_post_doc
        ; pmb_loc
        ; pmb_tokens
        } acc
        ->
      let pmb_pre_text, acc = self#list self#string pmb_pre_text acc in
      let pmb_pre_doc, acc = self#option self#string pmb_pre_doc acc in
      let pmb_ext_attrs, acc = self#ext_attribute pmb_ext_attrs acc in
      let pmb_name, acc =
        (fun (a, b) acc ->
          let a, acc = self#loc (self#option self#string) a acc in
          let b, acc = self#modes b acc in
          (a, b), acc)
          pmb_name
          acc
      in
      let pmb_params, acc = self#list self#functor_parameter pmb_params acc in
      let pmb_constraint, acc =
        self#option self#module_type pmb_constraint acc
      in
      let pmb_modes, acc = self#modes pmb_modes acc in
      let pmb_expr, acc = self#module_expr pmb_expr acc in
      let pmb_attributes, acc = self#attributes pmb_attributes acc in
      let pmb_post_doc, acc = self#option self#string pmb_post_doc acc in
      let pmb_loc, acc = self#location pmb_loc acc in
      let pmb_tokens, acc = self#token_seq pmb_tokens acc in
      ( { pmb_pre_text
        ; pmb_pre_doc
        ; pmb_ext_attrs
        ; pmb_name
        ; pmb_params
        ; pmb_constraint
        ; pmb_modes
        ; pmb_expr
        ; pmb_attributes
        ; pmb_post_doc
        ; pmb_loc
        ; pmb_tokens
        }
      , acc )

    method jkind_annotation_desc
      : jkind_annotation_desc -> 'acc -> jkind_annotation_desc * 'acc
      =
      fun x acc ->
      match x with
      | Pjk_default -> Pjk_default, acc
      | Pjk_abbreviation a ->
        let a, acc = self#string a acc in
        Pjk_abbreviation a, acc
      | Pjk_mod (a, b) ->
        let a, acc = self#jkind_annotation a acc in
        let b, acc = self#modes b acc in
        Pjk_mod (a, b), acc
      | Pjk_with (a, b, c) ->
        let a, acc = self#jkind_annotation a acc in
        let b, acc = self#core_type b acc in
        let c, acc = self#modalities c acc in
        Pjk_with (a, b, c), acc
      | Pjk_kind_of a ->
        let a, acc = self#core_type a acc in
        Pjk_kind_of a, acc
      | Pjk_product a ->
        let a, acc = self#list self#jkind_annotation a acc in
        Pjk_product a, acc
      | Pjk_parens a ->
        let a, acc = self#jkind_annotation_desc a acc in
        Pjk_parens a, acc

    method jkind_annotation
      : jkind_annotation -> 'acc -> jkind_annotation * 'acc
      =
      fun { pjkind_loc; pjkind_desc; pjkind_tokens } acc ->
      let pjkind_loc, acc = self#location pjkind_loc acc in
      let pjkind_desc, acc = self#jkind_annotation_desc pjkind_desc acc in
      let pjkind_tokens, acc = self#token_seq pjkind_tokens acc in
      { pjkind_loc; pjkind_desc; pjkind_tokens }, acc

    method use_file : use_file -> 'acc -> use_file * 'acc =
      fun (a, b) acc ->
      let a, acc = self#list self#toplevel_phrase a acc in
      let b, acc = self#token_seq b acc in
      (a, b), acc

    method toplevel_phrase : toplevel_phrase -> 'acc -> toplevel_phrase * 'acc =
      fun x acc ->
      match x with
      | Ptop_def a ->
        let a, acc = self#structure a acc in
        Ptop_def a, acc
      | Ptop_dir a ->
        let a, acc = self#toplevel_directive a acc in
        Ptop_dir a, acc
      | Ptop_lex a ->
        let a, acc = self#lexer_directive a acc in
        Ptop_lex a, acc

    method toplevel_directive
      : toplevel_directive -> 'acc -> toplevel_directive * 'acc
      =
      fun { pdir_name; pdir_arg; pdir_loc; pdir_tokens } acc ->
      let pdir_name, acc = self#loc self#string pdir_name acc in
      let pdir_arg, acc = self#option self#directive_argument pdir_arg acc in
      let pdir_loc, acc = self#location pdir_loc acc in
      let pdir_tokens, acc = self#token_seq pdir_tokens acc in
      { pdir_name; pdir_arg; pdir_loc; pdir_tokens }, acc

    method directive_argument
      : directive_argument -> 'acc -> directive_argument * 'acc
      =
      fun { pdira_desc; pdira_loc } acc ->
      let pdira_desc, acc = self#directive_argument_desc pdira_desc acc in
      let pdira_loc, acc = self#location pdira_loc acc in
      { pdira_desc; pdira_loc }, acc

    method directive_argument_desc
      : directive_argument_desc -> 'acc -> directive_argument_desc * 'acc
      =
      fun x acc ->
      match x with
      | Pdir_string a ->
        let a, acc = self#string a acc in
        Pdir_string a, acc
      | Pdir_int (a, b) ->
        let a, acc = self#string a acc in
        let b, acc = self#option self#char b acc in
        Pdir_int (a, b), acc
      | Pdir_ident a ->
        let a, acc = self#longident a acc in
        Pdir_ident a, acc
      | Pdir_bool a ->
        let a, acc = self#bool a acc in
        Pdir_bool a, acc

    method syntax_directive
      : syntax_directive -> 'acc -> syntax_directive * 'acc
      =
      fun { psyn_mode; psyn_toggle } acc ->
      let psyn_mode, acc = self#loc self#string psyn_mode acc in
      let psyn_toggle, acc = self#bool psyn_toggle acc in
      { psyn_mode; psyn_toggle }, acc

    method lexer_directive_desc
      : lexer_directive_desc -> 'acc -> lexer_directive_desc * 'acc
      =
      fun x acc ->
      match x with
      | Plex_syntax a ->
        let a, acc = self#syntax_directive a acc in
        Plex_syntax a, acc

    method lexer_directive : lexer_directive -> 'acc -> lexer_directive * 'acc =
      fun { plex_desc; plex_loc; plex_tokens } acc ->
      let plex_desc, acc = self#lexer_directive_desc plex_desc acc in
      let plex_loc, acc = self#location plex_loc acc in
      let plex_tokens, acc = self#token_seq plex_tokens acc in
      { plex_desc; plex_loc; plex_tokens }, acc
  end

class virtual ['ctx] map_with_context =
  object(self)
    method virtual bool : 'ctx -> bool -> bool

    method virtual char : 'ctx -> char -> char

    method virtual int : 'ctx -> int -> int

    method virtual list : 'a. ('ctx -> 'a -> 'a) -> 'ctx -> 'a list -> 'a list

    method
    virtual
    option
    :
    'a. ('ctx -> 'a -> 'a) -> 'ctx -> 'a option -> 'a option

    method virtual ref : 'a. ('ctx -> 'a -> 'a) -> 'ctx -> 'a ref -> 'a ref

    method virtual string : 'ctx -> string -> string

    method virtual token : 'ctx -> token -> token

    method position : 'ctx -> position -> position =
      fun ctx { pos_fname; pos_lnum; pos_bol; pos_cnum } ->
      let pos_fname = self#string ctx pos_fname in
      let pos_lnum = self#int ctx pos_lnum in
      let pos_bol = self#int ctx pos_bol in
      let pos_cnum = self#int ctx pos_cnum in
      { pos_fname; pos_lnum; pos_bol; pos_cnum }

    method location : 'ctx -> location -> location =
      fun ctx { loc_start; loc_end; loc_ghost } ->
      let loc_start = self#position ctx loc_start in
      let loc_end = self#position ctx loc_end in
      let loc_ghost = self#bool ctx loc_ghost in
      { loc_start; loc_end; loc_ghost }

    method longident_dotop_delims
      : 'ctx -> longident_dotop_delims -> longident_dotop_delims
      =
      fun _ctx x -> x

    method longident_str_or_op
      : 'ctx -> longident_str_or_op -> longident_str_or_op
      =
      fun ctx x ->
      match x with
      | Str a ->
        let a = self#string ctx a in
        Str a
      | Str_trailing_hash a ->
        let a = self#string ctx a in
        Str_trailing_hash a
      | Op a ->
        let a = self#string ctx a in
        Op a
      | DotOp (a, b, c, d) ->
        let a = self#string ctx a in
        let b = self#longident_dotop_delims ctx b in
        let c = self#string ctx c in
        let d = self#bool ctx d in
        DotOp (a, b, c, d)

    method longident_lid_desc : 'ctx -> longident_lid_desc -> longident_lid_desc
      =
      fun ctx x ->
      match x with
      | Lident a ->
        let a = self#longident_str_or_op ctx a in
        Lident a
      | Ldot (a, b) ->
        let a = self#longident ctx a in
        let b = self#longident_str_or_op ctx b in
        Ldot (a, b)
      | Lapply (a, b) ->
        let a = self#longident ctx a in
        let b = self#longident ctx b in
        Lapply (a, b)

    method longident : 'ctx -> longident -> longident =
      fun ctx { desc; tokens } ->
      let desc = self#longident_lid_desc ctx desc in
      let tokens = self#token_seq ctx tokens in
      { desc; tokens }

    method attachment : 'ctx -> attachment -> attachment = fun _ctx x -> x

    method comment : 'ctx -> comment -> comment =
      fun ctx { text; attachement; explicitely_inserted } ->
      let text = self#string ctx text in
      let attachement = self#attachment ctx attachement in
      let explicitely_inserted = self#ref self#bool ctx explicitely_inserted in
      { text; attachement; explicitely_inserted }

    method token_desc : 'ctx -> token_desc -> token_desc =
      fun ctx x ->
      match x with
      | Token (a, b) ->
        let a = self#token ctx a in
        let b = self#bool ctx b in
        Token (a, b)
      | Comment a ->
        let a = self#comment ctx a in
        Comment a
      | Child_node -> Child_node

    method token_elt : 'ctx -> token_elt -> token_elt =
      fun ctx { desc; pos } ->
      let desc = self#token_desc ctx desc in
      let pos = self#position ctx pos in
      { desc; pos }

    method token_seq : 'ctx -> token_seq -> token_seq = self#list self#token_elt

    method rec_flag : 'ctx -> rec_flag -> rec_flag = fun _ctx x -> x

    method direction_flag : 'ctx -> direction_flag -> direction_flag =
      fun _ctx x -> x

    method private_flag : 'ctx -> private_flag -> private_flag = fun _ctx x -> x

    method mutable_flag : 'ctx -> mutable_flag -> mutable_flag = fun _ctx x -> x

    method virtual_flag : 'ctx -> virtual_flag -> virtual_flag = fun _ctx x -> x

    method override_flag : 'ctx -> override_flag -> override_flag =
      fun _ctx x -> x

    method closed_flag : 'ctx -> closed_flag -> closed_flag = fun _ctx x -> x

    method label : 'ctx -> label -> label = self#string

    method arg_label : 'ctx -> arg_label -> arg_label =
      fun ctx x ->
      match x with
      | Nolabel -> Nolabel
      | Labelled a ->
        let a = self#string ctx a in
        Labelled a
      | Optional a ->
        let a = self#string ctx a in
        Optional a

    method loc : 'a. ('ctx -> 'a -> 'a) -> 'ctx -> 'a loc -> 'a loc =
      fun _a ctx { txt; loc } ->
      let txt = _a ctx txt in
      let loc = self#location ctx loc in
      { txt; loc }

    method variance : 'ctx -> variance -> variance = fun _ctx x -> x

    method injectivity : 'ctx -> injectivity -> injectivity = fun _ctx x -> x

    method index_kind : 'ctx -> index_kind -> index_kind = fun _ctx x -> x

    method paren_kind : 'ctx -> paren_kind -> paren_kind = fun _ctx x -> x

    method constant : 'ctx -> constant -> constant =
      fun ctx x ->
      match x with
      | Pconst_integer (a, b, c) ->
        let a = self#option self#string ctx a in
        let b = self#string ctx b in
        let c = self#option self#char ctx c in
        Pconst_integer (a, b, c)
      | Pconst_unboxed_integer (a, b, c) ->
        let a = self#option self#string ctx a in
        let b = self#string ctx b in
        let c = self#char ctx c in
        Pconst_unboxed_integer (a, b, c)
      | Pconst_char (a, b) ->
        let a = self#char ctx a in
        let b = self#string ctx b in
        Pconst_char (a, b)
      | Pconst_untagged_char (a, b) ->
        let a = self#char ctx a in
        let b = self#string ctx b in
        Pconst_untagged_char (a, b)
      | Pconst_string (a, b, c) ->
        let a = self#string ctx a in
        let b = self#location ctx b in
        let c = self#option self#string ctx c in
        Pconst_string (a, b, c)
      | Pconst_float (a, b, c) ->
        let a = self#option self#string ctx a in
        let b = self#string ctx b in
        let c = self#option self#char ctx c in
        Pconst_float (a, b, c)
      | Pconst_unboxed_float (a, b, c) ->
        let a = self#option self#string ctx a in
        let b = self#string ctx b in
        let c = self#option self#char ctx c in
        Pconst_unboxed_float (a, b, c)

    method modality : 'ctx -> modality -> modality =
      fun ctx x ->
      match x with
      | Modality a ->
        let a = self#string ctx a in
        Modality a

    method modalities : 'ctx -> modalities -> modalities =
      self#list (self#loc self#modality)

    method mode : 'ctx -> mode -> mode =
      fun ctx x ->
      match x with
      | Mode a ->
        let a = self#string ctx a in
        Mode a

    method modes : 'ctx -> modes -> modes = self#list (self#loc self#mode)

    method include_kind : 'ctx -> include_kind -> include_kind = fun _ctx x -> x

    method attribute : 'ctx -> attribute -> attribute =
      fun ctx { attr_name; attr_payload; attr_loc; attr_tokens } ->
      let attr_name = self#loc (self#list self#string) ctx attr_name in
      let attr_payload = self#payload ctx attr_payload in
      let attr_loc = self#location ctx attr_loc in
      let attr_tokens = self#token_seq ctx attr_tokens in
      { attr_name; attr_payload; attr_loc; attr_tokens }

    method extension : 'ctx -> extension -> extension =
      fun ctx (a, b, c) ->
      let a = self#loc (self#list self#string) ctx a in
      let b = self#payload ctx b in
      let c = self#token_seq ctx c in
      a, b, c

    method toplevel_extension : 'ctx -> toplevel_extension -> toplevel_extension
      =
      fun ctx { te_pre_doc; te_ext; te_attrs; te_post_doc } ->
      let te_pre_doc = self#option self#string ctx te_pre_doc in
      let te_ext = self#extension ctx te_ext in
      let te_attrs = self#attributes ctx te_attrs in
      let te_post_doc = self#option self#string ctx te_post_doc in
      { te_pre_doc; te_ext; te_attrs; te_post_doc }

    method attributes : 'ctx -> attributes -> attributes =
      self#list self#attribute

    method payload : 'ctx -> payload -> payload =
      fun ctx x ->
      match x with
      | PStr a ->
        let a = self#structure ctx a in
        PStr a
      | PSig a ->
        let a = self#signature ctx a in
        PSig a
      | PTyp a ->
        let a = self#core_type ctx a in
        PTyp a
      | PPat (a, b) ->
        let a = self#pattern ctx a in
        let b = self#option self#expression ctx b in
        PPat (a, b)
      | PString (a, b) ->
        let a = self#string ctx a in
        let b = self#string ctx b in
        PString (a, b)

    method ext_attribute : 'ctx -> ext_attribute -> ext_attribute =
      fun ctx { pea_ext; pea_attrs } ->
      let pea_ext =
        self#option (self#loc (self#list self#string)) ctx pea_ext
      in
      let pea_attrs = self#attributes ctx pea_attrs in
      { pea_ext; pea_attrs }

    method core_type : 'ctx -> core_type -> core_type =
      fun ctx { ptyp_desc; ptyp_loc; ptyp_attributes; ptyp_tokens } ->
      let ptyp_desc = self#core_type_desc ctx ptyp_desc in
      let ptyp_loc = self#location ctx ptyp_loc in
      let ptyp_attributes = self#attributes ctx ptyp_attributes in
      let ptyp_tokens = self#token_seq ctx ptyp_tokens in
      { ptyp_desc; ptyp_loc; ptyp_attributes; ptyp_tokens }

    method arrow_arg : 'ctx -> arrow_arg -> arrow_arg =
      fun
        ctx
        { aa_lbl
        ; aa_legacy_modes
        ; aa_type
        ; aa_modes
        ; aa_doc
        ; aa_loc
        ; aa_tokens
        }
        ->
      let aa_lbl = self#arg_label ctx aa_lbl in
      let aa_legacy_modes = self#modes ctx aa_legacy_modes in
      let aa_type = self#core_type ctx aa_type in
      let aa_modes = self#modes ctx aa_modes in
      let aa_doc = self#option self#string ctx aa_doc in
      let aa_loc = self#location ctx aa_loc in
      let aa_tokens = self#token_seq ctx aa_tokens in
      { aa_lbl; aa_legacy_modes; aa_type; aa_modes; aa_doc; aa_loc; aa_tokens }

    method core_type_desc : 'ctx -> core_type_desc -> core_type_desc =
      fun ctx x ->
      match x with
      | Ptyp_any a ->
        let a = self#option self#jkind_annotation ctx a in
        Ptyp_any a
      | Ptyp_var (a, b) ->
        let a = self#string ctx a in
        let b = self#option self#jkind_annotation ctx b in
        Ptyp_var (a, b)
      | Ptyp_arrow { domain; codom_legacy_modes; codom_type; codom_modes } ->
        let domain = self#arrow_arg ctx domain in
        let codom_legacy_modes = self#modes ctx codom_legacy_modes in
        let codom_type = self#core_type ctx codom_type in
        let codom_modes = self#modes ctx codom_modes in
        Ptyp_arrow { domain; codom_legacy_modes; codom_type; codom_modes }
      | Ptyp_tuple a ->
        let a =
          self
          #
          list
            (fun ctx (a, b) ->
              let a = self#option self#string ctx a in
              let b = self#core_type ctx b in
              a, b)
            ctx
            a
        in
        Ptyp_tuple a
      | Ptyp_unboxed_tuple a ->
        let a =
          self
          #
          list
            (fun ctx (a, b) ->
              let a = self#option self#string ctx a in
              let b = self#core_type ctx b in
              a, b)
            ctx
            a
        in
        Ptyp_unboxed_tuple a
      | Ptyp_constr (a, b) ->
        let a = self#list self#core_type ctx a in
        let b = self#loc self#longident ctx b in
        Ptyp_constr (a, b)
      | Ptyp_object (a, b) ->
        let a = self#list self#object_field ctx a in
        let b = self#closed_flag ctx b in
        Ptyp_object (a, b)
      | Ptyp_class (a, b) ->
        let a = self#loc self#longident ctx a in
        let b = self#list self#core_type ctx b in
        Ptyp_class (a, b)
      | Ptyp_alias (a, b, c) ->
        let a = self#core_type ctx a in
        let b = self#option (self#loc self#string) ctx b in
        let c = self#option self#jkind_annotation ctx c in
        Ptyp_alias (a, b, c)
      | Ptyp_variant (a, b, c) ->
        let a = self#list self#row_field ctx a in
        let b = self#closed_flag ctx b in
        let c = self#option (self#list self#label) ctx c in
        Ptyp_variant (a, b, c)
      | Ptyp_poly (a, b) ->
        let a =
          self
          #
          list
            (fun ctx (a, b) ->
              let a = self#loc self#string ctx a in
              let b = self#option self#jkind_annotation ctx b in
              a, b)
            ctx
            a
        in
        let b = self#core_type ctx b in
        Ptyp_poly (a, b)
      | Ptyp_package (a, b) ->
        let a = self#ext_attribute ctx a in
        let b = self#package_type ctx b in
        Ptyp_package (a, b)
      | Ptyp_open (a, b) ->
        let a = self#loc self#longident ctx a in
        let b = self#core_type ctx b in
        Ptyp_open (a, b)
      | Ptyp_quote a ->
        let a = self#core_type ctx a in
        Ptyp_quote a
      | Ptyp_splice a ->
        let a = self#core_type ctx a in
        Ptyp_splice a
      | Ptyp_of_kind a ->
        let a = self#jkind_annotation ctx a in
        Ptyp_of_kind a
      | Ptyp_extension a ->
        let a = self#extension ctx a in
        Ptyp_extension a
      | Ptyp_parens a ->
        let a = self#core_type ctx a in
        Ptyp_parens a

    method package_type : 'ctx -> package_type -> package_type =
      self
      #
      module_type

    method row_field : 'ctx -> row_field -> row_field =
      fun ctx { prf_desc; prf_loc; prf_attributes; prf_doc; prf_tokens } ->
      let prf_desc = self#row_field_desc ctx prf_desc in
      let prf_loc = self#location ctx prf_loc in
      let prf_attributes = self#attributes ctx prf_attributes in
      let prf_doc = self#option self#string ctx prf_doc in
      let prf_tokens = self#token_seq ctx prf_tokens in
      { prf_desc; prf_loc; prf_attributes; prf_doc; prf_tokens }

    method row_field_desc : 'ctx -> row_field_desc -> row_field_desc =
      fun ctx x ->
      match x with
      | Rtag (a, b, c) ->
        let a = self#loc self#string ctx a in
        let b = self#bool ctx b in
        let c = self#list self#core_type ctx c in
        Rtag (a, b, c)
      | Rinherit a ->
        let a = self#core_type ctx a in
        Rinherit a

    method object_field : 'ctx -> object_field -> object_field =
      fun ctx { pof_desc; pof_loc; pof_attributes; pof_doc; pof_tokens } ->
      let pof_desc = self#object_field_desc ctx pof_desc in
      let pof_loc = self#location ctx pof_loc in
      let pof_attributes = self#attributes ctx pof_attributes in
      let pof_doc = self#option self#string ctx pof_doc in
      let pof_tokens = self#token_seq ctx pof_tokens in
      { pof_desc; pof_loc; pof_attributes; pof_doc; pof_tokens }

    method object_field_desc : 'ctx -> object_field_desc -> object_field_desc =
      fun ctx x ->
      match x with
      | Otag (a, b) ->
        let a = self#loc self#string ctx a in
        let b = self#core_type ctx b in
        Otag (a, b)
      | Oinherit a ->
        let a = self#core_type ctx a in
        Oinherit a

    method pattern : 'ctx -> pattern -> pattern =
      fun
        ctx { ppat_ext_attr; ppat_desc; ppat_loc; ppat_attributes; ppat_tokens }
        ->
      let ppat_ext_attr = self#ext_attribute ctx ppat_ext_attr in
      let ppat_desc = self#pattern_desc ctx ppat_desc in
      let ppat_loc = self#location ctx ppat_loc in
      let ppat_attributes = self#attributes ctx ppat_attributes in
      let ppat_tokens = self#token_seq ctx ppat_tokens in
      { ppat_ext_attr; ppat_desc; ppat_loc; ppat_attributes; ppat_tokens }

    method pattern_desc : 'ctx -> pattern_desc -> pattern_desc =
      fun ctx x ->
      match x with
      | Ppat_any -> Ppat_any
      | Ppat_var a ->
        let a = self#loc self#longident_str_or_op ctx a in
        Ppat_var a
      | Ppat_alias (a, b) ->
        let a = self#pattern ctx a in
        let b = self#loc self#longident_str_or_op ctx b in
        Ppat_alias (a, b)
      | Ppat_constant a ->
        let a = self#constant ctx a in
        Ppat_constant a
      | Ppat_interval (a, b) ->
        let a = self#constant ctx a in
        let b = self#constant ctx b in
        Ppat_interval (a, b)
      | Ppat_tuple (a, b) ->
        let a = self#list (self#argument self#pattern) ctx a in
        let b = self#closed_flag ctx b in
        Ppat_tuple (a, b)
      | Ppat_unboxed_tuple (a, b) ->
        let a = self#list (self#argument self#pattern) ctx a in
        let b = self#closed_flag ctx b in
        Ppat_unboxed_tuple (a, b)
      | Ppat_construct (a, b) ->
        let a = self#loc self#longident ctx a in
        let b =
          self
          #
          option
            (fun ctx (a, b) ->
              let a =
                self
                #
                list
                  (fun ctx (a, b) ->
                    let a = self#loc self#string ctx a in
                    let b = self#option self#jkind_annotation ctx b in
                    a, b)
                  ctx
                  a
              in
              let b = self#pattern ctx b in
              a, b)
            ctx
            b
        in
        Ppat_construct (a, b)
      | Ppat_variant (a, b) ->
        let a = self#label ctx a in
        let b = self#option self#pattern ctx b in
        Ppat_variant (a, b)
      | Ppat_record (a, b) ->
        let a = self#list (self#record_field self#pattern) ctx a in
        let b = self#closed_flag ctx b in
        Ppat_record (a, b)
      | Ppat_record_unboxed_product (a, b) ->
        let a = self#list (self#record_field self#pattern) ctx a in
        let b = self#closed_flag ctx b in
        Ppat_record_unboxed_product (a, b)
      | Ppat_array (a, b) ->
        let a = self#mutable_flag ctx a in
        let b = self#list self#pattern ctx b in
        Ppat_array (a, b)
      | Ppat_or (a, b) ->
        let a = self#pattern ctx a in
        let b = self#pattern ctx b in
        Ppat_or (a, b)
      | Ppat_constraint (a, b, c) ->
        let a = self#pattern ctx a in
        let b = self#option self#core_type ctx b in
        let c = self#modes ctx c in
        Ppat_constraint (a, b, c)
      | Ppat_type a ->
        let a = self#loc self#longident ctx a in
        Ppat_type a
      | Ppat_lazy a ->
        let a = self#pattern ctx a in
        Ppat_lazy a
      | Ppat_unpack (a, b) ->
        let a = self#loc (self#option self#string) ctx a in
        let b = self#option self#package_type ctx b in
        Ppat_unpack (a, b)
      | Ppat_exception a ->
        let a = self#pattern ctx a in
        Ppat_exception a
      | Ppat_extension a ->
        let a = self#extension ctx a in
        Ppat_extension a
      | Ppat_open (a, b) ->
        let a = self#loc self#longident ctx a in
        let b = self#pattern ctx b in
        Ppat_open (a, b)
      | Ppat_parens { pat; optional } ->
        let pat = self#pattern ctx pat in
        let optional = self#bool ctx optional in
        Ppat_parens { pat; optional }
      | Ppat_list a ->
        let a = self#list self#pattern ctx a in
        Ppat_list a
      | Ppat_cons (a, b) ->
        let a = self#pattern ctx a in
        let b = self#pattern ctx b in
        Ppat_cons (a, b)

    method expression : 'ctx -> expression -> expression =
      fun
        ctx { pexp_ext_attr; pexp_desc; pexp_loc; pexp_attributes; pexp_tokens }
        ->
      let pexp_ext_attr = self#ext_attribute ctx pexp_ext_attr in
      let pexp_desc = self#expression_desc ctx pexp_desc in
      let pexp_loc = self#location ctx pexp_loc in
      let pexp_attributes = self#attributes ctx pexp_attributes in
      let pexp_tokens = self#token_seq ctx pexp_tokens in
      { pexp_ext_attr; pexp_desc; pexp_loc; pexp_attributes; pexp_tokens }

    method expression_desc : 'ctx -> expression_desc -> expression_desc =
      fun ctx x ->
      match x with
      | Pexp_ident a ->
        let a = self#loc self#longident ctx a in
        Pexp_ident a
      | Pexp_constant a ->
        let a = self#constant ctx a in
        Pexp_constant a
      | Pexp_let (a, b, c, d) ->
        let a = self#mutable_flag ctx a in
        let b = self#rec_flag ctx b in
        let c = self#list self#value_binding ctx c in
        let d = self#expression ctx d in
        Pexp_let (a, b, c, d)
      | Pexp_function (a, b, c) ->
        let a = self#list self#function_param ctx a in
        let b = self#function_constraint ctx b in
        let c = self#function_body ctx c in
        Pexp_function (a, b, c)
      | Pexp_prefix_apply (a, b) ->
        let a = self#expression ctx a in
        let b = self#expression ctx b in
        Pexp_prefix_apply (a, b)
      | Pexp_add_or_sub (a, b) ->
        let a = self#string ctx a in
        let b = self#expression ctx b in
        Pexp_add_or_sub (a, b)
      | Pexp_infix_apply { arg1; op; arg2 } ->
        let arg1 = self#expression ctx arg1 in
        let op = self#expression ctx op in
        let arg2 = self#expression ctx arg2 in
        Pexp_infix_apply { arg1; op; arg2 }
      | Pexp_apply (a, b) ->
        let a = self#expression ctx a in
        let b = self#list (self#argument self#expression) ctx b in
        Pexp_apply (a, b)
      | Pexp_match (a, b) ->
        let a = self#expression ctx a in
        let b = self#list self#case ctx b in
        Pexp_match (a, b)
      | Pexp_try (a, b) ->
        let a = self#expression ctx a in
        let b = self#list self#case ctx b in
        Pexp_try (a, b)
      | Pexp_tuple a ->
        let a = self#list (self#argument self#expression) ctx a in
        Pexp_tuple a
      | Pexp_unboxed_tuple a ->
        let a = self#list (self#argument self#expression) ctx a in
        Pexp_unboxed_tuple a
      | Pexp_construct (a, b) ->
        let a = self#loc self#longident ctx a in
        let b = self#option self#expression ctx b in
        Pexp_construct (a, b)
      | Pexp_variant (a, b) ->
        let a = self#label ctx a in
        let b = self#option self#expression ctx b in
        Pexp_variant (a, b)
      | Pexp_record (a, b) ->
        let a = self#option self#expression ctx a in
        let b = self#list (self#record_field self#expression) ctx b in
        Pexp_record (a, b)
      | Pexp_record_unboxed_product (a, b) ->
        let a = self#option self#expression ctx a in
        let b = self#list (self#record_field self#expression) ctx b in
        Pexp_record_unboxed_product (a, b)
      | Pexp_field (a, b) ->
        let a = self#expression ctx a in
        let b = self#loc self#longident ctx b in
        Pexp_field (a, b)
      | Pexp_unboxed_field (a, b) ->
        let a = self#expression ctx a in
        let b = self#loc self#longident ctx b in
        Pexp_unboxed_field (a, b)
      | Pexp_setfield (a, b, c) ->
        let a = self#expression ctx a in
        let b = self#loc self#longident ctx b in
        let c = self#expression ctx c in
        Pexp_setfield (a, b, c)
      | Pexp_array (a, b) ->
        let a = self#mutable_flag ctx a in
        let b = self#list self#expression ctx b in
        Pexp_array (a, b)
      | Pexp_idx (a, b) ->
        let a = self#block_access ctx a in
        let b = self#list self#unboxed_access ctx b in
        Pexp_idx (a, b)
      | Pexp_ifthenelse (a, b, c) ->
        let a = self#expression ctx a in
        let b = self#expression ctx b in
        let c = self#option self#expression ctx c in
        Pexp_ifthenelse (a, b, c)
      | Pexp_sequence (a, b) ->
        let a = self#expression ctx a in
        let b = self#expression ctx b in
        Pexp_sequence (a, b)
      | Pexp_seq_empty a ->
        let a = self#expression ctx a in
        Pexp_seq_empty a
      | Pexp_while (a, b) ->
        let a = self#expression ctx a in
        let b = self#expression ctx b in
        Pexp_while (a, b)
      | Pexp_for (a, b, c, d, e) ->
        let a = self#pattern ctx a in
        let b = self#expression ctx b in
        let c = self#expression ctx c in
        let d = self#direction_flag ctx d in
        let e = self#expression ctx e in
        Pexp_for (a, b, c, d, e)
      | Pexp_constraint (a, b, c) ->
        let a = self#expression ctx a in
        let b = self#option self#core_type ctx b in
        let c = self#modes ctx c in
        Pexp_constraint (a, b, c)
      | Pexp_coerce (a, b, c) ->
        let a = self#expression ctx a in
        let b = self#option self#core_type ctx b in
        let c = self#core_type ctx c in
        Pexp_coerce (a, b, c)
      | Pexp_send (a, b) ->
        let a = self#expression ctx a in
        let b = self#loc self#string ctx b in
        Pexp_send (a, b)
      | Pexp_new a ->
        let a = self#loc self#longident ctx a in
        Pexp_new a
      | Pexp_setvar (a, b) ->
        let a = self#loc self#string ctx a in
        let b = self#expression ctx b in
        Pexp_setvar (a, b)
      | Pexp_override a ->
        let a =
          self
          #
          list
            (fun ctx (a, b) ->
              let a = self#loc self#string ctx a in
              let b = self#option self#expression ctx b in
              a, b)
            ctx
            a
        in
        Pexp_override a
      | Pexp_letmodule (a, b) ->
        let a = self#module_binding ctx a in
        let b = self#expression ctx b in
        Pexp_letmodule (a, b)
      | Pexp_letexception (a, b) ->
        let a = self#extension_constructor ctx a in
        let b = self#expression ctx b in
        Pexp_letexception (a, b)
      | Pexp_assert a ->
        let a = self#expression ctx a in
        Pexp_assert a
      | Pexp_lazy a ->
        let a = self#expression ctx a in
        Pexp_lazy a
      | Pexp_object a ->
        let a = self#class_structure ctx a in
        Pexp_object a
      | Pexp_pack (a, b) ->
        let a = self#module_expr ctx a in
        let b = self#option self#package_type ctx b in
        Pexp_pack (a, b)
      | Pexp_dot_open (a, b) ->
        let a = self#loc self#longident ctx a in
        let b = self#expression ctx b in
        Pexp_dot_open (a, b)
      | Pexp_let_open (a, b) ->
        let a = self#open_declaration ctx a in
        let b = self#expression ctx b in
        Pexp_let_open (a, b)
      | Pexp_letop a ->
        let a = self#letop ctx a in
        Pexp_letop a
      | Pexp_extension a ->
        let a = self#extension ctx a in
        Pexp_extension a
      | Pexp_unreachable -> Pexp_unreachable
      | Pexp_stack a ->
        let a = self#expression ctx a in
        Pexp_stack a
      | Pexp_comprehension a ->
        let a = self#comprehension_expression ctx a in
        Pexp_comprehension a
      | Pexp_overwrite (a, b) ->
        let a = self#expression ctx a in
        let b = self#expression ctx b in
        Pexp_overwrite (a, b)
      | Pexp_quote a ->
        let a = self#expression ctx a in
        Pexp_quote a
      | Pexp_splice a ->
        let a = self#expression ctx a in
        Pexp_splice a
      | Pexp_hole -> Pexp_hole
      | Pexp_index_op { kind; op; seq; indices; assign } ->
        let kind = self#paren_kind ctx kind in
        let op =
          self
          #
          option
            (fun ctx (a, b) ->
              let a = self#option self#longident ctx a in
              let b = self#string ctx b in
              a, b)
            ctx
            op
        in
        let seq = self#expression ctx seq in
        let indices = self#list self#expression ctx indices in
        let assign = self#option self#expression ctx assign in
        Pexp_index_op { kind; op; seq; indices; assign }
      | Pexp_parens { exp; optional } ->
        let exp = self#expression ctx exp in
        let optional = self#bool ctx optional in
        Pexp_parens { exp; optional }
      | Pexp_begin_end a ->
        let a = self#option self#expression ctx a in
        Pexp_begin_end a
      | Pexp_list a ->
        let a = self#list self#expression ctx a in
        Pexp_list a
      | Pexp_cons (a, b) ->
        let a = self#expression ctx a in
        let b = self#expression ctx b in
        Pexp_cons (a, b)
      | Pexp_exclave a ->
        let a = self#expression ctx a in
        Pexp_exclave a
      | Pexp_mode_legacy (a, b) ->
        let a = self#loc self#mode ctx a in
        let b = self#expression ctx b in
        Pexp_mode_legacy (a, b)

    method record_field
      : 'a. ('ctx -> 'a -> 'a) -> 'ctx -> 'a record_field -> 'a record_field
      =
      fun _a ctx { field_name; typ; value } ->
      let field_name = self#loc self#longident ctx field_name in
      let typ = self#option self#type_constraint ctx typ in
      let value = self#option _a ctx value in
      { field_name; typ; value }

    method case : 'ctx -> case -> case =
      fun ctx { pc_lhs; pc_guard; pc_rhs; pc_tokens } ->
      let pc_lhs = self#pattern ctx pc_lhs in
      let pc_guard = self#option self#expression ctx pc_guard in
      let pc_rhs = self#expression ctx pc_rhs in
      let pc_tokens = self#token_seq ctx pc_tokens in
      { pc_lhs; pc_guard; pc_rhs; pc_tokens }

    method letop : 'ctx -> letop -> letop =
      fun ctx { let_; ands; body } ->
      let let_ = self#binding_op ctx let_ in
      let ands = self#list self#binding_op ctx ands in
      let body = self#expression ctx body in
      { let_; ands; body }

    method binding_op : 'ctx -> binding_op -> binding_op =
      fun ctx { pbop_op; pbop_binding; pbop_loc } ->
      let pbop_op = self#loc self#string ctx pbop_op in
      let pbop_binding = self#value_binding ctx pbop_binding in
      let pbop_loc = self#location ctx pbop_loc in
      { pbop_op; pbop_binding; pbop_loc }

    method argument_desc
      : 'a. ('ctx -> 'a -> 'a) -> 'ctx -> 'a argument_desc -> 'a argument_desc
      =
      fun _a ctx x ->
      match x with
      | Parg_unlabelled { legacy_modes; arg; typ_constraint; modes } ->
        let legacy_modes = self#modes ctx legacy_modes in
        let arg = _a ctx arg in
        let typ_constraint =
          self#option self#type_constraint ctx typ_constraint
        in
        let modes = self#modes ctx modes in
        Parg_unlabelled { legacy_modes; arg; typ_constraint; modes }
      | Parg_labelled
          { optional
          ; legacy_modes
          ; name
          ; maybe_punned
          ; typ_constraint
          ; modes
          ; default
          } ->
        let optional = self#bool ctx optional in
        let legacy_modes = self#modes ctx legacy_modes in
        let name = self#string ctx name in
        let maybe_punned = self#option _a ctx maybe_punned in
        let typ_constraint =
          self#option self#type_constraint ctx typ_constraint
        in
        let modes = self#modes ctx modes in
        let default = self#option self#expression ctx default in
        Parg_labelled
          { optional
          ; legacy_modes
          ; name
          ; maybe_punned
          ; typ_constraint
          ; modes
          ; default
          }

    method argument
      : 'a. ('ctx -> 'a -> 'a) -> 'ctx -> 'a argument -> 'a argument
      =
      fun _a ctx { parg_desc; parg_tokens } ->
      let parg_desc = self#argument_desc _a ctx parg_desc in
      let parg_tokens = self#token_seq ctx parg_tokens in
      { parg_desc; parg_tokens }

    method function_param_desc
      : 'ctx -> function_param_desc -> function_param_desc
      =
      fun ctx x ->
      match x with
      | Pparam_val a ->
        let a = self#argument self#pattern ctx a in
        Pparam_val a
      | Pparam_newtype (a, b) ->
        let a = self#loc self#string ctx a in
        let b = self#option self#jkind_annotation ctx b in
        Pparam_newtype (a, b)
      | Pparam_newtypes a ->
        let a =
          self
          #
          list
            (fun ctx (a, b) ->
              let a = self#loc self#string ctx a in
              let b = self#option self#jkind_annotation ctx b in
              a, b)
            ctx
            a
        in
        Pparam_newtypes a

    method function_param : 'ctx -> function_param -> function_param =
      fun ctx { pparam_loc; pparam_desc } ->
      let pparam_loc = self#location ctx pparam_loc in
      let pparam_desc = self#function_param_desc ctx pparam_desc in
      { pparam_loc; pparam_desc }

    method function_body : 'ctx -> function_body -> function_body =
      fun ctx { pfb_desc; pfb_loc; pfb_tokens } ->
      let pfb_desc = self#function_body_desc ctx pfb_desc in
      let pfb_loc = self#location ctx pfb_loc in
      let pfb_tokens = self#token_seq ctx pfb_tokens in
      { pfb_desc; pfb_loc; pfb_tokens }

    method function_body_desc : 'ctx -> function_body_desc -> function_body_desc
      =
      fun ctx x ->
      match x with
      | Pfunction_body a ->
        let a = self#expression ctx a in
        Pfunction_body a
      | Pfunction_cases (a, b) ->
        let a = self#list self#case ctx a in
        let b = self#ext_attribute ctx b in
        Pfunction_cases (a, b)

    method type_constraint : 'ctx -> type_constraint -> type_constraint =
      fun ctx x ->
      match x with
      | Pconstraint a ->
        let a = self#core_type ctx a in
        Pconstraint a
      | Pcoerce (a, b) ->
        let a = self#option self#core_type ctx a in
        let b = self#core_type ctx b in
        Pcoerce (a, b)

    method function_constraint
      : 'ctx -> function_constraint -> function_constraint
      =
      fun ctx { ret_mode_annotations; ret_type_constraint } ->
      let ret_mode_annotations = self#modes ctx ret_mode_annotations in
      let ret_type_constraint =
        self#option self#type_constraint ctx ret_type_constraint
      in
      { ret_mode_annotations; ret_type_constraint }

    method block_access : 'ctx -> block_access -> block_access =
      fun ctx x ->
      match x with
      | Baccess_field a ->
        let a = self#loc self#longident ctx a in
        Baccess_field a
      | Baccess_array (a, b, c) ->
        let a = self#mutable_flag ctx a in
        let b = self#index_kind ctx b in
        let c = self#expression ctx c in
        Baccess_array (a, b, c)
      | Baccess_block (a, b) ->
        let a = self#mutable_flag ctx a in
        let b = self#expression ctx b in
        Baccess_block (a, b)

    method unboxed_access : 'ctx -> unboxed_access -> unboxed_access =
      fun ctx x ->
      match x with
      | Uaccess_unboxed_field a ->
        let a = self#loc self#longident ctx a in
        Uaccess_unboxed_field a

    method comprehension_iterator
      : 'ctx -> comprehension_iterator -> comprehension_iterator
      =
      fun ctx x ->
      match x with
      | Pcomp_range { start; stop; direction } ->
        let start = self#expression ctx start in
        let stop = self#expression ctx stop in
        let direction = self#direction_flag ctx direction in
        Pcomp_range { start; stop; direction }
      | Pcomp_in a ->
        let a = self#expression ctx a in
        Pcomp_in a

    method comprehension_clause_binding
      : 'ctx -> comprehension_clause_binding -> comprehension_clause_binding
      =
      fun
        ctx
        { pcomp_cb_mode
        ; pcomp_cb_pattern
        ; pcomp_cb_iterator
        ; pcomp_cb_attributes
        ; pcomp_cb_tokens
        }
        ->
      let pcomp_cb_mode = self#option (self#loc self#mode) ctx pcomp_cb_mode in
      let pcomp_cb_pattern = self#pattern ctx pcomp_cb_pattern in
      let pcomp_cb_iterator =
        self#comprehension_iterator ctx pcomp_cb_iterator
      in
      let pcomp_cb_attributes = self#attributes ctx pcomp_cb_attributes in
      let pcomp_cb_tokens = self#token_seq ctx pcomp_cb_tokens in
      { pcomp_cb_mode
      ; pcomp_cb_pattern
      ; pcomp_cb_iterator
      ; pcomp_cb_attributes
      ; pcomp_cb_tokens
      }

    method comprehension_clause
      : 'ctx -> comprehension_clause -> comprehension_clause
      =
      fun ctx x ->
      match x with
      | Pcomp_for a ->
        let a = self#list self#comprehension_clause_binding ctx a in
        Pcomp_for a
      | Pcomp_when a ->
        let a = self#expression ctx a in
        Pcomp_when a

    method comprehension : 'ctx -> comprehension -> comprehension =
      fun ctx { pcomp_body; pcomp_clauses; pcomp_tokens } ->
      let pcomp_body = self#expression ctx pcomp_body in
      let pcomp_clauses =
        self#list self#comprehension_clause ctx pcomp_clauses
      in
      let pcomp_tokens = self#token_seq ctx pcomp_tokens in
      { pcomp_body; pcomp_clauses; pcomp_tokens }

    method comprehension_expression
      : 'ctx -> comprehension_expression -> comprehension_expression
      =
      fun ctx x ->
      match x with
      | Pcomp_list_comprehension a ->
        let a = self#comprehension ctx a in
        Pcomp_list_comprehension a
      | Pcomp_array_comprehension (a, b) ->
        let a = self#mutable_flag ctx a in
        let b = self#comprehension ctx b in
        Pcomp_array_comprehension (a, b)

    method value_description : 'ctx -> value_description -> value_description =
      fun
        ctx
        { pval_pre_doc
        ; pval_ext_attrs
        ; pval_name
        ; pval_type
        ; pval_modalities
        ; pval_prim
        ; pval_attributes
        ; pval_post_doc
        ; pval_loc
        ; pval_tokens
        }
        ->
      let pval_pre_doc = self#option self#string ctx pval_pre_doc in
      let pval_ext_attrs = self#ext_attribute ctx pval_ext_attrs in
      let pval_name = self#loc self#longident_str_or_op ctx pval_name in
      let pval_type = self#core_type ctx pval_type in
      let pval_modalities = self#modalities ctx pval_modalities in
      let pval_prim = self#list self#string ctx pval_prim in
      let pval_attributes = self#attributes ctx pval_attributes in
      let pval_post_doc = self#option self#string ctx pval_post_doc in
      let pval_loc = self#location ctx pval_loc in
      let pval_tokens = self#token_seq ctx pval_tokens in
      { pval_pre_doc
      ; pval_ext_attrs
      ; pval_name
      ; pval_type
      ; pval_modalities
      ; pval_prim
      ; pval_attributes
      ; pval_post_doc
      ; pval_loc
      ; pval_tokens
      }

    method ptype_param : 'ctx -> ptype_param -> ptype_param =
      fun ctx { ptp_typ; ptp_infos; ptp_tokens } ->
      let ptp_typ = self#core_type ctx ptp_typ in
      let ptp_infos =
        (fun ctx (a, b) ->
          let a = self#variance ctx a in
          let b = self#injectivity ctx b in
          a, b)
          ctx
          ptp_infos
      in
      let ptp_tokens = self#token_seq ctx ptp_tokens in
      { ptp_typ; ptp_infos; ptp_tokens }

    method ptype_params : 'ctx -> ptype_params -> ptype_params =
      self#list self#ptype_param

    method ptype_constraint : 'ctx -> ptype_constraint -> ptype_constraint =
      fun ctx (a, b, c) ->
      let a = self#core_type ctx a in
      let b = self#core_type ctx b in
      let c = self#location ctx c in
      a, b, c

    method type_declaration : 'ctx -> type_declaration -> type_declaration =
      fun
        ctx
        { ptype_pre_text
        ; ptype_pre_doc
        ; ptype_ext_attrs
        ; ptype_name
        ; ptype_params
        ; ptype_jkind_annotation
        ; ptype_private
        ; ptype_manifest
        ; ptype_kind
        ; ptype_cstrs
        ; ptype_attributes
        ; ptype_post_doc
        ; ptype_loc
        ; ptype_tokens
        }
        ->
      let ptype_pre_text = self#list self#string ctx ptype_pre_text in
      let ptype_pre_doc = self#option self#string ctx ptype_pre_doc in
      let ptype_ext_attrs = self#ext_attribute ctx ptype_ext_attrs in
      let ptype_name = self#loc self#string ctx ptype_name in
      let ptype_params = self#ptype_params ctx ptype_params in
      let ptype_jkind_annotation =
        self#option self#jkind_annotation ctx ptype_jkind_annotation
      in
      let ptype_private = self#private_flag ctx ptype_private in
      let ptype_manifest = self#option self#core_type ctx ptype_manifest in
      let ptype_kind = self#type_kind ctx ptype_kind in
      let ptype_cstrs = self#list self#ptype_constraint ctx ptype_cstrs in
      let ptype_attributes = self#attributes ctx ptype_attributes in
      let ptype_post_doc = self#option self#string ctx ptype_post_doc in
      let ptype_loc = self#location ctx ptype_loc in
      let ptype_tokens = self#token_seq ctx ptype_tokens in
      { ptype_pre_text
      ; ptype_pre_doc
      ; ptype_ext_attrs
      ; ptype_name
      ; ptype_params
      ; ptype_jkind_annotation
      ; ptype_private
      ; ptype_manifest
      ; ptype_kind
      ; ptype_cstrs
      ; ptype_attributes
      ; ptype_post_doc
      ; ptype_loc
      ; ptype_tokens
      }

    method type_kind : 'ctx -> type_kind -> type_kind =
      fun ctx x ->
      match x with
      | Ptype_abstract -> Ptype_abstract
      | Ptype_variant a ->
        let a = self#list self#constructor_declaration ctx a in
        Ptype_variant a
      | Ptype_record a ->
        let a = self#list self#label_declaration ctx a in
        Ptype_record a
      | Ptype_record_unboxed_product a ->
        let a = self#list self#label_declaration ctx a in
        Ptype_record_unboxed_product a
      | Ptype_open -> Ptype_open

    method label_declaration : 'ctx -> label_declaration -> label_declaration =
      fun
        ctx
        { pld_name
        ; pld_mutable
        ; pld_global
        ; pld_modalities
        ; pld_type
        ; pld_loc
        ; pld_attributes
        ; pld_doc
        ; pld_tokens
        }
        ->
      let pld_name = self#loc self#string ctx pld_name in
      let pld_mutable = self#mutable_flag ctx pld_mutable in
      let pld_global = self#bool ctx pld_global in
      let pld_modalities = self#modalities ctx pld_modalities in
      let pld_type = self#core_type ctx pld_type in
      let pld_loc = self#location ctx pld_loc in
      let pld_attributes = self#attributes ctx pld_attributes in
      let pld_doc = self#option self#string ctx pld_doc in
      let pld_tokens = self#token_seq ctx pld_tokens in
      { pld_name
      ; pld_mutable
      ; pld_global
      ; pld_modalities
      ; pld_type
      ; pld_loc
      ; pld_attributes
      ; pld_doc
      ; pld_tokens
      }

    method constructor_declaration
      : 'ctx -> constructor_declaration -> constructor_declaration
      =
      fun
        ctx
        { pcd_name
        ; pcd_vars
        ; pcd_args
        ; pcd_res
        ; pcd_loc
        ; pcd_attributes
        ; pcd_doc
        ; pcd_tokens
        }
        ->
      let pcd_name = self#loc self#longident_str_or_op ctx pcd_name in
      let pcd_vars =
        self
        #
        list
          (fun ctx (a, b) ->
            let a = self#loc self#string ctx a in
            let b = self#option self#jkind_annotation ctx b in
            a, b)
          ctx
          pcd_vars
      in
      let pcd_args = self#constructor_arguments ctx pcd_args in
      let pcd_res = self#option self#core_type ctx pcd_res in
      let pcd_loc = self#location ctx pcd_loc in
      let pcd_attributes = self#attributes ctx pcd_attributes in
      let pcd_doc = self#option self#string ctx pcd_doc in
      let pcd_tokens = self#token_seq ctx pcd_tokens in
      { pcd_name
      ; pcd_vars
      ; pcd_args
      ; pcd_res
      ; pcd_loc
      ; pcd_attributes
      ; pcd_doc
      ; pcd_tokens
      }

    method constructor_argument
      : 'ctx -> constructor_argument -> constructor_argument
      =
      fun ctx { pca_global; pca_type; pca_modalities; pca_loc } ->
      let pca_global = self#bool ctx pca_global in
      let pca_type = self#core_type ctx pca_type in
      let pca_modalities = self#modalities ctx pca_modalities in
      let pca_loc = self#location ctx pca_loc in
      { pca_global; pca_type; pca_modalities; pca_loc }

    method constructor_arguments
      : 'ctx -> constructor_arguments -> constructor_arguments
      =
      fun ctx x ->
      match x with
      | Pcstr_tuple a ->
        let a = self#list self#constructor_argument ctx a in
        Pcstr_tuple a
      | Pcstr_record a ->
        let a = self#list self#label_declaration ctx a in
        Pcstr_record a

    method type_extension : 'ctx -> type_extension -> type_extension =
      fun
        ctx
        { ptyext_pre_doc
        ; ptyext_ext_attrs
        ; ptyext_path
        ; ptyext_params
        ; ptyext_constructors
        ; ptyext_private
        ; ptyext_loc
        ; ptyext_attributes
        ; ptyext_post_doc
        ; ptyext_tokens
        }
        ->
      let ptyext_pre_doc = self#option self#string ctx ptyext_pre_doc in
      let ptyext_ext_attrs = self#ext_attribute ctx ptyext_ext_attrs in
      let ptyext_path = self#loc self#longident ctx ptyext_path in
      let ptyext_params = self#list self#ptype_param ctx ptyext_params in
      let ptyext_constructors =
        self#list self#extension_constructor ctx ptyext_constructors
      in
      let ptyext_private = self#private_flag ctx ptyext_private in
      let ptyext_loc = self#location ctx ptyext_loc in
      let ptyext_attributes = self#attributes ctx ptyext_attributes in
      let ptyext_post_doc = self#option self#string ctx ptyext_post_doc in
      let ptyext_tokens = self#token_seq ctx ptyext_tokens in
      { ptyext_pre_doc
      ; ptyext_ext_attrs
      ; ptyext_path
      ; ptyext_params
      ; ptyext_constructors
      ; ptyext_private
      ; ptyext_loc
      ; ptyext_attributes
      ; ptyext_post_doc
      ; ptyext_tokens
      }

    method extension_constructor
      : 'ctx -> extension_constructor -> extension_constructor
      =
      fun
        ctx
        { pext_name
        ; pext_kind
        ; pext_loc
        ; pext_attributes
        ; pext_doc
        ; pext_tokens
        }
        ->
      let pext_name = self#loc self#longident_str_or_op ctx pext_name in
      let pext_kind = self#extension_constructor_kind ctx pext_kind in
      let pext_loc = self#location ctx pext_loc in
      let pext_attributes = self#attributes ctx pext_attributes in
      let pext_doc = self#option self#string ctx pext_doc in
      let pext_tokens = self#token_seq ctx pext_tokens in
      { pext_name; pext_kind; pext_loc; pext_attributes; pext_doc; pext_tokens }

    method type_exception : 'ctx -> type_exception -> type_exception =
      fun
        ctx
        { ptyexn_pre_doc
        ; ptyexn_ext_attrs
        ; ptyexn_constructor
        ; ptyexn_loc
        ; ptyexn_attributes
        ; ptyexn_post_doc
        ; ptyexn_tokens
        }
        ->
      let ptyexn_pre_doc = self#option self#string ctx ptyexn_pre_doc in
      let ptyexn_ext_attrs = self#ext_attribute ctx ptyexn_ext_attrs in
      let ptyexn_constructor =
        self#extension_constructor ctx ptyexn_constructor
      in
      let ptyexn_loc = self#location ctx ptyexn_loc in
      let ptyexn_attributes = self#attributes ctx ptyexn_attributes in
      let ptyexn_post_doc = self#option self#string ctx ptyexn_post_doc in
      let ptyexn_tokens = self#token_seq ctx ptyexn_tokens in
      { ptyexn_pre_doc
      ; ptyexn_ext_attrs
      ; ptyexn_constructor
      ; ptyexn_loc
      ; ptyexn_attributes
      ; ptyexn_post_doc
      ; ptyexn_tokens
      }

    method extension_constructor_kind
      : 'ctx -> extension_constructor_kind -> extension_constructor_kind
      =
      fun ctx x ->
      match x with
      | Pext_decl (a, b, c) ->
        let a =
          self
          #
          list
            (fun ctx (a, b) ->
              let a = self#loc self#string ctx a in
              let b = self#option self#jkind_annotation ctx b in
              a, b)
            ctx
            a
        in
        let b = self#constructor_arguments ctx b in
        let c = self#option self#core_type ctx c in
        Pext_decl (a, b, c)
      | Pext_rebind a ->
        let a = self#loc self#longident ctx a in
        Pext_rebind a

    method class_type : 'ctx -> class_type -> class_type =
      fun ctx { pcty_desc; pcty_loc; pcty_attributes; pcty_tokens } ->
      let pcty_desc = self#class_type_desc ctx pcty_desc in
      let pcty_loc = self#location ctx pcty_loc in
      let pcty_attributes = self#attributes ctx pcty_attributes in
      let pcty_tokens = self#token_seq ctx pcty_tokens in
      { pcty_desc; pcty_loc; pcty_attributes; pcty_tokens }

    method class_type_desc : 'ctx -> class_type_desc -> class_type_desc =
      fun ctx x ->
      match x with
      | Pcty_constr (a, b) ->
        let a = self#loc self#longident ctx a in
        let b = self#list self#core_type ctx b in
        Pcty_constr (a, b)
      | Pcty_signature a ->
        let a = self#class_signature ctx a in
        Pcty_signature a
      | Pcty_arrow (a, b) ->
        let a = self#arrow_arg ctx a in
        let b = self#class_type ctx b in
        Pcty_arrow (a, b)
      | Pcty_extension a ->
        let a = self#extension ctx a in
        Pcty_extension a
      | Pcty_open (a, b) ->
        let a = self#open_description ctx a in
        let b = self#class_type ctx b in
        Pcty_open (a, b)

    method class_signature : 'ctx -> class_signature -> class_signature =
      fun ctx { pcsig_self; pcsig_fields } ->
      let pcsig_self = self#option self#core_type ctx pcsig_self in
      let pcsig_fields = self#list self#class_type_field ctx pcsig_fields in
      { pcsig_self; pcsig_fields }

    method class_type_field : 'ctx -> class_type_field -> class_type_field =
      fun
        ctx
        { pctf_pre_doc
        ; pctf_desc
        ; pctf_loc
        ; pctf_attributes
        ; pctf_post_doc
        ; pctf_tokens
        }
        ->
      let pctf_pre_doc = self#option self#string ctx pctf_pre_doc in
      let pctf_desc = self#class_type_field_desc ctx pctf_desc in
      let pctf_loc = self#location ctx pctf_loc in
      let pctf_attributes = self#attributes ctx pctf_attributes in
      let pctf_post_doc = self#option self#string ctx pctf_post_doc in
      let pctf_tokens = self#token_seq ctx pctf_tokens in
      { pctf_pre_doc
      ; pctf_desc
      ; pctf_loc
      ; pctf_attributes
      ; pctf_post_doc
      ; pctf_tokens
      }

    method class_type_field_desc
      : 'ctx -> class_type_field_desc -> class_type_field_desc
      =
      fun ctx x ->
      match x with
      | Pctf_inherit a ->
        let a = self#class_type ctx a in
        Pctf_inherit a
      | Pctf_val a ->
        let a =
          (fun ctx (a, b, c, d) ->
            let a = self#loc self#string ctx a in
            let b = self#mutable_flag ctx b in
            let c = self#virtual_flag ctx c in
            let d = self#core_type ctx d in
            a, b, c, d)
            ctx
            a
        in
        Pctf_val a
      | Pctf_method a ->
        let a =
          (fun ctx (a, b, c, d) ->
            let a = self#loc self#string ctx a in
            let b = self#private_flag ctx b in
            let c = self#virtual_flag ctx c in
            let d = self#core_type ctx d in
            a, b, c, d)
            ctx
            a
        in
        Pctf_method a
      | Pctf_constraint a ->
        let a =
          (fun ctx (a, b) ->
            let a = self#core_type ctx a in
            let b = self#core_type ctx b in
            a, b)
            ctx
            a
        in
        Pctf_constraint a
      | Pctf_attribute a ->
        let a = self#attribute ctx a in
        Pctf_attribute a
      | Pctf_extension a ->
        let a = self#extension ctx a in
        Pctf_extension a
      | Pctf_docstring a ->
        let a = self#string ctx a in
        Pctf_docstring a

    method class_infos
      : 'a. ('ctx -> 'a -> 'a) -> 'ctx -> 'a class_infos -> 'a class_infos
      =
      fun
        _a ctx
        { pci_pre_text
        ; pci_pre_doc
        ; pci_virt
        ; pci_ext_attrs
        ; pci_params
        ; pci_name
        ; pci_value_params
        ; pci_constraint
        ; pci_expr
        ; pci_loc
        ; pci_attributes
        ; pci_post_doc
        ; pci_tokens
        }
        ->
      let pci_pre_text = self#list self#string ctx pci_pre_text in
      let pci_pre_doc = self#option self#string ctx pci_pre_doc in
      let pci_virt = self#virtual_flag ctx pci_virt in
      let pci_ext_attrs = self#ext_attribute ctx pci_ext_attrs in
      let pci_params = self#list self#ptype_param ctx pci_params in
      let pci_name = self#loc self#string ctx pci_name in
      let pci_value_params =
        self#list (self#argument self#pattern) ctx pci_value_params
      in
      let pci_constraint = self#option self#class_type ctx pci_constraint in
      let pci_expr = _a ctx pci_expr in
      let pci_loc = self#location ctx pci_loc in
      let pci_attributes = self#attributes ctx pci_attributes in
      let pci_post_doc = self#option self#string ctx pci_post_doc in
      let pci_tokens = self#token_seq ctx pci_tokens in
      { pci_pre_text
      ; pci_pre_doc
      ; pci_virt
      ; pci_ext_attrs
      ; pci_params
      ; pci_name
      ; pci_value_params
      ; pci_constraint
      ; pci_expr
      ; pci_loc
      ; pci_attributes
      ; pci_post_doc
      ; pci_tokens
      }

    method class_description : 'ctx -> class_description -> class_description =
      self#class_infos self#class_type

    method class_type_declaration
      : 'ctx -> class_type_declaration -> class_type_declaration
      =
      self#class_infos self#class_type

    method class_expr : 'ctx -> class_expr -> class_expr =
      fun ctx { pcl_ext_attrs; pcl_desc; pcl_loc; pcl_attributes } ->
      let pcl_ext_attrs = self#ext_attribute ctx pcl_ext_attrs in
      let pcl_desc = self#class_expr_desc ctx pcl_desc in
      let pcl_loc = self#location ctx pcl_loc in
      let pcl_attributes = self#attributes ctx pcl_attributes in
      { pcl_ext_attrs; pcl_desc; pcl_loc; pcl_attributes }

    method class_expr_desc : 'ctx -> class_expr_desc -> class_expr_desc =
      fun ctx x ->
      match x with
      | Pcl_constr (a, b) ->
        let a = self#loc self#longident ctx a in
        let b = self#list self#core_type ctx b in
        Pcl_constr (a, b)
      | Pcl_structure a ->
        let a = self#class_structure ctx a in
        Pcl_structure a
      | Pcl_fun (a, b) ->
        let a = self#list (self#argument self#pattern) ctx a in
        let b = self#class_expr ctx b in
        Pcl_fun (a, b)
      | Pcl_apply (a, b) ->
        let a = self#class_expr ctx a in
        let b = self#list (self#argument self#expression) ctx b in
        Pcl_apply (a, b)
      | Pcl_let (a, b, c) ->
        let a = self#rec_flag ctx a in
        let b = self#list self#value_binding ctx b in
        let c = self#class_expr ctx c in
        Pcl_let (a, b, c)
      | Pcl_constraint (a, b) ->
        let a = self#class_expr ctx a in
        let b = self#class_type ctx b in
        Pcl_constraint (a, b)
      | Pcl_extension a ->
        let a = self#extension ctx a in
        Pcl_extension a
      | Pcl_open (a, b) ->
        let a = self#open_description ctx a in
        let b = self#class_expr ctx b in
        Pcl_open (a, b)
      | Pcl_parens a ->
        let a = self#class_expr ctx a in
        Pcl_parens a

    method class_structure : 'ctx -> class_structure -> class_structure =
      fun ctx { pcstr_self; pcstr_fields } ->
      let pcstr_self = self#pattern ctx pcstr_self in
      let pcstr_fields = self#list self#class_field ctx pcstr_fields in
      { pcstr_self; pcstr_fields }

    method class_field : 'ctx -> class_field -> class_field =
      fun
        ctx
        { pcf_pre_doc
        ; pcf_desc
        ; pcf_loc
        ; pcf_attributes
        ; pcf_post_doc
        ; pcf_tokens
        }
        ->
      let pcf_pre_doc = self#option self#string ctx pcf_pre_doc in
      let pcf_desc = self#class_field_desc ctx pcf_desc in
      let pcf_loc = self#location ctx pcf_loc in
      let pcf_attributes = self#attributes ctx pcf_attributes in
      let pcf_post_doc = self#option self#string ctx pcf_post_doc in
      let pcf_tokens = self#token_seq ctx pcf_tokens in
      { pcf_pre_doc
      ; pcf_desc
      ; pcf_loc
      ; pcf_attributes
      ; pcf_post_doc
      ; pcf_tokens
      }

    method class_field_desc : 'ctx -> class_field_desc -> class_field_desc =
      fun ctx x ->
      match x with
      | Pcf_inherit (a, b, c) ->
        let a = self#override_flag ctx a in
        let b = self#class_expr ctx b in
        let c = self#option (self#loc self#string) ctx c in
        Pcf_inherit (a, b, c)
      | Pcf_val a ->
        let a =
          (fun ctx (a, b, c) ->
            let a = self#loc self#string ctx a in
            let b = self#mutable_flag ctx b in
            let c = self#class_field_kind ctx c in
            a, b, c)
            ctx
            a
        in
        Pcf_val a
      | Pcf_method a ->
        let a =
          (fun ctx (a, b, c) ->
            let a = self#loc self#string ctx a in
            let b = self#private_flag ctx b in
            let c = self#class_field_kind ctx c in
            a, b, c)
            ctx
            a
        in
        Pcf_method a
      | Pcf_constraint a ->
        let a =
          (fun ctx (a, b) ->
            let a = self#core_type ctx a in
            let b = self#core_type ctx b in
            a, b)
            ctx
            a
        in
        Pcf_constraint a
      | Pcf_initializer a ->
        let a = self#expression ctx a in
        Pcf_initializer a
      | Pcf_attribute a ->
        let a = self#attribute ctx a in
        Pcf_attribute a
      | Pcf_extension a ->
        let a = self#extension ctx a in
        Pcf_extension a
      | Pcf_docstring a ->
        let a = self#string ctx a in
        Pcf_docstring a

    method class_field_kind : 'ctx -> class_field_kind -> class_field_kind =
      fun ctx x ->
      match x with
      | Cfk_virtual a ->
        let a = self#core_type ctx a in
        Cfk_virtual a
      | Cfk_concrete (a, b) ->
        let a = self#override_flag ctx a in
        let b = self#value_binding ctx b in
        Cfk_concrete (a, b)

    method class_declaration : 'ctx -> class_declaration -> class_declaration =
      self#class_infos self#class_expr

    method module_type : 'ctx -> module_type -> module_type =
      fun ctx { pmty_desc; pmty_loc; pmty_attributes; pmty_tokens } ->
      let pmty_desc = self#module_type_desc ctx pmty_desc in
      let pmty_loc = self#location ctx pmty_loc in
      let pmty_attributes = self#attributes ctx pmty_attributes in
      let pmty_tokens = self#token_seq ctx pmty_tokens in
      { pmty_desc; pmty_loc; pmty_attributes; pmty_tokens }

    method module_type_desc : 'ctx -> module_type_desc -> module_type_desc =
      fun ctx x ->
      match x with
      | Pmty_ident a ->
        let a = self#loc self#longident ctx a in
        Pmty_ident a
      | Pmty_signature a ->
        let a = self#signature ctx a in
        Pmty_signature a
      | Pmty_functor (a, b, c, d) ->
        let a = self#attributes ctx a in
        let b = self#list self#functor_parameter ctx b in
        let c = self#module_type ctx c in
        let d = self#modes ctx d in
        Pmty_functor (a, b, c, d)
      | Pmty_functor_type (a, b, c) ->
        let a = self#list self#functor_parameter ctx a in
        let b = self#module_type ctx b in
        let c = self#modes ctx c in
        Pmty_functor_type (a, b, c)
      | Pmty_with (a, b) ->
        let a = self#module_type ctx a in
        let b = self#list self#with_constraint ctx b in
        Pmty_with (a, b)
      | Pmty_typeof (a, b) ->
        let a = self#attributes ctx a in
        let b = self#module_expr ctx b in
        Pmty_typeof (a, b)
      | Pmty_extension a ->
        let a = self#extension ctx a in
        Pmty_extension a
      | Pmty_alias a ->
        let a = self#loc self#longident ctx a in
        Pmty_alias a
      | Pmty_strengthen (a, b) ->
        let a = self#module_type ctx a in
        let b = self#loc self#longident ctx b in
        Pmty_strengthen (a, b)
      | Pmty_parens a ->
        let a = self#module_type ctx a in
        Pmty_parens a

    method functor_parameter : 'ctx -> functor_parameter -> functor_parameter =
      fun ctx x ->
      match x with
      | Unit -> Unit
      | Named (a, b, c) ->
        let a = self#loc (self#option self#string) ctx a in
        let b = self#module_type ctx b in
        let c = self#modes ctx c in
        Named (a, b, c)
      | Unnamed (a, b) ->
        let a = self#module_type ctx a in
        let b = self#modes ctx b in
        Unnamed (a, b)

    method signature : 'ctx -> signature -> signature =
      fun ctx { psg_modalities; psg_items; psg_loc; psg_tokens } ->
      let psg_modalities = self#modalities ctx psg_modalities in
      let psg_items = self#list self#signature_item ctx psg_items in
      let psg_loc = self#location ctx psg_loc in
      let psg_tokens = self#token_seq ctx psg_tokens in
      { psg_modalities; psg_items; psg_loc; psg_tokens }

    method signature_item : 'ctx -> signature_item -> signature_item =
      fun ctx { psig_desc; psig_loc; psig_tokens } ->
      let psig_desc = self#signature_item_desc ctx psig_desc in
      let psig_loc = self#location ctx psig_loc in
      let psig_tokens = self#token_seq ctx psig_tokens in
      { psig_desc; psig_loc; psig_tokens }

    method signature_item_desc
      : 'ctx -> signature_item_desc -> signature_item_desc
      =
      fun ctx x ->
      match x with
      | Psig_value a ->
        let a = self#value_description ctx a in
        Psig_value a
      | Psig_type (a, b) ->
        let a = self#rec_flag ctx a in
        let b = self#list self#type_declaration ctx b in
        Psig_type (a, b)
      | Psig_typesubst a ->
        let a = self#list self#type_declaration ctx a in
        Psig_typesubst a
      | Psig_typext a ->
        let a = self#type_extension ctx a in
        Psig_typext a
      | Psig_exception a ->
        let a = self#type_exception ctx a in
        Psig_exception a
      | Psig_module a ->
        let a = self#module_declaration ctx a in
        Psig_module a
      | Psig_modsubst a ->
        let a = self#module_substitution ctx a in
        Psig_modsubst a
      | Psig_recmodule a ->
        let a = self#list self#module_declaration ctx a in
        Psig_recmodule a
      | Psig_modtype a ->
        let a = self#module_type_declaration ctx a in
        Psig_modtype a
      | Psig_modtypesubst a ->
        let a = self#module_type_declaration ctx a in
        Psig_modtypesubst a
      | Psig_open a ->
        let a = self#open_description ctx a in
        Psig_open a
      | Psig_include (a, b) ->
        let a = self#include_description ctx a in
        let b = self#modalities ctx b in
        Psig_include (a, b)
      | Psig_class a ->
        let a = self#list self#class_description ctx a in
        Psig_class a
      | Psig_class_type a ->
        let a = self#list self#class_type_declaration ctx a in
        Psig_class_type a
      | Psig_attribute a ->
        let a = self#attribute ctx a in
        Psig_attribute a
      | Psig_extension a ->
        let a = self#toplevel_extension ctx a in
        Psig_extension a
      | Psig_kind_abbrev (a, b) ->
        let a = self#loc self#string ctx a in
        let b = self#jkind_annotation ctx b in
        Psig_kind_abbrev (a, b)
      | Psig_docstring a ->
        let a = self#string ctx a in
        Psig_docstring a

    method module_declaration_body
      : 'ctx -> module_declaration_body -> module_declaration_body
      =
      fun ctx x ->
      match x with
      | With_params (a, b, c) ->
        let a = self#list self#functor_parameter ctx a in
        let b = self#module_type ctx b in
        let c = self#modes ctx c in
        With_params (a, b, c)
      | Without_params (a, b) ->
        let a = self#module_type ctx a in
        let b = self#modalities ctx b in
        Without_params (a, b)

    method module_declaration : 'ctx -> module_declaration -> module_declaration
      =
      fun
        ctx
        { pmd_pre_text
        ; pmd_pre_doc
        ; pmd_ext_attrs
        ; pmd_name
        ; pmd_body
        ; pmd_attributes
        ; pmd_post_doc
        ; pmd_loc
        ; pmd_tokens
        }
        ->
      let pmd_pre_text = self#list self#string ctx pmd_pre_text in
      let pmd_pre_doc = self#option self#string ctx pmd_pre_doc in
      let pmd_ext_attrs = self#ext_attribute ctx pmd_ext_attrs in
      let pmd_name =
        (fun ctx (a, b) ->
          let a = self#loc (self#option self#string) ctx a in
          let b = self#modalities ctx b in
          a, b)
          ctx
          pmd_name
      in
      let pmd_body = self#module_declaration_body ctx pmd_body in
      let pmd_attributes = self#attributes ctx pmd_attributes in
      let pmd_post_doc = self#option self#string ctx pmd_post_doc in
      let pmd_loc = self#location ctx pmd_loc in
      let pmd_tokens = self#token_seq ctx pmd_tokens in
      { pmd_pre_text
      ; pmd_pre_doc
      ; pmd_ext_attrs
      ; pmd_name
      ; pmd_body
      ; pmd_attributes
      ; pmd_post_doc
      ; pmd_loc
      ; pmd_tokens
      }

    method module_substitution
      : 'ctx -> module_substitution -> module_substitution
      =
      fun
        ctx
        { pms_pre_doc
        ; pms_ext_attrs
        ; pms_name
        ; pms_manifest
        ; pms_attributes
        ; pms_post_doc
        ; pms_loc
        ; pms_tokens
        }
        ->
      let pms_pre_doc = self#option self#string ctx pms_pre_doc in
      let pms_ext_attrs = self#ext_attribute ctx pms_ext_attrs in
      let pms_name = self#loc self#string ctx pms_name in
      let pms_manifest = self#loc self#longident ctx pms_manifest in
      let pms_attributes = self#attributes ctx pms_attributes in
      let pms_post_doc = self#option self#string ctx pms_post_doc in
      let pms_loc = self#location ctx pms_loc in
      let pms_tokens = self#token_seq ctx pms_tokens in
      { pms_pre_doc
      ; pms_ext_attrs
      ; pms_name
      ; pms_manifest
      ; pms_attributes
      ; pms_post_doc
      ; pms_loc
      ; pms_tokens
      }

    method module_type_declaration
      : 'ctx -> module_type_declaration -> module_type_declaration
      =
      fun
        ctx
        { pmtd_pre_doc
        ; pmtd_ext_attrs
        ; pmtd_name
        ; pmtd_type
        ; pmtd_attributes
        ; pmtd_post_doc
        ; pmtd_loc
        ; pmtd_tokens
        }
        ->
      let pmtd_pre_doc = self#option self#string ctx pmtd_pre_doc in
      let pmtd_ext_attrs = self#ext_attribute ctx pmtd_ext_attrs in
      let pmtd_name = self#loc self#string ctx pmtd_name in
      let pmtd_type = self#option self#module_type ctx pmtd_type in
      let pmtd_attributes = self#attributes ctx pmtd_attributes in
      let pmtd_post_doc = self#option self#string ctx pmtd_post_doc in
      let pmtd_loc = self#location ctx pmtd_loc in
      let pmtd_tokens = self#token_seq ctx pmtd_tokens in
      { pmtd_pre_doc
      ; pmtd_ext_attrs
      ; pmtd_name
      ; pmtd_type
      ; pmtd_attributes
      ; pmtd_post_doc
      ; pmtd_loc
      ; pmtd_tokens
      }

    method open_infos
      : 'a. ('ctx -> 'a -> 'a) -> 'ctx -> 'a open_infos -> 'a open_infos
      =
      fun
        _a ctx
        { popen_pre_doc
        ; popen_ext_attrs
        ; popen_expr
        ; popen_override
        ; popen_loc
        ; popen_attributes
        ; popen_post_doc
        ; popen_tokens
        }
        ->
      let popen_pre_doc = self#option self#string ctx popen_pre_doc in
      let popen_ext_attrs = self#ext_attribute ctx popen_ext_attrs in
      let popen_expr = _a ctx popen_expr in
      let popen_override = self#override_flag ctx popen_override in
      let popen_loc = self#location ctx popen_loc in
      let popen_attributes = self#attributes ctx popen_attributes in
      let popen_post_doc = self#option self#string ctx popen_post_doc in
      let popen_tokens = self#token_seq ctx popen_tokens in
      { popen_pre_doc
      ; popen_ext_attrs
      ; popen_expr
      ; popen_override
      ; popen_loc
      ; popen_attributes
      ; popen_post_doc
      ; popen_tokens
      }

    method open_description : 'ctx -> open_description -> open_description =
      self#open_infos (self#loc self#longident)

    method open_declaration : 'ctx -> open_declaration -> open_declaration =
      self#open_infos self#module_expr

    method include_infos
      : 'a. ('ctx -> 'a -> 'a) -> 'ctx -> 'a include_infos -> 'a include_infos
      =
      fun
        _a ctx
        { pincl_pre_doc
        ; pincl_kind
        ; pincl_ext_attrs
        ; pincl_mod
        ; pincl_loc
        ; pincl_attributes
        ; pincl_post_doc
        ; pincl_tokens
        }
        ->
      let pincl_pre_doc = self#option self#string ctx pincl_pre_doc in
      let pincl_kind = self#include_kind ctx pincl_kind in
      let pincl_ext_attrs = self#ext_attribute ctx pincl_ext_attrs in
      let pincl_mod = _a ctx pincl_mod in
      let pincl_loc = self#location ctx pincl_loc in
      let pincl_attributes = self#attributes ctx pincl_attributes in
      let pincl_post_doc = self#option self#string ctx pincl_post_doc in
      let pincl_tokens = self#token_seq ctx pincl_tokens in
      { pincl_pre_doc
      ; pincl_kind
      ; pincl_ext_attrs
      ; pincl_mod
      ; pincl_loc
      ; pincl_attributes
      ; pincl_post_doc
      ; pincl_tokens
      }

    method include_description
      : 'ctx -> include_description -> include_description
      =
      self#include_infos self#module_type

    method include_declaration
      : 'ctx -> include_declaration -> include_declaration
      =
      self#include_infos self#module_expr

    method with_constraint : 'ctx -> with_constraint -> with_constraint =
      fun ctx { wc_desc; wc_loc; wc_tokens } ->
      let wc_desc = self#with_constraint_desc ctx wc_desc in
      let wc_loc = self#location ctx wc_loc in
      let wc_tokens = self#token_seq ctx wc_tokens in
      { wc_desc; wc_loc; wc_tokens }

    method with_constraint_desc
      : 'ctx -> with_constraint_desc -> with_constraint_desc
      =
      fun ctx x ->
      match x with
      | Pwith_type (a, b, c, d, e) ->
        let a = self#ptype_params ctx a in
        let b = self#loc self#longident ctx b in
        let c = self#private_flag ctx c in
        let d = self#core_type ctx d in
        let e = self#list self#ptype_constraint ctx e in
        Pwith_type (a, b, c, d, e)
      | Pwith_module (a, b) ->
        let a = self#loc self#longident ctx a in
        let b = self#loc self#longident ctx b in
        Pwith_module (a, b)
      | Pwith_modtype (a, b) ->
        let a = self#loc self#longident ctx a in
        let b = self#module_type ctx b in
        Pwith_modtype (a, b)
      | Pwith_modtypesubst (a, b) ->
        let a = self#loc self#longident ctx a in
        let b = self#module_type ctx b in
        Pwith_modtypesubst (a, b)
      | Pwith_typesubst (a, b, c) ->
        let a = self#ptype_params ctx a in
        let b = self#loc self#longident ctx b in
        let c = self#core_type ctx c in
        Pwith_typesubst (a, b, c)
      | Pwith_modsubst (a, b) ->
        let a = self#loc self#longident ctx a in
        let b = self#loc self#longident ctx b in
        Pwith_modsubst (a, b)

    method module_expr : 'ctx -> module_expr -> module_expr =
      fun ctx { pmod_desc; pmod_loc; pmod_attributes; pmod_tokens } ->
      let pmod_desc = self#module_expr_desc ctx pmod_desc in
      let pmod_loc = self#location ctx pmod_loc in
      let pmod_attributes = self#attributes ctx pmod_attributes in
      let pmod_tokens = self#token_seq ctx pmod_tokens in
      { pmod_desc; pmod_loc; pmod_attributes; pmod_tokens }

    method module_expr_desc : 'ctx -> module_expr_desc -> module_expr_desc =
      fun ctx x ->
      match x with
      | Pmod_ident a ->
        let a = self#loc self#longident ctx a in
        Pmod_ident a
      | Pmod_structure (a, b) ->
        let a = self#attributes ctx a in
        let b = self#structure ctx b in
        Pmod_structure (a, b)
      | Pmod_functor (a, b, c) ->
        let a = self#attributes ctx a in
        let b = self#list self#functor_parameter ctx b in
        let c = self#module_expr ctx c in
        Pmod_functor (a, b, c)
      | Pmod_apply (a, b) ->
        let a = self#module_expr ctx a in
        let b = self#module_expr ctx b in
        Pmod_apply (a, b)
      | Pmod_apply_unit a ->
        let a = self#module_expr ctx a in
        Pmod_apply_unit a
      | Pmod_constraint (a, b, c) ->
        let a = self#module_expr ctx a in
        let b = self#option self#module_type ctx b in
        let c = self#modes ctx c in
        Pmod_constraint (a, b, c)
      | Pmod_unpack (a, b, c) ->
        let a = self#expression ctx a in
        let b = self#option self#package_type ctx b in
        let c = self#option self#package_type ctx c in
        Pmod_unpack (a, b, c)
      | Pmod_extension a ->
        let a = self#extension ctx a in
        Pmod_extension a
      | Pmod_parens a ->
        let a = self#module_expr ctx a in
        Pmod_parens a

    method structure : 'ctx -> structure -> structure =
      fun ctx (a, b) ->
      let a = self#list self#structure_item ctx a in
      let b = self#token_seq ctx b in
      a, b

    method structure_item : 'ctx -> structure_item -> structure_item =
      fun ctx { pstr_desc; pstr_loc; pstr_tokens } ->
      let pstr_desc = self#structure_item_desc ctx pstr_desc in
      let pstr_loc = self#location ctx pstr_loc in
      let pstr_tokens = self#token_seq ctx pstr_tokens in
      { pstr_desc; pstr_loc; pstr_tokens }

    method structure_item_desc
      : 'ctx -> structure_item_desc -> structure_item_desc
      =
      fun ctx x ->
      match x with
      | Pstr_eval (a, b) ->
        let a = self#expression ctx a in
        let b = self#attributes ctx b in
        Pstr_eval (a, b)
      | Pstr_value (a, b) ->
        let a = self#rec_flag ctx a in
        let b = self#list self#value_binding ctx b in
        Pstr_value (a, b)
      | Pstr_primitive a ->
        let a = self#value_description ctx a in
        Pstr_primitive a
      | Pstr_type (a, b) ->
        let a = self#rec_flag ctx a in
        let b = self#list self#type_declaration ctx b in
        Pstr_type (a, b)
      | Pstr_typext a ->
        let a = self#type_extension ctx a in
        Pstr_typext a
      | Pstr_exception a ->
        let a = self#type_exception ctx a in
        Pstr_exception a
      | Pstr_module a ->
        let a = self#module_binding ctx a in
        Pstr_module a
      | Pstr_recmodule a ->
        let a = self#list self#module_binding ctx a in
        Pstr_recmodule a
      | Pstr_modtype a ->
        let a = self#module_type_declaration ctx a in
        Pstr_modtype a
      | Pstr_open a ->
        let a = self#open_declaration ctx a in
        Pstr_open a
      | Pstr_class a ->
        let a = self#list self#class_declaration ctx a in
        Pstr_class a
      | Pstr_class_type a ->
        let a = self#list self#class_type_declaration ctx a in
        Pstr_class_type a
      | Pstr_include a ->
        let a = self#include_declaration ctx a in
        Pstr_include a
      | Pstr_attribute a ->
        let a = self#attribute ctx a in
        Pstr_attribute a
      | Pstr_extension a ->
        let a = self#toplevel_extension ctx a in
        Pstr_extension a
      | Pstr_kind_abbrev (a, b) ->
        let a = self#loc self#string ctx a in
        let b = self#jkind_annotation ctx b in
        Pstr_kind_abbrev (a, b)
      | Pstr_docstring a ->
        let a = self#string ctx a in
        Pstr_docstring a

    method value_constraint : 'ctx -> value_constraint -> value_constraint =
      fun ctx x ->
      match x with
      | Pvc_constraint { locally_abstract_univars; typ } ->
        let locally_abstract_univars =
          self
          #
          list
            (fun ctx (a, b) ->
              let a = self#loc self#string ctx a in
              let b = self#option self#jkind_annotation ctx b in
              a, b)
            ctx
            locally_abstract_univars
        in
        let typ = self#core_type ctx typ in
        Pvc_constraint { locally_abstract_univars; typ }
      | Pvc_coercion { ground; coercion } ->
        let ground = self#option self#core_type ctx ground in
        let coercion = self#core_type ctx coercion in
        Pvc_coercion { ground; coercion }

    method value_binding : 'ctx -> value_binding -> value_binding =
      fun
        ctx
        { pvb_pre_text
        ; pvb_pre_doc
        ; pvb_ext_attrs
        ; pvb_legacy_modes
        ; pvb_pat
        ; pvb_modes
        ; pvb_params
        ; pvb_constraint
        ; pvb_expr
        ; pvb_ret_modes
        ; pvb_attributes
        ; pvb_post_doc
        ; pvb_loc
        ; pvb_tokens
        }
        ->
      let pvb_pre_text = self#list self#string ctx pvb_pre_text in
      let pvb_pre_doc = self#option self#string ctx pvb_pre_doc in
      let pvb_ext_attrs = self#ext_attribute ctx pvb_ext_attrs in
      let pvb_legacy_modes = self#modes ctx pvb_legacy_modes in
      let pvb_pat = self#pattern ctx pvb_pat in
      let pvb_modes = self#modes ctx pvb_modes in
      let pvb_params = self#list self#function_param ctx pvb_params in
      let pvb_constraint =
        self#option self#value_constraint ctx pvb_constraint
      in
      let pvb_expr = self#option self#expression ctx pvb_expr in
      let pvb_ret_modes = self#modes ctx pvb_ret_modes in
      let pvb_attributes = self#attributes ctx pvb_attributes in
      let pvb_post_doc = self#option self#string ctx pvb_post_doc in
      let pvb_loc = self#location ctx pvb_loc in
      let pvb_tokens = self#token_seq ctx pvb_tokens in
      { pvb_pre_text
      ; pvb_pre_doc
      ; pvb_ext_attrs
      ; pvb_legacy_modes
      ; pvb_pat
      ; pvb_modes
      ; pvb_params
      ; pvb_constraint
      ; pvb_expr
      ; pvb_ret_modes
      ; pvb_attributes
      ; pvb_post_doc
      ; pvb_loc
      ; pvb_tokens
      }

    method module_binding : 'ctx -> module_binding -> module_binding =
      fun
        ctx
        { pmb_pre_text
        ; pmb_pre_doc
        ; pmb_ext_attrs
        ; pmb_name
        ; pmb_params
        ; pmb_constraint
        ; pmb_modes
        ; pmb_expr
        ; pmb_attributes
        ; pmb_post_doc
        ; pmb_loc
        ; pmb_tokens
        }
        ->
      let pmb_pre_text = self#list self#string ctx pmb_pre_text in
      let pmb_pre_doc = self#option self#string ctx pmb_pre_doc in
      let pmb_ext_attrs = self#ext_attribute ctx pmb_ext_attrs in
      let pmb_name =
        (fun ctx (a, b) ->
          let a = self#loc (self#option self#string) ctx a in
          let b = self#modes ctx b in
          a, b)
          ctx
          pmb_name
      in
      let pmb_params = self#list self#functor_parameter ctx pmb_params in
      let pmb_constraint = self#option self#module_type ctx pmb_constraint in
      let pmb_modes = self#modes ctx pmb_modes in
      let pmb_expr = self#module_expr ctx pmb_expr in
      let pmb_attributes = self#attributes ctx pmb_attributes in
      let pmb_post_doc = self#option self#string ctx pmb_post_doc in
      let pmb_loc = self#location ctx pmb_loc in
      let pmb_tokens = self#token_seq ctx pmb_tokens in
      { pmb_pre_text
      ; pmb_pre_doc
      ; pmb_ext_attrs
      ; pmb_name
      ; pmb_params
      ; pmb_constraint
      ; pmb_modes
      ; pmb_expr
      ; pmb_attributes
      ; pmb_post_doc
      ; pmb_loc
      ; pmb_tokens
      }

    method jkind_annotation_desc
      : 'ctx -> jkind_annotation_desc -> jkind_annotation_desc
      =
      fun ctx x ->
      match x with
      | Pjk_default -> Pjk_default
      | Pjk_abbreviation a ->
        let a = self#string ctx a in
        Pjk_abbreviation a
      | Pjk_mod (a, b) ->
        let a = self#jkind_annotation ctx a in
        let b = self#modes ctx b in
        Pjk_mod (a, b)
      | Pjk_with (a, b, c) ->
        let a = self#jkind_annotation ctx a in
        let b = self#core_type ctx b in
        let c = self#modalities ctx c in
        Pjk_with (a, b, c)
      | Pjk_kind_of a ->
        let a = self#core_type ctx a in
        Pjk_kind_of a
      | Pjk_product a ->
        let a = self#list self#jkind_annotation ctx a in
        Pjk_product a
      | Pjk_parens a ->
        let a = self#jkind_annotation_desc ctx a in
        Pjk_parens a

    method jkind_annotation : 'ctx -> jkind_annotation -> jkind_annotation =
      fun ctx { pjkind_loc; pjkind_desc; pjkind_tokens } ->
      let pjkind_loc = self#location ctx pjkind_loc in
      let pjkind_desc = self#jkind_annotation_desc ctx pjkind_desc in
      let pjkind_tokens = self#token_seq ctx pjkind_tokens in
      { pjkind_loc; pjkind_desc; pjkind_tokens }

    method use_file : 'ctx -> use_file -> use_file =
      fun ctx (a, b) ->
      let a = self#list self#toplevel_phrase ctx a in
      let b = self#token_seq ctx b in
      a, b

    method toplevel_phrase : 'ctx -> toplevel_phrase -> toplevel_phrase =
      fun ctx x ->
      match x with
      | Ptop_def a ->
        let a = self#structure ctx a in
        Ptop_def a
      | Ptop_dir a ->
        let a = self#toplevel_directive ctx a in
        Ptop_dir a
      | Ptop_lex a ->
        let a = self#lexer_directive ctx a in
        Ptop_lex a

    method toplevel_directive : 'ctx -> toplevel_directive -> toplevel_directive
      =
      fun ctx { pdir_name; pdir_arg; pdir_loc; pdir_tokens } ->
      let pdir_name = self#loc self#string ctx pdir_name in
      let pdir_arg = self#option self#directive_argument ctx pdir_arg in
      let pdir_loc = self#location ctx pdir_loc in
      let pdir_tokens = self#token_seq ctx pdir_tokens in
      { pdir_name; pdir_arg; pdir_loc; pdir_tokens }

    method directive_argument : 'ctx -> directive_argument -> directive_argument
      =
      fun ctx { pdira_desc; pdira_loc } ->
      let pdira_desc = self#directive_argument_desc ctx pdira_desc in
      let pdira_loc = self#location ctx pdira_loc in
      { pdira_desc; pdira_loc }

    method directive_argument_desc
      : 'ctx -> directive_argument_desc -> directive_argument_desc
      =
      fun ctx x ->
      match x with
      | Pdir_string a ->
        let a = self#string ctx a in
        Pdir_string a
      | Pdir_int (a, b) ->
        let a = self#string ctx a in
        let b = self#option self#char ctx b in
        Pdir_int (a, b)
      | Pdir_ident a ->
        let a = self#longident ctx a in
        Pdir_ident a
      | Pdir_bool a ->
        let a = self#bool ctx a in
        Pdir_bool a

    method syntax_directive : 'ctx -> syntax_directive -> syntax_directive =
      fun ctx { psyn_mode; psyn_toggle } ->
      let psyn_mode = self#loc self#string ctx psyn_mode in
      let psyn_toggle = self#bool ctx psyn_toggle in
      { psyn_mode; psyn_toggle }

    method lexer_directive_desc
      : 'ctx -> lexer_directive_desc -> lexer_directive_desc
      =
      fun ctx x ->
      match x with
      | Plex_syntax a ->
        let a = self#syntax_directive ctx a in
        Plex_syntax a

    method lexer_directive : 'ctx -> lexer_directive -> lexer_directive =
      fun ctx { plex_desc; plex_loc; plex_tokens } ->
      let plex_desc = self#lexer_directive_desc ctx plex_desc in
      let plex_loc = self#location ctx plex_loc in
      let plex_tokens = self#token_seq ctx plex_tokens in
      { plex_desc; plex_loc; plex_tokens }
  end

class virtual ['res] lift =
  object(self)
    method virtual record : (string * 'res) list -> 'res

    method virtual constr : string -> 'res list -> 'res

    method virtual tuple : 'res list -> 'res

    method virtual bool : bool -> 'res

    method virtual char : char -> 'res

    method virtual int : int -> 'res

    method virtual list : 'a. ('a -> 'res) -> 'a list -> 'res

    method virtual option : 'a. ('a -> 'res) -> 'a option -> 'res

    method virtual ref : 'a. ('a -> 'res) -> 'a ref -> 'res

    method virtual string : string -> 'res

    method virtual token : token -> 'res

    method position : position -> 'res =
      fun { pos_fname; pos_lnum; pos_bol; pos_cnum } ->
      let pos_fname = self#string pos_fname in
      let pos_lnum = self#int pos_lnum in
      let pos_bol = self#int pos_bol in
      let pos_cnum = self#int pos_cnum in
      self
      #
      record
        [ "pos_fname", pos_fname
        ; "pos_lnum", pos_lnum
        ; "pos_bol", pos_bol
        ; "pos_cnum", pos_cnum
        ]

    method location : location -> 'res =
      fun { loc_start; loc_end; loc_ghost } ->
      let loc_start = self#position loc_start in
      let loc_end = self#position loc_end in
      let loc_ghost = self#bool loc_ghost in
      self
      #
      record
        [ "loc_start", loc_start
        ; "loc_end", loc_end
        ; "loc_ghost", loc_ghost
        ]

    method longident_dotop_delims : longident_dotop_delims -> 'res =
      fun x ->
      match x with
      | Paren -> self#constr "Paren" []
      | Brace -> self#constr "Brace" []
      | Bracket -> self#constr "Bracket" []

    method longident_str_or_op : longident_str_or_op -> 'res =
      fun x ->
      match x with
      | Str a ->
        let a = self#string a in
        self#constr "Str" [ a ]
      | Str_trailing_hash a ->
        let a = self#string a in
        self#constr "Str_trailing_hash" [ a ]
      | Op a ->
        let a = self#string a in
        self#constr "Op" [ a ]
      | DotOp (a, b, c, d) ->
        let a = self#string a in
        let b = self#longident_dotop_delims b in
        let c = self#string c in
        let d = self#bool d in
        self#constr "DotOp" [ a; b; c; d ]

    method longident_lid_desc : longident_lid_desc -> 'res =
      fun x ->
      match x with
      | Lident a ->
        let a = self#longident_str_or_op a in
        self#constr "Lident" [ a ]
      | Ldot (a, b) ->
        let a = self#longident a in
        let b = self#longident_str_or_op b in
        self#constr "Ldot" [ a; b ]
      | Lapply (a, b) ->
        let a = self#longident a in
        let b = self#longident b in
        self#constr "Lapply" [ a; b ]

    method longident : longident -> 'res =
      fun { desc; tokens } ->
      let desc = self#longident_lid_desc desc in
      let tokens = self#token_seq tokens in
      self#record [ "desc", desc; "tokens", tokens ]

    method attachment : attachment -> 'res =
      fun x ->
      match x with
      | Before -> self#constr "Before" []
      | After -> self#constr "After" []
      | Floating -> self#constr "Floating" []

    method comment : comment -> 'res =
      fun { text; attachement; explicitely_inserted } ->
      let text = self#string text in
      let attachement = self#attachment attachement in
      let explicitely_inserted = self#ref self#bool explicitely_inserted in
      self
      #
      record
        [ "text", text
        ; "attachement", attachement
        ; "explicitely_inserted", explicitely_inserted
        ]

    method token_desc : token_desc -> 'res =
      fun x ->
      match x with
      | Token (a, b) ->
        let a = self#token a in
        let b = self#bool b in
        self#constr "Token" [ a; b ]
      | Comment a ->
        let a = self#comment a in
        self#constr "Comment" [ a ]
      | Child_node -> self#constr "Child_node" []

    method token_elt : token_elt -> 'res =
      fun { desc; pos } ->
      let desc = self#token_desc desc in
      let pos = self#position pos in
      self#record [ "desc", desc; "pos", pos ]

    method token_seq : token_seq -> 'res = self#list self#token_elt

    method rec_flag : rec_flag -> 'res =
      fun x ->
      match x with
      | Nonrecursive -> self#constr "Nonrecursive" []
      | Recursive -> self#constr "Recursive" []

    method direction_flag : direction_flag -> 'res =
      fun x ->
      match x with
      | Upto -> self#constr "Upto" []
      | Downto -> self#constr "Downto" []

    method private_flag : private_flag -> 'res =
      fun x ->
      match x with
      | Private -> self#constr "Private" []
      | Public -> self#constr "Public" []

    method mutable_flag : mutable_flag -> 'res =
      fun x ->
      match x with
      | Immutable -> self#constr "Immutable" []
      | Mutable -> self#constr "Mutable" []

    method virtual_flag : virtual_flag -> 'res =
      fun x ->
      match x with
      | Virtual -> self#constr "Virtual" []
      | Concrete -> self#constr "Concrete" []

    method override_flag : override_flag -> 'res =
      fun x ->
      match x with
      | Override -> self#constr "Override" []
      | Fresh -> self#constr "Fresh" []

    method closed_flag : closed_flag -> 'res =
      fun x ->
      match x with
      | Closed -> self#constr "Closed" []
      | Open -> self#constr "Open" []

    method label : label -> 'res = self#string

    method arg_label : arg_label -> 'res =
      fun x ->
      match x with
      | Nolabel -> self#constr "Nolabel" []
      | Labelled a ->
        let a = self#string a in
        self#constr "Labelled" [ a ]
      | Optional a ->
        let a = self#string a in
        self#constr "Optional" [ a ]

    method loc : 'a. ('a -> 'res) -> 'a loc -> 'res =
      fun _a { txt; loc } ->
      let txt = _a txt in
      let loc = self#location loc in
      self#record [ "txt", txt; "loc", loc ]

    method variance : variance -> 'res =
      fun x ->
      match x with
      | Covariant -> self#constr "Covariant" []
      | Contravariant -> self#constr "Contravariant" []
      | NoVariance -> self#constr "NoVariance" []

    method injectivity : injectivity -> 'res =
      fun x ->
      match x with
      | Injective -> self#constr "Injective" []
      | NoInjectivity -> self#constr "NoInjectivity" []

    method index_kind : index_kind -> 'res =
      fun x ->
      match x with
      | Index_int -> self#constr "Index_int" []
      | Index_unboxed_int64 -> self#constr "Index_unboxed_int64" []
      | Index_unboxed_int32 -> self#constr "Index_unboxed_int32" []
      | Index_unboxed_int16 -> self#constr "Index_unboxed_int16" []
      | Index_unboxed_int8 -> self#constr "Index_unboxed_int8" []
      | Index_unboxed_nativeint -> self#constr "Index_unboxed_nativeint" []

    method paren_kind : paren_kind -> 'res =
      fun x ->
      match x with
      | Paren -> self#constr "Paren" []
      | Brace -> self#constr "Brace" []
      | Bracket -> self#constr "Bracket" []

    method constant : constant -> 'res =
      fun x ->
      match x with
      | Pconst_integer (a, b, c) ->
        let a = self#option self#string a in
        let b = self#string b in
        let c = self#option self#char c in
        self#constr "Pconst_integer" [ a; b; c ]
      | Pconst_unboxed_integer (a, b, c) ->
        let a = self#option self#string a in
        let b = self#string b in
        let c = self#char c in
        self#constr "Pconst_unboxed_integer" [ a; b; c ]
      | Pconst_char (a, b) ->
        let a = self#char a in
        let b = self#string b in
        self#constr "Pconst_char" [ a; b ]
      | Pconst_untagged_char (a, b) ->
        let a = self#char a in
        let b = self#string b in
        self#constr "Pconst_untagged_char" [ a; b ]
      | Pconst_string (a, b, c) ->
        let a = self#string a in
        let b = self#location b in
        let c = self#option self#string c in
        self#constr "Pconst_string" [ a; b; c ]
      | Pconst_float (a, b, c) ->
        let a = self#option self#string a in
        let b = self#string b in
        let c = self#option self#char c in
        self#constr "Pconst_float" [ a; b; c ]
      | Pconst_unboxed_float (a, b, c) ->
        let a = self#option self#string a in
        let b = self#string b in
        let c = self#option self#char c in
        self#constr "Pconst_unboxed_float" [ a; b; c ]

    method modality : modality -> 'res =
      fun x ->
      match x with
      | Modality a ->
        let a = self#string a in
        self#constr "Modality" [ a ]

    method modalities : modalities -> 'res = self#list (self#loc self#modality)

    method mode : mode -> 'res =
      fun x ->
      match x with
      | Mode a ->
        let a = self#string a in
        self#constr "Mode" [ a ]

    method modes : modes -> 'res = self#list (self#loc self#mode)

    method include_kind : include_kind -> 'res =
      fun x ->
      match x with
      | Structure -> self#constr "Structure" []
      | Functor -> self#constr "Functor" []

    method attribute : attribute -> 'res =
      fun { attr_name; attr_payload; attr_loc; attr_tokens } ->
      let attr_name = self#loc (self#list self#string) attr_name in
      let attr_payload = self#payload attr_payload in
      let attr_loc = self#location attr_loc in
      let attr_tokens = self#token_seq attr_tokens in
      self
      #
      record
        [ "attr_name", attr_name
        ; "attr_payload", attr_payload
        ; "attr_loc", attr_loc
        ; "attr_tokens", attr_tokens
        ]

    method extension : extension -> 'res =
      fun (a, b, c) ->
      let a = self#loc (self#list self#string) a in
      let b = self#payload b in
      let c = self#token_seq c in
      self#tuple [ a; b; c ]

    method toplevel_extension : toplevel_extension -> 'res =
      fun { te_pre_doc; te_ext; te_attrs; te_post_doc } ->
      let te_pre_doc = self#option self#string te_pre_doc in
      let te_ext = self#extension te_ext in
      let te_attrs = self#attributes te_attrs in
      let te_post_doc = self#option self#string te_post_doc in
      self
      #
      record
        [ "te_pre_doc", te_pre_doc
        ; "te_ext", te_ext
        ; "te_attrs", te_attrs
        ; "te_post_doc", te_post_doc
        ]

    method attributes : attributes -> 'res = self#list self#attribute

    method payload : payload -> 'res =
      fun x ->
      match x with
      | PStr a ->
        let a = self#structure a in
        self#constr "PStr" [ a ]
      | PSig a ->
        let a = self#signature a in
        self#constr "PSig" [ a ]
      | PTyp a ->
        let a = self#core_type a in
        self#constr "PTyp" [ a ]
      | PPat (a, b) ->
        let a = self#pattern a in
        let b = self#option self#expression b in
        self#constr "PPat" [ a; b ]
      | PString (a, b) ->
        let a = self#string a in
        let b = self#string b in
        self#constr "PString" [ a; b ]

    method ext_attribute : ext_attribute -> 'res =
      fun { pea_ext; pea_attrs } ->
      let pea_ext = self#option (self#loc (self#list self#string)) pea_ext in
      let pea_attrs = self#attributes pea_attrs in
      self#record [ "pea_ext", pea_ext; "pea_attrs", pea_attrs ]

    method core_type : core_type -> 'res =
      fun { ptyp_desc; ptyp_loc; ptyp_attributes; ptyp_tokens } ->
      let ptyp_desc = self#core_type_desc ptyp_desc in
      let ptyp_loc = self#location ptyp_loc in
      let ptyp_attributes = self#attributes ptyp_attributes in
      let ptyp_tokens = self#token_seq ptyp_tokens in
      self
      #
      record
        [ "ptyp_desc", ptyp_desc
        ; "ptyp_loc", ptyp_loc
        ; "ptyp_attributes", ptyp_attributes
        ; "ptyp_tokens", ptyp_tokens
        ]

    method arrow_arg : arrow_arg -> 'res =
      fun
        { aa_lbl
        ; aa_legacy_modes
        ; aa_type
        ; aa_modes
        ; aa_doc
        ; aa_loc
        ; aa_tokens
        }
        ->
      let aa_lbl = self#arg_label aa_lbl in
      let aa_legacy_modes = self#modes aa_legacy_modes in
      let aa_type = self#core_type aa_type in
      let aa_modes = self#modes aa_modes in
      let aa_doc = self#option self#string aa_doc in
      let aa_loc = self#location aa_loc in
      let aa_tokens = self#token_seq aa_tokens in
      self
      #
      record
        [ "aa_lbl", aa_lbl
        ; "aa_legacy_modes", aa_legacy_modes
        ; "aa_type", aa_type
        ; "aa_modes", aa_modes
        ; "aa_doc", aa_doc
        ; "aa_loc", aa_loc
        ; "aa_tokens", aa_tokens
        ]

    method core_type_desc : core_type_desc -> 'res =
      fun x ->
      match x with
      | Ptyp_any a ->
        let a = self#option self#jkind_annotation a in
        self#constr "Ptyp_any" [ a ]
      | Ptyp_var (a, b) ->
        let a = self#string a in
        let b = self#option self#jkind_annotation b in
        self#constr "Ptyp_var" [ a; b ]
      | Ptyp_arrow { domain; codom_legacy_modes; codom_type; codom_modes } ->
        let domain = self#arrow_arg domain in
        let codom_legacy_modes = self#modes codom_legacy_modes in
        let codom_type = self#core_type codom_type in
        let codom_modes = self#modes codom_modes in
        self
        #
        constr
          "Ptyp_arrow"
          [ self
            #
            record
              [ "domain", domain
              ; "codom_legacy_modes", codom_legacy_modes
              ; "codom_type", codom_type
              ; "codom_modes", codom_modes
              ]
          ]
      | Ptyp_tuple a ->
        let a =
          self
          #
          list
            (fun (a, b) ->
              let a = self#option self#string a in
              let b = self#core_type b in
              self#tuple [ a; b ])
            a
        in
        self#constr "Ptyp_tuple" [ a ]
      | Ptyp_unboxed_tuple a ->
        let a =
          self
          #
          list
            (fun (a, b) ->
              let a = self#option self#string a in
              let b = self#core_type b in
              self#tuple [ a; b ])
            a
        in
        self#constr "Ptyp_unboxed_tuple" [ a ]
      | Ptyp_constr (a, b) ->
        let a = self#list self#core_type a in
        let b = self#loc self#longident b in
        self#constr "Ptyp_constr" [ a; b ]
      | Ptyp_object (a, b) ->
        let a = self#list self#object_field a in
        let b = self#closed_flag b in
        self#constr "Ptyp_object" [ a; b ]
      | Ptyp_class (a, b) ->
        let a = self#loc self#longident a in
        let b = self#list self#core_type b in
        self#constr "Ptyp_class" [ a; b ]
      | Ptyp_alias (a, b, c) ->
        let a = self#core_type a in
        let b = self#option (self#loc self#string) b in
        let c = self#option self#jkind_annotation c in
        self#constr "Ptyp_alias" [ a; b; c ]
      | Ptyp_variant (a, b, c) ->
        let a = self#list self#row_field a in
        let b = self#closed_flag b in
        let c = self#option (self#list self#label) c in
        self#constr "Ptyp_variant" [ a; b; c ]
      | Ptyp_poly (a, b) ->
        let a =
          self
          #
          list
            (fun (a, b) ->
              let a = self#loc self#string a in
              let b = self#option self#jkind_annotation b in
              self#tuple [ a; b ])
            a
        in
        let b = self#core_type b in
        self#constr "Ptyp_poly" [ a; b ]
      | Ptyp_package (a, b) ->
        let a = self#ext_attribute a in
        let b = self#package_type b in
        self#constr "Ptyp_package" [ a; b ]
      | Ptyp_open (a, b) ->
        let a = self#loc self#longident a in
        let b = self#core_type b in
        self#constr "Ptyp_open" [ a; b ]
      | Ptyp_quote a ->
        let a = self#core_type a in
        self#constr "Ptyp_quote" [ a ]
      | Ptyp_splice a ->
        let a = self#core_type a in
        self#constr "Ptyp_splice" [ a ]
      | Ptyp_of_kind a ->
        let a = self#jkind_annotation a in
        self#constr "Ptyp_of_kind" [ a ]
      | Ptyp_extension a ->
        let a = self#extension a in
        self#constr "Ptyp_extension" [ a ]
      | Ptyp_parens a ->
        let a = self#core_type a in
        self#constr "Ptyp_parens" [ a ]

    method package_type : package_type -> 'res = self#module_type

    method row_field : row_field -> 'res =
      fun { prf_desc; prf_loc; prf_attributes; prf_doc; prf_tokens } ->
      let prf_desc = self#row_field_desc prf_desc in
      let prf_loc = self#location prf_loc in
      let prf_attributes = self#attributes prf_attributes in
      let prf_doc = self#option self#string prf_doc in
      let prf_tokens = self#token_seq prf_tokens in
      self
      #
      record
        [ "prf_desc", prf_desc
        ; "prf_loc", prf_loc
        ; "prf_attributes", prf_attributes
        ; "prf_doc", prf_doc
        ; "prf_tokens", prf_tokens
        ]

    method row_field_desc : row_field_desc -> 'res =
      fun x ->
      match x with
      | Rtag (a, b, c) ->
        let a = self#loc self#string a in
        let b = self#bool b in
        let c = self#list self#core_type c in
        self#constr "Rtag" [ a; b; c ]
      | Rinherit a ->
        let a = self#core_type a in
        self#constr "Rinherit" [ a ]

    method object_field : object_field -> 'res =
      fun { pof_desc; pof_loc; pof_attributes; pof_doc; pof_tokens } ->
      let pof_desc = self#object_field_desc pof_desc in
      let pof_loc = self#location pof_loc in
      let pof_attributes = self#attributes pof_attributes in
      let pof_doc = self#option self#string pof_doc in
      let pof_tokens = self#token_seq pof_tokens in
      self
      #
      record
        [ "pof_desc", pof_desc
        ; "pof_loc", pof_loc
        ; "pof_attributes", pof_attributes
        ; "pof_doc", pof_doc
        ; "pof_tokens", pof_tokens
        ]

    method object_field_desc : object_field_desc -> 'res =
      fun x ->
      match x with
      | Otag (a, b) ->
        let a = self#loc self#string a in
        let b = self#core_type b in
        self#constr "Otag" [ a; b ]
      | Oinherit a ->
        let a = self#core_type a in
        self#constr "Oinherit" [ a ]

    method pattern : pattern -> 'res =
      fun
        { ppat_ext_attr; ppat_desc; ppat_loc; ppat_attributes; ppat_tokens } ->
      let ppat_ext_attr = self#ext_attribute ppat_ext_attr in
      let ppat_desc = self#pattern_desc ppat_desc in
      let ppat_loc = self#location ppat_loc in
      let ppat_attributes = self#attributes ppat_attributes in
      let ppat_tokens = self#token_seq ppat_tokens in
      self
      #
      record
        [ "ppat_ext_attr", ppat_ext_attr
        ; "ppat_desc", ppat_desc
        ; "ppat_loc", ppat_loc
        ; "ppat_attributes", ppat_attributes
        ; "ppat_tokens", ppat_tokens
        ]

    method pattern_desc : pattern_desc -> 'res =
      fun x ->
      match x with
      | Ppat_any -> self#constr "Ppat_any" []
      | Ppat_var a ->
        let a = self#loc self#longident_str_or_op a in
        self#constr "Ppat_var" [ a ]
      | Ppat_alias (a, b) ->
        let a = self#pattern a in
        let b = self#loc self#longident_str_or_op b in
        self#constr "Ppat_alias" [ a; b ]
      | Ppat_constant a ->
        let a = self#constant a in
        self#constr "Ppat_constant" [ a ]
      | Ppat_interval (a, b) ->
        let a = self#constant a in
        let b = self#constant b in
        self#constr "Ppat_interval" [ a; b ]
      | Ppat_tuple (a, b) ->
        let a = self#list (self#argument self#pattern) a in
        let b = self#closed_flag b in
        self#constr "Ppat_tuple" [ a; b ]
      | Ppat_unboxed_tuple (a, b) ->
        let a = self#list (self#argument self#pattern) a in
        let b = self#closed_flag b in
        self#constr "Ppat_unboxed_tuple" [ a; b ]
      | Ppat_construct (a, b) ->
        let a = self#loc self#longident a in
        let b =
          self
          #
          option
            (fun (a, b) ->
              let a =
                self
                #
                list
                  (fun (a, b) ->
                    let a = self#loc self#string a in
                    let b = self#option self#jkind_annotation b in
                    self#tuple [ a; b ])
                  a
              in
              let b = self#pattern b in
              self#tuple [ a; b ])
            b
        in
        self#constr "Ppat_construct" [ a; b ]
      | Ppat_variant (a, b) ->
        let a = self#label a in
        let b = self#option self#pattern b in
        self#constr "Ppat_variant" [ a; b ]
      | Ppat_record (a, b) ->
        let a = self#list (self#record_field self#pattern) a in
        let b = self#closed_flag b in
        self#constr "Ppat_record" [ a; b ]
      | Ppat_record_unboxed_product (a, b) ->
        let a = self#list (self#record_field self#pattern) a in
        let b = self#closed_flag b in
        self#constr "Ppat_record_unboxed_product" [ a; b ]
      | Ppat_array (a, b) ->
        let a = self#mutable_flag a in
        let b = self#list self#pattern b in
        self#constr "Ppat_array" [ a; b ]
      | Ppat_or (a, b) ->
        let a = self#pattern a in
        let b = self#pattern b in
        self#constr "Ppat_or" [ a; b ]
      | Ppat_constraint (a, b, c) ->
        let a = self#pattern a in
        let b = self#option self#core_type b in
        let c = self#modes c in
        self#constr "Ppat_constraint" [ a; b; c ]
      | Ppat_type a ->
        let a = self#loc self#longident a in
        self#constr "Ppat_type" [ a ]
      | Ppat_lazy a ->
        let a = self#pattern a in
        self#constr "Ppat_lazy" [ a ]
      | Ppat_unpack (a, b) ->
        let a = self#loc (self#option self#string) a in
        let b = self#option self#package_type b in
        self#constr "Ppat_unpack" [ a; b ]
      | Ppat_exception a ->
        let a = self#pattern a in
        self#constr "Ppat_exception" [ a ]
      | Ppat_extension a ->
        let a = self#extension a in
        self#constr "Ppat_extension" [ a ]
      | Ppat_open (a, b) ->
        let a = self#loc self#longident a in
        let b = self#pattern b in
        self#constr "Ppat_open" [ a; b ]
      | Ppat_parens { pat; optional } ->
        let pat = self#pattern pat in
        let optional = self#bool optional in
        self
        #
        constr
          "Ppat_parens"
          [ self#record [ "pat", pat; "optional", optional ] ]
      | Ppat_list a ->
        let a = self#list self#pattern a in
        self#constr "Ppat_list" [ a ]
      | Ppat_cons (a, b) ->
        let a = self#pattern a in
        let b = self#pattern b in
        self#constr "Ppat_cons" [ a; b ]

    method expression : expression -> 'res =
      fun
        { pexp_ext_attr; pexp_desc; pexp_loc; pexp_attributes; pexp_tokens } ->
      let pexp_ext_attr = self#ext_attribute pexp_ext_attr in
      let pexp_desc = self#expression_desc pexp_desc in
      let pexp_loc = self#location pexp_loc in
      let pexp_attributes = self#attributes pexp_attributes in
      let pexp_tokens = self#token_seq pexp_tokens in
      self
      #
      record
        [ "pexp_ext_attr", pexp_ext_attr
        ; "pexp_desc", pexp_desc
        ; "pexp_loc", pexp_loc
        ; "pexp_attributes", pexp_attributes
        ; "pexp_tokens", pexp_tokens
        ]

    method expression_desc : expression_desc -> 'res =
      fun x ->
      match x with
      | Pexp_ident a ->
        let a = self#loc self#longident a in
        self#constr "Pexp_ident" [ a ]
      | Pexp_constant a ->
        let a = self#constant a in
        self#constr "Pexp_constant" [ a ]
      | Pexp_let (a, b, c, d) ->
        let a = self#mutable_flag a in
        let b = self#rec_flag b in
        let c = self#list self#value_binding c in
        let d = self#expression d in
        self#constr "Pexp_let" [ a; b; c; d ]
      | Pexp_function (a, b, c) ->
        let a = self#list self#function_param a in
        let b = self#function_constraint b in
        let c = self#function_body c in
        self#constr "Pexp_function" [ a; b; c ]
      | Pexp_prefix_apply (a, b) ->
        let a = self#expression a in
        let b = self#expression b in
        self#constr "Pexp_prefix_apply" [ a; b ]
      | Pexp_add_or_sub (a, b) ->
        let a = self#string a in
        let b = self#expression b in
        self#constr "Pexp_add_or_sub" [ a; b ]
      | Pexp_infix_apply { arg1; op; arg2 } ->
        let arg1 = self#expression arg1 in
        let op = self#expression op in
        let arg2 = self#expression arg2 in
        self
        #
        constr
          "Pexp_infix_apply"
          [ self#record [ "arg1", arg1; "op", op; "arg2", arg2 ] ]
      | Pexp_apply (a, b) ->
        let a = self#expression a in
        let b = self#list (self#argument self#expression) b in
        self#constr "Pexp_apply" [ a; b ]
      | Pexp_match (a, b) ->
        let a = self#expression a in
        let b = self#list self#case b in
        self#constr "Pexp_match" [ a; b ]
      | Pexp_try (a, b) ->
        let a = self#expression a in
        let b = self#list self#case b in
        self#constr "Pexp_try" [ a; b ]
      | Pexp_tuple a ->
        let a = self#list (self#argument self#expression) a in
        self#constr "Pexp_tuple" [ a ]
      | Pexp_unboxed_tuple a ->
        let a = self#list (self#argument self#expression) a in
        self#constr "Pexp_unboxed_tuple" [ a ]
      | Pexp_construct (a, b) ->
        let a = self#loc self#longident a in
        let b = self#option self#expression b in
        self#constr "Pexp_construct" [ a; b ]
      | Pexp_variant (a, b) ->
        let a = self#label a in
        let b = self#option self#expression b in
        self#constr "Pexp_variant" [ a; b ]
      | Pexp_record (a, b) ->
        let a = self#option self#expression a in
        let b = self#list (self#record_field self#expression) b in
        self#constr "Pexp_record" [ a; b ]
      | Pexp_record_unboxed_product (a, b) ->
        let a = self#option self#expression a in
        let b = self#list (self#record_field self#expression) b in
        self#constr "Pexp_record_unboxed_product" [ a; b ]
      | Pexp_field (a, b) ->
        let a = self#expression a in
        let b = self#loc self#longident b in
        self#constr "Pexp_field" [ a; b ]
      | Pexp_unboxed_field (a, b) ->
        let a = self#expression a in
        let b = self#loc self#longident b in
        self#constr "Pexp_unboxed_field" [ a; b ]
      | Pexp_setfield (a, b, c) ->
        let a = self#expression a in
        let b = self#loc self#longident b in
        let c = self#expression c in
        self#constr "Pexp_setfield" [ a; b; c ]
      | Pexp_array (a, b) ->
        let a = self#mutable_flag a in
        let b = self#list self#expression b in
        self#constr "Pexp_array" [ a; b ]
      | Pexp_idx (a, b) ->
        let a = self#block_access a in
        let b = self#list self#unboxed_access b in
        self#constr "Pexp_idx" [ a; b ]
      | Pexp_ifthenelse (a, b, c) ->
        let a = self#expression a in
        let b = self#expression b in
        let c = self#option self#expression c in
        self#constr "Pexp_ifthenelse" [ a; b; c ]
      | Pexp_sequence (a, b) ->
        let a = self#expression a in
        let b = self#expression b in
        self#constr "Pexp_sequence" [ a; b ]
      | Pexp_seq_empty a ->
        let a = self#expression a in
        self#constr "Pexp_seq_empty" [ a ]
      | Pexp_while (a, b) ->
        let a = self#expression a in
        let b = self#expression b in
        self#constr "Pexp_while" [ a; b ]
      | Pexp_for (a, b, c, d, e) ->
        let a = self#pattern a in
        let b = self#expression b in
        let c = self#expression c in
        let d = self#direction_flag d in
        let e = self#expression e in
        self#constr "Pexp_for" [ a; b; c; d; e ]
      | Pexp_constraint (a, b, c) ->
        let a = self#expression a in
        let b = self#option self#core_type b in
        let c = self#modes c in
        self#constr "Pexp_constraint" [ a; b; c ]
      | Pexp_coerce (a, b, c) ->
        let a = self#expression a in
        let b = self#option self#core_type b in
        let c = self#core_type c in
        self#constr "Pexp_coerce" [ a; b; c ]
      | Pexp_send (a, b) ->
        let a = self#expression a in
        let b = self#loc self#string b in
        self#constr "Pexp_send" [ a; b ]
      | Pexp_new a ->
        let a = self#loc self#longident a in
        self#constr "Pexp_new" [ a ]
      | Pexp_setvar (a, b) ->
        let a = self#loc self#string a in
        let b = self#expression b in
        self#constr "Pexp_setvar" [ a; b ]
      | Pexp_override a ->
        let a =
          self
          #
          list
            (fun (a, b) ->
              let a = self#loc self#string a in
              let b = self#option self#expression b in
              self#tuple [ a; b ])
            a
        in
        self#constr "Pexp_override" [ a ]
      | Pexp_letmodule (a, b) ->
        let a = self#module_binding a in
        let b = self#expression b in
        self#constr "Pexp_letmodule" [ a; b ]
      | Pexp_letexception (a, b) ->
        let a = self#extension_constructor a in
        let b = self#expression b in
        self#constr "Pexp_letexception" [ a; b ]
      | Pexp_assert a ->
        let a = self#expression a in
        self#constr "Pexp_assert" [ a ]
      | Pexp_lazy a ->
        let a = self#expression a in
        self#constr "Pexp_lazy" [ a ]
      | Pexp_object a ->
        let a = self#class_structure a in
        self#constr "Pexp_object" [ a ]
      | Pexp_pack (a, b) ->
        let a = self#module_expr a in
        let b = self#option self#package_type b in
        self#constr "Pexp_pack" [ a; b ]
      | Pexp_dot_open (a, b) ->
        let a = self#loc self#longident a in
        let b = self#expression b in
        self#constr "Pexp_dot_open" [ a; b ]
      | Pexp_let_open (a, b) ->
        let a = self#open_declaration a in
        let b = self#expression b in
        self#constr "Pexp_let_open" [ a; b ]
      | Pexp_letop a ->
        let a = self#letop a in
        self#constr "Pexp_letop" [ a ]
      | Pexp_extension a ->
        let a = self#extension a in
        self#constr "Pexp_extension" [ a ]
      | Pexp_unreachable -> self#constr "Pexp_unreachable" []
      | Pexp_stack a ->
        let a = self#expression a in
        self#constr "Pexp_stack" [ a ]
      | Pexp_comprehension a ->
        let a = self#comprehension_expression a in
        self#constr "Pexp_comprehension" [ a ]
      | Pexp_overwrite (a, b) ->
        let a = self#expression a in
        let b = self#expression b in
        self#constr "Pexp_overwrite" [ a; b ]
      | Pexp_quote a ->
        let a = self#expression a in
        self#constr "Pexp_quote" [ a ]
      | Pexp_splice a ->
        let a = self#expression a in
        self#constr "Pexp_splice" [ a ]
      | Pexp_hole -> self#constr "Pexp_hole" []
      | Pexp_index_op { kind; op; seq; indices; assign } ->
        let kind = self#paren_kind kind in
        let op =
          self
          #
          option
            (fun (a, b) ->
              let a = self#option self#longident a in
              let b = self#string b in
              self#tuple [ a; b ])
            op
        in
        let seq = self#expression seq in
        let indices = self#list self#expression indices in
        let assign = self#option self#expression assign in
        self
        #
        constr
          "Pexp_index_op"
          [ self
            #
            record
              [ "kind", kind
              ; "op", op
              ; "seq", seq
              ; "indices", indices
              ; "assign", assign
              ]
          ]
      | Pexp_parens { exp; optional } ->
        let exp = self#expression exp in
        let optional = self#bool optional in
        self
        #
        constr
          "Pexp_parens"
          [ self#record [ "exp", exp; "optional", optional ] ]
      | Pexp_begin_end a ->
        let a = self#option self#expression a in
        self#constr "Pexp_begin_end" [ a ]
      | Pexp_list a ->
        let a = self#list self#expression a in
        self#constr "Pexp_list" [ a ]
      | Pexp_cons (a, b) ->
        let a = self#expression a in
        let b = self#expression b in
        self#constr "Pexp_cons" [ a; b ]
      | Pexp_exclave a ->
        let a = self#expression a in
        self#constr "Pexp_exclave" [ a ]
      | Pexp_mode_legacy (a, b) ->
        let a = self#loc self#mode a in
        let b = self#expression b in
        self#constr "Pexp_mode_legacy" [ a; b ]

    method record_field : 'a. ('a -> 'res) -> 'a record_field -> 'res =
      fun _a { field_name; typ; value } ->
      let field_name = self#loc self#longident field_name in
      let typ = self#option self#type_constraint typ in
      let value = self#option _a value in
      self#record [ "field_name", field_name; "typ", typ; "value", value ]

    method case : case -> 'res =
      fun { pc_lhs; pc_guard; pc_rhs; pc_tokens } ->
      let pc_lhs = self#pattern pc_lhs in
      let pc_guard = self#option self#expression pc_guard in
      let pc_rhs = self#expression pc_rhs in
      let pc_tokens = self#token_seq pc_tokens in
      self
      #
      record
        [ "pc_lhs", pc_lhs
        ; "pc_guard", pc_guard
        ; "pc_rhs", pc_rhs
        ; "pc_tokens", pc_tokens
        ]

    method letop : letop -> 'res =
      fun { let_; ands; body } ->
      let let_ = self#binding_op let_ in
      let ands = self#list self#binding_op ands in
      let body = self#expression body in
      self#record [ "let_", let_; "ands", ands; "body", body ]

    method binding_op : binding_op -> 'res =
      fun { pbop_op; pbop_binding; pbop_loc } ->
      let pbop_op = self#loc self#string pbop_op in
      let pbop_binding = self#value_binding pbop_binding in
      let pbop_loc = self#location pbop_loc in
      self
      #
      record
        [ "pbop_op", pbop_op
        ; "pbop_binding", pbop_binding
        ; "pbop_loc", pbop_loc
        ]

    method argument_desc : 'a. ('a -> 'res) -> 'a argument_desc -> 'res =
      fun _a x ->
      match x with
      | Parg_unlabelled { legacy_modes; arg; typ_constraint; modes } ->
        let legacy_modes = self#modes legacy_modes in
        let arg = _a arg in
        let typ_constraint = self#option self#type_constraint typ_constraint in
        let modes = self#modes modes in
        self
        #
        constr
          "Parg_unlabelled"
          [ self
            #
            record
              [ "legacy_modes", legacy_modes
              ; "arg", arg
              ; "typ_constraint", typ_constraint
              ; "modes", modes
              ]
          ]
      | Parg_labelled
          { optional
          ; legacy_modes
          ; name
          ; maybe_punned
          ; typ_constraint
          ; modes
          ; default
          } ->
        let optional = self#bool optional in
        let legacy_modes = self#modes legacy_modes in
        let name = self#string name in
        let maybe_punned = self#option _a maybe_punned in
        let typ_constraint = self#option self#type_constraint typ_constraint in
        let modes = self#modes modes in
        let default = self#option self#expression default in
        self
        #
        constr
          "Parg_labelled"
          [ self
            #
            record
              [ "optional", optional
              ; "legacy_modes", legacy_modes
              ; "name", name
              ; "maybe_punned", maybe_punned
              ; "typ_constraint", typ_constraint
              ; "modes", modes
              ; "default", default
              ]
          ]

    method argument : 'a. ('a -> 'res) -> 'a argument -> 'res =
      fun _a { parg_desc; parg_tokens } ->
      let parg_desc = self#argument_desc _a parg_desc in
      let parg_tokens = self#token_seq parg_tokens in
      self#record [ "parg_desc", parg_desc; "parg_tokens", parg_tokens ]

    method function_param_desc : function_param_desc -> 'res =
      fun x ->
      match x with
      | Pparam_val a ->
        let a = self#argument self#pattern a in
        self#constr "Pparam_val" [ a ]
      | Pparam_newtype (a, b) ->
        let a = self#loc self#string a in
        let b = self#option self#jkind_annotation b in
        self#constr "Pparam_newtype" [ a; b ]
      | Pparam_newtypes a ->
        let a =
          self
          #
          list
            (fun (a, b) ->
              let a = self#loc self#string a in
              let b = self#option self#jkind_annotation b in
              self#tuple [ a; b ])
            a
        in
        self#constr "Pparam_newtypes" [ a ]

    method function_param : function_param -> 'res =
      fun { pparam_loc; pparam_desc } ->
      let pparam_loc = self#location pparam_loc in
      let pparam_desc = self#function_param_desc pparam_desc in
      self#record [ "pparam_loc", pparam_loc; "pparam_desc", pparam_desc ]

    method function_body : function_body -> 'res =
      fun { pfb_desc; pfb_loc; pfb_tokens } ->
      let pfb_desc = self#function_body_desc pfb_desc in
      let pfb_loc = self#location pfb_loc in
      let pfb_tokens = self#token_seq pfb_tokens in
      self
      #
      record
        [ "pfb_desc", pfb_desc
        ; "pfb_loc", pfb_loc
        ; "pfb_tokens", pfb_tokens
        ]

    method function_body_desc : function_body_desc -> 'res =
      fun x ->
      match x with
      | Pfunction_body a ->
        let a = self#expression a in
        self#constr "Pfunction_body" [ a ]
      | Pfunction_cases (a, b) ->
        let a = self#list self#case a in
        let b = self#ext_attribute b in
        self#constr "Pfunction_cases" [ a; b ]

    method type_constraint : type_constraint -> 'res =
      fun x ->
      match x with
      | Pconstraint a ->
        let a = self#core_type a in
        self#constr "Pconstraint" [ a ]
      | Pcoerce (a, b) ->
        let a = self#option self#core_type a in
        let b = self#core_type b in
        self#constr "Pcoerce" [ a; b ]

    method function_constraint : function_constraint -> 'res =
      fun { ret_mode_annotations; ret_type_constraint } ->
      let ret_mode_annotations = self#modes ret_mode_annotations in
      let ret_type_constraint =
        self#option self#type_constraint ret_type_constraint
      in
      self
      #
      record
        [ "ret_mode_annotations", ret_mode_annotations
        ; "ret_type_constraint", ret_type_constraint
        ]

    method block_access : block_access -> 'res =
      fun x ->
      match x with
      | Baccess_field a ->
        let a = self#loc self#longident a in
        self#constr "Baccess_field" [ a ]
      | Baccess_array (a, b, c) ->
        let a = self#mutable_flag a in
        let b = self#index_kind b in
        let c = self#expression c in
        self#constr "Baccess_array" [ a; b; c ]
      | Baccess_block (a, b) ->
        let a = self#mutable_flag a in
        let b = self#expression b in
        self#constr "Baccess_block" [ a; b ]

    method unboxed_access : unboxed_access -> 'res =
      fun x ->
      match x with
      | Uaccess_unboxed_field a ->
        let a = self#loc self#longident a in
        self#constr "Uaccess_unboxed_field" [ a ]

    method comprehension_iterator : comprehension_iterator -> 'res =
      fun x ->
      match x with
      | Pcomp_range { start; stop; direction } ->
        let start = self#expression start in
        let stop = self#expression stop in
        let direction = self#direction_flag direction in
        self
        #
        constr
          "Pcomp_range"
          [ self
            #
            record
              [ "start", start; "stop", stop; "direction", direction ]
          ]
      | Pcomp_in a ->
        let a = self#expression a in
        self#constr "Pcomp_in" [ a ]

    method comprehension_clause_binding : comprehension_clause_binding -> 'res =
      fun
        { pcomp_cb_mode
        ; pcomp_cb_pattern
        ; pcomp_cb_iterator
        ; pcomp_cb_attributes
        ; pcomp_cb_tokens
        }
        ->
      let pcomp_cb_mode = self#option (self#loc self#mode) pcomp_cb_mode in
      let pcomp_cb_pattern = self#pattern pcomp_cb_pattern in
      let pcomp_cb_iterator = self#comprehension_iterator pcomp_cb_iterator in
      let pcomp_cb_attributes = self#attributes pcomp_cb_attributes in
      let pcomp_cb_tokens = self#token_seq pcomp_cb_tokens in
      self
      #
      record
        [ "pcomp_cb_mode", pcomp_cb_mode
        ; "pcomp_cb_pattern", pcomp_cb_pattern
        ; "pcomp_cb_iterator", pcomp_cb_iterator
        ; "pcomp_cb_attributes", pcomp_cb_attributes
        ; "pcomp_cb_tokens", pcomp_cb_tokens
        ]

    method comprehension_clause : comprehension_clause -> 'res =
      fun x ->
      match x with
      | Pcomp_for a ->
        let a = self#list self#comprehension_clause_binding a in
        self#constr "Pcomp_for" [ a ]
      | Pcomp_when a ->
        let a = self#expression a in
        self#constr "Pcomp_when" [ a ]

    method comprehension : comprehension -> 'res =
      fun { pcomp_body; pcomp_clauses; pcomp_tokens } ->
      let pcomp_body = self#expression pcomp_body in
      let pcomp_clauses = self#list self#comprehension_clause pcomp_clauses in
      let pcomp_tokens = self#token_seq pcomp_tokens in
      self
      #
      record
        [ "pcomp_body", pcomp_body
        ; "pcomp_clauses", pcomp_clauses
        ; "pcomp_tokens", pcomp_tokens
        ]

    method comprehension_expression : comprehension_expression -> 'res =
      fun x ->
      match x with
      | Pcomp_list_comprehension a ->
        let a = self#comprehension a in
        self#constr "Pcomp_list_comprehension" [ a ]
      | Pcomp_array_comprehension (a, b) ->
        let a = self#mutable_flag a in
        let b = self#comprehension b in
        self#constr "Pcomp_array_comprehension" [ a; b ]

    method value_description : value_description -> 'res =
      fun
        { pval_pre_doc
        ; pval_ext_attrs
        ; pval_name
        ; pval_type
        ; pval_modalities
        ; pval_prim
        ; pval_attributes
        ; pval_post_doc
        ; pval_loc
        ; pval_tokens
        }
        ->
      let pval_pre_doc = self#option self#string pval_pre_doc in
      let pval_ext_attrs = self#ext_attribute pval_ext_attrs in
      let pval_name = self#loc self#longident_str_or_op pval_name in
      let pval_type = self#core_type pval_type in
      let pval_modalities = self#modalities pval_modalities in
      let pval_prim = self#list self#string pval_prim in
      let pval_attributes = self#attributes pval_attributes in
      let pval_post_doc = self#option self#string pval_post_doc in
      let pval_loc = self#location pval_loc in
      let pval_tokens = self#token_seq pval_tokens in
      self
      #
      record
        [ "pval_pre_doc", pval_pre_doc
        ; "pval_ext_attrs", pval_ext_attrs
        ; "pval_name", pval_name
        ; "pval_type", pval_type
        ; "pval_modalities", pval_modalities
        ; "pval_prim", pval_prim
        ; "pval_attributes", pval_attributes
        ; "pval_post_doc", pval_post_doc
        ; "pval_loc", pval_loc
        ; "pval_tokens", pval_tokens
        ]

    method ptype_param : ptype_param -> 'res =
      fun { ptp_typ; ptp_infos; ptp_tokens } ->
      let ptp_typ = self#core_type ptp_typ in
      let ptp_infos =
        (fun (a, b) ->
          let a = self#variance a in
          let b = self#injectivity b in
          self#tuple [ a; b ])
          ptp_infos
      in
      let ptp_tokens = self#token_seq ptp_tokens in
      self
      #
      record
        [ "ptp_typ", ptp_typ
        ; "ptp_infos", ptp_infos
        ; "ptp_tokens", ptp_tokens
        ]

    method ptype_params : ptype_params -> 'res = self#list self#ptype_param

    method ptype_constraint : ptype_constraint -> 'res =
      fun (a, b, c) ->
      let a = self#core_type a in
      let b = self#core_type b in
      let c = self#location c in
      self#tuple [ a; b; c ]

    method type_declaration : type_declaration -> 'res =
      fun
        { ptype_pre_text
        ; ptype_pre_doc
        ; ptype_ext_attrs
        ; ptype_name
        ; ptype_params
        ; ptype_jkind_annotation
        ; ptype_private
        ; ptype_manifest
        ; ptype_kind
        ; ptype_cstrs
        ; ptype_attributes
        ; ptype_post_doc
        ; ptype_loc
        ; ptype_tokens
        }
        ->
      let ptype_pre_text = self#list self#string ptype_pre_text in
      let ptype_pre_doc = self#option self#string ptype_pre_doc in
      let ptype_ext_attrs = self#ext_attribute ptype_ext_attrs in
      let ptype_name = self#loc self#string ptype_name in
      let ptype_params = self#ptype_params ptype_params in
      let ptype_jkind_annotation =
        self#option self#jkind_annotation ptype_jkind_annotation
      in
      let ptype_private = self#private_flag ptype_private in
      let ptype_manifest = self#option self#core_type ptype_manifest in
      let ptype_kind = self#type_kind ptype_kind in
      let ptype_cstrs = self#list self#ptype_constraint ptype_cstrs in
      let ptype_attributes = self#attributes ptype_attributes in
      let ptype_post_doc = self#option self#string ptype_post_doc in
      let ptype_loc = self#location ptype_loc in
      let ptype_tokens = self#token_seq ptype_tokens in
      self
      #
      record
        [ "ptype_pre_text", ptype_pre_text
        ; "ptype_pre_doc", ptype_pre_doc
        ; "ptype_ext_attrs", ptype_ext_attrs
        ; "ptype_name", ptype_name
        ; "ptype_params", ptype_params
        ; "ptype_jkind_annotation", ptype_jkind_annotation
        ; "ptype_private", ptype_private
        ; "ptype_manifest", ptype_manifest
        ; "ptype_kind", ptype_kind
        ; "ptype_cstrs", ptype_cstrs
        ; "ptype_attributes", ptype_attributes
        ; "ptype_post_doc", ptype_post_doc
        ; "ptype_loc", ptype_loc
        ; "ptype_tokens", ptype_tokens
        ]

    method type_kind : type_kind -> 'res =
      fun x ->
      match x with
      | Ptype_abstract -> self#constr "Ptype_abstract" []
      | Ptype_variant a ->
        let a = self#list self#constructor_declaration a in
        self#constr "Ptype_variant" [ a ]
      | Ptype_record a ->
        let a = self#list self#label_declaration a in
        self#constr "Ptype_record" [ a ]
      | Ptype_record_unboxed_product a ->
        let a = self#list self#label_declaration a in
        self#constr "Ptype_record_unboxed_product" [ a ]
      | Ptype_open -> self#constr "Ptype_open" []

    method label_declaration : label_declaration -> 'res =
      fun
        { pld_name
        ; pld_mutable
        ; pld_global
        ; pld_modalities
        ; pld_type
        ; pld_loc
        ; pld_attributes
        ; pld_doc
        ; pld_tokens
        }
        ->
      let pld_name = self#loc self#string pld_name in
      let pld_mutable = self#mutable_flag pld_mutable in
      let pld_global = self#bool pld_global in
      let pld_modalities = self#modalities pld_modalities in
      let pld_type = self#core_type pld_type in
      let pld_loc = self#location pld_loc in
      let pld_attributes = self#attributes pld_attributes in
      let pld_doc = self#option self#string pld_doc in
      let pld_tokens = self#token_seq pld_tokens in
      self
      #
      record
        [ "pld_name", pld_name
        ; "pld_mutable", pld_mutable
        ; "pld_global", pld_global
        ; "pld_modalities", pld_modalities
        ; "pld_type", pld_type
        ; "pld_loc", pld_loc
        ; "pld_attributes", pld_attributes
        ; "pld_doc", pld_doc
        ; "pld_tokens", pld_tokens
        ]

    method constructor_declaration : constructor_declaration -> 'res =
      fun
        { pcd_name
        ; pcd_vars
        ; pcd_args
        ; pcd_res
        ; pcd_loc
        ; pcd_attributes
        ; pcd_doc
        ; pcd_tokens
        }
        ->
      let pcd_name = self#loc self#longident_str_or_op pcd_name in
      let pcd_vars =
        self
        #
        list
          (fun (a, b) ->
            let a = self#loc self#string a in
            let b = self#option self#jkind_annotation b in
            self#tuple [ a; b ])
          pcd_vars
      in
      let pcd_args = self#constructor_arguments pcd_args in
      let pcd_res = self#option self#core_type pcd_res in
      let pcd_loc = self#location pcd_loc in
      let pcd_attributes = self#attributes pcd_attributes in
      let pcd_doc = self#option self#string pcd_doc in
      let pcd_tokens = self#token_seq pcd_tokens in
      self
      #
      record
        [ "pcd_name", pcd_name
        ; "pcd_vars", pcd_vars
        ; "pcd_args", pcd_args
        ; "pcd_res", pcd_res
        ; "pcd_loc", pcd_loc
        ; "pcd_attributes", pcd_attributes
        ; "pcd_doc", pcd_doc
        ; "pcd_tokens", pcd_tokens
        ]

    method constructor_argument : constructor_argument -> 'res =
      fun { pca_global; pca_type; pca_modalities; pca_loc } ->
      let pca_global = self#bool pca_global in
      let pca_type = self#core_type pca_type in
      let pca_modalities = self#modalities pca_modalities in
      let pca_loc = self#location pca_loc in
      self
      #
      record
        [ "pca_global", pca_global
        ; "pca_type", pca_type
        ; "pca_modalities", pca_modalities
        ; "pca_loc", pca_loc
        ]

    method constructor_arguments : constructor_arguments -> 'res =
      fun x ->
      match x with
      | Pcstr_tuple a ->
        let a = self#list self#constructor_argument a in
        self#constr "Pcstr_tuple" [ a ]
      | Pcstr_record a ->
        let a = self#list self#label_declaration a in
        self#constr "Pcstr_record" [ a ]

    method type_extension : type_extension -> 'res =
      fun
        { ptyext_pre_doc
        ; ptyext_ext_attrs
        ; ptyext_path
        ; ptyext_params
        ; ptyext_constructors
        ; ptyext_private
        ; ptyext_loc
        ; ptyext_attributes
        ; ptyext_post_doc
        ; ptyext_tokens
        }
        ->
      let ptyext_pre_doc = self#option self#string ptyext_pre_doc in
      let ptyext_ext_attrs = self#ext_attribute ptyext_ext_attrs in
      let ptyext_path = self#loc self#longident ptyext_path in
      let ptyext_params = self#list self#ptype_param ptyext_params in
      let ptyext_constructors =
        self#list self#extension_constructor ptyext_constructors
      in
      let ptyext_private = self#private_flag ptyext_private in
      let ptyext_loc = self#location ptyext_loc in
      let ptyext_attributes = self#attributes ptyext_attributes in
      let ptyext_post_doc = self#option self#string ptyext_post_doc in
      let ptyext_tokens = self#token_seq ptyext_tokens in
      self
      #
      record
        [ "ptyext_pre_doc", ptyext_pre_doc
        ; "ptyext_ext_attrs", ptyext_ext_attrs
        ; "ptyext_path", ptyext_path
        ; "ptyext_params", ptyext_params
        ; "ptyext_constructors", ptyext_constructors
        ; "ptyext_private", ptyext_private
        ; "ptyext_loc", ptyext_loc
        ; "ptyext_attributes", ptyext_attributes
        ; "ptyext_post_doc", ptyext_post_doc
        ; "ptyext_tokens", ptyext_tokens
        ]

    method extension_constructor : extension_constructor -> 'res =
      fun
        { pext_name
        ; pext_kind
        ; pext_loc
        ; pext_attributes
        ; pext_doc
        ; pext_tokens
        }
        ->
      let pext_name = self#loc self#longident_str_or_op pext_name in
      let pext_kind = self#extension_constructor_kind pext_kind in
      let pext_loc = self#location pext_loc in
      let pext_attributes = self#attributes pext_attributes in
      let pext_doc = self#option self#string pext_doc in
      let pext_tokens = self#token_seq pext_tokens in
      self
      #
      record
        [ "pext_name", pext_name
        ; "pext_kind", pext_kind
        ; "pext_loc", pext_loc
        ; "pext_attributes", pext_attributes
        ; "pext_doc", pext_doc
        ; "pext_tokens", pext_tokens
        ]

    method type_exception : type_exception -> 'res =
      fun
        { ptyexn_pre_doc
        ; ptyexn_ext_attrs
        ; ptyexn_constructor
        ; ptyexn_loc
        ; ptyexn_attributes
        ; ptyexn_post_doc
        ; ptyexn_tokens
        }
        ->
      let ptyexn_pre_doc = self#option self#string ptyexn_pre_doc in
      let ptyexn_ext_attrs = self#ext_attribute ptyexn_ext_attrs in
      let ptyexn_constructor = self#extension_constructor ptyexn_constructor in
      let ptyexn_loc = self#location ptyexn_loc in
      let ptyexn_attributes = self#attributes ptyexn_attributes in
      let ptyexn_post_doc = self#option self#string ptyexn_post_doc in
      let ptyexn_tokens = self#token_seq ptyexn_tokens in
      self
      #
      record
        [ "ptyexn_pre_doc", ptyexn_pre_doc
        ; "ptyexn_ext_attrs", ptyexn_ext_attrs
        ; "ptyexn_constructor", ptyexn_constructor
        ; "ptyexn_loc", ptyexn_loc
        ; "ptyexn_attributes", ptyexn_attributes
        ; "ptyexn_post_doc", ptyexn_post_doc
        ; "ptyexn_tokens", ptyexn_tokens
        ]

    method extension_constructor_kind : extension_constructor_kind -> 'res =
      fun x ->
      match x with
      | Pext_decl (a, b, c) ->
        let a =
          self
          #
          list
            (fun (a, b) ->
              let a = self#loc self#string a in
              let b = self#option self#jkind_annotation b in
              self#tuple [ a; b ])
            a
        in
        let b = self#constructor_arguments b in
        let c = self#option self#core_type c in
        self#constr "Pext_decl" [ a; b; c ]
      | Pext_rebind a ->
        let a = self#loc self#longident a in
        self#constr "Pext_rebind" [ a ]

    method class_type : class_type -> 'res =
      fun { pcty_desc; pcty_loc; pcty_attributes; pcty_tokens } ->
      let pcty_desc = self#class_type_desc pcty_desc in
      let pcty_loc = self#location pcty_loc in
      let pcty_attributes = self#attributes pcty_attributes in
      let pcty_tokens = self#token_seq pcty_tokens in
      self
      #
      record
        [ "pcty_desc", pcty_desc
        ; "pcty_loc", pcty_loc
        ; "pcty_attributes", pcty_attributes
        ; "pcty_tokens", pcty_tokens
        ]

    method class_type_desc : class_type_desc -> 'res =
      fun x ->
      match x with
      | Pcty_constr (a, b) ->
        let a = self#loc self#longident a in
        let b = self#list self#core_type b in
        self#constr "Pcty_constr" [ a; b ]
      | Pcty_signature a ->
        let a = self#class_signature a in
        self#constr "Pcty_signature" [ a ]
      | Pcty_arrow (a, b) ->
        let a = self#arrow_arg a in
        let b = self#class_type b in
        self#constr "Pcty_arrow" [ a; b ]
      | Pcty_extension a ->
        let a = self#extension a in
        self#constr "Pcty_extension" [ a ]
      | Pcty_open (a, b) ->
        let a = self#open_description a in
        let b = self#class_type b in
        self#constr "Pcty_open" [ a; b ]

    method class_signature : class_signature -> 'res =
      fun { pcsig_self; pcsig_fields } ->
      let pcsig_self = self#option self#core_type pcsig_self in
      let pcsig_fields = self#list self#class_type_field pcsig_fields in
      self#record [ "pcsig_self", pcsig_self; "pcsig_fields", pcsig_fields ]

    method class_type_field : class_type_field -> 'res =
      fun
        { pctf_pre_doc
        ; pctf_desc
        ; pctf_loc
        ; pctf_attributes
        ; pctf_post_doc
        ; pctf_tokens
        }
        ->
      let pctf_pre_doc = self#option self#string pctf_pre_doc in
      let pctf_desc = self#class_type_field_desc pctf_desc in
      let pctf_loc = self#location pctf_loc in
      let pctf_attributes = self#attributes pctf_attributes in
      let pctf_post_doc = self#option self#string pctf_post_doc in
      let pctf_tokens = self#token_seq pctf_tokens in
      self
      #
      record
        [ "pctf_pre_doc", pctf_pre_doc
        ; "pctf_desc", pctf_desc
        ; "pctf_loc", pctf_loc
        ; "pctf_attributes", pctf_attributes
        ; "pctf_post_doc", pctf_post_doc
        ; "pctf_tokens", pctf_tokens
        ]

    method class_type_field_desc : class_type_field_desc -> 'res =
      fun x ->
      match x with
      | Pctf_inherit a ->
        let a = self#class_type a in
        self#constr "Pctf_inherit" [ a ]
      | Pctf_val a ->
        let a =
          (fun (a, b, c, d) ->
            let a = self#loc self#string a in
            let b = self#mutable_flag b in
            let c = self#virtual_flag c in
            let d = self#core_type d in
            self#tuple [ a; b; c; d ])
            a
        in
        self#constr "Pctf_val" [ a ]
      | Pctf_method a ->
        let a =
          (fun (a, b, c, d) ->
            let a = self#loc self#string a in
            let b = self#private_flag b in
            let c = self#virtual_flag c in
            let d = self#core_type d in
            self#tuple [ a; b; c; d ])
            a
        in
        self#constr "Pctf_method" [ a ]
      | Pctf_constraint a ->
        let a =
          (fun (a, b) ->
            let a = self#core_type a in
            let b = self#core_type b in
            self#tuple [ a; b ])
            a
        in
        self#constr "Pctf_constraint" [ a ]
      | Pctf_attribute a ->
        let a = self#attribute a in
        self#constr "Pctf_attribute" [ a ]
      | Pctf_extension a ->
        let a = self#extension a in
        self#constr "Pctf_extension" [ a ]
      | Pctf_docstring a ->
        let a = self#string a in
        self#constr "Pctf_docstring" [ a ]

    method class_infos : 'a. ('a -> 'res) -> 'a class_infos -> 'res =
      fun
        _a
        { pci_pre_text
        ; pci_pre_doc
        ; pci_virt
        ; pci_ext_attrs
        ; pci_params
        ; pci_name
        ; pci_value_params
        ; pci_constraint
        ; pci_expr
        ; pci_loc
        ; pci_attributes
        ; pci_post_doc
        ; pci_tokens
        }
        ->
      let pci_pre_text = self#list self#string pci_pre_text in
      let pci_pre_doc = self#option self#string pci_pre_doc in
      let pci_virt = self#virtual_flag pci_virt in
      let pci_ext_attrs = self#ext_attribute pci_ext_attrs in
      let pci_params = self#list self#ptype_param pci_params in
      let pci_name = self#loc self#string pci_name in
      let pci_value_params =
        self#list (self#argument self#pattern) pci_value_params
      in
      let pci_constraint = self#option self#class_type pci_constraint in
      let pci_expr = _a pci_expr in
      let pci_loc = self#location pci_loc in
      let pci_attributes = self#attributes pci_attributes in
      let pci_post_doc = self#option self#string pci_post_doc in
      let pci_tokens = self#token_seq pci_tokens in
      self
      #
      record
        [ "pci_pre_text", pci_pre_text
        ; "pci_pre_doc", pci_pre_doc
        ; "pci_virt", pci_virt
        ; "pci_ext_attrs", pci_ext_attrs
        ; "pci_params", pci_params
        ; "pci_name", pci_name
        ; "pci_value_params", pci_value_params
        ; "pci_constraint", pci_constraint
        ; "pci_expr", pci_expr
        ; "pci_loc", pci_loc
        ; "pci_attributes", pci_attributes
        ; "pci_post_doc", pci_post_doc
        ; "pci_tokens", pci_tokens
        ]

    method class_description : class_description -> 'res =
      self#class_infos self#class_type

    method class_type_declaration : class_type_declaration -> 'res =
      self#class_infos self#class_type

    method class_expr : class_expr -> 'res =
      fun { pcl_ext_attrs; pcl_desc; pcl_loc; pcl_attributes } ->
      let pcl_ext_attrs = self#ext_attribute pcl_ext_attrs in
      let pcl_desc = self#class_expr_desc pcl_desc in
      let pcl_loc = self#location pcl_loc in
      let pcl_attributes = self#attributes pcl_attributes in
      self
      #
      record
        [ "pcl_ext_attrs", pcl_ext_attrs
        ; "pcl_desc", pcl_desc
        ; "pcl_loc", pcl_loc
        ; "pcl_attributes", pcl_attributes
        ]

    method class_expr_desc : class_expr_desc -> 'res =
      fun x ->
      match x with
      | Pcl_constr (a, b) ->
        let a = self#loc self#longident a in
        let b = self#list self#core_type b in
        self#constr "Pcl_constr" [ a; b ]
      | Pcl_structure a ->
        let a = self#class_structure a in
        self#constr "Pcl_structure" [ a ]
      | Pcl_fun (a, b) ->
        let a = self#list (self#argument self#pattern) a in
        let b = self#class_expr b in
        self#constr "Pcl_fun" [ a; b ]
      | Pcl_apply (a, b) ->
        let a = self#class_expr a in
        let b = self#list (self#argument self#expression) b in
        self#constr "Pcl_apply" [ a; b ]
      | Pcl_let (a, b, c) ->
        let a = self#rec_flag a in
        let b = self#list self#value_binding b in
        let c = self#class_expr c in
        self#constr "Pcl_let" [ a; b; c ]
      | Pcl_constraint (a, b) ->
        let a = self#class_expr a in
        let b = self#class_type b in
        self#constr "Pcl_constraint" [ a; b ]
      | Pcl_extension a ->
        let a = self#extension a in
        self#constr "Pcl_extension" [ a ]
      | Pcl_open (a, b) ->
        let a = self#open_description a in
        let b = self#class_expr b in
        self#constr "Pcl_open" [ a; b ]
      | Pcl_parens a ->
        let a = self#class_expr a in
        self#constr "Pcl_parens" [ a ]

    method class_structure : class_structure -> 'res =
      fun { pcstr_self; pcstr_fields } ->
      let pcstr_self = self#pattern pcstr_self in
      let pcstr_fields = self#list self#class_field pcstr_fields in
      self#record [ "pcstr_self", pcstr_self; "pcstr_fields", pcstr_fields ]

    method class_field : class_field -> 'res =
      fun
        { pcf_pre_doc
        ; pcf_desc
        ; pcf_loc
        ; pcf_attributes
        ; pcf_post_doc
        ; pcf_tokens
        }
        ->
      let pcf_pre_doc = self#option self#string pcf_pre_doc in
      let pcf_desc = self#class_field_desc pcf_desc in
      let pcf_loc = self#location pcf_loc in
      let pcf_attributes = self#attributes pcf_attributes in
      let pcf_post_doc = self#option self#string pcf_post_doc in
      let pcf_tokens = self#token_seq pcf_tokens in
      self
      #
      record
        [ "pcf_pre_doc", pcf_pre_doc
        ; "pcf_desc", pcf_desc
        ; "pcf_loc", pcf_loc
        ; "pcf_attributes", pcf_attributes
        ; "pcf_post_doc", pcf_post_doc
        ; "pcf_tokens", pcf_tokens
        ]

    method class_field_desc : class_field_desc -> 'res =
      fun x ->
      match x with
      | Pcf_inherit (a, b, c) ->
        let a = self#override_flag a in
        let b = self#class_expr b in
        let c = self#option (self#loc self#string) c in
        self#constr "Pcf_inherit" [ a; b; c ]
      | Pcf_val a ->
        let a =
          (fun (a, b, c) ->
            let a = self#loc self#string a in
            let b = self#mutable_flag b in
            let c = self#class_field_kind c in
            self#tuple [ a; b; c ])
            a
        in
        self#constr "Pcf_val" [ a ]
      | Pcf_method a ->
        let a =
          (fun (a, b, c) ->
            let a = self#loc self#string a in
            let b = self#private_flag b in
            let c = self#class_field_kind c in
            self#tuple [ a; b; c ])
            a
        in
        self#constr "Pcf_method" [ a ]
      | Pcf_constraint a ->
        let a =
          (fun (a, b) ->
            let a = self#core_type a in
            let b = self#core_type b in
            self#tuple [ a; b ])
            a
        in
        self#constr "Pcf_constraint" [ a ]
      | Pcf_initializer a ->
        let a = self#expression a in
        self#constr "Pcf_initializer" [ a ]
      | Pcf_attribute a ->
        let a = self#attribute a in
        self#constr "Pcf_attribute" [ a ]
      | Pcf_extension a ->
        let a = self#extension a in
        self#constr "Pcf_extension" [ a ]
      | Pcf_docstring a ->
        let a = self#string a in
        self#constr "Pcf_docstring" [ a ]

    method class_field_kind : class_field_kind -> 'res =
      fun x ->
      match x with
      | Cfk_virtual a ->
        let a = self#core_type a in
        self#constr "Cfk_virtual" [ a ]
      | Cfk_concrete (a, b) ->
        let a = self#override_flag a in
        let b = self#value_binding b in
        self#constr "Cfk_concrete" [ a; b ]

    method class_declaration : class_declaration -> 'res =
      self#class_infos self#class_expr

    method module_type : module_type -> 'res =
      fun { pmty_desc; pmty_loc; pmty_attributes; pmty_tokens } ->
      let pmty_desc = self#module_type_desc pmty_desc in
      let pmty_loc = self#location pmty_loc in
      let pmty_attributes = self#attributes pmty_attributes in
      let pmty_tokens = self#token_seq pmty_tokens in
      self
      #
      record
        [ "pmty_desc", pmty_desc
        ; "pmty_loc", pmty_loc
        ; "pmty_attributes", pmty_attributes
        ; "pmty_tokens", pmty_tokens
        ]

    method module_type_desc : module_type_desc -> 'res =
      fun x ->
      match x with
      | Pmty_ident a ->
        let a = self#loc self#longident a in
        self#constr "Pmty_ident" [ a ]
      | Pmty_signature a ->
        let a = self#signature a in
        self#constr "Pmty_signature" [ a ]
      | Pmty_functor (a, b, c, d) ->
        let a = self#attributes a in
        let b = self#list self#functor_parameter b in
        let c = self#module_type c in
        let d = self#modes d in
        self#constr "Pmty_functor" [ a; b; c; d ]
      | Pmty_functor_type (a, b, c) ->
        let a = self#list self#functor_parameter a in
        let b = self#module_type b in
        let c = self#modes c in
        self#constr "Pmty_functor_type" [ a; b; c ]
      | Pmty_with (a, b) ->
        let a = self#module_type a in
        let b = self#list self#with_constraint b in
        self#constr "Pmty_with" [ a; b ]
      | Pmty_typeof (a, b) ->
        let a = self#attributes a in
        let b = self#module_expr b in
        self#constr "Pmty_typeof" [ a; b ]
      | Pmty_extension a ->
        let a = self#extension a in
        self#constr "Pmty_extension" [ a ]
      | Pmty_alias a ->
        let a = self#loc self#longident a in
        self#constr "Pmty_alias" [ a ]
      | Pmty_strengthen (a, b) ->
        let a = self#module_type a in
        let b = self#loc self#longident b in
        self#constr "Pmty_strengthen" [ a; b ]
      | Pmty_parens a ->
        let a = self#module_type a in
        self#constr "Pmty_parens" [ a ]

    method functor_parameter : functor_parameter -> 'res =
      fun x ->
      match x with
      | Unit -> self#constr "Unit" []
      | Named (a, b, c) ->
        let a = self#loc (self#option self#string) a in
        let b = self#module_type b in
        let c = self#modes c in
        self#constr "Named" [ a; b; c ]
      | Unnamed (a, b) ->
        let a = self#module_type a in
        let b = self#modes b in
        self#constr "Unnamed" [ a; b ]

    method signature : signature -> 'res =
      fun { psg_modalities; psg_items; psg_loc; psg_tokens } ->
      let psg_modalities = self#modalities psg_modalities in
      let psg_items = self#list self#signature_item psg_items in
      let psg_loc = self#location psg_loc in
      let psg_tokens = self#token_seq psg_tokens in
      self
      #
      record
        [ "psg_modalities", psg_modalities
        ; "psg_items", psg_items
        ; "psg_loc", psg_loc
        ; "psg_tokens", psg_tokens
        ]

    method signature_item : signature_item -> 'res =
      fun { psig_desc; psig_loc; psig_tokens } ->
      let psig_desc = self#signature_item_desc psig_desc in
      let psig_loc = self#location psig_loc in
      let psig_tokens = self#token_seq psig_tokens in
      self
      #
      record
        [ "psig_desc", psig_desc
        ; "psig_loc", psig_loc
        ; "psig_tokens", psig_tokens
        ]

    method signature_item_desc : signature_item_desc -> 'res =
      fun x ->
      match x with
      | Psig_value a ->
        let a = self#value_description a in
        self#constr "Psig_value" [ a ]
      | Psig_type (a, b) ->
        let a = self#rec_flag a in
        let b = self#list self#type_declaration b in
        self#constr "Psig_type" [ a; b ]
      | Psig_typesubst a ->
        let a = self#list self#type_declaration a in
        self#constr "Psig_typesubst" [ a ]
      | Psig_typext a ->
        let a = self#type_extension a in
        self#constr "Psig_typext" [ a ]
      | Psig_exception a ->
        let a = self#type_exception a in
        self#constr "Psig_exception" [ a ]
      | Psig_module a ->
        let a = self#module_declaration a in
        self#constr "Psig_module" [ a ]
      | Psig_modsubst a ->
        let a = self#module_substitution a in
        self#constr "Psig_modsubst" [ a ]
      | Psig_recmodule a ->
        let a = self#list self#module_declaration a in
        self#constr "Psig_recmodule" [ a ]
      | Psig_modtype a ->
        let a = self#module_type_declaration a in
        self#constr "Psig_modtype" [ a ]
      | Psig_modtypesubst a ->
        let a = self#module_type_declaration a in
        self#constr "Psig_modtypesubst" [ a ]
      | Psig_open a ->
        let a = self#open_description a in
        self#constr "Psig_open" [ a ]
      | Psig_include (a, b) ->
        let a = self#include_description a in
        let b = self#modalities b in
        self#constr "Psig_include" [ a; b ]
      | Psig_class a ->
        let a = self#list self#class_description a in
        self#constr "Psig_class" [ a ]
      | Psig_class_type a ->
        let a = self#list self#class_type_declaration a in
        self#constr "Psig_class_type" [ a ]
      | Psig_attribute a ->
        let a = self#attribute a in
        self#constr "Psig_attribute" [ a ]
      | Psig_extension a ->
        let a = self#toplevel_extension a in
        self#constr "Psig_extension" [ a ]
      | Psig_kind_abbrev (a, b) ->
        let a = self#loc self#string a in
        let b = self#jkind_annotation b in
        self#constr "Psig_kind_abbrev" [ a; b ]
      | Psig_docstring a ->
        let a = self#string a in
        self#constr "Psig_docstring" [ a ]

    method module_declaration_body : module_declaration_body -> 'res =
      fun x ->
      match x with
      | With_params (a, b, c) ->
        let a = self#list self#functor_parameter a in
        let b = self#module_type b in
        let c = self#modes c in
        self#constr "With_params" [ a; b; c ]
      | Without_params (a, b) ->
        let a = self#module_type a in
        let b = self#modalities b in
        self#constr "Without_params" [ a; b ]

    method module_declaration : module_declaration -> 'res =
      fun
        { pmd_pre_text
        ; pmd_pre_doc
        ; pmd_ext_attrs
        ; pmd_name
        ; pmd_body
        ; pmd_attributes
        ; pmd_post_doc
        ; pmd_loc
        ; pmd_tokens
        }
        ->
      let pmd_pre_text = self#list self#string pmd_pre_text in
      let pmd_pre_doc = self#option self#string pmd_pre_doc in
      let pmd_ext_attrs = self#ext_attribute pmd_ext_attrs in
      let pmd_name =
        (fun (a, b) ->
          let a = self#loc (self#option self#string) a in
          let b = self#modalities b in
          self#tuple [ a; b ])
          pmd_name
      in
      let pmd_body = self#module_declaration_body pmd_body in
      let pmd_attributes = self#attributes pmd_attributes in
      let pmd_post_doc = self#option self#string pmd_post_doc in
      let pmd_loc = self#location pmd_loc in
      let pmd_tokens = self#token_seq pmd_tokens in
      self
      #
      record
        [ "pmd_pre_text", pmd_pre_text
        ; "pmd_pre_doc", pmd_pre_doc
        ; "pmd_ext_attrs", pmd_ext_attrs
        ; "pmd_name", pmd_name
        ; "pmd_body", pmd_body
        ; "pmd_attributes", pmd_attributes
        ; "pmd_post_doc", pmd_post_doc
        ; "pmd_loc", pmd_loc
        ; "pmd_tokens", pmd_tokens
        ]

    method module_substitution : module_substitution -> 'res =
      fun
        { pms_pre_doc
        ; pms_ext_attrs
        ; pms_name
        ; pms_manifest
        ; pms_attributes
        ; pms_post_doc
        ; pms_loc
        ; pms_tokens
        }
        ->
      let pms_pre_doc = self#option self#string pms_pre_doc in
      let pms_ext_attrs = self#ext_attribute pms_ext_attrs in
      let pms_name = self#loc self#string pms_name in
      let pms_manifest = self#loc self#longident pms_manifest in
      let pms_attributes = self#attributes pms_attributes in
      let pms_post_doc = self#option self#string pms_post_doc in
      let pms_loc = self#location pms_loc in
      let pms_tokens = self#token_seq pms_tokens in
      self
      #
      record
        [ "pms_pre_doc", pms_pre_doc
        ; "pms_ext_attrs", pms_ext_attrs
        ; "pms_name", pms_name
        ; "pms_manifest", pms_manifest
        ; "pms_attributes", pms_attributes
        ; "pms_post_doc", pms_post_doc
        ; "pms_loc", pms_loc
        ; "pms_tokens", pms_tokens
        ]

    method module_type_declaration : module_type_declaration -> 'res =
      fun
        { pmtd_pre_doc
        ; pmtd_ext_attrs
        ; pmtd_name
        ; pmtd_type
        ; pmtd_attributes
        ; pmtd_post_doc
        ; pmtd_loc
        ; pmtd_tokens
        }
        ->
      let pmtd_pre_doc = self#option self#string pmtd_pre_doc in
      let pmtd_ext_attrs = self#ext_attribute pmtd_ext_attrs in
      let pmtd_name = self#loc self#string pmtd_name in
      let pmtd_type = self#option self#module_type pmtd_type in
      let pmtd_attributes = self#attributes pmtd_attributes in
      let pmtd_post_doc = self#option self#string pmtd_post_doc in
      let pmtd_loc = self#location pmtd_loc in
      let pmtd_tokens = self#token_seq pmtd_tokens in
      self
      #
      record
        [ "pmtd_pre_doc", pmtd_pre_doc
        ; "pmtd_ext_attrs", pmtd_ext_attrs
        ; "pmtd_name", pmtd_name
        ; "pmtd_type", pmtd_type
        ; "pmtd_attributes", pmtd_attributes
        ; "pmtd_post_doc", pmtd_post_doc
        ; "pmtd_loc", pmtd_loc
        ; "pmtd_tokens", pmtd_tokens
        ]

    method open_infos : 'a. ('a -> 'res) -> 'a open_infos -> 'res =
      fun
        _a
        { popen_pre_doc
        ; popen_ext_attrs
        ; popen_expr
        ; popen_override
        ; popen_loc
        ; popen_attributes
        ; popen_post_doc
        ; popen_tokens
        }
        ->
      let popen_pre_doc = self#option self#string popen_pre_doc in
      let popen_ext_attrs = self#ext_attribute popen_ext_attrs in
      let popen_expr = _a popen_expr in
      let popen_override = self#override_flag popen_override in
      let popen_loc = self#location popen_loc in
      let popen_attributes = self#attributes popen_attributes in
      let popen_post_doc = self#option self#string popen_post_doc in
      let popen_tokens = self#token_seq popen_tokens in
      self
      #
      record
        [ "popen_pre_doc", popen_pre_doc
        ; "popen_ext_attrs", popen_ext_attrs
        ; "popen_expr", popen_expr
        ; "popen_override", popen_override
        ; "popen_loc", popen_loc
        ; "popen_attributes", popen_attributes
        ; "popen_post_doc", popen_post_doc
        ; "popen_tokens", popen_tokens
        ]

    method open_description : open_description -> 'res =
      self#open_infos (self#loc self#longident)

    method open_declaration : open_declaration -> 'res =
      self#open_infos self#module_expr

    method include_infos : 'a. ('a -> 'res) -> 'a include_infos -> 'res =
      fun
        _a
        { pincl_pre_doc
        ; pincl_kind
        ; pincl_ext_attrs
        ; pincl_mod
        ; pincl_loc
        ; pincl_attributes
        ; pincl_post_doc
        ; pincl_tokens
        }
        ->
      let pincl_pre_doc = self#option self#string pincl_pre_doc in
      let pincl_kind = self#include_kind pincl_kind in
      let pincl_ext_attrs = self#ext_attribute pincl_ext_attrs in
      let pincl_mod = _a pincl_mod in
      let pincl_loc = self#location pincl_loc in
      let pincl_attributes = self#attributes pincl_attributes in
      let pincl_post_doc = self#option self#string pincl_post_doc in
      let pincl_tokens = self#token_seq pincl_tokens in
      self
      #
      record
        [ "pincl_pre_doc", pincl_pre_doc
        ; "pincl_kind", pincl_kind
        ; "pincl_ext_attrs", pincl_ext_attrs
        ; "pincl_mod", pincl_mod
        ; "pincl_loc", pincl_loc
        ; "pincl_attributes", pincl_attributes
        ; "pincl_post_doc", pincl_post_doc
        ; "pincl_tokens", pincl_tokens
        ]

    method include_description : include_description -> 'res =
      self#include_infos self#module_type

    method include_declaration : include_declaration -> 'res =
      self#include_infos self#module_expr

    method with_constraint : with_constraint -> 'res =
      fun { wc_desc; wc_loc; wc_tokens } ->
      let wc_desc = self#with_constraint_desc wc_desc in
      let wc_loc = self#location wc_loc in
      let wc_tokens = self#token_seq wc_tokens in
      self
      #
      record
        [ "wc_desc", wc_desc; "wc_loc", wc_loc; "wc_tokens", wc_tokens ]

    method with_constraint_desc : with_constraint_desc -> 'res =
      fun x ->
      match x with
      | Pwith_type (a, b, c, d, e) ->
        let a = self#ptype_params a in
        let b = self#loc self#longident b in
        let c = self#private_flag c in
        let d = self#core_type d in
        let e = self#list self#ptype_constraint e in
        self#constr "Pwith_type" [ a; b; c; d; e ]
      | Pwith_module (a, b) ->
        let a = self#loc self#longident a in
        let b = self#loc self#longident b in
        self#constr "Pwith_module" [ a; b ]
      | Pwith_modtype (a, b) ->
        let a = self#loc self#longident a in
        let b = self#module_type b in
        self#constr "Pwith_modtype" [ a; b ]
      | Pwith_modtypesubst (a, b) ->
        let a = self#loc self#longident a in
        let b = self#module_type b in
        self#constr "Pwith_modtypesubst" [ a; b ]
      | Pwith_typesubst (a, b, c) ->
        let a = self#ptype_params a in
        let b = self#loc self#longident b in
        let c = self#core_type c in
        self#constr "Pwith_typesubst" [ a; b; c ]
      | Pwith_modsubst (a, b) ->
        let a = self#loc self#longident a in
        let b = self#loc self#longident b in
        self#constr "Pwith_modsubst" [ a; b ]

    method module_expr : module_expr -> 'res =
      fun { pmod_desc; pmod_loc; pmod_attributes; pmod_tokens } ->
      let pmod_desc = self#module_expr_desc pmod_desc in
      let pmod_loc = self#location pmod_loc in
      let pmod_attributes = self#attributes pmod_attributes in
      let pmod_tokens = self#token_seq pmod_tokens in
      self
      #
      record
        [ "pmod_desc", pmod_desc
        ; "pmod_loc", pmod_loc
        ; "pmod_attributes", pmod_attributes
        ; "pmod_tokens", pmod_tokens
        ]

    method module_expr_desc : module_expr_desc -> 'res =
      fun x ->
      match x with
      | Pmod_ident a ->
        let a = self#loc self#longident a in
        self#constr "Pmod_ident" [ a ]
      | Pmod_structure (a, b) ->
        let a = self#attributes a in
        let b = self#structure b in
        self#constr "Pmod_structure" [ a; b ]
      | Pmod_functor (a, b, c) ->
        let a = self#attributes a in
        let b = self#list self#functor_parameter b in
        let c = self#module_expr c in
        self#constr "Pmod_functor" [ a; b; c ]
      | Pmod_apply (a, b) ->
        let a = self#module_expr a in
        let b = self#module_expr b in
        self#constr "Pmod_apply" [ a; b ]
      | Pmod_apply_unit a ->
        let a = self#module_expr a in
        self#constr "Pmod_apply_unit" [ a ]
      | Pmod_constraint (a, b, c) ->
        let a = self#module_expr a in
        let b = self#option self#module_type b in
        let c = self#modes c in
        self#constr "Pmod_constraint" [ a; b; c ]
      | Pmod_unpack (a, b, c) ->
        let a = self#expression a in
        let b = self#option self#package_type b in
        let c = self#option self#package_type c in
        self#constr "Pmod_unpack" [ a; b; c ]
      | Pmod_extension a ->
        let a = self#extension a in
        self#constr "Pmod_extension" [ a ]
      | Pmod_parens a ->
        let a = self#module_expr a in
        self#constr "Pmod_parens" [ a ]

    method structure : structure -> 'res =
      fun (a, b) ->
      let a = self#list self#structure_item a in
      let b = self#token_seq b in
      self#tuple [ a; b ]

    method structure_item : structure_item -> 'res =
      fun { pstr_desc; pstr_loc; pstr_tokens } ->
      let pstr_desc = self#structure_item_desc pstr_desc in
      let pstr_loc = self#location pstr_loc in
      let pstr_tokens = self#token_seq pstr_tokens in
      self
      #
      record
        [ "pstr_desc", pstr_desc
        ; "pstr_loc", pstr_loc
        ; "pstr_tokens", pstr_tokens
        ]

    method structure_item_desc : structure_item_desc -> 'res =
      fun x ->
      match x with
      | Pstr_eval (a, b) ->
        let a = self#expression a in
        let b = self#attributes b in
        self#constr "Pstr_eval" [ a; b ]
      | Pstr_value (a, b) ->
        let a = self#rec_flag a in
        let b = self#list self#value_binding b in
        self#constr "Pstr_value" [ a; b ]
      | Pstr_primitive a ->
        let a = self#value_description a in
        self#constr "Pstr_primitive" [ a ]
      | Pstr_type (a, b) ->
        let a = self#rec_flag a in
        let b = self#list self#type_declaration b in
        self#constr "Pstr_type" [ a; b ]
      | Pstr_typext a ->
        let a = self#type_extension a in
        self#constr "Pstr_typext" [ a ]
      | Pstr_exception a ->
        let a = self#type_exception a in
        self#constr "Pstr_exception" [ a ]
      | Pstr_module a ->
        let a = self#module_binding a in
        self#constr "Pstr_module" [ a ]
      | Pstr_recmodule a ->
        let a = self#list self#module_binding a in
        self#constr "Pstr_recmodule" [ a ]
      | Pstr_modtype a ->
        let a = self#module_type_declaration a in
        self#constr "Pstr_modtype" [ a ]
      | Pstr_open a ->
        let a = self#open_declaration a in
        self#constr "Pstr_open" [ a ]
      | Pstr_class a ->
        let a = self#list self#class_declaration a in
        self#constr "Pstr_class" [ a ]
      | Pstr_class_type a ->
        let a = self#list self#class_type_declaration a in
        self#constr "Pstr_class_type" [ a ]
      | Pstr_include a ->
        let a = self#include_declaration a in
        self#constr "Pstr_include" [ a ]
      | Pstr_attribute a ->
        let a = self#attribute a in
        self#constr "Pstr_attribute" [ a ]
      | Pstr_extension a ->
        let a = self#toplevel_extension a in
        self#constr "Pstr_extension" [ a ]
      | Pstr_kind_abbrev (a, b) ->
        let a = self#loc self#string a in
        let b = self#jkind_annotation b in
        self#constr "Pstr_kind_abbrev" [ a; b ]
      | Pstr_docstring a ->
        let a = self#string a in
        self#constr "Pstr_docstring" [ a ]

    method value_constraint : value_constraint -> 'res =
      fun x ->
      match x with
      | Pvc_constraint { locally_abstract_univars; typ } ->
        let locally_abstract_univars =
          self
          #
          list
            (fun (a, b) ->
              let a = self#loc self#string a in
              let b = self#option self#jkind_annotation b in
              self#tuple [ a; b ])
            locally_abstract_univars
        in
        let typ = self#core_type typ in
        self
        #
        constr
          "Pvc_constraint"
          [ self
            #
            record
              [ "locally_abstract_univars", locally_abstract_univars
              ; "typ", typ
              ]
          ]
      | Pvc_coercion { ground; coercion } ->
        let ground = self#option self#core_type ground in
        let coercion = self#core_type coercion in
        self
        #
        constr
          "Pvc_coercion"
          [ self#record [ "ground", ground; "coercion", coercion ] ]

    method value_binding : value_binding -> 'res =
      fun
        { pvb_pre_text
        ; pvb_pre_doc
        ; pvb_ext_attrs
        ; pvb_legacy_modes
        ; pvb_pat
        ; pvb_modes
        ; pvb_params
        ; pvb_constraint
        ; pvb_expr
        ; pvb_ret_modes
        ; pvb_attributes
        ; pvb_post_doc
        ; pvb_loc
        ; pvb_tokens
        }
        ->
      let pvb_pre_text = self#list self#string pvb_pre_text in
      let pvb_pre_doc = self#option self#string pvb_pre_doc in
      let pvb_ext_attrs = self#ext_attribute pvb_ext_attrs in
      let pvb_legacy_modes = self#modes pvb_legacy_modes in
      let pvb_pat = self#pattern pvb_pat in
      let pvb_modes = self#modes pvb_modes in
      let pvb_params = self#list self#function_param pvb_params in
      let pvb_constraint = self#option self#value_constraint pvb_constraint in
      let pvb_expr = self#option self#expression pvb_expr in
      let pvb_ret_modes = self#modes pvb_ret_modes in
      let pvb_attributes = self#attributes pvb_attributes in
      let pvb_post_doc = self#option self#string pvb_post_doc in
      let pvb_loc = self#location pvb_loc in
      let pvb_tokens = self#token_seq pvb_tokens in
      self
      #
      record
        [ "pvb_pre_text", pvb_pre_text
        ; "pvb_pre_doc", pvb_pre_doc
        ; "pvb_ext_attrs", pvb_ext_attrs
        ; "pvb_legacy_modes", pvb_legacy_modes
        ; "pvb_pat", pvb_pat
        ; "pvb_modes", pvb_modes
        ; "pvb_params", pvb_params
        ; "pvb_constraint", pvb_constraint
        ; "pvb_expr", pvb_expr
        ; "pvb_ret_modes", pvb_ret_modes
        ; "pvb_attributes", pvb_attributes
        ; "pvb_post_doc", pvb_post_doc
        ; "pvb_loc", pvb_loc
        ; "pvb_tokens", pvb_tokens
        ]

    method module_binding : module_binding -> 'res =
      fun
        { pmb_pre_text
        ; pmb_pre_doc
        ; pmb_ext_attrs
        ; pmb_name
        ; pmb_params
        ; pmb_constraint
        ; pmb_modes
        ; pmb_expr
        ; pmb_attributes
        ; pmb_post_doc
        ; pmb_loc
        ; pmb_tokens
        }
        ->
      let pmb_pre_text = self#list self#string pmb_pre_text in
      let pmb_pre_doc = self#option self#string pmb_pre_doc in
      let pmb_ext_attrs = self#ext_attribute pmb_ext_attrs in
      let pmb_name =
        (fun (a, b) ->
          let a = self#loc (self#option self#string) a in
          let b = self#modes b in
          self#tuple [ a; b ])
          pmb_name
      in
      let pmb_params = self#list self#functor_parameter pmb_params in
      let pmb_constraint = self#option self#module_type pmb_constraint in
      let pmb_modes = self#modes pmb_modes in
      let pmb_expr = self#module_expr pmb_expr in
      let pmb_attributes = self#attributes pmb_attributes in
      let pmb_post_doc = self#option self#string pmb_post_doc in
      let pmb_loc = self#location pmb_loc in
      let pmb_tokens = self#token_seq pmb_tokens in
      self
      #
      record
        [ "pmb_pre_text", pmb_pre_text
        ; "pmb_pre_doc", pmb_pre_doc
        ; "pmb_ext_attrs", pmb_ext_attrs
        ; "pmb_name", pmb_name
        ; "pmb_params", pmb_params
        ; "pmb_constraint", pmb_constraint
        ; "pmb_modes", pmb_modes
        ; "pmb_expr", pmb_expr
        ; "pmb_attributes", pmb_attributes
        ; "pmb_post_doc", pmb_post_doc
        ; "pmb_loc", pmb_loc
        ; "pmb_tokens", pmb_tokens
        ]

    method jkind_annotation_desc : jkind_annotation_desc -> 'res =
      fun x ->
      match x with
      | Pjk_default -> self#constr "Pjk_default" []
      | Pjk_abbreviation a ->
        let a = self#string a in
        self#constr "Pjk_abbreviation" [ a ]
      | Pjk_mod (a, b) ->
        let a = self#jkind_annotation a in
        let b = self#modes b in
        self#constr "Pjk_mod" [ a; b ]
      | Pjk_with (a, b, c) ->
        let a = self#jkind_annotation a in
        let b = self#core_type b in
        let c = self#modalities c in
        self#constr "Pjk_with" [ a; b; c ]
      | Pjk_kind_of a ->
        let a = self#core_type a in
        self#constr "Pjk_kind_of" [ a ]
      | Pjk_product a ->
        let a = self#list self#jkind_annotation a in
        self#constr "Pjk_product" [ a ]
      | Pjk_parens a ->
        let a = self#jkind_annotation_desc a in
        self#constr "Pjk_parens" [ a ]

    method jkind_annotation : jkind_annotation -> 'res =
      fun { pjkind_loc; pjkind_desc; pjkind_tokens } ->
      let pjkind_loc = self#location pjkind_loc in
      let pjkind_desc = self#jkind_annotation_desc pjkind_desc in
      let pjkind_tokens = self#token_seq pjkind_tokens in
      self
      #
      record
        [ "pjkind_loc", pjkind_loc
        ; "pjkind_desc", pjkind_desc
        ; "pjkind_tokens", pjkind_tokens
        ]

    method use_file : use_file -> 'res =
      fun (a, b) ->
      let a = self#list self#toplevel_phrase a in
      let b = self#token_seq b in
      self#tuple [ a; b ]

    method toplevel_phrase : toplevel_phrase -> 'res =
      fun x ->
      match x with
      | Ptop_def a ->
        let a = self#structure a in
        self#constr "Ptop_def" [ a ]
      | Ptop_dir a ->
        let a = self#toplevel_directive a in
        self#constr "Ptop_dir" [ a ]
      | Ptop_lex a ->
        let a = self#lexer_directive a in
        self#constr "Ptop_lex" [ a ]

    method toplevel_directive : toplevel_directive -> 'res =
      fun { pdir_name; pdir_arg; pdir_loc; pdir_tokens } ->
      let pdir_name = self#loc self#string pdir_name in
      let pdir_arg = self#option self#directive_argument pdir_arg in
      let pdir_loc = self#location pdir_loc in
      let pdir_tokens = self#token_seq pdir_tokens in
      self
      #
      record
        [ "pdir_name", pdir_name
        ; "pdir_arg", pdir_arg
        ; "pdir_loc", pdir_loc
        ; "pdir_tokens", pdir_tokens
        ]

    method directive_argument : directive_argument -> 'res =
      fun { pdira_desc; pdira_loc } ->
      let pdira_desc = self#directive_argument_desc pdira_desc in
      let pdira_loc = self#location pdira_loc in
      self#record [ "pdira_desc", pdira_desc; "pdira_loc", pdira_loc ]

    method directive_argument_desc : directive_argument_desc -> 'res =
      fun x ->
      match x with
      | Pdir_string a ->
        let a = self#string a in
        self#constr "Pdir_string" [ a ]
      | Pdir_int (a, b) ->
        let a = self#string a in
        let b = self#option self#char b in
        self#constr "Pdir_int" [ a; b ]
      | Pdir_ident a ->
        let a = self#longident a in
        self#constr "Pdir_ident" [ a ]
      | Pdir_bool a ->
        let a = self#bool a in
        self#constr "Pdir_bool" [ a ]

    method syntax_directive : syntax_directive -> 'res =
      fun { psyn_mode; psyn_toggle } ->
      let psyn_mode = self#loc self#string psyn_mode in
      let psyn_toggle = self#bool psyn_toggle in
      self#record [ "psyn_mode", psyn_mode; "psyn_toggle", psyn_toggle ]

    method lexer_directive_desc : lexer_directive_desc -> 'res =
      fun x ->
      match x with
      | Plex_syntax a ->
        let a = self#syntax_directive a in
        self#constr "Plex_syntax" [ a ]

    method lexer_directive : lexer_directive -> 'res =
      fun { plex_desc; plex_loc; plex_tokens } ->
      let plex_desc = self#lexer_directive_desc plex_desc in
      let plex_loc = self#location plex_loc in
      let plex_tokens = self#token_seq plex_tokens in
      self
      #
      record
        [ "plex_desc", plex_desc
        ; "plex_loc", plex_loc
        ; "plex_tokens", plex_tokens
        ]
  end

class virtual ['ctx, 'res] lift_map_with_context =
  object(self)
    method virtual record : 'ctx -> (string * 'res) list -> 'res

    method virtual constr : 'ctx -> string -> 'res list -> 'res

    method virtual tuple : 'ctx -> 'res list -> 'res

    method virtual other : 'a. 'ctx -> 'a -> 'res

    method virtual bool : 'ctx -> bool -> bool * 'res

    method virtual char : 'ctx -> char -> char * 'res

    method virtual int : 'ctx -> int -> int * 'res

    method
    virtual
    list
    :
    'a. ('ctx -> 'a -> 'a * 'res) -> 'ctx -> 'a list -> 'a list * 'res

    method
    virtual
    option
    :
    'a. ('ctx -> 'a -> 'a * 'res) -> 'ctx -> 'a option -> 'a option * 'res

    method
    virtual
    ref
    :
    'a. ('ctx -> 'a -> 'a * 'res) -> 'ctx -> 'a ref -> 'a ref * 'res

    method virtual string : 'ctx -> string -> string * 'res

    method virtual token : 'ctx -> token -> token * 'res

    method position : 'ctx -> position -> position * 'res =
      fun ctx { pos_fname; pos_lnum; pos_bol; pos_cnum } ->
      let pos_fname = self#string ctx pos_fname in
      let pos_lnum = self#int ctx pos_lnum in
      let pos_bol = self#int ctx pos_bol in
      let pos_cnum = self#int ctx pos_cnum in
      ( { pos_fname = Stdlib.fst pos_fname
        ; pos_lnum = Stdlib.fst pos_lnum
        ; pos_bol = Stdlib.fst pos_bol
        ; pos_cnum = Stdlib.fst pos_cnum
        }
      , self
        #
        record
          ctx
          [ "pos_fname", Stdlib.snd pos_fname
          ; "pos_lnum", Stdlib.snd pos_lnum
          ; "pos_bol", Stdlib.snd pos_bol
          ; "pos_cnum", Stdlib.snd pos_cnum
          ] )

    method location : 'ctx -> location -> location * 'res =
      fun ctx { loc_start; loc_end; loc_ghost } ->
      let loc_start = self#position ctx loc_start in
      let loc_end = self#position ctx loc_end in
      let loc_ghost = self#bool ctx loc_ghost in
      ( { loc_start = Stdlib.fst loc_start
        ; loc_end = Stdlib.fst loc_end
        ; loc_ghost = Stdlib.fst loc_ghost
        }
      , self
        #
        record
          ctx
          [ "loc_start", Stdlib.snd loc_start
          ; "loc_end", Stdlib.snd loc_end
          ; "loc_ghost", Stdlib.snd loc_ghost
          ] )

    method longident_dotop_delims
      : 'ctx -> longident_dotop_delims -> longident_dotop_delims * 'res
      =
      fun ctx x -> x, self#other ctx x

    method longident_str_or_op
      : 'ctx -> longident_str_or_op -> longident_str_or_op * 'res
      =
      fun ctx x ->
      match x with
      | Str a ->
        let a = self#string ctx a in
        Str (Stdlib.fst a), self#constr ctx "Str" [ Stdlib.snd a ]
      | Str_trailing_hash a ->
        let a = self#string ctx a in
        ( Str_trailing_hash (Stdlib.fst a)
        , self#constr ctx "Str_trailing_hash" [ Stdlib.snd a ] )
      | Op a ->
        let a = self#string ctx a in
        Op (Stdlib.fst a), self#constr ctx "Op" [ Stdlib.snd a ]
      | DotOp (a, b, c, d) ->
        let a = self#string ctx a in
        let b = self#longident_dotop_delims ctx b in
        let c = self#string ctx c in
        let d = self#bool ctx d in
        ( DotOp (Stdlib.fst a, Stdlib.fst b, Stdlib.fst c, Stdlib.fst d)
        , self
          #
          constr
            ctx
            "DotOp"
            [ Stdlib.snd a; Stdlib.snd b; Stdlib.snd c; Stdlib.snd d ] )

    method longident_lid_desc
      : 'ctx -> longident_lid_desc -> longident_lid_desc * 'res
      =
      fun ctx x ->
      match x with
      | Lident a ->
        let a = self#longident_str_or_op ctx a in
        Lident (Stdlib.fst a), self#constr ctx "Lident" [ Stdlib.snd a ]
      | Ldot (a, b) ->
        let a = self#longident ctx a in
        let b = self#longident_str_or_op ctx b in
        ( Ldot (Stdlib.fst a, Stdlib.fst b)
        , self#constr ctx "Ldot" [ Stdlib.snd a; Stdlib.snd b ] )
      | Lapply (a, b) ->
        let a = self#longident ctx a in
        let b = self#longident ctx b in
        ( Lapply (Stdlib.fst a, Stdlib.fst b)
        , self#constr ctx "Lapply" [ Stdlib.snd a; Stdlib.snd b ] )

    method longident : 'ctx -> longident -> longident * 'res =
      fun ctx { desc; tokens } ->
      let desc = self#longident_lid_desc ctx desc in
      let tokens = self#token_seq ctx tokens in
      ( { desc = Stdlib.fst desc; tokens = Stdlib.fst tokens }
      , self
        #
        record
          ctx
          [ "desc", Stdlib.snd desc; "tokens", Stdlib.snd tokens ] )

    method attachment : 'ctx -> attachment -> attachment * 'res =
      fun ctx x -> x, self#other ctx x

    method comment : 'ctx -> comment -> comment * 'res =
      fun ctx { text; attachement; explicitely_inserted } ->
      let text = self#string ctx text in
      let attachement = self#attachment ctx attachement in
      let explicitely_inserted = self#ref self#bool ctx explicitely_inserted in
      ( { text = Stdlib.fst text
        ; attachement = Stdlib.fst attachement
        ; explicitely_inserted = Stdlib.fst explicitely_inserted
        }
      , self
        #
        record
          ctx
          [ "text", Stdlib.snd text
          ; "attachement", Stdlib.snd attachement
          ; "explicitely_inserted", Stdlib.snd explicitely_inserted
          ] )

    method token_desc : 'ctx -> token_desc -> token_desc * 'res =
      fun ctx x ->
      match x with
      | Token (a, b) ->
        let a = self#token ctx a in
        let b = self#bool ctx b in
        ( Token (Stdlib.fst a, Stdlib.fst b)
        , self#constr ctx "Token" [ Stdlib.snd a; Stdlib.snd b ] )
      | Comment a ->
        let a = self#comment ctx a in
        Comment (Stdlib.fst a), self#constr ctx "Comment" [ Stdlib.snd a ]
      | Child_node -> Child_node, self#constr ctx "Child_node" []

    method token_elt : 'ctx -> token_elt -> token_elt * 'res =
      fun ctx { desc; pos } ->
      let desc = self#token_desc ctx desc in
      let pos = self#position ctx pos in
      ( { desc = Stdlib.fst desc; pos = Stdlib.fst pos }
      , self#record ctx [ "desc", Stdlib.snd desc; "pos", Stdlib.snd pos ] )

    method token_seq : 'ctx -> token_seq -> token_seq * 'res =
      self#list self#token_elt

    method rec_flag : 'ctx -> rec_flag -> rec_flag * 'res =
      fun ctx x -> x, self#other ctx x

    method direction_flag : 'ctx -> direction_flag -> direction_flag * 'res =
      fun ctx x -> x, self#other ctx x

    method private_flag : 'ctx -> private_flag -> private_flag * 'res =
      fun ctx x -> x, self#other ctx x

    method mutable_flag : 'ctx -> mutable_flag -> mutable_flag * 'res =
      fun ctx x -> x, self#other ctx x

    method virtual_flag : 'ctx -> virtual_flag -> virtual_flag * 'res =
      fun ctx x -> x, self#other ctx x

    method override_flag : 'ctx -> override_flag -> override_flag * 'res =
      fun ctx x -> x, self#other ctx x

    method closed_flag : 'ctx -> closed_flag -> closed_flag * 'res =
      fun ctx x -> x, self#other ctx x

    method label : 'ctx -> label -> label * 'res = self#string

    method arg_label : 'ctx -> arg_label -> arg_label * 'res =
      fun ctx x ->
      match x with
      | Nolabel -> Nolabel, self#constr ctx "Nolabel" []
      | Labelled a ->
        let a = self#string ctx a in
        Labelled (Stdlib.fst a), self#constr ctx "Labelled" [ Stdlib.snd a ]
      | Optional a ->
        let a = self#string ctx a in
        Optional (Stdlib.fst a), self#constr ctx "Optional" [ Stdlib.snd a ]

    method loc
      : 'a. ('ctx -> 'a -> 'a * 'res) -> 'ctx -> 'a loc -> 'a loc * 'res
      =
      fun _a ctx { txt; loc } ->
      let txt = _a ctx txt in
      let loc = self#location ctx loc in
      ( { txt = Stdlib.fst txt; loc = Stdlib.fst loc }
      , self#record ctx [ "txt", Stdlib.snd txt; "loc", Stdlib.snd loc ] )

    method variance : 'ctx -> variance -> variance * 'res =
      fun ctx x -> x, self#other ctx x

    method injectivity : 'ctx -> injectivity -> injectivity * 'res =
      fun ctx x -> x, self#other ctx x

    method index_kind : 'ctx -> index_kind -> index_kind * 'res =
      fun ctx x -> x, self#other ctx x

    method paren_kind : 'ctx -> paren_kind -> paren_kind * 'res =
      fun ctx x -> x, self#other ctx x

    method constant : 'ctx -> constant -> constant * 'res =
      fun ctx x ->
      match x with
      | Pconst_integer (a, b, c) ->
        let a = self#option self#string ctx a in
        let b = self#string ctx b in
        let c = self#option self#char ctx c in
        ( Pconst_integer (Stdlib.fst a, Stdlib.fst b, Stdlib.fst c)
        , self
          #
          constr
            ctx
            "Pconst_integer"
            [ Stdlib.snd a; Stdlib.snd b; Stdlib.snd c ] )
      | Pconst_unboxed_integer (a, b, c) ->
        let a = self#option self#string ctx a in
        let b = self#string ctx b in
        let c = self#char ctx c in
        ( Pconst_unboxed_integer (Stdlib.fst a, Stdlib.fst b, Stdlib.fst c)
        , self
          #
          constr
            ctx
            "Pconst_unboxed_integer"
            [ Stdlib.snd a; Stdlib.snd b; Stdlib.snd c ] )
      | Pconst_char (a, b) ->
        let a = self#char ctx a in
        let b = self#string ctx b in
        ( Pconst_char (Stdlib.fst a, Stdlib.fst b)
        , self#constr ctx "Pconst_char" [ Stdlib.snd a; Stdlib.snd b ] )
      | Pconst_untagged_char (a, b) ->
        let a = self#char ctx a in
        let b = self#string ctx b in
        ( Pconst_untagged_char (Stdlib.fst a, Stdlib.fst b)
        , self#constr ctx "Pconst_untagged_char" [ Stdlib.snd a; Stdlib.snd b ] )
      | Pconst_string (a, b, c) ->
        let a = self#string ctx a in
        let b = self#location ctx b in
        let c = self#option self#string ctx c in
        ( Pconst_string (Stdlib.fst a, Stdlib.fst b, Stdlib.fst c)
        , self
          #
          constr
            ctx
            "Pconst_string"
            [ Stdlib.snd a; Stdlib.snd b; Stdlib.snd c ] )
      | Pconst_float (a, b, c) ->
        let a = self#option self#string ctx a in
        let b = self#string ctx b in
        let c = self#option self#char ctx c in
        ( Pconst_float (Stdlib.fst a, Stdlib.fst b, Stdlib.fst c)
        , self
          #
          constr
            ctx
            "Pconst_float"
            [ Stdlib.snd a; Stdlib.snd b; Stdlib.snd c ] )
      | Pconst_unboxed_float (a, b, c) ->
        let a = self#option self#string ctx a in
        let b = self#string ctx b in
        let c = self#option self#char ctx c in
        ( Pconst_unboxed_float (Stdlib.fst a, Stdlib.fst b, Stdlib.fst c)
        , self
          #
          constr
            ctx
            "Pconst_unboxed_float"
            [ Stdlib.snd a; Stdlib.snd b; Stdlib.snd c ] )

    method modality : 'ctx -> modality -> modality * 'res =
      fun ctx x ->
      match x with
      | Modality a ->
        let a = self#string ctx a in
        Modality (Stdlib.fst a), self#constr ctx "Modality" [ Stdlib.snd a ]

    method modalities : 'ctx -> modalities -> modalities * 'res =
      self#list (self#loc self#modality)

    method mode : 'ctx -> mode -> mode * 'res =
      fun ctx x ->
      match x with
      | Mode a ->
        let a = self#string ctx a in
        Mode (Stdlib.fst a), self#constr ctx "Mode" [ Stdlib.snd a ]

    method modes : 'ctx -> modes -> modes * 'res =
      self#list (self#loc self#mode)

    method include_kind : 'ctx -> include_kind -> include_kind * 'res =
      fun ctx x -> x, self#other ctx x

    method attribute : 'ctx -> attribute -> attribute * 'res =
      fun ctx { attr_name; attr_payload; attr_loc; attr_tokens } ->
      let attr_name = self#loc (self#list self#string) ctx attr_name in
      let attr_payload = self#payload ctx attr_payload in
      let attr_loc = self#location ctx attr_loc in
      let attr_tokens = self#token_seq ctx attr_tokens in
      ( { attr_name = Stdlib.fst attr_name
        ; attr_payload = Stdlib.fst attr_payload
        ; attr_loc = Stdlib.fst attr_loc
        ; attr_tokens = Stdlib.fst attr_tokens
        }
      , self
        #
        record
          ctx
          [ "attr_name", Stdlib.snd attr_name
          ; "attr_payload", Stdlib.snd attr_payload
          ; "attr_loc", Stdlib.snd attr_loc
          ; "attr_tokens", Stdlib.snd attr_tokens
          ] )

    method extension : 'ctx -> extension -> extension * 'res =
      fun ctx (a, b, c) ->
      let a = self#loc (self#list self#string) ctx a in
      let b = self#payload ctx b in
      let c = self#token_seq ctx c in
      ( (Stdlib.fst a, Stdlib.fst b, Stdlib.fst c)
      , self#tuple ctx [ Stdlib.snd a; Stdlib.snd b; Stdlib.snd c ] )

    method toplevel_extension
      : 'ctx -> toplevel_extension -> toplevel_extension * 'res
      =
      fun ctx { te_pre_doc; te_ext; te_attrs; te_post_doc } ->
      let te_pre_doc = self#option self#string ctx te_pre_doc in
      let te_ext = self#extension ctx te_ext in
      let te_attrs = self#attributes ctx te_attrs in
      let te_post_doc = self#option self#string ctx te_post_doc in
      ( { te_pre_doc = Stdlib.fst te_pre_doc
        ; te_ext = Stdlib.fst te_ext
        ; te_attrs = Stdlib.fst te_attrs
        ; te_post_doc = Stdlib.fst te_post_doc
        }
      , self
        #
        record
          ctx
          [ "te_pre_doc", Stdlib.snd te_pre_doc
          ; "te_ext", Stdlib.snd te_ext
          ; "te_attrs", Stdlib.snd te_attrs
          ; "te_post_doc", Stdlib.snd te_post_doc
          ] )

    method attributes : 'ctx -> attributes -> attributes * 'res =
      self#list self#attribute

    method payload : 'ctx -> payload -> payload * 'res =
      fun ctx x ->
      match x with
      | PStr a ->
        let a = self#structure ctx a in
        PStr (Stdlib.fst a), self#constr ctx "PStr" [ Stdlib.snd a ]
      | PSig a ->
        let a = self#signature ctx a in
        PSig (Stdlib.fst a), self#constr ctx "PSig" [ Stdlib.snd a ]
      | PTyp a ->
        let a = self#core_type ctx a in
        PTyp (Stdlib.fst a), self#constr ctx "PTyp" [ Stdlib.snd a ]
      | PPat (a, b) ->
        let a = self#pattern ctx a in
        let b = self#option self#expression ctx b in
        ( PPat (Stdlib.fst a, Stdlib.fst b)
        , self#constr ctx "PPat" [ Stdlib.snd a; Stdlib.snd b ] )
      | PString (a, b) ->
        let a = self#string ctx a in
        let b = self#string ctx b in
        ( PString (Stdlib.fst a, Stdlib.fst b)
        , self#constr ctx "PString" [ Stdlib.snd a; Stdlib.snd b ] )

    method ext_attribute : 'ctx -> ext_attribute -> ext_attribute * 'res =
      fun ctx { pea_ext; pea_attrs } ->
      let pea_ext =
        self#option (self#loc (self#list self#string)) ctx pea_ext
      in
      let pea_attrs = self#attributes ctx pea_attrs in
      ( { pea_ext = Stdlib.fst pea_ext; pea_attrs = Stdlib.fst pea_attrs }
      , self
        #
        record
          ctx
          [ "pea_ext", Stdlib.snd pea_ext; "pea_attrs", Stdlib.snd pea_attrs ] )

    method core_type : 'ctx -> core_type -> core_type * 'res =
      fun ctx { ptyp_desc; ptyp_loc; ptyp_attributes; ptyp_tokens } ->
      let ptyp_desc = self#core_type_desc ctx ptyp_desc in
      let ptyp_loc = self#location ctx ptyp_loc in
      let ptyp_attributes = self#attributes ctx ptyp_attributes in
      let ptyp_tokens = self#token_seq ctx ptyp_tokens in
      ( { ptyp_desc = Stdlib.fst ptyp_desc
        ; ptyp_loc = Stdlib.fst ptyp_loc
        ; ptyp_attributes = Stdlib.fst ptyp_attributes
        ; ptyp_tokens = Stdlib.fst ptyp_tokens
        }
      , self
        #
        record
          ctx
          [ "ptyp_desc", Stdlib.snd ptyp_desc
          ; "ptyp_loc", Stdlib.snd ptyp_loc
          ; "ptyp_attributes", Stdlib.snd ptyp_attributes
          ; "ptyp_tokens", Stdlib.snd ptyp_tokens
          ] )

    method arrow_arg : 'ctx -> arrow_arg -> arrow_arg * 'res =
      fun
        ctx
        { aa_lbl
        ; aa_legacy_modes
        ; aa_type
        ; aa_modes
        ; aa_doc
        ; aa_loc
        ; aa_tokens
        }
        ->
      let aa_lbl = self#arg_label ctx aa_lbl in
      let aa_legacy_modes = self#modes ctx aa_legacy_modes in
      let aa_type = self#core_type ctx aa_type in
      let aa_modes = self#modes ctx aa_modes in
      let aa_doc = self#option self#string ctx aa_doc in
      let aa_loc = self#location ctx aa_loc in
      let aa_tokens = self#token_seq ctx aa_tokens in
      ( { aa_lbl = Stdlib.fst aa_lbl
        ; aa_legacy_modes = Stdlib.fst aa_legacy_modes
        ; aa_type = Stdlib.fst aa_type
        ; aa_modes = Stdlib.fst aa_modes
        ; aa_doc = Stdlib.fst aa_doc
        ; aa_loc = Stdlib.fst aa_loc
        ; aa_tokens = Stdlib.fst aa_tokens
        }
      , self
        #
        record
          ctx
          [ "aa_lbl", Stdlib.snd aa_lbl
          ; "aa_legacy_modes", Stdlib.snd aa_legacy_modes
          ; "aa_type", Stdlib.snd aa_type
          ; "aa_modes", Stdlib.snd aa_modes
          ; "aa_doc", Stdlib.snd aa_doc
          ; "aa_loc", Stdlib.snd aa_loc
          ; "aa_tokens", Stdlib.snd aa_tokens
          ] )

    method core_type_desc : 'ctx -> core_type_desc -> core_type_desc * 'res =
      fun ctx x ->
      match x with
      | Ptyp_any a ->
        let a = self#option self#jkind_annotation ctx a in
        Ptyp_any (Stdlib.fst a), self#constr ctx "Ptyp_any" [ Stdlib.snd a ]
      | Ptyp_var (a, b) ->
        let a = self#string ctx a in
        let b = self#option self#jkind_annotation ctx b in
        ( Ptyp_var (Stdlib.fst a, Stdlib.fst b)
        , self#constr ctx "Ptyp_var" [ Stdlib.snd a; Stdlib.snd b ] )
      | Ptyp_arrow { domain; codom_legacy_modes; codom_type; codom_modes } ->
        let domain = self#arrow_arg ctx domain in
        let codom_legacy_modes = self#modes ctx codom_legacy_modes in
        let codom_type = self#core_type ctx codom_type in
        let codom_modes = self#modes ctx codom_modes in
        ( Ptyp_arrow
            { domain = Stdlib.fst domain
            ; codom_legacy_modes = Stdlib.fst codom_legacy_modes
            ; codom_type = Stdlib.fst codom_type
            ; codom_modes = Stdlib.fst codom_modes
            }
        , self
          #
          constr
            ctx
            "Ptyp_arrow"
            [ self
              #
              record
                ctx
                [ "domain", Stdlib.snd domain
                ; "codom_legacy_modes", Stdlib.snd codom_legacy_modes
                ; "codom_type", Stdlib.snd codom_type
                ; "codom_modes", Stdlib.snd codom_modes
                ]
            ] )
      | Ptyp_tuple a ->
        let a =
          self
          #
          list
            (fun ctx (a, b) ->
              let a = self#option self#string ctx a in
              let b = self#core_type ctx b in
              ( (Stdlib.fst a, Stdlib.fst b)
              , self#tuple ctx [ Stdlib.snd a; Stdlib.snd b ] ))
            ctx
            a
        in
        ( Ptyp_tuple (Stdlib.fst a)
        , self#constr ctx "Ptyp_tuple" [ Stdlib.snd a ] )
      | Ptyp_unboxed_tuple a ->
        let a =
          self
          #
          list
            (fun ctx (a, b) ->
              let a = self#option self#string ctx a in
              let b = self#core_type ctx b in
              ( (Stdlib.fst a, Stdlib.fst b)
              , self#tuple ctx [ Stdlib.snd a; Stdlib.snd b ] ))
            ctx
            a
        in
        ( Ptyp_unboxed_tuple (Stdlib.fst a)
        , self#constr ctx "Ptyp_unboxed_tuple" [ Stdlib.snd a ] )
      | Ptyp_constr (a, b) ->
        let a = self#list self#core_type ctx a in
        let b = self#loc self#longident ctx b in
        ( Ptyp_constr (Stdlib.fst a, Stdlib.fst b)
        , self#constr ctx "Ptyp_constr" [ Stdlib.snd a; Stdlib.snd b ] )
      | Ptyp_object (a, b) ->
        let a = self#list self#object_field ctx a in
        let b = self#closed_flag ctx b in
        ( Ptyp_object (Stdlib.fst a, Stdlib.fst b)
        , self#constr ctx "Ptyp_object" [ Stdlib.snd a; Stdlib.snd b ] )
      | Ptyp_class (a, b) ->
        let a = self#loc self#longident ctx a in
        let b = self#list self#core_type ctx b in
        ( Ptyp_class (Stdlib.fst a, Stdlib.fst b)
        , self#constr ctx "Ptyp_class" [ Stdlib.snd a; Stdlib.snd b ] )
      | Ptyp_alias (a, b, c) ->
        let a = self#core_type ctx a in
        let b = self#option (self#loc self#string) ctx b in
        let c = self#option self#jkind_annotation ctx c in
        ( Ptyp_alias (Stdlib.fst a, Stdlib.fst b, Stdlib.fst c)
        , self
          #
          constr
            ctx
            "Ptyp_alias"
            [ Stdlib.snd a; Stdlib.snd b; Stdlib.snd c ] )
      | Ptyp_variant (a, b, c) ->
        let a = self#list self#row_field ctx a in
        let b = self#closed_flag ctx b in
        let c = self#option (self#list self#label) ctx c in
        ( Ptyp_variant (Stdlib.fst a, Stdlib.fst b, Stdlib.fst c)
        , self
          #
          constr
            ctx
            "Ptyp_variant"
            [ Stdlib.snd a; Stdlib.snd b; Stdlib.snd c ] )
      | Ptyp_poly (a, b) ->
        let a =
          self
          #
          list
            (fun ctx (a, b) ->
              let a = self#loc self#string ctx a in
              let b = self#option self#jkind_annotation ctx b in
              ( (Stdlib.fst a, Stdlib.fst b)
              , self#tuple ctx [ Stdlib.snd a; Stdlib.snd b ] ))
            ctx
            a
        in
        let b = self#core_type ctx b in
        ( Ptyp_poly (Stdlib.fst a, Stdlib.fst b)
        , self#constr ctx "Ptyp_poly" [ Stdlib.snd a; Stdlib.snd b ] )
      | Ptyp_package (a, b) ->
        let a = self#ext_attribute ctx a in
        let b = self#package_type ctx b in
        ( Ptyp_package (Stdlib.fst a, Stdlib.fst b)
        , self#constr ctx "Ptyp_package" [ Stdlib.snd a; Stdlib.snd b ] )
      | Ptyp_open (a, b) ->
        let a = self#loc self#longident ctx a in
        let b = self#core_type ctx b in
        ( Ptyp_open (Stdlib.fst a, Stdlib.fst b)
        , self#constr ctx "Ptyp_open" [ Stdlib.snd a; Stdlib.snd b ] )
      | Ptyp_quote a ->
        let a = self#core_type ctx a in
        ( Ptyp_quote (Stdlib.fst a)
        , self#constr ctx "Ptyp_quote" [ Stdlib.snd a ] )
      | Ptyp_splice a ->
        let a = self#core_type ctx a in
        ( Ptyp_splice (Stdlib.fst a)
        , self#constr ctx "Ptyp_splice" [ Stdlib.snd a ] )
      | Ptyp_of_kind a ->
        let a = self#jkind_annotation ctx a in
        ( Ptyp_of_kind (Stdlib.fst a)
        , self#constr ctx "Ptyp_of_kind" [ Stdlib.snd a ] )
      | Ptyp_extension a ->
        let a = self#extension ctx a in
        ( Ptyp_extension (Stdlib.fst a)
        , self#constr ctx "Ptyp_extension" [ Stdlib.snd a ] )
      | Ptyp_parens a ->
        let a = self#core_type ctx a in
        ( Ptyp_parens (Stdlib.fst a)
        , self#constr ctx "Ptyp_parens" [ Stdlib.snd a ] )

    method package_type : 'ctx -> package_type -> package_type * 'res =
      self
      #
      module_type

    method row_field : 'ctx -> row_field -> row_field * 'res =
      fun ctx { prf_desc; prf_loc; prf_attributes; prf_doc; prf_tokens } ->
      let prf_desc = self#row_field_desc ctx prf_desc in
      let prf_loc = self#location ctx prf_loc in
      let prf_attributes = self#attributes ctx prf_attributes in
      let prf_doc = self#option self#string ctx prf_doc in
      let prf_tokens = self#token_seq ctx prf_tokens in
      ( { prf_desc = Stdlib.fst prf_desc
        ; prf_loc = Stdlib.fst prf_loc
        ; prf_attributes = Stdlib.fst prf_attributes
        ; prf_doc = Stdlib.fst prf_doc
        ; prf_tokens = Stdlib.fst prf_tokens
        }
      , self
        #
        record
          ctx
          [ "prf_desc", Stdlib.snd prf_desc
          ; "prf_loc", Stdlib.snd prf_loc
          ; "prf_attributes", Stdlib.snd prf_attributes
          ; "prf_doc", Stdlib.snd prf_doc
          ; "prf_tokens", Stdlib.snd prf_tokens
          ] )

    method row_field_desc : 'ctx -> row_field_desc -> row_field_desc * 'res =
      fun ctx x ->
      match x with
      | Rtag (a, b, c) ->
        let a = self#loc self#string ctx a in
        let b = self#bool ctx b in
        let c = self#list self#core_type ctx c in
        ( Rtag (Stdlib.fst a, Stdlib.fst b, Stdlib.fst c)
        , self#constr ctx "Rtag" [ Stdlib.snd a; Stdlib.snd b; Stdlib.snd c ] )
      | Rinherit a ->
        let a = self#core_type ctx a in
        Rinherit (Stdlib.fst a), self#constr ctx "Rinherit" [ Stdlib.snd a ]

    method object_field : 'ctx -> object_field -> object_field * 'res =
      fun ctx { pof_desc; pof_loc; pof_attributes; pof_doc; pof_tokens } ->
      let pof_desc = self#object_field_desc ctx pof_desc in
      let pof_loc = self#location ctx pof_loc in
      let pof_attributes = self#attributes ctx pof_attributes in
      let pof_doc = self#option self#string ctx pof_doc in
      let pof_tokens = self#token_seq ctx pof_tokens in
      ( { pof_desc = Stdlib.fst pof_desc
        ; pof_loc = Stdlib.fst pof_loc
        ; pof_attributes = Stdlib.fst pof_attributes
        ; pof_doc = Stdlib.fst pof_doc
        ; pof_tokens = Stdlib.fst pof_tokens
        }
      , self
        #
        record
          ctx
          [ "pof_desc", Stdlib.snd pof_desc
          ; "pof_loc", Stdlib.snd pof_loc
          ; "pof_attributes", Stdlib.snd pof_attributes
          ; "pof_doc", Stdlib.snd pof_doc
          ; "pof_tokens", Stdlib.snd pof_tokens
          ] )

    method object_field_desc
      : 'ctx -> object_field_desc -> object_field_desc * 'res
      =
      fun ctx x ->
      match x with
      | Otag (a, b) ->
        let a = self#loc self#string ctx a in
        let b = self#core_type ctx b in
        ( Otag (Stdlib.fst a, Stdlib.fst b)
        , self#constr ctx "Otag" [ Stdlib.snd a; Stdlib.snd b ] )
      | Oinherit a ->
        let a = self#core_type ctx a in
        Oinherit (Stdlib.fst a), self#constr ctx "Oinherit" [ Stdlib.snd a ]

    method pattern : 'ctx -> pattern -> pattern * 'res =
      fun
        ctx { ppat_ext_attr; ppat_desc; ppat_loc; ppat_attributes; ppat_tokens }
        ->
      let ppat_ext_attr = self#ext_attribute ctx ppat_ext_attr in
      let ppat_desc = self#pattern_desc ctx ppat_desc in
      let ppat_loc = self#location ctx ppat_loc in
      let ppat_attributes = self#attributes ctx ppat_attributes in
      let ppat_tokens = self#token_seq ctx ppat_tokens in
      ( { ppat_ext_attr = Stdlib.fst ppat_ext_attr
        ; ppat_desc = Stdlib.fst ppat_desc
        ; ppat_loc = Stdlib.fst ppat_loc
        ; ppat_attributes = Stdlib.fst ppat_attributes
        ; ppat_tokens = Stdlib.fst ppat_tokens
        }
      , self
        #
        record
          ctx
          [ "ppat_ext_attr", Stdlib.snd ppat_ext_attr
          ; "ppat_desc", Stdlib.snd ppat_desc
          ; "ppat_loc", Stdlib.snd ppat_loc
          ; "ppat_attributes", Stdlib.snd ppat_attributes
          ; "ppat_tokens", Stdlib.snd ppat_tokens
          ] )

    method pattern_desc : 'ctx -> pattern_desc -> pattern_desc * 'res =
      fun ctx x ->
      match x with
      | Ppat_any -> Ppat_any, self#constr ctx "Ppat_any" []
      | Ppat_var a ->
        let a = self#loc self#longident_str_or_op ctx a in
        Ppat_var (Stdlib.fst a), self#constr ctx "Ppat_var" [ Stdlib.snd a ]
      | Ppat_alias (a, b) ->
        let a = self#pattern ctx a in
        let b = self#loc self#longident_str_or_op ctx b in
        ( Ppat_alias (Stdlib.fst a, Stdlib.fst b)
        , self#constr ctx "Ppat_alias" [ Stdlib.snd a; Stdlib.snd b ] )
      | Ppat_constant a ->
        let a = self#constant ctx a in
        ( Ppat_constant (Stdlib.fst a)
        , self#constr ctx "Ppat_constant" [ Stdlib.snd a ] )
      | Ppat_interval (a, b) ->
        let a = self#constant ctx a in
        let b = self#constant ctx b in
        ( Ppat_interval (Stdlib.fst a, Stdlib.fst b)
        , self#constr ctx "Ppat_interval" [ Stdlib.snd a; Stdlib.snd b ] )
      | Ppat_tuple (a, b) ->
        let a = self#list (self#argument self#pattern) ctx a in
        let b = self#closed_flag ctx b in
        ( Ppat_tuple (Stdlib.fst a, Stdlib.fst b)
        , self#constr ctx "Ppat_tuple" [ Stdlib.snd a; Stdlib.snd b ] )
      | Ppat_unboxed_tuple (a, b) ->
        let a = self#list (self#argument self#pattern) ctx a in
        let b = self#closed_flag ctx b in
        ( Ppat_unboxed_tuple (Stdlib.fst a, Stdlib.fst b)
        , self#constr ctx "Ppat_unboxed_tuple" [ Stdlib.snd a; Stdlib.snd b ] )
      | Ppat_construct (a, b) ->
        let a = self#loc self#longident ctx a in
        let b =
          self
          #
          option
            (fun ctx (a, b) ->
              let a =
                self
                #
                list
                  (fun ctx (a, b) ->
                    let a = self#loc self#string ctx a in
                    let b = self#option self#jkind_annotation ctx b in
                    ( (Stdlib.fst a, Stdlib.fst b)
                    , self#tuple ctx [ Stdlib.snd a; Stdlib.snd b ] ))
                  ctx
                  a
              in
              let b = self#pattern ctx b in
              ( (Stdlib.fst a, Stdlib.fst b)
              , self#tuple ctx [ Stdlib.snd a; Stdlib.snd b ] ))
            ctx
            b
        in
        ( Ppat_construct (Stdlib.fst a, Stdlib.fst b)
        , self#constr ctx "Ppat_construct" [ Stdlib.snd a; Stdlib.snd b ] )
      | Ppat_variant (a, b) ->
        let a = self#label ctx a in
        let b = self#option self#pattern ctx b in
        ( Ppat_variant (Stdlib.fst a, Stdlib.fst b)
        , self#constr ctx "Ppat_variant" [ Stdlib.snd a; Stdlib.snd b ] )
      | Ppat_record (a, b) ->
        let a = self#list (self#record_field self#pattern) ctx a in
        let b = self#closed_flag ctx b in
        ( Ppat_record (Stdlib.fst a, Stdlib.fst b)
        , self#constr ctx "Ppat_record" [ Stdlib.snd a; Stdlib.snd b ] )
      | Ppat_record_unboxed_product (a, b) ->
        let a = self#list (self#record_field self#pattern) ctx a in
        let b = self#closed_flag ctx b in
        ( Ppat_record_unboxed_product (Stdlib.fst a, Stdlib.fst b)
        , self
          #
          constr
            ctx
            "Ppat_record_unboxed_product"
            [ Stdlib.snd a; Stdlib.snd b ] )
      | Ppat_array (a, b) ->
        let a = self#mutable_flag ctx a in
        let b = self#list self#pattern ctx b in
        ( Ppat_array (Stdlib.fst a, Stdlib.fst b)
        , self#constr ctx "Ppat_array" [ Stdlib.snd a; Stdlib.snd b ] )
      | Ppat_or (a, b) ->
        let a = self#pattern ctx a in
        let b = self#pattern ctx b in
        ( Ppat_or (Stdlib.fst a, Stdlib.fst b)
        , self#constr ctx "Ppat_or" [ Stdlib.snd a; Stdlib.snd b ] )
      | Ppat_constraint (a, b, c) ->
        let a = self#pattern ctx a in
        let b = self#option self#core_type ctx b in
        let c = self#modes ctx c in
        ( Ppat_constraint (Stdlib.fst a, Stdlib.fst b, Stdlib.fst c)
        , self
          #
          constr
            ctx
            "Ppat_constraint"
            [ Stdlib.snd a; Stdlib.snd b; Stdlib.snd c ] )
      | Ppat_type a ->
        let a = self#loc self#longident ctx a in
        Ppat_type (Stdlib.fst a), self#constr ctx "Ppat_type" [ Stdlib.snd a ]
      | Ppat_lazy a ->
        let a = self#pattern ctx a in
        Ppat_lazy (Stdlib.fst a), self#constr ctx "Ppat_lazy" [ Stdlib.snd a ]
      | Ppat_unpack (a, b) ->
        let a = self#loc (self#option self#string) ctx a in
        let b = self#option self#package_type ctx b in
        ( Ppat_unpack (Stdlib.fst a, Stdlib.fst b)
        , self#constr ctx "Ppat_unpack" [ Stdlib.snd a; Stdlib.snd b ] )
      | Ppat_exception a ->
        let a = self#pattern ctx a in
        ( Ppat_exception (Stdlib.fst a)
        , self#constr ctx "Ppat_exception" [ Stdlib.snd a ] )
      | Ppat_extension a ->
        let a = self#extension ctx a in
        ( Ppat_extension (Stdlib.fst a)
        , self#constr ctx "Ppat_extension" [ Stdlib.snd a ] )
      | Ppat_open (a, b) ->
        let a = self#loc self#longident ctx a in
        let b = self#pattern ctx b in
        ( Ppat_open (Stdlib.fst a, Stdlib.fst b)
        , self#constr ctx "Ppat_open" [ Stdlib.snd a; Stdlib.snd b ] )
      | Ppat_parens { pat; optional } ->
        let pat = self#pattern ctx pat in
        let optional = self#bool ctx optional in
        ( Ppat_parens { pat = Stdlib.fst pat; optional = Stdlib.fst optional }
        , self
          #
          constr
            ctx
            "Ppat_parens"
            [ self
              #
              record
                ctx
                [ "pat", Stdlib.snd pat; "optional", Stdlib.snd optional ]
            ] )
      | Ppat_list a ->
        let a = self#list self#pattern ctx a in
        Ppat_list (Stdlib.fst a), self#constr ctx "Ppat_list" [ Stdlib.snd a ]
      | Ppat_cons (a, b) ->
        let a = self#pattern ctx a in
        let b = self#pattern ctx b in
        ( Ppat_cons (Stdlib.fst a, Stdlib.fst b)
        , self#constr ctx "Ppat_cons" [ Stdlib.snd a; Stdlib.snd b ] )

    method expression : 'ctx -> expression -> expression * 'res =
      fun
        ctx { pexp_ext_attr; pexp_desc; pexp_loc; pexp_attributes; pexp_tokens }
        ->
      let pexp_ext_attr = self#ext_attribute ctx pexp_ext_attr in
      let pexp_desc = self#expression_desc ctx pexp_desc in
      let pexp_loc = self#location ctx pexp_loc in
      let pexp_attributes = self#attributes ctx pexp_attributes in
      let pexp_tokens = self#token_seq ctx pexp_tokens in
      ( { pexp_ext_attr = Stdlib.fst pexp_ext_attr
        ; pexp_desc = Stdlib.fst pexp_desc
        ; pexp_loc = Stdlib.fst pexp_loc
        ; pexp_attributes = Stdlib.fst pexp_attributes
        ; pexp_tokens = Stdlib.fst pexp_tokens
        }
      , self
        #
        record
          ctx
          [ "pexp_ext_attr", Stdlib.snd pexp_ext_attr
          ; "pexp_desc", Stdlib.snd pexp_desc
          ; "pexp_loc", Stdlib.snd pexp_loc
          ; "pexp_attributes", Stdlib.snd pexp_attributes
          ; "pexp_tokens", Stdlib.snd pexp_tokens
          ] )

    method expression_desc : 'ctx -> expression_desc -> expression_desc * 'res =
      fun ctx x ->
      match x with
      | Pexp_ident a ->
        let a = self#loc self#longident ctx a in
        ( Pexp_ident (Stdlib.fst a)
        , self#constr ctx "Pexp_ident" [ Stdlib.snd a ] )
      | Pexp_constant a ->
        let a = self#constant ctx a in
        ( Pexp_constant (Stdlib.fst a)
        , self#constr ctx "Pexp_constant" [ Stdlib.snd a ] )
      | Pexp_let (a, b, c, d) ->
        let a = self#mutable_flag ctx a in
        let b = self#rec_flag ctx b in
        let c = self#list self#value_binding ctx c in
        let d = self#expression ctx d in
        ( Pexp_let (Stdlib.fst a, Stdlib.fst b, Stdlib.fst c, Stdlib.fst d)
        , self
          #
          constr
            ctx
            "Pexp_let"
            [ Stdlib.snd a; Stdlib.snd b; Stdlib.snd c; Stdlib.snd d ] )
      | Pexp_function (a, b, c) ->
        let a = self#list self#function_param ctx a in
        let b = self#function_constraint ctx b in
        let c = self#function_body ctx c in
        ( Pexp_function (Stdlib.fst a, Stdlib.fst b, Stdlib.fst c)
        , self
          #
          constr
            ctx
            "Pexp_function"
            [ Stdlib.snd a; Stdlib.snd b; Stdlib.snd c ] )
      | Pexp_prefix_apply (a, b) ->
        let a = self#expression ctx a in
        let b = self#expression ctx b in
        ( Pexp_prefix_apply (Stdlib.fst a, Stdlib.fst b)
        , self#constr ctx "Pexp_prefix_apply" [ Stdlib.snd a; Stdlib.snd b ] )
      | Pexp_add_or_sub (a, b) ->
        let a = self#string ctx a in
        let b = self#expression ctx b in
        ( Pexp_add_or_sub (Stdlib.fst a, Stdlib.fst b)
        , self#constr ctx "Pexp_add_or_sub" [ Stdlib.snd a; Stdlib.snd b ] )
      | Pexp_infix_apply { arg1; op; arg2 } ->
        let arg1 = self#expression ctx arg1 in
        let op = self#expression ctx op in
        let arg2 = self#expression ctx arg2 in
        ( Pexp_infix_apply
            { arg1 = Stdlib.fst arg1
            ; op = Stdlib.fst op
            ; arg2 = Stdlib.fst arg2
            }
        , self
          #
          constr
            ctx
            "Pexp_infix_apply"
            [ self
              #
              record
                ctx
                [ "arg1", Stdlib.snd arg1
                ; "op", Stdlib.snd op
                ; "arg2", Stdlib.snd arg2
                ]
            ] )
      | Pexp_apply (a, b) ->
        let a = self#expression ctx a in
        let b = self#list (self#argument self#expression) ctx b in
        ( Pexp_apply (Stdlib.fst a, Stdlib.fst b)
        , self#constr ctx "Pexp_apply" [ Stdlib.snd a; Stdlib.snd b ] )
      | Pexp_match (a, b) ->
        let a = self#expression ctx a in
        let b = self#list self#case ctx b in
        ( Pexp_match (Stdlib.fst a, Stdlib.fst b)
        , self#constr ctx "Pexp_match" [ Stdlib.snd a; Stdlib.snd b ] )
      | Pexp_try (a, b) ->
        let a = self#expression ctx a in
        let b = self#list self#case ctx b in
        ( Pexp_try (Stdlib.fst a, Stdlib.fst b)
        , self#constr ctx "Pexp_try" [ Stdlib.snd a; Stdlib.snd b ] )
      | Pexp_tuple a ->
        let a = self#list (self#argument self#expression) ctx a in
        ( Pexp_tuple (Stdlib.fst a)
        , self#constr ctx "Pexp_tuple" [ Stdlib.snd a ] )
      | Pexp_unboxed_tuple a ->
        let a = self#list (self#argument self#expression) ctx a in
        ( Pexp_unboxed_tuple (Stdlib.fst a)
        , self#constr ctx "Pexp_unboxed_tuple" [ Stdlib.snd a ] )
      | Pexp_construct (a, b) ->
        let a = self#loc self#longident ctx a in
        let b = self#option self#expression ctx b in
        ( Pexp_construct (Stdlib.fst a, Stdlib.fst b)
        , self#constr ctx "Pexp_construct" [ Stdlib.snd a; Stdlib.snd b ] )
      | Pexp_variant (a, b) ->
        let a = self#label ctx a in
        let b = self#option self#expression ctx b in
        ( Pexp_variant (Stdlib.fst a, Stdlib.fst b)
        , self#constr ctx "Pexp_variant" [ Stdlib.snd a; Stdlib.snd b ] )
      | Pexp_record (a, b) ->
        let a = self#option self#expression ctx a in
        let b = self#list (self#record_field self#expression) ctx b in
        ( Pexp_record (Stdlib.fst a, Stdlib.fst b)
        , self#constr ctx "Pexp_record" [ Stdlib.snd a; Stdlib.snd b ] )
      | Pexp_record_unboxed_product (a, b) ->
        let a = self#option self#expression ctx a in
        let b = self#list (self#record_field self#expression) ctx b in
        ( Pexp_record_unboxed_product (Stdlib.fst a, Stdlib.fst b)
        , self
          #
          constr
            ctx
            "Pexp_record_unboxed_product"
            [ Stdlib.snd a; Stdlib.snd b ] )
      | Pexp_field (a, b) ->
        let a = self#expression ctx a in
        let b = self#loc self#longident ctx b in
        ( Pexp_field (Stdlib.fst a, Stdlib.fst b)
        , self#constr ctx "Pexp_field" [ Stdlib.snd a; Stdlib.snd b ] )
      | Pexp_unboxed_field (a, b) ->
        let a = self#expression ctx a in
        let b = self#loc self#longident ctx b in
        ( Pexp_unboxed_field (Stdlib.fst a, Stdlib.fst b)
        , self#constr ctx "Pexp_unboxed_field" [ Stdlib.snd a; Stdlib.snd b ] )
      | Pexp_setfield (a, b, c) ->
        let a = self#expression ctx a in
        let b = self#loc self#longident ctx b in
        let c = self#expression ctx c in
        ( Pexp_setfield (Stdlib.fst a, Stdlib.fst b, Stdlib.fst c)
        , self
          #
          constr
            ctx
            "Pexp_setfield"
            [ Stdlib.snd a; Stdlib.snd b; Stdlib.snd c ] )
      | Pexp_array (a, b) ->
        let a = self#mutable_flag ctx a in
        let b = self#list self#expression ctx b in
        ( Pexp_array (Stdlib.fst a, Stdlib.fst b)
        , self#constr ctx "Pexp_array" [ Stdlib.snd a; Stdlib.snd b ] )
      | Pexp_idx (a, b) ->
        let a = self#block_access ctx a in
        let b = self#list self#unboxed_access ctx b in
        ( Pexp_idx (Stdlib.fst a, Stdlib.fst b)
        , self#constr ctx "Pexp_idx" [ Stdlib.snd a; Stdlib.snd b ] )
      | Pexp_ifthenelse (a, b, c) ->
        let a = self#expression ctx a in
        let b = self#expression ctx b in
        let c = self#option self#expression ctx c in
        ( Pexp_ifthenelse (Stdlib.fst a, Stdlib.fst b, Stdlib.fst c)
        , self
          #
          constr
            ctx
            "Pexp_ifthenelse"
            [ Stdlib.snd a; Stdlib.snd b; Stdlib.snd c ] )
      | Pexp_sequence (a, b) ->
        let a = self#expression ctx a in
        let b = self#expression ctx b in
        ( Pexp_sequence (Stdlib.fst a, Stdlib.fst b)
        , self#constr ctx "Pexp_sequence" [ Stdlib.snd a; Stdlib.snd b ] )
      | Pexp_seq_empty a ->
        let a = self#expression ctx a in
        ( Pexp_seq_empty (Stdlib.fst a)
        , self#constr ctx "Pexp_seq_empty" [ Stdlib.snd a ] )
      | Pexp_while (a, b) ->
        let a = self#expression ctx a in
        let b = self#expression ctx b in
        ( Pexp_while (Stdlib.fst a, Stdlib.fst b)
        , self#constr ctx "Pexp_while" [ Stdlib.snd a; Stdlib.snd b ] )
      | Pexp_for (a, b, c, d, e) ->
        let a = self#pattern ctx a in
        let b = self#expression ctx b in
        let c = self#expression ctx c in
        let d = self#direction_flag ctx d in
        let e = self#expression ctx e in
        ( Pexp_for
            ( Stdlib.fst a
            , Stdlib.fst b
            , Stdlib.fst c
            , Stdlib.fst d
            , Stdlib.fst e )
        , self
          #
          constr
            ctx
            "Pexp_for"
            [ Stdlib.snd a
            ; Stdlib.snd b
            ; Stdlib.snd c
            ; Stdlib.snd d
            ; Stdlib.snd e
            ] )
      | Pexp_constraint (a, b, c) ->
        let a = self#expression ctx a in
        let b = self#option self#core_type ctx b in
        let c = self#modes ctx c in
        ( Pexp_constraint (Stdlib.fst a, Stdlib.fst b, Stdlib.fst c)
        , self
          #
          constr
            ctx
            "Pexp_constraint"
            [ Stdlib.snd a; Stdlib.snd b; Stdlib.snd c ] )
      | Pexp_coerce (a, b, c) ->
        let a = self#expression ctx a in
        let b = self#option self#core_type ctx b in
        let c = self#core_type ctx c in
        ( Pexp_coerce (Stdlib.fst a, Stdlib.fst b, Stdlib.fst c)
        , self
          #
          constr
            ctx
            "Pexp_coerce"
            [ Stdlib.snd a; Stdlib.snd b; Stdlib.snd c ] )
      | Pexp_send (a, b) ->
        let a = self#expression ctx a in
        let b = self#loc self#string ctx b in
        ( Pexp_send (Stdlib.fst a, Stdlib.fst b)
        , self#constr ctx "Pexp_send" [ Stdlib.snd a; Stdlib.snd b ] )
      | Pexp_new a ->
        let a = self#loc self#longident ctx a in
        Pexp_new (Stdlib.fst a), self#constr ctx "Pexp_new" [ Stdlib.snd a ]
      | Pexp_setvar (a, b) ->
        let a = self#loc self#string ctx a in
        let b = self#expression ctx b in
        ( Pexp_setvar (Stdlib.fst a, Stdlib.fst b)
        , self#constr ctx "Pexp_setvar" [ Stdlib.snd a; Stdlib.snd b ] )
      | Pexp_override a ->
        let a =
          self
          #
          list
            (fun ctx (a, b) ->
              let a = self#loc self#string ctx a in
              let b = self#option self#expression ctx b in
              ( (Stdlib.fst a, Stdlib.fst b)
              , self#tuple ctx [ Stdlib.snd a; Stdlib.snd b ] ))
            ctx
            a
        in
        ( Pexp_override (Stdlib.fst a)
        , self#constr ctx "Pexp_override" [ Stdlib.snd a ] )
      | Pexp_letmodule (a, b) ->
        let a = self#module_binding ctx a in
        let b = self#expression ctx b in
        ( Pexp_letmodule (Stdlib.fst a, Stdlib.fst b)
        , self#constr ctx "Pexp_letmodule" [ Stdlib.snd a; Stdlib.snd b ] )
      | Pexp_letexception (a, b) ->
        let a = self#extension_constructor ctx a in
        let b = self#expression ctx b in
        ( Pexp_letexception (Stdlib.fst a, Stdlib.fst b)
        , self#constr ctx "Pexp_letexception" [ Stdlib.snd a; Stdlib.snd b ] )
      | Pexp_assert a ->
        let a = self#expression ctx a in
        ( Pexp_assert (Stdlib.fst a)
        , self#constr ctx "Pexp_assert" [ Stdlib.snd a ] )
      | Pexp_lazy a ->
        let a = self#expression ctx a in
        Pexp_lazy (Stdlib.fst a), self#constr ctx "Pexp_lazy" [ Stdlib.snd a ]
      | Pexp_object a ->
        let a = self#class_structure ctx a in
        ( Pexp_object (Stdlib.fst a)
        , self#constr ctx "Pexp_object" [ Stdlib.snd a ] )
      | Pexp_pack (a, b) ->
        let a = self#module_expr ctx a in
        let b = self#option self#package_type ctx b in
        ( Pexp_pack (Stdlib.fst a, Stdlib.fst b)
        , self#constr ctx "Pexp_pack" [ Stdlib.snd a; Stdlib.snd b ] )
      | Pexp_dot_open (a, b) ->
        let a = self#loc self#longident ctx a in
        let b = self#expression ctx b in
        ( Pexp_dot_open (Stdlib.fst a, Stdlib.fst b)
        , self#constr ctx "Pexp_dot_open" [ Stdlib.snd a; Stdlib.snd b ] )
      | Pexp_let_open (a, b) ->
        let a = self#open_declaration ctx a in
        let b = self#expression ctx b in
        ( Pexp_let_open (Stdlib.fst a, Stdlib.fst b)
        , self#constr ctx "Pexp_let_open" [ Stdlib.snd a; Stdlib.snd b ] )
      | Pexp_letop a ->
        let a = self#letop ctx a in
        ( Pexp_letop (Stdlib.fst a)
        , self#constr ctx "Pexp_letop" [ Stdlib.snd a ] )
      | Pexp_extension a ->
        let a = self#extension ctx a in
        ( Pexp_extension (Stdlib.fst a)
        , self#constr ctx "Pexp_extension" [ Stdlib.snd a ] )
      | Pexp_unreachable ->
        Pexp_unreachable, self#constr ctx "Pexp_unreachable" []
      | Pexp_stack a ->
        let a = self#expression ctx a in
        ( Pexp_stack (Stdlib.fst a)
        , self#constr ctx "Pexp_stack" [ Stdlib.snd a ] )
      | Pexp_comprehension a ->
        let a = self#comprehension_expression ctx a in
        ( Pexp_comprehension (Stdlib.fst a)
        , self#constr ctx "Pexp_comprehension" [ Stdlib.snd a ] )
      | Pexp_overwrite (a, b) ->
        let a = self#expression ctx a in
        let b = self#expression ctx b in
        ( Pexp_overwrite (Stdlib.fst a, Stdlib.fst b)
        , self#constr ctx "Pexp_overwrite" [ Stdlib.snd a; Stdlib.snd b ] )
      | Pexp_quote a ->
        let a = self#expression ctx a in
        ( Pexp_quote (Stdlib.fst a)
        , self#constr ctx "Pexp_quote" [ Stdlib.snd a ] )
      | Pexp_splice a ->
        let a = self#expression ctx a in
        ( Pexp_splice (Stdlib.fst a)
        , self#constr ctx "Pexp_splice" [ Stdlib.snd a ] )
      | Pexp_hole -> Pexp_hole, self#constr ctx "Pexp_hole" []
      | Pexp_index_op { kind; op; seq; indices; assign } ->
        let kind = self#paren_kind ctx kind in
        let op =
          self
          #
          option
            (fun ctx (a, b) ->
              let a = self#option self#longident ctx a in
              let b = self#string ctx b in
              ( (Stdlib.fst a, Stdlib.fst b)
              , self#tuple ctx [ Stdlib.snd a; Stdlib.snd b ] ))
            ctx
            op
        in
        let seq = self#expression ctx seq in
        let indices = self#list self#expression ctx indices in
        let assign = self#option self#expression ctx assign in
        ( Pexp_index_op
            { kind = Stdlib.fst kind
            ; op = Stdlib.fst op
            ; seq = Stdlib.fst seq
            ; indices = Stdlib.fst indices
            ; assign = Stdlib.fst assign
            }
        , self
          #
          constr
            ctx
            "Pexp_index_op"
            [ self
              #
              record
                ctx
                [ "kind", Stdlib.snd kind
                ; "op", Stdlib.snd op
                ; "seq", Stdlib.snd seq
                ; "indices", Stdlib.snd indices
                ; "assign", Stdlib.snd assign
                ]
            ] )
      | Pexp_parens { exp; optional } ->
        let exp = self#expression ctx exp in
        let optional = self#bool ctx optional in
        ( Pexp_parens { exp = Stdlib.fst exp; optional = Stdlib.fst optional }
        , self
          #
          constr
            ctx
            "Pexp_parens"
            [ self
              #
              record
                ctx
                [ "exp", Stdlib.snd exp; "optional", Stdlib.snd optional ]
            ] )
      | Pexp_begin_end a ->
        let a = self#option self#expression ctx a in
        ( Pexp_begin_end (Stdlib.fst a)
        , self#constr ctx "Pexp_begin_end" [ Stdlib.snd a ] )
      | Pexp_list a ->
        let a = self#list self#expression ctx a in
        Pexp_list (Stdlib.fst a), self#constr ctx "Pexp_list" [ Stdlib.snd a ]
      | Pexp_cons (a, b) ->
        let a = self#expression ctx a in
        let b = self#expression ctx b in
        ( Pexp_cons (Stdlib.fst a, Stdlib.fst b)
        , self#constr ctx "Pexp_cons" [ Stdlib.snd a; Stdlib.snd b ] )
      | Pexp_exclave a ->
        let a = self#expression ctx a in
        ( Pexp_exclave (Stdlib.fst a)
        , self#constr ctx "Pexp_exclave" [ Stdlib.snd a ] )
      | Pexp_mode_legacy (a, b) ->
        let a = self#loc self#mode ctx a in
        let b = self#expression ctx b in
        ( Pexp_mode_legacy (Stdlib.fst a, Stdlib.fst b)
        , self#constr ctx "Pexp_mode_legacy" [ Stdlib.snd a; Stdlib.snd b ] )

    method record_field
      :
        'a
        .  ('ctx -> 'a -> 'a * 'res)
        -> 'ctx
        -> 'a record_field
        -> 'a record_field * 'res
      =
      fun _a ctx { field_name; typ; value } ->
      let field_name = self#loc self#longident ctx field_name in
      let typ = self#option self#type_constraint ctx typ in
      let value = self#option _a ctx value in
      ( { field_name = Stdlib.fst field_name
        ; typ = Stdlib.fst typ
        ; value = Stdlib.fst value
        }
      , self
        #
        record
          ctx
          [ "field_name", Stdlib.snd field_name
          ; "typ", Stdlib.snd typ
          ; "value", Stdlib.snd value
          ] )

    method case : 'ctx -> case -> case * 'res =
      fun ctx { pc_lhs; pc_guard; pc_rhs; pc_tokens } ->
      let pc_lhs = self#pattern ctx pc_lhs in
      let pc_guard = self#option self#expression ctx pc_guard in
      let pc_rhs = self#expression ctx pc_rhs in
      let pc_tokens = self#token_seq ctx pc_tokens in
      ( { pc_lhs = Stdlib.fst pc_lhs
        ; pc_guard = Stdlib.fst pc_guard
        ; pc_rhs = Stdlib.fst pc_rhs
        ; pc_tokens = Stdlib.fst pc_tokens
        }
      , self
        #
        record
          ctx
          [ "pc_lhs", Stdlib.snd pc_lhs
          ; "pc_guard", Stdlib.snd pc_guard
          ; "pc_rhs", Stdlib.snd pc_rhs
          ; "pc_tokens", Stdlib.snd pc_tokens
          ] )

    method letop : 'ctx -> letop -> letop * 'res =
      fun ctx { let_; ands; body } ->
      let let_ = self#binding_op ctx let_ in
      let ands = self#list self#binding_op ctx ands in
      let body = self#expression ctx body in
      ( { let_ = Stdlib.fst let_
        ; ands = Stdlib.fst ands
        ; body = Stdlib.fst body
        }
      , self
        #
        record
          ctx
          [ "let_", Stdlib.snd let_
          ; "ands", Stdlib.snd ands
          ; "body", Stdlib.snd body
          ] )

    method binding_op : 'ctx -> binding_op -> binding_op * 'res =
      fun ctx { pbop_op; pbop_binding; pbop_loc } ->
      let pbop_op = self#loc self#string ctx pbop_op in
      let pbop_binding = self#value_binding ctx pbop_binding in
      let pbop_loc = self#location ctx pbop_loc in
      ( { pbop_op = Stdlib.fst pbop_op
        ; pbop_binding = Stdlib.fst pbop_binding
        ; pbop_loc = Stdlib.fst pbop_loc
        }
      , self
        #
        record
          ctx
          [ "pbop_op", Stdlib.snd pbop_op
          ; "pbop_binding", Stdlib.snd pbop_binding
          ; "pbop_loc", Stdlib.snd pbop_loc
          ] )

    method argument_desc
      :
        'a
        .  ('ctx -> 'a -> 'a * 'res)
        -> 'ctx
        -> 'a argument_desc
        -> 'a argument_desc * 'res
      =
      fun _a ctx x ->
      match x with
      | Parg_unlabelled { legacy_modes; arg; typ_constraint; modes } ->
        let legacy_modes = self#modes ctx legacy_modes in
        let arg = _a ctx arg in
        let typ_constraint =
          self#option self#type_constraint ctx typ_constraint
        in
        let modes = self#modes ctx modes in
        ( Parg_unlabelled
            { legacy_modes = Stdlib.fst legacy_modes
            ; arg = Stdlib.fst arg
            ; typ_constraint = Stdlib.fst typ_constraint
            ; modes = Stdlib.fst modes
            }
        , self
          #
          constr
            ctx
            "Parg_unlabelled"
            [ self
              #
              record
                ctx
                [ "legacy_modes", Stdlib.snd legacy_modes
                ; "arg", Stdlib.snd arg
                ; "typ_constraint", Stdlib.snd typ_constraint
                ; "modes", Stdlib.snd modes
                ]
            ] )
      | Parg_labelled
          { optional
          ; legacy_modes
          ; name
          ; maybe_punned
          ; typ_constraint
          ; modes
          ; default
          } ->
        let optional = self#bool ctx optional in
        let legacy_modes = self#modes ctx legacy_modes in
        let name = self#string ctx name in
        let maybe_punned = self#option _a ctx maybe_punned in
        let typ_constraint =
          self#option self#type_constraint ctx typ_constraint
        in
        let modes = self#modes ctx modes in
        let default = self#option self#expression ctx default in
        ( Parg_labelled
            { optional = Stdlib.fst optional
            ; legacy_modes = Stdlib.fst legacy_modes
            ; name = Stdlib.fst name
            ; maybe_punned = Stdlib.fst maybe_punned
            ; typ_constraint = Stdlib.fst typ_constraint
            ; modes = Stdlib.fst modes
            ; default = Stdlib.fst default
            }
        , self
          #
          constr
            ctx
            "Parg_labelled"
            [ self
              #
              record
                ctx
                [ "optional", Stdlib.snd optional
                ; "legacy_modes", Stdlib.snd legacy_modes
                ; "name", Stdlib.snd name
                ; "maybe_punned", Stdlib.snd maybe_punned
                ; "typ_constraint", Stdlib.snd typ_constraint
                ; "modes", Stdlib.snd modes
                ; "default", Stdlib.snd default
                ]
            ] )

    method argument
      :
        'a
        .  ('ctx -> 'a -> 'a * 'res)
        -> 'ctx
        -> 'a argument
        -> 'a argument * 'res
      =
      fun _a ctx { parg_desc; parg_tokens } ->
      let parg_desc = self#argument_desc _a ctx parg_desc in
      let parg_tokens = self#token_seq ctx parg_tokens in
      ( { parg_desc = Stdlib.fst parg_desc
        ; parg_tokens = Stdlib.fst parg_tokens
        }
      , self
        #
        record
          ctx
          [ "parg_desc", Stdlib.snd parg_desc
          ; "parg_tokens", Stdlib.snd parg_tokens
          ] )

    method function_param_desc
      : 'ctx -> function_param_desc -> function_param_desc * 'res
      =
      fun ctx x ->
      match x with
      | Pparam_val a ->
        let a = self#argument self#pattern ctx a in
        ( Pparam_val (Stdlib.fst a)
        , self#constr ctx "Pparam_val" [ Stdlib.snd a ] )
      | Pparam_newtype (a, b) ->
        let a = self#loc self#string ctx a in
        let b = self#option self#jkind_annotation ctx b in
        ( Pparam_newtype (Stdlib.fst a, Stdlib.fst b)
        , self#constr ctx "Pparam_newtype" [ Stdlib.snd a; Stdlib.snd b ] )
      | Pparam_newtypes a ->
        let a =
          self
          #
          list
            (fun ctx (a, b) ->
              let a = self#loc self#string ctx a in
              let b = self#option self#jkind_annotation ctx b in
              ( (Stdlib.fst a, Stdlib.fst b)
              , self#tuple ctx [ Stdlib.snd a; Stdlib.snd b ] ))
            ctx
            a
        in
        ( Pparam_newtypes (Stdlib.fst a)
        , self#constr ctx "Pparam_newtypes" [ Stdlib.snd a ] )

    method function_param : 'ctx -> function_param -> function_param * 'res =
      fun ctx { pparam_loc; pparam_desc } ->
      let pparam_loc = self#location ctx pparam_loc in
      let pparam_desc = self#function_param_desc ctx pparam_desc in
      ( { pparam_loc = Stdlib.fst pparam_loc
        ; pparam_desc = Stdlib.fst pparam_desc
        }
      , self
        #
        record
          ctx
          [ "pparam_loc", Stdlib.snd pparam_loc
          ; "pparam_desc", Stdlib.snd pparam_desc
          ] )

    method function_body : 'ctx -> function_body -> function_body * 'res =
      fun ctx { pfb_desc; pfb_loc; pfb_tokens } ->
      let pfb_desc = self#function_body_desc ctx pfb_desc in
      let pfb_loc = self#location ctx pfb_loc in
      let pfb_tokens = self#token_seq ctx pfb_tokens in
      ( { pfb_desc = Stdlib.fst pfb_desc
        ; pfb_loc = Stdlib.fst pfb_loc
        ; pfb_tokens = Stdlib.fst pfb_tokens
        }
      , self
        #
        record
          ctx
          [ "pfb_desc", Stdlib.snd pfb_desc
          ; "pfb_loc", Stdlib.snd pfb_loc
          ; "pfb_tokens", Stdlib.snd pfb_tokens
          ] )

    method function_body_desc
      : 'ctx -> function_body_desc -> function_body_desc * 'res
      =
      fun ctx x ->
      match x with
      | Pfunction_body a ->
        let a = self#expression ctx a in
        ( Pfunction_body (Stdlib.fst a)
        , self#constr ctx "Pfunction_body" [ Stdlib.snd a ] )
      | Pfunction_cases (a, b) ->
        let a = self#list self#case ctx a in
        let b = self#ext_attribute ctx b in
        ( Pfunction_cases (Stdlib.fst a, Stdlib.fst b)
        , self#constr ctx "Pfunction_cases" [ Stdlib.snd a; Stdlib.snd b ] )

    method type_constraint : 'ctx -> type_constraint -> type_constraint * 'res =
      fun ctx x ->
      match x with
      | Pconstraint a ->
        let a = self#core_type ctx a in
        ( Pconstraint (Stdlib.fst a)
        , self#constr ctx "Pconstraint" [ Stdlib.snd a ] )
      | Pcoerce (a, b) ->
        let a = self#option self#core_type ctx a in
        let b = self#core_type ctx b in
        ( Pcoerce (Stdlib.fst a, Stdlib.fst b)
        , self#constr ctx "Pcoerce" [ Stdlib.snd a; Stdlib.snd b ] )

    method function_constraint
      : 'ctx -> function_constraint -> function_constraint * 'res
      =
      fun ctx { ret_mode_annotations; ret_type_constraint } ->
      let ret_mode_annotations = self#modes ctx ret_mode_annotations in
      let ret_type_constraint =
        self#option self#type_constraint ctx ret_type_constraint
      in
      ( { ret_mode_annotations = Stdlib.fst ret_mode_annotations
        ; ret_type_constraint = Stdlib.fst ret_type_constraint
        }
      , self
        #
        record
          ctx
          [ "ret_mode_annotations", Stdlib.snd ret_mode_annotations
          ; "ret_type_constraint", Stdlib.snd ret_type_constraint
          ] )

    method block_access : 'ctx -> block_access -> block_access * 'res =
      fun ctx x ->
      match x with
      | Baccess_field a ->
        let a = self#loc self#longident ctx a in
        ( Baccess_field (Stdlib.fst a)
        , self#constr ctx "Baccess_field" [ Stdlib.snd a ] )
      | Baccess_array (a, b, c) ->
        let a = self#mutable_flag ctx a in
        let b = self#index_kind ctx b in
        let c = self#expression ctx c in
        ( Baccess_array (Stdlib.fst a, Stdlib.fst b, Stdlib.fst c)
        , self
          #
          constr
            ctx
            "Baccess_array"
            [ Stdlib.snd a; Stdlib.snd b; Stdlib.snd c ] )
      | Baccess_block (a, b) ->
        let a = self#mutable_flag ctx a in
        let b = self#expression ctx b in
        ( Baccess_block (Stdlib.fst a, Stdlib.fst b)
        , self#constr ctx "Baccess_block" [ Stdlib.snd a; Stdlib.snd b ] )

    method unboxed_access : 'ctx -> unboxed_access -> unboxed_access * 'res =
      fun ctx x ->
      match x with
      | Uaccess_unboxed_field a ->
        let a = self#loc self#longident ctx a in
        ( Uaccess_unboxed_field (Stdlib.fst a)
        , self#constr ctx "Uaccess_unboxed_field" [ Stdlib.snd a ] )

    method comprehension_iterator
      : 'ctx -> comprehension_iterator -> comprehension_iterator * 'res
      =
      fun ctx x ->
      match x with
      | Pcomp_range { start; stop; direction } ->
        let start = self#expression ctx start in
        let stop = self#expression ctx stop in
        let direction = self#direction_flag ctx direction in
        ( Pcomp_range
            { start = Stdlib.fst start
            ; stop = Stdlib.fst stop
            ; direction = Stdlib.fst direction
            }
        , self
          #
          constr
            ctx
            "Pcomp_range"
            [ self
              #
              record
                ctx
                [ "start", Stdlib.snd start
                ; "stop", Stdlib.snd stop
                ; "direction", Stdlib.snd direction
                ]
            ] )
      | Pcomp_in a ->
        let a = self#expression ctx a in
        Pcomp_in (Stdlib.fst a), self#constr ctx "Pcomp_in" [ Stdlib.snd a ]

    method comprehension_clause_binding
      :
        'ctx
        -> comprehension_clause_binding
        -> comprehension_clause_binding * 'res
      =
      fun
        ctx
        { pcomp_cb_mode
        ; pcomp_cb_pattern
        ; pcomp_cb_iterator
        ; pcomp_cb_attributes
        ; pcomp_cb_tokens
        }
        ->
      let pcomp_cb_mode = self#option (self#loc self#mode) ctx pcomp_cb_mode in
      let pcomp_cb_pattern = self#pattern ctx pcomp_cb_pattern in
      let pcomp_cb_iterator =
        self#comprehension_iterator ctx pcomp_cb_iterator
      in
      let pcomp_cb_attributes = self#attributes ctx pcomp_cb_attributes in
      let pcomp_cb_tokens = self#token_seq ctx pcomp_cb_tokens in
      ( { pcomp_cb_mode = Stdlib.fst pcomp_cb_mode
        ; pcomp_cb_pattern = Stdlib.fst pcomp_cb_pattern
        ; pcomp_cb_iterator = Stdlib.fst pcomp_cb_iterator
        ; pcomp_cb_attributes = Stdlib.fst pcomp_cb_attributes
        ; pcomp_cb_tokens = Stdlib.fst pcomp_cb_tokens
        }
      , self
        #
        record
          ctx
          [ "pcomp_cb_mode", Stdlib.snd pcomp_cb_mode
          ; "pcomp_cb_pattern", Stdlib.snd pcomp_cb_pattern
          ; "pcomp_cb_iterator", Stdlib.snd pcomp_cb_iterator
          ; "pcomp_cb_attributes", Stdlib.snd pcomp_cb_attributes
          ; "pcomp_cb_tokens", Stdlib.snd pcomp_cb_tokens
          ] )

    method comprehension_clause
      : 'ctx -> comprehension_clause -> comprehension_clause * 'res
      =
      fun ctx x ->
      match x with
      | Pcomp_for a ->
        let a = self#list self#comprehension_clause_binding ctx a in
        Pcomp_for (Stdlib.fst a), self#constr ctx "Pcomp_for" [ Stdlib.snd a ]
      | Pcomp_when a ->
        let a = self#expression ctx a in
        ( Pcomp_when (Stdlib.fst a)
        , self#constr ctx "Pcomp_when" [ Stdlib.snd a ] )

    method comprehension : 'ctx -> comprehension -> comprehension * 'res =
      fun ctx { pcomp_body; pcomp_clauses; pcomp_tokens } ->
      let pcomp_body = self#expression ctx pcomp_body in
      let pcomp_clauses =
        self#list self#comprehension_clause ctx pcomp_clauses
      in
      let pcomp_tokens = self#token_seq ctx pcomp_tokens in
      ( { pcomp_body = Stdlib.fst pcomp_body
        ; pcomp_clauses = Stdlib.fst pcomp_clauses
        ; pcomp_tokens = Stdlib.fst pcomp_tokens
        }
      , self
        #
        record
          ctx
          [ "pcomp_body", Stdlib.snd pcomp_body
          ; "pcomp_clauses", Stdlib.snd pcomp_clauses
          ; "pcomp_tokens", Stdlib.snd pcomp_tokens
          ] )

    method comprehension_expression
      : 'ctx -> comprehension_expression -> comprehension_expression * 'res
      =
      fun ctx x ->
      match x with
      | Pcomp_list_comprehension a ->
        let a = self#comprehension ctx a in
        ( Pcomp_list_comprehension (Stdlib.fst a)
        , self#constr ctx "Pcomp_list_comprehension" [ Stdlib.snd a ] )
      | Pcomp_array_comprehension (a, b) ->
        let a = self#mutable_flag ctx a in
        let b = self#comprehension ctx b in
        ( Pcomp_array_comprehension (Stdlib.fst a, Stdlib.fst b)
        , self
          #
          constr
            ctx
            "Pcomp_array_comprehension"
            [ Stdlib.snd a; Stdlib.snd b ] )

    method value_description
      : 'ctx -> value_description -> value_description * 'res
      =
      fun
        ctx
        { pval_pre_doc
        ; pval_ext_attrs
        ; pval_name
        ; pval_type
        ; pval_modalities
        ; pval_prim
        ; pval_attributes
        ; pval_post_doc
        ; pval_loc
        ; pval_tokens
        }
        ->
      let pval_pre_doc = self#option self#string ctx pval_pre_doc in
      let pval_ext_attrs = self#ext_attribute ctx pval_ext_attrs in
      let pval_name = self#loc self#longident_str_or_op ctx pval_name in
      let pval_type = self#core_type ctx pval_type in
      let pval_modalities = self#modalities ctx pval_modalities in
      let pval_prim = self#list self#string ctx pval_prim in
      let pval_attributes = self#attributes ctx pval_attributes in
      let pval_post_doc = self#option self#string ctx pval_post_doc in
      let pval_loc = self#location ctx pval_loc in
      let pval_tokens = self#token_seq ctx pval_tokens in
      ( { pval_pre_doc = Stdlib.fst pval_pre_doc
        ; pval_ext_attrs = Stdlib.fst pval_ext_attrs
        ; pval_name = Stdlib.fst pval_name
        ; pval_type = Stdlib.fst pval_type
        ; pval_modalities = Stdlib.fst pval_modalities
        ; pval_prim = Stdlib.fst pval_prim
        ; pval_attributes = Stdlib.fst pval_attributes
        ; pval_post_doc = Stdlib.fst pval_post_doc
        ; pval_loc = Stdlib.fst pval_loc
        ; pval_tokens = Stdlib.fst pval_tokens
        }
      , self
        #
        record
          ctx
          [ "pval_pre_doc", Stdlib.snd pval_pre_doc
          ; "pval_ext_attrs", Stdlib.snd pval_ext_attrs
          ; "pval_name", Stdlib.snd pval_name
          ; "pval_type", Stdlib.snd pval_type
          ; "pval_modalities", Stdlib.snd pval_modalities
          ; "pval_prim", Stdlib.snd pval_prim
          ; "pval_attributes", Stdlib.snd pval_attributes
          ; "pval_post_doc", Stdlib.snd pval_post_doc
          ; "pval_loc", Stdlib.snd pval_loc
          ; "pval_tokens", Stdlib.snd pval_tokens
          ] )

    method ptype_param : 'ctx -> ptype_param -> ptype_param * 'res =
      fun ctx { ptp_typ; ptp_infos; ptp_tokens } ->
      let ptp_typ = self#core_type ctx ptp_typ in
      let ptp_infos =
        (fun ctx (a, b) ->
          let a = self#variance ctx a in
          let b = self#injectivity ctx b in
          ( (Stdlib.fst a, Stdlib.fst b)
          , self#tuple ctx [ Stdlib.snd a; Stdlib.snd b ] ))
          ctx
          ptp_infos
      in
      let ptp_tokens = self#token_seq ctx ptp_tokens in
      ( { ptp_typ = Stdlib.fst ptp_typ
        ; ptp_infos = Stdlib.fst ptp_infos
        ; ptp_tokens = Stdlib.fst ptp_tokens
        }
      , self
        #
        record
          ctx
          [ "ptp_typ", Stdlib.snd ptp_typ
          ; "ptp_infos", Stdlib.snd ptp_infos
          ; "ptp_tokens", Stdlib.snd ptp_tokens
          ] )

    method ptype_params : 'ctx -> ptype_params -> ptype_params * 'res =
      self#list self#ptype_param

    method ptype_constraint
      : 'ctx -> ptype_constraint -> ptype_constraint * 'res
      =
      fun ctx (a, b, c) ->
      let a = self#core_type ctx a in
      let b = self#core_type ctx b in
      let c = self#location ctx c in
      ( (Stdlib.fst a, Stdlib.fst b, Stdlib.fst c)
      , self#tuple ctx [ Stdlib.snd a; Stdlib.snd b; Stdlib.snd c ] )

    method type_declaration
      : 'ctx -> type_declaration -> type_declaration * 'res
      =
      fun
        ctx
        { ptype_pre_text
        ; ptype_pre_doc
        ; ptype_ext_attrs
        ; ptype_name
        ; ptype_params
        ; ptype_jkind_annotation
        ; ptype_private
        ; ptype_manifest
        ; ptype_kind
        ; ptype_cstrs
        ; ptype_attributes
        ; ptype_post_doc
        ; ptype_loc
        ; ptype_tokens
        }
        ->
      let ptype_pre_text = self#list self#string ctx ptype_pre_text in
      let ptype_pre_doc = self#option self#string ctx ptype_pre_doc in
      let ptype_ext_attrs = self#ext_attribute ctx ptype_ext_attrs in
      let ptype_name = self#loc self#string ctx ptype_name in
      let ptype_params = self#ptype_params ctx ptype_params in
      let ptype_jkind_annotation =
        self#option self#jkind_annotation ctx ptype_jkind_annotation
      in
      let ptype_private = self#private_flag ctx ptype_private in
      let ptype_manifest = self#option self#core_type ctx ptype_manifest in
      let ptype_kind = self#type_kind ctx ptype_kind in
      let ptype_cstrs = self#list self#ptype_constraint ctx ptype_cstrs in
      let ptype_attributes = self#attributes ctx ptype_attributes in
      let ptype_post_doc = self#option self#string ctx ptype_post_doc in
      let ptype_loc = self#location ctx ptype_loc in
      let ptype_tokens = self#token_seq ctx ptype_tokens in
      ( { ptype_pre_text = Stdlib.fst ptype_pre_text
        ; ptype_pre_doc = Stdlib.fst ptype_pre_doc
        ; ptype_ext_attrs = Stdlib.fst ptype_ext_attrs
        ; ptype_name = Stdlib.fst ptype_name
        ; ptype_params = Stdlib.fst ptype_params
        ; ptype_jkind_annotation = Stdlib.fst ptype_jkind_annotation
        ; ptype_private = Stdlib.fst ptype_private
        ; ptype_manifest = Stdlib.fst ptype_manifest
        ; ptype_kind = Stdlib.fst ptype_kind
        ; ptype_cstrs = Stdlib.fst ptype_cstrs
        ; ptype_attributes = Stdlib.fst ptype_attributes
        ; ptype_post_doc = Stdlib.fst ptype_post_doc
        ; ptype_loc = Stdlib.fst ptype_loc
        ; ptype_tokens = Stdlib.fst ptype_tokens
        }
      , self
        #
        record
          ctx
          [ "ptype_pre_text", Stdlib.snd ptype_pre_text
          ; "ptype_pre_doc", Stdlib.snd ptype_pre_doc
          ; "ptype_ext_attrs", Stdlib.snd ptype_ext_attrs
          ; "ptype_name", Stdlib.snd ptype_name
          ; "ptype_params", Stdlib.snd ptype_params
          ; "ptype_jkind_annotation", Stdlib.snd ptype_jkind_annotation
          ; "ptype_private", Stdlib.snd ptype_private
          ; "ptype_manifest", Stdlib.snd ptype_manifest
          ; "ptype_kind", Stdlib.snd ptype_kind
          ; "ptype_cstrs", Stdlib.snd ptype_cstrs
          ; "ptype_attributes", Stdlib.snd ptype_attributes
          ; "ptype_post_doc", Stdlib.snd ptype_post_doc
          ; "ptype_loc", Stdlib.snd ptype_loc
          ; "ptype_tokens", Stdlib.snd ptype_tokens
          ] )

    method type_kind : 'ctx -> type_kind -> type_kind * 'res =
      fun ctx x ->
      match x with
      | Ptype_abstract -> Ptype_abstract, self#constr ctx "Ptype_abstract" []
      | Ptype_variant a ->
        let a = self#list self#constructor_declaration ctx a in
        ( Ptype_variant (Stdlib.fst a)
        , self#constr ctx "Ptype_variant" [ Stdlib.snd a ] )
      | Ptype_record a ->
        let a = self#list self#label_declaration ctx a in
        ( Ptype_record (Stdlib.fst a)
        , self#constr ctx "Ptype_record" [ Stdlib.snd a ] )
      | Ptype_record_unboxed_product a ->
        let a = self#list self#label_declaration ctx a in
        ( Ptype_record_unboxed_product (Stdlib.fst a)
        , self#constr ctx "Ptype_record_unboxed_product" [ Stdlib.snd a ] )
      | Ptype_open -> Ptype_open, self#constr ctx "Ptype_open" []

    method label_declaration
      : 'ctx -> label_declaration -> label_declaration * 'res
      =
      fun
        ctx
        { pld_name
        ; pld_mutable
        ; pld_global
        ; pld_modalities
        ; pld_type
        ; pld_loc
        ; pld_attributes
        ; pld_doc
        ; pld_tokens
        }
        ->
      let pld_name = self#loc self#string ctx pld_name in
      let pld_mutable = self#mutable_flag ctx pld_mutable in
      let pld_global = self#bool ctx pld_global in
      let pld_modalities = self#modalities ctx pld_modalities in
      let pld_type = self#core_type ctx pld_type in
      let pld_loc = self#location ctx pld_loc in
      let pld_attributes = self#attributes ctx pld_attributes in
      let pld_doc = self#option self#string ctx pld_doc in
      let pld_tokens = self#token_seq ctx pld_tokens in
      ( { pld_name = Stdlib.fst pld_name
        ; pld_mutable = Stdlib.fst pld_mutable
        ; pld_global = Stdlib.fst pld_global
        ; pld_modalities = Stdlib.fst pld_modalities
        ; pld_type = Stdlib.fst pld_type
        ; pld_loc = Stdlib.fst pld_loc
        ; pld_attributes = Stdlib.fst pld_attributes
        ; pld_doc = Stdlib.fst pld_doc
        ; pld_tokens = Stdlib.fst pld_tokens
        }
      , self
        #
        record
          ctx
          [ "pld_name", Stdlib.snd pld_name
          ; "pld_mutable", Stdlib.snd pld_mutable
          ; "pld_global", Stdlib.snd pld_global
          ; "pld_modalities", Stdlib.snd pld_modalities
          ; "pld_type", Stdlib.snd pld_type
          ; "pld_loc", Stdlib.snd pld_loc
          ; "pld_attributes", Stdlib.snd pld_attributes
          ; "pld_doc", Stdlib.snd pld_doc
          ; "pld_tokens", Stdlib.snd pld_tokens
          ] )

    method constructor_declaration
      : 'ctx -> constructor_declaration -> constructor_declaration * 'res
      =
      fun
        ctx
        { pcd_name
        ; pcd_vars
        ; pcd_args
        ; pcd_res
        ; pcd_loc
        ; pcd_attributes
        ; pcd_doc
        ; pcd_tokens
        }
        ->
      let pcd_name = self#loc self#longident_str_or_op ctx pcd_name in
      let pcd_vars =
        self
        #
        list
          (fun ctx (a, b) ->
            let a = self#loc self#string ctx a in
            let b = self#option self#jkind_annotation ctx b in
            ( (Stdlib.fst a, Stdlib.fst b)
            , self#tuple ctx [ Stdlib.snd a; Stdlib.snd b ] ))
          ctx
          pcd_vars
      in
      let pcd_args = self#constructor_arguments ctx pcd_args in
      let pcd_res = self#option self#core_type ctx pcd_res in
      let pcd_loc = self#location ctx pcd_loc in
      let pcd_attributes = self#attributes ctx pcd_attributes in
      let pcd_doc = self#option self#string ctx pcd_doc in
      let pcd_tokens = self#token_seq ctx pcd_tokens in
      ( { pcd_name = Stdlib.fst pcd_name
        ; pcd_vars = Stdlib.fst pcd_vars
        ; pcd_args = Stdlib.fst pcd_args
        ; pcd_res = Stdlib.fst pcd_res
        ; pcd_loc = Stdlib.fst pcd_loc
        ; pcd_attributes = Stdlib.fst pcd_attributes
        ; pcd_doc = Stdlib.fst pcd_doc
        ; pcd_tokens = Stdlib.fst pcd_tokens
        }
      , self
        #
        record
          ctx
          [ "pcd_name", Stdlib.snd pcd_name
          ; "pcd_vars", Stdlib.snd pcd_vars
          ; "pcd_args", Stdlib.snd pcd_args
          ; "pcd_res", Stdlib.snd pcd_res
          ; "pcd_loc", Stdlib.snd pcd_loc
          ; "pcd_attributes", Stdlib.snd pcd_attributes
          ; "pcd_doc", Stdlib.snd pcd_doc
          ; "pcd_tokens", Stdlib.snd pcd_tokens
          ] )

    method constructor_argument
      : 'ctx -> constructor_argument -> constructor_argument * 'res
      =
      fun ctx { pca_global; pca_type; pca_modalities; pca_loc } ->
      let pca_global = self#bool ctx pca_global in
      let pca_type = self#core_type ctx pca_type in
      let pca_modalities = self#modalities ctx pca_modalities in
      let pca_loc = self#location ctx pca_loc in
      ( { pca_global = Stdlib.fst pca_global
        ; pca_type = Stdlib.fst pca_type
        ; pca_modalities = Stdlib.fst pca_modalities
        ; pca_loc = Stdlib.fst pca_loc
        }
      , self
        #
        record
          ctx
          [ "pca_global", Stdlib.snd pca_global
          ; "pca_type", Stdlib.snd pca_type
          ; "pca_modalities", Stdlib.snd pca_modalities
          ; "pca_loc", Stdlib.snd pca_loc
          ] )

    method constructor_arguments
      : 'ctx -> constructor_arguments -> constructor_arguments * 'res
      =
      fun ctx x ->
      match x with
      | Pcstr_tuple a ->
        let a = self#list self#constructor_argument ctx a in
        ( Pcstr_tuple (Stdlib.fst a)
        , self#constr ctx "Pcstr_tuple" [ Stdlib.snd a ] )
      | Pcstr_record a ->
        let a = self#list self#label_declaration ctx a in
        ( Pcstr_record (Stdlib.fst a)
        , self#constr ctx "Pcstr_record" [ Stdlib.snd a ] )

    method type_extension : 'ctx -> type_extension -> type_extension * 'res =
      fun
        ctx
        { ptyext_pre_doc
        ; ptyext_ext_attrs
        ; ptyext_path
        ; ptyext_params
        ; ptyext_constructors
        ; ptyext_private
        ; ptyext_loc
        ; ptyext_attributes
        ; ptyext_post_doc
        ; ptyext_tokens
        }
        ->
      let ptyext_pre_doc = self#option self#string ctx ptyext_pre_doc in
      let ptyext_ext_attrs = self#ext_attribute ctx ptyext_ext_attrs in
      let ptyext_path = self#loc self#longident ctx ptyext_path in
      let ptyext_params = self#list self#ptype_param ctx ptyext_params in
      let ptyext_constructors =
        self#list self#extension_constructor ctx ptyext_constructors
      in
      let ptyext_private = self#private_flag ctx ptyext_private in
      let ptyext_loc = self#location ctx ptyext_loc in
      let ptyext_attributes = self#attributes ctx ptyext_attributes in
      let ptyext_post_doc = self#option self#string ctx ptyext_post_doc in
      let ptyext_tokens = self#token_seq ctx ptyext_tokens in
      ( { ptyext_pre_doc = Stdlib.fst ptyext_pre_doc
        ; ptyext_ext_attrs = Stdlib.fst ptyext_ext_attrs
        ; ptyext_path = Stdlib.fst ptyext_path
        ; ptyext_params = Stdlib.fst ptyext_params
        ; ptyext_constructors = Stdlib.fst ptyext_constructors
        ; ptyext_private = Stdlib.fst ptyext_private
        ; ptyext_loc = Stdlib.fst ptyext_loc
        ; ptyext_attributes = Stdlib.fst ptyext_attributes
        ; ptyext_post_doc = Stdlib.fst ptyext_post_doc
        ; ptyext_tokens = Stdlib.fst ptyext_tokens
        }
      , self
        #
        record
          ctx
          [ "ptyext_pre_doc", Stdlib.snd ptyext_pre_doc
          ; "ptyext_ext_attrs", Stdlib.snd ptyext_ext_attrs
          ; "ptyext_path", Stdlib.snd ptyext_path
          ; "ptyext_params", Stdlib.snd ptyext_params
          ; "ptyext_constructors", Stdlib.snd ptyext_constructors
          ; "ptyext_private", Stdlib.snd ptyext_private
          ; "ptyext_loc", Stdlib.snd ptyext_loc
          ; "ptyext_attributes", Stdlib.snd ptyext_attributes
          ; "ptyext_post_doc", Stdlib.snd ptyext_post_doc
          ; "ptyext_tokens", Stdlib.snd ptyext_tokens
          ] )

    method extension_constructor
      : 'ctx -> extension_constructor -> extension_constructor * 'res
      =
      fun
        ctx
        { pext_name
        ; pext_kind
        ; pext_loc
        ; pext_attributes
        ; pext_doc
        ; pext_tokens
        }
        ->
      let pext_name = self#loc self#longident_str_or_op ctx pext_name in
      let pext_kind = self#extension_constructor_kind ctx pext_kind in
      let pext_loc = self#location ctx pext_loc in
      let pext_attributes = self#attributes ctx pext_attributes in
      let pext_doc = self#option self#string ctx pext_doc in
      let pext_tokens = self#token_seq ctx pext_tokens in
      ( { pext_name = Stdlib.fst pext_name
        ; pext_kind = Stdlib.fst pext_kind
        ; pext_loc = Stdlib.fst pext_loc
        ; pext_attributes = Stdlib.fst pext_attributes
        ; pext_doc = Stdlib.fst pext_doc
        ; pext_tokens = Stdlib.fst pext_tokens
        }
      , self
        #
        record
          ctx
          [ "pext_name", Stdlib.snd pext_name
          ; "pext_kind", Stdlib.snd pext_kind
          ; "pext_loc", Stdlib.snd pext_loc
          ; "pext_attributes", Stdlib.snd pext_attributes
          ; "pext_doc", Stdlib.snd pext_doc
          ; "pext_tokens", Stdlib.snd pext_tokens
          ] )

    method type_exception : 'ctx -> type_exception -> type_exception * 'res =
      fun
        ctx
        { ptyexn_pre_doc
        ; ptyexn_ext_attrs
        ; ptyexn_constructor
        ; ptyexn_loc
        ; ptyexn_attributes
        ; ptyexn_post_doc
        ; ptyexn_tokens
        }
        ->
      let ptyexn_pre_doc = self#option self#string ctx ptyexn_pre_doc in
      let ptyexn_ext_attrs = self#ext_attribute ctx ptyexn_ext_attrs in
      let ptyexn_constructor =
        self#extension_constructor ctx ptyexn_constructor
      in
      let ptyexn_loc = self#location ctx ptyexn_loc in
      let ptyexn_attributes = self#attributes ctx ptyexn_attributes in
      let ptyexn_post_doc = self#option self#string ctx ptyexn_post_doc in
      let ptyexn_tokens = self#token_seq ctx ptyexn_tokens in
      ( { ptyexn_pre_doc = Stdlib.fst ptyexn_pre_doc
        ; ptyexn_ext_attrs = Stdlib.fst ptyexn_ext_attrs
        ; ptyexn_constructor = Stdlib.fst ptyexn_constructor
        ; ptyexn_loc = Stdlib.fst ptyexn_loc
        ; ptyexn_attributes = Stdlib.fst ptyexn_attributes
        ; ptyexn_post_doc = Stdlib.fst ptyexn_post_doc
        ; ptyexn_tokens = Stdlib.fst ptyexn_tokens
        }
      , self
        #
        record
          ctx
          [ "ptyexn_pre_doc", Stdlib.snd ptyexn_pre_doc
          ; "ptyexn_ext_attrs", Stdlib.snd ptyexn_ext_attrs
          ; "ptyexn_constructor", Stdlib.snd ptyexn_constructor
          ; "ptyexn_loc", Stdlib.snd ptyexn_loc
          ; "ptyexn_attributes", Stdlib.snd ptyexn_attributes
          ; "ptyexn_post_doc", Stdlib.snd ptyexn_post_doc
          ; "ptyexn_tokens", Stdlib.snd ptyexn_tokens
          ] )

    method extension_constructor_kind
      : 'ctx -> extension_constructor_kind -> extension_constructor_kind * 'res
      =
      fun ctx x ->
      match x with
      | Pext_decl (a, b, c) ->
        let a =
          self
          #
          list
            (fun ctx (a, b) ->
              let a = self#loc self#string ctx a in
              let b = self#option self#jkind_annotation ctx b in
              ( (Stdlib.fst a, Stdlib.fst b)
              , self#tuple ctx [ Stdlib.snd a; Stdlib.snd b ] ))
            ctx
            a
        in
        let b = self#constructor_arguments ctx b in
        let c = self#option self#core_type ctx c in
        ( Pext_decl (Stdlib.fst a, Stdlib.fst b, Stdlib.fst c)
        , self
          #
          constr
            ctx
            "Pext_decl"
            [ Stdlib.snd a; Stdlib.snd b; Stdlib.snd c ] )
      | Pext_rebind a ->
        let a = self#loc self#longident ctx a in
        ( Pext_rebind (Stdlib.fst a)
        , self#constr ctx "Pext_rebind" [ Stdlib.snd a ] )

    method class_type : 'ctx -> class_type -> class_type * 'res =
      fun ctx { pcty_desc; pcty_loc; pcty_attributes; pcty_tokens } ->
      let pcty_desc = self#class_type_desc ctx pcty_desc in
      let pcty_loc = self#location ctx pcty_loc in
      let pcty_attributes = self#attributes ctx pcty_attributes in
      let pcty_tokens = self#token_seq ctx pcty_tokens in
      ( { pcty_desc = Stdlib.fst pcty_desc
        ; pcty_loc = Stdlib.fst pcty_loc
        ; pcty_attributes = Stdlib.fst pcty_attributes
        ; pcty_tokens = Stdlib.fst pcty_tokens
        }
      , self
        #
        record
          ctx
          [ "pcty_desc", Stdlib.snd pcty_desc
          ; "pcty_loc", Stdlib.snd pcty_loc
          ; "pcty_attributes", Stdlib.snd pcty_attributes
          ; "pcty_tokens", Stdlib.snd pcty_tokens
          ] )

    method class_type_desc : 'ctx -> class_type_desc -> class_type_desc * 'res =
      fun ctx x ->
      match x with
      | Pcty_constr (a, b) ->
        let a = self#loc self#longident ctx a in
        let b = self#list self#core_type ctx b in
        ( Pcty_constr (Stdlib.fst a, Stdlib.fst b)
        , self#constr ctx "Pcty_constr" [ Stdlib.snd a; Stdlib.snd b ] )
      | Pcty_signature a ->
        let a = self#class_signature ctx a in
        ( Pcty_signature (Stdlib.fst a)
        , self#constr ctx "Pcty_signature" [ Stdlib.snd a ] )
      | Pcty_arrow (a, b) ->
        let a = self#arrow_arg ctx a in
        let b = self#class_type ctx b in
        ( Pcty_arrow (Stdlib.fst a, Stdlib.fst b)
        , self#constr ctx "Pcty_arrow" [ Stdlib.snd a; Stdlib.snd b ] )
      | Pcty_extension a ->
        let a = self#extension ctx a in
        ( Pcty_extension (Stdlib.fst a)
        , self#constr ctx "Pcty_extension" [ Stdlib.snd a ] )
      | Pcty_open (a, b) ->
        let a = self#open_description ctx a in
        let b = self#class_type ctx b in
        ( Pcty_open (Stdlib.fst a, Stdlib.fst b)
        , self#constr ctx "Pcty_open" [ Stdlib.snd a; Stdlib.snd b ] )

    method class_signature : 'ctx -> class_signature -> class_signature * 'res =
      fun ctx { pcsig_self; pcsig_fields } ->
      let pcsig_self = self#option self#core_type ctx pcsig_self in
      let pcsig_fields = self#list self#class_type_field ctx pcsig_fields in
      ( { pcsig_self = Stdlib.fst pcsig_self
        ; pcsig_fields = Stdlib.fst pcsig_fields
        }
      , self
        #
        record
          ctx
          [ "pcsig_self", Stdlib.snd pcsig_self
          ; "pcsig_fields", Stdlib.snd pcsig_fields
          ] )

    method class_type_field
      : 'ctx -> class_type_field -> class_type_field * 'res
      =
      fun
        ctx
        { pctf_pre_doc
        ; pctf_desc
        ; pctf_loc
        ; pctf_attributes
        ; pctf_post_doc
        ; pctf_tokens
        }
        ->
      let pctf_pre_doc = self#option self#string ctx pctf_pre_doc in
      let pctf_desc = self#class_type_field_desc ctx pctf_desc in
      let pctf_loc = self#location ctx pctf_loc in
      let pctf_attributes = self#attributes ctx pctf_attributes in
      let pctf_post_doc = self#option self#string ctx pctf_post_doc in
      let pctf_tokens = self#token_seq ctx pctf_tokens in
      ( { pctf_pre_doc = Stdlib.fst pctf_pre_doc
        ; pctf_desc = Stdlib.fst pctf_desc
        ; pctf_loc = Stdlib.fst pctf_loc
        ; pctf_attributes = Stdlib.fst pctf_attributes
        ; pctf_post_doc = Stdlib.fst pctf_post_doc
        ; pctf_tokens = Stdlib.fst pctf_tokens
        }
      , self
        #
        record
          ctx
          [ "pctf_pre_doc", Stdlib.snd pctf_pre_doc
          ; "pctf_desc", Stdlib.snd pctf_desc
          ; "pctf_loc", Stdlib.snd pctf_loc
          ; "pctf_attributes", Stdlib.snd pctf_attributes
          ; "pctf_post_doc", Stdlib.snd pctf_post_doc
          ; "pctf_tokens", Stdlib.snd pctf_tokens
          ] )

    method class_type_field_desc
      : 'ctx -> class_type_field_desc -> class_type_field_desc * 'res
      =
      fun ctx x ->
      match x with
      | Pctf_inherit a ->
        let a = self#class_type ctx a in
        ( Pctf_inherit (Stdlib.fst a)
        , self#constr ctx "Pctf_inherit" [ Stdlib.snd a ] )
      | Pctf_val a ->
        let a =
          (fun ctx (a, b, c, d) ->
            let a = self#loc self#string ctx a in
            let b = self#mutable_flag ctx b in
            let c = self#virtual_flag ctx c in
            let d = self#core_type ctx d in
            ( (Stdlib.fst a, Stdlib.fst b, Stdlib.fst c, Stdlib.fst d)
            , self
              #
              tuple
                ctx
                [ Stdlib.snd a; Stdlib.snd b; Stdlib.snd c; Stdlib.snd d ] ))
            ctx
            a
        in
        Pctf_val (Stdlib.fst a), self#constr ctx "Pctf_val" [ Stdlib.snd a ]
      | Pctf_method a ->
        let a =
          (fun ctx (a, b, c, d) ->
            let a = self#loc self#string ctx a in
            let b = self#private_flag ctx b in
            let c = self#virtual_flag ctx c in
            let d = self#core_type ctx d in
            ( (Stdlib.fst a, Stdlib.fst b, Stdlib.fst c, Stdlib.fst d)
            , self
              #
              tuple
                ctx
                [ Stdlib.snd a; Stdlib.snd b; Stdlib.snd c; Stdlib.snd d ] ))
            ctx
            a
        in
        ( Pctf_method (Stdlib.fst a)
        , self#constr ctx "Pctf_method" [ Stdlib.snd a ] )
      | Pctf_constraint a ->
        let a =
          (fun ctx (a, b) ->
            let a = self#core_type ctx a in
            let b = self#core_type ctx b in
            ( (Stdlib.fst a, Stdlib.fst b)
            , self#tuple ctx [ Stdlib.snd a; Stdlib.snd b ] ))
            ctx
            a
        in
        ( Pctf_constraint (Stdlib.fst a)
        , self#constr ctx "Pctf_constraint" [ Stdlib.snd a ] )
      | Pctf_attribute a ->
        let a = self#attribute ctx a in
        ( Pctf_attribute (Stdlib.fst a)
        , self#constr ctx "Pctf_attribute" [ Stdlib.snd a ] )
      | Pctf_extension a ->
        let a = self#extension ctx a in
        ( Pctf_extension (Stdlib.fst a)
        , self#constr ctx "Pctf_extension" [ Stdlib.snd a ] )
      | Pctf_docstring a ->
        let a = self#string ctx a in
        ( Pctf_docstring (Stdlib.fst a)
        , self#constr ctx "Pctf_docstring" [ Stdlib.snd a ] )

    method class_infos
      :
        'a
        .  ('ctx -> 'a -> 'a * 'res)
        -> 'ctx
        -> 'a class_infos
        -> 'a class_infos * 'res
      =
      fun
        _a ctx
        { pci_pre_text
        ; pci_pre_doc
        ; pci_virt
        ; pci_ext_attrs
        ; pci_params
        ; pci_name
        ; pci_value_params
        ; pci_constraint
        ; pci_expr
        ; pci_loc
        ; pci_attributes
        ; pci_post_doc
        ; pci_tokens
        }
        ->
      let pci_pre_text = self#list self#string ctx pci_pre_text in
      let pci_pre_doc = self#option self#string ctx pci_pre_doc in
      let pci_virt = self#virtual_flag ctx pci_virt in
      let pci_ext_attrs = self#ext_attribute ctx pci_ext_attrs in
      let pci_params = self#list self#ptype_param ctx pci_params in
      let pci_name = self#loc self#string ctx pci_name in
      let pci_value_params =
        self#list (self#argument self#pattern) ctx pci_value_params
      in
      let pci_constraint = self#option self#class_type ctx pci_constraint in
      let pci_expr = _a ctx pci_expr in
      let pci_loc = self#location ctx pci_loc in
      let pci_attributes = self#attributes ctx pci_attributes in
      let pci_post_doc = self#option self#string ctx pci_post_doc in
      let pci_tokens = self#token_seq ctx pci_tokens in
      ( { pci_pre_text = Stdlib.fst pci_pre_text
        ; pci_pre_doc = Stdlib.fst pci_pre_doc
        ; pci_virt = Stdlib.fst pci_virt
        ; pci_ext_attrs = Stdlib.fst pci_ext_attrs
        ; pci_params = Stdlib.fst pci_params
        ; pci_name = Stdlib.fst pci_name
        ; pci_value_params = Stdlib.fst pci_value_params
        ; pci_constraint = Stdlib.fst pci_constraint
        ; pci_expr = Stdlib.fst pci_expr
        ; pci_loc = Stdlib.fst pci_loc
        ; pci_attributes = Stdlib.fst pci_attributes
        ; pci_post_doc = Stdlib.fst pci_post_doc
        ; pci_tokens = Stdlib.fst pci_tokens
        }
      , self
        #
        record
          ctx
          [ "pci_pre_text", Stdlib.snd pci_pre_text
          ; "pci_pre_doc", Stdlib.snd pci_pre_doc
          ; "pci_virt", Stdlib.snd pci_virt
          ; "pci_ext_attrs", Stdlib.snd pci_ext_attrs
          ; "pci_params", Stdlib.snd pci_params
          ; "pci_name", Stdlib.snd pci_name
          ; "pci_value_params", Stdlib.snd pci_value_params
          ; "pci_constraint", Stdlib.snd pci_constraint
          ; "pci_expr", Stdlib.snd pci_expr
          ; "pci_loc", Stdlib.snd pci_loc
          ; "pci_attributes", Stdlib.snd pci_attributes
          ; "pci_post_doc", Stdlib.snd pci_post_doc
          ; "pci_tokens", Stdlib.snd pci_tokens
          ] )

    method class_description
      : 'ctx -> class_description -> class_description * 'res
      =
      self#class_infos self#class_type

    method class_type_declaration
      : 'ctx -> class_type_declaration -> class_type_declaration * 'res
      =
      self#class_infos self#class_type

    method class_expr : 'ctx -> class_expr -> class_expr * 'res =
      fun ctx { pcl_ext_attrs; pcl_desc; pcl_loc; pcl_attributes } ->
      let pcl_ext_attrs = self#ext_attribute ctx pcl_ext_attrs in
      let pcl_desc = self#class_expr_desc ctx pcl_desc in
      let pcl_loc = self#location ctx pcl_loc in
      let pcl_attributes = self#attributes ctx pcl_attributes in
      ( { pcl_ext_attrs = Stdlib.fst pcl_ext_attrs
        ; pcl_desc = Stdlib.fst pcl_desc
        ; pcl_loc = Stdlib.fst pcl_loc
        ; pcl_attributes = Stdlib.fst pcl_attributes
        }
      , self
        #
        record
          ctx
          [ "pcl_ext_attrs", Stdlib.snd pcl_ext_attrs
          ; "pcl_desc", Stdlib.snd pcl_desc
          ; "pcl_loc", Stdlib.snd pcl_loc
          ; "pcl_attributes", Stdlib.snd pcl_attributes
          ] )

    method class_expr_desc : 'ctx -> class_expr_desc -> class_expr_desc * 'res =
      fun ctx x ->
      match x with
      | Pcl_constr (a, b) ->
        let a = self#loc self#longident ctx a in
        let b = self#list self#core_type ctx b in
        ( Pcl_constr (Stdlib.fst a, Stdlib.fst b)
        , self#constr ctx "Pcl_constr" [ Stdlib.snd a; Stdlib.snd b ] )
      | Pcl_structure a ->
        let a = self#class_structure ctx a in
        ( Pcl_structure (Stdlib.fst a)
        , self#constr ctx "Pcl_structure" [ Stdlib.snd a ] )
      | Pcl_fun (a, b) ->
        let a = self#list (self#argument self#pattern) ctx a in
        let b = self#class_expr ctx b in
        ( Pcl_fun (Stdlib.fst a, Stdlib.fst b)
        , self#constr ctx "Pcl_fun" [ Stdlib.snd a; Stdlib.snd b ] )
      | Pcl_apply (a, b) ->
        let a = self#class_expr ctx a in
        let b = self#list (self#argument self#expression) ctx b in
        ( Pcl_apply (Stdlib.fst a, Stdlib.fst b)
        , self#constr ctx "Pcl_apply" [ Stdlib.snd a; Stdlib.snd b ] )
      | Pcl_let (a, b, c) ->
        let a = self#rec_flag ctx a in
        let b = self#list self#value_binding ctx b in
        let c = self#class_expr ctx c in
        ( Pcl_let (Stdlib.fst a, Stdlib.fst b, Stdlib.fst c)
        , self#constr ctx "Pcl_let" [ Stdlib.snd a; Stdlib.snd b; Stdlib.snd c ]
         )
      | Pcl_constraint (a, b) ->
        let a = self#class_expr ctx a in
        let b = self#class_type ctx b in
        ( Pcl_constraint (Stdlib.fst a, Stdlib.fst b)
        , self#constr ctx "Pcl_constraint" [ Stdlib.snd a; Stdlib.snd b ] )
      | Pcl_extension a ->
        let a = self#extension ctx a in
        ( Pcl_extension (Stdlib.fst a)
        , self#constr ctx "Pcl_extension" [ Stdlib.snd a ] )
      | Pcl_open (a, b) ->
        let a = self#open_description ctx a in
        let b = self#class_expr ctx b in
        ( Pcl_open (Stdlib.fst a, Stdlib.fst b)
        , self#constr ctx "Pcl_open" [ Stdlib.snd a; Stdlib.snd b ] )
      | Pcl_parens a ->
        let a = self#class_expr ctx a in
        ( Pcl_parens (Stdlib.fst a)
        , self#constr ctx "Pcl_parens" [ Stdlib.snd a ] )

    method class_structure : 'ctx -> class_structure -> class_structure * 'res =
      fun ctx { pcstr_self; pcstr_fields } ->
      let pcstr_self = self#pattern ctx pcstr_self in
      let pcstr_fields = self#list self#class_field ctx pcstr_fields in
      ( { pcstr_self = Stdlib.fst pcstr_self
        ; pcstr_fields = Stdlib.fst pcstr_fields
        }
      , self
        #
        record
          ctx
          [ "pcstr_self", Stdlib.snd pcstr_self
          ; "pcstr_fields", Stdlib.snd pcstr_fields
          ] )

    method class_field : 'ctx -> class_field -> class_field * 'res =
      fun
        ctx
        { pcf_pre_doc
        ; pcf_desc
        ; pcf_loc
        ; pcf_attributes
        ; pcf_post_doc
        ; pcf_tokens
        }
        ->
      let pcf_pre_doc = self#option self#string ctx pcf_pre_doc in
      let pcf_desc = self#class_field_desc ctx pcf_desc in
      let pcf_loc = self#location ctx pcf_loc in
      let pcf_attributes = self#attributes ctx pcf_attributes in
      let pcf_post_doc = self#option self#string ctx pcf_post_doc in
      let pcf_tokens = self#token_seq ctx pcf_tokens in
      ( { pcf_pre_doc = Stdlib.fst pcf_pre_doc
        ; pcf_desc = Stdlib.fst pcf_desc
        ; pcf_loc = Stdlib.fst pcf_loc
        ; pcf_attributes = Stdlib.fst pcf_attributes
        ; pcf_post_doc = Stdlib.fst pcf_post_doc
        ; pcf_tokens = Stdlib.fst pcf_tokens
        }
      , self
        #
        record
          ctx
          [ "pcf_pre_doc", Stdlib.snd pcf_pre_doc
          ; "pcf_desc", Stdlib.snd pcf_desc
          ; "pcf_loc", Stdlib.snd pcf_loc
          ; "pcf_attributes", Stdlib.snd pcf_attributes
          ; "pcf_post_doc", Stdlib.snd pcf_post_doc
          ; "pcf_tokens", Stdlib.snd pcf_tokens
          ] )

    method class_field_desc
      : 'ctx -> class_field_desc -> class_field_desc * 'res
      =
      fun ctx x ->
      match x with
      | Pcf_inherit (a, b, c) ->
        let a = self#override_flag ctx a in
        let b = self#class_expr ctx b in
        let c = self#option (self#loc self#string) ctx c in
        ( Pcf_inherit (Stdlib.fst a, Stdlib.fst b, Stdlib.fst c)
        , self
          #
          constr
            ctx
            "Pcf_inherit"
            [ Stdlib.snd a; Stdlib.snd b; Stdlib.snd c ] )
      | Pcf_val a ->
        let a =
          (fun ctx (a, b, c) ->
            let a = self#loc self#string ctx a in
            let b = self#mutable_flag ctx b in
            let c = self#class_field_kind ctx c in
            ( (Stdlib.fst a, Stdlib.fst b, Stdlib.fst c)
            , self#tuple ctx [ Stdlib.snd a; Stdlib.snd b; Stdlib.snd c ] ))
            ctx
            a
        in
        Pcf_val (Stdlib.fst a), self#constr ctx "Pcf_val" [ Stdlib.snd a ]
      | Pcf_method a ->
        let a =
          (fun ctx (a, b, c) ->
            let a = self#loc self#string ctx a in
            let b = self#private_flag ctx b in
            let c = self#class_field_kind ctx c in
            ( (Stdlib.fst a, Stdlib.fst b, Stdlib.fst c)
            , self#tuple ctx [ Stdlib.snd a; Stdlib.snd b; Stdlib.snd c ] ))
            ctx
            a
        in
        ( Pcf_method (Stdlib.fst a)
        , self#constr ctx "Pcf_method" [ Stdlib.snd a ] )
      | Pcf_constraint a ->
        let a =
          (fun ctx (a, b) ->
            let a = self#core_type ctx a in
            let b = self#core_type ctx b in
            ( (Stdlib.fst a, Stdlib.fst b)
            , self#tuple ctx [ Stdlib.snd a; Stdlib.snd b ] ))
            ctx
            a
        in
        ( Pcf_constraint (Stdlib.fst a)
        , self#constr ctx "Pcf_constraint" [ Stdlib.snd a ] )
      | Pcf_initializer a ->
        let a = self#expression ctx a in
        ( Pcf_initializer (Stdlib.fst a)
        , self#constr ctx "Pcf_initializer" [ Stdlib.snd a ] )
      | Pcf_attribute a ->
        let a = self#attribute ctx a in
        ( Pcf_attribute (Stdlib.fst a)
        , self#constr ctx "Pcf_attribute" [ Stdlib.snd a ] )
      | Pcf_extension a ->
        let a = self#extension ctx a in
        ( Pcf_extension (Stdlib.fst a)
        , self#constr ctx "Pcf_extension" [ Stdlib.snd a ] )
      | Pcf_docstring a ->
        let a = self#string ctx a in
        ( Pcf_docstring (Stdlib.fst a)
        , self#constr ctx "Pcf_docstring" [ Stdlib.snd a ] )

    method class_field_kind
      : 'ctx -> class_field_kind -> class_field_kind * 'res
      =
      fun ctx x ->
      match x with
      | Cfk_virtual a ->
        let a = self#core_type ctx a in
        ( Cfk_virtual (Stdlib.fst a)
        , self#constr ctx "Cfk_virtual" [ Stdlib.snd a ] )
      | Cfk_concrete (a, b) ->
        let a = self#override_flag ctx a in
        let b = self#value_binding ctx b in
        ( Cfk_concrete (Stdlib.fst a, Stdlib.fst b)
        , self#constr ctx "Cfk_concrete" [ Stdlib.snd a; Stdlib.snd b ] )

    method class_declaration
      : 'ctx -> class_declaration -> class_declaration * 'res
      =
      self#class_infos self#class_expr

    method module_type : 'ctx -> module_type -> module_type * 'res =
      fun ctx { pmty_desc; pmty_loc; pmty_attributes; pmty_tokens } ->
      let pmty_desc = self#module_type_desc ctx pmty_desc in
      let pmty_loc = self#location ctx pmty_loc in
      let pmty_attributes = self#attributes ctx pmty_attributes in
      let pmty_tokens = self#token_seq ctx pmty_tokens in
      ( { pmty_desc = Stdlib.fst pmty_desc
        ; pmty_loc = Stdlib.fst pmty_loc
        ; pmty_attributes = Stdlib.fst pmty_attributes
        ; pmty_tokens = Stdlib.fst pmty_tokens
        }
      , self
        #
        record
          ctx
          [ "pmty_desc", Stdlib.snd pmty_desc
          ; "pmty_loc", Stdlib.snd pmty_loc
          ; "pmty_attributes", Stdlib.snd pmty_attributes
          ; "pmty_tokens", Stdlib.snd pmty_tokens
          ] )

    method module_type_desc
      : 'ctx -> module_type_desc -> module_type_desc * 'res
      =
      fun ctx x ->
      match x with
      | Pmty_ident a ->
        let a = self#loc self#longident ctx a in
        ( Pmty_ident (Stdlib.fst a)
        , self#constr ctx "Pmty_ident" [ Stdlib.snd a ] )
      | Pmty_signature a ->
        let a = self#signature ctx a in
        ( Pmty_signature (Stdlib.fst a)
        , self#constr ctx "Pmty_signature" [ Stdlib.snd a ] )
      | Pmty_functor (a, b, c, d) ->
        let a = self#attributes ctx a in
        let b = self#list self#functor_parameter ctx b in
        let c = self#module_type ctx c in
        let d = self#modes ctx d in
        ( Pmty_functor (Stdlib.fst a, Stdlib.fst b, Stdlib.fst c, Stdlib.fst d)
        , self
          #
          constr
            ctx
            "Pmty_functor"
            [ Stdlib.snd a; Stdlib.snd b; Stdlib.snd c; Stdlib.snd d ] )
      | Pmty_functor_type (a, b, c) ->
        let a = self#list self#functor_parameter ctx a in
        let b = self#module_type ctx b in
        let c = self#modes ctx c in
        ( Pmty_functor_type (Stdlib.fst a, Stdlib.fst b, Stdlib.fst c)
        , self
          #
          constr
            ctx
            "Pmty_functor_type"
            [ Stdlib.snd a; Stdlib.snd b; Stdlib.snd c ] )
      | Pmty_with (a, b) ->
        let a = self#module_type ctx a in
        let b = self#list self#with_constraint ctx b in
        ( Pmty_with (Stdlib.fst a, Stdlib.fst b)
        , self#constr ctx "Pmty_with" [ Stdlib.snd a; Stdlib.snd b ] )
      | Pmty_typeof (a, b) ->
        let a = self#attributes ctx a in
        let b = self#module_expr ctx b in
        ( Pmty_typeof (Stdlib.fst a, Stdlib.fst b)
        , self#constr ctx "Pmty_typeof" [ Stdlib.snd a; Stdlib.snd b ] )
      | Pmty_extension a ->
        let a = self#extension ctx a in
        ( Pmty_extension (Stdlib.fst a)
        , self#constr ctx "Pmty_extension" [ Stdlib.snd a ] )
      | Pmty_alias a ->
        let a = self#loc self#longident ctx a in
        ( Pmty_alias (Stdlib.fst a)
        , self#constr ctx "Pmty_alias" [ Stdlib.snd a ] )
      | Pmty_strengthen (a, b) ->
        let a = self#module_type ctx a in
        let b = self#loc self#longident ctx b in
        ( Pmty_strengthen (Stdlib.fst a, Stdlib.fst b)
        , self#constr ctx "Pmty_strengthen" [ Stdlib.snd a; Stdlib.snd b ] )
      | Pmty_parens a ->
        let a = self#module_type ctx a in
        ( Pmty_parens (Stdlib.fst a)
        , self#constr ctx "Pmty_parens" [ Stdlib.snd a ] )

    method functor_parameter
      : 'ctx -> functor_parameter -> functor_parameter * 'res
      =
      fun ctx x ->
      match x with
      | Unit -> Unit, self#constr ctx "Unit" []
      | Named (a, b, c) ->
        let a = self#loc (self#option self#string) ctx a in
        let b = self#module_type ctx b in
        let c = self#modes ctx c in
        ( Named (Stdlib.fst a, Stdlib.fst b, Stdlib.fst c)
        , self#constr ctx "Named" [ Stdlib.snd a; Stdlib.snd b; Stdlib.snd c ] )
      | Unnamed (a, b) ->
        let a = self#module_type ctx a in
        let b = self#modes ctx b in
        ( Unnamed (Stdlib.fst a, Stdlib.fst b)
        , self#constr ctx "Unnamed" [ Stdlib.snd a; Stdlib.snd b ] )

    method signature : 'ctx -> signature -> signature * 'res =
      fun ctx { psg_modalities; psg_items; psg_loc; psg_tokens } ->
      let psg_modalities = self#modalities ctx psg_modalities in
      let psg_items = self#list self#signature_item ctx psg_items in
      let psg_loc = self#location ctx psg_loc in
      let psg_tokens = self#token_seq ctx psg_tokens in
      ( { psg_modalities = Stdlib.fst psg_modalities
        ; psg_items = Stdlib.fst psg_items
        ; psg_loc = Stdlib.fst psg_loc
        ; psg_tokens = Stdlib.fst psg_tokens
        }
      , self
        #
        record
          ctx
          [ "psg_modalities", Stdlib.snd psg_modalities
          ; "psg_items", Stdlib.snd psg_items
          ; "psg_loc", Stdlib.snd psg_loc
          ; "psg_tokens", Stdlib.snd psg_tokens
          ] )

    method signature_item : 'ctx -> signature_item -> signature_item * 'res =
      fun ctx { psig_desc; psig_loc; psig_tokens } ->
      let psig_desc = self#signature_item_desc ctx psig_desc in
      let psig_loc = self#location ctx psig_loc in
      let psig_tokens = self#token_seq ctx psig_tokens in
      ( { psig_desc = Stdlib.fst psig_desc
        ; psig_loc = Stdlib.fst psig_loc
        ; psig_tokens = Stdlib.fst psig_tokens
        }
      , self
        #
        record
          ctx
          [ "psig_desc", Stdlib.snd psig_desc
          ; "psig_loc", Stdlib.snd psig_loc
          ; "psig_tokens", Stdlib.snd psig_tokens
          ] )

    method signature_item_desc
      : 'ctx -> signature_item_desc -> signature_item_desc * 'res
      =
      fun ctx x ->
      match x with
      | Psig_value a ->
        let a = self#value_description ctx a in
        ( Psig_value (Stdlib.fst a)
        , self#constr ctx "Psig_value" [ Stdlib.snd a ] )
      | Psig_type (a, b) ->
        let a = self#rec_flag ctx a in
        let b = self#list self#type_declaration ctx b in
        ( Psig_type (Stdlib.fst a, Stdlib.fst b)
        , self#constr ctx "Psig_type" [ Stdlib.snd a; Stdlib.snd b ] )
      | Psig_typesubst a ->
        let a = self#list self#type_declaration ctx a in
        ( Psig_typesubst (Stdlib.fst a)
        , self#constr ctx "Psig_typesubst" [ Stdlib.snd a ] )
      | Psig_typext a ->
        let a = self#type_extension ctx a in
        ( Psig_typext (Stdlib.fst a)
        , self#constr ctx "Psig_typext" [ Stdlib.snd a ] )
      | Psig_exception a ->
        let a = self#type_exception ctx a in
        ( Psig_exception (Stdlib.fst a)
        , self#constr ctx "Psig_exception" [ Stdlib.snd a ] )
      | Psig_module a ->
        let a = self#module_declaration ctx a in
        ( Psig_module (Stdlib.fst a)
        , self#constr ctx "Psig_module" [ Stdlib.snd a ] )
      | Psig_modsubst a ->
        let a = self#module_substitution ctx a in
        ( Psig_modsubst (Stdlib.fst a)
        , self#constr ctx "Psig_modsubst" [ Stdlib.snd a ] )
      | Psig_recmodule a ->
        let a = self#list self#module_declaration ctx a in
        ( Psig_recmodule (Stdlib.fst a)
        , self#constr ctx "Psig_recmodule" [ Stdlib.snd a ] )
      | Psig_modtype a ->
        let a = self#module_type_declaration ctx a in
        ( Psig_modtype (Stdlib.fst a)
        , self#constr ctx "Psig_modtype" [ Stdlib.snd a ] )
      | Psig_modtypesubst a ->
        let a = self#module_type_declaration ctx a in
        ( Psig_modtypesubst (Stdlib.fst a)
        , self#constr ctx "Psig_modtypesubst" [ Stdlib.snd a ] )
      | Psig_open a ->
        let a = self#open_description ctx a in
        Psig_open (Stdlib.fst a), self#constr ctx "Psig_open" [ Stdlib.snd a ]
      | Psig_include (a, b) ->
        let a = self#include_description ctx a in
        let b = self#modalities ctx b in
        ( Psig_include (Stdlib.fst a, Stdlib.fst b)
        , self#constr ctx "Psig_include" [ Stdlib.snd a; Stdlib.snd b ] )
      | Psig_class a ->
        let a = self#list self#class_description ctx a in
        ( Psig_class (Stdlib.fst a)
        , self#constr ctx "Psig_class" [ Stdlib.snd a ] )
      | Psig_class_type a ->
        let a = self#list self#class_type_declaration ctx a in
        ( Psig_class_type (Stdlib.fst a)
        , self#constr ctx "Psig_class_type" [ Stdlib.snd a ] )
      | Psig_attribute a ->
        let a = self#attribute ctx a in
        ( Psig_attribute (Stdlib.fst a)
        , self#constr ctx "Psig_attribute" [ Stdlib.snd a ] )
      | Psig_extension a ->
        let a = self#toplevel_extension ctx a in
        ( Psig_extension (Stdlib.fst a)
        , self#constr ctx "Psig_extension" [ Stdlib.snd a ] )
      | Psig_kind_abbrev (a, b) ->
        let a = self#loc self#string ctx a in
        let b = self#jkind_annotation ctx b in
        ( Psig_kind_abbrev (Stdlib.fst a, Stdlib.fst b)
        , self#constr ctx "Psig_kind_abbrev" [ Stdlib.snd a; Stdlib.snd b ] )
      | Psig_docstring a ->
        let a = self#string ctx a in
        ( Psig_docstring (Stdlib.fst a)
        , self#constr ctx "Psig_docstring" [ Stdlib.snd a ] )

    method module_declaration_body
      : 'ctx -> module_declaration_body -> module_declaration_body * 'res
      =
      fun ctx x ->
      match x with
      | With_params (a, b, c) ->
        let a = self#list self#functor_parameter ctx a in
        let b = self#module_type ctx b in
        let c = self#modes ctx c in
        ( With_params (Stdlib.fst a, Stdlib.fst b, Stdlib.fst c)
        , self
          #
          constr
            ctx
            "With_params"
            [ Stdlib.snd a; Stdlib.snd b; Stdlib.snd c ] )
      | Without_params (a, b) ->
        let a = self#module_type ctx a in
        let b = self#modalities ctx b in
        ( Without_params (Stdlib.fst a, Stdlib.fst b)
        , self#constr ctx "Without_params" [ Stdlib.snd a; Stdlib.snd b ] )

    method module_declaration
      : 'ctx -> module_declaration -> module_declaration * 'res
      =
      fun
        ctx
        { pmd_pre_text
        ; pmd_pre_doc
        ; pmd_ext_attrs
        ; pmd_name
        ; pmd_body
        ; pmd_attributes
        ; pmd_post_doc
        ; pmd_loc
        ; pmd_tokens
        }
        ->
      let pmd_pre_text = self#list self#string ctx pmd_pre_text in
      let pmd_pre_doc = self#option self#string ctx pmd_pre_doc in
      let pmd_ext_attrs = self#ext_attribute ctx pmd_ext_attrs in
      let pmd_name =
        (fun ctx (a, b) ->
          let a = self#loc (self#option self#string) ctx a in
          let b = self#modalities ctx b in
          ( (Stdlib.fst a, Stdlib.fst b)
          , self#tuple ctx [ Stdlib.snd a; Stdlib.snd b ] ))
          ctx
          pmd_name
      in
      let pmd_body = self#module_declaration_body ctx pmd_body in
      let pmd_attributes = self#attributes ctx pmd_attributes in
      let pmd_post_doc = self#option self#string ctx pmd_post_doc in
      let pmd_loc = self#location ctx pmd_loc in
      let pmd_tokens = self#token_seq ctx pmd_tokens in
      ( { pmd_pre_text = Stdlib.fst pmd_pre_text
        ; pmd_pre_doc = Stdlib.fst pmd_pre_doc
        ; pmd_ext_attrs = Stdlib.fst pmd_ext_attrs
        ; pmd_name = Stdlib.fst pmd_name
        ; pmd_body = Stdlib.fst pmd_body
        ; pmd_attributes = Stdlib.fst pmd_attributes
        ; pmd_post_doc = Stdlib.fst pmd_post_doc
        ; pmd_loc = Stdlib.fst pmd_loc
        ; pmd_tokens = Stdlib.fst pmd_tokens
        }
      , self
        #
        record
          ctx
          [ "pmd_pre_text", Stdlib.snd pmd_pre_text
          ; "pmd_pre_doc", Stdlib.snd pmd_pre_doc
          ; "pmd_ext_attrs", Stdlib.snd pmd_ext_attrs
          ; "pmd_name", Stdlib.snd pmd_name
          ; "pmd_body", Stdlib.snd pmd_body
          ; "pmd_attributes", Stdlib.snd pmd_attributes
          ; "pmd_post_doc", Stdlib.snd pmd_post_doc
          ; "pmd_loc", Stdlib.snd pmd_loc
          ; "pmd_tokens", Stdlib.snd pmd_tokens
          ] )

    method module_substitution
      : 'ctx -> module_substitution -> module_substitution * 'res
      =
      fun
        ctx
        { pms_pre_doc
        ; pms_ext_attrs
        ; pms_name
        ; pms_manifest
        ; pms_attributes
        ; pms_post_doc
        ; pms_loc
        ; pms_tokens
        }
        ->
      let pms_pre_doc = self#option self#string ctx pms_pre_doc in
      let pms_ext_attrs = self#ext_attribute ctx pms_ext_attrs in
      let pms_name = self#loc self#string ctx pms_name in
      let pms_manifest = self#loc self#longident ctx pms_manifest in
      let pms_attributes = self#attributes ctx pms_attributes in
      let pms_post_doc = self#option self#string ctx pms_post_doc in
      let pms_loc = self#location ctx pms_loc in
      let pms_tokens = self#token_seq ctx pms_tokens in
      ( { pms_pre_doc = Stdlib.fst pms_pre_doc
        ; pms_ext_attrs = Stdlib.fst pms_ext_attrs
        ; pms_name = Stdlib.fst pms_name
        ; pms_manifest = Stdlib.fst pms_manifest
        ; pms_attributes = Stdlib.fst pms_attributes
        ; pms_post_doc = Stdlib.fst pms_post_doc
        ; pms_loc = Stdlib.fst pms_loc
        ; pms_tokens = Stdlib.fst pms_tokens
        }
      , self
        #
        record
          ctx
          [ "pms_pre_doc", Stdlib.snd pms_pre_doc
          ; "pms_ext_attrs", Stdlib.snd pms_ext_attrs
          ; "pms_name", Stdlib.snd pms_name
          ; "pms_manifest", Stdlib.snd pms_manifest
          ; "pms_attributes", Stdlib.snd pms_attributes
          ; "pms_post_doc", Stdlib.snd pms_post_doc
          ; "pms_loc", Stdlib.snd pms_loc
          ; "pms_tokens", Stdlib.snd pms_tokens
          ] )

    method module_type_declaration
      : 'ctx -> module_type_declaration -> module_type_declaration * 'res
      =
      fun
        ctx
        { pmtd_pre_doc
        ; pmtd_ext_attrs
        ; pmtd_name
        ; pmtd_type
        ; pmtd_attributes
        ; pmtd_post_doc
        ; pmtd_loc
        ; pmtd_tokens
        }
        ->
      let pmtd_pre_doc = self#option self#string ctx pmtd_pre_doc in
      let pmtd_ext_attrs = self#ext_attribute ctx pmtd_ext_attrs in
      let pmtd_name = self#loc self#string ctx pmtd_name in
      let pmtd_type = self#option self#module_type ctx pmtd_type in
      let pmtd_attributes = self#attributes ctx pmtd_attributes in
      let pmtd_post_doc = self#option self#string ctx pmtd_post_doc in
      let pmtd_loc = self#location ctx pmtd_loc in
      let pmtd_tokens = self#token_seq ctx pmtd_tokens in
      ( { pmtd_pre_doc = Stdlib.fst pmtd_pre_doc
        ; pmtd_ext_attrs = Stdlib.fst pmtd_ext_attrs
        ; pmtd_name = Stdlib.fst pmtd_name
        ; pmtd_type = Stdlib.fst pmtd_type
        ; pmtd_attributes = Stdlib.fst pmtd_attributes
        ; pmtd_post_doc = Stdlib.fst pmtd_post_doc
        ; pmtd_loc = Stdlib.fst pmtd_loc
        ; pmtd_tokens = Stdlib.fst pmtd_tokens
        }
      , self
        #
        record
          ctx
          [ "pmtd_pre_doc", Stdlib.snd pmtd_pre_doc
          ; "pmtd_ext_attrs", Stdlib.snd pmtd_ext_attrs
          ; "pmtd_name", Stdlib.snd pmtd_name
          ; "pmtd_type", Stdlib.snd pmtd_type
          ; "pmtd_attributes", Stdlib.snd pmtd_attributes
          ; "pmtd_post_doc", Stdlib.snd pmtd_post_doc
          ; "pmtd_loc", Stdlib.snd pmtd_loc
          ; "pmtd_tokens", Stdlib.snd pmtd_tokens
          ] )

    method open_infos
      :
        'a
        .  ('ctx -> 'a -> 'a * 'res)
        -> 'ctx
        -> 'a open_infos
        -> 'a open_infos * 'res
      =
      fun
        _a ctx
        { popen_pre_doc
        ; popen_ext_attrs
        ; popen_expr
        ; popen_override
        ; popen_loc
        ; popen_attributes
        ; popen_post_doc
        ; popen_tokens
        }
        ->
      let popen_pre_doc = self#option self#string ctx popen_pre_doc in
      let popen_ext_attrs = self#ext_attribute ctx popen_ext_attrs in
      let popen_expr = _a ctx popen_expr in
      let popen_override = self#override_flag ctx popen_override in
      let popen_loc = self#location ctx popen_loc in
      let popen_attributes = self#attributes ctx popen_attributes in
      let popen_post_doc = self#option self#string ctx popen_post_doc in
      let popen_tokens = self#token_seq ctx popen_tokens in
      ( { popen_pre_doc = Stdlib.fst popen_pre_doc
        ; popen_ext_attrs = Stdlib.fst popen_ext_attrs
        ; popen_expr = Stdlib.fst popen_expr
        ; popen_override = Stdlib.fst popen_override
        ; popen_loc = Stdlib.fst popen_loc
        ; popen_attributes = Stdlib.fst popen_attributes
        ; popen_post_doc = Stdlib.fst popen_post_doc
        ; popen_tokens = Stdlib.fst popen_tokens
        }
      , self
        #
        record
          ctx
          [ "popen_pre_doc", Stdlib.snd popen_pre_doc
          ; "popen_ext_attrs", Stdlib.snd popen_ext_attrs
          ; "popen_expr", Stdlib.snd popen_expr
          ; "popen_override", Stdlib.snd popen_override
          ; "popen_loc", Stdlib.snd popen_loc
          ; "popen_attributes", Stdlib.snd popen_attributes
          ; "popen_post_doc", Stdlib.snd popen_post_doc
          ; "popen_tokens", Stdlib.snd popen_tokens
          ] )

    method open_description
      : 'ctx -> open_description -> open_description * 'res
      =
      self#open_infos (self#loc self#longident)

    method open_declaration
      : 'ctx -> open_declaration -> open_declaration * 'res
      =
      self#open_infos self#module_expr

    method include_infos
      :
        'a
        .  ('ctx -> 'a -> 'a * 'res)
        -> 'ctx
        -> 'a include_infos
        -> 'a include_infos * 'res
      =
      fun
        _a ctx
        { pincl_pre_doc
        ; pincl_kind
        ; pincl_ext_attrs
        ; pincl_mod
        ; pincl_loc
        ; pincl_attributes
        ; pincl_post_doc
        ; pincl_tokens
        }
        ->
      let pincl_pre_doc = self#option self#string ctx pincl_pre_doc in
      let pincl_kind = self#include_kind ctx pincl_kind in
      let pincl_ext_attrs = self#ext_attribute ctx pincl_ext_attrs in
      let pincl_mod = _a ctx pincl_mod in
      let pincl_loc = self#location ctx pincl_loc in
      let pincl_attributes = self#attributes ctx pincl_attributes in
      let pincl_post_doc = self#option self#string ctx pincl_post_doc in
      let pincl_tokens = self#token_seq ctx pincl_tokens in
      ( { pincl_pre_doc = Stdlib.fst pincl_pre_doc
        ; pincl_kind = Stdlib.fst pincl_kind
        ; pincl_ext_attrs = Stdlib.fst pincl_ext_attrs
        ; pincl_mod = Stdlib.fst pincl_mod
        ; pincl_loc = Stdlib.fst pincl_loc
        ; pincl_attributes = Stdlib.fst pincl_attributes
        ; pincl_post_doc = Stdlib.fst pincl_post_doc
        ; pincl_tokens = Stdlib.fst pincl_tokens
        }
      , self
        #
        record
          ctx
          [ "pincl_pre_doc", Stdlib.snd pincl_pre_doc
          ; "pincl_kind", Stdlib.snd pincl_kind
          ; "pincl_ext_attrs", Stdlib.snd pincl_ext_attrs
          ; "pincl_mod", Stdlib.snd pincl_mod
          ; "pincl_loc", Stdlib.snd pincl_loc
          ; "pincl_attributes", Stdlib.snd pincl_attributes
          ; "pincl_post_doc", Stdlib.snd pincl_post_doc
          ; "pincl_tokens", Stdlib.snd pincl_tokens
          ] )

    method include_description
      : 'ctx -> include_description -> include_description * 'res
      =
      self#include_infos self#module_type

    method include_declaration
      : 'ctx -> include_declaration -> include_declaration * 'res
      =
      self#include_infos self#module_expr

    method with_constraint : 'ctx -> with_constraint -> with_constraint * 'res =
      fun ctx { wc_desc; wc_loc; wc_tokens } ->
      let wc_desc = self#with_constraint_desc ctx wc_desc in
      let wc_loc = self#location ctx wc_loc in
      let wc_tokens = self#token_seq ctx wc_tokens in
      ( { wc_desc = Stdlib.fst wc_desc
        ; wc_loc = Stdlib.fst wc_loc
        ; wc_tokens = Stdlib.fst wc_tokens
        }
      , self
        #
        record
          ctx
          [ "wc_desc", Stdlib.snd wc_desc
          ; "wc_loc", Stdlib.snd wc_loc
          ; "wc_tokens", Stdlib.snd wc_tokens
          ] )

    method with_constraint_desc
      : 'ctx -> with_constraint_desc -> with_constraint_desc * 'res
      =
      fun ctx x ->
      match x with
      | Pwith_type (a, b, c, d, e) ->
        let a = self#ptype_params ctx a in
        let b = self#loc self#longident ctx b in
        let c = self#private_flag ctx c in
        let d = self#core_type ctx d in
        let e = self#list self#ptype_constraint ctx e in
        ( Pwith_type
            ( Stdlib.fst a
            , Stdlib.fst b
            , Stdlib.fst c
            , Stdlib.fst d
            , Stdlib.fst e )
        , self
          #
          constr
            ctx
            "Pwith_type"
            [ Stdlib.snd a
            ; Stdlib.snd b
            ; Stdlib.snd c
            ; Stdlib.snd d
            ; Stdlib.snd e
            ] )
      | Pwith_module (a, b) ->
        let a = self#loc self#longident ctx a in
        let b = self#loc self#longident ctx b in
        ( Pwith_module (Stdlib.fst a, Stdlib.fst b)
        , self#constr ctx "Pwith_module" [ Stdlib.snd a; Stdlib.snd b ] )
      | Pwith_modtype (a, b) ->
        let a = self#loc self#longident ctx a in
        let b = self#module_type ctx b in
        ( Pwith_modtype (Stdlib.fst a, Stdlib.fst b)
        , self#constr ctx "Pwith_modtype" [ Stdlib.snd a; Stdlib.snd b ] )
      | Pwith_modtypesubst (a, b) ->
        let a = self#loc self#longident ctx a in
        let b = self#module_type ctx b in
        ( Pwith_modtypesubst (Stdlib.fst a, Stdlib.fst b)
        , self#constr ctx "Pwith_modtypesubst" [ Stdlib.snd a; Stdlib.snd b ] )
      | Pwith_typesubst (a, b, c) ->
        let a = self#ptype_params ctx a in
        let b = self#loc self#longident ctx b in
        let c = self#core_type ctx c in
        ( Pwith_typesubst (Stdlib.fst a, Stdlib.fst b, Stdlib.fst c)
        , self
          #
          constr
            ctx
            "Pwith_typesubst"
            [ Stdlib.snd a; Stdlib.snd b; Stdlib.snd c ] )
      | Pwith_modsubst (a, b) ->
        let a = self#loc self#longident ctx a in
        let b = self#loc self#longident ctx b in
        ( Pwith_modsubst (Stdlib.fst a, Stdlib.fst b)
        , self#constr ctx "Pwith_modsubst" [ Stdlib.snd a; Stdlib.snd b ] )

    method module_expr : 'ctx -> module_expr -> module_expr * 'res =
      fun ctx { pmod_desc; pmod_loc; pmod_attributes; pmod_tokens } ->
      let pmod_desc = self#module_expr_desc ctx pmod_desc in
      let pmod_loc = self#location ctx pmod_loc in
      let pmod_attributes = self#attributes ctx pmod_attributes in
      let pmod_tokens = self#token_seq ctx pmod_tokens in
      ( { pmod_desc = Stdlib.fst pmod_desc
        ; pmod_loc = Stdlib.fst pmod_loc
        ; pmod_attributes = Stdlib.fst pmod_attributes
        ; pmod_tokens = Stdlib.fst pmod_tokens
        }
      , self
        #
        record
          ctx
          [ "pmod_desc", Stdlib.snd pmod_desc
          ; "pmod_loc", Stdlib.snd pmod_loc
          ; "pmod_attributes", Stdlib.snd pmod_attributes
          ; "pmod_tokens", Stdlib.snd pmod_tokens
          ] )

    method module_expr_desc
      : 'ctx -> module_expr_desc -> module_expr_desc * 'res
      =
      fun ctx x ->
      match x with
      | Pmod_ident a ->
        let a = self#loc self#longident ctx a in
        ( Pmod_ident (Stdlib.fst a)
        , self#constr ctx "Pmod_ident" [ Stdlib.snd a ] )
      | Pmod_structure (a, b) ->
        let a = self#attributes ctx a in
        let b = self#structure ctx b in
        ( Pmod_structure (Stdlib.fst a, Stdlib.fst b)
        , self#constr ctx "Pmod_structure" [ Stdlib.snd a; Stdlib.snd b ] )
      | Pmod_functor (a, b, c) ->
        let a = self#attributes ctx a in
        let b = self#list self#functor_parameter ctx b in
        let c = self#module_expr ctx c in
        ( Pmod_functor (Stdlib.fst a, Stdlib.fst b, Stdlib.fst c)
        , self
          #
          constr
            ctx
            "Pmod_functor"
            [ Stdlib.snd a; Stdlib.snd b; Stdlib.snd c ] )
      | Pmod_apply (a, b) ->
        let a = self#module_expr ctx a in
        let b = self#module_expr ctx b in
        ( Pmod_apply (Stdlib.fst a, Stdlib.fst b)
        , self#constr ctx "Pmod_apply" [ Stdlib.snd a; Stdlib.snd b ] )
      | Pmod_apply_unit a ->
        let a = self#module_expr ctx a in
        ( Pmod_apply_unit (Stdlib.fst a)
        , self#constr ctx "Pmod_apply_unit" [ Stdlib.snd a ] )
      | Pmod_constraint (a, b, c) ->
        let a = self#module_expr ctx a in
        let b = self#option self#module_type ctx b in
        let c = self#modes ctx c in
        ( Pmod_constraint (Stdlib.fst a, Stdlib.fst b, Stdlib.fst c)
        , self
          #
          constr
            ctx
            "Pmod_constraint"
            [ Stdlib.snd a; Stdlib.snd b; Stdlib.snd c ] )
      | Pmod_unpack (a, b, c) ->
        let a = self#expression ctx a in
        let b = self#option self#package_type ctx b in
        let c = self#option self#package_type ctx c in
        ( Pmod_unpack (Stdlib.fst a, Stdlib.fst b, Stdlib.fst c)
        , self
          #
          constr
            ctx
            "Pmod_unpack"
            [ Stdlib.snd a; Stdlib.snd b; Stdlib.snd c ] )
      | Pmod_extension a ->
        let a = self#extension ctx a in
        ( Pmod_extension (Stdlib.fst a)
        , self#constr ctx "Pmod_extension" [ Stdlib.snd a ] )
      | Pmod_parens a ->
        let a = self#module_expr ctx a in
        ( Pmod_parens (Stdlib.fst a)
        , self#constr ctx "Pmod_parens" [ Stdlib.snd a ] )

    method structure : 'ctx -> structure -> structure * 'res =
      fun ctx (a, b) ->
      let a = self#list self#structure_item ctx a in
      let b = self#token_seq ctx b in
      ( (Stdlib.fst a, Stdlib.fst b)
      , self#tuple ctx [ Stdlib.snd a; Stdlib.snd b ] )

    method structure_item : 'ctx -> structure_item -> structure_item * 'res =
      fun ctx { pstr_desc; pstr_loc; pstr_tokens } ->
      let pstr_desc = self#structure_item_desc ctx pstr_desc in
      let pstr_loc = self#location ctx pstr_loc in
      let pstr_tokens = self#token_seq ctx pstr_tokens in
      ( { pstr_desc = Stdlib.fst pstr_desc
        ; pstr_loc = Stdlib.fst pstr_loc
        ; pstr_tokens = Stdlib.fst pstr_tokens
        }
      , self
        #
        record
          ctx
          [ "pstr_desc", Stdlib.snd pstr_desc
          ; "pstr_loc", Stdlib.snd pstr_loc
          ; "pstr_tokens", Stdlib.snd pstr_tokens
          ] )

    method structure_item_desc
      : 'ctx -> structure_item_desc -> structure_item_desc * 'res
      =
      fun ctx x ->
      match x with
      | Pstr_eval (a, b) ->
        let a = self#expression ctx a in
        let b = self#attributes ctx b in
        ( Pstr_eval (Stdlib.fst a, Stdlib.fst b)
        , self#constr ctx "Pstr_eval" [ Stdlib.snd a; Stdlib.snd b ] )
      | Pstr_value (a, b) ->
        let a = self#rec_flag ctx a in
        let b = self#list self#value_binding ctx b in
        ( Pstr_value (Stdlib.fst a, Stdlib.fst b)
        , self#constr ctx "Pstr_value" [ Stdlib.snd a; Stdlib.snd b ] )
      | Pstr_primitive a ->
        let a = self#value_description ctx a in
        ( Pstr_primitive (Stdlib.fst a)
        , self#constr ctx "Pstr_primitive" [ Stdlib.snd a ] )
      | Pstr_type (a, b) ->
        let a = self#rec_flag ctx a in
        let b = self#list self#type_declaration ctx b in
        ( Pstr_type (Stdlib.fst a, Stdlib.fst b)
        , self#constr ctx "Pstr_type" [ Stdlib.snd a; Stdlib.snd b ] )
      | Pstr_typext a ->
        let a = self#type_extension ctx a in
        ( Pstr_typext (Stdlib.fst a)
        , self#constr ctx "Pstr_typext" [ Stdlib.snd a ] )
      | Pstr_exception a ->
        let a = self#type_exception ctx a in
        ( Pstr_exception (Stdlib.fst a)
        , self#constr ctx "Pstr_exception" [ Stdlib.snd a ] )
      | Pstr_module a ->
        let a = self#module_binding ctx a in
        ( Pstr_module (Stdlib.fst a)
        , self#constr ctx "Pstr_module" [ Stdlib.snd a ] )
      | Pstr_recmodule a ->
        let a = self#list self#module_binding ctx a in
        ( Pstr_recmodule (Stdlib.fst a)
        , self#constr ctx "Pstr_recmodule" [ Stdlib.snd a ] )
      | Pstr_modtype a ->
        let a = self#module_type_declaration ctx a in
        ( Pstr_modtype (Stdlib.fst a)
        , self#constr ctx "Pstr_modtype" [ Stdlib.snd a ] )
      | Pstr_open a ->
        let a = self#open_declaration ctx a in
        Pstr_open (Stdlib.fst a), self#constr ctx "Pstr_open" [ Stdlib.snd a ]
      | Pstr_class a ->
        let a = self#list self#class_declaration ctx a in
        ( Pstr_class (Stdlib.fst a)
        , self#constr ctx "Pstr_class" [ Stdlib.snd a ] )
      | Pstr_class_type a ->
        let a = self#list self#class_type_declaration ctx a in
        ( Pstr_class_type (Stdlib.fst a)
        , self#constr ctx "Pstr_class_type" [ Stdlib.snd a ] )
      | Pstr_include a ->
        let a = self#include_declaration ctx a in
        ( Pstr_include (Stdlib.fst a)
        , self#constr ctx "Pstr_include" [ Stdlib.snd a ] )
      | Pstr_attribute a ->
        let a = self#attribute ctx a in
        ( Pstr_attribute (Stdlib.fst a)
        , self#constr ctx "Pstr_attribute" [ Stdlib.snd a ] )
      | Pstr_extension a ->
        let a = self#toplevel_extension ctx a in
        ( Pstr_extension (Stdlib.fst a)
        , self#constr ctx "Pstr_extension" [ Stdlib.snd a ] )
      | Pstr_kind_abbrev (a, b) ->
        let a = self#loc self#string ctx a in
        let b = self#jkind_annotation ctx b in
        ( Pstr_kind_abbrev (Stdlib.fst a, Stdlib.fst b)
        , self#constr ctx "Pstr_kind_abbrev" [ Stdlib.snd a; Stdlib.snd b ] )
      | Pstr_docstring a ->
        let a = self#string ctx a in
        ( Pstr_docstring (Stdlib.fst a)
        , self#constr ctx "Pstr_docstring" [ Stdlib.snd a ] )

    method value_constraint
      : 'ctx -> value_constraint -> value_constraint * 'res
      =
      fun ctx x ->
      match x with
      | Pvc_constraint { locally_abstract_univars; typ } ->
        let locally_abstract_univars =
          self
          #
          list
            (fun ctx (a, b) ->
              let a = self#loc self#string ctx a in
              let b = self#option self#jkind_annotation ctx b in
              ( (Stdlib.fst a, Stdlib.fst b)
              , self#tuple ctx [ Stdlib.snd a; Stdlib.snd b ] ))
            ctx
            locally_abstract_univars
        in
        let typ = self#core_type ctx typ in
        ( Pvc_constraint
            { locally_abstract_univars = Stdlib.fst locally_abstract_univars
            ; typ = Stdlib.fst typ
            }
        , self
          #
          constr
            ctx
            "Pvc_constraint"
            [ self
              #
              record
                ctx
                [ ( "locally_abstract_univars"
                  , Stdlib.snd locally_abstract_univars )
                ; "typ", Stdlib.snd typ
                ]
            ] )
      | Pvc_coercion { ground; coercion } ->
        let ground = self#option self#core_type ctx ground in
        let coercion = self#core_type ctx coercion in
        ( Pvc_coercion
            { ground = Stdlib.fst ground; coercion = Stdlib.fst coercion }
        , self
          #
          constr
            ctx
            "Pvc_coercion"
            [ self
              #
              record
                ctx
                [ "ground", Stdlib.snd ground
                ; "coercion", Stdlib.snd coercion
                ]
            ] )

    method value_binding : 'ctx -> value_binding -> value_binding * 'res =
      fun
        ctx
        { pvb_pre_text
        ; pvb_pre_doc
        ; pvb_ext_attrs
        ; pvb_legacy_modes
        ; pvb_pat
        ; pvb_modes
        ; pvb_params
        ; pvb_constraint
        ; pvb_expr
        ; pvb_ret_modes
        ; pvb_attributes
        ; pvb_post_doc
        ; pvb_loc
        ; pvb_tokens
        }
        ->
      let pvb_pre_text = self#list self#string ctx pvb_pre_text in
      let pvb_pre_doc = self#option self#string ctx pvb_pre_doc in
      let pvb_ext_attrs = self#ext_attribute ctx pvb_ext_attrs in
      let pvb_legacy_modes = self#modes ctx pvb_legacy_modes in
      let pvb_pat = self#pattern ctx pvb_pat in
      let pvb_modes = self#modes ctx pvb_modes in
      let pvb_params = self#list self#function_param ctx pvb_params in
      let pvb_constraint =
        self#option self#value_constraint ctx pvb_constraint
      in
      let pvb_expr = self#option self#expression ctx pvb_expr in
      let pvb_ret_modes = self#modes ctx pvb_ret_modes in
      let pvb_attributes = self#attributes ctx pvb_attributes in
      let pvb_post_doc = self#option self#string ctx pvb_post_doc in
      let pvb_loc = self#location ctx pvb_loc in
      let pvb_tokens = self#token_seq ctx pvb_tokens in
      ( { pvb_pre_text = Stdlib.fst pvb_pre_text
        ; pvb_pre_doc = Stdlib.fst pvb_pre_doc
        ; pvb_ext_attrs = Stdlib.fst pvb_ext_attrs
        ; pvb_legacy_modes = Stdlib.fst pvb_legacy_modes
        ; pvb_pat = Stdlib.fst pvb_pat
        ; pvb_modes = Stdlib.fst pvb_modes
        ; pvb_params = Stdlib.fst pvb_params
        ; pvb_constraint = Stdlib.fst pvb_constraint
        ; pvb_expr = Stdlib.fst pvb_expr
        ; pvb_ret_modes = Stdlib.fst pvb_ret_modes
        ; pvb_attributes = Stdlib.fst pvb_attributes
        ; pvb_post_doc = Stdlib.fst pvb_post_doc
        ; pvb_loc = Stdlib.fst pvb_loc
        ; pvb_tokens = Stdlib.fst pvb_tokens
        }
      , self
        #
        record
          ctx
          [ "pvb_pre_text", Stdlib.snd pvb_pre_text
          ; "pvb_pre_doc", Stdlib.snd pvb_pre_doc
          ; "pvb_ext_attrs", Stdlib.snd pvb_ext_attrs
          ; "pvb_legacy_modes", Stdlib.snd pvb_legacy_modes
          ; "pvb_pat", Stdlib.snd pvb_pat
          ; "pvb_modes", Stdlib.snd pvb_modes
          ; "pvb_params", Stdlib.snd pvb_params
          ; "pvb_constraint", Stdlib.snd pvb_constraint
          ; "pvb_expr", Stdlib.snd pvb_expr
          ; "pvb_ret_modes", Stdlib.snd pvb_ret_modes
          ; "pvb_attributes", Stdlib.snd pvb_attributes
          ; "pvb_post_doc", Stdlib.snd pvb_post_doc
          ; "pvb_loc", Stdlib.snd pvb_loc
          ; "pvb_tokens", Stdlib.snd pvb_tokens
          ] )

    method module_binding : 'ctx -> module_binding -> module_binding * 'res =
      fun
        ctx
        { pmb_pre_text
        ; pmb_pre_doc
        ; pmb_ext_attrs
        ; pmb_name
        ; pmb_params
        ; pmb_constraint
        ; pmb_modes
        ; pmb_expr
        ; pmb_attributes
        ; pmb_post_doc
        ; pmb_loc
        ; pmb_tokens
        }
        ->
      let pmb_pre_text = self#list self#string ctx pmb_pre_text in
      let pmb_pre_doc = self#option self#string ctx pmb_pre_doc in
      let pmb_ext_attrs = self#ext_attribute ctx pmb_ext_attrs in
      let pmb_name =
        (fun ctx (a, b) ->
          let a = self#loc (self#option self#string) ctx a in
          let b = self#modes ctx b in
          ( (Stdlib.fst a, Stdlib.fst b)
          , self#tuple ctx [ Stdlib.snd a; Stdlib.snd b ] ))
          ctx
          pmb_name
      in
      let pmb_params = self#list self#functor_parameter ctx pmb_params in
      let pmb_constraint = self#option self#module_type ctx pmb_constraint in
      let pmb_modes = self#modes ctx pmb_modes in
      let pmb_expr = self#module_expr ctx pmb_expr in
      let pmb_attributes = self#attributes ctx pmb_attributes in
      let pmb_post_doc = self#option self#string ctx pmb_post_doc in
      let pmb_loc = self#location ctx pmb_loc in
      let pmb_tokens = self#token_seq ctx pmb_tokens in
      ( { pmb_pre_text = Stdlib.fst pmb_pre_text
        ; pmb_pre_doc = Stdlib.fst pmb_pre_doc
        ; pmb_ext_attrs = Stdlib.fst pmb_ext_attrs
        ; pmb_name = Stdlib.fst pmb_name
        ; pmb_params = Stdlib.fst pmb_params
        ; pmb_constraint = Stdlib.fst pmb_constraint
        ; pmb_modes = Stdlib.fst pmb_modes
        ; pmb_expr = Stdlib.fst pmb_expr
        ; pmb_attributes = Stdlib.fst pmb_attributes
        ; pmb_post_doc = Stdlib.fst pmb_post_doc
        ; pmb_loc = Stdlib.fst pmb_loc
        ; pmb_tokens = Stdlib.fst pmb_tokens
        }
      , self
        #
        record
          ctx
          [ "pmb_pre_text", Stdlib.snd pmb_pre_text
          ; "pmb_pre_doc", Stdlib.snd pmb_pre_doc
          ; "pmb_ext_attrs", Stdlib.snd pmb_ext_attrs
          ; "pmb_name", Stdlib.snd pmb_name
          ; "pmb_params", Stdlib.snd pmb_params
          ; "pmb_constraint", Stdlib.snd pmb_constraint
          ; "pmb_modes", Stdlib.snd pmb_modes
          ; "pmb_expr", Stdlib.snd pmb_expr
          ; "pmb_attributes", Stdlib.snd pmb_attributes
          ; "pmb_post_doc", Stdlib.snd pmb_post_doc
          ; "pmb_loc", Stdlib.snd pmb_loc
          ; "pmb_tokens", Stdlib.snd pmb_tokens
          ] )

    method jkind_annotation_desc
      : 'ctx -> jkind_annotation_desc -> jkind_annotation_desc * 'res
      =
      fun ctx x ->
      match x with
      | Pjk_default -> Pjk_default, self#constr ctx "Pjk_default" []
      | Pjk_abbreviation a ->
        let a = self#string ctx a in
        ( Pjk_abbreviation (Stdlib.fst a)
        , self#constr ctx "Pjk_abbreviation" [ Stdlib.snd a ] )
      | Pjk_mod (a, b) ->
        let a = self#jkind_annotation ctx a in
        let b = self#modes ctx b in
        ( Pjk_mod (Stdlib.fst a, Stdlib.fst b)
        , self#constr ctx "Pjk_mod" [ Stdlib.snd a; Stdlib.snd b ] )
      | Pjk_with (a, b, c) ->
        let a = self#jkind_annotation ctx a in
        let b = self#core_type ctx b in
        let c = self#modalities ctx c in
        ( Pjk_with (Stdlib.fst a, Stdlib.fst b, Stdlib.fst c)
        , self
          #
          constr
            ctx
            "Pjk_with"
            [ Stdlib.snd a; Stdlib.snd b; Stdlib.snd c ] )
      | Pjk_kind_of a ->
        let a = self#core_type ctx a in
        ( Pjk_kind_of (Stdlib.fst a)
        , self#constr ctx "Pjk_kind_of" [ Stdlib.snd a ] )
      | Pjk_product a ->
        let a = self#list self#jkind_annotation ctx a in
        ( Pjk_product (Stdlib.fst a)
        , self#constr ctx "Pjk_product" [ Stdlib.snd a ] )
      | Pjk_parens a ->
        let a = self#jkind_annotation_desc ctx a in
        ( Pjk_parens (Stdlib.fst a)
        , self#constr ctx "Pjk_parens" [ Stdlib.snd a ] )

    method jkind_annotation
      : 'ctx -> jkind_annotation -> jkind_annotation * 'res
      =
      fun ctx { pjkind_loc; pjkind_desc; pjkind_tokens } ->
      let pjkind_loc = self#location ctx pjkind_loc in
      let pjkind_desc = self#jkind_annotation_desc ctx pjkind_desc in
      let pjkind_tokens = self#token_seq ctx pjkind_tokens in
      ( { pjkind_loc = Stdlib.fst pjkind_loc
        ; pjkind_desc = Stdlib.fst pjkind_desc
        ; pjkind_tokens = Stdlib.fst pjkind_tokens
        }
      , self
        #
        record
          ctx
          [ "pjkind_loc", Stdlib.snd pjkind_loc
          ; "pjkind_desc", Stdlib.snd pjkind_desc
          ; "pjkind_tokens", Stdlib.snd pjkind_tokens
          ] )

    method use_file : 'ctx -> use_file -> use_file * 'res =
      fun ctx (a, b) ->
      let a = self#list self#toplevel_phrase ctx a in
      let b = self#token_seq ctx b in
      ( (Stdlib.fst a, Stdlib.fst b)
      , self#tuple ctx [ Stdlib.snd a; Stdlib.snd b ] )

    method toplevel_phrase : 'ctx -> toplevel_phrase -> toplevel_phrase * 'res =
      fun ctx x ->
      match x with
      | Ptop_def a ->
        let a = self#structure ctx a in
        Ptop_def (Stdlib.fst a), self#constr ctx "Ptop_def" [ Stdlib.snd a ]
      | Ptop_dir a ->
        let a = self#toplevel_directive ctx a in
        Ptop_dir (Stdlib.fst a), self#constr ctx "Ptop_dir" [ Stdlib.snd a ]
      | Ptop_lex a ->
        let a = self#lexer_directive ctx a in
        Ptop_lex (Stdlib.fst a), self#constr ctx "Ptop_lex" [ Stdlib.snd a ]

    method toplevel_directive
      : 'ctx -> toplevel_directive -> toplevel_directive * 'res
      =
      fun ctx { pdir_name; pdir_arg; pdir_loc; pdir_tokens } ->
      let pdir_name = self#loc self#string ctx pdir_name in
      let pdir_arg = self#option self#directive_argument ctx pdir_arg in
      let pdir_loc = self#location ctx pdir_loc in
      let pdir_tokens = self#token_seq ctx pdir_tokens in
      ( { pdir_name = Stdlib.fst pdir_name
        ; pdir_arg = Stdlib.fst pdir_arg
        ; pdir_loc = Stdlib.fst pdir_loc
        ; pdir_tokens = Stdlib.fst pdir_tokens
        }
      , self
        #
        record
          ctx
          [ "pdir_name", Stdlib.snd pdir_name
          ; "pdir_arg", Stdlib.snd pdir_arg
          ; "pdir_loc", Stdlib.snd pdir_loc
          ; "pdir_tokens", Stdlib.snd pdir_tokens
          ] )

    method directive_argument
      : 'ctx -> directive_argument -> directive_argument * 'res
      =
      fun ctx { pdira_desc; pdira_loc } ->
      let pdira_desc = self#directive_argument_desc ctx pdira_desc in
      let pdira_loc = self#location ctx pdira_loc in
      ( { pdira_desc = Stdlib.fst pdira_desc; pdira_loc = Stdlib.fst pdira_loc }
      , self
        #
        record
          ctx
          [ "pdira_desc", Stdlib.snd pdira_desc
          ; "pdira_loc", Stdlib.snd pdira_loc
          ] )

    method directive_argument_desc
      : 'ctx -> directive_argument_desc -> directive_argument_desc * 'res
      =
      fun ctx x ->
      match x with
      | Pdir_string a ->
        let a = self#string ctx a in
        ( Pdir_string (Stdlib.fst a)
        , self#constr ctx "Pdir_string" [ Stdlib.snd a ] )
      | Pdir_int (a, b) ->
        let a = self#string ctx a in
        let b = self#option self#char ctx b in
        ( Pdir_int (Stdlib.fst a, Stdlib.fst b)
        , self#constr ctx "Pdir_int" [ Stdlib.snd a; Stdlib.snd b ] )
      | Pdir_ident a ->
        let a = self#longident ctx a in
        ( Pdir_ident (Stdlib.fst a)
        , self#constr ctx "Pdir_ident" [ Stdlib.snd a ] )
      | Pdir_bool a ->
        let a = self#bool ctx a in
        Pdir_bool (Stdlib.fst a), self#constr ctx "Pdir_bool" [ Stdlib.snd a ]

    method syntax_directive
      : 'ctx -> syntax_directive -> syntax_directive * 'res
      =
      fun ctx { psyn_mode; psyn_toggle } ->
      let psyn_mode = self#loc self#string ctx psyn_mode in
      let psyn_toggle = self#bool ctx psyn_toggle in
      ( { psyn_mode = Stdlib.fst psyn_mode
        ; psyn_toggle = Stdlib.fst psyn_toggle
        }
      , self
        #
        record
          ctx
          [ "psyn_mode", Stdlib.snd psyn_mode
          ; "psyn_toggle", Stdlib.snd psyn_toggle
          ] )

    method lexer_directive_desc
      : 'ctx -> lexer_directive_desc -> lexer_directive_desc * 'res
      =
      fun ctx x ->
      match x with
      | Plex_syntax a ->
        let a = self#syntax_directive ctx a in
        ( Plex_syntax (Stdlib.fst a)
        , self#constr ctx "Plex_syntax" [ Stdlib.snd a ] )

    method lexer_directive : 'ctx -> lexer_directive -> lexer_directive * 'res =
      fun ctx { plex_desc; plex_loc; plex_tokens } ->
      let plex_desc = self#lexer_directive_desc ctx plex_desc in
      let plex_loc = self#location ctx plex_loc in
      let plex_tokens = self#token_seq ctx plex_tokens in
      ( { plex_desc = Stdlib.fst plex_desc
        ; plex_loc = Stdlib.fst plex_loc
        ; plex_tokens = Stdlib.fst plex_tokens
        }
      , self
        #
        record
          ctx
          [ "plex_desc", Stdlib.snd plex_desc
          ; "plex_loc", Stdlib.snd plex_loc
          ; "plex_tokens", Stdlib.snd plex_tokens
          ] )
  end

[@@@end]
