open Ocaml_syntax
open Parsetree

let get_infix_op exp =
  match exp.Parsetree.pexp_desc with
  | Pexp_ident { txt = { desc = Lident Op s; _ }; _ } -> s
  | _ -> assert false

(* Pretty-printing precedence. ==================================================

   Three cooperating notions drive the parenthesiser:

   1. [Level] defines a totally-ordered scale of precedences from loosest to
      tightest:
        mandatory  = -1  (parens are mandatory around a node at this level)
        free       = 23  (parens are never needed; the node is never ambiguous)
      The names that line up 1:1 with a token in parser.mly's %nonassoc/%left/
      %right table keep that token's name (the infix-operator ladder, COMMA,
      COLONCOLON, unary +/-, pattern as/|).
      The rest (function application, field access, atoms, attributed nodes,
      ...) use local lowercase_underscore names.
      Several may share one integer because they belong to unrelated grammar
      categories that are never compared against each other.

   2. [Expr.get_prec] / [Pattern.get_prec] / [Type.get_prec] return the
      tightest level at which a node [n] is still unambiguous when printed bare
      (i.e. without surrounding parens).
      This is an *upper bound* on the set of "safe" required levels:

      get_prec n = p   =>   n needs no parens whenever the context requires ≤ p

   3. [simp_expr] / [simp_pat] / [simp_type] are top-down traversal that threads
      a required level (the lower bound: "the child must be unambiguous at
      *at least* this tightness") into each child position.
      After simplifying a child, the invariant is checked:

          required_level ≤ get_prec child

      When it holds, parentheses can be removed, the child is printed bare.
      When it fails the parens are preserved around that child.
      The parent's own level is passed to its children, possibly adjusted, so
      that the check is local to each edge of the tree.

   In short:
   - get_prec answers "how loose can *I* be?"
   - simp_* checks if each child is "tight enough" to occur bare there, and
     removes parens exactly when the answer is no.
 *)
module Level : sig
  type level = private int

  val get_stricter : level -> level -> level
  (* [is_compatible child ~parent] is true iff a node whose outward prec is
     [child] needs no parens in a context that requires [parent]. *)
  val is_compatible : level -> parent:level -> bool

  (* Absolute levels, loosest → tightest.  Names that match a parser.mly token
     keep that spelling; the rest are local. *)
  val mandatory    : level  (* always needs parens *)
  val free         : level  (* unconstrained tail position: let body, match/try scrutinee, arrow codomain *)
  val below_semi   : level  (* below_SEMI: dangling-construct cap, bare-arg constructor prec, or-pattern / arrow-domain threshold *)
  val comma        : level  (* COMMA: pattern/type tuple outward prec *)
  val tuple_elt    : level  (* tuple-element threshold, one tighter than [comma] *)
  val cons_tail    : level  (* COLONCOLON tail threshold *)
  val cons         : level  (* COLONCOLON outward prec *)
  val cons_head    : level  (* COLONCOLON head threshold, stricter than [cons] (right-assoc) *)
  val unary_sign   : level  (* prec_unary_minus / prec_unary_plus *)
  val attributed   : level  (* node carries [@attr]; forced here regardless of shape *)
  val apply        : level  (* application / method-send / prefix-keyword outward prec *)
  val apply_arg    : level  (* threshold required of an application / constructor / prefix-keyword argument *)
  val field_access : level  (* '.'-style access outward prec *)
  val lazy_arg     : level  (* [lazy] / [exception] pattern argument threshold (same int as [field_access], unrelated) *)
  val prefix_op    : level  (* unary prefix operator outward prec *)
  val atom         : level  (* self-delimited leaves *)
  val above_atom   : level  (* stricter than any atom can satisfy *)

  (* Infix-operator groups, mirroring parser.mly's %left/%right table.
     Each pair is (lhs_threshold, rhs_threshold):
       %left  → (prec, prec+1)   rhs rejects a bare same-level chain
       %right → (prec+1, prec)   lhs rejects it
   *)
  val pow_op    : level * level  (* INFIXOP4 / ** lsl lsr asr *)
  val and_op    : level * level  (* AMPERSAND / AMPERAMPER *)
  val or_op     : level * level  (* OR / BARBAR *)
  val assign_op : level * level  (* LESSMINUS / COLONEQUAL, approximated as one group *)
  val mul_op    : level * level  (* PERCENT / INFIXOP3 / MOD / STAR *)
  val plus_op   : level * level  (* INFIXOP2 / PLUS / MINUS / PLUSDOT / MINUSDOT / PLUSEQ *)
  val at_op     : level * level  (* AT / ATAT / INFIXOP1 *)
  val equal_op  : level * level  (* INFIXOP0 / EQUAL / LESS / GREATER *)
end = struct
  type level = int

  let get_stricter    = Int.min
  let is_compatible child ~parent = (child : int) >= (parent : int)

  let mandatory    = -1
  let free         =  0
  let below_semi   =  1
  let comma        =  3
  let tuple_elt    =  4
  let cons_tail    = 10
  let cons         = 11
  let cons_head    = 12
  let unary_sign   = 16
  let attributed   = 17
  let apply        = 18
  let apply_arg    = 19
  let field_access = 20
  let lazy_arg     = 20
  let prefix_op    = 21
  let atom         = 22
  let above_atom   = 23

  let left_op  prec = (prec, prec + 1)
  let right_op prec = (prec + 1, prec)

  let pow_op    = right_op 14
  let and_op    = right_op  6
  let or_op     = right_op  4
  let assign_op = right_op  1
  let mul_op    = left_op  13
  let plus_op   = left_op  11
  let at_op     = right_op  8
  let equal_op  = left_op   7
end
open Level

(* Patterns. ================================================================

   Precedence scale (loosest to tightest):

     0   Ppat_alias, Ppat_or
     3   Ppat_tuple
     11  Ppat_cons
     18  Ppat_lazy, Ppat_exception
     19  Ppat_construct (_, Some _), Ppat_variant (_, Some _)
     22  everything self-delimited

   [Ppat_alias] has no single correct level: it binds tighter than [,] and
   [::]-head (parens there are redundant) but looser than [|], [::]-tail,
   and constructor arguments (parens there are necessary). It is
   conservatively treated as [free] everywhere, keeping a few redundant
   parens rather than risk a missing one.
*)
module Pattern = struct

  let parens_pat = Pattern.parens_pat

  let get_prec (p : pattern) : level =
    match p.ppat_attributes with
    | Attributes _ -> attributed
    | No_attributes ->
      match p.ppat_desc with
      | Ppat_any | Ppat_var _ | Ppat_constant _ | Ppat_interval _
      | Ppat_unboxed_unit | Ppat_unboxed_bool _ | Ppat_unboxed_tuple _
      | Ppat_record _ | Ppat_record_unboxed_product _
      | Ppat_array _ | Ppat_list _
      | Ppat_construct (_, None) | Ppat_variant (_, None)
      | Ppat_type _ | Ppat_extension _
      | Ppat_open _ | Ppat_parens _
      | Ppat_constraint (_, _, _)
        -> atom
      | Ppat_construct (_, Some _) | Ppat_variant (_, Some _)
        -> apply_arg
      | Ppat_lazy _ | Ppat_exception _
        -> apply
      | Ppat_cons _ -> cons
      | Ppat_tuple _ -> comma
      | Ppat_alias _ | Ppat_or _ -> free
      | Ppat_unpack (_, _) | Ppat_effect (_, _) -> mandatory

  (* Pattern-simplifier *)
  let rec simp_pat
      super (self : 'a Traversals.map_with_context) (ctx : 'a)
      (prec : level) (p : pattern) : pattern
    =
    let return ppat_desc = {p with ppat_desc} in
    let simp_pat prec' pat' = simp_pat super self ctx prec' pat' in
    match p.ppat_desc with
    | Ppat_parens {pat; optional} ->
      let pat = simp_pat free pat in
      let skip_parens =
        match p.ppat_attributes, pat.ppat_attributes with
        | No_attributes, No_attributes -> is_compatible (get_prec pat) ~parent:prec
        | Attributes _, _ | _, Attributes _ -> false
      in
      if skip_parens then pat else return (Ppat_parens {pat; optional})

    | Ppat_alias (pat, name) ->
      let pat = simp_pat free pat in
      return (Ppat_alias (pat, name))

    | Ppat_or (p1, p2) ->
      let p1 = simp_pat below_semi p1 in
      let p2 = simp_pat below_semi p2 in
      return (Ppat_or (p1, p2))

    | Ppat_cons (p1, p2) ->
      (* Right-associative, like [Pexp_cons]:
         ([(a :: b) :: c] is different from [a :: b :: c]).
         The head must reject a bare cons
         The tail may accept one. *)
      let p1 = simp_pat cons_head p1 in
      let p2 = simp_pat cons_tail p2 in
      return (Ppat_cons (p1, p2))

    | Ppat_tuple (args, closed) ->
      let args = List.map (simp_pat_arg super self ctx tuple_elt) args in
      return (Ppat_tuple (args, closed))

    | Ppat_unboxed_tuple (args, closed) ->
      let args = List.map (simp_pat_arg super self ctx tuple_elt) args in
      return (Ppat_unboxed_tuple (args, closed))

    | Ppat_construct (lid, Some (vars, pat)) ->
      let pat = simp_pat apply pat in
      return (Ppat_construct (lid, Some (vars, pat)))

    | Ppat_variant (lbl, Some pat) ->
      let pat = simp_pat apply pat in
      return (Ppat_variant (lbl, Some pat))

    | Ppat_lazy pat ->
      (* Stricter than [Ppat_construct]: `lazy A n` is a parse error where
         `S S N` is not, so the threshold must be above [apply_arg]. *)
      let pat = simp_pat lazy_arg pat in
      return (Ppat_lazy pat)

    | Ppat_exception pat ->
      let pat = simp_pat lazy_arg pat in
      return (Ppat_exception pat)

    | Ppat_open (loc, pat) ->
      let pat = simp_pat above_atom pat in
      return (Ppat_open (loc, pat))

    (* Default: everything self-delimited, and maybe some things that might be
       handled later (safe, just incomplete). *)
    | Ppat_any | Ppat_var _ | Ppat_constant _ | Ppat_interval _
    | Ppat_unboxed_unit | Ppat_unboxed_bool _
    | Ppat_construct (_, None) | Ppat_variant (_, None)
    | Ppat_record _ | Ppat_record_unboxed_product _
    | Ppat_array _ | Ppat_list _
    | Ppat_type _ | Ppat_unpack _ | Ppat_extension _
    | Ppat_constraint _
    | Ppat_effect _
      -> super ctx p

  and simp_pat_arg
      super (self : 'a Traversals.map_with_context) (ctx : 'a)
      prec (x : pattern argument) : pattern argument =
    match x.parg_desc with
    | Parg_unlabelled ({ arg; typ_constraint = None;
                         legacy_modes = No_modes;
                         modes = No_modes; } as parg) ->
      let arg = simp_pat super self ctx prec arg in
      {x with parg_desc = Parg_unlabelled {parg with arg}}
    | Parg_labelled ({ maybe_punned = Some arg; typ_constraint = None;
                       legacy_modes = No_modes;
                       modes = No_modes;
                       default = None; _ } as parg) ->
      let arg = simp_pat super self ctx prec arg in
      {x with parg_desc = Parg_labelled {parg with maybe_punned = Some arg}}
    | Parg_unlabelled _
    | Parg_labelled _ ->
      self#argument self#pattern ctx x
  (* TODO: parentheses around an argument are inlined in the token stream, with no
     explicit representation in the CST.
     This traversal should be refined to account for them and remove them
     when they are proven unnecessary. *)

  let simplify_pat super self ctx p =
    simp_pat super self ctx free p

  (* A function-parameter is a pattern with a strict threshold ([atom]):
     [let swap (a, b) = ...] and [let force_int (lazy n) = ...]
     cannot drop their parens.

     Curried parameters appear in [Pexp_function]'s param list and in
     [value_binding.pvb_params]. *)
  let simp_param super self ctx : function_param -> function_param =
    fun (fp : function_param) ->
      match fp.pparam_desc with
      | Pparam_val arg ->
        let has_parens =
          Utils.token_exists arg.parg_tokens
            ~f:(function LPAREN | RPAREN -> true | _ -> false)
        in
        let level = if has_parens then free else atom in
        { fp with pparam_desc = Pparam_val (simp_pat_arg super self ctx level arg) }
      | Pparam_newtype _ | Pparam_newtypes _ -> fp

end

(* Core types. ==============================================================

   Precedence scale (loosest to tightest):

     0   Ptyp_arrow
     3   Ptyp_tuple
     19  Ptyp_constr / Ptyp_class WITH arguments
     22  everything self-delimited

   Tuple types are flat/N-ary (same as expressions and patterns).
   [Ptyp_tuple]'s low outward prec means it is never stripped generically;
   it is only accepted where a low threshold is already correct (arrow
   domain, [as]).
   Type-constructor application chains left-to-right without parens
   (['a option list] = [('a option) list]).
*)
module Type = struct


  let parens_typ ?(optional=false) typ =
    { ptyp_desc = Ptyp_parens typ
    ; ptyp_tokens = Utils.lparen_child_rparen ~optional typ.ptyp_loc.loc_start
    ; ptyp_loc = typ.ptyp_loc
    ; ptyp_attributes = No_attributes }

  let get_prec (t : core_type) : level =
    match t.ptyp_attributes with
    | Attributes _ -> attributed
    | No_attributes ->
      match t.ptyp_desc with
      | Ptyp_any _ | Ptyp_var _
      | Ptyp_constr ([], _) | Ptyp_class ([], _)
      | Ptyp_object _ | Ptyp_package _ | Ptyp_variant _
      | Ptyp_extension _ | Ptyp_of_kind _ | Ptyp_parens _ | Ptyp_unboxed_tuple _ ->
        atom
      | Ptyp_constr (_ :: _, _) | Ptyp_class (_ :: _, _) ->
        apply_arg
      | Ptyp_tuple _ -> comma
      | Ptyp_arrow _ -> free
      (* Safe approximation, always keep parentheses. *)
      | Ptyp_alias (_, _, _) | Ptyp_poly (_, _) | Ptyp_newlayout (_, _)
      | Ptyp_open (_, _) | Ptyp_quote _ | Ptyp_splice _ | Ptyp_repr (_, _)
        -> mandatory

  let rec simp_typ super ctx (prec : level) (t : core_type) : core_type =
    let simp_typ prec' t' = simp_typ super ctx prec' t' in
    let return ptyp_desc = {t with ptyp_desc} in
    match t.ptyp_desc with
    | Ptyp_parens tt ->
      let tt = simp_typ free tt in
      let child_prec = get_prec tt in
      let skip_parens =
        match t.ptyp_attributes with
        | No_attributes -> is_compatible child_prec ~parent:prec
        | Attributes _ -> false
      in
      if skip_parens then tt else return (Ptyp_parens tt)

    | Ptyp_arrow { domain; codom_legacy_modes; codom_type; codom_modes } ->
      (* -> right-assoc: domain rejects bare arrows, codomain accepts them *)
      let domain = { domain with aa_type = simp_typ below_semi domain.aa_type } in
      let codom_type = simp_typ free codom_type in
      return (Ptyp_arrow { domain; codom_legacy_modes; codom_type; codom_modes })

    | Ptyp_tuple args ->
      let args = List.map (fun (l, tt) -> (l, simp_typ tuple_elt tt)) args in
      return (Ptyp_tuple args)

    | Ptyp_unboxed_tuple args ->
      let args = List.map (fun (l, tt) -> (l, simp_typ tuple_elt tt)) args in
      return (Ptyp_unboxed_tuple args)

    | Ptyp_constr (tl, lid) ->
      let tl = List.map (simp_typ apply_arg) tl in
      return (Ptyp_constr (tl, lid))

    | Ptyp_class (tl, lid) ->
      let tl = List.map (simp_typ apply_arg) tl in
      return (Ptyp_class (tl, lid))

    | Ptyp_alias (tt, name, jkind) ->
      (* like [Ppat_alias], [as] grabs whatever type is fully to its left *)
      let tt = simp_typ free tt in
      return (Ptyp_alias (tt, name, jkind))

    (* Safe approximation *)
    | Ptyp_any _ | Ptyp_var _
    | Ptyp_object _ | Ptyp_package _ | Ptyp_variant _
    | Ptyp_extension _ | Ptyp_of_kind _ | Ptyp_poly _ | Ptyp_newlayout _
    | Ptyp_open _ | Ptyp_quote _ | Ptyp_splice _ | Ptyp_repr _
      -> super ctx t

  let simplify_typ (type a) super (ctx : a) t : core_type =
    simp_typ super ctx free t
end

module Expr = struct
  let get_op_class (op : string) : level * level =
    match op with
    | "" -> assert false

    (* 1. Specific static strings (Overriding character-based rules) *)
    | "**" | "lsl" | "lsr" | "asr" -> pow_op
    | "&"  | "&&" -> and_op
    | "||" | "or" -> or_op
    | "<-" | ":=" -> assign_op
    | ";" -> (free, mandatory)
    | "mod" | "land" | "lor" | "lxor" -> mul_op
    | "," -> (comma, comma)

    (* 2. Generalized match on the first character *)
    | _ ->
      match op.[0] with
      | '*' | '/' | '%' -> mul_op
      | '+' | '-' -> plus_op
      | '@' | '^' -> at_op
      | '=' | '<' | '>' | '|' | '&' | '$' | '!' -> equal_op
      (* 3. Fallback for empty strings or unknown operators *)
      | _ -> (above_atom, above_atom)

  let rec get_prec (e : expression) : level =
    match e.pexp_attributes with
    | Attributes _ -> attributed
    | No_attributes ->
      match e.pexp_desc with
      (* Atoms / Leaves: High precedence to avoid wrapping in parentheses *)
      | Pexp_ident _ | Pexp_constant _ | Pexp_new _ | Pexp_array _
      | Pexp_unboxed_unit | Pexp_unboxed_bool _ | Pexp_unboxed_tuple _
      | Pexp_record _ | Pexp_record_unboxed_product _
      | Pexp_while _ | Pexp_for _
      | Pexp_extension _
      | Pexp_unreachable
      | Pexp_begin_end _
      | Pexp_quote _
      | Pexp_hole
      | Pexp_list _
      | Pexp_comprehension _
      | Pexp_parens _
        -> atom

      (* Unary Prefix Operators *)
      | Pexp_prefix_apply _ -> prefix_op

      (* Field/Array Access and Application *)
      | Pexp_apply _
      | Pexp_mode_legacy _
      | Pexp_assert _ | Pexp_lazy _ | Pexp_stack _
      | Pexp_splice _ | Pexp_exclave _ | Pexp_borrow _
        -> apply
      | Pexp_field _ | Pexp_unboxed_field _ | Pexp_idx _
      | Pexp_index_op _ | Pexp_pack _ -> field_access

      (* Method Send *)
      | Pexp_send _ -> apply

      (* Unary Add/Sub *)
      | Pexp_add_or_sub _ -> unary_sign

      (* Infix Operators *)
      | Pexp_infix_apply {op; arg2; _} ->
        let (pl, pr) = get_op_class (get_infix_op op) in
        get_stricter (get_prec arg2) (get_stricter pl pr)

      (* Cons Operator *)
      | Pexp_cons _ -> cons

      (* Assignment Operators *)
      | Pexp_setvar _ | Pexp_setfield _ -> fst assign_op
      | Pexp_overwrite _ -> free

      (* Constructors and Variants *)
      | Pexp_construct (_, Some _) | Pexp_variant (_, Some _) -> below_semi
      | Pexp_construct (_, None) | Pexp_variant (_, None) -> atom

      (* Dangling constructs (if/;, let/match/try/function/local-module/...)
         extend as far right as possible; their outward prec is capped at
         [below_semi] and further limited by their trailing sub-expression.
         This is safe in "any expression" positions but NOT as an
         application argument or infix left operand — those are handled by
         [rightmost] in [simp_expr]. *)
      | Pexp_ifthenelse (_, e, None) | Pexp_ifthenelse (_, _, Some e)
      | Pexp_sequence (_, e) ->
        get_stricter (get_prec e) below_semi
      | Pexp_seq_empty _ -> below_semi

      | Pexp_let (_, _, _, body)
      | Pexp_letmodule (_, body)
      | Pexp_letexception (_, body)
      | Pexp_let_open (_, body)
      | Pexp_letop { body; _ }
      | Pexp_function (_, _, { pfb_desc = Pfunction_body body; _ }) ->
        get_stricter (get_prec body) below_semi

      | Pexp_match (_, cases)
      | Pexp_try (_, cases)
      | Pexp_function (_, _, { pfb_desc = Pfunction_cases (_, cases); _ }) ->
        get_stricter (get_prec (Std.List.last cases).pc_rhs) below_semi

      (* Tuples: a bare [a, b] often means something different from [(a, b)].
         It is always safe to keep parens.
         Jane Street-style favors parenthesizing tuples on multiple lines
         Since we don't know yet if tuple will get broken over multiple line,
         we always insert parentheses. They might get removed after layout. *)
      | Pexp_tuple _ -> mandatory

      (* Safe approximation: always keep parentheses *)
      | Pexp_constraint (_, _, _) | Pexp_coerce (_, _, _) | Pexp_override _
      | Pexp_object _ | Pexp_dot_open (_, _) -> mandatory

  (* Would a bare `| pat -> ...` immediately following [e] be swallowed as
     another case of e's innermost match/try/function?
     Non-delimited right-extending constructions inherit the danger from their tail:
     let/in, if/then/else, unary/binary operators, fun x -> body, ... *)
  let rec ends_in_bare_cases (e : expression) : bool =
    match e.pexp_desc with
    | Pexp_match _ | Pexp_try _ -> true
    | Pexp_function (_, _, { pfb_desc = Pfunction_cases _; _ }) -> true

    | Pexp_function (_, _, { pfb_desc = Pfunction_body body; _ })
    | Pexp_let (_, _, _, body)
    | Pexp_letmodule (_, body)
    | Pexp_letexception (_, body)
    | Pexp_let_open (_, body)
    | Pexp_letop { body; _ } ->
      ends_in_bare_cases body

    | Pexp_ifthenelse (_, e, None)
    | Pexp_ifthenelse (_, _, Some e)
    | Pexp_sequence (_, e) -> ends_in_bare_cases e
    | Pexp_infix_apply { arg2; _ } -> ends_in_bare_cases arg2
    | Pexp_cons (_, exp2) -> ends_in_bare_cases exp2
    | Pexp_add_or_sub (_, exp) -> ends_in_bare_cases exp
    | _ -> false

  (* Would [e], printed bare, swallow the following [;]
     as part of its own body?
     [if] is exempt: [if b then 1 else 2; z] is [(if ...); z]. *)
  let rec swallows_semicolon (e : expression) : bool =
    match e.pexp_desc with
    | Pexp_let _ | Pexp_letmodule _ | Pexp_letexception _
    | Pexp_let_open _ | Pexp_letop _
    | Pexp_match _ | Pexp_try _ | Pexp_function _
    | Pexp_sequence _ -> true
    | Pexp_infix_apply { arg2; _ } -> swallows_semicolon arg2
    | Pexp_cons (_, exp2) -> swallows_semicolon exp2
    | Pexp_add_or_sub (_, exp) -> swallows_semicolon exp
    | _ -> false

  (* Does [e] resolve to a bare [;] sequence?
     (possibly through a trailing position).
     Useful because [;] binds looser than [:=]/[<-]:
       [r := a; b] is [(r := a); b]. *)
  let rec is_bare_sequence (e : expression) : bool =
    match e.pexp_desc with
    | Pexp_sequence _ -> true
    | Pexp_infix_apply { arg2; _ } -> is_bare_sequence arg2
    | Pexp_cons (_, exp2) -> is_bare_sequence exp2
    | Pexp_add_or_sub (_, exp) -> is_bare_sequence exp
    | _ -> false

  (* Is nothing textually following this position that a dangling construct
     could swallow? [is_dangling_tail] answers this for a specific [e],
     matching [get_prec]'s [below_semi] cap but EXCLUDING bare sequences
     (handled separately by [is_bare_sequence]).
     [Rightmost_no_fun_cases]: a bare `function | ...` is a hard parse error
     as unary +/-'s operand, unlike every other rightmost position. *)
  type rightmost = Not_rightmost | Rightmost | Rightmost_no_fun_cases

  let no_fun_cases = function
    | Not_rightmost -> Not_rightmost
    | Rightmost | Rightmost_no_fun_cases -> Rightmost_no_fun_cases

  let is_dangling_tail (pos : rightmost) (e : expression) : bool =
    pos <> Not_rightmost &&
    match e.pexp_attributes with
    | Attributes _ -> false
    | No_attributes ->
      match e.pexp_desc with
      | Pexp_let _ | Pexp_letmodule _ | Pexp_letexception _
      | Pexp_let_open _ | Pexp_letop _
      | Pexp_match _ | Pexp_try _
      | Pexp_ifthenelse _ -> true
      | Pexp_function (_, _, { pfb_desc = Pfunction_cases _; _ }) ->
        pos <> Rightmost_no_fun_cases
      | Pexp_function (_, _, { pfb_desc = Pfunction_body _; _ }) -> true
      | _ -> false

  let simplify (type a) super super_pat (self : a Traversals.map_with_context)
      (ctx : a) ?(rightmost = Not_rightmost) e =
    (* [rightmost] must be PROPAGATED, never hardcoded: a node is only truly
       rightmost if every step from where it was first established down to
       that node was tail-preserving. *)
    let rec simp_expr
        ?(allow_bare_cases = true)
        ?(allow_semicolon_swallowers = true)
        ?(allow_bare_sequence = true)
        ?(rightmost = Not_rightmost)
        (prec : level) e
      : expression =
      let return pexp_desc = {e with pexp_desc} in
      match e.pexp_desc with
      | Pexp_parens {exp; optional} ->
        let exp =
          simp_expr ~allow_bare_cases ~allow_semicolon_swallowers
            ~allow_bare_sequence ~rightmost free exp
        in
        let child_prec = get_prec exp in
        let skip_parens =
          match e.pexp_attributes with
          | No_attributes ->
            (is_compatible child_prec ~parent:prec
             || is_dangling_tail rightmost exp)
            && (allow_bare_cases || not (ends_in_bare_cases exp))
            && (allow_semicolon_swallowers || not (swallows_semicolon exp))
            && (allow_bare_sequence || not (is_bare_sequence exp))
          | Attributes _ -> false
        in
        if skip_parens then
          exp
        else
          return (Pexp_parens {exp; optional})

      | Pexp_let (mutf, recf, bindings, body) ->
        let bindings = self#list self#value_binding ctx bindings in
        let body = simp_expr ~allow_bare_cases ~rightmost free body in
        return (Pexp_let (mutf, recf, bindings, body))

      | Pexp_function (params, modes, body) ->
        let params = List.map (Pattern.simp_param super_pat self ctx) params in
        let body = match body.pfb_desc with
          | Pfunction_body exp ->
            let exp = simp_expr ~allow_bare_cases ~rightmost free exp in
            {body with pfb_desc = Pfunction_body exp}
          | Pfunction_cases (attr, cases) ->
            let cases = simp_cases ~rightmost prec cases in
            {body with pfb_desc = Pfunction_cases (attr, cases)}
        in
        return (Pexp_function (params, modes, body))

      | Pexp_prefix_apply (op, exp) ->
        let exp = simp_expr prefix_op exp in
        return (Pexp_prefix_apply (op, exp))

      | Pexp_add_or_sub (op, exp) ->
        (* Unary +/- operand is rightmost (minus bare `function`),
           never a bare `;` sequence: `- a; b` is `(- a); b`. *)
        let exp =
          simp_expr ~allow_bare_cases ~allow_semicolon_swallowers
            ~allow_bare_sequence:false ~rightmost:(no_fun_cases rightmost)
            unary_sign exp
        in
        return (Pexp_add_or_sub (op, exp))

      | Pexp_infix_apply {arg1; op; arg2} ->
        let (pl, pr) = get_op_class (get_infix_op op) in
        let arg1 = simp_expr pl arg1 in
        (* arg2 is rightmost, never a bare `;` sequence. *)
        let arg2 =
          simp_expr ~allow_bare_cases ~allow_semicolon_swallowers
            ~allow_bare_sequence:false ~rightmost pr arg2
        in
        return (Pexp_infix_apply {arg1; op; arg2})

      | Pexp_apply (exp, args) ->
        let exp = simp_expr apply exp in
        let args = List.map (simp_arg apply_arg) args in
        return (Pexp_apply (exp, args))

      | Pexp_match (exp, cases) ->
        let exp = simp_expr free exp in
        let cases = simp_cases ~rightmost prec cases in
        return (Pexp_match (exp, cases))

      | Pexp_try (exp, cases) ->
        let exp = simp_expr free exp in
        let cases = simp_cases ~rightmost prec cases in
        return (Pexp_try (exp, cases))

      | Pexp_overwrite (exp1, exp) ->
        let exp1 = simp_expr free exp1 in
        let exp = simp_expr free exp in
        return (Pexp_overwrite (exp1, exp))

      | Pexp_tuple args ->
        (* Only the LAST element is rightmost; never a bare `;` sequence
           (`a, b; c` is `(a, b); c`). *)
        let n = List.length args in
        let args =
          List.mapi
            (fun i a ->
               if i = n - 1
               then
                 simp_arg ~allow_bare_cases ~allow_semicolon_swallowers
                   ~allow_bare_sequence:false ~rightmost tuple_elt a
               else simp_arg tuple_elt a)
            args
        in
        return (Pexp_tuple args)

      | Pexp_construct (name, Some exp) ->
        let exp = simp_expr apply_arg exp in
        return (Pexp_construct (name, Some exp))

      | Pexp_variant (name, Some exp) ->
        let exp = simp_expr apply_arg exp in
        return (Pexp_variant (name, Some exp))

      | Pexp_field (exp, path) ->
        let exp = simp_expr field_access exp in
        return (Pexp_field (exp, path))

      | Pexp_unboxed_field (exp, path) ->
        let exp = simp_expr field_access exp in
        return (Pexp_unboxed_field (exp, path))

      | Pexp_setfield (exp, path, arg) ->
        let exp = simp_expr (fst assign_op) exp in
        let arg = simp_expr ~allow_bare_sequence:false free arg in
        return (Pexp_setfield (exp, path, arg))

      | Pexp_ifthenelse (cond, ethen, None) ->
        let cond = self#expression ctx cond in
        let ethen = simp_expr ~allow_bare_cases ~rightmost (fst assign_op) ethen in
        return (Pexp_ifthenelse (cond, ethen, None))

      | Pexp_ifthenelse (cond, ethen, Some eelse) ->
        let cond = self#expression ctx cond in
        let ethen = simp_expr (fst assign_op) ethen in
        let eelse = simp_expr ~allow_bare_cases ~rightmost (fst assign_op) eelse in
        return (Pexp_ifthenelse (cond, ethen, Some eelse))

      | Pexp_sequence (e1, e2) ->
        let e1 = simp_expr ~allow_semicolon_swallowers:false below_semi e1 in
        let e2 = simp_expr ~allow_bare_cases ~rightmost below_semi e2 in
        return (Pexp_sequence (e1, e2))

      | Pexp_send (exp, mtd) ->
        let exp = simp_expr apply exp in
        return (Pexp_send (exp, mtd))

      | Pexp_setvar (var, exp) ->
        let exp = simp_expr ~allow_bare_sequence:false free exp in
        return (Pexp_setvar (var, exp))

      | Pexp_letmodule (mb, exp) ->
        let mb = self#module_binding ctx mb in
        let exp = simp_expr ~allow_bare_cases ~rightmost free exp in
        return (Pexp_letmodule (mb, exp))

      | Pexp_letexception (ext, exp) ->
        let exn = self#extension_constructor ctx ext in
        let exp = simp_expr ~allow_bare_cases ~rightmost free exp in
        return (Pexp_letexception (exn, exp))

      | Pexp_seq_empty exp ->
        let exp = simp_expr free exp in
        return (Pexp_seq_empty exp)

      | Pexp_let_open (mo, exp) ->
        let mo = self#open_infos self#module_expr ctx mo in
        let exp = simp_expr ~allow_bare_cases ~rightmost free exp in
        return (Pexp_let_open (mo, exp))

      | Pexp_letop {let_; ands; body} ->
        let let_ = self#binding_op ctx let_ in
        let ands = self#list self#binding_op ctx ands in
        let body = simp_expr ~allow_bare_cases ~rightmost free body in
        return (Pexp_letop {let_; ands; body})

      | Pexp_index_op iop ->
        let seq = simp_expr field_access iop.seq in
        let indices = List.map (simp_expr free) iop.indices in
        let assign =
          Option.map (simp_expr ~allow_bare_sequence:false free) iop.assign
        in
        return (Pexp_index_op {iop with seq; indices; assign})

      | Pexp_cons (exp1, exp2) ->
        (* Right-assoc: head must reject a bare cons; tail accepts one.
           Tail is rightmost, never a bare `;` sequence. *)
        let exp1 = simp_expr cons_head exp1 in
        let exp2 =
          simp_expr ~allow_bare_cases ~allow_semicolon_swallowers
            ~allow_bare_sequence:false ~rightmost cons_tail exp2
        in
        return (Pexp_cons (exp1, exp2))

      (* Prefixed keywords *)
      | Pexp_mode_legacy (mode, exp) ->
        let exp = simp_expr free exp in
        return (Pexp_mode_legacy (mode, exp))

      | Pexp_assert exp ->
        let exp = simp_expr apply_arg exp in
        return (Pexp_assert exp)

      | Pexp_lazy exp ->
        let exp = simp_expr apply_arg exp in
        return (Pexp_lazy exp)

      | Pexp_stack exp ->
        let exp = simp_expr apply_arg exp in
        return (Pexp_stack exp)

      | Pexp_splice exp ->
        let exp = simp_expr apply_arg exp in
        return (Pexp_splice exp)

      | Pexp_exclave exp ->
        let exp = simp_expr apply_arg exp in
        return (Pexp_exclave exp)

      | Pexp_borrow exp ->
        let exp = simp_expr apply_arg exp in
        return (Pexp_borrow exp)

      | Pexp_dot_open (path, exp) ->
        let exp = simp_expr above_atom exp in
        return (Pexp_dot_open (path, exp))

      | Pexp_record (base, fields) ->
        let base = Option.map (simp_expr above_atom) base in
        let fields = simp_record_fields fields in
        return (Pexp_record (base, fields))

      | Pexp_record_unboxed_product (base, fields) ->
        let base = Option.map (simp_expr above_atom) base in
        let fields = simp_record_fields fields in
        return (Pexp_record_unboxed_product (base, fields))

      | Pexp_array (mutf, elts) ->
        let elts = List.map (simp_expr ~allow_semicolon_swallowers:false free) elts in
        return (Pexp_array (mutf, elts))

      | Pexp_list elts ->
        let elts = List.map (simp_expr ~allow_semicolon_swallowers:false free) elts in
        return (Pexp_list elts)

      | Pexp_construct (_, None)
      | Pexp_variant (_, None)
      | Pexp_new _
      | Pexp_ident _ | Pexp_constant _
      | Pexp_unboxed_unit | Pexp_unboxed_bool _ | Pexp_unboxed_tuple _
      | Pexp_idx _
      | Pexp_while _ | Pexp_for _
      | Pexp_constraint _ | Pexp_coerce _
      | Pexp_override _ | Pexp_object _
      | Pexp_pack _
      | Pexp_extension _ | Pexp_unreachable
      | Pexp_begin_end _ | Pexp_quote _ | Pexp_hole
      | Pexp_comprehension _
        -> super ctx e

    and simp_record_fields fields =
      List.map
        (fun (field : expression record_field) ->
           match field.value with
           | None -> field
           | Some v ->
             { field with
               value = Some (simp_expr ~allow_semicolon_swallowers:false free v) })
        fields

    and simp_arg
        ?(allow_bare_cases = true)
        ?(allow_semicolon_swallowers = true)
        ?(allow_bare_sequence = true)
        ?(rightmost = Not_rightmost)
        prec x =
      match x.parg_desc with
      | Parg_unlabelled ({ arg; typ_constraint = None;
                           legacy_modes = No_modes;
                           modes = No_modes; } as parg) ->
        let arg =
          simp_expr ~allow_bare_cases ~allow_semicolon_swallowers
            ~allow_bare_sequence ~rightmost prec arg
        in
        {x with parg_desc = Parg_unlabelled {parg with arg}}
      | Parg_labelled ({ maybe_punned = Some arg; typ_constraint = None;
                         legacy_modes = No_modes;
                         modes = No_modes;
                         default = None; _ } as parg) ->
        (* Labeled args always parse at [apply_arg] regardless of the
           surrounding container's threshold: `~x:a + b` is `(f ~x:a) + b`. *)
        let arg = simp_expr apply_arg arg in
        {x with parg_desc = Parg_labelled {parg with maybe_punned = Some arg}}
      | _ ->
        self#argument self#expression ctx x

    and simp_cases ?(rightmost = Not_rightmost) prec = function
      | [x] ->
        (* Last case body: inherits the outer threshold, is rightmost,
           nothing follows to swallow. *)
        let pc_lhs = self#pattern ctx x.pc_lhs in
        let pc_guard = self#option self#expression ctx x.pc_guard in
        let pc_rhs = simp_expr ~rightmost prec x.pc_rhs in
        let pc_tokens = self#seq ctx x.pc_tokens in
        let pc_loc = self#location ctx x.pc_loc in
        [{pc_lhs; pc_guard; pc_rhs; pc_tokens; pc_loc}]
      | x :: xs ->
        (* Non-last case: followed by `| pat -> ...`; a bare nested
           match/try/function body would absorb that `|`. Never rightmost. *)
        let pc_lhs = self#pattern ctx x.pc_lhs in
        let pc_guard = self#option self#expression ctx x.pc_guard in
        let pc_rhs = simp_expr ~allow_bare_cases:false free x.pc_rhs in
        let pc_tokens = self#seq ctx x.pc_tokens in
        let pc_loc = self#location ctx x.pc_loc in
        {pc_lhs; pc_guard; pc_rhs; pc_tokens; pc_loc} :: simp_cases ~rightmost prec xs
      | [] -> assert false
    in
    simp_expr ~rightmost e
end

let remover = object(self)
  inherit [unit] Traversals_helpers.map_with_context
  inherit [unit] Traversals.map_with_context as super

  method position _ p = p

  method! expression ctx expr =
    Expr.simplify super#expression super#pattern self ctx free expr

  method! pattern ctx pat =
    Pattern.simplify_pat super#pattern self ctx pat

  method! value_binding ctx vb =
    let vb' = super#value_binding ctx vb in
    let simp_param = Pattern.simp_param super#pattern self ctx in
    let simp_pat = Pattern.simp_pat super#pattern self ctx in
    (* With a type constraint, the pattern is NOT a free position:
       `let a, b : t = e` attaches the constraint to `b` alone. *)
    let pvb_pat =
      match vb.pvb_constraint with
      | None -> vb'.pvb_pat
      | Some _ -> simp_pat atom vb.pvb_pat
    in
    (* The binding's RHS is the ultimate tail — seed [Rightmost] outright. *)
    let pvb_expr =
      Option.map
        (Expr.simplify super#expression super#pattern self ctx
           ~rightmost:Rightmost free)
        vb.pvb_expr
    in
    { vb' with pvb_pat; pvb_expr; pvb_params = List.map simp_param vb.pvb_params }

  method! class_structure ctx cs =
    (* [pcstr_self] (`object (self) ...`) is mandatory-paren syntax, not
       redundant grouping — leave it untouched. *)
    { cs with pcstr_fields = self#list self#class_field ctx cs.pcstr_fields }

  method! class_infos f ctx ci =
    let ci' = super#class_infos f ctx ci in
    let simp_pat_arg = Pattern.simp_pat_arg super#pattern self ctx in
    { ci' with
      pci_value_params = List.map (simp_pat_arg atom) ci.pci_value_params }

  method! core_type ctx t =
    Type.simplify_typ super#core_type ctx t
end

let inserter = object
  inherit [unit] Traversals_helpers.map_with_context
  inherit [unit] Traversals.map_with_context as super

  method position _ p = p

  method! pattern ctx patt =
    match patt.ppat_desc with
    | Ppat_parens {pat; optional} ->
      let pat = super#pattern ctx pat in
      {patt with ppat_desc = Ppat_parens {pat; optional}}
    | Ppat_any | Ppat_var _ | Ppat_constant _ -> patt
    | _ -> Pattern.parens_pat (super#pattern ctx patt)

  method! core_type ctx typ =
    match typ.ptyp_desc with
    | Ptyp_parens ptyp ->
      let ptyp = super#core_type ctx ptyp in
      {typ with ptyp_desc = Ptyp_parens ptyp}
    | Ptyp_any _ | Ptyp_var _ | Ptyp_poly _ -> typ
    | _ -> Type.parens_typ (super#core_type ctx typ)

  method! expression ctx expr =
    match expr.pexp_desc with
    | Pexp_parens pexp ->
      let exp = super#expression ctx pexp.exp in
      {expr with pexp_desc = Pexp_parens {pexp with exp}}
    | Pexp_ident _ -> expr
    | _ -> Expression.parens_exp (super#expression ctx expr)
end
