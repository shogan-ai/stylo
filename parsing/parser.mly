/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           */
/*                                                                        */
/*   Copyright 1996 Institut National de Recherche en Informatique et     */
/*     en Automatique.                                                    */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

/* The parser definition */

/* The commands [make list-parse-errors] and [make generate-parse-errors]
   run Menhir on a modified copy of the parser where every block of
   text comprised between the markers [BEGIN AVOID] and -----------
   [END AVOID] has been removed. This file should be formatted in
   such a way that this results in a clean removal of certain
   symbols, productions, or declarations. */

%{

[@@@ocaml.warning "-60"]
module Ocaml_syntax = struct end (* https://github.com/ocaml/dune/issues/2450 *)
module Str = Ast_helper.Str (* For ocamldep *)
[@@@ocaml.warning "+60"]

let failwith s = raise (Parser_types.Failwith s)

open Asttypes
open Longident
open Parsetree
open Ast_helper
open Docstrings
open Docstrings.WithMenhir
open Parser_types

let mkloc = Location.mkloc

let make_loc (startpos, endpos) = {
  Location.loc_start = startpos;
  Location.loc_end = endpos;
  Location.loc_ghost = false;
}

let ghost_loc (startpos, endpos) = {
  Location.loc_start = startpos;
  Location.loc_end = endpos;
  Location.loc_ghost = true;
}

let mklid ~loc d = { Longident.desc = d; tokens = Tokens.at loc }
let mkunit loc = mklid ~loc (Lident (Str "()"))

let wrap_str = Location.map (fun s -> Longident.Str s)

let mktyp ~loc ?attrs d =
  let tokens = Tokens.at loc in
  Typ.mk ~loc:(make_loc loc) ~tokens ?attrs d
let mkpat ~loc ?attrs d =
  let tokens = Tokens.at loc in
  Pat.mk ~loc:(make_loc loc) ~tokens ?attrs d
let mkexp ~loc ?attrs d =
  let tokens = Tokens.at loc in
  Exp.mk ~loc:(make_loc loc) ~tokens ?attrs d
let mkcase sloc pat ?guard exp =
  let tokens = Tokens.at sloc in
  Exp.case ~tokens pat ?guard exp
let mkmty ~loc ?attrs d =
  let tokens = Tokens.at loc in
  Mty.mk ~loc:(make_loc loc) ?attrs ~tokens d
let mksig ~loc d =
  let tokens = Tokens.at loc in
  Sig.mk ~loc:(make_loc loc) ~tokens d
let mkmod ~loc ?attrs d =
  let tokens = Tokens.at loc in
  Mod.mk ~loc:(make_loc loc) ?attrs ~tokens d
let mkstr ~loc d =
  let tokens = Tokens.at loc in
  Str.mk ~loc:(make_loc loc) ~tokens d
let mkclass ~loc ?ext_attrs ?attrs d =
  Cl.mk ~loc:(make_loc loc) ?ext_attrs ?attrs d
let mkcty ~loc ?attrs d =
  let tokens = Tokens.at loc in
  Cty.mk ~loc:(make_loc loc) ?attrs ~tokens d

let mkwc loc wc_desc =
  { wc_desc; wc_loc = make_loc loc; wc_tokens = Tokens.at loc }

let pstr_typext te =
  Pstr_typext te
let pstr_primitive vd =
  Pstr_primitive vd
let pstr_type (nr, tys) =
  Pstr_type (nr, tys)
let pstr_exception te =
  Pstr_exception te

let psig_typext te =
  Psig_typext te
let psig_value vd =
  Psig_value vd
let psig_type (nr, tys) =
  Psig_type (nr, tys)
let psig_typesubst (nr, tys) =
  assert (nr = Recursive); (* see [no_nonrec_flag] *)
  Psig_typesubst tys
let psig_exception te =
  Psig_exception te

let mkctf ~loc ?attrs ?docs d =
  Ctf.mk ~loc:(make_loc loc) ?attrs ?docs d
    ~tokens:(Tokens.at loc)
let mkcf ~loc ?attrs ?docs d =
  Cf.mk ~loc:(make_loc loc) ?attrs ?docs d
    ~tokens:(Tokens.at loc)

let mkrhs : 'a. 'a -> (Lexing.position * Lexing.position) -> 'a with_loc =
  fun rhs loc -> mkloc rhs (make_loc loc)

let mkexpvar ~loc (name : Longident.str_or_op) =
  mkexp ~loc (Pexp_ident(mkrhs (mklid ~loc (Lident name)) loc))

let mkoperator =
  mkexpvar

let mkpatvar ~loc name =
  mkpat ~loc (Ppat_var (mkrhs name loc))

(* See commentary about ghost locations at the declaration of Location.t *)
let ghexp ~loc d =
  let tokens = Tokens.at loc in
  Exp.mk ~tokens ~loc:(ghost_loc loc) d
let ghpat ~loc d =
  let tokens = Tokens.at ~consume_synthesized:false loc in
  Pat.mk ~tokens ~loc:(ghost_loc loc) d

let mkinfix arg1 op arg2 =
  Pexp_infix_apply {op; arg1; arg2}

let no_attrs = [], []
let mkuminus ~oploc:loc name arg =
  Pexp_add_or_sub (name, arg),
  empty_ext_attr (make_loc loc)

let mkuplus = mkuminus

let mk_attr ~sloc name payload =
  Attr.mk ~loc:(make_loc sloc) ~tokens:(Tokens.at sloc) name payload

let mkpat_with_modes ~loc ~pat ~cty ~modes =
  match cty, modes with
  | None, [] -> pat
  | cty, modes -> mkpat ~loc (Ppat_constraint (pat, cty, modes))

let mkexp_constraint ~loc ~exp ~cty ~modes =
  match cty, modes with
  | None, [] -> invalid_arg "empty mkexp_constraint" (* ? *)
  | cty, modes -> mkexp ~loc (Pexp_constraint (exp, cty, modes))

let ghexp_constraint ~loc ~exp ~cty ~modes =
  let exp = mkexp_constraint ~loc ~exp ~cty ~modes in
  { exp with pexp_loc = { exp.pexp_loc with loc_ghost = true }}

let maybe_curry_typ typ _loc =
  typ

(* TODO define an abstraction boundary between locations-as-pairs
   and locations-as-Location.t; it should be clear when we move from
   one world to the other *)

let mkexp_cons_desc _consloc hd tl = Pexp_cons(hd, tl)
let mkexp_cons ~loc consloc hd tl =
  mkexp ~loc (mkexp_cons_desc consloc hd tl)

let mkpat_cons_desc _consloc hd tl = Ppat_cons(hd, tl)
let mkpat_cons ~loc consloc hd tl =
  mkpat ~loc (mkpat_cons_desc consloc hd tl)

let mktailexp _nilloc elts = Pexp_list elts, _nilloc (* whatever *)

let mktailpat _nilloc elts = Ppat_list elts, _nilloc (* whatever *)

let mkstrexp sloc e attrs =
  Str.eval ~loc:(make_loc sloc) ~attrs ~tokens:(Tokens.at sloc) e

let syntax_error () =
  failwith "Syntax error"

let unclosed _opening_name _opening_loc _closing_name _closing_loc =
  let pos : Lexing.position = fst _opening_loc in
  Format.sprintf "%s: Unclosed %S at pos %d:%d@."
    pos.pos_fname _opening_name pos.pos_lnum (pos.pos_cnum - pos.pos_bol)
  |> failwith

let unspliceable loc =
  let pos : Lexing.position = fst loc in
  Format.sprintf "%s %d:%d: Syntax error: expression cannot be spliced"
    pos.pos_fname pos.pos_lnum (pos.pos_cnum - pos.pos_bol)
  |> failwith

(* Normal mutable arrays and immutable arrays are parsed identically, just with
   different delimiters.  The parsing is done by the [array_exprs] rule, and the
   [Generic_array] module provides (1) a type representing the possible results,
   and (2) a function for going from that type to an AST fragment representing
   an array. *)
module Generic_array = struct
  (** The possible ways of parsing an array (writing [[? ... ?]] for either
      [[| ... |]] or [[: ... :]]). The set of available constructs differs
      between expressions and patterns.
  *)

  module Simple = struct
    type 'a t =
      | Literal of 'a list
      (** A plain array literal/pattern, [[? x; y; z ?]] *)
      | Unclosed of (Lexing.position * Lexing.position) *
                    (Lexing.position * Lexing.position)
      (** Parse error: an unclosed array literal, [\[? x; y; z] with no closing
          [?\]]. *)

    let to_ast (open_ : string) (close : string) array t =
      match t with
      | Literal elts -> array elts
      | Unclosed (startpos, endpos) -> unclosed open_ startpos close endpos
  end


  module Expression = struct
    type t =
      | Simple of expression Simple.t
      | Opened_literal of Longident.t with_loc *
                        Lexing.position *
                        Lexing.position *
                        expression list
      (** An array literal with a local open, [Module.[? x; y; z ?]] (only valid
          in expressions) *)

    let to_desc (open_ : string) (close : string) mut t =
        let array elts = Pexp_array (mut, elts) in
        match t with
        | Simple x -> Simple.to_ast open_ close array x
        | Opened_literal (od, startpos, endpos, elts) ->
          Pexp_dot_open (od, mkexp ~loc:(startpos, endpos) (array elts))

    let to_expression (open_ : string) (close : string) mut ~loc t =
      let array ~loc elts = mkexp ~loc (Pexp_array (mut, elts)) in
      match t with
      | Simple x -> Simple.to_ast open_ close (array ~loc) x
      | Opened_literal (od, startpos, endpos, elts) ->
        mkexp ~loc (Pexp_dot_open (od, array ~loc:(startpos, endpos) elts))
  end

  module Pattern = struct
    type t = pattern Simple.t
    let to_ast open_ close mut (t : t) =
      Simple.to_ast open_ close (fun elts -> Ppat_array (mut, elts)) t
  end
end

let expecting (_loc : Lexing.position * Lexing.position) _nonterm =
  let pos = fst _loc in
  Printf.sprintf "%d:%d expected %s"
    pos.pos_lnum (pos.pos_cnum - pos.pos_bol) _nonterm
  |> failwith

(* Using the function [not_expecting] in a semantic action means that this
   syntactic form is recognized by the parser but is in fact incorrect. This
   idiom is used in a few places to produce ad hoc syntax error messages. *)

(* This idiom should be used as little as possible, because it confuses the
   analyses performed by Menhir. Because Menhir views the semantic action as
   opaque, it believes that this syntactic form is correct. This can lead
   [make generate-parse-errors] to produce sentences that cause an early
   (unexpected) syntax error and do not achieve the desired effect. This could
   also lead a completion system to propose completions which in fact are
   incorrect. In order to avoid these problems, the productions that use
   [not_expecting] should be marked with AVOID. *)

let not_expecting _loc _nonterm =
  let pos = fst _loc in
  Printf.sprintf "%d:%d did not expect %s"
    pos.Lexing.pos_lnum (pos.pos_cnum - pos.pos_bol) _nonterm
  |> failwith

let mkexp_type_constraint_with_modes ?(ghost=false) ~loc ~modes e t =
  match t with
  | Pconstraint t ->
     let mk = if ghost then ghexp_constraint else mkexp_constraint in
     mk ~loc ~exp:e ~cty:(Some t) ~modes
  | Pcoerce(t1, t2)  ->
     match modes with
     | [] ->
      let mk = if ghost then ghexp else mkexp ?attrs:None in
      mk ~loc (Pexp_coerce(e, t1, t2))
     | _ :: _ -> not_expecting loc "mode annotations"

(* Helper functions for desugaring array indexing operators *)

let paren_to_strings = function
  | Paren -> "(", ")"
  | Bracket -> "[", "]"
  | Brace -> "{", "}"

let indexop_unclosed_error loc_s s loc_e =
  let left, right = paren_to_strings s in
  unclosed left loc_s right loc_e

let lapply ~loc p1 p2 =
  mklid ~loc (Lapply(p1, p2))

let wrap_exp_attrs ~loc:_ body pexp_ext_attr =
  { body with pexp_ext_attr }

let mkexp_attrs ~loc d ext_attrs =
  wrap_exp_attrs ~loc (mkexp ~loc d) ext_attrs

let wrap_pat_attrs ~loc:_ pat ext_attr =
  { pat with ppat_ext_attr = ext_attr }

let mkpat_attrs ~loc d attrs =
  wrap_pat_attrs ~loc (mkpat ~loc d) attrs

let mk_quotedext ~loc (id, idloc, str, _, delim) =
  let exp_id = mkloc [id] idloc in
  (exp_id, PString (str, delim), Tokens.at loc)

let text_str pos = Str.text (rhs_text pos)
let text_sig pos = Sig.text (rhs_text pos)
let text_cstr pos = Cf.text (rhs_text pos)
let text_csig pos = Ctf.text (rhs_text pos)
let text_def pos =
  List.map (fun def ->
    let fake_tok = (* FIXME: don't add synthezise nodes *)
      { Tokens.desc = Child_node ; pos = def.pstr_loc.loc_start } in
    Ptop_def ([def], [fake_tok])) (Str.text (rhs_text pos))

let extra_text startpos endpos text items =
  match items with
  | [] ->
      let post = rhs_post_text endpos in
      let post_extras = rhs_post_extra_text endpos in
      text post @ text post_extras
  | _ :: _ ->
      let pre_extras = rhs_pre_extra_text startpos in
      let post_extras = rhs_post_extra_text endpos in
        text pre_extras @ items @ text post_extras

let extra_str p1 p2 items = extra_text p1 p2 Str.text items
let extra_sig p1 p2 items = extra_text p1 p2 Sig.text items
let extra_cstr p1 p2 items = extra_text p1 p2 Cf.text items
let extra_csig p1 p2 items = extra_text p1 p2 Ctf.text  items
let extra_def p1 p2 items =
  extra_text p1 p2
    (fun txt -> List.map (fun def ->
      let fake_tok = (* FIXME: don't add synthezise nodes *)
        { Tokens.desc = Child_node ; pos = def.pstr_loc.loc_start } in
      Ptop_def ([def], [fake_tok])) (Str.text txt))
    items

let mk_arrow_arg ~loc lbl legacy_modes typ modes =
  let doc =
    rhs_info (snd loc)
    |> Docs.info
  in
  let tokens = Tokens.at loc in
  {
    aa_lbl = lbl;
    aa_legacy_modes = legacy_modes;
    aa_type = typ;
    aa_modes = modes;
    aa_doc = doc;
    aa_loc = make_loc loc;
    aa_tokens = tokens;
  }

let mklb first ~loc body ext_attrs attrs =
  { lb_ext_attrs = ext_attrs;
    lb_body = body;
    lb_attributes = attrs;
    lb_docs = symbol_docs_lazy loc;
    lb_text = (if first then empty_text_lazy
               else symbol_text_lazy (fst loc));
    lb_loc = make_loc loc;
    lb_toks = Tokens.at loc;
  }

let addlb lbs lb =
  (* We are more permissive than upstream, this is ok. *)
(*
  if Option.is_none lb.lb_body.lbb_expr && lbs.lbs_extension = None then
    syntax_error ();
*)
  { lbs with lbs_bindings = lb :: lbs.lbs_bindings }

let mklbs mf rf lb =
  let lbs = {
    lbs_bindings = [];
    lbs_mutable = mf;
    lbs_rec = rf;
  } in
  addlb lbs lb

let val_of_let_bindings ~loc lbs =
  let bindings =
    List.map
      (fun lb ->
         Vb.mk ~loc:lb.lb_loc ~attrs:lb.lb_attributes
           ~ext_attr:lb.lb_ext_attrs
           ~tokens:lb.lb_toks
           ~legacy_modes:lb.lb_body.lbb_legacy_modes
           ~modes:lb.lb_body.lbb_modes
           ~docs:(Lazy.force lb.lb_docs)
           ~text:(Lazy.force lb.lb_text)
           ?value_constraint:lb.lb_body.lbb_constraint
           ~params:lb.lb_body.lbb_params
           ~ret_modes:lb.lb_body.lbb_ret_modes
           lb.lb_body.lbb_pat lb.lb_body.lbb_expr)
      lbs.lbs_bindings
  in
  match lbs.lbs_mutable with
  | Mutable -> syntax_error () (* FIXME: loc, err msg, etc. *)
  | Immutable -> mkstr ~loc (Pstr_value(lbs.lbs_rec, List.rev bindings))

let binding_with_params bindings =
  List.find_opt (fun binding -> binding.lb_body.lbb_params <> [])
  bindings
  |> Option.map (fun binding -> binding.lb_loc)

let expr_of_let_bindings ~loc lbs body =
  let bindings =
    List.map
      (fun lb ->
         Vb.mk ~loc:lb.lb_loc ~attrs:lb.lb_attributes
           ~ext_attr:lb.lb_ext_attrs
           ~tokens:lb.lb_toks
           ~legacy_modes:lb.lb_body.lbb_legacy_modes
           ~modes:lb.lb_body.lbb_modes
           ~docs:(Lazy.force lb.lb_docs)
           ~text:(Lazy.force lb.lb_text)
           ?value_constraint:lb.lb_body.lbb_constraint
           ~params:lb.lb_body.lbb_params
           ~ret_modes:lb.lb_body.lbb_ret_modes
           lb.lb_body.lbb_pat lb.lb_body.lbb_expr)
      lbs.lbs_bindings
  in
  (* Disallow [let mutable f x y = ..] but still allow
   * [let mutable f = fun x y -> ..]. *)
  match lbs.lbs_mutable, binding_with_params lbs.lbs_bindings with
  | Mutable, Some _loc -> syntax_error () (* FIXME *)
  | _ ->
    mkexp ~loc (Pexp_let(lbs.lbs_mutable, lbs.lbs_rec, List.rev bindings, body))

let class_of_let_bindings ~loc lbs body =
  let bindings =
    List.map
      (fun lb ->
         Vb.mk ~loc:lb.lb_loc ~attrs:lb.lb_attributes
           ~ext_attr:lb.lb_ext_attrs
           ~tokens:lb.lb_toks
           ~legacy_modes:lb.lb_body.lbb_legacy_modes
           ~modes:lb.lb_body.lbb_modes
           ~docs:(Lazy.force lb.lb_docs)
           ~text:(Lazy.force lb.lb_text)
           ?value_constraint:lb.lb_body.lbb_constraint
           ~params:lb.lb_body.lbb_params
           ~ret_modes:lb.lb_body.lbb_ret_modes
           lb.lb_body.lbb_pat lb.lb_body.lbb_expr)
      lbs.lbs_bindings
  in
  match lbs.lbs_mutable with
  | Mutable -> syntax_error () (* FIXME *)
  | Immutable ->
    mkclass ~loc (Pcl_let (lbs.lbs_rec, List.rev bindings, body))

let empty_body_constraint =
  { ret_type_constraint = None; ret_mode_annotations = []}

let mkfunction ~loc ?ext_attrs params body_constraint body =
  let desc = Pexp_function (params, body_constraint, body) in
  match ext_attrs with
  | None -> mkexp ~loc desc
  | Some ext_attrs -> mkexp_attrs desc ext_attrs ~loc


      (*
(* There's no dedicated syntax for module instances. Functor application
   syntax is translated into a module instance expression.
*)
let pmod_instance : module_expr -> module_expr_desc =
  let raise_malformed_instance _ =
    failwith "TODO"
  in
  let head_of_ident (lid : Longident.t Location.loc) =
    match lid with
    | { txt = Lident s; loc = _ } -> s
    | { txt = _; loc } ->  raise_malformed_instance loc
  in
  let gather_args mexpr =
    let rec loop mexpr acc =
      match mexpr with
      | { pmod_desc = Pmod_apply (f, v); _ } ->
         (match f.pmod_desc with
          | Pmod_apply (f, n) -> loop f ((n, v) :: acc)
          | _ -> raise_malformed_instance f.pmod_loc)
      | head -> head, acc
    in
    loop mexpr []
  in
  let string_of_module_expr mexpr =
    match mexpr.pmod_desc with
    | Pmod_ident i -> head_of_ident i
    | _ -> raise_malformed_instance mexpr.pmod_loc
  in
  let rec instance_of_module_expr mexpr =
    match gather_args mexpr with
    | { pmod_desc = Pmod_ident i; _ }, args ->
        let head = head_of_ident i in
        let args = List.map instances_of_arg_pair args in
        { pmod_instance_head = head; pmod_instance_args = args }
    | { pmod_loc; _ }, _ -> raise_malformed_instance pmod_loc
  and instances_of_arg_pair (n, v) =
    string_of_module_expr n, instance_of_module_expr v
  in
  fun mexpr -> Pmod_instance (instance_of_module_expr mexpr)
;;
*)

let mk_directive_arg ~loc k =
  { pdira_desc = k;
    pdira_loc = make_loc loc;
  }

let mk_directive ~loc name arg =
  Ptop_dir {
      pdir_name = name;
      pdir_arg = arg;
      pdir_loc = make_loc loc;
    }

(* Unboxed literals *)

(* CR layouts v2.5: The [unboxed_*] functions will both be improved and lose
   their explicit assert once we have real unboxed literals in Jane syntax; they
   may also get re-inlined at that point *)
type sign = Positive | Negative

let unboxed_int _sloc _int_loc ?sign (n, m) =
  let sign = Option.map (function Positive -> "+" | Negative -> "-") sign in
  match m with
  | Some m -> Pconst_unboxed_integer (sign, n, m)
  | None ->
    let pos = fst _sloc in
    Printf.sprintf "%d:%d unboxed integer needs a width"
      pos.Lexing.pos_lnum (pos.pos_cnum - pos.pos_bol)
    |> failwith

let unboxed_float ?sign (f, m) =
  let sign = Option.map (function Positive -> "+" | Negative -> "-") sign in
  Pconst_unboxed_float (sign, f, m)

(* Invariant: [lident] must end with an [Lident] that ends with a ["#"]. *)
let unboxed_type sloc lident tys =
  let loc = make_loc sloc in
  Ptyp_constr (tys, mkloc lident loc)
%}


/* Precedences and associativities.

Tokens and rules have precedences.  A reduce/reduce conflict is resolved
in favor of the first rule (in source file order).  A shift/reduce conflict
is resolved by comparing the precedence and associativity of the token to
be shifted with those of the rule to be reduced.

By default, a rule has the precedence of its rightmost terminal (if any).

When there is a shift/reduce conflict between a rule and a token that
have the same precedence, it is resolved using the associativity:
if the token is left-associative, the parser will reduce; if
right-associative, the parser will shift; if non-associative,
the parser will declare a syntax error.

We will only use associativities with operators of the kind  x * x -> x
for example, in the rules of the form    expr: expr BINOP expr
in all other cases, we define two precedences if needed to resolve
conflicts.

The precedences must be listed from low to high.
*/

%nonassoc IN
%nonassoc below_SEMI
%nonassoc SEMI                          /* below EQUAL ({lbl=...; lbl=...}) */
%nonassoc LET FOR                       /* above SEMI ( ...; let ... in ...) */
%nonassoc below_WITH
%nonassoc FUNCTION WITH                 /* below BAR  (match ... with ...) */
%nonassoc AND             /* above WITH (module rec A: SIG with ... and ...) */
%nonassoc THEN                          /* below ELSE (if ... then ...) */
%nonassoc ELSE                          /* (if ... then ... else ...) */
%nonassoc LESSMINUS                     /* below COLONEQUAL (lbl <- x := e) */
%right    COLONEQUAL                    /* expr (e := e := e) */
%nonassoc AS
%left     BAR                           /* pattern (p|p|p) */
%nonassoc below_COMMA
%left     COMMA                         /* expr/labeled_tuple (e,e,e) */
%nonassoc below_FUNCTOR                 /* include M */
%nonassoc FUNCTOR                       /* include functor M */
%right    MINUSGREATER                  /* function_type (t -> t -> t) */
%right    OR BARBAR                     /* expr (e || e || e) */
%nonassoc below_AMPERSAND
%right    AMPERSAND AMPERAMPER          /* expr (e && e && e) */
%nonassoc below_EQUAL
%left     INFIXOP0 EQUAL LESS GREATER   /* expr (e OP e OP e) */
%right    ATAT AT INFIXOP1              /* expr (e OP e OP e) */
%nonassoc below_LBRACKETAT
%nonassoc LBRACKETAT
%right    COLONCOLON                    /* expr (e :: e :: e) */
%left     INFIXOP2 PLUS PLUSDOT MINUS MINUSDOT PLUSEQ /* expr (e OP e OP e) */
%left     PERCENT INFIXOP3 MOD STAR                 /* expr (e OP e OP e) */
%right    INFIXOP4                      /* expr (e OP e OP e) */
%nonassoc prec_unboxed_product_kind
%nonassoc prec_unary_minus prec_unary_plus /* unary - */
%nonassoc prec_constant_constructor     /* cf. simple_expr (C versus C x) */
%nonassoc prec_constr_appl              /* above AS BAR COLONCOLON COMMA */
%nonassoc below_HASH
%nonassoc HASH HASH_SUFFIX              /* simple_expr/toplevel_directive */
%left     HASHOP
%nonassoc below_DOT
%nonassoc DOT DOTHASH DOTOP
/* Finally, the first tokens of simple_expr are above everything else. */
%nonassoc BACKQUOTE BANG BEGIN CHAR HASH_CHAR FALSE FLOAT HASH_FLOAT
          INT HASH_INT OBJECT
          LBRACE LBRACELESS LBRACKET LBRACKETBAR LBRACKETCOLON LIDENT LPAREN
          NEW PREFIXOP STRING TRUE UIDENT LESSLBRACKET DOLLAR
          LBRACKETPERCENT QUOTED_STRING_EXPR HASHLBRACE HASHLPAREN UNDERSCORE


/* Entry points */

/* Several start symbols are marked with AVOID so that they are not used by
   [make generate-parse-errors]. The three start symbols that we keep are
   [implementation], [use_file], and [toplevel_phrase]. The latter two are
   of marginal importance; only [implementation] really matters, since most
   states in the automaton are reachable from it. */

%start implementation                   /* for implementation files */
%type <Parsetree.structure> implementation
/* BEGIN AVOID */
%start interface                        /* for interface files */
%type <Parsetree.signature> interface
/* END AVOID */
%start toplevel_phrase                  /* for interactive use */
%type <Parsetree.toplevel_phrase> toplevel_phrase
%start use_file                         /* for the #use directive */
%type <Parsetree.toplevel_phrase list> use_file
/* BEGIN AVOID */
%start parse_module_type
%type <Parsetree.module_type> parse_module_type
%start parse_module_expr
%type <Parsetree.module_expr> parse_module_expr
%start parse_core_type
%type <Parsetree.core_type> parse_core_type
%start parse_expression
%type <Parsetree.expression> parse_expression
%start parse_pattern
%type <Parsetree.pattern> parse_pattern
%start parse_constr_longident
%type <Longident.t> parse_constr_longident
%start parse_val_longident
%type <Longident.t> parse_val_longident
%start parse_mty_longident
%type <Longident.t> parse_mty_longident
%start parse_mod_ext_longident
%type <Longident.t> parse_mod_ext_longident
%start parse_mod_longident
%type <Longident.t> parse_mod_longident
%start parse_any_longident
%type <Longident.t> parse_any_longident
/* END AVOID */

%%

/* macros */
%inline extra_str(symb): symb { extra_str $startpos $endpos $1 };
%inline extra_sig(symb): symb { extra_sig $startpos $endpos $1 };
%inline extra_cstr(symb): symb { extra_cstr $startpos $endpos $1 };
%inline extra_csig(symb): symb { extra_csig $startpos $endpos $1 };
%inline extra_def(symb): symb { extra_def $startpos $endpos $1 };
%inline extra_text(symb): symb { extra_text $startpos $endpos $1 };
%inline mkrhs(symb): symb
    { mkrhs $1 $sloc }
;

%inline mklid(symb): symb
    { mklid $1 ~loc:$sloc }
;

%inline text_str(symb): symb
  { text_str $startpos @ [$1] }
%inline text_str_SEMISEMI: SEMISEMI
  { text_str $startpos }
%inline text_sig(symb): symb
  { text_sig $startpos @ [$1] }
%inline text_sig_SEMISEMI: SEMISEMI
  { text_sig $startpos }
%inline text_def(symb): symb
  { text_def $startpos @ [$1] }
%inline top_def(symb): symb
  { let def = $1 in
    let fake_tok = (* FIXME: don't add synthezise nodes *)
      { Tokens.desc = Child_node ; pos = def.pstr_loc.loc_start } in
    Ptop_def ([def], [fake_tok]) }
%inline text_cstr(symb): symb
  { text_cstr $startpos @ [$1] }
%inline text_csig(symb): symb
  { text_csig $startpos @ [$1] }

(* Using this %inline definition means that we do not control precisely
   when [mark_rhs_docs] is called, but I don't think this matters. *)
%inline mark_rhs_docs(symb): symb
  { mark_rhs_docs $startpos $endpos;
    $1 }

%inline op(symb): symb
   { mkoperator ~loc:$sloc $1 }

%inline mkloc(symb): symb
    { mkloc $1 (make_loc $sloc) }

%inline mkexp(symb): symb
    { mkexp ~loc:$sloc $1 }
%inline mkpat(symb): symb
    { mkpat ~loc:$sloc $1 }
%inline mktyp(symb): symb
    { mktyp ~loc:$sloc $1 }
%inline mkstr(symb): symb
    { mkstr ~loc:$sloc $1 }
%inline mksig(symb): symb
    { mksig ~loc:$sloc $1 }
%inline mkmod(symb): symb
    { mkmod ~loc:$sloc $1 }
%inline mkmty(symb): symb
    { mkmty ~loc:$sloc $1 }
%inline mkcty(symb): symb
    { mkcty ~loc:$sloc $1 }
%inline mkctf(symb): symb
    { mkctf ~loc:$sloc $1 }
%inline mkcf(symb): symb
    { mkcf ~loc:$sloc $1 }
%inline mkclass(symb): symb
    { mkclass ~loc:$sloc $1 }

%inline wrap_mkstr_ext(symb): symb
    { wrap_mkstr_ext ~loc:$sloc $1 }
%inline wrap_mksig_ext(symb): symb
    { wrap_mksig_ext ~loc:$sloc $1 }

%inline mk_directive_arg(symb): symb
    { mk_directive_arg ~loc:$sloc $1 }

/* Generic definitions */

(* [iloption(X)] recognizes either nothing or [X]. Assuming [X] produces
   an OCaml list, it produces an OCaml list, too. *)

%inline iloption(X):
  /* nothing */
    { [] }
| x = X
    { x }

(* [llist(X)] recognizes a possibly empty list of [X]s. It is left-recursive. *)

reversed_llist(X):
  /* empty */
    { [] }
| xs = reversed_llist(X) x = X
    { x :: xs }

%inline llist(X):
  xs = rev(reversed_llist(X))
    { xs }

(* [reversed_nonempty_llist(X)] recognizes a nonempty list of [X]s, and produces
   an OCaml list in reverse order -- that is, the last element in the input text
   appears first in this list. Its definition is left-recursive. *)

reversed_nonempty_llist(X):
  x = X
    { [ x ] }
| xs = reversed_nonempty_llist(X) x = X
    { x :: xs }

(* [nonempty_llist(X)] recognizes a nonempty list of [X]s, and produces an OCaml
   list in direct order -- that is, the first element in the input text appears
   first in this list. *)

%inline nonempty_llist(X):
  xs = rev(reversed_nonempty_llist(X))
    { xs }

(* [reversed_nonempty_concat(X)] recognizes a nonempty sequence of [X]s (each of
    which is a list), and produces an OCaml list of their concatenation in
    reverse order -- that is, the last element of the last list in the input text
    appears first in the list.
*)
reversed_nonempty_concat(X):
  x = X
    { List.rev x }
| xs = reversed_nonempty_concat(X) x = X
    { List.rev_append x xs }

(* [nonempty_concat(X)] recognizes a nonempty sequence of [X]s
  (each of which is a list), and produces an OCaml list of their concatenation
  in direct order -- that is, the first element of the first list in the input
  text appears first in the list.
*)
%inline nonempty_concat(X):
  xs = rev(reversed_nonempty_concat(X))
    { xs }

(* [reversed_separated_nonempty_llist(separator, X)] recognizes a nonempty list
   of [X]s, separated with [separator]s, and produces an OCaml list in reverse
   order -- that is, the last element in the input text appears first in this
   list. Its definition is left-recursive. *)

(* [inline_reversed_separated_nonempty_llist(separator, X)] is semantically
   equivalent to [reversed_separated_nonempty_llist(separator, X)], but is
   marked %inline, which means that the case of a list of length one and
   the case of a list of length more than one will be distinguished at the
   use site, and will give rise there to two productions. This can be used
   to avoid certain conflicts. *)

%inline inline_reversed_separated_nonempty_llist(separator, X):
  x = X
    { [ x ] }
| xs = reversed_separated_nonempty_llist(separator, X)
  separator
  x = X
    { x :: xs }

reversed_separated_nonempty_llist(separator, X):
  xs = inline_reversed_separated_nonempty_llist(separator, X)
    { xs }

(* [separated_nonempty_llist(separator, X)] recognizes a nonempty list of [X]s,
   separated with [separator]s, and produces an OCaml list in direct order --
   that is, the first element in the input text appears first in this list. *)

%inline separated_nonempty_llist(separator, X):
  xs = rev(reversed_separated_nonempty_llist(separator, X))
    { xs }

%inline inline_separated_nonempty_llist(separator, X):
  xs = rev(inline_reversed_separated_nonempty_llist(separator, X))
    { xs }

(* [reversed_separated_nontrivial_llist(separator, X)] recognizes a list of at
   least two [X]s, separated with [separator]s, and produces an OCaml list in
   reverse order -- that is, the last element in the input text appears first
   in this list. Its definition is left-recursive. *)

reversed_separated_nontrivial_llist(separator, X):
  xs = reversed_separated_nontrivial_llist(separator, X)
  separator
  x = X
    { x :: xs }
| x1 = X
  separator
  x2 = X
    { [ x2; x1 ] }

(* [separated_nontrivial_llist(separator, X)] recognizes a list of at least
   two [X]s, separated with [separator]s, and produces an OCaml list in direct
   order -- that is, the first element in the input text appears first in this
   list. *)

%inline separated_nontrivial_llist(separator, X):
  xs = rev(reversed_separated_nontrivial_llist(separator, X))
    { xs }

(* [separated_or_terminated_nonempty_list(delimiter, X)] recognizes a nonempty
   list of [X]s, separated with [delimiter]s, and optionally terminated with a
   final [delimiter]. Its definition is right-recursive. *)

separated_or_terminated_nonempty_list(delimiter, X):
  x = X ioption(delimiter)
    { [x] }
| x = X
  delimiter
  xs = separated_or_terminated_nonempty_list(delimiter, X)
    { x :: xs }

(* [reversed_preceded_or_separated_nonempty_llist(delimiter, X)] recognizes a
   nonempty list of [X]s, separated with [delimiter]s, and optionally preceded
   with a leading [delimiter]. It produces an OCaml list in reverse order. Its
   definition is left-recursive. *)

reversed_preceded_or_separated_nonempty_llist(delimiter, X):
  ioption(delimiter) x = X
    { [x] }
| xs = reversed_preceded_or_separated_nonempty_llist(delimiter, X)
  delimiter
  x = X
    { x :: xs }

(* [preceded_or_separated_nonempty_llist(delimiter, X)] recognizes a nonempty
   list of [X]s, separated with [delimiter]s, and optionally preceded with a
   leading [delimiter]. It produces an OCaml list in direct order. *)

%inline preceded_or_separated_nonempty_llist(delimiter, X):
  xs = rev(reversed_preceded_or_separated_nonempty_llist(delimiter, X))
    { xs }

(* [bar_llist(X)] recognizes a nonempty list of [X]'s, separated with BARs,
   with an optional leading BAR. We assume that [X] is itself parameterized
   with an opening symbol, which can be [epsilon] or [BAR]. *)

(* This construction may seem needlessly complicated: one might think that
   using [preceded_or_separated_nonempty_llist(BAR, X)], where [X] is *not*
   itself parameterized, would be sufficient. Indeed, this simpler approach
   would recognize the same language. However, the two approaches differ in
   the footprint of [X]. We want the start location of [X] to include [BAR]
   when present. In the future, we might consider switching to the simpler
   definition, at the cost of producing slightly different locations. TODO *)

reversed_bar_llist(X):
    (* An [X] without a leading BAR. *)
    x = X(epsilon)
      { [x] }
  | (* An [X] with a leading BAR. *)
    x = X(BAR)
      { [x] }
  | (* An initial list, followed with a BAR and an [X]. *)
    xs = reversed_bar_llist(X)
    x = X(BAR)
      { x :: xs }

%inline bar_llist(X):
  xs = reversed_bar_llist(X)
    { List.rev xs }

(* [xlist(A, B)] recognizes [AB*]. We assume that the semantic value for [A]
   is a pair [x, b], while the semantic value for [B*] is a list [bs].
   We return the pair [x, b :: bs]. *)

%inline xlist(A, B):
  a = A bs = B*
    { let (x, b) = a in x, b :: bs }

(* [listx(delimiter, X, Y)] recognizes a nonempty list of [X]s, optionally
   followed with a [Y], separated-or-terminated with [delimiter]s. The
   semantic value is a pair of a list of [X]s and an optional [Y]. *)

listx(delimiter, X, Y):
| x = X ioption(delimiter)
    { [x], None }
| x = X delimiter y = Y delimiter?
    { [x], Some y }
| x = X
  delimiter
  tail = listx(delimiter, X, Y)
    { let xs, y = tail in
      x :: xs, y }

(* -------------------------------------------------------------------------- *)

(* Entry points. *)

(* An .ml file. *)
implementation:
  structure EOF
    { $1 }
;

/* BEGIN AVOID */
(* An .mli file. *)
interface:
  signature EOF
    { $1 }
;
/* END AVOID */

(* A toplevel phrase. *)
toplevel_phrase:
  (* An expression with attributes, ended by a double semicolon. *)
  extra_str(text_str(str_exp))
  SEMISEMI
    { Ptop_def ($1, Tokens.at $sloc) }
| (* A list of structure items, ended by a double semicolon. *)
  extra_str(flatten(text_str(structure_item)*))
  SEMISEMI
    { Ptop_def ($1, Tokens.at $sloc) }
| (* A directive, ended by a double semicolon. *)
  toplevel_directive
  SEMISEMI
    { $1 }
| (* End of input. *)
  EOF
    { raise End_of_file }
;

(* An .ml file that is read by #use. *)
use_file:
  (* An optional standalone expression,
     followed with a series of elements,
     followed with EOF. *)
  extra_def(append(
    optional_use_file_standalone_expression,
    flatten(use_file_element*)
  ))
  EOF
    { $1 }
;

(* An optional standalone expression is just an expression with attributes
   (str_exp), with extra wrapping. *)
%inline optional_use_file_standalone_expression:
  iloption(text_def(top_def(str_exp)))
    { $1 }
;

(* An element in a #used file is one of the following:
   - a double semicolon followed with an optional standalone expression;
   - a structure item;
   - a toplevel directive.
 *)
%inline use_file_element:
  preceded(SEMISEMI, optional_use_file_standalone_expression)
| text_def(top_def(structure_item))
| text_def(mark_rhs_docs(toplevel_directive))
      { $1 }
;

/* BEGIN AVOID */
parse_module_type:
  module_type EOF
    { $1 }
;

parse_module_expr:
  module_expr EOF
    { $1 }
;

parse_core_type:
  core_type EOF
    { $1 }
;

parse_expression:
  seq_expr EOF
    { $1 }
;

parse_pattern:
  pattern EOF
    { $1 }
;

parse_mty_longident:
  mty_longident EOF
    { $1 }
;

parse_val_longident:
  val_longident EOF
    { $1 }
;

parse_constr_longident:
  constr_longident EOF
    { $1 }
;

parse_mod_ext_longident:
  mod_ext_longident EOF
    { $1 }
;

parse_mod_longident:
  mod_longident EOF
    { $1 }
;

parse_any_longident:
  any_longident EOF
    { $1 }
;
/* END AVOID */

(* -------------------------------------------------------------------------- *)

(* Functor arguments appear in module expressions and module types. *)

%inline functor_args:
  reversed_nonempty_llist(functor_arg)
    { List.rev $1 }
;

functor_arg:
    (* An anonymous and untyped argument. *)
    LPAREN RPAREN
      { Unit }
  | (* An argument accompanied with an explicit type. *)
    LPAREN x = mkrhs(module_name) COLON mty_mm = module_type_with_optional_modes RPAREN
      { let mty, mm = mty_mm in
        Named (x, mty, mm) }
;

module_name:
    (* A named argument. *)
    x = UIDENT
      { Some x }
  | (* An anonymous argument. *)
    UNDERSCORE
      { None }
;

module_name_modal(at_modal_expr):
  | mkrhs(module_name) { $1, [] }
  | LPAREN mkrhs(module_name) at_modal_expr RPAREN { $2, $3 }

(* -------------------------------------------------------------------------- *)

(* Module expressions. *)

(* The syntax of module expressions is not properly stratified. The cases of
   functors, functor applications, and attributes interact and cause conflicts,
   which are resolved by precedence declarations. This is concise but fragile.
   Perhaps in the future an explicit stratification could be used. *)

module_expr:
  | STRUCT attrs = attributes s = structure END
      { mkmod ~loc:$sloc (Pmod_structure (attrs, s)) }
  | STRUCT attributes structure error
      { unclosed "struct" $loc($1) "end" $loc($4) }
  | SIG error
      { expecting $loc($1) "struct" }
  | FUNCTOR attrs = attributes args = functor_args MINUSGREATER me = module_expr
      { mkmod ~loc:$sloc (Pmod_functor (attrs, args, me)) }
  | me = paren_module_expr
      { me }
  | me = module_expr attr = attribute
      { Mod.attr me (attr, $loc(attr)) }
  | mkmod(
      (* A module identifier. *)
      x = mkrhs(mod_longident)
        { Pmod_ident x }
    | (* In a functor application, the actual argument must be parenthesized. *)
      me1 = module_expr me2 = paren_module_expr
        { Pmod_apply(me1, me2) }
    | (* Functor applied to unit. *)
      me = module_expr LPAREN RPAREN
        { Pmod_apply_unit me }
    | (* An extension. *)
      ex = extension
        { Pmod_extension ex }
    )
    { $1 }
;

(* A parenthesized module expression is a module expression that begins
   and ends with parentheses. *)

paren_module_expr:
    (* A module expression annotated with a module type. *)
    LPAREN me = module_expr mty_mm = module_constraint RPAREN
      { let mty, mm = mty_mm in
        mkmod ~loc:$sloc (Pmod_constraint(me, mty, mm)) }
  | LPAREN module_expr COLON module_type error
      { unclosed "(" $loc($1) ")" $loc($5) }
  | (* A module expression within parentheses. *)
    LPAREN me = module_expr RPAREN
      { mkmod ~loc:$sloc (Pmod_parens me)  }
  | LPAREN module_expr error
      { unclosed "(" $loc($1) ")" $loc($3) }
  | (* A core language expression that produces a first-class module.
       This expression can be annotated in various ways. *)
    LPAREN VAL attrs = attributes e = expr_colon_package_type RPAREN
      { let e, ty1, ty2 = e in
        mkmod ~loc:$sloc ~attrs (Pmod_unpack (e, ty1, ty2)) }
  | LPAREN VAL attributes expr COLON error
      { unclosed "(" $loc($1) ")" $loc($6) }
  | LPAREN VAL attributes expr COLONGREATER error
      { unclosed "(" $loc($1) ")" $loc($6) }
  | LPAREN VAL attributes expr error
      { unclosed "(" $loc($1) ")" $loc($5) }
;

(* The various ways of annotating a core language expression that
   produces a first-class module that we wish to unpack. *)
%inline expr_colon_package_type:
    e = expr
      { e, None, None }
  | e = expr COLON ty = package_type
      { e, Some ty, None }
  | e = expr COLON ty1 = package_type COLONGREATER ty2 = package_type
      { e, Some ty1, Some ty2 }
  | e = expr COLONGREATER ty2 = package_type
      { e, None, Some ty2 }
;

(* A structure, which appears between STRUCT and END (among other places),
   begins with an optional standalone expression, and continues with a list
   of structure elements. *)
structure:
  extra_str(append(
    optional_structure_standalone_expression,
    flatten(structure_element*)
  ))
  { let str = $1 in
    (* See the comments on [signature] for an explanation of the positions
       fiddleing. *)
    let startp, endp =
      match str with
      | [] -> $symbolstartpos, $endpos
      | { pstr_loc = { loc_start = startp; _ }; _ } :: _ ->
        let endp = (List.hd (List.rev str)).pstr_loc.loc_end in
        if $symbolstartpos = $endpos (* empty reduction *)
        then
          (* Just use the synthesized items pos.
             Especially since both $*pos are going to be equal to the $endpos of
             the most recently parsed token. *)
          startp, endp
        else
          (* Here we need to be careful: there might be synthesized str items
             that extend the location past ($symbolstartpos, $endpos).
             But symmetrically there might be symbols (namely SEMISEMI) that
             appeared before the first or after the last of the collected items.
             *)
          min $symbolstartpos startp, max $endpos endp
    in
    str, Tokens.at ~consume_synthesized:false (startp, endp) }
;

(* An optional standalone expression is just an expression with attributes
   (str_exp), with extra wrapping. *)
%inline optional_structure_standalone_expression:
  items = iloption(mark_rhs_docs(text_str(str_exp)))
    { items }
;

(* An expression with attributes, wrapped as a structure item. *)
%inline str_exp:
  e = seq_expr
  attrs = post_item_attributes
    { mkstrexp $sloc e attrs }
;

(* A structure element is one of the following:
   - a double semicolon followed with an optional standalone expression;
   - a structure item. *)
%inline structure_element:
    append(text_str_SEMISEMI, optional_structure_standalone_expression)
  | text_str(structure_item)
      { $1 }
;

(* A structure item. *)
structure_item:
    let_bindings(ext_attributes)
      { val_of_let_bindings ~loc:$sloc $1 }
  | mkstr(
      item_extension post_item_attributes
        { let docs, _sloc = symbol_docs $sloc in
          let ext = Ext.mk $1 $2 ~docs in
          (* FIXME: sloc not used for item tokens! *)
          Pstr_extension ext }
    | floating_attribute
        { Pstr_attribute $1 }
    | kind_abbreviation_decl
        { let name, jkind = $1 in
          Pstr_kind_abbrev (name, jkind)
        }
    | type_declarations
        { pstr_type $1 }
    | rec_module_bindings
        { Pstr_recmodule $1 }
    | module_binding
        { $1 }
    | class_declarations
        { (Pstr_class $1) }
    | class_type_declarations
        { (Pstr_class_type $1) }
    | primitive_declaration
        { pstr_primitive $1 }
    | value_description
        { pstr_primitive $1 }
    | str_type_extension
        { pstr_typext $1 }
    | str_exception_declaration
        { pstr_exception $1 }
    | module_type_declaration
        { (Pstr_modtype $1) }
    | open_declaration
        { (Pstr_open $1) }
    | include_statement(module_expr)
        { (Pstr_include $1) }
    )
    { $1 }
;

(* A single module binding. *)
%inline module_binding:
  MODULE
  ext_attr = ext_attributes
  name = module_name_modal(at_mode_expr)
  body = module_binding_body
  attrs = post_item_attributes
    { let docs, sloc = symbol_docs $sloc in
      let loc = make_loc $sloc in
      let params, mty_opt, modes, me = body in
      let mb =
        Mb.mk name params mty_opt modes me ~ext_attr ~attrs ~loc ~docs
          ~tokens:(Tokens.at sloc)
      in
      Pstr_module mb }
;

%inline module_constraint:
    COLON mty_mm = module_type_with_optional_modes
      { let mty, mm = mty_mm in
        (Some mty, mm)
      }
  | at_mode_expr
      { (None, $1) }

(* The body (right-hand side) of a module binding. *)
module_binding_body:
    EQUAL me = module_expr
      { [], None, [], me }
  | COLON error
      { expecting $loc($1) "=" }
  | mty_mm = module_constraint EQUAL me = module_expr
      {
        let mty, mm = mty_mm in
        [], mty, mm, me }
  | arg = functor_arg body = module_binding_body
      { let args, mty, mm, me = body in
        arg :: args, mty, mm, me }
;

(* A group of recursive module bindings. *)
%inline rec_module_bindings:
  rec_module_binding and_module_binding*
    { $1 :: $2 }
;

(* The first binding in a group of recursive module bindings. *)
%inline rec_module_binding:
  MODULE
  ext_attr = ext_attributes
  REC
  name = module_name_modal(at_mode_expr)
  body = module_binding_body
  attrs = post_item_attributes
  {
    let loc = make_loc $sloc in
    let docs, sloc = symbol_docs $sloc in
    let params, mty_opt, modes, me = body in
    Mb.mk name params mty_opt modes me ~ext_attr ~attrs ~loc ~docs
      ~tokens:(Tokens.at sloc)
  }
;

(* The following bindings in a group of recursive module bindings. *)
%inline and_module_binding:
  AND
  ext_attr = noext_attributes
  name = module_name_modal(at_mode_expr)
  body = module_binding_body
  attrs = post_item_attributes
  {
    let loc = make_loc $sloc in
    let docs, sloc = symbol_docs $sloc in
    (* FIXME: extend sloc with symbol_text! *)
    let text = symbol_text $symbolstartpos in
    let params, mty_opt, modes, me = body in
    Mb.mk name params mty_opt modes me ~ext_attr ~attrs ~loc ~text ~docs
      ~tokens:(Tokens.at sloc)
  }
;

(* -------------------------------------------------------------------------- *)

(* Shared material between structures and signatures. *)

include_kind:
  | INCLUDE %prec below_FUNCTOR
      { Structure }
  | INCLUDE FUNCTOR
      { Functor }
;

(* An [include] statement can appear in a structure or in a signature,
   which is why this definition is parameterized. *)
%inline include_statement(thing):
  kind = include_kind
  ext_attrs = ext_attributes
  thing = thing
  attrs = post_item_attributes
  {
    let loc = make_loc $sloc in
    let docs, sloc = symbol_docs $sloc in
    Incl.mk ~kind ~ext_attrs thing ~attrs ~loc ~docs
      ~tokens:(Tokens.at sloc)
  }
;

(* A module type declaration. *)
module_type_declaration:
  MODULE TYPE
  ext_attrs = ext_attributes
  id = mkrhs(ident)
  typ = preceded(EQUAL, module_type)?
  attrs = post_item_attributes
  {
    let loc = make_loc $sloc in
    let docs, sloc = symbol_docs $sloc in
    Mtd.mk id ?typ ~attrs ~loc ~docs ~ext_attrs
      ~tokens:(Tokens.at sloc)
  }
;

(* -------------------------------------------------------------------------- *)

(* Opens. *)

open_declaration:
  OPEN
  override = override_flag
  ext_attrs = ext_attributes
  me = module_expr
  attrs = post_item_attributes
  {
    let loc = make_loc $sloc in
    let docs, sloc = symbol_docs $sloc in
    Opn.mk me ~override ~attrs ~loc ~docs ~ext_attrs
      ~tokens:(Tokens.at sloc)
  }
;

open_description:
  OPEN
  override = override_flag
  ext_attrs = ext_attributes
  id = mkrhs(mod_ext_longident)
  attrs = post_item_attributes
  {
    let loc = make_loc $sloc in
    let docs, sloc = symbol_docs $sloc in
    Opn.mk id ~override ~attrs ~loc ~docs ~ext_attrs
      ~tokens:(Tokens.at sloc)
  }
;

%inline open_dot_declaration: mkrhs(mod_longident)
  { $1 }
;

(* -------------------------------------------------------------------------- *)

/* Module types */

module_type_atomic:
  | SIG attrs = attributes s = signature END
      { mkmty ~loc:$sloc ~attrs (Pmty_signature s) }
  | SIG attributes signature error
      { unclosed "sig" $loc($1) "end" $loc($4) }
  | STRUCT error
      { expecting $loc($1) "sig" }
  | LPAREN module_type RPAREN
      { mkmty ~loc:$sloc (Pmty_parens $2) }
  | LPAREN module_type error
      { unclosed "(" $loc($1) ")" $loc($3) }
  | mkmty(
      mkrhs(mty_longident)
        { Pmty_ident $1 }
/*  | LPAREN MODULE mkrhs(mod_longident) RPAREN
        { Pmty_alias $3 } */
  )
  { $1 }
;

module_type:
  | module_type_atomic { $1 }
  | FUNCTOR attrs = attributes args = functor_args
    MINUSGREATER mty_mm = module_type_with_optional_modes
      %prec below_WITH
      { let mty, mm = mty_mm in
        mkmty ~loc:$sloc (Pmty_functor (attrs, args, mty, mm)) }
  | args = functor_args
    MINUSGREATER mty_mm = module_type_with_optional_modes
      %prec below_WITH
      { let mty, mm = mty_mm in
        mkmty ~loc:$sloc (Pmty_functor_type (args, mty, mm)) }
  | MODULE TYPE OF attributes module_expr %prec below_LBRACKETAT
      { mkmty ~loc:$sloc (Pmty_typeof ($4, $5)) }
  | mty = module_type attr = attribute
      { Mty.attr mty (attr, $loc(attr)) }
  | mkmty(
      module_type_with_optional_modes MINUSGREATER module_type_with_optional_modes
        %prec below_WITH
        { let mty0, mm0 = $1 in
          let mty1, mm1 = $3 in
          Pmty_functor_type([Unnamed (mty0, mm0)], mty1, mm1) }
    | module_type WITH separated_nonempty_llist(AND, with_constraint)
        { Pmty_with($1, $3) }
    | extension
        { Pmty_extension $1 }
    | module_type WITH mkrhs(mod_ext_longident)
        { Pmty_strengthen ($1, $3) }
    )
    { $1 }
;

%inline module_type_with_optional_modes:
  | module_type { $1, [] }
  | module_type_atomic at_mode_expr { $1, $2 }

(* A signature, which appears between SIG and END (among other places),
   is a list of signature elements. *)
signature:
  optional_atat_modalities_expr extra_sig(flatten(signature_element*))
    { let empty_red = $symbolstartpos = $endpos in
      let startp =
        (* The start position is important to retrieve all the tokens.
           Usually $symbolstartpos is the right choice, however here
           we might reduce an empty production (no modalities, no signature
           element) while returning an non-empty signature (the extra_sig part).

          extra_sig doesn't match any symbol, it synthesizes signature items
          from a side table of docstrings. So $symbolstartpos doesn't know about
          that and would reduce to $endpos. *)
        match $1, $2 with
        | [], item :: _ ->
            if empty_red
            then item.psig_loc.loc_start
            else
              (* There might be ;; before the first element *)
              min item.psig_loc.loc_start $symbolstartpos
        | _ -> $symbolstartpos
      in
      let endp =
        (* Likewise for the end position: some elements might have been
           synthesized that extend the location past $endpos. *)
        match List.rev $2 with
        | item :: _ ->
            if empty_red
            then item.psig_loc.loc_end
            else max item.psig_loc.loc_end $endpos
        | _ -> $endpos
      in
      let loc = startp, endp in
      { psg_modalities = $1;
        psg_items = $2;
        psg_loc = make_loc loc;
        psg_tokens = Tokens.at ~consume_synthesized:false loc } }
;

(* A signature element is one of the following:
   - a double semicolon;
   - a signature item. *)
%inline signature_element:
    text_sig_SEMISEMI
  | text_sig(signature_item)
      { $1 }
;

(* A signature item. *)
signature_item:
  | item_extension post_item_attributes
      { let docs, sloc = symbol_docs $sloc in
        let ext = Ext.mk $1 $2 ~docs in
        mksig ~loc:sloc (Psig_extension ext) }
  | mksig(
      floating_attribute
        { Psig_attribute $1 }
     | kind_abbreviation_decl
        { let name, jkind = $1 in
          Psig_kind_abbrev (name, jkind)
        }
    | type_declarations
        { psig_type $1 }
    | type_subst_declarations
        { psig_typesubst $1 }
    | class_descriptions
        { (Psig_class $1) }
    | class_type_declarations
        { (Psig_class_type $1) }
    | value_description
        { psig_value $1 }
    | primitive_declaration
        { psig_value $1 }
    | sig_type_extension
        { psig_typext $1 }
    | sig_exception_declaration
        { psig_exception $1 }
    | module_declaration
        { (Psig_module $1) }
    | module_alias
        { (Psig_module $1) }
    | module_subst
        { (Psig_modsubst $1) }
    | rec_module_declarations
        { (Psig_recmodule $1) }
    | module_type_declaration
        { (Psig_modtype $1) }
    | module_type_subst
        { (Psig_modtypesubst $1) }
    | open_description
        { (Psig_open $1) }
    | include_statement(module_type) modalities = optional_atat_modalities_expr
      { (Psig_include ($1, modalities)) }
    )
    { $1 }

(* A module declaration. *)
%inline module_declaration:
  MODULE
  ext_attrs = ext_attributes
  name = module_name_modal(atat_modalities_expr)
  body = module_declaration_body(
    module_type optional_atat_modalities_expr { ($1, $2) }
  )
  attrs = post_item_attributes
  {
    let loc = make_loc $sloc in
    let docs, sloc = symbol_docs $sloc in
    let body =
      match body with
      | `Noparams (mty, modalities) -> Without_params (mty, modalities)
      | `Params (args, mty, modes) -> With_params (args, mty, modes)
    in
    Md.mk name body ~attrs ~loc ~docs ~ext_attrs
      ~tokens:(Tokens.at sloc)
  }
;

(* The body (right-hand side) of a module declaration. *)
module_declaration_body(module_type_with_optional_modal_expr):
    COLON mty_mm = module_type_with_optional_modal_expr
      { `Noparams mty_mm }
  | EQUAL error
      { expecting $loc($1) ":" }
  | arg = functor_arg body = module_declaration_body(module_type_with_optional_modes)
      { match body with
        | `Noparams (mty, modes) -> `Params ([arg], mty, modes)
        | `Params (args, mty, modes) -> `Params (arg :: args, mty, modes) }
;

(* A module alias declaration (in a signature). *)
%inline module_alias:
  MODULE
  ext_attrs = ext_attributes
  name = module_name_modal(atat_modalities_expr)
  EQUAL
  alias = module_expr_alias
  modalities = optional_atat_modalities_expr
  attrs = post_item_attributes
  {
    let loc = make_loc $sloc in
    let docs, sloc = symbol_docs $sloc in
    let body = Without_params (alias, modalities) in
    Md.mk name body ~attrs ~loc ~docs ~ext_attrs
      ~tokens:(Tokens.at sloc)
  }
;
%inline module_expr_alias:
  id = mkrhs(mod_longident)
    { Mty.alias ~tokens:(Tokens.at $sloc) ~loc:(make_loc $sloc) id }
;
(* A module substitution (in a signature). *)
module_subst:
  MODULE
  ext_attrs = ext_attributes
  uid = mkrhs(UIDENT)
  COLONEQUAL
  body = mkrhs(mod_ext_longident)
  attrs = post_item_attributes
  {
    let loc = make_loc $sloc in
    let docs, sloc = symbol_docs $sloc in
    Ms.mk uid body ~attrs ~loc ~docs ~ext_attrs
      ~tokens:(Tokens.at sloc)
  }
| MODULE ext attributes mkrhs(UIDENT) COLONEQUAL error
    { expecting $loc($6) "module path" }
;

(* A group of recursive module declarations. *)
%inline rec_module_declarations:
  rec_module_declaration and_module_declaration*
    { $1 :: $2 }
;
%inline rec_module_declaration:
  MODULE
  ext_attrs = ext_attributes
  REC
  name = mkrhs(module_name)
  COLON
  mty = module_type
  modalities = optional_atat_modalities_expr
  attrs = post_item_attributes
  {
    let loc = make_loc $sloc in
    let docs, sloc = symbol_docs $sloc in
    let body = Without_params (mty, modalities) in
    Md.mk ~ext_attrs (name, []) body ~attrs ~loc ~docs
      ~tokens:(Tokens.at sloc)
  }
;
%inline and_module_declaration:
  AND
  ext_attrs = noext_attributes
  name = mkrhs(module_name)
  COLON
  mty = module_type
  modalities = optional_atat_modalities_expr
  attrs = post_item_attributes
  {
    let docs, sloc = symbol_docs $sloc in
    let loc = make_loc $sloc in
    (* FIXME: extend sloc with text! *)
    let text = symbol_text $symbolstartpos in
    let body = Without_params (mty, modalities) in
    Md.mk ~ext_attrs (name, []) body ~attrs ~loc ~text ~docs
      ~tokens:(Tokens.at sloc)
  }
;

(* A module type substitution *)
module_type_subst:
  MODULE TYPE
  ext_attrs = ext_attributes
  id = mkrhs(ident)
  COLONEQUAL
  typ=module_type
  attrs = post_item_attributes
  {
    let loc = make_loc $sloc in
    let docs, sloc = symbol_docs $sloc in
    Mtd.mk id ~typ ~attrs ~loc ~docs ~ext_attrs
      ~tokens:(Tokens.at sloc)
  }


(* -------------------------------------------------------------------------- *)

(* Class declarations. *)

%inline class_declarations:
  class_declaration and_class_declaration*
    { $1 :: $2 }
;
%inline class_declaration:
  CLASS
  ext_attr = ext_attributes
  virt = virtual_flag
  params = formal_class_parameters
  id = mkrhs(LIDENT)
  body = class_fun_binding
  attrs = post_item_attributes
  {
    let (value_params, constraint_, body) = body in
    let loc = make_loc $sloc in
    let docs, sloc = symbol_docs $sloc in
    Ci.mk id body ~ext_attr ~virt ~params ~value_params ?constraint_ ~attrs ~loc
      ~docs ~tokens:(Tokens.at sloc)
  }
;
%inline and_class_declaration:
  AND
  ext_attr = noext_attributes
  virt = virtual_flag
  params = formal_class_parameters
  id = mkrhs(LIDENT)
  body = class_fun_binding
  attrs = post_item_attributes
  {
    let (value_params, constraint_, body) = body in
    let loc = make_loc $sloc in
    let docs, sloc = symbol_docs $sloc in
    let text = symbol_text $symbolstartpos in
    Ci.mk id body ~virt ~params ~value_params ?constraint_ ~ext_attr ~attrs ~loc
      ~text ~docs ~tokens:(Tokens.at sloc)
  }
;

class_fun_binding:
    EQUAL class_expr
      { [], None, $2 }
  | COLON class_type EQUAL class_expr
      { [], Some $2, $4 }
  | labeled_simple_pattern class_fun_binding
      { let params, cstr, ce = $2 in
        $1 :: params, cstr, ce }
;

formal_class_parameters:
  params = class_parameters(type_parameter)
    { params }
;

(* -------------------------------------------------------------------------- *)

(* Class expressions. *)

class_expr:
    class_simple_expr
      { $1 }
  | FUN noext_attributes class_fun_def
      { let (args, body) = $3 in
        Cl.fun_ ~loc:(make_loc $sloc) ~ext_attrs:$2 args body }
  | let_bindings(noext_attributes) IN class_expr
      { class_of_let_bindings ~loc:$sloc $1 $3 }
  | LET OPEN override_flag noext_attributes mkrhs(mod_longident) IN class_expr
      { let loc = ($startpos($2), $endpos($5)) in
        let od =
          Opn.mk ~ext_attrs:$4 ~override:$3 ~loc:(make_loc loc) ~tokens:(Tokens.at loc) $5
        in
        mkclass ~loc:$sloc (Pcl_open(od, $7)) }
  | class_expr attribute
      { Cl.attr $1 ($2, $loc($2)) }
  | mkclass(
      class_simple_expr nonempty_llist(labeled_simple_expr)
        { Pcl_apply($1, $2) }
    | extension
        { Pcl_extension $1 }
    ) { $1 }
;
class_simple_expr:
  | mkclass(
      LPAREN class_expr RPAREN
        { Pcl_parens $2 }
    | LPAREN class_expr error
        { unclosed "(" $loc($1) ")" $loc($3) }
    | tys = actual_class_parameters cid = mkrhs(class_longident)
        { Pcl_constr(cid, tys) }
    | OBJECT attributes class_structure error
        { unclosed "object" $loc($1) "end" $loc($4) }
    | LPAREN class_expr COLON class_type RPAREN
        { Pcl_constraint($2, $4) }
    | LPAREN class_expr COLON class_type error
        { unclosed "(" $loc($1) ")" $loc($5) }
    ) { $1 }
  | OBJECT noext_attributes class_structure END
    { mkclass ~loc:$sloc ~ext_attrs:$2 (Pcl_structure $3) }
;

class_fun_def:
    labeled_simple_pattern MINUSGREATER e = class_expr { [$1], e }
  | labeled_simple_pattern e = class_fun_def
      { let args, body = e in
        $1 :: args, body }
;
%inline class_structure:
  |  class_self_pattern extra_cstr(class_fields)
       { Cstr.mk $1 $2 }
;
class_self_pattern:
    LPAREN pattern RPAREN
      { mkpat ~loc:$sloc (Ppat_parens { pat = $2; optional = false }) }
  | LPAREN pattern COLON core_type RPAREN
      { mkpat_with_modes ~loc:$sloc ~pat:$2 ~cty:(Some $4) ~modes:[] }
  | /* empty */
      { ghpat ~loc:$sloc Ppat_any }
;
%inline class_fields:
  flatten(text_cstr(class_field)*)
    { $1 }
;
class_field:
  | INHERIT override_flag attributes class_expr
    self = preceded(AS, mkrhs(LIDENT))?
    post_item_attributes
      { let docs, sloc = symbol_docs $sloc in
        let attrs =
          ( fst $3 @ fst $6
          , snd $3 @ snd $6 )
        in
        mkcf ~loc:sloc (Pcf_inherit ($2, $4, self)) ~attrs ~docs }
  | VAL value post_item_attributes
      { let v, attrs = $2 in
        let attrs =
          ( fst attrs @ fst $3
          , snd attrs @ snd $3 )
        in
        let docs, sloc = symbol_docs $sloc in
        mkcf ~loc:sloc (Pcf_val v) ~attrs ~docs }
  | METHOD method_ post_item_attributes
      { let meth, attrs = $2 in
        let attrs =
          ( fst attrs @ fst $3
          , snd attrs @ snd $3 )
        in
        let docs, sloc = symbol_docs $sloc in
        mkcf ~loc:sloc (Pcf_method meth) ~attrs ~docs }
  | CONSTRAINT attributes constrain_field post_item_attributes
      { let docs, sloc = symbol_docs $sloc in
        let attrs =
          ( fst $2 @ fst $4
          , snd $2 @ snd $4 )
        in
        mkcf ~loc:sloc (Pcf_constraint $3) ~attrs ~docs }
  | INITIALIZER attributes seq_expr post_item_attributes
      { let docs, sloc = symbol_docs $sloc in
        let attrs =
          ( fst $2 @ fst $4
          , snd $2 @ snd $4 )
        in
        mkcf ~loc:sloc (Pcf_initializer $3) ~attrs ~docs }
  | item_extension post_item_attributes
      { let docs, sloc = symbol_docs $sloc in
        mkcf ~loc:sloc (Pcf_extension $1) ~attrs:$2 ~docs }
  | mkcf(floating_attribute
      { Pcf_attribute $1 })
      { $1 }
;
value:
    no_override_flag
    attrs = attributes
    mutable_ = virtual_with_mutable_flag
    label = mkrhs(label) COLON ty = core_type
      { (label, mutable_, Cfk_virtual ty), attrs }
  | override_flag noext_attributes mutable_flag mkrhs(label) EQUAL seq_expr
      { let pat = mkpat ~loc:$loc($4) (Ppat_var (wrap_str $4)) in
        let vb =
          { pvb_pre_text = [];
            pvb_pre_doc = None;
            pvb_ext_attrs = $2;
            pvb_pat = pat;
            pvb_legacy_modes = []; pvb_modes = []; pvb_params = [];
            pvb_constraint = None; pvb_ret_modes = [];
            pvb_expr = Some $6; pvb_loc = make_loc $sloc;
            pvb_attributes = no_attrs (* FIXME? *);
            pvb_post_doc = None;
            pvb_tokens = Tokens.at $sloc; }
        in
        ($4, $3, Cfk_concrete ($1, vb)), no_attrs }
  | override_flag noext_attributes mutable_flag mkrhs(label) type_constraint
    EQUAL seq_expr
      {
        let t =
          match $5 with
          | Pconstraint t ->
             Pvc_constraint { locally_abstract_univars = []; typ=t }
          | Pcoerce (ground, coercion) -> Pvc_coercion { ground; coercion}
        in
        let pat = mkpat ~loc:$loc($4) (Ppat_var (wrap_str $4)) in
        let vb =
          { pvb_pre_text = [];
            pvb_pre_doc = None;
            pvb_ext_attrs = $2;
            pvb_pat = pat;
            pvb_legacy_modes = []; pvb_modes = []; pvb_params = [];
            pvb_constraint = Some t; pvb_ret_modes = [];
            pvb_expr = Some $7; pvb_loc = make_loc $sloc;
            pvb_attributes = no_attrs (* FIXME? *);
            pvb_post_doc = None;
            pvb_tokens = Tokens.at $sloc; }
        in
        ($4, $3, Cfk_concrete ($1, vb)), no_attrs
      }
;
method_:
    no_override_flag
    attrs = attributes
    private_ = virtual_with_private_flag
    label = mkrhs(label) COLON ty = poly_type
      { (label, private_, Cfk_virtual ty), attrs }
  | override_flag noext_attributes private_flag mkrhs(label) strict_binding
      { let params, tc, ret_modes, e = $5 in
        let pat = mkpat ~loc:$loc($4) (Ppat_var (wrap_str $4)) in
        let vb =
          { pvb_pre_text = [];
            pvb_pre_doc = None;
            pvb_ext_attrs = $2;
            pvb_pat = pat;
            pvb_legacy_modes = []; pvb_modes = []; pvb_params = params;
            pvb_constraint = tc; pvb_ret_modes = ret_modes;
            pvb_expr = Some e;
            pvb_loc = make_loc $sloc;
            pvb_attributes = no_attrs (* FIXME: ? *);
            pvb_post_doc = None;
            pvb_tokens = Tokens.at $sloc; }
        in
        ($4, $3,
        Cfk_concrete ($1, vb)), no_attrs }
  | override_flag noext_attributes private_flag mkrhs(label)
    COLON poly_type EQUAL seq_expr
      { let constr =
          Pvc_constraint { locally_abstract_univars = []; typ = $6 }
        in
        let pat = mkpat ~loc:$loc($4) (Ppat_var (wrap_str $4)) in
        let vb =
          { pvb_pre_text = [];
            pvb_pre_doc = None;
            pvb_ext_attrs = $2;
            pvb_pat = pat;
            pvb_legacy_modes = []; pvb_modes = []; pvb_params = [];
            pvb_constraint = Some constr; pvb_ret_modes = [];
            pvb_expr = Some $8;
            pvb_loc = make_loc $sloc;
            pvb_attributes = no_attrs (* FIXME: ? *);
            pvb_post_doc = None;
            pvb_tokens = Tokens.at $sloc; }
        in
        ($4, $3, Cfk_concrete ($1, vb)), no_attrs }
  | override_flag noext_attributes private_flag mkrhs(label) COLON TYPE newtypes
    DOT core_type EQUAL seq_expr
      { let constr =
          Pvc_constraint { locally_abstract_univars = $7; typ = $9 }
        in
        let pat = mkpat ~loc:$loc($4) (Ppat_var (wrap_str $4)) in
        let vb =
          { pvb_pre_text = [];
            pvb_pre_doc = None;
            pvb_ext_attrs = $2;
            pvb_pat = pat;
            pvb_legacy_modes = []; pvb_modes = []; pvb_params = [];
            pvb_constraint = Some constr; pvb_ret_modes = [];
            pvb_expr = Some $11;
            pvb_loc = make_loc $sloc;
            pvb_attributes = no_attrs (* FIXME: ? *);
            pvb_post_doc = None;
            pvb_tokens = Tokens.at $sloc; }
        in
        ($4, $3, Cfk_concrete ($1, vb)), no_attrs }
;

/* Class types */

class_type:
    class_signature
      { $1 }
  | mkcty(
      label = arg_label
      domain = tuple_type
      MINUSGREATER
      codomain = class_type
        { let domain =
            mk_arrow_arg ~loc:($symbolstartpos, $endpos(domain)) label [] domain []
          in
          Pcty_arrow(domain, codomain) }
    ) { $1 }
 ;
class_signature:
    mkcty(
      tys = actual_class_parameters cid = mkrhs(clty_longident)
        { Pcty_constr (cid, tys) }
    | extension
        { Pcty_extension $1 }
    ) { $1 }
  | OBJECT attributes class_sig_body END
      { mkcty ~loc:$sloc ~attrs:$2 (Pcty_signature $3) }
  | OBJECT attributes class_sig_body error
      { unclosed "object" $loc($1) "end" $loc($4) }
  | cs=class_signature attribute
      { Cty.attr cs ($2, $loc($2)) }
  | LET OPEN override_flag noext_attributes mkrhs(mod_longident) IN class_signature
      { let loc = ($startpos($2), $endpos($5)) in
        let od =
          Opn.mk ~override:$3 ~ext_attrs:$4 ~loc:(make_loc loc)
            ~tokens:(Tokens.at loc) $5
        in
        mkcty ~loc:$sloc (Pcty_open(od, $7)) }
;
%inline class_parameters(parameter):
  | /* empty */
      { [] }
  | LBRACKET params = separated_nonempty_llist(COMMA, parameter) RBRACKET
      { params }
;
%inline actual_class_parameters:
  tys = class_parameters(core_type)
    { tys }
;
%inline class_sig_body:
    class_self_type extra_csig(class_sig_fields)
      { Csig.mk $1 $2 }
;
class_self_type:
    LPAREN core_type RPAREN
      { Some $2 }
  | (* empty *) { None }
;
%inline class_sig_fields:
  flatten(text_csig(class_sig_field)*)
    { $1 }
;
class_sig_field:
    INHERIT attributes class_signature post_item_attributes
      { let docs, sloc = symbol_docs $sloc in
        let attrs =
          ( fst $2 @ fst $4
          , snd $2 @ snd $4 )
        in
        mkctf ~loc:sloc (Pctf_inherit $3) ~attrs ~docs }
  | VAL attributes value_type post_item_attributes
      { let docs, sloc = symbol_docs $sloc in
        let attrs =
          ( fst $2 @ fst $4
          , snd $2 @ snd $4 )
        in
        mkctf ~loc:sloc (Pctf_val $3) ~attrs ~docs }
  | METHOD attributes private_virtual_flags mkrhs(label) COLON poly_type
    post_item_attributes
      { let (p, v) = $3 in
        let docs, sloc = symbol_docs $sloc in
        let attrs =
          ( fst $2 @ fst $7
          , snd $2 @ snd $7 )
        in
        mkctf ~loc:sloc (Pctf_method ($4, p, v, $6)) ~attrs ~docs }
  | CONSTRAINT attributes constrain_field post_item_attributes
      { let docs, sloc = symbol_docs $sloc in
        let attrs =
          ( fst $2 @ fst $4
          , snd $2 @ snd $4 )
        in
        mkctf ~loc:sloc (Pctf_constraint $3) ~attrs ~docs }
  | item_extension post_item_attributes
      { let docs, sloc = symbol_docs $sloc in
        mkctf ~loc:sloc (Pctf_extension $1) ~attrs:$2 ~docs }
  | mkctf(floating_attribute
      { Pctf_attribute $1 })
      { $1 }
;
%inline value_type:
  flags = mutable_virtual_flags
  label = mkrhs(label)
  COLON
  ty = core_type
  {
    let mut, virt = flags in
    label, mut, virt, ty
  }
;
%inline constrain:
    core_type EQUAL core_type
    { $1, $3, make_loc $sloc }
;
constrain_field:
  core_type EQUAL core_type
    { $1, $3 }
;
(* A group of class descriptions. *)
%inline class_descriptions:
  class_description and_class_description*
    { $1 :: $2 }
;
%inline class_description:
  CLASS
  ext_attr = ext_attributes
  virt = virtual_flag
  params = formal_class_parameters
  id = mkrhs(LIDENT)
  COLON
  cty = class_type
  attrs = post_item_attributes
    {
      let loc = make_loc $sloc in
      let docs, sloc = symbol_docs $sloc in
      Ci.mk id cty ~virt ~params ~ext_attr ~attrs ~loc ~docs
        ~tokens:(Tokens.at sloc)
    }
;
%inline and_class_description:
  AND
  ext_attr = noext_attributes
  virt = virtual_flag
  params = formal_class_parameters
  id = mkrhs(LIDENT)
  COLON
  cty = class_type
  attrs = post_item_attributes
    {
      let loc = make_loc $sloc in
      let docs, sloc = symbol_docs $sloc in
      let text = symbol_text $symbolstartpos in
      Ci.mk id cty ~virt ~params ~ext_attr ~attrs ~loc ~text ~docs
        ~tokens:(Tokens.at sloc)
    }
;
class_type_declarations:
  class_type_declaration and_class_type_declaration*
    { $1 :: $2 }
;
%inline class_type_declaration:
  CLASS TYPE
  ext_attr = ext_attributes
  virt = virtual_flag
  params = formal_class_parameters
  id = mkrhs(LIDENT)
  EQUAL
  csig = class_signature
  attrs = post_item_attributes
    {
      let loc = make_loc $sloc in
      let docs, sloc = symbol_docs $sloc in
      Ci.mk id csig ~ext_attr ~virt ~params ~attrs ~loc ~docs
        ~tokens:(Tokens.at sloc)
    }
;
%inline and_class_type_declaration:
  AND
  ext_attr = noext_attributes
  virt = virtual_flag
  params = formal_class_parameters
  id = mkrhs(LIDENT)
  EQUAL
  csig = class_signature
  attrs = post_item_attributes
    {
      let loc = make_loc $sloc in
      let docs, sloc = symbol_docs $sloc in
      let text = symbol_text $symbolstartpos in
      Ci.mk id csig ~virt ~params ~ext_attr ~attrs ~loc ~text ~docs
        ~tokens:(Tokens.at sloc)
    }
;

/* Core expressions */

%inline or_function(EXPR):
  | EXPR
      { $1 }
  | maybe_stack (
    FUNCTION ext_attributes match_cases
      { let loc = make_loc $sloc in
        let cases = $3 in
        let body =
          { pfb_desc = Pfunction_cases (cases, $2)
          ; pfb_loc = loc
          ; pfb_tokens = Tokens.at $sloc }
        in
        mkfunction [] empty_body_constraint body ~loc:$sloc
      }
    )
    { $1 }
;

(* [fun_seq_expr] (and [fun_expr]) are legal expression bodies of a function.
   [seq_expr] (and [expr]) are expressions that appear in other contexts
   (e.g. subexpressions of the expression body of a function).
   [fun_seq_expr] can't be a bare [function _ -> ...]. [seq_expr] can.
   This distinction exists because [function _ -> ...] is parsed as a *function
   cases* body of a function, not an expression body. This so functions can be
   parsed with the intended arity.
*)
fun_seq_expr:
  | fun_expr    %prec below_SEMI  { $1 }
  | fun_expr SEMI
    { mkexp ~loc:$sloc (Pexp_seq_empty $1) }
  | mkexp(fun_expr SEMI seq_expr
    { Pexp_sequence($1, $3) })
    { $1 }
  | fun_expr SEMI ext_noattrs seq_expr
    { mkexp_attrs ~loc:$sloc (Pexp_sequence ($1, $4)) $3 }
;
seq_expr:
  | or_function(fun_seq_expr) { $1 }
;

labeled_simple_pattern:
    QUESTION LPAREN label_let_pattern opt_default RPAREN
      { let legacy_modes, lbl, cty, modes = $3 in
        Arg.optional ~tokens:(Tokens.at $sloc) ~legacy_modes
          ?typ_constraint:(Option.map (fun c -> Pconstraint c) cty)
          ~modes ?default:$4 lbl
      }
  | QUESTION label_var
      { Arg.optional ~tokens:(Tokens.at $sloc) $2 }
  | OPTLABEL LPAREN let_pattern opt_default RPAREN
      { let legacy_modes, pat, cty, modes = $3 in
        Arg.optional ~tokens:(Tokens.at $sloc) ~legacy_modes ~maybe_punned:pat
          ?typ_constraint:(Option.map (fun c -> Pconstraint c) cty)
          ?default:$4 ~modes $1
      }
  | OPTLABEL pattern_var
      { Arg.optional ~tokens:(Tokens.at $sloc) ~maybe_punned:$2 $1 }
  | TILDE LPAREN label_let_pattern RPAREN
      { let legacy_modes, lbl, cty, modes = $3 in
        Arg.labelled ~tokens:(Tokens.at $sloc) ~legacy_modes
          ?typ_constraint:(Option.map (fun c -> Pconstraint c) cty)
          ~modes lbl
      }
  | TILDE label_var
      { Arg.labelled ~tokens:(Tokens.at $sloc) $2 }
  | LABEL simple_pattern_extend_modes_or_poly
      { let legacy_modes, pat, cty, modes = $2 in
        Arg.labelled ~tokens:(Tokens.at $sloc) ~legacy_modes ~maybe_punned:pat
          ?typ_constraint:(Option.map (fun c -> Pconstraint c) cty)
          ~modes $1 }
  | simple_pattern_extend_modes_or_poly
      { let legacy_modes, pat, cty, modes = $1 in
        Arg.nolabel ~tokens:(Tokens.at $sloc) ~legacy_modes
          ?typ_constraint:(Option.map (fun c -> Pconstraint c) cty)
          ~modes pat }
;

pattern_var:
  mkpat(
      mkrhs(LIDENT)     { Ppat_var (wrap_str $1) }
    | UNDERSCORE        { Ppat_any }
  ) { $1 }
;

%inline opt_default:
  preceded(EQUAL, seq_expr)?
    { $1 }
;

optional_poly_type_and_modes:
      { None, [] }
  | at_mode_expr
      { None, $1 }
  | COLON cty_mm = poly_type_with_optional_modes
      { let cty, mm = cty_mm in
        Some cty, mm }
;

label_let_pattern:
    modes0 = optional_mode_expr_legacy x = label_var
    cty_modes1 = optional_poly_type_and_modes
      {
        let cty, modes1 = cty_modes1 in
        modes0, x, cty, modes1
      }
;
%inline label_var:
    LIDENT
      { $1 }
;
let_pattern:
    modes0 = optional_mode_expr_legacy pat = pattern
    cty_modes1 = optional_poly_type_and_modes
      {
        let cty, modes1 = cty_modes1 in
        modes0, pat, cty, modes1
      }
;

(* simple_pattern extended with poly_type and modes *)
simple_pattern_extend_modes_or_poly:
    simple_pattern { [], $1, None, [] }
  | LPAREN pattern_with_modes_or_poly RPAREN
    { $2 }

pattern_with_modes_or_poly:
    modes0 = mode_expr_legacy pat = pattern cty_modes1 = optional_poly_type_and_modes
      { let cty, modes1 = cty_modes1 in
        modes0, pat, cty, modes1 }
  | pat = pattern COLON cty_modes = poly_type_with_modes
      {
        let cty, modes = cty_modes in
        [], pat, Some cty, modes
      }
  | pat = pattern modes = at_mode_expr
      {
        [], pat, None, modes
      }
  | pat = pattern COLON cty = strictly_poly_type
      {
        [], pat, Some cty, []
      }
;

%inline indexop_expr(dot, index, right):
  | array=simple_expr d=dot LPAREN i=index RPAREN r=right
    { Pexp_index_op
        { kind = Paren; op = d; seq = array; indices = i; assign=r } }
  | array=simple_expr d=dot LBRACE i=index RBRACE r=right
    { Pexp_index_op
        { kind = Brace; op = d; seq = array; indices = i; assign=r } }
  | array=simple_expr d=dot LBRACKET i=index RBRACKET r=right
    { Pexp_index_op
        { kind = Bracket; op = d; seq = array; indices = i; assign=r } }
;

%inline indexop_error(dot, index):
  | simple_expr dot _p=LPAREN index  _e=error
    { indexop_unclosed_error $loc(_p)  Paren $loc(_e) }
  | simple_expr dot _p=LBRACE index  _e=error
    { indexop_unclosed_error $loc(_p) Brace $loc(_e) }
  | simple_expr dot _p=LBRACKET index  _e=error
    { indexop_unclosed_error $loc(_p) Bracket $loc(_e) }
;

%inline qualified_dotop: ioption(DOT mod_longident {$2}) DOTOP { Some ($1, $2) };

optional_atomic_constraint_:
  | COLON atomic_type {
    { ret_type_constraint = Some (Pconstraint $2)
    ; ret_mode_annotations = []
    }
   }
  | at_mode_expr {
    { ret_type_constraint = None
    ; ret_mode_annotations = $1
    }
  }
  | { empty_body_constraint }

fun_:
    /* Cf #5939: we used to accept (fun p when e0 -> e) */
  | maybe_stack (
    FUN ext_attributes fun_params body_constraint = optional_atomic_constraint_
      MINUSGREATER fun_body
    {  mkfunction $3 body_constraint $6 ~loc:$sloc ~ext_attrs:$2 }
    ) { $1 }

fun_expr:
    simple_expr %prec below_HASH
      { $1 }
  | fun_expr_attrs
      { let desc, attrs = $1 in
        mkexp_attrs ~loc:$sloc desc attrs }
  | LET OPEN override_flag ext_attributes module_expr IN seq_expr
      { (* FIXME: extracted from fun_expr_attrs as the ext_attrs go into the
          open_decl.
          Could we push it into expr_? Should we care? *)
        let open_loc = ($startpos($2), $endpos($5)) in
        let od =
          Opn.mk $5 ~override:$3 ~loc:(make_loc open_loc)
            ~ext_attrs:$4
            ~tokens:(Tokens.at open_loc)
        in
        mkexp ~loc:$sloc (Pexp_let_open(od, $7)) }
  | LET MODULE ext_attributes module_name_modal(at_mode_expr) module_binding_body IN seq_expr
      { (* FIXME: as above *)
        let params, mty_opt, modes, me = $5 in
        let mb_loc = ($startpos($2), $endpos($5)) in
        (* N.B. we attach the ext_attrs to the module binding instead of the
           expr as wrt. the tokens they're part of the module binding. *)
        let mb =
          Mb.mk $4 params mty_opt modes me ~ext_attr:$3 ~loc:(make_loc mb_loc)
            ~tokens:(Tokens.at mb_loc)
        in
        mkexp ~loc:$sloc (Pexp_letmodule(mb, $7)) }
  | fun_
      { $1 }
  | expr_
      { $1 }
  | let_bindings(ext_attributes) IN seq_expr
      { expr_of_let_bindings ~loc:$sloc $1 $3 }
  | pbop_op = mkrhs(LETOP) bindings = letop_bindings IN body = seq_expr
      { let (pbop_binding, rev_ands) = bindings in
        let ands = List.rev rev_ands in
        let pbop_loc = make_loc $sloc in
        let let_ = {pbop_op; pbop_binding; pbop_loc} in
        mkexp ~loc:$sloc (Pexp_letop{ let_; ands; body}) }
  | fun_expr COLONCOLON expr
      { mkexp_cons ~loc:$sloc $loc($2) $1 $3 }
  | mkrhs(label) LESSMINUS expr
      { mkexp ~loc:$sloc (Pexp_setvar($1, $3)) }
  | simple_expr DOT mkrhs(label_longident) LESSMINUS expr
      { mkexp ~loc:$sloc (Pexp_setfield($1, $3, $5)) }
  | indexop_expr(DOT { None }, seq_expr { [$1] }, LESSMINUS v=expr {Some v})
    { mkexp ~loc:$sloc $1 }
  | indexop_expr(qualified_dotop, expr_semi_list, LESSMINUS v=expr {Some v})
    { mkexp ~loc:$sloc $1 }
  | exp = fun_expr attribute
      { Exp.attr exp ($2, $loc($2)) }
  | mode=mode_legacy exp=seq_expr
     { mkexp ~loc:$sloc (Pexp_mode_legacy (mode, exp)) }
  | EXCLAVE seq_expr
     { mkexp ~loc:$sloc (Pexp_exclave $2) }
;
%inline expr:
  | or_function(fun_expr) { $1 }
;
%inline fun_expr_attrs:
  | LET EXCEPTION ext_attributes let_exception_declaration IN seq_expr
      { Pexp_letexception($4, $6), $3 }
  | MATCH ext_attributes seq_expr WITH match_cases
      { Pexp_match($3, $5), $2 }
  | TRY ext_attributes seq_expr WITH match_cases
      { Pexp_try($3, $5), $2 }
  | TRY ext_attributes seq_expr WITH error
      { syntax_error() }
  | OVERWRITE ext_attributes seq_expr WITH expr
      { Pexp_overwrite($3, $5), $2 }
  | IF ext_attributes seq_expr THEN expr ELSE expr
      { Pexp_ifthenelse($3, $5, Some $7), $2 }
  | IF ext_attributes seq_expr THEN expr
      { Pexp_ifthenelse($3, $5, None), $2 }
  | WHILE ext_attributes seq_expr do_done_expr
      { Pexp_while($3, $4), $2 }
  | FOR ext_attributes pattern EQUAL seq_expr direction_flag seq_expr
    do_done_expr
      { Pexp_for($3, $5, $7, $6, $8), $2 }
  | ASSERT ext_attributes simple_expr %prec below_HASH
      { Pexp_assert $3, $2 }
  | LAZY ext_attributes simple_expr %prec below_HASH
      { Pexp_lazy $3, $2 }
  | subtractive expr %prec prec_unary_minus
      { mkuminus ~oploc:$loc($1) $1 $2 }
  | additive expr %prec prec_unary_plus
      { mkuplus ~oploc:$loc($1) $1 $2 }
;
%inline do_done_expr:
  | DO e = seq_expr DONE
      { e }
  | DO seq_expr error
      { unclosed "do" $loc($1) "done" $loc($2) }
;
%inline expr_:
  | simple_expr nonempty_llist(labeled_simple_expr)
      { mkexp ~loc:$sloc (Pexp_apply($1, $2)) }
  | stack(simple_expr) %prec below_HASH { $1 }
  | labeled_tuple %prec below_COMMA
      { mkexp ~loc:$sloc (Pexp_tuple $1) }
  | maybe_stack (
    mkrhs(constr_longident) simple_expr %prec below_HASH
      { mkexp ~loc:$sloc (Pexp_construct($1, Some $2)) }
    ) { $1 }
  | name_tag simple_expr %prec below_HASH
      { mkexp ~loc:$sloc (Pexp_variant($1, Some $2)) }
  | e1 = fun_expr op = op(infix_operator) e2 = expr
      { mkexp ~loc:$sloc (mkinfix e1 op e2) }
;

unboxed_access:
  | DOTHASH mkrhs(label_longident)
      { Uaccess_unboxed_field $2 }
;

spliceable_expr:
  | LESSLBRACKET seq_expr RBRACKETGREATER
      { mkexp ~loc:$sloc (Pexp_quote ($2)) }
  | LPAREN seq_expr RPAREN
      { mkexp ~loc:$sloc (Pexp_parens { exp = $2; optional = false }) }
  | LPAREN seq_expr error
      { unclosed "(" $loc($1) ")" $loc($3) }
  | LPAREN seq_expr type_constraint_with_modes RPAREN
      { let (t, m) = $3 in
        mkexp_type_constraint_with_modes ~ghost:true ~loc:$sloc ~modes:m $2 t }
  | mkrhs(val_longident)
      { mkexp ~loc:$sloc (Pexp_ident ($1)) }
  | error
      { unspliceable $sloc }
;

simple_expr:
  | LPAREN seq_expr RPAREN
      { mkexp ~loc:$sloc (Pexp_parens { exp = $2; optional = false }) }
  | LPAREN seq_expr error
      { unclosed "(" $loc($1) ")" $loc($3) }
  | LPAREN seq_expr type_constraint_with_modes RPAREN
      { let (t, m) = $3 in
        mkexp_type_constraint_with_modes ~ghost:true ~loc:$sloc ~modes:m $2 t }
  | indexop_expr(DOT { None }, seq_expr { [$1] }, { None })
      { mkexp ~loc:$sloc $1 }
  (* Immutable array indexing is a regular operator, so it doesn't need its own
     rule and is handled by the next case *)
  | indexop_expr(qualified_dotop, expr_semi_list, { None })
      { mkexp ~loc:$sloc $1 }
  | indexop_error (DOT, seq_expr) { $1 }
  | indexop_error (qualified_dotop, expr_semi_list) { $1 }
  | simple_expr_attrs
    { let desc, attrs = $1 in
      mkexp_attrs ~loc:$sloc desc attrs }
  | mkexp(simple_expr_)
      { $1 }
  (* Jane Syntax. These rules create [expression] instead of [expression_desc]
     because Jane Syntax can use attributes as part of their encoding.
  *)
  | array_exprs(LBRACKETCOLON, COLONRBRACKET)
      { Generic_array.Expression.to_expression
          "[:" ":]"
          ~loc:$sloc
          Immutable
          $1
      }
  | constant { mkexp ~loc:$sloc (Pexp_constant $1) }
  | comprehension_expr { $1 }
;
%inline simple_expr_attrs:
  | BEGIN ext_attributes e = seq_expr END
    { Pexp_begin_end (Some e), $2 }
  | BEGIN ext_attributes END
      { Pexp_begin_end None, $2 }
  | BEGIN ext_attributes seq_expr error
      { unclosed "begin" $loc($1) "end" $loc($4) }
  | NEW ext_attributes mkrhs(class_longident)
      { Pexp_new($3), $2 }
  | LPAREN MODULE ext_attributes module_expr RPAREN
      { Pexp_pack ($4, None), $3 }
  | LPAREN MODULE ext_attributes module_expr COLON package_type RPAREN
      { Pexp_pack ($4, Some $6), $3 }
  | LPAREN MODULE ext_attributes module_expr COLON error
      { unclosed "(" $loc($1) ")" $loc($6) }
  | OBJECT ext_attributes class_structure END
      { Pexp_object $3, $2 }
  | OBJECT ext_attributes class_structure error
      { unclosed "object" $loc($1) "end" $loc($4) }
;

comprehension_iterator:
  | EQUAL expr direction_flag expr
      { Pcomp_range { start = $2 ; stop = $4 ; direction = $3 } }
  | IN expr
      { Pcomp_in $2 }
;

comprehension_clause_binding:
  | attributes pattern comprehension_iterator
      { { pcomp_cb_mode = None; pcomp_cb_pattern = $2 ; pcomp_cb_iterator = $3 ; pcomp_cb_attributes = $1 } }
;

comprehension_clause:
  | FOR separated_nonempty_llist(AND, comprehension_clause_binding)
      { Pcomp_for $2 }
  | WHEN expr
      { Pcomp_when $2 }

%inline comprehension(lbracket, rbracket):
  lbracket expr nonempty_llist(comprehension_clause) rbracket
    { { pcomp_body = $2; pcomp_clauses = $3 } }
;

%inline comprehension_ext_expr:
  | comprehension(LBRACKET,RBRACKET)
      { Pcomp_list_comprehension  $1 }
  | comprehension(LBRACKETBAR,BARRBRACKET)
      { Pcomp_array_comprehension (Mutable, $1) }
  | comprehension(LBRACKETCOLON,COLONRBRACKET)
      { Pcomp_array_comprehension (Immutable, $1) }
;

%inline comprehension_expr:
  comprehension_ext_expr
    { mkexp ~loc:$sloc (Pexp_comprehension $1) }
;

%inline array_simple(ARR_OPEN, ARR_CLOSE, contents_semi_list):
  | ARR_OPEN contents_semi_list ARR_CLOSE
      { Generic_array.Simple.Literal $2 }
  | ARR_OPEN contents_semi_list error
      { Generic_array.Simple.Unclosed($loc($1),$loc($3)) }
  | ARR_OPEN ARR_CLOSE
      { Generic_array.Simple.Literal [] }
;

%inline array_exprs(ARR_OPEN, ARR_CLOSE):
  | array_simple(ARR_OPEN, ARR_CLOSE, expr_semi_list)
      { Generic_array.Expression.Simple $1 }
  | od=open_dot_declaration DOT ARR_OPEN expr_semi_list ARR_CLOSE
      { Generic_array.Expression.Opened_literal(od, $startpos($3), $endpos, $4)
      }
  | od=open_dot_declaration DOT ARR_OPEN ARR_CLOSE
      { (* TODO: review the location of Pexp_array *)
        Generic_array.Expression.Opened_literal(od, $startpos($3), $endpos, [])
      }
  | mod_longident DOT
    ARR_OPEN expr_semi_list error
      { Generic_array.Expression.Simple (Unclosed($loc($3), $loc($5))) }
;

%inline array_patterns(ARR_OPEN, ARR_CLOSE):
  | array_simple(ARR_OPEN, ARR_CLOSE, pattern_semi_list)
      { $1 }
;

%inline hash:
  | HASH { () }
  | HASH_SUFFIX { () }
;

%inline indexop_block_access(dot, index):
  | d=dot LPAREN i=index RPAREN
    { d, Paren,   i }
  | d=dot LBRACE i=index RBRACE
    { d, Brace,   i }
  | d=dot LBRACKET i=index RBRACKET
    { d, Bracket, i }
;

block_access:
  | DOT mkrhs(label_longident)
    { Baccess_field $2 }
  | DOT _p=LPAREN i=seq_expr RPAREN
    { Baccess_array (Mutable, Index_int, i) }
  | DOTOP _p=LPAREN i=seq_expr RPAREN
    {
      match $1 with
      | ":" -> Baccess_array (Immutable, Index_int, i)
      | _ -> syntax_error () (* FIXME *)
(*           raise Syntaxerr.(Error(Block_access_bad_paren(make_loc $loc(_p))))
*)
    }
  | DOT ident _p=LPAREN i=seq_expr RPAREN
    {
      match $2 with
      | "L" -> Baccess_array (Mutable, Index_unboxed_int64, i)
      | "l" -> Baccess_array (Mutable, Index_unboxed_int32, i)
      | "S" -> Baccess_array (Mutable, Index_unboxed_int16, i)
      | "s" -> Baccess_array (Mutable, Index_unboxed_int8, i)
      | "n" -> Baccess_array (Mutable, Index_unboxed_nativeint, i)
      | "idx_imm" -> Baccess_block (Immutable, i)
      | "idx_mut" -> Baccess_block (Mutable, i)
      | _ -> syntax_error () (* FIXME *)
(*         raise Syntaxerr.(Error(Block_access_bad_paren(make_loc $loc(_p)))) *)
    }
  | DOTOP ident _p=LPAREN i=seq_expr RPAREN
    {
      match $1, $2 with
      | ":", "L" -> Baccess_array (Immutable, Index_unboxed_int64, i)
      | ":", "l" -> Baccess_array (Immutable, Index_unboxed_int32, i)
      | ":", "S" -> Baccess_array (Immutable, Index_unboxed_int16, i)
      | ":", "s" -> Baccess_array (Immutable, Index_unboxed_int8, i)
      | ":", "n" -> Baccess_array (Immutable, Index_unboxed_nativeint, i)
      | _ -> syntax_error () (* FIXME *)
(*         raise Syntaxerr.(Error(Block_access_bad_paren(make_loc $loc(_p)))) *)
    }
  | DOT ident _p=LPAREN seq_expr _e=error
    { indexop_unclosed_error $loc(_p) Paren $loc(_e) }
;

%inline simple_expr_:
  | mkrhs(val_longident)
      { Pexp_ident ($1) }
  | mkrhs(constr_longident) %prec prec_constant_constructor
      { Pexp_construct($1, None) }
  | name_tag %prec prec_constant_constructor
      { Pexp_variant($1, None) }
  | op(PREFIXOP { Op $1 }) simple_expr
      { Pexp_prefix_apply($1, $2) }
  | op(BANG {Op "!"}) simple_expr
      { Pexp_prefix_apply($1, $2) }
  | LBRACELESS object_expr_content GREATERRBRACE
      { Pexp_override $2 }
  | LBRACELESS object_expr_content error
      { unclosed "{<" $loc($1) ">}" $loc($3) }
  | LBRACELESS GREATERRBRACE
      { Pexp_override [] }
  | simple_expr DOT mkrhs(label_longident)
      { Pexp_field($1, $3) }
  | simple_expr DOTHASH mkrhs(label_longident)
      { Pexp_unboxed_field($1, $3) }
  | LPAREN block_access llist(unboxed_access) RPAREN
      { Pexp_idx ($2, $3) }
  | od=open_dot_declaration DOT LPAREN seq_expr RPAREN
      { let exp =
          let loc = $startpos($3), $endpos($5) in
          mkexp ~loc (Pexp_parens { exp = $4; optional = false })
        in
        Pexp_dot_open(od, exp) }
  | od=open_dot_declaration DOT LBRACELESS object_expr_content GREATERRBRACE
      { let override_loc = $startpos($3), $endpos in
        Pexp_dot_open(od, mkexp ~loc:override_loc (Pexp_override $4)) }
  | mod_longident DOT LBRACELESS object_expr_content error
      { unclosed "{<" $loc($3) ">}" $loc($5) }
  | simple_expr hash mkrhs(label)
      { Pexp_send($1, $3) }
  | simple_expr op(HASHOP { Op $1 }) simple_expr
      { mkinfix $1 $2 $3 }
  | extension
      { Pexp_extension $1 }
  | od=open_dot_declaration DOT mkrhs(LPAREN RPAREN {mkunit $sloc})
      { Pexp_dot_open(od, mkexp ~loc:($loc($3)) (Pexp_construct($3, None))) }
  | mod_longident DOT LPAREN seq_expr error
      { unclosed "(" $loc($3) ")" $loc($5) }
  | LBRACE record_expr_content RBRACE
      { let (exten, fields) = $2 in
        Pexp_record(exten, fields) }
  | HASHLBRACE record_expr_content RBRACE
      { let (exten, fields) = $2 in
        Pexp_record_unboxed_product(exten, fields) }
  | LBRACE record_expr_content error
      { unclosed "{" $loc($1) "}" $loc($3) }
  | od=open_dot_declaration DOT LBRACE record_expr_content RBRACE
      { let (exten, fields) = $4 in
        Pexp_dot_open(od, mkexp ~loc:($startpos($3), $endpos)
                        (Pexp_record(exten, fields))) }
  | mod_longident DOT LBRACE record_expr_content error
      { unclosed "{" $loc($3) "}" $loc($5) }
  | array_exprs(LBRACKETBAR, BARRBRACKET)
      { Generic_array.Expression.to_desc
          "[|" "|]"
          Mutable
          $1
      }
  | LBRACKET expr_semi_list RBRACKET
      { fst (mktailexp $loc($3) $2) }
  | LBRACKET expr_semi_list error
      { unclosed "[" $loc($1) "]" $loc($3) }
  | od=open_dot_declaration DOT comprehension_expr
      { Pexp_dot_open(od, $3) }
  | od=open_dot_declaration DOT LBRACKET expr_semi_list RBRACKET
      { let list_exp =
          (* TODO: review the location of list_exp *)
          let tail_exp, _tail_loc = mktailexp $loc($5) $4 in
          mkexp ~loc:($startpos($3), $endpos) tail_exp in
        Pexp_dot_open(od, list_exp) }
  | od=open_dot_declaration DOT
    mkrhs(mklid(LBRACKET RBRACKET {Lident (Str "[]")}))
      { Pexp_dot_open(od, mkexp ~loc:$loc($3) (Pexp_construct($3, None))) }
  | mod_longident DOT
    LBRACKET expr_semi_list error
      { unclosed "[" $loc($3) "]" $loc($5) }
  | od=open_dot_declaration DOT LPAREN MODULE ext_attributes module_expr COLON
    package_type RPAREN
      { let modexp =
          mkexp_attrs ~loc:($startpos($3), $endpos)
            (Pexp_pack ($6, Some $8)) $5 in
        Pexp_dot_open(od, modexp) }
  | mod_longident DOT
    LPAREN MODULE ext_attributes module_expr COLON error
      { unclosed "(" $loc($3) ")" $loc($8) }
  | HASHLPAREN labeled_tuple RPAREN
      { Pexp_unboxed_tuple $2 }
  | DOLLAR spliceable_expr
      { Pexp_splice $2 }
  | LESSLBRACKET seq_expr RBRACKETGREATER
      { Pexp_quote $2 }
  | LESSLBRACKET seq_expr error
      { unclosed "<[" $loc($1) "]>" $loc($3) }
  | UNDERSCORE
      { Pexp_hole }
;
labeled_simple_expr:
    simple_expr %prec below_HASH
      { Arg.nolabel ~tokens:(Tokens.at $sloc) $1 }
  | LABEL simple_expr %prec below_HASH
      { Arg.labelled ~tokens:(Tokens.at $sloc) $1 ~maybe_punned:$2 }
  | TILDE label = LIDENT
      { Arg.labelled ~tokens:(Tokens.at $sloc) label }
  | TILDE UNDERSCORE
      { Arg.labelled ~tokens:(Tokens.at $sloc) "_" }
  | TILDE LPAREN label = LIDENT c = type_constraint RPAREN
      { Arg.labelled ~tokens:(Tokens.at $sloc) ~typ_constraint:c label }
  | QUESTION label = LIDENT
      { Arg.optional ~tokens:(Tokens.at $sloc) label }
  | QUESTION UNDERSCORE
      { Arg.optional ~tokens:(Tokens.at $sloc) "_" }
  | OPTLABEL simple_expr %prec below_HASH
      { Arg.optional ~tokens:(Tokens.at $sloc) $1 ~maybe_punned:$2 }
;
%inline let_ident:
    val_ident { mkpatvar ~loc:$sloc $1 }
;
%inline pvc_modes:
  | at_mode_expr {None, $1}
  | COLON core_type_with_optional_modes {
      let typ, mm = $2 in
      Some(Pvc_constraint { locally_abstract_univars=[]; typ }), mm
    }
;

%inline empty_list: { [] }

%inline let_ident_with_modes:
    optional_mode_expr_legacy let_ident
      { ($1, $2, []) }
  | LPAREN let_ident at_mode_expr RPAREN
      { ([], $2, $3) }

let_binding_body_no_punning:
    let_ident_with_modes strict_binding_modes
      { let (legacy_modes, v, modes) = $1 in
        let params, tc, ret_modes, e = $2 in
        { lbb_legacy_modes = legacy_modes;
          lbb_modes = modes; lbb_pat = v; lbb_params = params;
          lbb_constraint = tc; lbb_ret_modes = ret_modes; lbb_expr = Some e } }
  | let_ident_with_modes constraint_ EQUAL seq_expr
      (* CR zqian: modes are duplicated, and one of them needs to be made ghost
         to make internal tools happy. We should try to avoid that. *)
      { let legacy_modes, v, modes0 = $1 in (* PR#7344 *)
        let typ, modes1 = $2 in
        let t =
          Option.map (function
          | Pconstraint t ->
             Pvc_constraint { locally_abstract_univars = []; typ=t }
          | Pcoerce (ground, coercion) -> Pvc_coercion { ground; coercion}
          ) typ
        in
        { lbb_legacy_modes = legacy_modes; lbb_modes = modes0;
          lbb_pat = v; lbb_params = []; lbb_constraint = t;
          lbb_ret_modes = modes1; lbb_expr = Some $4 }
      }
  | let_ident_with_modes COLON strictly_poly_type_with_optional_modes EQUAL seq_expr
      { let legacy_modes, v, modes0 = $1 in
        let typ, modes1 = $3 in
        { lbb_legacy_modes = legacy_modes;
          lbb_modes = modes0;
          lbb_pat = v; lbb_params = [];
          lbb_constraint =
            Some (Pvc_constraint { locally_abstract_univars = []; typ });
          lbb_ret_modes = modes1; lbb_expr = Some $5 }
      }
  | let_ident_with_modes COLON TYPE ntys = newtypes DOT cty = core_type modes1=empty_list EQUAL e = seq_expr
  | let_ident_with_modes COLON TYPE ntys = newtypes DOT cty = tuple_type modes1=at_mode_expr EQUAL e = seq_expr
      (* The code upstream looks like:
         {[
           let constraint' =
             Pvc_constraint { locally_abstract_univars=$4; typ = $6}
           in
           ($1, $8, Some constraint')
         ]}

         But this would require adding a jkind field to [newtypes], which will require
         a small amount of additional work.

         The [typloc] argument to [wrap_type_annotation] is used to make the
         location on the [core_type] node for the annotation match the upstream
         version, even though we are creating a slightly different [core_type].
      *)
      (* FIXME: ! *)
      {
        let legacy_modes, v, modes0 = $1 in
        { lbb_legacy_modes = legacy_modes;
          lbb_modes = modes0;
          lbb_pat = v; lbb_params = [];
          lbb_constraint =
            Some (Pvc_constraint { locally_abstract_univars = ntys; typ = cty });
          lbb_ret_modes = modes1; lbb_expr = Some e }
       }
  | pattern_no_exn EQUAL seq_expr
      { { lbb_legacy_modes = []; lbb_modes = []; lbb_pat = $1; lbb_params = [];
          lbb_constraint = None; lbb_ret_modes = []; lbb_expr = Some $3 } }
  | simple_pattern_not_ident pvc_modes EQUAL seq_expr
      {
        let pvc, modes = $2 in
        { lbb_legacy_modes = []; lbb_modes = []; lbb_pat = $1; lbb_params = [];
          lbb_constraint = pvc; lbb_ret_modes = modes; lbb_expr = Some $4 }
      }
;
let_binding_body:
  | let_binding_body_no_punning
      { $1 }
/* BEGIN AVOID */
  | val_ident %prec below_HASH
      { { lbb_legacy_modes = []; lbb_modes = [];
          lbb_pat = mkpatvar ~loc:$loc $1;
          lbb_params = []; lbb_constraint = None; lbb_ret_modes = [];
          lbb_expr = None } }
  (* The production that allows puns is marked so that [make list-parse-errors]
     does not attempt to exploit it. That would be problematic because it
     would then generate bindings such as [let x], which are rejected by the
     auxiliary function [addlb] via a call to [syntax_error]. *)
/* END AVOID */
;
(* The formal parameter EXT_ATTR can be instantiated with ext_attributes or
   noext_attributes so as to indicate whether an extension is allowed or
   disallowed. *)
let_bindings(EXT_ATTR):
    let_binding(EXT_ATTR)                            { $1 }
  | let_bindings(EXT_ATTR) and_let_binding           { addlb $1 $2 }
;
%inline let_binding(EXT_ATTR):
  LET
  ext_attrs = EXT_ATTR
  mutable_flag = mutable_flag
  rec_flag = rec_flag
  body = let_binding_body
  attrs = post_item_attributes
    {
      mklbs mutable_flag rec_flag (mklb ~loc:$sloc true body ext_attrs attrs)
    }
;
and_let_binding:
  AND
  ext_attr = noext_attributes
  body = let_binding_body
  attrs = post_item_attributes
    {
      mklb ~loc:$sloc false body ext_attr attrs
    }
;
letop_binding_body:
    pat = let_ident exp = strict_binding
      { let params, tc, ret_modes, e = exp in
        Vb.mk ~loc:(make_loc $sloc)
          ~tokens:(Tokens.at $sloc)
          ~params ?value_constraint:tc ~ret_modes pat (Some e) }
  | val_ident
      (* Let-punning *)
      { Vb.mk ~loc:(make_loc $sloc)
          ~tokens:(Tokens.at $sloc)
          (mkpatvar ~loc:$loc $1) (* FIXME: ! *)
          None }
  (* CR zqian: support mode annotation on letop. *)
  | pat = simple_pattern COLON typ = core_type EQUAL exp = seq_expr
      { Vb.mk ~loc:(make_loc $sloc)
          ~tokens:(Tokens.at $sloc)
          ~value_constraint:(
            Pvc_constraint { locally_abstract_univars = []; typ })
          pat (Some exp) }
  | pat = pattern_no_exn EQUAL exp = seq_expr
      { Vb.mk ~tokens:(Tokens.at $sloc) ~loc:(make_loc $sloc) pat (Some exp) }
;
letop_bindings:
    body = letop_binding_body
      { body, [] }
  | bindings = letop_bindings pbop_op = mkrhs(ANDOP) body = letop_binding_body
      { let first_vb, rev_ands = bindings in
        let pbop_loc = make_loc ($startpos(pbop_op), $endpos(body)) in
        let and_ = {pbop_op; pbop_binding = body; pbop_loc} in
        first_vb, and_ :: rev_ands }
;
strict_binding_modes:
    EQUAL seq_expr
      { [], None, [], $2 }
  | fun_params constraint_? EQUAL fun_body
    {
      let ret_type_constraint, ret_mode_annotations =
        match $2 with
        | None -> None, []
        | Some (ret_type_constraint, ret_mode_annotations) ->
            ret_type_constraint, ret_mode_annotations
      in
      let typ =
        Option.map (function
          | Pconstraint t ->
             Pvc_constraint { locally_abstract_univars = []; typ=t }
          | Pcoerce (ground, coercion) -> Pvc_coercion { ground; coercion}
        ) ret_type_constraint
      in
      let expr =
        let fb = $4 in
        match fb.pfb_desc with
        | Pfunction_body e -> e
        | Pfunction_cases _ ->
            mkfunction [] empty_body_constraint fb ~loc:$loc($4)
      in
      $1, typ, ret_mode_annotations, expr
    }
;
%inline strict_binding:
  strict_binding_modes
    {$1}
;
fun_body:
  | FUNCTION ext_attributes match_cases
      { { pfb_desc = Pfunction_cases ($3, $2)
        ; pfb_loc = make_loc $sloc
        ; pfb_tokens = Tokens.at $sloc } }
  | fun_seq_expr
      { { pfb_desc = Pfunction_body $1
        ; pfb_loc = make_loc $sloc
        ; pfb_tokens = Tokens.at $sloc } }
;
%inline match_cases:
  xs = bar_llist(match_case)
    { xs }
;
match_case(opening):
    opening pattern MINUSGREATER seq_expr
      { mkcase $sloc $2 $4 }
  | opening pattern WHEN seq_expr MINUSGREATER seq_expr
      { mkcase $sloc $2 ~guard:$4 $6 }
  | opening pattern MINUSGREATER exp_unreachable
      { mkcase $sloc $2 $4 }
;
exp_unreachable:
  | DOT { mkexp ~loc:$sloc Pexp_unreachable }
;
fun_param_as_list:
  | LPAREN TYPE ty_params = newtypes RPAREN
      { [ { pparam_loc = make_loc $sloc;
            pparam_desc = Pparam_newtypes ty_params
          }
        ]
      }
  | LPAREN TYPE mkrhs(LIDENT) COLON jkind_annotation RPAREN
      { [ { pparam_loc = make_loc $sloc;
            pparam_desc = Pparam_newtype ($3, Some $5)
          }
        ]
      }
  | labeled_simple_pattern
      { [ { pparam_loc = make_loc $sloc;
            pparam_desc = Pparam_val $1
          }
        ]
      }
;
fun_params:
  | nonempty_concat(fun_param_as_list) { $1 }
;

(* Parsing labeled tuple expressions

   The grammar we want to parse is something like:

     labeled_tuple_element := expr | ~x:expr | ~x | ~(x:ty)
     labeled_tuple := lt_element [, lt_element]+

   (The last case of [labeled_tuple_element] is a punned label with a type
   constraint, which is allowed for functions, so we allow it here).

   So you might think [labeled_tuple] could therefore just be:

     labeled_tuple :
       separated_nontrivial_llist(COMMA, labeled_tuple_element)

   But this doesn't work:

   - If we don't mark [labeled_tuple_element] %inline, this causes many
     reduce/reduce conflicts (basically just ambiguities) because
     [labeled_tuple_element] trivially reduces to [expr].

   - If we do mark [labeled_tuple_element] %inline, it is not allowed to have
     %prec annotations.  Menhir doesn't permit these on %inline non-terminals
     that are used in non-tail position.

   To get around this, we do mark it inlined, and then because we can only use
   it in tail position it is _manually_ inlined into the occurrences in
   [separated_nontrivial_llist] where it doesn't appear in tail position.  This
   results in [labeled_tuple] and [reversed_labeled_tuple_body] below.  So the
   latter is just a list of comma-separated labeled tuple elements, with length
   at least two, where the first element in the base case is inlined (resulting
   in one base case for each case of [labeled_tuple_element].  *)
%inline labeled_tuple_element :
  | expr
     { Arg.nolabel ~tokens:(Tokens.at $sloc) $1 }
  | LABEL simple_expr %prec below_HASH
     { Arg.labelled ~tokens:(Tokens.at $sloc) $1 ~maybe_punned:$2 }
  | TILDE label = LIDENT
     { Arg.labelled ~tokens:(Tokens.at $sloc) label }
  | TILDE LPAREN label = LIDENT c = type_constraint RPAREN %prec below_HASH
     { Arg.labelled ~tokens:(Tokens.at $sloc) ~typ_constraint:c label }
;

%inline labeled_tuple_element_noprec :
  | expr
     { Arg.nolabel ~tokens:(Tokens.at $sloc) $1 }
  | LABEL simple_expr
     { Arg.labelled ~tokens:(Tokens.at $sloc) $1 ~maybe_punned:$2 }
  | TILDE label = LIDENT
     { Arg.labelled ~tokens:(Tokens.at $sloc) label }
  | TILDE LPAREN label = LIDENT c = type_constraint RPAREN
     { Arg.labelled ~tokens:(Tokens.at $sloc) ~typ_constraint:c label }
;

reversed_labeled_tuple_body:
  (* > 2 elements *)
  xs = reversed_labeled_tuple_body
  COMMA
  x = labeled_tuple_element
    { x :: xs }
  (* base cases (2 elements) *)
| x1 = labeled_tuple_element_noprec
  COMMA
  x2 = labeled_tuple_element
    { [ x2; x1 ] }
;
%inline labeled_tuple:
  xs = rev(reversed_labeled_tuple_body)
    { xs }
;

record_expr_content:
  eo = ioption(terminated(simple_expr, WITH))
  fields = separated_or_terminated_nonempty_list(SEMI, record_expr_field)
    { eo, fields }
;
%inline record_expr_field:
  | label = mkrhs(label_longident)
    c = type_constraint?
    eo = preceded(EQUAL, expr)?
  { { field_name = label;
      value = eo;
      typ = c; } }
;
%inline object_expr_content:
  xs = separated_or_terminated_nonempty_list(SEMI, object_expr_field)
    { xs }
;
%inline object_expr_field:
    label = mkrhs(label)
    oe = preceded(EQUAL, expr)?
      { label, oe }
;
%inline expr_semi_list:
  es = separated_or_terminated_nonempty_list(SEMI, expr)
    { es }
;
type_constraint:
    COLON core_type                             { Pconstraint $2 }
  | COLON core_type COLONGREATER core_type      { Pcoerce (Some $2, $4) }
  | COLONGREATER core_type                      { Pcoerce (None, $2) }
  | COLON error                                 { syntax_error() }
  | COLONGREATER error                          { syntax_error() }
;

%inline type_constraint_with_modes:
  | COLON core_type_with_optional_modes {
    let cty, mm = $2 in
    Pconstraint cty, mm }
  | COLON core_type COLONGREATER core_type_with_optional_modes {
    let cty, mm = $4 in
    Pcoerce (Some $2, cty), mm }
  | COLONGREATER core_type_with_optional_modes {
    let cty, mm = $2 in
    Pcoerce (None, cty), mm }
  | COLON error                                 { syntax_error() }
  | COLONGREATER error                          { syntax_error() }
;

%inline constraint_:
  | type_constraint_with_modes
    { let ty, modes = $1 in
      Some ty, modes }
  | at_mode_expr
    { None, $1 }
;

(* the thing between the [type] and the [.] in
   [let : type <<here>>. 'a -> 'a = ...] *)
newtypes: (* : (string with_loc * jkind_annotation option) list *)
  newtype+
    { $1 }

newtype: (* : string with_loc * jkind_annotation option *)
    mkrhs(LIDENT)                     { $1, None }
  | LPAREN name=mkrhs(LIDENT) COLON jkind=jkind_annotation RPAREN
      { name, Some jkind }

/* Patterns */

(* Whereas [pattern] is an arbitrary pattern, [pattern_no_exn] is a pattern
   that does not begin with the [EXCEPTION] keyword. Thus, [pattern_no_exn]
   is the intersection of the context-free language [pattern] with the
   regular language [^EXCEPTION .*].

   Ideally, we would like to use [pattern] everywhere and check in a later
   phase that EXCEPTION patterns are used only where they are allowed (there
   is code in typing/typecore.ml to this end). Unfortunately, in the
   definition of [let_binding_body], we cannot allow [pattern]. That would
   create a shift/reduce conflict: upon seeing LET EXCEPTION ..., the parser
   wouldn't know whether this is the beginning of a LET EXCEPTION construct or
   the beginning of a LET construct whose pattern happens to begin with
   EXCEPTION. The conflict is avoided there by using [pattern_no_exn] in the
   definition of [let_binding_body].

   In order to avoid duplication between the definitions of [pattern] and
   [pattern_no_exn], we create a parameterized definition [pattern_(self)]
   and instantiate it twice. *)

pattern:
    pattern_(pattern)
      { $1 }
  | EXCEPTION ext_attributes pattern %prec prec_constr_appl
      { mkpat_attrs ~loc:$sloc (Ppat_exception $3) $2}
;

pattern_no_exn:
    pattern_(pattern_no_exn)
      { $1 }
;

%inline pattern_(self):
  | self COLONCOLON pattern
      { mkpat_cons ~loc:$sloc $loc($2) $1 $3 }
  | pat=self attr=attribute
      { Pat.attr pat (attr, $loc(attr)) }
  | pattern_gen
      { $1 }
  | mkpat(
      self AS mkrhs(val_ident)
        { Ppat_alias($1, $3) }
    | self AS error
        { expecting $loc($3) "identifier" }
    | self COLONCOLON error
        { expecting $loc($3) "pattern" }
    | self BAR pattern
        { Ppat_or($1, $3) }
    | self BAR error
        { expecting $loc($3) "pattern" }
  ) { $1 }
  | reversed_labeled_tuple_pattern(self)
      { let closed, pats = $1 in
        mkpat ~loc:$sloc (Ppat_tuple (List.rev pats, closed))
      }
;

(* Parsing labeled tuple patterns

   Here we play essentially the same game we did for expressions - see the
   comment beginning "Parsing labeled tuple expressions".

   One difference is that we would need to manually inline the definition of
   individual elements in two places: Once in the base case for lists 2 or more
   elements, and once in the special case for open patterns with just one
   element (e.g., [~x, ..]).  Rather than manually inlining
   [labeled_tuple_pat_element] twice, we simply define it twice: once with the
   [%prec] annotations needed for its occurrences in tail position, and once
   without them suitable for use in other locations.
*)
%inline labeled_tuple_pat_element(self):
  | self
      { Arg.nolabel ~tokens:(Tokens.at $sloc) $1 }
  | LABEL simple_pattern %prec COMMA
      { Arg.labelled ~tokens:(Tokens.at $sloc) $1 ~maybe_punned:$2 }
  | TILDE label = LIDENT
      { Arg.labelled ~tokens:(Tokens.at $sloc) label }
  | TILDE LPAREN label = LIDENT COLON cty = core_type RPAREN %prec COMMA
      { Arg.labelled ~tokens:(Tokens.at $sloc)
          ~typ_constraint:(Pconstraint cty) label }

(* If changing this, don't forget to change its copy just above. *)
%inline labeled_tuple_pat_element_noprec(self):
  | self
      { Arg.nolabel ~tokens:(Tokens.at $sloc) $1 }
  | LABEL simple_pattern
      { Arg.labelled ~tokens:(Tokens.at $sloc) $1 ~maybe_punned:$2 }
  | TILDE label = LIDENT
      { Arg.labelled ~tokens:(Tokens.at $sloc) label }
  | TILDE LPAREN label = LIDENT COLON cty = core_type RPAREN
      { Arg.labelled ~tokens:(Tokens.at $sloc)
          ~typ_constraint:(Pconstraint cty) label }

labeled_tuple_pat_element_list(self):
  | labeled_tuple_pat_element_list(self) COMMA labeled_tuple_pat_element(self)
      { $3 :: $1 }
  | labeled_tuple_pat_element_noprec(self) COMMA labeled_tuple_pat_element(self)
      { [ $3; $1 ] }
  | self COMMA error
      { expecting $loc($3) "pattern" }
;

reversed_labeled_tuple_pattern(self):
  | labeled_tuple_pat_element_list(self) %prec below_COMMA
      { Closed, $1 }
  | labeled_tuple_pat_element_list(self) COMMA DOTDOT
      { Open, $1 }
  | labeled_tuple_pat_element_noprec(self) COMMA DOTDOT
      { Open, [ $1 ] }

pattern_gen:
    simple_pattern
      { $1 }
  | mkpat(
      mkrhs(constr_longident) pattern %prec prec_constr_appl
        { Ppat_construct($1, Some ([], $2)) }
    | constr=mkrhs(constr_longident) LPAREN TYPE newtypes=newtypes RPAREN
        pat=simple_pattern
        { Ppat_construct(constr, Some (newtypes, pat)) }
    | constr=mkrhs(constr_longident)
        LPAREN TYPE ty=mkrhs(LIDENT) COLON jkind=jkind_annotation RPAREN
        pat=simple_pattern
        { Ppat_construct(constr, Some ([(ty,Some jkind)], pat)) }
    | name_tag pattern %prec prec_constr_appl
        { Ppat_variant($1, Some $2) }
    ) { $1 }
  | LAZY ext_attributes simple_pattern
      { mkpat_attrs ~loc:$sloc (Ppat_lazy $3) $2}
;
simple_pattern:
    mkpat(mkrhs(val_ident) %prec below_EQUAL
      { Ppat_var ($1) })
      { $1 }
  | simple_pattern_not_ident { $1 }
;

simple_pattern_not_ident:
  | LPAREN pattern RPAREN
      { mkpat (Ppat_parens { pat = $2; optional = false }) ~loc:$sloc }
  | simple_delimited_pattern
      { $1 }
  | LPAREN MODULE ext_attributes mkrhs(module_name) RPAREN
      { mkpat_attrs ~loc:$sloc (Ppat_unpack ($4, None)) $3 }
  | LPAREN MODULE ext_attributes mkrhs(module_name) COLON package_type RPAREN
      { mkpat_attrs ~loc:$sloc (Ppat_unpack ($4, Some $6)) $3 }
  | simple_pattern_not_ident_
      { $1 }
  | signed_constant { mkpat (Ppat_constant $1) ~loc:$sloc }
;
%inline simple_pattern_not_ident_:
  mkpat(
    UNDERSCORE
      { Ppat_any }
  | signed_constant DOTDOT signed_constant
      { Ppat_interval ($1, $3) }
  | mkrhs(constr_longident)
      { Ppat_construct($1, None) }
  | name_tag
      { Ppat_variant($1, None) }
  | hash mkrhs(type_longident)
      { Ppat_type ($2) }
  | mkrhs(mod_longident) DOT simple_delimited_pattern
      { Ppat_open($1, $3) }
  | mkrhs(mod_longident) DOT mkrhs(mklid(LBRACKET RBRACKET {Lident (Str "[]")}))
    { Ppat_open($1, mkpat ~loc:$loc($3) (Ppat_construct($3, None))) }
  | mkrhs(mod_longident) DOT mkrhs(mklid(LPAREN RPAREN {Lident (Str "()")}))
    { Ppat_open($1, mkpat ~loc:$loc($3) (Ppat_construct($3, None))) }
  | mkrhs(mod_longident) DOT LPAREN pattern RPAREN
      { let sub =
          let loc = ($startpos($3), $endpos) in
          mkpat ~loc (Ppat_parens { pat = $4; optional = false })
        in
        Ppat_open ($1, sub) }
  | mod_longident DOT LPAREN pattern error
      { unclosed "(" $loc($3) ")" $loc($5)  }
  | mod_longident DOT LPAREN error
      { expecting $loc($4) "pattern" }
  | LPAREN pattern error
      { unclosed "(" $loc($1) ")" $loc($3) }
  | LPAREN pattern COLON core_type error
      { unclosed "(" $loc($1) ")" $loc($5) }
  | LPAREN pattern COLON error
      { expecting $loc($4) "type" }
  | LPAREN MODULE ext_attributes module_name COLON package_type
    error
      { unclosed "(" $loc($1) ")" $loc($7) }
  | extension
      { Ppat_extension $1 }
  ) { $1 }
  | LPAREN pattern COLON core_type RPAREN
    { mkpat_with_modes ~loc:$sloc ~pat:$2 ~cty:(Some $4) ~modes:[] }
;

simple_delimited_pattern:
  mkpat(
      LBRACE record_pat_content RBRACE
      { let (fields, closed) = $2 in
        Ppat_record(fields, closed) }
    | HASHLBRACE record_pat_content RBRACE
      { let (fields, closed) = $2 in
        Ppat_record_unboxed_product(fields, closed) }
    | LBRACE record_pat_content error
      { unclosed "{" $loc($1) "}" $loc($3) }
    | LBRACKET pattern_semi_list RBRACKET
      { fst (mktailpat $loc($3) $2) }
    | LBRACKET pattern_semi_list error
      { unclosed "[" $loc($1) "]" $loc($3) }
    | array_patterns(LBRACKETBAR, BARRBRACKET)
        { Generic_array.Pattern.to_ast
            "[|" "|]"
            Mutable
            $1
        }
    | array_patterns(LBRACKETCOLON, COLONRBRACKET)
        { Generic_array.Pattern.to_ast
            "[:" ":]"
            Immutable
            $1
        }
    | HASHLPAREN reversed_labeled_tuple_pattern(pattern) RPAREN
        { let (closed, fields) = $2 in
          Ppat_unboxed_tuple (List.rev fields, closed) }
  ) { $1 }

%inline pattern_semi_list:
  ps = separated_or_terminated_nonempty_list(SEMI, pattern)
    { ps }
;
(* A label-pattern list is a nonempty list of label-pattern pairs, optionally
   followed with an UNDERSCORE, separated-or-terminated with semicolons. *)
%inline record_pat_content:
  listx(SEMI, record_pat_field, UNDERSCORE)
    { let fields, closed = $1 in
      let closed = match closed with Some () -> Open | None -> Closed in
      fields, closed }
;
%inline record_pat_field:
  label = mkrhs(label_longident)
  octy = preceded(COLON, core_type)?
  opat = preceded(EQUAL, pattern)?
    {
      { field_name = label;
        value = opat;
        typ = Option.map (fun c -> Pconstraint c) octy; } }
;

/* Value descriptions */

value_description:
  VAL
  ext_attrs = ext_attributes
  id = mkrhs(val_ident)
  COLON
  ty = possibly_poly(core_type)
  modalities = optional_atat_modalities_expr
  attrs = post_item_attributes
    { let loc = make_loc $sloc in
      let docs, sloc = symbol_docs $sloc in
      Val.mk id ty ~attrs ~modalities ~loc ~docs ~ext_attrs
        ~tokens:(Tokens.at sloc)
    }
;

/* Primitive declarations */

primitive_declaration:
  EXTERNAL
  ext_attrs = ext_attributes
  id = mkrhs(val_ident)
  COLON
  ty = possibly_poly(core_type)
  modalities = optional_atat_modalities_expr
  EQUAL
  prim = raw_string+
  attrs = post_item_attributes
    { let loc = make_loc $sloc in
      let docs, sloc = symbol_docs $sloc in
      Val.mk id ty ~prim ~attrs ~modalities ~loc ~docs ~ext_attrs
        ~tokens:(Tokens.at sloc)
    }
;

(* Type declarations and type substitutions. *)

(* Type declarations [type t = u] and type substitutions [type t := u] are very
   similar, so we view them as instances of [generic_type_declarations]. In the
   case of a type declaration, the use of [nonrec_flag] means that [NONREC] may
   be absent or present, whereas in the case of a type substitution, the use of
   [no_nonrec_flag] means that [NONREC] must be absent. The use of [type_kind]
   versus [type_subst_kind] means that in the first case, we expect an [EQUAL]
   sign, whereas in the second case, we expect [COLONEQUAL]. *)

%inline type_declarations:
  generic_type_declarations(nonrec_flag, type_kind)
    { $1 }
;

%inline type_subst_declarations:
  generic_type_declarations(no_nonrec_flag, type_subst_kind)
    { $1 }
;

(* A set of type declarations or substitutions begins with a
   [generic_type_declaration] and continues with a possibly empty list of
   [generic_and_type_declaration]s. *)

%inline generic_type_declarations(flag, kind):
  xlist(
    generic_type_declaration(flag, kind),
    generic_and_type_declaration(kind)
  )
  { $1 }
;

(* [generic_type_declaration] and [generic_and_type_declaration] look similar,
   but are in reality different enough that it is difficult to share anything
   between them. *)

generic_type_declaration(flag, kind):
  TYPE
  ext_attr = ext_attributes
  flag = flag
  params = type_parameters
  id = mkrhs(LIDENT)
  jkind_annotation = jkind_constraint?
  kind_priv_manifest = kind
  cstrs = constraints
  attrs = post_item_attributes
    {
      let (kind, priv, manifest) = kind_priv_manifest in
      let docs, sloc = symbol_docs $sloc in
      let loc = make_loc $sloc in
      flag,
      Type.mk id ~ext_attr
        ~params ~cstrs ~kind ~priv ?manifest ~attrs ~loc ~docs
        ?jkind_annotation ~tokens:(Tokens.at sloc)
    }
;
%inline generic_and_type_declaration(kind):
  AND
  ext_attr = noext_attributes
  params = type_parameters
  id = mkrhs(LIDENT)
  jkind_annotation = jkind_constraint?
  kind_priv_manifest = kind
  cstrs = constraints
  attrs = post_item_attributes
    {
      let (kind, priv, manifest) = kind_priv_manifest in
      let loc = make_loc $sloc in
      let docs, sloc = symbol_docs $sloc in
      let text = symbol_text $symbolstartpos in
      Type.mk ~ext_attr id ~params ~cstrs ~kind ~priv ?manifest ~attrs ~loc
        ~docs ~text ?jkind_annotation ~tokens:(Tokens.at sloc)
    }
;
%inline constraints:
  llist(preceded(CONSTRAINT, constrain))
    { $1 }
;
(* Lots of %inline expansion are required for [nonempty_type_kind] to be
   LR(1). At the cost of some manual expansion, it would be possible to give a
   definition that leads to a smaller grammar (after expansion) and therefore
   a smaller automaton. *)
nonempty_type_kind:
  | priv = inline_private_flag
    ty = core_type
      { (Ptype_abstract, priv, Some ty) }
  | oty = type_synonym
    priv = inline_private_flag
    cs = constructor_declarations
      { (Ptype_variant cs, priv, oty) }
  | oty = type_synonym
    priv = inline_private_flag
    DOTDOT
      { (Ptype_open, priv, oty) }
  | oty = type_synonym
    priv = inline_private_flag
    LBRACE ls = label_declarations RBRACE
      { (Ptype_record ls, priv, oty) }
  | oty = type_synonym
    priv = inline_private_flag
    HASHLBRACE ls = label_declarations RBRACE
      { (Ptype_record_unboxed_product ls, priv, oty) }
;
%inline type_synonym:
  ioption(terminated(core_type, EQUAL))
    { $1 }
;
type_kind:
    /*empty*/
      { (Ptype_abstract, Public, None) }
  | EQUAL nonempty_type_kind
      { $2 }
;
%inline type_subst_kind:
    COLONEQUAL nonempty_type_kind
      { $2 }
;
type_parameters:
    /* empty */
      { [] }
  | p = type_parameter
      { [p] }
  | LPAREN
    ps = separated_nonempty_llist(COMMA, parenthesized_type_parameter)
    RPAREN
      { ps }
;

jkind_desc:
    jkind_annotation MOD mkrhs(LIDENT)+ { (* LIDENTs here are for modes *)
      let modes =
        List.map
          (fun {txt; loc} -> {txt = Mode txt; loc})
          $3
      in
      Pjk_mod ($1, modes)
    }
  | jkind_annotation WITH core_type optional_atat_modalities_expr {
      Pjk_with ($1, $3, $4)
    }
  | ident {
      Pjk_abbreviation $1
    }
  | KIND_OF ty=core_type {
      Pjk_kind_of ty
    }
  | UNDERSCORE {
      Pjk_default
    }
  | reverse_product_jkind %prec below_AMPERSAND {
      Pjk_product (List.rev $1)
    }
  | LPAREN jkind_desc RPAREN {
      Pjk_parens $2
    }
;

reverse_product_jkind :
  | jkind1 = jkind_annotation AMPERSAND jkind2 = jkind_annotation %prec prec_unboxed_product_kind
      { [jkind2; jkind1] }
  | jkinds = reverse_product_jkind
    AMPERSAND
    jkind = jkind_annotation %prec prec_unboxed_product_kind
    { jkind :: jkinds }

jkind_annotation: (* : jkind_annotation *)
  jkind_desc { { pjkind_loc = make_loc $sloc; pjkind_desc = $1 } }
;

jkind_constraint:
  COLON jkind_annotation { $2 }
;

kind_abbreviation_decl:
  KIND_ABBREV abbrev=mkrhs(LIDENT) EQUAL jkind=jkind_annotation {
    (abbrev, jkind)
  }
;

%inline type_param_with_jkind:
  name=tyvar_name_or_underscore
  attrs=attributes
  COLON
  jkind=jkind_annotation
    { match name with
      | None -> mktyp ~loc:$sloc ~attrs (Ptyp_any (Some jkind))
      | Some name -> mktyp ~loc:$sloc ~attrs (Ptyp_var (name, Some jkind)) }
;

parenthesized_type_parameter:
    type_parameter { $1 }
  | type_variance type_param_with_jkind
    { let typ = $2 in
      let infos = $1 in
      let tokens = Tokens.at $sloc in
      { ptp_typ = typ; ptp_infos = infos; ptp_tokens = tokens } }
;

type_parameter:
    type_variance ty=type_variable attributes
      { let typ =
          (* We are "patching" the type to add an attribute, we must also patch
             the tokens attached to the type to account for the new tokens
             relative to the attribute. *)
           let typ_tokens = Tokens.at ($startpos(ty), $endpos) in
           let ptyp_tokens =
             Tokens.replace_first_child typ_tokens ~subst:ty.ptyp_tokens
           in
           { ty with ptyp_tokens; ptyp_attributes = $3 }
        in
        let infos = $1 in
        let tokens = Tokens.at $sloc in
        { ptp_typ = typ; ptp_infos = infos; ptp_tokens = tokens } }
;

%inline type_variable:
  mktyp(
    QUOTE tyvar = ident
      { Ptyp_var (tyvar, None) }
  | UNDERSCORE
      { Ptyp_any None }
  ) { $1 }
;

%inline tyvar_name_or_underscore:
    QUOTE ident
      { Some $2 }
  | UNDERSCORE
      { None }
;

type_variance:
    /* empty */                             { NoVariance, NoInjectivity }
  | PLUS                                    { Covariant, NoInjectivity }
  | MINUS                                   { Contravariant, NoInjectivity }
  | BANG                                    { NoVariance, Injective }
  | PLUS BANG | BANG PLUS                   { Covariant, Injective }
  | MINUS BANG | BANG MINUS                 { Contravariant, Injective }
  | INFIXOP2
      { if $1 = "+!" then Covariant, Injective else
        if $1 = "-!" then Contravariant, Injective else
        expecting $loc($1) "type_variance" }
  | PREFIXOP
      { if $1 = "!+" then Covariant, Injective else
        if $1 = "!-" then Contravariant, Injective else
        expecting $loc($1) "type_variance" }
;

(* A sequence of constructor declarations is either a single BAR, which
   means that the list is empty, or a nonempty BAR-separated list of
   declarations, with an optional leading BAR. *)
constructor_declarations:
  | BAR
      { [] }
  | cs = bar_llist(constructor_declaration)
      { cs }
;
(* A constructor declaration begins with an opening symbol, which can
   be either epsilon or BAR. Note that this opening symbol is included
   in the footprint $sloc. *)
(* Because [constructor_declaration] and [extension_constructor_declaration]
   are identical except for their semantic actions, we introduce the symbol
   [generic_constructor_declaration], whose semantic action is neutral -- it
   merely returns a tuple. *)
generic_constructor_declaration(opening):
  opening
  cid = mkrhs(constr_ident)
  vars_args_res = generalized_constructor_arguments
  attrs = attributes
    {
      let vars, args, res = vars_args_res in
      let info = symbol_info $endpos in
      let loc = make_loc $sloc in
      cid, vars, args, res, attrs, loc, info
    }
;
%inline constructor_declaration(opening):
  d = generic_constructor_declaration(opening)
    {
      let cid, vars, args, res, attrs, loc, info = d in
      Type.constructor cid ~vars ~args ?res ~attrs ~tokens:(Tokens.at $sloc)
        ~loc ~info
    }
;
str_exception_declaration:
  sig_exception_declaration
    { $1 }
| EXCEPTION
  ext_attrs = ext_attributes
  id = mkrhs(constr_ident)
  EQUAL
  lid = mkrhs(constr_longident)
  attrs1 = attributes
  attrs = post_item_attributes
  { let loc = make_loc $sloc in
    let docs, sloc = symbol_docs $sloc in
    let rebind_loc =($startpos(lid), $endpos(attrs1)) in
    let rebind_toks = Tokens.at rebind_loc in
    Te.mk_exception ~attrs ~loc ~docs
      (Te.rebind id lid ~attrs:attrs1 ~loc:(make_loc rebind_loc)
        ~tokens:rebind_toks)
      ~ext_attrs
      ~tokens:(Tokens.at sloc)
  }
;
sig_exception_declaration:
  EXCEPTION
  ext_attrs = ext_attributes
  id = mkrhs(constr_ident)
  vars_args_res = generalized_constructor_arguments
  attrs1 = attributes
  attrs = post_item_attributes
    { let vars, args, res = vars_args_res in
      let decl_loc = ($startpos(id), $endpos(attrs1)) in
      (* If a docstring is present before attrs1, it will be in the [decl_toks]
         but it also will remain unattached, so there will be no conflict with
         [docs] and the parent tokens. *)
      let decl_toks =
        let consume_synthesized = attrs1 = ([], []) in
        Tokens.at ~consume_synthesized decl_loc 
      in
      let docs, sloc = symbol_docs $sloc in
      Te.mk_exception ~attrs ~loc:(make_loc $sloc) ~docs
        (Te.decl id ~vars ~args ?res ~attrs:attrs1 ~loc:(make_loc decl_loc)
          ~tokens:decl_toks)
        ~ext_attrs
        ~tokens:(Tokens.at sloc)
    }
;
%inline let_exception_declaration:
    mkrhs(constr_ident) generalized_constructor_arguments attributes
      { let vars, args, res = $2 in
        Te.decl $1 ~vars ~args ?res ~attrs:$3 ~loc:(make_loc $sloc)
          ~tokens:(Tokens.at $sloc)
      }
;

generalized_constructor_arguments:
    /*empty*/                     { ([],Pcstr_tuple [],None) }
  | OF constructor_arguments      { ([],$2,None) }
  | COLON constructor_arguments MINUSGREATER atomic_type %prec below_HASH
                                  { ([],$2,Some $4) }
  | COLON typevar_list DOT constructor_arguments MINUSGREATER atomic_type
     %prec below_HASH
                                  { ($2,$4,Some $6) }
  | COLON atomic_type %prec below_HASH
                                  { ([],Pcstr_tuple [],Some $2) }
  | COLON typevar_list DOT atomic_type %prec below_HASH
                                  { ($2,Pcstr_tuple [],Some $4) }
;

%inline constructor_argument:
  gbl=global_flag cty=atomic_type modalities=optional_atat_modalities_expr {
    Type.constructor_arg cty ~global:gbl ~modalities ~loc:(make_loc $sloc)
  }
;

constructor_arguments:
  | tys = inline_separated_nonempty_llist(STAR, constructor_argument)
      { Pcstr_tuple tys }
  | LBRACE label_declarations RBRACE
      { Pcstr_record $2 }
;
label_declarations:
    label_declaration                           { [$1] }
  | label_declaration_semi                      { [$1] }
  | label_declaration_semi label_declarations   { $1 :: $2 }
;
label_declaration:
    mutable_or_global_flag mkrhs(label) COLON poly_type_no_attr m1=optional_atat_modalities_expr attrs=attributes
      { let info = symbol_info $endpos in
        let mut, global = $1 in
        Type.field $2 $4 ~mut ~global ~modalities:m1 ~attrs
          ~tokens:(Tokens.at $sloc) ~loc:(make_loc $sloc) ~info}
;
label_declaration_semi:
    mutable_or_global_flag mkrhs(label) COLON poly_type_no_attr m1=optional_atat_modalities_expr attrs0=attributes
      SEMI attrs1=attributes
      { let info =
          match rhs_info $endpos(attrs0) with
          | Some _ as info_before_semi -> info_before_semi
          | None -> symbol_info $endpos
       in
       let mut, global = $1 in
       let attrs =
         ( fst attrs0 @ fst attrs1
         , snd attrs0 @ snd attrs1 )
       in
       Type.field $2 $4 ~mut ~global ~modalities:m1 ~attrs
         ~tokens:(Tokens.at $sloc) ~loc:(make_loc $sloc) ~info}
;

/* Type Extensions */

%inline str_type_extension:
  type_extension(extension_constructor)
    { $1 }
;
%inline sig_type_extension:
  type_extension(extension_constructor_declaration)
    { $1 }
;
%inline type_extension(declaration):
  TYPE
  ext_attrs = ext_attributes
  no_nonrec_flag
  params = type_parameters
  tid = mkrhs(type_longident)
  PLUSEQ
  priv = private_flag
  cs = bar_llist(declaration)
  attrs = post_item_attributes
    { let docs, sloc = symbol_docs $sloc in
      Te.mk tid cs ~params ~priv ~attrs ~docs ~ext_attrs
        ~tokens:(Tokens.at sloc)
    }
;
%inline extension_constructor(opening):
    extension_constructor_declaration(opening)
      { $1 }
  | extension_constructor_rebind(opening)
      { $1 }
;
%inline extension_constructor_declaration(opening):
  d = generic_constructor_declaration(opening)
    {
      let name, vars, args, res, attrs, loc, info = d in
      Te.decl name ~vars ~args ?res ~attrs ~loc ~info
        ~tokens:(Tokens.at $sloc)
    }
;
extension_constructor_rebind(opening):
  opening
  cid = mkrhs(constr_ident)
  EQUAL
  lid = mkrhs(constr_longident)
  attrs = attributes
      { let info = symbol_info $endpos in
        Te.rebind cid lid ~attrs ~loc:(make_loc $sloc) ~info
          ~tokens:(Tokens.at $sloc)
      }
;

/* "with" constraints (additional type equations over signature components) */

with_constraint:
    TYPE type_parameters mkrhs(label_longident) with_type_binder
    core_type_no_attr constraints
      { mkwc $sloc @@ Pwith_type ($2, $3, $4, $5, $6) }
    /* used label_longident instead of type_longident to disallow
       functor applications in type path */
  | TYPE type_parameters mkrhs(label_longident)
    COLONEQUAL core_type_no_attr
      { mkwc $sloc @@ Pwith_typesubst ($2, $3, $5) }
  | MODULE mkrhs(mod_longident) EQUAL mkrhs(mod_ext_longident)
      { mkwc $sloc @@ Pwith_module ($2, $4) }
  | MODULE mkrhs(mod_longident) COLONEQUAL mkrhs(mod_ext_longident)
      { mkwc $sloc @@ Pwith_modsubst ($2, $4) }
  | MODULE TYPE l=mkrhs(mty_longident) EQUAL rhs=module_type
      { mkwc $sloc @@ Pwith_modtype (l, rhs) }
  | MODULE TYPE l=mkrhs(mty_longident) COLONEQUAL rhs=module_type
      { mkwc $sloc @@ Pwith_modtypesubst (l, rhs) }
;
with_type_binder:
    EQUAL          { Public }
  | EQUAL PRIVATE  { Private }
;

/* Polymorphic types */

%inline typevar: (* : string with_loc * jkind_annotation option *)
    QUOTE mkrhs(ident)
      { ($2, None) }
    | LPAREN QUOTE tyvar=mkrhs(ident) COLON jkind=jkind_annotation RPAREN
      { (tyvar, Some jkind) }
;
%inline typevar_list:
  (* : (string with_loc * jkind_annotation option) list *)
  nonempty_llist(typevar)
    { $1 }
;
%inline poly(X):
  typevar_list DOT X
    { ($1, $3) }
;
%inline strictly_poly(X):
| poly(X)
    { let bound_vars, inner_type = $1 in
      mktyp ~loc:$sloc (Ptyp_poly (bound_vars, inner_type)) }
;

possibly_poly(X):
  X
    { $1 }
| strictly_poly(X)
    { $1 }
;
%inline poly_type:
  possibly_poly(core_type)
    { $1 }
;

%inline strictly_poly_type:
  strictly_poly(core_type)
    { $1 }
;

%inline strictly_poly_tuple_type:
  strictly_poly(tuple_type)
    { $1 }

%inline poly_tuple_type:
  | tuple_type { $1 }
  | strictly_poly_tuple_type { $1 }
;

%inline poly_type_with_modes:
  | poly_tuple_type at_mode_expr { $1, $2 }
;

%inline poly_type_with_optional_modes:
  | poly_type_with_modes { $1 }
  | poly_type { $1, [] }
;

%inline strictly_poly_type_with_optional_modes:
  | strictly_poly_type { $1, [] }
  | strictly_poly_tuple_type at_mode_expr { $1, $2 }
;

%inline poly_type_no_attr:
  possibly_poly(core_type_no_attr)
    { $1 }
;

(* -------------------------------------------------------------------------- *)

(* Core language types. *)

(* A core type (core_type) is a core type without attributes (core_type_no_attr)
   followed with a list of attributes. *)
core_type:
    core_type_no_attr
      { $1 }
  | ty = core_type attribute
      { Typ.attr ty ($2, $loc($2)) }
;

%inline core_type_with_optional_modes:
    core_type { $1, [] }
  | tuple_type at_mode_expr { $1, $2 }

(* A core type without attributes is currently defined as an alias type, but
   this could change in the future if new forms of types are introduced. From
   the outside, one should use core_type_no_attr. *)
%inline core_type_no_attr:
  alias_type
    { $1 }
;

(* Alias types include:
   - function types (see below);
   - proper alias types:                  'a -> int as 'a
 *)
alias_type:
    function_type
      { $1 }
  | mktyp(
      ty = alias_type AS QUOTE tyvar = mkrhs(ident)
        { Ptyp_alias(ty, Some tyvar, None) }
   )
   { $1 }
  | aliased_type = alias_type AS
             LPAREN
             name = mkrhs(tyvar_name_or_underscore)
             COLON
             jkind = jkind_annotation
             RPAREN
        { let name = Option.map (fun x -> mkloc x name.loc) name.txt in
          mktyp ~loc:$sloc (Ptyp_alias (aliased_type, name, Some jkind)) }
;

(* Function types include:
   - tuple types (see below);
   - proper function types:               int -> int
                                          foo: int -> int
                                          ?foo: int -> int
 *)
function_type:
  | ty = tuple_type
    %prec MINUSGREATER
      { ty }
  | ty = strict_function_or_labeled_tuple_type
      { ty }
;

strict_function_or_labeled_tuple_type:
  | mktyp(
      label = arg_label
      domain_with_modes = with_optional_mode_expr(param_type)
      MINUSGREATER
      codomain = strict_function_or_labeled_tuple_type
        { let arg_legacy_m, (typ, _), arg_modes = domain_with_modes in
          let domain =
            mk_arrow_arg ~loc:($symbolstartpos, $endpos(domain_with_modes))
              label arg_legacy_m typ arg_modes
          in
           Ptyp_arrow
             { domain ;
              codom_legacy_modes = [];
              codom_type = codomain;
              codom_modes = [] }
             }
    )
    { $1 }
  | mktyp(
      label = arg_label
      domain_with_modes = with_optional_mode_expr(param_type)
      MINUSGREATER
      codomain_with_modes = with_optional_mode_expr(tuple_type)
      %prec MINUSGREATER
        { let arg_legacy_m, (typ, _), arg_modes = domain_with_modes in
          let domain =
            mk_arrow_arg ~loc:($symbolstartpos, $endpos(domain_with_modes))
              label arg_legacy_m typ arg_modes
          in
          let ret_legacy_modes, (codomain, codomain_loc), ret_modes = codomain_with_modes in
          Ptyp_arrow
            { domain ;
              codom_legacy_modes = ret_legacy_modes;
              codom_type = maybe_curry_typ codomain codomain_loc;
              codom_modes = ret_modes }
        }
    )
    { $1 }
  (* These next three cases are for labled tuples - see comment on [tuple_type]
     below.

     The first two cases are present just to resolve a shift reduce conflict
     in a module type [S with t := x:t1 * t2 -> ...] which might be the
     beginning of
       [S with t := x:t1 * t2 -> S']    or    [S with t := x:t1 * t2 -> t3]
     They are the same as the previous two cases, but with [arg_label] replaced
     with the more specific [LIDENT COLON] and [param_type] replaced with the
     more specific [proper_tuple_type].  Apparently, this is sufficient for
     menhir to be able to delay a decision about which of the above module type
     cases we are in.  *)
  | mktyp(
      label = LIDENT COLON
      tuple_with_modes = with_optional_mode_expr(proper_tuple_type)
      MINUSGREATER
      codomain = strict_function_or_labeled_tuple_type
         {
           let arg_legacy_m, (tuple, tuple_loc), arg_modes = tuple_with_modes in
           let ty, ltys = tuple in
           let label = Labelled label in
           let typ = mktyp ~loc:tuple_loc (Ptyp_tuple ((None, ty) :: ltys)) in
           let domain =
             mk_arrow_arg ~loc:($symbolstartpos, $endpos(tuple_with_modes))
               label arg_legacy_m typ arg_modes
           in
           Ptyp_arrow
             { domain ;
              codom_legacy_modes = [];
              codom_type = codomain;
              codom_modes = [] }
         }
    )
    { $1 }
  | mktyp(
      label = LIDENT COLON
      tuple_with_modes = with_optional_mode_expr(proper_tuple_type)
      MINUSGREATER
      codomain_with_modes = with_optional_mode_expr(tuple_type)
      %prec MINUSGREATER
         { let arg_legacy_m, (tuple, tuple_loc), arg_modes = tuple_with_modes in
           let ret_legacy_modes, (codomain, codomain_loc), ret_modes =
             codomain_with_modes
           in
           let ty, ltys = tuple in
           let label = Labelled label in
           let typ = mktyp ~loc:tuple_loc (Ptyp_tuple ((None, ty) :: ltys)) in
           let domain =
             mk_arrow_arg ~loc:($symbolstartpos, $endpos(tuple_with_modes))
               label arg_legacy_m typ arg_modes
           in
           Ptyp_arrow{ domain ;
            codom_legacy_modes = ret_legacy_modes;
            codom_type = maybe_curry_typ codomain codomain_loc;
            codom_modes = ret_modes }
         }
    )
    { $1 }
  | label = LIDENT COLON proper_tuple_type %prec MINUSGREATER
    { let ty, ltys = $3 in
      mktyp ~loc:$sloc (Ptyp_tuple ((Some label, ty) :: ltys))
    }
;

%inline strict_arg_label:
  | label = optlabel
      { Optional label }
  | label = LIDENT COLON
      { Labelled label }
;

%inline arg_label:
  | strict_arg_label
      { $1 }
  | /* empty */
      { Nolabel }
;
/* Legacy mode annotations */
%inline mode_legacy:
   | LOCAL
       { mkloc (Mode "local") (make_loc $sloc) }
   | UNIQUE
       { mkloc (Mode "unique") (make_loc $sloc) }
   | ONCE
       { mkloc (Mode "once") (make_loc $sloc) }
;

%inline mode_expr_legacy:
   | mode_legacy+ { $1 }
;

%inline optional_mode_expr_legacy:
   | { [] }
   | mode_expr_legacy {$1}
;

/* New mode annotation, introduced by AT or ATAT */
%inline mode:
  | LIDENT { mkloc (Mode $1) (make_loc $sloc) }
;

%inline mode_expr:
  | mode+ { $1 }
;

at_mode_expr:
  | AT mode_expr {$2}
  | AT error { expecting $loc($2) "mode expression" }
;

%inline optional_at_mode_expr:
  | { [] }
  | at_mode_expr {$1}
;

%inline with_optional_mode_expr(ty):
  | m0=optional_mode_expr_legacy ty=ty m1=optional_at_mode_expr {
    m0, (ty, $loc(ty)), m1
  }
;


/* Modalities */

%inline modality:
  | LIDENT { mkloc (Modality $1) (make_loc $sloc) }

%inline modalities:
  | modality+ { $1 }

atat_modalities_expr:
  | ATAT modalities {$2}
  | ATAT error { expecting $loc($2) "modality expression" }
;

optional_atat_modalities_expr:
  | %prec below_HASH
    { [] }
  | atat_modalities_expr
    { $1 }
;

%inline stack(expr):
  | STACK expr { mkexp ~loc:$sloc (Pexp_stack $2) }

%inline maybe_stack(expr):
  | expr { $1 }
  | stack(expr) { $1 }

%inline param_type:
  | mktyp(
    LPAREN bound_vars = typevar_list DOT inner_type = core_type RPAREN
      { let poly_loc = $startpos(bound_vars), $endpos(inner_type) in
        let poly = mktyp ~loc:poly_loc (Ptyp_poly (bound_vars, inner_type)) in
        Ptyp_parens poly }
    )
    { $1 }
  | ty = tuple_type
    { ty }
;

(* Tuple types include:
   - atomic types (see below);
   - proper tuple types:                  int * int * int list
   A proper tuple type is a star-separated list of at least two atomic types.
   Tuple components can also be labeled, as an [int * int list * y:bool].

   However, the special case of labeled tuples where the first element has a
   label is not parsed as a proper_tuple_type, but rather as a case of
   strict_function_or_labled_tuple_type above.  This helps in dealing with
   ambiguities around [x:t1 * t2 -> t3] which must continue to parse as a
   function with one labeled argument even in the presense of labled tuples.
*)
tuple_type:
  | ty = atomic_type
    %prec below_HASH
      { ty }
  | proper_tuple_type %prec below_FUNCTOR
    { let ty, ltys = $1 in
      mktyp ~loc:$sloc (Ptyp_tuple ((None, ty) :: ltys))
    }
;

%inline proper_tuple_type:
  | ty = atomic_type
    STAR
    ltys = separated_nonempty_llist(STAR, labeled_tuple_typ_element)
      { ty, ltys }

(* In the case of an unboxed tuple, we don't need the nonsense above because
   the [#( ... )] disambiguates.  However, we still must write out
   the first element explicitly because [labeled_tuple_typ_element] is
   restricted to tail position by its %prec annotation. *)
%inline unboxed_tuple_type_body:
  | ty1 = atomic_type
    STAR
    ltys = separated_nonempty_llist(STAR, labeled_tuple_typ_element)
    { (None, ty1) :: ltys }
  | label = LIDENT
    COLON
    ty1 = atomic_type
    STAR
    ltys = separated_nonempty_llist(STAR, labeled_tuple_typ_element)
    { (Some label, ty1) :: ltys }

%inline labeled_tuple_typ_element :
  | atomic_type %prec STAR
     { None, $1 }
  | label = LIDENT COLON ty = atomic_type %prec STAR
     { Some label, ty }

(* Atomic types are the most basic level in the syntax of types.
   Atomic types include:
   - types between parentheses:           (int -> int)
   - first-class module types:            (module S)
   - type variables:                      'a
   - applications of type constructors:   int, int list, int option list
   - variant types:                       [`A]
 *)


(*
  Delimited types:
    - parenthesised type          (type)
    - first-class module types    (module S)
    - object types                < x: t; ... >
    - variant types               [ `A ]
    - extension                   [%foo ...]
    - quoted types                <[ t ]>

  We support local opens on the following classes of types:
    - parenthesised
    - first-class module types
    - variant types

  Object types are not support for local opens due to a potential
  conflict with MetaOCaml syntax:
    M.< x: t, y: t >
  and quoted expressions:
    .< e >.

  Extension types are not support for local opens merely as a precaution.
*)
delimited_type_supporting_local_open:
  | LPAREN type_ = core_type RPAREN
      { mktyp ~loc:$sloc (Ptyp_parens type_) }
  | LPAREN MODULE attrs = ext_attributes package_type = package_type RPAREN
      { mktyp ~loc:$sloc (Ptyp_package (attrs, package_type)) }
  | mktyp(
      LBRACKET field = tag_field RBRACKET
        { Ptyp_variant([ field ], Closed, None) }
    | LBRACKET BAR fields = row_field_list RBRACKET
        { Ptyp_variant(fields, Closed, None) }
    | LBRACKET field = row_field BAR fields = row_field_list RBRACKET
        { Ptyp_variant(field :: fields, Closed, None) }
    | LBRACKETGREATER BAR? fields = row_field_list RBRACKET
        { Ptyp_variant(fields, Open, None) }
    | LBRACKETGREATER RBRACKET
        { Ptyp_variant([], Open, None) }
    | LBRACKETLESS BAR? fields = row_field_list RBRACKET
        { Ptyp_variant(fields, Closed, Some []) }
    | LBRACKETLESS BAR? fields = row_field_list
      GREATER
      tags = name_tag_list
      RBRACKET
        { Ptyp_variant(fields, Closed, Some tags) }
    | HASHLPAREN unboxed_tuple_type_body RPAREN
        { Ptyp_unboxed_tuple $2 }
    | LESSLBRACKET core_type RBRACKETGREATER
        { Ptyp_quote $2 }
  )
  { $1 }
;

object_type:
  | mktyp(
      LESS meth_list = meth_list GREATER
        { let (f, c) = meth_list in Ptyp_object (f, c) }
    | LESS GREATER
        { Ptyp_object ([], Closed) }
  )
  { $1 }
;

extension_type:
  | mktyp (
      ext = extension
        { Ptyp_extension ext }
  )
  { $1 }
;

delimited_type:
  | object_type
  | extension_type
  | delimited_type_supporting_local_open
    { $1 }
;

spliceable_type:
  | type_ = delimited_type_supporting_local_open
      { type_ }
  | mktyp( /* begin mktyp group */
        tid = mkrhs(type_longident)
          { Ptyp_constr ([], tid) }
      | QUOTE ident = ident
          { Ptyp_var (ident, None) }
  )
  { $1 } /* end mktyp group */
;

atomic_type:
  | type_ = delimited_type
      { type_ }
  | mktyp( /* begin mktyp group */
      tys = actual_type_parameters
      tid = mkrhs(type_longident)
        { Ptyp_constr (tys, tid) }
    | tys = actual_type_parameters
      tid = mkrhs(type_unboxed_longident)
        { unboxed_type $loc(tid) tid.txt tys }
    | tys = actual_type_parameters
      HASH
      cid = mkrhs(clty_longident)
        { Ptyp_class (cid, tys) }
    | mod_ident = mkrhs(mod_ext_longident)
      DOT
      type_ = delimited_type_supporting_local_open
        { Ptyp_open (mod_ident, type_) }
    | QUOTE ident = ident
        { Ptyp_var (ident, None) }
    | UNDERSCORE
        { Ptyp_any None }
    | DOLLAR type_ = spliceable_type
        { Ptyp_splice type_ }
  )
  { $1 } /* end mktyp group */
  | LPAREN QUOTE name=ident COLON jkind=jkind_annotation RPAREN
      { let sub_loc = $startpos($2), $endpos(jkind) in
        let sub = mktyp ~loc:sub_loc (Ptyp_var (name, Some jkind)) in
        mktyp ~loc:$sloc (Ptyp_parens sub) }
  | LPAREN UNDERSCORE COLON jkind=jkind_annotation RPAREN
      { let sub_loc = $startpos($2), $endpos(jkind) in
        let sub = mktyp ~loc:sub_loc (Ptyp_any (Some jkind)) in
        mktyp ~loc:$sloc (Ptyp_parens sub) }
  | LPAREN TYPE COLON jkind=jkind_annotation RPAREN
      { let sub_loc = $startpos($2), $endpos(jkind) in
        let sub = mktyp ~loc:sub_loc (Ptyp_of_kind jkind) in
        mktyp ~loc:$sloc (Ptyp_parens sub) }


(* This is the syntax of the actual type parameters in an application of
   a type constructor, such as int, int list, or (int, bool) Hashtbl.t.
   We allow one of the following:
   - zero parameters;
   - one parameter:
     an atomic type;
     among other things, this can be an arbitrary type between parentheses;
   - two or more parameters:
     arbitrary types, between parentheses, separated with commas.
 *)
%inline actual_type_parameters:
  | /* empty */
      { [] }
  | ty = atomic_type
      { [ ty ] }
  | LPAREN
    tys = separated_nontrivial_llist(COMMA, one_type_parameter_of_several)
    RPAREN
      { tys }

(* Layout annotations on type expressions typically require parens, as in [('a :
   float64)].  But this is unnecessary when the type expression is used as the
   parameter of a tconstr with more than one argument, as in [(int, 'b :
   float64) t]. *)
%inline one_type_parameter_of_several:
  | core_type { $1 }
  | QUOTE id=ident COLON jkind=jkind_annotation
    { mktyp ~loc:$sloc (Ptyp_var (id, (Some jkind))) }
  | UNDERSCORE COLON jkind=jkind_annotation
    { mktyp ~loc:$sloc (Ptyp_any (Some jkind)) }

%inline package_type: module_type
    { $1 }
;
%inline row_field_list:
  separated_nonempty_llist(BAR, row_field)
    { $1 }
;
row_field:
    tag_field
      { $1 }
  | core_type
      { Rf.inherit_ ~loc:(make_loc $sloc) ~tokens:(Tokens.at $sloc) $1 }
;
tag_field:
    mkrhs(name_tag) OF opt_ampersand amper_type_list attributes
      { let info = symbol_info $endpos in
        Rf.tag ~loc:(make_loc $sloc) ~attrs:$5 ~info ~tokens:(Tokens.at $sloc)
          $1 $3 $4 }
  | mkrhs(name_tag) attributes
      { let info = symbol_info $endpos in
        Rf.tag ~loc:(make_loc $sloc) ~attrs:$2 ~info ~tokens:(Tokens.at $sloc)
          $1 true [] }
;
opt_ampersand:
    AMPERSAND                                   { true }
  | /* empty */                                 { false }
;
%inline amper_type_list:
  separated_nonempty_llist(AMPERSAND, core_type_no_attr)
    { $1 }
;
%inline name_tag_list:
  nonempty_llist(name_tag)
    { $1 }
;
(* A method list (in an object type). *)
meth_list:
    head = field_semi         tail = meth_list
  | head = inherit_field_semi tail = meth_list
      { let (f, c) = tail in (head :: f, c) }
  | head = field_semi
  | head = inherit_field_semi
      { [head], Closed }
  | head = field
  | head = inherit_field
      { [head], Closed }
  | DOTDOT
      { [], Open }
;
%inline field:
  mkrhs(label) COLON poly_type_no_attr attributes
    { let info = symbol_info $endpos in
      Of.tag ~loc:(make_loc $sloc) ~attrs:$4 ~info $1 $3
        ~tokens:(Tokens.at $sloc) }
;

%inline field_semi:
  mkrhs(label) COLON poly_type_no_attr attributes SEMI attributes
    { let info =
        match rhs_info $endpos($4) with
        | Some _ as info_before_semi -> info_before_semi
        | None -> symbol_info $endpos
      in
      let attrs = 
        ( fst $4 @ fst $6
        , snd $4 @ snd $6 )
      in
      Of.tag ~loc:(make_loc $sloc) ~attrs ~info ~tokens:(Tokens.at $sloc) $1 $3 }
;

%inline inherit_field:
  ty = atomic_type
    { Of.inherit_ ~loc:(make_loc $sloc) ~tokens:(Tokens.at $sloc) ty }
;

%inline inherit_field_semi:
  ty = atomic_type SEMI
    { Of.inherit_ ~loc:(make_loc $sloc) ~tokens:(Tokens.at $sloc) ty }
;

%inline label:
    LIDENT                                      { $1 }
;

/* Constants */

value_constant:
  | INT               { let (n, m) = $1 in Pconst_integer (None, n, m) }
  | CHAR              { let (c, s) = $1 in Pconst_char (c, s) }
  | STRING            { let (s, strloc, d) = $1 in
                        Pconst_string (s, strloc, d) }
  | FLOAT             { let (f, m) = $1 in Pconst_float (None, f, m) }
;
unboxed_constant:
  | HASH_INT          { unboxed_int $sloc $sloc $1 }
  | HASH_FLOAT        { unboxed_float $1 }
  | HASH_CHAR         { let (c, s) = $1 in Pconst_untagged_char (c, s) }
;
constant:
    value_constant    { $1 }
  | unboxed_constant  { $1 }
;
signed_value_constant:
    value_constant    { $1 }
  | MINUS INT         { let (n, m) = $2 in Pconst_integer(Some "-", n, m) }
  | MINUS FLOAT       { let (f, m) = $2 in Pconst_float(Some "-", f, m) }
  | PLUS INT          { let (n, m) = $2 in Pconst_integer (Some "+", n, m) }
  | PLUS FLOAT        { let (f, m) = $2 in Pconst_float(Some "+", f, m) }
;
signed_constant:
    signed_value_constant { $1 }
  | unboxed_constant      { $1 }
  | MINUS HASH_INT        { unboxed_int $sloc $loc($2) ~sign:Negative $2 }
  | MINUS HASH_FLOAT      { unboxed_float ~sign:Negative $2 }
  | PLUS HASH_INT         { unboxed_int $sloc $loc($2) ~sign:Positive $2 }
  | PLUS HASH_FLOAT       { unboxed_float ~sign:Positive $2 }
;

/* Identifiers and long identifiers */

%inline str_not_op(sym): sym { Longident.Str $1 }

ident:
    UIDENT                    { $1 }
  | LIDENT                    { $1 }
;
val_extra_ident:
  | LPAREN operator RPAREN    { $2 }
  | LPAREN operator error     { unclosed "(" $loc($1) ")" $loc($3) }
  | LPAREN error              { expecting $loc($2) "operator" }
  | LPAREN MODULE error       { expecting $loc($3) "module-expr" }
;
val_ident:
    LIDENT                    { Str $1 }
  | val_extra_ident           { $1 }
;
operator:
    PREFIXOP                                    { Op $1 }
  | LETOP                                       { Op $1 }
  | ANDOP                                       { Op $1 }
  | DOTOP LPAREN index_mod RPAREN               { DotOp ($1,Paren,$3,false) }
  | DOTOP LPAREN index_mod RPAREN LESSMINUS     { DotOp ($1,Paren, $3, true) }
  | DOTOP LBRACKET index_mod RBRACKET           { DotOp ($1,Bracket,$3,false) }
  | DOTOP LBRACKET index_mod RBRACKET LESSMINUS { DotOp ($1,Bracket,$3,true) }
  | DOTOP LBRACE index_mod RBRACE               { DotOp ($1,Brace,$3,false) }
  | DOTOP LBRACE index_mod RBRACE LESSMINUS     { DotOp ($1,Brace,$3,true) }
  | HASHOP                                      { Op $1 }
  | BANG                                        { Op "!" }
  | infix_operator                              { $1 }
;
%inline infixop3:
  | op = INFIXOP3 { op }
  | MOD           { "mod" }
;
%inline infix_operator:
  | op = INFIXOP0  { Op op }
  /* Still support the two symbols as infix operators */
  | AT             { Op "@"}
  | ATAT           { Op "@@"}
  | op = INFIXOP1  { Op op }
  | op = INFIXOP2  { Op op }
  | op = infixop3  { Op op }
  | op = INFIXOP4  { Op op }
  | PLUS           { Op "+"}
  | PLUSDOT        { Op "+."}
  | PLUSEQ         { Op "+="}
  | MINUS          { Op "-"}
  | MINUSDOT       { Op "-."}
  | STAR           { Op "*"}
  | PERCENT        { Op "%"}
  | EQUAL          { Op "="}
  | LESS           { Op "<"}
  | GREATER        { Op ">"}
  | OR             { Op "or"}
  | BARBAR         { Op "||"}
  | AMPERSAND      { Op "&"}
  | AMPERAMPER     { Op "&&"}
  | COLONEQUAL     { Op ":="}
;
index_mod:
| { "" }
| SEMI DOTDOT { ";.." }
;

%inline constr_extra_ident:
  | LPAREN COLONCOLON RPAREN      { Op "::" }
;
constr_extra_nonprefix_ident:
  | LBRACKET RBRACKET             { Str "[]" }
  | LPAREN RPAREN                 { Str "()" }
  | FALSE                         { Str "false" }
  | TRUE                          { Str "true" }
;
constr_ident:
    UIDENT                        { Str $1 }
  | constr_extra_ident            { $1 }
  | constr_extra_nonprefix_ident  { $1 }
;
constr_longident:
    mod_longident       %prec below_DOT  { $1 } /* A.B.x vs (A).B.x */
  | mod_longident DOT constr_extra_ident { mklid ~loc:$sloc @@ Ldot($1,$3) }
  | constr_extra_ident                   { mklid ~loc:$sloc @@ Lident $1 }
  | constr_extra_nonprefix_ident         { mklid ~loc:$sloc @@ Lident $1 }
;
mk_longident(prefix,final):
   | final            { mklid ~loc:$sloc @@ Lident $1 }
   | prefix DOT final { mklid ~loc:$sloc @@ Ldot($1,$3) }
;
val_longident:
    mk_longident(mod_longident, val_ident) { $1 }
;
label_longident:
    mk_longident(mod_longident, str_not_op(LIDENT)) { $1 }
;
type_trailing_no_hash:
  LIDENT  { Longident.Str $1 } %prec below_HASH
;
type_trailing_hash:
  LIDENT HASH_SUFFIX  { Longident.Str_trailing_hash $1 }
;
type_longident:
    mk_longident(mod_ext_longident, type_trailing_no_hash)  { $1 }
;
type_unboxed_longident:
    mk_longident(mod_ext_longident, type_trailing_hash)  { $1 }
;

mod_longident:
    mk_longident(mod_longident, str_not_op(UIDENT))  { $1 }
;
mod_ext_longident:
    mk_longident(mod_ext_longident, str_not_op(UIDENT)) { $1 }
  | mod_ext_longident LPAREN mod_ext_longident RPAREN
      { lapply ~loc:$sloc $1 $3 }
  | mod_ext_longident LPAREN error
      { expecting $loc($3) "module path" }
;
mty_longident:
    mk_longident(mod_ext_longident,str_not_op(ident)) { $1 }
;
clty_longident:
    mk_longident(mod_ext_longident,str_not_op(LIDENT)) { $1 }
;
class_longident:
   mk_longident(mod_longident,str_not_op(LIDENT)) { $1 }
;

/* BEGIN AVOID */
/* For compiler-libs: parse all valid longidents and a little more:
   final identifiers which are value specific are accepted even when
   the path prefix is only valid for types: (e.g. F(X).(::)) */
any_longident:
  | mk_longident (mod_ext_longident,
     str_not_op(ident) | constr_extra_ident | val_extra_ident { $1 }
    ) { $1 }
  | constr_extra_nonprefix_ident { mklid ~loc:$sloc @@ Lident $1 }
;
/* END AVOID */

/* Toplevel directives */

toplevel_directive:
  hash dir = mkrhs(ident)
  arg = ioption(mk_directive_arg(toplevel_directive_argument))
    { mk_directive ~loc:$sloc dir arg }
;

%inline toplevel_directive_argument:
  | STRING        { let (s, _, _) = $1 in Pdir_string s }
  | INT           { let (n, m) = $1 in Pdir_int (n ,m) }
  | val_longident { Pdir_ident $1 }
  | mod_longident { Pdir_ident $1 }
  | FALSE         { Pdir_bool false }
  | TRUE          { Pdir_bool true }
;

/* Miscellaneous */

(* The symbol epsilon can be used instead of an /* empty */ comment. *)
%inline epsilon:
  /* empty */
    { () }
;

%inline raw_string:
  s = STRING
    { let body, _, _ = s in body }
;

name_tag:
    BACKQUOTE ident                             { $2 }
;
rec_flag:
    /* empty */                                 { Nonrecursive }
  | REC                                         { Recursive }
;
%inline nonrec_flag:
    /* empty */                                 { Recursive }
  | NONREC                                      { Nonrecursive }
;
%inline no_nonrec_flag:
    /* empty */ { Recursive }
/* BEGIN AVOID */
  | NONREC      { not_expecting $loc "nonrec flag" }
/* END AVOID */
;
direction_flag:
    TO                                          { Upto }
  | DOWNTO                                      { Downto }
;
private_flag:
  inline_private_flag
    { $1 }
;
%inline inline_private_flag:
    /* empty */                                 { Public }
  | PRIVATE                                     { Private }
;
mutable_flag:
    /* empty */                                 { Immutable }
  | MUTABLE                                     { Mutable }
;
mutable_or_global_flag:
    /* empty */
    { Immutable, false }
  | MUTABLE
    { Mutable, false }
  | GLOBAL
    { Immutable, true }
;
%inline global_flag:
           { false }
  | GLOBAL { true }
;
virtual_flag:
    /* empty */                                 { Concrete }
  | VIRTUAL                                     { Virtual }
;
mutable_virtual_flags:
    /* empty */
      { Immutable, Concrete }
  | MUTABLE
      { Mutable, Concrete }
  | VIRTUAL
      { Immutable, Virtual }
  | MUTABLE VIRTUAL
  | VIRTUAL MUTABLE
      { Mutable, Virtual }
;
private_virtual_flags:
    /* empty */  { Public, Concrete }
  | PRIVATE { Private, Concrete }
  | VIRTUAL { Public, Virtual }
  | PRIVATE VIRTUAL { Private, Virtual }
  | VIRTUAL PRIVATE { Private, Virtual }
;
(* This nonterminal symbol indicates the definite presence of a VIRTUAL
   keyword and the possible presence of a MUTABLE keyword. *)
virtual_with_mutable_flag:
  | VIRTUAL { Immutable }
  | MUTABLE VIRTUAL { Mutable }
  | VIRTUAL MUTABLE { Mutable }
;
(* This nonterminal symbol indicates the definite presence of a VIRTUAL
   keyword and the possible presence of a PRIVATE keyword. *)
virtual_with_private_flag:
  | VIRTUAL { Public }
  | PRIVATE VIRTUAL { Private }
  | VIRTUAL PRIVATE { Private }
;
%inline no_override_flag:
    /* empty */                                 { Fresh }
;
%inline override_flag:
    /* empty */                                 { Fresh }
  | BANG                                        { Override }
;
subtractive:
  | MINUS                                       { "-" }
  | MINUSDOT                                    { "-." }
;
additive:
  | PLUS                                        { "+" }
  | PLUSDOT                                     { "+." }
;
optlabel:
   | OPTLABEL                                   { $1 }
   | QUESTION LIDENT COLON                      { $2 }
;

/* Attributes and extensions */

single_attr_id:
    LIDENT { $1 }
  | UIDENT { $1 }
  | AND { "and" }
  | AS { "as" }
  | ASSERT { "assert" }
  | BEGIN { "begin" }
  | CLASS { "class" }
  | CONSTRAINT { "constraint" }
  | DO { "do" }
  | DONE { "done" }
  | DOWNTO { "downto" }
  | ELSE { "else" }
  | END { "end" }
  | EXCEPTION { "exception" }
  | EXTERNAL { "external" }
  | FALSE { "false" }
  | FOR { "for" }
  | FUN { "fun" }
  | FUNCTION { "function" }
  | FUNCTOR { "functor" }
  | IF { "if" }
  | IN { "in" }
  | INCLUDE { "include" }
  | INHERIT { "inherit" }
  | INITIALIZER { "initializer" }
  | LAZY { "lazy" }
  | LET { "let" }
  | LOCAL { "local_" }
  | MATCH { "match" }
  | METHOD { "method" }
  | MODULE { "module" }
  | MUTABLE { "mutable" }
  | NEW { "new" }
  | NONREC { "nonrec" }
  | OBJECT { "object" }
  | OF { "of" }
  | OPEN { "open" }
  | OR { "or" }
  | PRIVATE { "private" }
  | REC { "rec" }
  | SIG { "sig" }
  | STRUCT { "struct" }
  | THEN { "then" }
  | TO { "to" }
  | TRUE { "true" }
  | TRY { "try" }
  | TYPE { "type" }
  | VAL { "val" }
  | VIRTUAL { "virtual" }
  | WHEN { "when" }
  | WHILE { "while" }
  | WITH { "with" }
/* mod/land/lor/lxor/lsl/lsr/asr are not supported for now */
;

attr_id:
  mkloc(
      single_attr_id { [$1] }
    | single_attr_id DOT attr_id { $1 :: $3.txt }
  ) { $1 }
;
attribute:
  LBRACKETAT attr_id attr_payload RBRACKET
    { mk_attr ~sloc:$sloc $2 $3 }
;
post_item_attribute:
  LBRACKETATAT attr_id attr_payload RBRACKET
    { mk_attr ~sloc:$sloc $2 $3 }
;
floating_attribute:
  LBRACKETATATAT attr_id attr_payload RBRACKET
    { mark_symbol_docs $sloc;
      mk_attr ~sloc:$sloc $2 $3 }
;
%inline post_item_attributes:
  post_item_attribute*
    { $1, Tokens.at ~consume_synthesized:false $sloc }
;
%inline attributes:
  attribute*
    { $1, Tokens.at ~consume_synthesized:false $sloc }
;
ext:
  | /* empty */   { None }
  | PERCENT attr_id { Some $2 }
;
%inline no_ext:
  | /* empty */     { None }
/* BEGIN AVOID */
  | PERCENT attr_id { not_expecting $loc "extension" }
/* END AVOID */
;
%inline ext_attributes:
  ext attributes
  { (* both symbols can be empty, but there are no child node for ext, so we would only
       consume the one for attributes... if we were consuming.
       But since the loc covers and empty region, we don't ... we instead add an extra
       child_node *)
    { pea_ext = $1; pea_attrs = $2; pea_tokens = Tokens.at $sloc } }
;
%inline ext_noattrs:
  | PERCENT attr_id
    { { pea_ext = Some $2; pea_attrs = no_attrs; pea_tokens = Tokens.at $sloc } }
;
%inline noext_attributes:
  no_ext attributes
  { { pea_ext = $1; pea_attrs = $2; pea_tokens = Tokens.at $sloc } }
;
extension:
  | LBRACKETPERCENT attr_id payload RBRACKET { ($2, $3, Tokens.at $sloc) }
  | QUOTED_STRING_EXPR
    { mk_quotedext ~loc:$sloc $1 }
;
item_extension:
  | LBRACKETPERCENTPERCENT attr_id payload RBRACKET
    { ($2, $3, Tokens.at $sloc) }
  | QUOTED_STRING_ITEM
    { mk_quotedext ~loc:$sloc $1 }
;
payload:
    structure { PStr $1 }
  | COLON signature { PSig $2 }
  | COLON core_type { PTyp $2 }
  | QUESTION pattern { PPat ($2, None) }
  | QUESTION pattern WHEN seq_expr { PPat ($2, Some $4) }
;
attr_payload:
  payload
    { $1 }
;
%%
