open PPrint
open Parse
open Parsetree

type 'a loc = 'a Location.loc = { txt: 'a; loc: Location.t }
let stringf fmt = Printf.ksprintf string fmt

let rec longident = function
  | Longident.Lident s -> string s
  | Ldot (lid, s) -> longident lid ^^ dot ^^ string s
  | Lapply (l1, l2) ->
    longident l1 ^^ lparen ^^ break 0 ^^ longident l2 ^^ break 0 ^^ rparen

let direction = function
  | Asttypes.Upto -> string "to"
  | Downto -> string "downto"

let private_ = function
  | Asttypes.Private -> string "private" ^^ break 1
  | Public -> empty

let mutable_ = function
  | Asttypes.Mutable -> string "mutable" ^^ break 1
  | Immutable -> empty

let virtual_ = function
  | Asttypes.Virtual -> string "virtual" ^^ break 1
  | Concrete -> empty

let virtual_field = function
  | Cfk_virtual _ -> string "virtual" ^^ break 1
  | Cfk_concrete _ -> empty

let override_field = function
  | Cfk_virtual _ -> empty
  | Cfk_concrete (Fresh, _) -> empty
  | Cfk_concrete (Override, _) -> bang

let param_info = Asttypes.(function
    | NoVariance, NoInjectivity -> empty
    | Covariant, NoInjectivity -> plus
    | Contravariant, NoInjectivity -> minus
    | NoVariance, Injective -> bang
    | Covariant, Injective -> plus ^^ bang (* FIXME: [bang ^^ plus] also ok *)
    | Contravariant, Injective -> minus ^^ bang (* FIXME: likewise *)
  )

let type_app ?(parens=true) ty args =
  let left, right =
    if parens then
      lparen, rparen
    else
      lbracket, rbracket
  in
  begin match args with
    | [] -> empty
    | [ x ] -> x ^^ break 1
    | _ -> left ^^ separate (comma ^^ break 1) args ^^ right ^^ break 1
  end ^^ ty

let arrow_type arg_lbl arg rhs =
    begin match arg_lbl with
      | Nolabel -> empty
      | Labelled s -> string s ^^ colon ^^ break 0
      | Optional s ->
        (* FIXME: sometimes the "?foo:" is a single token *)
        qmark ^^ string s ^^ colon ^^ break 0
    end ^^
    arg ^/^ string "->" ^/^ rhs

(* FIXME: handling of string literals is not good enough. *)
(* N.B. stringf is important here: suffixed number come out of the lexer as a
   single token. We can't use ^^ here. *)
let constant = function 
  | Pconst_float (nb, None)
  | Pconst_integer (nb, None) -> string nb
  | Pconst_float (nb, Some suffix)
  | Pconst_integer (nb, Some suffix) -> stringf "%s%c" nb suffix
  | Pconst_unboxed_integer (nb, suffix) -> stringf "#%s%c" nb suffix
  | Pconst_char c -> stringf "'%c'" c
  | Pconst_string (s, _, None) -> stringf "%S" s
  | Pconst_string (s, _, Some delim) -> stringf "{%s|%s|%s}" delim s delim
  | Pconst_unboxed_float (nb, None) -> stringf "#%s" nb
  | Pconst_unboxed_float (nb, Some suffix) -> stringf "#%s%c" nb suffix

let separate_loc_list sep f = separate_map sep (fun l -> f l.txt)

let modality (Modality s) = string s
let modalities = separate_loc_list (break 1) modality

let with_modalities ~modalities:l t =
  match l with
  | [] -> t
  | _ -> t ^/^ string "@@" ^/^ modalities l

let mode (Mode s) = string s
let modes = separate_loc_list (break 1) mode

let with_modes ~modes:l t =
  match l with
  | [] -> t
  | _ -> t ^/^ at ^/^ modes l

let include_kind = function
  | Structure -> empty
  | Functor -> string "functor"

(* TODO: post_item_* *)
let rec attribute ?(post=false) { attr_name; attr_payload; _ } =
  string (if post then "[@@" else "[@")
  ^^ string attr_name.txt ^^ payload attr_payload
  ^^ string "]"

and floating_attribute { attr_name; attr_payload; _ } =
  string "[@@@"
  ^^ string attr_name.txt ^^ payload attr_payload
  ^^ string "]"

and extension ?(floating=false) (ext_name, ext_payload) =
  string (if floating then "[%%" else "[%")
  ^^ string ext_name.txt ^^ payload ext_payload ^^ string "]"

and attributes ?post l = separate_map (break 0) (attribute ?post) l

(* FIXME: can't finish by '>', a space must be added *)
and payload = function
  | PStr s -> break 1 ^^ structure s
  | PSig s -> break 0 ^^ colon ^/^ signature s
  | PTyp c -> break 0 ^^ colon ^/^ core_type c
  | PPat (p, eo) ->
    break 0 ^^ qmark ^/^ pattern p 
    ^^ match eo with 
    | None -> empty 
    | Some e -> break 1 ^^ string "when" ^/^ expression e

and with_attrs ?post ~attrs t =
  match attrs with
  | [] -> t
  | attrs -> t ^/^ attributes ?post attrs

(** {1 Core language} *)
(** {2 Type expressions} *)

and core_type ct =
  core_type_desc ct.ptyp_desc
  |> with_attrs ~attrs:ct.ptyp_attributes

and core_type_desc = function
  | Ptyp_any None -> underscore
  | Ptyp_any Some k -> underscore ^/^ colon ^/^ jkind_annotation k
  | Ptyp_var (s, ko) ->
    let var = char '\'' ^^ string s in
    begin match ko with
      | None -> var
      | Some k -> var ^/^ colon ^/^ jkind_annotation k
    end
  | Ptyp_arrow (lbl, arg_ty, ret_ty, arg_mods, ret_mods) ->
    arrow_type lbl
      (with_modes ~modes:arg_mods (core_type arg_ty))
      (with_modes ~modes:ret_mods (core_type ret_ty))
  | Ptyp_tuple elts ->
    let elt (lbl_opt, ct) =
      begin match lbl_opt with
        | None -> empty
        | Some s -> string s ^^ colon ^^ break 0
      end ^^ core_type ct
    in
    separate_map (break 1 ^^ star ^^ break 1) elt elts
  | Ptyp_unboxed_tuple elts ->
    string "#(" ^^
    core_type_desc (Ptyp_tuple elts) ^^ string ")"
  | Ptyp_constr (lid, args) ->
    type_app (longident lid.txt) (List.map core_type args)
  | Ptyp_object (fields, closed) ->
    string "<" ^/^
    separate_map (semi ^^ break 1) object_field fields ^^
    begin match closed with
      | Closed -> empty
      | Open -> semi ^/^ string ".."
    end ^/^ string ">"
  | Ptyp_class (lid, args) ->
    begin match args with
      | [] -> empty
      | [ x ] -> core_type x ^^ break 1
      | lst ->
        lparen ^^ separate_map comma core_type lst ^^ rparen ^^ break 1
    end ^^ string "#" ^^ longident lid.txt
  | Ptyp_alias (ct, name, None) ->
    core_type ct ^/^ string "as" ^/^
    string "'" ^^ string (Option.get name).txt
  | Ptyp_alias (ct, name_o, Some jkind) ->
    core_type ct ^/^ string "as" ^/^
    lparen ^^ break 0 ^^ (
      match name_o with
      | None -> underscore
      | Some s -> string "'" ^^ string s.txt
    ) ^^ colon ^^ jkind_annotation jkind ^^ break 0 ^^ rparen
  | Ptyp_variant (fields, Closed, None) ->
    (* FIXME: leading | *)
    lbracket ^/^ separate_map (semi ^^ break 1) row_field fields ^/^ rbracket
  | Ptyp_variant (fields, Open, None) ->
    (* FIXME: leading | *)
    let trailing_break = if fields = [] then 0 else 1 in
    string "[>" ^/^ separate_map (semi ^^ break 1) row_field fields ^^
    break trailing_break ^^ rbracket
  | Ptyp_variant (fields, Closed, Some []) ->
    (* FIXME: leading | *)
    string "[<" ^/^ separate_map (semi ^^ break 1) row_field fields ^/^ rbracket
  | Ptyp_variant (fields, Closed, Some labels) ->
    string "[<" ^/^ separate_map (semi ^^ break 1) row_field fields ^/^
    string ">" ^/^ separate_map (break 1) string labels ^/^
    rbracket
  | Ptyp_variant (_, Open, Some _) -> assert false
  | Ptyp_poly (bound_vars, ct) ->
    let binding = function
      | var, None -> string "'" ^^ string var.Location.txt
      | var, Some jkind ->
        lparen ^^ break 0 ^^ string "'" ^^ string var.txt ^^
        jkind_annotation jkind ^^ break 0 ^^ rparen
    in
    separate_map (break 1) binding bound_vars ^^ break 0 ^^ dot ^/^ core_type ct
  | Ptyp_package pkg -> package_type pkg
  | Ptyp_open (lid, ct) -> longident lid.txt ^^ dot ^^ core_type ct
  | Ptyp_extension ext -> extension ext

and package_type (lid, constraints) =
  let with_ =
    match constraints with
    | [] -> empty
    | _ ->
      let one (lid, ct) = longident lid.txt ^/^ equals ^/^ core_type ct in
      break 1 ^^ string "with" ^/^
      separate_map (break 1 ^^ string "and" ^^ break 1) one constraints
  in
  lparen ^^ string "module" ^/^ longident lid.txt ^^ with_ ^^ break 0 ^^ rparen

and row_field rf =
  with_attrs ~attrs:rf.prf_attributes (row_field_desc rf.prf_desc)

and row_field_desc = function
  | Rinherit ct -> core_type ct
  | Rtag (label, _, []) -> bquote ^^ string label.txt
  | Rtag (label, has_const, at_types) ->
    bquote ^^ string label.txt ^/^ string "of" ^/^
    (if has_const then ampersand ^^ break 1 else empty) ^^
    separate_map (break 1 ^^ ampersand ^^ break 1) core_type at_types

and object_field of_ =
  with_attrs ~attrs:of_.pof_attributes (object_field_desc of_.pof_desc)

and object_field_desc = function
  | Oinherit ct -> core_type ct
  | Otag (lbl, ct) -> string lbl.txt ^^ colon ^/^ core_type ct

(** {2 Patterns} *)

and pattern p =
  pattern_desc p.ppat_desc
  |> with_attrs ~attrs:p.ppat_attributes

and pattern_desc = function
  | Ppat_any -> underscore
  | Ppat_var name -> string name.txt
  | Ppat_alias (p, alias) ->
    pattern p ^/^ string "as" ^/^ string alias.txt
  | Ppat_constant c -> constant c
  | Ppat_interval (c1,c2) -> constant c1 ^/^ string ".." ^/^ constant c2
  | Ppat_tuple (elts, closed) ->
    let elt (lbl_opt, p) =
      begin match lbl_opt with
        | None -> empty
        | Some s -> string s ^^ colon ^^ break 0
      end ^^ pattern p
    in
    separate_map (break 0 ^^ comma ^^ break 1) elt elts ^^
    begin match closed with
      | Closed -> empty
      | Open -> break 0 ^^ comma ^^ break 1 ^^ string ".."
    end
  | Ppat_unboxed_tuple (elts, cf) ->
    string "#(" ^^
    pattern_desc (Ppat_tuple (elts, cf)) ^^ string ")"
  | Ppat_construct (lid, None) -> longident lid.txt
  | Ppat_construct (lid, Some ([], p)) -> longident lid.txt ^/^ pattern p
  | Ppat_construct (lid, Some (bindings, p)) ->
    let binding (newtype, jkind) =
      match jkind with
      | None -> string newtype.txt
      | Some jkind ->
        lparen ^^ string newtype.txt ^/^ colon ^/^ jkind_annotation jkind ^^
        rparen
    in
    longident lid.txt ^/^
    lparen ^^ string "type" ^/^
    separate_map (break 1) binding bindings ^^ rparen ^/^
    pattern p
  | Ppat_variant (lbl, None) -> bquote ^^ string lbl
  | Ppat_variant (lbl, Some p) -> bquote ^^ string lbl ^/^ pattern p
  | Ppat_record (fields, cf) ->
    let field (lid, p) = longident lid.txt ^/^ equals ^/^ pattern p in
    lbrace ^/^
    separate_map (semi ^^ break 1) field fields ^/^
    begin match cf with
      | Closed -> empty
      | Open -> underscore ^^ break 1
    end ^^
    rbrace
  | Ppat_record_unboxed_product (fields, cf) ->
    let field (lid, p) = longident lid.txt ^/^ equals ^/^ pattern p in
    string "#{" ^/^
    separate_map (semi ^^ break 1) field fields ^/^
    begin match cf with
      | Closed -> empty
      | Open -> underscore ^^ break 1
    end ^^
    string "}"
  | Ppat_array (mut, ps) ->
    let opn, cls =
      match mut with
      | Mutable -> string "[:", string ":]"
      | Immutable -> string "[|", string "|]"
    in
    opn ^/^
    separate_map (semi ^^ break 1) pattern ps ^/^
    cls
  | Ppat_or (p1, p2) -> pattern p1 ^/^ string "|" ^/^ pattern p2
  | Ppat_constraint (p, None, modes) ->
    with_modes ~modes (pattern p)
  | Ppat_constraint (p, Some ty, atat_modes) ->
    pattern p ^/^ colon ^^
    begin match atat_modes with
      | [] -> empty
      | lst -> break 1 ^^ core_type ty ^/^ string "@@" ^/^ modes lst
    end
  (* FIXME: parser doesn't agree with what's written above I believe.
     Recognized form seems to depend on context... *)
  | Ppat_type lid -> string "#" ^^ longident lid.txt
  | Ppat_lazy p -> string "lazy" ^/^ pattern p
  | Ppat_unpack path ->
    let path =
      match path.txt with
      | None -> underscore
      | Some s -> string s
    in
    lparen ^^ string "module" ^/^ path ^^ rparen
  | Ppat_exception p -> string "exception" ^/^ pattern  p
  | Ppat_extension ext -> extension ext
  | Ppat_open (lid, p) -> longident lid.txt ^^ dot ^^ pattern p

(** {2 Value expressions} *)

and expression e =
  expression_desc e.pexp_desc
  |> with_attrs ~attrs:e.pexp_attributes

and expression_desc = function
  | Pexp_ident lid -> longident lid.txt
  | Pexp_constant c -> constant c
  | Pexp_let (rf, vbs, body) ->
    string "let" ^/^
    begin match rf with
      | Nonrecursive -> empty
      | Recursive -> string "rec" ^^ break 1
    end ^^
    separate_map (break 1 ^^ string "and" ^^ break 1) value_binding vbs ^/^
    string "in" ^/^
    expression body
  | Pexp_function ([], _, Pfunction_body _) -> assert false
  | Pexp_function ([], _, body) -> function_body body
  | Pexp_function (params, constr, body) ->
    string "fun" ^/^
    separate_map (break 1) function_param params ^^
    function_constraint constr ^/^ string "->" ^/^
    function_body body
  | Pexp_prefix_apply (op, arg) -> expression op ^^ expression arg
  | Pexp_infix_apply {op; arg1; arg2} ->
    expression arg1 ^/^ expression op ^/^ expression arg2
  | Pexp_apply (e, args) ->
    let arg (lbl, e) =
      begin match lbl with
      | Nolabel -> empty
      | Labelled l -> tilde ^^ string l ^^ colon ^^ break 0
      | Optional l -> qmark ^^ string l ^^ colon ^^ break 0
      end ^^
      expression e
    in
    expression e ^/^
    separate_map (break 1) arg args
  | Pexp_match (e, cases) ->
    (* FIXME: leading "|" *)
    string "match" ^/^ expression e ^/^ string "with" ^/^
    separate_map (break 1 ^^ string "|" ^^ break 1) case cases
  | Pexp_try (e, cases) ->
    string "try" ^/^ expression e ^/^ string "with" ^/^
    separate_map (break 1 ^^ string "|" ^^ break 1) case cases
  | Pexp_tuple elts ->
    let elt (lbl, e) =
      begin match lbl with
        | None -> empty
        | Some s -> tilde ^^ string s ^^ colon ^^ break 0
      end ^^
      expression e
    in
    separate_map (comma ^^ break 1) elt elts
  | Pexp_unboxed_tuple elts ->
    string "#(" ^^
    expression_desc (Pexp_tuple elts) ^^
    string ")"
  | Pexp_construct (lid, None) -> longident lid.txt
  | Pexp_construct (lid, Some e) -> longident lid.txt ^/^ expression e
  | Pexp_variant (lbl, eo) ->
    bquote ^^ string lbl ^^
    begin match eo with
      | None -> empty
      | Some e -> break 1 ^^ expression e
    end
  | Pexp_record (fields, eo)
  | Pexp_record_unboxed_product (fields, eo) (* FIXME *)->
    let eo =
      match eo with
      | None -> empty
      | Some e -> expression e ^/^ string "with" ^^ break 1
    in
    let field (lid, e) = longident lid.txt ^/^ equals ^/^ expression e in
    lbrace ^/^ eo ^^
    separate_map (semi ^^ break 1) field fields ^/^
    rbrace
  | Pexp_field (e, lid) -> expression e ^^ dot ^^ longident lid.txt
  | Pexp_unboxed_field (e, lid) ->
    expression e ^^ dot ^^ string "#" ^^ longident lid.txt
  | Pexp_setfield (e1, lid, e2) ->
    expression e1 ^^ dot ^^ longident lid.txt ^/^ string "<-" ^/^ expression e2
  | Pexp_array (mut, es) ->
    let opn, cls =
      match mut with
      | Mutable -> string "[:", string ":]"
      | Immutable -> string "[|", string "|]"
    in
    opn ^/^
    separate_map (semi ^^ break 1) expression es ^/^
    cls
  | Pexp_ifthenelse (e1, e2, e3_o) ->
    string "if" ^/^ expression e1 ^/^ string "then" ^/^ expression e2 ^^
    begin match e3_o with
      | None -> empty
      | Some e3 -> break 1 ^^ string "else" ^/^ expression e3
    end
  | Pexp_sequence (e1, e2) -> expression e1 ^^ semi ^/^ expression e2
  | Pexp_while (e1, e2) ->
    string "while" ^/^ expression e1 ^/^ string "do" ^/^ expression e2 ^/^
    string "done"
  | Pexp_for (p, e1, e2, dir, e3) ->
    string "for" ^/^
    pattern p ^/^ equals ^/^ expression e1 ^/^ direction dir ^/^
    expression e2 ^/^ string "do" ^/^ expression e3 ^/^ string "done"
  | Pexp_constraint (e, None, modes) ->
    with_modes ~modes (expression e)
  | Pexp_constraint (e, Some ct, atat_modes) ->
    expression e ^/^ colon ^/^ core_type ct ^^
    begin match atat_modes with
      | [] -> empty
      | lst -> break 1 ^^ string "@@" ^/^ modes lst
    end
  | Pexp_coerce (e, ct1, ct2) ->
    let ct1 =
      match ct1 with
      | None -> empty
      | Some ct -> break 1 ^^ colon ^/^ core_type ct
    in
    expression e ^^ ct1 ^/^ string ":>" ^/^ core_type ct2
  | Pexp_send (e, lbl) -> expression e ^/^ string "#" ^/^ string lbl.txt
  | Pexp_new lid -> string "new" ^/^ longident lid.txt
  | Pexp_setinstvar (lbl, e) -> string lbl.txt ^/^ string "<-" ^/^ expression e
  | Pexp_override fields ->
    let field (lbl, e) = string lbl.txt ^/^ equals ^/^ expression e in
    string "{<" ^/^
    separate_map (semi ^^ break 1) field fields ^/^
    string ">}"
  | Pexp_letmodule (name, me, body) ->
    let name =
      match name.txt with
      | None -> underscore
      | Some s -> string s
    in
    string "let" ^/^ string "module" ^/^ name ^/^ equals ^/^
    module_expr me ^/^ string "in" ^/^
    expression body
  | Pexp_letexception (ec, body) ->
    string "let" ^/^ string "exception" ^/^
    extension_constructor ec ^/^ string "in" ^/^
    expression body
  | Pexp_assert e -> string "assert" ^/^ expression e
  | Pexp_lazy e -> string "lazy" ^/^ expression e
  | Pexp_poly _ -> assert false (* FIXME: doesn't appear in concrete syntax *)
  | Pexp_object cs -> string "object" ^/^ class_structure cs ^/^ string "end"
  | Pexp_newtype (newty, jkind_o, body) ->
    string "fun" ^/^ lparen ^^ string "type" ^/^ string newty.txt ^^
    begin match jkind_o with
      | None -> empty
      | Some jkind -> break 1 ^^ colon ^/^ jkind_annotation jkind
    end ^/^ rparen ^/^ string "->" ^/^
    expression body
  | Pexp_pack me ->
    lparen ^^ string "module" ^/^ module_expr me ^^ rparen
  | Pexp_open (od, e) ->
    open_declaration od ^^ dot ^^ expression e
  | Pexp_letop lo -> letop lo
  | Pexp_extension ext -> extension ext
  | Pexp_unreachable  -> dot
  | Pexp_stack e -> string "stack_" ^/^ expression e
  | Pexp_comprehension ce -> comprehension_expression ce
  | Pexp_overwrite (e1, e2) ->
    string "overwrite_" ^/^ expression e1 ^/^ string "with" ^/^ expression e2
  | Pexp_hole -> underscore
  | Pexp_index_op access ->
    let left, right =
      match access.kind with
      | Paren -> lparen, rparen
      | Brace -> lbrace, rbrace
      | Bracket -> lbracket, rbracket
    in
    expression access.seq ^/^
    begin match access.op with
      | None -> dot
      | Some (None, op) -> string op
      | Some (Some lid, op) -> dot ^^ longident lid ^^ string op
    end ^^
    left ^^
    separate_map (semi ^^ break 1) expression access.indices ^^
    right ^^
    begin match access.assign with
      | None -> empty
      | Some e -> break 1 ^^ string "<-" ^/^ expression e
    end

and case { pc_lhs; pc_guard; pc_rhs } =
  pattern pc_lhs ^^
  begin match pc_guard with
    | None -> empty
    | Some e -> break 1 ^^ string "when" ^/^ expression e
  end ^/^ string "->" ^/^ expression pc_rhs

and letop { let_; ands; body } =
  let ands =
    match ands with
    | [] -> empty
    | _ ->
      let sep = break 1 ^^ string "and" in
      sep ^^ separate_map sep binding_op ands
  in
  string "let" ^^ binding_op let_ ^^ ands ^/^ string "in" ^/^ expression body

and binding_op { pbop_op; pbop_pat; pbop_exp; _ }=
  string pbop_op.txt ^/^ pattern pbop_pat ^/^ equals ^/^ expression pbop_exp

and function_param_desc = function
  | Pparam_val (Nolabel, None, p) -> pattern p
  | Pparam_val (Labelled l, None, p) ->
    (* FIXME: what about punning? *)
    tilde ^^ string l ^^ colon ^^ break 0 ^^ pattern p
  | Pparam_val (Optional l, None, p) ->
    (* FIXME: what about punning? *)
    qmark ^^ string l ^^ colon ^^ break 0 ^^ pattern p
  | Pparam_val (Optional l, Some e, p) ->
    qmark ^^ string l ^^ colon ^^ break 0 ^^ lparen ^^
    break 0 ^^ pattern p ^/^ equals ^/^ expression e ^^ break 0 ^^
    rparen
  | Pparam_val ((Nolabel | Labelled _), Some _, _) -> assert false
  | Pparam_newtype (lat, jkind_o) ->
    lparen ^^ string "type" ^/^ string lat.txt ^^
    begin match jkind_o with
      | None -> empty
      | Some j -> break 1 ^^ colon ^/^ jkind_annotation j
    end ^^
    rparen

and function_param fp = function_param_desc fp.pparam_desc

and function_body = function
  | Pfunction_body e -> expression e
  | Pfunction_cases (cases, _, attrs) ->
    string "function" ^/^
    (* FIXME: leading pipe? *)
    separate_map (break 1 ^^ string "|" ^^ break 1) case cases
    |> with_attrs ~attrs

and type_constraint = function
  | Pconstraint ct ->
    colon ^/^ core_type ct
  | Pcoerce (None, ct) -> string ":>" ^/^ core_type ct
  | Pcoerce (Some ct1, ct2) ->
    colon ^/^ core_type ct1 ^/^ string ":>" ^/^ core_type ct2

and function_constraint fc =
  (* FIXME:
  { mode_annotations : modes;
    (** The mode annotation placed on a function let-binding, e.g.
       [let local_ f x : int -> int = ...].
       The [local_] syntax is parsed into two nodes: the field here, and [pvb_modes].
       This field only affects the interpretation of [ret_type_constraint], while the
       latter is translated in [typecore] to [Pexp_constraint] to contrain the mode of the
       function.
       (* CR zqian: This field is not failthful representation of the user syntax, and
       complicates [pprintast]. It should be removed and their functionality should be
       moved to [pvb_modes]. *)
    *)
    ret_mode_annotations : modes;
    (** The mode annotation placed on a function's body, e.g.
       [let f x : int -> int @@ local = ...].
       This field constrains the mode of function's body.
    *)
    ret_type_constraint : type_constraint option;
    (** The type constraint placed on a function's body. *)
  }
     *)
  match fc.ret_type_constraint with
  | None -> empty
  | Some tc -> type_constraint tc

and comprehension_iterator = function
  | Pcomp_range { start; stop; direction = dir } ->
    equals ^/^ expression start ^/^ direction dir ^/^ expression stop
  | Pcomp_in e -> string "in" ^/^ expression e

and comprehension_clause_binding
  { pcomp_cb_pattern = p
  ; pcomp_cb_iterator = it
  ; pcomp_cb_attributes = attrs
  } =
  pattern p ^/^ comprehension_iterator it
  |> with_attrs ~attrs

and comprehension_clause = function
  | Pcomp_for l ->
    string "for" ^/^
    separate_map (break 1 ^^ string "and" ^^ break 1)
      comprehension_clause_binding l
  | Pcomp_when e ->
    string "when" ^/^ expression e

and comprehension c =
  expression c.pcomp_body ^/^
  separate_map (break 1) comprehension_clause c.pcomp_clauses

and comprehension_expression = function
  | Pcomp_list_comprehension c -> lbracket ^^ comprehension c ^^ rbracket
  | Pcomp_array_comprehension (Mutable, c) ->
    string "[|" ^^ comprehension c ^^ string "|]"
  | Pcomp_array_comprehension (Immutable, c) ->
    string "[:" ^^ comprehension c ^^ string ":]"

(** {2 Value descriptions} *)

and value_description vd =
  let kw =
    match vd.pval_prim with
    | [] -> "val"
    | _ -> "external"
  in
  string kw ^/^ string vd.pval_name.txt ^/^ colon ^/^
  core_type vd.pval_type ^^
  begin match vd.pval_modalities with
    | [] -> empty
    | ms -> break 1 ^^ string "@@" ^/^ modalities ms
  end ^^
  begin match vd.pval_prim with
    | [] -> empty
    | ps ->
      break 1 ^^ equals ^/^
      separate_map (break 1) (fun s -> dquotes (string s)) ps
  end

(** {2 Type declarations} *)

and type_declaration_rhs ?(subst=false) td =
  begin match td.ptype_manifest with
    | None -> empty
    | Some ct ->
      break 1 ^^ (if subst then string ":=" else equals) ^/^ core_type ct
  end ^^
  type_kind td.ptype_private td.ptype_kind ^^
  begin match td.ptype_cstrs with
    | [] -> empty
    | cs ->
      separate_map (break 1) (fun (ct1, ct2, _) ->
        string "constraint" ^/^ core_type ct1 ^/^ equals ^/^ core_type ct2
      ) cs
  end

and type_declaration ?subst td =
  let pp_param (x, info) = param_info info ^^ core_type x in
  type_app (string td.ptype_name.txt) (List.map pp_param td.ptype_params) ^^
  begin match td.ptype_jkind_annotation with
    | None -> empty
    | Some j -> break 1 ^^ colon ^/^ jkind_annotation j
  end ^^
  type_declaration_rhs ?subst td
  |> with_attrs ~post:true ~attrs:td.ptype_attributes

and type_kind priv = function
  | Ptype_abstract -> empty
  | Ptype_variant cds ->
    (* FIXME: leading pipe *)
    break 1 ^^ equals ^/^ private_ priv ^^
    separate_map (break 1 ^^ string "|" ^^ break 1) constructor_declaration
      cds
  | Ptype_record lbls ->
    break 1 ^^ equals ^/^ private_ priv ^^ lbrace ^/^
    separate_map (semi ^^ break 1) label_declaration lbls ^/^ rbrace
  | Ptype_record_unboxed_product lbls ->
    break 1 ^^ equals ^/^ private_ priv ^^ string "#{" ^/^
    separate_map (semi ^^ break 1) label_declaration lbls ^/^ rbrace
  | Ptype_open -> break 1 ^^ equals ^/^ string ".."

and label_declaration
    { pld_name; pld_mutable; pld_modalities; pld_type; pld_attributes; _ } =
  mutable_ pld_mutable ^^
  string pld_name.txt ^/^ colon ^/^ core_type pld_type ^^
  begin match pld_modalities with
    | [] -> empty
    | ms -> break 1 ^^ string "@@" ^/^ modalities ms
  end
  |> with_attrs ~attrs:pld_attributes

and constructor_declaration
    { pcd_name; pcd_vars; pcd_args; pcd_res; pcd_attributes; _ } =
  let pcd_vars =
    match pcd_vars with
      | [] -> empty
      | lst ->
        separate_map (break 1) (function
          | var, None -> string "'" ^^ string var.txt
          | var, Some j ->
            lparen ^^ string "'" ^^ string var.txt ^/^ colon ^/^
            jkind_annotation j ^^ rparen
        ) lst ^^ dot ^^ break 1
  in
  string pcd_name.txt ^^
  begin match pcd_args, pcd_res with
  | Pcstr_tuple [], None -> empty
  | args, None ->
    break 1 ^^ string "of" ^/^ constructor_arguments args
  | Pcstr_tuple [], Some ct ->
    break 1 ^^ colon ^/^ pcd_vars ^^ core_type ct
  | args, Some ct ->
    break 1 ^^ colon ^/^ pcd_vars ^^
    constructor_arguments args ^/^ string "->" ^/^ core_type ct
  end
  |> with_attrs ~attrs:pcd_attributes

and constructor_argument { pca_modalities; pca_type; _ } =
  core_type pca_type ^^
  begin match pca_modalities with
    | [] -> empty
    | ms -> break 1 ^^ string "@@" ^/^ modalities ms
  end

and constructor_arguments = function
  | Pcstr_tuple args ->
    separate_map (break 1 ^^ star ^^ break 1) constructor_argument args
  | Pcstr_record lbls ->
    lbrace ^/^ separate_map (semi ^^ break 1) label_declaration lbls ^/^ rbrace

and type_extension
    { ptyext_path; ptyext_params; ptyext_constructors; ptyext_private;
     ptyext_attributes; _ }=
  (* FIXME: factorize with type_decl *)
  let pp_param (x, info) = param_info info ^^ core_type x in
  let params =
    match ptyext_params with
    | [] -> empty
    | [ x ] -> pp_param x ^^ break 1
    | xs ->
      lparen ^^ separate_map (comma ^^ break 1) pp_param xs ^^ rparen ^^ break 1
  in
  params ^^ longident ptyext_path.txt ^/^
  string "+=" ^/^
  private_ ptyext_private ^^
  (* FIXME: leading pipe *)
  separate_map (break 1 ^^ string "|" ^^ break 1)
    extension_constructor ptyext_constructors
  |> with_attrs ~post:true ~attrs:ptyext_attributes

and extension_constructor { pext_name; pext_kind; pext_attributes; _ } =
  string pext_name.txt ^/^ extension_constructor_kind pext_kind
  |> with_attrs ~attrs:pext_attributes

and type_exception { ptyexn_constructor ; ptyexn_attributes ; _ } =
  string "exception" ^/^
  extension_constructor ptyexn_constructor
  |> with_attrs ~post:true ~attrs:ptyexn_attributes

and extension_constructor_kind = function
  | Pext_decl (vars, args, res) ->
    let vars =
      match vars with
      | [] -> empty
      | lst ->
        separate_map (break 1) (function
          | var, None -> string "'" ^^ string var.txt
          | var, Some j ->
            lparen ^^ string "'" ^^ string var.txt ^/^ colon ^/^
            jkind_annotation j ^^ rparen
        ) lst ^^ dot ^^ break 1
    in
    begin match args, res with
      | Pcstr_tuple [], None -> empty
      | args, None ->
        break 1 ^^ string "of" ^/^ constructor_arguments args
      | Pcstr_tuple [], Some ct ->
        break 1 ^^ colon ^/^ vars ^^ core_type ct
      | args, Some ct ->
        break 1 ^^ colon ^/^ vars ^^
        constructor_arguments args ^/^ string "->" ^/^ core_type ct
    end
  | Pext_rebind lid -> equals ^/^ longident lid.txt

(** {1 Class language} *)
(** {2 Type expressions for the class language} *)

and class_type { pcty_desc; pcty_attributes; _ } =
  class_type_desc pcty_desc
  |> with_attrs ~post:true ~attrs:pcty_attributes

and class_type_desc = function
  | Pcty_constr (lid, args) ->
    type_app ~parens:false (longident lid.txt) (List.map core_type args)
  | Pcty_signature cs -> class_signature cs
  | Pcty_arrow (lbl, arg, rhs) ->
    arrow_type lbl (core_type arg) (class_type rhs)
  | Pcty_extension ext -> extension ext
  | Pcty_open (od, ct) ->
    string "let" ^/^ string "open" ^^ open_description od ^/^ string "in" ^/^
    class_type ct

and class_signature { pcsig_self; pcsig_fields } =
  string "object" ^^
  begin match pcsig_self.ptyp_desc with
    | Ptyp_any None -> empty
    | _otherwise -> parens (core_type pcsig_self)
  end ^/^
  separate_map (break 1) class_type_field pcsig_fields ^/^
  string "end"

and class_type_field { pctf_desc; pctf_attributes; _ } =
  class_type_field_desc pctf_desc
  |> with_attrs ~attrs:pctf_attributes

and class_type_field_desc = function
  | Pctf_inherit ct -> string "inherit" ^/^ class_type ct
  | Pctf_val (lbl, mut, virt, ct) ->
    string "val" ^/^ mutable_ mut ^^ virtual_ virt ^^
    string lbl.txt ^/^ colon ^/^ core_type ct
  | Pctf_method (lbl, priv, virt, ct) ->
    string "method" ^/^ private_ priv ^^ virtual_ virt ^^
    string lbl.txt ^/^ colon ^/^ core_type ct
  | Pctf_constraint (ct1, ct2) ->
    string "constraint" ^/^ core_type ct1 ^/^ equals ^/^ core_type ct2
  | Pctf_attribute attr -> floating_attribute attr
  | Pctf_extension ext -> extension ~floating:true ext

and class_infos : 'a. ('a -> document) -> 'a class_infos -> document =
  fun pp_expr { pci_virt; pci_params; pci_name; pci_expr; pci_attributes; _ } ->
  let pp_param (x, info) = param_info info ^^ core_type x in
  virtual_ pci_virt ^^
  type_app (string pci_name.txt) (List.map pp_param pci_params) ^/^
  equals ^/^ pp_expr pci_expr
  |> with_attrs ~post:true ~attrs:pci_attributes

and class_description cd = class_infos class_type cd

and class_type_declaration ctd = class_infos class_type ctd

(** {2 Value expressions for the class language} *)

and class_expr { pcl_desc; pcl_attributes; _ } =
  class_expr_desc pcl_desc
  |> with_attrs ~post:true ~attrs:pcl_attributes

and class_expr_desc = function
  | Pcl_constr (lid, args) ->
    type_app ~parens:false (longident lid.txt) (List.map core_type args)
  | Pcl_structure cs -> class_structure cs
  | Pcl_fun (lbl, default, pat, rhs) ->
    function_param_desc (Pparam_val (lbl, default, pat))
      ^/^ string "->" ^/^ class_expr rhs
  | Pcl_apply (ce, args) ->
    let arg (lbl, e) =
      begin match lbl with
      | Nolabel -> empty
      | Labelled l -> tilde ^^ string l ^^ colon ^^ break 0
      | Optional l -> qmark ^^ string l ^^ colon ^^ break 0
      end ^^
      expression e
    in
    class_expr ce ^/^
    separate_map (break 1) arg args
  | Pcl_let (rf, vbs, body) ->
    (* FIXME: factorize with Pexp_let *)
    string "let" ^/^
    begin match rf with
      | Nonrecursive -> empty
      | Recursive -> string "rec" ^^ break 1
    end ^^
    separate_map (break 1 ^^ string "and" ^^ break 1) value_binding vbs ^/^
    string "in" ^/^
    class_expr body
  | Pcl_constraint (ce, ct) -> class_expr ce ^/^ colon ^/^ class_type ct
  | Pcl_extension ext -> extension ext
  | Pcl_open (od, ce) ->
    (* FIXME: factorize *)
    string "let" ^/^ string "open" ^^ open_description od ^/^ string "in" ^/^
    class_expr ce

and class_structure { pcstr_self; pcstr_fields } =
  string "object" ^^
  begin match pcstr_self.ppat_desc with
    | Ppat_any -> empty
    | _ -> pattern pcstr_self
  end ^/^
  separate_map (break 1) class_field pcstr_fields ^/^
  string "end"

and class_field { pcf_desc; pcf_attributes; _ } =
  class_field_desc pcf_desc
  |> with_attrs ~post:true ~attrs:pcf_attributes

and class_field_desc = function
  | Pcf_inherit (override, ce, alias) ->
    string "inherit" ^^
    begin match override with
      | Fresh -> empty
      | Override -> bang
    end ^/^
    class_expr ce ^^
    begin match alias with
      | None -> empty
      | Some s -> break 1 ^^ string "as" ^/^ string s.txt
    end
  | Pcf_val (lbl, mut, cfk) ->
    string "val" ^^ override_field cfk ^/^
    mutable_ mut ^^ virtual_field cfk ^^ string lbl.txt ^/^
    class_field_kind cfk
  | Pcf_method (lbl, priv, cfk) ->
    string "method" ^^ override_field cfk ^/^
    private_ priv ^^ virtual_field cfk ^^ string lbl.txt ^/^
    class_field_kind cfk
  | Pcf_constraint (ct1, ct2) ->
    string "constraint" ^/^ core_type ct1 ^/^ equals ^/^ core_type ct2
  | Pcf_initializer e -> string "initializer" ^/^ expression e
  | Pcf_attribute attr -> floating_attribute attr
  | Pcf_extension ext -> extension ~floating:true ext

and class_field_kind = function
  | Cfk_virtual ct -> colon ^/^ core_type ct
  | Cfk_concrete (_, e) -> equals ^/^ expression e

and class_declaration cd = class_infos class_expr cd

(** {1 Module language} *)
(** {2 Type expressions for the module language} *)

and module_type { pmty_desc; pmty_attributes; _ } =
  module_type_desc pmty_desc
  |> with_attrs ~attrs:pmty_attributes

and module_type_desc = function
  | Pmty_ident lid -> longident lid.txt
  | Pmty_signature sg -> signature sg
  | Pmty_functor (fp, mty, modes) ->
    string "functor" ^/^ parens (functor_parameter fp) ^/^ string "->" ^/^
    with_modes ~modes (module_type mty)
  | Pmty_with (mty, cstrs) ->
    module_type mty ^^
    begin match cstrs with
    | [] -> empty
    | _ ->
      break 1 ^^ string "with" ^/^
      separate_map (break 1 ^^ string "and" ^^ break 1) with_constraint cstrs
    end
  | Pmty_typeof me ->
    string "module" ^/^ string "type" ^/^ string "of" ^/^ module_expr me
  | Pmty_extension ext -> extension ext
  | Pmty_alias lid -> longident lid.txt
  | Pmty_strengthen (mty, lid) ->
    module_type mty ^/^ string "with" ^/^ longident lid.txt

and functor_parameter = function
  | Unit -> empty
  | Named (lbl, mty, modes) ->
    begin match lbl.txt with
      | None -> underscore
      | Some s -> string s
    end ^/^ colon ^/^ with_modes (module_type mty) ~modes

and signature { psg_modalities ; psg_items ; _ } =
  string "sig" ^/^
  begin match psg_modalities with
    | [] -> empty
    | lst -> string "@@" ^/^ modalities lst ^^ break 1
  end ^^
  separate_map (break 1) signature_item psg_items ^^
  break 1 (* FIXME: 0 when no items *)
  ^^
  string "end"

and signature_item it = signature_item_desc it.psig_desc

and signature_item_desc = function
  | Psig_value vd -> value_description vd
  | Psig_type (rf, tds) ->
    string "type" ^/^
    begin match rf with
      | Recursive -> empty
      | Nonrecursive -> string "nonrec" ^^ break 1
    end ^^
    separate_map (break 1 ^^ string "and" ^^ break 1) type_declaration tds
  | Psig_typesubst tds ->
    string "type" ^/^
    separate_map (break 1 ^^ string "and" ^^ break 1)
      (type_declaration ~subst:true) tds
  | Psig_typext te -> string "type" ^/^ type_extension te
  | Psig_exception exn -> type_exception exn
  | Psig_module md -> string "module" ^/^ module_declaration md
  | Psig_modsubst ms -> string "module" ^/^ module_substitution ms
  | Psig_recmodule mds ->
    string "module" ^/^ string "rec" ^/^
    separate_map (break 1 ^^ string "and" ^^ break 1) module_declaration mds
  | Psig_modtype mty ->
    string "module" ^/^ string "type" ^/^ module_type_declaration mty
  | Psig_modtypesubst mty ->
    string "module" ^/^ string "type" ^/^
    module_type_declaration ~subst:true mty
  | Psig_open od -> string "open" ^^ open_description od
  | Psig_include (incl, modalities) ->
    with_modalities ~modalities (include_description incl)
  | Psig_class cds ->
    string "class" ^/^
    separate_map (break 1 ^^ string "and" ^^ break 1) class_description cds
  | Psig_class_type ctds ->
    string "class" ^/^ string "type" ^/^
    separate_map (break 1 ^^ string "and" ^^ break 1) class_type_declaration
      ctds
  | Psig_attribute attr -> floating_attribute attr
  | Psig_extension (ext, attrs) ->
    with_attrs ~attrs (extension ~floating:true ext)
  | Psig_kind_abbrev (name, k) ->
    string "kind_abbrev_" ^/^ string name.txt ^/^ equals ^/^ jkind_annotation k

and module_declaration
    { pmd_name; pmd_type; pmd_modalities; pmd_attributes; _ } =
  let name =
    match pmd_name.txt with
    | None -> underscore
    | Some s -> string s
  in
  begin match pmd_modalities with
    | [] -> name
    | l -> name ^/^ at ^/^ modalities l
  end ^/^ equals ^/^ module_type pmd_type
  |> with_attrs ~post:true ~attrs:pmd_attributes

and module_substitution { pms_name; pms_manifest; pms_attributes; _ } =
  string pms_name.txt ^/^ string ":=" ^/^ longident pms_manifest.txt
  |> with_attrs ~attrs:pms_attributes

and module_type_declaration ?(subst=false)
    { pmtd_name; pmtd_type; pmtd_attributes; _ } =
  string pmtd_name.txt ^^
  begin match pmtd_type with
    | None -> empty
    | Some mty ->
      break 1 ^^ (if subst then string ":=" else equals) ^/^ module_type mty
  end
  |> with_attrs ~post:true ~attrs:pmtd_attributes

and open_infos : 'a. ('a -> document) -> 'a open_infos -> document =
  fun pp_expr { popen_expr; popen_override; popen_attributes; _ } ->
    begin match popen_override with
      | Override -> bang
      | Fresh -> empty
    end ^/^
    pp_expr popen_expr
    |> with_attrs ~attrs:popen_attributes

and open_description od = open_infos (fun lid -> longident lid.txt) od

and open_declaration od = open_infos module_expr od

and include_infos : 'a. ('a -> document) -> 'a include_infos -> document =
  fun pp_mod { pincl_kind ; pincl_mod; pincl_attributes; _ } ->
    string "include" ^/^
    begin match pincl_kind with
      | Functor -> string "functor" ^^ break 1
      | Structure -> empty
    end ^^
    pp_mod pincl_mod
    |> with_attrs ~attrs:pincl_attributes

and include_description incl = include_infos module_type incl

and include_declaration incl = include_infos module_expr incl

and with_constraint = function
  | Pwith_type (lid, td) ->
    string "type" ^/^ longident lid.txt ^^ type_declaration_rhs td
  | Pwith_module (lid1, lid2) ->
    string "module" ^/^ longident lid1.txt ^/^ equals ^/^ longident lid2.txt
  | Pwith_modtype (lid, mty) ->
    string "module" ^/^ string "type" ^/^ longident lid.txt ^/^ equals ^/^
    module_type mty
  | Pwith_modtypesubst (lid, mty) ->
    string "module" ^/^ string "type" ^/^ longident lid.txt ^/^ string ":=" ^/^
    module_type mty
  | Pwith_typesubst (lid, td) ->
    string "type" ^/^ longident lid.txt ^^ type_declaration_rhs ~subst:true td
  | Pwith_modsubst (lid1, lid2) ->
    string "module" ^/^ longident lid1.txt ^/^ string ":=" ^/^
    longident lid2.txt

(** {2 Value expressions for the module language} *)

and module_expr { pmod_desc; pmod_attributes; _ } =
  module_expr_desc pmod_desc
  |> with_attrs ~attrs:pmod_attributes

and module_expr_desc = function
  | Pmod_ident lid -> longident lid.txt
  | Pmod_structure str -> structure str
  | Pmod_functor (fp, me) ->
    string "functor" ^/^ parens (functor_parameter fp) ^/^ string "->" ^/^
    module_expr me
  | Pmod_apply (m1, m2) ->
    module_expr m1 ^^ parens (module_expr m2)
  | Pmod_apply_unit me -> module_expr me ^^ lparen ^^ rparen
  | Pmod_constraint (me, None, modes) ->
    (* FIXME: parens? shouldn't that be part of cst? *)
    parens (with_modes ~modes (module_expr me))
  | Pmod_constraint (me, Some mty, atat_modes) ->
    parens (
      module_expr me ^/^ colon ^/^ module_type mty ^^
      begin match atat_modes with
        | [] -> empty
        | l -> break 1 ^^ string "@@" ^/^ modes l
      end
    )
  | Pmod_unpack e -> parens (string "val" ^/^ expression e)
  | Pmod_extension ext -> extension ext

and structure items =
  string "struct" ^/^
  separate_map (break 1) structure_item items ^/^
  string "end"

and structure_item it = structure_item_desc it.pstr_desc

and structure_item_desc = function
  | Pstr_eval (e, attrs) -> with_attrs ~attrs (expression e)
  | Pstr_value (rf, vbs) ->
    (* FIXME: factorize Pexp_let *)
    string "let" ^/^
    begin match rf with
      | Nonrecursive -> empty
      | Recursive -> string "rec" ^^ break 1
    end ^^
    separate_map (break 1 ^^ string "and" ^^ break 1) value_binding vbs
  | Pstr_primitive vd -> value_description vd
  (* FIXME: factorize with Psig_* *)
  | Pstr_type (rf, tds) ->
    string "type" ^/^
    begin match rf with
      | Recursive -> empty
      | Nonrecursive -> string "nonrec" ^^ break 1
    end ^^
    separate_map (break 1 ^^ string "and" ^^ break 1) type_declaration tds
  | Pstr_typext te -> string "type" ^/^ type_extension te
  | Pstr_exception exn -> type_exception exn
  | Pstr_module mb -> string "module" ^/^ module_binding mb
  | Pstr_recmodule mbs ->
    string "module" ^/^ string "rec" ^/^
    separate_map (break 1 ^^ string "and" ^^ break 1) module_binding mbs
  | Pstr_modtype mty ->
    string "module" ^/^ string "type" ^/^ module_type_declaration mty
  | Pstr_open od -> string "open" ^^ open_declaration od
  | Pstr_class cds ->
    string "class" ^/^
    separate_map (break 1 ^^ string "and" ^^ break 1) class_declaration cds
  | Pstr_class_type ctds ->
    string "class" ^/^ string "type" ^/^
    separate_map (break 1 ^^ string "and" ^^ break 1) class_type_declaration
      ctds
  | Pstr_include incl -> include_declaration incl
  | Pstr_attribute a -> floating_attribute a
  | Pstr_extension (ext, attrs) ->
    with_attrs ~attrs (extension ~floating:true ext)
  | Pstr_kind_abbrev (name, k) ->
    string "kind_abbrev_" ^/^ string name.txt ^/^ equals ^/^ jkind_annotation k

and value_constraint = function
  | Pvc_constraint { locally_abstract_univars; typ } ->
    colon ^/^
    begin match locally_abstract_univars with
      | [] -> empty
      | vars ->
        string "type" ^/^
        separate_loc_list (break 1) string vars ^^ dot ^^ break 1
    end ^^
    core_type typ
  | Pvc_coercion {ground; coercion} ->
    begin match ground with
      | None -> empty
      | Some ct -> colon ^/^ core_type ct ^^ break 1
    end ^^
    string ":>" ^/^ core_type coercion

and value_binding
    { pvb_pat; pvb_expr; pvb_constraint; pvb_modes; pvb_attributes; _ } =
  pattern pvb_pat ^/^
  begin match pvb_constraint with
    | None -> empty
    | Some vc -> value_constraint vc ^^ break 1
  end ^^
  begin match pvb_modes with
    | [] -> empty
    | lst -> at ^/^ modes lst ^^ break 1
  end ^^
  equals ^/^ expression pvb_expr
  |> with_attrs ~attrs:pvb_attributes (* FIXME: post? *)

  (** [let pat : type_constraint = exp] *)

and module_binding { pmb_name; pmb_expr; pmb_attributes; _ } =
  begin match pmb_name.txt with
    | None -> underscore
    | Some s -> string s
  end ^/^ equals ^/^ module_expr pmb_expr
  |> with_attrs ~attrs:pmb_attributes

and jkind_annotation_desc = function
  | Default -> underscore
  | Abbreviation s -> string s
  | Mod (jk, ms) -> jkind_annotation jk ^/^ string "mod" ^/^ modes ms
  | With (jk, ct, modalities) ->
    jkind_annotation jk ^/^ string "with" ^/^ core_type ct
    |> with_modalities ~modalities
  | Kind_of ct -> string "kind_of_" ^/^ core_type ct
  | Product jks ->
    separate_map (break 1 ^^ ampersand ^^ break 1) jkind_annotation jks

and jkind_annotation jk = jkind_annotation_desc jk.pjkind_desc

(* FIXME: TODO? *)
(*
(** {1 Toplevel} *)

(** {2 Toplevel phrases} *)

type toplevel_phrase =
  | Ptop_def of structure
  | Ptop_dir of toplevel_directive  (** [#use], [#load] ... *)

and toplevel_directive =
  {
    pdir_name: string loc;
    pdir_arg: directive_argument option;
    pdir_loc: Location.t;
  }

and directive_argument =
  {
    pdira_desc: directive_argument_desc;
    pdira_loc: Location.t;
  }

and directive_argument_desc =
  | Pdir_string of string
  | Pdir_int of string * char option
  | Pdir_ident of Longident.t
  | Pdir_bool of bool
   *)

let implementation str = separate_map (break 1) structure_item str
