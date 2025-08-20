open PPrint
open Parse
open Parsetree

module S = Syntax

type 'a loc = 'a Location.loc = { txt: 'a; loc: Location.t }
let stringf fmt = Printf.ksprintf string fmt

let rec longident = function
  | Longident.Lident s -> string s
  | Ldot (lid, s) -> longident lid ^^ dot ^^ string s
  | Lapply (l1, l2) ->
    longident l1 ^^ lparen ^^ break 0 ^^ longident l2 ^^ break 0 ^^ rparen

let direction = function
  | Asttypes.Upto -> S.to_
  | Downto -> S.downto_

let private_ = function
  | Asttypes.Private -> S.private_ ^^ break 1
  | Public -> empty

let mutable_ = function
  | Asttypes.Mutable -> S.mutable_ ^^ break 1
  | Immutable -> empty

let virtual_ = function
  | Asttypes.Virtual -> S.virtual_ ^^ break 1
  | Concrete -> empty

let virtual_field = function
  | Cfk_virtual _ -> S.virtual_ ^^ break 1
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

let array_delimiters = function
  | Asttypes.Mutable -> S.lbracket_colon, S.colon_rbracket
  | Immutable -> S.lbracket_pipe, S.pipe_rbracket

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
  arg ^/^ S.rarrow ^/^ rhs

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
  | _ -> t ^/^ S.atat ^/^ modalities l

let mode (Mode s) = string s
let modes = separate_loc_list (break 1) mode

let with_modes ~modes:l t =
  match l with
  | [] -> t
  | _ -> t ^/^ at ^/^ modes l

let include_kind = function
  | Structure -> empty
  | Functor -> S.functor_

module rec Attribute : sig
  val pp : ?post:bool -> attribute -> document
  val pp_floating : attribute -> document
  val pp_list : ?post:bool -> attributes -> document

  val attach : ?post:bool -> attrs:attributes -> document -> document
end = struct
  let pp ?(post=false) { attr_name; attr_payload; _ } =
    (if post then S.lbracket_atat else S.lbracket_at)
    ^^ string attr_name.txt ^^ Payload.pp attr_payload
    ^^ rbracket

  let pp_floating { attr_name; attr_payload; _ } =
    S.lbracket_atatat
    ^^ string attr_name.txt ^^ Payload.pp attr_payload
    ^^ rbracket

  let pp_list ?post l = separate_map (break 0) (pp ?post) l

  let attach ?post ~attrs t =
    match attrs with
    | [] -> t
    | attrs -> t ^/^ pp_list ?post attrs
end

and Extension : sig
  val pp : ?floating:bool -> extension -> document
end = struct
  let pp ?(floating=false) (ext_name, ext_payload) =
    (if floating then S.lbracket_percentpercent else S.lbracket_percent)
    ^^ string ext_name.txt ^^ Payload.pp ext_payload ^^ rbracket
end

and Payload : sig
  val pp : payload -> document
end = struct
(* FIXME: can't finish by '>', a space must be added *)
  let pp = function
    | PStr s -> break 1 ^^ Structure.pp s
    | PSig s -> break 0 ^^ colon ^/^ Signature.pp s
    | PTyp c -> break 0 ^^ colon ^/^ Core_type.pp c
    | PPat (p, eo) ->
      break 0 ^^ qmark ^/^ Pattern.pp p
      ^^ match eo with
      | None -> empty
      | Some e -> break 1 ^^ S.when_ ^/^ Expression.pp e
end

(** {1 Core language} *)
(** {2 Type expressions} *)

and Core_type : sig
  val pp : core_type -> document
end = struct
  let rec pp ct =
    pp_desc ct.ptyp_desc
    |> Attribute.attach ~attrs:ct.ptyp_attributes

  and pp_desc = function
    | Ptyp_any None -> underscore
    | Ptyp_any Some k -> underscore ^/^ colon ^/^ Jkind_annotation.pp k
    | Ptyp_var (s, ko) ->
      let var = char '\'' ^^ string s in
      begin match ko with
        | None -> var
        | Some k -> var ^/^ colon ^/^ Jkind_annotation.pp k
      end
    | Ptyp_arrow (lbl, arg_ty, ret_ty, arg_mods, ret_mods) ->
      arrow_type lbl
        (with_modes ~modes:arg_mods (pp arg_ty))
        (with_modes ~modes:ret_mods (pp ret_ty))
    | Ptyp_tuple elts ->
      let elt (lbl_opt, ct) =
        begin match lbl_opt with
          | None -> empty
          | Some s -> string s ^^ colon ^^ break 0
        end ^^ pp ct
      in
      separate_map (break 1 ^^ star ^^ break 1) elt elts
    | Ptyp_unboxed_tuple elts ->
      S.hash_lparen ^^
      pp_desc (Ptyp_tuple elts) ^^ rparen
    | Ptyp_constr (lid, args) ->
      type_app (longident lid.txt) (List.map pp args)
    | Ptyp_object (fields, closed) ->
      S.lt ^/^
      separate_map (semi ^^ break 1) object_field fields ^^
      begin match closed with
        | Closed -> empty
        | Open -> semi ^/^ S.dotdot
      end ^/^ S.gt
    | Ptyp_class (lid, args) ->
      begin match args with
        | [] -> empty
        | [ x ] -> pp x ^^ break 1
        | lst ->
          lparen ^^ separate_map comma pp lst ^^ rparen ^^ break 1
      end ^^ S.hash ^^ longident lid.txt
    | Ptyp_alias (ct, name, None) ->
      pp ct ^/^ S.as_ ^/^
      squote ^^ string (Option.get name).txt
    | Ptyp_alias (ct, name_o, Some jkind) ->
      pp ct ^/^ S.as_ ^/^
      lparen ^^ break 0 ^^ (
        match name_o with
        | None -> underscore
        | Some s -> squote ^^ string s.txt
      ) ^^ colon ^^ Jkind_annotation.pp jkind ^^ break 0 ^^ rparen
    | Ptyp_variant (fields, Closed, None) ->
      (* FIXME: leading | *)
      lbracket ^/^ separate_map (semi ^^ break 1) row_field fields ^/^ rbracket
    | Ptyp_variant (fields, Open, None) ->
      (* FIXME: leading | *)
      let trailing_break = if fields = [] then 0 else 1 in
      S.lbracket_gt ^/^ separate_map (semi ^^ break 1) row_field fields ^^
      break trailing_break ^^ rbracket
    | Ptyp_variant (fields, Closed, Some []) ->
      (* FIXME: leading | *)
      S.lbracket_lt ^/^ separate_map (semi ^^ break 1) row_field fields ^/^ rbracket
    | Ptyp_variant (fields, Closed, Some labels) ->
      S.lbracket_lt ^/^ separate_map (semi ^^ break 1) row_field fields ^/^
      S.gt ^/^ separate_map (break 1) string labels ^/^
      rbracket
    | Ptyp_variant (_, Open, Some _) -> assert false
    | Ptyp_poly (bound_vars, ct) ->
      let binding = function
        | var, None -> squote ^^ string var.Location.txt
        | var, Some jkind ->
          lparen ^^ break 0 ^^ squote ^^ string var.txt ^^
          Jkind_annotation.pp jkind ^^ break 0 ^^ rparen
      in
      separate_map (break 1) binding bound_vars ^^ break 0 ^^ dot ^/^ pp ct
    | Ptyp_package pkg -> package_type pkg
    | Ptyp_open (lid, ct) -> longident lid.txt ^^ dot ^^ pp ct
    | Ptyp_extension ext -> Extension.pp ext
    | Ptyp_parens ct -> parens (pp ct)

  and package_type (lid, constraints) =
    let with_ =
      match constraints with
      | [] -> empty
      | _ ->
        let one (lid, ct) = longident lid.txt ^/^ equals ^/^ pp ct in
        break 1 ^^ S.with_ ^/^
        separate_map (break 1 ^^ S.and_ ^^ break 1) one constraints
    in
    lparen ^^ S.module_ ^/^ longident lid.txt ^^ with_ ^^ break 0 ^^ rparen

  and row_field rf =
    Attribute.attach ~attrs:rf.prf_attributes (row_field_desc rf.prf_desc)

  and row_field_desc = function
    | Rinherit ct -> pp ct
    | Rtag (label, _, []) -> bquote ^^ string label.txt
    | Rtag (label, has_const, at_types) ->
      bquote ^^ string label.txt ^/^ S.of_ ^/^
      (if has_const then ampersand ^^ break 1 else empty) ^^
      separate_map (break 1 ^^ ampersand ^^ break 1) pp at_types

  and object_field of_ =
    Attribute.attach ~attrs:of_.pof_attributes (object_field_desc of_.pof_desc)

  and object_field_desc = function
    | Oinherit ct -> pp ct
    | Otag (lbl, ct) -> string lbl.txt ^^ colon ^/^ pp ct
end

(** {2 Patterns} *)

and Pattern : sig
  val pp : pattern -> document
end = struct
  let rec pp p =
    pp_desc p.ppat_desc
    |> Attribute.attach ~attrs:p.ppat_attributes

  and pp_desc = function
    | Ppat_any -> underscore
    | Ppat_var name -> string name.txt
    | Ppat_alias (p, alias) ->
      pp p ^/^ S.as_ ^/^ string alias.txt
    | Ppat_constant c -> constant c
    | Ppat_interval (c1,c2) -> constant c1 ^/^ S.dotdot ^/^ constant c2
    | Ppat_tuple (elts, closed) ->
      let elt (lbl_opt, p) =
        begin match lbl_opt with
          | None -> empty
          | Some s -> string s ^^ colon ^^ break 0
        end ^^ pp p
      in
      separate_map (break 0 ^^ comma ^^ break 1) elt elts ^^
      begin match closed with
        | Closed -> empty
        | Open -> break 0 ^^ comma ^^ break 1 ^^ S.dotdot
      end
    | Ppat_unboxed_tuple (elts, cf) ->
      S.hash_lparen ^^ pp_desc (Ppat_tuple (elts, cf)) ^^ rparen
    | Ppat_construct (lid, None) -> longident lid.txt
    | Ppat_construct (lid, Some ([], p)) -> longident lid.txt ^/^ pp p
    | Ppat_construct (lid, Some (bindings, p)) ->
      let binding (newtype, jkind) =
        match jkind with
        | None -> string newtype.txt
        | Some jkind ->
          lparen ^^ string newtype.txt ^/^ colon ^/^ Jkind_annotation.pp jkind ^^
          rparen
      in
      longident lid.txt ^/^
      lparen ^^ S.type_ ^/^
      separate_map (break 1) binding bindings ^^ rparen ^/^
      pp p
    | Ppat_variant (lbl, None) -> bquote ^^ string lbl
    | Ppat_variant (lbl, Some p) -> bquote ^^ string lbl ^/^ pp p
    | Ppat_record (fields, cf) ->
      let field (lid, p) = longident lid.txt ^/^ equals ^/^ pp p in
      lbrace ^/^
      separate_map (semi ^^ break 1) field fields ^/^
      begin match cf with
        | Closed -> empty
        | Open -> underscore ^^ break 1
      end ^^
      rbrace
    | Ppat_record_unboxed_product (fields, cf) ->
      let field (lid, p) = longident lid.txt ^/^ equals ^/^ pp p in
      S.hash_lbrace ^/^
      separate_map (semi ^^ break 1) field fields ^/^
      begin match cf with
        | Closed -> empty
        | Open -> underscore ^^ break 1
      end ^^
      rbrace
    | Ppat_array (mut, ps) ->
      let opn, cls = array_delimiters mut in
      opn ^/^
      separate_map (semi ^^ break 1) pp ps ^/^
      cls
    | Ppat_or (p1, p2) -> pp p1 ^/^ S.pipe ^/^ pp p2
    | Ppat_constraint (p, None, modes) ->
      with_modes ~modes (pp p)
    | Ppat_constraint (p, Some ty, atat_modes) ->
      pp p ^/^ colon ^^
      begin match atat_modes with
        | [] -> empty
        | lst -> break 1 ^^ Core_type.pp ty ^/^ S.atat ^/^ modes lst
      end
    (* FIXME: parser doesn't agree with what's written above I believe.
       Recognized form seems to depend on context... *)
    | Ppat_type lid -> S.hash ^^ longident lid.txt
    | Ppat_lazy p -> S.lazy_ ^/^ pp p
    | Ppat_unpack path ->
      let path =
        match path.txt with
        | None -> underscore
        | Some s -> string s
      in
      lparen ^^ S.module_ ^/^ path ^^ rparen
    | Ppat_exception p -> S.exception_ ^/^ pp  p
    | Ppat_extension ext -> Extension.pp ext
    | Ppat_open (lid, p) -> longident lid.txt ^^ dot ^^ pp p
    | Ppat_parens p -> parens (pp p)
    | Ppat_list elts ->
      brackets (
        separate_map (semi ^^ break 1) pp elts
      )
    | Ppat_cons (hd, tl) -> pp hd ^/^ S.cons ^/^ pp tl
end

(** {2 Value expressions} *)

and Expression : sig
  val pp : expression -> document
end = struct
  let rec pp e =
    pp_desc e.pexp_desc
    |> Attribute.attach ~attrs:e.pexp_attributes

  and pp_desc = function
    | Pexp_ident lid -> longident lid.txt
    | Pexp_constant c -> constant c
    | Pexp_let (rf, vbs, body) ->
      S.let_ ^/^
      begin match rf with
        | Nonrecursive -> empty
        | Recursive -> S.rec_ ^^ break 1
      end ^^
      separate_map (break 1 ^^ S.and_ ^^ break 1) Value_binding.pp vbs ^/^
      S.in_ ^/^
      pp body
    | Pexp_function ([], _, Pfunction_body _) -> assert false
    | Pexp_function ([], _, body) -> Function_body.pp body
    | Pexp_function (params, constr, body) ->
      S.fun_ ^/^
      separate_map (break 1) Function_param.pp params ^^
      Function_constraint.pp constr ^/^ S.rarrow ^/^
      Function_body.pp body
    | Pexp_prefix_apply (op, arg) -> pp op ^^ pp arg
    | Pexp_add_or_sub (op, arg) -> string op ^^ pp arg
    | Pexp_infix_apply {op; arg1; arg2} ->
      pp arg1 ^/^ pp op ^/^ pp arg2
    | Pexp_apply (e, args) -> pp_apply e args
    | Pexp_match (e, cases) ->
      (* FIXME: leading "|" *)
      S.match_ ^/^ pp e ^/^ S.with_ ^/^
      separate_map (break 1 ^^ S.pipe ^^ break 1) Case.pp cases
    | Pexp_try (e, cases) ->
      S.try_ ^/^ pp e ^/^ S.with_ ^/^
      separate_map (break 1 ^^ S.pipe ^^ break 1) Case.pp cases
    | Pexp_tuple elts ->
      let elt (lbl, e) =
        begin match lbl with
          | None -> empty
          | Some s -> tilde ^^ string s ^^ colon ^^ break 0
        end ^^
        pp e
      in
      separate_map (comma ^^ break 1) elt elts
    | Pexp_unboxed_tuple elts ->
      S.hash_lparen ^^
      pp_desc (Pexp_tuple elts) ^^
      rparen
    | Pexp_construct (lid, None) -> longident lid.txt
    | Pexp_construct (lid, Some e) -> longident lid.txt ^/^ pp e
    | Pexp_variant (lbl, eo) ->
      bquote ^^ string lbl ^^
      begin match eo with
        | None -> empty
        | Some e -> break 1 ^^ pp e
      end
    | Pexp_record (fields, eo)
    | Pexp_record_unboxed_product (fields, eo) (* FIXME *)->
      let eo =
        match eo with
        | None -> empty
        | Some e -> pp e ^/^ S.with_ ^^ break 1
      in
      let field (lid, e) = longident lid.txt ^/^ equals ^/^ pp e in
      lbrace ^/^ eo ^^
      separate_map (semi ^^ break 1) field fields ^/^
      rbrace
    | Pexp_field (e, lid) -> pp e ^^ dot ^^ longident lid.txt
    | Pexp_unboxed_field (e, lid) ->
      pp e ^^ dot ^^ S.hash ^^ longident lid.txt
    | Pexp_setfield (e1, lid, e2) ->
      pp e1 ^^ dot ^^ longident lid.txt ^/^ S.larrow ^/^ pp e2
    | Pexp_array (mut, es) ->
      let opn, cls = array_delimiters mut in
      opn ^/^
      separate_map (semi ^^ break 1) pp es ^/^
      cls
    | Pexp_ifthenelse (e1, e2, e3_o) ->
      S.if_ ^/^ pp e1 ^/^ S.then_ ^/^ pp e2 ^^
      begin match e3_o with
        | None -> empty
        | Some e3 -> break 1 ^^ S.else_ ^/^ pp e3
      end
    | Pexp_sequence (e1, e2) -> pp e1 ^^ semi ^/^ pp e2
    | Pexp_while (e1, e2) ->
      S.while_ ^/^ pp e1 ^/^ S.do_ ^/^ pp e2 ^/^
      S.done_
    | Pexp_for (p, e1, e2, dir, e3) ->
      S.for_ ^/^
      Pattern.pp p ^/^ equals ^/^ pp e1 ^/^ direction dir ^/^
      pp e2 ^/^ S.do_ ^/^ pp e3 ^/^ S.done_
    | Pexp_constraint (e, None, modes) ->
      with_modes ~modes (pp e)
    | Pexp_constraint (e, Some ct, atat_modes) ->
      pp e ^/^ colon ^/^ Core_type.pp ct ^^
      begin match atat_modes with
        | [] -> empty
        | lst -> break 1 ^^ S.atat ^/^ modes lst
      end
    | Pexp_coerce (e, ct1, ct2) ->
      let ct1 =
        match ct1 with
        | None -> empty
        | Some ct -> break 1 ^^ colon ^/^ Core_type.pp ct
      in
      pp e ^^ ct1 ^/^ S.coerce ^/^ Core_type.pp ct2
    | Pexp_send (e, lbl) -> pp e ^/^ S.hash ^/^ string lbl.txt
    | Pexp_new lid -> S.new_ ^/^ longident lid.txt
    | Pexp_setinstvar (lbl, e) -> string lbl.txt ^/^ S.larrow ^/^ pp e
    | Pexp_override fields ->
      let field (lbl, e) = string lbl.txt ^/^ equals ^/^ pp e in
      S.lbrace_lt ^/^
      separate_map (semi ^^ break 1) field fields ^/^
      S.gt_rbrace
    | Pexp_letmodule (name, me, body) ->
      let name =
        match name.txt with
        | None -> underscore
        | Some s -> string s
      in
      S.let_ ^/^ S.module_ ^/^ name ^/^ equals ^/^
      Module_expr.pp me ^/^ S.in_ ^/^
      pp body
    | Pexp_letexception (ec, body) ->
      S.let_ ^/^ S.exception_ ^/^
      Extension_constructor.pp ec ^/^ S.in_ ^/^
      pp body
    | Pexp_assert e -> S.assert_ ^/^ pp e
    | Pexp_lazy e -> S.lazy_ ^/^ pp e
    | Pexp_poly _ -> assert false (* FIXME: doesn't appear in concrete syntax *)
    | Pexp_object cs ->
      S.object_ ^/^ Class_expr.pp_structure cs ^/^ S.end_
    | Pexp_newtype (newty, jkind_o, body) ->
      S.fun_ ^/^ lparen ^^ S.type_ ^/^ string newty.txt ^^
      begin match jkind_o with
        | None -> empty
        | Some jkind -> break 1 ^^ colon ^/^ Jkind_annotation.pp jkind
      end ^/^ rparen ^/^ S.rarrow ^/^
      pp body
    | Pexp_pack (me, ty) ->
      lparen ^^ S.module_ ^/^ Module_expr.pp me ^^
      begin match ty with
      | None -> empty
      | Some ct -> break 1 ^^ Type_constraint.pp (Pconstraint ct)
      end ^^
      rparen
    | Pexp_dot_open (od, e) ->
      Open_declaration.pp od ^^ dot ^^ pp e
    | Pexp_let_open (od, e) ->
      S.let_ ^/^ S.open_ ^^ Open_declaration.pp od ^/^ S.in_ ^/^ pp e
    | Pexp_letop lo -> Letop.pp lo
    | Pexp_extension ext -> Extension.pp ext
    | Pexp_unreachable  -> dot
    | Pexp_stack e -> S.stack_ ^/^ pp e
    | Pexp_comprehension ce -> Comprehension.pp_expr ce
    | Pexp_overwrite (e1, e2) ->
      S.overwrite_ ^/^ pp e1 ^/^ S.with_ ^/^ pp e2
    | Pexp_hole -> underscore
    | Pexp_index_op access ->
      let left, right =
        match access.kind with
        | Paren -> lparen, rparen
        | Brace -> lbrace, rbrace
        | Bracket -> lbracket, rbracket
      in
      pp access.seq ^/^
      begin match access.op with
        | None -> dot
        | Some (None, op) -> string op
        | Some (Some lid, op) -> dot ^^ longident lid ^^ string op
      end ^^
      left ^^
      separate_map (semi ^^ break 1) pp access.indices ^^
      right ^^
      begin match access.assign with
        | None -> empty
        | Some e -> break 1 ^^ S.larrow ^/^ pp e
      end
    | Pexp_parens { begin_end = false; exp } -> parens (pp exp)
    | Pexp_parens { begin_end = true; exp } ->
      S.begin_ ^/^ pp exp ^/^ S.end_
    | Pexp_list elts ->
      brackets (
        separate_map (semi ^^ break 1) pp elts
      )
    | Pexp_cons (hd, tl) -> pp hd ^/^ S.cons ^/^ pp tl

  and pp_apply e args = pp e ^/^ Application.pp_args args
end

and Application : sig
  val pp_args : expression argument list -> document
end = struct

  let pp_arg a = Argument.pp Expression.pp a

  let pp_args = separate_map (break 1) pp_arg
end

and Case : sig
  val pp : case -> document
end = struct
  let pp { pc_lhs; pc_guard; pc_rhs } =
    Pattern.pp pc_lhs ^^
    begin match pc_guard with
      | None -> empty
      | Some e -> break 1 ^^ S.when_ ^/^ Expression.pp e
    end ^/^ S.rarrow ^/^ Expression.pp pc_rhs
end

and Letop : sig
  val pp : letop -> document
end = struct
  let pp { let_; ands; body } =
    let ands =
      match ands with
      | [] -> empty
      | _ -> break 1 ^^ separate_map (break 1) Binding_op.pp ands
    in
    Binding_op.pp let_ ^^ ands ^/^ S.in_ ^/^
    Expression.pp body
end

and Binding_op : sig
  val pp : binding_op -> document
end = struct
  let pp { pbop_op; pbop_binding; _ }=
    string pbop_op.txt ^/^ Value_binding.pp pbop_binding
end

and Argument : sig
  val pp : ('a -> document) -> 'a argument -> document
end = struct
  let pp pp_arg = function
    | Parg_unlabelled
        { legacy_modes = []; arg; typ_constraint = None; modes = []; } ->
      pp_arg arg
    | Parg_unlabelled { legacy_modes; arg; typ_constraint; modes = m; } ->
      parens (
        modes legacy_modes ^/^ pp_arg arg ^^
        begin match typ_constraint with
          | None -> empty
          | Some ct -> break 1 ^^ Type_constraint.pp ct
        end
        |> with_modes ~modes:m (* FIXME @ or @@ ? *)
      )
    | Parg_labelled {
        optional; legacy_modes; name: string; maybe_punned = None; typ_constraint;
        modes = m; default;
      } ->
      (if optional then qmark else tilde) ^^
      parens (
        modes legacy_modes ^/^ string name ^^
        begin (match typ_constraint with
          | None -> empty
          | Some ct -> break 1 ^^ Type_constraint.pp ct)
              |> with_modes ~modes:m (* FIXME @ or @@ ? *)
        end ^^
           begin match default with
             | None -> empty
             | Some d ->
               equals ^^ Expression.pp d
           end
      )
    | Parg_labelled {
        optional; legacy_modes; name: string; maybe_punned = Some arg;
        typ_constraint; modes = m; default;
      } ->
      (* FIXME: single or multi-token? *)
      (if optional then qmark else tilde) ^^ string name ^^ colon ^^
      parens (* FIXME: check in tokens if really present *) (
        modes legacy_modes ^/^ pp_arg arg ^^
        begin (match typ_constraint with
          | None -> empty
          | Some ct -> break 1 ^^ Type_constraint.pp ct)
              |> with_modes ~modes:m (* FIXME @ or @@ ? *)
        end ^^
           begin match default with
             | None -> empty
             | Some d ->
               equals ^^ Expression.pp d
           end
      )
end

and Function_param : sig
  val pp : function_param -> document
  val pp_desc : function_param_desc -> document
end = struct
  let pp_desc = function
    | Pparam_val arg -> Argument.pp Pattern.pp arg
    | Pparam_newtype (lat, jkind_o) ->
      lparen ^^ S.type_ ^/^ string lat.txt ^^
      begin match jkind_o with
        | None -> empty
        | Some j -> break 1 ^^ colon ^/^ Jkind_annotation.pp j
      end ^^
      rparen

  let pp fp = pp_desc fp.pparam_desc
end

and Function_body : sig
  val pp : function_body -> document
end = struct
  let pp = function
    | Pfunction_body e -> Expression.pp e
    | Pfunction_cases (cases, _, attrs) ->
      S.function_ ^/^
      (* FIXME: leading pipe? *)
      separate_map (break 1 ^^ S.pipe ^^ break 1) Case.pp cases
      |> Attribute.attach ~attrs
end

and Type_constraint : sig
  val pp : type_constraint -> document
end = struct
  let pp = function
  | Pconstraint ct ->
    colon ^/^ Core_type.pp ct
  | Pcoerce (None, ct) -> S.coerce ^/^ Core_type.pp ct
  | Pcoerce (Some ct1, ct2) ->
    colon ^/^ Core_type.pp ct1 ^/^ S.coerce ^/^ Core_type.pp ct2
end


and Function_constraint : sig
  val pp : function_constraint -> document
end = struct
  let pp fc =
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
  | Some tc -> Type_constraint.pp tc
end

and Comprehension : sig
  val pp : comprehension -> document
  val pp_expr : comprehension_expression -> document
end = struct
  let pp_iterator = function
    | Pcomp_range { start; stop; direction = dir } ->
      equals ^/^ Expression.pp start ^/^ direction dir ^/^ Expression.pp stop
    | Pcomp_in e -> S.in_ ^/^ Expression.pp e

  let pp_clause_binding
    { pcomp_cb_pattern = p
    ; pcomp_cb_iterator = it
    ; pcomp_cb_attributes = attrs
    } =
    Pattern.pp p ^/^ pp_iterator it
    |> Attribute.attach ~attrs

  let pp_clause = function
    | Pcomp_for l ->
      S.for_ ^/^
      separate_map (break 1 ^^ S.and_ ^^ break 1)
        pp_clause_binding l
    | Pcomp_when e ->
      S.when_ ^/^ Expression.pp e

  let pp c =
    Expression.pp c.pcomp_body ^/^
    separate_map (break 1) pp_clause c.pcomp_clauses

  let pp_expr = function
    | Pcomp_list_comprehension c -> lbracket ^^ pp c ^^ rbracket
    | Pcomp_array_comprehension (mut, c) ->
      let left, right = array_delimiters mut in
      left ^^ pp c ^^ right
end

(** {2 Value descriptions} *)

and Value_description : sig
  val pp : value_description -> document
end = struct
  let pp vd =
    let kw =
      match vd.pval_prim with
      | [] -> S.val_
      | _ -> S.external_
    in
    kw ^/^ string vd.pval_name.txt ^/^ colon ^/^
    Core_type.pp vd.pval_type ^^
    begin match vd.pval_modalities with
      | [] -> empty
      | ms -> break 1 ^^ S.atat ^/^ modalities ms
    end ^^
    begin match vd.pval_prim with
      | [] -> empty
      | ps ->
        break 1 ^^ equals ^/^
        separate_map (break 1) (fun s -> dquotes (string s)) ps
    end
end

(** {2 Type declarations} *)
and Label_declaration : sig
  val pp : label_declaration -> document
end = struct
  let pp
      { pld_name; pld_mutable; pld_modalities; pld_type; pld_attributes; _ } =
    mutable_ pld_mutable ^^
    string pld_name.txt ^/^ colon ^/^ Core_type.pp pld_type ^^
    begin match pld_modalities with
      | [] -> empty
      | ms -> break 1 ^^ S.atat ^/^ modalities ms
    end
    |> Attribute.attach ~attrs:pld_attributes
end

and Constructor_argument : sig
  val pp : constructor_argument -> document

  val pp_args : constructor_arguments -> document
end = struct
  let pp { pca_modalities; pca_type; _ } =
    Core_type.pp pca_type ^^
    begin match pca_modalities with
      | [] -> empty
      | ms -> break 1 ^^ S.atat ^/^ modalities ms
    end

  let pp_args = function
    | Pcstr_tuple args ->
      separate_map (break 1 ^^ star ^^ break 1) pp args
    | Pcstr_record lbls ->
      lbrace ^/^
      separate_map (semi ^^ break 1) Label_declaration.pp lbls ^/^
      rbrace
end

and Type_declaration : sig
  val pp_rhs : ?subst:bool -> type_declaration -> document

  val pp : ?subst:bool -> type_declaration -> document
end = struct

  let constructor_declaration
      { pcd_name; pcd_vars; pcd_args; pcd_res; pcd_attributes; _ } =
    let pcd_vars =
      match pcd_vars with
        | [] -> empty
        | lst ->
          separate_map (break 1) (function
            | var, None -> squote ^^ string var.txt
            | var, Some j ->
              lparen ^^ squote ^^ string var.txt ^/^ colon ^/^
              Jkind_annotation.pp j ^^ rparen
          ) lst ^^ dot ^^ break 1
    in
    string pcd_name.txt ^^
    begin match pcd_args, pcd_res with
    | Pcstr_tuple [], None -> empty
    | args, None ->
      break 1 ^^ S.of_ ^/^ Constructor_argument.pp_args args
    | Pcstr_tuple [], Some ct ->
      break 1 ^^ colon ^/^ pcd_vars ^^ Core_type.pp ct
    | args, Some ct ->
      break 1 ^^ colon ^/^ pcd_vars ^^
      Constructor_argument.pp_args args ^/^ S.rarrow ^/^ Core_type.pp ct
    end
    |> Attribute.attach ~attrs:pcd_attributes

  let type_kind priv = function
    | Ptype_abstract -> empty
    | Ptype_variant cds ->
      (* FIXME: leading pipe *)
      break 1 ^^ equals ^/^ private_ priv ^^
      separate_map (break 1 ^^ S.pipe ^^ break 1) constructor_declaration
        cds
    | Ptype_record lbls ->
      break 1 ^^ equals ^/^ private_ priv ^^ lbrace ^/^
      separate_map (semi ^^ break 1) Label_declaration.pp lbls ^/^ rbrace
    | Ptype_record_unboxed_product lbls ->
      break 1 ^^ equals ^/^ private_ priv ^^ S.hash_lbrace ^/^
      separate_map (semi ^^ break 1) Label_declaration.pp lbls ^/^ rbrace
    | Ptype_open -> break 1 ^^ equals ^/^ S.dotdot

  let pp_rhs ?(subst=false) td =
    begin match td.ptype_manifest with
      | None -> empty
      | Some ct ->
        break 1 ^^ (if subst then S.colon_equals else equals) ^/^
        Core_type.pp ct
    end ^^
    type_kind td.ptype_private td.ptype_kind ^^
    begin match td.ptype_cstrs with
      | [] -> empty
      | cs ->
        separate_map (break 1) (fun (ct1, ct2, _) ->
          S.constraint_ ^/^ Core_type.pp ct1 ^/^ equals ^/^
          Core_type.pp ct2
        ) cs
    end

  let pp ?subst td =
    let pp_param (x, info) = param_info info ^^ Core_type.pp x in
    type_app (string td.ptype_name.txt) (List.map pp_param td.ptype_params) ^^
    begin match td.ptype_jkind_annotation with
      | None -> empty
      | Some j -> break 1 ^^ colon ^/^ Jkind_annotation.pp j
    end ^^
    pp_rhs ?subst td
    |> Attribute.attach ~post:true ~attrs:td.ptype_attributes
end

and Type_extension : sig
  val pp : type_extension -> document
end = struct
  let pp { ptyext_path; ptyext_params; ptyext_constructors; ptyext_private;
     ptyext_attributes; _ }=
  (* FIXME: factorize with type_decl *)
  let pp_param (x, info) = param_info info ^^ Core_type.pp x in
  let params =
    match ptyext_params with
    | [] -> empty
    | [ x ] -> pp_param x ^^ break 1
    | xs ->
      lparen ^^ separate_map (comma ^^ break 1) pp_param xs ^^ rparen ^^ break 1
  in
  params ^^ longident ptyext_path.txt ^/^
  S.plus_equals ^/^
  private_ ptyext_private ^^
  (* FIXME: leading pipe *)
  separate_map (break 1 ^^ S.pipe ^^ break 1)
    Extension_constructor.pp ptyext_constructors
  |> Attribute.attach ~post:true ~attrs:ptyext_attributes
end

and Extension_constructor : sig
  val pp : extension_constructor -> document
end = struct
  let pp_kind = function
    | Pext_decl (vars, args, res) ->
      let vars =
        match vars with
        | [] -> empty
        | lst ->
          separate_map (break 1) (function
            | var, None -> squote ^^ string var.txt
            | var, Some j ->
              lparen ^^ squote ^^ string var.txt ^/^ colon ^/^
              Jkind_annotation.pp j ^^ rparen
          ) lst ^^ dot ^^ break 1
      in
      begin match args, res with
        | Pcstr_tuple [], None -> empty
        | args, None ->
            break 1 ^^ S.of_ ^/^ Constructor_argument.pp_args args
          | Pcstr_tuple [], Some ct ->
          break 1 ^^ colon ^/^ vars ^^ Core_type.pp ct
        | args, Some ct ->
          break 1 ^^ colon ^/^ vars ^^
          Constructor_argument.pp_args args ^/^ S.rarrow ^/^ Core_type.pp ct
      end
    | Pext_rebind lid -> equals ^/^ longident lid.txt

  let pp { pext_name; pext_kind; pext_attributes; _ } =
    string pext_name.txt ^/^ pp_kind pext_kind
    |> Attribute.attach ~attrs:pext_attributes
end

and Type_exception : sig
  val pp : type_exception -> document
end = struct
  let pp { ptyexn_constructor ; ptyexn_attributes ; _ } =
  S.exception_ ^/^
  Extension_constructor.pp ptyexn_constructor
  |> Attribute.attach ~post:true ~attrs:ptyexn_attributes
end

(** {1 Class language} *)
(** {2 Type expressions for the class language} *)

and Class_type : sig
  val pp : class_type -> document
end = struct
  let rec pp_desc = function
    | Pcty_constr (lid, args) ->
      type_app ~parens:false (longident lid.txt) (List.map Core_type.pp args)
    | Pcty_signature cs -> pp_signature cs
    | Pcty_arrow (lbl, arg, rhs) ->
      arrow_type lbl (Core_type.pp arg) (pp rhs)
    | Pcty_extension ext -> Extension.pp ext
    | Pcty_open (od, ct) ->
      S.let_ ^/^ S.open_ ^^ Open_description.pp od ^/^
      S.in_ ^/^
      pp ct

  and pp_signature { pcsig_self; pcsig_fields } =
    S.object_ ^^
    begin match pcsig_self.ptyp_desc with
      | Ptyp_any None -> empty
      | _otherwise -> parens (Core_type.pp pcsig_self)
    end ^/^
    separate_map (break 1) pp_field pcsig_fields ^/^
    S.end_

  and pp_field { pctf_desc; pctf_attributes; _ } =
    pp_field_desc pctf_desc
    |> Attribute.attach ~attrs:pctf_attributes

  and pp_field_desc = function
    | Pctf_inherit ct -> S.inherit_ ^/^ pp ct
    | Pctf_val (lbl, mut, virt, ct) ->
      S.val_ ^/^ mutable_ mut ^^ virtual_ virt ^^
      string lbl.txt ^/^ colon ^/^ Core_type.pp ct
    | Pctf_method (lbl, priv, virt, ct) ->
      S.method_ ^/^ private_ priv ^^ virtual_ virt ^^
      string lbl.txt ^/^ colon ^/^ Core_type.pp ct
    | Pctf_constraint (ct1, ct2) ->
      S.constraint_ ^/^ Core_type.pp ct1 ^/^ equals ^/^ Core_type.pp ct2
    | Pctf_attribute attr -> Attribute.pp_floating attr
    | Pctf_extension ext -> Extension.pp ~floating:true ext

  and pp { pcty_desc; pcty_attributes; _ } =
    pp_desc pcty_desc
    |> Attribute.attach ~post:true ~attrs:pcty_attributes
end


and Class_infos : sig
  val pp : 'a. ('a -> document) -> 'a class_infos -> document
end = struct
  let pp pp_expr
      { pci_virt; pci_params; pci_name; pci_expr; pci_attributes; _ } =
    let pp_param (x, info) = param_info info ^^ Core_type.pp x in
    virtual_ pci_virt ^^
    type_app (string pci_name.txt) (List.map pp_param pci_params) ^/^
    equals ^/^ pp_expr pci_expr
    |> Attribute.attach ~post:true ~attrs:pci_attributes
end

and Class_description : sig
  val pp : class_description -> document
end = struct
  let pp = Class_infos.pp Class_type.pp
end

and Class_type_declaration : sig
  val pp : class_type_declaration -> document
end = struct
  let pp = Class_infos.pp Class_type.pp
end

(** {2 Value expressions for the class language} *)

and Class_expr : sig
  val pp : class_expr -> document
  val pp_structure : class_structure -> document
end = struct
  let rec pp_desc = function
    | Pcl_constr (lid, args) ->
      type_app ~parens:false (longident lid.txt) (List.map Core_type.pp args)
    | Pcl_structure cs -> pp_structure cs
    | Pcl_fun (arg, rhs) ->
      Argument.pp Pattern.pp arg ^/^ S.rarrow ^/^ pp rhs
    | Pcl_apply (ce, args) -> pp ce ^/^ Application.pp_args args
    | Pcl_let (rf, vbs, body) ->
      (* FIXME: factorize with Pexp_let *)
      S.let_ ^/^
      begin match rf with
        | Nonrecursive -> empty
        | Recursive -> S.rec_ ^^ break 1
      end ^^
      separate_map (break 1 ^^ S.and_ ^^ break 1) Value_binding.pp vbs ^/^
      S.in_ ^/^
      pp body
    | Pcl_constraint (ce, ct) -> pp ce ^/^ colon ^/^ Class_type.pp ct
    | Pcl_extension ext -> Extension.pp ext
    | Pcl_open (od, ce) ->
      (* FIXME: factorize *)
      S.let_ ^/^ S.open_ ^^ Open_description.pp od ^/^
      S.in_ ^/^ pp ce

  and pp_structure { pcstr_self; pcstr_fields } =
    S.object_ ^^
    begin match pcstr_self.ppat_desc with
      | Ppat_any -> empty
      | _ -> Pattern.pp pcstr_self
    end ^/^
    separate_map (break 1) pp_field pcstr_fields ^/^
    S.end_

  and pp_field { pcf_desc; pcf_attributes; _ } =
    pp_field_desc pcf_desc
    |> Attribute.attach ~post:true ~attrs:pcf_attributes

  and pp_field_desc = function
    | Pcf_inherit (override, ce, alias) ->
      S.inherit_ ^^
      begin match override with
        | Fresh -> empty
        | Override -> bang
      end ^/^
      pp ce ^^
      begin match alias with
        | None -> empty
        | Some s -> break 1 ^^ S.as_ ^/^ string s.txt
      end
    | Pcf_val (lbl, mut, cfk) ->
      S.val_ ^^ override_field cfk ^/^
      mutable_ mut ^^ virtual_field cfk ^^ string lbl.txt ^/^
      pp_field_kind cfk
    | Pcf_method (lbl, priv, cfk) ->
      S.method_ ^^ override_field cfk ^/^
      private_ priv ^^ virtual_field cfk ^^ string lbl.txt ^/^
      pp_field_kind cfk
    | Pcf_constraint (ct1, ct2) ->
      S.constraint_ ^/^ Core_type.pp ct1 ^/^ equals ^/^ Core_type.pp ct2
    | Pcf_initializer e -> S.initializer_ ^/^ Expression.pp e
    | Pcf_attribute attr -> Attribute.pp_floating attr
    | Pcf_extension ext -> Extension.pp ~floating:true ext

  and pp_field_kind = function
    | Cfk_virtual ct -> colon ^/^ Core_type.pp ct
    | Cfk_concrete (_, vb) -> Value_binding.pp vb

  and pp { pcl_desc; pcl_attributes; _ } =
    pp_desc pcl_desc
    |> Attribute.attach ~post:true ~attrs:pcl_attributes
end


and Class_declaration : sig
  val pp : class_declaration -> document
end = struct
  let pp = Class_infos.pp Class_expr.pp
end

(** {1 Module language} *)
(** {2 Type expressions for the module language} *)

and Module_type : sig
  val pp : module_type -> document
end = struct
  let rec pp_desc = function
    | Pmty_ident lid -> longident lid.txt
    | Pmty_signature sg -> Signature.pp sg
    | Pmty_functor (fp, mty, modes) ->
      S.functor_ ^/^ parens (Functor_parameter.pp fp) ^/^ S.rarrow ^/^
      with_modes ~modes (pp mty)
    | Pmty_with (mty, cstrs) ->
      pp mty ^^
      begin match cstrs with
      | [] -> empty
      | _ ->
        break 1 ^^ S.with_ ^/^
        separate_map (break 1 ^^ S.and_ ^^ break 1)
          With_constraint.pp cstrs
      end
    | Pmty_typeof me ->
      S.module_ ^/^ S.type_ ^/^ S.of_ ^/^ Module_expr.pp me
    | Pmty_extension ext -> Extension.pp ext
    | Pmty_alias lid -> longident lid.txt
    | Pmty_strengthen (mty, lid) ->
      pp mty ^/^ S.with_ ^/^ longident lid.txt

  and pp { pmty_desc; pmty_attributes; _ } =
    pp_desc pmty_desc
    |> Attribute.attach ~attrs:pmty_attributes
end

and Functor_parameter : sig
  val pp : functor_parameter -> document
end = struct
  let pp = function
    | Unit -> empty
    | Named (lbl, mty, modes) ->
      begin match lbl.txt with
        | None -> underscore
        | Some s -> string s
      end ^/^ colon ^/^ with_modes (Module_type.pp mty) ~modes
end

and Signature : sig
  val pp : signature -> document
end = struct
  let pp_item_desc = function
    | Psig_value vd -> Value_description.pp vd
    | Psig_type (rf, tds) ->
      S.type_ ^/^
      begin match rf with
        | Recursive -> empty
        | Nonrecursive -> S.nonrec_ ^^ break 1
      end ^^
      separate_map (break 1 ^^ S.and_ ^^ break 1) Type_declaration.pp tds
    | Psig_typesubst tds ->
      S.type_ ^/^
      separate_map (break 1 ^^ S.and_ ^^ break 1)
        (Type_declaration.pp ~subst:true) tds
    | Psig_typext te -> S.type_ ^/^ Type_extension.pp te
    | Psig_exception exn -> Type_exception.pp exn
    | Psig_module md -> S.module_ ^/^ Module_declaration.pp md
    | Psig_modsubst ms -> S.module_ ^/^ Module_substitution.pp ms
    | Psig_recmodule mds ->
      S.module_ ^/^ S.rec_ ^/^
      separate_map (break 1 ^^ S.and_ ^^ break 1)
        Module_declaration.pp mds
    | Psig_modtype mty ->
      S.module_ ^/^ S.type_ ^/^ Module_type_declaration.pp mty
    | Psig_modtypesubst mty ->
      S.module_ ^/^ S.type_ ^/^
      Module_type_declaration.pp ~subst:true mty
    | Psig_open od -> S.open_ ^^ Open_description.pp od
    | Psig_include (incl, modalities) ->
      with_modalities ~modalities (Include_description.pp incl)
    | Psig_class cds ->
      S.class_ ^/^
      separate_map (break 1 ^^ S.and_ ^^ break 1) Class_description.pp cds
    | Psig_class_type ctds ->
      S.class_ ^/^ S.type_ ^/^
      separate_map (break 1 ^^ S.and_ ^^ break 1)
        Class_type_declaration.pp ctds
    | Psig_attribute attr -> Attribute.pp_floating attr
    | Psig_extension (ext, attrs) ->
      Attribute.attach ~attrs (Extension.pp ~floating:true ext)
    | Psig_kind_abbrev (name, k) ->
      S.kind_abbrev_ ^/^ string name.txt ^/^ equals ^/^
        Jkind_annotation.pp k

  let pp_item it = pp_item_desc it.psig_desc

  let pp { psg_modalities ; psg_items ; _ } =
    S.sig_ ^/^
    begin match psg_modalities with
      | [] -> empty
      | lst -> S.atat ^/^ modalities lst ^^ break 1
    end ^^
    separate_map (break 1) pp_item psg_items ^^
    break 1 (* FIXME: 0 when no items *) ^^
    S.end_
end

and Module_declaration : sig
  val pp : module_declaration -> document
end = struct
  let pp { pmd_name; pmd_type; pmd_modalities; pmd_attributes; _ } =
    let name =
      match pmd_name.txt with
      | None -> underscore
      | Some s -> string s
    in
    begin match pmd_modalities with
      | [] -> name
      | l -> name ^/^ at ^/^ modalities l
    end ^/^ equals ^/^ Module_type.pp pmd_type
    |> Attribute.attach ~post:true ~attrs:pmd_attributes
end

and Module_substitution : sig
  val pp : module_substitution -> document
end = struct
  let pp { pms_name; pms_manifest; pms_attributes; _ } =
    string pms_name.txt ^/^ S.colon_equals ^/^ longident pms_manifest.txt
    |> Attribute.attach ~attrs:pms_attributes
end

and Module_type_declaration : sig
  val pp : ?subst:bool -> module_type_declaration -> document
end = struct
  let pp ?(subst=false) { pmtd_name; pmtd_type; pmtd_attributes; _ } =
    string pmtd_name.txt ^^
    begin match pmtd_type with
      | None -> empty
      | Some mty ->
        break 1 ^^ (if subst then S.colon_equals else equals) ^/^
          Module_type.pp mty
    end
    |> Attribute.attach ~post:true ~attrs:pmtd_attributes
end

and Open_infos : sig
  val pp : 'a. ('a -> document) -> 'a open_infos -> document
end = struct
  let pp pp_expr { popen_expr; popen_override; popen_attributes; _ } =
    begin match popen_override with
      | Override -> bang
      | Fresh -> empty
    end ^/^
    pp_expr popen_expr
    |> Attribute.attach ~attrs:popen_attributes
end

and Open_description : sig
  val pp : open_description -> document
end = struct
  let pp = Open_infos.pp (fun lid -> longident lid.txt)
end

and Open_declaration : sig
  val pp : open_declaration -> document
end = struct
  let pp = Open_infos.pp Module_expr.pp
end

and Include_infos : sig
  val pp : 'a. ('a -> document) -> 'a include_infos -> document
end = struct
  let pp pp_mod { pincl_kind ; pincl_mod; pincl_attributes; _ } =
    S.include_ ^/^
    begin match pincl_kind with
      | Functor -> S.functor_ ^^ break 1
      | Structure -> empty
    end ^^
    pp_mod pincl_mod
    |> Attribute.attach ~attrs:pincl_attributes
end

and Include_description : sig
  val pp : include_description -> document
end = struct
  let pp = Include_infos.pp Module_type.pp
end

and Include_declaration : sig
  val pp : include_declaration -> document
end = struct
  let pp = Include_infos.pp Module_expr.pp
end

and With_constraint : sig
  val pp : with_constraint -> document
end = struct
  let pp = function
    | Pwith_type (lid, td) ->
      S.type_ ^/^ longident lid.txt ^^ Type_declaration.pp_rhs td
    | Pwith_module (lid1, lid2) ->
      S.module_ ^/^ longident lid1.txt ^/^ equals ^/^ longident lid2.txt
    | Pwith_modtype (lid, mty) ->
      S.module_ ^/^ S.type_ ^/^ longident lid.txt ^/^ equals ^/^
      Module_type.pp mty
    | Pwith_modtypesubst (lid, mty) ->
      S.module_ ^/^ S.type_ ^/^ longident lid.txt ^/^ S.colon_equals ^/^
      Module_type.pp mty
    | Pwith_typesubst (lid, td) ->
      S.type_ ^/^ longident lid.txt ^^
      Type_declaration.pp_rhs ~subst:true td
    | Pwith_modsubst (lid1, lid2) ->
      S.module_ ^/^ longident lid1.txt ^/^ S.colon_equals ^/^
      longident lid2.txt
end

(** {2 Value expressions for the module language} *)

and Module_expr : sig
  val pp : module_expr -> document
end = struct
  let rec pp_desc = function
    | Pmod_ident lid -> longident lid.txt
    | Pmod_structure str -> Structure.pp str
    | Pmod_functor (fp, me) ->
      S.functor_ ^/^ parens (Functor_parameter.pp fp) ^/^ S.rarrow ^/^ pp me
    | Pmod_apply (m1, m2) ->
      pp m1 ^^ parens (pp m2)
    | Pmod_apply_unit me -> pp me ^^ lparen ^^ rparen
    | Pmod_constraint (me, None, modes) ->
      (* FIXME: parens? shouldn't that be part of cst? *)
      parens (with_modes ~modes (pp me))
    | Pmod_constraint (me, Some mty, atat_modes) ->
      parens (
        pp me ^/^ colon ^/^ Module_type.pp mty ^^
        begin match atat_modes with
          | [] -> empty
          | l -> break 1 ^^ S.atat ^/^ modes l
        end
      )
    | Pmod_unpack e -> parens (S.val_ ^/^ Expression.pp e)
    | Pmod_extension ext -> Extension.pp ext

  and pp { pmod_desc; pmod_attributes; _ } =
    pp_desc pmod_desc
    |> Attribute.attach ~attrs:pmod_attributes
end

and Structure : sig
  val pp : structure -> document
  val pp_implementation : structure -> document
end = struct
  let pp_item_desc = function
    | Pstr_eval (e, attrs) -> Attribute.attach ~attrs (Expression.pp e)
    | Pstr_value (rf, vbs) ->
      (* FIXME: factorize Pexp_let *)
      S.let_ ^/^
      begin match rf with
        | Nonrecursive -> empty
        | Recursive -> S.rec_ ^^ break 1
      end ^^
      separate_map (break 1 ^^ S.and_ ^^ break 1) Value_binding.pp vbs
    | Pstr_primitive vd -> Value_description.pp vd
    (* FIXME: factorize with Psig_* *)
    | Pstr_type (rf, tds) ->
      S.type_ ^/^
      begin match rf with
        | Recursive -> empty
        | Nonrecursive -> S.nonrec_ ^^ break 1
      end ^^
      separate_map (break 1 ^^ S.and_ ^^ break 1) Type_declaration.pp tds
    | Pstr_typext te -> S.type_ ^/^ Type_extension.pp te
    | Pstr_exception exn -> Type_exception.pp exn
    | Pstr_module mb -> S.module_ ^/^ Module_binding.pp mb
    | Pstr_recmodule mbs ->
      S.module_ ^/^ S.rec_ ^/^
      separate_map (break 1 ^^ S.and_ ^^ break 1) Module_binding.pp mbs
    | Pstr_modtype mty ->
      S.module_ ^/^ S.type_ ^/^ Module_type_declaration.pp mty
    | Pstr_open od -> S.open_ ^^ Open_declaration.pp od
    | Pstr_class cds ->
      S.class_ ^/^
      separate_map (break 1 ^^ S.and_ ^^ break 1) Class_declaration.pp cds
    | Pstr_class_type ctds ->
      S.class_ ^/^ S.type_ ^/^
      separate_map (break 1 ^^ S.and_ ^^ break 1)
        Class_type_declaration.pp
        ctds
    | Pstr_include incl -> Include_declaration.pp incl
    | Pstr_attribute a -> Attribute.pp_floating a
    | Pstr_extension (ext, attrs) ->
      Attribute.attach ~attrs (Extension.pp ~floating:true ext)
    | Pstr_kind_abbrev (name, k) ->
      S.kind_abbrev_ ^/^ string name.txt ^/^ equals ^/^
      Jkind_annotation.pp k

  let pp_item it = pp_item_desc it.pstr_desc

  let pp items =
    S.struct_ ^/^
    separate_map (break 1) pp_item items ^/^
    S.end_

  let pp_implementation = separate_map (break 1) pp_item
end

and Value_constraint : sig
  val pp : value_constraint -> document
end = struct
  let pp = function
    | Pvc_constraint { locally_abstract_univars; typ } ->
      colon ^/^
      begin match locally_abstract_univars with
        | [] -> empty
        | vars ->
          let pp_var (name, jkind) =
            string name.txt ^^
            match jkind with
            | None -> empty
            | Some j -> break 1 ^^ Jkind_annotation.pp j
          in
          S.type_ ^/^
          separate_map (break 1) pp_var vars ^^ dot ^^ break 1
      end ^^
      Core_type.pp typ
    | Pvc_coercion {ground; coercion} ->
      begin match ground with
        | None -> empty
        | Some ct -> colon ^/^ Core_type.pp ct ^^ break 1
      end ^^
      S.coerce ^/^ Core_type.pp coercion
end

and Value_binding : sig
  val pp : value_binding -> document
end = struct
  let pp { pvb_modes; pvb_pat; pvb_params; pvb_constraint;
           pvb_ret_modes; pvb_expr; pvb_attributes;
           pvb_loc = _ } =
    begin match pvb_modes with
      | [] -> empty
      | lst -> modes lst ^^ break 1
    end ^^
    Pattern.pp pvb_pat ^^
    begin match pvb_params with
      | [] -> empty
      | params ->
        break 1 ^^ separate_map (break 1) Function_param.pp params
    end ^^
    begin match pvb_constraint with
      | None -> empty
      | Some vc ->
        break 1 ^^ Value_constraint.pp vc
        |> with_modes ~modes:pvb_ret_modes
    end ^^
    begin match pvb_expr with
      | None -> empty
      | Some e -> break 1 ^^ equals ^/^ Expression.pp e
    end
    |> Attribute.attach ~attrs:pvb_attributes (* FIXME: post? *)
end

and Module_binding : sig
  val pp : module_binding -> document
end = struct
  let pp { pmb_name; pmb_expr; pmb_attributes; _ } =
    begin match pmb_name.txt with
      | None -> underscore
      | Some s -> string s
    end ^/^ equals ^/^ Module_expr.pp pmb_expr
    |> Attribute.attach ~attrs:pmb_attributes
end

and Jkind_annotation : sig
  val pp : jkind_annotation -> document
end = struct
  let jkind_annotation_desc = function
    | Default -> underscore
    | Abbreviation s -> string s
    | Mod (jk, ms) -> Jkind_annotation.pp jk ^/^ S.mod_ ^/^ modes ms
    | With (jk, ct, modalities) ->
      Jkind_annotation.pp jk ^/^ S.with_ ^/^ Core_type.pp ct
      |> with_modalities ~modalities
    | Kind_of ct -> S.kind_of_ ^/^ Core_type.pp ct
    | Product jks ->
      separate_map (break 1 ^^ ampersand ^^ break 1) Jkind_annotation.pp jks

  let pp jk = jkind_annotation_desc jk.pjkind_desc
end

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
