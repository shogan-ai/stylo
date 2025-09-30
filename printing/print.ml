open Wrapprint
open Ocaml_syntax
open Parsetree

module S = Syntax

let enclose l r d = group (l ^^ break 0 ^^ d ^^ break 0 ^^ r)

let parens d = (* fixme: nest 1?? *)
  group (S.lparen ^^ nest 1 d ^^ S.rparen)
let brackets = enclose S.lbracket S.rbracket

let dquotes d = S.dquote ^^ d ^^ S.dquote

let optional f v_opt =
  match v_opt with
  | None -> empty
  | Some v -> f v

let nb_semis =
  List.fold_left (fun nb tok ->
    if tok.Tokens.desc = Token SEMI then nb + 1 else nb
  ) 0

let rec starts_with tok = function
  | Tokens.{ desc = Token t; _ } :: _ -> t = tok
  | Tokens.{ desc = Comment _; _ } :: rest -> starts_with tok rest
  | _ -> false

let rec has_leading tok ~after:after_kw = function
  | [] -> false
  | Tokens.{ desc = Token t; _ } :: rest when t = after_kw ->
    starts_with tok rest
  | _ :: rest -> has_leading tok ~after:after_kw rest

let starts_with_pipe = starts_with BAR
let has_leading_pipe ~after = has_leading BAR ~after

let rec pipe_before_child = function
  | []
  | Tokens.{ desc = Child_node; _ } :: _ -> false
  | Tokens.{ desc = Token BAR; _ } :: _ -> true
  | _ :: rest -> pipe_before_child rest

type 'a loc = 'a Location.loc = { txt: 'a; loc: Location.t }
let stringf fmt = Printf.ksprintf string fmt

let str_or_op (so : Longident.str_or_op) =
  group @@
  match so with
  | Str s -> string s
  | Str_trailing_hash s -> string s ^^ S.hash
  | Op s ->
    let s =
      if String.get s 0 = '*' || String.get s (String.length s - 1) = '*' then
        break 1 ^^ string s ^^ break 1
      else
        string s
    in
    parens s
  | DotOp (op, paren_kind, index_mod, assign) ->
    let index_mod = if index_mod = "" then empty else S.semi ^^ S.dotdot in
    let left, right =
      match paren_kind with
      | `Brace -> S.lbrace, S.rbrace
      | `Bracket -> S.lbracket, S.rbracket
      | `Paren -> S.lparen, S.rparen
    in
    let assign = if assign then S.larrow else empty in
    parens (stringf ".%s" op ^^ left ^^ index_mod ^^ right ^^ assign)

let rec longident (l : Longident.t) =
  match l.desc with
  | Lident so -> str_or_op so
  | Ldot (lid, s) -> longident lid ^^ S.dot ^^ str_or_op s
  | Lapply (l1, l2) ->
    longident l1 ^^ S.lparen ^^ break 0 ^^ longident l2 ^^ break 0 ^^ S.rparen

let constr_ident = function
  | Longident.Str "[]" -> S.lbracket ^^ S.rbracket
  | Str "()" -> S.lparen ^^ S.rparen
  | so -> str_or_op so

let constr_longident (cstr : Longident.t) =
  match cstr.desc with
  | Lident (Str "[]") -> S.lbracket ^^ S.rbracket
  | Lident (Str "()") -> S.lparen ^^ S.rparen
  | _ -> longident cstr

let direction = function
  | Asttypes.Upto -> S.to_
  | Downto -> S.downto_

let private_ = function
  | Asttypes.Private -> S.private_
  | Public -> empty

let rec_ = function
  | Asttypes.Recursive -> S.rec_
  | Nonrecursive -> empty

let nonrec_ = function
  | Asttypes.Recursive -> empty
  | Nonrecursive -> S.nonrec_

let mutable_ = function
  | Asttypes.Mutable -> S.mutable_ ^^ break 1
  | Immutable -> empty

let virtual_ = function
  | Asttypes.Virtual -> S.virtual_
  | Concrete -> empty

let virtual_field = function
  | Cfk_virtual _ -> S.virtual_ ^^ break 1
  | Cfk_concrete _ -> empty

let override_= function
  | Asttypes.Fresh -> empty
  | Override -> S.bang

let array_delimiters = function
  | Asttypes.Immutable -> S.lbracket_colon, S.colon_rbracket
  | Mutable -> S.lbracket_pipe, S.pipe_rbracket

let type_app ?(omit_delims_if_possible=false) ?(parens=true) ty args =
  let left, right =
    if parens then
      S.lparen, S.rparen
    else
      S.lbracket, S.rbracket
  in
  begin match args with
    | [] -> empty
    | [ x ] when omit_delims_if_possible -> x
    | _ -> left ^^ separate (S.comma ^^ break 1) args ^^ right
  end ^?^ ty

(* N.B. stringf is important here: suffixed number come out of the lexer as a
   single token. We can't use ^^ here. *)
let constant = function
  | Pconst_float (sign, nb, None)
  | Pconst_integer (sign, nb, None) -> optional string sign ^^ string nb
  | Pconst_float (sign, nb, Some suffix)
  | Pconst_integer (sign, nb, Some suffix) ->
    optional string sign ^^ stringf "%s%c" nb suffix
  | Pconst_unboxed_integer (sign, nb, suffix) ->
    optional string sign ^^ stringf "#%s%c" nb suffix
  | Pconst_unboxed_float (sign, nb, None) ->
    optional string sign ^^ stringf "#%s" nb
  | Pconst_unboxed_float (sign, nb, Some suffix) ->
    optional string sign ^^ stringf "#%s%c" nb suffix
  | Pconst_char c -> stringf "'%s'" (Char.escaped c)
  | Pconst_untagged_char c -> stringf "#'%s'" (Char.escaped c)
  (* FIXME: handling of string literals is not good enough. *)
  | Pconst_string (s, _, None) -> stringf "%S" s
  | Pconst_string (s, _, Some delim) -> stringf "{%s|%s|%s}" delim s delim

let separate_loc_list sep f = separate_map sep (fun l -> f l.txt)

let modality (Modality s) = string s
let modalities = separate_loc_list (break 1) modality

let with_modalities ~modalities:l t =
  match l with
  | [] -> t
  | _ -> t ^?^ S.atat ^/^ modalities l

let mode (Mode s) = string s
let modes = separate_loc_list (break 1) mode

let mode_legacy = function
  | Mode ("local" | "once" | "unique" as s) -> stringf "%s_" s
  | _ -> assert false
let modes_legacy = separate_loc_list (break 1) mode_legacy

let with_modes ~modes:l t =
  match l with
  | [] -> t
  | _ -> t ^?^ S.at ^/^ modes l

let with_atat_modes ~modes:l t =
  match l with
  | [] -> t
  | _ -> t ^?^ S.atat ^/^ modes l

let include_kind = function
  | Structure -> empty
  | Functor -> S.functor_

module rec Attribute : sig
  val pp : ?item:bool -> attribute -> document
  val pp_floating : attribute -> document
  val pp_list : ?item:bool -> attributes -> document

  val pp_attr_name : string list loc -> document

  val attach
    :  ?item:bool
    -> ?text:attributes
    -> ?pre_doc:attribute
    -> ?post_doc:attribute
    -> attrs:attributes
    -> document
    -> document
end = struct
  let pp_doc : payload -> document = function
    | PStr ([ {
      pstr_desc =
        Pstr_eval
          ({ pexp_desc = Pexp_constant Pconst_string (s, _loc, None); _ }, []);
      _ } ], _)
      ->
      docstring s
    | _ -> assert false

  let pp_attr_name ids =
    let pp_one = function
      | "let" -> S.let_
      | s -> string s
    in
    separate_map S.dot pp_one ids.txt

  let pp lbat name payload =
    group @@ nest 2 (
      lbat ^^ pp_attr_name name ^^ Payload.pp payload ^^ S.rbracket
    )

  let pp_floating { attr_name; attr_payload; attr_loc = _; attr_tokens = _ } =
    match attr_name.txt with
    | ["ocaml"; ("doc"|"text")]  (* FIXME *) -> pp_doc attr_payload
    | _ -> pp S.lbracket_atatat attr_name attr_payload

  let pp ?(item=false)
      { attr_name; attr_payload; attr_loc = _; attr_tokens = _ } =
    match attr_name.txt with
    | ["ocaml"; ("doc"|"text")]  (* FIXME *) -> pp_doc attr_payload
    | _ ->
      pp (if item then S.lbracket_atat else S.lbracket_at) attr_name
        attr_payload

  let pp_list ?item l = separate_map (break 0) (pp ?item) l

  let attach ?item ?(text = []) ?pre_doc ?post_doc ~attrs t =
    begin match text with
    | [] -> empty
    | text ->
      hardline ^^ hardline ^^
      separate_map (break 1) pp text ^^
      hardline ^^ hardline
    end ^^
    optional pp pre_doc ^?/^
    t ^?^ pp_list ?item attrs ^?^
    optional pp post_doc
end

and Ext_attribute : sig
  val decorate : document -> ext_attribute -> document
end = struct
  let decorate kw { pea_ext; pea_attrs } =
    Attribute.attach ~attrs:pea_attrs (
      match pea_ext with
      | None -> kw
      | Some lst_loc ->
        kw ^^ string "%" ^^
        separate_map (group (S.dot ^^ break 0)) string lst_loc.txt
    )
end

and Extension : sig
  val pp : ?floating:bool -> extension -> document
end = struct
  let pp_classic ~floating names payload =
    let name = Attribute.pp_attr_name names in
    (if floating then S.lbracket_percentpercent else S.lbracket_percent)
    ^^ name ^^ Payload.pp payload ^^ S.rbracket

  let pp ?(floating=false) (ext_name, ext_payload, _tokens) =
    match ext_payload with
    | PString (s, delim) ->
      let ext_name = String.concat "." ext_name.txt in
      (* Careful: single token! *)
      (* FIXME: dedicated printer, same as string literals *)
      let percent = if floating then "%%" else "%" in
      let blank = if delim <> "" then " " else "" in
      stringf "{%s%s%s%s|%s|%s}" percent ext_name blank delim s delim
    | _ -> pp_classic ~floating ext_name ext_payload
end

and Payload : sig
  val pp : payload -> document
end = struct
(* FIXME: can't finish by '>', a space must be added *)
  let pp = function
    | PString _ -> assert false (* handled in Extension *)
    | PStr s -> break 1 ^^ Structure.pp_implementation s
    | PSig s -> break 0 ^^ S.colon ^/^ Signature.pp_interface s
    | PTyp c -> break 0 ^^ S.colon ^/^ Core_type.pp c
    | PPat (p, eo) ->
      break 0 ^^ S.qmark ^/^ Pattern.pp p
      ^^ match eo with
      | None -> empty
      | Some e -> break 1 ^^ S.when_ ^/^ Expression.pp e
end

(** {1 Core language} *)
(** {2 Type expressions} *)

and Core_type : sig
  val pp : core_type -> document

  val pp_arrow: Tokens.seq -> arg_label -> document -> document -> document

  val pp_poly_bindings : (string loc * jkind_annotation option) list -> document
end = struct
  let pp_arrow tokens arg_lbl arg rhs =
    begin match arg_lbl with
    | Nolabel -> empty
    | Labelled s -> string s ^^ S.colon
    | Optional s ->
      if
        List.exists (function
          | Tokens.{ desc = Token OPTLABEL _; _ } -> true
          | _ -> false
        ) tokens
      then stringf "?%s:" s
      else S.qmark ^^ string s ^^ S.colon
    end ^^
    arg ^/^ S.rarrow ^/^ rhs

  let rec pp { ptyp_desc; ptyp_attributes; ptyp_doc; ptyp_tokens; ptyp_loc=_} =
    group (pp_desc ptyp_tokens ptyp_desc)
    |> Attribute.attach ~attrs:ptyp_attributes ?post_doc:ptyp_doc

  and pp_desc tokens = function
    | Ptyp_any None -> S.underscore
    | Ptyp_any Some k -> S.underscore ^/^ S.colon ^/^ Jkind_annotation.pp k
    | Ptyp_var (s, ko) -> pp_var s ko
    | Ptyp_arrow { lbl; dom_legacy_modes; dom_type; dom_modes;
                   codom_legacy_modes; codom_type; codom_modes } ->
      let pp_surrounded pre_m post_m ty =
        modes_legacy pre_m ^?^ pp ty
        |> with_modes ~modes:post_m
      in
      pp_arrow tokens lbl
        (pp_surrounded dom_legacy_modes dom_modes dom_type)
        (pp_surrounded codom_legacy_modes codom_modes codom_type)
    | Ptyp_tuple elts -> pp_tuple elts
    | Ptyp_unboxed_tuple elts -> S.hash_lparen ^^ pp_tuple elts ^^ S.rparen
    | Ptyp_constr (args, lid) ->
      let omit_delims_if_possible = not (starts_with LPAREN tokens) in
      type_app ~omit_delims_if_possible (longident lid.txt) (List.map pp args)
    | Ptyp_object (fields, closed) ->
      let field_doc = separate_map (S.semi ^^ break 1) object_field fields in
      S.lt ^/^
      begin match fields, closed with
        | [], Open -> S.dotdot
        | _, Open -> field_doc ^^ S.semi ^/^ S.dotdot
        | _, Closed -> field_doc
      end ^?^ S.gt
    | Ptyp_class (lid, args) ->
      type_app (S.hash ^^ longident lid.txt)
        (List.map pp args)
    | Ptyp_alias (ct, name, None) ->
      pp ct ^/^ S.as_ ^/^
      S.squote ^^ string (Option.get name).txt
    | Ptyp_alias (ct, name_o, Some jkind) ->
      pp ct ^/^ S.as_ ^/^
      S.lparen ^^ break 0 ^^ (
        match name_o with
        | None -> S.underscore
        | Some s -> S.squote ^^ string s.txt
      ) ^^ S.colon ^^ Jkind_annotation.pp jkind ^^ break 0 ^^ S.rparen
    | Ptyp_variant (fields, cf, lbls) ->
      pp_variant ~tokens fields cf lbls
    | Ptyp_poly (bound_vars, ct) ->
      pp_poly_bindings bound_vars ^^ break 0 ^^ S.dot ^/^ pp ct
    | Ptyp_package pkg -> package_type pkg
    | Ptyp_open (lid, ct) -> longident lid.txt ^^ S.dot ^^ pp ct
    | Ptyp_of_kind jkind -> S.type_ ^/^ S.colon ^/^ Jkind_annotation.pp jkind
    | Ptyp_extension ext -> Extension.pp ext
    | Ptyp_parens ct -> parens (pp ct)

  and pp_var var_name jkind =
    let var = S.squote ^^ string var_name in
    match jkind with
    | None -> var
    | Some k -> var ^/^ S.colon ^/^ Jkind_annotation.pp k

  and pp_poly_bindings bound_vars =
    let pp_bound (var, jkind) =
      let var_and_kind = pp_var var.Location.txt jkind in
      match jkind with
      | None -> var_and_kind
      | Some _ -> parens var_and_kind
    in
    separate_map (break 1) pp_bound bound_vars

  and pp_variant ~tokens fields (cf : Asttypes.closed_flag) lbls =
    let fields =
      (if pipe_before_child tokens then S.pipe else empty) ^?^
      separate_map (break 1 ^^ S.pipe ^^ break 1) row_field fields
    in
    match cf, lbls with
    | Closed, None -> S.lbracket ^/^ fields  ^/^ S.rbracket
    | Open, None -> S.lbracket_gt ^/^ fields ^?^ S.rbracket
    | Closed, Some [] -> S.lbracket_lt ^/^ fields ^/^ S.rbracket
    | Closed, Some labels ->
      S.lbracket_lt ^/^ fields ^/^
      S.gt ^/^ separate_map (break 1) pp_poly_tag labels ^/^
      S.rbracket
    | Open, Some _ -> assert false

  and row_field rf =
    group (
      row_field_desc rf.prf_desc
      |> Attribute.attach ~attrs:rf.prf_attributes
    )

  and row_field_desc = function
    | Rinherit ct -> pp ct
    | Rtag (label, _, []) -> pp_poly_tag label.txt
    | Rtag (label, has_const, at_types) ->
      prefix (group (pp_poly_tag label.txt ^/^ S.of_)) (
        (if has_const then S.ampersand ^^ break 1 else empty) ^^
        separate_map (break 1 ^^ S.ampersand ^^ break 1) pp at_types
      )

  and pp_poly_tag lbl = S.bquote ^^ string lbl


  and pp_tuple elts =
    let pp_elt (lbl_opt, ct) =
      begin match lbl_opt with
      | None -> empty
      | Some s -> string s ^^ S.colon ^^ break 0
      end ^^ pp ct
    in
    separate_map (break 1 ^^ S.star ^^ break 1) pp_elt elts

  and package_type { ppt_ext_attr; ppt_name = lid; ppt_eqs = constraints } =
    let with_ =
      match constraints with
      | [] -> empty
      | _ ->
        let one (lid, ct) = longident lid.txt ^/^ S.equals ^/^ pp ct in
        break 1 ^^ S.with_ ^/^ S.type_ ^/^
        separate_map (break 1 ^^ S.and_ ^^ break 1) one constraints
    in
    let module_ =
      match ppt_ext_attr with
      | None -> S.module_
      | Some ea -> Ext_attribute.decorate S.module_ ea
    in
    S.lparen ^^ module_ ^/^ longident lid.txt ^^ with_ ^^ break 0 ^^ S.rparen

  and object_field of_ =
    Attribute.attach ~attrs:of_.pof_attributes (object_field_desc of_.pof_desc)

  and object_field_desc = function
    | Oinherit ct -> pp ct
    | Otag (lbl, ct) -> string lbl.txt ^^ S.colon ^/^ pp ct
end

(** {2 Patterns} *)

and Pattern : sig
  val pp : pattern -> document
end = struct
  let rec pp p =
    group (pp_desc p)
    |> Attribute.attach ~attrs:p.ppat_attributes

  and pp_desc p =
    let (!!) kw = Ext_attribute.decorate kw p.ppat_ext_attr in
    match p.ppat_desc with
    | Ppat_any -> S.underscore
    | Ppat_var name -> str_or_op name.txt
    | Ppat_alias (p, alias) ->
      prefix (pp p) (group (S.as_ ^/^ str_or_op alias.txt))
    | Ppat_constant c -> constant c
    | Ppat_interval (c1,c2) -> constant c1 ^/^ S.dotdot ^/^ constant c2
    | Ppat_tuple (elts, closed) -> pp_tuple closed elts
    | Ppat_unboxed_tuple (elts, cf) ->
      S.hash_lparen ^^ nest 1 (pp_tuple cf elts) ^^ S.rparen
    | Ppat_construct (lid, arg) -> pp_construct p.ppat_tokens lid arg
    | Ppat_variant (lbl, None) -> S.bquote ^^ string lbl
    | Ppat_variant (lbl, Some p) -> S.bquote ^^ string lbl ^/^ pp p
    | Ppat_record (fields, cf) -> pp_record (nb_semis p.ppat_tokens) cf fields
    | Ppat_record_unboxed_product (fields, cf) ->
      pp_record ~unboxed:true (nb_semis p.ppat_tokens) cf fields
    | Ppat_array (mut, ps) -> pp_array (nb_semis p.ppat_tokens) mut ps
    | Ppat_or (p1, p2) -> pp p1 ^/^ group (S.pipe ^/^ pp p2)
    | Ppat_constraint (p, None, modes) ->
      parens (with_modes ~modes (pp p))
    | Ppat_constraint (p, Some ty, atat_modes) ->
      parens (
        pp p ^/^ S.colon ^/^ Core_type.pp ty ^^
        match atat_modes with
        | [] -> empty
        | lst -> break 1 ^^ S.atat ^/^ modes lst
      )
    (* FIXME: parser doesn't agree with what's written above I believe.
       Recognized form seems to depend on context... *)
    | Ppat_type lid -> S.hash ^^ longident lid.txt
    | Ppat_lazy p -> !!S.lazy_ ^/^ pp p
    | Ppat_unpack (path, ty) -> pp_unpack p.ppat_ext_attr path ty
    | Ppat_exception p -> !!S.exception_ ^/^ pp  p
    | Ppat_extension ext -> Extension.pp ext
    | Ppat_open (lid, p) -> longident lid.txt ^^ S.dot ^^ pp p
    | Ppat_parens p -> parens (pp p)
    | Ppat_list elts -> pp_list (nb_semis p.ppat_tokens) elts
    | Ppat_cons (hd, tl) -> pp hd ^/^ S.cons ^/^ pp tl

  and pp_delimited_seq (opn, cls) nb_semis elts =
    let semi_as_term = List.compare_length_with elts nb_semis = 0 in
    let elts =
      if semi_as_term then
        separate_map (break 1) (fun elt -> pp elt ^^ S.semi) elts
      else
        separate_map (S.semi ^^ break 1) pp elts
    in
    prefix_nonempty opn elts ^?^ cls

  and pp_array nb_semis mut = pp_delimited_seq (array_delimiters mut) nb_semis

  and pp_list nb_semis = pp_delimited_seq (S.lbracket, S.rbracket) nb_semis

  and pp_unpack ext_attrs path ty =
    let path =
      match path.txt with
      | None -> S.underscore
      | Some s -> string s
    in
    parens (
      Ext_attribute.decorate S.module_ ext_attrs ^/^ path ^?^
      optional (fun c -> S.colon ^/^ Module_expr.pp_package_type c) ty
    )

  and pp_tuple closed elts =
    separate_map (break 0 ^^ S.comma ^^ break 1) (Argument.pp pp) elts ^^
    begin match closed with
      | Closed -> empty
      | Open -> break 0 ^^ S.comma ^^ break 1 ^^ S.dotdot
    end

  and pp_construct tokens name arg_opt =
    let name = constr_longident name.txt in
    let pp_annotated_newtype nt jkind =
      string nt.txt ^/^ S.colon ^/^ Jkind_annotation.pp jkind
    in
    match arg_opt with
    | None -> name
    | Some ([], arg_pat) -> prefix name (pp arg_pat)
    | Some ([newtype, Some jkind], arg_pat)
      when not (has_leading LPAREN ~after:TYPE tokens) ->
      (* We could decide to "normalize" this case.
         Here we are careful because we don't want to trigger if the user wrote
         {[
           Constr (type (a : jk)) arg
         ]} *)
      prefix name (
        parens (S.type_ ^/^ pp_annotated_newtype newtype jkind) ^/^
        pp arg_pat
      )
    | Some (bindings, arg_pat) ->
      let binding (newtype, jkind) =
        match jkind with
        | None -> string newtype.txt
        | Some jkind -> parens (pp_annotated_newtype newtype jkind)
      in
      prefix name (
        parens (S.type_ ^/^ flow_map (break 1) binding bindings) ^/^
        pp arg_pat
      )

  and pp_record ?(unboxed=false) nb_semis closed_flag fields =
    let semi_as_term =
      let nb_fields =
        List.length fields +
        if closed_flag = Closed then 0 else 1 (* underscore as extra field *)
      in
      (* [;] is used as a terminator if there are as many as there are fields *)
      nb_fields = nb_semis
    in
    prefix (if unboxed then S.hash_lbrace else S.lbrace) (
      Record_field.pp_list ~semi_as_term pp fields ^^
      match closed_flag with
      | Asttypes.Closed -> empty
      | Open when semi_as_term -> S.underscore ^/^ S.semi
      | Open -> S.semi ^/^ S.underscore
    ) ^/^
    S.rbrace

end

(** {2 Value expressions} *)

and Expression : sig
  val pp : expression -> document

  val pp_function_parts :
    expression -> document * document
end = struct
  let pp_op op =
    match op.pexp_desc with
    | Pexp_ident { txt = { desc = Lident Op s; _ }; _ } ->
      string s
    | _ -> assert false

  let rec pp e =
    pp_desc e
    |> Attribute.attach ~attrs:e.pexp_attributes

  and pp_desc exp =
    let (!!) kw = Ext_attribute.decorate kw exp.pexp_ext_attr in
    match exp.pexp_desc with
    | Pexp_ident lid -> longident lid.txt
    | Pexp_constant c -> constant c
    | Pexp_let (mf, rf, vbs, body) -> pp_let mf rf vbs body
    | Pexp_function _ -> pp_function exp
    | Pexp_prefix_apply (op, arg) -> pp_op op ^^ pp arg
    | Pexp_add_or_sub (op, arg) -> string op ^^ pp arg
    | Pexp_infix_apply {op; arg1; arg2} ->
      (* N.B. the associativity of [op] will impact the nesting... *)
      pp arg1 ^/^ pp_op_apply op arg2
    | Pexp_apply (e, args) -> pp_apply e args
    | Pexp_match (e, cases) ->
      group (!!S.match_ ^^ nest 2 (group (break 1 ^^ pp e)) ^/^ S.with_) ^/^
      Case.pp_cases cases
        ~has_leading_pipe:(has_leading_pipe ~after:WITH exp.pexp_tokens)
    | Pexp_try (e, cases) ->
      prefix !!S.try_ (pp e) ^/^ S.with_ ^/^
      Case.pp_cases cases
        ~has_leading_pipe:(has_leading_pipe ~after:WITH exp.pexp_tokens)
    | Pexp_tuple elts -> pp_tuple elts
    | Pexp_unboxed_tuple elts ->
      S.hash_lparen ^^ nest 1 (pp_tuple elts) ^^ S.rparen
    | Pexp_construct (lid, arg) ->
      prefix_nonempty (constr_longident lid.txt) (optional pp arg)
    | Pexp_variant (lbl, eo) -> pp_variant lbl eo
    | Pexp_record (eo, fields) -> pp_record (nb_semis exp.pexp_tokens) eo fields
    | Pexp_record_unboxed_product (eo, fields) ->
      pp_record ~unboxed:true (nb_semis exp.pexp_tokens) eo fields
    | Pexp_field (e, lid) -> pp e ^^ S.dot ^^ longident lid.txt
    | Pexp_unboxed_field (e, lid) ->
      pp e ^^ S.dothash ^^ longident lid.txt
    | Pexp_setfield (e1, lid, e2) ->
      pp e1 ^^ S.dot ^^ longident lid.txt ^/^ S.larrow ^/^ pp e2
    | Pexp_array (mut, es) -> pp_array (nb_semis exp.pexp_tokens) mut es
    | Pexp_idx (ba, uas) -> pp_block_idx ba uas
    | Pexp_ifthenelse (e1, e2, e3_o) -> pp_ite exp.pexp_ext_attr e1 e2 e3_o
    | Pexp_sequence (e1, e2) ->
      (* FIXME: ext_attr not at the beginning, the token synchronisation is
         going to have issues. *)
      pp e1 ^^ !!S.semi ^/^ pp e2
    | Pexp_seq_empty e -> pp e ^^ S.semi
    | Pexp_while (e1, e2) ->
      !!S.while_ ^/^ pp e1 ^/^ S.do_ ^/^ pp e2 ^/^
      S.done_
    | Pexp_for (p, e1, e2, dir, e3) ->
      let fst_line =
        prefix !!S.for_ (
          group (
            Pattern.pp p ^/^ S.equals ^/^ pp e1 ^/^ direction dir ^/^ pp e2
          )
        ) ^/^ S.do_
      in
      group (prefix fst_line (pp e3) ^/^ S.done_)
    | Pexp_constraint (e, None, modes) ->
      with_modes ~modes (pp e)
    | Pexp_constraint (e, Some ct, atat_modes) ->
      parens (
        pp e ^/^ S.colon ^/^ Core_type.pp ct ^^
        match atat_modes with
        | [] -> empty
        | lst -> break 1 ^^ S.atat ^/^ modes lst
      )
    | Pexp_coerce (e, ct1, ct2) ->
      let ct1 =
        match ct1 with
        | None -> empty
        | Some ct -> break 1 ^^ S.colon ^/^ Core_type.pp ct
      in
      parens (pp e ^^ ct1 ^/^ S.coerce ^/^ Core_type.pp ct2)
    | Pexp_send (e, lbl) -> pp e ^/^ S.hash ^/^ string lbl.txt
    | Pexp_new lid -> !!S.new_ ^/^ longident lid.txt
    | Pexp_setvar (lbl, e) -> string lbl.txt ^/^ S.larrow ^/^ pp e
    | Pexp_override fields ->
      let field (lbl, eo) =
        string lbl.txt ^^
        begin match eo with
          | None -> empty
          | Some e -> break 1 ^^ S.equals ^/^ pp e
        end
      in
      S.lbrace_lt ^/^
      separate_map (S.semi ^^ break 1) field fields ^/^
      S.gt_rbrace
    | Pexp_letmodule (name, me, body) ->
      let name =
        match name.txt with
        | None -> S.underscore
        | Some s -> string s
      in
      S.let_ ^/^ !!S.module_ ^/^ name ^/^ S.equals ^/^
      Module_expr.pp me ^/^ S.in_ ^/^
      pp body
    | Pexp_letexception (ec, body) ->
      S.let_ ^/^ !!S.exception_ ^/^
      Extension_constructor.pp ec ^/^ S.in_ ^/^
      pp body
    | Pexp_assert e -> !!S.assert_ ^/^ pp e
    | Pexp_lazy e -> !!S.lazy_ ^/^ pp e
    | Pexp_object cs -> Class_expr.pp_structure exp.pexp_ext_attr cs
    | Pexp_pack (me, ty) ->
      S.lparen ^^ !!S.module_ ^/^ Module_expr.pp me ^^
      optional (fun c ->
        break 1 ^^ S.colon ^/^ Module_expr.pp_package_type c) ty ^^
      S.rparen
    | Pexp_dot_open (lid, e) -> longident lid.txt ^^ S.dot ^^ pp e
    | Pexp_let_open (od, e) ->
      group (S.let_ ^/^ Open_declaration.pp ~item:false od ^/^ S.in_) ^/^ pp e
    | Pexp_letop lo -> Letop.pp lo
    | Pexp_extension ext -> Extension.pp ext
    | Pexp_unreachable  -> S.dot
    | Pexp_stack e -> S.stack__ ^/^ pp e
    | Pexp_comprehension ce -> Comprehension.pp_expr ce
    | Pexp_overwrite (e1, e2) ->
      S.overwrite__ ^/^ pp e1 ^/^ S.with_ ^/^ pp e2
    | Pexp_hole -> S.underscore
    | Pexp_index_op access ->
      pp_index_op (nb_semis exp.pexp_tokens)
        access.kind access.seq access.op access.indices access.assign
    | Pexp_parens { begin_end = false; exp = Some exp } -> parens (pp exp)
    | Pexp_parens { begin_end = false; exp = None } -> assert false
    | Pexp_parens { begin_end = true; exp } ->
      prefix_nonempty !!S.begin_ (optional pp exp) ^/^ S.end_
    | Pexp_list elts -> pp_list (nb_semis exp.pexp_tokens) elts
    | Pexp_cons (hd, tl) -> pp hd ^/^ S.cons ^/^ pp tl
    | Pexp_exclave exp -> S.exclave__ ^/^ pp exp
    | Pexp_mode_legacy (m, exp) -> mode_legacy m.txt ^/^ pp exp

  and pp_let mf rf vbs body =
    group (
      Value_binding.pp_list vbs ~start:(
        S.let_ ::
        match mf, rf with
        | Immutable, Nonrecursive -> []
        | Immutable, Recursive -> [S.rec_]
        | Mutable, Nonrecursive -> [S.mutable_]
        | Mutable, Recursive -> [S.mutable_; S.rec_]
      ) ^/^ S.in_
    ) ^/^ pp body

  and pp_function_parts exp =
    match exp.pexp_desc with
    | Pexp_function ([], _, body) ->
      (* exp.pexp_ext_attr is empty in this case, we attach on the body. *)
      begin match Function_body.as_rhs body with
      | Three_parts { start; main; stop = _ } ->
        (* we know stop is empty *)
        start, main
      | _ -> assert false
      end
    | Pexp_function (params, constr, body) ->
      let params = flow_map (break 1) Function_param.pp params in
      let constr = Function_constraint.pp constr in
      prefix
        (Ext_attribute.decorate S.fun_ exp.pexp_ext_attr)
        (group (params ^/^ group (constr ^?^ S.rarrow))),
      Function_body.pp body
    | _ -> assert false

  and pp_function exp =
    let (fun_and_params, body) = pp_function_parts exp in
    prefix fun_and_params body

  and pp_index_op nb_semis kind seq op indices assign =
    let delims =
      match kind with
      | Paren -> S.lparen, S.rparen
      | Brace -> S.lbrace, S.rbrace
      | Bracket -> S.lbracket, S.rbracket
    in
    pp seq ^/^
    begin match op with
    | None -> S.dot
    | Some (None, op) -> stringf ".%s" op
    | Some (Some lid, op) -> S.dot ^^ longident lid ^^ stringf ".%s" op
    end ^^
    pp_delimited_seq delims nb_semis indices ^^
    begin match assign with
    | None -> empty
    | Some e -> break 1 ^^ S.larrow ^/^ pp e
    end

  and pp_ite ext_attr e1 e2 e3_o =
    let if_ = Ext_attribute.decorate S.if_ ext_attr in
    let if_cond = if_ ^^ nest 2 (break 1 ^^ pp e1) in
    let then_ = S.then_ ^^ nest 2 (group (break 1 ^^ pp e2)) in
    let else_ =
      match e3_o with
      | None -> empty
      | Some e3 -> prefix S.else_ (pp e3)
    in
    group if_cond ^/^ then_ ^?^ else_

  and pp_delimited_seq (opn, cls) nb_semis elts =
    let semi_as_term = List.compare_length_with elts nb_semis = 0 in
    let elts =
      if semi_as_term then
        separate_map (break 1) (fun elt -> pp elt ^^ S.semi) elts
      else
        separate_map (S.semi ^^ break 1) pp elts
    in
    group (prefix_nonempty opn elts ^?^ cls)

  and pp_array nb_semis mut elts =
    pp_delimited_seq (array_delimiters mut) nb_semis elts

  and pp_list nb_semis elts =
    pp_delimited_seq (S.lbracket, S.rbracket) nb_semis elts

  and pp_block_idx block_access unboxed_accesses =
    parens (
      Block_access.pp block_access ^^
      separate_map (break 0) (fun (Uaccess_unboxed_field lid) ->
        S.dothash ^^ longident lid.txt
      ) unboxed_accesses
    )

  and pp_tuple elts =
    separate_map (S.comma ^^ break 1) (Argument.pp pp) elts
    |> group

  and pp_apply e args = Application.pp (pp e) args

  and pp_op_apply op arg =
    match arg.pexp_desc with
    | Pexp_apply (f, args) ->
      (* N.B. the app is not under parentheses, that's why this is valid! *)
      let op = pp_op op in
      (* Basically we align a sublock after the op... except when we finish on a
         literal function in which case we only indent by 2 ??? *)
      let indent = requirement op + 1 (* space *) + 2 (* indent *) in
      let op_and_f = op ^^ nest indent (group (break 1 ^^ pp f)) in
      Application.pp ~indent op_and_f args
    | _ ->
      prefix (pp_op op) (pp arg)

  and pp_variant lbl eo =
    let constr = S.bquote ^^ string lbl in
    let arg = optional pp eo in
    prefix_nonempty constr arg

  and pp_record ?(unboxed = false) nb_semis expr_opt fields =
    let semi_as_term = List.compare_length_with fields nb_semis = 0 in
    let eo =
      match expr_opt with
      | None -> empty
      | Some e -> pp e ^/^ S.with_
    in
    group (
      (* FIXME: that prefix implies a group, meaning in some cases we will end
         up with
         {[
           { foo with bar; baz
           }
         ]}
         thath is not what we want. *)
      prefix ((if unboxed then S.hash_lbrace else S.lbrace) ^?^ eo)
        (Record_field.pp_list ~semi_as_term pp fields) ^/^
      S.rbrace
    )
end

and Record_field : sig
(*   val pp : ('a -> document) -> 'a record_field -> document *)

  val pp_list : semi_as_term:bool -> ('a -> document) -> 'a record_field list ->
    document
end = struct
  let pp add_semi pp_value rf =
    let pre =
      group (
        longident rf.field_name.txt ^?^
        optional (fun v -> group @@ Type_constraint.pp v) rf.typ
      )
    in
    let field =
      match rf.value with
      | None -> pre
      | Some v ->
        prefix (group (pre ^/^ S.equals)) (pp_value v)
    in
    if add_semi then group (field ^^ S.semi) else field

  let pp_list ~semi_as_term pp_value fields =
    (* [separate_map] inlined so we can control ; insertion and grouping *)
    let len = List.length fields in
    let fields =
      List.mapi (fun i field ->
        let add_semi = semi_as_term || i + 1 < len in
        pp add_semi pp_value field
      ) fields
    in
    separate (break 1) fields
end

and Block_access : sig
  val pp : block_access -> document
end = struct
  let pp = function
  | Baccess_field lid -> S.dot ^^ longident lid.txt
  | Baccess_array (mut, idx_kind, e) ->
    let dot_or_dotop =
      match mut with
      | Mutable -> S.dot
      | Immutable -> string ".:"
    in
    let idx_kind =
      match idx_kind with
      | Index_int -> empty
      | Index_unboxed_int8 -> char 's'
      | Index_unboxed_int16 -> char 'S'
      | Index_unboxed_int32 -> char 'l'
      | Index_unboxed_int64 -> char 'L'
      | Index_unboxed_nativeint -> char 'n'
    in
    dot_or_dotop ^^ idx_kind ^^ parens (Expression.pp e)
  | Baccess_block (mut, e) ->
    S.dot ^^
    begin match mut with
    | Immutable -> string "idx_imm"
    | Mutable -> string "idx_mut"
    end ^^ parens (Expression.pp e)
end

and Application : sig
  val pp: ?indent:int -> document -> expression argument list -> document
end = struct
  let is_function p =
    match p.pexp_desc with
    | Pexp_parens
        { begin_end = false
        ; exp = Some { pexp_desc = Pexp_function _; _ }
        } ->
      true
    | _ -> false

  (* We want the following layouts (assuming no line overflows):
     {[
       some_fun arg1 arg2 ~f:(fun a b c d : whatever ->
         foo bar
       ) arg4 arg5
     ]}
     {[
       some_fun arg1 arg2
         ~f:(fun a b c d : whatever -> foo bar) arg4 arg5
     ]}
     {[
       some_fun arg1 arg2
         ~f:(fun a b c d : whatever ->
               foo bar)
         arg4 arg5
     ]}
  *)
  let pp_function_parts ?lbl exp =
    match exp.pexp_desc with
    | Pexp_parens { begin_end = false; exp = Some fun_exp } ->
      let (fun_and_params, body) = Expression.pp_function_parts fun_exp in
      let first_part =
        optional (stringf "~%s:") lbl ^^ S.lparen ^^ fun_and_params
      in
      group (break 1 ^^ first_part) ^^
      group (relative_nest 2 (break 1 ^^ body) ^^ S.rparen)
      |> Attribute.attach ~attrs:exp.pexp_attributes
    | _ -> assert false

  let pp_arg arg =
    match arg.parg_desc with
    | Parg_unlabelled { legacy_modes=[]; arg; typ_constraint=None; modes=[] }
      when is_function arg ->
      pp_function_parts arg
    | Parg_labelled { optional=false; legacy_modes=[]; name;
                      maybe_punned=Some arg; typ_constraint=None;modes=[];
                      default=None }
      when is_function arg ->
      pp_function_parts ~lbl:name arg
    | _ ->
      group (break 1 ^^ Argument.pp Expression.pp arg)

  let pp ?(indent=2) f args =
    List.fold_left (fun acc arg ->
      acc ^^ nest indent @@ pp_arg arg
    ) f args
end

and Case : sig
  val pp : case -> document

  val pp_cases : has_leading_pipe:bool -> case list -> document
end = struct
  let pp_guard = function
    | None -> empty
    | Some e -> group (S.when_ ^/^ Expression.pp e)

  (* we only nest the pattern, not the pipes (there might be several due to
     or-patterns). *)
  let pp_pattern pipe p =
    let rec split_top_or p =
      match p.ppat_desc with
      | Ppat_or (p1, p2) ->
        (* N.B. we're sure there's no attribute here, one needs parens to attach
           an attribute to an or pattern in ocaml. So the attribute would be on
           a Ppat_parens above the Ppat_or. *)
        split_top_or p1 @ split_top_or p2
      | _ -> [ p ]
    in
    split_top_or p
    |> List.fold_left (fun (acc, pipe) pat ->
      let pat = Pattern.pp pat in
      let p = if pipe then group (prefix S.pipe pat) else pat in
      acc ^?^ p, true
    ) (empty, pipe)
    |> fst

  let pp pipe { pc_lhs; pc_guard; pc_rhs } =
    group (
      prefix_nonempty (pp_pattern pipe pc_lhs)
        (pp_guard pc_guard) ^^
      nest 2 (
        (* always try to put on the same line as what preceeds if it fits. *)
        group (break 1 ^^ S.rarrow) ^/^
        Expression.pp pc_rhs
      )
    )

  let pp_cases ~has_leading_pipe =
    foldli (fun i accu x ->
      if i = 0 then
        pp has_leading_pipe x
      else
        accu ^/^ pp true x
    ) empty

  let pp = pp false
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
  let pp { pbop_op; pbop_binding; pbop_loc = _ }=
    Value_binding.pp_list ~start:[string pbop_op.txt] [pbop_binding]
end

and Argument : sig
  val pp : ('a -> document) -> 'a argument -> document
end = struct
  let had_parens arg =
    List.exists (fun elt -> elt.Tokens.desc = Token LPAREN) arg.parg_tokens

  let single_or_multi_token tokens ~optional name =
    match
      List.find_map (function
        | Tokens.{ desc = Token LABEL _; _ } -> Some (stringf "~%s:" name)
        | Tokens.{ desc = Token OPTLABEL _; _ } -> Some (stringf "?%s:" name)
        | _ -> None
      ) tokens
    with
    | Some doc -> doc
    | None -> (if optional then S.qmark else S.tilde) ^^ string name ^^ S.colon

  let pp_generic ?default legacy_modes arg_doc typ_constraint at_modes =
    let typ_and_modes =
      optional Type_constraint.pp typ_constraint
      |> with_modes ~modes:at_modes
    in
    modes_legacy legacy_modes ^?^ arg_doc ^?^ typ_and_modes ^?^
    match default with
    | None -> empty
    | Some d -> S.equals ^/^ Expression.pp d

  let pp pp_arg arg =
    let parenthesize doc =
      if had_parens arg
      then parens (break 0 ^^ doc ^^ break 0)
      else doc
    in
    match arg.parg_desc with
    | Parg_unlabelled { legacy_modes=[]; arg; typ_constraint=None; modes=[] } ->
      pp_arg arg
    | Parg_unlabelled { legacy_modes; arg; typ_constraint; modes } ->
      parens (pp_generic legacy_modes (pp_arg arg) typ_constraint modes)
    | Parg_labelled {
        optional; legacy_modes; name: string; maybe_punned = None;
        typ_constraint; modes; default;
      } ->
      (if optional then S.qmark else S.tilde) ^^
      parenthesize (
        pp_generic legacy_modes (string name) typ_constraint modes ?default
      )
    | Parg_labelled {
        optional; legacy_modes; name: string; maybe_punned = Some a;
        typ_constraint; modes; default;
      } ->
      single_or_multi_token arg.parg_tokens ~optional name ^^
      parenthesize (
        pp_generic legacy_modes (pp_arg a) typ_constraint modes ?default
      )
end

and Function_param : sig
  val pp : function_param -> document
  val pp_desc : function_param_desc -> document
end = struct
  let pp_newtype ?(needs_parens=true) = function
    | name, None -> string name.txt
    | name, Some jkind ->
      let doc = string name.txt ^/^ S.colon ^/^ Jkind_annotation.pp jkind in
      if needs_parens then parens doc else doc

  let pp_desc = function
    | Pparam_val arg -> Argument.pp Pattern.pp arg
    | Pparam_newtype (name, jkind) ->
      parens (S.type_ ^/^ pp_newtype ~needs_parens:false (name, jkind))
    | Pparam_newtypes lst ->
      parens (S.type_ ^/^ separate_map (break 1) pp_newtype lst)

  let pp fp = pp_desc fp.pparam_desc
end

and Function_body : sig
  val pp : function_body -> document
  val as_rhs : function_body -> Generic_binding.rhs
end = struct
  let pp_cases ~tokens cases ext_attrs =
    Ext_attribute.decorate S.function_ ext_attrs,
    Case.pp_cases cases
      ~has_leading_pipe:(has_leading_pipe ~after:FUNCTION tokens)

  let pp fb =
    match fb.pfb_desc with
    | Pfunction_body e -> Expression.pp e
    | Pfunction_cases (cases, ext_attrs) ->
      let kw, cases = pp_cases ~tokens:fb.pfb_tokens cases ext_attrs in
      kw ^/^ cases

  let as_rhs fb =
    match fb.pfb_desc with
    | Pfunction_body _ -> assert false (* can't be on value binding rhs *)
    | Pfunction_cases (cases, ext_attrs) ->
      let kw, cases = pp_cases ~tokens:fb.pfb_tokens cases ext_attrs in
      Generic_binding.Three_parts { start = kw; main = cases; stop = empty}
end

and Type_constraint : sig
  val pp : type_constraint -> document
end = struct
  let pp = function
  | Pconstraint ct ->
    S.colon ^/^ Core_type.pp ct
  | Pcoerce (None, ct) -> S.coerce ^/^ Core_type.pp ct
  | Pcoerce (Some ct1, ct2) ->
    S.colon ^/^ Core_type.pp ct1 ^/^ S.coerce ^/^ Core_type.pp ct2
end


and Function_constraint : sig
  val pp : function_constraint -> document
end = struct
  let pp fc =
    optional Type_constraint.pp fc.ret_type_constraint
    |> with_modes ~modes:fc.ret_mode_annotations
end

and Comprehension : sig
  val pp : comprehension -> document
  val pp_expr : comprehension_expression -> document
end = struct
  let pp_iterator = function
    | Pcomp_range { start; stop; direction = dir } ->
      S.equals ^/^ Expression.pp start ^/^ direction dir ^/^ Expression.pp stop
    | Pcomp_in e -> S.in_ ^/^ Expression.pp e

  let pp_clause_binding
    { pcomp_cb_mode = m_opt
    ; pcomp_cb_pattern = p
    ; pcomp_cb_iterator = it
    ; pcomp_cb_attributes = attrs
    } =
    optional (fun m -> mode m.txt ^^ break 1) m_opt ^^
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
    | Pcomp_list_comprehension c -> S.lbracket ^^ pp c ^^ S.rbracket
    | Pcomp_array_comprehension (mut, c) ->
      let left, right = array_delimiters mut in
      left ^^ pp c ^^ right
end

(** {2 Value descriptions} *)

and Value_description : sig
  val pp : value_description -> document
end = struct
  let pp { pval_pre_doc; pval_ext_attrs; pval_name; pval_type; pval_modalities;
           pval_prim; pval_attributes; pval_post_doc; pval_loc = _ ;
           pval_tokens = _ ; } =
    let kw =
      match pval_prim with
      | [] -> S.val_
      | _ -> S.external_
    in
    let kw = Ext_attribute.decorate kw pval_ext_attrs in
    prefix (group (kw ^/^ str_or_op pval_name.txt)) (
      with_modalities ~modalities:pval_modalities
        (group (S.colon ^/^ Core_type.pp pval_type))
      ^?^
      begin match pval_prim with
        | [] -> empty
        | ps ->
          S.equals ^/^ separate_map (break 1) (fun s -> stringf "%S" s) ps
      end
    )
    |> Attribute.attach ~item:true ?pre_doc:pval_pre_doc ?post_doc:pval_post_doc
      ~attrs:pval_attributes
    |> group
end

(** {2 Type declarations} *)
and Label_declaration : sig
  val pp : label_declaration -> document
end = struct
  let pp { pld_name; pld_mutable; pld_modalities; pld_type; pld_attributes;
           pld_doc = _ (* explicitely handled in the wrapper below *);
           pld_loc = _; pld_tokens = _ } =
    prefix (group (mutable_ pld_mutable ^^ string pld_name.txt ^/^ S.colon))
      (Core_type.pp pld_type ^^
       begin match pld_modalities with
       | [] -> empty
       | ms -> break 1 ^^ S.atat ^/^ modalities ms
       end)
    |> Attribute.attach ~attrs:pld_attributes

  let pp pld =
    (* N.B. we are "normalizing": docstring is always placed after the semicolon
       if there is one.
       Is that ok? *)
    pp pld ^^
    begin if List.exists (fun t -> t.Tokens.desc = Token SEMI) pld.pld_tokens
      then S.semi
      else empty
    end ^?^
    optional Attribute.pp pld.pld_doc

end

and Constructor_argument : sig
  val pp : constructor_argument -> document

  val pp_args : constructor_arguments -> document
end = struct
  let pp { pca_global; pca_modalities; pca_type; pca_loc = _ } =
    (if pca_global then S.global__ else empty) ^?^
    Core_type.pp pca_type
    |> with_modalities ~modalities:pca_modalities

  let pp_args = function
    | Pcstr_tuple args ->
      separate_map (break 1 ^^ S.star ^^ break 1) pp args
    | Pcstr_record lbls ->
      let lbls = separate_map (break 1) Label_declaration.pp lbls in
      prefix S.lbrace lbls ^/^ S.rbrace
end

and Type_param : sig
  val pp : ptype_param -> document
end = struct
  let var_inj_as_single_token =
    (* C.f. rule [type_variance] in the grammar. *)
    List.exists (function
      | Tokens.{ desc = Token (PREFIXOP _ | INFIXOP2 _); _ } -> true
      | _ -> false
    )

  let pp_infos tokens = Asttypes.(function
    | NoVariance, NoInjectivity -> empty
    | Covariant, NoInjectivity -> S.plus
    | Contravariant, NoInjectivity -> S.minus
    | NoVariance, Injective -> S.bang
    | Covariant, Injective ->
      (* N.B. we're normalizing here, in practice the order between variance and
         injectivity is not fixed. *)
      if var_inj_as_single_token tokens
      then string "!+"
      else S.plus ^^ S.bang
    | Contravariant, Injective ->
      if var_inj_as_single_token tokens
      then string "!-"
      else S.minus ^^ S.bang
  )


  let pp { ptp_typ; ptp_infos; ptp_tokens } =
    pp_infos ptp_tokens ptp_infos ^^ Core_type.pp ptp_typ
end

and Type_declaration : sig
  val pp_constraints : ptype_constraint list -> document

  val pp : start:document list -> ?subst:bool -> type_declaration -> document

  val pp_list :
    ?subst:bool -> Asttypes.rec_flag -> type_declaration list -> document
end = struct

  let constructor_declaration pipe
      { pcd_name; pcd_vars; pcd_args; pcd_res; pcd_attributes;
        pcd_doc; pcd_loc = _; pcd_tokens = _ } =
    let pcd_vars =
      match pcd_vars with
      | [] -> empty
      | vars -> Core_type.pp_poly_bindings vars ^^ S.dot
    in
    prefix_nonempty
      (group ((if pipe then S.pipe else empty) ^?^
              constr_ident pcd_name.txt))
      (match pcd_args, pcd_res with
       | Pcstr_tuple [], None -> empty
       | args, None ->
         S.of_ ^/^ Constructor_argument.pp_args args
       | Pcstr_tuple [], Some ct ->
         S.colon ^?^ pcd_vars ^?^ Core_type.pp ct
       | args, Some ct ->
         S.colon ^?^ pcd_vars ^?^
         Constructor_argument.pp_args args ^/^ S.rarrow ^/^ Core_type.pp ct)
    |> Attribute.attach ~attrs:pcd_attributes ?post_doc:pcd_doc

  let pp_constrs ~has_leading_pipe =
    foldli (fun i accu x ->
      if i = 0 then
        constructor_declaration has_leading_pipe x
      else
        accu ^/^ constructor_declaration true x
    ) empty

  let type_kind = function
    | Ptype_abstract -> empty
    | Ptype_variant cds ->
      begin match cds with
      | [] -> S.pipe
      | cd :: _ ->
        pp_constrs ~has_leading_pipe:(starts_with_pipe cd.pcd_tokens) cds
      end
    | Ptype_record lbls ->
      let lbls = separate_map (break 1) Label_declaration.pp lbls in
      prefix S.lbrace lbls ^/^ S.rbrace
    | Ptype_record_unboxed_product lbls ->
      let lbls = separate_map (break 1) Label_declaration.pp lbls in
      prefix S.hash_lbrace lbls ^/^ S.rbrace
    | Ptype_open -> S.dotdot

  let pp_constraints =
    separate_map (break 1) (fun (ct1, ct2, _) ->
      S.constraint_ ^/^ Core_type.pp ct1 ^/^ S.equals ^/^
      Core_type.pp ct2
    )

  let pp_rhs ?(subst=false) td =
    let eq = group (break 1 ^^ if subst then S.colon_equals else S.equals) in
    begin match td.ptype_manifest, td.ptype_kind with
    | None, Ptype_abstract -> empty
    | Some ct, Ptype_abstract ->
      eq ^?^ private_ td.ptype_private ^?^ Core_type.pp ct
    | Some ct, kind ->
      eq ^/^ Core_type.pp ct ^/^
      S.equals ^?^ private_ td.ptype_private ^?^ type_kind kind
    | None, kind ->
      eq ^?^ private_ td.ptype_private ^?^ type_kind kind
    end ^?^ pp_constraints td.ptype_cstrs

  let pp ~start ?subst td =
    let start =
      match start with
      | [] -> assert false
      | first_kw :: other_kws ->
        Ext_attribute.decorate first_kw td.ptype_ext_attrs
          ^?^ separate (break 1) other_kws
    in
    let lhs =
      prefix start (
        let omit_delims_if_possible =
          not (has_leading LPAREN ~after:TYPE td.ptype_tokens)
        in
        type_app ~omit_delims_if_possible (string td.ptype_name.txt)
          (List.map Type_param.pp td.ptype_params)
        ^?^
        match td.ptype_jkind_annotation with
        | None -> empty
        | Some j -> S.colon ^/^ Jkind_annotation.pp j
      )
    in
    lhs ^^ nest 2 (group (pp_rhs ?subst td))
    |> Attribute.attach ~item:true
      ~text:td.ptype_pre_text
      ?pre_doc:td.ptype_pre_doc
      ~attrs:td.ptype_attributes
      ?post_doc:td.ptype_post_doc

  let rec pp_list ?subst start = function
    | [] -> empty
    | td :: tds ->
      group (pp ?subst ~start td)
      ^?^ pp_list ?subst [S.and_] tds

  let pp_list ?subst rf tds =
    pp_list ?subst [S.type_; nonrec_ rf] tds
end

and Type_extension : sig
  val pp : type_extension -> document
end = struct
  let pp { ptyext_path; ptyext_params; ptyext_constructors; ptyext_private;
           ptyext_attributes; ptyext_ext_attrs; ptyext_pre_doc; ptyext_post_doc;
           ptyext_loc = _ ; ptyext_tokens }=
    let omit_delims_if_possible =
      not (has_leading LPAREN ~after:TYPE ptyext_tokens)
    in
    Ext_attribute.decorate S.type_ ptyext_ext_attrs ^/^
    type_app ~omit_delims_if_possible
      (longident ptyext_path.txt) (List.map Type_param.pp ptyext_params) ^/^
    S.plus_equals ^/^
    private_ ptyext_private ^?^
    separate_map (break 1)
      Extension_constructor.pp ptyext_constructors
    |> Attribute.attach ~item:true ~attrs:ptyext_attributes
      ?pre_doc:ptyext_pre_doc
      ?post_doc:ptyext_post_doc
    |> group
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
            | var, None -> S.squote ^^ string var.txt
            | var, Some j ->
              S.lparen ^^ S.squote ^^ string var.txt ^/^ S.colon ^/^
              Jkind_annotation.pp j ^^ S.rparen
          ) lst ^^ S.dot ^^ break 1
      in
      begin match args, res with
        | Pcstr_tuple [], None -> empty
        | args, None ->
            break 1 ^^ S.of_ ^/^ Constructor_argument.pp_args args
          | Pcstr_tuple [], Some ct ->
          break 1 ^^ S.colon ^/^ vars ^^ Core_type.pp ct
        | args, Some ct ->
          break 1 ^^ S.colon ^/^ vars ^^
          Constructor_argument.pp_args args ^/^ S.rarrow ^/^ Core_type.pp ct
      end
    | Pext_rebind lid -> S.equals ^/^ constr_longident lid.txt

  let pp { pext_name; pext_kind; pext_attributes; pext_doc;
           pext_loc = _; pext_tokens } =
    (if starts_with_pipe pext_tokens then S.pipe else empty) ^?^
    constr_ident pext_name.txt ^/^ pp_kind pext_kind
    |> Attribute.attach ~attrs:pext_attributes ?post_doc:pext_doc
end

and Type_exception : sig
  val pp : type_exception -> document
end = struct
  let pp { ptyexn_constructor ; ptyexn_attributes ; ptyexn_ext_attrs;
           ptyexn_pre_doc; ptyexn_post_doc; ptyexn_loc = _; ptyexn_tokens = _} =
    Ext_attribute.decorate S.exception_ ptyexn_ext_attrs ^/^
    Extension_constructor.pp ptyexn_constructor
    |> Attribute.attach ~item:true ~attrs:ptyexn_attributes
      ?pre_doc:ptyexn_pre_doc
      ?post_doc:ptyexn_post_doc
    |> group
end

(** {1 Class language} *)
(** {2 Type expressions for the class language} *)

and Class_type : sig
  val pp : class_type -> document
end = struct
  let rec pp_desc tokens = function
    | Pcty_constr (lid, args) ->
      type_app ~parens:false (longident lid.txt) (List.map Core_type.pp args)
    | Pcty_signature cs -> pp_signature cs
    | Pcty_arrow (lbl, arg, rhs) ->
      Core_type.pp_arrow tokens lbl (Core_type.pp arg) (pp rhs)
    | Pcty_extension ext -> Extension.pp ext
    | Pcty_open (od, ct) ->
      S.let_ ^/^ Open_description.pp od ^/^ S.in_ ^/^ pp ct

  and pp_signature { pcsig_self; pcsig_fields } =
    let obj_with_self =
      S.object_ ^^
      match pcsig_self.ptyp_desc with
      | Ptyp_any None -> empty
      | _otherwise -> parens (Core_type.pp pcsig_self)
    in
    prefix obj_with_self
      (separate_map (break 1) pp_field pcsig_fields) ^/^
    S.end_

  and pp_field { pctf_pre_doc; pctf_desc; pctf_attributes; pctf_post_doc;
                 pctf_loc = _; pctf_tokens = _ } =
    group (pp_field_desc pctf_desc)
    |> Attribute.attach ~attrs:pctf_attributes
      ?pre_doc:pctf_pre_doc ?post_doc:pctf_post_doc

  and pp_field_desc = function
    | Pctf_inherit ct -> S.inherit_ ^/^ pp ct
    | Pctf_val (lbl, mut, virt, ct) ->
      S.val_ ^/^ mutable_ mut ^^ virtual_ virt ^?^
      string lbl.txt ^/^ S.colon ^/^ Core_type.pp ct
    | Pctf_method (lbl, priv, virt, ct) ->
      S.method_ ^/^ private_ priv ^^ virtual_ virt ^?^
      string lbl.txt ^/^ S.colon ^/^ Core_type.pp ct
    | Pctf_constraint (ct1, ct2) ->
      S.constraint_ ^/^ Core_type.pp ct1 ^/^ S.equals ^/^ Core_type.pp ct2
    | Pctf_attribute attr -> Attribute.pp_floating attr
    | Pctf_extension ext -> Extension.pp ~floating:true ext

  and pp { pcty_desc; pcty_attributes; pcty_loc = _; pcty_tokens } =
    pp_desc pcty_tokens pcty_desc
    |> Attribute.attach ~item:true ~attrs:pcty_attributes
end


and Class_infos : sig
  val pp_list :
    'a. ?equal_sign:document -> ('a -> document) -> keywords:document list ->
    'a class_infos list -> document
end = struct
  let pp ?equal_sign pp_expr ~keywords
      { pci_ext_attrs; pci_virt; pci_params; pci_name; pci_expr; pci_attributes;
        pci_value_params; pci_constraint; pci_loc = _; pci_tokens = _;
        pci_pre_text; pci_pre_doc; pci_post_doc } =
    let keywords =
      match keywords with
      | [] -> assert false
      | first :: rest -> Ext_attribute.decorate first pci_ext_attrs :: rest
    in
    let value_params = List.map (Argument.pp Pattern.pp) pci_value_params in
    let bound_class_thingy =
      type_app ~parens:false (string pci_name.txt)
        (List.map Type_param.pp pci_params)
    in
    let open Generic_binding in
    let mk_constraint ct = Single_part (S.colon ^/^ Class_type.pp ct) in
    pp ?equal_sign
      ~pre_text:pci_pre_text
      ?pre_doc:pci_pre_doc
      ~keyword:(group (separate (break 1) keywords ^?^ virtual_ pci_virt))
      bound_class_thingy
      ~params:value_params
      ?constraint_:(Option.map mk_constraint pci_constraint)
      ~rhs:(Single_part (pp_expr pci_expr))
      ~attrs:pci_attributes
      ~item:true (* [@@] expected for attributes, as for struct/sig items *)
      ?post_doc:pci_post_doc

  let rec pp_list ?equal_sign pp_expr ~keywords = function
    | [] -> empty
    | x :: xs ->
      pp ?equal_sign pp_expr ~keywords x ^?^
      pp_list ?equal_sign pp_expr ~keywords:[S.and_] xs
end

and Class_description : sig
  val pp_list : class_description list -> document
end = struct
  let pp_list =
    Class_infos.pp_list ~equal_sign:S.colon Class_type.pp ~keywords:[S.class_]
end

and Class_type_declaration : sig
  val pp_list : class_type_declaration list -> document
end = struct
  let pp_list = Class_infos.pp_list Class_type.pp ~keywords:[S.class_; S.type_]
end

(** {2 Value expressions for the class language} *)

and Class_expr : sig
  val pp : class_expr -> document
  val pp_structure : ext_attribute -> class_structure -> document
end = struct
  let rec pp_desc ext_attr d =
    let (!!) kw = Ext_attribute.decorate kw ext_attr in
    match d with
    | Pcl_constr (lid, args) ->
      type_app ~parens:false (longident lid.txt) (List.map Core_type.pp args)
    | Pcl_structure cs -> pp_structure ext_attr cs
    | Pcl_fun (arg, rhs) ->
      !!S.fun_ ^/^ Argument.pp Pattern.pp arg ^/^ S.rarrow ^/^ pp rhs
    | Pcl_apply (ce, args) -> Application.pp (pp ce) args
    | Pcl_let (rf, vbs, body) ->
      (* FIXME: factorize with Pexp_let *)
      Value_binding.pp_list vbs ~start:(
        S.let_ ::
        match rf with
        | Nonrecursive -> []
        | Recursive -> [S.rec_]
      ) ^/^ S.in_ ^/^ pp body
    | Pcl_constraint (ce, ct) ->
      parens (pp ce ^/^ S.colon ^/^ Class_type.pp ct)
    | Pcl_extension ext -> Extension.pp ext
    | Pcl_open (od, ce) ->
      (* FIXME: factorize *)
      S.let_ ^/^ Open_description.pp od ^/^ S.in_ ^/^ pp ce
    | Pcl_parens ce -> parens (pp ce)

  and pp_structure ext_attr { pcstr_self; pcstr_fields } =
    let obj_with_self =
      Ext_attribute.decorate S.object_ ext_attr ^^
      match pcstr_self.ppat_desc with
      | Ppat_any -> empty
      | _ -> Pattern.pp pcstr_self
    in
    prefix obj_with_self
      (separate_map (hardline ^^ hardline) pp_field pcstr_fields) ^/^
    S.end_

  and pp_field { pcf_pre_doc; pcf_desc; pcf_attributes; pcf_post_doc;
                 pcf_loc = _; pcf_tokens = _ } =
    group (pp_field_desc pcf_desc)
    |> Attribute.attach ~item:true ~attrs:pcf_attributes
      ?pre_doc:pcf_pre_doc ?post_doc:pcf_post_doc

  and pp_field_desc = function
    | Pcf_inherit (override, ce, alias) ->
      S.inherit_ ^^
      begin match override with
        | Fresh -> empty
        | Override -> S.bang
      end ^/^
      pp ce ^^
      begin match alias with
        | None -> empty
        | Some s -> break 1 ^^ S.as_ ^/^ string s.txt
      end
    | Pcf_val (lbl, mut, kind) -> pp_value lbl mut kind
    | Pcf_method (lbl, priv, kind) -> pp_method lbl priv kind
    | Pcf_constraint (ct1, ct2) ->
      S.constraint_ ^/^ Core_type.pp ct1 ^/^ S.equals ^/^ Core_type.pp ct2
    | Pcf_initializer e -> S.initializer_ ^/^ Expression.pp e
    | Pcf_attribute attr -> Attribute.pp_floating attr
    | Pcf_extension ext -> Extension.pp ~floating:true ext

  and pp_value lbl mut = function
    | Cfk_virtual ct ->
      S.val_ ^/^ mutable_ mut ^^ S.virtual_ ^/^ string lbl.txt ^/^
      S.colon ^/^ Core_type.pp ct
    | Cfk_concrete (over, vb) ->
      let start = [
        S.val_ ^^ override_ over;
        mutable_ mut
      ] in
      Value_binding.pp_list ~start [vb]

  and pp_method lbl priv = function
    | Cfk_virtual ct ->
      S.method_ ^/^ private_ priv ^^ S.virtual_ ^/^ string lbl.txt ^/^
      S.colon ^/^ Core_type.pp ct
    | Cfk_concrete (over, vb) ->
      let start = [
        S.method_ ^^ override_ over;
        private_ priv
      ] in
      Value_binding.pp_list ~start [vb]

  and pp { pcl_ext_attrs; pcl_desc; pcl_attributes; pcl_loc = _ } =
    pp_desc pcl_ext_attrs pcl_desc
    |> Attribute.attach ~item:true ~attrs:pcl_attributes
end


and Class_declaration : sig
  val pp_list : class_declaration list -> document
end = struct
  let pp_list = Class_infos.pp_list Class_expr.pp ~keywords:[S.class_]
end

(** {1 Module language} *)
(** {2 Type expressions for the module language} *)

and Module_type : sig
  val pp : module_type -> document

  val as_rhs : module_type -> Generic_binding.rhs
end = struct
  let rec pp_desc = function
    | Pmty_ident lid -> longident lid.txt
    | Pmty_signature sg -> Signature.pp sg
    | Pmty_functor (attrs, fps, mty, modes) ->
      Attribute.attach ~attrs S.functor_ ^/^
      separate_map (break 1) Functor_parameter.pp fps ^/^ S.rarrow ^/^
      with_modes ~modes (pp mty)
    | Pmty_functor_type (fps, mty, modes) ->
      separate_map (break 1) Functor_parameter.pp_type fps ^/^ S.rarrow ^/^
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
    | Pmty_parens mty -> parens (pp mty)

  and pp { pmty_desc; pmty_attributes; pmty_loc = _; pmty_tokens = _ } =
    pp_desc pmty_desc
    |> Attribute.attach ~attrs:pmty_attributes

  let as_rhs ({ pmty_desc; pmty_attributes; pmty_loc=_; pmty_tokens=_ } as mty)
    : Generic_binding.rhs =
    match pmty_desc with
    | Pmty_signature sg ->
      let start, main, stop = Signature.pp_parts sg in
      let stop = Attribute.attach stop ~attrs:pmty_attributes in
      Three_parts { start; main; stop }
    | _ -> Single_part (pp mty)
end

and Functor_parameter : sig
  val pp : functor_parameter -> document
  val pp_type : functor_parameter -> document
end = struct
  let pp_type = function
    | Unit -> parens empty
    | Named (lbl, mty, modes) ->
      let mty_modes = with_modes (Module_type.pp mty) ~modes in
      match lbl.txt with
      | None -> mty_modes
      | Some s -> parens (string s ^/^ S.colon ^/^ mty_modes)

  let pp = function
    | Unit -> empty
    | Named (lbl, mty, modes) ->
      begin match lbl.txt with
        | None -> S.underscore
        | Some s -> string s
      end ^/^ S.colon ^/^ with_modes (Module_type.pp mty) ~modes

  let pp fp = parens (pp fp)
end

and Signature : sig
  val pp : signature -> document
  val pp_parts : signature -> document * document * document

  val pp_interface : signature -> document
end = struct
  let pp_item_desc = function
    | Psig_value vd -> Value_description.pp vd
    | Psig_type (rf, tds) -> Type_declaration.pp_list rf tds
    | Psig_typesubst tds -> Type_declaration.pp_list ~subst:true Recursive tds
    | Psig_typext te -> Type_extension.pp te
    | Psig_exception exn -> Type_exception.pp exn
    | Psig_module md -> Module_declaration.pp md
    | Psig_modsubst ms -> Module_substitution.pp ms
    | Psig_recmodule mds -> Module_declaration.pp_recmods mds
    | Psig_modtype mty -> Module_type_declaration.pp mty
    | Psig_modtypesubst mty -> Module_type_declaration.pp ~subst:true mty
    | Psig_open od -> Open_description.pp od
    | Psig_include (incl, modalities) ->
      with_modalities ~modalities (Include_description.pp incl)
      |> group
    | Psig_class cds -> Class_description.pp_list cds
    | Psig_class_type ctds -> Class_type_declaration.pp_list ctds
    | Psig_attribute attr -> Attribute.pp_floating attr
    | Psig_extension (ext, attrs) ->
      Attribute.attach ~item:true ~attrs (Extension.pp ~floating:true ext)
    | Psig_kind_abbrev (name, k) ->
      S.kind_abbrev__ ^/^ string name.txt ^/^ S.equals ^/^
        Jkind_annotation.pp k
      |> group

  let pp_item it = pp_item_desc it.psig_desc

  (* TODO: duplicated from Structure *)
  let (^//^) before after =
    if before = Empty then
      after
    else
      before ^^ hardline ^^ hardline ^^ after

  (* We keep the list of items in sync with the list of "tokens" of the
     signature (each [Child_node] is a signature item).
     That tells us where to insert [;;].

     N.B. as modalities are not marked as "child", we need to remove the prefix
     of the tokens as they correspond to the modalities tokens
  *)
  let pp_keeping_semi doc
        { psg_modalities = _; psg_items; psg_tokens; psg_loc = _ } =
    let rec aux doc = function
      | [], [] -> doc
      | item :: items, Tokens.{ desc = Child_node; _ } :: tokens ->
        aux (doc ^//^ pp_item item) (items, tokens)
      | _::_, [] -> assert false
      | items, tok :: tokens ->
        match tok.desc with
        | Child_node -> assert false
        | Token SEMISEMI ->
          aux (doc ^?^ S.semisemi) (items, tokens)
        | Comment _
        | Token EOF ->
          aux doc (items, tokens)
        | Token _ -> assert false
    in
    let items_tokens =
      List.drop_while (fun tok ->
        match tok.Tokens.desc with
        | Token (ATAT | LIDENT _) -> (* modality tokens *) true
        | Comment _ ->
          (* comments might appear before / inside the modalities *)
          true
        | _ -> false
      ) psg_tokens
    in
    aux doc (psg_items, items_tokens)


  let pp_parts sg =
    with_modalities S.sig_ ~modalities:sg.psg_modalities,
    pp_keeping_semi empty sg,
    S.end_

  let pp sg =
    let sig_with_mods, items, end_ = pp_parts sg in
    prefix sig_with_mods items ^/^ end_


  let pp_interface sg =
    with_modalities empty ~modalities:sg.psg_modalities ^?^
    group (pp_keeping_semi empty sg)
end

and Module_declaration : sig
  val pp : module_declaration -> document

  val pp_recmods : module_declaration list -> document
end = struct
  let equal_sign mty =
    match mty.pmty_desc with
    | Pmty_alias _ -> S.equals
    | _ -> S.colon

  let pp keywords { pmd_name = (name, modalities); pmd_body; pmd_attributes;
                    pmd_ext_attrs; pmd_pre_text; pmd_pre_doc; pmd_post_doc;
                    pmd_loc = _; pmd_tokens = _ } =
    let keywords =
      Ext_attribute.decorate (List.hd keywords) pmd_ext_attrs
        :: List.tl keywords
    in
    let binding =
      let name =
        match name.txt with
        | None -> S.underscore
        | Some s -> string s
      in
      match modalities with
      | [] -> name
      | _ -> parens (with_modalities name ~modalities)
    in
    let params, equal_sign, rhs =
      match pmd_body with
      | With_params (params, mty, modes) ->
        let rhs =
          Module_type.as_rhs mty
          |> Generic_binding.map_rhs_end (with_modes ~modes)
        in
        List.map Functor_parameter.pp params,
        S.colon,
        rhs
      | Without_params (mty, modalities) ->
        let rhs =
          Module_type.as_rhs mty
          |> Generic_binding.map_rhs_end (with_modalities ~modalities)
        in
        [], equal_sign mty, rhs
    in
    Generic_binding.pp ~item:true
      ~pre_text:pmd_pre_text ?pre_doc:pmd_pre_doc
      ~keyword:(separate (break 1) keywords)
      binding
      ~params
      ~equal_sign
      ~rhs
      ~attrs:pmd_attributes
      ?post_doc:pmd_post_doc

  let pp_recmods = function
    | [] -> empty (* assert false? *)
    | md :: mds ->
      pp [ S.module_; S.rec_ ] md ^?^
      separate_map (break 1) (pp [ S.and_ ]) mds

  let pp pmd = pp [ S.module_ ] pmd

end

and Module_substitution : sig
  val pp : module_substitution -> document
end = struct
  let pp { pms_name; pms_manifest; pms_attributes; pms_ext_attrs;
           pms_pre_doc; pms_post_doc; pms_loc = _; pms_tokens = _ } =
    Generic_binding.pp ~item:true ~equal_sign:S.colon_equals
      ?pre_doc:pms_pre_doc
      ~keyword:(Ext_attribute.decorate S.module_ pms_ext_attrs)
      (string pms_name.txt)
      ~rhs:(Generic_binding.Single_part (longident pms_manifest.txt))
      ~attrs:pms_attributes
      ?post_doc:pms_post_doc
end

and Module_type_declaration : sig
  val pp : ?subst:bool -> module_type_declaration -> document
end = struct
  let pp ?(subst=false)
      { pmtd_name; pmtd_type; pmtd_attributes; pmtd_ext_attrs; pmtd_pre_doc;
        pmtd_post_doc; pmtd_loc = _; pmtd_tokens = _ } =
    Generic_binding.pp ~item:true
      ?pre_doc:pmtd_pre_doc
      ~keyword:(Ext_attribute.decorate S.module_ pmtd_ext_attrs ^/^ S.type_)
      (string pmtd_name.txt)
      ~equal_sign:(if subst then S.colon_equals else S.equals)
      ?rhs:(Option.map Module_type.as_rhs pmtd_type)
      ~attrs:pmtd_attributes
      ?post_doc:pmtd_post_doc
end

and Open_infos : sig
  val pp : 'a. ?item:bool -> ('a -> document) -> 'a open_infos -> document
end = struct
  let pp ?(item=true) pp_expr
      { popen_expr; popen_override; popen_attributes; popen_ext_attrs;
        popen_pre_doc; popen_post_doc; popen_loc = _; popen_tokens = _ } =
    Ext_attribute.decorate (S.open_ ^^ override_ popen_override)
      popen_ext_attrs ^/^
    pp_expr popen_expr
    |> Attribute.attach ~item ~attrs:popen_attributes
      ?pre_doc:popen_pre_doc ?post_doc:popen_post_doc
    |> group
end

and Open_description : sig
  val pp : open_description -> document
end = struct
  let pp = Open_infos.pp (fun lid -> longident lid.txt)
end

and Open_declaration : sig
  val pp : ?item:bool -> open_declaration -> document
end = struct
  let pp ?item = Open_infos.pp ?item Module_expr.pp
end

and Include_infos : sig
  val pp : 'a. ('a -> document) -> 'a include_infos -> document
end = struct
  let pp pp_mod { pincl_kind ; pincl_mod; pincl_attributes; pincl_ext_attrs;
                  pincl_pre_doc; pincl_post_doc; pincl_loc = _;
                  pincl_tokens = _ } =
    Ext_attribute.decorate S.include_ pincl_ext_attrs ^/^
    begin match pincl_kind with
      | Functor -> S.functor_ ^^ break 1
      | Structure -> empty
    end ^^
    pp_mod pincl_mod
    |> Attribute.attach ~item:true ~attrs:pincl_attributes
      ?pre_doc:pincl_pre_doc ?post_doc:pincl_post_doc
    |> group
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
    | Pwith_type (params, lid, priv, ct, cstrs) ->
      S.type_ ^/^
      type_app (longident lid.txt)
        (List.map Type_param.pp params) ^/^
      S.equals ^?^ private_ priv ^?^
      Core_type.pp ct ^?^ Type_declaration.pp_constraints cstrs
    | Pwith_module (lid1, lid2) ->
      S.module_ ^/^ longident lid1.txt ^/^ S.equals ^/^ longident lid2.txt
    | Pwith_modtype (lid, mty) ->
      S.module_ ^/^ S.type_ ^/^ longident lid.txt ^/^ S.equals ^/^
      Module_type.pp mty
    | Pwith_modtypesubst (lid, mty) ->
      S.module_ ^/^ S.type_ ^/^ longident lid.txt ^/^ S.colon_equals ^/^
      Module_type.pp mty
    | Pwith_typesubst (params, lid, ct) ->
      S.type_ ^/^
      type_app (longident lid.txt)
        (List.map Type_param.pp params) ^/^
      S.colon_equals ^/^ Core_type.pp ct
    | Pwith_modsubst (lid1, lid2) ->
      S.module_ ^/^ longident lid1.txt ^/^ S.colon_equals ^/^
      longident lid2.txt
end

(** {2 Value expressions for the module language} *)

and Module_expr : sig
  val pp : module_expr -> document
  val as_rhs : module_expr -> Generic_binding.rhs

  (* TODO: not the most natural place for this. *)
  val pp_package_type : core_type -> document
end = struct
  let pp_package_type ct =
    match ct.ptyp_desc with
    | Ptyp_package { ppt_ext_attr = None; ppt_name; ppt_eqs } ->
      let with_ =
        match ppt_eqs with
        | [] -> empty
        | _ ->
          let one (lid, ct) =
            longident lid.txt ^/^ S.equals ^/^ Core_type.pp ct
          in
          S.with_ ^/^ S.type_ ^/^
          separate_map (break 1 ^^ S.and_ ^^ break 1) one ppt_eqs
      in
      longident ppt_name.txt ^?^ with_
    | _ -> assert false

  let rec pp_desc = function
    | Pmod_ident lid -> longident lid.txt
    | Pmod_structure (attrs, str) -> Structure.pp ~attrs str
    | Pmod_functor (attrs, fps, me) ->
      Attribute.attach ~attrs S.functor_ ^/^
      separate_map (break 1) Functor_parameter.pp fps ^/^ S.rarrow ^/^ pp me
    | Pmod_apply (m1, m2) -> pp m1 ^/^ pp m2
    | Pmod_apply_unit me -> pp me ^^ S.lparen ^^ S.rparen
    | Pmod_constraint (me, None, modes) ->
      (* FIXME: parens? shouldn't that be part of cst? *)
      parens (with_modes ~modes (pp me))
    | Pmod_constraint (me, Some mty, atat_modes) ->
      parens (
        pp me ^/^ S.colon ^/^ Module_type.pp mty ^^
        begin match atat_modes with
          | [] -> empty
          | l -> break 1 ^^ S.atat ^/^ modes l
        end
      )
    | Pmod_unpack (e, ty1, ty2) ->
      parens (
        S.val_ ^/^ Expression.pp e ^?^
        optional (fun c -> S.colon ^/^ pp_package_type c) ty1 ^?^
        optional (fun c -> S.coerce ^/^ pp_package_type c) ty2
      )
    | Pmod_extension ext -> Extension.pp ext
    | Pmod_parens me -> parens (pp me)

  and pp { pmod_desc; pmod_attributes; pmod_loc = _; pmod_tokens = _ } =
    pp_desc pmod_desc
    |> Attribute.attach ~attrs:pmod_attributes

  let as_rhs
        ({ pmod_desc; pmod_attributes; pmod_loc = _; pmod_tokens = _ } as me)
    : Generic_binding.rhs =
    match pmod_desc with
    | Pmod_structure (attrs, str) ->
      let start, main, stop = Structure.pp_parts attrs str in
      let stop = Attribute.attach stop ~attrs:pmod_attributes in
      Three_parts { start; main; stop }
    | _ -> Single_part (pp me)
end

and Structure : sig
  val pp : ?attrs:attributes -> structure -> document
  val pp_parts : attributes -> structure -> document * document * document

  val pp_implementation : structure -> document
end = struct
  let pp_item_desc item =
    match item.pstr_desc with
    | Pstr_eval (e, attrs) ->
      Attribute.attach ~item:true ~attrs (Expression.pp e)
    | Pstr_value (rf, vbs) ->
      (* FIXME: factorize Pexp_let *)
      Value_binding.pp_list ~item:true vbs ~start:(
        S.let_ ::
        match rf with
        | Nonrecursive -> []
        | Recursive -> [S.rec_]
      )
    | Pstr_primitive vd -> Value_description.pp vd
    (* FIXME: factorize with Psig_* *)
    | Pstr_type (rf, tds) -> Type_declaration.pp_list rf tds
    | Pstr_typext te -> Type_extension.pp te
    | Pstr_exception exn -> Type_exception.pp exn
    | Pstr_module mb -> Module_binding.pp ~keywords:[S.module_] mb
    | Pstr_recmodule mbs -> Module_binding.pp_recmods mbs
    | Pstr_modtype mty -> Module_type_declaration.pp mty
    | Pstr_open od -> Open_declaration.pp od
    | Pstr_class cds -> Class_declaration.pp_list cds
    | Pstr_class_type ctds -> Class_type_declaration.pp_list ctds
    | Pstr_include incl -> Include_declaration.pp incl
    | Pstr_attribute a -> Attribute.pp_floating a
    | Pstr_extension (ext, attrs) ->
      Attribute.attach ~item:true ~attrs (Extension.pp ~floating:true ext)
      |> group
    | Pstr_kind_abbrev (name, k) ->
      S.kind_abbrev__ ^/^ string name.txt ^/^ S.equals ^/^
      Jkind_annotation.pp k
      |> group

  let pp_item it = pp_item_desc it

  let (^//^) before after =
    if before = Empty then
      after
    else
      before ^^ hardline ^^ hardline ^^ after

  (* We keep the list of items in sync with the list of "tokens" of the
     structure (each [Child_node] is a structure item).
     That tells us where to insert [;;]. *)
  let rec pp_keeping_semi doc = function
    | [], [] -> doc
    | item :: items, Tokens.{ desc = Child_node; _ } :: tokens ->
      pp_keeping_semi (doc ^//^ pp_item item) (items, tokens)
    | _::_, [] -> assert false
    | items, tok :: tokens ->
      match tok.desc with
      | Child_node -> assert false
      | Token SEMISEMI ->
        pp_keeping_semi (doc ^?^ S.semisemi) (items, tokens)
      | Comment _
      | Token EOF ->
        pp_keeping_semi doc (items, tokens)
      | Token _ -> assert false

  let pp ?(attrs=[]) str =
    let struct_ = Attribute.attach ~attrs S.struct_ in
    group (prefix struct_ (pp_keeping_semi empty str) ^/^ S.end_)

  let pp_parts attrs str =
    Attribute.attach ~attrs S.struct_,
    group (pp_keeping_semi empty str), S.end_

  let pp_implementation str =
    group (pp_keeping_semi empty str)
end

and Value_constraint : sig
  val pp : value_constraint -> document
end = struct
  let pp = function
    | Pvc_constraint { locally_abstract_univars; typ } ->
      S.colon ^/^
      begin match locally_abstract_univars with
        | [] -> empty
        | vars ->
          let pp_var (name, jkind) =
            match jkind with
            | None -> string name.txt
            | Some j ->
              parens (string name.txt ^/^ S.colon ^/^ Jkind_annotation.pp j)
          in
          S.type_ ^/^
          separate_map (break 1) pp_var vars ^^ S.dot ^^ break 1
      end ^^
      Core_type.pp typ
    | Pvc_coercion {ground; coercion} ->
      begin match ground with
        | None -> empty
        | Some ct -> S.colon ^/^ Core_type.pp ct ^^ break 1
      end ^^
      S.coerce ^/^ Core_type.pp coercion

  let pp x = group (pp x)
end

and Generic_binding : sig
  type rhs =
    | Single_part of document
    | Three_parts of { start: document; main: document; stop: document }
    (** Meant for [struct/sig .. end]: we try to keep [start] on the same line
        as what preceeds it, [main] is indented, [stop] is not.

        N.B. also used for value bindings in cases such as
        [let f = function ...], there [start = function] and [stop] is empty. *)

  val map_rhs_end : (document -> document) -> rhs -> rhs
    (** either map the sole part, or the stop part *)

  val pp
    : ?item:bool
    -> ?equal_sign:document
    -> ?pre_text:attributes
    -> ?pre_doc:attribute
    -> keyword:document
    -> ?params:document list
    -> ?constraint_:rhs
    -> ?rhs:rhs
    -> ?attrs:attributes
    -> ?post_doc:attribute
    -> document
    -> document
end = struct
  type rhs =
    | Single_part of document
    | Three_parts of { start: document; main: document; stop: document }

  let map_rhs_end f = function
    | Single_part d -> Single_part (f d)
    | Three_parts r -> Three_parts { r with stop = f r.stop }

  let pp ~equal_sign ~keyword ~params ?constraint_ ?rhs bound =
    let bindings = nest 2 (flow (break 1) (keyword :: bound :: params)) in
    match constraint_, rhs with
    | None, None ->
      (* let-punning and abstract module types *)
      bindings
    | None, Some Single_part doc ->
      prefix (group (bindings ^/^ equal_sign)) doc
    | Some Single_part doc, None ->
      (* FIXME: weird asymmtry the "colon" is already part of [constraint_] but
         [equal_sign] is not part of [rhs]... *)
      prefix bindings doc
    | Some Single_part typ, Some Single_part exp ->
      prefix
        (prefix bindings (group (typ ^/^ equal_sign)))
        exp
    | None, Some Three_parts { start; main; stop } ->
      prefix bindings (group (equal_sign ^/^ start)) ^^
        nest 2 (break 1 ^^ main) ^/^
      stop
    | Some Three_parts { start; main; stop }, None ->
      (* FIXME: here the "colon" is not included??? *)
      prefix
        (prefix bindings (group (S.colon ^/^ start)))
        main ^/^
      stop
    | Some Three_parts { start; main; stop }, Some Single_part doc ->
      (* FIXME: here the "colon" is not included??? *)
      prefix
        (prefix bindings (group (S.colon ^/^ start)))
        main ^/^
      prefix
        (group (stop ^/^ equal_sign))
        doc
    | Some Single_part doc, Some Three_parts { start; main; stop } ->
      prefix
        (prefix bindings (group (doc ^/^ equal_sign ^/^ start)))
        main ^/^
      stop
    | Some Three_parts typ, Some Three_parts exp ->
      prefix
        (prefix bindings (group (S.colon ^/^ typ.start)))
        typ.main ^/^
      prefix
        (group (typ.stop ^/^ equal_sign ^/^ exp.start))
        exp.main ^/^
      exp.stop


  let pp ?item ?(equal_sign = S.equals) ?pre_text ?pre_doc ~keyword ?(params=[])
        ?constraint_ ?rhs ?(attrs=[]) ?post_doc bound =
    pp ~equal_sign ~keyword ~params ?constraint_ ?rhs bound
    |> Attribute.attach ?item ?text:pre_text ?pre_doc ?post_doc ~attrs
end

and Value_binding : sig
  val pp_list : ?item:bool -> start:document list -> value_binding list -> document
end = struct
  let rhs e =
    (* FIXME: attributes ! *)
    match e.pexp_desc with
    | Pexp_function ([], _, body) ->
      Function_body.as_rhs body
    | _ -> Single_part (Expression.pp e)

  let pp ?item start
        { pvb_modes; pvb_pat; pvb_params; pvb_constraint; pvb_ret_modes;
          pvb_expr; pvb_attributes; pvb_pre_text; pvb_pre_doc; pvb_post_doc;
          pvb_loc = _; pvb_tokens = _; pvb_ext_attrs } =
    let start =
      match start with
      | [] -> assert false
      | first_kw :: other_kws ->
        Ext_attribute.decorate first_kw pvb_ext_attrs
          ^?^ separate (break 1) other_kws
    in
    let kw_and_modes = group (start ^?^ modes pvb_modes) in
    let pat = Pattern.pp pvb_pat in
    let params = List.map Function_param.pp pvb_params in
    let open Generic_binding in
    let constraint_ =
      match pvb_constraint, pvb_ret_modes with
      | None, [] -> None
      | None, lst -> Some (Single_part (S.at ^/^ modes lst))
      | Some vc, lst ->
        Some (Single_part (with_modes (Value_constraint.pp vc) ~modes:lst))
    in
    pp ~pre_text:pvb_pre_text ?pre_doc:pvb_pre_doc
      ~keyword:kw_and_modes
      pat ~params ?constraint_
      ?rhs:(Option.map rhs pvb_expr)
      ?item ~attrs:pvb_attributes
      ?post_doc:pvb_post_doc

  let rec pp_list ?(item=false) ~start = function
    | [] -> empty
    | [ x ] -> pp ~item start x
    | x :: xs ->
      let sep = if item then hardline ^^ hardline else break 1 in
      group (pp ~item start x) ^^ sep ^^ pp_list ~item ~start:[S.and_] xs
end

and Module_binding : sig
  val pp : ?item:bool -> keywords:document list -> module_binding -> document

  val pp_recmods : module_binding list -> document
end = struct
  let modal_constraint constr mode_l : Generic_binding.rhs option =
    match Option.map Module_type.as_rhs constr, mode_l with
    | None, [] -> None
    | Some Three_parts { start; main; stop }, modes ->
      Some (Three_parts { start; main; stop = with_atat_modes stop ~modes })
    | Some Single_part mty, modes ->
      Some (Single_part (
        S.colon ^/^ mty
        |> with_atat_modes ~modes
      ))
    | None, at_modes -> Some (Single_part (S.at ^/^ modes at_modes))

  let pp ?item ~keywords
      { pmb_ext_attrs; pmb_name = (name, name_modes); pmb_params;
        pmb_constraint; pmb_modes; pmb_expr; pmb_attributes; pmb_pre_text;
        pmb_pre_doc; pmb_post_doc; pmb_loc = _; pmb_tokens = _ } =
    let kw =
      Ext_attribute.decorate (List.hd keywords) pmb_ext_attrs ^?^
        separate (break 1) (List.tl keywords)
    in
    let bound =
      let name =
        match name.txt with
        | None -> S.underscore
        | Some s -> string s
      in
      match name_modes with
      | [] -> name
      | modes -> parens (with_modes name ~modes)
    in
    let params = List.map Functor_parameter.pp pmb_params in
    let constraint_ = modal_constraint pmb_constraint pmb_modes in
    Generic_binding.pp ~pre_text:pmb_pre_text ?pre_doc:pmb_pre_doc
      ~keyword:(group kw) bound ~params
      ?constraint_
      ~rhs:(Module_expr.as_rhs pmb_expr)
      ~attrs:pmb_attributes ?item
      ?post_doc:pmb_post_doc

  let rec pp_recmods keywords = function
    | [] -> empty
    | [ mb ] -> pp ~item:true ~keywords mb
    | mb :: mbs ->
      pp ~item:true ~keywords mb ^^
      hardline ^^ hardline ^^
      pp_recmods [S.and_] mbs

  let pp_recmods = pp_recmods [S.module_; S.rec_]
end

and Jkind_annotation : sig
  val pp : jkind_annotation -> document
end = struct
  let rec jkind_annotation_desc = function
    | Default -> S.underscore
    | Abbreviation s -> string s
    | Mod (jk, ms) -> Jkind_annotation.pp jk ^/^ S.mod_ ^/^ modes ms
    | With (jk, ct, modalities) ->
      Jkind_annotation.pp jk ^/^ S.with_ ^/^ Core_type.pp ct
      |> with_modalities ~modalities
    | Kind_of ct -> S.kind_of__ ^/^ Core_type.pp ct
    | Product jks ->
      separate_map (break 1 ^^ S.ampersand ^^ break 1) Jkind_annotation.pp jks
    | Parens jkd -> parens (jkind_annotation_desc jkd)

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
