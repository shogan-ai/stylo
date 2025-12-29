open Document
open Document.Utils
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

let rec lparen_before_child = function
  | []
  | Tokens.{ desc = Child_node; _ } :: _ -> false
  | Tokens.{ desc = Token LPAREN; _ } :: _ -> true
  | _ :: rest -> lparen_before_child rest

type 'a loc = 'a Location.loc = { txt: 'a; loc: Location.t }
let stringf fmt = Printf.ksprintf string fmt
let fancy_string fmt = Printf.ksprintf fancy_string fmt

let str_or_op (so : Longident.str_or_op) =
  group @@
  match so with
  | Str s -> string s
  | Str_trailing_hash s -> string s ^^ S.hash
  | Op s -> parens (break 1 ^^ string s ^^ break 1)
  | DotOp (op, paren_kind, index_mod, assign) ->
    let index_mod = if index_mod = "" then empty else S.semi ^^ S.dotdot in
    let left, right =
      match paren_kind with
      | Brace -> S.lbrace, S.rbrace
      | Bracket -> S.lbracket, S.rbracket
      | Paren -> S.lparen, S.rparen
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
  | Asttypes.Mutable -> S.mutable_
  | Immutable -> empty

let virtual_ = function
  | Asttypes.Virtual -> S.virtual_
  | Concrete -> empty

let override_= function
  | Asttypes.Fresh -> empty
  | Override -> S.bang

let array_delimiters = function
  | Asttypes.Immutable -> S.lbracket_colon, S.colon_rbracket
  | Mutable -> S.lbracket_pipe, S.pipe_rbracket

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
  | Pconst_char (_, src) -> stringf "%s" src
  | Pconst_untagged_char (_, src) -> stringf "%s" src
  | Pconst_string (s, _, None) -> String_lit.pp s
  | Pconst_string (s, _, Some delim) -> fancy_string "{%s|%s|%s}" delim s delim

let separate_loc_list sep f = separate_map sep (fun l -> f l.txt)

let modality (Modality s) = string s
let modalities = separate_loc_list (break 1) modality

let with_modalities ~modalities:l t =
  match l with
  | [] -> t
  | _ -> t ^?^ group (S.atat ^/^ modalities l)

let mode (Mode s) = string s
let modes = separate_loc_list (break 1) mode

let mode_legacy = function
  | Mode ("local" | "once" | "unique" as s) -> stringf "%s_" s
  | _ -> assert false
let modes_legacy = separate_loc_list (break 1) mode_legacy

let with_modes ?(extra_nest=Fun.id) ~modes:l t =
  match l with
  | [] -> t
  | _ ->
    let modes = extra_nest @@ group (S.at ^/^ modes l) in
    match t with
    | Empty -> modes
    | _ -> t ^^ group (break 1 ^^ modes)

let with_atat_modes ?(extra_nest=Fun.id) ~modes:l t =
  match l with
  | [] -> t
  | _ -> t ^?^ extra_nest @@ group (S.atat ^/^ modes l)

let include_kind = function
  | Structure -> empty
  | Functor -> S.functor_

module Doc = struct
  let pp = docstring
  let pp_floating s =
    softline ^^ pp s ^^ softline
end

module rec Attribute : sig
  val pp : ?item:bool -> attribute -> t
  val pp_floating : attribute -> t
  val pp_list : ?item:bool -> attributes -> t

  val pp_attr_name : string list loc -> t

  val attach
    :  ?item:bool
    -> ?flatness:flatness
    -> ?text:string list
    -> ?pre_doc:string
    -> ?post_doc:string
    -> attrs:attributes
    -> t
    -> t
end = struct
  let pp_attr_name ids =
    separate_map S.dot string ids.txt

  let pp lbat name payload =
    group (
      lbat ^^ pp_attr_name name ^^ nest 2 @@ Payload.pp payload ^^ S.rbracket
    )

  let pp_floating { attr_name; attr_payload; attr_loc = _; attr_tokens = _ } =
    pp S.lbracket_atatat attr_name attr_payload

  let pp ?(item=false)
      { attr_name; attr_payload; attr_loc = _; attr_tokens = _ } =
    pp (if item then S.lbracket_atat else S.lbracket_at) attr_name
      attr_payload

  let pp_list ?item l = group @@ separate_map (break 1) (pp ?item) l

  let attach ?item ?flatness ?(text = []) ?pre_doc ?post_doc ~attrs t =
    let with_attrs =
      match attrs, flatness with
      | [], None -> t
      | _ ->
        (* we care about the flatness of the group, so we introduce it even in
           the absence of attributes. *)
        group ?flatness (t ^?^ pp_list ?item attrs)
    in
    begin match text with
    | [] -> empty
    | text ->
      softline ^^ softline ^^
      separate_map (break 1) Doc.pp text ^^
      softline ^^ softline
    end ^^
    optional Doc.pp pre_doc ^?/^
    match post_doc with
    | None -> with_attrs
    | Some s ->
      group (with_attrs ^^ softest_break ^^ Doc.pp s) ^^ softline
end

and Ext_attribute : sig
  val decorate : t -> ext_attribute -> t
  val decorate_value_binding : t -> ext_attribute -> t
  val decorate_optional_override :
    t -> Asttypes.override_flag -> ext_attribute -> t
end = struct
  let decorate ?(space=true) ~between kw { pea_ext; pea_attrs } =
    let kw_with_ext =
      match pea_ext with
      | None -> kw
      | Some lst_loc ->
        kw ^^ between ^^ string "%" ^^
        separate_map (group (S.dot ^^ break 0)) string lst_loc.txt
    in
    let (++) = if space then (^?^) else (^^) in
    let indent =
      (* align attrs *)
      if space then 2 else Requirement.to_int (requirement kw_with_ext)
    in
    kw_with_ext ++ nest indent (Attribute.pp_list pea_attrs)

  let decorate_optional_override kw ovr =
    let kw, between =
      match ovr with
      | Asttypes.Fresh -> kw, empty
      | Override -> kw ^^ S.bang, break 1
    in
    decorate kw ~between

  let decorate_value_binding = decorate ~between:empty ~space:false
  let decorate kw ea = decorate ~between:empty kw ea
end

and Extension : sig
  val pp : ?preceeding:Preceeding.t -> ?floating:bool -> extension -> t

  val pp_toplevel : toplevel_extension -> t
end = struct
  let payload_indent = function
    | PTyp _ | PPat _ | PString _ | PStr ([_], _) | PSig {psg_items = [_]; _} ->
      2
    | PStr _ | PSig _ -> 0

  let pp_classic ?preceeding ~floating names payload =
    let name = Attribute.pp_attr_name names in
    let opn, indent =
      if floating
      then S.lbracket_percentpercent, payload_indent payload
      else S.lbracket_percent, 2
    in
    let opn, pre_nest = Preceeding.group_with preceeding opn in
    opn ^^ pre_nest (name ^^ nest indent (Payload.pp payload ^^ S.rbracket))

  let pp ?preceeding ?(floating=false) (ext_name, ext_payload, _tokens) =
    match ext_payload with
    | PString (s, delim) ->
      let ext_name = String.concat "." ext_name.txt in
      (* Careful: single token! *)
      let percent = if floating then "%%" else "%" in
      let blank = if delim <> "" then " " else "" in
      let s =
        fancy_string "{%s%s%s%s|%s|%s}" percent ext_name blank delim s delim
      in
      Preceeding.group_with preceeding s
      |> fst
    | _ -> group (pp_classic ?preceeding ~floating ext_name ext_payload)

  let pp_toplevel { te_ext; te_attrs; te_pre_doc; te_post_doc } =
    pp ~floating:true te_ext
    |> Attribute.attach ~item:true ?pre_doc:te_pre_doc ~attrs:te_attrs
         ?post_doc:te_post_doc
end

and Payload : sig
  val pp : payload -> t
end = struct
  module Ends_in_obj_type = struct
    let rec core_type ct =
      ct.ptyp_attributes = [] &&
      match ct.ptyp_desc with
      | Ptyp_object (_, _) -> true
      | Ptyp_arrow { codom_type = rhs; codom_modes = []; _ }
      | Ptyp_poly (_, rhs) ->
        core_type rhs
      | Ptyp_tuple lst -> core_type (snd (List.hd (List.rev lst)))
      | Ptyp_any _
      | Ptyp_var _
      | Ptyp_parens _
      | Ptyp_unboxed_tuple _
      | Ptyp_constr (_, _)
      | Ptyp_class (_, _)
      | Ptyp_alias _
      | Ptyp_variant (_, _, _)
      | Ptyp_package _
      | Ptyp_open _
      | Ptyp_extension _
      | Ptyp_arrow _
      | Ptyp_of_kind _
        ->
        false

    let rec jkind_annotation jk =
      match jk.pjkind_desc with
      | With (_, ct, [])
      | Kind_of ct ->
        core_type ct
      | Product (_ :: _ as jks) ->
        jkind_annotation List.(hd @@ rev jks)
      | Default
      | Abbreviation _
      | Mod _
      | With (_, _, _ :: _)
      | Product []
      | Parens _ ->
        false

    let constructor_argument ca =
      ca.pca_modalities = [] && core_type ca.pca_type

    let constructor_decl cd =
      cd.pcd_attributes = [] &&
      match cd with
      | { pcd_res = Some ct; _ } -> core_type ct
      | { pcd_args = Pcstr_tuple (_ :: _ as args); _ } ->
        constructor_argument (List.hd @@ List.rev args)
      | _ -> false

    let type_declaration td =
      td.ptype_attributes = [] &&
      match td with
      | { ptype_cstrs = (_ :: _ as cstrs); _ } ->
        let _, ct, _ = List.hd (List.rev cstrs) in
        core_type ct
      | { ptype_manifest = Some ct; ptype_kind = Ptype_abstract; _ } ->
        core_type ct
      | { ptype_kind = Ptype_variant (_ :: _ as cds); _ } ->
        constructor_decl List.(hd (rev cds))
      | _ -> false

    let rec type_declarations = function
      | [] -> false
      | [ td ] -> type_declaration td
      | _ :: tds -> type_declarations tds

    let extension_constructor ec =
      ec.pext_attributes = [] &&
      match ec.pext_kind with
      | Pext_decl (_, _, Some ret_ct) -> core_type ret_ct
      | Pext_decl (_, Pcstr_tuple (_ :: _ as args), None) ->
        constructor_argument List.(hd (rev args))
      | _ -> false

    let type_extension te =
      te.ptyext_attributes = [] &&
      extension_constructor List.(hd (rev te.ptyext_constructors))

    let type_exception exn =
      exn.ptyexn_attributes = [] &&
      extension_constructor exn.ptyexn_constructor

    let structure_item it =
      match it.pstr_desc with
      | Pstr_type (_, tds) -> type_declarations tds
      | Pstr_typext te -> type_extension te
      | Pstr_exception exn -> type_exception exn
      | Pstr_kind_abbrev (_, jk) -> jkind_annotation jk
      | Pstr_eval _
      | Pstr_value _
      | Pstr_primitive _
      | Pstr_module _
      | Pstr_recmodule _
      | Pstr_modtype _
      | Pstr_open _
      | Pstr_include _
      | Pstr_attribute _
      | Pstr_extension _
      | Pstr_class _
      | Pstr_class_type _
      | Pstr_docstring _
        ->
        false

    let value_description vd =
      vd.pval_attributes = [] &&
      vd.pval_prim = [] &&
      vd.pval_modalities = [] &&
      core_type vd.pval_type

    let signature_item si =
      match si.psig_desc with
      | Psig_value vd -> value_description vd
      | Psig_type (_, decls)
      | Psig_typesubst decls ->
        type_declarations decls
      | Psig_typext te -> type_extension te
      | Psig_exception exn -> type_exception exn
      | Psig_kind_abbrev (_, jk) -> jkind_annotation jk
      | Psig_attribute _
      | Psig_extension _
      | Psig_docstring _
      | Psig_class _
      | Psig_class_type _
      (* FIXME: any of the following could have a with constraint. *)
      | Psig_module _
      | Psig_recmodule _
      | Psig_modsubst _
      | Psig_modtype _
      | Psig_modtypesubst _
      | Psig_open _
      | Psig_include _
        ->
        false

    let structure (items, _tokens) =
      let rec last = function
        | [] -> false
        | [ item ] -> structure_item item
        | _ :: items -> last items
      in
      last items

    let signature sg =
      let rec last = function
        | [] -> false
        | [ item ] -> signature_item item
        | _ :: items -> last items
      in
      last sg.psg_items
  end

  let pp = function
    | PString _ -> assert false (* handled in Extension *)
    | PStr s ->
      let doc = Structure.pp_implementation s in
      begin match doc with
      | Empty -> doc
      | _ ->
        break 1 ^^ doc ^^
        if Ends_in_obj_type.structure s then break 1 else empty
      end
    | PSig s ->
      S.colon ^/^ Signature.pp_interface s ^^
      if Ends_in_obj_type.signature s then break 1 else empty
    | PTyp c ->
      S.colon ^/^ Core_type.pp c ^^
      if Ends_in_obj_type.core_type c then break 1 else empty
    | PPat (p, eo) ->
      S.qmark ^/^ Pattern.pp p ^^
      match eo with
      | None -> empty
      | Some e -> break 1 ^^ S.when_ ^/^ Expression.pp e
end

(** {1 Core language} *)
(** {2 Type expressions} *)


and Type_constructor : sig
  val pp_core_type
    : ?class_:bool
    -> ?preceeding:Preceeding.t
    -> Tokens.seq
    -> core_type list
    -> Longident.t
    -> t

  val pp_class_constr : core_type list -> Longident.t -> t

  val pp_decl
    :  Tokens.seq
    -> ptype_params
    -> t
    -> t

  val pp_class_info_decl : ptype_params -> string -> t
end = struct
  type params_delimiters =
    | Brackets of { spaces_required: bool }
    | Parens of { omit_when_single_param: bool }

  let pp (type a) ?preceeding ~(pp_arg:?preceeding:Preceeding.t -> a -> t) delims
        (args : a list) ty_doc =
        let flatness = flatness_tracker () in
    let left, right, omit =
      match delims with
      | Brackets { spaces_required } ->
        let sp = if spaces_required then break 1 else empty in
        S.lbracket ^^ sp, sp ^^ S.rbracket, false
      | Parens { omit_when_single_param } ->
        let extra_space = vanishing_whitespace (Condition.flat flatness) nbsp in
        S.lparen ^^ extra_space, extra_space ^^ S.rparen, omit_when_single_param
    in
    match args with
    | [] ->
      Preceeding.group_with preceeding ty_doc
      |> fst
    | arg :: args ->
      match args with
      | [] when omit ->
        let pre_nest = Preceeding.implied_nest preceeding in
        pp_arg ?preceeding arg ^/^ pre_nest @@ nest 2 ty_doc
      | _ ->
        let left, pre_nest = Preceeding.extend preceeding left ~indent:2 in
        let comma_pre = Preceeding.mk (S.comma ^^ break 1) ~indent:2 in
        let left_and_args =
          List.fold_left (fun acc arg ->
            acc ^^ break 0 ^^ pre_nest (pp_arg ~preceeding:comma_pre arg)
          ) (pp_arg ~preceeding:left arg) args
        in
        group ~flatness (left_and_args ^^ pre_nest right) ^/^
        pre_nest @@ nest 2 ty_doc

  let pp_core_type ?(class_=false) ?preceeding tokens args lid =
    let delims =
      Parens {
        omit_when_single_param =
          args <> [] && not (starts_with LPAREN tokens)
      }
    in
    let lid = (if class_ then S.hash else empty) ^^ longident lid in
    pp ?preceeding ~pp_arg:Core_type.pp delims args lid

  let pp_decl tokens params name =
    let delims =
      Parens {
        omit_when_single_param =
          params <> [] && not (lparen_before_child tokens)
      }
    in
    pp ~pp_arg:Type_param.pp delims params name

  let starts_or_ends_with_obj = function
    | [] -> false
    | first :: rest ->
      let is_obj ct =
        match ct.ptyp_desc with
        | Ptyp_object _ -> true
        | _ -> false
      in
      let rec is_obj_last = function
        | [] -> false
        | [ x ] -> is_obj x
        | _ :: xs -> is_obj_last xs
      in
      is_obj first || is_obj_last rest

  let pp_class_constr args lid =
    let delims = Brackets { spaces_required = starts_or_ends_with_obj args } in
    let lid = longident lid in
    pp ~pp_arg:Core_type.pp delims args lid

  let pp_class_info_decl params name =
    let delims = Brackets { spaces_required = false } in
    pp ~pp_arg:Type_param.pp delims params (string name)
end

and Core_type : sig
  val pp : ?preceeding:Preceeding.t -> core_type -> t
  val pp_for_decl : core_type -> t

  module Arrow : sig
    type printer = ?preceeding:Preceeding.t -> unit -> t

    val pp_ret_ty : modes * modes * core_type -> printer

    val components
      :  core_type
      -> ((string loc * jkind_annotation option) list
          * arrow_arg list
          * (modes * modes * core_type)) option

    val pp
      :  ?preceeding:Preceeding.t
      -> flatness
      -> (string loc * jkind_annotation option) list
      -> arrow_arg list
      -> pp_rhs:printer
      -> t

    val pp_for_descr
      :  flatness
      -> (string loc * jkind_annotation option) list
      -> arrow_arg list
      -> pp_rhs:printer
      -> t
  end

  val pp_poly_bindings
    :  ?preceeding:Preceeding.t
    -> (string loc * jkind_annotation option) list
    -> t
end = struct

  let pp_var ?preceeding var_name jkind =
    let var, pre_nest =
      Preceeding.group_with preceeding (S.squote ^^ string var_name)
    in
    match jkind with
    | None -> var
    | Some k -> var ^/^ pre_nest (S.colon ^/^ Jkind_annotation.pp k)

  let pp_poly_bindings ?preceeding bound_vars =
    let pp_bound (var, jkind) =
      let var_and_kind = pp_var var.Location.txt jkind in
      match jkind with
      | None -> var_and_kind
      | Some _ -> parens var_and_kind
    in
    match bound_vars with
    | [] -> assert false
    | v :: vs ->
      let v, pre_nest = Preceeding.group_with preceeding (pp_bound v) in
      let vs = List.map (fun v -> pre_nest (pp_bound v)) vs in
      flow (break 1) (v :: vs)

  module Arrow = struct
    type printer = ?preceeding:Preceeding.t -> unit -> t

    let rec collect_args rev_args = function
      | [], [], { ptyp_desc = Ptyp_arrow arrow; _ } ->
        (* there can't be attributes on Ptyp_arrow, they'd be on Ptyp_parens *)
        collect_args (arrow.domain :: rev_args)
          (arrow.codom_legacy_modes, arrow.codom_modes, arrow.codom_type)
      | codom -> List.rev rev_args, codom

    let components ct =
      match ct.ptyp_desc with
      | Ptyp_arrow { domain=dom; codom_legacy_modes; codom_type; codom_modes } ->
        let args, ret_ty =
          collect_args [dom] (codom_legacy_modes, codom_modes, codom_type)
        in
        Some ([], args, ret_ty)
      | Ptyp_poly (poly_params,
                   { ptyp_desc =
                       Ptyp_arrow
                         { domain=dom; codom_legacy_modes
                         ; codom_type; codom_modes }
                   ; ptyp_attributes = [] ; _ }) ->
        let args, ret_ty =
          collect_args [dom] (codom_legacy_modes, codom_modes, codom_type)
        in
        Some (poly_params, args, ret_ty)
      | _ ->
        None

    let pp_arg_moded_ty pre_m post_m ty ?preceeding () =
      let pre_nest = Preceeding.implied_nest preceeding in
      let pre_ty =
        match pre_m with
        | [] -> Core_type.pp ?preceeding ty
        | _ ->
          let pre_modes, _ =
            Preceeding.group_with preceeding (modes_legacy pre_m)
          in
          pre_modes ^/^ pre_nest (Core_type.pp ty)
      in
      with_modes ~modes:post_m pre_ty ~extra_nest:pre_nest

    let pp_ret_ty (pre_m, post_m, ty) = pp_arg_moded_ty pre_m post_m ty

    let pp_arg ?preceeding
          { aa_lbl; aa_legacy_modes; aa_type; aa_modes; aa_doc; aa_tokens;
            aa_loc = _ } =
      let pre_nest = Preceeding.implied_nest preceeding in
      let arg =
        match aa_lbl with
        | Nolabel ->
          pp_arg_moded_ty ?preceeding aa_legacy_modes aa_modes aa_type ()
        | Labelled s ->
          let lbl, _ = Preceeding.group_with preceeding (string s ^^ S.colon) in
          lbl ^^ break 0 ^^
          nest 2 @@ pre_nest (pp_arg_moded_ty aa_legacy_modes aa_modes aa_type ())
        | Optional s ->
          let lbl =
            if
              List.exists (function
                | Tokens.{ desc = Token OPTLABEL _; _ } -> true
                | _ -> false
              ) aa_tokens
            then stringf "?%s:" s
            else S.qmark ^^ string s ^^ S.colon
          in
          let lbl, _ = Preceeding.group_with preceeding lbl in
          lbl ^^ break 0 ^^
          nest 2 @@ pre_nest (pp_arg_moded_ty aa_legacy_modes aa_modes aa_type ())
      in
      let arg = group arg ^?^ nest 2 @@ pre_nest @@ optional Doc.pp aa_doc in
      group arg

    let pp ?(for_descr=false) flatness ?preceeding poly_params args
          ~(pp_rhs:printer) =
      let extra_space_vanishing_cond = Condition.flat flatness in
      let mk_pre_doc token =
        group (token ^/^ vanishing_whitespace extra_space_vanishing_cond nbsp)
      in
      let first_pre, pre_nest =
        if for_descr then
          let pre, pre_nest =
            Preceeding.extend preceeding ~indent:3 (mk_pre_doc S.colon)
          in
          Some pre, pre_nest
        else
          preceeding, Preceeding.implied_nest preceeding
      in
      let params, preceeding, first_pre_nest =
        match poly_params with
        | [] -> empty, first_pre, Fun.id
        | _ ->
          let polys = pp_poly_bindings ?preceeding:first_pre poly_params in
          let dot_pre = Preceeding.mk ~indent:3 (mk_pre_doc S.dot) in
          polys ^^ break 0, Some dot_pre, pre_nest
      in
      let arrow_pre = Preceeding.mk (S.rarrow ^^ break 1) ~indent:3 in
      let args =
        List.mapi (fun i arg ->
          if i = 0
          then first_pre_nest @@ pp_arg ?preceeding arg
          else pre_nest @@ pp_arg ~preceeding:arrow_pre arg
        ) args
        |> separate (break 1)
      in
      params ^^ args ^/^ pre_nest @@ pp_rhs ~preceeding:arrow_pre ()

    let pp_for_descr descr_flatness poly_params args ~pp_rhs =
      pp ~for_descr:true descr_flatness poly_params args ~pp_rhs

    let pp ?preceeding ct_flatness poly_params args ~pp_rhs =
      pp ?preceeding ct_flatness poly_params args ~pp_rhs
  end

  let rec pp ?preceeding ?(in_parens=false)
            ({ ptyp_desc=_; ptyp_attributes; ptyp_tokens = _; ptyp_loc=_} as ct)
    =
    let attach_attrs doc =
      if in_parens
      then doc ^^ Attribute.pp_list ptyp_attributes
      else doc ^?^ Attribute.pp_list ptyp_attributes
    in
    let desc_doc =
      match Arrow.components ct with
      | None -> group (pp_desc ?preceeding ct)
      | Some (poly_params, args, ret) ->
        let flatness = flatness_tracker () in
        Arrow.pp ?preceeding flatness poly_params args
          ~pp_rhs:(Arrow.pp_ret_ty ret)
        |> group ~flatness
    in
    attach_attrs desc_doc

  and pp_desc ?preceeding ct =
    let tokens = ct.ptyp_tokens in
    match ct.ptyp_desc with
    | Ptyp_arrow _ -> assert false (* handled in [pp] *)
    | Ptyp_any None ->
      Preceeding.group_with preceeding S.underscore
      |> fst
    | Ptyp_any Some k ->
      let underscore, pre_nest =
        Preceeding.group_with preceeding S.underscore
      in
      underscore ^/^ pre_nest (S.colon ^/^ Jkind_annotation.pp k)
    | Ptyp_var (s, ko) -> pp_var ?preceeding s ko
    | Ptyp_tuple elts -> pp_tuple ?preceeding elts
    | Ptyp_unboxed_tuple elts ->
      let hlparen, pre_nest =
        Preceeding.group_with preceeding S.hash_lparen
      in
      hlparen ^^ pre_nest (pp_tuple elts ^^ S.rparen)
    | Ptyp_constr (args, lid) ->
      Type_constructor.pp_core_type ?preceeding tokens args lid.txt
    | Ptyp_object (fields, closed) -> pp_object ?preceeding fields closed
    | Ptyp_class (lid, args) ->
      Type_constructor.pp_core_type ?preceeding ~class_:true tokens args lid.txt
    | Ptyp_alias (ct, name, None) ->
      let pre_nest = Preceeding.implied_nest preceeding in
      pp ?preceeding ct ^/^ pre_nest (
        S.as_ ^/^
        S.squote ^^ string (Option.get name).txt
      )
    | Ptyp_alias (ct, name_o, Some jkind) ->
      let pre_nest = Preceeding.implied_nest preceeding in
      pp ?preceeding ct ^/^ pre_nest (
        S.as_ ^/^
        S.lparen ^^ break 0 ^^ (
          match name_o with
          | None -> S.underscore
          | Some s -> S.squote ^^ string s.txt
        ) ^^ S.colon ^^ Jkind_annotation.pp jkind ^^ break 0 ^^ S.rparen
      )
    | Ptyp_variant (fields, cf, lbls) ->
      pp_variant ?preceeding ~tokens fields cf lbls
    | Ptyp_poly (bound_vars, ty) ->
      let pre_nest = Preceeding.implied_nest preceeding in
      let pre_vars = pp_poly_bindings ?preceeding bound_vars in
      group (pre_vars ^^ break 0 ^^ pre_nest S.dot) ^/^ pre_nest (pp ty)
    | Ptyp_package (ext_attrs, pkg) -> package_type ?preceeding ext_attrs pkg
    | Ptyp_open (lid, ct) ->
      let space =
        match ct.ptyp_desc with
        | Ptyp_unboxed_tuple _ -> break 1
        | _ -> empty
      in
      let pre_lid, pre_nest =
        Preceeding.group_with preceeding (longident lid.txt ^^ S.dot)
      in
      pre_lid ^^ pre_nest (space ^^ pp ct)
    | Ptyp_of_kind jkind ->
      let pre_type, pre_nest = Preceeding.group_with preceeding S.type_ in
      pre_type ^/^ pre_nest (S.colon ^/^ Jkind_annotation.pp jkind)
    | Ptyp_extension ext ->
      Extension.pp ext
      |> Preceeding.group_with preceeding
      |> fst
    | Ptyp_parens ct -> pp_parens ?preceeding ct

  and pp_parens ?preceeding ct =
    let lparen, pre_nest = Preceeding.extend preceeding S.lparen ~indent:1 in
    pp ~in_parens:true ~preceeding:lparen ct ^^ pre_nest S.rparen

  and pp_variant ?(compact=true) ?preceeding ~tokens fields
        (cf : Asttypes.closed_flag) lbls =
    let pre_nest = Preceeding.implied_nest preceeding in
    let pp_fields bracket =
      let init =
        bracket ^?^ if pipe_before_child tokens then S.pipe else empty
      in
      match fields with
      | [] -> fst (Preceeding.group_with preceeding init)
      | rf :: rfs ->
        let indent = Requirement.to_int (requirement init) + 1 in
        let first_pre, _ =
          Preceeding.extend preceeding (init ^^ break 1) ~indent
        in
        let pipe_pre = Preceeding.mk (S.pipe ^^ break 1) ~indent in
        let rf = row_field first_pre rf in
        let rfs = List.map (fun rf -> pre_nest (row_field pipe_pre rf)) rfs in
        separate (if compact then break 1 else hardline) (rf :: rfs)
    in
    match cf, lbls with
    | Closed, None -> pp_fields S.lbracket ^/^ pre_nest S.rbracket
    | Open, None -> pp_fields S.lbracket_gt ^/^ pre_nest S.rbracket
    | Closed, Some [] -> pp_fields S.lbracket_lt ^/^ pre_nest S.rbracket
    | Closed, Some labels ->
      pp_fields S.lbracket_lt ^/^ pre_nest (
        S.gt ^/^ separate_map (break 1) pp_poly_tag labels ^/^
        S.rbracket
      )
    | Open, Some _ -> assert false

  and row_field preceeding { prf_desc; prf_attributes; prf_doc;
                                 prf_loc = _; prf_tokens = _ } =
    let pre_nest = Preceeding.implied_nest (Some preceeding) in
    prefix (row_field_desc preceeding prf_desc)
      (pre_nest @@ Attribute.pp_list prf_attributes ^?^ optional Doc.pp prf_doc)

  and row_field_desc preceeding = function
    | Rinherit ct -> pp ~preceeding ct
    | Rtag (label, _, []) ->
      pp_poly_tag label.txt
      |> Preceeding.group_with (Some preceeding)
      |> fst
    | Rtag (label, has_const, at_types) ->
      let pre_tag, pre_nest =
        Preceeding.group_with (Some preceeding)
          (pp_poly_tag label.txt ^/^ S.of_)
      in
      let args =
        (if has_const then S.ampersand ^^ break 1 else empty) ^^
        separate_map (break 1 ^^ S.ampersand ^^ break 1) pp at_types
      in
      (* N.B. the [nest 0] below mimics ocamlformat, but feels *wrong*. *)
      group (pre_tag ^/^ pre_nest (nest 0 args))

  and pp_poly_tag lbl = S.bquote ^^ string lbl

  and pp_tuple ?preceeding elts =
    let pp_elt ?preceeding (lbl_opt, ct) =
      match lbl_opt with
      | None -> pp ?preceeding ct
      | Some s ->
        let lbl, pre_nest =
          string s ^^ S.colon ^^ break 0
          |> Preceeding.group_with preceeding
        in
        lbl ^^ pre_nest (pp ct)
    in
    let pre_nest = Preceeding.implied_nest preceeding in
    let elts =
      List.mapi (fun i elt ->
        if i = 0
        then pp_elt ?preceeding elt
        else pre_nest (pp_elt elt)
      ) elts
    in
    separate (break 1 ^^ pre_nest S.star ^^ break 1) elts

  and package_type ?preceeding ext_attrs mty =
    let module_ = Ext_attribute.decorate S.module_ ext_attrs in
    let pre_lparen_module, pre_nest =
      Preceeding.group_with preceeding (S.lparen ^^ module_)
    in
    pre_lparen_module ^/^ pre_nest (Module_type.pp mty ^^ break 0 ^^ S.rparen)

  and pp_object ?preceeding fields closed =
    let lt, pre_nest = Preceeding.group_with preceeding S.lt in
    let fields_doc = separate_map (break 1) object_field fields in
    let fields_and_dotdot =
      match fields, closed with
      | _, Asttypes.Closed -> fields_doc
      | _, Open -> fields_doc ^?^ S.dotdot
    in
    lt ^/^ pre_nest (nest 2 fields_and_dotdot ^/^ S.gt)

  and object_field { pof_desc; pof_attributes; pof_doc; pof_tokens;
                     pof_loc = _ } =
    let semi = List.exists (fun t -> t.Tokens.desc = Token SEMI) pof_tokens in
    prefix (group (object_field_desc pof_desc)) (
      Attribute.pp_list pof_attributes
      ^?^ optional Doc.pp pof_doc
    ) ^^ if semi then S.semi else empty

  and object_field_desc = function
    | Oinherit ct -> pp ct
    | Otag (lbl, ct) -> string lbl.txt ^^ S.colon ^/^ pp ct

  let pp_for_decl
        ({ ptyp_desc; ptyp_attributes; ptyp_tokens; ptyp_loc=_} as ct) =
    match ptyp_desc with
    | Ptyp_variant (rf, cf, lbls) ->
      pp_variant ~compact:false ~tokens:ptyp_tokens rf cf lbls
      |> Attribute.attach ~attrs:ptyp_attributes
    | _ -> pp ct

  let pp ?preceeding ct = pp ?preceeding ct
end

(** {2 Patterns} *)

and Pattern : sig
  val pp : ?preceeding:Preceeding.t -> pattern -> t
end = struct
  let rec pp ?preceeding p =
    let pat_lvl = flatness_tracker () in
    group ~flatness:pat_lvl (pp_desc ~preceeding pat_lvl p)
    |> Attribute.attach ~attrs:p.ppat_attributes

  and pp_desc ~preceeding pat_flatness p =
    let (!!) kw = Ext_attribute.decorate kw p.ppat_ext_attr in
    match p.ppat_desc with
    | Ppat_any ->
      Preceeding.group_with preceeding S.underscore
      |> fst
    | Ppat_var name ->
      Preceeding.group_with preceeding (str_or_op name.txt)
      |> fst
    | Ppat_alias (p, alias) ->
      let pre_nest = Preceeding.implied_nest preceeding in
      pp ?preceeding p ^^
      (* try to fit the alias at the end of the current line, but prioritize
         keeping the [as alias] together over filling the line. *)
      group (
        break 1 ^^
        pre_nest @@ nest 2 (group (S.as_ ^/^ str_or_op alias.txt))
      )
    | Ppat_constant c ->
      Preceeding.group_with preceeding (constant c)
      |> fst
    | Ppat_interval (c1,c2) ->
      group (constant c1 ^/^ S.dotdot ^/^ constant c2)
      |> Preceeding.group_with preceeding
      |> fst
    | Ppat_tuple (elts, closed) -> pp_tuple ?preceeding closed elts
    | Ppat_unboxed_tuple (elts, cf) ->
      let pre_hash_lparen, _ =
        Preceeding.extend preceeding ~indent:1 S.hash_lparen
      in
      pp_tuple ~preceeding:pre_hash_lparen cf elts ^^ S.rparen
    | Ppat_construct (lid, arg) -> pp_construct ~preceeding p.ppat_tokens lid arg
    | Ppat_variant (lbl, arg) -> pp_variant ~preceeding lbl arg
    | Ppat_record (fields, cf) ->
      pp_record ~preceeding (nb_semis p.ppat_tokens) cf fields
    | Ppat_record_unboxed_product (fields, cf) ->
      pp_record ~preceeding ~unboxed:true (nb_semis p.ppat_tokens) cf fields
    | Ppat_array (mut, ps) ->
      pp_array ~preceeding (nb_semis p.ppat_tokens) mut ps
    | Ppat_or (p1, p2) ->
      let pre_nest = Preceeding.implied_nest preceeding in
      let pipe_pre = Preceeding.mk (S.pipe ^^ break 1) ~indent:2 in
      pp ?preceeding p1 ^/^
      pre_nest (pp ~preceeding:pipe_pre p2)
    | Ppat_constraint (p, ty, modes) ->
      pp_constraint ~preceeding p ty modes
    | Ppat_type lid ->
      Preceeding.group_with preceeding (S.hash ^^ longident lid.txt)
      |> fst
    | Ppat_lazy p ->
      let lazy_, pre_nest = Preceeding.group_with preceeding !!S.lazy_ in
      lazy_ ^/^ pre_nest @@ nest 2 (pp p)
    | Ppat_unpack (path, ty) ->
      pp_unpack ~preceeding p.ppat_ext_attr path ty
    | Ppat_exception p ->
      let exn, pre_nest = Preceeding.group_with preceeding !!S.exception_ in
      exn ^/^ pre_nest @@ nest 2 (pp p)
    | Ppat_extension ext -> Extension.pp ?preceeding ext
    | Ppat_open (lid, p) -> pp_open ~preceeding lid p
    | Ppat_parens { pat; optional } ->
      pp_parens ~preceeding ~optional pat_flatness pat
    | Ppat_list elts -> pp_list ~preceeding (nb_semis p.ppat_tokens) elts
    | Ppat_cons (hd, tl) ->
      let pre_nest = Preceeding.implied_nest preceeding in
      group (
        pp_cons ?preceeding hd ^/^
        pre_nest (pp_cons ~on_right:true tl)
      )

  and pp_cons ?preceeding ?(on_right=false) lst =
    match lst.ppat_desc with
    | Ppat_cons (hd, tl) ->
      let pre_nest = Preceeding.implied_nest preceeding in
      pp_cons ?preceeding ~on_right hd ^/^
      pre_nest (
        pp_cons ~on_right:true tl
        |> Attribute.attach ~attrs:lst.ppat_attributes
      )
    | _ ->
      let preceeding =
        if on_right then (
          assert (Option.is_none preceeding);
          Some (Preceeding.mk (S.cons ^^ break 1) ~indent:3)
        ) else
          preceeding
      in
      pp ?preceeding lst


  and pp_parens ~preceeding ~optional flatness pat =
    match pat.ppat_desc with
    | Ppat_tuple (pats, closed) ->
      pp_parens_tuple ~preceeding ~optional ~closed flatness
        ~attrs:pat.ppat_attributes pats
    | _ ->
      let before, after =
        (* No break? *)
        if optional
        then
          let cond = Condition.flat flatness in
          opt_token cond "(", opt_token cond ")"
        else S.lparen, S.rparen
      in
      let before, pre_nest = Preceeding.extend preceeding before ~indent:1 in
      pp ~preceeding:before pat ^^ pre_nest after

  and pp_parens_tuple ~preceeding ~attrs ~optional ~closed flatness pats =
    let lparen, rparen =
      if optional
      then
        let cond = Condition.flat flatness in
        opt_token cond "(" ~ws_after:(break 1),
        opt_token cond ~ws_before:(break 1) ")"
      else
        let pat_is_flat = Condition.flat flatness in
        let space_when_multiline =
          group (vanishing_whitespace pat_is_flat nbsp)
        in
        S.lparen ^^ space_when_multiline ^^ break 0,
        space_when_multiline ^^ break 0 ^^ S.rparen
    in
    let lparen, pre_nest = Preceeding.extend preceeding lparen ~indent:2 in
    let comma = Preceeding.mk (S.comma ^^ break 1) ~indent:2 in
    let join a b = a ^^ break 0 ^^ pre_nest b in
    let pats =
      foldli (fun i acc pat ->
        if i = 0
        then Argument.pp_preceeded ~preceeding:lparen pp pat
        else join acc (Argument.pp_preceeded ~preceeding:comma pp pat)
      ) empty pats
    in
    let pats =
      match closed with
      | Asttypes.Closed -> pats
      | Open -> join pats (S.comma ^/^ S.dotdot)
    in
    (* FIXME: attributes indent! *)
    Attribute.attach ~attrs pats ^^ pre_nest (group rparen)

  and pp_open ~preceeding lid p =
    let space =
      match p.ppat_desc with
      | Ppat_unboxed_tuple _
      | Ppat_record_unboxed_product _ -> break 1
      | _ -> empty
    in
    let lid = longident lid.txt in
    let pre_lid, pre_nest = Preceeding.group_with preceeding lid in
    pre_lid ^^ pre_nest (S.dot ^^ space ^^ pp p)

  and pp_delimited_seq ~preceeding (opn, cls) nb_semis = function
    | [] ->
      let opn, pre_nest = Preceeding.group_with preceeding opn in
      group (opn ^/^ pre_nest cls)
    | elts ->
      let semi_as_term = List.compare_length_with elts nb_semis = 0 in
      let pre_opn, pre_nest =
        Preceeding.extend preceeding (opn ^^ break 1) ~indent:2
      in
      let semi_as_pre = Preceeding.mk (S.semi ^^ break 1) ~indent:2 in
      let opn_and_pats =
        List.mapi (fun i pat ->
          if i = 0
          then pp ~preceeding:pre_opn pat
          else pre_nest (pp ~preceeding:semi_as_pre pat)
        ) elts
      in
      separate (break 0) opn_and_pats ^^
      (if semi_as_term then break 0 ^^ S.semi else empty)
      ^/^ pre_nest cls

  and pp_array ~preceeding nb_semis mut =
    pp_delimited_seq ~preceeding (array_delimiters mut) nb_semis

  and pp_list ~preceeding nb_semis =
    pp_delimited_seq ~preceeding (S.lbracket, S.rbracket) nb_semis

  and pp_constraint ~preceeding p ty_opt modes =
    let pre_lparen, pre_nest =
      Preceeding.extend preceeding ~indent:1 S.lparen
    in
    let p = pp ~preceeding:pre_lparen p in
    let p_with_extras =
      match ty_opt with
      | None -> with_modes ~extra_nest:pre_nest ~modes p
      | Some ty ->
        (* Fit colon at EOL if we can. *)
        let p =
          p ^^ pre_nest (
            group (break 1 ^^ S.colon) ^/^
            nest 2 (Core_type.pp ty)
          )
        in
        with_atat_modes ~extra_nest:pre_nest ~modes p
    in
    p_with_extras ^^ S.rparen

  and pp_unpack ~preceeding ext_attrs path ty =
    let pre_lp_mod, pre_nest =
      Preceeding.group_with preceeding (
        S.lparen ^^ break 0 ^^ Ext_attribute.decorate S.module_ ext_attrs
      )
    in
    let path =
      match path.txt with
      | None -> S.underscore
      | Some s -> string s
    in
    pre_lp_mod ^/^
    pre_nest (
      nest 1 (path ^?^ optional (fun c -> S.colon ^/^ Module_type.pp c) ty)
      ^^ break 0 ^^ S.rparen
    )

  and pp_tuple ?preceeding closed pats =
    let pre_nest = Preceeding.implied_nest preceeding in
    let comma_join a b = a ^^ pre_nest (break 0 ^^ S.comma ^/^ b) in
    let pats =
      foldli (fun i acc pat ->
        if i = 0
        then Argument.pp_preceeded ?preceeding pp pat
        else comma_join acc (Argument.pp pp pat)
      ) empty pats
    in
    match closed with
    | Closed -> pats
    | Open -> comma_join pats S.dotdot

  and pp_construct ~preceeding tokens name arg_opt =
    let name, pre_nest =
      Preceeding.group_with preceeding (constr_longident name.txt)
    in
    let pp_annotated_newtype nt jkind =
      string nt.txt ^/^ S.colon ^/^ Jkind_annotation.pp jkind
    in
    match arg_opt with
    | None -> name
    | Some ([], arg_pat) ->
      name ^/^ pre_nest @@ nest 2 (pp arg_pat)
    | Some ([newtype, Some jkind], arg_pat)
      when not (has_leading LPAREN ~after:TYPE tokens) ->
      (* We could decide to "normalize" this case.
         Here we are careful because we don't want to trigger if the user wrote
         {[
           Constr (type (a : jk)) arg
         ]} *)
      name ^/^ pre_nest @@ nest 2 (
        parens (S.type_ ^/^ pp_annotated_newtype newtype jkind) ^/^
        pp arg_pat
      )
    | Some (bindings, arg_pat) ->
      let binding (newtype, jkind) =
        match jkind with
        | None -> string newtype.txt
        | Some jkind -> parens (pp_annotated_newtype newtype jkind)
      in
      name ^/^ pre_nest @@ nest 2 (
        parens (S.type_ ^/^ flow_map (break 1) binding bindings) ^/^
        pp arg_pat
      )

  and pp_variant ~preceeding lbl pat_o =
    let tag, pre_nest =
      Preceeding.group_with preceeding (S.bquote ^^ string lbl)
    in
    tag ^?^ pre_nest @@ nest 2 (optional pp pat_o)

  and pp_record ~preceeding ?(unboxed=false) nb_semis closed_flag fields =
    let semi_as_term =
      let nb_fields =
        List.length fields +
        if closed_flag = Closed then 0 else 1 (* underscore as extra field *)
      in
      (* [;] is used as a terminator if there are as many as there are fields *)
      nb_fields = nb_semis
    in
    let pre_lbrace, pre_nest =
      Preceeding.extend preceeding ~indent:2
        ((if unboxed then S.hash_lbrace else S.lbrace) ^^ break 1)
    in
    let semi_as_pre = Preceeding.mk ~indent:2 (S.semi ^^ break 1) in
    group (
      foldli (fun i acc field ->
        if i = 0 then
          (* acc = empty *)
          Record_field.pp ~preceeding:pre_lbrace pp field
        else
          acc ^^ break 0 ^^
          pre_nest (Record_field.pp ~preceeding:semi_as_pre pp field)
      ) empty fields ^^
      pre_nest (
        begin match closed_flag with
        | Asttypes.Open -> break 0 ^^ group (S.semi ^/^ S.underscore)
        | Closed -> empty
        end ^^
        (if semi_as_term then break 0 ^^ S.semi else empty) ^/^
        S.rbrace
      )
    )

end

(** {2 Value expressions} *)

and Expression : sig
  val pp : ?preceeding:Preceeding.t -> expression -> t

  val pp_function_parts : ?preceeding:Preceeding.t -> expression -> t * t
end = struct
  let pp_op op =
    match op.pexp_desc with
    | Pexp_ident { txt = { desc = Lident Op s; _ }; _ } ->
      string s
    | _ -> assert false

  let rec pp ?preceeding e =
    pp_desc ~preceeding e
    |> Attribute.attach ~attrs:e.pexp_attributes

  and opt_space_then_pp ~extra_nest e =
    let rec needs_space e =
      match e.pexp_desc with
      (* we force a space before # *)
      | Pexp_unboxed_tuple _
      | Pexp_record_unboxed_product _
      | Pexp_constant
          ( Pconst_unboxed_float _
          | Pconst_unboxed_integer _
          | Pconst_untagged_char _)
      (* As well as before other operators *)
      | Pexp_prefix_apply _
      | Pexp_add_or_sub _
      (* Or when the next token is a tilde *)
      | Pexp_tuple ({ parg_desc = Parg_labelled _ ; _ } :: _) ->
        true
      (* Need to check rhs of "infix applications" *)
      | Pexp_infix_apply { arg1 = lhs; _ }
      | Pexp_send (lhs, _)
      | Pexp_field (lhs, _)
      | Pexp_index_op { seq = lhs; _ } ->
        needs_space lhs
      | _ -> false
    in
    extra_nest (
      (if needs_space e then break 1 else empty) ^^ pp e
    )

  and pp_desc ~preceeding exp =
    let (!!) kw = Ext_attribute.decorate kw exp.pexp_ext_attr in
    match exp.pexp_desc with
    | Pexp_ident lid ->
      Preceeding.group_with preceeding (longident lid.txt)
      |> fst
    | Pexp_constant c ->
      Preceeding.group_with preceeding (constant c)
      |> fst
    | Pexp_let (mf, rf, vbs, body) -> pp_let ~preceeding mf rf vbs body
    | Pexp_function _ -> pp_function ~preceeding exp
    | Pexp_prefix_apply (op, arg) ->
      let op, pre_nest = Preceeding.group_with preceeding (pp_op op) in
      op ^^ opt_space_then_pp ~extra_nest:pre_nest arg
    | Pexp_add_or_sub (op, arg) ->
      let op, pre_nest = Preceeding.group_with preceeding (string op) in
      op ^^ opt_space_then_pp ~extra_nest:pre_nest arg
    | Pexp_infix_apply {op; arg1; arg2} ->
      let pre_nest = Preceeding.implied_nest preceeding in
      let op_prec = Precedence.of_infix_op op in
      group (
        pp_op_apply ?preceeding ~op_prec arg1 ^/^
        pre_nest (pp_op_apply ~on_right:true ~op ~op_prec arg2)
      )
    | Pexp_apply (e, args) -> pp_apply ~preceeding e args
    | Pexp_match (e, cases) ->
      pp_match ~preceeding ~ext_attrs:exp.pexp_ext_attr
        ~tokens:exp.pexp_tokens e cases
    | Pexp_try (e, cases) ->
      pp_try ~preceeding ~ext_attrs:exp.pexp_ext_attr ~tokens:exp.pexp_tokens
        e cases
    | Pexp_tuple elts -> pp_tuple ?preceeding elts
    | Pexp_unboxed_tuple elts ->
      let flatness = flatness_tracker () in
      group ~flatness @@
      pp_parens_tuple ~preceeding ~kind:`Unboxed ~attrs:[] flatness elts
    | Pexp_construct (lid, arg) ->
      let lid, pre_nest =
        Preceeding.group_with preceeding @@ constr_longident lid.txt
      in
      group (lid ^?^ pre_nest @@ nest 2 (optional pp arg))
    | Pexp_variant (lbl, eo) -> pp_variant ~preceeding lbl eo
    | Pexp_record (eo, fields) ->
      pp_record ~preceeding (nb_semis exp.pexp_tokens) eo fields
    | Pexp_record_unboxed_product (eo, fields) ->
      pp_record ~preceeding ~unboxed:true (nb_semis exp.pexp_tokens) eo fields
    | Pexp_field (e, lid) -> pp_field ?preceeding e lid
    | Pexp_unboxed_field (e, lid) -> pp_field ?preceeding ~unboxed:true e lid
    | Pexp_setfield (e1, lid, e2) -> pp_setfield ?preceeding e1 lid e2
    | Pexp_array (mut, es) ->
      pp_array ~preceeding (nb_semis exp.pexp_tokens) mut es
    | Pexp_idx (ba, uas) ->
      (* FIXME: indent! *)
      Preceeding.group_with preceeding (pp_block_idx ba uas)
      |> fst
    | Pexp_ifthenelse (e1, e2, e3_o) ->
      pp_ifthenelse ~preceeding exp.pexp_ext_attr e1 e2 e3_o
    | Pexp_sequence (e1, e2) ->
      (* FIXME: ext_attr not at the beginning, the token synchronisation is
         going to have issues. *)
      pp ?preceeding e1 ^^ !!S.semi ^^ hardline ^^ pp e2
    | Pexp_seq_empty e ->
      pp ?preceeding e ^^ S.semi
      (* the following is needed if we're at the end of a record (or
         override) field for instance.

         TODO: improve. *)
      ^^ break 1
    | Pexp_while (e1, e2) ->
      let while_, pre_nest = Preceeding.group_with preceeding !!S.while_ in
      let fst_line =
        group (while_ ^/^ pre_nest (nest 2 (pp e1) ^/^ S.do_))
      in
      fst_line ^^ hardline ^^
      pre_nest (nest 2 (pp e2)) ^^ hardline ^^
      pre_nest S.done_
    | Pexp_for (p, e1, e2, dir, e3) ->
      let for_, pre_nest = Preceeding.group_with preceeding !!S.for_ in
      let fst_line =
        for_ ^/^
        group (pre_nest @@ nest 2 (
          Pattern.pp p ^/^ S.equals ^/^ pp e1 ^/^ direction dir ^/^ pp e2
        )) ^/^
        pre_nest S.do_
      in
      group fst_line ^^ hardline ^^
      pre_nest (nest 2 (pp e3)) ^^ hardline ^^
      pre_nest S.done_
    | Pexp_constraint (e, ct_opt, modes) ->
      pp_constraint ~preceeding e ct_opt modes
    | Pexp_coerce (e, ct1, ct2) ->
      pp_coerce ~preceeding e ct1 ct2
    | Pexp_send (e, lbl) ->
      let pre_nest = Preceeding.implied_nest preceeding in
      pp ?preceeding e ^^ break 0 ^^
      pre_nest (S.hash ^^ break 0 ^^ string lbl.txt)
    | Pexp_new lid ->
      let new_, pre_nest = Preceeding.group_with preceeding !!S.new_ in
      group (new_ ^/^ pre_nest (longident lid.txt))
    | Pexp_setvar (lbl, e) ->
      let pre_lbl, pre_nest =
        Preceeding.group_with preceeding (string lbl.txt)
      in
      group (pre_lbl ^/^ pre_nest S.larrow) ^/^
      pre_nest @@ nest 2 (pp e)
    | Pexp_override fields ->
      pp_override ~preceeding (nb_semis exp.pexp_tokens) fields
    | Pexp_letmodule (mb, body) ->
      let pre_nest = Preceeding.implied_nest preceeding in
      let binding =
        Module_binding.pp ~item:false ?preceeding
          ~keywords:[ S.let_; S.module_ ] mb ^/^
        S.in_
      in
      group binding ^^ hardline ^^ pre_nest (pp body)
    | Pexp_letexception (ec, body) ->
      let let_exn, pre_nest =
        Preceeding.group_with preceeding (group (S.let_ ^/^ !!S.exception_))
      in
      let binding =
        let_exn ^/^
        pre_nest (nest 2 (Extension_constructor.pp ec) ^/^ S.in_)
      in
      binding ^^ hardline ^^ pre_nest (pp body)
    | Pexp_assert e ->
      let assert_, pre_nest =
        Preceeding.group_with preceeding !!S.assert_
      in
      group (assert_ ^/^ pre_nest @@ nest 2 (pp e))
    | Pexp_lazy e ->
      let lazy_, pre_nest = Preceeding.group_with preceeding !!S.lazy_ in
      group (lazy_ ^/^ pre_nest @@ nest 2 (pp e))
    | Pexp_object cs -> Class_expr.pp_structure ?preceeding exp.pexp_ext_attr cs
    | Pexp_pack (me, ty) ->
      pp_pack ~preceeding ~ext_attr:exp.pexp_ext_attr me ty
    | Pexp_dot_open (lid, e) -> pp_dot_open ~preceeding lid e
    | Pexp_let_open (od, e) ->
      let pre_let, pre_nest = Preceeding.group_with preceeding S.let_ in
      let let_open =
        pre_let ^/^
        pre_nest (Open_declaration.pp ~item:false od ^/^ S.in_)
      in
      group let_open ^^ hardline ^^ pre_nest (pp e)
    | Pexp_letop lo -> Letop.pp ?preceeding lo
    | Pexp_extension ext -> Extension.pp ?preceeding ext
    | Pexp_unreachable  ->
      let dot, _ = Preceeding.group_with preceeding S.dot in
      dot ^^ break 1 (* prevents unintentional conversion into DOTOP *)
    | Pexp_stack e ->
      let stack__, pre_nest = Preceeding.group_with preceeding S.stack__ in
      stack__ ^/^ pre_nest @@ nest 2 (pp e)
    | Pexp_comprehension ce -> Comprehension.pp_expr ?preceeding ce
    | Pexp_overwrite (e1, e2) ->
      let overwrite, pre_nest =
        Preceeding.group_with preceeding !!S.overwrite__
      in
      group (
        overwrite ^/^
        pre_nest @@ nest 2 (pp e1)
      ) ^/^
      group @@ pre_nest (
        S.with_ ^/^
        nest 2 (pp e2)
      )
    | Pexp_hole ->
      Preceeding.group_with preceeding S.underscore
      |> fst
    | Pexp_index_op access ->
      pp_index_op ~preceeding (nb_semis exp.pexp_tokens)
        access.kind access.seq access.op access.indices access.assign
    | Pexp_parens { exp; optional } -> pp_parens ~preceeding ~optional exp
    | Pexp_begin_end exp ->
      let begin_, pre_nest = Preceeding.group_with preceeding !!S.begin_ in
      begin_ ^?^
      pre_nest (nest 2 (optional pp exp) ^?^ S.end_)
    | Pexp_list elts -> pp_list ~preceeding (nb_semis exp.pexp_tokens) elts
    | Pexp_cons (hd, tl) ->
      let pre_nest = Preceeding.implied_nest preceeding in
      group (
        pp_cons ?preceeding hd ^/^
        pre_nest (pp_cons ~on_right:true tl)
      )
    | Pexp_exclave exp ->
      let excl, pre_nest = Preceeding.group_with preceeding S.exclave__ in
      excl ^/^ pre_nest @@ nest 2 (pp exp)
    | Pexp_mode_legacy (m, exp) ->
      let mode, pre_nest =
        Preceeding.group_with preceeding (mode_legacy m.txt)
      in
      mode ^/^ pre_nest @@ nest 2 (pp exp)

  and pp_let ~preceeding mf rf vbs body =
    let pre_nest = Preceeding.implied_nest preceeding in
    let bindings =
      Value_binding.pp_list ?preceeding vbs ~start:(
        S.let_ ::
        match mf, rf with
        | Immutable, Nonrecursive -> []
        | Immutable, Recursive -> [S.rec_]
        | Mutable, Nonrecursive -> [S.mutable_]
        | Mutable, Recursive -> [S.mutable_; S.rec_]
      ) ~add_in:true
    in
    group bindings ^^ hardline ^^
    pre_nest (pp body)

  and pp_function_parts ?preceeding exp =
    match exp.pexp_desc with
    | Pexp_function ([], _, body) ->
      (* exp.pexp_ext_attr is empty in this case, we attach on the body. *)
      Function_body.pp_parts ?preceeding body
    | Pexp_function (params, constr, body) ->
      let fun_, pre_nest =
        Ext_attribute.decorate S.fun_ exp.pexp_ext_attr
        |> Preceeding.group_with preceeding
      in
      let fun_and_params =
        let params = flow_map (break 1) Function_param.pp params in
        let constr = Function_constraint.pp constr in
        fun_ ^/^
        pre_nest @@ nest 2 (group (params ^/^ group (constr ^?^ S.rarrow)))
      in
      let function_or_empty, cases_or_body = Function_body.pp_parts body in
      let fun_and_function =
        group fun_and_params ^?^
        pre_nest function_or_empty
      in
      group fun_and_function,
      pre_nest cases_or_body
    | _ -> assert false

  and pp_function ~preceeding exp =
    let indent =
      match exp.pexp_desc with
      | Pexp_function ([], _, _) -> (* no params means [function] kw *) 0
      | _ -> 2
    in
    let (fun_and_params, body) = pp_function_parts ?preceeding exp in
    group (fun_and_params ^/^ nest indent body)

  and pp_match ~preceeding ~ext_attrs ~tokens e cases =
    let match_, pre_nest =
      Ext_attribute.decorate S.match_ ext_attrs
      |> Preceeding.group_with preceeding
    in
    let cases =
      Case.pp_cases cases
        ~has_leading_pipe:(has_leading_pipe ~after:WITH tokens)
    in
    group (match_ ^/^ pre_nest (nest 2 (group (pp e)) ^/^ S.with_)) ^^
    hardline ^^
    pre_nest cases

  and pp_try ~preceeding ~ext_attrs ~tokens e cases =
    let try_, pre_nest =
      Ext_attribute.decorate S.try_ ext_attrs
      |> Preceeding.group_with preceeding
    in
    let cases =
      Case.pp_cases cases
        ~has_leading_pipe:(has_leading_pipe ~after:WITH tokens)
    in
    group (
      try_ ^/^
      pre_nest (nest 2 (pp e)) ^/^
      pre_nest S.with_
    ) ^^ hardline ^^
    pre_nest cases

  and pp_index_op ~preceeding nb_semis kind seq op indices assign =
    let open_, close =
      match kind with
      | Paren -> S.lparen, S.rparen
      | Brace -> S.lbrace, S.rbrace
      | Bracket -> S.lbracket, S.rbracket
    in
    let indices =
      let semi_as_term = List.compare_length_with indices nb_semis = 0 in
      if semi_as_term
      then separate_map (break 1) (fun i -> pp i ^^ S.semi) indices
      else separate_map (S.semi ^^ break 1) pp indices
    in
    let dotop =
      match op with
      | None -> S.dot
      | Some (None, op) -> stringf ".%s" op
      | Some (Some lid, op) -> S.dot ^^ longident lid ^^ stringf ".%s" op
    in
    let pre_nest = Preceeding.implied_nest preceeding in
    let access =
      group (
        pp ?preceeding seq ^^ break 0 ^^
        pre_nest (group dotop ^^ open_)
      ) ^^
      break 0 ^^
      pre_nest (nest 2 indices) ^^
      break 0 ^^
      pre_nest close
    in
    match assign with
    | None -> group access
    | Some e ->
      let preceeding = Preceeding.mk (S.larrow ^^ break 1) ~indent:3 in
      let assignment = pp ~preceeding e in
      group access ^^ group (break 1 ^^ pre_nest assignment)

  and pp_ifthenelse ~preceeding ?kw ext_attr e1 e2 e3_o =
    (* group the whole [if .. then .. (else if ..)* else? ..] *)
    group (pp_ite ?preceeding ?kw ext_attr e1 e2 e3_o)

  and pp_ite ?preceeding ?(kw=S.if_) ext_attr e1 e2 e3_o =
    let pre_kw, pre_nest =
      let decorated = Ext_attribute.decorate kw ext_attr in
      let pre_ext = group (decorated ^^ break 1) in
      let ext_indent = Requirement.to_int (requirement pre_ext) in
      Preceeding.extend preceeding pre_ext ~indent:ext_indent
    in
    let if_cond = pp ~preceeding:pre_kw e1 in
    let then_ = pp_if_branch S.then_ e2 in
    let else_ = optional pp_else_branch e3_o in
    group if_cond ^/^ pre_nest (then_ ^?^ else_)

  and pp_else_branch = function
    | { pexp_ext_attr = ext_attrs
      ; pexp_attributes = [] (* we'd need to be under parens to have attrs *)
      ; pexp_desc = Pexp_ifthenelse (e1, e2, e3)
      ; _ } ->
      pp_ite ~kw:(S.else_ ^/^ S.if_) ext_attrs e1 e2 e3
    | exp -> pp_if_branch S.else_ exp

  and pp_if_branch kw = function
    | { pexp_desc = Pexp_parens { exp = { pexp_desc = Pexp_tuple _; _ }; _ }
      ; _ } as exp ->
      (* we print parenthesized branches specially, but tuples do not count as
         parenthesized branches! *)
      kw ^^ nest 2 (group (break 1 ^^ pp exp))
    | { pexp_ext_attr = { pea_attrs = []; pea_ext = None }
      ; pexp_attributes = []
      ; pexp_desc = Pexp_parens { exp = e; optional = false }
      ; _ } ->
      group (kw ^/^ S.lparen) ^^ nest 2 (group (break 0 ^^ pp e ^^ S.rparen))
    | exp ->
      group (kw ^/^ nest 2 (pp exp))

  and pp_delimited_seq ~preceeding (opn, cls) nb_semis = function
    | [] ->
      let opn, pre_nest = Preceeding.group_with preceeding opn in
      group (opn ^^ pre_nest cls)
    | elts ->
      let semi_as_term = List.compare_length_with elts nb_semis = 0 in
      let pre_opn, pre_nest =
        Preceeding.extend preceeding (opn ^^ break 1) ~indent:2 in
      let semi_as_pre = Preceeding.mk ~indent:2 (S.semi ^^ break 1) in
      let opn_and_elts =
        foldli (fun i acc elt ->
          if i = 0 then
            pp ~preceeding:pre_opn elt
          else
            acc ^^ break 0 ^^ pre_nest @@ pp ~preceeding:semi_as_pre elt
        ) empty elts
      in
      group (
        opn_and_elts ^^
        pre_nest (
          (if semi_as_term then break 0 ^^ S.semi else empty) ^/^
          cls
        )
      )

  and pp_dot_open ~preceeding lid e =
    let lid, pre_nest = Preceeding.group_with preceeding (longident lid.txt) in
    group @@ match e with
    | { pexp_ext_attr = { pea_attrs = []; pea_ext = None }
      ; pexp_attributes = []
      ; pexp_desc = Pexp_parens { exp = e; optional = false }
      ; _ } ->
      lid ^^ S.dot ^^ S.lparen ^^ break 0 ^^
      pre_nest (nest 2 @@ pp e) ^^
      S.rparen
    | _ ->
      lid ^^ S.dot ^^ pre_nest (pp e)

  and pp_pack ~preceeding ~ext_attr me ty =
    let lparen_module, pre_nest =
      let module_ = Ext_attribute.decorate S.module_ ext_attr in
      Preceeding.group_with preceeding (group (S.lparen ^^ module_))
    in
    let me_parts = Module_expr.as_rhs me in
    let pack_parts =
      match ty with
      | None -> me_parts
      | Some c ->
        Layout_module_binding.map_rhs_end
          (fun t -> group (t ^/^ nest 2 S.colon) ^/^ nest 2 (Module_type.pp c))
          me_parts
    in
    group (
      match pack_parts with
      | Single_part me_and_ty ->
        lparen_module ^/^ pre_nest (nest 2 me_and_ty ^^ S.rparen)
      | Three_parts { start; main; stop } ->
        let flatness = flatness_tracker () in
        let single_line_start = Condition.flat flatness in
        let opt_nest = nest ~vanish:single_line_start 2 in
        group ~flatness (lparen_module ^/^ pre_nest @@ opt_nest start) ^/^
        pre_nest (opt_nest (nest 2 main ^/^ stop) ^^ S.rparen)
    )


  and pp_constraint ~preceeding e ct_opt modes =
    let pre_lparen, pre_nest =
      Preceeding.extend preceeding S.lparen ~indent:1
    in
    let colon_constr =
      optional (fun ct ->
        let preceeding = Preceeding.mk (S.colon ^^ break 1) ~indent:2 in
        Core_type.pp ~preceeding ct
      ) ct_opt
      |> with_modes ~modes
    in
    group (
      pp ~preceeding:pre_lparen e ^/^
      pre_nest (nest 1 colon_constr ^^ S.rparen)
    )

  and pp_coerce ~preceeding e ct1 ct2 =
    let pre_lparen, pre_nest =
      Preceeding.extend preceeding S.lparen ~indent:1
    in
    let lparen_e = pp ~preceeding:pre_lparen e in
    let colon_ct1 =
      optional (fun ct ->
        let preceeding = Preceeding.mk (S.colon ^^ break 1) ~indent:2 in
        Core_type.pp ~preceeding ct
      ) ct1
    in
    let coerce_ct2 =
      let preceeding = Preceeding.mk (S.coerce ^^ break 1) ~indent:3 in
      Core_type.pp ~preceeding ct2
    in
    group (
      lparen_e ^/^
      pre_nest (nest 1 (colon_ct1 ^?^ coerce_ct2) ^^ S.rparen)
    )

  and pp_override ~preceeding nb_semis fields =
    let semi_as_term = List.compare_length_with fields nb_semis = 0 in
    let field (lbl, eo) =
      string lbl.txt ^^
      begin match eo with
      | None -> empty
      | Some e -> break 1 ^^ S.equals ^/^ pp e
      end
      in
    let fields =
      if semi_as_term then
        separate_map (break 1) (fun elt -> field elt ^^ S.semi) fields
      else
        separate_map (S.semi ^^ break 1) field fields
    in
    let lbrace_lt, pre_nest = Preceeding.group_with preceeding S.lbrace_lt in
    group (
      lbrace_lt ^?^
      pre_nest (nest 2 fields ^?^ S.gt_rbrace)
    )

  and pp_array ~preceeding nb_semis mut elts =
    pp_delimited_seq ~preceeding (array_delimiters mut) nb_semis elts

  and pp_list ~preceeding nb_semis elts =
    pp_delimited_seq ~preceeding (S.lbracket, S.rbracket) nb_semis elts

  and pp_block_idx block_access unboxed_accesses =
    parens (
      Block_access.pp block_access ^^
      separate_map (break 0) (fun (Uaccess_unboxed_field lid) ->
        S.dothash ^^ longident lid.txt
      ) unboxed_accesses
    )

  and pp_tuple ?preceeding ?(comma_aligner=empty) ?pre_nest_override elts =
    let pre_nest =
      match pre_nest_override with
      | None -> Preceeding.implied_nest preceeding
      | Some nest -> nest
    in
    let comma =
      let comma = comma_aligner ^^ S.comma ^^ break 1 in
      let indent = if is_empty comma_aligner then 2 else 3 in
      Preceeding.mk comma ~indent
    in
    foldli (fun i acc exp ->
      if i = 0 then
        Argument.pp_preceeded ?preceeding pp exp
      else
        acc ^^ break 0 ^^
        pre_nest @@ Argument.pp_preceeded ~preceeding:comma pp exp
    ) empty elts

  and pp_parens_tuple ~preceeding ~attrs ~kind flatness elts =
    let lparen, comma_aligner, rparen =
      match kind with
      | `Optional ->
        let cond = Condition.flat flatness in
        opt_token cond "(" ~ws_after:(break 1),
        None,
        opt_token cond ~ws_before:(break 1) ")"
      | `Regular ->
        let pat_is_flat = Condition.flat flatness in
        let space_when_multiline =
          group (vanishing_whitespace pat_is_flat nbsp)
        in
        S.lparen ^^ space_when_multiline ^^ break 0,
        None,
        space_when_multiline ^^ break 0 ^^ S.rparen
      | `Unboxed ->
        let pat_is_flat = Condition.flat flatness in
        let space_when_multiline =
          group (vanishing_whitespace pat_is_flat nbsp)
        in
        S.hash_lparen ^^ space_when_multiline ^^ break 0,
        Some space_when_multiline,
        space_when_multiline ^^ break 0 ^^ S.rparen
    in
    let lparen, pre_nest =
      let indent = if kind = `Unboxed then 3 else 2 in
      Preceeding.extend preceeding lparen ~indent
    in
    let elts =
      pp_tuple ~preceeding:lparen ~pre_nest_override:pre_nest elts
        ?comma_aligner
    in
    (* FIXME: indent of attrs! *)
    Attribute.attach ~attrs elts ^^ pre_nest (group rparen)

  and pp_parens ~preceeding ~optional exp =
    let flatness = flatness_tracker () in
    group ~flatness @@
    match exp.pexp_desc with
    | Pexp_tuple elts ->
      let kind = if optional then `Optional else `Regular in
      pp_parens_tuple ~preceeding ~kind flatness ~attrs:exp.pexp_attributes elts
    | _ ->
      let before, after =
        (* No break? *)
        if optional
        then
          let cond = Condition.flat flatness in
          opt_token cond "(", opt_token cond ")"
        else S.lparen, S.rparen
      in
      let indent =
        (* Special case which reproduces inconsistencies in oxcamlformat's
           output. *)
        match exp.pexp_desc with
        | Pexp_function ([], _, _) -> 2
        | Pexp_function _ -> 0
        | _ -> 1
      in
      let before,pre_nest = Preceeding.extend preceeding before ~indent in
      pp ~preceeding:before exp ^^
      pre_nest after

  and pp_apply ~preceeding e args =
    let f = pp ?preceeding e in
    let pre_nest = Preceeding.implied_nest preceeding in
    Application.pp ~extra_nest:pre_nest f args

  and pp_op_apply ?preceeding ?(on_right=false) ?op ~op_prec arg =
    let default = function
      | None ->
        assert (not on_right);
        pp ?preceeding arg
      | Some op ->
        assert (Option.is_none preceeding);
        let op = pp_op op ^^ break 1 in
        let indent = Requirement.to_int (requirement op) in
        let op_pre = Preceeding.mk op ~indent in
        pp ~preceeding:op_pre arg
    in
    match arg.pexp_desc with
    | Pexp_apply (f, args) when on_right ->
      let op = Option.get op in
      begin match op.pexp_desc with
      | Pexp_ident { txt = { desc = Lident Op ":="; _ }; _ } ->
        (* ":=" should be laid out like "<-", not like a regular infix op. *)
        default (Some op)
      | _ ->
        (* N.B. the app is not under parentheses, that's why this is valid!
           (we are not dropping attributes or anything) *)
        let op = pp_op op ^^ break 1 in
        let indent = Requirement.to_int (requirement op) in
        let op_pre = Preceeding.mk op ~indent in
        let f = pp ~preceeding:op_pre f in
        Application.pp ~indent:(indent + 2) f args
      end
    | Pexp_function _ when on_right ->
      let op = pp_op (Option.get op) ^^ break 1 in
      let op_pre = Preceeding.mk op ~indent:0 (* ! *) in
      let kw_and_params, body = pp_function_parts ~preceeding:op_pre arg in
      kw_and_params ^/^ body
      |> Attribute.attach ~attrs:arg.pexp_attributes
    | Pexp_infix_apply { op = next_op; arg1; arg2 } ->
      let next_op_prec = Precedence.of_infix_op next_op in
      if op_prec <> next_op_prec then
        default op
      else (
        let pre_nest = Preceeding.implied_nest preceeding in
        pp_op_apply ?preceeding ?op ~op_prec arg1 ^/^
        pre_nest (
          pp_op_apply ~on_right:true ~op:next_op ~op_prec:next_op_prec arg2
          |> Attribute.attach ~attrs:arg.pexp_attributes
        )
      )
    | _ -> default op

  and pp_cons ?preceeding ?(on_right=false)lst =
    match lst.pexp_desc with
    | Pexp_cons (hd, tl) ->
      let pre_nest = Preceeding.implied_nest preceeding in
      pp_cons ?preceeding ~on_right hd ^/^
      pre_nest (
        pp_cons ~on_right:true tl
        |> Attribute.attach ~attrs:lst.pexp_attributes
      )
    | _ ->
      let preceeding =
        if on_right then (
          assert (Option.is_none preceeding);
          Some (Preceeding.mk (S.cons ^^ break 1) ~indent:3)
        ) else
          preceeding
      in
      pp ?preceeding lst

  and pp_variant ~preceeding lbl eo =
    let constr = S.bquote ^^ string lbl in
    let arg = optional pp eo in
    let constr, pre_nest = Preceeding.group_with preceeding constr in
    group (constr ^?^ pre_nest @@ nest 2 arg)

  and pp_record ~preceeding ?(unboxed = false) nb_semis expr_opt fields =
    let semi_as_term = List.compare_length_with fields nb_semis = 0 in
    let opening_tok, pre_nest =
      ((if unboxed then S.hash_lbrace else S.lbrace) ^^ break 1)
      |> Preceeding.extend preceeding ~indent:2
    in
    let first_part, nest_first_field, first_pre =
      match expr_opt with
      | None -> empty, Fun.id, Some opening_tok
      | Some e ->
        group (pp ~preceeding:opening_tok e ^/^ pre_nest @@ nest 2 S.with_),
        (fun f -> pre_nest @@ nest 2 f),
        None
    in
    let semi_as_pre = Preceeding.mk ~indent:2 (S.semi ^^ break 1) in
    let first_part_and_fields =
      foldli (fun i acc field ->
        if i = 0 then
          acc ^?^
          nest_first_field (Record_field.pp ?preceeding:first_pre pp field)
        else
          acc ^^ break 0 ^^
          pre_nest @@ Record_field.pp ~preceeding:semi_as_pre pp field
      ) first_part fields
    in
    group (
      first_part_and_fields ^/^
      pre_nest
        ((if semi_as_term then S.semi else empty) ^?^ S.rbrace)
    )

  and pp_field ?preceeding ?(unboxed=false) e lid =
    let dot = if unboxed then S.dothash else S.dot in
    let pre_nest = Preceeding.implied_nest preceeding in
    group (
      pp ?preceeding e ^^ dot ^^ pre_nest (longident lid.txt)
    )

  and pp_setfield ?preceeding e1 lid e2 =
    let pre_nest = Preceeding.implied_nest preceeding in
    let access = pp_field ?preceeding e1 lid in
    let assignment =
      let arrow_pre = Preceeding.mk (S.larrow ^^ break 1) ~indent:3 in
      pp ~preceeding:arrow_pre e2
    in
    group (access ^/^ pre_nest assignment)
end

and Record_field : sig
  val pp : ?preceeding:Preceeding.t -> ('a -> t) -> 'a record_field -> t
end = struct
  let pp ?preceeding pp_value rf =
    let pre_name, pre_nest =
      Preceeding.group_with preceeding
        (longident rf.field_name.txt)
    in
    let lhs =
      pre_name ^?^
      pre_nest @@ nest 2
        (optional (fun v -> group @@ Type_constraint.pp v) rf.typ)
    in
    group @@
    match rf.value with
    | None -> lhs
    | Some v ->
      group (lhs ^/^ S.equals) ^/^
      pre_nest @@ nest 2 (pp_value v)
end

and Block_access : sig
  val pp : block_access -> t
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
      | Index_unboxed_int8 -> string "s"
      | Index_unboxed_int16 -> string "S"
      | Index_unboxed_int32 -> string "l"
      | Index_unboxed_int64 -> string "L"
      | Index_unboxed_nativeint -> string "n"
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
  val pp
    :  ?extra_nest:(Document.t -> Document.t)
    -> ?indent:int
    -> t
    -> expression argument list
    -> t
end = struct
  let is_function p =
    match p.pexp_desc with
    | Pexp_parens
        { optional = false
        ; exp = { pexp_desc = Pexp_function _; _ }
        } ->
      true
    | _ -> false

  let pp_function_parts ?app_prefix_flatness ?lbl exp =
    match exp.pexp_desc with
    | Pexp_parens { exp = fun_exp; optional = false } ->
      let (fun_and_params, body) = Expression.pp_function_parts fun_exp in
      let first_part =
        optional (stringf "~%s:") lbl ^^ S.lparen ^^ fun_and_params
      in
      first_part,
      group (
        nest ?vanish:(Option.map Condition.flat app_prefix_flatness) 2
          (break 1 ^^ body)
        ^^ S.rparen
      )
      |> Attribute.attach ~attrs:exp.pexp_attributes
    | _ -> assert false

  let pp_arg_parts ?app_prefix_flatness arg =
    match arg.parg_desc with
    | Parg_unlabelled { legacy_modes=[]; arg; typ_constraint=None; modes=[] }
      when is_function arg ->
      pp_function_parts ?app_prefix_flatness arg
    | Parg_labelled { optional=false; legacy_modes=[]; name;
                      maybe_punned=Some arg; typ_constraint=None;modes=[];
                      default=None }
      when is_function arg ->
      pp_function_parts ?app_prefix_flatness ~lbl:name arg
    | _ ->
      Argument.pp Expression.pp arg, empty

  let pp_last_arg app_prefix_flatness arg =
    let p1, p2 = pp_arg_parts ~app_prefix_flatness arg in
    [ p1 ], p2

  let pp_arg arg =
    let p1, p2 = pp_arg_parts arg in
    p1 ^^ p2

  let rec pp_args app_prefix_flatness = function
    | [] -> assert false
    | [ arg ] -> pp_last_arg app_prefix_flatness arg
    | arg :: args ->
      let args, trailing = pp_args app_prefix_flatness args in
      pp_arg arg :: args, trailing

  let pp ?(extra_nest=Fun.id) ?(indent=2) f args =
    let app_prefix_flatness = flatness_tracker () in
    let args, trailing = pp_args app_prefix_flatness args in
    let args = separate (break 1) args in
    group ~flatness:app_prefix_flatness (
      f ^/^
      extra_nest @@ nest indent args
    ) ^^
    (* N.B. the 2 (instead of [indent]) below is to reproduce ocamlformat's
       behavior. But it feels *wrong* and should eventually be fixed? *)
    extra_nest @@ nest 2 trailing
end

and Case : sig
  val pp : case -> t

  val pp_cases : has_leading_pipe:bool -> case list -> t
end = struct
  let pp_guard = function
    | None -> empty
    | Some e ->
      let pre = S.when_ ^^ break 1 in
      let indent = Requirement.to_int (requirement pre) in
      let preceeding = Preceeding.mk pre ~indent in
      Expression.pp ~preceeding e

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
    let pipe_pre = Some (Preceeding.mk (S.pipe ^^ break 1) ~indent:2) in
    let first_pre = if pipe then pipe_pre else None in
    split_top_or p
    |> List.fold_left (fun (acc, preceeding) pat ->
      let p = Pattern.pp ?preceeding pat in
      acc ^?^ p, pipe_pre
    ) (empty, first_pre)
    |> fst

  let pp pipe { pc_lhs; pc_guard; pc_rhs } =
    let guarded_pat =
      prefix (group (pp_pattern pipe pc_lhs))
        (pp_guard pc_guard)
    in
    let body = Expression.pp pc_rhs in
    (* try to put things on the same line as what preceeds if they fit. *)
    flow (break 1) [ guarded_pat; nest 2 S.rarrow; nest 2 body ]
    |> group

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
  val pp : ?preceeding:Preceeding.t -> letop -> t
end = struct
  let pp ?preceeding { let_; ands; body } =
    let pre_nest = Preceeding.implied_nest preceeding in
    Binding_op.pp_list ?preceeding (let_ :: ands) ^^ hardline ^^
    pre_nest @@ Expression.pp body
end

and Binding_op : sig
  val pp_list : ?preceeding:Preceeding.t -> binding_op list -> t
end = struct
  let pp ?(add_in=false) ?preceeding { pbop_op; pbop_binding; pbop_loc = _ } =
    Value_binding.pp ~item:false ?preceeding ~start:[string pbop_op.txt]
      pbop_binding ~add_in

  let rec pp_list ?preceeding = function
    | [] -> empty
    | [ bop ] -> pp ?preceeding ~add_in:true bop
    | bop :: bops -> pp ?preceeding bop ^/^ pp_list bops
end

and Argument : sig
  val pp : ('a -> t) -> 'a argument -> t

  val pp_preceeded
    :  ?preceeding:Preceeding.t
    -> (?preceeding:Preceeding.t -> 'a -> t)
    -> 'a argument
    -> t
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

  let pp_generic ?preceeding ?default legacy_modes arg_doc typ_constraint
        at_modes =
    let typ_and_modes =
      optional Type_constraint.pp typ_constraint
      |> with_modes ~modes:at_modes
    in
    let pre_arg_ty_modes, pre_nest =
      Preceeding.group_with preceeding
        (modes_legacy legacy_modes ^?^ arg_doc ^?^ typ_and_modes)
    in
    pre_arg_ty_modes ^?^
    pre_nest @@ nest 2
      (optional (fun d -> S.equals ^/^ Expression.pp d) default)

  let pp pp_arg arg =
    let parenthesize doc =
      if had_parens arg
      then parens doc
      else doc
    in
    group @@ match arg.parg_desc with
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
      single_or_multi_token arg.parg_tokens ~optional name ^^ break 0 ^^
      nest 2 @@ parenthesize (
        pp_generic legacy_modes (pp_arg a) typ_constraint modes ?default
      )

  let pp_preceeded ?preceeding
        (pp_preceeded_arg : ?preceeding:Preceeding.t -> _) arg =
    let parenthesize fst_tok =
      let lparen, rparen, indent =
        if had_parens arg
        then S.lparen, break 0 ^^ S.rparen, 3
        else empty, empty, 2
      in
      let pre, pre_nest =
        Preceeding.extend preceeding (fst_tok ^^ break 0 ^^ lparen) ~indent
      in
      pre, pre_nest rparen
    in
    group @@ match arg.parg_desc with
    | Parg_unlabelled { legacy_modes=[]; arg; typ_constraint=None; modes=[] } ->
      pp_preceeded_arg ?preceeding arg
    | Parg_unlabelled { legacy_modes; arg; typ_constraint; modes } ->
      let lpre, pre_nest =
        Preceeding.extend preceeding S.lparen ~indent:1
      in
      pp_generic ~preceeding:lpre legacy_modes (pp_preceeded_arg arg)
        typ_constraint modes ^^
      pre_nest S.rparen
    | Parg_labelled {
        optional; legacy_modes; name: string; maybe_punned = None;
        typ_constraint; modes; default;
      } ->
      let pre, post = parenthesize (if optional then S.qmark else S.tilde) in
      pp_generic ~preceeding:pre legacy_modes (string name) typ_constraint modes
        ?default ^^
      post
    | Parg_labelled {
        optional; legacy_modes; name: string; maybe_punned = Some a;
        typ_constraint; modes; default;
      } ->
      let pre, post =
        parenthesize (single_or_multi_token arg.parg_tokens ~optional name)
      in
      pp_generic ~preceeding:pre legacy_modes (pp_preceeded_arg a)
        typ_constraint modes ?default ^^
      post
end

and Function_param : sig
  val pp : function_param -> t
  val pp_desc : function_param_desc -> t
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
  val pp_parts : ?preceeding:Preceeding.t -> function_body -> t * t
  (** [pp_parts (Pfunction_cases _)] is ["function", cases]
      [pp_parts (Pfunction_body e)] is [<empty>, e] *)

  val as_rhs : function_body -> Layout_module_binding.rhs
end = struct
  let pp_cases ?preceeding ~tokens cases ext_attrs =
    let pre_function_, pre_nest =
      Preceeding.group_with preceeding
        (Ext_attribute.decorate S.function_ ext_attrs)
    in
    pre_function_,
    pre_nest (
      softest_line ^^
      Case.pp_cases cases
        ~has_leading_pipe:(has_leading_pipe ~after:FUNCTION tokens)
    )

  let pp_parts ?preceeding fb =
    match fb.pfb_desc with
    | Pfunction_body e -> empty, Expression.pp ?preceeding e
    | Pfunction_cases (cases, ext_attrs) ->
      pp_cases ?preceeding ~tokens:fb.pfb_tokens cases ext_attrs

  let as_rhs fb =
    match fb.pfb_desc with
    | Pfunction_body _ -> assert false (* can't be on value binding rhs *)
    | Pfunction_cases (cases, ext_attrs) ->
      let kw, cases = pp_cases ~tokens:fb.pfb_tokens cases ext_attrs in
      Layout_module_binding.Three_parts { start = kw; main = cases; stop = empty}
end

and Type_constraint : sig
  val pp : type_constraint -> t
end = struct
  let pp = function
  | Pconstraint ct ->
    S.colon ^/^ Core_type.pp ct
  | Pcoerce (None, ct) -> S.coerce ^/^ Core_type.pp ct
  | Pcoerce (Some ct1, ct2) ->
    S.colon ^/^ Core_type.pp ct1 ^/^ S.coerce ^/^ Core_type.pp ct2
end


and Function_constraint : sig
  val pp : function_constraint -> t
end = struct
  let pp fc =
    optional Type_constraint.pp fc.ret_type_constraint
    |> with_modes ~modes:fc.ret_mode_annotations
end

and Comprehension : sig
  val pp_expr : ?preceeding:Preceeding.t -> comprehension_expression -> t
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
    Attribute.pp_list attrs ^?^
    optional (fun m -> mode_legacy m.txt ^^ break 1) m_opt ^^
    Pattern.pp p ^/^ pp_iterator it

  let pp_clause = function
    | Pcomp_for l ->
      group (
        S.for_ ^/^
        separate_map (break 1 ^^ S.and_ ^^ break 1)
          pp_clause_binding l
      )
    | Pcomp_when e ->
      group (S.when_ ^/^ Expression.pp e)

  let pp ?preceeding c =
    let pre_nest = Preceeding.implied_nest preceeding in
    Expression.pp ?preceeding c.pcomp_body ^/^
    pre_nest (separate_map (break 1) pp_clause c.pcomp_clauses)

  let pp_expr ?preceeding e =
    let left, right, c =
      match e with
      | Pcomp_list_comprehension c -> S.lbracket, S.rbracket, c
      | Pcomp_array_comprehension (mut, c) ->
        let left, right = array_delimiters mut in
        left, right, c
    in
    let left_pre, pre_nest =
      Preceeding.extend preceeding (left ^^ break 1) ~indent:2
    in
    pp ~preceeding:left_pre c ^/^ pre_nest right
end

(** {2 Value descriptions} *)

and Value_description : sig
  val pp : value_description -> t
end = struct
  let pp_type flatness ct =
    match Core_type.Arrow.components ct with
    | None ->
      let preceeding = Preceeding.mk (S.colon ^^ break 1) ~indent:2 in
      Core_type.pp ~preceeding ct
    | Some (poly_params, args, ret) ->
      Core_type.Arrow.pp_for_descr flatness poly_params args
        ~pp_rhs:(Core_type.Arrow.pp_ret_ty ret)
      |> Attribute.attach ~attrs:ct.ptyp_attributes

  let pp { pval_pre_doc; pval_ext_attrs; pval_name; pval_type; pval_modalities;
           pval_prim; pval_attributes; pval_post_doc; pval_loc = _ ;
           pval_tokens = _ ; } =
    let kw =
      match pval_prim with
      | [] -> S.val_
      | _ -> S.external_
    in
    let kw = Ext_attribute.decorate kw pval_ext_attrs in
    let flatness = flatness_tracker () in
    group (kw ^/^ str_or_op pval_name.txt) ^/^ nest 2 (
      with_modalities ~modalities:pval_modalities
        (pp_type flatness pval_type)
      ^?^
      begin match pval_prim with
        | [] -> empty
        | ps ->
          group (
            S.equals ^/^ separate_map (break 1) (fun s -> stringf "%S" s) ps
          )
      end
    )
    |> Attribute.attach ~item:true ~flatness
         ?pre_doc:pval_pre_doc ?post_doc:pval_post_doc ~attrs:pval_attributes
end

(** {2 Type declarations} *)

and Record : sig
  val pp_decl: ?unboxed:bool -> label_declaration list -> t
end = struct
  let pp_field ?preceeding
        { pld_name; pld_mutable; pld_global;
          pld_modalities; pld_type; pld_attributes;
          pld_doc; pld_loc = _; pld_tokens = _ } =
    let mut_or_glob =
      match pld_mutable, pld_global with
      | Immutable, false -> empty
      | Mutable, false -> S.mutable_
      | Immutable, true -> S.global__
      | Mutable, true -> assert false
    in
    let fld_name_colon, pre_nest =
      Preceeding.group_with preceeding
        (group @@ mut_or_glob ^?^ string pld_name.txt ^/^ S.colon)
    in
    let typ_and_modalities =
      Core_type.pp pld_type
      |> with_modalities ~modalities:pld_modalities
    in
    group (
      fld_name_colon ^/^
      pre_nest @@ nest 2 typ_and_modalities
    ) ^?^
    pre_nest @@ nest 3 (* oxcamlformat indents by 3 instead of 2 here, why? *) (
      group (
        group (Attribute.pp_list pld_attributes) ^?^
        optional Doc.pp pld_doc
      )
    )

  let pp_decl ?(unboxed=false) lbls =
    let maybe_trailing_semi, lbls =
      List.fold_left_map (fun preceding_tok lbl ->
        let next_tok =
          if List.exists (fun t -> t.Tokens.desc = Token SEMI) lbl.pld_tokens
          then S.semi
          else empty
        in
        let preceeding = Preceeding.mk (preceding_tok ^^ break 1) ~indent:2 in
        next_tok, group (pp_field ~preceeding lbl)
      ) (if unboxed then S.hash_lbrace else S.lbrace) lbls
    in
    separate hardline lbls ^?^ maybe_trailing_semi ^?^ S.rbrace
end

and Constructor_argument : sig
  val pp : constructor_argument -> t

  val pp_args : constructor_arguments -> t
end = struct
  let pp { pca_global; pca_modalities; pca_type; pca_loc = _ } =
    (if pca_global then S.global__ else empty) ^?^
    Core_type.pp pca_type
    |> with_modalities ~modalities:pca_modalities

  let pp_args = function
    | Pcstr_tuple args ->
      separate_map (break 1 ^^ S.star ^^ break 1) pp args
    | Pcstr_record lbls -> Record.pp_decl lbls
end

and Type_param : sig
  val pp : ?preceeding:Preceeding.t -> ptype_param -> t
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


  let pp ?preceeding { ptp_typ; ptp_infos; ptp_tokens } =
    match pp_infos ptp_tokens ptp_infos with
    | Empty -> Core_type.pp ?preceeding ptp_typ
    | infos ->
      let infos, pre_nest = Preceeding.group_with preceeding infos in
      infos ^^ pre_nest (Core_type.pp ptp_typ)
end

and Type_declaration : sig
  val pp_constraints : ptype_constraint list -> t

  val pp_list :
    ?subst:bool -> Asttypes.rec_flag -> type_declaration list -> t
end = struct

  let constructor_declaration td_flatness
      { pcd_name; pcd_vars; pcd_args; pcd_res; pcd_attributes;
        pcd_doc; pcd_loc = _; pcd_tokens } =
    let pcd_vars =
      match pcd_vars with
      | [] -> empty
      | vars -> Core_type.pp_poly_bindings vars ^^ S.dot
    in
    let name = group (constr_ident pcd_name.txt) in
    let constr =
      match pcd_args, pcd_res with
      | Pcstr_tuple [], None -> name
      | args, None ->
        group (name ^/^ nest 2 S.of_) ^/^ nest 2 @@ Constructor_argument.pp_args args
      | Pcstr_tuple [], Some ct ->
        group (name ^/^ nest 2 S.colon) ^?^ nest 2 (pcd_vars ^?^ Core_type.pp ct)
      | args, Some ct ->
        group (name ^/^ nest 2 S.colon) ^?^ nest 2 (
          pcd_vars ^?^
          Constructor_argument.pp_args args ^/^ S.rarrow ^/^ Core_type.pp ct
        )
    in
    let without_pipe =
      Attribute.attach ~attrs:pcd_attributes ?post_doc:pcd_doc
        (group constr)
      |> group
      |> nest 2
    in
    match List.find Tokens.is_token pcd_tokens with
    | { desc = Token BAR; _ } -> S.pipe ^^ nbsp ^^ without_pipe
    | { desc = Opt_token BAR; _ } ->
      S.optional_pipe (Condition.flat td_flatness) ^^ without_pipe
    | _ ->
      without_pipe

  let pp_variant td_flatness = function
    | [] -> S.pipe
    | cd :: cds ->
      let cd = constructor_declaration td_flatness cd in
      match cds with
      | [] -> cd
      | _ ->
        List.fold_left (fun doc cd ->
          doc ^^ softest_line ^^ constructor_declaration td_flatness cd
        ) cd cds

  let type_kind td_flatness = function
    | Ptype_abstract -> empty
    | Ptype_variant cds -> pp_variant td_flatness cds
    | Ptype_record lbls -> Record.pp_decl lbls
    | Ptype_record_unboxed_product lbls -> Record.pp_decl ~unboxed:true lbls
    | Ptype_open -> S.dotdot

  let pp_constraints =
    separate_map (break 1) (fun (ct1, ct2, _) ->
      S.constraint_ ^/^ Core_type.pp ct1 ^/^ S.equals ^/^
      Core_type.pp ct2
    )

  let pp_rhs ?(subst=false) td_flatness td =
    let eq = group (break 1 ^^ if subst then S.colon_equals else S.equals) in
    match td.ptype_manifest, td.ptype_kind with
    | None, Ptype_abstract -> empty
    | Some ct, Ptype_abstract ->
      eq ^?^ private_ td.ptype_private ^?^ Core_type.pp_for_decl ct
    | Some ct, kind ->
      eq ^/^ Core_type.pp_for_decl ct ^/^
      S.equals ^?^ private_ td.ptype_private ^?^ type_kind td_flatness kind
    | None, kind ->
      group (eq ^?^ private_ td.ptype_private) ^?^ type_kind td_flatness kind

  let pp ~start ?subst td =
    let lhs =
      let kws =
        match start with
        | [] -> assert false
        | first_kw :: other_kws ->
          Ext_attribute.decorate first_kw td.ptype_ext_attrs
          ^?^ separate (break 1) other_kws
      in
      let tconstr =
        Type_constructor.pp_decl td.ptype_tokens
          td.ptype_params (string td.ptype_name.txt)
      in
      let jkind =
        optional (fun j ->
          (* FIXME: pass colon as preceeding? *)
          group (S.colon ^/^ Jkind_annotation.pp j)
        ) td.ptype_jkind_annotation
      in
      group (kws ^/^ nest 2 tconstr) ^?^
      nest 2 jkind
    in
    let flatness = flatness_tracker () in
    prefix
      (group ~flatness (lhs ^^ nest 2 (group (pp_rhs ?subst flatness td))))
      (pp_constraints td.ptype_cstrs)
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
  val pp : type_extension -> t
end = struct
  let pp { ptyext_path; ptyext_params; ptyext_constructors; ptyext_private;
           ptyext_attributes; ptyext_ext_attrs; ptyext_pre_doc; ptyext_post_doc;
           ptyext_loc = _ ; ptyext_tokens }=
    Ext_attribute.decorate S.type_ ptyext_ext_attrs ^/^
    Type_constructor.pp_decl ptyext_tokens
      ptyext_params (longident ptyext_path.txt)  ^/^
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
  val pp : extension_constructor -> t
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
  val pp : type_exception -> t
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
  val pp : class_type -> t
end = struct
  let rec collect_arrow_args rev_args = function
    | { pcty_desc = Pcty_arrow (arg, res); _ } ->
      collect_arrow_args (arg :: rev_args) res
    | otherwise -> List.rev rev_args, otherwise

  let rec pp_desc ct =
    match ct.pcty_desc with
    | Pcty_constr (lid, args) -> Type_constructor.pp_class_constr args lid.txt
    | Pcty_signature cs -> pp_signature cs
    | Pcty_arrow (arrow_arg, rhs) ->
      let args, rhs = collect_arrow_args [arrow_arg] rhs in
      Core_type.Arrow.pp (flatness_tracker () (* never flat? *)) [] args
        ~pp_rhs:(fun ?preceeding () ->
          (* FIXME! *)
          Preceeding.group_with preceeding (pp rhs)
          |> fst)
    | Pcty_extension ext -> Extension.pp ext
    | Pcty_open (od, ct) ->
      S.let_ ^/^ Open_description.pp od ^/^ S.in_ ^/^ pp ct

  and pp_signature { pcsig_self; pcsig_fields } =
    let obj_with_self =
      S.object_ ^^
      match pcsig_self with
      | None -> empty
      | Some pcsig_self -> parens (Core_type.pp pcsig_self)
    in
    prefix obj_with_self
      (separate_map (break 1) pp_field pcsig_fields) ^/^
    S.end_

  and pp_field { pctf_pre_doc; pctf_desc; pctf_attributes; pctf_post_doc;
                 pctf_loc = _; pctf_tokens = _ } =
    group (pp_field_desc pctf_desc)
    |> Attribute.attach ~item:true ~attrs:pctf_attributes
      ?pre_doc:pctf_pre_doc ?post_doc:pctf_post_doc

  and pp_field_desc = function
    | Pctf_inherit ct -> S.inherit_ ^/^ pp ct
    | Pctf_val (lbl, mut, virt, ct) ->
      S.val_ ^/^ mutable_ mut ^?^ virtual_ virt ^?^
      string lbl.txt ^/^ S.colon ^/^ Core_type.pp ct
    | Pctf_method (lbl, priv, virt, ct) ->
      S.method_ ^/^ private_ priv ^?^ virtual_ virt ^?^
      string lbl.txt ^/^ S.colon ^/^ Core_type.pp ct
    | Pctf_constraint (ct1, ct2) ->
      S.constraint_ ^/^ Core_type.pp ct1 ^/^ S.equals ^/^ Core_type.pp ct2
    | Pctf_attribute attr -> Attribute.pp_floating attr
    | Pctf_extension ext -> Extension.pp ~floating:true ext
    | Pctf_docstring s -> Doc.pp_floating s

  and pp ({ pcty_desc=_; pcty_attributes; pcty_loc=_; pcty_tokens=_ } as ct) =
    pp_desc ct
    |> Attribute.attach ~attrs:pcty_attributes
end


and Class_infos : sig
  val pp_list :
    'a. ?equal_sign:t -> ('a -> t) -> keywords:t list ->
    'a class_infos list -> t
end = struct
  let pp ?equal_sign pp_expr ~keywords
      { pci_ext_attrs; pci_virt; pci_params; pci_name; pci_expr; pci_attributes;
        pci_value_params; pci_constraint; pci_loc = _; pci_tokens = _;
        pci_pre_text; pci_pre_doc; pci_post_doc } =
    let keywords =
      match keywords with
      | [ class_ ] -> Ext_attribute.decorate class_ pci_ext_attrs
      | [ class_; type_ ] ->
        class_  ^/^ Ext_attribute.decorate type_ pci_ext_attrs
      | _ -> assert false
    in
    let value_params = List.map (Argument.pp Pattern.pp) pci_value_params in
    let bound_class_thingy =
      Type_constructor.pp_class_info_decl pci_params pci_name.txt
    in
    let open Layout_module_binding in
    let mk_constraint ct = Single_part (S.colon ^/^ Class_type.pp ct) in
    pp ?equal_sign
      ~pre_text:pci_pre_text
      ?pre_doc:pci_pre_doc
      ~keyword:(keywords ^?^ virtual_ pci_virt)
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
  val pp_list : class_description list -> t
end = struct
  let pp_list =
    Class_infos.pp_list ~equal_sign:S.colon Class_type.pp ~keywords:[S.class_]
end

and Class_type_declaration : sig
  val pp_list : class_type_declaration list -> t
end = struct
  let pp_list = Class_infos.pp_list Class_type.pp ~keywords:[S.class_; S.type_]
end

(** {2 Value expressions for the class language} *)

and Class_expr : sig
  val pp : class_expr -> t
  val pp_structure
    : ?preceeding:Preceeding.t -> ext_attribute -> class_structure -> t
end = struct
  let rec pp_desc ext_attr d =
    let (!!) kw = Ext_attribute.decorate kw ext_attr in
    match d with
    | Pcl_constr (lid, args) -> Type_constructor.pp_class_constr args lid.txt
    | Pcl_structure cs -> pp_structure ext_attr cs
    | Pcl_fun (params, rhs) ->
      let params = flow_map (break 1) (Argument.pp Pattern.pp) params in
      prefix
        (prefix !!S.fun_ (group (params ^/^ S.rarrow)))
        (pp rhs)
    | Pcl_apply (ce, args) -> Application.pp (pp ce) args
    | Pcl_let (rf, vbs, body) ->
      (* FIXME: factorize with Pexp_let *)
      Value_binding.pp_list ~item:true ~add_in:true vbs ~start:(
        S.let_ ::
        match rf with
        | Nonrecursive -> []
        | Recursive -> [S.rec_]
      ) ^/^ pp body
    | Pcl_constraint (ce, ct) ->
      parens (pp ce ^/^ S.colon ^/^ Class_type.pp ct)
    | Pcl_extension ext -> Extension.pp ext
    | Pcl_open (od, ce) ->
      (* FIXME: factorize *)
      S.let_ ^/^ Open_description.pp od ^/^ S.in_ ^/^ pp ce
    | Pcl_parens ce -> parens (pp ce)

  and pp_structure ?preceeding ext_attr { pcstr_self; pcstr_fields } =
    let pre_obj, pre_nest =
      Ext_attribute.decorate S.object_ ext_attr
      |> Preceeding.group_with preceeding
    in
    let obj_with_self =
      pre_obj ^^
      pre_nest @@ match pcstr_self.ppat_desc with
      | Ppat_any -> empty
      | _ -> Pattern.pp pcstr_self
    in
    prefix obj_with_self
      (pre_nest @@ separate_map (hardline ^^ hardline) pp_field pcstr_fields)
    ^/^
    pre_nest S.end_

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
    | Pcf_docstring s -> Doc.pp_floating s

  and pp_value lbl mut = function
    | Cfk_virtual ct ->
      S.val_ ^/^ mutable_ mut ^?^ S.virtual_ ^/^ string lbl.txt ^/^
      S.colon ^/^ Core_type.pp ct
    | Cfk_concrete (over, vb) ->
      let start = [
        S.val_ ^^ override_ over;
        mutable_ mut
      ] in
      Value_binding.pp ~start vb ~add_in:false

  and pp_method lbl priv = function
    | Cfk_virtual ct ->
      S.method_ ^/^ private_ priv ^?^ S.virtual_ ^/^ string lbl.txt ^/^
      S.colon ^/^ Core_type.pp ct
    | Cfk_concrete (over, vb) ->
      let start = [
        S.method_ ^^ override_ over;
        private_ priv
      ] in
      Value_binding.pp ~start vb ~add_in:false

  and pp { pcl_ext_attrs; pcl_desc; pcl_attributes; pcl_loc = _ } =
    pp_desc pcl_ext_attrs pcl_desc
    |> Attribute.attach ~attrs:pcl_attributes
end


and Class_declaration : sig
  val pp_list : class_declaration list -> t
end = struct
  let pp_list = Class_infos.pp_list Class_expr.pp ~keywords:[S.class_]
end

(** {1 Module language} *)
(** {2 Type expressions for the module language} *)

and Module_type : sig
  val pp : module_type -> t

  val as_rhs : module_type -> Layout_module_binding.rhs
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
      let flatness = flatness_tracker () in
      let vspace = vanishing_whitespace (Condition.flat flatness) nbsp in
      let _last_and, cstrs =
        List.fold_left (fun (kw, acc) cstr ->
          let cstr = With_constraint.pp cstr in
          vspace ^^ S.and_, acc ^?^ group (kw ^/^ cstr)
        ) (S.with_, empty) cstrs
      in
      group ~flatness (pp mty ^?^ cstrs)
    | Pmty_typeof (attrs, me) ->
      let kws = S.module_ ^/^ S.type_ ^/^ S.of_ in
      Attribute.attach (group kws) ~attrs ^/^ Module_expr.pp me
    | Pmty_extension ext -> Extension.pp ext
    | Pmty_alias lid -> longident lid.txt
    | Pmty_strengthen (mty, lid) ->
      pp mty ^/^ S.with_ ^/^ longident lid.txt
    | Pmty_parens mty -> parens (pp mty)

  and pp { pmty_desc; pmty_attributes; pmty_loc = _; pmty_tokens = _ } =
    pp_desc pmty_desc
    |> Attribute.attach ~attrs:pmty_attributes

  let as_rhs ({ pmty_desc; pmty_attributes; pmty_loc=_; pmty_tokens=_ } as mty)
    : Layout_module_binding.rhs =
    match pmty_desc with
    | Pmty_signature sg ->
      let start, main, stop = Signature.pp_parts sg in
      let stop = Attribute.attach stop ~attrs:pmty_attributes in
      Three_parts { start; main; stop }
    | Pmty_typeof (attrs, me) ->
      let kws = S.module_ ^/^ S.type_ ^/^ S.of_ in
      let prefix = Attribute.attach ~attrs (group kws) in
      begin match Module_expr.as_rhs me with
      | Single_part t -> Single_part (prefix ^/^ nest 2 t)
      | Three_parts rhs -> Three_parts { rhs with start = prefix ^/^ rhs.start }
      end
    | _ -> Single_part (pp mty)
end

and Functor_parameter : sig
  val pp : functor_parameter -> t
  val pp_type : functor_parameter -> t
end = struct
  let pp_named name mty =
    match Module_type.as_rhs mty with
    | Single_part mty -> group (name ^/^ S.colon ^/^ mty)
    | Three_parts { start; main; stop } ->
      group (
        group (name ^/^ group (nest 2 S.colon ^/^ start)) ^/^
        nest 2 main ^/^
        stop
      )

  let pp = function
    | Unit -> empty
    | Named (lbl, mty, modes) ->
      let name =
        match lbl.txt with
        | None -> S.underscore
        | Some s -> string s
      in
      with_modes (pp_named name mty) ~modes
    | Unnamed _ -> assert false

  let pp fp = parens (pp fp)

  let pp_type = function
    | Unnamed (mty, modes) -> with_modes (Module_type.pp mty) ~modes
    | fp -> pp fp

end

and Signature : sig
  val pp : signature -> t
  val pp_parts : signature -> t * t * t

  val pp_interface : signature -> t
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
    | Psig_docstring s -> Doc.pp s
    | Psig_extension te -> Extension.pp_toplevel te
    | Psig_kind_abbrev (name, k) ->
      S.kind_abbrev__ ^/^ string name.txt ^/^ S.equals ^/^
        Jkind_annotation.pp k
      |> group

  let pp_item it = pp_item_desc it.psig_desc

  let pp_keeping_semi { psg_modalities=_; psg_items; psg_tokens; psg_loc=_ } =
    (* Modalities are currently not marked as "children" of the signature, so we
       need to remove the prefix of the tokens as they correspond to the
       modalities tokens *)
    let items_tokens =
      List.drop_while (fun tok ->
        match tok.Tokens.desc with
        | Token (ATAT | LIDENT _) (* modality tokens *)
        | Comment _ (* comments might appear before/inside the modalities *) ->
          true
        | _ -> false
      ) psg_tokens
    in
    Toplevel_items.Sig.pp_grouped_keeping_semi pp_item (psg_items, items_tokens)


  let pp_parts sg =
    group (with_modalities S.sig_ ~modalities:sg.psg_modalities),
    softest_line ^^ pp_keeping_semi sg,
    S.end_

  let pp sg =
    let sig_with_mods, items, end_ = pp_parts sg in
    prefix sig_with_mods items ^/^ end_


  let pp_interface sg =
    with_modalities empty ~modalities:sg.psg_modalities ^?^
    pp_keeping_semi sg
end

and Module_declaration : sig
  val pp : module_declaration -> t

  val pp_recmods : module_declaration list -> t
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
          |> Layout_module_binding.map_rhs_end (with_modes ~modes)
        in
        List.map Functor_parameter.pp params,
        S.colon,
        rhs
      | Without_params (mty, modalities) ->
        let rhs =
          Module_type.as_rhs mty
          |> Layout_module_binding.map_rhs_end (with_modalities ~modalities)
        in
        [], equal_sign mty, rhs
    in
    Layout_module_binding.pp ~item:true
      ~params_indent:4
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
  val pp : module_substitution -> t
end = struct
  let pp { pms_name; pms_manifest; pms_attributes; pms_ext_attrs;
           pms_pre_doc; pms_post_doc; pms_loc = _; pms_tokens = _ } =
    Layout_module_binding.pp ~item:true ~equal_sign:S.colon_equals
      ?pre_doc:pms_pre_doc
      ~keyword:(Ext_attribute.decorate S.module_ pms_ext_attrs)
      (string pms_name.txt)
      ~rhs:(Layout_module_binding.Single_part (longident pms_manifest.txt))
      ~attrs:pms_attributes
      ?post_doc:pms_post_doc
end

and Module_type_declaration : sig
  val pp : ?subst:bool -> module_type_declaration -> t
end = struct
  let pp ?(subst=false)
      { pmtd_name; pmtd_type; pmtd_attributes; pmtd_ext_attrs; pmtd_pre_doc;
        pmtd_post_doc; pmtd_loc = _; pmtd_tokens = _ } =
    Layout_module_binding.pp ~item:true
      ?pre_doc:pmtd_pre_doc
      ~keyword:(S.module_ ^/^ Ext_attribute.decorate S.type_ pmtd_ext_attrs)
      (string pmtd_name.txt)
      ~equal_sign:(if subst then S.colon_equals else S.equals)
      ?rhs:(Option.map Module_type.as_rhs pmtd_type)
      ~attrs:pmtd_attributes
      ?post_doc:pmtd_post_doc
end

and Open_infos : sig
  val pp : 'a. ?item:bool -> ('a -> t) -> 'a open_infos -> t
end = struct
  let pp ?(item=true) pp_expr
      { popen_expr; popen_override; popen_attributes; popen_ext_attrs;
        popen_pre_doc; popen_post_doc; popen_loc = _; popen_tokens = _ } =
    Ext_attribute.decorate_optional_override S.open_ popen_override
      popen_ext_attrs ^/^
    pp_expr popen_expr
    |> Attribute.attach ~item ~attrs:popen_attributes
      ?pre_doc:popen_pre_doc ?post_doc:popen_post_doc
    |> group
end

and Open_description : sig
  val pp : open_description -> t
end = struct
  let pp = Open_infos.pp (fun lid -> longident lid.txt)
end

and Open_declaration : sig
  val pp : ?item:bool -> open_declaration -> t
end = struct
  let pp ?item = Open_infos.pp ?item Module_expr.pp
end

and Include_infos : sig
  val pp : 'a. ('a -> Layout_module_binding.rhs) -> 'a include_infos -> t
end = struct
  let pp pp_mod { pincl_kind ; pincl_mod; pincl_attributes; pincl_ext_attrs;
                  pincl_pre_doc; pincl_post_doc; pincl_loc = _;
                  pincl_tokens = _ } =
    let keywords =
      Ext_attribute.decorate S.include_ pincl_ext_attrs ^?^
      match pincl_kind with
      | Functor -> S.functor_
      | Structure -> empty
    in
    let include_ =
      match pp_mod pincl_mod with
      | Layout_module_binding.Single_part doc -> group (keywords ^/^ nest 2 doc)
      | Three_parts { start; main; stop } ->
        group (keywords ^/^ start) ^/^
        nest 2 main ^/^
        stop
    in
    Attribute.attach ~item:true ~attrs:pincl_attributes
      ?pre_doc:pincl_pre_doc ?post_doc:pincl_post_doc
      include_
end

and Include_description : sig
  val pp : include_description -> t
end = struct
  let pp = Include_infos.pp Module_type.as_rhs
end

and Include_declaration : sig
  val pp : include_declaration -> t
end = struct
  let pp = Include_infos.pp Module_expr.as_rhs
end

and With_constraint : sig
  val pp : with_constraint -> t
end = struct
  let pp wc =
    group @@
    match wc.wc_desc with
    | Pwith_type (params, lid, priv, ct, cstrs) ->
      S.type_ ^/^
      Type_constructor.pp_decl wc.wc_tokens params
        (longident lid.txt) ^/^
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
      Type_constructor.pp_decl wc.wc_tokens params
        (longident lid.txt) ^/^
      S.colon_equals ^/^ Core_type.pp ct
    | Pwith_modsubst (lid1, lid2) ->
      S.module_ ^/^ longident lid1.txt ^/^ S.colon_equals ^/^
      longident lid2.txt
end

(** {2 Value expressions for the module language} *)

and Module_expr : sig
  val pp : module_expr -> t
  val as_rhs : module_expr -> Layout_module_binding.rhs
end = struct
  let rec pp { pmod_desc; pmod_attributes; pmod_loc = _; pmod_tokens = _ } =
    pp_desc pmod_desc
    |> Attribute.attach ~attrs:pmod_attributes

  and pp_desc = function
    | Pmod_ident lid -> longident lid.txt
    | Pmod_structure (attrs, str) -> Structure.pp ~attrs str
    | Pmod_functor (attrs, fps, me) ->
      Attribute.attach ~attrs S.functor_ ^/^
      separate_map (break 1) Functor_parameter.pp fps ^/^ S.rarrow ^/^ pp me
    | Pmod_apply (m1, m2) -> pp_apply m1 m2
    | Pmod_apply_unit me -> pp me ^^ S.lparen ^^ S.rparen
    | Pmod_constraint (me, mty_opt, modes) ->
      let me = pp me in
      let with_mty =
        match mty_opt with
        | None -> me
        | Some mty -> me ^^ group (break 1 ^^ S.colon) ^/^ Module_type.pp mty
      in
      parens (with_modes ~modes with_mty)
    | Pmod_unpack (e, ty1, ty2) ->
      parens (
        S.val_ ^/^ Expression.pp e ^?^
        optional (fun c -> S.colon ^/^ Module_type.pp c) ty1 ^?^
        optional (fun c -> S.coerce ^/^ Module_type.pp c) ty2
      )
    | Pmod_extension ext -> Extension.pp ext
    | Pmod_parens me -> parens (pp me)

  and pp_apply m1 m2 =
    let m1 = pp m1 in
    match m2 with
    | { pmod_attributes = []
      ; pmod_desc =
          Pmod_parens
            { pmod_attributes = attrs2
            ; pmod_desc = Pmod_structure (attrs, str)
            ; _ }
      ; _ } ->
      let start, main, stop = Structure.pp_parts attrs str in
      let start = group (m1 ^/^ S.lparen ^^ start) in
      let main = nest 2 main in
      let stop =
        let internal_stop = Attribute.attach stop ~attrs:attrs2 in
        internal_stop ^^ S.rparen
      in
      start ^/^ main ^/^ stop
    | _ -> m1 ^/^ nest 2 (pp m2)

  let as_rhs
        ({ pmod_desc; pmod_attributes; pmod_loc = _; pmod_tokens = _ } as me)
    : Layout_module_binding.rhs =
    match pmod_desc with
    | Pmod_structure (attrs, str) ->
      let start, main, stop = Structure.pp_parts attrs str in
      let stop = Attribute.attach stop ~attrs:pmod_attributes in
      Three_parts { start; main; stop }
    | Pmod_apply
        (m1, { pmod_attributes = []
             ; pmod_desc =
                 Pmod_parens
                   { pmod_attributes = attrs2
                   ; pmod_desc = Pmod_structure (attrs, str)
                   ; _ }
             ; _ }) ->
      let start, main, stop = Structure.pp_parts attrs str in
      let start = group (pp m1 ^/^ S.lparen ^^ start) in
      let main = nest 2 main in
      let stop =
        let internal_stop = Attribute.attach stop ~attrs:attrs2 in
        Attribute.attach (internal_stop ^^ S.rparen) ~attrs:pmod_attributes
        |> nest 2
      in
      Three_parts { start; main; stop }
    | _ -> Single_part (pp me)
end

and Structure : sig
  val pp : ?attrs:attributes -> structure -> t
  val pp_parts : attributes -> structure -> t * t * t

  val pp_implementation : structure -> t
end = struct
  let pp_item item =
    match item.pstr_desc with
    | Pstr_eval (e, attrs) ->
      Attribute.attach ~item:true ~attrs (Expression.pp e)
    | Pstr_value (rf, vbs) ->
      (* FIXME: factorize Pexp_let *)
      Value_binding.pp_list ~item:true ~add_in:false vbs ~start:(
        S.let_ ::
        match rf with
        | Nonrecursive -> []
        | Recursive -> [S.rec_]
      )
    | Pstr_primitive vd -> Value_description.pp vd
    | Pstr_type (rf, tds) -> Type_declaration.pp_list rf tds
    | Pstr_typext te -> Type_extension.pp te
    | Pstr_exception exn -> Type_exception.pp exn
    | Pstr_module mb -> Module_binding.pp ~item:true ~keywords:[S.module_] mb
    | Pstr_recmodule mbs -> Module_binding.pp_recmods mbs
    | Pstr_modtype mty -> Module_type_declaration.pp mty
    | Pstr_open od -> Open_declaration.pp od
    | Pstr_class cds -> Class_declaration.pp_list cds
    | Pstr_class_type ctds -> Class_type_declaration.pp_list ctds
    | Pstr_include incl -> Include_declaration.pp incl
    | Pstr_attribute a -> Attribute.pp_floating a
    | Pstr_docstring s -> Doc.pp s
    | Pstr_extension te ->
      Extension.pp_toplevel te
      |> group
    | Pstr_kind_abbrev (name, k) ->
      S.kind_abbrev__ ^/^ string name.txt ^/^ S.equals ^/^
      Jkind_annotation.pp k
      |> group

  let pp_keeping_semi = Toplevel_items.Struct.pp_grouped_keeping_semi pp_item

  let pp ?(attrs=[]) str =
    let struct_ = Attribute.attach ~attrs S.struct_ in
    group (
      struct_ ^?^
      nest 2 (pp_keeping_semi str) ^?^
      S.end_
    )

  let pp_parts attrs str =
    let str_doc =
      match pp_keeping_semi str with
      | Empty -> empty
      | doc -> softest_line ^^ group doc
    in
    Attribute.attach ~attrs S.struct_, str_doc, S.end_

  let pp_implementation str =
    group (pp_keeping_semi str)
end

and Value_constraint : sig
  val pp : value_constraint -> t
end = struct
  let pp = function
    | Pvc_constraint { locally_abstract_univars = []; typ } ->
      S.colon ^/^ nest 2 @@ Core_type.pp typ
    | Pvc_constraint { locally_abstract_univars = vars; typ } ->
      let pp_var (name, jkind) =
        match jkind with
        | None -> string name.txt
        | Some j ->
          parens (string name.txt ^/^ S.colon ^/^ Jkind_annotation.pp j)
      in
      group (
        S.colon ^/^ nest 2 (
          group (S.type_ ^/^ separate_map (break 1) pp_var vars ^^ S.dot)
        )
      ) ^/^
      nest 2 (Core_type.pp typ)
    | Pvc_coercion {ground; coercion} ->
      begin match ground with
        | None -> empty
        | Some ct -> S.colon ^/^ Core_type.pp ct ^^ break 1
      end ^^
      S.coerce ^/^ Core_type.pp coercion

  let pp x = group (pp x)
end

and Layout_module_binding : sig
  type rhs =
    | Single_part of t
    | Three_parts of { start: t; main: t; stop: t }
    (** Meant for [struct/sig .. end]: we try to keep [start] on the same line
        as what preceeds it, [main] is indented, [stop] is not.

        N.B. also used for value bindings in cases such as
        [let f = function ...], there [start = function] and [stop] is empty. *)

  val map_rhs_end : (t -> t) -> rhs -> rhs
    (** either map the sole part, or the stop part *)

  val pp
    :  ?preceeding:Preceeding.t
    -> ?params_indent:int
    -> ?item:bool
    -> ?equal_sign:t
    -> ?pre_text:string list
    -> ?pre_doc:string
    -> keyword:t
    -> ?params:t list
    -> ?constraint_:rhs
    -> ?rhs:rhs
    -> ?attrs:attributes
    -> ?post_doc:string
    -> t
    -> t
end = struct
  type rhs =
    | Single_part of t
    | Three_parts of { start: t; main: t; stop: t }

  let map_rhs_end f = function
    | Single_part d -> Single_part (f d)
    | Three_parts r -> Three_parts { r with stop = f r.stop }


  (* Beware: [equal_sign] is never part of [rhs], we have to insert it
     explicitely, whereas [constraint] sometimes include the "delimiter": a colon
     or an at.
     We could have had a [colon_sign] for symmetry, but pffff...

     Also: "sometimes" == it does include it when it is [Single_part], but
     doesn't for [Three_parts] as there it is always a colon, so we do it here.

     The whole thing is kinda ugly. *)
  let pp ?preceeding ?(params_indent=2)
        ~equal_sign ~keyword ~params ?constraint_ ?rhs bound =
    let keyword, pre_nest =
      Preceeding.group_with preceeding (group keyword)
    in
    let bindings =
      let main = group (keyword ^/^ pre_nest @@ nest 2 bound) in
      match params with
      | [] -> main
      | _ ->
        group (
          main ^/^
          separate_map (break 1)
            (fun p -> pre_nest @@ nest params_indent p) params
        )
    in
    (* FIXME: the ':' and '=' (and friends) should be attached to what preceeds
       them if we want to match oxcamlformat's janestreet style.
       This is currently not doable as the colon is part of the constraint when
       the constraint is Single_part. *)
    match constraint_, rhs with
    | None, None ->
      (* abstract module types *)
      bindings
    | None, Some Single_part doc ->
      group (
        group (bindings ^/^ pre_nest @@ nest 2 equal_sign) ^/^
        pre_nest @@ nest 2 doc
      )
    | Some Single_part doc, None ->
      group (bindings ^/^ pre_nest @@ nest 2 doc)
    | Some Single_part typ, Some Single_part exp ->
      group (
        group (bindings ^/^ pre_nest @@ nest 2 (group (typ ^/^ equal_sign)))
      ) ^/^
      pre_nest @@ nest 2 exp
    | None, Some Three_parts { start; main; stop } ->
      let bindings_and_main =
        group
          (bindings ^/^ pre_nest @@ group (nest 2 equal_sign ^/^ start)) ^?^
        pre_nest @@ nest 2 main
      in
      group (
        group bindings_and_main ^?^
        pre_nest stop
      )
    | Some Three_parts { start; main; stop }, None ->
      let bindings_and_main =
        group
          (bindings ^/^ pre_nest @@ group (nest 2 S.colon ^/^ start)) ^/^
        pre_nest @@ nest 2 main
      in
      group (
        group bindings_and_main ^/^
        pre_nest stop
      )
    | Some Three_parts { start; main; stop }, Some Single_part doc ->
      let bindings_and_main =
        group
          (bindings ^/^ pre_nest @@ group (nest 2 S.colon ^/^ start)) ^/^
        pre_nest @@ nest 2 main
      in
      let stop_and_rhs = group (stop ^/^ nest 2 equal_sign) ^/^ nest 2 doc in
      group (
        group bindings_and_main ^/^
        pre_nest (group stop_and_rhs)
      )
    | Some Single_part doc, Some Three_parts { start; main; stop } ->
      let bindings_cstr_main =
        group (
          bindings ^/^
          pre_nest @@ group (nest 2 (group (doc ^/^ equal_sign)) ^/^ start)
        ) ^/^
        pre_nest @@ nest 2 main
      in
      group (
        group bindings_cstr_main ^/^
        pre_nest stop
      )
    | Some Three_parts typ, Some Three_parts exp ->
      let bindings_and_typ =
        group (
          bindings ^/^
          pre_nest @@ nest 2 (group (S.colon ^/^ typ.start))
        ) ^/^
        pre_nest @@ nest 2 typ.main
      in
      let eq_and_exp =
        group (typ.stop ^/^ equal_sign ^/^ exp.start) ^/^
        nest 2 exp.main ^?^
        exp.stop
      in
      group (
        group bindings_and_typ ^/^
        pre_nest eq_and_exp
      )


  let pp ?preceeding ?params_indent ?item ?(equal_sign = S.equals) ?pre_text ?pre_doc ~keyword
        ?(params=[]) ?constraint_ ?rhs ?(attrs=[]) ?post_doc bound =
    (* Here we assume that [preceeding] cannot be [Some _] at the same time as
       [pre_text] or [pre_doc]. *)
    pp ?preceeding ?params_indent ~equal_sign ~keyword ~params ?constraint_ ?rhs bound
    |> Attribute.attach ?item ?text:pre_text ?pre_doc ?post_doc ~attrs
end

and Value_binding : sig
  val pp_list
    :  ?preceeding:Preceeding.t
    -> ?item:bool
    -> add_in:bool
    -> start:t list
    -> value_binding list -> t

  val pp
    : ?preceeding:Preceeding.t
    -> ?item:bool
    -> add_in:bool
    -> start:t list -> value_binding -> t
end = struct

  let layout ?preceeding ~keyword ~params ?constraint_ ?rhs bound in_kw =
    let keyword, pre_nest =
      Preceeding.group_with preceeding (group keyword)
    in
    let bindings =
      let main = keyword ^^ group (break 1 ^^ pre_nest @@ nest 2 bound) in
      match params with
      | [] -> main
      | _ ->
        group (
          main ^/^
          separate_map (break 1)
            (fun p -> pre_nest @@ nest 2 p) params
        )
    in
    match constraint_, rhs with
    (* let-punned *)
    | None, None -> bindings ^?^ pre_nest in_kw
    | Some doc, None -> group (bindings ^/^ pre_nest (nest 2 doc ^?^ in_kw))
    (* constraint-less *)
    | None, Some Layout_module_binding.Single_part doc ->
      group (
        group (bindings ^/^ pre_nest @@ nest 2 S.equals) ^/^
        pre_nest (nest 2 doc) ^?^
        pre_nest in_kw
      )
    | None, Some Three_parts { start; main; stop } ->
      let bindings_and_main =
        group
          (bindings ^/^ pre_nest @@ nest 2 (group (S.equals ^/^ start))) ^/^
        pre_nest @@ nest 2 main
      in
      group (
        group bindings_and_main ^/^
        pre_nest (stop ^?^ in_kw)
      )
    (* constraint and body *)
    | Some typ, Some Single_part exp ->
      group (
        group (
          group (bindings ^/^ pre_nest @@ nest 2 typ) ^/^
          pre_nest @@ nest 2 S.equals
        ) ^/^
        pre_nest (nest 2 exp) ^?^
        pre_nest in_kw
      )
    | Some doc, Some Three_parts { start; main; stop } ->
      let bindings_cstr_main =
        group (
          group (bindings ^/^ pre_nest @@ nest 2 doc) ^/^
          pre_nest (nest 2 @@ group (S.equals ^/^ start))
        ) ^/^
        pre_nest @@ nest 2 main
      in
      group (
        group bindings_cstr_main ^/^
        pre_nest stop ^?^
        pre_nest in_kw
      )

  let rhs e =
    match e.pexp_desc with
    | Pexp_function ([], _, body) ->
      (* N.B. if there were attributes we'd see [Pexp_parens] *)
      Function_body.as_rhs body
    | Pexp_function _ ->
      (* Specialized version of [Expression.pp_function]: we don't indent *)
      let (fun_and_params, body) = Expression.pp_function_parts e in
      let fun_ = group (fun_and_params ^/^ body) in
      (* N.B. there can be attributes here in unexpected cases, for instance:
         [fun _ -> x; [@attr]] the semicolon make the attribute attach to the
         function, not the body. *)
      Single_part (Attribute.attach fun_ ~attrs:e.pexp_attributes)
    | _ -> Single_part (Expression.pp e)

  let pp ?preceeding ?(item=false) ~add_in ~start
        { pvb_legacy_modes; pvb_pat; pvb_modes; pvb_params; pvb_constraint;
          pvb_ret_modes; pvb_expr; pvb_attributes; pvb_pre_text; pvb_pre_doc;
          pvb_post_doc; pvb_loc = _; pvb_tokens = _; pvb_ext_attrs } =
    let start =
      match start with
      | [] -> assert false
      | first_kw :: other_kws ->
        let decorated =
          Ext_attribute.decorate_value_binding first_kw pvb_ext_attrs
        in
        flow (break 1) (decorated :: List.map (nest 2) other_kws)
    in
    let kw_and_modes = start ^?^ modes_legacy pvb_legacy_modes in
    let pat = Pattern.pp pvb_pat in
    let pat =
      match pvb_modes with
      | [] -> pat
      | modes -> parens (with_modes pat ~modes)
    in
    let params = List.map Function_param.pp pvb_params in
    let constraint_ =
      match pvb_constraint, pvb_ret_modes with
      | None, [] -> None
      | None, lst -> Some (S.at ^/^ modes lst)
      | Some vc, lst ->
        Some (with_modes (Value_constraint.pp vc) ~modes:lst)
    in
    let attrs, in_kw =
      if not add_in
      then pvb_attributes, empty
      else
        (* The attrs go before [in].
           In this case there are also no docs, so the later call to attach is a
           noop.
           But I'd rather leave it than have two separate code paths depending
           on whether we're adding [in] or not. *)
        [], Attribute.pp_list ~item pvb_attributes ^?^ S.in_
    in
    layout ?preceeding
      ~keyword:kw_and_modes
      pat ~params ?constraint_
      ?rhs:(Option.map rhs pvb_expr)
      in_kw
    |> Attribute.attach ~item ~text:pvb_pre_text ?pre_doc:pvb_pre_doc
      ~attrs ?post_doc:pvb_post_doc

  let pp_ands ?(extra_nest=Fun.id) ~add_in ~item ~sep bindings =
    let rec aux = function
      | [] -> assert false
      | [ it ] -> extra_nest @@ group (pp ~item ~add_in ~start:[S.and_] it)
      | it :: its ->
        extra_nest (group (pp ~item ~add_in:false ~start:[S.and_] it))
        ^^ sep ^^ aux its
    in
    aux bindings

  let pp_list ?preceeding ?(item=false) ~add_in ~start = function
    | [] -> empty
    | [ x ] -> pp ?preceeding ~item ~add_in ~start x
    | x :: xs ->
      let sep = if item then hardline ^^ hardline else softest_line in
      let extra_nest =
        Option.map (fun _ -> Preceeding.implied_nest preceeding) preceeding
      in
      group (pp ?preceeding ~add_in:false ~item ~start x) ^^
      sep ^^
      pp_ands ?extra_nest ~add_in ~item ~sep xs
end

and Module_binding : sig
  val pp
    :  ?item:bool
    -> ?preceeding:Preceeding.t
    -> keywords:t list
    -> module_binding
    -> t

  val pp_recmods : module_binding list -> t
end = struct
  let modal_constraint constr mode_l : Layout_module_binding.rhs option =
    match Option.map Module_type.as_rhs constr, mode_l with
    | None, [] -> None
    | Some Three_parts { start; main; stop }, modes ->
      Some (Three_parts { start; main; stop = with_modes stop ~modes })
    | Some Single_part mty, modes ->
      Some (Single_part (
        S.colon ^/^ mty
        |> with_modes ~modes
      ))
    | None, at_modes -> Some (Single_part (S.at ^/^ modes at_modes))

  let pp ?item ?preceeding ~keywords
      { pmb_ext_attrs; pmb_name = (name, name_modes); pmb_params;
        pmb_constraint; pmb_modes; pmb_expr; pmb_attributes; pmb_pre_text;
        pmb_pre_doc; pmb_post_doc; pmb_loc = _; pmb_tokens = _ } =
    let kw =
      separate_map (break 1) (fun kw ->
        if kw = S.module_ || kw = S.and_
        then Ext_attribute.decorate kw pmb_ext_attrs
        else kw
      ) keywords
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
    Layout_module_binding.pp ?preceeding ~pre_text:pmb_pre_text ?pre_doc:pmb_pre_doc
      ~keyword:kw bound
      ~params_indent:4 ~params
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
  val pp : jkind_annotation -> t
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

  let pp jk = group (jkind_annotation_desc jk.pjkind_desc)
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
