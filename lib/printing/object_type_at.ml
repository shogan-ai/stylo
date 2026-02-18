open Ocaml_syntax
open Parsetree

module End = struct
  (* We need to be careful in payload and class path contructors to add an extra space
     before the closing ']' if the payload ends with and object type, otherwise the lexer
     will parse '>]' as a single token.

     While oxcaml's quotations also end with a '>' we do need to check for
     them as ']>' will have be recognized as a token already and the following
     ']' will be lexed independently. *)

  let rec of_core_type ct =
    ct.ptyp_attributes = []
    &&
    match ct.ptyp_desc with
    | Ptyp_object (_, _) -> true
    | Ptyp_arrow { codom_type = rhs; codom_modes = No_modes; _ }
    | Ptyp_poly (_, rhs)
      -> of_core_type rhs
    | Ptyp_tuple lst -> of_core_type (snd (List.hd (List.rev lst)))
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
    | Ptyp_quote _
    | Ptyp_splice _ -> false
  ;;

  let rec of_jkind_annotation jk =
    match jk.pjkind_desc with
    | Pjk_with (_, ct, No_modalities) | Pjk_kind_of ct -> of_core_type ct
    | Pjk_product (_ :: _ as jks) -> of_jkind_annotation List.(hd @@ rev jks)
    | Pjk_default
    | Pjk_abbreviation _
    | Pjk_mod _
    | Pjk_with (_, _, Modalities _)
    | Pjk_product []
    | Pjk_parens _ -> false
  ;;

  let of_constructor_argument ca =
    ca.pca_modalities = No_modalities && of_core_type ca.pca_type
  ;;

  let of_constructor_decl cd =
    cd.pcd_attributes = []
    &&
    match cd with
    | { pcd_res = Some ct; _ } -> of_core_type ct
    | { pcd_args = Pcstr_tuple (_ :: _ as args); _ } ->
      of_constructor_argument (List.hd @@ List.rev args)
    | _ -> false
  ;;

  let of_type_declaration td =
    td.ptype_attributes = []
    &&
    match td with
    | { ptype_cstrs = _ :: _ as cstrs; _ } ->
      let _, ct, _ = List.hd (List.rev cstrs) in
      of_core_type ct
    | { ptype_manifest = Some ct; ptype_kind = Ptype_abstract; _ } ->
      of_core_type ct
    | { ptype_kind = Ptype_variant (_ :: _ as cds); _ } ->
      of_constructor_decl List.(hd (rev cds))
    | _ -> false
  ;;

  let rec of_type_declarations = function
    | [] -> false
    | [ td ] -> of_type_declaration td
    | _ :: tds -> of_type_declarations tds
  ;;

  let of_extension_constructor ec =
    ec.pext_attributes = []
    &&
    match ec.pext_kind with
    | Pext_decl (_, _, Some ret_ct) -> of_core_type ret_ct
    | Pext_decl (_, Pcstr_tuple (_ :: _ as args), None) ->
      of_constructor_argument List.(hd (rev args))
    | _ -> false
  ;;

  let of_type_extension te =
    te.ptyext_attributes = []
    && of_extension_constructor List.(hd (rev te.ptyext_constructors))
  ;;

  let of_type_exception exn =
    exn.ptyexn_attributes = []
    && of_extension_constructor exn.ptyexn_constructor
  ;;

  let of_structure_item it =
    match it.pstr_desc with
    | Pstr_type (_, tds) -> of_type_declarations tds
    | Pstr_typext te -> of_type_extension te
    | Pstr_exception exn -> of_type_exception exn
    | Pstr_kind_abbrev (_, jk) -> of_jkind_annotation jk
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
    | Pstr_docstring _ -> false
  ;;

  let of_value_description vd =
    vd.pval_attributes = []
    && vd.pval_prim = []
    && vd.pval_modalities = No_modalities
    && of_core_type vd.pval_type
  ;;

  let of_signature_item si =
    match si.psig_desc with
    | Psig_value vd -> of_value_description vd
    | Psig_type (_, decls) | Psig_typesubst decls -> of_type_declarations decls
    | Psig_typext te -> of_type_extension te
    | Psig_exception exn -> of_type_exception exn
    | Psig_kind_abbrev (_, jk) -> of_jkind_annotation jk
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
    | Psig_include _ -> false
  ;;

  let of_structure str =
    let rec last = function
      | [] -> false
      | [ item ] -> of_structure_item item
      | _ :: items -> last items
    in
    last str.pst_items
  ;;

  let of_signature sg =
    let rec last = function
      | [] -> false
      | [ item ] -> of_signature_item item
      | _ :: items -> last items
    in
    last sg.psg_items
  ;;
end

module Start = struct
  let rec of_core_type ct =
    match ct.ptyp_desc with
    | Ptyp_quote _ | Ptyp_object (_, _) -> true
    | Ptyp_arrow { domain = aa; _ } -> of_arrow_arg aa
    | Ptyp_alias (ct, _, _) | Ptyp_tuple ((_, ct) :: _) -> of_core_type ct
    | Ptyp_tuple []
    | Ptyp_poly _
    | Ptyp_any _
    | Ptyp_var _
    | Ptyp_parens _
    | Ptyp_unboxed_tuple _
    | Ptyp_constr (_, _)
    | Ptyp_class (_, _)
    | Ptyp_variant (_, _, _)
    | Ptyp_package _
    | Ptyp_open _
    | Ptyp_extension _
    | Ptyp_of_kind _
    | Ptyp_splice _ -> false

  and of_arrow_arg aa =
    aa.aa_legacy_modes = No_modes &&
    aa.aa_lbl = Nolabel &&
    of_core_type aa.aa_type
  ;;
end
