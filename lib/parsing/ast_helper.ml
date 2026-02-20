(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                         Alain Frisch, LexiFi                           *)
(*                                                                        *)
(*   Copyright 2012 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Helpers to produce Parsetree fragments *)

open Asttypes
open Parsetree
open Docstrings

type 'a with_loc = 'a Location.loc
type loc = Location.t

type lid = Longident.t with_loc
type str = string with_loc
type str_or_op = Longident.str_or_op with_loc
type str_opt = string option with_loc
type attrs = attributes

let default_loc = ref Location.none

let simplify_ds : Docstring.t option -> _ = function
  | None | Some { ds_body=""; _ } -> None
  | Some ds -> Some ds

let with_default_loc l f =
  let orig = !default_loc in
  default_loc := l;
  Fun.protect ~finally:(fun () -> default_loc := orig) f

let empty_ext_attr =
  { pea_ext = None; pea_attrs = No_attributes }

module Docs = struct
  let body_as_doc ds = Docstring (Docstrings.docstring_body ds)

  let text =
    List.filter_map (fun ds ->
      simplify_ds (Some ds)
      |> Option.map body_as_doc
    )

  let pre_post docs =
    let pre_doc = simplify_ds docs.docs_pre in
    let post_doc = simplify_ds docs.docs_post in
    Option.map body_as_doc pre_doc,
    Option.map body_as_doc post_doc

  let info info =
    let info = simplify_ds info in
    Option.map body_as_doc info
end

module Ext = struct
  let mk ?(docs=empty_docs) ext attrs =
    let pre, post = Docs.pre_post docs in
    { te_pre_doc = pre
    ; te_ext = ext
    ; te_attrs = attrs
    ; te_post_doc = post }
end

module Const = struct
  let integer ?sign ?suffix i = Pconst_integer (sign, i, suffix)
  let int ?suffix i = integer ?suffix (Int.to_string i)
  let int32 ?(suffix='l') i = integer ~suffix (Int32.to_string i)
  let int64 ?(suffix='L') i = integer ~suffix (Int64.to_string i)
  let nativeint ?(suffix='n') i = integer ~suffix (Nativeint.to_string i)
  let float ?sign ?suffix f = Pconst_float (sign, f, suffix)
  let char c = Pconst_char (c, String.make 1 c)
  let string ?quotation_delimiter ?(loc= !default_loc) s =
    Pconst_string (s, loc, quotation_delimiter)
end

module Attr = struct
  let mk ?(loc= !default_loc) ~tokens name payload =
    { attr_name = name;
      attr_payload = payload;
      attr_loc = loc;
      attr_tokens = tokens; }
end

module Typ = struct
  let mk ?(loc = !default_loc) ?(attrs = No_attributes) ~tokens d =
    {ptyp_desc = d;
     ptyp_loc = loc;
     ptyp_attributes = attrs;
     ptyp_tokens = tokens;}

(*   let attr d a = {d with ptyp_attributes = d.ptyp_attributes @ [a]} *)

  let any ?loc ?attrs ~tokens a = mk ?loc ?attrs ~tokens (Ptyp_any a)
  let var ?loc ?attrs ~tokens a b = mk ?loc ?attrs ~tokens (Ptyp_var (a, b))
(*   let arrow ?loc ?attrs ~tokens a b c d e = mk ?loc ?attrs ~tokens
     (Ptyp_arrow (a, b, c, d, e)) *)
  let tuple ?loc ?attrs ~tokens a = mk ?loc ?attrs ~tokens (Ptyp_tuple a)
  let unboxed_tuple ?loc ?attrs ~tokens a = mk ?loc ?attrs ~tokens (Ptyp_unboxed_tuple a)
  let constr ?loc ?attrs ~tokens a b = mk ?loc ?attrs ~tokens (Ptyp_constr (a, b))
  let object_ ?loc ?attrs ~tokens a b = mk ?loc ?attrs ~tokens (Ptyp_object (a, b))
  let class_ ?loc ?attrs ~tokens a b = mk ?loc ?attrs ~tokens (Ptyp_class (a, b))
  let alias ?loc ?attrs ~tokens a b c = mk ?loc ?attrs ~tokens (Ptyp_alias (a, b, c))
  let variant ?loc ?attrs ~tokens a b c = mk ?loc ?attrs ~tokens (Ptyp_variant (a, b, c))
  let poly ?loc ?attrs ~tokens a b = mk ?loc ?attrs ~tokens (Ptyp_poly (a, b))
(*
  let package ?loc ?attrs ~tokens a b =
    let pkg = { ppt_ext_attr = None; ppt_name = a; ppt_eqs = b } in
    mk ?loc ?attrs ~tokens (Ptyp_package pkg)
*)
  let extension ?loc ?attrs ~tokens a = mk ?loc ?attrs ~tokens (Ptyp_extension a)
  let open_ ?loc ?attrs ~tokens mod_ident t = mk ?loc ?attrs ~tokens (Ptyp_open (mod_ident, t))

end

module Pat = struct
  let mk ?(loc = !default_loc) ?(attrs = No_attributes) ~tokens d =
    {ppat_ext_attr = empty_ext_attr;
     ppat_desc = d;
     ppat_loc = loc;
     ppat_attributes = attrs;
     ppat_tokens = tokens}
(*   let attr d a = {d with ppat_attributes = d.ppat_attributes @ [a]} *)

  let any ?loc ?attrs ~tokens () = mk ?loc ?attrs ~tokens Ppat_any
  let var ?loc ?attrs ~tokens a = mk ?loc ?attrs ~tokens (Ppat_var a)
  let alias ?loc ?attrs ~tokens a b = mk ?loc ?attrs ~tokens (Ppat_alias (a, b))
  let constant ?loc ?attrs ~tokens a = mk ?loc ?attrs ~tokens (Ppat_constant a)
  let interval ?loc ?attrs ~tokens a b = mk ?loc ?attrs ~tokens (Ppat_interval (a, b))
  let tuple ?loc ?attrs ~tokens a b = mk ?loc ?attrs ~tokens (Ppat_tuple (a, b))
  let unboxed_tuple ?loc ?attrs ~tokens a b = mk ?loc ?attrs ~tokens (Ppat_unboxed_tuple (a, b))
  let construct ?loc ?attrs ~tokens a b = mk ?loc ?attrs ~tokens (Ppat_construct (a, b))
  let variant ?loc ?attrs ~tokens a b = mk ?loc ?attrs ~tokens (Ppat_variant (a, b))
  let record ?loc ?attrs ~tokens a b = mk ?loc ?attrs ~tokens (Ppat_record (a, b))
  let record_unboxed_product ?loc ?attrs ~tokens a b =
    mk ?loc ?attrs ~tokens (Ppat_record_unboxed_product (a, b))
  let array ?loc ?attrs ~tokens a b = mk ?loc ?attrs ~tokens (Ppat_array (a, b))
  let or_ ?loc ?attrs ~tokens a b = mk ?loc ?attrs ~tokens (Ppat_or (a, b))
  let constraint_ ?loc ?attrs ~tokens a b c = mk ?loc ?attrs ~tokens (Ppat_constraint (a, b, c))
  let type_ ?loc ?attrs ~tokens a = mk ?loc ?attrs ~tokens (Ppat_type a)
  let lazy_ ?loc ?attrs ~tokens a = mk ?loc ?attrs ~tokens (Ppat_lazy a)
  let unpack ?loc ?attrs ~tokens a b = mk ?loc ?attrs ~tokens (Ppat_unpack (a, b))
  let open_ ?loc ?attrs ~tokens a b = mk ?loc ?attrs ~tokens (Ppat_open (a, b))
  let exception_ ?loc ?attrs ~tokens a = mk ?loc ?attrs ~tokens (Ppat_exception a)
  let extension ?loc ?attrs ~tokens a = mk ?loc ?attrs ~tokens (Ppat_extension a)
end

module Exp = struct
  let mk ?(loc = !default_loc) ?(attrs = No_attributes) ~tokens d =
    {pexp_ext_attr = empty_ext_attr;
     pexp_desc = d;
     pexp_loc = loc;
     pexp_attributes = attrs;
     pexp_tokens = tokens}
(*   let attr d a = {d with pexp_attributes = d.pexp_attributes @ [a]} *)

  let ident ?loc ?attrs ~tokens a = mk ?loc ?attrs ~tokens (Pexp_ident a)
  let constant ?loc ?attrs ~tokens a = mk ?loc ?attrs ~tokens (Pexp_constant a)
  let let_ ?loc ?attrs ~tokens a b c d = mk ?loc ?attrs ~tokens (Pexp_let (a, b, c, d))
  let function_ ?loc ?attrs ~tokens a b c = mk ?loc ?attrs ~tokens (Pexp_function (a, b, c))
  let apply ?loc ?attrs ~tokens a b = mk ?loc ?attrs ~tokens (Pexp_apply (a, b))
  let match_ ?loc ?attrs ~tokens a b = mk ?loc ?attrs ~tokens (Pexp_match (a, b))
  let try_ ?loc ?attrs ~tokens a b = mk ?loc ?attrs ~tokens (Pexp_try (a, b))
  let tuple ?loc ?attrs ~tokens a = mk ?loc ?attrs ~tokens (Pexp_tuple a)
  let unboxed_tuple ?loc ?attrs ~tokens a = mk ?loc ?attrs ~tokens (Pexp_unboxed_tuple a)
  let construct ?loc ?attrs ~tokens a b = mk ?loc ?attrs ~tokens (Pexp_construct (a, b))
  let variant ?loc ?attrs ~tokens a b = mk ?loc ?attrs ~tokens (Pexp_variant (a, b))
  let record ?loc ?attrs ~tokens a b = mk ?loc ?attrs ~tokens (Pexp_record (b, a))
  let record_unboxed_product ?loc ?attrs ~tokens a b =
    mk ?loc ?attrs ~tokens (Pexp_record_unboxed_product (b, a))
  let field ?loc ?attrs ~tokens a b = mk ?loc ?attrs ~tokens (Pexp_field (a, b))
  let unboxed_field ?loc ?attrs ~tokens a b = mk ?loc ?attrs ~tokens (Pexp_unboxed_field (a, b))
  let setfield ?loc ?attrs ~tokens a b c = mk ?loc ?attrs ~tokens (Pexp_setfield (a, b, c))
  let array ?loc ?attrs ~tokens a b = mk ?loc ?attrs ~tokens (Pexp_array (a, b))
  let ifthenelse ?loc ?attrs ~tokens a b c = mk ?loc ?attrs ~tokens (Pexp_ifthenelse (a, b, c))
  let sequence ?loc ?attrs ~tokens a b = mk ?loc ?attrs ~tokens (Pexp_sequence (a, b))
  let while_ ?loc ?attrs ~tokens a b = mk ?loc ?attrs ~tokens (Pexp_while (a, b))
  let for_ ?loc ?attrs ~tokens a b c d e = mk ?loc ?attrs ~tokens (Pexp_for (a, b, c, d, e))
  let constraint_ ?loc ?attrs ~tokens a b c = mk ?loc ?attrs ~tokens (Pexp_constraint (a, b, c))
  let coerce ?loc ?attrs ~tokens a b c = mk ?loc ?attrs ~tokens (Pexp_coerce (a, b, c))
  let send ?loc ?attrs ~tokens a b = mk ?loc ?attrs ~tokens (Pexp_send (a, b))
  let new_ ?loc ?attrs ~tokens a = mk ?loc ?attrs ~tokens (Pexp_new a)
  let setinstvar ?loc ?attrs ~tokens a b = mk ?loc ?attrs ~tokens (Pexp_setvar (a, b))
  let override ?loc ?attrs ~tokens a = mk ?loc ?attrs ~tokens (Pexp_override a)
  let letmodule ?loc ?attrs ~tokens a b = mk ?loc ?attrs ~tokens (Pexp_letmodule (a, b))
  let letexception ?loc ?attrs ~tokens a b = mk ?loc ?attrs ~tokens (Pexp_letexception (a, b))
  let assert_ ?loc ?attrs ~tokens a = mk ?loc ?attrs ~tokens (Pexp_assert a)
  let lazy_ ?loc ?attrs ~tokens a = mk ?loc ?attrs ~tokens (Pexp_lazy a)
  let object_ ?loc ?attrs ~tokens a = mk ?loc ?attrs ~tokens (Pexp_object a)
  let pack ?loc ?attrs ~tokens ?pkg_type a = mk ?loc ?attrs ~tokens (Pexp_pack (a, pkg_type))
  let open_ ?loc ?attrs ~tokens a b = mk ?loc ?attrs ~tokens (Pexp_dot_open (a, b))
  let letop ?loc ?attrs ~tokens let_ ands body =
    mk ?loc ?attrs ~tokens (Pexp_letop {let_; ands; body})
  let extension ?loc ?attrs ~tokens a = mk ?loc ?attrs ~tokens (Pexp_extension a)
  let unreachable ?loc ?attrs ~tokens () = mk ?loc ?attrs ~tokens Pexp_unreachable
  let stack ?loc ?attrs ~tokens e = mk ?loc ?attrs ~tokens (Pexp_stack e)
  let comprehension ?loc ?attrs ~tokens e = mk ?loc ?attrs ~tokens (Pexp_comprehension e)
  let overwrite ?loc ?attrs ~tokens a b = mk ?loc ?attrs ~tokens (Pexp_overwrite (a, b))
  let hole ?loc ?attrs ~tokens () = mk ?loc ?attrs ~tokens Pexp_hole

  let case ?(loc = !default_loc) ~tokens lhs ?guard rhs =
    {
     pc_lhs = lhs;
     pc_guard = guard;
     pc_rhs = rhs;
     pc_tokens = tokens;
     pc_loc = loc;
    }

  let binding_op op vb loc =
    {
      pbop_op = op;
      pbop_binding = vb;
      pbop_loc = loc;
    }
end

module Mty = struct
  let mk ?(loc = !default_loc) ?(attrs = No_attributes) ~tokens d =
    {pmty_desc = d; pmty_loc = loc; pmty_attributes = attrs;
     pmty_tokens = tokens}
(*   let attr d a = {d with pmty_attributes = d.pmty_attributes @ [a]} *)

  let ident ?loc ?attrs ~tokens a = mk ?loc ?attrs ~tokens (Pmty_ident a)
  let alias ?loc ?attrs ~tokens a = mk ?loc ?attrs ~tokens (Pmty_alias a)
  let signature ?loc ?attrs ~tokens a = mk ?loc ?attrs ~tokens (Pmty_signature a)
  let functor_ ?loc ?attrs ~tokens ?(ret_mode=No_modes) a b c =
    mk ?loc ?attrs ~tokens (Pmty_functor (a, b, c,ret_mode))
  let with_ ?loc ?attrs ~tokens a b = mk ?loc ?attrs ~tokens (Pmty_with (a, b))
  let typeof_ ?loc ?attrs ~tokens a b =
    mk ?loc ?attrs ~tokens (Pmty_typeof (a, b))
  let extension ?loc ?attrs ~tokens a = mk ?loc ?attrs ~tokens (Pmty_extension a)
  let strengthen ?loc ?attrs ~tokens a b = mk ?loc ?attrs ~tokens (Pmty_strengthen (a, b))
end

module Mod = struct
  let mk ?(loc = !default_loc) ?(attrs = No_attributes) ~tokens d =
    {pmod_desc = d; pmod_loc = loc; pmod_attributes = attrs;
     pmod_tokens = tokens}
(*   let attr d a = {d with pmod_attributes = d.pmod_attributes @ [a]} *)

  let ident ?loc ?attrs ~tokens x = mk ?loc ?attrs ~tokens (Pmod_ident x)
  let structure ?loc ?attrs ~tokens a x =
    mk ?loc ?attrs ~tokens (Pmod_structure (a, x))
  let functor_ ?loc ?attrs ~tokens attributes args body =
    mk ?loc ?attrs ~tokens (Pmod_functor (attributes, args, body))
  let apply ?loc ?attrs ~tokens m1 m2 = mk ?loc ?attrs ~tokens (Pmod_apply (m1, m2))
  let apply_unit ?loc ?attrs ~tokens m1 = mk ?loc ?attrs ~tokens (Pmod_apply_unit m1)
  let constraint_ ?loc ?attrs ~tokens ty mode m =
    mk ?loc ?attrs ~tokens (Pmod_constraint (m, ty, mode))
  let unpack ?loc ?(attrs = No_attributes) ~tokens ?constr ?coerce e =
    mk ?loc ~tokens (Pmod_unpack (attrs, e, constr, coerce))
  let extension ?loc ?attrs ~tokens a = mk ?loc ?attrs ~tokens (Pmod_extension a)
(*   let instance ?loc ?attrs a = mk ?loc ?attrs (Pmod_instance a) *)
end

module Sig = struct
  let mk ?(loc = !default_loc) ~tokens d =
    {psig_desc = d; psig_loc = loc; psig_tokens = tokens}

  (*
  let value ?loc a = mk ?loc (Psig_value a)
  let type_ ?loc rec_flag a = mk ?loc (Psig_type (rec_flag, a))
  let type_subst ?loc a = mk ?loc (Psig_typesubst a)
  let type_extension ?loc a = mk ?loc (Psig_typext a)
  let exception_ ?loc a = mk ?loc (Psig_exception a)
  let module_ ?loc a = mk ?loc (Psig_module a)
  let mod_subst ?loc a = mk ?loc (Psig_modsubst a)
  let rec_module ?loc a = mk ?loc (Psig_recmodule a)
  let modtype ?loc a = mk ?loc (Psig_modtype a)
  let modtype_subst ?loc a = mk ?loc (Psig_modtypesubst a)
  let open_ ?loc a = mk ?loc (Psig_open a)
  let include_ ?loc ?(modalities = []) a = mk ?loc (Psig_include (a, modalities))
  let class_ ?loc a = mk ?loc (Psig_class a)
  let class_type ?loc a = mk ?loc (Psig_class_type a)
  let extension ?loc ?(attrs = No_attributes) a = mk ?loc (Psig_extension (a, attrs))
  let kind_abbrev ?loc a b = mk ?loc (Psig_kind_abbrev (a, b))
  let attribute ?loc ~tokens a = mk ?loc ~tokens (Psig_attribute a)
  *)
  let text txt =
    let f_txt = List.filter (fun ds -> docstring_body ds <> "") txt in
    List.map
      (fun ds ->
         let loc = docstring_loc ds in
         let ds_toks = Tokens.at (loc.loc_start, loc.loc_end) in
         mk ~loc ~tokens:ds_toks (Psig_docstring (Docs.body_as_doc ds)))
      f_txt
end

module Sg = struct
  let mk ?(loc = !default_loc) ~tokens ?(modalities = No_modalities) a =
    {psg_items = a; psg_modalities = modalities; psg_loc = loc;
     psg_tokens = tokens}
end

module Str = struct
  let mk ?(loc = !default_loc) ~tokens d =
    {pstr_desc = d; pstr_loc = loc; pstr_tokens = tokens}

  let eval ?loc ?(attrs = No_attributes) ~tokens a = mk ?loc ~tokens (Pstr_eval (a, attrs))
  (*
  let value ?loc a b = mk ?loc (Pstr_value (a, b))
  let primitive ?loc a = mk ?loc (Pstr_primitive a)
  let type_ ?loc rec_flag a = mk ?loc (Pstr_type (rec_flag, a))
  let type_extension ?loc a = mk ?loc (Pstr_typext a)
  let exception_ ?loc a = mk ?loc (Pstr_exception a)
  let module_ ?loc a = mk ?loc (Pstr_module a)
  let rec_module ?loc a = mk ?loc (Pstr_recmodule a)
  let modtype ?loc a = mk ?loc (Pstr_modtype a)
  let open_ ?loc a = mk ?loc (Pstr_open a)
  let class_ ?loc a = mk ?loc (Pstr_class a)
  let class_type ?loc a = mk ?loc (Pstr_class_type a)
  let include_ ?loc a = mk ?loc (Pstr_include a)
  let extension ?loc ?(attrs = No_attributes) a = mk ?loc (Pstr_extension (a, attrs))
  let kind_abbrev ?loc a b = mk ?loc (Pstr_kind_abbrev (a, b))
  let attribute ?loc ~tokens a = mk ?loc ~tokens (Pstr_attribute a)
     *)
  let text txt =
    let f_txt = List.filter (fun ds -> docstring_body ds <> "") txt in
    List.map
      (fun ds ->
         let loc = docstring_loc ds in
         let ds_toks = Tokens.at (loc.loc_start, loc.loc_end) in
         mk ~loc ~tokens:ds_toks (Pstr_docstring (Docs.body_as_doc ds)))
      f_txt
end

module Cl = struct
  let mk ?(loc = !default_loc) ?(ext_attrs = empty_ext_attr)
        ?(attrs = No_attributes) ~tokens d =
    {
     pcl_ext_attrs = ext_attrs;
     pcl_desc = d;
     pcl_loc = loc;
     pcl_attributes = attrs;
     pcl_tokens = tokens;
    }
(*   let attr d a = {d with pcl_attributes = d.pcl_attributes @ [a]} *)

  let constr ?loc ?attrs ~tokens a b = mk ?loc ?attrs ~tokens (Pcl_constr (a, b))
  let structure ?loc ?attrs ~tokens a = mk ?loc ?attrs ~tokens (Pcl_structure a)
  let fun_ ?loc ?ext_attrs ?attrs ~tokens a b =
    mk ?loc ?ext_attrs ?attrs ~tokens (Pcl_fun (a, b))
  let apply ?loc ?attrs ~tokens a b = mk ?loc ?attrs ~tokens (Pcl_apply (a, b))
  let let_ ?loc ?attrs ~tokens a b c = mk ?loc ?attrs ~tokens (Pcl_let (a, b, c))
  let constraint_ ?loc ?attrs ~tokens a b = mk ?loc ?attrs ~tokens (Pcl_constraint (a, b))
  let extension ?loc ?attrs ~tokens a = mk ?loc ?attrs ~tokens (Pcl_extension a)
  let open_ ?loc ?attrs ~tokens a b = mk ?loc ?attrs ~tokens (Pcl_open (a, b))
end

module Cty = struct
  let mk ?(loc = !default_loc) ?(attrs = No_attributes) ~tokens d =
    {
     pcty_desc = d;
     pcty_loc = loc;
     pcty_attributes = attrs;
     pcty_tokens = tokens;
    }
(*   let attr d a = {d with pcty_attributes = d.pcty_attributes @ [a]} *)

  let constr ?loc ?attrs ~tokens a b = mk ?loc ?attrs ~tokens (Pcty_constr (a, b))
  let signature ?loc ?attrs ~tokens a = mk ?loc ?attrs ~tokens (Pcty_signature a)
  let arrow ?loc ?attrs ~tokens a b = mk ?loc ?attrs ~tokens (Pcty_arrow (a, b))
  let extension ?loc ?attrs ~tokens a = mk ?loc ?attrs ~tokens (Pcty_extension a)
  let open_ ?loc ?attrs ~tokens a b = mk ?loc ?attrs ~tokens (Pcty_open (a, b))
end

module Ctf = struct
  let mk ?(loc = !default_loc) ?(attrs = No_attributes) ~tokens
           ?(docs = empty_docs) d =
    let pre_doc, post_doc = Docs.pre_post docs in
    {
     pctf_pre_doc = pre_doc;
     pctf_desc = d;
     pctf_loc = loc;
     pctf_attributes = attrs;
     pctf_post_doc = post_doc;
     pctf_tokens = tokens;
    }

  (*
  let inherit_ ?loc ?attrs a = mk ?loc ?attrs (Pctf_inherit a)
  let val_ ?loc ?attrs a b c d = mk ?loc ?attrs (Pctf_val (a, b, c, d))
  let method_ ?loc ?attrs a b c d = mk ?loc ?attrs (Pctf_method (a, b, c, d))
  let constraint_ ?loc ?attrs a b = mk ?loc ?attrs (Pctf_constraint (a, b))
  let extension ?loc ?attrs a = mk ?loc ?attrs (Pctf_extension a)
  let attribute ?loc a = mk ?loc (Pctf_attribute a)
      *)
  let text txt =
    let f_txt = List.filter (fun ds -> docstring_body ds <> "") txt in
    List.map
      (fun ds ->
         let loc = docstring_loc ds in
         let ds_toks = Tokens.at (loc.loc_start, loc.loc_end) in
         mk ~loc ~tokens:ds_toks (Pctf_docstring (Docs.body_as_doc ds)))
      f_txt

(*   let attr d a = {d with pctf_attributes = d.pctf_attributes @ [a]} *)

end

module Cf = struct
  let mk ?(loc = !default_loc) ?(attrs = No_attributes) ~tokens
        ?(docs = empty_docs) d =
    let pre_doc, post_doc = Docs.pre_post docs in
    {
     pcf_pre_doc = pre_doc;
     pcf_desc = d;
     pcf_loc = loc;
     pcf_attributes = attrs;
     pcf_post_doc = post_doc;
     pcf_tokens = tokens;
    }

  (*
  let inherit_ ?loc ?attrs a b c = mk ?loc ?attrs (Pcf_inherit (a, b, c))
  let val_ ?loc ?attrs a b c = mk ?loc ?attrs (Pcf_val (a, b, c))
  let method_ ?loc ?attrs a b c = mk ?loc ?attrs (Pcf_method (a, b, c))
  let constraint_ ?loc ?attrs a b = mk ?loc ?attrs (Pcf_constraint (a, b))
  let initializer_ ?loc ?attrs a = mk ?loc ?attrs (Pcf_initializer a)
  let extension ?loc ?attrs a = mk ?loc ?attrs (Pcf_extension a)
  let attribute ?loc ~tokens a = mk ?loc ~tokens (Pcf_attribute a)
   *)
  let text txt =
    let f_txt = List.filter (fun ds -> docstring_body ds <> "") txt in
    List.map
      (fun ds ->
         let loc = docstring_loc ds in
         let ds_toks = Tokens.at (loc.loc_start, loc.loc_end) in
         mk ~loc ~tokens:ds_toks (Pcf_docstring (Docs.body_as_doc ds)))
      f_txt

  let virtual_ ct = Cfk_virtual ct
  let concrete o e = Cfk_concrete (o, e)

(*   let attr d a = {d with pcf_attributes = d.pcf_attributes @ [a]} *)

end

module Val = struct
  let mk ?(loc = !default_loc) ?(ext_attrs = empty_ext_attr) ?(attrs = No_attributes)
        ~tokens ?(docs = empty_docs) ?(prim = []) ?(modalities=No_modalities)
        name typ =
    let pre_doc, post_doc = Docs.pre_post docs in
    {
     pval_pre_doc = pre_doc;
     pval_ext_attrs = ext_attrs;
     pval_name = name;
     pval_type = typ;
     pval_attributes = attrs;
     pval_modalities = modalities;
     pval_loc = loc;
     pval_prim = prim;
     pval_post_doc = post_doc;
     pval_tokens = tokens;
    }
end

module Md = struct
  let mk ?(loc = !default_loc) ?(ext_attrs = empty_ext_attr) ?(attrs = No_attributes)
        ~tokens ?(docs = empty_docs) ?(text = []) name body =
    let pre_doc, post_doc = Docs.pre_post docs in
    {
     pmd_pre_text = Docs.text text;
     pmd_pre_doc = pre_doc;
     pmd_ext_attrs = ext_attrs;
     pmd_name = name;
     pmd_body = body;
     pmd_attributes = attrs;
     pmd_post_doc = post_doc;
     pmd_loc = loc;
     pmd_tokens = tokens;
    }
end

module Ms = struct
  let mk ?(loc = !default_loc) ?(ext_attrs = empty_ext_attr) ?(attrs = No_attributes)
        ~tokens ?(docs = empty_docs) name syn =
    let pre_doc, post_doc = Docs.pre_post docs in
    {
     pms_pre_doc = pre_doc;
     pms_ext_attrs = ext_attrs;
     pms_name = name;
     pms_manifest = syn;
     pms_attributes = attrs;
     pms_post_doc = post_doc;
     pms_loc = loc;
     pms_tokens = tokens;
    }
end

module Mtd = struct
  let mk ?(loc = !default_loc) ?(ext_attrs = empty_ext_attr) ?(attrs = No_attributes)
        ~tokens ?(docs = empty_docs) ?typ name =
    let pre_doc, post_doc = Docs.pre_post docs in
    {
     pmtd_pre_doc = pre_doc;
     pmtd_ext_attrs = ext_attrs;
     pmtd_name = name;
     pmtd_type = typ;
     pmtd_attributes = attrs;
     pmtd_loc = loc;
     pmtd_post_doc = post_doc;
     pmtd_tokens = tokens;
    }
end

module Mb = struct
  let mk ?(loc = !default_loc) ?(ext_attr=empty_ext_attr) ?(attrs = No_attributes) ~tokens
        ?(docs = empty_docs) ?(text = []) name params mty_opt modes expr =
    let pmb_pre_doc, pmb_post_doc = Docs.pre_post docs in
    {
     pmb_pre_text = Docs.text text;
     pmb_pre_doc;
     pmb_ext_attrs = ext_attr;
     pmb_name = name;
     pmb_params = params;
     pmb_constraint = mty_opt;
     pmb_modes = modes;
     pmb_expr = expr;
     pmb_attributes = attrs;
     pmb_post_doc;
     pmb_loc = loc;
     pmb_tokens = tokens;
    }
end

module Opn = struct
  let mk ?(loc = !default_loc) ?(ext_attrs = empty_ext_attr) ?(attrs = No_attributes)
      ~tokens ?(docs = empty_docs) ?(override = Fresh) expr =
    let pre_doc, post_doc = Docs.pre_post docs in
    {
     popen_pre_doc = pre_doc;
     popen_ext_attrs = ext_attrs;
     popen_expr = expr;
     popen_override = override;
     popen_loc = loc;
     popen_attributes = attrs;
     popen_post_doc = post_doc;
     popen_tokens = tokens;
    }
end

module Incl = struct
  let mk ?(loc = !default_loc) ?(ext_attrs = empty_ext_attr) ?(attrs = No_attributes)
      ~tokens ?(docs = empty_docs)
    ?(kind = Structure) mexpr =
    let pre_doc, post_doc = Docs.pre_post docs in
    {
     pincl_pre_doc = pre_doc;
     pincl_ext_attrs = ext_attrs;
     pincl_kind = kind;
     pincl_mod = mexpr;
     pincl_loc = loc;
     pincl_attributes = attrs;
     pincl_post_doc = post_doc;
     pincl_tokens = tokens;
    }

end

module Vb = struct
  let mk ?(loc = !default_loc) ?(ext_attr=empty_ext_attr)
        ?(attrs = No_attributes) ~tokens ?(docs = empty_docs)
        ?(text = []) ?(params = []) ?(legacy_modes = No_modes) ?(modes = No_modes)
        ?value_constraint ?(ret_modes = No_modes) pat expr =
    let pre_doc, post_doc = Docs.pre_post docs in
    {
     pvb_pre_text = Docs.text text;
     pvb_pre_doc = pre_doc;
     pvb_ext_attrs = ext_attr;
     pvb_legacy_modes = legacy_modes;
     pvb_pat = pat;
     pvb_params = params;
     pvb_expr = expr;
     pvb_constraint=value_constraint;
     pvb_modes = modes;
     pvb_ret_modes = ret_modes;
     pvb_attributes = attrs;
     pvb_post_doc = post_doc;
     pvb_loc = loc;
     pvb_tokens = tokens;
    }
end

module Ci = struct
  let mk ?(loc = !default_loc) ?(ext_attr=empty_ext_attr) ?(attrs = No_attributes) ~tokens
        ?(docs = empty_docs) ?(text = [])
        ?(virt = Concrete) ?(params = []) name ?(value_params=[])
        ?constraint_ expr =
    let pre_doc, post_doc = Docs.pre_post docs in
    {
     pci_pre_text = Docs.text text;
     pci_pre_doc = pre_doc;
     pci_virt = virt;
     pci_ext_attrs = ext_attr;
     pci_params = params;
     pci_name = name;
     pci_value_params = value_params;
     pci_constraint = constraint_;
     pci_expr = expr;
     pci_attributes = attrs;
     pci_post_doc = post_doc;
     pci_loc = loc;
     pci_tokens = tokens;
    }
end

module Type = struct
  let mk ?(loc = !default_loc) ?(ext_attr=empty_ext_attr) ?(attrs = No_attributes)
        ~tokens ?(docs = empty_docs) ?(text = [])
      ?(params = [])
      ?(cstrs = [])
      ?(kind = Ptype_abstract)
      ?(priv = Public)
      ?manifest
      ?jkind_annotation
      name =
    let pre_doc, post_doc = Docs.pre_post docs in
    {
     ptype_pre_text = Docs.text text;
     ptype_pre_doc = pre_doc;
     ptype_ext_attrs = ext_attr;
     ptype_name = name;
     ptype_params = params;
     ptype_cstrs = cstrs;
     ptype_kind = kind;
     ptype_private = priv;
     ptype_manifest = manifest;
     ptype_attributes = attrs;
     ptype_post_doc = post_doc;
     ptype_jkind_annotation = jkind_annotation;
     ptype_loc = loc;
     ptype_tokens = tokens;
    }

  let constructor ?(loc = !default_loc) ?(attrs = No_attributes) ~tokens
        ?(info = empty_info)
        ?(vars = []) ?(args = Pcstr_tuple []) ?res name =
    let doc = Docs.info info in
    {
     pcd_name = name;
     pcd_vars = vars;
     pcd_args = args;
     pcd_res = res;
     pcd_loc = loc;
     pcd_attributes = attrs;
     pcd_doc = doc;
     pcd_tokens = tokens;
    }

  let constructor_arg ?(loc = !default_loc) ~global ?(modalities = No_modalities) typ =
    {
      pca_global = global;
      pca_modalities = modalities;
      pca_type = typ;
      pca_loc = loc;
    }

  let field ?(loc = !default_loc) ?(attrs = No_attributes) ~tokens ?(info = empty_info)
        ?(mut = Immutable) ?(global=false) ?(modalities = No_modalities) name typ =
    {
     pld_name = name;
     pld_mutable = mut;
     pld_global = global;
     pld_modalities = modalities;
     pld_type = typ;
     pld_loc = loc;
     pld_attributes = attrs;
     pld_doc = Docs.info info;
     pld_tokens = tokens;
    }

end

(** Type extensions *)
module Te = struct
  let mk ?(loc = !default_loc) ?(ext_attrs = empty_ext_attr) ?(attrs = No_attributes)
      ~tokens ?(docs = empty_docs) ?(params = []) ?(priv = Public) path
      constructors =
    let pre_doc, post_doc = Docs.pre_post docs in
    {
     ptyext_pre_doc = pre_doc;
     ptyext_ext_attrs = ext_attrs;
     ptyext_path = path;
     ptyext_params = params;
     ptyext_constructors = constructors;
     ptyext_private = priv;
     ptyext_loc = loc;
     ptyext_attributes = attrs;
     ptyext_post_doc = post_doc;
     ptyext_tokens = tokens;
    }

  let mk_exception ?(loc = !default_loc) ?(ext_attrs = empty_ext_attr)
      ?(attrs = No_attributes) ~tokens ?(docs = empty_docs) constructor =
    let pre_doc, post_doc = Docs.pre_post docs in
    {
     ptyexn_pre_doc = pre_doc;
     ptyexn_ext_attrs = ext_attrs;
     ptyexn_constructor = constructor;
     ptyexn_loc = loc;
     ptyexn_attributes = attrs;
     ptyexn_post_doc = post_doc;
     ptyexn_tokens = tokens;
    }

  let constructor ?(loc = !default_loc) ?(attrs = No_attributes) ~tokens
        ?(info = empty_info) name kind =
    let doc = Docs.info info in
    {
     pext_name = name;
     pext_kind = kind;
     pext_loc = loc;
     pext_attributes = attrs;
     pext_doc = doc;
     pext_tokens = tokens;
    }

  let decl ?(loc = !default_loc) ?(attrs = No_attributes) ~tokens
         ?(info = empty_info) ?(vars = []) ?(args = Pcstr_tuple []) ?res name =
    let doc = Docs.info info in
    {
     pext_name = name;
     pext_kind = Pext_decl(vars, args, res);
     pext_loc = loc;
     pext_attributes = attrs;
     pext_doc = doc;
     pext_tokens = tokens;
    }

  let rebind ?(loc = !default_loc) ?(attrs = No_attributes) ~tokens
        ?(info = empty_info) name lid =
    let doc = Docs.info info in
    {
     pext_name = name;
     pext_kind = Pext_rebind lid;
     pext_loc = loc;
     pext_attributes = attrs;
     pext_doc = doc;
     pext_tokens = tokens;
    }

end

module Csig = struct
  let mk self fields =
    {
     pcsig_self = self;
     pcsig_fields = fields;
    }
end

module Cstr = struct
  let mk self fields =
    {
     pcstr_self = self;
     pcstr_fields = fields;
    }
end

(** Row fields *)
module Rf = struct
  let mk ?(loc = !default_loc) ?(attrs = No_attributes) ~tokens ?(info = empty_info)desc =
    let doc = Docs.info info in
    {
      prf_desc = desc;
      prf_loc = loc;
      prf_attributes = attrs;
      prf_doc = doc;
      prf_tokens = tokens;
    }

  let tag ?loc ?attrs ~tokens ?info label const tys =
    mk ?loc ?attrs ?info ~tokens (Rtag (label, const, tys))
  let inherit_?loc ~tokens ty =
    mk ?loc ~tokens (Rinherit ty)
end

(** Object fields *)
module Of = struct
  let mk ?(loc = !default_loc) ?(attrs = No_attributes) ~tokens ?(info = empty_info) desc =
    {
      pof_desc = desc;
      pof_loc = loc;
      pof_attributes = attrs;
      pof_doc = Docs.info info;
      pof_tokens = tokens;
    }

  let tag ?loc ?attrs ~tokens ?info label ty =
    mk ?loc ?attrs ~tokens ?info (Otag (label, ty))
  let inherit_ ?loc ~tokens ty =
    mk ?loc ~tokens (Oinherit ty)
end

module Arg = struct
  let nolabel ~loc ~tokens ?(legacy_modes=No_modes) ?typ_constraint
        ?(modes=No_modes) arg =
    { parg_desc = Parg_unlabelled { legacy_modes; arg; typ_constraint; modes };
      parg_tokens = tokens; parg_loc = loc }

  let mk ~opt ?(legacy_modes=No_modes) ?maybe_punned ?typ_constraint
      ?(modes=No_modes) ?default name =
    Parg_labelled {
      optional = opt;
      legacy_modes;
      name;
      maybe_punned;
      typ_constraint;
      modes;
      default;
    }

  let labelled ~loc ~tokens ?legacy_modes ?maybe_punned ?typ_constraint ?modes
        name =
    let desc =
      mk ~opt:false ?legacy_modes ?maybe_punned ?typ_constraint ?modes name
    in
    { parg_desc = desc; parg_tokens = tokens; parg_loc = loc }

  let optional ~loc ~tokens ?legacy_modes ?maybe_punned ?typ_constraint ?modes
        ?default name =
    let desc =
      mk ~opt:true ?legacy_modes ?maybe_punned ?typ_constraint ?modes ?default
        name
    in
    { parg_desc = desc; parg_tokens = tokens; parg_loc = loc }
end
