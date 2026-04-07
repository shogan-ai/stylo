open Ocaml_syntax
open Parsetree

let value_description x =
  if Option.is_none x.pval_pre_doc && Option.is_some x.pval_post_doc
  then
    { x with pval_pre_doc = x.pval_post_doc; pval_post_doc = None }
  else x

let type_declaration x =
  if Option.is_none x.ptype_pre_doc && Option.is_some x.ptype_post_doc
  then
    { x with
      ptype_pre_doc = x.ptype_post_doc;
      ptype_post_doc = None }
  else x

let type_extension x =
  if Option.is_none x.ptyext_pre_doc && Option.is_some x.ptyext_post_doc
  then
    { x with
      ptyext_pre_doc = x.ptyext_post_doc;
      ptyext_post_doc = None }
  else x

let type_exception x =
  if Option.is_none x.ptyexn_pre_doc && Option.is_some x.ptyexn_post_doc
  then
  { x with
    ptyexn_pre_doc = x.ptyexn_post_doc;
    ptyexn_post_doc = None }
  else x

let class_type_field x =
  if Option.is_none x.pctf_pre_doc && Option.is_some x.pctf_post_doc
  then
    { x with
      pctf_pre_doc = x.pctf_post_doc;
      pctf_post_doc = None }
  else x

let class_infos x =
  if Option.is_none x.pci_pre_doc && Option.is_some x.pci_post_doc
  then
    { x with
      pci_pre_doc = x.pci_post_doc;
      pci_post_doc = None }
  else x

let class_declaration x = class_infos x
let class_description x = class_infos x
let class_type_declaration x = class_infos x

let class_field x =
  if Option.is_none x.pcf_pre_doc && Option.is_some x.pcf_post_doc
  then
    { x with
      pcf_pre_doc = x.pcf_post_doc;
      pcf_post_doc = None }
  else x

let module_declaration x =
  if Option.is_none x.pmd_pre_doc && Option.is_some x.pmd_post_doc
  then
    { x with
      pmd_pre_doc = x.pmd_post_doc;
      pmd_post_doc = None }
  else x

let module_substitution x =
  if Option.is_none x.pms_pre_doc && Option.is_some x.pms_post_doc
  then
    { x with
      pms_pre_doc = x.pms_post_doc;
      pms_post_doc = None }
  else x

let module_type_declaration x =
  if Option.is_none x.pmtd_pre_doc && Option.is_some x.pmtd_post_doc
  then
    { x with
      pmtd_pre_doc = x.pmtd_post_doc;
      pmtd_post_doc = None }
  else x

let open_infos x =
  if Option.is_none x.popen_pre_doc && Option.is_some x.popen_post_doc
  then
    { x with
      popen_pre_doc = x.popen_post_doc;
      popen_post_doc = None }
  else x

let open_description x = open_infos x
let open_declaration x = open_infos x

let include_infos x =
  if Option.is_none x.pincl_pre_doc && Option.is_some x.pincl_post_doc
  then
    { x with
      pincl_pre_doc = x.pincl_post_doc;
      pincl_post_doc = None }
  else x

let include_description x = include_infos x
let include_declaration x = include_infos x

let value_binding x =
  if Option.is_none x.pvb_pre_doc && Option.is_some x.pvb_post_doc
  then
  { x with
    pvb_pre_doc = x.pvb_post_doc;
    pvb_post_doc = None }
  else x

let module_binding x =
  if Option.is_none x.pmb_pre_doc && Option.is_some x.pmb_post_doc
  then
    { x with
      pmb_pre_doc = x.pmb_post_doc;
      pmb_post_doc = None }
  else x
