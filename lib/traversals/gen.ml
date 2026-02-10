open Ppxlib

let args = List.tl (Array.to_list Sys.argv)

let gen_fold, fnames =
  match args with
  | "--with-fold" :: fnames -> true, fnames
  | fnames -> false, fnames

let to_modname fn =
  Filename.basename fn
  |> Filename.chop_extension
  |> String.capitalize_ascii

let modnames = "Lexing" :: List.map to_modname fnames

let not_a_reexport (td : type_declaration) =
  match td.ptype_manifest with
  | None -> true
  | Some _ ->
    (* just an alias, but probably to an instance of a more generic type *)
    td.ptype_kind = Ptype_abstract

let from_str_item (si : structure_item) =
    match si.pstr_desc with
    | Pstr_type (_rf, tds) ->
      let without_aliases = List.filter not_a_reexport tds in
      Some without_aliases
    | _ -> None

let from_sig_item (si : signature_item) =
    match si.psig_desc with
    | Psig_type (_rf, tds) ->
      let without_aliases = List.filter not_a_reexport tds in
      Some without_aliases
    | _ -> None

let aliases = Hashtbl.create 0

let cleanup_names fn tds =
  let modname = to_modname fn in
  let t_substitute = String.uncapitalize_ascii modname in
  let mapper = object
    inherit Ast_traverse.map as super

    method! longident = function
      | Lident "t" ->
        Hashtbl.replace aliases modname ();
        Lident t_substitute
      | Ldot (Lident othermod, tname) when List.mem othermod modnames ->
        if tname <> "t"
        then Lident tname
        else (
          Hashtbl.replace aliases othermod ();
          Lident (String.uncapitalize_ascii othermod)
        )
      | (Lident _ | Ldot _) as lid -> lid
      | lid ->
        Format.eprintf "unexpected longident `%a' in %s@."
          Pprintast.longident lid fn;
        exit 1

    method! type_declaration td =
      let name = td.ptype_name in
      let td =
        if name.txt = "t"
        then { td with ptype_name = { name with txt = t_substitute } }
        else td
      in
      super#type_declaration td
  end in
  List.map mapper#type_declaration tds

let extract_type_decls fn =
  In_channel.with_open_text fn @@ fun ic ->
  let lb = Lexing.from_channel ic in
  let tds =
    if Filename.check_suffix fn ".mli" then
      Parse.interface lb
      |> List.filter_map from_sig_item
    else
      Parse.implementation lb
      |> List.filter_map from_str_item
  in
  List.concat_map (cleanup_names fn) tds

let tds = List.concat_map extract_type_decls fnames

module Builder = Ast_builder.Make (struct let loc = Location.none end)

let traversals =
  let open Ppxlib_traverse in
  let mapper_class =
    Builder.pstr_class
      [ gen_class ~what:Ppxlib_traverse.Backends.mapper_with_context
          ~loc:Location.none tds ]
  in
  let lift_class =
    if not gen_fold
    then []
    else [
      Builder.pstr_class
        [ gen_class ~what:Ppxlib_traverse.Backends.lifter
            ~loc:Location.none tds ]
    ]
  in
  mapper_class :: lift_class

let mapper =
  let open Builder in
  let aliases =
    Hashtbl.fold (fun modname () tds ->
      let alias = String.uncapitalize_ascii modname in
      let aliased = Located.mk Ppxlib.(Ldot (Lident modname, "t")) in
      let aliased_ty = ptyp_constr aliased [] in
      type_declaration ~name:(Located.mk alias)
        ~params:[]
        ~cstrs:[]
        ~kind:Ptype_abstract
        ~private_:Public
        ~manifest:(Some aliased_ty)
      :: tds
    ) aliases []
  in
  let aliases =
    let expr =
      List.map (fun td -> pstr_type Nonrecursive [td]) aliases
      |> pmod_structure
    in
    pstr_open (open_infos ~override:Fresh ~expr)
  in
  let opens =
    List.map (fun modname ->
      let expr = pmod_ident (Located.lident modname) in
      pstr_open (open_infos ~override:Override ~expr)
    ) modnames
  in
  aliases :: opens @ traversals

let () =
  Pprintast.structure Format.std_formatter mapper
