let files = ref []

let () =
  Arg.parse [] (fun fn -> files := fn :: !files) "gen_reducer.exe CM[OI]S"
;;

open Typedtree

module Decls : sig
  val register : unit -> unit

  type id =
    | Predef of string
    | Local of (string * string)
    | Unknown of (string * string)

  val canonical_id : string -> Path.t -> id
  val iter : (string * string -> type_declaration -> unit) -> unit
end = struct
  let cmts =
    List.map
      Filename.(
        fun fn ->
          if check_suffix fn ".cmo"
          then remove_extension fn ^ ".cmt"
          else remove_extension fn ^ ".cmti")
      !files
  ;;

  let process_cmts decls =
    let process_cmt fn =
      let cmt = Cmt_format.read_cmt fn in
      let modname =
        let wrapping = "ocaml_syntax__" in
        String.sub
          cmt.cmt_modname
          (String.length wrapping)
          (String.length cmt.cmt_modname - String.length wrapping)
      in
      match cmt.cmt_annots with
      | Implementation str ->
        let process_item item =
          match item.str_desc with
          | Tstr_type (_rf, tds) ->
            List.iter
              (fun td -> Hashtbl.add decls (modname, td.typ_name.txt) td)
              tds
          | _ -> ()
        in
        List.iter process_item str.str_items
      | Interface sg ->
        let process_item item =
          match item.sig_desc with
          | Tsig_type (_rf, tds) ->
            List.iter
              (fun td -> Hashtbl.add decls (modname, td.typ_name.txt) td)
              tds
          | _ -> ()
        in
        List.iter process_item sg.sig_items
      | _ -> assert false
    in
    List.iter process_cmt cmts
  ;;

  type decl =
    | Canonical of type_declaration
    | Alias of (string * string)

  let canonical_decls = Hashtbl.create 0

  let canonicalize raw_decls =
    Hashtbl.iter
      (fun decl_id td ->
        let decl =
          match td.typ_manifest with
          | Some { ctyp_desc = Ttyp_constr (path, _, _); _ } ->
            (match path with
             | Pdot (Pdot (__double_underscore_wrapper, unit_name), ty_name)
               when Hashtbl.mem raw_decls (unit_name, ty_name) ->
               Alias (unit_name, ty_name)
             | _ -> Canonical td)
          | _ -> Canonical td
        in
        Hashtbl.add canonical_decls decl_id decl)
      raw_decls
  ;;

  let register () =
    let decls = Hashtbl.create 0 in
    process_cmts decls;
    canonicalize decls
  ;;

  type id =
    | Predef of string
    | Local of (string * string)
    | Unknown of (string * string)

  let canonical_id curr_unit = function
    | Path.Pident id when Ident.is_predef id -> Predef (Ident.name id)
    | Pdot (Pident id, name) when Ident.name id = "Stdlib" ->
      Predef name (* Admittedly hackish. *)
    | p ->
      let id =
        match p with
        | Pident id -> curr_unit, Ident.name id
        | Pdot (Pdot (__double_underscore_wrapper, unit_name), name) ->
          unit_name, name
        | _ ->
          Format.eprintf
            "Unexpected path: @[<hov 2>%a@]@."
            (Format_doc.compat Path.print)
            p;
          exit 1
      in
      let rec follow_indirs id =
        match Hashtbl.find canonical_decls id with
        | Alias id -> follow_indirs id
        | Canonical _ -> Local id
        | exception Not_found -> Unknown id
      in
      follow_indirs id
  ;;

  let iter f =
    Hashtbl.iter
      (fun id decl ->
        match decl with
        | Alias _ -> ()
        | Canonical td -> f id td)
      canonical_decls
  ;;
end

let opened = [ "Parsetree"; "Asttypes" ]
let env_var = "'reducer_env"
let res_var = "'result"

let reducer_name modname tname =
  if List.mem modname opened
  then tname
  else if tname = "t"
  then String.uncapitalize_ascii modname
  else Format.sprintf "%s_%s" (String.uncapitalize_ascii modname) tname
;;

let vars_of_params (modname, tname) lst =
  List.map
    (fun (ct, _) ->
      match ct.ctyp_desc with
      | Ttyp_var s -> s
      | _ ->
        Format.eprintf
          "%s.%s: only named var parameters allowed@\n\
           (No underscore, no constraints)@."
          modname
          tname;
        exit 1)
    lst
;;

open Format

let comma fmt () = fprintf fmt ",@ "

let reducer_field_of_td fmt (modname, tname) td =
  let vars = vars_of_params (modname, tname) td.typ_params in
  let field_name = reducer_name modname tname in
  let name =
    if List.mem modname opened then tname else sprintf "%s.%s" modname tname
  in
  let pp_var fmt = fprintf fmt "'%s" in
  let pp_typ fmt () =
    let pp_params fmt params =
      match params with
      | [] -> ()
      | lst -> fprintf fmt "(%a)@ " (pp_print_list ~pp_sep:comma pp_var) lst
    in
    fprintf fmt "@[<hov 2>%a%s@]" pp_params vars name
  in
  let pp_vars_and_reducers fmt = function
    | [] -> fprintf fmt "@ (%s, %s) reducer@ ->" env_var res_var
    | vars ->
      let reducer fmt s =
        fprintf fmt "@[<hov 2>(%s@ ->@ '%s@ ->@ %s)@]" env_var s res_var
      in
      fprintf
        fmt
        "@ %a.@ (%s, %s) reducer@ ->@ %a@ ->"
        (pp_print_list ~pp_sep:pp_print_space pp_var)
        vars
        env_var
        res_var
        (pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt "@ ->@ ") reducer)
        vars
  in
  fprintf
    fmt
    "@;@[<hov 2>%s :%a@ %s@ ->@ %a@ ->@ %s;@]"
    field_name
    pp_vars_and_reducers
    vars
    env_var
    pp_typ
    ()
    res_var
;;

let reducer_call curr_unit fmt p =
  let predefs =
    [ "option", "reduce_option"
    ; "list", "reduce_list"
    ; "ref", "(fun reducer _reduce_content -> ret_init reducer)"
    ]
  in
  let without_reducer = [ "Lexing", "position"; "Parser_tokens", "token" ] in
  match Decls.canonical_id curr_unit p with
  | Predef name ->
    fprintf
      fmt
      "%s reducer"
      (try List.assoc name predefs with
       | Not_found -> "ret_init")
  | Local (punit, pname) ->
    fprintf fmt "reducer.%s reducer" (reducer_name punit pname)
  | Unknown ((punit, pname) as id) ->
    if List.mem id without_reducer
    then pp_print_string fmt "ret_init reducer"
    else (
      eprintf "Unknown type: %s.%s (from: %s)@." punit pname curr_unit;
      exit 1)
;;

let rec ty_constr_reducer curr_unit fmt (path, ct_list) =
  let arg_reducer fmt ct =
    match ct.ctyp_desc with
    | Ttyp_var s -> fprintf fmt "reduce_'%s" s
    | Ttyp_constr (path, _, ct_list) ->
      fprintf fmt "@[<hov 2>(";
      ty_constr_reducer curr_unit fmt (path, ct_list);
      fprintf fmt ")@]"
    | Ttyp_tuple _ ->
      fprintf fmt "@[<hov 2>@[<hov 2>(fun@ env __arg@ ->@]@ ";
      reduce_field curr_unit fmt ct "__arg";
      fprintf fmt ")@]"
    | _ ->
      eprintf
        "Only variables, named types and tuples thereof allowed as type \
         constructor arguments@.";
      exit 1
  in
  let pp_args fmt = function
    | [] -> ()
    | args ->
      fprintf fmt "@ %a" (pp_print_list ~pp_sep:pp_print_space arg_reducer) args
  in
  fprintf fmt "@[<hov 2>%a%a@]" (reducer_call curr_unit) path pp_args ct_list

and reduce_tuple curr_unit types values fmt () =
  let first = ref true in
  List.iter2
    (fun ct vn ->
      if !first
      then (
        first := false;
        reduce_field curr_unit fmt ct vn)
      else (
        fprintf fmt "@;@[<hov 2>|>@ fun x -> reducer.__combine x@ (";
        reduce_field curr_unit fmt ct vn;
        fprintf fmt ")@]"))
    types
    values

and destruct_and_reduce_tuple curr_unit fmt types value_name =
  let values = List.mapi (fun i _ -> sprintf "_%d" i) types in
  fprintf
    fmt
    "@[@[<hov 2>let@ %a@ =@ %s@ in@]@ %a@]"
    (pp_print_list ~pp_sep:comma pp_print_string)
    values
    value_name
    (reduce_tuple curr_unit types values)
    ()

and reduce_field curr_unit fmt ct value_name =
  match ct.ctyp_desc with
  | Ttyp_any | Ttyp_arrow _ -> assert false
  | Ttyp_var s -> fprintf fmt "reduce_'%s@ env %s" s value_name
  | Ttyp_tuple types -> destruct_and_reduce_tuple curr_unit fmt types value_name
  | Ttyp_constr (path, _, ct_list) ->
    fprintf
      fmt
      "@[<hov 2>%a@ env %s@]"
      (ty_constr_reducer curr_unit)
      (path, ct_list)
      value_name
  | Ttyp_object (_, _) -> assert false
  | Ttyp_class (_, _, _) -> assert false
  | Ttyp_alias (_, _) -> assert false
  | Ttyp_variant (_, _, _) -> assert false
  | Ttyp_poly ([], ct) -> reduce_field curr_unit fmt ct value_name
  | Ttyp_poly (_, _) -> assert false
  | Ttyp_package _ -> assert false
  | Ttyp_open (_, _, _) -> assert false
;;

let record_reducer_body curr_unit fmt rec_name lbls =
  let pp_field first fmt ld =
    if not first
    then fprintf fmt "@;@[<hov 2>|>@ fun x -> reducer.__combine x@ (";
    reduce_field
      curr_unit
      fmt
      ld.ld_type
      (sprintf
         "%s.%s%s"
         rec_name
         (if List.mem curr_unit opened then "" else sprintf "%s." curr_unit)
         ld.ld_name.txt);
    if not first then fprintf fmt ")@]"
  in
  List.iteri (fun i ld -> pp_field (i = 0) fmt ld) lbls
;;

let reducer_of_record curr_unit fmt rec_name lbls =
  record_reducer_body curr_unit fmt rec_name lbls
;;

let reduce_constructor_decl curr_unit fmt cd =
  let constr_longname =
    if List.mem curr_unit opened
    then cd.cd_name.txt
    else sprintf "%s.%s" curr_unit cd.cd_name.txt
  in
  let pp_pattern fmt () =
    let pp_args fmt = function
      | Cstr_tuple [] -> ()
      | Cstr_record _ -> fprintf fmt "@ _arg"
      | Cstr_tuple lst ->
        fprintf
          fmt
          "@ (%a)"
          (pp_print_list ~pp_sep:comma pp_print_string)
          (List.mapi (fun i _ -> sprintf "_arg_%d" i) lst)
    in
    fprintf fmt "@[<hov 2>%s%a@]" constr_longname pp_args cd.cd_args
  in
  let pp_body fmt () =
    match cd.cd_args with
    | Cstr_tuple [] -> fprintf fmt "reducer.__init"
    | Cstr_tuple cts ->
      fprintf
        fmt
        "%a"
        (reduce_tuple
           curr_unit
           cts
           (List.mapi (fun i _ -> sprintf "_arg_%d" i) cts))
        ()
    | Cstr_record lbls -> record_reducer_body curr_unit fmt "_arg" lbls
  in
  fprintf fmt "%a@ ->@ %a" pp_pattern () pp_body ()
;;

let reducer_of_variant curr_unit fmt value_name cds =
  fprintf fmt "match %s with" value_name;
  List.iter
    (fprintf fmt "@;@[<hov 2>| %a@]" (reduce_constructor_decl curr_unit))
    cds
;;

let reducer_of_alias curr_unit fmt name ct =
  match ct.ctyp_desc with
  | Ttyp_constr (path, _, args) ->
    fprintf
      fmt
      "@[<hov 2>%a@ env@ %s@]"
      (ty_constr_reducer curr_unit)
      (path, args)
      name
  | Ttyp_tuple types -> destruct_and_reduce_tuple curr_unit fmt types name
  | _ ->
    eprintf "unexpected manifest for %s.%s@." curr_unit name;
    exit 1
;;

let reducer_of_td fmt ((decl_unit, ty_name) as id) td =
  let vars = vars_of_params id td.typ_params in
  fprintf
    fmt
    "@[<v 2>@[<hov 2>let@ reduce_%s@ reducer%a@ env@ %s@ =@]@ "
    (reducer_name decl_unit ty_name)
    (pp_print_list (fun fmt -> fprintf fmt "@ reduce_'%s"))
    vars
    ty_name;
  (match td.typ_kind with
   | Ttype_record lbls -> reducer_of_record decl_unit fmt ty_name lbls
   | Ttype_variant cds -> reducer_of_variant decl_unit fmt ty_name cds
   | Ttype_abstract ->
     reducer_of_alias decl_unit fmt ty_name (Option.get td.typ_manifest)
   | _ -> ());
  fprintf fmt "@;@]@\n"
;;

let binding_of_td fmt (unit_name, type_name) _ =
  let reducer_name = reducer_name unit_name type_name in
  fprintf fmt "@[<hov 2>@ %s =@ reduce_%s@]@;;" reducer_name reducer_name
;;

let () =
  Decls.register ();
  List.iter (printf "open %s@\n") opened;
  print_newline ();
  printf "@[<v 2>type (%s, %s) reducer = {" env_var res_var;
  printf "@;__init : %s;" res_var;
  printf "@;__combine : %s -> %s -> %s;" res_var res_var res_var;
  Decls.iter (reducer_field_of_td std_formatter);
  printf "@]@;}@\n";
  printf "@\n";
  printf "@[<v 2>open struct[@ocaml.warning \"-27\"]@\n";
  printf
    "let ret_init (r : ('env, 'res) reducer) (_ : 'env) _ : 'res = r.__init@\n";
  printf
    "let reduce_option reducer f env = function None -> reducer.__init | Some \
     x -> f env x@\n";
  printf "let reduce_list reducer f env l =@\n";
  printf
    "  List.fold_left (fun acc x -> reducer.__combine acc (f env x)) \
     reducer.__init l@\n\
     @\n";
  Decls.iter (reducer_of_td std_formatter);
  printf "@]@,end@\n";
  printf "@[<v 2>let mk_default_reducer __init __combine =@;{";
  printf " __init@;;";
  printf " __combine@;;";
  Decls.iter (binding_of_td std_formatter);
  printf "@;}@]@."
;;
