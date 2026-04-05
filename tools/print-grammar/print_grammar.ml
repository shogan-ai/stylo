let inlined_terms = ref []

let () = if Array.length Sys.argv <> 2 || not (Sys.file_exists Sys.argv.(1)) then (
    if Array.length Sys.argv = 2 then
      Printf.eprintf "File %S does not exist.\n\n" Sys.argv.(1);
    Printf.eprintf
      "Usage: %s <grammar.cmly>\n\n\
       Print clean rules without semantic actions\n"
      Sys.argv.(0);
    exit 1
  )

module G = MenhirSdk.Cmly_read.Read(struct
    let filename = Sys.argv.(1)
  end)

open G.Surface

let errorf fmt =
  Printf.kfprintf (fun _ -> exit 1) stderr fmt

(* Reverse engineer anonymous producers.
   Minor bug in Menhir: the grammar is serialized post lambda-lifting. It shouldn't. *)

let anonymous = Hashtbl.create 7

let () =
  List.iter begin fun (name, def) ->
    if String.starts_with ~prefix:"__anonymous_" name then
      Hashtbl.add anonymous name def
  end (Syntax.rules before_expansion)

let resolve_anonymous param =
  (* Sanity checks *)
  let valid_app param arg =
    match Parameter.desc arg with
    | App _ | Anonymous _ -> false
    | Var arg -> String.equal param arg
  in
  let valid_rule rule args =
    List.for_all2 valid_app (Rule.parameters rule) args
  in
  (* Check if anonymous *)
  let desc = Parameter.desc param in
  match desc with
  | Var var ->
    begin match Hashtbl.find_opt anonymous var with
      | None -> desc
      | Some rule ->
        assert (valid_rule rule []);
        Anonymous (Rule.branches rule)
    end
  | App (name, args) ->
    begin match Hashtbl.find_opt anonymous name with
      | None -> desc
      | Some rule ->
        assert (valid_rule rule args);
        Anonymous (Rule.branches rule)
    end
  | Anonymous _ -> desc

(* Lift to an internal syntax for custom inlining *)

type term =
  | App of string * term list
  | Var of string
  | Anonymous of branch list

and branch = term list

let rec lift_parameter param =
  match resolve_anonymous param with
  | App (name, args) ->
    App (name, List.map lift_parameter args)
  | Var name -> Var name
  | Anonymous branches ->
    Anonymous (List.map lift_branch branches)

and lift_producer prod = lift_parameter (Producer.symbol prod)

and lift_branch branch = List.map lift_producer (Branch.producers branch)

let lift_rule rule =
  (Rule.parameters rule, List.map lift_branch (Rule.branches rule))

let rules =
  let table = Hashtbl.create 7 in
  List.iter (fun (name, def) ->
      Hashtbl.add table name (lift_rule def)
    ) (Syntax.rules before_expansion);
  table

(* Inline matching terms *)

let need_inlining =
  let table = Hashtbl.create 7 in
  let is_operator = function
    | ',' | '(' | ')' -> true
    | _ -> false
  in
  let parse_term term =
    if String.exists (function
        | ' ' -> errorf "inlining term: ' ' not allowed in %S\n" term
        | c -> is_operator c
      ) term
    then
      let position = ref 0 in
      let tokens = ref [] in
      let flush pos =
        if !position <> pos then
          tokens := String.sub term !position (pos - !position) :: !tokens;
        position := pos + 1
      in
      for i = 0 to String.length term - 1 do
        if is_operator term.[i] then (
          flush i;
          tokens := String.make 1 term.[i] :: !tokens
        )
      done;
      let malformed () = errorf "inlining term: %S is malformed\n" term in
      if !position <> String.length term then
        malformed ();
      let rec parse_param = function
        | ("," | "(" | ")") :: _ -> malformed ()
        | name :: "(" :: rest ->
          let params, rest = parse_params rest in
          (App (name, params), rest)
        | name :: rest ->
          (Var name, rest)
        | [] -> malformed ()
      and parse_params = function
        | "," :: rest ->
          let param, rest = parse_param rest in
          let params, rest = parse_params rest in
          (param :: params, rest)
        | ")" :: rest ->
          ([], rest)
        | _ :: _ -> malformed ()
        | [] -> malformed ()
      in
      match parse_param (List.rev !tokens) with
      | term, [] -> term
      | _, _ -> malformed ()
    else Var term
  in
  List.iter (fun term -> Hashtbl.add table (parse_term term) ()) !inlined_terms;
  fun term -> Hashtbl.mem table term

(*let rec subst_and_inline lookup env term =
  let term =
    if need_inlining term then
      lookup term
    else
      term
  in
  match term with
  | App (name, args) ->
    let args = List.map (subst_and_inline lookup env) args in
    App (name, args)
  | Var name ->
    begin match List.assoc_opt name env with
      | None -> Var name
      | Some ([], def) -> def
      | Some (_, _) ->
        Printf.eprintf "invalid kind: %s\n" name;
        exit 1
    end
  | Anonymous branches ->
    List.map (List.map (subst_and_inline lookup env)) branches)*)

let inline lookup =
  let rec aux term =
    let term =
      if need_inlining term
      then lookup term
      else term
    in
    match term with
    | Var name -> Var name
    | App (name, args) ->
      let args = List.map aux args in
      App (name, args)
    | Anonymous branches ->
      Anonymous (List.map (List.map aux) branches)
  in
  aux

let rec subst env = function
  | Var name ->
    begin match List.assoc_opt name env with
      | None -> Var name
      | Some ([], def) -> def
      | Some (_, _) ->
        Printf.eprintf "invalid kind: %s\n" name;
        exit 1
    end
  | App (head, args) ->
    let args = List.map (subst env) args in
    begin match List.assoc_opt head env with
      | None -> App (head, args)
      | Some (params, def) ->
        let env = List.map2 (fun param arg -> (param, ([], arg))) params args in
        subst env def
    end
  | Anonymous branches ->
    Anonymous (List.map (List.map (subst env)) branches)

let rec traverse_seq f = function
  | [] -> [[]]
  | x :: xs ->
    let x' = f x in
    let xs' = traverse_seq f xs in
    List.concat_map (fun x' -> List.concat_map (fun xs' -> x' @ xs') xs') x'

let traverse_branches f branches =
    List.concat_map (traverse_seq f) branches

let rec inline env (term : term) =
  let branches =
    if need_inlining term then
      match term with
      | Anonymous _ -> assert false
      | Var name ->
        if List.mem_assoc name env then
          [[term]]
        else
          begin match Hashtbl.find_opt rules name with
            | None -> errorf "unbound variable %s\n" name
            | Some ([], def) -> traverse_branches (inline []) def
            | Some (_, _) -> errorf "invalid arity %s\n" name
          end
      | App (head, args) ->
        if List.mem_assoc head env then
          [[term]]
        else
          begin match Hashtbl.find_opt rules head with
            | None -> errorf "unbound rule %s\n" head
            | Some (params, def) ->
              let env = List.map2 (fun param arg -> (param, ([], arg))) params args in
              traverse_branches (inline env) def
          end
    else [[term]]
  in
  traverse_branches (function
      | Var _ | App _ as term -> [[term]]
      | Anonymous branches ->
        traverse_branches (inline env) branches
    ) branches

(* Traverse grammar *)

(* Print *)

let rec print_parameter param =
  match resolve_anonymous param with
  | Var var -> print_string var
  | App (name, args) ->
    print_string name;
    print_char '(';
    List.iter print_parameter args;
    print_char ')';
  | Anonymous branches ->
    print_char '(';
    List.iteri print_inline_branch branches;
    print_char ')';

and print_producer prod =
  print_char ' ';
  print_parameter (Producer.symbol prod)

and print_inline_branch i branch =
  if i > 0 then print_string " | ";
  List.iter print_producer (Branch.producers branch)

let print_branch branch =
  print_string "|";
  List.iter print_producer (Branch.producers branch);
  print_newline ()

let print_rule (name, def) =
  if not (Hashtbl.mem anonymous name) then (
    print_string name;
    begin match Rule.parameters def with
      | [] -> ()
      | params ->
        print_char '(';
        List.iteri begin fun i n ->
          if i > 0 then print_char ',';
          print_string n
        end params;
        print_char ')';
    end;
    print_string ":\n";
    List.iter print_branch (Rule.branches def);
    print_newline ()
  )

let () = List.iter print_rule (Syntax.rules before_expansion)
