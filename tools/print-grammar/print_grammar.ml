(* Refs to accumulate flags as we parse the command line *)
let entrypoints = ref []
let inlined_terms = ref []

let grammar = ref ""

let () =
  (* This will collect positional (anonymous) arguments *)

  (* Define the specification *)
  (* The tuple format is: (flag_name, action, positional_action) *)
  (* For flags with parameters, the action is a function: string -> unit *)
  let spec = [
    ("--entrypoint", Arg.String (fun s -> entrypoints := s :: !entrypoints), "<symbol> Entrypoint to process");
    ("-e", Arg.String (fun s -> entrypoints := s :: !entrypoints), "<symbol> Entrypoint to process");
    ("-i", Arg.String (fun s -> inlined_terms := s :: !inlined_terms), "<term> Term to inline");
    ("--inline", Arg.String (fun s -> inlined_terms := s :: !inlined_terms), "<term> Term to inline");
  ] in
  (* Arg.parse takes: error_handler, spec, argv, positional_handler *)
  let usage_msg = "print_grammar [--inline term] [--entrypoint symbol] <grammar.cmly>]" in
  Arg.parse spec (fun s -> if !grammar = "" then grammar := s else
                     raise (Arg.Bad (Printf.sprintf "input specified twice (%S and %S)" s !grammar)))
    usage_msg;
  if !grammar = "" then (
    Arg.usage spec (usage_msg ^ "\nExpecting a grammar (.cmly) file\n");
    exit 1
  )

module G = MenhirSdk.Cmly_read.Read(struct
    let filename = !grammar
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

(* Internal syntax for custom inlining *)

type term =
  | App of string
           * bool (* should we inline this term? *)
           * term list
  | Anonymous of branch list

and branch = term list

module TermSet = Set.Make(struct
    type t = term
    let rec compare t1 t2 =
      match t1, t2 with
      | App _, Anonymous _ -> +1
      | Anonymous _, App _ -> -1
      | App (h1, _, a1), App (h2, _, a2) ->
        let c = String.compare h1 h2 in
        if c <> 0 then c else
          List.compare compare a1 a2
      | Anonymous br1, Anonymous br2 ->
        List.compare (List.compare compare) br1 br2

  end)
module StringSet = Set.Make(String)

(* Auto inline identity rules *)

let () =
  List.iter
    (fun (name, def) ->
       if
         begin match Rule.parameters def with
           | [var] ->
             begin match Rule.branches def with
               | [branch] ->
                 begin match Branch.producers branch with
                   | [prod] ->
                     begin match Parameter.desc (Producer.symbol prod) with
                       | Var var' -> (var = var')
                       | _ -> false
                     end
                   | _ -> false
                 end
               | _ -> false
             end
           | _ -> false
         end
       then
         inlined_terms := name :: !inlined_terms
    )
    (Syntax.rules before_expansion)

(* Parse inlining specifications *)

let need_inlining =
  let names = ref StringSet.empty in
  let terms = ref TermSet.empty in
  let is_ws = function
    | ' ' | '\t' | '\n' -> true
    | _ -> false
  in
  let is_operator = function
    | ',' | '(' | ')' -> true
    | _ -> false
  in
  let is_meta c = is_ws c || is_operator c in
  let parse_spec term =
    if String.exists is_meta term then
      let position = ref 0 in
      let tokens = ref [] in
      let flush pos =
        if !position <> pos then
          tokens := String.sub term !position (pos - !position) :: !tokens;
        position := pos + 1
      in
      for i = 0 to String.length term - 1 do
        if is_ws term.[i] then
          flush i
        else if is_operator term.[i] then (
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
          (App (name, false, params), rest)
        | name :: rest ->
          (App (name, false, []), rest)
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
      let rec parse_terms acc = function
        | [] -> acc
        | tokens ->
          let (term, ("," :: rest | rest)) = parse_param tokens in
          parse_terms (term :: acc) rest
      in
      parse_terms [] (List.rev !tokens)
    else [App (term, false, [])]
  in
  List.iter (fun spec ->
      List.iter (fun term ->
          match term with
          | App (name, _, []) ->
            names := StringSet.add name !names
          | term -> terms := TermSet.add term !terms
        ) (parse_spec spec)
    ) !inlined_terms;
  let names = !names in
  let terms = !terms in
  function
  | App (name, _, _) as term ->
    StringSet.mem name names || TermSet.mem term terms
  | Anonymous _ -> false

let rec lift_parameter param =
  let mk_app head args =
    let result = App (head, false, args) in
    if need_inlining result
    then App (head, true, args)
    else result
  in
  match resolve_anonymous param with
  | App (head, args) -> mk_app head (List.map lift_parameter args)
  | Var name -> mk_app name []
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

(* Inline terms *)

let rec traverse_seq (f : term -> term list list) : term list -> term list list = function
  | [] -> [[]]
  | x :: xs ->
    let fx = f x and fxs = traverse_seq f xs in
    List.concat_map (fun x' -> List.map (fun xs' -> x' @ xs') fxs) fx

let traverse_branches f branches =
  List.concat_map (traverse_seq f) branches

let subst env =
  let rec aux = function
  | Anonymous branches ->
    Anonymous (List.map (List.map aux) branches)
  | App (head, inline, args) ->
    begin match List.assoc_opt head env with
      | None -> App (head, inline, List.map aux args)
      | Some (Anonymous _ as term') ->
        assert (args = []);
        term'
      | Some (App (head', inline', args') as term') ->
        match args with
        | [] -> term'
        | args -> App (head', inline || inline', args' @ List.map aux args)
    end
  in
  aux

let rec inline import abstract = function
  | App (head, do_inline, args) when do_inline && not (List.mem head abstract) ->
    begin match Hashtbl.find_opt rules head with
      | None -> assert false
      | Some (parameters, branches) ->
        let env = List.combine parameters args in
        let branches = List.map (List.map (subst env)) branches in
        traverse_branches (inline import abstract) branches
    end
  | Anonymous branches ->
    inline_branches import abstract branches
  | term ->
    [[inline_args import abstract term]]

and inline_args import abstract = function
  | Anonymous [[term]] ->
    inline_args import abstract term
  | Anonymous branches ->
    Anonymous (inline_branches import abstract branches)
  | App (head, do_inline, args) when do_inline && not (List.mem head abstract) ->
    begin match Hashtbl.find_opt rules head with
      | None -> assert false
      | Some (parameters, branches) ->
        let env = List.combine parameters args in
        let branches = List.map (List.map (subst env)) branches in
        match traverse_branches (inline import abstract) branches with
        | [[term]] ->
          term
        | branches -> Anonymous branches
    end
  | App (head, do_inline, args) ->
    if not (List.mem head abstract) then
      import head;
    App (head, do_inline, List.map (inline_args import abstract) args)

and inline_branches import abstract branches =
  traverse_branches (inline import abstract) branches

(* Import definitions starting from entrypoints *)

let definitions = ref []

let tokens = Syntax.tokens before_expansion

let rec import_rule =
  let imported = Hashtbl.create 7 in
  function
  | "error" -> ()
  | name ->
    if not (Hashtbl.mem imported name) then (
      Hashtbl.add imported name ();
      match Hashtbl.find_opt rules name with
      | Some (parameters, branches) ->
        let branches = inline_branches import_rule parameters branches in
        definitions := (name, parameters, branches) :: !definitions
      | None ->
        if not (List.mem_assoc name tokens) then
          errorf "Unknown rule %s.\n" name
    )

let () = List.iter import_rule
    (if !entrypoints = [] then List.rev start_symbols else !entrypoints)

(* Print *)

let rec print_parameter = function
  | App (name, _, []) -> print_string name
  | App (name, _, arg :: args) ->
    print_string name;
    print_char '(';
    print_parameter arg;
    List.iter (fun arg' -> print_char ','; print_parameter arg') args;
    print_char ')';
  | Anonymous [branch] ->
    print_inline_branch 0 branch
  | Anonymous branches ->
    print_char '(';
    List.iteri print_inline_branch branches;
    print_char ')';

and print_produceri i prod =
  if i > 0 then
    print_char ' ';
  print_parameter prod

and print_producer prod =
  print_char ' ';
  print_parameter prod

and print_inline_branch i branch =
  if i > 0 then print_string " | ";
  List.iteri print_produceri branch

let print_branch branch =
  print_string "|";
  List.iter print_producer branch;
  print_newline ()

let print_rule (name, parameters, branches) =
  print_string name;
  begin match parameters with
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
  List.iter print_branch branches;
  print_newline ()

let () = List.iter print_rule !definitions
