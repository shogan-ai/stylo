open MenhirSdk

module G = Cmly_read.Read(struct let filename = Sys.argv.(1) end)

(***********)
(* Helpers *)
(***********)

let prepend heads tails =
  List.concat_map heads ~f:(fun head ->
    List.map tails ~f:(List.cons head)
  )

let rec expand_map ~f = function
  | [] -> [[]]
  | x :: xs -> prepend (f x) (expand_map ~f xs)

module StringMap = Map.Make(String)
module IntMap = Map.Make(Int)

(*******************)
(* Pseudo-inlining *)
(*******************)

(* A branch is turned into a tree instead of a (flat) production
   N.B. we know there is no cycle since menhir already checked for them. *)

open G.Surface

let nb_rules = ref 0
let nb_branches = ref 0

let rules =
  List.fold_left (Syntax.rules before_inlining) ~f:(fun m (name, rule) ->
    incr nb_rules;
    nb_branches := !nb_branches + List.length (Rule.branches rule);
    StringMap.add m ~key:name ~data:rule
  ) ~init:StringMap.empty

type symbol = name

type expanded_producer =
  | Regular of symbol
  | Inlined of expanded_branch
  | (* tokens are not retrieved in the action, so we actually inline in the
       Symbol_tree *)
    Inlined_inlinable of expanded_branch

and expanded_branch = expanded_producer list
and post_expansion = {
  tokens_are_accessed: bool;
  producers: expanded_branch;
}

let expanded_inline_symbols : (name, post_expansion list) Hashtbl.t =
  Hashtbl.create 42

let tokens_are_accessed =
  let accessor = Str.regexp "Tokens.of_production" in
  fun action ->
    let source = G.Action.expr action in
    try ignore (Str.search_forward accessor source 0); true
    with Not_found -> false

let rec expand_branch b =
  let branches =
    expand_map (Branch.producers b) ~f:(fun p ->
      let name = Producer.symbol p in
      match StringMap.find name rules with
      | rule when Rule.inline rule ->
        List.map (expand_symbol name rule) ~f:(fun b ->
          if b.tokens_are_accessed
          then Inlined b.producers
          else Inlined_inlinable b.producers
        )
      | _ | exception Not_found ->
        [Regular name]
    )
  in
  let tokens_are_accessed = tokens_are_accessed (Branch.action b) in
  List.map branches ~f:(fun producers -> { tokens_are_accessed; producers })

and expand_symbol name rule =
  match Hashtbl.find expanded_inline_symbols name with
  | branches -> branches
  | exception Not_found ->
    let branches = do_expand_symbol rule in
    Hashtbl.add expanded_inline_symbols ~key:name ~data:branches;
    branches

and do_expand_symbol rule =
  List.concat_map (Rule.branches rule) ~f:expand_branch

let expanded =
  StringMap.filter_map rules ~f:(fun _name rule ->
    if Rule.inline rule
    then None
    else Some (List.map (do_expand_symbol rule) ~f:(fun pe -> pe.producers))
  )

(* Matching productions to their associated expanded branch *)

module Prod_rhs_it = struct
  type t = G.(symbol * identifier * Attribute.t list) array * int

  let start p : t = p, 0

  let ended (arr, i) = i >= Array.length arr

  let next (arr, i) = (arr, i + 1)

  let name (arr, i) =
    let sym, _, _ = arr.(i) in
    G.symbol_name sym
end

let rec matches p_rhs expanded_branch =
  match expanded_branch with
  | [] -> Prod_rhs_it.ended p_rhs
  | Regular id :: expanded_branch_tail ->
    not (Prod_rhs_it.ended p_rhs) &&
    Prod_rhs_it.name p_rhs = id &&
    matches (Prod_rhs_it.next p_rhs) expanded_branch_tail
  | (Inlined producers | Inlined_inlinable producers) :: expanded_branch_tail ->
    matches p_rhs (producers @ expanded_branch_tail)

let rec extract_branch prod_rhs = function
  | [] -> assert false
  | x :: xs ->
    if matches (Prod_rhs_it.start prod_rhs) x then
      x, xs
    else
      let matched, rest = extract_branch prod_rhs xs in
      matched, x :: rest

let extract prod expanded_symbols =
  let name = G.Nonterminal.name (G.Production.lhs prod) in
  let branches = StringMap.find name expanded_symbols in
  let branch, remaining = extract_branch (G.Production.rhs prod) branches in
  let remaining_expanded_symbols =
    if remaining = [] then
      StringMap.remove name expanded_symbols
    else
      StringMap.add expanded_symbols ~key:name ~data:remaining
  in
  branch, remaining_expanded_symbols

let prod_to_branch =
  let res, name_to_branches =
    G.Production.fold (fun p (prod_to_branch, name_to_branches as acc) ->
      match G.Production.kind p with
      | `START -> acc (* ignore internal start symbols *)
      | `REGULAR ->
        let branch, name_to_remaining_branches = extract p name_to_branches in
        IntMap.add prod_to_branch ~key:(G.Production.to_int p) ~data:branch,
        name_to_remaining_branches
    ) (IntMap.empty, expanded)
  in
  assert (StringMap.is_empty name_to_branches);
  res

let symbols = Hashtbl.create (G.Nonterminal.count + G.Terminal.count)
let () =
  G.Nonterminal.iter
    (fun s -> Hashtbl.add symbols ~key:(G.Nonterminal.name s) ~data:true);
  G.Terminal.iter
    (fun s -> Hashtbl.add symbols ~key:(G.Terminal.name s) ~data:false)

open Printf

let rec string_of_branch = function
  | [] -> ""
  | Regular s :: b ->
    let nonterm = Hashtbl.find symbols s in
    sprintf "%s; %s" (if nonterm then "Nonterminal" else "Terminal")
      (string_of_branch b)
  | Inlined bi :: b ->
    sprintf "Inlined [%s]; %s" (string_of_branch bi) (string_of_branch b)
  | Inlined_inlinable bi :: b ->
    sprintf "%s %s" (string_of_branch bi) (string_of_branch b)

let interp = "Ocaml_syntax.Parser.MenhirInterpreter"

let () =
  printf "type t = node list\n";
  printf "and node = Terminal | Nonterminal | Inlined of t\n\n";
  printf "let rec pp_node ppf = Format.(function\n";
  printf "  | Terminal -> pp_print_string ppf {|terminal|}\n";
  printf "  | Nonterminal -> pp_print_string ppf {|nonterminal|}\n";
  printf "  | Inlined t -> pp ppf t)\n\n";
  printf "and pp ppf lst =\n\n";
  printf "  let open Format in\n";
  printf "  fprintf ppf {|{@[@ %%a@}|}\n";
  printf "    (pp_print_list ~pp_sep:(fun ppf () -> fprintf ppf {|,@ |})\n";
  printf "       pp_node) lst\n";
  printf "let symbols_of_prod = [|\n";
  IntMap.iter prod_to_branch ~f:(fun ~key:_ ~data ->
    (* We rely on the ordering of IntMap.iter *)
    printf "  [ %s];\n" (string_of_branch data)
  );
  printf "|]\n\n";
  printf "let of_production p =\n";
  printf "  symbols_of_prod.(%s.production_index p)\n" interp

let () =
  printf "\n";
  printf "let xsym_to_string : %s.xsymbol -> string = function\n" interp;
  G.Nonterminal.(iter (fun nt ->
    if G.Nonterminal.kind nt = `START then () else
    let s = G.Nonterminal.mangled_name nt in
    printf "  | X N N_%s -> %S\n" s s
  ));
  printf "  | _ -> \"Terminal\"";
  printf "%!"
