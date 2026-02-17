open Ocaml_syntax

let (let*) = Result.bind

module Cst = Ocaml_syntax.Parsetree
module Ast = Oxcaml_frontend.Parsetree

type (_, _) input_kind =
  | Impl : (Cst.structure, Ast.structure) input_kind
  | Intf : (Cst.signature, Ast.signature) input_kind

type ('a, 'b) input = {
  fname : string;
  start_line : int;
  source : string;
  kind : ('a, 'b) input_kind;
}

module Debug = Ast_checker.Debug

module Check = struct
  open Ast_checker

  type (_, _) checker_input =
    | Ast : ('cst, 'ast) input * 'ast -> ('cst, 'ast) checker_input
    | Cst : ('cst, 'ast) input * 'cst -> ('cst, 'ast) checker_input

  let to_cst_kind
    : type cst ast. (cst, ast) input_kind -> cst Cst_checker.input_kind =
    function
    | Impl -> Impl
    | Intf -> Intf

  let to_ast_kind
    : type cst ast. (cst, ast) input_kind -> ast Oxcaml_checker.input_kind =
    function
    | Impl -> Impl
    | Intf -> Intf

  let make_cst_input { fname; start_line; kind; source = _ } source =
    { Cst_checker.fname; start_line; source; kind = to_cst_kind kind }

  let make_ast_input { fname; start_line; kind; source = _ } source =
    { Oxcaml_checker.fname; start_line; source; kind = to_ast_kind kind }

  let same_ast checker_input output =
    if not !Config.check_same_ast then Ok () else (
      match checker_input with
      | Ast (input, input_ast) ->
        Oxcaml_checker.check_same_ast input_ast (make_ast_input input output)
      | Cst (input, input_cst) ->
        Cst_checker.check_same_ast input_cst (make_cst_input input output)
    )

  open Tokenisation_check

  let retokenisation tokens_lazy =
    if not !Config.check_retokenisation then Ok () else (
      let* tokens = Lazy.force tokens_lazy in
      Ordering.ensure_preserved tokens
    )

  let normalization_kept_comments tokens_before tokens_after =
    if not !Config.check_normalization_kept_comments ||
       tokens_before == tokens_after
    then Ok ()
    else (
      let* tokens_before = Lazy.force tokens_before in
      let* tokens_after = Lazy.force tokens_after in
      Comments_comparison.same_number tokens_before tokens_after
    )

  type error = [
    | Ordering.error
    | Comments_comparison.error
    | Ast_checker.Errors.t
  ]
end

module Pipeline = struct
  let parse (type cst ast) (input : (cst, ast) input) : (cst, _) result =
    let lb = Lexing.from_string input.source in
    Location.init lb ~lnum:input.start_line input.fname;
    try
      Ok (
        match input.kind with
        | Impl -> Parse.implementation lb
        | Intf -> Parse.interface lb
      )
    with exn ->
      Error (`Input_parse_error (Ast_checker.Errors.Stylo's,  exn))

  let normalize (type cst ast) (kind: (cst, ast) input_kind) (cst: cst) : cst =
    match kind with
    | Impl -> Normalize.structure cst
    | Intf -> Normalize.signature cst

  let tokens_of_tree (type cst ast) (kind : (cst, ast) input_kind) (cst : cst)
    : (Tokens.seq, _) result =
    match kind with
    | Impl -> Tokens_of_tree.structure cst
    | Intf -> Tokens_of_tree.signature cst

  let build_doc (type cst ast) (kind : (cst, ast) input_kind) (cst : cst)
    : Document.t =
    match kind with
    | Impl -> Print.Structure.pp_implementation cst
    | Intf -> Print.Signature.pp_interface cst

  let print_doc doc =
    Document.Print.to_string ~width:!Config.width doc

  let run ?normalize:(run_normalize=true) ({ kind; _ } as input) =
    let* cst = parse input in
    let tokens_pre_normalize = lazy (tokens_of_tree kind cst) in
    let* () = Debug.dump_tokens input.fname ~src:Parser tokens_pre_normalize in
    let* () = Check.retokenisation tokens_pre_normalize in
    let* cst, tokens_post_normalize, ast_for_checker =
      if not run_normalize
      then Ok (cst, tokens_pre_normalize, Check.Cst (input, cst))
      else (
        (* we normalize only if the source parses with the upstream parser *)
        let input_for_oxchecker = Check.make_ast_input input input.source in
        match Ast_checker.Oxcaml_checker.parse input_for_oxchecker with
        | Error e ->
          if !Config.check_same_ast
          then Error e (* might as well fail early *)
          else Ok (cst, tokens_pre_normalize, Check.Cst (input, cst))
        | Ok ast ->
          let normalized = normalize kind cst in
          let tokens =
            (* No need to suspend, we know those will be used. *)
            Lazy.from_val (tokens_of_tree kind normalized)
          in
          let* () = Debug.dump_tokens input.fname ~src:Normalization tokens in
          Ok (normalized, tokens, Check.Ast (input, ast))
      )
    in
    let* () =
      Check.normalization_kept_comments tokens_pre_normalize
        tokens_post_normalize
    in
    let* tokens_post_normalize = Lazy.force tokens_post_normalize in
    let* document =
      build_doc kind cst
      |> Comments.Insert.from_tokens tokens_post_normalize
    in
    let output = print_doc document in
    let* () = Check.same_ast ast_for_checker output in
    Ok output

  type error = [
    | Tokens_of_tree.Error.t
    | Check.error
    | Comments.Insert.error
  ]

  let pp_error fname : error -> unit =
    let open Ast_checker in
    let open Tokenisation_check in
    function
    | `Comments_dropped as e -> Comments_comparison.pp_error e
    | (`Reordered _ | `Incomplete_flattening _) as e -> Ordering.pp_error e
    | `Comment_insertion_error e ->
      Format.eprintf "%s: ERROR: %a@." fname
        Comments.Insert.Error.pp e
    | (`Input_parse_error _ | `Output_parse_error _ | `Ast_changed _) as e ->
      Ast_checker.Errors.pp_error e
    | (`Missing_children _ | `Extra_children _) as e ->
      Tokens_of_tree.Error.pp Format.err_formatter e
end

let style_file kind ~fname source =
  Pipeline.run { kind; fname; source; start_line = 1 }

let split_fuzzer_line entrypoint_and_src =
  let intf = String.starts_with ~prefix:"interface:" entrypoint_and_src in
  let src =
    let prefix_len =
      String.length (if intf then "interface:" else "implementation:")
    in
    String.sub entrypoint_and_src prefix_len
      (String.length entrypoint_and_src - prefix_len)
  in
  intf, src

let style_fuzzer_line ~lnum:start_line ~fname entrypoint_and_src =
  let intf, source = split_fuzzer_line entrypoint_and_src in
  if intf
  then Pipeline.run ~normalize:false { fname; start_line; source; kind = Intf }
  else Pipeline.run ~normalize:false { fname; start_line; source; kind = Impl }
