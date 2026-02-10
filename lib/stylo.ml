open Ocaml_syntax

type _ input_kind =
  | Impl : Parsetree.structure input_kind
  | Intf : Parsetree.signature input_kind

type 'a input = {
  fname : string;
  start_line : int;
  source : string;
  kind : 'a input_kind;
}

module Check = struct
  open Ast_checker

  let retokenisation tokens_lazy =
    if !Config.check_retokenisation then (
      Lazy.force tokens_lazy
      |> Tokenisation_check.Ordering.ensure_preserved
    )

  let normalization_kept_comments tokens_before tokens_after =
    if !Config.check_normalization_kept_comments then (
      let tokens_before = Lazy.force tokens_before in
      Tokenisation_check.Comments_comparison.same_number
        tokens_before tokens_after
    )

  let same_ast (type a) { fname; start_line; kind; source = input } output =
    if !Config.check_same_ast then (
      let impl =
        match (kind : a input_kind) with
        | Impl -> true
        | Intf -> false
      in
      Oxcaml_checker.check_same_ast ~fname ~start_line ~impl input output
    )

end

module Pipeline = struct
  let parse (type a) (input : a input) : a =
    let lb = Lexing.from_string input.source in
    Location.init lb ~lnum:input.start_line input.fname;
    match input.kind with
    | Impl -> Parse.implementation lb
    | Intf -> Parse.interface lb

  let normalize (type a) (kind : a input_kind) (cst : a) : a =
    match kind with
    | Impl -> Normalize.structure cst
    | Intf -> Normalize.signature cst

  let tokens_of_tree (type a) (kind : a input_kind) (cst : a) : Tokens.seq =
    match kind with
    | Impl -> Tokens_of_tree.structure cst
    | Intf -> Tokens_of_tree.signature cst

  let build_doc (type a) (kind : a input_kind) (cst : a) : Document.t =
    match kind with
    | Impl -> Print.Structure.pp_implementation cst
    | Intf -> Print.Signature.pp_interface cst

  let print_doc doc =
    Document.Print.to_string ~width:!Config.width doc

  let run ({ kind; _ } as input) =
    let cst = parse input in
    let tokens_pre_normalize = lazy (tokens_of_tree kind cst) in
    Check.retokenisation tokens_pre_normalize;
    let cst = normalize kind cst in
    let tokens_post_normalize = tokens_of_tree kind cst in
    Check.normalization_kept_comments tokens_pre_normalize tokens_post_normalize;
    let document =
      build_doc kind cst
      |> Comments.Insert.from_tokens tokens_post_normalize
    in
    let output = print_doc document in
    Check.same_ast input output;
    output
end

let style_file kind fname =
  let source = In_channel.(with_open_text fname input_all) in
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
  then Pipeline.run { fname; start_line; source; kind = Intf }
  else Pipeline.run { fname; start_line; source; kind = Impl }
