open Ocaml_syntax

let ( let* ) = Result.bind

module Debug = Ast_checker.Debug

module type Checker = sig
  type options
  type ast

  val parse
    :  options
    -> Source.t
    -> ( ast
       , Ast_checker.Errors.parser * Lexing.position * Lexing.position * exn )
         result

  val check_same_ast
    :  options
    -> ast
    -> Source.t
    -> (unit, [> Ast_checker.Errors.t ]) result
end

module Check = struct
  open Ast_checker

  module Options = struct
    type t =
      { same_ast : bool
      ; retokenisation : bool
      ; normalization_kept_comments : bool
      }

    let none =
      { same_ast = false
      ; retokenisation = false
      ; normalization_kept_comments = false
      }
    ;;
  end

  open Tokenisation_check

  let retokenisation (options : Options.t) tokens_lazy =
    if not options.retokenisation
    then Ok ()
    else (
      let* tokens = Lazy.force tokens_lazy in
      Ordering.ensure_preserved tokens)
  ;;

  let normalization_kept_comments
    (options : Options.t)
    tokens_before
    tokens_after
    =
    if not options.normalization_kept_comments || tokens_before == tokens_after
    then Ok ()
    else (
      let* tokens_before = Lazy.force tokens_before in
      let* tokens_after = Lazy.force tokens_after in
      Comments_comparison.same_number tokens_before tokens_after)
  ;;

  type error =
    [ | Ordering.error
    | Comments_comparison.error
    | Ast_checker.Errors.t
    ]
end

module type Style = sig
  type options

  val normalize : options -> Cst.t -> Cst.t

  val doc_of_cst
    :  options
    -> format_code_block:(string -> Document.t option)
    -> Cst.t
    -> Document.t

  val render_comment
    :  options
    -> format_code_block:(string -> Document.t option)
    -> start_pos:Lexing.position
    -> string
    -> Document.t
end

module Without_normalization (S : Style) = struct
  include S

  let normalize _ cst = cst
end

module Cst_checker = Ast_checker.Cst_checker

module Pipeline = struct
  let parse options (input : Source.t) : (Cst.t, _) result =
    let lb = Lexing.from_string input.source in
    Location.init lb ~lnum:input.start_line input.fname;
    try
      Ok
        (match input.kind with
         | Impl -> Structure (Parse.implementation options lb)
         | Intf -> Signature (Parse.interface options lb))
    with
    | exn ->
      Error
        (`Input_parse_error
           (Ast_checker.Errors.Stylo's, lb.lex_start_p, lb.lex_curr_p, exn))
  ;;

  let try_parse options source : Cst.t option =
    let try_parse src parse =
      let lb = Lexing.from_string src in
      try Some (parse lb) with
      | _ -> None
    in
    match try_parse source (Parse.implementation options) with
    | Some str -> Some (Structure str)
    | None ->
      Option.map
        (fun sg -> Cst.Signature sg)
        (try_parse source (Parse.interface options))
  ;;

  let tokens_of_tree : Cst.t -> (Tokens.seq, _) result = function
    | Structure str -> Tokens_of_tree.structure str
    | Signature sg -> Tokens_of_tree.signature sg
  ;;

  let print_doc ~width doc = Document.Print.to_string ~width doc

  type error =
    [ | Tokens_of_tree.Error.t
    | Check.error
    | Comments.Insert.error
    ]

  let pp_error ppf fname : error -> unit =
    let open Ast_checker in
    let open Tokenisation_check in
    function
    | `Comments_dropped as e ->
      Format.fprintf ppf "%s: %a" fname Comments_comparison.pp_error e
    | (`Reordered _ | `Incomplete_flattening _) as e -> Ordering.pp_error ppf e
    | `Comment_insertion_error e ->
      Format.fprintf ppf "%s: ERROR: %a@." fname Comments.Insert.Error.pp e
    | (`Input_parse_error _ | `Output_parse_error _ | `Ast_changed _) as e ->
      Ast_checker.Errors.pp_error ppf fname e
    | (`CST_tokens_mismatch _) as e -> Tokens_of_tree.Error.pp ppf e
  ;;
end

module Make (S : Style) (C : Checker) = struct
  type options =
    { width : int
    ; parse : Parse.Options.t
    ; checks : Check.Options.t
    ; debug : bool
    ; style : S.options
    ; checker : C.options
    }

  let rec format_code_block opts source =
    match Pipeline.try_parse opts.parse source with
    | None -> None
    | Some cst ->
      match Pipeline.tokens_of_tree cst with
      | Error _ -> None
      | Ok tokens ->
        let format_code_block = format_code_block opts in
        let doc = S.doc_of_cst opts.style ~format_code_block cst in
        let render_comment = S.render_comment opts.style ~format_code_block in
        match Comments.Insert.from_tokens ~render_comment tokens doc with
        | Error _ -> None
        | Ok doc -> Some doc
  ;;

  type base_for_ast_check =
    | Ast of C.ast
    | Cst of Cst.t

  let check_same_ast opts (input : Source.t) reference output =
    if not opts.checks.same_ast
    then Ok ()
    else (
      let output = { input with source = output } in
      match reference with
      | Ast ast -> C.check_same_ast opts.checker ast output
      | Cst cst -> Cst_checker.check_same_ast opts.parse cst output)
  ;;

  let run opts input =
    let* cst = Pipeline.parse opts.parse input in
    let tokens_pre_normalize = lazy (Pipeline.tokens_of_tree cst) in
    let* () =
      Debug.dump_tokens
        ~enabled:opts.debug
        input.fname
        ~src:Parser
        tokens_pre_normalize
    in
    let* () = Check.retokenisation opts.checks tokens_pre_normalize in
    let* cst, tokens_post_normalize, reference =
      (* we normalize only if the source is accepted by the reference checker *)
      match C.parse opts.checker input with
      | Error (src, startp, endp, exn) ->
        if opts.checks.same_ast
        then
          Error
            (`Input_parse_error (src, startp, endp, exn))
            (* might as well fail early *)
        else Ok (cst, tokens_pre_normalize, Cst cst)
      | Ok ast ->
        let normalized = S.normalize opts.style cst in
        let tokens =
          (* No need to suspend, we know those will be used. *)
          Lazy.from_val (Pipeline.tokens_of_tree normalized)
        in
        let* () =
          Debug.dump_tokens
            ~enabled:opts.debug
            input.fname
            ~src:Normalization
            tokens
        in
        Ok (normalized, tokens, Ast ast)
    in
    let* () =
      Check.normalization_kept_comments
        opts.checks
        tokens_pre_normalize
        tokens_post_normalize
    in
    let* tokens_post_normalize = Lazy.force tokens_post_normalize in
    let format_code_block = format_code_block opts in
    let* document =
      S.doc_of_cst opts.style ~format_code_block cst
      |> Comments.Insert.from_tokens
           ~render_comment:(S.render_comment opts.style ~format_code_block)
           tokens_post_normalize
    in
    let output = Pipeline.print_doc ~width:opts.width document in
    let* () = check_same_ast opts input reference output in
    Ok output
  ;;
end
