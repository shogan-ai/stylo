open Ocaml_syntax
open Parsetree

let remove_from_tokens tokens = function
  | None -> tokens
  | Some (m_loc, m_tokens, _) ->
    Token_helpers.without_child ~at:m_loc.Location.loc_start
      (List.filter Tokens.is_comment m_tokens) tokens

let remove_from_name ~replace name =
  let (name_loc, m, name_tokens) = name in
  match replace m with
  | None -> name
  | Some (m_loc, m_tokens, replacement) ->
    let tokens =
      Token_helpers.without_child ~at:m_loc.Location.loc_start
        (List.filter Tokens.is_comment m_tokens) name_tokens
      |> Tokens.Seq.without ~token:LPAREN
      |> Tokens.Seq.without ~token:RPAREN
    in
    name_loc, replacement, tokens

module Modes = struct
  let replace = function
    | No_modes -> None
    | Modes { loc; tokens; _ } -> Some (loc, tokens, No_modes)

  let remove_from_tokens m tokens = remove_from_tokens tokens (replace m)
  let remove_from_name = remove_from_name ~replace
end

module Modalities = struct
  let replace = function
    | No_modalities -> None
    | Modalities { loc; tokens; _ } -> Some (loc, tokens, No_modalities)

  let remove_from_tokens m tokens = remove_from_tokens tokens (replace m)
  let remove_from_name = remove_from_name ~replace
end
