(* fake one, just make things compile *)

let color : Misc.Color.setting option ref = ref None
let absname = ref false
let locs = ref true
let error_style : Misc.Error_style.setting option ref = ref None
let unsafe = ref false
let applicative_functors = ref true
let keyword_edition: string option ref = ref None

let parse_keyword_edition s =
  let parse_version s =
  let bad_version () =
    raise (Arg.Bad "Ill-formed version in keywords flag,\n\
                    the supported format is <major>.<minor>, for example 5.2 .")
  in
  if s = "" then None else match String.split_on_char '.' s with
  | [] | [_] | _ :: _ :: _ :: _ -> bad_version ()
  | [major;minor] -> match int_of_string_opt major, int_of_string_opt minor with
    | Some major, Some minor -> Some (major,minor)
    | _ -> bad_version ()
  in
  match String.split_on_char '+' s with
  | [] -> None, []
  | [s] -> parse_version s, []
  | v :: rest -> parse_version v, rest
