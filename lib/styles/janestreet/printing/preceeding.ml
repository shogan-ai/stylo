open Document

type nest_fun = t -> t

type chunk =
  { pre_doc : t
  ; space : t
  ; after_comments : t
  ; nest : nest_fun
  }

type t = chunk list * nest_fun

let implied_nest = function
  | None -> Fun.id
  | Some (_, nest) -> nest
;;

let mk ?indent ?(after_comments = break 1) doc space =
  let indent =
    match indent with
    | None ->
      let req = requirement doc in
      if req = Requirement.infinity (* doc contains [hardline]s *)
      then 2
      else Requirement.to_int req + if is_empty space then 0 else 1
    | Some i -> i
  in
  let vanish =
    match doc with
    | Cat (_, Token { vanishing_cond; _ }, _)
    | Cat (_, _, Token { vanishing_cond; _ })
    | Token { vanishing_cond; _ } -> Some vanishing_cond
    | _ -> None
  in
  let vanish =
    Condition.mark_item (Option.value vanish ~default:Condition.never)
  in
  let chunk = { pre_doc = doc; space; after_comments; nest = nest ~vanish indent } in
  [ chunk ], chunk.nest
;;

let tight ?indent doc = mk doc empty ?indent
let spaced ?indent doc = mk doc (break 1) ?indent

(* There's no point in breaking after a separating [;]: the item would start at
   the same column on the next line. *)
let semi = mk Syntax.semi nbsp

let line_if_broken flatness =
  let flat = Condition.flat flatness in
  vanishing_whitespace flat hardline
  ^^ vanishing_whitespace (Condition.not_ flat) (break 1)
;;

let field_semi ~flatness =
  mk Syntax.semi nbsp ~after_comments:(line_if_broken flatness)
;;

let preceed ~by:t doc =
  let inserted, hint =
    flush_comments
      ~pull_preceeding_comments:true
      ~ws_before:(group ~margin:3 t.space) (* start the comment on same line. *)
      ~ws_after:t.after_comments
  in
  let space = vanishing_whitespace inserted t.space in
  t.pre_doc ^^ t.nest (hint ^^ space ^^ doc)
;;

let ( + ) fst_opt snd =
  match fst_opt with
  | None -> snd, Fun.id
  | Some (fst_chunks, fst_nest) ->
    let (snd_chunks, snd_nest) = snd in
    let chunks = snd_chunks @ fst_chunks in
    let nest doc = fst_nest (snd_nest doc) in
    (chunks, nest), fst_nest
;;

let rec can_break : Document.t -> bool = function
  | Whitespace { value = Break _ | Line_break _; _ } -> true
  | Cat (_, a, b) -> can_break a || can_break b
  | Nest (_, _, _, d) | Group (_, _, _, d) -> can_break d
  | Empty
  | Whitespace { value = Non_breakable; _ }
  | Token _ | Directive _ | Comment _ | Comments_flushing_hint _ -> false
;;

let never_breaks = function
  | None -> true
  | Some (chunks, _) ->
    List.for_all
      (fun chunk -> not (can_break chunk.pre_doc || can_break chunk.space))
      chunks
;;

let group_with t doc =
  match t with
  | None -> doc, Fun.id
  | Some (chunks, nest) ->
    ( List.fold_left (fun doc chunk -> group (preceed doc ~by:chunk)) doc chunks
    , nest )
;;
