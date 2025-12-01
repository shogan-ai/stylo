let f = function
  | Prev
      ( Current_whatever_thingy (aaaaaaaaaaaaaaaaaaaaaaaaaaaaaarrrrrrrr,
                                 foooooooooooooooooooo), move_kindddddddddddddddddddd) ->
          Prev
            (( { x =
            Array.map
                ar
                ~f:(apply
                   ~refined:false
                   ~rule:(match move_kind with
                    | Some (Move _) -> rules.moved_from_prev
                    | Some (Within_move _) -> rules.removed_in_move
                    | None -> rules.line_prev))}
          , move_kind )
            , foo)
  | Prev
      ( Current_whatever_thingy (aaaaaaaaaaaaaaaaaaaaaaaaaaaaaarrrrrrrr,
                                 foooooooooooooooooooo), move_kindddddddddddddddddddd) ->
          Prev
            (
            Constr (
                ar,
                { a; b = (match () with () -> true) },
                (a, match x with () -> false),
                (apply
                   ~refined:false
                   ~rule:(match move_kind with
                    | Some (Move _) -> rules.moved_from_prev
                    | Some (Within_move _) -> rules.removed_in_move
                    | None -> rules.line_prev)))
            , move_kind )
