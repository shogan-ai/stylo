let dbg =
  match Sys.getenv "DBG" with
  | _ -> true
  | exception Not_found -> false

let dprintf fmt =
  (if dbg then Format.fprintf else Format.ifprintf)
    Format.err_formatter fmt
