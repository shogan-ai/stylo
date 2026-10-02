(** A piece of OCaml source code, as given to the pipeline. *)

type kind =
  | Impl
  | Intf

type t =
  { fname : string
  ; start_line : int
  ; source : string
  ; kind : kind
  }
