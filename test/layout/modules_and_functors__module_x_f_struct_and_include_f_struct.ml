module Int_set = Set.Make (struct
    type t = int [@@deriving compare, sexp]
  end)

include Comparable.Make (struct
    type t = int [@@deriving compare, sexp]
  end)
