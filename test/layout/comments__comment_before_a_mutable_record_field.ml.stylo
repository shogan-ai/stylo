type t =
  { mutable cached_attempts : Attempt_summary.t String.Map.t option
  ; (* Bumped by [with_volatile_mutation] on commit, by each bulk load, and by each
       example update. Some tracking code uses this to wait for the next sync.. *)
    mutable data_generation : int
  }
