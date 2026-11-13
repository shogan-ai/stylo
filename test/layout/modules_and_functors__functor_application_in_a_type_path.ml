module Types = struct
  module Some_inner_thing = struct
    type t =
      { relevant_entries :
          Relevant_entries.Some_inner_thing.t
            Incr_join.Into.M(Incr)(Some_symbol).can_read_now
      ; limits : Bignum.t Key.Map.t Incr.Observer.t
      }
  end
end
