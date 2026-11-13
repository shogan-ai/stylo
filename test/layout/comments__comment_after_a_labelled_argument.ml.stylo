let create_implementations ~implementations =
  Logged_msg.Implementations.create_exn
    ~on_unknown_msg:`Close_connection
      (* Defaulting [~on_exception] to [Log_on_other_kind_exn]. Consider the other options
         if you'd rather exns be louder. *)
    ~implementations
;;
