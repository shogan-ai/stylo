let handle_stored_message
  (type wrapped_msg_t)
  t
  (module Messages : Storable_messages_intf.S
    with type Stable.With_bork.Wrapped_message.Latest.t = wrapped_msg_t)
  (wrapped_message : wrapped_msg_t)
  =
  Messages.store t wrapped_message
;;
