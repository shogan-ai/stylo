let find_or_load_many t keys ~num_keys =
  let%bind cached_values =
    Rocksdb.Async.multi_get_exn
      t.db
      t.read_options
      keys [@trace.async "Rocksdb.multi_get" (num_keys : int)]
  in
  return cached_values
;;
