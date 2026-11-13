let process t =
  With_resource.use t ~f:(local_ fun resource ->
    print_endline (Resource.name resource);
    Resource.close resource)
;;
