let foo =
  List.map (function
    | None -> true
    | Some _ -> false)

let bar =
  test (List.map (function
    | None -> true
    | Some _ -> false))

let baz =
  test @@ List.map (function
    | None -> true
    | Some _ -> false)

let lol =
  (List.map (function
    | None -> true
    | Some _ -> false), true)

let loool =
  ((List.map (function
    | None -> true
    | Some _ -> false), true), true)

let l0l =
  (test (List.map (function
    | None -> true
    | Some _ -> false)), true)

let foo =
  List.map (fun _ -> function
    | None -> true
    | Some _ -> false)

let bar =
  test (List.map (fun _ -> function
    | None -> true
    | Some _ -> false))

let baz =
  test @@ List.map (fun _ -> function
    | None -> true
    | Some _ -> false)

let lol =
  (List.map (fun _ -> function
    | None -> true
    | Some _ -> false), true)

let loool =
  ((List.map (fun _ -> function
    | None -> true
    | Some _ -> false), true), true)

let l0l =
  (test (List.map (fun _ -> function
    | None -> true
    | Some _ -> false)), true)

let foo =
  List.map (fun x ->
    let () = () in
    something x)

let bar =
  test (List.map (fun x ->
    let () = () in
    something x))

let baz =
  test @@ List.map (fun x ->
    let () = () in
    something x)

let lol =
  ( List.map (fun x ->
    let () = () in
    something x), true)

let loool =
  (( List.map (fun x ->
    let () = () in
    something x), true), true)

let l0l =
  ( test (List.map (fun x ->
    let () = () in
    something x)), true)
