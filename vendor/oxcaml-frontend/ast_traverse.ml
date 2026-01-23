class map =
  object
    inherit Parsetree.map

    method bool : bool -> bool = fun x -> x

    method char : char -> char = fun x -> x

    method int : int -> int = fun x -> x

    method string : string -> string = fun x -> x

    method list : 'a. ('a -> 'a) -> 'a list -> 'a list = List.map

    method option : 'a. ('a -> 'a) -> 'a option -> 'a option = Option.map
  end

class iter =
  object
    inherit Parsetree.iter

    method bool : bool -> unit = ignore

    method char : char -> unit = ignore

    method int : int -> unit = ignore

    method string : string -> unit = ignore

    method list : 'a. ('a -> unit) -> 'a list -> unit = List.iter

    method option : 'a. ('a -> unit) -> 'a option -> unit = Option.iter
  end

class ['acc] fold =
  object
    inherit ['acc] Parsetree.fold

    method bool : bool -> 'acc -> 'acc = fun _ acc -> acc

    method char : char -> 'acc -> 'acc = fun _ acc -> acc

    method int : int -> 'acc -> 'acc = fun _ acc -> acc

    method string : string -> 'acc -> 'acc = fun _ acc -> acc

    method list : 'a. ('a -> 'acc -> 'acc) -> 'a list -> 'acc -> 'acc =
      fun f l acc -> List.fold_left (fun elt acc -> f acc elt) acc l

    method option : 'a. ('a -> 'acc -> 'acc) -> 'a option -> 'acc -> 'acc =
      fun f o acc -> Option.fold o ~none:acc ~some:(fun x -> f x acc)
  end

class ['acc] fold_map =
  object
    inherit ['acc] Parsetree.fold_map

    method bool : bool -> 'acc -> bool * 'acc = fun x acc -> x, acc

    method char : char -> 'acc -> char * 'acc = fun x acc -> x, acc

    method int : int -> 'acc -> int * 'acc = fun x acc -> x, acc

    method string : string -> 'acc -> string * 'acc = fun x acc -> x, acc

    method list
      : 'a. ('a -> 'acc -> 'a * 'acc) -> 'a list -> 'acc -> 'a list * 'acc
      =
      fun f l acc ->
      let l, acc =
        List.fold_left
          (fun (l, acc) x ->
            let y, acc = f x acc in
            y :: l, acc)
          ([], acc)
          l
      in
      List.rev l, acc

    method option
      : 'a. ('a -> 'acc -> 'a * 'acc) -> 'a option -> 'acc -> 'a option * 'acc
      =
      fun f o acc ->
      match o with
      | None -> None, acc
      | Some x ->
        let y, acc = f x acc in
        Some y, acc
  end

class ['ctx] map_with_context =
  object
    inherit ['ctx] Parsetree.map_with_context

    method bool : 'ctx -> bool -> bool = fun _ x -> x

    method char : 'ctx -> char -> char = fun _ x -> x

    method int : 'ctx -> int -> int = fun _ x -> x

    method string : 'ctx -> string -> string = fun _ x -> x

    method list : 'a. ('ctx -> 'a -> 'a) -> 'ctx -> 'a list -> 'a list =
      fun f ctx l -> List.map (f ctx) l

    method option : 'a. ('ctx -> 'a -> 'a) -> 'ctx -> 'a option -> 'a option =
      fun f ctx o -> Option.map (f ctx) o
  end

class type ['res] lift =
  object
    inherit ['res] Parsetree.lift
    method bool : bool -> 'res
    method char : char -> 'res
    method int : int -> 'res
    method string : string -> 'res
    method list : 'a. ('a -> 'res) -> 'a list -> 'res
    method option : 'a. ('a -> 'res) -> 'a option -> 'res
    method record : (string * 'res) list -> 'res
    method constr : string -> 'res list -> 'res
    method tuple : 'res list -> 'res
  end

class virtual ['res] lift_virtual = object inherit ['res] Parsetree.lift end

class type ['ctx, 'res] lift_map_with_context =
  object
    inherit ['ctx, 'res] Parsetree.lift_map_with_context
    method bool : 'ctx -> bool -> bool * 'res
    method char : 'ctx -> char -> char * 'res
    method int : 'ctx -> int -> int * 'res
    method string : 'ctx -> string -> string * 'res
    method
    list
    :
    'a. ('ctx -> 'a -> 'a * 'res) -> 'ctx -> 'a list -> 'a list * 'res
    method
    option
    :
    'a. ('ctx -> 'a -> 'a * 'res) -> 'ctx -> 'a option -> 'a option * 'res
    method other : 'a. 'ctx -> 'a -> 'res
    method record : 'ctx -> (string * 'res) list -> 'res
    method constr : 'ctx -> string -> 'res list -> 'res
    method tuple : 'ctx -> 'res list -> 'res
  end

class virtual ['ctx, 'res] lift_map_with_context_virtual =
  object inherit ['ctx, 'res] Parsetree.lift_map_with_context
  end

class sexp_of =
  let
  open Sexp_type
  in
  object
    inherit [Sexp.t] Parsetree.lift

    method bool b = Sexp.Atom (Bool.to_string b)

    method char c = Sexp.Atom (String.make 1 c)

    method int i = Sexp.Atom (Int.to_string i)

    method string s = Sexp.Atom s

    method list f list = Sexp.List (List.map f list)

    method option f = function
      | None -> Sexp.List []
      | Some x -> Sexp.List [ f x ]


    method constr name = function
      | [] -> Sexp.Atom name
      | _ :: _ as args -> Sexp.List (Atom name :: args)


    method record fields =
      Sexp.List
        (List.filter_map
           (function
             | _, Sexp.List [] -> None
             | name, sexp -> Some (Sexp.List [ Atom name; sexp ]))
           fields)

    method tuple elts = Sexp.List elts

    method! location loc =
      if Location.is_none loc
      then Sexp.List []
      else Sexp.Atom (Location.to_string loc)
  end

let sexp_of = new sexp_of
