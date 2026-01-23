class map :
  object
    inherit Parsetree.map
    method bool : bool -> bool
    method char : char -> char
    method int : int -> int
    method string : string -> string
    method list : 'a. ('a -> 'a) -> 'a list -> 'a list
    method option : 'a. ('a -> 'a) -> 'a option -> 'a option
  end

class iter :
  object
    inherit Parsetree.iter
    method bool : bool -> unit
    method char : char -> unit
    method int : int -> unit
    method string : string -> unit
    method list : 'a. ('a -> unit) -> 'a list -> unit
    method option : 'a. ('a -> unit) -> 'a option -> unit
  end

class ['acc] fold :
  object
    inherit ['acc] Parsetree.fold
    method bool : bool -> 'acc -> 'acc
    method char : char -> 'acc -> 'acc
    method int : int -> 'acc -> 'acc
    method string : string -> 'acc -> 'acc
    method list : 'a. ('a -> 'acc -> 'acc) -> 'a list -> 'acc -> 'acc
    method option : 'a. ('a -> 'acc -> 'acc) -> 'a option -> 'acc -> 'acc
  end

class ['acc] fold_map :
  object
    inherit ['acc] Parsetree.fold_map
    method bool : bool -> 'acc -> bool * 'acc
    method char : char -> 'acc -> char * 'acc
    method int : int -> 'acc -> int * 'acc
    method string : string -> 'acc -> string * 'acc
    method
    list
    :
    'a. ('a -> 'acc -> 'a * 'acc) -> 'a list -> 'acc -> 'a list * 'acc
    method
    option
    :
    'a. ('a -> 'acc -> 'a * 'acc) -> 'a option -> 'acc -> 'a option * 'acc
  end

class ['ctx] map_with_context :
  object
    inherit ['ctx] Parsetree.map_with_context
    method bool : 'ctx -> bool -> bool
    method char : 'ctx -> char -> char
    method int : 'ctx -> int -> int
    method string : 'ctx -> string -> string
    method list : 'a. ('ctx -> 'a -> 'a) -> 'ctx -> 'a list -> 'a list
    method option : 'a. ('ctx -> 'a -> 'a) -> 'ctx -> 'a option -> 'a option
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

class virtual ['res] lift_virtual :
  object
    inherit ['res] Parsetree.lift
    method virtual bool : bool -> 'res
    method virtual char : char -> 'res
    method virtual int : int -> 'res
    method virtual string : string -> 'res
    method virtual list : 'a. ('a -> 'res) -> 'a list -> 'res
    method virtual option : 'a. ('a -> 'res) -> 'a option -> 'res
    method virtual record : (string * 'res) list -> 'res
    method virtual constr : string -> 'res list -> 'res
    method virtual tuple : 'res list -> 'res
  end

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

class virtual ['ctx, 'res] lift_map_with_context_virtual :
  object
    inherit ['ctx, 'res] Parsetree.lift_map_with_context
    method virtual bool : 'ctx -> bool -> bool * 'res
    method virtual char : 'ctx -> char -> char * 'res
    method virtual int : 'ctx -> int -> int * 'res
    method virtual string : 'ctx -> string -> string * 'res
    method
    virtual
    list
    :
    'a. ('ctx -> 'a -> 'a * 'res) -> 'ctx -> 'a list -> 'a list * 'res
    method
    virtual
    option
    :
    'a. ('ctx -> 'a -> 'a * 'res) -> 'ctx -> 'a option -> 'a option * 'res
    method virtual other : 'a. 'ctx -> 'a -> 'res
    method virtual record : 'ctx -> (string * 'res) list -> 'res
    method virtual constr : 'ctx -> string -> 'res list -> 'res
    method virtual tuple : 'ctx -> 'res list -> 'res
  end

class sexp_of : object inherit [Sexp_type.Sexp.t] lift end

val sexp_of : sexp_of
