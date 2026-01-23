type token := Parser_tokens.token

class map :
  object
    inherit Parsetree.map
    method bool : bool -> bool
    method char : char -> char
    method int : int -> int
    method string : string -> string
    method token : token -> token
    method list : 'a. ('a -> 'a) -> 'a list -> 'a list
    method option : 'a. ('a -> 'a) -> 'a option -> 'a option
    method ref : 'a. ('a -> 'a) -> 'a ref -> 'a ref
  end

class iter :
  object
    inherit Parsetree.iter
    method bool : bool -> unit
    method char : char -> unit
    method int : int -> unit
    method string : string -> unit
    method token : token -> unit
    method list : 'a. ('a -> unit) -> 'a list -> unit
    method option : 'a. ('a -> unit) -> 'a option -> unit
    method ref : 'a. ('a -> unit) -> 'a ref -> unit
  end

class ['acc] fold :
  object
    inherit ['acc] Parsetree.fold
    method bool : bool -> 'acc -> 'acc
    method char : char -> 'acc -> 'acc
    method int : int -> 'acc -> 'acc
    method string : string -> 'acc -> 'acc
    method token : token -> 'acc -> 'acc
    method list : 'a. ('a -> 'acc -> 'acc) -> 'a list -> 'acc -> 'acc
    method option : 'a. ('a -> 'acc -> 'acc) -> 'a option -> 'acc -> 'acc
    method ref : 'a. ('a -> 'acc -> 'acc) -> 'a ref -> 'acc -> 'acc
  end

class ['acc] fold_map :
  object
    inherit ['acc] Parsetree.fold_map
    method bool : bool -> 'acc -> bool * 'acc
    method char : char -> 'acc -> char * 'acc
    method int : int -> 'acc -> int * 'acc
    method string : string -> 'acc -> string * 'acc
    method token : token -> 'acc -> token * 'acc
    method
    list
    :
    'a. ('a -> 'acc -> 'a * 'acc) -> 'a list -> 'acc -> 'a list * 'acc
    method
    option
    :
    'a. ('a -> 'acc -> 'a * 'acc) -> 'a option -> 'acc -> 'a option * 'acc
    method
    ref
    :
    'a. ('a -> 'acc -> 'a * 'acc) -> 'a ref -> 'acc -> 'a ref * 'acc
  end

class ['ctx] map_with_context :
  object
    inherit ['ctx] Parsetree.map_with_context
    method bool : 'ctx -> bool -> bool
    method char : 'ctx -> char -> char
    method int : 'ctx -> int -> int
    method string : 'ctx -> string -> string
    method token : 'ctx -> token -> token
    method list : 'a. ('ctx -> 'a -> 'a) -> 'ctx -> 'a list -> 'a list
    method option : 'a. ('ctx -> 'a -> 'a) -> 'ctx -> 'a option -> 'a option
    method ref : 'a. ('ctx -> 'a -> 'a) -> 'ctx -> 'a ref -> 'a ref
  end

class type ['res] lift =
  object
    inherit ['res] Parsetree.lift
    method bool : bool -> 'res
    method char : char -> 'res
    method int : int -> 'res
    method string : string -> 'res
    method token : token -> 'res
    method list : 'a. ('a -> 'res) -> 'a list -> 'res
    method option : 'a. ('a -> 'res) -> 'a option -> 'res
    method ref : 'a. ('a -> 'res) -> 'a ref -> 'res
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
    method virtual token : token -> 'res
    method virtual list : 'a. ('a -> 'res) -> 'a list -> 'res
    method virtual option : 'a. ('a -> 'res) -> 'a option -> 'res
    method virtual ref : 'a. ('a -> 'res) -> 'a ref -> 'res
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
    method token : 'ctx -> token -> token * 'res
    method
    list
    :
    'a. ('ctx -> 'a -> 'a * 'res) -> 'ctx -> 'a list -> 'a list * 'res
    method
    option
    :
    'a. ('ctx -> 'a -> 'a * 'res) -> 'ctx -> 'a option -> 'a option * 'res
    method
    ref
    :
    'a. ('ctx -> 'a -> 'a * 'res) -> 'ctx -> 'a ref -> 'a ref * 'res
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
    method virtual token : 'ctx -> token -> token * 'res
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
    method
    virtual
    ref
    :
    'a. ('ctx -> 'a -> 'a * 'res) -> 'ctx -> 'a ref -> 'a ref * 'res
    method virtual other : 'a. 'ctx -> 'a -> 'res
    method virtual record : 'ctx -> (string * 'res) list -> 'res
    method virtual constr : 'ctx -> string -> 'res list -> 'res
    method virtual tuple : 'ctx -> 'res list -> 'res
  end

class sexp_of : object inherit [Sexp_type.Sexp.t] lift end

val sexp_of : sexp_of
