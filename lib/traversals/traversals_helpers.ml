class ['env] map_with_context = object
  (* Primitive types *)

  method string : 'env -> string -> string = fun _ x -> x
  method int : 'env -> int -> int = fun _ x -> x
  method char : 'env -> char -> char = fun _ x -> x
  method bool : 'env -> bool -> bool = fun _ x -> x

  method ref : 'a. ('env -> 'a -> 'a) -> 'env -> 'a ref -> 'a ref =
    fun f env r -> ref (f env !r)
  method option : 'a. ('env -> 'a -> 'a) -> 'env -> 'a option -> 'a option =
    fun f env o -> Option.map (f env) o
  method list : 'a. ('env -> 'a -> 'a) -> 'env -> 'a list -> 'a list =
    fun f env l -> List.map (f env) l
end
