module Requirement (* : sig
  type t

  val nest : int -> t -> t

  val to_int_including_indent : current_indent:int -> t -> int
end *) = struct
  type expr =
    | Infinity
    | Const of int
    | If of Condition.t * expr * expr
    | Add of expr * expr
end

let foo x =
  if x (* && some_long_check_which imply splitting this_if's condition
    && some other smaller check *) && another small one
  then
    foo
  else
    let () = () in
    bar
