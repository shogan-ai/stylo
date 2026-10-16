(* The first line of a row is the only one aligned: rows whose cells end on the
   following lines don't contribute to the width of columns. *)
let f x =
  match x with
  | A -> 1
  | Long_constructor_name_here { field1; field2; field3 } when some_guard field1 field2 ->
    some_long_function_call field1 field2 field3
  | Bbbbbb -> 2
;;

type t =
  | Short
  | Very_long_constructor_name_for_testing of some_type_with_a_long_name * another_long_type_name * int
  | Mid of int
