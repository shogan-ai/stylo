let is_concrete = function
  | Some Physical_something_spot -> true
  | Some
      ( Financial_something_future
      | Financial_something_generic
      | Financial_something_option
      | Financial_something_spot
      | Physical_something_forward
      | Physical_something_future
      | Physical_something_generic
      | Physical_something_option ) -> false
  | None -> false
;;
