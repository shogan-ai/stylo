module Labels = struct
  module Retail_order_time_in_force =
    Make_truncated_stringable (Retail_order_time_in_force)

  module Retailer = Make_truncated_stringable (Retailer.Name)
end
