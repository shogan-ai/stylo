let sample_submit_request_form graph =
  project_to_example_value_request_
    (module Example_first_kind_request_form)
    (module Unsupported_thing.Fill_with_file__
              (Sample_example_value.Example_first_kind_example_value__.Request))
    ~example_value_request_form_
    graph
;;
