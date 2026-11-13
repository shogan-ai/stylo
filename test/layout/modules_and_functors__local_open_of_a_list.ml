let all_sections = function
  | `Simple -> Section.[ Overall; By_second_grouper ]
  | `Large_example_ ->
    Section.
      [ Overall
      ; By_entity
      ; By_first_group__
      ; By_first_group_and_entity__
      ; By_some_region___
      ; By_first_group___entity_and_some_region___
      ; By_account
      ; By_second_grouper
      ; By_on_behalf_of_another_kind
      ; By_example_key
      ]
;;
