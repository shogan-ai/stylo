module Make
    (Inputflow : Inputflow.S)
    (Settings : Settings with module Inputflow := Inputflow.Historical)
    (Second_part : Second_part
                   with type settings_config := Settings.Config.t
                    and type annotation := Settings.Annotation.t) =
struct
  let x = 1
end
