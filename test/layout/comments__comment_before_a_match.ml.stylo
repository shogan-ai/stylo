let shard office =
  [ "-instance"
  ; (* Make it [office]-based rather than [Parts.index]-based. See the comment above about
       simplifying this. *)
    (match Location.Abbrev.to_string office with
     | "vdc" | "hk1" | "igm" -> "1-of-2"
     | _ -> "2-of-2")
  ]
;;
