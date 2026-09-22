let f = object
  method foo () x = x (* cmt *)
  method bar () x = x
end

let f x = x (* cmt *)
and g x = x

module rec F : sig val x : int end = struct
  let x = 3
end (* cmt *)
and G : sig val x : int end = F
