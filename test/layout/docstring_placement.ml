module _ : sig
  val x : int
  (** x ds *)

  external prim : int -> int = "prim"
  (** prim ds *)

  type t
  (** t ds *)

  and u
  (** u ds *)

  type alias := t
  (** alias ds *)

  type ext = ..
  (** ext ds *)

  type ext += A | B (**)
  (** ext ext ds *)

  exception Exn
  (** Exn ds *)

  module M : sig end
  (** M ds *)

  module rec N : sig end
  (** N ds *)

  and O : sig end
  (** O ds *)

  module type S
  (** S ds *)

  module type S2 := S
  (** S2 ds *)

  open M
  (** open ds *)

  include S
  (** include ds *)

  class c1 : object end
  (** c1 ds *)

  and c2 : t
  (** c2 ds *)

  class type ct1 = t
  (** ct1 ds *)

  and ct2 = object
    inherit foo
    (** inherit ds *)

    val v : int
    (** v ds *)

    method m : int -> int
    (** m ds *)
  end
  (** ct2 ds *)
end = struct
  let x = 3
  (** x ds *)

  and y = ()
  (** y ds *)

  val whatever : int
  (** whatever ds *)

  external prim : int -> int = "prim"
  (** prim ds *)

  type t = < >
  (** t ds *)

  and u
  (** u ds *)

  type ext = ..
  (** ext ds *)

  type ext += A | B (**)
  (** ext ext ds *)

  exception Exn
  (** Exn ds *)

  module M = struct end
  (** M ds *)

  module rec N : sig end = N
  (** N ds *)

  and O : sig end = O
  (** O ds *)

  module type S
  (** S ds *)

  open M
  (** open ds *)

  class c1 = object end
  (** c1 ds *)

  and c2 = c1
  (** c2 ds *)

  class type ct1 = t
  (** ct1 ds *)

  and ct2 = object
    inherit foo
    (** inherit ds *)

    val v : int
    (** v ds *)

    method m : int -> int
    (** m ds *)
  end
  (** ct2 ds *)

  include M
  (** include ds *)

end
