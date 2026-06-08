(*
MIT License

Copyright (c) 2017-2023, Facebook, Inc.
Copyright (c) 2023-present, Tarides.

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
*)

let f y = borrow_ y

let x = ref (borrow_ x)

let x = ignore_local (borrow_ x)

let f (r : r) = ref (borrow_ r.x)

let f (r : r) = ignore_local (borrow_ r.x) [@nontail]

let f () = borrow_ (3, 5)

let f () =
    let g = borrow_ x in
    g

let f () = List.length (borrow_ [1; 2; 3])

let x = borrow_ (borrow_ x)

let x = borrow_ (let y = 1 in Some y)

let x = borrow_ c#x

let _ = borrow_ (X foo) [@bar]

let _ = borrow_ (X (foo [@bar]))

let _ = borrow_ ((fun x -> x) [@bar])

(* Constructor without arg *)
let x = borrow_ Foo

(* Polymorphic variant *)
let x = borrow_ `Foo

let x = borrow_ (`Bar 42)

(* Array *)
let x = borrow_ [| 1; 2; 3 |]

(* Record *)
let x = borrow_ { x = 1 }

(* Object *)
class foo cla = object method bar = borrow_ {< >} end

let f () = borrow_ (object end)

let f () = borrow_ (new cla)

(* Lazy *)
let x = borrow_ (lazy "hello")

(* Uminus/uplus — not simple_expr, need parens *)
let x = borrow_ (-x)

let x = borrow_ (-42)

let x = borrow_ (+x)

let x = borrow_ (+42)

let x = borrow_ (-(-x))

let x = borrow_ (-(-42))

(* If/then/else — needs parens *)
let x = borrow_ (if x then y else z)

(* Assignment — needs parens *)
let x = borrow_ (r.x <- x)

(* Comments *)
let x = (* 1 *) borrow_ (* 2 *) x
