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

(* this one is purposefully misformatted *)

let x = +#3.14
let x = #3.14+#5.82
let x = #3l-#4n
let x = #4n+ -#5.0
let x = f #3n#4l#5.0-#6L+-#7.0z#3n
let x = f #3n#4l#5.0-#6L+-#7.0 z#m #3n

type t = float#float
type t = float #float
type t = float# float
type t = float#float#
type t = #float float# #float


type ('a                 : float64) t = 'a
type (   'b, 'a : float64)
       t
type ('b : float64, 'a :                        immediate) t
  =
  'a
