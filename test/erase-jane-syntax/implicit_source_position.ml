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

(* This file is the test case for erasing "implicit source position"
   arguments.

   The test output for this file can be found in
   [implicit_source_position_erased.ml.ref]. The options for this test can be
   found in [implicit_source_position_erased.ml.opts]. *)
let punned_pattern ~(call_pos : [%call_pos]) () = call_pos

let ignored_pattern ~call_pos:(_ : [%call_pos]) () = 1

let destructured_pattern ~call_pos:({pos_fname; _} : [%call_pos]) () = ()

let in_a_type : call_pos:[%call_pos] -> unit -> Lexing.position =
  punned_pattern

let in_an_expression = [%src_pos]

let with_locals ~(local_ call_pos : [%call_pos]) () = ()

type 'a t = here:[%call_pos] -> 'a

let f
  (x : here:[%call_pos] -> _)
  ~(y : here:[%call_pos] -> _)
  ?(z : here:[%call_pos] -> _)
  =
  ()
