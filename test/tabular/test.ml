(* Vertical alignment of documents, independently of any style. *)

open Document

let print name ?(width = 80) d =
  Printf.printf "=== %s\n%s\n\n" name (Print.to_string ~vertically_aligned:true ~width d)
;;

(* Rows are separated by hardlines, so they are never flat. *)
let table rows = vertically_aligned ~sep:hardline rows

let row a b = cell (string a) ^^ nbsp ^^ cell (string b)

let () =
  print
    "nested cells"
    (table
       [ cell (string "Foo") ^^ cell (string " of") ^^ cell (string " int")
       ; cell (string "Bar") ^^ cell (string " of") ^^ cell (string " " ^^ cell (string "float") ^^ string " list")
       ; cell (string "Baz")
         ^^ cell (string " of")
         ^^ cell
              (string " "
               ^^ cell (cell (string "(unit") ^^ string " " ^^ cell (string "*") ^^ string " " ^^ cell (string "char)"))
               ^^ string " list")
       ]);
  print
    "terminal cells don't constrain columns"
    (table
       [ cell (string "let a =") ^^ cell (string " some_long_function_call x y")
       ; cell (string "let bb =") ^^ cell (string " 1") ^^ string " (* one *)"
       ; cell (string "let c =") ^^ cell (string " 22") ^^ string " (* twenty-two *)"
       ]);
  print
    "rows with fewer cells, empty trailing cells"
    (table
       [ cell (cell (string "| Int") ^^ cell (string " of int")) ^^ string " (** foo *)"
       ; cell (cell (string "| None")) ^^ string " (** baz *)"
       ; cell (cell (string "| Floatttt") ^^ cell empty) ^^ cell empty
       ]);
  print
    "whitespace in anonymous cells"
    (table
       [ cell (string "x") ^^ nbsp ^^ cell (string "foo") ^^ cell (string " |")
       ; cell (string "x") ^^ cell (string "bar") ^^ cell (string " |")
       ]);
  print
    "only the first line of a row is aligned"
    (table
       [ cell (string "| A") ^^ cell (string " -> 1")
       ; cell (string "| Bbbbbbbbbbbbbbbbbb" ^^ hardline ^^ string "  when x") ^^ cell (string " -> 2")
       ; cell (string "| Cc") ^^ cell (string " ->") ^^ hardline ^^ string "  3"
       ]);
  print
    "nested contexts"
    (table
       [ cell (string "| A") ^^ cell (string " ->")
       ; cell (string "| Bbbbbb") ^^ cell (string " ->") ^^ nest 2 (hardline ^^ table [ cell (string "| Z") ^^ cell (string " -> 0"); cell (string "| Zzzz") ^^ cell (string " -> 1") ])
       ; cell (string "| Cc") ^^ cell (string " -> 2")
       ]);
  let right d = cell ~align:Right d in
  print
    "right aligned cells are padded at the end of lines"
    (table
       [ cell (string "a =") ^^ string " " ^^ right (string "1")
       ; cell (string "b =") ^^ string " " ^^ right (string "100")
       ; cell (string "c =") ^^ string " " ^^ right (string "22")
       ]);
  print
    "left aligned cells at the end of lines don't push right aligned ones"
    (table
       [ cell (string "x =") ^^ string " " ^^ right (string "1")
       ; cell (string "y =") ^^ string " " ^^ right (string "100")
       ; cell (string "z =") ^^ string " " ^^ cell (string "some_long_thing")
       ]);
  print
    "right aligned cells followed by something"
    (table
       [ right (string "1") ^^ string ", " ^^ right (string "-2") ^^ string ";"
       ; right (string "100") ^^ string ", " ^^ right (string "3") ^^ string ";"
       ]);
  print
    "right aligned leaf in a subdivided column"
    (table
       [ cell (string "| A") ^^ string " " ^^ right (string "int") ^^ string " (** a *)"
       ; cell (string "| B") ^^ string " " ^^ right (cell (string "float") ^^ string " " ^^ cell (string "list")) ^^ string " (** b *)"
       ]);
  print
    "right aligned cells spanning several lines"
    (table
       [ cell (string "a =") ^^ string " " ^^ right (string "1")
       ; cell (string "b =") ^^ string " " ^^ right (string "[ 100" ^^ hardline ^^ string "    ]")
       ]);
  print
    "text after a table: the last row isn't aligned if it would overflow"
    ~width:14
    (table [ row "aaaaaaaaaa" "b"; row "a" "b" ] ^^ string ")))))");
  print
    "text after a table: the last row is still aligned if it fits"
    ~width:17
    (table [ row "aaaaaaaaaa" "b"; row "a" "b" ] ^^ string ")))))");
  print
    "text after a table: trailing spaces don't count"
    ~width:14
    (table [ row "aaaaaaaaaa" "b"; row "a" "b" ] ^^ nbsp ^^ nbsp ^^ nbsp);
  print
    "text after a table: only the last row is affected"
    ~width:14
    (table [ row "a" "b"; row "aaaaaaaaaa" "b"; row "a" "b" ] ^^ string ")))))");
  print
    "text after nested tables, closed on the same line"
    ~width:16
    (table
       [ string "x"
         ^^ hardline
         ^^ table [ row "aaaaaaaaaa" "b"; row "a" "b" ]
         ^^ string ")))"
       ]
     ^^ string ")))");
  print
    "text after a table, in a row of the parent table"
    ~width:14
    (table
       [ string "x"
         ^^ hardline
         ^^ table [ row "aaaaaaaaaa" "b"; row "a" "b" ]
         ^^ string ")))))"
       ; string "y"
       ]);
  print
    "text after a table, before another table on the same line"
    ~width:14
    (table [ row "aaaaaaaaaa" "b"; row "a" "b" ]
     ^^ string ")))))"
     ^^ table [ row "c" "d"; row "ccc" "d" ]);
  print "flat contexts are not aligned" (group (vertically_aligned ~sep:(break 1) [ cell (string "a") ^^ string "1"; cell (string "bbb") ^^ string "2" ]))
;;
