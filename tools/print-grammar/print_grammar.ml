let () = if Array.length Sys.argv <> 2 || not (Sys.file_exists Sys.argv.(1)) then (
    if Array.length Sys.argv = 2 then
      Printf.eprintf "File %S does not exist.\n\n" Sys.argv.(1);
    Printf.eprintf
      "Usage: %s <grammar.cmly>\n\n\
       Print clean rules without semantic actions\n"
      Sys.argv.(0);
    exit 1
  )

module G = MenhirSdk.Cmly_read.Read(struct
    let filename = Sys.argv.(1)
  end)

open G.Surface

let rec print_parameter param =
  match Parameter.desc param with
  | Var var -> print_string var
  | App (name, args) ->
    print_string name;
    print_char '(';
    List.iter print_parameter args;
    print_char ')';
  | Anonymous branches ->
    print_char '(';
    List.iteri print_inline_branch branches;
    print_char ')';

and print_producer prod =
  print_char ' ';
  print_parameter (Producer.symbol prod)

and print_inline_branch i branch =
  if i > 0 then print_string " | ";
  List.iter print_producer (Branch.producers branch)

let print_branch branch =
  print_string "|";
  List.iter print_producer (Branch.producers branch);
  print_newline ()

let print_rule (name, def) =
  print_string name;
  begin match Rule.parameters def with
    | [] -> ()
    | params ->
      print_char '(';
      List.iteri begin fun i n ->
        if i > 0 then print_char ',';
        print_string n
      end params;
      print_char ')';
  end;
  print_string ":\n";
  List.iter print_branch (Rule.branches def);
  print_newline ()

let () = List.iter print_rule (Syntax.rules before_expansion)
