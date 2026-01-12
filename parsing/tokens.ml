module Raw = struct
  let to_string = function
  | Parser_tokens.AMPERAMPER  -> "AMPERAMPER"
  | AMPERSAND  -> "AMPERSAND"
  | AND  -> "AND"
  | AS  -> "AS"
  | ASSERT  -> "ASSERT"
  | BACKQUOTE  -> "BACKQUOTE"
  | BANG  -> "BANG"
  | BAR  -> "BAR"
  | BARBAR  -> "BARBAR"
  | BARRBRACKET  -> "BARRBRACKET"
  | BEGIN  -> "BEGIN"
  | CHAR _ -> "CHAR"
  | HASH_CHAR _ -> "HASH_CHAR"
  | CLASS  -> "CLASS"
  | COLON  -> "COLON"
  | COLONCOLON  -> "COLONCOLON"
  | COLONEQUAL  -> "COLONEQUAL"
  | COLONGREATER  -> "COLONGREATER"
  | COLONRBRACKET  -> "COLONRBRACKET"
  | COMMA  -> "COMMA"
  | CONSTRAINT  -> "CONSTRAINT"
  | DO  -> "DO"
  | DOLLAR  -> "DOLLAR"
  | DONE  -> "DONE"
  | DOT  -> "DOT"
  | DOTDOT  -> "DOTDOT"
  | DOTHASH  -> "DOTHASH"
  | DOWNTO  -> "DOWNTO"
  | ELSE  -> "ELSE"
  | END  -> "END"
  | EOF  -> "EOF"
  | EQUAL  -> "EQUAL"
  | EXCEPTION  -> "EXCEPTION"
  | EXCLAVE  -> "EXCLAVE"
  | EXTERNAL  -> "EXTERNAL"
  | FALSE  -> "FALSE"
  | FLOAT _ -> "FLOAT"
  | HASH_FLOAT _ -> "HASH_FLOAT"
  | FOR  -> "FOR"
  | FUN  -> "FUN"
  | FUNCTION  -> "FUNCTION"
  | FUNCTOR  -> "FUNCTOR"
  | GLOBAL  -> "GLOBAL"
  | GREATER  -> "GREATER"
  | GREATERRBRACE  -> "GREATERRBRACE"
  | GREATERRBRACKET  -> "GREATERRBRACKET"
  | HASHLPAREN  -> "HASHLPAREN"
  | HASHLBRACE  -> "HASHLBRACE"
  | IF  -> "IF"
  | IN  -> "IN"
  | INCLUDE  -> "INCLUDE"
  | INFIXOP0 _ -> "INFIXOP0"
  | AT  -> "AT"
  | ATAT  -> "ATAT"
  | INFIXOP1 _ -> "INFIXOP1"
  | INFIXOP2 _ -> "INFIXOP2"
  | INFIXOP3 _ -> "INFIXOP3"
  | INFIXOP4 _ -> "INFIXOP4"
  | DOTOP _ -> "DOTOP"
  | LETOP _ -> "LETOP"
  | ANDOP _ -> "ANDOP"
  | INHERIT  -> "INHERIT"
  | INITIALIZER  -> "INITIALIZER"
  | INT _ -> "INT"
  | HASH_INT _ -> "HASH_INT"
  | KIND_ABBREV  -> "KIND_ABBREV"
  | KIND_OF  -> "KIND_OF"
  | LABEL _ -> "LABEL"
  | LAZY  -> "LAZY"
  | LBRACE  -> "LBRACE"
  | LBRACELESS  -> "LBRACELESS"
  | LBRACKET  -> "LBRACKET"
  | LBRACKETBAR  -> "LBRACKETBAR"
  | LBRACKETCOLON  -> "LBRACKETCOLON"
  | LBRACKETLESS  -> "LBRACKETLESS"
  | LBRACKETGREATER  -> "LBRACKETGREATER"
  | LBRACKETPERCENT  -> "LBRACKETPERCENT"
  | LBRACKETPERCENTPERCENT  -> "LBRACKETPERCENTPERCENT"
  | LESS  -> "LESS"
  | LESSLBRACKET -> "LESSLBRACKET"
  | LESSMINUS  -> "LESSMINUS"
  | LET  -> "LET"
  | LIDENT _ -> "LIDENT"
  | LOCAL  -> "LOCAL"
  | LPAREN  -> "LPAREN"
  | LBRACKETAT  -> "LBRACKETAT"
  | LBRACKETATAT  -> "LBRACKETATAT"
  | LBRACKETATATAT  -> "LBRACKETATATAT"
  | MATCH  -> "MATCH"
  | METHOD  -> "METHOD"
  | MINUS  -> "MINUS"
  | MINUSDOT  -> "MINUSDOT"
  | MINUSGREATER  -> "MINUSGREATER"
  | MOD  -> "MOD"
  | MODULE  -> "MODULE"
  | MUTABLE  -> "MUTABLE"
  | NEW  -> "NEW"
  | NONREC  -> "NONREC"
  | OBJECT  -> "OBJECT"
  | OF  -> "OF"
  | ONCE  -> "ONCE"
  | OPEN  -> "OPEN"
  | OPTLABEL _ -> "OPTLABEL"
  | OR  -> "OR"
  | OVERWRITE  -> "OVERWRITE"
  | PERCENT  -> "PERCENT"
  | PLUS  -> "PLUS"
  | PLUSDOT  -> "PLUSDOT"
  | PLUSEQ  -> "PLUSEQ"
  | PREFIXOP _ -> "PREFIXOP"
  | PRIVATE  -> "PRIVATE"
  | QUESTION  -> "QUESTION"
  | QUOTE  -> "QUOTE"
  | RBRACE  -> "RBRACE"
  | RBRACKET  -> "RBRACKET"
  | RBRACKETGREATER  -> "RBRACKETGREATER"
  | REC  -> "REC"
  | RPAREN  -> "RPAREN"
  | SEMI  -> "SEMI"
  | SEMISEMI  -> "SEMISEMI"
  | HASH  -> "HASH"
  | HASH_SUFFIX  -> "HASH_SUFFIX"
  | HASHOP _ -> "HASHOP"
  | SIG  -> "SIG"
  | STACK  -> "STACK"
  | STAR  -> "STAR"
  | STRING _ -> "STRING"
  | QUOTED_STRING_EXPR _ -> "QUOTED_STRING_EXPR"
  | QUOTED_STRING_ITEM _ -> "QUOTED_STRING_ITEM"
  | STRUCT  -> "STRUCT"
  | THEN  -> "THEN"
  | TILDE  -> "TILDE"
  | TO  -> "TO"
  | TRUE  -> "TRUE"
  | TRY  -> "TRY"
  | TYPE  -> "TYPE"
  | UIDENT _ -> "UIDENT"
  | UNDERSCORE  -> "UNDERSCORE"
  | UNIQUE  -> "UNIQUE"
  | VAL  -> "VAL"
  | VIRTUAL  -> "VIRTUAL"
  | WHEN  -> "WHEN"
  | WHILE  -> "WHILE"
  | WITH  -> "WITH"
  | COMMENT _ -> "COMMENT"
  | DOCSTRING _ -> "DOCSTRING"
  | EOL  -> "EOL"

  let equals t1 t2 = match t1, t2 with
  | Parser_tokens.AMPERAMPER , Parser_tokens.AMPERAMPER -> true
  | AMPERSAND , AMPERSAND -> true
  | AND , AND -> true
  | AS , AS -> true
  | ASSERT , ASSERT -> true
  | BACKQUOTE , BACKQUOTE -> true
  | BANG , BANG -> true
  | BAR , BAR -> true
  | BARBAR , BARBAR -> true
  | BARRBRACKET , BARRBRACKET -> true
  | BEGIN , BEGIN -> true
  | CHAR _, CHAR _-> true
  | HASH_CHAR _, HASH_CHAR _-> true
  | CLASS , CLASS -> true
  | COLON , COLON -> true
  | COLONCOLON , COLONCOLON -> true
  | COLONEQUAL , COLONEQUAL -> true
  | COLONGREATER , COLONGREATER -> true
  | COLONRBRACKET , COLONRBRACKET -> true
  | COMMA , COMMA -> true
  | CONSTRAINT , CONSTRAINT -> true
  | DO , DO -> true
  | DOLLAR, DOLLAR -> true
  | DONE , DONE -> true
  | DOT , DOT -> true
  | DOTDOT , DOTDOT -> true
  | DOTHASH , DOTHASH -> true
  | DOWNTO , DOWNTO -> true
  | ELSE , ELSE -> true
  | END , END -> true
  | EOF , EOF -> true
  | EQUAL , EQUAL -> true
  | EXCEPTION , EXCEPTION -> true
  | EXCLAVE , EXCLAVE -> true
  | EXTERNAL , EXTERNAL -> true
  | FALSE , FALSE -> true
  | FLOAT _, FLOAT _-> true
  | HASH_FLOAT _, HASH_FLOAT _-> true
  | FOR , FOR -> true
  | FUN , FUN -> true
  | FUNCTION , FUNCTION -> true
  | FUNCTOR , FUNCTOR -> true
  | GLOBAL , GLOBAL -> true
  | GREATER , GREATER -> true
  | GREATERRBRACE , GREATERRBRACE -> true
  | GREATERRBRACKET , GREATERRBRACKET -> true
  | HASHLPAREN , HASHLPAREN -> true
  | HASHLBRACE , HASHLBRACE -> true
  | IF , IF -> true
  | IN , IN -> true
  | INCLUDE , INCLUDE -> true
  | INFIXOP0 _, INFIXOP0 _-> true
  | AT , AT -> true
  | ATAT , ATAT -> true
  | INFIXOP1 _, INFIXOP1 _-> true
  | INFIXOP2 _, INFIXOP2 _-> true
  | INFIXOP3 _, INFIXOP3 _-> true
  | INFIXOP4 _, INFIXOP4 _-> true
  | DOTOP _, DOTOP _-> true
  | LETOP _, LETOP _-> true
  | ANDOP _, ANDOP _-> true
  | INHERIT , INHERIT -> true
  | INITIALIZER , INITIALIZER -> true
  | INT _, INT _-> true
  | HASH_INT _, HASH_INT _-> true
  | KIND_ABBREV , KIND_ABBREV -> true
  | KIND_OF , KIND_OF -> true
  | LABEL _, LABEL _-> true
  | LAZY , LAZY -> true
  | LBRACE , LBRACE -> true
  | LBRACELESS , LBRACELESS -> true
  | LBRACKET , LBRACKET -> true
  | LBRACKETBAR , LBRACKETBAR -> true
  | LBRACKETCOLON , LBRACKETCOLON -> true
  | LBRACKETLESS , LBRACKETLESS -> true
  | LBRACKETGREATER , LBRACKETGREATER -> true
  | LBRACKETPERCENT , LBRACKETPERCENT -> true
  | LBRACKETPERCENTPERCENT , LBRACKETPERCENTPERCENT -> true
  | LESS , LESS -> true
  | LESSLBRACKET , LESSLBRACKET -> true
  | LESSMINUS , LESSMINUS -> true
  | LET , LET -> true
  | LIDENT _, LIDENT _-> true
  | LOCAL , LOCAL -> true
  | LPAREN , LPAREN -> true
  | LBRACKETAT , LBRACKETAT -> true
  | LBRACKETATAT , LBRACKETATAT -> true
  | LBRACKETATATAT , LBRACKETATATAT -> true
  | MATCH , MATCH -> true
  | METHOD , METHOD -> true
  | MINUS , MINUS -> true
  | MINUSDOT , MINUSDOT -> true
  | MINUSGREATER , MINUSGREATER -> true
  | MOD , MOD -> true
  | MODULE , MODULE -> true
  | MUTABLE , MUTABLE -> true
  | NEW , NEW -> true
  | NONREC , NONREC -> true
  | OBJECT , OBJECT -> true
  | OF , OF -> true
  | ONCE , ONCE -> true
  | OPEN , OPEN -> true
  | OPTLABEL _, OPTLABEL _-> true
  | OR , OR -> true
  | OVERWRITE , OVERWRITE -> true
  | PERCENT , PERCENT -> true
  | PLUS , PLUS -> true
  | PLUSDOT , PLUSDOT -> true
  | PLUSEQ , PLUSEQ -> true
  | PREFIXOP _, PREFIXOP _-> true
  | PRIVATE , PRIVATE -> true
  | QUESTION , QUESTION -> true
  | QUOTE , QUOTE -> true
  | RBRACE , RBRACE -> true
  | RBRACKET , RBRACKET -> true
  | RBRACKETGREATER , RBRACKETGREATER -> true
  | REC , REC -> true
  | RPAREN , RPAREN -> true
  | SEMI , SEMI -> true
  | SEMISEMI , SEMISEMI -> true
  | HASH , HASH -> true
  | HASH_SUFFIX , HASH_SUFFIX -> true
  | HASHOP _, HASHOP _-> true
  | SIG , SIG -> true
  | STACK , STACK -> true
  | STAR , STAR -> true
  | STRING _, STRING _-> true
  | QUOTED_STRING_EXPR _, QUOTED_STRING_EXPR _-> true
  | QUOTED_STRING_ITEM _, QUOTED_STRING_ITEM _-> true
  | STRUCT , STRUCT -> true
  | THEN , THEN -> true
  | TILDE , TILDE -> true
  | TO , TO -> true
  | TRUE , TRUE -> true
  | TRY , TRY -> true
  | TYPE , TYPE -> true
  | UIDENT _, UIDENT _-> true
  | UNDERSCORE , UNDERSCORE -> true
  | UNIQUE , UNIQUE -> true
  | VAL , VAL -> true
  | VIRTUAL , VIRTUAL -> true
  | WHEN , WHEN -> true
  | WHILE , WHILE -> true
  | WITH , WITH -> true
  | COMMENT _, COMMENT _-> true
  | DOCSTRING _, DOCSTRING _-> true
  | EOL , EOL -> true
  | _ -> false
end

type attachment = Before | After | Floating

type comment = {
  text: string;
  attachement: attachment;
  explicitely_inserted: bool ref;
}

type desc =
  | Token of Parser_tokens.token
  | Opt_token of Parser_tokens.token
  | Comment of comment
  | Child_node

type elt = {
  desc: desc;
  pos: Lexing.position;
}

type seq = elt list

let desc_as_string = function
  | Token t ->
    if Dbg_print.dbg then
      Raw.to_string t
    else
      "tok"
  | Opt_token t ->
    if Dbg_print.dbg then
      "optional(" ^ Raw.to_string t ^ ")"
    else
      "opttok"
  | Comment c -> Printf.sprintf "(* %s *)" c.text
  | Child_node -> "child"

let pp_elt ppf e = Format.pp_print_string ppf (desc_as_string e.desc)

let pp_seq =
  let open Format in
  pp_print_list
    ~pp_sep:(fun ppf () -> fprintf ppf ",@ ")
    pp_elt

module Indexed_list = struct
  open Lexing

  type cell =
    | Empty
    | Node of {
        pos: position;
        mutable value: elt;
        mutable prev: cell;
        mutable next: cell;
      }

  module Tbl =
    Hashtbl.Make(struct
      type t = position
      let equal t1 t2 = Int.equal t1.pos_cnum t2.pos_cnum
      let hash t = Hashtbl.hash t.pos_cnum
    end)

  type t = {
    tbl: cell Tbl.t;
    mutable last: cell;
  }
  let create () : t = { tbl = Tbl.create 42; last = Empty }

  let append t ~pos desc =
    let elt = { desc; pos } in
    let node = Node { pos; value = elt; prev = t.last; next = Empty } in
    Tbl.add t.tbl pos node;
    begin match t.last with
    | Empty -> ()
    | Node n -> n.next <- node
    end;
    t.last <- node

  let insert_child t pos =
    dprintf "Inserting Child_node for empty reduction at pos %d:%d@\n"
      pos.pos_lnum (pos.pos_cnum - pos.pos_bol);
    let[@warning "-8"] (Node node) as cell =
      let value = { desc = Child_node; pos } in
      Node { pos; value; prev = Empty; next = Empty }
    in
    Tbl.add t.tbl pos cell;
    match t.last with
    | Empty -> t.last <- cell
    | Node _ as last ->
      let rec aux = function
        | Empty -> assert false
        | Node n as n_cell ->
          if n.pos < pos then (
            node.next <- n.next;
            begin match n.next with
            | Empty -> ()
            | Node next ->
              dprintf "prev = %a@ next = %a@."
                pp_elt n.value pp_elt next.value;
              next.prev <- cell
            end;
            n.next <- cell;
            node.prev <- n_cell
          ) else (
            match n.prev with
            | Node _ -> aux n.prev
            | Empty ->
              node.next <- n_cell;
              n.prev <- cell
          )
      in
      aux last

  let consume t start stop =
    dprintf "consume %d:%d -> %d:%d@."
      start.pos_lnum (start.pos_cnum - start.pos_bol)
      stop.pos_lnum (stop.pos_cnum - stop.pos_bol);
    let rec aux ~replaced_by = function
      | Empty -> invalid_arg "Tokens.consume"
      | Node n as curr ->
        if n.pos >= stop then (
          (* Stop is an endpos, and we index by startpos *)
          [], curr
        ) else (
          Tbl.replace t.tbl n.pos replaced_by;
          let seq, after = aux ~replaced_by n.next in
          n.value :: seq, after
        )
    in
    if start = stop then (
      (* empty rule / epsilon reduction
         We will return an empty list of tokens, but we must remember to insert
         a [Child_node] entry at that position, because the parent node in the
         token tree will expect a child, even if it's empty. *)
      insert_child t start;
      []
    ) else
      match Tbl.find t.tbl start with
      | Empty -> invalid_arg "Tokens.consume"
      | Node start_node as cell ->
        let seq, after = aux ~replaced_by:cell cell in
        start_node.value <- { start_node.value with desc = Child_node };
        start_node.next <- after;
        begin match after with
        | Empty -> ()
        | Node n -> n.prev <- cell
        end;
        seq

  let consume t start stop =
    let res = consume t start stop in
    dprintf "consumed: @[<h 2>%a@]@." pp_seq res;
    res

  let consume_all t =
    let rec aux acc = function
      | Empty ->
        dprintf "consumed_all: @[<h 2>%a@]@." pp_seq acc;
        acc
      | Node n -> aux (n.value :: acc) n.prev
    in
    aux [] t.last


  let global = create ()
  let reset_global () =
    Tbl.clear global.tbl;
    global.last <- Empty
end

let add ~pos desc = Indexed_list.(append global ~pos desc)
let at (startpos,endpos) = Indexed_list.(consume global startpos endpos)

let rec replace_first_child ~subst = function
  | [] -> invalid_arg "Tokens.replace_first_child: never saw a child"
  | { desc = Child_node; _ } :: xs -> subst @ xs
  | x :: xs -> x :: replace_first_child ~subst xs

let attach_leading_and_trailing tokens =
  let all_tokens = Indexed_list.(consume_all global) in
  replace_first_child ~subst:tokens all_tokens

let reset = Indexed_list.reset_global

let is_child t = t.desc = Child_node

let is_comment t =
  match t.desc with
  | Comment _ -> true
  | _ -> false

let is_token t =
  match t.desc with
  | Token _ | Opt_token _ -> true
  | _ -> false
