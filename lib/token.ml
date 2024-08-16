type token_type =
  (* Single-character tokens *)
  | LEFT_PAREN
  | RIGHT_PAREN
  | LEFT_BRACE
  | RIGHT_BRACE
  | COMMA
  | DOT
  | MINUS
  | PLUS
  | SEMICOLON
  | SLASH
  | STAR
  (* One or two characters tokens *)
  | BANG
  | BANG_EQUAL
  | EQUAL
  | EQUAL_EQUAL
  | GREATER
  | GREATER_EQUAL
  | LESS
  | LESS_EQUAL
  (* Literals *)
  | IDENTIFIER
  | STRING
  | NUMBER
  (* Keywords *)
  | AND
  | CLASS
  | ELSE
  | FALSE
  | FUN
  | FOR
  | IF
  | OR
  | PRINT
  | RETURN
  | SUPER
  | THIS
  | TRUE
  | VAR
  | WHILE
  | NIL
  | EOF

type literal = StrLiteral of string | NumLiteral of float | None

type token = {
  token_type : token_type;
  lexeme : string;
  literal : literal;
  line : int;
}

type identifiers = (string, token_type) Hashtbl.t

(* TODO: This probably can be cleaner, maybe map token_type some way? *)
let get_identifiers : identifiers =
  let hashtbl = Hashtbl.create 38 in
  Hashtbl.add hashtbl "and" AND;
  Hashtbl.add hashtbl "class" CLASS;
  Hashtbl.add hashtbl "else" ELSE;
  Hashtbl.add hashtbl "false" FALSE;
  Hashtbl.add hashtbl "for" FOR;
  Hashtbl.add hashtbl "fun" FUN;
  Hashtbl.add hashtbl "if" IF;
  Hashtbl.add hashtbl "nil" NIL;
  Hashtbl.add hashtbl "or" OR;
  Hashtbl.add hashtbl "print" PRINT;
  Hashtbl.add hashtbl "return" RETURN;
  Hashtbl.add hashtbl "super" SUPER;
  Hashtbl.add hashtbl "this" THIS;
  Hashtbl.add hashtbl "true" TRUE;
  Hashtbl.add hashtbl "var" VAR;
  Hashtbl.add hashtbl "while" WHILE;
  hashtbl
