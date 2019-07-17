{
open Cc_parse
;;
}

rule lex = parse
| [' ' '\t' '\n'] { lex lexbuf }
| "break"      { BREAK }
| "continue"   { CONTINUE }
| "else"       { ELSE }
| "if"         { IF }
| "return"     { RETURN }
| "while"      { WHILE }
| "long"       { LONG }
| "("          { LPAREN }
| ")"          { RPAREN }
| "{"          { LBRACE }
| "}"          { RBRACE }
| "*"          { ASTERISK }
| "+"          { PLUS }
| "-"          { MINUS }
| "/"          { DIV }
| "%"          { MOD }
| "!"          { BANG }
| "<"          { LT }
| ">"          { GT }
| "<="         { LEQ }
| ">="         { GEQ }
| "=="         { EQEQ }
| "!="         { NEQ }
| ";"          { SEMICOLON }
| "="          { EQ }
| ","          { COMMA }
| '-'? ['1'-'9'] ['0'-'9']* as s { NUM(int_of_string s) }
| '-'? '0' as s                  { NUM(int_of_string s) }
| ['a'-'z' '_'] ['a'-'z' '_' '0'-'9']* as s { ID(s) }
| eof          { EOF }

{

let string_of_token t =
  match t with
    NUM(s) -> Printf.sprintf "NUM(%d)" s
  | ID(s)  -> Printf.sprintf "ID(%s)" s
  | BREAK  -> Printf.sprintf "BREAK"
  | CONTINUE -> Printf.sprintf "CONTINUE"
  | ELSE -> Printf.sprintf "ELSE"
  | IF -> Printf.sprintf "IF"
  | RETURN -> Printf.sprintf "RETURN"
  | WHILE -> Printf.sprintf "WHILE"
  | LONG -> Printf.sprintf "LONG"
  | LPAREN -> Printf.sprintf "LPAREN"
  | RPAREN -> Printf.sprintf "RPAREN"
  | LBRACE -> Printf.sprintf "LBRACE"
  | RBRACE -> Printf.sprintf "RBRACE"
  | ASTERISK -> Printf.sprintf "ASTERISK"
  | PLUS -> Printf.sprintf "PLUS"
  | MINUS -> Printf.sprintf "MINUS"
  | DIV -> Printf.sprintf "DIV"
  | MOD -> Printf.sprintf "MOD"
  | BANG -> Printf.sprintf "BANG"
  | LT -> Printf.sprintf "LT"
  | GT -> Printf.sprintf "GT"
  | LEQ -> Printf.sprintf "LEQ"
  | GEQ -> Printf.sprintf "GEQ"
  | EQEQ -> Printf.sprintf "EQEQ"
  | NEQ -> Printf.sprintf "NEQ"
  | SEMICOLON -> Printf.sprintf "SEMICOLON"
  | EQ -> Printf.sprintf "EQ"
  | COMMA -> Printf.sprintf "COMMA"
  | EOF -> "EOF"
;;

let print_token t =
  Printf.printf "%s\n" (string_of_token t);
  t
;;

let lex_string s =
  let rec loop b = 
    match print_token (lex b) with
      EOF -> ()
    | _ -> loop b
  in
  loop (Lexing.from_string s)
;;
}
