(* parse the string in lexer buffer b *)
let parse_buf b =
  let prog = 
    try
      Cc_parse.program Cc_lex.lex b     (* main work *)
    with exn ->
      (* handle parse error *)
      let i = Lexing.lexeme_start b in
      let tok = Lexing.lexeme b in
      Printf.fprintf stdout
        "error: parse error at char=%d, near token '%s'\n" i tok;
      raise exn
  in
  prog
;;

(* parse a string (s) *)
let parse_string s = 
  let b = Lexing.from_string s in
  parse_buf b
;;

(* parse the string in a file (filename) *)
let parse_file filename =
  let ch = open_in filename in
  let b = Lexing.from_channel ch in
  let prog = parse_buf b in
  let _ = close_in ch in
  prog
;;

