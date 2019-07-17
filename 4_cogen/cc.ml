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

(* buf -> ir -> asm *)
let cogen_buf b src_name =
  let prog = parse_buf b in
  Cc_cogen.cogen_program prog src_name
;;

(* file -> ir -> asm *)
let cogen_file filename_c filename_s =
  let ch = open_in filename_c in
  let b = Lexing.from_channel ch in
  let insn_str = cogen_buf b filename_c in
  let _ = close_in ch in
  let och = open_out filename_s in
  let _ = Printf.fprintf och "%s" insn_str in
  close_out och
;;

(* entry point for non-interactive version *)
let main () =
  if not (!Sys.interactive) then
    cogen_file Sys.argv.(1) Sys.argv.(2)
;;

main ()
;;
