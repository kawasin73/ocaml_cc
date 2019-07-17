open Cc_ast
;;

let sp = Printf.sprintf
;;

let mapcat delimiter f l =
  String.concat delimiter (List.map f l)
;;

(* これを実装するのが役目 *)
let rec string_of_program (program : definition list) =
  "<not implemented> you should implement the print function in cc_ast_print.ml\n"
;;

