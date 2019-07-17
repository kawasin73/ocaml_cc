(* 中間言語 (IR) *)

let sp = Printf.sprintf
;;

let map_append f l =        (* (f l0) @ (f l1) @ ... @ (f ln1-)*)
  List.concat (List.map f l)
;;

let map_cat delim f l =
  String.concat delim (List.map f l)
;;

(* これを作るのが仕事 (cc.ml の cogen_file を参照 *)
let cogen_program program src_name =
  "<cogen_program not implemented yet>"
;;
