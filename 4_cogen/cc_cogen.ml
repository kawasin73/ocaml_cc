(* 中間言語 (IR) *)

let sp = Printf.sprintf
;;

let map_append f l =        (* (f l0) @ (f l1) @ ... @ (f ln1-)*)
  List.concat (List.map f l)
;;

let map_cat delim f l =
  String.concat delim (List.map f l)
;;

let map_cati delim f l =
  String.concat delim (List.mapi f l)
;;

let multi_string l =
  (String.concat "\n" l)
;;

let rec cogen_expr ast =
  match ast with
  | Cc_ast.EXPR_NUM(v) -> (multi_string [
    sp "\tmov\t$%d, %%rax" v;
    "\tpushq\t%rax"
  ])
  (* | Cc_ast.EXPR_VAR(v) -> VAR_VAR *)
  | Cc_ast.EXPR_BIN_OP(op, a, b) ->
    let code_a = cogen_expr(a) in
    let code_b = cogen_expr(b) in
    let op_code = (match op with
    | Cc_ast.BIN_OP_PLUS -> "\taddq\t%rbx, %rax"
    ) in
    (multi_string [
      code_a;
      code_b;
      "\tpopq\t%rbx";
      "\tpopq\t%rax";
      op_code;
      "\tpushq\t%rax"
    ])
;;

let rec cogen_stmt ast =
  match ast with
  | Cc_ast.STMT_RETURN(v) ->
    let code = cogen_expr(v) in
    multi_string [
      code;
      "\tpopq\t%rax";
      "\tret"
    ]
  | Cc_ast.STMT_COMPOUND(_, stmts) -> map_cat "\n" cogen_stmt stmts
;;

let cogen_fundef i ast =
  match ast with
  Cc_ast.FUN_DEF(_, name, _, stmt) ->
  let header = sp "	.p2align 4,,15
	.globl	%s
	.type	%s, @function
%s:
.LFB%d:
	.cfi_startproc
" name name name i in
  let footer = sp "	.cfi_endproc
.LFE%d:
	.size	%s, .-%s" i name name in
  let b = cogen_stmt stmt in
  let body = if b = "" then "\tret\n" else b ^ "\n" in
  header ^ body ^ footer
;;

(* これを作るのが仕事 (cc.ml の cogen_file を参照 *)
let cogen_program program src_name =
  let body = (map_cati "\n" cogen_fundef program) ^ "\n" in
  (sp "\t.file\t\"%s\"\n\t.text\n" src_name) ^ body
;;
