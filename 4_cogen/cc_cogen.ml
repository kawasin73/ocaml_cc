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

let rec cogen_expr env ast =
  match ast with
  | Cc_ast.EXPR_NUM(v) -> (multi_string [
      sp "\tmov\t$%d, %%rax" v;
      "\tpushq\t%rax"
    ])
  | Cc_ast.EXPR_VAR(v) ->
    let find_var name v =
      let n, _ = v in n = name
    in
    let (_, env_vars) = env in
    let _, offset = List.find (find_var v) env_vars in
    (multi_string [
      sp "\tmovq\t%d(%%rbp), %%rax" offset;
      "\tpushq\t%rax"
    ])
  | Cc_ast.EXPR_BIN_OP(op, a, b) ->
    let code_a = cogen_expr env a in
    let code_b = cogen_expr env b in
    let op_code = (match op with
    | Cc_ast.BIN_OP_EQ ->        (* = *)
      "TODO"
    | Cc_ast.BIN_OP_EQEQ ->      (* == *)
      multi_string [
        "\tcmpq\t%rbx, %rax";
        "\tsete\t%al";
        "\tmovzbq\t%al, %rax"
      ]
    | Cc_ast.BIN_OP_NEQ ->       (* != *)
      multi_string [
        "\tcmpq\t%rbx, %rax";
        "\tsetne\t%al";
        "\tmovzbq\t%al, %rax"
      ]
    | Cc_ast.BIN_OP_LT ->        (* < *)
      multi_string [
        "\tcmpq\t%rbx, %rax";
        "\tsetl\t%al";
        "\tmovzbq\t%al, %rax"
      ]
    | Cc_ast.BIN_OP_GT ->        (* > *)
      multi_string [
        "\tcmpq\t%rbx, %rax";
        "\tsetg\t%al";
        "\tmovzbq\t%al, %rax"
      ]
    | Cc_ast.BIN_OP_LEQ ->       (* <= *)
      multi_string [
        "\tcmpq\t%rbx, %rax";
        "\tsetle\t%al";
        "\tmovzbq\t%al, %rax"
      ]
    | Cc_ast.BIN_OP_GEQ ->       (* >= *)
      multi_string [
        "\tcmpq\t%rbx, %rax";
        "\tsetge\t%al";
        "\tmovzbq\t%al, %rax"
      ]
    | Cc_ast.BIN_OP_PLUS ->      (* + *)
      "\taddq\t%rbx, %rax"
    | Cc_ast.BIN_OP_MINUS ->     (* - *)
      "\tsubq\t%rbx, %rax"
    | Cc_ast.BIN_OP_MUL ->       (* * *)
      "\timulq\t%rbx, %rax"
    | Cc_ast.BIN_OP_DIV ->       (* / *)
      multi_string [
        "\tcqto";
        "\tidivq\t%rbx"
      ]
    | Cc_ast.BIN_OP_MOD ->       (* % *)
      multi_string [
        "\tcqto";
        "\tidivq\t%rbx";
        "\tmovq\t%rdx, %rax"
      ]
    ) in
    (multi_string [
      code_a;
      code_b;
      "\tpopq\t%rbx";
      "\tpopq\t%rax";
      op_code;
      "\tpushq\t%rax"
    ])
  | Cc_ast.EXPR_UN_OP(op, expr) ->
    let code = cogen_expr env expr in
    (match op with
    | Cc_ast.UN_OP_PLUS -> code
    | Cc_ast.UN_OP_MINUS -> multi_string [
        code;
        "\tpopq\t%rax";
        "\timulq\t$-1, %rax";
        "\tpushq\t%rax"
      ]
    | Cc_ast.UN_OP_BANG -> multi_string [
        code;
        "\tpopq\t%rbx";
        "\txorl\t%eax, %eax";
        "\ttestq\t%rbx, %rbx";
        "\tsete\t%al";
        "\tpushq\t%rax"
      ]
    )
;;

let rec cogen_stmt env ast =
  match ast with
  | Cc_ast.STMT_RETURN(v) ->
    let code = cogen_expr env v in
    let (end_label, _) = env in
    multi_string [
      code;
      "\tpopq\t%rax";
      sp "\tjmp\t%s" end_label
    ]
  | Cc_ast.STMT_COMPOUND(_, stmts) -> map_cat "\n" (cogen_stmt env) stmts
;;

let args_regs = ["rdi"; "rsi"; "rdx"; "rcx"; "r8"; "r9"]
;;

let cogen_args args =
  let cogen_arg i arg =
    if i < 6 then
      (multi_string [
        sp "\tmovq\t%%%s, %d(%%rbp)" (List.nth args_regs i) (-(i+1)*8)
      ])
    else
      "TODO"
  in
  let env_arg i arg =
    let _, name = arg in
    if i < 6 then
      (name, -(i+1) * 8)
    else
      (name, (i-5)*8)
  in
  let code = map_cati "\n" cogen_arg args in
  let env = List.mapi env_arg args in
  code, env
;;

let cogen_extend_stack args stmt =
  match stmt with
  | Cc_ast.STMT_COMPOUND(vars, _) ->
    let arg_len = (if (List.length args) < 6 then
      List.length args
    else
      6) in
    let size = 8 * (arg_len + (List.length vars)) in
    sp "\tsubq\t$%d, %%rsp" size


let cogen_fundef i ast =
  let start_label = sp ".LFB%d" i in
  let end_label = sp ".LFE%d" i in
  match ast with
  Cc_ast.FUN_DEF(_, name, args, stmt) ->
  let header = multi_string [
    "\t.p2align 4,,15";
    sp "\t.globl\t%s" name;
    sp "\t.type\t%s, @function" name;
    sp "%s:" name;
    sp "%s:" start_label;
    "\t.cfi_startproc";
    "\tpushq	%rbp";
    "\tmovq	%rsp, %rbp"
  ] in
  let footer = multi_string [
    "\t.cfi_endproc";
    sp "%s:" end_label;
    "\tmovq	%rbp, %rsp";
    "\tpopq	%rbp";
    "\tret";
    sp "\t.size\t%s, .-%s" name name
  ] in
  let code_args, env_vars = cogen_args args in
  let code_extend = cogen_extend_stack args stmt in
  let b = cogen_stmt (end_label, env_vars) stmt in
  let body = if b = "" then (sp "\tjmp	%s" end_label) else b in
  multi_string [
    header;
    code_extend;
    code_args;
    body;
    footer;
  ]
;;

(* これを作るのが仕事 (cc.ml の cogen_file を参照 *)
let cogen_program program src_name =
  let body = (map_cati "\n" cogen_fundef program) ^ "\n" in
  (sp "\t.file\t\"%s\"\n\t.text\n" src_name) ^ body
;;
