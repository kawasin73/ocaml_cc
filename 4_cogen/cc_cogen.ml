(* 中間言語 (IR) ? そんなものはない。AST から直接アセンブリを吐くのだ *)
(* IR の設計をどうすればいいのかよくわからなかったので、IR を使わず AST から直接アセンブリを
   出力しています。また、処理も簡単にするためにレジスタベースではなく、スタックベースになって
   います。
 *)
(* 今年の正月に C で C コンパイラを実装していたのでそれを参考にしました。 *)
(* https://github.com/kawasin73/kcc *)
(* このコードは、実行環境の Dockerfile も含めて以下の github に公開しています。 *)
(* https://github.com/kawasin73/ocaml_cc *)

(* ============= *)
(* Utility 関数群 *)
(* ============= *)

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

(* l の文字列リストを改行で連結する *)
let multi_string l =
  (String.concat "\n" l)
;;

(* l のリストの要素の先頭 n 要素を抽出したリストを返す *)
let take n l =
  let combine i v = (i, v) in
  let unwrap vv =
    let (_, v) = vv in
    v
  in
  let first_nth n vv =
    let (i, _) = vv in
    i < n
  in
  List.map unwrap (List.filter (first_nth n) (List.mapi combine l))
;;

(* [a...b) の整数リストを返す *)
let create_index a b =
  let rec create_index_rec l a b =
    if a = b then
      l
    else
      create_index_rec ((b-1) :: l) a (b-1)
  in
  create_index_rec [] a b
;;

(* アセンブリファイル内で一意なラベルを生成する *)
let gen_label =
  let label_count = ref 0 in
  fun () -> incr label_count; (sp ".L%d" !label_count)
;;

(* ================= *)
(* 関数呼び出しレジスタ *)
(* ================= *)

let args_regs = ["rdi"; "rsi"; "rdx"; "rcx"; "r8"; "r9"]
;;

(* ============= *)
(* 環境          *)
(* ============= *)

class environment (end_label:string) len_stack env_vars =
object
  val end_label = end_label
  val len_stack = len_stack
  val env_vars = env_vars
  val continue_label = ""
  val break_label = ""
  method end_label =
    end_label
  method continue_label =
    continue_label
  method break_label =
    break_label
  method find_var_offset name =
    let find_var v =
      let n, _ = v in n = name
    in
    let _, offset = List.find find_var env_vars in
    offset
  method add_vars (new_vars: (Cc_ast.type_expr * string) list) =
    let conv_env_var i var =
      let (_, name) = var in
      (name, -(i+len_stack+1) * 8)
    in
    let new_env_vars = env_vars @ List.mapi conv_env_var new_vars in
    {<len_stack = (len_stack + List.length new_vars); env_vars = new_env_vars>}
  method update_loop_label continue_label break_label =
    {<continue_label = continue_label; break_label = break_label>}
end
;;

(* ============= *)
(* EXPR の出力    *)
(* ============= *)

let rec cogen_expr env ast =
  match ast with
  | Cc_ast.EXPR_NUM(v) -> (multi_string [
      sp "\tmov\t$%d, %%rax" v;
      "\tpushq\t%rax"
    ])

  | Cc_ast.EXPR_VAR(name) ->
    let offset = env#find_var_offset name in
    (multi_string [
      sp "\tmovq\t%d(%%rbp), %%rax" offset;
      "\tpushq\t%rax"
    ])

  | Cc_ast.EXPR_BIN_OP(op, a, b) ->
    (match op with
    | Cc_ast.BIN_OP_EQ ->        (* = *)
      let code_b = cogen_expr env b in
      (match a with
      (* a must be variable name *)
      | Cc_ast.EXPR_VAR(name) ->
        let offset = env#find_var_offset name in
        multi_string [
          code_b;
          "\tpopq\t%rax";
          sp "\tmovq\t%%rax, %d(%%rbp)" offset;
          "\tpushq\t%rax";
        ]
      )
    | _ ->
      (* else *)
      let code_a = cogen_expr env a in
      let code_b = cogen_expr env b in
      let op_code = (match op with
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
    )

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

  (* 関数呼び出し *)
  | Cc_ast.EXPR_CALL(name, args) ->
    let set_arg i =
      sp "\tpopq\t%%%s" (List.nth args_regs i)
    in
    let len_args = List.length args in
    let idx = take 6 (create_index 0 len_args) in
    let stack_arg_size = if len_args >= 6 then len_args - 6 else 0 in
    let code_set_args = map_cat "\n" set_arg idx in
    let code_args = map_cat "\n" (cogen_expr env) (List.rev args) in
    multi_string [
      (* スタックを16バイトに揃える *)
      (* <現在のRSPの値> + <第7引数以降のスタックサイズ> + <RSPの操作量の保存領域> が
      16 の倍数になるように揃える *)
      (* 全て 8 バイトであるため、下から3ビット目が 1 になっていれば RSP から 8 を引き、
       0 であれば、RSP は変えない。 *)
       "\tmovq\t$8, %rdi";
      sp "\tmovq\t$%d, %%rax" ((stack_arg_size+1) * -8);
      "\taddq\t%rsp, %rax";
      "\tandq\t%rdi, %rax";
      "\tsubq\t%rax, %rsp";

      (* RSP を揃えた量を保存する *)
      "\tpushq\t%rax";
      (* 全ての引数を逆順にスタック上に積み重ねる *)
      code_args;
      (* 第1~6引数をレジスタにポップする。第7引数以降はRSPより上に配置されているためこのまま関数呼び出しができる *)
      code_set_args;
      sp "\tcall\t%s" name;

      (* 第7引数以降の分のスタックを縮める *)
      sp "\taddq\t$%d, %%rsp" (stack_arg_size * 8);

      (* 保存していた 16 バイト揃えのための RSP への操作を元に戻す *)
      "\tpopq\t%rbx";
      "\taddq\t%rbx, %rsp";

      (* 結果をプッシュする *)
      "\tpushq\t%rax"
    ]
;;

(* ============= *)
(* STMT の出力    *)
(* ============= *)

let rec cogen_stmt env ast =
  match ast with
  | Cc_ast.STMT_EMPTY -> ""

  | Cc_ast.STMT_CONTINUE -> sp "\tjmp\t%s" env#continue_label

  | Cc_ast.STMT_BREAK -> sp "\tjmp\t%s" env#break_label

  | Cc_ast.STMT_RETURN(v) ->
    let code = cogen_expr env v in
    multi_string [
      code;
      "\tpopq\t%rax";
      (* `ret` ではなく、エピローグを実行するためにジャンプする *)
      sp "\tjmp\t%s" env#end_label
    ]

  | Cc_ast.STMT_EXPR(expr) ->
    let code = cogen_expr env expr in
    multi_string [
      code;
      "\tpopq\t%rax"
    ]

  | Cc_ast.STMT_COMPOUND(vars, stmts) ->
    (* 宣言された変数を環境に加える。あらかじめその変数のためのスタック領域はプロローグで拡張済み *)
    let new_env = env#add_vars vars in
    map_cat "\n" (cogen_stmt new_env) stmts

  | Cc_ast.STMT_IF(cond, stmt, else_stmt) ->
    let label_else = gen_label () in
    let label_end = gen_label () in
    let code_cond = cogen_expr env cond in
    let code_stmt = cogen_stmt env stmt in
    let code_else = cogen_stmt env else_stmt in
    multi_string [
      code_cond;
      "\tpopq\t%rax";
      "\ttestq\t%rax, %rax";
      sp "\tje\t%s" label_else;
      code_stmt;
      sp "\tjmp\t%s" label_end;
      sp "%s:" label_else;
      code_else;
      sp "%s:" label_end
    ]

  | Cc_ast.STMT_WHILE(cond, stmt) ->
    let label_cond = gen_label () in
    let label_end = gen_label () in
    let code_cond = cogen_expr env cond in
    (* continue と break した時にジャンプするラベルを更新 *)
    let new_env = env#update_loop_label label_cond label_end in
    let code_stmt = cogen_stmt new_env stmt in
    multi_string [
      sp "%s:" label_cond;
      code_cond;
      "\tpopq\t%rax";
      "\ttestq\t%rax, %rax";
      sp "\tje\t%s" label_end;
      code_stmt;
      sp "\tjmp\t%s" label_cond;
      sp "%s:" label_end;
    ]
;;

(* =========================== *)
(* 関数のエピローグとプロローグ出力 *)
(* =========================== *)

(* 関数の引数をレジスタからスタック上にコピーする code と、それぞれの引数の RBP からのオフセットのリストを返す *)
(* それに伴うスタックの拡張したサイズも返す *)
let cogen_args args =
  let cogen_arg i arg =
    sp "\tmovq\t%%%s, %d(%%rbp)" (List.nth args_regs i) (-(i+1)*8)
  in
  let env_arg i arg =
    let _, name = arg in
    if i < 6 then
      (* 第1~6引数の rbp からのオフセット  *)
      (name, -(i+1) * 8)
    else
      (* 第7引数以降 *)
      (* rbp から第7引数までは 3 * 8 バイト空いている（返り番地、rbp、rbx） *)
      (* rbp からのオフセットは (i - 6 + 3) * 8 *)
      (* i == 7 の時、4 * 8 *)
      (name, (i-3)*8)
  in
  let reg_args = take 6 args in
  let code = map_cati "\n" cogen_arg reg_args in
  let env = List.mapi env_arg args in
  code, env, List.length reg_args
;;

(* スタックを拡張させる量を決定し、`subq` により拡張させる code を返す *)
(* FUNDEF の最初の AST ノードは必ず STMT_COMPOUND であるという前提 *)
(* また、FUNDEF の最初の AST ノード以外では変数の定義は行われないという前提 *)
(* これらの前提があるため AST ノードを探索することなくスタックの拡張量を決定している。 *)
(* もし、これらの前提が崩れた場合は、スタックが破壊される。 *)
let cogen_extend_stack args stmt =
  match stmt with
  | Cc_ast.STMT_COMPOUND(vars, _) ->
    let arg_len = (if (List.length args) < 6 then
      List.length args
    else
      6) in
    let size = 8 * (arg_len + (List.length vars)) in
    sp "\tsubq\t$%d, %%rsp" size

(* i 番目の関数 AST ノードからコードを吐き出す *)
let cogen_fundef i ast =
  let start_label = sp ".LFB%d" i in
  let end_label = sp ".LFE%d" i in
  match ast with
  Cc_ast.FUN_DEF(_, name, args, stmt) ->
  let prologue = multi_string [
    "\t.p2align 4,,15";
    sp "\t.globl\t%s" name;
    sp "\t.type\t%s, @function" name;
    sp "%s:" name;
    sp "%s:" start_label;
    "\t.cfi_startproc";
    (* rbp と rbx は処理内で上書きされる可能性があるのでスタックに退避 *)
    "\tpushq	%rbp";
    "\tpushq	%rbx";
    (* rbp を更新する *)
    "\tmovq	%rsp, %rbp"
  ] in
  let epilogue = multi_string [
    (* STMT_RETURN では、`ret` の代わりにこのラベルに飛んでエピローグを実行する *)
    sp "%s:" end_label;
    "\tmovq	%rbp, %rsp";
    "\tpopq	%rbx";
    "\tpopq	%rbp";
    "\tret";
    "\t.cfi_endproc";
    sp "\t.size\t%s, .-%s" name name
  ] in
  let code_args, env_vars, len_stack = cogen_args args in
  let code_extend = cogen_extend_stack args stmt in
  let env = new environment end_label len_stack env_vars in
  let body = cogen_stmt env stmt in
  multi_string [
    prologue;
    code_extend;
    code_args;
    body;
    epilogue;
  ]
;;

(* これを作るのが仕事 (cc.ml の cogen_file を参照 *)
let cogen_program program src_name =
  let body = (map_cati "\n" cogen_fundef program) ^ "\n" in
  (sp "\t.file\t\"%s\"\n\t.text\n" src_name) ^ body
;;
