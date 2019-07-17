ファイル
===============

* cc_lex.mll   --- 字句解析
* cc_parse.mly --- 構文解析
* cc_ast.ml    --- 構文木の定義
* cc.ml        --- トップレベル (parse_string という関数だけ)

コンパイル
===============

$ make

実行
===============

$ ./cc.top -I _build
        OCaml version 4.05.0

(* 文字列を直接与えて構文解析 *)
# Cc.parse_string "long f(long x) { return x + 1; }";;
- : Cc_ast.definition list =
[Cc_ast.FUN_DEF
  (Cc_ast.TYPE_LONG, "f", [(Cc_ast.TYPE_LONG, "x")],
   Cc_ast.STMT_COMPOUND
    ([],
     [Cc_ast.STMT_RETURN
       (Cc_ast.EXPR_BIN_OP (Cc_ast.BIN_OP_PLUS, Cc_ast.EXPR_VAR "x",
         Cc_ast.EXPR_NUM 1))]))]

(* ファイルの中身を構文解析 *)
# Cc.parse_file "a.c";;
- : Cc_ast.definition list =
[Cc_ast.FUN_DEF
  (Cc_ast.TYPE_LONG, "f", [(Cc_ast.TYPE_LONG, "x")],
   Cc_ast.STMT_COMPOUND
    ([],
     [Cc_ast.STMT_RETURN
       (Cc_ast.EXPR_BIN_OP (Cc_ast.BIN_OP_PLUS, Cc_ast.EXPR_VAR "x",
         Cc_ast.EXPR_NUM 1))]))]
