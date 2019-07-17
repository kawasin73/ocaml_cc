(* 型は long (64 bit整数) だけ *)
type type_expr =
  TYPE_LONG
;;

(* 2項演算子 *)
type bin_op = BIN_OP_EQ         (* = *)
            | BIN_OP_EQEQ       (* == *)
            | BIN_OP_NEQ        (* != *)
            | BIN_OP_LT         (* < *)
            | BIN_OP_GT         (* > *)
            | BIN_OP_LEQ        (* <= *)
            | BIN_OP_GEQ        (* >= *)
            | BIN_OP_PLUS       (* + *)
            | BIN_OP_MINUS      (* - *)
            | BIN_OP_MUL        (* * *)
            | BIN_OP_DIV        (* / *)
            | BIN_OP_MOD        (* % *)
;;

(* 単項演算子 *)
type un_op = UN_OP_PLUS         (* + *)
           | UN_OP_MINUS        (* - *)
           | UN_OP_BANG         (* ! *)
;;

(* 式 *)
type expr =
  EXPR_NUM of int               (* 整数リテラル(0, 1, 2, 3, ...) *)
| EXPR_VAR of string            (* 変数 *)
| EXPR_BIN_OP of bin_op * expr * expr (* 2項演算 (A + B, A < B, A = B, ...) *)
| EXPR_UN_OP of un_op * expr          (* 単項演算 (+A, -A, !A) *)
| EXPR_CALL of (string * expr list)   (* 関数呼び出し f(A,B,C,...) *)
;;

(* 文 *)
type stmt =
  STMT_EMPTY                    (* ; *)
| STMT_CONTINUE                 (* continue; *)
| STMT_BREAK                    (* break; *)
| STMT_RETURN of expr           (* return A; *)
| STMT_EXPR of expr             (* A; *)
| STMT_COMPOUND of ((type_expr * string) list * stmt list) (* { int x; ... } *)
| STMT_IF of (expr * stmt * stmt)                          (* if (A) S else T *)
| STMT_WHILE of (expr * stmt)                              (* while (A) S *)
;;

(* 関数定義 *)
type definition =
  (* long f(long x, long y) { ... } *)
  FUN_DEF of (type_expr * string * (type_expr * string) list * stmt) 
;;

