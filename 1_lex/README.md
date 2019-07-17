
$ ocamllex cc_lex.mll

cc_lex.ml というファイルができる

$ make

でも同じことがおきる

$ ocaml -init cc_lex.ml

lex という関数が定義されている.
それを用いた lex_string 関数で以下のようにテストしてみよ.

# lex_string "long foo (long x, long y) { x = x + y; return 2 * x; }" ;;

これはCの極小サブセットの字句を認識する. なおこの時点では構文解析を行っていないのでCの構文として正しいかどうかは認識しない. ので, 以下も普通に成功する

# lex_string "long long foo bar 1 2 -23" ;;