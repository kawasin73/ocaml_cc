ファイル
===============

* cc_lex.mll   --- 字句解析
* cc_parse.mly --- 構文解析
* cc_ast.ml    --- 構文木の定義
* cc.ml        --- トップレベル (parse_string という関数だけ)

### これを実装するのが課題 ###
* cc_cogen.ml --- 構文木をIR, アセンブリ言語に翻訳するファイルのプレースホルダー

コンパイル
===============

$ make

実行
===============

$ ./cc.top -I _build

で対話的に実行するか, 

または

$ ./cc.byte a.c a.s

とすると a.c を読み込みその構文木からアセンブリ言語へ翻訳する関数 cogen_file を呼ぶ.

これを正しく実装するのが課題.

テスト
===============

$ make -f test.mk

小さい関数がいっぱい入ったテストプログラムでテスト(GCCと出力を比べる)を行う. 以下のような表示ですべてのテストに OK が出たら合格

初期状態ではおかしな出力を吐いて, それを受け取ったアセンブラが以下のようなエラーを出して終わる

```
gcc -o exe/f00.gcc -Dfoo=f00 ../test/main.c ../test/fun.c -O0 -g


gcc -o exe/f73.gcc -Dfoo=f73 ../test/main.c ../test/fun.c -O0 -g
gcc -o exe/f74.gcc -Dfoo=f74 ../test/main.c ../test/fun.c -O0 -g
gcc -P -E ../test/fun.c -o fun.i
./cc.byte fun.i fun_occ.s
gcc -o exe/f00.occ -Dfoo=f00 ../test/main.c fun_occ.s -O0 -g
fun_occ.s: Assembler messages:
fun_occ.s: Warning: end of file not at end of a line; newline inserted
fun_occ.s:1: Error: junk at end of line, first unrecognized character is `<'
test.mk:22: recipe for target 'exe/f00.occ' failed
make: *** [exe/f00.occ] Error 1
```




