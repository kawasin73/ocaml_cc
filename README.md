# ocaml_cc

東京大学工学部電子情報学科の「プログラミング言語」の授業の課題。

Ocaml で簡単な C 言語のコンパイラを実装します。

## 環境の立ち上げ

Docker を利用して開発します。

```bash
$ docker-compose run --rm ocaml bash

# コンテナ内で
$ cd /code/4_cogen
# コンパイル
$ make && ./cc.byte test.c test.s
# REPL
$ make && ./cc.top -I _build

# テスト
$ make -f test.mk
```
