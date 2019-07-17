#
# Makefile
#

prog:=cc
srcs:=$(prog)_parse.mly $(prog)_lex.mll $(prog).ml
cmos:=$(prog)_parse.cmo $(prog)_lex.cmo $(prog).cmo

all : $(prog).top

$(prog).top : $(srcs)
	ocamlbuild -use-menhir $(prog).byte
	ocamlmktop -o $(prog).top -I _build $(cmos)

clean :
	rm -rf _build $(prog).top $(prog).byte


