#
# Makefile
#

prog:=cc
mll:=$(prog)_lex.mll
mly:=$(prog)_parse.mly
mls:=$(wildcard *.ml)
cmos:=$(prog)_lex.cmo $(prog)_parse.cmo $(patsubst %.ml,%.cmo,$(mls))

all : $(prog).top

$(prog).top : $(mll) $(mly) $(mls)
	ocamlbuild -use-menhir $(prog).byte
	ocamlmktop -o $(prog).top -I _build $(cmos)

clean :
	rm -rf _build $(prog).top $(prog).byte


