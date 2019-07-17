
all : cc_lex.ml

cc_lex.ml : cc_lex.mll
	ocamllex $<
	@echo "---------------------"
	@echo "run 'ocaml -init cc_lex.ml' and try tokenizing any string with lex_string function"
