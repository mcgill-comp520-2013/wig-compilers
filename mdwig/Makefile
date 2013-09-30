all: code

code: native

native:
	ocamlbuild mdwig.byte -lib str -cflags -w,-26

byte:
	ocamlbuild mdwig.byte -lib str

debug:
	ocamlbuild mdwig.d.byte -lib str

profile:
	ocamlbuild mdwig.p.native -lib str

.PHONY: all code clean mrproper check weed pretty unit symbol benchmarks

clean: 
	rm -f _build/*.cm* _build/*.o _build/*.ml*

mrproper:
	ocamlbuild -clean

check: code benchmarks weed pretty unit symbol

weed: code
	@echo Weed: not implemented

pretty: code
	@echo Pretty: not implemented

unit: code
	@echo Unit: not implemented

symbol: code
	@echo Symbol: Just run the normal mdwig command, w/o print-ast -print-symboltable on whatever example you want.

benchmarks: code
	@./benchmarks.sh

# Dependencies
prettyprint.cmo: lang.cmo
mdwig.cmo: symboltable.cmo prettyprint.cmo lang.cmo lexer.cmo parser.cmo
lexer.cmo: lang.cmo
parser.cmo: parser.cmi lang.cmo
symboltable.cmo: lang.cmo
