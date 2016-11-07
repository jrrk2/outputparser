#**************************************************************************)
#*                                                                        *)
#* OCaml template Copyright (C) 2004-2010                                 *)
#*  Sylvain Conchon, Jean-Christophe Filliatre and Julien Signoles        *)
#* Adapted to boolean logic by Jonathan Kimmitt                           *)
#*  Copyright 2016 University of Cambridge                                *)
#*                                                                        *)
#*  This software is free software; you can redistribute it and/or        *)
#*  modify it under the terms of the GNU Library General Public           *)
#*  License version 2.1, with the special exception on linking            *)
#*  described in file LICENSE.                                            *)
#*                                                                        *)
#*  This software is distributed in the hope that it will be useful,      *)
#*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
#*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                  *)
#*                                                                        *)
#**************************************************************************)

.PHONY: everything
PARSER=ocamlyacc
SED=sed -e 's=\^ _Nonnull=* =g' -e 's=\* _Nonnull=* =g'
CPP=clang -E -P -D __extension__= -D __restrict= -D __const=const -D __attribute__\(x\)= -D __asm__\(x\)= -D __PRETTY_FUNCTION__=__FILE__ -I ../simpleDMC_restructure/dest -D DSFMT_MEXP=19937 -D __inline__=inline -D _Nullable= -D__asm\(x\)=

everything: output_parser.top output_parser output_parser.top output_parser

output_parser.top: output_types.mli output_parser.mli ord.ml output_parser.ml output_lexer.ml template.ml output.ml
	ocamlmktop -g -o $@ output_types.mli output_parser.mli ord.ml output_parser.ml output_lexer.ml template.ml output.ml

output_parser: output_types.mli output_parser.mli ord.ml output_parser.ml output_lexer.ml template.ml output.ml
	ocamlopt -g -o $@ output_types.mli output_parser.mli ord.ml output_parser.ml output_lexer.ml template.ml output.ml

output_lexer.ml: output_lexer.mll
	ocamllex output_lexer.mll

output_parser.mli output_parser.ml: output_parser.mly
	$(PARSER) output_parser.mly 

ord.ml: ord.sh output_parser.mli
	sh ord.sh

clean:
	rm -f output_lexer.ml output_parser.mli output_parser.ml outputparser outputparser.top ord.ml *.cm? *.o

leftest: output_parser
	env OCAMLRUNPARAM=b K_HISTORY=string T_STRING=string QSTRING=string NUMBER=float ./output_parser lef.output
ctest: output_parser
	env OCAMLRUNPARAM=b STRING=string IDENTIFIER=string CONSTANT=float ./output_parser c-parse.output

MENHIRFLAGS=#--trace

lef_file: lef_file_edited.cmo lef_file_lex.ml lef_file_main.ml
	ocamlc -g -o $@ lef_file_edited.cmo lef_file_lex.ml lef_file_main.ml

lef_file.top: lef_file_edited.cmo lef_file_lex.ml lef_file_main.ml
	ocamlmktop -g -o $@ lef_file_edited.cmo lef_file_lex.ml lef_file_main.ml

lef_file_lex.ml: lef_file_lex.mll
	ocamllex $<

lef_file_edited.cmo: lef_file_edited.mly
	menhir $(MENHIRFLAGS) $<
	ocamlc -c -g lef_file_edited.mli lef_file_edited.ml

Program: Program_edited.cmo Program_types.ml Program_lex.ml Program_main.ml
	ocamlc -g -o $@ Program_edited.cmo Program_types.ml Program_lex.ml Program_main.ml

Program.top: Program_edited.cmo Program_types.ml Program_lex.ml Program_main.ml
	ocamlmktop -g -o $@ Program_edited.cmo Program_types.ml Program_lex.ml Program_main.ml

Program_lex.ml: Program_lex.mll
	ocamllex $<

Program_edited.cmo: Program_edited.mly
	menhir $(MENHIRFLAGS) $<
	ocamlc -c -g Program_edited.mli Program_types.ml Program_edited.ml

############################################################################

ansic: y.tab.o lex.yy.o ansimain.o
	gcc -o $@ y.tab.o lex.yy.o ansimain.o

y.tab.c y.tab.h: ansic.y
	bison -v -y -d ansic.y

lex.yy.c: ansic.l
	flex ansic.l

ansitest: Translation_unit_list.mly

Translation_unit_list.mly: ansic convert.i dSFMT.i dump.i dynamics.i kernel.i main.i support.i output_parser Translation_unit_list_78
	-./ansic <kernel.i >& kernel.log
	env OCAMLRUNPARAM=b STRING_LITERAL=string IDENTIFIER=string CONSTANT=string TYPE_NAME=string ./output_parser y.output

convert.i: ../simpleDMC_restructure/src/convert.c
	$(CPP) $< | $(SED) >$@

dSFMT.i: ../simpleDMC_restructure/src/dSFMT.c
	$(CPP) $< | $(SED) >$@

dump.i: ../simpleDMC_restructure/src/dump.c
	$(CPP) $< | $(SED) >$@

dynamics.i: ../simpleDMC_restructure/src/dynamics.c
	$(CPP) $< | $(SED) >$@

kernel.i: ../simpleDMC_restructure/src/kernel.c
	$(CPP) $< | $(SED) >$@

main.i: ../simpleDMC_restructure/src/main.c
	$(CPP) $< | $(SED) >$@

support.i: ../simpleDMC_restructure/src/support.c
	$(CPP) $< | $(SED) >$@

Translation_unit_list: Translation_unit_list.cmi Translation_unit_list_types.cmo Translation_unit_list.cmo Translation_unit_list_lex.cmo Translation_unit_list_main.cmo
	 ocamlc -g -o $@ Translation_unit_list_types.cmo Translation_unit_list.cmo Translation_unit_list_lex.cmo Translation_unit_list_main.cmo

Translation_unit_list.top: Translation_unit_list.cmi Translation_unit_list_types.cmo Translation_unit_list.cmo Translation_unit_list_lex.cmo Translation_unit_list_main.cmo
	 ocamlmktop -g -o $@ Translation_unit_list_types.cmo Translation_unit_list.cmo Translation_unit_list_lex.cmo Translation_unit_list_main.cmo

Translation_unit_list_lex.ml: Translation_unit_list_lex.mll
	ocamllex Translation_unit_list_lex.mll

Translation_unit_list.mli Translation_unit_list.ml: Translation_unit_list.mly Translation_unit_list_types.ml
#	ocamlyacc $<
	menhir $(MENHIRFLAGS) $<
	echo 'val declst : (token * token) list ref' >> Translation_unit_list.mli
	ocamlc -c -g Translation_unit_list.mli Translation_unit_list_types.ml Translation_unit_list.ml

parsetest: Translation_unit_list Translation_unit_list.top convert.i dSFMT.i dump.i dynamics.i kernel.i main.i support.i
	./Translation_unit_list convert.i dSFMT.i dynamics.i kernel.i main.i support.i dump.i 

%.cmi: %.mli
	ocamlc -c $<

%.cmo: %.ml
	ocamlc -c $<
