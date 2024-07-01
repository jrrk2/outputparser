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

.PHONY: everything convert

PARSER=ocamlyacc
MENHIRFLAGS= --infer # --trace
PARSER=menhir $(MENHIRFLAGS)

all: output_parser output_parser.top Source_text_top Source_text

output_parser: output_types.mli output_parser.mli ord.ml output_parser.ml output_lexer.ml template.ml output.ml
	ocamlopt -g -o $@ output_types.mli output_parser.mli ord.ml output_parser.ml output_lexer.ml template.ml output.ml

output_parser.top: output_types.mli output_parser.mli ord.ml output_parser.ml output_lexer.ml template.ml output.ml
	ocamlmktop -g -o $@ output_types.mli output_parser.mli ord.ml output_parser.ml output_lexer.ml template.ml output.ml

output_lexer.ml: output_lexer.mll
	ocamllex output_lexer.mll

output_parser.mli output_parser.ml: output_parser.mly
	$(PARSER) output_parser.mly 

ord.ml: ord.sh output_parser.mli
	sh ord.sh

clean:
	rm -f output_lexer.ml output_parser.mli output_parser.ml outputparser outputparser.top ord.ml *.cm? *.o

###############################################################################
#  Tseitin_intf.ml 

Source_text_top: Source_text.cmo Source_text_tokens.cmo Source_text_lex.ml Source_text_rewrite_types.mli classify.ml matchmly.ml String_lit.mli String_lit.ml Msat_sat_slit.mli Msat_sat_slit.ml Msat_tseitin.mli Msat_tseitin.ml Source_text_preproc.ml Source_text_rewrite.ml dump_rtlil.ml Input_types.cmo Input.cmo Input_tokens.cmo Input_rewrite_types.mli ord_input.ml Input_lex.ml Input_dump.ml Input_rewrite.ml Source_text_main.ml convert.ml
	ocamlfind ocamlmktop -package msat -linkpkg -g -o $@ -I +unix unix.cma Source_text_types.cmo Source_text.cmo Source_text_tokens.cmo Source_text_lex.ml Source_text_rewrite_types.mli classify.ml matchmly.ml String_lit.{mli,ml} Msat_sat_slit.{mli,ml} Msat_tseitin.{mli,ml} Source_text_rewrite.ml Source_text_preproc.ml Input_types.ml Input.ml Input_tokens.ml Input_rewrite_types.mli dump_rtlil.ml Input_rewrite_types.mli ord_input.ml Input_lex.ml Input_dump.ml Input_rewrite.ml convert.ml Source_text_main.ml

Source_text: Source_text_types.ml Source_text.mli Source_text.ml Source_text_tokens.ml Source_text_lex.ml Source_text_rewrite_types.mli classify.ml matchmly.ml String_lit.mli String_lit.ml Msat_sat_slit.mli Msat_sat_slit.ml Msat_tseitin.mli Msat_tseitin.ml Source_text_preproc.ml Source_text_rewrite.ml dump_rtlil.ml Input_types.ml Input.mli Input.ml Input_tokens.ml Input_rewrite_types.mli ord_input.ml Input_lex.ml Input_dump.ml Input_rewrite.ml Source_text_main.ml convert.ml
	ocamlfind ocamlmktop -package msat -linkpkg -g -o $@ -I +unix unix.cma Source_text_types.ml Source_text.mli Source_text.ml Source_text_tokens.ml Source_text_lex.ml String_lit.{mli,ml} Msat_sat_slit.{mli,ml} Msat_tseitin.{mli,ml} Input_rewrite_types.mli Source_text_rewrite_types.mli classify.ml matchmly.ml Source_text_rewrite.ml Source_text_preproc.ml Input_types.ml Input.mli Input.ml Input_tokens.ml dump_rtlil.ml Input_rewrite_types.mli ord_input.ml Input_lex.ml Input_dump.ml Input_rewrite.ml convert.ml Source_text_main.ml

Source_text_convert: Source_text_types.ml Source_text.ml Source_text_tokens.ml Source_text_lex.ml Msat_sat_slit.mli Msat_sat_slit.ml Source_text_rewrite_types.mli dump_rtlil.ml Input.ml Input_tokens.ml Input_rewrite_types.mli ord_input.ml Input_lex.ml classify.ml matchmly.ml Source_text_rewrite.ml Source_text_convert.ml
	ocamlfind ocamlc -package msat -linkpkg -g -o $@ -I +unix unix.cma Source_text_types.ml Source_text.mli Source_text.ml Source_text_tokens.ml Source_text_lex.ml String_lit.{mli,ml} Msat_sat_slit.mli Msat_sat_slit.ml Input_rewrite_types.mli Source_text_rewrite_types.mli classify.ml matchmly.ml Msat_tseitin.{mli,ml} Source_text_rewrite.ml Source_text_convert.ml

Source_text_convert_top: Source_text_types.ml Source_text.ml Source_text_tokens.ml Source_text_lex.ml Msat_sat_slit.mli Msat_sat_slit.ml Source_text_rewrite_types.mli dump_rtlil.ml Input.ml Input_tokens.ml Input_rewrite_types.mli ord_input.ml Input_lex.ml classify.ml matchmly.ml Source_text_rewrite.ml Source_text_convert.ml
	ocamlfind ocamlmktop -package msat -linkpkg -g -o $@ -I +unix unix.cma Source_text_types.ml Source_text.mli Source_text.ml Source_text_tokens.ml Source_text_lex.ml String_lit.{mli,ml} Msat_sat_slit.mli Msat_sat_slit.ml Input_rewrite_types.mli Source_text_rewrite_types.mli classify.ml matchmly.ml Msat_tseitin.{mli,ml} Source_text_rewrite.ml Source_text_convert.ml

Source_text_preproc: Source_text_preproc.ml Source_text_preproc_grammar.ml Source_text_preproc_lexer.ml Source_text_preproc.ml Source_text_preproc_main.ml
	ocamlfind ocamlopt -package menhir,menhirlib -linkpkg -g -o $@ Source_text_preproc_grammar.{mli,ml} Source_text_preproc_lexer.ml Source_text_preproc.ml Source_text_preproc_main.ml

convert: convert.ml
	@echo 'make convert.ml'

convert.ml: ../yosys/share/simcells.v
	./Source_text_convert ../yosys/share/simcells.v $@

Source_text.mly Source_text_tokens.ml: V3ParseBison.output Source_text.patch
	env OCAMLRUNPARAM=b STRING_LITERAL=string IDENTIFIER=string INTEGER_NUMBER=string FLOATING_HYPHEN_POINT_NUMBER=float TYPE_HYPHEN_IDENTIFIER=string IDENTIFIER_HYPHEN_COLON_COLON=string STRING=string ./output_parser $<
	patch Source_text.mly < Source_text.patch

Source_text.cmo: Source_text_tokens.ml Source_text.ml Source_text.mli
	ocamlc.opt -g -c Source_text.mli Source_text_types.ml Source_text.ml

Source_text.cmx: Source_text_tokens.ml Source_text.ml Source_text.mli
	ocamlopt.opt -g -c Source_text.mli Source_text_types.ml Source_text.ml

Source_text_lex.ml: Source_text_lex.mll
	ocamllex $<

Source_text_preproc_lexer.ml: Source_text_preproc_lexer.mll
	ocamllex $<

Source_text.ml Source_text.mli: Source_text.mly
	menhir $(MENHIRFLAGS) $<

Source_text_preproc_grammar.ml: Source_text_preproc_grammar.mly
	menhir $<

dump.cmo: Source_text_rewrite_types.cmi

############################################################################

Input_top: Input.cmo Input_tokens.cmo Input_rewrite_types.mli ord_input.ml Input_lex.ml Input_dump.ml Input_rewrite.ml Input_main.ml
	ocamlmktop -g -o $@ Input_tokens.cmo Input_rewrite_types.mli Input.cmo ord_input.ml Input_lex.ml Input_dump.ml Input_rewrite.ml Input_main.ml

Input.mly Input_tokens.ml: rtlil_parser.output
	env OCAMLRUNPARAM=b TOK_STRING=string TOK_ID=string TOK_INT=int TOK_VALUE=string ./output_parser $<

Input.cmo: Input_tokens.ml Input.ml Input.mli
	ocamlc.opt -g -c Input.mli Input_tokens.ml Input.ml

Input.cmx: Input_tokens.ml Input.ml Input.mli
	ocamlopt.opt -g -c Input.mli Input_tokens.ml Input.ml

Input_lex.ml: Input_lex.mll
	ocamllex $<

Input.ml: Input.mly
	ocamlc -c Input_types.ml
	menhir $(MENHIRFLAGS) $<

############################################################################

Pat_top: Pat.cmo Pat_types.cmo Pat_lex.ml Pat_rewrite.ml
	ocamlmktop -g -o $@ Pat_types.cmo Pat.cmo Pat_lex.ml Pat_rewrite.ml

Pat.cmo: Pat_types.ml Pat.ml Pat.mli
	ocamlc.opt -g -c Pat.mli Pat_types.ml Pat.ml

Pat_lex.ml: Pat_lex.mll
	ocamllex $<

Pat.ml: Pat.mly
	menhir $(MENHIRFLAGS) $<

############################################################################

Extract_top: Extract.cmo Extract_types.cmo Extract_lex.ml Extract_rewrite.ml
	ocamlmktop -g -o $@ Extract_types.cmo Extract.cmo Extract_lex.ml Extract_rewrite.ml

Extract.cmo: Extract_types.ml Extract.ml Extract.mli
	ocamlc.opt -g -c Extract.mli Extract_types.ml Extract.ml

Extract_lex.ml: Extract_lex.mll
	ocamllex $<

Extract.ml: Extract.mly
	menhir $(MENHIRFLAGS) $<

############################################################################

File_top: File.cmo File_types.cmo File_ord.ml File_lex.ml File_rewrite_types.mli File_rewrite.ml File_main.ml
	ocamlmktop -g -o $@ File_types.cmo File.cmo File_ord.ml File_lex.ml File_rewrite_types.mli File_rewrite.ml

File: File.cmx File_types.cmx File_lex.ml File_rewrite_types.mli File_rewrite.ml File_main.ml
	ocamlopt.opt -g -o $@ File_types.cmx File.cmx File_lex.ml File_rewrite_types.mli File_rewrite.ml File_main.ml

liberty_parser.tab.c liberty_parser.tab.h: liberty_parser.y
	bison -v -y -d $<

libertytest: File.mly

File.mly: output_parser liberty_parser.output
	env OCAMLRUNPARAM=b STRING=string IDENT=string NUM=string TYPE_NAME=string ./output_parser liberty_parser.output

File.cmo: File_types.ml File.ml File.mli
	ocamlc.opt -g -c File.mli File_types.ml File.ml

File.cmx: File_types.ml File.ml File.mli
	ocamlopt.opt -g -c File.mli File_types.ml File.ml

File_lex.ml: File_lex.mll
	ocamllex $<

File.ml File.mli: File.mly
	menhir $(MENHIRFLAGS) $<

File_ord.ml: ord_file.sh File.mli
	sh ord_file.sh

############################################################################

ansic: y.tab.o lex.yy.o ansimain.o
	gcc -o $@ y.tab.o lex.yy.o ansimain.o

y.tab.c y.tab.h: ansic.y
	bison -v -y -d ansic.y

lex.yy.c: ansic.l
	flex ansic.l

ansitest: Translation_unit_list.mly

Translation_unit_list.mly: y.tab.c output_parser Translation_unit_list_78
	env OCAMLRUNPARAM=b STRING_LITERAL=string IDENTIFIER=string CONSTANT=string TYPE_NAME=string ./output_parser y.output

%.i: $(SIMPLEDMC)/src/%.c
	$(CPP) $< | $(SED) >$@

Translation_unit_list: Translation_unit_list.cmi Translation_unit_list_types.cmo Translation_unit_list.cmo Translation_unit_list_lex.cmo Translation_unit_list_filt.cmo Translation_unit_list_transform.cmo Translation_unit_list_main.cmo
	 ocamlc.opt -g -o $@ Translation_unit_list_types.cmo Translation_unit_list.cmo Translation_unit_list_lex.cmo Translation_unit_list_filt.cmo Translation_unit_list_transform.cmo Translation_unit_list_main.cmo

Translation_unit_list.top: Translation_unit_list.cmi Translation_unit_list_types.cmo Translation_unit_list.cmo Translation_unit_list_lex.cmo sample.cmo Translation_unit_list_main.cmo
	 ocamlmktop -g -o $@ Translation_unit_list_types.cmo Translation_unit_list.cmo Translation_unit_list_lex.cmo Translation_unit_list_main.cmo sample.cmo

Translation_unit_list_lex.ml: Translation_unit_list_lex.mll
	ocamllex Translation_unit_list_lex.mll

Translation_unit_list.mli Translation_unit_list.ml: Translation_unit_list.mly Translation_unit_list_types.ml
	$(PARSER) $<
	echo 'val declst : (token * token) list ref' >> Translation_unit_list.mli
	ocamlc.opt -g -c Translation_unit_list.mli Translation_unit_list_types.ml Translation_unit_list.ml

parsetest: Translation_unit_list Translation_unit_list.top $(DMCFILES)
	env OCAMLRUNPARAM=b TRANS_MAIN=testgaussian ./Translation_unit_list $(DMCFILES) >mykernel_test.c
	clang mykernel_test.c -lm -g -Dtestgaussian=main -o testgaussian

parsemain: Translation_unit_list Translation_unit_list.top $(DMCFILES)
	env OCAMLRUNPARAM=b TRANS_MAIN=main ./Translation_unit_list $(DMCFILES) >mykernel_main.c

%.cmi: %.mli
	ocamlc.opt -g -c $<

%.cmo: %.ml
	ocamlc.opt -g -c $<

%.vsyn: 
	./Source_text $*.sv
	rm -f work-obj93.cf
	ghdl -a $*.sv_dump.vhd
	ghdl -e $*
	ghdl --synth $* > $*_synth.ghdl
	yosys -m ghdl -p 'ghdl $*; synth; write_verilog $*.vsyn; write_json $*_vsyn.json' 2>&1 | tee $*.log
	netlistsvg $*_vsyn.json -o $*_vsyn.svg 

%.liberty:
	yosys -p 'read_verilog $*.v; proc; opt; memory; opt; techmap; opt; dfflibmap -liberty NangateOpenCellLibrary_typical.lib; abc -liberty NangateOpenCellLibrary_typical.lib; opt; write_verilog $*.liberty; write_json $*_liberty.json' 2>&1 | tee $*.log
	netlistsvg $*_check.json -o $*_check.svg 

%.check:
	yosys -p 'read_verilog $*.v; proc; write_verilog $*.check; write_json $*_check.json' 2>&1 | tee $*.log
	netlistsvg $*_check.json -o $*_check.svg 

%.sat:
	yosys -p 'read_verilog $*.v; hierarchy -auto-top; proc; expose -evert-dff; design -save gold; opt_expr; design -stash gate; design -import gold -as gold; design -import gate -as gate; miter -equiv -flatten -make_assert -make_outputs gold gate miter; sat -verify -prove-asserts -show-ports miter; write_verilog $*.sat'

%.cmp: %.check %.vsyn
	yosys -p 'read_verilog $*.check; hierarchy -auto-top; proc; expose -evert-dff; design -stash gold; read_verilog $*.vsyn; hierarchy -auto-top; proc; expose -evert-dff; design -stash gate; design -import gold -as gold; write_verilog $*.gold; design -import gate -as gate; write_verilog $*.gate; miter -equiv -flatten -make_assert -make_outputs gold gate miter; sat -verify -prove-asserts -show-ports miter; write_verilog $*.cmp'

%.temp: %.check %.vsyn
	yosys -p 'read_verilog $*.v; hierarchy -auto-top; proc; design -stash gold; read_verilog $*.vsyn; hierarchy -auto-top; proc; design -stash gate; design -import gold -as gold; design -import gate -as gate; miter -equiv -flatten -make_assert -make_outputs gold gate miter; opt; clk2fflogic;  write_smt2 $*.temp; sat -seq -tempinduct -verify -prove-asserts -show-ports miter' 2>&1 | tee $*.templog

# ; proc; write_verilog $*.temp

show:
	yosys -p 'read_verilog apb_uart.vsyn; prep; opt; show -format ps'

yosys:
	yosys

depend:
	ocamldep *.ml >.depend

include .depend
