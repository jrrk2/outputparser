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
MENHIRFLAGS=#--trace
PARSER=menhir $(MENHIRFLAGS)
SIMPLEDMC=../simpleDMC_verify
DMCFILES=dSFMT.i main.i Orbital.i position.i support.i System.i Walker.i walkthewalk.i Wavefunction.i
SED=sed -e 's=\^ _Nonnull=* =g' -e 's=\* _Nonnull=* =g'
CPP=clang -E -D __extension__= -D __restrict= -D __const=const -D __attribute__\(x\)= -D __asm__\(x\)= -D __PRETTY_FUNCTION__=__FILE__ -I $(SIMPLEDMC)/dest -I $(SIMPLEDMC)/dest/gcc/debug_dump -D DSFMT_MEXP=19937 -D __inline__=inline -D _Nullable= -D__asm\(x\)=

all: output_parser output_parser.top Source_text Source_text_top

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

############################################################################

Source_text_top: Source_text.cmo Source_text_types.cmo Source_text_lex.ml Source_text_rewrite_types.mli dump_vhdl.ml Source_text_rewrite.ml Source_text_main.ml
	ocamlmktop -g -o $@ Source_text_types.cmo Source_text.cmo Source_text_lex.ml Source_text_rewrite_types.mli dump_vhdl.ml Source_text_rewrite.ml

Source_text: Source_text.cmx Source_text_types.cmx Source_text_lex.ml Source_text_rewrite_types.mli dump_vhdl.ml Source_text_rewrite.ml Source_text_main.ml
	ocamlopt.opt -g -o $@ Source_text_types.cmx Source_text.cmx Source_text_lex.ml Source_text_rewrite_types.mli dump_vhdl.ml Source_text_rewrite.ml Source_text_main.ml

Source_text.mly Source_text_types.ml: V3ParseBison.output Source_text.patch
	env OCAMLRUNPARAM=b STRING_LITERAL=string IDENTIFIER=string INTEGER_NUMBER=string FLOATING_HYPHEN_POINT_NUMBER=float TYPE_HYPHEN_IDENTIFIER=string IDENTIFIER_HYPHEN_COLON_COLON=string STRING=string ./output_parser $<
	patch Source_text.mly < Source_text.patch

Source_text.cmo: Source_text_types.ml Source_text.ml Source_text.mli
	ocamlc.opt -g -c Source_text.mli Source_text_types.ml Source_text.ml

Source_text.cmx: Source_text_types.ml Source_text.ml Source_text.mli
	ocamlopt.opt -g -c Source_text.mli Source_text_types.ml Source_text.ml

Source_text_lex.ml: Source_text_lex.mll
	ocamllex $<

Source_text.ml: Source_text.mly
	menhir $(MENHIRFLAGS) $<

############################################################################

ansic: y.tab.o lex.yy.o ansimain.o
	gcc -o $@ y.tab.o lex.yy.o ansimain.o

y.tab.c y.tab.h: ansic.y
	bison -v -y -d ansic.y

lex.yy.c: ansic.l
	flex ansic.l

ansitest: Translation_unit_list.mly

Translation_unit_list.mly: ansic output_parser Translation_unit_list_78
	env OCAMLRUNPARAM=b STRING_LITERAL=string IDENTIFIER=string CONSTANT=string TYPE_NAME=string ./output_parser y.output

%.i: $(SIMPLEDMC)/src/%.c
	$(CPP) $< | $(SED) >$@

Translation_unit_list: Translation_unit_list.cmi Translation_unit_list_types.cmo Translation_unit_list.cmo Translation_unit_list_lex.cmo Translation_unit_list_filt.cmo Translation_unit_list_transform.cmo Translation_unit_list_main.cmo
	 ocamlc.opt -g -o $@ Translation_unit_list_types.cmo Translation_unit_list.cmo Translation_unit_list_lex.cmo Translation_unit_list_filt.cmo Translation_unit_list_transform.cmo Translation_unit_list_main.cmo

Translation_unit_list.top: Translation_unit_list.cmi Translation_unit_list_types.cmo Translation_unit_list.cmo Translation_unit_list_lex.cmo Translation_unit_list_filt.cmo Translation_unit_list_transform.cmo  Translation_unit_list_main.cmo
	 ocamlmktop -g -o $@ Translation_unit_list_types.cmo Translation_unit_list.cmo Translation_unit_list_lex.cmo Translation_unit_list_filt.cmo Translation_unit_list_transform.cmo Translation_unit_list_main.cmo

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
