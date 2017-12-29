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

MENHIRFLAGS=--trace

############################################################################

y.output: firrtl.y
	bison -v -y -d firrtl.y

firrtltest: Circuit.mly Circuit Circuit.top

ordfirrtl.ml: ordfirrtl.sh Circuit.mli
	sh ordfirrtl.sh

Circuit.mly: y.output output_parser
	env OCAMLRUNPARAM=b STRING=string ID=string CHARACTERLITERAL=string INT=int FLOATINGPOINTLITERAL=float ./output_parser y.output

%.i: $(SIMPLEDMC)/src/%.c
	$(CPP) $< | $(SED) >$@

Circuit: Circuit.cmi ordfirrtl.cmo Circuit_types.cmo Circuit.cmo Circuit_lex.cmo Circuit_filt.cmo Circuit_transform.cmo Circuit_main.cmo
	 ocamlc.opt -g -o $@ Circuit_types.cmo ordfirrtl.cmo Circuit.cmo Circuit_lex.cmo Circuit_filt.cmo Circuit_transform.cmo Circuit_main.cmo

Circuit.top: Circuit.cmi ordfirrtl.cmo Circuit_types.cmo Circuit.cmo Circuit_lex.cmo Circuit_filt.cmo Circuit_transform.cmo  Circuit_main.cmo
	 ocamlmktop -g -o $@ Circuit_types.cmo ordfirrtl.cmo Circuit.cmo Circuit_lex.cmo Circuit_filt.cmo Circuit_transform.cmo Circuit_main.cmo

Circuit_lex.ml: Circuit_lex.mll
	ocamllex Circuit_lex.mll

Circuit.mli Circuit.ml: Circuit.mly Circuit_types.ml
#	ocamlyacc $<
	menhir $(MENHIRFLAGS) $<
	echo 'val declst : (token * token) list ref' >> Circuit.mli
	ocamlc.opt -g -c Circuit.mli Circuit_types.ml Circuit.ml

parsetest: Circuit Circuit.top
	env OCAMLRUNPARAM=b ./Circuit.top

parsemain: Circuit Circuit.top
	env OCAMLRUNPARAM=b ./Circuit

%.cmi: %.mli
	ocamlc.opt -g -c $<

%.cmo: %.ml
	ocamlc.opt -g -c $<

depend:
	ocamldep *.ml >.depend

include .depend
