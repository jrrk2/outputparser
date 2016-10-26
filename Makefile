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

MENHIRFLAGS=--trace

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
