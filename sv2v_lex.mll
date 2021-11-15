(**************************************************************************)
(*                                                                        *)
(* OCaml template Copyright (C) 2004-2010                                 *)
(*  Sylvain Conchon, Jean-Christophe Filliatre and Julien Signoles        *)
(* Adapted to boolean logic by Jonathan Kimmitt                           *)
(*  Copyright 2016 University of Cambridge                                *)
(*                                                                        *)
(*  This software is free software; you can redistribute it and/or        *)
(*  modify it under the terms of the GNU Library General Public           *)
(*  License version 2.1, with the special exception on linking            *)
(*  described in file LICENSE.                                            *)
(*                                                                        *)
(*  This software is distributed in the hope that it will be useful,      *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                  *)
(*                                                                        *)
(**************************************************************************)

{
  open Lexing
  open Sv2v
(*
  open Sv2v_types
*)
  let verbose = try int_of_string(Sys.getenv "LEXER_VERBOSE") > 0 with _ -> false
  let lincnt = ref 0

  let keyword =
    let h = Hashtbl.create 17 in
    List.iter 
      (fun (k,s) -> Hashtbl.add h s k)
      [
	AMPERSAND , ("ampersand");
	AMPERSAND_AMPERSAND , ("ampersand_AMPERSAND");
	AMPERSAND_EQ , ("ampersand_EQ");
	AT , ("at");
	AT_STAR , ("at_STAR");
	Always , ("always");
	Always_comb , ("always_comb");
	Always_ff , ("always_ff");
	Always_latch , ("always_latch");
	And , ("and");
	Assert , ("assert");
	Assign , ("assign");
	Assume , ("assume");
	Automatic , ("automatic");
	BACKQUOTE , ("backquote");
	Begin , ("begin");
	Bit , ("bit");
	Bits , ("bits");
	Break , ("break");
	Buf , ("buf");
	Byte , ("byte");
	CARET , ("caret");
	CARET_EQ , ("caret_EQ");
	CARET_TILDE , ("caret_TILDE");
	CASE , ("case");
	CHARGESTRENGTH , ("chargestrength");
	COLON , ("colon");
	COLON_COLON , ("colon_COLON");
	COMMA , ("comma");
	Casex , ("casex");
	Casez , ("casez");
	Celldefine , ("celldefine");
	Class , ("class");
	Const , ("const");
	Continue , ("continue");
	Cover , ("cover");
	DIMENSIONS , ("dimensions");
	DOLLAR , ("dollar");
	DOT , ("dot");
	DOT_STAR , ("dot_STAR");
	DOUBLEQUOTE , ("doublequote");
	DRIVESTRENGTH , ("drivestrength");
	Default , ("default");
	Default_nettype , ("default_nettype");
	Defparam , ("defparam");
	Disable , ("disable");
	Dist , ("dist");
	Do , ("do");
	EQUALS , ("equals");
	EQ_EQ , ("eq_EQ");
	EQ_EQ_EQ , ("eq_EQ_EQ");
	EQ_EQ_QUERY , ("eq_EQ_QUERY");
	Else , ("else");
	End , ("end");
	Endcase , ("endcase");
	Endcelldefine , ("endcelldefine");
	Endclass , ("endclass");
	Endfunction , ("endfunction");
	Endgenerate , ("endgenerate");
	Endinterface , ("endinterface");
	Endmodule , ("endmodule");
	Endpackage , ("endpackage");
	Endtask , ("endtask");
	Enum , ("enum");
	Event , ("event");
	Export , ("export");
	Extern , ("extern");
	Final , ("final");
	First_match , ("first_match");
	For , ("for");
	Foreach , ("foreach");
	Forever , ("forever");
	Fork , ("fork");
	Function , ("function");
	GREATER , ("greater");
	GT_EQ , ("gt_EQ");
	GT_GT , ("gt_GT");
	GT_GT_EQUALS , ("gt_GT_EQUALS");
	GT_GT_GT , ("gt_GT_GT");
	GT_GT_GT_EQUALS , ("gt_GT_GT_EQUALS");
	Generate , ("generate");
	Genvar , ("genvar");
	HASH , ("hash");
	HASH_EQUALS_HASH , ("hash_EQUALS_HASH");
	HASH_HASH , ("hash_HASH");
	HASH_HYPHEN_HASH , ("hash_HYPHEN_HASH");
	HYPHEN , ("hyphen");
	HYPHEN_COLON , ("hyphen_COLON");
	HYPHEN_EQ , ("hyphen_EQ");
	HYPHEN_GT , ("hyphen_GT");
	HYPHEN_GT_GT , ("hyphen_GT_GT");
	HYPHEN_HYPHEN , ("hyphen_HYPHEN");
	High , ("high");
	Highz0 , ("highz0");
	Highz1 , ("highz1");
	If , ("if");
	Iff , ("iff");
	Import , ("import");
	Increment , ("increment");
	Initial , ("initial");
	Inout , ("inout");
	Input , ("input");
	Inside , ("inside");
	Int , ("int");
	Integer , ("integer");
	Interface , ("interface");
	Intersect , ("intersect");
	Join , ("join");
	LBRACE , ("lbrace");
	LBRACK , ("lbrack");
	LBRACK_EQUALS_RBRACK , ("lbrack_EQUALS_RBRACK");
	LBRACK_HYPHEN_GT , ("lbrack_HYPHEN_GT");
	LBRACK_HYPHEN_GT_RBRACK , ("lbrack_HYPHEN_GT_RBRACK");
	LBRACK_STAR_RBRACK , ("lbrack_STAR_RBRACK");
	LESS , ("less");
	LPAREN , ("lparen");
	LPAREN_STAR , ("lparen_STAR");
	LPAREN_STAR_RPAREN , ("lparen_STAR_RPAREN");
	LT_EQ , ("lt_EQ");
	LT_HYPHEN_GT , ("lt_HYPHEN_GT");
	LT_LT , ("lt_LT");
	LT_LT_EQ , ("lt_LT_EQ");
	LT_LT_LT , ("lt_LT_LT");
	LT_LT_LT_EQUALS , ("lt_LT_LT_EQUALS");
	Large , ("large");
	Left , ("left");
	Local , ("local");
	Localparam , ("localparam");
	Logic , ("logic");
	Longint , ("longint");
	Low , ("low");
	Macromodule , ("macromodule");
	Medium , ("medium");
	Modport , ("modport");
	Module , ("module");
	Nand , ("nand");
	Negedge , ("negedge");
	Nor , ("nor");
	Not , ("not");
	Nounconnected_drive , ("nounconnected_drive");
	Or , ("or");
	Output , ("output");
	PERCENT , ("percent");
	PERCENT_EQ , ("percent_EQ");
	PLING , ("pling");
	PLING_EQ , ("pling_EQ");
	PLING_EQ_EQ , ("pling_EQ_EQ");
	PLING_EQ_QUERY , ("pling_EQ_QUERY");
	PLUS , ("plus");
	PLUS_COLON , ("plus_COLON");
	PLUS_EQ , ("plus_EQ");
	PLUS_PLUS , ("plus_PLUS");
	Package , ("package");
	Packed , ("packed");
	Parameter , ("parameter");
	Posedge , ("posedge");
	Priority , ("priority");
	Property , ("property");
	Protected , ("protected");
	Pull0 , ("pull0");
	Pull1 , ("pull1");
	QUERY , ("query");
	QUOTE , ("quote");
	QUOTE_LBRACE , ("quote_LBRACE");
	RBRACE , ("rbrace");
	RBRACK , ("rbrack");
	REAL , ("real");
	RPAREN , ("rparen");
	Realtime , ("realtime");
	Reg , ("reg");
	Repeat , ("repeat");
	Resetall , ("resetall");
	Return , ("return");
	Right , ("right");
	SEMICOLON , ("semicolon");
	SLASH , ("slash");
	SLASH_EQ , ("slash_EQ");
	STAR , ("star");
	STAR_EQUALS , ("star_EQUALS");
	STAR_RPAREN , ("star_RPAREN");
	STAR_STAR , ("star_STAR");
	Shortint , ("shortint");
	Shortreal , ("shortreal");
	Signed , ("signed");
	Size , ("size");
	Small , ("small");
	Static , ("static");
	Strong0 , ("strong0");
	Strong1 , ("strong1");
	Struct , ("struct");
	Supply0 , ("supply0");
	Supply1 , ("supply1");
	TILDE , ("tilde");
	TILDE_AMPERSAND , ("tilde_AMPERSAND");
	TILDE_CARET , ("tilde_CARET");
	TILDE_VBAR , ("tilde_VBAR");
	TIME , ("time");
	TYPE , ("type");
	TYPEDEF , ("typedef");
	Task , ("task");
	Throughout , ("throughout");
	Timeprecision , ("timeprecision");
	Timeunit , ("timeunit");
	Tri , ("tri");
	Tri0 , ("tri0");
	Tri1 , ("tri1");
	Triand , ("triand");
	Trior , ("trior");
	Trireg , ("trireg");
	UNDERSCORE , ("underscore");
	UNIQUE , ("unique");
	Unconnected_drive , ("unconnected_drive");
	Union , ("union");
	Unique0 , ("unique0");
	Unpacked_dimensions , ("unpacked_dimensions");
	Unsigned , ("unsigned");
	Uwire , ("uwire");
	VBAR , ("vbar");
	VBAR_EQ , ("vbar_EQ");
	VBAR_EQ_GT , ("vbar_EQ_GT");
	VBAR_HYPHEN_GT , ("vbar_HYPHEN_GT");
	VBAR_VBAR , ("vbar_VBAR");
	Var , ("var");
	Void , ("void");
	Wand , ("wand");
	Weak0 , ("weak0");
	Weak1 , ("weak1");
	While , ("while");
	Wire , ("wire");
	Within , ("within");
	Wor , ("wor");
	Xnor , ("xnor");
	Xor , ("xor");
      ];
    fun s -> Hashtbl.find h s

let tok' x = Sv2v_ord.getstr x

let ord = function
| oth -> let tok'' = tok' oth in Char.code (tok''.[0])

let import_seen = ref false

let tok arg = arg
}

let ident = ['a'-'z' 'A'-'Z' '$' '_'] ['a'-'z' 'A'-'Z' '_' '0'-'9' '$']*
let escaped = '\\'[^' ']*' '
let fltnum = ['0'-'9']+'.'['E' '-' '+' '0'-'9']*
let sizednumber = ['0'-'9']+'\''['s']*['b' 'd' 'h' 'o' 'x'][' ']*[ '0'-'9' 'a'-'f' 'x' 'z' 'A'-'F' 'X' 'Z' '_' '?']+
let number = ['0'-'9']['0'-'9' '_']*
let dfltnum = '''['0'-'9' 'b' 'd' 'h' 'o' 'x' 'X']['0'-'9' 'a'-'f' 'A'-'F' 'x' '?']*
let space = [' ' '\t' '\r']+
let newline = ['\n']
let qstring = '"'[^'"']*'"'
let comment = '/''/'[^'\n']*
let line = '`'[^'\n']*
let colon_begin = ':'[' ']*"begin"
   
rule token = parse
(*
| colon_begin { tok ( COLON_HYPHEN_begin ) }
*)
| ">>>" { tok ( GT_GT_GT ) }
| "<<<" { tok ( LT_LT ) } (* placeholder *)
| "<<=" { tok ( LT_LT_EQ ) }
| "===" { tok ( EQ_EQ_EQ ) }
| "!==" { tok ( PLING_EQ_EQ ) }
| "||" { tok ( VBAR_VBAR ) }
| "~|" { tok ( TILDE_VBAR ) }
| "^~" { tok ( CARET_TILDE ) }
| "~&" { tok ( TILDE_AMPERSAND ) }
| "==" { tok ( EQ_EQ ) }
| "+=" { tok ( PLUS_EQ ) }
| "-=" { tok ( HYPHEN_EQ ) }
| "<=" { tok ( LT_EQ ) }
| "!=" { tok ( PLING_EQ ) }
| "|=" { tok ( VBAR_EQ ) }
| "<<" { tok ( LT_LT ) }
| ">>" { tok ( GT_GT ) }
| ">=" { tok ( GT_EQ ) }
| ".*" { tok ( DOT_STAR ) }
| "+:" { tok ( PLUS_COLON ) }
| "-:" { tok ( HYPHEN_COLON ) }
| "::" { tok ( COLON_COLON ) }
| "++" { tok ( PLUS_PLUS ) }
| "--" { tok ( HYPHEN_HYPHEN ) }
| "**" { tok ( STAR_STAR ) }
| "&&" { tok ( AMPERSAND_AMPERSAND ) }
| "&=" { tok ( AMPERSAND_EQ ) }
| "'{" { tok ( QUOTE_LBRACE ) }
| '-' { tok ( HYPHEN ) }
| '+' { tok ( PLUS ) }
| '!' { tok ( PLING ) }
| '"' { tok ( DOUBLEQUOTE ) }
| '#' { tok ( HASH ) }
| '$' { tok ( DOLLAR ) }
| '%' { tok ( PERCENT ) }
| '&' { tok ( AMPERSAND ) }
| ''' { tok ( QUOTE ) }
| '(' { tok ( LPAREN ) }
| '[' { tok ( LBRACK ) }
| '{' { tok ( LBRACE ) }
| '<' { tok ( LESS ) }
| ')' { tok ( RPAREN ) }
| ']' { tok ( RBRACK ) }
| '}' { tok ( RBRACE ) }
| '>' { tok ( GREATER ) }
| '*' { tok ( STAR ) }
| ',' { tok ( COMMA ) }
| '.' { tok ( DOT ) }
| '/' { tok ( SLASH ) }
(*
| '\\' { tok ( BACKSLASH ) }
*)
| ':' { tok ( COLON ) }
| ';' { tok ( SEMICOLON ) }
| '=' { tok ( EQUALS ) }
| '?' { tok ( QUERY ) }
| '@' { tok ( AT ) }
| '^' { tok ( CARET ) }
| '_' { tok ( UNDERSCORE ) }
| '|' { tok ( VBAR ) }
| '~' { tok ( TILDE ) }

| line { token lexbuf }
| "/*" { comment lexbuf }
| "(* " { comment lexbuf }

  | comment
      { token lexbuf }
  | space
      { token lexbuf }
  | newline
      { incr lincnt; token lexbuf }
  | sizednumber as n
      { tok ( Number (n) ) }
  | number as n
      { tok ( Number (n) ) }
  | dfltnum as n
      { tok ( Number (n) ) }
  | fltnum as n
      { tok ( try let f = float_of_string n in Real f with _ -> SimpleIdentifier n) }
  | ident as s
      { tok ( try keyword s with Not_found -> SimpleIdentifier s ) }
  | escaped as s
      { let s = String.sub s 1 (String.length s - 2) in tok (EscapedIdentifier s ) }
  | qstring as s
      { tok ( STRING (String.sub s 1 (String.length s - 2)) ) }
  | eof
      { tok ( EOF_TOKEN ) }

| _ as oth
{ tok ( failwith ("Sv2v_lex: "^String.make 1 oth) ) }

and comment = parse
| newline { incr lincnt; comment lexbuf }
| '*''/' as com { if false then print_endline ("/*"^com); token lexbuf }
| '*'')' as com { if false then print_endline ("/*"^com); token lexbuf }
| _ { comment lexbuf }

(* pre-processing should have removed this stuff, but if not, skip over it *)
   
and ifdef = parse
| "`endif" { token lexbuf }
| newline { incr lincnt; ifdef lexbuf }
| _ { ifdef lexbuf }

and ifndef = parse
| "`endif" { token lexbuf }
| newline { incr lincnt; ifndef lexbuf }
| _ { ifndef lexbuf }
