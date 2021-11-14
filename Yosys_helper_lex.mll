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
  open Yosys_helper
  open Yosys_helper_types

  let verbose = try int_of_string(Sys.getenv "LEXER_VERBOSE") > 0 with _ -> false
  let lincnt = ref 0

  let keyword =
    let h = Hashtbl.create 17 in
    List.iter 
      (fun (k,s) -> Hashtbl.add h s k)
      [
TOK_ALWAYS , "always";
TOK_ALWAYS_COMB , "always_comb";
TOK_ALWAYS_FF , "always_ff";
TOK_ALWAYS_LATCH , "always_latch";
TOK_ASSERT , "assert";
TOK_ASSIGN , "assign";
TOK_ASSUME , "assume";
TOK_AUTOMATIC , "automatic";
TOK_BASE , "base";
TOK_BEGIN , "begin";
TOK_BYTE , "byte";
TOK_CASE , "case";
TOK_CASEX , "casex";
TOK_CASEZ , "casez";
TOK_CHECKER , "checker";
TOK_CONST , "const";
TOK_COVER , "cover";
TOK_DECREMENT , "decrement";
TOK_DEFAULT , "default";
TOK_DEFPARAM , "defparam";
TOK_ELSE , "else";
TOK_END , "end";
TOK_ENDCASE , "endcase";
TOK_ENDCHECKER , "endchecker";
TOK_ENDFUNCTION , "endfunction";
TOK_ENDGENERATE , "endgenerate";
TOK_ENDINTERFACE , "endinterface";
TOK_ENDMODULE , "endmodule";
TOK_ENDPACKAGE , "endpackage";
TOK_ENDSPECIFY , "endspecify";
TOK_ENDTASK , "endtask";
TOK_ENUM , "enum";
TOK_EVENTUALLY , "eventually";
TOK_FINAL , "final";
TOK_FOR , "for";
TOK_FUNCTION , "function";
TOK_GENERATE , "generate";
TOK_GENVAR , "genvar";
TOK_IF , "if";
TOK_INCREMENT , "increment";
TOK_INITIAL , "initial";
TOK_INOUT , "inout";
TOK_INPUT , "input";
TOK_INT , "int";
TOK_INTEGER , "integer";
TOK_INTERFACE , "interface";
TOK_LOCALPARAM , "localparam";
TOK_LOGIC , "logic";
TOK_MODPORT , "modport";
TOK_MODULE , "module";
TOK_MSG_TASKS , "msg_TASKS";
TOK_NEGEDGE , "negedge";
TOK_OR , "or";
TOK_OUTPUT , "output";
TOK_PACKAGE , "package";
TOK_PACKAGESEP , "packagesep";
TOK_PACKED , "packed";
TOK_PARAMETER , "parameter";
TOK_POSEDGE , "posedge";
TOK_PRIMITIVE , "primitive";
TOK_PRIORITY , "priority";
TOK_PROPERTY , "property";
TOK_RAND , "rand";
TOK_REAL , "real";
TOK_REALVAL , "realval";
TOK_REG , "reg";
TOK_REPEAT , "repeat";
TOK_RESTRICT , "restrict";
TOK_SHORTINT , "shortint";
TOK_SIGNED , "signed";
TOK_SPECIFY , "specify";
TOK_SPECPARAM , "specparam";
TOK_STRUCT , "struct";
TOK_SUB_ASSIGN , "sub_assign";
TOK_SUPPLY0 , "supply0";
TOK_SUPPLY1 , "supply1";
TOK_SVA_LABEL , "sva_LABEL";
TOK_SYNOPSYS_FULL_CASE , "synopsys_full_case";
TOK_SYNOPSYS_PARALLEL_CASE , "synopsys_parallel_case";
TOK_TASK , "task";
TOK_TYPEDEF , "typedef";
TOK_UNION , "union";
TOK_UNIQUE , "unique";
TOK_UNSIGNED , "unsigned";
TOK_VAR , "var";
TOK_WAND , "wand";
TOK_WHILE , "while";
TOK_WIRE , "wire";
TOK_WOR , "wor";

      ];
    fun s -> Hashtbl.find h s

let tok' = function
| ACCEPT  -> "ACCEPT"
| AMPERSAND  -> "AMPERSAND"
| AT  -> "AT"
| ATTR_BEGIN  -> "ATTR_BEGIN"
| ATTR_END  -> "ATTR_END"
| BACKQUOTE  -> "BACKQUOTE"
| BACKSLASH  -> "BACKSLASH"
| CARET  -> "CARET"
| COLON  -> "COLON"
| COMMA  -> "COMMA"
| CONS1  _ -> "CONS1"
| CONS2  _ -> "CONS2"
| CONS3  _ -> "CONS3"
| CONS4  _ -> "CONS4"
| DEFATTR_BEGIN  -> "DEFATTR_BEGIN"
| DEFATTR_END  -> "DEFATTR_END"
| DEFAULT  -> "DEFAULT"
| DOLLAR  -> "DOLLAR"
| DOT  -> "DOT"
| DOUBLEQUOTE  -> "DOUBLEQUOTE"
| ELIST  _ -> "ELIST"
| EMPTY_TOKEN  -> "EMPTY_TOKEN"
| END  -> "END"
| EOF_TOKEN  -> "EOF_TOKEN"
| EQUALS  -> "EQUALS"
| ERROR  -> "ERROR"
| ERROR_TOKEN  -> "ERROR_TOKEN"
| FAKE_THEN  -> "FAKE_THEN"
| GREATER  -> "GREATER"
| HASH  -> "HASH"
| HYPHEN  -> "HYPHEN"
| LBRACE  -> "LBRACE"
| LBRACK  -> "LBRACK"
| LESS  -> "LESS"
| LINEFEED  -> "LINEFEED"
| LPAREN  -> "LPAREN"
| OP_EQ  -> "OP_EQ"
| OP_EQX  -> "OP_EQX"
| OP_GE  -> "OP_GE"
| OP_LAND  -> "OP_LAND"
| OP_LE  -> "OP_LE"
| OP_LOR  -> "OP_LOR"
| OP_NAND  -> "OP_NAND"
| OP_NE  -> "OP_NE"
| OP_NEX  -> "OP_NEX"
| OP_NOR  -> "OP_NOR"
| OP_POW  -> "OP_POW"
| OP_SHL  -> "OP_SHL"
| OP_SHR  -> "OP_SHR"
| OP_SSHL  -> "OP_SSHL"
| OP_SSHR  -> "OP_SSHR"
| OP_XNOR  -> "OP_XNOR"
| PERCENT  -> "PERCENT"
| PLING  -> "PLING"
| PLUS  -> "PLUS"
| QUERY  -> "QUERY"
| QUOTE  -> "QUOTE"
| RBRACE  -> "RBRACE"
| RBRACK  -> "RBRACK"
| RPAREN  -> "RPAREN"
| SEMICOLON  -> "SEMICOLON"
| SLASH  -> "SLASH"
| SLIST  _ -> "SLIST"
| STAR  -> "STAR"
| STRING  _ -> "STRING"
| TILDE  -> "TILDE"
| TLIST  _ -> "TLIST"
| TOK_ALWAYS  -> "TOK_ALWAYS"
| TOK_ALWAYS_COMB  -> "TOK_ALWAYS_COMB"
| TOK_ALWAYS_FF  -> "TOK_ALWAYS_FF"
| TOK_ALWAYS_LATCH  -> "TOK_ALWAYS_LATCH"
| TOK_ASSERT  -> "TOK_ASSERT"
| TOK_ASSIGN  -> "TOK_ASSIGN"
| TOK_ASSUME  -> "TOK_ASSUME"
| TOK_AUTOMATIC  -> "TOK_AUTOMATIC"
| TOK_BASE  -> "TOK_BASE"
| TOK_BASED_CONSTVAL  -> "TOK_BASED_CONSTVAL"
| TOK_BEGIN  -> "TOK_BEGIN"
| TOK_BYTE  -> "TOK_BYTE"
| TOK_CASE  -> "TOK_CASE"
| TOK_CASEX  -> "TOK_CASEX"
| TOK_CASEZ  -> "TOK_CASEZ"
| TOK_CHECKER  -> "TOK_CHECKER"
| TOK_CONST  -> "TOK_CONST"
| TOK_COVER  -> "TOK_COVER"
| TOK_DECREMENT  -> "TOK_DECREMENT"
| TOK_DEFAULT  -> "TOK_DEFAULT"
| TOK_DEFPARAM  -> "TOK_DEFPARAM"
| TOK_DPI_FUNCTION  -> "TOK_DPI_FUNCTION"
| TOK_ELSE  -> "TOK_ELSE"
| TOK_END  -> "TOK_END"
| TOK_ENDCASE  -> "TOK_ENDCASE"
| TOK_ENDCHECKER  -> "TOK_ENDCHECKER"
| TOK_ENDFUNCTION  -> "TOK_ENDFUNCTION"
| TOK_ENDGENERATE  -> "TOK_ENDGENERATE"
| TOK_ENDINTERFACE  -> "TOK_ENDINTERFACE"
| TOK_ENDMODULE  -> "TOK_ENDMODULE"
| TOK_ENDPACKAGE  -> "TOK_ENDPACKAGE"
| TOK_ENDSPECIFY  -> "TOK_ENDSPECIFY"
| TOK_ENDTASK  -> "TOK_ENDTASK"
| TOK_ENUM  -> "TOK_ENUM"
| TOK_EVENTUALLY  -> "TOK_EVENTUALLY"
| TOK_FINAL  -> "TOK_FINAL"
| TOK_FOR  -> "TOK_FOR"
| TOK_FUNCTION  -> "TOK_FUNCTION"
| TOK_GENERATE  -> "TOK_GENERATE"
| TOK_GENVAR  -> "TOK_GENVAR"
| TOK_IF  -> "TOK_IF"
| TOK_IGNORED_SPECIFY  -> "TOK_IGNORED_SPECIFY"
| TOK_IGNORED_SPECIFY_AND  -> "TOK_IGNORED_SPECIFY_AND"
| TOK_INCREMENT  -> "TOK_INCREMENT"
| TOK_INITIAL  -> "TOK_INITIAL"
| TOK_INOUT  -> "TOK_INOUT"
| TOK_INPUT  -> "TOK_INPUT"
| TOK_INT  -> "TOK_INT"
| TOK_INTEGER  -> "TOK_INTEGER"
| TOK_INTERFACE  -> "TOK_INTERFACE"
| TOK_LOCALPARAM  -> "TOK_LOCALPARAM"
| TOK_LOGIC  -> "TOK_LOGIC"
| TOK_MODPORT  -> "TOK_MODPORT"
| TOK_MODULE  -> "TOK_MODULE"
| TOK_MSG_TASKS  -> "TOK_MSG_TASKS"
| TOK_NEGEDGE  -> "TOK_NEGEDGE"
| TOK_NEG_INDEXED  -> "TOK_NEG_INDEXED"
| TOK_OR  -> "TOK_OR"
| TOK_OUTPUT  -> "TOK_OUTPUT"
| TOK_PACKAGE  -> "TOK_PACKAGE"
| TOK_PACKAGESEP  -> "TOK_PACKAGESEP"
| TOK_PACKED  -> "TOK_PACKED"
| TOK_PARAMETER  -> "TOK_PARAMETER"
| TOK_PKG_USER_TYPE  -> "TOK_PKG_USER_TYPE"
| TOK_POSEDGE  -> "TOK_POSEDGE"
| TOK_POS_INDEXED  -> "TOK_POS_INDEXED"
| TOK_PRIMITIVE  -> "TOK_PRIMITIVE"
| TOK_PRIORITY  -> "TOK_PRIORITY"
| TOK_PROPERTY  -> "TOK_PROPERTY"
| TOK_RAND  -> "TOK_RAND"
| TOK_REAL  -> "TOK_REAL"
| TOK_REALVAL  -> "TOK_REALVAL"
| TOK_REG  -> "TOK_REG"
| TOK_REPEAT  -> "TOK_REPEAT"
| TOK_RESTRICT  -> "TOK_RESTRICT"
| TOK_SHORTINT  -> "TOK_SHORTINT"
| TOK_SIGNED  -> "TOK_SIGNED"
| TOK_SPECIFY  -> "TOK_SPECIFY"
| TOK_SPECIFY_AND  -> "TOK_SPECIFY_AND"
| TOK_SPECIFY_OPER  -> "TOK_SPECIFY_OPER"
| TOK_SPECPARAM  -> "TOK_SPECPARAM"
| TOK_STRUCT  -> "TOK_STRUCT"
| TOK_SUB_ASSIGN  -> "TOK_SUB_ASSIGN"
| TOK_SUPPLY0  -> "TOK_SUPPLY0"
| TOK_SUPPLY1  -> "TOK_SUPPLY1"
| TOK_SVA_LABEL  -> "TOK_SVA_LABEL"
| TOK_SYNOPSYS_FULL_CASE  -> "TOK_SYNOPSYS_FULL_CASE"
| TOK_SYNOPSYS_PARALLEL_CASE  -> "TOK_SYNOPSYS_PARALLEL_CASE"
| TOK_TASK  -> "TOK_TASK"
| TOK_TO_SIGNED  -> "TOK_TO_SIGNED"
| TOK_TO_UNSIGNED  -> "TOK_TO_UNSIGNED"
| TOK_TYPEDEF  -> "TOK_TYPEDEF"
| TOK_UNBASED_UNSIZED_CONSTVAL  -> "TOK_UNBASED_UNSIZED_CONSTVAL"
| TOK_UNION  -> "TOK_UNION"
| TOK_UNIQUE  -> "TOK_UNIQUE"
| TOK_UNSIGNED  -> "TOK_UNSIGNED"
| TOK_VAR  -> "TOK_VAR"
| TOK_WAND  -> "TOK_WAND"
| TOK_WHILE  -> "TOK_WHILE"
| TOK_WILDCARD_CONNECT  -> "TOK_WILDCARD_CONNECT"
| TOK_WIRE  -> "TOK_WIRE"
| TOK_WOR  -> "TOK_WOR"
| TUPLE10  _ -> "TUPLE10"
| TUPLE11  _ -> "TUPLE11"
| TUPLE12  _ -> "TUPLE12"
| TUPLE13  _ -> "TUPLE13"
| TUPLE14  _ -> "TUPLE14"
| TUPLE15  _ -> "TUPLE15"
| TUPLE16  _ -> "TUPLE16"
| TUPLE17  _ -> "TUPLE17"
| TUPLE18  _ -> "TUPLE18"
| TUPLE19  _ -> "TUPLE19"
| TUPLE2  _ -> "TUPLE2"
| TUPLE20  _ -> "TUPLE20"
| TUPLE21  _ -> "TUPLE21"
| TUPLE22  _ -> "TUPLE22"
| TUPLE23  _ -> "TUPLE23"
| TUPLE24  _ -> "TUPLE24"
| TUPLE25  _ -> "TUPLE25"
| TUPLE26  _ -> "TUPLE26"
| TUPLE3  _ -> "TUPLE3"
| TUPLE4  _ -> "TUPLE4"
| TUPLE5  _ -> "TUPLE5"
| TUPLE6  _ -> "TUPLE6"
| TUPLE7  _ -> "TUPLE7"
| TUPLE8  _ -> "TUPLE8"
| TUPLE9  _ -> "TUPLE9"
| UNARY_OPS  -> "UNARY_OPS"
| UNDERSCORE  -> "UNDERSCORE"
| VBAR  -> "VBAR"
| TOK_USER_TYPE t -> t
| TOK_STRING str -> str
| TOK_ID id -> id
| TOK_CONSTVAL n -> n

let import_seen = ref false

let tok arg = if verbose then Printf.printf "tokenToBison  TOKEN {c%d-%d:}=%s\n" (!lincnt+1) (!lincnt+1) (tok' arg);
  arg
}

let ident = ['a'-'z' 'A'-'Z' '$' '_'] ['a'-'z' 'A'-'Z' '_' '0'-'9' '$']*
let escaped = '\\'[^' ']*' '
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
*)
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
| '\\' { tok ( BACKSLASH ) }
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
      { tok ( TOK_CONSTVAL (n) ) }
  | number as n
      { tok ( TOK_CONSTVAL (n) ) }
  | dfltnum as n
      { tok ( TOK_CONSTVAL (n) ) }
  | ident as s
      { tok ( try keyword s with Not_found -> TOK_ID s ) }
  | escaped as s
      { let s = String.sub s 1 (String.length s - 2) in TOK_ID s }
  | qstring as s
      { tok ( STRING (String.sub s 1 (String.length s - 2)) ) }
  | eof
      { tok ( EOF_TOKEN ) }

| _ as oth
{ tok ( failwith ("Yosys_helper_lex: "^String.make 1 oth) ) }

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
