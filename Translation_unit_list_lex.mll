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
  open Translation_unit_list
  open Translation_unit_list_types

  let verbose = ref false
  let lincnt = ref 0

  let keyword =
    let h = Hashtbl.create 17 in
    List.iter 
      (fun (s,k) -> Hashtbl.add h s k)
      [
	"auto",(AUTO); 
	"_Bool",(BOOL); 
	"break",(BREAK); 
	"case",(CASE); 
	"char",(CHAR); 
	"_Complex",(COMPLEX); 
	"const",(CONST); 
	"continue",(CONTINUE); 
	"default",(DEFAULT); 
	"do",(DO); 
	"double",(DOUBLE); 
	"else",(ELSE); 
	"enum",(ENUM); 
	"extern",(EXTERN); 
	"float",(FLOAT); 
	"for",(FOR); 
	"goto",(GOTO); 
	"if",(IF); 
	"_Imaginary",(IMAGINARY); 
	"inline",(INLINE); 
	"int",(INT); 
	"long",(LONG); 
	"register",(REGISTER); 
	"restrict",(RESTRICT); 
	"return",(RETURN); 
	"short",(SHORT); 
	"signed",(SIGNED); 
	"sizeof",(SIZEOF); 
	"static",(STATIC); 
	"struct",(STRUCT); 
	"switch",(SWITCH); 
	"typedef",(TYPEDEF); 
	"union",(UNION); 
	"unsigned",(UNSIGNED); 
	"void",(VOID); 
	"volatile",(VOLATILE); 
	"while",(WHILE); 
      ];
    fun s -> Hashtbl.find h s

let toklst = ref []

let tok arg =
  let tran = function
  | CONSTANT s -> "Constant: "^s
  | IDENTIFIER s -> "Ident: "^s
  | arg -> Translation_unit_list_types.getstr arg in
if !verbose then print_endline (tran arg);
toklst := arg :: !toklst;
arg
}

let D	=		['0'-'9']
let L	=		['a'-'z' 'A'-'Z' '_']
let H	=		['a'-'f' 'A'-'F' '0'-'9']
let E	=		(['E' 'e']['+' '-']?D+)
let P   =               (['P' 'p']['+' '-']?D+)
let FS	=		('f'|'F'|'l'|'L')
let IS  =               (('u'|'U')|('u'|'U')?('l'|'L'|'l''l'|'L''L')|('l'|'L'|'l''l'|'L''L')('u'|'U'))

rule token = parse
  | [ ' ' '\t' ]  { token lexbuf }
  | [ '\n' ]      	        { incr lincnt; token lexbuf }
  | "//"[^'\n']* 			{ token lexbuf }
  | L(L|D)* as s        {
     if !verbose then print_endline ("A: "^s);
     tok(try keyword s with Not_found -> if Hashtbl.mem typehash s then TYPE_NAME s else IDENTIFIER s); }
  | '0'['x' 'X']H+(IS)? as s       { if !verbose then print_endline ("B: "^s); tok(CONSTANT s); }
  | '0'['0'-'7']*(IS)? as s        { if !verbose then print_endline ("C: "^s); tok(CONSTANT s); }
  | ['1'-'9'](D)*(IS)? as s        { if !verbose then print_endline ("D: "^s); tok(CONSTANT s); }
(*
  | L?('\\''.'|[^'\\''\n'])+ as s  { if !verbose then print_endline ("E: "^s); tok(CONSTANT s); }
*)
  | (D)+(E)(FS)? as s              { if !verbose then print_endline ("F: "^s); tok(CONSTANT s); }
  | (D)*"."(D)+(E)?(FS)? as s      { if !verbose then print_endline ("G: "^s); tok(CONSTANT s); }
  | (D)+"."(D)*(E)?(FS)? as s      { if !verbose then print_endline ("H: "^s); tok(CONSTANT s); }
  | '0'['x' 'X'](H)+(P)(FS)? as s  { if !verbose then print_endline ("I: "^s); tok(CONSTANT s); }
  | '0'['x' 'X'](H)*"."(H)+(P)?(FS)? as s   { if !verbose then print_endline ("J: "^s); tok(CONSTANT s); }
  | '0'['x' 'X'](H)+"."(H)*(P)?(FS)? as s   { if !verbose then print_endline ("K: "^s); tok(CONSTANT s); }
  | '\"'[^'"']*'\"' as s	{ if !verbose then print_endline ("L: "^s); tok(STRING_LITERAL s); }
  | "..."			{ tok(ELLIPSIS); }
  | ">>="			{ tok(RIGHT_ASSIGN); }
  | "<<="			{ tok(LEFT_ASSIGN); }
  | "+="			{ tok(ADD_ASSIGN); }
  | "-="			{ tok(SUB_ASSIGN); }
  | "*="			{ tok(MUL_ASSIGN); }
  | "/="			{ tok(DIV_ASSIGN); }
  | "%="			{ tok(MOD_ASSIGN); }
  | "&="			{ tok(AND_ASSIGN); }
  | "^="			{ tok(XOR_ASSIGN); }
  | "|="			{ tok(OR_ASSIGN); }
  | ">>"			{ tok(RIGHT_OP); }
  | "<<"			{ tok(LEFT_OP); }
  | "++"			{ tok(INC_OP); }
  | "--"			{ tok(DEC_OP); }
  | "->"			{ tok(PTR_OP); }
  | "&&"			{ tok(AND_OP); }
  | "||"			{ tok(OR_OP); }
  | "<="			{ tok(LE_OP); }
  | ">="			{ tok(GE_OP); }
  | "=="			{ tok(EQ_OP); }
  | "!="			{ tok(NE_OP); }
  | ("{"|"<%")		{ tok(LBRACE); }
  | ("}"|"%>")		{ tok(RBRACE); }
  | ("["|"<:")		{ tok(LBRACK); }
  | ("]"|":>")		{ tok(RBRACK); }
| '!'
{ tok ( PLING ) }

| '"'
{ tok ( DOUBLEQUOTE ) }

| '#'
{ tok ( HASH ) }

| '$'
{ tok ( DOLLAR ) }

| '%'
{ tok ( PERCENT ) }

| '&'
{ tok ( AMPERSAND ) }

| '''
{ tok ( QUOTE ) }

| '('
{ tok ( LPAREN ) }

| '['
{ tok ( LBRACK ) }

| '{'
{ tok ( LBRACE ) }

| '<'
{ tok ( LESS ) }

| ')'
{ tok ( RPAREN ) }

| ']'
{ tok ( RBRACK ) }

| '}'
{ tok ( RBRACE ) }

| '>'
{ tok ( GREATER ) }

| '*'
{ tok ( STAR ) }

| '+'
{ tok ( PLUS ) }

| ','
{ tok ( COMMA ) }

| '-'
{ tok ( HYPHEN ) }

| '.'
{ tok ( DOT ) }

| '/'
{ tok ( SLASH ) }

| '\\'
{ tok ( BACKSLASH ) }

| ':'
{ tok ( COLON ) }

| ';'
{ tok ( SEMICOLON ) }

| '='
{ tok ( EQUALS ) }

| '?'
{ tok ( QUERY ) }

| '@'
{ tok ( AT ) }

| '^'
{ tok ( CARET ) }

| '_'
{ tok ( UNDERSCORE ) }

| '`'
{ tok ( BACKQUOTE ) }

| '|'
{ tok ( VBAR ) }

| '~'
{ tok ( TILDE ) }

| eof
      { tok ( EOF_TOKEN ) }

| _ as oth
{ tok ( failwith ("lex_file_lex: "^String.make 1 oth) ) }
