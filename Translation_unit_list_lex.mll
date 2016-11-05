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

  let verbose = ref (try Sys.getenv ("LEX_VERBOSE") <> "" with _ -> false)
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
let semicolon_seen = ref false

let tok lexbuf arg =
  let tran = function
  | CONSTANT s -> "Constant: "^s
  | IDENTIFIER s -> "Ident: "^s
  | arg -> Translation_unit_list_types.getstr arg in
if !verbose then print_endline (string_of_int (lexeme_start lexbuf)^": "^tran arg);
toklst := arg :: !toklst;
semicolon_seen := arg = SEMICOLON; 
arg

let probable_type s =
let len = String.length s in
let rslt = len >= 3 && String.sub s (len-2) 2 = "_t" && !semicolon_seen in
semicolon_seen := false;
print_endline ("!!!! "^s^": "^string_of_bool rslt);
rslt
}

let D	=		['0'-'9']
let L	=		['a'-'z' 'A'-'Z' '_']
let H	=		['a'-'f' 'A'-'F' '0'-'9']
let E	=		(['E' 'e']['+' '-']?D+)
let P   =              (['P' 'p']['+' '-']?D+)
let FS	=		('f'|'F'|'l'|'L')
let IS  =              (('u'|'U')|('u'|'U')?('l'|'L'|'l''l'|'L''L')|('l'|'L'|'l''l'|'L''L')('u'|'U'))

rule token = parse
  | [ ' ' '\t' ]  { token lexbuf }
  | [ '\n' ]      	        { incr lincnt; token lexbuf }
  | "//"[^'\n']* 			{ token lexbuf }
  | L(L|D)* as s        {
     if !verbose then print_endline ("A: "^s);
     tok lexbuf (try keyword s with Not_found -> if Hashtbl.mem typehash s || probable_type s then TYPE_NAME s else IDENTIFIER s); }
  | '0'['x' 'X']H+(IS)? as s       { if !verbose then print_endline ("B: "^s); tok lexbuf (CONSTANT s); }
  | '0'['0'-'7']*(IS)? as s        { if !verbose then print_endline ("C: "^s); tok lexbuf (CONSTANT s); }
  | ['1'-'9'](D)*(IS)? as s        { if !verbose then print_endline ("D: "^s); tok lexbuf (CONSTANT s); }
  | (D)+(E)(FS)? as s              { if !verbose then print_endline ("F: "^s); tok lexbuf (CONSTANT s); }
  | (D)*"."(D)+(E)?(FS)? as s      { if !verbose then print_endline ("G: "^s); tok lexbuf (CONSTANT s); }
  | (D)+"."(D)*(E)?(FS)? as s      { if !verbose then print_endline ("H: "^s); tok lexbuf (CONSTANT s); }
  | '0'['x' 'X'](H)+(P)(FS)? as s  { if !verbose then print_endline ("I: "^s); tok lexbuf (CONSTANT s); }
  | '0'['x' 'X'](H)*"."(H)+(P)?(FS)? as s   { if !verbose then print_endline ("J: "^s); tok lexbuf (CONSTANT s); }
  | '0'['x' 'X'](H)+"."(H)*(P)?(FS)? as s   { if !verbose then print_endline ("K: "^s); tok lexbuf (CONSTANT s); }
  | '\"'[^'"']*'\"' as s	{ if !verbose then print_endline ("L: "^s); tok lexbuf (STRING_LITERAL s); }
  | '\''[^''']*'\'' as s	{ if !verbose then print_endline ("L: "^s); tok lexbuf (CONSTANT s); }
  | "..."			{ tok lexbuf (ELLIPSIS); }
  | ">>="			{ tok lexbuf (RIGHT_ASSIGN); }
  | "<<="			{ tok lexbuf (LEFT_ASSIGN); }
  | "+="			{ tok lexbuf (ADD_ASSIGN); }
  | "-="			{ tok lexbuf (SUB_ASSIGN); }
  | "*="			{ tok lexbuf (MUL_ASSIGN); }
  | "/="			{ tok lexbuf (DIV_ASSIGN); }
  | "%="			{ tok lexbuf (MOD_ASSIGN); }
  | "&="			{ tok lexbuf (AND_ASSIGN); }
  | "^="			{ tok lexbuf (XOR_ASSIGN); }
  | "|="			{ tok lexbuf (OR_ASSIGN); }
  | ">>"			{ tok lexbuf (RIGHT_OP); }
  | "<<"			{ tok lexbuf (LEFT_OP); }
  | "++"			{ tok lexbuf (INC_OP); }
  | "--"			{ tok lexbuf (DEC_OP); }
  | "->"			{ tok lexbuf (PTR_OP); }
  | "&&"			{ tok lexbuf (AND_OP); }
  | "||"			{ tok lexbuf (OR_OP); }
  | "<="			{ tok lexbuf (LE_OP); }
  | ">="			{ tok lexbuf (GE_OP); }
  | "=="			{ tok lexbuf (EQ_OP); }
  | "!="			{ tok lexbuf (NE_OP); }
  | ("{"|"<%")		{ tok lexbuf (LBRACE); }
  | ("}"|"%>")		{ tok lexbuf (RBRACE); }
  | ("["|"<:")		{ tok lexbuf (LBRACK); }
  | ("]"|":>")		{ tok lexbuf (RBRACK); }
| '!'
{ tok lexbuf ( PLING ) }

| '"'
{ tok lexbuf ( DOUBLEQUOTE ) }

| '#'
{ tok lexbuf ( HASH ) }

| '$'
{ tok lexbuf ( DOLLAR ) }

| '%'
{ tok lexbuf ( PERCENT ) }

| '&'
{ tok lexbuf ( AMPERSAND ) }

| '\''
{ tok lexbuf ( QUOTE ) }

| '('
{ tok lexbuf ( LPAREN ) }

| '['
{ tok lexbuf ( LBRACK ) }

| '{'
{ tok lexbuf ( LBRACE ) }

| '<'
{ tok lexbuf ( LESS ) }

| ')'
{ tok lexbuf ( RPAREN ) }

| ']'
{ tok lexbuf ( RBRACK ) }

| '}'
{ tok lexbuf ( RBRACE ) }

| '>'
{ tok lexbuf ( GREATER ) }

| '*'
{ tok lexbuf ( STAR ) }

| '+'
{ tok lexbuf ( PLUS ) }

| ','
{ tok lexbuf ( COMMA ) }

| '-'
{ tok lexbuf ( HYPHEN ) }

| '.'
{ tok lexbuf ( DOT ) }

| '/'
{ tok lexbuf ( SLASH ) }

| '\\'
{ tok lexbuf ( BACKSLASH ) }

| ':'
{ tok lexbuf ( COLON ) }

| ';'
{ tok lexbuf ( SEMICOLON ) }

| '='
{ tok lexbuf ( EQUALS ) }

| '?'
{ tok lexbuf ( QUERY ) }

| '@'
{ tok lexbuf ( AT ) }

| '^'
{ tok lexbuf ( CARET ) }

| '_'
{ tok lexbuf ( UNDERSCORE ) }

| '`'
{ tok lexbuf ( BACKQUOTE ) }

| '|'
{ tok lexbuf ( VBAR ) }

| '~'
{ tok lexbuf ( TILDE ) }

| eof
      { tok lexbuf ( EOF_TOKEN ) }

| _ as oth
{ tok lexbuf ( failwith ("lex_file_lex: "^String.make 1 oth) ) }
