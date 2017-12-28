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
  open CompilationUnit

  let verbose = ref true
  let lincnt = ref 0

  let keyword =
    let h = Hashtbl.create 17 in
    List.iter 
      (fun (k,s) -> Hashtbl.add h s k)
      [
	ABSTRACT, "ABSTRACT"; 
	ACCEPT, "ACCEPT"; 
	AMPERSAND, "AMPERSAND"; 
	AT, "AT"; 
	BACKQUOTE, "BACKQUOTE"; 
	BACKSLASH, "BACKSLASH"; 
	CARET, "CARET"; 
	CASE, "CASE"; 
	CATCH, "CATCH"; 
	CLASS, "CLASS"; 
	COLON, "COLON"; 
	COMMA, "COMMA"; 
	DEFAULT, "DEFAULT"; 
	DEF, "DEF"; 
	DOLLAR, "DOLLAR"; 
	DOT, "DOT"; 
	DOUBLEQUOTE, "DOUBLEQUOTE"; 
	DO, "DO"; 
	ELSE, "ELSE"; 
	EMPTY_TOKEN, "EMPTY_TOKEN"; 
	END, "END"; 
	EOF_TOKEN, "EOF_TOKEN"; 
	EQGT, "EQGT"; 
	EQUALS, "EQUALS"; 
	ERROR_TOKEN, "ERROR_TOKEN"; 
	ERROR, "ERROR"; 
	EXTENDS, "EXTENDS"; 
	FALSE, "FALSE"; 
	FINALLY, "FINALLY"; 
	FINAL, "FINAL"; 
	FOR_SOME, "FOR_SOME"; 
	FOR, "FOR"; 
	GREATER, "GREATER"; 
	HASH, "HASH"; 
	HYPHEN, "HYPHEN"; 
	IF, "IF"; 
	IMPLICIT, "IMPLICIT"; 
	IMPORT, "IMPORT"; 
	LAZY, "LAZY"; 
	LBRACE, "LBRACE"; 
	LBRACK, "LBRACK"; 
	LESS, "LESS"; 
	LINEFEED, "LINEFEED"; 
	LPAREN, "LPAREN"; 
	MATCH, "MATCH"; 
	NEWLINE, "NEWLINE"; 
	NEW, "NEW"; 
	NULL, "NULL"; 
	OBJECT, "OBJECT"; 
	OVERRIDE, "OVERRIDE"; 
	PACKAGE, "PACKAGE"; 
	PERCENT, "PERCENT"; 
	PLING, "PLING"; 
	PLUS, "PLUS"; 
	PRIVATE, "PRIVATE"; 
	PROTECTED, "PROTECTED"; 
	QUERY, "QUERY"; 
	QUOTE, "QUOTE"; 
	RBRACE, "RBRACE"; 
	RBRACK, "RBRACK"; 
	RETURN, "RETURN"; 
	RPAREN, "RPAREN"; 
	SEALED, "SEALED"; 
	SEMICOLON, "SEMICOLON"; 
	STAR, "STAR"; 
	SUPER, "SUPER"; 
	THIS, "THIS"; 
	THROW, "THROW"; 
	TILDE, "TILDE"; 
	TRAIT, "TRAIT"; 
	TRUE, "TRUE"; 
	TRY, "TRY"; 
	TYPE, "TYPE"; 
	UNDERSCORE, "UNDERSCORE"; 
	VAL, "VAL"; 
	VARID, "VARID"; 
	VAR, "VAR"; 
	VBAR, "VBAR"; 
	WHILE, "WHILE"; 
	WITH, "WITH"; 
	YIELD, "YIELD"; 
      ];
    fun s -> Hashtbl.find h (String.uppercase s)

let tok arg = if !verbose then print_endline (
  match arg with
    | PLAINID s -> "'"^s^"'"
    | _ -> Ordscala.getstr arg );
  arg
}

let ident = ['a'-'z' 'A'-'Z' ] ['a'-'z' 'A'-'Z' '_' '0'-'9']*
let fltnum = ['-' '+']*['0'-'9']*['.']*['0'-'9']*['E' '-' '+' '0'-'9']*
let number = ['-' '+']*['0'-'9']+
let space = [' ' '\t' '\r']+
let newline = ['\n']
let qstring = '"'[^'"']*'"'
let comment = '/''/'[^'\n']*

rule token = parse
  | comment
      { token lexbuf }
  | space
      { token lexbuf }
  | newline
      { incr lincnt; token lexbuf }
  | number as n
      { tok ( INTEGERLITERAL (int_of_string n) ) }
  | fltnum as n
      { tok ( let f = float_of_string n in FLOATINGPOINTLITERAL f ) }
  | ident as s
      { tok ( try keyword s with Not_found -> PLAINID s ) }
  | qstring as s
      { tok ( STRINGLITERAL s ) }
  | eof
      { tok ( EOF_TOKEN ) }
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

| _ as oth
{ tok ( failwith ("lex_file_lex: "^String.make 1 oth) ) }
