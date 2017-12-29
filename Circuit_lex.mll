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
  open Circuit

  let verbose = ref true
  let lincnt = ref 0

  let keyword =
    let h = Hashtbl.create 17 in
    List.iter 
      (fun (k,s) -> Hashtbl.add h s k)
      [
       ACCEPT, "ACCEPT"; 
       ADD, "ADD"; 
       AMPERSAND, "AMPERSAND"; 
       ANDR, "ANDR"; 
       AND, "AND"; 
       ASCLOCK, "ASCLOCK"; 
       ASSINT, "ASSINT"; 
       ASUINT, "ASUINT"; 
       AT, "AT"; 
       BACKQUOTE, "BACKQUOTE"; 
       BACKSLASH, "BACKSLASH"; 
       BITS, "BITS"; 
       BREAK, "BREAK"; 
       CARET, "CARET"; 
       CASE, "CASE"; 
       CAT, "CAT"; 
       CIRCUIT, "CIRCUIT"; 
       CLOCK, "CLOCK"; 
       COLON, "COLON"; 
       COMMA, "COMMA"; 
       CONTINUE, "CONTINUE"; 
       CVT, "CVT"; 
       DATA_TYPE, "DATA_TYPE"; 
       DEFAULT, "DEFAULT"; 
       DEPTH, "DEPTH"; 
       DIV, "DIV"; 
       DOLLAR, "DOLLAR"; 
       DOT, "DOT"; 
       DOUBLEQUOTE, "DOUBLEQUOTE"; 
       DO, "DO"; 
       DSHL, "DSHL"; 
       DSHR, "DSHR"; 
       ELSE, "ELSE"; 
       EMPTY_TOKEN, "EMPTY_TOKEN"; 
       END, "END"; 
       EOF_TOKEN, "EOF_TOKEN"; 
       EQUALS, "EQUALS"; 
       EQ, "EQ"; 
       ERROR_TOKEN, "ERROR_TOKEN"; 
       ERROR, "ERROR"; 
       EXTMODULE, "EXTMODULE"; 
       FLIP, "FLIP"; 
       FOR, "FOR"; 
       GEQ, "GEQ"; 
       GOTO, "GOTO"; 
       GREATER, "GREATER"; 
       GT, "GT"; 
       HASH, "HASH"; 
       HEAD, "HEAD"; 
       HYPHEN, "HYPHEN"; 
       IF, "IF"; 
       INPUT, "INPUT"; 
       INST, "INST"; 
       INVALID, "INVALID"; 
       IS, "IS"; 
       LBRACE, "LBRACE"; 
       LBRACK, "LBRACK"; 
       LEQ, "LEQ"; 
       LESS, "LESS"; 
       LINEFEED, "LINEFEED"; 
       LPAREN, "LPAREN"; 
       LT, "LT"; 
       MEM, "MEM"; 
       MODULE, "MODULE"; 
       MOD, "MOD"; 
       MUL, "MUL"; 
       MUX, "MUX"; 
       NEG, "NEG"; 
       NEQ, "NEQ"; 
       NEW, "NEW"; 
       NODE, "NODE"; 
       NOT, "NOT"; 
       OF, "OF"; 
       OLD, "OLD"; 
       ORR, "ORR"; 
       OR, "OR"; 
       OUTPUT, "OUTPUT"; 
       PAD, "PAD"; 
       PERCENT, "PERCENT"; 
       PLING, "PLING"; 
       PRINTF, "PRINTF"; 
       QUERY, "QUERY"; 
       QUOTE, "QUOTE"; 
       RBRACE, "RBRACE"; 
       RBRACK, "RBRACK"; 
       READER, "READER"; 
       READ_LATENCY, "READ_LATENCY"; 
       READ_UNDER_WRITE, "READ_UNDER_WRITE"; 
       READWRITER, "READWRITER"; 
       REG, "REG"; 
       RETURN, "RETURN"; 
       RPAREN, "RPAREN"; 
       SHL, "SHL"; 
       SHR, "SHR"; 
       SINT, "SINT"; 
       SKIP, "SKIP"; 
       STOP, "STOP"; 
       SUB, "SUB"; 
       SWITCH, "SWITCH"; 
       TAIL, "TAIL"; 
       TILDE, "TILDE"; 
       UINT, "UINT"; 
       UNDEFINED, "UNDEFINED"; 
       UNDERSCORE, "UNDERSCORE"; 
       VALIDIF, "VALIDIF"; 
       VBAR, "VBAR"; 
       WHEN, "WHEN"; 
       WHILE, "WHILE"; 
       WIRE, "WIRE"; 
       WRITE_LATENCY, "WRITE_LATENCY"; 
       WRITER, "WRITER"; 
       XORR, "XORR"; 
       XOR, "XOR"; 
      ];
    fun s -> Hashtbl.find h (String.uppercase s)

let tok arg = if !verbose then print_endline (
  match arg with
    | ID s -> "'"^s^"'"
    | _ -> Ordfirrtl.getstr arg );
  arg
}

let ident = ['a'-'z' 'A'-'Z' ] ['a'-'z' 'A'-'Z' '_' '0'-'9']*
let number = ['-' '+']*['0'-'9']+
let space = [' ' '\t' '\r']+
let newline = ['\n']
let qstring = '"'[^'"']*'"'
let comment = ';'[^'\n']*

rule token = parse
  | comment
      { token lexbuf }
  | space
      { token lexbuf }
  | newline
      { incr lincnt; token lexbuf }
  | number as n
      { tok ( INT (int_of_string n) ) }
  | ident as s
      { tok ( try keyword s with Not_found -> ID s ) }
  | qstring as s
      { tok ( STRING s ) }
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
(*
| '*'
{ tok ( STAR ) }

| '+'
{ tok ( PLUS ) }
*)
| ','
{ tok ( COMMA ) }

| '-'
{ tok ( HYPHEN ) }

| '.'
{ tok ( DOT ) }
(*
| '/'
{ tok ( SLASH ) }
*)
| '\\'
{ tok ( BACKSLASH ) }

| ':'
{ tok ( COLON ) }
(*
| ';'
{ tok ( SEMICOLON ) }
*)
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
