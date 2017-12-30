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
       ADD, "add("; 
       AMPERSAND, "AMPERSAND"; 
       ANDR, "andr("; 
       AND, "and("; 
       ASCLOCK, "asClock("; 
       ASSINT, "asSInt("; 
       ASUINT, "asUInt("; 
       AT, "AT"; 
       BACKQUOTE, "BACKQUOTE"; 
       BACKSLASH, "BACKSLASH"; 
       BITS, "bits("; 
       BREAK, "BREAK"; 
       CARET, "CARET"; 
       CASE, "CASE"; 
       CAT, "cat("; 
       CIRCUIT, "circuit"; 
       CLOCK, "Clock"; 
       COLON, "COLON"; 
       COMMA, "COMMA"; 
       CONTINUE, "CONTINUE"; 
       CVT, "cvt("; 
       DATA_TYPE, "DATA_TYPE"; 
       DEFAULT, "DEFAULT"; 
       DEPTH, "DEPTH"; 
       DIV, "div("; 
       DOLLAR, "DOLLAR"; 
       DOT, "DOT"; 
       DOUBLEQUOTE, "DOUBLEQUOTE"; 
       DO, "DO"; 
       DSHL, "dshl("; 
       DSHR, "dshr("; 
       ELSE, "ELSE"; 
       EMPTY_TOKEN, "EMPTY_TOKEN"; 
       END, "END"; 
       EOF_TOKEN, "EOF_TOKEN"; 
       EQUALS, "EQUALS"; 
       EQ, "eq("; 
       ERROR_TOKEN, "ERROR_TOKEN"; 
       ERROR, "ERROR"; 
       EXTMODULE, "EXTMODULE"; 
       FLIP, "flip"; 
       FOR, "FOR"; 
       GEQ, "geq("; 
       GOTO, "GOTO"; 
       GREATER, "GREATER"; 
       GT, "gt("; 
       HASH, "HASH"; 
       HEAD, "head("; 
       HYPHEN, "HYPHEN"; 
       IF, "IF"; 
       INPUT, "input"; 
       INST, "INST"; 
       INVALID, "invalid"; 
       IS, "is"; 
       LBRACE, "LBRACE"; 
       LBRACK, "LBRACK"; 
       LEQ, "leq("; 
       LESS, "LESS"; 
       LINEFEED, "LINEFEED"; 
       LPAREN, "LPAREN"; 
       LT, "lt("; 
       MEM, "MEM"; 
       MODULE, "module"; 
       MOD, "mod("; 
       MUL, "mul("; 
       MUX, "mux("; 
       NEG, "neg("; 
       NEQ, "neq("; 
       NEW, "NEW"; 
       NODE, "node"; 
       NOT, "not("; 
       OF, "OF"; 
       OLD, "OLD"; 
       ORR, "orr("; 
       OR, "or("; 
       OUTPUT, "output"; 
       PAD, "pad("; 
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
       REG, "reg"; 
       RETURN, "RETURN"; 
       RPAREN, "RPAREN"; 
       SHL, "shl("; 
       SHR, "shr("; 
       SINT, "SInt"; 
       SKIP, "SKIP"; 
       STOP, "STOP"; 
       SUB, "sub("; 
       SWITCH, "SWITCH"; 
       TAIL, "tail("; 
       TILDE, "TILDE"; 
       UINT, "UInt"; 
       UNDEFINED, "UNDEFINED"; 
       UNDERSCORE, "UNDERSCORE"; 
       VALIDIF, "VALIDIF"; 
       VBAR, "VBAR"; 
       WHEN, "WHEN"; 
       WHILE, "WHILE"; 
       WIRE, "wire"; 
       WRITE_LATENCY, "WRITE_LATENCY"; 
       WRITER, "WRITER"; 
       XORR, "xorr("; 
       XOR, "xor("; 
      ];
    fun s -> Hashtbl.find h s

let tok arg = if !verbose then print_endline (
  match arg with
    | ID s -> "'"^s^"'"
    | _ -> Ordfirrtl.getstr arg );
  arg
}

let identlpar = ['a'-'z' 'A'-'Z' '_' ] ['a'-'z' 'A'-'Z' '_' '0'-'9']*'('
let ident = ['a'-'z' 'A'-'Z' '_' ] ['a'-'z' 'A'-'Z' '_' '0'-'9']*
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
  | identlpar as s
      { tok ( try keyword s with Not_found -> STRING s ) }
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
