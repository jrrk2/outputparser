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

  let verbose = ref false
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
       BITS, "bits("; 
       CAT, "cat("; 
       CIRCUIT, "circuit"; 
       CLOCK, "Clock"; 
       CMEM, "cmem"; 
       CVT, "cvt("; 
       DATA_TYPE, "DATA_TYPE"; 
       DEFAULT, "DEFAULT"; 
       DEFNAME, "defname"; 
       DEPTH, "depth"; 
       DIV, "div("; 
       DSHL, "dshl("; 
       DSHR, "dshr("; 
       ELSE, "ELSE"; 
       EMPTY_TOKEN, "EMPTY_TOKEN"; 
       END, "END"; 
       EOF_TOKEN, "EOF_TOKEN"; 
       EQ, "eq("; 
       ERROR_TOKEN, "ERROR_TOKEN"; 
       ERROR, "ERROR"; 
       EXTMODULE, "extmodule"; 
       FLIP, "flip"; 
       GEQ, "geq("; 
       GT, "gt("; 
       HEAD, "head("; 
       INPUT, "input"; 
       INST, "inst"; 
       IS, "is"; 
       LEQ, "leq("; 
       LESS, "LESS"; 
       LT, "lt("; 
       MEM, "mem("; 
       MODULE, "module"; 
       MOD, "mod("; 
       MPORT, "mport"; 
       MUL, "mul("; 
       MUX, "mux("; 
       NEG, "neg("; 
       NEQ, "neq("; 
       NEW, "NEW"; 
       NODE, "node"; 
       NOT, "not("; 
       OF, "of"; 
       OLD, "OLD"; 
       ORR, "orr("; 
       OR, "or("; 
       OUTPUT, "output"; 
       PAD, "pad("; 
       PARAMETER, "parameter"; 
       PLING, "PLING"; 
       PRINTF, "printf("; 
       QUOTE, "QUOTE"; 
       READER, "READER"; 
       READ_LATENCY, "READ_LATENCY"; 
       READ_UNDER_WRITE, "READ_UNDER_WRITE"; 
       READWRITER, "READWRITER"; 
       REG, "reg"; 
       SHL, "shl("; 
       SHR, "shr("; 
       SINT, "SInt"; 
       SKIP, "skip"; 
       SMEM, "smem"; 
       STOP, "stop("; 
       SUB, "sub("; 
       TAIL, "tail("; 
       TILDE, "TILDE"; 
       UINT, "UInt"; 
       UNDEFINED, "UNDEFINED"; 
       VALIDIF, "VALIDIF"; 
       VBAR, "VBAR"; 
       WHEN, "when"; 
       WIRE, "wire"; 
       WITH, "with"; 
       WRITE_LATENCY, "WRITE_LATENCY"; 
       WRITER, "WRITER"; 
       XORR, "xorr("; 
       XOR, "xor("; 
      ];
    fun s -> Hashtbl.find h s

let tok arg = if !verbose then print_endline (
  match arg with
    | ID s -> "ID '"^s^"'"
    | STRING s -> "STRING '"^s^"'"
    | _ -> Ordfirrtl.getstr arg );
  arg
}

let identlpar = ['a'-'z' 'A'-'Z' '_' ] ['a'-'z' 'A'-'Z' '_' '0'-'9']*'('
let ident = ['a'-'z' 'A'-'Z' '_' ] ['a'-'z' 'A'-'Z' '_' '0'-'9']*
let number = ['-' '+']*['0'-'9']+
let space = [' ' '\t' '\r']+
let newline = ['\n']
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
  | eof
      { tok ( EOF_TOKEN ) }
| '!'
{ tok ( PLING ) }

| '"'
{ tok ( STRING (string (Buffer.create 100) lexbuf ) ) }

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

(* This bit contributed by Christian Lindig https://medium.com/@huund/recipes-for-ocamllex-bb4efa0afe53 *)

and string buf = parse
| [^'"' '\n' '\\']+ as s
            { Buffer.add_string buf @@ s
            ; string buf lexbuf 
            }
| '\n'      { Buffer.add_string buf @@ "\n"
            ; Lexing.new_line lexbuf
            ; string buf lexbuf
            }
| '\\' '"'  { Buffer.add_char buf '"'
            ; string buf lexbuf
            }
| '\\'      { Buffer.add_char buf '\\'
            ; string buf lexbuf
            }
| '"'       { Buffer.contents buf } (* return *)
| eof       { failwith "end of input inside of a string" }
| _         { failwith "found '%s' - don't know how to handle" }
