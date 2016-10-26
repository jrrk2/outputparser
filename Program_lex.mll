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
  open Program_edited

  let verbose = ref false
  let lincnt = ref 0

  let keyword =
    let h = Hashtbl.create 17 in
    List.iter 
      (fun (k,s) -> Hashtbl.add h s k)
      [
      ];
    fun s -> Hashtbl.find h (String.uppercase s)

let toklst = ref []

let tok arg = if !verbose then print_endline (Program_types.getstr arg);
  toklst := arg :: !toklst;
  arg
}

let ident = ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '_' '0'-'9']*
let fltnum = ['-' '+']*['0'-'9']*['.']*['0'-'9']*['E' '-' '+' '0'-'9']*
let number = ['-' '+']*['0'-'9']+
let percent = ['0'-'9']+'%'
let space = [' ' '\t' '\r']+
let newline = ['\n']
let qstring = '"'[^'"']*'"'
let ampident = '&'[^' ']*
let comment = '#'[^'\n']*
let pattern = ['0'-'9' 'A'-'R' 'a'-'f']+'_'['0'-'9' 'A'-'R' 'a'-'f' '_']*

rule token = parse
  | comment
      { token lexbuf }
  | space
      { token lexbuf }
  | newline
      { incr lincnt; token lexbuf }
  | pattern as s
      { tok ( STRING s ) }
  | percent as s
      { tok ( STRING s ) }
  | number as n
      { tok ( CONSTANT (float_of_string n) ) }
  | fltnum as n
      { tok ( match n with "E" -> STRING n | "." -> DOT | _ -> let f = try float_of_string n with _ -> failwith ("fltnum "^n) in CONSTANT f ) }
  | ident as s
      { tok ( try keyword s with Not_found -> STRING s ) }
  | ampident as s
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
