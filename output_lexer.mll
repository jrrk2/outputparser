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
  open Output_parser

  let verbose = ref false
  let lincnt = ref 0

  let keyword =
    let h = Hashtbl.create 17 in
    List.iter 
      (fun (k,s) -> Hashtbl.add h s k)
      [
    TERMINALS, "Terminals";
    NONTERMINALS, "Nonterminals";
    GRAMMAR, "Grammar";
    STATE, "State";
      ];
    fun s -> Hashtbl.find h s

let tok arg = if !verbose then print_endline ( match arg with
  | ID id -> id
  | NUMBER n -> string_of_int n
  | CHAR ch -> String.make 1 ch
  | oth -> Ord.getstr oth );
  arg
}

let ident = ['a'-'z' 'A'-'Z' ] ['a'-'z' 'A'-'Z' '_' '0'-'9']*
let quoted = '"' ['a'-'z' 'A'-'Z' ':' '/' '`' '+' '-' '*' '/' '%' '&' '|' '^' '<' '>' '.' '$' '\'' '~' '_' '0'-'9' ' ' '-' ':' '*' '/' '=' '<' '>' '{' '}' '[' ']' '+' '&' '$' '&' '!' '?' '@' '#' ]* '"'
let number = ['0'-'9']+
let space = [' ' '\t' '\r']+
let newline = ['\n']

rule token = parse
  | space
      { token lexbuf }
  | newline
      { incr lincnt; token lexbuf }
  | '\''
      { tok ( QUOTE ) }
  | number as n
      { tok ( NUMBER (int_of_string n) ) }
  | ident as s
      { tok ( try keyword s with Not_found -> ID s ) }
  | quoted as s
      { tok ( QUOTED (String.sub s 1 (String.length s - 2))) }
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

  | _ as c
      { tok ( CHAR c ) }
