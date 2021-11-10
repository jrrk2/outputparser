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
  open Moore

  let verbose = try int_of_string(Sys.getenv "LEXER_VERBOSE") > 0 with _ -> false
  let lincnt = ref 0

  let keyword =
    let h = Hashtbl.create 17 in
    List.iter 
      (fun (k,s) -> Hashtbl.add h s k)
      [
      ELIST [], "ELIST";

      ];
    fun s -> Hashtbl.find h s

let import_seen = ref false

let tok arg =
  arg
}

let ident = ['a'-'z' 'A'-'Z' '$' '_' '\\'] ['a'-'z' 'A'-'Z' '_' '0'-'9' ]*
let number = ['0'-'9']['0'-'9']*
let space = [' ' '\t' '\r']+
let newline = ['\n']
let qchar = "'"['(' ')' '[' ']' '{' '}' '#' ':' ';' '.' ',' '=' '+' '-' '*'
        '/' '~' '|' '<' '>' '!' '%' '^' '&' '?' '\'' '"' '`' '$'
        '\\' '@']"'"
let qstring = '"'[^'"']*'"'
let qstring2 = '\''[^'\'']'\''
let comment = "//"[^'\n']*
let hcomment = "\n#"[^'\n']*

rule token = parse
| '#' { tok ( HASH ) }
| ''' { tok ( QUOTE ) }
| '[' { tok ( LBRACK ) }
| '{' { tok ( LBRACE ) }
| '<' { tok ( LESS ) }
| ']' { tok ( RBRACK ) }
| '}' { tok ( RBRACE ) }
| ',' { tok ( COMMA ) }
| ':' { tok ( COLON ) }
| '|' { tok ( VBAR ) }
| '~' { tok ( TILDE ) }
| '-' { tok ( HYPHEN ) }
| '+' { tok ( PLUS ) }
| '!' { tok ( PLING ) }
| '"' { tok ( DOUBLEQUOTE ) }
| '$' { tok ( DOLLAR ) }
| '%' { tok ( PERCENT ) }
| '&' { tok ( AMPERSAND ) }
| '(' { tok ( LPAREN ) }
| ')' { tok ( RPAREN ) }
| '>' { tok ( GREATER ) }
| '*' { tok ( STAR ) }
| '.' { tok ( DOT ) }
| '/' { tok ( SLASH ) }
| '\\' { tok ( BACKSLASH ) }
| ';' { tok ( SEMICOLON ) }
| '=' { tok ( EQUALS ) }
| '?' { tok ( QUERY ) }
| '@' { tok ( AT ) }
| '^' { tok ( CARET ) }
| '_' { tok ( UNDERSCORE ) }

| "/*" { comment lexbuf }
| "(* " { comment lexbuf }

  | comment as c
      { TOK_COMMENT c }
  | hcomment
      { token lexbuf }
  | space
      { token lexbuf }
  | newline
      { incr lincnt; token lexbuf }
  | number as n
      { tok ( TOK_INT (int_of_string n) ) }
  | ident as s
      { tok ( try keyword s with Not_found -> TOK_ID s ) }
  | qstring as s
      { tok ( TOK_STRING (String.sub s 1 (String.length s - 2)) ) }
  | qstring2 as s
      { tok ( TOK_OTH (Char.code s.[1]) ) }
  | eof
      { tok ( EOF_TOKEN ) }
  | qchar as oth
      { tok ( TOK_OTH (Char.code oth.[0]) ) }

| _ as oth { tok ( TOK_OTH (Char.code oth) ) }

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
