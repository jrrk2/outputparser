/**************************************************************************/
/*                                                                        */
/* OCaml template Copyright (C) 2004-2010                                 */
/*  Sylvain Conchon, Jean-Christophe Filliatre and Julien Signoles        */
/* Adapted to boolean logic by Jonathan Kimmitt                           */
/*  Copyright 2016 University of Cambridge                                */
/*                                                                        */
/*  This software is free software; you can redistribute it and/or        */
/*  modify it under the terms of the GNU Library General Public           */
/*  License version 2.1, with the special exception on linking            */
/*  described in file LICENSE.                                            */
/*                                                                        */
/*  This software is distributed in the hope that it will be useful,      */
/*  but WITHOUT ANY WARRANTY; without even the implied warranty of        */
/*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                  */
/*                                                                        */
/**************************************************************************/

/* $Id:$ */

%{
  open Parsing
  open Output_types

  let dollar = function
  | "accept" -> ACCEPT
  | "default" -> DEFAULT
  | "end" -> END
  | oth -> ID oth

%}

%token LINEFEED
%token AMPERSAND 
%token AT 
%token BACKQUOTE 
%token BACKSLASH 
%token CARET 
%token COLON 
%token COMMA 
%token ACCEPT
%token DEFAULT 
%token END
%token EMPTY
%token DOLLAR 
%token DOT 
%token DOUBLEQUOTE 
%token EQUALS 
%token GREATER 
%token HASH 
%token HYPHEN 
%token LBRACE 
%token LBRACK 
%token LESS 
%token LPAREN 
%token PERCENT 
%token PLING 
%token PLUS 
%token QUERY 
%token QUOTE 
%token RBRACE 
%token RBRACK 
%token RPAREN 
%token SEMICOLON 
%token SLASH 
%token STAR 
%token TILDE 
%token UNDERSCORE 
%token VBAR 
%token <char>   CHAR
%token <int>   NUMBER
%token <string>   ID;
%token    EOF_TOKEN
%token    STATE
%token    GRAMMAR
%token    TERMINALS
%token    NONTERMINALS
%token <int> DOLLAR_AT
%token <string list> UNUSED
%token <token*token list> GRAMITM
%token <token*token list> TERMS
%token <token*int> TERMITM
%token <token*int> NONTERMITM
%token <token*token list> STATEITM
%token <token list> TLIST
%type <string list*token list*token list*token list*token list> start
%type <token list> termlst
%type <token> termitm
%start start
%%


/* Parser rules */

start: unused grammar terminals nonterminals statelst EOF_TOKEN { ($1, $2, $3, List.rev $4, List.rev $5); }

unused:
	conflst { [] }
    |	TERMINALS ID ID ID unlst conflst { List.rev $5; }

grammar:
	GRAMMAR gramitmlst { List.mapi (fun ix (n,lft,rght) -> assert (ix=n); GRAMITM(lft,rght)) (List.rev $2) }

terminals:
	TERMINALS COMMA ID ID ID ID ID termlst { List.rev $8 }

nonterminals:
	NONTERMINALS COMMA ID ID ID ID ID nontermlst { $8 } 

conflst:
	/*empty*/ { [ ] }
    | conflst confitm { $2 :: $1 }

confitm:
    |   STATE NUMBER ID COLON NUMBER ID SLASH ID { STATEITM(NUMBER $2,[]) }

statelst:
	stateitm { [ $1 ] }
    | statelst stateitm { $2 :: $1 }

stateitm:
    |   STATE NUMBER silst { STATEITM(NUMBER $2,List.rev $3) }

silst:
	sitm { [ $1 ] }
    | silst sitm { $2 :: $1 }

sitm:
    ID { ID $1 }
    | dolitm { $1 }
    | spunct { $1 }
    | quotitm { $1 }
    | NUMBER { NUMBER $1 }
    ;

spunct:
| AMPERSAND { AMPERSAND }
| AT { AT }
| BACKQUOTE { BACKQUOTE }
| BACKSLASH { BACKSLASH }
| CARET { CARET }
| COLON { COLON }
| COMMA { COMMA }
| DOLLAR { DOLLAR }
| DOT { DOT }
| DOUBLEQUOTE { DOUBLEQUOTE }
| EQUALS { EQUALS }
| GREATER { GREATER }
| HASH { HASH }
| HYPHEN { HYPHEN }
| LBRACE { LBRACE }
| LBRACK { LBRACK }
| LESS { LESS }
| LPAREN { LPAREN }
| PERCENT { PERCENT }
| PLING { PLING }
| PLUS { PLUS }
| QUERY { QUERY }
| RBRACE { RBRACE }
| RBRACK { RBRACK }
| RPAREN { RPAREN }
| SEMICOLON { SEMICOLON }
| SLASH { SLASH }
| STAR { STAR }
| TILDE { TILDE }
| UNDERSCORE { UNDERSCORE }
| VBAR { VBAR }

nontermlst:
	nontermitm { [ $1 ] }
    | nontermlst nontermitm { $2 :: $1 }

nontermitm:
    |   dolitm LPAREN NUMBER RPAREN ntlst { NONTERMITM($1,$3) }
    |   ID LPAREN NUMBER RPAREN ntlst { NONTERMITM(ID $1,$3) }
    |   dolat LPAREN NUMBER RPAREN ntlst { NONTERMITM($1,$3) }

ntlst:
	ntitm { [ $1 ] }
    | ntlst COMMA ntitm { $3 :: $1 }

ntitm:
	ID ID COLON numlst { ID $1 }

termlst:
	termitm { [ $1 ] }
    | termlst termitm { $2 :: $1 }

termitm:
	dolitm LPAREN NUMBER RPAREN numlst { TERMITM($1,$3) }
    |   quotitm LPAREN NUMBER RPAREN numlst { TERMITM($1,$3) }
    |   ID LPAREN NUMBER RPAREN numlst { TERMITM(ID (String.uppercase $1),$3) }

quotitm:
	QUOTE BACKSLASH ID QUOTE
	      { match $3 with "n" -> LINEFEED | _ -> CHAR ($3.[0]) }
    |   QUOTE punct QUOTE { $2 }

numlst:
    | { [ ] }
    | NUMBER numlst { $1 :: $2 }

unlst:
	ID { [ $1 ] }
    | unlst ID { $2 :: $1 }

gramitmlst:
	gramitm { [ $1 ] }
    | gramitmlst gramitm { $2 :: $1 }

gramitm:
	NUMBER dolitm COLON rulst { ($1, $2, List.rev $4) }
    |   NUMBER ID COLON rulst { ($1, ID $2, List.rev $4) }
    |   NUMBER dolat COLON rulst { ($1, $2, List.rev $4) }
    |   NUMBER VBAR rulst { ($1, VBAR, List.rev $3) }

dolitm:
	DOLLAR ID { dollar $2 }

dolat:
	DOLLAR AT NUMBER { DOLLAR_AT $3 }
    |   AT NUMBER { DOLLAR_AT $2 }

rulst:
    | ru { [ $1 ] }
    | rulst ru { $2 :: $1 }
    
ru:
    ID { ID $1 }
    | dolitm { $1 }
    | PERCENT ID { match $2 with "empty" -> EMPTY | oth -> ID oth }
    | QUOTE punct QUOTE { $2 }
    | dolat { $1 }
    | quotitm { $1 }
    ;

punct:
| AMPERSAND { AMPERSAND }
| AT { AT }
| BACKQUOTE { BACKQUOTE }
| BACKSLASH { BACKSLASH }
| CARET { CARET }
| COLON { COLON }
| COMMA { COMMA }
| DOLLAR { DOLLAR }
| DOT { DOT }
| DOUBLEQUOTE { DOUBLEQUOTE }
| EQUALS { EQUALS }
| GREATER { GREATER }
| HASH { HASH }
| HYPHEN { HYPHEN }
| LBRACE { LBRACE }
| LBRACK { LBRACK }
| LESS { LESS }
| LPAREN { LPAREN }
| PERCENT { PERCENT }
| PLING { PLING }
| PLUS { PLUS }
| QUERY { QUERY }
| QUOTE { QUOTE }
| RBRACE { RBRACE }
| RBRACK { RBRACK }
| RPAREN { RPAREN }
| SEMICOLON { SEMICOLON }
| SLASH { SLASH }
| STAR { STAR }
| TILDE { TILDE }
| UNDERSCORE { UNDERSCORE }
| VBAR { VBAR }
