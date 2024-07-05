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

  let unquot = function
  | ACCEPT -> ("ACCEPT") 
  | AMPERSAND -> ("AMPERSAND") 
  | AT -> ("AT") 
  | BACKQUOTE -> ("BACKQUOTE") 
  | BACKSLASH -> ("BACKSLASH") 
  | CARET -> ("CARET") 
  | CHAR ch -> String.make 1 ch
  | COLON -> ("COLON") 
  | COMMA -> ("COMMA") 
  | DEFAULT -> ("DEFAULT") 
  | DOLLAR -> ("DLR") 
  | DOLLAR_AT _-> ("DOLLAR_AT") 
  | DOT -> ("DOT") 
  | DOUBLEQUOTE -> ("DOUBLEQUOTE") 
  | EMPTY -> ("EMPTY") 
  | END -> ("END") 
  | EOF_TOKEN -> ("EOF_TOKEN") 
  | EQUALS -> ("EQ") 
  | GRAMITM _-> ("GRAMITM") 
  | GRAMMAR -> ("GRAMMAR") 
  | GREATER -> ("GT") 
  | HASH -> ("HASH") 
  | HYPHEN -> ("HYPHEN") 
  | ID id -> id
  | LBRACE -> ("LBRACE") 
  | LBRACK -> ("LBRACK") 
  | LESS -> ("LT") 
  | LINEFEED -> ("LINEFEED") 
  | LPAREN -> ("LPAREN") 
  | NONTERMINALS -> ("NONTERMINALS") 
  | NONTERMITM _-> ("NONTERMITM") 
  | NUMBER _-> ("NUMBER") 
  | PERCENT -> ("PERCENT") 
  | PLING -> ("PLING") 
  | PLUS -> ("PLUS") 
  | QUERY -> ("QUERY") 
  | QUOTE -> ("QUOTE") 
  | RBRACE -> ("RBRACE") 
  | RBRACK -> ("RBRACK") 
  | RPAREN -> ("RPAREN") 
  | SEMICOLON -> ("SEMICOLON") 
  | SLASH -> ("SLASH") 
  | STAR -> ("STAR") 
  | STATE -> ("STATE") 
  | STATEITM _-> ("STATEITM") 
  | STRING_LITERAL _-> ("STRING_LITERAL") 
  | TERMINALS -> ("TERMINALS") 
  | TERMITM _-> ("TERMITM") 
  | TERMS _-> ("TERMS") 
  | TILDE -> ("TILDE") 
  | TLIST _-> ("TLIST") 
  | UNDERSCORE -> ("UNDERSCORE") 
  | UNUSED _-> ("UNUSED") 
  | VBAR -> ("VBAR") 

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
%token <string>   STRING_LITERAL;
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

start: unused grammar terminals NONTERMINALS { ($1, $2, $3, [], []); }

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

nontermlst:
	nontermitm { [ $1 ] }
    | nontermlst nontermitm { $2 :: $1 }

nontermitm:
    |   dolitm LPAREN NUMBER RPAREN ntlst { NONTERMITM($1,$3) }
    |   ID LPAREN NUMBER RPAREN ntlst { NONTERMITM(ID $1,$3) }
    |   ID LESS ID GREATER LPAREN NUMBER RPAREN ntlst { NONTERMITM(ID $1,$6) }
    |   dolat LPAREN NUMBER RPAREN ntlst { NONTERMITM($1,$3) }

ntlst:
	ntitm { [ $1 ] }
    | ntlst ntitm { $2 :: $1 }

ntitm:
	ID ID COLON numlst { ID $1 }

termlst:
	termitm { [ $1 ] }
    | termlst termitm { $2 :: $1 }

termitm:
	dolitm LPAREN NUMBER RPAREN numlst { TERMITM($1,$3) }
    |   quotitm LPAREN NUMBER RPAREN numlst { TERMITM($1,$3) }
    |   dquotitm LPAREN NUMBER RPAREN numlst { TERMITM($1,$3) }
    |   ID LPAREN NUMBER RPAREN numlst { TERMITM(ID ( (*String.uppercase_ascii*) $1),$3) }
    |   ID LESS ID GREATER LPAREN NUMBER RPAREN numlst { TERMITM(ID ( (*String.uppercase_ascii*) $1),$6) }

quotitm:
	QUOTE BACKSLASH QUOTE QUOTE { QUOTE }
    |	QUOTE BACKSLASH ID QUOTE
	      { match $3 with "n" -> LINEFEED | _ -> CHAR ($3.[0]) }
    |	QUOTE ID QUOTE
	      { CHAR ($2.[0]) }
    |	QUOTE NUMBER QUOTE
	      { CHAR ( char_of_int ($2 + int_of_char '0') ) }
    |   QUOTE punct QUOTE { $2 }

dquotitm:
	DOUBLEQUOTE dqlst DOUBLEQUOTE
	      { STRING_LITERAL (String.concat ";" (List.rev (List.map unquot $2))) }
    |   DOUBLEQUOTE punct QUOTE { $2 }

numlst:
    | { [ ] }
    | NUMBER numlst { $1 :: $2 }

unlst:
	ID { [ $1 ] }
    | dquotitm { [ "\"" ] }
    | unlst ID { $2 :: $1 }
    | unlst dquotitm { "\"" :: $1 }

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
    | dquotitm { $1 }
    | dolat { $1 }
    | quotitm { $1 }
    | SLASH STAR ID STAR SLASH { match $3 with "empty" -> EMPTY | oth -> ID oth }
    ;

dqlst:
    | ID { [ ID $1 ] }
    | NUMBER { [ NUMBER $1 ] }
    | punct { [ $1 ] }
    | dqlst ID { ID $2 :: $1 }
    | dqlst punct { $2 :: $1 }

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
