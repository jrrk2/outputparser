%{
  open Parsing
%}

%token  ACCEPT
%token  AMPERSAND
%token  AT
%token  EQUALS
%token  BACKQUOTE
%token  BACKSLASH
%token  CARET
%token  COLON
%token  COMMA
%token  HYPHEN
%token  PLUS
%token  LPAREN
%token  RPAREN
%token  STAR
%token  SLASH
%token  SEMICOLON
%token <token> CONS1
%token <token*token> CONS2
%token <token*token*token> CONS3
%token <token*token*token*token> CONS4
%token <string> TOK_COMMENT
%token <string> TOK_STRING
%token <string> TOK_VALUE
%token <string> TOK_ID
%token <int> TOK_OTH
%token <int> TOK_INT
%token  DEFAULT
%token  DOLLAR
%token  DOT
%token  EOL
%token  DOUBLEQUOTE
%token <token list> ELIST
%token  EMPTY_TOKEN
%token  END
%token  EOF_TOKEN
%token  ERROR
%token  ERROR_TOKEN
%token  GREATER
%token  HASH
%token  LBRACE
%token  LBRACK
%token  LESS
%token  LINEFEED
%token  PERCENT
%token  PLING
%token  QUERY
%token  QUOTE
%token  RBRACE
%token  RBRACK
%token <string list> SLIST
%token <string> STRING
%token  TILDE
%token <token list> TLIST
%token <token*token*token*token*token*token*token*token*token*token> TUPLE10
%token <token*token> TUPLE2
%token <token*token*token> TUPLE3
%token <token*token*token*token> TUPLE4
%token <token*token*token*token*token> TUPLE5
%token <token*token*token*token*token*token> TUPLE6
%token <token*token*token*token*token*token*token> TUPLE7
%token <token*token*token*token*token*token*token*token> TUPLE8
%token <token*token*token*token*token*token*token*token*token> TUPLE9
%token  UNDERSCORE
%token  VBAR

%type <token> ml_start id
%start ml_start
%%

ml_start: LBRACK id paren0 RBRACK EOF_TOKEN { TUPLE2 ($2,$3) }

id : TOK_ID { TOK_ID $1 }

int : TOK_INT { TOK_INT $1 }

str: TOK_STRING { TOK_STRING $1 }

oth: TOK_OTH { TOK_OTH $1 }

hash: HASH int { $2 }

paren0: LPAREN paren_lst1 RPAREN COMMA { TLIST (List.rev $2) }

paren_lst1: { [ ] }
  | paren_lst1 paren1 { $2 :: $1 }

paren1: id COMMA { $1 }
  | int { $1 }
  | int paren0 { TUPLE2($1,$2) }
  | hash id brace0 { TUPLE2($2,$3) }
  | id hash id brace0 { TUPLE3($1,$3,$4) }
  | id hash id paren0 { TUPLE3($1,$3,$4) }
  | id paren0 { TUPLE2($1,$2) }
  | id brace0 { TUPLE2($1,$2) }
  | oth COMMA { $1 }

brack0: LBRACK brack_lst1 RBRACK COMMA { TLIST (List.rev $2) }

brack_lst1: { [ ] }
  | brack_lst1 brack1 { $2 :: $1 }

brack1: id COLON LBRACK brack_lst1 RBRACK { TUPLE2($1,TLIST (List.rev $4)) }
  | id hash id paren0 { TUPLE3($1,$3,$4) }
  | id paren0 { TUPLE2($1,$2) }
  | id hash id brace0 { TUPLE3($1,$3,$4) }

brace0: LBRACE brace_lst1 RBRACE COMMA { TLIST (List.rev $2) }

brace_lst1: { [ ] }
  | brace_lst1 brace2 { $2 :: $1 }

brace2: id COLON id brace0 { TUPLE3($1,COLON,TUPLE2($3,$4)) }
  | id COLON id COMMA { TUPLE3($1,COLON,$3) }
  | id COLON brack0 { TUPLE3($1,COLON,$3) }
  | id COLON id paren0 { TUPLE3($1,COLON,TUPLE2($3,$4)) }
  | id COLON id hash id brace0 { TUPLE3($1,COLON,TUPLE3($3,$5,$6)) }
  | id COLON id hash id paren0 { TUPLE3($1,COLON,TUPLE3($3,$5,$6)) }
  | id COLON id hash id COMMA { TUPLE3($1,COLON,TUPLE2($3,$5)) }
  | id COLON id LPAREN int RPAREN COMMA { TUPLE3($1,COLON,TUPLE2($3,TLIST [$5])) }
  | id COLON id LPAREN int SEMICOLON str RPAREN COLON int HYPHEN int COMMA { TUPLE3($1,COLON,TUPLE5($3,$5,$7,$10,$12)) }
