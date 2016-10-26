%{
  open Parsing
  open Program_types
%}

%token  ACCEPT
%token  ALIGNOF
%token  AMPERSAND
%token  ANDAND
%token  ARITHCOMPARE
%token  ASM_KEYWORD
%token  ASSIGN
%token  AT
%token  ATTRIBUTE
%token  AT_ALIAS
%token  AT_CATCH
%token  AT_CLASS
%token  AT_DEFS
%token  AT_ENCODE
%token  AT_END
%token  AT_FINALLY
%token  AT_IMPLEMENTATION
%token  AT_INTERFACE
%token  AT_PRIVATE
%token  AT_PROTECTED
%token  AT_PROTOCOL
%token  AT_PUBLIC
%token  AT_SELECTOR
%token  AT_SYNCHRONIZED
%token  AT_THROW
%token  AT_TRY
%token  BACKQUOTE
%token  BACKSLASH
%token  BREAK
%token  CARET
%token  CASE
%token  CHOOSE_EXPR
%token  CLASSNAME
%token  COLON
%token  COMMA
%token <float> CONSTANT
%token  CONTINUE
%token  DEFAULT
%token  DO
%token  DOLLAR
%token  DOT
%token  DOUBLEQUOTE
%token  ELLIPSIS
%token  ELSE
%token  EMPTY_TOKEN
%token  END
%token  ENUM
%token  EOF_TOKEN
%token  EQCOMPARE
%token  EQUALS
%token  ERROR
%token  ERROR_TOKEN
%token  EXTENSION
%token  FOR
%token  FUNC_NAME
%token  GOTO
%token  GREATER
%token  HASH
%token  HYPERUNARY
%token  HYPHEN
%token <string> IDENTIFIER
%token  IF
%token  IMAGPART
%token <int> INT
%token  LABEL
%token  LBRACE
%token  LBRACK
%token  LESS
%token  LINEFEED
%token  LPAREN
%token  LSHIFT
%token  MINUSMINUS
%token  OBJC_STRING
%token  OBJC_TYPE_QUAL
%token  OFFSETOF
%token  OROR
%token  PERCENT
%token  PLING
%token  PLUS
%token  PLUSPLUS
%token  POINTSAT
%token  QUERY
%token  QUOTE
%token  RBRACE
%token  RBRACK
%token  REALPART
%token  RETURN
%token  RPAREN
%token  RSHIFT
%token  SCSPEC
%token  SEMICOLON
%token  SIZEOF
%token  SLASH
%token <string list> SLIST
%token  STAR
%token  STATIC
%token <string> STRING
%token  STRUCT
%token  SWITCH
%token  TILDE
%token <token list> TLIST
%token <token> TUPLE1
%token <token*token*token*token*token*token*token*token*token*token> TUPLE10
%token <token*token*token*token*token*token*token*token*token*token*token> TUPLE11
%token <token*token*token*token*token*token*token*token*token*token*token*token> TUPLE12
%token <token*token*token*token*token*token*token*token*token*token*token*token*token> TUPLE13
%token <token*token> TUPLE2
%token <token*token*token> TUPLE3
%token <token*token*token*token> TUPLE4
%token <token*token*token*token*token> TUPLE5
%token <token*token*token*token*token*token> TUPLE6
%token <token*token*token*token*token*token*token> TUPLE7
%token <token*token*token*token*token*token*token*token> TUPLE8
%token <token*token*token*token*token*token*token*token*token> TUPLE9
%token  TYPENAME
%token  TYPEOF
%token  TYPESPEC
%token  TYPES_COMPATIBLE_P
%token  TYPE_QUAL
%token  UNARY
%token  UNDERSCORE
%token  UNION
%token  VA_ARG
%token  VBAR
%token  WHILE
%type <token> ml_start
%start ml_start
%%


ml_start: program EOF_TOKEN { TUPLE2($1,EOF_TOKEN) }

program: /* empty */ { EMPTY_TOKEN }
	|	extdefs { TUPLE1($1) }

extdefs: save_obstack_position /* 1 */ extdef { TUPLE2($1,$2) }
	|	extdefs save_obstack_position /* 2 */ extdef { TUPLE3($1,$2,$3) }

extdef: fndef { TUPLE1($1) }
	|	datadef { TUPLE1($1) }
	|	asmdef { TUPLE1($1) }
	|	extension extdef { TUPLE2($1,$2) }

save_obstack_position: /* empty */ { EMPTY_TOKEN }

datadef: setspecs notype_initdecls SEMICOLON { TUPLE3($1,$2,SEMICOLON) }
	|	declspecs_nots setspecs notype_initdecls SEMICOLON { TUPLE4($1,$2,$3,SEMICOLON) }
	|	declspecs_ts setspecs initdecls SEMICOLON { TUPLE4($1,$2,$3,SEMICOLON) }
	|	declspecs SEMICOLON { TUPLE2($1,SEMICOLON) }
	|	ERROR_TOKEN SEMICOLON { TUPLE2(ERROR_TOKEN,SEMICOLON) }
	|	ERROR_TOKEN RBRACE { TUPLE2(ERROR_TOKEN,RBRACE) }
	|	SEMICOLON { TUPLE1(SEMICOLON) }

fndef: declspecs_ts setspecs declarator /* 3 */ old_style_parm_decls save_location /* 4 */ compstmt_or_error { TUPLE6($1,$2,$3,$4,$5,$6) }
	|	declspecs_ts setspecs declarator ERROR_TOKEN { TUPLE4($1,$2,$3,ERROR_TOKEN) }
	|	declspecs_nots setspecs notype_declarator /* 5 */ old_style_parm_decls save_location /* 6 */ compstmt_or_error { TUPLE6($1,$2,$3,$4,$5,$6) }
	|	declspecs_nots setspecs notype_declarator ERROR_TOKEN { TUPLE4($1,$2,$3,ERROR_TOKEN) }
	|	setspecs notype_declarator /* 7 */ old_style_parm_decls save_location /* 8 */ compstmt_or_error { TUPLE5($1,$2,$3,$4,$5) }
	|	setspecs notype_declarator ERROR_TOKEN { TUPLE3($1,$2,ERROR_TOKEN) }

identifier: IDENTIFIER { TUPLE1(IDENTIFIER $1) }
	|	TYPENAME { TUPLE1(TYPENAME) }

unop: AMPERSAND { TUPLE1(AMPERSAND) }
	|	HYPHEN { TUPLE1(HYPHEN) }
	|	PLUS { TUPLE1(PLUS) }
	|	PLUSPLUS { TUPLE1(PLUSPLUS) }
	|	MINUSMINUS { TUPLE1(MINUSMINUS) }
	|	TILDE { TUPLE1(TILDE) }
	|	PLING { TUPLE1(PLING) }

expr: expr_no_commas { TUPLE1($1) }
	|	expr COMMA expr_no_commas { TUPLE3($1,COMMA,$3) }

exprlist: /* empty */ { EMPTY_TOKEN }
	|	nonnull_exprlist { TUPLE1($1) }

nonnull_exprlist: expr_no_commas { TUPLE1($1) }
	|	nonnull_exprlist COMMA expr_no_commas { TUPLE3($1,COMMA,$3) }

unary_expr: primary { TUPLE1($1) }
	|	STAR cast_expr { TUPLE2(STAR,$2) }
	|	extension cast_expr { TUPLE2($1,$2) }
	|	unop cast_expr { TUPLE2($1,$2) }
	|	ANDAND identifier { TUPLE2(ANDAND,$2) }
	|	sizeof unary_expr { TUPLE2($1,$2) }
	|	sizeof LPAREN typename RPAREN { TUPLE4($1,LPAREN,$3,RPAREN) }
	|	alignof unary_expr { TUPLE2($1,$2) }
	|	alignof LPAREN typename RPAREN { TUPLE4($1,LPAREN,$3,RPAREN) }
	|	REALPART cast_expr { TUPLE2(REALPART,$2) }
	|	IMAGPART cast_expr { TUPLE2(IMAGPART,$2) }

sizeof: SIZEOF { TUPLE1(SIZEOF) }

alignof: ALIGNOF { TUPLE1(ALIGNOF) }

typeof: TYPEOF { TUPLE1(TYPEOF) }

cast_expr: unary_expr { TUPLE1($1) }
	|	LPAREN typename RPAREN cast_expr { TUPLE4(LPAREN,$2,RPAREN,$4) }

expr_no_commas: cast_expr { TUPLE1($1) }
	|	expr_no_commas PLUS expr_no_commas { TUPLE3($1,PLUS,$3) }
	|	expr_no_commas HYPHEN expr_no_commas { TUPLE3($1,HYPHEN,$3) }
	|	expr_no_commas STAR expr_no_commas { TUPLE3($1,STAR,$3) }
	|	expr_no_commas SLASH expr_no_commas { TUPLE3($1,SLASH,$3) }
	|	expr_no_commas PERCENT expr_no_commas { TUPLE3($1,PERCENT,$3) }
	|	expr_no_commas LSHIFT expr_no_commas { TUPLE3($1,LSHIFT,$3) }
	|	expr_no_commas RSHIFT expr_no_commas { TUPLE3($1,RSHIFT,$3) }
	|	expr_no_commas ARITHCOMPARE expr_no_commas { TUPLE3($1,ARITHCOMPARE,$3) }
	|	expr_no_commas EQCOMPARE expr_no_commas { TUPLE3($1,EQCOMPARE,$3) }
	|	expr_no_commas AMPERSAND expr_no_commas { TUPLE3($1,AMPERSAND,$3) }
	|	expr_no_commas VBAR expr_no_commas { TUPLE3($1,VBAR,$3) }
	|	expr_no_commas CARET expr_no_commas { TUPLE3($1,CARET,$3) }
	|	expr_no_commas ANDAND /* 9 */ expr_no_commas { TUPLE3($1,ANDAND,$3) }
	|	expr_no_commas OROR /* 10 */ expr_no_commas { TUPLE3($1,OROR,$3) }
	|	expr_no_commas QUERY /* 11 */ expr COLON /* 12 */ expr_no_commas { TUPLE5($1,QUERY,$3,COLON,$5) }
	|	expr_no_commas QUERY /* 13 */ COLON expr_no_commas { TUPLE4($1,QUERY,COLON,$4) }
	|	expr_no_commas EQUALS expr_no_commas { TUPLE3($1,EQUALS,$3) }
	|	expr_no_commas ASSIGN expr_no_commas { TUPLE3($1,ASSIGN,$3) }

primary: IDENTIFIER { TUPLE1(IDENTIFIER $1) }
	|	CONSTANT { TUPLE1(CONSTANT $1) }
	|	STRING { TUPLE1(STRING $1) }
	|	FUNC_NAME { TUPLE1(FUNC_NAME) }
	|	LPAREN typename RPAREN LBRACE /* 14 */ initlist_maybe_comma RBRACE { TUPLE6(LPAREN,$2,RPAREN,LBRACE,$5,RBRACE) }
	|	LPAREN expr RPAREN { TUPLE3(LPAREN,$2,RPAREN) }
	|	LPAREN ERROR_TOKEN RPAREN { TUPLE3(LPAREN,ERROR_TOKEN,RPAREN) }
	|	compstmt_primary_start compstmt_nostart RPAREN { TUPLE3($1,$2,RPAREN) }
	|	compstmt_primary_start ERROR_TOKEN RPAREN { TUPLE3($1,ERROR_TOKEN,RPAREN) }
	|	primary LPAREN exprlist RPAREN { TUPLE4($1,LPAREN,$3,RPAREN) }
	|	VA_ARG LPAREN expr_no_commas COMMA typename RPAREN { TUPLE6(VA_ARG,LPAREN,$3,COMMA,$5,RPAREN) }
	|	OFFSETOF LPAREN typename COMMA /* 15 */ offsetof_member_designator RPAREN { TUPLE6(OFFSETOF,LPAREN,$3,COMMA,$5,RPAREN) }
	|	OFFSETOF LPAREN ERROR_TOKEN RPAREN { TUPLE4(OFFSETOF,LPAREN,ERROR_TOKEN,RPAREN) }
	|	CHOOSE_EXPR LPAREN expr_no_commas COMMA expr_no_commas COMMA expr_no_commas RPAREN { TUPLE8(CHOOSE_EXPR,LPAREN,$3,COMMA,$5,COMMA,$7,RPAREN) }
	|	CHOOSE_EXPR LPAREN ERROR_TOKEN RPAREN { TUPLE4(CHOOSE_EXPR,LPAREN,ERROR_TOKEN,RPAREN) }
	|	TYPES_COMPATIBLE_P LPAREN typename COMMA typename RPAREN { TUPLE6(TYPES_COMPATIBLE_P,LPAREN,$3,COMMA,$5,RPAREN) }
	|	TYPES_COMPATIBLE_P LPAREN ERROR_TOKEN RPAREN { TUPLE4(TYPES_COMPATIBLE_P,LPAREN,ERROR_TOKEN,RPAREN) }
	|	primary LBRACK expr RBRACK { TUPLE4($1,LBRACK,$3,RBRACK) }
	|	primary DOT identifier { TUPLE3($1,DOT,$3) }
	|	primary POINTSAT identifier { TUPLE3($1,POINTSAT,$3) }
	|	primary PLUSPLUS { TUPLE2($1,PLUSPLUS) }
	|	primary MINUSMINUS { TUPLE2($1,MINUSMINUS) }

offsetof_member_designator: identifier { TUPLE1($1) }
	|	offsetof_member_designator DOT identifier { TUPLE3($1,DOT,$3) }
	|	offsetof_member_designator LBRACK expr RBRACK { TUPLE4($1,LBRACK,$3,RBRACK) }

old_style_parm_decls: /* empty */ { EMPTY_TOKEN }
	|	datadecls { TUPLE1($1) }

lineno_datadecl: save_location datadecl { TUPLE2($1,$2) }

datadecls: lineno_datadecl { TUPLE1($1) }
	|	errstmt { TUPLE1($1) }
	|	datadecls lineno_datadecl { TUPLE2($1,$2) }
	|	lineno_datadecl errstmt { TUPLE2($1,$2) }

datadecl: declspecs_ts_nosa setspecs initdecls SEMICOLON { TUPLE4($1,$2,$3,SEMICOLON) }
	|	declspecs_nots_nosa setspecs notype_initdecls SEMICOLON { TUPLE4($1,$2,$3,SEMICOLON) }
	|	declspecs_ts_nosa SEMICOLON { TUPLE2($1,SEMICOLON) }
	|	declspecs_nots_nosa SEMICOLON { TUPLE2($1,SEMICOLON) }

lineno_decl: save_location decl { TUPLE2($1,$2) }

setspecs: /* empty */ { EMPTY_TOKEN }

maybe_resetattrs: maybe_attribute { TUPLE1($1) }

decl: declspecs_ts setspecs initdecls SEMICOLON { TUPLE4($1,$2,$3,SEMICOLON) }
	|	declspecs_nots setspecs notype_initdecls SEMICOLON { TUPLE4($1,$2,$3,SEMICOLON) }
	|	declspecs_ts setspecs nested_function { TUPLE3($1,$2,$3) }
	|	declspecs_nots setspecs notype_nested_function { TUPLE3($1,$2,$3) }
	|	declspecs SEMICOLON { TUPLE2($1,SEMICOLON) }
	|	extension decl { TUPLE2($1,$2) }

declspecs_nosc_nots_nosa_noea: TYPE_QUAL { TUPLE1(TYPE_QUAL) }
	|	declspecs_nosc_nots_nosa_noea TYPE_QUAL { TUPLE2($1,TYPE_QUAL) }
	|	declspecs_nosc_nots_nosa_ea TYPE_QUAL { TUPLE2($1,TYPE_QUAL) }

declspecs_nosc_nots_nosa_ea: declspecs_nosc_nots_nosa_noea attributes { TUPLE2($1,$2) }

declspecs_nosc_nots_sa_noea: declspecs_nosc_nots_sa_noea TYPE_QUAL { TUPLE2($1,TYPE_QUAL) }
	|	declspecs_nosc_nots_sa_ea TYPE_QUAL { TUPLE2($1,TYPE_QUAL) }

declspecs_nosc_nots_sa_ea: attributes { TUPLE1($1) }
	|	declspecs_nosc_nots_sa_noea attributes { TUPLE2($1,$2) }

declspecs_nosc_ts_nosa_noea: typespec_nonattr { TUPLE1($1) }
	|	declspecs_nosc_ts_nosa_noea TYPE_QUAL { TUPLE2($1,TYPE_QUAL) }
	|	declspecs_nosc_ts_nosa_ea TYPE_QUAL { TUPLE2($1,TYPE_QUAL) }
	|	declspecs_nosc_ts_nosa_noea typespec_reserved_nonattr { TUPLE2($1,$2) }
	|	declspecs_nosc_ts_nosa_ea typespec_reserved_nonattr { TUPLE2($1,$2) }
	|	declspecs_nosc_nots_nosa_noea typespec_nonattr { TUPLE2($1,$2) }
	|	declspecs_nosc_nots_nosa_ea typespec_nonattr { TUPLE2($1,$2) }

declspecs_nosc_ts_nosa_ea: typespec_attr { TUPLE1($1) }
	|	declspecs_nosc_ts_nosa_noea attributes { TUPLE2($1,$2) }
	|	declspecs_nosc_ts_nosa_noea typespec_reserved_attr { TUPLE2($1,$2) }
	|	declspecs_nosc_ts_nosa_ea typespec_reserved_attr { TUPLE2($1,$2) }
	|	declspecs_nosc_nots_nosa_noea typespec_attr { TUPLE2($1,$2) }
	|	declspecs_nosc_nots_nosa_ea typespec_attr { TUPLE2($1,$2) }

declspecs_nosc_ts_sa_noea: declspecs_nosc_ts_sa_noea TYPE_QUAL { TUPLE2($1,TYPE_QUAL) }
	|	declspecs_nosc_ts_sa_ea TYPE_QUAL { TUPLE2($1,TYPE_QUAL) }
	|	declspecs_nosc_ts_sa_noea typespec_reserved_nonattr { TUPLE2($1,$2) }
	|	declspecs_nosc_ts_sa_ea typespec_reserved_nonattr { TUPLE2($1,$2) }
	|	declspecs_nosc_nots_sa_noea typespec_nonattr { TUPLE2($1,$2) }
	|	declspecs_nosc_nots_sa_ea typespec_nonattr { TUPLE2($1,$2) }

declspecs_nosc_ts_sa_ea: declspecs_nosc_ts_sa_noea attributes { TUPLE2($1,$2) }
	|	declspecs_nosc_ts_sa_noea typespec_reserved_attr { TUPLE2($1,$2) }
	|	declspecs_nosc_ts_sa_ea typespec_reserved_attr { TUPLE2($1,$2) }
	|	declspecs_nosc_nots_sa_noea typespec_attr { TUPLE2($1,$2) }
	|	declspecs_nosc_nots_sa_ea typespec_attr { TUPLE2($1,$2) }

declspecs_sc_nots_nosa_noea: scspec { TUPLE1($1) }
	|	declspecs_sc_nots_nosa_noea TYPE_QUAL { TUPLE2($1,TYPE_QUAL) }
	|	declspecs_sc_nots_nosa_ea TYPE_QUAL { TUPLE2($1,TYPE_QUAL) }
	|	declspecs_nosc_nots_nosa_noea scspec { TUPLE2($1,$2) }
	|	declspecs_nosc_nots_nosa_ea scspec { TUPLE2($1,$2) }
	|	declspecs_sc_nots_nosa_noea scspec { TUPLE2($1,$2) }
	|	declspecs_sc_nots_nosa_ea scspec { TUPLE2($1,$2) }

declspecs_sc_nots_nosa_ea: declspecs_sc_nots_nosa_noea attributes { TUPLE2($1,$2) }

declspecs_sc_nots_sa_noea: declspecs_sc_nots_sa_noea TYPE_QUAL { TUPLE2($1,TYPE_QUAL) }
	|	declspecs_sc_nots_sa_ea TYPE_QUAL { TUPLE2($1,TYPE_QUAL) }
	|	declspecs_nosc_nots_sa_noea scspec { TUPLE2($1,$2) }
	|	declspecs_nosc_nots_sa_ea scspec { TUPLE2($1,$2) }
	|	declspecs_sc_nots_sa_noea scspec { TUPLE2($1,$2) }
	|	declspecs_sc_nots_sa_ea scspec { TUPLE2($1,$2) }

declspecs_sc_nots_sa_ea: declspecs_sc_nots_sa_noea attributes { TUPLE2($1,$2) }

declspecs_sc_ts_nosa_noea: declspecs_sc_ts_nosa_noea TYPE_QUAL { TUPLE2($1,TYPE_QUAL) }
	|	declspecs_sc_ts_nosa_ea TYPE_QUAL { TUPLE2($1,TYPE_QUAL) }
	|	declspecs_sc_ts_nosa_noea typespec_reserved_nonattr { TUPLE2($1,$2) }
	|	declspecs_sc_ts_nosa_ea typespec_reserved_nonattr { TUPLE2($1,$2) }
	|	declspecs_sc_nots_nosa_noea typespec_nonattr { TUPLE2($1,$2) }
	|	declspecs_sc_nots_nosa_ea typespec_nonattr { TUPLE2($1,$2) }
	|	declspecs_nosc_ts_nosa_noea scspec { TUPLE2($1,$2) }
	|	declspecs_nosc_ts_nosa_ea scspec { TUPLE2($1,$2) }
	|	declspecs_sc_ts_nosa_noea scspec { TUPLE2($1,$2) }
	|	declspecs_sc_ts_nosa_ea scspec { TUPLE2($1,$2) }

declspecs_sc_ts_nosa_ea: declspecs_sc_ts_nosa_noea attributes { TUPLE2($1,$2) }
	|	declspecs_sc_ts_nosa_noea typespec_reserved_attr { TUPLE2($1,$2) }
	|	declspecs_sc_ts_nosa_ea typespec_reserved_attr { TUPLE2($1,$2) }
	|	declspecs_sc_nots_nosa_noea typespec_attr { TUPLE2($1,$2) }
	|	declspecs_sc_nots_nosa_ea typespec_attr { TUPLE2($1,$2) }

declspecs_sc_ts_sa_noea: declspecs_sc_ts_sa_noea TYPE_QUAL { TUPLE2($1,TYPE_QUAL) }
	|	declspecs_sc_ts_sa_ea TYPE_QUAL { TUPLE2($1,TYPE_QUAL) }
	|	declspecs_sc_ts_sa_noea typespec_reserved_nonattr { TUPLE2($1,$2) }
	|	declspecs_sc_ts_sa_ea typespec_reserved_nonattr { TUPLE2($1,$2) }
	|	declspecs_sc_nots_sa_noea typespec_nonattr { TUPLE2($1,$2) }
	|	declspecs_sc_nots_sa_ea typespec_nonattr { TUPLE2($1,$2) }
	|	declspecs_nosc_ts_sa_noea scspec { TUPLE2($1,$2) }
	|	declspecs_nosc_ts_sa_ea scspec { TUPLE2($1,$2) }
	|	declspecs_sc_ts_sa_noea scspec { TUPLE2($1,$2) }
	|	declspecs_sc_ts_sa_ea scspec { TUPLE2($1,$2) }

declspecs_sc_ts_sa_ea: declspecs_sc_ts_sa_noea attributes { TUPLE2($1,$2) }
	|	declspecs_sc_ts_sa_noea typespec_reserved_attr { TUPLE2($1,$2) }
	|	declspecs_sc_ts_sa_ea typespec_reserved_attr { TUPLE2($1,$2) }
	|	declspecs_sc_nots_sa_noea typespec_attr { TUPLE2($1,$2) }
	|	declspecs_sc_nots_sa_ea typespec_attr { TUPLE2($1,$2) }

declspecs_ts: declspecs_nosc_ts_nosa_noea { TUPLE1($1) }
	|	declspecs_nosc_ts_nosa_ea { TUPLE1($1) }
	|	declspecs_nosc_ts_sa_noea { TUPLE1($1) }
	|	declspecs_nosc_ts_sa_ea { TUPLE1($1) }
	|	declspecs_sc_ts_nosa_noea { TUPLE1($1) }
	|	declspecs_sc_ts_nosa_ea { TUPLE1($1) }
	|	declspecs_sc_ts_sa_noea { TUPLE1($1) }
	|	declspecs_sc_ts_sa_ea { TUPLE1($1) }

declspecs_nots: declspecs_nosc_nots_nosa_noea { TUPLE1($1) }
	|	declspecs_nosc_nots_nosa_ea { TUPLE1($1) }
	|	declspecs_nosc_nots_sa_noea { TUPLE1($1) }
	|	declspecs_nosc_nots_sa_ea { TUPLE1($1) }
	|	declspecs_sc_nots_nosa_noea { TUPLE1($1) }
	|	declspecs_sc_nots_nosa_ea { TUPLE1($1) }
	|	declspecs_sc_nots_sa_noea { TUPLE1($1) }
	|	declspecs_sc_nots_sa_ea { TUPLE1($1) }

declspecs_ts_nosa: declspecs_nosc_ts_nosa_noea { TUPLE1($1) }
	|	declspecs_nosc_ts_nosa_ea { TUPLE1($1) }
	|	declspecs_sc_ts_nosa_noea { TUPLE1($1) }
	|	declspecs_sc_ts_nosa_ea { TUPLE1($1) }

declspecs_nots_nosa: declspecs_nosc_nots_nosa_noea { TUPLE1($1) }
	|	declspecs_nosc_nots_nosa_ea { TUPLE1($1) }
	|	declspecs_sc_nots_nosa_noea { TUPLE1($1) }
	|	declspecs_sc_nots_nosa_ea { TUPLE1($1) }

declspecs_nosc_ts: declspecs_nosc_ts_nosa_noea { TUPLE1($1) }
	|	declspecs_nosc_ts_nosa_ea { TUPLE1($1) }
	|	declspecs_nosc_ts_sa_noea { TUPLE1($1) }
	|	declspecs_nosc_ts_sa_ea { TUPLE1($1) }

declspecs_nosc_nots: declspecs_nosc_nots_nosa_noea { TUPLE1($1) }
	|	declspecs_nosc_nots_nosa_ea { TUPLE1($1) }
	|	declspecs_nosc_nots_sa_noea { TUPLE1($1) }
	|	declspecs_nosc_nots_sa_ea { TUPLE1($1) }

declspecs_nosc: declspecs_nosc_ts_nosa_noea { TUPLE1($1) }
	|	declspecs_nosc_ts_nosa_ea { TUPLE1($1) }
	|	declspecs_nosc_ts_sa_noea { TUPLE1($1) }
	|	declspecs_nosc_ts_sa_ea { TUPLE1($1) }
	|	declspecs_nosc_nots_nosa_noea { TUPLE1($1) }
	|	declspecs_nosc_nots_nosa_ea { TUPLE1($1) }
	|	declspecs_nosc_nots_sa_noea { TUPLE1($1) }
	|	declspecs_nosc_nots_sa_ea { TUPLE1($1) }

declspecs: declspecs_nosc_nots_nosa_noea { TUPLE1($1) }
	|	declspecs_nosc_nots_nosa_ea { TUPLE1($1) }
	|	declspecs_nosc_nots_sa_noea { TUPLE1($1) }
	|	declspecs_nosc_nots_sa_ea { TUPLE1($1) }
	|	declspecs_nosc_ts_nosa_noea { TUPLE1($1) }
	|	declspecs_nosc_ts_nosa_ea { TUPLE1($1) }
	|	declspecs_nosc_ts_sa_noea { TUPLE1($1) }
	|	declspecs_nosc_ts_sa_ea { TUPLE1($1) }
	|	declspecs_sc_nots_nosa_noea { TUPLE1($1) }
	|	declspecs_sc_nots_nosa_ea { TUPLE1($1) }
	|	declspecs_sc_nots_sa_noea { TUPLE1($1) }
	|	declspecs_sc_nots_sa_ea { TUPLE1($1) }
	|	declspecs_sc_ts_nosa_noea { TUPLE1($1) }
	|	declspecs_sc_ts_nosa_ea { TUPLE1($1) }
	|	declspecs_sc_ts_sa_noea { TUPLE1($1) }
	|	declspecs_sc_ts_sa_ea { TUPLE1($1) }

maybe_type_quals_attrs: /* empty */ { EMPTY_TOKEN }
	|	declspecs_nosc_nots { TUPLE1($1) }

typespec_nonattr: typespec_reserved_nonattr { TUPLE1($1) }
	|	typespec_nonreserved_nonattr { TUPLE1($1) }

typespec_attr: typespec_reserved_attr { TUPLE1($1) }

typespec_reserved_nonattr: TYPESPEC { TUPLE1(TYPESPEC) }
	|	structsp_nonattr { TUPLE1($1) }

typespec_reserved_attr: structsp_attr { TUPLE1($1) }

typespec_nonreserved_nonattr: TYPENAME { TUPLE1(TYPENAME) }
	|	typeof LPAREN expr RPAREN { TUPLE4($1,LPAREN,$3,RPAREN) }
	|	typeof LPAREN typename RPAREN { TUPLE4($1,LPAREN,$3,RPAREN) }

initdecls: initdcl { TUPLE1($1) }
	|	initdecls COMMA maybe_resetattrs initdcl { TUPLE4($1,COMMA,$3,$4) }

notype_initdecls: notype_initdcl { TUPLE1($1) }
	|	notype_initdecls COMMA maybe_resetattrs notype_initdcl { TUPLE4($1,COMMA,$3,$4) }

initdcl: declarator maybeasm maybe_attribute EQUALS /* 16 */ init { TUPLE5($1,$2,$3,EQUALS,$5) }
	|	declarator maybeasm maybe_attribute { TUPLE3($1,$2,$3) }

notype_initdcl: notype_declarator maybeasm maybe_attribute EQUALS /* 17 */ init { TUPLE5($1,$2,$3,EQUALS,$5) }
	|	notype_declarator maybeasm maybe_attribute { TUPLE3($1,$2,$3) }

maybe_attribute: /* empty */ { EMPTY_TOKEN }
	|	attributes { TUPLE1($1) }

attributes: attribute { TUPLE1($1) }
	|	attributes attribute { TUPLE2($1,$2) }

attribute: ATTRIBUTE stop_string_translation LPAREN LPAREN attribute_list RPAREN RPAREN start_string_translation { TUPLE8(ATTRIBUTE,$2,LPAREN,LPAREN,$5,RPAREN,RPAREN,$8) }
	|	ATTRIBUTE ERROR_TOKEN start_string_translation { TUPLE3(ATTRIBUTE,ERROR_TOKEN,$3) }

attribute_list: attrib { TUPLE1($1) }
	|	attribute_list COMMA attrib { TUPLE3($1,COMMA,$3) }

attrib: /* empty */ { EMPTY_TOKEN }
	|	any_word { TUPLE1($1) }
	|	any_word LPAREN IDENTIFIER RPAREN { TUPLE4($1,LPAREN,IDENTIFIER $3,RPAREN) }
	|	any_word LPAREN IDENTIFIER COMMA nonnull_exprlist RPAREN { TUPLE6($1,LPAREN,IDENTIFIER $3,COMMA,$5,RPAREN) }
	|	any_word LPAREN exprlist RPAREN { TUPLE4($1,LPAREN,$3,RPAREN) }

any_word: identifier { TUPLE1($1) }
	|	scspec { TUPLE1($1) }
	|	TYPESPEC { TUPLE1(TYPESPEC) }
	|	TYPE_QUAL { TUPLE1(TYPE_QUAL) }

scspec: STATIC { TUPLE1(STATIC) }
	|	SCSPEC { TUPLE1(SCSPEC) }

init: expr_no_commas { TUPLE1($1) }
	|	LBRACE /* 18 */ initlist_maybe_comma RBRACE { TUPLE3(LBRACE,$2,RBRACE) }
	|	ERROR_TOKEN { TUPLE1(ERROR_TOKEN) }

initlist_maybe_comma: /* empty */ { EMPTY_TOKEN }
	|	initlist1 maybecomma { TUPLE2($1,$2) }

initlist1: initelt { TUPLE1($1) }
	|	initlist1 COMMA initelt { TUPLE3($1,COMMA,$3) }

initelt: designator_list EQUALS initval { TUPLE3($1,EQUALS,$3) }
	|	array_designator initval { TUPLE2($1,$2) }
	|	identifier COLON /* 19 */ initval { TUPLE3($1,COLON,$3) }
	|	initval { TUPLE1($1) }

initval: LBRACE /* 20 */ initlist_maybe_comma RBRACE { TUPLE3(LBRACE,$2,RBRACE) }
	|	expr_no_commas { TUPLE1($1) }
	|	ERROR_TOKEN { TUPLE1(ERROR_TOKEN) }

designator_list: designator { TUPLE1($1) }
	|	designator_list designator { TUPLE2($1,$2) }

designator: DOT identifier { TUPLE2(DOT,$2) }
	|	array_designator { TUPLE1($1) }

array_designator: LBRACK expr_no_commas ELLIPSIS expr_no_commas RBRACK { TUPLE5(LBRACK,$2,ELLIPSIS,$4,RBRACK) }
	|	LBRACK expr_no_commas RBRACK { TUPLE3(LBRACK,$2,RBRACK) }

nested_function: declarator /* 21 */ old_style_parm_decls save_location /* 22 */ bounds_compstmt { TUPLE4($1,$2,$3,$4) }

notype_nested_function: notype_declarator /* 23 */ old_style_parm_decls save_location /* 24 */ bounds_compstmt { TUPLE4($1,$2,$3,$4) }

declarator: after_type_declarator { TUPLE1($1) }
	|	notype_declarator { TUPLE1($1) }

after_type_declarator: LPAREN maybe_attribute after_type_declarator RPAREN { TUPLE4(LPAREN,$2,$3,RPAREN) }
	|	after_type_declarator LPAREN parmlist_or_identifiers { TUPLE3($1,LPAREN,$3) }
	|	after_type_declarator array_declarator { TUPLE2($1,$2) }
	|	STAR maybe_type_quals_attrs after_type_declarator { TUPLE3(STAR,$2,$3) }
	|	TYPENAME { TUPLE1(TYPENAME) }

parm_declarator: parm_declarator_starttypename { TUPLE1($1) }
	|	parm_declarator_nostarttypename { TUPLE1($1) }

parm_declarator_starttypename: parm_declarator_starttypename LPAREN parmlist_or_identifiers { TUPLE3($1,LPAREN,$3) }
	|	parm_declarator_starttypename array_declarator { TUPLE2($1,$2) }
	|	TYPENAME { TUPLE1(TYPENAME) }

parm_declarator_nostarttypename: parm_declarator_nostarttypename LPAREN parmlist_or_identifiers { TUPLE3($1,LPAREN,$3) }
	|	parm_declarator_nostarttypename array_declarator { TUPLE2($1,$2) }
	|	STAR maybe_type_quals_attrs parm_declarator_starttypename { TUPLE3(STAR,$2,$3) }
	|	STAR maybe_type_quals_attrs parm_declarator_nostarttypename { TUPLE3(STAR,$2,$3) }
	|	LPAREN maybe_attribute parm_declarator_nostarttypename RPAREN { TUPLE4(LPAREN,$2,$3,RPAREN) }

notype_declarator: notype_declarator LPAREN parmlist_or_identifiers { TUPLE3($1,LPAREN,$3) }
	|	LPAREN maybe_attribute notype_declarator RPAREN { TUPLE4(LPAREN,$2,$3,RPAREN) }
	|	STAR maybe_type_quals_attrs notype_declarator { TUPLE3(STAR,$2,$3) }
	|	notype_declarator array_declarator { TUPLE2($1,$2) }
	|	IDENTIFIER { TUPLE1(IDENTIFIER $1) }

struct_head: STRUCT { TUPLE1(STRUCT) }
	|	STRUCT attributes { TUPLE2(STRUCT,$2) }

union_head: UNION { TUPLE1(UNION) }
	|	UNION attributes { TUPLE2(UNION,$2) }

enum_head: ENUM { TUPLE1(ENUM) }
	|	ENUM attributes { TUPLE2(ENUM,$2) }

structsp_attr: struct_head identifier LBRACE /* 25 */ component_decl_list RBRACE maybe_attribute { TUPLE6($1,$2,LBRACE,$4,RBRACE,$6) }
	|	struct_head LBRACE component_decl_list RBRACE maybe_attribute { TUPLE5($1,LBRACE,$3,RBRACE,$5) }
	|	union_head identifier LBRACE /* 26 */ component_decl_list RBRACE maybe_attribute { TUPLE6($1,$2,LBRACE,$4,RBRACE,$6) }
	|	union_head LBRACE component_decl_list RBRACE maybe_attribute { TUPLE5($1,LBRACE,$3,RBRACE,$5) }
	|	enum_head identifier LBRACE /* 27 */ enumlist maybecomma_warn RBRACE maybe_attribute { TUPLE7($1,$2,LBRACE,$4,$5,RBRACE,$7) }
	|	enum_head LBRACE /* 28 */ enumlist maybecomma_warn RBRACE maybe_attribute { TUPLE6($1,LBRACE,$3,$4,RBRACE,$6) }

structsp_nonattr: struct_head identifier { TUPLE2($1,$2) }
	|	union_head identifier { TUPLE2($1,$2) }
	|	enum_head identifier { TUPLE2($1,$2) }

maybecomma: /* empty */ { EMPTY_TOKEN }
	|	COMMA { TUPLE1(COMMA) }

maybecomma_warn: /* empty */ { EMPTY_TOKEN }
	|	COMMA { TUPLE1(COMMA) }

component_decl_list: component_decl_list2 { TUPLE1($1) }
	|	component_decl_list2 component_decl { TUPLE2($1,$2) }

component_decl_list2: /* empty */ { EMPTY_TOKEN }
	|	component_decl_list2 component_decl SEMICOLON { TUPLE3($1,$2,SEMICOLON) }
	|	component_decl_list2 SEMICOLON { TUPLE2($1,SEMICOLON) }

component_decl: declspecs_nosc_ts setspecs components { TUPLE3($1,$2,$3) }
	|	declspecs_nosc_ts setspecs { TUPLE2($1,$2) }
	|	declspecs_nosc_nots setspecs components_notype { TUPLE3($1,$2,$3) }
	|	declspecs_nosc_nots { TUPLE1($1) }
	|	ERROR_TOKEN { TUPLE1(ERROR_TOKEN) }
	|	extension component_decl { TUPLE2($1,$2) }

components: component_declarator { TUPLE1($1) }
	|	components COMMA maybe_resetattrs component_declarator { TUPLE4($1,COMMA,$3,$4) }

components_notype: component_notype_declarator { TUPLE1($1) }
	|	components_notype COMMA maybe_resetattrs component_notype_declarator { TUPLE4($1,COMMA,$3,$4) }

component_declarator: declarator maybe_attribute { TUPLE2($1,$2) }
	|	declarator COLON expr_no_commas maybe_attribute { TUPLE4($1,COLON,$3,$4) }
	|	COLON expr_no_commas maybe_attribute { TUPLE3(COLON,$2,$3) }

component_notype_declarator: notype_declarator maybe_attribute { TUPLE2($1,$2) }
	|	notype_declarator COLON expr_no_commas maybe_attribute { TUPLE4($1,COLON,$3,$4) }
	|	COLON expr_no_commas maybe_attribute { TUPLE3(COLON,$2,$3) }

enumlist: enumerator { TUPLE1($1) }
	|	enumlist COMMA enumerator { TUPLE3($1,COMMA,$3) }
	|	ERROR_TOKEN { TUPLE1(ERROR_TOKEN) }

enumerator: identifier { TUPLE1($1) }
	|	identifier EQUALS expr_no_commas { TUPLE3($1,EQUALS,$3) }

typename: declspecs_nosc /* 29 */ absdcl { TUPLE2($1,$2) }

absdcl: /* empty */ { EMPTY_TOKEN }
	|	absdcl1 { TUPLE1($1) }

absdcl_maybe_attribute: /* empty */ { EMPTY_TOKEN }
	|	absdcl1 { TUPLE1($1) }
	|	absdcl1_noea attributes { TUPLE2($1,$2) }

absdcl1: absdcl1_ea { TUPLE1($1) }
	|	absdcl1_noea { TUPLE1($1) }

absdcl1_noea: direct_absdcl1 { TUPLE1($1) }
	|	STAR maybe_type_quals_attrs absdcl1_noea { TUPLE3(STAR,$2,$3) }

absdcl1_ea: STAR maybe_type_quals_attrs { TUPLE2(STAR,$2) }
	|	STAR maybe_type_quals_attrs absdcl1_ea { TUPLE3(STAR,$2,$3) }

direct_absdcl1: LPAREN maybe_attribute absdcl1 RPAREN { TUPLE4(LPAREN,$2,$3,RPAREN) }
	|	direct_absdcl1 LPAREN parmlist { TUPLE3($1,LPAREN,$3) }
	|	direct_absdcl1 array_declarator { TUPLE2($1,$2) }
	|	LPAREN parmlist { TUPLE2(LPAREN,$2) }
	|	array_declarator { TUPLE1($1) }

array_declarator: LBRACK maybe_type_quals_attrs expr_no_commas RBRACK { TUPLE4(LBRACK,$2,$3,RBRACK) }
	|	LBRACK maybe_type_quals_attrs RBRACK { TUPLE3(LBRACK,$2,RBRACK) }
	|	LBRACK maybe_type_quals_attrs STAR RBRACK { TUPLE4(LBRACK,$2,STAR,RBRACK) }
	|	LBRACK STATIC maybe_type_quals_attrs expr_no_commas RBRACK { TUPLE5(LBRACK,STATIC,$3,$4,RBRACK) }
	|	LBRACK declspecs_nosc_nots STATIC expr_no_commas RBRACK { TUPLE5(LBRACK,$2,STATIC,$4,RBRACK) }

stmts_and_decls: lineno_stmt_decl_or_labels_ending_stmt { TUPLE1($1) }
	|	lineno_stmt_decl_or_labels_ending_decl { TUPLE1($1) }
	|	lineno_stmt_decl_or_labels_ending_label { TUPLE1($1) }
	|	lineno_stmt_decl_or_labels_ending_error { TUPLE1($1) }

lineno_stmt_decl_or_labels_ending_stmt: lineno_stmt { TUPLE1($1) }
	|	lineno_stmt_decl_or_labels_ending_stmt lineno_stmt { TUPLE2($1,$2) }
	|	lineno_stmt_decl_or_labels_ending_decl lineno_stmt { TUPLE2($1,$2) }
	|	lineno_stmt_decl_or_labels_ending_label lineno_stmt { TUPLE2($1,$2) }
	|	lineno_stmt_decl_or_labels_ending_error lineno_stmt { TUPLE2($1,$2) }

lineno_stmt_decl_or_labels_ending_decl: lineno_decl { TUPLE1($1) }
	|	lineno_stmt_decl_or_labels_ending_stmt lineno_decl { TUPLE2($1,$2) }
	|	lineno_stmt_decl_or_labels_ending_decl lineno_decl { TUPLE2($1,$2) }
	|	lineno_stmt_decl_or_labels_ending_error lineno_decl { TUPLE2($1,$2) }

lineno_stmt_decl_or_labels_ending_label: lineno_label { TUPLE1($1) }
	|	lineno_stmt_decl_or_labels_ending_stmt lineno_label { TUPLE2($1,$2) }
	|	lineno_stmt_decl_or_labels_ending_decl lineno_label { TUPLE2($1,$2) }
	|	lineno_stmt_decl_or_labels_ending_label lineno_label { TUPLE2($1,$2) }
	|	lineno_stmt_decl_or_labels_ending_error lineno_label { TUPLE2($1,$2) }

lineno_stmt_decl_or_labels_ending_error: errstmt { TUPLE1($1) }
	|	lineno_stmt_decl_or_labels errstmt { TUPLE2($1,$2) }

lineno_stmt_decl_or_labels: lineno_stmt_decl_or_labels_ending_stmt { TUPLE1($1) }
	|	lineno_stmt_decl_or_labels_ending_decl { TUPLE1($1) }
	|	lineno_stmt_decl_or_labels_ending_label { TUPLE1($1) }
	|	lineno_stmt_decl_or_labels_ending_error { TUPLE1($1) }

errstmt: ERROR_TOKEN SEMICOLON { TUPLE2(ERROR_TOKEN,SEMICOLON) }

c99_block_start: /* empty */ { EMPTY_TOKEN }

maybe_label_decls: /* empty */ { EMPTY_TOKEN }
	|	label_decls { TUPLE1($1) }

label_decls: label_decl { TUPLE1($1) }
	|	label_decls label_decl { TUPLE2($1,$2) }

label_decl: LABEL identifiers_or_typenames SEMICOLON { TUPLE3(LABEL,$2,SEMICOLON) }

compstmt_or_error: bounds_compstmt { TUPLE1($1) }
	|	ERROR_TOKEN bounds_compstmt { TUPLE2(ERROR_TOKEN,$2) }

bounds_compstmt_start: /* empty */ { EMPTY_TOKEN }

bounds_compstmt: bounds_compstmt_start compstmt { TUPLE2($1,$2) }

compstmt_start: LBRACE { TUPLE1(LBRACE) }

compstmt_nostart: RBRACE { TUPLE1(RBRACE) }
	|	maybe_label_decls compstmt_contents_nonempty RBRACE { TUPLE3($1,$2,RBRACE) }

compstmt_contents_nonempty: stmts_and_decls { TUPLE1($1) }
	|	ERROR_TOKEN { TUPLE1(ERROR_TOKEN) }

compstmt_primary_start: LPAREN LBRACE { TUPLE2(LPAREN,LBRACE) }

compstmt: compstmt_start compstmt_nostart { TUPLE2($1,$2) }

save_location: /* empty */ { EMPTY_TOKEN }

lineno_labels: /* empty */ { EMPTY_TOKEN }
	|	lineno_labels lineno_label { TUPLE2($1,$2) }

c99_block_lineno_labeled_stmt: c99_block_start lineno_labels lineno_stmt { TUPLE3($1,$2,$3) }

lineno_stmt: save_location stmt { TUPLE2($1,$2) }

lineno_label: save_location label { TUPLE2($1,$2) }

condition: save_location expr { TUPLE2($1,$2) }

if_statement_1: c99_block_start lineno_labels if_statement { TUPLE3($1,$2,$3) }

if_statement_2: c99_block_start lineno_labels SEMICOLON { TUPLE3($1,$2,SEMICOLON) }
	|	c99_block_lineno_labeled_stmt { TUPLE1($1) }

if_statement: IF c99_block_start save_location LPAREN condition RPAREN if_statement_1 ELSE if_statement_2 { TUPLE9(IF,$2,$3,LPAREN,$5,RPAREN,$7,ELSE,$9) }
	|	IF c99_block_start save_location LPAREN condition RPAREN if_statement_2 ELSE if_statement_2 { TUPLE9(IF,$2,$3,LPAREN,$5,RPAREN,$7,ELSE,$9) }
	|	IF c99_block_start save_location LPAREN condition RPAREN if_statement_1 { TUPLE7(IF,$2,$3,LPAREN,$5,RPAREN,$7) }
	|	IF c99_block_start save_location LPAREN condition RPAREN if_statement_2 { TUPLE7(IF,$2,$3,LPAREN,$5,RPAREN,$7) }

start_break: /* empty */ { EMPTY_TOKEN }

start_continue: /* empty */ { EMPTY_TOKEN }

while_statement: WHILE c99_block_start save_location LPAREN condition RPAREN start_break start_continue c99_block_lineno_labeled_stmt { TUPLE9(WHILE,$2,$3,LPAREN,$5,RPAREN,$7,$8,$9) }

do_statement: DO c99_block_start save_location start_break start_continue c99_block_lineno_labeled_stmt WHILE /* 30 */ /* 31 */ LPAREN condition RPAREN SEMICOLON { TUPLE11(DO,$2,$3,$4,$5,$6,WHILE,LPAREN,$9,RPAREN,SEMICOLON) }

xexpr: /* empty */ { EMPTY_TOKEN }
	|	expr { TUPLE1($1) }

for_init_stmt: xexpr SEMICOLON { TUPLE2($1,SEMICOLON) }
	|	decl { TUPLE1($1) }

for_cond_expr: save_location xexpr { TUPLE2($1,$2) }

for_incr_expr: xexpr { TUPLE1($1) }

for_statement: FOR c99_block_start LPAREN for_init_stmt save_location for_cond_expr SEMICOLON for_incr_expr RPAREN start_break start_continue c99_block_lineno_labeled_stmt { TUPLE12(FOR,$2,LPAREN,$4,$5,$6,SEMICOLON,$8,RPAREN,$10,$11,$12) }

switch_statement: SWITCH c99_block_start LPAREN expr RPAREN /* 32 */ start_break c99_block_lineno_labeled_stmt { TUPLE7(SWITCH,$2,LPAREN,$4,RPAREN,$6,$7) }

stmt_nocomp: expr SEMICOLON { TUPLE2($1,SEMICOLON) }
	|	if_statement { TUPLE1($1) }
	|	while_statement { TUPLE1($1) }
	|	do_statement { TUPLE1($1) }
	|	for_statement { TUPLE1($1) }
	|	switch_statement { TUPLE1($1) }
	|	BREAK SEMICOLON { TUPLE2(BREAK,SEMICOLON) }
	|	CONTINUE SEMICOLON { TUPLE2(CONTINUE,SEMICOLON) }
	|	RETURN SEMICOLON { TUPLE2(RETURN,SEMICOLON) }
	|	RETURN expr SEMICOLON { TUPLE3(RETURN,$2,SEMICOLON) }
	|	asm_stmt { TUPLE1($1) }
	|	GOTO identifier SEMICOLON { TUPLE3(GOTO,$2,SEMICOLON) }
	|	GOTO STAR expr SEMICOLON { TUPLE4(GOTO,STAR,$3,SEMICOLON) }
	|	SEMICOLON { TUPLE1(SEMICOLON) }

stmt: compstmt { TUPLE1($1) }
	|	stmt_nocomp { TUPLE1($1) }

label: CASE expr_no_commas COLON { TUPLE3(CASE,$2,COLON) }
	|	CASE expr_no_commas ELLIPSIS expr_no_commas COLON { TUPLE5(CASE,$2,ELLIPSIS,$4,COLON) }
	|	DEFAULT COLON { TUPLE2(DEFAULT,COLON) }
	|	identifier save_location COLON maybe_attribute { TUPLE4($1,$2,COLON,$4) }

simple_asm_expr: ASM_KEYWORD stop_string_translation LPAREN asm_string RPAREN start_string_translation { TUPLE6(ASM_KEYWORD,$2,LPAREN,$4,RPAREN,$6) }

maybeasm: /* empty */ { EMPTY_TOKEN }
	|	simple_asm_expr { TUPLE1($1) }

asmdef: simple_asm_expr SEMICOLON { TUPLE2($1,SEMICOLON) }
	|	ASM_KEYWORD ERROR_TOKEN start_string_translation SEMICOLON { TUPLE4(ASM_KEYWORD,ERROR_TOKEN,$3,SEMICOLON) }

asm_stmt: ASM_KEYWORD maybe_volatile stop_string_translation LPAREN asm_argument RPAREN start_string_translation SEMICOLON { TUPLE8(ASM_KEYWORD,$2,$3,LPAREN,$5,RPAREN,$7,SEMICOLON) }

asm_argument: asm_string { TUPLE1($1) }
	|	asm_string COLON asm_operands { TUPLE3($1,COLON,$3) }
	|	asm_string COLON asm_operands COLON asm_operands { TUPLE5($1,COLON,$3,COLON,$5) }
	|	asm_string COLON asm_operands COLON asm_operands COLON asm_clobbers { TUPLE7($1,COLON,$3,COLON,$5,COLON,$7) }

maybe_volatile: /* empty */ { EMPTY_TOKEN }
	|	TYPE_QUAL { TUPLE1(TYPE_QUAL) }

asm_operands: /* empty */ { EMPTY_TOKEN }
	|	nonnull_asm_operands { TUPLE1($1) }

nonnull_asm_operands: asm_operand { TUPLE1($1) }
	|	nonnull_asm_operands COMMA asm_operand { TUPLE3($1,COMMA,$3) }

asm_operand: asm_string start_string_translation LPAREN expr RPAREN stop_string_translation { TUPLE6($1,$2,LPAREN,$4,RPAREN,$6) }
	|	LBRACK identifier RBRACK asm_string start_string_translation LPAREN expr RPAREN stop_string_translation { TUPLE9(LBRACK,$2,RBRACK,$4,$5,LPAREN,$7,RPAREN,$9) }

asm_clobbers: asm_string { TUPLE1($1) }
	|	asm_clobbers COMMA asm_string { TUPLE3($1,COMMA,$3) }

asm_string: STRING { TUPLE1(STRING $1) }

stop_string_translation: /* empty */ { EMPTY_TOKEN }

start_string_translation: /* empty */ { EMPTY_TOKEN }

parmlist: maybe_attribute /* 33 */ parmlist_1 { TUPLE2($1,$2) }

parmlist_1: parmlist_2 RPAREN { TUPLE2($1,RPAREN) }
	|	parms SEMICOLON /* 34 */ maybe_attribute /* 35 */ parmlist_1 { TUPLE4($1,SEMICOLON,$3,$4) }
	|	ERROR_TOKEN RPAREN { TUPLE2(ERROR_TOKEN,RPAREN) }

parmlist_2: /* empty */ { EMPTY_TOKEN }
	|	ELLIPSIS { TUPLE1(ELLIPSIS) }
	|	parms { TUPLE1($1) }
	|	parms COMMA ELLIPSIS { TUPLE3($1,COMMA,ELLIPSIS) }

parms: firstparm { TUPLE1($1) }
	|	parms COMMA parm { TUPLE3($1,COMMA,$3) }

parm: declspecs_ts setspecs parm_declarator maybe_attribute { TUPLE4($1,$2,$3,$4) }
	|	declspecs_ts setspecs notype_declarator maybe_attribute { TUPLE4($1,$2,$3,$4) }
	|	declspecs_ts setspecs absdcl_maybe_attribute { TUPLE3($1,$2,$3) }
	|	declspecs_nots setspecs notype_declarator maybe_attribute { TUPLE4($1,$2,$3,$4) }
	|	declspecs_nots setspecs absdcl_maybe_attribute { TUPLE3($1,$2,$3) }

firstparm: declspecs_ts_nosa setspecs_fp parm_declarator maybe_attribute { TUPLE4($1,$2,$3,$4) }
	|	declspecs_ts_nosa setspecs_fp notype_declarator maybe_attribute { TUPLE4($1,$2,$3,$4) }
	|	declspecs_ts_nosa setspecs_fp absdcl_maybe_attribute { TUPLE3($1,$2,$3) }
	|	declspecs_nots_nosa setspecs_fp notype_declarator maybe_attribute { TUPLE4($1,$2,$3,$4) }
	|	declspecs_nots_nosa setspecs_fp absdcl_maybe_attribute { TUPLE3($1,$2,$3) }

setspecs_fp: setspecs { TUPLE1($1) }

parmlist_or_identifiers: maybe_attribute /* 36 */ parmlist_or_identifiers_1 { TUPLE2($1,$2) }

parmlist_or_identifiers_1: parmlist_1 { TUPLE1($1) }
	|	identifiers RPAREN { TUPLE2($1,RPAREN) }

identifiers: IDENTIFIER { TUPLE1(IDENTIFIER $1) }
	|	identifiers COMMA IDENTIFIER { TUPLE3($1,COMMA,IDENTIFIER $3) }

identifiers_or_typenames: identifier { TUPLE1($1) }
	|	identifiers_or_typenames COMMA identifier { TUPLE3($1,COMMA,$3) }

extension: EXTENSION { TUPLE1(EXTENSION) }


