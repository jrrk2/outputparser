%{
  open Parsing
  open CompilationUnit_types
  let declst = ref []
  let typehash_add id_t = Hashtbl.add typehash id_t ()
%}

%token  ABSTRACT
%token  ACCEPT
%token  AMPERSAND
%token  AT
%token  BACKQUOTE
%token  BACKSLASH
%token  CARET
%token  CASE
%token  CATCH
%token  CHARACTERLITERAL
%token  CLASS
%token  COLON
%token  COMMA
%token <token> CONS1
%token <token*token> CONS2
%token <token*token*token> CONS3
%token <token*token*token*token> CONS4
%token  DEF
%token  DEFAULT
%token  DO
%token  DOLLAR
%token  DOT
%token  DOUBLEQUOTE
%token  ELSE
%token  EMPTY_TOKEN
%token  END
%token  EOF_TOKEN
%token  EQGT
%token  EQUALS
%token  ERROR
%token  ERROR_TOKEN
%token  EXTENDS
%token  FALSE
%token  FINAL
%token  FINALLY
%token  FLOATINGPOINTLITERAL
%token  FOR
%token  FOR_SOME
%token  GREATER
%token  HASH
%token  HYPHEN
%token  IF
%token  IMPLICIT
%token  IMPORT
%token  INTEGERLITERAL
%token  LAZY
%token  LBRACE
%token  LBRACK
%token  LESS
%token  LINEFEED
%token  LPAREN
%token  MATCH
%token  NEW
%token  NEWLINE
%token  NULL
%token  OBJECT
%token  OVERRIDE
%token  PACKAGE
%token  PARAMTYPE
%token  PARAMTYPECOMMALST
%token  PERCENT
%token  PLAINID
%token  PLING
%token  PLUS
%token  PRIVATE
%token  PROTECTED
%token  QUERY
%token  QUOTE
%token  RBRACE
%token  RBRACK
%token  RETURN
%token  RPAREN
%token  SEALED
%token  SEMICOLON
%token <string list> SLIST
%token  STAR
%token  STRINGLITERAL
%token  SUPER
%token  THIS
%token  THROW
%token  TILDE
%token <token list> TLIST
%token  TRAIT
%token  TRUE
%token  TRY
%token <token*token> TUPLE2
%token <token*token*token> TUPLE3
%token <token*token*token*token> TUPLE4
%token <token*token*token*token*token> TUPLE5
%token <token*token*token*token*token*token> TUPLE6
%token <token*token*token*token*token*token*token> TUPLE7
%token  TYPE
%token  UNDERSCORE
%token  VAL
%token  VAR
%token  VARID
%token  VBAR
%token  WHILE
%token  WITH
%token  XMLEXPR
%token  XMLPATTERN
%token  YIELD
%type <token> ml_start
%start ml_start
%%


ml_start: CompilationUnit EOF_TOKEN { TUPLE2($1,EOF_TOKEN) }

id: plainid { ($1) }
	|	BACKQUOTE stringLiteral BACKQUOTE { TUPLE3(BACKQUOTE,$2,BACKQUOTE) }

ids: id { ($1) }

QualId: id { ($1) }

StableId: id { ($1) }
	|	Path DOT id { TUPLE3($1,DOT,$3) }
	|	idopt SUPER DOT id { TUPLE4($1,SUPER,DOT,$4) }

idopt: /* empty */ { EMPTY_TOKEN }
	|	id DOT { TUPLE2($1,DOT) }

Path: StableId { ($1) }
	|	idopt THIS { TUPLE2($1,THIS) }

booleanLiteral: TRUE { (TRUE) }
	|	FALSE { (FALSE) }

symbolLiteral: QUOTE plainid { TUPLE2(QUOTE,$2) }

Literal: integerLiteral { ($1) }
	|	HYPHEN integerLiteral { TUPLE2(HYPHEN,$2) }
	|	floatingPointLiteral { ($1) }
	|	HYPHEN floatingPointLiteral { TUPLE2(HYPHEN,$2) }
	|	booleanLiteral { ($1) }
	|	characterLiteral { ($1) }
	|	stringLiteral { ($1) }
	|	symbolLiteral { ($1) }
	|	NULL { (NULL) }

SimpleType: StableId { ($1) }
	|	Path DOT TYPE { TUPLE3($1,DOT,TYPE) }
	|	SimpleType TypeArgs { TUPLE2($1,$2) }
	|	SimpleType HASH id { TUPLE3($1,HASH,$3) }
	|	LPAREN Types RPAREN { TUPLE3(LPAREN,$2,RPAREN) }

AnnotType: SimpleType { ($1) }

CompoundType: AnnotType { ($1) }
	|	Refinement { ($1) }

InfixType: CompoundType { ($1) }

ParamType: Type { ($1) }
	|	EQGT Type { TUPLE2(EQGT,$2) }
	|	Type STAR { TUPLE2($1,STAR) }

ParamTypeCommaLstOpt: /* empty */ { EMPTY_TOKEN }
	|	ParamtypeCommaLst { ($1) }

FunctionArgTypes: InfixType { ($1) }
	|	LPAREN ParamTypeCommaLstOpt RPAREN { TUPLE3(LPAREN,$2,RPAREN) }

Type: InfixType { ($1) }
	|	FunctionArgTypes EQGT Type { TUPLE3($1,EQGT,$3) }

Types: Type { ($1) }

TypeArgs: LBRACK Types RBRACK { TUPLE3(LBRACK,$2,RBRACK) }

Ascription: COLON InfixType { TUPLE2(COLON,$2) }
	|	COLON Annotation { TUPLE2(COLON,$2) }
	|	COLON UNDERSCORE STAR { TUPLE3(COLON,UNDERSCORE,STAR) }

Binding: id_ { ($1) }
	|	id_ COLON Type { TUPLE3($1,COLON,$3) }

id_: id { ($1) }
	|	UNDERSCORE { (UNDERSCORE) }

Bindings: LPAREN Binding /* 1 */ RPAREN { TUPLE3(LPAREN,$2,RPAREN) }

TypePat: Type { ($1) }

SimplePattern: UNDERSCORE { (UNDERSCORE) }
	|	varid { ($1) }
	|	Literal { ($1) }
	|	StableId { ($1) }
	|	StableId LPAREN RPAREN { TUPLE3($1,LPAREN,RPAREN) }
	|	StableId LPAREN PatternCommaOpt varidat UNDERSCORE STAR RPAREN { TUPLE7($1,LPAREN,$3,$4,UNDERSCORE,STAR,RPAREN) }
	|	LPAREN RPAREN { TUPLE2(LPAREN,RPAREN) }
	|	XmlPattern { ($1) }

PatternCommaOpt: /* empty */ { EMPTY_TOKEN }
	|	Patterns COMMA { TUPLE2($1,COMMA) }

varidat: /* empty */ { EMPTY_TOKEN }
	|	varid AT { TUPLE2($1,AT) }

Pattern3: SimplePattern { ($1) }
	|	SimplePattern { ($1) }

Pattern2: Pattern3 { ($1) }
	|	varid atpattern3 { TUPLE2($1,$2) }

atpattern3: /* empty */ { EMPTY_TOKEN }
	|	AT Pattern3 { TUPLE2(AT,$2) }

cmpatterns: /* empty */ { EMPTY_TOKEN }
	|	COMMA Patterns { TUPLE2(COMMA,$2) }

Pattern1: Pattern2 { ($1) }
	|	varid COLON TypePat { TUPLE3($1,COLON,$3) }
	|	UNDERSCORE COLON TypePat { TUPLE3(UNDERSCORE,COLON,$3) }

Pattern: Pattern1 { ($1) }

Patterns: Pattern cmpatterns { TUPLE2($1,$2) }
	|	UNDERSCORE STAR { TUPLE2(UNDERSCORE,STAR) }

CaseClause: CASE Pattern EQGT Block { TUPLE4(CASE,$2,EQGT,$4) }

CaseClauses: CaseClause { ($1) }

Param: /* 2 */ id clnpt eqexp { TUPLE3($1,$2,$3) }

clnpt: /* empty */ { EMPTY_TOKEN }
	|	COLON ParamType { TUPLE2(COLON,$2) }

eqexp: /* empty */ { EMPTY_TOKEN }
	|	EQUALS Expr { TUPLE2(EQUALS,$2) }

Params: Param { ($1) }

ParamClause: LPAREN RPAREN { TUPLE2(LPAREN,RPAREN) }

ParamClauses: /* 3 */ iparamopt { ($1) }

nlopt: /* empty */ { EMPTY_TOKEN }
	|	NEWLINE { (NEWLINE) }

iparamopt: /* empty */ { EMPTY_TOKEN }
	|	nlopt LPAREN IMPLICIT Params RPAREN { TUPLE5($1,LPAREN,IMPLICIT,$4,RPAREN) }

gtclnt: /* empty */ { EMPTY_TOKEN }
	|	GREATER COLON Type { TUPLE3(GREATER,COLON,$3) }

ltclnt: /* empty */ { EMPTY_TOKEN }
	|	LESS COLON Type { TUPLE3(LESS,COLON,$3) }

clnt: /* empty */ { EMPTY_TOKEN }
	|	COLON Type { TUPLE2(COLON,$2) }

vaopt: /* empty */ { EMPTY_TOKEN }
	|	VAL { (VAL) }
	|	VAR { (VAR) }

ClassParam: /* 7 */ /* 8 */ vaopt id COLON ParamType eqexp { TUPLE5($1,$2,COLON,$4,$5) }

ClassParamCommaLst: ClassParam { ($1) }
	|	ParamtypeCommaLst COMMA Paramtype { TUPLE3($1,COMMA,$3) }

ClassParams: ClassParamCommaLst { ($1) }

ClassParamClauses: /* empty */ { EMPTY_TOKEN }
	|	/* 9 */ nlopt LPAREN IMPLICIT ClassParams RPAREN { TUPLE5($1,LPAREN,IMPLICIT,$4,RPAREN) }

FunSig: id ParamClauses { TUPLE2($1,$2) }

ValDcl: ids COLON Type { TUPLE3($1,COLON,$3) }

VarDcl: ids COLON Type { TUPLE3($1,COLON,$3) }

FunDcl: FunSig { ($1) }
	|	FunSig COLON Type { TUPLE3($1,COLON,$3) }

TypeDcl: id gtclnt ltclnt { TUPLE3($1,$2,$3) }

Dcl: VAL ValDcl { TUPLE2(VAL,$2) }
	|	VAR VarDcl { TUPLE2(VAR,$2) }
	|	DEF FunDcl { TUPLE2(DEF,$2) }
	|	TYPE /* 10 */ TypeDcl { TUPLE2(TYPE,$2) }

SimpleExpr1: Literal { ($1) }
	|	Path { ($1) }
	|	UNDERSCORE { (UNDERSCORE) }
	|	LPAREN RPAREN { TUPLE2(LPAREN,RPAREN) }
	|	SimpleExpr DOT id { TUPLE3($1,DOT,$3) }
	|	SimpleExpr TypeArgs { TUPLE2($1,$2) }
	|	SimpleExpr1 ArgumentExprs { TUPLE2($1,$2) }
	|	XmlExpr { ($1) }

unopt: /* empty */ { EMPTY_TOKEN }
	|	UNDERSCORE { (UNDERSCORE) }

SimpleExpr: SimpleExpr1 unopt { TUPLE2($1,$2) }
	|	BlockExpr { ($1) }
	|	NEW ClassTemplate { TUPLE2(NEW,$2) }
	|	NEW TemplateBody { TUPLE2(NEW,$2) }

Prefix: /* empty */ { EMPTY_TOKEN }
	|	HYPHEN { (HYPHEN) }
	|	PLUS { (PLUS) }
	|	TILDE { (TILDE) }
	|	PLING { (PLING) }

semiopt: /* empty */ { EMPTY_TOKEN }
	|	SEMICOLON { (SEMICOLON) }

InfixExpr: Prefix SimpleExpr { TUPLE2($1,$2) }
	|	InfixExpr id nlopt InfixExpr { TUPLE4($1,$2,$3,$4) }

PostfixExpr: InfixExpr { ($1) }
	|	InfixExpr id nlopt { TUPLE3($1,$2,$3) }

elseopt: /* empty */ { EMPTY_TOKEN }
	|	semiopt ELSE Expr { TUPLE3($1,ELSE,$3) }

tryblk: LBRACE Block RBRACE { TUPLE3(LBRACE,$2,RBRACE) }
	|	Expr { ($1) }

forblk: LPAREN Enumerators RPAREN { TUPLE3(LPAREN,$2,RPAREN) }
	|	LBRACE Enumerators RBRACE { TUPLE3(LBRACE,$2,RBRACE) }

yieldopt: /* empty */ { EMPTY_TOKEN }
	|	YIELD { (YIELD) }

catchopt: CATCH LBRACE CaseClauses RBRACE { TUPLE4(CATCH,LBRACE,$3,RBRACE) }

Expr1: PostfixExpr { ($1) }
	|	PostfixExpr Ascription { TUPLE2($1,$2) }
	|	PostfixExpr MATCH LBRACE CaseClauses RBRACE { TUPLE5($1,MATCH,LBRACE,$4,RBRACE) }
	|	id EQUALS Expr { TUPLE3($1,EQUALS,$3) }
	|	SimpleExpr DOT id EQUALS Expr { TUPLE5($1,DOT,$3,EQUALS,$5) }
	|	SimpleExpr1 ArgumentExprs EQUALS Expr { TUPLE4($1,$2,EQUALS,$4) }
	|	IF LPAREN Expr RPAREN /* 12 */ Expr elseopt { TUPLE6(IF,LPAREN,$3,RPAREN,$5,$6) }
	|	WHILE LPAREN Expr RPAREN /* 13 */ Expr { TUPLE5(WHILE,LPAREN,$3,RPAREN,$5) }
	|	TRY tryblk catchopt { TUPLE3(TRY,$2,$3) }
	|	TRY tryblk catchopt FINALLY Expr { TUPLE5(TRY,$2,$3,FINALLY,$5) }
	|	DO Expr semiopt WHILE LPAREN Expr RPAREN { TUPLE7(DO,$2,$3,WHILE,LPAREN,$6,RPAREN) }
	|	FOR forblk /* 14 */ yieldopt Expr { TUPLE4(FOR,$2,$3,$4) }
	|	THROW Expr { TUPLE2(THROW,$2) }
	|	RETURN { (RETURN) }
	|	RETURN Expr { TUPLE2(RETURN,$2) }

impopt: /* empty */ { EMPTY_TOKEN }
	|	IMPLICIT { (IMPLICIT) }

bindopt: Bindings { ($1) }
	|	impopt id { TUPLE2($1,$2) }
	|	UNDERSCORE { (UNDERSCORE) }

Expr: Expr1 { ($1) }
	|	bindopt EQGT Expr { TUPLE3($1,EQGT,$3) }

BlockExpr: LBRACE Block RBRACE { TUPLE3(LBRACE,$2,RBRACE) }
	|	LBRACE CaseClauses RBRACE { TUPLE3(LBRACE,$2,RBRACE) }

Exprs: Expr { ($1) }

exprsopt: /* empty */ { EMPTY_TOKEN }
	|	Exprs { ($1) }

exprscmopt: /* empty */ { EMPTY_TOKEN }
	|	Exprs COMMA { TUPLE2($1,COMMA) }

ArgumentExprs: LPAREN exprsopt RPAREN { TUPLE3(LPAREN,$2,RPAREN) }
	|	LPAREN exprscmopt PostfixExpr COLON UNDERSCORE STAR RPAREN { TUPLE7(LPAREN,$2,$3,COLON,UNDERSCORE,STAR,RPAREN) }
	|	nlopt BlockExpr { TUPLE2($1,$2) }

SelfInvocation: THIS ArgumentExprs { TUPLE2(THIS,$2) }

ConstrBlock: LBRACE SelfInvocation /* 15 */ RBRACE { TUPLE3(LBRACE,$2,RBRACE) }

ConstrExpr: SelfInvocation { ($1) }
	|	ConstrBlock { ($1) }

Generator: Pattern1 LESS HYPHEN Expr { TUPLE4($1,LESS,HYPHEN,$4) }

Enumerators: Generator { ($1) }

Annotation: AT SimpleType { TUPLE2(AT,$2) }

PatDef: Pattern2 /* 16 */ clnt EQUALS Expr { TUPLE4($1,$2,EQUALS,$4) }

VarDef: PatDef { ($1) }
	|	ids COLON Type EQUALS UNDERSCORE { TUPLE5($1,COLON,$3,EQUALS,UNDERSCORE) }

PatVarDef: VAL PatDef { TUPLE2(VAL,$2) }
	|	VAR VarDef { TUPLE2(VAR,$2) }

FunDef: FunSig clnt EQUALS Expr { TUPLE4($1,$2,EQUALS,$4) }
	|	FunSig nlopt LBRACE Block RBRACE { TUPLE5($1,$2,LBRACE,$4,RBRACE) }
	|	THIS ParamClause ParamClauses EQUALS ConstrExpr { TUPLE5(THIS,$2,$3,EQUALS,$5) }
	|	THIS ParamClause ParamClauses nlopt ConstrBlock { TUPLE5(THIS,$2,$3,$4,$5) }

TypeDef: id EQUALS Type { TUPLE3($1,EQUALS,$3) }

ClassDef: id /* 17 */ ClassParamClauses ClassTemplateOpt { TUPLE3($1,$2,$3) }

ObjectDef: id ClassTemplateOpt { TUPLE2($1,$2) }

TraitDef: id TraitTemplateOpt { TUPLE2($1,$2) }

caseopt: /* empty */ { EMPTY_TOKEN }
	|	CASE { (CASE) }

TmplDef: caseopt CLASS ClassDef { TUPLE3($1,CLASS,$3) }
	|	caseopt OBJECT ObjectDef { TUPLE3($1,OBJECT,$3) }
	|	TRAIT TraitDef { TUPLE2(TRAIT,$2) }

Def: PatVarDef { ($1) }
	|	DEF FunDef { TUPLE2(DEF,$2) }
	|	TYPE /* 18 */ TypeDef { TUPLE2(TYPE,$2) }
	|	TmplDef { ($1) }

RefineStat: Dcl { ($1) }
	|	TYPE TypeDef { TUPLE2(TYPE,$2) }

Refinement: nlopt LBRACE RefineStat /* 21 */ RBRACE { TUPLE4($1,LBRACE,$3,RBRACE) }

ImportSelector: id EQGT id { TUPLE3($1,EQGT,$3) }
	|	id EQGT UNDERSCORE { TUPLE3($1,EQGT,UNDERSCORE) }

ImportSelectorAlt: ImportSelector { ($1) }
	|	UNDERSCORE { (UNDERSCORE) }

ImportSelectors: LBRACE /* 22 */ ImportSelectorAlt RBRACE { TUPLE3(LBRACE,$2,RBRACE) }

ImportExpr: StableId DOT id { TUPLE3($1,DOT,$3) }
	|	StableId DOT UNDERSCORE { TUPLE3($1,DOT,UNDERSCORE) }
	|	StableId DOT ImportSelectors { TUPLE3($1,DOT,$3) }

ImportExprLst: ImportExpr { ($1) }
	|	ImportExprLst COMMA ImportExpr { TUPLE3($1,COMMA,$3) }

Import: IMPORT ImportExprLst { TUPLE2(IMPORT,$2) }

BlockStat: Import { ($1) }
	|	/* 23 */ IMPLICIT Def { TUPLE2(IMPLICIT,$2) }
	|	/* 24 */ LAZY Def { TUPLE2(LAZY,$2) }
	|	/* 25 */ /* 26 */ TmplDef { ($1) }
	|	Expr1 { ($1) }

Block: BlockStat { ($1) }

annotlst: /* empty */ { EMPTY_TOKEN }
	|	annotlst Annotation { TUPLE2($1,$2) }
	|	annotlst Annotation NEWLINE { TUPLE3($1,$2,NEWLINE) }

defalt: Def { ($1) }
	|	Dcl { ($1) }

TemplateStat: Import { ($1) }
	|	Expr { ($1) }
	|	annotlst /* 27 */ defalt { TUPLE2($1,$2) }

TemplateBody: nlopt LBRACE TemplateStat /* 28 */ RBRACE { TUPLE4($1,LBRACE,$3,RBRACE) }

Constr: AnnotType { ($1) }

ClassParents: Constr { ($1) }

ClassTemplate: ClassParents { ($1) }

extendsopt: /* empty */ { EMPTY_TOKEN }
	|	EXTENDS { (EXTENDS) }

ClassTemplateOpt: /* empty */ { EMPTY_TOKEN }
	|	EXTENDS ClassTemplate { TUPLE2(EXTENDS,$2) }
	|	extendsopt TemplateBody { TUPLE2($1,$2) }

TraitParents: AnnotType { ($1) }

TraitTemplate: TraitParents { ($1) }

TraitTemplateOpt: /* empty */ { EMPTY_TOKEN }
	|	EXTENDS TraitTemplate { TUPLE2(EXTENDS,$2) }
	|	extendsopt TemplateBody { TUPLE2($1,$2) }

Packaging: PACKAGE QualId nlopt LBRACE TopStatSeq RBRACE { TUPLE6(PACKAGE,$2,$3,LBRACE,$5,RBRACE) }

PackageObject: PACKAGE OBJECT ObjectDef { TUPLE3(PACKAGE,OBJECT,$3) }

TopStat: Packaging { ($1) }
	|	Import { ($1) }
	|	PackageObject { ($1) }
	|	/* 29 */ /* 30 */ TmplDef { ($1) }

TopStatSeq: TopStat { ($1) }

CompilationUnit: /* 31 */ TopStatSeq { ($1) }


