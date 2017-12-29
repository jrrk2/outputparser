%token ABSTRACT
%token CATCH
%token CHARACTERLITERAL
%token CLASS
%token DEF
%token DO
%token ELSE
%token EXTENDS
%token FINAL
%token FINALLY
%token FLOATINGPOINTLITERAL
%token FOR
%token FOR_SOME
%token IMPORT
%token INTEGERLITERAL
%token LAZY
%token MATCH
%token NEWLINE
%token OBJECT
%token OVERRIDE
%token PACKAGE
%token PLAINID
%token PRIVATE
%token PROTECTED
%token RETURN
%token SEALED
%token STRINGLITERAL
%token THROW
%token TRAIT
%token TRY
%token VAL
%token VARID
%token VAR
%token WHILE
%token WITH
%token YIELD
%token SUPER
%token THIS
%token TRUE
%token FALSE
%token NULL
%token TYPE
%token EQGT
%token CASE
%token IF
%token NEW
%token SEMICOLON
%token IMPLICIT
%token QUOTE
%token SLASH

%start CompilationUnit

%%
// IDS
//                    opchar :  // printableChar not matched by (whiteSpace | letter |
                                // digit | paren | delim | Unicode_Sm | Unicode_So)
//                        op :  opchar {opchar}
//                    idrest :  {letter | digit} ['_' op]
//                     VARID :  lower idrest
//                   PLAINID :  (upper idrest) | VARID | op
 //                        id :  PLAINID | '`' STRINGLITERAL '`'
                       ids :  PLAINID {',' PLAINID}
                    QualId :  PLAINID {'.' PLAINID}
            ClassQualifier :  '[' PLAINID ']'
           AccessQualifier :  '[' PLAINID ']' | '[' THIS ']'
                  StableId :  PLAINID | StableId '.' PLAINID | THIS '.' PLAINID | PLAINID '.' THIS '.' PLAINID | SUPER [ClassQualifier] '.' PLAINID | PLAINID '.' SUPER [ClassQualifier] '.' PLAINID

// LITERALS
            BOOLEANLITERAL :  TRUE | FALSE
             SYMBOLLITERAL :  QUOTE PLAINID
                   Literal :  INTEGERLITERAL | '-' INTEGERLITERAL
                             |  FLOATINGPOINTLITERAL | '-' FLOATINGPOINTLITERAL
                             |  BOOLEANLITERAL
                             |  CHARACTERLITERAL
                             |  STRINGLITERAL
                             |  SYMBOLLITERAL
                             |  NULL

// TYPES
                SimpleType :  StableId
                             |  StableId '.' TYPE
                             |  THIS '.' TYPE
                             |  PLAINID '.' THIS '.' TYPE
                             |  SimpleType TypeArgs
                             |  SimpleType '#' PLAINID
                             |  '(' Types ')'
                 AnnotType :  SimpleType | SimpleType AnnotationLst
              CompoundType :  AnnotType {'with' AnnotType} [Refinement] | Refinement
                 InfixType :  CompoundType {id [nl] CompoundType}
                 ParamType :  Type | EQGT Type | Type '*'
	 ParamTypeCommaLstOpt :  | ParamTypeCommaLst
	 ParamTypeCommaLst :  ParamType | ParamTypeCommaLst ',' ParamType
          FunctionArgTypes :  InfixType | '(' ParamTypeCommaLstOpt ')'
                      Type :  InfixType [ExistentialClause] | FunctionArgTypes EQGT Type

                     Types :  Type {',' Type}
                  TypeArgs :  '[' Types ']'

                Ascription :  ':' InfixType | ':' Annotation {Annotation} | ':' '_' '*'
                   Binding :  id_ | id_ ':' Type
		       id_ :  PLAINID | '_'
                  Bindings :  '(' Binding {',' Binding} ')'

// PATTERN MATCHING
                   TypePat :  Type
             SimplePattern :  '_'
                             |  VARID
                             |  Literal
                             |  StableId
                             |  StableId '(' [Patterns] ')'
                             |  StableId '(' PatternCommaOpt VARIDat '_' '*' ')'
                             |  '(' [Patterns] ')'
//                             |  XmlPattern // TODO: What's an XmlPattern
	   PatternCommaOpt : | Patterns ','
		  VARIDat  : | VARID '@'
                  Pattern3 :  SimplePattern | SimplePattern { PLAINID [nl] SimplePattern }
                  Pattern2 :  Pattern3 | VARID atpattern3
		  atpattern3 : | '@' Pattern3
		  cmpatterns : | ',' Patterns
                  Pattern1 :  Pattern2 | VARID ':' TypePat | '_' ':' TypePat
                   Pattern :  Pattern1 { '|' Pattern1 }
                  Patterns :  Pattern cmpatterns | '_' '*'

                     Guard :  IF PostfixExpr
                CaseClause :  CASE Pattern [Guard] EQGT Block
               CaseClauses :  CaseClause { CaseClause }

// PARAMS & PARAM CLAUSES
                     Param :  {Annotation} PLAINID clnpt eqexp
		     clnpt : | ':' ParamType
		     eqexp : | '=' Expr
                    Params :  Param {',' Param}
               ParamClause :  [nl] '(' [Params] ')'
              ParamClauses :  {ParamClause} iparamopt
	      nlopt :        | NEWLINE
	      iparamopt    : | nlopt '(' IMPLICIT Params ')'
	      idun : PLAINID | '_'
	      gtclnt : | '>' ':' Type
	      ltclnt : | '<' ':' Type
	      ltpct : | '<' '%' Type
	      clnt : | ':' Type
	      plmi : | '+' | '-'
	      vaopt : | VAL | VAR
                 TypeParam :  idun [TypeParamClause] gtclnt ltclnt ltpct clnt
          VariantTypeParam :  {Annotation} plmi TypeParam
           TypeParamClause :  '[' VariantTypeParam {',' VariantTypeParam} ']'

        FunTypeParamClause :  '[' TypeParam {',' TypeParam} ']'

                ClassParam : PLAINID ':' ParamType eqexp
                             |  VAL PLAINID ':' ParamType eqexp		
                             |  VAR PLAINID ':' ParamType eqexp		
			     |  AnnotationLst vaopt PLAINID ':' ParamType eqexp
			     |  ModifierLst vaopt PLAINID ':' ParamType eqexp
			     |  AnnotationLst ModifierLst vaopt PLAINID ':' ParamType eqexp
	 ClassParamCommaLst :  ClassParam | ClassParamCommaLst ',' ClassParam
          ClassParamClause :  nlopt '(' ')' | nlopt '(' ClassParamCommaLst ')'
         ClassParamClauses :  ClassParamClause | ClassParamClause nlopt '(' IMPLICIT ClassParamCommaLst ')'

// DECLARATIONS
                    FunSig :  PLAINID [FunTypeParamClause] ParamClauses

                    ValDcl :  ids ':' Type
                    VarDcl :  ids ':' Type
                    FunDcl :  FunSig | FunSig ':' Type
                   TypeDcl :  PLAINID [TypeParamClause] gtclnt ltclnt
                       Dcl :  VAL ValDcl | VAR VarDcl | DEF FunDcl | TYPE {nl} TypeDcl

            ExistentialDcl :  TYPE TypeDcl | VAL ValDcl
         ExistentialClause :  FOR_SOME '{' ExistentialDcl {SEMICOLON ExistentialDcl} '}'

// EXPRESSIONS
               SimpleExpr1 :  Literal
                             |  StableId
	                     | THIS
	                     | PLAINID '.' THIS
                             |  '_'
                             |  '(' [Exprs] ')'
                             |  SimpleExpr '.' PLAINID
                             |  SimpleExpr TypeArgs
                             |  SimpleExpr1 ArgumentExprs
//                             |  XmlExpr // TODO: What's an XmlExpr
	       unopt : | '_'
                SimpleExpr :  SimpleExpr1 unopt | BlockExpr | NEW ClassTemplate | NEW TemplateBody
                Prefix :  | '-' | '+' | '~' | '!'
	        semiopt : | SEMICOLON
                 InfixExpr :  Prefix SimpleExpr | InfixExpr PLAINID nlopt InfixExpr
               PostfixExpr :  InfixExpr | InfixExpr PLAINID nlopt
	 elseopt: | semiopt ELSE Expr
	 tryblk : '{' Block '}' | Expr
	 forblk: '(' Enumerators ')' | '{' Enumerators '}'
	 yieldopt: | YIELD
	 catchopt: CATCH '{' CaseClauses '}'
                     Expr1 :  PostfixExpr
                             |  PostfixExpr Ascription
                             |  PostfixExpr MATCH '{' CaseClauses '}'
                             |  PLAINID '=' Expr | SimpleExpr '.' PLAINID '=' Expr
                             |  SimpleExpr1 ArgumentExprs '=' Expr
                             |  IF '(' Expr ')' {nl} Expr elseopt
                             |  WHILE '(' Expr ')' {nl} Expr
                             |  TRY tryblk catchopt |TRY tryblk catchopt FINALLY Expr
                             |  DO Expr semiopt WHILE '(' Expr ')'
                             |  FOR forblk {nl} yieldopt Expr
                             |  THROW Expr
                             |  RETURN | RETURN Expr
			     impopt: | IMPLICIT
		     bindopt: Bindings | impopt PLAINID | '_'
                      Expr :  Expr1 | bindopt EQGT Expr
                ResultExpr :  Expr1 | bindopt ':' CompoundType EQGT Block
                 BlockExpr :  '{' Block '}' | '{' CaseClauses '}'

                     Exprs :  Expr {',' Expr}
exprsopt : | Exprs
exprscmopt : | Exprs ','
             ArgumentExprs :  '(' exprsopt ')'
                             |  '(' exprscmopt PostfixExpr ':' '_' '*' ')'
                             |  nlopt BlockExpr

            SelfInvocation :  THIS ArgumentExprs {ArgumentExprs}
               ConstrBlock :  '{' SelfInvocation {SEMICOLON BlockStat} '}'
                ConstrExpr :  SelfInvocation | ConstrBlock

// FOR COMPREHENSIONS
                 Generator :  Pattern1 '<' '-' Expr {semiopt Guard | SEMICOLON Pattern1 '=' Expr}
               Enumerators :  Generator {SEMICOLON Generator}

// MODIFIERS
access: PRIVATE | PROTECTED
            AccessModifier :  access | access AccessQualifier
             LocalModifier :  ABSTRACT | FINAL | SEALED | IMPLICIT | LAZY
                  Modifier :  LocalModifier | AccessModifier | OVERRIDE
               ModifierLst : Modifier | ModifierLst Modifier
// ANNOTATIONS
                Annotation :  '@' SimpleType {ArgumentExprs}
             AnnotationLst : Annotation | AnnotationLst Annotation
          ConstrAnnotation :  '@' SimpleType ArgumentExprs

// DEFINITIONS
                    PatDef :  Pattern2 {',' Pattern2} clnt '=' Expr
                    VarDef :  PatDef | ids ':' Type '=' '_'
                 PatVarDef :  VAL PatDef | VAR VarDef
                    FunDef :  FunSig clnt '=' Expr
                             |  FunSig nlopt '{' Block '}'
                             |  THIS ParamClause ParamClauses '=' ConstrExpr
                             |  THIS ParamClause ParamClauses nlopt ConstrBlock
                   TypeDef :  PLAINID [TypeParamClause] '=' Type
                  ClassDef :  PLAINID [TypeParamClause] {ConstrAnnotation} [AccessModifier]
                                ClassParamClauses ClassTemplateOpt
                 ObjectDef :  PLAINID ClassTemplateOpt
                  TraitDef :  PLAINID [TypeParamClause] TraitTemplateOpt
		  TmplDefAnno : TmplDef
		             | Modifier TmplDef
		             | Annotation nlopt TmplDef
		             | Annotation nlopt Modifier TmplDef
                   TmplDef : CLASS ClassDef
		             | CASE CLASS ClassDef
                             | OBJECT ObjectDef
                             | CASE OBJECT ObjectDef
                             |  TRAIT TraitDef
                       Def :  PatVarDef | DEF FunDef | TYPE {nl} TypeDef | TmplDef

                  EarlyDef :  {Annotation nlopt} {Modifier} PatVarDef
		  earlydeflstopt : | earlydeflst
		  earlydeflst : EarlyDef | earlydeflst SEMICOLON EarlyDef
                 EarlyDefs :  '{' earlydeflstopt '}' WITH

// REFINEMENTS
                RefineStat :  Dcl | TYPE TypeDef
                Refinement :  nlopt '{' RefineStat {SEMICOLON RefineStat} '}'

// IMPORTS
            ImportSelector :  PLAINID EQGT PLAINID | PLAINID EQGT '_'
	    ImportSelectorAlt : ImportSelector | '_'
           ImportSelectors :  '{' {ImportSelector ','} ImportSelectorAlt '}'
                ImportExpr :  StableId '.' PLAINID | PLAINID '.' '_' | StableId '.' ImportSelectors
		ImportExprLst : ImportExpr | ImportExprLst ',' ImportExpr
                    Import :  IMPORT ImportExprLst

// BLOCKS
                 BlockStat :  Import
                             |  {Annotation} IMPLICIT Def
                             |  {Annotation} LAZY Def
                             |  {Annotation} {LocalModifier} TmplDef
                             |  Expr1
                     Block :  BlockStat {SEMICOLON BlockStat} [ResultExpr]

// CLASSES/TRAITS
                  SelfType :  PLAINID clnt EQGT | THIS ':' Type EQGT
		  annotlst : | annotlst Annotation | annotlst Annotation NEWLINE
		     defalt : Def | Dcl
              TemplateStat :  Import | Expr | annotlst {Modifier} defalt
              TemplateBody :  nlopt '{' [SelfType] TemplateStat {SEMICOLON TemplateStat} '}'

                    Constr :  AnnotType {ArgumentExprs}

              ClassParents :  Constr | Constr WITH AnnotType
             ClassTemplate :  ClassParents [TemplateBody]
	                     | EarlyDefs ClassParents [TemplateBody]
	     extendsopt : | EXTENDS
          ClassTemplateOpt : | EXTENDS ClassTemplate | extendsopt TemplateBody

              TraitParents :  AnnotType {WITH AnnotType}
             TraitTemplate :  [EarlyDefs] TraitParents [TemplateBody]
          TraitTemplateOpt : | EXTENDS TraitTemplate | extendsopt TemplateBody

// COMPILATION UNIT
                 Packaging :  PACKAGE QualId nlopt | PACKAGE QualId nlopt TopStatSeq
             PackageObject :  PACKAGE OBJECT ObjectDef
                   TopStat :  Packaging | Import | PackageObject | TmplDefAnno
                TopStatSeq :  TopStat | TopStatSeq TopStat | TopStatSeq SEMICOLON TopStat
           CompilationUnit :   TopStatSeq | PACKAGE QualId SEMICOLON TopStatSeq 
