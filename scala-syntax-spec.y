%token ABSTRACT
%token CATCH
%token characterLiteral
%token CLASS
%token DEF
%token DO
%token ELSE
%token EXTENDS
%token FINAL
%token FINALLY
%token floatingPointLiteral
%token FOR
%token FOR_SOME
%token IMPORT
%token integerLiteral
%token LAZY
%token MATCH
%token NEWLINE
%token OBJECT
%token OVERRIDE
%token PACKAGE
%token ParamtypeCommaLst
%token Paramtype
%token plainid
%token PRIVATE
%token PROTECTED
%token RETURN
%token SEALED
%token stringLiteral
%token THROW
%token TRAIT
%token TRY
%token VAL
%token varid
%token VAR
%token WHILE
%token WITH
%token XmlExpr
%token XmlPattern
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

%start CompilationUnit

%%
// IDS
//                    opchar :  // printableChar not matched by (whiteSpace | letter |
                                // digit | paren | delim | Unicode_Sm | Unicode_So)
//                        op :  opchar {opchar}
//                    idrest :  {letter | digit} ['_' op]
//                     varid :  lower idrest
//                   plainid :  (upper idrest) | varid | op
                        id :  plainid | '`' stringLiteral '`'
                       ids :  id {',' id}
                    QualId :  id {'.' id}
            ClassQualifier :  '[' id ']'
           AccessQualifier :  '[' id ']' | '[' THIS ']'
                  StableId :  id | Path '.' id | idopt SUPER [ClassQualifier] '.' id
		     idopt :  | id '.'
                      Path :  StableId | idopt THIS

// LITERALS
            booleanLiteral :  TRUE | FALSE
             symbolLiteral :  QUOTE plainid
                   Literal :  integerLiteral | '-' integerLiteral
                             |  floatingPointLiteral | '-' floatingPointLiteral
                             |  booleanLiteral
                             |  characterLiteral
                             |  stringLiteral
                             |  symbolLiteral
                             |  NULL

// TYPES
                SimpleType :  StableId
                             |  Path '.' TYPE
                             |  SimpleType TypeArgs
                             |  SimpleType '#' id
                             |  '(' Types ')'
                 AnnotType :  SimpleType {Annotation}
              CompoundType :  AnnotType {'with' AnnotType} [Refinement] | Refinement
                 InfixType :  CompoundType {id [nl] CompoundType}
                 ParamType :  Type | EQGT Type | Type '*'
	 ParamTypeCommaLstOpt :  | ParamtypeCommaLst
	 ParamTypeCommaLst :  ParamType | ParamTypeCommaLst ',' Paramtype
          FunctionArgTypes :  InfixType | '(' ParamTypeCommaLstOpt ')'
                      Type :  InfixType [ExistentialClause] | FunctionArgTypes EQGT Type

                     Types :  Type {',' Type}
                  TypeArgs :  '[' Types ']'

                Ascription :  ':' InfixType | ':' Annotation {Annotation} | ':' '_' '*'
                   Binding :  id_ | id_ ':' Type
		       id_ :  id | '_'
                  Bindings :  '(' Binding {',' Binding} ')'

// PATTERN MATCHING
                   TypePat :  Type
             SimplePattern :  '_'
                             |  varid
                             |  Literal
                             |  StableId
                             |  StableId '(' [Patterns] ')'
                             |  StableId '(' PatternCommaOpt varidat '_' '*' ')'
                             |  '(' [Patterns] ')'
                             |  XmlPattern // TODO: What's an XmlPattern
	   PatternCommaOpt : | Patterns ','
		  varidat  : | varid '@'
                  Pattern3 :  SimplePattern | SimplePattern { id [nl] SimplePattern }
                  Pattern2 :  Pattern3 | varid atpattern3
		  atpattern3 : | '@' Pattern3
		  cmpatterns : | ',' Patterns
                  Pattern1 :  Pattern2 | varid ':' TypePat | '_' ':' TypePat
                   Pattern :  Pattern1 { '|' Pattern1 }
                  Patterns :  Pattern cmpatterns | '_' '*'

                     Guard :  IF PostfixExpr
                CaseClause :  CASE Pattern [Guard] EQGT Block
               CaseClauses :  CaseClause { CaseClause }

// PARAMS & PARAM CLAUSES
                     Param :  {Annotation} id clnpt eqexp
		     clnpt : | ':' ParamType
		     eqexp : | '=' Expr
                    Params :  Param {',' Param}
               ParamClause :  [nl] '(' [Params] ')'
              ParamClauses :  {ParamClause} iparamopt
	      nlopt :        | NEWLINE
	      iparamopt    : | nlopt '(' IMPLICIT Params ')'
	      idun : id | '_'
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

                ClassParam :  {Annotation} {Modifier} vaopt id ':' ParamType eqexp
	 ClassParamCommaLstOpt :  | ClassParamCommaLst
	 ClassParamCommaLst :  ClassParam | ParamtypeCommaLst ',' Paramtype
               ClassParams :  ClassParamCommaLst
          ClassParamClause :  nlopt '(' [ClassParams] ')'
         ClassParamClauses :  {ClassParamClause} | {ClassParamClause} nlopt '(' IMPLICIT ClassParams ')'

// DECLARATIONS
                    FunSig :  id [FunTypeParamClause] ParamClauses

                    ValDcl :  ids ':' Type
                    VarDcl :  ids ':' Type
                    FunDcl :  FunSig | FunSig ':' Type
                   TypeDcl :  id [TypeParamClause] gtclnt ltclnt
                       Dcl :  VAL ValDcl | VAR VarDcl | DEF FunDcl | TYPE {nl} TypeDcl

            ExistentialDcl :  TYPE TypeDcl | VAL ValDcl
         ExistentialClause :  FOR_SOME '{' ExistentialDcl {SEMICOLON ExistentialDcl} '}'

// EXPRESSIONS
               SimpleExpr1 :  Literal
                             |  Path
                             |  '_'
                             |  '(' [Exprs] ')'
                             |  SimpleExpr '.' id
                             |  SimpleExpr TypeArgs
                             |  SimpleExpr1 ArgumentExprs
                             |  XmlExpr // TODO: What's an XmlExpr
	       unopt : | '_'
                SimpleExpr :  SimpleExpr1 unopt | BlockExpr | NEW ClassTemplate | NEW TemplateBody
                Prefix :  | '-' | '+' | '~' | '!'
	        semiopt : | SEMICOLON
                 InfixExpr :  Prefix SimpleExpr | InfixExpr id nlopt InfixExpr
               PostfixExpr :  InfixExpr | InfixExpr id nlopt
	 elseopt: | semiopt ELSE Expr
	 tryblk : '{' Block '}' | Expr
	 forblk: '(' Enumerators ')' | '{' Enumerators '}'
	 yieldopt: | YIELD
	 catchopt: CATCH '{' CaseClauses '}'
                     Expr1 :  PostfixExpr
                             |  PostfixExpr Ascription
                             |  PostfixExpr MATCH '{' CaseClauses '}'
                             |  id '=' Expr | SimpleExpr '.' id '=' Expr
                             |  SimpleExpr1 ArgumentExprs '=' Expr
                             |  IF '(' Expr ')' {nl} Expr elseopt
                             |  WHILE '(' Expr ')' {nl} Expr
                             |  TRY tryblk catchopt |TRY tryblk catchopt FINALLY Expr
                             |  DO Expr semiopt WHILE '(' Expr ')'
                             |  FOR forblk {nl} yieldopt Expr
                             |  THROW Expr
                             |  RETURN | RETURN Expr
			     impopt: | IMPLICIT
		     bindopt: Bindings | impopt id | '_'
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

// ANNOTATIONS
                Annotation :  '@' SimpleType {ArgumentExprs}
          ConstrAnnotation :  '@' SimpleType ArgumentExprs

// DEFINITIONS
                    PatDef :  Pattern2 {',' Pattern2} clnt '=' Expr
                    VarDef :  PatDef | ids ':' Type '=' '_'
                 PatVarDef :  VAL PatDef | VAR VarDef
                    FunDef :  FunSig clnt '=' Expr
                             |  FunSig nlopt '{' Block '}'
                             |  THIS ParamClause ParamClauses '=' ConstrExpr
                             |  THIS ParamClause ParamClauses nlopt ConstrBlock
                   TypeDef :  id [TypeParamClause] '=' Type
                  ClassDef :  id [TypeParamClause] {ConstrAnnotation} [AccessModifier]
                                ClassParamClauses ClassTemplateOpt
                 ObjectDef :  id ClassTemplateOpt
                  TraitDef :  id [TypeParamClause] TraitTemplateOpt
		  caseopt : | CASE
                   TmplDef :  caseopt CLASS ClassDef
                             |  caseopt OBJECT ObjectDef
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
            ImportSelector :  id EQGT id | id EQGT '_'
	    ImportSelectorAlt : ImportSelector | '_'
           ImportSelectors :  '{' {ImportSelector ','} ImportSelectorAlt '}'
                ImportExpr :  StableId '.' id | StableId '.' '_' | StableId '.' ImportSelectors
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
                  SelfType :  id clnt EQGT | THIS ':' Type EQGT
		  annotlst : | annotlst Annotation | annotlst Annotation NEWLINE
		     defalt : Def | Dcl
              TemplateStat :  Import | Expr | annotlst {Modifier} defalt
              TemplateBody :  nlopt '{' [SelfType] TemplateStat {SEMICOLON TemplateStat} '}'

                    Constr :  AnnotType {ArgumentExprs}

              ClassParents :  Constr {WITH AnnotType}
             ClassTemplate :  [EarlyDefs] ClassParents [TemplateBody]
	     extendsopt : | EXTENDS
          ClassTemplateOpt : | EXTENDS ClassTemplate | extendsopt TemplateBody

              TraitParents :  AnnotType {WITH AnnotType}
             TraitTemplate :  [EarlyDefs] TraitParents [TemplateBody]
          TraitTemplateOpt : | EXTENDS TraitTemplate | extendsopt TemplateBody

// COMPILATION UNIT
                 Packaging :  PACKAGE QualId nlopt '{' TopStatSeq '}'
             PackageObject :  PACKAGE OBJECT ObjectDef
                   TopStat :  Packaging | Import | PackageObject | {Annotation nlopt} {Modifier} TmplDef
                TopStatSeq :  TopStat {SEMICOLON TopStat}

           CompilationUnit :  {PACKAGE QualId SEMICOLON} TopStatSeq
