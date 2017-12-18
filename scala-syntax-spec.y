%%
// IDS
                    opchar :  // printableChar not matched by (whiteSpace | letter |
                                // digit | paren | delim | Unicode_Sm | Unicode_So)
                        op :  opchar {opchar}
                    idrest :  {letter | digit} ['_' op]
                     varid :  lower idrest
                   plainid :  (upper idrest) | varid | op
                        id :  plainid | ('`' stringLiteral '`')
                       ids :  id {',' id}
                    QualId :  id {'.' id}
            ClassQualifier :  '[' id ']'
           AccessQualifier :  '[' (id | 'this') ']'
                  StableId :  id | (Path '.' id) | ([id '.'] 'super' [ClassQualifier] '.' id)
                      Path :  StableId | ([id '.'] 'this')

// LITERALS
            booleanLiteral :  'true' | 'false'
             symbolLiteral :  ''' plainid
                   Literal :  ['-'] integerLiteral
                             |  ['-'] floatingPointLiteral
                             |  booleanLiteral
                             |  characterLiteral
                             |  stringLiteral
                             |  symbolLiteral
                             |  'null'

// TYPES
                SimpleType :  StableId
                             |  Path '.' 'type'
                             |  SimpleType TypeArgs
                             |  SimpleType '#' id
                             |  '(' Types ')'
                 AnnotType :  SimpleType {Annotation}
              CompoundType :  (AnnotType {'with' AnnotType} [Refinement]) | Refinement
                 InfixType :  CompoundType {id [nl] CompoundType}
                 ParamType :  Type | ('=>' Type) | (Type '*')
          FunctionArgTypes :  InfixType | ('(' [ ParamType {',' ParamType } ] ')')
                      Type :  (InfixType [ExistentialClause]) | (FunctionArgTypes '=>' Type)

                     Types :  Type {',' Type}
                  TypeArgs :  '[' Types ']'

                Ascription :  (':' InfixType) | (':' Annotation {Annotation}) | (':' '_' '*')
                   Binding :  (id | '_') [':' Type]
                  Bindings :  '(' Binding {',' Binding} ')'

// PATTERN MATCHING
                   TypePat :  Type
             SimplePattern :  '_'
                             |  varid
                             |  Literal
                             |  StableId
                             |  StableId '(' [Patterns] ')'
                             |  StableId '(' [Patterns ','] [varid '@'] '_' '*' ')'
                             |  '(' [Patterns] ')'
                             |  XmlPattern // TODO: What's an XmlPattern
                  Pattern3 :  SimplePattern | (SimplePattern { id [nl] SimplePattern })
                  Pattern2 :  Pattern3 | (varid ['@' Pattern3])
                  Pattern1 :  Pattern2 | (varid ':' TypePat) | ('_' ':' TypePat)
                   Pattern :  Pattern1 { '|' Pattern1 }
                  Patterns :  (Pattern [',' Patterns]) | ('_' *)

                     Guard :  'if' PostfixExpr
                CaseClause :  'case' Pattern [Guard] '=>' Block
               CaseClauses :  CaseClause { CaseClause }

// PARAMS & PARAM CLAUSES
                     Param :  {Annotation} id [':' ParamType] ['=' Expr]
                    Params :  Param {',' Param}
               ParamClause :  [nl] '(' [Params] ')'
              ParamClauses :  {ParamClause} [[nl] '(' 'implicit' Params ')']

                 TypeParam :  (id | '_') [TypeParamClause] ['>:' Type] ['<:' Type] {'<%' Type} {':' Type}
          VariantTypeParam :  {Annotation} ['+' | '-'] TypeParam
           TypeParamClause :  '[' VariantTypeParam {',' VariantTypeParam} ']'

        FunTypeParamClause :  '[' TypeParam {',' TypeParam} ']'

                ClassParam :  {Annotation} {Modifier} [(`val` | `var`)] id ':' ParamType ['=' Expr]
               ClassParams :  ClassParam {',' ClassParam}
          ClassParamClause :  [nl] '(' [ClassParams] ')'
         ClassParamClauses :  {ClassParamClause} [[nl] '(' 'implicit' ClassParams ')']

// DECLARATIONS
                    FunSig :  id [FunTypeParamClause] ParamClauses

                    ValDcl :  ids ':' Type
                    VarDcl :  ids ':' Type
                    FunDcl :  FunSig [':' Type]
                   TypeDcl :  id [TypeParamClause] ['>:' Type] ['<:' Type]
                       Dcl :  'val' ValDcl | 'var' VarDcl | 'def' FunDcl | 'type' {nl} TypeDcl

            ExistentialDcl :  ('type' TypeDcl) | ('val' ValDcl)
         ExistentialClause :  'forSome' '{' ExistentialDcl {semi ExistentialDcl} '}'

// EXPRESSIONS
               SimpleExpr1 :  Literal
                             |  Path
                             |  '_'
                             |  '(' [Exprs] ')'
                             |  SimpleExpr '.' id
                             |  SimpleExpr TypeArgs
                             |  SimpleExpr1 ArgumentExprs
                             |  XmlExpr // TODO: What's an XmlExpr
                SimpleExpr :  SimpleExpr1 ['_'] | BlockExpr | ('new' (ClassTemplate | TemplateBody))
                PrefixExpr :  ['-' | '+' | '~' | '!'] SimpleExpr
                 InfixExpr :  PrefixExpr | (InfixExpr id [nl] InfixExpr)
               PostfixExpr :  InfixExpr [id [nl]]
                     Expr1 :  PostfixExpr
                             |  PostfixExpr Ascription
                             |  PostfixExpr `match` `{` CaseClauses `}`
                             |  [SimpleExpr `.`] id `=` Expr
                             |  SimpleExpr1 ArgumentExprs `=` Expr
                             |  `if` `(` Expr `)` {nl} Expr [[semi] `else` Expr]
                             |  `while` `(` Expr `)` {nl} Expr
                             |  `try` (`{` Block `}` | Expr) [`catch` `{` CaseClauses `}`] [`finally` Expr]
                             |  `do` Expr [semi] `while` `(` Expr `)`
                             |  `for` (`(` Enumerators `)` | `{` Enumerators `}`) {nl} [`yield`] Expr
                             |  `throw` Expr
                             |  `return` [Expr]
                      Expr :  Expr1 | ((Bindings | (['implicit'] id) | '_') '=>' Expr)
                ResultExpr :  Expr1 | ((Bindings | (['implicit'] id | '_') ':' CompoundType) '=>' Block)
                 BlockExpr :  ('{' Block '}') | ('{' CaseClauses '}')

                     Exprs :  Expr {',' Expr}
             ArgumentExprs :  '(' [Exprs] ')'
                             |  '(' [Exprs ','] PostfixExpr ':' '_' '*' ')'
                             |  [nl] BlockExpr

            SelfInvocation :  'this' ArgumentExprs {ArgumentExprs}
               ConstrBlock :  '{' SelfInvocation {semi BlockStat} '}'
                ConstrExpr :  SelfInvocation | ConstrBlock

// FOR COMPREHENSIONS
                 Generator :  Pattern1 '<-' Expr {[semi] Guard | semi Pattern1 '=' Expr}
               Enumerators :  Generator {semi Generator}

// MODIFIERS
            AccessModifier :  ('private' | 'protected') [AccessQualifier]
             LocalModifier :  'abstract' | 'final' | 'sealed' | 'implicit' | 'lazy'
                  Modifier :  LocalModifier | AccessModifier | 'override'

// ANNOTATIONS
                Annotation :  '@' SimpleType {ArgumentExprs}
          ConstrAnnotation :  '@' SimpleType ArgumentExprs

// DEFINITIONS
                    PatDef :  Pattern2 {',' Pattern2} [':' Type] '=' Expr
                    VarDef :  PatDef | (ids ':' Type '=' '_')
                 PatVarDef :  ('val' PatDef) | ('var' VarDef)
                    FunDef :  FunSig [':' Type] '=' Expr
                             |  FunSig [nl] '{' Block '}'
                             |  'this' ParamClause ParamClauses ('=' ConstrExpr | [nl] ConstrBlock)
                   TypeDef :  id [TypeParamClause] '=' Type
                  ClassDef :  id [TypeParamClause] {ConstrAnnotation} [AccessModifier]
                                ClassParamClauses ClassTemplateOpt
                 ObjectDef :  id ClassTemplateOpt
                  TraitDef :  id [TypeParamClause] TraitTemplateOpt
                   TmplDef :  ['case'] 'class' ClassDef
                             |  ['case'] 'object' ObjectDef
                             |  'trait' TraitDef
                       Def :  PatVarDef | ('def' FunDef) | ('type' {nl} TypeDef) | TmplDef

                  EarlyDef :  {Annotation [nl]} {Modifier} PatVarDef
                 EarlyDefs :  '{' [EarlyDef {semi EarlyDef}] '}' 'with'

// REFINEMENTS
                RefineStat :  Dcl | ('type' TypeDef)
                Refinement :  [nl] '{' RefineStat {semi RefineStat} '}'

// IMPORTS
            ImportSelector :  id ['=>' id | '=>' '_']
           ImportSelectors :  '{' {ImportSelector ','} (ImportSelector | '_') '}'
                ImportExpr :  StableId '.' (id | '_' | ImportSelectors)
                    Import :  'import' ImportExpr {',' ImportExpr}

// BLOCKS
                 BlockStat :  Import
                             |  {Annotation} ['implicit' | 'lazy'] Def
                             |  {Annotation} {LocalModifier} TmplDef
                             |  Expr1
                     Block :  BlockStat {semi BlockStat} [ResultExpr]

// CLASSES/TRAITS
                  SelfType :  (id [':' Type] '=>') | ('this' ':' Type '=>')

              TemplateStat :  Import | Expr | {Annotation [nl]} {Modifier} (Def | Dcl)
              TemplateBody :  [nl] '{' [SelfType] TemplateStat {semi TemplateStat} '}'

                    Constr :  AnnotType {ArgumentExprs}

              ClassParents :  Constr {'with' AnnotType}
             ClassTemplate :  [EarlyDefs] ClassParents [TemplateBody]
          ClassTemplateOpt :  'extends' ClassTemplate | [['extends'] TemplateBody]

              TraitParents :  AnnotType {'with' AnnotType}
             TraitTemplate :  [EarlyDefs] TraitParents [TemplateBody]
          TraitTemplateOpt :  'extends' TraitTemplate | [['extends'] TemplateBody]

// COMPILATION UNIT
                 Packaging :  'package' QualId [nl] '{' TopStatSeq '}'
             PackageObject :  'package' 'object' ObjectDef
                   TopStat :  Packaging | Import | PackageObject | ({Annotation [nl]} {Modifier} TmplDef)
                TopStatSeq :  TopStat {semi TopStat}

           CompilationUnit :  {'package' QualId semi} TopStatSeq