%nonassoc DefaultStrength
%nonassoc DRIVESTRENGTH CHARGESTRENGTH
%nonassoc NoElse
%nonassoc "else"
%right "|->" "|=>" "#-#" "#=#"
%right "iff"
%left "or"
%left "and"
%left "intersect"
%left "within"
%right "throughout"
%left "##"
%nonassoc "[*]" "[=]" "[->]"
%right "->" "<->"
%right "?" ":"
%left "||"
%left "&&"
%left "|" "~|"
%left "^" "^~" "~^"
%left "&" "~&"
%left "==" "!=" "===" "!==" "==?" "!=?"
%left "<" "<=" ">" ">=" "inside" "dist"
%left "<<" ">>" "<<<" ">>>"
%left "+" "-"
%left "*" "/" "%"
%left "**"
%right REDUCE_OP "!" "~" "++" "--"
%left "'"
%left "(" RPAREN "[" "]" "." COLON_COLON "#"

%token DRIVESTRENGTH CHARGESTRENGTH EOF_TOKEN
%token Always "always"
%token Always_comb "always_comb"
%token Always_ff "always_ff"
%token Always_latch "always_latch"
%token And "and"
%token Assert "assert"
%token Assign "assign"
%token Assume "assume"
%token Automatic "automatic"
%token Begin "begin"
%token Bit "bit"
%token Break "break"
%token Buf "buf"
%token Byte "byte"
%token CASE "case"
%token Casex "casex"
%token Casez "casez"
%token Class "class"
%token Const "const"
%token Continue "continue"
%token Cover "cover"
%token Default "default"
%token Defparam "defparam"
%token Disable "disable"
%token Dist "dist"
%token Do "do"
%token Else "else"
%token End "end"
%token Endcase "endcase"
%token Endclass "endclass"
%token Endfunction "endfunction"
%token Endgenerate "endgenerate"
%token Endinterface "endinterface"
%token Endmodule "endmodule"
%token Endpackage "endpackage"
%token Endtask "endtask"
%token Enum "enum"
%token Event "event"
%token Export "export"
%token Extern "extern"
%token Final "final"
%token First_match "first_match"
%token For "for"
%token Foreach "foreach"
%token Forever "forever"
%token Fork "fork"
%token Function "function"
%token Generate "generate"
%token Genvar "genvar"
%token Highz0 "highz0"
%token Highz1 "highz1"
%token If "if"
%token Iff "iff"
%token Import "import"
%token Initial "initial"
%token Inout "inout"
%token Input "input"
%token Inside "inside"
%token Int "int"
%token Integer "integer"
%token Interface "interface"
%token Intersect "intersect"
%token Join "join"
%token Large "large"
%token Local "local"
%token Localparam "localparam"
%token Logic "logic"
%token Longint "longint"
%token Macromodule "macromodule"
%token Medium "medium"
%token Modport "modport"
%token Module "module"
%token Nand "nand"
%token Negedge "negedge"
%token Nor "nor"
%token Not "not"
%token Or "or"
%token Output "output"
%token Package "package"
%token Packed "packed"
%token Parameter "parameter"
%token Posedge "posedge"
%token Priority "priority"
%token Property "property"
%token Protected "protected"
%token Pull0 "pull0"
%token Pull1 "pull1"
%token REAL "real"
%token Realtime "realtime"
%token Reg "reg"
%token Repeat "repeat"
%token Return "return"
%token Shortint "shortint"
%token Shortreal "shortreal"
%token Signed "signed"
%token Small "small"
%token Static "static"
%token String "string"
%token Strong0 "strong0"
%token Strong1 "strong1"
%token Struct "struct"
%token Supply0 "supply0"
%token Supply1 "supply1"
%token Task "task"
%token Throughout "throughout"
%token TIME "time"
%token Timeprecision "timeprecision"
%token Timeunit "timeunit"
%token Tri "tri"
%token Tri0 "tri0"
%token Tri1 "tri1"
%token Triand "triand"
%token Trior "trior"
%token Trireg "trireg"
%token TYPE "type"
%token TYPEDEF "typedef"
%token Union "union"
%token UNIQUE "unique"
%token Unique0 "unique0"
%token Unsigned "unsigned"
%token Uwire "uwire"
%token Var "var"
%token Void "void"
%token Wand "wand"
%token Weak0 "weak0"
%token Weak1 "weak1"
%token While "while"
%token Wire "wire"
%token Within "within"
%token Wor "wor"
%token Xnor "xnor"
%token Xor "xor"
%token AMPERSAND "&"
%token AT "@"
%token BACKQUOTE "`"
%token CARET "^"
%token COLON ":"
%token COMMA ","
%token DOLLAR "$"
%token DOT "."
%token EQUALS "="
%token GREATER ">"
%token HASH "#"
%token HYPHEN "-"
%token LBRACE "{"
%token LBRACK "["
%token LESS "<"
%token LPAREN "("
%token PERCENT "%"
%token PLING "!"
%token PLUS "+"
%token QUERY "?"
%token RBRACE "}"
%token RBRACK "]"
%token RPAREN ")"
%token SEMICOLON ";"
%token SLASH "/"
%token STAR "*"
%token TILDE "~"
%token UNDERSCORE "_"
%token VBAR "|"
%token CARET_EQ  "^="
%token DOUBLEQUOTE  "\""
%token PERCENT_EQ  "%="
%token SLASH_EQ  "/="
%token VBAR_VBAR  "||" 
%token TILDE_VBAR  "~|" 
%token CARET_TILDE  "^~" 
%token TILDE_AMPERSAND  "~&" 
%token EQ_EQ  "==" 
%token PLUS_EQ  "+=" 
%token HYPHEN_EQ  "-=" 
%token LT_EQ  "<=" 
%token PLING_EQ  "!=" 
%token VBAR_EQ  "|=" 
%token LT_LT  "<<" 
%token GT_GT  ">>" 
%token HYPHEN_GT  "->" 
%token HYPHEN_GT_GT  "->>" 
%token GT_EQ  ">=" 
%token DOT_STAR  ".*" 
%token PLUS_COLON  "+:" 
%token HYPHEN_COLON  "-:" 
%token COLON_COLON  "::" 
%token PLUS_PLUS  "++" 
%token HYPHEN_HYPHEN  "--" 
%token STAR_STAR  "**" 
%token AMPERSAND_AMPERSAND  "&&" 
%token AMPERSAND_EQ  "&=" 
%token QUOTE_LBRACE  "'{" 
%token LBRACK_HYPHEN_GT "[->"
%token GT_GT_GT  ">>>" 
%token GT_GT_GT_EQUALS  ">>>=" 
%token GT_GT_EQUALS  ">>=" 
%token LT_LT_LT  "<<<" 
%token LT_LT_LT_EQUALS  "<<<=" 
%token LT_LT_EQ  "<<=" 
%token EQ_EQ_EQ  "===" 
%token EQ_EQ_QUERY  "==?" 
%token PLING_EQ_EQ  "!==" 
%token PLING_EQ_QUERY  "!=?" 
%token VBAR_HYPHEN_GT "|->"
%token LT_HYPHEN_GT "<->"
%token VBAR_EQ_GT "|=>"
%token HASH_HYPHEN_HASH "#-#"
%token HASH_EQUALS_HASH "#=#"
%token HASH_HASH "##"
%token LPAREN_STAR "(*"
%token AT_STAR "@*"
%token STAR_EQUALS "*="
%token TILDE_CARET "~^"
%token STAR_RPAREN "*)"
%token LPAREN_STAR_RPAREN "(*)"
%token LBRACK_STAR_RBRACK "[*]"
%token LBRACK_EQUALS_RBRACK "[=]"
%token LBRACK_HYPHEN_GT_RBRACK "[->]"
%token QUOTE "'"
%token Celldefine "`celldefine"
%token Default_nettype "`default_nettype"
%token Endcelldefine "`endcelldefine"
%token Nounconnected_drive "`nounconnected_drive"
%token Resetall "`resetall"
%token Unconnected_drive "`unconnected_drive"
%token Bits "$bits"
%token DIMENSIONS "$dimensions"
%token High "$high"
%token Increment "$increment"
%token Left "$left"
%token Low "$low"
%token Right "$right"
%token Size "$size"
%token Unpacked_dimensions "$unpacked_dimensions"
%token <string> STRING
%token <string> SimpleIdentifier
%token <string> SystemIdentifier
%token <string> EscapedIdentifier
%token <float> Real
%token <int64> Time
%token <string> Number
%token <token list> TLIST
%token <token*token> TUPLE2
%token <token*token*token> TUPLE3
%token <token*token*token*token> TUPLE4
%token <token*token*token*token*token> TUPLE5
%token <token*token*token*token*token*token> TUPLE6
%token <token*token*token*token*token*token*token> TUPLE7
%token <token*token*token*token*token*token*token*token> TUPLE8
%token <token*token*token*token*token*token*token*token*token> TUPLE9

%type <token>  Description Identifier IdentifierP PartHeader Expr NonIntegerTypeP ModuleItem NonGenerateModuleItem
%type <token list> Descriptions PortDecls PackageImportDeclarations PIParams ModuleItems AttributeInstances Lifetime

%start <token> ml_start
%%

ml_start: Descriptions EOF_TOKEN { TUPLE2 (TLIST (List.rev $1), EOF_TOKEN) }

opt(p)
  : p { [] }
  | { [] }

Descriptions
  : { [] }
  | Descriptions SEMICOLON { $1 }
  | Descriptions Description { $2 :: $1 }

Description
  : Part(ModuleKW, endmodule) { $1 }
  | Part(InterfaceKW, endinterface) { $1 }
  | PackageDeclaration { TLIST $1 }
  | PITrace PackageItem { $1 }

OptAsgn
  : { [] }
  | EQUALS Expr { [ $2 ] }

Type
  : TypeNonIdent { [] }
  | TypeAlias { [] }

TypeAlias
  : Identifier Dimensions { [] }
  | Identifier COLON_COLON Identifier Dimensions { [] }
  | Identifier ParamBindings COLON_COLON Identifier Dimensions { [] }

TypeNonIdent
  : PartialType OptSigning Dimensions { [] }

PartialType
  : PartialTypeP { [] }

PartialTypeP
  : IntegerVectorTypeP { [] }
  | IntegerAtomTypeP { [] }
  | NonIntegerTypeP { [] }
  | "type" LPAREN Expr RPAREN { [] }
  | "enum" EnumBaseType LBRACE EnumItems RBRACE { [] }
  | "struct" Packing LBRACE StructItems RBRACE { [] }
  | "union" Packing LBRACE StructItems RBRACE { [] }

CastingType
  : IntegerVectorTypeP { [] }
  | IntegerAtomTypeP { [] }
  | NonIntegerTypeP { [] }
  | SigningP { [] }

EnumBaseType
  : Type { [] }
  | Dimensions { [] }

Signing
  : SigningP { [] }

SigningP
  : "signed" { [] }
  | "unsigned" { [] }

OptSigning
  : Signing { [] }
  | { [] }

NetTypeP
  : "supply0" { [] }
  | "supply1" { [] }
  | "tri" { [] }
  | "triand" { [] }
  | "trior" { [] }
  | "trireg" { [] }
  | "tri0" { [] }
  | "tri1" { [] }
  | "uwire" { [] }
  | "wire" { [] }
  | "wand" { [] }
  | "wor" { [] }

IntegerVectorTypeP
  : "bit" { [] }
  | "logic" { [] }
  | "reg" { [] }

IntegerAtomTypeP
  : "byte" { [] }
  | "shortint" { [] }
  | "int" { [] }
  | "longint" { [] }
  | "integer" { [] }
  | "time" { [] }

NonIntegerTypeP
  : "shortreal" { TLIST [] }
  | "real" { TLIST [] }
  | "realtime" { TLIST [] }
  | STRING { STRING $1 }
  | "event" { TLIST [] }

EnumItems
  : VariablePortIdentifiers { [] }

StructItems
  : StructItem { [] }
  | StructItems StructItem { [] }

StructItem
  : Type FieldDecls SEMICOLON { [] }

FieldDecls
  : FieldDecl { [] }
  | FieldDecls "," FieldDecl { [] }

FieldDecl
  : Identifier Dimensions { [] }

Packing
  : "packed" OptSigning { [] }
  | { [] }

Part(begin_rule,end_rule)
  : AttributeInstances begin_rule PartHeader ModuleItems end_rule StrTag { TUPLE6(TLIST $1,$2,$3,TLIST $4,$5,TLIST $6) }
  | AttributeInstances "extern" begin_rule PartHeader { TUPLE3(TLIST $1,$3,$4) }

PartHeader
  : Lifetime Identifier PackageImportDeclarations Params PortDecls SEMICOLON { TUPLE5(TLIST $1,$2,TLIST $3,$4,TLIST $5) }

ModuleKW
  : "module" { Module }
  | "macromodule" { Macromodule }

InterfaceKW
  : "interface" { Interface }

PackageDeclaration
  : "package" Lifetime Identifier SEMICOLON PackageItems endpackage StrTag { [] }
  | "class" Lifetime Identifier PIParams SEMICOLON ClassItems endclass StrTag { [] }

StrTag
  : { [] }
  | ":" Identifier { [] }

PackageImportDeclarations
  : PackageImportDeclaration PackageImportDeclarations { $1 :: $2 }
  | { [] }

PackageImportDeclaration
  : "import" PackageImportItems SEMICOLON { TLIST $2 }

Params
  : PIParams { TLIST [] }

PIParams
  : { [] }
  | "#" LPAREN RPAREN { [] }
  | "#" LPAREN ParamDeclTokens(RPAREN) { $3 }

PortDecls
  : LPAREN PortDeclTokens(RPAREN) { [ $2 ] }
  | LPAREN RPAREN { [] }
  | { [] }

ModportItems
  : ModportItem { [] }
  | ModportItems "," ModportItem { [] }

ModportItem
  : Identifier LPAREN ModportPortsDeclarations { [] }

ModportPortsDeclarations
  : ModportPortsDeclaration(RPAREN) { [] }
  | ModportPortsDeclaration(",") ModportPortsDeclarations { [] }

ModportPortsDeclaration(delim)
  : ModportSimplePortsDeclaration(delim) { [] }

ModportSimplePortsDeclaration(delim)
  : Direction ModportSimplePorts delim { [] }

ModportSimplePorts
  : ModportSimplePort { [] }
  | ModportSimplePorts "," ModportSimplePort { [] }

ModportSimplePort
  : "." Identifier LPAREN ExprOrNil RPAREN { [] }
  | Identifier { [] }

Identifier
  : IdentifierP { $1 }

IdentifierP
  : SimpleIdentifier { SimpleIdentifier $1 }
  | EscapedIdentifier { EscapedIdentifier $1 }
  | SystemIdentifier { SystemIdentifier $1 }

Identifiers
  : Identifier { [ $1 ] }
  | Identifiers "," Identifier { $3 :: $1 }

Strength
  : { [] }
  | DRIVESTRENGTH %prec DRIVESTRENGTH { [] }
  | CHARGESTRENGTH %prec CHARGESTRENGTH { [] }

DeclTokens(delim)
  : DeclTokensBase(DeclTokens(delim), delim) { [] }

DeclTokensBase(repeat, delim)
  : DeclToken DTDelim(delim) { [] }
  | DeclToken repeat { [] }
  | IdentifierP ParamBindings repeat { [] }
  | DeclTokenAsgn "," repeat { [] }
  | DeclTokenAsgn DTDelim(delim) { [] }

DeclToken
  : "," { [] }
  | "[" "]" { [] }
  | "const" { [] }
  | "var" { [] }
  | PartSelectP { [] }
  | IdentifierP { [] }
  | DirectionP { [] }
  | LHSConcatP { [] }
  | LHSStreamP { [] }
  | PartialTypeP { [] }
  | NetTypeP Strength { [] }
  | PortBindingsP { [] }
  | SigningP { [] }
  | "[" Expr "]" { [] }
  | "." Identifier { [] }
  | "automatic" { [] }
  | IncOrDecOperatorP { [] }
  | IdentifierP COLON_COLON Identifier { [] }
  | IdentifierP ParamBindings COLON_COLON Identifier { [] }

DTDelim(delim)
  : delim { [] }

DeclTokenAsgn
  : EQUALS DelayOrEvent Expr { [] }
  | EQUALS Expr { [] }
  | "<=" OptDelayOrEvent Expr { [] }
  | AsgnBinOpP Expr { [] }

PortDeclTokens(delim)
  : DeclTokensBase(PortDeclTokens(delim), delim) { TLIST $1 }
  | GenericInterfaceDecl PortDeclTokens(delim) { TUPLE2($1, $2) }
  | GenericInterfaceDecl DTDelim(delim) { $1 }
  | AttributeInstanceP PortDeclTokens(delim) { TUPLE2($1, $2) }

ModuleDeclTokens(delim)
  : DeclTokensBase(ModuleDeclTokens(delim), delim) { [] }
  | GenericInterfaceDecl ModuleDeclTokens(delim) { [] }
  | GenericInterfaceDecl DTDelim(delim) { [] }

GenericInterfaceDecl
  : "interface" IdentifierP { TUPLE2(Interface, $2) }

ParamDeclTokens(delim)
  : DeclTokensBase(ParamDeclTokens(delim), delim) { [] }
  | DeclTokenAsgn "," DTDelim(delim) { [] }
  | ParamDeclToken ParamDeclTokens(delim) { [] }
  | ParamDeclToken DTDelim(delim) { [] }

ParamDeclToken
  : EQUALS PartialTypeP { [] }
  | "type" IdentifierP { [] }
  | ParameterDeclKW { [] }

VariablePortIdentifiers
  : VariablePortIdentifier { [] }
  | VariablePortIdentifiers "," VariablePortIdentifier { [] }

VariablePortIdentifier
  : Identifier OptAsgn { [] }

Direction
  : DirectionP { [] }

DirectionP
  : "inout" { [] }
  | "input" { [] }
  | "output" { [] }

ModuleItems
  : { [] }
  | SEMICOLON ModuleItems { $2 }
  | MITrace ModuleItem ModuleItems { $2 :: $3 }

ModuleItem
  : NonGenerateModuleItem { $1 }
  | ConditionalGenerateConstruct { TLIST $1 }
  | LoopGenerateConstruct { $1 }
  | "generate" GenItems endgenerate { TLIST $2 }

NonGenerateModuleItem
  : ModuleDeclTokens(SEMICOLON) { TLIST [] }
  | ParameterDecl(SEMICOLON) { TLIST [] }
  | "defparam" LHSAsgns SEMICOLON { TLIST [] }
  | "assign" AssignOption LHSAsgns SEMICOLON { TLIST [] }
  | AlwaysKW Stmt { TLIST [] }
  | "initial" Stmt { TLIST [] }
  | "final" Stmt { TLIST [] }
  | "genvar" Identifiers SEMICOLON { TLIST [] }
  | "modport" ModportItems SEMICOLON { TLIST [] }
  | NonDeclPackageItem { TLIST [] }
  | TaskOrFunction { TLIST [] }
  | NInputGateKW NInputGates SEMICOLON { TLIST [] }
  | NOutputGateKW NOutputGates SEMICOLON { TLIST [] }
  | AttributeInstance ModuleItem { TLIST [] }
  | AssertionItem { TLIST [] }

AssignOption
  : { [] }
  | DelayControl { [] }
  | DriveStrength { [] }

AssertionItem
  : ConcurrentAssertionItem { [] }

ProceduralAssertionStatement
  : ConcurrentAssertionStatement { [] }
  | ImmediateAssertionStatement { [] }

ConcurrentAssertionItem
  : Identifier ":" ConcurrentAssertionStatement { [] }
  | ConcurrentAssertionStatement { [] }

ConcurrentAssertionStatement
  : "assert" "property" LPAREN PropertySpec RPAREN ActionBlock { [] }
  | "assume" "property" LPAREN PropertySpec RPAREN ActionBlock { [] }
  | "cover" "property" LPAREN PropertySpec RPAREN Stmt { [] }

ImmediateAssertionStatement
  : "assert" Deferral LPAREN Expr RPAREN ActionBlock { [] }
  | "assume" Deferral LPAREN Expr RPAREN ActionBlock { [] }
  | "cover" Deferral LPAREN Expr RPAREN Stmt { [] }

Deferral
  : { [] }
  | "#" Number { [] }
  | "final" { [] }

PropertySpec
  : OptClockingEvent "disable" "iff" LPAREN Expr RPAREN PropExpr { [] }
  | OptClockingEvent PropExpr { [] }

OptClockingEvent
  : ClockingEvent { [] }
  | { [] }

PropExpr
  : SeqExpr { [] }
  | PropExprParens { [] }

PropExprParens
  : LPAREN PropExprParens RPAREN { [] }
  | SeqExpr "|->" PropExpr { [] }
  | SeqExpr "|=>" PropExpr { [] }
  | SeqExpr "#-#" PropExpr { [] }
  | SeqExpr "#=#" PropExpr { [] }
  | PropExpr "iff" PropExpr { [] }

SeqExpr
  : Expr { [] }
  | SeqExprParens { [] }

SeqExprParens
  : LPAREN SeqExprParens RPAREN { [] }
  | SeqExpr "and" SeqExpr { [] }
  | SeqExpr "or" SeqExpr { [] }
  | SeqExpr "intersect" SeqExpr { [] }
  | Expr "throughout" SeqExpr { [] }
  | SeqExpr "within" SeqExpr { [] }
  | SeqExpr "##" Number SeqExpr { [] }
  | "##" Number SeqExpr { [] }
  | "first_match" LPAREN SeqExpr SeqMatchItems RPAREN { [] }

SeqMatchItems
  : "," SeqMatchItem { [] }
  | SeqMatchItems "," SeqMatchItem { [] }

SeqMatchItem
  : ForStepAssignment { [] }
  | Identifier CallArgs { [] }

ActionBlock
  : Stmt %prec NoElse { [] }
  | "else" Stmt { [] }
  | Stmt "else" Stmt { [] }

AttributeInstances
  : { [] }
  | AttributeInstance AttributeInstances { $1 :: $2 }

AttributeInstance
  : AttributeInstanceP { $1 }

AttributeInstanceP
  : "(*" AttrSpecs "*)" { TLIST $2 }

AttrSpecs
  : AttrSpec { [ $1 ] }
  | AttrSpecs "," AttrSpec { $3 :: $1 }

AttrSpec
  : Identifier OptAsgn { TUPLE2($1,TLIST $2) }

NInputGates
  : NInputGate { [] }
  | NInputGates "," NInputGate { [] }

NOutputGates
  : NOutputGate { [] }
  | NOutputGates "," NOutputGate { [] }

NInputGate
  : DelayControlOrNil OptIdentifier LPAREN LHS "," Exprs RPAREN { [] }

NOutputGate
  : DelayControlOrNil OptIdentifier LPAREN Exprs "," Expr RPAREN { [] }

DelayControlOrNil
  : DelayControl { [] }
  | { [] }

OptIdentifier
  : Identifier { [] }
  | { [] }

NInputGateKW
  : "and" { [] }
  | "nand" { [] }
  | "or" { [] }
  | "nor" { [] }
  | "xor" { [] }
  | "xnor" { [] }

NOutputGateKW
  : "buf" { [] }
  | "not" { [] }

DriveStrength
  : LPAREN Strength0 "," Strength1 RPAREN { [] }
  | LPAREN Strength1 "," Strength0 RPAREN { [] }
  | LPAREN Strength0 "," "highz1" RPAREN { [] }
  | LPAREN Strength1 "," "highz0" RPAREN { [] }
  | LPAREN "highz0" "," Strength1 RPAREN { [] }
  | LPAREN "highz1" "," Strength0 RPAREN { [] }

Strength0
  : "supply0" { [] }
  | "strong0" { [] }
  | "pull0" { [] }
  | "weak0" { [] }

Strength1
  : "supply1" { [] }
  | "strong1" { [] }
  | "pull1" { [] }
  | "weak1" { [] }

ChargeStrength
  : LPAREN "small" RPAREN { [] }
  | LPAREN "medium" RPAREN { [] }
  | LPAREN "large" RPAREN { [] }

LHSAsgns
  : LHSAsgn { [] }
  | LHSAsgns "," LHSAsgn { [] }

LHSAsgn
  : LHS EQUALS Expr { [] }

PackageItems
  : { [] }
  | SEMICOLON PackageItems { [] }
  | PITrace PackageItem PackageItems { [] }

PackageItem
  : PackageOrClassItem { [] }
  | TaskOrFunction { [] }

ClassItems
  : { [] }
  | SEMICOLON ClassItems { [] }
  | CITrace ClassItem ClassItems { [] }

ClassItem
  : ClassItemQualifier PackageOrClassItem { [] }
  | ClassItemQualifier TaskOrFunction { [] }

ClassItemQualifier
  : { [] }
  | "static" { [] }
  | "local" { [] }
  | "protected" { [] }

PackageOrClassItem
  : DeclTokens(SEMICOLON) { [] }
  | ParameterDecl(SEMICOLON) { [] }
  | NonDeclPackageItem { [] }

NonDeclPackageItem
  : Typedef { [] }
  | ImportOrExport { [] }
  | ForwardTypedef SEMICOLON { [] }
  | TimeunitsDeclaration { [] }
  | Directive { [] }

ImportOrExport
  : "import" PackageImportItems SEMICOLON { [] }
  | "export" PackageImportItems SEMICOLON { [] }
  | "export" "*" COLON_COLON "*" SEMICOLON { [] }

TaskOrFunction
  : "function" Lifetime FuncRetAndName TFItems DeclsAndStmts endfunction StrTag { [] }
  | "function" Lifetime "void" Identifier TFItems DeclsAndStmts endfunction StrTag { [] }
  | "task" Lifetime Identifier TFItems DeclsAndStmts endtask StrTag { [] }

Typedef
  : "typedef" Type Identifier SEMICOLON { [] }
  | "typedef" Type Identifier DimensionsNonEmpty SEMICOLON { [] }
  | "typedef" TypedefRef Identifier SEMICOLON { [] }

TypedefRef
  : Identifier "." Identifier { [] }
  | Identifier "[" Expr "]" "." Identifier { [] }

ForwardTypedef
  : "typedef" Identifier { [] }
  | "typedef" "enum" Identifier { [] }
  | "typedef" "struct" Identifier { [] }
  | "typedef" "union" Identifier { [] }

TimeunitsDeclaration
  : "timeunit" Time SEMICOLON { [] }
  | "timeunit" Time "/" Time SEMICOLON { [] }
  | "timeprecision" Time SEMICOLON { [] }

Directive
  : "`celldefine" { [] }
  | "`endcelldefine" { [] }
  | "`unconnected_drive" Drive { [] }
  | "`nounconnected_drive" { [] }
  | "`default_nettype" DefaultNetType { [] }
  | "`resetall" { [] }

Drive
  : "pull0" { [] }
  | "pull1" { [] }

DefaultNetType
  : NetTypeP { [] }
  | Identifier { [] }

PackageImportItems
  : PackageImportItem { [] }
  | PackageImportItems "," PackageImportItem { [] }

PackageImportItem
  : Identifier COLON_COLON Identifier { [] }
  | Identifier COLON_COLON "*" { [] }

FuncRetAndName
  : Type Identifier { [] }
  | Identifier { [] }
  | Signing Identifier { [] }
  | DimensionsNonEmpty Identifier { [] }
  | Signing DimensionsNonEmpty Identifier { [] }

AlwaysKW
  : "always" { [] }
  | "always_comb" { [] }
  | "always_ff" { [] }
  | "always_latch" { [] }

Lifetime
  : { [] }
  | ExplicitLifetime { [ $1 ] }

ExplicitLifetime
  : "static" { Static }
  | "automatic" { Automatic }

TFItems
  : LPAREN DeclTokens(RPAREN) SEMICOLON { [] }
  | LPAREN RPAREN SEMICOLON { [] }
  | SEMICOLON { [] }

ParamType
  : PartialType OptSigning Dimensions { [] }
  | DimensionsNonEmpty { [] }
  | Signing Dimensions { [] }

Dimensions
  : { [] }
  | DimensionsNonEmpty { [] }

DimensionsNonEmpty
  : Dimension { [] }
  | DimensionsNonEmpty Dimension { [] }

Dimension
  : Range { [] }
  | "[" Expr "]" { [] }

DeclAsgns
  : DeclAsgn { [] }
  | DeclAsgns "," DeclAsgn { [] }

DeclAsgn
  : Identifier Dimensions OptAsgn { [] }

Range
  : "[" Expr ":" Expr "]" { [] }

PartSelectP
  : "[" Expr ":" Expr "]" { [] }
  | "[" Expr "+:" Expr "]" { [] }
  | "[" Expr "-:" Expr "]" { [] }

LHS
  : Identifier { [] }
  | LHS PartSelectP { [] }
  | LHS "[" Expr "]" { [] }
  | LHS "." Identifier { [] }
  | LHSConcatP { [] }
  | LHSStreamP { [] }

LHSStreamP
  : LBRACE StreamOp StreamSize Concat RBRACE { [] }
  | LBRACE StreamOp Concat RBRACE { [] }

LHSConcatP
  : LBRACE LHSs RBRACE { [] }

LHSs
  : LHS { [] }
  | LHSs "," LHS { [] }

PortBindingsP
  : LPAREN PortBindingsInside RPAREN { [] }

PortBindingsInside
  : OptPortBinding { [] }
  | OptPortBinding "," PortBindingsInside { [] }

OptPortBinding
  : { [] }
  | PortBinding { [] }

PortBinding
  : "." Identifier LPAREN ExprOrNil RPAREN { [] }
  | "." Identifier { [] }
  | Expr { [] }
  | ".*" { [] }

ParamBindings
  : "#" LPAREN RPAREN { [] }
  | "#" LPAREN ParamBindingsInside RPAREN { [] }

ParamBindingsInside
  : ParamBinding opt(",") { [] }
  | ParamBinding "," ParamBindingsInside { [] }

ParamBinding
  : "." Identifier LPAREN TypeOrExpr RPAREN { [] }
  | "." Identifier LPAREN RPAREN { [] }
  | TypeOrExpr { [] }

Stmts
  : { [] }
  | Stmts Stmt { [] }

Stmt
  : StmtTrace StmtAsgn { [] }
  | StmtTrace StmtNonAsgn { [] }

StmtAsgn
  : LHS EQUALS OptDelayOrEvent Expr SEMICOLON { [] }
  | LHS "<=" OptDelayOrEvent Expr SEMICOLON { [] }
  | LHS AsgnBinOp Expr SEMICOLON { [] }
  | LHS IncOrDecOperator SEMICOLON { [] }
  | IncOrDecOperator LHS SEMICOLON { [] }
  | LHS SEMICOLON { [] }
  | LHS CallArgs SEMICOLON { [] }
  | Identifier COLON_COLON Identifier SEMICOLON { [] }
  | Identifier COLON_COLON Identifier CallArgs SEMICOLON { [] }
  | Identifier ParamBindings COLON_COLON Identifier SEMICOLON { [] }
  | Identifier ParamBindings COLON_COLON Identifier CallArgs SEMICOLON { [] }

StmtNonAsgn
  : StmtBlock(BlockKWSeq, end_rule) { [] }
  | StmtBlock(BlockKWPar, join) { [] }
  | StmtNonBlock { [] }
  | Identifier ":" StmtNonBlock { [] }

StmtBlock(begin_rule,end_rule)
  : begin_rule StrTag DeclsAndStmts end_rule StrTag { [] }
  | Identifier ":" begin_rule DeclsAndStmts end_rule StrTag { [] }

StmtNonBlock
  : SEMICOLON { [] }
  | Unique "if" LPAREN Expr RPAREN Stmt "else" Stmt { [] }
  | Unique "if" LPAREN Expr RPAREN Stmt %prec NoElse { [] }
  | "for" LPAREN ForInit ForCond ForStep RPAREN Stmt { [] }
  | CaseStmt { [] }
  | TimingControl Stmt { [] }
  | "return" ExprOrNil SEMICOLON { [] }
  | "break" SEMICOLON { [] }
  | "continue" SEMICOLON { [] }
  | "while" LPAREN Expr RPAREN Stmt { [] }
  | "repeat" LPAREN Expr RPAREN Stmt { [] }
  | "do" Stmt "while" LPAREN Expr RPAREN SEMICOLON { [] }
  | "forever" Stmt { [] }
  | "foreach" LPAREN Identifier IdxVars RPAREN Stmt { [] }
  | "->" Identifier SEMICOLON { [] }
  | "->>" Identifier SEMICOLON { [] }
  | AttributeInstance Stmt { [] }
  | ProceduralAssertionStatement { [] }
  | "void" "'" LPAREN Expr CallArgs RPAREN SEMICOLON { [] }

OptDelayOrEvent
  : DelayOrEvent { [] }
  | { [] }

CaseStmt
  : Unique CaseKW LPAREN Expr RPAREN Cases "endcase" { [] }
  | Unique CaseKW LPAREN Expr RPAREN "inside" InsideCases "endcase" { [] }

BlockKWPar
  : "fork" { [] }

BlockKWSeq
  : "begin" { [] }

Unique
  : { [] }
  | "unique" { [] }
  | "unique0" { [] }
  | "priority" { [] }

ForInit
  : SEMICOLON { [] }
  | DeclTokens(SEMICOLON) { [] }

ForCond
  : SEMICOLON { [] }
  | Expr SEMICOLON { [] }

ForStep
  : { [] }
  | ForStepNonEmpty { [] }

ForStepNonEmpty
  : ForStepAssignment { [] }
  | ForStepNonEmpty "," ForStepAssignment { [] }

ForStepAssignment
  : LHS AsgnOp Expr { [] }
  | IncOrDecOperator LHS { [] }
  | LHS IncOrDecOperator { [] }

IdxVars
  : "[" IdxVarsInside "]" { [] }

IdxVarsInside
  : IdxVar { [] }
  | IdxVar "," IdxVarsInside { [] }

IdxVar
  : { [] }
  | Identifier { [] }

DeclsAndStmts
  : StmtTrace DeclOrStmt DeclsAndStmts { [] }
  | StmtTrace StmtNonAsgn Stmts { [] }
  | StmtTrace { [] }

DeclOrStmt
  : DeclTokens(SEMICOLON) { [] }
  | ParameterDecl(SEMICOLON) { [] }

ParameterDecl(delim)
  : ParameterDeclKW DeclAsgns delim { [] }
  | ParameterDeclKW ParamType DeclAsgns delim { [] }
  | ParameterDeclKW TypeAlias DeclAsgns delim { [] }
  | ParameterDeclKW "type" TypeAsgns delim { [] }

ParameterDeclKW
  : "parameter" { [] }
  | "localparam" { [] }

TypeAsgns
  : TypeAsgn { [] }
  | TypeAsgns "," TypeAsgn { [] }

TypeAsgn
  : Identifier EQUALS Type { [] }
  | Identifier { [] }

ClockingEvent
  : "@" LPAREN Senses RPAREN { [] }

TimingControl
  : DelayOrEvent { [] }
  | CycleDelay { [] }

DelayOrEvent
  : DelayControl
  | EventControl { [] }

DelayControl
  : "#" Number { [] }
  | "#" Real { [] }
  | "#" Time { [] }
  | "#" LPAREN Expr RPAREN { [] }
  | "#" LPAREN Expr ":" Expr ":" Expr RPAREN { [] }
  | "#" Identifier { [] }
  | "#" Identifier COLON_COLON Identifier { [] }
  | "#" Identifier ParamBindings COLON_COLON Identifier { [] }

CycleDelay
  : "##" Expr { [] }

EventControl
  : "@" LPAREN Senses RPAREN { [] }
  | "@" "(*)" { [] }
  | "@" LPAREN "*" RPAREN { [] }
  | "@" "(*" RPAREN { [] }
  | "@" LPAREN "*)" { [] }
  | "@" "*" { [] }
  | "@*" { [] }
  | "@" Identifier { [] }

Senses
  : Sense { [] }
  | Senses "or" Sense { [] }
  | Senses "," Sense { [] }

Sense
  : LPAREN Sense RPAREN { [] }
  | LHS { [] }
  | "posedge" LHS { [] }
  | "negedge" LHS { [] }
  | "posedge" LPAREN LHS RPAREN { [] }
  | "negedge" LPAREN LHS RPAREN { [] }

CaseKW
  : "case" { [] }
  | "casex" { [] }
  | "casez" { [] }

Cases
  : Case { [] }
  | Case Cases { [] }

Case
  : Exprs ":" Stmt
  | "default" opt(":") Stmt { [] }

InsideCases
  : InsideCase { [] }
  | InsideCase InsideCases { [] }

InsideCase
  : OpenRangeList ":" Stmt { [] }
  | "default" opt(":") Stmt { [] }

(*
Real
  : real { [] }

Number
  : number { [] }

Time
  : time { [] }
*)

CallArgs
  : LPAREN CallArgsInside RPAREN { TLIST $2 }

CallArgsInside
  : { [] }
  | NamedCallArgsFollow { [] }
  | Expr NamedCallArgs { [] }
  | UnnamedCallArgs NamedCallArgs { [] }
  | Expr UnnamedCallArgs NamedCallArgs { [] }

UnnamedCallArgs
  : "," ExprOrNil { [] }
  | UnnamedCallArgs "," ExprOrNil { [] }

NamedCallArgs
  : { [] }
  | "," NamedCallArgsFollow { [] }

NamedCallArgsFollow
  : NamedCallArg { [] }
  | NamedCallArgsFollow "," NamedCallArg { [] }

NamedCallArg
  : "." Identifier LPAREN ExprOrNil RPAREN { [] }

Exprs
  : Expr { [] }
  | Exprs "," Expr { [] }

TypeOrExpr
  : TypeNonIdent { [] }
  | Expr { [] }

OpenRangeList
  : ValueRange { [] }
  | OpenRangeList "," ValueRange { [] }

ValueRange
  : Expr { [] }
  | Range { [] }

Expr
  : LPAREN Expr RPAREN { $2 }
  | STRING { STRING $1 }
  | Real { Real $1 }
  | Number { Number $1 }
  | Time { Time $1 }
  | Expr CallArgs { TUPLE2($1,$2) }
  | DimsFn LPAREN TypeOrExpr RPAREN { TLIST [] }
  | DimFn LPAREN TypeOrExpr RPAREN { TLIST [] }
  | DimFn LPAREN TypeOrExpr "," Expr RPAREN { TLIST [] }
  | Expr PartSelectP { TLIST [] }
  | Expr "[" Expr "]" { TLIST [] }
  | LBRACE Expr Concat RBRACE { TLIST [] }
  | Concat { TLIST [] }
  | Expr "?" Expr ":" Expr { TUPLE4(QUERY,$1,$3,$5) }
  | Expr "." Identifier { TUPLE3(DOT,$1,$3) }
  | "'" LBRACE PatternItems RBRACE { TLIST [] }
  | Expr "'" LBRACE PatternItems RBRACE { $1 }
  | CastingType "'" LPAREN Expr RPAREN { TLIST [] }
  | Expr "'" LPAREN Expr RPAREN { TLIST [] }
  | LBRACE StreamOp StreamSize Concat RBRACE { TLIST [] }
  | LBRACE StreamOp Concat RBRACE { TLIST [] }
  | Expr "inside" LBRACE OpenRangeList RBRACE { TLIST [] }
  | LPAREN Expr ":" Expr ":" Expr RPAREN { TLIST [] }
  | Identifier %prec REDUCE_OP { TLIST [] }
  | Identifier COLON_COLON Identifier { TLIST [] }
  | Identifier ParamBindings COLON_COLON Identifier { TLIST [] }
  | Expr "||" Expr { TLIST [] }
  | Expr "&&" Expr { TLIST [] }
  | Expr "->" Expr { TLIST [] }
  | Expr "<->" Expr { TLIST [] }
  | Expr "|" Expr { TLIST [] }
  | Expr "^" Expr { TLIST [] }
  | Expr "&" Expr { TLIST [] }
  | Expr "~^" Expr { TLIST [] }
  | Expr "^~" Expr { TLIST [] }
  | Expr "+" Expr { TLIST [] }
  | Expr "-" Expr { TLIST [] }
  | Expr "*" Expr { TLIST [] }
  | Expr "/" Expr { TLIST [] }
  | Expr "%" Expr { TLIST [] }
  | Expr "**" Expr { TLIST [] }
  | Expr "==" Expr { TLIST [] }
  | Expr "!=" Expr { TLIST [] }
  | Expr "<" Expr { TLIST [] }
  | Expr "<=" Expr { TLIST [] }
  | Expr ">" Expr { TLIST [] }
  | Expr ">=" Expr { TLIST [] }
  | Expr "===" Expr { TLIST [] }
  | Expr "!==" Expr { TLIST [] }
  | Expr "==?" Expr { TLIST [] }
  | Expr "!=?" Expr { TLIST [] }
  | Expr "<<" Expr { TLIST [] }
  | Expr ">>" Expr { TLIST [] }
  | Expr "<<<" Expr { TLIST [] }
  | Expr ">>>" Expr { TLIST [] }
  | "!" Expr { TLIST [] }
  | "~" Expr { TLIST [] }
  | "+" Expr %prec REDUCE_OP { TLIST [] }
  | "-" Expr %prec REDUCE_OP { TLIST [] }
  | "&" Expr %prec REDUCE_OP { TLIST [] }
  | "~&" Expr %prec REDUCE_OP { TLIST [] }
  | "|" Expr %prec REDUCE_OP { TLIST [] }
  | "~|" Expr %prec REDUCE_OP { TLIST [] }
  | "^" Expr %prec REDUCE_OP { TLIST [] }
  | "~^" Expr %prec REDUCE_OP { TLIST [] }
  | "^~" Expr %prec REDUCE_OP { TLIST [] }

ExprOrNil
  : Expr { [] }
  | { [] }

PatternItems
  : PatternNamedItems { [] }
  | PatternUnnamedItems { [] }

PatternNamedItems
  : PatternNamedItem { [] }
  | PatternNamedItems "," PatternNamedItem { [] }

PatternNamedItem
  : PatternName ":" Expr { [] }

PatternName
  : Expr { [] }
  | PartialType { [] }
  | "default" { [] }

PatternUnnamedItems
  : PatternUnnamedItem { [] }
  | PatternUnnamedItems "," PatternUnnamedItem { [] }

PatternUnnamedItem
  : Expr { [] }
  | Expr Concat { [] }

Concat
  : LBRACE Exprs RBRACE { [] }

StreamOp
  : "<<" { [] }
  | ">>" { [] }

StreamSize
  : TypeNonIdent { [] }
  | Expr { [] }

GenItemOrNull
  : GenItem { [] }
  | SEMICOLON { [] }

GenItems
  : { [] }
  | GenItems SEMICOLON { [] }
  | GenItems GenItem { [] }

GenItem
  : MITrace GenBlock { [] }
  | MITrace NonGenerateModuleItem { [] }
  | MITrace "generate" GenItems "endgenerate" { [] }
  | MITrace ConditionalGenerateConstruct { [] }
  | MITrace LoopGenerateConstruct { [] }

ConditionalGenerateConstruct
  : "if" LPAREN Expr RPAREN GenItemOrNull "else" GenItemOrNull { [] }
  | "if" LPAREN Expr RPAREN GenItemOrNull %prec NoElse { [] }
  | "case" LPAREN Expr RPAREN GenCases "endcase" { [] }

LoopGenerateConstruct
  : "for" LPAREN GenvarInitialization SEMICOLON Expr SEMICOLON GenvarIteration RPAREN GenItem { TLIST [] }

GenBlock
  : GenBlockBegin_Rule GenItems end_rule StrTag { [] }

GenBlockBegin_Rule
  : "begin" StrTag { [] }
  | Identifier ":" "begin" { [] }

GenCases
  : GenCase { [] }
  | GenCase GenCases { [] }

GenCase
  : Exprs ":" GenItemOrNull { [] }
  | "default" opt(":") GenItemOrNull { [] }

GenvarInitialization
  : "genvar" Identifier EQUALS Expr { [] }
  | Identifier EQUALS Expr { [] }

GenvarIteration
  : Identifier AsgnOp Expr { [] }
  | IncOrDecOperator Identifier { [] }
  | Identifier IncOrDecOperator { [] }

AsgnOp
  : EQUALS { [] }
  | AsgnBinOp { [] }

AsgnBinOp
  : AsgnBinOpP { [] }

AsgnBinOpP
  : "+=" { [] }
  | "-=" { [] }
  | "*=" { [] }
  | "/=" { [] }
  | "%=" { [] }
  | "&=" { [] }
  | "|=" { [] }
  | "^=" { [] }
  | "<<=" { [] }
  | ">>=" { [] }
  | "<<<=" { [] }
  | ">>>=" { [] }

IncOrDecOperator
  : IncOrDecOperatorP { [] }

IncOrDecOperatorP
  : "++" { [] }
  | "--" { [] }

DimsFn
  : "$bits" { [] }
  | "$dimensions" { [] }
  | "$unpacked_dimensions" { [] }

DimFn
  : "$left" { [] }
  | "$right" { [] }
  | "$low" { [] }
  | "$high" { [] }
  | "$increment" { [] }
  | "$size" { [] }

MITrace
  : PITrace { [] }

PITrace
  : DeclTrace { TLIST [] }

CITrace
  : PITrace { [] }

DeclTrace
  : Trace { [] }

StmtTrace
  : Trace { [] }

Trace
  : position { [] }

position
  : { [] }

begin_rule
  : "begin" { [] }
  | error { [] }

end_rule
  : "end" { [] }
  | error { [] }

endclass
  : "endclass" { [] }
  | error { [] }

endfunction
  : "endfunction" { [] }
  | error { [] }

endgenerate
  : "endgenerate" { [] }
  | error { [] }

endinterface
  : "endinterface" { Endinterface }
  | error { EOF_TOKEN }

endmodule
  : "endmodule" { Endmodule }
  | EOF_TOKEN { EOF_TOKEN }
  | error { EOF_TOKEN }

endpackage
  : "endpackage" { [] }
  | error { [] }

endtask
  : "endtask" { [] }
  | error { [] }

join
  : "join" { [] }
  | error { [] }
