%token CASE DEFAULT IF SWITCH WHILE DO FOR GOTO CONTINUE BREAK RETURN
%token  ADD
%token  AND
%token  ANDR
%token  ASCLOCK
%token  ASSINT
%token  ASUINT
%token  BITS
%token  CAT
%token  CVT
%token  CIRCUIT
%token  CLOCK
%token  DATA_TYPE
%token  DEPTH
%token  DIV
%token  DSHL
%token  DSHR
%token  ELSE
%token  FLIP
%token  EQ
%token  EXTMODULE
%token  GEQ
%token  GT
%token  HEAD
%token  ID
%token  INPUT
%token  INST
%token  INT
%token  INVALID
%token  IS
%token  LEQ
%token  LT
%token  MEM
%token  MOD
%token  MODULE
%token  MUL
%token  MUX
%token  NEG
%token  NEQ
%token  NEW
%token  NODE
%token  NOT
%token  OF
%token  OLD
%token  OR
%token  ORR
%token  OUTPUT
%token  PAD
%token  PRINTF
%token  READER
%token  READ_LATENCY
%token  READ_UNDER_WRITE
%token  READWRITER
%token  REG
%token  SHL
%token  SHR
%token  SKIP
%token  SINT
%token  STOP
%token  STRING
%token  SUB
%token  TAIL
%token  UNDEFINED
%token  UINT
%token  VALIDIF
%token  WHEN
%token  WIRE
%token  WRITE_LATENCY
%token  WRITER
%token  XOR
%token  XORR

%start Circuit

%{
  // #include "firrtl.h"
%}

%%

Circuit:CIRCUIT ID ':' module_lst  // CIRCUIT
Module : MODULE ID ':' port_lst stmt_lst  // Module
|  EXTMODULE ID ':' port_lst  // External Module
port :  dir ID ':' Type // Port
dir : INPUT | OUTPUT // Port Direction
INT_opt : | '<' INT '>'
Type : UINT INT_opt  // Unsigned Integer
| SINT INT_opt  // Signed Integer
| CLOCK // CLOCK
| '{' field_clst '}'  // Bundle
| Type '[' INT ']'  // Vector
field : FLIP_opt ID ':' Type // Bundle Field
| FLIP_opt BITS ':' Type // Bundle Field
| FLIP_opt INT ':' Type // Bundle Field
stmt :  WIRE ID ':' Type // WIRE
|  REG ID ':' Type  ','  exp  '['  exp  ','  exp_opt  // REGISter
|  MEM ID ':' '('  // MEMory
DATA_TYPE '=' '>' Type DEPTH '=' '>' INT READ_LATENCY '=' '>' INT WRITE_LATENCY '=' '>' INT READ_UNDER_WRITE '=' '>' ruw READER '=' '>' ID_lst WRITER '=' '>' ID_lst READWRITER '=' '>' ID_lst  ')'  |  INST ID OF ID // INSTance
|  NODE ID '=' exp // NODE
|  exp '<' '=' exp // Connect
|  exp '<' '-' exp // Partial Connect
|  exp IS INVALID // InvalIDate
|  WHEN exp ':' stmt ELSE_stmt_opt  // Conditional
|  STOP'('  exp  ','  exp  ','  INT  ')'  // STOP
|  PRINTF'('  exp  ','  exp  ','  STRING  ','  exp_lst  ')'  // PrINTf
|  SKIP // Empty
|  '('  stmt_lst  ')'  // Statement Group
ruw : OLD | NEW | UNDEFINED // Read Under Write Flag
info : '@'  '[' ID '.' ID INT ':'  INT  ']'  // File Information Token

exp : UINT INT_opt '('  INT  ')'  // Literal Unsigned Integer
| UINT INT_opt  '('  STRING  ')'  // Literal Unsigned Integer From BITS
| SINT INT_opt  '('  INT  ')'  // Literal Signed Integer
| SINT INT_opt  '('  STRING  ')'  // Literal Signed Integer From BITS
| ID // Reference
| exp '.' ID // SUBfield
| exp '.' INT // SUBfield
| exp  '['  INT  ']'  // SUBindex
| exp  '['  exp  ']'  // SUBaccess
| MUX exp  ','  exp  ','  exp  ')'  // MULTipleXOR (LPAREN grabbed by Lexer)
| VALIDIF '('  exp  ','  exp  ')'  // Conditionally ValID
| primop exp_int_clst ')' // Lexer adds the '(' automatically

primop : ADD // ADD
| SUB // SUBtract
| MUL // MULTiply
| DIV // DIVIDe
| MOD // Modulo
| LT // Less Than
| LEQ // Less or Equal
| GT // Greater Than
| GEQ // Greater or Equal
| EQ // Equal
| NEQ // NOT Equal
| PAD // PAD
| ASUINT // Interpret BITS as UINT
| ASSINT // Interpret BITS as SINT
| ASCLOCK // Interpret as CLOCK
| SHL // Shift Left
| SHR // Shift Right
| DSHL // Dynamic Shift Left
| DSHR // Dynamic Shift Right
| CVT // Arithmetic Convert to Signed
| NEG // NEGate
| NOT // NOT
| AND // And
| OR // Or
| XOR // XOR
| ANDR // And Reduce
| ORR // Or Reduce
| XORR // XOR Reduce
| CAT // ConCATenation
| BITS // Bit Extraction
| HEAD // HEAD
| TAIL // TAIL

ID_lst: ID | ID_lst ID
exp_lst: exp | exp_lst exp
exp_int_clst: exp | INT | exp_int_clst ',' exp | exp_int_clst ',' INT
field_clst: field | field_clst ',' field
INT_lst: INT | INT_lst INT
module_lst: Module | module_lst Module
port_lst: port | port_lst port
stmt_lst: stmt | stmt info | stmt_lst stmt | stmt_lst stmt info
ELSE_stmt_opt: | ELSE ':' stmt
exp_opt: | exp
FLIP_opt: | FLIP
