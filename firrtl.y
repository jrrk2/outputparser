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

Circuit:info_opt CIRCUIT ID ':' '(' module_lst ')'  // CIRCUIT
Module : info_opt MODULE ID ':' '('  port_lst stmt  ')'  // Module
| info_opt  EXTMODULE ID ':' '('  port_lst  ')'  // External Module
port : info_opt  dir ID ':' Type // Port
dir : INPUT | OUTPUT // Port Direction
INT_opt : | '<' INT '>'
Type : UINT INT_opt  // Unsigned INTeger
| SINT INT_opt  // Signed INTeger
| CLOCK // CLOCK
| '{' field_lst '}'  // Bundle
| Type '[' INT ']'  // Vector
field : FLIP_opt ID ':' Type // Bundle Field
stmt : info_opt  WIRE ID ':' Type // WIRE
| info_opt  REG ID ':' Type  ','  exp  '['  exp  ','  exp_opt  // REGISter
| info_opt  MEM ID ':' '('  // MEMory
DATA_TYPE '=' '>' Type DEPTH '=' '>' INT READ_LATENCY '=' '>' INT WRITE_LATENCY '=' '>' INT READ_UNDER_WRITE '=' '>' ruw READER '=' '>' ID_lst WRITER '=' '>' ID_lst READWRITER '=' '>' ID_lst  ')'  | info_opt  INST ID OF ID // INSTance
| info_opt  NODE ID '=' exp // NODE
| info_opt  exp '<' '=' exp // Connect
| info_opt  exp '<' '-' exp // Partial Connect
| info_opt  exp IS INVALID // InvalIDate
| info_opt  WHEN exp ':' stmt ELSE_stmt_opt  // Conditional
| info_opt  STOP'('  exp  ','  exp  ','  INT  ')'  // STOP
| info_opt  PRINTF'('  exp  ','  exp  ','  STRING  ','  exp_lst  ')'  // PrINTf
| info_opt  SKIP // Empty
| info_opt  '('  stmt_lst  ')'  // Statement Group
ruw : OLD | NEW | UNDEFINED // Read Under Write Flag
info : '@'  '['  STRING  ','  INT  ','  INT  ']'  // File Information Token

exp : UINT INT_opt '('  INT  ')'  // Literal Unsigned INTeger
| UINT INT_opt  '('  STRING  ')'  // Literal Unsigned INTeger From BITS
| SINT INT_opt  '('  INT  ')'  // Literal Signed INTeger
| SINT INT_opt  '('  STRING  ')'  // Literal Signed INTeger From BITS
| ID // Reference
| exp '.' ID // SUBfield
| exp  '['  INT  ']'  // SUBindex
| exp  '['  exp  ']'  // SUBaccess
| MUX '('  exp  ','  exp  ','  exp  ')'  // MULTipleXOR
| VALIDIF '('  exp  ','  exp  ')'  // Conditionally ValID
| primop '('  exp_lst  ','  INT_lst  ')' 

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
| ASUINT // INTerpret BITS as UINT
| ASSINT // INTerpret BITS as SINT
| ASCLOCK // INTerpret as CLOCK
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
field_lst: field | field_lst field
INT_lst: INT | INT_lst INT
module_lst: Module | module_lst Module
port_lst: port | port_lst port
stmt_lst: stmt | stmt_lst stmt

info_opt: | info
ELSE_stmt_opt: | ELSE ':' stmt
exp_opt: | exp
FLIP_opt: | FLIP
