%token  ADD
%token  AND
%token  ANDR
%token  ASCLOCK
%token  ASSINT
%token  ASUINT
%token  BITS
%token  CAT
%token  CMEM
%token  CVT
%token  CIRCUIT
%token  CLOCK
%token  DATA_TYPE
%token  DEFAULT
%token  DEFNAME
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
%token  IS
%token  LEQ
%token  LT
%token  MEM
%token  MOD
%token  MODULE
%token  MPORT
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
%token  PARAMETER
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
%token  SMEM
%token  STOP
%token  STRING
%token  SUB
%token  TAIL
%token  UNDEFINED
%token  UINT
%token  VALIDIF
%token  WHEN
%token  WIRE
%token  WITH
%token  WRITE_LATENCY
%token  WRITER
%token  XOR
%token  XORR

%start Circuit

%{
 // #include "firrtl.h"
%}

%%

Circuit:CIRCUIT ID ':' module_list // CIRCUIT
Module : MODULE ID ':' port_list stmt_list // Module
| EXTMODULE ID ':' port_list ext_list // External Module
port : dir ID ':' Type // Port
dir : INPUT | OUTPUT // Port Direction
INT_opt : | '<' INT '>'
Type : UINT INT_opt // Unsigned Integer
| SINT INT_opt // Signed Integer
| CLOCK // CLOCK
| '{' fieldc_list_opt '}' // Bundle
| Type '[' INT ']' // Vector
field : FLIP_opt ID ':' Type // Bundle Field
| FLIP_opt BITS ':' Type // Bundle Field
| FLIP_opt INT ':' Type // Bundle Field
| FLIP_opt INST ':' Type // Bundle Field (INST special case)
stmt : WIRE ID ':' Type // WIRE
| REG ID ':' Type ',' exp with_opt // REGISter
| CMEM ID ':' Type // Cmemory
| SMEM ID ':' Type // Smemory
| MEM ID ':' '(' // Memory
DATA_TYPE '=' '>' Type DEPTH '=' '>' INT READ_LATENCY '=' '>' INT WRITE_LATENCY '=' '>' INT READ_UNDER_WRITE '=' '>' ruw READER '=' '>' ID_list WRITER '=' '>' ID_list READWRITER '=' '>' ID_list ')' | INST ID OF ID // Instance
| NODE any '=' exp // Node
| exp '<' '=' exp // Connect
| exp '<' '-' exp // Partial Connect
| exp IS ID // Invalidate
| WHEN exp ':' info_opt stmt ELSE_stmt_opt // Conditional
| STOP exp ',' exp ',' INT ')' // STOP
| PRINTF exp ',' exp ',' STRING exp_list ')' // Printf
| SKIP // Empty
| ID MPORT exp '=' ID '[' exp ']' ',' ID
| '(' stmt_list ')' // Statement Group
ruw : OLD | NEW | UNDEFINED // Read Under Write Flag
info : '@' '[' ID '.' ID INT ':' INT ']' // File Information Token

any : ID | INST // Convert keyword to ID

exp : UINT INT_opt '(' INT ')' // Literal Unsigned Integer
| UINT INT_opt '(' STRING ')' // Literal Unsigned Integer From BITS
| SINT INT_opt '(' INT ')' // Literal Signed Integer
| SINT INT_opt '(' STRING ')' // Literal Signed Integer From BITS
| ID // Reference
| INST // Reference special case
| exp '.' ID // SUBfield
| exp '.' INT // SUBfield
| exp '.' INST // SUBfield INST special case
| exp '[' INT ']' // SUBindex
| exp '[' exp ']' // SUBaccess
| MUX exp ',' exp ',' exp ')' // MULTipleXOR (LPAREN grabbed by Lexer)
| VALIDIF '(' exp ',' exp ')' // Conditionally ValID
| primop exp_intc_list ')' // Lexer adds the '(' automatically

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

ext: DEFNAME '=' ID
| PARAMETER ID '=' STRING
| PARAMETER DEFAULT '=' INT

ID_list: ID | ID_list ID
exp_list: | exp_list ',' exp
exp_intc_list: exp | INT | exp_intc_list ',' exp | exp_intc_list ',' INT
fieldc_list: field | fieldc_list ',' field
module_list: Module | module_list Module
port_list: port | port_list port
stmt_list: stmt | stmt info | stmt_list stmt | stmt_list stmt info
ext_list: ext | ext_list ext
ELSE_stmt_opt: | ELSE ':' stmt
fieldc_list_opt: | fieldc_list
FLIP_opt: | FLIP
with_opt: | WITH ':' '(' ID '=' '>' '(' exp ',' exp ')' ')'
info_opt: | info