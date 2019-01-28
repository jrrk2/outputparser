%{

open Parsing

(*
  open Source_text_types_edited
  let declst = ref []
  let typehash_add id_t = Hashtbl.add typehash id_t ()
*)

%}

%token  ACCEPT
%token  ALWAYS
%token  ALWAYS_UNDERSCORE_COMB
%token  ALWAYS_UNDERSCORE_FF
%token  ALWAYS_UNDERSCORE_LATCH
%token  AMPERSAND
%token  AND
%token  ASSERT
%token  ASSIGN
%token  ASSUME
%token  AT
%token  AUTOMATIC
%token  BACKQUOTE
%token  BACKSLASH
%token  BEGIN
%token  BIND
%token  BIT
%token  BREAK
%token  BUF
%token  BUFIF0
%token  BUFIF1
%token  BYTE
%token  CARET
%token  CASE
%token  CASEX
%token  CASEZ
%token  CHANDLE
%token  CLOCKING
%token  CMOS
%token  COLON
%token  COMMA
%token <token> CONS1
%token <token*token> CONS2
%token <token*token*token> CONS3
%token <token*token*token*token> CONS4
%token  CONST
%token  CONST_HYPHEN_IN_HYPHEN_LEX
%token  CONST_HYPHEN_THEN_HYPHEN_REF
%token  CONTEXT
%token  CONTINUE
%token  COVER
%token  COVERAGE_UNDERSCORE_OFF
%token  COVERAGE_UNDERSCORE_ON
%token  DEASSIGN
%token  DEFAULT
%token  DEFPARAM
%token  DISABLE
%token  DO
%token  DOLLAR
%token  DOLLAR_END
%token  DOT
%token  DOUBLEQUOTE
%token  EDGE
%token  ELSE
%token  EMPTY_TOKEN
%token  END
%token  ENDCASE
%token  ENDCLOCKING
%token  ENDFUNCTION
%token  ENDGENERATE
%token  ENDINTERFACE
%token  ENDMODULE
%token  ENDPACKAGE
%token  ENDPRIMITIVE
%token  ENDPROGRAM
%token  ENDPROPERTY
%token  ENDSPECIFY
%token  ENDTABLE
%token  ENDTASK
%token  ENUM
%token  EOF_TOKEN
%token  EQUALS
%token  ERROR
%token  ERROR_TOKEN
%token  EXPORT
%token  EXTERN
%token  FINAL
%token  FLOATING_HYPHEN_POINT_BLANK_NUMBER
%token  FOR
%token  FOREACH
%token  FOREVER
%token  FORKJOIN
%token  FUNCTION
%token  GENERATE
%token  GENVAR
%token  GLOBAL_HYPHEN_IN_HYPHEN_LEX
%token  GLOBAL_HYPHEN_THEN_HYPHEN_CLOCKING
%token  GREATER
%token  HASH
%token  HYPHEN
%token <string> IDENTIFIER
%token <string> NUMBER
%token  IDENTIFIER_HYPHEN_IN_HYPHEN_LEX
%token  IF
%token  IFF
%token  IMPORT
%token  INITIAL
%token  INOUT
%token  INPUT
%token  INSIDE
%token  INT
%token  INTEGER
%token  INTEGER_BLANK_NUMBER
%token  INTERFACE
%token  LBRACE
%token  LBRACK
%token  LESS
%token  LINEFEED
%token  LINT_UNDERSCORE_OFF
%token  LINT_UNDERSCORE_ON
%token  LOCALPARAM
%token  LOGIC
%token  LONGINT
%token  LPAREN
%token  MODPORT
%token  MODULE
%token  NAND
%token  NEGEDGE
%token  NMOS
%token  NOR
%token  NOT
%token  NOTIF0
%token  NOTIF1
%token  OR
%token  OUTPUT
%token  PACKAGE
%token  PACKAGE_HYPHEN_IDENTIFIER
%token  PACKED
%token  PARAMETER
%token  PERCENT
%token  PLING
%token  PLUS
%token  PMOS
%token  POSEDGE
%token  PRIMITIVE
%token  PRIORITY
%token  PRLOWER_THAN_ELSE
%token  PRNEGATION
%token  PROGRAM
%token  PROPERTY
%token  PRREDUCTION
%token  PRUNARYARITH
%token  PULLDOWN
%token  PULLUP
%token  PURE
%token  QUERY
%token  QUOTE
%token  Q_AMPERSAND__AMPERSAND_
%token  Q_AMPERSAND__AMPERSAND__AMPERSAND_
%token  Q_AMPERSAND__EQUALS_
%token  Q_AT__AT_
%token  Q_BACKQUOTE_SYSTEMC_UNDERSCORE_CTOR_BLANK_BLOCK
%token  Q_BACKQUOTE_SYSTEMC_UNDERSCORE_DTOR_BLANK_BLOCK
%token  Q_BACKQUOTE_SYSTEMC_UNDERSCORE_HEADER_BLANK_BLOCK
%token  Q_BACKQUOTE_SYSTEMC_UNDERSCORE_IMPLEMENTATION_BLANK_BLOCK
%token  Q_BACKQUOTE_SYSTEMC_UNDERSCORE_IMP_UNDERSCORE_HEADER_BLANK_BLOCK
%token  Q_BACKQUOTE_SYSTEMC_UNDERSCORE_INTERFACE_BLANK_BLOCK
%token  Q_CARET__EQUALS_
%token  Q_CARET__TILDE_
%token  Q_COLON__COLON_
%token  Q_COLON__EQUALS_
%token  Q_COLON__SLASH_
%token  Q_DOLLAR_ACOS
%token  Q_DOLLAR_ACOSH
%token  Q_DOLLAR_ASIN
%token  Q_DOLLAR_ASINH
%token  Q_DOLLAR_ATAN
%token  Q_DOLLAR_ATAN2
%token  Q_DOLLAR_ATANH
%token  Q_DOLLAR_BITS
%token  Q_DOLLAR_BITSTOREAL
%token  Q_DOLLAR_C
%token  Q_DOLLAR_CEIL
%token  Q_DOLLAR_CLOG2
%token  Q_DOLLAR_COS
%token  Q_DOLLAR_COSH
%token  Q_DOLLAR_COUNTONES
%token  Q_DOLLAR_DIMENSIONS
%token  Q_DOLLAR_DISPLAY
%token  Q_DOLLAR_ERROR
%token  Q_DOLLAR_EXP
%token  Q_DOLLAR_FATAL
%token  Q_DOLLAR_FCLOSE
%token  Q_DOLLAR_FDISPLAY
%token  Q_DOLLAR_FEOF
%token  Q_DOLLAR_FFLUSH
%token  Q_DOLLAR_FGETC
%token  Q_DOLLAR_FGETS
%token  Q_DOLLAR_FINISH
%token  Q_DOLLAR_FLOOR
%token  Q_DOLLAR_FOPEN
%token  Q_DOLLAR_FSCANF
%token  Q_DOLLAR_FWRITE
%token  Q_DOLLAR_HIGH
%token  Q_DOLLAR_HYPOT
%token  Q_DOLLAR_INCREMENT
%token  Q_DOLLAR_INFO
%token  Q_DOLLAR_ISUNKNOWN
%token  Q_DOLLAR_ITOR
%token  Q_DOLLAR_LEFT
%token  Q_DOLLAR_LN
%token  Q_DOLLAR_LOG10
%token  Q_DOLLAR_LOW
%token  Q_DOLLAR_ONEHOT
%token  Q_DOLLAR_ONEHOT0
%token  Q_DOLLAR_PAST
%token  Q_DOLLAR_POW
%token  Q_DOLLAR_RANDOM
%token  Q_DOLLAR_READMEMB
%token  Q_DOLLAR_READMEMH
%token  Q_DOLLAR_REALTIME
%token  Q_DOLLAR_REALTOBITS
%token  Q_DOLLAR_RIGHT
%token  Q_DOLLAR_RTOI
%token  Q_DOLLAR_SFORMAT
%token  Q_DOLLAR_SFORMATF
%token  Q_DOLLAR_SIGNED
%token  Q_DOLLAR_SIN
%token  Q_DOLLAR_SINH
%token  Q_DOLLAR_SIZE
%token  Q_DOLLAR_SQRT
%token  Q_DOLLAR_SSCANF
%token  Q_DOLLAR_STIME
%token  Q_DOLLAR_STOP
%token  Q_DOLLAR_SWRITE
%token  Q_DOLLAR_SYSTEM
%token  Q_DOLLAR_TAN
%token  Q_DOLLAR_TANH
%token  Q_DOLLAR_TEST_DOLLAR_PLUSARGS
%token  Q_DOLLAR_TIME
%token  Q_DOLLAR_UNIT
%token  Q_DOLLAR_UNPACKED_UNDERSCORE_DIMENSIONS
%token  Q_DOLLAR_UNSIGNED
%token  Q_DOLLAR_VALUE_DOLLAR_PLUSARGS
%token  Q_DOLLAR_WARNING
%token  Q_DOLLAR_WRITE
%token  Q_DOLLAR_WRITEMEMH
%token  Q_DOLLAR__LBRACE_DPI_HYPHEN_SYS_RBRACE_
%token  Q_DOLLAR__LBRACE_IGNORED_HYPHEN_BBOX_HYPHEN_SYS_RBRACE_
%token  Q_DOT__STAR_
%token  Q_EQUALS__EQUALS_
%token  Q_EQUALS__EQUALS__EQUALS_
%token  Q_EQUALS__EQUALS__QUERY_
%token  Q_EQUALS__GREATER_
%token  Q_GREATER__EQUALS_
%token  Q_GREATER__GREATER_
%token  Q_GREATER__GREATER__EQUALS_
%token  Q_GREATER__GREATER__GREATER_
%token  Q_GREATER__GREATER__GREATER__EQUALS_
%token  Q_HASH__HASH_
%token  Q_HYPHEN__COLON_
%token  Q_HYPHEN__EQUALS_
%token  Q_HYPHEN__GREATER_
%token  Q_HYPHEN__GREATER__GREATER_
%token  Q_HYPHEN__HYPHEN_
%token  Q_HYPHEN__HYPHEN_FILE
%token  Q_HYPHEN__HYPHEN_LINES
%token  Q_HYPHEN__HYPHEN_MSG
%token  Q_LBRACK__EQUALS_
%token  Q_LBRACK__HYPHEN__GREATER_
%token  Q_LBRACK__STAR_
%token  Q_LESS__EQUALS_
%token  Q_LESS__EQUALS__HYPHEN_IGNORED
%token  Q_LESS__LESS_
%token  Q_LESS__LESS__EQUALS_
%token  Q_PERCENT__EQUALS_
%token  Q_PLING__EQUALS_
%token  Q_PLING__EQUALS__EQUALS_
%token  Q_PLING__EQUALS__QUERY_
%token  Q_PLUS__COLON_
%token  Q_PLUS__EQUALS_
%token  Q_PLUS__PLUS_
%token  Q_QUOTE_
%token  Q_QUOTE__LBRACE_
%token  Q_SLASH__EQUALS_
%token  Q_SLASH__STAR_VERILATOR_BLANK_CLOCKER_STAR__SLASH_
%token  Q_SLASH__STAR_VERILATOR_BLANK_CLOCK_UNDERSCORE_ENABLE_STAR__SLASH_
%token  Q_SLASH__STAR_VERILATOR_BLANK_COVERAGE_UNDERSCORE_BLOCK_UNDERSCORE_OFF_STAR__SLASH_
%token  Q_SLASH__STAR_VERILATOR_BLANK_FULL_UNDERSCORE_CASE_STAR__SLASH_
%token  Q_SLASH__STAR_VERILATOR_BLANK_INLINE_UNDERSCORE_MODULE_STAR__SLASH_
%token  Q_SLASH__STAR_VERILATOR_BLANK_ISOLATE_UNDERSCORE_ASSIGNMENTS_STAR__SLASH_
%token  Q_SLASH__STAR_VERILATOR_BLANK_NO_UNDERSCORE_CLOCKER_STAR__SLASH_
%token  Q_SLASH__STAR_VERILATOR_BLANK_NO_UNDERSCORE_INLINE_UNDERSCORE_MODULE_STAR__SLASH_
%token  Q_SLASH__STAR_VERILATOR_BLANK_NO_UNDERSCORE_INLINE_UNDERSCORE_TASK_STAR__SLASH_
%token  Q_SLASH__STAR_VERILATOR_BLANK_PARALLEL_UNDERSCORE_CASE_STAR__SLASH_
%token  Q_SLASH__STAR_VERILATOR_BLANK_PUBLIC_STAR__SLASH_
%token  Q_SLASH__STAR_VERILATOR_BLANK_PUBLIC_UNDERSCORE_FLAT_STAR__SLASH_
%token  Q_SLASH__STAR_VERILATOR_BLANK_PUBLIC_UNDERSCORE_FLAT_UNDERSCORE_RD_STAR__SLASH_
%token  Q_SLASH__STAR_VERILATOR_BLANK_PUBLIC_UNDERSCORE_FLAT_UNDERSCORE_RW_STAR__SLASH_
%token  Q_SLASH__STAR_VERILATOR_BLANK_PUBLIC_UNDERSCORE_MODULE_STAR__SLASH_
%token  Q_SLASH__STAR_VERILATOR_BLANK_SC_UNDERSCORE_BV_STAR__SLASH_
%token  Q_SLASH__STAR_VERILATOR_BLANK_SC_UNDERSCORE_CLOCK_STAR__SLASH_
%token  Q_SLASH__STAR_VERILATOR_BLANK_SFORMAT_STAR__SLASH_
%token  Q_STAR__EQUALS_
%token  Q_STAR__GREATER_
%token  Q_STAR__STAR_
%token  Q_TILDE__AMPERSAND_
%token  Q_TILDE__VBAR_
%token  Q_VBAR__EQUALS_
%token  Q_VBAR__EQUALS__GREATER_
%token  Q_VBAR__HYPHEN__GREATER_
%token  Q_VBAR__VBAR_
%token  RAND
%token  RANDC
%token  RBRACE
%token  RBRACK
%token  RCMOS
%token  REAL
%token  REALTIME
%token  REF
%token  REG
%token  REPEAT
%token  RESTRICT
%token  RETURN
%token  RNMOS
%token  RPAREN
%token  RPMOS
%token  RTRAN
%token  RTRANIF0
%token  RTRANIF1
%token  SCALARED
%token  SEMICOLON
%token  SHORTINT
%token  SHORTREAL
%token  SIGNED
%token  SLASH
%token <string list> SLIST
%token  SPECIFY
%token  SPECPARAM
%token  STAR
%token  STATIC
%token  STRING
%token  STRING0
%token  STRING1
%token  STRING2
%token  STRING3
%token  STRING4
%token  STRING5
%token  STRING6
%token  STRING7
%token  STRING8
%token  STRING9
%token  STRING10
%token  STRING11
%token  STRING12
%token  STRING13
%token  STRING14
%token  STRING15
%token  STRING_HYPHEN_IGNORED
%token  STRUCT
%token  SUPPLY0
%token  SUPPLY1
%token  TABLE
%token  TABLE_BLANK_LINE
%token  TASK
%token  TILDE
%token  TIME
%token  TIMEPRECISION
%token  TIMEUNIT
%token  TIME_BLANK_NUMBER
%token  TIMING_BLANK_SPEC_BLANK_ELEMENT
%token <token list> TLIST
%token  TRACING_UNDERSCORE_OFF
%token  TRACING_UNDERSCORE_ON
%token  TRAN
%token  TRANIF0
%token  TRANIF1
%token  TRI
%token  TRI0
%token  TRI1
%token  TRUE
%token <token*token*token*token*token*token*token*token*token*token> TUPLE10
%token <token*token> TUPLE2
%token <token*token*token> TUPLE3
%token <token*token*token*token> TUPLE4
%token <token*token*token*token*token> TUPLE5
%token <token*token*token*token*token*token> TUPLE6
%token <token*token*token*token*token*token*token> TUPLE7
%token <token*token*token*token*token*token*token*token> TUPLE8
%token <token*token*token*token*token*token*token*token*token> TUPLE9
%token  TYPE
%token  TYPEDEF
%token  TYPE_HYPHEN_IDENTIFIER
%token  UNDERSCORE
%token  UNION
%token  UNIQUE
%token  UNIQUE0
%token  UNSIGNED
%token  VAR
%token  VBAR
%token  VECTORED
%token  VOID
%token  WHILE
%token  WIRE
%token  WREAL
%token  XNOR
%token  XOR
%token  YP_LOGIFF
%type <token> ml_start
%start ml_start
%%


ml_start: source_text EOF_TOKEN { TUPLE2($1,EOF_TOKEN) }

source_text: /* empty */ { EMPTY_TOKEN }
	|	descriptionList { ($1) }

descriptionList: description { ($1) }
	|	descriptionList description { TUPLE2($1,$2) }

description: module_declaration { ($1) }
	|	interface_declaration { ($1) }
	|	program_declaration { ($1) }
	|	package_declaration { ($1) }
	|	package_item { ($1) }
	|	bind_directive { ($1) }
	|	vltItem { ($1) }
	|	ERROR_TOKEN { (ERROR_TOKEN) }

timeunits_declaration: TIMEUNIT TIME_BLANK_NUMBER SEMICOLON { TUPLE3(TIMEUNIT,TIME_BLANK_NUMBER,SEMICOLON) }
	|	TIMEUNIT TIME_BLANK_NUMBER SLASH TIME_BLANK_NUMBER SEMICOLON { TUPLE5(TIMEUNIT,TIME_BLANK_NUMBER,SLASH,TIME_BLANK_NUMBER,SEMICOLON) }
	|	TIMEPRECISION TIME_BLANK_NUMBER SEMICOLON { TUPLE3(TIMEPRECISION,TIME_BLANK_NUMBER,SEMICOLON) }

package_declaration: packageFront package_itemListE ENDPACKAGE endLabelE { TUPLE4($1,$2,ENDPACKAGE,$4) }

packageFront: PACKAGE lifetimeE idAny SEMICOLON { TUPLE4(PACKAGE,$2,$3,SEMICOLON) }

package_itemListE: /* empty */ { EMPTY_TOKEN }
	|	package_itemList { ($1) }

package_itemList: package_item { ($1) }
	|	package_itemList package_item { TUPLE2($1,$2) }

package_item: package_or_generate_item_declaration { ($1) }
	|	anonymous_program { ($1) }
	|	package_export_declaration { ($1) }
	|	timeunits_declaration { ($1) }

package_or_generate_item_declaration: net_declaration { ($1) }
	|	data_declaration { ($1) }
	|	task_declaration { ($1) }
	|	function_declaration { ($1) }
	|	dpi_import_export { ($1) }
	|	local_parameter_declaration SEMICOLON { TUPLE2($1,SEMICOLON) }
	|	parameter_declaration SEMICOLON { TUPLE2($1,SEMICOLON) }
	|	SEMICOLON { (SEMICOLON) }

package_import_declarationList: package_import_declaration { ($1) }
	|	package_import_declarationList package_import_declaration { TUPLE2($1,$2) }

package_import_declaration: IMPORT package_import_itemList SEMICOLON { TUPLE3(IMPORT,$2,SEMICOLON) }

package_import_itemList: package_import_item { ($1) }
	|	package_import_itemList COMMA package_import_item { TUPLE3($1,COMMA,$3) }

package_import_item: PACKAGE_HYPHEN_IDENTIFIER Q_COLON__COLON_ package_import_itemObj { TUPLE3(PACKAGE_HYPHEN_IDENTIFIER,Q_COLON__COLON_,$3) }

package_import_itemObj: idAny { ($1) }
	|	STAR { (STAR) }

package_export_declaration: EXPORT STAR Q_COLON__COLON_ STAR SEMICOLON { TUPLE5(EXPORT,STAR,Q_COLON__COLON_,STAR,SEMICOLON) }
	|	EXPORT package_export_itemList SEMICOLON { TUPLE3(EXPORT,$2,SEMICOLON) }

package_export_itemList: package_export_item { ($1) }
	|	package_export_itemList COMMA package_export_item { TUPLE3($1,COMMA,$3) }

package_export_item: PACKAGE_HYPHEN_IDENTIFIER Q_COLON__COLON_ package_import_itemObj { TUPLE3(PACKAGE_HYPHEN_IDENTIFIER,Q_COLON__COLON_,$3) }

module_declaration: modFront importsAndParametersE portsStarE SEMICOLON module_itemListE ENDMODULE endLabelE { TUPLE7($1,$2,$3,SEMICOLON,$5,ENDMODULE,$7) }
	|	udpFront parameter_port_listE portsStarE SEMICOLON module_itemListE ENDPRIMITIVE endLabelE { TUPLE7($1,$2,$3,SEMICOLON,$5,ENDPRIMITIVE,$7) }
	|	EXTERN modFront parameter_port_listE portsStarE SEMICOLON { TUPLE5(EXTERN,$2,$3,$4,SEMICOLON) }

modFront: MODULE lifetimeE idAny { TUPLE3(MODULE,$2,$3) }

importsAndParametersE: parameter_port_listE { ($1) }
	|	package_import_declarationList parameter_port_listE { TUPLE2($1,$2) }

udpFront: PRIMITIVE lifetimeE idAny { TUPLE3(PRIMITIVE,$2,$3) }

parameter_value_assignmentE: /* empty */ { EMPTY_TOKEN }
	|	HASH LPAREN cellparamList RPAREN { TUPLE4(HASH,LPAREN,$3,RPAREN) }
	|	HASH INTEGER_BLANK_NUMBER { TUPLE2(HASH,INTEGER_BLANK_NUMBER) }
	|	HASH FLOATING_HYPHEN_POINT_BLANK_NUMBER { TUPLE2(HASH,FLOATING_HYPHEN_POINT_BLANK_NUMBER) }
	|	HASH idClassSel { TUPLE2(HASH,$2) }

parameter_port_listE: /* empty */ { EMPTY_TOKEN }
	|	HASH LPAREN RPAREN { TUPLE3(HASH,LPAREN,RPAREN) }
	|	HASH LPAREN /* 1 */ paramPortDeclOrArgList RPAREN { TUPLE4(HASH,LPAREN,$3,RPAREN) }

paramPortDeclOrArgList: paramPortDeclOrArg { ($1) }
	|	paramPortDeclOrArgList COMMA paramPortDeclOrArg { TUPLE3($1,COMMA,$3) }

paramPortDeclOrArg: parameter_port_declarationFrontE param_assignment { TUPLE2($1,$2) }

portsStarE: /* empty */ { EMPTY_TOKEN }
	|	LPAREN RPAREN { TUPLE2(LPAREN,RPAREN) }
	|	LPAREN /* 2 */ list_of_ports RPAREN { TUPLE3(LPAREN,$2,RPAREN) }

list_of_ports: port { ($1) }
	|	list_of_ports COMMA port { TUPLE3($1,COMMA,$3) }

port: portDirNetE id portSig variable_dimensionListE sigAttrListE { TUPLE5($1,$2,$3,$4,$5) }
	|	portDirNetE id DOT idAny portSig variable_dimensionListE sigAttrListE { TUPLE7($1,$2,DOT,$4,$5,$6,$7) }
	|	portDirNetE INTERFACE portSig rangeListE sigAttrListE { TUPLE5($1,INTERFACE,$3,$4,$5) }
	|	portDirNetE INTERFACE DOT idAny portSig rangeListE sigAttrListE { TUPLE7($1,INTERFACE,DOT,$4,$5,$6,$7) }
	|	portDirNetE data_type portSig variable_dimensionListE sigAttrListE { TUPLE5($1,$2,$3,$4,$5) }
	|	portDirNetE VAR data_type portSig variable_dimensionListE sigAttrListE { TUPLE6($1,VAR,$3,$4,$5,$6) }
	|	portDirNetE VAR implicit_typeE portSig variable_dimensionListE sigAttrListE { TUPLE6($1,VAR,$3,$4,$5,$6) }
	|	portDirNetE signing portSig variable_dimensionListE sigAttrListE { TUPLE5($1,$2,$3,$4,$5) }
	|	portDirNetE signingE rangeList portSig variable_dimensionListE sigAttrListE { TUPLE6($1,$2,$3,$4,$5,$6) }
	|	portDirNetE portSig variable_dimensionListE sigAttrListE { TUPLE4($1,$2,$3,$4) }
	|	portDirNetE data_type portSig variable_dimensionListE sigAttrListE EQUALS constExpr { TUPLE7($1,$2,$3,$4,$5,EQUALS,$7) }
	|	portDirNetE VAR data_type portSig variable_dimensionListE sigAttrListE EQUALS constExpr { TUPLE8($1,VAR,$3,$4,$5,$6,EQUALS,$8) }
	|	portDirNetE VAR implicit_typeE portSig variable_dimensionListE sigAttrListE EQUALS constExpr { TUPLE8($1,VAR,$3,$4,$5,$6,EQUALS,$8) }
	|	portDirNetE portSig variable_dimensionListE sigAttrListE EQUALS constExpr { TUPLE6($1,$2,$3,$4,EQUALS,$6) }

portDirNetE: /* empty */ { EMPTY_TOKEN }
	|	port_direction { ($1) }
	|	port_direction /* 3 */ net_type { TUPLE2($1,$2) }
	|	net_type { ($1) }

port_declNetE: /* empty */ { EMPTY_TOKEN }
	|	net_type { ($1) }

portSig: id { ($1) }
	|	idSVKwd { ($1) }

interface_declaration: intFront parameter_port_listE portsStarE SEMICOLON interface_itemListE ENDINTERFACE endLabelE { TUPLE7($1,$2,$3,SEMICOLON,$5,ENDINTERFACE,$7) }
	|	EXTERN intFront parameter_port_listE portsStarE SEMICOLON { TUPLE5(EXTERN,$2,$3,$4,SEMICOLON) }

intFront: INTERFACE lifetimeE idAny { TUPLE3(INTERFACE,$2,$3) }

interface_itemListE: /* empty */ { EMPTY_TOKEN }
	|	interface_itemList { ($1) }

interface_itemList: interface_item { ($1) }
	|	interface_itemList interface_item { TUPLE2($1,$2) }

interface_item: port_declaration SEMICOLON { TUPLE2($1,SEMICOLON) }
	|	interface_generate_region { ($1) }
	|	interface_or_generate_item { ($1) }
	|	timeunits_declaration { ($1) }
	|	module_common_item { ($1) }

interface_generate_region: GENERATE interface_itemList ENDGENERATE { TUPLE3(GENERATE,$2,ENDGENERATE) }
	|	GENERATE ENDGENERATE { TUPLE2(GENERATE,ENDGENERATE) }

interface_or_generate_item: modport_declaration { ($1) }
	|	extern_tf_declaration { ($1) }

anonymous_program: PROGRAM SEMICOLON anonymous_program_itemListE ENDPROGRAM { TUPLE4(PROGRAM,SEMICOLON,$3,ENDPROGRAM) }

anonymous_program_itemListE: /* empty */ { EMPTY_TOKEN }
	|	anonymous_program_itemList { ($1) }

anonymous_program_itemList: anonymous_program_item { ($1) }
	|	anonymous_program_itemList anonymous_program_item { TUPLE2($1,$2) }

anonymous_program_item: task_declaration { ($1) }
	|	function_declaration { ($1) }
	|	SEMICOLON { (SEMICOLON) }

program_declaration: pgmFront parameter_port_listE portsStarE SEMICOLON program_itemListE ENDPROGRAM endLabelE { TUPLE7($1,$2,$3,SEMICOLON,$5,ENDPROGRAM,$7) }
	|	EXTERN pgmFront parameter_port_listE portsStarE SEMICOLON { TUPLE5(EXTERN,$2,$3,$4,SEMICOLON) }

pgmFront: PROGRAM lifetimeE idAny { TUPLE3(PROGRAM,$2,$3) }

program_itemListE: /* empty */ { EMPTY_TOKEN }
	|	program_itemList { ($1) }

program_itemList: program_item { ($1) }
	|	program_itemList program_item { TUPLE2($1,$2) }

program_item: port_declaration SEMICOLON { TUPLE2($1,SEMICOLON) }
	|	non_port_program_item { ($1) }

non_port_program_item: continuous_assign { ($1) }
	|	module_or_generate_item_declaration { ($1) }
	|	initial_construct { ($1) }
	|	final_construct { ($1) }
	|	concurrent_assertion_item { ($1) }
	|	timeunits_declaration { ($1) }
	|	program_generate_item { ($1) }

program_generate_item: loop_generate_construct { ($1) }
	|	conditional_generate_construct { ($1) }
	|	generate_region { ($1) }
	|	elaboration_system_task { ($1) }

extern_tf_declaration: EXTERN task_prototype SEMICOLON { TUPLE3(EXTERN,$2,SEMICOLON) }
	|	EXTERN function_prototype SEMICOLON { TUPLE3(EXTERN,$2,SEMICOLON) }
	|	EXTERN FORKJOIN task_prototype SEMICOLON { TUPLE4(EXTERN,FORKJOIN,$3,SEMICOLON) }

modport_declaration: MODPORT modport_itemList SEMICOLON { TUPLE3(MODPORT,$2,SEMICOLON) }

modport_itemList: modport_item { ($1) }
	|	modport_itemList COMMA modport_item { TUPLE3($1,COMMA,$3) }

modport_item: id LPAREN /* 4 */ modportPortsDeclList RPAREN { TUPLE4($1,LPAREN,$3,RPAREN) }

modportPortsDeclList: modportPortsDecl { ($1) }
	|	modportPortsDeclList COMMA modportPortsDecl { TUPLE3($1,COMMA,$3) }

modportPortsDecl: port_direction modportSimplePort { TUPLE2($1,$2) }
	|	CLOCKING idAny { TUPLE2(CLOCKING,$2) }
	|	IMPORT id { TUPLE2(IMPORT,$2) }
	|	EXPORT id { TUPLE2(EXPORT,$2) }
	|	IMPORT method_prototype { TUPLE2(IMPORT,$2) }
	|	EXPORT method_prototype { TUPLE2(EXPORT,$2) }
	|	modportSimplePort { ($1) }

modportSimplePort: id { ($1) }

genvar_declaration: GENVAR list_of_genvar_identifiers SEMICOLON { TUPLE3(GENVAR,$2,SEMICOLON) }

list_of_genvar_identifiers: genvar_identifierDecl { ($1) }
	|	list_of_genvar_identifiers COMMA genvar_identifierDecl { TUPLE3($1,COMMA,$3) }

genvar_identifierDecl: id sigAttrListE { TUPLE2($1,$2) }

local_parameter_declaration: local_parameter_declarationFront list_of_param_assignments { TUPLE2($1,$2) }

parameter_declaration: parameter_declarationFront list_of_param_assignments { TUPLE2($1,$2) }

local_parameter_declarationFront: varLParamReset implicit_typeE { TUPLE2($1,$2) }
	|	varLParamReset data_type { TUPLE2($1,$2) }
	|	varLParamReset TYPE { TUPLE2($1,TYPE) }

parameter_declarationFront: varGParamReset implicit_typeE { TUPLE2($1,$2) }
	|	varGParamReset data_type { TUPLE2($1,$2) }
	|	varGParamReset TYPE { TUPLE2($1,TYPE) }

parameter_port_declarationFrontE: varGParamReset implicit_typeE { TUPLE2($1,$2) }
	|	varGParamReset data_type { TUPLE2($1,$2) }
	|	varGParamReset TYPE { TUPLE2($1,TYPE) }
	|	varLParamReset implicit_typeE { TUPLE2($1,$2) }
	|	varLParamReset data_type { TUPLE2($1,$2) }
	|	varLParamReset TYPE { TUPLE2($1,TYPE) }
	|	implicit_typeE { ($1) }
	|	data_type { ($1) }
	|	TYPE { (TYPE) }

net_declaration: net_declarationFront netSigList SEMICOLON { TUPLE3($1,$2,SEMICOLON) }

net_declarationFront: net_declRESET net_type strengthSpecE net_scalaredE net_dataType { TUPLE5($1,$2,$3,$4,$5) }

net_declRESET: /* empty */ { EMPTY_TOKEN }

net_scalaredE: /* empty */ { EMPTY_TOKEN }
	|	SCALARED { (SCALARED) }
	|	VECTORED { (VECTORED) }

net_dataType: var_data_type { ($1) }
	|	signingE rangeList delayE { TUPLE3($1,$2,$3) }
	|	signing { ($1) }
	|	delayE { ($1) }

net_type: SUPPLY0 { (SUPPLY0) }
	|	SUPPLY1 { (SUPPLY1) }
	|	TRI { (TRI) }
	|	TRI0 { (TRI0) }
	|	TRI1 { (TRI1) }
	|	WIRE { (WIRE) }
	|	WREAL { (WREAL) }

varGParamReset: PARAMETER { (PARAMETER) }

varLParamReset: LOCALPARAM { (LOCALPARAM) }

port_direction: INPUT { (INPUT) }
	|	OUTPUT { (OUTPUT) }
	|	INOUT { (INOUT) }
	|	REF { (REF) }
	|	CONST_HYPHEN_THEN_HYPHEN_REF REF { TUPLE2(CONST_HYPHEN_THEN_HYPHEN_REF,REF) }

port_directionReset: INPUT { (INPUT) }
	|	OUTPUT { (OUTPUT) }
	|	INOUT { (INOUT) }
	|	REF { (REF) }
	|	CONST_HYPHEN_THEN_HYPHEN_REF REF { TUPLE2(CONST_HYPHEN_THEN_HYPHEN_REF,REF) }

port_declaration: port_directionReset port_declNetE data_type /* 5 */ list_of_variable_decl_assignments { TUPLE4($1,$2,$3,$4) }
	|	port_directionReset port_declNetE VAR data_type /* 6 */ list_of_variable_decl_assignments { TUPLE5($1,$2,VAR,$4,$5) }
	|	port_directionReset port_declNetE VAR implicit_typeE /* 7 */ list_of_variable_decl_assignments { TUPLE5($1,$2,VAR,$4,$5) }
	|	port_directionReset port_declNetE signingE rangeList /* 8 */ list_of_variable_decl_assignments { TUPLE5($1,$2,$3,$4,$5) }
	|	port_directionReset port_declNetE signing /* 9 */ list_of_variable_decl_assignments { TUPLE4($1,$2,$3,$4) }
	|	port_directionReset port_declNetE /* 10 */ list_of_variable_decl_assignments { TUPLE3($1,$2,$3) }

tf_port_declaration: port_directionReset data_type /* 11 */ list_of_tf_variable_identifiers SEMICOLON { TUPLE4($1,$2,$3,SEMICOLON) }
	|	port_directionReset implicit_typeE /* 12 */ list_of_tf_variable_identifiers SEMICOLON { TUPLE4($1,$2,$3,SEMICOLON) }
	|	port_directionReset VAR data_type /* 13 */ list_of_tf_variable_identifiers SEMICOLON { TUPLE5($1,VAR,$3,$4,SEMICOLON) }
	|	port_directionReset VAR implicit_typeE /* 14 */ list_of_tf_variable_identifiers SEMICOLON { TUPLE5($1,VAR,$3,$4,SEMICOLON) }

integer_atom_type: BYTE { (BYTE) }
	|	SHORTINT { (SHORTINT) }
	|	INT { (INT) }
	|	LONGINT { (LONGINT) }
	|	INTEGER { (INTEGER) }
	|	TIME { (TIME) }

integer_vector_type: BIT { (BIT) }
	|	LOGIC { (LOGIC) }
	|	REG { (REG) }

non_integer_type: REAL { (REAL) }
	|	REALTIME { (REALTIME) }
	|	SHORTREAL { (SHORTREAL) }

signingE: /* empty */ { EMPTY_TOKEN }
	|	signing { ($1) }

signing: SIGNED { (SIGNED) }
	|	UNSIGNED { (UNSIGNED) }

casting_type: simple_type { ($1) }

simple_type: integer_atom_type { ($1) }
	|	integer_vector_type { ($1) }
	|	non_integer_type { ($1) }
	|	ps_type { ($1) }

data_type: data_typeNoRef { ($1) }
	|	ps_type packed_dimensionListE { TUPLE2($1,$2) }

data_typeBasic: integer_vector_type signingE rangeListE { TUPLE3($1,$2,$3) }
	|	integer_atom_type signingE { TUPLE2($1,$2) }
	|	non_integer_type { ($1) }

data_typeNoRef: data_typeBasic { ($1) }
	|	struct_unionDecl packed_dimensionListE { TUPLE2($1,$2) }
	|	enumDecl { ($1) }
	|	STRING { (STRING) }
	|	CHANDLE { (CHANDLE) }

data_type_or_void: data_type { ($1) }

var_data_type: data_type { ($1) }
	|	VAR data_type { TUPLE2(VAR,$2) }
	|	VAR implicit_typeE { TUPLE2(VAR,$2) }

struct_unionDecl: STRUCT packedSigningE LBRACE /* 15 */ struct_union_memberList RBRACE { TUPLE5(STRUCT,$2,LBRACE,$4,RBRACE) }
	|	UNION taggedE packedSigningE LBRACE /* 16 */ struct_union_memberList RBRACE { TUPLE6(UNION,$2,$3,LBRACE,$5,RBRACE) }

struct_union_memberList: struct_union_member { ($1) }
	|	struct_union_memberList struct_union_member { TUPLE2($1,$2) }

struct_union_member: random_qualifierE data_type_or_void /* 17 */ list_of_member_decl_assignments SEMICOLON { TUPLE4($1,$2,$3,SEMICOLON) }

list_of_member_decl_assignments: member_decl_assignment { ($1) }
	|	list_of_member_decl_assignments COMMA member_decl_assignment { TUPLE3($1,COMMA,$3) }

member_decl_assignment: id variable_dimensionListE { TUPLE2($1,$2) }
	|	id variable_dimensionListE EQUALS variable_declExpr { TUPLE4($1,$2,EQUALS,$4) }
	|	idSVKwd { ($1) }

list_of_variable_decl_assignments: variable_decl_assignment { ($1) }
	|	list_of_variable_decl_assignments COMMA variable_decl_assignment { TUPLE3($1,COMMA,$3) }

variable_decl_assignment: id variable_dimensionListE sigAttrListE { TUPLE3($1,$2,$3) }
	|	id variable_dimensionListE sigAttrListE EQUALS variable_declExpr { TUPLE5($1,$2,$3,EQUALS,$5) }
	|	idSVKwd { ($1) }

list_of_tf_variable_identifiers: tf_variable_identifier { ($1) }
	|	list_of_tf_variable_identifiers COMMA tf_variable_identifier { TUPLE3($1,COMMA,$3) }

tf_variable_identifier: id variable_dimensionListE sigAttrListE { TUPLE3($1,$2,$3) }
	|	id variable_dimensionListE sigAttrListE EQUALS expr { TUPLE5($1,$2,$3,EQUALS,$5) }

variable_declExpr: expr { ($1) }

variable_dimensionListE: /* empty */ { EMPTY_TOKEN }
	|	variable_dimensionList { ($1) }

variable_dimensionList: variable_dimension { ($1) }
	|	variable_dimensionList variable_dimension { TUPLE2($1,$2) }

variable_dimension: LBRACK RBRACK { TUPLE2(LBRACK,RBRACK) }
	|	anyrange { ($1) }
	|	LBRACK constExpr RBRACK { TUPLE3(LBRACK,$2,RBRACK) }

random_qualifierE: /* empty */ { EMPTY_TOKEN }
	|	random_qualifier { ($1) }

random_qualifier: RAND { (RAND) }
	|	RANDC { (RANDC) }

taggedE: /* empty */ { EMPTY_TOKEN }

packedSigningE: /* empty */ { EMPTY_TOKEN }
	|	PACKED signingE { TUPLE2(PACKED,$2) }

enumDecl: ENUM enum_base_typeE LBRACE enum_nameList RBRACE { TUPLE5(ENUM,$2,LBRACE,$4,RBRACE) }

enum_base_typeE: /* empty */ { EMPTY_TOKEN }
	|	signingE rangeList { TUPLE2($1,$2) }
	|	signing { ($1) }
	|	integer_atom_type signingE { TUPLE2($1,$2) }
	|	integer_vector_type signingE rangeListE { TUPLE3($1,$2,$3) }
	|	idAny rangeListE { TUPLE2($1,$2) }

enum_nameList: enum_name_declaration { ($1) }
	|	enum_nameList COMMA enum_name_declaration { TUPLE3($1,COMMA,$3) }

enum_name_declaration: idAny enumNameRangeE enumNameStartE { TUPLE3($1,$2,$3) }

enumNameRangeE: /* empty */ { EMPTY_TOKEN }
	|	LBRACK intnumAsConst RBRACK { TUPLE3(LBRACK,$2,RBRACK) }
	|	LBRACK intnumAsConst COLON intnumAsConst RBRACK { TUPLE5(LBRACK,$2,COLON,$4,RBRACK) }

enumNameStartE: /* empty */ { EMPTY_TOKEN }
	|	EQUALS constExpr { TUPLE2(EQUALS,$2) }

intnumAsConst: INTEGER_BLANK_NUMBER { (INTEGER_BLANK_NUMBER) }

data_declaration: data_declarationVar { ($1) }
	|	type_declaration { ($1) }
	|	package_import_declaration { ($1) }

data_declarationVar: data_declarationVarFront list_of_variable_decl_assignments SEMICOLON { TUPLE3($1,$2,SEMICOLON) }

data_declarationVarFront: VAR lifetimeE data_type { TUPLE3(VAR,$2,$3) }
	|	VAR lifetimeE { TUPLE2(VAR,$2) }
	|	VAR lifetimeE signingE rangeList { TUPLE4(VAR,$2,$3,$4) }
	|	CONST VAR lifetimeE data_type { TUPLE4(CONST,VAR,$3,$4) }
	|	CONST VAR lifetimeE { TUPLE3(CONST,VAR,$3) }
	|	CONST VAR lifetimeE signingE rangeList { TUPLE5(CONST,VAR,$3,$4,$5) }
	|	data_type { ($1) }
	|	lifetime data_type { TUPLE2($1,$2) }
	|	CONST lifetimeE data_type { TUPLE3(CONST,$2,$3) }

implicit_typeE: /* empty */ { EMPTY_TOKEN }
	|	signingE rangeList { TUPLE2($1,$2) }
	|	signing { ($1) }

type_declaration: TYPEDEF data_type idAny variable_dimensionListE dtypeAttrListE SEMICOLON { TUPLE6(TYPEDEF,$2,$3,$4,$5,SEMICOLON) }
	|	TYPEDEF id SEMICOLON { TUPLE3(TYPEDEF,$2,SEMICOLON) }
	|	TYPEDEF ENUM idAny SEMICOLON { TUPLE4(TYPEDEF,ENUM,$3,SEMICOLON) }
	|	TYPEDEF STRUCT idAny SEMICOLON { TUPLE4(TYPEDEF,STRUCT,$3,SEMICOLON) }
	|	TYPEDEF UNION idAny SEMICOLON { TUPLE4(TYPEDEF,UNION,$3,SEMICOLON) }

dtypeAttrListE: /* empty */ { EMPTY_TOKEN }
	|	dtypeAttrList { ($1) }

dtypeAttrList: dtypeAttr { ($1) }
	|	dtypeAttrList dtypeAttr { TUPLE2($1,$2) }

dtypeAttr: Q_SLASH__STAR_VERILATOR_BLANK_PUBLIC_STAR__SLASH_ { (Q_SLASH__STAR_VERILATOR_BLANK_PUBLIC_STAR__SLASH_) }

module_itemListE: /* empty */ { EMPTY_TOKEN }
	|	module_itemList { ($1) }

module_itemList: module_item { ($1) }
	|	module_itemList module_item { TUPLE2($1,$2) }

module_item: port_declaration SEMICOLON { TUPLE2($1,SEMICOLON) }
	|	non_port_module_item { ($1) }

non_port_module_item: generate_region { ($1) }
	|	module_or_generate_item { ($1) }
	|	specify_block { ($1) }
	|	specparam_declaration { ($1) }
	|	timeunits_declaration { ($1) }
	|	Q_BACKQUOTE_SYSTEMC_UNDERSCORE_HEADER_BLANK_BLOCK { (Q_BACKQUOTE_SYSTEMC_UNDERSCORE_HEADER_BLANK_BLOCK) }
	|	Q_BACKQUOTE_SYSTEMC_UNDERSCORE_CTOR_BLANK_BLOCK { (Q_BACKQUOTE_SYSTEMC_UNDERSCORE_CTOR_BLANK_BLOCK) }
	|	Q_BACKQUOTE_SYSTEMC_UNDERSCORE_DTOR_BLANK_BLOCK { (Q_BACKQUOTE_SYSTEMC_UNDERSCORE_DTOR_BLANK_BLOCK) }
	|	Q_BACKQUOTE_SYSTEMC_UNDERSCORE_INTERFACE_BLANK_BLOCK { (Q_BACKQUOTE_SYSTEMC_UNDERSCORE_INTERFACE_BLANK_BLOCK) }
	|	Q_BACKQUOTE_SYSTEMC_UNDERSCORE_IMPLEMENTATION_BLANK_BLOCK { (Q_BACKQUOTE_SYSTEMC_UNDERSCORE_IMPLEMENTATION_BLANK_BLOCK) }
	|	Q_BACKQUOTE_SYSTEMC_UNDERSCORE_IMP_UNDERSCORE_HEADER_BLANK_BLOCK { (Q_BACKQUOTE_SYSTEMC_UNDERSCORE_IMP_UNDERSCORE_HEADER_BLANK_BLOCK) }
	|	Q_SLASH__STAR_VERILATOR_BLANK_INLINE_UNDERSCORE_MODULE_STAR__SLASH_ { (Q_SLASH__STAR_VERILATOR_BLANK_INLINE_UNDERSCORE_MODULE_STAR__SLASH_) }
	|	Q_SLASH__STAR_VERILATOR_BLANK_NO_UNDERSCORE_INLINE_UNDERSCORE_MODULE_STAR__SLASH_ { (Q_SLASH__STAR_VERILATOR_BLANK_NO_UNDERSCORE_INLINE_UNDERSCORE_MODULE_STAR__SLASH_) }
	|	Q_SLASH__STAR_VERILATOR_BLANK_PUBLIC_UNDERSCORE_MODULE_STAR__SLASH_ { (Q_SLASH__STAR_VERILATOR_BLANK_PUBLIC_UNDERSCORE_MODULE_STAR__SLASH_) }

module_or_generate_item: DEFPARAM list_of_defparam_assignments SEMICOLON { TUPLE3(DEFPARAM,$2,SEMICOLON) }
	|	combinational_body { ($1) }
	|	module_common_item { ($1) }

module_common_item: module_or_generate_item_declaration { ($1) }
	|	etcInst { ($1) }
	|	concurrent_assertion_item { ($1) }
	|	bind_directive { ($1) }
	|	continuous_assign { ($1) }
	|	initial_construct { ($1) }
	|	final_construct { ($1) }
	|	ALWAYS event_controlE stmtBlock { TUPLE3(ALWAYS,$2,$3) }
	|	ALWAYS_UNDERSCORE_FF event_controlE stmtBlock { TUPLE3(ALWAYS_UNDERSCORE_FF,$2,$3) }
	|	ALWAYS_UNDERSCORE_LATCH event_controlE stmtBlock { TUPLE3(ALWAYS_UNDERSCORE_LATCH,$2,$3) }
	|	ALWAYS_UNDERSCORE_COMB stmtBlock { TUPLE2(ALWAYS_UNDERSCORE_COMB,$2) }
	|	loop_generate_construct { ($1) }
	|	conditional_generate_construct { ($1) }
	|	elaboration_system_task { ($1) }
	|	ERROR_TOKEN SEMICOLON { TUPLE2(ERROR_TOKEN,SEMICOLON) }

continuous_assign: ASSIGN delayE assignList SEMICOLON { TUPLE4(ASSIGN,$2,$3,SEMICOLON) }

initial_construct: INITIAL stmtBlock { TUPLE2(INITIAL,$2) }

final_construct: FINAL stmtBlock { TUPLE2(FINAL,$2) }

module_or_generate_item_declaration: package_or_generate_item_declaration { ($1) }
	|	genvar_declaration { ($1) }
	|	clocking_declaration { ($1) }

bind_directive: BIND bind_target_instance bind_instantiation { TUPLE3(BIND,$2,$3) }
	|	BIND bind_target_instance COLON bind_target_instance_list bind_instantiation { TUPLE5(BIND,$2,COLON,$4,$5) }

bind_target_instance_list: bind_target_instance { CONS1 ($1) }
	|	bind_target_instance_list COMMA bind_target_instance { CONS3($1,COMMA,$3) }

bind_target_instance: idAny { ($1) }

bind_instantiation: instDecl { ($1) }

generate_region: GENERATE genItemList ENDGENERATE { TUPLE3(GENERATE,$2,ENDGENERATE) }
	|	GENERATE ENDGENERATE { TUPLE2(GENERATE,ENDGENERATE) }

generate_block_or_null: generate_item { ($1) }
	|	genItemBegin { ($1) }

genItemBegin: BEGIN genItemList END { TUPLE3(BEGIN,$2,END) }
	|	BEGIN END { TUPLE2(BEGIN,END) }
	|	id COLON BEGIN genItemList END endLabelE { TUPLE6($1,COLON,BEGIN,$4,END,$6) }
	|	id COLON BEGIN END endLabelE { TUPLE5($1,COLON,BEGIN,END,$5) }
	|	BEGIN COLON idAny genItemList END endLabelE { TUPLE6(BEGIN,COLON,$3,$4,END,$6) }
	|	BEGIN COLON idAny END endLabelE { TUPLE5(BEGIN,COLON,$3,END,$5) }

genItemOrBegin: generate_item { ($1) }
	|	genItemBegin { ($1) }

genItemList: genItemOrBegin { ($1) }
	|	genItemList genItemOrBegin { TUPLE2($1,$2) }

generate_item: module_or_generate_item { ($1) }

conditional_generate_construct: CASE LPAREN expr RPAREN case_generate_itemListE ENDCASE { TUPLE6(CASE,LPAREN,$3,RPAREN,$5,ENDCASE) }
	|	IF LPAREN expr RPAREN generate_block_or_null { TUPLE5(IF,LPAREN,$3,RPAREN,$5) }
	|	IF LPAREN expr RPAREN generate_block_or_null ELSE generate_block_or_null { TUPLE7(IF,LPAREN,$3,RPAREN,$5,ELSE,$7) }

loop_generate_construct: FOR LPAREN genvar_initialization SEMICOLON expr SEMICOLON genvar_iteration RPAREN generate_block_or_null { TUPLE9(FOR,LPAREN,$3,SEMICOLON,$5,SEMICOLON,$7,RPAREN,$9) }

genvar_initialization: varRefBase EQUALS expr { TUPLE3($1,EQUALS,$3) }
	|	GENVAR genvar_identifierDecl EQUALS constExpr { TUPLE4(GENVAR,$2,EQUALS,$4) }

genvar_iteration: varRefBase EQUALS expr { TUPLE3($1,EQUALS,$3) }
	|	varRefBase Q_PLUS__EQUALS_ expr { TUPLE3($1,Q_PLUS__EQUALS_,$3) }
	|	varRefBase Q_HYPHEN__EQUALS_ expr { TUPLE3($1,Q_HYPHEN__EQUALS_,$3) }
	|	varRefBase Q_STAR__EQUALS_ expr { TUPLE3($1,Q_STAR__EQUALS_,$3) }
	|	varRefBase Q_SLASH__EQUALS_ expr { TUPLE3($1,Q_SLASH__EQUALS_,$3) }
	|	varRefBase Q_PERCENT__EQUALS_ expr { TUPLE3($1,Q_PERCENT__EQUALS_,$3) }
	|	varRefBase Q_AMPERSAND__EQUALS_ expr { TUPLE3($1,Q_AMPERSAND__EQUALS_,$3) }
	|	varRefBase Q_VBAR__EQUALS_ expr { TUPLE3($1,Q_VBAR__EQUALS_,$3) }
	|	varRefBase Q_CARET__EQUALS_ expr { TUPLE3($1,Q_CARET__EQUALS_,$3) }
	|	varRefBase Q_LESS__LESS__EQUALS_ expr { TUPLE3($1,Q_LESS__LESS__EQUALS_,$3) }
	|	varRefBase Q_GREATER__GREATER__EQUALS_ expr { TUPLE3($1,Q_GREATER__GREATER__EQUALS_,$3) }
	|	varRefBase Q_GREATER__GREATER__GREATER__EQUALS_ expr { TUPLE3($1,Q_GREATER__GREATER__GREATER__EQUALS_,$3) }
	|	Q_PLUS__PLUS_ varRefBase { TUPLE2(Q_PLUS__PLUS_,$2) }
	|	Q_HYPHEN__HYPHEN_ varRefBase { TUPLE2(Q_HYPHEN__HYPHEN_,$2) }
	|	varRefBase Q_PLUS__PLUS_ { TUPLE2($1,Q_PLUS__PLUS_) }
	|	varRefBase Q_HYPHEN__HYPHEN_ { TUPLE2($1,Q_HYPHEN__HYPHEN_) }

case_generate_itemListE: /* empty */ { EMPTY_TOKEN }
	|	case_generate_itemList { ($1) }

case_generate_itemList: case_generate_item { ($1) }
	|	case_generate_itemList case_generate_item { TUPLE2($1,$2) }

case_generate_item: caseCondList COLON generate_block_or_null { TUPLE3($1,COLON,$3) }
	|	DEFAULT COLON generate_block_or_null { TUPLE3(DEFAULT,COLON,$3) }
	|	DEFAULT generate_block_or_null { TUPLE2(DEFAULT,$2) }

assignList: assignOne { ($1) }
	|	assignList COMMA assignOne { TUPLE3($1,COMMA,$3) }

assignOne: variable_lvalue EQUALS expr { TUPLE3($1,EQUALS,$3) }

delayE: /* empty */ { EMPTY_TOKEN }
	|	delay_control { ($1) }

delay_control: HASH delay_value { TUPLE2(HASH,$2) }
	|	HASH LPAREN minTypMax RPAREN { TUPLE4(HASH,LPAREN,$3,RPAREN) }
	|	HASH LPAREN minTypMax COMMA minTypMax RPAREN { TUPLE6(HASH,LPAREN,$3,COMMA,$5,RPAREN) }
	|	HASH LPAREN minTypMax COMMA minTypMax COMMA minTypMax RPAREN { TUPLE8(HASH,LPAREN,$3,COMMA,$5,COMMA,$7,RPAREN) }

delay_value: ps_id_etc { ($1) }
	|	INTEGER_BLANK_NUMBER { (INTEGER_BLANK_NUMBER) }
	|	FLOATING_HYPHEN_POINT_BLANK_NUMBER { (FLOATING_HYPHEN_POINT_BLANK_NUMBER) }
	|	TIME_BLANK_NUMBER { (TIME_BLANK_NUMBER) }

delayExpr: expr { ($1) }
	|	TIME_BLANK_NUMBER { (TIME_BLANK_NUMBER) }

minTypMax: delayExpr { ($1) }
	|	delayExpr COLON delayExpr COLON delayExpr { TUPLE5($1,COLON,$3,COLON,$5) }

netSigList: netSig { ($1) }
	|	netSigList COMMA netSig { TUPLE3($1,COMMA,$3) }

netSig: netId sigAttrListE { TUPLE2($1,$2) }
	|	netId sigAttrListE EQUALS expr { TUPLE4($1,$2,EQUALS,$4) }
	|	netId variable_dimensionList sigAttrListE { TUPLE3($1,$2,$3) }

netId: id { ($1) }
	|	idSVKwd { ($1) }

sigAttrListE: /* empty */ { EMPTY_TOKEN }
	|	sigAttrList { ($1) }

sigAttrList: sigAttr { ($1) }
	|	sigAttrList sigAttr { TUPLE2($1,$2) }

sigAttr: Q_SLASH__STAR_VERILATOR_BLANK_SC_UNDERSCORE_CLOCK_STAR__SLASH_ { (Q_SLASH__STAR_VERILATOR_BLANK_SC_UNDERSCORE_CLOCK_STAR__SLASH_) }
	|	Q_SLASH__STAR_VERILATOR_BLANK_CLOCKER_STAR__SLASH_ { (Q_SLASH__STAR_VERILATOR_BLANK_CLOCKER_STAR__SLASH_) }
	|	Q_SLASH__STAR_VERILATOR_BLANK_NO_UNDERSCORE_CLOCKER_STAR__SLASH_ { (Q_SLASH__STAR_VERILATOR_BLANK_NO_UNDERSCORE_CLOCKER_STAR__SLASH_) }
	|	Q_SLASH__STAR_VERILATOR_BLANK_CLOCK_UNDERSCORE_ENABLE_STAR__SLASH_ { (Q_SLASH__STAR_VERILATOR_BLANK_CLOCK_UNDERSCORE_ENABLE_STAR__SLASH_) }
	|	Q_SLASH__STAR_VERILATOR_BLANK_PUBLIC_STAR__SLASH_ { (Q_SLASH__STAR_VERILATOR_BLANK_PUBLIC_STAR__SLASH_) }
	|	Q_SLASH__STAR_VERILATOR_BLANK_PUBLIC_UNDERSCORE_FLAT_STAR__SLASH_ { (Q_SLASH__STAR_VERILATOR_BLANK_PUBLIC_UNDERSCORE_FLAT_STAR__SLASH_) }
	|	Q_SLASH__STAR_VERILATOR_BLANK_PUBLIC_UNDERSCORE_FLAT_UNDERSCORE_RD_STAR__SLASH_ { (Q_SLASH__STAR_VERILATOR_BLANK_PUBLIC_UNDERSCORE_FLAT_UNDERSCORE_RD_STAR__SLASH_) }
	|	Q_SLASH__STAR_VERILATOR_BLANK_PUBLIC_UNDERSCORE_FLAT_UNDERSCORE_RW_STAR__SLASH_ { (Q_SLASH__STAR_VERILATOR_BLANK_PUBLIC_UNDERSCORE_FLAT_UNDERSCORE_RW_STAR__SLASH_) }
	|	Q_SLASH__STAR_VERILATOR_BLANK_PUBLIC_UNDERSCORE_FLAT_UNDERSCORE_RW_STAR__SLASH_ attr_event_control { TUPLE2(Q_SLASH__STAR_VERILATOR_BLANK_PUBLIC_UNDERSCORE_FLAT_UNDERSCORE_RW_STAR__SLASH_,$2) }
	|	Q_SLASH__STAR_VERILATOR_BLANK_ISOLATE_UNDERSCORE_ASSIGNMENTS_STAR__SLASH_ { (Q_SLASH__STAR_VERILATOR_BLANK_ISOLATE_UNDERSCORE_ASSIGNMENTS_STAR__SLASH_) }
	|	Q_SLASH__STAR_VERILATOR_BLANK_SC_UNDERSCORE_BV_STAR__SLASH_ { (Q_SLASH__STAR_VERILATOR_BLANK_SC_UNDERSCORE_BV_STAR__SLASH_) }
	|	Q_SLASH__STAR_VERILATOR_BLANK_SFORMAT_STAR__SLASH_ { (Q_SLASH__STAR_VERILATOR_BLANK_SFORMAT_STAR__SLASH_) }

rangeListE: /* empty */ { EMPTY_TOKEN }
	|	rangeList { ($1) }

rangeList: anyrange { ($1) }
	|	rangeList anyrange { TUPLE2($1,$2) }

anyrange: LBRACK constExpr COLON constExpr RBRACK { TUPLE5(LBRACK,$2,COLON,$4,RBRACK) }

packed_dimensionListE: /* empty */ { EMPTY_TOKEN }
	|	packed_dimensionList { ($1) }

packed_dimensionList: packed_dimension { ($1) }
	|	packed_dimensionList packed_dimension { TUPLE2($1,$2) }

packed_dimension: anyrange { ($1) }
	|	LBRACK RBRACK { TUPLE2(LBRACK,RBRACK) }

param_assignment: id variable_dimensionListE sigAttrListE EQUALS exprOrDataType { TUPLE5($1,$2,$3,EQUALS,$5) }
	|	id variable_dimensionListE sigAttrListE { TUPLE3($1,$2,$3) }

list_of_param_assignments: param_assignment { ($1) }
	|	list_of_param_assignments COMMA param_assignment { TUPLE3($1,COMMA,$3) }

list_of_defparam_assignments: defparam_assignment { ($1) }
	|	list_of_defparam_assignments COMMA defparam_assignment { TUPLE3($1,COMMA,$3) }

defparam_assignment: id DOT id EQUALS expr { TUPLE5($1,DOT,$3,EQUALS,$5) }

etcInst: instDecl { ($1) }
	|	gateDecl { ($1) }

instDecl: id parameter_value_assignmentE /* 18 */ instnameList SEMICOLON { TUPLE4($1,$2,$3,SEMICOLON) }
	|	id DOT id /* 19 */ mpInstnameList SEMICOLON { TUPLE5($1,DOT,$3,$4,SEMICOLON) }

mpInstnameList: mpInstnameParen { ($1) }
	|	mpInstnameList COMMA mpInstnameParen { TUPLE3($1,COMMA,$3) }

mpInstnameParen: id instRangeE sigAttrListE { TUPLE3($1,$2,$3) }

instnameList: instnameParen { ($1) }
	|	instnameList COMMA instnameParen { TUPLE3($1,COMMA,$3) }

instnameParen: id instRangeE LPAREN cellpinList RPAREN { TUPLE5($1,$2,LPAREN,$4,RPAREN) }
	|	id instRangeE { TUPLE2($1,$2) }

instRangeE: /* empty */ { EMPTY_TOKEN }
	|	LBRACK constExpr RBRACK { TUPLE3(LBRACK,$2,RBRACK) }
	|	LBRACK constExpr COLON constExpr RBRACK { TUPLE5(LBRACK,$2,COLON,$4,RBRACK) }

cellparamList: /* 20 */ cellparamItList { ($1) }

cellpinList: /* 21 */ cellpinItList { ($1) }

cellparamItList: cellparamItemE { ($1) }
	|	cellparamItList COMMA cellparamItemE { TUPLE3($1,COMMA,$3) }

cellpinItList: cellpinItemE { ($1) }
	|	cellpinItList COMMA cellpinItemE { TUPLE3($1,COMMA,$3) }

cellparamItemE: /* empty */ { EMPTY_TOKEN }
	|	Q_DOT__STAR_ { (Q_DOT__STAR_) }
	|	DOT idSVKwd { TUPLE2(DOT,$2) }
	|	DOT idAny { TUPLE2(DOT,$2) }
	|	DOT idAny LPAREN RPAREN { TUPLE4(DOT,$2,LPAREN,RPAREN) }
	|	DOT idAny LPAREN expr RPAREN { TUPLE5(DOT,$2,LPAREN,$4,RPAREN) }
	|	DOT idAny LPAREN data_type RPAREN { TUPLE5(DOT,$2,LPAREN,$4,RPAREN) }
	|	data_type { ($1) }
	|	expr { ($1) }

cellpinItemE: /* empty */ { EMPTY_TOKEN }
	|	Q_DOT__STAR_ { (Q_DOT__STAR_) }
	|	DOT idSVKwd { TUPLE2(DOT,$2) }
	|	DOT idAny { TUPLE2(DOT,$2) }
	|	DOT idAny LPAREN RPAREN { TUPLE4(DOT,$2,LPAREN,RPAREN) }
	|	DOT idAny LPAREN expr RPAREN { TUPLE5(DOT,$2,LPAREN,$4,RPAREN) }
	|	expr { ($1) }

attr_event_control: AT LPAREN event_expression RPAREN { TUPLE4(AT,LPAREN,$3,RPAREN) }
	|	AT LPAREN STAR RPAREN { TUPLE4(AT,LPAREN,STAR,RPAREN) }
	|	AT STAR { TUPLE2(AT,STAR) }

event_controlE: /* empty */ { EMPTY_TOKEN }
	|	event_control { ($1) }

event_control: AT LPAREN event_expression RPAREN { TUPLE4(AT,LPAREN,$3,RPAREN) }
	|	AT LPAREN STAR RPAREN { TUPLE4(AT,LPAREN,STAR,RPAREN) }
	|	AT STAR { TUPLE2(AT,STAR) }
	|	AT senitemVar { TUPLE2(AT,$2) }

event_expression: senitem { ($1) }
	|	event_expression OR senitem { TUPLE3($1,OR,$3) }
	|	event_expression COMMA senitem { TUPLE3($1,COMMA,$3) }

senitem: senitemEdge { ($1) }
	|	senitemVar { ($1) }
	|	LPAREN senitem RPAREN { TUPLE3(LPAREN,$2,RPAREN) }
	|	LBRACE event_expression RBRACE { TUPLE3(LBRACE,$2,RBRACE) }
	|	senitem Q_AMPERSAND__AMPERSAND_ senitem { TUPLE3($1,Q_AMPERSAND__AMPERSAND_,$3) }
	|	INTEGER_BLANK_NUMBER { (INTEGER_BLANK_NUMBER) }
	|	FLOATING_HYPHEN_POINT_BLANK_NUMBER { (FLOATING_HYPHEN_POINT_BLANK_NUMBER) }

senitemVar: idClassSel { ($1) }

senitemEdge: POSEDGE idClassSel { TUPLE2(POSEDGE,$2) }
	|	NEGEDGE idClassSel { TUPLE2(NEGEDGE,$2) }
	|	EDGE idClassSel { TUPLE2(EDGE,$2) }
	|	POSEDGE LPAREN idClassSel RPAREN { TUPLE4(POSEDGE,LPAREN,$3,RPAREN) }
	|	NEGEDGE LPAREN idClassSel RPAREN { TUPLE4(NEGEDGE,LPAREN,$3,RPAREN) }
	|	EDGE LPAREN idClassSel RPAREN { TUPLE4(EDGE,LPAREN,$3,RPAREN) }

stmtBlock: stmt { ($1) }

seq_block: seq_blockFront blockDeclStmtList END endLabelE { TUPLE4($1,$2,END,$4) }
	|	seq_blockFront END endLabelE { TUPLE3($1,END,$3) }

seq_blockFront: BEGIN { (BEGIN) }
	|	BEGIN COLON idAny { TUPLE3(BEGIN,COLON,$3) }

blockDeclStmtList: block_item_declarationList { ($1) }
	|	block_item_declarationList stmtList { TUPLE2($1,$2) }
	|	stmtList { ($1) }

block_item_declarationList: block_item_declaration { ($1) }
	|	block_item_declarationList block_item_declaration { TUPLE2($1,$2) }

block_item_declaration: data_declaration { ($1) }
	|	local_parameter_declaration SEMICOLON { TUPLE2($1,SEMICOLON) }
	|	parameter_declaration SEMICOLON { TUPLE2($1,SEMICOLON) }

stmtList: stmtBlock { ($1) }
	|	stmtList stmtBlock { TUPLE2($1,$2) }

stmt: statement_item { ($1) }
	|	labeledStmt { ($1) }
	|	id COLON labeledStmt { TUPLE3($1,COLON,$3) }
	|	SEMICOLON { (SEMICOLON) }

statement_item: foperator_assignment SEMICOLON { TUPLE2($1,SEMICOLON) }
	|	fexprLvalue Q_LESS__EQUALS_ delayE expr SEMICOLON { TUPLE5($1,Q_LESS__EQUALS_,$3,$4,SEMICOLON) }
	|	ASSIGN idClassSel EQUALS delayE expr SEMICOLON { TUPLE6(ASSIGN,$2,EQUALS,$4,$5,SEMICOLON) }
	|	DEASSIGN variable_lvalue SEMICOLON { TUPLE3(DEASSIGN,$2,SEMICOLON) }
	|	unique_priorityE caseStart caseAttrE case_itemListE ENDCASE { TUPLE5($1,$2,$3,$4,ENDCASE) }
	|	unique_priorityE caseStart caseAttrE INSIDE case_insideListE ENDCASE { TUPLE6($1,$2,$3,INSIDE,$5,ENDCASE) }
	|	unique_priorityE IF LPAREN expr RPAREN stmtBlock { TUPLE6($1,IF,LPAREN,$4,RPAREN,$6) }
	|	unique_priorityE IF LPAREN expr RPAREN stmtBlock ELSE stmtBlock { TUPLE8($1,IF,LPAREN,$4,RPAREN,$6,ELSE,$8) }
	|	finc_or_dec_expression SEMICOLON { TUPLE2($1,SEMICOLON) }
	|	task_subroutine_callNoMethod SEMICOLON { TUPLE2($1,SEMICOLON) }
	|	fexpr DOT task_subroutine_callNoMethod SEMICOLON { TUPLE4($1,DOT,$3,SEMICOLON) }
	|	statementVerilatorPragmas { ($1) }
	|	DISABLE idAny SEMICOLON { TUPLE3(DISABLE,$2,SEMICOLON) }
	|	FOREVER stmtBlock { TUPLE2(FOREVER,$2) }
	|	REPEAT LPAREN expr RPAREN stmtBlock { TUPLE5(REPEAT,LPAREN,$3,RPAREN,$5) }
	|	WHILE LPAREN expr RPAREN stmtBlock { TUPLE5(WHILE,LPAREN,$3,RPAREN,$5) }
	|	statementFor { ($1) }
	|	DO stmtBlock WHILE LPAREN expr RPAREN SEMICOLON { TUPLE7(DO,$2,WHILE,LPAREN,$5,RPAREN,SEMICOLON) }
	|	FOREACH LPAREN idClassForeach LBRACK loop_variables RBRACK RPAREN stmtBlock { TUPLE8(FOREACH,LPAREN,$3,LBRACK,$5,RBRACK,RPAREN,$8) }
	|	RETURN SEMICOLON { TUPLE2(RETURN,SEMICOLON) }
	|	RETURN expr SEMICOLON { TUPLE3(RETURN,$2,SEMICOLON) }
	|	BREAK SEMICOLON { TUPLE2(BREAK,SEMICOLON) }
	|	CONTINUE SEMICOLON { TUPLE2(CONTINUE,SEMICOLON) }
	|	delay_control stmtBlock { TUPLE2($1,$2) }
	|	seq_block { ($1) }
	|	concurrent_assertion_item { ($1) }
	|	ERROR_TOKEN SEMICOLON { TUPLE2(ERROR_TOKEN,SEMICOLON) }

statementFor: FOR LPAREN for_initialization expr SEMICOLON for_stepE RPAREN stmtBlock { TUPLE8(FOR,LPAREN,$3,$4,SEMICOLON,$6,RPAREN,$8) }
	|	FOR LPAREN for_initialization SEMICOLON for_stepE RPAREN stmtBlock { TUPLE7(FOR,LPAREN,$3,SEMICOLON,$5,RPAREN,$7) }

statementVerilatorPragmas: Q_SLASH__STAR_VERILATOR_BLANK_COVERAGE_UNDERSCORE_BLOCK_UNDERSCORE_OFF_STAR__SLASH_ { (Q_SLASH__STAR_VERILATOR_BLANK_COVERAGE_UNDERSCORE_BLOCK_UNDERSCORE_OFF_STAR__SLASH_) }

foperator_assignment: fexprLvalue EQUALS delayE expr { TUPLE4($1,EQUALS,$3,$4) }
	|	fexprLvalue EQUALS Q_DOLLAR_FOPEN LPAREN expr RPAREN { TUPLE6($1,EQUALS,Q_DOLLAR_FOPEN,LPAREN,$5,RPAREN) }
	|	fexprLvalue EQUALS Q_DOLLAR_FOPEN LPAREN expr COMMA expr RPAREN { TUPLE8($1,EQUALS,Q_DOLLAR_FOPEN,LPAREN,$5,COMMA,$7,RPAREN) }
	|	fexprLvalue Q_PLUS__EQUALS_ expr { TUPLE3($1,Q_PLUS__EQUALS_,$3) }
	|	fexprLvalue Q_HYPHEN__EQUALS_ expr { TUPLE3($1,Q_HYPHEN__EQUALS_,$3) }
	|	fexprLvalue Q_STAR__EQUALS_ expr { TUPLE3($1,Q_STAR__EQUALS_,$3) }
	|	fexprLvalue Q_SLASH__EQUALS_ expr { TUPLE3($1,Q_SLASH__EQUALS_,$3) }
	|	fexprLvalue Q_PERCENT__EQUALS_ expr { TUPLE3($1,Q_PERCENT__EQUALS_,$3) }
	|	fexprLvalue Q_AMPERSAND__EQUALS_ expr { TUPLE3($1,Q_AMPERSAND__EQUALS_,$3) }
	|	fexprLvalue Q_VBAR__EQUALS_ expr { TUPLE3($1,Q_VBAR__EQUALS_,$3) }
	|	fexprLvalue Q_CARET__EQUALS_ expr { TUPLE3($1,Q_CARET__EQUALS_,$3) }
	|	fexprLvalue Q_LESS__LESS__EQUALS_ expr { TUPLE3($1,Q_LESS__LESS__EQUALS_,$3) }
	|	fexprLvalue Q_GREATER__GREATER__EQUALS_ expr { TUPLE3($1,Q_GREATER__GREATER__EQUALS_,$3) }
	|	fexprLvalue Q_GREATER__GREATER__GREATER__EQUALS_ expr { TUPLE3($1,Q_GREATER__GREATER__GREATER__EQUALS_,$3) }

finc_or_dec_expression: fexprLvalue Q_PLUS__PLUS_ { TUPLE2($1,Q_PLUS__PLUS_) }
	|	fexprLvalue Q_HYPHEN__HYPHEN_ { TUPLE2($1,Q_HYPHEN__HYPHEN_) }
	|	Q_PLUS__PLUS_ fexprLvalue { TUPLE2(Q_PLUS__PLUS_,$2) }
	|	Q_HYPHEN__HYPHEN_ fexprLvalue { TUPLE2(Q_HYPHEN__HYPHEN_,$2) }

unique_priorityE: /* empty */ { EMPTY_TOKEN }
	|	PRIORITY { (PRIORITY) }
	|	UNIQUE { (UNIQUE) }
	|	UNIQUE0 { (UNIQUE0) }

caseStart: CASE LPAREN expr RPAREN { TUPLE4(CASE,LPAREN,$3,RPAREN) }
	|	CASEX LPAREN expr RPAREN { TUPLE4(CASEX,LPAREN,$3,RPAREN) }
	|	CASEZ LPAREN expr RPAREN { TUPLE4(CASEZ,LPAREN,$3,RPAREN) }

caseAttrE: /* empty */ { EMPTY_TOKEN }
	|	caseAttrE Q_SLASH__STAR_VERILATOR_BLANK_FULL_UNDERSCORE_CASE_STAR__SLASH_ { TUPLE2($1,Q_SLASH__STAR_VERILATOR_BLANK_FULL_UNDERSCORE_CASE_STAR__SLASH_) }
	|	caseAttrE Q_SLASH__STAR_VERILATOR_BLANK_PARALLEL_UNDERSCORE_CASE_STAR__SLASH_ { TUPLE2($1,Q_SLASH__STAR_VERILATOR_BLANK_PARALLEL_UNDERSCORE_CASE_STAR__SLASH_) }

case_itemListE: /* empty */ { EMPTY_TOKEN }
	|	case_itemList { ($1) }

case_insideListE: /* empty */ { EMPTY_TOKEN }
	|	case_inside_itemList { ($1) }

case_itemList: caseCondList COLON stmtBlock { TUPLE3($1,COLON,$3) }
	|	DEFAULT COLON stmtBlock { TUPLE3(DEFAULT,COLON,$3) }
	|	DEFAULT stmtBlock { TUPLE2(DEFAULT,$2) }
	|	case_itemList caseCondList COLON stmtBlock { TUPLE4($1,$2,COLON,$4) }
	|	case_itemList DEFAULT stmtBlock { TUPLE3($1,DEFAULT,$3) }
	|	case_itemList DEFAULT COLON stmtBlock { TUPLE4($1,DEFAULT,COLON,$4) }

case_inside_itemList: open_range_list COLON stmtBlock { TUPLE3($1,COLON,$3) }
	|	DEFAULT COLON stmtBlock { TUPLE3(DEFAULT,COLON,$3) }
	|	DEFAULT stmtBlock { TUPLE2(DEFAULT,$2) }
	|	case_inside_itemList open_range_list COLON stmtBlock { TUPLE4($1,$2,COLON,$4) }
	|	case_inside_itemList DEFAULT stmtBlock { TUPLE3($1,DEFAULT,$3) }
	|	case_inside_itemList DEFAULT COLON stmtBlock { TUPLE4($1,DEFAULT,COLON,$4) }

open_range_list: open_value_range { CONS1 ($1) }
	|	open_range_list COMMA open_value_range { CONS3($1,COMMA,$3) }

open_value_range: value_range { ($1) }

value_range: expr { ($1) }
	|	LBRACK expr COLON expr RBRACK { TUPLE5(LBRACK,$2,COLON,$4,RBRACK) }

caseCondList: expr { ($1) }
	|	caseCondList COMMA expr { TUPLE3($1,COMMA,$3) }

patternNoExpr: DOT id { TUPLE2(DOT,$2) }
	|	Q_DOT__STAR_ { (Q_DOT__STAR_) }

patternList: patternOne { ($1) }
	|	patternList COMMA patternOne { TUPLE3($1,COMMA,$3) }

patternOne: expr { ($1) }
	|	expr LBRACE argsExprList RBRACE { TUPLE4($1,LBRACE,$3,RBRACE) }
	|	patternNoExpr { ($1) }

patternMemberList: patternMemberOne { ($1) }
	|	patternMemberList COMMA patternMemberOne { TUPLE3($1,COMMA,$3) }

patternMemberOne: patternKey COLON expr { TUPLE3($1,COLON,$3) }
	|	patternKey COLON patternNoExpr { TUPLE3($1,COLON,$3) }
	|	DEFAULT COLON expr { TUPLE3(DEFAULT,COLON,$3) }
	|	DEFAULT COLON patternNoExpr { TUPLE3(DEFAULT,COLON,$3) }

patternKey: INTEGER_BLANK_NUMBER { (INTEGER_BLANK_NUMBER) }
	|	FLOATING_HYPHEN_POINT_BLANK_NUMBER { (FLOATING_HYPHEN_POINT_BLANK_NUMBER) }
	|	IDENTIFIER { (IDENTIFIER $1) }

assignment_pattern: Q_QUOTE__LBRACE_ patternList RBRACE { TUPLE3(Q_QUOTE__LBRACE_,$2,RBRACE) }
	|	Q_QUOTE__LBRACE_ patternMemberList RBRACE { TUPLE3(Q_QUOTE__LBRACE_,$2,RBRACE) }
	|	Q_QUOTE__LBRACE_ RBRACE { TUPLE2(Q_QUOTE__LBRACE_,RBRACE) }

for_initialization: for_initializationItemList SEMICOLON { TUPLE2($1,SEMICOLON) }
	|	SEMICOLON { (SEMICOLON) }

for_initializationItemList: for_initializationItem { ($1) }
	|	for_initializationItemList COMMA for_initializationItem { TUPLE3($1,COMMA,$3) }

for_initializationItem: data_type idAny EQUALS expr { TUPLE4($1,$2,EQUALS,$4) }
	|	VAR data_type idAny EQUALS expr { TUPLE5(VAR,$2,$3,EQUALS,$5) }
	|	varRefBase EQUALS expr { TUPLE3($1,EQUALS,$3) }

for_stepE: /* empty */ { EMPTY_TOKEN }
	|	for_step { ($1) }

for_step: genvar_iteration { ($1) }
	|	for_step COMMA genvar_iteration { TUPLE3($1,COMMA,$3) }

loop_variables: varRefBase { ($1) }
	|	loop_variables COMMA varRefBase { TUPLE3($1,COMMA,$3) }

taskRef: id { ($1) }
	|	id LPAREN list_of_argumentsE RPAREN { TUPLE4($1,LPAREN,$3,RPAREN) }
	|	package_scopeIdFollows id LPAREN list_of_argumentsE RPAREN { TUPLE5($1,$2,LPAREN,$4,RPAREN) }

funcRef: id LPAREN list_of_argumentsE RPAREN { TUPLE4($1,LPAREN,$3,RPAREN) }
	|	package_scopeIdFollows id LPAREN list_of_argumentsE RPAREN { TUPLE5($1,$2,LPAREN,$4,RPAREN) }

task_subroutine_callNoMethod: taskRef { ($1) }
	|	system_t_call { ($1) }

function_subroutine_callNoMethod: funcRef { ($1) }
	|	system_f_call { ($1) }

system_t_call: Q_DOLLAR__LBRACE_IGNORED_HYPHEN_BBOX_HYPHEN_SYS_RBRACE_ parenE { TUPLE2(Q_DOLLAR__LBRACE_IGNORED_HYPHEN_BBOX_HYPHEN_SYS_RBRACE_,$2) }
	|	Q_DOLLAR__LBRACE_IGNORED_HYPHEN_BBOX_HYPHEN_SYS_RBRACE_ LPAREN exprList RPAREN { TUPLE4(Q_DOLLAR__LBRACE_IGNORED_HYPHEN_BBOX_HYPHEN_SYS_RBRACE_,LPAREN,$3,RPAREN) }
	|	Q_DOLLAR__LBRACE_DPI_HYPHEN_SYS_RBRACE_ parenE { TUPLE2(Q_DOLLAR__LBRACE_DPI_HYPHEN_SYS_RBRACE_,$2) }
	|	Q_DOLLAR__LBRACE_DPI_HYPHEN_SYS_RBRACE_ LPAREN exprList RPAREN { TUPLE4(Q_DOLLAR__LBRACE_DPI_HYPHEN_SYS_RBRACE_,LPAREN,$3,RPAREN) }
	|	Q_DOLLAR_C LPAREN cStrList RPAREN { TUPLE4(Q_DOLLAR_C,LPAREN,$3,RPAREN) }
	|	Q_DOLLAR_SYSTEM LPAREN expr RPAREN { TUPLE4(Q_DOLLAR_SYSTEM,LPAREN,$3,RPAREN) }
	|	Q_DOLLAR_FCLOSE LPAREN idClassSel RPAREN { TUPLE4(Q_DOLLAR_FCLOSE,LPAREN,$3,RPAREN) }
	|	Q_DOLLAR_FFLUSH parenE { TUPLE2(Q_DOLLAR_FFLUSH,$2) }
	|	Q_DOLLAR_FFLUSH LPAREN expr RPAREN { TUPLE4(Q_DOLLAR_FFLUSH,LPAREN,$3,RPAREN) }
	|	Q_DOLLAR_FINISH parenE { TUPLE2(Q_DOLLAR_FINISH,$2) }
	|	Q_DOLLAR_FINISH LPAREN expr RPAREN { TUPLE4(Q_DOLLAR_FINISH,LPAREN,$3,RPAREN) }
	|	Q_DOLLAR_STOP parenE { TUPLE2(Q_DOLLAR_STOP,$2) }
	|	Q_DOLLAR_STOP LPAREN expr RPAREN { TUPLE4(Q_DOLLAR_STOP,LPAREN,$3,RPAREN) }
	|	Q_DOLLAR_SFORMAT LPAREN expr COMMA str commaEListE RPAREN { TUPLE7(Q_DOLLAR_SFORMAT,LPAREN,$3,COMMA,$5,$6,RPAREN) }
	|	Q_DOLLAR_SWRITE LPAREN expr COMMA str commaEListE RPAREN { TUPLE7(Q_DOLLAR_SWRITE,LPAREN,$3,COMMA,$5,$6,RPAREN) }
	|	Q_DOLLAR_DISPLAY parenE { TUPLE2(Q_DOLLAR_DISPLAY,$2) }
	|	Q_DOLLAR_DISPLAY LPAREN exprList RPAREN { TUPLE4(Q_DOLLAR_DISPLAY,LPAREN,$3,RPAREN) }
	|	Q_DOLLAR_WRITE parenE { TUPLE2(Q_DOLLAR_WRITE,$2) }
	|	Q_DOLLAR_WRITE LPAREN exprList RPAREN { TUPLE4(Q_DOLLAR_WRITE,LPAREN,$3,RPAREN) }
	|	Q_DOLLAR_FDISPLAY LPAREN expr RPAREN { TUPLE4(Q_DOLLAR_FDISPLAY,LPAREN,$3,RPAREN) }
	|	Q_DOLLAR_FDISPLAY LPAREN expr COMMA exprListE RPAREN { TUPLE6(Q_DOLLAR_FDISPLAY,LPAREN,$3,COMMA,$5,RPAREN) }
	|	Q_DOLLAR_FWRITE LPAREN expr COMMA exprListE RPAREN { TUPLE6(Q_DOLLAR_FWRITE,LPAREN,$3,COMMA,$5,RPAREN) }
	|	Q_DOLLAR_INFO parenE { TUPLE2(Q_DOLLAR_INFO,$2) }
	|	Q_DOLLAR_INFO LPAREN str commaEListE RPAREN { TUPLE5(Q_DOLLAR_INFO,LPAREN,$3,$4,RPAREN) }
	|	Q_DOLLAR_WARNING parenE { TUPLE2(Q_DOLLAR_WARNING,$2) }
	|	Q_DOLLAR_WARNING LPAREN str commaEListE RPAREN { TUPLE5(Q_DOLLAR_WARNING,LPAREN,$3,$4,RPAREN) }
	|	Q_DOLLAR_ERROR parenE { TUPLE2(Q_DOLLAR_ERROR,$2) }
	|	Q_DOLLAR_ERROR LPAREN str commaEListE RPAREN { TUPLE5(Q_DOLLAR_ERROR,LPAREN,$3,$4,RPAREN) }
	|	Q_DOLLAR_FATAL parenE { TUPLE2(Q_DOLLAR_FATAL,$2) }
	|	Q_DOLLAR_FATAL LPAREN expr RPAREN { TUPLE4(Q_DOLLAR_FATAL,LPAREN,$3,RPAREN) }
	|	Q_DOLLAR_FATAL LPAREN expr COMMA str commaEListE RPAREN { TUPLE7(Q_DOLLAR_FATAL,LPAREN,$3,COMMA,$5,$6,RPAREN) }
	|	Q_DOLLAR_READMEMB LPAREN expr COMMA idClassSel RPAREN { TUPLE6(Q_DOLLAR_READMEMB,LPAREN,$3,COMMA,$5,RPAREN) }
	|	Q_DOLLAR_READMEMB LPAREN expr COMMA idClassSel COMMA expr RPAREN { TUPLE8(Q_DOLLAR_READMEMB,LPAREN,$3,COMMA,$5,COMMA,$7,RPAREN) }
	|	Q_DOLLAR_READMEMB LPAREN expr COMMA idClassSel COMMA expr COMMA expr RPAREN { TUPLE10(Q_DOLLAR_READMEMB,LPAREN,$3,COMMA,$5,COMMA,$7,COMMA,$9,RPAREN) }
	|	Q_DOLLAR_READMEMH LPAREN expr COMMA idClassSel RPAREN { TUPLE6(Q_DOLLAR_READMEMH,LPAREN,$3,COMMA,$5,RPAREN) }
	|	Q_DOLLAR_READMEMH LPAREN expr COMMA idClassSel COMMA expr RPAREN { TUPLE8(Q_DOLLAR_READMEMH,LPAREN,$3,COMMA,$5,COMMA,$7,RPAREN) }
	|	Q_DOLLAR_READMEMH LPAREN expr COMMA idClassSel COMMA expr COMMA expr RPAREN { TUPLE10(Q_DOLLAR_READMEMH,LPAREN,$3,COMMA,$5,COMMA,$7,COMMA,$9,RPAREN) }
	|	Q_DOLLAR_WRITEMEMH LPAREN expr COMMA idClassSel RPAREN { TUPLE6(Q_DOLLAR_WRITEMEMH,LPAREN,$3,COMMA,$5,RPAREN) }
	|	Q_DOLLAR_WRITEMEMH LPAREN expr COMMA idClassSel COMMA expr RPAREN { TUPLE8(Q_DOLLAR_WRITEMEMH,LPAREN,$3,COMMA,$5,COMMA,$7,RPAREN) }
	|	Q_DOLLAR_WRITEMEMH LPAREN expr COMMA idClassSel COMMA expr COMMA expr RPAREN { TUPLE10(Q_DOLLAR_WRITEMEMH,LPAREN,$3,COMMA,$5,COMMA,$7,COMMA,$9,RPAREN) }
	|	system_f_call_or_t { ($1) }

system_f_call: Q_DOLLAR__LBRACE_IGNORED_HYPHEN_BBOX_HYPHEN_SYS_RBRACE_ parenE { TUPLE2(Q_DOLLAR__LBRACE_IGNORED_HYPHEN_BBOX_HYPHEN_SYS_RBRACE_,$2) }
	|	Q_DOLLAR__LBRACE_IGNORED_HYPHEN_BBOX_HYPHEN_SYS_RBRACE_ LPAREN exprList RPAREN { TUPLE4(Q_DOLLAR__LBRACE_IGNORED_HYPHEN_BBOX_HYPHEN_SYS_RBRACE_,LPAREN,$3,RPAREN) }
	|	Q_DOLLAR__LBRACE_DPI_HYPHEN_SYS_RBRACE_ parenE { TUPLE2(Q_DOLLAR__LBRACE_DPI_HYPHEN_SYS_RBRACE_,$2) }
	|	Q_DOLLAR__LBRACE_DPI_HYPHEN_SYS_RBRACE_ LPAREN exprList RPAREN { TUPLE4(Q_DOLLAR__LBRACE_DPI_HYPHEN_SYS_RBRACE_,LPAREN,$3,RPAREN) }
	|	Q_DOLLAR_C LPAREN cStrList RPAREN { TUPLE4(Q_DOLLAR_C,LPAREN,$3,RPAREN) }
	|	Q_DOLLAR_SYSTEM LPAREN expr RPAREN { TUPLE4(Q_DOLLAR_SYSTEM,LPAREN,$3,RPAREN) }
	|	system_f_call_or_t { ($1) }

system_f_call_or_t: Q_DOLLAR_ACOS LPAREN expr RPAREN { TUPLE4(Q_DOLLAR_ACOS,LPAREN,$3,RPAREN) }
	|	Q_DOLLAR_ACOSH LPAREN expr RPAREN { TUPLE4(Q_DOLLAR_ACOSH,LPAREN,$3,RPAREN) }
	|	Q_DOLLAR_ASIN LPAREN expr RPAREN { TUPLE4(Q_DOLLAR_ASIN,LPAREN,$3,RPAREN) }
	|	Q_DOLLAR_ASINH LPAREN expr RPAREN { TUPLE4(Q_DOLLAR_ASINH,LPAREN,$3,RPAREN) }
	|	Q_DOLLAR_ATAN LPAREN expr RPAREN { TUPLE4(Q_DOLLAR_ATAN,LPAREN,$3,RPAREN) }
	|	Q_DOLLAR_ATAN2 LPAREN expr COMMA expr RPAREN { TUPLE6(Q_DOLLAR_ATAN2,LPAREN,$3,COMMA,$5,RPAREN) }
	|	Q_DOLLAR_ATANH LPAREN expr RPAREN { TUPLE4(Q_DOLLAR_ATANH,LPAREN,$3,RPAREN) }
	|	Q_DOLLAR_BITS LPAREN exprOrDataType RPAREN { TUPLE4(Q_DOLLAR_BITS,LPAREN,$3,RPAREN) }
	|	Q_DOLLAR_BITS LPAREN exprOrDataType COMMA expr RPAREN { TUPLE6(Q_DOLLAR_BITS,LPAREN,$3,COMMA,$5,RPAREN) }
	|	Q_DOLLAR_BITSTOREAL LPAREN expr RPAREN { TUPLE4(Q_DOLLAR_BITSTOREAL,LPAREN,$3,RPAREN) }
	|	Q_DOLLAR_CEIL LPAREN expr RPAREN { TUPLE4(Q_DOLLAR_CEIL,LPAREN,$3,RPAREN) }
	|	Q_DOLLAR_CLOG2 LPAREN expr RPAREN { TUPLE4(Q_DOLLAR_CLOG2,LPAREN,$3,RPAREN) }
	|	Q_DOLLAR_COS LPAREN expr RPAREN { TUPLE4(Q_DOLLAR_COS,LPAREN,$3,RPAREN) }
	|	Q_DOLLAR_COSH LPAREN expr RPAREN { TUPLE4(Q_DOLLAR_COSH,LPAREN,$3,RPAREN) }
	|	Q_DOLLAR_COUNTONES LPAREN expr RPAREN { TUPLE4(Q_DOLLAR_COUNTONES,LPAREN,$3,RPAREN) }
	|	Q_DOLLAR_DIMENSIONS LPAREN exprOrDataType RPAREN { TUPLE4(Q_DOLLAR_DIMENSIONS,LPAREN,$3,RPAREN) }
	|	Q_DOLLAR_EXP LPAREN expr RPAREN { TUPLE4(Q_DOLLAR_EXP,LPAREN,$3,RPAREN) }
	|	Q_DOLLAR_FEOF LPAREN expr RPAREN { TUPLE4(Q_DOLLAR_FEOF,LPAREN,$3,RPAREN) }
	|	Q_DOLLAR_FGETC LPAREN expr RPAREN { TUPLE4(Q_DOLLAR_FGETC,LPAREN,$3,RPAREN) }
	|	Q_DOLLAR_FGETS LPAREN idClassSel COMMA expr RPAREN { TUPLE6(Q_DOLLAR_FGETS,LPAREN,$3,COMMA,$5,RPAREN) }
	|	Q_DOLLAR_FLOOR LPAREN expr RPAREN { TUPLE4(Q_DOLLAR_FLOOR,LPAREN,$3,RPAREN) }
	|	Q_DOLLAR_FSCANF LPAREN expr COMMA str commaVRDListE RPAREN { TUPLE7(Q_DOLLAR_FSCANF,LPAREN,$3,COMMA,$5,$6,RPAREN) }
	|	Q_DOLLAR_HIGH LPAREN exprOrDataType RPAREN { TUPLE4(Q_DOLLAR_HIGH,LPAREN,$3,RPAREN) }
	|	Q_DOLLAR_HIGH LPAREN exprOrDataType COMMA expr RPAREN { TUPLE6(Q_DOLLAR_HIGH,LPAREN,$3,COMMA,$5,RPAREN) }
	|	Q_DOLLAR_HYPOT LPAREN expr COMMA expr RPAREN { TUPLE6(Q_DOLLAR_HYPOT,LPAREN,$3,COMMA,$5,RPAREN) }
	|	Q_DOLLAR_INCREMENT LPAREN exprOrDataType RPAREN { TUPLE4(Q_DOLLAR_INCREMENT,LPAREN,$3,RPAREN) }
	|	Q_DOLLAR_INCREMENT LPAREN exprOrDataType COMMA expr RPAREN { TUPLE6(Q_DOLLAR_INCREMENT,LPAREN,$3,COMMA,$5,RPAREN) }
	|	Q_DOLLAR_ISUNKNOWN LPAREN expr RPAREN { TUPLE4(Q_DOLLAR_ISUNKNOWN,LPAREN,$3,RPAREN) }
	|	Q_DOLLAR_ITOR LPAREN expr RPAREN { TUPLE4(Q_DOLLAR_ITOR,LPAREN,$3,RPAREN) }
	|	Q_DOLLAR_LEFT LPAREN exprOrDataType RPAREN { TUPLE4(Q_DOLLAR_LEFT,LPAREN,$3,RPAREN) }
	|	Q_DOLLAR_LEFT LPAREN exprOrDataType COMMA expr RPAREN { TUPLE6(Q_DOLLAR_LEFT,LPAREN,$3,COMMA,$5,RPAREN) }
	|	Q_DOLLAR_LN LPAREN expr RPAREN { TUPLE4(Q_DOLLAR_LN,LPAREN,$3,RPAREN) }
	|	Q_DOLLAR_LOG10 LPAREN expr RPAREN { TUPLE4(Q_DOLLAR_LOG10,LPAREN,$3,RPAREN) }
	|	Q_DOLLAR_LOW LPAREN exprOrDataType RPAREN { TUPLE4(Q_DOLLAR_LOW,LPAREN,$3,RPAREN) }
	|	Q_DOLLAR_LOW LPAREN exprOrDataType COMMA expr RPAREN { TUPLE6(Q_DOLLAR_LOW,LPAREN,$3,COMMA,$5,RPAREN) }
	|	Q_DOLLAR_ONEHOT LPAREN expr RPAREN { TUPLE4(Q_DOLLAR_ONEHOT,LPAREN,$3,RPAREN) }
	|	Q_DOLLAR_ONEHOT0 LPAREN expr RPAREN { TUPLE4(Q_DOLLAR_ONEHOT0,LPAREN,$3,RPAREN) }
	|	Q_DOLLAR_PAST LPAREN expr RPAREN { TUPLE4(Q_DOLLAR_PAST,LPAREN,$3,RPAREN) }
	|	Q_DOLLAR_PAST LPAREN expr COMMA expr RPAREN { TUPLE6(Q_DOLLAR_PAST,LPAREN,$3,COMMA,$5,RPAREN) }
	|	Q_DOLLAR_PAST LPAREN expr COMMA expr COMMA expr RPAREN { TUPLE8(Q_DOLLAR_PAST,LPAREN,$3,COMMA,$5,COMMA,$7,RPAREN) }
	|	Q_DOLLAR_PAST LPAREN expr COMMA expr COMMA expr COMMA expr RPAREN { TUPLE10(Q_DOLLAR_PAST,LPAREN,$3,COMMA,$5,COMMA,$7,COMMA,$9,RPAREN) }
	|	Q_DOLLAR_POW LPAREN expr COMMA expr RPAREN { TUPLE6(Q_DOLLAR_POW,LPAREN,$3,COMMA,$5,RPAREN) }
	|	Q_DOLLAR_RANDOM LPAREN expr RPAREN { TUPLE4(Q_DOLLAR_RANDOM,LPAREN,$3,RPAREN) }
	|	Q_DOLLAR_RANDOM parenE { TUPLE2(Q_DOLLAR_RANDOM,$2) }
	|	Q_DOLLAR_REALTIME parenE { TUPLE2(Q_DOLLAR_REALTIME,$2) }
	|	Q_DOLLAR_REALTOBITS LPAREN expr RPAREN { TUPLE4(Q_DOLLAR_REALTOBITS,LPAREN,$3,RPAREN) }
	|	Q_DOLLAR_RIGHT LPAREN exprOrDataType RPAREN { TUPLE4(Q_DOLLAR_RIGHT,LPAREN,$3,RPAREN) }
	|	Q_DOLLAR_RIGHT LPAREN exprOrDataType COMMA expr RPAREN { TUPLE6(Q_DOLLAR_RIGHT,LPAREN,$3,COMMA,$5,RPAREN) }
	|	Q_DOLLAR_RTOI LPAREN expr RPAREN { TUPLE4(Q_DOLLAR_RTOI,LPAREN,$3,RPAREN) }
	|	Q_DOLLAR_SFORMATF LPAREN str commaEListE RPAREN { TUPLE5(Q_DOLLAR_SFORMATF,LPAREN,$3,$4,RPAREN) }
	|	Q_DOLLAR_SIGNED LPAREN expr RPAREN { TUPLE4(Q_DOLLAR_SIGNED,LPAREN,$3,RPAREN) }
	|	Q_DOLLAR_SIN LPAREN expr RPAREN { TUPLE4(Q_DOLLAR_SIN,LPAREN,$3,RPAREN) }
	|	Q_DOLLAR_SINH LPAREN expr RPAREN { TUPLE4(Q_DOLLAR_SINH,LPAREN,$3,RPAREN) }
	|	Q_DOLLAR_SIZE LPAREN exprOrDataType RPAREN { TUPLE4(Q_DOLLAR_SIZE,LPAREN,$3,RPAREN) }
	|	Q_DOLLAR_SIZE LPAREN exprOrDataType COMMA expr RPAREN { TUPLE6(Q_DOLLAR_SIZE,LPAREN,$3,COMMA,$5,RPAREN) }
	|	Q_DOLLAR_SQRT LPAREN expr RPAREN { TUPLE4(Q_DOLLAR_SQRT,LPAREN,$3,RPAREN) }
	|	Q_DOLLAR_SSCANF LPAREN expr COMMA str commaVRDListE RPAREN { TUPLE7(Q_DOLLAR_SSCANF,LPAREN,$3,COMMA,$5,$6,RPAREN) }
	|	Q_DOLLAR_STIME parenE { TUPLE2(Q_DOLLAR_STIME,$2) }
	|	Q_DOLLAR_TAN LPAREN expr RPAREN { TUPLE4(Q_DOLLAR_TAN,LPAREN,$3,RPAREN) }
	|	Q_DOLLAR_TANH LPAREN expr RPAREN { TUPLE4(Q_DOLLAR_TANH,LPAREN,$3,RPAREN) }
	|	Q_DOLLAR_TEST_DOLLAR_PLUSARGS LPAREN str RPAREN { TUPLE4(Q_DOLLAR_TEST_DOLLAR_PLUSARGS,LPAREN,$3,RPAREN) }
	|	Q_DOLLAR_TIME parenE { TUPLE2(Q_DOLLAR_TIME,$2) }
	|	Q_DOLLAR_UNPACKED_UNDERSCORE_DIMENSIONS LPAREN exprOrDataType RPAREN { TUPLE4(Q_DOLLAR_UNPACKED_UNDERSCORE_DIMENSIONS,LPAREN,$3,RPAREN) }
	|	Q_DOLLAR_UNSIGNED LPAREN expr RPAREN { TUPLE4(Q_DOLLAR_UNSIGNED,LPAREN,$3,RPAREN) }
	|	Q_DOLLAR_VALUE_DOLLAR_PLUSARGS LPAREN expr COMMA expr RPAREN { TUPLE6(Q_DOLLAR_VALUE_DOLLAR_PLUSARGS,LPAREN,$3,COMMA,$5,RPAREN) }

elaboration_system_task: elaboration_system_task_guts SEMICOLON { TUPLE2($1,SEMICOLON) }

elaboration_system_task_guts: Q_DOLLAR_INFO parenE { TUPLE2(Q_DOLLAR_INFO,$2) }
	|	Q_DOLLAR_INFO LPAREN str commaEListE RPAREN { TUPLE5(Q_DOLLAR_INFO,LPAREN,$3,$4,RPAREN) }
	|	Q_DOLLAR_WARNING parenE { TUPLE2(Q_DOLLAR_WARNING,$2) }
	|	Q_DOLLAR_WARNING LPAREN str commaEListE RPAREN { TUPLE5(Q_DOLLAR_WARNING,LPAREN,$3,$4,RPAREN) }
	|	Q_DOLLAR_ERROR parenE { TUPLE2(Q_DOLLAR_ERROR,$2) }
	|	Q_DOLLAR_ERROR LPAREN str commaEListE RPAREN { TUPLE5(Q_DOLLAR_ERROR,LPAREN,$3,$4,RPAREN) }
	|	Q_DOLLAR_FATAL parenE { TUPLE2(Q_DOLLAR_FATAL,$2) }
	|	Q_DOLLAR_FATAL LPAREN expr RPAREN { TUPLE4(Q_DOLLAR_FATAL,LPAREN,$3,RPAREN) }
	|	Q_DOLLAR_FATAL LPAREN expr COMMA str commaEListE RPAREN { TUPLE7(Q_DOLLAR_FATAL,LPAREN,$3,COMMA,$5,$6,RPAREN) }

exprOrDataType: expr { ($1) }
	|	data_type { ($1) }

list_of_argumentsE: argsDottedList { ($1) }
	|	argsExprListE { ($1) }
	|	argsExprListE COMMA argsDottedList { TUPLE3($1,COMMA,$3) }

task_declaration: TASK lifetimeE taskId tfGuts ENDTASK endLabelE { TUPLE6(TASK,$2,$3,$4,ENDTASK,$6) }

task_prototype: TASK taskId LPAREN tf_port_listE RPAREN { TUPLE5(TASK,$2,LPAREN,$4,RPAREN) }
	|	TASK taskId { TUPLE2(TASK,$2) }

function_declaration: FUNCTION lifetimeE funcId funcIsolateE tfGuts ENDFUNCTION endLabelE { TUPLE7(FUNCTION,$2,$3,$4,$5,ENDFUNCTION,$7) }

function_prototype: FUNCTION funcId LPAREN tf_port_listE RPAREN { TUPLE5(FUNCTION,$2,LPAREN,$4,RPAREN) }
	|	FUNCTION funcId { TUPLE2(FUNCTION,$2) }

funcIsolateE: /* empty */ { EMPTY_TOKEN }
	|	Q_SLASH__STAR_VERILATOR_BLANK_ISOLATE_UNDERSCORE_ASSIGNMENTS_STAR__SLASH_ { (Q_SLASH__STAR_VERILATOR_BLANK_ISOLATE_UNDERSCORE_ASSIGNMENTS_STAR__SLASH_) }

method_prototype: task_prototype { ($1) }
	|	function_prototype { ($1) }

lifetimeE: /* empty */ { EMPTY_TOKEN }
	|	lifetime { ($1) }

lifetime: STATIC { (STATIC) }
	|	AUTOMATIC { (AUTOMATIC) }

taskId: tfIdScoped { ($1) }

funcId: tfIdScoped { ($1) }
	|	signingE rangeList tfIdScoped { TUPLE3($1,$2,$3) }
	|	signing tfIdScoped { TUPLE2($1,$2) }
	|	data_type tfIdScoped { TUPLE2($1,$2) }
	|	VOID tfIdScoped { TUPLE2(VOID,$2) }

tfIdScoped: id { ($1) }

tfGuts: LPAREN tf_port_listE RPAREN SEMICOLON tfBodyE { TUPLE5(LPAREN,$2,RPAREN,SEMICOLON,$5) }
	|	SEMICOLON tfBodyE { TUPLE2(SEMICOLON,$2) }

tfBodyE: /* empty */ { EMPTY_TOKEN }
	|	tf_item_declarationList { ($1) }
	|	tf_item_declarationList stmtList { TUPLE2($1,$2) }
	|	stmtList { ($1) }

tf_item_declarationList: tf_item_declaration { ($1) }
	|	tf_item_declarationList tf_item_declaration { TUPLE2($1,$2) }

tf_item_declaration: block_item_declaration { ($1) }
	|	tf_port_declaration { ($1) }
	|	tf_item_declarationVerilator { ($1) }

tf_item_declarationVerilator: Q_SLASH__STAR_VERILATOR_BLANK_PUBLIC_STAR__SLASH_ { (Q_SLASH__STAR_VERILATOR_BLANK_PUBLIC_STAR__SLASH_) }
	|	Q_SLASH__STAR_VERILATOR_BLANK_NO_UNDERSCORE_INLINE_UNDERSCORE_TASK_STAR__SLASH_ { (Q_SLASH__STAR_VERILATOR_BLANK_NO_UNDERSCORE_INLINE_UNDERSCORE_TASK_STAR__SLASH_) }

tf_port_listE: /* 22 */ tf_port_listList { ($1) }

tf_port_listList: tf_port_item { ($1) }
	|	tf_port_listList COMMA tf_port_item { TUPLE3($1,COMMA,$3) }

tf_port_item: /* empty */ { EMPTY_TOKEN }
	|	tf_port_itemFront tf_port_itemAssignment { TUPLE2($1,$2) }
	|	tf_port_itemAssignment { ($1) }

tf_port_itemFront: data_type { ($1) }
	|	signingE rangeList { TUPLE2($1,$2) }
	|	signing { ($1) }
	|	VAR data_type { TUPLE2(VAR,$2) }
	|	VAR implicit_typeE { TUPLE2(VAR,$2) }
	|	tf_port_itemDir { ($1) }
	|	tf_port_itemDir data_type { TUPLE2($1,$2) }
	|	tf_port_itemDir signingE rangeList { TUPLE3($1,$2,$3) }
	|	tf_port_itemDir signing { TUPLE2($1,$2) }
	|	tf_port_itemDir VAR data_type { TUPLE3($1,VAR,$3) }
	|	tf_port_itemDir VAR implicit_typeE { TUPLE3($1,VAR,$3) }

tf_port_itemDir: port_direction { ($1) }

tf_port_itemAssignment: id variable_dimensionListE sigAttrListE { TUPLE3($1,$2,$3) }
	|	id variable_dimensionListE sigAttrListE EQUALS expr { TUPLE5($1,$2,$3,EQUALS,$5) }

parenE: /* empty */ { EMPTY_TOKEN }
	|	LPAREN RPAREN { TUPLE2(LPAREN,RPAREN) }

array_methodNoRoot: OR { (OR) }
	|	AND { (AND) }
	|	XOR { (XOR) }

dpi_import_export: IMPORT STRING0 dpi_tf_import_propertyE dpi_importLabelE function_prototype SEMICOLON { TUPLE6(IMPORT,STRING0,$3,$4,$5,SEMICOLON) }
	|	IMPORT STRING1 dpi_tf_import_propertyE dpi_importLabelE task_prototype SEMICOLON { TUPLE6(IMPORT,STRING1,$3,$4,$5,SEMICOLON) }
	|	EXPORT STRING2 dpi_importLabelE FUNCTION idAny SEMICOLON { TUPLE6(EXPORT,STRING2,$3,FUNCTION,$5,SEMICOLON) }
	|	EXPORT STRING3 dpi_importLabelE TASK idAny SEMICOLON { TUPLE6(EXPORT,STRING3,$3,TASK,$5,SEMICOLON) }

dpi_importLabelE: /* empty */ { EMPTY_TOKEN }
	|	idAny EQUALS { TUPLE2($1,EQUALS) }

dpi_tf_import_propertyE: /* empty */ { EMPTY_TOKEN }
	|	CONTEXT { (CONTEXT) }
	|	PURE { (PURE) }

constExpr: expr { ($1) }

expr: PLUS expr { TUPLE2(PLUS,$2) }
	|	HYPHEN expr { TUPLE2(HYPHEN,$2) }
	|	PLING expr { TUPLE2(PLING,$2) }
	|	AMPERSAND expr { TUPLE2(AMPERSAND,$2) }
	|	TILDE expr { TUPLE2(TILDE,$2) }
	|	VBAR expr { TUPLE2(VBAR,$2) }
	|	CARET expr { TUPLE2(CARET,$2) }
	|	Q_TILDE__AMPERSAND_ expr { TUPLE2(Q_TILDE__AMPERSAND_,$2) }
	|	Q_TILDE__VBAR_ expr { TUPLE2(Q_TILDE__VBAR_,$2) }
	|	Q_CARET__TILDE_ expr { TUPLE2(Q_CARET__TILDE_,$2) }
	|	expr PLUS expr { TUPLE3($1,PLUS,$3) }
	|	expr HYPHEN expr { TUPLE3($1,HYPHEN,$3) }
	|	expr STAR expr { TUPLE3($1,STAR,$3) }
	|	expr SLASH expr { TUPLE3($1,SLASH,$3) }
	|	expr PERCENT expr { TUPLE3($1,PERCENT,$3) }
	|	expr Q_EQUALS__EQUALS_ expr { TUPLE3($1,Q_EQUALS__EQUALS_,$3) }
	|	expr Q_PLING__EQUALS_ expr { TUPLE3($1,Q_PLING__EQUALS_,$3) }
	|	expr Q_EQUALS__EQUALS__EQUALS_ expr { TUPLE3($1,Q_EQUALS__EQUALS__EQUALS_,$3) }
	|	expr Q_PLING__EQUALS__EQUALS_ expr { TUPLE3($1,Q_PLING__EQUALS__EQUALS_,$3) }
	|	expr Q_EQUALS__EQUALS__QUERY_ expr { TUPLE3($1,Q_EQUALS__EQUALS__QUERY_,$3) }
	|	expr Q_PLING__EQUALS__QUERY_ expr { TUPLE3($1,Q_PLING__EQUALS__QUERY_,$3) }
	|	expr Q_AMPERSAND__AMPERSAND_ expr { TUPLE3($1,Q_AMPERSAND__AMPERSAND_,$3) }
	|	expr Q_VBAR__VBAR_ expr { TUPLE3($1,Q_VBAR__VBAR_,$3) }
	|	expr Q_STAR__STAR_ expr { TUPLE3($1,Q_STAR__STAR_,$3) }
	|	expr LESS expr { TUPLE3($1,LESS,$3) }
	|	expr GREATER expr { TUPLE3($1,GREATER,$3) }
	|	expr Q_GREATER__EQUALS_ expr { TUPLE3($1,Q_GREATER__EQUALS_,$3) }
	|	expr AMPERSAND expr { TUPLE3($1,AMPERSAND,$3) }
	|	expr VBAR expr { TUPLE3($1,VBAR,$3) }
	|	expr CARET expr { TUPLE3($1,CARET,$3) }
	|	expr Q_CARET__TILDE_ expr { TUPLE3($1,Q_CARET__TILDE_,$3) }
	|	expr Q_TILDE__VBAR_ expr { TUPLE3($1,Q_TILDE__VBAR_,$3) }
	|	expr Q_TILDE__AMPERSAND_ expr { TUPLE3($1,Q_TILDE__AMPERSAND_,$3) }
	|	expr Q_LESS__LESS_ expr { TUPLE3($1,Q_LESS__LESS_,$3) }
	|	expr Q_GREATER__GREATER_ expr { TUPLE3($1,Q_GREATER__GREATER_,$3) }
	|	expr Q_GREATER__GREATER__GREATER_ expr { TUPLE3($1,Q_GREATER__GREATER__GREATER_,$3) }
	|	expr Q_LESS__EQUALS_ expr { TUPLE3($1,Q_LESS__EQUALS_,$3) }
	|	expr QUERY expr COLON expr { TUPLE5($1,QUERY,$3,COLON,$5) }
	|	expr INSIDE LBRACE open_range_list RBRACE { TUPLE5($1,INSIDE,LBRACE,$4,RBRACE) }
	|	expr Q_HYPHEN__GREATER_ expr { TUPLE3($1,Q_HYPHEN__GREATER_,$3) }
	|	expr YP_LOGIFF expr { TUPLE3($1,YP_LOGIFF,$3) }
	|	INTEGER_BLANK_NUMBER { (INTEGER_BLANK_NUMBER) }
	|	FLOATING_HYPHEN_POINT_BLANK_NUMBER { (FLOATING_HYPHEN_POINT_BLANK_NUMBER) }
	|	strAsInt { ($1) }
	|	LBRACE constExpr LBRACE cateList RBRACE RBRACE { TUPLE6(LBRACE,$2,LBRACE,$4,RBRACE,RBRACE) }
	|	function_subroutine_callNoMethod { ($1) }
	|	expr DOT function_subroutine_callNoMethod { TUPLE3($1,DOT,$3) }
	|	expr DOT array_methodNoRoot { TUPLE3($1,DOT,$3) }
	|	LPAREN expr RPAREN { TUPLE3(LPAREN,$2,RPAREN) }
	|	UNDERSCORE LPAREN expr RPAREN { TUPLE4(UNDERSCORE,LPAREN,$3,RPAREN) }
	|	casting_type Q_QUOTE_ LPAREN expr RPAREN { TUPLE5($1,Q_QUOTE_,LPAREN,$4,RPAREN) }
	|	SIGNED Q_QUOTE_ LPAREN expr RPAREN { TUPLE5(SIGNED,Q_QUOTE_,LPAREN,$4,RPAREN) }
	|	UNSIGNED Q_QUOTE_ LPAREN expr RPAREN { TUPLE5(UNSIGNED,Q_QUOTE_,LPAREN,$4,RPAREN) }
	|	expr Q_QUOTE_ LPAREN expr RPAREN { TUPLE5($1,Q_QUOTE_,LPAREN,$4,RPAREN) }
	|	exprOkLvalue { ($1) }

fexpr: PLUS fexpr { TUPLE2(PLUS,$2) }
	|	HYPHEN fexpr { TUPLE2(HYPHEN,$2) }
	|	PLING fexpr { TUPLE2(PLING,$2) }
	|	AMPERSAND fexpr { TUPLE2(AMPERSAND,$2) }
	|	TILDE fexpr { TUPLE2(TILDE,$2) }
	|	VBAR fexpr { TUPLE2(VBAR,$2) }
	|	CARET fexpr { TUPLE2(CARET,$2) }
	|	Q_TILDE__AMPERSAND_ fexpr { TUPLE2(Q_TILDE__AMPERSAND_,$2) }
	|	Q_TILDE__VBAR_ fexpr { TUPLE2(Q_TILDE__VBAR_,$2) }
	|	Q_CARET__TILDE_ fexpr { TUPLE2(Q_CARET__TILDE_,$2) }
	|	fexpr PLUS fexpr { TUPLE3($1,PLUS,$3) }
	|	fexpr HYPHEN fexpr { TUPLE3($1,HYPHEN,$3) }
	|	fexpr STAR fexpr { TUPLE3($1,STAR,$3) }
	|	fexpr SLASH fexpr { TUPLE3($1,SLASH,$3) }
	|	fexpr PERCENT fexpr { TUPLE3($1,PERCENT,$3) }
	|	fexpr Q_EQUALS__EQUALS_ fexpr { TUPLE3($1,Q_EQUALS__EQUALS_,$3) }
	|	fexpr Q_PLING__EQUALS_ fexpr { TUPLE3($1,Q_PLING__EQUALS_,$3) }
	|	fexpr Q_EQUALS__EQUALS__EQUALS_ fexpr { TUPLE3($1,Q_EQUALS__EQUALS__EQUALS_,$3) }
	|	fexpr Q_PLING__EQUALS__EQUALS_ fexpr { TUPLE3($1,Q_PLING__EQUALS__EQUALS_,$3) }
	|	fexpr Q_EQUALS__EQUALS__QUERY_ fexpr { TUPLE3($1,Q_EQUALS__EQUALS__QUERY_,$3) }
	|	fexpr Q_PLING__EQUALS__QUERY_ fexpr { TUPLE3($1,Q_PLING__EQUALS__QUERY_,$3) }
	|	fexpr Q_AMPERSAND__AMPERSAND_ fexpr { TUPLE3($1,Q_AMPERSAND__AMPERSAND_,$3) }
	|	fexpr Q_VBAR__VBAR_ fexpr { TUPLE3($1,Q_VBAR__VBAR_,$3) }
	|	fexpr Q_STAR__STAR_ fexpr { TUPLE3($1,Q_STAR__STAR_,$3) }
	|	fexpr LESS fexpr { TUPLE3($1,LESS,$3) }
	|	fexpr GREATER fexpr { TUPLE3($1,GREATER,$3) }
	|	fexpr Q_GREATER__EQUALS_ fexpr { TUPLE3($1,Q_GREATER__EQUALS_,$3) }
	|	fexpr AMPERSAND fexpr { TUPLE3($1,AMPERSAND,$3) }
	|	fexpr VBAR fexpr { TUPLE3($1,VBAR,$3) }
	|	fexpr CARET fexpr { TUPLE3($1,CARET,$3) }
	|	fexpr Q_CARET__TILDE_ fexpr { TUPLE3($1,Q_CARET__TILDE_,$3) }
	|	fexpr Q_TILDE__VBAR_ fexpr { TUPLE3($1,Q_TILDE__VBAR_,$3) }
	|	fexpr Q_TILDE__AMPERSAND_ fexpr { TUPLE3($1,Q_TILDE__AMPERSAND_,$3) }
	|	fexpr Q_LESS__LESS_ fexpr { TUPLE3($1,Q_LESS__LESS_,$3) }
	|	fexpr Q_GREATER__GREATER_ fexpr { TUPLE3($1,Q_GREATER__GREATER_,$3) }
	|	fexpr Q_GREATER__GREATER__GREATER_ fexpr { TUPLE3($1,Q_GREATER__GREATER__GREATER_,$3) }
	|	fexpr Q_LESS__EQUALS__HYPHEN_IGNORED fexpr { TUPLE3($1,Q_LESS__EQUALS__HYPHEN_IGNORED,$3) }
	|	fexpr QUERY fexpr COLON fexpr { TUPLE5($1,QUERY,$3,COLON,$5) }
	|	fexpr INSIDE LBRACE open_range_list RBRACE { TUPLE5($1,INSIDE,LBRACE,$4,RBRACE) }
	|	fexpr Q_HYPHEN__GREATER_ fexpr { TUPLE3($1,Q_HYPHEN__GREATER_,$3) }
	|	fexpr YP_LOGIFF fexpr { TUPLE3($1,YP_LOGIFF,$3) }
	|	INTEGER_BLANK_NUMBER { (INTEGER_BLANK_NUMBER) }
	|	FLOATING_HYPHEN_POINT_BLANK_NUMBER { (FLOATING_HYPHEN_POINT_BLANK_NUMBER) }
	|	strAsInt { ($1) }
	|	LBRACE constExpr LBRACE cateList RBRACE RBRACE { TUPLE6(LBRACE,$2,LBRACE,$4,RBRACE,RBRACE) }
	|	function_subroutine_callNoMethod { ($1) }
	|	fexpr DOT function_subroutine_callNoMethod { TUPLE3($1,DOT,$3) }
	|	fexpr DOT array_methodNoRoot { TUPLE3($1,DOT,$3) }
	|	LPAREN expr RPAREN { TUPLE3(LPAREN,$2,RPAREN) }
	|	UNDERSCORE LPAREN expr RPAREN { TUPLE4(UNDERSCORE,LPAREN,$3,RPAREN) }
	|	casting_type Q_QUOTE_ LPAREN expr RPAREN { TUPLE5($1,Q_QUOTE_,LPAREN,$4,RPAREN) }
	|	SIGNED Q_QUOTE_ LPAREN expr RPAREN { TUPLE5(SIGNED,Q_QUOTE_,LPAREN,$4,RPAREN) }
	|	UNSIGNED Q_QUOTE_ LPAREN expr RPAREN { TUPLE5(UNSIGNED,Q_QUOTE_,LPAREN,$4,RPAREN) }
	|	fexpr Q_QUOTE_ LPAREN expr RPAREN { TUPLE5($1,Q_QUOTE_,LPAREN,$4,RPAREN) }
	|	fexprOkLvalue { ($1) }

exprNoStr: PLUS expr { TUPLE2(PLUS,$2) }
	|	HYPHEN expr { TUPLE2(HYPHEN,$2) }
	|	PLING expr { TUPLE2(PLING,$2) }
	|	AMPERSAND expr { TUPLE2(AMPERSAND,$2) }
	|	TILDE expr { TUPLE2(TILDE,$2) }
	|	VBAR expr { TUPLE2(VBAR,$2) }
	|	CARET expr { TUPLE2(CARET,$2) }
	|	Q_TILDE__AMPERSAND_ expr { TUPLE2(Q_TILDE__AMPERSAND_,$2) }
	|	Q_TILDE__VBAR_ expr { TUPLE2(Q_TILDE__VBAR_,$2) }
	|	Q_CARET__TILDE_ expr { TUPLE2(Q_CARET__TILDE_,$2) }
	|	expr PLUS expr { TUPLE3($1,PLUS,$3) }
	|	expr HYPHEN expr { TUPLE3($1,HYPHEN,$3) }
	|	expr STAR expr { TUPLE3($1,STAR,$3) }
	|	expr SLASH expr { TUPLE3($1,SLASH,$3) }
	|	expr PERCENT expr { TUPLE3($1,PERCENT,$3) }
	|	expr Q_EQUALS__EQUALS_ expr { TUPLE3($1,Q_EQUALS__EQUALS_,$3) }
	|	expr Q_PLING__EQUALS_ expr { TUPLE3($1,Q_PLING__EQUALS_,$3) }
	|	expr Q_EQUALS__EQUALS__EQUALS_ expr { TUPLE3($1,Q_EQUALS__EQUALS__EQUALS_,$3) }
	|	expr Q_PLING__EQUALS__EQUALS_ expr { TUPLE3($1,Q_PLING__EQUALS__EQUALS_,$3) }
	|	expr Q_EQUALS__EQUALS__QUERY_ expr { TUPLE3($1,Q_EQUALS__EQUALS__QUERY_,$3) }
	|	expr Q_PLING__EQUALS__QUERY_ expr { TUPLE3($1,Q_PLING__EQUALS__QUERY_,$3) }
	|	expr Q_AMPERSAND__AMPERSAND_ expr { TUPLE3($1,Q_AMPERSAND__AMPERSAND_,$3) }
	|	expr Q_VBAR__VBAR_ expr { TUPLE3($1,Q_VBAR__VBAR_,$3) }
	|	expr Q_STAR__STAR_ expr { TUPLE3($1,Q_STAR__STAR_,$3) }
	|	expr LESS expr { TUPLE3($1,LESS,$3) }
	|	expr GREATER expr { TUPLE3($1,GREATER,$3) }
	|	expr Q_GREATER__EQUALS_ expr { TUPLE3($1,Q_GREATER__EQUALS_,$3) }
	|	expr AMPERSAND expr { TUPLE3($1,AMPERSAND,$3) }
	|	expr VBAR expr { TUPLE3($1,VBAR,$3) }
	|	expr CARET expr { TUPLE3($1,CARET,$3) }
	|	expr Q_CARET__TILDE_ expr { TUPLE3($1,Q_CARET__TILDE_,$3) }
	|	expr Q_TILDE__VBAR_ expr { TUPLE3($1,Q_TILDE__VBAR_,$3) }
	|	expr Q_TILDE__AMPERSAND_ expr { TUPLE3($1,Q_TILDE__AMPERSAND_,$3) }
	|	expr Q_LESS__LESS_ expr { TUPLE3($1,Q_LESS__LESS_,$3) }
	|	expr Q_GREATER__GREATER_ expr { TUPLE3($1,Q_GREATER__GREATER_,$3) }
	|	expr Q_GREATER__GREATER__GREATER_ expr { TUPLE3($1,Q_GREATER__GREATER__GREATER_,$3) }
	|	expr Q_LESS__EQUALS_ expr { TUPLE3($1,Q_LESS__EQUALS_,$3) }
	|	expr QUERY expr COLON expr { TUPLE5($1,QUERY,$3,COLON,$5) }
	|	expr INSIDE LBRACE open_range_list RBRACE { TUPLE5($1,INSIDE,LBRACE,$4,RBRACE) }
	|	expr Q_HYPHEN__GREATER_ expr { TUPLE3($1,Q_HYPHEN__GREATER_,$3) }
	|	expr YP_LOGIFF expr { TUPLE3($1,YP_LOGIFF,$3) }
	|	INTEGER_BLANK_NUMBER { (INTEGER_BLANK_NUMBER) }
	|	FLOATING_HYPHEN_POINT_BLANK_NUMBER { (FLOATING_HYPHEN_POINT_BLANK_NUMBER) }
	|	strAsIntIgnore { ($1) }
	|	LBRACE constExpr LBRACE cateList RBRACE RBRACE { TUPLE6(LBRACE,$2,LBRACE,$4,RBRACE,RBRACE) }
	|	function_subroutine_callNoMethod { ($1) }
	|	expr DOT function_subroutine_callNoMethod { TUPLE3($1,DOT,$3) }
	|	expr DOT array_methodNoRoot { TUPLE3($1,DOT,$3) }
	|	LPAREN expr RPAREN { TUPLE3(LPAREN,$2,RPAREN) }
	|	UNDERSCORE LPAREN expr RPAREN { TUPLE4(UNDERSCORE,LPAREN,$3,RPAREN) }
	|	casting_type Q_QUOTE_ LPAREN expr RPAREN { TUPLE5($1,Q_QUOTE_,LPAREN,$4,RPAREN) }
	|	SIGNED Q_QUOTE_ LPAREN expr RPAREN { TUPLE5(SIGNED,Q_QUOTE_,LPAREN,$4,RPAREN) }
	|	UNSIGNED Q_QUOTE_ LPAREN expr RPAREN { TUPLE5(UNSIGNED,Q_QUOTE_,LPAREN,$4,RPAREN) }
	|	expr Q_QUOTE_ LPAREN expr RPAREN { TUPLE5($1,Q_QUOTE_,LPAREN,$4,RPAREN) }
	|	exprOkLvalue { ($1) }

exprOkLvalue: exprScope { ($1) }
	|	LBRACE cateList RBRACE { TUPLE3(LBRACE,$2,RBRACE) }
	|	data_type assignment_pattern { TUPLE2($1,$2) }
	|	assignment_pattern { ($1) }
	|	streaming_concatenation { ($1) }

fexprOkLvalue: fexprScope { ($1) }
	|	LBRACE cateList RBRACE { TUPLE3(LBRACE,$2,RBRACE) }
	|	data_type assignment_pattern { TUPLE2($1,$2) }
	|	assignment_pattern { ($1) }
	|	streaming_concatenation { ($1) }

fexprLvalue: fexprOkLvalue { ($1) }

exprScope: idArrayed { ($1) }
	|	package_scopeIdFollows idArrayed { TUPLE2($1,$2) }
	|	expr DOT idArrayed { TUPLE3($1,DOT,$3) }

fexprScope: idArrayed { ($1) }
	|	package_scopeIdFollows idArrayed { TUPLE2($1,$2) }
	|	fexpr DOT idArrayed { TUPLE3($1,DOT,$3) }

exprStrText: exprNoStr { ($1) }
	|	strAsText { ($1) }

cStrList: exprStrText { ($1) }
	|	exprStrText COMMA cStrList { TUPLE3($1,COMMA,$3) }

cateList: stream_expression { ($1) }
	|	cateList COMMA stream_expression { TUPLE3($1,COMMA,$3) }

exprListE: /* empty */ { EMPTY_TOKEN }
	|	exprList { ($1) }

exprList: expr { ($1) }
	|	exprList COMMA expr { TUPLE3($1,COMMA,$3) }

commaEListE: /* empty */ { EMPTY_TOKEN }
	|	COMMA exprList { TUPLE2(COMMA,$2) }

vrdList: idClassSel { ($1) }
	|	vrdList COMMA idClassSel { TUPLE3($1,COMMA,$3) }

commaVRDListE: /* empty */ { EMPTY_TOKEN }
	|	COMMA vrdList { TUPLE2(COMMA,$2) }

argsExprList: expr { ($1) }
	|	argsExprList COMMA expr { TUPLE3($1,COMMA,$3) }

argsExprListE: argsExprOneE { ($1) }
	|	argsExprListE COMMA argsExprOneE { TUPLE3($1,COMMA,$3) }

argsExprOneE: /* empty */ { EMPTY_TOKEN }
	|	expr { ($1) }

argsDottedList: argsDotted { ($1) }
	|	argsDottedList COMMA argsDotted { TUPLE3($1,COMMA,$3) }

argsDotted: DOT idAny LPAREN RPAREN { TUPLE4(DOT,$2,LPAREN,RPAREN) }
	|	DOT idAny LPAREN expr RPAREN { TUPLE5(DOT,$2,LPAREN,$4,RPAREN) }

streaming_concatenation: LBRACE Q_LESS__LESS_ stream_concOrExprOrType RBRACE { TUPLE4(LBRACE,Q_LESS__LESS_,$3,RBRACE) }
	|	LBRACE Q_GREATER__GREATER_ stream_concOrExprOrType RBRACE { TUPLE4(LBRACE,Q_GREATER__GREATER_,$3,RBRACE) }
	|	LBRACE Q_LESS__LESS_ stream_concOrExprOrType stream_concatenation RBRACE { TUPLE5(LBRACE,Q_LESS__LESS_,$3,$4,RBRACE) }
	|	LBRACE Q_GREATER__GREATER_ stream_concOrExprOrType stream_concatenation RBRACE { TUPLE5(LBRACE,Q_GREATER__GREATER_,$3,$4,RBRACE) }

stream_concOrExprOrType: cateList { ($1) }
	|	simple_type { ($1) }

stream_concatenation: LBRACE cateList RBRACE { TUPLE3(LBRACE,$2,RBRACE) }

stream_expression: expr { ($1) }

gateDecl: BUF delayE gateBufList SEMICOLON { TUPLE4(BUF,$2,$3,SEMICOLON) }
	|	BUFIF0 delayE gateBufif0List SEMICOLON { TUPLE4(BUFIF0,$2,$3,SEMICOLON) }
	|	BUFIF1 delayE gateBufif1List SEMICOLON { TUPLE4(BUFIF1,$2,$3,SEMICOLON) }
	|	NOT delayE gateNotList SEMICOLON { TUPLE4(NOT,$2,$3,SEMICOLON) }
	|	NOTIF0 delayE gateNotif0List SEMICOLON { TUPLE4(NOTIF0,$2,$3,SEMICOLON) }
	|	NOTIF1 delayE gateNotif1List SEMICOLON { TUPLE4(NOTIF1,$2,$3,SEMICOLON) }
	|	AND delayE gateAndList SEMICOLON { TUPLE4(AND,$2,$3,SEMICOLON) }
	|	NAND delayE gateNandList SEMICOLON { TUPLE4(NAND,$2,$3,SEMICOLON) }
	|	OR delayE gateOrList SEMICOLON { TUPLE4(OR,$2,$3,SEMICOLON) }
	|	NOR delayE gateNorList SEMICOLON { TUPLE4(NOR,$2,$3,SEMICOLON) }
	|	XOR delayE gateXorList SEMICOLON { TUPLE4(XOR,$2,$3,SEMICOLON) }
	|	XNOR delayE gateXnorList SEMICOLON { TUPLE4(XNOR,$2,$3,SEMICOLON) }
	|	PULLUP delayE gatePullupList SEMICOLON { TUPLE4(PULLUP,$2,$3,SEMICOLON) }
	|	PULLDOWN delayE gatePulldownList SEMICOLON { TUPLE4(PULLDOWN,$2,$3,SEMICOLON) }
	|	NMOS delayE gateBufif1List SEMICOLON { TUPLE4(NMOS,$2,$3,SEMICOLON) }
	|	PMOS delayE gateBufif0List SEMICOLON { TUPLE4(PMOS,$2,$3,SEMICOLON) }
	|	TRAN delayE gateUnsupList SEMICOLON { TUPLE4(TRAN,$2,$3,SEMICOLON) }
	|	RCMOS delayE gateUnsupList SEMICOLON { TUPLE4(RCMOS,$2,$3,SEMICOLON) }
	|	CMOS delayE gateUnsupList SEMICOLON { TUPLE4(CMOS,$2,$3,SEMICOLON) }
	|	RNMOS delayE gateUnsupList SEMICOLON { TUPLE4(RNMOS,$2,$3,SEMICOLON) }
	|	RPMOS delayE gateUnsupList SEMICOLON { TUPLE4(RPMOS,$2,$3,SEMICOLON) }
	|	RTRAN delayE gateUnsupList SEMICOLON { TUPLE4(RTRAN,$2,$3,SEMICOLON) }
	|	RTRANIF0 delayE gateUnsupList SEMICOLON { TUPLE4(RTRANIF0,$2,$3,SEMICOLON) }
	|	RTRANIF1 delayE gateUnsupList SEMICOLON { TUPLE4(RTRANIF1,$2,$3,SEMICOLON) }
	|	TRANIF0 delayE gateUnsupList SEMICOLON { TUPLE4(TRANIF0,$2,$3,SEMICOLON) }
	|	TRANIF1 delayE gateUnsupList SEMICOLON { TUPLE4(TRANIF1,$2,$3,SEMICOLON) }

gateBufList: gateBuf { ($1) }
	|	gateBufList COMMA gateBuf { TUPLE3($1,COMMA,$3) }

gateBufif0List: gateBufif0 { ($1) }
	|	gateBufif0List COMMA gateBufif0 { TUPLE3($1,COMMA,$3) }

gateBufif1List: gateBufif1 { ($1) }
	|	gateBufif1List COMMA gateBufif1 { TUPLE3($1,COMMA,$3) }

gateNotList: gateNot { ($1) }
	|	gateNotList COMMA gateNot { TUPLE3($1,COMMA,$3) }

gateNotif0List: gateNotif0 { ($1) }
	|	gateNotif0List COMMA gateNotif0 { TUPLE3($1,COMMA,$3) }

gateNotif1List: gateNotif1 { ($1) }
	|	gateNotif1List COMMA gateNotif1 { TUPLE3($1,COMMA,$3) }

gateAndList: gateAnd { ($1) }
	|	gateAndList COMMA gateAnd { TUPLE3($1,COMMA,$3) }

gateNandList: gateNand { ($1) }
	|	gateNandList COMMA gateNand { TUPLE3($1,COMMA,$3) }

gateOrList: gateOr { ($1) }
	|	gateOrList COMMA gateOr { TUPLE3($1,COMMA,$3) }

gateNorList: gateNor { ($1) }
	|	gateNorList COMMA gateNor { TUPLE3($1,COMMA,$3) }

gateXorList: gateXor { ($1) }
	|	gateXorList COMMA gateXor { TUPLE3($1,COMMA,$3) }

gateXnorList: gateXnor { ($1) }
	|	gateXnorList COMMA gateXnor { TUPLE3($1,COMMA,$3) }

gatePullupList: gatePullup { ($1) }
	|	gatePullupList COMMA gatePullup { TUPLE3($1,COMMA,$3) }

gatePulldownList: gatePulldown { ($1) }
	|	gatePulldownList COMMA gatePulldown { TUPLE3($1,COMMA,$3) }

gateUnsupList: gateUnsup { ($1) }
	|	gateUnsupList COMMA gateUnsup { TUPLE3($1,COMMA,$3) }

gateRangeE: instRangeE { ($1) }

gateBuf: gateIdE gateRangeE LPAREN variable_lvalue COMMA gatePinExpr RPAREN { TUPLE7($1,$2,LPAREN,$4,COMMA,$6,RPAREN) }

gateBufif0: gateIdE gateRangeE LPAREN variable_lvalue COMMA gatePinExpr COMMA gatePinExpr RPAREN { TUPLE9($1,$2,LPAREN,$4,COMMA,$6,COMMA,$8,RPAREN) }

gateBufif1: gateIdE gateRangeE LPAREN variable_lvalue COMMA gatePinExpr COMMA gatePinExpr RPAREN { TUPLE9($1,$2,LPAREN,$4,COMMA,$6,COMMA,$8,RPAREN) }

gateNot: gateIdE gateRangeE LPAREN variable_lvalue COMMA gatePinExpr RPAREN { TUPLE7($1,$2,LPAREN,$4,COMMA,$6,RPAREN) }

gateNotif0: gateIdE gateRangeE LPAREN variable_lvalue COMMA gatePinExpr COMMA gatePinExpr RPAREN { TUPLE9($1,$2,LPAREN,$4,COMMA,$6,COMMA,$8,RPAREN) }

gateNotif1: gateIdE gateRangeE LPAREN variable_lvalue COMMA gatePinExpr COMMA gatePinExpr RPAREN { TUPLE9($1,$2,LPAREN,$4,COMMA,$6,COMMA,$8,RPAREN) }

gateAnd: gateIdE gateRangeE LPAREN variable_lvalue COMMA gateAndPinList RPAREN { TUPLE7($1,$2,LPAREN,$4,COMMA,$6,RPAREN) }

gateNand: gateIdE gateRangeE LPAREN variable_lvalue COMMA gateAndPinList RPAREN { TUPLE7($1,$2,LPAREN,$4,COMMA,$6,RPAREN) }

gateOr: gateIdE gateRangeE LPAREN variable_lvalue COMMA gateOrPinList RPAREN { TUPLE7($1,$2,LPAREN,$4,COMMA,$6,RPAREN) }

gateNor: gateIdE gateRangeE LPAREN variable_lvalue COMMA gateOrPinList RPAREN { TUPLE7($1,$2,LPAREN,$4,COMMA,$6,RPAREN) }

gateXor: gateIdE gateRangeE LPAREN variable_lvalue COMMA gateXorPinList RPAREN { TUPLE7($1,$2,LPAREN,$4,COMMA,$6,RPAREN) }

gateXnor: gateIdE gateRangeE LPAREN variable_lvalue COMMA gateXorPinList RPAREN { TUPLE7($1,$2,LPAREN,$4,COMMA,$6,RPAREN) }

gatePullup: gateIdE gateRangeE LPAREN variable_lvalue RPAREN { TUPLE5($1,$2,LPAREN,$4,RPAREN) }

gatePulldown: gateIdE gateRangeE LPAREN variable_lvalue RPAREN { TUPLE5($1,$2,LPAREN,$4,RPAREN) }

gateUnsup: gateIdE gateRangeE LPAREN gateUnsupPinList RPAREN { TUPLE5($1,$2,LPAREN,$4,RPAREN) }

gateIdE: /* empty */ { EMPTY_TOKEN }
	|	id { ($1) }

gateAndPinList: gatePinExpr { ($1) }
	|	gateAndPinList COMMA gatePinExpr { TUPLE3($1,COMMA,$3) }

gateOrPinList: gatePinExpr { ($1) }
	|	gateOrPinList COMMA gatePinExpr { TUPLE3($1,COMMA,$3) }

gateXorPinList: gatePinExpr { ($1) }
	|	gateXorPinList COMMA gatePinExpr { TUPLE3($1,COMMA,$3) }

gateUnsupPinList: gatePinExpr { ($1) }
	|	gateUnsupPinList COMMA gatePinExpr { TUPLE3($1,COMMA,$3) }

gatePinExpr: expr { ($1) }

strengthSpecE: /* empty */ { EMPTY_TOKEN }

combinational_body: TABLE tableEntryList ENDTABLE { TUPLE3(TABLE,$2,ENDTABLE) }

tableEntryList: tableEntry { ($1) }
	|	tableEntryList tableEntry { TUPLE2($1,$2) }

tableEntry: TABLE_BLANK_LINE { (TABLE_BLANK_LINE) }
	|	ERROR_TOKEN { (ERROR_TOKEN) }

specify_block: SPECIFY specifyJunkList ENDSPECIFY { TUPLE3(SPECIFY,$2,ENDSPECIFY) }
	|	SPECIFY ENDSPECIFY { TUPLE2(SPECIFY,ENDSPECIFY) }

specifyJunkList: specifyJunk { ($1) }
	|	specifyJunkList specifyJunk { TUPLE2($1,$2) }

specifyJunk: PLING { (PLING) }
	|	HASH { (HASH) }
	|	PERCENT { (PERCENT) }
	|	AMPERSAND { (AMPERSAND) }
	|	LPAREN { (LPAREN) }
	|	RPAREN { (RPAREN) }
	|	STAR { (STAR) }
	|	PLUS { (PLUS) }
	|	COMMA { (COMMA) }
	|	HYPHEN { (HYPHEN) }
	|	DOT { (DOT) }
	|	SLASH { (SLASH) }
	|	COLON { (COLON) }
	|	SEMICOLON { (SEMICOLON) }
	|	LESS { (LESS) }
	|	EQUALS { (EQUALS) }
	|	GREATER { (GREATER) }
	|	QUERY { (QUERY) }
	|	AT { (AT) }
	|	LBRACK { (LBRACK) }
	|	RBRACK { (RBRACK) }
	|	CARET { (CARET) }
	|	LBRACE { (LBRACE) }
	|	VBAR { (VBAR) }
	|	RBRACE { (RBRACE) }
	|	TILDE { (TILDE) }
	|	ALWAYS { (ALWAYS) }
	|	ALWAYS_UNDERSCORE_COMB { (ALWAYS_UNDERSCORE_COMB) }
	|	ALWAYS_UNDERSCORE_FF { (ALWAYS_UNDERSCORE_FF) }
	|	ALWAYS_UNDERSCORE_LATCH { (ALWAYS_UNDERSCORE_LATCH) }
	|	AND { (AND) }
	|	ASSERT { (ASSERT) }
	|	ASSIGN { (ASSIGN) }
	|	ASSUME { (ASSUME) }
	|	AUTOMATIC { (AUTOMATIC) }
	|	BEGIN { (BEGIN) }
	|	BIND { (BIND) }
	|	BIT { (BIT) }
	|	BREAK { (BREAK) }
	|	BUF { (BUF) }
	|	BUFIF0 { (BUFIF0) }
	|	BUFIF1 { (BUFIF1) }
	|	BYTE { (BYTE) }
	|	CASE { (CASE) }
	|	CASEX { (CASEX) }
	|	CASEZ { (CASEZ) }
	|	CHANDLE { (CHANDLE) }
	|	CLOCKING { (CLOCKING) }
	|	CMOS { (CMOS) }
	|	CONST { (CONST) }
	|	CONST_HYPHEN_IN_HYPHEN_LEX { (CONST_HYPHEN_IN_HYPHEN_LEX) }
	|	CONST_HYPHEN_THEN_HYPHEN_REF { (CONST_HYPHEN_THEN_HYPHEN_REF) }
	|	CONTEXT { (CONTEXT) }
	|	CONTINUE { (CONTINUE) }
	|	COVER { (COVER) }
	|	DEASSIGN { (DEASSIGN) }
	|	DEFAULT { (DEFAULT) }
	|	DEFPARAM { (DEFPARAM) }
	|	DISABLE { (DISABLE) }
	|	DO { (DO) }
	|	Q_DOLLAR_ACOS { (Q_DOLLAR_ACOS) }
	|	Q_DOLLAR_ACOSH { (Q_DOLLAR_ACOSH) }
	|	Q_DOLLAR_ASIN { (Q_DOLLAR_ASIN) }
	|	Q_DOLLAR_ASINH { (Q_DOLLAR_ASINH) }
	|	Q_DOLLAR_ATAN { (Q_DOLLAR_ATAN) }
	|	Q_DOLLAR_ATAN2 { (Q_DOLLAR_ATAN2) }
	|	Q_DOLLAR_ATANH { (Q_DOLLAR_ATANH) }
	|	Q_DOLLAR_BITS { (Q_DOLLAR_BITS) }
	|	Q_DOLLAR_BITSTOREAL { (Q_DOLLAR_BITSTOREAL) }
	|	Q_DOLLAR_C { (Q_DOLLAR_C) }
	|	Q_DOLLAR_CEIL { (Q_DOLLAR_CEIL) }
	|	Q_DOLLAR_CLOG2 { (Q_DOLLAR_CLOG2) }
	|	Q_DOLLAR_COS { (Q_DOLLAR_COS) }
	|	Q_DOLLAR_COSH { (Q_DOLLAR_COSH) }
	|	Q_DOLLAR_COUNTONES { (Q_DOLLAR_COUNTONES) }
	|	Q_DOLLAR_DIMENSIONS { (Q_DOLLAR_DIMENSIONS) }
	|	Q_DOLLAR_DISPLAY { (Q_DOLLAR_DISPLAY) }
	|	Q_DOLLAR_ERROR { (Q_DOLLAR_ERROR) }
	|	Q_DOLLAR_EXP { (Q_DOLLAR_EXP) }
	|	Q_DOLLAR_FATAL { (Q_DOLLAR_FATAL) }
	|	Q_DOLLAR_FCLOSE { (Q_DOLLAR_FCLOSE) }
	|	Q_DOLLAR_FDISPLAY { (Q_DOLLAR_FDISPLAY) }
	|	Q_DOLLAR_FEOF { (Q_DOLLAR_FEOF) }
	|	Q_DOLLAR_FFLUSH { (Q_DOLLAR_FFLUSH) }
	|	Q_DOLLAR_FGETC { (Q_DOLLAR_FGETC) }
	|	Q_DOLLAR_FGETS { (Q_DOLLAR_FGETS) }
	|	Q_DOLLAR_FINISH { (Q_DOLLAR_FINISH) }
	|	Q_DOLLAR_FLOOR { (Q_DOLLAR_FLOOR) }
	|	Q_DOLLAR_FOPEN { (Q_DOLLAR_FOPEN) }
	|	Q_DOLLAR_FSCANF { (Q_DOLLAR_FSCANF) }
	|	Q_DOLLAR_FWRITE { (Q_DOLLAR_FWRITE) }
	|	Q_DOLLAR_HIGH { (Q_DOLLAR_HIGH) }
	|	Q_DOLLAR_HYPOT { (Q_DOLLAR_HYPOT) }
	|	Q_DOLLAR_INCREMENT { (Q_DOLLAR_INCREMENT) }
	|	Q_DOLLAR_INFO { (Q_DOLLAR_INFO) }
	|	Q_DOLLAR_ISUNKNOWN { (Q_DOLLAR_ISUNKNOWN) }
	|	Q_DOLLAR_ITOR { (Q_DOLLAR_ITOR) }
	|	Q_DOLLAR_LEFT { (Q_DOLLAR_LEFT) }
	|	Q_DOLLAR_LN { (Q_DOLLAR_LN) }
	|	Q_DOLLAR_LOG10 { (Q_DOLLAR_LOG10) }
	|	Q_DOLLAR_LOW { (Q_DOLLAR_LOW) }
	|	Q_DOLLAR_ONEHOT { (Q_DOLLAR_ONEHOT) }
	|	Q_DOLLAR_ONEHOT0 { (Q_DOLLAR_ONEHOT0) }
	|	Q_DOLLAR_PAST { (Q_DOLLAR_PAST) }
	|	Q_DOLLAR_POW { (Q_DOLLAR_POW) }
	|	Q_DOLLAR_RANDOM { (Q_DOLLAR_RANDOM) }
	|	Q_DOLLAR_READMEMB { (Q_DOLLAR_READMEMB) }
	|	Q_DOLLAR_READMEMH { (Q_DOLLAR_READMEMH) }
	|	Q_DOLLAR_REALTIME { (Q_DOLLAR_REALTIME) }
	|	Q_DOLLAR_REALTOBITS { (Q_DOLLAR_REALTOBITS) }
	|	Q_DOLLAR_RIGHT { (Q_DOLLAR_RIGHT) }
	|	Q_DOLLAR_RTOI { (Q_DOLLAR_RTOI) }
	|	Q_DOLLAR_SFORMAT { (Q_DOLLAR_SFORMAT) }
	|	Q_DOLLAR_SFORMATF { (Q_DOLLAR_SFORMATF) }
	|	Q_DOLLAR_SIGNED { (Q_DOLLAR_SIGNED) }
	|	Q_DOLLAR_SIN { (Q_DOLLAR_SIN) }
	|	Q_DOLLAR_SINH { (Q_DOLLAR_SINH) }
	|	Q_DOLLAR_SIZE { (Q_DOLLAR_SIZE) }
	|	Q_DOLLAR_SQRT { (Q_DOLLAR_SQRT) }
	|	Q_DOLLAR_SSCANF { (Q_DOLLAR_SSCANF) }
	|	Q_DOLLAR_STIME { (Q_DOLLAR_STIME) }
	|	Q_DOLLAR_STOP { (Q_DOLLAR_STOP) }
	|	Q_DOLLAR_SWRITE { (Q_DOLLAR_SWRITE) }
	|	Q_DOLLAR_SYSTEM { (Q_DOLLAR_SYSTEM) }
	|	Q_DOLLAR_TAN { (Q_DOLLAR_TAN) }
	|	Q_DOLLAR_TANH { (Q_DOLLAR_TANH) }
	|	Q_DOLLAR_TEST_DOLLAR_PLUSARGS { (Q_DOLLAR_TEST_DOLLAR_PLUSARGS) }
	|	Q_DOLLAR_TIME { (Q_DOLLAR_TIME) }
	|	Q_DOLLAR_UNIT { (Q_DOLLAR_UNIT) }
	|	Q_DOLLAR_UNPACKED_UNDERSCORE_DIMENSIONS { (Q_DOLLAR_UNPACKED_UNDERSCORE_DIMENSIONS) }
	|	Q_DOLLAR_UNSIGNED { (Q_DOLLAR_UNSIGNED) }
	|	Q_DOLLAR_VALUE_DOLLAR_PLUSARGS { (Q_DOLLAR_VALUE_DOLLAR_PLUSARGS) }
	|	Q_DOLLAR_WARNING { (Q_DOLLAR_WARNING) }
	|	Q_DOLLAR_WRITE { (Q_DOLLAR_WRITE) }
	|	Q_DOLLAR_WRITEMEMH { (Q_DOLLAR_WRITEMEMH) }
	|	EDGE { (EDGE) }
	|	ELSE { (ELSE) }
	|	END { (END) }
	|	ENDCASE { (ENDCASE) }
	|	ENDCLOCKING { (ENDCLOCKING) }
	|	ENDFUNCTION { (ENDFUNCTION) }
	|	ENDGENERATE { (ENDGENERATE) }
	|	ENDINTERFACE { (ENDINTERFACE) }
	|	ENDMODULE { (ENDMODULE) }
	|	ENDPACKAGE { (ENDPACKAGE) }
	|	ENDPRIMITIVE { (ENDPRIMITIVE) }
	|	ENDPROGRAM { (ENDPROGRAM) }
	|	ENDPROPERTY { (ENDPROPERTY) }
	|	ENDTABLE { (ENDTABLE) }
	|	ENDTASK { (ENDTASK) }
	|	ENUM { (ENUM) }
	|	EXPORT { (EXPORT) }
	|	EXTERN { (EXTERN) }
	|	FINAL { (FINAL) }
	|	FOR { (FOR) }
	|	FOREACH { (FOREACH) }
	|	FOREVER { (FOREVER) }
	|	FORKJOIN { (FORKJOIN) }
	|	FUNCTION { (FUNCTION) }
	|	GENERATE { (GENERATE) }
	|	GENVAR { (GENVAR) }
	|	GLOBAL_HYPHEN_THEN_HYPHEN_CLOCKING { (GLOBAL_HYPHEN_THEN_HYPHEN_CLOCKING) }
	|	GLOBAL_HYPHEN_IN_HYPHEN_LEX { (GLOBAL_HYPHEN_IN_HYPHEN_LEX) }
	|	IF { (IF) }
	|	IFF { (IFF) }
	|	IMPORT { (IMPORT) }
	|	INITIAL { (INITIAL) }
	|	INOUT { (INOUT) }
	|	INPUT { (INPUT) }
	|	INSIDE { (INSIDE) }
	|	INT { (INT) }
	|	INTEGER { (INTEGER) }
	|	INTERFACE { (INTERFACE) }
	|	LOCALPARAM { (LOCALPARAM) }
	|	LOGIC { (LOGIC) }
	|	LONGINT { (LONGINT) }
	|	MODPORT { (MODPORT) }
	|	MODULE { (MODULE) }
	|	NAND { (NAND) }
	|	NEGEDGE { (NEGEDGE) }
	|	NMOS { (NMOS) }
	|	NOR { (NOR) }
	|	NOT { (NOT) }
	|	NOTIF0 { (NOTIF0) }
	|	NOTIF1 { (NOTIF1) }
	|	OR { (OR) }
	|	OUTPUT { (OUTPUT) }
	|	PACKAGE { (PACKAGE) }
	|	PACKED { (PACKED) }
	|	PARAMETER { (PARAMETER) }
	|	PMOS { (PMOS) }
	|	POSEDGE { (POSEDGE) }
	|	PRIMITIVE { (PRIMITIVE) }
	|	PRIORITY { (PRIORITY) }
	|	PROGRAM { (PROGRAM) }
	|	PROPERTY { (PROPERTY) }
	|	PULLDOWN { (PULLDOWN) }
	|	PULLUP { (PULLUP) }
	|	PURE { (PURE) }
	|	Q_AMPERSAND__AMPERSAND_ { (Q_AMPERSAND__AMPERSAND_) }
	|	Q_AMPERSAND__AMPERSAND__AMPERSAND_ { (Q_AMPERSAND__AMPERSAND__AMPERSAND_) }
	|	Q_AMPERSAND__EQUALS_ { (Q_AMPERSAND__EQUALS_) }
	|	Q_STAR__GREATER_ { (Q_STAR__GREATER_) }
	|	Q_AT__AT_ { (Q_AT__AT_) }
	|	Q_LBRACK__EQUALS_ { (Q_LBRACK__EQUALS_) }
	|	Q_LBRACK__HYPHEN__GREATER_ { (Q_LBRACK__HYPHEN__GREATER_) }
	|	Q_LBRACK__STAR_ { (Q_LBRACK__STAR_) }
	|	Q_EQUALS__EQUALS__EQUALS_ { (Q_EQUALS__EQUALS__EQUALS_) }
	|	Q_PLING__EQUALS__EQUALS_ { (Q_PLING__EQUALS__EQUALS_) }
	|	Q_COLON__COLON_ { (Q_COLON__COLON_) }
	|	Q_COLON__SLASH_ { (Q_COLON__SLASH_) }
	|	Q_COLON__EQUALS_ { (Q_COLON__EQUALS_) }
	|	Q_SLASH__EQUALS_ { (Q_SLASH__EQUALS_) }
	|	Q_DOT__STAR_ { (Q_DOT__STAR_) }
	|	Q_EQUALS__GREATER_ { (Q_EQUALS__GREATER_) }
	|	Q_EQUALS__EQUALS_ { (Q_EQUALS__EQUALS_) }
	|	Q_GREATER__EQUALS_ { (Q_GREATER__EQUALS_) }
	|	YP_LOGIFF { (YP_LOGIFF) }
	|	Q_LESS__EQUALS_ { (Q_LESS__EQUALS_) }
	|	Q_LESS__EQUALS__HYPHEN_IGNORED { (Q_LESS__EQUALS__HYPHEN_IGNORED) }
	|	Q_HYPHEN__COLON_ { (Q_HYPHEN__COLON_) }
	|	Q_HYPHEN__EQUALS_ { (Q_HYPHEN__EQUALS_) }
	|	Q_HYPHEN__GREATER_ { (Q_HYPHEN__GREATER_) }
	|	Q_HYPHEN__GREATER__GREATER_ { (Q_HYPHEN__GREATER__GREATER_) }
	|	Q_HYPHEN__HYPHEN_ { (Q_HYPHEN__HYPHEN_) }
	|	Q_PERCENT__EQUALS_ { (Q_PERCENT__EQUALS_) }
	|	Q_TILDE__AMPERSAND_ { (Q_TILDE__AMPERSAND_) }
	|	Q_TILDE__VBAR_ { (Q_TILDE__VBAR_) }
	|	Q_PLING__EQUALS_ { (Q_PLING__EQUALS_) }
	|	Q_VBAR__EQUALS_ { (Q_VBAR__EQUALS_) }
	|	Q_VBAR__EQUALS__GREATER_ { (Q_VBAR__EQUALS__GREATER_) }
	|	Q_VBAR__HYPHEN__GREATER_ { (Q_VBAR__HYPHEN__GREATER_) }
	|	Q_VBAR__VBAR_ { (Q_VBAR__VBAR_) }
	|	Q_PLUS__COLON_ { (Q_PLUS__COLON_) }
	|	Q_PLUS__EQUALS_ { (Q_PLUS__EQUALS_) }
	|	Q_PLUS__PLUS_ { (Q_PLUS__PLUS_) }
	|	Q_HASH__HASH_ { (Q_HASH__HASH_) }
	|	Q_STAR__STAR_ { (Q_STAR__STAR_) }
	|	Q_LESS__LESS_ { (Q_LESS__LESS_) }
	|	Q_LESS__LESS__EQUALS_ { (Q_LESS__LESS__EQUALS_) }
	|	Q_GREATER__GREATER_ { (Q_GREATER__GREATER_) }
	|	Q_GREATER__GREATER__EQUALS_ { (Q_GREATER__GREATER__EQUALS_) }
	|	Q_GREATER__GREATER__GREATER_ { (Q_GREATER__GREATER__GREATER_) }
	|	Q_GREATER__GREATER__GREATER__EQUALS_ { (Q_GREATER__GREATER__GREATER__EQUALS_) }
	|	Q_QUOTE_ { (Q_QUOTE_) }
	|	Q_QUOTE__LBRACE_ { (Q_QUOTE__LBRACE_) }
	|	Q_STAR__EQUALS_ { (Q_STAR__EQUALS_) }
	|	Q_EQUALS__EQUALS__QUERY_ { (Q_EQUALS__EQUALS__QUERY_) }
	|	Q_PLING__EQUALS__QUERY_ { (Q_PLING__EQUALS__QUERY_) }
	|	Q_CARET__TILDE_ { (Q_CARET__TILDE_) }
	|	Q_CARET__EQUALS_ { (Q_CARET__EQUALS_) }
	|	RAND { (RAND) }
	|	RANDC { (RANDC) }
	|	RCMOS { (RCMOS) }
	|	REAL { (REAL) }
	|	REALTIME { (REALTIME) }
	|	REF { (REF) }
	|	REG { (REG) }
	|	REPEAT { (REPEAT) }
	|	RESTRICT { (RESTRICT) }
	|	RETURN { (RETURN) }
	|	RNMOS { (RNMOS) }
	|	RPMOS { (RPMOS) }
	|	RTRAN { (RTRAN) }
	|	RTRANIF0 { (RTRANIF0) }
	|	RTRANIF1 { (RTRANIF1) }
	|	SCALARED { (SCALARED) }
	|	SHORTINT { (SHORTINT) }
	|	SHORTREAL { (SHORTREAL) }
	|	SIGNED { (SIGNED) }
	|	SPECPARAM { (SPECPARAM) }
	|	STATIC { (STATIC) }
	|	STRING { (STRING) }
	|	STRUCT { (STRUCT) }
	|	SUPPLY0 { (SUPPLY0) }
	|	SUPPLY1 { (SUPPLY1) }
	|	TABLE { (TABLE) }
	|	TASK { (TASK) }
	|	TIME { (TIME) }
	|	TIMEPRECISION { (TIMEPRECISION) }
	|	TIMEUNIT { (TIMEUNIT) }
	|	TRAN { (TRAN) }
	|	TRANIF0 { (TRANIF0) }
	|	TRANIF1 { (TRANIF1) }
	|	TRI { (TRI) }
	|	TRI0 { (TRI0) }
	|	TRI1 { (TRI1) }
	|	TRUE { (TRUE) }
	|	TYPE { (TYPE) }
	|	TYPEDEF { (TYPEDEF) }
	|	UNION { (UNION) }
	|	UNIQUE { (UNIQUE) }
	|	UNIQUE0 { (UNIQUE0) }
	|	UNSIGNED { (UNSIGNED) }
	|	VAR { (VAR) }
	|	VECTORED { (VECTORED) }
	|	COVERAGE_UNDERSCORE_OFF { (COVERAGE_UNDERSCORE_OFF) }
	|	COVERAGE_UNDERSCORE_ON { (COVERAGE_UNDERSCORE_ON) }
	|	Q_HYPHEN__HYPHEN_FILE { (Q_HYPHEN__HYPHEN_FILE) }
	|	Q_HYPHEN__HYPHEN_LINES { (Q_HYPHEN__HYPHEN_LINES) }
	|	Q_HYPHEN__HYPHEN_MSG { (Q_HYPHEN__HYPHEN_MSG) }
	|	LINT_UNDERSCORE_OFF { (LINT_UNDERSCORE_OFF) }
	|	LINT_UNDERSCORE_ON { (LINT_UNDERSCORE_ON) }
	|	TRACING_UNDERSCORE_OFF { (TRACING_UNDERSCORE_OFF) }
	|	TRACING_UNDERSCORE_ON { (TRACING_UNDERSCORE_ON) }
	|	Q_SLASH__STAR_VERILATOR_BLANK_SC_UNDERSCORE_CLOCK_STAR__SLASH_ { (Q_SLASH__STAR_VERILATOR_BLANK_SC_UNDERSCORE_CLOCK_STAR__SLASH_) }
	|	Q_SLASH__STAR_VERILATOR_BLANK_CLOCKER_STAR__SLASH_ { (Q_SLASH__STAR_VERILATOR_BLANK_CLOCKER_STAR__SLASH_) }
	|	Q_SLASH__STAR_VERILATOR_BLANK_CLOCK_UNDERSCORE_ENABLE_STAR__SLASH_ { (Q_SLASH__STAR_VERILATOR_BLANK_CLOCK_UNDERSCORE_ENABLE_STAR__SLASH_) }
	|	Q_SLASH__STAR_VERILATOR_BLANK_COVERAGE_UNDERSCORE_BLOCK_UNDERSCORE_OFF_STAR__SLASH_ { (Q_SLASH__STAR_VERILATOR_BLANK_COVERAGE_UNDERSCORE_BLOCK_UNDERSCORE_OFF_STAR__SLASH_) }
	|	Q_SLASH__STAR_VERILATOR_BLANK_FULL_UNDERSCORE_CASE_STAR__SLASH_ { (Q_SLASH__STAR_VERILATOR_BLANK_FULL_UNDERSCORE_CASE_STAR__SLASH_) }
	|	Q_SLASH__STAR_VERILATOR_BLANK_INLINE_UNDERSCORE_MODULE_STAR__SLASH_ { (Q_SLASH__STAR_VERILATOR_BLANK_INLINE_UNDERSCORE_MODULE_STAR__SLASH_) }
	|	Q_SLASH__STAR_VERILATOR_BLANK_ISOLATE_UNDERSCORE_ASSIGNMENTS_STAR__SLASH_ { (Q_SLASH__STAR_VERILATOR_BLANK_ISOLATE_UNDERSCORE_ASSIGNMENTS_STAR__SLASH_) }
	|	Q_SLASH__STAR_VERILATOR_BLANK_NO_UNDERSCORE_CLOCKER_STAR__SLASH_ { (Q_SLASH__STAR_VERILATOR_BLANK_NO_UNDERSCORE_CLOCKER_STAR__SLASH_) }
	|	Q_SLASH__STAR_VERILATOR_BLANK_NO_UNDERSCORE_INLINE_UNDERSCORE_MODULE_STAR__SLASH_ { (Q_SLASH__STAR_VERILATOR_BLANK_NO_UNDERSCORE_INLINE_UNDERSCORE_MODULE_STAR__SLASH_) }
	|	Q_SLASH__STAR_VERILATOR_BLANK_NO_UNDERSCORE_INLINE_UNDERSCORE_TASK_STAR__SLASH_ { (Q_SLASH__STAR_VERILATOR_BLANK_NO_UNDERSCORE_INLINE_UNDERSCORE_TASK_STAR__SLASH_) }
	|	Q_SLASH__STAR_VERILATOR_BLANK_PARALLEL_UNDERSCORE_CASE_STAR__SLASH_ { (Q_SLASH__STAR_VERILATOR_BLANK_PARALLEL_UNDERSCORE_CASE_STAR__SLASH_) }
	|	Q_SLASH__STAR_VERILATOR_BLANK_PUBLIC_STAR__SLASH_ { (Q_SLASH__STAR_VERILATOR_BLANK_PUBLIC_STAR__SLASH_) }
	|	Q_SLASH__STAR_VERILATOR_BLANK_PUBLIC_UNDERSCORE_FLAT_STAR__SLASH_ { (Q_SLASH__STAR_VERILATOR_BLANK_PUBLIC_UNDERSCORE_FLAT_STAR__SLASH_) }
	|	Q_SLASH__STAR_VERILATOR_BLANK_PUBLIC_UNDERSCORE_FLAT_UNDERSCORE_RD_STAR__SLASH_ { (Q_SLASH__STAR_VERILATOR_BLANK_PUBLIC_UNDERSCORE_FLAT_UNDERSCORE_RD_STAR__SLASH_) }
	|	Q_SLASH__STAR_VERILATOR_BLANK_PUBLIC_UNDERSCORE_FLAT_UNDERSCORE_RW_STAR__SLASH_ { (Q_SLASH__STAR_VERILATOR_BLANK_PUBLIC_UNDERSCORE_FLAT_UNDERSCORE_RW_STAR__SLASH_) }
	|	Q_SLASH__STAR_VERILATOR_BLANK_PUBLIC_UNDERSCORE_MODULE_STAR__SLASH_ { (Q_SLASH__STAR_VERILATOR_BLANK_PUBLIC_UNDERSCORE_MODULE_STAR__SLASH_) }
	|	Q_SLASH__STAR_VERILATOR_BLANK_SC_UNDERSCORE_BV_STAR__SLASH_ { (Q_SLASH__STAR_VERILATOR_BLANK_SC_UNDERSCORE_BV_STAR__SLASH_) }
	|	Q_SLASH__STAR_VERILATOR_BLANK_SFORMAT_STAR__SLASH_ { (Q_SLASH__STAR_VERILATOR_BLANK_SFORMAT_STAR__SLASH_) }
	|	VOID { (VOID) }
	|	WHILE { (WHILE) }
	|	WIRE { (WIRE) }
	|	WREAL { (WREAL) }
	|	XNOR { (XNOR) }
	|	XOR { (XOR) }
	|	Q_DOLLAR__LBRACE_DPI_HYPHEN_SYS_RBRACE_ { (Q_DOLLAR__LBRACE_DPI_HYPHEN_SYS_RBRACE_) }
	|	Q_DOLLAR__LBRACE_IGNORED_HYPHEN_BBOX_HYPHEN_SYS_RBRACE_ { (Q_DOLLAR__LBRACE_IGNORED_HYPHEN_BBOX_HYPHEN_SYS_RBRACE_) }
	|	FLOATING_HYPHEN_POINT_BLANK_NUMBER { (FLOATING_HYPHEN_POINT_BLANK_NUMBER) }
	|	IDENTIFIER { (IDENTIFIER $1) }
	|	IDENTIFIER_HYPHEN_IN_HYPHEN_LEX { (IDENTIFIER_HYPHEN_IN_HYPHEN_LEX) }
	|	PACKAGE_HYPHEN_IDENTIFIER { (PACKAGE_HYPHEN_IDENTIFIER) }
	|	TYPE_HYPHEN_IDENTIFIER { (TYPE_HYPHEN_IDENTIFIER) }
	|	INTEGER_BLANK_NUMBER { (INTEGER_BLANK_NUMBER) }
	|	Q_BACKQUOTE_SYSTEMC_UNDERSCORE_IMPLEMENTATION_BLANK_BLOCK { (Q_BACKQUOTE_SYSTEMC_UNDERSCORE_IMPLEMENTATION_BLANK_BLOCK) }
	|	Q_BACKQUOTE_SYSTEMC_UNDERSCORE_IMP_UNDERSCORE_HEADER_BLANK_BLOCK { (Q_BACKQUOTE_SYSTEMC_UNDERSCORE_IMP_UNDERSCORE_HEADER_BLANK_BLOCK) }
	|	Q_BACKQUOTE_SYSTEMC_UNDERSCORE_HEADER_BLANK_BLOCK { (Q_BACKQUOTE_SYSTEMC_UNDERSCORE_HEADER_BLANK_BLOCK) }
	|	Q_BACKQUOTE_SYSTEMC_UNDERSCORE_DTOR_BLANK_BLOCK { (Q_BACKQUOTE_SYSTEMC_UNDERSCORE_DTOR_BLANK_BLOCK) }
	|	Q_BACKQUOTE_SYSTEMC_UNDERSCORE_INTERFACE_BLANK_BLOCK { (Q_BACKQUOTE_SYSTEMC_UNDERSCORE_INTERFACE_BLANK_BLOCK) }
	|	Q_BACKQUOTE_SYSTEMC_UNDERSCORE_CTOR_BLANK_BLOCK { (Q_BACKQUOTE_SYSTEMC_UNDERSCORE_CTOR_BLANK_BLOCK) }
	|	STRING4 { (STRING4) }
	|	STRING_HYPHEN_IGNORED { (STRING_HYPHEN_IGNORED) }
	|	TABLE_BLANK_LINE { (TABLE_BLANK_LINE) }
	|	TIME_BLANK_NUMBER { (TIME_BLANK_NUMBER) }
	|	TIMING_BLANK_SPEC_BLANK_ELEMENT { (TIMING_BLANK_SPEC_BLANK_ELEMENT) }
	|	SPECIFY specifyJunk ENDSPECIFY { TUPLE3(SPECIFY,$2,ENDSPECIFY) }
	|	ERROR_TOKEN { (ERROR_TOKEN) }

specparam_declaration: SPECPARAM junkToSemiList SEMICOLON { TUPLE3(SPECPARAM,$2,SEMICOLON) }

junkToSemiList: junkToSemi { ($1) }
	|	junkToSemiList junkToSemi { TUPLE2($1,$2) }

junkToSemi: PLING { (PLING) }
	|	HASH { (HASH) }
	|	PERCENT { (PERCENT) }
	|	AMPERSAND { (AMPERSAND) }
	|	LPAREN { (LPAREN) }
	|	RPAREN { (RPAREN) }
	|	STAR { (STAR) }
	|	PLUS { (PLUS) }
	|	COMMA { (COMMA) }
	|	HYPHEN { (HYPHEN) }
	|	DOT { (DOT) }
	|	SLASH { (SLASH) }
	|	COLON { (COLON) }
	|	LESS { (LESS) }
	|	EQUALS { (EQUALS) }
	|	GREATER { (GREATER) }
	|	QUERY { (QUERY) }
	|	AT { (AT) }
	|	LBRACK { (LBRACK) }
	|	RBRACK { (RBRACK) }
	|	CARET { (CARET) }
	|	LBRACE { (LBRACE) }
	|	VBAR { (VBAR) }
	|	RBRACE { (RBRACE) }
	|	TILDE { (TILDE) }
	|	ALWAYS { (ALWAYS) }
	|	ALWAYS_UNDERSCORE_COMB { (ALWAYS_UNDERSCORE_COMB) }
	|	ALWAYS_UNDERSCORE_FF { (ALWAYS_UNDERSCORE_FF) }
	|	ALWAYS_UNDERSCORE_LATCH { (ALWAYS_UNDERSCORE_LATCH) }
	|	AND { (AND) }
	|	ASSERT { (ASSERT) }
	|	ASSIGN { (ASSIGN) }
	|	ASSUME { (ASSUME) }
	|	AUTOMATIC { (AUTOMATIC) }
	|	BEGIN { (BEGIN) }
	|	BIND { (BIND) }
	|	BIT { (BIT) }
	|	BREAK { (BREAK) }
	|	BUF { (BUF) }
	|	BUFIF0 { (BUFIF0) }
	|	BUFIF1 { (BUFIF1) }
	|	BYTE { (BYTE) }
	|	CASE { (CASE) }
	|	CASEX { (CASEX) }
	|	CASEZ { (CASEZ) }
	|	CHANDLE { (CHANDLE) }
	|	CLOCKING { (CLOCKING) }
	|	CMOS { (CMOS) }
	|	CONST { (CONST) }
	|	CONST_HYPHEN_IN_HYPHEN_LEX { (CONST_HYPHEN_IN_HYPHEN_LEX) }
	|	CONST_HYPHEN_THEN_HYPHEN_REF { (CONST_HYPHEN_THEN_HYPHEN_REF) }
	|	CONTEXT { (CONTEXT) }
	|	CONTINUE { (CONTINUE) }
	|	COVER { (COVER) }
	|	DEASSIGN { (DEASSIGN) }
	|	DEFAULT { (DEFAULT) }
	|	DEFPARAM { (DEFPARAM) }
	|	DISABLE { (DISABLE) }
	|	DO { (DO) }
	|	Q_DOLLAR_ACOS { (Q_DOLLAR_ACOS) }
	|	Q_DOLLAR_ACOSH { (Q_DOLLAR_ACOSH) }
	|	Q_DOLLAR_ASIN { (Q_DOLLAR_ASIN) }
	|	Q_DOLLAR_ASINH { (Q_DOLLAR_ASINH) }
	|	Q_DOLLAR_ATAN { (Q_DOLLAR_ATAN) }
	|	Q_DOLLAR_ATAN2 { (Q_DOLLAR_ATAN2) }
	|	Q_DOLLAR_ATANH { (Q_DOLLAR_ATANH) }
	|	Q_DOLLAR_BITS { (Q_DOLLAR_BITS) }
	|	Q_DOLLAR_BITSTOREAL { (Q_DOLLAR_BITSTOREAL) }
	|	Q_DOLLAR_C { (Q_DOLLAR_C) }
	|	Q_DOLLAR_CEIL { (Q_DOLLAR_CEIL) }
	|	Q_DOLLAR_CLOG2 { (Q_DOLLAR_CLOG2) }
	|	Q_DOLLAR_COS { (Q_DOLLAR_COS) }
	|	Q_DOLLAR_COSH { (Q_DOLLAR_COSH) }
	|	Q_DOLLAR_COUNTONES { (Q_DOLLAR_COUNTONES) }
	|	Q_DOLLAR_DIMENSIONS { (Q_DOLLAR_DIMENSIONS) }
	|	Q_DOLLAR_DISPLAY { (Q_DOLLAR_DISPLAY) }
	|	Q_DOLLAR_ERROR { (Q_DOLLAR_ERROR) }
	|	Q_DOLLAR_EXP { (Q_DOLLAR_EXP) }
	|	Q_DOLLAR_FATAL { (Q_DOLLAR_FATAL) }
	|	Q_DOLLAR_FCLOSE { (Q_DOLLAR_FCLOSE) }
	|	Q_DOLLAR_FDISPLAY { (Q_DOLLAR_FDISPLAY) }
	|	Q_DOLLAR_FEOF { (Q_DOLLAR_FEOF) }
	|	Q_DOLLAR_FFLUSH { (Q_DOLLAR_FFLUSH) }
	|	Q_DOLLAR_FGETC { (Q_DOLLAR_FGETC) }
	|	Q_DOLLAR_FGETS { (Q_DOLLAR_FGETS) }
	|	Q_DOLLAR_FINISH { (Q_DOLLAR_FINISH) }
	|	Q_DOLLAR_FLOOR { (Q_DOLLAR_FLOOR) }
	|	Q_DOLLAR_FOPEN { (Q_DOLLAR_FOPEN) }
	|	Q_DOLLAR_FSCANF { (Q_DOLLAR_FSCANF) }
	|	Q_DOLLAR_FWRITE { (Q_DOLLAR_FWRITE) }
	|	Q_DOLLAR_HIGH { (Q_DOLLAR_HIGH) }
	|	Q_DOLLAR_HYPOT { (Q_DOLLAR_HYPOT) }
	|	Q_DOLLAR_INCREMENT { (Q_DOLLAR_INCREMENT) }
	|	Q_DOLLAR_INFO { (Q_DOLLAR_INFO) }
	|	Q_DOLLAR_ISUNKNOWN { (Q_DOLLAR_ISUNKNOWN) }
	|	Q_DOLLAR_ITOR { (Q_DOLLAR_ITOR) }
	|	Q_DOLLAR_LEFT { (Q_DOLLAR_LEFT) }
	|	Q_DOLLAR_LN { (Q_DOLLAR_LN) }
	|	Q_DOLLAR_LOG10 { (Q_DOLLAR_LOG10) }
	|	Q_DOLLAR_LOW { (Q_DOLLAR_LOW) }
	|	Q_DOLLAR_ONEHOT { (Q_DOLLAR_ONEHOT) }
	|	Q_DOLLAR_ONEHOT0 { (Q_DOLLAR_ONEHOT0) }
	|	Q_DOLLAR_PAST { (Q_DOLLAR_PAST) }
	|	Q_DOLLAR_POW { (Q_DOLLAR_POW) }
	|	Q_DOLLAR_RANDOM { (Q_DOLLAR_RANDOM) }
	|	Q_DOLLAR_READMEMB { (Q_DOLLAR_READMEMB) }
	|	Q_DOLLAR_READMEMH { (Q_DOLLAR_READMEMH) }
	|	Q_DOLLAR_REALTIME { (Q_DOLLAR_REALTIME) }
	|	Q_DOLLAR_REALTOBITS { (Q_DOLLAR_REALTOBITS) }
	|	Q_DOLLAR_RIGHT { (Q_DOLLAR_RIGHT) }
	|	Q_DOLLAR_RTOI { (Q_DOLLAR_RTOI) }
	|	Q_DOLLAR_SFORMAT { (Q_DOLLAR_SFORMAT) }
	|	Q_DOLLAR_SFORMATF { (Q_DOLLAR_SFORMATF) }
	|	Q_DOLLAR_SIGNED { (Q_DOLLAR_SIGNED) }
	|	Q_DOLLAR_SIN { (Q_DOLLAR_SIN) }
	|	Q_DOLLAR_SINH { (Q_DOLLAR_SINH) }
	|	Q_DOLLAR_SIZE { (Q_DOLLAR_SIZE) }
	|	Q_DOLLAR_SQRT { (Q_DOLLAR_SQRT) }
	|	Q_DOLLAR_SSCANF { (Q_DOLLAR_SSCANF) }
	|	Q_DOLLAR_STIME { (Q_DOLLAR_STIME) }
	|	Q_DOLLAR_STOP { (Q_DOLLAR_STOP) }
	|	Q_DOLLAR_SWRITE { (Q_DOLLAR_SWRITE) }
	|	Q_DOLLAR_SYSTEM { (Q_DOLLAR_SYSTEM) }
	|	Q_DOLLAR_TAN { (Q_DOLLAR_TAN) }
	|	Q_DOLLAR_TANH { (Q_DOLLAR_TANH) }
	|	Q_DOLLAR_TEST_DOLLAR_PLUSARGS { (Q_DOLLAR_TEST_DOLLAR_PLUSARGS) }
	|	Q_DOLLAR_TIME { (Q_DOLLAR_TIME) }
	|	Q_DOLLAR_UNIT { (Q_DOLLAR_UNIT) }
	|	Q_DOLLAR_UNPACKED_UNDERSCORE_DIMENSIONS { (Q_DOLLAR_UNPACKED_UNDERSCORE_DIMENSIONS) }
	|	Q_DOLLAR_UNSIGNED { (Q_DOLLAR_UNSIGNED) }
	|	Q_DOLLAR_VALUE_DOLLAR_PLUSARGS { (Q_DOLLAR_VALUE_DOLLAR_PLUSARGS) }
	|	Q_DOLLAR_WARNING { (Q_DOLLAR_WARNING) }
	|	Q_DOLLAR_WRITE { (Q_DOLLAR_WRITE) }
	|	Q_DOLLAR_WRITEMEMH { (Q_DOLLAR_WRITEMEMH) }
	|	EDGE { (EDGE) }
	|	ELSE { (ELSE) }
	|	END { (END) }
	|	ENDCASE { (ENDCASE) }
	|	ENDCLOCKING { (ENDCLOCKING) }
	|	ENDFUNCTION { (ENDFUNCTION) }
	|	ENDGENERATE { (ENDGENERATE) }
	|	ENDINTERFACE { (ENDINTERFACE) }
	|	ENDPACKAGE { (ENDPACKAGE) }
	|	ENDPRIMITIVE { (ENDPRIMITIVE) }
	|	ENDPROGRAM { (ENDPROGRAM) }
	|	ENDPROPERTY { (ENDPROPERTY) }
	|	ENDTABLE { (ENDTABLE) }
	|	ENDTASK { (ENDTASK) }
	|	ENUM { (ENUM) }
	|	EXPORT { (EXPORT) }
	|	EXTERN { (EXTERN) }
	|	FINAL { (FINAL) }
	|	FOR { (FOR) }
	|	FOREACH { (FOREACH) }
	|	FOREVER { (FOREVER) }
	|	FORKJOIN { (FORKJOIN) }
	|	FUNCTION { (FUNCTION) }
	|	GENERATE { (GENERATE) }
	|	GENVAR { (GENVAR) }
	|	GLOBAL_HYPHEN_THEN_HYPHEN_CLOCKING { (GLOBAL_HYPHEN_THEN_HYPHEN_CLOCKING) }
	|	GLOBAL_HYPHEN_IN_HYPHEN_LEX { (GLOBAL_HYPHEN_IN_HYPHEN_LEX) }
	|	IF { (IF) }
	|	IFF { (IFF) }
	|	IMPORT { (IMPORT) }
	|	INITIAL { (INITIAL) }
	|	INOUT { (INOUT) }
	|	INPUT { (INPUT) }
	|	INSIDE { (INSIDE) }
	|	INT { (INT) }
	|	INTEGER { (INTEGER) }
	|	INTERFACE { (INTERFACE) }
	|	LOCALPARAM { (LOCALPARAM) }
	|	LOGIC { (LOGIC) }
	|	LONGINT { (LONGINT) }
	|	MODPORT { (MODPORT) }
	|	MODULE { (MODULE) }
	|	NAND { (NAND) }
	|	NEGEDGE { (NEGEDGE) }
	|	NMOS { (NMOS) }
	|	NOR { (NOR) }
	|	NOT { (NOT) }
	|	NOTIF0 { (NOTIF0) }
	|	NOTIF1 { (NOTIF1) }
	|	OR { (OR) }
	|	OUTPUT { (OUTPUT) }
	|	PACKAGE { (PACKAGE) }
	|	PACKED { (PACKED) }
	|	PARAMETER { (PARAMETER) }
	|	PMOS { (PMOS) }
	|	POSEDGE { (POSEDGE) }
	|	PRIMITIVE { (PRIMITIVE) }
	|	PRIORITY { (PRIORITY) }
	|	PROGRAM { (PROGRAM) }
	|	PROPERTY { (PROPERTY) }
	|	PULLDOWN { (PULLDOWN) }
	|	PULLUP { (PULLUP) }
	|	PURE { (PURE) }
	|	Q_AMPERSAND__AMPERSAND_ { (Q_AMPERSAND__AMPERSAND_) }
	|	Q_AMPERSAND__AMPERSAND__AMPERSAND_ { (Q_AMPERSAND__AMPERSAND__AMPERSAND_) }
	|	Q_AMPERSAND__EQUALS_ { (Q_AMPERSAND__EQUALS_) }
	|	Q_STAR__GREATER_ { (Q_STAR__GREATER_) }
	|	Q_AT__AT_ { (Q_AT__AT_) }
	|	Q_LBRACK__EQUALS_ { (Q_LBRACK__EQUALS_) }
	|	Q_LBRACK__HYPHEN__GREATER_ { (Q_LBRACK__HYPHEN__GREATER_) }
	|	Q_LBRACK__STAR_ { (Q_LBRACK__STAR_) }
	|	Q_EQUALS__EQUALS__EQUALS_ { (Q_EQUALS__EQUALS__EQUALS_) }
	|	Q_PLING__EQUALS__EQUALS_ { (Q_PLING__EQUALS__EQUALS_) }
	|	Q_COLON__COLON_ { (Q_COLON__COLON_) }
	|	Q_COLON__SLASH_ { (Q_COLON__SLASH_) }
	|	Q_COLON__EQUALS_ { (Q_COLON__EQUALS_) }
	|	Q_SLASH__EQUALS_ { (Q_SLASH__EQUALS_) }
	|	Q_DOT__STAR_ { (Q_DOT__STAR_) }
	|	Q_EQUALS__GREATER_ { (Q_EQUALS__GREATER_) }
	|	Q_EQUALS__EQUALS_ { (Q_EQUALS__EQUALS_) }
	|	Q_GREATER__EQUALS_ { (Q_GREATER__EQUALS_) }
	|	YP_LOGIFF { ( YP_LOGIFF ) }
	|	Q_LESS__EQUALS_ { (Q_LESS__EQUALS_) }
	|	Q_LESS__EQUALS__HYPHEN_IGNORED { (Q_LESS__EQUALS__HYPHEN_IGNORED) }
	|	Q_HYPHEN__COLON_ { (Q_HYPHEN__COLON_) }
	|	Q_HYPHEN__EQUALS_ { (Q_HYPHEN__EQUALS_) }
	|	Q_HYPHEN__GREATER_ { (Q_HYPHEN__GREATER_) }
	|	Q_HYPHEN__GREATER__GREATER_ { (Q_HYPHEN__GREATER__GREATER_) }
	|	Q_HYPHEN__HYPHEN_ { (Q_HYPHEN__HYPHEN_) }
	|	Q_PERCENT__EQUALS_ { (Q_PERCENT__EQUALS_) }
	|	Q_TILDE__AMPERSAND_ { (Q_TILDE__AMPERSAND_) }
	|	Q_TILDE__VBAR_ { (Q_TILDE__VBAR_) }
	|	Q_PLING__EQUALS_ { (Q_PLING__EQUALS_) }
	|	Q_VBAR__EQUALS_ { (Q_VBAR__EQUALS_) }
	|	Q_VBAR__EQUALS__GREATER_ { (Q_VBAR__EQUALS__GREATER_) }
	|	Q_VBAR__HYPHEN__GREATER_ { (Q_VBAR__HYPHEN__GREATER_) }
	|	Q_VBAR__VBAR_ { (Q_VBAR__VBAR_) }
	|	Q_PLUS__COLON_ { (Q_PLUS__COLON_) }
	|	Q_PLUS__EQUALS_ { (Q_PLUS__EQUALS_) }
	|	Q_PLUS__PLUS_ { (Q_PLUS__PLUS_) }
	|	Q_HASH__HASH_ { (Q_HASH__HASH_) }
	|	Q_STAR__STAR_ { (Q_STAR__STAR_) }
	|	Q_LESS__LESS_ { (Q_LESS__LESS_) }
	|	Q_LESS__LESS__EQUALS_ { (Q_LESS__LESS__EQUALS_) }
	|	Q_GREATER__GREATER_ { (Q_GREATER__GREATER_) }
	|	Q_GREATER__GREATER__EQUALS_ { (Q_GREATER__GREATER__EQUALS_) }
	|	Q_GREATER__GREATER__GREATER_ { (Q_GREATER__GREATER__GREATER_) }
	|	Q_GREATER__GREATER__GREATER__EQUALS_ { (Q_GREATER__GREATER__GREATER__EQUALS_) }
	|	Q_QUOTE_ { (Q_QUOTE_) }
	|	Q_QUOTE__LBRACE_ { (Q_QUOTE__LBRACE_) }
	|	Q_STAR__EQUALS_ { (Q_STAR__EQUALS_) }
	|	Q_EQUALS__EQUALS__QUERY_ { (Q_EQUALS__EQUALS__QUERY_) }
	|	Q_PLING__EQUALS__QUERY_ { (Q_PLING__EQUALS__QUERY_) }
	|	Q_CARET__TILDE_ { (Q_CARET__TILDE_) }
	|	Q_CARET__EQUALS_ { (Q_CARET__EQUALS_) }
	|	RAND { (RAND) }
	|	RANDC { (RANDC) }
	|	RCMOS { (RCMOS) }
	|	REAL { (REAL) }
	|	REALTIME { (REALTIME) }
	|	REF { (REF) }
	|	REG { (REG) }
	|	REPEAT { (REPEAT) }
	|	RESTRICT { (RESTRICT) }
	|	RETURN { (RETURN) }
	|	RNMOS { (RNMOS) }
	|	RPMOS { (RPMOS) }
	|	RTRAN { (RTRAN) }
	|	RTRANIF0 { (RTRANIF0) }
	|	RTRANIF1 { (RTRANIF1) }
	|	SCALARED { (SCALARED) }
	|	SHORTINT { (SHORTINT) }
	|	SHORTREAL { (SHORTREAL) }
	|	SIGNED { (SIGNED) }
	|	SPECIFY { (SPECIFY) }
	|	SPECPARAM { (SPECPARAM) }
	|	STATIC { (STATIC) }
	|	STRING { (STRING) }
	|	STRUCT { (STRUCT) }
	|	SUPPLY0 { (SUPPLY0) }
	|	SUPPLY1 { (SUPPLY1) }
	|	TABLE { (TABLE) }
	|	TASK { (TASK) }
	|	TIME { (TIME) }
	|	TIMEPRECISION { (TIMEPRECISION) }
	|	TIMEUNIT { (TIMEUNIT) }
	|	TRAN { (TRAN) }
	|	TRANIF0 { (TRANIF0) }
	|	TRANIF1 { (TRANIF1) }
	|	TRI { (TRI) }
	|	TRI0 { (TRI0) }
	|	TRI1 { (TRI1) }
	|	TRUE { (TRUE) }
	|	TYPE { (TYPE) }
	|	TYPEDEF { (TYPEDEF) }
	|	UNION { (UNION) }
	|	UNIQUE { (UNIQUE) }
	|	UNIQUE0 { (UNIQUE0) }
	|	UNSIGNED { (UNSIGNED) }
	|	VAR { (VAR) }
	|	VECTORED { (VECTORED) }
	|	COVERAGE_UNDERSCORE_OFF { (COVERAGE_UNDERSCORE_OFF) }
	|	COVERAGE_UNDERSCORE_ON { (COVERAGE_UNDERSCORE_ON) }
	|	Q_HYPHEN__HYPHEN_FILE { (Q_HYPHEN__HYPHEN_FILE) }
	|	Q_HYPHEN__HYPHEN_LINES { (Q_HYPHEN__HYPHEN_LINES) }
	|	Q_HYPHEN__HYPHEN_MSG { (Q_HYPHEN__HYPHEN_MSG) }
	|	LINT_UNDERSCORE_OFF { (LINT_UNDERSCORE_OFF) }
	|	LINT_UNDERSCORE_ON { (LINT_UNDERSCORE_ON) }
	|	TRACING_UNDERSCORE_OFF { (TRACING_UNDERSCORE_OFF) }
	|	TRACING_UNDERSCORE_ON { (TRACING_UNDERSCORE_ON) }
	|	Q_SLASH__STAR_VERILATOR_BLANK_SC_UNDERSCORE_CLOCK_STAR__SLASH_ { (Q_SLASH__STAR_VERILATOR_BLANK_SC_UNDERSCORE_CLOCK_STAR__SLASH_) }
	|	Q_SLASH__STAR_VERILATOR_BLANK_CLOCKER_STAR__SLASH_ { (Q_SLASH__STAR_VERILATOR_BLANK_CLOCKER_STAR__SLASH_) }
	|	Q_SLASH__STAR_VERILATOR_BLANK_CLOCK_UNDERSCORE_ENABLE_STAR__SLASH_ { (Q_SLASH__STAR_VERILATOR_BLANK_CLOCK_UNDERSCORE_ENABLE_STAR__SLASH_) }
	|	Q_SLASH__STAR_VERILATOR_BLANK_COVERAGE_UNDERSCORE_BLOCK_UNDERSCORE_OFF_STAR__SLASH_ { (Q_SLASH__STAR_VERILATOR_BLANK_COVERAGE_UNDERSCORE_BLOCK_UNDERSCORE_OFF_STAR__SLASH_) }
	|	Q_SLASH__STAR_VERILATOR_BLANK_FULL_UNDERSCORE_CASE_STAR__SLASH_ { (Q_SLASH__STAR_VERILATOR_BLANK_FULL_UNDERSCORE_CASE_STAR__SLASH_) }
	|	Q_SLASH__STAR_VERILATOR_BLANK_INLINE_UNDERSCORE_MODULE_STAR__SLASH_ { (Q_SLASH__STAR_VERILATOR_BLANK_INLINE_UNDERSCORE_MODULE_STAR__SLASH_) }
	|	Q_SLASH__STAR_VERILATOR_BLANK_ISOLATE_UNDERSCORE_ASSIGNMENTS_STAR__SLASH_ { (Q_SLASH__STAR_VERILATOR_BLANK_ISOLATE_UNDERSCORE_ASSIGNMENTS_STAR__SLASH_) }
	|	Q_SLASH__STAR_VERILATOR_BLANK_NO_UNDERSCORE_CLOCKER_STAR__SLASH_ { (Q_SLASH__STAR_VERILATOR_BLANK_NO_UNDERSCORE_CLOCKER_STAR__SLASH_) }
	|	Q_SLASH__STAR_VERILATOR_BLANK_NO_UNDERSCORE_INLINE_UNDERSCORE_MODULE_STAR__SLASH_ { (Q_SLASH__STAR_VERILATOR_BLANK_NO_UNDERSCORE_INLINE_UNDERSCORE_MODULE_STAR__SLASH_) }
	|	Q_SLASH__STAR_VERILATOR_BLANK_NO_UNDERSCORE_INLINE_UNDERSCORE_TASK_STAR__SLASH_ { (Q_SLASH__STAR_VERILATOR_BLANK_NO_UNDERSCORE_INLINE_UNDERSCORE_TASK_STAR__SLASH_) }
	|	Q_SLASH__STAR_VERILATOR_BLANK_PARALLEL_UNDERSCORE_CASE_STAR__SLASH_ { (Q_SLASH__STAR_VERILATOR_BLANK_PARALLEL_UNDERSCORE_CASE_STAR__SLASH_) }
	|	Q_SLASH__STAR_VERILATOR_BLANK_PUBLIC_STAR__SLASH_ { (Q_SLASH__STAR_VERILATOR_BLANK_PUBLIC_STAR__SLASH_) }
	|	Q_SLASH__STAR_VERILATOR_BLANK_PUBLIC_UNDERSCORE_FLAT_STAR__SLASH_ { (Q_SLASH__STAR_VERILATOR_BLANK_PUBLIC_UNDERSCORE_FLAT_STAR__SLASH_) }
	|	Q_SLASH__STAR_VERILATOR_BLANK_PUBLIC_UNDERSCORE_FLAT_UNDERSCORE_RD_STAR__SLASH_ { (Q_SLASH__STAR_VERILATOR_BLANK_PUBLIC_UNDERSCORE_FLAT_UNDERSCORE_RD_STAR__SLASH_) }
	|	Q_SLASH__STAR_VERILATOR_BLANK_PUBLIC_UNDERSCORE_FLAT_UNDERSCORE_RW_STAR__SLASH_ { (Q_SLASH__STAR_VERILATOR_BLANK_PUBLIC_UNDERSCORE_FLAT_UNDERSCORE_RW_STAR__SLASH_) }
	|	Q_SLASH__STAR_VERILATOR_BLANK_PUBLIC_UNDERSCORE_MODULE_STAR__SLASH_ { (Q_SLASH__STAR_VERILATOR_BLANK_PUBLIC_UNDERSCORE_MODULE_STAR__SLASH_) }
	|	Q_SLASH__STAR_VERILATOR_BLANK_SC_UNDERSCORE_BV_STAR__SLASH_ { (Q_SLASH__STAR_VERILATOR_BLANK_SC_UNDERSCORE_BV_STAR__SLASH_) }
	|	Q_SLASH__STAR_VERILATOR_BLANK_SFORMAT_STAR__SLASH_ { (Q_SLASH__STAR_VERILATOR_BLANK_SFORMAT_STAR__SLASH_) }
	|	VOID { (VOID) }
	|	WHILE { (WHILE) }
	|	WIRE { (WIRE) }
	|	WREAL { (WREAL) }
	|	XNOR { (XNOR) }
	|	XOR { (XOR) }
	|	Q_DOLLAR__LBRACE_DPI_HYPHEN_SYS_RBRACE_ { (Q_DOLLAR__LBRACE_DPI_HYPHEN_SYS_RBRACE_) }
	|	Q_DOLLAR__LBRACE_IGNORED_HYPHEN_BBOX_HYPHEN_SYS_RBRACE_ { (Q_DOLLAR__LBRACE_IGNORED_HYPHEN_BBOX_HYPHEN_SYS_RBRACE_) }
	|	FLOATING_HYPHEN_POINT_BLANK_NUMBER { (FLOATING_HYPHEN_POINT_BLANK_NUMBER) }
	|	IDENTIFIER { (IDENTIFIER $1) }
	|	IDENTIFIER_HYPHEN_IN_HYPHEN_LEX { (IDENTIFIER_HYPHEN_IN_HYPHEN_LEX) }
	|	PACKAGE_HYPHEN_IDENTIFIER { (PACKAGE_HYPHEN_IDENTIFIER) }
	|	TYPE_HYPHEN_IDENTIFIER { (TYPE_HYPHEN_IDENTIFIER) }
	|	INTEGER_BLANK_NUMBER { (INTEGER_BLANK_NUMBER) }
	|	Q_BACKQUOTE_SYSTEMC_UNDERSCORE_IMPLEMENTATION_BLANK_BLOCK { (Q_BACKQUOTE_SYSTEMC_UNDERSCORE_IMPLEMENTATION_BLANK_BLOCK) }
	|	Q_BACKQUOTE_SYSTEMC_UNDERSCORE_IMP_UNDERSCORE_HEADER_BLANK_BLOCK { (Q_BACKQUOTE_SYSTEMC_UNDERSCORE_IMP_UNDERSCORE_HEADER_BLANK_BLOCK) }
	|	Q_BACKQUOTE_SYSTEMC_UNDERSCORE_HEADER_BLANK_BLOCK { (Q_BACKQUOTE_SYSTEMC_UNDERSCORE_HEADER_BLANK_BLOCK) }
	|	Q_BACKQUOTE_SYSTEMC_UNDERSCORE_DTOR_BLANK_BLOCK { (Q_BACKQUOTE_SYSTEMC_UNDERSCORE_DTOR_BLANK_BLOCK) }
	|	Q_BACKQUOTE_SYSTEMC_UNDERSCORE_INTERFACE_BLANK_BLOCK { (Q_BACKQUOTE_SYSTEMC_UNDERSCORE_INTERFACE_BLANK_BLOCK) }
	|	Q_BACKQUOTE_SYSTEMC_UNDERSCORE_CTOR_BLANK_BLOCK { (Q_BACKQUOTE_SYSTEMC_UNDERSCORE_CTOR_BLANK_BLOCK) }
	|	STRING5 { (STRING5) }
	|	STRING_HYPHEN_IGNORED { (STRING_HYPHEN_IGNORED) }
	|	TABLE_BLANK_LINE { (TABLE_BLANK_LINE) }
	|	TIME_BLANK_NUMBER { (TIME_BLANK_NUMBER) }
	|	TIMING_BLANK_SPEC_BLANK_ELEMENT { (TIMING_BLANK_SPEC_BLANK_ELEMENT) }
	|	ERROR_TOKEN { (ERROR_TOKEN) }

id: IDENTIFIER { (IDENTIFIER $1) }

idAny: PACKAGE_HYPHEN_IDENTIFIER { (PACKAGE_HYPHEN_IDENTIFIER) }
	|	TYPE_HYPHEN_IDENTIFIER { (TYPE_HYPHEN_IDENTIFIER) }
	|	IDENTIFIER { (IDENTIFIER $1) }

idSVKwd: DO { (DO) }
	|	FINAL { (FINAL) }

variable_lvalue: idClassSel { ($1) }
	|	LBRACE variable_lvalueConcList RBRACE { TUPLE3(LBRACE,$2,RBRACE) }
	|	streaming_concatenation { ($1) }

variable_lvalueConcList: variable_lvalue { ($1) }
	|	variable_lvalueConcList COMMA variable_lvalue { TUPLE3($1,COMMA,$3) }

idClassSel: idDotted { ($1) }

idDotted: idDottedMore { ($1) }

idDottedMore: idArrayed { ($1) }
	|	idDottedMore DOT idArrayed { TUPLE3($1,DOT,$3) }

idArrayed: id { ($1) }
	|	idArrayed LBRACK expr RBRACK { TUPLE4($1,LBRACK,$3,RBRACK) }
	|	idArrayed LBRACK constExpr COLON constExpr RBRACK { TUPLE6($1,LBRACK,$3,COLON,$5,RBRACK) }
	|	idArrayed LBRACK expr Q_PLUS__COLON_ constExpr RBRACK { TUPLE6($1,LBRACK,$3,Q_PLUS__COLON_,$5,RBRACK) }
	|	idArrayed LBRACK expr Q_HYPHEN__COLON_ constExpr RBRACK { TUPLE6($1,LBRACK,$3,Q_HYPHEN__COLON_,$5,RBRACK) }

idClassForeach: idForeach { ($1) }
	|	package_scopeIdFollows idForeach { TUPLE2($1,$2) }

idForeach: varRefBase { ($1) }
	|	idForeach DOT varRefBase { TUPLE3($1,DOT,$3) }

varRefBase: id { ($1) }

str: STRING6 { (STRING6) }

strAsInt: NUMBER { (NUMBER $1) }

strAsIntIgnore: STRING_HYPHEN_IGNORED { (STRING_HYPHEN_IGNORED) }

strAsText: STRING8 { (STRING8) }

endLabelE: /* empty */ { EMPTY_TOKEN }
	|	COLON idAny { TUPLE2(COLON,$2) }

clocking_declaration: DEFAULT CLOCKING AT LPAREN senitemEdge RPAREN SEMICOLON ENDCLOCKING { TUPLE8(DEFAULT,CLOCKING,AT,LPAREN,$5,RPAREN,SEMICOLON,ENDCLOCKING) }

labeledStmt: immediate_assert_statement { ($1) }
	|	immediate_assume_statement { ($1) }

concurrent_assertion_item: concurrent_assertion_statement { ($1) }
	|	id COLON concurrent_assertion_statement { TUPLE3($1,COLON,$3) }

concurrent_assertion_statement: ASSERT PROPERTY LPAREN property_spec RPAREN elseStmtBlock { TUPLE6(ASSERT,PROPERTY,LPAREN,$4,RPAREN,$6) }
	|	COVER PROPERTY LPAREN property_spec RPAREN stmtBlock { TUPLE6(COVER,PROPERTY,LPAREN,$4,RPAREN,$6) }
	|	RESTRICT PROPERTY LPAREN property_spec RPAREN SEMICOLON { TUPLE6(RESTRICT,PROPERTY,LPAREN,$4,RPAREN,SEMICOLON) }

elseStmtBlock: SEMICOLON { (SEMICOLON) }
	|	ELSE stmtBlock { TUPLE2(ELSE,$2) }

property_spec: AT LPAREN senitemEdge RPAREN DISABLE IFF LPAREN expr RPAREN expr { TUPLE10(AT,LPAREN,$3,RPAREN,DISABLE,IFF,LPAREN,$8,RPAREN,$10) }
	|	AT LPAREN senitemEdge RPAREN expr { TUPLE5(AT,LPAREN,$3,RPAREN,$5) }
	|	DISABLE IFF LPAREN expr RPAREN expr { TUPLE6(DISABLE,IFF,LPAREN,$4,RPAREN,$6) }
	|	expr { ($1) }

immediate_assert_statement: ASSERT LPAREN expr RPAREN stmtBlock { TUPLE5(ASSERT,LPAREN,$3,RPAREN,$5) }
	|	ASSERT LPAREN expr RPAREN ELSE stmtBlock { TUPLE6(ASSERT,LPAREN,$3,RPAREN,ELSE,$6) }
	|	ASSERT LPAREN expr RPAREN stmtBlock ELSE stmtBlock { TUPLE7(ASSERT,LPAREN,$3,RPAREN,$5,ELSE,$7) }

immediate_assume_statement: ASSUME LPAREN expr RPAREN stmtBlock { TUPLE5(ASSUME,LPAREN,$3,RPAREN,$5) }
	|	ASSUME LPAREN expr RPAREN ELSE stmtBlock { TUPLE6(ASSUME,LPAREN,$3,RPAREN,ELSE,$6) }
	|	ASSUME LPAREN expr RPAREN stmtBlock ELSE stmtBlock { TUPLE7(ASSUME,LPAREN,$3,RPAREN,$5,ELSE,$7) }

ps_id_etc: package_scopeIdFollowsE id { TUPLE2($1,$2) }

ps_type: package_scopeIdFollowsE TYPE_HYPHEN_IDENTIFIER { TUPLE2($1,TYPE_HYPHEN_IDENTIFIER) }

package_scopeIdFollowsE: /* empty */ { EMPTY_TOKEN }
	|	package_scopeIdFollows { ($1) }

package_scopeIdFollows: Q_DOLLAR_UNIT /* 23 */ Q_COLON__COLON_ { TUPLE2(Q_DOLLAR_UNIT,Q_COLON__COLON_) }
	|	PACKAGE_HYPHEN_IDENTIFIER /* 24 */ Q_COLON__COLON_ { TUPLE2(PACKAGE_HYPHEN_IDENTIFIER,Q_COLON__COLON_) }

vltItem: vltOffFront { ($1) }
	|	vltOffFront Q_HYPHEN__HYPHEN_FILE STRING9 { TUPLE3($1,Q_HYPHEN__HYPHEN_FILE,STRING9) }
	|	vltOffFront Q_HYPHEN__HYPHEN_FILE STRING10 Q_HYPHEN__HYPHEN_LINES INTEGER_BLANK_NUMBER { TUPLE5($1,Q_HYPHEN__HYPHEN_FILE,STRING10,Q_HYPHEN__HYPHEN_LINES,INTEGER_BLANK_NUMBER) }
	|	vltOffFront Q_HYPHEN__HYPHEN_FILE STRING11 Q_HYPHEN__HYPHEN_LINES INTEGER_BLANK_NUMBER HYPHEN INTEGER_BLANK_NUMBER { TUPLE7($1,Q_HYPHEN__HYPHEN_FILE,STRING11,Q_HYPHEN__HYPHEN_LINES,INTEGER_BLANK_NUMBER,HYPHEN,INTEGER_BLANK_NUMBER) }
	|	vltOnFront { ($1) }
	|	vltOnFront Q_HYPHEN__HYPHEN_FILE STRING12 { TUPLE3($1,Q_HYPHEN__HYPHEN_FILE,STRING12) }
	|	vltOnFront Q_HYPHEN__HYPHEN_FILE STRING13 Q_HYPHEN__HYPHEN_LINES INTEGER_BLANK_NUMBER { TUPLE5($1,Q_HYPHEN__HYPHEN_FILE,STRING13,Q_HYPHEN__HYPHEN_LINES,INTEGER_BLANK_NUMBER) }
	|	vltOnFront Q_HYPHEN__HYPHEN_FILE STRING14 Q_HYPHEN__HYPHEN_LINES INTEGER_BLANK_NUMBER HYPHEN INTEGER_BLANK_NUMBER { TUPLE7($1,Q_HYPHEN__HYPHEN_FILE,STRING14,Q_HYPHEN__HYPHEN_LINES,INTEGER_BLANK_NUMBER,HYPHEN,INTEGER_BLANK_NUMBER) }

vltOffFront: COVERAGE_UNDERSCORE_OFF { (COVERAGE_UNDERSCORE_OFF) }
	|	TRACING_UNDERSCORE_OFF { (TRACING_UNDERSCORE_OFF) }
	|	LINT_UNDERSCORE_OFF { (LINT_UNDERSCORE_OFF) }
	|	LINT_UNDERSCORE_OFF Q_HYPHEN__HYPHEN_MSG IDENTIFIER { TUPLE3(LINT_UNDERSCORE_OFF,Q_HYPHEN__HYPHEN_MSG,IDENTIFIER $3) }

vltOnFront: COVERAGE_UNDERSCORE_ON { (COVERAGE_UNDERSCORE_ON) }
	|	TRACING_UNDERSCORE_ON { (TRACING_UNDERSCORE_ON) }
	|	LINT_UNDERSCORE_ON { (LINT_UNDERSCORE_ON) }
	|	LINT_UNDERSCORE_ON Q_HYPHEN__HYPHEN_MSG IDENTIFIER { TUPLE3(LINT_UNDERSCORE_ON,Q_HYPHEN__HYPHEN_MSG,IDENTIFIER $3) }


