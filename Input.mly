%{
  open Parsing
  open Input_types
  let stderr = open_out "parser_stderr.log" (* to capture parser trace mode info *)
  let declst = ref []
  let packhash_add id_t = Hashtbl.add packhash id_t ()
  let typehash_add id_t = Hashtbl.add typehash id_t ()
%}

%token  ACCEPT
%token  AMPERSAND
%token  AT
%token  ATTR_BEGIN
%token  ATTR_END
%token  BACKQUOTE
%token  BACKSLASH
%token  CARET
%token  COLON
%token  COMMA
%token <token> CONS1
%token <token*token> CONS2
%token <token*token*token> CONS3
%token <token*token*token*token> CONS4
%token  DEFATTR_BEGIN
%token  DEFATTR_END
%token  DEFAULT
%token  DOLLAR
%token  DOT
%token  DOUBLEQUOTE
%token <token list> ELIST
%token  EMPTY_TOKEN
%token  END
%token  EOF_TOKEN
%token  EQUALS
%token  ERROR
%token  ERROR_TOKEN
%token  FAKE_THEN
%token  GREATER
%token  HASH
%token  HYPHEN
%token  LBRACE
%token  LBRACK
%token  LESS
%token  LINEFEED
%token  LPAREN
%token  OP_EQ
%token  OP_EQX
%token  OP_GE
%token  OP_LAND
%token  OP_LE
%token  OP_LOR
%token  OP_NAND
%token  OP_NE
%token  OP_NEX
%token  OP_NOR
%token  OP_POW
%token  OP_SHL
%token  OP_SHR
%token  OP_SSHL
%token  OP_SSHR
%token  OP_XNOR
%token  PERCENT
%token  PLING
%token  PLUS
%token  QUERY
%token  QUOTE
%token  RBRACE
%token  RBRACK
%token  RPAREN
%token  SEMICOLON
%token  SLASH
%token <string list> SLIST
%token  STAR
%token <string> STRING
%token  TILDE
%token <token list> TLIST
%token  TOK_ALWAYS
%token  TOK_ALWAYS_COMB
%token  TOK_ALWAYS_FF
%token  TOK_ALWAYS_LATCH
%token  TOK_AND_ASSIGN
%token  TOK_ASSERT
%token  TOK_ASSIGN
%token  TOK_ASSUME
%token  TOK_AUTOMATIC
%token  TOK_BASE
%token  TOK_BASED_CONSTVAL
%token  TOK_BEGIN
%token  TOK_BYTE
%token  TOK_CASE
%token  TOK_CASEX
%token  TOK_CASEZ
%token  TOK_CHECKER
%token  TOK_CONST
%token  TOK_CONSTVAL
%token  TOK_COVER
%token  TOK_DECREMENT
%token  TOK_DEFAULT
%token  TOK_DEFPARAM
%token  TOK_DPI_FUNCTION
%token  TOK_ELSE
%token  TOK_END
%token  TOK_ENDCASE
%token  TOK_ENDCHECKER
%token  TOK_ENDFUNCTION
%token  TOK_ENDGENERATE
%token  TOK_ENDINTERFACE
%token  TOK_ENDMODULE
%token  TOK_ENDPACKAGE
%token  TOK_ENDSPECIFY
%token  TOK_ENDTASK
%token  TOK_ENUM
%token  TOK_EVENTUALLY
%token  TOK_FINAL
%token  TOK_FOR
%token  TOK_FUNCTION
%token  TOK_GENERATE
%token  TOK_GENVAR
%token  TOK_HIGHZ0
%token  TOK_HIGHZ1
%token  TOK_ID
%token  TOK_IF
%token  TOK_IGNORED_SPECIFY
%token  TOK_IGNORED_SPECIFY_AND
%token  TOK_IMPORT_PACKAGE
%token  TOK_INCREMENT
%token  TOK_INITIAL
%token  TOK_INOUT
%token  TOK_INPUT
%token  TOK_INSIDE
%token  TOK_INT
%token  TOK_INTEGER
%token  TOK_INTERFACE
%token  TOK_LOCALPARAM
%token  TOK_LOGIC
%token  TOK_MODPORT
%token  TOK_MODULE
%token  TOK_MSG_TASKS
%token  TOK_NEGEDGE
%token  TOK_NEG_INDEXED
%token  TOK_OR
%token  TOK_OR_ASSIGN
%token  TOK_OUTPUT
%token  TOK_PACKAGE
%token  TOK_PACKAGESEP
%token  TOK_PACKED
%token  TOK_PARAMETER
%token  TOK_PKG_USER_TYPE
%token  TOK_PLUS_ASSIGN
%token  TOK_POSEDGE
%token  TOK_POS_INDEXED
%token  TOK_PRIMITIVE
%token  TOK_PRIORITY
%token  TOK_PROPERTY
%token  TOK_PULL0
%token  TOK_PULL1
%token  TOK_RAND
%token  TOK_REAL
%token  TOK_REALVAL
%token  TOK_REG
%token  TOK_REPEAT
%token  TOK_RESTRICT
%token  TOK_RETURN
%token  TOK_SHORTINT
%token  TOK_SIGNED
%token  TOK_SPECIFY
%token  TOK_SPECIFY_AND
%token  TOK_SPECIFY_OPER
%token  TOK_SPECPARAM
%token  TOK_STRING
%token  TOK_STRONG0
%token  TOK_STRONG1
%token  TOK_STRUCT
%token  TOK_SUB_ASSIGN
%token  TOK_SUPPLY0
%token  TOK_SUPPLY1
%token  TOK_SVA_LABEL
%token  TOK_SYNOPSYS_FULL_CASE
%token  TOK_SYNOPSYS_PARALLEL_CASE
%token  TOK_TASK
%token  TOK_TO_SIGNED
%token  TOK_TO_UNSIGNED
%token  TOK_TYPEDEF
%token  TOK_UNBASED_UNSIZED_CONSTVAL
%token  TOK_UNION
%token  TOK_UNIQUE
%token  TOK_UNSIGNED
%token  TOK_USER_TYPE
%token  TOK_VAR
%token  TOK_WAND
%token  TOK_WEAK0
%token  TOK_WEAK1
%token  TOK_WHILE
%token  TOK_WILDCARD_CONNECT
%token  TOK_WIRE
%token  TOK_WOR
%token  TOK_XOR_ASSIGN
%token <token*token*token*token*token*token*token*token*token*token> TUPLE10
%token <token*token*token*token*token*token*token*token*token*token*token> TUPLE11
%token <token*token*token*token*token*token*token*token*token*token*token*token> TUPLE12
%token <token*token*token*token*token*token*token*token*token*token*token*token*token> TUPLE13
%token <token*token*token*token*token*token*token*token*token*token*token*token*token*token> TUPLE14
%token <token*token*token*token*token*token*token*token*token*token*token*token*token*token*token> TUPLE15
%token <token*token*token*token*token*token*token*token*token*token*token*token*token*token*token*token> TUPLE16
%token <token*token*token*token*token*token*token*token*token*token*token*token*token*token*token*token*token> TUPLE17
%token <token*token*token*token*token*token*token*token*token*token*token*token*token*token*token*token*token*token> TUPLE18
%token <token*token*token*token*token*token*token*token*token*token*token*token*token*token*token*token*token*token*token> TUPLE19
%token <token*token> TUPLE2
%token <token*token*token*token*token*token*token*token*token*token*token*token*token*token*token*token*token*token*token*token> TUPLE20
%token <token*token*token*token*token*token*token*token*token*token*token*token*token*token*token*token*token*token*token*token*token> TUPLE21
%token <token*token*token*token*token*token*token*token*token*token*token*token*token*token*token*token*token*token*token*token*token*token> TUPLE22
%token <token*token*token*token*token*token*token*token*token*token*token*token*token*token*token*token*token*token*token*token*token*token*token> TUPLE23
%token <token*token*token*token*token*token*token*token*token*token*token*token*token*token*token*token*token*token*token*token*token*token*token*token> TUPLE24
%token <token*token*token*token*token*token*token*token*token*token*token*token*token*token*token*token*token*token*token*token*token*token*token*token*token> TUPLE25
%token <token*token*token*token*token*token*token*token*token*token*token*token*token*token*token*token*token*token*token*token*token*token*token*token*token*token> TUPLE26
%token <token*token*token> TUPLE3
%token <token*token*token*token> TUPLE4
%token <token*token*token*token*token> TUPLE5
%token <token*token*token*token*token*token> TUPLE6
%token <token*token*token*token*token*token*token> TUPLE7
%token <token*token*token*token*token*token*token*token> TUPLE8
%token <token*token*token*token*token*token*token*token*token> TUPLE9
%token  UNARY_OPS
%token  UNDERSCORE
%token  VBAR
%type <token> ml_start
%start ml_start
%%


ml_start: input EOF_TOKEN { TUPLE3(STRING("ml_start0"),$1,EOF_TOKEN) }

input: /* 1 */ design { ($1) }

design: module_rule design { TUPLE3(STRING("design3"),$1,$2) }
	|	defattr design { TUPLE3(STRING("design4"),$1,$2) }
	|	task_func_decl design { TUPLE3(STRING("design5"),$1,$2) }
	|	param_decl design { TUPLE3(STRING("design6"),$1,$2) }
	|	localparam_decl design { TUPLE3(STRING("design7"),$1,$2) }
	|	typedef_decl design { TUPLE3(STRING("design8"),$1,$2) }
	|	package design { TUPLE3(STRING("design9"),$1,$2) }
	|	interface design { TUPLE3(STRING("design10"),$1,$2) }
	|	EOF_TOKEN EOF_TOKEN { TUPLE3(STRING("design11"),EOF_TOKEN,EOF_TOKEN) }

attr: /* 2 */ attr_opt { ($1) }

attr_opt: attr_opt ATTR_BEGIN opt_attr_list ATTR_END { TUPLE5(STRING("attr_opt14"),$1,ATTR_BEGIN,$3,ATTR_END) }
	|	EOF_TOKEN EOF_TOKEN { TUPLE3(STRING("attr_opt15"),EOF_TOKEN,EOF_TOKEN) }

defattr: DEFATTR_BEGIN /* 3 */ opt_attr_list /* 4 */ DEFATTR_END { TUPLE4(STRING("defattr18"),DEFATTR_BEGIN,$2,DEFATTR_END) }

opt_attr_list: attr_list { ($1) }
	|	EOF_TOKEN EOF_TOKEN { TUPLE3(STRING("opt_attr_list20"),EOF_TOKEN,EOF_TOKEN) }

attr_list: attr_assign { ($1) }
	|	attr_list COMMA attr_assign { TUPLE4(STRING("attr_list22"),$1,COMMA,$3) }

attr_assign: hierarchical_id { ($1) }
	|	hierarchical_id EQUALS expr { TUPLE4(STRING("attr_assign24"),$1,EQUALS,$3) }

hierarchical_id: TOK_ID { (TOK_ID) }
	|	hierarchical_id TOK_PACKAGESEP TOK_ID { TUPLE4(STRING("hierarchical_id26"),$1,TOK_PACKAGESEP,TOK_ID) }
	|	hierarchical_id DOT TOK_ID { TUPLE4(STRING("hierarchical_id27"),$1,DOT,TOK_ID) }

hierarchical_type_id: TOK_USER_TYPE { (TOK_USER_TYPE) }
	|	TOK_PKG_USER_TYPE { (TOK_PKG_USER_TYPE) }
	|	LPAREN TOK_USER_TYPE RPAREN { TUPLE4(STRING("hierarchical_type_id30"),LPAREN,TOK_USER_TYPE,RPAREN) }

module_rule: attr TOK_MODULE /* 5 */ TOK_ID /* 6 */ package_import_opt module_para_opt module_args_opt SEMICOLON module_body TOK_ENDMODULE opt_label { TUPLE11(STRING("module33"),$1,TOK_MODULE,TOK_ID,$4,$5,$6,SEMICOLON,$8,TOK_ENDMODULE,$10) }

package_import_opt: import_package { ($1) }
	|	EOF_TOKEN EOF_TOKEN { TUPLE3(STRING("package_import_opt35"),EOF_TOKEN,EOF_TOKEN) }

module_para_opt: HASH LPAREN /* 7 */ module_para_list /* 8 */ RPAREN { TUPLE5(STRING("module_para_opt38"),HASH,LPAREN,$3,RPAREN) }
	|	EOF_TOKEN EOF_TOKEN { TUPLE3(STRING("module_para_opt39"),EOF_TOKEN,EOF_TOKEN) }

module_para_list: single_module_para { ($1) }
	|	module_para_list COMMA single_module_para { TUPLE4(STRING("module_para_list41"),$1,COMMA,$3) }

single_module_para: EOF_TOKEN EOF_TOKEN { TUPLE3(STRING("single_module_para42"),EOF_TOKEN,EOF_TOKEN) }
	|	attr TOK_PARAMETER /* 9 */ param_type single_param_decl { TUPLE5(STRING("single_module_para44"),$1,TOK_PARAMETER,$3,$4) }
	|	attr TOK_LOCALPARAM /* 10 */ param_type single_param_decl { TUPLE5(STRING("single_module_para46"),$1,TOK_LOCALPARAM,$3,$4) }
	|	single_param_decl { ($1) }

module_args_opt: LPAREN RPAREN { TUPLE3(STRING("module_args_opt48"),LPAREN,RPAREN) }
	|	EOF_TOKEN EOF_TOKEN { TUPLE3(STRING("module_args_opt49"),EOF_TOKEN,EOF_TOKEN) }
	|	LPAREN module_args optional_comma RPAREN { TUPLE5(STRING("module_args_opt50"),LPAREN,$2,$3,RPAREN) }

module_args: module_arg { ($1) }
	|	module_args COMMA module_arg { TUPLE4(STRING("module_args52"),$1,COMMA,$3) }

optional_comma: COMMA { (COMMA) }
	|	EOF_TOKEN EOF_TOKEN { TUPLE3(STRING("optional_comma54"),EOF_TOKEN,EOF_TOKEN) }

module_arg_opt_assignment: EQUALS expr { TUPLE3(STRING("module_arg_opt_assignment55"),EQUALS,$2) }
	|	EOF_TOKEN EOF_TOKEN { TUPLE3(STRING("module_arg_opt_assignment56"),EOF_TOKEN,EOF_TOKEN) }

module_arg: TOK_ID /* 11 */ module_arg_opt_assignment { TUPLE3(STRING("module_arg58"),TOK_ID,$2) }
	|	TOK_ID /* 12 */ TOK_ID /* 13 */ module_arg_opt_assignment { TUPLE4(STRING("module_arg61"),TOK_ID,TOK_ID,$3) }
	|	attr wire_type range TOK_ID range /* 14 */ module_arg_opt_assignment { TUPLE7(STRING("module_arg63"),$1,$2,$3,TOK_ID,$5,$6) }
	|	DOT DOT DOT { TUPLE4(STRING("module_arg64"),DOT,DOT,DOT) }

package: attr TOK_PACKAGE /* 15 */ TOK_ID /* 16 */ SEMICOLON package_body TOK_ENDPACKAGE opt_label { TUPLE8(STRING("package67"),$1,TOK_PACKAGE,TOK_ID,SEMICOLON,$5,TOK_ENDPACKAGE,$7) }

package_body: package_body package_body_stmt { TUPLE3(STRING("package_body68"),$1,$2) }
	|	EOF_TOKEN EOF_TOKEN { TUPLE3(STRING("package_body69"),EOF_TOKEN,EOF_TOKEN) }

package_body_stmt: typedef_decl { ($1) }
	|	localparam_decl { ($1) }
	|	param_decl { ($1) }
	|	task_func_decl { ($1) }

interface: TOK_INTERFACE /* 17 */ TOK_ID /* 18 */ module_para_opt module_args_opt SEMICOLON interface_body TOK_ENDINTERFACE { TUPLE8(STRING("interface76"),TOK_INTERFACE,TOK_ID,$3,$4,SEMICOLON,$6,TOK_ENDINTERFACE) }

interface_body: interface_body interface_body_stmt { TUPLE3(STRING("interface_body77"),$1,$2) }
	|	EOF_TOKEN EOF_TOKEN { TUPLE3(STRING("interface_body78"),EOF_TOKEN,EOF_TOKEN) }

interface_body_stmt: param_decl { ($1) }
	|	localparam_decl { ($1) }
	|	typedef_decl { ($1) }
	|	defparam_decl { ($1) }
	|	wire_decl { ($1) }
	|	always_stmt { ($1) }
	|	assign_stmt { ($1) }
	|	modport_stmt { ($1) }

non_opt_delay: HASH TOK_ID { TUPLE3(STRING("non_opt_delay87"),HASH,TOK_ID) }
	|	HASH TOK_CONSTVAL { TUPLE3(STRING("non_opt_delay88"),HASH,TOK_CONSTVAL) }
	|	HASH TOK_REALVAL { TUPLE3(STRING("non_opt_delay89"),HASH,TOK_REALVAL) }
	|	HASH LPAREN expr RPAREN { TUPLE5(STRING("non_opt_delay90"),HASH,LPAREN,$3,RPAREN) }
	|	HASH LPAREN expr COLON expr COLON expr RPAREN { TUPLE9(STRING("non_opt_delay91"),HASH,LPAREN,$3,COLON,$5,COLON,$7,RPAREN) }

delay: non_opt_delay { ($1) }
	|	EOF_TOKEN EOF_TOKEN { TUPLE3(STRING("delay93"),EOF_TOKEN,EOF_TOKEN) }

wire_type: /* 19 */ wire_type_token_list { ($1) }

wire_type_token_list: wire_type_token { ($1) }
	|	wire_type_token_list wire_type_token { TUPLE3(STRING("wire_type_token_list97"),$1,$2) }
	|	wire_type_token_io { ($1) }
	|	wire_type_token_list hierarchical_type_id { TUPLE3(STRING("wire_type_token_list99"),$1,$2) }
	|	hierarchical_type_id { ($1) }

wire_type_token_io: TOK_INPUT { (TOK_INPUT) }
	|	TOK_OUTPUT { (TOK_OUTPUT) }
	|	TOK_INOUT { (TOK_INOUT) }

wire_type_token: TOK_WIRE { (TOK_WIRE) }
	|	TOK_WOR { (TOK_WOR) }
	|	TOK_WAND { (TOK_WAND) }
	|	TOK_REG { (TOK_REG) }
	|	TOK_LOGIC { (TOK_LOGIC) }
	|	TOK_VAR { (TOK_VAR) }
	|	TOK_INTEGER { (TOK_INTEGER) }
	|	TOK_GENVAR { (TOK_GENVAR) }
	|	TOK_SIGNED { (TOK_SIGNED) }
	|	TOK_RAND { (TOK_RAND) }
	|	TOK_CONST { (TOK_CONST) }

non_opt_range: LBRACK expr COLON expr RBRACK { TUPLE6(STRING("non_opt_range115"),LBRACK,$2,COLON,$4,RBRACK) }
	|	LBRACK expr TOK_POS_INDEXED expr RBRACK { TUPLE6(STRING("non_opt_range116"),LBRACK,$2,TOK_POS_INDEXED,$4,RBRACK) }
	|	LBRACK expr TOK_NEG_INDEXED expr RBRACK { TUPLE6(STRING("non_opt_range117"),LBRACK,$2,TOK_NEG_INDEXED,$4,RBRACK) }
	|	LBRACK expr RBRACK { TUPLE4(STRING("non_opt_range118"),LBRACK,$2,RBRACK) }

non_opt_multirange: non_opt_range non_opt_range { TUPLE3(STRING("non_opt_multirange119"),$1,$2) }
	|	non_opt_multirange non_opt_range { TUPLE3(STRING("non_opt_multirange120"),$1,$2) }

range: non_opt_range { ($1) }
	|	EOF_TOKEN EOF_TOKEN { TUPLE3(STRING("range122"),EOF_TOKEN,EOF_TOKEN) }

range_or_multirange: range { ($1) }
	|	non_opt_multirange { ($1) }

range_or_signed_int: range { ($1) }
	|	TOK_INTEGER { (TOK_INTEGER) }
	|	TOK_LOGIC range { TUPLE3(STRING("range_or_signed_int127"),TOK_LOGIC,$2) }

module_body: module_body module_body_stmt { TUPLE3(STRING("module_body128"),$1,$2) }
	|	module_body gen_stmt { TUPLE3(STRING("module_body129"),$1,$2) }
	|	EOF_TOKEN EOF_TOKEN { TUPLE3(STRING("module_body130"),EOF_TOKEN,EOF_TOKEN) }

module_body_stmt: task_func_decl { ($1) }
	|	specify_block { ($1) }
	|	param_decl { ($1) }
	|	localparam_decl { ($1) }
	|	typedef_decl { ($1) }
	|	defparam_decl { ($1) }
	|	specparam_declaration { ($1) }
	|	wire_decl { ($1) }
	|	assign_stmt { ($1) }
	|	cell_stmt { ($1) }
	|	enum_decl { ($1) }
	|	struct_decl { ($1) }
	|	always_stmt { ($1) }
	|	TOK_GENERATE module_gen_body TOK_ENDGENERATE { TUPLE4(STRING("module_body_stmt144"),TOK_GENERATE,$2,TOK_ENDGENERATE) }
	|	defattr { ($1) }
	|	assert_property { ($1) }
	|	checker_decl { ($1) }
	|	ignored_specify_block { ($1) }
	|	import_package { ($1) }
	|	SEMICOLON { (SEMICOLON) }

import_package: TOK_IMPORT_PACKAGE { (TOK_IMPORT_PACKAGE) }

checker_decl: TOK_CHECKER TOK_ID SEMICOLON /* 20 */ module_body TOK_ENDCHECKER { TUPLE6(STRING("checker_decl153"),TOK_CHECKER,TOK_ID,SEMICOLON,$4,TOK_ENDCHECKER) }

task_func_decl: attr TOK_DPI_FUNCTION TOK_ID TOK_ID /* 21 */ opt_dpi_function_args SEMICOLON { TUPLE7(STRING("task_func_decl155"),$1,TOK_DPI_FUNCTION,TOK_ID,TOK_ID,$5,SEMICOLON) }
	|	attr TOK_DPI_FUNCTION TOK_ID EQUALS TOK_ID TOK_ID /* 22 */ opt_dpi_function_args SEMICOLON { TUPLE9(STRING("task_func_decl157"),$1,TOK_DPI_FUNCTION,TOK_ID,EQUALS,TOK_ID,TOK_ID,$7,SEMICOLON) }
	|	attr TOK_DPI_FUNCTION TOK_ID COLON TOK_ID EQUALS TOK_ID TOK_ID /* 23 */ opt_dpi_function_args SEMICOLON { TUPLE11(STRING("task_func_decl159"),$1,TOK_DPI_FUNCTION,TOK_ID,COLON,TOK_ID,EQUALS,TOK_ID,TOK_ID,$9,SEMICOLON) }
	|	attr TOK_TASK opt_automatic TOK_ID /* 24 */ task_func_args_opt SEMICOLON task_func_body TOK_ENDTASK { TUPLE9(STRING("task_func_decl161"),$1,TOK_TASK,$3,TOK_ID,$5,SEMICOLON,$7,TOK_ENDTASK) }
	|	attr TOK_FUNCTION opt_automatic opt_signed range_or_signed_int TOK_ID /* 25 */ task_func_args_opt SEMICOLON task_func_body TOK_ENDFUNCTION opt_label { TUPLE12(STRING("task_func_decl163"),$1,TOK_FUNCTION,$3,$4,$5,TOK_ID,$7,SEMICOLON,$9,TOK_ENDFUNCTION,$11) }

dpi_function_arg: TOK_ID TOK_ID { TUPLE3(STRING("dpi_function_arg164"),TOK_ID,TOK_ID) }
	|	TOK_ID { (TOK_ID) }

opt_dpi_function_args: LPAREN dpi_function_args RPAREN { TUPLE4(STRING("opt_dpi_function_args166"),LPAREN,$2,RPAREN) }
	|	EOF_TOKEN EOF_TOKEN { TUPLE3(STRING("opt_dpi_function_args167"),EOF_TOKEN,EOF_TOKEN) }

dpi_function_args: dpi_function_args COMMA dpi_function_arg { TUPLE4(STRING("dpi_function_args168"),$1,COMMA,$3) }
	|	dpi_function_args COMMA { TUPLE3(STRING("dpi_function_args169"),$1,COMMA) }
	|	dpi_function_arg { ($1) }
	|	EOF_TOKEN EOF_TOKEN { TUPLE3(STRING("dpi_function_args171"),EOF_TOKEN,EOF_TOKEN) }

opt_automatic: TOK_AUTOMATIC { (TOK_AUTOMATIC) }
	|	EOF_TOKEN EOF_TOKEN { TUPLE3(STRING("opt_automatic173"),EOF_TOKEN,EOF_TOKEN) }

opt_signed: TOK_SIGNED { (TOK_SIGNED) }
	|	EOF_TOKEN EOF_TOKEN { TUPLE3(STRING("opt_signed175"),EOF_TOKEN,EOF_TOKEN) }

task_func_args_opt: LPAREN RPAREN { TUPLE3(STRING("task_func_args_opt176"),LPAREN,RPAREN) }
	|	EOF_TOKEN EOF_TOKEN { TUPLE3(STRING("task_func_args_opt177"),EOF_TOKEN,EOF_TOKEN) }
	|	LPAREN /* 26 */ task_func_args optional_comma /* 27 */ RPAREN { TUPLE5(STRING("task_func_args_opt180"),LPAREN,$2,$3,RPAREN) }

task_func_args: task_func_port { ($1) }
	|	task_func_args COMMA task_func_port { TUPLE4(STRING("task_func_args182"),$1,COMMA,$3) }

task_func_port: attr wire_type range_or_multirange /* 28 */ wire_name { TUPLE5(STRING("task_func_port184"),$1,$2,$3,$4) }
	|	/* 29 */ wire_name { ($1) }

task_func_body: task_func_body behavioral_stmt { TUPLE3(STRING("task_func_body187"),$1,$2) }
	|	EOF_TOKEN EOF_TOKEN { TUPLE3(STRING("task_func_body188"),EOF_TOKEN,EOF_TOKEN) }

specify_block: TOK_SPECIFY specify_item_list TOK_ENDSPECIFY { TUPLE4(STRING("specify_block189"),TOK_SPECIFY,$2,TOK_ENDSPECIFY) }

specify_item_list: specify_item specify_item_list { TUPLE3(STRING("specify_item_list190"),$1,$2) }
	|	EOF_TOKEN EOF_TOKEN { TUPLE3(STRING("specify_item_list191"),EOF_TOKEN,EOF_TOKEN) }

specify_item: specify_if LPAREN specify_edge expr TOK_SPECIFY_OPER specify_target RPAREN EQUALS specify_rise_fall SEMICOLON { TUPLE11(STRING("specify_item192"),$1,LPAREN,$3,$4,TOK_SPECIFY_OPER,$6,RPAREN,EQUALS,$9,SEMICOLON) }
	|	TOK_ID LPAREN specify_edge expr specify_condition COMMA specify_edge expr specify_condition COMMA specify_triple specify_opt_triple RPAREN SEMICOLON { TUPLE15(STRING("specify_item193"),TOK_ID,LPAREN,$3,$4,$5,COMMA,$7,$8,$9,COMMA,$11,$12,RPAREN,SEMICOLON) }

specify_opt_triple: COMMA specify_triple { TUPLE3(STRING("specify_opt_triple194"),COMMA,$2) }
	|	EOF_TOKEN EOF_TOKEN { TUPLE3(STRING("specify_opt_triple195"),EOF_TOKEN,EOF_TOKEN) }

specify_if: TOK_IF LPAREN expr RPAREN { TUPLE5(STRING("specify_if196"),TOK_IF,LPAREN,$3,RPAREN) }
	|	EOF_TOKEN EOF_TOKEN { TUPLE3(STRING("specify_if197"),EOF_TOKEN,EOF_TOKEN) }

specify_condition: TOK_SPECIFY_AND expr { TUPLE3(STRING("specify_condition198"),TOK_SPECIFY_AND,$2) }
	|	EOF_TOKEN EOF_TOKEN { TUPLE3(STRING("specify_condition199"),EOF_TOKEN,EOF_TOKEN) }

specify_target: expr { ($1) }
	|	LPAREN expr COLON expr RPAREN { TUPLE6(STRING("specify_target201"),LPAREN,$2,COLON,$4,RPAREN) }
	|	LPAREN expr TOK_NEG_INDEXED expr RPAREN { TUPLE6(STRING("specify_target202"),LPAREN,$2,TOK_NEG_INDEXED,$4,RPAREN) }
	|	LPAREN expr TOK_POS_INDEXED expr RPAREN { TUPLE6(STRING("specify_target203"),LPAREN,$2,TOK_POS_INDEXED,$4,RPAREN) }

specify_edge: TOK_POSEDGE { (TOK_POSEDGE) }
	|	TOK_NEGEDGE { (TOK_NEGEDGE) }
	|	EOF_TOKEN EOF_TOKEN { TUPLE3(STRING("specify_edge206"),EOF_TOKEN,EOF_TOKEN) }

specify_rise_fall: specify_triple { ($1) }
	|	LPAREN specify_triple COMMA specify_triple RPAREN { TUPLE6(STRING("specify_rise_fall208"),LPAREN,$2,COMMA,$4,RPAREN) }
	|	LPAREN specify_triple COMMA specify_triple COMMA specify_triple RPAREN { TUPLE8(STRING("specify_rise_fall209"),LPAREN,$2,COMMA,$4,COMMA,$6,RPAREN) }
	|	LPAREN specify_triple COMMA specify_triple COMMA specify_triple COMMA specify_triple COMMA specify_triple COMMA specify_triple RPAREN { TUPLE14(STRING("specify_rise_fall210"),LPAREN,$2,COMMA,$4,COMMA,$6,COMMA,$8,COMMA,$10,COMMA,$12,RPAREN) }
	|	LPAREN specify_triple COMMA specify_triple COMMA specify_triple COMMA specify_triple COMMA specify_triple COMMA specify_triple COMMA specify_triple COMMA specify_triple COMMA specify_triple COMMA specify_triple COMMA specify_triple COMMA specify_triple RPAREN { TUPLE26(STRING("specify_rise_fall211"),LPAREN,$2,COMMA,$4,COMMA,$6,COMMA,$8,COMMA,$10,COMMA,$12,COMMA,$14,COMMA,$16,COMMA,$18,COMMA,$20,COMMA,$22,COMMA,$24,RPAREN) }

specify_triple: expr { ($1) }
	|	expr COLON expr COLON expr { TUPLE6(STRING("specify_triple213"),$1,COLON,$3,COLON,$5) }

ignored_specify_block: TOK_IGNORED_SPECIFY ignored_specify_item_opt TOK_ENDSPECIFY { TUPLE4(STRING("ignored_specify_block214"),TOK_IGNORED_SPECIFY,$2,TOK_ENDSPECIFY) }
	|	TOK_IGNORED_SPECIFY TOK_ENDSPECIFY { TUPLE3(STRING("ignored_specify_block215"),TOK_IGNORED_SPECIFY,TOK_ENDSPECIFY) }

ignored_specify_item_opt: ignored_specify_item_opt ignored_specify_item { TUPLE3(STRING("ignored_specify_item_opt216"),$1,$2) }
	|	ignored_specify_item { ($1) }

ignored_specify_item: specparam_declaration { ($1) }
	|	path_declaration { ($1) }
	|	system_timing_declaration { ($1) }

specparam_declaration: TOK_SPECPARAM list_of_specparam_assignments SEMICOLON { TUPLE4(STRING("specparam_declaration221"),TOK_SPECPARAM,$2,SEMICOLON) }
	|	TOK_SPECPARAM specparam_range list_of_specparam_assignments SEMICOLON { TUPLE5(STRING("specparam_declaration222"),TOK_SPECPARAM,$2,$3,SEMICOLON) }

specparam_range: LBRACK ignspec_constant_expression COLON ignspec_constant_expression RBRACK { TUPLE6(STRING("specparam_range223"),LBRACK,$2,COLON,$4,RBRACK) }

list_of_specparam_assignments: specparam_assignment { CONS1 ($1) }
	|	list_of_specparam_assignments COMMA specparam_assignment { CONS3($1,COMMA,$3) }

specparam_assignment: ignspec_id EQUALS ignspec_expr { TUPLE4(STRING("specparam_assignment226"),$1,EQUALS,$3) }

ignspec_opt_cond: TOK_IF LPAREN ignspec_expr RPAREN { TUPLE5(STRING("ignspec_opt_cond227"),TOK_IF,LPAREN,$3,RPAREN) }
	|	EOF_TOKEN EOF_TOKEN { TUPLE3(STRING("ignspec_opt_cond228"),EOF_TOKEN,EOF_TOKEN) }

path_declaration: simple_path_declaration SEMICOLON { TUPLE3(STRING("path_declaration229"),$1,SEMICOLON) }

simple_path_declaration: ignspec_opt_cond parallel_path_description EQUALS path_delay_value { TUPLE5(STRING("simple_path_declaration230"),$1,$2,EQUALS,$4) }
	|	ignspec_opt_cond full_path_description EQUALS path_delay_value { TUPLE5(STRING("simple_path_declaration231"),$1,$2,EQUALS,$4) }

path_delay_value: LPAREN ignspec_expr list_of_path_delay_extra_expressions RPAREN { TUPLE5(STRING("path_delay_value232"),LPAREN,$2,$3,RPAREN) }
	|	ignspec_expr { ($1) }
	|	ignspec_expr list_of_path_delay_extra_expressions { TUPLE3(STRING("path_delay_value234"),$1,$2) }

list_of_path_delay_extra_expressions: COMMA ignspec_expr { CONS2(COMMA,$2) }
	|	COMMA ignspec_expr list_of_path_delay_extra_expressions { CONS3(COMMA,$2,$3) }

specify_edge_identifier: TOK_POSEDGE { (TOK_POSEDGE) }
	|	TOK_NEGEDGE { (TOK_NEGEDGE) }

parallel_path_description: LPAREN specify_input_terminal_descriptor opt_polarity_operator EQUALS GREATER specify_output_terminal_descriptor RPAREN { TUPLE8(STRING("parallel_path_description239"),LPAREN,$2,$3,EQUALS,GREATER,$6,RPAREN) }
	|	LPAREN specify_edge_identifier specify_input_terminal_descriptor EQUALS GREATER LPAREN specify_output_terminal_descriptor opt_polarity_operator COLON ignspec_expr RPAREN RPAREN { TUPLE13(STRING("parallel_path_description240"),LPAREN,$2,$3,EQUALS,GREATER,LPAREN,$7,$8,COLON,$10,RPAREN,RPAREN) }
	|	LPAREN specify_edge_identifier specify_input_terminal_descriptor EQUALS GREATER LPAREN specify_output_terminal_descriptor TOK_POS_INDEXED ignspec_expr RPAREN RPAREN { TUPLE12(STRING("parallel_path_description241"),LPAREN,$2,$3,EQUALS,GREATER,LPAREN,$7,TOK_POS_INDEXED,$9,RPAREN,RPAREN) }

full_path_description: LPAREN list_of_path_inputs STAR GREATER list_of_path_outputs RPAREN { TUPLE7(STRING("full_path_description242"),LPAREN,$2,STAR,GREATER,$5,RPAREN) }
	|	LPAREN specify_edge_identifier list_of_path_inputs STAR GREATER LPAREN list_of_path_outputs opt_polarity_operator COLON ignspec_expr RPAREN RPAREN { TUPLE13(STRING("full_path_description243"),LPAREN,$2,$3,STAR,GREATER,LPAREN,$7,$8,COLON,$10,RPAREN,RPAREN) }
	|	LPAREN specify_edge_identifier list_of_path_inputs STAR GREATER LPAREN list_of_path_outputs TOK_POS_INDEXED ignspec_expr RPAREN RPAREN { TUPLE12(STRING("full_path_description244"),LPAREN,$2,$3,STAR,GREATER,LPAREN,$7,TOK_POS_INDEXED,$9,RPAREN,RPAREN) }

list_of_path_inputs: specify_input_terminal_descriptor opt_polarity_operator { CONS2($1,$2) }
	|	specify_input_terminal_descriptor more_path_inputs opt_polarity_operator { CONS3($1,$2,$3) }

more_path_inputs: COMMA specify_input_terminal_descriptor { TUPLE3(STRING("more_path_inputs247"),COMMA,$2) }
	|	more_path_inputs COMMA specify_input_terminal_descriptor { TUPLE4(STRING("more_path_inputs248"),$1,COMMA,$3) }

list_of_path_outputs: specify_output_terminal_descriptor { CONS1 ($1) }
	|	list_of_path_outputs COMMA specify_output_terminal_descriptor { CONS3($1,COMMA,$3) }

opt_polarity_operator: PLUS { (PLUS) }
	|	HYPHEN { (HYPHEN) }
	|	EOF_TOKEN EOF_TOKEN { TUPLE3(STRING("opt_polarity_operator253"),EOF_TOKEN,EOF_TOKEN) }

specify_input_terminal_descriptor: ignspec_id { ($1) }

specify_output_terminal_descriptor: ignspec_id { ($1) }

system_timing_declaration: ignspec_id LPAREN system_timing_args RPAREN SEMICOLON { TUPLE6(STRING("system_timing_declaration256"),$1,LPAREN,$3,RPAREN,SEMICOLON) }

system_timing_arg: TOK_POSEDGE ignspec_id { TUPLE3(STRING("system_timing_arg257"),TOK_POSEDGE,$2) }
	|	TOK_NEGEDGE ignspec_id { TUPLE3(STRING("system_timing_arg258"),TOK_NEGEDGE,$2) }
	|	ignspec_expr { ($1) }

system_timing_args: system_timing_arg { ($1) }
	|	system_timing_args TOK_IGNORED_SPECIFY_AND system_timing_arg { TUPLE4(STRING("system_timing_args261"),$1,TOK_IGNORED_SPECIFY_AND,$3) }
	|	system_timing_args COMMA system_timing_arg { TUPLE4(STRING("system_timing_args262"),$1,COMMA,$3) }

ignspec_constant_expression: expr { ($1) }

ignspec_expr: expr { ($1) }
	|	expr COLON expr COLON expr { TUPLE6(STRING("ignspec_expr265"),$1,COLON,$3,COLON,$5) }

ignspec_id: TOK_ID /* 30 */ range_or_multirange { TUPLE3(STRING("ignspec_id267"),TOK_ID,$2) }

param_signed: TOK_SIGNED { (TOK_SIGNED) }
	|	TOK_UNSIGNED { (TOK_UNSIGNED) }
	|	EOF_TOKEN EOF_TOKEN { TUPLE3(STRING("param_signed270"),EOF_TOKEN,EOF_TOKEN) }

param_integer: TOK_INTEGER { (TOK_INTEGER) }
	|	TOK_INT { (TOK_INT) }

param_real: TOK_REAL { (TOK_REAL) }

param_logic: TOK_LOGIC { (TOK_LOGIC) }

param_reg: TOK_REG { (TOK_REG) }

param_logic_or_reg: param_logic { ($1) }
	|	param_reg { ($1) }

param_range: range { ($1) }

param_logic_range: range_or_multirange { ($1) }

param_integer_type: param_integer param_signed { TUPLE3(STRING("param_integer_type280"),$1,$2) }

param_integer_vector_type: param_logic_or_reg param_signed param_logic_range { TUPLE4(STRING("param_integer_vector_type281"),$1,$2,$3) }

param_range_type: param_signed param_range { TUPLE3(STRING("param_range_type282"),$1,$2) }

param_type: param_integer_type { ($1) }
	|	param_integer_vector_type { ($1) }
	|	param_real { ($1) }
	|	param_range_type { ($1) }
	|	hierarchical_type_id { ($1) }

param_decl: attr TOK_PARAMETER /* 31 */ param_type param_decl_list SEMICOLON { TUPLE6(STRING("param_decl289"),$1,TOK_PARAMETER,$3,$4,SEMICOLON) }

localparam_decl: attr TOK_LOCALPARAM /* 32 */ param_type param_decl_list SEMICOLON { TUPLE6(STRING("localparam_decl291"),$1,TOK_LOCALPARAM,$3,$4,SEMICOLON) }

param_decl_list: single_param_decl { ($1) }
	|	param_decl_list COMMA single_param_decl { TUPLE4(STRING("param_decl_list293"),$1,COMMA,$3) }

single_param_decl: TOK_ID range EQUALS expr { TUPLE5(STRING("single_param_decl294"),TOK_ID,$2,EQUALS,$4) }

defparam_decl: TOK_DEFPARAM defparam_decl_list SEMICOLON { TUPLE4(STRING("defparam_decl295"),TOK_DEFPARAM,$2,SEMICOLON) }

defparam_decl_list: single_defparam_decl { ($1) }
	|	defparam_decl_list COMMA single_defparam_decl { TUPLE4(STRING("defparam_decl_list297"),$1,COMMA,$3) }

single_defparam_decl: range rvalue EQUALS expr { TUPLE5(STRING("single_defparam_decl298"),$1,$2,EQUALS,$4) }

enum_type: TOK_ENUM /* 33 */ enum_base_type LBRACE enum_name_list RBRACE { TUPLE6(STRING("enum_type300"),TOK_ENUM,$2,LBRACE,$4,RBRACE) }

enum_base_type: type_atom type_signing { TUPLE3(STRING("enum_base_type301"),$1,$2) }
	|	type_vec type_signing range { TUPLE4(STRING("enum_base_type302"),$1,$2,$3) }
	|	EOF_TOKEN EOF_TOKEN { TUPLE3(STRING("enum_base_type303"),EOF_TOKEN,EOF_TOKEN) }

type_atom: TOK_INTEGER { (TOK_INTEGER) }
	|	TOK_INT { (TOK_INT) }
	|	TOK_SHORTINT { (TOK_SHORTINT) }
	|	TOK_BYTE { (TOK_BYTE) }

type_vec: TOK_REG { (TOK_REG) }
	|	TOK_LOGIC { (TOK_LOGIC) }

type_signing: TOK_SIGNED { (TOK_SIGNED) }
	|	TOK_UNSIGNED { (TOK_UNSIGNED) }
	|	EOF_TOKEN EOF_TOKEN { TUPLE3(STRING("type_signing312"),EOF_TOKEN,EOF_TOKEN) }

enum_name_list: enum_name_decl { ($1) }
	|	enum_name_list COMMA enum_name_decl { TUPLE4(STRING("enum_name_list314"),$1,COMMA,$3) }

enum_name_decl: TOK_ID opt_enum_init { TUPLE3(STRING("enum_name_decl315"),TOK_ID,$2) }

opt_enum_init: EQUALS basic_expr { TUPLE3(STRING("opt_enum_init316"),EQUALS,$2) }
	|	EOF_TOKEN EOF_TOKEN { TUPLE3(STRING("opt_enum_init317"),EOF_TOKEN,EOF_TOKEN) }

enum_var_list: enum_var { ($1) }
	|	enum_var_list COMMA enum_var { TUPLE4(STRING("enum_var_list319"),$1,COMMA,$3) }

enum_var: TOK_ID { (TOK_ID) }

enum_decl: enum_type enum_var_list SEMICOLON { TUPLE4(STRING("enum_decl321"),$1,$2,SEMICOLON) }

struct_decl: struct_type struct_var_list SEMICOLON { TUPLE4(STRING("struct_decl322"),$1,$2,SEMICOLON) }

struct_type: struct_union /* 34 */ struct_body { TUPLE3(STRING("struct_type324"),$1,$2) }

struct_union: TOK_STRUCT { (TOK_STRUCT) }
	|	TOK_UNION { (TOK_UNION) }

struct_body: opt_packed LBRACE struct_member_list RBRACE { TUPLE5(STRING("struct_body327"),$1,LBRACE,$3,RBRACE) }

opt_packed: TOK_PACKED opt_signed_struct { TUPLE3(STRING("opt_packed328"),TOK_PACKED,$2) }
	|	EOF_TOKEN EOF_TOKEN { TUPLE3(STRING("opt_packed329"),EOF_TOKEN,EOF_TOKEN) }

opt_signed_struct: TOK_SIGNED { (TOK_SIGNED) }
	|	TOK_UNSIGNED { (TOK_UNSIGNED) }
	|	EOF_TOKEN EOF_TOKEN { TUPLE3(STRING("opt_signed_struct332"),EOF_TOKEN,EOF_TOKEN) }

struct_member_list: struct_member { ($1) }
	|	struct_member_list struct_member { TUPLE3(STRING("struct_member_list334"),$1,$2) }

struct_member: struct_member_type member_name_list SEMICOLON { TUPLE4(STRING("struct_member335"),$1,$2,SEMICOLON) }

member_name_list: member_name { ($1) }
	|	member_name_list COMMA member_name { TUPLE4(STRING("member_name_list337"),$1,COMMA,$3) }

member_name: TOK_ID /* 35 */ range { TUPLE3(STRING("member_name339"),TOK_ID,$2) }

struct_member_type: /* 36 */ member_type_token { ($1) }

member_type_token: member_type { ($1) }
	|	hierarchical_type_id { ($1) }
	|	struct_union /* 37 */ struct_body { TUPLE3(STRING("member_type_token345"),$1,$2) }

member_type: type_atom type_signing { TUPLE3(STRING("member_type346"),$1,$2) }
	|	type_vec type_signing range_or_multirange { TUPLE4(STRING("member_type347"),$1,$2,$3) }

struct_var_list: struct_var { ($1) }
	|	struct_var_list COMMA struct_var { TUPLE4(STRING("struct_var_list349"),$1,COMMA,$3) }

struct_var: TOK_ID { (TOK_ID) }

wire_decl: attr wire_type range_or_multirange /* 38 */ delay wire_name_list /* 39 */ SEMICOLON { TUPLE7(STRING("wire_decl353"),$1,$2,$3,$4,$5,SEMICOLON) }
	|	attr TOK_SUPPLY0 TOK_ID /* 40 */ opt_supply_wires SEMICOLON { TUPLE6(STRING("wire_decl355"),$1,TOK_SUPPLY0,TOK_ID,$4,SEMICOLON) }
	|	attr TOK_SUPPLY1 TOK_ID /* 41 */ opt_supply_wires SEMICOLON { TUPLE6(STRING("wire_decl357"),$1,TOK_SUPPLY1,TOK_ID,$4,SEMICOLON) }

opt_supply_wires: EOF_TOKEN EOF_TOKEN { TUPLE3(STRING("opt_supply_wires358"),EOF_TOKEN,EOF_TOKEN) }
	|	opt_supply_wires COMMA TOK_ID { TUPLE4(STRING("opt_supply_wires359"),$1,COMMA,TOK_ID) }

wire_name_list: wire_name_and_opt_assign { ($1) }
	|	wire_name_list COMMA wire_name_and_opt_assign { TUPLE4(STRING("wire_name_list361"),$1,COMMA,$3) }

wire_name_and_opt_assign: wire_name { ($1) }
	|	wire_name EQUALS expr { TUPLE4(STRING("wire_name_and_opt_assign363"),$1,EQUALS,$3) }

wire_name: TOK_ID range_or_multirange { TUPLE3(STRING("wire_name364"),TOK_ID,$2) }

strength_types: TOK_STRONG0 { (TOK_STRONG0) }
	|	TOK_STRONG1 { (TOK_STRONG1) }
	|	TOK_PULL0 { (TOK_PULL0) }
	|	TOK_PULL1 { (TOK_PULL1) }
	|	TOK_HIGHZ0 { (TOK_HIGHZ0) }
	|	TOK_HIGHZ1 { (TOK_HIGHZ1) }
	|	TOK_WEAK0 { (TOK_WEAK0) }
	|	TOK_WEAK1 { (TOK_WEAK1) }

non_opt_strength: LPAREN strength_types COMMA strength_types RPAREN { TUPLE6(STRING("non_opt_strength373"),LPAREN,$2,COMMA,$4,RPAREN) }

strength: non_opt_strength { ($1) }
	|	EOF_TOKEN EOF_TOKEN { TUPLE3(STRING("strength375"),EOF_TOKEN,EOF_TOKEN) }

assign_stmt: TOK_ASSIGN strength delay assign_expr_list SEMICOLON { TUPLE6(STRING("assign_stmt376"),TOK_ASSIGN,$2,$3,$4,SEMICOLON) }

assign_expr_list: assign_expr { ($1) }
	|	assign_expr_list COMMA assign_expr { TUPLE4(STRING("assign_expr_list378"),$1,COMMA,$3) }

assign_expr: lvalue EQUALS expr { TUPLE4(STRING("assign_expr379"),$1,EQUALS,$3) }

type_name: TOK_ID { (TOK_ID) }
	|	TOK_USER_TYPE { (TOK_USER_TYPE) }

typedef_decl: TOK_TYPEDEF wire_type range type_name range_or_multirange SEMICOLON { TUPLE7(STRING("typedef_decl382"),TOK_TYPEDEF,$2,$3,$4,$5,SEMICOLON) }
	|	TOK_TYPEDEF non_wire_data_type type_name SEMICOLON { TUPLE5(STRING("typedef_decl383"),TOK_TYPEDEF,$2,$3,SEMICOLON) }

non_wire_data_type: enum_type { ($1) }
	|	struct_type { ($1) }

cell_stmt: attr TOK_ID /* 42 */ cell_parameter_list_opt cell_list SEMICOLON { TUPLE6(STRING("cell_stmt387"),$1,TOK_ID,$3,$4,SEMICOLON) }
	|	attr tok_prim_wrapper delay /* 43 */ prim_list SEMICOLON { TUPLE6(STRING("cell_stmt389"),$1,$2,$3,$4,SEMICOLON) }

tok_prim_wrapper: TOK_PRIMITIVE { (TOK_PRIMITIVE) }
	|	TOK_OR { (TOK_OR) }

cell_list: single_cell { ($1) }
	|	cell_list COMMA single_cell { TUPLE4(STRING("cell_list393"),$1,COMMA,$3) }

single_cell: TOK_ID /* 44 */ LPAREN cell_port_list RPAREN { TUPLE5(STRING("single_cell395"),TOK_ID,LPAREN,$3,RPAREN) }
	|	TOK_ID non_opt_range /* 45 */ LPAREN cell_port_list RPAREN { TUPLE6(STRING("single_cell397"),TOK_ID,$2,LPAREN,$4,RPAREN) }

prim_list: single_prim { ($1) }
	|	prim_list COMMA single_prim { TUPLE4(STRING("prim_list399"),$1,COMMA,$3) }

single_prim: single_cell { ($1) }
	|	/* 46 */ LPAREN cell_port_list RPAREN { TUPLE4(STRING("single_prim402"),LPAREN,$2,RPAREN) }

cell_parameter_list_opt: HASH LPAREN cell_parameter_list RPAREN { TUPLE5(STRING("cell_parameter_list_opt403"),HASH,LPAREN,$3,RPAREN) }
	|	EOF_TOKEN EOF_TOKEN { TUPLE3(STRING("cell_parameter_list_opt404"),EOF_TOKEN,EOF_TOKEN) }

cell_parameter_list: cell_parameter { ($1) }
	|	cell_parameter_list COMMA cell_parameter { TUPLE4(STRING("cell_parameter_list406"),$1,COMMA,$3) }

cell_parameter: EOF_TOKEN EOF_TOKEN { TUPLE3(STRING("cell_parameter407"),EOF_TOKEN,EOF_TOKEN) }
	|	expr { ($1) }
	|	DOT TOK_ID LPAREN expr RPAREN { TUPLE6(STRING("cell_parameter409"),DOT,TOK_ID,LPAREN,$4,RPAREN) }

cell_port_list: cell_port_list_rules { ($1) }

cell_port_list_rules: cell_port { ($1) }
	|	cell_port_list_rules COMMA cell_port { TUPLE4(STRING("cell_port_list_rules412"),$1,COMMA,$3) }

cell_port: attr { ($1) }
	|	attr expr { TUPLE3(STRING("cell_port414"),$1,$2) }
	|	attr DOT TOK_ID LPAREN expr RPAREN { TUPLE7(STRING("cell_port415"),$1,DOT,TOK_ID,LPAREN,$5,RPAREN) }
	|	attr DOT TOK_ID LPAREN RPAREN { TUPLE6(STRING("cell_port416"),$1,DOT,TOK_ID,LPAREN,RPAREN) }
	|	attr DOT TOK_ID { TUPLE4(STRING("cell_port417"),$1,DOT,TOK_ID) }
	|	attr TOK_WILDCARD_CONNECT { TUPLE3(STRING("cell_port418"),$1,TOK_WILDCARD_CONNECT) }

always_comb_or_latch: TOK_ALWAYS_COMB { (TOK_ALWAYS_COMB) }
	|	TOK_ALWAYS_LATCH { (TOK_ALWAYS_LATCH) }

always_or_always_ff: TOK_ALWAYS { (TOK_ALWAYS) }
	|	TOK_ALWAYS_FF { (TOK_ALWAYS_FF) }

always_stmt: attr always_or_always_ff /* 47 */ always_cond /* 48 */ behavioral_stmt { TUPLE5(STRING("always_stmt425"),$1,$2,$3,$4) }
	|	attr always_comb_or_latch /* 49 */ behavioral_stmt { TUPLE4(STRING("always_stmt427"),$1,$2,$3) }
	|	attr TOK_INITIAL /* 50 */ behavioral_stmt { TUPLE4(STRING("always_stmt429"),$1,TOK_INITIAL,$3) }

always_cond: AT LPAREN always_events RPAREN { TUPLE5(STRING("always_cond430"),AT,LPAREN,$3,RPAREN) }
	|	AT LPAREN STAR RPAREN { TUPLE5(STRING("always_cond431"),AT,LPAREN,STAR,RPAREN) }
	|	AT ATTR_BEGIN RPAREN { TUPLE4(STRING("always_cond432"),AT,ATTR_BEGIN,RPAREN) }
	|	AT LPAREN ATTR_END { TUPLE4(STRING("always_cond433"),AT,LPAREN,ATTR_END) }
	|	AT STAR { TUPLE3(STRING("always_cond434"),AT,STAR) }
	|	EOF_TOKEN EOF_TOKEN { TUPLE3(STRING("always_cond435"),EOF_TOKEN,EOF_TOKEN) }

always_events: always_event { ($1) }
	|	always_events TOK_OR always_event { TUPLE4(STRING("always_events437"),$1,TOK_OR,$3) }
	|	always_events COMMA always_event { TUPLE4(STRING("always_events438"),$1,COMMA,$3) }

always_event: TOK_POSEDGE expr { TUPLE3(STRING("always_event439"),TOK_POSEDGE,$2) }
	|	TOK_NEGEDGE expr { TUPLE3(STRING("always_event440"),TOK_NEGEDGE,$2) }
	|	expr { ($1) }

opt_label: COLON TOK_ID { TUPLE3(STRING("opt_label442"),COLON,TOK_ID) }
	|	EOF_TOKEN EOF_TOKEN { TUPLE3(STRING("opt_label443"),EOF_TOKEN,EOF_TOKEN) }

opt_sva_label: TOK_SVA_LABEL COLON { TUPLE3(STRING("opt_sva_label444"),TOK_SVA_LABEL,COLON) }
	|	EOF_TOKEN EOF_TOKEN { TUPLE3(STRING("opt_sva_label445"),EOF_TOKEN,EOF_TOKEN) }

opt_property: TOK_PROPERTY { (TOK_PROPERTY) }
	|	TOK_FINAL { (TOK_FINAL) }
	|	EOF_TOKEN EOF_TOKEN { TUPLE3(STRING("opt_property448"),EOF_TOKEN,EOF_TOKEN) }

modport_stmt: TOK_MODPORT TOK_ID /* 51 */ modport_args_opt /* 52 */ SEMICOLON { TUPLE5(STRING("modport_stmt451"),TOK_MODPORT,TOK_ID,$3,SEMICOLON) }

modport_args_opt: LPAREN RPAREN { TUPLE3(STRING("modport_args_opt452"),LPAREN,RPAREN) }
	|	LPAREN modport_args optional_comma RPAREN { TUPLE5(STRING("modport_args_opt453"),LPAREN,$2,$3,RPAREN) }

modport_args: modport_arg { ($1) }
	|	modport_args COMMA modport_arg { TUPLE4(STRING("modport_args455"),$1,COMMA,$3) }

modport_arg: modport_type_token modport_member { TUPLE3(STRING("modport_arg456"),$1,$2) }
	|	modport_member { ($1) }

modport_member: TOK_ID { (TOK_ID) }

modport_type_token: TOK_INPUT { (TOK_INPUT) }
	|	TOK_OUTPUT { (TOK_OUTPUT) }

assert_rule: opt_sva_label TOK_ASSERT opt_property LPAREN expr RPAREN SEMICOLON { TUPLE8(STRING("assert461"),$1,TOK_ASSERT,$3,LPAREN,$5,RPAREN,SEMICOLON) }
	|	opt_sva_label TOK_ASSUME opt_property LPAREN expr RPAREN SEMICOLON { TUPLE8(STRING("assert462"),$1,TOK_ASSUME,$3,LPAREN,$5,RPAREN,SEMICOLON) }
	|	opt_sva_label TOK_ASSERT opt_property LPAREN TOK_EVENTUALLY expr RPAREN SEMICOLON { TUPLE9(STRING("assert463"),$1,TOK_ASSERT,$3,LPAREN,TOK_EVENTUALLY,$6,RPAREN,SEMICOLON) }
	|	opt_sva_label TOK_ASSUME opt_property LPAREN TOK_EVENTUALLY expr RPAREN SEMICOLON { TUPLE9(STRING("assert464"),$1,TOK_ASSUME,$3,LPAREN,TOK_EVENTUALLY,$6,RPAREN,SEMICOLON) }
	|	opt_sva_label TOK_COVER opt_property LPAREN expr RPAREN SEMICOLON { TUPLE8(STRING("assert465"),$1,TOK_COVER,$3,LPAREN,$5,RPAREN,SEMICOLON) }
	|	opt_sva_label TOK_COVER opt_property LPAREN RPAREN SEMICOLON { TUPLE7(STRING("assert466"),$1,TOK_COVER,$3,LPAREN,RPAREN,SEMICOLON) }
	|	opt_sva_label TOK_COVER SEMICOLON { TUPLE4(STRING("assert467"),$1,TOK_COVER,SEMICOLON) }
	|	opt_sva_label TOK_RESTRICT opt_property LPAREN expr RPAREN SEMICOLON { TUPLE8(STRING("assert468"),$1,TOK_RESTRICT,$3,LPAREN,$5,RPAREN,SEMICOLON) }
	|	opt_sva_label TOK_RESTRICT opt_property LPAREN TOK_EVENTUALLY expr RPAREN SEMICOLON { TUPLE9(STRING("assert469"),$1,TOK_RESTRICT,$3,LPAREN,TOK_EVENTUALLY,$6,RPAREN,SEMICOLON) }

assert_property: opt_sva_label TOK_ASSERT TOK_PROPERTY LPAREN expr RPAREN SEMICOLON { TUPLE8(STRING("assert_property470"),$1,TOK_ASSERT,TOK_PROPERTY,LPAREN,$5,RPAREN,SEMICOLON) }
	|	opt_sva_label TOK_ASSUME TOK_PROPERTY LPAREN expr RPAREN SEMICOLON { TUPLE8(STRING("assert_property471"),$1,TOK_ASSUME,TOK_PROPERTY,LPAREN,$5,RPAREN,SEMICOLON) }
	|	opt_sva_label TOK_ASSERT TOK_PROPERTY LPAREN TOK_EVENTUALLY expr RPAREN SEMICOLON { TUPLE9(STRING("assert_property472"),$1,TOK_ASSERT,TOK_PROPERTY,LPAREN,TOK_EVENTUALLY,$6,RPAREN,SEMICOLON) }
	|	opt_sva_label TOK_ASSUME TOK_PROPERTY LPAREN TOK_EVENTUALLY expr RPAREN SEMICOLON { TUPLE9(STRING("assert_property473"),$1,TOK_ASSUME,TOK_PROPERTY,LPAREN,TOK_EVENTUALLY,$6,RPAREN,SEMICOLON) }
	|	opt_sva_label TOK_COVER TOK_PROPERTY LPAREN expr RPAREN SEMICOLON { TUPLE8(STRING("assert_property474"),$1,TOK_COVER,TOK_PROPERTY,LPAREN,$5,RPAREN,SEMICOLON) }
	|	opt_sva_label TOK_RESTRICT TOK_PROPERTY LPAREN expr RPAREN SEMICOLON { TUPLE8(STRING("assert_property475"),$1,TOK_RESTRICT,TOK_PROPERTY,LPAREN,$5,RPAREN,SEMICOLON) }
	|	opt_sva_label TOK_RESTRICT TOK_PROPERTY LPAREN TOK_EVENTUALLY expr RPAREN SEMICOLON { TUPLE9(STRING("assert_property476"),$1,TOK_RESTRICT,TOK_PROPERTY,LPAREN,TOK_EVENTUALLY,$6,RPAREN,SEMICOLON) }

simple_behavioral_stmt: attr lvalue EQUALS delay expr { TUPLE6(STRING("simple_behavioral_stmt477"),$1,$2,EQUALS,$4,$5) }
	|	TOK_INT lvalue EQUALS delay expr { TUPLE6(STRING("simple_behavioral_stmt478"),TOK_INT,$2,EQUALS,$4,$5) }
	|	TOK_INT TOK_UNSIGNED lvalue EQUALS delay expr { TUPLE7(STRING("simple_behavioral_stmt479"),TOK_INT,TOK_UNSIGNED,$3,EQUALS,$5,$6) }
	|	TOK_GENVAR lvalue EQUALS delay expr { TUPLE6(STRING("simple_behavioral_stmt480"),TOK_GENVAR,$2,EQUALS,$4,$5) }
	|	attr lvalue TOK_INCREMENT { TUPLE4(STRING("simple_behavioral_stmt481"),$1,$2,TOK_INCREMENT) }
	|	attr lvalue TOK_DECREMENT { TUPLE4(STRING("simple_behavioral_stmt482"),$1,$2,TOK_DECREMENT) }
	|	attr lvalue OP_LE delay expr { TUPLE6(STRING("simple_behavioral_stmt483"),$1,$2,OP_LE,$4,$5) }
	|	attr lvalue TOK_XOR_ASSIGN delay expr { TUPLE6(STRING("simple_behavioral_stmt484"),$1,$2,TOK_XOR_ASSIGN,$4,$5) }
	|	attr lvalue TOK_OR_ASSIGN delay expr { TUPLE6(STRING("simple_behavioral_stmt485"),$1,$2,TOK_OR_ASSIGN,$4,$5) }
	|	attr lvalue TOK_PLUS_ASSIGN delay expr { TUPLE6(STRING("simple_behavioral_stmt486"),$1,$2,TOK_PLUS_ASSIGN,$4,$5) }
	|	attr lvalue TOK_SUB_ASSIGN delay expr { TUPLE6(STRING("simple_behavioral_stmt487"),$1,$2,TOK_SUB_ASSIGN,$4,$5) }
	|	attr lvalue TOK_AND_ASSIGN delay expr { TUPLE6(STRING("simple_behavioral_stmt488"),$1,$2,TOK_AND_ASSIGN,$4,$5) }

behavioral_stmt: defattr { ($1) }
	|	assert_rule { ($1) }
	|	wire_decl { ($1) }
	|	param_decl { ($1) }
	|	localparam_decl { ($1) }
	|	typedef_decl { ($1) }
	|	non_opt_delay behavioral_stmt { TUPLE3(STRING("behavioral_stmt495"),$1,$2) }
	|	simple_behavioral_stmt SEMICOLON { TUPLE3(STRING("behavioral_stmt496"),$1,SEMICOLON) }
	|	attr SEMICOLON { TUPLE3(STRING("behavioral_stmt497"),$1,SEMICOLON) }
	|	attr hierarchical_id /* 53 */ opt_arg_list SEMICOLON { TUPLE5(STRING("behavioral_stmt499"),$1,$2,$3,SEMICOLON) }
	|	attr TOK_MSG_TASKS /* 54 */ opt_arg_list SEMICOLON { TUPLE5(STRING("behavioral_stmt501"),$1,TOK_MSG_TASKS,$3,SEMICOLON) }
	|	attr TOK_BEGIN /* 55 */ opt_label /* 56 */ behavioral_stmt_list TOK_END opt_label { TUPLE7(STRING("behavioral_stmt504"),$1,TOK_BEGIN,$3,$4,TOK_END,$6) }
	|	attr TOK_FOR LPAREN /* 57 */ simple_behavioral_stmt SEMICOLON expr /* 58 */ SEMICOLON simple_behavioral_stmt RPAREN /* 59 */ behavioral_stmt { TUPLE11(STRING("behavioral_stmt508"),$1,TOK_FOR,LPAREN,$4,SEMICOLON,$6,SEMICOLON,$8,RPAREN,$10) }
	|	attr TOK_WHILE LPAREN expr RPAREN /* 60 */ behavioral_stmt { TUPLE7(STRING("behavioral_stmt510"),$1,TOK_WHILE,LPAREN,$4,RPAREN,$6) }
	|	attr TOK_REPEAT LPAREN expr RPAREN /* 61 */ behavioral_stmt { TUPLE7(STRING("behavioral_stmt512"),$1,TOK_REPEAT,LPAREN,$4,RPAREN,$6) }
	|	attr TOK_IF LPAREN expr RPAREN /* 62 */ behavioral_stmt /* 63 */ optional_else { TUPLE8(STRING("behavioral_stmt515"),$1,TOK_IF,LPAREN,$4,RPAREN,$6,$7) }
	|	case_attr case_type LPAREN expr RPAREN /* 64 */ opt_synopsys_attr case_body TOK_ENDCASE { TUPLE9(STRING("behavioral_stmt517"),$1,$2,LPAREN,$4,RPAREN,$6,$7,TOK_ENDCASE) }
	|	TOK_RETURN expr { TUPLE3(STRING("behavioral_stmt518"),TOK_RETURN,$2) }

unique_case_attr: EOF_TOKEN EOF_TOKEN { TUPLE3(STRING("unique_case_attr519"),EOF_TOKEN,EOF_TOKEN) }
	|	TOK_PRIORITY case_attr { TUPLE3(STRING("unique_case_attr520"),TOK_PRIORITY,$2) }
	|	TOK_UNIQUE case_attr { TUPLE3(STRING("unique_case_attr521"),TOK_UNIQUE,$2) }

case_attr: attr unique_case_attr { TUPLE3(STRING("case_attr522"),$1,$2) }

case_type: TOK_CASE { (TOK_CASE) }
	|	TOK_CASEX { (TOK_CASEX) }
	|	TOK_CASEZ { (TOK_CASEZ) }

opt_synopsys_attr: opt_synopsys_attr TOK_SYNOPSYS_FULL_CASE { TUPLE3(STRING("opt_synopsys_attr526"),$1,TOK_SYNOPSYS_FULL_CASE) }
	|	opt_synopsys_attr TOK_SYNOPSYS_PARALLEL_CASE { TUPLE3(STRING("opt_synopsys_attr527"),$1,TOK_SYNOPSYS_PARALLEL_CASE) }
	|	EOF_TOKEN EOF_TOKEN { TUPLE3(STRING("opt_synopsys_attr528"),EOF_TOKEN,EOF_TOKEN) }

behavioral_stmt_list: behavioral_stmt_list behavioral_stmt { TUPLE3(STRING("behavioral_stmt_list529"),$1,$2) }
	|	EOF_TOKEN EOF_TOKEN { TUPLE3(STRING("behavioral_stmt_list530"),EOF_TOKEN,EOF_TOKEN) }

optional_else: TOK_ELSE /* 65 */ behavioral_stmt { TUPLE3(STRING("optional_else532"),TOK_ELSE,$2) }
	|	EOF_TOKEN EOF_TOKEN { TUPLE3(STRING("optional_else533"),EOF_TOKEN,EOF_TOKEN) }

case_body: case_body case_item { TUPLE3(STRING("case_body534"),$1,$2) }
	|	EOF_TOKEN EOF_TOKEN { TUPLE3(STRING("case_body535"),EOF_TOKEN,EOF_TOKEN) }

case_item: /* 66 */ case_select /* 67 */ behavioral_stmt { TUPLE3(STRING("case_item538"),$1,$2) }

gen_case_body: gen_case_body gen_case_item { TUPLE3(STRING("gen_case_body539"),$1,$2) }
	|	EOF_TOKEN EOF_TOKEN { TUPLE3(STRING("gen_case_body540"),EOF_TOKEN,EOF_TOKEN) }

gen_case_item: /* 68 */ case_select /* 69 */ gen_stmt_block { TUPLE3(STRING("gen_case_item543"),$1,$2) }

case_select: case_expr_list COLON { TUPLE3(STRING("case_select544"),$1,COLON) }
	|	TOK_DEFAULT { (TOK_DEFAULT) }

case_expr_list: TOK_DEFAULT { (TOK_DEFAULT) }
	|	TOK_SVA_LABEL { (TOK_SVA_LABEL) }
	|	expr { ($1) }
	|	case_expr_list COMMA expr { TUPLE4(STRING("case_expr_list549"),$1,COMMA,$3) }

rvalue: hierarchical_id LBRACK expr RBRACK DOT rvalue { TUPLE7(STRING("rvalue550"),$1,LBRACK,$3,RBRACK,DOT,$6) }
	|	hierarchical_id range { TUPLE3(STRING("rvalue551"),$1,$2) }
	|	hierarchical_id non_opt_multirange { TUPLE3(STRING("rvalue552"),$1,$2) }

lvalue: rvalue { ($1) }
	|	LBRACE lvalue_concat_list RBRACE { TUPLE4(STRING("lvalue554"),LBRACE,$2,RBRACE) }

lvalue_concat_list: expr { ($1) }
	|	expr COMMA lvalue_concat_list { TUPLE4(STRING("lvalue_concat_list556"),$1,COMMA,$3) }

opt_arg_list: LPAREN arg_list optional_comma RPAREN { TUPLE5(STRING("opt_arg_list557"),LPAREN,$2,$3,RPAREN) }
	|	EOF_TOKEN EOF_TOKEN { TUPLE3(STRING("opt_arg_list558"),EOF_TOKEN,EOF_TOKEN) }

arg_list: arg_list2 { ($1) }
	|	EOF_TOKEN EOF_TOKEN { TUPLE3(STRING("arg_list560"),EOF_TOKEN,EOF_TOKEN) }

arg_list2: single_arg { ($1) }
	|	arg_list COMMA single_arg { TUPLE4(STRING("arg_list2562"),$1,COMMA,$3) }

single_arg: expr { ($1) }

module_gen_body: module_gen_body gen_stmt_or_module_body_stmt { TUPLE3(STRING("module_gen_body564"),$1,$2) }
	|	EOF_TOKEN EOF_TOKEN { TUPLE3(STRING("module_gen_body565"),EOF_TOKEN,EOF_TOKEN) }

gen_stmt_or_module_body_stmt: gen_stmt { ($1) }
	|	module_body_stmt { ($1) }
	|	attr SEMICOLON { TUPLE3(STRING("gen_stmt_or_module_body_stmt568"),$1,SEMICOLON) }

gen_stmt: TOK_FOR LPAREN /* 70 */ simple_behavioral_stmt SEMICOLON expr /* 71 */ SEMICOLON simple_behavioral_stmt RPAREN gen_stmt_block { TUPLE10(STRING("gen_stmt571"),TOK_FOR,LPAREN,$3,SEMICOLON,$5,SEMICOLON,$7,RPAREN,$9) }
	|	TOK_IF LPAREN expr RPAREN /* 72 */ gen_stmt_block /* 73 */ opt_gen_else { TUPLE7(STRING("gen_stmt574"),TOK_IF,LPAREN,$3,RPAREN,$5,$6) }
	|	case_type LPAREN expr RPAREN /* 74 */ gen_case_body TOK_ENDCASE { TUPLE7(STRING("gen_stmt576"),$1,LPAREN,$3,RPAREN,$5,TOK_ENDCASE) }
	|	TOK_BEGIN /* 75 */ opt_label /* 76 */ module_gen_body TOK_END opt_label { TUPLE6(STRING("gen_stmt579"),TOK_BEGIN,$2,$3,TOK_END,$5) }
	|	TOK_MSG_TASKS /* 77 */ opt_arg_list SEMICOLON { TUPLE4(STRING("gen_stmt581"),TOK_MSG_TASKS,$2,SEMICOLON) }

gen_stmt_block: /* 78 */ gen_stmt_or_module_body_stmt { ($1) }

opt_gen_else: TOK_ELSE gen_stmt_block { TUPLE3(STRING("opt_gen_else584"),TOK_ELSE,$2) }
	|	EOF_TOKEN EOF_TOKEN { TUPLE3(STRING("opt_gen_else585"),EOF_TOKEN,EOF_TOKEN) }

expr: basic_expr { ($1) }
	|	basic_expr QUERY attr expr COLON expr { TUPLE7(STRING("expr587"),$1,QUERY,$3,$4,COLON,$6) }
	|	inside_begin inside_list RBRACE { TUPLE4(STRING("expr588"),$1,$2,RBRACE) }

inside_begin: basic_expr TOK_INSIDE LBRACE { TUPLE4(STRING("inside_begin589"),$1,TOK_INSIDE,LBRACE) }

inside_list: rvalue { ($1) }
	|	inside_list COMMA inside_list { TUPLE4(STRING("inside_list591"),$1,COMMA,$3) }

basic_expr: rvalue { ($1) }
	|	LPAREN expr RPAREN integral_number { TUPLE5(STRING("basic_expr593"),LPAREN,$2,RPAREN,$4) }
	|	hierarchical_id integral_number { TUPLE3(STRING("basic_expr594"),$1,$2) }
	|	integral_number { ($1) }
	|	TOK_REALVAL { (TOK_REALVAL) }
	|	TOK_STRING { (TOK_STRING) }
	|	hierarchical_id attr /* 79 */ LPAREN arg_list optional_comma RPAREN { TUPLE7(STRING("basic_expr599"),$1,$2,LPAREN,$4,$5,RPAREN) }
	|	TOK_TO_SIGNED attr LPAREN expr RPAREN { TUPLE6(STRING("basic_expr600"),TOK_TO_SIGNED,$2,LPAREN,$4,RPAREN) }
	|	TOK_TO_UNSIGNED attr LPAREN expr RPAREN { TUPLE6(STRING("basic_expr601"),TOK_TO_UNSIGNED,$2,LPAREN,$4,RPAREN) }
	|	LPAREN expr RPAREN { TUPLE4(STRING("basic_expr602"),LPAREN,$2,RPAREN) }
	|	LPAREN expr COLON expr COLON expr RPAREN { TUPLE8(STRING("basic_expr603"),LPAREN,$2,COLON,$4,COLON,$6,RPAREN) }
	|	LBRACE concat_list RBRACE { TUPLE4(STRING("basic_expr604"),LBRACE,$2,RBRACE) }
	|	QUOTE LBRACE concat_list RBRACE { TUPLE5(STRING("basic_expr605"),QUOTE,LBRACE,$3,RBRACE) }
	|	QUOTE LBRACE assigment_pattern RBRACE { TUPLE5(STRING("basic_expr606"),QUOTE,LBRACE,$3,RBRACE) }
	|	LBRACE expr LBRACE concat_list RBRACE RBRACE { TUPLE7(STRING("basic_expr607"),LBRACE,$2,LBRACE,$4,RBRACE,RBRACE) }
	|	TILDE attr basic_expr { TUPLE4(STRING("basic_expr608"),TILDE,$2,$3) }
	|	basic_expr AMPERSAND attr basic_expr { TUPLE5(STRING("basic_expr609"),$1,AMPERSAND,$3,$4) }
	|	basic_expr OP_NAND attr basic_expr { TUPLE5(STRING("basic_expr610"),$1,OP_NAND,$3,$4) }
	|	basic_expr VBAR attr basic_expr { TUPLE5(STRING("basic_expr611"),$1,VBAR,$3,$4) }
	|	basic_expr OP_NOR attr basic_expr { TUPLE5(STRING("basic_expr612"),$1,OP_NOR,$3,$4) }
	|	basic_expr CARET attr basic_expr { TUPLE5(STRING("basic_expr613"),$1,CARET,$3,$4) }
	|	basic_expr OP_XNOR attr basic_expr { TUPLE5(STRING("basic_expr614"),$1,OP_XNOR,$3,$4) }
	|	AMPERSAND attr basic_expr { TUPLE4(STRING("basic_expr615"),AMPERSAND,$2,$3) }
	|	OP_NAND attr basic_expr { TUPLE4(STRING("basic_expr616"),OP_NAND,$2,$3) }
	|	VBAR attr basic_expr { TUPLE4(STRING("basic_expr617"),VBAR,$2,$3) }
	|	OP_NOR attr basic_expr { TUPLE4(STRING("basic_expr618"),OP_NOR,$2,$3) }
	|	CARET attr basic_expr { TUPLE4(STRING("basic_expr619"),CARET,$2,$3) }
	|	OP_XNOR attr basic_expr { TUPLE4(STRING("basic_expr620"),OP_XNOR,$2,$3) }
	|	basic_expr OP_SHL attr basic_expr { TUPLE5(STRING("basic_expr621"),$1,OP_SHL,$3,$4) }
	|	basic_expr OP_SHR attr basic_expr { TUPLE5(STRING("basic_expr622"),$1,OP_SHR,$3,$4) }
	|	basic_expr OP_SSHL attr basic_expr { TUPLE5(STRING("basic_expr623"),$1,OP_SSHL,$3,$4) }
	|	basic_expr OP_SSHR attr basic_expr { TUPLE5(STRING("basic_expr624"),$1,OP_SSHR,$3,$4) }
	|	basic_expr LESS attr basic_expr { TUPLE5(STRING("basic_expr625"),$1,LESS,$3,$4) }
	|	basic_expr OP_LE attr basic_expr { TUPLE5(STRING("basic_expr626"),$1,OP_LE,$3,$4) }
	|	basic_expr OP_EQ attr basic_expr { TUPLE5(STRING("basic_expr627"),$1,OP_EQ,$3,$4) }
	|	basic_expr OP_NE attr basic_expr { TUPLE5(STRING("basic_expr628"),$1,OP_NE,$3,$4) }
	|	basic_expr OP_EQX attr basic_expr { TUPLE5(STRING("basic_expr629"),$1,OP_EQX,$3,$4) }
	|	basic_expr OP_NEX attr basic_expr { TUPLE5(STRING("basic_expr630"),$1,OP_NEX,$3,$4) }
	|	basic_expr OP_GE attr basic_expr { TUPLE5(STRING("basic_expr631"),$1,OP_GE,$3,$4) }
	|	basic_expr GREATER attr basic_expr { TUPLE5(STRING("basic_expr632"),$1,GREATER,$3,$4) }
	|	basic_expr PLUS attr basic_expr { TUPLE5(STRING("basic_expr633"),$1,PLUS,$3,$4) }
	|	basic_expr HYPHEN attr basic_expr { TUPLE5(STRING("basic_expr634"),$1,HYPHEN,$3,$4) }
	|	basic_expr STAR attr basic_expr { TUPLE5(STRING("basic_expr635"),$1,STAR,$3,$4) }
	|	basic_expr SLASH attr basic_expr { TUPLE5(STRING("basic_expr636"),$1,SLASH,$3,$4) }
	|	basic_expr PERCENT attr basic_expr { TUPLE5(STRING("basic_expr637"),$1,PERCENT,$3,$4) }
	|	basic_expr OP_POW attr basic_expr { TUPLE5(STRING("basic_expr638"),$1,OP_POW,$3,$4) }
	|	PLUS attr basic_expr { TUPLE4(STRING("basic_expr639"),PLUS,$2,$3) }
	|	HYPHEN attr basic_expr { TUPLE4(STRING("basic_expr640"),HYPHEN,$2,$3) }
	|	basic_expr OP_LAND attr basic_expr { TUPLE5(STRING("basic_expr641"),$1,OP_LAND,$3,$4) }
	|	basic_expr OP_LOR attr basic_expr { TUPLE5(STRING("basic_expr642"),$1,OP_LOR,$3,$4) }
	|	PLING attr basic_expr { TUPLE4(STRING("basic_expr643"),PLING,$2,$3) }
	|	TOK_PKG_USER_TYPE QUOTE LPAREN expr RPAREN { TUPLE6(STRING("basic_expr644"),TOK_PKG_USER_TYPE,QUOTE,LPAREN,$4,RPAREN) }
	|	TOK_USER_TYPE QUOTE LPAREN expr RPAREN { TUPLE6(STRING("basic_expr645"),TOK_USER_TYPE,QUOTE,LPAREN,$4,RPAREN) }
	|	TOK_USER_TYPE { (TOK_USER_TYPE) }
	|	basic_expr QUOTE expr { TUPLE4(STRING("basic_expr647"),$1,QUOTE,$3) }
	|	TOK_PKG_USER_TYPE { (TOK_PKG_USER_TYPE) }

concat_list: expr { ($1) }
	|	expr COMMA concat_list { TUPLE4(STRING("concat_list650"),$1,COMMA,$3) }

assigment_pattern: lvalue COLON expr { TUPLE4(STRING("assigment_pattern651"),$1,COLON,$3) }
	|	TOK_DEFAULT COLON expr { TUPLE4(STRING("assigment_pattern652"),TOK_DEFAULT,COLON,$3) }
	|	lvalue COLON expr COMMA assigment_pattern { TUPLE6(STRING("assigment_pattern653"),$1,COLON,$3,COMMA,$5) }
	|	TOK_DEFAULT COLON expr COMMA assigment_pattern { TUPLE6(STRING("assigment_pattern654"),TOK_DEFAULT,COLON,$3,COMMA,$5) }

integral_number: TOK_CONSTVAL { (TOK_CONSTVAL) }
	|	TOK_UNBASED_UNSIZED_CONSTVAL { (TOK_UNBASED_UNSIZED_CONSTVAL) }
	|	TOK_BASE TOK_BASED_CONSTVAL { TUPLE3(STRING("integral_number657"),TOK_BASE,TOK_BASED_CONSTVAL) }
	|	TOK_CONSTVAL TOK_BASE TOK_BASED_CONSTVAL { TUPLE4(STRING("integral_number658"),TOK_CONSTVAL,TOK_BASE,TOK_BASED_CONSTVAL) }


