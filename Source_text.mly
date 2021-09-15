%{
  open Parsing
  open Source_text_types
  let declst = ref []
  let packhash_add id_t = Hashtbl.add packhash id_t ()
  let typehash_add id_t = Hashtbl.add typehash id_t ()
  let type_decl = function
    | IDENTIFIER id as x -> typehash_add id; x
    | oth -> typehash_add "oth"; oth
%}

%token  ACCEPT
%token  AMPERSAND
%token  AMPERSAND_AMPERSAND
%token  AMPERSAND_AMPERSAND_AMPERSAND
%token  AMPERSAND_EQ
%token  AT
%token  AT_AT
%token  Alias
%token  Always
%token  Always_comb
%token  Always_ff
%token  Always_latch
%token  And
%token  Assert
%token  Assign
%token  Assume
%token  Automatic
%token  BACKQUOTE
%token  BACKQUOTE_nounconnecteddrive
%token  BACKQUOTE_resetall
%token  BACKQUOTE_systemc_ctor_BLOCK
%token  BACKQUOTE_systemc_dtor_BLOCK
%token  BACKQUOTE_systemc_header_BLOCK
%token  BACKQUOTE_systemc_imp_header_BLOCK
%token  BACKQUOTE_systemc_implementation_BLOCK
%token  BACKQUOTE_systemc_interface_BLOCK
%token  BACKQUOTE_unconnected_drive_pull0
%token  BACKQUOTE_unconnected_drive_pull1
%token  BACKSLASH
%token  Before
%token  Begin
%token  Bind
%token  Bit
%token  Break
%token  Buf
%token  Bufif0
%token  Bufif1
%token  Byte
%token  CARET
%token  CARET_EQ
%token  CARET_TILDE
%token  COLON
%token  COLON_COLON
%token  COLON_EQ
%token  COLON_HYPHEN_begin
%token  COLON_HYPHEN_fork
%token  COLON_SLASH
%token  COMMA
%token <token> CONS1
%token <token*token> CONS2
%token <token*token*token> CONS3
%token <token*token*token*token> CONS4
%token  Case
%token  Casex
%token  Casez
%token  Chandle
%token  Class
%token  Clock_enable
%token  Clocker
%token  Clocking
%token  Cmos
%token  Const
%token  Const_HYPHEN_in_HYPHEN_lex
%token  Const_HYPHEN_then_HYPHEN_ref
%token  Constraint
%token  Context
%token  Continue
%token  Cover
%token  Coverage_block_off
%token  Coverage_off
%token  Coverage_on
%token  DEFAULT
%token  DLR_LBRACE_pli_HYPHEN_system_RBRACE
%token  DLR_acos
%token  DLR_acosh
%token  DLR_asin
%token  DLR_asinh
%token  DLR_atan
%token  DLR_atan2
%token  DLR_atanh
%token  DLR_bits
%token  DLR_bitstoreal
%token  DLR_bitstoshortreal
%token  DLR_c
%token  DLR_cast
%token  DLR_ceil
%token  DLR_changed
%token  DLR_clog2
%token  DLR_cos
%token  DLR_cosh
%token  DLR_countbits
%token  DLR_countones
%token  DLR_dimensions
%token  DLR_display
%token  DLR_displayb
%token  DLR_displayh
%token  DLR_displayo
%token  DLR_dumpall
%token  DLR_dumpfile
%token  DLR_dumpflush
%token  DLR_dumplimit
%token  DLR_dumpoff
%token  DLR_dumpon
%token  DLR_dumpports
%token  DLR_dumpvars
%token  DLR_error
%token  DLR_exit
%token  DLR_exp
%token  DLR_fatal
%token  DLR_fclose
%token  DLR_fdisplay
%token  DLR_fdisplayb
%token  DLR_fdisplayh
%token  DLR_fdisplayo
%token  DLR_fell
%token  DLR_feof
%token  DLR_ferror
%token  DLR_fflush
%token  DLR_fgetc
%token  DLR_fgets
%token  DLR_finish
%token  DLR_floor
%token  DLR_fmonitor
%token  DLR_fmonitorb
%token  DLR_fmonitorh
%token  DLR_fmonitoro
%token  DLR_fopen
%token  DLR_fread
%token  DLR_frewind
%token  DLR_fscanf
%token  DLR_fseek
%token  DLR_fstrobe
%token  DLR_fstrobeb
%token  DLR_fstrobeh
%token  DLR_fstrobeo
%token  DLR_ftell
%token  DLR_fwrite
%token  DLR_fwriteb
%token  DLR_fwriteh
%token  DLR_fwriteo
%token  DLR_high
%token  DLR_hypot
%token  DLR_increment
%token  DLR_info
%token  DLR_isunbounded
%token  DLR_isunknown
%token  DLR_itor
%token  DLR_left
%token  DLR_ln
%token  DLR_log10
%token  DLR_low
%token  DLR_monitor
%token  DLR_monitorb
%token  DLR_monitorh
%token  DLR_monitoro
%token  DLR_monitoroff
%token  DLR_monitoron
%token  DLR_onehot
%token  DLR_onehot0
%token  DLR_past
%token  DLR_pow
%token  DLR_printtimescale
%token  DLR_random
%token  DLR_readmemb
%token  DLR_readmemh
%token  DLR_realtime
%token  DLR_realtobits
%token  DLR_rewind
%token  DLR_right
%token  DLR_root
%token  DLR_rose
%token  DLR_rtoi
%token  DLR_sampled
%token  DLR_sformat
%token  DLR_sformatf
%token  DLR_shortrealtobits
%token  DLR_signed
%token  DLR_sin
%token  DLR_sinh
%token  DLR_size
%token  DLR_sqrt
%token  DLR_sscanf
%token  DLR_stable
%token  DLR_stime
%token  DLR_stop
%token  DLR_strobe
%token  DLR_strobeb
%token  DLR_strobeh
%token  DLR_strobeo
%token  DLR_swrite
%token  DLR_swriteb
%token  DLR_swriteh
%token  DLR_swriteo
%token  DLR_system
%token  DLR_tan
%token  DLR_tanh
%token  DLR_test_DLR_plusargs
%token  DLR_time
%token  DLR_timeformat
%token  DLR_typename
%token  DLR_ungetc
%token  DLR_unit
%token  DLR_unpacked_dimensions
%token  DLR_unsigned
%token  DLR_urandom
%token  DLR_urandom_range
%token  DLR_value_DLR_plusargs
%token  DLR_warning
%token  DLR_write
%token  DLR_writeb
%token  DLR_writeh
%token  DLR_writememb
%token  DLR_writememh
%token  DLR_writeo
%token  DOLLAR
%token  DOT
%token  DOT_STAR
%token  DOUBLEQUOTE
%token  Deassign
%token  Default
%token  Defparam
%token  Disable
%token  Dist
%token  Do
%token <token list> ELIST
%token  EMPTY_TOKEN
%token  END
%token  EOF_TOKEN
%token  EQUALS
%token  EQ_EQ
%token  EQ_EQ_EQ
%token  EQ_EQ_QUERY
%token  EQ_GT
%token  ERROR
%token  ERROR_TOKEN
%token  Edge
%token  Else
%token  End
%token  Endcase
%token  Endclass
%token  Endclocking
%token  Endfunction
%token  Endgenerate
%token  Endinterface
%token  Endmodule
%token  Endpackage
%token  Endprimitive
%token  Endprogram
%token  Endproperty
%token  Endspecify
%token  Endtable
%token  Endtask
%token  Enum
%token  Event
%token  Export
%token  Extends
%token  Extern
%token <float> FLOATING_HYPHEN_POINT_NUMBER
%token  Final
%token  For
%token  Force
%token  Foreach
%token  Forever
%token  Fork
%token  Forkjoin
%token  Full_case
%token  Function
%token  GREATER
%token  GT_EQ
%token  GT_GT
%token  GT_GT_EQ
%token  GT_GT_GT
%token  GT_GT_GT_EQ
%token  Generate
%token  Genvar
%token  Global
%token  Global_HYPHEN_in_HYPHEN_lex
%token  Global_HYPHEN_then_HYPHEN_clocking
%token  HASH
%token  HASH_HASH
%token  HYPHEN
%token  HYPHEN_COLON
%token  HYPHEN_EQ
%token  HYPHEN_GT
%token  HYPHEN_GT_GT
%token  HYPHEN_HYPHEN
%token  HYPHEN_HYPHEN_block
%token  HYPHEN_HYPHEN_file
%token  HYPHEN_HYPHEN_function
%token  HYPHEN_HYPHEN_lines
%token  HYPHEN_HYPHEN_match
%token  HYPHEN_HYPHEN_module
%token  HYPHEN_HYPHEN_msg
%token  HYPHEN_HYPHEN_rule
%token  HYPHEN_HYPHEN_task
%token  HYPHEN_HYPHEN_var
%token  Hier_block
%token <string> IDENTIFIER
%token <string> IDENTIFIER_HYPHEN_COLON_COLON
%token  IDENTIFIER_HYPHEN_in_HYPHEN_lex
%token <string> INTEGER_NUMBER
%token  If
%token  Iff
%token  Implements
%token  Import
%token  Initial
%token  Inline
%token  Inout
%token  Input
%token  Inside
%token  Int
%token  Integer
%token  Interface
%token  Isolate_assignments
%token  Join
%token  Join_any
%token  Join_none
%token  LBRACE
%token  LBRACK
%token  LBRACK_EQ
%token  LBRACK_HYPHEN_GT
%token  LBRACK_STAR
%token  LESS
%token  LINEFEED
%token  LPAREN
%token  LPAREN_HYPHEN_for_HYPHEN_strength
%token  LT_EQ
%token  LT_EQ_HYPHEN_ignored
%token  LT_HYPHEN_GT
%token  LT_LT
%token  LT_LT_EQ
%token  Lint_off
%token  Lint_on
%token  Local
%token  Local_HYPHEN_in_HYPHEN_lex
%token  Local_HYPHEN_then_HYPHEN_COLON_COLON
%token  Localparam
%token  Logic
%token  Longint
%token  Modport
%token  Module
%token  Nand
%token  Negedge
%token  New
%token  New_HYPHEN_in_HYPHEN_lex
%token  New_HYPHEN_then_HYPHEN_paren
%token  Nmos
%token  No_clocker
%token  No_inline
%token  Nor
%token  Not
%token  Notif0
%token  Notif1
%token  Null
%token  Or
%token  Output
%token  PERCENT
%token  PERCENT_EQ
%token  PLING
%token  PLING_EQ
%token  PLING_EQ_EQ
%token  PLING_EQ_QUERY
%token  PLUS
%token  PLUS_COLON
%token  PLUS_EQ
%token  PLUS_PLUS
%token  PRLOWER_THAN_ELSE
%token  PRNEGATION
%token  PRREDUCTION
%token  PRUNARYARITH
%token  Package
%token  Packed
%token  Parallel_case
%token  Parameter
%token  Pmos
%token  Posedge
%token  Primitive
%token  Priority
%token  Program
%token  Property
%token  Protected
%token  Public
%token  Public_flat
%token  Public_flat_rd
%token  Public_flat_rw
%token  Public_module
%token  Pulldown
%token  Pullup
%token  Pure
%token  QUERY
%token  QUOTE
%token  QUOTE_LBRACE
%token  RBRACE
%token  RBRACK
%token  RPAREN
%token  Rand
%token  Randc
%token  Randcase
%token  Randomize
%token  Rcmos
%token  Real
%token  Realtime
%token  Ref
%token  Reg
%token  Release
%token  Repeat
%token  Restrict
%token  Return
%token  Rnmos
%token  Rpmos
%token  Rtran
%token  Rtranif0
%token  Rtranif1
%token  SEMICOLON
%token  SLASH
%token  SLASH_EQ
%token  SLASH_STAR_verilator_clock_enable_STAR_SLASH
%token  SLASH_STAR_verilator_clocker_STAR_SLASH
%token  SLASH_STAR_verilator_coverage_block_off_STAR_SLASH
%token  SLASH_STAR_verilator_full_case_STAR_SLASH
%token  SLASH_STAR_verilator_hier_block_STAR_SLASH
%token  SLASH_STAR_verilator_inline_module_STAR_SLASH
%token  SLASH_STAR_verilator_isolate_assignments_STAR_SLASH
%token  SLASH_STAR_verilator_no_clocker_STAR_SLASH
%token  SLASH_STAR_verilator_no_inline_module_STAR_SLASH
%token  SLASH_STAR_verilator_no_inline_task_STAR_SLASH
%token  SLASH_STAR_verilator_parallel_case_STAR_SLASH
%token  SLASH_STAR_verilator_public_STAR_SLASH
%token  SLASH_STAR_verilator_public_flat_STAR_SLASH
%token  SLASH_STAR_verilator_public_flat_rd_STAR_SLASH
%token  SLASH_STAR_verilator_public_flat_rw_STAR_SLASH
%token  SLASH_STAR_verilator_public_module_STAR_SLASH
%token  SLASH_STAR_verilator_sc_bv_STAR_SLASH
%token  SLASH_STAR_verilator_sformat_STAR_SLASH
%token  SLASH_STAR_verilator_split_var_STAR_SLASH
%token  SLASH_STAR_verilator_tag_STAR_SLASH
%token <string list> SLIST
%token  STAR
%token  STAR_EQ
%token  STAR_GT
%token  STAR_STAR
%token  STRENGTH_keyword_LPAREN_strong1_SLASH_etc_RPAREN
%token <string> STRING
%token  STRING_HYPHEN_ignored
%token  Sc_bv
%token  Scalared
%token  Sformat
%token  Shortint
%token  Shortreal
%token  Signed
%token  Soft
%token  Solve
%token  Specify
%token  Specparam
%token  Split_var
%token  Static
%token  Static_HYPHEN_in_HYPHEN_lex
%token  Static_HYPHEN_then_HYPHEN_constraint
%token  String
%token  Struct
%token  Super
%token  Supply0
%token  Supply1
%token  TABLE_LINE
%token  TILDE
%token  TILDE_AMPERSAND
%token  TILDE_VBAR
%token  TIME_NUMBER
%token  TIMING_SPEC_ELEMENT
%token <token list> TLIST
%token <token*token*token*token*token*token*token*token*token*token> TUPLE10
%token <token*token*token*token*token*token*token*token*token*token*token> TUPLE11
%token <token*token*token*token*token*token*token*token*token*token*token*token> TUPLE12
%token <token*token*token*token*token*token*token*token*token*token*token*token*token> TUPLE13
%token <token*token> TUPLE2
%token <token*token*token> TUPLE3
%token <token*token*token*token> TUPLE4
%token <token*token*token*token*token> TUPLE5
%token <token*token*token*token*token*token> TUPLE6
%token <token*token*token*token*token*token*token> TUPLE7
%token <token*token*token*token*token*token*token*token> TUPLE8
%token <token*token*token*token*token*token*token*token*token> TUPLE9
%token <string> TYPE_HYPHEN_IDENTIFIER
%token  Table
%token  Task
%token  This
%token  Time
%token  Timeprecision
%token  Timeunit
%token  Tracing_off
%token  Tracing_on
%token  Tran
%token  Tranif0
%token  Tranif1
%token  Tri
%token  Tri0
%token  Tri1
%token  Triand
%token  Trior
%token  Trireg
%token  True
%token  Type
%token  Typedef
%token  UNDERSCORE
%token  Union
%token  Unique
%token  Unique0
%token  Unsigned
%token  VBAR
%token  VBAR_EQ
%token  VBAR_EQ_GT
%token  VBAR_HYPHEN_GT
%token  VBAR_VBAR
%token  Var
%token  Vectored
%token  Virtual
%token  Virtual_HYPHEN_in_HYPHEN_lex
%token  Virtual_HYPHEN_then_HYPHEN_class
%token  Virtual_HYPHEN_then_HYPHEN_identifier
%token  Virtual_HYPHEN_then_HYPHEN_interface
%token  Void
%token  Wait
%token  Wand
%token  While
%token  Wire
%token  With
%token  With_HYPHEN_in_HYPHEN_lex
%token  With_HYPHEN_then_HYPHEN_LBRACE
%token  With_HYPHEN_then_HYPHEN_LBRACK
%token  With_HYPHEN_then_HYPHEN_LPAREN
%token  Wor
%token  Wreal
%token  Xnor
%token  Xor
%type <token> ml_start
%start ml_start
%%


ml_start: source_text EOF_TOKEN { TUPLE3(STRING("ml_start0"),$1,EOF_TOKEN) }

source_text: /* empty */ { EMPTY_TOKEN }
	|	descriptionList { ($1) }

descriptionList: description { CONS1 ($1) }
	|	descriptionList description { CONS2($1,$2) }

description: module_declaration { ($1) }
	|	interface_declaration { ($1) }
	|	program_declaration { ($1) }
	|	package_declaration { ($1) }
	|	package_item { ($1) }
	|	bind_directive { ($1) }
	|	BACKQUOTE_resetall { (BACKQUOTE_resetall) }
	|	BACKQUOTE_nounconnecteddrive { (BACKQUOTE_nounconnecteddrive) }
	|	BACKQUOTE_unconnected_drive_pull0 { (BACKQUOTE_unconnected_drive_pull0) }
	|	BACKQUOTE_unconnected_drive_pull1 { (BACKQUOTE_unconnected_drive_pull1) }
	|	vltItem { ($1) }
	|	ERROR_TOKEN { (ERROR_TOKEN) }

timeunits_declaration: Timeunit TIME_NUMBER SEMICOLON { TUPLE4(STRING("timeunits_declaration17"),Timeunit,TIME_NUMBER,SEMICOLON) }
	|	Timeunit TIME_NUMBER SLASH TIME_NUMBER SEMICOLON { TUPLE6(STRING("timeunits_declaration18"),Timeunit,TIME_NUMBER,SLASH,TIME_NUMBER,SEMICOLON) }
	|	Timeprecision TIME_NUMBER SEMICOLON { TUPLE4(STRING("timeunits_declaration19"),Timeprecision,TIME_NUMBER,SEMICOLON) }

package_declaration: packageFront package_itemListE Endpackage endLabelE { TUPLE5(STRING("package_declaration20"),$1,$2,Endpackage,$4) }

packageFront: Package lifetimeE idCC SEMICOLON { TUPLE5(STRING("packageFront21"),Package,$2,$3,SEMICOLON) }

package_itemListE: /* empty */ { EMPTY_TOKEN }
	|	package_itemList { ($1) }

package_itemList: package_item { CONS1 ($1) }
	|	package_itemList package_item { CONS2($1,$2) }

package_item: package_or_generate_item_declaration { ($1) }
	|	anonymous_program { ($1) }
	|	package_export_declaration { ($1) }
	|	timeunits_declaration { ($1) }

package_or_generate_item_declaration: net_declaration { ($1) }
	|	data_declaration { ($1) }
	|	task_declaration { ($1) }
	|	function_declaration { ($1) }
	|	dpi_import_export { ($1) }
	|	class_declaration { ($1) }
	|	parameter_declaration SEMICOLON { TUPLE3(STRING("package_or_generate_item_declaration36"),$1,SEMICOLON) }
	|	SEMICOLON { (SEMICOLON) }

package_import_declarationList: package_import_declaration { CONS1 ($1) }
	|	package_import_declarationList package_import_declaration { CONS2($1,$2) }

package_import_declaration: Import package_import_itemList SEMICOLON { TUPLE4(STRING("package_import_declaration40"),Import,$2,SEMICOLON) }

package_import_itemList: package_import_item { CONS1 ($1) }
	|	package_import_itemList COMMA package_import_item { CONS3($1,COMMA,$3) }

package_import_item: idCC COLON_COLON package_import_itemObj { TUPLE4(STRING("package_import_item43"),$1,COLON_COLON,$3) }

package_import_itemObj: idAny { ($1) }
	|	STAR { (STAR) }

package_export_declaration: Export STAR COLON_COLON STAR SEMICOLON { TUPLE6(STRING("package_export_declaration46"),Export,STAR,COLON_COLON,STAR,SEMICOLON) }
	|	Export package_export_itemList SEMICOLON { TUPLE4(STRING("package_export_declaration47"),Export,$2,SEMICOLON) }

package_export_itemList: package_export_item { CONS1 ($1) }
	|	package_export_itemList COMMA package_export_item { CONS3($1,COMMA,$3) }

package_export_item: idCC COLON_COLON package_import_itemObj { TUPLE4(STRING("package_export_item50"),$1,COLON_COLON,$3) }

module_declaration: modFront importsAndParametersE portsStarE SEMICOLON module_itemListE Endmodule endLabelE { TUPLE8(STRING("module_declaration51"),$1,$2,$3,SEMICOLON,$5,Endmodule,$7) }
	|	udpFront parameter_port_listE portsStarE SEMICOLON module_itemListE Endprimitive endLabelE { TUPLE8(STRING("module_declaration52"),$1,$2,$3,SEMICOLON,$5,Endprimitive,$7) }
	|	Extern modFront parameter_port_listE portsStarE SEMICOLON { TUPLE6(STRING("module_declaration53"),Extern,$2,$3,$4,SEMICOLON) }

modFront: Module lifetimeE idAny { TUPLE4(STRING("modFront54"),Module,$2,$3) }

importsAndParametersE: parameter_port_listE { ($1) }
	|	package_import_declarationList parameter_port_listE { TUPLE3(STRING("importsAndParametersE56"),$1,$2) }

udpFront: Primitive lifetimeE idAny { TUPLE4(STRING("udpFront57"),Primitive,$2,$3) }

parameter_value_assignmentE: /* empty */ { EMPTY_TOKEN }
	|	parameter_value_assignment { ($1) }

parameter_value_assignment: HASH LPAREN cellparamList RPAREN { TUPLE5(STRING("parameter_value_assignment60"),HASH,LPAREN,$3,RPAREN) }
	|	HASH INTEGER_NUMBER { TUPLE3(STRING("parameter_value_assignment61"),HASH,INTEGER_NUMBER $2) }
	|	HASH FLOATING_HYPHEN_POINT_NUMBER { TUPLE3(STRING("parameter_value_assignment62"),HASH,FLOATING_HYPHEN_POINT_NUMBER $2) }
	|	HASH timeNumAdjusted { TUPLE3(STRING("parameter_value_assignment63"),HASH,$2) }
	|	HASH idClassSel { TUPLE3(STRING("parameter_value_assignment64"),HASH,$2) }

parameter_value_assignmentClass: HASH LPAREN cellparamList RPAREN { TUPLE5(STRING("parameter_value_assignmentClass65"),HASH,LPAREN,$3,RPAREN) }

parameter_port_listE: /* empty */ { EMPTY_TOKEN }
	|	HASH LPAREN RPAREN { TUPLE4(STRING("parameter_port_listE67"),HASH,LPAREN,RPAREN) }
	|	HASH LPAREN /* 1 */ paramPortDeclOrArgList RPAREN { TUPLE5(STRING("parameter_port_listE69"),HASH,LPAREN,$3,RPAREN) }

paramPortDeclOrArgList: paramPortDeclOrArg { CONS1 ($1) }
	|	paramPortDeclOrArgList COMMA paramPortDeclOrArg { CONS3($1,COMMA,$3) }

paramPortDeclOrArg: parameter_port_declarationFrontE param_assignment { TUPLE3(STRING("paramPortDeclOrArg72"),$1,$2) }
	|	parameter_port_declarationTypeFrontE type_assignment { TUPLE3(STRING("paramPortDeclOrArg73"),$1,$2) }
	|	vlTag { ($1) }

portsStarE: /* empty */ { EMPTY_TOKEN }
	|	LPAREN RPAREN { TUPLE3(STRING("portsStarE76"),LPAREN,RPAREN) }
	|	LPAREN /* 2 */ list_of_ports RPAREN { TUPLE4(STRING("portsStarE78"),LPAREN,$2,RPAREN) }

list_of_ports: portAndTag { CONS1 ($1) }
	|	list_of_ports COMMA portAndTag { CONS3($1,COMMA,$3) }

portAndTag: port { ($1) }
	|	vlTag port { TUPLE3(STRING("portAndTag82"),$1,$2) }

port: portDirNetE id portSig variable_dimensionListE sigAttrListE { TUPLE6(STRING("port83"),$1,$2,$3,$4,$5) }
	|	portDirNetE id DOT idAny portSig variable_dimensionListE sigAttrListE { TUPLE8(STRING("port84"),$1,$2,DOT,$4,$5,$6,$7) }
	|	portDirNetE Interface portSig rangeListE sigAttrListE { TUPLE6(STRING("port85"),$1,Interface,$3,$4,$5) }
	|	portDirNetE Interface DOT idAny portSig rangeListE sigAttrListE { TUPLE8(STRING("port86"),$1,Interface,DOT,$4,$5,$6,$7) }
	|	portDirNetE data_type portSig variable_dimensionListE sigAttrListE { TUPLE6(STRING("port87"),$1,$2,$3,$4,$5) }
	|	portDirNetE Var data_type portSig variable_dimensionListE sigAttrListE { TUPLE7(STRING("port88"),$1,Var,$3,$4,$5,$6) }
	|	portDirNetE Var implicit_typeE portSig variable_dimensionListE sigAttrListE { TUPLE7(STRING("port89"),$1,Var,$3,$4,$5,$6) }
	|	portDirNetE signing portSig variable_dimensionListE sigAttrListE { TUPLE6(STRING("port90"),$1,$2,$3,$4,$5) }
	|	portDirNetE signingE rangeList portSig variable_dimensionListE sigAttrListE { TUPLE7(STRING("port91"),$1,$2,$3,$4,$5,$6) }
	|	portDirNetE portSig variable_dimensionListE sigAttrListE { TUPLE5(STRING("port92"),$1,$2,$3,$4) }
	|	portDirNetE data_type portSig variable_dimensionListE sigAttrListE EQUALS constExpr { TUPLE8(STRING("port93"),$1,$2,$3,$4,$5,EQUALS,$7) }
	|	portDirNetE Var data_type portSig variable_dimensionListE sigAttrListE EQUALS constExpr { TUPLE9(STRING("port94"),$1,Var,$3,$4,$5,$6,EQUALS,$8) }
	|	portDirNetE Var implicit_typeE portSig variable_dimensionListE sigAttrListE EQUALS constExpr { TUPLE9(STRING("port95"),$1,Var,$3,$4,$5,$6,EQUALS,$8) }
	|	portDirNetE portSig variable_dimensionListE sigAttrListE EQUALS constExpr { TUPLE7(STRING("port96"),$1,$2,$3,$4,EQUALS,$6) }

portDirNetE: /* empty */ { EMPTY_TOKEN }
	|	port_direction { ($1) }
	|	port_direction /* 3 */ net_type { TUPLE3(STRING("portDirNetE100"),$1,$2) }
	|	net_type { ($1) }

port_declNetE: /* empty */ { EMPTY_TOKEN }
	|	net_type { ($1) }

portSig: id { ($1) }
	|	idSVKwd { ($1) }

interface_declaration: intFront importsAndParametersE portsStarE SEMICOLON interface_itemListE Endinterface endLabelE { TUPLE8(STRING("interface_declaration106"),$1,$2,$3,SEMICOLON,$5,Endinterface,$7) }
	|	Extern intFront parameter_port_listE portsStarE SEMICOLON { TUPLE6(STRING("interface_declaration107"),Extern,$2,$3,$4,SEMICOLON) }

intFront: Interface lifetimeE idAny { TUPLE4(STRING("intFront108"),Interface,$2,$3) }

interface_itemListE: /* empty */ { EMPTY_TOKEN }
	|	interface_itemList { ($1) }

interface_itemList: interface_item { CONS1 ($1) }
	|	interface_itemList interface_item { CONS2($1,$2) }

interface_item: port_declaration SEMICOLON { TUPLE3(STRING("interface_item113"),$1,SEMICOLON) }
	|	interface_generate_region { ($1) }
	|	interface_or_generate_item { ($1) }
	|	program_declaration { ($1) }
	|	interface_declaration { ($1) }
	|	timeunits_declaration { ($1) }
	|	module_common_item { ($1) }

interface_generate_region: Generate interface_itemList Endgenerate { TUPLE4(STRING("interface_generate_region120"),Generate,$2,Endgenerate) }
	|	Generate Endgenerate { TUPLE3(STRING("interface_generate_region121"),Generate,Endgenerate) }

interface_or_generate_item: modport_declaration { ($1) }
	|	extern_tf_declaration { ($1) }

anonymous_program: Program SEMICOLON anonymous_program_itemListE Endprogram { TUPLE5(STRING("anonymous_program124"),Program,SEMICOLON,$3,Endprogram) }

anonymous_program_itemListE: /* empty */ { EMPTY_TOKEN }
	|	anonymous_program_itemList { ($1) }

anonymous_program_itemList: anonymous_program_item { CONS1 ($1) }
	|	anonymous_program_itemList anonymous_program_item { CONS2($1,$2) }

anonymous_program_item: task_declaration { ($1) }
	|	function_declaration { ($1) }
	|	class_declaration { ($1) }
	|	SEMICOLON { (SEMICOLON) }

program_declaration: pgmFront parameter_port_listE portsStarE SEMICOLON program_itemListE Endprogram endLabelE { TUPLE8(STRING("program_declaration133"),$1,$2,$3,SEMICOLON,$5,Endprogram,$7) }
	|	Extern pgmFront parameter_port_listE portsStarE SEMICOLON { TUPLE6(STRING("program_declaration134"),Extern,$2,$3,$4,SEMICOLON) }

pgmFront: Program lifetimeE idAny { TUPLE4(STRING("pgmFront135"),Program,$2,$3) }

program_itemListE: /* empty */ { EMPTY_TOKEN }
	|	program_itemList { ($1) }

program_itemList: program_item { CONS1 ($1) }
	|	program_itemList program_item { CONS2($1,$2) }

program_item: port_declaration SEMICOLON { TUPLE3(STRING("program_item140"),$1,SEMICOLON) }
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

extern_tf_declaration: Extern task_prototype SEMICOLON { TUPLE4(STRING("extern_tf_declaration153"),Extern,$2,SEMICOLON) }
	|	Extern function_prototype SEMICOLON { TUPLE4(STRING("extern_tf_declaration154"),Extern,$2,SEMICOLON) }
	|	Extern Forkjoin task_prototype SEMICOLON { TUPLE5(STRING("extern_tf_declaration155"),Extern,Forkjoin,$3,SEMICOLON) }

modport_declaration: Modport modport_itemList SEMICOLON { TUPLE4(STRING("modport_declaration156"),Modport,$2,SEMICOLON) }

modport_itemList: modport_item { CONS1 ($1) }
	|	modport_itemList COMMA modport_item { CONS3($1,COMMA,$3) }

modport_item: id LPAREN /* 4 */ modportPortsDeclList RPAREN { TUPLE5(STRING("modport_item160"),$1,LPAREN,$3,RPAREN) }

modportPortsDeclList: modportPortsDecl { CONS1 ($1) }
	|	modportPortsDeclList COMMA modportPortsDecl { CONS3($1,COMMA,$3) }

modportPortsDecl: port_direction modportSimplePort { TUPLE3(STRING("modportPortsDecl163"),$1,$2) }
	|	Clocking idAny { TUPLE3(STRING("modportPortsDecl164"),Clocking,$2) }
	|	Import id { TUPLE3(STRING("modportPortsDecl165"),Import,$2) }
	|	Export id { TUPLE3(STRING("modportPortsDecl166"),Export,$2) }
	|	Import method_prototype { TUPLE3(STRING("modportPortsDecl167"),Import,$2) }
	|	Export method_prototype { TUPLE3(STRING("modportPortsDecl168"),Export,$2) }
	|	modportSimplePort { ($1) }

modportSimplePort: id { ($1) }

genvar_declaration: Genvar list_of_genvar_identifiers SEMICOLON { TUPLE4(STRING("genvar_declaration171"),Genvar,$2,SEMICOLON) }

list_of_genvar_identifiers: genvar_identifierDecl { CONS1 ($1) }
	|	list_of_genvar_identifiers COMMA genvar_identifierDecl { CONS3($1,COMMA,$3) }

genvar_identifierDecl: id sigAttrListE { TUPLE3(STRING("genvar_identifierDecl174"),$1,$2) }

parameter_declaration: parameter_declarationFront list_of_param_assignments { TUPLE3(STRING("parameter_declaration175"),$1,$2) }
	|	parameter_declarationTypeFront list_of_type_assignments { TUPLE3(STRING("parameter_declaration176"),$1,$2) }

parameter_declarationFront: varParamReset implicit_typeE { TUPLE3(STRING("parameter_declarationFront177"),$1,$2) }
	|	varParamReset data_type { TUPLE3(STRING("parameter_declarationFront178"),$1,$2) }

parameter_declarationTypeFront: varParamReset Type { TUPLE3(STRING("parameter_declarationTypeFront179"),$1,Type) }

parameter_port_declarationFrontE: varParamReset implicit_typeE { TUPLE3(STRING("parameter_port_declarationFrontE180"),$1,$2) }
	|	varParamReset data_type { TUPLE3(STRING("parameter_port_declarationFrontE181"),$1,$2) }
	|	implicit_typeE { ($1) }
	|	data_type { ($1) }

parameter_port_declarationTypeFrontE: varParamReset Type { TUPLE3(STRING("parameter_port_declarationTypeFrontE184"),$1,Type) }
	|	Type { (Type) }

net_declaration: net_declarationFront netSigList SEMICOLON { TUPLE4(STRING("net_declaration186"),$1,$2,SEMICOLON) }

net_declarationFront: net_declRESET net_type strengthSpecE net_scalaredE net_dataTypeE { TUPLE6(STRING("net_declarationFront187"),$1,$2,$3,$4,$5) }

net_declRESET: /* empty */ { EMPTY_TOKEN }

net_scalaredE: /* empty */ { EMPTY_TOKEN }
	|	Scalared { (Scalared) }
	|	Vectored { (Vectored) }

net_dataTypeE: var_data_type { ($1) }
	|	signingE rangeList delayE { TUPLE4(STRING("net_dataTypeE193"),$1,$2,$3) }
	|	signing { ($1) }
	|	delayE { ($1) }

net_type: Supply0 { (Supply0) }
	|	Supply1 { (Supply1) }
	|	Tri { (Tri) }
	|	Tri0 { (Tri0) }
	|	Tri1 { (Tri1) }
	|	Triand { (Triand) }
	|	Trior { (Trior) }
	|	Trireg { (Trireg) }
	|	Wand { (Wand) }
	|	Wire { (Wire) }
	|	Wor { (Wor) }
	|	Wreal { (Wreal) }

varParamReset: Parameter { (Parameter) }
	|	Localparam { (Localparam) }

port_direction: Input { (Input) }
	|	Output { (Output) }
	|	Inout { (Inout) }
	|	Ref { (Ref) }
	|	Const_HYPHEN_then_HYPHEN_ref Ref { TUPLE3(STRING("port_direction214"),Const_HYPHEN_then_HYPHEN_ref,Ref) }

port_directionReset: Input { (Input) }
	|	Output { (Output) }
	|	Inout { (Inout) }
	|	Ref { (Ref) }
	|	Const_HYPHEN_then_HYPHEN_ref Ref { TUPLE3(STRING("port_directionReset219"),Const_HYPHEN_then_HYPHEN_ref,Ref) }

port_declaration: port_directionReset port_declNetE data_type /* 5 */ list_of_variable_decl_assignments { TUPLE5(STRING("port_declaration221"),$1,$2,$3,$4) }
	|	port_directionReset port_declNetE Var data_type /* 6 */ list_of_variable_decl_assignments { TUPLE6(STRING("port_declaration223"),$1,$2,Var,$4,$5) }
	|	port_directionReset port_declNetE Var implicit_typeE /* 7 */ list_of_variable_decl_assignments { TUPLE6(STRING("port_declaration225"),$1,$2,Var,$4,$5) }
	|	port_directionReset port_declNetE signingE rangeList /* 8 */ list_of_variable_decl_assignments { TUPLE6(STRING("port_declaration227"),$1,$2,$3,$4,$5) }
	|	port_directionReset port_declNetE signing /* 9 */ list_of_variable_decl_assignments { TUPLE5(STRING("port_declaration229"),$1,$2,$3,$4) }
	|	port_directionReset port_declNetE /* 10 */ list_of_variable_decl_assignments { TUPLE4(STRING("port_declaration231"),$1,$2,$3) }

tf_port_declaration: port_directionReset data_type /* 11 */ list_of_tf_variable_identifiers SEMICOLON { TUPLE5(STRING("tf_port_declaration233"),$1,$2,$3,SEMICOLON) }
	|	port_directionReset implicit_typeE /* 12 */ list_of_tf_variable_identifiers SEMICOLON { TUPLE5(STRING("tf_port_declaration235"),$1,$2,$3,SEMICOLON) }
	|	port_directionReset Var data_type /* 13 */ list_of_tf_variable_identifiers SEMICOLON { TUPLE6(STRING("tf_port_declaration237"),$1,Var,$3,$4,SEMICOLON) }
	|	port_directionReset Var implicit_typeE /* 14 */ list_of_tf_variable_identifiers SEMICOLON { TUPLE6(STRING("tf_port_declaration239"),$1,Var,$3,$4,SEMICOLON) }

integer_atom_type: Byte { (Byte) }
	|	Shortint { (Shortint) }
	|	Int { (Int) }
	|	Longint { (Longint) }
	|	Integer { (Integer) }
	|	Time { (Time) }

integer_vector_type: Bit { (Bit) }
	|	Logic { (Logic) }
	|	Reg { (Reg) }

non_integer_type: Real { (Real) }
	|	Realtime { (Realtime) }
	|	Shortreal { (Shortreal) }

signingE: /* empty */ { EMPTY_TOKEN }
	|	signing { ($1) }

signing: Signed { (Signed) }
	|	Unsigned { (Unsigned) }

simple_type: integer_atom_type { ($1) }
	|	integer_vector_type { ($1) }
	|	non_integer_type { ($1) }
	|	packageClassScopeE idType { TUPLE3(STRING("simple_type259"),$1,$2) }

data_type: data_typeNoRef { ($1) }
	|	packageClassScopeE idType packed_dimensionListE { TUPLE4(STRING("data_type261"),$1,$2,$3) }
	|	packageClassScopeE idType parameter_value_assignmentClass packed_dimensionListE { TUPLE5(STRING("data_type262"),$1,$2,$3,$4) }

data_typeBasic: integer_vector_type signingE rangeListE { TUPLE4(STRING("data_typeBasic263"),$1,$2,$3) }
	|	integer_atom_type signingE { TUPLE3(STRING("data_typeBasic264"),$1,$2) }
	|	non_integer_type { ($1) }

data_typeNoRef: data_typeBasic { ($1) }
	|	struct_unionDecl packed_dimensionListE { TUPLE3(STRING("data_typeNoRef267"),$1,$2) }
	|	enumDecl { ($1) }
	|	String { (String) }
	|	Chandle { (Chandle) }
	|	Event { (Event) }
	|	Virtual_HYPHEN_then_HYPHEN_interface Interface id { TUPLE4(STRING("data_typeNoRef272"),Virtual_HYPHEN_then_HYPHEN_interface,Interface,$3) }
	|	Virtual_HYPHEN_then_HYPHEN_identifier id { TUPLE3(STRING("data_typeNoRef273"),Virtual_HYPHEN_then_HYPHEN_identifier,$2) }
	|	type_reference { ($1) }

data_type_or_void: data_type { ($1) }

var_data_type: data_type { ($1) }
	|	Var data_type { TUPLE3(STRING("var_data_type277"),Var,$2) }
	|	Var implicit_typeE { TUPLE3(STRING("var_data_type278"),Var,$2) }

type_reference: Type LPAREN exprOrDataType RPAREN { TUPLE5(STRING("type_reference279"),Type,LPAREN,$3,RPAREN) }

struct_unionDecl: Struct packedSigningE LBRACE /* 15 */ struct_union_memberList RBRACE { TUPLE6(STRING("struct_unionDecl281"),Struct,$2,LBRACE,$4,RBRACE) }
	|	Union taggedE packedSigningE LBRACE /* 16 */ struct_union_memberList RBRACE { TUPLE7(STRING("struct_unionDecl283"),Union,$2,$3,LBRACE,$5,RBRACE) }

struct_union_memberList: struct_union_member { CONS1 ($1) }
	|	struct_union_memberList struct_union_member { CONS2($1,$2) }

struct_union_member: random_qualifierE data_type_or_void /* 17 */ list_of_member_decl_assignments SEMICOLON { TUPLE5(STRING("struct_union_member287"),$1,$2,$3,SEMICOLON) }
	|	vlTag { ($1) }

list_of_member_decl_assignments: member_decl_assignment { CONS1 ($1) }
	|	list_of_member_decl_assignments COMMA member_decl_assignment { CONS3($1,COMMA,$3) }

member_decl_assignment: id variable_dimensionListE { TUPLE3(STRING("member_decl_assignment291"),$1,$2) }
	|	id variable_dimensionListE EQUALS variable_declExpr { TUPLE5(STRING("member_decl_assignment292"),$1,$2,EQUALS,$4) }
	|	idSVKwd { ($1) }
	|	EQUALS class_new { TUPLE3(STRING("member_decl_assignment294"),EQUALS,$2) }

list_of_variable_decl_assignments: variable_decl_assignment { CONS1 ($1) }
	|	list_of_variable_decl_assignments COMMA variable_decl_assignment { CONS3($1,COMMA,$3) }

variable_decl_assignment: id variable_dimensionListE sigAttrListE { TUPLE4(STRING("variable_decl_assignment297"),$1,$2,$3) }
	|	id variable_dimensionListE sigAttrListE EQUALS variable_declExpr { TUPLE6(STRING("variable_decl_assignment298"),$1,$2,$3,EQUALS,$5) }
	|	idSVKwd { ($1) }
	|	EQUALS class_new { TUPLE3(STRING("variable_decl_assignment300"),EQUALS,$2) }

list_of_tf_variable_identifiers: tf_variable_identifier { CONS1 ($1) }
	|	list_of_tf_variable_identifiers COMMA tf_variable_identifier { CONS3($1,COMMA,$3) }

tf_variable_identifier: id variable_dimensionListE sigAttrListE exprEqE { TUPLE5(STRING("tf_variable_identifier303"),$1,$2,$3,$4) }

variable_declExpr: expr { ($1) }
	|	dynamic_array_new { ($1) }
	|	class_new { ($1) }

variable_dimensionListE: /* empty */ { EMPTY_TOKEN }
	|	variable_dimensionList { ($1) }

variable_dimensionList: variable_dimension { CONS1 ($1) }
	|	variable_dimensionList variable_dimension { CONS2($1,$2) }

variable_dimension: LBRACK RBRACK { TUPLE3(STRING("variable_dimension311"),LBRACK,RBRACK) }
	|	anyrange { ($1) }
	|	LBRACK exprOrDataType RBRACK { TUPLE4(STRING("variable_dimension313"),LBRACK,$2,RBRACK) }
	|	LBRACK_STAR RBRACK { TUPLE3(STRING("variable_dimension314"),LBRACK_STAR,RBRACK) }
	|	LBRACK STAR RBRACK { TUPLE4(STRING("variable_dimension315"),LBRACK,STAR,RBRACK) }

random_qualifierE: /* empty */ { EMPTY_TOKEN }
	|	random_qualifier { ($1) }

random_qualifier: Rand { (Rand) }
	|	Randc { (Randc) }

taggedE: /* empty */ { EMPTY_TOKEN }

packedSigningE: /* empty */ { EMPTY_TOKEN }
	|	Packed signingE { TUPLE3(STRING("packedSigningE322"),Packed,$2) }

enumDecl: Enum enum_base_typeE LBRACE enum_nameList RBRACE { TUPLE6(STRING("enumDecl323"),Enum,$2,LBRACE,$4,RBRACE) }

enum_base_typeE: /* empty */ { EMPTY_TOKEN }
	|	signingE rangeList { TUPLE3(STRING("enum_base_typeE325"),$1,$2) }
	|	signing { ($1) }
	|	integer_atom_type signingE { TUPLE3(STRING("enum_base_typeE327"),$1,$2) }
	|	integer_vector_type signingE rangeListE { TUPLE4(STRING("enum_base_typeE328"),$1,$2,$3) }
	|	idAny rangeListE { TUPLE3(STRING("enum_base_typeE329"),$1,$2) }
	|	packageClassScope idAny rangeListE { TUPLE4(STRING("enum_base_typeE330"),$1,$2,$3) }

enum_nameList: enum_name_declaration { CONS1 ($1) }
	|	enum_nameList COMMA enum_name_declaration { CONS3($1,COMMA,$3) }

enum_name_declaration: idAny enumNameRangeE enumNameStartE { TUPLE4(STRING("enum_name_declaration333"),$1,$2,$3) }

enumNameRangeE: /* empty */ { EMPTY_TOKEN }
	|	LBRACK intnumAsConst RBRACK { TUPLE4(STRING("enumNameRangeE335"),LBRACK,$2,RBRACK) }
	|	LBRACK intnumAsConst COLON intnumAsConst RBRACK { TUPLE6(STRING("enumNameRangeE336"),LBRACK,$2,COLON,$4,RBRACK) }

enumNameStartE: /* empty */ { EMPTY_TOKEN }
	|	EQUALS constExpr { TUPLE3(STRING("enumNameStartE338"),EQUALS,$2) }

intnumAsConst: INTEGER_NUMBER { (INTEGER_NUMBER $1) }

data_declaration: data_declarationVar { ($1) }
	|	type_declaration { ($1) }
	|	package_import_declaration { ($1) }
	|	vlTag { ($1) }

class_property: memberQualListE data_declarationVarClass { TUPLE3(STRING("class_property344"),$1,$2) }
	|	memberQualListE type_declaration { TUPLE3(STRING("class_property345"),$1,$2) }
	|	memberQualListE package_import_declaration { TUPLE3(STRING("class_property346"),$1,$2) }

data_declarationVar: data_declarationVarFront list_of_variable_decl_assignments SEMICOLON { TUPLE4(STRING("data_declarationVar347"),$1,$2,SEMICOLON) }

data_declarationVarClass: data_declarationVarFrontClass list_of_variable_decl_assignments SEMICOLON { TUPLE4(STRING("data_declarationVarClass348"),$1,$2,SEMICOLON) }

data_declarationVarFront: Var lifetimeE data_type { TUPLE4(STRING("data_declarationVarFront349"),Var,$2,$3) }
	|	Var lifetimeE { TUPLE3(STRING("data_declarationVarFront350"),Var,$2) }
	|	Var lifetimeE signingE rangeList { TUPLE5(STRING("data_declarationVarFront351"),Var,$2,$3,$4) }
	|	Const Var lifetimeE data_type { TUPLE5(STRING("data_declarationVarFront352"),Const,Var,$3,$4) }
	|	Const Var lifetimeE { TUPLE4(STRING("data_declarationVarFront353"),Const,Var,$3) }
	|	Const Var lifetimeE signingE rangeList { TUPLE6(STRING("data_declarationVarFront354"),Const,Var,$3,$4,$5) }
	|	data_type { ($1) }
	|	lifetime data_type { TUPLE3(STRING("data_declarationVarFront356"),$1,$2) }
	|	Const lifetimeE data_type { TUPLE4(STRING("data_declarationVarFront357"),Const,$2,$3) }

data_declarationVarFrontClass: Var lifetimeE data_type { TUPLE4(STRING("data_declarationVarFrontClass358"),Var,$2,$3) }
	|	Var lifetimeE { TUPLE3(STRING("data_declarationVarFrontClass359"),Var,$2) }
	|	Var lifetimeE signingE rangeList { TUPLE5(STRING("data_declarationVarFrontClass360"),Var,$2,$3,$4) }
	|	data_type { ($1) }

implicit_typeE: /* empty */ { EMPTY_TOKEN }
	|	signingE rangeList { TUPLE3(STRING("implicit_typeE363"),$1,$2) }
	|	signing { ($1) }

type_declaration: Typedef data_typeNoRef idAny variable_dimensionListE dtypeAttrListE SEMICOLON { TUPLE7(STRING("type_declaration365"),Typedef,$2,type_decl $3,$4,$5,SEMICOLON) }
	|	Typedef packageClassScope idType packed_dimensionListE idAny variable_dimensionListE dtypeAttrListE SEMICOLON { TUPLE9(STRING("type_declaration366"),Typedef,$2,$3,$4,type_decl $5,$6,$7,SEMICOLON) }
	|	Typedef packageClassScope idType parameter_value_assignmentClass packed_dimensionListE idAny variable_dimensionListE dtypeAttrListE SEMICOLON { TUPLE10(STRING("type_declaration367"),Typedef,$2,$3,$4,$5,type_decl $6,$7,$8,SEMICOLON) }
	|	Typedef idType packed_dimensionListE idAny variable_dimensionListE dtypeAttrListE SEMICOLON { TUPLE8(STRING("type_declaration368"),Typedef,$2,$3,type_decl $4,$5,$6,SEMICOLON) }
	|	Typedef idType parameter_value_assignmentClass packed_dimensionListE idAny variable_dimensionListE dtypeAttrListE SEMICOLON { TUPLE9(STRING("type_declaration369"),Typedef,$2,$3,$4,type_decl $5,$6,$7,SEMICOLON) }
	|	Typedef id DOT idAny idAny SEMICOLON { TUPLE7(STRING("type_declaration370"),Typedef,$2,DOT,$4,type_decl $5,SEMICOLON) }
	|	Typedef idType SEMICOLON { TUPLE4(STRING("type_declaration371"),Typedef,type_decl $2,SEMICOLON) }
	|	Typedef id SEMICOLON { TUPLE4(STRING("type_declaration372"),Typedef,type_decl $2,SEMICOLON) }
	|	Typedef Enum idAny SEMICOLON { TUPLE5(STRING("type_declaration373"),Typedef,Enum,type_decl $3,SEMICOLON) }
	|	Typedef Struct idAny SEMICOLON { TUPLE5(STRING("type_declaration374"),Typedef,Struct,type_decl $3,SEMICOLON) }
	|	Typedef Union idAny SEMICOLON { TUPLE5(STRING("type_declaration375"),Typedef,Union,type_decl $3,SEMICOLON) }
	|	Typedef Class idAny SEMICOLON { TUPLE5(STRING("type_declaration376"),Typedef,Class,type_decl $3,SEMICOLON) }
	|	Typedef Interface Class idAny SEMICOLON { TUPLE6(STRING("type_declaration377"),Typedef,Interface,Class,type_decl $4,SEMICOLON) }

dtypeAttrListE: /* empty */ { EMPTY_TOKEN }
	|	dtypeAttrList { ($1) }

dtypeAttrList: dtypeAttr { CONS1 ($1) }
	|	dtypeAttrList dtypeAttr { CONS2($1,$2) }

dtypeAttr: SLASH_STAR_verilator_public_STAR_SLASH { (SLASH_STAR_verilator_public_STAR_SLASH) }

vlTag: SLASH_STAR_verilator_tag_STAR_SLASH { (SLASH_STAR_verilator_tag_STAR_SLASH) }

module_itemListE: /* empty */ { EMPTY_TOKEN }
	|	module_itemList { ($1) }

module_itemList: module_item { CONS1 ($1) }
	|	module_itemList module_item { CONS2($1,$2) }

module_item: port_declaration SEMICOLON { TUPLE3(STRING("module_item388"),$1,SEMICOLON) }
	|	non_port_module_item { ($1) }

non_port_module_item: generate_region { ($1) }
	|	module_or_generate_item { ($1) }
	|	specify_block { ($1) }
	|	specparam_declaration { ($1) }
	|	program_declaration { ($1) }
	|	module_declaration { ($1) }
	|	interface_declaration { ($1) }
	|	timeunits_declaration { ($1) }
	|	BACKQUOTE_systemc_header_BLOCK { (BACKQUOTE_systemc_header_BLOCK) }
	|	BACKQUOTE_systemc_ctor_BLOCK { (BACKQUOTE_systemc_ctor_BLOCK) }
	|	BACKQUOTE_systemc_dtor_BLOCK { (BACKQUOTE_systemc_dtor_BLOCK) }
	|	BACKQUOTE_systemc_interface_BLOCK { (BACKQUOTE_systemc_interface_BLOCK) }
	|	BACKQUOTE_systemc_implementation_BLOCK { (BACKQUOTE_systemc_implementation_BLOCK) }
	|	BACKQUOTE_systemc_imp_header_BLOCK { (BACKQUOTE_systemc_imp_header_BLOCK) }
	|	SLASH_STAR_verilator_hier_block_STAR_SLASH { (SLASH_STAR_verilator_hier_block_STAR_SLASH) }
	|	SLASH_STAR_verilator_inline_module_STAR_SLASH { (SLASH_STAR_verilator_inline_module_STAR_SLASH) }
	|	SLASH_STAR_verilator_no_inline_module_STAR_SLASH { (SLASH_STAR_verilator_no_inline_module_STAR_SLASH) }
	|	SLASH_STAR_verilator_public_module_STAR_SLASH { (SLASH_STAR_verilator_public_module_STAR_SLASH) }

module_or_generate_item: Defparam list_of_defparam_assignments SEMICOLON { TUPLE4(STRING("module_or_generate_item408"),Defparam,$2,SEMICOLON) }
	|	combinational_body { ($1) }
	|	module_common_item { ($1) }

module_common_item: module_or_generate_item_declaration { ($1) }
	|	etcInst { ($1) }
	|	assertion_item { ($1) }
	|	bind_directive { ($1) }
	|	continuous_assign { ($1) }
	|	Alias variable_lvalue aliasEqList SEMICOLON { TUPLE5(STRING("module_common_item416"),Alias,$2,$3,SEMICOLON) }
	|	initial_construct { ($1) }
	|	final_construct { ($1) }
	|	Always stmtBlock { TUPLE3(STRING("module_common_item419"),Always,$2) }
	|	Always_ff stmtBlock { TUPLE3(STRING("module_common_item420"),Always_ff,$2) }
	|	Always_latch stmtBlock { TUPLE3(STRING("module_common_item421"),Always_latch,$2) }
	|	Always_comb stmtBlock { TUPLE3(STRING("module_common_item422"),Always_comb,$2) }
	|	loop_generate_construct { ($1) }
	|	conditional_generate_construct { ($1) }
	|	elaboration_system_task { ($1) }
	|	ERROR_TOKEN SEMICOLON { TUPLE3(STRING("module_common_item426"),ERROR_TOKEN,SEMICOLON) }

continuous_assign: Assign strengthSpecE delayE assignList SEMICOLON { TUPLE6(STRING("continuous_assign427"),Assign,$2,$3,$4,SEMICOLON) }

initial_construct: Initial stmtBlock { TUPLE3(STRING("initial_construct428"),Initial,$2) }

final_construct: Final stmtBlock { TUPLE3(STRING("final_construct429"),Final,$2) }

module_or_generate_item_declaration: package_or_generate_item_declaration { ($1) }
	|	genvar_declaration { ($1) }
	|	clocking_declaration { ($1) }
	|	Default Clocking idAny SEMICOLON { TUPLE5(STRING("module_or_generate_item_declaration433"),Default,Clocking,$3,SEMICOLON) }

aliasEqList: EQUALS variable_lvalue { CONS2(EQUALS,$2) }
	|	aliasEqList EQUALS variable_lvalue { CONS3($1,EQUALS,$3) }

bind_directive: Bind bind_target_instance bind_instantiation { TUPLE4(STRING("bind_directive436"),Bind,$2,$3) }
	|	Bind bind_target_instance COLON bind_target_instance_list bind_instantiation { TUPLE6(STRING("bind_directive437"),Bind,$2,COLON,$4,$5) }

bind_target_instance_list: bind_target_instance { ($1) }
	|	bind_target_instance_list COMMA bind_target_instance { TUPLE4(STRING("bind_target_instance_list439"),$1,COMMA,$3) }

bind_target_instance: idAny { ($1) }

bind_instantiation: instDecl { ($1) }

generate_region: Generate genItemList Endgenerate { TUPLE4(STRING("generate_region442"),Generate,$2,Endgenerate) }
	|	Generate Endgenerate { TUPLE3(STRING("generate_region443"),Generate,Endgenerate) }

generate_block_or_null: generate_item { ($1) }
	|	genItemBegin { ($1) }

genItemBegin: Begin genItemList End { TUPLE4(STRING("genItemBegin446"),Begin,$2,End) }
	|	Begin End { TUPLE3(STRING("genItemBegin447"),Begin,End) }
	|	id COLON_HYPHEN_begin Begin genItemList End endLabelE { TUPLE7(STRING("genItemBegin448"),$1,COLON_HYPHEN_begin,Begin,$4,End,$6) }
	|	id COLON_HYPHEN_begin Begin End endLabelE { TUPLE6(STRING("genItemBegin449"),$1,COLON_HYPHEN_begin,Begin,End,$5) }
	|	Begin COLON idAny genItemList End endLabelE { TUPLE7(STRING("genItemBegin450"),Begin,COLON,$3,$4,End,$6) }
	|	Begin COLON idAny End endLabelE { TUPLE6(STRING("genItemBegin451"),Begin,COLON,$3,End,$5) }

genItemOrBegin: generate_item { ($1) }
	|	genItemBegin { ($1) }

genItemList: genItemOrBegin { CONS1 ($1) }
	|	genItemList genItemOrBegin { CONS2($1,$2) }

generate_item: module_or_generate_item { ($1) }

conditional_generate_construct: Case LPAREN expr RPAREN case_generate_itemListE Endcase { TUPLE7(STRING("conditional_generate_construct457"),Case,LPAREN,$3,RPAREN,$5,Endcase) }
	|	If LPAREN expr RPAREN generate_block_or_null { TUPLE6(STRING("conditional_generate_construct458"),If,LPAREN,$3,RPAREN,$5) }
	|	If LPAREN expr RPAREN generate_block_or_null Else generate_block_or_null { TUPLE8(STRING("conditional_generate_construct459"),If,LPAREN,$3,RPAREN,$5,Else,$7) }

loop_generate_construct: For LPAREN genvar_initialization SEMICOLON expr SEMICOLON genvar_iteration RPAREN generate_block_or_null { TUPLE10(STRING("loop_generate_construct460"),For,LPAREN,$3,SEMICOLON,$5,SEMICOLON,$7,RPAREN,$9) }

genvar_initialization: varRefBase EQUALS expr { TUPLE4(STRING("genvar_initialization461"),$1,EQUALS,$3) }
	|	Genvar genvar_identifierDecl EQUALS constExpr { TUPLE5(STRING("genvar_initialization462"),Genvar,$2,EQUALS,$4) }

genvar_iteration: varRefBase EQUALS expr { TUPLE4(STRING("genvar_iteration463"),$1,EQUALS,$3) }
	|	varRefBase PLUS_EQ expr { TUPLE4(STRING("genvar_iteration464"),$1,PLUS_EQ,$3) }
	|	varRefBase HYPHEN_EQ expr { TUPLE4(STRING("genvar_iteration465"),$1,HYPHEN_EQ,$3) }
	|	varRefBase STAR_EQ expr { TUPLE4(STRING("genvar_iteration466"),$1,STAR_EQ,$3) }
	|	varRefBase SLASH_EQ expr { TUPLE4(STRING("genvar_iteration467"),$1,SLASH_EQ,$3) }
	|	varRefBase PERCENT_EQ expr { TUPLE4(STRING("genvar_iteration468"),$1,PERCENT_EQ,$3) }
	|	varRefBase AMPERSAND_EQ expr { TUPLE4(STRING("genvar_iteration469"),$1,AMPERSAND_EQ,$3) }
	|	varRefBase VBAR_EQ expr { TUPLE4(STRING("genvar_iteration470"),$1,VBAR_EQ,$3) }
	|	varRefBase CARET_EQ expr { TUPLE4(STRING("genvar_iteration471"),$1,CARET_EQ,$3) }
	|	varRefBase LT_LT_EQ expr { TUPLE4(STRING("genvar_iteration472"),$1,LT_LT_EQ,$3) }
	|	varRefBase GT_GT_EQ expr { TUPLE4(STRING("genvar_iteration473"),$1,GT_GT_EQ,$3) }
	|	varRefBase GT_GT_GT_EQ expr { TUPLE4(STRING("genvar_iteration474"),$1,GT_GT_GT_EQ,$3) }
	|	PLUS_PLUS varRefBase { TUPLE3(STRING("genvar_iteration475"),PLUS_PLUS,$2) }
	|	HYPHEN_HYPHEN varRefBase { TUPLE3(STRING("genvar_iteration476"),HYPHEN_HYPHEN,$2) }
	|	varRefBase PLUS_PLUS { TUPLE3(STRING("genvar_iteration477"),$1,PLUS_PLUS) }
	|	varRefBase HYPHEN_HYPHEN { TUPLE3(STRING("genvar_iteration478"),$1,HYPHEN_HYPHEN) }

case_generate_itemListE: /* empty */ { EMPTY_TOKEN }
	|	case_generate_itemList { ($1) }

case_generate_itemList: case_generate_item { CONS1 ($1) }
	|	case_generate_itemList case_generate_item { CONS2($1,$2) }

case_generate_item: caseCondList colon generate_block_or_null { TUPLE4(STRING("case_generate_item483"),$1,$2,$3) }
	|	Default colon generate_block_or_null { TUPLE4(STRING("case_generate_item484"),Default,$2,$3) }
	|	Default generate_block_or_null { TUPLE3(STRING("case_generate_item485"),Default,$2) }

assignList: assignOne { CONS1 ($1) }
	|	assignList COMMA assignOne { CONS3($1,COMMA,$3) }

assignOne: variable_lvalue EQUALS expr { TUPLE4(STRING("assignOne488"),$1,EQUALS,$3) }

delayE: /* empty */ { EMPTY_TOKEN }
	|	delay { ($1) }

delay: delay_control { ($1) }

delay_control: HASH delay_value { TUPLE3(STRING("delay_control492"),HASH,$2) }
	|	HASH LPAREN minTypMax RPAREN { TUPLE5(STRING("delay_control493"),HASH,LPAREN,$3,RPAREN) }
	|	HASH LPAREN minTypMax COMMA minTypMax RPAREN { TUPLE7(STRING("delay_control494"),HASH,LPAREN,$3,COMMA,$5,RPAREN) }
	|	HASH LPAREN minTypMax COMMA minTypMax COMMA minTypMax RPAREN { TUPLE9(STRING("delay_control495"),HASH,LPAREN,$3,COMMA,$5,COMMA,$7,RPAREN) }

delay_value: packageClassScopeE varRefBase { TUPLE3(STRING("delay_value496"),$1,$2) }
	|	INTEGER_NUMBER { (INTEGER_NUMBER $1) }
	|	FLOATING_HYPHEN_POINT_NUMBER { (FLOATING_HYPHEN_POINT_NUMBER $1) }
	|	timeNumAdjusted { ($1) }

delayExpr: expr { ($1) }

minTypMax: delayExpr { ($1) }
	|	delayExpr COLON delayExpr COLON delayExpr { TUPLE6(STRING("minTypMax502"),$1,COLON,$3,COLON,$5) }

netSigList: netSig { CONS1 ($1) }
	|	netSigList COMMA netSig { CONS3($1,COMMA,$3) }

netSig: netId sigAttrListE { TUPLE3(STRING("netSig505"),$1,$2) }
	|	netId sigAttrListE EQUALS expr { TUPLE5(STRING("netSig506"),$1,$2,EQUALS,$4) }
	|	netId variable_dimensionList sigAttrListE { TUPLE4(STRING("netSig507"),$1,$2,$3) }

netId: id { ($1) }
	|	idSVKwd { ($1) }

sigAttrListE: /* empty */ { EMPTY_TOKEN }
	|	sigAttrList { ($1) }

sigAttrList: sigAttr { CONS1 ($1) }
	|	sigAttrList sigAttr { CONS2($1,$2) }

sigAttr: SLASH_STAR_verilator_clocker_STAR_SLASH { (SLASH_STAR_verilator_clocker_STAR_SLASH) }
	|	SLASH_STAR_verilator_no_clocker_STAR_SLASH { (SLASH_STAR_verilator_no_clocker_STAR_SLASH) }
	|	SLASH_STAR_verilator_clock_enable_STAR_SLASH { (SLASH_STAR_verilator_clock_enable_STAR_SLASH) }
	|	SLASH_STAR_verilator_public_STAR_SLASH { (SLASH_STAR_verilator_public_STAR_SLASH) }
	|	SLASH_STAR_verilator_public_flat_STAR_SLASH { (SLASH_STAR_verilator_public_flat_STAR_SLASH) }
	|	SLASH_STAR_verilator_public_flat_rd_STAR_SLASH { (SLASH_STAR_verilator_public_flat_rd_STAR_SLASH) }
	|	SLASH_STAR_verilator_public_flat_rw_STAR_SLASH { (SLASH_STAR_verilator_public_flat_rw_STAR_SLASH) }
	|	SLASH_STAR_verilator_public_flat_rw_STAR_SLASH attr_event_control { TUPLE3(STRING("sigAttr521"),SLASH_STAR_verilator_public_flat_rw_STAR_SLASH,$2) }
	|	SLASH_STAR_verilator_isolate_assignments_STAR_SLASH { (SLASH_STAR_verilator_isolate_assignments_STAR_SLASH) }
	|	SLASH_STAR_verilator_sc_bv_STAR_SLASH { (SLASH_STAR_verilator_sc_bv_STAR_SLASH) }
	|	SLASH_STAR_verilator_sformat_STAR_SLASH { (SLASH_STAR_verilator_sformat_STAR_SLASH) }
	|	SLASH_STAR_verilator_split_var_STAR_SLASH { (SLASH_STAR_verilator_split_var_STAR_SLASH) }

rangeListE: /* empty */ { EMPTY_TOKEN }
	|	rangeList { ($1) }

rangeList: anyrange { CONS1 ($1) }
	|	rangeList anyrange { CONS2($1,$2) }

anyrange: LBRACK constExpr COLON constExpr RBRACK { TUPLE6(STRING("anyrange530"),LBRACK,$2,COLON,$4,RBRACK) }

packed_dimensionListE: /* empty */ { EMPTY_TOKEN }
	|	packed_dimensionList { ($1) }

packed_dimensionList: packed_dimension { CONS1 ($1) }
	|	packed_dimensionList packed_dimension { CONS2($1,$2) }

packed_dimension: anyrange { ($1) }
	|	LBRACK RBRACK { TUPLE3(STRING("packed_dimension536"),LBRACK,RBRACK) }

param_assignment: id variable_dimensionListE sigAttrListE exprOrDataTypeEqE { TUPLE5(STRING("param_assignment537"),$1,$2,$3,$4) }

list_of_param_assignments: param_assignment { CONS1 ($1) }
	|	list_of_param_assignments COMMA param_assignment { CONS3($1,COMMA,$3) }

type_assignment: idAny sigAttrListE EQUALS data_type { TUPLE5(STRING("type_assignment540"),type_decl $1,$2,EQUALS,$4) }

list_of_type_assignments: type_assignment { CONS1 ($1) }
	|	list_of_type_assignments COMMA type_assignment { CONS3($1,COMMA,$3) }

list_of_defparam_assignments: defparam_assignment { CONS1 ($1) }
	|	list_of_defparam_assignments COMMA defparam_assignment { CONS3($1,COMMA,$3) }

defparam_assignment: idAny DOT idAny EQUALS expr { TUPLE6(STRING("defparam_assignment545"),$1,DOT,$3,EQUALS,$5) }
	|	idAny DOT idAny DOT { TUPLE5(STRING("defparam_assignment546"),$1,DOT,$3,DOT) }

etcInst: instDecl { ($1) }
	|	gateDecl { ($1) }

instDecl: id parameter_value_assignmentE /* 18 */ instnameList SEMICOLON { TUPLE5(STRING("instDecl550"),$1,$2,$3,SEMICOLON) }
	|	id DOT id /* 19 */ mpInstnameList SEMICOLON { TUPLE6(STRING("instDecl552"),$1,DOT,$3,$4,SEMICOLON) }

mpInstnameList: mpInstnameParen { CONS1 ($1) }
	|	mpInstnameList COMMA mpInstnameParen { CONS3($1,COMMA,$3) }

mpInstnameParen: id instRangeListE sigAttrListE { TUPLE4(STRING("mpInstnameParen555"),$1,$2,$3) }

instnameList: instnameParen { CONS1 ($1) }
	|	instnameList COMMA instnameParen { CONS3($1,COMMA,$3) }

instnameParen: id instRangeListE LPAREN cellpinList RPAREN { TUPLE6(STRING("instnameParen558"),$1,$2,LPAREN,$4,RPAREN) }
	|	id instRangeListE { TUPLE3(STRING("instnameParen559"),$1,$2) }

instRangeListE: /* empty */ { EMPTY_TOKEN }
	|	instRangeList { ($1) }

instRangeList: instRange { CONS1 ($1) }
	|	instRangeList instRange { CONS2($1,$2) }

instRange: LBRACK constExpr RBRACK { TUPLE4(STRING("instRange564"),LBRACK,$2,RBRACK) }
	|	LBRACK constExpr COLON constExpr RBRACK { TUPLE6(STRING("instRange565"),LBRACK,$2,COLON,$4,RBRACK) }

cellparamList: /* 20 */ cellparamItList { CONS1 ($1) }

cellpinList: /* 21 */ cellpinItList { CONS1 ($1) }

cellparamItList: cellparamItemE { CONS1 ($1) }
	|	cellparamItList COMMA cellparamItemE { CONS3($1,COMMA,$3) }

cellpinItList: cellpinItemE { CONS1 ($1) }
	|	cellpinItList COMMA cellpinItemE { CONS3($1,COMMA,$3) }

cellparamItemE: /* empty */ { EMPTY_TOKEN }
	|	DOT_STAR { (DOT_STAR) }
	|	DOT idSVKwd { TUPLE3(STRING("cellparamItemE576"),DOT,$2) }
	|	DOT idAny { TUPLE3(STRING("cellparamItemE577"),DOT,$2) }
	|	DOT idAny LPAREN RPAREN { TUPLE5(STRING("cellparamItemE578"),DOT,$2,LPAREN,RPAREN) }
	|	DOT idAny LPAREN exprOrDataType RPAREN { TUPLE6(STRING("cellparamItemE579"),DOT,$2,LPAREN,$4,RPAREN) }
	|	exprOrDataType { ($1) }

cellpinItemE: /* empty */ { EMPTY_TOKEN }
	|	DOT_STAR { (DOT_STAR) }
	|	DOT idSVKwd { TUPLE3(STRING("cellpinItemE583"),DOT,$2) }
	|	DOT idAny { TUPLE3(STRING("cellpinItemE584"),DOT,$2) }
	|	DOT idAny LPAREN RPAREN { TUPLE5(STRING("cellpinItemE585"),DOT,$2,LPAREN,RPAREN) }
	|	DOT idAny LPAREN expr RPAREN { TUPLE6(STRING("cellpinItemE586"),DOT,$2,LPAREN,$4,RPAREN) }
	|	expr { ($1) }

attr_event_controlE: /* empty */ { EMPTY_TOKEN }
	|	attr_event_control { ($1) }

attr_event_control: AT LPAREN event_expression RPAREN { TUPLE5(STRING("attr_event_control590"),AT,LPAREN,$3,RPAREN) }
	|	AT LPAREN STAR RPAREN { TUPLE5(STRING("attr_event_control591"),AT,LPAREN,STAR,RPAREN) }
	|	AT STAR { TUPLE3(STRING("attr_event_control592"),AT,STAR) }

event_control: AT LPAREN event_expression RPAREN { TUPLE5(STRING("event_control593"),AT,LPAREN,$3,RPAREN) }
	|	AT LPAREN STAR RPAREN { TUPLE5(STRING("event_control594"),AT,LPAREN,STAR,RPAREN) }
	|	AT STAR { TUPLE3(STRING("event_control595"),AT,STAR) }
	|	AT senitemVar { TUPLE3(STRING("event_control596"),AT,$2) }

event_expression: senitem { ($1) }
	|	event_expression Or senitem { TUPLE4(STRING("event_expression598"),$1,Or,$3) }
	|	event_expression COMMA senitem { TUPLE4(STRING("event_expression599"),$1,COMMA,$3) }

senitem: senitemEdge { ($1) }
	|	senitemVar { ($1) }
	|	LPAREN senitem RPAREN { TUPLE4(STRING("senitem602"),LPAREN,$2,RPAREN) }
	|	LBRACE event_expression RBRACE { TUPLE4(STRING("senitem603"),LBRACE,$2,RBRACE) }
	|	senitem AMPERSAND_AMPERSAND senitem { TUPLE4(STRING("senitem604"),$1,AMPERSAND_AMPERSAND,$3) }
	|	INTEGER_NUMBER { (INTEGER_NUMBER $1) }
	|	FLOATING_HYPHEN_POINT_NUMBER { (FLOATING_HYPHEN_POINT_NUMBER $1) }

senitemVar: idClassSel { ($1) }

senitemEdge: Posedge idClassSel { TUPLE3(STRING("senitemEdge608"),Posedge,$2) }
	|	Negedge idClassSel { TUPLE3(STRING("senitemEdge609"),Negedge,$2) }
	|	Edge idClassSel { TUPLE3(STRING("senitemEdge610"),Edge,$2) }
	|	Posedge LPAREN idClassSel RPAREN { TUPLE5(STRING("senitemEdge611"),Posedge,LPAREN,$3,RPAREN) }
	|	Negedge LPAREN idClassSel RPAREN { TUPLE5(STRING("senitemEdge612"),Negedge,LPAREN,$3,RPAREN) }
	|	Edge LPAREN idClassSel RPAREN { TUPLE5(STRING("senitemEdge613"),Edge,LPAREN,$3,RPAREN) }

stmtBlock: stmt { ($1) }

seq_block: seq_blockFront blockDeclStmtListE End endLabelE { TUPLE5(STRING("seq_block615"),$1,$2,End,$4) }

seq_blockPreId: seq_blockFrontPreId blockDeclStmtListE End endLabelE { TUPLE5(STRING("seq_blockPreId616"),$1,$2,End,$4) }

par_block: par_blockFront blockDeclStmtListE Join endLabelE { TUPLE5(STRING("par_block617"),$1,$2,Join,$4) }
	|	par_blockFront blockDeclStmtListE Join_any endLabelE { TUPLE5(STRING("par_block618"),$1,$2,Join_any,$4) }
	|	par_blockFront blockDeclStmtListE Join_none endLabelE { TUPLE5(STRING("par_block619"),$1,$2,Join_none,$4) }

par_blockPreId: par_blockFrontPreId blockDeclStmtListE Join endLabelE { TUPLE5(STRING("par_blockPreId620"),$1,$2,Join,$4) }
	|	par_blockFrontPreId blockDeclStmtListE Join_any endLabelE { TUPLE5(STRING("par_blockPreId621"),$1,$2,Join_any,$4) }
	|	par_blockFrontPreId blockDeclStmtListE Join_none endLabelE { TUPLE5(STRING("par_blockPreId622"),$1,$2,Join_none,$4) }

seq_blockFront: Begin { (Begin) }
	|	Begin COLON idAny { TUPLE4(STRING("seq_blockFront624"),Begin,COLON,$3) }

par_blockFront: Fork { (Fork) }
	|	Fork COLON idAny { TUPLE4(STRING("par_blockFront626"),Fork,COLON,$3) }

seq_blockFrontPreId: id COLON_HYPHEN_begin Begin { TUPLE4(STRING("seq_blockFrontPreId627"),$1,COLON_HYPHEN_begin,Begin) }

par_blockFrontPreId: id COLON_HYPHEN_fork Fork { TUPLE4(STRING("par_blockFrontPreId628"),$1,COLON_HYPHEN_fork,Fork) }

blockDeclStmtList: block_item_declarationList { CONS1 ($1) }
	|	block_item_declarationList stmtList { CONS2($1,$2) }
	|	stmtList { CONS1 ($1) }

blockDeclStmtListE: /* empty */ { EMPTY_TOKEN }
	|	blockDeclStmtList { ($1) }

block_item_declarationList: block_item_declaration { CONS1 ($1) }
	|	block_item_declarationList block_item_declaration { CONS2($1,$2) }

block_item_declaration: data_declaration { ($1) }
	|	parameter_declaration SEMICOLON { TUPLE3(STRING("block_item_declaration637"),$1,SEMICOLON) }

stmtList: stmtBlock { CONS1 ($1) }
	|	stmtList stmtBlock { CONS2($1,$2) }

stmt: statement_item { ($1) }
	|	id COLON statement_item { TUPLE4(STRING("stmt641"),$1,COLON,$3) }
	|	SEMICOLON { (SEMICOLON) }
	|	seq_blockPreId { ($1) }
	|	par_blockPreId { ($1) }

statement_item: foperator_assignment SEMICOLON { TUPLE3(STRING("statement_item645"),$1,SEMICOLON) }
	|	fexprLvalue EQUALS class_new SEMICOLON { TUPLE5(STRING("statement_item646"),$1,EQUALS,$3,SEMICOLON) }
	|	fexprLvalue EQUALS dynamic_array_new SEMICOLON { TUPLE5(STRING("statement_item647"),$1,EQUALS,$3,SEMICOLON) }
	|	fexprLvalue LT_EQ delayE expr SEMICOLON { TUPLE6(STRING("statement_item648"),$1,LT_EQ,$3,$4,SEMICOLON) }
	|	Assign idClassSel EQUALS delayE expr SEMICOLON { TUPLE7(STRING("statement_item649"),Assign,$2,EQUALS,$4,$5,SEMICOLON) }
	|	Deassign variable_lvalue SEMICOLON { TUPLE4(STRING("statement_item650"),Deassign,$2,SEMICOLON) }
	|	Force expr EQUALS expr SEMICOLON { TUPLE6(STRING("statement_item651"),Force,$2,EQUALS,$4,SEMICOLON) }
	|	Release variable_lvalue SEMICOLON { TUPLE4(STRING("statement_item652"),Release,$2,SEMICOLON) }
	|	unique_priorityE caseStart caseAttrE case_itemListE Endcase { TUPLE6(STRING("statement_item653"),$1,$2,$3,$4,Endcase) }
	|	unique_priorityE caseStart caseAttrE Inside case_insideListE Endcase { TUPLE7(STRING("statement_item654"),$1,$2,$3,Inside,$5,Endcase) }
	|	unique_priorityE If LPAREN expr RPAREN stmtBlock { TUPLE7(STRING("statement_item655"),$1,If,LPAREN,$4,RPAREN,$6) }
	|	unique_priorityE If LPAREN expr RPAREN stmtBlock Else stmtBlock { TUPLE9(STRING("statement_item656"),$1,If,LPAREN,$4,RPAREN,$6,Else,$8) }
	|	finc_or_dec_expression SEMICOLON { TUPLE3(STRING("statement_item657"),$1,SEMICOLON) }
	|	Void QUOTE LPAREN task_subroutine_callNoMethod RPAREN SEMICOLON { TUPLE7(STRING("statement_item658"),Void,QUOTE,LPAREN,$4,RPAREN,SEMICOLON) }
	|	Void QUOTE LPAREN expr DOT task_subroutine_callNoMethod RPAREN SEMICOLON { TUPLE9(STRING("statement_item659"),Void,QUOTE,LPAREN,$4,DOT,$6,RPAREN,SEMICOLON) }
	|	task_subroutine_callNoMethod SEMICOLON { TUPLE3(STRING("statement_item660"),$1,SEMICOLON) }
	|	fexpr DOT task_subroutine_callNoMethod SEMICOLON { TUPLE5(STRING("statement_item661"),$1,DOT,$3,SEMICOLON) }
	|	fexpr DOT class_new SEMICOLON { TUPLE5(STRING("statement_item662"),$1,DOT,$3,SEMICOLON) }
	|	statementVerilatorPragmas { ($1) }
	|	Disable idAny SEMICOLON { TUPLE4(STRING("statement_item664"),Disable,$2,SEMICOLON) }
	|	Disable Fork SEMICOLON { TUPLE4(STRING("statement_item665"),Disable,Fork,SEMICOLON) }
	|	HYPHEN_GT idDotted SEMICOLON { TUPLE4(STRING("statement_item666"),HYPHEN_GT,$2,SEMICOLON) }
	|	HYPHEN_GT_GT delayE idDotted SEMICOLON { TUPLE5(STRING("statement_item667"),HYPHEN_GT_GT,$2,$3,SEMICOLON) }
	|	Forever stmtBlock { TUPLE3(STRING("statement_item668"),Forever,$2) }
	|	Repeat LPAREN expr RPAREN stmtBlock { TUPLE6(STRING("statement_item669"),Repeat,LPAREN,$3,RPAREN,$5) }
	|	While LPAREN expr RPAREN stmtBlock { TUPLE6(STRING("statement_item670"),While,LPAREN,$3,RPAREN,$5) }
	|	statementFor { ($1) }
	|	Do stmtBlock While LPAREN expr RPAREN SEMICOLON { TUPLE8(STRING("statement_item672"),Do,$2,While,LPAREN,$5,RPAREN,SEMICOLON) }
	|	Foreach LPAREN idClassSelForeach RPAREN stmtBlock { TUPLE6(STRING("statement_item673"),Foreach,LPAREN,$3,RPAREN,$5) }
	|	Return SEMICOLON { TUPLE3(STRING("statement_item674"),Return,SEMICOLON) }
	|	Return expr SEMICOLON { TUPLE4(STRING("statement_item675"),Return,$2,SEMICOLON) }
	|	Break SEMICOLON { TUPLE3(STRING("statement_item676"),Break,SEMICOLON) }
	|	Continue SEMICOLON { TUPLE3(STRING("statement_item677"),Continue,SEMICOLON) }
	|	par_block { ($1) }
	|	delay_control stmtBlock { TUPLE3(STRING("statement_item679"),$1,$2) }
	|	event_control stmtBlock { TUPLE3(STRING("statement_item680"),$1,$2) }
	|	seq_block { ($1) }
	|	Wait LPAREN expr RPAREN stmtBlock { TUPLE6(STRING("statement_item682"),Wait,LPAREN,$3,RPAREN,$5) }
	|	Wait Fork SEMICOLON { TUPLE4(STRING("statement_item683"),Wait,Fork,SEMICOLON) }
	|	procedural_assertion_statement { ($1) }
	|	Randcase case_itemList Endcase { TUPLE4(STRING("statement_item685"),Randcase,$2,Endcase) }
	|	ERROR_TOKEN SEMICOLON { TUPLE3(STRING("statement_item686"),ERROR_TOKEN,SEMICOLON) }

statementFor: For LPAREN for_initialization expr SEMICOLON for_stepE RPAREN stmtBlock { TUPLE9(STRING("statementFor687"),For,LPAREN,$3,$4,SEMICOLON,$6,RPAREN,$8) }
	|	For LPAREN for_initialization SEMICOLON for_stepE RPAREN stmtBlock { TUPLE8(STRING("statementFor688"),For,LPAREN,$3,SEMICOLON,$5,RPAREN,$7) }

statementVerilatorPragmas: SLASH_STAR_verilator_coverage_block_off_STAR_SLASH { (SLASH_STAR_verilator_coverage_block_off_STAR_SLASH) }

foperator_assignment: fexprLvalue EQUALS delayE expr { TUPLE5(STRING("foperator_assignment690"),$1,EQUALS,$3,$4) }
	|	fexprLvalue EQUALS DLR_fopen LPAREN expr RPAREN { TUPLE7(STRING("foperator_assignment691"),$1,EQUALS,DLR_fopen,LPAREN,$5,RPAREN) }
	|	fexprLvalue EQUALS DLR_fopen LPAREN expr COMMA expr RPAREN { TUPLE9(STRING("foperator_assignment692"),$1,EQUALS,DLR_fopen,LPAREN,$5,COMMA,$7,RPAREN) }
	|	fexprLvalue PLUS_EQ expr { TUPLE4(STRING("foperator_assignment693"),$1,PLUS_EQ,$3) }
	|	fexprLvalue HYPHEN_EQ expr { TUPLE4(STRING("foperator_assignment694"),$1,HYPHEN_EQ,$3) }
	|	fexprLvalue STAR_EQ expr { TUPLE4(STRING("foperator_assignment695"),$1,STAR_EQ,$3) }
	|	fexprLvalue SLASH_EQ expr { TUPLE4(STRING("foperator_assignment696"),$1,SLASH_EQ,$3) }
	|	fexprLvalue PERCENT_EQ expr { TUPLE4(STRING("foperator_assignment697"),$1,PERCENT_EQ,$3) }
	|	fexprLvalue AMPERSAND_EQ expr { TUPLE4(STRING("foperator_assignment698"),$1,AMPERSAND_EQ,$3) }
	|	fexprLvalue VBAR_EQ expr { TUPLE4(STRING("foperator_assignment699"),$1,VBAR_EQ,$3) }
	|	fexprLvalue CARET_EQ expr { TUPLE4(STRING("foperator_assignment700"),$1,CARET_EQ,$3) }
	|	fexprLvalue LT_LT_EQ expr { TUPLE4(STRING("foperator_assignment701"),$1,LT_LT_EQ,$3) }
	|	fexprLvalue GT_GT_EQ expr { TUPLE4(STRING("foperator_assignment702"),$1,GT_GT_EQ,$3) }
	|	fexprLvalue GT_GT_GT_EQ expr { TUPLE4(STRING("foperator_assignment703"),$1,GT_GT_GT_EQ,$3) }

inc_or_dec_expression: exprScope PLUS_PLUS { TUPLE3(STRING("inc_or_dec_expression704"),$1,PLUS_PLUS) }
	|	exprScope HYPHEN_HYPHEN { TUPLE3(STRING("inc_or_dec_expression705"),$1,HYPHEN_HYPHEN) }
	|	PLUS_PLUS expr { TUPLE3(STRING("inc_or_dec_expression706"),PLUS_PLUS,$2) }
	|	HYPHEN_HYPHEN expr { TUPLE3(STRING("inc_or_dec_expression707"),HYPHEN_HYPHEN,$2) }

finc_or_dec_expression: fexprScope PLUS_PLUS { TUPLE3(STRING("finc_or_dec_expression708"),$1,PLUS_PLUS) }
	|	fexprScope HYPHEN_HYPHEN { TUPLE3(STRING("finc_or_dec_expression709"),$1,HYPHEN_HYPHEN) }
	|	PLUS_PLUS expr { TUPLE3(STRING("finc_or_dec_expression710"),PLUS_PLUS,$2) }
	|	HYPHEN_HYPHEN expr { TUPLE3(STRING("finc_or_dec_expression711"),HYPHEN_HYPHEN,$2) }

class_new: New { (New) }
	|	New expr { TUPLE3(STRING("class_new713"),New,$2) }
	|	New_HYPHEN_then_HYPHEN_paren LPAREN list_of_argumentsE RPAREN { TUPLE5(STRING("class_new714"),New_HYPHEN_then_HYPHEN_paren,LPAREN,$3,RPAREN) }

dynamic_array_new: New LBRACK expr RBRACK { TUPLE5(STRING("dynamic_array_new715"),New,LBRACK,$3,RBRACK) }
	|	New LBRACK expr RBRACK LPAREN expr RPAREN { TUPLE8(STRING("dynamic_array_new716"),New,LBRACK,$3,RBRACK,LPAREN,$6,RPAREN) }

unique_priorityE: /* empty */ { EMPTY_TOKEN }
	|	Priority { (Priority) }
	|	Unique { (Unique) }
	|	Unique0 { (Unique0) }

caseStart: Case LPAREN expr RPAREN { TUPLE5(STRING("caseStart721"),Case,LPAREN,$3,RPAREN) }
	|	Casex LPAREN expr RPAREN { TUPLE5(STRING("caseStart722"),Casex,LPAREN,$3,RPAREN) }
	|	Casez LPAREN expr RPAREN { TUPLE5(STRING("caseStart723"),Casez,LPAREN,$3,RPAREN) }

caseAttrE: /* empty */ { EMPTY_TOKEN }
	|	caseAttrE SLASH_STAR_verilator_full_case_STAR_SLASH { TUPLE3(STRING("caseAttrE725"),$1,SLASH_STAR_verilator_full_case_STAR_SLASH) }
	|	caseAttrE SLASH_STAR_verilator_parallel_case_STAR_SLASH { TUPLE3(STRING("caseAttrE726"),$1,SLASH_STAR_verilator_parallel_case_STAR_SLASH) }

case_itemListE: /* empty */ { EMPTY_TOKEN }
	|	case_itemList { ($1) }

case_insideListE: /* empty */ { EMPTY_TOKEN }
	|	case_inside_itemList { ($1) }

case_itemList: caseCondList colon stmtBlock { CONS3($1,$2,$3) }
	|	Default colon stmtBlock { CONS3(Default,$2,$3) }
	|	Default stmtBlock { CONS2(Default,$2) }
	|	case_itemList caseCondList colon stmtBlock { CONS4($1,$2,$3,$4) }
	|	case_itemList Default stmtBlock { CONS3($1,Default,$3) }
	|	case_itemList Default colon stmtBlock { CONS4($1,Default,$3,$4) }

case_inside_itemList: open_range_list colon stmtBlock { CONS3($1,$2,$3) }
	|	Default colon stmtBlock { CONS3(Default,$2,$3) }
	|	Default stmtBlock { CONS2(Default,$2) }
	|	case_inside_itemList open_range_list colon stmtBlock { CONS4($1,$2,$3,$4) }
	|	case_inside_itemList Default stmtBlock { CONS3($1,Default,$3) }
	|	case_inside_itemList Default colon stmtBlock { CONS4($1,Default,$3,$4) }

open_range_list: open_value_range { ($1) }
	|	open_range_list COMMA open_value_range { TUPLE4(STRING("open_range_list744"),$1,COMMA,$3) }

open_value_range: value_range { ($1) }

value_range: expr { ($1) }
	|	LBRACK expr COLON expr RBRACK { TUPLE6(STRING("value_range747"),LBRACK,$2,COLON,$4,RBRACK) }

caseCondList: expr { CONS1 ($1) }
	|	caseCondList COMMA expr { CONS3($1,COMMA,$3) }

patternNoExpr: DOT id { TUPLE3(STRING("patternNoExpr750"),DOT,$2) }
	|	DOT_STAR { (DOT_STAR) }

patternList: patternOne { CONS1 ($1) }
	|	patternList COMMA patternOne { CONS3($1,COMMA,$3) }

patternOne: expr { ($1) }
	|	expr LBRACE argsExprList RBRACE { TUPLE5(STRING("patternOne755"),$1,LBRACE,$3,RBRACE) }
	|	patternNoExpr { ($1) }

patternMemberList: patternMemberOne { CONS1 ($1) }
	|	patternMemberList COMMA patternMemberOne { CONS3($1,COMMA,$3) }

patternMemberOne: patternKey COLON expr { TUPLE4(STRING("patternMemberOne759"),$1,COLON,$3) }
	|	patternKey COLON patternNoExpr { TUPLE4(STRING("patternMemberOne760"),$1,COLON,$3) }
	|	Default COLON expr { TUPLE4(STRING("patternMemberOne761"),Default,COLON,$3) }
	|	Default COLON patternNoExpr { TUPLE4(STRING("patternMemberOne762"),Default,COLON,$3) }

patternKey: INTEGER_NUMBER { (INTEGER_NUMBER $1) }
	|	FLOATING_HYPHEN_POINT_NUMBER { (FLOATING_HYPHEN_POINT_NUMBER $1) }
	|	id { ($1) }
	|	strAsInt { ($1) }

assignment_pattern: QUOTE_LBRACE patternList RBRACE { TUPLE4(STRING("assignment_pattern767"),QUOTE_LBRACE,$2,RBRACE) }
	|	QUOTE_LBRACE patternMemberList RBRACE { TUPLE4(STRING("assignment_pattern768"),QUOTE_LBRACE,$2,RBRACE) }
	|	QUOTE_LBRACE RBRACE { TUPLE3(STRING("assignment_pattern769"),QUOTE_LBRACE,RBRACE) }

for_initialization: for_initializationItemList SEMICOLON { TUPLE3(STRING("for_initialization770"),$1,SEMICOLON) }
	|	SEMICOLON { (SEMICOLON) }

for_initializationItemList: for_initializationItem { CONS1 ($1) }
	|	for_initializationItemList COMMA for_initializationItem { CONS3($1,COMMA,$3) }

for_initializationItem: data_type idAny EQUALS expr { TUPLE5(STRING("for_initializationItem774"),$1,$2,EQUALS,$4) }
	|	Var data_type idAny EQUALS expr { TUPLE6(STRING("for_initializationItem775"),Var,$2,$3,EQUALS,$5) }
	|	varRefBase EQUALS expr { TUPLE4(STRING("for_initializationItem776"),$1,EQUALS,$3) }

for_stepE: /* empty */ { EMPTY_TOKEN }
	|	for_step { ($1) }

for_step: for_step_assignment { ($1) }
	|	for_step COMMA for_step_assignment { TUPLE4(STRING("for_step780"),$1,COMMA,$3) }

for_step_assignment: genvar_iteration { ($1) }

loop_variables: varRefBase { ($1) }
	|	loop_variables COMMA varRefBase { TUPLE4(STRING("loop_variables783"),$1,COMMA,$3) }

taskRef: id { ($1) }
	|	id LPAREN list_of_argumentsE RPAREN { TUPLE5(STRING("taskRef785"),$1,LPAREN,$3,RPAREN) }
	|	packageClassScope id LPAREN list_of_argumentsE RPAREN { TUPLE6(STRING("taskRef786"),$1,$2,LPAREN,$4,RPAREN) }

funcRef: id LPAREN list_of_argumentsE RPAREN { TUPLE5(STRING("funcRef787"),$1,LPAREN,$3,RPAREN) }
	|	packageClassScope id LPAREN list_of_argumentsE RPAREN { TUPLE6(STRING("funcRef788"),$1,$2,LPAREN,$4,RPAREN) }

task_subroutine_callNoMethod: taskRef { ($1) }
	|	funcRef With_HYPHEN_then_HYPHEN_LPAREN LPAREN expr RPAREN { TUPLE6(STRING("task_subroutine_callNoMethod790"),$1,With_HYPHEN_then_HYPHEN_LPAREN,LPAREN,$4,RPAREN) }
	|	id With_HYPHEN_then_HYPHEN_LPAREN LPAREN expr RPAREN { TUPLE6(STRING("task_subroutine_callNoMethod791"),$1,With_HYPHEN_then_HYPHEN_LPAREN,LPAREN,$4,RPAREN) }
	|	system_t_call { ($1) }

function_subroutine_callNoMethod: funcRef { ($1) }
	|	funcRef With_HYPHEN_then_HYPHEN_LPAREN LPAREN expr RPAREN { TUPLE6(STRING("function_subroutine_callNoMethod794"),$1,With_HYPHEN_then_HYPHEN_LPAREN,LPAREN,$4,RPAREN) }
	|	id With_HYPHEN_then_HYPHEN_LPAREN LPAREN expr RPAREN { TUPLE6(STRING("function_subroutine_callNoMethod795"),$1,With_HYPHEN_then_HYPHEN_LPAREN,LPAREN,$4,RPAREN) }
	|	system_f_call { ($1) }
	|	funcRef With_HYPHEN_then_HYPHEN_LBRACE constraint_block { TUPLE4(STRING("function_subroutine_callNoMethod797"),$1,With_HYPHEN_then_HYPHEN_LBRACE,$3) }
	|	funcRef With_HYPHEN_then_HYPHEN_LBRACE LBRACE RBRACE { TUPLE5(STRING("function_subroutine_callNoMethod798"),$1,With_HYPHEN_then_HYPHEN_LBRACE,LBRACE,RBRACE) }

system_t_call: DLR_LBRACE_pli_HYPHEN_system_RBRACE systemDpiArgsE { TUPLE3(STRING("system_t_call799"),DLR_LBRACE_pli_HYPHEN_system_RBRACE,$2) }
	|	DLR_dumpports LPAREN idDotted COMMA expr RPAREN { TUPLE7(STRING("system_t_call800"),DLR_dumpports,LPAREN,$3,COMMA,$5,RPAREN) }
	|	DLR_dumpports LPAREN COMMA expr RPAREN { TUPLE6(STRING("system_t_call801"),DLR_dumpports,LPAREN,COMMA,$4,RPAREN) }
	|	DLR_dumpfile LPAREN expr RPAREN { TUPLE5(STRING("system_t_call802"),DLR_dumpfile,LPAREN,$3,RPAREN) }
	|	DLR_dumpvars parenE { TUPLE3(STRING("system_t_call803"),DLR_dumpvars,$2) }
	|	DLR_dumpvars LPAREN expr RPAREN { TUPLE5(STRING("system_t_call804"),DLR_dumpvars,LPAREN,$3,RPAREN) }
	|	DLR_dumpvars LPAREN expr COMMA idDotted RPAREN { TUPLE7(STRING("system_t_call805"),DLR_dumpvars,LPAREN,$3,COMMA,$5,RPAREN) }
	|	DLR_dumpall parenE { TUPLE3(STRING("system_t_call806"),DLR_dumpall,$2) }
	|	DLR_dumpall LPAREN expr RPAREN { TUPLE5(STRING("system_t_call807"),DLR_dumpall,LPAREN,$3,RPAREN) }
	|	DLR_dumpflush parenE { TUPLE3(STRING("system_t_call808"),DLR_dumpflush,$2) }
	|	DLR_dumpflush LPAREN expr RPAREN { TUPLE5(STRING("system_t_call809"),DLR_dumpflush,LPAREN,$3,RPAREN) }
	|	DLR_dumplimit LPAREN expr RPAREN { TUPLE5(STRING("system_t_call810"),DLR_dumplimit,LPAREN,$3,RPAREN) }
	|	DLR_dumplimit LPAREN expr COMMA expr RPAREN { TUPLE7(STRING("system_t_call811"),DLR_dumplimit,LPAREN,$3,COMMA,$5,RPAREN) }
	|	DLR_dumpoff parenE { TUPLE3(STRING("system_t_call812"),DLR_dumpoff,$2) }
	|	DLR_dumpoff LPAREN expr RPAREN { TUPLE5(STRING("system_t_call813"),DLR_dumpoff,LPAREN,$3,RPAREN) }
	|	DLR_dumpon parenE { TUPLE3(STRING("system_t_call814"),DLR_dumpon,$2) }
	|	DLR_dumpon LPAREN expr RPAREN { TUPLE5(STRING("system_t_call815"),DLR_dumpon,LPAREN,$3,RPAREN) }
	|	DLR_c LPAREN cStrList RPAREN { TUPLE5(STRING("system_t_call816"),DLR_c,LPAREN,$3,RPAREN) }
	|	DLR_system LPAREN expr RPAREN { TUPLE5(STRING("system_t_call817"),DLR_system,LPAREN,$3,RPAREN) }
	|	DLR_exit parenE { TUPLE3(STRING("system_t_call818"),DLR_exit,$2) }
	|	DLR_fclose LPAREN idClassSel RPAREN { TUPLE5(STRING("system_t_call819"),DLR_fclose,LPAREN,$3,RPAREN) }
	|	DLR_fflush parenE { TUPLE3(STRING("system_t_call820"),DLR_fflush,$2) }
	|	DLR_fflush LPAREN expr RPAREN { TUPLE5(STRING("system_t_call821"),DLR_fflush,LPAREN,$3,RPAREN) }
	|	DLR_finish parenE { TUPLE3(STRING("system_t_call822"),DLR_finish,$2) }
	|	DLR_finish LPAREN expr RPAREN { TUPLE5(STRING("system_t_call823"),DLR_finish,LPAREN,$3,RPAREN) }
	|	DLR_stop parenE { TUPLE3(STRING("system_t_call824"),DLR_stop,$2) }
	|	DLR_stop LPAREN expr RPAREN { TUPLE5(STRING("system_t_call825"),DLR_stop,LPAREN,$3,RPAREN) }
	|	DLR_sformat LPAREN expr COMMA exprDispList RPAREN { TUPLE7(STRING("system_t_call826"),DLR_sformat,LPAREN,$3,COMMA,$5,RPAREN) }
	|	DLR_swrite LPAREN expr COMMA exprDispList RPAREN { TUPLE7(STRING("system_t_call827"),DLR_swrite,LPAREN,$3,COMMA,$5,RPAREN) }
	|	DLR_swriteb LPAREN expr COMMA exprDispList RPAREN { TUPLE7(STRING("system_t_call828"),DLR_swriteb,LPAREN,$3,COMMA,$5,RPAREN) }
	|	DLR_swriteh LPAREN expr COMMA exprDispList RPAREN { TUPLE7(STRING("system_t_call829"),DLR_swriteh,LPAREN,$3,COMMA,$5,RPAREN) }
	|	DLR_swriteo LPAREN expr COMMA exprDispList RPAREN { TUPLE7(STRING("system_t_call830"),DLR_swriteo,LPAREN,$3,COMMA,$5,RPAREN) }
	|	DLR_display parenE { TUPLE3(STRING("system_t_call831"),DLR_display,$2) }
	|	DLR_display LPAREN exprDispList RPAREN { TUPLE5(STRING("system_t_call832"),DLR_display,LPAREN,$3,RPAREN) }
	|	DLR_displayb parenE { TUPLE3(STRING("system_t_call833"),DLR_displayb,$2) }
	|	DLR_displayb LPAREN exprDispList RPAREN { TUPLE5(STRING("system_t_call834"),DLR_displayb,LPAREN,$3,RPAREN) }
	|	DLR_displayh parenE { TUPLE3(STRING("system_t_call835"),DLR_displayh,$2) }
	|	DLR_displayh LPAREN exprDispList RPAREN { TUPLE5(STRING("system_t_call836"),DLR_displayh,LPAREN,$3,RPAREN) }
	|	DLR_displayo parenE { TUPLE3(STRING("system_t_call837"),DLR_displayo,$2) }
	|	DLR_displayo LPAREN exprDispList RPAREN { TUPLE5(STRING("system_t_call838"),DLR_displayo,LPAREN,$3,RPAREN) }
	|	DLR_monitor LPAREN exprDispList RPAREN { TUPLE5(STRING("system_t_call839"),DLR_monitor,LPAREN,$3,RPAREN) }
	|	DLR_monitorb LPAREN exprDispList RPAREN { TUPLE5(STRING("system_t_call840"),DLR_monitorb,LPAREN,$3,RPAREN) }
	|	DLR_monitorh LPAREN exprDispList RPAREN { TUPLE5(STRING("system_t_call841"),DLR_monitorh,LPAREN,$3,RPAREN) }
	|	DLR_monitoro LPAREN exprDispList RPAREN { TUPLE5(STRING("system_t_call842"),DLR_monitoro,LPAREN,$3,RPAREN) }
	|	DLR_strobe LPAREN exprDispList RPAREN { TUPLE5(STRING("system_t_call843"),DLR_strobe,LPAREN,$3,RPAREN) }
	|	DLR_strobeb LPAREN exprDispList RPAREN { TUPLE5(STRING("system_t_call844"),DLR_strobeb,LPAREN,$3,RPAREN) }
	|	DLR_strobeh LPAREN exprDispList RPAREN { TUPLE5(STRING("system_t_call845"),DLR_strobeh,LPAREN,$3,RPAREN) }
	|	DLR_strobeo LPAREN exprDispList RPAREN { TUPLE5(STRING("system_t_call846"),DLR_strobeo,LPAREN,$3,RPAREN) }
	|	DLR_write parenE { TUPLE3(STRING("system_t_call847"),DLR_write,$2) }
	|	DLR_write LPAREN exprDispList RPAREN { TUPLE5(STRING("system_t_call848"),DLR_write,LPAREN,$3,RPAREN) }
	|	DLR_writeb parenE { TUPLE3(STRING("system_t_call849"),DLR_writeb,$2) }
	|	DLR_writeb LPAREN exprDispList RPAREN { TUPLE5(STRING("system_t_call850"),DLR_writeb,LPAREN,$3,RPAREN) }
	|	DLR_writeh parenE { TUPLE3(STRING("system_t_call851"),DLR_writeh,$2) }
	|	DLR_writeh LPAREN exprDispList RPAREN { TUPLE5(STRING("system_t_call852"),DLR_writeh,LPAREN,$3,RPAREN) }
	|	DLR_writeo parenE { TUPLE3(STRING("system_t_call853"),DLR_writeo,$2) }
	|	DLR_writeo LPAREN exprDispList RPAREN { TUPLE5(STRING("system_t_call854"),DLR_writeo,LPAREN,$3,RPAREN) }
	|	DLR_fdisplay LPAREN expr RPAREN { TUPLE5(STRING("system_t_call855"),DLR_fdisplay,LPAREN,$3,RPAREN) }
	|	DLR_fdisplay LPAREN expr COMMA exprDispList RPAREN { TUPLE7(STRING("system_t_call856"),DLR_fdisplay,LPAREN,$3,COMMA,$5,RPAREN) }
	|	DLR_fdisplayb LPAREN expr RPAREN { TUPLE5(STRING("system_t_call857"),DLR_fdisplayb,LPAREN,$3,RPAREN) }
	|	DLR_fdisplayb LPAREN expr COMMA exprDispList RPAREN { TUPLE7(STRING("system_t_call858"),DLR_fdisplayb,LPAREN,$3,COMMA,$5,RPAREN) }
	|	DLR_fdisplayh LPAREN expr RPAREN { TUPLE5(STRING("system_t_call859"),DLR_fdisplayh,LPAREN,$3,RPAREN) }
	|	DLR_fdisplayh LPAREN expr COMMA exprDispList RPAREN { TUPLE7(STRING("system_t_call860"),DLR_fdisplayh,LPAREN,$3,COMMA,$5,RPAREN) }
	|	DLR_fdisplayo LPAREN expr RPAREN { TUPLE5(STRING("system_t_call861"),DLR_fdisplayo,LPAREN,$3,RPAREN) }
	|	DLR_fdisplayo LPAREN expr COMMA exprDispList RPAREN { TUPLE7(STRING("system_t_call862"),DLR_fdisplayo,LPAREN,$3,COMMA,$5,RPAREN) }
	|	DLR_fmonitor LPAREN expr COMMA exprDispList RPAREN { TUPLE7(STRING("system_t_call863"),DLR_fmonitor,LPAREN,$3,COMMA,$5,RPAREN) }
	|	DLR_fmonitorb LPAREN expr COMMA exprDispList RPAREN { TUPLE7(STRING("system_t_call864"),DLR_fmonitorb,LPAREN,$3,COMMA,$5,RPAREN) }
	|	DLR_fmonitorh LPAREN expr COMMA exprDispList RPAREN { TUPLE7(STRING("system_t_call865"),DLR_fmonitorh,LPAREN,$3,COMMA,$5,RPAREN) }
	|	DLR_fmonitoro LPAREN expr COMMA exprDispList RPAREN { TUPLE7(STRING("system_t_call866"),DLR_fmonitoro,LPAREN,$3,COMMA,$5,RPAREN) }
	|	DLR_fstrobe LPAREN expr COMMA exprDispList RPAREN { TUPLE7(STRING("system_t_call867"),DLR_fstrobe,LPAREN,$3,COMMA,$5,RPAREN) }
	|	DLR_fstrobeb LPAREN expr COMMA exprDispList RPAREN { TUPLE7(STRING("system_t_call868"),DLR_fstrobeb,LPAREN,$3,COMMA,$5,RPAREN) }
	|	DLR_fstrobeh LPAREN expr COMMA exprDispList RPAREN { TUPLE7(STRING("system_t_call869"),DLR_fstrobeh,LPAREN,$3,COMMA,$5,RPAREN) }
	|	DLR_fstrobeo LPAREN expr COMMA exprDispList RPAREN { TUPLE7(STRING("system_t_call870"),DLR_fstrobeo,LPAREN,$3,COMMA,$5,RPAREN) }
	|	DLR_fwrite LPAREN expr COMMA exprDispList RPAREN { TUPLE7(STRING("system_t_call871"),DLR_fwrite,LPAREN,$3,COMMA,$5,RPAREN) }
	|	DLR_fwriteb LPAREN expr COMMA exprDispList RPAREN { TUPLE7(STRING("system_t_call872"),DLR_fwriteb,LPAREN,$3,COMMA,$5,RPAREN) }
	|	DLR_fwriteh LPAREN expr COMMA exprDispList RPAREN { TUPLE7(STRING("system_t_call873"),DLR_fwriteh,LPAREN,$3,COMMA,$5,RPAREN) }
	|	DLR_fwriteo LPAREN expr COMMA exprDispList RPAREN { TUPLE7(STRING("system_t_call874"),DLR_fwriteo,LPAREN,$3,COMMA,$5,RPAREN) }
	|	DLR_info parenE { TUPLE3(STRING("system_t_call875"),DLR_info,$2) }
	|	DLR_info LPAREN exprDispList RPAREN { TUPLE5(STRING("system_t_call876"),DLR_info,LPAREN,$3,RPAREN) }
	|	DLR_warning parenE { TUPLE3(STRING("system_t_call877"),DLR_warning,$2) }
	|	DLR_warning LPAREN exprDispList RPAREN { TUPLE5(STRING("system_t_call878"),DLR_warning,LPAREN,$3,RPAREN) }
	|	DLR_error parenE { TUPLE3(STRING("system_t_call879"),DLR_error,$2) }
	|	DLR_error LPAREN exprDispList RPAREN { TUPLE5(STRING("system_t_call880"),DLR_error,LPAREN,$3,RPAREN) }
	|	DLR_fatal parenE { TUPLE3(STRING("system_t_call881"),DLR_fatal,$2) }
	|	DLR_fatal LPAREN expr RPAREN { TUPLE5(STRING("system_t_call882"),DLR_fatal,LPAREN,$3,RPAREN) }
	|	DLR_fatal LPAREN expr COMMA exprDispList RPAREN { TUPLE7(STRING("system_t_call883"),DLR_fatal,LPAREN,$3,COMMA,$5,RPAREN) }
	|	DLR_monitoroff parenE { TUPLE3(STRING("system_t_call884"),DLR_monitoroff,$2) }
	|	DLR_monitoron parenE { TUPLE3(STRING("system_t_call885"),DLR_monitoron,$2) }
	|	DLR_printtimescale { (DLR_printtimescale) }
	|	DLR_printtimescale LPAREN RPAREN { TUPLE4(STRING("system_t_call887"),DLR_printtimescale,LPAREN,RPAREN) }
	|	DLR_printtimescale LPAREN idClassSel RPAREN { TUPLE5(STRING("system_t_call888"),DLR_printtimescale,LPAREN,$3,RPAREN) }
	|	DLR_timeformat LPAREN expr COMMA expr COMMA expr COMMA expr RPAREN { TUPLE11(STRING("system_t_call889"),DLR_timeformat,LPAREN,$3,COMMA,$5,COMMA,$7,COMMA,$9,RPAREN) }
	|	DLR_readmemb LPAREN expr COMMA idClassSel RPAREN { TUPLE7(STRING("system_t_call890"),DLR_readmemb,LPAREN,$3,COMMA,$5,RPAREN) }
	|	DLR_readmemb LPAREN expr COMMA idClassSel COMMA expr RPAREN { TUPLE9(STRING("system_t_call891"),DLR_readmemb,LPAREN,$3,COMMA,$5,COMMA,$7,RPAREN) }
	|	DLR_readmemb LPAREN expr COMMA idClassSel COMMA expr COMMA expr RPAREN { TUPLE11(STRING("system_t_call892"),DLR_readmemb,LPAREN,$3,COMMA,$5,COMMA,$7,COMMA,$9,RPAREN) }
	|	DLR_readmemh LPAREN expr COMMA idClassSel RPAREN { TUPLE7(STRING("system_t_call893"),DLR_readmemh,LPAREN,$3,COMMA,$5,RPAREN) }
	|	DLR_readmemh LPAREN expr COMMA idClassSel COMMA expr RPAREN { TUPLE9(STRING("system_t_call894"),DLR_readmemh,LPAREN,$3,COMMA,$5,COMMA,$7,RPAREN) }
	|	DLR_readmemh LPAREN expr COMMA idClassSel COMMA expr COMMA expr RPAREN { TUPLE11(STRING("system_t_call895"),DLR_readmemh,LPAREN,$3,COMMA,$5,COMMA,$7,COMMA,$9,RPAREN) }
	|	DLR_writememb LPAREN expr COMMA idClassSel RPAREN { TUPLE7(STRING("system_t_call896"),DLR_writememb,LPAREN,$3,COMMA,$5,RPAREN) }
	|	DLR_writememb LPAREN expr COMMA idClassSel COMMA expr RPAREN { TUPLE9(STRING("system_t_call897"),DLR_writememb,LPAREN,$3,COMMA,$5,COMMA,$7,RPAREN) }
	|	DLR_writememb LPAREN expr COMMA idClassSel COMMA expr COMMA expr RPAREN { TUPLE11(STRING("system_t_call898"),DLR_writememb,LPAREN,$3,COMMA,$5,COMMA,$7,COMMA,$9,RPAREN) }
	|	DLR_writememh LPAREN expr COMMA idClassSel RPAREN { TUPLE7(STRING("system_t_call899"),DLR_writememh,LPAREN,$3,COMMA,$5,RPAREN) }
	|	DLR_writememh LPAREN expr COMMA idClassSel COMMA expr RPAREN { TUPLE9(STRING("system_t_call900"),DLR_writememh,LPAREN,$3,COMMA,$5,COMMA,$7,RPAREN) }
	|	DLR_writememh LPAREN expr COMMA idClassSel COMMA expr COMMA expr RPAREN { TUPLE11(STRING("system_t_call901"),DLR_writememh,LPAREN,$3,COMMA,$5,COMMA,$7,COMMA,$9,RPAREN) }
	|	DLR_cast LPAREN expr COMMA expr RPAREN { TUPLE7(STRING("system_t_call902"),DLR_cast,LPAREN,$3,COMMA,$5,RPAREN) }
	|	system_f_call_or_t { ($1) }

system_f_call: DLR_LBRACE_pli_HYPHEN_system_RBRACE systemDpiArgsE { TUPLE3(STRING("system_f_call904"),DLR_LBRACE_pli_HYPHEN_system_RBRACE,$2) }
	|	DLR_c LPAREN cStrList RPAREN { TUPLE5(STRING("system_f_call905"),DLR_c,LPAREN,$3,RPAREN) }
	|	DLR_cast LPAREN expr COMMA expr RPAREN { TUPLE7(STRING("system_f_call906"),DLR_cast,LPAREN,$3,COMMA,$5,RPAREN) }
	|	DLR_system LPAREN expr RPAREN { TUPLE5(STRING("system_f_call907"),DLR_system,LPAREN,$3,RPAREN) }
	|	system_f_call_or_t { ($1) }

systemDpiArgsE: parenE { ($1) }
	|	LPAREN exprList RPAREN { TUPLE4(STRING("systemDpiArgsE910"),LPAREN,$2,RPAREN) }

system_f_call_or_t: DLR_acos LPAREN expr RPAREN { TUPLE5(STRING("system_f_call_or_t911"),DLR_acos,LPAREN,$3,RPAREN) }
	|	DLR_acosh LPAREN expr RPAREN { TUPLE5(STRING("system_f_call_or_t912"),DLR_acosh,LPAREN,$3,RPAREN) }
	|	DLR_asin LPAREN expr RPAREN { TUPLE5(STRING("system_f_call_or_t913"),DLR_asin,LPAREN,$3,RPAREN) }
	|	DLR_asinh LPAREN expr RPAREN { TUPLE5(STRING("system_f_call_or_t914"),DLR_asinh,LPAREN,$3,RPAREN) }
	|	DLR_atan LPAREN expr RPAREN { TUPLE5(STRING("system_f_call_or_t915"),DLR_atan,LPAREN,$3,RPAREN) }
	|	DLR_atan2 LPAREN expr COMMA expr RPAREN { TUPLE7(STRING("system_f_call_or_t916"),DLR_atan2,LPAREN,$3,COMMA,$5,RPAREN) }
	|	DLR_atanh LPAREN expr RPAREN { TUPLE5(STRING("system_f_call_or_t917"),DLR_atanh,LPAREN,$3,RPAREN) }
	|	DLR_bits LPAREN exprOrDataType RPAREN { TUPLE5(STRING("system_f_call_or_t918"),DLR_bits,LPAREN,$3,RPAREN) }
	|	DLR_bits LPAREN exprOrDataType COMMA expr RPAREN { TUPLE7(STRING("system_f_call_or_t919"),DLR_bits,LPAREN,$3,COMMA,$5,RPAREN) }
	|	DLR_bitstoreal LPAREN expr RPAREN { TUPLE5(STRING("system_f_call_or_t920"),DLR_bitstoreal,LPAREN,$3,RPAREN) }
	|	DLR_bitstoshortreal LPAREN expr RPAREN { TUPLE5(STRING("system_f_call_or_t921"),DLR_bitstoshortreal,LPAREN,$3,RPAREN) }
	|	DLR_ceil LPAREN expr RPAREN { TUPLE5(STRING("system_f_call_or_t922"),DLR_ceil,LPAREN,$3,RPAREN) }
	|	DLR_changed LPAREN expr RPAREN { TUPLE5(STRING("system_f_call_or_t923"),DLR_changed,LPAREN,$3,RPAREN) }
	|	DLR_changed LPAREN expr COMMA expr RPAREN { TUPLE7(STRING("system_f_call_or_t924"),DLR_changed,LPAREN,$3,COMMA,$5,RPAREN) }
	|	DLR_clog2 LPAREN expr RPAREN { TUPLE5(STRING("system_f_call_or_t925"),DLR_clog2,LPAREN,$3,RPAREN) }
	|	DLR_cos LPAREN expr RPAREN { TUPLE5(STRING("system_f_call_or_t926"),DLR_cos,LPAREN,$3,RPAREN) }
	|	DLR_cosh LPAREN expr RPAREN { TUPLE5(STRING("system_f_call_or_t927"),DLR_cosh,LPAREN,$3,RPAREN) }
	|	DLR_countbits LPAREN expr COMMA expr RPAREN { TUPLE7(STRING("system_f_call_or_t928"),DLR_countbits,LPAREN,$3,COMMA,$5,RPAREN) }
	|	DLR_countbits LPAREN expr COMMA expr COMMA expr RPAREN { TUPLE9(STRING("system_f_call_or_t929"),DLR_countbits,LPAREN,$3,COMMA,$5,COMMA,$7,RPAREN) }
	|	DLR_countbits LPAREN expr COMMA expr COMMA expr COMMA expr RPAREN { TUPLE11(STRING("system_f_call_or_t930"),DLR_countbits,LPAREN,$3,COMMA,$5,COMMA,$7,COMMA,$9,RPAREN) }
	|	DLR_countbits LPAREN expr COMMA expr COMMA expr COMMA expr COMMA exprList RPAREN { TUPLE13(STRING("system_f_call_or_t931"),DLR_countbits,LPAREN,$3,COMMA,$5,COMMA,$7,COMMA,$9,COMMA,$11,RPAREN) }
	|	DLR_countones LPAREN expr RPAREN { TUPLE5(STRING("system_f_call_or_t932"),DLR_countones,LPAREN,$3,RPAREN) }
	|	DLR_dimensions LPAREN exprOrDataType RPAREN { TUPLE5(STRING("system_f_call_or_t933"),DLR_dimensions,LPAREN,$3,RPAREN) }
	|	DLR_exp LPAREN expr RPAREN { TUPLE5(STRING("system_f_call_or_t934"),DLR_exp,LPAREN,$3,RPAREN) }
	|	DLR_fell LPAREN expr RPAREN { TUPLE5(STRING("system_f_call_or_t935"),DLR_fell,LPAREN,$3,RPAREN) }
	|	DLR_fell LPAREN expr COMMA expr RPAREN { TUPLE7(STRING("system_f_call_or_t936"),DLR_fell,LPAREN,$3,COMMA,$5,RPAREN) }
	|	DLR_feof LPAREN expr RPAREN { TUPLE5(STRING("system_f_call_or_t937"),DLR_feof,LPAREN,$3,RPAREN) }
	|	DLR_ferror LPAREN idClassSel COMMA idClassSel RPAREN { TUPLE7(STRING("system_f_call_or_t938"),DLR_ferror,LPAREN,$3,COMMA,$5,RPAREN) }
	|	DLR_fgetc LPAREN expr RPAREN { TUPLE5(STRING("system_f_call_or_t939"),DLR_fgetc,LPAREN,$3,RPAREN) }
	|	DLR_fgets LPAREN idClassSel COMMA expr RPAREN { TUPLE7(STRING("system_f_call_or_t940"),DLR_fgets,LPAREN,$3,COMMA,$5,RPAREN) }
	|	DLR_fread LPAREN idClassSel COMMA expr RPAREN { TUPLE7(STRING("system_f_call_or_t941"),DLR_fread,LPAREN,$3,COMMA,$5,RPAREN) }
	|	DLR_fread LPAREN idClassSel COMMA expr COMMA expr RPAREN { TUPLE9(STRING("system_f_call_or_t942"),DLR_fread,LPAREN,$3,COMMA,$5,COMMA,$7,RPAREN) }
	|	DLR_fread LPAREN idClassSel COMMA expr COMMA expr COMMA expr RPAREN { TUPLE11(STRING("system_f_call_or_t943"),DLR_fread,LPAREN,$3,COMMA,$5,COMMA,$7,COMMA,$9,RPAREN) }
	|	DLR_frewind LPAREN idClassSel RPAREN { TUPLE5(STRING("system_f_call_or_t944"),DLR_frewind,LPAREN,$3,RPAREN) }
	|	DLR_floor LPAREN expr RPAREN { TUPLE5(STRING("system_f_call_or_t945"),DLR_floor,LPAREN,$3,RPAREN) }
	|	DLR_fscanf LPAREN expr COMMA str commaVRDListE RPAREN { TUPLE8(STRING("system_f_call_or_t946"),DLR_fscanf,LPAREN,$3,COMMA,$5,$6,RPAREN) }
	|	DLR_fseek LPAREN idClassSel COMMA expr COMMA expr RPAREN { TUPLE9(STRING("system_f_call_or_t947"),DLR_fseek,LPAREN,$3,COMMA,$5,COMMA,$7,RPAREN) }
	|	DLR_ftell LPAREN idClassSel RPAREN { TUPLE5(STRING("system_f_call_or_t948"),DLR_ftell,LPAREN,$3,RPAREN) }
	|	DLR_high LPAREN exprOrDataType RPAREN { TUPLE5(STRING("system_f_call_or_t949"),DLR_high,LPAREN,$3,RPAREN) }
	|	DLR_high LPAREN exprOrDataType COMMA expr RPAREN { TUPLE7(STRING("system_f_call_or_t950"),DLR_high,LPAREN,$3,COMMA,$5,RPAREN) }
	|	DLR_hypot LPAREN expr COMMA expr RPAREN { TUPLE7(STRING("system_f_call_or_t951"),DLR_hypot,LPAREN,$3,COMMA,$5,RPAREN) }
	|	DLR_increment LPAREN exprOrDataType RPAREN { TUPLE5(STRING("system_f_call_or_t952"),DLR_increment,LPAREN,$3,RPAREN) }
	|	DLR_increment LPAREN exprOrDataType COMMA expr RPAREN { TUPLE7(STRING("system_f_call_or_t953"),DLR_increment,LPAREN,$3,COMMA,$5,RPAREN) }
	|	DLR_isunbounded LPAREN expr RPAREN { TUPLE5(STRING("system_f_call_or_t954"),DLR_isunbounded,LPAREN,$3,RPAREN) }
	|	DLR_isunknown LPAREN expr RPAREN { TUPLE5(STRING("system_f_call_or_t955"),DLR_isunknown,LPAREN,$3,RPAREN) }
	|	DLR_itor LPAREN expr RPAREN { TUPLE5(STRING("system_f_call_or_t956"),DLR_itor,LPAREN,$3,RPAREN) }
	|	DLR_left LPAREN exprOrDataType RPAREN { TUPLE5(STRING("system_f_call_or_t957"),DLR_left,LPAREN,$3,RPAREN) }
	|	DLR_left LPAREN exprOrDataType COMMA expr RPAREN { TUPLE7(STRING("system_f_call_or_t958"),DLR_left,LPAREN,$3,COMMA,$5,RPAREN) }
	|	DLR_ln LPAREN expr RPAREN { TUPLE5(STRING("system_f_call_or_t959"),DLR_ln,LPAREN,$3,RPAREN) }
	|	DLR_log10 LPAREN expr RPAREN { TUPLE5(STRING("system_f_call_or_t960"),DLR_log10,LPAREN,$3,RPAREN) }
	|	DLR_low LPAREN exprOrDataType RPAREN { TUPLE5(STRING("system_f_call_or_t961"),DLR_low,LPAREN,$3,RPAREN) }
	|	DLR_low LPAREN exprOrDataType COMMA expr RPAREN { TUPLE7(STRING("system_f_call_or_t962"),DLR_low,LPAREN,$3,COMMA,$5,RPAREN) }
	|	DLR_onehot LPAREN expr RPAREN { TUPLE5(STRING("system_f_call_or_t963"),DLR_onehot,LPAREN,$3,RPAREN) }
	|	DLR_onehot0 LPAREN expr RPAREN { TUPLE5(STRING("system_f_call_or_t964"),DLR_onehot0,LPAREN,$3,RPAREN) }
	|	DLR_past LPAREN expr RPAREN { TUPLE5(STRING("system_f_call_or_t965"),DLR_past,LPAREN,$3,RPAREN) }
	|	DLR_past LPAREN expr COMMA expr RPAREN { TUPLE7(STRING("system_f_call_or_t966"),DLR_past,LPAREN,$3,COMMA,$5,RPAREN) }
	|	DLR_past LPAREN expr COMMA expr COMMA expr RPAREN { TUPLE9(STRING("system_f_call_or_t967"),DLR_past,LPAREN,$3,COMMA,$5,COMMA,$7,RPAREN) }
	|	DLR_past LPAREN expr COMMA expr COMMA expr COMMA expr RPAREN { TUPLE11(STRING("system_f_call_or_t968"),DLR_past,LPAREN,$3,COMMA,$5,COMMA,$7,COMMA,$9,RPAREN) }
	|	DLR_pow LPAREN expr COMMA expr RPAREN { TUPLE7(STRING("system_f_call_or_t969"),DLR_pow,LPAREN,$3,COMMA,$5,RPAREN) }
	|	DLR_random LPAREN expr RPAREN { TUPLE5(STRING("system_f_call_or_t970"),DLR_random,LPAREN,$3,RPAREN) }
	|	DLR_random parenE { TUPLE3(STRING("system_f_call_or_t971"),DLR_random,$2) }
	|	DLR_realtime parenE { TUPLE3(STRING("system_f_call_or_t972"),DLR_realtime,$2) }
	|	DLR_realtobits LPAREN expr RPAREN { TUPLE5(STRING("system_f_call_or_t973"),DLR_realtobits,LPAREN,$3,RPAREN) }
	|	DLR_rewind LPAREN idClassSel RPAREN { TUPLE5(STRING("system_f_call_or_t974"),DLR_rewind,LPAREN,$3,RPAREN) }
	|	DLR_right LPAREN exprOrDataType RPAREN { TUPLE5(STRING("system_f_call_or_t975"),DLR_right,LPAREN,$3,RPAREN) }
	|	DLR_right LPAREN exprOrDataType COMMA expr RPAREN { TUPLE7(STRING("system_f_call_or_t976"),DLR_right,LPAREN,$3,COMMA,$5,RPAREN) }
	|	DLR_rose LPAREN expr RPAREN { TUPLE5(STRING("system_f_call_or_t977"),DLR_rose,LPAREN,$3,RPAREN) }
	|	DLR_rose LPAREN expr COMMA expr RPAREN { TUPLE7(STRING("system_f_call_or_t978"),DLR_rose,LPAREN,$3,COMMA,$5,RPAREN) }
	|	DLR_rtoi LPAREN expr RPAREN { TUPLE5(STRING("system_f_call_or_t979"),DLR_rtoi,LPAREN,$3,RPAREN) }
	|	DLR_sampled LPAREN expr RPAREN { TUPLE5(STRING("system_f_call_or_t980"),DLR_sampled,LPAREN,$3,RPAREN) }
	|	DLR_sformatf LPAREN exprDispList RPAREN { TUPLE5(STRING("system_f_call_or_t981"),DLR_sformatf,LPAREN,$3,RPAREN) }
	|	DLR_shortrealtobits LPAREN expr RPAREN { TUPLE5(STRING("system_f_call_or_t982"),DLR_shortrealtobits,LPAREN,$3,RPAREN) }
	|	DLR_signed LPAREN expr RPAREN { TUPLE5(STRING("system_f_call_or_t983"),DLR_signed,LPAREN,$3,RPAREN) }
	|	DLR_sin LPAREN expr RPAREN { TUPLE5(STRING("system_f_call_or_t984"),DLR_sin,LPAREN,$3,RPAREN) }
	|	DLR_sinh LPAREN expr RPAREN { TUPLE5(STRING("system_f_call_or_t985"),DLR_sinh,LPAREN,$3,RPAREN) }
	|	DLR_size LPAREN exprOrDataType RPAREN { TUPLE5(STRING("system_f_call_or_t986"),DLR_size,LPAREN,$3,RPAREN) }
	|	DLR_size LPAREN exprOrDataType COMMA expr RPAREN { TUPLE7(STRING("system_f_call_or_t987"),DLR_size,LPAREN,$3,COMMA,$5,RPAREN) }
	|	DLR_sqrt LPAREN expr RPAREN { TUPLE5(STRING("system_f_call_or_t988"),DLR_sqrt,LPAREN,$3,RPAREN) }
	|	DLR_sscanf LPAREN expr COMMA str commaVRDListE RPAREN { TUPLE8(STRING("system_f_call_or_t989"),DLR_sscanf,LPAREN,$3,COMMA,$5,$6,RPAREN) }
	|	DLR_stime parenE { TUPLE3(STRING("system_f_call_or_t990"),DLR_stime,$2) }
	|	DLR_stable LPAREN expr RPAREN { TUPLE5(STRING("system_f_call_or_t991"),DLR_stable,LPAREN,$3,RPAREN) }
	|	DLR_stable LPAREN expr COMMA expr RPAREN { TUPLE7(STRING("system_f_call_or_t992"),DLR_stable,LPAREN,$3,COMMA,$5,RPAREN) }
	|	DLR_tan LPAREN expr RPAREN { TUPLE5(STRING("system_f_call_or_t993"),DLR_tan,LPAREN,$3,RPAREN) }
	|	DLR_tanh LPAREN expr RPAREN { TUPLE5(STRING("system_f_call_or_t994"),DLR_tanh,LPAREN,$3,RPAREN) }
	|	DLR_test_DLR_plusargs LPAREN str RPAREN { TUPLE5(STRING("system_f_call_or_t995"),DLR_test_DLR_plusargs,LPAREN,$3,RPAREN) }
	|	DLR_time parenE { TUPLE3(STRING("system_f_call_or_t996"),DLR_time,$2) }
	|	DLR_typename LPAREN exprOrDataType RPAREN { TUPLE5(STRING("system_f_call_or_t997"),DLR_typename,LPAREN,$3,RPAREN) }
	|	DLR_ungetc LPAREN expr COMMA expr RPAREN { TUPLE7(STRING("system_f_call_or_t998"),DLR_ungetc,LPAREN,$3,COMMA,$5,RPAREN) }
	|	DLR_unpacked_dimensions LPAREN exprOrDataType RPAREN { TUPLE5(STRING("system_f_call_or_t999"),DLR_unpacked_dimensions,LPAREN,$3,RPAREN) }
	|	DLR_unsigned LPAREN expr RPAREN { TUPLE5(STRING("system_f_call_or_t1000"),DLR_unsigned,LPAREN,$3,RPAREN) }
	|	DLR_urandom LPAREN expr RPAREN { TUPLE5(STRING("system_f_call_or_t1001"),DLR_urandom,LPAREN,$3,RPAREN) }
	|	DLR_urandom parenE { TUPLE3(STRING("system_f_call_or_t1002"),DLR_urandom,$2) }
	|	DLR_urandom_range LPAREN expr RPAREN { TUPLE5(STRING("system_f_call_or_t1003"),DLR_urandom_range,LPAREN,$3,RPAREN) }
	|	DLR_urandom_range LPAREN expr COMMA expr RPAREN { TUPLE7(STRING("system_f_call_or_t1004"),DLR_urandom_range,LPAREN,$3,COMMA,$5,RPAREN) }
	|	DLR_value_DLR_plusargs LPAREN expr COMMA expr RPAREN { TUPLE7(STRING("system_f_call_or_t1005"),DLR_value_DLR_plusargs,LPAREN,$3,COMMA,$5,RPAREN) }

elaboration_system_task: elaboration_system_task_guts SEMICOLON { TUPLE3(STRING("elaboration_system_task1006"),$1,SEMICOLON) }

elaboration_system_task_guts: DLR_info parenE { TUPLE3(STRING("elaboration_system_task_guts1007"),DLR_info,$2) }
	|	DLR_info LPAREN exprList RPAREN { TUPLE5(STRING("elaboration_system_task_guts1008"),DLR_info,LPAREN,$3,RPAREN) }
	|	DLR_warning parenE { TUPLE3(STRING("elaboration_system_task_guts1009"),DLR_warning,$2) }
	|	DLR_warning LPAREN exprList RPAREN { TUPLE5(STRING("elaboration_system_task_guts1010"),DLR_warning,LPAREN,$3,RPAREN) }
	|	DLR_error parenE { TUPLE3(STRING("elaboration_system_task_guts1011"),DLR_error,$2) }
	|	DLR_error LPAREN exprList RPAREN { TUPLE5(STRING("elaboration_system_task_guts1012"),DLR_error,LPAREN,$3,RPAREN) }
	|	DLR_fatal parenE { TUPLE3(STRING("elaboration_system_task_guts1013"),DLR_fatal,$2) }
	|	DLR_fatal LPAREN expr RPAREN { TUPLE5(STRING("elaboration_system_task_guts1014"),DLR_fatal,LPAREN,$3,RPAREN) }
	|	DLR_fatal LPAREN expr COMMA exprListE RPAREN { TUPLE7(STRING("elaboration_system_task_guts1015"),DLR_fatal,LPAREN,$3,COMMA,$5,RPAREN) }

exprOrDataType: expr { ($1) }
	|	data_type { ($1) }

list_of_argumentsE: argsDottedList { CONS1 ($1) }
	|	argsExprListE { CONS1 ($1) }
	|	argsExprListE COMMA argsDottedList { CONS3($1,COMMA,$3) }

task_declaration: Task lifetimeE taskId tfGuts Endtask endLabelE { TUPLE7(STRING("task_declaration1021"),Task,$2,$3,$4,Endtask,$6) }

task_prototype: Task taskId LPAREN tf_port_listE RPAREN { TUPLE6(STRING("task_prototype1022"),Task,$2,LPAREN,$4,RPAREN) }
	|	Task taskId { TUPLE3(STRING("task_prototype1023"),Task,$2) }

function_declaration: Function lifetimeE funcId funcIsolateE tfGuts Endfunction endLabelE { TUPLE8(STRING("function_declaration1024"),Function,$2,$3,$4,$5,Endfunction,$7) }
	|	Function lifetimeE funcIdNew funcIsolateE tfGuts Endfunction endLabelE { TUPLE8(STRING("function_declaration1025"),Function,$2,$3,$4,$5,Endfunction,$7) }

function_prototype: Function funcId LPAREN tf_port_listE RPAREN { TUPLE6(STRING("function_prototype1026"),Function,$2,LPAREN,$4,RPAREN) }
	|	Function funcId { TUPLE3(STRING("function_prototype1027"),Function,$2) }

class_constructor_prototype: Function funcIdNew LPAREN tf_port_listE RPAREN SEMICOLON { TUPLE7(STRING("class_constructor_prototype1028"),Function,$2,LPAREN,$4,RPAREN,SEMICOLON) }
	|	Function funcIdNew SEMICOLON { TUPLE4(STRING("class_constructor_prototype1029"),Function,$2,SEMICOLON) }

funcIsolateE: /* empty */ { EMPTY_TOKEN }
	|	SLASH_STAR_verilator_isolate_assignments_STAR_SLASH { (SLASH_STAR_verilator_isolate_assignments_STAR_SLASH) }

method_prototype: task_prototype { ($1) }
	|	function_prototype { ($1) }

lifetimeE: /* empty */ { EMPTY_TOKEN }
	|	lifetime { ($1) }

lifetime: Static { (Static) }
	|	Automatic { (Automatic) }

taskId: id { ($1) }
	|	id DOT id { TUPLE4(STRING("taskId1039"),$1,DOT,$3) }
	|	packageClassScope id { TUPLE3(STRING("taskId1040"),$1,$2) }

funcId: fIdScoped { ($1) }
	|	signingE rangeList fIdScoped { TUPLE4(STRING("funcId1042"),$1,$2,$3) }
	|	signing fIdScoped { TUPLE3(STRING("funcId1043"),$1,$2) }
	|	data_type fIdScoped { TUPLE3(STRING("funcId1044"),$1,$2) }
	|	Void taskId { TUPLE3(STRING("funcId1045"),Void,$2) }

funcIdNew: New { (New) }
	|	New_HYPHEN_then_HYPHEN_paren { (New_HYPHEN_then_HYPHEN_paren) }
	|	packageClassScopeNoId New_HYPHEN_then_HYPHEN_paren { TUPLE3(STRING("funcIdNew1048"),$1,New_HYPHEN_then_HYPHEN_paren) }

fIdScoped: id { ($1) }
	|	id DOT id { TUPLE4(STRING("fIdScoped1050"),$1,DOT,$3) }
	|	packageClassScope id { TUPLE3(STRING("fIdScoped1051"),$1,$2) }

tfGuts: LPAREN tf_port_listE RPAREN SEMICOLON tfBodyE { TUPLE6(STRING("tfGuts1052"),LPAREN,$2,RPAREN,SEMICOLON,$5) }
	|	SEMICOLON tfBodyE { TUPLE3(STRING("tfGuts1053"),SEMICOLON,$2) }

tfBodyE: /* empty */ { EMPTY_TOKEN }
	|	tf_item_declarationList { ($1) }
	|	tf_item_declarationList stmtList { TUPLE3(STRING("tfBodyE1056"),$1,$2) }
	|	stmtList { ($1) }

tf_item_declarationList: tf_item_declaration { CONS1 ($1) }
	|	tf_item_declarationList tf_item_declaration { CONS2($1,$2) }

tf_item_declaration: block_item_declaration { ($1) }
	|	tf_port_declaration { ($1) }
	|	tf_item_declarationVerilator { ($1) }

tf_item_declarationVerilator: SLASH_STAR_verilator_public_STAR_SLASH { (SLASH_STAR_verilator_public_STAR_SLASH) }
	|	SLASH_STAR_verilator_no_inline_task_STAR_SLASH { (SLASH_STAR_verilator_no_inline_task_STAR_SLASH) }

tf_port_listE: /* 22 */ tf_port_listList { ($1) }

tf_port_listList: tf_port_item { CONS1 ($1) }
	|	tf_port_listList COMMA tf_port_item { CONS3($1,COMMA,$3) }

tf_port_item: /* empty */ { EMPTY_TOKEN }
	|	tf_port_itemFront tf_port_itemAssignment { TUPLE3(STRING("tf_port_item1070"),$1,$2) }
	|	tf_port_itemAssignment { ($1) }

tf_port_itemFront: data_type { ($1) }
	|	signingE rangeList { TUPLE3(STRING("tf_port_itemFront1073"),$1,$2) }
	|	signing { ($1) }
	|	Var data_type { TUPLE3(STRING("tf_port_itemFront1075"),Var,$2) }
	|	Var implicit_typeE { TUPLE3(STRING("tf_port_itemFront1076"),Var,$2) }
	|	tf_port_itemDir { ($1) }
	|	tf_port_itemDir data_type { TUPLE3(STRING("tf_port_itemFront1078"),$1,$2) }
	|	tf_port_itemDir signingE rangeList { TUPLE4(STRING("tf_port_itemFront1079"),$1,$2,$3) }
	|	tf_port_itemDir signing { TUPLE3(STRING("tf_port_itemFront1080"),$1,$2) }
	|	tf_port_itemDir Var data_type { TUPLE4(STRING("tf_port_itemFront1081"),$1,Var,$3) }
	|	tf_port_itemDir Var implicit_typeE { TUPLE4(STRING("tf_port_itemFront1082"),$1,Var,$3) }

tf_port_itemDir: port_direction { ($1) }

tf_port_itemAssignment: id variable_dimensionListE sigAttrListE exprEqE { TUPLE5(STRING("tf_port_itemAssignment1084"),$1,$2,$3,$4) }

parenE: /* empty */ { EMPTY_TOKEN }
	|	LPAREN RPAREN { TUPLE3(STRING("parenE1086"),LPAREN,RPAREN) }

array_methodNoRoot: Or { (Or) }
	|	And { (And) }
	|	Xor { (Xor) }
	|	Unique { (Unique) }

array_methodWith: array_methodNoRoot { ($1) }
	|	array_methodNoRoot parenE With_HYPHEN_then_HYPHEN_LPAREN LPAREN expr RPAREN { TUPLE7(STRING("array_methodWith1092"),$1,$2,With_HYPHEN_then_HYPHEN_LPAREN,LPAREN,$5,RPAREN) }
	|	array_methodNoRoot LPAREN expr RPAREN With_HYPHEN_then_HYPHEN_LPAREN LPAREN expr RPAREN { TUPLE9(STRING("array_methodWith1093"),$1,LPAREN,$3,RPAREN,With_HYPHEN_then_HYPHEN_LPAREN,LPAREN,$7,RPAREN) }

dpi_import_export: Import STRING dpi_tf_import_propertyE dpi_importLabelE function_prototype SEMICOLON { TUPLE7(STRING("dpi_import_export1094"),Import,STRING $2,$3,$4,$5,SEMICOLON) }
	|	Import STRING dpi_tf_import_propertyE dpi_importLabelE task_prototype SEMICOLON { TUPLE7(STRING("dpi_import_export1095"),Import,STRING $2,$3,$4,$5,SEMICOLON) }
	|	Export STRING dpi_importLabelE Function idAny SEMICOLON { TUPLE7(STRING("dpi_import_export1096"),Export,STRING $2,$3,Function,$5,SEMICOLON) }
	|	Export STRING dpi_importLabelE Task idAny SEMICOLON { TUPLE7(STRING("dpi_import_export1097"),Export,STRING $2,$3,Task,$5,SEMICOLON) }

dpi_importLabelE: /* empty */ { EMPTY_TOKEN }
	|	idAny EQUALS { TUPLE3(STRING("dpi_importLabelE1099"),$1,EQUALS) }

dpi_tf_import_propertyE: /* empty */ { EMPTY_TOKEN }
	|	Context { (Context) }
	|	Pure { (Pure) }

exprEqE: /* empty */ { EMPTY_TOKEN }
	|	EQUALS expr { TUPLE3(STRING("exprEqE1104"),EQUALS,$2) }

exprOrDataTypeEqE: /* empty */ { EMPTY_TOKEN }
	|	EQUALS exprOrDataType { TUPLE3(STRING("exprOrDataTypeEqE1106"),EQUALS,$2) }

constExpr: expr { ($1) }

expr: PLUS expr { TUPLE3(STRING("expr1108"),PLUS,$2) }
	|	HYPHEN expr { TUPLE3(STRING("expr1109"),HYPHEN,$2) }
	|	PLING expr { TUPLE3(STRING("expr1110"),PLING,$2) }
	|	AMPERSAND expr { TUPLE3(STRING("expr1111"),AMPERSAND,$2) }
	|	TILDE expr { TUPLE3(STRING("expr1112"),TILDE,$2) }
	|	VBAR expr { TUPLE3(STRING("expr1113"),VBAR,$2) }
	|	CARET expr { TUPLE3(STRING("expr1114"),CARET,$2) }
	|	TILDE_AMPERSAND expr { TUPLE3(STRING("expr1115"),TILDE_AMPERSAND,$2) }
	|	TILDE_VBAR expr { TUPLE3(STRING("expr1116"),TILDE_VBAR,$2) }
	|	CARET_TILDE expr { TUPLE3(STRING("expr1117"),CARET_TILDE,$2) }
	|	inc_or_dec_expression { ($1) }
	|	expr PLUS expr { TUPLE4(STRING("expr1119"),$1,PLUS,$3) }
	|	expr HYPHEN expr { TUPLE4(STRING("expr1120"),$1,HYPHEN,$3) }
	|	expr STAR expr { TUPLE4(STRING("expr1121"),$1,STAR,$3) }
	|	expr SLASH expr { TUPLE4(STRING("expr1122"),$1,SLASH,$3) }
	|	expr PERCENT expr { TUPLE4(STRING("expr1123"),$1,PERCENT,$3) }
	|	expr EQ_EQ expr { TUPLE4(STRING("expr1124"),$1,EQ_EQ,$3) }
	|	expr PLING_EQ expr { TUPLE4(STRING("expr1125"),$1,PLING_EQ,$3) }
	|	expr EQ_EQ_EQ expr { TUPLE4(STRING("expr1126"),$1,EQ_EQ_EQ,$3) }
	|	expr PLING_EQ_EQ expr { TUPLE4(STRING("expr1127"),$1,PLING_EQ_EQ,$3) }
	|	expr EQ_EQ_QUERY expr { TUPLE4(STRING("expr1128"),$1,EQ_EQ_QUERY,$3) }
	|	expr PLING_EQ_QUERY expr { TUPLE4(STRING("expr1129"),$1,PLING_EQ_QUERY,$3) }
	|	expr AMPERSAND_AMPERSAND expr { TUPLE4(STRING("expr1130"),$1,AMPERSAND_AMPERSAND,$3) }
	|	expr VBAR_VBAR expr { TUPLE4(STRING("expr1131"),$1,VBAR_VBAR,$3) }
	|	expr STAR_STAR expr { TUPLE4(STRING("expr1132"),$1,STAR_STAR,$3) }
	|	expr LESS expr { TUPLE4(STRING("expr1133"),$1,LESS,$3) }
	|	expr GREATER expr { TUPLE4(STRING("expr1134"),$1,GREATER,$3) }
	|	expr GT_EQ expr { TUPLE4(STRING("expr1135"),$1,GT_EQ,$3) }
	|	expr AMPERSAND expr { TUPLE4(STRING("expr1136"),$1,AMPERSAND,$3) }
	|	expr VBAR expr { TUPLE4(STRING("expr1137"),$1,VBAR,$3) }
	|	expr CARET expr { TUPLE4(STRING("expr1138"),$1,CARET,$3) }
	|	expr CARET_TILDE expr { TUPLE4(STRING("expr1139"),$1,CARET_TILDE,$3) }
	|	expr TILDE_VBAR expr { TUPLE4(STRING("expr1140"),$1,TILDE_VBAR,$3) }
	|	expr TILDE_AMPERSAND expr { TUPLE4(STRING("expr1141"),$1,TILDE_AMPERSAND,$3) }
	|	expr LT_LT expr { TUPLE4(STRING("expr1142"),$1,LT_LT,$3) }
	|	expr GT_GT expr { TUPLE4(STRING("expr1143"),$1,GT_GT,$3) }
	|	expr GT_GT_GT expr { TUPLE4(STRING("expr1144"),$1,GT_GT_GT,$3) }
	|	expr LT_HYPHEN_GT expr { TUPLE4(STRING("expr1145"),$1,LT_HYPHEN_GT,$3) }
	|	expr HYPHEN_GT expr { TUPLE4(STRING("expr1146"),$1,HYPHEN_GT,$3) }
	|	expr LT_EQ expr { TUPLE4(STRING("expr1147"),$1,LT_EQ,$3) }
	|	expr QUERY expr COLON expr { TUPLE6(STRING("expr1148"),$1,QUERY,$3,COLON,$5) }
	|	expr Inside LBRACE open_range_list RBRACE { TUPLE6(STRING("expr1149"),$1,Inside,LBRACE,$4,RBRACE) }
	|	INTEGER_NUMBER { (INTEGER_NUMBER $1) }
	|	FLOATING_HYPHEN_POINT_NUMBER { (FLOATING_HYPHEN_POINT_NUMBER $1) }
	|	timeNumAdjusted { ($1) }
	|	strAsInt { ($1) }
	|	LBRACE RBRACE { TUPLE3(STRING("expr1154"),LBRACE,RBRACE) }
	|	LBRACE constExpr LBRACE cateList RBRACE RBRACE { TUPLE7(STRING("expr1155"),LBRACE,$2,LBRACE,$4,RBRACE,RBRACE) }
	|	function_subroutine_callNoMethod { ($1) }
	|	expr DOT function_subroutine_callNoMethod { TUPLE4(STRING("expr1157"),$1,DOT,$3) }
	|	expr DOT array_methodWith { TUPLE4(STRING("expr1158"),$1,DOT,$3) }
	|	LPAREN expr RPAREN { TUPLE4(STRING("expr1159"),LPAREN,$2,RPAREN) }
	|	LPAREN expr COLON expr COLON expr RPAREN { TUPLE8(STRING("expr1160"),LPAREN,$2,COLON,$4,COLON,$6,RPAREN) }
	|	UNDERSCORE LPAREN expr RPAREN { TUPLE5(STRING("expr1161"),UNDERSCORE,LPAREN,$3,RPAREN) }
	|	simple_type QUOTE LPAREN expr RPAREN { TUPLE6(STRING("expr1162"),$1,QUOTE,LPAREN,$4,RPAREN) }
	|	Type LPAREN exprOrDataType RPAREN QUOTE LPAREN expr RPAREN { TUPLE9(STRING("expr1163"),Type,LPAREN,$3,RPAREN,QUOTE,LPAREN,$7,RPAREN) }
	|	Signed QUOTE LPAREN expr RPAREN { TUPLE6(STRING("expr1164"),Signed,QUOTE,LPAREN,$4,RPAREN) }
	|	Unsigned QUOTE LPAREN expr RPAREN { TUPLE6(STRING("expr1165"),Unsigned,QUOTE,LPAREN,$4,RPAREN) }
	|	String QUOTE LPAREN expr RPAREN { TUPLE6(STRING("expr1166"),String,QUOTE,LPAREN,$4,RPAREN) }
	|	Const QUOTE LPAREN expr RPAREN { TUPLE6(STRING("expr1167"),Const,QUOTE,LPAREN,$4,RPAREN) }
	|	expr QUOTE LPAREN expr RPAREN { TUPLE6(STRING("expr1168"),$1,QUOTE,LPAREN,$4,RPAREN) }
	|	DOLLAR { (DOLLAR) }
	|	Null { (Null) }
	|	exprOkLvalue { ($1) }
	|	expr AMPERSAND_AMPERSAND_AMPERSAND expr { TUPLE4(STRING("expr1172"),$1,AMPERSAND_AMPERSAND_AMPERSAND,$3) }
	|	expr Dist LBRACE dist_list RBRACE { TUPLE6(STRING("expr1173"),$1,Dist,LBRACE,$4,RBRACE) }

fexpr: PLUS fexpr { TUPLE3(STRING("fexpr1174"),PLUS,$2) }
	|	HYPHEN fexpr { TUPLE3(STRING("fexpr1175"),HYPHEN,$2) }
	|	PLING fexpr { TUPLE3(STRING("fexpr1176"),PLING,$2) }
	|	AMPERSAND fexpr { TUPLE3(STRING("fexpr1177"),AMPERSAND,$2) }
	|	TILDE fexpr { TUPLE3(STRING("fexpr1178"),TILDE,$2) }
	|	VBAR fexpr { TUPLE3(STRING("fexpr1179"),VBAR,$2) }
	|	CARET fexpr { TUPLE3(STRING("fexpr1180"),CARET,$2) }
	|	TILDE_AMPERSAND fexpr { TUPLE3(STRING("fexpr1181"),TILDE_AMPERSAND,$2) }
	|	TILDE_VBAR fexpr { TUPLE3(STRING("fexpr1182"),TILDE_VBAR,$2) }
	|	CARET_TILDE fexpr { TUPLE3(STRING("fexpr1183"),CARET_TILDE,$2) }
	|	finc_or_dec_expression { ($1) }
	|	fexpr PLUS fexpr { TUPLE4(STRING("fexpr1185"),$1,PLUS,$3) }
	|	fexpr HYPHEN fexpr { TUPLE4(STRING("fexpr1186"),$1,HYPHEN,$3) }
	|	fexpr STAR fexpr { TUPLE4(STRING("fexpr1187"),$1,STAR,$3) }
	|	fexpr SLASH fexpr { TUPLE4(STRING("fexpr1188"),$1,SLASH,$3) }
	|	fexpr PERCENT fexpr { TUPLE4(STRING("fexpr1189"),$1,PERCENT,$3) }
	|	fexpr EQ_EQ fexpr { TUPLE4(STRING("fexpr1190"),$1,EQ_EQ,$3) }
	|	fexpr PLING_EQ fexpr { TUPLE4(STRING("fexpr1191"),$1,PLING_EQ,$3) }
	|	fexpr EQ_EQ_EQ fexpr { TUPLE4(STRING("fexpr1192"),$1,EQ_EQ_EQ,$3) }
	|	fexpr PLING_EQ_EQ fexpr { TUPLE4(STRING("fexpr1193"),$1,PLING_EQ_EQ,$3) }
	|	fexpr EQ_EQ_QUERY fexpr { TUPLE4(STRING("fexpr1194"),$1,EQ_EQ_QUERY,$3) }
	|	fexpr PLING_EQ_QUERY fexpr { TUPLE4(STRING("fexpr1195"),$1,PLING_EQ_QUERY,$3) }
	|	fexpr AMPERSAND_AMPERSAND fexpr { TUPLE4(STRING("fexpr1196"),$1,AMPERSAND_AMPERSAND,$3) }
	|	fexpr VBAR_VBAR fexpr { TUPLE4(STRING("fexpr1197"),$1,VBAR_VBAR,$3) }
	|	fexpr STAR_STAR fexpr { TUPLE4(STRING("fexpr1198"),$1,STAR_STAR,$3) }
	|	fexpr LESS fexpr { TUPLE4(STRING("fexpr1199"),$1,LESS,$3) }
	|	fexpr GREATER fexpr { TUPLE4(STRING("fexpr1200"),$1,GREATER,$3) }
	|	fexpr GT_EQ fexpr { TUPLE4(STRING("fexpr1201"),$1,GT_EQ,$3) }
	|	fexpr AMPERSAND fexpr { TUPLE4(STRING("fexpr1202"),$1,AMPERSAND,$3) }
	|	fexpr VBAR fexpr { TUPLE4(STRING("fexpr1203"),$1,VBAR,$3) }
	|	fexpr CARET fexpr { TUPLE4(STRING("fexpr1204"),$1,CARET,$3) }
	|	fexpr CARET_TILDE fexpr { TUPLE4(STRING("fexpr1205"),$1,CARET_TILDE,$3) }
	|	fexpr TILDE_VBAR fexpr { TUPLE4(STRING("fexpr1206"),$1,TILDE_VBAR,$3) }
	|	fexpr TILDE_AMPERSAND fexpr { TUPLE4(STRING("fexpr1207"),$1,TILDE_AMPERSAND,$3) }
	|	fexpr LT_LT fexpr { TUPLE4(STRING("fexpr1208"),$1,LT_LT,$3) }
	|	fexpr GT_GT fexpr { TUPLE4(STRING("fexpr1209"),$1,GT_GT,$3) }
	|	fexpr GT_GT_GT fexpr { TUPLE4(STRING("fexpr1210"),$1,GT_GT_GT,$3) }
	|	fexpr LT_HYPHEN_GT fexpr { TUPLE4(STRING("fexpr1211"),$1,LT_HYPHEN_GT,$3) }
	|	fexpr HYPHEN_GT fexpr { TUPLE4(STRING("fexpr1212"),$1,HYPHEN_GT,$3) }
	|	fexpr LT_EQ_HYPHEN_ignored fexpr { TUPLE4(STRING("fexpr1213"),$1,LT_EQ_HYPHEN_ignored,$3) }
	|	fexpr QUERY fexpr COLON fexpr { TUPLE6(STRING("fexpr1214"),$1,QUERY,$3,COLON,$5) }
	|	fexpr Inside LBRACE open_range_list RBRACE { TUPLE6(STRING("fexpr1215"),$1,Inside,LBRACE,$4,RBRACE) }
	|	INTEGER_NUMBER { (INTEGER_NUMBER $1) }
	|	FLOATING_HYPHEN_POINT_NUMBER { (FLOATING_HYPHEN_POINT_NUMBER $1) }
	|	timeNumAdjusted { ($1) }
	|	strAsInt { ($1) }
	|	LBRACE RBRACE { TUPLE3(STRING("fexpr1220"),LBRACE,RBRACE) }
	|	LBRACE constExpr LBRACE cateList RBRACE RBRACE { TUPLE7(STRING("fexpr1221"),LBRACE,$2,LBRACE,$4,RBRACE,RBRACE) }
	|	function_subroutine_callNoMethod { ($1) }
	|	fexpr DOT function_subroutine_callNoMethod { TUPLE4(STRING("fexpr1223"),$1,DOT,$3) }
	|	fexpr DOT array_methodWith { TUPLE4(STRING("fexpr1224"),$1,DOT,$3) }
	|	LPAREN expr RPAREN { TUPLE4(STRING("fexpr1225"),LPAREN,$2,RPAREN) }
	|	LPAREN expr COLON expr COLON expr RPAREN { TUPLE8(STRING("fexpr1226"),LPAREN,$2,COLON,$4,COLON,$6,RPAREN) }
	|	UNDERSCORE LPAREN expr RPAREN { TUPLE5(STRING("fexpr1227"),UNDERSCORE,LPAREN,$3,RPAREN) }
	|	simple_type QUOTE LPAREN expr RPAREN { TUPLE6(STRING("fexpr1228"),$1,QUOTE,LPAREN,$4,RPAREN) }
	|	Type LPAREN exprOrDataType RPAREN QUOTE LPAREN expr RPAREN { TUPLE9(STRING("fexpr1229"),Type,LPAREN,$3,RPAREN,QUOTE,LPAREN,$7,RPAREN) }
	|	Signed QUOTE LPAREN expr RPAREN { TUPLE6(STRING("fexpr1230"),Signed,QUOTE,LPAREN,$4,RPAREN) }
	|	Unsigned QUOTE LPAREN expr RPAREN { TUPLE6(STRING("fexpr1231"),Unsigned,QUOTE,LPAREN,$4,RPAREN) }
	|	String QUOTE LPAREN expr RPAREN { TUPLE6(STRING("fexpr1232"),String,QUOTE,LPAREN,$4,RPAREN) }
	|	Const QUOTE LPAREN expr RPAREN { TUPLE6(STRING("fexpr1233"),Const,QUOTE,LPAREN,$4,RPAREN) }
	|	fexpr QUOTE LPAREN expr RPAREN { TUPLE6(STRING("fexpr1234"),$1,QUOTE,LPAREN,$4,RPAREN) }
	|	DOLLAR { (DOLLAR) }
	|	Null { (Null) }
	|	fexprOkLvalue { ($1) }
	|	fexpr AMPERSAND_AMPERSAND_AMPERSAND fexpr { TUPLE4(STRING("fexpr1238"),$1,AMPERSAND_AMPERSAND_AMPERSAND,$3) }
	|	fexpr Dist LBRACE dist_list RBRACE { TUPLE6(STRING("fexpr1239"),$1,Dist,LBRACE,$4,RBRACE) }

exprNoStr: PLUS expr { TUPLE3(STRING("exprNoStr1240"),PLUS,$2) }
	|	HYPHEN expr { TUPLE3(STRING("exprNoStr1241"),HYPHEN,$2) }
	|	PLING expr { TUPLE3(STRING("exprNoStr1242"),PLING,$2) }
	|	AMPERSAND expr { TUPLE3(STRING("exprNoStr1243"),AMPERSAND,$2) }
	|	TILDE expr { TUPLE3(STRING("exprNoStr1244"),TILDE,$2) }
	|	VBAR expr { TUPLE3(STRING("exprNoStr1245"),VBAR,$2) }
	|	CARET expr { TUPLE3(STRING("exprNoStr1246"),CARET,$2) }
	|	TILDE_AMPERSAND expr { TUPLE3(STRING("exprNoStr1247"),TILDE_AMPERSAND,$2) }
	|	TILDE_VBAR expr { TUPLE3(STRING("exprNoStr1248"),TILDE_VBAR,$2) }
	|	CARET_TILDE expr { TUPLE3(STRING("exprNoStr1249"),CARET_TILDE,$2) }
	|	inc_or_dec_expression { ($1) }
	|	expr PLUS expr { TUPLE4(STRING("exprNoStr1251"),$1,PLUS,$3) }
	|	expr HYPHEN expr { TUPLE4(STRING("exprNoStr1252"),$1,HYPHEN,$3) }
	|	expr STAR expr { TUPLE4(STRING("exprNoStr1253"),$1,STAR,$3) }
	|	expr SLASH expr { TUPLE4(STRING("exprNoStr1254"),$1,SLASH,$3) }
	|	expr PERCENT expr { TUPLE4(STRING("exprNoStr1255"),$1,PERCENT,$3) }
	|	expr EQ_EQ expr { TUPLE4(STRING("exprNoStr1256"),$1,EQ_EQ,$3) }
	|	expr PLING_EQ expr { TUPLE4(STRING("exprNoStr1257"),$1,PLING_EQ,$3) }
	|	expr EQ_EQ_EQ expr { TUPLE4(STRING("exprNoStr1258"),$1,EQ_EQ_EQ,$3) }
	|	expr PLING_EQ_EQ expr { TUPLE4(STRING("exprNoStr1259"),$1,PLING_EQ_EQ,$3) }
	|	expr EQ_EQ_QUERY expr { TUPLE4(STRING("exprNoStr1260"),$1,EQ_EQ_QUERY,$3) }
	|	expr PLING_EQ_QUERY expr { TUPLE4(STRING("exprNoStr1261"),$1,PLING_EQ_QUERY,$3) }
	|	expr AMPERSAND_AMPERSAND expr { TUPLE4(STRING("exprNoStr1262"),$1,AMPERSAND_AMPERSAND,$3) }
	|	expr VBAR_VBAR expr { TUPLE4(STRING("exprNoStr1263"),$1,VBAR_VBAR,$3) }
	|	expr STAR_STAR expr { TUPLE4(STRING("exprNoStr1264"),$1,STAR_STAR,$3) }
	|	expr LESS expr { TUPLE4(STRING("exprNoStr1265"),$1,LESS,$3) }
	|	expr GREATER expr { TUPLE4(STRING("exprNoStr1266"),$1,GREATER,$3) }
	|	expr GT_EQ expr { TUPLE4(STRING("exprNoStr1267"),$1,GT_EQ,$3) }
	|	expr AMPERSAND expr { TUPLE4(STRING("exprNoStr1268"),$1,AMPERSAND,$3) }
	|	expr VBAR expr { TUPLE4(STRING("exprNoStr1269"),$1,VBAR,$3) }
	|	expr CARET expr { TUPLE4(STRING("exprNoStr1270"),$1,CARET,$3) }
	|	expr CARET_TILDE expr { TUPLE4(STRING("exprNoStr1271"),$1,CARET_TILDE,$3) }
	|	expr TILDE_VBAR expr { TUPLE4(STRING("exprNoStr1272"),$1,TILDE_VBAR,$3) }
	|	expr TILDE_AMPERSAND expr { TUPLE4(STRING("exprNoStr1273"),$1,TILDE_AMPERSAND,$3) }
	|	expr LT_LT expr { TUPLE4(STRING("exprNoStr1274"),$1,LT_LT,$3) }
	|	expr GT_GT expr { TUPLE4(STRING("exprNoStr1275"),$1,GT_GT,$3) }
	|	expr GT_GT_GT expr { TUPLE4(STRING("exprNoStr1276"),$1,GT_GT_GT,$3) }
	|	expr LT_HYPHEN_GT expr { TUPLE4(STRING("exprNoStr1277"),$1,LT_HYPHEN_GT,$3) }
	|	expr HYPHEN_GT expr { TUPLE4(STRING("exprNoStr1278"),$1,HYPHEN_GT,$3) }
	|	expr LT_EQ expr { TUPLE4(STRING("exprNoStr1279"),$1,LT_EQ,$3) }
	|	expr QUERY expr COLON expr { TUPLE6(STRING("exprNoStr1280"),$1,QUERY,$3,COLON,$5) }
	|	expr Inside LBRACE open_range_list RBRACE { TUPLE6(STRING("exprNoStr1281"),$1,Inside,LBRACE,$4,RBRACE) }
	|	INTEGER_NUMBER { (INTEGER_NUMBER $1) }
	|	FLOATING_HYPHEN_POINT_NUMBER { (FLOATING_HYPHEN_POINT_NUMBER $1) }
	|	timeNumAdjusted { ($1) }
	|	strAsIntIgnore { ($1) }
	|	LBRACE RBRACE { TUPLE3(STRING("exprNoStr1286"),LBRACE,RBRACE) }
	|	LBRACE constExpr LBRACE cateList RBRACE RBRACE { TUPLE7(STRING("exprNoStr1287"),LBRACE,$2,LBRACE,$4,RBRACE,RBRACE) }
	|	function_subroutine_callNoMethod { ($1) }
	|	expr DOT function_subroutine_callNoMethod { TUPLE4(STRING("exprNoStr1289"),$1,DOT,$3) }
	|	expr DOT array_methodWith { TUPLE4(STRING("exprNoStr1290"),$1,DOT,$3) }
	|	LPAREN expr RPAREN { TUPLE4(STRING("exprNoStr1291"),LPAREN,$2,RPAREN) }
	|	LPAREN expr COLON expr COLON expr RPAREN { TUPLE8(STRING("exprNoStr1292"),LPAREN,$2,COLON,$4,COLON,$6,RPAREN) }
	|	UNDERSCORE LPAREN expr RPAREN { TUPLE5(STRING("exprNoStr1293"),UNDERSCORE,LPAREN,$3,RPAREN) }
	|	simple_type QUOTE LPAREN expr RPAREN { TUPLE6(STRING("exprNoStr1294"),$1,QUOTE,LPAREN,$4,RPAREN) }
	|	Type LPAREN exprOrDataType RPAREN QUOTE LPAREN expr RPAREN { TUPLE9(STRING("exprNoStr1295"),Type,LPAREN,$3,RPAREN,QUOTE,LPAREN,$7,RPAREN) }
	|	Signed QUOTE LPAREN expr RPAREN { TUPLE6(STRING("exprNoStr1296"),Signed,QUOTE,LPAREN,$4,RPAREN) }
	|	Unsigned QUOTE LPAREN expr RPAREN { TUPLE6(STRING("exprNoStr1297"),Unsigned,QUOTE,LPAREN,$4,RPAREN) }
	|	String QUOTE LPAREN expr RPAREN { TUPLE6(STRING("exprNoStr1298"),String,QUOTE,LPAREN,$4,RPAREN) }
	|	Const QUOTE LPAREN expr RPAREN { TUPLE6(STRING("exprNoStr1299"),Const,QUOTE,LPAREN,$4,RPAREN) }
	|	expr QUOTE LPAREN expr RPAREN { TUPLE6(STRING("exprNoStr1300"),$1,QUOTE,LPAREN,$4,RPAREN) }
	|	DOLLAR { (DOLLAR) }
	|	Null { (Null) }
	|	exprOkLvalue { ($1) }
	|	expr AMPERSAND_AMPERSAND_AMPERSAND expr { TUPLE4(STRING("exprNoStr1304"),$1,AMPERSAND_AMPERSAND_AMPERSAND,$3) }
	|	expr Dist LBRACE dist_list RBRACE { TUPLE6(STRING("exprNoStr1305"),$1,Dist,LBRACE,$4,RBRACE) }

exprOkLvalue: exprScope { ($1) }
	|	LBRACE cateList RBRACE { TUPLE4(STRING("exprOkLvalue1307"),LBRACE,$2,RBRACE) }
	|	LBRACE cateList RBRACE LBRACK expr RBRACK { TUPLE7(STRING("exprOkLvalue1308"),LBRACE,$2,RBRACE,LBRACK,$5,RBRACK) }
	|	LBRACE cateList RBRACE LBRACK constExpr COLON constExpr RBRACK { TUPLE9(STRING("exprOkLvalue1309"),LBRACE,$2,RBRACE,LBRACK,$5,COLON,$7,RBRACK) }
	|	LBRACE cateList RBRACE LBRACK expr PLUS_COLON constExpr RBRACK { TUPLE9(STRING("exprOkLvalue1310"),LBRACE,$2,RBRACE,LBRACK,$5,PLUS_COLON,$7,RBRACK) }
	|	LBRACE cateList RBRACE LBRACK expr HYPHEN_COLON constExpr RBRACK { TUPLE9(STRING("exprOkLvalue1311"),LBRACE,$2,RBRACE,LBRACK,$5,HYPHEN_COLON,$7,RBRACK) }
	|	data_type assignment_pattern { TUPLE3(STRING("exprOkLvalue1312"),$1,$2) }
	|	assignment_pattern { ($1) }
	|	streaming_concatenation { ($1) }

fexprOkLvalue: fexprScope { ($1) }
	|	LBRACE cateList RBRACE { TUPLE4(STRING("fexprOkLvalue1316"),LBRACE,$2,RBRACE) }
	|	LBRACE cateList RBRACE LBRACK expr RBRACK { TUPLE7(STRING("fexprOkLvalue1317"),LBRACE,$2,RBRACE,LBRACK,$5,RBRACK) }
	|	LBRACE cateList RBRACE LBRACK constExpr COLON constExpr RBRACK { TUPLE9(STRING("fexprOkLvalue1318"),LBRACE,$2,RBRACE,LBRACK,$5,COLON,$7,RBRACK) }
	|	LBRACE cateList RBRACE LBRACK expr PLUS_COLON constExpr RBRACK { TUPLE9(STRING("fexprOkLvalue1319"),LBRACE,$2,RBRACE,LBRACK,$5,PLUS_COLON,$7,RBRACK) }
	|	LBRACE cateList RBRACE LBRACK expr HYPHEN_COLON constExpr RBRACK { TUPLE9(STRING("fexprOkLvalue1320"),LBRACE,$2,RBRACE,LBRACK,$5,HYPHEN_COLON,$7,RBRACK) }
	|	data_type assignment_pattern { TUPLE3(STRING("fexprOkLvalue1321"),$1,$2) }
	|	assignment_pattern { ($1) }
	|	streaming_concatenation { ($1) }

fexprLvalue: fexprOkLvalue { ($1) }

exprScope: This { (This) }
	|	DLR_root { (DLR_root) }
	|	idArrayed { ($1) }
	|	packageClassScope idArrayed { TUPLE3(STRING("exprScope1328"),$1,$2) }
	|	expr DOT idArrayed { TUPLE4(STRING("exprScope1329"),$1,DOT,$3) }
	|	expr DOT Super { TUPLE4(STRING("exprScope1330"),$1,DOT,Super) }
	|	Super { (Super) }

fexprScope: This { (This) }
	|	DLR_root { (DLR_root) }
	|	idArrayed { ($1) }
	|	packageClassScope idArrayed { TUPLE3(STRING("fexprScope1335"),$1,$2) }
	|	fexpr DOT idArrayed { TUPLE4(STRING("fexprScope1336"),$1,DOT,$3) }
	|	fexpr DOT Super { TUPLE4(STRING("fexprScope1337"),$1,DOT,Super) }
	|	Super { (Super) }

exprStrText: exprNoStr { ($1) }
	|	strAsText { ($1) }

cStrList: exprStrText { CONS1 ($1) }
	|	exprStrText COMMA cStrList { CONS3($1,COMMA,$3) }

cateList: stream_expression { CONS1 ($1) }
	|	cateList COMMA stream_expression { CONS3($1,COMMA,$3) }

exprListE: /* empty */ { EMPTY_TOKEN }
	|	exprList { ($1) }

exprList: expr { CONS1 ($1) }
	|	exprList COMMA expr { CONS3($1,COMMA,$3) }

exprDispList: expr { CONS1 ($1) }
	|	exprDispList COMMA expr { CONS3($1,COMMA,$3) }
	|	exprDispList COMMA { CONS2($1,COMMA) }

vrdList: idClassSel { CONS1 ($1) }
	|	vrdList COMMA idClassSel { CONS3($1,COMMA,$3) }

commaVRDListE: /* empty */ { EMPTY_TOKEN }
	|	COMMA vrdList { TUPLE3(STRING("commaVRDListE1355"),COMMA,$2) }

argsExprList: expr { CONS1 ($1) }
	|	argsExprList COMMA expr { CONS3($1,COMMA,$3) }

argsExprListE: argsExprOneE { ($1) }
	|	argsExprListE COMMA argsExprOneE { TUPLE4(STRING("argsExprListE1359"),$1,COMMA,$3) }

argsExprOneE: /* empty */ { EMPTY_TOKEN }
	|	expr { ($1) }

argsDottedList: argsDotted { CONS1 ($1) }
	|	argsDottedList COMMA argsDotted { CONS3($1,COMMA,$3) }

argsDotted: DOT idAny LPAREN RPAREN { TUPLE5(STRING("argsDotted1364"),DOT,$2,LPAREN,RPAREN) }
	|	DOT idAny LPAREN expr RPAREN { TUPLE6(STRING("argsDotted1365"),DOT,$2,LPAREN,$4,RPAREN) }

streaming_concatenation: LBRACE LT_LT stream_concatenation RBRACE { TUPLE5(STRING("streaming_concatenation1366"),LBRACE,LT_LT,$3,RBRACE) }
	|	LBRACE GT_GT stream_concatenation RBRACE { TUPLE5(STRING("streaming_concatenation1367"),LBRACE,GT_GT,$3,RBRACE) }
	|	LBRACE LT_LT stream_expressionOrDataType stream_concatenation RBRACE { TUPLE6(STRING("streaming_concatenation1368"),LBRACE,LT_LT,$3,$4,RBRACE) }
	|	LBRACE GT_GT stream_expressionOrDataType stream_concatenation RBRACE { TUPLE6(STRING("streaming_concatenation1369"),LBRACE,GT_GT,$3,$4,RBRACE) }

stream_concatenation: LBRACE cateList RBRACE { TUPLE4(STRING("stream_concatenation1370"),LBRACE,$2,RBRACE) }

stream_expression: expr { ($1) }

stream_expressionOrDataType: exprOrDataType { ($1) }

gateDecl: Buf delayE gateBufList SEMICOLON { TUPLE5(STRING("gateDecl1373"),Buf,$2,$3,SEMICOLON) }
	|	Bufif0 delayE gateBufif0List SEMICOLON { TUPLE5(STRING("gateDecl1374"),Bufif0,$2,$3,SEMICOLON) }
	|	Bufif1 delayE gateBufif1List SEMICOLON { TUPLE5(STRING("gateDecl1375"),Bufif1,$2,$3,SEMICOLON) }
	|	Not delayE gateNotList SEMICOLON { TUPLE5(STRING("gateDecl1376"),Not,$2,$3,SEMICOLON) }
	|	Notif0 delayE gateNotif0List SEMICOLON { TUPLE5(STRING("gateDecl1377"),Notif0,$2,$3,SEMICOLON) }
	|	Notif1 delayE gateNotif1List SEMICOLON { TUPLE5(STRING("gateDecl1378"),Notif1,$2,$3,SEMICOLON) }
	|	And delayE gateAndList SEMICOLON { TUPLE5(STRING("gateDecl1379"),And,$2,$3,SEMICOLON) }
	|	Nand delayE gateNandList SEMICOLON { TUPLE5(STRING("gateDecl1380"),Nand,$2,$3,SEMICOLON) }
	|	Or delayE gateOrList SEMICOLON { TUPLE5(STRING("gateDecl1381"),Or,$2,$3,SEMICOLON) }
	|	Nor delayE gateNorList SEMICOLON { TUPLE5(STRING("gateDecl1382"),Nor,$2,$3,SEMICOLON) }
	|	Xor delayE gateXorList SEMICOLON { TUPLE5(STRING("gateDecl1383"),Xor,$2,$3,SEMICOLON) }
	|	Xnor delayE gateXnorList SEMICOLON { TUPLE5(STRING("gateDecl1384"),Xnor,$2,$3,SEMICOLON) }
	|	Pullup delayE gatePullupList SEMICOLON { TUPLE5(STRING("gateDecl1385"),Pullup,$2,$3,SEMICOLON) }
	|	Pulldown delayE gatePulldownList SEMICOLON { TUPLE5(STRING("gateDecl1386"),Pulldown,$2,$3,SEMICOLON) }
	|	Nmos delayE gateBufif1List SEMICOLON { TUPLE5(STRING("gateDecl1387"),Nmos,$2,$3,SEMICOLON) }
	|	Pmos delayE gateBufif0List SEMICOLON { TUPLE5(STRING("gateDecl1388"),Pmos,$2,$3,SEMICOLON) }
	|	Tran delayE gateUnsupList SEMICOLON { TUPLE5(STRING("gateDecl1389"),Tran,$2,$3,SEMICOLON) }
	|	Rcmos delayE gateUnsupList SEMICOLON { TUPLE5(STRING("gateDecl1390"),Rcmos,$2,$3,SEMICOLON) }
	|	Cmos delayE gateUnsupList SEMICOLON { TUPLE5(STRING("gateDecl1391"),Cmos,$2,$3,SEMICOLON) }
	|	Rnmos delayE gateUnsupList SEMICOLON { TUPLE5(STRING("gateDecl1392"),Rnmos,$2,$3,SEMICOLON) }
	|	Rpmos delayE gateUnsupList SEMICOLON { TUPLE5(STRING("gateDecl1393"),Rpmos,$2,$3,SEMICOLON) }
	|	Rtran delayE gateUnsupList SEMICOLON { TUPLE5(STRING("gateDecl1394"),Rtran,$2,$3,SEMICOLON) }
	|	Rtranif0 delayE gateUnsupList SEMICOLON { TUPLE5(STRING("gateDecl1395"),Rtranif0,$2,$3,SEMICOLON) }
	|	Rtranif1 delayE gateUnsupList SEMICOLON { TUPLE5(STRING("gateDecl1396"),Rtranif1,$2,$3,SEMICOLON) }
	|	Tranif0 delayE gateUnsupList SEMICOLON { TUPLE5(STRING("gateDecl1397"),Tranif0,$2,$3,SEMICOLON) }
	|	Tranif1 delayE gateUnsupList SEMICOLON { TUPLE5(STRING("gateDecl1398"),Tranif1,$2,$3,SEMICOLON) }

gateBufList: gateBuf { CONS1 ($1) }
	|	gateBufList COMMA gateBuf { CONS3($1,COMMA,$3) }

gateBufif0List: gateBufif0 { CONS1 ($1) }
	|	gateBufif0List COMMA gateBufif0 { CONS3($1,COMMA,$3) }

gateBufif1List: gateBufif1 { CONS1 ($1) }
	|	gateBufif1List COMMA gateBufif1 { CONS3($1,COMMA,$3) }

gateNotList: gateNot { CONS1 ($1) }
	|	gateNotList COMMA gateNot { CONS3($1,COMMA,$3) }

gateNotif0List: gateNotif0 { CONS1 ($1) }
	|	gateNotif0List COMMA gateNotif0 { CONS3($1,COMMA,$3) }

gateNotif1List: gateNotif1 { CONS1 ($1) }
	|	gateNotif1List COMMA gateNotif1 { CONS3($1,COMMA,$3) }

gateAndList: gateAnd { CONS1 ($1) }
	|	gateAndList COMMA gateAnd { CONS3($1,COMMA,$3) }

gateNandList: gateNand { CONS1 ($1) }
	|	gateNandList COMMA gateNand { CONS3($1,COMMA,$3) }

gateOrList: gateOr { CONS1 ($1) }
	|	gateOrList COMMA gateOr { CONS3($1,COMMA,$3) }

gateNorList: gateNor { CONS1 ($1) }
	|	gateNorList COMMA gateNor { CONS3($1,COMMA,$3) }

gateXorList: gateXor { CONS1 ($1) }
	|	gateXorList COMMA gateXor { CONS3($1,COMMA,$3) }

gateXnorList: gateXnor { CONS1 ($1) }
	|	gateXnorList COMMA gateXnor { CONS3($1,COMMA,$3) }

gatePullupList: gatePullup { CONS1 ($1) }
	|	gatePullupList COMMA gatePullup { CONS3($1,COMMA,$3) }

gatePulldownList: gatePulldown { CONS1 ($1) }
	|	gatePulldownList COMMA gatePulldown { CONS3($1,COMMA,$3) }

gateUnsupList: gateUnsup { CONS1 ($1) }
	|	gateUnsupList COMMA gateUnsup { CONS3($1,COMMA,$3) }

gateRangeE: instRangeListE { ($1) }

gateBuf: gateFront variable_lvalue COMMA gatePinExpr RPAREN { TUPLE6(STRING("gateBuf1430"),$1,$2,COMMA,$4,RPAREN) }

gateBufif0: gateFront variable_lvalue COMMA gatePinExpr COMMA gatePinExpr RPAREN { TUPLE8(STRING("gateBufif01431"),$1,$2,COMMA,$4,COMMA,$6,RPAREN) }

gateBufif1: gateFront variable_lvalue COMMA gatePinExpr COMMA gatePinExpr RPAREN { TUPLE8(STRING("gateBufif11432"),$1,$2,COMMA,$4,COMMA,$6,RPAREN) }

gateNot: gateFront variable_lvalue COMMA gatePinExpr RPAREN { TUPLE6(STRING("gateNot1433"),$1,$2,COMMA,$4,RPAREN) }

gateNotif0: gateFront variable_lvalue COMMA gatePinExpr COMMA gatePinExpr RPAREN { TUPLE8(STRING("gateNotif01434"),$1,$2,COMMA,$4,COMMA,$6,RPAREN) }

gateNotif1: gateFront variable_lvalue COMMA gatePinExpr COMMA gatePinExpr RPAREN { TUPLE8(STRING("gateNotif11435"),$1,$2,COMMA,$4,COMMA,$6,RPAREN) }

gateAnd: gateFront variable_lvalue COMMA gateAndPinList RPAREN { TUPLE6(STRING("gateAnd1436"),$1,$2,COMMA,$4,RPAREN) }

gateNand: gateFront variable_lvalue COMMA gateAndPinList RPAREN { TUPLE6(STRING("gateNand1437"),$1,$2,COMMA,$4,RPAREN) }

gateOr: gateFront variable_lvalue COMMA gateOrPinList RPAREN { TUPLE6(STRING("gateOr1438"),$1,$2,COMMA,$4,RPAREN) }

gateNor: gateFront variable_lvalue COMMA gateOrPinList RPAREN { TUPLE6(STRING("gateNor1439"),$1,$2,COMMA,$4,RPAREN) }

gateXor: gateFront variable_lvalue COMMA gateXorPinList RPAREN { TUPLE6(STRING("gateXor1440"),$1,$2,COMMA,$4,RPAREN) }

gateXnor: gateFront variable_lvalue COMMA gateXorPinList RPAREN { TUPLE6(STRING("gateXnor1441"),$1,$2,COMMA,$4,RPAREN) }

gatePullup: gateFront variable_lvalue RPAREN { TUPLE4(STRING("gatePullup1442"),$1,$2,RPAREN) }

gatePulldown: gateFront variable_lvalue RPAREN { TUPLE4(STRING("gatePulldown1443"),$1,$2,RPAREN) }

gateUnsup: gateFront gateUnsupPinList RPAREN { TUPLE4(STRING("gateUnsup1444"),$1,$2,RPAREN) }

gateFront: id gateRangeE LPAREN { TUPLE4(STRING("gateFront1445"),$1,$2,LPAREN) }
	|	gateRangeE LPAREN { TUPLE3(STRING("gateFront1446"),$1,LPAREN) }

gateAndPinList: gatePinExpr { CONS1 ($1) }
	|	gateAndPinList COMMA gatePinExpr { CONS3($1,COMMA,$3) }

gateOrPinList: gatePinExpr { CONS1 ($1) }
	|	gateOrPinList COMMA gatePinExpr { CONS3($1,COMMA,$3) }

gateXorPinList: gatePinExpr { CONS1 ($1) }
	|	gateXorPinList COMMA gatePinExpr { CONS3($1,COMMA,$3) }

gateUnsupPinList: gatePinExpr { CONS1 ($1) }
	|	gateUnsupPinList COMMA gatePinExpr { CONS3($1,COMMA,$3) }

gatePinExpr: expr { ($1) }

strength: STRENGTH_keyword_LPAREN_strong1_SLASH_etc_RPAREN { (STRENGTH_keyword_LPAREN_strong1_SLASH_etc_RPAREN) }
	|	Supply0 { (Supply0) }
	|	Supply1 { (Supply1) }

strengthSpecE: /* empty */ { EMPTY_TOKEN }
	|	strengthSpec { ($1) }

strengthSpec: LPAREN_HYPHEN_for_HYPHEN_strength strength RPAREN { TUPLE4(STRING("strengthSpec1461"),LPAREN_HYPHEN_for_HYPHEN_strength,$2,RPAREN) }
	|	LPAREN_HYPHEN_for_HYPHEN_strength strength COMMA strength RPAREN { TUPLE6(STRING("strengthSpec1462"),LPAREN_HYPHEN_for_HYPHEN_strength,$2,COMMA,$4,RPAREN) }

combinational_body: Table tableEntryList Endtable { TUPLE4(STRING("combinational_body1463"),Table,$2,Endtable) }

tableEntryList: tableEntry { CONS1 ($1) }
	|	tableEntryList tableEntry { CONS2($1,$2) }

tableEntry: TABLE_LINE { (TABLE_LINE) }
	|	ERROR_TOKEN { (ERROR_TOKEN) }

specify_block: Specify specifyJunkList Endspecify { TUPLE4(STRING("specify_block1468"),Specify,$2,Endspecify) }
	|	Specify Endspecify { TUPLE3(STRING("specify_block1469"),Specify,Endspecify) }

specifyJunkList: specifyJunk { CONS1 ($1) }
	|	specifyJunkList specifyJunk { CONS2($1,$2) }

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
	|	Alias { (Alias) }
	|	Always { (Always) }
	|	Always_comb { (Always_comb) }
	|	Always_ff { (Always_ff) }
	|	Always_latch { (Always_latch) }
	|	And { (And) }
	|	Assert { (Assert) }
	|	Assign { (Assign) }
	|	Assume { (Assume) }
	|	Automatic { (Automatic) }
	|	Before { (Before) }
	|	Begin { (Begin) }
	|	Bind { (Bind) }
	|	Bit { (Bit) }
	|	Break { (Break) }
	|	Buf { (Buf) }
	|	Bufif0 { (Bufif0) }
	|	Bufif1 { (Bufif1) }
	|	Byte { (Byte) }
	|	Case { (Case) }
	|	Casex { (Casex) }
	|	Casez { (Casez) }
	|	Chandle { (Chandle) }
	|	Class { (Class) }
	|	Clocking { (Clocking) }
	|	Cmos { (Cmos) }
	|	Constraint { (Constraint) }
	|	Const { (Const) }
	|	Const_HYPHEN_in_HYPHEN_lex { (Const_HYPHEN_in_HYPHEN_lex) }
	|	Const_HYPHEN_then_HYPHEN_ref { (Const_HYPHEN_then_HYPHEN_ref) }
	|	Context { (Context) }
	|	Continue { (Continue) }
	|	Cover { (Cover) }
	|	Deassign { (Deassign) }
	|	Default { (Default) }
	|	Defparam { (Defparam) }
	|	Disable { (Disable) }
	|	Dist { (Dist) }
	|	Do { (Do) }
	|	DLR_acos { (DLR_acos) }
	|	DLR_acosh { (DLR_acosh) }
	|	DLR_asin { (DLR_asin) }
	|	DLR_asinh { (DLR_asinh) }
	|	DLR_atan { (DLR_atan) }
	|	DLR_atan2 { (DLR_atan2) }
	|	DLR_atanh { (DLR_atanh) }
	|	DLR_bits { (DLR_bits) }
	|	DLR_bitstoreal { (DLR_bitstoreal) }
	|	DLR_bitstoshortreal { (DLR_bitstoshortreal) }
	|	DLR_c { (DLR_c) }
	|	DLR_cast { (DLR_cast) }
	|	DLR_ceil { (DLR_ceil) }
	|	DLR_changed { (DLR_changed) }
	|	DLR_clog2 { (DLR_clog2) }
	|	DLR_cos { (DLR_cos) }
	|	DLR_cosh { (DLR_cosh) }
	|	DLR_countbits { (DLR_countbits) }
	|	DLR_countones { (DLR_countones) }
	|	DLR_dimensions { (DLR_dimensions) }
	|	DLR_display { (DLR_display) }
	|	DLR_displayb { (DLR_displayb) }
	|	DLR_displayh { (DLR_displayh) }
	|	DLR_displayo { (DLR_displayo) }
	|	DLR_dumpall { (DLR_dumpall) }
	|	DLR_dumpfile { (DLR_dumpfile) }
	|	DLR_dumpflush { (DLR_dumpflush) }
	|	DLR_dumplimit { (DLR_dumplimit) }
	|	DLR_dumpoff { (DLR_dumpoff) }
	|	DLR_dumpon { (DLR_dumpon) }
	|	DLR_dumpports { (DLR_dumpports) }
	|	DLR_dumpvars { (DLR_dumpvars) }
	|	DLR_error { (DLR_error) }
	|	DLR_exit { (DLR_exit) }
	|	DLR_exp { (DLR_exp) }
	|	DLR_fatal { (DLR_fatal) }
	|	DLR_fclose { (DLR_fclose) }
	|	DLR_fdisplay { (DLR_fdisplay) }
	|	DLR_fdisplayb { (DLR_fdisplayb) }
	|	DLR_fdisplayh { (DLR_fdisplayh) }
	|	DLR_fdisplayo { (DLR_fdisplayo) }
	|	DLR_fell { (DLR_fell) }
	|	DLR_feof { (DLR_feof) }
	|	DLR_ferror { (DLR_ferror) }
	|	DLR_fflush { (DLR_fflush) }
	|	DLR_fgetc { (DLR_fgetc) }
	|	DLR_fgets { (DLR_fgets) }
	|	DLR_finish { (DLR_finish) }
	|	DLR_floor { (DLR_floor) }
	|	DLR_fmonitor { (DLR_fmonitor) }
	|	DLR_fmonitorb { (DLR_fmonitorb) }
	|	DLR_fmonitorh { (DLR_fmonitorh) }
	|	DLR_fmonitoro { (DLR_fmonitoro) }
	|	DLR_fopen { (DLR_fopen) }
	|	DLR_fread { (DLR_fread) }
	|	DLR_frewind { (DLR_frewind) }
	|	DLR_fscanf { (DLR_fscanf) }
	|	DLR_fseek { (DLR_fseek) }
	|	DLR_fstrobe { (DLR_fstrobe) }
	|	DLR_fstrobeb { (DLR_fstrobeb) }
	|	DLR_fstrobeh { (DLR_fstrobeh) }
	|	DLR_fstrobeo { (DLR_fstrobeo) }
	|	DLR_ftell { (DLR_ftell) }
	|	DLR_fwrite { (DLR_fwrite) }
	|	DLR_fwriteb { (DLR_fwriteb) }
	|	DLR_fwriteh { (DLR_fwriteh) }
	|	DLR_fwriteo { (DLR_fwriteo) }
	|	DLR_high { (DLR_high) }
	|	DLR_hypot { (DLR_hypot) }
	|	DLR_increment { (DLR_increment) }
	|	DLR_info { (DLR_info) }
	|	DLR_isunbounded { (DLR_isunbounded) }
	|	DLR_isunknown { (DLR_isunknown) }
	|	DLR_itor { (DLR_itor) }
	|	DLR_left { (DLR_left) }
	|	DLR_ln { (DLR_ln) }
	|	DLR_log10 { (DLR_log10) }
	|	DLR_low { (DLR_low) }
	|	DLR_monitor { (DLR_monitor) }
	|	DLR_monitorb { (DLR_monitorb) }
	|	DLR_monitorh { (DLR_monitorh) }
	|	DLR_monitoro { (DLR_monitoro) }
	|	DLR_monitoroff { (DLR_monitoroff) }
	|	DLR_monitoron { (DLR_monitoron) }
	|	DLR_onehot { (DLR_onehot) }
	|	DLR_onehot0 { (DLR_onehot0) }
	|	DLR_past { (DLR_past) }
	|	DLR_pow { (DLR_pow) }
	|	DLR_printtimescale { (DLR_printtimescale) }
	|	DLR_random { (DLR_random) }
	|	DLR_readmemb { (DLR_readmemb) }
	|	DLR_readmemh { (DLR_readmemh) }
	|	DLR_realtime { (DLR_realtime) }
	|	DLR_realtobits { (DLR_realtobits) }
	|	DLR_rewind { (DLR_rewind) }
	|	DLR_right { (DLR_right) }
	|	DLR_root { (DLR_root) }
	|	DLR_rose { (DLR_rose) }
	|	DLR_rtoi { (DLR_rtoi) }
	|	DLR_sampled { (DLR_sampled) }
	|	DLR_sformat { (DLR_sformat) }
	|	DLR_sformatf { (DLR_sformatf) }
	|	DLR_shortrealtobits { (DLR_shortrealtobits) }
	|	DLR_signed { (DLR_signed) }
	|	DLR_sin { (DLR_sin) }
	|	DLR_sinh { (DLR_sinh) }
	|	DLR_size { (DLR_size) }
	|	DLR_sqrt { (DLR_sqrt) }
	|	DLR_sscanf { (DLR_sscanf) }
	|	DLR_stable { (DLR_stable) }
	|	DLR_stime { (DLR_stime) }
	|	DLR_stop { (DLR_stop) }
	|	DLR_strobe { (DLR_strobe) }
	|	DLR_strobeb { (DLR_strobeb) }
	|	DLR_strobeh { (DLR_strobeh) }
	|	DLR_strobeo { (DLR_strobeo) }
	|	DLR_swrite { (DLR_swrite) }
	|	DLR_swriteb { (DLR_swriteb) }
	|	DLR_swriteh { (DLR_swriteh) }
	|	DLR_swriteo { (DLR_swriteo) }
	|	DLR_system { (DLR_system) }
	|	DLR_tan { (DLR_tan) }
	|	DLR_tanh { (DLR_tanh) }
	|	DLR_test_DLR_plusargs { (DLR_test_DLR_plusargs) }
	|	DLR_time { (DLR_time) }
	|	DLR_timeformat { (DLR_timeformat) }
	|	DLR_typename { (DLR_typename) }
	|	DLR_ungetc { (DLR_ungetc) }
	|	DLR_unit { (DLR_unit) }
	|	DLR_unpacked_dimensions { (DLR_unpacked_dimensions) }
	|	DLR_unsigned { (DLR_unsigned) }
	|	DLR_urandom { (DLR_urandom) }
	|	DLR_urandom_range { (DLR_urandom_range) }
	|	DLR_value_DLR_plusargs { (DLR_value_DLR_plusargs) }
	|	DLR_warning { (DLR_warning) }
	|	DLR_write { (DLR_write) }
	|	DLR_writeb { (DLR_writeb) }
	|	DLR_writeh { (DLR_writeh) }
	|	DLR_writememb { (DLR_writememb) }
	|	DLR_writememh { (DLR_writememh) }
	|	DLR_writeo { (DLR_writeo) }
	|	Edge { (Edge) }
	|	Else { (Else) }
	|	End { (End) }
	|	Endcase { (Endcase) }
	|	Endclass { (Endclass) }
	|	Endclocking { (Endclocking) }
	|	Endfunction { (Endfunction) }
	|	Endgenerate { (Endgenerate) }
	|	Endinterface { (Endinterface) }
	|	Endmodule { (Endmodule) }
	|	Endpackage { (Endpackage) }
	|	Endprimitive { (Endprimitive) }
	|	Endprogram { (Endprogram) }
	|	Endproperty { (Endproperty) }
	|	Endtable { (Endtable) }
	|	Endtask { (Endtask) }
	|	Enum { (Enum) }
	|	Event { (Event) }
	|	Export { (Export) }
	|	Extends { (Extends) }
	|	Extern { (Extern) }
	|	Final { (Final) }
	|	For { (For) }
	|	Force { (Force) }
	|	Foreach { (Foreach) }
	|	Forever { (Forever) }
	|	Fork { (Fork) }
	|	Forkjoin { (Forkjoin) }
	|	Function { (Function) }
	|	Generate { (Generate) }
	|	Genvar { (Genvar) }
	|	Global_HYPHEN_then_HYPHEN_clocking { (Global_HYPHEN_then_HYPHEN_clocking) }
	|	Global { (Global) }
	|	Global_HYPHEN_in_HYPHEN_lex { (Global_HYPHEN_in_HYPHEN_lex) }
	|	If { (If) }
	|	Iff { (Iff) }
	|	Implements { (Implements) }
	|	Import { (Import) }
	|	Initial { (Initial) }
	|	Inout { (Inout) }
	|	Input { (Input) }
	|	Inside { (Inside) }
	|	Int { (Int) }
	|	Integer { (Integer) }
	|	Interface { (Interface) }
	|	Join { (Join) }
	|	Join_any { (Join_any) }
	|	Join_none { (Join_none) }
	|	Localparam { (Localparam) }
	|	Local_HYPHEN_then_HYPHEN_COLON_COLON { (Local_HYPHEN_then_HYPHEN_COLON_COLON) }
	|	Local { (Local) }
	|	Local_HYPHEN_in_HYPHEN_lex { (Local_HYPHEN_in_HYPHEN_lex) }
	|	Logic { (Logic) }
	|	Longint { (Longint) }
	|	Modport { (Modport) }
	|	Module { (Module) }
	|	Nand { (Nand) }
	|	Negedge { (Negedge) }
	|	New { (New) }
	|	New_HYPHEN_in_HYPHEN_lex { (New_HYPHEN_in_HYPHEN_lex) }
	|	New_HYPHEN_then_HYPHEN_paren { (New_HYPHEN_then_HYPHEN_paren) }
	|	Nmos { (Nmos) }
	|	Nor { (Nor) }
	|	Not { (Not) }
	|	Notif0 { (Notif0) }
	|	Notif1 { (Notif1) }
	|	Null { (Null) }
	|	Or { (Or) }
	|	Output { (Output) }
	|	Package { (Package) }
	|	Packed { (Packed) }
	|	Parameter { (Parameter) }
	|	Pmos { (Pmos) }
	|	Posedge { (Posedge) }
	|	Primitive { (Primitive) }
	|	Priority { (Priority) }
	|	Program { (Program) }
	|	Property { (Property) }
	|	Protected { (Protected) }
	|	Pulldown { (Pulldown) }
	|	Pullup { (Pullup) }
	|	Pure { (Pure) }
	|	AMPERSAND_AMPERSAND { (AMPERSAND_AMPERSAND) }
	|	AMPERSAND_AMPERSAND_AMPERSAND { (AMPERSAND_AMPERSAND_AMPERSAND) }
	|	AMPERSAND_EQ { (AMPERSAND_EQ) }
	|	STAR_GT { (STAR_GT) }
	|	AT_AT { (AT_AT) }
	|	LBRACK_EQ { (LBRACK_EQ) }
	|	LBRACK_HYPHEN_GT { (LBRACK_HYPHEN_GT) }
	|	LBRACK_STAR { (LBRACK_STAR) }
	|	EQ_EQ_EQ { (EQ_EQ_EQ) }
	|	PLING_EQ_EQ { (PLING_EQ_EQ) }
	|	COLON_COLON { (COLON_COLON) }
	|	COLON_SLASH { (COLON_SLASH) }
	|	COLON_EQ { (COLON_EQ) }
	|	COLON_HYPHEN_begin { (COLON_HYPHEN_begin) }
	|	COLON_HYPHEN_fork { (COLON_HYPHEN_fork) }
	|	SLASH_EQ { (SLASH_EQ) }
	|	DOT_STAR { (DOT_STAR) }
	|	EQ_GT { (EQ_GT) }
	|	EQ_EQ { (EQ_EQ) }
	|	GT_EQ { (GT_EQ) }
	|	LT_EQ { (LT_EQ) }
	|	LT_EQ_HYPHEN_ignored { (LT_EQ_HYPHEN_ignored) }
	|	LT_HYPHEN_GT { (LT_HYPHEN_GT) }
	|	HYPHEN_COLON { (HYPHEN_COLON) }
	|	HYPHEN_EQ { (HYPHEN_EQ) }
	|	HYPHEN_GT { (HYPHEN_GT) }
	|	HYPHEN_GT_GT { (HYPHEN_GT_GT) }
	|	HYPHEN_HYPHEN { (HYPHEN_HYPHEN) }
	|	PERCENT_EQ { (PERCENT_EQ) }
	|	TILDE_AMPERSAND { (TILDE_AMPERSAND) }
	|	TILDE_VBAR { (TILDE_VBAR) }
	|	PLING_EQ { (PLING_EQ) }
	|	VBAR_EQ { (VBAR_EQ) }
	|	VBAR_EQ_GT { (VBAR_EQ_GT) }
	|	VBAR_HYPHEN_GT { (VBAR_HYPHEN_GT) }
	|	VBAR_VBAR { (VBAR_VBAR) }
	|	LPAREN_HYPHEN_for_HYPHEN_strength { (LPAREN_HYPHEN_for_HYPHEN_strength) }
	|	PLUS_COLON { (PLUS_COLON) }
	|	PLUS_EQ { (PLUS_EQ) }
	|	PLUS_PLUS { (PLUS_PLUS) }
	|	HASH_HASH { (HASH_HASH) }
	|	STAR_STAR { (STAR_STAR) }
	|	LT_LT { (LT_LT) }
	|	LT_LT_EQ { (LT_LT_EQ) }
	|	GT_GT { (GT_GT) }
	|	GT_GT_EQ { (GT_GT_EQ) }
	|	GT_GT_GT { (GT_GT_GT) }
	|	GT_GT_GT_EQ { (GT_GT_GT_EQ) }
	|	QUOTE { (QUOTE) }
	|	QUOTE_LBRACE { (QUOTE_LBRACE) }
	|	STAR_EQ { (STAR_EQ) }
	|	EQ_EQ_QUERY { (EQ_EQ_QUERY) }
	|	PLING_EQ_QUERY { (PLING_EQ_QUERY) }
	|	CARET_TILDE { (CARET_TILDE) }
	|	CARET_EQ { (CARET_EQ) }
	|	Rand { (Rand) }
	|	Randc { (Randc) }
	|	Randcase { (Randcase) }
	|	Randomize { (Randomize) }
	|	Rcmos { (Rcmos) }
	|	Real { (Real) }
	|	Realtime { (Realtime) }
	|	Ref { (Ref) }
	|	Reg { (Reg) }
	|	Release { (Release) }
	|	Repeat { (Repeat) }
	|	Restrict { (Restrict) }
	|	Return { (Return) }
	|	Rnmos { (Rnmos) }
	|	Rpmos { (Rpmos) }
	|	Rtran { (Rtran) }
	|	Rtranif0 { (Rtranif0) }
	|	Rtranif1 { (Rtranif1) }
	|	Scalared { (Scalared) }
	|	Shortint { (Shortint) }
	|	Shortreal { (Shortreal) }
	|	Signed { (Signed) }
	|	Soft { (Soft) }
	|	Solve { (Solve) }
	|	Specparam { (Specparam) }
	|	Static_HYPHEN_then_HYPHEN_constraint { (Static_HYPHEN_then_HYPHEN_constraint) }
	|	Static { (Static) }
	|	Static_HYPHEN_in_HYPHEN_lex { (Static_HYPHEN_in_HYPHEN_lex) }
	|	String { (String) }
	|	Struct { (Struct) }
	|	Super { (Super) }
	|	Supply0 { (Supply0) }
	|	Supply1 { (Supply1) }
	|	Table { (Table) }
	|	Task { (Task) }
	|	This { (This) }
	|	Time { (Time) }
	|	Timeprecision { (Timeprecision) }
	|	Timeunit { (Timeunit) }
	|	Tran { (Tran) }
	|	Tranif0 { (Tranif0) }
	|	Tranif1 { (Tranif1) }
	|	Tri { (Tri) }
	|	Tri0 { (Tri0) }
	|	Tri1 { (Tri1) }
	|	Triand { (Triand) }
	|	Trior { (Trior) }
	|	Trireg { (Trireg) }
	|	True { (True) }
	|	Type { (Type) }
	|	Typedef { (Typedef) }
	|	Union { (Union) }
	|	Unique { (Unique) }
	|	Unique0 { (Unique0) }
	|	Unsigned { (Unsigned) }
	|	Var { (Var) }
	|	Vectored { (Vectored) }
	|	Virtual_HYPHEN_then_HYPHEN_class { (Virtual_HYPHEN_then_HYPHEN_class) }
	|	Virtual { (Virtual) }
	|	Virtual_HYPHEN_then_HYPHEN_interface { (Virtual_HYPHEN_then_HYPHEN_interface) }
	|	Virtual_HYPHEN_in_HYPHEN_lex { (Virtual_HYPHEN_in_HYPHEN_lex) }
	|	Virtual_HYPHEN_then_HYPHEN_identifier { (Virtual_HYPHEN_then_HYPHEN_identifier) }
	|	Clocker { (Clocker) }
	|	Clock_enable { (Clock_enable) }
	|	Coverage_block_off { (Coverage_block_off) }
	|	Coverage_off { (Coverage_off) }
	|	Coverage_on { (Coverage_on) }
	|	HYPHEN_HYPHEN_block { (HYPHEN_HYPHEN_block) }
	|	HYPHEN_HYPHEN_file { (HYPHEN_HYPHEN_file) }
	|	HYPHEN_HYPHEN_function { (HYPHEN_HYPHEN_function) }
	|	HYPHEN_HYPHEN_lines { (HYPHEN_HYPHEN_lines) }
	|	HYPHEN_HYPHEN_match { (HYPHEN_HYPHEN_match) }
	|	HYPHEN_HYPHEN_module { (HYPHEN_HYPHEN_module) }
	|	HYPHEN_HYPHEN_msg { (HYPHEN_HYPHEN_msg) }
	|	HYPHEN_HYPHEN_rule { (HYPHEN_HYPHEN_rule) }
	|	HYPHEN_HYPHEN_task { (HYPHEN_HYPHEN_task) }
	|	HYPHEN_HYPHEN_var { (HYPHEN_HYPHEN_var) }
	|	Full_case { (Full_case) }
	|	Hier_block { (Hier_block) }
	|	Inline { (Inline) }
	|	Isolate_assignments { (Isolate_assignments) }
	|	Lint_off { (Lint_off) }
	|	Lint_on { (Lint_on) }
	|	No_clocker { (No_clocker) }
	|	No_inline { (No_inline) }
	|	Parallel_case { (Parallel_case) }
	|	Public { (Public) }
	|	Public_flat { (Public_flat) }
	|	Public_flat_rd { (Public_flat_rd) }
	|	Public_flat_rw { (Public_flat_rw) }
	|	Public_module { (Public_module) }
	|	Sc_bv { (Sc_bv) }
	|	Sformat { (Sformat) }
	|	Split_var { (Split_var) }
	|	Tracing_off { (Tracing_off) }
	|	Tracing_on { (Tracing_on) }
	|	SLASH_STAR_verilator_clocker_STAR_SLASH { (SLASH_STAR_verilator_clocker_STAR_SLASH) }
	|	SLASH_STAR_verilator_clock_enable_STAR_SLASH { (SLASH_STAR_verilator_clock_enable_STAR_SLASH) }
	|	SLASH_STAR_verilator_coverage_block_off_STAR_SLASH { (SLASH_STAR_verilator_coverage_block_off_STAR_SLASH) }
	|	SLASH_STAR_verilator_full_case_STAR_SLASH { (SLASH_STAR_verilator_full_case_STAR_SLASH) }
	|	SLASH_STAR_verilator_hier_block_STAR_SLASH { (SLASH_STAR_verilator_hier_block_STAR_SLASH) }
	|	SLASH_STAR_verilator_inline_module_STAR_SLASH { (SLASH_STAR_verilator_inline_module_STAR_SLASH) }
	|	SLASH_STAR_verilator_isolate_assignments_STAR_SLASH { (SLASH_STAR_verilator_isolate_assignments_STAR_SLASH) }
	|	SLASH_STAR_verilator_no_clocker_STAR_SLASH { (SLASH_STAR_verilator_no_clocker_STAR_SLASH) }
	|	SLASH_STAR_verilator_no_inline_module_STAR_SLASH { (SLASH_STAR_verilator_no_inline_module_STAR_SLASH) }
	|	SLASH_STAR_verilator_no_inline_task_STAR_SLASH { (SLASH_STAR_verilator_no_inline_task_STAR_SLASH) }
	|	SLASH_STAR_verilator_parallel_case_STAR_SLASH { (SLASH_STAR_verilator_parallel_case_STAR_SLASH) }
	|	SLASH_STAR_verilator_public_STAR_SLASH { (SLASH_STAR_verilator_public_STAR_SLASH) }
	|	SLASH_STAR_verilator_public_flat_STAR_SLASH { (SLASH_STAR_verilator_public_flat_STAR_SLASH) }
	|	SLASH_STAR_verilator_public_flat_rd_STAR_SLASH { (SLASH_STAR_verilator_public_flat_rd_STAR_SLASH) }
	|	SLASH_STAR_verilator_public_flat_rw_STAR_SLASH { (SLASH_STAR_verilator_public_flat_rw_STAR_SLASH) }
	|	SLASH_STAR_verilator_public_module_STAR_SLASH { (SLASH_STAR_verilator_public_module_STAR_SLASH) }
	|	SLASH_STAR_verilator_sc_bv_STAR_SLASH { (SLASH_STAR_verilator_sc_bv_STAR_SLASH) }
	|	SLASH_STAR_verilator_sformat_STAR_SLASH { (SLASH_STAR_verilator_sformat_STAR_SLASH) }
	|	SLASH_STAR_verilator_split_var_STAR_SLASH { (SLASH_STAR_verilator_split_var_STAR_SLASH) }
	|	SLASH_STAR_verilator_tag_STAR_SLASH { (SLASH_STAR_verilator_tag_STAR_SLASH) }
	|	Void { (Void) }
	|	Wait { (Wait) }
	|	Wand { (Wand) }
	|	While { (While) }
	|	Wire { (Wire) }
	|	With_HYPHEN_then_HYPHEN_LBRACK { (With_HYPHEN_then_HYPHEN_LBRACK) }
	|	With_HYPHEN_then_HYPHEN_LBRACE { (With_HYPHEN_then_HYPHEN_LBRACE) }
	|	With { (With) }
	|	With_HYPHEN_in_HYPHEN_lex { (With_HYPHEN_in_HYPHEN_lex) }
	|	With_HYPHEN_then_HYPHEN_LPAREN { (With_HYPHEN_then_HYPHEN_LPAREN) }
	|	Wor { (Wor) }
	|	Wreal { (Wreal) }
	|	Xnor { (Xnor) }
	|	Xor { (Xor) }
	|	DLR_LBRACE_pli_HYPHEN_system_RBRACE { (DLR_LBRACE_pli_HYPHEN_system_RBRACE) }
	|	FLOATING_HYPHEN_POINT_NUMBER { (FLOATING_HYPHEN_POINT_NUMBER $1) }
	|	IDENTIFIER_HYPHEN_COLON_COLON { (IDENTIFIER_HYPHEN_COLON_COLON $1) }
	|	IDENTIFIER { (IDENTIFIER $1) }
	|	IDENTIFIER_HYPHEN_in_HYPHEN_lex { (IDENTIFIER_HYPHEN_in_HYPHEN_lex) }
	|	TYPE_HYPHEN_IDENTIFIER { (TYPE_HYPHEN_IDENTIFIER $1) }
	|	INTEGER_NUMBER { (INTEGER_NUMBER $1) }
	|	BACKQUOTE_systemc_implementation_BLOCK { (BACKQUOTE_systemc_implementation_BLOCK) }
	|	BACKQUOTE_systemc_imp_header_BLOCK { (BACKQUOTE_systemc_imp_header_BLOCK) }
	|	BACKQUOTE_systemc_header_BLOCK { (BACKQUOTE_systemc_header_BLOCK) }
	|	BACKQUOTE_systemc_dtor_BLOCK { (BACKQUOTE_systemc_dtor_BLOCK) }
	|	BACKQUOTE_systemc_interface_BLOCK { (BACKQUOTE_systemc_interface_BLOCK) }
	|	BACKQUOTE_systemc_ctor_BLOCK { (BACKQUOTE_systemc_ctor_BLOCK) }
	|	STRING { (STRING $1) }
	|	STRING_HYPHEN_ignored { (STRING_HYPHEN_ignored) }
	|	TABLE_LINE { (TABLE_LINE) }
	|	TIME_NUMBER { (TIME_NUMBER) }
	|	TIMING_SPEC_ELEMENT { (TIMING_SPEC_ELEMENT) }
	|	BACKQUOTE_nounconnecteddrive { (BACKQUOTE_nounconnecteddrive) }
	|	BACKQUOTE_resetall { (BACKQUOTE_resetall) }
	|	BACKQUOTE_unconnected_drive_pull0 { (BACKQUOTE_unconnected_drive_pull0) }
	|	BACKQUOTE_unconnected_drive_pull1 { (BACKQUOTE_unconnected_drive_pull1) }
	|	STRENGTH_keyword_LPAREN_strong1_SLASH_etc_RPAREN { (STRENGTH_keyword_LPAREN_strong1_SLASH_etc_RPAREN) }
	|	Specify specifyJunk Endspecify { TUPLE4(STRING("specifyJunk1968"),Specify,$2,Endspecify) }
	|	ERROR_TOKEN { (ERROR_TOKEN) }

specparam_declaration: Specparam junkToSemiList SEMICOLON { TUPLE4(STRING("specparam_declaration1970"),Specparam,$2,SEMICOLON) }

junkToSemiList: junkToSemi { CONS1 ($1) }
	|	junkToSemiList junkToSemi { CONS2($1,$2) }

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
	|	Alias { (Alias) }
	|	Always { (Always) }
	|	Always_comb { (Always_comb) }
	|	Always_ff { (Always_ff) }
	|	Always_latch { (Always_latch) }
	|	And { (And) }
	|	Assert { (Assert) }
	|	Assign { (Assign) }
	|	Assume { (Assume) }
	|	Automatic { (Automatic) }
	|	Before { (Before) }
	|	Begin { (Begin) }
	|	Bind { (Bind) }
	|	Bit { (Bit) }
	|	Break { (Break) }
	|	Buf { (Buf) }
	|	Bufif0 { (Bufif0) }
	|	Bufif1 { (Bufif1) }
	|	Byte { (Byte) }
	|	Case { (Case) }
	|	Casex { (Casex) }
	|	Casez { (Casez) }
	|	Chandle { (Chandle) }
	|	Class { (Class) }
	|	Clocking { (Clocking) }
	|	Cmos { (Cmos) }
	|	Constraint { (Constraint) }
	|	Const { (Const) }
	|	Const_HYPHEN_in_HYPHEN_lex { (Const_HYPHEN_in_HYPHEN_lex) }
	|	Const_HYPHEN_then_HYPHEN_ref { (Const_HYPHEN_then_HYPHEN_ref) }
	|	Context { (Context) }
	|	Continue { (Continue) }
	|	Cover { (Cover) }
	|	Deassign { (Deassign) }
	|	Default { (Default) }
	|	Defparam { (Defparam) }
	|	Disable { (Disable) }
	|	Dist { (Dist) }
	|	Do { (Do) }
	|	DLR_acos { (DLR_acos) }
	|	DLR_acosh { (DLR_acosh) }
	|	DLR_asin { (DLR_asin) }
	|	DLR_asinh { (DLR_asinh) }
	|	DLR_atan { (DLR_atan) }
	|	DLR_atan2 { (DLR_atan2) }
	|	DLR_atanh { (DLR_atanh) }
	|	DLR_bits { (DLR_bits) }
	|	DLR_bitstoreal { (DLR_bitstoreal) }
	|	DLR_bitstoshortreal { (DLR_bitstoshortreal) }
	|	DLR_c { (DLR_c) }
	|	DLR_cast { (DLR_cast) }
	|	DLR_ceil { (DLR_ceil) }
	|	DLR_changed { (DLR_changed) }
	|	DLR_clog2 { (DLR_clog2) }
	|	DLR_cos { (DLR_cos) }
	|	DLR_cosh { (DLR_cosh) }
	|	DLR_countbits { (DLR_countbits) }
	|	DLR_countones { (DLR_countones) }
	|	DLR_dimensions { (DLR_dimensions) }
	|	DLR_display { (DLR_display) }
	|	DLR_displayb { (DLR_displayb) }
	|	DLR_displayh { (DLR_displayh) }
	|	DLR_displayo { (DLR_displayo) }
	|	DLR_dumpall { (DLR_dumpall) }
	|	DLR_dumpfile { (DLR_dumpfile) }
	|	DLR_dumpflush { (DLR_dumpflush) }
	|	DLR_dumplimit { (DLR_dumplimit) }
	|	DLR_dumpoff { (DLR_dumpoff) }
	|	DLR_dumpon { (DLR_dumpon) }
	|	DLR_dumpports { (DLR_dumpports) }
	|	DLR_dumpvars { (DLR_dumpvars) }
	|	DLR_error { (DLR_error) }
	|	DLR_exit { (DLR_exit) }
	|	DLR_exp { (DLR_exp) }
	|	DLR_fatal { (DLR_fatal) }
	|	DLR_fclose { (DLR_fclose) }
	|	DLR_fdisplay { (DLR_fdisplay) }
	|	DLR_fdisplayb { (DLR_fdisplayb) }
	|	DLR_fdisplayh { (DLR_fdisplayh) }
	|	DLR_fdisplayo { (DLR_fdisplayo) }
	|	DLR_fell { (DLR_fell) }
	|	DLR_feof { (DLR_feof) }
	|	DLR_ferror { (DLR_ferror) }
	|	DLR_fflush { (DLR_fflush) }
	|	DLR_fgetc { (DLR_fgetc) }
	|	DLR_fgets { (DLR_fgets) }
	|	DLR_finish { (DLR_finish) }
	|	DLR_floor { (DLR_floor) }
	|	DLR_fmonitor { (DLR_fmonitor) }
	|	DLR_fmonitorb { (DLR_fmonitorb) }
	|	DLR_fmonitorh { (DLR_fmonitorh) }
	|	DLR_fmonitoro { (DLR_fmonitoro) }
	|	DLR_fopen { (DLR_fopen) }
	|	DLR_fread { (DLR_fread) }
	|	DLR_frewind { (DLR_frewind) }
	|	DLR_fscanf { (DLR_fscanf) }
	|	DLR_fseek { (DLR_fseek) }
	|	DLR_fstrobe { (DLR_fstrobe) }
	|	DLR_fstrobeb { (DLR_fstrobeb) }
	|	DLR_fstrobeh { (DLR_fstrobeh) }
	|	DLR_fstrobeo { (DLR_fstrobeo) }
	|	DLR_ftell { (DLR_ftell) }
	|	DLR_fwrite { (DLR_fwrite) }
	|	DLR_fwriteb { (DLR_fwriteb) }
	|	DLR_fwriteh { (DLR_fwriteh) }
	|	DLR_fwriteo { (DLR_fwriteo) }
	|	DLR_high { (DLR_high) }
	|	DLR_hypot { (DLR_hypot) }
	|	DLR_increment { (DLR_increment) }
	|	DLR_info { (DLR_info) }
	|	DLR_isunbounded { (DLR_isunbounded) }
	|	DLR_isunknown { (DLR_isunknown) }
	|	DLR_itor { (DLR_itor) }
	|	DLR_left { (DLR_left) }
	|	DLR_ln { (DLR_ln) }
	|	DLR_log10 { (DLR_log10) }
	|	DLR_low { (DLR_low) }
	|	DLR_monitor { (DLR_monitor) }
	|	DLR_monitorb { (DLR_monitorb) }
	|	DLR_monitorh { (DLR_monitorh) }
	|	DLR_monitoro { (DLR_monitoro) }
	|	DLR_monitoroff { (DLR_monitoroff) }
	|	DLR_monitoron { (DLR_monitoron) }
	|	DLR_onehot { (DLR_onehot) }
	|	DLR_onehot0 { (DLR_onehot0) }
	|	DLR_past { (DLR_past) }
	|	DLR_pow { (DLR_pow) }
	|	DLR_printtimescale { (DLR_printtimescale) }
	|	DLR_random { (DLR_random) }
	|	DLR_readmemb { (DLR_readmemb) }
	|	DLR_readmemh { (DLR_readmemh) }
	|	DLR_realtime { (DLR_realtime) }
	|	DLR_realtobits { (DLR_realtobits) }
	|	DLR_rewind { (DLR_rewind) }
	|	DLR_right { (DLR_right) }
	|	DLR_root { (DLR_root) }
	|	DLR_rose { (DLR_rose) }
	|	DLR_rtoi { (DLR_rtoi) }
	|	DLR_sampled { (DLR_sampled) }
	|	DLR_sformat { (DLR_sformat) }
	|	DLR_sformatf { (DLR_sformatf) }
	|	DLR_shortrealtobits { (DLR_shortrealtobits) }
	|	DLR_signed { (DLR_signed) }
	|	DLR_sin { (DLR_sin) }
	|	DLR_sinh { (DLR_sinh) }
	|	DLR_size { (DLR_size) }
	|	DLR_sqrt { (DLR_sqrt) }
	|	DLR_sscanf { (DLR_sscanf) }
	|	DLR_stable { (DLR_stable) }
	|	DLR_stime { (DLR_stime) }
	|	DLR_stop { (DLR_stop) }
	|	DLR_strobe { (DLR_strobe) }
	|	DLR_strobeb { (DLR_strobeb) }
	|	DLR_strobeh { (DLR_strobeh) }
	|	DLR_strobeo { (DLR_strobeo) }
	|	DLR_swrite { (DLR_swrite) }
	|	DLR_swriteb { (DLR_swriteb) }
	|	DLR_swriteh { (DLR_swriteh) }
	|	DLR_swriteo { (DLR_swriteo) }
	|	DLR_system { (DLR_system) }
	|	DLR_tan { (DLR_tan) }
	|	DLR_tanh { (DLR_tanh) }
	|	DLR_test_DLR_plusargs { (DLR_test_DLR_plusargs) }
	|	DLR_time { (DLR_time) }
	|	DLR_timeformat { (DLR_timeformat) }
	|	DLR_typename { (DLR_typename) }
	|	DLR_ungetc { (DLR_ungetc) }
	|	DLR_unit { (DLR_unit) }
	|	DLR_unpacked_dimensions { (DLR_unpacked_dimensions) }
	|	DLR_unsigned { (DLR_unsigned) }
	|	DLR_urandom { (DLR_urandom) }
	|	DLR_urandom_range { (DLR_urandom_range) }
	|	DLR_value_DLR_plusargs { (DLR_value_DLR_plusargs) }
	|	DLR_warning { (DLR_warning) }
	|	DLR_write { (DLR_write) }
	|	DLR_writeb { (DLR_writeb) }
	|	DLR_writeh { (DLR_writeh) }
	|	DLR_writememb { (DLR_writememb) }
	|	DLR_writememh { (DLR_writememh) }
	|	DLR_writeo { (DLR_writeo) }
	|	Edge { (Edge) }
	|	Else { (Else) }
	|	End { (End) }
	|	Endcase { (Endcase) }
	|	Endclass { (Endclass) }
	|	Endclocking { (Endclocking) }
	|	Endfunction { (Endfunction) }
	|	Endgenerate { (Endgenerate) }
	|	Endinterface { (Endinterface) }
	|	Endpackage { (Endpackage) }
	|	Endprimitive { (Endprimitive) }
	|	Endprogram { (Endprogram) }
	|	Endproperty { (Endproperty) }
	|	Endtable { (Endtable) }
	|	Endtask { (Endtask) }
	|	Enum { (Enum) }
	|	Event { (Event) }
	|	Export { (Export) }
	|	Extends { (Extends) }
	|	Extern { (Extern) }
	|	Final { (Final) }
	|	For { (For) }
	|	Force { (Force) }
	|	Foreach { (Foreach) }
	|	Forever { (Forever) }
	|	Fork { (Fork) }
	|	Forkjoin { (Forkjoin) }
	|	Function { (Function) }
	|	Generate { (Generate) }
	|	Genvar { (Genvar) }
	|	Global_HYPHEN_then_HYPHEN_clocking { (Global_HYPHEN_then_HYPHEN_clocking) }
	|	Global { (Global) }
	|	Global_HYPHEN_in_HYPHEN_lex { (Global_HYPHEN_in_HYPHEN_lex) }
	|	If { (If) }
	|	Iff { (Iff) }
	|	Implements { (Implements) }
	|	Import { (Import) }
	|	Initial { (Initial) }
	|	Inout { (Inout) }
	|	Input { (Input) }
	|	Inside { (Inside) }
	|	Int { (Int) }
	|	Integer { (Integer) }
	|	Interface { (Interface) }
	|	Join { (Join) }
	|	Join_any { (Join_any) }
	|	Join_none { (Join_none) }
	|	Localparam { (Localparam) }
	|	Local_HYPHEN_then_HYPHEN_COLON_COLON { (Local_HYPHEN_then_HYPHEN_COLON_COLON) }
	|	Local { (Local) }
	|	Local_HYPHEN_in_HYPHEN_lex { (Local_HYPHEN_in_HYPHEN_lex) }
	|	Logic { (Logic) }
	|	Longint { (Longint) }
	|	Modport { (Modport) }
	|	Module { (Module) }
	|	Nand { (Nand) }
	|	Negedge { (Negedge) }
	|	New { (New) }
	|	New_HYPHEN_in_HYPHEN_lex { (New_HYPHEN_in_HYPHEN_lex) }
	|	New_HYPHEN_then_HYPHEN_paren { (New_HYPHEN_then_HYPHEN_paren) }
	|	Nmos { (Nmos) }
	|	Nor { (Nor) }
	|	Not { (Not) }
	|	Notif0 { (Notif0) }
	|	Notif1 { (Notif1) }
	|	Null { (Null) }
	|	Or { (Or) }
	|	Output { (Output) }
	|	Package { (Package) }
	|	Packed { (Packed) }
	|	Parameter { (Parameter) }
	|	Pmos { (Pmos) }
	|	Posedge { (Posedge) }
	|	Primitive { (Primitive) }
	|	Priority { (Priority) }
	|	Program { (Program) }
	|	Property { (Property) }
	|	Protected { (Protected) }
	|	Pulldown { (Pulldown) }
	|	Pullup { (Pullup) }
	|	Pure { (Pure) }
	|	AMPERSAND_AMPERSAND { (AMPERSAND_AMPERSAND) }
	|	AMPERSAND_AMPERSAND_AMPERSAND { (AMPERSAND_AMPERSAND_AMPERSAND) }
	|	AMPERSAND_EQ { (AMPERSAND_EQ) }
	|	STAR_GT { (STAR_GT) }
	|	AT_AT { (AT_AT) }
	|	LBRACK_EQ { (LBRACK_EQ) }
	|	LBRACK_HYPHEN_GT { (LBRACK_HYPHEN_GT) }
	|	LBRACK_STAR { (LBRACK_STAR) }
	|	EQ_EQ_EQ { (EQ_EQ_EQ) }
	|	PLING_EQ_EQ { (PLING_EQ_EQ) }
	|	COLON_COLON { (COLON_COLON) }
	|	COLON_SLASH { (COLON_SLASH) }
	|	COLON_EQ { (COLON_EQ) }
	|	COLON_HYPHEN_begin { (COLON_HYPHEN_begin) }
	|	COLON_HYPHEN_fork { (COLON_HYPHEN_fork) }
	|	SLASH_EQ { (SLASH_EQ) }
	|	DOT_STAR { (DOT_STAR) }
	|	EQ_GT { (EQ_GT) }
	|	EQ_EQ { (EQ_EQ) }
	|	GT_EQ { (GT_EQ) }
	|	LT_EQ { (LT_EQ) }
	|	LT_EQ_HYPHEN_ignored { (LT_EQ_HYPHEN_ignored) }
	|	LT_HYPHEN_GT { (LT_HYPHEN_GT) }
	|	HYPHEN_COLON { (HYPHEN_COLON) }
	|	HYPHEN_EQ { (HYPHEN_EQ) }
	|	HYPHEN_GT { (HYPHEN_GT) }
	|	HYPHEN_GT_GT { (HYPHEN_GT_GT) }
	|	HYPHEN_HYPHEN { (HYPHEN_HYPHEN) }
	|	PERCENT_EQ { (PERCENT_EQ) }
	|	TILDE_AMPERSAND { (TILDE_AMPERSAND) }
	|	TILDE_VBAR { (TILDE_VBAR) }
	|	PLING_EQ { (PLING_EQ) }
	|	VBAR_EQ { (VBAR_EQ) }
	|	VBAR_EQ_GT { (VBAR_EQ_GT) }
	|	VBAR_HYPHEN_GT { (VBAR_HYPHEN_GT) }
	|	VBAR_VBAR { (VBAR_VBAR) }
	|	LPAREN_HYPHEN_for_HYPHEN_strength { (LPAREN_HYPHEN_for_HYPHEN_strength) }
	|	PLUS_COLON { (PLUS_COLON) }
	|	PLUS_EQ { (PLUS_EQ) }
	|	PLUS_PLUS { (PLUS_PLUS) }
	|	HASH_HASH { (HASH_HASH) }
	|	STAR_STAR { (STAR_STAR) }
	|	LT_LT { (LT_LT) }
	|	LT_LT_EQ { (LT_LT_EQ) }
	|	GT_GT { (GT_GT) }
	|	GT_GT_EQ { (GT_GT_EQ) }
	|	GT_GT_GT { (GT_GT_GT) }
	|	GT_GT_GT_EQ { (GT_GT_GT_EQ) }
	|	QUOTE { (QUOTE) }
	|	QUOTE_LBRACE { (QUOTE_LBRACE) }
	|	STAR_EQ { (STAR_EQ) }
	|	EQ_EQ_QUERY { (EQ_EQ_QUERY) }
	|	PLING_EQ_QUERY { (PLING_EQ_QUERY) }
	|	CARET_TILDE { (CARET_TILDE) }
	|	CARET_EQ { (CARET_EQ) }
	|	Rand { (Rand) }
	|	Randc { (Randc) }
	|	Randcase { (Randcase) }
	|	Randomize { (Randomize) }
	|	Rcmos { (Rcmos) }
	|	Real { (Real) }
	|	Realtime { (Realtime) }
	|	Ref { (Ref) }
	|	Reg { (Reg) }
	|	Release { (Release) }
	|	Repeat { (Repeat) }
	|	Restrict { (Restrict) }
	|	Return { (Return) }
	|	Rnmos { (Rnmos) }
	|	Rpmos { (Rpmos) }
	|	Rtran { (Rtran) }
	|	Rtranif0 { (Rtranif0) }
	|	Rtranif1 { (Rtranif1) }
	|	Scalared { (Scalared) }
	|	Shortint { (Shortint) }
	|	Shortreal { (Shortreal) }
	|	Signed { (Signed) }
	|	Soft { (Soft) }
	|	Solve { (Solve) }
	|	Specify { (Specify) }
	|	Specparam { (Specparam) }
	|	Static_HYPHEN_then_HYPHEN_constraint { (Static_HYPHEN_then_HYPHEN_constraint) }
	|	Static { (Static) }
	|	Static_HYPHEN_in_HYPHEN_lex { (Static_HYPHEN_in_HYPHEN_lex) }
	|	String { (String) }
	|	Struct { (Struct) }
	|	Super { (Super) }
	|	Supply0 { (Supply0) }
	|	Supply1 { (Supply1) }
	|	Table { (Table) }
	|	Task { (Task) }
	|	This { (This) }
	|	Time { (Time) }
	|	Timeprecision { (Timeprecision) }
	|	Timeunit { (Timeunit) }
	|	Tran { (Tran) }
	|	Tranif0 { (Tranif0) }
	|	Tranif1 { (Tranif1) }
	|	Tri { (Tri) }
	|	Tri0 { (Tri0) }
	|	Tri1 { (Tri1) }
	|	Triand { (Triand) }
	|	Trior { (Trior) }
	|	Trireg { (Trireg) }
	|	True { (True) }
	|	Type { (Type) }
	|	Typedef { (Typedef) }
	|	Union { (Union) }
	|	Unique { (Unique) }
	|	Unique0 { (Unique0) }
	|	Unsigned { (Unsigned) }
	|	Var { (Var) }
	|	Vectored { (Vectored) }
	|	Virtual_HYPHEN_then_HYPHEN_class { (Virtual_HYPHEN_then_HYPHEN_class) }
	|	Virtual { (Virtual) }
	|	Virtual_HYPHEN_then_HYPHEN_interface { (Virtual_HYPHEN_then_HYPHEN_interface) }
	|	Virtual_HYPHEN_in_HYPHEN_lex { (Virtual_HYPHEN_in_HYPHEN_lex) }
	|	Virtual_HYPHEN_then_HYPHEN_identifier { (Virtual_HYPHEN_then_HYPHEN_identifier) }
	|	Clocker { (Clocker) }
	|	Clock_enable { (Clock_enable) }
	|	Coverage_block_off { (Coverage_block_off) }
	|	Coverage_off { (Coverage_off) }
	|	Coverage_on { (Coverage_on) }
	|	HYPHEN_HYPHEN_block { (HYPHEN_HYPHEN_block) }
	|	HYPHEN_HYPHEN_file { (HYPHEN_HYPHEN_file) }
	|	HYPHEN_HYPHEN_function { (HYPHEN_HYPHEN_function) }
	|	HYPHEN_HYPHEN_lines { (HYPHEN_HYPHEN_lines) }
	|	HYPHEN_HYPHEN_match { (HYPHEN_HYPHEN_match) }
	|	HYPHEN_HYPHEN_module { (HYPHEN_HYPHEN_module) }
	|	HYPHEN_HYPHEN_msg { (HYPHEN_HYPHEN_msg) }
	|	HYPHEN_HYPHEN_rule { (HYPHEN_HYPHEN_rule) }
	|	HYPHEN_HYPHEN_task { (HYPHEN_HYPHEN_task) }
	|	HYPHEN_HYPHEN_var { (HYPHEN_HYPHEN_var) }
	|	Full_case { (Full_case) }
	|	Hier_block { (Hier_block) }
	|	Inline { (Inline) }
	|	Isolate_assignments { (Isolate_assignments) }
	|	Lint_off { (Lint_off) }
	|	Lint_on { (Lint_on) }
	|	No_clocker { (No_clocker) }
	|	No_inline { (No_inline) }
	|	Parallel_case { (Parallel_case) }
	|	Public { (Public) }
	|	Public_flat { (Public_flat) }
	|	Public_flat_rd { (Public_flat_rd) }
	|	Public_flat_rw { (Public_flat_rw) }
	|	Public_module { (Public_module) }
	|	Sc_bv { (Sc_bv) }
	|	Sformat { (Sformat) }
	|	Split_var { (Split_var) }
	|	Tracing_off { (Tracing_off) }
	|	Tracing_on { (Tracing_on) }
	|	SLASH_STAR_verilator_clocker_STAR_SLASH { (SLASH_STAR_verilator_clocker_STAR_SLASH) }
	|	SLASH_STAR_verilator_clock_enable_STAR_SLASH { (SLASH_STAR_verilator_clock_enable_STAR_SLASH) }
	|	SLASH_STAR_verilator_coverage_block_off_STAR_SLASH { (SLASH_STAR_verilator_coverage_block_off_STAR_SLASH) }
	|	SLASH_STAR_verilator_full_case_STAR_SLASH { (SLASH_STAR_verilator_full_case_STAR_SLASH) }
	|	SLASH_STAR_verilator_hier_block_STAR_SLASH { (SLASH_STAR_verilator_hier_block_STAR_SLASH) }
	|	SLASH_STAR_verilator_inline_module_STAR_SLASH { (SLASH_STAR_verilator_inline_module_STAR_SLASH) }
	|	SLASH_STAR_verilator_isolate_assignments_STAR_SLASH { (SLASH_STAR_verilator_isolate_assignments_STAR_SLASH) }
	|	SLASH_STAR_verilator_no_clocker_STAR_SLASH { (SLASH_STAR_verilator_no_clocker_STAR_SLASH) }
	|	SLASH_STAR_verilator_no_inline_module_STAR_SLASH { (SLASH_STAR_verilator_no_inline_module_STAR_SLASH) }
	|	SLASH_STAR_verilator_no_inline_task_STAR_SLASH { (SLASH_STAR_verilator_no_inline_task_STAR_SLASH) }
	|	SLASH_STAR_verilator_parallel_case_STAR_SLASH { (SLASH_STAR_verilator_parallel_case_STAR_SLASH) }
	|	SLASH_STAR_verilator_public_STAR_SLASH { (SLASH_STAR_verilator_public_STAR_SLASH) }
	|	SLASH_STAR_verilator_public_flat_STAR_SLASH { (SLASH_STAR_verilator_public_flat_STAR_SLASH) }
	|	SLASH_STAR_verilator_public_flat_rd_STAR_SLASH { (SLASH_STAR_verilator_public_flat_rd_STAR_SLASH) }
	|	SLASH_STAR_verilator_public_flat_rw_STAR_SLASH { (SLASH_STAR_verilator_public_flat_rw_STAR_SLASH) }
	|	SLASH_STAR_verilator_public_module_STAR_SLASH { (SLASH_STAR_verilator_public_module_STAR_SLASH) }
	|	SLASH_STAR_verilator_sc_bv_STAR_SLASH { (SLASH_STAR_verilator_sc_bv_STAR_SLASH) }
	|	SLASH_STAR_verilator_sformat_STAR_SLASH { (SLASH_STAR_verilator_sformat_STAR_SLASH) }
	|	SLASH_STAR_verilator_split_var_STAR_SLASH { (SLASH_STAR_verilator_split_var_STAR_SLASH) }
	|	SLASH_STAR_verilator_tag_STAR_SLASH { (SLASH_STAR_verilator_tag_STAR_SLASH) }
	|	Void { (Void) }
	|	Wait { (Wait) }
	|	Wand { (Wand) }
	|	While { (While) }
	|	Wire { (Wire) }
	|	With_HYPHEN_then_HYPHEN_LBRACK { (With_HYPHEN_then_HYPHEN_LBRACK) }
	|	With_HYPHEN_then_HYPHEN_LBRACE { (With_HYPHEN_then_HYPHEN_LBRACE) }
	|	With { (With) }
	|	With_HYPHEN_in_HYPHEN_lex { (With_HYPHEN_in_HYPHEN_lex) }
	|	With_HYPHEN_then_HYPHEN_LPAREN { (With_HYPHEN_then_HYPHEN_LPAREN) }
	|	Wor { (Wor) }
	|	Wreal { (Wreal) }
	|	Xnor { (Xnor) }
	|	Xor { (Xor) }
	|	DLR_LBRACE_pli_HYPHEN_system_RBRACE { (DLR_LBRACE_pli_HYPHEN_system_RBRACE) }
	|	FLOATING_HYPHEN_POINT_NUMBER { (FLOATING_HYPHEN_POINT_NUMBER $1) }
	|	IDENTIFIER_HYPHEN_COLON_COLON { (IDENTIFIER_HYPHEN_COLON_COLON $1) }
	|	IDENTIFIER { (IDENTIFIER $1) }
	|	IDENTIFIER_HYPHEN_in_HYPHEN_lex { (IDENTIFIER_HYPHEN_in_HYPHEN_lex) }
	|	TYPE_HYPHEN_IDENTIFIER { (TYPE_HYPHEN_IDENTIFIER $1) }
	|	INTEGER_NUMBER { (INTEGER_NUMBER $1) }
	|	BACKQUOTE_systemc_implementation_BLOCK { (BACKQUOTE_systemc_implementation_BLOCK) }
	|	BACKQUOTE_systemc_imp_header_BLOCK { (BACKQUOTE_systemc_imp_header_BLOCK) }
	|	BACKQUOTE_systemc_header_BLOCK { (BACKQUOTE_systemc_header_BLOCK) }
	|	BACKQUOTE_systemc_dtor_BLOCK { (BACKQUOTE_systemc_dtor_BLOCK) }
	|	BACKQUOTE_systemc_interface_BLOCK { (BACKQUOTE_systemc_interface_BLOCK) }
	|	BACKQUOTE_systemc_ctor_BLOCK { (BACKQUOTE_systemc_ctor_BLOCK) }
	|	STRING { (STRING $1) }
	|	STRING_HYPHEN_ignored { (STRING_HYPHEN_ignored) }
	|	TABLE_LINE { (TABLE_LINE) }
	|	TIME_NUMBER { (TIME_NUMBER) }
	|	TIMING_SPEC_ELEMENT { (TIMING_SPEC_ELEMENT) }
	|	BACKQUOTE_nounconnecteddrive { (BACKQUOTE_nounconnecteddrive) }
	|	BACKQUOTE_resetall { (BACKQUOTE_resetall) }
	|	BACKQUOTE_unconnected_drive_pull0 { (BACKQUOTE_unconnected_drive_pull0) }
	|	BACKQUOTE_unconnected_drive_pull1 { (BACKQUOTE_unconnected_drive_pull1) }
	|	STRENGTH_keyword_LPAREN_strong1_SLASH_etc_RPAREN { (STRENGTH_keyword_LPAREN_strong1_SLASH_etc_RPAREN) }
	|	ERROR_TOKEN { (ERROR_TOKEN) }

id: IDENTIFIER { (IDENTIFIER $1) }
	|	idRandomize { ($1) }

idAny: IDENTIFIER { (IDENTIFIER $1) }
	|	TYPE_HYPHEN_IDENTIFIER { (TYPE_HYPHEN_IDENTIFIER $1) }
	|	IDENTIFIER_HYPHEN_COLON_COLON { (IDENTIFIER_HYPHEN_COLON_COLON $1) }
	|	idRandomize { ($1) }

idType: TYPE_HYPHEN_IDENTIFIER { (TYPE_HYPHEN_IDENTIFIER $1) }

idCC: IDENTIFIER_HYPHEN_COLON_COLON { (IDENTIFIER_HYPHEN_COLON_COLON $1) }

idRandomize: Randomize { (Randomize) }

idSVKwd: Do { (Do) }
	|	Final { (Final) }

variable_lvalue: idClassSel { ($1) }
	|	LBRACE variable_lvalueConcList RBRACE { TUPLE4(STRING("variable_lvalue2480"),LBRACE,$2,RBRACE) }
	|	streaming_concatenation { ($1) }

variable_lvalueConcList: variable_lvalue { CONS1 ($1) }
	|	variable_lvalueConcList COMMA variable_lvalue { CONS3($1,COMMA,$3) }

idClassSel: idDotted { ($1) }
	|	This DOT idDotted { TUPLE4(STRING("idClassSel2485"),This,DOT,$3) }
	|	Super DOT idDotted { TUPLE4(STRING("idClassSel2486"),Super,DOT,$3) }
	|	This DOT Super DOT idDotted { TUPLE6(STRING("idClassSel2487"),This,DOT,Super,DOT,$5) }
	|	packageClassScope idDotted { TUPLE3(STRING("idClassSel2488"),$1,$2) }

idClassSelForeach: idDottedForeach { ($1) }
	|	This DOT idDottedForeach { TUPLE4(STRING("idClassSelForeach2490"),This,DOT,$3) }
	|	Super DOT idDottedForeach { TUPLE4(STRING("idClassSelForeach2491"),Super,DOT,$3) }
	|	This DOT Super DOT idDottedForeach { TUPLE6(STRING("idClassSelForeach2492"),This,DOT,Super,DOT,$5) }
	|	packageClassScope idDottedForeach { TUPLE3(STRING("idClassSelForeach2493"),$1,$2) }

idDotted: DLR_root DOT idDottedMore { TUPLE4(STRING("idDotted2494"),DLR_root,DOT,$3) }
	|	idDottedMore { ($1) }

idDottedForeach: DLR_root DOT idDottedMoreForeach { TUPLE4(STRING("idDottedForeach2496"),DLR_root,DOT,$3) }
	|	idDottedMoreForeach { ($1) }

idDottedMore: idArrayed { ($1) }
	|	idDottedMore DOT idArrayed { TUPLE4(STRING("idDottedMore2499"),$1,DOT,$3) }

idDottedMoreForeach: idArrayedForeach { ($1) }
	|	idDottedMoreForeach DOT idArrayedForeach { TUPLE4(STRING("idDottedMoreForeach2501"),$1,DOT,$3) }

idArrayed: id { ($1) }
	|	idArrayed LBRACK expr RBRACK { TUPLE5(STRING("idArrayed2503"),$1,LBRACK,$3,RBRACK) }
	|	idArrayed LBRACK constExpr COLON constExpr RBRACK { TUPLE7(STRING("idArrayed2504"),$1,LBRACK,$3,COLON,$5,RBRACK) }
	|	idArrayed LBRACK expr PLUS_COLON constExpr RBRACK { TUPLE7(STRING("idArrayed2505"),$1,LBRACK,$3,PLUS_COLON,$5,RBRACK) }
	|	idArrayed LBRACK expr HYPHEN_COLON constExpr RBRACK { TUPLE7(STRING("idArrayed2506"),$1,LBRACK,$3,HYPHEN_COLON,$5,RBRACK) }

idArrayedForeach: id { ($1) }
	|	idArrayed LBRACK expr RBRACK { TUPLE5(STRING("idArrayedForeach2508"),$1,LBRACK,$3,RBRACK) }
	|	idArrayed LBRACK constExpr COLON constExpr RBRACK { TUPLE7(STRING("idArrayedForeach2509"),$1,LBRACK,$3,COLON,$5,RBRACK) }
	|	idArrayed LBRACK expr PLUS_COLON constExpr RBRACK { TUPLE7(STRING("idArrayedForeach2510"),$1,LBRACK,$3,PLUS_COLON,$5,RBRACK) }
	|	idArrayed LBRACK expr HYPHEN_COLON constExpr RBRACK { TUPLE7(STRING("idArrayedForeach2511"),$1,LBRACK,$3,HYPHEN_COLON,$5,RBRACK) }
	|	idArrayed LBRACK expr COMMA loop_variables RBRACK { TUPLE7(STRING("idArrayedForeach2512"),$1,LBRACK,$3,COMMA,$5,RBRACK) }

varRefBase: id { ($1) }

str: STRING { (STRING $1) }

strAsInt: STRING { (STRING $1) }

strAsIntIgnore: STRING_HYPHEN_ignored { (STRING_HYPHEN_ignored) }

strAsText: STRING { (STRING $1) }

endLabelE: /* empty */ { EMPTY_TOKEN }
	|	COLON idAny { TUPLE3(STRING("endLabelE2519"),COLON,$2) }
	|	COLON New { TUPLE3(STRING("endLabelE2520"),COLON,New) }

clocking_declaration: Default Clocking AT LPAREN senitemEdge RPAREN SEMICOLON Endclocking { TUPLE9(STRING("clocking_declaration2521"),Default,Clocking,AT,LPAREN,$5,RPAREN,SEMICOLON,Endclocking) }

assertion_item: concurrent_assertion_item { ($1) }
	|	deferred_immediate_assertion_item { ($1) }

deferred_immediate_assertion_item: deferred_immediate_assertion_statement { ($1) }
	|	id COLON deferred_immediate_assertion_statement { TUPLE4(STRING("deferred_immediate_assertion_item2525"),$1,COLON,$3) }

procedural_assertion_statement: concurrent_assertion_statement { ($1) }
	|	immediate_assertion_statement { ($1) }

immediate_assertion_statement: simple_immediate_assertion_statement { ($1) }
	|	deferred_immediate_assertion_statement { ($1) }

simple_immediate_assertion_statement: Assert LPAREN expr RPAREN stmtBlock { TUPLE6(STRING("simple_immediate_assertion_statement2530"),Assert,LPAREN,$3,RPAREN,$5) }
	|	Assert LPAREN expr RPAREN Else stmtBlock { TUPLE7(STRING("simple_immediate_assertion_statement2531"),Assert,LPAREN,$3,RPAREN,Else,$6) }
	|	Assert LPAREN expr RPAREN stmtBlock Else stmtBlock { TUPLE8(STRING("simple_immediate_assertion_statement2532"),Assert,LPAREN,$3,RPAREN,$5,Else,$7) }
	|	Assume LPAREN expr RPAREN stmtBlock { TUPLE6(STRING("simple_immediate_assertion_statement2533"),Assume,LPAREN,$3,RPAREN,$5) }
	|	Assume LPAREN expr RPAREN Else stmtBlock { TUPLE7(STRING("simple_immediate_assertion_statement2534"),Assume,LPAREN,$3,RPAREN,Else,$6) }
	|	Assume LPAREN expr RPAREN stmtBlock Else stmtBlock { TUPLE8(STRING("simple_immediate_assertion_statement2535"),Assume,LPAREN,$3,RPAREN,$5,Else,$7) }
	|	Cover LPAREN expr RPAREN stmt { TUPLE6(STRING("simple_immediate_assertion_statement2536"),Cover,LPAREN,$3,RPAREN,$5) }

final_zero: HASH INTEGER_NUMBER { TUPLE3(STRING("final_zero2537"),HASH,INTEGER_NUMBER $2) }
	|	Final { (Final) }

deferred_immediate_assertion_statement: Assert final_zero LPAREN expr RPAREN stmtBlock { TUPLE7(STRING("deferred_immediate_assertion_statement2539"),Assert,$2,LPAREN,$4,RPAREN,$6) }
	|	Assert final_zero LPAREN expr RPAREN Else stmtBlock { TUPLE8(STRING("deferred_immediate_assertion_statement2540"),Assert,$2,LPAREN,$4,RPAREN,Else,$7) }
	|	Assert final_zero LPAREN expr RPAREN stmtBlock Else stmtBlock { TUPLE9(STRING("deferred_immediate_assertion_statement2541"),Assert,$2,LPAREN,$4,RPAREN,$6,Else,$8) }
	|	Assume final_zero LPAREN expr RPAREN stmtBlock { TUPLE7(STRING("deferred_immediate_assertion_statement2542"),Assume,$2,LPAREN,$4,RPAREN,$6) }
	|	Assume final_zero LPAREN expr RPAREN Else stmtBlock { TUPLE8(STRING("deferred_immediate_assertion_statement2543"),Assume,$2,LPAREN,$4,RPAREN,Else,$7) }
	|	Assume final_zero LPAREN expr RPAREN stmtBlock Else stmtBlock { TUPLE9(STRING("deferred_immediate_assertion_statement2544"),Assume,$2,LPAREN,$4,RPAREN,$6,Else,$8) }
	|	Cover final_zero LPAREN expr RPAREN stmt { TUPLE7(STRING("deferred_immediate_assertion_statement2545"),Cover,$2,LPAREN,$4,RPAREN,$6) }

concurrent_assertion_item: concurrent_assertion_statement { ($1) }
	|	id COLON concurrent_assertion_statement { TUPLE4(STRING("concurrent_assertion_item2547"),$1,COLON,$3) }

concurrent_assertion_statement: Assert Property LPAREN property_spec RPAREN elseStmtBlock { TUPLE7(STRING("concurrent_assertion_statement2548"),Assert,Property,LPAREN,$4,RPAREN,$6) }
	|	Assume Property LPAREN property_spec RPAREN elseStmtBlock { TUPLE7(STRING("concurrent_assertion_statement2549"),Assume,Property,LPAREN,$4,RPAREN,$6) }
	|	Cover Property LPAREN property_spec RPAREN stmtBlock { TUPLE7(STRING("concurrent_assertion_statement2550"),Cover,Property,LPAREN,$4,RPAREN,$6) }
	|	Restrict Property LPAREN property_spec RPAREN SEMICOLON { TUPLE7(STRING("concurrent_assertion_statement2551"),Restrict,Property,LPAREN,$4,RPAREN,SEMICOLON) }

elseStmtBlock: SEMICOLON { (SEMICOLON) }
	|	Else stmtBlock { TUPLE3(STRING("elseStmtBlock2553"),Else,$2) }

property_spec: AT LPAREN senitemEdge RPAREN Disable Iff LPAREN expr RPAREN pexpr { TUPLE11(STRING("property_spec2554"),AT,LPAREN,$3,RPAREN,Disable,Iff,LPAREN,$8,RPAREN,$10) }
	|	AT LPAREN senitemEdge RPAREN pexpr { TUPLE6(STRING("property_spec2555"),AT,LPAREN,$3,RPAREN,$5) }
	|	Disable Iff LPAREN expr RPAREN pexpr { TUPLE7(STRING("property_spec2556"),Disable,Iff,LPAREN,$4,RPAREN,$6) }
	|	pexpr { ($1) }

pexpr: expr VBAR_HYPHEN_GT pexpr { TUPLE4(STRING("pexpr2558"),$1,VBAR_HYPHEN_GT,$3) }
	|	expr VBAR_EQ_GT pexpr { TUPLE4(STRING("pexpr2559"),$1,VBAR_EQ_GT,$3) }
	|	expr { ($1) }

class_declaration: classFront parameter_port_listE classExtendsE classImplementsE SEMICOLON /* 23 */ class_itemListE Endclass endLabelE { TUPLE9(STRING("class_declaration2562"),$1,$2,$3,$4,SEMICOLON,$6,Endclass,$8) }

classFront: classVirtualE Class lifetimeE idAny { TUPLE5(STRING("classFront2563"),$1,Class,$3,$4) }
	|	Interface Class lifetimeE idAny { TUPLE5(STRING("classFront2564"),Interface,Class,$3,$4) }

classVirtualE: /* empty */ { EMPTY_TOKEN }
	|	Virtual_HYPHEN_then_HYPHEN_class { (Virtual_HYPHEN_then_HYPHEN_class) }

classExtendsE: /* empty */ { EMPTY_TOKEN }
	|	Extends classExtendsList { TUPLE3(STRING("classExtendsE2568"),Extends,$2) }

classExtendsList: classExtendsOne { CONS1 ($1) }
	|	classExtendsList COMMA classExtendsOne { CONS3($1,COMMA,$3) }

classExtendsOne: class_typeExtImpList { ($1) }
	|	class_typeExtImpList LPAREN list_of_argumentsE RPAREN { TUPLE5(STRING("classExtendsOne2572"),$1,LPAREN,$3,RPAREN) }

classImplementsE: /* empty */ { EMPTY_TOKEN }
	|	Implements classImplementsList { TUPLE3(STRING("classImplementsE2574"),Implements,$2) }

classImplementsList: class_typeExtImpList { CONS1 ($1) }
	|	classImplementsList COMMA class_typeExtImpList { CONS3($1,COMMA,$3) }

class_typeExtImpList: class_typeExtImpOne { CONS1 ($1) }
	|	class_typeExtImpList COLON_COLON class_typeExtImpOne { CONS3($1,COLON_COLON,$3) }

class_typeExtImpOne: idAny /* 24 */ parameter_value_assignmentE { TUPLE3(STRING("class_typeExtImpOne2580"),$1,$2) }
	|	DLR_unit COLON_COLON { TUPLE3(STRING("class_typeExtImpOne2581"),DLR_unit,COLON_COLON) }
	|	Local_HYPHEN_then_HYPHEN_COLON_COLON COLON_COLON { TUPLE3(STRING("class_typeExtImpOne2582"),Local_HYPHEN_then_HYPHEN_COLON_COLON,COLON_COLON) }

packageClassScopeNoId: packageClassScope { ($1) }

packageClassScopeE: /* empty */ { EMPTY_TOKEN }
	|	packageClassScope { ($1) }

packageClassScope: packageClassScopeList { ($1) }
	|	localNextId COLON_COLON { TUPLE3(STRING("packageClassScope2587"),$1,COLON_COLON) }
	|	dollarUnitNextId COLON_COLON { TUPLE3(STRING("packageClassScope2588"),$1,COLON_COLON) }
	|	dollarUnitNextId COLON_COLON packageClassScopeList { TUPLE4(STRING("packageClassScope2589"),$1,COLON_COLON,$3) }

packageClassScopeList: packageClassScopeItem { CONS1 ($1) }
	|	packageClassScopeList packageClassScopeItem { CONS2($1,$2) }

packageClassScopeItem: idCC /* 25 */ COLON_COLON { TUPLE3(STRING("packageClassScopeItem2593"),$1,COLON_COLON) }
	|	idCC parameter_value_assignment /* 26 */ COLON_COLON { TUPLE4(STRING("packageClassScopeItem2595"),$1,$2,COLON_COLON) }

dollarUnitNextId: DLR_unit { (DLR_unit) }

localNextId: Local_HYPHEN_then_HYPHEN_COLON_COLON { (Local_HYPHEN_then_HYPHEN_COLON_COLON) }

class_itemListE: /* empty */ { EMPTY_TOKEN }
	|	class_itemList { ($1) }

class_itemList: class_item { CONS1 ($1) }
	|	class_itemList class_item { CONS2($1,$2) }

class_item: class_property { ($1) }
	|	class_method { ($1) }
	|	class_constraint { ($1) }
	|	class_declaration { ($1) }
	|	timeunits_declaration { ($1) }
	|	parameter_declaration SEMICOLON { TUPLE3(STRING("class_item2607"),$1,SEMICOLON) }
	|	SEMICOLON { (SEMICOLON) }
	|	ERROR_TOKEN SEMICOLON { TUPLE3(STRING("class_item2609"),ERROR_TOKEN,SEMICOLON) }

class_method: memberQualListE task_declaration { TUPLE3(STRING("class_method2610"),$1,$2) }
	|	memberQualListE function_declaration { TUPLE3(STRING("class_method2611"),$1,$2) }
	|	Pure Virtual memberQualListE method_prototype SEMICOLON { TUPLE6(STRING("class_method2612"),Pure,Virtual,$3,$4,SEMICOLON) }
	|	Extern memberQualListE method_prototype SEMICOLON { TUPLE5(STRING("class_method2613"),Extern,$2,$3,SEMICOLON) }
	|	Extern memberQualListE class_constructor_prototype { TUPLE4(STRING("class_method2614"),Extern,$2,$3) }

memberQualListE: /* empty */ { EMPTY_TOKEN }
	|	memberQualList { ($1) }

memberQualList: memberQualOne { CONS1 ($1) }
	|	memberQualList memberQualOne { CONS2($1,$2) }

memberQualOne: Protected { (Protected) }
	|	Local { (Local) }
	|	Static { (Static) }
	|	Virtual { (Virtual) }
	|	random_qualifier { ($1) }
	|	Automatic { (Automatic) }
	|	Const { (Const) }

class_constraint: constraintStaticE Constraint idAny constraint_block { TUPLE5(STRING("class_constraint2626"),$1,Constraint,$3,$4) }
	|	constraintStaticE Constraint idAny SEMICOLON { TUPLE5(STRING("class_constraint2627"),$1,Constraint,$3,SEMICOLON) }
	|	Extern constraintStaticE Constraint idAny SEMICOLON { TUPLE6(STRING("class_constraint2628"),Extern,$2,Constraint,$4,SEMICOLON) }
	|	Pure constraintStaticE Constraint idAny SEMICOLON { TUPLE6(STRING("class_constraint2629"),Pure,$2,Constraint,$4,SEMICOLON) }

constraint_block: LBRACE constraint_block_itemList RBRACE { TUPLE4(STRING("constraint_block2630"),LBRACE,$2,RBRACE) }

constraint_block_itemList: constraint_block_item { CONS1 ($1) }
	|	constraint_block_itemList constraint_block_item { CONS2($1,$2) }

constraint_block_item: constraint_expression { ($1) }
	|	Solve solve_before_list Before solve_before_list SEMICOLON { TUPLE6(STRING("constraint_block_item2634"),Solve,$2,Before,$4,SEMICOLON) }

solve_before_list: constraint_primary { ($1) }
	|	solve_before_list COMMA constraint_primary { TUPLE4(STRING("solve_before_list2636"),$1,COMMA,$3) }

constraint_primary: exprScope { ($1) }

constraint_expressionList: constraint_expression { CONS1 ($1) }
	|	constraint_expressionList constraint_expression { CONS2($1,$2) }

constraint_expression: expr SEMICOLON { TUPLE3(STRING("constraint_expression2640"),$1,SEMICOLON) }
	|	Soft expr SEMICOLON { TUPLE4(STRING("constraint_expression2641"),Soft,$2,SEMICOLON) }
	|	Unique LBRACE open_range_list RBRACE { TUPLE5(STRING("constraint_expression2642"),Unique,LBRACE,$3,RBRACE) }
	|	If LPAREN expr RPAREN constraint_set { TUPLE6(STRING("constraint_expression2643"),If,LPAREN,$3,RPAREN,$5) }
	|	If LPAREN expr RPAREN constraint_set Else constraint_set { TUPLE8(STRING("constraint_expression2644"),If,LPAREN,$3,RPAREN,$5,Else,$7) }
	|	Foreach LPAREN idClassSelForeach RPAREN constraint_set { TUPLE6(STRING("constraint_expression2645"),Foreach,LPAREN,$3,RPAREN,$5) }
	|	Disable Soft expr SEMICOLON { TUPLE5(STRING("constraint_expression2646"),Disable,Soft,$3,SEMICOLON) }

constraint_set: constraint_expression { ($1) }
	|	LBRACE constraint_expressionList RBRACE { TUPLE4(STRING("constraint_set2648"),LBRACE,$2,RBRACE) }

dist_list: dist_item { ($1) }
	|	dist_list COMMA dist_item { TUPLE4(STRING("dist_list2650"),$1,COMMA,$3) }

dist_item: value_range { ($1) }
	|	value_range COLON_EQ expr { TUPLE4(STRING("dist_item2652"),$1,COLON_EQ,$3) }
	|	value_range COLON_SLASH expr { TUPLE4(STRING("dist_item2653"),$1,COLON_SLASH,$3) }

constraintStaticE: /* empty */ { EMPTY_TOKEN }
	|	Static_HYPHEN_then_HYPHEN_constraint { (Static_HYPHEN_then_HYPHEN_constraint) }

timeNumAdjusted: TIME_NUMBER { (TIME_NUMBER) }

colon: COLON { (COLON) }
	|	COLON_HYPHEN_begin { (COLON_HYPHEN_begin) }
	|	COLON_HYPHEN_fork { (COLON_HYPHEN_fork) }

vltItem: vltOffFront { ($1) }
	|	vltOffFront HYPHEN_HYPHEN_file STRING { TUPLE4(STRING("vltItem2661"),$1,HYPHEN_HYPHEN_file,STRING $3) }
	|	vltOffFront HYPHEN_HYPHEN_file STRING HYPHEN_HYPHEN_lines INTEGER_NUMBER { TUPLE6(STRING("vltItem2662"),$1,HYPHEN_HYPHEN_file,STRING $3,HYPHEN_HYPHEN_lines,INTEGER_NUMBER $5) }
	|	vltOffFront HYPHEN_HYPHEN_file STRING HYPHEN_HYPHEN_lines INTEGER_NUMBER HYPHEN INTEGER_NUMBER { TUPLE8(STRING("vltItem2663"),$1,HYPHEN_HYPHEN_file,STRING $3,HYPHEN_HYPHEN_lines,INTEGER_NUMBER $5,HYPHEN,INTEGER_NUMBER $7) }
	|	vltOffFront HYPHEN_HYPHEN_file STRING HYPHEN_HYPHEN_match STRING { TUPLE6(STRING("vltItem2664"),$1,HYPHEN_HYPHEN_file,STRING $3,HYPHEN_HYPHEN_match,STRING $5) }
	|	vltOnFront { ($1) }
	|	vltOnFront HYPHEN_HYPHEN_file STRING { TUPLE4(STRING("vltItem2666"),$1,HYPHEN_HYPHEN_file,STRING $3) }
	|	vltOnFront HYPHEN_HYPHEN_file STRING HYPHEN_HYPHEN_lines INTEGER_NUMBER { TUPLE6(STRING("vltItem2667"),$1,HYPHEN_HYPHEN_file,STRING $3,HYPHEN_HYPHEN_lines,INTEGER_NUMBER $5) }
	|	vltOnFront HYPHEN_HYPHEN_file STRING HYPHEN_HYPHEN_lines INTEGER_NUMBER HYPHEN INTEGER_NUMBER { TUPLE8(STRING("vltItem2668"),$1,HYPHEN_HYPHEN_file,STRING $3,HYPHEN_HYPHEN_lines,INTEGER_NUMBER $5,HYPHEN,INTEGER_NUMBER $7) }
	|	vltVarAttrFront vltDModuleE vltDFTaskE vltVarAttrVarE attr_event_controlE { TUPLE6(STRING("vltItem2669"),$1,$2,$3,$4,$5) }
	|	vltInlineFront vltDModuleE vltDFTaskE { TUPLE4(STRING("vltItem2670"),$1,$2,$3) }
	|	Coverage_block_off HYPHEN_HYPHEN_file STRING { TUPLE4(STRING("vltItem2671"),Coverage_block_off,HYPHEN_HYPHEN_file,STRING $3) }
	|	Coverage_block_off HYPHEN_HYPHEN_file STRING HYPHEN_HYPHEN_lines INTEGER_NUMBER { TUPLE6(STRING("vltItem2672"),Coverage_block_off,HYPHEN_HYPHEN_file,STRING $3,HYPHEN_HYPHEN_lines,INTEGER_NUMBER $5) }
	|	Coverage_block_off HYPHEN_HYPHEN_module STRING HYPHEN_HYPHEN_block STRING { TUPLE6(STRING("vltItem2673"),Coverage_block_off,HYPHEN_HYPHEN_module,STRING $3,HYPHEN_HYPHEN_block,STRING $5) }
	|	Full_case HYPHEN_HYPHEN_file STRING { TUPLE4(STRING("vltItem2674"),Full_case,HYPHEN_HYPHEN_file,STRING $3) }
	|	Full_case HYPHEN_HYPHEN_file STRING HYPHEN_HYPHEN_lines INTEGER_NUMBER { TUPLE6(STRING("vltItem2675"),Full_case,HYPHEN_HYPHEN_file,STRING $3,HYPHEN_HYPHEN_lines,INTEGER_NUMBER $5) }
	|	Hier_block vltDModuleE { TUPLE3(STRING("vltItem2676"),Hier_block,$2) }
	|	Parallel_case HYPHEN_HYPHEN_file STRING { TUPLE4(STRING("vltItem2677"),Parallel_case,HYPHEN_HYPHEN_file,STRING $3) }
	|	Parallel_case HYPHEN_HYPHEN_file STRING HYPHEN_HYPHEN_lines INTEGER_NUMBER { TUPLE6(STRING("vltItem2678"),Parallel_case,HYPHEN_HYPHEN_file,STRING $3,HYPHEN_HYPHEN_lines,INTEGER_NUMBER $5) }

vltOffFront: Coverage_off { (Coverage_off) }
	|	Tracing_off { (Tracing_off) }
	|	Lint_off { (Lint_off) }
	|	Lint_off HYPHEN_HYPHEN_msg idAny { TUPLE4(STRING("vltOffFront2682"),Lint_off,HYPHEN_HYPHEN_msg,$3) }
	|	Lint_off HYPHEN_HYPHEN_rule idAny { TUPLE4(STRING("vltOffFront2683"),Lint_off,HYPHEN_HYPHEN_rule,$3) }

vltOnFront: Coverage_on { (Coverage_on) }
	|	Tracing_on { (Tracing_on) }
	|	Lint_on { (Lint_on) }
	|	Lint_on HYPHEN_HYPHEN_msg idAny { TUPLE4(STRING("vltOnFront2687"),Lint_on,HYPHEN_HYPHEN_msg,$3) }
	|	Lint_on HYPHEN_HYPHEN_rule idAny { TUPLE4(STRING("vltOnFront2688"),Lint_on,HYPHEN_HYPHEN_rule,$3) }

vltDModuleE: /* empty */ { EMPTY_TOKEN }
	|	HYPHEN_HYPHEN_module str { TUPLE3(STRING("vltDModuleE2690"),HYPHEN_HYPHEN_module,$2) }

vltDFTaskE: /* empty */ { EMPTY_TOKEN }
	|	HYPHEN_HYPHEN_function str { TUPLE3(STRING("vltDFTaskE2692"),HYPHEN_HYPHEN_function,$2) }
	|	HYPHEN_HYPHEN_task str { TUPLE3(STRING("vltDFTaskE2693"),HYPHEN_HYPHEN_task,$2) }

vltInlineFront: Inline { (Inline) }
	|	No_inline { (No_inline) }

vltVarAttrVarE: /* empty */ { EMPTY_TOKEN }
	|	HYPHEN_HYPHEN_var str { TUPLE3(STRING("vltVarAttrVarE2697"),HYPHEN_HYPHEN_var,$2) }

vltVarAttrFront: Clock_enable { (Clock_enable) }
	|	Clocker { (Clocker) }
	|	Isolate_assignments { (Isolate_assignments) }
	|	No_clocker { (No_clocker) }
	|	Public { (Public) }
	|	Public_flat { (Public_flat) }
	|	Public_flat_rd { (Public_flat_rd) }
	|	Public_flat_rw { (Public_flat_rw) }
	|	Sc_bv { (Sc_bv) }
	|	Sformat { (Sformat) }
	|	Split_var { (Split_var) }


