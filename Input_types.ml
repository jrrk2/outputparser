  open Input
let getstr = function
| ACCEPT  -> "ACCEPT"
| AMPERSAND  -> "AMPERSAND"
| AT  -> "AT"
| ATTR_BEGIN  -> "ATTR_BEGIN"
| ATTR_END  -> "ATTR_END"
| BACKQUOTE  -> "BACKQUOTE"
| BACKSLASH  -> "BACKSLASH"
| CARET  -> "CARET"
| COLON  -> "COLON"
| COMMA  -> "COMMA"
| CONS1  _ -> "CONS1"
| CONS2  _ -> "CONS2"
| CONS3  _ -> "CONS3"
| CONS4  _ -> "CONS4"
| DEFATTR_BEGIN  -> "DEFATTR_BEGIN"
| DEFATTR_END  -> "DEFATTR_END"
| DEFAULT  -> "DEFAULT"
| DOLLAR  -> "DOLLAR"
| DOT  -> "DOT"
| DOUBLEQUOTE  -> "DOUBLEQUOTE"
| ELIST  _ -> "ELIST"
| EMPTY_TOKEN  -> "EMPTY_TOKEN"
| END  -> "END"
| EOF_TOKEN  -> "EOF_TOKEN"
| EQUALS  -> "EQUALS"
| ERROR  -> "ERROR"
| ERROR_TOKEN  -> "ERROR_TOKEN"
| FAKE_THEN  -> "FAKE_THEN"
| GREATER  -> "GREATER"
| HASH  -> "HASH"
| HYPHEN  -> "HYPHEN"
| LBRACE  -> "LBRACE"
| LBRACK  -> "LBRACK"
| LESS  -> "LESS"
| LINEFEED  -> "LINEFEED"
| LPAREN  -> "LPAREN"
| OP_EQ  -> "OP_EQ"
| OP_EQX  -> "OP_EQX"
| OP_GE  -> "OP_GE"
| OP_LAND  -> "OP_LAND"
| OP_LE  -> "OP_LE"
| OP_LOR  -> "OP_LOR"
| OP_NAND  -> "OP_NAND"
| OP_NE  -> "OP_NE"
| OP_NEX  -> "OP_NEX"
| OP_NOR  -> "OP_NOR"
| OP_POW  -> "OP_POW"
| OP_SHL  -> "OP_SHL"
| OP_SHR  -> "OP_SHR"
| OP_SSHL  -> "OP_SSHL"
| OP_SSHR  -> "OP_SSHR"
| OP_XNOR  -> "OP_XNOR"
| PERCENT  -> "PERCENT"
| PLING  -> "PLING"
| PLUS  -> "PLUS"
| QUERY  -> "QUERY"
| QUOTE  -> "QUOTE"
| RBRACE  -> "RBRACE"
| RBRACK  -> "RBRACK"
| RPAREN  -> "RPAREN"
| SEMICOLON  -> "SEMICOLON"
| SLASH  -> "SLASH"
| SLIST  _ -> "SLIST"
| STAR  -> "STAR"
| STRING  _ -> "STRING"
| TILDE  -> "TILDE"
| TLIST  _ -> "TLIST"
| TOK_ALWAYS  -> "TOK_ALWAYS"
| TOK_ALWAYS_COMB  -> "TOK_ALWAYS_COMB"
| TOK_ALWAYS_FF  -> "TOK_ALWAYS_FF"
| TOK_ALWAYS_LATCH  -> "TOK_ALWAYS_LATCH"
| TOK_AND_ASSIGN  -> "TOK_AND_ASSIGN"
| TOK_ASSERT  -> "TOK_ASSERT"
| TOK_ASSIGN  -> "TOK_ASSIGN"
| TOK_ASSUME  -> "TOK_ASSUME"
| TOK_AUTOMATIC  -> "TOK_AUTOMATIC"
| TOK_BASE  -> "TOK_BASE"
| TOK_BASED_CONSTVAL  -> "TOK_BASED_CONSTVAL"
| TOK_BEGIN  -> "TOK_BEGIN"
| TOK_BYTE  -> "TOK_BYTE"
| TOK_CASE  -> "TOK_CASE"
| TOK_CASEX  -> "TOK_CASEX"
| TOK_CASEZ  -> "TOK_CASEZ"
| TOK_CHECKER  -> "TOK_CHECKER"
| TOK_CONST  -> "TOK_CONST"
| TOK_CONSTVAL  -> "TOK_CONSTVAL"
| TOK_COVER  -> "TOK_COVER"
| TOK_DECREMENT  -> "TOK_DECREMENT"
| TOK_DEFAULT  -> "TOK_DEFAULT"
| TOK_DEFPARAM  -> "TOK_DEFPARAM"
| TOK_DPI_FUNCTION  -> "TOK_DPI_FUNCTION"
| TOK_ELSE  -> "TOK_ELSE"
| TOK_END  -> "TOK_END"
| TOK_ENDCASE  -> "TOK_ENDCASE"
| TOK_ENDCHECKER  -> "TOK_ENDCHECKER"
| TOK_ENDFUNCTION  -> "TOK_ENDFUNCTION"
| TOK_ENDGENERATE  -> "TOK_ENDGENERATE"
| TOK_ENDINTERFACE  -> "TOK_ENDINTERFACE"
| TOK_ENDMODULE  -> "TOK_ENDMODULE"
| TOK_ENDPACKAGE  -> "TOK_ENDPACKAGE"
| TOK_ENDSPECIFY  -> "TOK_ENDSPECIFY"
| TOK_ENDTASK  -> "TOK_ENDTASK"
| TOK_ENUM  -> "TOK_ENUM"
| TOK_EVENTUALLY  -> "TOK_EVENTUALLY"
| TOK_FINAL  -> "TOK_FINAL"
| TOK_FOR  -> "TOK_FOR"
| TOK_FUNCTION  -> "TOK_FUNCTION"
| TOK_GENERATE  -> "TOK_GENERATE"
| TOK_GENVAR  -> "TOK_GENVAR"
| TOK_HIGHZ0  -> "TOK_HIGHZ0"
| TOK_HIGHZ1  -> "TOK_HIGHZ1"
| TOK_ID  -> "TOK_ID"
| TOK_IF  -> "TOK_IF"
| TOK_IGNORED_SPECIFY  -> "TOK_IGNORED_SPECIFY"
| TOK_IGNORED_SPECIFY_AND  -> "TOK_IGNORED_SPECIFY_AND"
| TOK_IMPORT_PACKAGE  -> "TOK_IMPORT_PACKAGE"
| TOK_INCREMENT  -> "TOK_INCREMENT"
| TOK_INITIAL  -> "TOK_INITIAL"
| TOK_INOUT  -> "TOK_INOUT"
| TOK_INPUT  -> "TOK_INPUT"
| TOK_INSIDE  -> "TOK_INSIDE"
| TOK_INT  -> "TOK_INT"
| TOK_INTEGER  -> "TOK_INTEGER"
| TOK_INTERFACE  -> "TOK_INTERFACE"
| TOK_LOCALPARAM  -> "TOK_LOCALPARAM"
| TOK_LOGIC  -> "TOK_LOGIC"
| TOK_MODPORT  -> "TOK_MODPORT"
| TOK_MODULE  -> "TOK_MODULE"
| TOK_MSG_TASKS  -> "TOK_MSG_TASKS"
| TOK_NEGEDGE  -> "TOK_NEGEDGE"
| TOK_NEG_INDEXED  -> "TOK_NEG_INDEXED"
| TOK_OR  -> "TOK_OR"
| TOK_OR_ASSIGN  -> "TOK_OR_ASSIGN"
| TOK_OUTPUT  -> "TOK_OUTPUT"
| TOK_PACKAGE  -> "TOK_PACKAGE"
| TOK_PACKAGESEP  -> "TOK_PACKAGESEP"
| TOK_PACKED  -> "TOK_PACKED"
| TOK_PARAMETER  -> "TOK_PARAMETER"
| TOK_PKG_USER_TYPE  -> "TOK_PKG_USER_TYPE"
| TOK_PLUS_ASSIGN  -> "TOK_PLUS_ASSIGN"
| TOK_POSEDGE  -> "TOK_POSEDGE"
| TOK_POS_INDEXED  -> "TOK_POS_INDEXED"
| TOK_PRIMITIVE  -> "TOK_PRIMITIVE"
| TOK_PRIORITY  -> "TOK_PRIORITY"
| TOK_PROPERTY  -> "TOK_PROPERTY"
| TOK_PULL0  -> "TOK_PULL0"
| TOK_PULL1  -> "TOK_PULL1"
| TOK_RAND  -> "TOK_RAND"
| TOK_REAL  -> "TOK_REAL"
| TOK_REALVAL  -> "TOK_REALVAL"
| TOK_REG  -> "TOK_REG"
| TOK_REPEAT  -> "TOK_REPEAT"
| TOK_RESTRICT  -> "TOK_RESTRICT"
| TOK_RETURN  -> "TOK_RETURN"
| TOK_SHORTINT  -> "TOK_SHORTINT"
| TOK_SIGNED  -> "TOK_SIGNED"
| TOK_SPECIFY  -> "TOK_SPECIFY"
| TOK_SPECIFY_AND  -> "TOK_SPECIFY_AND"
| TOK_SPECIFY_OPER  -> "TOK_SPECIFY_OPER"
| TOK_SPECPARAM  -> "TOK_SPECPARAM"
| TOK_STRING  -> "TOK_STRING"
| TOK_STRONG0  -> "TOK_STRONG0"
| TOK_STRONG1  -> "TOK_STRONG1"
| TOK_STRUCT  -> "TOK_STRUCT"
| TOK_SUB_ASSIGN  -> "TOK_SUB_ASSIGN"
| TOK_SUPPLY0  -> "TOK_SUPPLY0"
| TOK_SUPPLY1  -> "TOK_SUPPLY1"
| TOK_SVA_LABEL  -> "TOK_SVA_LABEL"
| TOK_SYNOPSYS_FULL_CASE  -> "TOK_SYNOPSYS_FULL_CASE"
| TOK_SYNOPSYS_PARALLEL_CASE  -> "TOK_SYNOPSYS_PARALLEL_CASE"
| TOK_TASK  -> "TOK_TASK"
| TOK_TO_SIGNED  -> "TOK_TO_SIGNED"
| TOK_TO_UNSIGNED  -> "TOK_TO_UNSIGNED"
| TOK_TYPEDEF  -> "TOK_TYPEDEF"
| TOK_UNBASED_UNSIZED_CONSTVAL  -> "TOK_UNBASED_UNSIZED_CONSTVAL"
| TOK_UNION  -> "TOK_UNION"
| TOK_UNIQUE  -> "TOK_UNIQUE"
| TOK_UNSIGNED  -> "TOK_UNSIGNED"
| TOK_USER_TYPE  -> "TOK_USER_TYPE"
| TOK_VAR  -> "TOK_VAR"
| TOK_WAND  -> "TOK_WAND"
| TOK_WEAK0  -> "TOK_WEAK0"
| TOK_WEAK1  -> "TOK_WEAK1"
| TOK_WHILE  -> "TOK_WHILE"
| TOK_WILDCARD_CONNECT  -> "TOK_WILDCARD_CONNECT"
| TOK_WIRE  -> "TOK_WIRE"
| TOK_WOR  -> "TOK_WOR"
| TOK_XOR_ASSIGN  -> "TOK_XOR_ASSIGN"
| TUPLE10  _ -> "TUPLE10"
| TUPLE11  _ -> "TUPLE11"
| TUPLE12  _ -> "TUPLE12"
| TUPLE13  _ -> "TUPLE13"
| TUPLE14  _ -> "TUPLE14"
| TUPLE15  _ -> "TUPLE15"
| TUPLE16  _ -> "TUPLE16"
| TUPLE17  _ -> "TUPLE17"
| TUPLE18  _ -> "TUPLE18"
| TUPLE19  _ -> "TUPLE19"
| TUPLE2  _ -> "TUPLE2"
| TUPLE20  _ -> "TUPLE20"
| TUPLE21  _ -> "TUPLE21"
| TUPLE22  _ -> "TUPLE22"
| TUPLE23  _ -> "TUPLE23"
| TUPLE24  _ -> "TUPLE24"
| TUPLE25  _ -> "TUPLE25"
| TUPLE26  _ -> "TUPLE26"
| TUPLE3  _ -> "TUPLE3"
| TUPLE4  _ -> "TUPLE4"
| TUPLE5  _ -> "TUPLE5"
| TUPLE6  _ -> "TUPLE6"
| TUPLE7  _ -> "TUPLE7"
| TUPLE8  _ -> "TUPLE8"
| TUPLE9  _ -> "TUPLE9"
| UNARY_OPS  -> "UNARY_OPS"
| UNDERSCORE  -> "UNDERSCORE"
| VBAR  -> "VBAR"

let (typehash:(string,unit)Hashtbl.t) = Hashtbl.create 257

let (packhash:(string,unit)Hashtbl.t) = Hashtbl.create 257
