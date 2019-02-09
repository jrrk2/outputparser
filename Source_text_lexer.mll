(**************************************************************************)
(*                                                                        *)
(* OCaml template Copyright (C) 2004-2010                                 *)
(*  Sylvain Conchon, Jean-Christophe Filliatre and Julien Signoles        *)
(* Adapted to boolean logic by Jonathan Kimmitt                           *)
(*  Copyright 2016 University of Cambridge                                *)
(*                                                                        *)
(*  This software is free software; you can redistribute it and/or        *)
(*  modify it under the terms of the GNU Library General Public           *)
(*  License version 2.1, with the special exception on linking            *)
(*  described in file LICENSE.                                            *)
(*                                                                        *)
(*  This software is distributed in the hope that it will be useful,      *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                  *)
(*                                                                        *)
(**************************************************************************)

{
  open Lexing
  open Source_text_edited

  let verbose = ref true
  let lincnt = ref 0

  let keyword =
    let h = Hashtbl.create 17 in
    List.iter 
      (fun (k,s) -> Hashtbl.add h s k)
      [
(Q_DOLLAR_SQRT,"$sqrt");
(Q_AMPERSAND__EQUALS_,"&=");
(TABLE,"table");
(Q_DOLLAR_PAST,"$past");
(ENDCLOCKING,"endclocking");
(Q_DOLLAR_RANDOM,"$random");
(Q_DOLLAR_ONEHOT0,"$onehot0");
(SUPPLY0,"supply0");
(TYPE_HYPHEN_IDENTIFIER,"TYPE-IDENTIFIER");
(SPECIFY,"specify");
(UNIQUE,"unique");
(TRI0,"tri0");
(SCALARED,"scalared");
(Q_GREATER__GREATER__GREATER__EQUALS_,">>>=");
(Q_DOLLAR_ERROR,"$error");
(Q_DOLLAR_ONEHOT,"$onehot");
(Q_COLON__SLASH_,":/");
(Q_DOLLAR_DISPLAY,"$display");
(RANDC,"randc");
(TRACING_UNDERSCORE_ON,"tracing_on");
(Q_VBAR__EQUALS__GREATER_,"|=>");
(Q_SLASH__STAR_VERILATOR_BLANK_PUBLIC_UNDERSCORE_FLAT_UNDERSCORE_RW_STAR__SLASH_,"/*verilator public_flat_rw*/");
(Q_AMPERSAND__AMPERSAND__AMPERSAND_,"&&&");
(Q_DOLLAR_ASINH,"$asinh");
(UNIQUE0,"unique0");
(BEGIN,"begin");
(REAL,"real");
(RTRANIF1,"rtranif1");
(Q_DOLLAR_FWRITE,"$fwrite");
(Q_LESS__LESS__EQUALS_,"<<=");
(Q_SLASH__EQUALS_,"/=");
(Q_LBRACK__EQUALS_,"[=");
(NOTIF1,"notif1");
(Q_SLASH__STAR_VERILATOR_BLANK_CLOCKER_STAR__SLASH_,"/*verilator clocker*/");
(Q_DOLLAR_COS,"$cos");
(ASSIGN,"assign");
(Q_HYPHEN__GREATER__GREATER_,"->>");
(PMOS,"pmos");
(LONGINT,"longint");
(CASE,"case");
(TIME_BLANK_NUMBER,"TIME NUMBER");
(Q_COLON__EQUALS_,":=");
(INPUT,"input");
(ENDPACKAGE,"endpackage");
(GLOBAL_HYPHEN_IN_HYPHEN_LEX,"global-in-lex");
(Q_DOLLAR_POW,"$pow");
(Q_VBAR__HYPHEN__GREATER_,"|->");
(TABLE_BLANK_LINE,"TABLE LINE");
(Q_DOLLAR_INCREMENT,"$increment");
(Q_HYPHEN__HYPHEN_,"--");
(Q_HYPHEN__EQUALS_,"-=");
(Q_HYPHEN__HYPHEN_LINES,"--lines");
(Q_CARET__EQUALS_,"^=");
(LINT_UNDERSCORE_OFF,"lint_off");
(Q_EQUALS__EQUALS__QUERY_,"==?");
(Q_DOLLAR_ATAN,"$atan");
(CASEX,"casex");
(ENDINTERFACE,"endinterface");
(TRANIF1,"tranif1");
(Q_DOLLAR_INFO,"$info");
(IDENTIFIER_HYPHEN_IN_HYPHEN_LEX,"IDENTIFIER-in-lex");
(Q_HYPHEN__HYPHEN_MSG,"--msg");
(Q_STAR__EQUALS_,"*=");
(TIMEPRECISION,"timeprecision");
(Q_LBRACK__STAR_,"[*");
(REALTIME,"realtime");
(EXTERN,"extern");
(Q_STAR__STAR_,"**");
(FOR,"for");
(COVERAGE_UNDERSCORE_ON,"coverage_on");
(CONTEXT,"context");
(Q_DOLLAR_RTOI,"$rtoi");
(Q_GREATER__GREATER__GREATER_,">>>");
(Q_TILDE__AMPERSAND_,"~&");
(PRIORITY,"priority");
(ALWAYS_UNDERSCORE_COMB,"always_comb");
(PURE,"pure");
(Q_DOLLAR_SIN,"$sin");
(TASK,"task");
(Q_DOLLAR_ATANH,"$atanh");
(ELSE,"else");
(ENDMODULE,"endmodule");
(Q_SLASH__STAR_VERILATOR_BLANK_FULL_UNDERSCORE_CASE_STAR__SLASH_,"/*verilator full_case*/");
(Q_DOLLAR_HIGH,"$high");
(Q_DOLLAR_COSH,"$cosh");
(Q_VBAR__EQUALS_,"|=");
(RESTRICT,"restrict");
(Q_DOLLAR_FATAL,"$fatal");
(Q_LESS__EQUALS__HYPHEN_IGNORED,"<=-ignored");
(Q_DOLLAR_FFLUSH,"$fflush");
(ASSUME,"assume");
(FOREACH,"foreach");
(FORKJOIN,"forkjoin");
(RPMOS,"rpmos");
(Q_DOLLAR_FSCANF,"$fscanf");
(Q_DOLLAR_FINISH,"$finish");
(Q_PERCENT__EQUALS_,"%=");
(XOR,"xor");
(Q_BACKQUOTE_SYSTEMC_UNDERSCORE_HEADER_BLANK_BLOCK,"`systemc_header BLOCK");
(GENERATE,"generate");
(Q_DOLLAR_RIGHT,"$right");
(NMOS,"nmos");
(Q_HYPHEN__GREATER_,"->");
(Q_PLING__EQUALS__EQUALS_,"!==");
(Q_TILDE__VBAR_,"~|");
(Q_DOLLAR_CEIL,"$ceil");
(Q_DOLLAR_SINH,"$sinh");
(Q_GREATER__GREATER_,">>");
(Q_DOLLAR_WRITEMEMH,"$writememh");
(ASSERT,"assert");
(VAR,"var");
(Q_DOLLAR__LBRACE_DPI_HYPHEN_SYS_RBRACE_,"${dpi-sys}");
(Q_DOLLAR_TANH,"$tanh");
(Q_CARET__TILDE_,"^~");
(Q_DOLLAR_VALUE_DOLLAR_PLUSARGS,"$value$plusargs");
(CONTINUE,"continue");
(TYPEDEF,"typedef");
(ENUM,"enum");
(Q_SLASH__STAR_VERILATOR_BLANK_PARALLEL_UNDERSCORE_CASE_STAR__SLASH_,"/*verilator parallel_case*/");
(Q_DOLLAR_READMEMB,"$readmemb");
(Q_GREATER__GREATER__EQUALS_,">>=");
(Q_DOLLAR_WARNING,"$warning");
(DO,"do");
(BIND,"bind");
(UNION,"union");
(XNOR,"xnor");
(Q_DOLLAR_REALTIME,"$realtime");
(Q_DOLLAR_FDISPLAY,"$fdisplay");
(SHORTINT,"shortint");
(Q_DOLLAR_FEOF,"$feof");
(NEGEDGE,"negedge");
(FOREVER,"forever");
(ENDTASK,"endtask");
(Q_GREATER__EQUALS_,">=");
(Q_SLASH__STAR_VERILATOR_BLANK_PUBLIC_STAR__SLASH_,"/*verilator public*/");
(Q_DOLLAR_UNSIGNED,"$unsigned");
(Q_DOLLAR_TIME,"$time");
(TIMING_BLANK_SPEC_BLANK_ELEMENT,"TIMING SPEC ELEMENT");
(Q_DOLLAR_ITOR,"$itor");
(Q_DOLLAR_DIMENSIONS,"$dimensions");
(Q_DOLLAR_SFORMAT,"$sformat");
(ENDSPECIFY,"endspecify");
(Q_HASH__HASH_,"##");
(Q_DOLLAR_STIME,"$stime");
(CHANDLE,"chandle");
(PROPERTY,"property");
(Q_DOLLAR_LOG10,"$log10");
(Q_DOLLAR_FLOOR,"$floor");
(Q_SLASH__STAR_VERILATOR_BLANK_SFORMAT_STAR__SLASH_,"/*verilator sformat*/");
(Q_STAR__GREATER_,"*>");
(Q_DOLLAR__LBRACE_IGNORED_HYPHEN_BBOX_HYPHEN_SYS_RBRACE_,"${ignored-bbox-sys}");
(ENDPRIMITIVE,"endprimitive");
(LOGIC,"logic");
(CASEZ,"casez");
(UNSIGNED,"unsigned");
(TRANIF0,"tranif0");
(BYTE,"byte");
(VECTORED,"vectored");
(Q_DOLLAR_SWRITE,"$swrite");
(Q_DOLLAR_SYSTEM,"$system");
(Q_PLING__EQUALS_,"!=");
(TRAN,"tran");
(Q_BACKQUOTE_SYSTEMC_UNDERSCORE_IMPLEMENTATION_BLANK_BLOCK,"`systemc_implementation BLOCK");
(Q_SLASH__STAR_VERILATOR_BLANK_SC_UNDERSCORE_CLOCK_STAR__SLASH_,"/*verilator sc_clock*/");
(VOID,"void");
(ENDGENERATE,"endgenerate");
(ENDPROPERTY,"endproperty");
(PRIMITIVE,"primitive");
(Q_SLASH__STAR_VERILATOR_BLANK_CLOCK_UNDERSCORE_ENABLE_STAR__SLASH_,"/*verilator clock_enable*/");
(Q_BACKQUOTE_SYSTEMC_UNDERSCORE_DTOR_BLANK_BLOCK,"`systemc_dtor BLOCK");
(Q_AT__AT_,"@@");
(Q_DOLLAR_CLOG2,"$clog2");
(BUFIF0,"bufif0");
(Q_SLASH__STAR_VERILATOR_BLANK_PUBLIC_UNDERSCORE_FLAT_STAR__SLASH_,"/*verilator public_flat*/");
(REF,"ref");
(TRI,"tri");
(SPECPARAM,"specparam");
(IF,"if");
(Q_PLING__EQUALS__QUERY_,"!=?");
(INSIDE,"inside");
(ALWAYS_UNDERSCORE_FF,"always_ff");
(Q_BACKQUOTE_SYSTEMC_UNDERSCORE_CTOR_BLANK_BLOCK,"`systemc_ctor BLOCK");
(NOTIF0,"notif0");
(Q_DOLLAR_STOP,"$stop");
(Q_DOT__STAR_,".*");
(END,"end");
(SUPPLY1,"supply1");
(Q_DOLLAR_ATAN2,"$atan2");
(FINAL,"final");
(DEFPARAM,"defparam");
(TRUE,"true");
(Q_SLASH__STAR_VERILATOR_BLANK_NO_UNDERSCORE_INLINE_UNDERSCORE_TASK_STAR__SLASH_,"/*verilator no_inline_task*/");
(CMOS,"cmos");
(Q_EQUALS__GREATER_,"=>");
(Q_DOLLAR_SIZE,"$size");
(FLOATING_HYPHEN_POINT_BLANK_NUMBER,"FLOATING-POINT NUMBER");
(IMPORT,"import");
(PROGRAM,"program");
(IFF,"iff");
(Q_DOLLAR_SIGNED,"$signed");
(Q_SLASH__STAR_VERILATOR_BLANK_NO_UNDERSCORE_INLINE_UNDERSCORE_MODULE_STAR__SLASH_,"/*verilator no_inline_module*/");
(Q_DOLLAR_C,"$c");
(CLOCKING,"clocking");
(Q_DOLLAR_LN,"$ln");
(ENDFUNCTION,"endfunction");
(Q_DOLLAR_FCLOSE,"$fclose");
(DEFAULT,"default");
(FUNCTION,"function");
(REG,"reg");
(Q_DOLLAR_REALTOBITS,"$realtobits");
(MODPORT,"modport");
(STRING_HYPHEN_IGNORED,"STRING-ignored");
(LOCALPARAM,"localparam");
(Q_HYPHEN__COLON_,"-:");
(OR,"or");
(RAND,"rand");
(Q_DOLLAR_ACOSH,"$acosh");
(EXPORT,"export");
(LINT_UNDERSCORE_ON,"lint_on");
(TYPE,"type");
(TRACING_UNDERSCORE_OFF,"tracing_off");
(INTEGER,"integer");
(GLOBAL_HYPHEN_THEN_HYPHEN_CLOCKING,"global-then-clocking");
(Q_DOLLAR_BITS,"$bits");
(PARAMETER,"parameter");
(GENVAR,"genvar");
(RTRANIF0,"rtranif0");
(NAND,"nand");
(STATIC,"static");
(TIMEUNIT,"timeunit");
(CONST,"const");
(AND,"and");
(Q_DOLLAR_FGETC,"$fgetc");
(Q_DOLLAR_ASIN,"$asin");
(ENDTABLE,"endtable");
(Q_DOLLAR_LEFT,"$left");
(NOR,"nor");
(Q_EQUALS__EQUALS__EQUALS_,"===");
(Q_HYPHEN__HYPHEN_FILE,"--file");
(BUF,"buf");
(Q_DOLLAR_ISUNKNOWN,"$isunknown");
(Q_DOLLAR_EXP,"$exp");
(Q_DOLLAR_COUNTONES,"$countones");
(Q_SLASH__STAR_VERILATOR_BLANK_NO_UNDERSCORE_CLOCKER_STAR__SLASH_,"/*verilator no_clocker*/");
(WIRE,"wire");
(PULLUP,"pullup");
(Q_SLASH__STAR_VERILATOR_BLANK_COVERAGE_UNDERSCORE_BLOCK_UNDERSCORE_OFF_STAR__SLASH_,"/*verilator coverage_block_off*/");
(WHILE,"while");
(Q_DOLLAR_UNIT,"$unit");
(COVER,"cover");
(RTRAN,"rtran");
(Q_DOLLAR_TEST_DOLLAR_PLUSARGS,"$test$plusargs");
(CONST_HYPHEN_IN_HYPHEN_LEX,"const-in-lex");
(Q_DOLLAR_TAN,"$tan");
(Q_DOLLAR_UNPACKED_UNDERSCORE_DIMENSIONS,"$unpacked_dimensions");
(Q_COLON__COLON_,"::");
(BREAK,"break");
(Q_SLASH__STAR_VERILATOR_BLANK_SC_UNDERSCORE_BV_STAR__SLASH_,"/*verilator sc_bv*/");
(Q_LESS__EQUALS_,"<=");
(MODULE,"module");
(ALWAYS_UNDERSCORE_LATCH,"always_latch");
(Q_QUOTE__LBRACE_,"'{");
(Q_VBAR__VBAR_,"||");
(INITIAL,"initial");
(AUTOMATIC,"automatic");
(Q_DOLLAR_SFORMATF,"$sformatf");
(Q_EQUALS__EQUALS_,"==");
(POSEDGE,"posedge");
(SHORTREAL,"shortreal");
(CONST_HYPHEN_THEN_HYPHEN_REF,"const-then-ref");
(INTERFACE,"interface");
(Q_DOLLAR_FGETS,"$fgets");
(Q_DOLLAR_ACOS,"$acos");
(Q_SLASH__STAR_VERILATOR_BLANK_PUBLIC_UNDERSCORE_MODULE_STAR__SLASH_,"/*verilator public_module*/");
(TIME,"time");
(OUTPUT,"output");
(DEASSIGN,"deassign");
(BUFIF1,"bufif1");
(TRI1,"tri1");
(NOT,"not");
(Q_LESS__LESS_,"<<");
(ENDCASE,"endcase");
(INT,"int");
(Q_PLUS__EQUALS_,"+=");
(Q_AMPERSAND__AMPERSAND_,"&&");
(Q_BACKQUOTE_SYSTEMC_UNDERSCORE_IMP_UNDERSCORE_HEADER_BLANK_BLOCK,"`systemc_imp_header BLOCK");
(Q_SLASH__STAR_VERILATOR_BLANK_INLINE_UNDERSCORE_MODULE_STAR__SLASH_,"/*verilator inline_module*/");
(Q_DOLLAR_WRITE,"$write");
(EDGE,"edge");
(Q_SLASH__STAR_VERILATOR_BLANK_PUBLIC_UNDERSCORE_FLAT_UNDERSCORE_RD_STAR__SLASH_,"/*verilator public_flat_rd*/");
(INOUT,"inout");
(DISABLE,"disable");
(ENDPROGRAM,"endprogram");
(INTEGER_BLANK_NUMBER,"INTEGER NUMBER");
(BIT,"bit");
(RNMOS,"rnmos");
(Q_DOLLAR_SSCANF,"$sscanf");
(Q_DOLLAR_HYPOT,"$hypot");
(Q_DOLLAR_BITSTOREAL,"$bitstoreal");
(PULLDOWN,"pulldown");
(COVERAGE_UNDERSCORE_OFF,"coverage_off");
(PACKAGE_HYPHEN_IDENTIFIER,"PACKAGE-IDENTIFIER");
(PACKAGE,"package");
(Q_DOLLAR_LOW,"$low");
(RETURN,"return");
(Q_LBRACK__HYPHEN__GREATER_,"[->");
(PACKED,"packed");
(ALWAYS,"always");
(SIGNED,"signed");
(Q_DOLLAR_FOPEN,"$fopen");
(REPEAT,"repeat");
(Q_PLUS__PLUS_,"++");
(Q_SLASH__STAR_VERILATOR_BLANK_ISOLATE_UNDERSCORE_ASSIGNMENTS_STAR__SLASH_,"/*verilator isolate_assignments*/");
(WREAL,"wreal");
(Q_DOLLAR_READMEMH,"$readmemh");
(Q_BACKQUOTE_SYSTEMC_UNDERSCORE_INTERFACE_BLANK_BLOCK,"`systemc_interface BLOCK");
(STRUCT,"struct");
(Q_PLUS__COLON_,"+:");
(Q_QUOTE_,"'");
(RCMOS,"rcmos");
      ];
    fun s -> Hashtbl.find h s

let tok arg = if !verbose then print_endline ( match arg with
  | IDENTIFIER id -> "ID \""^id^"\""
  | NUMBER n -> "NUM "^n
  | oth -> Vord.getstr oth );
  arg
}

let ident = ['a'-'z' 'A'-'Z' '$' ] ['a'-'z' 'A'-'Z' '$' '_' '0'-'9']*
let string = '"' [^ '"' ]* '"'
let compound = [':' '/' '`' '+' '-' '*' '/' '%' '&' '|' '^' '<' '>' '.' '$' '\'' '~' '_' '-' ':' '*' '/' '=' '<' '>' '{' '[' '+' '$' '!' '?' '#' '('] [':' '/' '`' '+' '-' '*' '/' '%' '|' '^' '<' '>' '.' '\'' '_' '-' ':' '*' '/' '=' '<' '>' '[' ']' '+' '?' '@' '#' ]+
let number = ['0'-'9']+
let space = [' ' '\t' '\r']+
let newline = ['\n']
let linecomment = '/' '/' [^ '\n' ]* '\n'
let vnum1 =  ['0'-'9']*['\'']['b' 'c' 'o' 'd' 'h' 'B' 'C' 'O' 'D' 'H'][' ' '\t' '\n']*['A'-'F' 'a'-'f' '0'-'9' 'x' 'X' 'z' 'Z' '_' '?']*
let vnum2 =  ['0'-'9']*['\'']['0' '1' 'x' 'X' 'z' 'Z']
let vnum3 =  ['0'-'9']['_' '0'-'9']*[' ' '\t' '\n']*['\'']['b' 'c' 'o' 'd' 'h' 'B' 'C' 'O' 'D' 'H']?[' ' '\t' '\n']*['A'-'F' 'a'-'f' '0'-'9' 'x' 'X' 'z' 'Z' '_' '?']+
let vnum4 =  ['0'-'9']['_' '0'-'9']*[' ' '\t' '\n']*['\'']['b' 'c' 'o' 'd' 'h' 'B' 'C' 'O' 'D' 'H']
let vnum5 =  ['0'-'9']['_' '0'-'9']*[' ' '\t' '\n']*['\'']
let vnum  =  number|vnum1|vnum2|vnum3|vnum4|vnum5

rule token = parse
  | space
      { token lexbuf }
  | newline
      { incr lincnt; token lexbuf }
  | '\''
      { tok ( QUOTE ) }
  | vnum as n
      { tok ( NUMBER n) }
  | ident as s
      { tok ( try keyword s with Not_found -> IDENTIFIER s ) }
  | string as s
      { tok ( STRING (String.sub s 1 (String.length s - 2))) }
  | compound as s
      { match String.sub s 0 2 with
         | "/*" -> comment (Lexing.lexeme_start lexbuf) lexbuf; token lexbuf
         | "(*" -> comment2 (Lexing.lexeme_start lexbuf) lexbuf; token lexbuf
         | "//" -> comment3 (Lexing.lexeme_start lexbuf) lexbuf; token lexbuf
         | _ -> tok ( try keyword s with Not_found -> failwith ("compound not matched: "^s )) }
      
  | eof
      { tok ( EOF_TOKEN ) }
| '!'
{ tok ( PLING ) }

| '"'
{ tok ( DOUBLEQUOTE ) }

| '#'
{ tok ( HASH ) }

| '$'
{ tok ( DOLLAR ) }

| '%'
{ tok ( PERCENT ) }

| '&'
{ tok ( AMPERSAND ) }

| '''
{ tok ( QUOTE ) }

| '('
{ tok ( LPAREN ) }

| '['
{ tok ( LBRACK ) }

| '{'
{ tok ( LBRACE ) }

| '<'
{ tok ( LESS ) }

| ')'
{ tok ( RPAREN ) }

| ']'
{ tok ( RBRACK ) }

| '}'
{ tok ( RBRACE ) }

| '>'
{ tok ( GREATER ) }

| '*'
{ tok ( STAR ) }

| '+'
{ tok ( PLUS ) }

| ','
{ tok ( COMMA ) }

| '-'
{ tok ( HYPHEN ) }

| '.'
{ tok ( DOT ) }

| '/'
{ tok ( SLASH ) }

| '\\'
{ tok ( BACKSLASH ) }

| ':'
{ tok ( COLON ) }

| ';'
{ tok ( SEMICOLON ) }

| '='
{ tok ( EQUALS ) }

| '?'
{ tok ( QUERY ) }

| '@'
{ tok ( AT ) }

| '^'
{ tok ( CARET ) }

| '_'
{ tok ( UNDERSCORE ) }

| '`'
{ tok ( BACKQUOTE ) }

| '|'
{ tok ( VBAR ) }

| '~'
{ tok ( TILDE ) }

| _ as c
      { tok ( failwith (String.make 1 c) ) }

and comment start = parse
  "/*"
    { comment (Lexing.lexeme_start lexbuf) lexbuf; comment start lexbuf }
| "*/"
    { () }
| eof
    { failwith (Printf.sprintf "Unterminated /* comment */ at offset %d." start) }
| _
    { comment start lexbuf }

and comment2 start = parse
  "(*"
    { comment2 (Lexing.lexeme_start lexbuf) lexbuf; comment2 start lexbuf }
| "*)"
    { () }
| eof
    { failwith (Printf.sprintf "Unterminated (* comment *) at offset %d." start) }
| _
    { comment2 start lexbuf }

and comment3 start = parse
  "//"
    { comment3 (Lexing.lexeme_start lexbuf) lexbuf; comment3 start lexbuf }
| '\n'
    { () }
| eof
    { failwith (Printf.sprintf "Unterminated // comment at offset %d." start) }
| _
    { comment3 start lexbuf }
