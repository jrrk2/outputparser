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
  open Source_text
  open Source_text_types

  let verbose = try int_of_string(Sys.getenv "LEXER_VERBOSE") > 0 with _ -> false
  let lincnt = ref 0

  let keyword =
    let h = Hashtbl.create 17 in
    List.iter 
      (fun (k,s) -> Hashtbl.add h s k)
      [
    AMPERSAND_AMPERSAND, "AMPERSAND;AMPERSAND";
    AMPERSAND_AMPERSAND_AMPERSAND, "AMPERSAND;AMPERSAND;AMPERSAND";
    AMPERSAND_EQ, "AMPERSAND;EQ";
    AT_AT, "AT;AT";
    Alias, "alias";
    Always, "always";
    Always_comb, "always_comb";
    Always_ff, "always_ff";
    Always_latch, "always_latch";
    And, "and";
    Assert, "assert";
    Assign, "assign";
    Assume, "assume";
    Automatic, "automatic";
    BACKQUOTE_nounconnecteddrive, "BACKQUOTE;nounconnecteddrive";
    BACKQUOTE_resetall, "BACKQUOTE;resetall";
    BACKQUOTE_systemc_ctor_BLOCK, "BACKQUOTE;systemc_ctor;BLOCK";
    BACKQUOTE_systemc_dtor_BLOCK, "BACKQUOTE;systemc_dtor;BLOCK";
    BACKQUOTE_systemc_header_BLOCK, "BACKQUOTE;systemc_header;BLOCK";
    BACKQUOTE_systemc_imp_header_BLOCK, "BACKQUOTE;systemc_imp_header;BLOCK";
    BACKQUOTE_systemc_implementation_BLOCK, "BACKQUOTE;systemc_implementation;BLOCK";
    BACKQUOTE_systemc_interface_BLOCK, "BACKQUOTE;systemc_interface;BLOCK";
    BACKQUOTE_unconnected_drive_pull0, "BACKQUOTE;unconnected_drive;pull0";
    BACKQUOTE_unconnected_drive_pull1, "BACKQUOTE;unconnected_drive;pull1";
    Before, "before";
    Begin, "begin";
    Bind, "bind";
    Bit, "bit";
    Break, "break";
    Buf, "buf";
    Bufif0, "bufif0";
    Bufif1, "bufif1";
    Byte, "byte";
    CARET_EQ, "CARET;EQ";
    CARET_TILDE, "CARET;TILDE";
    COLON_COLON, "COLON;COLON";
    COLON_EQ, "COLON;EQ";
    COLON_HYPHEN_begin, "COLON;HYPHEN;begin";
    COLON_HYPHEN_fork, "COLON;HYPHEN;fork";
    COLON_SLASH, "COLON;SLASH";
    Case, "case";
    Casex, "casex";
    Casez, "casez";
    Chandle, "chandle";
    Class, "class";
    Clock_enable, "clock_enable";
    Clocker, "clocker";
    Clocking, "clocking";
    Cmos, "cmos";
    Const, "const";
    Const_HYPHEN_in_HYPHEN_lex, "const;HYPHEN;in;HYPHEN;lex";
    Const_HYPHEN_then_HYPHEN_ref, "const;HYPHEN;then;HYPHEN;ref";
    Constraint, "constraint";
    Context, "context";
    Continue, "continue";
    Cover, "cover";
    Coverage_block_off, "coverage_block_off";
    Coverage_off, "coverage_off";
    Coverage_on, "coverage_on";
    DLR_LBRACE_pli_HYPHEN_system_RBRACE, "$LBRACE;pli;HYPHEN;system;RBRACE";
    DLR_acos, "$acos";
    DLR_acosh, "$acosh";
    DLR_asin, "$asin";
    DLR_asinh, "$asinh";
    DLR_atan, "$atan";
    DLR_atan2, "$atan2";
    DLR_atanh, "$atanh";
    DLR_bits, "$bits";
    DLR_bitstoreal, "$bitstoreal";
    DLR_bitstoshortreal, "$bitstoshortreal";
    DLR_c, "$c";
    DLR_cast, "$cast";
    DLR_ceil, "$ceil";
    DLR_changed, "$changed";
    DLR_clog2, "$clog2";
    DLR_cos, "$cos";
    DLR_cosh, "$cosh";
    DLR_countbits, "$countbits";
    DLR_countones, "$countones";
    DLR_dimensions, "$dimensions";
    DLR_display, "$display";
    DLR_displayb, "$displayb";
    DLR_displayh, "$displayh";
    DLR_displayo, "$displayo";
    DLR_dumpall, "$dumpall";
    DLR_dumpfile, "$dumpfile";
    DLR_dumpflush, "$dumpflush";
    DLR_dumplimit, "$dumplimit";
    DLR_dumpoff, "$dumpoff";
    DLR_dumpon, "$dumpon";
    DLR_dumpports, "$dumpports";
    DLR_dumpvars, "$dumpvars";
    DLR_error, "$error";
    DLR_exit, "$exit";
    DLR_exp, "$exp";
    DLR_fatal, "$fatal";
    DLR_fclose, "$fclose";
    DLR_fdisplay, "$fdisplay";
    DLR_fdisplayb, "$fdisplayb";
    DLR_fdisplayh, "$fdisplayh";
    DLR_fdisplayo, "$fdisplayo";
    DLR_fell, "$fell";
    DLR_feof, "$feof";
    DLR_ferror, "$ferror";
    DLR_fflush, "$fflush";
    DLR_fgetc, "$fgetc";
    DLR_fgets, "$fgets";
    DLR_finish, "$finish";
    DLR_floor, "$floor";
    DLR_fmonitor, "$fmonitor";
    DLR_fmonitorb, "$fmonitorb";
    DLR_fmonitorh, "$fmonitorh";
    DLR_fmonitoro, "$fmonitoro";
    DLR_fopen, "$fopen";
    DLR_fread, "$fread";
    DLR_frewind, "$frewind";
    DLR_fscanf, "$fscanf";
    DLR_fseek, "$fseek";
    DLR_fstrobe, "$fstrobe";
    DLR_fstrobeb, "$fstrobeb";
    DLR_fstrobeh, "$fstrobeh";
    DLR_fstrobeo, "$fstrobeo";
    DLR_ftell, "$ftell";
    DLR_fwrite, "$fwrite";
    DLR_fwriteb, "$fwriteb";
    DLR_fwriteh, "$fwriteh";
    DLR_fwriteo, "$fwriteo";
    DLR_high, "$high";
    DLR_hypot, "$hypot";
    DLR_increment, "$increment";
    DLR_info, "$info";
    DLR_isunbounded, "$isunbounded";
    DLR_isunknown, "$isunknown";
    DLR_itor, "$itor";
    DLR_left, "$left";
    DLR_ln, "$ln";
    DLR_log10, "$log10";
    DLR_low, "$low";
    DLR_monitor, "$monitor";
    DLR_monitorb, "$monitorb";
    DLR_monitorh, "$monitorh";
    DLR_monitoro, "$monitoro";
    DLR_monitoroff, "$monitoroff";
    DLR_monitoron, "$monitoron";
    DLR_onehot, "$onehot";
    DLR_onehot0, "$onehot0";
    DLR_past, "$past";
    DLR_pow, "$pow";
    DLR_printtimescale, "$printtimescale";
    DLR_random, "$random";
    DLR_readmemb, "$readmemb";
    DLR_readmemh, "$readmemh";
    DLR_realtime, "$realtime";
    DLR_realtobits, "$realtobits";
    DLR_rewind, "$rewind";
    DLR_right, "$right";
    DLR_root, "$root";
    DLR_rose, "$rose";
    DLR_rtoi, "$rtoi";
    DLR_sampled, "$sampled";
    DLR_sformat, "$sformat";
    DLR_sformatf, "$sformatf";
    DLR_shortrealtobits, "$shortrealtobits";
    DLR_signed, "$signed";
    DLR_sin, "$sin";
    DLR_sinh, "$sinh";
    DLR_size, "$size";
    DLR_sqrt, "$sqrt";
    DLR_sscanf, "$sscanf";
    DLR_stable, "$stable";
    DLR_stime, "$stime";
    DLR_stop, "$stop";
    DLR_strobe, "$strobe";
    DLR_strobeb, "$strobeb";
    DLR_strobeh, "$strobeh";
    DLR_strobeo, "$strobeo";
    DLR_swrite, "$swrite";
    DLR_swriteb, "$swriteb";
    DLR_swriteh, "$swriteh";
    DLR_swriteo, "$swriteo";
    DLR_system, "$system";
    DLR_tan, "$tan";
    DLR_tanh, "$tanh";
    DLR_test_DLR_plusargs, "$test;DLR;plusargs";
    DLR_time, "$time";
    DLR_timeformat, "$timeformat";
    DLR_typename, "$typename";
    DLR_ungetc, "$ungetc";
    DLR_unit, "$unit";
    DLR_unpacked_dimensions, "$unpacked_dimensions";
    DLR_unsigned, "$unsigned";
    DLR_urandom, "$urandom";
    DLR_urandom_range, "$urandom_range";
    DLR_value_DLR_plusargs, "$value;DLR;plusargs";
    DLR_warning, "$warning";
    DLR_write, "$write";
    DLR_writeb, "$writeb";
    DLR_writeh, "$writeh";
    DLR_writememb, "$writememb";
    DLR_writememh, "$writememh";
    DLR_writeo, "$writeo";
    DOT_STAR, "DOT;STAR";
    Deassign, "deassign";
    Default, "default";
    Defparam, "defparam";
    Disable, "disable";
    Dist, "dist";
    Do, "do";
    EQ_EQ, "EQ;EQ";
    EQ_EQ_EQ, "EQ;EQ;EQ";
    EQ_EQ_QUERY, "EQ;EQ;QUERY";
    EQ_GT, "EQ;GT";
    Edge, "edge";
    Else, "else";
    End, "end";
    Endcase, "endcase";
    Endclass, "endclass";
    Endclocking, "endclocking";
    Endfunction, "endfunction";
    Endgenerate, "endgenerate";
    Endinterface, "endinterface";
    Endmodule, "endmodule";
    Endpackage, "endpackage";
    Endprimitive, "endprimitive";
    Endprogram, "endprogram";
    Endproperty, "endproperty";
    Endspecify, "endspecify";
    Endtable, "endtable";
    Endtask, "endtask";
    Enum, "enum";
    Event, "event";
    Export, "export";
    Extends, "extends";
    Extern, "extern";
    Final, "final";
    For, "for";
    Force, "force";
    Foreach, "foreach";
    Forever, "forever";
    Fork, "fork";
    Forkjoin, "forkjoin";
    Full_case, "full_case";
    Function, "function";
    GT_EQ, "GT;EQ";
    GT_GT, "GT;GT";
    GT_GT_EQ, "GT;GT;EQ";
    GT_GT_GT, "GT;GT;GT";
    GT_GT_GT_EQ, "GT;GT;GT;EQ";
    Generate, "generate";
    Genvar, "genvar";
    Global, "global";
    Global_HYPHEN_in_HYPHEN_lex, "global;HYPHEN;in;HYPHEN;lex";
    Global_HYPHEN_then_HYPHEN_clocking, "global;HYPHEN;then;HYPHEN;clocking";
    HASH_HASH, "HASH;HASH";
    HYPHEN_COLON, "HYPHEN;COLON";
    HYPHEN_EQ, "HYPHEN;EQ";
    HYPHEN_GT, "HYPHEN;GT";
    HYPHEN_GT_GT, "HYPHEN;GT;GT";
    HYPHEN_HYPHEN, "HYPHEN;HYPHEN";
    HYPHEN_HYPHEN_block, "HYPHEN;HYPHEN;block";
    HYPHEN_HYPHEN_file, "HYPHEN;HYPHEN;file";
    HYPHEN_HYPHEN_function, "HYPHEN;HYPHEN;function";
    HYPHEN_HYPHEN_lines, "HYPHEN;HYPHEN;lines";
    HYPHEN_HYPHEN_match, "HYPHEN;HYPHEN;match";
    HYPHEN_HYPHEN_module, "HYPHEN;HYPHEN;module";
    HYPHEN_HYPHEN_msg, "HYPHEN;HYPHEN;msg";
    HYPHEN_HYPHEN_rule, "HYPHEN;HYPHEN;rule";
    HYPHEN_HYPHEN_task, "HYPHEN;HYPHEN;task";
    HYPHEN_HYPHEN_var, "HYPHEN;HYPHEN;var";
    Hier_block, "hier_block";
    If, "if";
    Iff, "iff";
    Implements, "implements";
    Import, "import";
    Initial, "initial";
    Inline, "inline";
    Inout, "inout";
    Input, "input";
    Inside, "inside";
    Int, "int";
    Integer, "integer";
    Interface, "interface";
    Isolate_assignments, "isolate_assignments";
    Join, "join";
    Join_any, "join_any";
    Join_none, "join_none";
    LBRACK_EQ, "LBRACK;EQ";
    LBRACK_HYPHEN_GT, "LBRACK;HYPHEN;GT";
    LBRACK_STAR, "LBRACK;STAR";
    LPAREN_HYPHEN_for_HYPHEN_strength, "LPAREN;HYPHEN;for;HYPHEN;strength";
    LT_EQ, "LT;EQ";
    LT_EQ_HYPHEN_ignored, "LT;EQ;HYPHEN;ignored";
    LT_HYPHEN_GT, "LT;HYPHEN;GT";
    LT_LT, "LT;LT";
    LT_LT_EQ, "LT;LT;EQ";
    Lint_off, "lint_off";
    Lint_on, "lint_on";
    Local, "local";
    Local_HYPHEN_in_HYPHEN_lex, "local;HYPHEN;in;HYPHEN;lex";
    Local_HYPHEN_then_HYPHEN_COLON_COLON, "local;HYPHEN;then;HYPHEN;COLON;COLON";
    Localparam, "localparam";
    Logic, "logic";
    Longint, "longint";
    Modport, "modport";
    Module, "module";
    Nand, "nand";
    Negedge, "negedge";
    New, "new";
    New_HYPHEN_in_HYPHEN_lex, "new;HYPHEN;in;HYPHEN;lex";
    New_HYPHEN_then_HYPHEN_paren, "new;HYPHEN;then;HYPHEN;paren";
    Nmos, "nmos";
    No_clocker, "no_clocker";
    No_inline, "no_inline";
    Nor, "nor";
    Not, "not";
    Notif0, "notif0";
    Notif1, "notif1";
    Null, "null";
    Or, "or";
    Output, "output";
    PERCENT_EQ, "PERCENT;EQ";
    PLING_EQ, "PLING;EQ";
    PLING_EQ_EQ, "PLING;EQ;EQ";
    PLING_EQ_QUERY, "PLING;EQ;QUERY";
    PLUS_COLON, "PLUS;COLON";
    PLUS_EQ, "PLUS;EQ";
    PLUS_PLUS, "PLUS;PLUS";
    Package, "package";
    Packed, "packed";
    Parallel_case, "parallel_case";
    Parameter, "parameter";
    Pmos, "pmos";
    Posedge, "posedge";
    Primitive, "primitive";
    Priority, "priority";
    Program, "program";
    Property, "property";
    Protected, "protected";
    Public, "public";
    Public_flat, "public_flat";
    Public_flat_rd, "public_flat_rd";
    Public_flat_rw, "public_flat_rw";
    Public_module, "public_module";
    Pulldown, "pulldown";
    Pullup, "pullup";
    Pure, "pure";
    QUOTE, "QUOTE";
    QUOTE_LBRACE, "QUOTE;LBRACE";
    Rand, "rand";
    Randc, "randc";
    Randcase, "randcase";
    Randomize, "randomize";
    Rcmos, "rcmos";
    Real, "real";
    Realtime, "realtime";
    Ref, "ref";
    Reg, "reg";
    Release, "release";
    Repeat, "repeat";
    Restrict, "restrict";
    Return, "return";
    Rnmos, "rnmos";
    Rpmos, "rpmos";
    Rtran, "rtran";
    Rtranif0, "rtranif0";
    Rtranif1, "rtranif1";
    SLASH_EQ, "SLASH;EQ";
    SLASH_STAR_verilator_clock_enable_STAR_SLASH, "SLASH;STAR;verilator;clock_enable;STAR;SLASH";
    SLASH_STAR_verilator_clocker_STAR_SLASH, "SLASH;STAR;verilator;clocker;STAR;SLASH";
    SLASH_STAR_verilator_coverage_block_off_STAR_SLASH, "SLASH;STAR;verilator;coverage_block_off;STAR;SLASH";
    SLASH_STAR_verilator_full_case_STAR_SLASH, "SLASH;STAR;verilator;full_case;STAR;SLASH";
    SLASH_STAR_verilator_hier_block_STAR_SLASH, "SLASH;STAR;verilator;hier_block;STAR;SLASH";
    SLASH_STAR_verilator_inline_module_STAR_SLASH, "SLASH;STAR;verilator;inline_module;STAR;SLASH";
    SLASH_STAR_verilator_isolate_assignments_STAR_SLASH, "SLASH;STAR;verilator;isolate_assignments;STAR;SLASH";
    SLASH_STAR_verilator_no_clocker_STAR_SLASH, "SLASH;STAR;verilator;no_clocker;STAR;SLASH";
    SLASH_STAR_verilator_no_inline_module_STAR_SLASH, "SLASH;STAR;verilator;no_inline_module;STAR;SLASH";
    SLASH_STAR_verilator_no_inline_task_STAR_SLASH, "SLASH;STAR;verilator;no_inline_task;STAR;SLASH";
    SLASH_STAR_verilator_parallel_case_STAR_SLASH, "SLASH;STAR;verilator;parallel_case;STAR;SLASH";
    SLASH_STAR_verilator_public_STAR_SLASH, "SLASH;STAR;verilator;public;STAR;SLASH";
    SLASH_STAR_verilator_public_flat_STAR_SLASH, "SLASH;STAR;verilator;public_flat;STAR;SLASH";
    SLASH_STAR_verilator_public_flat_rd_STAR_SLASH, "SLASH;STAR;verilator;public_flat_rd;STAR;SLASH";
    SLASH_STAR_verilator_public_flat_rw_STAR_SLASH, "SLASH;STAR;verilator;public_flat_rw;STAR;SLASH";
    SLASH_STAR_verilator_public_module_STAR_SLASH, "SLASH;STAR;verilator;public_module;STAR;SLASH";
    SLASH_STAR_verilator_sc_bv_STAR_SLASH, "SLASH;STAR;verilator;sc_bv;STAR;SLASH";
    SLASH_STAR_verilator_sformat_STAR_SLASH, "SLASH;STAR;verilator;sformat;STAR;SLASH";
    SLASH_STAR_verilator_split_var_STAR_SLASH, "SLASH;STAR;verilator;split_var;STAR;SLASH";
    SLASH_STAR_verilator_tag_STAR_SLASH, "SLASH;STAR;verilator;tag;STAR;SLASH";
    STAR_EQ, "STAR;EQ";
    STAR_GT, "STAR;GT";
    STAR_STAR, "STAR;STAR";
    STRENGTH_keyword_LPAREN_strong1_SLASH_etc_RPAREN, "STRENGTH;keyword;LPAREN;strong1;SLASH;etc;RPAREN";
    STRING_HYPHEN_ignored, "STRING;HYPHEN;ignored";
    Sc_bv, "sc_bv";
    Scalared, "scalared";
    Sformat, "sformat";
    Shortint, "shortint";
    Shortreal, "shortreal";
    Signed, "signed";
    Soft, "soft";
    Solve, "solve";
    Specify, "specify";
    Specparam, "specparam";
    Split_var, "split_var";
    Static, "static";
    Static_HYPHEN_in_HYPHEN_lex, "static;HYPHEN;in;HYPHEN;lex";
    Static_HYPHEN_then_HYPHEN_constraint, "static;HYPHEN;then;HYPHEN;constraint";
    Struct, "struct";
    Super, "super";
    Supply0, "supply0";
    Supply1, "supply1";
    TABLE_LINE, "TABLE;LINE";
    TILDE_AMPERSAND, "TILDE;AMPERSAND";
    TILDE_VBAR, "TILDE;VBAR";
    TIME_NUMBER, "TIME;NUMBER";
    TIMING_SPEC_ELEMENT, "TIMING;SPEC;ELEMENT";
    Table, "table";
    Task, "task";
    This, "this";
    Time, "time";
    Timeprecision, "timeprecision";
    Timeunit, "timeunit";
    Tracing_off, "tracing_off";
    Tracing_on, "tracing_on";
    Tran, "tran";
    Tranif0, "tranif0";
    Tranif1, "tranif1";
    Tri, "tri";
    Tri0, "tri0";
    Tri1, "tri1";
    Triand, "triand";
    Trior, "trior";
    Trireg, "trireg";
    True, "true";
    Type, "type";
    Typedef, "typedef";
    Union, "union";
    Unique, "unique";
    Unique0, "unique0";
    Unsigned, "unsigned";
    VBAR_EQ, "VBAR;EQ";
    VBAR_EQ_GT, "VBAR;EQ;GT";
    VBAR_HYPHEN_GT, "VBAR;HYPHEN;GT";
    VBAR_VBAR, "VBAR;VBAR";
    Var, "var";
    Vectored, "vectored";
    Virtual, "virtual";
    Virtual_HYPHEN_in_HYPHEN_lex, "virtual;HYPHEN;in;HYPHEN;lex";
    Virtual_HYPHEN_then_HYPHEN_class, "virtual;HYPHEN;then;HYPHEN;class";
    Virtual_HYPHEN_then_HYPHEN_identifier, "virtual;HYPHEN;then;HYPHEN;identifier";
    Virtual_HYPHEN_then_HYPHEN_interface, "virtual;HYPHEN;then;HYPHEN;interface";
    Void, "void";
    Wait, "wait";
    Wand, "wand";
    While, "while";
    Wire, "wire";
    With, "with";
    With_HYPHEN_in_HYPHEN_lex, "with;HYPHEN;in;HYPHEN;lex";
    With_HYPHEN_then_HYPHEN_LBRACE, "with;HYPHEN;then;HYPHEN;LBRACE";
    With_HYPHEN_then_HYPHEN_LBRACK, "with;HYPHEN;then;HYPHEN;LBRACK";
    With_HYPHEN_then_HYPHEN_LPAREN, "with;HYPHEN;then;HYPHEN;LPAREN";
    Wor, "wor";
    Wreal, "wreal";
    Xnor, "xnor";
    Xor, "xor";
      ];
    fun s -> Hashtbl.find h s

let tok' = function
| ACCEPT  -> "\"ACCEPT\""
| AMPERSAND  -> "&"
| AMPERSAND_AMPERSAND  -> "\"&&\""
| AMPERSAND_AMPERSAND_AMPERSAND  -> "\"&&&\""
| AMPERSAND_EQ  -> "\"&=\""
| AT  -> "@"
| AT_AT  -> "\"@@\""
| Alias  -> "\"Alias\""
| Always  -> "\"always\""
| Always_comb  -> "\"always_comb\""
| Always_ff  -> "\"always_ff\""
| Always_latch  -> "\"always_latch\""
| And  -> "\"And\""
| Assert  -> "\"assert\""
| Assign  -> "\"assign\""
| Assume  -> "\"assume\""
| Automatic  -> "\"automatic\""
| BACKQUOTE  -> "`"
| BACKQUOTE_nounconnecteddrive  -> "\"BACKQUOTE_nounconnecteddrive\""
| BACKQUOTE_resetall  -> "\"BACKQUOTE_resetall\""
| BACKQUOTE_systemc_ctor_BLOCK  -> "\"BACKQUOTE_systemc_ctor_BLOCK\""
| BACKQUOTE_systemc_dtor_BLOCK  -> "\"BACKQUOTE_systemc_dtor_BLOCK\""
| BACKQUOTE_systemc_header_BLOCK  -> "\"BACKQUOTE_systemc_header_BLOCK\""
| BACKQUOTE_systemc_imp_header_BLOCK  -> "\"BACKQUOTE_systemc_imp_header_BLOCK\""
| BACKQUOTE_systemc_implementation_BLOCK  -> "\"BACKQUOTE_systemc_implementation_BLOCK\""
| BACKQUOTE_systemc_interface_BLOCK  -> "\"BACKQUOTE_systemc_interface_BLOCK\""
| BACKQUOTE_unconnected_drive_pull0  -> "\"BACKQUOTE_unconnected_drive_pull0\""
| BACKQUOTE_unconnected_drive_pull1  -> "\"BACKQUOTE_unconnected_drive_pull1\""
| BACKSLASH  -> "\"BACKSLASH\""
| Before  -> "\"Before\""
| Begin  -> "\"begin\""
| Bind  -> "\"Bind\""
| Bit  -> "\"Bit\""
| Break  -> "\"Break\""
| Buf  -> "\"buf\""
| Bufif0  -> "\"bufif0\""
| Bufif1  -> "\"bufif1\""
| Byte  -> "\"byte\""
| CARET  -> "^"
| CARET_EQ  -> "^="
| CARET_TILDE  -> "\"^~\""
| COLON  -> ":"
| COLON_COLON  -> "\"::\""
| COLON_EQ  -> "\":=\""
| COLON_HYPHEN_begin  -> "\":-begin\""
| COLON_HYPHEN_fork  -> "\":-fork\""
| COLON_SLASH  -> "\":/\""
| COMMA  -> ","
| CONS1  _ -> "\"CONS1\""
| CONS2  _ -> "\"CONS2\""
| CONS3  _ -> "\"CONS3\""
| CONS4  _ -> "\"CONS4\""
| Case  -> "\"case\""
| Casex  -> "\"casex\""
| Casez  -> "\"casez\""
| Chandle  -> "\"Chandle\""
| Class  -> "\"Class\""
| Clock_enable  -> "\"Clock_enable\""
| Clocker  -> "\"Clocker\""
| Clocking  -> "\"Clocking\""
| Cmos  -> "\"Cmos\""
| Const  -> "\"Const\""
| Const_HYPHEN_in_HYPHEN_lex  -> "\"Const_HYPHEN_in_HYPHEN_lex\""
| Const_HYPHEN_then_HYPHEN_ref  -> "\"Const_HYPHEN_then_HYPHEN_ref\""
| Constraint  -> "\"Constraint\""
| Context  -> "\"Context\""
| Continue  -> "\"Continue\""
| Cover  -> "\"Cover\""
| Coverage_block_off  -> "\"Coverage_block_off\""
| Coverage_off  -> "\"Coverage_off\""
| Coverage_on  -> "\"Coverage_on\""
| DEFAULT  -> "\"DEFAULT\""
| DLR_LBRACE_pli_HYPHEN_system_RBRACE  -> "\"$LBRACE_pli_HYPHEN_system_RBRACE\""
| DLR_acos  -> "\"$acos\""
| DLR_acosh  -> "\"$acosh\""
| DLR_asin  -> "\"$asin\""
| DLR_asinh  -> "\"$asinh\""
| DLR_atan  -> "\"$atan\""
| DLR_atan2  -> "\"$atan2\""
| DLR_atanh  -> "\"$atanh\""
| DLR_bits  -> "\"$bits\""
| DLR_bitstoreal  -> "\"$bitstoreal\""
| DLR_bitstoshortreal  -> "\"$bitstoshortreal\""
| DLR_c  -> "\"$c\""
| DLR_cast  -> "\"$cast\""
| DLR_ceil  -> "\"$ceil\""
| DLR_changed  -> "\"$changed\""
| DLR_clog2  -> "\"$clog2\""
| DLR_cos  -> "\"$cos\""
| DLR_cosh  -> "\"$cosh\""
| DLR_countbits  -> "\"$countbits\""
| DLR_countones  -> "\"$countones\""
| DLR_dimensions  -> "\"$dimensions\""
| DLR_display  -> "\"$display\""
| DLR_displayb  -> "\"$displayb\""
| DLR_displayh  -> "\"$displayh\""
| DLR_displayo  -> "\"$displayo\""
| DLR_dumpall  -> "\"$dumpall\""
| DLR_dumpfile  -> "\"$dumpfile\""
| DLR_dumpflush  -> "\"$dumpflush\""
| DLR_dumplimit  -> "\"$dumplimit\""
| DLR_dumpoff  -> "\"$dumpoff\""
| DLR_dumpon  -> "\"$dumpon\""
| DLR_dumpports  -> "\"$dumpports\""
| DLR_dumpvars  -> "\"$dumpvars\""
| DLR_error  -> "\"$error\""
| DLR_exit  -> "\"$exit\""
| DLR_exp  -> "\"$exp\""
| DLR_fatal  -> "\"$fatal\""
| DLR_fclose  -> "\"$fclose\""
| DLR_fdisplay  -> "\"$fdisplay\""
| DLR_fdisplayb  -> "\"$fdisplayb\""
| DLR_fdisplayh  -> "\"$fdisplayh\""
| DLR_fdisplayo  -> "\"$fdisplayo\""
| DLR_fell  -> "\"$fell\""
| DLR_feof  -> "\"$feof\""
| DLR_ferror  -> "\"$ferror\""
| DLR_fflush  -> "\"$fflush\""
| DLR_fgetc  -> "\"$fgetc\""
| DLR_fgets  -> "\"$fgets\""
| DLR_finish  -> "\"$finish\""
| DLR_floor  -> "\"$floor\""
| DLR_fmonitor  -> "\"$fmonitor\""
| DLR_fmonitorb  -> "\"$fmonitorb\""
| DLR_fmonitorh  -> "\"$fmonitorh\""
| DLR_fmonitoro  -> "\"$fmonitoro\""
| DLR_fopen  -> "\"$fopen\""
| DLR_fread  -> "\"$fread\""
| DLR_frewind  -> "\"$frewind\""
| DLR_fscanf  -> "\"$fscanf\""
| DLR_fseek  -> "\"$fseek\""
| DLR_fstrobe  -> "\"$fstrobe\""
| DLR_fstrobeb  -> "\"$fstrobeb\""
| DLR_fstrobeh  -> "\"$fstrobeh\""
| DLR_fstrobeo  -> "\"$fstrobeo\""
| DLR_ftell  -> "\"$ftell\""
| DLR_fwrite  -> "\"$fwrite\""
| DLR_fwriteb  -> "\"$fwriteb\""
| DLR_fwriteh  -> "\"$fwriteh\""
| DLR_fwriteo  -> "\"$fwriteo\""
| DLR_high  -> "\"$high\""
| DLR_hypot  -> "\"$hypot\""
| DLR_increment  -> "\"$increment\""
| DLR_info  -> "\"$info\""
| DLR_isunbounded  -> "\"$isunbounded\""
| DLR_isunknown  -> "\"$isunknown\""
| DLR_itor  -> "\"$itor\""
| DLR_left  -> "\"$left\""
| DLR_ln  -> "\"$ln\""
| DLR_log10  -> "\"$log10\""
| DLR_low  -> "\"$low\""
| DLR_monitor  -> "\"$monitor\""
| DLR_monitorb  -> "\"$monitorb\""
| DLR_monitorh  -> "\"$monitorh\""
| DLR_monitoro  -> "\"$monitoro\""
| DLR_monitoroff  -> "\"$monitoroff\""
| DLR_monitoron  -> "\"$monitoron\""
| DLR_onehot  -> "\"$onehot\""
| DLR_onehot0  -> "\"$onehot0\""
| DLR_past  -> "\"$past\""
| DLR_pow  -> "\"$pow\""
| DLR_printtimescale  -> "\"$printtimescale\""
| DLR_random  -> "\"$random\""
| DLR_readmemb  -> "\"$readmemb\""
| DLR_readmemh  -> "\"$readmemh\""
| DLR_realtime  -> "\"$realtime\""
| DLR_realtobits  -> "\"$realtobits\""
| DLR_rewind  -> "\"$rewind\""
| DLR_right  -> "\"$right\""
| DLR_root  -> "\"$root\""
| DLR_rose  -> "\"$rose\""
| DLR_rtoi  -> "\"$rtoi\""
| DLR_sampled  -> "\"$sampled\""
| DLR_sformat  -> "\"$sformat\""
| DLR_sformatf  -> "\"$sformatf\""
| DLR_shortrealtobits  -> "\"$shortrealtobits\""
| DLR_signed  -> "\"$signed\""
| DLR_sin  -> "\"$sin\""
| DLR_sinh  -> "\"$sinh\""
| DLR_size  -> "\"$size\""
| DLR_sqrt  -> "\"$sqrt\""
| DLR_sscanf  -> "\"$sscanf\""
| DLR_stable  -> "\"$stable\""
| DLR_stime  -> "\"$stime\""
| DLR_stop  -> "\"$stop\""
| DLR_strobe  -> "\"$strobe\""
| DLR_strobeb  -> "\"$strobeb\""
| DLR_strobeh  -> "\"$strobeh\""
| DLR_strobeo  -> "\"$strobeo\""
| DLR_swrite  -> "\"$swrite\""
| DLR_swriteb  -> "\"$swriteb\""
| DLR_swriteh  -> "\"$swriteh\""
| DLR_swriteo  -> "\"$swriteo\""
| DLR_system  -> "\"$system\""
| DLR_tan  -> "\"$tan\""
| DLR_tanh  -> "\"$tanh\""
| DLR_test_DLR_plusargs  -> "\"$test$plusargs\""
| DLR_time  -> "\"$time\""
| DLR_timeformat  -> "\"$timeformat\""
| DLR_typename  -> "\"$typename\""
| DLR_ungetc  -> "\"$ungetc\""
| DLR_unit  -> "\"$unit\""
| DLR_unpacked_dimensions  -> "\"$unpacked_dimensions\""
| DLR_unsigned  -> "\"$unsigned\""
| DLR_urandom  -> "\"$urandom\""
| DLR_urandom_range  -> "\"$urandom_range\""
| DLR_value_DLR_plusargs  -> "\"$value$plusargs\""
| DLR_warning  -> "\"$warning\""
| DLR_write  -> "\"$write\""
| DLR_writeb  -> "\"$writeb\""
| DLR_writeh  -> "\"$writeh\""
| DLR_writememb  -> "\"$writememb\""
| DLR_writememh  -> "\"$writememh\""
| DLR_writeo  -> "\"$writeo\""
| DOLLAR  -> "$"
| DOT  -> "."
| DOT_STAR  -> "\".*\""
| DOUBLEQUOTE  -> "\""
| Deassign  -> "\"deassign\""
| Default  -> "\"default\""
| Defparam  -> "\"defparam\""
| Disable  -> "\"disable\""
| Dist  -> "\"Dist\""
| Do  -> "\"do\""
| EMPTY_TOKEN  -> "\"EMPTY_TOKEN\""
| END  -> "\"END\""
| ELIST  _ -> "\"ELIST\""
| EOF_TOKEN  -> ""
| EQUALS  -> "="
| EQ_EQ  -> "\"==\""
| EQ_EQ_EQ  -> "\"===\""
| EQ_EQ_QUERY  -> "\"==?\""
| EQ_GT  -> "\"=>\""
| ERROR  -> "\"ERROR\""
| ERROR_TOKEN  -> "\"ERROR_TOKEN\""
| Edge  -> "\"Edge\""
| Else  -> "\"else\""
| End  -> "\"end\""
| Endcase  -> "\"endcase\""
| Endclass  -> "\"Endclass\""
| Endclocking  -> "\"Endclocking\""
| Endfunction  -> "\"endfunction\""
| Endgenerate  -> "\"endgenerate\""
| Endinterface  -> "\"endinterface\""
| Endmodule  -> "\"endmodule\""
| Endpackage  -> "\"endpackage\""
| Endprimitive  -> "\"endprimitive\""
| Endprogram  -> "\"endprogram\""
| Endproperty  -> "\"Endproperty\""
| Endspecify  -> "\"Endspecify\""
| Endtable  -> "\"Endtable\""
| Endtask  -> "\"endtask\""
| Enum  -> "\"enum\""
| Event  -> "\"event\""
| Export  -> "\"export\""
| Extends  -> "\"Extends\""
| Extern  -> "\"Extern\""
| FLOATING_HYPHEN_POINT_NUMBER f -> "\"FLOAT \""^string_of_float f
| Final  -> "\"Final\""
| For  -> "\"for\""
| Force  -> "\"force\""
| Foreach  -> "\"foreach\""
| Forever  -> "\"forever\""
| Fork  -> "\"fork\""
| Forkjoin  -> "\"forkjoin\""
| Full_case  -> "\"Full_case\""
| Function  -> "\"function\""
| GREATER  -> ">"
| GT_EQ  -> "\">=\""
| GT_GT  -> "\">>\""
| GT_GT_EQ  -> "\">>=\""
| GT_GT_GT  -> "\">>>\""
| GT_GT_GT_EQ  -> "\">>>=\""
| Generate  -> "\"generate\""
| Genvar  -> "\"genvar\""
| Global  -> "\"Global\""
| Global_HYPHEN_in_HYPHEN_lex  -> "\"Global_HYPHEN_in_HYPHEN_lex\""
| Global_HYPHEN_then_HYPHEN_clocking  -> "\"Global_HYPHEN_then_HYPHEN_clocking\""
| HASH  -> "#"
| HASH_HASH  -> "\"##\""
| HYPHEN  -> "-"
| HYPHEN_COLON  -> "\"-:\""
| HYPHEN_EQ  -> "\"->\""
| HYPHEN_GT  -> "\"->\""
| HYPHEN_GT_GT  -> "\"->>\""
| HYPHEN_HYPHEN  -> "\"--\""
| HYPHEN_HYPHEN_block  -> "\"--block\""
| HYPHEN_HYPHEN_file  -> "\"--file\""
| HYPHEN_HYPHEN_function  -> "\"--function\""
| HYPHEN_HYPHEN_lines  -> "\"--lines\""
| HYPHEN_HYPHEN_match  -> "\"--match\""
| HYPHEN_HYPHEN_module  -> "\"--module\""
| HYPHEN_HYPHEN_msg  -> "\"--msg\""
| HYPHEN_HYPHEN_rule  -> "\"--rule\""
| HYPHEN_HYPHEN_task  -> "\"--task\""
| HYPHEN_HYPHEN_var  -> "\"--var\""
| Hier_block  -> "\"Hier_block\""
| IDENTIFIER id -> "\"IDENTIFIER\" strp='"^id^"'"
| IDENTIFIER_HYPHEN_COLON_COLON id -> "\"IDENTIFIER-::\" strp='"^id^"'"
| IDENTIFIER_HYPHEN_in_HYPHEN_lex  -> "\"IDENTIFIER-in-lex\""
| INTEGER_NUMBER _ -> "\"INTEGER NUMBER\""
| If  -> "\"if\""
| Iff  -> "\"if\""
| Implements  -> "\"implements\""
| Import  -> "\"import\""
| Initial  -> "\"initial\""
| Inline  -> "\"inline\""
| Inout  -> "\"inout\""
| Input  -> "\"input\""
| Inside  -> "\"inside\""
| Int  -> "\"int\""
| Integer  -> "\"integer\""
| Interface  -> "\"interface\""
| Isolate_assignments  -> "\"Isolate_assignments\""
| Join  -> "\"Join\""
| Join_any  -> "\"Join_any\""
| Join_none  -> "\"Join_none\""
| LBRACE  -> "{"
| LBRACK  -> "["
| LBRACK_EQ  -> "\"[=\""
| LBRACK_HYPHEN_GT  -> "[->"
| LBRACK_STAR  -> "\"[*\""
| LESS  -> "<"
| LINEFEED  -> "\n"
| LPAREN  -> "("
| LPAREN_HYPHEN_for_HYPHEN_strength  -> "\"LPAREN_HYPHEN_for_HYPHEN_strength\""
| LT_EQ  -> "\"<=\""
| LT_EQ_HYPHEN_ignored  -> "\"LT_EQ_HYPHEN_ignored\""
| LT_HYPHEN_GT  -> "\"<->\""
| LT_LT  -> "\"<<\""
| LT_LT_EQ  -> "\"<<=\""
| Lint_off  -> "\"Lint_off\""
| Lint_on  -> "\"Lint_on\""
| Local  -> "\"Local\""
| Local_HYPHEN_in_HYPHEN_lex  -> "\"Local_HYPHEN_in_HYPHEN_lex\""
| Local_HYPHEN_then_HYPHEN_COLON_COLON  -> "\"Local_HYPHEN_then_HYPHEN_COLON_COLON\""
| Localparam  -> "\"localparam\""
| Logic  -> "\"logic\""
| Longint  -> "\"longint\""
| Modport  -> "\"modport\""
| Module  -> "\"module\""
| Nand  -> "\"nand\""
| Negedge  -> "\"negedge\""
| New  -> "\"New\""
| New_HYPHEN_in_HYPHEN_lex  -> "\"New_HYPHEN_in_HYPHEN_lex\""
| New_HYPHEN_then_HYPHEN_paren  -> "\"New_HYPHEN_then_HYPHEN_paren\""
| Nmos  -> "\"nmos\""
| No_clocker  -> "\"No_clocker\""
| No_inline  -> "\"No_inline\""
| Nor  -> "\"nor\""
| Not  -> "\"not\""
| Notif0  -> "\"notif0\""
| Notif1  -> "\"notif1\""
| Null  -> "\"Null\""
| Or  -> "\"or\""
| Output  -> "\"output\""
| PERCENT  -> "%"
| PERCENT_EQ  -> "%="
| PLING  -> "!"
| PLING_EQ  -> "\"!=\""
| PLING_EQ_EQ  -> "\"!==\""
| PLING_EQ_QUERY  -> "\"!=?\""
| PLUS  -> "+"
| PLUS_COLON  -> "\"+:\""
| PLUS_EQ  -> "\"+=\""
| PLUS_PLUS  -> "\"++\""
| PRLOWER_THAN_ELSE  -> "\"PRLOWER_THAN_ELSE\""
| PRNEGATION  -> "\"PRNEGATION\""
| PRREDUCTION  -> "\"PRREDUCTION\""
| PRUNARYARITH  -> "\"PRUNARYARITH\""
| Package  -> "\"package\""
| Packed  -> "\"packed\""
| Parallel_case  -> "\"Parallel_case\""
| Parameter  -> "\"parameter\""
| Pmos  -> "\"pmos\""
| Posedge  -> "\"posedge\""
| Primitive  -> "\"primitive\""
| Priority  -> "\"Priority\""
| Program  -> "\"Program\""
| Property  -> "\"Property\""
| Protected  -> "\"Protected\""
| Public  -> "\"Public\""
| Public_flat  -> "\"Public_flat\""
| Public_flat_rd  -> "\"Public_flat_rd\""
| Public_flat_rw  -> "\"Public_flat_rw\""
| Public_module  -> "\"Public_module\""
| Pulldown  -> "\"pulldown\""
| Pullup  -> "\"pullup\""
| Pure  -> "\"Pure\""
| QUERY  -> "?"
| QUOTE  -> "\"'\""
| QUOTE_LBRACE  -> "\"'{\""
| RBRACE  -> "}"
| RBRACK  -> "]"
| RPAREN  -> ")"
| Rand  -> "\"Rand\""
| Randc  -> "\"Randc\""
| Randcase  -> "\"Randcase\""
| Randomize  -> "\"Randomize\""
| Rcmos  -> "\"rcmos\""
| Real  -> "\"real\""
| Realtime  -> "\"realtime\""
| Ref  -> "\"Ref\""
| Reg  -> "\"reg\""
| Release  -> "\"Release\""
| Repeat  -> "\"Repeat\""
| Restrict  -> "\"Restrict\""
| Return  -> "\"return\""
| Rnmos  -> "\"rnmos\""
| Rpmos  -> "\"rpmos\""
| Rtran  -> "\"rtran\""
| Rtranif0  -> "\"rtranif0\""
| Rtranif1  -> "\"rtranif1\""
| SEMICOLON  -> ";"
| SLASH  -> "/"
| SLASH_EQ  -> "/="
| SLASH_STAR_verilator_clock_enable_STAR_SLASH  -> "\"SLASH_STAR_verilator_clock_enable_STAR_SLASH\""
| SLASH_STAR_verilator_clocker_STAR_SLASH  -> "\"SLASH_STAR_verilator_clocker_STAR_SLASH\""
| SLASH_STAR_verilator_coverage_block_off_STAR_SLASH  -> "\"SLASH_STAR_verilator_coverage_block_off_STAR_SLASH\""
| SLASH_STAR_verilator_full_case_STAR_SLASH  -> "\"SLASH_STAR_verilator_full_case_STAR_SLASH\""
| SLASH_STAR_verilator_hier_block_STAR_SLASH  -> "\"SLASH_STAR_verilator_hier_block_STAR_SLASH\""
| SLASH_STAR_verilator_inline_module_STAR_SLASH  -> "\"SLASH_STAR_verilator_inline_module_STAR_SLASH\""
| SLASH_STAR_verilator_isolate_assignments_STAR_SLASH  -> "\"SLASH_STAR_verilator_isolate_assignments_STAR_SLASH\""
| SLASH_STAR_verilator_no_clocker_STAR_SLASH  -> "\"SLASH_STAR_verilator_no_clocker_STAR_SLASH\""
| SLASH_STAR_verilator_no_inline_module_STAR_SLASH  -> "\"SLASH_STAR_verilator_no_inline_module_STAR_SLASH\""
| SLASH_STAR_verilator_no_inline_task_STAR_SLASH  -> "\"SLASH_STAR_verilator_no_inline_task_STAR_SLASH\""
| SLASH_STAR_verilator_parallel_case_STAR_SLASH  -> "\"SLASH_STAR_verilator_parallel_case_STAR_SLASH\""
| SLASH_STAR_verilator_public_STAR_SLASH  -> "\"SLASH_STAR_verilator_public_STAR_SLASH\""
| SLASH_STAR_verilator_public_flat_STAR_SLASH  -> "\"SLASH_STAR_verilator_public_flat_STAR_SLASH\""
| SLASH_STAR_verilator_public_flat_rd_STAR_SLASH  -> "\"SLASH_STAR_verilator_public_flat_rd_STAR_SLASH\""
| SLASH_STAR_verilator_public_flat_rw_STAR_SLASH  -> "\"SLASH_STAR_verilator_public_flat_rw_STAR_SLASH\""
| SLASH_STAR_verilator_public_module_STAR_SLASH  -> "\"SLASH_STAR_verilator_public_module_STAR_SLASH\""
| SLASH_STAR_verilator_sc_bv_STAR_SLASH  -> "\"SLASH_STAR_verilator_sc_bv_STAR_SLASH\""
| SLASH_STAR_verilator_sformat_STAR_SLASH  -> "\"SLASH_STAR_verilator_sformat_STAR_SLASH\""
| SLASH_STAR_verilator_split_var_STAR_SLASH  -> "\"SLASH_STAR_verilator_split_var_STAR_SLASH\""
| SLASH_STAR_verilator_tag_STAR_SLASH  -> "\"SLASH_STAR_verilator_tag_STAR_SLASH\""
| SLIST  _ -> "\"SLIST\""
| STAR  -> "*"
| STAR_EQ  -> "\"*=\""
| STAR_GT  -> "\"*>\""
| STAR_STAR  -> "\"**\""
| STRENGTH_keyword_LPAREN_strong1_SLASH_etc_RPAREN  -> "\"STRENGTH_keyword_LPAREN_strong1_SLASH_etc_RPAREN\""
| STRING s -> "\"STRING\""
| STRING_HYPHEN_ignored  -> "\"STRING_HYPHEN_ignored\""
| Sc_bv  -> "\"Sc_bv\""
| Scalared  -> "\"scalared\""
| Sformat  -> "\"sformat\""
| Shortint  -> "\"shortint\""
| Shortreal  -> "\"shortreal\""
| Signed  -> "\"signed\""
| Soft  -> "\"Soft\""
| Solve  -> "\"Solve\""
| Specify  -> "\"specify\""
| Specparam  -> "\"Specparam\""
| Split_var  -> "\"Split_var\""
| Static  -> "\"Static\""
| Static_HYPHEN_in_HYPHEN_lex  -> "\"Static_HYPHEN_in_HYPHEN_lex\""
| Static_HYPHEN_then_HYPHEN_constraint  -> "\"Static_HYPHEN_then_HYPHEN_constraint\""
| Struct  -> "\"struct\""
| String  -> "\"string\""
| Super  -> "\"Super\""
| Supply0  -> "\"supply0\""
| Supply1  -> "\"supply1\""
| TABLE_LINE  -> "\"TABLE_LINE\""
| TILDE  -> "~"
| TILDE_AMPERSAND  -> "\"~&\""
| TILDE_VBAR  -> "\"~|\""
| TIME_NUMBER  -> "\"TIME_NUMBER\""
| TIMING_SPEC_ELEMENT  -> "\"TIMING_SPEC_ELEMENT\""
| TLIST  _ -> "\"TLIST\""
| TUPLE10  _ -> "\"TUPLE10\""
| TUPLE11  _ -> "\"TUPLE11\""
| TUPLE12  _ -> "\"TUPLE12\""
| TUPLE2  _ -> "\"TUPLE2\""
| TUPLE3  _ -> "\"TUPLE3\""
| TUPLE4  _ -> "\"TUPLE4\""
| TUPLE5  _ -> "\"TUPLE5\""
| TUPLE6  _ -> "\"TUPLE6\""
| TUPLE7  _ -> "\"TUPLE7\""
| TUPLE8  _ -> "\"TUPLE8\""
| TUPLE9  _ -> "\"TUPLE9\""
| TYPE_HYPHEN_IDENTIFIER id -> "\"TYPE-IDENTIFIER\" strp='"^id^"'"
| Table  -> "\"Table\""
| Task  -> "\"Task\""
| This  -> "\"This\""
| Time  -> "\"Time\""
| Timeprecision  -> "\"Timeprecision\""
| Timeunit  -> "\"Timeunit\""
| Tracing_off  -> "\"Tracing_off\""
| Tracing_on  -> "\"Tracing_on\""
| Tran  -> "\"Tran\""
| Tranif0  -> "\"Tranif0\""
| Tranif1  -> "\"Tranif1\""
| Tri  -> "\"Tri\""
| Tri0  -> "\"Tri0\""
| Tri1  -> "\"Tri1\""
| Triand  -> "\"Triand\""
| Trior  -> "\"Trior\""
| Trireg  -> "\"Trireg\""
| True  -> "\"True\""
| Type  -> "\"type\""
| Typedef  -> "\"typedef\""
| UNDERSCORE  -> "_"
| Union  -> "\"union\""
| Unique  -> "\"unique\""
| Unique0  -> "\"unique0\""
| Unsigned  -> "\"unsigned\""
| VBAR  -> "|"
| VBAR_EQ  -> "\"|=\""
| VBAR_EQ_GT  -> "\"|=>\""
| VBAR_HYPHEN_GT  -> "\"|->\""
| VBAR_VBAR  -> "\"||\""
| Var  -> "\"var\""
| Vectored  -> "\"Vectored\""
| Virtual  -> "\"Virtual\""
| Virtual_HYPHEN_in_HYPHEN_lex  -> "\"Virtual_HYPHEN_in_HYPHEN_lex\""
| Virtual_HYPHEN_then_HYPHEN_class  -> "\"Virtual_HYPHEN_then_HYPHEN_class\""
| Virtual_HYPHEN_then_HYPHEN_identifier  -> "\"Virtual_HYPHEN_then_HYPHEN_identifier\""
| Virtual_HYPHEN_then_HYPHEN_interface  -> "\"Virtual_HYPHEN_then_HYPHEN_interface\""
| Void  -> "\"void\""
| Wait  -> "\"Wait\""
| Wand  -> "\"wand\""
| While  -> "\"while\""
| Wire  -> "\"wire\""
| With  -> "\"With\""
| With_HYPHEN_in_HYPHEN_lex  -> "\"With_HYPHEN_in_HYPHEN_lex\""
| With_HYPHEN_then_HYPHEN_LBRACE  -> "\"With_HYPHEN_then_HYPHEN_LBRACE\""
| With_HYPHEN_then_HYPHEN_LBRACK  -> "\"With_HYPHEN_then_HYPHEN_LBRACK\""
| With_HYPHEN_then_HYPHEN_LPAREN  -> "\"With_HYPHEN_then_HYPHEN_LPAREN\""
| Wor  -> "\"Wor\""
| Wreal  -> "\"Wreal\""
| Xnor  -> "\"Xnor\""
| Xor  -> "\"Xor\""

let ord = function
| IDENTIFIER _ -> 259 
| INTEGER_NUMBER _ -> 263 
| TYPE_HYPHEN_IDENTIFIER _ -> 262
| IDENTIFIER_HYPHEN_COLON_COLON _ -> 260
| Endmodule -> 363 
| Input -> 395 
| Localparam -> 403 
| Logic -> 407 
| Module -> 410 
| Output -> 423
| Reg -> 445
| Wire -> 504
| Always -> 316
| Begin -> 326
| If -> 389
| For -> 377
| Posedge -> 428
| LT_EQ -> 689
| PLUS_PLUS -> 717
| Else -> 355   
| End -> 356
| DOT_STAR -> 707
| Package -> 424
| Endpackage -> 364
| Typedef -> 488
| Struct -> 467
| Packed -> 425
| Enum -> 371
| Import -> 392
| COLON_COLON -> 709
| EQ_EQ -> 682
| Assign -> 322
| DLR_clog2 -> 528
| Int -> 397
| QUOTE_LBRACE -> 676
| Default -> 349
| STAR_STAR -> 694
| Genvar -> 385
| Always_comb -> 317
| VBAR_EQ -> 725
| Case -> 334
| Casez -> 336
| Endcase -> 357
| Parameter -> 426
| PLUS_COLON -> 699
| PLUS_EQ -> 719
| PLING_EQ -> 683
| LT_LT -> 691
| GT_GT -> 692
| GT_EQ -> 688
| COLON_HYPHEN_begin -> 695
| Function -> 383
| Endfunction -> 360
| QUOTE -> 675
| DLR_bits -> 521
| TILDE_VBAR -> 679
| TILDE_AMPERSAND -> 681
| HYPHEN_HYPHEN -> 718
| Automatic -> 324
| Return -> 449
| CARET_TILDE -> 680
| Always_ff -> 318
| Or -> 422
| Negedge -> 412
| VBAR_VBAR -> 677
| DLR_error -> 546
| AMPERSAND_EQ -> 724
| STRING _ -> 265
| Signed -> 458
| GT_GT_GT -> 693
| EOF_TOKEN -> 0
| oth -> let tok'' = tok' oth in Char.code (tok''.[0])

let import_seen = ref false

let tok arg = if verbose then Printf.printf "tokenToBison  TOKEN {c%d-%d:}=%d %s\n" (!lincnt+1) (!lincnt+1) (ord arg) (tok' arg);
  import_seen := (match arg with Import -> true | IDENTIFIER_HYPHEN_COLON_COLON _ -> false | _ -> !import_seen);
  arg
}

let ident = ['a'-'z' 'A'-'Z' '$' '_'] ['a'-'z' 'A'-'Z' '_' '0'-'9' '$']*
let escaped = '\\'[^' ']*' '
let fltnum = ['0'-'9']+'.'['E' '-' '+' '0'-'9']*
let sizednumber = ['0'-'9']+'\''['b' 'd' 'h'][ '0'-'9' 'a'-'f' 'x' 'A'-'F' 'X' '_' ' ']+
let number = ['0'-'9']['0'-'9' '_']*
let dfltnum = '''['0'-'9' 'b' 'h' 'x']['0'-'9' 'a'-'f' 'A'-'F' 'x']*
let space = [' ' '\t' '\r']+
let newline = ['\n']
let qstring = '"'[^'"']*'"'
let comment = '/''/'[^'\n']*
let definition = '`'['a'-'z' 'A'-'Z' '$'] ['a'-'z' 'A'-'Z' '_' '0'-'9' '$']*
let define = "`define"[^'\n']*
let include = "`include"[^'\n']*
let colon_begin = ':'[' ']*"begin"
   
rule token = parse
(*
| colon_begin { tok ( COLON_HYPHEN_begin ) }
*)
| ">>>" { tok ( GT_GT_GT ) }
| "<<=" { tok ( LT_LT_EQ ) }
| "||" { tok ( VBAR_VBAR ) }
| "~|" { tok ( TILDE_VBAR ) }
| "^~" { tok ( CARET_TILDE ) }
| "~&" { tok ( TILDE_AMPERSAND ) }
| "==" { tok ( EQ_EQ ) }
| "+=" { tok ( PLUS_EQ ) }
| "<=" { tok ( LT_EQ ) }
| "!=" { tok ( PLING_EQ ) }
| "|=" { tok ( VBAR_EQ ) }
| "<<" { tok ( LT_LT ) }
| ">>" { tok ( GT_GT ) }
| ">=" { tok ( GT_EQ ) }
| ".*" { tok ( DOT_STAR ) }
| "+:" { tok ( PLUS_COLON ) }
| "::" { tok ( COLON_COLON ) }
| "++" { tok ( PLUS_PLUS ) }
| "--" { tok ( HYPHEN_HYPHEN ) }
| "**" { tok ( STAR_STAR ) }
| "&&" { tok ( AMPERSAND_AMPERSAND ) }
| "&=" { tok ( AMPERSAND_EQ ) }
| "'{" { tok ( QUOTE_LBRACE ) }
| '-' { tok ( HYPHEN ) }
| '+' { tok ( PLUS ) }
| '!' { tok ( PLING ) }
| '"' { tok ( DOUBLEQUOTE ) }
| '#' { tok ( HASH ) }
| '$' { tok ( DOLLAR ) }
| '%' { tok ( PERCENT ) }
| '&' { tok ( AMPERSAND ) }
| ''' { tok ( QUOTE ) }
| '(' { tok ( LPAREN ) }
| '[' { tok ( LBRACK ) }
| '{' { tok ( LBRACE ) }
| '<' { tok ( LESS ) }
| ')' { tok ( RPAREN ) }
| ']' { tok ( RBRACK ) }
| '}' { tok ( RBRACE ) }
| '>' { tok ( GREATER ) }
| '*' { tok ( STAR ) }
| ',' { tok ( COMMA ) }
| '.' { tok ( DOT ) }
| '/' { tok ( SLASH ) }
| '\\' { tok ( BACKSLASH ) }
| ':' { tok ( COLON ) }
| ';' { tok ( SEMICOLON ) }
| '=' { tok ( EQUALS ) }
| '?' { tok ( QUERY ) }
| '@' { tok ( AT ) }
| '^' { tok ( CARET ) }
| '_' { tok ( UNDERSCORE ) }
| '|' { tok ( VBAR ) }
| '~' { tok ( TILDE ) }

| define { token lexbuf }
| include { token lexbuf }
| "`ifdef" { ifdef lexbuf }
| "`ifndef" { ifndef lexbuf }
| definition as s { tok (IDENTIFIER s) }
| "/*" { comment lexbuf }
| "(* " { comment lexbuf }

  | comment
      { token lexbuf }
  | space
      { token lexbuf }
  | newline
      { incr lincnt; token lexbuf }
  | sizednumber as n
      { tok ( INTEGER_NUMBER (n) ) }
  | number as n
      { tok ( INTEGER_NUMBER (n) ) }
  | dfltnum as n
      { tok ( INTEGER_NUMBER (n) ) }
  | fltnum as n
      { tok ( try let f = float_of_string n in FLOATING_HYPHEN_POINT_NUMBER f with _ -> IDENTIFIER n) }
  | ident as s
      { tok ( try keyword s with Not_found -> if Hashtbl.mem typehash s then TYPE_HYPHEN_IDENTIFIER s else if !import_seen then IDENTIFIER_HYPHEN_COLON_COLON s else IDENTIFIER s ) }
  | escaped as s
      { let s = String.sub s 1 (String.length s - 1) in
        tok ( if Hashtbl.mem typehash s then TYPE_HYPHEN_IDENTIFIER s else if !import_seen then IDENTIFIER_HYPHEN_COLON_COLON s else IDENTIFIER s ) }
  | qstring as s
      { tok ( STRING s ) }
  | eof
      { tok ( EOF_TOKEN ) }

| _ as oth
{ tok ( failwith ("Source_text_lex: "^String.make 1 oth) ) }

and comment = parse
| newline { incr lincnt; comment lexbuf }
| '*''/' as com { if false then print_endline ("/*"^com); token lexbuf }
| '*'')' as com { if false then print_endline ("/*"^com); token lexbuf }
| _ { comment lexbuf }

(* pre-processing should have removed this stuff, but if not, skip over it *)
   
and ifdef = parse
| "`endif" { token lexbuf }
| newline { incr lincnt; ifdef lexbuf }
| _ { ifdef lexbuf }

and ifndef = parse
| "`endif" { token lexbuf }
| newline { incr lincnt; ifndef lexbuf }
| _ { ifndef lexbuf }
