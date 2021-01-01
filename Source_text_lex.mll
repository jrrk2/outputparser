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
    DLR_LBRACE_pli_HYPHEN_system_RBRACE, "DLR;LBRACE;pli;HYPHEN;system;RBRACE";
    DLR_acos, "DLR;acos";
    DLR_acosh, "DLR;acosh";
    DLR_asin, "DLR;asin";
    DLR_asinh, "DLR;asinh";
    DLR_atan, "DLR;atan";
    DLR_atan2, "DLR;atan2";
    DLR_atanh, "DLR;atanh";
    DLR_bits, "DLR;bits";
    DLR_bitstoreal, "DLR;bitstoreal";
    DLR_bitstoshortreal, "DLR;bitstoshortreal";
    DLR_c, "DLR;c";
    DLR_cast, "DLR;cast";
    DLR_ceil, "DLR;ceil";
    DLR_changed, "DLR;changed";
    DLR_clog2, "DLR;clog2";
    DLR_cos, "DLR;cos";
    DLR_cosh, "DLR;cosh";
    DLR_countbits, "DLR;countbits";
    DLR_countones, "DLR;countones";
    DLR_dimensions, "DLR;dimensions";
    DLR_display, "DLR;display";
    DLR_displayb, "DLR;displayb";
    DLR_displayh, "DLR;displayh";
    DLR_displayo, "DLR;displayo";
    DLR_dumpall, "DLR;dumpall";
    DLR_dumpfile, "DLR;dumpfile";
    DLR_dumpflush, "DLR;dumpflush";
    DLR_dumplimit, "DLR;dumplimit";
    DLR_dumpoff, "DLR;dumpoff";
    DLR_dumpon, "DLR;dumpon";
    DLR_dumpports, "DLR;dumpports";
    DLR_dumpvars, "DLR;dumpvars";
    DLR_error, "DLR;error";
    DLR_exit, "DLR;exit";
    DLR_exp, "DLR;exp";
    DLR_fatal, "DLR;fatal";
    DLR_fclose, "DLR;fclose";
    DLR_fdisplay, "DLR;fdisplay";
    DLR_fdisplayb, "DLR;fdisplayb";
    DLR_fdisplayh, "DLR;fdisplayh";
    DLR_fdisplayo, "DLR;fdisplayo";
    DLR_fell, "DLR;fell";
    DLR_feof, "DLR;feof";
    DLR_ferror, "DLR;ferror";
    DLR_fflush, "DLR;fflush";
    DLR_fgetc, "DLR;fgetc";
    DLR_fgets, "DLR;fgets";
    DLR_finish, "DLR;finish";
    DLR_floor, "DLR;floor";
    DLR_fmonitor, "DLR;fmonitor";
    DLR_fmonitorb, "DLR;fmonitorb";
    DLR_fmonitorh, "DLR;fmonitorh";
    DLR_fmonitoro, "DLR;fmonitoro";
    DLR_fopen, "DLR;fopen";
    DLR_fread, "DLR;fread";
    DLR_frewind, "DLR;frewind";
    DLR_fscanf, "DLR;fscanf";
    DLR_fseek, "DLR;fseek";
    DLR_fstrobe, "DLR;fstrobe";
    DLR_fstrobeb, "DLR;fstrobeb";
    DLR_fstrobeh, "DLR;fstrobeh";
    DLR_fstrobeo, "DLR;fstrobeo";
    DLR_ftell, "DLR;ftell";
    DLR_fwrite, "DLR;fwrite";
    DLR_fwriteb, "DLR;fwriteb";
    DLR_fwriteh, "DLR;fwriteh";
    DLR_fwriteo, "DLR;fwriteo";
    DLR_high, "DLR;high";
    DLR_hypot, "DLR;hypot";
    DLR_increment, "DLR;increment";
    DLR_info, "DLR;info";
    DLR_isunbounded, "DLR;isunbounded";
    DLR_isunknown, "DLR;isunknown";
    DLR_itor, "DLR;itor";
    DLR_left, "DLR;left";
    DLR_ln, "DLR;ln";
    DLR_log10, "DLR;log10";
    DLR_low, "DLR;low";
    DLR_monitor, "DLR;monitor";
    DLR_monitorb, "DLR;monitorb";
    DLR_monitorh, "DLR;monitorh";
    DLR_monitoro, "DLR;monitoro";
    DLR_monitoroff, "DLR;monitoroff";
    DLR_monitoron, "DLR;monitoron";
    DLR_onehot, "DLR;onehot";
    DLR_onehot0, "DLR;onehot0";
    DLR_past, "DLR;past";
    DLR_pow, "DLR;pow";
    DLR_printtimescale, "DLR;printtimescale";
    DLR_random, "DLR;random";
    DLR_readmemb, "DLR;readmemb";
    DLR_readmemh, "DLR;readmemh";
    DLR_realtime, "DLR;realtime";
    DLR_realtobits, "DLR;realtobits";
    DLR_rewind, "DLR;rewind";
    DLR_right, "DLR;right";
    DLR_root, "DLR;root";
    DLR_rose, "DLR;rose";
    DLR_rtoi, "DLR;rtoi";
    DLR_sampled, "DLR;sampled";
    DLR_sformat, "DLR;sformat";
    DLR_sformatf, "DLR;sformatf";
    DLR_shortrealtobits, "DLR;shortrealtobits";
    DLR_signed, "DLR;signed";
    DLR_sin, "DLR;sin";
    DLR_sinh, "DLR;sinh";
    DLR_size, "DLR;size";
    DLR_sqrt, "DLR;sqrt";
    DLR_sscanf, "DLR;sscanf";
    DLR_stable, "DLR;stable";
    DLR_stime, "DLR;stime";
    DLR_stop, "DLR;stop";
    DLR_strobe, "DLR;strobe";
    DLR_strobeb, "DLR;strobeb";
    DLR_strobeh, "DLR;strobeh";
    DLR_strobeo, "DLR;strobeo";
    DLR_swrite, "DLR;swrite";
    DLR_swriteb, "DLR;swriteb";
    DLR_swriteh, "DLR;swriteh";
    DLR_swriteo, "DLR;swriteo";
    DLR_system, "DLR;system";
    DLR_tan, "DLR;tan";
    DLR_tanh, "DLR;tanh";
    DLR_test_DLR_plusargs, "DLR;test;DLR;plusargs";
    DLR_time, "DLR;time";
    DLR_timeformat, "DLR;timeformat";
    DLR_typename, "DLR;typename";
    DLR_ungetc, "DLR;ungetc";
    DLR_unit, "DLR;unit";
    DLR_unpacked_dimensions, "DLR;unpacked_dimensions";
    DLR_unsigned, "DLR;unsigned";
    DLR_urandom, "DLR;urandom";
    DLR_urandom_range, "DLR;urandom_range";
    DLR_value_DLR_plusargs, "DLR;value;DLR;plusargs";
    DLR_warning, "DLR;warning";
    DLR_write, "DLR;write";
    DLR_writeb, "DLR;writeb";
    DLR_writeh, "DLR;writeh";
    DLR_writememb, "DLR;writememb";
    DLR_writememh, "DLR;writememh";
    DLR_writeo, "DLR;writeo";
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
    IDENTIFIER_HYPHEN_COLON_COLON, "IDENTIFIER;HYPHEN;COLON;COLON";
    IDENTIFIER_HYPHEN_in_HYPHEN_lex, "IDENTIFIER;HYPHEN;in;HYPHEN;lex";
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
    STRING, "STRING";
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
    String, "string";
    Struct, "struct";
    Super, "super";
    Supply0, "supply0";
    Supply1, "supply1";
    TABLE_LINE, "TABLE;LINE";
    TILDE_AMPERSAND, "TILDE;AMPERSAND";
    TILDE_VBAR, "TILDE;VBAR";
    TIME_NUMBER, "TIME;NUMBER";
    TIMING_SPEC_ELEMENT, "TIMING;SPEC;ELEMENT";
    TYPE_HYPHEN_IDENTIFIER, "TYPE;HYPHEN;IDENTIFIER";
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

let tok arg = if verbose then print_endline (string_of_int !lincnt ^ " **" ^ (match arg with
(*
  | K_HISTORY id -> id
*)
| ACCEPT  -> "ACCEPT"
| AMPERSAND  -> "AMPERSAND"
| AMPERSAND_AMPERSAND  -> "AMPERSAND_AMPERSAND"
| AMPERSAND_AMPERSAND_AMPERSAND  -> "AMPERSAND_AMPERSAND_AMPERSAND"
| AMPERSAND_EQ  -> "AMPERSAND_EQ"
| AT  -> "AT"
| AT_AT  -> "AT_AT"
| Alias  -> "Alias"
| Always  -> "Always"
| Always_comb  -> "Always_comb"
| Always_ff  -> "Always_ff"
| Always_latch  -> "Always_latch"
| And  -> "And"
| Assert  -> "Assert"
| Assign  -> "Assign"
| Assume  -> "Assume"
| Automatic  -> "Automatic"
| BACKQUOTE  -> "BACKQUOTE"
| BACKQUOTE_nounconnecteddrive  -> "BACKQUOTE_nounconnecteddrive"
| BACKQUOTE_resetall  -> "BACKQUOTE_resetall"
| BACKQUOTE_systemc_ctor_BLOCK  -> "BACKQUOTE_systemc_ctor_BLOCK"
| BACKQUOTE_systemc_dtor_BLOCK  -> "BACKQUOTE_systemc_dtor_BLOCK"
| BACKQUOTE_systemc_header_BLOCK  -> "BACKQUOTE_systemc_header_BLOCK"
| BACKQUOTE_systemc_imp_header_BLOCK  -> "BACKQUOTE_systemc_imp_header_BLOCK"
| BACKQUOTE_systemc_implementation_BLOCK  -> "BACKQUOTE_systemc_implementation_BLOCK"
| BACKQUOTE_systemc_interface_BLOCK  -> "BACKQUOTE_systemc_interface_BLOCK"
| BACKQUOTE_unconnected_drive_pull0  -> "BACKQUOTE_unconnected_drive_pull0"
| BACKQUOTE_unconnected_drive_pull1  -> "BACKQUOTE_unconnected_drive_pull1"
| BACKSLASH  -> "BACKSLASH"
| Before  -> "Before"
| Begin  -> "Begin"
| Bind  -> "Bind"
| Bit  -> "Bit"
| Break  -> "Break"
| Buf  -> "Buf"
| Bufif0  -> "Bufif0"
| Bufif1  -> "Bufif1"
| Byte  -> "Byte"
| CARET  -> "CARET"
| CARET_EQ  -> "CARET_EQ"
| CARET_TILDE  -> "CARET_TILDE"
| COLON  -> "COLON"
| COLON_COLON  -> "COLON_COLON"
| COLON_EQ  -> "COLON_EQ"
| COLON_HYPHEN_begin  -> "COLON_HYPHEN_begin"
| COLON_HYPHEN_fork  -> "COLON_HYPHEN_fork"
| COLON_SLASH  -> "COLON_SLASH"
| COMMA  -> "COMMA"
| CONS1  _ -> "CONS1"
| CONS2  _ -> "CONS2"
| CONS3  _ -> "CONS3"
| CONS4  _ -> "CONS4"
| Case  -> "Case"
| Casex  -> "Casex"
| Casez  -> "Casez"
| Chandle  -> "Chandle"
| Class  -> "Class"
| Clock_enable  -> "Clock_enable"
| Clocker  -> "Clocker"
| Clocking  -> "Clocking"
| Cmos  -> "Cmos"
| Const  -> "Const"
| Const_HYPHEN_in_HYPHEN_lex  -> "Const_HYPHEN_in_HYPHEN_lex"
| Const_HYPHEN_then_HYPHEN_ref  -> "Const_HYPHEN_then_HYPHEN_ref"
| Constraint  -> "Constraint"
| Context  -> "Context"
| Continue  -> "Continue"
| Cover  -> "Cover"
| Coverage_block_off  -> "Coverage_block_off"
| Coverage_off  -> "Coverage_off"
| Coverage_on  -> "Coverage_on"
| DEFAULT  -> "DEFAULT"
| DLR_LBRACE_pli_HYPHEN_system_RBRACE  -> "DLR_LBRACE_pli_HYPHEN_system_RBRACE"
| DLR_acos  -> "DLR_acos"
| DLR_acosh  -> "DLR_acosh"
| DLR_asin  -> "DLR_asin"
| DLR_asinh  -> "DLR_asinh"
| DLR_atan  -> "DLR_atan"
| DLR_atan2  -> "DLR_atan2"
| DLR_atanh  -> "DLR_atanh"
| DLR_bits  -> "DLR_bits"
| DLR_bitstoreal  -> "DLR_bitstoreal"
| DLR_bitstoshortreal  -> "DLR_bitstoshortreal"
| DLR_c  -> "DLR_c"
| DLR_cast  -> "DLR_cast"
| DLR_ceil  -> "DLR_ceil"
| DLR_changed  -> "DLR_changed"
| DLR_clog2  -> "DLR_clog2"
| DLR_cos  -> "DLR_cos"
| DLR_cosh  -> "DLR_cosh"
| DLR_countbits  -> "DLR_countbits"
| DLR_countones  -> "DLR_countones"
| DLR_dimensions  -> "DLR_dimensions"
| DLR_display  -> "DLR_display"
| DLR_displayb  -> "DLR_displayb"
| DLR_displayh  -> "DLR_displayh"
| DLR_displayo  -> "DLR_displayo"
| DLR_dumpall  -> "DLR_dumpall"
| DLR_dumpfile  -> "DLR_dumpfile"
| DLR_dumpflush  -> "DLR_dumpflush"
| DLR_dumplimit  -> "DLR_dumplimit"
| DLR_dumpoff  -> "DLR_dumpoff"
| DLR_dumpon  -> "DLR_dumpon"
| DLR_dumpports  -> "DLR_dumpports"
| DLR_dumpvars  -> "DLR_dumpvars"
| DLR_error  -> "DLR_error"
| DLR_exit  -> "DLR_exit"
| DLR_exp  -> "DLR_exp"
| DLR_fatal  -> "DLR_fatal"
| DLR_fclose  -> "DLR_fclose"
| DLR_fdisplay  -> "DLR_fdisplay"
| DLR_fdisplayb  -> "DLR_fdisplayb"
| DLR_fdisplayh  -> "DLR_fdisplayh"
| DLR_fdisplayo  -> "DLR_fdisplayo"
| DLR_fell  -> "DLR_fell"
| DLR_feof  -> "DLR_feof"
| DLR_ferror  -> "DLR_ferror"
| DLR_fflush  -> "DLR_fflush"
| DLR_fgetc  -> "DLR_fgetc"
| DLR_fgets  -> "DLR_fgets"
| DLR_finish  -> "DLR_finish"
| DLR_floor  -> "DLR_floor"
| DLR_fmonitor  -> "DLR_fmonitor"
| DLR_fmonitorb  -> "DLR_fmonitorb"
| DLR_fmonitorh  -> "DLR_fmonitorh"
| DLR_fmonitoro  -> "DLR_fmonitoro"
| DLR_fopen  -> "DLR_fopen"
| DLR_fread  -> "DLR_fread"
| DLR_frewind  -> "DLR_frewind"
| DLR_fscanf  -> "DLR_fscanf"
| DLR_fseek  -> "DLR_fseek"
| DLR_fstrobe  -> "DLR_fstrobe"
| DLR_fstrobeb  -> "DLR_fstrobeb"
| DLR_fstrobeh  -> "DLR_fstrobeh"
| DLR_fstrobeo  -> "DLR_fstrobeo"
| DLR_ftell  -> "DLR_ftell"
| DLR_fwrite  -> "DLR_fwrite"
| DLR_fwriteb  -> "DLR_fwriteb"
| DLR_fwriteh  -> "DLR_fwriteh"
| DLR_fwriteo  -> "DLR_fwriteo"
| DLR_high  -> "DLR_high"
| DLR_hypot  -> "DLR_hypot"
| DLR_increment  -> "DLR_increment"
| DLR_info  -> "DLR_info"
| DLR_isunbounded  -> "DLR_isunbounded"
| DLR_isunknown  -> "DLR_isunknown"
| DLR_itor  -> "DLR_itor"
| DLR_left  -> "DLR_left"
| DLR_ln  -> "DLR_ln"
| DLR_log10  -> "DLR_log10"
| DLR_low  -> "DLR_low"
| DLR_monitor  -> "DLR_monitor"
| DLR_monitorb  -> "DLR_monitorb"
| DLR_monitorh  -> "DLR_monitorh"
| DLR_monitoro  -> "DLR_monitoro"
| DLR_monitoroff  -> "DLR_monitoroff"
| DLR_monitoron  -> "DLR_monitoron"
| DLR_onehot  -> "DLR_onehot"
| DLR_onehot0  -> "DLR_onehot0"
| DLR_past  -> "DLR_past"
| DLR_pow  -> "DLR_pow"
| DLR_printtimescale  -> "DLR_printtimescale"
| DLR_random  -> "DLR_random"
| DLR_readmemb  -> "DLR_readmemb"
| DLR_readmemh  -> "DLR_readmemh"
| DLR_realtime  -> "DLR_realtime"
| DLR_realtobits  -> "DLR_realtobits"
| DLR_rewind  -> "DLR_rewind"
| DLR_right  -> "DLR_right"
| DLR_root  -> "DLR_root"
| DLR_rose  -> "DLR_rose"
| DLR_rtoi  -> "DLR_rtoi"
| DLR_sampled  -> "DLR_sampled"
| DLR_sformat  -> "DLR_sformat"
| DLR_sformatf  -> "DLR_sformatf"
| DLR_shortrealtobits  -> "DLR_shortrealtobits"
| DLR_signed  -> "DLR_signed"
| DLR_sin  -> "DLR_sin"
| DLR_sinh  -> "DLR_sinh"
| DLR_size  -> "DLR_size"
| DLR_sqrt  -> "DLR_sqrt"
| DLR_sscanf  -> "DLR_sscanf"
| DLR_stable  -> "DLR_stable"
| DLR_stime  -> "DLR_stime"
| DLR_stop  -> "DLR_stop"
| DLR_strobe  -> "DLR_strobe"
| DLR_strobeb  -> "DLR_strobeb"
| DLR_strobeh  -> "DLR_strobeh"
| DLR_strobeo  -> "DLR_strobeo"
| DLR_swrite  -> "DLR_swrite"
| DLR_swriteb  -> "DLR_swriteb"
| DLR_swriteh  -> "DLR_swriteh"
| DLR_swriteo  -> "DLR_swriteo"
| DLR_system  -> "DLR_system"
| DLR_tan  -> "DLR_tan"
| DLR_tanh  -> "DLR_tanh"
| DLR_test_DLR_plusargs  -> "DLR_test_DLR_plusargs"
| DLR_time  -> "DLR_time"
| DLR_timeformat  -> "DLR_timeformat"
| DLR_typename  -> "DLR_typename"
| DLR_ungetc  -> "DLR_ungetc"
| DLR_unit  -> "DLR_unit"
| DLR_unpacked_dimensions  -> "DLR_unpacked_dimensions"
| DLR_unsigned  -> "DLR_unsigned"
| DLR_urandom  -> "DLR_urandom"
| DLR_urandom_range  -> "DLR_urandom_range"
| DLR_value_DLR_plusargs  -> "DLR_value_DLR_plusargs"
| DLR_warning  -> "DLR_warning"
| DLR_write  -> "DLR_write"
| DLR_writeb  -> "DLR_writeb"
| DLR_writeh  -> "DLR_writeh"
| DLR_writememb  -> "DLR_writememb"
| DLR_writememh  -> "DLR_writememh"
| DLR_writeo  -> "DLR_writeo"
| DOLLAR  -> "DOLLAR"
| DOT  -> "DOT"
| DOT_STAR  -> "DOT_STAR"
| DOUBLEQUOTE  -> "DOUBLEQUOTE"
| Deassign  -> "Deassign"
| Default  -> "Default"
| Defparam  -> "Defparam"
| Disable  -> "Disable"
| Dist  -> "Dist"
| Do  -> "Do"
| EMPTY_TOKEN  -> "EMPTY_TOKEN"
| END  -> "END"
| EOF_TOKEN  -> "EOF_TOKEN"
| EQUALS  -> "EQUALS"
| EQ_EQ  -> "EQ_EQ"
| EQ_EQ_EQ  -> "EQ_EQ_EQ"
| EQ_EQ_QUERY  -> "EQ_EQ_QUERY"
| EQ_GT  -> "EQ_GT"
| ERROR  -> "ERROR"
| ERROR_TOKEN  -> "ERROR_TOKEN"
| Edge  -> "Edge"
| Else  -> "Else"
| End  -> "End"
| Endcase  -> "Endcase"
| Endclass  -> "Endclass"
| Endclocking  -> "Endclocking"
| Endfunction  -> "Endfunction"
| Endgenerate  -> "Endgenerate"
| Endinterface  -> "Endinterface"
| Endmodule  -> "Endmodule"
| Endpackage  -> "Endpackage"
| Endprimitive  -> "Endprimitive"
| Endprogram  -> "Endprogram"
| Endproperty  -> "Endproperty"
| Endspecify  -> "Endspecify"
| Endtable  -> "Endtable"
| Endtask  -> "Endtask"
| Enum  -> "Enum"
| Event  -> "Event"
| Export  -> "Export"
| Extends  -> "Extends"
| Extern  -> "Extern"
| FLOATING_HYPHEN_POINT_NUMBER f -> "FLOAT "^string_of_float f
| Final  -> "Final"
| For  -> "For"
| Force  -> "Force"
| Foreach  -> "Foreach"
| Forever  -> "Forever"
| Fork  -> "Fork"
| Forkjoin  -> "Forkjoin"
| Full_case  -> "Full_case"
| Function  -> "Function"
| GREATER  -> "GREATER"
| GT_EQ  -> "GT_EQ"
| GT_GT  -> "GT_GT"
| GT_GT_EQ  -> "GT_GT_EQ"
| GT_GT_GT  -> "GT_GT_GT"
| GT_GT_GT_EQ  -> "GT_GT_GT_EQ"
| Generate  -> "Generate"
| Genvar  -> "Genvar"
| Global  -> "Global"
| Global_HYPHEN_in_HYPHEN_lex  -> "Global_HYPHEN_in_HYPHEN_lex"
| Global_HYPHEN_then_HYPHEN_clocking  -> "Global_HYPHEN_then_HYPHEN_clocking"
| HASH  -> "HASH"
| HASH_HASH  -> "HASH_HASH"
| HYPHEN  -> "HYPHEN"
| HYPHEN_COLON  -> "HYPHEN_COLON"
| HYPHEN_EQ  -> "HYPHEN_EQ"
| HYPHEN_GT  -> "HYPHEN_GT"
| HYPHEN_GT_GT  -> "HYPHEN_GT_GT"
| HYPHEN_HYPHEN  -> "HYPHEN_HYPHEN"
| HYPHEN_HYPHEN_block  -> "HYPHEN_HYPHEN_block"
| HYPHEN_HYPHEN_file  -> "HYPHEN_HYPHEN_file"
| HYPHEN_HYPHEN_function  -> "HYPHEN_HYPHEN_function"
| HYPHEN_HYPHEN_lines  -> "HYPHEN_HYPHEN_lines"
| HYPHEN_HYPHEN_match  -> "HYPHEN_HYPHEN_match"
| HYPHEN_HYPHEN_module  -> "HYPHEN_HYPHEN_module"
| HYPHEN_HYPHEN_msg  -> "HYPHEN_HYPHEN_msg"
| HYPHEN_HYPHEN_rule  -> "HYPHEN_HYPHEN_rule"
| HYPHEN_HYPHEN_task  -> "HYPHEN_HYPHEN_task"
| HYPHEN_HYPHEN_var  -> "HYPHEN_HYPHEN_var"
| Hier_block  -> "Hier_block"
  | IDENTIFIER id -> id
| IDENTIFIER_HYPHEN_COLON_COLON  -> "IDENTIFIER_HYPHEN_COLON_COLON"
| IDENTIFIER_HYPHEN_in_HYPHEN_lex  -> "IDENTIFIER_HYPHEN_in_HYPHEN_lex"
| INTEGER_NUMBER n -> "INTEGER "^n
| If  -> "If"
| Iff  -> "Iff"
| Implements  -> "Implements"
| Import  -> "Import"
| Initial  -> "Initial"
| Inline  -> "Inline"
| Inout  -> "Inout"
| Input  -> "Input"
| Inside  -> "Inside"
| Int  -> "Int"
| Integer  -> "Integer"
| Interface  -> "Interface"
| Isolate_assignments  -> "Isolate_assignments"
| Join  -> "Join"
| Join_any  -> "Join_any"
| Join_none  -> "Join_none"
| LBRACE  -> "LBRACE"
| LBRACK  -> "LBRACK"
| LBRACK_EQ  -> "LBRACK_EQ"
| LBRACK_HYPHEN_GT  -> "LBRACK_HYPHEN_GT"
| LBRACK_STAR  -> "LBRACK_STAR"
| LESS  -> "LESS"
| LINEFEED  -> "LINEFEED"
| LPAREN  -> "LPAREN"
| LPAREN_HYPHEN_for_HYPHEN_strength  -> "LPAREN_HYPHEN_for_HYPHEN_strength"
| LT_EQ  -> "LT_EQ"
| LT_EQ_HYPHEN_ignored  -> "LT_EQ_HYPHEN_ignored"
| LT_HYPHEN_GT  -> "LT_HYPHEN_GT"
| LT_LT  -> "LT_LT"
| LT_LT_EQ  -> "LT_LT_EQ"
| Lint_off  -> "Lint_off"
| Lint_on  -> "Lint_on"
| Local  -> "Local"
| Local_HYPHEN_in_HYPHEN_lex  -> "Local_HYPHEN_in_HYPHEN_lex"
| Local_HYPHEN_then_HYPHEN_COLON_COLON  -> "Local_HYPHEN_then_HYPHEN_COLON_COLON"
| Localparam  -> "Localparam"
| Logic  -> "Logic"
| Longint  -> "Longint"
| Modport  -> "Modport"
| Module  -> "Module"
| Nand  -> "Nand"
| Negedge  -> "Negedge"
| New  -> "New"
| New_HYPHEN_in_HYPHEN_lex  -> "New_HYPHEN_in_HYPHEN_lex"
| New_HYPHEN_then_HYPHEN_paren  -> "New_HYPHEN_then_HYPHEN_paren"
| Nmos  -> "Nmos"
| No_clocker  -> "No_clocker"
| No_inline  -> "No_inline"
| Nor  -> "Nor"
| Not  -> "Not"
| Notif0  -> "Notif0"
| Notif1  -> "Notif1"
| Null  -> "Null"
| Or  -> "Or"
| Output  -> "Output"
| PERCENT  -> "PERCENT"
| PERCENT_EQ  -> "PERCENT_EQ"
| PLING  -> "PLING"
| PLING_EQ  -> "PLING_EQ"
| PLING_EQ_EQ  -> "PLING_EQ_EQ"
| PLING_EQ_QUERY  -> "PLING_EQ_QUERY"
| PLUS  -> "PLUS"
| PLUS_COLON  -> "PLUS_COLON"
| PLUS_EQ  -> "PLUS_EQ"
| PLUS_PLUS  -> "PLUS_PLUS"
| PRLOWER_THAN_ELSE  -> "PRLOWER_THAN_ELSE"
| PRNEGATION  -> "PRNEGATION"
| PRREDUCTION  -> "PRREDUCTION"
| PRUNARYARITH  -> "PRUNARYARITH"
| Package  -> "Package"
| Packed  -> "Packed"
| Parallel_case  -> "Parallel_case"
| Parameter  -> "Parameter"
| Pmos  -> "Pmos"
| Posedge  -> "Posedge"
| Primitive  -> "Primitive"
| Priority  -> "Priority"
| Program  -> "Program"
| Property  -> "Property"
| Protected  -> "Protected"
| Public  -> "Public"
| Public_flat  -> "Public_flat"
| Public_flat_rd  -> "Public_flat_rd"
| Public_flat_rw  -> "Public_flat_rw"
| Public_module  -> "Public_module"
| Pulldown  -> "Pulldown"
| Pullup  -> "Pullup"
| Pure  -> "Pure"
| QUERY  -> "QUERY"
| QUOTE  -> "QUOTE"
| QUOTE_LBRACE  -> "QUOTE_LBRACE"
| RBRACE  -> "RBRACE"
| RBRACK  -> "RBRACK"
| RPAREN  -> "RPAREN"
| Rand  -> "Rand"
| Randc  -> "Randc"
| Randcase  -> "Randcase"
| Randomize  -> "Randomize"
| Rcmos  -> "Rcmos"
| Real  -> "Real"
| Realtime  -> "Realtime"
| Ref  -> "Ref"
| Reg  -> "Reg"
| Release  -> "Release"
| Repeat  -> "Repeat"
| Restrict  -> "Restrict"
| Return  -> "Return"
| Rnmos  -> "Rnmos"
| Rpmos  -> "Rpmos"
| Rtran  -> "Rtran"
| Rtranif0  -> "Rtranif0"
| Rtranif1  -> "Rtranif1"
| SEMICOLON  -> "SEMICOLON"
| SLASH  -> "SLASH"
| SLASH_EQ  -> "SLASH_EQ"
| SLASH_STAR_verilator_clock_enable_STAR_SLASH  -> "SLASH_STAR_verilator_clock_enable_STAR_SLASH"
| SLASH_STAR_verilator_clocker_STAR_SLASH  -> "SLASH_STAR_verilator_clocker_STAR_SLASH"
| SLASH_STAR_verilator_coverage_block_off_STAR_SLASH  -> "SLASH_STAR_verilator_coverage_block_off_STAR_SLASH"
| SLASH_STAR_verilator_full_case_STAR_SLASH  -> "SLASH_STAR_verilator_full_case_STAR_SLASH"
| SLASH_STAR_verilator_hier_block_STAR_SLASH  -> "SLASH_STAR_verilator_hier_block_STAR_SLASH"
| SLASH_STAR_verilator_inline_module_STAR_SLASH  -> "SLASH_STAR_verilator_inline_module_STAR_SLASH"
| SLASH_STAR_verilator_isolate_assignments_STAR_SLASH  -> "SLASH_STAR_verilator_isolate_assignments_STAR_SLASH"
| SLASH_STAR_verilator_no_clocker_STAR_SLASH  -> "SLASH_STAR_verilator_no_clocker_STAR_SLASH"
| SLASH_STAR_verilator_no_inline_module_STAR_SLASH  -> "SLASH_STAR_verilator_no_inline_module_STAR_SLASH"
| SLASH_STAR_verilator_no_inline_task_STAR_SLASH  -> "SLASH_STAR_verilator_no_inline_task_STAR_SLASH"
| SLASH_STAR_verilator_parallel_case_STAR_SLASH  -> "SLASH_STAR_verilator_parallel_case_STAR_SLASH"
| SLASH_STAR_verilator_public_STAR_SLASH  -> "SLASH_STAR_verilator_public_STAR_SLASH"
| SLASH_STAR_verilator_public_flat_STAR_SLASH  -> "SLASH_STAR_verilator_public_flat_STAR_SLASH"
| SLASH_STAR_verilator_public_flat_rd_STAR_SLASH  -> "SLASH_STAR_verilator_public_flat_rd_STAR_SLASH"
| SLASH_STAR_verilator_public_flat_rw_STAR_SLASH  -> "SLASH_STAR_verilator_public_flat_rw_STAR_SLASH"
| SLASH_STAR_verilator_public_module_STAR_SLASH  -> "SLASH_STAR_verilator_public_module_STAR_SLASH"
| SLASH_STAR_verilator_sc_bv_STAR_SLASH  -> "SLASH_STAR_verilator_sc_bv_STAR_SLASH"
| SLASH_STAR_verilator_sformat_STAR_SLASH  -> "SLASH_STAR_verilator_sformat_STAR_SLASH"
| SLASH_STAR_verilator_split_var_STAR_SLASH  -> "SLASH_STAR_verilator_split_var_STAR_SLASH"
| SLASH_STAR_verilator_tag_STAR_SLASH  -> "SLASH_STAR_verilator_tag_STAR_SLASH"
| SLIST  _ -> "SLIST"
| STAR  -> "STAR"
| STAR_EQ  -> "STAR_EQ"
| STAR_GT  -> "STAR_GT"
| STAR_STAR  -> "STAR_STAR"
| STRENGTH_keyword_LPAREN_strong1_SLASH_etc_RPAREN  -> "STRENGTH_keyword_LPAREN_strong1_SLASH_etc_RPAREN"
| STRING  -> "STRING"
| STRING_HYPHEN_ignored  -> "STRING_HYPHEN_ignored"
| Sc_bv  -> "Sc_bv"
| Scalared  -> "Scalared"
| Sformat  -> "Sformat"
| Shortint  -> "Shortint"
| Shortreal  -> "Shortreal"
| Signed  -> "Signed"
| Soft  -> "Soft"
| Solve  -> "Solve"
| Specify  -> "Specify"
| Specparam  -> "Specparam"
| Split_var  -> "Split_var"
| Static  -> "Static"
| Static_HYPHEN_in_HYPHEN_lex  -> "Static_HYPHEN_in_HYPHEN_lex"
| Static_HYPHEN_then_HYPHEN_constraint  -> "Static_HYPHEN_then_HYPHEN_constraint"
| String  -> "String"
| Struct  -> "Struct"
| Super  -> "Super"
| Supply0  -> "Supply0"
| Supply1  -> "Supply1"
| TABLE_LINE  -> "TABLE_LINE"
| TILDE  -> "TILDE"
| TILDE_AMPERSAND  -> "TILDE_AMPERSAND"
| TILDE_VBAR  -> "TILDE_VBAR"
| TIME_NUMBER  -> "TIME_NUMBER"
| TIMING_SPEC_ELEMENT  -> "TIMING_SPEC_ELEMENT"
| TLIST  _ -> "TLIST"
| TUPLE10  _ -> "TUPLE10"
| TUPLE11  _ -> "TUPLE11"
| TUPLE12  _ -> "TUPLE12"
| TUPLE2  _ -> "TUPLE2"
| TUPLE3  _ -> "TUPLE3"
| TUPLE4  _ -> "TUPLE4"
| TUPLE5  _ -> "TUPLE5"
| TUPLE6  _ -> "TUPLE6"
| TUPLE7  _ -> "TUPLE7"
| TUPLE8  _ -> "TUPLE8"
| TUPLE9  _ -> "TUPLE9"
| TYPE_HYPHEN_IDENTIFIER  -> "TYPE_HYPHEN_IDENTIFIER"
| Table  -> "Table"
| Task  -> "Task"
| This  -> "This"
| Time  -> "Time"
| Timeprecision  -> "Timeprecision"
| Timeunit  -> "Timeunit"
| Tracing_off  -> "Tracing_off"
| Tracing_on  -> "Tracing_on"
| Tran  -> "Tran"
| Tranif0  -> "Tranif0"
| Tranif1  -> "Tranif1"
| Tri  -> "Tri"
| Tri0  -> "Tri0"
| Tri1  -> "Tri1"
| Triand  -> "Triand"
| Trior  -> "Trior"
| Trireg  -> "Trireg"
| True  -> "True"
| Type  -> "Type"
| Typedef  -> "Typedef"
| UNDERSCORE  -> "UNDERSCORE"
| Union  -> "Union"
| Unique  -> "Unique"
| Unique0  -> "Unique0"
| Unsigned  -> "Unsigned"
| VBAR  -> "VBAR"
| VBAR_EQ  -> "VBAR_EQ"
| VBAR_EQ_GT  -> "VBAR_EQ_GT"
| VBAR_HYPHEN_GT  -> "VBAR_HYPHEN_GT"
| VBAR_VBAR  -> "VBAR_VBAR"
| Var  -> "Var"
| Vectored  -> "Vectored"
| Virtual  -> "Virtual"
| Virtual_HYPHEN_in_HYPHEN_lex  -> "Virtual_HYPHEN_in_HYPHEN_lex"
| Virtual_HYPHEN_then_HYPHEN_class  -> "Virtual_HYPHEN_then_HYPHEN_class"
| Virtual_HYPHEN_then_HYPHEN_identifier  -> "Virtual_HYPHEN_then_HYPHEN_identifier"
| Virtual_HYPHEN_then_HYPHEN_interface  -> "Virtual_HYPHEN_then_HYPHEN_interface"
| Void  -> "Void"
| Wait  -> "Wait"
| Wand  -> "Wand"
| While  -> "While"
| Wire  -> "Wire"
| With  -> "With"
| With_HYPHEN_in_HYPHEN_lex  -> "With_HYPHEN_in_HYPHEN_lex"
| With_HYPHEN_then_HYPHEN_LBRACE  -> "With_HYPHEN_then_HYPHEN_LBRACE"
| With_HYPHEN_then_HYPHEN_LBRACK  -> "With_HYPHEN_then_HYPHEN_LBRACK"
| With_HYPHEN_then_HYPHEN_LPAREN  -> "With_HYPHEN_then_HYPHEN_LPAREN"
| Wor  -> "Wor"
| Wreal  -> "Wreal"
| Xnor  -> "Xnor"
| Xor  -> "Xor") ^ "**");
  arg
}

let ident = ['a'-'z' 'A'-'Z' ] ['a'-'z' 'A'-'Z' '_' '0'-'9']*
let fltnum = ['0'-'9' '.']+['E' '-' '+' '0'-'9']*
let number = ['0'-'9']+['\'' 'b' 'd' 'h' '0'-'9' 'a'-'f' 'A'-'F']*
let dfltnum = '''['0'-'9']+
let space = [' ' '\t' '\r']+
let newline = ['\n']
let qstring = '"'[^'"']*'"'
let ampident = '&'[^' ']*
let comment = '/''/'[^'\n']*
let pattern = ['0'-'9' 'A'-'R' 'a'-'f']+'_'['0'-'9' 'A'-'R' 'a'-'f' '_']*

rule token = parse
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
| "<=" { tok ( LT_EQ ) }
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
| '`' { tok ( BACKQUOTE ) }
| '|' { tok ( VBAR ) }
| '~' { tok ( TILDE ) }

| "/*" { comment lexbuf }

  | comment
      { token lexbuf }
  | space
      { token lexbuf }
  | newline
      { incr lincnt; token lexbuf }
  | pattern as s
      { tok ( IDENTIFIER s ) }
  | number as n
      { tok ( INTEGER_NUMBER (n) ) }
  | dfltnum as n
      { tok ( INTEGER_NUMBER (n) ) }
  | fltnum as n
      { tok ( try let f = float_of_string n in FLOATING_HYPHEN_POINT_NUMBER f with _ -> IDENTIFIER n) }
  | ident as s
      { tok ( try keyword s with Not_found -> IDENTIFIER s ) }
  | ampident as s
      { tok ( try keyword s with Not_found -> IDENTIFIER s ) }
  | qstring as s
      { tok ( IDENTIFIER s ) }
  | eof
      { tok ( EOF_TOKEN ) }

| _ as oth
{ tok ( failwith ("Source_text_lex: "^String.make 1 oth) ) }

and comment = parse
| [^'*']*'*''/' as com { if verbose then print_endline ("/*"^com); token lexbuf }

