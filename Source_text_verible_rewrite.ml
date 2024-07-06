open Source_text_verible_rewrite_types
open Source_text_verible_lex
open Source_text_verible

let othcons = ref End_of_file

let verbose = try int_of_string (Sys.getenv ("DESCEND_VERBOSE")) > 0 with _ -> false

let trim s = if s.[0] = '\\' then String.sub s 1 (String.length s - 1) else s

let notsupp kind lst = failwith ("Not supported: "^kind)

let getstr = function
  | Add _ -> "Add"
  | AlwaysComb _ -> "AlwaysComb"
  | AlwaysComb2 _ -> "AlwaysComb2"
  | AlwaysFF _ -> "AlwaysFF"
  | AlwaysLatch _ -> "AlwaysLatch"
  | AlwaysLegacy _ -> "AlwaysLegacy"
  | AlwaysSync -> "AlwaysSync"
  | And _ -> "And"
  | And2 _ -> "And2"
  | And3 _ -> "And3"
  | AnyRange _ -> "AnyRange"
  | Asgn1 _ -> "Asgn1"
  | AsgnPat _ -> "AsgnPat"
  | Assert -> "Assert"
  | AssertProperty -> "AssertProperty"
  | At _ -> "At"
  | AtStar
  | Atom _ -> "Atom"
  | AutoFunDecl _ -> "AutoFunDecl"
  | BeginBlock _ -> "BeginBlock"
  | Bitlst _ -> "Bitlst"
  | BlockItem _ -> "BlockItem"
  | BreakSemi -> "BreakSemi"
  | CaretTilde _ -> "CaretTilde"
  | CaseItm _ -> "CaseItm"
  | CaseStart _ -> "CaseStart"
  | CaseStart1 _ -> "CaseStart1"
  | CaseStartInside _ -> "CaseStartInside"
  | CaseStartUniq _ -> "CaseStartUniq"
  | CaseStartUniqInside _ -> "CaseStartUniqInside"
  | CaseStmt _ -> "CaseStmt"
  | Cast _ -> "Cast"
  | CellParamItem2 _ -> "CellParamItem2"
  | CellParamItem3 _ -> "CellParamItem3"
  | CellPinItem2 _ -> "CellPinItem2"
  | CellPinItemImplied _ -> "CellPinItemImplied"
  | CellPinItemNC _ -> "CellPinItemNC"
  | Concat _ -> "Concat"
  | CondGen1 _ -> "CondGen1"
  | ContAsgn _ -> "ContAsgn"
  | DeclAsgn _ -> "DeclAsgn"
  | DeclData _ -> "DeclData"
  | DeclInt2 _ -> "DeclInt2"
  | DeclLogic _ -> "DeclLogic"
  | DeclLogic2 _ -> "DeclLogic2"
  | DeclModPort _ -> "DeclModPort"
  | DeclReg _ -> "DeclReg"
  | Deflt
  | Div _ -> "Div"
  | Dot1 _ -> "Dot1"
  | Dot3 _ -> "Dot3"
  | DotBus _ -> "DotBus"
  | Edge _ -> "Edge"
  | ElabTask _ -> "ElabTask"
  | ElseStmt _ -> "ElseStmt"
  | EnumInit _ -> "EnumInit"
  | Equals _ -> "Equals"
  | Equals3 _ -> "Equals3"
  | EqualsQuery _ -> "EqualsQuery"
  | Equate _ -> "Equate"
  | EquateArrayField _ -> "EquateArrayField"
  | EquateConcat _ -> "EquateConcat"
  | EquateField _ -> "EquateField"
  | EquateSelect _ -> "EquateSelect"
  | EquateSelect2 _ -> "EquateSelect2"
  | EquateSlice _ -> "EquateSlice"
  | EquateSlicePlus _ -> "EquateSlicePlus"
  | EventOr _ -> "EventOr"
  | ExprOKL _ -> "ExprOKL"
  | ExprQuote1 _ -> "ExprQuote1"
  | Expression _ -> "Expression"
  | Final _ -> "Final"
  | Float _ -> "Float"
  | FopAsgn _ -> "FopAsgn"
  | FopAsgn1 _ -> "FopAsgn1"
  | FopAsgnArrayField _ -> "FopAsgnArrayField"
  | FopAsgnArrayField2 _ -> "FopAsgnArrayField2"
  | FopAsgnArrayField3 _ -> "FopAsgnArrayField3"
  | FopAsgnArrayField4 _ -> "FopAsgnArrayField4"
  | FopAsgnArrayField5 _ -> "FopAsgnArrayField5"
  | FopAsgnArrayField6 _ -> "FopAsgnArrayField6"
  | FopAsgnArrayField7 _ -> "FopAsgnArrayField7"
  | FopAsgnArrayField8 _ -> "FopAsgnArrayField8"
  | FopAsgnArrayField9 _ -> "FopAsgnArrayField9"
  | FopAsgnArrayMemSel _ -> "FopAsgnArrayMemSel"
  | FopAsgnArrayRange _ -> "FopAsgnArrayRange"
  | FopAsgnArrayRange2 _ -> "FopAsgnArrayRange2"
  | FopAsgnArraySel _ -> "FopAsgnArraySel"
  | FopAsgnArrayWid _ -> "FopAsgnArrayWid"
  | FopAsgnConcat _ -> "FopAsgnConcat"
  | ForEach _ -> "ForEach"
  | ForLoop _ -> "ForLoop"
  | FunDecl _ -> "FunDecl"
  | FunGuts _ -> "FunGuts"
  | FunRef _ -> "FunRef"
  | FunRef2 _ -> "FunRef2"
  | GenBlock _ -> "GenBlock"
  | GenItem _ -> "GenItem"
  | Genvar _ -> "GenItem"
  | Greater _ -> "Greater"
  | GtEq _ -> "GtEq"
  | HyphenGt _ -> "HyphenGt"
  | Id _ -> "Id"
  | IdArrayed1 _ -> "IdArrayed1"
  | IdArrayed2 _ -> "IdArrayed2"
  | IdArrayed3 _ -> "IdArrayed3"
  | IdArrayedColon _ -> "IdArrayedColon"
  | IdArrayedHyphenColon _ -> "IdArrayedHyphenColon"
  | IdArrayedPlusColon _ -> "IdArrayedPlusColon"
  | If1 _ -> "If1"
  | If2 _ -> "If2"
  | Iff _ -> "Iff"
  | Import _ -> "Import"
  | In
  | Inc _ -> "Inc"
  | InitPair _ -> "InitPair"
  | InitPat _ -> "InitPat"
  | InitSig _ -> "InitSig"
  | Initial _ -> "Initial"
  | Inout
  | InsideCase _ -> "InsideCase"
  | InsideRange _ -> "InsideRange"
  | InstArrayDecl _ -> "InstArrayDecl"
  | InstDecl _ -> "InstDecl"
  | InstNameParen1 _ -> "InstNameParen1"
  | InstNameParen2 _ -> "InstNameParen2"
  | InstRange _ -> "InstRange"
  | IntfDecl _ -> "IntfDecl"
  | Intgr _ -> "Intgr"
  | ItemAsgn _ -> "ItemAsgn"
  | Itmlst _ -> "Itmlst"
  | Less _ -> "Less"
  | LocalParamTyp _ -> "LocalParamTyp"
  | Logic _ -> "Logic"
  | LoopGen1 _ -> "LoopGen1"
  | LtEq _ -> "LtEq"
  | LtGt _ -> "LtGt"
  | Mod _ -> "Mod"
  | ModPortItm _ -> "ModPortItm"
  | Modul _ -> "Modul"
  | Mult _ -> "Mult"
  | Nand _ -> "Nand"
  | Neg _ -> "Neg"
  | NetDecl _ -> "NetDecl"
  | NonBlocking _ -> "NonBlocking"
  | Nor _ -> "Nor"
  | NotEq _ -> "NotEq"
  | NotEq3 _ -> "NotEq3"
  | NotEqQuery _ -> "NotEqQuery"
  | Number _ -> "Number"
  | OpenRange _ -> "OpenRange"
  | Or _ -> "Or"
  | Or2 _ -> "Or2"
  | Out
  | PackageBody _ -> "PackageBody"
  | PackageParam _ -> "PackageParam"
  | PackageParam2 _ -> "PackageParam2"
  | Param _ -> "Param"
  | ParamAsgn1 _ -> "ParamAsgn1"
  | ParamAsgn2 _ -> "ParamAsgn2"
  | ParamDecl _ -> "ParamDecl"
  | ParamPort _ -> "ParamPort"
  | PatMember1 _ -> "PatMember1"
  | PatMemberDflt _ -> "PatMemberDflt"
  | PkgImport _ -> "PkgImport"
  | PkgImportItm _ -> "PkgImportItm"
  | Pling _ -> "Pling"
  | Port _ -> "Port"
  | PortDir _ -> "PortDir"
  | PortFront _ -> "PortFront"
  | PortItem _ -> "PortItem"
  | PortItemFront _ -> "PortItemFront"
  | PortItemFront2 _ -> "PortItemFront2"
  | PortsStar _ -> "PortsStar"
  | Pos _ -> "Pos"
  | PropertySpec -> "PropertySpec"
  | Query _ -> "Query"
  | RedAnd _ -> "RedAnd"
  | RedOr _ -> "RedOr"
  | RedXor _ -> "RedXor"
  | Repl _ -> "Repl"
  | Return _ -> "Return"
  | SUDecl _ -> "SUDecl"
  | SUMember _ -> "SUMember"
  | Seq _ -> "Seq"
  | Shiftl _ -> "Shiftl"
  | Shiftr _ -> "Shiftr"
  | Shiftr3 _ -> "Shiftr3"
  | SideEffect _ -> "SideEffect"
  | Signed _ -> "Signed"
  | StarStar _ -> "StarStar"
  | Blocking _ -> "Blocking"
  | String _ -> "String"
  | Sub _ -> "Sub"
  | Sys _ -> "Sys"
  | SysFuncCall _ -> "SysFuncCall"
  | SysTaskCall _ -> "SysTaskCall"
  | TFBody _ -> "TFBody"
  | TF_port_decl _ -> "TF_port_decl"
  | TF_variable _ -> "TF_variable"
  | TaskDecl _ -> "TaskDecl"
  | TaskRef _ -> "TaskRef"
  | SysTaskRef _ -> "SysTaskRef"
  | Tilde _ -> "Tilde"
  | TildeAnd _ -> "TildeAnd"
  | TildeOr _ -> "TildeOr"
  | Typ1 _ -> "Typ1"
  | Typ2 _ -> "Typ2"
  | Typ3 _ -> "Typ3"
  | Typ4 _ -> "Typ4"
  | Typ5 _ -> "Typ5"
  | Typ6 _ -> "Typ6"
  | Typ7 _ -> "Typ7"
  | Typ8 _ -> "Typ8"
  | Typ9 _ -> "Typ9"
  | Typ10 _ -> "Typ10"
  | TypEnum3 _ -> "TypEnum3"
  | TypEnum4 _ -> "TypEnum4"
  | TypEnum5 _ -> "TypEnum5"
  | TypEnum6 _ -> "TypEnum6"
  | TypParam _ -> "TypParam"
  | UMinus _ -> "UMinus"
  | UPlus _ -> "UPlus"
  | Unimplemented _ -> "Unimplemented"
  | Union _ -> "Union"
  | Unknown _ -> "Unknown"
  | Unsigned _ -> "Unsigned"
  | VNum _ -> "VNum"
  | ValueRange _ -> "ValueRange"
  | VarDeclAsgn _ -> "VarDeclAsgn"
  | VarDim _ -> "VarDim"
  | While _ -> "While"
  | WireExpr _ -> "WireExpr"
  | Xnor _ -> "Xnor"
  | Xor _ -> "Xor"

let rec exp_lst_proper = function
| TUPLE4 (STRING "expression_list_proper1", (TUPLE4 (STRING "expression_list_proper1", _, COMMA, _) as lst), COMMA, expr) ->
    expr :: exp_lst_proper lst
| TUPLE4 (STRING "expression_list_proper1", expr, COMMA, expr') -> expr :: expr' :: []
| oth ->failwith "exp_lst_proper"

let rec rw = function
| STRING _ as str -> str
| TK_BinBase _ as b -> b
| TK_BinDigits _ as b -> b
| TK_OctBase _ as o -> o
| TK_OctDigits _ as o -> o
| TK_DecBase _ as d -> d
| TK_DecDigits _ as d -> d
| TK_HexBase _ as h -> h
| TK_HexDigits _ as h -> h
| TK_DecNumber _ as d -> d
| TK_StringLiteral _ as s -> s
| TK_UnBasedNumber _ as d -> d
| TK_RealTime _ as f -> f
| SymbolIdentifier _ as sym -> sym
| SystemTFIdentifier _ as sym -> sym
| TUPLE4 (STRING "assignment_pattern1", QUOTE_LBRACE, lst, RBRACE) ->
  TUPLE4 (STRING "assignment_pattern1", QUOTE_LBRACE, TLIST (exp_lst_proper lst), RBRACE)
(*
| CONS1 (TUPLE5 (STRING "port_declaration_noattr1", _, _, _, _) as p) -> rw p
| CONS1 (CONS2 _ as c) -> rw c
*)
| CONS1 x -> (match rw x with TLIST x -> TLIST x | oth -> TLIST [oth])
| CONS2 (CONS1 a, b) -> (match rw a with TLIST lst -> TLIST (rw b :: lst) | oth -> TLIST (rw b :: oth :: []))
| CONS2 (CONS2 (a, b), c) -> (match rw a with TLIST lst -> TLIST (rw c :: rw b :: lst) | oth -> TLIST (rw c :: rw b :: oth :: []))
| CONS3 (CONS1 a, COMMA, b) -> (match rw a with TLIST lst -> TLIST (rw b :: lst) | oth -> TLIST (rw b :: oth :: []))
| CONS3 (CONS3 (a, COMMA, b), COMMA, c) -> (match rw a with TLIST lst -> TLIST (rw c :: rw b :: lst) | oth -> TLIST (rw c :: rw b :: oth :: []))
| TLIST lst -> TLIST (List.map rw lst)
| TUPLE2(arg1,arg2) -> TUPLE2 (rw arg1, rw arg2)
| TUPLE4(STRING _ as arg0,LPAREN,arg,RPAREN) -> TUPLE4(arg0,LPAREN,rw arg,RPAREN)
| TUPLE3(arg1,arg2,arg3) -> TUPLE3 (rw arg1, rw arg2, rw arg3)
| TUPLE4(arg1,arg2,arg3,arg4) -> TUPLE4 (rw arg1, rw arg2, rw arg3, rw arg4)
| TUPLE5(arg1,arg2,arg3,arg4,arg5) -> TUPLE5 (rw arg1, rw arg2, rw arg3, rw arg4, rw arg5)
| TUPLE6(arg1,arg2,arg3,arg4,arg5,arg6) -> TUPLE6 (rw arg1, rw arg2, rw arg3, rw arg4, rw arg5, rw arg6)
| TUPLE7(arg1,arg2,arg3,arg4,arg5,arg6,arg7) -> TUPLE7 (rw arg1, rw arg2, rw arg3, rw arg4, rw arg5, rw arg6, rw arg7)
| TUPLE8(arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8) ->
   TUPLE8(rw arg1, rw arg2, rw arg3, rw arg4, rw arg5, rw arg6, rw arg7, rw arg8)
| TUPLE9(arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9) ->
   TUPLE9(rw arg1, rw arg2, rw arg3, rw arg4, rw arg5, rw arg6, rw arg7, rw arg8, rw arg9)
| TUPLE10(arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10) ->
   TUPLE10(rw arg1, rw arg2, rw arg3, rw arg4, rw arg5, rw arg6, rw arg7, rw arg8, rw arg9, rw arg10)
| TUPLE11(arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10,arg11) ->
   TUPLE11(rw arg1, rw arg2, rw arg3, rw arg4, rw arg5, rw arg6, rw arg7, rw arg8, rw arg9, rw arg10, rw arg11)
| TUPLE12(arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10,arg11,arg12) ->
   TUPLE12(rw arg1, rw arg2, rw arg3, rw arg4, rw arg5, rw arg6, rw arg7, rw arg8, rw arg9, rw arg10, rw arg11, rw arg12)
| TUPLE13(arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10,arg11,arg12,arg13) ->
   TUPLE13(rw arg1, rw arg2, rw arg3, rw arg4, rw arg5, rw arg6, rw arg7, rw arg8, rw arg9, rw arg10, rw arg11, rw arg12, rw arg13)
| ((AT|EMPTY_TOKEN|LPAREN|RPAREN|LBRACK|RBRACK|LBRACE|RBRACE
| COLON|SEMICOLON|COMMA|CARET|TILDE|QUERY|QUOTE
| PLUS|HYPHEN|STAR|SLASH|HASH|PLING
| AMPERSAND|AMPERSAND_AMPERSAND|AMPERSAND_EQ
| GT_GT_GT|PLUS_COLON|PLUS_PLUS|COLON_COLON
| EQUALS|LT_EQ|VBAR_VBAR|LT_LT|GT_GT|GT_EQ|EQ_EQ|LESS|GREATER|VBAR
| TILDE_VBAR|TILDE_AMPERSAND
| TILDE_CARET
| HYPHEN_HYPHEN
| VBAR_EQ|PLUS_EQ
| PLING_EQ|DOT_STAR|STAR_STAR
| QUOTE_LBRACE
| Module|Always|Assign|Reg|Wire|Logic|Bit|Int|Integer
| Unsigned|Signed
| Output|Input|Posedge|Negedge|Or|DOT
| Parameter|Localparam|Initial
| If|Else|Modport|For
| Begin|End|Endmodule
| Interface|Endinterface
| Task|Endtask
| Package|Endpackage
| Generate|Endgenerate
| Function|Endfunction
| Struct
| Typedef
| Enum
| Case|Casez|Casex|Default|Endcase
| Packed
| Import
| Genvar
| Always_comb|Always_ff|Always_latch
| Return
| Automatic
| Union
| Assert
| Const
| Inout
| Break
| Type
| Property
| While
| String
| Byte
| Longint
| Void
| Inside
| Unique
| Foreach
| Final
| HYPHEN_COLON
| HYPHEN_EQ
| End_of_file
|Zi_zp|Zi_zd|Zi_np|Zi_nd|Xor|Xnor|Wreal|Wor|Wone|Within|
With_LPAREN_covergroup_RPAREN|With|Wildcard|White_noise|Weak1|Weak0|Weak|
Wand|Wait_order|Wait|Virtual|Vectored|Var|VBAR_HYPHEN_GT|VBAR_EQ_GT|Uwire|
Use|Untyped|Until_with|Until|Units|Unique_index|Unique0|UNDERSCORE|
Type_option|Trireg|Trior|Triand|Tri1|Tri0|Tri|Transition|Tranif1|Tranif0|
Tran|Timeunit_check|Timeunit|Timeprecision_check|Timeprecision|Time|
Throughout|This|Tagged|Table|TK_edge_descriptor|TK_XZDigits|
TK_TimeLiteral|TK_RS_EQ|TK_RSS_EQ|
TK_OTHER|TK_LS_EQ|TK_EvalStringLiteral|
TK_AngleBracketInclude|
Sync_reject_on|Sync_accept_on|Supply1|Supply0|Super|Sum|Strong1|Strong0|
Strong|Static|Specparam|Specify|Sort|Solve|Soft|Small|Shuffle|Showcancelled|
Shortreal|Shortint|Sequence|Scalared|Sample|S_until_with|S_until|S_nexttime|
S_eventually|S_always|STAR_RPAREN|STAR_GT|STAR_EQ|
SLASH_SLASH_end_of_line_comment|SLASH_EQ|
SLASH_AMPERSAND_lowast_SEMICOLON_comment_AMPERSAND_lowast_SEMICOLON_SLASH|
SEMICOLON_LPAREN_after_HYPHEN_assertion_HYPHEN_variable_HYPHEN_decls_RPAREN|
Rtranif1|Rtranif0|Rtran|Rsort|Rpmos|Rnmos|Reverse|Restrict|Resolveto|Repeat|
Release|Reject_on|Ref|Realtime|Real|Rcmos|Randsequence|Randomize|Randcase|
Randc|Rand|Pure|Pulsestyle_onevent|Pulsestyle_ondetect|Pullup|Pulldown|Pull1|
Pull0|Protected|Program|Product|Priority|Primitive|Pow|Potential|Pmos|
Paramset|PP_Identifier|PLING_EQ_QUERY|PLING_EQ_EQ|PERCENT_EQ|PERCENT|Option|
Null|Notif1|Notif0|Not|Noshowcancelled|Nor|Noise_table|Nmos|Nexttime|New|
Nettype|Net_resolution|Nature|Nand|NUMBER_step|Min|Medium|Max|Matches|
Macromodule|MacroNumericWidth|MacroIdentifier|MacroIdItem|MacroCallId|
MacroCallCloseToEndLine|MacroArg|Local_COLON_COLON|Local|Limexp|Library|
Liblist|Let|Less_than_TK_else|Last_crossing|Large|Laplace_zp|Laplace_zd|
Laplace_np|Laplace_nd|LT_PLUS|LT_LT_space_GT_GT|LT_LT_filepath_GT_GT|
LT_LT_default_HYPHEN_text_GT_GT|LT_LT_BACKSLASH_BACKSLASH_n_GT_GT|
LT_LT_BACKSLASH_BACKSLASH_line_HYPHEN_cont_GT_GT|
LT_LT_BACKQUOTE_define_HYPHEN_tokens_GT_GT|LT_HYPHEN_GT|
LPAREN_timescale_unit_RPAREN|LPAREN_STAR_attribute_STAR_RPAREN|LPAREN_STAR|
LINEFEED|LBRACK_STAR_RBRACK|LBRACK_STAR|LBRACK_PLUS_RBRACK|LBRACK_HYPHEN_GT|
LBRACK_EQ|Join_none|Join_any|Join|Intersect|Interconnect|Instance|Infinite|
Inf|Include|Incdir|Implies|Implements|Illegal_bins|Ignore_bins|Ifnone|Iff|
Idt_nature|Highz1|Highz0|HYPHEN_GT_LPAREN_trigger_RPAREN|
HYPHEN_GT_LPAREN_logical_HYPHEN_implies_RPAREN|
HYPHEN_GT_LPAREN_constraint_HYPHEN_implies_RPAREN|HYPHEN_GT_GT|HYPHEN_GT|
HASH_HYPHEN_HASH|HASH_HASH|HASH_EQ_HASH|Global|From|Forkjoin|Fork|Forever|
Force|Flow|Flicker_noise|First_match|Find_last_index|Find_last|Find_index|
Find_first_index|Find_first|Find|Extern|Extends|Export|Expect|Exclude|
Eventually|Event|EscapedIdentifier|Error|Endtable|Endspecify|Endsequence|
Endproperty|Endprogram|Endprimitive|Endparamset|Endnature|Endgroup|
Enddiscipline|Endconnectrules|Endconfig|Endclocking|Endclass|Endchecker|Edge|
ERROR_TOKEN|EQ_GT|EQ_EQ_QUERY|EQ_EQ_EQ|EOF_TOKEN|Driver_update|Domain|Do|
Dist|Discrete|Discipline|Disable|Design|Defparam|Deassign|Ddt_nature|
DOUBLEQUOTE|DOLLAR|DLR_width|DLR_unit|DLR_timeskew|DLR_skew|DLR_setuphold|
DLR_setup|DLR_root|DLR_removal|DLR_recrem|DLR_recovery|DLR_period|
DLR_nochange|DLR_hold|DLR_fullskew|DLR_attribute|DEFAULT|Cross|Coverpoint|
Covergroup|Cover|Continuous|Continue|Context|Constraint|Connectrules|
Connectmodule|Connect|Config|Cmos|Clocking|Class|Checker|Chandle|Cell|
COLON_SLASH|COLON_EQ|CHAR|CARET_EQ|Bufif1|Bufif0|Buf|Binsof|Bins|Bind|Before|
BACKSLASH|BACKQUOTE_uselib|BACKQUOTE_undef|BACKQUOTE_unconnected_drive|
BACKQUOTE_timescale|BACKQUOTE_suppress_faults|BACKQUOTE_resetall|
BACKQUOTE_protect|BACKQUOTE_pragma|BACKQUOTE_nounconnected_drive|
BACKQUOTE_nosuppress_faults|BACKQUOTE_include|BACKQUOTE_ifndef|
BACKQUOTE_ifdef|BACKQUOTE_endprotect|BACKQUOTE_endif|BACKQUOTE_endcelldefine|
BACKQUOTE_end_keywords|BACKQUOTE_enable_portfaults|BACKQUOTE_elsif|
BACKQUOTE_else|BACKQUOTE_disable_portfaults|BACKQUOTE_delay_mode_zero|
BACKQUOTE_delay_mode_unit|BACKQUOTE_delay_mode_path|
BACKQUOTE_delay_mode_distributed|BACKQUOTE_define|
BACKQUOTE_default_trireg_strength|BACKQUOTE_default_nettype|
BACKQUOTE_default_decay_time|BACKQUOTE_celldefine|BACKQUOTE_begin_keywords|
BACKQUOTE_UNDERSCORE_UNDERSCORE_UNDERSCORE_UNDERSCORE_verible_verilog_library_end____|
BACKQUOTE_UNDERSCORE_UNDERSCORE_UNDERSCORE_UNDERSCORE_verible_verilog_library_begin____|
BACKQUOTE_BACKQUOTE|BACKQUOTE|Assume|And|Analysis|Analog|Aliasparam|Alias|
Access|Accept_on|Ac_stim|Abstol|Absdelay|AT_AT|AMPERSAND_AMPERSAND_AMPERSAND|
ACCEPT|TUPLE15 _|TUPLE14 _|SLIST _|
CONS5 _
) as oth) -> oth
| oth -> othcons := oth; failwith "CONS1"
(*  
| oth -> failwith ("rw fail: "^Source_text_verible_tokens.getstr oth)
*)

type attr = {subst: (rw,rw)Hashtbl.t; fn: attr -> rw -> rw; pth: string}

let rec descend' (attr:attr) = function
  | Add(rw1, rw2) -> Add(descend_itm attr rw1, descend_itm attr rw2)
  | AlwaysComb(rw_lst1) -> AlwaysComb(descend_lst attr rw_lst1)
  | AlwaysComb2(rw1) -> AlwaysComb2(descend_itm attr rw1)
  | AlwaysFF(rw1, rw2) -> AlwaysFF(descend_itm attr rw1, descend_itm attr rw2)
  | AlwaysLatch(rw1) -> AlwaysLatch(descend_itm attr rw1)
  | AlwaysLegacy(rw1, rw2) -> AlwaysLegacy(descend_itm attr rw1, descend_itm attr rw2)
  | AlwaysSync -> AlwaysSync
  | And(rw1, rw2) -> And(descend_itm attr rw1, descend_itm attr rw2)
  | And2(rw1, rw2) -> And2(descend_itm attr rw1, descend_itm attr rw2)
  | And3(rw1, rw2) -> And3(descend_itm attr rw1, descend_itm attr rw2)
  | AnyRange(rw1, rw2) -> AnyRange(descend_itm attr rw1, descend_itm attr rw2)
  | Asgn1(rw1, rw2) -> Asgn1(descend_itm attr rw1, descend_itm attr rw2)
  | AsgnPat(rw_lst1) -> AsgnPat(descend_lst attr rw_lst1)
  | Assert -> Assert
  | AssertProperty -> AssertProperty
  | At(rw1) -> At(descend_itm attr rw1)
  | AtStar -> AtStar
  | Atom(string1) -> Atom(string1)
  | AutoFunDecl(string1, rw2, rw3) -> AutoFunDecl(string1, descend_itm attr rw2, descend_itm attr rw3)
  | BeginBlock(rw_lst1) -> BeginBlock(descend_lst attr rw_lst1)
  | Bitlst(rw_lst1) -> Bitlst(descend_lst attr rw_lst1)
  | BlockItem(rw1) -> BlockItem(descend_itm attr rw1)
  | Blocking(rw1) -> Blocking(descend_itm attr rw1)
  | BreakSemi -> BreakSemi
  | CaretTilde(rw1) -> CaretTilde(descend_itm attr rw1)
  | CaseItm(rw_lst1) -> CaseItm(descend_lst attr rw_lst1)
  | CaseStart(rw1, rw_lst2) -> CaseStart(descend_itm attr rw1, descend_lst attr rw_lst2)
  | CaseStart1(rw1) -> CaseStart1(descend_itm attr rw1)
  | CaseStartInside(rw1, rw_lst2) -> CaseStartInside(descend_itm attr rw1, descend_lst attr rw_lst2)
  | CaseStartUniq(rw1, rw_lst2) -> CaseStartUniq(descend_itm attr rw1, descend_lst attr rw_lst2)
  | CaseStartUniqInside(rw1, rw_lst2) -> CaseStartUniqInside(descend_itm attr rw1, descend_lst attr rw_lst2)
  | CaseStmt(rw_lst1, rw_lst2) -> CaseStmt(descend_lst attr rw_lst1, descend_lst attr rw_lst2)
  | Cast(rw1, rw2) -> Cast(descend_itm attr rw1, descend_itm attr rw2)
  | CellParamItem2(string1, rw2) -> CellParamItem2(string1, descend_itm attr rw2)
  | CellParamItem3(string1, rw2) -> CellParamItem3(string1, descend_itm attr rw2)
  | CellPinItem2(string1, rw2) -> CellPinItem2(string1, descend_itm attr rw2)
  | CellPinItemImplied(string1) -> CellPinItemImplied(string1)
  | CellPinItemNC(string1) -> CellPinItemNC(string1)
  | Concat(rw_lst1) -> Concat(descend_lst attr rw_lst1)
  | CondGen1(rw1, rw2, rw3) -> CondGen1(descend_itm attr rw1, descend_itm attr rw2, descend_itm attr rw3)
  | ContAsgn(rw_lst1) -> ContAsgn(descend_lst attr rw_lst1)
  | DeclAsgn(rw1, rw_lst2) -> DeclAsgn(descend_itm attr rw1, descend_lst attr rw_lst2)
  | DeclData(rw1, rw_lst2) -> DeclData(descend_itm attr rw1, descend_lst attr rw_lst2)
  | DeclInt2(rw_lst1) -> DeclInt2(descend_lst attr rw_lst1)
  | DeclLogic(rw_lst1) -> DeclLogic(descend_lst attr rw_lst1)
  | DeclLogic2(rw_lst1, rw_lst2) -> DeclLogic2(descend_lst attr rw_lst1, descend_lst attr rw_lst2)
  | DeclModPort(rw_lst1) -> DeclModPort(descend_lst attr rw_lst1)
  | DeclReg(rw_lst1, rw_lst2, rw3) -> DeclReg(descend_lst attr rw_lst1, descend_lst attr rw_lst2, descend_itm attr rw3)
  | Deflt -> Deflt
  | Div(rw1, rw2) -> Div(descend_itm attr rw1, descend_itm attr rw2)
  | Dot1(rw1, rw2) -> Dot1(descend_itm attr rw1, descend_itm attr rw2)
  | Dot3(rw1, rw2, rw3) -> Dot3(descend_itm attr rw1, descend_itm attr rw2, descend_itm attr rw3)
  | DotBus(rw1, rw2, rw3, rw_lst4) -> DotBus(descend_itm attr rw1, descend_itm attr rw2, descend_itm attr rw3, descend_lst attr rw_lst4)
  | Edge(rw1, rw2) -> Edge(descend_itm attr rw1, descend_itm attr rw2)
  | ElabTask(rw1) -> ElabTask(descend_itm attr rw1)
  | ElseStmt(rw1) -> ElseStmt(descend_itm attr rw1)
  | EnumInit(string1, rw2) -> EnumInit(string1, descend_itm attr rw2)
  | Equals(rw1, rw2) -> Equals(descend_itm attr rw1, descend_itm attr rw2)
  | Equals3(rw1, rw2) -> Equals3(descend_itm attr rw1, descend_itm attr rw2)
  | EqualsQuery(rw1, rw2) -> EqualsQuery(descend_itm attr rw1, descend_itm attr rw2)
  | Equate(rw1, rw2) -> Equate(descend_itm attr rw1, descend_itm attr rw2)
  | EquateArrayField(rw1, rw2, rw3, rw4, rw5) -> EquateArrayField(descend_itm attr rw1, descend_itm attr rw2, descend_itm attr rw3, descend_itm attr rw4, descend_itm attr rw5)
  | EquateConcat(rw_lst1, rw2) -> EquateConcat(descend_lst attr rw_lst1, descend_itm attr rw2)
  | EquateField(rw1, rw2, rw3) -> EquateField(descend_itm attr rw1, descend_itm attr rw2, descend_itm attr rw3)
  | EquateSelect(rw1, rw2, rw3) -> EquateSelect(descend_itm attr rw1, descend_itm attr rw2, descend_itm attr rw3)
  | EquateSelect2(rw1, rw2, rw3) -> EquateSelect2(descend_itm attr rw1, descend_itm attr rw2, descend_itm attr rw3)
  | EquateSlice(rw1, rw2, rw3, rw4) -> EquateSlice(descend_itm attr rw1, descend_itm attr rw2, descend_itm attr rw3, descend_itm attr rw4)
  | EquateSlicePlus(rw1, rw2, rw3, rw4) -> EquateSlicePlus(descend_itm attr rw1, descend_itm attr rw2, descend_itm attr rw3, descend_itm attr rw4)
  | EventOr(rw_lst1) -> EventOr(descend_lst attr rw_lst1)
  | ExprOKL(rw_lst1) -> ExprOKL(descend_lst attr rw_lst1)
  | ExprQuote1(rw1, rw2) -> ExprQuote1(descend_itm attr rw1, descend_itm attr rw2)
  | Expression(rw1) -> Expression(descend_itm attr rw1)
  | Final(rw_lst1) -> Final(descend_lst attr rw_lst1)
  | Float(float1) -> Float(float1)
  | FopAsgn(rw1, rw2) -> FopAsgn(descend_itm attr rw1, descend_itm attr rw2)
  | FopAsgn1(rw1, rw2, rw3, rw4) -> FopAsgn1(descend_itm attr rw1, descend_itm attr rw2, descend_itm attr rw3, descend_itm attr rw4)
  | FopAsgnArrayField(rw1, rw2, rw3) -> FopAsgnArrayField(descend_itm attr rw1, descend_itm attr rw2, descend_itm attr rw3)
  | FopAsgnArrayField2(rw1, rw2, rw3) -> FopAsgnArrayField2(descend_itm attr rw1, descend_itm attr rw2, descend_itm attr rw3)
  | FopAsgnArrayField3(rw1, rw2, rw3, rw4) -> FopAsgnArrayField3(descend_itm attr rw1, descend_itm attr rw2, descend_itm attr rw3, descend_itm attr rw4)
  | FopAsgnArrayField4(rw1, rw2, rw3, rw4, rw5, rw6) -> FopAsgnArrayField4(descend_itm attr rw1, descend_itm attr rw2, descend_itm attr rw3, descend_itm attr rw4, descend_itm attr rw5, descend_itm attr rw6)
  | FopAsgnArrayField5(rw1, rw2, rw3, rw4, rw5) -> FopAsgnArrayField5(descend_itm attr rw1, descend_itm attr rw2, descend_itm attr rw3, descend_itm attr rw4, descend_itm attr rw5)
  | FopAsgnArrayField6(rw1, rw2, rw3, rw4, rw5) -> FopAsgnArrayField6(descend_itm attr rw1, descend_itm attr rw2, descend_itm attr rw3, descend_itm attr rw4, descend_itm attr rw5)
  | FopAsgnArrayField7(rw1, rw2, rw3, rw4, rw5) -> FopAsgnArrayField7(descend_itm attr rw1, descend_itm attr rw2, descend_itm attr rw3, descend_itm attr rw4, descend_itm attr rw5)
  | FopAsgnArrayField8(rw1, rw2, rw3, rw4, rw5) -> FopAsgnArrayField8(descend_itm attr rw1, descend_itm attr rw2, descend_itm attr rw3, descend_itm attr rw4, descend_itm attr rw5)
  | FopAsgnArrayField9(rw1, rw2, rw3, rw4, rw5, rw6) -> FopAsgnArrayField9(descend_itm attr rw1, descend_itm attr rw2, descend_itm attr rw3, descend_itm attr rw4, descend_itm attr rw5, descend_itm attr rw6)
  | FopAsgnArrayMemSel(rw1, rw2, rw3, rw4) -> FopAsgnArrayMemSel(descend_itm attr rw1, descend_itm attr rw2, descend_itm attr rw3, descend_itm attr rw4)
  | FopAsgnArrayRange(rw1, rw2, rw3, rw4) -> FopAsgnArrayRange(descend_itm attr rw1, descend_itm attr rw2, descend_itm attr rw3, descend_itm attr rw4)
  | FopAsgnArrayRange2(rw1, rw2, rw3, rw4) -> FopAsgnArrayRange2(descend_itm attr rw1, descend_itm attr rw2, descend_itm attr rw3, descend_itm attr rw4)
  | FopAsgnArraySel(rw1, rw2, rw3) -> FopAsgnArraySel(descend_itm attr rw1, descend_itm attr rw2, descend_itm attr rw3)
  | FopAsgnArrayWid(rw1, rw2, rw3, rw4) -> FopAsgnArrayWid(descend_itm attr rw1, descend_itm attr rw2, descend_itm attr rw3, descend_itm attr rw4)
  | FopAsgnConcat(rw_lst1, rw2) -> FopAsgnConcat(descend_lst attr rw_lst1, descend_itm attr rw2)
  | ForEach(rw1, rw_lst2) -> ForEach(descend_itm attr rw1, descend_lst attr rw_lst2)
  | ForLoop(rw_lst1, rw2, rw3, rw4) -> ForLoop(descend_lst attr rw_lst1, descend_itm attr rw2, descend_itm attr rw3, descend_itm attr rw4)
  | FunDecl(string1, rw2, rw3) -> FunDecl(string1, descend_itm attr rw2, descend_itm attr rw3)
  | FunGuts(rw_lst1, rw_lst2) -> FunGuts(descend_lst attr rw_lst1, descend_lst attr rw_lst2)
  | FunRef(string1, rw_lst2) -> FunRef(string1, descend_lst attr rw_lst2)
  | FunRef2(string1, rw_lst2, rw_lst3) -> FunRef2(string1, descend_lst attr rw_lst2, descend_lst attr rw_lst3)
  | GenBlock(rw_lst1) -> GenBlock(descend_lst attr rw_lst1)
  | GenItem(string1, rw_lst2) -> GenItem(string1, descend_lst attr rw_lst2)
  | Genvar(rw_lst1) -> Genvar(descend_lst attr rw_lst1)
  | Greater(rw1, rw2) -> Greater(descend_itm attr rw1, descend_itm attr rw2)
  | GtEq(rw1, rw2) -> GtEq(descend_itm attr rw1, descend_itm attr rw2)
  | HyphenGt(rw1, rw2) -> HyphenGt(descend_itm attr rw1, descend_itm attr rw2)
  | Id(string1) -> Id(string1)
  | IdArrayed1(rw1, rw2, rw3) -> IdArrayed1(descend_itm attr rw1, descend_itm attr rw2, descend_itm attr rw3)
  | IdArrayed2(rw1, rw2) -> IdArrayed2(descend_itm attr rw1, descend_itm attr rw2)
  | IdArrayed3(rw_lst1, rw2) -> IdArrayed3(descend_lst attr rw_lst1, descend_itm attr rw2)
  | IdArrayedColon(rw1, rw2, rw3) -> IdArrayedColon(descend_itm attr rw1, descend_itm attr rw2, descend_itm attr rw3)
  | IdArrayedHyphenColon(rw1, rw2, rw3) -> IdArrayedHyphenColon(descend_itm attr rw1, descend_itm attr rw2, descend_itm attr rw3)
  | IdArrayedPlusColon(rw1, rw2, rw3) -> IdArrayedPlusColon(descend_itm attr rw1, descend_itm attr rw2, descend_itm attr rw3)
  | If1(rw1, rw2) -> If1(descend_itm attr rw1, descend_itm attr rw2)
  | If2(rw1, rw2, rw3) -> If2(descend_itm attr rw1, descend_itm attr rw2, descend_itm attr rw3)
  | Iff(rw1, rw2) -> Iff(descend_itm attr rw1, descend_itm attr rw2)
  | Import(rw_lst1) -> Import(descend_lst attr rw_lst1)
  | In -> In
  | Inc(rw1) -> Inc(descend_itm attr rw1)
  | InitPair(rw1, rw2) -> InitPair(descend_itm attr rw1, descend_itm attr rw2)
  | InitPat(rw_lst1) -> InitPat(descend_lst attr rw_lst1)
  | InitSig(rw1, rw2) -> InitSig(descend_itm attr rw1, descend_itm attr rw2)
  | Initial(rw_lst1) -> Initial(descend_lst attr rw_lst1)
  | Inout -> Inout
  | InsideCase(rw1, rw2) -> InsideCase(descend_itm attr rw1, descend_itm attr rw2)
  | InsideRange(rw1, rw2) -> InsideRange(descend_itm attr rw1, descend_itm attr rw2)
  | InstArrayDecl(rw1, rw_lst2, rw3, rw_lst4, rw_lst5) -> InstArrayDecl(descend_itm attr rw1, descend_lst attr rw_lst2, descend_itm attr rw3, descend_lst attr rw_lst4, descend_lst attr rw_lst5)
  | InstDecl(rw1, rw_lst2, rw_lst3) -> InstDecl(descend_itm attr rw1, descend_lst attr rw_lst2, descend_lst attr rw_lst3)
  | InstNameParen1(string1, rw_lst2) -> InstNameParen1(string1, descend_lst attr rw_lst2)
  | InstNameParen2(string1, rw_lst2) -> InstNameParen2(string1, descend_lst attr rw_lst2)
  | InstRange(rw1, rw2) -> InstRange(descend_itm attr rw1, descend_itm attr rw2)
  | IntfDecl(string1, rw2, rw3, rw4) -> IntfDecl(string1, descend_itm attr rw2, descend_itm attr rw3, descend_itm attr rw4)
  | Intgr(int1) -> Intgr(int1)
  | ItemAsgn(rw1) -> ItemAsgn(descend_itm attr rw1)
  | Itmlst(rw_lst1) -> Itmlst(descend_lst attr rw_lst1)
  | Less(rw1, rw2) -> Less(descend_itm attr rw1, descend_itm attr rw2)
  | LocalParamTyp(rw1) -> LocalParamTyp(descend_itm attr rw1)
  | Logic(rw_lst1, rw_lst2) -> Logic(descend_lst attr rw_lst1, descend_lst attr rw_lst2)
  | LoopGen1(rw1, string2, rw3, rw4, rw5, rw_lst6) -> LoopGen1(descend_itm attr rw1, string2, descend_itm attr rw3, descend_itm attr rw4, descend_itm attr rw5, descend_lst attr rw_lst6)
  | LtEq(rw1, rw2) -> LtEq(descend_itm attr rw1, descend_itm attr rw2)
  | LtGt(rw1, rw2) -> LtGt(descend_itm attr rw1, descend_itm attr rw2)
  | Mod(rw1, rw2) -> Mod(descend_itm attr rw1, descend_itm attr rw2)
  | ModPortItm(string1, rw_lst2) -> ModPortItm(string1, descend_lst attr rw_lst2)
  | Modul(string1, rw_lst2, rw_lst3, rw_lst4) -> Modul(string1, descend_lst attr rw_lst2, descend_lst attr rw_lst3, descend_lst attr rw_lst4)
  | Mult(rw1, rw2) -> Mult(descend_itm attr rw1, descend_itm attr rw2)
  | Nand(rw1, rw2) -> Nand(descend_itm attr rw1, descend_itm attr rw2)
  | Neg(rw1) -> Neg(descend_itm attr rw1)
  | NetDecl(rw_lst1, rw_lst2) -> NetDecl(descend_lst attr rw_lst1, descend_lst attr rw_lst2)
  | NonBlocking(rw1, rw2) -> NonBlocking(descend_itm attr rw1, descend_itm attr rw2)
  | Nor(rw1, rw2) -> Nor(descend_itm attr rw1, descend_itm attr rw2)
  | NotEq(rw1, rw2) -> NotEq(descend_itm attr rw1, descend_itm attr rw2)
  | NotEq3(rw1, rw2) -> NotEq3(descend_itm attr rw1, descend_itm attr rw2)
  | NotEqQuery(rw1, rw2) -> NotEqQuery(descend_itm attr rw1, descend_itm attr rw2)
  | Number(int1, int2, int3, string4) -> Number(int1, int2, int3, string4)
  | OpenRange(rw_lst1) -> OpenRange(descend_lst attr rw_lst1)
  | Or(rw1, rw2) -> Or(descend_itm attr rw1, descend_itm attr rw2)
  | Or2(rw1, rw2) -> Or2(descend_itm attr rw1, descend_itm attr rw2)
  | Out -> Out
  | PackageBody(string1, rw_lst2) -> PackageBody(string1, descend_lst attr rw_lst2)
  | PackageParam(rw_lst1, rw2) -> PackageParam(descend_lst attr rw_lst1, descend_itm attr rw2)
  | PackageParam2(string1, string2, rw_lst3, rw4) -> PackageParam2(string1, string2, descend_lst attr rw_lst3, descend_itm attr rw4)
  | Param(string1, rw2, rw_lst3) -> Param(string1, descend_itm attr rw2, descend_lst attr rw_lst3)
  | ParamAsgn1(string1, rw2) -> ParamAsgn1(string1, descend_itm attr rw2)
  | ParamAsgn2(string1, rw_lst2, rw3) -> ParamAsgn2(string1, descend_lst attr rw_lst2, descend_itm attr rw3)
  | ParamDecl(rw1, rw_lst2) -> ParamDecl(descend_itm attr rw1, descend_lst attr rw_lst2)
  | ParamPort(rw_lst1) -> ParamPort(descend_lst attr rw_lst1)
  | PatMember1(rw1, rw2) -> PatMember1(descend_itm attr rw1, descend_itm attr rw2)
  | PatMemberDflt(rw1) -> PatMemberDflt(descend_itm attr rw1)
  | PkgImport(rw1) -> PkgImport(descend_itm attr rw1)
  | PkgImportItm(string1, rw2) -> PkgImportItm(string1, descend_itm attr rw2)
  | Pling(rw1) -> Pling(descend_itm attr rw1)
  | Port(rw1, string2, rw_lst3, rw4) -> Port(descend_itm attr rw1, string2, descend_lst attr rw_lst3, descend_itm attr rw4)
  | PortDir(rw1, rw2) -> PortDir(descend_itm attr rw1, descend_itm attr rw2)
  | PortFront(rw1, rw2) -> PortFront(descend_itm attr rw1, descend_itm attr rw2)
  | PortItem(rw1, rw2) -> PortItem(descend_itm attr rw1, descend_itm attr rw2)
  | PortItemFront(rw1, rw2) -> PortItemFront(descend_itm attr rw1, descend_itm attr rw2)
  | PortItemFront2(rw1, rw2, rw_lst3) -> PortItemFront2(descend_itm attr rw1, descend_itm attr rw2, descend_lst attr rw_lst3)
  | PortsStar(rw_lst1) -> PortsStar(descend_lst attr rw_lst1)
  | Pos(rw1) -> Pos(descend_itm attr rw1)
  | PropertySpec -> PropertySpec
  | Query(rw1, rw2, rw3) -> Query(descend_itm attr rw1, descend_itm attr rw2, descend_itm attr rw3)
  | RedAnd(rw1) -> RedAnd(descend_itm attr rw1)
  | RedOr(rw1) -> RedOr(descend_itm attr rw1)
  | RedXor(rw1) -> RedXor(descend_itm attr rw1)
  | Repl(rw1, rw_lst2) -> Repl(descend_itm attr rw1, descend_lst attr rw_lst2)
  | Return(rw1) -> Return(descend_itm attr rw1)
  | SUDecl(rw1, rw_lst2) -> SUDecl(descend_itm attr rw1, descend_lst attr rw_lst2)
  | SUMember(rw1, rw_lst2) -> SUMember(descend_itm attr rw1, descend_lst attr rw_lst2)
  | Seq(string1, rw_lst2) -> Seq(string1, descend_lst attr rw_lst2)
  | Shiftl(rw1, rw2) -> Shiftl(descend_itm attr rw1, descend_itm attr rw2)
  | Shiftr(rw1, rw2) -> Shiftr(descend_itm attr rw1, descend_itm attr rw2)
  | Shiftr3(rw1, rw2) -> Shiftr3(descend_itm attr rw1, descend_itm attr rw2)
  | SideEffect(rw1, rw2) -> SideEffect(descend_itm attr rw1, descend_itm attr rw2)
  | Signed(rw1) -> Signed(descend_itm attr rw1)
  | StarStar(rw1, rw2) -> StarStar(descend_itm attr rw1, descend_itm attr rw2)
  | String(string1) -> String(string1)
  | Sub(rw1, rw2) -> Sub(descend_itm attr rw1, descend_itm attr rw2)
  | Sys(string1, rw2) -> Sys(string1, descend_itm attr rw2)
  | SysFuncCall(string1, rw_lst2) -> SysFuncCall(string1, descend_lst attr rw_lst2)
  | SysTaskCall(string1, rw_lst2) -> SysTaskCall(string1, descend_lst attr rw_lst2)
  | SysTaskRef(rw1, rw_lst2) -> SysTaskRef(descend_itm attr rw1, descend_lst attr rw_lst2)
  | TFBody(rw_lst1, rw_lst2) -> TFBody(descend_lst attr rw_lst1, descend_lst attr rw_lst2)
  | TF_port_decl(rw1, rw_lst2, rw_lst3) -> TF_port_decl(descend_itm attr rw1, descend_lst attr rw_lst2, descend_lst attr rw_lst3)
  | TF_variable(rw1, rw2, rw3, rw4) -> TF_variable(descend_itm attr rw1, descend_itm attr rw2, descend_itm attr rw3, descend_itm attr rw4)
  | TaskDecl(string1, rw2, rw3, rw4) -> TaskDecl(string1, descend_itm attr rw2, descend_itm attr rw3, descend_itm attr rw4)
  | TaskRef(string1, rw_lst2) -> TaskRef(string1, descend_lst attr rw_lst2)
  | Tilde(rw1) -> Tilde(descend_itm attr rw1)
  | TildeAnd(rw1) -> TildeAnd(descend_itm attr rw1)
  | TildeOr(rw1) -> TildeOr(descend_itm attr rw1)
  | Typ1(string1) -> Typ1(string1)
  | Typ10(string1, rw_lst2, string3) -> Typ10(string1, descend_lst attr rw_lst2, string3)
  | Typ2(string1, rw_lst2, rw_lst3) -> Typ2(string1, descend_lst attr rw_lst2, descend_lst attr rw_lst3)
  | Typ3(string1, rw_lst2) -> Typ3(string1, descend_lst attr rw_lst2)
  | Typ4(string1, rw_lst2, rw_lst3, rw_lst4) -> Typ4(string1, descend_lst attr rw_lst2, descend_lst attr rw_lst3, descend_lst attr rw_lst4)
  | Typ5(rw1, rw_lst2) -> Typ5(descend_itm attr rw1, descend_lst attr rw_lst2)
  | Typ6(rw1) -> Typ6(descend_itm attr rw1)
  | Typ7(string1, rw2) -> Typ7(string1, descend_itm attr rw2)
  | Typ8(rw1, rw2) -> Typ8(descend_itm attr rw1, descend_itm attr rw2)
  | Typ9(string1, rw_lst2, rw3) -> Typ9(string1, descend_lst attr rw_lst2, descend_itm attr rw3)
  | TypEnum3(rw_lst1) -> TypEnum3(descend_lst attr rw_lst1)
  | TypEnum4(rw1, rw_lst2, rw_lst3) -> TypEnum4(descend_itm attr rw1, descend_lst attr rw_lst2, descend_lst attr rw_lst3)
  | TypEnum5(rw1) -> TypEnum5(descend_itm attr rw1)
  | TypEnum6(string1, rw2, rw_lst3) -> TypEnum6(string1, descend_itm attr rw2, descend_lst attr rw_lst3)
  | TypParam(string1, rw2, rw_lst3) -> TypParam(string1, descend_itm attr rw2, descend_lst attr rw_lst3)
  | UMinus(rw1) -> UMinus(descend_itm attr rw1)
  | UPlus(rw1) -> UPlus(descend_itm attr rw1)
  | Unimplemented(string1, rw_lst2) -> Unimplemented(string1, descend_lst attr rw_lst2)
  | Union(rw1, rw_lst2) -> Union(descend_itm attr rw1, descend_lst attr rw_lst2)
  | Unknown(string1, rw_lst2) -> Unknown(string1, descend_lst attr rw_lst2)
  | Unsigned(rw1) -> Unsigned(descend_itm attr rw1)
  | VNum(string1) -> VNum(string1)
  | ValueRange(rw1, rw2) -> ValueRange(descend_itm attr rw1, descend_itm attr rw2)
  | VarDeclAsgn(rw1, rw2) -> VarDeclAsgn(descend_itm attr rw1, descend_itm attr rw2)
  | VarDim(rw1) -> VarDim(descend_itm attr rw1)
  | While(rw1, rw_lst2) -> While(descend_itm attr rw1, descend_lst attr rw_lst2)
  | WireExpr(rw1, rw2) -> WireExpr(descend_itm attr rw1, descend_itm attr rw2)
  | Xnor(rw1, rw2) -> Xnor(descend_itm attr rw1, descend_itm attr rw2)
  | Xor(rw1, rw2) -> Xor(descend_itm attr rw1, descend_itm attr rw2)
  
and descend_lst attr x = List.map (fun x -> if verbose then print_endline (getstr x); attr.fn attr x) x

and descend_itm attr x = if verbose then print_endline (getstr x); attr.fn attr x

let rec descend (attr:attr) = function
  | Id _ as id -> (match Hashtbl.find_opt attr.subst id with None -> id | Some exp -> exp)
  | oth -> descend' {attr with fn=descend} oth
