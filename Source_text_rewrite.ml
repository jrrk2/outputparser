open Source_text_rewrite_types
open Source_text_lex
open Source_text

let verbose = try int_of_string (Sys.getenv ("DESCEND_VERBOSE")) > 0 with _ -> false

let getstr = function
  | Add _ -> "Add"
  | AlwaysComb _ -> "AlwaysComb"
  | AlwaysComb2 _ -> "AlwaysComb2"
  | AlwaysFF _ -> "AlwaysFF"
  | AlwaysLatch _ -> "AlwaysLatch"
  | AlwaysLegacy _ -> "AlwaysLegacy"
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
  | Generate _ -> "Generate"
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

let rec rw = function
| CONS1 oth -> TLIST (rw oth::[])
| CONS3(lft,_,rght) -> (match rw lft with TLIST arg -> TLIST (rw rght :: arg) | oth -> TLIST (rw rght :: oth :: []))
| CONS4(lft,arg1,arg2,arg3) -> (match rw lft with TLIST arg -> TLIST (rw arg1 :: rw arg2 :: rw arg3 :: arg) | _ -> failwith "CONS4")
| CONS2(lft,rght) -> (match rw lft with TLIST arg -> TLIST (rw rght :: arg) | _ -> failwith "CONS2")
| ELIST lst -> ELIST (List.map rw lst)
| TLIST lst -> TLIST (List.map rw lst)
| TUPLE2(arg1,arg2) -> TUPLE2 (rw arg1, rw arg2)
| TUPLE4(STRING _ as arg0,LPAREN,arg,RPAREN) -> TUPLE4(arg0,LPAREN,rw arg,RPAREN)
| TUPLE4(STRING _,arg1,(PLUS|HYPHEN|STAR|SLASH|AMPERSAND|AMPERSAND_AMPERSAND|VBAR|VBAR_VBAR|EQ_EQ|PLING_EQ|LT_EQ|GT_EQ|LT_LT|GT_GT|GT_GT_GT|LESS|GREATER|CARET|CARET_TILDE|STAR_STAR|EQ_EQ_EQ|PLING_EQ_EQ as arg2),arg3) -> ELIST (flatten (flatten (arg1 :: arg2 :: arg3 :: [])))
| TUPLE5(arg1,QUERY,arg3,COLON,arg5) -> ELIST (flatten (flatten (arg1 :: QUERY :: arg3 :: COLON :: arg5 :: [])))
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
| ((IDENTIFIER _
| INTEGER_NUMBER _| STRING _
| AT|EMPTY_TOKEN|LPAREN|RPAREN|LBRACK|RBRACK|LBRACE|RBRACE
| COLON|SEMICOLON|COMMA|CARET|TILDE|QUERY|QUOTE|PERCENT
| PLUS|HYPHEN|STAR|SLASH|HASH|PLING
| AMPERSAND|AMPERSAND_AMPERSAND|AMPERSAND_EQ
| GT_GT_GT|PLUS_COLON|PLUS_PLUS|COLON_COLON
| EQUALS|LT_EQ|VBAR_VBAR|LT_LT|GT_GT|GT_EQ|EQ_EQ|LESS|GREATER|VBAR
| TILDE_VBAR|TILDE_AMPERSAND|EQ_EQ_EQ|PLING_EQ_EQ
| CARET_TILDE
| HYPHEN_HYPHEN
| VBAR_EQ|PLUS_EQ
| PLING_EQ|DOT_STAR|STAR_STAR
| TYPE_HYPHEN_IDENTIFIER _|IDENTIFIER_HYPHEN_COLON_COLON _|QUOTE_LBRACE
| DLR_display|DLR_stop|DLR_finish|DLR_write
| DLR_signed|DLR_unsigned|DLR_time|DLR_readmemh|DLR_clog2|DLR_bits|DLR_error
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
| DLR_fatal
| DLR_warning
| DLR_random
| DLR_sformatf
| DLR_fscanf
| DLR_fwrite
| DLR_fclose
| DLR_fopen
| DLR_feof
| DLR_size
| DLR_high
| DLR_low
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
| FLOATING_HYPHEN_POINT_NUMBER _
| EOF_TOKEN) as oth) -> oth
| oth -> failwith ("rw fail: "^Source_text_types.getstr oth)

and flatten lst = List.flatten (List.map (function ELIST lst -> List.map rw lst | oth -> [rw oth]) lst)
 
type attr = {subst: (rw,rw)Hashtbl.t; fn: attr -> rw -> rw; pth: string}

let rec descend' (attr:attr) = function
  | Add(rw, rw2) -> Add(descend_itm attr (rw), descend_itm attr (rw2))
  | AlwaysComb(rw_lst) -> AlwaysComb(descend_lst attr (rw_lst))
  | AlwaysComb2 (rw) -> AlwaysComb2 (descend_itm attr rw)
  | AlwaysFF (_, _) as x -> x
  | AlwaysLatch _ as x -> x
  | AlwaysLegacy (rw, rw') -> AlwaysLegacy (descend_itm attr rw, descend_itm attr rw')
  | And(rw, rw2) -> And(descend_itm attr (rw), descend_itm attr (rw2))
  | And2(rw, rw2) -> And2(descend_itm attr (rw), descend_itm attr (rw2))
  | And3 (_, _) as x -> x
  | AnyRange (_, _) as x -> x
  | Asgn1 (lhs, rhs) -> Asgn1(descend_itm attr lhs, descend_itm attr rhs)
  | AsgnPat _ as x -> x
  | Assert as x -> x
  | AssertProperty as x -> x
  | At _ as x -> x
  | AtStar as x -> x
  | Atom _ as x -> x
  | AutoFunDecl (_, _, _) as x -> x
  | BeginBlock(rw_lst) -> BeginBlock(descend_lst attr (rw_lst))
  | Bitlst(rw_lst) -> Bitlst(descend_lst attr (rw_lst))
  | Blocking rw -> Blocking ( descend_itm attr rw )
  | BlockItem(rw) -> BlockItem(descend_itm attr rw)
  | BreakSemi as x -> x
  | CaretTilde _ as x -> x
  | CaseItm(rw_lst) -> CaseItm(descend_lst attr (rw_lst))
  | CaseStart (rw, rw_lst) -> CaseStart(descend_itm attr rw, descend_lst attr rw_lst)
  | CaseStart1 (rw) -> CaseStart1(descend_itm attr rw)
  | CaseStartInside (_, _) as x -> x
  | CaseStartUniq (_, _) as x -> x
  | CaseStartUniqInside (_, _) as x -> x
  | CaseStmt(rw_lst, rw_lst') -> CaseStmt(descend_lst attr (rw_lst), descend_lst attr (rw_lst'))
  | Cast (_, _) as x -> x
  | CellParamItem2 (_, _) as x -> x
  | CellParamItem3 (_, _) as x -> x
  | CellPinItem2 (_, _) as x -> x
  | CellPinItemImplied _ as x -> x
  | CellPinItemNC _ as x -> x
  | Concat(rw_lst) -> Concat(descend_lst attr (rw_lst))
  | CondGen1 (rw, rw2, rw3) -> CondGen1 (descend_itm attr rw, descend_itm attr rw2, descend_itm attr rw3)
  | ContAsgn lst -> ContAsgn (descend_lst attr lst)
  | DeclAsgn (_, _) as x -> x
  | DeclData (_, _) as x -> x
  | DeclInt2 _ as x -> x
  | DeclLogic(rw_lst) -> DeclLogic(descend_lst attr (rw_lst))
  | DeclLogic2 (_, _) as x -> x
  | DeclModPort(rw_lst) -> DeclModPort(descend_lst attr (rw_lst))
  | DeclReg(rw_lst, str1_lst, rw') -> DeclReg(descend_lst attr rw_lst, str1_lst, descend_itm attr rw')
  | Deflt -> Deflt
  | Div(rw, rw2) -> Div(descend_itm attr (rw), descend_itm attr (rw2))
  | Dot1(rw, rw2) -> Dot1(descend_itm attr (rw), descend_itm attr (rw2))
  | Dot3(str1, str2, str3) -> Dot3(str1, str2, str3)
  | DotBus (_, _, _, _) as x -> x
  | Edge(rw, rw2) -> Edge(descend_itm attr (rw), descend_itm attr (rw2))
  | ElabTask _ as x -> x
  | ElseStmt _ as x -> x
  | EnumInit (_, _) as x -> x
  | Equals(rw, rw2) -> Equals(descend_itm attr (rw), descend_itm attr (rw2))
  | Equals3 (_, _) as x -> x
  | EqualsQuery (_, _) as x -> x
  | Equate (_, _) as x -> x
  | EquateArrayField (_, _, _, _, _) as x -> x
  | EquateConcat _ as x -> x
  | EquateField (_, _, _) as x -> x
  | EquateSelect (_, _, _) as x -> x
  | EquateSelect2 (_, _, _) as x -> x
  | EquateSlice (rw, rw2, rw3, rw4) -> EquateSlice (descend_itm attr rw, descend_itm attr rw2, descend_itm attr rw3, descend_itm attr rw4)
  | EquateSlicePlus (rw, rw2, rw3, rw4) -> EquateSlicePlus (descend_itm attr rw, descend_itm attr rw2, descend_itm attr rw3, descend_itm attr rw4)
  | EventOr _ as x -> x
  | ExprOKL _ as x -> x
  | ExprQuote1 (_, _) as x -> x
  | Expression rw -> Expression(descend_itm attr rw)
  | Final _ as x -> x
  | FopAsgn (rw, rw') -> FopAsgn(descend_itm attr rw, descend_itm attr rw')
  | FopAsgn1 (_, _, _, _) as x -> x
  | FopAsgnArrayField (_, _, _) as x -> x
  | FopAsgnArrayField2 (_, _, _) as x -> x
  | FopAsgnArrayField3 (_, _, _, _) as x -> x
  | FopAsgnArrayField4 (_, _, _, _, _, _) as x -> x
  | FopAsgnArrayField5 (_, _, _, _, _) as x -> x
  | FopAsgnArrayField6 (_, _, _, _, _) as x -> x
  | FopAsgnArrayField7 (_, _, _, _, _) as x -> x
  | FopAsgnArrayField8 (_, _, _, _, _) as x -> x
  | FopAsgnArrayField9 (_, _, _, _, _, _) as x -> x
  | FopAsgnArrayMemSel (_, _, _, _) as x -> x
  | FopAsgnArrayRange (_, _, _, _) as x -> x
  | FopAsgnArrayRange2 (_, _, _, _) as x -> x
  | FopAsgnArraySel (s, rw, rw') -> FopAsgnArraySel (s, descend_itm attr rw, descend_itm attr rw')
  | FopAsgnArrayWid (_, _, _, _) as x -> x
  | FopAsgnConcat (_, _) as x -> x
  | ForEach (_, _) as x -> x
  | ForLoop(rw_lst, rw2, rw3, rw4) -> ForLoop(descend_lst attr (rw_lst), descend_itm attr (rw2), descend_itm attr (rw3), descend_itm attr (rw4))
  | FunDecl (_, _, _) as x -> x
  | FunGuts (_, _) as x -> x
  | FunRef (_, _) as x -> x
  | FunRef2 (_, _, _) as x -> x
  | GenBlock(rw_lst) -> GenBlock(descend_lst attr (rw_lst))
  | GenItem (_, _) as x -> x
  | Generate _ as x -> x
  | Genvar _ as x -> x
  | Greater(rw, rw2) -> Greater(descend_itm attr (rw), descend_itm attr (rw2))
  | GtEq(rw, rw2) -> GtEq(descend_itm attr (rw), descend_itm attr (rw2))
  | HyphenGt (_, _) as x -> x
  | Id id -> Id id
  | IdArrayed1 (_, _, _) as x -> x
  | IdArrayed2 (id, sel) -> IdArrayed2 (descend_itm attr id, descend_itm attr sel)
  | IdArrayed3 (_, _) as x -> x
  | IdArrayedColon (rw, rw2, rw3) -> IdArrayedColon (descend_itm attr rw, descend_itm attr rw2, descend_itm attr rw3)
  | IdArrayedHyphenColon (rw, rw2, rw3) -> IdArrayedHyphenColon (descend_itm attr rw, descend_itm attr rw2, descend_itm attr rw3)
  | IdArrayedPlusColon (rw, rw2, rw3) -> IdArrayedPlusColon (descend_itm attr rw, descend_itm attr rw2, descend_itm attr rw3)
  | If1(rw, rw2) -> If1(descend_itm attr (rw), descend_itm attr (rw2))
  | If2(rw, rw2, rw3) -> If2(descend_itm attr (rw), descend_itm attr (rw2), descend_itm attr (rw3))
  | Iff (_, _) as x -> x
  | Import _ as x -> x
  | In -> In
  | Inc _ as x -> x
  | InitPair (_, _) as x -> x
  | InitPat _ as x -> x
  | InitSig (rw, rw2) -> InitSig(descend_itm attr (rw), descend_itm attr (rw2))
  | Initial _ as x -> x
  | Inout -> Inout
  | InsideCase (_, _) as x -> x
  | InsideRange (_, _) as x -> x
  | InstArrayDecl (_, _, _, _, _) as x -> x
  | InstDecl (_, _, _) as x -> x
  | InstNameParen1 (_, _) as x -> x
  | InstNameParen2 (_, _) as x -> x
  | InstRange (_, _) as x -> x
  | IntfDecl (_, _, _, _) as x -> x
  | Intgr(int1) -> Intgr int1
  | ItemAsgn _ as x -> x
  | Itmlst(rw_lst) -> Itmlst(descend_lst attr (rw_lst:rw list))
  | Less(rw, rw2) -> Less(descend_itm attr (rw), descend_itm attr (rw2))
  | LocalParamTyp _ as x -> x
  | Logic(rw_lst, rw_lst2) -> Logic(descend_lst attr (rw_lst), descend_lst attr (rw_lst2))
  | LoopGen1 (rw, rw2, rw3, rw4, rw5, rw6) ->
       LoopGen1 (descend_itm attr (rw), (rw2), descend_itm attr (rw3), descend_itm attr (rw4),
               descend_itm attr (rw5), descend_lst attr (rw6))
  | LtEq(rw, rw2) -> LtEq(descend_itm attr (rw), descend_itm attr (rw2))
  | LtGt (_, _) as x -> x
  | Mod (_, _) as x -> x
  | ModPortItm (_, _) as x -> x
  | Modul(str1, rw_lst1, rw_lst2, rw_lst3) -> Modul(str1, descend_lst attr rw_lst1, descend_lst attr rw_lst2, descend_lst attr rw_lst3)
  | Mult(rw, rw2) -> Mult(descend_itm attr (rw), descend_itm attr (rw2))
  | Nand (_, _) as x -> x
  | Neg(str1) -> Neg (str1)
  | NetDecl (rw, rwlst) -> NetDecl(descend_itm attr (rw), descend_lst attr (rwlst))
  | NonBlocking(rw, rw2) -> NonBlocking(descend_itm attr rw, descend_itm attr rw2)
  | Nor (_, _) as x -> x
  | NotEq(rw, rw2) -> NotEq(descend_itm attr (rw), descend_itm attr (rw2))
  | NotEq3 (_, _) as x -> x
  | NotEqQuery (_, _) as x -> x
  | Number _ as n -> n
  | OpenRange _ as x -> x
  | Or(rw, rw2) -> Or(descend_itm attr (rw), descend_itm attr (rw2))
  | Or2(rw, rw2) -> Or2(descend_itm attr (rw), descend_itm attr (rw2))
  | Out -> Out
  | PackageBody (id, rw_lst) -> PackageBody (id, descend_lst attr rw_lst)
  | PackageParam _ as x -> x
  | PackageParam2 _ as x -> x
  | Param(str1, rw2, rw_lst2) -> Param(str1, descend_itm attr (rw2), descend_lst attr (rw_lst2))
  | ParamAsgn1 (_, _) as x -> x
  | ParamAsgn2 (_, _, _) as x -> x
  | ParamDecl (_, _) as x -> x
  | ParamPort _ as x -> x
  | PatMember1 (_, _) as x -> x
  | PatMemberDflt _ as x -> x
  | PkgImport _ as x -> x
  | PkgImportItm (_, _) as x -> x
  | Pling(rw) -> Pling(descend_itm attr (rw))
  | Port(rw, str1, rw_lst1, rw') -> Port(descend_itm attr rw, str1, descend_lst attr rw_lst1, descend_itm attr rw')
  | PortDir (_, _) as x -> x
  | PortFront (_, _) as x -> x
  | PortItem (_, _) as x -> x
  | PortItemFront (_, _) as x -> x
  | PortItemFront2 (_, _, _) as x -> x
  | PortsStar _ as x -> x
  | Pos(str1) -> Pos (str1)
  | PropertySpec as x -> x
  | Query(rw, rw2, rw3) -> Query(descend_itm attr rw, descend_itm attr rw2, descend_itm attr rw3)
  | RedAnd(rw) -> RedAnd(descend_itm attr (rw))
  | RedOr(rw) -> RedOr(descend_itm attr (rw))
  | RedXor(rw) -> RedXor(descend_itm attr (rw))
  | Repl(rw, rw_lst) -> Repl(descend_itm attr (rw), descend_lst attr (rw_lst))
  | Return _ as x -> x
  | SUDecl (_, _) as x -> x
  | SUMember (_, _) as x -> x
  | Seq (lbl, lst) -> Seq(lbl, descend_lst attr lst)
  | Shiftl(rw, rw2) -> Shiftl(descend_itm attr (rw), descend_itm attr (rw2))
  | Shiftr(rw, rw2) -> Shiftr(descend_itm attr (rw), descend_itm attr (rw2))
  | Shiftr3(rw, rw2) -> Shiftr3(descend_itm attr (rw), descend_itm attr (rw2))
  | SideEffect _ as x -> x
  | Signed(rw) -> Signed(descend_itm attr (rw))
  | StarStar(rw, rw2) -> StarStar(descend_itm attr (rw), descend_itm attr (rw2))
  | String _ as x -> x
  | Sub(rw, rw2) -> Sub(descend_itm attr (rw), descend_itm attr (rw2))
  | Sys (_, _) as x -> x
  | SysFuncCall (_, _) as x -> x
  | SysTaskCall (_, _) as x -> x
  | SysTaskRef _ as x -> x
  | TFBody (_, _) as x -> x
  | TF_port_decl (_, _, _) as x -> x
  | TF_variable (_, _, _, _) as x -> x
  | TaskDecl _ as x -> x
  | TaskRef (_, _) as x -> x
  | Tilde(rw) -> Tilde(descend_itm attr (rw))
  | TildeAnd(rw) -> TildeAnd(descend_itm attr (rw))
  | TildeOr(rw) -> TildeOr(descend_itm attr (rw))
  | Typ1 _ as x -> x
  | Typ10 (_, _, _) as x -> x
  | Typ2 (_, _, _) as x -> x
  | Typ3 (_, _) as x -> x
  | Typ4 (_, _, _, _) as x -> x
  | Typ5 (_, _) as x -> x
  | Typ6 _ as x -> x
  | Typ7 (_, _) as x -> x
  | Typ8 (_, _) as x -> x
  | Typ9 (_, _, _) as x -> x
  | TypEnum3 _ as x -> x
  | TypEnum4 (_, _, _) as x -> x
  | TypEnum5 _ as x -> x
  | TypEnum6 (_, _, _) as x -> x
  | TypParam (_, _, _) as x -> x
  | UMinus(rw) -> UMinus(descend_itm attr (rw))
  | UPlus _ as x -> x
  | Union (_, _) as x -> x
  | Unknown (str,rw_lst) -> Unknown (str, descend_lst attr (rw_lst:rw list))
  | Unsigned(rw) -> Unsigned(descend_itm attr (rw))
  | VNum _ as x -> x
  | ValueRange (_, _) as x -> x
  | VarDeclAsgn (_, _) as x -> x
  | VarDim _ as x -> x
  | While (_, _) as x -> x
  | WireExpr (_, _) as x -> x
  | Xnor(rw, rw2) -> Xnor(descend_itm attr (rw), descend_itm attr (rw2))
  | Xor(rw, rw2) -> Xor(descend_itm attr (rw), descend_itm attr (rw2))
  
and descend_lst attr x = List.map (fun x -> if verbose then print_endline (getstr x); attr.fn attr x) x

and descend_itm attr x = if verbose then print_endline (getstr x); attr.fn attr x

let rec descend (attr:attr) = function
  | Id _ as id -> (match Hashtbl.find_opt attr.subst id with None -> id | Some exp -> exp)
  | oth -> descend' {attr with fn=descend} oth

let parse_output_ast_from_chan ch =
  let lb = Lexing.from_channel ch in
  let output = try
      ml_start token lb
  with
    | Parsing.Parse_error ->
      let n = Lexing.lexeme_start lb in
      failwith (Printf.sprintf "Output.parse: parse error at character %d" n);
  in
  output

let parse arg =
  let ch = open_in arg in
  let rslt = parse_output_ast_from_chan ch in
  close_in ch;
  rslt
