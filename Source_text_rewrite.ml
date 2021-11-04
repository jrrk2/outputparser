open Source_text_rewrite_types
open Source_text_lex
open Source_text

let verbose = try int_of_string (Sys.getenv ("DESCEND_VERBOSE")) > 0 with _ -> false

let getstr x = Sexplib.Sexp.to_string (sexp_of_rw x)

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
| DLR_display|DLR_stop|DLR_finish|DLR_write|DLR_countones
| DLR_signed|DLR_unsigned|DLR_time|DLR_readmemh|DLR_clog2|DLR_bits|DLR_error
| Module|Always|Assign|Reg|Wire|Logic|Bit|Int|Integer
| Unsigned|Signed|Real
| Output|Input|Posedge|Negedge|Or|DOT|Wand|Wor
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
| Specparam | Specify | Endspecify
| Static
| HYPHEN_COLON
| HYPHEN_EQ
| FLOATING_HYPHEN_POINT_NUMBER _
| EOF_TOKEN) as oth) -> oth
| oth -> failwith ("rw fail: "^Source_text_types.getstr oth)

and flatten lst = List.flatten (List.map (function ELIST lst -> List.map rw lst | oth -> [rw oth]) lst)
 
type attr = {subst: (rw,rw)Hashtbl.t; fn: attr -> rw -> rw; pth: string; pkg: string}

let rec descend' (attr:attr) = function
  | Active(vtyp1, rw2, rw3) -> Active(vtyp1, descend_itm attr rw2, descend_itm attr rw3)
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
  | DeclData(rw1, rw2, rw_lst3) -> DeclData(descend_itm attr rw1, descend_itm attr rw2, descend_lst attr rw_lst3)
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
  | FopAsgnArrayField10(rw1, rw2, rw3, rw4) -> FopAsgnArrayField10(descend_itm attr rw1, descend_itm attr rw2, descend_itm attr rw3, descend_itm attr rw4)
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
  | Hash(rw1) -> Hash(descend_itm attr rw1)
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
  | PackageRef(string1, string2) -> PackageRef(string1, string2)
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
  | Split(rw1, rw2) -> Split(descend_itm attr rw1, descend_itm attr rw2)
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
  | TaskRef2(string1, rw2) -> TaskRef2(string1, descend_itm attr rw2)
  | Tilde(rw1) -> Tilde(descend_itm attr rw1)
  | TildeAnd(rw1) -> TildeAnd(descend_itm attr rw1)
  | TildeOr(rw1) -> TildeOr(descend_itm attr rw1)
  | Typ1(string1) -> Typ1(string1)
  | Typ10(string1, rw_lst2, string3) -> Typ10(string1, descend_lst attr rw_lst2, string3)
  | Typ11(rw1, rw_lst2, rw_lst3) -> Typ11(descend_itm attr rw1, descend_lst attr rw_lst2, descend_lst attr rw_lst3)
  | Typ12(rw_lst1, rw2, rw_lst3) -> Typ12(descend_lst attr rw_lst1, descend_itm attr rw2, descend_lst attr rw_lst3)
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
  | TypEnum6(string1, rw2, rw_lst3) -> TypEnum6(string1, descend_itm attr rw2, (* descend_lst attr *) rw_lst3)
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

(* for plain Verilog reading without pre-proc only *)

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

(* for preprocessed input *)

let parse_output_ast_from_function f =
  let lb = Lexing.from_function f in
  let output = try
      ml_start token lb
  with
    | Parsing.Parse_error ->
      let n = Lexing.lexeme_start lb in
      failwith (Printf.sprintf "Output.parse: parse error at character %d" n);
  in
  output

(* for Verilator pre-processed input *)

let parse_output_ast_from_pipe v =
  let ch = Unix.open_process_in ("verilator -E "^v) in
  let lb = Lexing.from_channel ch in
  let output = try
      ml_start token lb
  with
    | Parsing.Parse_error ->
      let n = Lexing.lexeme_start lb in
      failwith (Printf.sprintf "Output.parse: parse error at character %d" n);
  in
  output
