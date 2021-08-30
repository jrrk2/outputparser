open Source_text_rewrite_types
open Source_text_lex
open Source_text

let rec rw = function
| CONS1 oth -> TLIST (rw oth::[])
| CONS3(lft,_,rght) -> (match rw lft with TLIST arg -> TLIST (rw rght :: arg) | oth -> TLIST (rw rght :: oth :: []))
| CONS4(lft,arg1,arg2,arg3) -> (match rw lft with TLIST arg -> TLIST (rw arg1 :: rw arg2 :: rw arg3 :: arg) | _ -> failwith "CONS4")
| CONS2(lft,rght) -> (match rw lft with TLIST arg -> TLIST (rw rght :: arg) | _ -> failwith "CONS2")
| ELIST lst -> ELIST (List.map rw lst)
| TLIST lst -> TLIST (List.map rw lst)
| TUPLE2(arg1,arg2) -> TUPLE2 (rw arg1, rw arg2)
| TUPLE4(STRING _ as arg0,LPAREN,arg,RPAREN) -> TUPLE4(arg0,LPAREN,rw arg,RPAREN)
| TUPLE4(STRING _,arg1,(PLUS|HYPHEN|STAR|SLASH|AMPERSAND|AMPERSAND_AMPERSAND|VBAR|VBAR_VBAR|EQ_EQ|PLING_EQ|LT_EQ|GT_EQ|LT_LT|GT_GT|GT_GT_GT|LESS|GREATER|CARET|CARET_TILDE|STAR_STAR as arg2),arg3) -> ELIST (flatten (flatten (arg1 :: arg2 :: arg3 :: [])))
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
| COLON|SEMICOLON|COMMA|CARET|TILDE|QUERY|QUOTE
| PLUS|HYPHEN|STAR|SLASH|HASH|PLING
| AMPERSAND|AMPERSAND_AMPERSAND|AMPERSAND_EQ
| GT_GT_GT|PLUS_COLON|PLUS_PLUS|COLON_COLON
| EQUALS|LT_EQ|VBAR_VBAR|LT_LT|GT_GT|GT_EQ|EQ_EQ|LESS|GREATER|VBAR
| TILDE_VBAR|TILDE_AMPERSAND
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
 
type attr = {subst: (string,rw)Hashtbl.t; fn: attr -> rw -> rw}

let rec descend' (attr:attr) = function
  | Id id -> Id id
  | Expression rw -> Expression(descend_itm attr rw)
  | Itmlst(rw_lst) -> Itmlst(descend_lst attr (rw_lst:rw list))
  | Unknown (str,rw_lst) -> Unknown (str, descend_lst attr (rw_lst:rw list))
  | In -> In
  | Out -> Out
  | Inout -> Inout
  | Modul(str1, rw_lst1, rw_lst2, rw_lst3) -> Modul(str1, descend_lst attr rw_lst1, descend_lst attr rw_lst2, descend_lst attr rw_lst3)
  | DeclReg(rw_lst, str1_lst, rw_lst_lst) -> DeclReg(descend_lst attr rw_lst, str1_lst,
    List.map (fun itm -> descend_lst attr itm) rw_lst_lst)
  | NonBlocking(rw, rw2) -> NonBlocking(descend_itm attr rw, descend_itm attr rw2)
  | Query(rw, rw2, rw3) -> Query(descend_itm attr rw, descend_itm attr rw2, descend_itm attr rw3)
  | Port(rw, str1, rw_lst1, rw_lst2) -> Port(descend_itm attr rw, str1, descend_lst attr rw_lst1, descend_lst attr rw_lst2)
  | Pos(str1) -> Pos (str1)
  | Neg(str1) -> Neg (str1)
  | Edge(rw, rw2) -> Edge(descend_itm attr (rw), descend_itm attr (rw2))
  | Intgr(int1) -> Intgr int1
  | Number _ as n -> n
  | UMinus(rw) -> UMinus(descend_itm attr (rw))
  | Pling(rw) -> Pling(descend_itm attr (rw))
  | Tilde(rw) -> Tilde(descend_itm attr (rw))
  | TildeAnd(rw) -> TildeAnd(descend_itm attr (rw))
  | TildeOr(rw) -> TildeOr(descend_itm attr (rw))
  | Equals(rw, rw2) -> Equals(descend_itm attr (rw), descend_itm attr (rw2))
  | NotEq(rw, rw2) -> NotEq(descend_itm attr (rw), descend_itm attr (rw2))
  | LtEq(rw, rw2) -> LtEq(descend_itm attr (rw), descend_itm attr (rw2))
  | GtEq(rw, rw2) -> GtEq(descend_itm attr (rw), descend_itm attr (rw2))
  | Less(rw, rw2) -> Less(descend_itm attr (rw), descend_itm attr (rw2))
  | Greater(rw, rw2) -> Greater(descend_itm attr (rw), descend_itm attr (rw2))
  | And(rw, rw2) -> And(descend_itm attr (rw), descend_itm attr (rw2))
  | And2(rw, rw2) -> And2(descend_itm attr (rw), descend_itm attr (rw2))
  | Or(rw, rw2) -> Or(descend_itm attr (rw), descend_itm attr (rw2))
  | Or2(rw, rw2) -> Or2(descend_itm attr (rw), descend_itm attr (rw2))
  | Xor(rw, rw2) -> Xor(descend_itm attr (rw), descend_itm attr (rw2))
  | Xnor(rw, rw2) -> Xnor(descend_itm attr (rw), descend_itm attr (rw2))
  | Shiftl(rw, rw2) -> Shiftl(descend_itm attr (rw), descend_itm attr (rw2))
  | Shiftr(rw, rw2) -> Shiftr(descend_itm attr (rw), descend_itm attr (rw2))
  | Shiftr3(rw, rw2) -> Shiftr3(descend_itm attr (rw), descend_itm attr (rw2))
  | Add(rw, rw2) -> Add(descend_itm attr (rw), descend_itm attr (rw2))
  | Sub(rw, rw2) -> Sub(descend_itm attr (rw), descend_itm attr (rw2))
  | Mult(rw, rw2) -> Mult(descend_itm attr (rw), descend_itm attr (rw2))
  | Div(rw, rw2) -> Div(descend_itm attr (rw), descend_itm attr (rw2))
  | StarStar(rw, rw2) -> StarStar(descend_itm attr (rw), descend_itm attr (rw2))
  | If2(rw, rw2, rw3) -> If2(descend_itm attr (rw), descend_itm attr (rw2), descend_itm attr (rw3))
  | If1(rw, rw2) -> If1(descend_itm attr (rw), descend_itm attr (rw2))
  | ForLoop(rw_lst, rw2, rw3, rw4) ->
    ForLoop(descend_lst attr (rw_lst), descend_itm attr (rw2), descend_itm attr (rw3), descend_itm attr (rw4))
  | CaseStmt(rw_lst, rw_lst') -> CaseStmt(descend_lst attr (rw_lst), descend_lst attr (rw_lst'))
  | CaseItm(rw_lst) -> CaseItm(descend_lst attr (rw_lst))
  | AlwaysComb(rw_lst) -> AlwaysComb(descend_lst attr (rw_lst))
  | BeginBlock(rw_lst) -> BeginBlock(descend_lst attr (rw_lst))
  | Bitlst(rw_lst) -> Bitlst(descend_lst attr (rw_lst))
  | Dot1(str1, rw2) -> Dot1(str1, descend_itm attr (rw2))
  | Unsigned(rw) -> Unsigned(descend_itm attr (rw))
  | Signed(rw) -> Signed(descend_itm attr (rw))
  | Concat(rw_lst) -> Concat(descend_lst attr (rw_lst))
  | DeclModPort(rw_lst) -> DeclModPort(descend_lst attr (rw_lst))
  | Repl(rw, rw_lst) -> Repl(descend_itm attr (rw), descend_lst attr (rw_lst))
  | RedAnd(rw) -> RedAnd(descend_itm attr (rw))
  | RedOr(rw) -> RedOr(descend_itm attr (rw))
  | RedXor(rw) -> RedXor(descend_itm attr (rw))
  | Dot3(str1, str2, str3) -> Dot3(str1, str2, str3)
  | Logic(rw_lst, rw_lst2) -> Logic(descend_lst attr (rw_lst), descend_lst attr (rw_lst2))
  | Param(str1, rw2, rw_lst2) -> Param(str1, descend_itm attr (rw2), descend_lst attr (rw_lst2))
  | DeclLogic(rw_lst) -> DeclLogic(descend_lst attr (rw_lst))
  | GenBlock(rw_lst) -> GenBlock(descend_lst attr (rw_lst))
  | PackageBody (id, rw_lst) -> PackageBody (id, descend_lst attr rw_lst)
  | Typ1 _ as x -> x
  | Cast (_, _) as x -> x
  | Deflt -> Deflt
  | TypEnum3 _ as x -> x
  | InsideCase (_, _) as x -> x
  | InsideRange (_, _) as x -> x
  | TypParam (_, _, _) as x -> x
  | Import _ as x -> x
  | InitPair (_, _) as x -> x
  | SideEffect _ as x -> x
  | (Assert|AssertProperty|AtStar|BreakSemi|PropertySpec|AlwaysComb2 _|
AlwaysFF (_, _)|AlwaysLatch _|AlwaysLegacy (_, _)|And3 (_, _)|
AnyRange (_, _)|Asgn1 (_, _)|AsgnPat _|At _|Atom _|AutoFunDecl (_, _, _)|
CaseStart (_, _)|CaseStart1 _|CaseStartInside (_, _)|CaseStartUniq (_, _)|
CaseStartUniqInside (_, _)|CellParamItem1 (_, _)|CellParamItem2 (_, _)|
CellParamItem3 (_, _)|CellPinItem1 (_, _)|CellPinItem2 (_, _)|
CellPinItemImplied _|CellPinItemNC _|CondGen1 (_, _, _)|ContAsgn _|
DeclAsgn (_, _)|DeclData (_, _)|DeclInt2 _|DeclLogic2 (_, _)|DeclReg2 (_, _)|
DotBus (_, _, _, _)|ElabTask _|ElseStmt _|EnumInit (_, _)|
Equals3 (_, _)|EqualsQuery (_, _)|Equate (_, _)|
EquateArrayField (_, _, _, _, _)|EquateField (_, _, _)|
EquateSelect (_, _, _)|EquateSelect2 (_, _, _)|EquateSlice (_, _, _, _)|
EventOr _|ExprOKL _|ExprQuote1 (_, _)|Final _|FopAsgn (_, _)|
FopAsgn1 (_, _, _, _)|FopAsgnArrayField (_, _, _)|
FopAsgnArrayField2 (_, _, _)|FopAsgnArrayField3 (_, _, _, _)|
FopAsgnArrayField4 (_, _, _, _, _, _)|FopAsgnArrayField5 (_, _, _, _, _)|
FopAsgnArrayField6 (_, _, _, _, _)|FopAsgnArrayField7 (_, _, _, _, _)|
FopAsgnArrayMemSel (_, _, _, _)|FopAsgnArrayRange (_, _, _, _)|
FopAsgnArrayRange2 (_, _, _, _)|FopAsgnArraySel (_, _, _)|
FopAsgnArrayWid (_, _, _, _)|FopAsgnConcat (_, _)|ForEach (_, _)|
FunDecl (_, _, _)|FunGuts (_, _)|FunRef (_, _)|FunRef2 (_, _, _)|
GenItem (_, _)|Generate _|HyphenGt (_, _)|IdArrayed1 (_, _, _)|
IdArrayed2 (_, _)|IdArrayed3 (_, _)|IdArrayedColon (_, _, _)|
IdArrayedHyphenColon (_, _, _)|IdArrayedPlusColon (_, _, _)|Iff (_, _)|
Inc _|InitPat _|InitSig (_, _)|Initial _|InstDecl (_, _, _)|
InstNameParen1 (_, _)|InstNameParen2 (_, _)|
InstRange (_, _)|IntfDecl (_, _, _, _)|ItemAsgn _|LocalParamTyp _|
LoopGen1 (_, _, _, _, _, _)|LtGt (_, _)|Mod (_, _)|ModPortItm (_, _)|
Nand (_, _)|NetDecl (_, _)|Nor (_, _)|NotEq3 (_, _)|NotEqQuery (_, _)|
OpenRange _|ParamAsgn1 (_, _)|ParamAsgn2 (_, _, _)|ParamDecl (_, _)|
ParamPort _|PatMember1 (_, _)|PatMemberDflt _|PkgImport _|
PkgImportItm (_, _)|PortDir (_, _)|PortFront (_, _)|PortItem (_, _)|
PortItemFront (_, _)|PortsStar _|Return _|SUDecl (_, _)|SUMember (_, _)|Seq (_, _)|
Blocking _|String _|Sys (_, _)|SysFuncCall (_, _)|SysTaskCall (_, _)|
TaskBody (_, _)|TaskRef (_, _)|Typ2 (_, _, _)|Typ3 (_, _)|Typ4 (_, _, _, _)|
Typ5 (_, _)|Typ6 _|Typ7 (_, _)|Typ8 (_, _)|Typ9 (_, _, _)|Typ10 (_, _, _)|
TypEnum4 (_, _, _)|TypEnum5 _|TypEnum6 (_, _, _)|UPlus _|Union (_, _)|
VNum _|ValueRange (_, _)|VarDeclAsgn (_, _)|VarDim _|While (_, _)|
WireExpr (_, _)|PackageParam _|PackageParam2 _) as x -> x

and descend_lst attr x = List.map (attr.fn attr) x

and descend_itm attr x = attr.fn attr x

let rec descend (attr:attr) = function
  | Id id -> (match Hashtbl.find_opt attr.subst id with None -> Id id | Some exp -> exp)
  | oth -> descend' {attr with fn=descend} oth

let iter attr ix strt stop inc stmts = 
    let loopvar = ref strt in
    let block = ref [] in
    while (!loopvar <= stop) do
      begin
	Hashtbl.replace attr.subst ix (Intgr !loopvar);
	block := descend_itm attr stmts :: !block;
	loopvar := !loopvar + inc;
      end
    done;
    BeginBlock (List.rev !block)

let rec unroll (attr:attr) = function
  | Id id -> (match Hashtbl.find_opt attr.subst id with None -> Id id | Some exp -> exp)
  | ForLoop(rw_lst, rw2, rw3, rw4) ->
    begin 
    match rw_lst, rw2, rw3 with
 (*
     | Blocking (Id ix, Intgr strt) :: [], LtEq (Id ix', Intgr stop), Inc (Id ix'') ->
	iter attr ix strt stop 1 rw4
	*)
     | _ -> ForLoop(descend_lst attr (rw_lst),
	    descend_itm attr (rw2),
	    descend_itm attr (rw3),
	    descend_itm attr (rw4))
    end
  | oth -> descend' {attr with fn=unroll} oth

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

let rewrite_vhdl v =
  let p = parse v in
  let p' = rw p in
  let x = Matchmly.mly p' in
  let fd = open_out (v^"_dump.vhd") in 
  let modlst = ref [] in
  List.iter (fun (k, x) ->
		modlst := k :: !modlst;
		Dump_vhdl.template fd Matchmly.modules x;
		) !(Matchmly.modules);
  close_out fd;
  let modlst = !modlst in
  modlst, x, p, p'

let rewrite_sysver v =
  let p = parse v in
  let p' = rw p in
  let x = Matchmly.mly p' in
  let fd = open_out (v^"_dump.sv") in 
  let modlst = ref [] in
  List.iter (fun (k, x) ->
		modlst := k :: !modlst;
		Dump_sysver.template fd Matchmly.modules x;
		) !(Matchmly.modules);
  close_out fd;
  let modlst = !modlst in
  modlst, x, p, p'
