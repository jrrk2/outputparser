open Source_text_rewrite_types
open Source_text_lex
open Source_text

let modules = ref []
let mlst = ref []
let attlst = ref []
let othpat = ref None
let othpat1 = ref None
let othpat2 = ref None
let othpat3 = ref None
let othpat4 = ref None
let othpat5 = ref None
(*
 let othpat6 = ref None
let missing = ref None
let caseref = ref None
 *)
let othpat7 = ref None
let othport = ref None
let othport3 = ref None
let priorlst = ref None
let remap = ref None

let g str pat =
let len = String.length pat in
if String.length str < len then false
else let substr = String.sub str 0 len in
substr = pat

let enc_expr = function
| AMPERSAND -> STRING "expr1136"
| AMPERSAND_AMPERSAND -> STRING "expr1130"
| AMPERSAND_AMPERSAND_AMPERSAND -> STRING "expr1172"
| CARET -> STRING "expr1138"
| CARET_TILDE -> STRING "expr1139"
| DOT -> STRING "expr1157"
(*
 | DOT -> STRING "expr1158"
*)
| EQ_EQ -> STRING "expr1124"
| EQ_EQ_EQ -> STRING "expr1126"
| EQ_EQ_QUERY -> STRING "expr1128"
| GREATER -> STRING "expr1134"
| GT_EQ -> STRING "expr1135"
| GT_GT -> STRING "expr1143"
| GT_GT_GT -> STRING "expr1144"
| HYPHEN -> STRING "expr1120"
| HYPHEN_GT -> STRING "expr1146"
| LESS -> STRING "expr1133"
| LT_EQ -> STRING "expr1147"
| LT_HYPHEN_GT -> STRING "expr1145"
| LT_LT -> STRING "expr1142"
| PERCENT -> STRING "expr1123"
| PLING_EQ -> STRING "expr1125"
| PLING_EQ_EQ -> STRING "expr1127"
| PLING_EQ_QUERY -> STRING "expr1129"
| PLUS -> STRING "expr1119"
| SLASH -> STRING "expr1122"
| STAR -> STRING "expr1121"
| STAR_STAR -> STRING "expr1132"
| TILDE_AMPERSAND -> STRING "expr1141"
| TILDE_VBAR -> STRING "expr1140"
| VBAR -> STRING "expr1137"
| VBAR_VBAR -> STRING "expr1131"
| oth -> STRING "expr????"

let pred1 = function
  | TUPLE9 (STRING stmt, _, _, _, _, _, _, _, _) when g stmt "statementFor" -> true
  | TUPLE9 (STRING stmt, _, _, _, _, _, _, _, _) when g stmt "statement_item" -> true
  | TUPLE8 (STRING stmt, _, _, _, _, _, _, _) when g stmt "statement_item" -> true
  | TUPLE7 (STRING stmt, _, _, _, _, _, _) when g stmt "statement_item" -> true
  | TUPLE6 (STRING stmt, _, _, _, _, _) when g stmt "statement_item" -> true
  | TUPLE5 (STRING stmt, _, _, _, _) when g stmt "statement_item" -> true
  | TUPLE4 (STRING stmt, _, _, _) when g stmt "statement_item" -> true
  | TUPLE3 (STRING stmt, _, _) when g stmt "statement_item" -> true
  | TUPLE2 (STRING stmt, _) when g stmt "statement_item" -> true
  | TUPLE5 (STRING "seq_block615", _, _, _, _) -> true
  | SEMICOLON -> true
  | _ -> false

let canfail = ref true

let widthnum (str:string) =
let base = ref 10
and width = ref 0
and value = ref 0
and basing = ref 0
and converting = ref true in
for idx = 0 to String.length(str)-1 do let ch = Char.lowercase_ascii(str.[idx]) in begin
match ch with
| '_' -> ()
| '?' -> value := !value * !base
| '\'' -> converting := false; basing := idx+1;
| '0'..'9' -> if (!converting) then
    width := (!width * !base) + int_of_char(ch) - int_of_char('0')
else
    value := (!value * !base) + int_of_char(ch) - int_of_char('0')
| 'a'..'z' ->  if (!converting) then
    width := (!width * !base) + int_of_char(ch) - int_of_char('a') + 10
else if (!basing==idx) then begin match ch with
  | 'b' -> base := 2
  | 'd' -> base := 10
  | 'o' -> base := 8
  | 'h' -> base := 16
  | _ -> value := int_of_char(ch) - int_of_char('a') + 10
end else
    value := (!value * !base) + int_of_char(ch) - int_of_char('a') + 10;
| _ -> converting := false; width := 0
end
done;
if (!basing == 0) then begin
  value := !width;
  width := 32;
end;
Number (!base, !width, !value, (String.sub str (1 + !basing) (String.length str - 1 - !basing)))

let rec event_lst = function
| EventOr lst, EventOr lst' -> EventOr (lst @ lst')
| EventOr lst, oth -> EventOr (lst @ [ oth ] )
| oth, EventOr lst -> EventOr (oth :: lst)
| oth, oth' -> EventOr (oth :: [oth'])

let dbgportitms = ref []

let rec mly = function
| Wire -> Atom "wire"
(* *)
 | Reg -> Atom "reg"
(* *)
| Logic -> Atom "logic"
| Void -> Atom "void"
| Byte -> Atom "byte"
| Int -> Atom "int"
| Integer -> Atom "integer"
| Longint -> Atom "longint"
| Unsigned -> Atom "unsigned"
| String -> Atom "string"
| Signed -> Atom "signed"
| Bit -> Atom "bit"
| Real -> Atom "real"
| Wand -> Atom "wand"
| Wor -> Atom "wor"
| Default -> Atom "default"
| Packed -> Atom "packed"
| Input -> In
| Output -> Out
| Inout -> Inout
| EMPTY_TOKEN -> Deflt
| IDENTIFIER id -> Id id
| STRING s -> String s
| FLOATING_HYPHEN_POINT_NUMBER f -> Float f
| VBAR -> Atom "|"
| PLUS -> Atom "++"
| PLUS_PLUS -> Atom "++"
| HYPHEN_HYPHEN -> Atom "--"
| STAR -> Atom "*"
| SLASH -> Atom "/"
| DOT_STAR -> Atom ".*"
| COLON -> Atom ":"
| HYPHEN -> Atom "-"
| CARET -> Atom "^"
| LESS -> Atom "<"
| GREATER -> Atom ">"
| SEMICOLON -> Atom ";"
| AMPERSAND -> Atom "&"
| EQ_EQ -> Atom "=="
| LT_LT -> Atom "<<"
| LT_EQ -> Atom "<="
| GT_GT -> Atom ">>"
| GT_EQ -> Atom ">="
| STAR_STAR -> Atom "**"
| PLING_EQ -> Atom "!="
| VBAR_VBAR -> Atom "||"
| GT_GT_GT -> Atom ">>>"
| LT_LT_EQ -> Atom "<<="
| AMPERSAND_AMPERSAND -> Atom "&&"
| INTEGER_NUMBER n -> widthnum n
| DLR_finish -> Atom "$finish"
| DLR_display -> Atom "$display"
| DLR_write -> Atom "$write"
| DLR_stop -> Atom "$stop"
| DLR_readmemh -> Atom "$readmemh"
| TUPLE3(STRING("assignment_pattern769"), QUOTE_LBRACE, RBRACE) as oth -> mayfail oth  "assignment_pattern769"
| TUPLE3(STRING("attr_event_control592"), AT, STAR) as oth -> mayfail oth  "attr_event_control592"
| TUPLE3(STRING("block_item_declaration637"), arg1, SEMICOLON) -> BlockItem (mly arg1)
| TUPLE3(STRING("caseAttrE725"), arg1, SLASH_STAR_verilator_full_case_STAR_SLASH) as oth -> mayfail oth  "caseAttrE725"
| TUPLE3(STRING("caseAttrE726"), arg1, SLASH_STAR_verilator_parallel_case_STAR_SLASH) as oth -> mayfail oth  "caseAttrE726"
| TUPLE3(STRING("case_generate_item485"), Default, arg2) as oth -> mayfail oth  "case_generate_item485"
| TUPLE3(STRING("cellparamItemE576"), DOT, arg2) as oth -> mayfail oth  "cellparamItemE576"
| TUPLE3(STRING("cellparamItemE577"), DOT, arg2) as oth -> mayfail oth  "cellparamItemE577"
| TUPLE3(STRING("cellpinItemE583"), DOT, arg2) as oth -> mayfail oth  "cellpinItemE583"
| TUPLE3(STRING("cellpinItemE584"), DOT, arg2) -> CellPinItemImplied(match arg2 with IDENTIFIER id -> id | oth -> failwith "cellpinItemE584")
| TUPLE3(STRING("classExtendsE2568"), Extends, arg2) as oth -> mayfail oth  "classExtendsE2568"
| TUPLE3(STRING("classImplementsE2574"), Implements, arg2) as oth -> mayfail oth  "classImplementsE2574"
| TUPLE3(STRING("class_item2607"), arg1, SEMICOLON) as oth -> mayfail oth  "class_item2607"
| TUPLE3(STRING("class_item2609"), ERROR_TOKEN, SEMICOLON) as oth -> mayfail oth  "class_item2609"
| TUPLE3(STRING("class_method2610"), arg1, arg2) as oth -> mayfail oth  "class_method2610"
| TUPLE3(STRING("class_method2611"), arg1, arg2) as oth -> mayfail oth  "class_method2611"
| TUPLE3(STRING("class_new713"), New, arg2) as oth -> mayfail oth  "class_new713"
| TUPLE3(STRING("class_property344"), arg1, arg2) as oth -> mayfail oth  "class_property344"
| TUPLE3(STRING("class_property345"), arg1, arg2) as oth -> mayfail oth  "class_property345"
| TUPLE3(STRING("class_property346"), arg1, arg2) as oth -> mayfail oth  "class_property346"
| TUPLE3(STRING("class_typeExtImpOne2580"), arg1, arg2) as oth -> mayfail oth  "class_typeExtImpOne2580"
| TUPLE3(STRING("class_typeExtImpOne2581"), DLR_unit, COLON_COLON) as oth -> mayfail oth  "class_typeExtImpOne2581"
| TUPLE3(STRING("class_typeExtImpOne2582"), Local_HYPHEN_then_HYPHEN_COLON_COLON, COLON_COLON) as oth -> mayfail oth  "class_typeExtImpOne2582"
| TUPLE3(STRING("commaVRDListE1355"), COMMA, TLIST lst) -> Itmlst (rml lst)
| TUPLE3(STRING("commaVRDListE1355"), COMMA, arg2) -> failwith "commaVRDListE1355"
| TUPLE3(STRING("constraint_expression2640"), arg1, SEMICOLON) as oth -> mayfail oth  "constraint_expression2640"
| TUPLE3(STRING("data_declarationVarFront350"), Var, arg2) as oth -> mayfail oth  "data_declarationVarFront350"
| TUPLE3(STRING("data_declarationVarFront356"), arg1, arg2) as oth -> mayfail oth  "data_declarationVarFront356"
| TUPLE3(STRING("data_declarationVarFrontClass359"), Var, arg2) as oth -> mayfail oth  "data_declarationVarFrontClass359"
| TUPLE3(STRING("data_typeBasic264"), arg1, arg2) ->
  (match arg1, arg2 with
    | (Byte|Int|Integer|Longint), (EMPTY_TOKEN|Signed|Unsigned) -> Typ8(mly arg1, mly arg2)
    | oth -> othpat2 := Some oth; failwith "data_typeBasic264")
| TUPLE3(STRING("data_typeNoRef267"), arg1, arg2) -> Typ8(mly arg1, mly arg2)
| TUPLE3(STRING("data_typeNoRef273"), Virtual_HYPHEN_then_HYPHEN_identifier, arg2) as oth -> mayfail oth  "data_typeNoRef273"
| TUPLE3(STRING("delay_control492"), HASH, arg2) as oth -> mayfail oth  "delay_control492"
| TUPLE3(STRING("delay_value496"), arg1, arg2) as oth -> mayfail oth  "delay_value496"
| TUPLE3(STRING("dpi_importLabelE1099"), arg1, EQUALS) as oth -> mayfail oth  "dpi_importLabelE1099"
| TUPLE3(STRING("elaboration_system_task1006"), arg1, SEMICOLON) -> ElabTask(mly arg1)
| TUPLE3(STRING("elaboration_system_task_guts1007"), DLR_info, arg2) as oth -> mayfail oth  "elaboration_system_task_guts1007"
| TUPLE3(STRING("elaboration_system_task_guts1009"), DLR_warning, arg2) as oth -> mayfail oth  "elaboration_system_task_guts1009"
| TUPLE3(STRING("elaboration_system_task_guts1011"), DLR_error, arg2) as oth -> mayfail oth  "elaboration_system_task_guts1011"
| TUPLE3(STRING("elaboration_system_task_guts1013"), DLR_fatal, arg2) as oth -> mayfail oth  "elaboration_system_task_guts1013"
| TUPLE3(STRING("elseStmtBlock2553"), Else, arg2) -> ElseStmt(mly arg2)
| TUPLE3(STRING("endLabelE2519"), COLON, arg2) as oth -> mayfail oth  "endLabelE2519"
| TUPLE3(STRING("endLabelE2520"), COLON, New) as oth -> mayfail oth  "endLabelE2520"
| TUPLE3(STRING("enumNameStartE338"), EQUALS, arg2) as oth -> mayfail oth  "enumNameStartE338"
| TUPLE3(STRING("enum_base_typeE325"), arg1, arg2) as oth -> mayfail oth  "enum_base_typeE325"
| TUPLE3(STRING("enum_base_typeE327"), arg1, arg2) -> Typ8(mly arg1, mly arg2)
| TUPLE3(STRING("enum_base_typeE329"), arg1, arg2) as oth -> mayfail oth  "enum_base_typeE329"
| TUPLE3(STRING("event_control595"), AT, STAR) -> AtStar
| TUPLE3(STRING("event_control596"), AT, arg2) as oth -> mayfail oth  "event_control596"
| TUPLE3(STRING("expr1108"), PLUS, arg2) -> UPlus (mly arg2)
| TUPLE3(STRING("expr1109"), HYPHEN, arg2) -> UMinus (mly arg2)
| TUPLE3(STRING("expr1110"), PLING, arg2) -> Pling (mly arg2)
| TUPLE3(STRING("expr1111"), AMPERSAND, arg2) -> RedAnd(mly arg2)
| TUPLE3(STRING("expr1112"), TILDE, arg2) -> Tilde (mly arg2)
| TUPLE3(STRING("expr1113"), VBAR, arg2) -> RedOr(mly arg2)
| TUPLE3(STRING("expr1114"), CARET, arg2) -> RedXor(mly arg2)
| TUPLE3(STRING("expr1115"), TILDE_AMPERSAND, arg2) -> TildeAnd(mly arg2)
| TUPLE3(STRING("expr1116"), TILDE_VBAR, arg2) -> TildeOr(mly arg2)
| TUPLE3(STRING("expr1117"), CARET_TILDE, arg2) -> CaretTilde(mly arg2)
| TUPLE3(STRING("expr1154"), LBRACE, RBRACE) as oth -> mayfail oth  "expr1154"
| TUPLE3(STRING("exprEqE1104"), EQUALS, arg2) as oth -> mayfail oth  "exprEqE1104"
| TUPLE3(STRING("exprNoStr1240"), PLUS, arg2) as oth -> mayfail oth  "exprNoStr1240"
| TUPLE3(STRING("exprNoStr1241"), HYPHEN, arg2) as oth -> mayfail oth  "exprNoStr1241"
| TUPLE3(STRING("exprNoStr1242"), PLING, arg2) as oth -> mayfail oth  "exprNoStr1242"
| TUPLE3(STRING("exprNoStr1243"), AMPERSAND, arg2) as oth -> mayfail oth  "exprNoStr1243"
| TUPLE3(STRING("exprNoStr1244"), TILDE, arg2) as oth -> mayfail oth  "exprNoStr1244"
| TUPLE3(STRING("exprNoStr1245"), VBAR, arg2) as oth -> mayfail oth  "exprNoStr1245"
| TUPLE3(STRING("exprNoStr1246"), CARET, arg2) as oth -> mayfail oth  "exprNoStr1246"
| TUPLE3(STRING("exprNoStr1247"), TILDE_AMPERSAND, arg2) as oth -> mayfail oth  "exprNoStr1247"
| TUPLE3(STRING("exprNoStr1248"), TILDE_VBAR, arg2) as oth -> mayfail oth  "exprNoStr1248"
| TUPLE3(STRING("exprNoStr1249"), CARET_TILDE, arg2) as oth -> mayfail oth  "exprNoStr1249"
| TUPLE3(STRING("exprNoStr1286"), LBRACE, RBRACE) as oth -> mayfail oth  "exprNoStr1286"
| TUPLE3(STRING("exprOkLvalue1312"), arg1, arg2) as oth -> mayfail oth  "exprOkLvalue1312"
| TUPLE3(STRING("exprOrDataTypeEqE1106"), EQUALS, arg2) ->
(match arg2 with
       | IDENTIFIER member -> Id member
       | INTEGER_NUMBER n -> mly arg2
       | FLOATING_HYPHEN_POINT_NUMBER f -> mly arg2
       | TUPLE4 (STRING ("expr1119"|"expr1120"|"expr1121"|"expr1122"|"expr1124"|"expr1136"|"expr1142"|"expr1143"|"expr1132"|"expr1137"|"expr1159"|"exprScope1329"), _, _, _) -> mly arg2
       | TUPLE6 (STRING ("expr1148"|"expr1162"|"expr1165"|"funcRef788"), _, _, _, _, _) -> mly arg2
       | TUPLE7 (STRING ("expr1155"), _, _, _, _, _, _) -> mly arg2
       | TUPLE5 (STRING "system_f_call_or_t925", _, _, _, _) -> mly arg2
       | TUPLE4 (STRING "exprOkLvalue1307", LBRACE, TLIST lst, RBRACE) -> ExprOKL (rml lst)
       | TUPLE4 (STRING "assignment_pattern767", QUOTE_LBRACE, TLIST lst, RBRACE) -> InitPat (rml lst)
       | TUPLE4 (STRING "assignment_pattern768", QUOTE_LBRACE, TLIST lst, RBRACE) -> InitPat (rml lst)
       | TUPLE3 (STRING ("expr1109"|"exprScope1328"), _, _) -> mly arg2
       | ELIST lst -> priority lst
       | oth -> othpat1 := Some oth; failwith "exprOrDataTypeEqE1106")
| TUPLE3(STRING("exprScope1328"), arg1, arg2) ->
(match arg1, arg2 with
       | TLIST lst, IDENTIFIER member -> (match rml lst with PackageBody (id_cc, []) :: [] -> PackageBody (id_cc, Id member :: []) | _ -> failwith "exprScope1328")
       | TLIST lst, TUPLE7  (STRING "idArrayed2504", _, _, _, _, _, _) -> IdArrayed3(rml lst, mly arg2)
       | oth -> othpat2 := Some oth; failwith "exprScope1328")
| TUPLE3(STRING("fIdScoped1051"), arg1, arg2) as oth -> mayfail oth  "fIdScoped1051"
| TUPLE3(STRING("fexpr1174"), PLUS, arg2) as oth -> mayfail oth  "fexpr1174"
| TUPLE3(STRING("fexpr1175"), HYPHEN, arg2) as oth -> mayfail oth  "fexpr1175"
| TUPLE3(STRING("fexpr1176"), PLING, arg2) as oth -> mayfail oth  "fexpr1176"
| TUPLE3(STRING("fexpr1177"), AMPERSAND, arg2) as oth -> mayfail oth  "fexpr1177"
| TUPLE3(STRING("fexpr1178"), TILDE, arg2) as oth -> mayfail oth  "fexpr1178"
| TUPLE3(STRING("fexpr1179"), VBAR, arg2) as oth -> mayfail oth  "fexpr1179"
| TUPLE3(STRING("fexpr1180"), CARET, arg2) as oth -> mayfail oth  "fexpr1180"
| TUPLE3(STRING("fexpr1181"), TILDE_AMPERSAND, arg2) as oth -> mayfail oth  "fexpr1181"
| TUPLE3(STRING("fexpr1182"), TILDE_VBAR, arg2) as oth -> mayfail oth  "fexpr1182"
| TUPLE3(STRING("fexpr1183"), CARET_TILDE, arg2) as oth -> mayfail oth  "fexpr1183"
| TUPLE3(STRING("fexpr1220"), LBRACE, RBRACE) as oth -> mayfail oth  "fexpr1220"
| TUPLE3(STRING("fexprOkLvalue1321"), arg1, arg2) as oth -> mayfail oth  "fexprOkLvalue1321"
| TUPLE3(STRING("fexprScope1335"), arg1, arg2) as oth -> mayfail oth  "fexprScope1335"
| TUPLE3(STRING("final_construct429"), Final, arg2) -> Final(match arg2 with TUPLE5 (STRING "seq_block615", Begin, TLIST lst, End, EMPTY_TOKEN) -> rml lst | _ -> mly arg2 :: [])
| TUPLE3(STRING("final_zero2537"), HASH, INTEGER_NUMBER arg2) as oth -> mayfail oth  "final_zero2537"
| TUPLE3(STRING("finc_or_dec_expression708"), arg1, PLUS_PLUS) -> SideEffect(mly arg1, mly PLUS_PLUS)
| TUPLE3(STRING("finc_or_dec_expression709"), arg1, HYPHEN_HYPHEN) -> SideEffect(mly arg1, mly HYPHEN_HYPHEN)
| TUPLE3(STRING("finc_or_dec_expression710"), PLUS_PLUS, arg2) -> SideEffect(mly PLUS_PLUS, mly arg2)
| TUPLE3(STRING("finc_or_dec_expression711"), HYPHEN_HYPHEN, arg2) -> SideEffect(mly HYPHEN_HYPHEN, mly arg2)
| TUPLE3(STRING("for_initialization770"), arg1, SEMICOLON) as oth -> mayfail oth  "for_initialization770"
| TUPLE3(STRING("funcId1043"), arg1, arg2) as oth -> mayfail oth  "funcId1043"
| TUPLE3(STRING("funcId1044"), arg1, arg2) as oth -> mayfail oth  "funcId1044"
| TUPLE3(STRING("funcId1045"), Void, arg2) as oth -> mayfail oth  "funcId1045"
| TUPLE3(STRING("funcIdNew1048"), arg1, New_HYPHEN_then_HYPHEN_paren) as oth -> mayfail oth  "funcIdNew1048"
| TUPLE3(STRING("function_prototype1027"), Function, arg2) as oth -> mayfail oth  "function_prototype1027"
| TUPLE3(STRING("gateFront1446"), arg1, LPAREN) as oth -> mayfail oth  "gateFront1446"
| TUPLE3(STRING("genItemBegin447"), Begin, End) -> GenItem("", [])
| TUPLE3(STRING("generate_region443"), Generate, Endgenerate) as oth -> mayfail oth  "generate_region443"
| TUPLE3(STRING("genvar_identifierDecl174"), arg1, arg2) ->
(match arg1, arg2 with
       | IDENTIFIER id, EMPTY_TOKEN -> Id id
       | oth -> othpat2 := Some oth; failwith "genvar_identifierDecl174")
| TUPLE3(STRING("genvar_iteration475"), PLUS_PLUS, arg2) as oth -> mayfail oth  "genvar_iteration475"
| TUPLE3(STRING("genvar_iteration476"), HYPHEN_HYPHEN, arg2) as oth -> mayfail oth  "genvar_iteration476"
| TUPLE3(STRING("genvar_iteration477"), arg1, PLUS_PLUS) -> SideEffect (mly arg1, mly PLUS_PLUS)
| TUPLE3(STRING("genvar_iteration478"), arg1, HYPHEN_HYPHEN) -> SideEffect (mly arg1, mly HYPHEN_HYPHEN)
| TUPLE3(STRING("idClassSel2488"), arg1, arg2) as oth -> mayfail oth  "idClassSel2488"
| TUPLE3(STRING("idClassSelForeach2493"), arg1, arg2) as oth -> mayfail oth  "idClassSelForeach2493"
| TUPLE3(STRING("implicit_typeE363"), arg1, arg2) -> Param("implicit", mly arg1, match arg2 with TLIST lst -> rml lst | oth -> mly oth :: [])
| TUPLE3(STRING("importsAndParametersE56"), arg1, arg2) as oth -> mayfail oth  "importsAndParametersE56"
| TUPLE3(STRING("inc_or_dec_expression704"), arg1, PLUS_PLUS) as oth -> mayfail oth  "inc_or_dec_expression704"
| TUPLE3(STRING("inc_or_dec_expression705"), arg1, HYPHEN_HYPHEN) as oth -> mayfail oth  "inc_or_dec_expression705"
| TUPLE3(STRING("inc_or_dec_expression706"), PLUS_PLUS, arg2) as oth -> mayfail oth  "inc_or_dec_expression706"
| TUPLE3(STRING("inc_or_dec_expression707"), HYPHEN_HYPHEN, arg2) as oth -> mayfail oth  "inc_or_dec_expression707"
| TUPLE3(STRING("initial_construct428"), Initial, arg2) ->
(match arg2 with
       | TUPLE5 (STRING "seq_block615", (Begin|TUPLE4 (STRING "seq_blockFront624", Begin, COLON, _)), TLIST lst, End, EMPTY_TOKEN) -> Initial(rml lst)
  | stmt when pred1 stmt -> Initial(mly stmt::[])
  | oth -> othpat1 := Some oth; failwith "initial_construct428")
| TUPLE3(STRING("instnameParen559"), arg1, arg2) ->
(match arg1, arg2 with
       | IDENTIFIER id, EMPTY_TOKEN -> Id id
       | oth -> othpat2 := Some oth; failwith "instnameParen559")
| TUPLE3(STRING("interface_generate_region121"), Generate, Endgenerate) as oth -> mayfail oth  "interface_generate_region121"
| TUPLE3(STRING("interface_item113"), arg1, SEMICOLON) as oth -> mayfail oth  "interface_item113"
| TUPLE3(STRING("member_decl_assignment291"), IDENTIFIER id, EMPTY_TOKEN) -> Id id
| TUPLE3(STRING("member_decl_assignment291"), arg1, arg2) as oth -> mayfail oth  "member_decl_assignment291"
| TUPLE3(STRING("member_decl_assignment294"), EQUALS, arg2) as oth -> mayfail oth  "member_decl_assignment294"
| TUPLE3(STRING("ml_start0"), TLIST arg1, EOF_TOKEN) -> Itmlst (List.map mly arg1)
| TUPLE3(STRING("modportPortsDecl163"), arg1, arg2) -> (match arg1, arg2 with
				      | (Input|Output|Inout),IDENTIFIER id -> PortDir(mly arg1, mly arg2)
				      | oth -> othpat2 := Some oth; failwith "modportPortsDecl163")
| TUPLE3(STRING("modportPortsDecl164"), Clocking, arg2) as oth -> mayfail oth  "modportPortsDecl164"
| TUPLE3(STRING("modportPortsDecl165"), Import, arg2) as oth -> mayfail oth  "modportPortsDecl165"
| TUPLE3(STRING("modportPortsDecl166"), Export, arg2) as oth -> mayfail oth  "modportPortsDecl166"
| TUPLE3(STRING("modportPortsDecl167"), Import, arg2) as oth -> mayfail oth  "modportPortsDecl167"
| TUPLE3(STRING("modportPortsDecl168"), Export, arg2) as oth -> mayfail oth  "modportPortsDecl168"
| TUPLE3(STRING("module_common_item419"), Always, arg2) ->
    (match arg2 with 
        | TUPLE3 (STRING "statement_item680", event, block) -> AlwaysLegacy(mly event, mly block)
        | oth -> othpat1 := Some oth; failwith "module_common_item419")
| TUPLE3(STRING("module_common_item420"), Always_ff, arg2) ->
    (match arg2 with 
        | TUPLE3 (STRING "statement_item680", event, block) -> AlwaysFF(mly event, mly block)
        | oth -> othpat1 := Some oth; failwith "module_common_item420")
| TUPLE3(STRING("module_common_item421"), Always_latch, oth) ->
    if pred1 oth then AlwaysLatch(mly oth) else (othpat1 := Some oth; failwith "module_common_item421")
| TUPLE3(STRING("module_common_item422"), Always_comb, oth) ->
    if pred1 oth then AlwaysComb2(mly oth) else ( othpat1 := Some oth; failwith "module_common_item422")
| TUPLE3(STRING("module_common_item426"), ERROR_TOKEN, SEMICOLON) as oth -> mayfail oth  "module_common_item426"
| TUPLE3(STRING("module_item388"), arg1, SEMICOLON) -> mly arg1
| TUPLE3(STRING("netSig505"), arg1, arg2) ->
    (match arg1,arg2 with 
        | IDENTIFIER id, EMPTY_TOKEN -> Id id
        | oth -> othpat2 := Some oth; failwith "netSig505")
| TUPLE3(STRING("packageClassScope2587"), arg1, COLON_COLON) as oth -> mayfail oth  "packageClassScope2587"
| TUPLE3(STRING("packageClassScope2588"), arg1, COLON_COLON) as oth -> mayfail oth  "packageClassScope2588"
| TUPLE3(STRING("packageClassScopeItem2593"), arg1, COLON_COLON) ->
    (match arg1 with 
        | IDENTIFIER_HYPHEN_COLON_COLON id_cc -> PackageBody (id_cc, [])
        | oth -> othpat1 := Some oth; failwith "packageClassScopeItem2593")
| TUPLE3(STRING("package_or_generate_item_declaration36"), arg1, SEMICOLON) -> mly arg1
| TUPLE3(STRING("packedSigningE322"), Packed, EMPTY_TOKEN) -> mly Packed
| TUPLE3(STRING("packedSigningE322"), Packed, arg2) as oth -> mayfail oth  "packedSigningE322"
| TUPLE3(STRING("packed_dimension536"), LBRACK, RBRACK) as oth -> mayfail oth  "packed_dimension536"
| TUPLE3(STRING("paramPortDeclOrArg72"), arg1, arg2) -> PortItem (mly arg1, mly arg2)
| TUPLE3(STRING("paramPortDeclOrArg73"), arg1, arg2) as oth -> mayfail oth  "paramPortDeclOrArg73"
| TUPLE3(STRING("parameter_declaration175"), arg1, arg2) -> ParamDecl(mly arg1, match arg2 with TLIST lst -> rml lst | oth -> mly oth :: [])
| TUPLE3(STRING("parameter_declaration176"), arg1, arg2) as oth -> mayfail oth  "parameter_declaration176"
| TUPLE3(STRING("parameter_declarationFront177"), arg1, arg2) ->
    (match arg1,arg2 with 
        | Parameter, EMPTY_TOKEN -> Atom "Parameter"
        | Parameter, Real -> Atom "Parameter_real"
        | Localparam, EMPTY_TOKEN -> Atom "localparam"
        | Localparam, TUPLE3 (STRING "implicit_typeE363", (EMPTY_TOKEN|Signed as sgn), TLIST lst) -> Param("localparam", signed_flag sgn, rml lst)
        | oth -> othpat2 := Some oth; failwith "parameter_declarationFront177")
| TUPLE3(STRING("parameter_declarationFront178"), arg1, arg2) ->
(match arg1, arg2 with
       | Parameter, Real -> Atom("Parameter_real")
       | Localparam, Real -> Atom("Localparam_real")
       | Localparam, _ -> LocalParamTyp(mly arg2)
       | oth -> othpat2 := Some oth; failwith "parameter_declarationFront178")
| TUPLE3(STRING("parameter_declarationTypeFront179"), arg1, Type) as oth -> mayfail oth  "parameter_declarationTypeFront179"
| TUPLE3(STRING("parameter_port_declarationFrontE180"), arg1, arg2) ->
(match arg1, arg2 with
       | Parameter, EMPTY_TOKEN -> ParamDecl (mly arg2, [])
       | oth -> othpat2 := Some oth; failwith "parameter_port_declarationFrontE180")
| TUPLE3(STRING("parameter_port_declarationFrontE181"), arg1, arg2) ->
(match arg1, arg2 with
       | Parameter, TUPLE3 (STRING "data_typeBasic264", Int, (EMPTY_TOKEN|Unsigned)) -> ParamDecl (mly arg2, [])
       | oth -> othpat2 := Some oth; failwith "parameter_port_declarationFrontE181")
| TUPLE3(STRING("parameter_port_declarationTypeFrontE184"), arg1, Type) as oth -> mayfail oth  "parameter_port_declarationTypeFrontE184"
| TUPLE3(STRING("parameter_value_assignment61"), HASH, INTEGER_NUMBER arg2) as oth -> mayfail oth  "parameter_value_assignment61"
| TUPLE3(STRING("parameter_value_assignment62"), HASH, FLOATING_HYPHEN_POINT_NUMBER arg2) as oth -> mayfail oth  "parameter_value_assignment62"
| TUPLE3(STRING("parameter_value_assignment63"), HASH, arg2) as oth -> mayfail oth  "parameter_value_assignment63"
| TUPLE3(STRING("parameter_value_assignment64"), HASH, arg2) as oth -> mayfail oth  "parameter_value_assignment64"
| TUPLE3(STRING("parenE1086"), LPAREN, RPAREN) as oth -> mayfail oth  "parenE1086"
| TUPLE3(STRING("patternNoExpr750"), DOT, arg2) as oth -> mayfail oth  "patternNoExpr750"
| TUPLE3(STRING("portAndTag82"), arg1, arg2) as oth -> mayfail oth  "portAndTag82"
| TUPLE3(STRING("portDirNetE100"), arg1, arg2) -> PortDir(mly arg1, mly arg2)
| TUPLE3(STRING("port_direction214"), Const_HYPHEN_then_HYPHEN_ref, Ref) as oth -> mayfail oth  "port_direction214"
| TUPLE3(STRING("port_directionReset219"), Const_HYPHEN_then_HYPHEN_ref, Ref) as oth -> mayfail oth  "port_directionReset219"
| TUPLE3(STRING("portsStarE76"), LPAREN, RPAREN) as oth -> mayfail oth  "portsStarE76"
| TUPLE3(STRING("program_item140"), arg1, SEMICOLON) as oth -> mayfail oth  "program_item140"
| TUPLE3(STRING("senitemEdge608"), Posedge, arg2) -> Pos (mly arg2)
| TUPLE3(STRING("senitemEdge609"), Negedge, arg2) -> Neg (mly arg2)
| TUPLE3(STRING("senitemEdge610"), Edge, arg2) as oth -> mayfail oth  "senitemEdge610"
| TUPLE3(STRING("sigAttr521"), SLASH_STAR_verilator_public_flat_rw_STAR_SLASH, arg2) as oth -> mayfail oth  "sigAttr521"
| TUPLE3(STRING("simple_type259"), arg1, arg2) -> 
(match arg1, arg2 with
    | EMPTY_TOKEN, TYPE_HYPHEN_IDENTIFIER id_t -> Typ1(id_t)
    | TLIST lst, TYPE_HYPHEN_IDENTIFIER id_t -> Typ3(id_t, rml lst)
    | oth -> othpat2 := Some oth; failwith "simple_type259")
| TUPLE3(STRING("specify_block1469"), Specify, Endspecify) -> Unimplemented ("specify_block1469", [])
| TUPLE3(STRING("statement_item645"), arg1, SEMICOLON) ->
(match arg1 with
       | TUPLE5 (STRING "foperator_assignment690", lhs, EQUALS, EMPTY_TOKEN, expr) -> Blocking(mly arg1)
(* we assume these are blocking also for now, even though that may not always make much sense *)
       | TUPLE9 (STRING "foperator_assignment692", lhs, EQUALS, _, _, _, _, _, _) -> Blocking(mly arg1)
       | TUPLE4 (STRING "foperator_assignment693", lhs, PLUS_EQ, expr) -> Blocking(mly arg1)
       | TUPLE4 (STRING "foperator_assignment694", lhs, HYPHEN_EQ, expr) -> Blocking(mly arg1)
       | TUPLE4 (STRING "foperator_assignment698", lhs, AMPERSAND_EQ, expr) -> Blocking(mly arg1)
       | TUPLE4 (STRING "foperator_assignment699", lhs, VBAR_EQ, expr) -> Blocking(mly arg1)
       | oth -> othpat1 := Some oth; failwith "statement_item645")
| TUPLE3(STRING("statement_item657"), arg1, SEMICOLON) -> 
(match arg1 with
       | TUPLE3 (STRING "finc_or_dec_expression708", IDENTIFIER _, PLUS_PLUS) -> Blocking(mly arg1)
       | TUPLE3 (STRING "finc_or_dec_expression709", IDENTIFIER _, HYPHEN_HYPHEN) -> Blocking(mly arg1)
       | oth -> othpat1 := Some oth; failwith "statement_item657")
| TUPLE3(STRING("statement_item660"), arg1, SEMICOLON) ->
(match arg1 with
(*
       | TUPLE7(STRING str, arg1, arg2, arg3, arg4, arg5, arg6) when g str "system_t_call" ->
           SysTaskRef(mly arg1, List.map mly [arg2;arg3;arg4;arg5;arg6])
*)
       | TUPLE7(STRING str, arg1, LPAREN, arg3, COMMA, arg5, RPAREN) when g str "system_t_call" ->
           SysTaskRef(mly arg1, List.map mly [arg3;arg5])
       | TUPLE6(STRING str, arg1, arg2, arg3, arg4, arg5) when g str "system_t_call" -> SysTaskRef(mly arg1, List.map mly [arg2;arg3;arg4;arg5])
       | TUPLE5(STRING str, arg1, LPAREN, TLIST lst, RPAREN) when g str "system_t_call" -> SysTaskRef(mly arg1, List.map mly lst)
       | TUPLE4(STRING str, arg1, arg2, arg3) when g str "system_t_call" -> SysTaskRef(mly arg1, List.map mly [arg2;arg3])
       | TUPLE3(STRING str, arg1, arg2) when g str "system_t_call" -> SysTaskRef(mly arg1, List.map mly [arg2])
       | TUPLE5 (STRING "taskRef785", _, _, _, _) -> mly arg1
       | IDENTIFIER _ as x -> mly x
       | oth -> othpat1 := Some oth; failwith "statement_item660")
| TUPLE3(STRING("statement_item668"), Forever, arg2) as oth -> mayfail oth  "statement_item668"
| TUPLE3(STRING("statement_item674"), Return, SEMICOLON) as oth -> mayfail oth  "statement_item674"
| TUPLE3(STRING("statement_item676"), Break, SEMICOLON) -> BreakSemi
| TUPLE3(STRING("statement_item677"), Continue, SEMICOLON) as oth -> mayfail oth  "statement_item677"
| TUPLE3(STRING("statement_item679"), arg1, arg2) as oth -> mayfail oth  "statement_item679"
| TUPLE3(STRING("statement_item680"), arg1, arg2) as oth -> mayfail oth  "statement_item680"
| TUPLE3(STRING("statement_item686"), ERROR_TOKEN, SEMICOLON) as oth -> mayfail oth  "statement_item686"
| TUPLE3(STRING("system_f_call904"), DLR_LBRACE_pli_HYPHEN_system_RBRACE, arg2) as oth -> mayfail oth  "system_f_call904"
| TUPLE3(STRING("system_f_call_or_t1002"), DLR_urandom, arg2) as oth -> mayfail oth  "system_f_call_or_t1002"
| TUPLE3(STRING("system_f_call_or_t971"), DLR_random, arg2) -> SysFuncCall("$random", mly arg2 :: [])
| TUPLE3(STRING("system_f_call_or_t972"), DLR_realtime, arg2) as oth -> mayfail oth  "system_f_call_or_t972"
| TUPLE3(STRING("system_f_call_or_t990"), DLR_stime, arg2) as oth -> mayfail oth  "system_f_call_or_t990"
| TUPLE3(STRING("system_f_call_or_t996"), DLR_time, arg2) -> SysFuncCall("$time", mly arg2 :: [])
| TUPLE3(STRING("system_t_call799"), DLR_LBRACE_pli_HYPHEN_system_RBRACE, arg2) as oth -> mayfail oth  "system_t_call799"
| TUPLE3(STRING("system_t_call803"), DLR_dumpvars, arg2) as oth -> mayfail oth  "system_t_call803"
| TUPLE3(STRING("system_t_call806"), DLR_dumpall, arg2) as oth -> mayfail oth  "system_t_call806"
| TUPLE3(STRING("system_t_call808"), DLR_dumpflush, arg2) as oth -> mayfail oth  "system_t_call808"
| TUPLE3(STRING("system_t_call812"), DLR_dumpoff, arg2) as oth -> mayfail oth  "system_t_call812"
| TUPLE3(STRING("system_t_call814"), DLR_dumpon, arg2) as oth -> mayfail oth  "system_t_call814"
| TUPLE3(STRING("system_t_call818"), DLR_exit, arg2) as oth -> mayfail oth  "system_t_call818"
| TUPLE3(STRING("system_t_call820"), DLR_fflush, arg2) as oth -> mayfail oth  "system_t_call820"
| TUPLE3(STRING("system_t_call822"), DLR_finish, arg2) as oth -> mayfail oth  "system_t_call822"
| TUPLE3(STRING("system_t_call824"), DLR_stop, arg2) as oth -> mayfail oth  "system_t_call824"
| TUPLE3(STRING("system_t_call831"), DLR_display, arg2) as oth -> mayfail oth  "system_t_call831"
| TUPLE3(STRING("system_t_call833"), DLR_displayb, arg2) as oth -> mayfail oth  "system_t_call833"
| TUPLE3(STRING("system_t_call835"), DLR_displayh, arg2) as oth -> mayfail oth  "system_t_call835"
| TUPLE3(STRING("system_t_call837"), DLR_displayo, arg2) as oth -> mayfail oth  "system_t_call837"
| TUPLE3(STRING("system_t_call847"), DLR_write, arg2) as oth -> mayfail oth  "system_t_call847"
| TUPLE3(STRING("system_t_call849"), DLR_writeb, arg2) as oth -> mayfail oth  "system_t_call849"
| TUPLE3(STRING("system_t_call851"), DLR_writeh, arg2) as oth -> mayfail oth  "system_t_call851"
| TUPLE3(STRING("system_t_call853"), DLR_writeo, arg2) as oth -> mayfail oth  "system_t_call853"
| TUPLE3(STRING("system_t_call875"), DLR_info, arg2) as oth -> mayfail oth  "system_t_call875"
| TUPLE3(STRING("system_t_call877"), DLR_warning, arg2) as oth -> mayfail oth  "system_t_call877"
| TUPLE3(STRING("system_t_call879"), DLR_error, arg2) as oth -> mayfail oth  "system_t_call879"
| TUPLE3(STRING("system_t_call881"), DLR_fatal, arg2) as oth -> mayfail oth  "system_t_call881"
| TUPLE3(STRING("system_t_call884"), DLR_monitoroff, arg2) as oth -> mayfail oth  "system_t_call884"
| TUPLE3(STRING("system_t_call885"), DLR_monitoron, arg2) as oth -> mayfail oth  "system_t_call885"
| TUPLE3(STRING("taskId1040"), arg1, arg2) as oth -> mayfail oth  "taskId1040"
| TUPLE3(STRING("task_prototype1023"), Task, arg2) as oth -> mayfail oth  "task_prototype1023"
| TUPLE3(STRING("tfBodyE1056"), arg1, arg2) ->
    TFBody((match arg1 with TLIST lst -> rml lst | oth -> mly oth :: []), match arg2 with TLIST lst -> rml lst | oth -> mly oth :: [])
| TUPLE3(STRING("tfGuts1053"), SEMICOLON, arg2) -> mly arg2
| TUPLE3(STRING("tf_port_item1070"), arg1, arg2) ->
  (match arg1,arg2 with
    | TUPLE3 (STRING "data_typeBasic264", (Byte|Int|Integer), (EMPTY_TOKEN|Unsigned)),
      TUPLE5 (STRING "tf_port_itemAssignment1084", _, _, _, _) -> PortItem(mly arg1, mly arg2)
    | TUPLE4 (STRING "data_typeBasic263", Logic, (EMPTY_TOKEN|Signed), EMPTY_TOKEN),
      TUPLE5 (STRING "tf_port_itemAssignment1084", _, _, _, _) -> PortItem(mly arg1, mly arg2)
    | TUPLE4 (STRING "data_typeBasic263", Logic, (EMPTY_TOKEN|Signed), TLIST lst'),
      TUPLE5 (STRING "tf_port_itemAssignment1084", _, _, _, _) -> PortItem(mly arg1, mly arg2)
    | TUPLE3 (STRING "tf_port_itemFront1078", Input, TUPLE4 (STRING "data_typeBasic263", Source_text.Logic, EMPTY_TOKEN, TLIST lst)),
      TUPLE5 (STRING "tf_port_itemAssignment1084", _, _, _, _) -> PortFront(mly arg1, mly arg2)
    | TUPLE4 (STRING "data_type261", EMPTY_TOKEN, TYPE_HYPHEN_IDENTIFIER id_t, EMPTY_TOKEN),
      TUPLE5 (STRING "tf_port_itemAssignment1084", _, _, _, _) -> PortItem(mly arg1, mly arg2)
    | TUPLE3 (STRING "tf_port_itemFront1078", Input, TUPLE4 (STRING "data_type261", EMPTY_TOKEN, TYPE_HYPHEN_IDENTIFIER id_t, EMPTY_TOKEN)),
      TUPLE5 (STRING "tf_port_itemAssignment1084", _, _, _, _) -> PortFront(mly arg1, mly arg2)
    | TUPLE4 (STRING "tf_port_itemFront1079", (Input|Output), EMPTY_TOKEN, rng),
      TUPLE5 (STRING "tf_port_itemAssignment1084", _, _, _, _) -> PortFront(mly arg1, mly arg2)
(*
    | TUPLE4 (STRING "data_typeBasic263", (Bit|Logic), (EMPTY_TOKEN|Signed), EMPTY_TOKEN), TLIST lst -> DeclLogic(rml lst)
    | TUPLE4 (STRING "data_typeBasic263", Reg, EMPTY_TOKEN, EMPTY_TOKEN), TLIST lst -> DeclReg(rml lst, [], [])
    | TUPLE4 (STRING "data_typeBasic263", Reg, (EMPTY_TOKEN|Signed as signed), TLIST lst'), TLIST lst -> DeclReg(rml lst, rml lst', signed_flag signed)
    | TUPLE4 (STRING "data_type261", TLIST lst, TYPE_HYPHEN_IDENTIFIER typ_id, EMPTY_TOKEN), TLIST lst' -> Typ2(typ_id, rml lst, rml lst')
    | TUPLE4 (STRING "data_type261", EMPTY_TOKEN, TYPE_HYPHEN_IDENTIFIER typ_id, TLIST lst), TLIST lst' -> Typ2(typ_id, rml lst, rml lst')
    | TUPLE4 (STRING "data_type261", EMPTY_TOKEN, TYPE_HYPHEN_IDENTIFIER typ_id, EMPTY_TOKEN), TLIST lst -> Typ3(typ_id, rml lst)
    | TUPLE6 (STRING "enumDecl323", Enum, base_type, LBRACE, TLIST lst, RBRACE), TLIST lst' -> TypEnum4(mly base_type, rml lst, rml lst')
    | TUPLE4 (STRING "data_declarationVarFront357", Const, EMPTY_TOKEN, base_type), TLIST lst -> DeclData(mly base_type, rml lst)
    | TUPLE4 (STRING "data_type261", TLIST lst, TYPE_HYPHEN_IDENTIFIER typ_id, TLIST lst'), TLIST lst'' -> Typ4(typ_id, rml lst, rml lst', rml lst'')
    | TUPLE3 (STRING "data_typeNoRef267", struct_decl, TLIST lst), TLIST lst' -> Typ5(mly struct_decl, rml lst')
    | TUPLE3 (STRING "data_typeNoRef267", struct_decl, EMPTY_TOKEN), TLIST lst' -> Typ6(mly struct_decl)
*)
        | oth -> othpat2 := Some oth; failwith "tf_port_item1070")
| TUPLE3(STRING("tf_port_itemFront1073"), arg1, arg2) as oth -> mayfail oth  "tf_port_itemFront1073"
| TUPLE3(STRING("tf_port_itemFront1075"), Var, arg2) as oth -> mayfail oth  "tf_port_itemFront1075"
| TUPLE3(STRING("tf_port_itemFront1076"), Var, arg2) as oth -> mayfail oth  "tf_port_itemFront1076"
| TUPLE3(STRING("tf_port_itemFront1078"), arg1, arg2) -> PortItemFront(mly arg1, mly arg2)
| TUPLE3(STRING("tf_port_itemFront1080"), arg1, arg2) as oth -> mayfail oth  "tf_port_itemFront1080"
| TUPLE3(STRING("var_data_type277"), Var, arg2) as oth -> mayfail oth  "var_data_type277"
| TUPLE3(STRING("var_data_type278"), Var, arg2) as oth -> mayfail oth  "var_data_type278"
| TUPLE3(STRING("variable_decl_assignment300"), EQUALS, arg2) as oth -> mayfail oth  "variable_decl_assignment300"
| TUPLE3(STRING("variable_dimension311"), LBRACK, RBRACK) as oth -> mayfail oth  "variable_dimension311"
| TUPLE3(STRING("variable_dimension314"), LBRACK_STAR, RBRACK) as oth -> mayfail oth  "variable_dimension314"
| TUPLE3(STRING("vltDFTaskE2692"), HYPHEN_HYPHEN_function, arg2) as oth -> mayfail oth  "vltDFTaskE2692"
| TUPLE3(STRING("vltDFTaskE2693"), HYPHEN_HYPHEN_task, arg2) as oth -> mayfail oth  "vltDFTaskE2693"
| TUPLE3(STRING("vltDModuleE2690"), HYPHEN_HYPHEN_module, arg2) as oth -> mayfail oth  "vltDModuleE2690"
| TUPLE3(STRING("vltItem2676"), Hier_block, arg2) as oth -> mayfail oth  "vltItem2676"
| TUPLE3(STRING("vltVarAttrVarE2697"), HYPHEN_HYPHEN_var, arg2) as oth -> mayfail oth  "vltVarAttrVarE2697"
| TUPLE4(STRING("argsExprListE1359"), arg1, COMMA, arg3) ->
  ( match (mly arg1, mly arg3) with
    | Itmlst lst, Itmlst lst' -> Itmlst (lst@lst')
    | hd, Itmlst lst -> Itmlst (hd::lst)
    | Itmlst lst, oth -> Itmlst (lst@[oth])
    | oth, oth' -> Itmlst (oth::[oth']))
| TUPLE4(STRING("assignOne488"), arg1, EQUALS, arg3) -> Asgn1(mly arg1, mly arg3)
| TUPLE4(STRING("assignment_pattern767"), QUOTE_LBRACE, arg2, RBRACE) ->
(match arg2 with TLIST lst -> AsgnPat (rml lst) | oth -> othpat1 := Some oth; failwith "assignment_pattern768")
| TUPLE4(STRING("assignment_pattern768"), QUOTE_LBRACE, arg2, RBRACE) ->
(match arg2 with TLIST lst -> AsgnPat (rml lst) | oth -> othpat1 := Some oth; failwith "assignment_pattern768")
| TUPLE4(STRING("bind_directive436"), Bind, arg2, arg3) as oth -> mayfail oth  "bind_directive436"
| TUPLE4(STRING("bind_target_instance_list439"), arg1, COMMA, arg3) as oth -> mayfail oth  "bind_target_instance_list439"
| TUPLE4(STRING("case_generate_item483"), arg1, arg2, arg3) as oth -> mayfail oth  "case_generate_item483"
| TUPLE4(STRING("case_generate_item484"), Default, arg2, arg3) as oth -> mayfail oth  "case_generate_item484"
| TUPLE4(STRING("class_constructor_prototype1029"), Function, arg2, SEMICOLON) as oth -> mayfail oth  "class_constructor_prototype1029"
| TUPLE4(STRING("class_method2614"), Extern, arg2, arg3) as oth -> mayfail oth  "class_method2614"
| TUPLE4(STRING("combinational_body1463"), Table, arg2, Endtable) as oth -> mayfail oth  "combinational_body1463"
| TUPLE4(STRING("concurrent_assertion_item2547"), arg1, COLON, arg3) as oth -> mayfail oth  "concurrent_assertion_item2547"
| TUPLE4(STRING("constraint_block2630"), LBRACE, arg2, RBRACE) as oth -> mayfail oth  "constraint_block2630"
| TUPLE4(STRING("constraint_expression2641"), Soft, arg2, SEMICOLON) as oth -> mayfail oth  "constraint_expression2641"
| TUPLE4(STRING("constraint_set2648"), LBRACE, arg2, RBRACE) as oth -> mayfail oth  "constraint_set2648"
| TUPLE4(STRING("data_declarationVar347"), arg1, arg2, SEMICOLON) ->
  (match arg1, arg2 with
    | TUPLE4 (STRING "data_typeBasic263", (Bit|Logic), (EMPTY_TOKEN|Signed), EMPTY_TOKEN), TLIST lst -> DeclLogic(rml lst)
    | TUPLE4 (STRING "data_typeBasic263", Logic, (EMPTY_TOKEN|Signed), TLIST lst'), TLIST lst -> DeclLogic2(rml lst, rml lst')
    | TUPLE3 (STRING "data_typeBasic264", (Byte|Int|Integer|Longint), (EMPTY_TOKEN|Unsigned)), TLIST lst -> DeclInt2(rml lst)
    | TUPLE4 (STRING "data_typeBasic263", Reg, (EMPTY_TOKEN|Signed as signed), EMPTY_TOKEN), TLIST lst -> DeclReg(rml lst, [], signed_flag signed)
    | TUPLE4 (STRING "data_typeBasic263", Reg, (EMPTY_TOKEN|Signed as signed), TLIST lst'), TLIST lst -> DeclReg(rml lst, rml lst', signed_flag signed)
    | TUPLE4 (STRING "data_type261", TLIST lst, TYPE_HYPHEN_IDENTIFIER typ_id, EMPTY_TOKEN), TLIST lst' -> Typ2(typ_id, rml lst, rml lst')
    | TUPLE4 (STRING "data_type261", EMPTY_TOKEN, TYPE_HYPHEN_IDENTIFIER typ_id, TLIST lst), TLIST lst' -> Typ2(typ_id, rml lst, rml lst')
    | TUPLE4 (STRING "data_type261", EMPTY_TOKEN, TYPE_HYPHEN_IDENTIFIER typ_id, EMPTY_TOKEN), TLIST lst -> Typ3(typ_id, rml lst)
    | TUPLE6 (STRING "enumDecl323", Enum, base_type, LBRACE, TLIST lst, RBRACE), TLIST lst' -> TypEnum4(mly base_type, rml lst, rml lst')
    | TUPLE3 (STRING "data_declarationVarFront356", Automatic, base_type), TLIST lst -> DeclData(mly base_type, rml lst)
    | TUPLE4 (STRING "data_declarationVarFront357", Const, EMPTY_TOKEN, base_type), TLIST lst -> DeclData(mly base_type, rml lst)
    | TUPLE4 (STRING "data_type261", TLIST lst, TYPE_HYPHEN_IDENTIFIER typ_id, TLIST lst'), TLIST lst'' -> Typ4(typ_id, rml lst, rml lst', rml lst'')
    | TUPLE3 (STRING "data_typeNoRef267", struct_decl, TLIST lst), TLIST lst' -> Typ5(mly struct_decl, rml lst')
    | TUPLE3 (STRING "data_typeNoRef267", struct_decl, EMPTY_TOKEN), TLIST lst' -> Typ6(mly struct_decl)
    | String, TLIST lst -> Typ5(mly arg1, rml lst)
    | oth -> othpat2 := Some oth; failwith "data_declarationVar347")
| TUPLE4(STRING("data_declarationVarClass348"), arg1, arg2, SEMICOLON) as oth -> mayfail oth  "data_declarationVarClass348"
| TUPLE4(STRING("data_declarationVarFront349"), Var, arg2, arg3) as oth -> mayfail oth  "data_declarationVarFront349"
| TUPLE4(STRING("data_declarationVarFront353"), Const, Var, arg3) as oth -> mayfail oth  "data_declarationVarFront353"
| TUPLE4(STRING("data_declarationVarFront357"), Const, arg2, arg3) as oth -> mayfail oth  "data_declarationVarFront357"
| TUPLE4(STRING("data_declarationVarFrontClass358"), Var, arg2, arg3) as oth -> mayfail oth  "data_declarationVarFrontClass358"
| TUPLE4(STRING("data_type261"), arg1, arg2, arg3) ->
(match arg1, arg2, arg3 with
       | TLIST lst, TYPE_HYPHEN_IDENTIFIER id_t, EMPTY_TOKEN -> Typ3(id_t, rml lst)
       | EMPTY_TOKEN, TYPE_HYPHEN_IDENTIFIER id_t, EMPTY_TOKEN -> Typ1(id_t)
       | EMPTY_TOKEN, TYPE_HYPHEN_IDENTIFIER id_t, TLIST lst -> Typ3(id_t, rml lst)
       | oth -> othpat3 := Some oth; failwith "data_type261")
| TUPLE4(STRING("data_typeBasic263"), arg1, arg2, arg3) ->
(match arg1, arg2, arg3 with
  | Logic, EMPTY_TOKEN, TLIST lst -> Typ5(mly arg1, rml lst)
  | (Logic|Bit), EMPTY_TOKEN, EMPTY_TOKEN -> Typ6(mly arg1)
  | oth -> othpat3 := Some oth; failwith "data_typeBasic263")
| TUPLE4(STRING("data_typeNoRef272"), Virtual_HYPHEN_then_HYPHEN_interface, Interface, arg3) as oth -> mayfail oth  "data_typeNoRef272"
| TUPLE4(STRING("deferred_immediate_assertion_item2525"), arg1, COLON, arg3) as oth -> mayfail oth  "deferred_immediate_assertion_item2525"
| TUPLE4(STRING("dist_item2652"), arg1, COLON_EQ, arg3) as oth -> mayfail oth  "dist_item2652"
| TUPLE4(STRING("dist_item2653"), arg1, COLON_SLASH, arg3) as oth -> mayfail oth  "dist_item2653"
| TUPLE4(STRING("dist_list2650"), arg1, COMMA, arg3) as oth -> mayfail oth  "dist_list2650"
| TUPLE4(STRING("enumNameRangeE335"), LBRACK, arg2, RBRACK) as oth -> mayfail oth  "enumNameRangeE335"
| TUPLE4(STRING("enum_base_typeE328"), arg1, arg2, arg3) ->
(match (arg1, arg2, arg3) with
       | Logic, EMPTY_TOKEN, EMPTY_TOKEN -> TypEnum5 (mly Logic)
       | Logic, EMPTY_TOKEN, TLIST lst -> TypEnum3 (rml lst)
       | oth -> othpat3 := Some oth; failwith "enum_base_typeE328")
| TUPLE4(STRING("enum_base_typeE330"), arg1, arg2, arg3) as oth -> mayfail oth  "enum_base_typeE330"
| TUPLE4(STRING("enum_name_declaration333"), arg1, arg2, arg3) ->
(match (arg1, arg2, arg3) with
       | IDENTIFIER id, EMPTY_TOKEN, EMPTY_TOKEN -> Id id
       | IDENTIFIER id, EMPTY_TOKEN, TUPLE3 (STRING "enumNameStartE338", EQUALS, expr) -> EnumInit(id, mly expr)
       | oth -> othpat3 := Some oth; failwith "enum_name_declaration333")
| TUPLE4(STRING("event_expression598"), arg1, Or, arg3) -> event_lst (mly arg1, mly arg3)
| TUPLE4(STRING("event_expression599"), arg1, COMMA, arg3) -> event_lst (mly arg1, mly arg3)
| TUPLE4(STRING("expr1119"), arg1, PLUS, arg3) -> Add(mly arg1, mly arg3)
| TUPLE4(STRING("expr1120"), arg1, HYPHEN, arg3) -> Sub(mly arg1, mly arg3)
| TUPLE4(STRING("expr1121"), arg1, STAR, arg3) -> Mult(mly arg1, mly arg3)
| TUPLE4(STRING("expr1122"), arg1, SLASH, arg3) -> Div(mly arg1, mly arg3)
| TUPLE4(STRING("expr1123"), arg1, PERCENT, arg3) -> Mod(mly arg1, mly arg3)
| TUPLE4(STRING("expr1124"), arg1, EQ_EQ, arg3) -> Equals(mly arg1, mly arg3)
| TUPLE4(STRING("expr1125"), arg1, PLING_EQ, arg3) -> NotEq(mly arg1, mly arg3)
| TUPLE4(STRING("expr1126"), arg1, EQ_EQ_EQ, arg3) -> Equals3(mly arg1, mly arg3)
| TUPLE4(STRING("expr1127"), arg1, PLING_EQ_EQ, arg3) -> NotEq3(mly arg1, mly arg3)
| TUPLE4(STRING("expr1128"), arg1, EQ_EQ_QUERY, arg3) -> EqualsQuery(mly arg1, mly arg3)
| TUPLE4(STRING("expr1129"), arg1, PLING_EQ_QUERY, arg3) -> NotEqQuery(mly arg1, mly arg3)
| TUPLE4(STRING("expr1130"), arg1, AMPERSAND_AMPERSAND, arg3) -> And2(mly arg1, mly arg3)
| TUPLE4(STRING("expr1131"), arg1, VBAR_VBAR, arg3) -> Or2(mly arg1, mly arg3)
| TUPLE4(STRING("expr1132"), arg1, STAR_STAR, arg3) -> StarStar(mly arg1, mly arg3)
| TUPLE4(STRING("expr1133"), arg1, LESS, arg3) -> Less(mly arg1, mly arg3)
| TUPLE4(STRING("expr1134"), arg1, GREATER, arg3) -> Greater(mly arg1, mly arg3)
| TUPLE4(STRING("expr1135"), arg1, GT_EQ, arg3) -> GtEq(mly arg1, mly arg3)
| TUPLE4(STRING("expr1136"), arg1, AMPERSAND, arg3) -> And(mly arg1, mly arg3)
| TUPLE4(STRING("expr1137"), arg1, VBAR, arg3) -> Or(mly arg1, mly arg3)
| TUPLE4(STRING("expr1138"), arg1, CARET, arg3) -> Xor(mly arg1, mly arg3)
| TUPLE4(STRING("expr1139"), arg1, CARET_TILDE, arg3) -> Xnor(mly arg1, mly arg3)
| TUPLE4(STRING("expr1140"), arg1, TILDE_VBAR, arg3) -> Nor(mly arg1, mly arg3)
| TUPLE4(STRING("expr1141"), arg1, TILDE_AMPERSAND, arg3) -> Nand(mly arg1, mly arg3)
| TUPLE4(STRING("expr1142"), arg1, LT_LT, arg3) -> Shiftl(mly arg1, mly arg3)
| TUPLE4(STRING("expr1143"), arg1, GT_GT, arg3) -> Shiftr(mly arg1, mly arg3)
| TUPLE4(STRING("expr1144"), arg1, GT_GT_GT, arg3) -> Shiftr3(mly arg1, mly arg3)
| TUPLE4(STRING("expr1145"), arg1, LT_HYPHEN_GT, arg3) -> LtGt(mly arg1, mly arg3)
| TUPLE4(STRING("expr1146"), arg1, HYPHEN_GT, arg3) -> HyphenGt(mly arg1, mly arg3)
| TUPLE4(STRING("expr1147"), arg1, LT_EQ, arg3) -> LtEq(mly arg1, mly arg3)
| TUPLE4(STRING("expr1157"), arg1, DOT, arg3) -> Dot1(mly arg1, mly arg3)
| TUPLE4(STRING("expr1158"), arg1, DOT, arg3) -> Dot1(mly arg1, mly arg3)
| TUPLE4(STRING("expr1159"), LPAREN, arg2, RPAREN) -> Expression(mly arg2)
| TUPLE4(STRING("expr1172"), arg1, AMPERSAND_AMPERSAND_AMPERSAND, arg3) -> And3(mly arg1, mly arg3)
| TUPLE4(STRING("exprNoStr1251"), arg1, PLUS, arg3) as oth -> mayfail oth  "exprNoStr1251"
| TUPLE4(STRING("exprNoStr1252"), arg1, HYPHEN, arg3) as oth -> mayfail oth  "exprNoStr1252"
| TUPLE4(STRING("exprNoStr1253"), arg1, STAR, arg3) as oth -> mayfail oth  "exprNoStr1253"
| TUPLE4(STRING("exprNoStr1254"), arg1, SLASH, arg3) as oth -> mayfail oth  "exprNoStr1254"
| TUPLE4(STRING("exprNoStr1255"), arg1, PERCENT, arg3) as oth -> mayfail oth  "exprNoStr1255"
| TUPLE4(STRING("exprNoStr1256"), arg1, EQ_EQ, arg3) as oth -> mayfail oth  "exprNoStr1256"
| TUPLE4(STRING("exprNoStr1257"), arg1, PLING_EQ, arg3) as oth -> mayfail oth  "exprNoStr1257"
| TUPLE4(STRING("exprNoStr1258"), arg1, EQ_EQ_EQ, arg3) as oth -> mayfail oth  "exprNoStr1258"
| TUPLE4(STRING("exprNoStr1259"), arg1, PLING_EQ_EQ, arg3) as oth -> mayfail oth  "exprNoStr1259"
| TUPLE4(STRING("exprNoStr1260"), arg1, EQ_EQ_QUERY, arg3) as oth -> mayfail oth  "exprNoStr1260"
| TUPLE4(STRING("exprNoStr1261"), arg1, PLING_EQ_QUERY, arg3) as oth -> mayfail oth  "exprNoStr1261"
| TUPLE4(STRING("exprNoStr1262"), arg1, AMPERSAND_AMPERSAND, arg3) as oth -> mayfail oth  "exprNoStr1262"
| TUPLE4(STRING("exprNoStr1263"), arg1, VBAR_VBAR, arg3) as oth -> mayfail oth  "exprNoStr1263"
| TUPLE4(STRING("exprNoStr1264"), arg1, STAR_STAR, arg3) as oth -> mayfail oth  "exprNoStr1264"
| TUPLE4(STRING("exprNoStr1265"), arg1, LESS, arg3) as oth -> mayfail oth  "exprNoStr1265"
| TUPLE4(STRING("exprNoStr1266"), arg1, GREATER, arg3) as oth -> mayfail oth  "exprNoStr1266"
| TUPLE4(STRING("exprNoStr1267"), arg1, GT_EQ, arg3) as oth -> mayfail oth  "exprNoStr1267"
| TUPLE4(STRING("exprNoStr1268"), arg1, AMPERSAND, arg3) as oth -> mayfail oth  "exprNoStr1268"
| TUPLE4(STRING("exprNoStr1269"), arg1, VBAR, arg3) as oth -> mayfail oth  "exprNoStr1269"
| TUPLE4(STRING("exprNoStr1270"), arg1, CARET, arg3) as oth -> mayfail oth  "exprNoStr1270"
| TUPLE4(STRING("exprNoStr1271"), arg1, CARET_TILDE, arg3) as oth -> mayfail oth  "exprNoStr1271"
| TUPLE4(STRING("exprNoStr1272"), arg1, TILDE_VBAR, arg3) as oth -> mayfail oth  "exprNoStr1272"
| TUPLE4(STRING("exprNoStr1273"), arg1, TILDE_AMPERSAND, arg3) as oth -> mayfail oth  "exprNoStr1273"
| TUPLE4(STRING("exprNoStr1274"), arg1, LT_LT, arg3) as oth -> mayfail oth  "exprNoStr1274"
| TUPLE4(STRING("exprNoStr1275"), arg1, GT_GT, arg3) as oth -> mayfail oth  "exprNoStr1275"
| TUPLE4(STRING("exprNoStr1276"), arg1, GT_GT_GT, arg3) as oth -> mayfail oth  "exprNoStr1276"
| TUPLE4(STRING("exprNoStr1277"), arg1, LT_HYPHEN_GT, arg3) as oth -> mayfail oth  "exprNoStr1277"
| TUPLE4(STRING("exprNoStr1278"), arg1, HYPHEN_GT, arg3) as oth -> mayfail oth  "exprNoStr1278"
| TUPLE4(STRING("exprNoStr1279"), arg1, LT_EQ, arg3) as oth -> mayfail oth  "exprNoStr1279"
| TUPLE4(STRING("exprNoStr1289"), arg1, DOT, arg3) as oth -> mayfail oth  "exprNoStr1289"
| TUPLE4(STRING("exprNoStr1290"), arg1, DOT, arg3) as oth -> mayfail oth  "exprNoStr1290"
| TUPLE4(STRING("exprNoStr1291"), LPAREN, arg2, RPAREN) as oth -> mayfail oth  "exprNoStr1291"
| TUPLE4(STRING("exprNoStr1304"), arg1, AMPERSAND_AMPERSAND_AMPERSAND, arg3) as oth -> mayfail oth  "exprNoStr1304"
| TUPLE4(STRING("exprOkLvalue1307"), LBRACE, arg2, RBRACE) ->
  (match arg2 with
    | TLIST lst -> ExprOKL (rml lst)
    | oth -> othpat1 := Some oth; failwith "exprOkLvalue1307")
| TUPLE4(STRING("exprScope1329"), arg1, DOT, arg3) -> Dot1(mly arg1, mly arg3)
| TUPLE4(STRING("exprScope1330"), arg1, DOT, Super) as oth -> mayfail oth  "exprScope1330"
| TUPLE4(STRING("extern_tf_declaration153"), Extern, arg2, SEMICOLON) as oth -> mayfail oth  "extern_tf_declaration153"
| TUPLE4(STRING("extern_tf_declaration154"), Extern, arg2, SEMICOLON) as oth -> mayfail oth  "extern_tf_declaration154"
| TUPLE4(STRING("fIdScoped1050"), arg1, DOT, arg3) as oth -> mayfail oth  "fIdScoped1050"
| TUPLE4(STRING("fexpr1185"), arg1, PLUS, arg3) as oth -> mayfail oth  "fexpr1185"
| TUPLE4(STRING("fexpr1186"), arg1, HYPHEN, arg3) as oth -> mayfail oth  "fexpr1186"
| TUPLE4(STRING("fexpr1187"), arg1, STAR, arg3) as oth -> mayfail oth  "fexpr1187"
| TUPLE4(STRING("fexpr1188"), arg1, SLASH, arg3) as oth -> mayfail oth  "fexpr1188"
| TUPLE4(STRING("fexpr1189"), arg1, PERCENT, arg3) as oth -> mayfail oth  "fexpr1189"
| TUPLE4(STRING("fexpr1190"), arg1, EQ_EQ, arg3) as oth -> mayfail oth  "fexpr1190"
| TUPLE4(STRING("fexpr1191"), arg1, PLING_EQ, arg3) as oth -> mayfail oth  "fexpr1191"
| TUPLE4(STRING("fexpr1192"), arg1, EQ_EQ_EQ, arg3) as oth -> mayfail oth  "fexpr1192"
| TUPLE4(STRING("fexpr1193"), arg1, PLING_EQ_EQ, arg3) as oth -> mayfail oth  "fexpr1193"
| TUPLE4(STRING("fexpr1194"), arg1, EQ_EQ_QUERY, arg3) as oth -> mayfail oth  "fexpr1194"
| TUPLE4(STRING("fexpr1195"), arg1, PLING_EQ_QUERY, arg3) as oth -> mayfail oth  "fexpr1195"
| TUPLE4(STRING("fexpr1196"), arg1, AMPERSAND_AMPERSAND, arg3) as oth -> mayfail oth  "fexpr1196"
| TUPLE4(STRING("fexpr1197"), arg1, VBAR_VBAR, arg3) as oth -> mayfail oth  "fexpr1197"
| TUPLE4(STRING("fexpr1198"), arg1, STAR_STAR, arg3) as oth -> mayfail oth  "fexpr1198"
| TUPLE4(STRING("fexpr1199"), arg1, LESS, arg3) as oth -> mayfail oth  "fexpr1199"
| TUPLE4(STRING("fexpr1200"), arg1, GREATER, arg3) as oth -> mayfail oth  "fexpr1200"
| TUPLE4(STRING("fexpr1201"), arg1, GT_EQ, arg3) as oth -> mayfail oth  "fexpr1201"
| TUPLE4(STRING("fexpr1202"), arg1, AMPERSAND, arg3) as oth -> mayfail oth  "fexpr1202"
| TUPLE4(STRING("fexpr1203"), arg1, VBAR, arg3) as oth -> mayfail oth  "fexpr1203"
| TUPLE4(STRING("fexpr1204"), arg1, CARET, arg3) as oth -> mayfail oth  "fexpr1204"
| TUPLE4(STRING("fexpr1205"), arg1, CARET_TILDE, arg3) as oth -> mayfail oth  "fexpr1205"
| TUPLE4(STRING("fexpr1206"), arg1, TILDE_VBAR, arg3) as oth -> mayfail oth  "fexpr1206"
| TUPLE4(STRING("fexpr1207"), arg1, TILDE_AMPERSAND, arg3) as oth -> mayfail oth  "fexpr1207"
| TUPLE4(STRING("fexpr1208"), arg1, LT_LT, arg3) as oth -> mayfail oth  "fexpr1208"
| TUPLE4(STRING("fexpr1209"), arg1, GT_GT, arg3) as oth -> mayfail oth  "fexpr1209"
| TUPLE4(STRING("fexpr1210"), arg1, GT_GT_GT, arg3) as oth -> mayfail oth  "fexpr1210"
| TUPLE4(STRING("fexpr1211"), arg1, LT_HYPHEN_GT, arg3) as oth -> mayfail oth  "fexpr1211"
| TUPLE4(STRING("fexpr1212"), arg1, HYPHEN_GT, arg3) as oth -> mayfail oth  "fexpr1212"
| TUPLE4(STRING("fexpr1213"), arg1, LT_EQ_HYPHEN_ignored, arg3) as oth -> mayfail oth  "fexpr1213"
| TUPLE4(STRING("fexpr1223"), arg1, DOT, arg3) as oth -> mayfail oth  "fexpr1223"
| TUPLE4(STRING("fexpr1224"), arg1, DOT, arg3) as oth -> mayfail oth  "fexpr1224"
| TUPLE4(STRING("fexpr1225"), LPAREN, arg2, RPAREN) as oth -> mayfail oth  "fexpr1225"
| TUPLE4(STRING("fexpr1238"), arg1, AMPERSAND_AMPERSAND_AMPERSAND, arg3) as oth -> mayfail oth  "fexpr1238"
| TUPLE4(STRING("fexprOkLvalue1316"), LBRACE, arg2, RBRACE) as oth -> mayfail oth  "fexprOkLvalue1316"
| TUPLE4(STRING("fexprScope1336"), arg1, DOT, arg3) -> Dot1(mly arg1, mly arg3)
| TUPLE4(STRING("fexprScope1337"), arg1, DOT, Super) as oth -> mayfail oth  "fexprScope1337"
| TUPLE4(STRING("foperator_assignment693"), arg1, PLUS_EQ, arg3) -> Asgn1(mly arg1, Add(mly arg1, mly arg3))
| TUPLE4(STRING("foperator_assignment694"), arg1, HYPHEN_EQ, arg3) -> Asgn1 (mly arg1, Sub(mly arg1, mly arg3))
| TUPLE4(STRING("foperator_assignment695"), arg1, STAR_EQ, arg3) -> Asgn1 (mly arg1, Mult(mly arg1, mly arg3))
| TUPLE4(STRING("foperator_assignment696"), arg1, SLASH_EQ, arg3) -> Asgn1 (mly arg1, Div(mly arg1, mly arg3))
| TUPLE4(STRING("foperator_assignment697"), arg1, PERCENT_EQ, arg3) -> Asgn1 (mly arg1, Mod(mly arg1, mly arg3))
| TUPLE4(STRING("foperator_assignment698"), arg1, AMPERSAND_EQ, arg3) -> Asgn1(mly arg1, And(mly arg1, mly arg3))
| TUPLE4(STRING("foperator_assignment699"), arg1, VBAR_EQ, arg3) -> Asgn1(mly arg1, Or(mly arg1, mly arg3))
| TUPLE4(STRING("foperator_assignment700"), arg1, CARET_EQ, arg3) as oth -> mayfail oth  "foperator_assignment700"
| TUPLE4(STRING("foperator_assignment701"), arg1, LT_LT_EQ, arg3) as oth -> mayfail oth  "foperator_assignment701"
| TUPLE4(STRING("foperator_assignment702"), arg1, GT_GT_EQ, arg3) as oth -> mayfail oth  "foperator_assignment702"
| TUPLE4(STRING("foperator_assignment703"), arg1, GT_GT_GT_EQ, arg3) as oth -> mayfail oth  "foperator_assignment703"
| TUPLE4(STRING("for_initializationItem776"), arg1, EQUALS, arg3) -> Asgn1 (mly arg1, mly arg3)
| TUPLE4(STRING("for_step780"), arg1, COMMA, arg3) as oth -> mayfail oth  "for_step780"
| TUPLE4(STRING("funcId1042"), arg1, arg2, arg3) as oth -> mayfail oth  "funcId1042"
| TUPLE4(STRING("function_subroutine_callNoMethod797"), arg1, With_HYPHEN_then_HYPHEN_LBRACE, arg3) as oth -> mayfail oth  "function_subroutine_callNoMethod797"
| TUPLE4(STRING("gateFront1445"), arg1, arg2, LPAREN) as oth -> mayfail oth  "gateFront1445"
| TUPLE4(STRING("gatePulldown1443"), arg1, arg2, RPAREN) as oth -> mayfail oth  "gatePulldown1443"
| TUPLE4(STRING("gatePullup1442"), arg1, arg2, RPAREN) as oth -> mayfail oth  "gatePullup1442"
| TUPLE4(STRING("gateUnsup1444"), arg1, arg2, RPAREN) as oth -> mayfail oth  "gateUnsup1444"
| TUPLE4(STRING("genItemBegin446"), Begin, arg2, End) ->
(match arg2 with TLIST lst -> GenItem("", rml lst) | oth -> othpat1 := Some oth; failwith "genItemBegin446")
| TUPLE4(STRING("generate_region442"), Generate, arg2, Endgenerate) -> mly arg2 (* these keywords are obsolete *)
| TUPLE4(STRING("genvar_declaration171"), Genvar, arg2, SEMICOLON) -> Genvar(match arg2 with TLIST lst -> rml lst | oth -> mly oth :: [])
| TUPLE4(STRING("genvar_initialization461"), arg1, EQUALS, arg3) as oth -> mayfail oth  "genvar_initialization461"
| TUPLE4(STRING("genvar_iteration463"), arg1, EQUALS, arg3) -> Asgn1(mly arg1, mly arg3)
| TUPLE4(STRING("genvar_iteration464"), arg1, PLUS_EQ, arg3) -> Asgn1(mly arg1, Add(mly arg1, mly arg3))
| TUPLE4(STRING("genvar_iteration465"), arg1, HYPHEN_EQ, arg3) as oth -> mayfail oth  "genvar_iteration465"
| TUPLE4(STRING("genvar_iteration466"), arg1, STAR_EQ, arg3) as oth -> mayfail oth  "genvar_iteration466"
| TUPLE4(STRING("genvar_iteration467"), arg1, SLASH_EQ, arg3) as oth -> mayfail oth  "genvar_iteration467"
| TUPLE4(STRING("genvar_iteration468"), arg1, PERCENT_EQ, arg3) as oth -> mayfail oth  "genvar_iteration468"
| TUPLE4(STRING("genvar_iteration469"), arg1, AMPERSAND_EQ, arg3) as oth -> mayfail oth  "genvar_iteration469"
| TUPLE4(STRING("genvar_iteration470"), arg1, VBAR_EQ, arg3) as oth -> mayfail oth  "genvar_iteration470"
| TUPLE4(STRING("genvar_iteration471"), arg1, CARET_EQ, arg3) as oth -> mayfail oth  "genvar_iteration471"
| TUPLE4(STRING("genvar_iteration472"), arg1, LT_LT_EQ, arg3) as oth -> mayfail oth  "genvar_iteration472"
| TUPLE4(STRING("genvar_iteration473"), arg1, GT_GT_EQ, arg3) as oth -> mayfail oth  "genvar_iteration473"
| TUPLE4(STRING("genvar_iteration474"), arg1, GT_GT_GT_EQ, arg3) as oth -> mayfail oth  "genvar_iteration474"
| TUPLE4(STRING("idClassSel2485"), This, DOT, arg3) as oth -> mayfail oth  "idClassSel2485"
| TUPLE4(STRING("idClassSel2486"), Super, DOT, arg3) as oth -> mayfail oth  "idClassSel2486"
| TUPLE4(STRING("idClassSelForeach2490"), This, DOT, arg3) as oth -> mayfail oth  "idClassSelForeach2490"
| TUPLE4(STRING("idClassSelForeach2491"), Super, DOT, arg3) as oth -> mayfail oth  "idClassSelForeach2491"
| TUPLE4(STRING("idDotted2494"), DLR_root, DOT, arg3) as oth -> mayfail oth  "idDotted2494"
| TUPLE4(STRING("idDottedForeach2496"), DLR_root, DOT, arg3) as oth -> mayfail oth  "idDottedForeach2496"
| TUPLE4(STRING("idDottedMore2499"), arg1, DOT, arg3) -> Dot1(mly arg1, mly arg3)
| TUPLE4(STRING("idDottedMoreForeach2501"), arg1, DOT, arg3) as oth -> mayfail oth  "idDottedMoreForeach2501"
| TUPLE4(STRING("instRange564"), LBRACK, arg2, RBRACK) as oth -> mayfail oth  "instRange564"
| TUPLE4(STRING("intFront108"), Interface, arg2, arg3) as oth -> mayfail oth  "intFront108"
| TUPLE4(STRING("interface_generate_region120"), Generate, arg2, Endgenerate) as oth -> mayfail oth  "interface_generate_region120"
| TUPLE4(STRING("loop_variables783"), arg1, COMMA, arg3) as oth -> mayfail oth  "loop_variables783"
| TUPLE4(STRING("modFront54"), Module, arg2, modid) ->
    (match modid with IDENTIFIER id -> print_endline id | oth -> othpat1 := Some oth; failwith "modFront54");
    Unknown ("modFront54", mly arg2 :: [])
| TUPLE4(STRING("modport_declaration156"), Modport, arg2, SEMICOLON) -> DeclModPort(match arg2 with TLIST lst -> rml lst | _ -> mly arg2 :: [])
| TUPLE4(STRING("module_or_generate_item408"), Defparam, arg2, SEMICOLON) as oth -> mayfail oth  "module_or_generate_item408"
| TUPLE4(STRING("mpInstnameParen555"), arg1, arg2, arg3) as oth -> mayfail oth  "mpInstnameParen555"
| TUPLE4(STRING("netSig507"), arg1, arg2, arg3) ->
  (match arg1, arg2, arg3 with
       | IDENTIFIER id, TLIST rng, EMPTY_TOKEN -> NetDecl(rml rng, Id id :: [])
       | oth -> othpat3 := Some oth; failwith "netSig507")
| TUPLE4(STRING("net_dataTypeE193"), arg1, arg2, arg3) ->
  (match arg1, arg2, arg3 with
     | EMPTY_TOKEN, TLIST rng, EMPTY_TOKEN -> Itmlst (rml rng)
     | oth -> othpat3 := Some oth; failwith "net_dataTypeE193")
| TUPLE4(STRING("net_declaration186"), arg1, arg2, SEMICOLON) ->
  (match arg1, arg2 with
    | TUPLE6 (STRING "net_declarationFront187", EMPTY_TOKEN, (Wire as typ), EMPTY_TOKEN, EMPTY_TOKEN, EMPTY_TOKEN), TLIST lst ->
      NetDecl(mly typ :: [], rml lst)
    | TUPLE6 (STRING "net_declarationFront187", EMPTY_TOKEN, (Wire as typ), EMPTY_TOKEN, EMPTY_TOKEN, rng), TLIST lst ->
      (match mly rng with Itmlst rng -> NetDecl(mly typ :: rng, rml lst) | oth -> othpat := Some oth; failwith "net_declaration186'")
    | oth -> othpat2 := Some oth; failwith "net_declaration186")
| TUPLE4(STRING("open_range_list744"), arg1, COMMA, arg3) -> OpenRange(match mly arg1, mly arg3 with
     | OpenRange lst, OpenRange lst' -> lst @ lst'
     | OpenRange lst, oth -> lst @ [oth]
     | oth, OpenRange lst' -> oth :: lst'
     | oth, oth' -> oth :: [oth'])
| TUPLE4(STRING("packageClassScope2589"), arg1, COLON_COLON, arg3) as oth -> mayfail oth  "packageClassScope2589"
| TUPLE4(STRING("packageClassScopeItem2595"), arg1, arg2, COLON_COLON) as oth -> mayfail oth  "packageClassScopeItem2595"
| TUPLE4(STRING("package_export_declaration47"), Export, arg2, SEMICOLON) as oth -> mayfail oth  "package_export_declaration47"
| TUPLE4(STRING("package_export_item50"), arg1, COLON_COLON, arg3) as oth -> mayfail oth  "package_export_item50"
| TUPLE4(STRING("package_import_declaration40"), Import, arg2, SEMICOLON)  -> PkgImport(mly arg2)
| TUPLE4(STRING("package_import_item43"), IDENTIFIER_HYPHEN_COLON_COLON arg1, COLON_COLON, arg3) -> PkgImportItm(arg1, mly arg3)
| TUPLE4(STRING("par_blockFront626"), Fork, COLON, arg3) as oth -> mayfail oth  "par_blockFront626"
| TUPLE4(STRING("par_blockFrontPreId628"), arg1, COLON_HYPHEN_fork, Fork) as oth -> mayfail oth  "par_blockFrontPreId628"
| TUPLE4(STRING("parameter_port_listE67"), HASH, LPAREN, RPAREN) as oth -> mayfail oth  "parameter_port_listE67"
| TUPLE4(STRING("patternMemberOne759"), arg1, COLON, arg3) -> PatMember1 (mly arg1, mly arg3)
| TUPLE4(STRING("patternMemberOne760"), arg1, COLON, arg3) as oth -> mayfail oth  "patternMemberOne760"
| TUPLE4(STRING("patternMemberOne761"), Default, COLON, arg3) -> PatMemberDflt(mly arg3)
| TUPLE4(STRING("patternMemberOne762"), Default, COLON, arg3) as oth -> mayfail oth  "patternMemberOne762"
| TUPLE4(STRING("pexpr2558"), arg1, VBAR_HYPHEN_GT, arg3) as oth -> mayfail oth  "pexpr2558"
| TUPLE4(STRING("pexpr2559"), arg1, VBAR_EQ_GT, arg3) as oth -> mayfail oth  "pexpr2559"
| TUPLE4(STRING("pgmFront135"), Program, arg2, arg3) as oth -> mayfail oth  "pgmFront135"
| TUPLE4(STRING("port_declaration231"), arg1, arg2, arg3) -> 
  let dir, lst = match (mly arg1, mly arg2, mly arg3) with
    | dir, Deflt, Itmlst lst -> dir, lst
    | dir, Atom ("wire"|"wand"|"wor"), Itmlst lst -> dir, lst
    | oth -> othport3 := Some oth; failwith "n231" in
    Itmlst (List.map (function Id port -> Port(dir, port,[], Deflt) | oth -> othport := Some oth; failwith "n231'") lst)
| TUPLE4(STRING("portsStarE78"), LPAREN, arg2, RPAREN) ->
(match arg2 with
       | TLIST lst ->  PortsStar(rml lst)
       | oth -> othpat1 := Some oth; failwith "portsStarE78")
| TUPLE4(STRING("senitem602"), LPAREN, arg2, RPAREN) as oth -> mayfail oth  "senitem602"
| TUPLE4(STRING("senitem603"), LBRACE, arg2, RBRACE) as oth -> mayfail oth  "senitem603"
| TUPLE4(STRING("senitem604"), arg1, AMPERSAND_AMPERSAND, arg3) as oth -> mayfail oth  "senitem604"
| TUPLE4(STRING("seq_blockFront624"), Begin, COLON, arg3) as oth -> mayfail oth  "seq_blockFront624"
| TUPLE4(STRING("seq_blockFrontPreId627"), arg1, COLON_HYPHEN_begin, Begin) as oth -> mayfail oth  "seq_blockFrontPreId627"
| TUPLE4(STRING("solve_before_list2636"), arg1, COMMA, arg3) as oth -> mayfail oth  "solve_before_list2636"
| TUPLE4(STRING("specifyJunk1968"), Specify, arg2, Endspecify) as oth -> mayfail oth  "specifyJunk1968"
| TUPLE4(STRING("specify_block1468"), Specify, arg2, Endspecify) -> Unimplemented ("specify_block1468", [])
| TUPLE4(STRING("specparam_declaration1970"), Specparam, arg2, SEMICOLON) -> Unimplemented ("specparam_declaration1970", [])
| TUPLE4(STRING("statement_item650"), Deassign, arg2, SEMICOLON) as oth -> mayfail oth  "statement_item650"
| TUPLE4(STRING("statement_item652"), Release, arg2, SEMICOLON) as oth -> mayfail oth  "statement_item652"
| TUPLE4(STRING("statement_item664"), Disable, arg2, SEMICOLON) as oth -> mayfail oth  "statement_item664"
| TUPLE4(STRING("statement_item665"), Disable, Fork, SEMICOLON) as oth -> mayfail oth  "statement_item665"
| TUPLE4(STRING("statement_item666"), HYPHEN_GT, arg2, SEMICOLON) as oth -> mayfail oth  "statement_item666"
| TUPLE4(STRING("statement_item675"), Return, arg2, SEMICOLON) -> Return (mly arg2)
| TUPLE4(STRING("statement_item683"), Wait, Fork, SEMICOLON) as oth -> mayfail oth  "statement_item683"
| TUPLE4(STRING("statement_item685"), Randcase, arg2, Endcase) as oth -> mayfail oth  "statement_item685"
| TUPLE4(STRING("stmt641"), arg1, COLON, arg3) as oth -> mayfail oth  "stmt641"
| TUPLE4(STRING("stream_concatenation1370"), LBRACE, arg2, RBRACE) as oth -> mayfail oth  "stream_concatenation1370"
| TUPLE4(STRING("strengthSpec1461"), LPAREN_HYPHEN_for_HYPHEN_strength, arg2, RPAREN) as oth -> mayfail oth  "strengthSpec1461"
| TUPLE4(STRING("systemDpiArgsE910"), LPAREN, arg2, RPAREN) as oth -> mayfail oth  "systemDpiArgsE910"
| TUPLE4(STRING("system_t_call887"), DLR_printtimescale, LPAREN, RPAREN) as oth -> mayfail oth  "system_t_call887"
| TUPLE4(STRING("taskId1039"), arg1, DOT, arg3) as oth -> mayfail oth  "taskId1039"
| TUPLE4(STRING("tf_port_itemFront1079"), arg1, arg2, arg3) -> PortItemFront2(mly arg1, mly arg2, match arg3 with TLIST lst -> rml lst | oth -> mly oth :: [])
| TUPLE4(STRING("tf_port_itemFront1081"), arg1, Var, arg3) as oth -> mayfail oth  "tf_port_itemFront1081"
| TUPLE4(STRING("tf_port_itemFront1082"), arg1, Var, arg3) as oth -> mayfail oth  "tf_port_itemFront1082"
| TUPLE4(STRING("timeunits_declaration17"), Timeunit, TIME_NUMBER, SEMICOLON) as oth -> mayfail oth  "timeunits_declaration17"
| TUPLE4(STRING("timeunits_declaration19"), Timeprecision, TIME_NUMBER, SEMICOLON) as oth -> mayfail oth  "timeunits_declaration19"
| TUPLE4(STRING("type_declaration371"), Typedef, arg2, SEMICOLON) as oth -> mayfail oth  "type_declaration371"
| TUPLE4(STRING("type_declaration372"), Typedef, arg2, SEMICOLON) as oth -> mayfail oth  "type_declaration372"
| TUPLE4(STRING("udpFront57"), Primitive, arg2, arg3) as oth -> mayfail oth  "udpFront57"
| TUPLE4(STRING("variable_decl_assignment297"), arg1, arg2, arg3) ->
(match (arg1, arg2, arg3) with
       | IDENTIFIER id, EMPTY_TOKEN, EMPTY_TOKEN -> Id id
       | IDENTIFIER id, TLIST lst, EMPTY_TOKEN -> DeclAsgn(mly arg1, rml lst)
       | oth -> othpat3 := Some oth; failwith "variable_decl_assignment297")
| TUPLE4(STRING("variable_dimension313"), LBRACK, arg2, RBRACK) -> VarDim(mly arg2)
| TUPLE4(STRING("variable_dimension315"), LBRACK, STAR, RBRACK) as oth -> mayfail oth  "variable_dimension315"
| TUPLE4(STRING("variable_lvalue2480"), LBRACE, arg2, RBRACE) -> Concat(match arg2 with TLIST lst -> rml lst | oth -> mly oth :: [])
| TUPLE4(STRING("vltItem2661"), arg1, HYPHEN_HYPHEN_file, STRING arg3) as oth -> mayfail oth  "vltItem2661"
| TUPLE4(STRING("vltItem2666"), arg1, HYPHEN_HYPHEN_file, STRING arg3) as oth -> mayfail oth  "vltItem2666"
| TUPLE4(STRING("vltItem2670"), arg1, arg2, arg3) as oth -> mayfail oth  "vltItem2670"
| TUPLE4(STRING("vltItem2671"), Coverage_block_off, HYPHEN_HYPHEN_file, STRING arg3) as oth -> mayfail oth  "vltItem2671"
| TUPLE4(STRING("vltItem2674"), Full_case, HYPHEN_HYPHEN_file, STRING arg3) as oth -> mayfail oth  "vltItem2674"
| TUPLE4(STRING("vltItem2677"), Parallel_case, HYPHEN_HYPHEN_file, STRING arg3) as oth -> mayfail oth  "vltItem2677"
| TUPLE4(STRING("vltOffFront2682"), Lint_off, HYPHEN_HYPHEN_msg, arg3) as oth -> mayfail oth  "vltOffFront2682"
| TUPLE4(STRING("vltOffFront2683"), Lint_off, HYPHEN_HYPHEN_rule, arg3) as oth -> mayfail oth  "vltOffFront2683"
| TUPLE4(STRING("vltOnFront2687"), Lint_on, HYPHEN_HYPHEN_msg, arg3) as oth -> mayfail oth  "vltOnFront2687"
| TUPLE4(STRING("vltOnFront2688"), Lint_on, HYPHEN_HYPHEN_rule, arg3) as oth -> mayfail oth  "vltOnFront2688"
| TUPLE5(STRING("anonymous_program124"), Program, SEMICOLON, arg3, Endprogram) as oth -> mayfail oth  "anonymous_program124"
| TUPLE5(STRING("argsDotted1364"), DOT, arg2, LPAREN, RPAREN) as oth -> mayfail oth  "argsDotted1364"
| TUPLE5(STRING("attr_event_control590"), AT, LPAREN, arg3, RPAREN) as oth -> mayfail oth  "attr_event_control590"
| TUPLE5(STRING("attr_event_control591"), AT, LPAREN, STAR, RPAREN) as oth -> mayfail oth  "attr_event_control591"
| TUPLE5(STRING("caseStart721"), Case, LPAREN, arg3, RPAREN) -> CaseStart1 (mly arg3)
| TUPLE5(STRING("caseStart722"), Casex, LPAREN, arg3, RPAREN) as oth -> mayfail oth  "caseStart722"
| TUPLE5(STRING("caseStart723"), Casez, LPAREN, arg3, RPAREN) as oth -> mayfail oth  "caseStart723"
| TUPLE5(STRING("cellparamItemE578"), DOT, arg2, LPAREN, RPAREN) as oth -> mayfail oth  "cellparamItemE578"
| TUPLE5(STRING("cellpinItemE585"), DOT, arg2, LPAREN, RPAREN) ->
CellPinItemNC(match arg2 with IDENTIFIER id -> id | oth -> failwith "cellpinItemE585")
| TUPLE5(STRING("classExtendsOne2572"), arg1, LPAREN, arg3, RPAREN) as oth -> mayfail oth  "classExtendsOne2572"
| TUPLE5(STRING("classFront2563"), arg1, Class, arg3, arg4) as oth -> mayfail oth  "classFront2563"
| TUPLE5(STRING("classFront2564"), Interface, Class, arg3, arg4) as oth -> mayfail oth  "classFront2564"
| TUPLE5(STRING("class_constraint2626"), arg1, Constraint, arg3, arg4) as oth -> mayfail oth  "class_constraint2626"
| TUPLE5(STRING("class_constraint2627"), arg1, Constraint, arg3, SEMICOLON) as oth -> mayfail oth  "class_constraint2627"
| TUPLE5(STRING("class_method2613"), Extern, arg2, arg3, SEMICOLON) as oth -> mayfail oth  "class_method2613"
| TUPLE5(STRING("class_new714"), New_HYPHEN_then_HYPHEN_paren, LPAREN, arg3, RPAREN) as oth -> mayfail oth  "class_new714"
| TUPLE5(STRING("constraint_expression2642"), Unique, LBRACE, arg3, RBRACE) as oth -> mayfail oth  "constraint_expression2642"
| TUPLE5(STRING("constraint_expression2646"), Disable, Soft, arg3, SEMICOLON) as oth -> mayfail oth  "constraint_expression2646"
| TUPLE5(STRING("data_declarationVarFront351"), Var, arg2, arg3, arg4) as oth -> mayfail oth  "data_declarationVarFront351"
| TUPLE5(STRING("data_declarationVarFront352"), Const, Var, arg3, arg4) as oth -> mayfail oth  "data_declarationVarFront352"
| TUPLE5(STRING("data_declarationVarFrontClass360"), Var, arg2, arg3, arg4) as oth -> mayfail oth  "data_declarationVarFrontClass360"
| TUPLE5(STRING("data_type262"), arg1, arg2, arg3, arg4) as oth -> mayfail oth  "data_type262"
| TUPLE5(STRING("defparam_assignment546"), arg1, DOT, arg3, DOT) as oth -> mayfail oth  "defparam_assignment546"
| TUPLE5(STRING("delay_control493"), HASH, LPAREN, arg3, RPAREN) as oth -> mayfail oth  "delay_control493"
| TUPLE5(STRING("dynamic_array_new715"), New, LBRACK, arg3, RBRACK) as oth -> mayfail oth  "dynamic_array_new715"
| TUPLE5(STRING("elaboration_system_task_guts1008"), DLR_info, LPAREN, arg3, RPAREN) as oth -> mayfail oth  "elaboration_system_task_guts1008"
| TUPLE5(STRING("elaboration_system_task_guts1010"), DLR_warning, LPAREN, arg3, RPAREN) as oth -> mayfail oth  "elaboration_system_task_guts1010"
| TUPLE5(STRING("elaboration_system_task_guts1012"), DLR_error, LPAREN, arg3, RPAREN) as oth -> mayfail oth  "elaboration_system_task_guts1012"
| TUPLE5(STRING("elaboration_system_task_guts1014"), DLR_fatal, LPAREN, arg3, RPAREN) as oth -> mayfail oth  "elaboration_system_task_guts1014"
| TUPLE5(STRING("event_control593"), AT, LPAREN, arg3, RPAREN) -> At(match mly arg3 with EventOr _ as x -> x | oth -> EventOr [oth])
| TUPLE5(STRING("event_control594"), AT, LPAREN, STAR, RPAREN) -> AtStar
| TUPLE5(STRING("expr1161"), UNDERSCORE, LPAREN, arg3, RPAREN) as oth -> mayfail oth  "expr1161"
| TUPLE5(STRING("exprNoStr1293"), UNDERSCORE, LPAREN, arg3, RPAREN) as oth -> mayfail oth  "exprNoStr1293"
| TUPLE5(STRING("extern_tf_declaration155"), Extern, Forkjoin, arg3, SEMICOLON) as oth -> mayfail oth  "extern_tf_declaration155"
| TUPLE5(STRING("fexpr1227"), UNDERSCORE, LPAREN, arg3, RPAREN) as oth -> mayfail oth  "fexpr1227"
| TUPLE5(STRING("foperator_assignment690"), arg1, EQUALS, arg3, arg4) ->
( match arg1, arg3, arg4 with
	| IDENTIFIER id, EMPTY_TOKEN, expr -> FopAsgn(mly arg1, mly expr)
	| TUPLE4 (STRING "fexprOkLvalue1316", LBRACE, TLIST lst, RBRACE), EMPTY_TOKEN, expr -> FopAsgnConcat(rml lst, mly expr)
        | TUPLE5 (STRING "idArrayed2503", IDENTIFIER id, LBRACK, sel, RBRACK), EMPTY_TOKEN, expr -> FopAsgnArraySel(Id id, mly sel, mly expr)
        | TUPLE5 (STRING "idArrayed2503", TUPLE5 (STRING "idArrayed2503", IDENTIFIER id, LBRACK, sel, RBRACK), LBRACK, sel', RBRACK), EMPTY_TOKEN, expr ->
            FopAsgnArrayMemSel(Id id, mly sel, mly sel', mly expr)
        | TUPLE7 (STRING "idArrayed2504", IDENTIFIER id, LBRACK, hi, COLON, lo, RBRACK), EMPTY_TOKEN, expr -> FopAsgnArrayRange(Id id, mly hi, mly lo, mly expr)
        | TUPLE7 (STRING "idArrayed2504", array, LBRACK, hi, COLON, lo, RBRACK), EMPTY_TOKEN, expr -> FopAsgnArrayRange2(mly array, mly hi, mly lo, mly expr)
        | TUPLE7 (STRING "idArrayed2505", IDENTIFIER id, LBRACK, hi, PLUS_COLON, lo, RBRACK), EMPTY_TOKEN, expr -> FopAsgnArrayWid(Id id, mly hi, mly lo, mly expr)
        | TUPLE4 (STRING "fexprScope1336", IDENTIFIER id, DOT, IDENTIFIER field), EMPTY_TOKEN, expr ->  FopAsgnArrayField(Id id, Id field, mly expr)
        | TUPLE4 (STRING "fexprScope1336", IDENTIFIER id, DOT, (TUPLE7 (STRING "idArrayed2504",_,_,_,_,_,_) as x)), EMPTY_TOKEN, expr -> FopAsgnArrayField2(Id id, mly x, mly expr)
        | TUPLE4 (STRING "fexprScope1336", TUPLE5 (STRING "idArrayed2503", IDENTIFIER id, LBRACK, sel, RBRACK), DOT, IDENTIFIER field), EMPTY_TOKEN, expr -> 
            FopAsgnArrayField3(Id id, mly sel, Id field, mly expr)
        | TUPLE4 (STRING "fexprScope1336", TUPLE5 (STRING "idArrayed2503", IDENTIFIER id, LBRACK, sel, RBRACK), DOT, TUPLE7 (STRING "idArrayed2505", IDENTIFIER id', LBRACK, sel', PLUS_COLON, wid, RBRACK)), EMPTY_TOKEN, expr -> 
        FopAsgnArrayField4(Id id, mly sel, Id id', mly sel', mly wid, mly expr)
        | TUPLE4 (STRING "fexprScope1336", TUPLE5 (STRING "idArrayed2503", IDENTIFIER id, LBRACK, sel, RBRACK), DOT, TUPLE5 (STRING "idArrayed2503", IDENTIFIER id', LBRACK, sel', RBRACK)), EMPTY_TOKEN, expr -> 
            FopAsgnArrayField5(Id id, mly sel, Id id, mly sel', mly expr)
	| TUPLE4 (STRING "fexprScope1336", IDENTIFIER id, DOT, TUPLE7 (STRING "idArrayed2505", IDENTIFIER id', LBRACK, sel,PLUS_COLON, wid, RBRACK)), EMPTY_TOKEN, expr ->
        FopAsgnArrayField6(Id id, Id id', mly sel, mly wid, mly expr)
        | TUPLE4 (STRING "fexprScope1336", TUPLE4 (STRING "fexprScope1336", IDENTIFIER id, DOT, IDENTIFIER id'), DOT, IDENTIFIER id''), EMPTY_TOKEN, expr ->
	FopAsgn1(Id id, Id id', Id id'', mly expr)
	| TUPLE4 (STRING "fexprScope1336", TUPLE5 (STRING "idArrayed2503", TUPLE5 (STRING "idArrayed2503", IDENTIFIER id, LBRACK, sel, RBRACK), LBRACK, sel', RBRACK), DOT, IDENTIFIER id'), EMPTY_TOKEN, expr ->
        FopAsgnArrayField7(Id id, mly sel, mly sel', Id id', mly expr)
        | TUPLE4 (STRING "fexprScope1336", TUPLE4 (STRING "fexprScope1336", TUPLE5 (STRING "idArrayed2503", IDENTIFIER id, LBRACK, sel, RBRACK), DOT, IDENTIFIER id'), DOT, IDENTIFIER id''), EMPTY_TOKEN, expr -> FopAsgnArrayField8(Id id, mly sel, Id id', Id id'', mly expr)
        | TUPLE4 (STRING "fexprScope1336", TUPLE4 (STRING "fexprScope1336", TUPLE4 (STRING "fexprScope1336", TUPLE5 (STRING "idArrayed2503", IDENTIFIER id, LBRACK, sel, RBRACK), DOT, IDENTIFIER id'), DOT, IDENTIFIER id''), DOT, IDENTIFIER id'''), EMPTY_TOKEN, expr -> FopAsgnArrayField9(Id id, mly sel, Id id', Id id'', Id id''', mly expr)
        | oth -> othpat3 := Some oth; failwith "foperator_assignment690")
| TUPLE5(STRING("for_initializationItem774"), arg1, arg2, EQUALS, arg4) ->
(match arg1, arg2, arg4 with
  | TUPLE4 (STRING "data_typeBasic263", Logic, EMPTY_TOKEN, TLIST lst), IDENTIFIER k, expr -> Typ9(k, rml lst, mly Logic)
  | TUPLE3 (STRING "data_typeBasic264", Int, Unsigned), IDENTIFIER k, expr -> Typ7(k, Atom "unsigned_int")
  | TUPLE3 (STRING "data_typeBasic264", Int, EMPTY_TOKEN), IDENTIFIER k, expr -> Typ7(k, Atom "int")
  | oth -> othpat3 := Some oth; failwith "for_initializationItem774")
| TUPLE5(STRING("funcRef787"), arg1, LPAREN, arg3, RPAREN) ->
( match arg1, arg3 with
	 | IDENTIFIER id, TLIST ((IDENTIFIER _|INTEGER_NUMBER _|STRING _) :: _ as lst) -> FunRef(id, rml lst)
	 | IDENTIFIER id, TLIST (TUPLE5 _::_ as lst) -> FunRef(id, rml lst)
         | IDENTIFIER id, TLIST (TUPLE4 (STRING "argsExprListE1359", lft, COMMA, rght) :: []) ->
	     FunRef(id, match (mly lft, mly rght) with
		  | Itmlst lst, Itmlst lst' -> lst@lst'
		  | hd, Itmlst lst -> hd::lst
		  | Itmlst lst, oth -> lst@[oth]
		  | oth, oth' -> oth::[oth'])
         | oth -> othpat2 := Some oth; failwith "funcRef787")
| TUPLE5(STRING("function_subroutine_callNoMethod798"), arg1, With_HYPHEN_then_HYPHEN_LBRACE, LBRACE, RBRACE) as oth -> mayfail oth  "function_subroutine_callNoMethod798"
| TUPLE5(STRING("gateDecl1373"), Buf, arg2, arg3, SEMICOLON) as oth -> mayfail oth  "gateDecl1373"
| TUPLE5(STRING("gateDecl1374"), Bufif0, arg2, arg3, SEMICOLON) as oth -> mayfail oth  "gateDecl1374"
| TUPLE5(STRING("gateDecl1375"), Bufif1, arg2, arg3, SEMICOLON) as oth -> mayfail oth  "gateDecl1375"
| TUPLE5(STRING("gateDecl1376"), Not, arg2, arg3, SEMICOLON) as oth -> mayfail oth  "gateDecl1376"
| TUPLE5(STRING("gateDecl1377"), Notif0, arg2, arg3, SEMICOLON) as oth -> mayfail oth  "gateDecl1377"
| TUPLE5(STRING("gateDecl1378"), Notif1, arg2, arg3, SEMICOLON) as oth -> mayfail oth  "gateDecl1378"
| TUPLE5(STRING("gateDecl1379"), And, arg2, arg3, SEMICOLON) as oth -> mayfail oth  "gateDecl1379"
| TUPLE5(STRING("gateDecl1380"), Nand, arg2, arg3, SEMICOLON) as oth -> mayfail oth  "gateDecl1380"
| TUPLE5(STRING("gateDecl1381"), Or, arg2, arg3, SEMICOLON) as oth -> mayfail oth  "gateDecl1381"
| TUPLE5(STRING("gateDecl1382"), Nor, arg2, arg3, SEMICOLON) as oth -> mayfail oth  "gateDecl1382"
| TUPLE5(STRING("gateDecl1383"), Xor, arg2, arg3, SEMICOLON) as oth -> mayfail oth  "gateDecl1383"
| TUPLE5(STRING("gateDecl1384"), Xnor, arg2, arg3, SEMICOLON) as oth -> mayfail oth  "gateDecl1384"
| TUPLE5(STRING("gateDecl1385"), Pullup, arg2, arg3, SEMICOLON) as oth -> mayfail oth  "gateDecl1385"
| TUPLE5(STRING("gateDecl1386"), Pulldown, arg2, arg3, SEMICOLON) as oth -> mayfail oth  "gateDecl1386"
| TUPLE5(STRING("gateDecl1387"), Nmos, arg2, arg3, SEMICOLON) as oth -> mayfail oth  "gateDecl1387"
| TUPLE5(STRING("gateDecl1388"), Pmos, arg2, arg3, SEMICOLON) as oth -> mayfail oth  "gateDecl1388"
| TUPLE5(STRING("gateDecl1389"), Tran, arg2, arg3, SEMICOLON) as oth -> mayfail oth  "gateDecl1389"
| TUPLE5(STRING("gateDecl1390"), Rcmos, arg2, arg3, SEMICOLON) as oth -> mayfail oth  "gateDecl1390"
| TUPLE5(STRING("gateDecl1391"), Cmos, arg2, arg3, SEMICOLON) as oth -> mayfail oth  "gateDecl1391"
| TUPLE5(STRING("gateDecl1392"), Rnmos, arg2, arg3, SEMICOLON) as oth -> mayfail oth  "gateDecl1392"
| TUPLE5(STRING("gateDecl1393"), Rpmos, arg2, arg3, SEMICOLON) as oth -> mayfail oth  "gateDecl1393"
| TUPLE5(STRING("gateDecl1394"), Rtran, arg2, arg3, SEMICOLON) as oth -> mayfail oth  "gateDecl1394"
| TUPLE5(STRING("gateDecl1395"), Rtranif0, arg2, arg3, SEMICOLON) as oth -> mayfail oth  "gateDecl1395"
| TUPLE5(STRING("gateDecl1396"), Rtranif1, arg2, arg3, SEMICOLON) as oth -> mayfail oth  "gateDecl1396"
| TUPLE5(STRING("gateDecl1397"), Tranif0, arg2, arg3, SEMICOLON) as oth -> mayfail oth  "gateDecl1397"
| TUPLE5(STRING("gateDecl1398"), Tranif1, arg2, arg3, SEMICOLON) as oth -> mayfail oth  "gateDecl1398"
| TUPLE5(STRING("genvar_initialization462"), Genvar, arg2, EQUALS, arg4) as oth -> mayfail oth  "genvar_initialization462"
| TUPLE5(STRING("idArrayed2503"), arg1, LBRACK, arg3, RBRACK) -> IdArrayed2(mly arg1, mly arg3)
| TUPLE5(STRING("idArrayedForeach2508"), arg1, LBRACK, arg3, RBRACK) as oth -> mayfail oth  "idArrayedForeach2508"
| TUPLE5(STRING("instDecl550"), arg1, arg2, arg3, SEMICOLON) ->
 (match arg1,arg2,arg3 with
  | IDENTIFIER id, TUPLE5 (STRING "parameter_value_assignment60", HASH, LPAREN, TLIST lst, RPAREN), TLIST lst' -> InstDecl(Id id, rml lst, rml lst')
  | IDENTIFIER id, EMPTY_TOKEN, TLIST (TUPLE6 (STRING "instnameParen558", IDENTIFIER inst, TLIST rng, LPAREN, TLIST [TLIST lst], RPAREN) :: []) ->
      InstArrayDecl(Id id, [], Id inst, rml rng, rml lst)
  | IDENTIFIER id, EMPTY_TOKEN, TLIST lst -> InstDecl(Id id, [], rml lst)
  | oth -> othpat3 := Some oth; failwith "instDecl550")
| TUPLE5(STRING("member_decl_assignment292"), arg1, arg2, EQUALS, arg4) as oth -> mayfail oth  "member_decl_assignment292"
| TUPLE5(STRING("modport_item160"), arg1, LPAREN, arg3, RPAREN) -> 
(match arg1,arg3 with IDENTIFIER id, TLIST lst -> ModPortItm(id, rml lst) | oth -> othpat2 := Some oth; failwith "modport_item160")
| TUPLE5(STRING("module_common_item416"), Alias, arg2, arg3, SEMICOLON) as oth -> mayfail oth  "module_common_item416"
| TUPLE5(STRING("module_or_generate_item_declaration433"), Default, Clocking, arg3, SEMICOLON) as oth -> mayfail oth  "module_or_generate_item_declaration433"
| TUPLE5(STRING("netSig506"), arg1, arg2, EQUALS, arg4) ->
    (match arg1,arg2,arg4 with 
        | IDENTIFIER id, EMPTY_TOKEN, expr -> InitSig(Id id, mly expr)
        | oth -> othpat3 := Some oth; failwith "netSig506")
| TUPLE5(STRING("packageFront21"), Package, arg2, arg3, SEMICOLON) as oth -> mayfail oth  "packageFront21"
| TUPLE5(STRING("package_declaration20"), arg1, arg2, Endpackage, arg4) ->
(match arg1, arg2, arg4 with
       | TUPLE5 (STRING "packageFront21", Package, EMPTY_TOKEN, IDENTIFIER_HYPHEN_COLON_COLON pkg_cc, SEMICOLON),
         TLIST lst,
         (EMPTY_TOKEN|TUPLE3 (STRING "endLabelE2519", COLON, IDENTIFIER_HYPHEN_COLON_COLON _)) ->
	 let p = PackageBody (pkg_cc, rml lst) in modules := (pkg_cc,p) :: !modules; p
       | oth -> othpat3 := Some oth; failwith "package_declaration20")
| TUPLE5(STRING("par_block617"), arg1, arg2, Join, arg4) as oth -> mayfail oth  "par_block617"
| TUPLE5(STRING("par_block618"), arg1, arg2, Join_any, arg4) as oth -> mayfail oth  "par_block618"
| TUPLE5(STRING("par_block619"), arg1, arg2, Join_none, arg4) as oth -> mayfail oth  "par_block619"
| TUPLE5(STRING("par_blockPreId620"), arg1, arg2, Join, arg4) as oth -> mayfail oth  "par_blockPreId620"
| TUPLE5(STRING("par_blockPreId621"), arg1, arg2, Join_any, arg4) as oth -> mayfail oth  "par_blockPreId621"
| TUPLE5(STRING("par_blockPreId622"), arg1, arg2, Join_none, arg4) as oth -> mayfail oth  "par_blockPreId622"
| TUPLE5(STRING("param_assignment537"), arg1, arg2, arg3, arg4) ->
    (match arg1, arg2, arg3, arg4 with
      | IDENTIFIER id, EMPTY_TOKEN, EMPTY_TOKEN, expr_typ -> ParamAsgn1(id, mly expr_typ)
      | IDENTIFIER id, TLIST lst, EMPTY_TOKEN, expr_typ -> ParamAsgn2(id, rml lst, mly expr_typ)
      | oth -> othpat4 := Some oth; failwith "param_assignment537")
| TUPLE5(STRING("parameter_port_listE69"), HASH, LPAREN, arg3, RPAREN) ->
(match arg3 with
       | TLIST lst -> ParamPort(rml lst)
       | oth -> othpat1 := Some oth; failwith "parameter_port_listE69")
| TUPLE5(STRING("parameter_value_assignment60"), HASH, LPAREN, arg3, RPAREN) as oth -> mayfail oth  "parameter_value_assignment60"
| TUPLE5(STRING("parameter_value_assignmentClass65"), HASH, LPAREN, arg3, RPAREN) as oth -> mayfail oth  "parameter_value_assignmentClass65"
| TUPLE5(STRING("patternOne755"), arg1, LBRACE, arg3, RBRACE) as oth -> mayfail oth  "patternOne755"
| TUPLE5(STRING("port92"), dir, port, arg3, arg4) ->
    (match arg3, arg4, port with
      | EMPTY_TOKEN, EMPTY_TOKEN, IDENTIFIER port -> Port(mly dir, port, [], Deflt)
      | oth -> othpat3 := Some oth; failwith "port92")
| TUPLE5(STRING("port_declaration221"), dir, arg2, arg3, arg4) ->
    (match arg3, arg4 with
      | TUPLE4 (STRING "data_typeBasic263", (Reg|Logic as x), EMPTY_TOKEN, rng), TLIST lst ->
          Itmlst (List.map (function
	      | TUPLE4 (STRING "variable_decl_assignment297", IDENTIFIER port, EMPTY_TOKEN, EMPTY_TOKEN) ->
                  Port(PortDir(mly dir, mly x), port, (match rng with EMPTY_TOKEN -> [] | TLIST rng -> rml rng | oth -> [mly rng]), Deflt)
              | oth -> othpat1 := Some oth; failwith "port_declaration221'") lst)
      | oth -> othpat2 := Some oth; failwith "port_declaration221")
| TUPLE5(STRING("port_declaration229"), arg1, arg2, arg3, arg4) as oth -> mayfail oth  "port_declaration229"
| TUPLE5(STRING("senitemEdge611"), Posedge, LPAREN, arg3, RPAREN) -> Pos (mly arg3)
| TUPLE5(STRING("senitemEdge612"), Negedge, LPAREN, arg3, RPAREN) -> Neg (mly arg3)
| TUPLE5(STRING("senitemEdge613"), Edge, LPAREN, arg3, RPAREN) as oth -> mayfail oth  "senitemEdge613"
| TUPLE5(STRING("seq_block615"), arg1, arg2, End, arg4) ->
    (match arg1, arg2, arg4 with
      | Begin, EMPTY_TOKEN, EMPTY_TOKEN -> Seq ("", [])
      | Begin, TLIST (TLIST lst :: []), EMPTY_TOKEN -> Seq ("", rml lst)
      | Begin, TLIST (TLIST lst :: (TUPLE4(STRING "data_declarationVar347", _, _, _) :: _ as lst')), EMPTY_TOKEN -> Seq ("", rml lst' @ rml lst)
      | TUPLE4 (STRING "seq_blockFront624", Begin, COLON, IDENTIFIER lbl), TLIST (TLIST lst :: []),_ -> Seq(lbl, rml lst)
      | TUPLE4 (STRING "seq_blockFront624", Begin, COLON, IDENTIFIER lbl), TLIST (TLIST lst :: itm),_ -> Seq(lbl, rml itm @ rml lst)
(*
       | Begin, TLIST lst, EMPTY_TOKEN -> portlstref := lst; failwith "lst"; Seq ("", rml lst)
*)
      | oth -> othpat3 := Some oth; failwith "seq_block615")
| TUPLE5(STRING("seq_blockPreId616"), arg1, arg2, End, arg4) as oth -> mayfail oth  "seq_blockPreId616"
| TUPLE5(STRING("statement_item646"), arg1, EQUALS, arg3, SEMICOLON) as oth -> mayfail oth  "statement_item646"
| TUPLE5(STRING("statement_item647"), arg1, EQUALS, arg3, SEMICOLON) as oth -> mayfail oth  "statement_item647"
| TUPLE5(STRING("statement_item661"), arg1, DOT, arg3, SEMICOLON) as oth -> mayfail oth  "statement_item661"
| TUPLE5(STRING("statement_item662"), arg1, DOT, arg3, SEMICOLON) as oth -> mayfail oth  "statement_item662"
| TUPLE5(STRING("statement_item667"), HYPHEN_GT_GT, arg2, arg3, SEMICOLON) as oth -> mayfail oth  "statement_item667"
| TUPLE5(STRING("streaming_concatenation1366"), LBRACE, LT_LT, arg3, RBRACE) as oth -> mayfail oth  "streaming_concatenation1366"
| TUPLE5(STRING("streaming_concatenation1367"), LBRACE, GT_GT, arg3, RBRACE) as oth -> mayfail oth  "streaming_concatenation1367"
| TUPLE5(STRING("struct_union_member287"), EMPTY_TOKEN, typ, TLIST lst, SEMICOLON) -> SUMember (mly typ, rml lst)
| TUPLE5(STRING("struct_union_member287"), arg1, arg2, arg3, SEMICOLON) as oth -> mayfail oth  "struct_union_member287"
| TUPLE5(STRING("system_f_call905"), DLR_c, LPAREN, arg3, RPAREN) as oth -> mayfail oth  "system_f_call905"
| TUPLE5(STRING("system_f_call907"), DLR_system, LPAREN, arg3, RPAREN) as oth -> mayfail oth  "system_f_call907"
| TUPLE5(STRING("system_f_call_or_t1000"), DLR_unsigned, LPAREN, arg3, RPAREN) -> Sys("$unsigned", mly arg3)
| TUPLE5(STRING("system_f_call_or_t1001"), DLR_urandom, LPAREN, arg3, RPAREN) as oth -> mayfail oth  "system_f_call_or_t1001"
| TUPLE5(STRING("system_f_call_or_t1003"), DLR_urandom_range, LPAREN, arg3, RPAREN) as oth -> mayfail oth  "system_f_call_or_t1003"
| TUPLE5(STRING("system_f_call_or_t911"), DLR_acos, LPAREN, arg3, RPAREN) as oth -> mayfail oth  "system_f_call_or_t911"
| TUPLE5(STRING("system_f_call_or_t912"), DLR_acosh, LPAREN, arg3, RPAREN) as oth -> mayfail oth  "system_f_call_or_t912"
| TUPLE5(STRING("system_f_call_or_t913"), DLR_asin, LPAREN, arg3, RPAREN) as oth -> mayfail oth  "system_f_call_or_t913"
| TUPLE5(STRING("system_f_call_or_t914"), DLR_asinh, LPAREN, arg3, RPAREN) as oth -> mayfail oth  "system_f_call_or_t914"
| TUPLE5(STRING("system_f_call_or_t915"), DLR_atan, LPAREN, arg3, RPAREN) as oth -> mayfail oth  "system_f_call_or_t915"
| TUPLE5(STRING("system_f_call_or_t917"), DLR_atanh, LPAREN, arg3, RPAREN) as oth -> mayfail oth  "system_f_call_or_t917"
| TUPLE5(STRING("system_f_call_or_t918"), DLR_bits, LPAREN, arg3, RPAREN) -> Sys("$bits", mly arg3)
| TUPLE5(STRING("system_f_call_or_t920"), DLR_bitstoreal, LPAREN, arg3, RPAREN) as oth -> mayfail oth  "system_f_call_or_t920"
| TUPLE5(STRING("system_f_call_or_t921"), DLR_bitstoshortreal, LPAREN, arg3, RPAREN) as oth -> mayfail oth  "system_f_call_or_t921"
| TUPLE5(STRING("system_f_call_or_t922"), DLR_ceil, LPAREN, arg3, RPAREN) as oth -> mayfail oth  "system_f_call_or_t922"
| TUPLE5(STRING("system_f_call_or_t923"), DLR_changed, LPAREN, arg3, RPAREN) as oth -> mayfail oth  "system_f_call_or_t923"
| TUPLE5(STRING("system_f_call_or_t925"), DLR_clog2, LPAREN, arg3, RPAREN) -> Sys("$clog2", mly arg3)
| TUPLE5(STRING("system_f_call_or_t926"), DLR_cos, LPAREN, arg3, RPAREN) as oth -> mayfail oth  "system_f_call_or_t926"
| TUPLE5(STRING("system_f_call_or_t927"), DLR_cosh, LPAREN, arg3, RPAREN) as oth -> mayfail oth  "system_f_call_or_t927"
| TUPLE5(STRING("system_f_call_or_t932"), DLR_countones, LPAREN, arg3, RPAREN) as oth -> mayfail oth  "system_f_call_or_t932"
| TUPLE5(STRING("system_f_call_or_t933"), DLR_dimensions, LPAREN, arg3, RPAREN) as oth -> mayfail oth  "system_f_call_or_t933"
| TUPLE5(STRING("system_f_call_or_t934"), DLR_exp, LPAREN, arg3, RPAREN) as oth -> mayfail oth  "system_f_call_or_t934"
| TUPLE5(STRING("system_f_call_or_t935"), DLR_fell, LPAREN, arg3, RPAREN) as oth -> mayfail oth  "system_f_call_or_t935"
| TUPLE5(STRING("system_f_call_or_t937"), DLR_feof, LPAREN, arg3, RPAREN) -> Sys("$feof", mly arg3)
| TUPLE5(STRING("system_f_call_or_t939"), DLR_fgetc, LPAREN, arg3, RPAREN) as oth -> mayfail oth  "system_f_call_or_t939"
| TUPLE5(STRING("system_f_call_or_t944"), DLR_frewind, LPAREN, arg3, RPAREN) as oth -> mayfail oth  "system_f_call_or_t944"
| TUPLE5(STRING("system_f_call_or_t945"), DLR_floor, LPAREN, arg3, RPAREN) as oth -> mayfail oth  "system_f_call_or_t945"
| TUPLE5(STRING("system_f_call_or_t948"), DLR_ftell, LPAREN, arg3, RPAREN) as oth -> mayfail oth  "system_f_call_or_t948"
| TUPLE5(STRING("system_f_call_or_t949"), DLR_high, LPAREN, arg3, RPAREN) -> Sys("$high", mly arg3)
| TUPLE5(STRING("system_f_call_or_t952"), DLR_increment, LPAREN, arg3, RPAREN) as oth -> mayfail oth  "system_f_call_or_t952"
| TUPLE5(STRING("system_f_call_or_t954"), DLR_isunbounded, LPAREN, arg3, RPAREN) as oth -> mayfail oth  "system_f_call_or_t954"
| TUPLE5(STRING("system_f_call_or_t955"), DLR_isunknown, LPAREN, arg3, RPAREN) as oth -> mayfail oth  "system_f_call_or_t955"
| TUPLE5(STRING("system_f_call_or_t956"), DLR_itor, LPAREN, arg3, RPAREN) as oth -> mayfail oth  "system_f_call_or_t956"
| TUPLE5(STRING("system_f_call_or_t957"), DLR_left, LPAREN, arg3, RPAREN) as oth -> mayfail oth  "system_f_call_or_t957"
| TUPLE5(STRING("system_f_call_or_t959"), DLR_ln, LPAREN, arg3, RPAREN) as oth -> mayfail oth  "system_f_call_or_t959"
| TUPLE5(STRING("system_f_call_or_t960"), DLR_log10, LPAREN, arg3, RPAREN) as oth -> mayfail oth  "system_f_call_or_t960"
| TUPLE5(STRING("system_f_call_or_t961"), DLR_low, LPAREN, arg3, RPAREN) -> Sys("$low", mly arg3)
| TUPLE5(STRING("system_f_call_or_t963"), DLR_onehot, LPAREN, arg3, RPAREN) as oth -> mayfail oth  "system_f_call_or_t963"
| TUPLE5(STRING("system_f_call_or_t964"), DLR_onehot0, LPAREN, arg3, RPAREN) as oth -> mayfail oth  "system_f_call_or_t964"
| TUPLE5(STRING("system_f_call_or_t965"), DLR_past, LPAREN, arg3, RPAREN) as oth -> mayfail oth  "system_f_call_or_t965"
| TUPLE5(STRING("system_f_call_or_t970"), DLR_random, LPAREN, arg3, RPAREN) as oth -> mayfail oth  "system_f_call_or_t970"
| TUPLE5(STRING("system_f_call_or_t973"), DLR_realtobits, LPAREN, arg3, RPAREN) as oth -> mayfail oth  "system_f_call_or_t973"
| TUPLE5(STRING("system_f_call_or_t974"), DLR_rewind, LPAREN, arg3, RPAREN) as oth -> mayfail oth  "system_f_call_or_t974"
| TUPLE5(STRING("system_f_call_or_t975"), DLR_right, LPAREN, arg3, RPAREN) as oth -> mayfail oth  "system_f_call_or_t975"
| TUPLE5(STRING("system_f_call_or_t977"), DLR_rose, LPAREN, arg3, RPAREN) as oth -> mayfail oth  "system_f_call_or_t977"
| TUPLE5(STRING("system_f_call_or_t979"), DLR_rtoi, LPAREN, arg3, RPAREN) as oth -> mayfail oth  "system_f_call_or_t979"
| TUPLE5(STRING("system_f_call_or_t980"), DLR_sampled, LPAREN, arg3, RPAREN) as oth -> mayfail oth  "system_f_call_or_t980"
| TUPLE5(STRING("system_f_call_or_t981"), DLR_sformatf, LPAREN, arg3, RPAREN) -> Sys("$sformatf", mly arg3)
| TUPLE5(STRING("system_f_call_or_t982"), DLR_shortrealtobits, LPAREN, arg3, RPAREN) as oth -> mayfail oth  "system_f_call_or_t982"
| TUPLE5(STRING("system_f_call_or_t983"), DLR_signed, LPAREN, arg3, RPAREN) -> Sys("$signed", mly arg3)
| TUPLE5(STRING("system_f_call_or_t984"), DLR_sin, LPAREN, arg3, RPAREN) as oth -> mayfail oth  "system_f_call_or_t984"
| TUPLE5(STRING("system_f_call_or_t985"), DLR_sinh, LPAREN, arg3, RPAREN) as oth -> mayfail oth  "system_f_call_or_t985"
| TUPLE5(STRING("system_f_call_or_t986"), DLR_size, LPAREN, arg3, RPAREN) -> Sys("$size", mly arg3)
| TUPLE5(STRING("system_f_call_or_t988"), DLR_sqrt, LPAREN, arg3, RPAREN) as oth -> mayfail oth  "system_f_call_or_t988"
| TUPLE5(STRING("system_f_call_or_t991"), DLR_stable, LPAREN, arg3, RPAREN) as oth -> mayfail oth  "system_f_call_or_t991"
| TUPLE5(STRING("system_f_call_or_t993"), DLR_tan, LPAREN, arg3, RPAREN) as oth -> mayfail oth  "system_f_call_or_t993"
| TUPLE5(STRING("system_f_call_or_t994"), DLR_tanh, LPAREN, arg3, RPAREN) as oth -> mayfail oth  "system_f_call_or_t994"
| TUPLE5(STRING("system_f_call_or_t995"), DLR_test_DLR_plusargs, LPAREN, arg3, RPAREN) as oth -> mayfail oth  "system_f_call_or_t995"
| TUPLE5(STRING("system_f_call_or_t997"), DLR_typename, LPAREN, arg3, RPAREN) as oth -> mayfail oth  "system_f_call_or_t997"
| TUPLE5(STRING("system_f_call_or_t999"), DLR_unpacked_dimensions, LPAREN, arg3, RPAREN) as oth -> mayfail oth  "system_f_call_or_t999"
| TUPLE5(STRING("system_t_call802"), DLR_dumpfile, LPAREN, arg3, RPAREN) as oth -> mayfail oth  "system_t_call802"
| TUPLE5(STRING("system_t_call804"), DLR_dumpvars, LPAREN, arg3, RPAREN) as oth -> mayfail oth  "system_t_call804"
| TUPLE5(STRING("system_t_call807"), DLR_dumpall, LPAREN, arg3, RPAREN) as oth -> mayfail oth  "system_t_call807"
| TUPLE5(STRING("system_t_call809"), DLR_dumpflush, LPAREN, arg3, RPAREN) as oth -> mayfail oth  "system_t_call809"
| TUPLE5(STRING("system_t_call810"), DLR_dumplimit, LPAREN, arg3, RPAREN) as oth -> mayfail oth  "system_t_call810"
| TUPLE5(STRING("system_t_call813"), DLR_dumpoff, LPAREN, arg3, RPAREN) as oth -> mayfail oth  "system_t_call813"
| TUPLE5(STRING("system_t_call815"), DLR_dumpon, LPAREN, arg3, RPAREN) as oth -> mayfail oth  "system_t_call815"
| TUPLE5(STRING("system_t_call816"), DLR_c, LPAREN, arg3, RPAREN) as oth -> mayfail oth  "system_t_call816"
| TUPLE5(STRING("system_t_call817"), DLR_system, LPAREN, arg3, RPAREN) as oth -> mayfail oth  "system_t_call817"
| TUPLE5(STRING("system_t_call819"), DLR_fclose, LPAREN, arg3, RPAREN) -> SysTaskCall("$fclose", match arg3 with TLIST lst -> rml lst | oth -> mly oth :: [])
| TUPLE5(STRING("system_t_call821"), DLR_fflush, LPAREN, arg3, RPAREN) as oth -> mayfail oth  "system_t_call821"
| TUPLE5(STRING("system_t_call823"), DLR_finish, LPAREN, arg3, RPAREN) as oth -> mayfail oth  "system_t_call823"
| TUPLE5(STRING("system_t_call825"), DLR_stop, LPAREN, arg3, RPAREN) as oth -> mayfail oth  "system_t_call825"
| TUPLE5(STRING("system_t_call832"), DLR_display, LPAREN, arg3, RPAREN) ->
    SysTaskCall("$display", match arg3 with TLIST lst -> rml lst | oth -> mly oth :: [])
| TUPLE5(STRING("system_t_call834"), DLR_displayb, LPAREN, arg3, RPAREN) as oth -> mayfail oth  "system_t_call834"
| TUPLE5(STRING("system_t_call836"), DLR_displayh, LPAREN, arg3, RPAREN) as oth -> mayfail oth  "system_t_call836"
| TUPLE5(STRING("system_t_call838"), DLR_displayo, LPAREN, arg3, RPAREN) as oth -> mayfail oth  "system_t_call838"
| TUPLE5(STRING("system_t_call839"), DLR_monitor, LPAREN, arg3, RPAREN) as oth -> mayfail oth  "system_t_call839"
| TUPLE5(STRING("system_t_call840"), DLR_monitorb, LPAREN, arg3, RPAREN) as oth -> mayfail oth  "system_t_call840"
| TUPLE5(STRING("system_t_call841"), DLR_monitorh, LPAREN, arg3, RPAREN) as oth -> mayfail oth  "system_t_call841"
| TUPLE5(STRING("system_t_call842"), DLR_monitoro, LPAREN, arg3, RPAREN) as oth -> mayfail oth  "system_t_call842"
| TUPLE5(STRING("system_t_call843"), DLR_strobe, LPAREN, arg3, RPAREN) as oth -> mayfail oth  "system_t_call843"
| TUPLE5(STRING("system_t_call844"), DLR_strobeb, LPAREN, arg3, RPAREN) as oth -> mayfail oth  "system_t_call844"
| TUPLE5(STRING("system_t_call845"), DLR_strobeh, LPAREN, arg3, RPAREN) as oth -> mayfail oth  "system_t_call845"
| TUPLE5(STRING("system_t_call846"), DLR_strobeo, LPAREN, arg3, RPAREN) as oth -> mayfail oth  "system_t_call846"
| TUPLE5(STRING("system_t_call848"), DLR_write, LPAREN, arg3, RPAREN) ->
    SysTaskCall("$write", match arg3 with TLIST lst -> rml lst | oth -> mly oth :: [])
| TUPLE5(STRING("system_t_call850"), DLR_writeb, LPAREN, arg3, RPAREN) as oth -> mayfail oth  "system_t_call850"
| TUPLE5(STRING("system_t_call852"), DLR_writeh, LPAREN, arg3, RPAREN) as oth -> mayfail oth  "system_t_call852"
| TUPLE5(STRING("system_t_call854"), DLR_writeo, LPAREN, arg3, RPAREN) as oth -> mayfail oth  "system_t_call854"
| TUPLE5(STRING("system_t_call855"), DLR_fdisplay, LPAREN, arg3, RPAREN) as oth -> mayfail oth  "system_t_call855"
| TUPLE5(STRING("system_t_call857"), DLR_fdisplayb, LPAREN, arg3, RPAREN) as oth -> mayfail oth  "system_t_call857"
| TUPLE5(STRING("system_t_call859"), DLR_fdisplayh, LPAREN, arg3, RPAREN) as oth -> mayfail oth  "system_t_call859"
| TUPLE5(STRING("system_t_call861"), DLR_fdisplayo, LPAREN, arg3, RPAREN) as oth -> mayfail oth  "system_t_call861"
| TUPLE5(STRING("system_t_call876"), DLR_info, LPAREN, arg3, RPAREN) as oth -> mayfail oth  "system_t_call876"
| TUPLE5(STRING("system_t_call878"), DLR_warning, LPAREN, arg3, RPAREN) ->
    SysTaskCall("$warning", match arg3 with TLIST lst -> rml lst | oth -> mly oth :: [])
| TUPLE5(STRING("system_t_call880"), DLR_error, LPAREN, arg3, RPAREN) ->
    SysTaskCall("$error", match arg3 with TLIST lst -> rml lst | oth -> mly oth :: [])
| TUPLE5(STRING("system_t_call882"), DLR_fatal, LPAREN, arg3, RPAREN) as oth -> mayfail oth  "system_t_call882"
| TUPLE5(STRING("system_t_call888"), DLR_printtimescale, LPAREN, arg3, RPAREN) as oth -> mayfail oth  "system_t_call888"
| TUPLE5(STRING("taskRef785"), arg1, LPAREN, arg3, RPAREN) ->
(match arg1, arg3 with
	 | IDENTIFIER tid, TLIST (IDENTIFIER _ :: _ as lst) -> TaskRef(tid, rml lst)
         | IDENTIFIER tid, TLIST (TUPLE4 (STRING "argsExprListE1359", lft, COMMA, rght) :: []) ->
	     TaskRef(tid, match (mly lft, mly rght) with
		  | Itmlst lst, Itmlst lst' -> lst@lst'
		  | hd, Itmlst lst -> hd::lst
		  | Itmlst lst, oth -> lst@[oth]
		  | oth, oth' -> oth::[oth'])
         | oth -> othpat2 := Some oth; failwith "taskRef785")
| TUPLE5(STRING("tf_port_declaration233"), arg1, arg2, arg3, SEMICOLON) ->
(match arg1, arg2, arg3 with
       | (Input|Output), EMPTY_TOKEN, TLIST lst -> TF_port_decl(mly arg1, [], rml lst)
       | (Input|Output), TUPLE3 (STRING "data_typeBasic264", Integer, EMPTY_TOKEN), TLIST lst -> TF_port_decl(mly arg1, mly Integer :: [], rml lst)
       | oth -> othpat3 := Some oth; failwith "tf_port_declaration233")
| TUPLE5(STRING("tf_port_declaration235"), arg1, arg2, arg3, SEMICOLON) ->
(match arg1, arg2, arg3 with
       | (Input|Output), EMPTY_TOKEN, TLIST lst -> TF_port_decl(mly arg1, [], rml lst)
       | (Input|Output), TUPLE3 (STRING "implicit_typeE363", EMPTY_TOKEN, TLIST rng), TLIST lst -> TF_port_decl(mly arg1, rml rng, rml lst)
       | oth -> othpat3 := Some oth; failwith "tf_port_declaration235")
| TUPLE5(STRING("tf_port_itemAssignment1084"), arg1, arg2, arg3, arg4) ->
(match arg1, arg2, arg3, arg4 with
       | IDENTIFIER _, EMPTY_TOKEN, EMPTY_TOKEN, EMPTY_TOKEN -> ItemAsgn (mly arg1)
       | oth -> othpat4 := Some oth; failwith "tf_port_itemAssignment1084")
| TUPLE5(STRING("tf_variable_identifier303"), arg1, arg2, arg3, arg4) -> TF_variable(mly arg1, mly arg2, mly arg3, mly arg4)
| TUPLE5(STRING("type_assignment540"), arg1, arg2, EQUALS, arg4) as oth -> mayfail oth  "type_assignment540"
| TUPLE5(STRING("type_declaration373"), Typedef, Enum, arg3, SEMICOLON) as oth -> mayfail oth  "type_declaration373"
| TUPLE5(STRING("type_declaration374"), Typedef, Struct, arg3, SEMICOLON) as oth -> mayfail oth  "type_declaration374"
| TUPLE5(STRING("type_declaration375"), Typedef, Union, arg3, SEMICOLON) as oth -> mayfail oth  "type_declaration375"
| TUPLE5(STRING("type_declaration376"), Typedef, Class, arg3, SEMICOLON) as oth -> mayfail oth  "type_declaration376"
| TUPLE5(STRING("type_reference279"), Type, LPAREN, arg3, RPAREN) as oth -> mayfail oth  "type_reference279"
| TUPLE6(STRING("anyrange530"), LBRACK, arg2, COLON, arg4, RBRACK) -> AnyRange(mly arg2, mly arg4)
| TUPLE6(STRING("argsDotted1365"), DOT, arg2, LPAREN, arg4, RPAREN) as oth -> mayfail oth  "argsDotted1365"
| TUPLE6(STRING("bind_directive437"), Bind, arg2, COLON, arg4, arg5) as oth -> mayfail oth  "bind_directive437"
| TUPLE6(STRING("cellparamItemE579"), DOT, arg2, LPAREN, arg4, RPAREN) ->
(match arg2, arg4 with
       | IDENTIFIER lhs, expr -> CellParamItem2(lhs, mly expr)
       | TYPE_HYPHEN_IDENTIFIER lhs, expr -> CellParamItem3(lhs, mly expr)
       | oth -> othpat2 := Some oth; failwith "cellparamItemE579")
| TUPLE6(STRING("cellpinItemE586"), DOT, arg2, LPAREN, arg4, RPAREN) ->
(match arg2, arg4 with
       | IDENTIFIER lhs, expr -> CellPinItem2(lhs, mly expr)
       | oth -> othpat2 := Some oth; failwith "cellpinItemE586")
| TUPLE6(STRING("class_constraint2628"), Extern, arg2, Constraint, arg4, SEMICOLON) as oth -> mayfail oth  "class_constraint2628"
| TUPLE6(STRING("class_constraint2629"), Pure, arg2, Constraint, arg4, SEMICOLON) as oth -> mayfail oth  "class_constraint2629"
| TUPLE6(STRING("class_method2612"), Pure, Virtual, arg3, arg4, SEMICOLON) as oth -> mayfail oth  "class_method2612"
| TUPLE6(STRING("conditional_generate_construct458"), If, LPAREN, arg3, RPAREN, arg5) ->
(match arg3, arg5 with
| expr, TUPLE7 (STRING "genItemBegin450", Begin, COLON, IDENTIFIER id, TLIST lst, End, EMPTY_TOKEN) -> GenItem(id, rml lst)
| expr, TUPLE4 (STRING "genItemBegin446", Begin, TLIST lst, End) -> GenItem("", rml lst)
| expr, TUPLE6 (STRING "continuous_assign427", Assign, EMPTY_TOKEN, EMPTY_TOKEN, TLIST lst, SEMICOLON) -> GenItem("", rml lst)
| oth -> othpat2 := Some oth; failwith "conditional_generate_construct458")
| TUPLE6(STRING("constraint_block_item2634"), Solve, arg2, Before, arg4, SEMICOLON) as oth -> mayfail oth  "constraint_block_item2634"
| TUPLE6(STRING("constraint_expression2643"), If, LPAREN, arg3, RPAREN, arg5) as oth -> mayfail oth  "constraint_expression2643"
| TUPLE6(STRING("constraint_expression2645"), Foreach, LPAREN, arg3, RPAREN, arg5) as oth -> mayfail oth  "constraint_expression2645"
| TUPLE6(STRING("continuous_assign427"), Assign, arg2, arg3, arg4, SEMICOLON) ->
(match arg2, arg3, arg4 with
       | EMPTY_TOKEN, (EMPTY_TOKEN|TUPLE3(STRING "delay_control492",_,_)), TLIST lst -> ContAsgn(rml lst)
       | oth -> othpat3 := Some oth; failwith "continuous_assign427")
| TUPLE6(STRING("data_declarationVarFront354"), Const, Var, arg3, arg4, arg5) as oth -> mayfail oth  "data_declarationVarFront354"
| TUPLE6(STRING("defparam_assignment545"), arg1, DOT, arg3, EQUALS, arg5) as oth -> mayfail oth  "defparam_assignment545"
| TUPLE6(STRING("enumDecl323"), Enum, arg2, LBRACE, arg4, RBRACE) as oth -> mayfail oth  "enumDecl323"
| TUPLE6(STRING("enumNameRangeE336"), LBRACK, arg2, COLON, arg4, RBRACK) as oth -> mayfail oth  "enumNameRangeE336"
| TUPLE6(STRING("expr1148"), arg1, QUERY, arg3, COLON, arg5) -> Query(mly arg1, mly arg3, mly arg5)
| TUPLE6(STRING("expr1149"), arg1, Inside, LBRACE, arg4, RBRACE) -> InsideRange(mly arg1, mly arg4)
| TUPLE6(STRING("expr1162"), arg1, QUOTE, LPAREN, arg4, RPAREN) -> ExprQuote1(mly arg1, mly arg4)
| TUPLE6(STRING("expr1164"), Signed, QUOTE, LPAREN, arg4, RPAREN) -> ExprQuote1(mly Signed, mly arg4)
| TUPLE6(STRING("expr1165"), Unsigned, QUOTE, LPAREN, arg4, RPAREN) -> ExprQuote1(mly Unsigned, mly arg4)
| TUPLE6(STRING("expr1166"), String, QUOTE, LPAREN, arg4, RPAREN) -> ExprQuote1(mly String, mly arg4)
| TUPLE6(STRING("expr1167"), Const, QUOTE, LPAREN, arg4, RPAREN) -> ExprQuote1(mly Const, mly arg4)
| TUPLE6(STRING("expr1168"), arg1, QUOTE, LPAREN, arg4, RPAREN) -> ExprQuote1(mly arg1, mly arg4)
| TUPLE6(STRING("expr1173"), arg1, Dist, LBRACE, arg4, RBRACE) as oth -> mayfail oth  "expr1173"
| TUPLE6(STRING("exprNoStr1280"), arg1, QUERY, arg3, COLON, arg5) as oth -> mayfail oth  "exprNoStr1280"
| TUPLE6(STRING("exprNoStr1281"), arg1, Inside, LBRACE, arg4, RBRACE) as oth -> mayfail oth  "exprNoStr1281"
| TUPLE6(STRING("exprNoStr1294"), arg1, QUOTE, LPAREN, arg4, RPAREN) as oth -> mayfail oth  "exprNoStr1294"
| TUPLE6(STRING("exprNoStr1296"), Signed, QUOTE, LPAREN, arg4, RPAREN) as oth -> mayfail oth  "exprNoStr1296"
| TUPLE6(STRING("exprNoStr1297"), Unsigned, QUOTE, LPAREN, arg4, RPAREN) as oth -> mayfail oth  "exprNoStr1297"
| TUPLE6(STRING("exprNoStr1298"), String, QUOTE, LPAREN, arg4, RPAREN) as oth -> mayfail oth  "exprNoStr1298"
| TUPLE6(STRING("exprNoStr1299"), Const, QUOTE, LPAREN, arg4, RPAREN) as oth -> mayfail oth  "exprNoStr1299"
| TUPLE6(STRING("exprNoStr1300"), arg1, QUOTE, LPAREN, arg4, RPAREN) as oth -> mayfail oth  "exprNoStr1300"
| TUPLE6(STRING("exprNoStr1305"), arg1, Dist, LBRACE, arg4, RBRACE) as oth -> mayfail oth  "exprNoStr1305"
| TUPLE6(STRING("fexpr1214"), arg1, QUERY, arg3, COLON, arg5) as oth -> mayfail oth  "fexpr1214"
| TUPLE6(STRING("fexpr1215"), arg1, Inside, LBRACE, arg4, RBRACE) as oth -> mayfail oth  "fexpr1215"
| TUPLE6(STRING("fexpr1228"), arg1, QUOTE, LPAREN, arg4, RPAREN) as oth -> mayfail oth  "fexpr1228"
| TUPLE6(STRING("fexpr1230"), Signed, QUOTE, LPAREN, arg4, RPAREN) as oth -> mayfail oth  "fexpr1230"
| TUPLE6(STRING("fexpr1231"), Unsigned, QUOTE, LPAREN, arg4, RPAREN) as oth -> mayfail oth  "fexpr1231"
| TUPLE6(STRING("fexpr1232"), String, QUOTE, LPAREN, arg4, RPAREN) as oth -> mayfail oth  "fexpr1232"
| TUPLE6(STRING("fexpr1233"), Const, QUOTE, LPAREN, arg4, RPAREN) as oth -> mayfail oth  "fexpr1233"
| TUPLE6(STRING("fexpr1234"), arg1, QUOTE, LPAREN, arg4, RPAREN) as oth -> mayfail oth  "fexpr1234"
| TUPLE6(STRING("fexpr1239"), arg1, Dist, LBRACE, arg4, RBRACE) as oth -> mayfail oth  "fexpr1239"
| TUPLE6(STRING("for_initializationItem775"), Var, arg2, arg3, EQUALS, arg5) as oth -> mayfail oth  "for_initializationItem775"
| TUPLE6(STRING("funcRef788"), arg1, arg2, LPAREN, arg4, RPAREN) ->
(match arg1, arg2, arg4 with
(*
 | TLIST lst, IDENTIFIER id, (EMPTY_TOKEN | TLIST [EMPTY_TOKEN]) -> FunRef(id, rml lst)
 | TLIST lst, IDENTIFIER id, TLIST lst' -> FunRef2(id, rml lst, rml lst')
*) | oth -> othpat3 := Some oth; failwith "funcRef788")
| TUPLE6(STRING("function_prototype1026"), Function, arg2, LPAREN, arg4, RPAREN) as oth -> mayfail oth  "function_prototype1026"
| TUPLE6(STRING("function_subroutine_callNoMethod794"), arg1, With_HYPHEN_then_HYPHEN_LPAREN, LPAREN, arg4, RPAREN) as oth -> mayfail oth  "function_subroutine_callNoMethod794"
| TUPLE6(STRING("function_subroutine_callNoMethod795"), arg1, With_HYPHEN_then_HYPHEN_LPAREN, LPAREN, arg4, RPAREN) as oth -> mayfail oth  "function_subroutine_callNoMethod795"
| TUPLE6(STRING("gateAnd1436"), arg1, arg2, COMMA, arg4, RPAREN) as oth -> mayfail oth  "gateAnd1436"
| TUPLE6(STRING("gateBuf1430"), arg1, arg2, COMMA, arg4, RPAREN) as oth -> mayfail oth  "gateBuf1430"
| TUPLE6(STRING("gateNand1437"), arg1, arg2, COMMA, arg4, RPAREN) as oth -> mayfail oth  "gateNand1437"
| TUPLE6(STRING("gateNor1439"), arg1, arg2, COMMA, arg4, RPAREN) as oth -> mayfail oth  "gateNor1439"
| TUPLE6(STRING("gateNot1433"), arg1, arg2, COMMA, arg4, RPAREN) as oth -> mayfail oth  "gateNot1433"
| TUPLE6(STRING("gateOr1438"), arg1, arg2, COMMA, arg4, RPAREN) as oth -> mayfail oth  "gateOr1438"
| TUPLE6(STRING("gateXnor1441"), arg1, arg2, COMMA, arg4, RPAREN) as oth -> mayfail oth  "gateXnor1441"
| TUPLE6(STRING("gateXor1440"), arg1, arg2, COMMA, arg4, RPAREN) as oth -> mayfail oth  "gateXor1440"
| TUPLE6(STRING("genItemBegin449"), arg1, COLON_HYPHEN_begin, Begin, End, arg5) as oth -> mayfail oth  "genItemBegin449"
| TUPLE6(STRING("genItemBegin451"), Begin, COLON, arg3, End, arg5) ->
(match arg3, arg5 with
  | IDENTIFIER lbl, EMPTY_TOKEN -> GenItem(lbl, [])
  | oth -> othpat2 := Some oth; failwith "genItemBegin451")
| TUPLE6(STRING("idClassSel2487"), This, DOT, Super, DOT, arg5) as oth -> mayfail oth  "idClassSel2487"
| TUPLE6(STRING("idClassSelForeach2492"), This, DOT, Super, DOT, arg5) as oth -> mayfail oth  "idClassSelForeach2492"
| TUPLE6(STRING("instDecl552"), arg1, DOT, arg3, arg4, SEMICOLON) as oth -> mayfail oth  "instDecl552"
| TUPLE6(STRING("instRange565"), LBRACK, arg2, COLON, arg4, RBRACK) -> InstRange(mly arg2, mly arg4)
| TUPLE6(STRING("instnameParen558"), arg1, arg2, LPAREN, arg4, RPAREN) ->
(match arg1, arg2, arg4 with
       | IDENTIFIER id, EMPTY_TOKEN, TLIST lst -> InstNameParen1(id, rml lst)
       | IDENTIFIER id, TLIST lst, TLIST [TLIST [EMPTY_TOKEN]] -> InstNameParen2(id, rml lst)
| oth -> othpat3 := Some oth; failwith "instnameParen558")
| TUPLE6(STRING("interface_declaration107"), Extern, arg2, arg3, arg4, SEMICOLON) as oth -> mayfail oth  "interface_declaration107"
| TUPLE6(STRING("minTypMax502"), arg1, COLON, arg3, COLON, arg5) as oth -> mayfail oth  "minTypMax502"
| TUPLE6(STRING("module_declaration53"), Extern, arg2, arg3, arg4, SEMICOLON) as oth -> mayfail oth  "module_declaration53"
| TUPLE6(STRING("net_declarationFront187"), arg1, arg2, arg3, arg4, arg5) as oth -> mayfail oth  "net_declarationFront187"
| TUPLE6(STRING("package_export_declaration46"), Export, STAR, COLON_COLON, STAR, SEMICOLON) as oth -> mayfail oth  "package_export_declaration46"
| TUPLE6(STRING("port83"), arg1, arg2, arg3, arg4, arg5) as oth -> mayfail oth  "port83"
| TUPLE6(STRING("port85"), arg1, Interface, arg3, arg4, arg5) as oth -> mayfail oth  "port85"
| TUPLE6(STRING("port87"), dir, typ, port, arg4, arg5) ->
    (match typ, port with
      | TUPLE4 (STRING "data_typeBasic263", (Reg|Logic as x), EMPTY_TOKEN, EMPTY_TOKEN), IDENTIFIER port ->
	   Port(PortDir(mly dir, mly x), port, [], Deflt)
      | TUPLE4 (STRING "data_typeBasic263", (Reg|Logic), (EMPTY_TOKEN|Signed), TLIST lst), IDENTIFIER port -> Port(mly dir, port, rml lst, Deflt)
      | TUPLE4 (STRING "data_type261", EMPTY_TOKEN, TYPE_HYPHEN_IDENTIFIER typ, EMPTY_TOKEN), IDENTIFIER port -> Port(mly dir, port, Typ2(typ, [], []) :: [], Deflt)
      | TUPLE4 (STRING "data_type261", TLIST lst, TYPE_HYPHEN_IDENTIFIER typ, EMPTY_TOKEN), IDENTIFIER port -> Port(mly dir, port, Typ2(typ, rml lst, []) :: [], Deflt)
      | TUPLE4 (STRING "data_type261", EMPTY_TOKEN, TYPE_HYPHEN_IDENTIFIER typ, TLIST lst), IDENTIFIER port -> Port(mly dir, port, Typ2(typ, [], []) :: rml lst, Deflt)
      | TUPLE4 (STRING "data_type261", TLIST lst, TYPE_HYPHEN_IDENTIFIER typ, TLIST lst'), IDENTIFIER port -> Port(mly dir, port, Typ2(typ, rml lst, []) :: rml lst', Deflt)
      | oth -> othpat2 := Some oth; failwith "port87")
| TUPLE6(STRING("port90"), arg1, arg2, arg3, arg4, arg5) as oth -> mayfail oth  "port90"
| TUPLE6(STRING("port_declaration223"), arg1, arg2, Var, arg4, arg5) as oth -> mayfail oth  "port_declaration223"
| TUPLE6(STRING("port_declaration225"), arg1, arg2, Var, arg4, arg5) as oth -> mayfail oth  "port_declaration225"
| TUPLE6(STRING("port_declaration227"), dir, arg2, arg3, arg4, nam) ->
  let rng = match mly arg4 with Itmlst lst -> lst | oth -> [oth] in
  let portlst = match mly nam with Itmlst(Id _ :: _ as lst) -> lst | oth -> othport := Some oth; failwith "port" in
  Itmlst (List.map (function Id port -> Port(mly dir, port, rng, mly arg3) | oth -> othport := Some oth; failwith "portlst") portlst)
| TUPLE6(STRING("program_declaration134"), Extern, arg2, arg3, arg4, SEMICOLON) as oth -> mayfail oth  "program_declaration134"
| TUPLE6(STRING("property_spec2555"), AT, LPAREN, arg3, RPAREN, arg5) -> PropertySpec (* (mly arg3, mly arg5) *)
| TUPLE6(STRING("simple_immediate_assertion_statement2530"), Assert, LPAREN, arg3, RPAREN, arg5) -> Assert (* (mly arg3, mly arg5) *)
| TUPLE6(STRING("simple_immediate_assertion_statement2533"), Assume, LPAREN, arg3, RPAREN, arg5) as oth -> mayfail oth  "simple_immediate_assertion_statement2533"
| TUPLE6(STRING("simple_immediate_assertion_statement2536"), Cover, LPAREN, arg3, RPAREN, arg5) as oth -> mayfail oth  "simple_immediate_assertion_statement2536"
| TUPLE6(STRING("statement_item648"), arg1, LT_EQ, arg3, arg4, SEMICOLON) ->
( match arg1, arg3, arg4 with
	| IDENTIFIER lhs, EMPTY_TOKEN, expr -> Equate(Id lhs, mly expr)
	| TUPLE4 (STRING "fexprScope1336", IDENTIFIER lhs, DOT, IDENTIFIER field), EMPTY_TOKEN, expr -> EquateField(Id lhs, Id field, mly expr)
	| TUPLE4 (STRING "fexprScope1336", TUPLE5 (STRING "idArrayed2503", TUPLE5 (STRING "idArrayed2503", IDENTIFIER id, LBRACK, ix, RBRACK),
            LBRACK, ix', RBRACK), DOT, IDENTIFIER field), EMPTY_TOKEN, expr -> EquateArrayField(Id id, Id field, mly ix, mly ix', mly expr)
	| TUPLE7 (STRING "idArrayed2504", lhs, LBRACK, hi, COLON, lo, RBRACK), EMPTY_TOKEN, expr -> EquateSlice(mly lhs, mly hi, mly lo, mly expr)
	| TUPLE5 (STRING "idArrayed2503", IDENTIFIER lhs, LBRACK, ix, RBRACK), EMPTY_TOKEN, expr -> EquateSelect(Id lhs, mly ix, mly expr)
	| TUPLE5 (STRING "idArrayed2503", lhs, LBRACK, ix, RBRACK), EMPTY_TOKEN, expr -> EquateSelect2(mly lhs, mly ix, mly expr)
	| TUPLE4 (STRING "fexprOkLvalue1316", LBRACE, TLIST lst, RBRACE), EMPTY_TOKEN, expr -> EquateConcat(rml lst, mly expr)
	| TUPLE7 (STRING "idArrayed2505", lhs, LBRACK, hi, PLUS_COLON, lo, RBRACK), EMPTY_TOKEN, expr -> EquateSlicePlus(mly lhs, mly hi, mly lo, mly expr)
	| oth -> othpat3 := Some oth; failwith "statement_item648")
| TUPLE6(STRING("statement_item651"), Force, arg2, EQUALS, arg4, SEMICOLON) as oth -> mayfail oth  "statement_item651"
| TUPLE6(STRING("statement_item653"), arg1, arg2, arg3, arg4, Endcase) ->
( match arg1, arg2, arg3, arg4 with
	| EMPTY_TOKEN, TUPLE5(STRING "caseStart721", _, _, _, _), EMPTY_TOKEN, TLIST lst -> CaseStart(mly arg2, rml (attach_lbl lst))
	| Unique, TUPLE5(STRING "caseStart721", _, _, _, _), EMPTY_TOKEN, TLIST lst -> CaseStartUniq(mly arg2, rml (attach_lbl lst))
	| oth -> othpat4 := Some oth; failwith "statement_item653")
| TUPLE6(STRING("statement_item669"), Repeat, LPAREN, arg3, RPAREN, arg5) as oth -> mayfail oth  "statement_item669"
| TUPLE6(STRING("statement_item670"), While, LPAREN, arg3, RPAREN, arg5) -> While (mly arg3, match arg5 with
    | TUPLE5 (STRING "seq_block615", Begin, TLIST lst, End, EMPTY_TOKEN) -> rml lst
    | oth -> othpat1 := Some oth; failwith "statement_item670")
| TUPLE6(STRING("statement_item673"), Foreach, LPAREN, arg3, RPAREN, arg5) ->
(match arg3, arg5 with
   |		    TUPLE5 (STRING "idArrayedForeach2508", IDENTIFIER "in", LBRACK, IDENTIFIER k, RBRACK),
		    TUPLE5 (STRING "seq_block615", Begin, TLIST lst, End, EMPTY_TOKEN) -> ForEach(Id k, rml lst)
   | oth -> othpat2 := Some oth; failwith "statement_item673")
| TUPLE6(STRING("statement_item682"), Wait, LPAREN, arg3, RPAREN, arg5) as oth -> mayfail oth  "statement_item682"
| TUPLE6(STRING("streaming_concatenation1368"), LBRACE, LT_LT, arg3, arg4, RBRACE) as oth -> mayfail oth  "streaming_concatenation1368"
| TUPLE6(STRING("streaming_concatenation1369"), LBRACE, GT_GT, arg3, arg4, RBRACE) as oth -> mayfail oth  "streaming_concatenation1369"
| TUPLE6(STRING("strengthSpec1462"), LPAREN_HYPHEN_for_HYPHEN_strength, arg2, COMMA, arg4, RPAREN) as oth -> mayfail oth  "strengthSpec1462"
| TUPLE6(STRING("struct_unionDecl281"), Struct, arg2, LBRACE, arg4, RBRACE) ->
  (match arg2,arg4 with
	| TUPLE3 (STRING "packedSigningE322", Packed, EMPTY_TOKEN), TLIST lst -> SUDecl(mly Packed, rml lst)
        | EMPTY_TOKEN, TLIST lst -> Itmlst (rml lst)
        | oth -> othpat2 := Some oth; failwith "struct_unionDecl281")
| TUPLE6(STRING("system_t_call801"), DLR_dumpports, LPAREN, COMMA, arg4, RPAREN) as oth -> mayfail oth  "system_t_call801"
| TUPLE6(STRING("taskRef786"), arg1, arg2, LPAREN, arg4, RPAREN) as oth -> mayfail oth  "taskRef786"
| TUPLE6(STRING("task_prototype1022"), Task, arg2, LPAREN, arg4, RPAREN) as oth -> mayfail oth  "task_prototype1022"
| TUPLE6(STRING("task_subroutine_callNoMethod790"), arg1, With_HYPHEN_then_HYPHEN_LPAREN, LPAREN, arg4, RPAREN) as oth -> mayfail oth  "task_subroutine_callNoMethod790"
| TUPLE6(STRING("task_subroutine_callNoMethod791"), arg1, With_HYPHEN_then_HYPHEN_LPAREN, LPAREN, arg4, RPAREN) as oth -> mayfail oth  "task_subroutine_callNoMethod791"
| TUPLE6(STRING("tfGuts1052"), LPAREN, arg2, RPAREN, SEMICOLON, arg5) ->
  (match arg2,arg5 with
	| TLIST lst, EMPTY_TOKEN -> FunGuts(rml lst, [])
	| TLIST lst, TLIST lst' -> FunGuts(rml lst, rml lst')
	| TLIST lst, TUPLE3 (STRING "tfBodyE1056", _, _) -> FunGuts(rml lst, mly arg5 :: [])
        | oth -> othpat2 := Some oth; failwith "tfGuts1052")
| TUPLE6(STRING("tf_port_declaration237"), arg1, Var, arg3, arg4, SEMICOLON) as oth -> mayfail oth  "tf_port_declaration237"
| TUPLE6(STRING("tf_port_declaration239"), arg1, Var, arg3, arg4, SEMICOLON) as oth -> mayfail oth  "tf_port_declaration239"
| TUPLE6(STRING("timeunits_declaration18"), Timeunit, TIME_NUMBER, SLASH, TIME_NUMBER, SEMICOLON) as oth -> mayfail oth  "timeunits_declaration18"
| TUPLE6(STRING("type_declaration377"), Typedef, Interface, Class, arg4, SEMICOLON) as oth -> mayfail oth  "type_declaration377"
| TUPLE6(STRING("value_range747"), LBRACK, arg2, COLON, arg4, RBRACK) -> ValueRange(mly arg2, mly arg4)
| TUPLE6(STRING("variable_decl_assignment298"), arg1, arg2, arg3, EQUALS, arg5) ->
  (match arg1,arg2,arg3,arg5 with
	| IDENTIFIER id, EMPTY_TOKEN, EMPTY_TOKEN, expr -> VarDeclAsgn(Id id, mly expr)
        | oth -> othpat4 := Some oth; failwith "variable_decl_assignment298")
| TUPLE6(STRING("vltItem2662"), arg1, HYPHEN_HYPHEN_file, STRING arg3, HYPHEN_HYPHEN_lines, INTEGER_NUMBER arg5) as oth -> mayfail oth  "vltItem2662"
| TUPLE6(STRING("vltItem2664"), arg1, HYPHEN_HYPHEN_file, STRING arg3, HYPHEN_HYPHEN_match, STRING arg5) as oth -> mayfail oth  "vltItem2664"
| TUPLE6(STRING("vltItem2667"), arg1, HYPHEN_HYPHEN_file, STRING arg3, HYPHEN_HYPHEN_lines, INTEGER_NUMBER arg5) as oth -> mayfail oth  "vltItem2667"
| TUPLE6(STRING("vltItem2669"), arg1, arg2, arg3, arg4, arg5) as oth -> mayfail oth  "vltItem2669"
| TUPLE6(STRING("vltItem2672"), Coverage_block_off, HYPHEN_HYPHEN_file, STRING arg3, HYPHEN_HYPHEN_lines, INTEGER_NUMBER arg5) as oth -> mayfail oth  "vltItem2672"
| TUPLE6(STRING("vltItem2673"), Coverage_block_off, HYPHEN_HYPHEN_module, STRING arg3, HYPHEN_HYPHEN_block, STRING arg5) as oth -> mayfail oth  "vltItem2673"
| TUPLE6(STRING("vltItem2675"), Full_case, HYPHEN_HYPHEN_file, STRING arg3, HYPHEN_HYPHEN_lines, INTEGER_NUMBER arg5) as oth -> mayfail oth  "vltItem2675"
| TUPLE6(STRING("vltItem2678"), Parallel_case, HYPHEN_HYPHEN_file, STRING arg3, HYPHEN_HYPHEN_lines, INTEGER_NUMBER arg5) as oth -> mayfail oth  "vltItem2678"
| TUPLE7(STRING("array_methodWith1092"), arg1, arg2, With_HYPHEN_then_HYPHEN_LPAREN, LPAREN, arg5, RPAREN) as oth -> mayfail oth  "array_methodWith1092"
| TUPLE7(STRING("class_constructor_prototype1028"), Function, arg2, LPAREN, arg4, RPAREN, SEMICOLON) as oth -> mayfail oth  "class_constructor_prototype1028"
| TUPLE7(STRING("concurrent_assertion_statement2548"), Assert, Property, LPAREN, arg4, RPAREN, arg6) -> AssertProperty (* (mly arg4, mly arg6) *)
| TUPLE7(STRING("concurrent_assertion_statement2549"), Assume, Property, LPAREN, arg4, RPAREN, arg6) as oth -> mayfail oth  "concurrent_assertion_statement2549"
| TUPLE7(STRING("concurrent_assertion_statement2550"), Cover, Property, LPAREN, arg4, RPAREN, arg6) as oth -> mayfail oth  "concurrent_assertion_statement2550"
| TUPLE7(STRING("concurrent_assertion_statement2551"), Restrict, Property, LPAREN, arg4, RPAREN, SEMICOLON) as oth -> mayfail oth  "concurrent_assertion_statement2551"
| TUPLE7(STRING("conditional_generate_construct457"), Case, LPAREN, arg3, RPAREN, arg5, Endcase) as oth -> mayfail oth  "conditional_generate_construct457"
| TUPLE7(STRING("deferred_immediate_assertion_statement2539"), Assert, arg2, LPAREN, arg4, RPAREN, arg6) as oth -> mayfail oth  "deferred_immediate_assertion_statement2539"
| TUPLE7(STRING("deferred_immediate_assertion_statement2542"), Assume, arg2, LPAREN, arg4, RPAREN, arg6) as oth -> mayfail oth  "deferred_immediate_assertion_statement2542"
| TUPLE7(STRING("deferred_immediate_assertion_statement2545"), Cover, arg2, LPAREN, arg4, RPAREN, arg6) as oth -> mayfail oth  "deferred_immediate_assertion_statement2545"
| TUPLE7(STRING("delay_control494"), HASH, LPAREN, arg3, COMMA, arg5, RPAREN) as oth -> mayfail oth  "delay_control494"
| TUPLE7(STRING("dpi_import_export1094"), Import, STRING arg2, arg3, arg4, arg5, SEMICOLON) as oth -> mayfail oth  "dpi_import_export1094"
| TUPLE7(STRING("dpi_import_export1095"), Import, STRING arg2, arg3, arg4, arg5, SEMICOLON) as oth -> mayfail oth  "dpi_import_export1095"
| TUPLE7(STRING("dpi_import_export1096"), Export, STRING arg2, arg3, Function, arg5, SEMICOLON) as oth -> mayfail oth  "dpi_import_export1096"
| TUPLE7(STRING("dpi_import_export1097"), Export, STRING arg2, arg3, Task, arg5, SEMICOLON) as oth -> mayfail oth  "dpi_import_export1097"
| TUPLE7(STRING("elaboration_system_task_guts1015"), DLR_fatal, LPAREN, arg3, COMMA, arg5, RPAREN) -> 
    SysTaskCall("$fatal", mly arg3 :: match arg5 with TLIST lst -> rml lst | oth -> mly oth :: [])
| TUPLE7(STRING("expr1155"), LBRACE, arg2, LBRACE, arg4, RBRACE, RBRACE) ->
  (match arg4 with TLIST lst -> Repl(mly arg2, rml lst) | oth -> othpat1 := Some oth; failwith "expr1155")
| TUPLE7(STRING("exprNoStr1287"), LBRACE, arg2, LBRACE, arg4, RBRACE, RBRACE) as oth -> mayfail oth  "exprNoStr1287"
| TUPLE7(STRING("exprOkLvalue1308"), LBRACE, arg2, RBRACE, LBRACK, arg5, RBRACK) as oth -> mayfail oth  "exprOkLvalue1308"
| TUPLE7(STRING("fexpr1221"), LBRACE, arg2, LBRACE, arg4, RBRACE, RBRACE) as oth -> mayfail oth  "fexpr1221"
| TUPLE7(STRING("fexprOkLvalue1317"), LBRACE, arg2, RBRACE, LBRACK, arg5, RBRACK) as oth -> mayfail oth  "fexprOkLvalue1317"
| TUPLE7(STRING("foperator_assignment691"), arg1, EQUALS, DLR_fopen, LPAREN, arg5, RPAREN) as oth -> mayfail oth  "foperator_assignment691"
| TUPLE7(STRING("genItemBegin448"), arg1, COLON_HYPHEN_begin, Begin, arg4, End, arg6) as oth -> mayfail oth  "genItemBegin448"
| TUPLE7(STRING("genItemBegin450"), Begin, COLON, arg3, arg4, End, arg6) ->
(match arg3,arg4,arg6 with
| IDENTIFIER id, TLIST lst, (EMPTY_TOKEN|TUPLE3 (STRING "endLabelE2519", COLON, IDENTIFIER _)) -> GenItem(id, rml lst)
| oth -> othpat3 := Some oth; failwith "genItemBegin450")
| TUPLE7(STRING("idArrayed2504"), arg1, LBRACK, arg3, COLON, arg5, RBRACK) -> IdArrayedColon(mly arg1, mly arg3, mly arg5)
| TUPLE7(STRING("idArrayed2505"), arg1, LBRACK, arg3, PLUS_COLON, arg5, RBRACK) -> IdArrayedPlusColon(mly arg1, mly arg3, mly arg5)
| TUPLE7(STRING("idArrayed2506"), arg1, LBRACK, arg3, HYPHEN_COLON, arg5, RBRACK) -> IdArrayedHyphenColon(mly arg1, mly arg3, mly arg5)
| TUPLE7(STRING("idArrayedForeach2509"), arg1, LBRACK, arg3, COLON, arg5, RBRACK) as oth -> mayfail oth  "idArrayedForeach2509"
| TUPLE7(STRING("idArrayedForeach2510"), arg1, LBRACK, arg3, PLUS_COLON, arg5, RBRACK) as oth -> mayfail oth  "idArrayedForeach2510"
| TUPLE7(STRING("idArrayedForeach2511"), arg1, LBRACK, arg3, HYPHEN_COLON, arg5, RBRACK) as oth -> mayfail oth  "idArrayedForeach2511"
| TUPLE7(STRING("idArrayedForeach2512"), arg1, LBRACK, arg3, COMMA, arg5, RBRACK) as oth -> mayfail oth  "idArrayedForeach2512"
| TUPLE7(STRING("port88"), arg1, Var, arg3, arg4, arg5, arg6) as oth -> mayfail oth  "port88"
| TUPLE7(STRING("port89"), arg1, Var, arg3, arg4, arg5, arg6) as oth -> mayfail oth  "port89"
| TUPLE7(STRING("port91"), dir, arg2, typ, port, arg5, arg6) ->
    (match typ, port with
      | TLIST lst, IDENTIFIER port -> Port(mly dir, port, rml lst, Deflt)
      | oth -> othpat2 := Some oth; failwith "port91")
| TUPLE7(STRING("port96"), arg1, arg2, arg3, arg4, EQUALS, arg6) as oth -> mayfail oth  "port96"
| TUPLE7(STRING("property_spec2556"), Disable, Iff, LPAREN, arg4, RPAREN, arg6) as oth -> mayfail oth  "property_spec2556"
| TUPLE7(STRING("simple_immediate_assertion_statement2531"), Assert, LPAREN, arg3, RPAREN, Else, arg6) -> Assert (* (mly arg3, mly arg6) *)
| TUPLE7(STRING("simple_immediate_assertion_statement2534"), Assume, LPAREN, arg3, RPAREN, Else, arg6) -> Assert (* (mly arg3, mly arg6) *)
| TUPLE7(STRING("statement_item649"), Assign, arg2, EQUALS, arg4, arg5, SEMICOLON) as oth -> mayfail oth  "statement_item649"
| TUPLE7(STRING("statement_item654"), arg1, arg2, arg3, Inside, arg5, Endcase) ->
(match arg1, arg2, arg3, arg5 with
       | Unique, TUPLE5 (STRING "caseStart721", Case, LPAREN, expr, RPAREN), EMPTY_TOKEN, TLIST lst ->
           CaseStartUniqInside(mly expr, rml (attach_lbl lst))
       | EMPTY_TOKEN, TUPLE5 (STRING "caseStart721", Case, LPAREN, expr, RPAREN), EMPTY_TOKEN, TLIST lst ->
           CaseStartInside(mly expr, rml (attach_lbl lst))
       | oth -> othpat4 := Some oth; failwith "statement_item654")
| TUPLE7(STRING("statement_item655"), arg1, If, LPAREN, arg4, RPAREN, arg6) ->
(match arg1, arg4, arg6 with
       | EMPTY_TOKEN, expr, stmt when pred1 stmt -> If1(mly expr, mly stmt)
       | oth -> othpat3 := Some oth; failwith "statement_item655")
| TUPLE7(STRING("statement_item658"), Void, QUOTE, LPAREN, arg4, RPAREN, SEMICOLON) as oth -> mayfail oth  "statement_item658"
| TUPLE7(STRING("struct_unionDecl283"), Union, EMPTY_TOKEN, packsign, LBRACE, TLIST lst, RBRACE) -> Union(mly packsign, rml lst)
| TUPLE7(STRING("struct_unionDecl283"), Union, arg2, arg3, LBRACE, arg5, RBRACE) as oth -> mayfail oth  "struct_unionDecl283"
| TUPLE7(STRING("system_f_call906"), DLR_cast, LPAREN, arg3, COMMA, arg5, RPAREN) as oth -> mayfail oth  "system_f_call906"
| TUPLE7(STRING("system_f_call_or_t1004"), DLR_urandom_range, LPAREN, arg3, COMMA, arg5, RPAREN) as oth -> mayfail oth  "system_f_call_or_t1004"
| TUPLE7(STRING("system_f_call_or_t1005"), DLR_value_DLR_plusargs, LPAREN, arg3, COMMA, arg5, RPAREN) as oth -> mayfail oth  "system_f_call_or_t1005"
| TUPLE7(STRING("system_f_call_or_t916"), DLR_atan2, LPAREN, arg3, COMMA, arg5, RPAREN) as oth -> mayfail oth  "system_f_call_or_t916"
| TUPLE7(STRING("system_f_call_or_t919"), DLR_bits, LPAREN, arg3, COMMA, arg5, RPAREN) as oth -> mayfail oth  "system_f_call_or_t919"
| TUPLE7(STRING("system_f_call_or_t924"), DLR_changed, LPAREN, arg3, COMMA, arg5, RPAREN) as oth -> mayfail oth  "system_f_call_or_t924"
| TUPLE7(STRING("system_f_call_or_t928"), DLR_countbits, LPAREN, arg3, COMMA, arg5, RPAREN) as oth -> mayfail oth  "system_f_call_or_t928"
| TUPLE7(STRING("system_f_call_or_t936"), DLR_fell, LPAREN, arg3, COMMA, arg5, RPAREN) as oth -> mayfail oth  "system_f_call_or_t936"
| TUPLE7(STRING("system_f_call_or_t938"), DLR_ferror, LPAREN, arg3, COMMA, arg5, RPAREN) as oth -> mayfail oth  "system_f_call_or_t938"
| TUPLE7(STRING("system_f_call_or_t940"), DLR_fgets, LPAREN, arg3, COMMA, arg5, RPAREN) as oth -> mayfail oth  "system_f_call_or_t940"
| TUPLE7(STRING("system_f_call_or_t941"), DLR_fread, LPAREN, arg3, COMMA, arg5, RPAREN) as oth -> mayfail oth  "system_f_call_or_t941"
| TUPLE7(STRING("system_f_call_or_t950"), DLR_high, LPAREN, arg3, COMMA, arg5, RPAREN) as oth -> mayfail oth  "system_f_call_or_t950"
| TUPLE7(STRING("system_f_call_or_t951"), DLR_hypot, LPAREN, arg3, COMMA, arg5, RPAREN) as oth -> mayfail oth  "system_f_call_or_t951"
| TUPLE7(STRING("system_f_call_or_t953"), DLR_increment, LPAREN, arg3, COMMA, arg5, RPAREN) as oth -> mayfail oth  "system_f_call_or_t953"
| TUPLE7(STRING("system_f_call_or_t958"), DLR_left, LPAREN, arg3, COMMA, arg5, RPAREN) as oth -> mayfail oth  "system_f_call_or_t958"
| TUPLE7(STRING("system_f_call_or_t962"), DLR_low, LPAREN, arg3, COMMA, arg5, RPAREN) -> Sys("$low", mly arg3)
| TUPLE7(STRING("system_f_call_or_t966"), DLR_past, LPAREN, arg3, COMMA, arg5, RPAREN) as oth -> mayfail oth  "system_f_call_or_t966"
| TUPLE7(STRING("system_f_call_or_t969"), DLR_pow, LPAREN, arg3, COMMA, arg5, RPAREN) as oth -> mayfail oth  "system_f_call_or_t969"
| TUPLE7(STRING("system_f_call_or_t976"), DLR_right, LPAREN, arg3, COMMA, arg5, RPAREN) as oth -> mayfail oth  "system_f_call_or_t976"
| TUPLE7(STRING("system_f_call_or_t978"), DLR_rose, LPAREN, arg3, COMMA, arg5, RPAREN) as oth -> mayfail oth  "system_f_call_or_t978"
| TUPLE7(STRING("system_f_call_or_t987"), DLR_size, LPAREN, arg3, COMMA, arg5, RPAREN) as oth -> mayfail oth  "system_f_call_or_t987"
| TUPLE7(STRING("system_f_call_or_t992"), DLR_stable, LPAREN, arg3, COMMA, arg5, RPAREN) as oth -> mayfail oth  "system_f_call_or_t992"
| TUPLE7(STRING("system_f_call_or_t998"), DLR_ungetc, LPAREN, arg3, COMMA, arg5, RPAREN) as oth -> mayfail oth  "system_f_call_or_t998"
| TUPLE7(STRING("system_t_call800"), DLR_dumpports, LPAREN, arg3, COMMA, arg5, RPAREN) as oth -> mayfail oth  "system_t_call800"
| TUPLE7(STRING("system_t_call805"), DLR_dumpvars, LPAREN, arg3, COMMA, arg5, RPAREN) as oth -> mayfail oth  "system_t_call805"
| TUPLE7(STRING("system_t_call811"), DLR_dumplimit, LPAREN, arg3, COMMA, arg5, RPAREN) as oth -> mayfail oth  "system_t_call811"
| TUPLE7(STRING("system_t_call826"), DLR_sformat, LPAREN, arg3, COMMA, arg5, RPAREN) as oth -> mayfail oth  "system_t_call826"
| TUPLE7(STRING("system_t_call827"), DLR_swrite, LPAREN, arg3, COMMA, arg5, RPAREN) as oth -> mayfail oth  "system_t_call827"
| TUPLE7(STRING("system_t_call828"), DLR_swriteb, LPAREN, arg3, COMMA, arg5, RPAREN) as oth -> mayfail oth  "system_t_call828"
| TUPLE7(STRING("system_t_call829"), DLR_swriteh, LPAREN, arg3, COMMA, arg5, RPAREN) as oth -> mayfail oth  "system_t_call829"
| TUPLE7(STRING("system_t_call830"), DLR_swriteo, LPAREN, arg3, COMMA, arg5, RPAREN) as oth -> mayfail oth  "system_t_call830"
| TUPLE7(STRING("system_t_call856"), DLR_fdisplay, LPAREN, arg3, COMMA, arg5, RPAREN) as oth -> mayfail oth  "system_t_call856"
| TUPLE7(STRING("system_t_call858"), DLR_fdisplayb, LPAREN, arg3, COMMA, arg5, RPAREN) as oth -> mayfail oth  "system_t_call858"
| TUPLE7(STRING("system_t_call860"), DLR_fdisplayh, LPAREN, arg3, COMMA, arg5, RPAREN) as oth -> mayfail oth  "system_t_call860"
| TUPLE7(STRING("system_t_call862"), DLR_fdisplayo, LPAREN, arg3, COMMA, arg5, RPAREN) as oth -> mayfail oth  "system_t_call862"
| TUPLE7(STRING("system_t_call863"), DLR_fmonitor, LPAREN, arg3, COMMA, arg5, RPAREN) as oth -> mayfail oth  "system_t_call863"
| TUPLE7(STRING("system_t_call864"), DLR_fmonitorb, LPAREN, arg3, COMMA, arg5, RPAREN) as oth -> mayfail oth  "system_t_call864"
| TUPLE7(STRING("system_t_call865"), DLR_fmonitorh, LPAREN, arg3, COMMA, arg5, RPAREN) as oth -> mayfail oth  "system_t_call865"
| TUPLE7(STRING("system_t_call866"), DLR_fmonitoro, LPAREN, arg3, COMMA, arg5, RPAREN) as oth -> mayfail oth  "system_t_call866"
| TUPLE7(STRING("system_t_call867"), DLR_fstrobe, LPAREN, arg3, COMMA, arg5, RPAREN) as oth -> mayfail oth  "system_t_call867"
| TUPLE7(STRING("system_t_call868"), DLR_fstrobeb, LPAREN, arg3, COMMA, arg5, RPAREN) as oth -> mayfail oth  "system_t_call868"
| TUPLE7(STRING("system_t_call869"), DLR_fstrobeh, LPAREN, arg3, COMMA, arg5, RPAREN) as oth -> mayfail oth  "system_t_call869"
| TUPLE7(STRING("system_t_call870"), DLR_fstrobeo, LPAREN, arg3, COMMA, arg5, RPAREN) as oth -> mayfail oth  "system_t_call870"
| TUPLE7(STRING("system_t_call871"), DLR_fwrite, LPAREN, arg3, COMMA, arg5, RPAREN) -> SysTaskCall("$fwrite", match arg3 with TLIST lst -> rml lst | oth -> mly oth :: [])
| TUPLE7(STRING("system_t_call872"), DLR_fwriteb, LPAREN, arg3, COMMA, arg5, RPAREN) as oth -> mayfail oth  "system_t_call872"
| TUPLE7(STRING("system_t_call873"), DLR_fwriteh, LPAREN, arg3, COMMA, arg5, RPAREN) as oth -> mayfail oth  "system_t_call873"
| TUPLE7(STRING("system_t_call874"), DLR_fwriteo, LPAREN, arg3, COMMA, arg5, RPAREN) as oth -> mayfail oth  "system_t_call874"
| TUPLE7(STRING("system_t_call883"), DLR_fatal, LPAREN, arg3, COMMA, arg5, RPAREN) ->
    SysTaskCall("$fatal", mly arg3 :: match arg5 with TLIST lst -> rml lst | oth -> mly oth :: [])
| TUPLE7(STRING("system_t_call890"), DLR_readmemb, LPAREN, arg3, COMMA, arg5, RPAREN) as oth -> mayfail oth  "system_t_call890"
| TUPLE7(STRING("system_t_call893"), DLR_readmemh, LPAREN, arg3, COMMA, arg5, RPAREN) as oth -> mayfail oth  "system_t_call893"
| TUPLE7(STRING("system_t_call896"), DLR_writememb, LPAREN, arg3, COMMA, arg5, RPAREN) as oth -> mayfail oth  "system_t_call896"
| TUPLE7(STRING("system_t_call899"), DLR_writememh, LPAREN, arg3, COMMA, arg5, RPAREN) as oth -> mayfail oth  "system_t_call899"
| TUPLE7(STRING("system_t_call902"), DLR_cast, LPAREN, arg3, COMMA, arg5, RPAREN) as oth -> mayfail oth  "system_t_call902"
| TUPLE7(STRING("task_declaration1021"), Task, arg2, IDENTIFIER arg3, arg4, Endtask, arg6) -> TaskDecl(arg3, mly arg2, mly arg4, mly arg6)
| TUPLE7(STRING("type_declaration365"), Typedef, arg2, arg3, arg4, arg5, SEMICOLON) ->
(match  arg2, arg3, arg4, arg5 with
	| TUPLE4 (STRING "data_typeBasic263", _, _, TLIST lst), (IDENTIFIER id_t|TYPE_HYPHEN_IDENTIFIER id_t), EMPTY_TOKEN, EMPTY_TOKEN -> Typ7(id_t, mly arg2)
	| TUPLE6 (STRING "enumDecl323", Enum, typ, LBRACE, TLIST lst, RBRACE), (IDENTIFIER typ_e|TYPE_HYPHEN_IDENTIFIER typ_e), EMPTY_TOKEN, EMPTY_TOKEN -> TypEnum6(typ_e, mly typ, rml lst)
	| TUPLE3 (STRING "data_typeNoRef267", _, _), (IDENTIFIER id_t|TYPE_HYPHEN_IDENTIFIER id_t), EMPTY_TOKEN, EMPTY_TOKEN -> Typ7(id_t, mly arg2)
        | oth -> othpat4 := Some oth; failwith "type_declaration365")
| TUPLE7(STRING("type_declaration370"), Typedef, arg2, DOT, arg4, arg5, SEMICOLON) as oth -> mayfail oth  "type_declaration370"
| TUPLE8(STRING("conditional_generate_construct459"), If, LPAREN, arg3, RPAREN, arg5, Else, arg7) ->
CondGen1(mly arg3, mly arg5, mly arg7)
| TUPLE8(STRING("constraint_expression2644"), If, LPAREN, arg3, RPAREN, arg5, Else, arg7) as oth -> mayfail oth  "constraint_expression2644"
| TUPLE8(STRING("deferred_immediate_assertion_statement2540"), Assert, arg2, LPAREN, arg4, RPAREN, Else, arg7) as oth -> mayfail oth  "deferred_immediate_assertion_statement2540"
| TUPLE8(STRING("deferred_immediate_assertion_statement2543"), Assume, arg2, LPAREN, arg4, RPAREN, Else, arg7) as oth -> mayfail oth  "deferred_immediate_assertion_statement2543"
| TUPLE8(STRING("dynamic_array_new716"), New, LBRACK, arg3, RBRACK, LPAREN, arg6, RPAREN) as oth -> mayfail oth  "dynamic_array_new716"
| TUPLE8(STRING("expr1160"), LPAREN, arg2, COLON, arg4, COLON, arg6, RPAREN) as oth -> mayfail oth  "expr1160"
| TUPLE8(STRING("exprNoStr1292"), LPAREN, arg2, COLON, arg4, COLON, arg6, RPAREN) as oth -> mayfail oth  "exprNoStr1292"
| TUPLE8(STRING("fexpr1226"), LPAREN, arg2, COLON, arg4, COLON, arg6, RPAREN) as oth -> mayfail oth  "fexpr1226"
| TUPLE8(STRING("function_declaration1024"), Function, EMPTY_TOKEN, arg3, arg4, arg5, Endfunction, arg7) ->
  (match arg3, arg4, arg5, arg7 with
       | TUPLE3 (STRING ("funcId1044"|"funcId1045"), typ, IDENTIFIER fid), EMPTY_TOKEN, guts, (EMPTY_TOKEN|TUPLE3 (STRING "endLabelE2519", _, _)) ->
           FunDecl(fid,mly typ,mly guts)
       | TUPLE4 (STRING ("funcId1042"), arg1, typ, IDENTIFIER fid), EMPTY_TOKEN, guts, (EMPTY_TOKEN|TUPLE3 (STRING "endLabelE2519", _, _)) ->
           FunDecl(fid,mly typ,mly guts)
       | oth -> othpat4 := Some oth; failwith "function_declaration1024")
| TUPLE8(STRING("function_declaration1024"), Function, Automatic, arg3, arg4, arg5, Endfunction, arg7) ->
  (match arg3, arg4, arg5, arg7 with
       | TUPLE3 (STRING ("funcId1044"|"funcId1045"), typ, IDENTIFIER fid), EMPTY_TOKEN, guts, (EMPTY_TOKEN|TUPLE3 (STRING "endLabelE2519", _, _)) ->
           AutoFunDecl(fid,mly typ,mly guts)
       | TUPLE4 (STRING ("funcId1042"), arg1, typ, IDENTIFIER fid), EMPTY_TOKEN, guts, (EMPTY_TOKEN|TUPLE3 (STRING "endLabelE2519", _, _)) ->
           AutoFunDecl(fid,mly typ,mly guts)
       | oth -> othpat4 := Some oth; failwith "function_declaration1024_auto")
| TUPLE8(STRING("function_declaration1025"), Function, arg2, arg3, arg4, arg5, Endfunction, arg7) as oth -> mayfail oth  "function_declaration1025"
| TUPLE8(STRING("gateBufif01431"), arg1, arg2, COMMA, arg4, COMMA, arg6, RPAREN) as oth -> mayfail oth  "gateBufif01431"
| TUPLE8(STRING("gateBufif11432"), arg1, arg2, COMMA, arg4, COMMA, arg6, RPAREN) as oth -> mayfail oth  "gateBufif11432"
| TUPLE8(STRING("gateNotif01434"), arg1, arg2, COMMA, arg4, COMMA, arg6, RPAREN) as oth -> mayfail oth  "gateNotif01434"
| TUPLE8(STRING("gateNotif11435"), arg1, arg2, COMMA, arg4, COMMA, arg6, RPAREN) as oth -> mayfail oth  "gateNotif11435"
| TUPLE8(STRING("interface_declaration106"), arg1, arg2, arg3, SEMICOLON, arg5, Endinterface, arg7)  ->
(match arg1, arg2, arg3, arg5, arg7 with
       | TUPLE4 (STRING "intFront108", Interface, EMPTY_TOKEN, IDENTIFIER id), params, ports, decls, EMPTY_TOKEN ->
       IntfDecl(id, mly params, mly ports, mly decls)
       | oth -> othpat5 := Some oth; failwith "interface_declaration106")
| TUPLE8(STRING("module_declaration51"), arg1, params, arg3, SEMICOLON, itmlst, Endmodule, arg7) ->
(match arg1,arg3,arg7 with
       | TUPLE4 (STRING "modFront54", Module, EMPTY_TOKEN, IDENTIFIER modid), EMPTY_TOKEN, EMPTY_TOKEN ->
let ports', itms = itemsf [] itmlst in
let m = Modul (modid, parmf params, ports', itms) in
modules := (modid,m) :: !modules;
m
       | TUPLE4 (STRING "modFront54", Module, arg2, IDENTIFIER modid), TUPLE4(STRING "portsStarE78", LPAREN, TLIST portlst, RPAREN), (EMPTY_TOKEN|TUPLE3 (STRING "endLabelE2519", COLON, IDENTIFIER _)) ->
let ports', itms = itemsf portlst itmlst in
let m = Modul (modid, parmf params, ports', itms) in
modules := (modid,m) :: !modules;
m
| oth -> othpat3 := Some oth; failwith "module_declaration51")
| TUPLE8(STRING("module_declaration52"), arg1, arg2, arg3, SEMICOLON, arg5, Endprimitive, arg7) as oth -> mayfail oth  "module_declaration52"
| TUPLE8(STRING("port84"), arg1, bus, dot, dir, member, arg6, arg7) ->
    (match arg1, arg6, arg7, dir, dot, bus, member with
      | EMPTY_TOKEN, EMPTY_TOKEN, EMPTY_TOKEN, IDENTIFIER dir, DOT, IDENTIFIER bus, IDENTIFIER member -> Dot3(Id bus, Id dir, Id member)
      | EMPTY_TOKEN, TLIST lst, EMPTY_TOKEN, IDENTIFIER dir, DOT, IDENTIFIER bus, IDENTIFIER member -> DotBus(Id bus, Id dir, Id member, rml lst)
      | oth -> othpat7 := Some oth; failwith "port84")
| TUPLE8(STRING("port86"), arg1, Interface, DOT, arg4, arg5, arg6, arg7) as oth -> mayfail oth  "port86"
| TUPLE8(STRING("port93"), arg1, arg2, arg3, arg4, arg5, EQUALS, arg7) as oth -> mayfail oth  "port93"
| TUPLE8(STRING("program_declaration133"), arg1, arg2, arg3, SEMICOLON, arg5, Endprogram, arg7) as oth -> mayfail oth  "program_declaration133"
| TUPLE8(STRING("simple_immediate_assertion_statement2532"), Assert, LPAREN, arg3, RPAREN, arg5, Else, arg7) as oth -> mayfail oth  "simple_immediate_assertion_statement2532"
| TUPLE8(STRING("simple_immediate_assertion_statement2535"), Assume, LPAREN, arg3, RPAREN, arg5, Else, arg7) as oth -> mayfail oth  "simple_immediate_assertion_statement2535"
| TUPLE8(STRING("statementFor688"), For, LPAREN, arg3, SEMICOLON, arg5, RPAREN, arg7) as oth -> mayfail oth  "statementFor688"
| TUPLE8(STRING("statement_item672"), Do, arg2, While, LPAREN, arg5, RPAREN, SEMICOLON) as oth -> mayfail oth  "statement_item672"
| TUPLE8(STRING("system_f_call_or_t946"), DLR_fscanf, LPAREN, arg3, COMMA, arg5, arg6, RPAREN) -> SysFuncCall("$fscanf", mly arg3 :: mly arg5 :: mly arg6 ::[])
| TUPLE8(STRING("system_f_call_or_t989"), DLR_sscanf, LPAREN, arg3, COMMA, arg5, arg6, RPAREN) as oth -> mayfail oth  "system_f_call_or_t989"
| TUPLE8(STRING("type_declaration368"), Typedef, arg2, arg3, arg4, arg5, arg6, SEMICOLON) ->
( match arg2, arg3, arg4, arg5, arg6 with
	| TYPE_HYPHEN_IDENTIFIER id_t, TLIST lst, IDENTIFIER id, EMPTY_TOKEN, EMPTY_TOKEN -> Typ10(id_t, rml lst, id)
        | oth -> failwith "type_declaration368")
| TUPLE8(STRING("vltItem2663"), arg1, HYPHEN_HYPHEN_file, STRING arg3, HYPHEN_HYPHEN_lines, INTEGER_NUMBER arg5, HYPHEN, INTEGER_NUMBER arg7) as oth -> mayfail oth  "vltItem2663"
| TUPLE8(STRING("vltItem2668"), arg1, HYPHEN_HYPHEN_file, STRING arg3, HYPHEN_HYPHEN_lines, INTEGER_NUMBER arg5, HYPHEN, INTEGER_NUMBER arg7) as oth -> mayfail oth  "vltItem2668"
| TUPLE9(STRING("array_methodWith1093"), arg1, LPAREN, arg3, RPAREN, With_HYPHEN_then_HYPHEN_LPAREN, LPAREN, arg7, RPAREN) as oth -> mayfail oth  "array_methodWith1093"
| TUPLE9(STRING("class_declaration2562"), arg1, arg2, arg3, arg4, SEMICOLON, arg6, Endclass, arg8) as oth -> mayfail oth  "class_declaration2562"
| TUPLE9(STRING("clocking_declaration2521"), Default, Clocking, AT, LPAREN, arg5, RPAREN, SEMICOLON, Endclocking) as oth -> mayfail oth  "clocking_declaration2521"
| TUPLE9(STRING("deferred_immediate_assertion_statement2541"), Assert, arg2, LPAREN, arg4, RPAREN, arg6, Else, arg8) as oth -> mayfail oth  "deferred_immediate_assertion_statement2541"
| TUPLE9(STRING("deferred_immediate_assertion_statement2544"), Assume, arg2, LPAREN, arg4, RPAREN, arg6, Else, arg8) as oth -> mayfail oth  "deferred_immediate_assertion_statement2544"
| TUPLE9(STRING("delay_control495"), HASH, LPAREN, arg3, COMMA, arg5, COMMA, arg7, RPAREN) as oth -> mayfail oth  "delay_control495"
| TUPLE9(STRING("expr1163"), Type, LPAREN, arg3, RPAREN, QUOTE, LPAREN, arg7, RPAREN) as oth -> mayfail oth  "expr1163"
| TUPLE9(STRING("exprNoStr1295"), Type, LPAREN, arg3, RPAREN, QUOTE, LPAREN, arg7, RPAREN) as oth -> mayfail oth  "exprNoStr1295"
| TUPLE9(STRING("exprOkLvalue1309"), LBRACE, arg2, RBRACE, LBRACK, arg5, COLON, arg7, RBRACK) as oth -> mayfail oth  "exprOkLvalue1309"
| TUPLE9(STRING("exprOkLvalue1310"), LBRACE, arg2, RBRACE, LBRACK, arg5, PLUS_COLON, arg7, RBRACK) as oth -> mayfail oth  "exprOkLvalue1310"
| TUPLE9(STRING("exprOkLvalue1311"), LBRACE, arg2, RBRACE, LBRACK, arg5, HYPHEN_COLON, arg7, RBRACK) as oth -> mayfail oth  "exprOkLvalue1311"
| TUPLE9(STRING("fexpr1229"), Type, LPAREN, arg3, RPAREN, QUOTE, LPAREN, arg7, RPAREN) as oth -> mayfail oth  "fexpr1229"
| TUPLE9(STRING("fexprOkLvalue1318"), LBRACE, arg2, RBRACE, LBRACK, arg5, COLON, arg7, RBRACK) as oth -> mayfail oth  "fexprOkLvalue1318"
| TUPLE9(STRING("fexprOkLvalue1319"), LBRACE, arg2, RBRACE, LBRACK, arg5, PLUS_COLON, arg7, RBRACK) as oth -> mayfail oth  "fexprOkLvalue1319"
| TUPLE9(STRING("fexprOkLvalue1320"), LBRACE, arg2, RBRACE, LBRACK, arg5, HYPHEN_COLON, arg7, RBRACK) as oth -> mayfail oth  "fexprOkLvalue1320"
| TUPLE9(STRING("foperator_assignment692"), arg1, EQUALS, DLR_fopen, LPAREN, arg5, COMMA, arg7, RPAREN) -> Asgn1(mly arg1, Sys("$fopen", Itmlst(rml(arg5::arg7::[]))))
| TUPLE9(STRING("port94"), arg1, Var, arg3, arg4, arg5, arg6, EQUALS, arg8) as oth -> mayfail oth  "port94"
| TUPLE9(STRING("port95"), arg1, Var, arg3, arg4, arg5, arg6, EQUALS, arg8) as oth -> mayfail oth  "port95"
| TUPLE9(STRING("statementFor687"), For, LPAREN, arg3, stop, SEMICOLON, inc, RPAREN, stmts) ->
  (match arg3 with
    | TUPLE2 (TLIST strtlst, SEMICOLON) -> ForLoop (rml strtlst, mly stop, mly inc, mly stmts)
    | TUPLE3 (STRING "for_initialization770", TLIST strtlst, SEMICOLON) -> ForLoop (rml strtlst, mly stop, mly inc, mly stmts)
    | oth -> othpat1 := Some oth; failwith "statementFor687")
| TUPLE9(STRING("statement_item656"), arg1, If, LPAREN, arg4, RPAREN, arg6, Else, arg8) ->
   ( match arg1, arg4, arg6, arg8 with
    | EMPTY_TOKEN, expr, stmt1, stmt2 when pred1 stmt1 && pred1 stmt2 -> If2(mly expr, mly arg6, mly arg8)
    | oth -> othpat4 := Some oth; failwith "statement_item656")
| TUPLE9(STRING("statement_item659"), Void, QUOTE, LPAREN, arg4, DOT, arg6, RPAREN, SEMICOLON) as oth -> mayfail oth  "statement_item659"
| TUPLE9(STRING("system_f_call_or_t929"), DLR_countbits, LPAREN, arg3, COMMA, arg5, COMMA, arg7, RPAREN) as oth -> mayfail oth  "system_f_call_or_t929"
| TUPLE9(STRING("system_f_call_or_t942"), DLR_fread, LPAREN, arg3, COMMA, arg5, COMMA, arg7, RPAREN) as oth -> mayfail oth  "system_f_call_or_t942"
| TUPLE9(STRING("system_f_call_or_t947"), DLR_fseek, LPAREN, arg3, COMMA, arg5, COMMA, arg7, RPAREN) as oth -> mayfail oth  "system_f_call_or_t947"
| TUPLE9(STRING("system_f_call_or_t967"), DLR_past, LPAREN, arg3, COMMA, arg5, COMMA, arg7, RPAREN) as oth -> mayfail oth  "system_f_call_or_t967"
| TUPLE9(STRING("system_t_call891"), DLR_readmemb, LPAREN, arg3, COMMA, arg5, COMMA, arg7, RPAREN) as oth -> mayfail oth  "system_t_call891"
| TUPLE9(STRING("system_t_call894"), DLR_readmemh, LPAREN, arg3, COMMA, arg5, COMMA, arg7, RPAREN) as oth -> mayfail oth  "system_t_call894"
| TUPLE9(STRING("system_t_call897"), DLR_writememb, LPAREN, arg3, COMMA, arg5, COMMA, arg7, RPAREN) as oth -> mayfail oth  "system_t_call897"
| TUPLE9(STRING("system_t_call900"), DLR_writememh, LPAREN, arg3, COMMA, arg5, COMMA, arg7, RPAREN) as oth -> mayfail oth  "system_t_call900"
| TUPLE9(STRING("type_declaration366"), Typedef, arg2, arg3, arg4, arg5, arg6, arg7, SEMICOLON) as oth -> mayfail oth  "type_declaration366"
| TUPLE9(STRING("type_declaration369"), Typedef, arg2, arg3, arg4, arg5, arg6, arg7, SEMICOLON) as oth -> mayfail oth  "type_declaration369"
| TUPLE10(STRING("loop_generate_construct460"), For, LPAREN, arg3, SEMICOLON, limit, SEMICOLON, arg7, RPAREN, arg9) ->
  let ix, strt = match arg3 with
    | TUPLE5 (STRING "genvar_initialization462", Genvar, TUPLE3 (STRING "genvar_identifierDecl174", IDENTIFIER ix, EMPTY_TOKEN), EQUALS, strt)
    | TUPLE4 (STRING "genvar_initialization461", IDENTIFIER ix, EQUALS, strt) -> ix, strt
    | oth -> othpat1 := Some oth; failwith "loop_generate_construct460_arg3" in
  let inc = match arg7 with
    | TUPLE3 (STRING "genvar_iteration477", IDENTIFIER k'', PLUS_PLUS) -> Intgr 1
    | TUPLE3 (STRING "genvar_iteration475", PLUS_PLUS, IDENTIFIER k'') -> Intgr 1
    | TUPLE4 (STRING "genvar_iteration463", IDENTIFIER i'', EQUALS, ELIST [IDENTIFIER i'''; PLUS; inc]) when i'' = i''' -> mly inc
    | oth -> othpat1 := Some oth; failwith "loop_generate_construct460_arg7" in
  let lbl, body = match arg9 with
    | TUPLE7 (STRING "genItemBegin450", Begin, COLON, IDENTIFIER lbl, TLIST body, End, EMPTY_TOKEN) -> lbl, rml body
    | TUPLE4 (STRING "genItemBegin446", Begin, TLIST body, End) -> "", rml body
    | TUPLE6 (STRING "continuous_assign427", Assign, EMPTY_TOKEN, EMPTY_TOKEN, TLIST body, SEMICOLON) -> "", ContAsgn (rml body) :: []
    | oth -> othpat1 := Some oth; failwith "loop_generate_construct460_arg9" in
   LoopGen1(Id ix, lbl, mly strt, mly limit, inc, body)
	       
| TUPLE10(STRING("type_declaration367"), Typedef, arg2, arg3, arg4, arg5, arg6, arg7, arg8, SEMICOLON) as oth -> mayfail oth  "type_declaration367"
| TUPLE11(STRING("property_spec2554"), AT, LPAREN, arg3, RPAREN, Disable, Iff, LPAREN, arg8, RPAREN, arg10) as oth -> mayfail oth  "property_spec2554"
| TUPLE11(STRING("system_f_call_or_t930"), DLR_countbits, LPAREN, arg3, COMMA, arg5, COMMA, arg7, COMMA, arg9, RPAREN) as oth -> mayfail oth  "system_f_call_or_t930"
| TUPLE11(STRING("system_f_call_or_t943"), DLR_fread, LPAREN, arg3, COMMA, arg5, COMMA, arg7, COMMA, arg9, RPAREN) as oth -> mayfail oth  "system_f_call_or_t943"
| TUPLE11(STRING("system_f_call_or_t968"), DLR_past, LPAREN, arg3, COMMA, arg5, COMMA, arg7, COMMA, arg9, RPAREN) as oth -> mayfail oth  "system_f_call_or_t968"
| TUPLE11(STRING("system_t_call889"), DLR_timeformat, LPAREN, arg3, COMMA, arg5, COMMA, arg7, COMMA, arg9, RPAREN) as oth -> mayfail oth  "system_t_call889"
| TUPLE11(STRING("system_t_call892"), DLR_readmemb, LPAREN, arg3, COMMA, arg5, COMMA, arg7, COMMA, arg9, RPAREN) as oth -> mayfail oth  "system_t_call892"
| TUPLE11(STRING("system_t_call895"), DLR_readmemh, LPAREN, arg3, COMMA, arg5, COMMA, arg7, COMMA, arg9, RPAREN) as oth -> mayfail oth  "system_t_call895"
| TUPLE11(STRING("system_t_call898"), DLR_writememb, LPAREN, arg3, COMMA, arg5, COMMA, arg7, COMMA, arg9, RPAREN) as oth -> mayfail oth  "system_t_call898"
| TUPLE11(STRING("system_t_call901"), DLR_writememh, LPAREN, arg3, COMMA, arg5, COMMA, arg7, COMMA, arg9, RPAREN) as oth -> mayfail oth  "system_t_call901"
| TUPLE13(STRING("system_f_call_or_t931"), DLR_countbits, LPAREN, arg3, COMMA, arg5, COMMA, arg7, COMMA, arg9, COMMA, arg11, RPAREN) as oth -> mayfail oth  "system_f_call_or_t931"
| ELIST lst -> priority lst
| TLIST lst -> Itmlst(rml lst)
| TUPLE3 (lbl, COLON, TUPLE5 (STRING "seq_block615", Begin, TLIST lst, End, EMPTY_TOKEN)) ->
    CaseStmt((match lbl with TLIST lst -> rml lst | oth -> mly oth :: []), rml lst)
| TUPLE3 (lbl, COLON, stmt) when pred1 stmt ->
    CaseStmt((match lbl with TLIST lst -> rml lst | oth -> mly oth :: []), mly stmt :: [])
| TUPLE3 (Default, COLON, SEMICOLON) -> CaseStmt(mly Default :: [], [])
| oth -> othpat1 := Some oth; failwith "mly"

and other msg = function
  | TUPLE9 (STRING stmt, a, b, c, d, e, f, g, h) -> Unknown(msg, rml [a;b;c;d;e;f;g;h])
  | TUPLE8 (STRING stmt, a, b, c, d, e, f, g) -> Unknown(msg, rml [a;b;c;d;e;f;g])
  | TUPLE7 (STRING stmt, a, b, c, d, e, f) -> Unknown(msg, rml [a;b;c;d;e;f])
  | TUPLE6 (STRING stmt, a, b, c, d, e) -> Unknown(msg, rml [a;b;c;d;e])
  | TUPLE5 (STRING stmt, a, b, c, d) -> Unknown(msg, rml [a;b;c;d])
  | TUPLE4 (STRING stmt, a, b, c) -> Unknown(msg, rml [a;b;c])
  | TUPLE3 (STRING stmt, a, b) -> Unknown(msg, rml [a;b])
  | TUPLE2 (STRING stmt, a) -> Unknown(msg, rml [a])
  | oth -> failwith msg

and mayfail oth msg = if !canfail then (othpat1 := Some oth; failwith msg) else other msg oth

and signed_flag x = mly x

and parmf = function
    | EMPTY_TOKEN -> []
    | TUPLE5(STRING "parameter_port_listE69", HASH, LPAREN, TLIST plst, RPAREN) ->
        List.rev_map (function
	      | TUPLE3 (STRING "paramPortDeclOrArg72", EMPTY_TOKEN,
                        TUPLE5 (STRING "param_assignment537", IDENTIFIER nam, EMPTY_TOKEN, EMPTY_TOKEN,
				       TUPLE3 (STRING "exprOrDataTypeEqE1106", EQUALS, expr))) ->
				       Param(nam, mly expr, [])
              | TUPLE3 (STRING "paramPortDeclOrArg72", TUPLE3 (STRING "parameter_port_declarationFrontE180", (Parameter|Localparam), EMPTY_TOKEN),
			       TUPLE5 (STRING "param_assignment537", IDENTIFIER nam, EMPTY_TOKEN, EMPTY_TOKEN,
					      TUPLE3 (STRING "exprOrDataTypeEqE1106", EQUALS, expr))) -> Param(nam, mly expr, [])
	      | TUPLE3 (STRING "paramPortDeclOrArg72",
                        TUPLE3 (STRING "parameter_port_declarationFrontE181", Parameter, TUPLE4 (STRING "data_typeBasic263", (Bit|Logic), EMPTY_TOKEN, dims)),
                        TUPLE5 (STRING "param_assignment537", IDENTIFIER nam, EMPTY_TOKEN, EMPTY_TOKEN,
				       TUPLE3 (STRING "exprOrDataTypeEqE1106", EQUALS, expr))) ->
				       Param(nam, mly expr, match dims with TLIST lst -> rml lst | EMPTY_TOKEN -> [] | oth -> othpat1 := Some oth; failwith "dim param1519")
	      | TUPLE3 (STRING "paramPortDeclOrArg72",
                        TUPLE3 (STRING "parameter_port_declarationFrontE181", (Parameter|Localparam),
                        TUPLE3 (STRING "data_typeBasic264", (Int|Integer|Longint), (EMPTY_TOKEN|Unsigned))),
                        TUPLE5 (STRING "param_assignment537", IDENTIFIER nam, EMPTY_TOKEN, EMPTY_TOKEN,
				       TUPLE3 (STRING "exprOrDataTypeEqE1106", EQUALS, expr))) -> Param(nam, mly expr, [])
	      | TUPLE3 (STRING "paramPortDeclOrArg72",
                  TUPLE3 (STRING "parameter_port_declarationFrontE181", (Parameter|Localparam), TUPLE4 (STRING "data_type261", EMPTY_TOKEN, TYPE_HYPHEN_IDENTIFIER id_cc, EMPTY_TOKEN)),
                  TUPLE5 (STRING "param_assignment537", IDENTIFIER nam, EMPTY_TOKEN, EMPTY_TOKEN,
				       TUPLE3 (STRING "exprOrDataTypeEqE1106", EQUALS, expr))) -> PackageParam2(id_cc, nam, [], mly expr)
	      | TUPLE3 (STRING "paramPortDeclOrArg72",
                  TUPLE3 (STRING "parameter_port_declarationFrontE181", (Parameter|Localparam), TUPLE4 (STRING "data_type261", TLIST pkglst, TYPE_HYPHEN_IDENTIFIER id_cc, EMPTY_TOKEN)),
                  TUPLE5 (STRING "param_assignment537", IDENTIFIER nam, EMPTY_TOKEN, EMPTY_TOKEN,
				       TUPLE3 (STRING "exprOrDataTypeEqE1106", EQUALS, expr))) -> PackageParam2(id_cc, nam, rml pkglst, mly expr)
              | TUPLE3 (STRING "paramPortDeclOrArg73",
			TUPLE3 (STRING "parameter_port_declarationTypeFrontE184", Parameter, Type),
			TUPLE5 (STRING "type_assignment540", (TYPE_HYPHEN_IDENTIFIER nam|IDENTIFIER nam), EMPTY_TOKEN, EQUALS,
				       TUPLE4 (STRING "data_typeBasic263", (Logic as kind), EMPTY_TOKEN, typ_expr))) ->
				       TypParam(nam, mly kind, match typ_expr with
						     | TLIST lst -> rml lst | EMPTY_TOKEN -> [] | oth -> othpat1 := Some oth; failwith "typ_param1529")
              | TUPLE3 (STRING "paramPortDeclOrArg73",
			TUPLE3 (STRING "parameter_port_declarationTypeFrontE184", Parameter, Type),
			TUPLE5 (STRING "type_assignment540", (TYPE_HYPHEN_IDENTIFIER nam|IDENTIFIER nam), EMPTY_TOKEN, EQUALS,
			               TUPLE4 (STRING "data_type261", typ_expr, (TYPE_HYPHEN_IDENTIFIER typ_t|IDENTIFIER typ_t), EMPTY_TOKEN))) ->
				       TypParam(nam, Id typ_t, match typ_expr with
						     | TLIST lst -> rml lst | EMPTY_TOKEN -> [] | oth -> othpat1 := Some oth; failwith "typ_param1540")
	      | TUPLE3 (STRING "paramPortDeclOrArg72",
                        TUPLE3 (STRING "parameter_port_declarationFrontE180", Parameter, TUPLE3 (STRING "implicit_typeE363", EMPTY_TOKEN, TLIST lst)),
                        TUPLE5 (STRING "param_assignment537", IDENTIFIER nam, EMPTY_TOKEN, EMPTY_TOKEN,
                                       TUPLE3 (STRING "exprOrDataTypeEqE1106", EQUALS, expr))) -> Param(nam, mly expr, rml lst)
	      | oth -> othpat1 := Some oth; failwith "param1531") plst
     | TUPLE3 (STRING "importsAndParametersE56", TLIST importlst, params) -> let pkg = rml importlst in List.map (fun itm -> PackageParam(pkg, itm)) (parmf params)
     | oth -> othpat1 := Some oth; failwith "param1532"

and itemsf portlst itmlst =	      
	      let itms = match itmlst with TLIST itmlst -> rml itmlst | EMPTY_TOKEN -> [] | oth -> mly oth :: [] in
	      let portdecl, itms = List.partition (function DeclLogic (Port _ :: _) -> true | _ -> false) itms in
              let porthash = Hashtbl.create 255 in
              let _ = List.iter (function DeclLogic lst -> List.iter (function Port(dir, nam, dimlst, xlst) -> Hashtbl.add porthash nam (dir,dimlst) | _ -> ()) lst | _ -> ()) portdecl in
	      let ports = rml portlst in
              dbgportitms := ports;
              (* deal with ANSI default port syntax *)
	      let ports' = let dir' = ref Deflt in List.map (function
				      | Id port -> let dir,dims = Hashtbl.find porthash port in Port(dir, port, dims, Deflt)
				      | Port (Deflt, port, dims, xlst) -> Port (!dir', port, dims, xlst)
				      | Port (dir, port, dims, xlst) as x -> dir' := dir; x
                                      | Dot3 _ as x -> x (* placeholder *)
                                      | DotBus _ as x -> x (* placeholder *)
				      | oth -> remap := Some oth; failwith "ports") ports in
	      ports', itms
	      
and rml pat = List.rev_map mly pat

and prior_all lst = prior12 (prior11 (prior10 (prior9 (prior8 (prior7 (prior6 (prior5 (prior4 (prior3 (prior2 (prior1 lst)))))))))))

and priority explst =
  let matched = prior_all (prior_all (prior_all (prior_all (prior_all explst)))) in
  mlst := (explst,matched) :: !mlst; match matched with hd :: [] -> mly hd | oth -> priorlst := Some oth; failwith "priority"

and prior1 = function
| [] -> []
| lhs :: (STAR_STAR as op) :: rhs :: tl -> prior1 (TUPLE4(enc_expr op, lhs, op, rhs) :: tl)
| hd :: tl -> hd :: prior1 tl

and prior2 = function
| [] -> []
| lhs :: (STAR|SLASH as op) :: rhs :: tl -> prior2 (TUPLE4(enc_expr op, lhs, op, rhs) :: tl)
| hd :: tl -> hd :: prior2 tl

and prior3 = function
| [] -> []
| lhs :: (PLUS|HYPHEN as op) :: rhs :: tl -> prior3 (TUPLE4(enc_expr op, lhs, op, rhs) :: tl)
| hd :: tl -> hd :: prior3 tl

and prior4 = function
| [] -> []
| lhs :: (LT_LT|GT_GT|GT_GT_GT (* |LT_LT_LT *) as op) :: rhs :: tl -> prior4 (TUPLE4(enc_expr op, lhs, op, rhs) :: tl)
| hd :: tl -> hd :: prior4 tl

and prior5 = function
| [] -> []
| lhs :: (LESS|LT_EQ|GREATER|GT_EQ as op) :: rhs :: tl -> prior5 (TUPLE4(enc_expr op, lhs, op, rhs) :: tl)
| hd :: tl -> hd :: prior5 tl

and prior6 = function
| [] -> []
| lhs :: (EQ_EQ|PLING_EQ|EQ_EQ_EQ|PLING_EQ_EQ as op) :: rhs :: tl -> prior6 (TUPLE4(enc_expr op, lhs, op, rhs) :: tl)
| hd :: tl -> hd :: prior6 tl

and prior7 = function
| [] -> []
| lhs :: (AMPERSAND as op) :: rhs :: tl -> prior7 (TUPLE4(enc_expr op, lhs, op, rhs) :: tl)
| hd :: tl -> hd :: prior6 tl

and prior8 = function
| [] -> []
| lhs :: (CARET|CARET_TILDE as op) :: rhs :: tl -> prior8 (TUPLE4(enc_expr op, lhs, op, rhs) :: tl)
| hd :: tl -> hd :: prior8 tl

and prior9 = function
| [] -> []
| lhs :: (VBAR as op) :: rhs :: tl -> prior9 (TUPLE4(STRING "expr1137", lhs, op, rhs) :: tl)
| hd :: tl -> hd :: prior9 tl

and prior10 = function
| [] -> []
| lhs :: (AMPERSAND_AMPERSAND as op) :: rhs :: tl -> prior10 (TUPLE4(enc_expr op, lhs, op, rhs) :: tl)
| hd :: tl -> hd :: prior10 tl

and prior11 = function
| [] -> []
| lhs :: (VBAR_VBAR as op) :: rhs :: tl -> prior11 (TUPLE4(enc_expr op, lhs, op, rhs) :: tl)
| hd :: tl -> hd :: prior11 tl

and prior12 = function
| [] -> []
| lhs :: QUERY :: tl -> (match prior_all tl with rhs :: COLON :: rhs' :: tl -> prior_all (TUPLE5(lhs, QUERY, rhs, COLON, rhs') :: tl) | oth -> failwith "query")
| hd :: tl -> hd :: prior12 tl

and attach_lbl = function
| [] -> []
| (TLIST ((IDENTIFIER _ | INTEGER_NUMBER _) :: _) | Default) as l :: COLON :: (TUPLE5(STRING "seq_block615", Begin, TLIST _, End, EMPTY_TOKEN) as stmt) :: tl -> TUPLE3 (l, COLON, stmt) :: attach_lbl tl
| (TLIST ((IDENTIFIER _ | INTEGER_NUMBER _) :: _) | Default) as l :: COLON :: stmt :: tl when pred1 stmt -> TUPLE3 (l, COLON, stmt) :: attach_lbl tl
| (IDENTIFIER _ | INTEGER_NUMBER _) as l :: COLON :: stmt :: tl when pred1 stmt -> TUPLE3 (l, COLON, stmt) :: attach_lbl tl
| (TLIST (TUPLE3 (STRING "exprScope1328", _, _) :: _) as l) :: COLON :: stmt :: tl when pred1 stmt -> TUPLE3 (l, COLON, stmt) :: attach_lbl tl
| stmt :: ((IDENTIFIER _ | INTEGER_NUMBER _) as l) :: [] when pred1 stmt -> TUPLE3 (l, COLON, stmt) :: []
| Default :: COLON :: SEMICOLON :: TLIST ( (IDENTIFIER _ | INTEGER_NUMBER _) :: _ as l) :: COLON :: stmt :: tl when pred1 stmt ->
    TUPLE3 (TLIST (Default :: l), COLON, stmt) :: attach_lbl tl
| (TUPLE5(STRING "seq_block615", Begin, TLIST _, End, EMPTY_TOKEN) as stmt) :: (TUPLE3 (STRING "exprScope1328", _, _) as l) :: [] ->
 TUPLE3 (l, COLON, stmt) :: []
| (TUPLE5(STRING "seq_block615", Begin, TLIST _, End, EMPTY_TOKEN) as stmt) :: (TUPLE6 (STRING "value_range747", LBRACK, lft, COLON, rght, RBRACK) as l) :: [] ->
 TUPLE3 (l, COLON, stmt) :: []
| stmt :: ((IDENTIFIER _ | INTEGER_NUMBER _) as l) :: ((IDENTIFIER _ | INTEGER_NUMBER _) as l') :: [] when pred1 stmt -> TUPLE3 (TLIST (l::l'::[]), COLON, stmt) :: []
| (TUPLE5(STRING "seq_block615", Begin, TLIST _, End, EMPTY_TOKEN) as stmt) :: tl when labels tl -> TUPLE3 (TLIST tl, COLON, stmt) :: []
| stmt :: tl when pred1 stmt && labels tl -> TUPLE3 (TLIST tl, COLON, stmt) :: []
| Default :: COLON :: SEMICOLON :: tl -> TUPLE3 (Default, COLON, SEMICOLON) :: attach_lbl tl
| stmt :: (TUPLE3 (STRING "exprScope1328", _, _) as l) :: [] when pred1 stmt -> TUPLE3(l, COLON, stmt) :: []
| (TUPLE3 (STRING "exprScope1328", _, _) as l) :: stmt :: tl when pred1 stmt -> TUPLE3(l, COLON, stmt) :: attach_lbl tl
| stmt :: (TUPLE4 (STRING "open_range_list744", _, _, _) as l) :: [] when pred1 stmt -> TUPLE3(l, COLON, stmt) :: []
| TUPLE6 (STRING "value_range747", LBRACK, lft, COLON, rght, RBRACK) as l :: COLON :: (TUPLE5(STRING "seq_block615", Begin, TLIST _, End, EMPTY_TOKEN) as stmt) :: tl -> TUPLE3 (l, COLON, stmt) :: attach_lbl tl
| SEMICOLON :: (TUPLE6 (STRING "value_range747", LBRACK, lft, COLON, rght, RBRACK) as l) :: COLON :: (TUPLE5(STRING "seq_block615", Begin, TLIST _, End, EMPTY_TOKEN) as stmt) :: tl -> TUPLE3 (l, COLON, stmt) :: attach_lbl tl
| (TUPLE3 (STRING "exprScope1328", _, _) as l) :: COLON :: stmt :: tl when pred1 stmt -> TUPLE3(l, COLON, stmt) :: attach_lbl tl
| SEMICOLON :: (TUPLE6 (STRING "value_range747", LBRACK, lft, COLON, rght, RBRACK) as l) :: [] -> TUPLE3(l, COLON, SEMICOLON) :: []
| stmt :: Default :: [] -> TUPLE3(Default, COLON, stmt) :: []
| stmt :: (ELIST _ as l) :: [] -> TUPLE3(l, COLON, stmt) :: []
| (TLIST [TUPLE4 (STRING "exprOkLvalue1307", LBRACE, TLIST _, RBRACE)] as l) :: COLON :: stmt :: tl when pred1 stmt -> TUPLE3(l, COLON, stmt) :: attach_lbl tl
| stmt :: (TUPLE4 (STRING "exprOkLvalue1307", LBRACE, TLIST _, RBRACE) as l) :: tl when pred1 stmt -> TUPLE3(l, COLON, stmt) :: attach_lbl tl
| TLIST [TUPLE3 (STRING "exprScope1328", TLIST _, _) as l] :: COLON :: stmt :: tl when pred1 stmt -> TUPLE3(l, COLON, stmt) :: attach_lbl tl
| TLIST [TUPLE3 (STRING "expr1113", _, _) as l] :: COLON :: stmt :: tl when pred1 stmt -> TUPLE3(l, COLON, stmt) :: attach_lbl tl
| TLIST [ELIST _ as l] :: COLON :: stmt :: tl when pred1 stmt -> TUPLE3(l, COLON, stmt) :: attach_lbl tl
| TLIST (ELIST _ as l :: IDENTIFIER x :: []) :: COLON :: stmt :: tl when pred1 stmt -> TUPLE3(l, COLON, stmt) :: attach_lbl tl
| oth -> attlst := oth; failwith "attach"

and labels = function
| [] -> true
| (IDENTIFIER _ | INTEGER_NUMBER _ | TUPLE3 (STRING "exprScope1328", TLIST _, IDENTIFIER _)) :: tl -> labels tl
| _ -> false
