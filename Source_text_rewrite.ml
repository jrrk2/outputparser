open Source_text_rewrite_types
open Source_text_lex
open Source_text

let rec rw = function
| CONS1 oth -> TLIST (rw oth::[])
| CONS3(lft,_,rght) -> (match rw lft with TLIST arg -> TLIST (rw rght :: arg) | _ -> failwith "CONS3")
| CONS4(lft,arg1,arg2,arg3) -> (match rw lft with TLIST arg -> TLIST (rw arg1 :: rw arg2 :: rw arg3 :: arg) | _ -> failwith "CONS4")
| CONS2(lft,rght) -> (match rw lft with TLIST arg -> TLIST (rw rght :: arg) | _ -> failwith "CONS2")
| TLIST lst -> TLIST (List.map rw lst)
| TUPLE2(arg1,arg2) -> TUPLE2 (rw arg1, rw arg2)
| TUPLE3(arg1,arg2,arg3) -> TUPLE3 (rw arg1, rw arg2, rw arg3)
| TUPLE4(arg1,arg2,arg3,arg4) -> TUPLE4 (rw arg1, rw arg2, rw arg3, rw arg4)
| TUPLE5(arg1,arg2,arg3,arg4,arg5) -> TUPLE5 (rw arg1, rw arg2, rw arg3, rw arg4, rw arg5)
| TUPLE6(arg1,arg2,arg3,arg4,arg5,arg6) -> TUPLE6 (rw arg1, rw arg2, rw arg3, rw arg4, rw arg5, rw arg6)
| TUPLE7(arg1,arg2,arg3,arg4,arg5,arg6,arg7) -> TUPLE7 (rw arg1, rw arg2, rw arg3, rw arg4, rw arg5, rw arg6, rw arg7)
| TUPLE8(arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8) ->
   TUPLE8(rw arg1, rw arg2, rw arg3, rw arg4, rw arg5, rw arg6, rw arg7, rw arg8)
| TUPLE9(arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9) ->
   TUPLE9(rw arg1, rw arg2, rw arg3, rw arg4, rw arg5, rw arg6, rw arg7, rw arg8, rw arg9)
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
| Always_comb|Always_ff
| Return
| Automatic
| EOF_TOKEN) as oth) -> oth
| oth -> failwith ("rw fail: "^Source_text_types.getstr oth)

let missing = ref None

let modules = Hashtbl.create 255

let rec rw' = function
| Input -> In
| Output -> Out
| TUPLE2 (TLIST lst, EOF_TOKEN) -> Itmlst(List.rev_map rw' lst)
| TUPLE4 ((Input|Output) as dir, IDENTIFIER port, EMPTY_TOKEN, EMPTY_TOKEN) -> Port(rw' dir, port, [])
| TUPLE5 ((Input|Output) as dir, TUPLE2 (Integer, EMPTY_TOKEN), IDENTIFIER port, EMPTY_TOKEN, EMPTY_TOKEN) ->
   Port(rw' dir, port, [])
| TUPLE5 ((Input|Output) as dir, TUPLE3 (EMPTY_TOKEN, TYPE_HYPHEN_IDENTIFIER id, lst), IDENTIFIER port, EMPTY_TOKEN, EMPTY_TOKEN) ->
   Port(rw' dir, port, Typ (id, []) :: match lst with EMPTY_TOKEN -> [] | TLIST lst -> List.rev_map rw' lst | _ -> failwith "port")
| TUPLE5 ((Input|Output) as dir, TUPLE3 (Logic, EMPTY_TOKEN, lst), IDENTIFIER port, EMPTY_TOKEN, EMPTY_TOKEN) ->
   Port(rw' dir, port, match lst with EMPTY_TOKEN -> [] | TLIST lst -> List.rev_map rw' lst | _ -> failwith "port")
| TUPLE6 ((Input|Output) as dir, EMPTY_TOKEN, TLIST lst, IDENTIFIER port, EMPTY_TOKEN, EMPTY_TOKEN) ->
   Port(rw' dir, port, List.rev_map rw' lst)
| TUPLE4 (TUPLE2 ((Input|Output) as dir, Wire), IDENTIFIER port, EMPTY_TOKEN, EMPTY_TOKEN) -> 
Port (rw' dir, port, [])
| TUPLE2 ((Input|Output) as dir, IDENTIFIER port) -> Port(rw' dir, port, [])
| TUPLE7 (TUPLE3 (Module, EMPTY_TOKEN, IDENTIFIER modid), params,
   TUPLE3(LPAREN, TLIST portlst, RPAREN), SEMICOLON, TLIST itmlst, Endmodule, EMPTY_TOKEN) ->
let m = Modul (modid, List.rev_map rw' (portlst), List.rev_map rw' (itmlst)) in
Hashtbl.add modules modid m;
m
| TUPLE5 (tag, QUOTE, LPAREN, exp, RPAREN) -> Cast(rw' tag, rw' exp)
| TUPLE3 (TUPLE2 (Posedge, IDENTIFIER clk), (Or|COMMA), TUPLE2 (Posedge, IDENTIFIER reset)) ->
  Edge(Pos clk, Pos reset)
| TUPLE3 (TUPLE2 (Posedge, IDENTIFIER clk), (Or|COMMA), TUPLE2 (Negedge, IDENTIFIER reset)) ->
  Edge(Pos clk, Neg reset)
| TUPLE3 (TUPLE2 (Negedge, IDENTIFIER clk), (Or|COMMA), TUPLE2 (Negedge, IDENTIFIER reset)) ->
  Edge(Neg clk, Neg reset)
| TUPLE2 (Posedge, IDENTIFIER clk) -> Pos clk
| TUPLE2 (Negedge, IDENTIFIER clk) -> Neg clk
| INTEGER_NUMBER n -> (try Intgr (int_of_string (String.trim n)) with err -> Number n)
| IDENTIFIER id -> Id id
| TUPLE3 (IDENTIFIER id, EMPTY_TOKEN, EMPTY_TOKEN) -> Id id
| TUPLE2 (IDENTIFIER id, SEMICOLON) -> Id id
| TUPLE7 (EMPTY_TOKEN, IDENTIFIER id1, DOT, IDENTIFIER id2, IDENTIFIER id3, EMPTY_TOKEN, EMPTY_TOKEN) ->
  Dot3(id1,id2,id3)
| TUPLE3 (lhs, DOT, rhs) -> Field (rw' lhs, rw' rhs)
| TUPLE4 (IDENTIFIER id, LBRACK, exp, RBRACK) -> Sel (id, rw' exp)
| TUPLE6 (IDENTIFIER id, LBRACK, lft, COLON, rght, RBRACK) -> Slice (id, rw' lft, rw' rght)
| TUPLE3 (TUPLE3 (exp1, COMMA, exp2), COMMA, exp3) -> Comma (rw' exp1, rw' exp2, rw' exp3)
| TUPLE5 (lhs, LT_EQ, EMPTY_TOKEN, exp, SEMICOLON) -> NonBlocking (rw' lhs, rw' exp)
| TUPLE2 (HYPHEN, rhs) -> UMinus(rw' rhs)
| TUPLE2 (PLING, rhs) -> Pling(rw' rhs)
| TUPLE2 (TILDE, rhs) -> Tilde(rw' rhs)
| TUPLE2 (CARET, rhs) -> Caret(rw' rhs)
| TUPLE2 (AMPERSAND, rhs) -> RedAnd(rw' rhs)
| TUPLE2 (VBAR, rhs) -> RedOr(rw' rhs)
| TUPLE2 (lhs, PLUS_PLUS) -> Inc(rw' lhs)
| TUPLE2 (lhs, HYPHEN_HYPHEN) -> Dec(rw' lhs)
| TUPLE7 (Function, Automatic, typ, EMPTY_TOKEN, body, Endfunction, elbl) ->
  let body = match body with
    | TUPLE5 (LPAREN, TLIST lst, RPAREN, SEMICOLON, TUPLE2 (TLIST lst2, TLIST lst3)) -> Unknown "function1"
    | TUPLE2 (SEMICOLON, TUPLE2 (TLIST lst, TLIST lst3)) -> Unknown "function2"
    | oth -> Unknown "function3" in body
| TUPLE4 (DLR_bits, LPAREN, rhs, RPAREN) -> Bits(match rhs with
  | TUPLE3 (EMPTY_TOKEN, TYPE_HYPHEN_IDENTIFIER typ, EMPTY_TOKEN) -> Typ(typ, [])
  | oth -> failwith "$bits")
| TUPLE4 (DLR_clog2, LPAREN, rhs, RPAREN) -> Clog2(rw' rhs)
| TUPLE4 (DLR_signed, LPAREN, rhs, RPAREN) -> Signed(rw' rhs)
| TUPLE4 (DLR_unsigned, LPAREN, rhs, RPAREN) -> Unsigned(rw' rhs)
| TUPLE3 (lhs, PLUS, rhs) -> Add(rw' lhs, rw' rhs)
| TUPLE3 (lhs, HYPHEN, rhs) -> Sub(rw' lhs, rw' rhs)
| TUPLE3 (lhs, STAR, rhs) -> Mult(rw' lhs, rw' rhs)
| TUPLE3 (lhs, SLASH, rhs) -> Div(rw' lhs, rw' rhs)
| TUPLE3 (lhs, AMPERSAND, rhs) -> And(rw' lhs, rw' rhs)
| TUPLE3 (lhs, AMPERSAND_AMPERSAND, rhs) -> And2(rw' lhs, rw' rhs)
| TUPLE3 (lhs, VBAR, rhs) -> Or(rw' lhs, rw' rhs)
| TUPLE3 (lhs, VBAR_VBAR, rhs) -> Or2(rw' lhs, rw' rhs)
| TUPLE3 (lhs, EQ_EQ, rhs) -> Equals(rw' lhs, rw' rhs)
| TUPLE3 (lhs, PLING_EQ, rhs) -> NotEq(rw' lhs, rw' rhs)
| TUPLE3 (lhs, LT_EQ, rhs) -> LtEq(rw' lhs, rw' rhs)
| TUPLE3 (lhs, GT_EQ, rhs) -> GtEq(rw' lhs, rw' rhs)
| TUPLE3 (lhs, LT_LT, rhs) -> Shiftl(rw' lhs, rw' rhs)
| TUPLE3 (lhs, GT_GT, rhs) -> Shiftr(rw' lhs, rw' rhs)
| TUPLE3 (lhs, GT_GT_GT, rhs) -> Shiftr3(rw' lhs, rw' rhs)
| TUPLE3 (lhs, LESS, rhs) -> Less(rw' lhs, rw' rhs)
| TUPLE3 (lhs, GREATER, rhs) -> Greater(rw' lhs, rw' rhs)
| TUPLE3 (lhs, CARET, rhs) -> Xor(rw' lhs, rw' rhs)
| TUPLE3 (lhs, CARET_TILDE, rhs) -> Xnor(rw' lhs, rw' rhs)
| TUPLE2 (TUPLE3 (lhs, AMPERSAND_EQ, rhs), SEMICOLON) -> Blocking(rw' lhs, And(rw' lhs, rw' rhs))
| TUPLE2 (TUPLE3 (lhs, VBAR_EQ, rhs), SEMICOLON) -> Blocking(rw' lhs, Or(rw' lhs, rw' rhs))
| TUPLE2 (TUPLE3 (lhs, PLUS_EQ, rhs), SEMICOLON) -> Blocking(rw' lhs, Add(rw' lhs, rw' rhs))
| TUPLE4 (TUPLE2 (Int, EMPTY_TOKEN), lhs, EQUALS, rhs) -> Blocking(rw' lhs, rw' rhs)
| TUPLE3 (LPAREN, exp, RPAREN) -> rw' exp
| TUPLE6 (LBRACE, repeat, LBRACE, TLIST lst, RBRACE, RBRACE) -> Repl(rw' repeat, List.rev_map rw' lst)
| TUPLE8 (EMPTY_TOKEN, If, LPAREN, cond, RPAREN, if_clause, Else, else_clause) ->
 Ifelse(rw' cond, rw' if_clause, rw' else_clause)
| TUPLE7 (If, LPAREN, cond, RPAREN, if_clause, Else, else_clause) ->
 Ifelse(rw' cond, rw' if_clause, rw' else_clause)
| TUPLE6 (EMPTY_TOKEN, If, LPAREN, cond, RPAREN, if_clause) ->
 Iff(rw' cond, rw' if_clause)
| TUPLE2 (Always, TUPLE2 (TUPLE4 (AT, LPAREN, sentry, RPAREN), stmts)) ->
 Sentry(rw' sentry, rw' stmts)
| TUPLE2 (Always, TUPLE2 (TUPLE2 (AT, STAR), TUPLE4(Begin, TLIST stmts, End, EMPTY_TOKEN))) ->
 AlwaysComb(List.rev_map rw' stmts)
| TUPLE2 (Always_comb, TUPLE4(Begin, TLIST stmts, End, EMPTY_TOKEN)) ->
 AlwaysComb(List.rev_map rw' stmts)
| TUPLE2 (Always_ff, TUPLE2 (TUPLE4 (AT, LPAREN, sentry, RPAREN), stmts)) -> Sentry(rw' sentry, rw' stmts)
| TUPLE3 (lhs, EQUALS, exp) -> Blocking (rw' lhs, rw' exp)
| TUPLE2 (TUPLE4 (lhs, EQUALS, EMPTY_TOKEN, exp), SEMICOLON) -> Blocking (rw' lhs, rw' exp)
| TUPLE5 (Assign, EMPTY_TOKEN, EMPTY_TOKEN,
      TLIST asgnlst,
      SEMICOLON) -> Asgnlst (List.rev_map rw' (asgnlst))
| TUPLE5 (LBRACK, hi, COLON, lo, RBRACK) -> Dim(rw' hi, rw' lo)
| TUPLE5 (Output, TUPLE3 (Reg, EMPTY_TOKEN, dimlst'),
      IDENTIFIER id, EMPTY_TOKEN, EMPTY_TOKEN) ->
let dimlst = match dimlst' with
    | TLIST dimlst -> List.rev_map rw' (dimlst)
    | EMPTY_TOKEN -> []
    | _ -> failwith "dimlst'" in
 DeclReg (dimlst, [id], [])
| TUPLE4 (Begin, EMPTY_TOKEN, End, EMPTY_TOKEN) -> BeginBlock []
| TUPLE4 (TUPLE3 (Begin, COLON, IDENTIFIER lbl), TLIST stmts, End, elbl) ->
 BeginBlock (match List.rev_map rw' stmts with Itmlst x :: [] -> x | oth -> oth)
| TUPLE4 (Begin, TLIST stmts, End, EMPTY_TOKEN) ->
 BeginBlock (match List.rev_map rw' stmts with Itmlst x :: [] -> x | oth -> oth)
| TUPLE6 (Begin, COLON, IDENTIFIER lbl, TLIST stmts, End, elbl) ->
 BeginBlock (match List.rev_map rw' stmts with Itmlst x :: [] -> x | oth -> oth)
| TUPLE3 (Begin, TLIST stmts, End) ->
 BeginBlock (match List.rev_map rw' stmts with Itmlst x :: [] -> x | oth -> oth)
| TLIST lst -> Itmlst(List.rev_map rw' lst)
| TUPLE5 (lhs, EMPTY_TOKEN, EMPTY_TOKEN, EQUALS, exp) -> Blocking (rw' lhs, rw' exp)
| TUPLE3 (TUPLE3 (Bit, EMPTY_TOKEN, EMPTY_TOKEN), TLIST lst, SEMICOLON) ->
   Bitlst(List.rev_map rw' lst)
| TUPLE3 (TUPLE2 ((Int|Integer), EMPTY_TOKEN), TLIST lst, SEMICOLON) ->
  DeclInt (List.rev_map (function
  	  | TUPLE3 (IDENTIFIER id, EMPTY_TOKEN, EMPTY_TOKEN) -> id
	  | oth -> failwith "Int[eger]") lst)
| TUPLE5 (DOT, IDENTIFIER port, LPAREN, conn, RPAREN) -> Dot(port,rw' conn)
| DOT_STAR -> Dot("*", Unknown "*")
| TUPLE5 (IDENTIFIER id, EMPTY_TOKEN, LPAREN, TLIST [TLIST lst], RPAREN) ->
  DeclIntf2 (id, List.rev_map rw' lst)
| TUPLE4 (IDENTIFIER id, EMPTY_TOKEN, TLIST lst, SEMICOLON) -> DeclIntf1 (id, List.rev_map rw' lst)
| TUPLE5 (IDENTIFIER id, EMPTY_TOKEN, LPAREN, TLIST lst, RPAREN) -> Parenth (id, List.rev_map rw' lst)
| TUPLE4 (IDENTIFIER id, TUPLE4 (HASH, LPAREN, TLIST lst, RPAREN), TLIST lst', SEMICOLON) ->
  Hash(id, List.rev_map rw' lst, List.rev_map rw' lst')
| TUPLE2 (TUPLE2 (Parameter, TUPLE2 (Int, Unsigned)),
            TUPLE4 (IDENTIFIER id, EMPTY_TOKEN, EMPTY_TOKEN,
              TUPLE2 (EQUALS, exp))) -> Param(id, rw' exp)
| TUPLE4 (IDENTIFIER id, EMPTY_TOKEN, EMPTY_TOKEN, TUPLE2 (EQUALS, exp)) -> Param(id, rw' exp)
| TUPLE2 (TUPLE2 (TUPLE2 (Localparam, args), TLIST lst), SEMICOLON) ->
  let args' = match args with TLIST lst -> List.rev_map rw' lst | EMPTY_TOKEN -> [] | _ -> [] in
  LocalP(args', List.rev_map rw' lst)
| TUPLE4 (IDENTIFIER id, LPAREN, TLIST lst, RPAREN) -> Parenth (id, List.rev_map rw' lst)
| TUPLE3 (Modport, TLIST lst, SEMICOLON) -> DeclModPort(List.rev_map rw' lst)
| TUPLE3 (TUPLE3 (Logic, EMPTY_TOKEN, EMPTY_TOKEN), TLIST lst, SEMICOLON) -> DeclLogic(List.rev_map rw' lst)
| TUPLE3 (LBRACE, TLIST lst, RBRACE) -> Concat (List.rev_map rw' lst)
| TUPLE7
     (TUPLE3 (Interface, EMPTY_TOKEN, IDENTIFIER id),
      TUPLE4 (HASH, LPAREN, TLIST lst, RPAREN),
      TUPLE3 (LPAREN, TLIST lst', RPAREN),
      SEMICOLON, TLIST lst'', Endinterface, EMPTY_TOKEN) ->
  DeclIntf(id, List.rev_map rw' lst, List.rev_map rw' lst', List.rev_map rw' lst'')
| TUPLE3 (TUPLE3 (Logic, EMPTY_TOKEN, TLIST lst), TLIST lst', SEMICOLON) ->
  Logic(List.rev_map rw' lst, List.rev_map rw' lst')
| TUPLE6 (Task, EMPTY_TOKEN, IDENTIFIER taskid, TUPLE2 (SEMICOLON, TLIST lst), arg5, arg6) ->
  DeclTask(taskid, List.rev_map rw' lst, rw' arg5, rw' arg6)
| EMPTY_TOKEN -> rw' (TLIST [])
| Endtask ->  rw' (TLIST [])
| TUPLE6 (TUPLE4 (IDENTIFIER memory, LBRACK, exp, RBRACK), LBRACK, hi, COLON, lo, RBRACK) ->
  Mem3(memory, rw' exp, rw' hi, rw' lo)
| TUPLE3 (IDENTIFIER memory, TLIST lst, EMPTY_TOKEN) -> Mem1(memory, List.rev_map rw' lst)
| TUPLE5 (arg1, QUERY, arg2, COLON, arg3) -> Query(rw' arg1, rw' arg2, rw' arg3)
| TUPLE2 (TUPLE2 (DLR_stop, EMPTY_TOKEN), SEMICOLON) -> Unknown "$stop"
| TUPLE2 (TUPLE2 (DLR_finish, EMPTY_TOKEN), SEMICOLON) -> Unknown "$finish"
| TUPLE2 (TUPLE4 (DLR_display, LPAREN, TLIST lst, RPAREN), SEMICOLON) -> Unknown "$display"
| TUPLE2 (TUPLE4 (DLR_write, LPAREN, TLIST lst, RPAREN), SEMICOLON) -> Unknown "$write"
| TUPLE2 (TUPLE4 (DLR_error, LPAREN, TLIST lst, RPAREN), SEMICOLON) -> Unknown "$error"
| TUPLE3 (Genvar, TLIST lst, SEMICOLON) ->
  DeclGenvar (List.map (function TUPLE2 (IDENTIFIER ix, EMPTY_TOKEN) -> ix | oth -> failwith "genvar") lst)
| TUPLE3 (TUPLE3 (Reg, EMPTY_TOKEN, dimlst'), TLIST idlst, SEMICOLON) ->
let dims = match dimlst' with TLIST dimlst -> List.rev_map rw' dimlst | EMPTY_TOKEN -> [] | _ -> [] in
let lft, rght = List.split (List.rev_map (function
       | TUPLE3 (IDENTIFIER id, EMPTY_TOKEN, EMPTY_TOKEN) -> id, []
       | TUPLE5 (IDENTIFIER id, EMPTY_TOKEN, EMPTY_TOKEN, EQUALS, exp) -> id, [rw' exp]
       | TUPLE3 (IDENTIFIER memory, TLIST lst, EMPTY_TOKEN) -> memory, List.rev_map rw' lst
       | oth -> missing := Some oth; failwith "DeclReg") idlst) in
DeclReg(dims,lft,rght)
| TUPLE3 (TUPLE5 (EMPTY_TOKEN, Wire, EMPTY_TOKEN, EMPTY_TOKEN, 
  TUPLE3 (EMPTY_TOKEN, TLIST lst, EMPTY_TOKEN)),
      TLIST lst', SEMICOLON) -> DeclWire (List.rev_map rw' lst, List.rev_map wire_map lst')
| TUPLE3 (TUPLE5 (EMPTY_TOKEN, Wire, EMPTY_TOKEN, EMPTY_TOKEN, EMPTY_TOKEN),
      TLIST lst, SEMICOLON) -> DeclWire ([], List.rev_map wire_map lst)
| TUPLE2 (Initial, TUPLE2(stmts, SEMICOLON)) -> Unknown "initial"
| TUPLE2 (Initial, TUPLE4(Begin, TLIST lst, End, EMPTY_TOKEN)) -> Unknown "initial"
| TUPLE8 (For, LPAREN, TUPLE2 (TLIST strtlst, SEMICOLON), stop, SEMICOLON, inc, RPAREN, stmts) -> 
ForLoop (List.rev_map rw' strtlst, rw' stop, rw' inc, rw' stmts)
| TUPLE9 (For, LPAREN, TUPLE4 (Genvar, TUPLE2 (IDENTIFIER ix, EMPTY_TOKEN), EQUALS,
        INTEGER_NUMBER strt), SEMICOLON, stop, SEMICOLON, inc, RPAREN, stmts) -> 
ForLoop (rw' (TUPLE3(IDENTIFIER ix, EQUALS, INTEGER_NUMBER strt)) :: [], rw' stop, rw' inc, rw' stmts)
| TUPLE9 (For, LPAREN, TUPLE3 (IDENTIFIER ix, EQUALS, INTEGER_NUMBER strt),
	  SEMICOLON, stop, SEMICOLON, inc, RPAREN, stmts) -> 
ForLoop (rw' (TUPLE3(IDENTIFIER ix, EQUALS, INTEGER_NUMBER strt)) :: [], rw' stop, rw' inc, rw' stmts)
| TUPLE4 (EMPTY_TOKEN, IDENTIFIER id, EMPTY_TOKEN, EMPTY_TOKEN) -> Id id
| TUPLE6 (lval, LBRACK, lhs, PLUS_COLON, rhs, RBRACK) ->
  PartSel(rw' lval, rw' lhs, rw' rhs)
| TUPLE5 (EMPTY_TOKEN, TUPLE4 (Case, LPAREN, slice, RPAREN),
      EMPTY_TOKEN,
      TLIST caselst,
      Endcase) ->
let lst' = ref [] in
let lstrf = ref [] in
List.iter (collapse_case lst' lstrf) (List.rev caselst);
 CaseStmt(rw' slice, List.rev !lst')
| TUPLE3 (Generate, TLIST genlst, Endgenerate) -> GenBlock(List.rev_map rw' genlst)
| TUPLE3 (TUPLE3 (EMPTY_TOKEN, TYPE_HYPHEN_IDENTIFIER id, EMPTY_TOKEN), TLIST lst, SEMICOLON) ->
  Typ(id, List.rev_map rw' lst)
| TUPLE6 (Typedef, TUPLE5 (Enum, TUPLE3 (Logic, EMPTY_TOKEN, TLIST lst),
        LBRACE,
        TLIST lst',
	RBRACE),
(TYPE_HYPHEN_IDENTIFIER id | IDENTIFIER id), EMPTY_TOKEN, EMPTY_TOKEN, SEMICOLON) -> TypEnum id
| oth -> missing := Some oth; failwith ("rw' fail: "^Source_text_types.getstr oth)

and wire_map = function
| TUPLE2 (IDENTIFIER id, EMPTY_TOKEN) -> Id id
| TUPLE4 (IDENTIFIER id, EMPTY_TOKEN, EQUALS, exp) -> WireExpr(id, rw' exp)
| oth -> missing := Some oth; failwith ("wire map fail: "^Source_text_types.getstr oth)

and collapse_case lst' lstrf = function
| COLON -> lst' := CaseItm !lstrf :: !lst'
| TLIST [INTEGER_NUMBER n] -> lstrf := [Number n]
| INTEGER_NUMBER n -> lstrf := [Number n]
| Default -> lstrf := [Number ""]
| oth -> lstrf := rw' oth :: !lstrf

open Printf
let unhand = ref None

let rec dump fd = function
  | Sentry (Pos clk, BeginBlock lst) ->
     fprintf fd "always @(posedge %s) begin\n" clk;
     List.iter (dump fd) lst
  | Unknown str -> fprintf fd "%s" str
  | In -> fprintf fd "input\n"
  | Out -> fprintf fd "output\n"
  | Itmlst(rw_lst) -> fprintf fd "Itmlst(rw_lst)\n"
  | Modul(str1, rw_lst2, rw_lst3) -> 
    fprintf fd "module %s(\n" str1;
    List.iter (dump fd) rw_lst2;
    List.iter (dump fd) rw_lst3;
  | Port (In, id, []) -> fprintf fd "input %s,\n" id
  | Id id -> fprintf fd "%s" id
  | Port (In, id, [Dim (Intgr hi, Intgr lo)]) -> fprintf fd "input [%d:%d] %s,\n" hi lo id
  | DeclReg ([Dim (Intgr hi, Intgr lo)], [id], []) -> fprintf fd "reg [%d:%d] %s,\n" hi lo id
  | DeclReg ([Dim (Intgr hi, Intgr lo)], [id], [[]]) -> fprintf fd "reg [%d:%d] %s,\n" hi lo id
  | DeclReg ([Dim (Intgr hi, Intgr lo)], [id], [[init]]) -> fprintf fd "reg [%d:%d] %s = " hi lo id;
    	    dump fd init
  | DeclReg ([], [id], [[]]) -> fprintf fd "reg %s;\n" id
  | DeclReg ([], [id], [[init]]) -> fprintf fd "reg %s = " id; dump fd init
  | DeclReg ([], idlst, initlst) -> List.iter (fun id -> fprintf fd "reg %s;\n" id) idlst
  | DeclReg(rw_lst, str1_lst, rw_lst_lst) as x -> unhand := Some x; fprintf fd "DeclReg\n"
  | NonBlocking(rw, rw2) -> dump fd rw; fprintf fd " <= "; dump fd rw2; fprintf fd ";\n"
  | Query(rw, rw2, rw3) -> dump fd rw; fprintf fd " ? "; dump fd rw2; fprintf fd ": "; dump fd rw3; fprintf fd "; "
  | Port(rw, str1, rw_lst) -> fprintf fd "Port %s\n" str1; dump fd (rw); dump_lst fd ";" (rw_lst)
  | Pos(str1) -> fprintf fd "posedge %s" (str1)
  | Neg(str1) -> fprintf fd "negedge %s" (str1)
  | Edge(rw, rw2) -> fprintf fd "Edge\n"; dump fd (rw); dump fd (rw2)
  | Intgr(int1) -> fprintf fd "%d" int1
  | Number(str1) -> fprintf fd "%s" (str1)
  | Sel(str1, rw2) -> fprintf fd "%s[" (str1); dump fd (rw2); fprintf fd "]"
  | Inc(rw) -> fprintf fd "("; dump fd (rw); fprintf fd ")++"
  | Dec(rw) -> fprintf fd "("; dump fd (rw); fprintf fd ")--"
  | RedAnd(rw) -> fprintf fd "&("; dump fd (rw); fprintf fd ")"
  | RedOr(rw) -> fprintf fd "|("; dump fd (rw); fprintf fd ")"
  | UMinus(rw) -> fprintf fd "-("; dump fd (rw); fprintf fd ")"
  | Pling(rw) -> fprintf fd "!("; dump fd (rw); fprintf fd ")"
  | Tilde(rw) -> fprintf fd "~("; dump fd (rw); fprintf fd ")"
  | Caret(rw) -> fprintf fd "^("; dump fd (rw); fprintf fd ")"
  | Bits(rw) -> fprintf fd "$bits("; dump fd (rw); fprintf fd ")"
  | Typ(s, rwlst) -> fprintf fd "(%s)" s
  | TypEnum(s) -> fprintf fd "Enum(%s)" s
  | Comma(rw, rw2, rw3) -> fprintf fd "("; dump fd (rw); fprintf fd ", "; dump fd (rw2); fprintf fd ", "; dump fd (rw3); fprintf fd ")"
  | Clog2(rw) -> fprintf fd "$clog2("; dump fd (rw); fprintf fd ")"
  | Equals(rw, rw2) -> fprintf fd "("; dump fd (rw); fprintf fd " == "; dump fd (rw2); fprintf fd ")";
  | NotEq(rw, rw2) -> fprintf fd "("; dump fd (rw); fprintf fd " != "; dump fd (rw2); fprintf fd ")"
  | LtEq(rw, rw2) -> fprintf fd "("; dump fd (rw); fprintf fd " <= "; dump fd (rw2); fprintf fd ")"
  | GtEq(rw, rw2) -> fprintf fd "("; dump fd (rw); fprintf fd " >= "; dump fd (rw2); fprintf fd ")"
  | Less(rw, rw2) -> fprintf fd "("; dump fd (rw); fprintf fd " < "; dump fd (rw2); fprintf fd ")"
  | Greater(rw, rw2) -> fprintf fd "("; dump fd (rw); fprintf fd " > "; dump fd (rw2); fprintf fd ")"
  | And(rw, rw2) -> fprintf fd "("; dump fd (rw); fprintf fd " & "; dump fd (rw2); fprintf fd ")"
  | And2(rw, rw2) -> fprintf fd "("; dump fd (rw); fprintf fd " && "; dump fd (rw2); fprintf fd ")"
  | Or(rw, rw2) -> fprintf fd "("; dump fd (rw); fprintf fd " | "; dump fd (rw2); fprintf fd ")"
  | Or2(rw, rw2) -> fprintf fd "("; dump fd (rw); fprintf fd " || "; dump fd (rw2); fprintf fd ")"
  | Xor(rw, rw2) -> fprintf fd "("; dump fd (rw); fprintf fd " ^ "; dump fd (rw2); fprintf fd ")"
  | Xnor(rw, rw2) -> fprintf fd "("; dump fd (rw); fprintf fd " ^~ "; dump fd (rw2); fprintf fd ")"
  | Shiftl(rw, rw2) -> fprintf fd "("; dump fd (rw); fprintf fd " << "; dump fd (rw2); fprintf fd ")"
  | Shiftr(rw, rw2) -> fprintf fd "("; dump fd (rw); fprintf fd " >> "; dump fd (rw2); fprintf fd ")"
  | Shiftr3(rw, rw2) -> fprintf fd "("; dump fd (rw); fprintf fd " >>> "; dump fd (rw2); fprintf fd ")"
  | Add(rw, rw2) -> fprintf fd "("; dump fd (rw); fprintf fd " + "; dump fd (rw2); fprintf fd ")"
  | Sub(rw, rw2) -> fprintf fd "("; dump fd (rw); fprintf fd " - "; dump fd (rw2); fprintf fd ")"
  | Mult(rw, rw2) -> fprintf fd "("; dump fd (rw); fprintf fd " * "; dump fd (rw2); fprintf fd ")"
  | Div(rw, rw2) -> fprintf fd "("; dump fd (rw); fprintf fd " / "; dump fd (rw2); fprintf fd ")"
  | Ifelse(rw, rw2, rw3) -> fprintf fd "if ("; dump fd (rw); fprintf fd ") begin\n"; dump fd (rw2); fprintf fd "\nend else begin\n"; dump fd (rw3); fprintf fd "\nend\n"
  | Iff(rw, rw2) -> fprintf fd "if ("; dump fd (rw); fprintf fd ") begin\n"; dump fd (rw2); fprintf fd "\nend\n"
  | ForLoop(rw_lst, rw2, rw3, rw4) -> fprintf fd "for (";
  dump_lst fd ";" (rw_lst); 
  fprintf fd "; ";
  dump fd (rw2);
  fprintf fd "; ";
  dump fd (rw3);
  fprintf fd ")";
  dump fd (rw4)
  | CaseStmt(rw, rw_lst) -> fprintf fd "case("; dump fd (rw); fprintf fd ")"; dump_lst fd ";" (rw_lst)
  | CaseItm(rw_lst) -> fprintf fd "CaseItm\n"; dump_lst fd ";" (rw_lst)
  | AlwaysComb(rw_lst) -> fprintf fd "always @*\n"; dump_lst fd ";" (rw_lst)
  | Sentry(rw, rw2) -> fprintf fd "Sentry\n"; dump fd (rw); dump fd (rw2)
  | Blocking(rw, rw2) -> dump fd (rw); fprintf fd " = "; dump fd (rw2)
  | Asgnlst(rw_lst) -> fprintf fd "Asgnlst\n"; dump_lst fd ";" (rw_lst)
  | DeclInt(str1_lst) -> fprintf fd "DeclInt\n"; dump_str_lst fd ";" (str1_lst)
  | DeclGenvar(str1_lst) -> fprintf fd "DeclGenvar\n"; dump_str_lst fd ";" (str1_lst)
  | Dim(rw, rw2) -> fprintf fd "Dim\n"; dump fd (rw); dump fd (rw2)
  | BeginBlock(rw_lst) -> fprintf fd "\nbegin\n"; dump_lst fd ";" (rw_lst); fprintf fd "\nend\n"
  | Bitlst(rw_lst) -> fprintf fd "Bitlst\n"; dump_lst fd ";" (rw_lst)
  | Dot(str1, rw2) -> fprintf fd "Dot %s\n" (str1); dump fd (rw2)
  | Unsigned(rw) -> fprintf fd "Unsigned\n"; dump fd (rw)
  | Signed(rw) -> fprintf fd "Signed\n"; dump fd (rw)
  | Concat(rw_lst) -> fprintf fd "{"; dump_lst fd "," (rw_lst); fprintf fd "}"
  | DeclWire(rw_lst, rw_lst2) -> fprintf fd "DeclWire\n"; dump_lst fd ";" (rw_lst); dump_lst fd ";" (rw_lst2)
  | WireExpr(str1, rw2) -> fprintf fd "WireExpr %s\n" (str1); dump fd (rw2)
  | DeclIntf1(str1, rw_lst) -> fprintf fd "DeclIntf1 %s\n" (str1); dump_lst fd ";" (rw_lst)
  | DeclIntf2(str1, rw_lst) -> fprintf fd "DeclIntf2 %s\n" (str1); dump_lst fd ";" (rw_lst)
  | Hash(str1, rw_lst, rw_lst2) -> fprintf fd "Hash %s\n" (str1); dump_lst fd ";" (rw_lst); dump_lst fd ";" (rw_lst2)
  | DeclIntf(str1, rw_lst, rw_lst2, rw_lst3) -> fprintf fd "DeclIntf %s\n" (str1); dump_lst fd ";" (rw_lst); dump_lst fd ";" (rw_lst2); dump_lst fd ";" (rw_lst3)
  | DeclModPort(rw_lst) -> fprintf fd "DeclModPort\n"; dump_lst fd ";" (rw_lst)
  | Repl(rw, rw_lst) -> fprintf fd "Repl\n"; dump fd (rw); dump_lst fd ";" (rw_lst)
  | Slice(str1, rw2, rw3) -> fprintf fd "%s[" (str1); dump fd (rw2); fprintf fd ":"; dump fd (rw3); fprintf fd "]"
  | Field(rw, rw2) -> fprintf fd "Field\n"; dump fd (rw); dump fd (rw2)
  | Dot3(str1, str2, str3) -> fprintf fd "Dot3 %s,%s,%s\n" (str1) (str2) (str3)
  | Parenth(str1, rw_lst) -> fprintf fd "Parenth %s\n" (str1); dump_lst fd ";" (rw_lst)
  | Logic(rw_lst, rw_lst2) -> fprintf fd "Logic\n"; dump_lst fd ";" (rw_lst); dump_lst fd ";" (rw_lst2)
  | Param(str1, rw2) -> fprintf fd "Param %s\n" (str1); dump fd (rw2)
  | LocalP(rw_lst, rw_lst2) -> fprintf fd "LocalP\n"; dump_lst fd ";" (rw_lst); dump_lst fd ";" (rw_lst2)
  | DeclLogic(rw_lst) -> fprintf fd "DeclLogic\n"; dump_lst fd ";" (rw_lst)
  | DeclTask(str1, rw_lst2, rw3, rw4) -> fprintf fd "DeclTask %s\n" (str1); dump_lst fd ";" (rw_lst2); dump fd (rw3); dump fd (rw4)
  | Mem1(str1, rw_lst) -> fprintf fd "Mem1 %s\n" (str1); dump_lst fd ";" (rw_lst)
  | Mem3(str1, rw2, rw3, rw4) -> fprintf fd "Mem3 %s\n" (str1); dump fd (rw2); dump fd (rw3); dump fd (rw4)
  | PartSel(rw, rw2, rw3) -> fprintf fd "PartSel "; dump fd (rw); dump fd (rw2); dump fd (rw3)
  | GenBlock(rw_lst) -> fprintf fd "GenBlock\n"; dump_lst fd ";" (rw_lst)

and dump_lst fd sep rw = let delim = ref "" in
    List.iter (fun itm -> fprintf fd "%s" !delim; dump fd itm; delim := sep) rw
and dump_str_lst fd sep lst = fprintf fd "%s" (String.concat sep lst)

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

let rewrite v =
  let p = parse v in
  let p' = rw p in
  let x = rw' p' in
  let modlst = ref [] in
  Hashtbl.iter (fun k x ->
		modlst := k :: !modlst;
		let fd = open_out (k^"_dump.v") in dump fd x;
		close_out fd) modules;
  let modlst = !modlst in
  modlst, x

