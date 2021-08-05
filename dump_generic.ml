open Source_text_rewrite_types
open Source_text_lex
open Source_text
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
  | Sel(rw, rw2) -> dump fd (rw); fprintf fd "["; dump fd (rw2); fprintf fd "]"
  | Inc(rw) -> fprintf fd "("; dump fd (rw); fprintf fd ")++"
  | Dec(rw) -> fprintf fd "("; dump fd (rw); fprintf fd ")--"
  | RedAnd(rw) -> fprintf fd "&("; dump fd (rw); fprintf fd ")"
  | RedOr(rw) -> fprintf fd "|("; dump fd (rw); fprintf fd ")"
  | UMinus(rw) -> fprintf fd "-("; dump fd (rw); fprintf fd ")"
  | Pling(rw) -> fprintf fd "!("; dump fd (rw); fprintf fd ")"
  | Tilde(rw) -> fprintf fd "~("; dump fd (rw); fprintf fd ")"
  | Caret(rw) -> fprintf fd "^("; dump fd (rw); fprintf fd ")"
  | Bits(rw) -> fprintf fd "$bits("; dump fd (rw); fprintf fd ")"
  | Typ(s, rwlst, rwlst') -> fprintf fd "(%s)" s
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
  | StarStar(rw, rw2) -> fprintf fd "("; dump fd (rw); fprintf fd " ** "; dump fd (rw2); fprintf fd ")"
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
  | Mem3(rw, rw2, rw3, rw4) -> fprintf fd "Mem3 "; dump fd (rw); dump fd (rw2); dump fd (rw3); dump fd (rw4)
  | PartSel(rw, rw2, rw3) -> fprintf fd "PartSel "; dump fd (rw); dump fd (rw2); dump fd (rw3)
  | GenBlock(rw_lst) -> fprintf fd "GenBlock\n"; dump_lst fd ";" (rw_lst)
  | Cast(rw, rw2) -> fprintf fd "Cast "; dump fd (rw); dump fd (rw2)
  | TildeAnd _ -> fprintf fd "~&"
  | TildeOr _ -> fprintf fd "~|"
  | Struct (id, _) -> fprintf fd "struct %s" id
  | Package (id, _) -> fprintf fd "package %s" id
  | DepLst(rw_lst) -> fprintf fd "DepLst\n"; dump_str_lst fd ";" (rw_lst)

and dump_lst fd sep rw = let delim = ref "" in
    List.iter (fun itm -> fprintf fd "%s" !delim; dump fd itm; delim := sep) rw
and dump_str_lst fd sep lst = fprintf fd "%s" (String.concat sep lst)
