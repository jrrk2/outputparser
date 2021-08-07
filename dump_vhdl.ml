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
  | Number(int1,int2,int3,str1) -> fprintf fd "(%d,%d,%d,%s)" int1 int2 int3 str1
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
  | Deflt -> fprintf fd "default"
  
and dump_lst fd sep rw = let delim = ref "" in
    List.iter (fun itm -> fprintf fd "%s" !delim; dump fd itm; delim := sep) rw
and dump_str_lst fd sep lst = fprintf fd "%s" (String.concat sep lst)

let rec dump fd = function
| TypEnum _ -> ()
| DeclReg _ -> ()
| oth -> unhand := Some oth; failwith "dump"

let rec obin w n = 
  (if w > 0 then obin (w-1) (n lsr 2) else "")^string_of_int (n mod 2)

let rec vexpr = function
| Id s -> s
| Number (2,1,n,_) -> "'" ^ string_of_int n ^ "'"
| Number (2,w,n,_) -> "\"" ^ (obin w n) ^ "\""
| Number (b,w,n,s) -> string_of_int n
| Intgr n -> string_of_int n
| Tilde expr -> "~" ^ vexpr expr
| Concat lst -> String.concat " & " (List.map vexpr lst)
| Sel (Id id, rhs) -> id ^ "( " ^ vexpr rhs ^ "  s)"
| Slice (id, hi, lo) -> id ^ "( " ^ vexpr hi ^ " downto " ^ vexpr lo ^ " )"
| Add (lhs, rhs) -> vexpr lhs ^ "-" ^ vexpr rhs
| Sub (lhs, rhs) -> vexpr lhs ^ "-" ^ vexpr rhs
| Equals (lhs, rhs) -> vexpr lhs ^ "=" ^ vexpr rhs
| NotEq (lhs, rhs) -> vexpr lhs ^ "!=" ^ vexpr rhs
| GtEq (lhs, rhs) -> vexpr lhs ^ ">=" ^ vexpr rhs
| Or (lhs, rhs) -> vexpr lhs ^ " or " ^ vexpr rhs
| Xor (lhs, rhs) -> vexpr lhs ^ " xor " ^ vexpr rhs
| And (lhs, rhs) -> vexpr lhs ^ " and " ^ vexpr rhs
| And2 (lhs, rhs) -> vexpr lhs ^ " && " ^ vexpr rhs
| Clog2 expr -> "Clog2("^vexpr expr^")"
| Unsigned expr -> "unsigned("^vexpr expr^")"
| Shiftl (lhs, rhs) -> "shift_left("^vexpr lhs^", "^vexpr rhs^")"
| Dot (port, conn) -> port ^ " => " ^ (vexpr conn)
| Unknown u -> "unknown: " ^ u
| oth -> unhand := Some oth; failwith "vexpr"

let rec simplify = function
| Add (Intgr lhs, Intgr rhs) -> Intgr (lhs + rhs)
| Sub (Intgr lhs, Intgr rhs) -> Intgr (lhs - rhs)
| And (Intgr lhs, Intgr rhs) -> Intgr (lhs land rhs)
| Or (Intgr lhs, Intgr rhs) -> Intgr (lhs lor rhs)
| Xor (Intgr lhs, Intgr rhs) -> Intgr (lhs lxor rhs)
| Shiftl (Intgr lhs, Intgr rhs) -> Intgr (lhs lsl rhs)
| Add (lhs, rhs) -> Add (simplify lhs, simplify rhs)
| Sub (lhs, rhs) -> Sub (simplify lhs, simplify rhs)
| And (lhs, rhs) -> And (simplify lhs, simplify rhs)
| Or (lhs, rhs) -> Or (simplify lhs, simplify rhs)
| Xor (lhs, rhs) -> Xor (simplify lhs, simplify rhs)
| Shiftl (lhs, rhs) -> Shiftl (simplify lhs, simplify rhs)
| oth -> oth

let simplify x = 
  let rslt1 = ref (simplify (simplify (simplify (simplify x)))) in
  let rslt2 = ref (simplify (simplify (simplify (simplify !rslt1)))) in
  while !rslt1 <> !rslt2 do
    rslt1 := simplify (simplify (simplify (simplify !rslt2)));
    rslt2 := simplify (simplify (simplify (simplify !rslt1)));
  done;
  !rslt2

let vexpr x = vexpr (simplify x)

let asgn fd expr = function
| Id lhs -> fprintf fd "            %s <= %s;\n" lhs (vexpr expr)
| Sel (Id id, rhs) -> fprintf fd "            %s(%s) <= %s;\n" id (vexpr rhs) (vexpr expr)
| Slice (id, hi, lo) -> fprintf fd "            %s( %s downto %s ) <= %s;\n" id (vexpr hi) (vexpr lo) (vexpr expr)
| oth -> unhand := Some oth; failwith "asgn"

let vdir = function
  | In -> "in "
  | Out -> "out"
  | _ -> failwith "vdir"

let ports = function
    | Port((In|Out) as dir,  nam, []) ->
        sprintf "%24s         : %s std_logic" nam (vdir dir);
    | Port ((In|Out) as dir, nam, [Dim (hi, Intgr 0)]) ->
        sprintf "%24s         : %s std_logic_vector(%s downto 0)" nam (vdir dir) (vexpr hi);
    | oth -> unhand := Some oth; failwith "component"

let decl_template fd modules complst = function
    | DeclReg ([Dim (Intgr hi, Intgr lo)], [nam], [[]]) ->
    fprintf fd "    signal %s : std_logic_vector(%d downto %d);\n" nam hi lo
    | DeclReg ([], [nam], [[]]) ->
    fprintf fd "    signal %s : std_logic;\n" nam
    | DeclReg ([Dim (hi, Intgr lo)], [nam], [[]]) ->
    fprintf fd "    signal %s : std_logic_vector(%s downto %d);\n" nam (vexpr hi) lo
    | DeclIntf1 (typ, _) -> if not (List.mem typ !complst) then
    begin
    complst := typ :: !complst;
    match Hashtbl.find_opt modules typ with Some (Modul(_, port_lst, _)) ->
  fprintf fd "    -- Component %s description\n" typ;
  fprintf fd "    component %s is\n" typ;
  fprintf fd "    port (\n%s\n    );\n" (String.concat ";\n" (List.map ports port_lst));
  fprintf fd "    end component;\n";
	| None -> fprintf fd "-- %s is not a module\n" typ;
        | Some oth -> unhand := Some oth; failwith ("DeclIntf: "^typ)
    end
    | CaseStmt _ -> ()
    | Asgnlst _ -> ()
    | Sentry _ -> ()
    | TypEnum _ -> ()
    | Hash _ -> ()
    | Typ (typ, [], typ_lst) -> ()
    | oth -> unhand := Some oth; failwith "decl_template"

let rec stmt_clause fd = function
      | Itmlst lst -> List.iter (stmt_clause fd) lst      
      | BeginBlock lst -> List.iter (stmt_clause fd) lst      
      | Ifelse (condition, if_lst, else_lst) ->
  fprintf fd "        if (%s) then\n" (vexpr condition);
    (match if_lst with BeginBlock if_lst -> List.iter (stmt_clause fd) if_lst | _ -> stmt_clause fd if_lst);       
  fprintf fd "        else\n";
    (match else_lst with BeginBlock else_lst -> List.iter (stmt_clause fd) else_lst | _ -> stmt_clause fd else_lst);
      | Blocking (lhs, expr) -> asgn fd expr lhs
      | NonBlocking (lhs, expr) -> asgn fd expr lhs
      | Iff _ as x -> iff_template fd x
      | DeclLogic lst -> ()
      | CaseStmt (Id state, imtlst) -> ()
      | Unknown ";" -> ()
      | oth -> unhand := Some oth; failwith "stmt_clause"
      
and iff_template fd = function
    | Source_text_rewrite_types.Iff(condition, if_lst) ->
  fprintf fd "        if (%s) then\n" (vexpr condition);
    stmt_clause fd if_lst;       
    | oth -> unhand := Some oth; failwith "iff_template"

let rec sent_template fd = function
    | BeginBlock lst -> List.iter (sent_template fd) lst
    | Ifelse (Equals (Id rst, lev), BeginBlock if_lst, BeginBlock else_lst) ->
  fprintf fd "        if (%s = %s) then\n" rst (vexpr lev);
    List.iter (stmt_clause fd) if_lst;       
  fprintf fd "        elsif (CLK'event and CLK = '1') then\n";
    List.iter (stmt_clause fd) else_lst;       
    | oth -> stmt_clause fd oth

let proc_template fd cnt = function
    | DeclReg _ -> ()
    | Sentry (Edge (Pos clk, Pos rst), sent_lst) ->
  fprintf fd "    -- clocked process %d description goes here\n" !cnt;
  fprintf fd "    SEQ%d: process (%s, %s)\n" !cnt clk rst;
  incr cnt;
  fprintf fd "    begin\n";
  sent_template fd sent_lst;       
  fprintf fd "    end process;\n";
  fprintf fd "\n";
    | Sentry (DepLst dep_lst, sent_lst) ->
  fprintf fd "    -- combinational process %d description goes here\n" !cnt;
  fprintf fd "    COMB%d: process (%s)\n" !cnt (String.concat ", " dep_lst);
  incr cnt;
  fprintf fd "    begin\n";
  stmt_clause fd sent_lst;       
  fprintf fd "    end process;\n";
  fprintf fd "\n";
    | CaseStmt _ -> ()
    | Asgnlst _ -> ()
    | Iff _ -> ()
    | Hash _ -> ()
    | TypEnum _ -> ()
    | Typ (typ, [], typ_lst) -> ()
    | DeclIntf1 (typ, lst) -> List.iter (function
        | DeclIntf2 (inst, pinlst) ->
	fprintf fd "%s: %s port map (\n\t%s);\n" inst typ (String.concat ",\n\t" (List.map vexpr pinlst))
	| Id id -> fprintf fd "--%s\n" id
	| oth -> unhand := Some oth; failwith "DeclIntf"
	) lst;
    | oth -> unhand := Some oth; failwith "proc_template"

let template fd modules = function Modul(entnam, port_lst, body_lst) -> let cnt = ref 1 in
  fprintf fd "--\n";
  fprintf fd "-- This converter does not currently preserve comments and license information\n";
  fprintf fd "--\n";
  fprintf fd "\n";
  fprintf fd "LIBRARY IEEE;\n";
  fprintf fd "USE IEEE.std_logic_1164.all;\n";
  fprintf fd "USE IEEE.numeric_std.all;\n";
  fprintf fd "\n";
  fprintf fd "-- entity description goes here\n";
  fprintf fd "entity %s is\n" entnam;
  fprintf fd "    port (\n%s\n    );\n" (String.concat ";\n" (List.map ports port_lst));
  fprintf fd "end %s;\n" entnam;
  fprintf fd "\n";
  fprintf fd "architecture rtl of %s is\n" entnam;
  fprintf fd "    -- Signals\n";
  let complst = ref [] in
  List.iter (decl_template fd modules complst) (List.sort compare (List.filter (function DeclIntf1 _ -> true | _ -> false) body_lst));
  List.iter (decl_template fd modules complst) (List.filter (function DeclIntf1 _ -> false | _ -> true) body_lst);
  fprintf fd "begin\n";
List.iter (proc_template fd cnt) body_lst;
  fprintf fd "\n";
  fprintf fd "end rtl;\n";
  fprintf fd "\n";
  fprintf fd "\n";
  | oth -> failwith "This template only handles modules"
