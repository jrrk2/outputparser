open Source_text_rewrite_types
open Source_text_lex
open Source_text
open Printf

let unhand = ref None

type vhdl_attr = {fn: vhdl_attr -> rw -> rw}

let rec recurs' (vhdl_attr:vhdl_attr) = function
  | Id id -> Id id
  | Expression rw -> Expression(recurs_itm vhdl_attr  rw)
  | Itmlst(rw_lst) -> Itmlst(recurs_lst vhdl_attr  (rw_lst:rw list))
  | Sentry (Pos clk, lst) -> Sentry(Pos clk, recurs_itm vhdl_attr  lst)
  | Unknown str -> Unknown str
  | In -> In
  | Out -> Out
  | Modul(str1, rw_lst2, rw_lst3) -> Modul(str1, recurs_lst vhdl_attr  rw_lst2, recurs_lst vhdl_attr  rw_lst3)
  | DeclReg(rw_lst, str1_lst, rw_lst_lst) -> DeclReg(recurs_lst vhdl_attr  rw_lst, str1_lst,
    List.map (fun itm -> recurs_lst vhdl_attr  itm) rw_lst_lst)
  | NonBlocking(rw, rw2) -> NonBlocking(recurs_itm vhdl_attr  rw, recurs_itm vhdl_attr  rw2)
  | Query(rw, rw2, rw3) -> Query(recurs_itm vhdl_attr  rw, recurs_itm vhdl_attr  rw2, recurs_itm vhdl_attr  rw3)
  | Port(rw, str1, rw_lst) -> Port(recurs_itm vhdl_attr  rw, str1, recurs_lst vhdl_attr  rw_lst)
  | Pos(str1) -> Pos (str1)
  | Neg(str1) -> Neg (str1)
  | Edge(rw, rw2) -> Edge(recurs_itm vhdl_attr  (rw), recurs_itm vhdl_attr  (rw2))
  | Intgr(int1) -> Intgr int1
  | Number _ as n -> n
  | Sel(str1, rw2) -> Sel(str1, recurs_itm vhdl_attr  rw2)
  | Inc(rw) -> Inc(recurs_itm vhdl_attr  rw)
  | Dec(rw) -> Dec(recurs_itm vhdl_attr  rw)
  | RedAnd(rw) -> RedAnd(recurs_itm vhdl_attr  (rw))
  | RedOr(rw) -> RedOr(recurs_itm vhdl_attr  (rw))
  | UMinus(rw) -> UMinus(recurs_itm vhdl_attr  (rw))
  | Pling(rw) -> Pling(recurs_itm vhdl_attr  (rw))
  | Tilde(rw) -> Tilde(recurs_itm vhdl_attr  (rw))
  | TildeAnd(rw) -> TildeAnd(recurs_itm vhdl_attr  (rw))
  | TildeOr(rw) -> TildeOr(recurs_itm vhdl_attr  (rw))
  | Equals(rw, rw2) -> Equals(recurs_itm vhdl_attr  (rw), recurs_itm vhdl_attr  (rw2))
  | NotEq(rw, rw2) -> NotEq(recurs_itm vhdl_attr  (rw), recurs_itm vhdl_attr  (rw2))
  | LtEq(rw, rw2) -> LtEq(recurs_itm vhdl_attr  (rw), recurs_itm vhdl_attr  (rw2))
  | GtEq(rw, rw2) -> GtEq(recurs_itm vhdl_attr  (rw), recurs_itm vhdl_attr  (rw2))
  | Less(rw, rw2) -> Less(recurs_itm vhdl_attr  (rw), recurs_itm vhdl_attr  (rw2))
  | Greater(rw, rw2) -> Greater(recurs_itm vhdl_attr  (rw), recurs_itm vhdl_attr  (rw2))
  | And(rw, rw2) -> And(recurs_itm vhdl_attr  (rw), recurs_itm vhdl_attr  (rw2))
  | And2(rw, rw2) -> And2(recurs_itm vhdl_attr  (rw), recurs_itm vhdl_attr  (rw2))
  | Or(rw, rw2) -> Or(recurs_itm vhdl_attr  (rw), recurs_itm vhdl_attr  (rw2))
  | Or2(rw, rw2) -> Or2(recurs_itm vhdl_attr  (rw), recurs_itm vhdl_attr  (rw2))
  | Xor(rw, rw2) -> Xor(recurs_itm vhdl_attr  (rw), recurs_itm vhdl_attr  (rw2))
  | Xnor(rw, rw2) -> Xnor(recurs_itm vhdl_attr  (rw), recurs_itm vhdl_attr  (rw2))
  | Shiftl(rw, rw2) -> Shiftl(recurs_itm vhdl_attr  (rw), recurs_itm vhdl_attr  (rw2))
  | Shiftr(rw, rw2) -> Shiftr(recurs_itm vhdl_attr  (rw), recurs_itm vhdl_attr  (rw2))
  | Shiftr3(rw, rw2) -> Shiftr3(recurs_itm vhdl_attr  (rw), recurs_itm vhdl_attr  (rw2))
  | Add(rw, rw2) -> Add(recurs_itm vhdl_attr  (rw), recurs_itm vhdl_attr  (rw2))
  | Sub(rw, rw2) -> Sub(recurs_itm vhdl_attr  (rw), recurs_itm vhdl_attr  (rw2))
  | Mult(rw, rw2) -> Mult(recurs_itm vhdl_attr  (rw), recurs_itm vhdl_attr  (rw2))
  | Div(rw, rw2) -> Div(recurs_itm vhdl_attr  (rw), recurs_itm vhdl_attr  (rw2))
  | StarStar(rw, rw2) -> StarStar(recurs_itm vhdl_attr  (rw), recurs_itm vhdl_attr  (rw2))
  | Ifelse(rw, rw2, rw3) -> Ifelse(recurs_itm vhdl_attr  (rw), recurs_itm vhdl_attr  (rw2), recurs_itm vhdl_attr  (rw3))
  | Iff(rw, rw2) -> Iff(recurs_itm vhdl_attr  (rw), recurs_itm vhdl_attr  (rw2))
  | ForLoop(rw_lst, rw2, rw3, rw4) ->
    ForLoop(recurs_lst vhdl_attr  (rw_lst), recurs_itm vhdl_attr  (rw2), recurs_itm vhdl_attr  (rw3), recurs_itm vhdl_attr  (rw4))
  | CaseStmt(rw, rw_lst) -> CaseStmt(recurs_itm vhdl_attr  (rw), recurs_lst vhdl_attr  (rw_lst))
  | CaseItm(rw_lst) -> CaseItm(recurs_lst vhdl_attr  (rw_lst))
  | AlwaysComb(rw_lst) -> AlwaysComb(recurs_lst vhdl_attr  (rw_lst))
  | Sentry(rw, rw2) -> Sentry(recurs_itm vhdl_attr  (rw), recurs_itm vhdl_attr  (rw2))
  | Blocking(rw, rw2) -> Blocking(recurs_itm vhdl_attr  (rw), recurs_itm vhdl_attr  (rw2))
  | Asgnlst(rw_lst) -> Asgnlst(recurs_lst vhdl_attr  (rw_lst))
  | DeclInt(str1_lst) -> DeclInt((str1_lst))
  | Dim(rw, rw2) -> Dim(recurs_itm vhdl_attr  (rw), recurs_itm vhdl_attr  (rw2))
  | BeginBlock(rw_lst) -> BeginBlock(recurs_lst vhdl_attr  (rw_lst))
  | Bitlst(rw_lst) -> Bitlst(recurs_lst vhdl_attr  (rw_lst))
  | Dot(str1, rw2) -> Dot(str1, recurs_itm vhdl_attr  (rw2))
  | Unsigned(rw) -> Unsigned(recurs_itm vhdl_attr  (rw))
  | Signed(rw) -> Signed(recurs_itm vhdl_attr  (rw))
  | Concat(rw_lst) -> Concat(recurs_lst vhdl_attr  (rw_lst))
  | DeclWire(rw_lst, rw_lst2) -> DeclWire(recurs_lst vhdl_attr  (rw_lst), recurs_lst vhdl_attr  (rw_lst2))
  | WireExpr(str1, rw2) -> WireExpr(str1, recurs_itm vhdl_attr  (rw2))
  | DeclIntf1(str1, rw_lst) -> DeclIntf1(str1, recurs_lst vhdl_attr  (rw_lst))
  | DeclIntf2(str1, rw_lst) -> DeclIntf2(str1, recurs_lst vhdl_attr  (rw_lst))
  | Hash(str1, rw_lst, rw_lst2) -> Hash(str1, recurs_lst vhdl_attr  (rw_lst), recurs_lst vhdl_attr  (rw_lst2))
  | DeclIntf(str1, rw_lst, rw_lst2, rw_lst3) -> DeclIntf(str1, recurs_lst vhdl_attr  (rw_lst), recurs_lst vhdl_attr  (rw_lst2), recurs_lst vhdl_attr  (rw_lst3))
  | DeclModPort(rw_lst) -> DeclModPort(recurs_lst vhdl_attr  (rw_lst))
  | Repl(rw, rw_lst) -> Repl(recurs_itm vhdl_attr  (rw), recurs_lst vhdl_attr  (rw_lst))
  | Slice(str1, rw2, rw3) -> Slice(str1, recurs_itm vhdl_attr  (rw2), recurs_itm vhdl_attr  (rw3))
  | Field(rw, rw2) -> Field(recurs_itm vhdl_attr  (rw), recurs_itm vhdl_attr  (rw2))
  | Dot3(str1, str2, str3) -> Dot3(str1, str2, str3)
  | Parenth(str1, rw_lst) -> Parenth(str1, recurs_lst vhdl_attr  (rw_lst))
  | Logic(rw_lst, rw_lst2) -> Logic(recurs_lst vhdl_attr  (rw_lst), recurs_lst vhdl_attr  (rw_lst2))
  | Param(str1, rw2) -> Param(str1, recurs_itm vhdl_attr  (rw2))
  | LocalP(rw_lst, rw_lst2) -> LocalP(recurs_lst vhdl_attr  (rw_lst), recurs_lst vhdl_attr  (rw_lst2))
  | DeclLogic(rw_lst) -> DeclLogic(recurs_lst vhdl_attr  (rw_lst))
  | DeclTask(str1, rw_lst2, rw3, rw4) -> DeclTask(str1, recurs_lst vhdl_attr  (rw_lst2), recurs_itm vhdl_attr  (rw3), recurs_itm vhdl_attr  (rw4))
  | Mem1(str1, rw_lst) -> Mem1(str1, recurs_lst vhdl_attr  (rw_lst))
  | Mem3(str1, rw2, rw3, rw4) -> Mem3(str1, recurs_itm vhdl_attr  (rw2), recurs_itm vhdl_attr  (rw3), recurs_itm vhdl_attr  (rw4))
  | PartSel(str1, rw2, rw3) -> PartSel(str1, recurs_itm vhdl_attr  (rw2), recurs_itm vhdl_attr  (rw3))
  | GenBlock(rw_lst) -> GenBlock(recurs_lst vhdl_attr  (rw_lst))
  | Package (id, rw_lst) -> Package(id, recurs_lst vhdl_attr  rw_lst)
  | Caret _ as x -> x
  | Bits _ as x -> x
  | Typ (_, _, _) as x -> x
  | Struct (_, _) as x -> x
  | TypEnum _ as x -> x
  | Comma (_, _, _) as x -> x
  | Clog2 _ as x -> x
  | DeclGenvar _ as x -> x
  | Cast (_, _) as x -> x
  | DepLst lst as x -> x
  | Deflt -> Deflt

and recurs_lst vhdl_attr x = List.map (vhdl_attr.fn vhdl_attr ) x

and recurs_itm vhdl_attr  x = vhdl_attr .fn vhdl_attr  x

let rec recurs (vhdl_attr:vhdl_attr) = function
  | Ifelse(Expression rw, rw2, rw3) -> Ifelse(recurs_itm vhdl_attr (rw), recurs_itm vhdl_attr (rw2), recurs_itm vhdl_attr (rw3))
  | Or (Expression (Or _ as x), expr) -> Or (recurs_itm vhdl_attr x, recurs_itm vhdl_attr expr)
  | Or (expr, Expression (Or _ as x)) -> Or (recurs_itm vhdl_attr expr, recurs_itm vhdl_attr x)
  | And2 (Expression (And2 _ as x), expr) -> And2 (recurs_itm vhdl_attr x, recurs_itm vhdl_attr expr)
  | And2 (expr, Expression (And2 _ as x)) -> And2 (recurs_itm vhdl_attr expr, recurs_itm vhdl_attr x)
  | Or (expr, (And _ as x)) -> Or (recurs_itm vhdl_attr expr, Expression (recurs_itm vhdl_attr x))
  | Or (And2 _ as x, expr) -> Or (Expression (recurs_itm vhdl_attr x), recurs_itm vhdl_attr expr)
  | oth -> recurs' {fn=recurs} oth

let rec obin w n = 
  (if w > 1 then obin (w-1) (n lsr 1) else "")^string_of_int (n land 1)

let vlst = ref []

let rec vexpr = function
| Id s -> s
| Expression x -> " ( " ^ vexpr x ^ " ) "
| Number (2,1,n,_) -> "'" ^ string_of_int n ^ "'"
| Number (2,w,n,_) -> "\"" ^ (obin w n) ^ "\""
| Number (b,w,n,s) -> string_of_int n
| Intgr n -> string_of_int n
| Tilde expr -> "~" ^ vexpr expr
| Concat lst -> String.concat " & " (List.map vexpr lst)
| Sel (Id id, rhs) -> id ^ "( " ^ vexpr rhs ^ " )"
| Slice (id, hi, lo) -> id ^ "( " ^ vexpr hi ^ " downto " ^ vexpr lo ^ " )"
| Add (lhs, rhs) -> vexpr lhs ^ "+" ^ vexpr rhs
| Sub (lhs, rhs) -> vexpr lhs ^ "-" ^ vexpr rhs
| Equals (lhs, rhs) -> vexpr lhs ^ " = " ^ vexpr rhs
| NotEq (lhs, rhs) -> vexpr lhs ^ " != " ^ vexpr rhs
| GtEq (lhs, rhs) -> vexpr lhs ^ " >= " ^ vexpr rhs
| Or (lhs, rhs) -> vexpr lhs ^ " or " ^ vexpr rhs
| Xor (lhs, rhs) -> vexpr lhs ^ " xor " ^ vexpr rhs
| And (lhs, rhs) -> vexpr lhs ^ " and " ^ vexpr rhs
| And2 (lhs, rhs) -> vexpr lhs ^ " and " ^ vexpr rhs
| Clog2 expr -> "Clog2("^vexpr expr^")"
| Unsigned expr -> "unsigned("^vexpr expr^")"
| Shiftl (lhs, rhs) -> "shift_left("^vexpr lhs^", "^vexpr rhs^")"
| Dot (port, conn) -> port ^ " => " ^ (vexpr conn)
| Query (cond', ctrue', cfalse') -> sprintf "%s when %s else %s" (vexpr ctrue') (vexpr cond') (vexpr cfalse')
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
| Expression (Shiftl (lhs, rhs)) -> Expression (Shiftl (simplify lhs, simplify rhs))
| oth -> oth

let simplify x = 
  let rslt1 = ref (simplify (simplify (simplify (simplify (match x with Expression x -> x | _ -> x))))) in
  let rslt2 = ref (simplify (simplify (simplify (simplify !rslt1)))) in
  while !rslt1 <> !rslt2 do
    rslt1 := simplify (simplify (simplify (simplify !rslt2)));
    rslt2 := simplify (simplify (simplify (simplify !rslt1)));
  done;
  !rslt2

let vexpr x = let x' = simplify x in let s = vexpr x' in vlst := (x,x',s) :: !vlst; s

let initexpr x = match simplify x with Intgr 0 -> "(others => '0')" | oth -> vexpr oth

let asgn fd expr = function
| Id lhs -> fprintf fd "            %s <= %s;\n" lhs (initexpr expr)
| Sel (Id id, rhs) -> fprintf fd "            %s(%s) <= %s;\n" id (vexpr rhs) (initexpr expr)
| Slice (id, hi, lo) -> fprintf fd "            %s( %s downto %s ) <= %s;\n" id (vexpr hi) (vexpr lo) (initexpr expr)
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
  fprintf fd "        end if;\n";
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
  fprintf fd "        end if;\n";
    | oth -> unhand := Some oth; failwith "iff_template"

let rec sent_template fd = function
    | BeginBlock lst -> List.iter (sent_template fd) lst
    | Ifelse (Equals (Id rst, lev), if_lst, else_lst) ->
  fprintf fd "        if (%s = %s) then\n" rst (vexpr lev);
    stmt_clause fd if_lst;
  fprintf fd "        elsif (CLK'event and CLK = '1') then\n";
    stmt_clause fd else_lst;
  fprintf fd "        end if;\n";
    | Ifelse (Id rst, if_lst, else_lst) ->
  fprintf fd "        if (%s = %s) then\n" rst (vexpr (Number (2, 1, 1, "1")));
    stmt_clause fd if_lst;
  fprintf fd "        elsif (CLK'event and CLK = '1') then\n";
    stmt_clause fd else_lst;
  fprintf fd "        end if;\n";
    | oth -> unhand := Some oth; failwith "sent_template"
(*
    | oth -> stmt_clause fd oth
*)

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
    | Asgnlst lst -> List.iter (function
      | Blocking (lhs, expr) -> asgn fd expr lhs
      | oth -> unhand := Some oth; failwith "assign_template") lst
    | Iff _ -> ()
    | Hash _ -> ()
    | TypEnum _ -> ()
    | Typ (typ, [], typ_lst) -> ()
    | DeclIntf1 (typ, lst) -> List.iter (function
        | DeclIntf2 (inst, pinlst) ->
	fprintf fd "%s: %s port map (\n\t%s\n\t);\n" inst typ (String.concat ",\n\t" (List.map vexpr pinlst))
	| Id id -> fprintf fd "--%s\n" id
	| oth -> unhand := Some oth; failwith "DeclIntf"
	) lst;
    | oth -> unhand := Some oth; failwith "proc_template"

let template fd modules = function Modul(entnam, port_lst, body_lst') -> let cnt = ref 1 in
  let body_lst = List.map (recurs {fn=recurs}) body_lst' in
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
