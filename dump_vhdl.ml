open Source_text_rewrite_types
open Source_text_lex
open Source_text
open Printf

type vtyp =
  | Vint of int
(*
  | Vpkg of string
*)
  | Std_logic
  | Std_logic_vector of rw * rw
  | Vsigtyp
  | Vtyp
  | Venum

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
  | Inout -> Inout
  | Modul(str1, rw_lst1, rw_lst2, rw_lst3) -> Modul(str1, recurs_lst vhdl_attr  rw_lst1, recurs_lst vhdl_attr  rw_lst2, recurs_lst vhdl_attr  rw_lst3)
  | DeclReg(rw_lst, str1_lst, rw_lst_lst) -> DeclReg(recurs_lst vhdl_attr  rw_lst, str1_lst,
    List.map (fun itm -> recurs_lst vhdl_attr  itm) rw_lst_lst)
  | NonBlocking(rw, rw2) -> NonBlocking(recurs_itm vhdl_attr  rw, recurs_itm vhdl_attr  rw2)
  | Query(rw, rw2, rw3) -> Query(recurs_itm vhdl_attr  rw, recurs_itm vhdl_attr  rw2, recurs_itm vhdl_attr  rw3)
  | Port(rw, str1, rw_lst1, rw_lst2) -> Port(recurs_itm vhdl_attr  rw, str1, recurs_lst vhdl_attr rw_lst1, recurs_lst vhdl_attr rw_lst2)
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
  | Dot2 (id1, [], id2) as x -> x
  | oth -> unhand := Some oth; failwith "vhdl"

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
let coth = ref None
let eqlst = ref []
let clst = ref []

let ceval' = function
| Vint n -> n
| oth -> coth := Some oth; failwith "ceval'"

let clog2 n = if n = 0 then 0 else int_of_float(ceil(log(float_of_int n)/.log 2.))

let rec ceval typhash = function
| Intgr n -> n
| Id s -> ceval' (Hashtbl.find typhash s)
| Add (lhs, rhs) -> ceval typhash lhs + ceval typhash rhs
| Sub (lhs, rhs) -> ceval typhash lhs - ceval typhash rhs
| Clog2 x -> clog2 (ceval typhash x)
| Expression x -> ceval typhash x
| oth -> unhand := Some oth; failwith "ceval"

let is_const' = function
| Vint n -> true
| Venum -> true
| Std_logic_vector _ -> false
| oth -> coth := Some oth; failwith "is_const'"

let rec is_const typhash = function
| Intgr n -> true
| Id s -> is_const' (Hashtbl.find typhash s)
| Add (lhs, rhs) -> is_const typhash lhs && is_const typhash rhs
| Sub (lhs, rhs) -> is_const typhash lhs && is_const typhash rhs
| Clog2 x -> is_const typhash x
| Expression x -> is_const typhash x
| Unsigned x -> is_const typhash x
| oth -> unhand := Some oth; failwith "is_const"

let rec vexpr typhash = function
| Id s -> s
| Expression x -> " ( " ^ (vexpr typhash) x ^ " ) "
| Number (2,1,n,_) -> "'" ^ (obin 1 n) ^ "'"
| Number (2,w,n,_) -> "\"" ^ (obin w n) ^ "\""
| Number (b,w,n,s) -> "\"" ^ (obin w n) ^ "\""
| Intgr n -> "\"" ^ (obin 32 n) ^ "\""
| Tilde expr -> "not " ^ (vexpr typhash) expr
| Pling expr -> "not " ^ (vexpr typhash) expr
| Concat lst -> String.concat " & " (List.map (vexpr typhash) lst)
| Sel (Id id, (Slice _ as rhs)) -> id ^ "(to_integer (" ^ vexpr' typhash rhs ^ ") )"
| Sel (Id id, rhs) -> id ^ "( " ^ cexpr typhash rhs ^ " )"
| Slice (id, hi, lo) -> id ^ "( " ^ cexpr typhash hi ^ " downto " ^ cexpr typhash lo ^ " )"
| Add ((Id s as lhs), (Intgr n as rhs)) -> (match Hashtbl.find typhash s with
    | Std_logic_vector(hi', lo') -> let hi = ceval typhash hi' and lo = ceval typhash lo' in vexpr' typhash lhs ^ " + " ^ vexpr' typhash (Number(2, hi-lo+1, n, string_of_int n))
    | oth -> vexpr' typhash lhs ^ " + " ^ vexpr' typhash rhs)
| Add (lhs, rhs) -> vexpr' typhash lhs ^ " + " ^ vexpr' typhash rhs
| Sub ((Id s as lhs), (Intgr n as rhs)) -> (match Hashtbl.find typhash s with
    | Std_logic_vector(hi', lo') -> let hi = ceval typhash hi' and lo = ceval typhash lo' in vexpr' typhash lhs ^ " - " ^ vexpr' typhash (Number(2, hi-lo+1, n, string_of_int n))
    | oth -> vexpr' typhash lhs ^ " - " ^ vexpr' typhash rhs)
| Sub (lhs, rhs) -> vexpr' typhash lhs ^ " - " ^ vexpr' typhash rhs
| Equals (lhs, (Number(_,1,_,_) as rhs)) -> (vexpr typhash) lhs ^ " = " ^ (vexpr typhash) rhs
| Equals ((Id s as lhs), (Intgr n as rhs)) -> (match Hashtbl.find typhash s with
    | Std_logic_vector(hi', lo') -> let hi = ceval typhash hi' and lo = ceval typhash lo' in vexpr' typhash lhs ^ " = " ^ vexpr' typhash (Number(2, hi-lo+1, n, string_of_int n))
    | oth -> vexpr' typhash lhs ^ " - " ^ vexpr' typhash rhs)
| Equals ((Id s as lhs), (Number _ as rhs)) -> (match Hashtbl.find typhash s with
    | Std_logic_vector _ -> vexpr' typhash lhs ^ " = " ^ vexpr' typhash rhs
    | _ -> vexpr' typhash lhs ^ " = " ^ vexpr typhash rhs)
| Equals ((Id s as lhs), (Id _ as rhs)) -> (match Hashtbl.find typhash s with
    | Std_logic_vector _ -> vexpr' typhash lhs ^ " = " ^ vexpr' typhash rhs
    | _ -> vexpr' typhash lhs ^ " = " ^ vexpr typhash rhs)
| Equals ((Id s as lhs), rhs) as x when is_const typhash rhs -> clst := x :: !clst; (match Hashtbl.find typhash s with
    | Std_logic_vector _ -> vexpr' typhash lhs ^ " = " ^ cexpr typhash rhs
    | _ -> vexpr' typhash lhs ^ " = " ^ vexpr typhash rhs)
| Equals ((Id s as lhs), rhs) as x -> eqlst := x :: !eqlst; (match Hashtbl.find typhash s with
    | Std_logic_vector _ -> vexpr' typhash lhs ^ " = " ^ vexpr' typhash rhs
    | _ -> vexpr' typhash lhs ^ " = " ^ vexpr typhash rhs)
| Equals (lhs, rhs) -> vexpr' typhash lhs ^ " = " ^ vexpr' typhash rhs
| NotEq (lhs, rhs) -> vexpr' typhash lhs ^ " /= " ^ vexpr' typhash rhs
| GtEq (lhs, rhs) -> vexpr' typhash lhs ^ " >= " ^ vexpr' typhash rhs
| Or (lhs, rhs) -> (vexpr typhash) lhs ^ " or " ^ (vexpr typhash) rhs
| Xor (lhs, rhs) -> (vexpr typhash) lhs ^ " xor " ^ (vexpr typhash) rhs
| And (lhs, rhs) -> (vexpr typhash) lhs ^ " and " ^ (vexpr typhash) rhs
| And2 (lhs, rhs) -> (vexpr typhash) lhs ^ " and " ^ (vexpr typhash) rhs
| Unsigned expr -> "std_logic_vector("^vexpr typhash expr^")"
| Shiftl (lhs, rhs) -> "shift_left("^vexpr typhash lhs^", "^vexpr typhash rhs^")"
| Dot (port, conn) -> port ^ " => " ^ (vexpr typhash conn)
| Query (Id cond', ctrue', cfalse') -> sprintf "%s when %s = '1' else %s" (vexpr typhash ctrue') cond' (vexpr typhash cfalse')
| Query (cond', ctrue', cfalse') -> sprintf "%s when %s else %s" (vexpr typhash ctrue') (vexpr typhash cond') (vexpr typhash cfalse')
| Deflt -> "open"
| oth -> unhand := Some oth; failwith "vexpr"

and vexpr' typhash = function
| (Intgr _ | Number _) as x -> "(unsigned'(" ^ (vexpr typhash) x ^ ")) "
| (Id s) as x -> (match Hashtbl.find_opt typhash s with
    | Some (Vint _|Vtyp|Venum|Vsigtyp) -> vexpr typhash x
    | Some Std_logic -> vexpr typhash x
    | Some Std_logic_vector _ -> "unsigned (" ^ vexpr typhash x ^ ") "
    | None -> print_endline ("not found: "^s); s)
| Sel _ as x -> (vexpr typhash) x
| x -> "unsigned(" ^ vexpr typhash x ^ ") "

and cexpr typhash = function
    | Id s -> s
    | Intgr n -> string_of_int n
    | Number (_,_,n,_) -> string_of_int n
    | Add (lhs, rhs) -> cexpr typhash lhs ^ "+" ^ cexpr typhash rhs
    | Sub (lhs, rhs) -> cexpr typhash lhs ^ "-" ^ cexpr typhash rhs
    | Clog2 _ as expr -> string_of_int (ceval typhash expr)
    | Dot (port, conn) -> port ^ " => " ^ (cexpr typhash conn)
    | Expression x -> cexpr typhash x
    | StarStar (lhs, rhs) -> cexpr typhash lhs ^ "+" ^ cexpr typhash rhs
    | oth -> unhand := Some oth; failwith "cexpr"

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
| Expression (Intgr _ as x) -> x
| Expression (Shiftl (lhs, rhs)) -> Expression (simplify (Shiftl (simplify lhs, simplify rhs)))
| oth -> oth

let simplify x = 
  let rslt1 = ref (simplify (simplify (simplify (simplify (match x with Expression x -> x | _ -> x))))) in
  let rslt2 = ref (simplify (simplify (simplify (simplify !rslt1)))) in
  while !rslt1 <> !rslt2 do
    rslt1 := simplify (simplify (simplify (simplify !rslt2)));
    rslt2 := simplify (simplify (simplify (simplify !rslt1)));
  done;
  !rslt2

let vexpr typhash x = let x' = simplify x in let s = vexpr typhash x' in vlst := (x,x',s) :: !vlst; s

let initlst = ref []

let initexpr typhash x = match simplify x with
  | Intgr 0 -> "(others => '0')"
  | Sel _ as x -> vexpr typhash x
  | (Add _|Sub _) as x -> "std_logic_vector("^vexpr typhash x^")"
  | oth -> let s = (vexpr typhash) oth in initlst := (oth, s) :: !initlst; s

let sel_expr typhash x = match simplify x with Intgr _ -> "'0'" | oth -> vexpr typhash oth

let asgn fd typhash expr = function
| Id lhs -> fprintf fd "            %s <= %s;\n" lhs (initexpr typhash expr)
| Sel (Id id, Intgr rhs) -> fprintf fd "            %s(%d) <= %s;\n" id rhs (sel_expr typhash expr)
| Sel (Id id, Id ("WIDTH" as rhs)) -> fprintf fd "            %s(%s) <= %s;\n" id rhs (sel_expr typhash expr)
| Sel (Id id, rhs) -> fprintf fd "            %s(to_integer(%s)) <= %s;\n" id (vexpr' typhash rhs) (sel_expr typhash expr)
| Slice (id, hi, lo) -> fprintf fd "            %s( %s downto %s ) <= %s;\n" id (cexpr typhash hi) (cexpr typhash lo) (initexpr typhash expr)
| Concat _ as lst -> fprintf fd "            %s <= %s;\n" (vexpr typhash lst) (initexpr typhash expr)
| oth -> unhand := Some oth; failwith "asgn"

let vdir = function
  | In -> "in "
  | Out -> "out"
  | _ -> failwith "vdir"

let ports typhash = function
    | Port((In|Out) as dir,  nam, [], []) -> Hashtbl.replace typhash nam Std_logic;
        sprintf "%24s         : %s std_logic" nam (vdir dir);
    | Port ((In|Out) as dir, nam, [Dim (hi, lo)], []) -> Hashtbl.replace typhash nam (Std_logic_vector(hi,lo));
        sprintf "%24s         : %s std_logic_vector(%s downto %s)" nam (vdir dir) (cexpr typhash hi) (cexpr typhash lo);
    | Port ((In|Out) as dir, nam, Dim (hi, lo) :: Dim(hi', lo') :: [], []) -> Hashtbl.replace typhash nam (Std_logic_vector(hi,lo));
        sprintf "%24s         : %s std_logic_vector(%s downto %s)(%s downto %s)" nam (vdir dir) (cexpr typhash hi) (cexpr typhash lo) (cexpr typhash hi') (cexpr typhash lo')
    | oth -> unhand := Some oth; failwith "component"

let parm_template fd typhash parm_lst = 
  if parm_lst <> [] then
  fprintf fd "    generic (\n%s\n    );\n" (String.concat ";\n" (List.map (function
  | Param (nam, Intgr expr) -> Hashtbl.replace typhash nam (Vint expr); sprintf "%24s         : integer := %d" nam expr
  | Param (nam, Number (_, _, expr, _)) -> Hashtbl.replace typhash nam (Vint expr); sprintf "%24s         : integer := %d" nam expr
  | Dot (nam, Intgr expr) -> Hashtbl.replace typhash nam (Vint expr); sprintf "%24s         : integer := %d" nam expr
  | Param (nam, Unknown pkg) -> sprintf "%24s         : string := %s" nam pkg
  | TypParam (nam, Logic _, Unknown typ :: _) -> sprintf "%24s         : string := %s" nam typ
  | oth -> unhand := Some oth; failwith "param_lst") parm_lst))

let decl_mem fd typhash first last hi lo cnt mem =
    fprintf fd "    type Mem_Type%d is array (%s downto %s) of std_logic_vector(%s downto %s);\n" !cnt (cexpr typhash last) (cexpr typhash first) (cexpr typhash hi) (cexpr typhash lo);
    fprintf fd "    signal %s : Mem_Type%d := (others => (others => '0'));\n" mem !cnt;
incr cnt

let decl_template fd typhash modules complst cnt = function
    | DeclWire ([Dim (hi, lo)], wire_lst) -> List.iter (function
	  | Id nam -> Hashtbl.replace typhash nam (Std_logic_vector(hi,lo));
	  fprintf fd "    signal %s : std_logic_vector(%s downto %s);\n" nam (cexpr typhash hi) (cexpr typhash lo)
	  | oth -> unhand := Some oth; failwith "DeclWire") wire_lst
    | DeclWire ([], wire_lst) -> List.iter (function
          | Id nam -> Hashtbl.replace typhash nam Std_logic;
	  fprintf fd "    signal %s : std_logic;\n" nam
	  | oth -> unhand := Some oth; failwith "DeclWire") wire_lst;
    | DeclLogic (reg_lst) -> List.iter (function
	  | Id nam -> Hashtbl.replace typhash nam Std_logic; fprintf fd "    signal %s : std_logic;\n" nam
	  | oth -> unhand := Some oth; failwith "DeclLogic"
        ) reg_lst;
    | DeclReg ([], reg_lst, [[]]) -> List.iter (fun nam -> Hashtbl.replace typhash nam Std_logic;
    fprintf fd "    signal %s : std_logic;\n" nam) reg_lst;
    | DeclReg ([Dim (hi, lo)], mem_lst, [[Dim (first, last)]]) -> List.iter (decl_mem fd typhash first last hi lo cnt) mem_lst
    | DeclReg ([Dim (hi, lo)], nam_lst, init) -> List.iter (fun nam -> Hashtbl.replace typhash nam (Std_logic_vector(hi,lo));
    fprintf fd "    signal %s : std_logic_vector(%s downto %s);\n" nam (cexpr typhash hi) (cexpr typhash lo);
    if init <> [] then fprintf stderr "register %s is initialised (ignored)\n" nam) nam_lst
    | DeclIntf1 (typ, _) -> if not (List.mem typ !complst) then
    begin
    complst := typ :: !complst;
    match Hashtbl.find_opt modules typ with Some (Modul(_, _, port_lst, _)) ->
  fprintf fd "    -- Component %s description\n" typ;
  fprintf fd "    component %s is\n" typ;
  fprintf fd "    port (\n%s\n    );\n" (String.concat ";\n" (List.map (ports typhash) port_lst));
  fprintf fd "    end component;\n";
	| None -> fprintf fd "-- %s is not a module\n" typ;
        | Some oth -> unhand := Some oth; failwith ("DeclIntf: "^typ)
    end
    | CaseStmt _ -> ()
    | Asgnlst _ -> ()
    | Sentry _ -> ()
    | Hash (typ, parm_lst, _) -> if not (List.mem typ !complst) then
    begin
    complst := typ :: !complst;
    match Hashtbl.find_opt modules typ with Some (Modul(_, _, port_lst, _)) ->
  fprintf fd "    -- Component %s description\n" typ;
  fprintf fd "    component %s is\n" typ;
  (match parm_lst with Itmlst lst :: [] -> parm_template fd typhash lst | _ -> failwith "component");
  fprintf fd "    port (\n%s\n    );\n" (String.concat ";\n" (List.map (ports typhash) port_lst));
  fprintf fd "    end component;\n";
	| None -> fprintf fd "-- %s is not a module\n" typ;
        | Some oth -> unhand := Some oth; failwith ("DeclIntf: "^typ)
    end
    | Typ ("bool_t", [], [Id "FALSE"; Id "TRUE"]) -> ()
    | Typ (nam, _, id :: []) ->  Hashtbl.replace typhash nam Vtyp;
        let s = vexpr typhash id in Hashtbl.replace typhash s Vsigtyp; fprintf fd "    signal %s : %s;\n" s nam
    | Typ (nam, _, id_lst) ->  Hashtbl.replace typhash nam Vtyp;
        List.iter (fun itm -> let s = vexpr typhash itm in Hashtbl.replace typhash s Vsigtyp; fprintf fd "    signal %s : %s;\n" s nam) id_lst;
    | TypEnum (nam, _, id_lst) -> Hashtbl.replace typhash nam Vtyp;
        fprintf fd "    type %s is (%s);\n" nam (String.concat ", " (List.map (fun itm -> let s = vexpr typhash itm in Hashtbl.replace typhash s Venum; s) id_lst))
    | oth -> unhand := Some oth; failwith "decl_template"

let othlst = ref []

let rec stmt_clause fd typhash = function
      | Itmlst lst -> List.iter (stmt_clause fd typhash) lst      
      | BeginBlock lst -> List.iter (stmt_clause fd typhash) lst      
      | Ifelse (condition, if_lst, else_lst) ->
  fprintf fd "        if (%s) then\n" (vexpr typhash condition);
    (match if_lst with BeginBlock if_lst -> List.iter (stmt_clause fd typhash) if_lst | _ -> stmt_clause fd typhash if_lst);       
  fprintf fd "        else\n";
    (match else_lst with BeginBlock else_lst -> List.iter (stmt_clause fd typhash) else_lst | _ -> stmt_clause fd typhash else_lst);
  fprintf fd "        end if;\n";
      | Blocking (lhs, expr) -> asgn fd typhash expr lhs
      | NonBlocking (lhs, expr) -> asgn fd typhash expr lhs
      | Iff _ as x -> iff_template fd typhash x
      | DeclLogic lst -> ()
      | CaseStmt (state, itmlst) ->
  fprintf fd "            case %s is\n" (vexpr typhash state);
          List.iter (case_clause fd typhash) itmlst;
  fprintf fd "            end case;\n";
      | Unknown ";" -> ()
      | oth -> unhand := Some oth; failwith "stmt_clause"

and case_clause fd typhash = function
    | CaseItm caselst -> (match List.rev caselst with 
        | Id caseid :: body ->
  fprintf fd "                when %s       =>  " caseid;
	    List.iter (stmt_clause fd typhash) body
        | Number _ as x :: body ->
  fprintf fd "                when %s       =>  " (vexpr typhash x);
            List.iter (stmt_clause fd typhash) body
        | Deflt :: body ->
  fprintf fd "                when others       =>  ";
	    List.iter (stmt_clause fd typhash) body
	| oth -> othlst := oth; failwith "case_item")
    | oth -> unhand := Some oth; failwith "case_clause"
    
and iff_template fd typhash = function
    | Source_text_rewrite_types.Iff(condition, if_lst) ->
  fprintf fd "        if (%s) then\n" (vexpr typhash condition);
    stmt_clause fd typhash if_lst;
  fprintf fd "        end if;\n";
    | oth -> unhand := Some oth; failwith "iff_template"

let rec sent_template fd typhash clk = function
    | BeginBlock lst -> List.iter (sent_template fd typhash clk) lst
    | Ifelse (Equals (Id rst, lev), if_lst, else_lst) ->
  fprintf fd "        if (%s = %s) then\n" rst (vexpr typhash lev);
    stmt_clause fd typhash if_lst;
  fprintf fd "        elsif (%s'event and %s = '1') then\n" clk clk;
    stmt_clause fd typhash else_lst;
  fprintf fd "        end if;\n";
    | Ifelse (Id rst, if_lst, else_lst) ->
  fprintf fd "        if (%s = %s) then\n" rst (vexpr typhash (Number (2, 1, 1, "1")));
    stmt_clause fd typhash if_lst;
  fprintf fd "        elsif (%s'event and %s = '1') then\n" clk clk;
    stmt_clause fd typhash else_lst;
  fprintf fd "        end if;\n";
    | Ifelse (Pling (Id rst), if_lst, else_lst) ->
  fprintf fd "        if (%s = %s) then\n" rst (vexpr typhash (Number (2, 1, 0, "1")));
    stmt_clause fd typhash if_lst;
  fprintf fd "        elsif (%s'event and %s = '1') then\n" clk clk;
    stmt_clause fd typhash else_lst;
  fprintf fd "        end if;\n";
    | Iff (Id cond, if_lst) ->
  fprintf fd "        if (%s'event and %s = '1') then\n" clk clk;
    stmt_clause fd typhash if_lst;
  fprintf fd "        end if;\n";

    | oth -> unhand := Some oth; failwith "sent_template"

let instance_template fd typhash typ params = function
    | DeclIntf2 (inst, pinlst) -> othlst := pinlst;
        fprintf fd "%s: %s" inst typ;
        if params <> [] then fprintf fd " generic map (%s)\n" (String.concat ", " (List.map (cexpr typhash) params));
        fprintf fd " port map (\n\t%s\n\t);\n" (String.concat ",\n\t" (List.map (vexpr typhash) pinlst))
    | Id id ->  fprintf fd "--%s proc318\n" id
    | oth -> unhand := Some oth; failwith "DeclIntf"	
    
let proc_template fd typhash cnt = function
    | DeclReg _ -> ()
    | DeclWire _ -> ()
    | DeclLogic _ -> ()
    | Sentry (Edge (Pos clk, (Pos rst|Neg rst)), sent_lst) ->
  fprintf fd "    -- clocked process %d description goes here\n" !cnt;
  fprintf fd "    SEQ%d: process (%s, %s)\n" !cnt clk rst;
  incr cnt;
  fprintf fd "    begin\n";
  sent_template fd typhash clk sent_lst;       
  fprintf fd "    end process;\n";
  fprintf fd "\n";
    | Sentry (Pos clk, sent_lst) ->
  fprintf fd "    -- clocked process %d description goes here\n" !cnt;
  fprintf fd "    SEQ%d: process (%s)\n" !cnt clk;
  incr cnt;
  fprintf fd "    begin\n";
  sent_template fd typhash clk sent_lst;       
  fprintf fd "    end process;\n";
  fprintf fd "\n";
    | Sentry (DepLst dep_lst, sent_lst) ->
  fprintf fd "    -- combinational process %d description goes here\n" !cnt;
  fprintf fd "    COMB%d: process (%s)\n" !cnt (String.concat ", " dep_lst);
  incr cnt;
  fprintf fd "    begin\n";
  stmt_clause fd typhash sent_lst;
  fprintf fd "    end process;\n";
  fprintf fd "\n";
  (* elaboration case *)
   | CaseStmt (Id id, (CaseItm (BeginBlock [] :: Unknown "$error" :: Deflt :: []) :: [])) -> fprintf fd "-- elaboration case %s\n" id;
    | Asgnlst lst -> List.iter (function
      | Blocking (lhs, expr) -> asgn fd typhash expr lhs
      | oth -> unhand := Some oth; failwith "assign_template") lst
    | Iff _ -> ()
    | Hash (typ, [Itmlst params], lst) -> List.iter (instance_template fd typhash typ params) lst;
    | TypEnum _ -> ()
    | Typ (typ, _, typ_lst) -> ()
    | DeclIntf1 (typ, lst) -> List.iter (instance_template fd typhash typ []) lst;
    | oth -> unhand := Some oth; failwith "proc_template"

let template fd modules = function Modul(entnam, parm_lst, port_lst, body_lst') -> let cnt = ref 1 in
  let body_lst = List.map (recurs {fn=recurs}) body_lst' in
  let typhash = Hashtbl.create 255 in
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
  parm_template fd typhash parm_lst;
  fprintf fd "    port (\n%s\n    );\n" (String.concat ";\n" (List.map (ports typhash) port_lst));
  fprintf fd "end %s;\n" entnam;
  fprintf fd "\n";
  fprintf fd "architecture rtl of %s is\n" entnam;
  fprintf fd "    -- Signals\n";
  let complst = ref [] in
  List.iter (decl_template fd typhash modules complst cnt) (List.sort compare (List.filter (function DeclIntf1 _ -> true | _ -> false) body_lst));
  List.iter (decl_template fd typhash modules complst cnt) (List.filter (function DeclIntf1 _ -> false | _ -> true) body_lst);
  fprintf fd "begin\n";
List.iter (proc_template fd typhash cnt) body_lst;
  fprintf fd "\n";
  fprintf fd "end rtl;\n";
  fprintf fd "\n";
  fprintf fd "\n";
  | oth -> failwith "This template only handles modules"
