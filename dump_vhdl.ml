open Source_text_rewrite_types
open Source_text_lex
open Source_text
open Printf

type vtyp =
  | Vint of int
  | Vpkg of string * string
  | Std_logic
  | Std_logic_vector of rw * rw
  | Vsigtyp
  | Vtyp of string
  | Vfun of string
  | Venum of string

let unhand = ref None
let unhandlst = ref []
let update typhash id expr =
(*
 print_endline id;
 *)
  Hashtbl.replace typhash id expr

type vhdl_attr = {fn: vhdl_attr -> rw -> rw}

let rec recurs' (vhdl_attr:vhdl_attr) = function
  | Id id -> Id id
  | Expression rw -> Expression(recurs_itm vhdl_attr  rw)
  | Itmlst(rw_lst) -> Itmlst(recurs_lst vhdl_attr  (rw_lst:rw list))
(*
  | Sentry (Pos clk, lst) -> Sentry(Pos clk, recurs_itm vhdl_attr  lst)
*)
  | Unknown (str,lst) -> Unknown (str, recurs_lst vhdl_attr  lst)
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
(*
  | Sel(str1, rw2) -> Sel(str1, recurs_itm vhdl_attr  rw2)
  | Inc(rw) -> Inc(recurs_itm vhdl_attr  rw)
  | Dec(rw) -> Dec(recurs_itm vhdl_attr  rw)
  | RedAnd(rw) -> RedAnd(recurs_itm vhdl_attr  (rw))
  | RedOr(rw) -> RedOr(recurs_itm vhdl_attr  (rw))
  *)
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
(*
  | Ifelse(rw, rw2, rw3) -> Ifelse(recurs_itm vhdl_attr  (rw), recurs_itm vhdl_attr  (rw2), recurs_itm vhdl_attr  (rw3))
  | Iff(rw, rw2) -> Iff(recurs_itm vhdl_attr  (rw), recurs_itm vhdl_attr  (rw2))
*)
  | ForLoop(rw_lst, rw2, rw3, rw4) ->
    ForLoop(recurs_lst vhdl_attr  (rw_lst), recurs_itm vhdl_attr  (rw2), recurs_itm vhdl_attr  (rw3), recurs_itm vhdl_attr  (rw4))
  | CaseStmt(rw, rw_lst) -> CaseStmt(recurs_itm vhdl_attr  (rw), recurs_lst vhdl_attr  (rw_lst))
  | CaseItm(rw_lst) -> CaseItm(recurs_lst vhdl_attr  (rw_lst))
  | AlwaysComb(rw_lst) -> AlwaysComb(recurs_lst vhdl_attr  (rw_lst))
(*
  | Sentry(rw, rw2) -> Sentry(recurs_itm vhdl_attr  (rw), recurs_itm vhdl_attr  (rw2))
  | Blocking(rw, rw2) -> Blocking(recurs_itm vhdl_attr  (rw), recurs_itm vhdl_attr  (rw2))
  | Asgnlst(rw_lst) -> Asgnlst(recurs_lst vhdl_attr  (rw_lst))
  | DeclInt(str1_lst) -> DeclInt((str1_lst))
  | Dim(rw, rw2) -> Dim(recurs_itm vhdl_attr  (rw), recurs_itm vhdl_attr  (rw2))
*)
  | BeginBlock(rw_lst) -> BeginBlock(recurs_lst vhdl_attr  (rw_lst))
  | Bitlst(rw_lst) -> Bitlst(recurs_lst vhdl_attr  (rw_lst))
  | Dot1(str1, rw2) -> Dot1(str1, recurs_itm vhdl_attr  (rw2))
  | Unsigned(rw) -> Unsigned(recurs_itm vhdl_attr  (rw))
  | Signed(rw) -> Signed(recurs_itm vhdl_attr  (rw))
  | Concat(rw_lst) -> Concat(recurs_lst vhdl_attr  (rw_lst))
(*
  | DeclWire(rw_lst, rw_lst2) -> DeclWire(recurs_lst vhdl_attr  (rw_lst), recurs_lst vhdl_attr  (rw_lst2))
*)
  | WireExpr(str1, rw2) -> WireExpr(str1, recurs_itm vhdl_attr  (rw2))
(*
   | DeclIntf1(str1, rw_lst) -> DeclIntf1(str1, recurs_lst vhdl_attr  (rw_lst))
  | DeclIntf2(str1, rw_lst) -> DeclIntf2(str1, recurs_lst vhdl_attr  (rw_lst))
  | Hash(str1, rw_lst, rw_lst2) -> Hash(str1, recurs_lst vhdl_attr  (rw_lst), recurs_lst vhdl_attr  (rw_lst2))
  | DeclIntf(str1, rw_lst, rw_lst2, rw_lst3) -> DeclIntf(str1, recurs_lst vhdl_attr  (rw_lst), recurs_lst vhdl_attr  (rw_lst2), recurs_lst vhdl_attr  (rw_lst3))
  | Slice(str1, rw2, rw3) -> Slice(str1, recurs_itm vhdl_attr  (rw2), recurs_itm vhdl_attr  (rw3))
  | Field(rw, rw2) -> Field(recurs_itm vhdl_attr  (rw), recurs_itm vhdl_attr  (rw2))
*)
  | DeclModPort(rw_lst) -> DeclModPort(recurs_lst vhdl_attr  (rw_lst))
  | Repl(rw, rw_lst) -> Repl(recurs_itm vhdl_attr  (rw), recurs_lst vhdl_attr  (rw_lst))
  | Dot3(str1, str2, str3) -> Dot3(str1, str2, str3)
(*
  | Parenth(str1, rw_lst) -> Parenth(str1, recurs_lst vhdl_attr  (rw_lst))
*)
  | Logic(rw_lst, rw_lst2) -> Logic(recurs_lst vhdl_attr  (rw_lst), recurs_lst vhdl_attr  (rw_lst2))
  | Param(str1, rw2, rw_lst) -> Param(str1, recurs_itm vhdl_attr (rw2), recurs_lst vhdl_attr (rw_lst))
  | DeclLogic(rw_lst) -> DeclLogic(recurs_lst vhdl_attr  (rw_lst))
  | DeclData(rw1, rw_lst2) -> DeclData(recurs_itm vhdl_attr  (rw1), recurs_lst vhdl_attr  (rw_lst2))
(*
  | Mem1(str1, rw_lst) -> Mem1(str1, recurs_lst vhdl_attr  (rw_lst))
  | Mem3(str1, rw2, rw3, rw4) -> Mem3(str1, recurs_itm vhdl_attr  (rw2), recurs_itm vhdl_attr  (rw3), recurs_itm vhdl_attr  (rw4))
  | PartSel(str1, rw2, rw3) -> PartSel(str1, recurs_itm vhdl_attr  (rw2), recurs_itm vhdl_attr  (rw3))
*)
  | GenBlock(rw_lst) -> GenBlock(recurs_lst vhdl_attr  (rw_lst))
  | Package (id, rw_lst) -> Package(id, recurs_lst vhdl_attr  rw_lst)
(*
  | Caret _ as x -> x
  | Bits _ as x -> x
  | Struct _ as x -> x
  | TypEnum _ as x -> x
  | Clog2 _ as x -> x
  | Comma _ as x -> x
  | DeclGenvar _ as x -> x
  | DepLst lst as x -> x
*)
  | Typ1 _ as x -> x
  | Cast (_, _) as x -> x
  | Deflt -> Deflt
  | (Assert|AssertProperty|AtStar|BreakSemi|PropertySpec|AlwaysComb2 _|
AlwaysFF (_, _)|AlwaysLatch _|AlwaysLegacy (_, _)|And3 (_, _)|
AnyRange (_, _)|Asgn1 (_, _)|AsgnPat _|At _|Atom _|AutoFunDecl (_, _, _)|
CaseStart (_, _)|CaseStart1 _|CaseStart2 (_, _)|CaseStartUniq (_, _)|
CaseStartUniq2 (_, _)|CellParamItem1 (_, _)|CellParamItem2 (_, _)|
CellParamItem3 (_, _)|CellPinItem1 (_, _)|CellPinItem2 (_, _)|
CellPinItemImplied _|CellPinItemNC _|CondGen1 (_, _, _)|ContAsgn _|
DeclAsgn (_, _)|DeclInt2 _|DeclLogic2 (_, _)|DeclReg2 (_, _)|
DotBus (_, _, _, _)|ElabTask _|Elist _|ElseStmt _|EnumInit (_, _)|
Equals3 (_, _)|EqualsQuery (_, _)|Equate (_, _)|
EquateArrayField (_, _, _, _, _)|EquateField (_, _, _)|
EquateSelect (_, _, _)|EquateSelect2 (_, _, _)|EquateSlice (_, _, _, _)|
EventOr (_, _)|ExprOKL _|ExprQuote1 (_, _)|Final _|FopAsgn (_, _)|
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
IdArrayedHyphenColon (_, _, _)|IdArrayedPlusColon (_, _, _)|If1 (_, _)|
If2 (_, _, _)|Iff (_, _)|Import _|Inc _|InitPair (_, _)|InitPat _|
InitSig (_, _)|Initial _|InsideCase (_, _)|InsideRange (_, _)|
InstDecl (_, _, _)|InstNameParen1 (_, _)|
InstNameParen2 (_, _)|InstRange (_, _)|IntfDecl (_, _, _, _)|ItemAsgn _|
LocalParamTyp _|LoopGen1 (_, _, _, _, _, _)|LtGt (_, _)|Mod (_, _)|
ModPortItm (_, _)|Nand (_, _)|NetDecl (_, _)|Nor (_, _)|NotEq3 (_, _)|
NotEqQuery (_, _)|OpenRange (_, _)|ParamAsgn1 (_, _)|ParamAsgn2 (_, _, _)|
ParamDecl (_, _)|ParamPort _|PatMember1 (_, _)|PatMemberDflt _|PkgImport _|
PkgImportItm (_, _)|PortDir (_, _)|PortFront (_, _)|PortItem (_, _)|
PortItemFront (_, _)|PortsStar _|Return _|SUMember (_, _)|Seq (_, _)|
SideEffect (_, _)|Stmt1 _|String _|Sys (_, _)|SysFuncCall (_, _)|
SysTaskCall (_, _)|TaskBody (_, _)|TaskRef (_, _)|Typ2 (_, _, _)|Typ3 (_, _)|
Typ4 (_, _, _, _)|Typ5 (_, _)|Typ6 _|Typ7 (_, _)|Typ8 (_, _)|Typ9 (_, _, _)|
Typ10 (_, _, _)|TypEnum3 _|TypEnum4 (_, _, _)|TypEnum5 _|TypEnum6 (_, _, _)|
TypParam (_, _, _)|UPlus _|Union (_, _)|VNum _|ValueRange (_, _)|
VarDeclAsgn (_, _)|VarDim _|While (_, _)|PackageParam _|PackageParam2 _ as x) -> x
  (*
  | oth -> unhand := Some oth; failwith "vhdl"
*)

and recurs_lst vhdl_attr x = List.map (vhdl_attr.fn vhdl_attr ) x

and recurs_itm vhdl_attr  x = vhdl_attr .fn vhdl_attr  x

let rec recurs (vhdl_attr:vhdl_attr) = function
  | If2(Expression rw, rw2, rw3) -> If2(recurs_itm vhdl_attr (rw), recurs_itm vhdl_attr (rw2), recurs_itm vhdl_attr (rw3))
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
| Vtyp s -> print_endline ("Type "^s^" evaluated to zero"); 0
| oth -> coth := Some oth; failwith "ceval'"

let clog2 n = if n = 0 then 0 else int_of_float(ceil(log(float_of_int n)/.log 2.))

let rec ceval typhash = function
| Intgr n -> n
| Id s -> ceval' (match Hashtbl.find_opt typhash s with Some x -> x | None -> failwith ("Not found: "^s))
| Add (lhs, rhs) -> ceval typhash lhs + ceval typhash rhs
| Sub (lhs, rhs) -> ceval typhash lhs - ceval typhash rhs
| Sys ("$clog2", x) -> clog2 (ceval typhash x)
| Expression x -> ceval typhash x
| oth -> unhand := Some oth; failwith "ceval"

let is_const' = function
| Vint n -> true
| Venum _ -> true
| Std_logic_vector _ -> false
| oth -> coth := Some oth; failwith "is_const'"

let rec is_const typhash = function
| Intgr n -> true
| Id s -> is_const' (Hashtbl.find typhash s)
| Add (lhs, rhs) -> is_const typhash lhs && is_const typhash rhs
| Sub (lhs, rhs) -> is_const typhash lhs && is_const typhash rhs
(*
| Clog2 x -> is_const typhash x
*)
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
(*
| Sel (Id id, (Slice _ as rhs)) -> id ^ "(to_integer (" ^ vexpr' typhash rhs ^ ") )"
| Sel (Id id, rhs) -> id ^ "( " ^ cexpr typhash rhs ^ " )"
| Slice (id, hi, lo) -> id ^ "( " ^ cexpr typhash hi ^ " downto " ^ cexpr typhash lo ^ " )"
*)
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
| CellPinItem1 (port, conn) -> port ^ " => " ^ conn
| CellPinItem2 (port, expr) -> port ^ " => " ^ vexpr typhash expr
| CellPinItemImplied (port) -> port ^ " => " ^ port
| CellPinItemNC (port) -> port ^ " => open"
| Query (Id cond', ctrue', cfalse') -> sprintf "%s when %s = '1' else %s" (vexpr typhash ctrue') cond' (vexpr typhash cfalse')
| Query (cond', ctrue', cfalse') -> sprintf "%s when %s else %s" (vexpr typhash ctrue') (vexpr typhash cond') (vexpr typhash cfalse')
| Deflt -> "open"
| IdArrayed2 (Id lft, Id rght) -> lft^"."^rght
| Dot1(Id lft, Id rght) -> lft^"."^rght
| oth -> unhand := Some oth; failwith "vexpr"

and vexpr' typhash = function
| (Intgr _ | Number _) as x -> "(unsigned'(" ^ (vexpr typhash) x ^ ")) "
| (Id s) as x -> (match Hashtbl.find_opt typhash s with
    | Some (Vint _|Vtyp _|Venum _|Vsigtyp|Vpkg _|Vfun _) -> vexpr typhash x
    | Some Std_logic -> vexpr typhash x
    | Some Std_logic_vector _ -> "unsigned (" ^ vexpr typhash x ^ ") "
    | None -> print_endline ("not found: "^s); s)
(*
| Sel _ as x -> (vexpr typhash) x
*)
| x -> "unsigned(" ^ vexpr typhash x ^ ") "

and cexpr typhash = function
    | Id s -> s
    | Intgr n -> string_of_int n
    | Number (_,_,n,_) -> string_of_int n
    | Add (lhs, rhs) -> cexpr typhash lhs ^ "+" ^ cexpr typhash rhs
    | Sub (lhs, rhs) -> cexpr typhash lhs ^ "-" ^ cexpr typhash rhs
    | Sys ("$clog2", _) as expr -> string_of_int (ceval typhash expr)
    | Dot1 (port, conn) -> (cexpr typhash port) ^ " => " ^ (cexpr typhash conn)
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
(*
  | Sel _ as x -> vexpr typhash x
*)
  | (Add _|Sub _) as x -> "std_logic_vector("^vexpr typhash x^")"
  | oth -> let s = (vexpr typhash) oth in initlst := (oth, s) :: !initlst; s

let sel_expr typhash x = match simplify x with Intgr _ -> "'0'" | oth -> vexpr typhash oth

let asgn fd typhash expr = function
| Id lhs -> fprintf fd "            %s <= %s;\n" lhs (initexpr typhash expr)
(*
| Sel (Id id, Intgr rhs) -> fprintf fd "            %s(%d) <= %s;\n" id rhs (sel_expr typhash expr)
| Sel (Id id, Id ("WIDTH" as rhs)) -> fprintf fd "            %s(%s) <= %s;\n" id rhs (sel_expr typhash expr)
| Sel (Id id, rhs) -> fprintf fd "            %s(to_integer(%s)) <= %s;\n" id (vexpr' typhash rhs) (sel_expr typhash expr)
| Slice (id, hi, lo) -> fprintf fd "            %s( %s downto %s ) <= %s;\n" id (cexpr typhash hi) (cexpr typhash lo) (initexpr typhash expr)
*)
| Concat _ as lst -> fprintf fd "            %s <= %s;\n" (vexpr typhash lst) (initexpr typhash expr)
| oth -> unhand := Some oth; failwith "asgn"

let vdir = function
  | In -> "in "
  | Out -> "out"
  | _ -> failwith "vdir"

let ports typhash = function
    | Port((In|Out) as dir,  nam, [], []) -> update typhash nam Std_logic;
        sprintf "%24s         : %s std_logic" nam (vdir dir);
    | Port ((In|Out) as dir, nam, [AnyRange (hi, lo)], []) -> update typhash nam (Std_logic_vector(hi,lo));
        sprintf "%24s         : %s std_logic_vector(%s downto %s)" nam (vdir dir) (cexpr typhash hi) (cexpr typhash lo);
    | Port ((In|Out) as dir, nam, AnyRange (hi, lo) :: AnyRange(hi', lo') :: [], []) -> update typhash nam (Std_logic_vector(hi,lo));
        sprintf "%24s         : %s std_logic_vector(%s downto %s)(%s downto %s)" nam (vdir dir) (cexpr typhash hi) (cexpr typhash lo) (cexpr typhash hi') (cexpr typhash lo')
    |  Port ((In|Out) as dir, nam, Typ2 (typ_e, (Package (pkg, [])) :: [], []) :: [], []) -> update typhash nam (Vpkg(pkg, typ_e));
        sprintf "%24s         : %s std_logic" nam (vdir dir);
    |  Port ((In|Out) as dir, nam, Typ2 (typ_t, [], []) :: [], []) -> update typhash nam (Vtyp(typ_t));
        sprintf "%24s         : %s std_logic" nam (vdir dir);
    |  Port ((In|Out) as dir, nam, Typ2 (typ_t, [], []) :: AnyRange (hi, lo) :: [], []) -> update typhash nam (Vtyp(typ_t));
        sprintf "%24s         : %s std_logic_vector(%s downto %s)" nam (vdir dir) (cexpr typhash hi) (cexpr typhash lo);
    | oth -> unhand := Some oth; failwith "component"

let parm_dump typhash = function
  | CellParamItem1 (nam, s) ->
      update typhash nam (Hashtbl.find typhash s);
      sprintf "%24s         : string := %s" nam s
  | CellParamItem2 (nam, Typ1 s) ->
      sprintf "%24s         : type := %s" nam s
  | CellParamItem2 (nam, Number (_, _, n, _)) ->
      update typhash nam (Vint n); 
      sprintf "%24s         : integer := %d" nam n
  | Param (nam, Number (_, _, n, _), []) ->
      update typhash nam (Vint n);
      sprintf "%24s         : integer := %d" nam n
  | PackageParam2 (grp_e, nam, [Package (pkg, [])], Package (pkg', [Id s])) ->
      update typhash nam (Venum grp_e);
      sprintf "%24s         : %s := %s" grp_e nam s
  | PackageParam2 (id_t, nam, [Package (pkg, [])], Number (_, _, n, _)) ->
      update typhash nam (Vint n);
      sprintf "%24s         : integer := %d" nam n
  | PackageParam2 (id_t, nam, [Package (pkg, [])], AsgnPat [PatMemberDflt (Number (_, _, n, ""))]) ->
      update typhash nam (Vint n);
      sprintf "%24s         : integer := %d" nam n
  | PackageParam2 (id_t, nam, [Package (pkg, [])], AsgnPat [PatMemberDflt (Package (pkg', [Id id]))]) ->
      update typhash nam (Vtyp id_t);
      sprintf "%24s         : string := %s" nam id
  | TypParam (nam, Atom "logic", []) ->
      update typhash nam (Vtyp nam); 
      sprintf "%24s         : type := std_logic" nam
  | Param (nam, Package (pkg, [Id id]), []) ->
      update typhash nam (Vtyp nam);
      sprintf "%24s         : integer := %s" nam id
  | Param (nam, FunRef2 (fn, [Package (pkg, [])], [Id id]), []) ->
      update typhash nam (Vfun fn);
      sprintf "%24s         : string := %s" nam id
  | oth -> unhand := Some oth; failwith "param_lst"

let parm_template fd typhash parm_lst = 
  if parm_lst <> [] then
  fprintf fd "    generic (\n%s\n    );\n" (String.concat ";\n" (List.map (parm_dump typhash) parm_lst))

let decl_mem fd typhash first last hi lo cnt mem =
    fprintf fd "    type Mem_Type%d is array (%s downto %s) of std_logic_vector(%s downto %s);\n" !cnt (cexpr typhash last) (cexpr typhash first) (cexpr typhash hi) (cexpr typhash lo);
    fprintf fd "    signal %s : Mem_Type%d := (others => (others => '0'));\n" mem !cnt;
incr cnt

let mod_template fd modules typhash complst (typ, parm_lst) =
if not (List.mem typ !complst) then
    begin
    complst := typ :: !complst;
    match Hashtbl.find_opt modules typ with Some (Modul(_, _, port_lst, _)) ->
  fprintf fd "    -- Component %s description\n" typ;
  fprintf fd "    component %s is\n" typ;
  (match parm_lst with [] -> () | Itmlst lst :: [] -> parm_template fd typhash lst | _ -> unhandlst := parm_lst; failwith "parm_lst");
  fprintf fd "    port (\n%s\n    );\n" (String.concat ";\n" (List.map (ports typhash) port_lst));
  fprintf fd "    end component;\n";
	| None -> fprintf fd "-- %s is not a module\n" typ;
        | Some oth -> unhand := Some oth; failwith ("DeclIntf: "^typ)
end

let decl_template fd typhash modules complst cnt = function
(*
    | DeclWire ([Dim (hi, lo)], wire_lst) -> List.iter (function
	  | Id nam -> update typhash nam (Std_logic_vector(hi,lo));
	  fprintf fd "    signal %s : std_logic_vector(%s downto %s);\n" nam (cexpr typhash hi) (cexpr typhash lo)
	  | oth -> unhand := Some oth; failwith "DeclWire") wire_lst
    | DeclWire ([], wire_lst) -> List.iter (function
          | Id nam -> update typhash nam Std_logic;
	  fprintf fd "    signal %s : std_logic;\n" nam
	  | oth -> unhand := Some oth; failwith "DeclWire") wire_lst;
*)
    | DeclLogic (reg_lst) -> List.iter (function
	  | Id nam -> update typhash nam Std_logic; fprintf fd "    signal %s : std_logic;\n" nam
	  | oth -> unhand := Some oth; failwith "DeclLogic"
        ) reg_lst;
    | DeclLogic2 (wire_lst, [AnyRange (hi, lo)]) -> List.iter (function
	  | Id nam -> update typhash nam (Std_logic_vector(hi,lo));
	  fprintf fd "    signal %s : std_logic_vector(%s downto %s);\n" nam (cexpr typhash hi) (cexpr typhash lo)
	  | oth -> unhand := Some oth; failwith "DeclWire") wire_lst
    | DeclReg ([], reg_lst, [[]]) -> List.iter (fun nam -> update typhash nam Std_logic;
    fprintf fd "    signal %s : std_logic;\n" nam) reg_lst;
(*
    | DeclReg ([Dim (hi, lo)], mem_lst, [[Dim (first, last)]]) -> List.iter (decl_mem fd typhash first last hi lo cnt) mem_lst
    | DeclReg ([Dim (hi, lo)], nam_lst, init) -> List.iter (fun nam -> update typhash nam (Std_logic_vector(hi,lo));
    fprintf fd "    signal %s : std_logic_vector(%s downto %s);\n" nam (cexpr typhash hi) (cexpr typhash lo);
    if init <> [] then fprintf stderr "register %s is initialised (ignored)\n" nam) nam_lst
*)
(*
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
    *)
| CaseStmt _ -> ()
    | ContAsgn _ -> ()
    | LoopGen1 _ -> ()
    | GenItem _ -> ()
(*
    | Sentry _ -> ()
*)
    | InstDecl (typ, parm_lst, _) -> mod_template fd modules typhash complst (typ, parm_lst)
    | Typ2 ("bool_t", [], [Id "FALSE"; Id "TRUE"]) -> ()
    | Typ3 (nam, id_lst) -> List.iter (fun _ -> ()) id_lst
    | Typ2 (nam, _, id :: []) ->  update typhash nam (Vtyp nam);
        let s = vexpr typhash id in update typhash s Vsigtyp; fprintf fd "    signal %s : %s;\n" s nam
    | Typ2 (nam, _, id_lst) ->  update typhash nam (Vtyp nam);
        List.iter (fun itm -> let s = vexpr typhash itm in update typhash s Vsigtyp; fprintf fd "    signal %s : %s;\n" s nam) id_lst;
    | Typ7 (nam, Typ8 (Atom "packed", Deflt)) -> update typhash nam (Vtyp nam);
        fprintf fd "    signal %s : std_logic;\n" nam
    | TypEnum6 (nam, _, id_lst) -> update typhash nam (Venum nam);
        fprintf fd "    type %s is (%s);\n" nam (String.concat ", " (List.map (fun itm -> let s = vexpr typhash itm in update typhash s (Venum nam); s) id_lst))
    | oth -> unhand := Some oth; failwith "decl_template"

let othlst = ref []

let rec stmt_clause fd typhash = function
      | Itmlst lst -> List.iter (stmt_clause fd typhash) lst      
      | BeginBlock lst -> List.iter (stmt_clause fd typhash) lst      
      | If2 (condition, if_lst, else_lst) ->
  fprintf fd "        if (%s) then\n" (vexpr typhash condition);
    (match if_lst with BeginBlock if_lst -> List.iter (stmt_clause fd typhash) if_lst | _ -> stmt_clause fd typhash if_lst);       
  fprintf fd "        else\n";
    (match else_lst with BeginBlock else_lst -> List.iter (stmt_clause fd typhash) else_lst | _ -> stmt_clause fd typhash else_lst);
  fprintf fd "        end if;\n";
(*
      | Blocking (lhs, expr) -> asgn fd typhash expr lhs
      | NonBlocking (lhs, expr) -> asgn fd typhash expr lhs
*)
      | If1 _ as x -> iff_template fd typhash x
      | DeclLogic lst -> ()
      | CaseStmt (state, itmlst) ->
  fprintf fd "            case %s is\n" (vexpr typhash state);
          List.iter (case_clause fd typhash) itmlst;
  fprintf fd "            end case;\n";
      | Unknown (";",_) -> ()
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
    | If2 (Equals (Id rst, lev), if_lst, else_lst) ->
  fprintf fd "        if (%s = %s) then\n" rst (vexpr typhash lev);
    stmt_clause fd typhash if_lst;
  fprintf fd "        elsif (%s'event and %s = '1') then\n" clk clk;
    stmt_clause fd typhash else_lst;
  fprintf fd "        end if;\n";
    | If2 (Id rst, if_lst, else_lst) ->
  fprintf fd "        if (%s = %s) then\n" rst (vexpr typhash (Number (2, 1, 1, "1")));
    stmt_clause fd typhash if_lst;
  fprintf fd "        elsif (%s'event and %s = '1') then\n" clk clk;
    stmt_clause fd typhash else_lst;
  fprintf fd "        end if;\n";
    | If2 (Pling (Id rst), if_lst, else_lst) ->
  fprintf fd "        if (%s = %s) then\n" rst (vexpr typhash (Number (2, 1, 0, "1")));
    stmt_clause fd typhash if_lst;
  fprintf fd "        elsif (%s'event and %s = '1') then\n" clk clk;
    stmt_clause fd typhash else_lst;
  fprintf fd "        end if;\n";
    | If1 (Id cond, if_lst) ->
  fprintf fd "        if (%s'event and %s = '1') then\n" clk clk;
    stmt_clause fd typhash if_lst;
  fprintf fd "        end if;\n";

    | oth -> unhand := Some oth; failwith "sent_template"

let instance_template fd typhash typ params inst pinlst =
        fprintf fd "%s: %s" inst typ;
        if params <> [] then fprintf fd " generic map (%s)\n" (String.concat ", " (List.map (parm_dump typhash) params));
        fprintf fd " port map (\n\t%s\n\t);\n" (String.concat ",\n\t" (List.map (vexpr typhash) pinlst))
    
let proc_template fd typhash cnt = function
    | DeclReg _ -> ()
    | DeclLogic _ -> ()
    | AlwaysFF (At (EventOr (Pos clk, (Pos rst|Neg rst))), sent_lst) ->
  fprintf fd "    -- clocked process %d description goes here\n" !cnt;
  fprintf fd "    SEQ%d: process (%s, %s)\n" !cnt clk rst;
  incr cnt;
  fprintf fd "    begin\n";
  sent_template fd typhash clk sent_lst;       
  fprintf fd "    end process;\n";
  fprintf fd "\n";
    | AlwaysLegacy (At (Pos clk), sent_lst) ->
  fprintf fd "    -- clocked process %d description goes here\n" !cnt;
  fprintf fd "    SEQ%d: process (%s)\n" !cnt clk;
  incr cnt;
  fprintf fd "    begin\n";
  sent_template fd typhash clk sent_lst;       
  fprintf fd "    end process;\n";
  fprintf fd "\n";
    | AlwaysComb2 ( (*DepLst dep_lst,*) sent_lst) -> let dep_lst = [] in
  fprintf fd "    -- combinational process %d description goes here\n" !cnt;
  fprintf fd "    COMB%d: process (%s)\n" !cnt (String.concat ", " dep_lst);
  incr cnt;
  fprintf fd "    begin\n";
  stmt_clause fd typhash sent_lst;
  fprintf fd "    end process;\n";
  fprintf fd "\n";
  (* elaboration case *)
   | CaseStmt (Id id, (CaseItm (BeginBlock [] :: Unknown ("$error",_) :: Deflt :: []) :: [])) -> fprintf fd "-- elaboration case %s\n" id;
    | ContAsgn lst -> List.iter (function
      | Asgn1 (lhs, expr) -> asgn fd typhash expr lhs
      | oth -> unhand := Some oth; failwith "assign_template") lst
    | Iff _ -> ()
    | InstDecl (typ, params, lst) -> List.iter (function
        | InstNameParen1 (inst, [Itmlst pins]) -> instance_template fd typhash typ (match params with Itmlst lst :: _ -> lst | _ -> []) inst pins
        | oth -> unhand := Some oth; failwith "InstDecl") lst;
(*
    | TypEnum _ -> ()
*)
    | Typ2 (typ, _, typ_lst) -> ()
    | Typ3 _ -> ()
    | Typ7 _ -> ()
    | DeclLogic2 _ -> ()
    | LoopGen1 _ -> () (* placeholder *)
    | GenItem _ -> () (* placeholder *)
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
  List.iter (decl_template fd typhash modules complst cnt) (List.sort compare (List.filter (function InstDecl _ -> true | _ -> false) body_lst));
  List.iter (decl_template fd typhash modules complst cnt) (List.filter (function InstDecl _ -> false | _ -> true) body_lst);
  fprintf fd "begin\n";
List.iter (proc_template fd typhash cnt) body_lst;
  fprintf fd "\n";
  fprintf fd "end rtl;\n";
  fprintf fd "\n";
  fprintf fd "\n";
  | oth -> failwith "This template only handles modules"
