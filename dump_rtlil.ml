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
  | Vdot
  | Vstr of string
  | Vtyp of string
  | Vfun of string
  | Venum of string
  | Vintf of string

let unhand = ref None
let update typhash id expr =
  Hashtbl.replace typhash id expr

type rtlil_attr = {fn: rtlil_attr -> rw -> rw}

let rec recurs' (rtlil_attr:rtlil_attr) = function
  | Id id -> Id id
  | Expression rw -> Expression(recurs_itm rtlil_attr  rw)
  | Itmlst(rw_lst) -> Itmlst(recurs_lst rtlil_attr (rw_lst:rw list))
  | Unknown (str,lst) -> Unknown (str, recurs_lst rtlil_attr  lst)
  | In -> In
  | Out -> Out
  | Inout -> Inout
  | Modul(str1, rw_lst1, rw_lst2, rw_lst3) -> Modul(str1, recurs_lst rtlil_attr  rw_lst1, recurs_lst rtlil_attr  rw_lst2, recurs_lst rtlil_attr  rw_lst3)
  | DeclReg(rw_lst, str1_lst, rw_lst_lst) -> DeclReg(recurs_lst rtlil_attr  rw_lst, str1_lst,
    List.map (fun itm -> recurs_lst rtlil_attr  itm) rw_lst_lst)
  | NonBlocking(rw, rw2) -> NonBlocking(recurs_itm rtlil_attr  rw, recurs_itm rtlil_attr  rw2)
  | Query(rw, rw2, rw3) -> Query(recurs_itm rtlil_attr  rw, recurs_itm rtlil_attr  rw2, recurs_itm rtlil_attr  rw3)
  | Port(rw, str1, rw_lst1, rw_lst2) -> Port(recurs_itm rtlil_attr  rw, str1, recurs_lst rtlil_attr rw_lst1, recurs_lst rtlil_attr rw_lst2)
  | Pos(str1) -> Pos (str1)
  | Neg(str1) -> Neg (str1)
  | Edge(rw, rw2) -> Edge(recurs_itm rtlil_attr (rw), recurs_itm rtlil_attr (rw2))
  | Intgr(int1) -> Intgr int1
  | Number _ as n -> n
  | RedAnd(rw) -> RedAnd(recurs_itm rtlil_attr (rw))
  | RedOr(rw) -> RedOr(recurs_itm rtlil_attr (rw))
  | RedXor(rw) -> RedXor(recurs_itm rtlil_attr (rw))
  | UMinus(rw) -> UMinus(recurs_itm rtlil_attr (rw))
  | Pling(rw) -> Pling(recurs_itm rtlil_attr (rw))
  | Tilde(rw) -> Tilde(recurs_itm rtlil_attr (rw))
  | TildeAnd(rw) -> TildeAnd(recurs_itm rtlil_attr (rw))
  | TildeOr(rw) -> TildeOr(recurs_itm rtlil_attr (rw))
  | Equals(rw, rw2) -> Equals(recurs_itm rtlil_attr (rw), recurs_itm rtlil_attr (rw2))
  | NotEq(rw, rw2) -> NotEq(recurs_itm rtlil_attr (rw), recurs_itm rtlil_attr (rw2))
  | LtEq(rw, rw2) -> LtEq(recurs_itm rtlil_attr (rw), recurs_itm rtlil_attr (rw2))
  | GtEq(rw, rw2) -> GtEq(recurs_itm rtlil_attr (rw), recurs_itm rtlil_attr (rw2))
  | Less(rw, rw2) -> Less(recurs_itm rtlil_attr (rw), recurs_itm rtlil_attr (rw2))
  | Greater(rw, rw2) -> Greater(recurs_itm rtlil_attr (rw), recurs_itm rtlil_attr (rw2))
  | And(rw, rw2) -> And(recurs_itm rtlil_attr (rw), recurs_itm rtlil_attr (rw2))
  | And2(rw, rw2) -> And2(recurs_itm rtlil_attr (rw), recurs_itm rtlil_attr (rw2))
  | Or(rw, rw2) -> Or(recurs_itm rtlil_attr (rw), recurs_itm rtlil_attr (rw2))
  | Or2(rw, rw2) -> Or2(recurs_itm rtlil_attr (rw), recurs_itm rtlil_attr (rw2))
  | Xor(rw, rw2) -> Xor(recurs_itm rtlil_attr (rw), recurs_itm rtlil_attr (rw2))
  | Xnor(rw, rw2) -> Xnor(recurs_itm rtlil_attr (rw), recurs_itm rtlil_attr (rw2))
  | Shiftl(rw, rw2) -> Shiftl(recurs_itm rtlil_attr (rw), recurs_itm rtlil_attr (rw2))
  | Shiftr(rw, rw2) -> Shiftr(recurs_itm rtlil_attr (rw), recurs_itm rtlil_attr (rw2))
  | Shiftr3(rw, rw2) -> Shiftr3(recurs_itm rtlil_attr (rw), recurs_itm rtlil_attr (rw2))
  | Add(rw, rw2) -> Add(recurs_itm rtlil_attr (rw), recurs_itm rtlil_attr (rw2))
  | Sub(rw, rw2) -> Sub(recurs_itm rtlil_attr (rw), recurs_itm rtlil_attr (rw2))
  | Mult(rw, rw2) -> Mult(recurs_itm rtlil_attr (rw), recurs_itm rtlil_attr (rw2))
  | Div(rw, rw2) -> Div(recurs_itm rtlil_attr (rw), recurs_itm rtlil_attr (rw2))
  | StarStar(rw, rw2) -> StarStar(recurs_itm rtlil_attr (rw), recurs_itm rtlil_attr (rw2))
  | If2(rw, rw2, rw3) -> If2(recurs_itm rtlil_attr (rw), recurs_itm rtlil_attr (rw2), recurs_itm rtlil_attr (rw3))
  | If1(rw, rw2) -> If1(recurs_itm rtlil_attr (rw), recurs_itm rtlil_attr (rw2))
  | ForLoop(rw_lst, rw2, rw3, rw4) ->
    ForLoop(recurs_lst rtlil_attr (rw_lst), recurs_itm rtlil_attr (rw2), recurs_itm rtlil_attr (rw3), recurs_itm rtlil_attr (rw4))
  | CaseStmt(rw_lst, rw_lst') -> CaseStmt(recurs_lst rtlil_attr rw_lst, recurs_lst rtlil_attr rw_lst')
  | CaseItm(rw_lst) -> CaseItm(recurs_lst rtlil_attr (rw_lst))
  | AlwaysComb(rw_lst) -> AlwaysComb(recurs_lst rtlil_attr (rw_lst))
  | BeginBlock(rw_lst) -> BeginBlock(recurs_lst rtlil_attr (rw_lst))
  | Bitlst(rw_lst) -> Bitlst(recurs_lst rtlil_attr (rw_lst))
  | Dot1(str1, rw2) -> Dot1(str1, recurs_itm rtlil_attr (rw2))
  | Unsigned(rw) -> Unsigned(recurs_itm rtlil_attr (rw))
  | Signed(rw) -> Signed(recurs_itm rtlil_attr (rw))
  | Concat(rw_lst) -> Concat(recurs_lst rtlil_attr (rw_lst))
  | WireExpr(str1, rw2) -> WireExpr(str1, recurs_itm rtlil_attr (rw2))
  | DeclModPort(rw_lst) -> DeclModPort(recurs_lst rtlil_attr (rw_lst))
  | Repl(rw, rw_lst) -> Repl(recurs_itm rtlil_attr (rw), recurs_lst rtlil_attr (rw_lst))
  | Dot3(str1, str2, str3) -> Dot3(str1, str2, str3)
  | Logic(rw_lst, rw_lst2) -> Logic(recurs_lst rtlil_attr (rw_lst), recurs_lst rtlil_attr (rw_lst2))
  | Param(str1, rw2, rw_lst) -> Param(str1, recurs_itm rtlil_attr (rw2), recurs_lst rtlil_attr (rw_lst))
  | DeclLogic(rw_lst) -> DeclLogic(recurs_lst rtlil_attr (rw_lst))
  | DeclData(rw1, rw_lst2) -> DeclData(recurs_itm rtlil_attr (rw1), recurs_lst rtlil_attr (rw_lst2))
  | GenBlock(rw_lst) -> GenBlock(recurs_lst rtlil_attr (rw_lst))
  | PackageBody (id, rw_lst) -> PackageBody (id, recurs_lst rtlil_attr  rw_lst)
  | Typ1 _ as x -> x
  | Cast (_, _) as x -> x
  | Deflt -> Deflt
  | (Assert|AssertProperty|AtStar|BreakSemi|PropertySpec|AlwaysComb2 _|
AlwaysFF (_, _)|AlwaysLatch _|AlwaysLegacy (_, _)|And3 (_, _)|
AnyRange (_, _)|Asgn1 (_, _)|AsgnPat _|At _|Atom _|AutoFunDecl (_, _, _)|
CaseStart (_, _)|CaseStart1 _|CaseStartInside (_, _)|CaseStartUniq (_, _)|
CaseStartUniqInside (_, _)|CellParamItem1 (_, _)|CellParamItem2 (_, _)|
CellParamItem3 (_, _)|CellPinItem1 (_, _)|CellPinItem2 (_, _)|
CellPinItemImplied _|CellPinItemNC _|CondGen1 (_, _, _)|ContAsgn _|
DeclAsgn (_, _)|DeclInt2 _|DeclLogic2 (_, _)|DeclReg2 (_, _)|
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
IdArrayedHyphenColon (_, _, _)|IdArrayedPlusColon (_, _, _)|
Iff (_, _)|Import _|Inc _|InitPair (_, _)|InitPat _|
InitSig (_, _)|Initial _|InsideCase (_, _)|InsideRange (_, _)|
InstDecl (_, _, _)|InstNameParen1 (_, _)|
InstNameParen2 (_, _)|InstRange (_, _)|IntfDecl (_, _, _, _)|ItemAsgn _|
LocalParamTyp _|LoopGen1 (_, _, _, _, _, _)|LtGt (_, _)|Mod (_, _)|
ModPortItm (_, _)|Nand (_, _)|NetDecl (_, _)|Nor (_, _)|NotEq3 (_, _)|
NotEqQuery (_, _)|OpenRange _|ParamAsgn1 (_, _)|ParamAsgn2 (_, _, _)|
ParamDecl (_, _)|ParamPort _|PatMember1 (_, _)|PatMemberDflt _|PkgImport _|
PkgImportItm (_, _)|PortDir (_, _)|PortFront (_, _)|PortItem (_, _)|
PortItemFront (_, _)|PortsStar _|Return _|SUDecl (_, _)|SUMember (_, _)|Seq (_, _)|
SideEffect (_, _)|Blocking _|String _|Sys (_, _)|SysFuncCall (_, _)|
SysTaskCall (_, _)|TaskBody (_, _)|TaskRef (_, _)|Typ2 (_, _, _)|Typ3 (_, _)|
Typ4 (_, _, _, _)|Typ5 (_, _)|Typ6 _|Typ7 (_, _)|Typ8 (_, _)|Typ9 (_, _, _)|
Typ10 (_, _, _)|TypEnum3 _|TypEnum4 (_, _, _)|TypEnum5 _|TypEnum6 (_, _, _)|
TypParam (_, _, _)|UPlus _|Union (_, _)|VNum _|ValueRange (_, _)|
VarDeclAsgn (_, _)|VarDim _|While (_, _)|PackageParam _|PackageParam2 _ as x) -> x

and recurs_lst rtlil_attr x = List.map (rtlil_attr.fn rtlil_attr ) x

and recurs_itm rtlil_attr  x = rtlil_attr .fn rtlil_attr  x

let rec recurs (rtlil_attr:rtlil_attr) = function
  | If2(Expression rw, rw2, rw3) -> If2(recurs_itm rtlil_attr (rw), recurs_itm rtlil_attr (rw2), recurs_itm rtlil_attr (rw3))
  | Or (Expression (Or _ as x), expr) -> Or (recurs_itm rtlil_attr x, recurs_itm rtlil_attr expr)
  | Or (expr, Expression (Or _ as x)) -> Or (recurs_itm rtlil_attr expr, recurs_itm rtlil_attr x)
  | And2 (Expression (And2 _ as x), expr) -> And2 (recurs_itm rtlil_attr x, recurs_itm rtlil_attr expr)
  | And2 (expr, Expression (And2 _ as x)) -> And2 (recurs_itm rtlil_attr expr, recurs_itm rtlil_attr x)
  | Or (expr, (And _ as x)) -> Or (recurs_itm rtlil_attr expr, Expression (recurs_itm rtlil_attr x))
  | Or (And2 _ as x, expr) -> Or (Expression (recurs_itm rtlil_attr x), recurs_itm rtlil_attr expr)
  | oth -> recurs' {fn=recurs} oth

let rec obin w n = 
  (if w > 1 then obin (w-1) (n lsr 1) else "")^string_of_int (n land 1)

let vlst = ref []
let coth = ref None
let eqlst = ref []
let clst = ref []

let ceval' = function
| Vint n -> n
| Std_logic -> print_endline ("Type std_logic evaluated to zero"); 0
| Vtyp s -> print_endline ("Type "^s^" evaluated to zero"); 0
| oth -> coth := Some oth; failwith "ceval'"

let clog2 n = if n = 0 then 0 else int_of_float(ceil(log(float_of_int n)/.log 2.))

let rec ceval typhash = function
| Intgr n -> n
| Number (_,_,n,_) -> n
| Id s -> ceval' (match Hashtbl.find_opt typhash s with Some x -> x | None -> print_endline ("Not found: "^s); Vint 0)
| Add (lhs, rhs) -> ceval typhash lhs + ceval typhash rhs
| Sub (lhs, rhs) -> ceval typhash lhs - ceval typhash rhs
| Mult (lhs, rhs) -> ceval typhash lhs * ceval typhash rhs
| Div (lhs, rhs) -> let divisor = ceval typhash rhs in if divisor = 0 then ceval typhash lhs else ceval typhash lhs / divisor
| Equals (lhs, rhs) -> if ceval typhash lhs = ceval typhash rhs then 1 else 0
| Greater (lhs, rhs) -> if ceval typhash lhs > ceval typhash rhs then 1 else 0
| Query (lhs, rhs, rhs') -> if ceval typhash lhs <> 0 then ceval typhash rhs else  ceval typhash rhs'
| StarStar (lhs, rhs) -> int_of_float(float_of_int (ceval typhash lhs) ** float_of_int(ceval typhash rhs))
| Sys ("$clog2", x) -> clog2 (ceval typhash x)
| Expression x -> ceval typhash x
| PackageBody (pkg, [id]) -> ceval typhash id (* placeholder *)
| Sys ("$bits", Typ1 id_t) -> csiz' typhash (match Hashtbl.find_opt typhash id_t with Some x -> x | None -> print_endline ("Not found: "^id_t); Std_logic)
| Sys ("$size", Dot1 _) -> 1 (* placeholder *)
| oth -> unhand := Some oth; failwith "ceval"

and csiz' typhash = function
| Vint n -> 32
| Std_logic -> 1
| Std_logic_vector(hi,lo) -> ceval typhash hi - ceval typhash lo + 1
| Vtyp s -> print_endline ("Type "^s^" evaluated to zero"); 0
| oth -> coth := Some oth; failwith "ceval'"

let is_const' = function
| Vint n -> true
| Venum _ -> true
| Std_logic -> false
| Std_logic_vector _ -> false
| oth -> coth := Some oth; failwith "is_const'"

let rec is_const typhash = function
| Intgr n -> true
| Number _ -> true
| Id s -> (match Hashtbl.find_opt typhash s with Some x -> is_const' x | None -> false)
| Add (lhs, rhs) -> is_const typhash lhs && is_const typhash rhs
| Sub (lhs, rhs) -> is_const typhash lhs && is_const typhash rhs
| Query(lhs, rhs, rhs') -> is_const typhash rhs && is_const typhash rhs'
| Sys ("$clog2", x) -> is_const typhash x
| Sys ("$unsigned", x) -> is_const typhash x
| Expression x -> is_const typhash x
| Unsigned x -> is_const typhash x
| PackageBody (pkg, id :: _) -> is_const typhash id
| Dot1 ((Id _ | IdArrayed2 _), _) -> false
| IdArrayedColon _ -> false
| oth -> unhand := Some oth; failwith "is_const"

let rec width typhash = function
| Intgr n -> 32
| Number (_,w,_,_) -> w
| Id s -> csiz' typhash (match Hashtbl.find_opt typhash s with Some x -> x | None -> print_endline ("Not found: "^s); Std_logic)
| Add (lhs, rhs) -> max (width typhash lhs) (width typhash rhs)
| Sub (lhs, rhs) -> max (width typhash lhs) (width typhash rhs)
| Mult (lhs, rhs) -> width typhash lhs + width typhash rhs
| Div (lhs, rhs) -> width typhash lhs
| And (lhs, rhs) -> min (width typhash lhs) (width typhash rhs)
| And2 _ -> 1
| Or (lhs, rhs) -> max (width typhash lhs) (width typhash rhs)
| Or2 _ -> 1
| Xor (lhs, rhs) -> max (width typhash lhs) (width typhash rhs)
| Less (lhs, rhs) -> 1
| LtEq (lhs, rhs) -> 1
| Equals (lhs, rhs) -> 1
| NotEq (lhs, rhs) -> 1
| GtEq (lhs, rhs) -> 1
| Greater (lhs, rhs) -> 1
| Query (lhs, rhs, rhs') -> max (width typhash rhs) (width typhash rhs')
| StarStar (lhs, rhs) -> (width typhash lhs) + ceval typhash rhs
| Sys ("$clog2", x) -> clog2 (width typhash x)
| Expression x -> width typhash x
| PackageBody (pkg, [id]) -> width typhash id (* placeholder *)
| Sys ("$bits", Typ1 id_t) -> csiz' typhash (match Hashtbl.find_opt typhash id_t with Some x -> x | None -> print_endline ("Not found: "^id_t); Std_logic)
| Sys ("$size", Dot1 _) -> 1 (* placeholder *)
| oth -> unhand := Some oth; failwith "width"

let rec vexpr typhash = function
| Id s -> s
| Expression x -> " ( " ^ (vexpr typhash) x ^ " ) "
| Number (2,w,n,_) -> string_of_int w^"'b"^obin w n
| Number (10,w,n,s) -> string_of_int n
| Number (16,w,n,_) -> sprintf "%d'h%x" w n
| Number (b,w,n,s) -> s
| Intgr n -> string_of_int n
| Tilde expr -> "not " ^ (vexpr typhash) expr
| Pling expr -> "not " ^ (vexpr typhash) expr
| Concat lst -> String.concat " & " (List.map (vexpr typhash) lst)
| Add ((Id s as lhs), (Intgr n as rhs)) -> (match Hashtbl.find typhash s with
    | Std_logic_vector(hi', lo') -> let hi = ceval typhash hi' and lo = ceval typhash lo' in vexpr' typhash lhs ^ " + " ^ vexpr' typhash (Number(2, hi-lo+1, n, string_of_int n))
    | oth -> vexpr' typhash lhs ^ " + " ^ vexpr' typhash rhs)
| Add (lhs, rhs) -> vexpr' typhash lhs ^ " + " ^ vexpr' typhash rhs
| Sub ((Id s as lhs), (Intgr n as rhs)) -> (match Hashtbl.find typhash s with
    | Std_logic_vector(hi', lo') -> let hi = ceval typhash hi' and lo = ceval typhash lo' in vexpr' typhash lhs ^ " - " ^ vexpr' typhash (Number(2, hi-lo+1, n, string_of_int n))
    | oth -> vexpr' typhash lhs ^ " - " ^ vexpr' typhash rhs)
| Mult (lhs, rhs) -> vexpr' typhash lhs ^ " * " ^ vexpr' typhash rhs
| StarStar (lhs, rhs) -> vexpr' typhash lhs ^ " ** " ^ vexpr' typhash rhs
| Div (lhs, rhs) -> vexpr' typhash lhs ^ " * " ^ vexpr' typhash rhs
| Sub (lhs, rhs) -> vexpr' typhash lhs ^ " - " ^ vexpr' typhash rhs
| LtEq (lhs, rhs) -> vexpr' typhash lhs ^ " <= " ^ vexpr' typhash rhs
| UMinus (rhs) -> " - " ^ vexpr' typhash rhs
| Equals (lhs, (Number(_,1,_,_) as rhs)) -> (vexpr typhash) lhs ^ " = " ^ (vexpr typhash) rhs
| Equals ((Id s as lhs), (Intgr n as rhs)) -> (match Hashtbl.find typhash s with
    | Std_logic_vector(hi', lo') -> let hi = ceval typhash hi' and lo = ceval typhash lo' in vexpr' typhash lhs ^ " = " ^ vexpr' typhash (Number(2, hi-lo+1, n, string_of_int n))
    | oth -> vexpr' typhash lhs ^ " - " ^ vexpr' typhash rhs)
| Equals ((Id s as lhs), (Number _ as rhs)) -> (match Hashtbl.find_opt typhash s with
    | Some (Std_logic_vector _) -> vexpr' typhash lhs ^ " = " ^ vexpr' typhash rhs
    | _ -> vexpr' typhash lhs ^ " = " ^ vexpr typhash rhs)
| Equals ((Id s as lhs), (Id _ as rhs)) -> (match Hashtbl.find_opt typhash s with
    | Some (Std_logic_vector _) -> vexpr' typhash lhs ^ " = " ^ vexpr' typhash rhs
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
| Less (lhs, rhs) -> vexpr' typhash lhs ^ " < " ^ vexpr' typhash rhs
| Greater (lhs, rhs) -> vexpr' typhash lhs ^ " > " ^ vexpr' typhash rhs
| Or (lhs, rhs) -> (vexpr typhash) lhs ^ " or " ^ (vexpr typhash) rhs
| Or2 (lhs, rhs) -> (vexpr typhash) lhs ^ " or " ^ (vexpr typhash) rhs
| Xor (lhs, rhs) -> (vexpr typhash) lhs ^ " xor " ^ (vexpr typhash) rhs
| And (lhs, rhs) -> (vexpr typhash) lhs ^ " and " ^ (vexpr typhash) rhs
| And2 (lhs, rhs) -> (vexpr typhash) lhs ^ " and " ^ (vexpr typhash) rhs
| Unsigned expr -> "unsigned("^vexpr typhash expr^")"
| Shiftl (lhs, rhs) -> "shift_left("^vexpr typhash lhs^", "^vexpr typhash rhs^")"
| Shiftr (lhs, rhs) -> "shift_right("^vexpr typhash lhs^", "^vexpr typhash rhs^")"
| Shiftr3 (lhs, rhs) -> "shift_right3("^vexpr typhash lhs^", "^vexpr typhash rhs^")"
| CellPinItem1 (port, conn) -> port ^ " => " ^ conn
| CellPinItem2 (port, expr) -> port ^ " => " ^ vexpr typhash expr
| CellPinItemImplied (port) -> port ^ " => " ^ port
| CellPinItemNC (port) -> port ^ " => open"
| Query (Id cond', ctrue', cfalse') -> sprintf "%s ? %s : %s" cond' (vexpr typhash ctrue') (vexpr typhash cfalse')
| Query (cond', ctrue', cfalse') -> sprintf "%s ? %s : %s" (vexpr typhash cond') (vexpr typhash ctrue') (vexpr typhash cfalse')
| Deflt -> "open"
| Dot1(lft, rght) -> vexpr typhash lft^"."^vexpr typhash rght
| RedOr (lhs) -> " or (" ^ vexpr typhash lhs ^ ")"
| RedAnd (lhs) -> " and (" ^ vexpr typhash lhs ^ ")"
| RedXor (lhs) -> " xor (" ^ vexpr typhash lhs ^ ")"
| TildeOr (lhs) -> " nor (" ^ vexpr typhash lhs ^ ")"
| IdArrayed2 (id, ix) -> vexpr typhash id^"["^vexpr typhash ix^"]"
| IdArrayed3 (PackageBody (pkg, []) :: [], arr) -> pkg^"::"^vexpr typhash arr
| IdArrayedColon (id, expr, expr') -> vexpr typhash id^"["^vexpr typhash expr^" : "^vexpr typhash expr'^"]"
| IdArrayedPlusColon (id, expr, expr') -> vexpr typhash id^"["^vexpr typhash expr^" : "^vexpr typhash expr'^"]"
| IdArrayedHyphenColon (id, expr, expr') -> vexpr typhash id^"["^vexpr typhash expr^" : "^vexpr typhash expr'^"]"
| FunRef (fn, arglst) -> fn^"("^String.concat ", " (List.map (vexpr typhash) arglst)^")"
| FunRef2 (fn, _, arglst) -> fn^"("^String.concat ", " (List.map (vexpr typhash) arglst)^")"
| AsgnPat [PatMemberDflt (Number _ as expr)] -> "others => "^(vexpr typhash expr)
| Repl (expr', [expr]) -> "{{"^(vexpr typhash expr')^"}{"^(vexpr typhash expr)^"}}"
| InsideRange (first, last) -> "inside_range("^(vexpr typhash first)^", "^(vexpr typhash last)^")"
| OpenRange lst -> "open_range("^String.concat ", " (List.map (vexpr typhash) lst)^")"
| ExprOKL lst -> " {" ^ String.concat ", " (List.map (vexpr typhash) lst) ^ "} "
| PackageBody (pkg, [Id id]) -> pkg^"::"^id
| ExprQuote1(lhs, rhs) -> vexpr typhash lhs^"'("^vexpr typhash rhs^")"
| Sys (sys_id, arglst) -> let args = (match arglst with
        | Itmlst lst -> String.concat ", " (List.map (vexpr typhash) lst)
	| oth -> vexpr typhash oth) in
    String.sub sys_id 1 (String.length sys_id - 1) ^ "(" ^ args ^")"
| Typ3 (id_t, [PackageBody (pkg, [])]) -> pkg^"::"^id_t
| PackageBody (pkg, []) -> pkg^"::"
| Atom ".*" -> "" (* placeholder *)
| Atom kind -> kind (* placeholder *)
| Typ1 id_t -> id_t
| AsgnPat lst -> String.concat "; " (List.map (vexpr typhash) lst)
| PatMember1 (Id id, expr) -> id ^ " = " ^ vexpr typhash expr
| PatMemberDflt expr -> vexpr typhash expr
| ValueRange (lft, rght) -> "["^vexpr typhash lft^" .. "^vexpr typhash rght^"]"
| String s -> s
| oth -> unhand := Some oth; failwith "vexpr"

and vexpr' typhash = function
| (Intgr _ | Number _) as x -> "(unsigned'(" ^ (vexpr typhash) x ^ ")) "
| (Id s) as x -> (match Hashtbl.find_opt typhash s with
    | Some (Vint _|Vtyp _|Venum _|Vsigtyp|Vpkg _|Vfun _|Vintf _|Vstr _|Vdot) -> vexpr typhash x
    | Some Std_logic -> vexpr typhash x
    | Some Std_logic_vector _ -> "unsigned (" ^ vexpr typhash x ^ ") "
    | None -> print_endline ("not found: "^s); s)
| x -> "unsigned(" ^ vexpr typhash x ^ ") "

and cexpr typhash = function
    | Id s -> s
    | Intgr n -> string_of_int n
    | Number (_,_,n,_) -> string_of_int n
    | Add (lhs, rhs) -> cexpr typhash lhs ^ "+" ^ cexpr typhash rhs
    | Sub (lhs, rhs) -> cexpr typhash lhs ^ "-" ^ cexpr typhash rhs
    | Mult (lhs, rhs) -> cexpr typhash lhs ^ "*" ^ cexpr typhash rhs
    | Div (lhs, rhs) -> cexpr typhash lhs ^ "/" ^ cexpr typhash rhs
    | Or (lhs, rhs) -> cexpr typhash lhs ^ " | " ^ cexpr typhash rhs
    | Or2 (lhs, rhs) -> cexpr typhash lhs ^ " || " ^ cexpr typhash rhs
    | Equals (lhs, rhs) -> cexpr typhash lhs ^ " == " ^ cexpr typhash rhs
    | Query (lhs, rhs, rhs') -> cexpr typhash lhs ^ " ? " ^ cexpr typhash rhs ^ " : " ^ cexpr typhash rhs
    | Greater (lhs, rhs) -> cexpr typhash lhs ^ " > " ^ cexpr typhash rhs
    | LtEq (lhs, rhs) -> cexpr typhash lhs ^ " <= " ^ cexpr typhash rhs
    | Less (lhs, rhs) -> cexpr typhash lhs ^ " < " ^ cexpr typhash rhs
    | Shiftl (lhs, rhs) -> cexpr typhash lhs ^ " << " ^ cexpr typhash rhs
    | Shiftr (lhs, rhs) -> cexpr typhash lhs ^ " >> " ^ cexpr typhash rhs
    | Tilde rhs -> "~"  ^ cexpr typhash rhs
    | Sys ("$clog2", _) as expr -> string_of_int (ceval typhash expr)
    | Sys ("$bits", _) -> "1" (* placeholder *)
    | Sys ("$size", _) -> "1" (* placeholder *)
    | Sys ("$random", _) -> "1" (* placeholder *)
    | Dot1 (port, conn) -> (cexpr typhash port) ^ " => " ^ (cexpr typhash conn)
    | Expression x -> cexpr typhash x
    | StarStar (lhs, rhs) -> cexpr typhash lhs ^ "+" ^ cexpr typhash rhs
    | PackageBody (pkg, [Id id]) -> pkg^"::"^id
    | Concat lst -> "{" ^ String.concat ", " (List.map (cexpr typhash) lst) ^ "}"
    | ExprOKL lst -> "{" ^ String.concat ", " (List.map (cexpr typhash) lst) ^ "}"
    | Repl (expr, expr' :: []) -> "{{"^cexpr typhash expr^"} {"^cexpr typhash expr^"}}"
    | FunRef2 (fid, [PackageBody (pkg, [])], arglst) -> pkg^"::"^fid^"("^String.concat ", " (List.map (cexpr typhash) arglst)^")"
    | ExprQuote1 (Atom typ, arg) -> "("^typ^")"^cexpr typhash arg
    | oth -> unhand := Some oth; failwith "cexpr"

let funtyp typhash = function
| Atom primtyp -> primtyp
| Typ1 id_t -> id_t
| Typ3 (id_t, [PackageBody (pkg, [])]) -> pkg^"::"^id_t
| Typ5 (Atom primtyp, AnyRange (lft, rght) :: []) -> sprintf "%s [%s:%s]" primtyp (cexpr typhash lft) (cexpr typhash rght)
| Typ6 (Atom primtyp) -> primtyp
| Typ8 (Atom kind, Atom kind') -> kind' ^ kind
| Typ8 (Atom kind, Deflt) -> kind
| oth -> unhand := Some oth; failwith "funtyp"

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

let id_ix = ref 0

let newnam () = 
  incr id_ix;
  "$Id$"^string_of_int !id_ix

let newid buf' typhash wid =
  let nam = newnam() in
  let rslt = Id nam in
  update typhash nam (Std_logic_vector(Intgr (wid-1),Intgr 0));
  bprintf buf' "  wire width %d \\%s\n" wid nam;
  rslt

let rec asgnexpr' buf' initlst typhash wid = function
  | (Number _ | Intgr _) as x -> x
  | Add (lhs, rhs) -> dyadic buf' initlst typhash wid "add" [lhs;rhs] "ABY"
  | Sub (lhs, rhs) -> dyadic buf' initlst typhash wid "sub" [lhs;rhs] "ABY"
  | Mult (lhs, rhs) -> dyadic buf' initlst typhash wid "mul" [lhs;rhs] "ABY"
  | Div (lhs, rhs) -> dyadic buf' initlst typhash wid "div" [lhs;rhs] "ABY"
  | And (lhs, rhs) -> dyadic buf' initlst typhash wid "and" [lhs;rhs] "ABY"
  | And2 (lhs, rhs) -> dyadic buf' initlst typhash wid "logic_and" [lhs;rhs] "ABY"
  | Or (lhs, rhs) -> dyadic buf' initlst typhash wid "or" [lhs;rhs] "ABY"
  | Or2 (lhs, rhs) -> dyadic buf' initlst typhash wid "logic_or" [lhs;rhs] "ABY"
  | Xor (lhs, rhs) -> dyadic buf' initlst typhash wid "xor" [lhs;rhs] "ABY"
  | Less (lhs, rhs) -> dyadic buf' initlst typhash wid "lt" [lhs;rhs] "ABY"
  | LtEq (lhs, rhs) -> dyadic buf' initlst typhash wid "le" [lhs;rhs] "ABY"
  | Equals (lhs, rhs) -> dyadic buf' initlst typhash wid "eq" [lhs;rhs] "ABY"
  | NotEq (lhs, rhs) -> dyadic buf' initlst typhash wid "ne" [lhs;rhs] "ABY"
  | GtEq (lhs, rhs) -> dyadic buf' initlst typhash wid "ge" [lhs;rhs] "ABY"
  | Greater (lhs, rhs) -> dyadic buf' initlst typhash wid "gt" [lhs;rhs] "ABY"
  | Id s -> Id s
  | Expression x -> asgnexpr buf' initlst typhash x
  | Tilde expr -> let rhs = asgnexpr buf' initlst typhash expr and rslt = newid buf' typhash wid in addprim buf' initlst typhash "$tilde" [] [rhs;rslt] "AY"; rslt
(*
  | Pling expr -> "not " ^ (asgnexpr buf' initlst typhash) expr
  | Concat lst -> String.concat " & " (List.map (asgnexpr buf' initlst typhash) lst)
  | StarStar (lhs, rhs) -> asgnexpr buf' initlst typhash lhs ^ " ** " ^ asgnexpr buf' initlst typhash rhs
  | UMinus (rhs) -> " - " ^ asgnexpr buf' initlst typhash rhs
  | Unsigned expr -> "unsigned("^asgnexpr buf' initlst typhash expr^")"
  | Shiftl (lhs, rhs) -> "shift_left("^asgnexpr buf' initlst typhash lhs^", "^asgnexpr buf' initlst typhash rhs^")"
  | Shiftr (lhs, rhs) -> "shift_right("^asgnexpr buf' initlst typhash lhs^", "^asgnexpr buf' initlst typhash rhs^")"
  | Shiftr3 (lhs, rhs) -> "shift_right3("^asgnexpr buf' initlst typhash lhs^", "^asgnexpr buf' initlst typhash rhs^")"
  | CellPinItem1 (port, conn) -> port ^ " => " ^ conn
  | CellPinItem2 (port, expr) -> port ^ " => " ^ asgnexpr buf' initlst typhash expr
  | CellPinItemImplied (port) -> port ^ " => " ^ port
  | CellPinItemNC (port) -> port ^ " => open"
  | Query (Id cond', ctrue', cfalse') -> sprintf "%s ? %s : %s" cond' (asgnexpr buf' initlst typhash ctrue') (asgnexpr buf' initlst typhash cfalse')
  | Query (cond', ctrue', cfalse') -> sprintf "%s ? %s : %s" (asgnexpr buf' initlst typhash cond') (asgnexpr buf' initlst typhash ctrue') (asgnexpr buf' initlst typhash cfalse')
  | Deflt -> "open"
  | Dot1(lft, rght) -> asgnexpr buf' initlst typhash lft^"."^asgnexpr buf' initlst typhash rght
  | RedOr (lhs) -> " or (" ^ asgnexpr buf' initlst typhash lhs ^ ")"
  | RedAnd (lhs) -> " and (" ^ asgnexpr buf' initlst typhash lhs ^ ")"
  | RedXor (lhs) -> " xor (" ^ asgnexpr buf' initlst typhash lhs ^ ")"
  | TildeOr (lhs) -> " nor (" ^ asgnexpr buf' initlst typhash lhs ^ ")"
  | IdArrayed2 (id, ix) -> asgnexpr buf' initlst typhash id^"["^asgnexpr buf' initlst typhash ix^"]"
  | IdArrayed3 (PackageBody (pkg, []) :: [], arr) -> pkg^"::"^asgnexpr buf' initlst typhash arr
  | IdArrayedColon (id, expr, expr') -> asgnexpr buf' initlst typhash id^"["^asgnexpr buf' initlst typhash expr^" : "^asgnexpr buf' initlst typhash expr'^"]"
  | IdArrayedPlusColon (id, expr, expr') -> asgnexpr buf' initlst typhash id^"["^asgnexpr buf' initlst typhash expr^" : "^asgnexpr buf' initlst typhash expr'^"]"
  | IdArrayedHyphenColon (id, expr, expr') -> asgnexpr buf' initlst typhash id^"["^asgnexpr buf' initlst typhash expr^" : "^asgnexpr buf' initlst typhash expr'^"]"
  | FunRef (fn, arglst) -> fn^"("^String.concat ", " (List.map (asgnexpr buf' initlst typhash) arglst)^")"
  | FunRef2 (fn, _, arglst) -> fn^"("^String.concat ", " (List.map (asgnexpr buf' initlst typhash) arglst)^")"
  | AsgnPat [PatMemberDflt (Number _ as expr)] -> "others => "^(asgnexpr buf' initlst typhash expr)
  | Repl (expr', [expr]) -> "{{"^(asgnexpr buf' initlst typhash expr')^"}{"^(asgnexpr buf' initlst typhash expr)^"}}"
  | InsideRange (first, last) -> "inside_range("^(asgnexpr buf' initlst typhash first)^", "^(asgnexpr buf' initlst typhash last)^")"
  | OpenRange lst -> "open_range("^String.concat ", " (List.map (asgnexpr buf' initlst typhash) lst)^")"
  | ExprOKL lst -> " {" ^ String.concat ", " (List.map (asgnexpr buf' initlst typhash) lst) ^ "} "
  | PackageBody (pkg, [Id id]) -> pkg^"::"^id
  | ExprQuote1(lhs, rhs) -> asgnexpr buf' initlst typhash lhs^"'("^asgnexpr buf' initlst typhash rhs^")"
  | Sys (sys_id, arglst) -> let args = (match arglst with
	  | Itmlst lst -> String.concat ", " (List.map (asgnexpr buf' initlst typhash) lst)
	  | oth -> asgnexpr buf' initlst typhash oth) in
      String.sub sys_id 1 (String.length sys_id - 1) ^ "(" ^ args ^")"
  | Typ3 (id_t, [PackageBody (pkg, [])]) -> pkg^"::"^id_t
  | PackageBody (pkg, []) -> pkg^"::"
  | Atom ".*" -> "" (* placeholder *)
  | Atom kind -> kind (* placeholder *)
  | Typ1 id_t -> id_t
  | AsgnPat lst -> String.concat "; " (List.map (asgnexpr buf' initlst typhash) lst)
  | PatMember1 (Id id, expr) -> id ^ " = " ^ asgnexpr buf' initlst typhash expr
  | PatMemberDflt expr -> asgnexpr buf' initlst typhash expr
  | ValueRange (lft, rght) -> "["^asgnexpr buf' initlst typhash lft^" .. "^asgnexpr buf' initlst typhash rght^"]"
  | String s -> s
*)
  | oth -> unhand := Some oth; failwith "asgnexpr buf' initlst"

and asgnexpr buf' initlst typhash x = asgnexpr' buf' initlst typhash (width typhash x) x

and addprim buf' initlst typhash typ params args templ = 
  print_endline ("addprim: "^typ);
  let ex' arg = (asgnexpr buf' initlst typhash arg, width typhash arg) in
  let widths = List.mapi (fun ix itm -> CellParamItem2 (String.make 1 templ.[ix]^"_WIDTH", Number(10, 32, snd(ex' itm), ""))) args in
  initlst := InstDecl(typ, [Itmlst (params@widths)], (InstNameParen1 (newnam(), Itmlst (List.mapi (fun ix itm -> CellPinItem2(String.make 1 templ.[ix], itm)) args) :: []) :: [])) :: !initlst

and dyadic buf' initlst typhash wid func args pnam =
  let rslt = newid buf' typhash wid in
  addprim buf' initlst typhash func (List.map (fun itm -> CellParamItem2 (itm^"_SIGNED", Number (10, 32, 0, ""))) ["A";"B"]) (args@[rslt]) pnam;
  rslt

let sel_expr typhash x = match simplify x with Intgr _ -> "'0'" | oth -> vexpr typhash oth

let vdir ix = function
  | In -> sprintf "input %d " ix
  | Out -> sprintf "output %d " ix
  | Inout -> "inout"
  | _ -> failwith "vdir"

let ports' typhash ix hi lo dir nam = sprintf "  wire width %d %s \\%s" (ceval typhash hi - ceval typhash lo +1)  (vdir ix dir) nam
  
let ports typhash ix = function
    | Port((In|Out|Inout|Deflt) as dir, nam, [], []) -> update typhash nam Std_logic;
        sprintf "  wire %s \\%s" (vdir ix dir) nam;
    | Port(PortDir((In|Out|Inout) as dir, Atom "wire"), nam, [], []) -> update typhash nam Std_logic;
        sprintf "  wire %s \\%s" (vdir ix dir) nam;
    | Port ((In|Out|Inout) as dir, nam, AnyRange (hi, lo) :: [], []) -> update typhash nam (Std_logic_vector(hi,lo));
        ports' typhash ix hi lo dir nam              
    | Port (PortDir((In|Out|Inout) as dir, Atom "wire"), nam, AnyRange (hi, lo) :: [], []) -> update typhash nam (Std_logic_vector(hi,lo));
        ports' typhash ix hi lo dir nam              
    | Port ((In|Out|Inout) as dir, nam, AnyRange (hi, lo) :: AnyRange(hi', lo') :: [], []) -> update typhash nam (Std_logic_vector(hi,lo));
        ports' typhash ix hi lo dir nam              
    | Port ((In|Out|Inout) as dir, nam, AnyRange (hi, lo) :: AnyRange(hi', lo') :: AnyRange(hi'', lo'') :: [], []) -> update typhash nam (Std_logic_vector(hi,lo));
        ports' typhash ix hi lo dir nam              
    |  Port ((In|Out|Inout) as dir, nam, Typ2 (typ_e, (PackageBody (pkg, [])) :: [], []) :: [], []) -> update typhash nam (Vpkg(pkg, typ_e));
        sprintf "  wire %s \\%s" (vdir ix dir) nam;
    |  Port ((In|Out|Inout) as dir, nam, Typ2 (typ_e, (PackageBody (pkg, [])) :: [], []) :: AnyRange (hi, lo) :: [], []) -> update typhash nam (Std_logic_vector(hi, lo));
        ports' typhash ix hi lo dir nam              
    |  Port ((In|Out|Inout) as dir, nam, Typ2 (typ_t, [], []) :: [], []) -> update typhash nam (Vtyp(typ_t));
        sprintf "  wire %s \\%s" (vdir ix dir) nam;
    |  Port ((In|Out|Inout) as dir, nam, Typ2 (typ_t, [], []) :: AnyRange (hi, lo) :: [], []) -> update typhash nam (Vtyp(typ_t));
        ports' typhash ix hi lo dir nam              
    |  Port ((In|Out|Inout) as dir, nam, Typ6 (Atom primtyp) :: [], []) -> update typhash nam (Std_logic);
        sprintf "  wire %s \\%s" (vdir ix dir) nam;
    |  Port (Deflt, nam, Typ2 (typ_t, PackageBody (pkg, []) :: [], []) :: AnyRange (hi, lo) :: [], []) -> update typhash nam (Vtyp(typ_t));
        ports' typhash ix hi lo Inout nam              
    |  Dot3 (bus, dir, member) -> update typhash member (Vintf bus);
        sprintf "  wire %s %s \\%s" member bus dir
    |  DotBus (bus, dir, member, AnyRange(hi,lo) :: []) -> update typhash member (Vintf bus);
        ports' typhash ix hi lo Inout bus            
    | oth -> unhand := Some oth; failwith "component"

let rec parm_generic typhash = function
  | CellParamItem1 (nam, s) ->
      update typhash nam (match Hashtbl.find_opt typhash s with Some x -> x | None -> Std_logic);
      sprintf "%24s         : string := %s" nam s
  | CellParamItem2 (nam, Typ1 s) ->
      sprintf "%24s         : type := %s" nam s
  | CellParamItem2 (nam, Typ3(id_t, PackageBody (pkg,[]) :: [])) ->
      sprintf "%24s         : type := %s" nam id_t
  | CellParamItem2 (nam, Typ5(Atom "logic", AnyRange(lft,rght) :: [])) ->
      sprintf "%24s         : type := logic [%s : %s]" nam (cexpr typhash lft) (cexpr typhash rght)
  | CellParamItem2 (nam, PackageBody (s, Id id :: _)) ->
      sprintf "%24s         : type := %s" nam s
  | CellParamItem2 (nam, Dot1 (Id lft, Id rght)) ->
      sprintf "%24s         : type := %s.%s" nam lft rght
  | CellParamItem2 (nam, Number (_, _, n, _)) ->
      update typhash nam (Vint n); 
      sprintf "%24s         : integer := %d" nam n
  | CellParamItem2 (nam, ((Add _|Sub _|Mult _|Div _|Sys _) as x)) ->
      let n = ceval typhash x in
      update typhash nam (Vint n);
      sprintf "%24s         : integer := %d" nam n
  | CellParamItem3 (nam, Typ1(id_t)) ->
      sprintf "%24s         : type := %s" nam id_t
  | CellParamItem3 (nam, Typ3(id_t, PackageBody (pkg, []) :: [])) ->
      sprintf "%24s         : type := %s" nam id_t
  | Param (nam, Number (_, _, n, _), []) ->
      update typhash nam (Vint n);
      sprintf "%24s         : integer := %d" nam n
  | PackageParam2 (id_t, nam, [], Id s) ->
      update typhash nam (Vtyp id_t);
      sprintf "%24s         => %s" id_t s
  | PackageParam2 (grp_e, nam, [PackageBody (pkg, [])], PackageBody (pkg', [Id s])) ->
      update typhash nam (Venum grp_e);
      sprintf "%24s         => %s" grp_e s
  | PackageParam2 (id_t, nam, [PackageBody (pkg, [])], Number (_, _, n, _)) ->
      update typhash nam (Vint n);
      sprintf "%24s         => %d" nam n
  | PackageParam2 (id_t, nam, [PackageBody (pkg, [])], AsgnPat [PatMemberDflt (Number (_, _, n, ""))]) ->
      update typhash nam (Vint n);
      sprintf "%24s         => %d" nam n
  | PackageParam2 (id_t, nam, [PackageBody (pkg, [])], AsgnPat [PatMemberDflt (PackageBody (pkg', [Id id]))]) ->
      update typhash nam (Vtyp id_t);
      sprintf "%24s         => %s" nam id
  | PackageParam2 (id_t, nam, [PackageBody (pkg, [])], ExprQuote1 (Typ3(id, PackageBody (pkg', []) :: []), expr)) ->
      update typhash nam (Vtyp id_t);
      sprintf "%24s         => %s" nam id
  | TypParam (nam, Atom typ, []) ->
      update typhash nam (Vtyp nam); 
      sprintf "%24s         => %s" nam typ
  | TypParam (nam, Id id_t, PackageBody (pkg, []) :: []) ->
      update typhash nam (Vtyp nam); 
      sprintf "%24s         => %s" nam id_t
  | TypParam (nam, Atom typ, AnyRange (lft, rght) :: []) ->
      update typhash nam (Std_logic_vector(lft,rght));
      sprintf "%24s         => %s[%s : %s]" nam typ (cexpr typhash lft) (cexpr typhash rght)
  | Param (nam, PackageBody (pkg, [Id id]), []) ->
      update typhash nam (Vtyp nam);
      sprintf "%24s         => %s" nam id
  | Param (nam, FunRef2 (fn, [PackageBody (pkg, [])], expr :: []), []) ->
      update typhash nam (Vfun fn);
      sprintf "%24s         => %s" nam (cexpr typhash expr)
  | Param (nam, String s, []) ->
      update typhash nam (Vstr s);
      sprintf "%24s         => %s" nam s
  | Param (nam, Dot1(lft,rght), []) ->
      update typhash nam (Vdot);
      sprintf "%24s         => %s.%s" nam (cexpr typhash lft) (cexpr typhash rght)
  | Param (nam, Number (_, _, n, _), AnyRange (left, rght) :: []) ->
      update typhash nam (Vint n);
      sprintf "%24s         => %d" nam n
  | Param (nam, UMinus (Number (_, _, n, _)), []) ->
      update typhash nam (Vint (-n));
      sprintf "%24s         => %d" nam (-n)
  | Param (nam, UMinus (Number (_, _, n, _)), AnyRange (lft, rght) :: []) ->
      update typhash nam (Vint (-n));
      sprintf "%24s         => %d" nam (-n)
  | Param (nam, ((Add _|Sub _|Mult _|Div _|StarStar _ |Sys _|Equals _|Query _) as x), []) ->
      let n = ceval typhash x in
      update typhash nam (Vint n);
      sprintf "%24s         => %d" nam n
 | PackageParam (lst, inner) -> String.concat ", " (List.map (function PkgImport (Itmlst [PkgImportItm (pkg, Atom "*")]) -> parm_generic typhash inner | _ -> "") lst)
  | oth -> unhand := Some oth; failwith "parm_generic"

let rec parm_map typhash = function  
  | CellParamItem1 (nam, s) ->
      update typhash nam (match Hashtbl.find_opt typhash s with Some x -> x | None -> Std_logic);
      sprintf "%24s         : string := %s" nam s
  | CellParamItem2 (nam, Number (_, _, n, _)) ->
      update typhash nam (Vint n); 
      sprintf "    parameter \\%s %d\n" nam n
   | CellParamItem2 (nam, Typ1 s) ->
      sprintf "%24s         => %s" nam s
  | CellParamItem2 (nam, Typ3(id_t, PackageBody (pkg,[]) :: [])) ->
      sprintf "%24s         : type := %s" nam id_t
  | CellParamItem2 (nam, Typ5(Atom "logic", AnyRange(lft,rght) :: [])) ->
      sprintf "%24s         => logic[%s : %s]" nam (cexpr typhash lft) (cexpr typhash rght)
  | CellParamItem2 (nam, PackageBody (s, Id id :: _)) ->
      sprintf "%24s         => %s" nam s
  | CellParamItem2 (nam, Dot1 (Id lft, Id rght)) ->
      sprintf "%24s         => %s.%s" nam lft rght
  | CellParamItem2 (nam, ((Add _|Sub _|Mult _|Div _|Sys _) as x)) ->
      let n = ceval typhash x in
      update typhash nam (Vint n);
      sprintf "%24s         => %d" nam n
  | CellParamItem3 (nam, Typ1(id_t)) ->
      sprintf "%24s         => %s" nam id_t
  | CellParamItem3 (nam, Typ3(id_t, PackageBody (pkg, []) :: [])) ->
      sprintf "%24s         => %s" nam id_t
 | PackageParam (lst, inner) -> String.concat ", " (List.map (function PkgImport (Itmlst [PkgImportItm (pkg, Atom "*")]) -> parm_map typhash inner | _ -> "") lst)
  | PackageParam2 (grp_e, nam, [PackageBody (pkg, [])], PackageBody (pkg', [Id s])) ->
      update typhash nam (Venum grp_e);
      sprintf "%24s         => %s" grp_e s
  | PackageParam2 (id_t, nam, [PackageBody (pkg, [])], Number (_, _, n, _)) ->
      update typhash nam (Vint n);
      sprintf "%24s         => %d" nam n
  | PackageParam2 (id_t, nam, [PackageBody (pkg, [])], AsgnPat [PatMemberDflt (Number (_, _, n, ""))]) ->
      update typhash nam (Vint n);
      sprintf "%24s         => %d" nam n
  | PackageParam2 (id_t, nam, [PackageBody (pkg, [])], AsgnPat [PatMemberDflt (PackageBody (pkg', [Id id]))]) ->
      update typhash nam (Vtyp id_t);
      sprintf "%24s         => %s" nam id
  | PackageParam2 (id_t, nam, [PackageBody (pkg, [])], ExprQuote1 (Typ3(id, PackageBody (pkg', []) :: []), expr)) ->
      update typhash nam (Vtyp id_t);
      sprintf "%24s         => %s" nam id
  | TypParam (nam, Atom typ, []) ->
      update typhash nam (Vtyp nam); 
      sprintf "%24s         => %s" nam typ
  | TypParam (nam, Id id_t, PackageBody (pkg, []) :: []) ->
      update typhash nam (Vtyp nam); 
      sprintf "%24s         => %s" nam id_t
  | Param (nam, Number (_, _, n, _), []) ->
      update typhash nam (Vint n);
      sprintf "%24s         => %d" nam n
  | Param (nam, String s, []) ->
      update typhash nam (Vstr s);
      sprintf "%24s         => %s" nam s
  | Param (nam, Dot1(lft,rght), []) ->
      update typhash nam (Vdot);
      sprintf "%24s         => %s.%s" nam (cexpr typhash lft) (cexpr typhash rght)
  | Param (nam, PackageBody (pkg, [Id id]), []) ->
      update typhash nam (Vtyp nam);
      sprintf "%24s         => %s" nam id
  | Param (nam, FunRef2 (fn, [PackageBody (pkg, [])], [Id id]), []) ->
      update typhash nam (Vfun fn);
      sprintf "%24s         => %s" nam id
  | Param (nam, Number (_, _, n, _), AnyRange (left, rght) :: []) ->
      update typhash nam (Vint n);
      sprintf "%24s         => %d" nam n
  | Param (nam, UMinus (Number (_, _, n, _)), []) ->
      update typhash nam (Vint (-n));
      sprintf "%24s         => %d" nam (-n)
  | Param (nam, UMinus (Number (_, _, n, _)), AnyRange (lft, rght) :: []) ->
      update typhash nam (Vint (-n));
      sprintf "%24s         => %d" nam (-n)
  | Param (nam, ((Add _|Sub _|Mult _|Div _|StarStar _ |Sys _) as x), []) ->
      let n = ceval typhash x in
      update typhash nam (Vint n);
      sprintf "%24s         => %d" nam n
  | oth -> unhand := Some oth; failwith "parm_map"

let parm_template buf' typhash parm_lst = 
  if parm_lst <> [] then
  bprintf buf' "    generic (\n%s\n    ); // 588	\n" (String.concat ";\n" (List.map (parm_generic typhash) parm_lst))

let decl_mem buf' typhash first last hi lo cnt mem =
    bprintf buf' "    type Mem_Type%d is array [%s : %s] of logic[%s : %s]; // 591	\n" !cnt (cexpr typhash last) (cexpr typhash first) (cexpr typhash hi) (cexpr typhash lo);
    bprintf buf' "    signal %s : Mem_Type%d := (others => (others => '0')); // 592	\n" mem !cnt;
incr cnt

let struct_union buf' typhash = function
	   | SUMember (Typ3 (id_t, PackageBody(pkg, []) :: []), lst) -> List.iter (function
               | Id id -> bprintf buf' "\t%s::%s %s;\n" pkg id_t id
	       | oth -> unhand := Some oth; failwith "SUMember'") lst
	   | SUMember (Typ5 (Atom kind, AnyRange (lft, rght) :: []), lst) -> List.iter (function
               | Id id -> bprintf buf' "\t%s [%s:%s] %s;\n" kind (cexpr typhash lft) (cexpr typhash rght) id
	       | oth -> unhand := Some oth; failwith "SUMember'") lst
           | SUMember (Typ1 typ_t, lst) -> List.iter (function
	       | Id id -> bprintf buf' "\t%s %s;\n" typ_t id
	       | oth -> unhand := Some oth; failwith "SUMember''") lst
           | SUMember (Typ5 (Atom kind, AnyRange(lft,rght) :: AnyRange(lft',rght') :: []), lst) -> List.iter (function
               | Id id -> bprintf buf' "\t%s [%s:%s] [%s:%s] %s;\n" kind (cexpr typhash lft) (cexpr typhash rght) (cexpr typhash lft') (cexpr typhash rght') id
	       | oth -> unhand := Some oth; failwith "SUMember'''") lst
	   | SUMember (Typ6 (Atom ("logic"|"bit"|"byte"|"int"|"longint" as kind)), lst) -> List.iter (function
               | Id id -> bprintf buf' "\t%s %s;\n" kind id
	       | oth -> unhand := Some oth; failwith "SUMember''''") lst
	   | SUMember (Typ8 (Atom ("byte"|"int"|"longint" as kind), (Deflt|Atom "unsigned" as kind')), lst) -> List.iter (function
               | Id id -> bprintf buf' "\t%s %s %s\n" (match kind' with Atom id -> id | _ -> "") kind id
	       | oth -> unhand := Some oth; failwith "SUMember'''''") lst
           | oth -> unhand := Some oth; failwith "SUMember"

let fn_arg typhash = function
		   | PortItem (Typ5 (Atom primtyp, AnyRange (lft, rght) :: []), ItemAsgn (Id id)) ->
                   sprintf "%s [%s:%s] %s" primtyp (cexpr typhash lft) (cexpr typhash rght) id
		   | PortItem (Typ1 (id_t), ItemAsgn (Id id)) ->
                   sprintf "%s %s" id_t id;
		   | PortItem (Typ6 (Atom primtyp), ItemAsgn (Id id)) ->
                   sprintf "%s %s" primtyp id;
		   | PortFront (PortItemFront (dir, Typ1 id_t), ItemAsgn (Id id)) ->
                   sprintf "%s %s %s" (vdir 0 dir) id_t id
                   | PortFront (PortItemFront (dir, Typ5 (Atom primtyp, AnyRange(lft,rght) :: [])), ItemAsgn (Id id)) ->
                   sprintf "%s %s [%s:%s] %s" (vdir 0 dir) primtyp (cexpr typhash lft) (cexpr typhash rght) id
		   | PortItem (Typ8 (Atom primtyp, kind'), ItemAsgn (Id id)) ->
                   sprintf "%s %s %s" (match kind' with Atom typ -> typ | oth -> "") primtyp id;
		   | Deflt -> ""
                   | oth -> unhand := Some oth; failwith "fn_arg"		   

let instance_template buf' typhash typ params inst pinlst =
        bprintf buf' "  cell $%s %s\n" typ inst;
        List.iter (fun itm -> bprintf buf' "%s" (parm_map typhash itm)) params;
        List.iter (function
		   | CellPinItem2 (pin, Number(b,w,n,_)) -> bprintf buf' "    connect \\%s %d'%s\n" pin w (obin w n)
		   | CellPinItem2 (pin, Id conn) -> bprintf buf' "    connect \\%s \\%s\n" pin conn 
		   | CellPinItem1 (pin, conn) -> bprintf buf' "    connect \\%s \\%s\n" pin conn 
                   | oth -> unhand := Some oth; failwith "inst_arg") pinlst;
        bprintf buf' "  end\n"

let dump buf' typhash initlst =
  List.iter (function InstDecl (typ, params, lst) -> List.iter (function
        | InstNameParen1 (inst, Itmlst pins :: []) -> instance_template buf' typhash typ (match params with Itmlst lst :: _ -> lst | _ -> []) inst pins
        | InstNameParen2 (inst, InstRange(lft,rght) :: []) -> instance_template buf' typhash typ (match params with Itmlst lst :: _ -> lst | _ -> []) inst []
        | Id id -> bprintf buf' "    // %s\n" id
        | oth -> unhand := Some oth; failwith "InstDecl'") lst |  oth -> unhand := Some oth; failwith "InstDecl''") initlst

let buflst = ref []
	
let buffer buf' typhash expr lhs =
  let initlst = ref [] in
  let rhs = match asgnexpr' buf' initlst typhash (max (width typhash (Id lhs)) (width typhash expr)) expr with
     | Id rhs -> rhs
     | oth -> unhand := Some oth; failwith "rhs" in
  buflst := !initlst;
  dump buf' typhash !initlst;
  rhs

let asgn buf' typhash expr = function
| Id lhs -> let rhs = buffer buf' typhash expr lhs in bprintf buf' "  connect \\%s \\%s\n" lhs rhs
(*
| Concat _ as lst -> bprintf buf' "            %s <= %s; // 385	\n" (vexpr typhash lst) (asgnexpr buf' typhash expr)
| Dot1 (lft, rght) -> bprintf buf' "            %s.%s <= %s; // 386	\n" (vexpr typhash lft) (vexpr typhash rght) (asgnexpr buf' typhash expr)
| IdArrayed2 (Id id, ix) -> bprintf buf' "            %s(%s) <= %s; // 387	\n" id (vexpr typhash ix) (asgnexpr buf' typhash expr)
| IdArrayedColon (Id id, hi, lo) -> bprintf buf' "            %s[%s : %s] <= %s; // 388	\n" id (vexpr typhash hi) (vexpr typhash lo) (asgnexpr buf' typhash expr)
*)
| oth -> unhand := Some oth; failwith "asgn"

let rec dump_deps buf' kind = function
| [] -> ()
| Pos clk :: [] -> bprintf buf' "%s @(posedge %s)\n" kind clk
| Pos clk :: Pos rst :: [] -> bprintf buf' "%s @(posedge %s or posedge %s)\n" kind clk rst
| oth :: _ -> unhand := Some oth; failwith "dump_deps"

let rec dump_deps_comb buf' typhash kind lst = 
bprintf buf' "%s @(%s)\n" kind (String.concat " or " (List.map (cexpr typhash) lst))

let rec stmt_clause buf' typhash = function
      | Itmlst lst -> List.iter (stmt_clause buf' typhash) lst      
      | BeginBlock lst -> List.iter (stmt_clause buf' typhash) lst
      | If2 (condition, if_lst, else_lst) ->
  bprintf buf' "        if (%s) then\n" (vexpr typhash condition);
    (match if_lst with BeginBlock if_lst -> List.iter (stmt_clause buf' typhash) if_lst | _ -> stmt_clause buf' typhash if_lst);       
  bprintf buf' "        else\n";
    (match else_lst with BeginBlock else_lst -> List.iter (stmt_clause buf' typhash) else_lst | _ -> stmt_clause buf' typhash else_lst);
  bprintf buf' "        end if; // 772	\n";
      | If1 _ as x -> iff_template buf' typhash x
      | DeclLogic lst -> ()
      | Seq (id, []) ->  bprintf buf' "       null; // 775	\n"
      | Seq (id, lst) ->  List.iter (stmt_clause buf' typhash) lst
      | Blocking (Asgn1 (Id id, expr)) -> bprintf buf' "        %s = %s; // 777	\n" id (vexpr typhash expr)
      | Blocking (Asgn1 (IdArrayed2(Id id, sel), expr)) -> bprintf buf' "        %s[%s] = %s; // 777	\n" id (vexpr typhash sel) (vexpr typhash expr)
      | Blocking (Asgn1 (Dot1(Id lft, Id rght), expr)) -> bprintf buf' "        %s.%s = %s; // 778	\n" lft rght (vexpr typhash expr)
      | Blocking (FopAsgn1 (id, id', id'', expr)) -> bprintf buf' "        %s = %s; // 780	\n" id (vexpr typhash expr)
      | Blocking (FopAsgnArrayMemSel (id, hi, lo, expr)) -> bprintf buf' "        %s[%s : %s] = %s; // 781	\n" id (vexpr typhash hi) (vexpr typhash lo) (vexpr typhash expr)
      | Blocking (FopAsgnConcat (idlst, expr)) -> bprintf buf' "        %s = %s; // 782	\n" (String.concat ", " (List.map (vexpr typhash) idlst)) (vexpr typhash expr)
      | Blocking (FopAsgnArraySel (id, ix, expr)) -> bprintf buf' "        %s[%s] = %s; // 783	\n" id (vexpr typhash ix) (vexpr typhash expr)
      | Blocking (FopAsgnArrayWid (id, hi, lo, expr)) -> bprintf buf' "        %s[%s : %s] = %s; // 784	\n" id (vexpr typhash hi) (vexpr typhash lo) (vexpr typhash expr)
      | Blocking (FopAsgnArrayRange (id, hi, lo, expr)) -> bprintf buf' "        %s[%s : %s] = %s; // 785	\n" id (vexpr typhash hi) (vexpr typhash lo) (vexpr typhash expr)
      | Blocking (FopAsgnArrayRange2 (id, ix, ix', expr)) -> bprintf buf' "        %s[%s][%s] = %s; // 786	\n" (vexpr typhash id) (vexpr typhash ix) (vexpr typhash ix') (vexpr typhash expr)
      | Blocking (FopAsgnArrayField (id, ix, expr)) -> bprintf buf' "        %s.%s = %s; // 787	\n" id ix (vexpr typhash expr)
      | Blocking (FopAsgnArrayField2 (id, IdArrayedColon(Id ix, hi, lo), expr)) -> bprintf buf' "        %s.%s[%s : %s] = %s; // 788	\n" id ix (vexpr typhash hi) (vexpr typhash lo) (vexpr typhash expr)
      | Blocking (FopAsgnArrayField3 (id, sel, sel', expr)) ->
          bprintf buf' "        %s[%s].%s = %s; // 790	\n" id (vexpr typhash sel) sel' (vexpr typhash expr)
      | Blocking (FopAsgnArrayField4 (id, sel, id', sel', sel'', expr)) ->
          bprintf buf' "        %s[%s].%s[%s] = %s; // 792	\n" id (vexpr typhash sel) (id') (vexpr typhash sel') (vexpr typhash expr)
      | Blocking (FopAsgnArrayField5 (id, sel, id', sel', expr)) ->
          bprintf buf' "        %s[%s].%s[%s] = %s; // 794	\n" id (vexpr typhash sel) (id') (vexpr typhash sel') (vexpr typhash expr)
      | Blocking (FopAsgnArrayField6 (id, sel, sel', id', expr)) ->
          bprintf buf' "        %s[%s][%s].%s = %s; // 796	\n" id (sel) (vexpr typhash sel') (vexpr typhash id') (vexpr typhash expr)
      | Blocking (FopAsgnArrayField7 (id, sel, sel', id', expr)) ->
          bprintf buf' "        %s[%s][%s].%s = %s; // 798	\n" id (vexpr typhash sel) (vexpr typhash sel') (id') (vexpr typhash expr)
      | ForEach (ix, lst) ->
          bprintf buf' "            foreach %s; // 800	\n" ix;
          List.iter (stmt_clause buf' typhash) lst;
      | ForLoop (Asgn1 (Id ix, strt) :: [], Less (Id ix', limit), SideEffect (Id xi'', Atom "++"), seq) ->
          bprintf buf' "            for %s; // 800	\n" ix;
          stmt_clause buf' typhash seq;
      | ForLoop (Asgn1 (Id ix, strt) :: [], LtEq (Id ix', limit), Asgn1 (Id ix'', Add (Id ix''', Number (_, _, 1, _))), seq) ->
          bprintf buf' "            for %s; // 803	\n" ix;
          stmt_clause buf' typhash seq;
      | ForLoop ([Typ7 (ix, Atom ("int"|"unsigned_int"))], Less (Id ix', limit), Asgn1 (Id ix'', Add (Id ix''', Number (_, _, inc, _))), seq) ->
          bprintf buf' "            for %s; // 806	\n" ix;
          stmt_clause buf' typhash seq;
      | ForLoop ([Typ7 (ix, Atom ("int"|"unsigned_int"))], Less (Id ix', limit), SideEffect (Id xi'', Atom "++"), seq) ->
          bprintf buf' "            for %s; // 809	\n" ix;
          stmt_clause buf' typhash seq;
      | ForLoop ([Typ7 (ix, Atom ("int"|"unsigned_int"))], GtEq (Id ix', limit), SideEffect (Id xi'', Atom "--"), seq) ->
          bprintf buf' "            for %s; // 812	\n" ix;
          stmt_clause buf' typhash seq;
      | ForLoop ([Typ9 (ix, AnyRange(hi,lo) :: [], Atom ("logic"))], Less (Id ix', limit), SideEffect (Id xi'', Atom "++"), seq) ->
          bprintf buf' "            for %s; // 815	\n" ix;
          stmt_clause buf' typhash seq;
      | Equate (id,expr) -> bprintf buf' "            %s <= %s; // 817	\n" id (vexpr typhash expr);
      | EquateSlice (id,hi,lo,expr) -> bprintf buf' "            %s[%s : %s] <= %s; // 818	\n" (vexpr typhash id) (vexpr typhash hi) (vexpr typhash lo) (vexpr typhash expr);
      | EquateSelect (id,ix,expr) -> bprintf buf' "            %s[%s] <= %s; // 819	\n" id (vexpr typhash ix) (vexpr typhash expr);
      | EquateSelect2 (id,ix,expr) -> bprintf buf' "            %s[%s] <= %s; // 820	\n" (vexpr typhash id) (vexpr typhash ix) (vexpr typhash expr);
      | EquateArrayField (id,id',ix,ix',expr) -> bprintf buf' "            %s.%s(%s)(%s) <= %s; // 821	\n" id id' (vexpr typhash ix) (vexpr typhash ix') (vexpr typhash expr);
      | CaseStart (CaseStart1 (sel), lst) ->
        bprintf buf' "case (%s)\n" (vexpr typhash sel);
        List.iter (case_clause buf' typhash) lst;
        bprintf buf' "endcase; // 825	\n";
      | CaseStartInside (sel, lst) ->
        bprintf buf' "case (%s) inside\n" (vexpr typhash sel);
        List.iter (case_clause buf' typhash) lst;
        bprintf buf' "endcase; // 829	\n";
      | CaseStartUniq (CaseStart1 (sel), lst) ->
        bprintf buf' "unique case (%s)\n" (vexpr typhash sel);
        List.iter (case_clause buf' typhash) lst;
        bprintf buf' "endcase; // 833	\n";
      | CaseStartUniqInside (sel, lst) ->
        bprintf buf' "unique case (%s) inside\n" (vexpr typhash sel);
        List.iter (case_clause buf' typhash) lst;
        bprintf buf' "endcase; // 841	\n";
      | Blocking (SideEffect (Id id, Atom "++")) -> bprintf buf' "            %s <= %s+1; // 842	\n" id id
      | Blocking (SideEffect (Id id, Atom "--")) -> bprintf buf' "            %s <= %s-1; // 843	\n" id id
      | DeclData _ -> ()
      | Blocking (BreakSemi) -> () (* placeholder *)
      | BreakSemi -> () (* placeholder *)
      | Atom ";" -> ()
      | TaskRef (tid, lst) -> () (* placeholder *)
      | TaskBody (decls, lst) -> List.iter (stmt_clause buf' typhash) lst
      | SysTaskCall (tid, args) -> bprintf buf' "%s(...);\n" tid
      | EquateField (id, field, expr) ->  bprintf buf' "            %s.%s <= %s; // 851	\n" id field (vexpr typhash expr)
      | DeclInt2 _ -> ()
      | DeclLogic2 _ -> ()
      | Return expr -> bprintf buf' "return %s;\n" (vexpr typhash expr)
      | oth -> unhand := Some oth; failwith "stmt_clause"

and case_clause buf' typhash = function
        | CaseStmt (lbls, body) ->
            bprintf buf' "\t";
            List.iter (function
               | Id lbl -> bprintf buf' "%s" lbl
               | Number _ as lbl -> bprintf buf' "%s" (vexpr typhash lbl)
               | Atom "default" -> bprintf buf' "default"
               | PackageBody (pkg, Id lbl :: []) -> bprintf buf' "                %s::%s       =>  " pkg lbl
	       | OpenRange lst -> bprintf buf' "%s" (String.concat ", " (List.map (function
		   | Id id -> id
		   | ValueRange (lft, rght) -> " [" ^ vexpr typhash lft ^ " : " ^ vexpr typhash rght ^ " ] "
		   | oth -> unhand := Some oth; failwith "open range") lst))
		   | ValueRange(lft, rght) -> bprintf buf' " [%s.%s] " (vexpr typhash lft) (vexpr typhash rght)
	       | ExprOKL lbls -> List.iter (function
		   | Number _ as lbl -> bprintf buf' "                case %s:  " (vexpr typhash lbl)
		   | oth -> unhand := Some oth; failwith "case_label'") lbls
	       | oth -> unhand := Some oth; failwith "case_label") lbls;
	    bprintf buf' " : ";
            List.iter (stmt_clause buf' typhash) body
        | Id lbl -> bprintf buf' "                %s: ; " lbl
        | Number _ as lbl -> bprintf buf' "                %s:  " (vexpr typhash lbl)
        | Atom "default" -> bprintf buf' "                default: "
	| Atom ":" -> ()
	| Atom ";" -> bprintf buf' "                ; "
        | PackageBody (pkg, Id id :: []) -> bprintf buf' "                case %s::%s: " pkg id
        | Itmlst lst -> List.iter (case_clause buf' typhash) lst
        | (Seq _ | Blocking _ | If1 _ |If2 _ | ForLoop _ | CaseStartUniq _ ) as x -> stmt_clause buf' typhash x
        | Return expr -> bprintf buf' "return %s;\n" (vexpr typhash expr)
	| oth -> unhand := Some oth; failwith "case_item"
    
and iff_template buf' typhash = function
    | If1(condition, if_lst) ->
  bprintf buf' "        if (%s) then\n" (vexpr typhash condition);
    stmt_clause buf' typhash if_lst;
  bprintf buf' "        end if; // 883	\n";
    | oth -> unhand := Some oth; failwith "iff_template"

let ev' buf' typhash nam id_lst =
    let ev ix e = bprintf buf' "  attribute \\enum_value_%s \"\\\\%s\"\n" (obin 32 ix) e in
    List.iteri (fun ix -> function 
        | Id e -> ev ix e
        | EnumInit (e, expr) -> ev (ceval typhash expr) e
	| oth -> unhand := Some oth; failwith "TypEnum6") id_lst

    
let rec decl_template buf' typhash modules cnt = function
    | NetDecl (Atom "wire", wire_lst) -> List.iter (function
          | Id nam -> update typhash nam Std_logic;
	      bprintf buf' "  wire %s; // 610	\n" nam
	  | DeclAsgn (nam, AnyRange (hi, lo) :: []) ->
              bprintf buf' "  wire [%s:%s] %s; // 612	\n" (cexpr typhash hi) (cexpr typhash lo) nam
          | InitSig (nam, expr) -> (function
	      | Id id -> bprintf buf' "  wire %s = %s; // 614	\n" nam (cexpr typhash expr)
              | ExprOKL lbls -> bprintf buf' "  wire %s : %s; // 615	\n" nam (String.concat "; " (List.map (cexpr typhash) lbls))
	      | SysFuncCall ("$random", [Deflt]) -> bprintf buf' "    signal %s : $random; // 616	\n" nam
              | Query _ as x -> bprintf buf' "  wire %s : %s; // 617	\n" nam (cexpr typhash x)

	      | oth -> unhand := Some oth; failwith "initsig") expr
	  | oth -> unhand := Some oth; failwith "NetDecl'") wire_lst;
    | Itmlst (id_lst) -> List.iter (function
	  | Id nam -> update typhash nam Std_logic; bprintf buf' "  wire %s; // 622	\n" nam
	  | oth -> unhand := Some oth; failwith "DeclLogic647"
        ) id_lst;
    | DeclLogic (reg_lst) -> List.iter (function
	  | Id nam -> update typhash nam Std_logic; bprintf buf' "  wire %s; // 626	\n" nam
          | DeclAsgn (nam, AnyRange (hi, lo) :: []) ->
              bprintf buf' "  wire [%s : %s] %s; // 628\n" (cexpr typhash hi) (cexpr typhash lo) nam
          | VarDeclAsgn (nam, expr) ->
              bprintf buf' "  wire %s = %s\n" nam (cexpr typhash expr)
	  | oth -> unhand := Some oth; failwith "DeclLogic651"
        ) reg_lst;
    | DeclLogic2 (wire_lst, AnyRange (hi, lo) :: []) -> List.iter (function
	  | Id nam -> update typhash nam (Std_logic_vector(hi,lo));
	  bprintf buf' "  wire [%s : %s] %s; // 635	\n" (cexpr typhash hi) (cexpr typhash lo) nam 
	  | DeclAsgn (nam, AnyRange (hi, lo) :: []) ->
              bprintf buf' "  wire [%s : %s] %s ; // 637\n" (cexpr typhash hi) (cexpr typhash lo) nam 
	  | oth -> unhand := Some oth; failwith "DeclLogic2") wire_lst
    | DeclLogic2 (wire_lst, AnyRange (hi, lo) :: AnyRange (hi', lo') :: []) -> List.iter (function
	  | Id nam -> update typhash nam (Std_logic_vector(hi,lo));
	  bprintf buf' "  wire [%s : %s] [%s : %s] %s ; // 641\n" (cexpr typhash hi) (cexpr typhash lo) (cexpr typhash hi') (cexpr typhash lo') nam
	  | oth -> unhand := Some oth; failwith "DeclWire") wire_lst
    | DeclLogic2 (wire_lst, AnyRange (hi, lo) :: AnyRange (hi', lo') :: AnyRange (hi'', lo'') :: []) -> List.iter (function
	  | Id nam -> update typhash nam (Std_logic_vector(hi,lo));
	  bprintf buf' "  wire [%s : %s] [%s : %s] [%s : %s] %s ; // 645\n" (cexpr typhash hi) (cexpr typhash lo) (cexpr typhash hi') (cexpr typhash lo') (cexpr typhash hi'') (cexpr typhash lo'') nam
	  | oth -> unhand := Some oth; failwith "DeclWire") wire_lst
    | DeclReg (reg_lst, [], []) ->
    List.iter (function
      | Id nam -> update typhash nam Std_logic;
    bprintf buf' "  wire \\%s\n" nam
      | oth -> unhand := Some oth; failwith "DeclReg550") reg_lst
    | DeclReg2 (reg_lst, AnyRange(hi, lo) :: []) ->
    List.iter (function
      | Id nam -> update typhash nam (Std_logic_vector(hi,lo));
      bprintf buf' "  wire width %d \\%s\n" (ceval typhash hi - ceval typhash lo + 1) nam;
      | DeclAsgn (mem, AnyRange (lft, rght) :: []) ->
      bprintf buf' "  wire [%s : %s][%s : %s] %s; // 657\n" (cexpr typhash hi) (cexpr typhash lo) (cexpr typhash lft) (cexpr typhash rght) mem;
      | oth -> unhand := Some oth; failwith "DeclReg555") reg_lst;
    | CaseStmt _ -> ()
    | ContAsgn _ -> ()
    | LoopGen1 _ -> ()
    | CondGen1 _ -> ()
    | GenItem _ -> ()
    | AlwaysComb2 _ -> ()
    | AlwaysFF _ -> ()
    | AlwaysLatch _ -> ()
    | AlwaysLegacy _ -> ()
    | Initial _ -> ()
    | Final _ -> ()
    | Generate _ -> ()
    | DeclInt2 id_lst -> List.iter (function
	| Id itm -> bprintf buf' "  wire %s; // 672	\n" itm
        | VarDeclAsgn (id, expr) -> bprintf buf' "  wire %s = %s\n" id (cexpr typhash expr)
        | oth -> unhand := Some oth; failwith "DeclInt2") id_lst
    | InstDecl (typ, params, lst) -> List.iter (function
        | (InstNameParen1 _ | InstNameParen2 _) -> ()
        | Id id ->     bprintf buf' "  attribute \\wiretype \"\\\\%s\"\n  wire width 32 \\%s\n" typ id
        | oth -> unhand := Some oth; failwith "InstDecl") lst;
    | Typ2 ("bool_t", [], [Id "FALSE"; Id "TRUE"]) -> ()
    | Typ2 (nam, _, id :: []) ->  update typhash nam (Vtyp nam);
        let s = vexpr typhash id in update typhash s Vsigtyp; bprintf buf' "  wire %s : %s; // 681	\n" s nam
    | Typ2 (nam, _, id_lst) -> update typhash nam (Vtyp nam);
        List.iter (function
	     | Id _ as itm -> let s = vexpr typhash itm in update typhash s Vsigtyp; bprintf buf' "  wire %s : %s; // 684	\n" s nam
             | DeclAsgn (id, AnyRange(lft,rght) :: AnyRange(lft',rght') :: []) -> update typhash nam (Vtyp nam);
                bprintf buf' "  wire %s; // 686	\n" id
             | oth -> unhand := Some oth; failwith "Typ2") id_lst;
    | Typ3 (nam, id_lst) -> List.iter (fun _ -> ()) id_lst
    | Typ4 (nam, pkg, rng, id_lst) -> List.iter (fun _ -> ()) id_lst
    | Typ5 (SUDecl (Atom "packed", lst), inst_lst) ->
        bprintf buf' "typedef struct packed { // 703	\n";
        List.iter (struct_union buf' typhash) lst;
        bprintf buf' "} %s;\n" (String.concat ", " (List.map (cexpr typhash) inst_lst));
    | Typ6 (SUDecl (Atom "packed", lst)) ->
        bprintf buf' "typedef struct packed { // 703	\n";
        List.iter (struct_union buf' typhash) lst;
        bprintf buf' "};\n"
    | Typ7 (nam, Typ5 (Atom "logic", AnyRange(lft,rght) :: AnyRange(lft',rght') :: AnyRange(lft'',rght'') :: [])) -> update typhash nam (Vtyp nam);
        bprintf buf' "  wire %s; // 692	\n" nam
    | Typ7 (nam, Typ5 (Atom "logic", AnyRange(lft,rght) :: AnyRange(lft',rght') :: [])) -> update typhash nam (Vtyp nam);
        bprintf buf' "  wire %s; // 694	\n" nam
    | Typ7 (nam, Typ5 (Atom "logic", AnyRange(lft,rght) :: [])) -> update typhash nam (Vtyp nam);
        bprintf buf' "typedef logic [%s:%s] %s; // 696	\n" (cexpr typhash lft) (cexpr typhash rght) nam
    | Typ7 (nam, Typ8 (SUDecl (Atom "packed", lst), Deflt)) -> update typhash nam (Vtyp nam);
        bprintf buf' "typedef struct packed { // 714	\n";
        List.iter (struct_union buf' typhash) lst;
        bprintf buf' "} %s;\n" nam
    | Typ7 (id_t, Typ8 (Union (Atom "packed", lst), Deflt)) ->
        bprintf buf' "typedef union packed { // 718	\n";
        List.iter (struct_union buf' typhash) lst;
        bprintf buf' "} %s;\n" id_t
    | Typ7 (id_t, Typ8 (Itmlst lst, Deflt)) ->
        bprintf buf' "typedef struct { // 722	\n";
        List.iter (struct_union buf' typhash) lst;
        bprintf buf' "} %s;\n" id_t
    | TypEnum4 (TypEnum3 (AnyRange(lft,rght) :: []), id_lst, id_lst') ->
        let kind = Std_logic_vector(lft,rght) in
        List.iter (function Id nam -> update typhash nam kind | _ -> ()) id_lst';
        let f' itm = let s = vexpr typhash itm in update typhash s kind; s in
        let f'' = function Id nam -> bprintf buf' "typedef enum {%s} %s; // 705\n" (String.concat ", " (List.map f' id_lst)) nam | _ -> () in
        List.iter f'' id_lst'
    | TypEnum4 (TypEnum5 (Atom "logic"), id_lst, id_lst') ->
        let kind = Std_logic in
        List.iter (function Id nam -> update typhash nam kind | _ -> ()) id_lst';
        let f' itm = let s = vexpr typhash itm in update typhash s kind; s in
        let f'' = function Id nam -> bprintf buf' "    typedef enum {%s} %s; // 711\n" (String.concat ", " (List.map f' id_lst)) nam | _ -> () in
        List.iter f'' id_lst'
    | TypEnum6 (nam, TypEnum3 (AnyRange(lft,rght) :: []), id_lst) -> update typhash nam (Venum nam);
        bprintf buf' "typedef enum logic [%s:%s] {\n\t%s\n} %s; // 714\n" (cexpr typhash lft) (cexpr typhash rght) (String.concat ",\n\t" (List.map (function
        | Id e -> e
        | EnumInit (e, expr) ->
	    let s = cexpr typhash expr in
	    let s' = sprintf "%s = %s" e s in
	    update typhash s (Venum nam);
	    s'
	| oth -> unhand := Some oth; failwith "TypEnum6") id_lst)) nam
    | TypEnum6 (nam, TypEnum5 (Atom "logic"), id_lst) -> update typhash nam (Venum nam); ev' buf' typhash nam id_lst
    | TypEnum6 (nam, Deflt, id_lst) -> update typhash nam (Venum nam); ev' buf' typhash nam id_lst
    | TypEnum6 (nam, Typ8 (Atom "int", Atom "unsigned"), id_lst) -> update typhash nam (Venum nam); ev' buf' typhash nam id_lst
    | ParamDecl (Atom "localparam", [ParamAsgn1 (nam, expr)]) -> bprintf buf' "    localparam %s = %s; // 740	\n" nam (cexpr typhash expr)
    | Typ6 (Atom "packed") -> () (* placeholder *)
    | Typ10 (id_t, AnyRange (lft, rght) :: [], id_t') -> () (* placeholder *)
    | ParamDecl (LocalParamTyp (Typ1 id), ParamAsgn1 (nam, init) :: []) ->
    (match init with
	   | InitPat lst ->
               bprintf buf' "localparam %s %s = // 935\n" id nam;
               List.iter (function
                   | AsgnPat lst -> List.iter (fun itm -> bprintf buf' "    parameter %s = %s; // 940\n" nam (cexpr typhash itm)) lst
		   | PatMember1 (Id id, AsgnPat lst) -> List.iter (function
                      | PatMemberDflt expr -> bprintf buf' "    parameter %s = %s; // 941\n" nam (cexpr typhash expr)
                      | AsgnPat (PatMemberDflt expr :: []) -> bprintf buf' "    parameter %s = %s; // 941\n" nam (cexpr typhash expr)
		      | oth -> unhand := Some oth; failwith "ParamDecl'''") lst
		   | PatMember1 (Id id, (Id _ | Number _ | ExprOKL _ as x)) -> bprintf buf' "    %s: %s; // 942\n" id (cexpr typhash x)
                   | (Number _ | Id _ as x) -> bprintf buf' "    parameter %s = %s; // 941\n" nam (cexpr typhash x)
	           | oth -> unhand := Some oth; failwith "ParamDecl''") lst
           | (Number _ | Query _ | Expression _  as x) -> bprintf buf' "    parameter %s = %s; // 943\n" nam (cexpr typhash x)
           | oth -> unhand := Some oth; failwith "ParamDecl'")
    | ParamDecl (LocalParamTyp (Typ3 (id, PackageBody (pkg, []) :: [])), ParamAsgn1 (nam, cexpr) :: []) -> ()
    | ParamDecl (LocalParamTyp (Typ3 (id_t, AnyRange (lft, rght) :: [])), ParamAsgn1 (nam, InitPat lst) :: []) -> List.iter (function
        | AsgnPat lst -> List.iter (fun itm -> bprintf buf' "    parameter %s = %s; // 947\n" nam (cexpr typhash itm)) lst
	| oth -> unhand := Some oth; failwith "ParamDecl") lst
    | ParamDecl (LocalParamTyp (Typ5 (Atom "logic", AnyRange (lft, rght) :: [])), [ParamAsgn2 (nam, [AnyRange (lft', rght')], InitPat lst)]) ->
        bprintf buf' "%s; // 773\n" nam
    | ParamDecl (LocalParamTyp (Typ5 (Atom "logic", AnyRange (lft, rght) :: AnyRange (lft', rght') :: [])), [ParamAsgn1 (nam, expr)]) ->
        bprintf buf' "%s = %s; // 775\n" nam (cexpr typhash expr)
    | ParamDecl (LocalParamTyp (Typ5 (Atom "logic", AnyRange (lft, rght) :: [])), ParamAsgn1 (nam, expr) :: []) ->
        bprintf buf' "localparam logic [%s:%s] %s = %s; // 777\n" (cexpr typhash lft) (cexpr typhash rght) nam (cexpr typhash expr)
    | ParamDecl (LocalParamTyp (Typ6 (Atom ("bit"|"logic"))), lst) -> List.iter (function
	        | ParamAsgn1 (id, expr) -> ()
                | oth -> unhand := Some oth; failwith "param_asgn") lst
    | ParamDecl (LocalParamTyp (Typ8 (Atom ("int"|"integer"|"longint"), Deflt)), [ParamAsgn1 (nam , expr)]) ->
        bprintf buf' "%s = %s; // 782\n" nam (cexpr typhash expr)
    | ParamDecl (LocalParamTyp (Typ8 (Atom ("int"|"integer"|"longint" as kind), Atom kind')), [ParamAsgn1 (nam , expr)]) ->
        bprintf buf' "localparam %s %s %s = %s; // 784\n" kind' kind nam (cexpr typhash expr)
    | FunDecl (fn, typ, FunGuts (ports, lst)) ->
        bprintf buf' "function %s %s (%s);\n" (funtyp typhash typ) fn (String.concat ", " (List.map (fn_arg typhash) ports));
        List.iter (stmt_clause buf' typhash) lst;
        bprintf buf' "    endfunction\n";
    | AutoFunDecl (fn, typ, FunGuts (ports, lst)) ->
        bprintf buf' "function automatic %s %s (%s);\n" (funtyp typhash typ) fn (String.concat ", " (List.map (fn_arg typhash) ports));
        List.iter (stmt_clause buf' typhash) lst;
        bprintf buf' "    endfunction\n";
    | PkgImport (Itmlst lst) -> List.iter (decl_template buf' typhash modules cnt) lst
    | PkgImportItm (pkg, Atom "*") -> ()
    | DeclData (Typ5 (Atom "logic", AnyRange (lft, rght) :: AnyRange (lft', rght') :: []), VarDeclAsgn (mem, ExprOKL lst) :: []) -> () (* placeholder *)
    | AssertProperty -> ()
    | oth -> unhand := Some oth; failwith "decl_template"

let rec sent_template buf' typhash clk = function
    | BeginBlock lst -> List.iter (sent_template buf' typhash clk) lst
    | Seq (lbl, lst) -> List.iter (sent_template buf' typhash clk) lst
    | If2 ((Equals (Id rst, lev)|Expression(Equals (Id rst, lev))), if_lst, else_lst) ->
  bprintf buf' "        if (%s = %s) then\n" rst (vexpr typhash lev);
    stmt_clause buf' typhash if_lst;
  bprintf buf' "        elsif (%s'event and %s = '1') then\n" clk clk;
    stmt_clause buf' typhash else_lst;
  bprintf buf' "        end if; // 894	\n";
    | If2 (Id rst, if_lst, else_lst) ->
  bprintf buf' "        if (%s = %s) then\n" rst (vexpr typhash (Number (2, 1, 1, "1")));
    stmt_clause buf' typhash if_lst;
  bprintf buf' "        elsif (%s'event and %s = '1') then\n" clk clk;
    stmt_clause buf' typhash else_lst;
  bprintf buf' "        end if; // 900	\n";
    | If2 ((Pling (Id rst)|Tilde (Id rst)), if_lst, else_lst) ->
  bprintf buf' "        if (%s = %s) then\n" rst (vexpr typhash (Number (2, 1, 0, "1")));
    stmt_clause buf' typhash if_lst;
  bprintf buf' "        elsif (%s'event and %s = '1') then\n" clk clk;
    stmt_clause buf' typhash else_lst;
  bprintf buf' "        end if; // 906	\n";
    | If1 (Id cond, if_lst) ->
  bprintf buf' "        if (%s'event and %s = '1') then\n" clk clk;
    stmt_clause buf' typhash if_lst;
  bprintf buf' "        end if; // 910	\n";
    | If1 (cond, if_lst) ->
  bprintf buf' "        if (%s = '1') then\n" (vexpr typhash cond);
    stmt_clause buf' typhash if_lst;
    bprintf buf' "        end if; // 914	\n";
    | If2 (cond, if_lst, else_lst) ->
  bprintf buf' "        if (%s = '1') then\n" (vexpr typhash cond);
    stmt_clause buf' typhash if_lst;
  bprintf buf' "        elsif (%s'event and %s = '1') then\n" clk clk;
    stmt_clause buf' typhash else_lst;
    bprintf buf' "        end if; // 920	\n";
    | Equate (lhs, rhs) ->
  bprintf buf' "        %s <= %s; // 922	\n" lhs (vexpr typhash rhs);
    | DeclData _ -> ()  
    | oth -> unhand := Some oth; failwith "sent_template"

let dlymemo buf' bufd dhash typhash lhs =
    if Hashtbl.mem dhash lhs then Hashtbl.find dhash lhs else
        begin
        let dly = match newid buf' typhash (width typhash (Id lhs)) with Id id -> id | oth -> failwith "newid'" in
        bprintf bufd "    assign \\%s \\%s\n" dly lhs;
        Hashtbl.add dhash lhs dly;
        dly
        end

let rec rst_template dhash buf' bufd bufr typhash = function
  | Seq(lbl, stmt_lst) -> List.iter (rst_template dhash buf' bufd bufr typhash) stmt_lst
  | Blocking (FopAsgn (lhs, Number (b,w,n,_))) ->
        let dly = dlymemo buf' bufd dhash typhash lhs in
        bprintf bufr "        assign \\%s %d'%s\n" dly w (obin w n);
  | oth -> unhand := Some oth; failwith "rst_template"
				     
let rec oper_template dhash buf' bufd buf'' buf''' typhash = function
  | Seq(lbl, stmt_lst) -> List.iter (oper_template dhash buf' bufd buf'' buf''' typhash) stmt_lst
  | Blocking (FopAsgn (lhs, Number (b,w,n,_))) -> failwith "oper_template'"
  | Blocking (FopAsgn (lhs, expr)) ->
        let dly = dlymemo buf' bufd dhash typhash lhs in
        let rhs = buffer buf' typhash expr lhs in
        bprintf buf'' "        assign \\%s \\%s\n" dly rhs;
        bprintf buf''' "      update \\%s \\%s\n" lhs dly;
  | oth -> unhand := Some oth; failwith "oper_template"
				     
let rec proc_template buf' typhash cnt = function
    | DeclReg _ -> ()
    | DeclLogic _ -> ()
    | AlwaysLegacy (At (EventOr [Pos clk]),
   If2 (Id rst, Equate (lhs, Number (2, w, n, "0")),
    Equate (lhs', expr))) ->
  let rhs = buffer buf' typhash expr lhs in
  let dly = match newid buf' typhash (width typhash (Id lhs')) with Id id -> id | oth -> failwith "newid" in
  bprintf buf' "  process %s\n" (newnam());
  bprintf buf' "    assign \\%s \\%s\n" dly lhs;
  bprintf buf' "    switch \\%s\n" rst;
  bprintf buf' "      case 1'1\n";
  bprintf buf' "        assign \\%s %d'%s\n" dly w (obin w n);
  bprintf buf' "      case \n";
  bprintf buf' "        assign \\%s \\%s\n" dly rhs;
  bprintf buf' "    end\n";
  bprintf buf' "    sync posedge \\%s\n" clk;
  bprintf buf' "      update \\%s \\%s\n" lhs' dly;
  bprintf buf' "  end\n"
    | AlwaysLegacy (At (EventOr [Pos clk]), If2 (Id rst, rst_clause, oper_clause)) ->
    let bufr = Buffer.create 10000 in
    let bufd = Buffer.create 10000 in
    let buf'' = Buffer.create 10000 in
    let buf''' = Buffer.create 10000 in
    let dhash = Hashtbl.create 255 in
    rst_template dhash buf' bufd bufr typhash rst_clause;
    oper_template dhash buf' bufd buf'' buf''' typhash oper_clause;
    bprintf buf' "  process %s\n" (newnam());
    Buffer.add_buffer buf' bufd;
    bprintf buf' "    switch \\%s\n" rst;
    bprintf buf' "      case 1'1\n";
    Buffer.add_buffer buf' bufr;
    bprintf buf' "      case \n";
    Buffer.add_buffer buf' buf'';
    bprintf buf' "    end\n";
    bprintf buf' "    sync posedge \\%s\n" clk;
    Buffer.add_buffer buf' buf''';
    bprintf buf' "  end\n";

(*
    | AlwaysFF (At (EventOr (Pos clk :: _ as dep_lst)), sent_lst) ->
  bprintf buf' "    // clocked process %d description goes here\n" !cnt;
  incr cnt;
  dump_deps buf' "always_ff" dep_lst;
  bprintf buf' "    begin\n";
  sent_template buf' typhash clk sent_lst;       
  bprintf buf' "    end process; // 947	\n";
  bprintf buf' "\n";
    | AlwaysLegacy (At (EventOr (Pos clk :: _ as dep_lst)), sent_lst) ->
  bprintf buf' "    process\n";
  sent_template buf' typhash clk sent_lst;       
  bprintf buf' "    end\n";
    | AlwaysLegacy (At (EventOr dep_lst), sent_lst) ->
  bprintf buf' "    // combinational process %d description goes here\n" !cnt;
  bprintf buf' "    always @ (%s)\n" (String.concat ", " (List.map (vexpr typhash) dep_lst));
  incr cnt;
  dump_deps_comb buf' typhash "always" dep_lst;
  bprintf buf' "    begin\n";
  stmt_clause buf' typhash sent_lst;
  bprintf buf' "    end; // 979	\n";
  bprintf buf' "\n";
    | AlwaysComb2 ( (*DepLst dep_lst,*) sent_lst) -> let dep_lst = [] in
  bprintf buf' "    // combinational process %d description goes here\n" !cnt;
  bprintf buf' "    COMB%d: process (%s)\n" !cnt (String.concat ", " dep_lst);
  incr cnt;
  bprintf buf' "    begin\n";
  stmt_clause buf' typhash sent_lst;
  bprintf buf' "    end; // 987	\n";
  bprintf buf' "\n";
    | AlwaysLatch ( sent_lst ) ->
  bprintf buf' "    // combinational latch process %d description goes here\n" !cnt;
  bprintf buf' "    LATCH%d: process ()\n" !cnt;
  incr cnt;
  bprintf buf' "    begin\n";
  stmt_clause buf' typhash sent_lst;
  bprintf buf' "    end process; // 995	\n";
  bprintf buf' "\n";
  *)
   (* elaboration case *)
   | CaseStart (Id id, (CaseItm (BeginBlock [] :: Unknown ("$error",_) :: Deflt :: []) :: [])) -> bprintf buf' "// elaboration case %s\n" id;
    | ContAsgn lst -> List.iter (function
      | Asgn1 (lhs, expr) -> asgn buf' typhash expr lhs
      | oth -> unhand := Some oth; failwith "assign_template") lst
    | Iff _ -> ()
    | InstDecl (typ, params, lst) -> List.iter (function
        | InstNameParen1 (inst, Itmlst pins :: []) -> instance_template buf' typhash typ (match params with Itmlst lst :: _ -> lst | _ -> []) inst pins
        | InstNameParen2 (inst, InstRange(lft,rght) :: []) -> instance_template buf' typhash typ (match params with Itmlst lst :: _ -> lst | _ -> []) inst []
        | Id id -> bprintf buf' "    // %s\n" id
        | oth -> unhand := Some oth; failwith "InstDecl") lst;
    | TypEnum4 _ -> ()
    | TypEnum6 _ -> ()
    | Typ2 (typ, _, typ_lst) -> ()
    | Typ3 _ -> ()
    | Typ4 _ -> ()
    | Typ5 _ -> ()
    | Typ6 _ -> ()
    | Typ7 _ -> ()
    | DeclInt2 _ -> ()
    | NetDecl _ -> ()
    | DeclReg2 _ -> ()
    | DeclLogic2 _ -> ()
    | LoopGen1 _ -> () (* placeholder *)
    | CondGen1 _ -> () (* placeholder *)
    | GenItem _ -> () (* placeholder *)
    | Generate _ -> () (* placeholder *)
    | ParamDecl _ -> ()
    | FunDecl (fn, Atom primtyp, FunGuts (PortItem _ :: [], lst)) -> () (* placeholder *)
    | AutoFunDecl (fn, typ, FunGuts (ports, lst)) -> () (* placeholder *)
    | Initial _ -> bprintf buf' "// initial is not implemented\n"
    | Final _ -> bprintf buf' "// final is not implemented\n"
    | Itmlst (Id _ :: _) -> ()
    | PkgImport _ -> ()
    | DeclData _ -> ()
    | AssertProperty -> ()
    | oth -> unhand := Some oth; failwith "proc_template"

let dbgtyp = ref (Hashtbl.create 1)
    
let template fd modules = function
  | Modul(nam, parm_lst, port_lst, body_lst') -> let cnt = ref 1 in
  let body_lst = List.map (recurs {fn=recurs}) body_lst' in
  let typhash = Hashtbl.create 255 in
  let buf' = Buffer.create 10000 in
  dbgtyp := typhash;
  bprintf buf' "attribute \\cells_not_processed 1\n";
  bprintf buf' "module \\%s\n" nam;
  List.iteri (fun ix itm -> bprintf buf' "%s\n" (ports typhash (ix+1) itm)) port_lst;
  let typlst, othlst = List.partition (function TypEnum6 _ -> true | _ -> false) body_lst in
  List.iter (decl_template buf' typhash modules cnt) (typlst);
  let components, othlst = List.partition (function InstDecl _ -> true | _ -> false) othlst in
  List.iter (decl_template buf' typhash modules cnt) (List.sort compare components);
  List.iter (decl_template buf' typhash modules cnt) othlst;
  List.iter (proc_template buf' typhash cnt) body_lst;
  bprintf buf' "\n";
  bprintf buf' "end\n";
  bprintf buf' "\n";
  bprintf buf' "\n";
  Buffer.output_buffer fd buf'
  | PackageBody (pkg, body_lst') -> let cnt = ref 1 in
  let body_lst = List.map (recurs {fn=recurs}) body_lst' in
  let typhash = Hashtbl.create 255 in
  let buf' = Buffer.create 10000 in
  bprintf buf' "//\n";
  bprintf buf' "// This converter does not currently preserve comments and license information\n";
  bprintf buf' "//\n";
  bprintf buf' "\n";
  bprintf buf' "package %s; // 1065	\n" pkg;
  List.iter (decl_template buf' typhash modules cnt) body_lst;
  bprintf buf' "endpackage\n";
  Buffer.output_buffer fd buf'
  | oth -> unhand := Some oth; failwith "This template only handles modules/packages"
