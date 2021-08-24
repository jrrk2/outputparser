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
  | Itmlst(rw_lst) -> Itmlst(recurs_lst vhdl_attr (rw_lst:rw list))
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
  | Edge(rw, rw2) -> Edge(recurs_itm vhdl_attr (rw), recurs_itm vhdl_attr (rw2))
  | Intgr(int1) -> Intgr int1
  | Number _ as n -> n
  | RedAnd(rw) -> RedAnd(recurs_itm vhdl_attr (rw))
  | RedOr(rw) -> RedOr(recurs_itm vhdl_attr (rw))
  | RedXor(rw) -> RedXor(recurs_itm vhdl_attr (rw))
  | UMinus(rw) -> UMinus(recurs_itm vhdl_attr (rw))
  | Pling(rw) -> Pling(recurs_itm vhdl_attr (rw))
  | Tilde(rw) -> Tilde(recurs_itm vhdl_attr (rw))
  | TildeAnd(rw) -> TildeAnd(recurs_itm vhdl_attr (rw))
  | TildeOr(rw) -> TildeOr(recurs_itm vhdl_attr (rw))
  | Equals(rw, rw2) -> Equals(recurs_itm vhdl_attr (rw), recurs_itm vhdl_attr (rw2))
  | NotEq(rw, rw2) -> NotEq(recurs_itm vhdl_attr (rw), recurs_itm vhdl_attr (rw2))
  | LtEq(rw, rw2) -> LtEq(recurs_itm vhdl_attr (rw), recurs_itm vhdl_attr (rw2))
  | GtEq(rw, rw2) -> GtEq(recurs_itm vhdl_attr (rw), recurs_itm vhdl_attr (rw2))
  | Less(rw, rw2) -> Less(recurs_itm vhdl_attr (rw), recurs_itm vhdl_attr (rw2))
  | Greater(rw, rw2) -> Greater(recurs_itm vhdl_attr (rw), recurs_itm vhdl_attr (rw2))
  | And(rw, rw2) -> And(recurs_itm vhdl_attr (rw), recurs_itm vhdl_attr (rw2))
  | And2(rw, rw2) -> And2(recurs_itm vhdl_attr (rw), recurs_itm vhdl_attr (rw2))
  | Or(rw, rw2) -> Or(recurs_itm vhdl_attr (rw), recurs_itm vhdl_attr (rw2))
  | Or2(rw, rw2) -> Or2(recurs_itm vhdl_attr (rw), recurs_itm vhdl_attr (rw2))
  | Xor(rw, rw2) -> Xor(recurs_itm vhdl_attr (rw), recurs_itm vhdl_attr (rw2))
  | Xnor(rw, rw2) -> Xnor(recurs_itm vhdl_attr (rw), recurs_itm vhdl_attr (rw2))
  | Shiftl(rw, rw2) -> Shiftl(recurs_itm vhdl_attr (rw), recurs_itm vhdl_attr (rw2))
  | Shiftr(rw, rw2) -> Shiftr(recurs_itm vhdl_attr (rw), recurs_itm vhdl_attr (rw2))
  | Shiftr3(rw, rw2) -> Shiftr3(recurs_itm vhdl_attr (rw), recurs_itm vhdl_attr (rw2))
  | Add(rw, rw2) -> Add(recurs_itm vhdl_attr (rw), recurs_itm vhdl_attr (rw2))
  | Sub(rw, rw2) -> Sub(recurs_itm vhdl_attr (rw), recurs_itm vhdl_attr (rw2))
  | Mult(rw, rw2) -> Mult(recurs_itm vhdl_attr (rw), recurs_itm vhdl_attr (rw2))
  | Div(rw, rw2) -> Div(recurs_itm vhdl_attr (rw), recurs_itm vhdl_attr (rw2))
  | StarStar(rw, rw2) -> StarStar(recurs_itm vhdl_attr (rw), recurs_itm vhdl_attr (rw2))
  | If2(rw, rw2, rw3) -> If2(recurs_itm vhdl_attr (rw), recurs_itm vhdl_attr (rw2), recurs_itm vhdl_attr (rw3))
  | If1(rw, rw2) -> If1(recurs_itm vhdl_attr (rw), recurs_itm vhdl_attr (rw2))
  | ForLoop(rw_lst, rw2, rw3, rw4) ->
    ForLoop(recurs_lst vhdl_attr (rw_lst), recurs_itm vhdl_attr (rw2), recurs_itm vhdl_attr (rw3), recurs_itm vhdl_attr (rw4))
  | CaseStmt(rw, rw_lst) -> CaseStmt(recurs_itm vhdl_attr (rw), recurs_lst vhdl_attr (rw_lst))
  | CaseItm(rw_lst) -> CaseItm(recurs_lst vhdl_attr (rw_lst))
  | AlwaysComb(rw_lst) -> AlwaysComb(recurs_lst vhdl_attr (rw_lst))
  | BeginBlock(rw_lst) -> BeginBlock(recurs_lst vhdl_attr (rw_lst))
  | Bitlst(rw_lst) -> Bitlst(recurs_lst vhdl_attr (rw_lst))
  | Dot1(str1, rw2) -> Dot1(str1, recurs_itm vhdl_attr (rw2))
  | Unsigned(rw) -> Unsigned(recurs_itm vhdl_attr (rw))
  | Signed(rw) -> Signed(recurs_itm vhdl_attr (rw))
  | Concat(rw_lst) -> Concat(recurs_lst vhdl_attr (rw_lst))
  | WireExpr(str1, rw2) -> WireExpr(str1, recurs_itm vhdl_attr (rw2))
  | DeclModPort(rw_lst) -> DeclModPort(recurs_lst vhdl_attr (rw_lst))
  | Repl(rw, rw_lst) -> Repl(recurs_itm vhdl_attr (rw), recurs_lst vhdl_attr (rw_lst))
  | Dot3(str1, str2, str3) -> Dot3(str1, str2, str3)
  | Logic(rw_lst, rw_lst2) -> Logic(recurs_lst vhdl_attr (rw_lst), recurs_lst vhdl_attr (rw_lst2))
  | Param(str1, rw2, rw_lst) -> Param(str1, recurs_itm vhdl_attr (rw2), recurs_lst vhdl_attr (rw_lst))
  | DeclLogic(rw_lst) -> DeclLogic(recurs_lst vhdl_attr (rw_lst))
  | DeclData(rw1, rw_lst2) -> DeclData(recurs_itm vhdl_attr (rw1), recurs_lst vhdl_attr (rw_lst2))
  | GenBlock(rw_lst) -> GenBlock(recurs_lst vhdl_attr (rw_lst))
  | Package (id, rw_lst) -> Package(id, recurs_lst vhdl_attr  rw_lst)
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
DotBus (_, _, _, _)|ElabTask _|ElseStmt _|EnumInit (_, _)|
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
IdArrayedHyphenColon (_, _, _)|IdArrayedPlusColon (_, _, _)|
Iff (_, _)|Import _|Inc _|InitPair (_, _)|InitPat _|
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
| Div (lhs, rhs) -> ceval typhash lhs / ceval typhash rhs
| StarStar (lhs, rhs) -> int_of_float(float_of_int (ceval typhash lhs) ** float_of_int(ceval typhash rhs))
| Sys ("$clog2", x) -> clog2 (ceval typhash x)
| Expression x -> ceval typhash x
| Package (pkg, [id]) -> ceval typhash id (* placeholder *)
| oth -> unhand := Some oth; failwith "ceval"

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
| Package (pkg, id :: _) -> is_const typhash id
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
| Equals ((Id s as lhs), (Number _ as rhs)) -> (match Hashtbl.find typhash s with
    | Std_logic_vector _ -> vexpr' typhash lhs ^ " = " ^ vexpr' typhash rhs
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
| Unsigned expr -> "std_logic_vector("^vexpr typhash expr^")"
| Shiftl (lhs, rhs) -> "shift_left("^vexpr typhash lhs^", "^vexpr typhash rhs^")"
| Shiftr (lhs, rhs) -> "shift_right("^vexpr typhash lhs^", "^vexpr typhash rhs^")"
| CellPinItem1 (port, conn) -> port ^ " => " ^ conn
| CellPinItem2 (port, expr) -> port ^ " => " ^ vexpr typhash expr
| CellPinItemImplied (port) -> port ^ " => " ^ port
| CellPinItemNC (port) -> port ^ " => open"
| Query (Id cond', ctrue', cfalse') -> sprintf "%s when %s = '1' else %s" (vexpr typhash ctrue') cond' (vexpr typhash cfalse')
| Query (cond', ctrue', cfalse') -> sprintf "%s when %s else %s" (vexpr typhash ctrue') (vexpr typhash cond') (vexpr typhash cfalse')
| Deflt -> "open"
| Dot1(lft, rght) -> vexpr typhash lft^"."^vexpr typhash rght
| RedOr (lhs) -> " or (" ^ vexpr typhash lhs ^ ")"
| RedAnd (lhs) -> " and (" ^ vexpr typhash lhs ^ ")"
| RedXor (lhs) -> " xor (" ^ vexpr typhash lhs ^ ")"
| IdArrayed2 (id, ix) -> vexpr typhash id^"("^vexpr typhash ix^")"
| IdArrayed3 (Package (pkg, []) :: [], arr) -> pkg^"::"^vexpr typhash arr
| IdArrayedColon (id, expr, expr') -> vexpr typhash id^"("^vexpr typhash expr^" downto "^vexpr typhash expr'^")"
| IdArrayedPlusColon (id, expr, expr') -> vexpr typhash id^"("^vexpr typhash expr^" downto "^vexpr typhash expr'^")"
| FunRef (fn, arglst) -> fn^"("^String.concat ", " (List.map (vexpr typhash) arglst)^")"
| FunRef2 (fn, _, arglst) -> fn^"("^String.concat ", " (List.map (vexpr typhash) arglst)^")"
| AsgnPat [PatMemberDflt (Number _ as expr)] -> "others => "^(vexpr typhash expr)
| Repl (expr', [expr]) -> "{{"^(vexpr typhash expr')^"}{"^(vexpr typhash expr)^"}}"
| InsideRange (first, last) -> "inside_range("^(vexpr typhash first)^", "^(vexpr typhash last)^")"
| OpenRange (first, last) -> "open_range("^(vexpr typhash first)^", "^(vexpr typhash last)^")"
| ExprOKL lst -> String.concat ", " (List.map (vexpr typhash) lst)
| Package (pkg, [Id id]) -> pkg^"::"^id
| ExprQuote1(lhs, rhs) -> vexpr typhash lhs^"'("^vexpr typhash rhs^")"
| Sys (sys_id, expr) -> String.sub sys_id 1 (String.length sys_id - 1)^"("^vexpr typhash expr^")"
| Typ3 (id_t, [Package (pkg, [])]) -> pkg^"::"^id_t
| Package (pkg, []) -> pkg^"::"
| Atom ".*" -> "" (* placeholder *)
| Typ1 id_t -> id_t
| AsgnPat lst -> String.concat "; " (List.map (vexpr typhash) lst)
| PatMember1 (Id id, expr) -> id ^ " = " ^ vexpr typhash expr
| PatMemberDflt expr -> vexpr typhash expr
| oth -> unhand := Some oth; failwith "vexpr"

and vexpr' typhash = function
| (Intgr _ | Number _) as x -> "(unsigned'(" ^ (vexpr typhash) x ^ ")) "
| (Id s) as x -> (match Hashtbl.find_opt typhash s with
    | Some (Vint _|Vtyp _|Venum _|Vsigtyp|Vpkg _|Vfun _|Vintf _|Vstr _|Vdot) -> vexpr typhash x
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
    | Mult (lhs, rhs) -> cexpr typhash lhs ^ "-" ^ cexpr typhash rhs
    | Div (lhs, rhs) -> cexpr typhash lhs ^ "-" ^ cexpr typhash rhs
    | Or2 (lhs, rhs) -> cexpr typhash lhs ^ " or " ^ cexpr typhash rhs
    | Equals (lhs, rhs) -> cexpr typhash lhs ^ " = " ^ cexpr typhash rhs
    | Query (lhs, rhs, rhs') -> cexpr typhash rhs ^ "when " ^ cexpr typhash lhs ^ " otherwise " ^ cexpr typhash rhs
    | Greater (lhs, rhs) -> cexpr typhash lhs ^ ">" ^ cexpr typhash rhs
    | Sys ("$clog2", _) as expr -> string_of_int (ceval typhash expr)
    | Sys ("$bits", _) -> "1" (* placeholder *)
    | Dot1 (port, conn) -> (cexpr typhash port) ^ " => " ^ (cexpr typhash conn)
    | Expression x -> cexpr typhash x
    | StarStar (lhs, rhs) -> cexpr typhash lhs ^ "+" ^ cexpr typhash rhs
    | Package (pkg, [Id id]) -> pkg^"::"^id
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
| Dot1 (lft, rght) -> fprintf fd "            %s.%s <= %s;\n" (vexpr typhash lft) (vexpr typhash rght) (initexpr typhash expr)
| IdArrayed2 (Id id, ix) -> fprintf fd "            %s(%s) <= %s;\n" id (vexpr typhash ix) (initexpr typhash expr)
| IdArrayedColon (Id id, hi, lo) -> fprintf fd "            %s(%s downto %s) <= %s;\n" id (vexpr typhash hi) (vexpr typhash lo) (initexpr typhash expr)
| oth -> unhand := Some oth; failwith "asgn"

let vdir = function
  | In -> "in "
  | Out -> "out"
  | Inout -> "inout"
  | _ -> failwith "vdir"

let ports typhash = function
    | Port((In|Out|Inout) as dir, nam, [], []) -> update typhash nam Std_logic;
        sprintf "%24s         : %s std_logic" nam (vdir dir);
    | Port(PortDir((In|Out|Inout) as dir, Atom "wire"), nam, [], []) -> update typhash nam Std_logic;
        sprintf "%24s         : %s std_logic" nam (vdir dir);
    | Port ((In|Out|Inout) as dir, nam, AnyRange (hi, lo) :: [], []) -> update typhash nam (Std_logic_vector(hi,lo));
        sprintf "%24s         : %s std_logic_vector(%s downto %s)" nam (vdir dir) (cexpr typhash hi) (cexpr typhash lo);
    | Port (PortDir((In|Out|Inout) as dir, Atom "wire"), nam, AnyRange (hi, lo) :: [], []) -> update typhash nam (Std_logic_vector(hi,lo));
        sprintf "%24s         : %s std_logic_vector(%s downto %s)" nam (vdir dir) (cexpr typhash hi) (cexpr typhash lo);
    | Port ((In|Out|Inout) as dir, nam, AnyRange (hi, lo) :: AnyRange(hi', lo') :: [], []) -> update typhash nam (Std_logic_vector(hi,lo));
        sprintf "%24s         : %s std_logic_vector(%s downto %s)(%s downto %s)" nam (vdir dir) (cexpr typhash hi) (cexpr typhash lo) (cexpr typhash hi') (cexpr typhash lo')
    | Port ((In|Out|Inout) as dir, nam, AnyRange (hi, lo) :: AnyRange(hi', lo') :: AnyRange(hi'', lo'') :: [], []) -> update typhash nam (Std_logic_vector(hi,lo));
        sprintf "%24s         : %s std_logic_vector(%s downto %s)(%s downto %s)(%s downto %s)" nam (vdir dir) (cexpr typhash hi) (cexpr typhash lo) (cexpr typhash hi') (cexpr typhash lo') (cexpr typhash hi'') (cexpr typhash lo'')
    |  Port ((In|Out|Inout) as dir, nam, Typ2 (typ_e, (Package (pkg, [])) :: [], []) :: [], []) -> update typhash nam (Vpkg(pkg, typ_e));
        sprintf "%24s         : %s std_logic" nam (vdir dir);
    |  Port ((In|Out|Inout) as dir, nam, Typ2 (typ_e, (Package (pkg, [])) :: [], []) :: AnyRange (hi, lo) :: [], []) -> update typhash nam (Std_logic_vector(hi, lo));
        sprintf "%24s         : %s std_logic" nam (vdir dir);
    |  Port ((In|Out|Inout) as dir, nam, Typ2 (typ_t, [], []) :: [], []) -> update typhash nam (Vtyp(typ_t));
        sprintf "%24s         : %s std_logic" nam (vdir dir);
    |  Port ((In|Out|Inout) as dir, nam, Typ2 (typ_t, [], []) :: AnyRange (hi, lo) :: [], []) -> update typhash nam (Vtyp(typ_t));
        sprintf "%24s         : %s std_logic_vector(%s downto %s)" nam (vdir dir) (cexpr typhash hi) (cexpr typhash lo);
    |  Port (Deflt, nam, Typ2 (typ_t, Package(pkg, []) :: [], []) :: AnyRange (hi, lo) :: [], []) -> update typhash nam (Vtyp(typ_t));
        sprintf "%24s         : %s(%s downto %s)" nam typ_t (cexpr typhash hi) (cexpr typhash lo);
    |  Dot3 (bus, dir, member) -> update typhash member (Vintf bus);
        sprintf "%24s         : type %s_%s" member bus dir
    |  DotBus (bus, dir, member, AnyRange(lft,rght) :: []) -> update typhash member (Vintf bus);
        sprintf "%24s         : type %s_%s" member bus dir
    | oth -> unhand := Some oth; failwith "component"

let rec parm_generic typhash = function
  | CellParamItem1 (nam, s) ->
      update typhash nam (match Hashtbl.find_opt typhash s with Some x -> x | None -> Std_logic);
      sprintf "%24s         : string := %s" nam s
  | CellParamItem2 (nam, Typ1 s) ->
      sprintf "%24s         : type := %s" nam s
  | CellParamItem2 (nam, Typ5(Atom "logic", AnyRange(lft,rght) :: [])) ->
      sprintf "%24s         : type := std_logc_vector(%s downto %s)" nam (cexpr typhash lft) (cexpr typhash rght)
  | CellParamItem2 (nam, Package (s, Id id :: _)) ->
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
  | CellParamItem3 (nam, Typ3(id_t, Package(pkg, []) :: [])) ->
  sprintf "%24s         : type := %s" nam id_t
  | Param (nam, Number (_, _, n, _), []) ->
      update typhash nam (Vint n);
      sprintf "%24s         : integer := %d" nam n
  | oth -> unhand := Some oth; failwith "parm_generic"

let rec parm_map typhash = function  
  | CellParamItem2 (nam, Number (_, _, n, _)) ->
      update typhash nam (Vint n); 
      sprintf "%24s         => %d" nam n
  | PackageParam (lst, inner) -> String.concat ", " (List.map (function PkgImport (Itmlst [PkgImportItm (pkg, Atom "*")]) -> parm_map typhash inner | _ -> "") lst)
  | PackageParam2 (grp_e, nam, [Package (pkg, [])], Package (pkg', [Id s])) ->
      update typhash nam (Venum grp_e);
      sprintf "%24s         => %s" grp_e s
  | PackageParam2 (id_t, nam, [Package (pkg, [])], Number (_, _, n, _)) ->
      update typhash nam (Vint n);
      sprintf "%24s         => %d" nam n
  | PackageParam2 (id_t, nam, [Package (pkg, [])], AsgnPat [PatMemberDflt (Number (_, _, n, ""))]) ->
      update typhash nam (Vint n);
      sprintf "%24s         => %d" nam n
  | PackageParam2 (id_t, nam, [Package (pkg, [])], AsgnPat [PatMemberDflt (Package (pkg', [Id id]))]) ->
      update typhash nam (Vtyp id_t);
      sprintf "%24s         => %s" nam id
  | PackageParam2 (id_t, nam, [Package (pkg, [])], ExprQuote1 (Typ3(id, Package (pkg', []) :: []), expr)) ->
      update typhash nam (Vtyp id_t);
      sprintf "%24s         => %s" nam id
  | TypParam (nam, Atom typ, []) ->
      update typhash nam (Vtyp nam); 
      sprintf "%24s         => %s" nam typ
  | TypParam (nam, Id id_t, Package(pkg, []) :: []) ->
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
  | Param (nam, Package (pkg, [Id id]), []) ->
      update typhash nam (Vtyp nam);
      sprintf "%24s         => %s" nam id
  | Param (nam, FunRef2 (fn, [Package (pkg, [])], [Id id]), []) ->
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

let parm_template fd typhash parm_lst = 
  if parm_lst <> [] then
  fprintf fd "    generic (\n%s\n    );\n" (String.concat ";\n" (List.map (parm_generic typhash) parm_lst))

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

let rec decl_template fd typhash modules complst cnt = function
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
    | Itmlst (id_lst) -> List.iter (function
	  | Id nam -> update typhash nam Std_logic; fprintf fd "    signal %s : std_logic;\n" nam
	  | oth -> unhand := Some oth; failwith "DeclLogic"
        ) id_lst;
    | DeclLogic (reg_lst) -> List.iter (function
	  | Id nam -> update typhash nam Std_logic; fprintf fd "    signal %s : std_logic;\n" nam
	  | oth -> unhand := Some oth; failwith "DeclLogic"
        ) reg_lst;
    | DeclLogic2 (wire_lst, AnyRange (hi, lo) :: []) -> List.iter (function
	  | Id nam -> update typhash nam (Std_logic_vector(hi,lo));
	  fprintf fd "    signal %s : std_logic_vector(%s downto %s);\n" nam (cexpr typhash hi) (cexpr typhash lo)
	  | oth -> unhand := Some oth; failwith "DeclWire") wire_lst
    | DeclLogic2 (wire_lst, AnyRange (hi, lo) :: AnyRange (hi', lo') :: []) -> List.iter (function
	  | Id nam -> update typhash nam (Std_logic_vector(hi,lo));
	  fprintf fd "    signal %s : std_logic_vector(%s downto %s)(%s downto %s);\n" nam (cexpr typhash hi) (cexpr typhash lo) (cexpr typhash hi') (cexpr typhash lo')
	  | oth -> unhand := Some oth; failwith "DeclWire") wire_lst
    | DeclLogic2 (wire_lst, AnyRange (hi, lo) :: AnyRange (hi', lo') :: AnyRange (hi'', lo'') :: []) -> List.iter (function
	  | Id nam -> update typhash nam (Std_logic_vector(hi,lo));
	  fprintf fd "    signal %s : std_logic_vector(%s downto %s)(%s downto %s)(%s downto %s);\n" nam (cexpr typhash hi) (cexpr typhash lo) (cexpr typhash hi') (cexpr typhash lo') (cexpr typhash hi'') (cexpr typhash lo'')
	  | oth -> unhand := Some oth; failwith "DeclWire") wire_lst
    | DeclReg (reg_lst, [], []) ->
    List.iter (function
      | Id nam -> update typhash nam Std_logic;
    fprintf fd "    signal %s : std_logic;\n" nam
      | oth -> unhand := Some oth; failwith "DeclReg550") reg_lst
    | DeclReg2 (reg_lst, AnyRange(hi, lo) :: []) ->
    List.iter (function
      | Id nam -> update typhash nam (Std_logic_vector(hi,lo));
      fprintf fd "    signal %s : std_logic_vector(%s downto %s);\n" nam (cexpr typhash hi) (cexpr typhash lo);
      | DeclAsgn (mem, AnyRange (lft, rght) :: []) ->
      fprintf fd "    signal %s : std_logic_vector(%s downto %s)(%s downto %s);\n" mem (cexpr typhash hi) (cexpr typhash lo) (cexpr typhash lft) (cexpr typhash rght);
      | oth -> unhand := Some oth; failwith "DeclReg555") reg_lst;
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
    | CondGen1 _ -> ()
    | GenItem _ -> ()
    | AlwaysComb2 _ -> ()
    | AlwaysFF _ -> ()
    | AlwaysLatch _ -> ()
    | AlwaysLegacy _ -> ()
    | Initial _ -> ()
    | Final _ -> ()
    | Generate _ -> ()
    | DeclInt2 id_lst -> List.iter (function Id itm -> fprintf fd "    signal %s : std_logic;\n" itm | oth -> failwith "DeclInt2") id_lst
    | InstDecl (typ, parm_lst, _) -> mod_template fd modules typhash complst (typ, parm_lst)
    | Typ2 ("bool_t", [], [Id "FALSE"; Id "TRUE"]) -> ()
    | Typ2 (nam, _, id :: []) ->  update typhash nam (Vtyp nam);
        let s = vexpr typhash id in update typhash s Vsigtyp; fprintf fd "    signal %s : %s;\n" s nam
    | Typ2 (nam, _, id_lst) ->  update typhash nam (Vtyp nam);
        List.iter (fun itm -> let s = vexpr typhash itm in update typhash s Vsigtyp; fprintf fd "    signal %s : %s;\n" s nam) id_lst;
    | Typ3 (nam, id_lst) -> List.iter (fun _ -> ()) id_lst
    | Typ4 (nam, pkg, rng, id_lst) -> List.iter (fun _ -> ()) id_lst
    | Typ7 (nam, Typ5 (Atom "logic", AnyRange(lft,rght) :: AnyRange(lft',rght') :: AnyRange(lft'',rght'') :: [])) -> update typhash nam (Vtyp nam);
        fprintf fd "    signal %s : std_logic;\n" nam
    | Typ7 (nam, Typ8 (Atom "packed", Deflt)) -> update typhash nam (Vtyp nam);
        fprintf fd "    signal %s : std_logic;\n" nam
    | TypEnum4 (TypEnum3 (AnyRange(lft,rght) :: []), id_lst, id_lst') ->
        let kind = Std_logic_vector(lft,rght) in
        List.iter (function Id nam -> update typhash nam kind | _ -> ()) id_lst';
        let f' itm = let s = vexpr typhash itm in update typhash s kind; s in
        let f'' = function Id nam -> fprintf fd "    type %s is (%s);\n" nam (String.concat ", " (List.map f' id_lst)) | _ -> () in
        List.iter f'' id_lst'
    | TypEnum4 (TypEnum5 (Atom "logic"), id_lst, id_lst') ->
        let kind = Std_logic in
        List.iter (function Id nam -> update typhash nam kind | _ -> ()) id_lst';
        let f' itm = let s = vexpr typhash itm in update typhash s kind; s in
        let f'' = function Id nam -> fprintf fd "    type %s is (%s);\n" nam (String.concat ", " (List.map f' id_lst)) | _ -> () in
        List.iter f'' id_lst'
    | TypEnum6 (nam, _, id_lst) -> update typhash nam (Venum nam);
        fprintf fd "    type %s is (%s);\n" nam (String.concat ", " (List.map (fun itm -> let s = vexpr typhash itm in update typhash s (Venum nam); s) id_lst))
    | ParamDecl (Atom "localparam", [ParamAsgn1 (nam, expr)]) -> fprintf fd "    parameter %s = %s;\n" nam (cexpr typhash expr)
    | Typ6 (Atom "packed") -> () (* placeholder *)
    | ParamDecl (LocalParamTyp (Typ3 (id, Package(pkg, []) :: [])), ParamAsgn1 (nam, cexpr) :: []) -> ()
    | ParamDecl (LocalParamTyp (Typ5 (Atom "logic", AnyRange (lft, rght) :: [])), [ParamAsgn2 (nam, [AnyRange (lft', rght')], InitPat lst)]) -> () (* placeholder *)
    | ParamDecl (LocalParamTyp (Typ5 (Atom "logic", AnyRange (lft, rght) :: AnyRange (lft', rght') :: [])), [ParamAsgn1 (nam, ExprOKL lst)]) -> () (* placeholder *)
    | ParamDecl (LocalParamTyp (Typ5 (Atom "logic", AnyRange (lft, rght) :: [])), ParamAsgn1 (nam, cexpr) :: []) -> ()
    | ParamDecl (LocalParamTyp (Typ8 (Atom "int", Deflt)), [ParamAsgn1 (id , cexpr)]) -> ()
    | ParamDecl (LocalParamTyp (Typ8 (Atom "int", Atom "unsigned")), [ParamAsgn1 (id , cexpr)]) -> ()
    | AutoFunDecl (fn, Typ5 (Atom "logic", [AnyRange (lft, rght)]),
        FunGuts ([PortItem (Typ5 (Atom "logic", [AnyRange (lft', rght')]), ItemAsgn (Id id))], [TaskBody (decls, lst)])) -> ()
    | PkgImport (Itmlst lst) -> List.iter (decl_template fd typhash modules complst cnt) lst
    | PkgImportItm (pkg, Atom "*") -> ()
    | oth -> unhand := Some oth; failwith "decl_template"

let rec stmt_clause fd typhash = function
      | Itmlst lst -> List.iter (stmt_clause fd typhash) lst      
      | BeginBlock lst -> List.iter (stmt_clause fd typhash) lst
      | If2 (condition, if_lst, else_lst) ->
  fprintf fd "        if (%s) then\n" (vexpr typhash condition);
    (match if_lst with BeginBlock if_lst -> List.iter (stmt_clause fd typhash) if_lst | _ -> stmt_clause fd typhash if_lst);       
  fprintf fd "        else\n";
    (match else_lst with BeginBlock else_lst -> List.iter (stmt_clause fd typhash) else_lst | _ -> stmt_clause fd typhash else_lst);
  fprintf fd "        end if;\n";
      | If1 _ as x -> iff_template fd typhash x
      | DeclLogic lst -> ()
      | CaseStmt (state, itmlst) ->
  fprintf fd "            case %s is\n" (vexpr typhash state);
          List.iter (case_clause fd typhash) itmlst;
  fprintf fd "            end case;\n";
      | Seq (id, lst) ->  List.iter (stmt_clause fd typhash) lst
      | Stmt1 (Asgn1 (Id id, expr)) -> fprintf fd "        %s <= %s;\n" id (vexpr typhash expr)
      | Stmt1 (FopAsgn (id, expr)) -> fprintf fd "        %s <= %s;\n" id (vexpr typhash expr)
      | Stmt1 (FopAsgn1 (id, id', id'', expr)) -> fprintf fd "        %s <= %s;\n" id (vexpr typhash expr)
      | Stmt1 (FopAsgnArrayMemSel (id, hi, lo, expr)) -> fprintf fd "        %s(%s downto %s) <= %s;\n" id (vexpr typhash hi) (vexpr typhash lo) (vexpr typhash expr)
      | Stmt1 (FopAsgnConcat (idlst, expr)) -> fprintf fd "        %s <= %s;\n" (String.concat ", " (List.map (vexpr typhash) idlst)) (vexpr typhash expr)
      | Stmt1 (FopAsgnArraySel (id, ix, expr)) -> fprintf fd "        %s(%s) <= %s;\n" id (vexpr typhash ix) (vexpr typhash expr)
      | Stmt1 (FopAsgnArrayWid (id, hi, lo, expr)) -> fprintf fd "        %s(%s down to %s) <= %s;\n" id (vexpr typhash hi) (vexpr typhash lo) (vexpr typhash expr)
      | Stmt1 (FopAsgnArrayRange (id, hi, lo, expr)) -> fprintf fd "        %s(%s down to %s) <= %s;\n" id (vexpr typhash hi) (vexpr typhash lo) (vexpr typhash expr)
      | Stmt1 (FopAsgnArrayRange2 (id, ix, ix', expr)) -> fprintf fd "        %s(%s)(%s) <= %s;\n" (vexpr typhash id) (vexpr typhash ix) (vexpr typhash ix') (vexpr typhash expr)
      | Stmt1 (FopAsgnArrayField (id, ix, expr)) -> fprintf fd "        %s.%s <= %s;\n" id ix (vexpr typhash expr)
      | Stmt1 (FopAsgnArrayField2 (id, IdArrayedColon(Id ix, hi, lo), expr)) -> fprintf fd "        %s.%s(%s down to %s) <= %s;\n" id ix (vexpr typhash hi) (vexpr typhash lo) (vexpr typhash expr)
      | Stmt1 (FopAsgnArrayField3 (id, sel, sel', expr)) ->
          fprintf fd "        %s(%s).%s <= %s;\n" id (vexpr typhash sel) sel' (vexpr typhash expr)
      | Stmt1 (FopAsgnArrayField6 (id, sel, sel', id', expr)) ->
          fprintf fd "        %s(%s)(%s).%s <= %s;\n" id (sel) (vexpr typhash sel') (vexpr typhash id') (vexpr typhash expr)
      | Stmt1 (FopAsgnArrayField7 (id, sel, sel', id', expr)) ->
          fprintf fd "        %s(%s)(%s).%s <= %s;\n" id (vexpr typhash sel) (vexpr typhash sel') (id') (vexpr typhash expr)
      | ForLoop ([Typ7 (ix, Atom ("int"|"unsigned_int"))], Less (Id ix', limit), SideEffect (Id xi'', Atom "++"), seq) ->
          fprintf fd "            for %s;\n" ix;
          stmt_clause fd typhash seq;
      | ForLoop ([Typ7 (ix, Atom ("int"|"unsigned_int"))], GtEq (Id ix', limit), SideEffect (Id xi'', Atom "--"), seq) ->
          fprintf fd "            for %s;\n" ix;
          stmt_clause fd typhash seq;
      | ForLoop ([Typ9 (ix, AnyRange(hi,lo) :: [], Atom ("logic"))], Less (Id ix', limit), SideEffect (Id xi'', Atom "++"), seq) ->
          fprintf fd "            for %s;\n" ix;
          stmt_clause fd typhash seq;
      | Equate (id,expr) -> fprintf fd "            %s <= %s;\n" id (vexpr typhash expr);
      | EquateSlice (id,hi,lo,expr) -> fprintf fd "            %s(%s downto %s) <= %s;\n" (vexpr typhash id) (vexpr typhash hi) (vexpr typhash lo) (vexpr typhash expr);
      | EquateSelect (id,ix,expr) -> fprintf fd "            %s(%s) <= %s;\n" id (vexpr typhash ix) (vexpr typhash expr);
      | EquateSelect2 (id,ix,expr) -> fprintf fd "            %s(%s) <= %s;\n" (vexpr typhash id) (vexpr typhash ix) (vexpr typhash expr);
      | EquateArrayField (id,id',ix,ix',expr) -> fprintf fd "            %s.%s(%s)(%s) <= %s;\n" id id' (vexpr typhash ix) (vexpr typhash ix') (vexpr typhash expr);
      | CaseStart (CaseStart1 (sel), lst) ->
      fprintf fd "            case %s is\n" (vexpr typhash sel);
      List.iter (case_clause fd typhash) lst
      | CaseStartUniq (CaseStart1 (sel), lst) ->
      fprintf fd "            unique case %s is\n" (vexpr typhash sel);
      List.iter (case_clause fd typhash) lst
      | CaseStartUniq2 (IdArrayedColon (Id arr, lft, rght) as sel, lst) ->
      fprintf fd "            unique case %s is\n" (vexpr typhash sel);
      List.iter (case_clause fd typhash) lst
      | Stmt1 (SideEffect (Id id, Atom "++")) -> fprintf fd "            %s <= %s+1;\n" id id
      | Stmt1 (SideEffect (Id id, Atom "--")) -> fprintf fd "            %s <= %s-1;\n" id id
      | DeclData _ -> ()
      | Stmt1 (BreakSemi) -> () (* placeholder *)
      | BreakSemi -> () (* placeholder *)
      | Atom ";" -> ()
      | SysTaskCall (tid, String msg :: []) -> ()
      | oth -> unhand := Some oth; failwith "stmt_clause"

and case_clause fd typhash = function
        | Id caseid ->
            fprintf fd "                when %s       =>  " caseid;
        | Number _ as x ->
            fprintf fd "                when %s       =>  " (vexpr typhash x);
        | Package (pkg, [Id caseid]) ->
            fprintf fd "                when %s::%s   =>  " pkg caseid;
        | Itmlst (caseidlst) ->
            fprintf fd "                when %s       =>  " (String.concat ", " (List.map (vexpr typhash) caseidlst));
        | ValueRange(lft, rght) ->
            fprintf fd "                when %s       =>  " (vexpr typhash lft);
        | Seq (lbl, body) ->
            List.iter (stmt_clause fd typhash) body
        | Stmt1 _ as stmt ->
            stmt_clause fd typhash stmt
        | Atom ("default") ->
            fprintf fd "                when others       =>  ";
        | Atom ":" -> ()
        | Atom ";" -> ()
	| Equate _ as x -> stmt_clause fd typhash x
	| oth -> unhand := Some oth; failwith "case_item"
    
and iff_template fd typhash = function
    | If1(condition, if_lst) ->
  fprintf fd "        if (%s) then\n" (vexpr typhash condition);
    stmt_clause fd typhash if_lst;
  fprintf fd "        end if;\n";
    | oth -> unhand := Some oth; failwith "iff_template"

let rec sent_template fd typhash clk = function
    | BeginBlock lst -> List.iter (sent_template fd typhash clk) lst
    | Seq (lbl, lst) -> List.iter (sent_template fd typhash clk) lst
    | If2 ((Equals (Id rst, lev)|Expression(Equals (Id rst, lev))), if_lst, else_lst) ->
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
    | If2 ((Pling (Id rst)|Tilde (Id rst)), if_lst, else_lst) ->
  fprintf fd "        if (%s = %s) then\n" rst (vexpr typhash (Number (2, 1, 0, "1")));
    stmt_clause fd typhash if_lst;
  fprintf fd "        elsif (%s'event and %s = '1') then\n" clk clk;
    stmt_clause fd typhash else_lst;
  fprintf fd "        end if;\n";
    | If1 (Id cond, if_lst) ->
  fprintf fd "        if (%s'event and %s = '1') then\n" clk clk;
    stmt_clause fd typhash if_lst;
  fprintf fd "        end if;\n";
    | If1 (cond, if_lst) ->
  fprintf fd "        if (%s = '1') then\n" (vexpr typhash cond);
    stmt_clause fd typhash if_lst;
  fprintf fd "        end if;\n";
    | oth -> unhand := Some oth; failwith "sent_template"

let instance_template fd typhash typ params inst pinlst =
        fprintf fd "%s: %s" inst typ;
        if params <> [] then fprintf fd " generic map (%s)\n" (String.concat ", " (List.map (parm_map typhash) params));
        fprintf fd " port map (\n\t%s\n\t);\n" (String.concat ",\n\t" (List.map (vexpr typhash) pinlst))

let rec event_lst = function
| EventOr (Id id, Id id') -> id :: id' :: []	
| EventOr (ev, Id id) -> id :: event_lst ev
| EventOr (Id id, ev) -> id :: event_lst ev
| EventOr (lft, rght) -> event_lst lft @ event_lst rght
| _ -> []

let rec proc_template fd typhash cnt = function
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
    | AlwaysFF (At (Pos clk), sent_lst) ->
  fprintf fd "    -- clocked process %d description goes here\n" !cnt;
  fprintf fd "    SEQ%d: process (%s)\n" !cnt clk;
  incr cnt;
  fprintf fd "    begin\n";
  sent_template fd typhash clk sent_lst;       
  fprintf fd "    end process;\n";
  fprintf fd "\n";
    | AlwaysLegacy (At (EventOr (Pos clk, (Pos rst|Neg rst))), sent_lst) ->
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
    | AlwaysLegacy (At (EventOr _ as expr), sent_lst) -> let dep_lst = event_lst expr in
  fprintf fd "    -- combinational process %d description goes here\n" !cnt;
  fprintf fd "    COMB%d: process (%s)\n" !cnt (String.concat ", " dep_lst);
  incr cnt;
  fprintf fd "    begin\n";
  stmt_clause fd typhash sent_lst;
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
    | AlwaysLatch ( sent_lst ) ->
  fprintf fd "    -- combinational latch process %d description goes here\n" !cnt;
  fprintf fd "    LATCH%d: process ()\n" !cnt;
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
        | InstNameParen1 (inst, Itmlst pins :: []) -> instance_template fd typhash typ (match params with Itmlst lst :: _ -> lst | _ -> []) inst pins
        | InstNameParen2 (inst, InstRange(lft,rght) :: []) -> instance_template fd typhash typ (match params with Itmlst lst :: _ -> lst | _ -> []) inst []
        | Id id -> fprintf fd "    -- %s\n" id
        | oth -> unhand := Some oth; failwith "InstDecl") lst;
    | TypEnum4 _ -> ()
    | TypEnum6 _ -> ()
    | Typ2 (typ, _, typ_lst) -> ()
    | Typ3 _ -> ()
    | Typ4 _ -> ()
    | Typ6 _ -> ()
    | Typ7 _ -> ()
    | DeclInt2 _ -> ()
    | DeclReg2 _ -> ()
    | DeclLogic2 _ -> ()
    | LoopGen1 _ -> () (* placeholder *)
    | CondGen1 _ -> () (* placeholder *)
    | GenItem _ -> () (* placeholder *)
    | Generate _ -> () (* placeholder *)
    | ParamDecl _ -> ()
    | AutoFunDecl (fn, Typ5 (Atom "logic", [AnyRange (lft, rght)]),
        FunGuts ([PortItem (Typ5 (Atom "logic", [AnyRange (lft', rght')]), ItemAsgn (Id id))], [TaskBody (decls, lst)])) -> () (* placeholder *)
(*
    | CondGen1 (cond, (GenItem _ as x), (GenItem _ (* as x' *) )) -> proc_template fd typhash cnt x
*)
    | Initial _ -> fprintf fd "-- initial is not implemented"
    | Final _ -> fprintf fd "-- final is not implemented"
    | Itmlst (Id _ :: _) -> ()
    | PkgImport _ -> ()
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
