open Vxml_types
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
  | Vdot of string * string
  | Vstr of string
  | Vtyp of string
  | Vfun of string
  | Venum of string
  | Vintf of string
  | Vcomp of Vxml_types.rw list
  | Vemember of string * string * rw

let unhand = ref None
let othinst = ref None

let update typhash id expr =
(*  print_endline id;
*)
  Hashtbl.replace typhash id expr

type sysver_attr = {fn: sysver_attr -> rw -> rw}

let rec recurs' (sysver_attr:sysver_attr) = function
  | Id id -> Id id
  | Expression rw -> Expression(recurs_itm sysver_attr  rw)
  | Itmlst(rw_lst) -> Itmlst(recurs_lst sysver_attr (rw_lst:rw list))
  | Unknown (str,lst) -> Unknown (str, recurs_lst sysver_attr  lst)
  | In -> In
  | Out -> Out
  | Inout -> Inout
  | Modul(str1, rw_lst1, rw_lst2, rw_lst3) -> Modul(str1, recurs_lst sysver_attr  rw_lst1, recurs_lst sysver_attr  rw_lst2, recurs_lst sysver_attr  rw_lst3)
  | DeclReg(rw_lst, str1_lst, rw_lst_lst) -> DeclReg(recurs_lst sysver_attr  rw_lst, str1_lst,
    List.map (fun itm -> recurs_lst sysver_attr  itm) rw_lst_lst)
  | NonBlocking(rw, rw2) -> NonBlocking(recurs_itm sysver_attr  rw, recurs_itm sysver_attr  rw2)
  | Query(rw, rw2, rw3) -> Query(recurs_itm sysver_attr  rw, recurs_itm sysver_attr  rw2, recurs_itm sysver_attr  rw3)
  | Port(rw, str1, rw_lst1, rw_lst2) -> Port(recurs_itm sysver_attr  rw, str1, recurs_lst sysver_attr rw_lst1, recurs_lst sysver_attr rw_lst2)
  | Pos(str1) -> Pos (str1)
  | Neg(str1) -> Neg (str1)
  | Edge(rw, rw2) -> Edge(recurs_itm sysver_attr (rw), recurs_itm sysver_attr (rw2))
  | Intgr(int1) -> Intgr int1
  | Number _ as n -> n
  | RedAnd(rw) -> RedAnd(recurs_itm sysver_attr (rw))
  | RedOr(rw) -> RedOr(recurs_itm sysver_attr (rw))
  | RedXor(rw) -> RedXor(recurs_itm sysver_attr (rw))
  | UMinus(rw) -> UMinus(recurs_itm sysver_attr (rw))
  | Pling(rw) -> Pling(recurs_itm sysver_attr (rw))
  | Tilde(rw) -> Tilde(recurs_itm sysver_attr (rw))
  | TildeAnd(rw) -> TildeAnd(recurs_itm sysver_attr (rw))
  | TildeOr(rw) -> TildeOr(recurs_itm sysver_attr (rw))
  | Equals(rw, rw2) -> Equals(recurs_itm sysver_attr (rw), recurs_itm sysver_attr (rw2))
  | NotEq(rw, rw2) -> NotEq(recurs_itm sysver_attr (rw), recurs_itm sysver_attr (rw2))
  | LtEq(rw, rw2) -> LtEq(recurs_itm sysver_attr (rw), recurs_itm sysver_attr (rw2))
  | GtEq(rw, rw2) -> GtEq(recurs_itm sysver_attr (rw), recurs_itm sysver_attr (rw2))
  | Less(rw, rw2) -> Less(recurs_itm sysver_attr (rw), recurs_itm sysver_attr (rw2))
  | Greater(rw, rw2) -> Greater(recurs_itm sysver_attr (rw), recurs_itm sysver_attr (rw2))
  | And(rw, rw2) -> And(recurs_itm sysver_attr (rw), recurs_itm sysver_attr (rw2))
  | And2(rw, rw2) -> And2(recurs_itm sysver_attr (rw), recurs_itm sysver_attr (rw2))
  | Or(rw, rw2) -> Or(recurs_itm sysver_attr (rw), recurs_itm sysver_attr (rw2))
  | Or2(rw, rw2) -> Or2(recurs_itm sysver_attr (rw), recurs_itm sysver_attr (rw2))
  | Xor(rw, rw2) -> Xor(recurs_itm sysver_attr (rw), recurs_itm sysver_attr (rw2))
  | Xnor(rw, rw2) -> Xnor(recurs_itm sysver_attr (rw), recurs_itm sysver_attr (rw2))
  | Shiftl(rw, rw2) -> Shiftl(recurs_itm sysver_attr (rw), recurs_itm sysver_attr (rw2))
  | Shiftr(rw, rw2) -> Shiftr(recurs_itm sysver_attr (rw), recurs_itm sysver_attr (rw2))
  | Shiftr3(rw, rw2) -> Shiftr3(recurs_itm sysver_attr (rw), recurs_itm sysver_attr (rw2))
  | Add(rw, rw2) -> Add(recurs_itm sysver_attr (rw), recurs_itm sysver_attr (rw2))
  | Sub(rw, rw2) -> Sub(recurs_itm sysver_attr (rw), recurs_itm sysver_attr (rw2))
  | Mult(rw, rw2) -> Mult(recurs_itm sysver_attr (rw), recurs_itm sysver_attr (rw2))
  | Div(rw, rw2) -> Div(recurs_itm sysver_attr (rw), recurs_itm sysver_attr (rw2))
  | StarStar(rw, rw2) -> StarStar(recurs_itm sysver_attr (rw), recurs_itm sysver_attr (rw2))
  | If2(rw, rw2, rw3) -> If2(recurs_itm sysver_attr (rw), recurs_itm sysver_attr (rw2), recurs_itm sysver_attr (rw3))
  | If1(rw, rw2) -> If1(recurs_itm sysver_attr (rw), recurs_itm sysver_attr (rw2))
  | ForLoop(rw_lst, rw2, rw3, rw4) ->
    ForLoop(recurs_lst sysver_attr (rw_lst), recurs_itm sysver_attr (rw2), recurs_itm sysver_attr (rw3), recurs_itm sysver_attr (rw4))
  | CaseStmt(rw_lst, rw_lst') -> CaseStmt(recurs_lst sysver_attr rw_lst, recurs_lst sysver_attr rw_lst')
  | CaseItm(rw_lst) -> CaseItm(recurs_lst sysver_attr (rw_lst))
  | AlwaysComb(rw_lst) -> AlwaysComb(recurs_lst sysver_attr (rw_lst))
  | BeginBlock(rw_lst) -> BeginBlock(recurs_lst sysver_attr (rw_lst))
  | Bitlst(rw_lst) -> Bitlst(recurs_lst sysver_attr (rw_lst))
  | Dot1(str1, rw2) -> Dot1(str1, recurs_itm sysver_attr (rw2))
  | Unsigned(rw) -> Unsigned(recurs_itm sysver_attr (rw))
  | Signed(rw) -> Signed(recurs_itm sysver_attr (rw))
  | Concat(rw_lst) -> Concat(recurs_lst sysver_attr (rw_lst))
  | WireExpr(str1, rw2) -> WireExpr(str1, recurs_itm sysver_attr (rw2))
  | DeclModPort(rw_lst) -> DeclModPort(recurs_lst sysver_attr (rw_lst))
  | Repl(rw, rw_lst) -> Repl(recurs_itm sysver_attr (rw), recurs_lst sysver_attr (rw_lst))
  | Dot3(str1, str2, str3) -> Dot3(str1, str2, str3)
  | Logic(rw_lst, rw_lst2) -> Logic(recurs_lst sysver_attr (rw_lst), recurs_lst sysver_attr (rw_lst2))
  | Param(str1, rw2, rw_lst) -> Param(str1, recurs_itm sysver_attr (rw2), recurs_lst sysver_attr (rw_lst))
  | DeclLogic(rw_lst) -> DeclLogic(recurs_lst sysver_attr (rw_lst))
  | DeclData(rw1, rw_lst2) -> DeclData(recurs_itm sysver_attr (rw1), recurs_lst sysver_attr (rw_lst2))
  | GenBlock(rw_lst) -> GenBlock(recurs_lst sysver_attr (rw_lst))
  | PackageBody (id, rw_lst) -> PackageBody (id, recurs_lst sysver_attr  rw_lst)
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

and recurs_lst sysver_attr x = List.map (sysver_attr.fn sysver_attr ) x

and recurs_itm sysver_attr  x = sysver_attr .fn sysver_attr  x

let rec recurs (sysver_attr:sysver_attr) = function
  | If2(Expression rw, rw2, rw3) -> If2(recurs_itm sysver_attr (rw), recurs_itm sysver_attr (rw2), recurs_itm sysver_attr (rw3))
  | Or (Expression (Or _ as x), expr) -> Or (recurs_itm sysver_attr x, recurs_itm sysver_attr expr)
  | Or (expr, Expression (Or _ as x)) -> Or (recurs_itm sysver_attr expr, recurs_itm sysver_attr x)
  | And2 (Expression (And2 _ as x), expr) -> And2 (recurs_itm sysver_attr x, recurs_itm sysver_attr expr)
  | And2 (expr, Expression (And2 _ as x)) -> And2 (recurs_itm sysver_attr expr, recurs_itm sysver_attr x)
  | Or (expr, (And _ as x)) -> Or (recurs_itm sysver_attr expr, Expression (recurs_itm sysver_attr x))
  | Or (And2 _ as x, expr) -> Or (Expression (recurs_itm sysver_attr x), recurs_itm sysver_attr expr)
  | oth -> recurs' {fn=recurs} oth

let rec obin w n = 
  (if w > 1 then obin (w-1) (n lsr 1) else "")^string_of_int (n land 1)

let vlst = ref []
let coth = ref None

let ceval' = function
| Vint n -> n
| Std_logic -> print_endline ("Type std_logic evaluated to zero"); 0
| Vtyp s -> print_endline ("Type "^s^" evaluated to zero"); 0
| Venum s ->  print_endline ("Type enum evaluated to 42"); 42
| Vemember(enum,nam,Intgr n) -> n
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

let cexpr typhash = function
| Number (b,w,n,s) -> HEX n
| Intgr n -> HEX n
| oth -> HEX (ceval typhash oth)

let rec vexpr typhash = function
| Id s ->
    let typ' = match Hashtbl.find_opt typhash s with
    | Some Std_logic_vector(hi,lo) -> VRF (s, (BASDTYP, "logic", TYPRNG(cexpr typhash hi, cexpr typhash lo), []), [])
    | Some Vemember(enum,nam,Intgr n) -> CNST(32, HEX n)
    | Some Vint n -> CNST(32, HEX n)
    | _ -> VRF (s, (BASDTYP, "logic", TYPNONE, []), []) in typ'
| Expression x -> vexpr typhash x
| Number (b,w,n,s) -> CNST(w, HEX n)
| Intgr n -> CNST(32, HEX n)
| Tilde expr -> UNRY(Unot, vexpr typhash expr :: [])
| Pling expr -> UNRY(Unegate, vexpr typhash expr :: [])
| Concat lst -> REPL("", 1, List.map (vexpr typhash) lst)
| Add (lhs, rhs) -> ARITH(Aadd, vexpr typhash lhs :: vexpr typhash rhs :: [])
| Sub (lhs, rhs) -> ARITH(Asub, vexpr typhash lhs :: vexpr typhash rhs :: [])
| Mult (lhs, rhs) -> ARITH(Amul, vexpr typhash lhs :: vexpr typhash rhs :: [])
| Div (lhs, rhs) -> ARITH(Aunknown, vexpr typhash lhs :: vexpr typhash rhs :: [])
| StarStar (lhs, rhs) -> ARITH(Aunknown, vexpr typhash lhs :: vexpr typhash rhs :: [])
| LtEq (lhs, rhs) -> CMP(Clte, vexpr typhash lhs :: vexpr typhash rhs :: [])
| UMinus (rhs) -> UNRY (Unegate, vexpr typhash rhs :: [])
| Equals (lhs, rhs) -> CMP(Ceq, vexpr typhash lhs :: vexpr typhash rhs :: [])
| NotEq (lhs, rhs) -> CMP(Cneq, vexpr typhash lhs :: vexpr typhash rhs :: [])
| GtEq (lhs, rhs) -> CMP (Cgte, vexpr typhash lhs :: vexpr typhash rhs :: [])
| Less (lhs, rhs) -> CMP (Clt, vexpr typhash lhs :: vexpr typhash rhs :: [])
| Greater (lhs, rhs) -> CMP (Cgt, vexpr typhash lhs :: vexpr typhash rhs :: [])
| Or (lhs, rhs) -> LOGIC(Lor, vexpr typhash lhs :: vexpr typhash rhs :: [])
| Or2 (lhs, rhs) -> LOGIC(Lor, vexpr typhash lhs :: vexpr typhash rhs :: [])
| Xor (lhs, rhs) -> LOGIC(Lxor, vexpr typhash lhs :: vexpr typhash rhs :: [])
| And (lhs, rhs) -> LOGIC(Land, vexpr typhash lhs :: vexpr typhash rhs :: [])
| And2 (lhs, rhs) -> LOGIC(Land, vexpr typhash lhs :: vexpr typhash rhs :: [])
| Unsigned expr -> UNRY(Uunsigned, vexpr typhash expr :: [])
| Shiftl (lhs, rhs) -> LOGIC(Lshiftl, vexpr typhash lhs :: vexpr typhash rhs :: [])
| Shiftr (lhs, rhs) -> LOGIC(Lshiftr, vexpr typhash lhs :: vexpr typhash rhs :: [])
| Shiftr3 (lhs, rhs) -> LOGIC(Lshiftr, vexpr typhash lhs :: vexpr typhash rhs :: [])
| Query (cond', ctrue', cfalse') -> CND("", vexpr typhash cond' :: vexpr typhash ctrue' :: vexpr typhash cfalse' :: [])
(*
| CellPinItem1 (port, conn) -> port ^ " => " ^ conn
| CellPinItem2 (port, expr) -> port ^ " => " ^ vexpr typhash expr
| CellPinItemImplied (port) -> port ^ " => " ^ port
| CellPinItemNC (port) -> port ^ " => open"
*)
| IdArrayedColon (id, expr, expr') ->
   let lo = ceval typhash expr' in
   let hi = ceval typhash expr in
   SEL ("", vexpr typhash id :: CNST(32, HEX lo) :: CNST(32, HEX(hi-lo+1)) :: [])
| ExprOKL lst -> CAT ("", List.map (vexpr typhash) lst)
| IdArrayed2 (id, ix) -> SEL ("", vexpr typhash id :: vexpr typhash ix :: CNST(1, HEX 1) :: [])
(*
| Deflt -> "open"
| Dot1(lft, rght) -> vexpr typhash lft^"."^vexpr typhash rght
| RedOr (lhs) -> " or (" ^ vexpr typhash lhs ^ ")"
| RedAnd (lhs) -> " and (" ^ vexpr typhash lhs ^ ")"
| RedXor (lhs) -> " xor (" ^ vexpr typhash lhs ^ ")"
| TildeOr (lhs) -> " nor (" ^ vexpr typhash lhs ^ ")"
| IdArrayed3 (PackageBody (pkg, []) :: [], arr) -> pkg^"::"^vexpr typhash arr
| IdArrayedPlusColon (id, expr, expr') -> vexpr typhash id^"["^vexpr typhash expr^" : "^vexpr typhash expr'^"]"
| IdArrayedHyphenColon (id, expr, expr') -> vexpr typhash id^"["^vexpr typhash expr^" : "^vexpr typhash expr'^"]"
| FunRef (fn, arglst) -> fn^"("^String.concat ", " (List.map (vexpr typhash) arglst)^")"
| FunRef2 (fn, _, arglst) -> fn^"("^String.concat ", " (List.map (vexpr typhash) arglst)^")"
| AsgnPat [PatMemberDflt (Number _ as expr)] -> "others => "^(vexpr typhash expr)
| Repl (expr', [expr]) -> "{{"^(vexpr typhash expr')^"}{"^(vexpr typhash expr)^"}}"
| InsideRange (first, last) -> "inside_range("^(vexpr typhash first)^", "^(vexpr typhash last)^")"
| OpenRange lst -> "open_range("^String.concat ", " (List.map (vexpr typhash) lst)^")"
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
*)
| Sys ("$unsigned", expr) -> UNRY(Uunsigned, vexpr typhash expr :: [])
| oth -> unhand := Some oth; failwith "vexpr"

let funtyp typhash = function
| Atom primtyp -> (BASDTYP, primtyp, TYPNONE, [])
| Typ6 (Atom primtyp) -> (BASDTYP, primtyp, TYPNONE, [])
| Typ1 id_t -> (BASDTYP, id_t, TYPNONE, [])
| Typ3 (id_t, [PackageBody (pkg, [])]) -> (BASDTYP, id_t, TYPNONE, [])
| Typ5 (Atom primtyp, AnyRange (lft, rght) :: []) -> (BASDTYP, primtyp, TYPRNG (cexpr typhash lft, cexpr typhash rght), [])
| Typ8 (Atom kind, Atom kind') -> (BASDTYP, kind, TYPNONE, [])
| Typ8 (Atom kind, Deflt) ->  (BASDTYP, kind, TYPNONE, [])
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

let vdir = function
  | In -> Dinput
  | Out -> Doutput
  | Inout -> Dinout
  | _ -> failwith "vdir"

let ports typhash = function
    | Port((In|Out|Inout) as dir, nam, [], []) -> update typhash nam Std_logic;
        IO ("", [nam], (BASDTYP, "logic", TYPNONE, []), vdir dir, "logic", [])
    | Port(PortDir((In|Out|Inout) as dir, Atom "wire"), nam, [], []) -> update typhash nam Std_logic;
        IO ("", [nam], (BASDTYP, "logic", TYPNONE, []), vdir dir, "logic", [])
    | Port ((In|Out|Inout) as dir, nam, AnyRange (hi, lo) :: [], []) -> update typhash nam (Std_logic_vector(hi,lo));
        IO ("", [nam], (BASDTYP, "logic", TYPRNG(cexpr typhash hi, cexpr typhash lo), []), vdir dir, "logic", [])
    | Port (PortDir((In|Out|Inout) as dir, Atom "wire"), nam, AnyRange (hi, lo) :: [], []) -> update typhash nam (Std_logic_vector(hi,lo));
        IO ("", [nam], (BASDTYP, "logic", TYPRNG(cexpr typhash hi, cexpr typhash lo), []), vdir dir, "logic", [])
(*
    | Port ((In|Out|Inout) as dir, nam, AnyRange (hi, lo) :: AnyRange(hi', lo') :: [], []) -> update typhash nam (Std_logic_vector(hi,lo));
        sprintf "%s [%s:%s][%s:%s] %s" (vdir dir) (vexpr typhash hi) (vexpr typhash lo) (vexpr typhash hi') (vexpr typhash lo') nam
    | Port ((In|Out|Inout) as dir, nam, AnyRange (hi, lo) :: AnyRange(hi', lo') :: AnyRange(hi'', lo'') :: [], []) -> update typhash nam (Std_logic_vector(hi,lo));
        sprintf "%s [%s:%s][%s:%s][%s:%s] %s" (vdir dir) (vexpr typhash hi) (vexpr typhash lo) (vexpr typhash hi') (vexpr typhash lo') (vexpr typhash hi'') (vexpr typhash lo'') nam
    |  Port ((In|Out|Inout) as dir, nam, Typ2 (typ_e, (PackageBody (pkg, [])) :: [], []) :: [], []) -> update typhash nam (Vpkg(pkg, typ_e));
        sprintf "%s %s" (vdir dir) nam;
    |  Port ((In|Out|Inout) as dir, nam, Typ2 (typ_e, (PackageBody (pkg, [])) :: [], []) :: AnyRange (hi, lo) :: [], []) -> update typhash nam (Std_logic_vector(hi, lo));
        sprintf "%s %s" (vdir dir) nam;
    |  Port ((In|Out|Inout) as dir, nam, Typ2 (typ_t, [], []) :: [], []) -> update typhash nam (Vtyp(typ_t));
        sprintf "%s %s" (vdir dir) nam;
    |  Port ((In|Out|Inout) as dir, nam, Typ2 (typ_t, [], []) :: AnyRange (hi, lo) :: [], []) -> update typhash nam (Vtyp(typ_t));
        sprintf "%s [%s:%s] %s" (vdir dir) (vexpr typhash hi) (vexpr typhash lo) nam;
    |  Port (Deflt, nam, Typ2 (typ_t, PackageBody (pkg, []) :: [], []) :: AnyRange (hi, lo) :: [], []) -> update typhash nam (Vtyp(typ_t));
        sprintf "%s [%s:%s] %s" typ_t (vexpr typhash hi) (vexpr typhash lo) nam;
    |  Dot3 (bus, dir, member) -> update typhash member (Vintf bus);
        sprintf "%s %s %s" member bus dir
    |  DotBus (bus, dir, member, AnyRange(lft,rght) :: []) -> update typhash member (Vintf bus);
        sprintf "%s %s %s" member bus dir
*)
    |  Port ((In|Out|Inout) as dir, nam, Typ6 (Atom primtyp) :: [], []) -> update typhash nam (Std_logic);
        IO ("", [nam], (BASDTYP, primtyp, TYPNONE, []), vdir dir, "logic", [])
    | oth -> unhand := Some oth; failwith "component"

let rec parm_generic typhash = function
  | CellParamItem1 (nam, s) ->
      update typhash nam (match Hashtbl.find_opt typhash s with Some x -> x | None -> Std_logic)
  | CellParamItem2 (nam, Typ1 s) -> update typhash nam (Vtyp s)
  | CellParamItem2 (nam, Typ3(id_t, PackageBody (pkg,[]) :: [])) -> update typhash nam (Vtyp id_t)
  | CellParamItem2 (nam, Typ5(Atom "logic", AnyRange(lft,rght) :: [])) -> update typhash nam (Std_logic_vector(lft,rght))
  | CellParamItem2 (nam, PackageBody (s, Id id :: _)) -> update typhash nam (Vtyp s)
  | CellParamItem2 (nam, Dot1 (Id lft, Id rght)) -> update typhash nam (Vdot(lft,rght))
  | CellParamItem2 (nam, Number (_, _, n, _)) -> update typhash nam (Vint n); 
  | CellParamItem2 (nam, ((Add _|Sub _|Mult _|Div _|Sys _) as x)) ->
      let n = ceval typhash x in
      update typhash nam (Vint n)
  | CellParamItem3 (nam, Typ1(id_t)) -> update typhash nam (Vtyp id_t)
  | CellParamItem3 (nam, Typ3(id_t, PackageBody (pkg, []) :: [])) ->  update typhash nam (Vtyp id_t)
  | Param (nam, Number (_, _, n, _), []) -> update typhash nam (Vint n);
  | PackageParam2 (id_t, nam, [], Id s) ->
      update typhash nam (Vtyp id_t);
  | PackageParam2 (grp_e, nam, [PackageBody (pkg, [])], PackageBody (pkg', [Id s])) ->
      update typhash nam (Venum grp_e);
  | PackageParam2 (id_t, nam, [PackageBody (pkg, [])], Number (_, _, n, _)) ->
      update typhash nam (Vint n);
  | PackageParam2 (id_t, nam, [PackageBody (pkg, [])], AsgnPat [PatMemberDflt (Number (_, _, n, ""))]) ->
      update typhash nam (Vint n);
  | PackageParam2 (id_t, nam, [PackageBody (pkg, [])], AsgnPat [PatMemberDflt (PackageBody (pkg', [Id id]))]) ->
      update typhash nam (Vtyp id_t);
  | PackageParam2 (id_t, nam, [PackageBody (pkg, [])], ExprQuote1 (Typ3(id, PackageBody (pkg', []) :: []), expr)) ->
      update typhash nam (Vtyp id_t);
  | TypParam (nam, Atom typ, []) ->
      update typhash nam (Vtyp nam); 
  | TypParam (nam, Id id_t, PackageBody (pkg, []) :: []) ->
      update typhash nam (Vtyp nam); 
  | TypParam (nam, Atom typ, AnyRange (lft, rght) :: []) ->
      update typhash nam (Std_logic_vector(lft,rght));
  | Param (nam, PackageBody (pkg, [Id id]), []) ->
      update typhash nam (Vtyp nam);
  | Param (nam, FunRef2 (fn, [PackageBody (pkg, [])], expr :: []), []) ->
      update typhash nam (Vfun fn);
  | Param (nam, String s, []) ->
      update typhash nam (Vstr s);
  | Param (nam, Dot1(Id lft,Id rght), []) ->
      update typhash nam (Vdot(lft,rght));
  | Param (nam, Number (_, _, n, _), AnyRange (left, rght) :: []) ->
      update typhash nam (Vint n);
  | Param (nam, UMinus (Number (_, _, n, _)), []) ->
      update typhash nam (Vint (-n));
  | Param (nam, UMinus (Number (_, _, n, _)), AnyRange (lft, rght) :: []) ->
      update typhash nam (Vint (-n));
  | Param (nam, ((Add _|Sub _|Mult _|Div _|StarStar _ |Sys _|Equals _|Query _) as x), []) ->
      let n = ceval typhash x in
      update typhash nam (Vint n);
 | PackageParam (lst, inner) -> List.iter (function
    | PkgImport (Itmlst [PkgImportItm (pkg, Atom "*")]) -> parm_generic typhash inner
    | _ -> ()) lst
 | oth -> unhand := Some oth; failwith "parm_generic"

let parm_template typhash parm_lst = 
  List.map (parm_generic typhash) parm_lst

(*  
let decl_mem typhash first last hi lo cnt mem =
    sprintf "    type Mem_Type%d is array [%s : %s] of logic[%s : %s]; // 591	\n" !cnt (vexpr typhash last) (vexpr typhash first) (vexpr typhash hi) (vexpr typhash lo) ,
    sprintf "    signal %s : Mem_Type%d := (others => (others => '0')); // 592	\n" mem !cnt;
incr cnt
*)

let struct_union typhash = function
(*
           | SUMember (Typ3 (id_t, PackageBody(pkg, []) :: []), lst) -> List.map (function
               | Id id -> sprintf "\t%s::%s %s;\n" pkg id_t id
	       | oth -> unhand := Some oth; failwith "SUMember'") lst
	   | SUMember (Typ5 (Atom kind, AnyRange (lft, rght) :: []), lst) -> List.map (function
               | Id id -> sprintf "\t%s [%s:%s] %s;\n" kind (vexpr typhash lft) (vexpr typhash rght) id
	       | oth -> unhand := Some oth; failwith "SUMember'") lst
           | SUMember (Typ1 typ_t, lst) -> List.map (function
	       | Id id -> sprintf "\t%s %s;\n" typ_t id
	       | oth -> unhand := Some oth; failwith "SUMember''") lst
           | SUMember (Typ5 (Atom kind, AnyRange(lft,rght) :: AnyRange(lft',rght') :: []), lst) -> List.map (function
               | Id id -> sprintf "\t%s [%s:%s] [%s:%s] %s;\n" kind (vexpr typhash lft) (vexpr typhash rght) (vexpr typhash lft') (vexpr typhash rght') id
	       | oth -> unhand := Some oth; failwith "SUMember'''") lst
	   | SUMember (Typ6 (Atom ("logic"|"bit"|"byte"|"int"|"longint" as kind)), lst) -> List.map (function
               | Id id -> sprintf "\t%s %s;\n" kind id
	       | oth -> unhand := Some oth; failwith "SUMember''''") lst
	   | SUMember (Typ8 (Atom ("byte"|"int"|"longint" as kind), (Deflt|Atom "unsigned" as kind')), lst) -> List.map (function
               | Id id -> sprintf "\t%s %s %s\n" (match kind' with Atom id -> id | _ -> "") kind id
	       | oth -> unhand := Some oth; failwith "SUMember'''''") lst
	       *)
           | oth -> unhand := Some oth; failwith "SUMember"

let fn_arg typhash = function
(*
		   | PortItem (Typ5 (Atom primtyp, AnyRange (lft, rght) :: []), ItemAsgn (Id id)) ->
                   sprintf "%s [%s:%s] %s" primtyp (vexpr typhash lft) (vexpr typhash rght) id
		   | PortItem (Typ1 (id_t), ItemAsgn (Id id)) ->
                   sprintf "%s %s" id_t id;
		   | PortItem (Typ6 (Atom primtyp), ItemAsgn (Id id)) ->
                   sprintf "%s %s" primtyp id;
		   | PortFront (PortItemFront (dir, Typ1 id_t), ItemAsgn (Id id)) ->
                   sprintf "%s %s %s" (vdir dir) id_t id
                   | PortFront (PortItemFront (dir, Typ5 (Atom primtyp, AnyRange(lft,rght) :: [])), ItemAsgn (Id id)) ->
                   sprintf "%s %s [%s:%s] %s" (vdir dir) primtyp (vexpr typhash lft) (vexpr typhash rght) id
		   | PortItem (Typ8 (Atom primtyp, kind'), ItemAsgn (Id id)) ->
                   sprintf "%s %s %s" (match kind' with Atom typ -> typ | oth -> "") primtyp id;
		   | Deflt -> ""
*)
                   | oth -> unhand := Some oth; failwith "fn_arg"		   

let rec stmt_clause typhash = function
      | Itmlst lst -> BGN(None, List.map (stmt_clause typhash) lst)
      | BeginBlock lst -> BGN(None, List.map (stmt_clause typhash) lst)
      | If2 (condition, if_lst, else_lst) -> IF ("", vexpr typhash condition :: stmt_clause typhash if_lst :: stmt_clause typhash else_lst :: [])
      | If1 _ as x -> iff_template typhash x
      | DeclLogic lst -> BGN(None, [])
      | Seq (id, lst) -> BGN(None, List.map (stmt_clause typhash) lst)
      | Blocking (Asgn1 (lhs, expr)) -> ASGN(true, "", vexpr typhash expr :: vexpr typhash lhs :: [])
      | Blocking (FopAsgn (lhs, expr)) -> ASGN(true, "", vexpr typhash expr :: vexpr typhash (Id lhs) :: [])
(*
      | Blocking (Asgn1 (IdArrayed2(Id id, sel), expr)) -> sprintf "        %s[%s] = %s; // 777	\n" id (vexpr typhash sel) (vexpr typhash expr)
      | Blocking (Asgn1 (Dot1(Id lft, Id rght), expr)) -> sprintf "        %s.%s = %s; // 778	\n" lft rght (vexpr typhash expr)
      | Blocking (FopAsgn1 (id, id', id'', expr)) -> sprintf "        %s = %s; // 780	\n" id (vexpr typhash expr)
      | Blocking (FopAsgnArrayMemSel (id, hi, lo, expr)) -> sprintf "        %s[%s : %s] = %s; // 781	\n" id (vexpr typhash hi) (vexpr typhash lo) (vexpr typhash expr)
      | Blocking (FopAsgnConcat (idlst, expr)) -> sprintf "        %s = %s; // 782	\n" (String.concat ", " (List.map (vexpr typhash) idlst)) (vexpr typhash expr)
      | Blocking (FopAsgnArraySel (id, ix, expr)) -> sprintf "        %s[%s] = %s; // 783	\n" id (vexpr typhash ix) (vexpr typhash expr)
      | Blocking (FopAsgnArrayWid (id, hi, lo, expr)) -> sprintf "        %s[%s : %s] = %s; // 784	\n" id (vexpr typhash hi) (vexpr typhash lo) (vexpr typhash expr)
      | Blocking (FopAsgnArrayRange (id, hi, lo, expr)) -> sprintf "        %s[%s : %s] = %s; // 785	\n" id (vexpr typhash hi) (vexpr typhash lo) (vexpr typhash expr)
      | Blocking (FopAsgnArrayRange2 (id, ix, ix', expr)) -> sprintf "        %s[%s][%s] = %s; // 786	\n" (vexpr typhash id) (vexpr typhash ix) (vexpr typhash ix') (vexpr typhash expr)
      | Blocking (FopAsgnArrayField (id, ix, expr)) -> sprintf "        %s.%s = %s; // 787	\n" id ix (vexpr typhash expr)
      | Blocking (FopAsgnArrayField2 (id, IdArrayedColon(Id ix, hi, lo), expr)) -> sprintf "        %s.%s[%s : %s] = %s; // 788	\n" id ix (vexpr typhash hi) (vexpr typhash lo) (vexpr typhash expr)
      | Blocking (FopAsgnArrayField3 (id, sel, sel', expr)) ->
          sprintf "        %s[%s].%s = %s; // 790	\n" id (vexpr typhash sel) sel' (vexpr typhash expr)
      | Blocking (FopAsgnArrayField4 (id, sel, id', sel', sel'', expr)) ->
          sprintf "        %s[%s].%s[%s] = %s; // 792	\n" id (vexpr typhash sel) (id') (vexpr typhash sel') (vexpr typhash expr)
      | Blocking (FopAsgnArrayField5 (id, sel, id', sel', expr)) ->
          sprintf "        %s[%s].%s[%s] = %s; // 794	\n" id (vexpr typhash sel) (id') (vexpr typhash sel') (vexpr typhash expr)
      | Blocking (FopAsgnArrayField6 (id, sel, sel', id', expr)) ->
          sprintf "        %s[%s][%s].%s = %s; // 796	\n" id (sel) (vexpr typhash sel') (vexpr typhash id') (vexpr typhash expr)
      | Blocking (FopAsgnArrayField7 (id, sel, sel', id', expr)) ->
          sprintf "        %s[%s][%s].%s = %s; // 798	\n" id (vexpr typhash sel) (vexpr typhash sel') (id') (vexpr typhash expr)
      | ForEach (ix, lst) ->
          sprintf "            foreach %s; // 800	\n" ix;
          List.map (stmt_clause typhash) lst;
*)
      | ForLoop (Asgn1 (Id ix, strt) :: [], Less (Id ix', limit), SideEffect (Id xi'', Atom "++"), seq) ->
          FORSTMT("", ix, Clt, vexpr typhash strt, (32, HEX 0),  (32, HEX 1),  (32, HEX 2), stmt_clause typhash seq :: [])
      | ForLoop (Asgn1 (Id ix, strt) :: [], LtEq (Id ix', limit), Asgn1 (Id ix'', Add (Id ix''', Number (_, _, 1, _))), seq) ->
          FORSTMT("", ix, Clte, vexpr typhash strt, (32, HEX 0),  (32, HEX 1),  (32, HEX 2), stmt_clause typhash seq :: [])
      | ForLoop ([Typ7 (ix, Atom ("int"|"unsigned_int"))], Less (Id ix', limit), Asgn1 (Id ix'', Add (Id ix''', Number (_, _, inc, _))), seq) ->
          FORSTMT("", ix, Clt, vexpr typhash (Intgr 0), (32, HEX 0),  (32, HEX 1),  (32, HEX 2), stmt_clause typhash seq :: [])
      | ForLoop ([Typ7 (ix, Atom ("int"|"unsigned_int"))], Less (Id ix', limit), SideEffect (Id xi'', Atom "++"), seq) ->
          FORSTMT("", ix, Clt, vexpr typhash (Intgr 0), (32, HEX 0),  (32, HEX 1),  (32, HEX 2), stmt_clause typhash seq :: [])
      | ForLoop ([Typ7 (ix, Atom ("int"|"unsigned_int"))], GtEq (Id ix', limit), SideEffect (Id xi'', Atom "--"), seq) ->
          FORSTMT("", ix, Clt, vexpr typhash (Intgr 0), (32, HEX 0),  (32, HEX 1),  (32, HEX 2), stmt_clause typhash seq :: [])
      | ForLoop ([Typ9 (ix, AnyRange(hi,lo) :: [], Atom ("logic"))], Less (Id ix', limit), SideEffect (Id xi'', Atom "++"), seq) ->
          FORSTMT("", ix, Clt, vexpr typhash (Intgr 0), (32, HEX 0),  (32, HEX 1),  (32, HEX 2), stmt_clause typhash seq :: [])
      | Equate (id,expr) -> ASGN(true, "", vexpr typhash expr :: vexpr typhash (Id id) :: [])
      | EquateSlice (id,hi',lo',expr) ->
	  let lo = ceval typhash lo' in
	  let hi = ceval typhash hi' in
	  ASGN(true, "", vexpr typhash expr :: SEL ("", vexpr typhash id :: CNST(32, HEX lo) :: CNST(32, HEX(hi-lo+1)) :: []) :: [])
      | EquateSelect (id,ix,expr) ->
          ASGN(true, "", vexpr typhash expr :: SEL("", vexpr typhash (Id id) :: vexpr typhash ix :: CNST(32, HEX 1) :: []) :: [])
(*
      | EquateSelect2 (id,ix,expr) -> sprintf "            %s[%s] <= %s; // 820	\n" (vexpr typhash id) (vexpr typhash ix) (vexpr typhash expr);
      | EquateArrayField (id,id',ix,ix',expr) -> sprintf "            %s.%s(%s)(%s) <= %s; // 821	\n" id id' (vexpr typhash ix) (vexpr typhash ix') (vexpr typhash expr);
*)
      | CaseStart (CaseStart1 (sel), lst) -> CS("", vexpr typhash sel :: List.map (case_clause typhash) lst)
(*
      | CaseStartInside (sel, lst) ->
        sprintf "case (%s) inside\n" (vexpr typhash sel);
        List.map (case_clause typhash) lst;
        sprintf "endcase; // 829	\n";
      | CaseStartUniq (CaseStart1 (sel), lst) ->
        sprintf "unique case (%s)\n" (vexpr typhash sel);
        List.map (case_clause typhash) lst;
        sprintf "endcase; // 833	\n";
      | CaseStartUniqInside (sel, lst) ->
        sprintf "unique case (%s) inside\n" (vexpr typhash sel);
        List.map (case_clause typhash) lst;
        sprintf "endcase; // 841	\n";
      | Blocking (SideEffect (Id id, Atom "++")) -> sprintf "            %s <= %s+1; // 842	\n" id id
      | Blocking (SideEffect (Id id, Atom "--")) -> sprintf "            %s <= %s-1; // 843	\n" id id
      | DeclData _ -> ()
      | Blocking (BreakSemi) -> () (* placeholder *)
      | BreakSemi -> () (* placeholder *)
      | TaskRef (tid, lst) -> () (* placeholder *)
      | TaskBody (decls, lst) -> List.map (stmt_clause typhash) lst
      | SysTaskCall (tid, args) -> sprintf "%s(...);\n" tid
      | EquateField (id, field, expr) ->  sprintf "            %s.%s <= %s; // 851	\n" id field (vexpr typhash expr)
      | DeclInt2 _ -> ()
      | DeclLogic2 _ -> ()
      | Return expr -> sprintf "return %s;\n" (vexpr typhash expr)
      *)
      | Atom ";" -> BGN(None, [])
      | oth -> unhand := Some oth; failwith "stmt_clause"

and case_clause typhash = function
        | CaseStmt (lbls, body) -> CSITM("", List.flatten(
            List.map (function
               | (Id _ | Number _ as lbl) -> CNST (32, cexpr typhash lbl) :: []
               | Atom "default" -> BGN(None, []) :: []
               | PackageBody (pkg, Id lbl :: []) -> PKG("", pkg, []) :: []
(*
	       | OpenRange lst -> sprintf "%s" (String.concat ", " (List.map (function
		   | Id id -> id
		   | ValueRange (lft, rght) -> " [" ^ vexpr typhash lft ^ " : " ^ vexpr typhash rght ^ " ] "
		   | oth -> unhand := Some oth; failwith "open range") lst))
		   | ValueRange(lft, rght) -> sprintf " [%s.%s] " (vexpr typhash lft) (vexpr typhash rght)
*)
               | ExprOKL lbls -> List.map (function
		   | Number _ as lbl -> CNST(32, cexpr typhash lbl)
		   | oth -> unhand := Some oth; failwith "case_label'") lbls
	       | oth -> unhand := Some oth; failwith "case_label") lbls) @
            List.map (stmt_clause typhash) body)
(*
        | Id lbl -> sprintf "                %s: ; " lbl
        | Number _ as lbl -> sprintf "                %s:  " (vexpr typhash lbl)
        | Atom "default" -> sprintf "                default: "
	| Atom ":" -> ()
	| Atom ";" -> sprintf "                ; "
        | PackageBody (pkg, Id id :: []) -> sprintf "                case %s::%s: " pkg id
        | Itmlst lst -> List.map (case_clause typhash) lst
        | (Seq _ | Blocking _ | If1 _ |If2 _ | ForLoop _ | CaseStartUniq _ ) as x -> stmt_clause typhash x
        | Return expr -> sprintf "return %s;\n" (vexpr typhash expr)
*)
        | oth -> unhand := Some oth; failwith "case_item"
	    
and iff_template typhash = function
    | If1(condition, if_lst) -> IF ("", vexpr typhash condition :: stmt_clause typhash if_lst :: [])
    | oth -> unhand := Some oth; failwith "iff_template"

let mod_template modules typhash ((typ:string), parm_lst) =
if not (Hashtbl.mem typhash typ) then
    begin
    match List.assoc_opt typ !modules with
        | Some (Modul(_, _, port_lst, _)) -> update typhash typ (Vcomp(List.map (ports typhash) port_lst));
	| None -> printf "-- %s is not a module\n" typ;
        | Some oth -> unhand := Some oth; failwith ("mod_template: "^typ)
    end

let rec decl_template typhash modules cnt = function
    | NetDecl (Atom "wire", wire_lst) -> List.map (function
          | Id nam -> update typhash nam Std_logic;
	      VAR ("", [nam], (BASDTYP, "logic", TYPNONE, []), "logic")
	  | DeclAsgn (nam, AnyRange (hi, lo) :: []) ->
	      VAR ("", [nam], (BASDTYP, "logic", TYPRNG (cexpr typhash hi, cexpr typhash lo), []), "logic")
(*
          | InitSig (nam, expr) -> (function
	      | Id id -> sprintf "wire %s = %s; // 614	\n" nam (vexpr typhash expr)
              | ExprOKL lbls -> sprintf "wire %s : %s; // 615	\n" nam (String.concat "; " (List.map (vexpr typhash) lbls))
	      | SysFuncCall ("$random", [Deflt]) -> sprintf "    signal %s : $random; // 616	\n" nam
              | Query _ as x -> sprintf "wire %s : %s; // 617	\n" nam (vexpr typhash x)

	      | oth -> unhand := Some oth; failwith "initsig") expr
*)
	  | oth -> unhand := Some oth; failwith "NetDecl'") wire_lst;
    | Itmlst (id_lst) -> List.map (function
	  | Id nam -> update typhash nam Std_logic;  VAR ("", [nam], (BASDTYP, "logic", TYPNONE, []), "logic")
	  | oth -> unhand := Some oth; failwith "DeclLogic647"
        ) id_lst;
    | DeclLogic (reg_lst) -> List.map (function
	  | Id nam -> update typhash nam Std_logic; VAR ("", [nam], (BASDTYP, "logic", TYPNONE, []), "logic")
          | DeclAsgn (nam, AnyRange (hi, lo) :: []) ->
	      VAR ("", [nam], (BASDTYP, "logic", TYPRNG (cexpr typhash hi, cexpr typhash lo), []), "logic")
          | VarDeclAsgn (nam, expr) ->
              VAR ("", [nam], (BASDTYP, "logic", TYPNONE, []), "logic")
	  | oth -> unhand := Some oth; failwith "DeclLogic651"
        ) reg_lst;
    | DeclLogic2 (wire_lst, AnyRange (hi, lo) :: []) -> List.map (function
	  | Id nam -> update typhash nam (Std_logic_vector(hi,lo));
	      VAR ("", [nam], (BASDTYP, "logic", TYPRNG (cexpr typhash hi, cexpr typhash lo), []), "logic")
	  | DeclAsgn (nam, AnyRange (hi, lo) :: []) ->
	      VAR ("", [nam], (BASDTYP, "logic", TYPRNG (cexpr typhash hi, cexpr typhash lo), []), "logic")
	  | oth -> unhand := Some oth; failwith "DeclLogic2") wire_lst
    | DeclLogic2 (wire_lst, AnyRange (hi, lo) :: AnyRange (hi', lo') :: []) -> List.map (function
(*
	  | Id nam -> update typhash nam (Std_logic_vector(hi,lo));
	  sprintf "wire [%s : %s] [%s : %s] %s ; // 641\n" (vexpr typhash hi) (vexpr typhash lo) (vexpr typhash hi') (vexpr typhash lo') nam
	  *)
	  | oth -> unhand := Some oth; failwith "DeclWire") wire_lst
    | DeclLogic2 (wire_lst, AnyRange (hi, lo) :: AnyRange (hi', lo') :: AnyRange (hi'', lo'') :: []) -> List.map (function
(*	  | Id nam -> update typhash nam (Std_logic_vector(hi,lo));
	  sprintf "wire [%s : %s] [%s : %s] [%s : %s] %s ; // 645\n" (vexpr typhash hi) (vexpr typhash lo) (vexpr typhash hi') (vexpr typhash lo') (vexpr typhash hi'') (vexpr typhash lo'') nam
*)
	  | oth -> unhand := Some oth; failwith "DeclWire") wire_lst
    | DeclReg (reg_lst, [], []) -> List.map (function
      | Id nam -> update typhash nam Std_logic;
      VAR ("", [nam], (BASDTYP, "logic", TYPNONE, []), "logic")
      | oth -> unhand := Some oth; failwith "DeclReg550") reg_lst
    | DeclReg2 (reg_lst, AnyRange(hi, lo) :: []) -> List.map (function
      | Id nam -> update typhash nam (Std_logic_vector(hi,lo));
	      VAR ("", [nam], (BASDTYP, "logic", TYPRNG (cexpr typhash hi, cexpr typhash lo), []), "logic")
      | DeclAsgn (mem, AnyRange (lft, rght) :: []) ->
       let rec' = RECTYP (BASDTYP, "logic", TYPRNG (cexpr typhash hi, cexpr typhash lo), []) in
       VAR ("", [mem], (UNPACKADTYP, "mem", rec', TYPRNG (cexpr typhash lft, cexpr typhash rght) :: []), "logic")
      | oth -> unhand := Some oth; failwith "DeclReg555") reg_lst;
    | CaseStmt _ -> []
    | ContAsgn _ -> []
    | LoopGen1 _ -> []
    | CondGen1 _ -> []
    | GenItem _ -> []
    | AlwaysComb2 _ -> []
    | AlwaysFF _ -> []
    | AlwaysLatch _ -> []
    | AlwaysLegacy _ -> []
    | Initial _ -> []
    | Final _ -> []
    | Generate _ -> []
    | DeclInt2 id_lst -> List.map (function
	| Id nam ->  VAR ("", [nam], (BASDTYP, "logic", TYPNONE, []), "logic")
        | VarDeclAsgn (nam, expr) -> VAR ("", [nam], (BASDTYP, "logic", TYPNONE, []), "logic")
        | oth -> unhand := Some oth; failwith "DeclInt2") id_lst
    | InstDecl (typ, params, lst) -> List.flatten (List.map (function
        | (InstNameParen1 _ | InstNameParen2 _) ->  mod_template modules typhash (typ, params); []
        | Id nam -> VAR ("", [nam], (BASDTYP, "logic", TYPNONE, []), "logic") :: []
        | oth -> unhand := Some oth; failwith "InstDecl") lst);
    | Typ2 ("bool_t", [], [Id "FALSE"; Id "TRUE"]) -> []
    | Typ2 (nam, _, Id id :: []) ->  update typhash nam (Vtyp nam);
    let s = vexpr typhash (Id id) in update typhash id Vsigtyp;
     VAR ("", [id], (BASDTYP, "logic", TYPNONE, []), "logic") :: []
    | Typ3 (nam, id_lst) -> List.flatten (List.map (function
        | Id id -> VAR ("", [id], (BASDTYP, "logic", TYPNONE, []), "logic") :: []
        | oth -> unhand := Some oth; failwith "InstDecl") id_lst)
     (*
      | Typ2 (nam, _, id_lst) -> update typhash nam (Vtyp nam);
        List.map (function
	     | Id _ as itm -> let s = vexpr typhash itm in update typhash s Vsigtyp; sprintf "wire %s : %s; // 684	\n" s nam
             | DeclAsgn (id, AnyRange(lft,rght) :: AnyRange(lft',rght') :: []) -> update typhash nam (Vtyp nam);
                sprintf "wire %s; // 686	\n" id
             | oth -> unhand := Some oth; failwith "Typ2") id_lst;
    | Typ4 (nam, pkg, rng, id_lst) -> List.map (fun _ -> []) id_lst
    | Typ5 (SUDecl (Atom "packed", lst), inst_lst) ->
        sprintf "typedef struct packed { // 703	\n";
        List.map (struct_union typhash) lst;
        sprintf "} %s;\n" (String.concat ", " (List.map (vexpr typhash) inst_lst));
    | Typ6 (SUDecl (Atom "packed", lst)) ->
        sprintf "typedef struct packed { // 703	\n";
        List.map (struct_union typhash) lst;
        sprintf "};\n"
    | Typ7 (nam, Typ5 (Atom "logic", AnyRange(lft,rght) :: AnyRange(lft',rght') :: AnyRange(lft'',rght'') :: [])) -> update typhash nam (Vtyp nam);
        sprintf "wire %s; // 692	\n" nam
    | Typ7 (nam, Typ5 (Atom "logic", AnyRange(lft,rght) :: AnyRange(lft',rght') :: [])) -> update typhash nam (Vtyp nam);
        sprintf "wire %s; // 694	\n" nam
    | Typ7 (nam, Typ5 (Atom "logic", AnyRange(lft,rght) :: [])) -> update typhash nam (Vtyp nam);
        sprintf "typedef logic [%s:%s] %s; // 696	\n" (vexpr typhash lft) (vexpr typhash rght) nam
    | Typ7 (nam, Typ8 (SUDecl (Atom "packed", lst), Deflt)) -> update typhash nam (Vtyp nam);
        sprintf "typedef struct packed { // 714	\n";
        List.map (struct_union typhash) lst;
        sprintf "} %s;\n" nam
    | Typ7 (id_t, Typ8 (Union (Atom "packed", lst), Deflt)) ->
        sprintf "typedef union packed { // 718	\n";
        List.map (struct_union typhash) lst;
        sprintf "} %s;\n" id_t
    | Typ7 (id_t, Typ8 (Itmlst lst, Deflt)) ->
        sprintf "typedef struct { // 722	\n";
        List.map (struct_union typhash) lst;
        sprintf "} %s;\n" id_t
    | TypEnum4 (TypEnum3 (AnyRange(lft,rght) :: []), id_lst, id_lst') ->
        let kind = Std_logic_vector(lft,rght) in
        List.map (function Id nam -> update typhash nam kind | _ -> ()) id_lst';
        let f' itm = let s = vexpr typhash itm in update typhash s kind; s in
        let f'' = function Id nam -> sprintf "typedef enum {%s} %s; // 705\n" (String.concat ", " (List.map f' id_lst)) nam | _ -> () in
        List.map f'' id_lst'
    | TypEnum4 (TypEnum5 (Atom "logic"), id_lst, id_lst') ->
        let kind = Std_logic in
        List.map (function Id nam -> update typhash nam kind | _ -> ()) id_lst';
        let f' itm = let s = vexpr typhash itm in update typhash s kind; s in
        let f'' = function Id nam -> sprintf "    typedef enum {%s} %s; // 711\n" (String.concat ", " (List.map f' id_lst)) nam | _ -> () in
        List.map f'' id_lst'
    | TypEnum6 (nam, TypEnum5 (Atom "logic"), id_lst) -> update typhash nam (Venum nam); []
    | TypEnum6 (nam, Typ8 (Atom "int", Atom "unsigned"), id_lst) -> update typhash nam (Venum nam); []
*)
    | TypEnum6 (nam, Deflt, id_lst) -> update typhash nam (Venum nam);
        let strt = ref 0 in let enums = List.map (function
        | Id e -> update typhash e (Vemember(nam, e, Intgr(!strt))); incr strt
        | EnumInit (e, expr) ->
	    let s = vexpr typhash expr in
	    update typhash e (Vemember(nam, e, expr))
	| oth -> unhand := Some oth; failwith "TypEnum6") id_lst in
	[]
    | TypEnum6 (nam, TypEnum3 (AnyRange(lft,rght) :: []), id_lst) -> update typhash nam (Venum nam);
        let strt = ref 0 in let enums = List.map (function
        | Id e -> update typhash e (Vemember(nam, e, Intgr(!strt)));
	    incr strt
        | EnumInit (e, expr) ->
	    strt := ceval typhash expr;
	    update typhash e (Vemember(nam, e, Intgr(!strt)));
            incr strt
	| oth -> unhand := Some oth; failwith "TypEnum6") id_lst in
	[]

	(*
    | ParamDecl (Atom "localparam", [ParamAsgn1 (nam, expr)]) -> sprintf "    localparam %s = %s; // 740	\n" nam (vexpr typhash expr)
    | Typ6 (Atom "packed") -> () (* placeholder *)
    | Typ10 (id_t, AnyRange (lft, rght) :: [], id_t') -> () (* placeholder *)
    | ParamDecl (LocalParamTyp (Typ1 id), ParamAsgn1 (nam, init) :: []) ->
    (match init with
	   | InitPat lst ->
               sprintf "localparam %s %s = // 935\n" id nam;
               List.map (function
                   | AsgnPat lst -> List.map (fun itm -> sprintf "    parameter %s = %s; // 940\n" nam (vexpr typhash itm)) lst
		   | PatMember1 (Id id, AsgnPat lst) -> List.map (function
                      | PatMemberDflt expr -> sprintf "    parameter %s = %s; // 941\n" nam (vexpr typhash expr)
                      | AsgnPat (PatMemberDflt expr :: []) -> sprintf "    parameter %s = %s; // 941\n" nam (vexpr typhash expr)
		      | oth -> unhand := Some oth; failwith "ParamDecl'''") lst
		   | PatMember1 (Id id, (Id _ | Number _ | ExprOKL _ as x)) -> sprintf "    %s: %s; // 942\n" id (vexpr typhash x)
                   | (Number _ | Id _ as x) -> sprintf "    parameter %s = %s; // 941\n" nam (vexpr typhash x)
	           | oth -> unhand := Some oth; failwith "ParamDecl''") lst
           | (Number _ | Query _ | Expression _  as x) -> sprintf "    parameter %s = %s; // 943\n" nam (vexpr typhash x)
           | oth -> unhand := Some oth; failwith "ParamDecl'")
    | ParamDecl (LocalParamTyp (Typ3 (id, PackageBody (pkg, []) :: [])), ParamAsgn1 (nam, vexpr) :: []) -> ()
    | ParamDecl (LocalParamTyp (Typ3 (id_t, AnyRange (lft, rght) :: [])), ParamAsgn1 (nam, InitPat lst) :: []) -> List.map (function
        | AsgnPat lst -> List.map (fun itm -> sprintf "    parameter %s = %s; // 947\n" nam (vexpr typhash itm)) lst
	| oth -> unhand := Some oth; failwith "ParamDecl") lst
    | ParamDecl (LocalParamTyp (Typ5 (Atom "logic", AnyRange (lft, rght) :: [])), [ParamAsgn2 (nam, [AnyRange (lft', rght')], InitPat lst)]) ->
        sprintf "%s; // 773\n" nam
    | ParamDecl (LocalParamTyp (Typ5 (Atom "logic", AnyRange (lft, rght) :: AnyRange (lft', rght') :: [])), [ParamAsgn1 (nam, expr)]) ->
        sprintf "%s = %s; // 775\n" nam (vexpr typhash expr)
    | ParamDecl (LocalParamTyp (Typ5 (Atom "logic", AnyRange (lft, rght) :: [])), ParamAsgn1 (nam, expr) :: []) ->
        sprintf "localparam logic [%s:%s] %s = %s; // 777\n" (vexpr typhash lft) (vexpr typhash rght) nam (vexpr typhash expr)
    | ParamDecl (LocalParamTyp (Typ6 (Atom ("bit"|"logic"))), lst) -> List.map (function
	        | ParamAsgn1 (id, expr) -> ()
                | oth -> unhand := Some oth; failwith "param_asgn") lst
    | ParamDecl (LocalParamTyp (Typ8 (Atom ("int"|"integer"|"longint"), Deflt)), [ParamAsgn1 (nam , expr)]) ->
        sprintf "%s = %s; // 782\n" nam (vexpr typhash expr)
    | ParamDecl (LocalParamTyp (Typ8 (Atom ("int"|"integer"|"longint" as kind), Atom kind')), [ParamAsgn1 (nam , expr)]) ->
        sprintf "localparam %s %s %s = %s; // 784\n" kind' kind nam (vexpr typhash expr)
    | FunDecl (fn, typ, FunGuts (ports, lst)) ->
        sprintf "function %s %s (%s);\n" (funtyp typhash typ) fn (String.concat ", " (List.map (fn_arg typhash) ports));
        List.map (stmt_clause typhash) lst;
        sprintf "    endfunction\n";
    | AutoFunDecl (fn, typ, FunGuts (ports, lst)) ->
        sprintf "function automatic %s %s (%s);\n" (funtyp typhash typ) fn (String.concat ", " (List.map (fn_arg typhash) ports));
        List.map (stmt_clause typhash) lst;
        sprintf "    endfunction\n";
    | PkgImport (Itmlst lst) -> List.map (decl_template typhash modules cnt) lst
    | PkgImportItm (pkg, Atom "*") -> ()
    | DeclData (Typ5 (Atom "logic", AnyRange (lft, rght) :: AnyRange (lft', rght') :: []), VarDeclAsgn (mem, ExprOKL lst) :: []) -> () (* placeholder *)
    *)
    | AssertProperty -> []
    | oth -> unhand := Some oth; failwith "decl_template"

let instance_template typhash typ params inst pinlst =
 let params = List.map (parm_generic typhash) params in
INST("", MODULE, [inst], (typ, (List.mapi (fun ix -> function
  | Id id -> (match Hashtbl.find_opt typhash typ with
    | Some (Vcomp lst) -> (match List.nth lst ix with
      | IO (loc, [pin], (BASDTYP, basetyp, wid, []), dir, "logic", []) -> PORT(loc, pin, dir, vexpr typhash (Id id) :: [])
      | oth -> othinst := Some oth; failwith "instance_template'")
    | _ -> PORT("", ("__pinNumber"^string_of_int ix), Dinout, vexpr typhash (Id id) :: []))     
  | CellPinItem1 (pin, conn) -> PORT("", pin, Dinout, vexpr typhash (Id conn) :: [])
  | CellPinItem2 (pin, conn) -> PORT("", pin, Dinout, vexpr typhash conn :: [])
  | CellPinItemImplied (pin) -> PORT("", pin, Dinout, vexpr typhash (Id pin) :: [])
  | CellPinItemNC pin -> PORT("", pin, Dinout, [])
  | oth -> unhand := Some oth; failwith "instance_template") pinlst)))

let at_template typhash sent_lst = function
| At (EventOr (Pos clk :: Pos rst :: _ as dep_lst)) ->
ALWYS ("",
       (SNTRE
        (List.map (function
		   | Pos dep -> SNITM ("POS", vexpr typhash (Id dep) :: [])
		   | Neg dep -> SNITM ("NEG", vexpr typhash (Id dep) :: [])
		   | oth -> unhand := Some oth; failwith "at_template_edge") dep_lst)) ::
	  stmt_clause typhash sent_lst :: []) :: []
| At (EventOr (Id _ :: _ as dep_lst)) ->
ALWYS ("",
       (SNTRE
        (List.map (function
		   | Id dep -> SNITM ("ANY", vexpr typhash (Id dep) :: [])
		   | oth -> unhand := Some oth; failwith "at_template_comb") dep_lst)) ::
	  stmt_clause typhash sent_lst :: []) :: []
| At Deflt ->
ALWYS ("",
       (SNTRE
        (stmt_clause typhash sent_lst :: []) :: [])) :: []
| oth -> unhand := Some oth; failwith "at_template"

let rec proc_template typhash cnt = function
    | DeclReg _ -> []
    | DeclLogic _ -> []
    | AlwaysFF (at, sent_lst) -> at_template typhash sent_lst at
    | AlwaysLegacy (at, sent_lst) -> at_template typhash sent_lst at
    | AlwaysComb2 ( sent_lst) -> at_template typhash sent_lst (At Deflt)
    | AlwaysLatch ( sent_lst ) -> at_template typhash sent_lst (At Deflt)
  (* elaboration case *)
   | CaseStart (Id id, (CaseItm (BeginBlock [] :: Unknown ("$error",_) :: Deflt :: []) :: [])) -> INIT("elab case", id, []) :: []
    | ContAsgn lst -> List.map (function
      | Asgn1 (lhs, expr) -> ASGN(false, "", vexpr typhash expr :: vexpr typhash lhs :: [])
      | oth -> unhand := Some oth; failwith "assign_template") lst
  (*
    | Iff _ -> ()
	*)
    | InstDecl (typ, params, lst) -> List.map (function
        | InstNameParen1 (inst, Itmlst pins :: []) -> instance_template typhash typ (match params with Itmlst lst :: _ -> lst | _ -> []) inst pins
        | InstNameParen2 (inst, InstRange(lft,rght) :: []) -> instance_template typhash typ (match params with Itmlst lst :: _ -> lst | _ -> []) inst []
        | Id id -> VAR ("", [id], (BASDTYP, "logic", TYPRNG (HEX 2, HEX 0), []), "state_type")
        | oth -> unhand := Some oth; failwith "InstDecl") lst;
    | TypEnum4 _ -> []
    | TypEnum6 _ -> []
    | Typ2 (typ, _, typ_lst) -> []
    | Typ3 _ -> []
    | Typ4 _ -> []
    | Typ5 _ -> []
    | Typ6 _ -> []
    | Typ7 _ -> []
    | DeclInt2 _ -> []
    | NetDecl _ -> []
    | DeclReg2 _ -> []
    | DeclLogic2 _ -> []
    | LoopGen1 _ -> [] (* placeholder *)
    | CondGen1 _ -> [] (* placeholder *)
    | GenItem _ -> [] (* placeholder *)
    | Generate _ -> [] (* placeholder *)
    | ParamDecl _ -> []
    | FunDecl (fn, Atom primtyp, FunGuts (PortItem _ :: [], lst)) -> [] (* placeholder *)
    | AutoFunDecl (fn, typ, FunGuts (ports, lst)) -> [] (* placeholder *)
    | Initial _ -> INIT ("initial is not implemented", "", []) :: []
    | Final _ -> INIT("final is not implemented", "", []) :: []
    | Itmlst (Id _ :: _) -> []
    | PkgImport _ -> []
    | DeclData _ -> []
    | AssertProperty -> []
    | oth -> unhand := Some oth; failwith "proc_template"

let debughash = Hashtbl.create 255
    
let template modules = function
  | Modul(nam, parm_lst, port_lst, body_lst') -> let cnt = ref 1 in
  let body_lst = List.map (recurs {fn=recurs}) body_lst' in
  let typhash = Hashtbl.create 255 in
  print_endline ("Converting: "^nam);
  let params = parm_template typhash parm_lst in
  let typlst, othlst = List.partition (function TypEnum6 _ -> true | _ -> false) body_lst in
  let decl1 = List.map (decl_template typhash modules cnt) (typlst) in
  let components, othlst = List.partition (function InstDecl _ -> true | _ -> false) othlst in
  let decl2 = List.map (decl_template typhash modules cnt) (List.sort compare components) in
  let decl3 = List.map (decl_template typhash modules cnt) othlst in
  let proc1 = List.map (proc_template typhash cnt) body_lst in
  Hashtbl.replace debughash nam typhash;
  MODUL ("", nam, (* params *) List.map (ports typhash) port_lst
     @ List.flatten decl1
     @ List.flatten decl2
     @ List.flatten decl3
     @ List.flatten proc1, [])

  | PackageBody (pkg, body_lst') -> let cnt = ref 1 in
  let body_lst = List.map (recurs {fn=recurs}) body_lst' in
  let typhash = Hashtbl.create 255 in
  PKG ("", pkg, List.flatten (List.map (decl_template typhash modules cnt) body_lst))
  | oth -> unhand := Some oth; failwith "This template only handles modules/packages"

let template_header modlst xml = XML
 [FILS ("files",
   [FIL ("c", "../outputparser/apb_uart.sv"); FIL ("a", "&lt;built-in&gt;");
    FIL ("b", "&lt;command-line&gt;")]);
  FILS ("module_files", [FIL ("c", "../outputparser/apb_uart.sv")]);
  NTL xml;
  CELLS ([CELL ("c33", "apb_uart", "apb_uart", "apb_uart",
      [CELL ("c222", "UART_IS_SIN", "slib_input_sync",
        "apb_uart.UART_IS_SIN", []);
       CELL ("c223", "UART_IS_CTS", "slib_input_sync",
        "apb_uart.UART_IS_CTS", []);
       CELL ("c224", "UART_IS_DSR", "slib_input_sync",
        "apb_uart.UART_IS_DSR", []);
       CELL ("c225", "UART_IS_DCD", "slib_input_sync",
        "apb_uart.UART_IS_DCD", []);
       CELL ("c226", "UART_IS_RI", "slib_input_sync", "apb_uart.UART_IS_RI",
        []);
       CELL ("c227", "UART_IF_CTS", "slib_input_filter",
        "apb_uart.UART_IF_CTS", []);
       CELL ("c228", "UART_IF_DSR", "slib_input_filter",
        "apb_uart.UART_IF_DSR", []);
       CELL ("c229", "UART_IF_DCD", "slib_input_filter",
        "apb_uart.UART_IF_DCD", []);
       CELL ("c230", "UART_IF_RI", "slib_input_filter",
        "apb_uart.UART_IF_RI", []);
       CELL ("c270", "UART_IIC", "uart_interrupt", "apb_uart.UART_IIC", []);
       CELL ("c282", "UART_IIC_THRE_ED", "slib_edge_detect",
        "apb_uart.UART_IIC_THRE_ED", []);
       CELL ("c511", "UART_PEDET", "slib_edge_detect", "apb_uart.UART_PEDET",
        []);
       CELL ("c512", "UART_FEDET", "slib_edge_detect", "apb_uart.UART_FEDET",
        []);
       CELL ("c513", "UART_BIDET", "slib_edge_detect", "apb_uart.UART_BIDET",
        []);
       CELL ("c533", "UART_ED_CTS", "slib_edge_detect",
        "apb_uart.UART_ED_CTS", []);
       CELL ("c539", "UART_ED_DSR", "slib_edge_detect",
        "apb_uart.UART_ED_DSR", []);
       CELL ("c545", "UART_ED_RI", "slib_edge_detect", "apb_uart.UART_ED_RI",
        []);
       CELL ("c551", "UART_ED_DCD", "slib_edge_detect",
        "apb_uart.UART_ED_DCD", []);
       CELL ("c627", "UART_BG16", "uart_baudgen", "apb_uart.UART_BG16", []);
       CELL ("c634", "UART_BG2", "slib_clock_div", "apb_uart.UART_BG2", []);
       CELL ("c639", "UART_RCLK", "slib_edge_detect", "apb_uart.UART_RCLK",
        []);
       CELL ("c645", "UART_TXFF", "slib_fifo", "apb_uart.UART_TXFF", []);
       CELL ("c660", "UART_RXFF", "slib_fifo", "apb_uart.UART_RXFF", []);
       CELL ("c678", "UART_TX", "uart_transmitter", "apb_uart.UART_TX", []);
       CELL ("c694", "UART_RX", "uart_receiver", "apb_uart.UART_RX",
        [CELL ("c1767", "RX_BRC", "slib_counter", "apb_uart.UART_RX.RX_BRC",
          []);
         CELL ("c1777", "RX_MVF", "slib_mv_filter",
          "apb_uart.UART_RX.RX_MVF", []);
         CELL ("c1784", "RX_IFSB", "slib_input_filter",
          "apb_uart.UART_RX.RX_IFSB", [])])])], Vxml.empty_attr(ref []))]
