open Source_text_rewrite_types
open Input_rewrite_types
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
  | MaybePort of int
  | Vemember of string * string * rw
  | Task of rw * rw * rw

type bufh = {c:ilang list ref;
(*
    d:ilang list ref;
*)
    i:ilang list ref;
    l:ilang list ref;
(*
    o:ilang list ref;
    r:ilang list ref;
    s:ilang list ref;
*)
    w:ilang list ref}

let unhand = ref None
let update typhash id expr =
  Hashtbl.replace typhash id expr

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
| Vtyp s -> print_endline ("Sizeof type "^s^" evaluated to zero"); 0
| MaybePort n -> print_endline ("Sizeof type MaybePort "^string_of_int n^" evaluated to one"); 1
| Venum s -> 8
| Vemember(s, _, _) -> csiz' typhash (match Hashtbl.find_opt typhash s with Some x -> x | None -> print_endline ("Not found: "^s); Std_logic)

| oth -> coth := Some oth; failwith "csiz'"

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
| IdArrayed2 (Id s, _) -> csiz' typhash (match Hashtbl.find_opt typhash s with Some x -> x | None -> print_endline ("Not found: "^s); Std_logic)
| Add (lhs, rhs) -> max (width typhash lhs) (width typhash rhs)
| Sub (lhs, rhs) -> max (width typhash lhs) (width typhash rhs)
| Mult (lhs, rhs) -> width typhash lhs + width typhash rhs
| Div (lhs, rhs) -> width typhash lhs
| And (lhs, rhs) -> min (width typhash lhs) (width typhash rhs)
| And2 _ -> 1
| Or (lhs, rhs) -> max (width typhash lhs) (width typhash rhs)
| Or2 _ -> 1
| Pling _ -> 1
| Xor (lhs, rhs) -> max (width typhash lhs) (width typhash rhs)
| Less (lhs, rhs) -> 1
| LtEq (lhs, rhs) -> 1
| Equals (lhs, rhs) -> 1
| NotEq (lhs, rhs) -> 1
| GtEq (lhs, rhs) -> 1
| Greater (lhs, rhs) -> 1
| Atom "default" -> 1
| Query (lhs, rhs, rhs') -> max (width typhash rhs) (width typhash rhs')
| StarStar (lhs, rhs) -> (width typhash lhs) + ceval typhash rhs
| Sys ("$clog2", x) -> clog2 (width typhash x)
| Expression x -> width typhash x
| PackageBody (pkg, [id]) -> width typhash id (* placeholder *)
| Sys ("$bits", Typ1 id_t) -> csiz' typhash (match Hashtbl.find_opt typhash id_t with Some x -> x | None -> print_endline ("Not found: "^id_t); Std_logic)
| Sys ("$size", Dot1 _) -> 1 (* placeholder *)
| ExprOKL [] -> 0
| ExprOKL (hd::tl) -> width typhash hd + width typhash (ExprOKL tl)
| oth -> unhand := Some oth; failwith "width"

let rec recurs t (attr: Source_text_rewrite.attr) = function
  | If2(cond, if_clause, else_clause) ->
      CaseStart (CaseStart1 (cond),
       (CaseStmt ([Number(2,1,1,"")],
         recurs t attr if_clause :: []) ::
        CaseStmt ([Number(2,1,0,"")],
         recurs t attr else_clause :: []) ::
        []))
  | If1(cond, if_clause) ->
      CaseStart (CaseStart1 (cond),
       (CaseStmt ([Number(2,1,1,"")],
         recurs t attr if_clause :: []) ::
         []))
  | EquateSelect(lhs, sel, expr) as x ->
      let wid = width t (Id lhs) in
      print_endline ("wid="^string_of_int wid);
      let foreach = List.init wid (fun ix -> let sel = Number(2,clog2 wid,ix,"") in CaseStmt ([], EquateSelect(lhs, sel, expr) :: [])) in
      CaseStart (CaseStart1 (sel), foreach)
  | oth -> rfn t attr oth

and rfn t attr x = print_endline (Source_text_rewrite.getstr x); Source_text_rewrite.descend' {attr with fn=recurs t} x

let sub' typhash = List.map (recurs typhash {fn=recurs typhash; subst=Hashtbl.create 255})

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
    | Some (Vint _|Vtyp _|Venum _|Vsigtyp|Vpkg _|Vfun _|Vintf _|Vstr _|Vdot|Vemember _|MaybePort _) -> vexpr typhash x
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

let addwire bufh = function
| (options, nam) ->
  if List.filter (function Wire_stmt (_, id) -> id=nam | _ -> false) !(bufh.w) <> [] then
    print_endline ("Warning: "^nam^" is already defined, could be obsolete syntax")
  else
    bufh.w := Wire_stmt(options, nam) :: !(bufh.w)

let newid bufh typhash wid =
  let nam = newnam() in
  let rslt = Id nam in
  update typhash nam (Std_logic_vector(Intgr (wid-1),Intgr 0));
  addwire bufh ([Wire_optionswidth wid], nam);
  rslt


let rec parm_map typhash = function  
  | CellParamItem2 (nam, Number (_, _, n, _)) ->
      update typhash nam (Vint n); 
      TokParam([TokID ("\\"^nam)], [TokInt n])
      (*
  | CellParamItem1 (nam, s) ->
      update typhash nam (match Hashtbl.find_opt typhash s with Some x -> x | None -> Std_logic);
      sprintf "%24s         : string := %s" nam s
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
*)
  | oth -> unhand := Some oth; failwith "parm_map"

and instance_template bufh typhash typ params inst pinlst =
        Cell_stmt ("$"^typ, inst, [],
        List.map (parm_map typhash) params @
        List.map (function
		   | CellPinItem2 (pin, Number(b,w,n,_)) -> TokConn([TokID("\\"^pin)], [TokVal(sprintf "%d'%s\n" w (obin w n))])
		   | CellPinItem2 (pin, Id conn) -> TokConn([TokID("\\"^pin)], [TokID conn ])
		   | CellPinItem1 (pin, conn) -> TokConn([TokID("\\"^pin)], [TokID conn])
                   | oth -> unhand := Some oth; failwith "inst_arg") pinlst)

let rec asgnexpr' bufh typhash wid = function
  | (Number _ | Intgr _) as x -> x
  | Add (lhs, rhs) -> dyadic bufh typhash wid "add" [lhs;rhs] "ABY"
  | Sub (lhs, rhs) -> dyadic bufh typhash wid "sub" [lhs;rhs] "ABY"
  | Mult (lhs, rhs) -> dyadic bufh typhash wid "mul" [lhs;rhs] "ABY"
  | Div (lhs, rhs) -> dyadic bufh typhash wid "div" [lhs;rhs] "ABY"
  | And (lhs, rhs) -> dyadic bufh typhash wid "and" [lhs;rhs] "ABY"
  | And2 (lhs, rhs) -> dyadic bufh typhash wid "logic_and" [lhs;rhs] "ABY"
  | Or (lhs, rhs) -> dyadic bufh typhash wid "or" [lhs;rhs] "ABY"
  | Or2 (lhs, rhs) -> dyadic bufh typhash wid "logic_or" [lhs;rhs] "ABY"
  | Xor (lhs, rhs) -> dyadic bufh typhash wid "xor" [lhs;rhs] "ABY"
  | Less (lhs, rhs) -> dyadic bufh typhash wid "lt" [lhs;rhs] "ABY"
  | LtEq (lhs, rhs) -> dyadic bufh typhash wid "le" [lhs;rhs] "ABY"
  | Equals (lhs, rhs) -> dyadic bufh typhash wid "eq" [lhs;rhs] "ABY"
  | NotEq (lhs, rhs) -> dyadic bufh typhash wid "ne" [lhs;rhs] "ABY"
  | GtEq (lhs, rhs) -> dyadic bufh typhash wid "ge" [lhs;rhs] "ABY"
  | Greater (lhs, rhs) -> dyadic bufh typhash wid "gt" [lhs;rhs] "ABY"
  | Id s as x -> let oldw = width typhash x in if oldw < wid then Concat (Number(2,wid-oldw,0,"") :: Id s :: []) else Id s
  | Expression x -> asgnexpr bufh typhash x
  | Tilde expr -> let rhs = asgnexpr bufh typhash expr and rslt = newid bufh typhash wid in addprim bufh typhash "$tilde" [] [rhs;rslt] "AY"; rslt
  | Pling expr -> let rhs = asgnexpr bufh typhash expr and rslt = newid bufh typhash wid in addprim bufh typhash "$not" [] [rhs;rslt] "AY"; rslt
  | IdArrayed2 (id, ix) as x -> x
  | Query (cond', ctrue', cfalse') -> ternary bufh typhash wid "mux" [cfalse'; ctrue'; cond'] "ABSY"
(*
  | Pling expr -> "not " ^ (asgnexpr bufh typhash) expr
  | Concat lst -> String.concat " & " (List.map (asgnexpr bufh typhash) lst)
  | StarStar (lhs, rhs) -> asgnexpr bufh typhash lhs ^ " ** " ^ asgnexpr bufh typhash rhs
  | UMinus (rhs) -> " - " ^ asgnexpr bufh typhash rhs
  | Unsigned expr -> "unsigned("^asgnexpr bufh typhash expr^")"
  | Shiftl (lhs, rhs) -> "shift_left("^asgnexpr bufh typhash lhs^", "^asgnexpr bufh typhash rhs^")"
  | Shiftr (lhs, rhs) -> "shift_right("^asgnexpr bufh typhash lhs^", "^asgnexpr bufh typhash rhs^")"
  | Shiftr3 (lhs, rhs) -> "shift_right3("^asgnexpr bufh typhash lhs^", "^asgnexpr bufh typhash rhs^")"
  | CellPinItem1 (port, conn) -> port ^ " => " ^ conn
  | CellPinItem2 (port, expr) -> port ^ " => " ^ asgnexpr bufh typhash expr
  | CellPinItemImplied (port) -> port ^ " => " ^ port
  | CellPinItemNC (port) -> port ^ " => open"
  | Query (Id cond', ctrue', cfalse') -> sprintf "%s ? %s : %s" cond' (asgnexpr bufh typhash ctrue') (asgnexpr bufh typhash cfalse')
  | Deflt -> "open"
  | Dot1(lft, rght) -> asgnexpr bufh typhash lft^"."^asgnexpr bufh typhash rght
  | RedOr (lhs) -> " or (" ^ asgnexpr bufh typhash lhs ^ ")"
  | RedAnd (lhs) -> " and (" ^ asgnexpr bufh typhash lhs ^ ")"
  | RedXor (lhs) -> " xor (" ^ asgnexpr bufh typhash lhs ^ ")"
  | TildeOr (lhs) -> " nor (" ^ asgnexpr bufh typhash lhs ^ ")"
  | IdArrayed3 (PackageBody (pkg, []) :: [], arr) -> pkg^"::"^asgnexpr bufh typhash arr
  | IdArrayedColon (id, expr, expr') -> asgnexpr bufh typhash id^"["^asgnexpr bufh typhash expr^" : "^asgnexpr bufh typhash expr'^"]"
  | IdArrayedPlusColon (id, expr, expr') -> asgnexpr bufh typhash id^"["^asgnexpr bufh typhash expr^" : "^asgnexpr bufh typhash expr'^"]"
  | IdArrayedHyphenColon (id, expr, expr') -> asgnexpr bufh typhash id^"["^asgnexpr bufh typhash expr^" : "^asgnexpr bufh typhash expr'^"]"
  | FunRef (fn, arglst) -> fn^"("^String.concat ", " (List.map (asgnexpr bufh typhash) arglst)^")"
  | FunRef2 (fn, _, arglst) -> fn^"("^String.concat ", " (List.map (asgnexpr bufh typhash) arglst)^")"
  | AsgnPat [PatMemberDflt (Number _ as expr)] -> "others => "^(asgnexpr bufh typhash expr)
  | Repl (expr', [expr]) -> "{{"^(asgnexpr bufh typhash expr')^"}{"^(asgnexpr bufh typhash expr)^"}}"
  | InsideRange (first, last) -> "inside_range("^(asgnexpr bufh typhash first)^", "^(asgnexpr bufh typhash last)^")"
  | OpenRange lst -> "open_range("^String.concat ", " (List.map (asgnexpr bufh typhash) lst)^")"
  | ExprOKL lst -> " {" ^ String.concat ", " (List.map (asgnexpr bufh typhash) lst) ^ "} "
  | PackageBody (pkg, [Id id]) -> pkg^"::"^id
  | ExprQuote1(lhs, rhs) -> asgnexpr bufh typhash lhs^"'("^asgnexpr bufh typhash rhs^")"
  | Sys (sys_id, arglst) -> let args = (match arglst with
	  | Itmlst lst -> String.concat ", " (List.map (asgnexpr bufh typhash) lst)
	  | oth -> asgnexpr bufh typhash oth) in
      String.sub sys_id 1 (String.length sys_id - 1) ^ "(" ^ args ^")"
  | Typ3 (id_t, [PackageBody (pkg, [])]) -> pkg^"::"^id_t
  | PackageBody (pkg, []) -> pkg^"::"
  | Atom ".*" -> "" (* placeholder *)
  | Atom kind -> kind (* placeholder *)
  | Typ1 id_t -> id_t
  | AsgnPat lst -> String.concat "; " (List.map (asgnexpr bufh typhash) lst)
  | PatMember1 (Id id, expr) -> id ^ " = " ^ asgnexpr bufh typhash expr
  | PatMemberDflt expr -> asgnexpr bufh typhash expr
  | ValueRange (lft, rght) -> "["^asgnexpr bufh typhash lft^" .. "^asgnexpr bufh typhash rght^"]"
  | String s -> s
*)
  | oth -> unhand := Some oth; failwith "asgnexpr bufh"

and asgnexpr bufh typhash x = asgnexpr' bufh typhash (width typhash x) x

and addprim bufh typhash typ params args templ = 
  print_endline ("addprim: "^typ);
  let wid' ix arg = CellParamItem2 (String.make 1 templ.[ix]^"_WIDTH", Number(10, 32, width typhash arg, "")) in
  let conn' ix arg = let itm = asgnexpr bufh typhash arg in CellPinItem2(String.make 1 templ.[ix], itm) in
  let widths = List.mapi wid' args in
  dumpi bufh typhash (typ, [Itmlst (params@widths)], (InstNameParen1 (newnam(), Itmlst (List.mapi conn' args) :: []) :: []))

and dyadic bufh typhash wid func args pnam =
  let rslt = newid bufh typhash wid in
  addprim bufh typhash func (List.map (fun itm -> CellParamItem2 (itm^"_SIGNED", Number (10, 32, 0, ""))) ["A";"B"]) (args@[rslt]) pnam;
  rslt

and ternary bufh typhash wid func args pnam =
  let rslt = newid bufh typhash wid in
  addprim bufh typhash func (List.map (fun itm -> CellParamItem2 (itm^"_SIGNED", Number (10, 32, 0, ""))) ["A";"B";"S"]) (args@[rslt]) pnam;
  rslt

and dumpi bufh typhash (typ, params, lst) = List.iter (function
        | InstNameParen1 (inst, Itmlst pins :: []) ->
	    bufh.i := (instance_template bufh typhash typ (match params with Itmlst lst :: _ -> lst | _ -> []) inst pins) :: !(bufh.i)
        | InstNameParen2 (inst, InstRange(lft,rght) :: []) ->
	    bufh.i := (instance_template bufh typhash typ (match params with Itmlst lst :: _ -> lst | _ -> []) inst []) :: !(bufh.i)
        | Id id -> addwire bufh ([], id)
        | oth -> unhand := Some oth; failwith "InstDecl'") lst

let sel_expr typhash x = match simplify x with Intgr _ -> "'0'" | oth -> vexpr typhash oth

let vdir ix = function
  | In -> Wire_optionsinput ix
  | Out -> Wire_optionsoutput ix
  | Inout -> Wire_optionsinout ix
  | Deflt -> Wire_optionsinvalid
  | oth -> unhand := Some oth; failwith "vdir"

let ports' typhash ix hi lo dir nam =
  let wid = ceval typhash hi - ceval typhash lo +1 in
  update typhash nam (Std_logic_vector(hi,lo));
  [Wire_stmt (Wire_optionswidth wid :: vdir ix dir :: [], nam)]

let maybe_port typhash nam ix dir = 
  let dir' = vdir ix dir in
  if dir' <> Wire_optionsinvalid then (update typhash nam Std_logic; [Wire_stmt (dir' :: [], nam)])
  else (update typhash nam (MaybePort ix); [])

let ports typhash ix = function
    | Port((In|Out|Inout|Deflt) as dir, nam, [], []) -> maybe_port typhash nam ix dir
    | Port(PortDir((In|Out|Inout) as dir, Atom ("wire"|"reg")), nam, [], []) -> maybe_port typhash nam ix dir
    | Port ((In|Out|Inout) as dir, nam, AnyRange (hi, lo) :: [], []) -> ports' typhash ix hi lo dir nam              
    | Port (PortDir((In|Out|Inout) as dir, Atom "wire"), nam, AnyRange (hi, lo) :: [], []) -> ports' typhash ix hi lo dir nam              
    | Port ((In|Out|Inout) as dir, nam, AnyRange (hi, lo) :: AnyRange(hi', lo') :: [], []) -> ports' typhash ix hi lo dir nam              
    | Port ((In|Out|Inout) as dir, nam, AnyRange (hi, lo) :: AnyRange(hi', lo') :: AnyRange(hi'', lo'') :: [], []) -> ports' typhash ix hi lo dir nam
    | Port ((In|Out|Inout) as dir, nam, Typ6 (Atom primtyp) :: [], []) -> maybe_port typhash nam ix dir
    | Port (Deflt, nam, Typ2 (typ_t, PackageBody (pkg, []) :: [], []) :: AnyRange (hi, lo) :: [], []) -> update typhash nam (Vtyp(typ_t));
        ports' typhash ix hi lo Inout nam              
(*
    | Port ((In|Out|Inout) as dir, nam, Typ2 (typ_t, [], []) :: AnyRange (hi, lo) :: [], []) -> update typhash nam (Vtyp(typ_t));
        ports' typhash ix hi lo dir nam              
    | Port ((In|Out|Inout) as dir, nam, Typ2 (typ_t, [], []) :: [], []) -> update typhash nam (Vtyp(typ_t));
        Wire_stmt (vdir ix dir :: [], nam)
    | Port ((In|Out|Inout) as dir, nam, Typ2 (typ_e, (PackageBody (pkg, [])) :: [], []) :: [], []) -> update typhash nam (Vpkg(pkg, typ_e));
        Wire_stmt (vdir ix dir :: [], nam)
    | Port ((In|Out|Inout) as dir, nam, Typ2 (typ_e, (PackageBody (pkg, [])) :: [], []) :: AnyRange (hi, lo) :: [], []) -> update typhash nam (Std_logic_vector(hi, lo));
        ports' typhash ix hi lo dir nam              
    | Dot3 (bus, dir, member) -> update typhash member (Vintf bus);
        sprintf "  wire %s %s \\%s" member bus dir
*)
    | DotBus (bus, dir, member, AnyRange(hi,lo) :: []) -> update typhash member (Vintf bus);
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
(*
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
		   *)
		   | Deflt -> ""
                   | oth -> unhand := Some oth; failwith "fn_arg"		   

let buffer' bufh typhash expr wid =
  let wid' = max wid (width typhash expr) in
  let rhs = asgnexpr' bufh typhash wid' expr in
  rhs

let buffer bufh typhash expr lhs = buffer' bufh typhash expr (width typhash (Id lhs))

let vsel' n = function Id lhs -> [Sigspec90 (lhs, n)] | oth -> unhand := Some oth; failwith "vsel'"

let rec tran' = function
   | Id id -> TokID id :: []
   | Number (b,w,n,_) -> TokVal (sprintf "%d'%s" w (obin w n)) :: []
   | Atom "default" -> []
   | Concat lst -> List.flatten (List.map tran' lst)
   | ExprOKL lst -> List.flatten (List.map tran' lst)
   | oth -> unhand := Some oth; failwith "tran'"

let asgn bufh typhash expr = function
| Id lhs -> let rhs = buffer bufh typhash expr lhs in bufh.c := Conn_stmt96([TokID lhs], tran' rhs) :: !(bufh.c)
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

let range typhash nam = function
    | AnyRange(hi,lo) ->
        update typhash nam (Std_logic_vector(hi,lo));
        let wid = ceval typhash hi - ceval typhash lo + 1 in Wire_optionswidth wid
    | oth -> unhand := Some oth; failwith "range"

let portpos typhash nam =
  match Hashtbl.find_opt typhash nam with Some (MaybePort ix) -> ix | _ -> 0

let elabenum typhash nam id_lst = update typhash nam (Venum nam);
    let strt = ref 0 in List.iter (function
        | Id e -> update typhash e (Vemember(nam, e, Intgr(!strt)));
	    incr strt
        | EnumInit (e, expr) ->
	    strt := ceval typhash expr;
	    update typhash e (Vemember(nam, e, Intgr(!strt)));
            incr strt
	| oth -> unhand := Some oth; failwith "TypEnum6") id_lst

let rec decl_template bufh typhash modules cnt = function
    | Port(PortDir(dir, Atom kind), nam, [], []) -> addwire bufh ([vdir (portpos typhash nam) dir], nam)
    | Port(dir, nam, [], []) -> addwire bufh ([vdir (portpos typhash nam) dir], nam)
    | Itmlst (Port(dir, nam, [], []) :: _ as lst) ->
        List.iter (function
		   | Port(PortDir(dir, Atom kind), nam, [], []) -> addwire bufh ([vdir (portpos typhash nam) dir], nam)
		   | Port(dir, nam, [], []) -> addwire bufh ([vdir (portpos typhash nam) dir], nam)
                   | oth -> unhand := Some oth; failwith "Port Itmlst") lst
    | Port(PortDir(dir, Atom kind), nam, [AnyRange _ as x], []) -> addwire bufh (range typhash nam x :: [vdir (portpos typhash nam) dir], nam)
    | Port(dir, nam, [AnyRange _ as x], []) -> addwire bufh (range typhash nam x :: [vdir (portpos typhash nam) dir], nam)
    | DeclReg (reg_lst, [], []) -> List.iter (function
      | Id nam -> addwire bufh ([], nam);
      | VarDeclAsgn (nam, expr) -> addwire bufh ([], nam)
      | oth -> unhand := Some oth; failwith "DeclReg550") reg_lst
    | DeclReg2 (reg_lst, [AnyRange _ as x]) -> List.iter (function
      | Id nam -> addwire bufh ([range typhash nam x], nam);
      | DeclAsgn (mem, [AnyRange _ as x]) -> addwire bufh ([range typhash mem x], mem)
      | VarDeclAsgn (nam, expr) -> addwire bufh ([], nam)
      | oth -> unhand := Some oth; failwith "DeclReg555") reg_lst;
    | TypEnum4 (Deflt, id_lst, [Id nam]) -> elabenum typhash nam id_lst
    | TaskDecl(nam, arg1, arg2, arg3) -> update typhash nam (Task(arg1,arg2,arg3))
(*
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
    | AssertProperty -> ()
    | oth -> unhand := Some oth; failwith "decl_template"

let rec sent_template (buf':ilang ref) typhash clk = function
    | BeginBlock lst -> List.iter (sent_template buf' typhash clk) lst
    | Seq (lbl, lst) -> List.iter (sent_template buf' typhash clk) lst
    (*
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
    *)
	   | oth -> unhand := Some oth; failwith "sent_template"

let dlymemo bufh dhash typhash lhs =
    match Hashtbl.find_opt dhash lhs with Some x -> x | None ->
        let dly = newid bufh typhash (width typhash lhs) in
        Hashtbl.add dhash lhs dly;
        dly
	
let bufhash () = {c=ref[];i=ref[];l=ref[];w=ref[]}

let catbuf bufh = !(bufh.w) @ !(bufh.i) @ !(bufh.c) @ !(bufh.l)

let copybuf bufh bufh' =
        bufh.c := !(bufh'.c) @ !(bufh.c);
        bufh'.c := [];
        bufh.i := !(bufh'.i) @ !(bufh.i);
        bufh'.i := [];
        bufh.l := !(bufh'.l) @ !(bufh.l);
        bufh'.l := [];
        bufh.w := !(bufh'.w) @ !(bufh.w);
        bufh'.w := [];
        bufh.l := !(bufh'.l) @ !(bufh.l)

let dbgcase = ref []

let restrict' typhash wid = function
  | Vemember(s, _, Number(b,w,n,_)) -> Number(b,min w wid,n mod (1 lsl wid),"")
  | Vemember(s, _, Intgr n) -> Number(2,wid,n mod (1 lsl wid),"")
  | oth -> coth := Some oth; failwith "restrict'"

let restrict typhash wid = function
  | Number(b,w,n,_) -> Number(b,min w wid,n mod (1 lsl wid),"")
  | Id s -> (match Hashtbl.find_opt typhash s with Some x -> restrict' typhash wid x | None -> Id s)
  | Atom "default" as x -> x

  | oth -> unhand := Some oth; failwith "restrict"

let rec cnv' bufh dhash typhash inst = function
    | Atom ";" -> ([],[],[])
    | Seq ("", lst) ->
        let lst' = List.map (cnv' bufh dhash typhash inst) lst in
        (List.flatten (List.map (fun (p,u,d) -> p) lst'),
         List.flatten (List.map (fun (p,u,d) -> u) lst'),
         List.flatten (List.map (fun (p,u,d) -> d) lst'))
    | EquateConcat(lhslst, expr) ->
        let wid = ref 0 in
        let dlylst = List.map (function
          | Id lhs ->
          let dly = dlymemo bufh dhash typhash (Id lhs) in
	  wid := !wid + width typhash (Id lhs);
          (Assign_stmt67 (tran' dly, [TokID lhs]),
           TokUpdate ([TokID lhs], tran' dly),
          dly)
	  | oth -> unhand := Some oth; failwith "concat") lhslst in
        let rhs = buffer' bufh typhash expr !wid in
        (List.map (fun (p,u,d) -> p) dlylst,
		  Assign_stmt67 ([Sigspec92 (List.flatten (List.map (fun (p,u,d) -> tran' d) dlylst))], [Sigspec92 (tran' rhs)]) :: [],
		  List.map (fun (p,u,d) -> u) dlylst)
    | Equate (lhs, (Number _ as expr)) ->
        let dly = dlymemo bufh dhash typhash (Id lhs) in
        (Assign_stmt67 (tran' dly, [TokID lhs]) :: [],
         Assign_stmt67 (tran' dly, tran' expr) :: [],
         TokUpdate ([TokID lhs], tran' dly) :: [])
    | Equate (lhs, expr) -> 
        let dly = dlymemo bufh dhash typhash (Id lhs) in
        let rhs = buffer bufh typhash expr lhs in
        (Assign_stmt67 (tran' dly, [TokID lhs]) :: [],
         Assign_stmt67 (tran' dly, tran' rhs) :: [],
         TokUpdate ([TokID lhs], tran' dly) :: [])
    | EquateSelect (lhs, Number(_,_,n,_), expr) ->
        let dly = dlymemo bufh dhash typhash (Id lhs) in
        (Assign_stmt67 (tran' dly, [TokID lhs]) :: [],
         Assign_stmt67 (vsel' n dly, tran' expr) :: [],
         TokUpdate ([TokID lhs], tran' dly) :: [])
    | Blocking (FopAsgn (lhs, (Number _ as expr))) ->
        let dly = dlymemo bufh dhash typhash (Id lhs) in
        (Assign_stmt67 (tran' dly, [TokID lhs]) :: [],
         Assign_stmt67 (tran' dly, tran' expr) :: [],
         TokUpdate ([TokID lhs], tran' dly) :: [])
    | Blocking (FopAsgn (lhs, expr)) -> 
        let dly = dlymemo bufh dhash typhash (Id lhs) in
        let rhs = buffer bufh typhash expr lhs in
        (Assign_stmt67 (tran' dly, [TokID lhs]) :: [],
         Assign_stmt67 (tran' dly, tran' rhs) :: [],
         TokUpdate ([TokID lhs], tran' dly) :: [])
    | CaseStmt (caselbl, itm_stmts) ->
        let lst' = List.map (cnv' bufh dhash typhash inst) itm_stmts in
	let wid = width typhash inst in
        (List.flatten (List.map (fun (p,u,d) -> p) lst'),
         Switch_bodycase (List.flatten(List.map (fun itm -> tran' (restrict typhash wid itm)) caselbl), [], List.flatten (List.map (fun (p,u,d) -> u) lst')) :: [],
         List.flatten (List.map (fun (p,u,d) -> d) lst'))
    | CaseStart (CaseStart1 expr, stmts) ->
        let dly = dlymemo bufh dhash typhash expr in
        let lst' = List.map (cnv' bufh dhash typhash expr) stmts in
        dbgcase := lst';
        (List.flatten (List.map (fun (p,u,d) -> p) lst'),
         Switch_stmt (tran' dly, [], [], (List.flatten (List.map (fun (p,u,d) -> u) lst'))) :: [],
         List.flatten (List.map (fun (p,u,d) -> d) lst'))
    | Id t when Hashtbl.mem typhash t -> print_endline t; ([],[],[])
    | oth -> unhand := Some oth; failwith "cnv'"

let dbgproc = ref None

let mapedge sync_lst = function
  | Pos signal -> Sync_list69 ([TokPos], [TokID signal], [], sync_lst)
  | Neg signal -> Sync_list69 ([TokNeg], [TokID signal], [], sync_lst)

let rec proc_template bufh typhash cnt = function
    | DeclReg _ -> ()
    | DeclLogic _ -> ()
    | AlwaysLegacy (At (EventOr ((Pos _|Neg _) :: _ as edglst)), body) ->
    let dhash = Hashtbl.create 255 in
    let inst = newnam() in
    dbgproc := Some (typhash, dhash, inst, edglst, body);
    let (p,u,s) = cnv' bufh dhash typhash (Id inst) body in
    let sync_lst = List.sort_uniq compare s in
    bufh.l := Proc_stmt (inst, [], (List.sort_uniq compare p) @ u, List.map (mapedge sync_lst) edglst) :: !(bufh.l)
    | AlwaysLegacy (AtStar, body) -> let edglst = [] in 
    let dhash = Hashtbl.create 255 in
    let inst = newnam() in
    dbgproc := Some (typhash, dhash, inst, edglst, body);
    let (p,u,s) = cnv' bufh dhash typhash (Id inst) body in
    let sync_lst = List.sort_uniq compare s in
    bufh.l := Proc_stmt (inst, [], (List.sort_uniq compare p) @ u, List.map (mapedge sync_lst) edglst) :: !(bufh.l)
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
   | CaseStart (Id id, (CaseItm (BeginBlock [] :: Unknown ("$error",_) :: Deflt :: []) :: [])) -> bufh.l := Switch_bodycase([], [], []) :: !(bufh.l)
    | ContAsgn lst -> List.iter (function
      | Asgn1 (lhs, expr) -> asgn bufh typhash expr lhs
      | oth -> unhand := Some oth; failwith "assign_template") lst
    | Iff _ -> ()
    | InstDecl (typ, params, lst) -> List.iter (function
        | InstNameParen1 (inst, Itmlst pins :: []) -> bufh.i := instance_template bufh typhash typ (match params with Itmlst lst :: _ -> lst | _ -> []) inst pins :: !(bufh.i)
        | InstNameParen2 (inst, InstRange(lft,rght) :: []) -> bufh.i := instance_template bufh typhash typ (match params with Itmlst lst :: _ -> lst | _ -> []) inst [] :: !(bufh.i)
        | Id id -> addwire bufh ([], id)
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
    | TaskDecl _ -> ()
    | FunDecl (fn, Atom primtyp, FunGuts (PortItem _ :: [], lst)) -> () (* placeholder *)
    | AutoFunDecl (fn, typ, FunGuts (ports, lst)) -> () (* placeholder *)
    | Initial _ -> bufh.l := TokStr "// initial is not implemented\n" :: !(bufh.l)
    | Final _ -> bufh.l := TokStr "// final is not implemented\n" :: !(bufh.l)
    | Itmlst ((Id _|Port _) :: _) -> ()
    | PkgImport _ -> ()
    | DeclData _ -> ()
    | AssertProperty -> ()
    | Port _ -> ()
    | oth -> unhand := Some oth; failwith "proc_template"

let body = ref []
let body' = ref []

let template modules = function
  | Modul(nam, parm_lst, port_lst, body_lst') -> let cnt = ref 1 in
  let typhash = Hashtbl.create 255 in
  let dhash = (Hashtbl.create 255, Hashtbl.create 255) in
  let bufh = bufhash () in
  let bufm = ref [] in
  bufm := Attr_stmt ("\\cells_not_processed", [TokInt 1]) :: !(bufm);
  let ports' = List.flatten(List.mapi (fun ix itm -> ports typhash (ix+1) itm) port_lst) in
  let typlst, othlst = List.partition (function TypEnum6 _ -> true | _ -> false) body_lst' in
  List.iter (decl_template bufh typhash modules cnt) (typlst);
  let components, othlst = List.partition (function InstDecl _ -> true | _ -> false) othlst in
  List.iter (decl_template bufh typhash modules cnt) (List.sort compare components);
  List.iter (decl_template bufh typhash modules cnt) othlst;
  let body_lst = sub' typhash body_lst' in
  body := body_lst;
  body' := body_lst';
  List.iter (proc_template bufh typhash cnt) body_lst;
  bufm := Module12 (nam, ports' @ catbuf bufh) :: !(bufm);
  List.rev (!bufm);
  | PackageBody (pkg, body_lst') -> let cnt = ref 1 in
  let typhash = Hashtbl.create 255 in
  let body_lst = sub' typhash body_lst' in
  let dhash = (Hashtbl.create 255, Hashtbl.create 255) in
  let inst = newnam() in
  let bufh = bufhash () in
  List.iter (decl_template bufh typhash modules cnt) body_lst;
  List.rev (catbuf bufh);
  | oth -> unhand := Some oth; failwith "This template only handles modules/packages"
