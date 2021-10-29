open Source_text_rewrite_types
open Source_text_lex
open Source_text
open Source_text_simplify
open Printf

let dump_unhand = ref None
let dump_unhand_lst = ref []
let dumps = ref ""
let dbgsulst = ref []

let rec obin w n =
  (if w > 1 then obin (w-1) (n lsr 1) else "")^string_of_int (n land 1)

let vlst = ref []
let dbgfld = ref None
let dbgdot  = ref []
let dbgunion = ref []

let dot_extract typhash lft rght =
  match Hashtbl.find_opt typhash lft with
    | Some (Vsu _ | MaybePort(_, Vsu _, _)) ->
      let off,wid,typ = off_width_field typhash lft rght in
      dbgdot := (lft,(rght,off,wid,typ)) :: !dbgdot;
      IdArrayedColon (Id lft, Intgr (off+wid-1), Intgr off)
    | Some oth ->
      dbgfield := Some oth;
      dbgdot := (lft,(rght,0,0,oth)) :: !dbgdot;
      Id (lft^"."^rght)
    | None ->
      dbgdot := (lft,(rght,0,0,Unsigned)) :: !dbgdot;
      print_endline (lft^" not found in types dump_sysver.ml:30");
      Id (lft^"."^rght)

let dot_extract3 typhash lft field subfield =
  match Hashtbl.find_opt typhash lft with
    | Some (Vsu _ | MaybePort(_, Vsu _, _)) ->
      let off,wid,typ = off_width_field typhash lft field in
      let field_lst = match typ with
         | Vsu (Id id, field_lst) -> field_lst
         | oth -> dbgfld := Some oth; failwith "extract3" in
      let off',wid',typ' = search_field typhash off subfield field_lst in
      dbgdot := (lft,(field,off',wid',typ')) :: !dbgdot;
      IdArrayedColon (Id lft, Intgr (off'+wid'-1), Intgr off')
    | Some oth -> dbgfield := Some oth; Id (lft^"."^field^"."^subfield)
    | None -> failwith (lft^" not found in types dump_sysver.ml:43")

let rec vexpr'' (typhash:(string,vtyp)Hashtbl.t) = function
| Id s -> s
| Expression x -> " ( " ^ (vexpr typhash) x ^ " ) "
| Number (2,w,n,_) -> string_of_int w^"'b"^obin w n
| Number (10,w,n,s) -> string_of_int n
| Number (16,w,n,_) -> sprintf "%d'h%x" w n
| Number (b,w,n,s) -> s
| Intgr n -> string_of_int n
| Tilde expr -> " ~ " ^ (vexpr typhash) expr
| Pling expr -> " ! " ^ (vexpr typhash) expr
| Concat lst -> "{"^String.concat ", " (List.map (vexpr typhash) lst)^"}"
| Add ((Id s as lhs), (Intgr n as rhs)) -> (match Hashtbl.find_opt typhash s with
    | Some (Unsigned_vector(hi', lo')) -> let hi = ceval typhash hi' and lo = ceval typhash lo' in vexpr' typhash lhs ^ " + " ^ vexpr' typhash (Number(2, hi-lo+1, n, string_of_int n))
    | oth -> vexpr' typhash lhs ^ " + " ^ vexpr' typhash rhs)
| Add (lhs, rhs) -> vexpr' typhash lhs ^ " + " ^ vexpr' typhash rhs
| Sub ((Id s as lhs), (Intgr n as rhs)) -> (match Hashtbl.find_opt typhash s with
    | Some (Unsigned_vector(hi', lo')) -> let hi = ceval typhash hi' and lo = ceval typhash lo' in vexpr' typhash lhs ^ " - " ^ vexpr' typhash (Number(2, hi-lo+1, n, string_of_int n))
    | oth -> vexpr' typhash lhs ^ " - " ^ vexpr' typhash rhs)
| Mult (lhs, rhs) -> vexpr' typhash lhs ^ " * " ^ vexpr' typhash rhs
| StarStar (lhs, rhs) -> vexpr' typhash lhs ^ " ** " ^ vexpr' typhash rhs
| Div (lhs, rhs) -> vexpr' typhash lhs ^ " * " ^ vexpr' typhash rhs
| Sub (lhs, rhs) -> vexpr' typhash lhs ^ " - " ^ vexpr' typhash rhs
| LtEq (lhs, rhs) -> vexpr' typhash lhs ^ " <= " ^ vexpr' typhash rhs
| UMinus (rhs) -> " - " ^ vexpr' typhash rhs
| Equals (ExprQuote1 (typ, lhs), rhs) -> vexpr' typhash lhs ^ " == " ^ vexpr' typhash rhs
| Equals (lhs, rhs) -> vexpr' typhash lhs ^ " == " ^ vexpr' typhash rhs
| NotEq (ExprQuote1 (typ, lhs), rhs) -> vexpr' typhash lhs ^ " != " ^ vexpr' typhash rhs
| NotEq (lhs, rhs) -> vexpr' typhash lhs ^ " != " ^ vexpr' typhash rhs
| GtEq (lhs, rhs) -> vexpr' typhash lhs ^ " >= " ^ vexpr' typhash rhs
| Less (lhs, rhs) -> vexpr' typhash lhs ^ " < " ^ vexpr' typhash rhs
| Greater (lhs, rhs) -> vexpr' typhash lhs ^ " > " ^ vexpr' typhash rhs
| Or (lhs, rhs) -> (vexpr typhash) lhs ^ " | " ^ (vexpr typhash) rhs
| Or2 (lhs, rhs) -> (vexpr typhash) lhs ^ " || " ^ (vexpr typhash) rhs
| Xor (lhs, rhs) -> (vexpr typhash) lhs ^ " ^ " ^ (vexpr typhash) rhs
| And (lhs, rhs) -> (vexpr typhash) lhs ^ " & " ^ (vexpr typhash) rhs
| TildeAnd (rhs) -> " ~& " ^ (vexpr typhash) rhs
| And2 (lhs, rhs) -> (vexpr typhash) lhs ^ " && " ^ (vexpr typhash) rhs
| Unsigned expr -> "$unsigned("^vexpr typhash expr^")"
| Signed expr -> "$signed("^vexpr typhash expr^")"
| Shiftl (lhs, rhs) -> "("^vexpr typhash lhs^" << "^vexpr typhash rhs^")"
| Shiftr (lhs, rhs) -> "("^vexpr typhash lhs^" >> "^vexpr typhash rhs^")"
| Shiftr3 (lhs, rhs) -> "("^vexpr typhash lhs^" >>> "^vexpr typhash rhs^")"
| CellPinItem2 (port, expr) -> "." ^ port ^ " ( " ^ vexpr typhash expr ^ " ) "
| CellPinItemImplied (port) -> "." ^ port ^ " ( " ^ port ^ " ) "
| CellPinItemNC (port) -> "." ^ port ^ " ( ) "
| Query (Id cond', ctrue', cfalse') -> sprintf "%s ? %s : %s" cond' (vexpr typhash ctrue') (vexpr typhash cfalse')
| Query (cond', ctrue', cfalse') -> sprintf "%s ? %s : %s" (vexpr typhash cond') (vexpr typhash ctrue') (vexpr typhash cfalse')
| Deflt -> "()"
| Dot1(Id lft, Id rght) -> vexpr typhash (dot_extract typhash lft rght)
| Dot1 (Id lft, IdArrayed2 (Id rght, IdArrayedColon (Intgr ix, Intgr hi, Intgr 0))) when hi > clog2 (ix) ->
   vexpr typhash (match dot_extract typhash lft rght with
     | IdArrayed2 (Id lft', Intgr ix') ->
       IdArrayed2 (Id lft', Intgr (ix'+ix))
     | oth -> oth)
| Dot1 (Id lft, IdArrayed2 (Id rght, Intgr ix)) ->
   vexpr typhash (match dot_extract typhash lft rght with
     | IdArrayed2 (Id lft', Intgr ix') ->
       IdArrayed2 (Id lft', Intgr (ix'+ix))
     | oth -> oth)
| Dot1 (Id lft, IdArrayedColon (Id rght, Intgr hi, Intgr lo)) ->
   vexpr typhash (match dot_extract typhash lft rght with
     | IdArrayedColon (Id lft', Intgr hi', Intgr lo') ->
       IdArrayedColon (Id lft', Intgr (lo'+hi), Intgr (lo'+lo))
     | oth -> oth)
| Dot1 (Dot1 (Id lft, Id field), Id subfield) -> vexpr typhash (dot_extract3 typhash lft field subfield)
| Dot1 (IdArrayed2 (Id arr, ix), IdArrayedColon (Id slice, hi, lo)) -> "placeholder: " ^ arr ^ slice
| Dot1 (Dot1 (IdArrayed2 (Id arr, ix), Id field), Id subfield) -> "placeholder: " ^ arr ^ "[" ^ vexpr typhash ix ^ "]" ^ field ^ "." ^ subfield
| Dot1 (IdArrayed2 (Id arr, ix), Id field) -> "placeholder: " ^ arr ^ "[" ^ vexpr typhash ix ^ "]" ^ field
| Dot1 (Dot1 (IdArrayed2 (Id arr, ix), Id field), (IdArrayedColon _ as x)) -> "placeholder: " ^ arr ^ "[" ^ vexpr typhash ix ^ "]" ^ field ^ "." ^ vexpr typhash x
| Dot1 (IdArrayed2 (Id arr, ix), IdArrayed2 (Id field, ix')) -> "placeholder: " ^ arr ^ "[" ^ vexpr typhash ix ^ "] . " ^ field ^ "." ^ vexpr typhash ix'
| Dot1 (Dot1 (Id id, Id field), IdArrayedColon (Id subfield, hi, lo)) -> "placeholder: " ^ id ^ "." ^ field ^ "." ^ subfield ^ "[" ^ vexpr typhash hi ^ ":" ^ vexpr typhash lo ^ "]"
| Dot1 (Id lft, IdArrayed2 (Id rght, IdArrayedColon (Id arr, hi, lo))) -> "placeholder: " ^ lft ^ "." ^ rght ^ "[" ^ arr ^ "[" ^ vexpr typhash hi ^ ":" ^ vexpr typhash lo ^ "] ]"
| Dot1 (Id lft, IdArrayed2 (Id rght, Id id)) -> "placeholder: " ^ lft ^ "." ^ rght ^ "[" ^ id ^ "]"
| RedOr (lhs) -> " | (" ^ vexpr typhash lhs ^ ")"
| RedAnd (lhs) -> " & (" ^ vexpr typhash lhs ^ ")"
| RedXor (lhs) -> " ^ (" ^ vexpr typhash lhs ^ ")"
| TildeOr (lhs) -> " ~| (" ^ vexpr typhash lhs ^ ")"
| IdArrayed2 (InitPat lst, Intgr n) -> vexpr typhash (if n < List.length lst then List.nth lst n else Intgr 0)
| IdArrayed2 (InitPat _, _) as x ->
  dump_unhand := Some x;
  failwith "InitPat'";
| IdArrayed2 (IdArrayed2 (Id mem, ix), ix') ->
  (match Hashtbl.find_opt typhash mem with
      | Some (Unsigned_array [hi,lo; hi',lo']) -> vexpr typhash (IdArrayed2 (Id mem, Add (Sub (hi', lo'), Mult (Sub (ix, lo), Add (Sub (hi', lo'), Intgr 1)))))
      | Some (Unsigned_vector (lft, rght)) -> vexpr typhash (IdArrayed2 (Id mem, ix)) (* placeholder *)
      | Some (Unsigned_array dims) -> vexpr typhash (IdArrayed2 (Id mem, ix)) (* placeholder *)
      | Some (MaybePort (_, Unsigned_array dims, _)) -> vexpr typhash (IdArrayed2 (Id mem, ix)) (* placeholder *)
      | oth -> coth := oth; failwith "dims")
| IdArrayed2 (id, ix) -> vexpr typhash id^"["^vexpr typhash ix^"]"
| IdArrayed3 (PackageBody (pkg, []) :: [], arr) -> pkg^"::"^vexpr typhash arr
| IdArrayedColon (Number(_,w,n,_), expr, expr') ->
  let hi = ceval typhash expr and lo = ceval typhash expr' in
  let mask = 1 lsl (hi-lo) in
  sprintf "%d'h%x" (hi - lo + 1) ((land) (n lsr lo) (mask-1))
| IdArrayedColon (id, expr, expr') -> vexpr typhash id^"["^vexpr typhash expr^" : "^vexpr typhash expr'^"]"
| IdArrayedPlusColon (id, expr, expr') ->
  let lo = ceval typhash expr and wid = ceval typhash expr' in
  vexpr typhash id^"["^string_of_int (lo+wid-1)^" : "^string_of_int lo^"]"
| IdArrayedHyphenColon (id, expr, expr') -> vexpr typhash id^"["^vexpr typhash expr^" : "^vexpr typhash expr'^"]"
| FunRef (fn, arglst) -> fn^"("^String.concat ", " (List.map (vexpr typhash) arglst)^")"
| FunRef2 (fn, _, arglst) -> fn^"("^String.concat ", " (List.map (vexpr typhash) arglst)^")"
| AsgnPat [PatMemberDflt (Number _ as expr)] -> "{"^(vexpr typhash expr)^"}"
| AsgnPat lst -> String.concat "; " (List.map (vexpr typhash) lst)
| Repl (Intgr n, [expr]) -> if n >= 0 then " {" ^ String.concat ", " (List.init n (fun _ -> vexpr typhash expr)) ^ "} " else "{ ... }"
| Repl (expr', [expr]) -> "{{"^(vexpr typhash expr')^"}{"^(vexpr typhash expr)^"}}"
| InsideRange (first, last) -> "inside_range("^(vexpr typhash first)^", "^(vexpr typhash last)^")"
| OpenRange lst -> "open_range("^String.concat ", " (List.map (vexpr typhash) lst)^")"
| ExprOKL lst -> " {" ^ String.concat ", " (List.map (function
      | ExprQuote1(typ, expr) -> vexpr typhash expr
      | oth -> vexpr typhash oth) lst) ^ "} "
| PackageBody (pkg, [Id id]) -> pkg^"::"^id
(*
| ExprQuote1(lhs, rhs) -> vexpr typhash lhs^"'("^vexpr typhash rhs^")"
*)
| Sys ("$unsigned", expr) -> vexpr typhash (Unsigned expr)
| Sys ("$signed", expr) -> vexpr typhash (Signed expr)
| Sys ("$clog2", StarStar (Intgr n, Intgr m)) -> vexpr typhash (Intgr (clog2 (n lsl m)))
| Sys ("$clog2", expr) -> "$clog2 (" ^ vexpr typhash expr ^ ")"
| Sys ("$high", expr) -> "$high (" ^ vexpr typhash expr ^ ")"
| Sys ("$low", expr) -> "$low (" ^ vexpr typhash expr ^ ")"
| Sys ("$bits", expr) -> string_of_int (typsiz' typhash expr)
| Sys (sys_id, arglst) -> let args = (match arglst with
        | Itmlst lst -> String.concat ", " (List.map (vexpr typhash) lst)
	| oth -> vexpr typhash oth) in
    String.sub sys_id 1 (String.length sys_id - 1) ^ "(" ^ args ^")"
| Typ3 (id_t, [PackageBody (pkg, [])]) -> pkg^"::"^id_t
| PackageBody (pkg, []) -> pkg^"::"
| Atom ".*" -> "" (* placeholder *)
| Atom kind -> kind (* placeholder *)
| Typ1 id_t -> id_t
| PatMember1 (Id id, expr) -> id ^ " == " ^ vexpr typhash expr
| PatMemberDflt expr -> vexpr typhash expr
| ValueRange (lft, rght) -> "["^vexpr typhash lft^" .. "^vexpr typhash rght^"]"
| String s -> s
| AtStar -> "*"
| PackageRef (pkg_id, id) -> pkg_id ^ "::" ^ id
| Dot1 (FunRef2 _, _) -> "FunRef2.x" (* placeholder *)
| Itmlst _ -> "Itmlst ..." (* placeholder *)
| Dot1 (Id value, IdArrayed2 (Id mantissa, Sub (FunRef2 _, _))) -> "Dot1 ..." (* placeholder *)
| Dot1 (IdArrayed2 (IdArrayed2 (Id arr, Id ix), ix'), Id field) -> "Dot1 ..." (* placeholder *)
| Dot1 (IdArrayed2 (IdArrayed2 (Id arr, Id ix), ix'), IdArrayed2 (Id arr', ix'')) -> "Dot1 ..." (* placeholder *)
| Dot1 (Id id, IdArrayedColon (Id arr, hi, lo)) -> "Dot1 ..." (* placeholder *)
| Dot1 (Id id, IdArrayedPlusColon (Id arr, strt, wid)) -> "Dot1 ..." (* placeholder *)
| Dot1 (Id id, IdArrayed2 (Id arr, ix)) -> "Dot1 ..." (* placeholder *)
| DeclAsgn (Id id, (AnyRange _ :: _ as dims)) -> "DeclAsgn ("^id^", ...)"
| InitPat _ -> failwith "InitPat";
| oth -> dump_unhand := Some oth; failwith "vexpr dump_sysver.ml:180"

and vexpr' typhash = function
| (Intgr _ | Number _) as x -> "($unsigned(" ^ (vexpr typhash) x ^ ")) "
| (Id s) as x -> (match Hashtbl.find_opt typhash s with
    | Some (Vint _|Vtyp _|Venum _|Vsigtyp|Vpkg _|Vfun _|Vintf _|Vstr _|Vdot) -> vexpr typhash x
    | Some Unsigned_vector _ -> "$unsigned(" ^ vexpr typhash x ^ ") "
    | Some oth -> vexpr typhash x
    | None -> print_endline ("not found: "^s); s)
| x -> "$unsigned(" ^ vexpr typhash x ^ ") "

and cexpr' typhash = function
    | Id s -> s
    | Intgr n -> string_of_int n
    | Number (_,_,n,_) -> string_of_int n
    | Add (lhs, rhs) -> cexpr' typhash lhs ^ "+" ^ cexpr' typhash rhs
    | Sub (lhs, rhs) -> cexpr' typhash lhs ^ "-" ^ cexpr' typhash rhs
    | Mult (lhs, rhs) -> cexpr' typhash lhs ^ "*" ^ cexpr' typhash rhs
    | Div (lhs, rhs) -> cexpr' typhash lhs ^ "/" ^ cexpr' typhash rhs
    | Or (lhs, rhs) -> cexpr' typhash lhs ^ " | " ^ cexpr' typhash rhs
    | Or2 (lhs, rhs) -> cexpr' typhash lhs ^ " || " ^ cexpr' typhash rhs
    | Equals (lhs, rhs) -> cexpr' typhash lhs ^ " == " ^ cexpr' typhash rhs
    | Query (lhs, rhs, rhs') -> cexpr' typhash lhs ^ " ? " ^ cexpr' typhash rhs ^ " : " ^ cexpr' typhash rhs
    | Greater (lhs, rhs) -> cexpr' typhash lhs ^ " > " ^ cexpr' typhash rhs
    | LtEq (lhs, rhs) -> cexpr' typhash lhs ^ " <= " ^ cexpr' typhash rhs
    | Less (lhs, rhs) -> cexpr' typhash lhs ^ " < " ^ cexpr' typhash rhs
    | Shiftl (lhs, rhs) -> cexpr' typhash lhs ^ " << " ^ cexpr' typhash rhs
    | Shiftr (lhs, rhs) -> cexpr' typhash lhs ^ " >> " ^ cexpr' typhash rhs
    | Tilde rhs -> "~"  ^ cexpr' typhash rhs
    | Sys ("$clog2", _) as expr -> string_of_int (ceval typhash expr)
    | Sys ("$bits", _) -> "1" (* placeholder *)
    | Sys ("$size", _) -> "1" (* placeholder *)
    | Sys ("$random", _) -> "1" (* placeholder *)
    | Dot1 (port, conn) -> (cexpr' typhash port) ^ " => " ^ (cexpr' typhash conn)
    | Expression x -> cexpr' typhash x
    | StarStar (lhs, rhs) -> cexpr' typhash lhs ^ "+" ^ cexpr' typhash rhs
    | PackageBody (pkg, [Id id]) -> pkg^"::"^id
    | Concat lst -> "{" ^ String.concat ", " (List.map (cexpr' typhash) lst) ^ "}"
    | ExprOKL lst -> "{" ^ String.concat ", " (List.map (cexpr' typhash) lst) ^ "}"
    | Repl (expr, expr' :: []) -> "{{"^cexpr' typhash expr^"} {"^cexpr' typhash expr^"}}"
    | FunRef2 (fid, [PackageBody (pkg, [])], arglst) -> pkg^"::"^fid^"("^String.concat ", " (List.map (cexpr' typhash) arglst)^")"
    | ExprQuote1 (Atom typ, arg) -> "("^typ^")"^cexpr' typhash arg
    | oth -> dump_unhand := Some oth; failwith "cexpr"

and funtyp typhash = function
| Atom primtyp -> primtyp
| Typ1 id_t -> id_t
| Typ3 (id_t, [PackageBody (pkg, [])]) -> pkg^"::"^id_t
| Typ5 (Atom primtyp, AnyRange (lft, rght) :: []) -> sprintf "%s [%s:%s]" primtyp (vexpr typhash lft) (vexpr typhash rght)
| Typ6 (Atom primtyp) -> primtyp
| Typ8 (Atom kind, Atom kind') -> kind' ^ kind
| Typ8 (Atom kind, Deflt) -> kind
| oth -> dump_unhand := Some oth; failwith "funtyp"

and simplify = function
| Add (Intgr lhs, Intgr rhs) -> Intgr (lhs + rhs)
| Add (lhs, Intgr 0) -> lhs
| Add (Intgr 0, rhs) -> rhs
| Sub (Intgr lhs, Intgr rhs) -> Intgr (lhs - rhs)
| Sub (lhs, Intgr 0) -> lhs
| And (Intgr lhs, Intgr rhs) -> Intgr (lhs land rhs)
| Or (Intgr lhs, Intgr rhs) -> Intgr (lhs lor rhs)
| Xor (Intgr lhs, Intgr rhs) -> Intgr (lhs lxor rhs)
| Shiftl (Intgr lhs, Intgr rhs) -> Intgr (lhs lsl rhs)
| Add (lhs, ExprQuote1 (typ, rhs)) -> Add (simplify lhs, simplify rhs)
| Add (lhs, rhs) -> Add (simplify lhs, simplify rhs)
| Sub (lhs, rhs) -> Sub (simplify lhs, simplify rhs)
| And (lhs, rhs) -> And (simplify lhs, simplify rhs)
| Or (lhs, rhs) -> Or (simplify lhs, simplify rhs)
| Xor (lhs, rhs) -> Xor (simplify lhs, simplify rhs)
| Shiftl (lhs, rhs) -> Shiftl (simplify lhs, simplify rhs)
| Query (cond, lft, rght) -> Query(simplify cond, simplify lft, simplify rght)
| Expression (Intgr _ as x) -> x
| Expression (Shiftl (lhs, rhs)) -> Expression (simplify (Shiftl (simplify lhs, simplify rhs)))
| oth -> oth

and simplify'' x = 
  let rslt1 = ref (simplify (simplify (simplify (simplify (match x with Expression x -> x | _ -> x))))) in
  let rslt2 = ref (simplify (simplify (simplify (simplify !rslt1)))) in
  while !rslt1 <> !rslt2 do
    rslt1 := simplify (simplify (simplify (simplify !rslt2)));
    rslt2 := simplify (simplify (simplify (simplify !rslt1)));
  done;
  !rslt2

and vexpr typhash x =
  let x' = match simplify'' x with
    | ExprQuote1 (typ, x) -> x
    | oth -> oth in
  vlst := (x,x') :: !vlst;  
  let s = vexpr'' typhash x' in
  s

let sel_expr typhash x = match simplify x with Intgr _ -> "0" | oth -> vexpr typhash oth

let asgn fd typhash expr = function
| Id lhs ->
  fprintf fd "assign %s = %s; // 384\n" lhs (vexpr typhash expr)
| Concat _ as lst -> fprintf fd "assign %s = %s; // 385\n" (vexpr typhash lst) (vexpr typhash expr)
| Dot1 (lft, rght) as x -> fprintf fd "assign %s = %s; // 386\n" (vexpr typhash x) (vexpr typhash expr)
| IdArrayed2 (Id id, ix) -> 
  fprintf fd "assign %s[%s] = %s; // 387\n" id (vexpr typhash ix) (vexpr typhash expr)
| IdArrayedColon (Id id, hi, lo) -> fprintf fd "assign %s[%s : %s] = %s; // 388\n" id (vexpr typhash hi) (vexpr typhash lo) (vexpr typhash expr)
| IdArrayed2 (IdArrayed2 (Id arr, ix), ix') -> 
  fprintf fd "assign %s[%s][%s] = %s; // 387\n" arr (vexpr typhash ix) (vexpr typhash ix') (vexpr typhash expr)
| IdArrayedPlusColon (Id arr, base, wid) ->
  fprintf fd "assign %s[%s :+ %s] = %s; // 387\n" arr (vexpr typhash base) (vexpr typhash wid) (vexpr typhash expr)
| IdArrayedColon (IdArrayed2 (Id arr, ix), base, wid) ->
  fprintf fd "assign %s[%s][%s :+ %s] = %s; // 387\n" arr (vexpr typhash ix) (vexpr typhash base) (vexpr typhash wid) (vexpr typhash expr)
| oth -> dump_unhand := Some oth; failwith "asgn"

let rec parm_generic typhash = function
  | CellParamItem2 (nam, Typ1 s) ->
      sprintf "%24s         : type := %s" nam s
  | CellParamItem2 (nam, Typ3(id_t, PackageBody (pkg,[]) :: [])) ->
      sprintf "%24s         : type := %s" nam id_t
  | CellParamItem2 (nam, Typ5(Atom "logic", AnyRange(lft,rght) :: [])) ->
      sprintf "%24s         : type := logic [%s : %s]" nam (vexpr typhash lft) (vexpr typhash rght)
  | CellParamItem2 (nam, PackageBody (s, Id id :: _)) ->
      sprintf "%24s         : type := %s" nam s
  | CellParamItem2 (nam, Dot1 (Id lft, Id rght)) ->
      sprintf "%24s         : type := %s.%s" nam lft rght
  | CellParamItem2 (nam, Number (_, _, n, _)) ->
      sprintf "%24s         : integer := %d" nam n
  | CellParamItem2 (nam, ((Add _|Sub _|Mult _|Div _|Sys _) as x)) ->
      let n = ceval typhash x in
      sprintf "%24s         : integer := %d" nam n
  | CellParamItem3 (nam, Typ1(id_t)) ->
      sprintf "%24s         : type := %s" nam id_t
  | CellParamItem3 (nam, Typ3(id_t, PackageBody (pkg, []) :: [])) ->
      sprintf "%24s         : type := %s" nam id_t
  | Param (nam, Intgr n, []) ->
      sprintf "%24s         : integer := %d" nam n
  | Param (nam, Number (_, _, n, _), []) ->
      sprintf "%24s         : integer := %d" nam n
  | PackageParam2 (id_t, nam, [], Id s) ->
      sprintf "%24s         => %s" id_t s
  | PackageParam2 (grp_e, nam, [PackageBody (pkg, [])], PackageBody (pkg', [Id s])) ->
      sprintf "%24s         => %s" grp_e s
  | PackageParam2 (id_t, nam, [PackageBody (pkg, [])], Number (_, _, n, _)) ->
      sprintf "%24s         => %d" nam n
  | PackageParam2 (id_t, nam, [PackageBody (pkg, [])], AsgnPat [PatMemberDflt (Number (_, _, n, ""))]) ->
      sprintf "%24s         => %d" nam n
  | PackageParam2 (id_t, nam, [PackageBody (pkg, [])], AsgnPat [PatMemberDflt (PackageBody (pkg', [Id id]))]) ->
      sprintf "%24s         => %s" nam id
  | PackageParam2 (id_t, nam, [PackageBody (pkg, [])], ExprQuote1 (Typ3(id, PackageBody (pkg', []) :: []), expr)) ->
      sprintf "%24s         => %s" nam id
  | TypParam (nam, Atom typ, []) ->
      sprintf "%24s         => %s" nam typ
  | TypParam (nam, Id id_t, PackageBody (pkg, []) :: []) ->
      sprintf "%24s         => %s" nam id_t
  | TypParam (nam, Atom typ, AnyRange (lft, rght) :: []) ->
      sprintf "%24s         => %s[%s : %s]" nam typ (vexpr typhash lft) (vexpr typhash rght)
  | Param (nam, PackageBody (pkg, [Id id]), []) ->
      sprintf "%24s         => %s" nam id
  | Param (nam, FunRef2 (fn, [PackageBody (pkg, [])], expr :: []), []) ->
      sprintf "%24s         => %s" nam (vexpr typhash expr)
  | Param (nam, String s, []) ->
      sprintf "%24s         => %s" nam s
  | Param (nam, Dot1(lft,rght), []) ->
      sprintf "%24s         => %s.%s" nam (vexpr typhash lft) (vexpr typhash rght)
  | Param (nam, Number (_, _, n, _), AnyRange (left, rght) :: []) ->
      sprintf "%24s         => %d" nam n
  | Param (nam, UMinus (Number (_, _, n, _)), []) ->
      sprintf "%24s         => %d" nam (-n)
  | Param (nam, UMinus (Number (_, _, n, _)), AnyRange (lft, rght) :: []) ->
      sprintf "%24s         => %d" nam (-n)
  | Param (nam, ((Add _|Sub _|Mult _|Div _|StarStar _ |Sys _|Equals _|Query _) as x), []) ->
      let n = ceval typhash x in
      sprintf "%24s         => %d" nam n
 | PackageParam (lst, inner) -> String.concat ", " (List.map (function PkgImport (Itmlst [PkgImportItm (pkg, Atom "*")]) -> parm_generic typhash inner | _ -> "") lst)
  | oth -> dump_unhand := Some oth; failwith "parm_generic"

let rec parm_map typhash = function
  | CellParamItem2 (nam, Intgr n) ->
      sprintf "%24s         => %d" nam n
  | CellParamItem2 (nam, Number (_, _, n, _)) ->
      sprintf "%24s         => %d" nam n
   | CellParamItem2 (nam, Typ1 s) ->
      sprintf "%24s         => %s" nam s
  | CellParamItem2 (nam, Typ3(id_t, PackageBody (pkg,[]) :: [])) ->
      sprintf "%24s         : type := %s" nam id_t
  | CellParamItem2 (nam, Typ5(Atom "logic", AnyRange(lft,rght) :: [])) ->
      sprintf "%24s         => logic[%s : %s]" nam (vexpr typhash lft) (vexpr typhash rght)
  | CellParamItem2 (nam, PackageBody (s, Id id :: _)) ->
      sprintf "%24s         => %s" nam s
  | CellParamItem2 (nam, Dot1 (Id lft, Id rght)) ->
      sprintf "%24s         => %s.%s" nam lft rght
  | CellParamItem2 (nam, ((Add _|Sub _|Mult _|Div _|Sys _) as x)) ->
      let n = ceval typhash x in
      sprintf "%24s         => %d" nam n
  | CellParamItem3 (nam, Typ1(id_t)) ->
      sprintf "%24s         => %s" nam id_t
  | CellParamItem3 (nam, Typ3(id_t, PackageBody (pkg, []) :: [])) ->
      sprintf "%24s         => %s" nam id_t
  | PackageParam (lst, inner) -> String.concat ", " (List.map (function PkgImport (Itmlst [PkgImportItm (pkg, Atom "*")]) -> parm_map typhash inner | _ -> "") lst)
  | PackageParam2 (grp_e, nam, [PackageBody (pkg, [])], PackageBody (pkg', [Id s])) ->
      sprintf "%24s         => %s" grp_e s
  | PackageParam2 (id_t, nam, [PackageBody (pkg, [])], Number (_, _, n, _)) ->
      sprintf "%24s         => %d" nam n
  | PackageParam2 (id_t, nam, [PackageBody (pkg, [])], AsgnPat [PatMemberDflt (Number (_, _, n, ""))]) ->
      sprintf "%24s         => %d" nam n
  | PackageParam2 (id_t, nam, [PackageBody (pkg, [])], AsgnPat [PatMemberDflt (PackageBody (pkg', [Id id]))]) ->
      sprintf "%24s         => %s" nam id
  | PackageParam2 (id_t, nam, [PackageBody (pkg, [])], ExprQuote1 (Typ3(id, PackageBody (pkg', []) :: []), expr)) ->
      sprintf "%24s         => %s" nam id
  | TypParam (nam, Atom typ, []) ->
      sprintf "%24s         => %s" nam typ
  | TypParam (nam, Id id_t, PackageBody (pkg, []) :: []) ->
      sprintf "%24s         => %s" nam id_t
  | Param (nam, Number (_, _, n, _), []) ->
      sprintf "%24s         => %d" nam n
  | Param (nam, String s, []) ->
      sprintf "%24s         => %s" nam s
  | Param (nam, Dot1(lft,rght), []) ->
      sprintf "%24s         => %s.%s" nam (vexpr typhash lft) (vexpr typhash rght)
  | Param (nam, PackageBody (pkg, [Id id]), []) ->
      sprintf "%24s         => %s" nam id
  | Param (nam, FunRef2 (fn, [PackageBody (pkg, [])], [Id id]), []) ->
      sprintf "%24s         => %s" nam id
  | Param (nam, Number (_, _, n, _), AnyRange (left, rght) :: []) ->
      sprintf "%24s         => %d" nam n
  | Param (nam, UMinus (Number (_, _, n, _)), []) ->
      sprintf "%24s         => %d" nam (-n)
  | Param (nam, UMinus (Number (_, _, n, _)), AnyRange (lft, rght) :: []) ->
      sprintf "%24s         => %d" nam (-n)
  | Param (nam, ((Add _|Sub _|Mult _|Div _|StarStar _ |Sys _) as x), []) ->
      let n = ceval typhash x in
      sprintf "%24s         => %d" nam n
  | oth -> dump_unhand := Some oth; failwith "parm_map"

let parm_dump_template fd typhash parm_lst = 
  if parm_lst <> [] then
  fprintf fd "    generic (\n%s\n    ); // 588\n" (String.concat ";\n" (List.map (parm_generic typhash) parm_lst))

let decl_mem fd typhash first last hi lo cnt mem =
    fprintf fd "logic [%s : %s] %s [%s : %s]; // 591\n" (vexpr typhash last) (vexpr typhash first) mem (vexpr typhash hi) (vexpr typhash lo)

let dump_enum fd typhash front lft rght elst back =
        fprintf fd "%senum logic [%s:%s] {\n\t%s\n} %s // 326\n" front (vexpr typhash lft) (vexpr typhash rght) (String.concat ",\n\t" (List.map (function
        | Id e -> e
        | EnumInit (e, expr) ->
	    let s = vexpr typhash expr in
	    let s' = sprintf "%s = %s" e s in
	    s'
	| oth -> dump_unhand := Some oth; failwith "TypEnum6") elst)) back

let dump_vdir = function
  | In -> "input logic"
  | Out -> "output logic"
  | Inout -> "inout wire"
  | Deflt -> "inout wire"
  | oth -> dump_unhand := Some oth; failwith "dump_vdir"

let rec dump_unpack_typ typhash prefix sep stem = function
  | (nam, (Vsu (id, lst))) -> let stem' = stem^"."^nam in String.concat sep (List.map (dump_unpack_typ typhash prefix sep stem') lst)
  | (nam, ((Unsigned | Signed | Unsigned_vector _ | Signed_vector _ ) as typ')) ->
    prefix ^ " [" ^ string_of_int (csiz' typhash typ') ^ ":0] \\" ^ stem ^ "." ^ nam ^ " "
  | oth -> dbgsu := Some oth; failwith "unpack_typ"

let rec dump_struct_flat fd typhash memblst stem =
  let sep = ";\n" in
  let sulst = List.flatten (List.map (struct_union typhash) memblst) in
  dbgsulst := (stem,sulst) :: !dbgsulst;
  let cat = String.concat sep (List.map (dump_unpack_typ typhash "logic" sep stem) sulst) in
  fprintf fd "%s; // 758\n" cat

let rec dump_struct fd typhash = function
  | SUMember (Typ6 (Atom ("logic"|"bit"|"byte"|"int"|"longint" as kind)), lst) -> List.iter (function
      | Id id -> fprintf fd "logic [%d:0] %s; // 350\n" (atom_width kind - 1) id
      | oth -> dump_unhand := Some oth; failwith "SUMember5") lst
  | SUMember (Typ5 (Atom kind, AnyRange (lft, rght) :: []), lst) -> List.iter (function
      | Id id -> fprintf fd "%s [%s:%s] %s; // 353\n" kind  (vexpr typhash lft) (vexpr typhash rght) id;
      | oth -> dump_unhand := Some oth; failwith "SUMember2") lst
  | SUMember (Typ5 (Atom kind, AnyRange (lft, rght) :: AnyRange (lft', rght') :: []), lst) -> List.iter (function
      | Id id -> fprintf fd "%s [%s:%s] [%s:%s] %s; // 356\n" kind (vexpr typhash lft) (vexpr typhash rght) (vexpr typhash lft') (vexpr typhash rght') id;
      | oth -> dump_unhand := Some oth; failwith "SUMember2") lst
  | SUMember (Typ6 (Typ5 (Atom ("logic" as kind), [AnyRange (lft, rght)])), id_lst) -> List.iter (function
      | Id id -> fprintf fd "%s [%s:%s] %s; // 359\n" kind  (vexpr typhash lft) (vexpr typhash rght) id;
      | oth -> dump_unhand := Some oth; failwith "SUMember7") id_lst
  | SUMember (Typ5 (TypEnum3 [AnyRange (lft, rght)], eid_lst), id_lst) -> List.iter (function
      | Id id -> fprintf fd "logic [%s:%s] %s; // 362\n" (vexpr typhash lft) (vexpr typhash rght) id;
      | oth -> dump_unhand := Some oth; failwith "SUMember8") id_lst
  | SUMember (TypEnum6 (id_t, TypEnum3 [AnyRange (lft, rght)], eid_lst), id_lst) ->  List.iter (function
      | Id id -> fprintf fd "logic [%s:%s] %s; // 365\n" (vexpr typhash lft) (vexpr typhash rght) id;
      | oth -> dump_unhand := Some oth; failwith "SUMember9") id_lst
  | SUMember (Typ8 (SUDecl (Atom ("packed"|"signed" as signage), lst), Deflt), id_lst) -> List.iter (function
      | Id id -> List.iter (dump_struct fd typhash) lst
      | oth -> dump_unhand := Some oth; failwith "SUMember10") id_lst
  | SUMember (Typ8 (Atom (("byte") as kind), Deflt), id_lst) -> List.iter (function
      | Id id -> fprintf fd "%s %s; // 371\n" kind id;
      | oth -> dump_unhand := Some oth; failwith "SUMember11") id_lst
  | SUMember (Typ8 (Atom ("int"|"longint" as kind), Atom "unsigned"), id_lst) -> List.iter (function
      | Id id -> fprintf fd "%s %s; // 374\n" kind  id;
      | oth -> dump_unhand := Some oth; failwith "SUMember12") id_lst
  | SUMember (Typ8 (Atom ("int"|"longint" as kind), Deflt), id_lst) -> List.iter (function
      | Id id -> fprintf fd "%s %s; // 377\n" kind  id;
      | oth -> dump_unhand := Some oth; failwith "SUMember13") id_lst
  | SUMember (Typ9 (id_t, [AnyRange _ as dim1], Typ5 (Atom "logic", dims)), id_lst) -> List.iter (function
      | Id id -> fprintf fd "logic [%d:0] %s; // 430\n" (csiz' typhash (Unsigned_array (mapdims (dim1 :: dims))) - 1) id;
      | oth -> dump_unhand := Some oth; failwith "SUMember14") id_lst
  | SUMember (Typ9 (old_id, (AnyRange _ as dim1 :: []),
                    Typ9 (old_id', (AnyRange (lft', rght') :: []), 
                          Typ5 (TypEnum3 (AnyRange _ :: _ as dims), e_lst))), id_lst) -> List.iter (function
      | Id id -> fprintf fd "logic [%d:0] %s; // 430\n" (csiz' typhash (Unsigned_array (mapdims (dim1 :: dims))) - 1) id;
      | oth -> dump_unhand := Some oth; failwith "SUMember15") id_lst
  | SUMember (Typ6 (PackageRef (pkgid, idp)), id_lst) -> dump_struct fd typhash (SUMember (find_pkg pkgid idp, id_lst))
  | SUMember (Typ3 (old_id, [Typ5 (Atom "logic", [AnyRange (lft, rght)])]), id_lst) -> List.iter (function
      | Id id -> fprintf fd "logic [%s:%s] %s; // 362\n" (vexpr typhash lft) (vexpr typhash rght) id;
      | oth -> dump_unhand := Some oth; failwith "SUMember16") id_lst
  | SUMember (Typ3 (old_id, [Typ8 (SUDecl (Atom ("packed"|"signed" as signage), lst), Deflt)]), id_lst) -> List.iter (function
      | Id id -> List.iter (dump_struct fd typhash) lst
      | oth -> dump_unhand := Some oth; failwith "SUMember17") id_lst
  | SUMember (Typ9 (old_id_t, [TypEnum6 (id_t, TypEnum3 (AnyRange _ :: _ as dims), e_lst)], Typ5 (TypEnum3 (AnyRange _ :: _ as dims'), e_lst')), id_lst) -> List.iter (function
      | Id id -> fprintf fd "logic [%d:0] %s; // 430\n" (csiz' typhash (Unsigned_array (mapdims (dims @ dims'))) - 1) id;
      | oth -> dump_unhand := Some oth; failwith "SUMember15") id_lst
  | SUMember (Typ9 (old_id, [Typ5 (Atom "logic", [AnyRange (Intgr hi, Intgr lo)])], Typ8 (SUDecl (Atom ("packed"|"signed" as signage), lst), Deflt)), id_lst) -> List.iter (function
      | Id id -> fprintf fd "logic [%d:0] %s; // 430\n" (csiz' typhash (Vsua (hi, lo, List.flatten (List.map (struct_union typhash) lst)))) id;
      | oth -> dump_unhand := Some oth; failwith "SUMember16") id_lst
  | SUMember (Typ8 (Atom ("byte"|"logic" as kind), Atom ("signed"|"unsigned" as signage)), id_lst) -> List.iter (function
      | Id id -> fprintf fd "logic [%d:0] %s; // 430\n" (csiz' typhash (atom_cnv_signage (kind, signage))) id;
      | oth -> dump_unhand := Some oth; failwith "SUMember17") id_lst
  | SUMember (Typ12 ([Atom ("reg" as kind)], Atom ("signed"|"unsigned" as signage), AnyRange(hi,lo) :: []), id_lst) -> List.iter (function
      | Id id -> fprintf fd "logic [%d:0] %s; // 430\n" (csiz' typhash (atom_cnv_signage_array hi lo (kind, signage))) id;
      | oth -> dump_unhand := Some oth; failwith "SUMember18") id_lst
  | SUMember (Typ3 (id_t, (AnyRange _ :: _ as dims)), id_lst) ->
    let typ' = match Hashtbl.find_opt typhash id_t with
      | Some (Unsigned_array lst) -> (Unsigned_array (mapdims dims@lst))
      | None -> print_endline ("Missing: "^id_t); Unsigned_vector(Intgr 0, Intgr 0) in
    List.iter (function 
        | Id id -> fprintf fd "logic [%d:0] %s; // 430\n" (csiz' typhash typ') id;
	| oth -> dump_unhand := Some oth; failwith "SUMember_525") id_lst
  | SUMember (Typ9 (old_id, [AnyRange (lft,rght)], PackageRef (pkgid, new_id)), id_lst) -> List.iter (function
      | Id id -> fprintf fd "logic [%d:0] %s; // 527\n" (abs (ceval typhash rght - ceval typhash lft) + 1) id;
      | oth -> dump_unhand := Some oth; failwith "SUMember_528") id_lst
  | SUMember (Typ3 (old_id, [PackageRef (pkgid, idp)]), id_lst) ->
    let x = match find_pkg pkgid idp with
      | (Typ5 _ | Typ8 _ | TypEnum6 _ as x) -> x
      | oth -> dump_unhand := Some oth; failwith "SUMember_532" in
    dump_struct fd typhash (SUMember (x, id_lst))
  | SUMember (Typ6 (TypEnum6 (old_id, TypEnum3 [AnyRange (hi, lo)], e_lst)), id_lst) -> List.iter (function
      | Id id -> fprintf fd "logic [%s:%s] %s; // 535\n" (vexpr typhash hi) (vexpr typhash lo) id;
      | oth -> dump_unhand := Some oth; failwith "SUMember_536") id_lst
  | SUMember (Typ6 (Typ8 (SUDecl (Atom "packed", lst), Deflt)), id_lst) -> List.iter (function
      | Id id -> List.iter (dump_struct fd typhash) lst
      | oth -> dump_unhand := Some oth; failwith "SUMember_539") id_lst
  | SUMember (Atom "logic", id_lst) -> List.iter (function
      | Id id -> fprintf fd "logic %s; // 541\n" id
      | oth -> dump_unhand := Some oth; failwith "SUMember_542") id_lst
  | SUMember (Typ1 id_t, id_lst) -> List.iter (function
      | Id id -> fprintf fd "logic %s; // 544 %s FIXME\n" id id_t
      | oth -> dump_unhand := Some oth; failwith "SUMember_542") id_lst
  | oth -> dump_unhand := Some oth; failwith "dump_struct"

let dump_dep = function
| Pos (Id signal) -> sprintf "posedge %s" signal
| Neg (Id signal) -> sprintf "negedge %s" signal
| oth -> dump_unhand := Some oth; failwith "dump_dep"
 
let rec dump_deps fd kind = function
| [] -> ()
| (Pos _ | Neg _) :: _ as lst -> fprintf fd "%s @(%s)\n" kind (String.concat " or " (List.map dump_dep lst))
| oth -> dump_unhand_lst := oth; failwith "dump_deps"

let rec dump_deps_comb fd typhash kind lst =
fprintf fd "%s @(%s)\n" kind (String.concat " or " (List.map (vexpr typhash) lst))

let dbgeq = ref None
let dbgiff = ref None
let dbgdmpu = ref None
let dbgasgn = ref None
let dbgdmpsu = ref None

let rec stmt_clause fd typhash = function
      | Itmlst lst -> List.iter (stmt_clause fd typhash) lst
      | BeginBlock lst -> List.iter (stmt_clause fd typhash) lst
      | If2 (condition, if_lst, else_lst) ->
        fprintf fd "        if (%s) begin\n" (vexpr typhash condition);
        (match if_lst with BeginBlock if_lst -> List.iter (stmt_clause fd typhash) if_lst | _ -> stmt_clause fd typhash if_lst);
        fprintf fd "        end\nelse\nbegin";
        (match else_lst with BeginBlock else_lst -> List.iter (stmt_clause fd typhash) else_lst | _ -> stmt_clause fd typhash else_lst);
        fprintf fd "        end\n";
      | If1 (condition, if_lst) ->
        dbgiff := Some condition;
        fprintf fd "        if (%s) begin\n" (vexpr typhash condition);
        (match if_lst with BeginBlock if_lst -> List.iter (stmt_clause fd typhash) if_lst | _ -> stmt_clause fd typhash if_lst);
        fprintf fd "end\n";
      | DeclLogic2 (wire_lst, AnyRange (hi, lo) :: []) -> List.iter (function
	  | Id nam ->
	  fprintf fd "logic [%s : %s] %s; // 412\n" (vexpr typhash hi) (vexpr typhash lo) (nam)
	  | DeclAsgn (nam, AnyRange (hi, lo) :: []) ->
              fprintf fd "logic [%s : %s] %s ; // 450\n" (vexpr typhash hi) (vexpr typhash lo) (vexpr typhash nam)
	  | oth -> dump_unhand := Some oth; failwith "DeclLogic2") wire_lst
      | DeclLogic lst -> ()
      | Seq (id, []) -> fprintf fd "       begin end; // 775\n"
      | Seq (id, lst) ->  List.iter (stmt_clause fd typhash) lst
      | Blocking (Asgn1 (IdArrayed2(id, sel), expr)) -> fprintf fd "        %s[%s] = %s; // 777\n" (vexpr typhash id) (vexpr typhash sel) (vexpr typhash expr)
      | Blocking (Asgn1 (Dot1(Id lft, Id rght), expr)) -> fprintf fd "        %s.%s = %s; // 778\n" lft rght (vexpr typhash expr)
      | Blocking (Asgn1 (id, expr)) -> fprintf fd "        %s = %s; // 777\n" (vexpr typhash id) (vexpr typhash expr)
      | Blocking (FopAsgn (id, ExprQuote1 _)) -> fprintf fd "        %s = ExprQuote1; // 489\n" (vexpr typhash id)
      | Blocking (FopAsgn (id, expr)) -> fprintf fd "        %s = %s; // 779\n" (vexpr typhash id) (vexpr typhash expr)
      | Blocking (FopAsgn1 (id, id', id'', expr)) -> fprintf fd "        %s = %s; // 780\n" (vexpr typhash id) (vexpr typhash expr)
      | Blocking (FopAsgnArrayMemSel (id, hi, lo, expr)) -> fprintf fd "        %s[%s : %s] = %s; // 781\n" (vexpr typhash id) (vexpr typhash hi) (vexpr typhash lo) (vexpr typhash expr)
      | Blocking (FopAsgnConcat (idlst, expr)) -> fprintf fd "        %s = %s; // 782\n" (String.concat ", " (List.map (vexpr typhash) idlst)) (vexpr typhash expr)
      | Blocking (FopAsgnArraySel (id, ix, expr)) -> fprintf fd "        %s[%s] = %s; // 783\n" (vexpr typhash id) (vexpr typhash ix) (vexpr typhash expr)
      | Blocking (FopAsgnArrayWid (id, hi, lo, expr)) -> fprintf fd "        %s[%s : %s] = %s; // 469\n" (vexpr typhash id) (vexpr typhash hi) (vexpr typhash lo) (vexpr typhash expr)
      | Blocking (FopAsgnArrayRange (id, hi, lo, expr)) -> fprintf fd "        %s[%s : %s] = %s; // 785\n" (vexpr typhash id) (vexpr typhash hi) (vexpr typhash lo) (vexpr typhash expr)
      | Blocking (FopAsgnArrayRange2 (id, ix, ix', expr)) -> fprintf fd "        %s[%s][%s] = %s; // 786\n" (vexpr typhash id) (vexpr typhash ix) (vexpr typhash ix') (vexpr typhash expr)
      | Blocking (FopAsgnArrayField (id,  ix, expr)) -> fprintf fd "        %s = %s; // 787\n" (vexpr typhash (Dot1(id, ix))) (vexpr typhash expr)
      | Blocking (FopAsgnArrayField2 (id, IdArrayedColon(Id ix, hi, lo), expr)) ->
          fprintf fd "        %s.%s[%s : %s] = %s; // 548\n" (vexpr typhash id) ix (vexpr typhash hi) (vexpr typhash lo) (vexpr typhash expr)
      | Blocking (FopAsgnArrayField3 (id, sel, sel', expr)) ->
          fprintf fd "        %s[%s].%s = %s; // 550\n" (vexpr typhash id) (vexpr typhash sel) (vexpr typhash sel') (vexpr typhash expr)
      | Blocking (FopAsgnArrayField4 (id, sel, id', sel', sel'', expr)) ->
          fprintf fd "        %s[%s].%s[%s] = %s; // 552\n" (vexpr typhash id) (vexpr typhash sel) (vexpr typhash id') (vexpr typhash sel') (vexpr typhash expr)
      | Blocking (FopAsgnArrayField5 (id, sel, id', sel', expr)) ->
          fprintf fd "        %s[%s].%s[%s] = %s; // 554\n" (vexpr typhash id) (vexpr typhash sel) (vexpr typhash id') (vexpr typhash sel') (vexpr typhash expr)
      | Blocking (FopAsgnArrayField6 (id, sel, sel', id', expr)) ->
          fprintf fd "        %s[%s][%s].%s = %s; // 556\n" (vexpr typhash id) (vexpr typhash sel) (vexpr typhash sel') (vexpr typhash id') (vexpr typhash expr)
      | Blocking (FopAsgnArrayField7 (id, sel, sel', id', expr)) ->
          fprintf fd "        %s[%s][%s].%s = %s; // 558\n" (vexpr typhash id) (vexpr typhash sel) (vexpr typhash sel') (vexpr typhash id') (vexpr typhash expr)
      | Blocking (FopAsgnArrayField8 (id, sel, id', id'', expr)) ->
          fprintf fd "        %s[%s][%s].%s = %s; // 560\n" (vexpr typhash id) (vexpr typhash sel) (vexpr typhash id') (vexpr typhash id'') (vexpr typhash expr)
      | Blocking (FopAsgnArrayField9 (id, arr, id', id'', id3, expr)) ->
          fprintf fd "        %s.%s[%s][%s].%s = %s; // 562\n" (vexpr typhash id) (vexpr typhash arr) (vexpr typhash id') (vexpr typhash id'') (vexpr typhash id3) (vexpr typhash expr)
      | ForEach (ix, lst) ->
          fprintf fd "            foreach %s; // 576\n" (vexpr typhash ix);
          List.iter (stmt_clause fd typhash) lst;
      | ForLoop (Asgn1 (Id ix, strt) :: [], Less (Id ix', limit), SideEffect (Id xi'', Atom "++"), seq) ->
          fprintf fd "            for %s; // 579\n" ix;
          stmt_clause fd typhash seq;
      | ForLoop (Asgn1 (Id ix, strt) :: [], LtEq (Id ix', limit), Asgn1 (Id ix'', Add (Id ix''', Number (_, _, 1, _))), seq) ->
          fprintf fd "            for %s; // 582\n" ix;
          stmt_clause fd typhash seq;
      | ForLoop ([Typ7 (ix, Atom ("int"|"unsigned_int"))], Less (Id ix', limit), Asgn1 (Id ix'', Add (Id ix''', Number (_, _, inc, _))), seq) ->
          fprintf fd "            for %s; // 585\n" ix;
          stmt_clause fd typhash seq;
      | ForLoop ([Typ7 (ix, Atom ("int"|"unsigned_int"))], Less (Id ix', limit), SideEffect (Id xi'', Atom "++"), seq) ->
          fprintf fd "            for %s; // 588\n" ix;
          stmt_clause fd typhash seq;
      | ForLoop ([Typ7 (ix, Atom ("int"|"unsigned_int"))], GtEq (Id ix', limit), SideEffect (Id xi'', Atom "--"), seq) ->
          fprintf fd "            for %s; // 591\n" ix;
          stmt_clause fd typhash seq;
      | ForLoop ([Typ9 (ix, AnyRange(hi,lo) :: [], Atom ("logic"))], Less (Id ix', limit), SideEffect (Id xi'', Atom "++"), seq) ->
          fprintf fd "            for %s; // 594\n" ix;
          stmt_clause fd typhash seq;
      | ForLoop ([Typ9 (ix, [Atom "unsigned"; Atom "int"], strt)], Less (Id ix', stop), SideEffect (Id ix'', Atom "++"), seq) ->
          fprintf fd "            for %s; // 597\n" ix;
          stmt_clause fd typhash seq;
      | ForLoop ([Typ9 (ix, [Atom "int"], strt)], Less (Id ix', stop), Asgn1 (Id ix'', Add (Id ix3, inc)), seq) ->
          fprintf fd "            for %s; // 601\n" ix;
          stmt_clause fd typhash seq;
      | ForLoop ([Typ9 (ix, [Atom "int"], strt)], Less (Id ix', stop), SideEffect (Id ix'', Atom "++"), seq) ->
          fprintf fd "            for %s; // 604\n" ix;
          stmt_clause fd typhash seq;
      | Equate (id,AsgnPat exprlst) ->
        dbgeq := Some (id, exprlst);
      | Equate (id,expr) ->
        fprintf fd "            %s <= %s; // 586\n" (vexpr typhash id) (vexpr typhash expr);
      | EquateSlice (id,hi,lo,expr) -> fprintf fd "            %s[%s : %s] <= %s; // 587\n" (vexpr typhash id) (vexpr typhash hi) (vexpr typhash lo) (vexpr typhash expr);
      | EquateSelect (id,ix,expr) -> fprintf fd "            %s[%s] <= %s; // 588\n" (vexpr typhash id) (vexpr typhash ix) (vexpr typhash expr);
      | EquateSelect2 (id,ix,expr) -> fprintf fd "            %s[%s] <= %s; // 589\n" (vexpr typhash id) (vexpr typhash ix) (vexpr typhash expr);
      | EquateArrayField (id,id',ix,ix',expr) -> fprintf fd "            %s.%s[%s][%s] <= %s; // 590\n" (vexpr typhash id) (vexpr typhash id') (vexpr typhash ix) (vexpr typhash ix') (vexpr typhash expr);
      | CaseStart (CaseStart1 (sel), lst) ->
        fprintf fd "case (%s)\n" (vexpr typhash sel);
        List.iter (case_clause fd typhash) lst;
        fprintf fd "endcase; // 825\n";
      | CaseStartInside (sel, lst) ->
        fprintf fd "case (%s) inside\n" (vexpr typhash sel);
        List.iter (case_clause fd typhash) lst;
        fprintf fd "endcase; // 829\n";
      | CaseStartUniq (CaseStart1 (sel), lst) ->
        fprintf fd "unique case (%s)\n" (vexpr typhash sel);
        List.iter (case_clause fd typhash) lst;
        fprintf fd "endcase; // 833\n";
      | CaseStartUniqInside (sel, lst) ->
        fprintf fd "unique case (%s) inside\n" (vexpr typhash sel);
        List.iter (case_clause fd typhash) lst;
        fprintf fd "endcase; // 841\n";
      | Blocking (SideEffect (id, Atom "++")) -> fprintf fd "            %s <= %s+1; // 842\n" (vexpr typhash id) (vexpr typhash id)
      | Blocking (SideEffect (id, Atom "--")) -> fprintf fd "            %s <= %s-1; // 843\n" (vexpr typhash id) (vexpr typhash id)
      | DeclData _ -> ()
      | Blocking (BreakSemi) -> () (* placeholder *)
      | BreakSemi -> () (* placeholder *)
      | Assert -> ()
      | Atom ";" -> ()
      | TaskRef (tid, lst) -> () (* placeholder *)
      | TaskRef2 (tid, (IdArrayedColon (Id id, hi, lo) as expr)) -> fprintf fd "// %s %s // 666\n" tid (vexpr typhash expr)
      | TFBody (decls, lst) -> List.iter (stmt_clause fd typhash) lst
      | SysTaskCall (tid, args) -> fprintf fd "%s(...);\n" tid
      | EquateField (id, field, expr) ->  fprintf fd "            %s.%s <= %s; // 851\n" (vexpr typhash id) (vexpr typhash field) (vexpr typhash expr)
      | DeclInt2 _ -> ()
      | DeclLogic2 _ -> fprintf fd "// 491\n"
      | Return expr -> fprintf fd "return %s;\n" (vexpr typhash expr)
      | CaseStmt _ as x -> case_clause fd typhash x
      | SysTaskRef (Atom ("$fwrite" as task), Id f :: tl) -> fprintf fd "%s(%s, ...)" task f
      | SysTaskRef (Atom ("$warning" as task), String s :: []) -> fprintf fd "%s(\"%s\")" task s

      | oth -> dump_unhand := Some oth; failwith "stmt_clause"

and case_clause fd typhash = function
        | CaseStmt (lbls, body) ->
            fprintf fd "\t";
            if lbls <> [] then List.iter (function
               | Id lbl -> fprintf fd "%s" lbl
               | Intgr _ as lbl -> fprintf fd "%s" (vexpr typhash lbl)
               | Number _ as lbl -> fprintf fd "%s" (vexpr typhash lbl)
               | Atom "default" -> fprintf fd "default"
               | PackageBody (pkg, Id lbl :: []) -> fprintf fd "                %s::%s       =>  " pkg lbl
	       | OpenRange lst -> fprintf fd "%s" (String.concat ", " (List.map (function
		   | Id id -> id
		   | ValueRange (lft, rght) -> " [" ^ vexpr typhash lft ^ " : " ^ vexpr typhash rght ^ " ] "
		   | oth -> dump_unhand := Some oth; failwith "open range") lst))
		   | ValueRange(lft, rght) -> fprintf fd " [%s.%s] " (vexpr typhash lft) (vexpr typhash rght)
	       | ExprOKL lbls -> List.iter (function
		   | Number _ as lbl -> fprintf fd "                case %s:  " (vexpr typhash lbl)
		   | oth -> dump_unhand := Some oth; failwith "case_label'") lbls
	       | oth -> dump_unhand := Some oth; failwith "case_label") lbls
                else fprintf fd "default";
	    fprintf fd " : begin\n\t";
            List.iter (stmt_clause fd typhash) body;
	    fprintf fd "end\n";
        | Id lbl -> fprintf fd "                %s: ; " lbl
        | Number _ as lbl -> fprintf fd "                %s:  " (vexpr typhash lbl)
        | Atom "default" -> fprintf fd "                default: "
	| Atom ":" -> ()
	| Atom ";" -> fprintf fd "                ; "
        | PackageBody (pkg, Id id :: []) -> fprintf fd "                case %s::%s: " pkg id
        | Itmlst lst -> List.iter (case_clause fd typhash) lst
        | (Seq _ | Blocking _ | If1 _ |If2 _ | ForLoop _ | CaseStartUniq _ ) as x -> stmt_clause fd typhash x
        | Return expr -> fprintf fd "return %s;\n" (vexpr typhash expr)
	| oth -> dump_unhand := Some oth; failwith "case_item"

let dump_unhand_conn = ref None

let dump_conn typhash = function
  | CellPinItem2 (pin, ExprOKL (InitPair _ :: _)) as x when false -> dump_unhand := Some x; failwith "dump_conn'"
  | CellPinItem2 (pin, ExprOKL (InitPair (Typ8 (SUDecl (Atom "packed", sulst), Deflt), InitPat patlst) :: _)) ->
     "." ^ pin ^ " ( { " ^ String.concat ", " (List.map (function
      | (SUMember (arg1, arg2), PatMember1 (Id rhs, expr)) -> print_endline rhs; rhs
      | oth -> dump_unhand_conn := Some oth; failwith "dump_conn'") (List.combine sulst patlst))^" } )"
  | (Id _ | CellPinItem2 _ | CellPinItemImplied _ | CellPinItemNC _) as x -> vexpr typhash x
  | oth -> dump_unhand := Some oth; failwith "dump_conn"

let instance_dump_template fd typhash (typ:rw) params inst pinlst =
  match typ with 
    | Id typ ->
    output_string fd (typ ^ " " ^
                      inst^" (\n\t"^
                      String.concat ",\n\t" (List.map (dump_conn typhash) pinlst)^");\n")
    | oth -> failwith "instance type"

let dump_struct_single fd typhash nam memblst =
  let sulst = List.flatten (List.map (struct_union typhash) memblst) in
  dbgdmpsu := Some sulst;
  let wid = csiz' typhash (Vsu (Id nam, sulst)) in
  fprintf fd "logic [%d : 0] %s; // 607\n" (wid-1) nam

let rec dump_union fd typhash = function
  | SUMember (Typ5 (Atom "logic", [AnyRange (hi, lo)]), id_lst) -> List.iter (function
        | Id id ->
          let wid = ceval typhash hi - ceval typhash lo + 1 in
          fprintf fd "logic [%d : 0] %s; // 657\n" (wid-1) id
        | oth -> dbgdmpu := Some oth; failwith "dump_union_713") id_lst
  | SUMember (Typ8 (SUDecl (Atom ("packed"|"signed" as signage), memblst), Deflt), id_lst) -> List.iter (function
        | Id id ->
          let sulst = List.flatten (List.map (struct_union typhash) memblst) in
          let wid = csiz' typhash (Vsu (Id id, sulst)) in
          fprintf fd "logic [%d : 0] %s; // 663\n" (wid-1) id
        | oth -> dbgdmpu := Some oth; failwith "dump_union_719") id_lst
  | SUMember (Typ5 (TypEnum3 [AnyRange (hi, lo)], e_lst), id_lst) -> List.iter (function
        | Id id ->
          let wid = ceval typhash hi - ceval typhash lo + 1 in
          fprintf fd "logic [%d : 0] %s; // 657\n" (wid-1) id
        | oth -> dbgdmpu := Some oth; failwith "dump_union_724") id_lst
  | SUMember (Typ8 (Atom kind, Deflt), id_lst) -> List.iter (function
        | Id id ->
          let wid = atom_width kind in
          fprintf fd "logic [%d : 0] %s; // 657\n" (wid-1) id
        | oth -> dbgdmpu := Some oth; failwith "dump_union_729") id_lst
  | SUMember (TypEnum6 (old_id, TypEnum3 [AnyRange (hi, lo)], e_lst), id_lst) -> List.iter (function
        | Id id ->
          let wid = ceval typhash hi - ceval typhash lo + 1 in
          fprintf fd "logic [%d : 0] %s; // 657\n" (wid-1) id
        | oth -> dbgdmpu := Some oth; failwith "dump_union_724") id_lst
  | SUMember (Typ6 (PackageRef (pkgid, idp)), id_lst) -> dump_union fd typhash (SUMember (find_pkg pkgid idp, id_lst))
  | oth -> dbgdmpu := Some oth; failwith "dump_union_726"
(*
  (dump_vdir dir^" ["^string_of_int (wid-1)^" : 0] "^nam)
*)

let dbgcomb = ref None

let rec proc_dump_template fd typhash modules = function
    | AlwaysFF (At (EventOr (Pos clk :: _ as dep_lst)), sent_lst) ->
  fprintf fd "    // 825 clocked process description goes here\n";
  dump_deps fd "always_ff" dep_lst;
  fprintf fd "    begin\n";
  stmt_clause fd typhash sent_lst;       
  fprintf fd "    end; // 947\n";
  fprintf fd "\n";
    | AlwaysLegacy (At (EventOr (Pos clk :: _ as dep_lst)), sent_lst) ->
  fprintf fd "    // 833 clocked process description goes here\n";
  dump_deps fd "always" dep_lst;
  fprintf fd "    begin\n";
  stmt_clause fd typhash sent_lst;
  fprintf fd "    end; // 947\n";
  fprintf fd "\n";
    | AlwaysLegacy (At (EventOr dep_lst), sent_lst) ->
  fprintf fd "    // 841 combinational process description goes here\n";
  dump_deps_comb fd typhash "always" dep_lst;
  fprintf fd "    begin\n";
  stmt_clause fd typhash sent_lst;
  fprintf fd "    end; // 979\n";
  fprintf fd "\n";
    | AlwaysLegacy (AtStar, sent_lst) ->
  fprintf fd "    // 849 combinational process description goes here\n";
  dump_deps_comb fd typhash "always" [AtStar];
  fprintf fd "    begin\n";
  stmt_clause fd typhash sent_lst;
  fprintf fd "    end; // 979\n";
  fprintf fd "\n";
    | AlwaysComb2 Assert -> ()
    | AlwaysComb2 (sent_lst) ->
      dbgcomb := Some sent_lst;
  fprintf fd "    // 853 combinational process description goes here\n";
  dump_deps_comb fd typhash "always" [AtStar];
  fprintf fd "    begin\n";
  stmt_clause fd typhash sent_lst;
  fprintf fd "    end; // 987\n";
  fprintf fd "\n";
    | AlwaysLatch ( sent_lst ) ->
  fprintf fd "    // 865 combinational latch process description goes here\n";
  fprintf fd "    always_latch\n";
  fprintf fd "    begin\n";
  stmt_clause fd typhash sent_lst;
  fprintf fd "    end; // 995\n";
  fprintf fd "\n";
  (* elaboration case *)
   | CaseStart (Id id, (CaseItm (BeginBlock [] :: Unknown ("$error",_) :: Deflt :: []) :: [])) -> fprintf fd "// elaboration case %s\n" id;
    | ContAsgn lst -> List.iter (function
      | Asgn1 (Id lhs, ExprQuote1 _) -> print_endline ("ExprQuote1: "^lhs)
      | Asgn1 (lhs, expr) -> dbgasgn := Some (lhs,expr); asgn fd typhash expr lhs
      | oth -> dump_unhand := Some oth; failwith "assign_dump_template") lst
    | Iff _ -> ()
    | InstDecl (Typ8 (Union (Atom ("packed"|"signed" as signage), lst), Deflt), [], id_lst) -> List.iter (function
        | Id id ->
        fprintf fd "union packed { // 718\n";
        dbgunion := lst :: !dbgunion;
        List.iter (dump_union fd typhash) lst;
        fprintf fd "} %s;\n" id) id_lst
    | InstDecl (Typ5 (TypEnum3 [AnyRange (hi, lo)], e_lst), [], inst_lst) -> List.iter (function
        | Id id -> fprintf fd "logic [%s:%s] %s; // 844\n" (vexpr typhash hi) (vexpr typhash lo) id
        | oth -> dump_unhand := Some oth; failwith "InstDecl829") inst_lst;
    | InstDecl (Typ8 (SUDecl (Atom ("packed"|"signed" as signage), lst), Deflt), [], inst_lst) ->
        fprintf fd "struct packed { // 622\n";
        List.iter (dump_struct fd typhash) lst;
        fprintf fd "} %s;\n" (String.concat ", " (List.map (vexpr typhash) inst_lst));
    | InstDecl (typ, params, lst) as x -> List.iter (function
        | InstNameParen1 (inst, pins) -> instance_dump_template fd typhash typ (match params with Itmlst lst :: _ -> lst | _ -> []) inst pins
        | InstNameParen2 (inst, InstRange(lft,rght) :: []) -> instance_dump_template fd typhash typ (match params with Itmlst lst :: _ -> lst | _ -> []) inst []
        | Id id -> dump_unhand := Some x; failwith "InstDecl"
        | oth -> dump_unhand := Some oth; failwith "InstDecl829") lst;
    | Typ5 (Union (Atom "packed", lst), [Id "u"]) when false -> ()
    | Typ5 (Union (Atom ("packed"|"signed" as signage), lst), id_lst) -> List.iter (function
        | Id id ->
        fprintf fd "union packed { // 718\n";
        dbgunion := lst :: !dbgunion;
        List.iter (dump_union fd typhash) lst;
        fprintf fd "} %s;\n" id) id_lst
    | Typ11 (Typ8 (SUDecl (Atom ("packed"|"signed" as signage), memblst), Deflt), AnyRange (hi,lo) :: [], inst_lst) -> List.iter (function
        | Id stem -> dump_struct_single fd typhash stem memblst;
        | oth -> failwith "dump_sysver.ml:916") inst_lst
    | Typ12 ([AnyRange (hi, lo)], Typ8 (SUDecl (Atom ("packed"|"signed" as signage), memblst), Deflt), inst_lst) -> List.iter (function
        | Id stem ->
          let sep = ";\n" in
          let sulst = List.flatten (List.map (struct_union typhash) memblst) in
          dbgsulst := (stem,sulst) :: !dbgsulst;
          let cat = String.concat sep (List.map (dump_unpack_typ typhash "logic" sep stem) sulst) in
          fprintf fd "%s; // 755\n" cat
        | oth -> failwith "dump_sysver.ml:745") inst_lst
    | Typ12 (AnyRange (hi, lo) :: [], Typ5 (TypEnum3 (AnyRange (hi', lo') :: []), e_lst), inst_lst) -> ()
    | Itmlst lst -> List.iter (proc_dump_template fd typhash modules) lst
    | Seq (lbl, lst) -> List.iter (proc_dump_template fd typhash modules) lst
    | Typ11 (TypEnum6 (old_id, TypEnum3 (AnyRange (lft, rght) :: []), elst), AnyRange (lft', rght') :: [], id_lst) ->
        List.iter (function
            | Id id as nam -> fprintf fd "logic [%d: 0] %s; // 930\n" (width typhash nam - 1) id
            | oth -> failwith "dump_sysver.ml:931") id_lst
    | NetDecl (Atom "wire" :: [], wire_lst) -> List.iter (function
          | Id nam ->
	      fprintf fd "logic %s; // 610\n" nam
	  | DeclAsgn (nam, AnyRange (hi, lo) :: []) ->
              fprintf fd "logic [%s:%s] %s; // 612\n" (vexpr typhash hi) (vexpr typhash lo) (vexpr typhash nam)
          | InitSig (nam, expr) -> (function
	      | Id id -> fprintf fd "logic %s = %s; // 614\n" (vexpr typhash nam) (vexpr typhash expr)
              | ExprOKL lbls -> fprintf fd "logic %s : %s; // 615\n" (vexpr typhash nam) (String.concat "; " (List.map (vexpr typhash) lbls))
	      | SysFuncCall ("$random", [Deflt]) -> fprintf fd "    signal %s : $random; // 616\n" (vexpr typhash nam)
              | Query _ as x -> fprintf fd "logic %s : %s; // 617\n" (vexpr typhash nam) (vexpr typhash x)

	      | oth -> dump_unhand := Some oth; failwith "initsig") expr
	  | oth -> dump_unhand := Some oth; failwith "NetDecl'") wire_lst;
    | Itmlst (id_lst) -> List.iter (function
	  | Id nam -> fprintf fd "logic %s; // 544\n" nam
          | Seq(lbl, lst) -> List.iter (proc_dump_template fd typhash modules) lst
	  | oth -> dump_unhand := Some oth; failwith "DeclLogic647") id_lst;
    | DeclLogic (reg_lst) -> List.iter (function
	  | Id nam ->  fprintf fd "logic %s; // 848\n" nam
          | DeclAsgn (nam, AnyRange (hi, lo) :: []) ->
              fprintf fd "logic [%s : %s] %s; // 628\n" (vexpr typhash hi) (vexpr typhash lo) (vexpr typhash nam)
          | VarDeclAsgn (nam, expr) ->
              fprintf fd "logic %s = %s\n" (vexpr typhash nam) (vexpr typhash expr)
	  | oth -> dump_unhand := Some oth; failwith "DeclLogic651"
        ) reg_lst;
    | DeclLogic2 (wire_lst, (AnyRange (hi, lo) as x) :: []) -> List.iter (function
	  | Id nam -> fprintf fd "logic [%s : %s] %s; // 635\n" (vexpr typhash hi) (vexpr typhash lo) (nam)
	  | DeclAsgn (nam, (AnyRange _ :: _ as dims)) ->
              let wid = width typhash nam in
              printf "mem siz = %d\n" wid;
              fprintf fd "logic [%d : 0] %s ; // 746\n" (wid - 1) (vexpr typhash nam)
	  | oth -> dump_unhand := Some oth; failwith "DeclLogic2") wire_lst
    | DeclLogic2 (wire_lst, AnyRange (hi, lo) :: AnyRange (hi', lo') :: []) -> List.iter (function
	  | Id nam ->
	  fprintf fd "logic [%s : %s] [%s : %s] %s ; // 641\n" (vexpr typhash hi) (vexpr typhash lo) (vexpr typhash hi') (vexpr typhash lo') nam
	  | oth -> dump_unhand := Some oth; failwith "DeclWire") wire_lst
    | DeclLogic2 (wire_lst, AnyRange (hi, lo) :: AnyRange (hi', lo') :: AnyRange (hi'', lo'') :: []) -> List.iter (function
	  | Id nam ->
	  fprintf fd "logic [%s : %s] [%s : %s] [%s : %s] %s ; // 645\n" (vexpr typhash hi) (vexpr typhash lo) (vexpr typhash hi') (vexpr typhash lo') (vexpr typhash hi'') (vexpr typhash lo'') nam
	  | oth -> dump_unhand := Some oth; failwith "DeclWire") wire_lst
    | DeclReg (reg_lst, [], Deflt) -> List.iter (function
      | Id nam -> fprintf fd "logic %s; // 535\n" nam
      | oth -> dump_unhand := Some oth; failwith "DeclReg547") reg_lst
    | DeclReg (reg_lst, [AnyRange(hi,lo)], Deflt) -> List.iter (function
      | Id nam -> fprintf fd "logic [%s:%s] %s; // 538\n" (vexpr typhash hi) (vexpr typhash lo) nam
      | DeclAsgn (Id nam, (AnyRange (first,last) :: [])) ->
        fprintf fd "logic [%s:%s] %s[%s:%s]; // 550\n" (vexpr typhash hi) (vexpr typhash lo) nam (vexpr typhash first) (vexpr typhash last)
      | VarDeclAsgn (Id nam, expr) -> fprintf fd "reg [%s:%s] %s = %s;\n" (vexpr typhash hi) (vexpr typhash lo) nam (vexpr typhash expr)
      | oth -> dump_unhand := Some oth; failwith "DeclReg550") reg_lst
    | DeclInt2 id_lst -> List.iter (function
	| Id itm -> fprintf fd "logic %s; // 672\n" itm
        | VarDeclAsgn (id, expr) -> fprintf fd "logic %s = %s\n" (vexpr typhash id) (vexpr typhash expr)
        | Intgr _ -> () (* probably a result of loop unrolling and substitution *)
        | oth -> dump_unhand := Some oth; failwith "DeclInt2") id_lst
    | InstDecl (typ, params, lst) -> List.iter (function
        | (InstNameParen1 _ | InstNameParen2 _) -> ()
        | Id _ as id -> fprintf fd "logic %s; // 677\n" (vexpr typhash id)
        | oth -> dump_unhand := Some oth; failwith "InstDecl586") lst;
    | Typ2 (id_t, [Typ8 (SUDecl (Atom "packed", lst), Deflt)], inst_lst) ->
        fprintf fd "struct packed { // 888\n";
        List.iter (dump_struct fd typhash) lst;
        fprintf fd "} %s;\n" (String.concat ", " (List.map (vexpr typhash) inst_lst));
    | Typ2 (nam, _, id_lst) ->
        List.iter (function
	     | Id _ as itm -> let s = vexpr typhash itm in  fprintf fd "logic %s : %s; // 684\n" s nam
             | DeclAsgn (id, AnyRange(lft,rght) :: AnyRange(lft',rght') :: []) ->
                fprintf fd "logic %s; // 686\n" (vexpr typhash id)
             | oth -> dump_unhand := Some oth; failwith "Typ2") id_lst;
    | Typ3 (nam, id_lst) -> List.iter (fun _ -> ()) id_lst
    | Typ4 (nam, pkg, rng, id_lst) -> List.iter (fun _ -> ()) id_lst
    | Typ9 (orig_id, id_lst, Typ5 (TypEnum3 (AnyRange(lft,rght) :: []), elst)) -> List.iter (function
          | Id id -> fprintf fd "%s %s;\n" orig_id id
          | oth -> dump_unhand := Some oth; failwith "enum range") id_lst
    | Typ9 (orig_id, id_lst, Typ5 (Deflt, elst)) -> List.iter (function
          | Id id -> fprintf fd "%s %s;\n" orig_id id
            | oth -> dump_unhand := Some oth; failwith "enum range") id_lst
    | Typ5 (SUDecl (Atom ("packed"|"signed" as signage), lst), inst_lst) -> List.iter (function
        Id id ->
        fprintf fd "typedef struct packed { // 926\n";
        List.iter (dump_struct fd typhash) lst;
        fprintf fd "} %s;\n" id;
        | DeclAsgn (Id id, [AnyRange (lft, rght)]) ->
        fprintf fd "struct packed { // 930\n";
        List.iter (dump_struct fd typhash) lst;
        fprintf fd "} %s[%s:%s]; // 932\n" id (vexpr typhash lft) (vexpr typhash rght);
        | DeclAsgn (Id id, (AnyRange _ :: _ as dims)) ->
        fprintf fd "struct packed { // 930\n";
        List.iter (dump_struct fd typhash) lst;
        let dims' = String.concat " " (List.map (fun (hi,lo) -> sprintf "[%s:%s]" (vexpr typhash hi) (vexpr typhash lo)) (mapdims dims)) in
        fprintf fd "} %s %s;\n" dims' id
        | oth -> dump_unhand := Some oth; failwith "dump_sysver:929") inst_lst
    | Typ5 (Typ5 (Atom "logic", AnyRange(hi,lo) :: []), inst_lst) -> List.iter (function
        | Id id -> fprintf fd "logic %s; // 619\n" id
                     | oth -> failwith "dump_sysver.ml:615") inst_lst
    | Typ5 (Typ8 (SUDecl (Atom ("packed"|"signed" as signage), lst), su_lst), inst_lst) ->
        fprintf fd "struct packed { // 622\n";
        List.iter (dump_struct fd typhash) lst;
        fprintf fd "} %s;\n" (String.concat ", " (List.map (vexpr typhash) inst_lst));
    | Typ9 (old_id, inst_lst, Typ8 (SUDecl (Atom ("packed"|"signed" as signage), su_lst), Deflt)) ->
        fprintf fd "struct packed { // 917\n";
        List.iter (dump_struct fd typhash) su_lst;
        fprintf fd "} %s;\n" (String.concat ", " (List.map (vexpr typhash) inst_lst));
    | Typ6 (SUDecl (Atom ("packed"|"signed" as signage), lst)) ->
        fprintf fd "typedef struct packed { // 630\n";
        List.iter (dump_struct fd typhash) lst;
        fprintf fd "};\n"
    | Typ7 (nam, Typ5 (Atom "logic", AnyRange(lft,rght) :: AnyRange(lft',rght') :: AnyRange(lft'',rght'') :: [])) ->
        fprintf fd "logic %s; // 692\n" nam
    | Typ7 (nam, Typ5 (Atom "logic", AnyRange(lft,rght) :: AnyRange(lft',rght') :: [])) ->
        fprintf fd "logic %s; // 694\n" nam
    | Typ7 (nam, Typ5 (Atom "logic", AnyRange(lft,rght) :: [])) ->
        fprintf fd "typedef logic [%s:%s] %s; // 696\n" (vexpr typhash lft) (vexpr typhash rght) nam
    | Typ7 (nam, Typ8 (SUDecl (Atom ("packed"|"signed" as signage), lst), Deflt)) ->
        fprintf fd "typedef struct packed { // 646\n";
        List.iter (dump_struct fd typhash) lst;
        fprintf fd "} %s;\n" nam
    | Typ7 (id_t, Typ8 (Union (Atom ("packed"|"signed" as signage), lst), Deflt)) ->
        fprintf fd "typedef union packed { // 718\n";
        dbgunion := lst :: !dbgunion;
        List.iter (dump_union fd typhash) lst;
        fprintf fd "} %s;\n" id_t
    | Typ7 (id_t, Typ8 (Itmlst lst, Deflt)) ->
        fprintf fd "typedef struct { // 722\n";
        List.iter (dump_struct fd typhash) lst;
        fprintf fd "} %s;\n" id_t
    | TypEnum4 (TypEnum3 (AnyRange(lft,rght) :: []), id_lst, id_lst') ->
        let kind = Unsigned_vector(lft,rght) in
        let f' itm = let s = vexpr typhash itm in  s in
        let f'' = function Id nam -> fprintf fd "typedef enum {%s} %s; // 606\n" (String.concat ", " (List.map f' id_lst)) nam | _ -> () in
        List.iter f'' id_lst'
    | TypEnum4 (Deflt, id_lst, id_lst') ->
        let f' itm = let s = vexpr typhash itm in  s in
        let f'' = function Id nam -> fprintf fd "    typedef enum {%s} %s; // 610\n" (String.concat ", " (List.map f' id_lst)) nam | _ -> () in
        List.iter f'' id_lst'
    | TypEnum4 (TypEnum5 (Atom "logic"), id_lst, id_lst') ->
        let f' itm = let s = vexpr typhash itm in  s in
        let f'' = function Id nam -> fprintf fd "    typedef enum {%s} %s; // 614\n" (String.concat ", " (List.map f' id_lst)) nam | _ -> () in
        List.iter f'' id_lst'
    | TypEnum6 (nam, TypEnum3 (AnyRange(lft,rght) :: []), id_lst) -> dump_enum fd typhash "typedef // 670\n" lft rght id_lst (nam^"; ")
    | Typ5 (TypEnum6 (old_id, TypEnum3 (AnyRange(lft,rght) :: []), eid_lst), id_lst) ->
          List.iter (function
            | Id itm ->
(*
              dump_enum fd typhash "// 671\n" lft rght eid_lst (itm^"; ")
*)
              fprintf fd "logic [%s:%s] %s; // 880\n" (vexpr typhash lft) (vexpr typhash rght) itm
	    | oth -> dump_unhand := Some oth; failwith "TypEnum6") id_lst
    | TypEnum6 (nam, TypEnum5 (Atom "logic"), id_lst) -> 
        fprintf fd "typedef enum logic {\n\t%s\n} %s; // 660\n" (String.concat ",\n\t" (List.map (function
        | Id e -> e
        | EnumInit (e, expr) ->
	    let s = vexpr typhash expr in
	    let s' = sprintf "%s = %s" e s in
	    s'
	| oth -> dump_unhand := Some oth; failwith "TypEnum6") id_lst)) nam
    | TypEnum6 (nam, Deflt, id_lst) -> 
        fprintf fd "typedef enum logic {\n\t%s\n} %s; // 974\n" (String.concat ",\n\t" (List.map (function
        | Id e -> e
        | EnumInit (e, expr) ->
	    let s = vexpr typhash expr in
	    let s' = sprintf "%s = %s" e s in
	    s'
	| oth -> dump_unhand := Some oth; failwith "TypEnum6") id_lst)) nam
    | TypEnum6 (nam, Typ8 (Atom "int", Atom "unsigned"), id_lst) -> 
        fprintf fd "typedef enum int {\n\t%s\n} %s; // 634\n" (String.concat ",\n\t" (List.map (function
        | Id e -> e
        | EnumInit (e, expr) ->
	    let s = vexpr typhash expr in
	    let s' = sprintf "%s = %s" e s in
	    s'
	| oth -> dump_unhand := Some oth; failwith "TypEnum6") id_lst)) nam
    | ParamDecl (Atom "localparam", [ParamAsgn1 (nam, expr)]) -> fprintf fd "    localparam %s = %s; // 740\n" nam (vexpr typhash expr)
    | Typ6 (Atom ("packed"|"signed" as signage)) -> () (* placeholder *)
    | Typ10 (id_t, AnyRange (lft, rght) :: [], id_t') -> () (* placeholder *)
    | ParamDecl (LocalParamTyp (Typ5 (TypEnum3 [AnyRange (lft, rght)], e_lst)), param_lst) -> List.iter (function
          | ParamAsgn1 (nam, expr) ->
              let wid = width typhash expr in
              ()
	  | oth -> dump_unhand := Some oth; failwith "localparam_int") param_lst;
    | ParamDecl (LocalParamTyp (Typ1 id), ParamAsgn1 (nam, init) :: []) ->
    (match init with
	   | InitPat lst ->
               fprintf fd "localparam %s %s = // 935\n" id nam;
               List.iter (function
                   | AsgnPat lst -> List.iter (fun itm -> fprintf fd "    parameter %s = %s; // 940\n" nam (vexpr typhash itm)) lst
		   | PatMember1 (Id id, AsgnPat lst) -> List.iter (function
                      | PatMemberDflt expr -> fprintf fd "    parameter %s = %s; // 941\n" nam (vexpr typhash expr)
                      | AsgnPat (PatMemberDflt expr :: []) -> fprintf fd "    parameter %s = %s; // 941\n" nam (vexpr typhash expr)
		      | oth -> dump_unhand := Some oth; failwith "ParamDecl'''") lst
		   | PatMember1 (Id id, (Id _ | Number _ | ExprOKL _ as x)) -> fprintf fd "    %s: %s; // 942\n" id (vexpr typhash x)
                   | (Number _ | Id _ as x) -> fprintf fd "    parameter %s = %s; // 941\n" nam (vexpr typhash x)
	           | oth -> dump_unhand := Some oth; failwith "ParamDecl''") lst
           | (Number _ | Query _ | Expression _  as x) -> fprintf fd "    parameter %s = %s; // 943\n" nam (vexpr typhash x)
           | oth -> dump_unhand := Some oth; failwith "ParamDecl'")
    | ParamDecl (LocalParamTyp (Typ3 (id, PackageBody (pkg, []) :: [])), ParamAsgn1 (nam, cexpr) :: []) -> ()
    | ParamDecl (LocalParamTyp (Typ3 (id_t, AnyRange (lft, rght) :: [])), ParamAsgn1 (nam, InitPat lst) :: []) -> List.iter (function
        | AsgnPat lst -> List.iter (fun itm -> fprintf fd "    parameter %s = %s; // 947\n" nam (vexpr typhash itm)) lst
	| oth -> dump_unhand := Some oth; failwith "ParamDecl") lst
    | ParamDecl (LocalParamTyp (Typ5 (Atom "logic", AnyRange (lft, rght) :: [])), [ParamAsgn2 (nam, [AnyRange (lft', rght')], InitPat lst)]) ->
        fprintf fd "%s; // 773\n" nam
    | ParamDecl (LocalParamTyp (Typ5 (Atom "logic", AnyRange (lft, rght) :: AnyRange (lft', rght') :: [])), [ParamAsgn1 (nam, expr)]) ->
        fprintf fd "%s = %s; // 775\n" nam (vexpr typhash expr)
    | ParamDecl (LocalParamTyp (Typ5 (Atom "logic", AnyRange (lft, rght) :: [])), ParamAsgn1 (nam, expr) :: []) ->
        fprintf fd "localparam logic [%s:%s] %s = %s; // 777\n" (vexpr typhash lft) (vexpr typhash rght) nam (vexpr typhash expr)
    | ParamDecl (LocalParamTyp (Typ6 (Atom ("bit"|"logic"))), lst) -> List.iter (function
	        | ParamAsgn1 (id, expr) -> ()
                | oth -> dump_unhand := Some oth; failwith "param_asgn") lst
    | ParamDecl (LocalParamTyp (Typ8 (Atom ("int"|"integer"|"longint"), Deflt)), [ParamAsgn1 (nam , expr)]) ->
        fprintf fd "%s = %s; // 782\n" nam (vexpr typhash expr)
    | ParamDecl (LocalParamTyp (Typ8 (Atom "int", Atom "unsigned")), [ParamAsgn1 (nam , expr)]) ->
        fprintf fd "localparam unsigned %s = %s; // 915\n" nam (vexpr typhash expr)
    | ParamDecl (LocalParamTyp (Typ8 (Atom ("int"|"integer"|"longint" as kind), Atom kind')), [ParamAsgn1 (nam , expr)]) ->
        fprintf fd "localparam %s %s %s = %s; // 915\n" kind' kind nam (vexpr typhash expr)
    | ParamDecl (LocalParamTyp (Typ8 (SUDecl (Atom ("packed"|"signed" as signage), sulst), Deflt)), id_lst) -> 
        fprintf fd "localparamtyp SUDecl ...; // 737\n"
    | FunDecl (fn, typ, FunGuts (ports, lst)) ->
        fprintf fd "// function %s ...\n" fn;
(*
        List.iter (stmt_clause fd typhash) lst;
        fprintf fd "    endfunction\n";
*)
    | AutoFunDecl (fn, typ, FunGuts (ports, lst)) ->
        fprintf fd "// function automatic %s ...\n" fn;
(*
        List.iter (stmt_clause fd typhash) lst;
        fprintf fd "    endfunction\n";
*)
    | FunDecl _ -> ()
    | AutoFunDecl _ -> ()
    | PkgImport (Itmlst lst) -> List.iter (proc_dump_template fd typhash modules) lst
    | PkgImportItm (pkg, Atom "*") -> ()
    | DeclData (Typ5 (Atom "logic", AnyRange (lft, rght) :: AnyRange (lft', rght') :: []), Deflt, VarDeclAsgn (mem, ExprOKL lst) :: []) -> () (* placeholder *)
    | AssertProperty -> ()
    | Port (dir, nam, [AnyRange (hi, lo)], Deflt) -> ()
    | Port (dir, nam, [AnyRange (hi, lo)], Atom "signed") -> ()
    | Typ12 (AnyRange (hi, lo) :: [], Typ5 (TypEnum3 (AnyRange (hi', lo') :: []), e_lst), inst_lst) -> List.iter (function
        | Id id -> fprintf fd "logic %s; // 733\n" id
        | oth -> failwith "dump_sysver.ml:733") inst_lst
    | Typ12 ([AnyRange (hi, lo)], Typ8 (SUDecl (Atom ("packed"|"signed" as signage), memblst), Deflt), inst_lst) -> List.iter (function
(*
        | Id stem -> dump_struct_flat fd typhash memblst stem;
*)
        | oth -> failwith "dump_sysver.ml:751") inst_lst
    | Typ11 (Typ8 (SUDecl (Atom ("packed"|"signed" as signage), memblst), Deflt), AnyRange (hi, lo) :: [], inst_lst) -> List.iter (function
        | Id stem ->
        fprintf fd "struct packed { // 756\n";
        List.iter (dump_struct fd typhash) memblst;
        fprintf fd "} [%s:%s] %s;\n" (vexpr typhash hi) (vexpr typhash lo) (String.concat ", " (List.map (vexpr typhash) inst_lst))
        | oth -> dump_unhand := Some oth; failwith "dump_sysver.ml:759") inst_lst
    | Typ11 (TypEnum6 (old_id, TypEnum3 (AnyRange (lft, rght) :: []), elst), AnyRange (lft', rght') :: [], id_lst) ->
        List.iter (function
            | Id nam -> dump_enum fd typhash "" lft rght elst (sprintf "%s [%s:%s]; // 768" nam (vexpr typhash lft') (vexpr typhash rght'))
            | oth -> dump_unhand := Some oth; failwith "dump_sysver.ml:765") id_lst
    | Typ11 (Typ8 (SUDecl (Atom "packed", memblst), Deflt), (AnyRange _ :: _ as dims), inst_lst) ->
        fprintf fd "struct packed { // 1125\n";
        List.iter (dump_struct fd typhash) memblst;
        let dims' = String.concat " " (List.map (fun (hi,lo) -> sprintf "[%s:%s]" (vexpr typhash hi) (vexpr typhash lo)) (mapdims dims)) in
        fprintf fd "} %s %s;\n" dims' (String.concat ", " (List.map (vexpr typhash) inst_lst))
    | Typ12 ([TypEnum6 (old_id, TypEnum3 [AnyRange (lft, rght)], e_lst)], Typ5 (TypEnum3 [AnyRange (lft', rght')], e_lst'), id_lst) ->
        List.iter (function
            | Id nam -> dump_enum fd typhash "" lft rght e_lst (sprintf "%s [%s:%s]; // 768" nam (vexpr typhash lft') (vexpr typhash rght'))
            | oth -> dump_unhand := Some oth; failwith "dump_sysver.ml:765") id_lst
    | Typ12 ([TypEnum6 (old_id, TypEnum5 (Atom "logic"), e_lst)], Typ5 (TypEnum5 (Atom "logic"), e_lst'), inst_lst) -> List.iter (function
            | Id nam -> fprintf fd "logic %s; // 1147\n" nam
            | oth -> dump_unhand := Some oth; failwith "dump_sysver.ml:1148") inst_lst
    | Typ12 ([Typ8 (SUDecl (Atom "packed", su_lst), Deflt)], Typ8 (SUDecl (Atom "packed", su_lst'), Deflt), inst_lst) -> List.iter (function
            | Id nam -> fprintf fd "logic %s; // 1151\n" nam
            | oth -> dump_unhand := Some oth; failwith "dump_sysver.ml:1148") inst_lst
    | TypEnum4 _ -> ()
    | TypEnum6 _ -> ()
    | Typ2 (typ, _, typ_lst) -> ()
    | Typ3 _ -> ()
    | Typ4 _ -> ()
(*
    | Typ5 _ -> ()
*)
    | Typ6 _ -> ()
    | Typ7 _ -> ()
    | Typ9 _ -> ()
    | DeclInt2 _ -> ()
    | NetDecl _ -> ()
    | DeclLogic2 _ -> ()
    | LoopGen1 _ -> () (* placeholder *)
    | CondGen1 _ -> () (* placeholder *)
    | GenItem _ -> () (* placeholder *)
    | ParamDecl _ -> ()
    | FunDecl (fn, Atom primtyp, FunGuts (PortItem _ :: [], lst)) -> () (* placeholder *)
    | AutoFunDecl (fn, typ, FunGuts (ports, lst)) -> () (* placeholder *)
    | Initial _ -> fprintf fd "// initial is not implemented\n"
    | Final _ -> fprintf fd "// final is not implemented\n"
    | Itmlst (Id _ :: _) -> ()
    | PkgImport _ -> ()
    | DeclData _ -> ()
    | AssertProperty -> ()
    | Port (dir, nam, [AnyRange (hi, lo)], Deflt) -> ()
    | Port (dir, nam, [AnyRange (hi, lo)], Atom "signed") -> ()
    | CaseStmt _ -> ()
    | ContAsgn _ -> ()
    | LoopGen1 _ -> ()
    | CondGen1 _ -> ()
    | GenItem _ -> ()
    | Genvar _ -> ()
    | AlwaysComb2 _ -> ()
    | AlwaysFF _ -> ()
    | AlwaysLatch _ -> ()
    | AlwaysLegacy _ -> ()
    | Initial _ -> ()
    | Final _ -> ()
    | DeclReg _ -> ()
    | DeclLogic _ -> ()
    | PackageParam _ -> ()
    | Dot3 (Id bus, Id dir, Id inst) -> fprintf fd "// %s.%s.%s // 1163\n" bus dir inst
    | DeclModPort lst -> List.iter (function
          | ModPortItm (itm_id, dirlst) -> List.iter (function
              | PortDir ((In|Out as dir), Id id) -> fprintf fd "%s" id
              | Id id -> fprintf fd "%s" id
              | oth -> dump_unhand := Some oth; failwith "del_mod_port_1204") dirlst
          | oth -> dump_unhand := Some oth; failwith "decl_mod_port_1205") lst
    | Typ11 (SUDecl (Atom ("packed"|"signed" as signage), su_lst), [AnyRange (hi, lo)], inst_lst) when false -> ()
    | Typ11 (SUDecl (Atom ("packed"|"signed" as signage), memblst), (AnyRange (hi,lo) :: []), inst_lst) -> List.iter (function
        | Id stem -> dump_struct_single fd typhash stem memblst;
        | DeclAsgn (Id id, [VarDim (Id dim)]) ->  dump_struct_single fd typhash id memblst;
        | oth -> failwith "dump_sysver.ml:916") inst_lst
    | DotBus (Id bus, Id dir, Id inst, [AnyRange (hi, lo)]) -> fprintf fd "// %s.%s.%s[%s:%s] // 1175\n" bus dir inst (vexpr typhash hi) (vexpr typhash lo)
    | Deflt -> fprintf fd "// Deflt // 1183\n"
    | Typ11 (Typ5 (TypEnum3 [AnyRange (hi, lo)], e_lst), [TypEnum6 (old_id, TypEnum3 [AnyRange (hi', lo')], e_lst')], inst_lst) -> fprintf fd "// 1197\n"
    | Typ11 (Typ5 (TypEnum3 [AnyRange (hi, lo)], e_lst), [AnyRange (hi', lo')], inst_lst) -> fprintf fd "// 1198\n"
    | Typ11 (Atom "logic", [AnyRange (lft, rght)], inst_lst) -> fprintf fd "// 1199\n"
    | Typ11 (Typ5 (TypEnum5 (Atom "logic"), e_lst), [TypEnum6 (old_id, TypEnum5 (Atom "logic"), e_lst')], inst_lst) -> fprintf fd "// 1203\n"
    | Typ11 (Typ8 (SUDecl (Atom "packed", su_lst), Deflt), [Typ8 (SUDecl (Atom "packed", su_lst'), Deflt)], inst_lst) -> fprintf fd "// 1204\n"
    | oth -> dump_unhand := Some oth; failwith "proc_dump_template"

let signcnv = function
        | (Deflt,AnyRange(hi,lo)) -> Unsigned_vector(hi,lo)
	| (Deflt,Deflt) -> Unsigned
        | (Atom "signed",Deflt) -> Signed
        | (Atom "signed",AnyRange(hi,lo)) -> Signed_vector(hi,lo)
	| (Deflt,oth) -> failwith "signcnv'"
	| oth, _ -> dump_unhand := Some oth; failwith "signcnv"

let dump_array_port typhash hi lo dir = function
  | Id nam -> (dump_vdir dir^" ["^string_of_int (ceval typhash hi)^" : "^string_of_int (ceval typhash lo)^"] "^nam)
  | oth -> dump_unhand := Some oth; failwith "dump_array_port"

let dump_port_single typhash nam dir typ = (dump_vdir dir^" "^nam)

let dump_struct_port_single typhash nam dir memblst =
  let sulst = List.flatten (List.map (struct_union typhash) memblst) in
  dbgdmpsu := Some sulst;
  let wid = csiz' typhash (Vsu (Id nam, sulst)) in
  (dump_vdir dir^" ["^string_of_int (wid-1)^" : 0] "^nam)

let dump_struct_port_array typhash nam hi lo dir memblst =
  let sulst = List.flatten (List.map (struct_union typhash) memblst) in
  dbgdmpsu := Some sulst;
  let wid = csiz' typhash (Vsu (Id nam, sulst)) in
  (dump_vdir dir^" ["^string_of_int (wid-1)^" : 0] "^nam)

let dump_port_array_dims typhash nam dir dims =
  let wid = csiz' typhash (Unsigned_array (mapdims dims)) in
  (dump_vdir dir^" ["^string_of_int (wid-1)^" : 0] "^nam)

let rec dump_port typhash = function
    | Port ((In|Out|Inout|Deflt) as dir, nam, [], sgn) -> dump_port_single typhash nam dir (signcnv (sgn, Deflt))
    | Port (PortDir ((In|Out|Inout|Deflt) as dir, Atom ("wire"|"reg"|"logic")), nam, [], sgn) -> dump_port_single typhash nam dir (signcnv (sgn, Deflt))
    | Port ((In|Out|Inout) as dir, nam, AnyRange (hi, lo) :: [], sgn) -> dump_array_port typhash hi lo dir (Id nam)
    | Port (PortDir ((In|Out|Inout|Deflt) as dir, Atom ("wire"|"reg"|"logic")), nam, AnyRange (hi, lo) :: [], sgn) -> dump_array_port typhash hi lo dir (Id nam)
    | Port ((In|Out|Inout) as dir, nam, AnyRange (hi, lo) :: AnyRange(hi', lo') :: [], sgn) -> dump_array_port typhash hi lo dir (Id nam)
    | Port ((In|Out|Inout) as dir, nam, AnyRange (hi, lo) :: AnyRange(hi', lo') :: AnyRange(hi'', lo'') :: [], sgn) -> dump_array_port typhash hi lo dir (Id nam)
    | Port ((In|Out|Inout) as dir, nam, Typ6 (Atom primtyp) :: [], sgn) -> dump_port_single typhash nam dir Unsigned
    | Port (Deflt, nam, Typ2 (typ_t, PackageRef (pkg, idp) :: [], []) :: AnyRange (hi, lo) :: [], sgn) ->
        dump_array_port typhash hi lo Inout (Id nam)
    | Port ((In|Out|Inout) as dir, nam, Typ2 (typ_t, [], []) :: AnyRange (hi, lo) :: [], sgn) ->
        dump_array_port typhash hi lo dir (Id nam)
    | Port ((In|Out|Inout) as dir, nam, Typ2 (typ_t, [], []) :: [], sgn) ->
        dump_port_single typhash nam dir (signcnv (sgn, Deflt))
    | Port ((In|Out|Inout) as dir, nam, Typ2 (typ_e, (PackageRef (pkg, idp)) :: [], []) :: [], sgn) ->
        dump_port_single typhash nam dir (signcnv (sgn, Deflt))
    | Port ((In|Out|Inout) as dir, nam, Typ2 (typ_e, (PackageRef (pkg, idp)) :: [], []) :: AnyRange (hi, lo) :: [], sgn) ->
        dump_array_port typhash hi lo dir (Id nam)
    | Dot3 (bus, dir, member) -> "dot3 port"
    | DotBus (bus, dir, member, AnyRange(hi,lo) :: []) ->
        dump_array_port typhash hi lo Inout bus
    | Port ((In|Out|Inout) as dir, stem, [Typ5 (Typ8 (SUDecl (Atom ("packed"|"signed" as signage), memblst), Deflt), [])], Deflt) ->
        dump_struct_port_single typhash stem dir memblst
    | Port ((In|Out|Inout) as dir, id_i, [Typ12 ([], Typ8 (SUDecl (Atom ("packed"|"signed" as signage), memblst), Deflt), [])], Deflt) ->
        ("struct port "^id_i^" // 941")
    | Port ((In|Out|Inout) as dir, stem, [Typ8 (SUDecl (Atom ("packed"|"signed" as signage), memblst), Deflt)], Deflt) ->
        dump_struct_port_single typhash stem dir memblst
    | Port ((In|Out|Inout) as dir, nam, Typ5 (Typ5 (Atom "logic", AnyRange(hi,lo) :: []), []) :: [], Deflt) ->
        dump_array_port typhash hi lo dir (Id nam)
    | Port ((In|Inout|Out) as dir, stem, [Typ5 (Typ8 (SUDecl (Atom ("packed"|"signed" as signage), memblst), Deflt), []); AnyRange (Intgr 1, Intgr 0)], Deflt) ->
        dump_struct_port_single typhash stem dir memblst
    | Port ((In|Out|Inout) as dir, nam, [Typ5 (TypEnum6 (id_t, TypEnum3 [AnyRange (hi, lo)], e_lst), [])], Deflt) ->
      dump_array_port typhash hi lo dir (Id nam)
    | Port ((In|Out|Inout) as dir, nam, [Typ5 (TypEnum6 (old_id, TypEnum5 (Atom "logic"), e_lst), [])], Deflt) ->
        dump_port_single typhash nam dir Unsigned
    | Port ((In|Inout|Out) as dir, nam, [Typ5 (Atom "logic", [AnyRange (hi, lo)])], Deflt) ->
      dump_array_port typhash hi lo dir (Id nam)
    | Port ((In|Inout|Out) as dir, nam, [TypEnum6 (old_id, TypEnum3 [AnyRange (hi, lo)], e_lst)], Deflt) ->
      dump_array_port typhash hi lo dir (Id nam)
    | Port ((In|Inout|Out) as dir, nam, [TypEnum6 (old_id, TypEnum5 (Atom "logic"), e_lst)], Deflt) ->
      dump_port_single typhash nam dir Unsigned
    | Port ((In|Inout|Out) as dir, nam, [Typ12 ([], Typ8 (SUDecl (Atom ("packed"|"signed" as signage), memblst), Deflt), []); AnyRange (hi, lo)],  Deflt) ->
      dump_struct_port_array typhash nam hi lo dir memblst
    | Port ((In|Inout|Out) as dir, nam, [Typ5 (TypEnum3 [AnyRange (hi, lo)], e_lst)], Deflt) ->
      dump_array_port typhash hi lo dir (Id nam)
    | Port ((In|Inout|Out) as dir, nam, [Typ8 (SUDecl (Atom ("packed"|"signed" as signage), memblst), Deflt); AnyRange (hi, lo)], Deflt) ->
      dump_struct_port_array typhash nam hi lo dir memblst
    | Port ((In|Inout|Out) as dir, nam, Typ12 ([], Typ5 (TypEnum3 (AnyRange _ :: _ as dim), e_lst), []) :: (AnyRange _ :: _ as dims), Deflt) ->
      dump_port_array_dims typhash nam dir (dim@dims)
    | Port ((In|Inout|Out) as dir, nam, [Typ2 (id_t, [Typ5 (Atom "logic", [AnyRange (hi,lo)] )], [])], Deflt) ->
      dump_array_port typhash hi lo dir (Id nam)
    | Port ((In|Inout|Out) as dir, nam, [Typ2 (id_t, [TypEnum6 (old_id, TypEnum3 [AnyRange (hi,lo)], e_lst)], [])], Deflt) ->
      dump_array_port typhash hi lo dir (Id nam)
    | Port ((In|Inout|Out) as dir, nam, [Typ2 (old_t, [Typ8 (SUDecl (Atom ("packed"|"signed" as signage), memblst), Deflt)], [])], Deflt) ->
      dump_struct_port_single typhash nam dir memblst
    | Port ((In|Inout|Out) as dir, nam, [Typ12 ([TypEnum6 (id_t, TypEnum3 (AnyRange _ :: _ as dims), e_lst)], Typ5 (TypEnum3 (AnyRange _ :: _ as dims'), e_lst'), [])], Deflt) ->
      dump_port_array_dims typhash nam dir (dims@dims')
    | Port ((In|Inout|Out) as dir, nam, [Typ2 (old_id, [PackageRef (pkg, entry)], [])], Deflt) ->
        dump_port_single typhash nam dir Unsigned
    | Port ((In|Inout|Out) as dir, nam, (Typ2 (old_id, [PackageRef (pkg, entry)], []) :: AnyRange(hi,lo) :: []), Deflt) ->
      dump_array_port typhash hi lo dir (Id nam)
    | Port ((In|Inout|Out) as dir, nam, [Typ2 (old_id,[TypEnum6 (id_t, TypEnum5 (Atom "logic"), e_lst)], [])], Deflt) ->
      dump_array_port typhash (Intgr (clog2 (List.length e_lst))) (Intgr 0) dir (Id nam)
    | Port ((In|Inout|Out) as dir, nam, [Typ12 ([TypEnum6 (id_t, TypEnum5 (Atom "logic"), e_lst)], Typ5 (TypEnum5 (Atom "logic"), e_lst'), [])], Deflt) ->
      dump_array_port typhash (Intgr (clog2 (List.length e_lst))) (Intgr 0) dir (Id nam)
    | Port ((In|Inout|Out) as dir, nam, [Typ2 (old_id, [TypEnum6 (id_t, TypEnum5 (Atom "logic"), e_lst)], [])], Deflt) ->
      dump_port_single typhash nam dir Unsigned
    | Port ((In|Inout|Out) as dir, nam, [Typ2 (old_id, [Typ8 (SUDecl (Atom "packed", su_lst), Deflt)], []); AnyRange (hi, lo)], Deflt) ->
      dump_array_port typhash hi lo dir (Id nam)
    | Port ((In|Inout|Out) as dir, nam, [Typ2 (old_id, [TypEnum6 (id_t, TypEnum3 [AnyRange (hi, lo)], e_lst)], []); AnyRange (lft, rght)], Deflt) ->
      dump_array_port typhash hi lo dir (Id nam)
    | Port ((In|Inout|Out) as dir, nam, [Typ12 ([TypEnum6 (old_id, TypEnum3 [AnyRange (hi, lo)], e_lst)], Typ5 (TypEnum3 [AnyRange (lft, rght)], e_lst'), []); AnyRange (lft', rght')], Deflt) ->
      dump_array_port typhash hi lo dir (Id nam)
    | Port ((In|Inout|Out) as dir, nam, (TypEnum6 (id_t, TypEnum3 (AnyRange _ :: _ as dims), e_lst) :: (AnyRange _ :: _ as dims')), Deflt) ->
      dump_port_array_dims typhash nam dir (dims@dims')
    | Port (Deflt, nam, Typ8 (SUDecl (Atom ("packed"|"signed" as signage), memblst), Deflt) :: AnyRange (hi,lo) :: [], Deflt) ->
      dump_array_port typhash hi lo Inout (Id nam)
    | Port ((In|Inout|Out) as dir, nam, [Typ11 (Typ8 (SUDecl (Atom "packed", lst), Deflt), [], []); AnyRange (hi, lo)], Deflt) ->
      dump_array_port typhash hi lo dir (Id nam)
    | Port ((In|Inout|Out) as dir, nam, [Typ11 (Typ5 (TypEnum3 [AnyRange (hi, lo)], e_lst), [], []); AnyRange (hi', lo')], Deflt) ->
      dump_array_port typhash hi lo dir (Id nam)
    | Port ((In|Inout|Out) as dir, nam, [Atom "logic"], Deflt) ->
        dump_port_single typhash nam dir Unsigned
    | Port ((In|Inout|Out) as dir, nam, [Typ11 (Atom "logic", [], []); AnyRange (hi, lo)], Deflt) -> 
      dump_array_port typhash hi lo dir (Id nam)

    | oth -> dump_unhand := Some oth; failwith "dump_component"

let dbgports = ref []
let dbgtyplst = ref []

let dump_template fd modules = function
  | Modul(nam, parm_lst, port_lst, body_lst) as x ->
  fprintf fd "//\n";
  fprintf fd "// This converter does not currently preserve comments and license information\n";
  fprintf fd "//\n";
  fprintf fd "\n";
  let bufh', typhash, ports' = Source_text_simplify.module_header [] x in
  dbgtyplst := (nam, typhash) :: !dbgtyplst;
  dbgports := (nam, ports') :: !dbgports;
  fprintf fd "module %s (\n\t%s);\n" nam (String.concat ",\n\t" (List.map (dump_port typhash) port_lst));
(*
  parm_dump_template fd typhash parm_lst;
*)
  fprintf fd "    // Signals\n";
  let typlst, othlst = List.partition (function TypEnum6 _ -> true | _ -> false) body_lst in
  List.iter (proc_dump_template fd typhash modules) (typlst);
  let components, othlst = List.partition (function InstDecl _ -> true | _ -> false) othlst in
  List.iter (proc_dump_template fd typhash modules) (List.sort compare components);
  List.iter (proc_dump_template fd typhash modules) othlst;
  fprintf fd "\n";
  fprintf fd "endmodule // 1055\n";
  fprintf fd "\n";
  fprintf fd "\n";
  | PackageBody (pkg, body_lst) as x ->
  fprintf fd "//\n";
  fprintf fd "// This converter does not currently preserve comments and license information\n";
  fprintf fd "//\n";
  fprintf fd "\n";
  let bufh', typhash, ports' = Source_text_simplify.module_header [] x in
  fprintf fd "package %s; // 1341\n" pkg;
  List.iter (proc_dump_template fd typhash modules) body_lst;
  fprintf fd "endpackage\n";
  | IntfDecl(id, params, ports, decl) as x ->
  fprintf fd "//\n";
  fprintf fd "// This converter does not currently preserve comments and license information\n";
  fprintf fd "//\n";
  fprintf fd "\n";
  let bufh', typhash, ports' = Source_text_simplify.module_header [] x in
  fprintf fd "interface %s; // 1350\n" id;
  let itms = match decl with Itmlst lst -> lst | oth -> [oth] in
  List.iter (proc_dump_template fd typhash modules) itms;
  fprintf fd "endinterface\n";
  | oth -> dump_unhand := Some oth; failwith "This template only handles modules/packages"
