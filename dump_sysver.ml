open Source_text_rewrite_types
open Source_text_lex
open Source_text
open Source_text_simplify
open Printf

let dump_unhand = ref None
let dumps = ref ""

let rec obin w n = 
  (if w > 1 then obin (w-1) (n lsr 1) else "")^string_of_int (n land 1)

let vlst = ref []
let eqlst = ref []
let clst = ref []

let rec vexpr (typhash:(string,vtyp)Hashtbl.t) = function
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
| Add ((Id s as lhs), (Intgr n as rhs)) -> (match Hashtbl.find typhash s with
    | Unsigned_vector(hi', lo') -> let hi = ceval typhash hi' and lo = ceval typhash lo' in vexpr' typhash lhs ^ " + " ^ vexpr' typhash (Number(2, hi-lo+1, n, string_of_int n))
    | oth -> vexpr' typhash lhs ^ " + " ^ vexpr' typhash rhs)
| Add (lhs, rhs) -> vexpr' typhash lhs ^ " + " ^ vexpr' typhash rhs
| Sub ((Id s as lhs), (Intgr n as rhs)) -> (match Hashtbl.find typhash s with
    | Unsigned_vector(hi', lo') -> let hi = ceval typhash hi' and lo = ceval typhash lo' in vexpr' typhash lhs ^ " - " ^ vexpr' typhash (Number(2, hi-lo+1, n, string_of_int n))
    | oth -> vexpr' typhash lhs ^ " - " ^ vexpr' typhash rhs)
| Mult (lhs, rhs) -> vexpr' typhash lhs ^ " * " ^ vexpr' typhash rhs
| StarStar (lhs, rhs) -> vexpr' typhash lhs ^ " ** " ^ vexpr' typhash rhs
| Div (lhs, rhs) -> vexpr' typhash lhs ^ " * " ^ vexpr' typhash rhs
| Sub (lhs, rhs) -> vexpr' typhash lhs ^ " - " ^ vexpr' typhash rhs
| LtEq (lhs, rhs) -> vexpr' typhash lhs ^ " <= " ^ vexpr' typhash rhs
| UMinus (rhs) -> " - " ^ vexpr' typhash rhs
| Equals (lhs, (Number(_,1,_,_) as rhs)) -> (vexpr typhash) lhs ^ " == " ^ (vexpr typhash) rhs
| Equals ((Id s as lhs), (Intgr n as rhs)) -> (match Hashtbl.find typhash s with
    | Unsigned_vector(hi', lo') -> let hi = ceval typhash hi' and lo = ceval typhash lo' in vexpr' typhash lhs ^ " == " ^ vexpr' typhash (Number(2, hi-lo+1, n, string_of_int n))
    | oth -> vexpr' typhash lhs ^ " - " ^ vexpr' typhash rhs)
| Equals ((Id s as lhs), (Number _ as rhs)) -> (match Hashtbl.find_opt typhash s with
    | Some (Unsigned_vector _) -> vexpr' typhash lhs ^ " == " ^ vexpr' typhash rhs
    | _ -> vexpr' typhash lhs ^ " == " ^ vexpr typhash rhs)
| Equals ((Id s as lhs), (Id _ as rhs)) -> (match Hashtbl.find_opt typhash s with
    | Some (Unsigned_vector _) -> vexpr' typhash lhs ^ " == " ^ vexpr' typhash rhs
    | _ -> vexpr' typhash lhs ^ " == " ^ vexpr typhash rhs)
| Equals ((Id s as lhs), rhs) as x when is_const typhash rhs -> clst := x :: !clst; (match Hashtbl.find typhash s with
    | Unsigned_vector _ -> vexpr' typhash lhs ^ " == " ^ vexpr typhash rhs
    | _ -> vexpr' typhash lhs ^ " == " ^ vexpr typhash rhs)
| Equals ((Id s as lhs), rhs) as x -> eqlst := x :: !eqlst; (dumps := s; match Hashtbl.find typhash s with
    | MaybePort (_, Unsigned_vector _, _) -> vexpr' typhash lhs ^ " == " ^ vexpr' typhash rhs
    | Unsigned_vector _ -> vexpr' typhash lhs ^ " == " ^ vexpr' typhash rhs
    | _ -> vexpr' typhash lhs ^ " == " ^ vexpr typhash rhs)
| Equals (lhs, rhs) -> vexpr' typhash lhs ^ " == " ^ vexpr' typhash rhs
| NotEq (lhs, rhs) -> vexpr' typhash lhs ^ " != " ^ vexpr' typhash rhs
| GtEq (lhs, rhs) -> vexpr' typhash lhs ^ " >= " ^ vexpr' typhash rhs
| Less (lhs, rhs) -> vexpr' typhash lhs ^ " < " ^ vexpr' typhash rhs
| Greater (lhs, rhs) -> vexpr' typhash lhs ^ " > " ^ vexpr' typhash rhs
| Or (lhs, rhs) -> (vexpr typhash) lhs ^ " | " ^ (vexpr typhash) rhs
| Or2 (lhs, rhs) -> (vexpr typhash) lhs ^ " || " ^ (vexpr typhash) rhs
| Xor (lhs, rhs) -> (vexpr typhash) lhs ^ " ^ " ^ (vexpr typhash) rhs
| And (lhs, rhs) -> (vexpr typhash) lhs ^ " & " ^ (vexpr typhash) rhs
| And2 (lhs, rhs) -> (vexpr typhash) lhs ^ " && " ^ (vexpr typhash) rhs
| Unsigned expr -> "$unsigned("^vexpr typhash expr^")"
| Shiftl (lhs, rhs) -> "shift_left("^vexpr typhash lhs^", "^vexpr typhash rhs^")"
| Shiftr (lhs, rhs) -> "shift_right("^vexpr typhash lhs^", "^vexpr typhash rhs^")"
| Shiftr3 (lhs, rhs) -> "shift_right3("^vexpr typhash lhs^", "^vexpr typhash rhs^")"
| CellPinItem2 (port, expr) -> "." ^ port ^ " ( " ^ vexpr typhash expr ^ " ) "
| CellPinItemImplied (port) -> "." ^ port ^ " ( " ^ port ^ " ) "
| CellPinItemNC (port) -> "." ^ port ^ " ( ) "
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
| AsgnPat [PatMemberDflt (Number _ as expr)] -> "{"^(vexpr typhash expr)^"}"
| Repl (expr', [expr]) -> "{{"^(vexpr typhash expr')^"}{"^(vexpr typhash expr)^"}}"
| InsideRange (first, last) -> "inside_range("^(vexpr typhash first)^", "^(vexpr typhash last)^")"
| OpenRange lst -> "open_range("^String.concat ", " (List.map (vexpr typhash) lst)^")"
| ExprOKL lst -> " {" ^ String.concat ", " (List.map (vexpr typhash) lst) ^ "} "
| PackageBody (pkg, [Id id]) -> pkg^"::"^id
| ExprQuote1(lhs, rhs) -> vexpr typhash lhs^"'("^vexpr typhash rhs^")"
| Sys ("$unsigned", expr) -> vexpr typhash (Unsigned expr)
(*
| Sys (sys_id, arglst) -> let args = (match arglst with
        | Itmlst lst -> String.concat ", " (List.map (vexpr typhash) lst)
	| oth -> vexpr typhash oth) in
    String.sub sys_id 1 (String.length sys_id - 1) ^ "(" ^ args ^")"
*)
| Typ3 (id_t, [PackageBody (pkg, [])]) -> pkg^"::"^id_t
| PackageBody (pkg, []) -> pkg^"::"
| Atom ".*" -> "" (* placeholder *)
| Atom kind -> kind (* placeholder *)
| Typ1 id_t -> id_t
| AsgnPat lst -> String.concat "; " (List.map (vexpr typhash) lst)
| PatMember1 (Id id, expr) -> id ^ " == " ^ vexpr typhash expr
| PatMemberDflt expr -> vexpr typhash expr
| ValueRange (lft, rght) -> "["^vexpr typhash lft^" .. "^vexpr typhash rght^"]"
| String s -> s
| AtStar -> "*"
| oth -> dump_unhand := Some oth; failwith "vexpr"

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

let funtyp typhash = function
| Atom primtyp -> primtyp
| Typ1 id_t -> id_t
| Typ3 (id_t, [PackageBody (pkg, [])]) -> pkg^"::"^id_t
| Typ5 (Atom primtyp, AnyRange (lft, rght) :: []) -> sprintf "%s [%s:%s]" primtyp (vexpr typhash lft) (vexpr typhash rght)
| Typ6 (Atom primtyp) -> primtyp
| Typ8 (Atom kind, Atom kind') -> kind' ^ kind
| Typ8 (Atom kind, Deflt) -> kind
| oth -> dump_unhand := Some oth; failwith "funtyp"

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
  | oth -> let s = (vexpr typhash) oth in initlst := (oth, s) :: !initlst; s

let sel_expr typhash x = match simplify x with Intgr _ -> "0" | oth -> vexpr typhash oth

let asgn fd typhash expr = function
| Id lhs -> fprintf fd "assign %s = %s; // 384	\n" lhs (initexpr typhash expr)
| Concat _ as lst -> fprintf fd "assign %s = %s; // 385	\n" (vexpr typhash lst) (initexpr typhash expr)
| Dot1 (lft, rght) -> fprintf fd "assign %s.%s = %s; // 386	\n" (vexpr typhash lft) (vexpr typhash rght) (initexpr typhash expr)
| IdArrayed2 (Id id, ix) -> fprintf fd "assign %s[%s] = %s; // 387	\n" id (vexpr typhash ix) (initexpr typhash expr)
| IdArrayedColon (Id id, hi, lo) -> fprintf fd "assign %s[%s : %s] = %s; // 388	\n" id (vexpr typhash hi) (vexpr typhash lo) (initexpr typhash expr)
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

let rec dump_struct typhash = function
| oth -> dump_unhand := Some oth; failwith "dump_struct"

let rec dump_deps fd kind = function
| [] -> ()
| Pos (Id clk) :: [] -> fprintf fd "%s @(posedge %s)\n" kind clk
| Pos (Id clk) :: Pos (Id rst) :: [] -> fprintf fd "%s @(posedge %s or posedge %s)\n" kind clk rst
| oth :: _ -> dump_unhand := Some oth; failwith "dump_deps"

let rec dump_deps_comb fd typhash kind lst =
fprintf fd "%s @(%s)\n" kind (String.concat " or " (List.map (vexpr typhash) lst))

let rec stmt_clause fd typhash = function
      | Itmlst lst -> List.iter (stmt_clause fd typhash) lst
      | BeginBlock lst -> List.iter (stmt_clause fd typhash) lst
      | If2 (condition, if_lst, else_lst) ->
  fprintf fd "        if (%s) begin\n" (vexpr typhash condition);
    (match if_lst with BeginBlock if_lst -> List.iter (stmt_clause fd typhash) if_lst | _ -> stmt_clause fd typhash if_lst);
  fprintf fd "        end\nelse\nbegin";
    (match else_lst with BeginBlock else_lst -> List.iter (stmt_clause fd typhash) else_lst | _ -> stmt_clause fd typhash else_lst);
  fprintf fd "        end\n";
      | If1 _ as x -> iff_dump_template fd typhash x
      | DeclLogic lst -> ()
      | Seq (id, []) -> fprintf fd "       begin end; // 775	\n"
      | Seq (id, lst) ->  List.iter (stmt_clause fd typhash) lst
      | Blocking (Asgn1 (IdArrayed2(id, sel), expr)) -> fprintf fd "        %s[%s] = %s; // 777	\n" (vexpr typhash id) (vexpr typhash sel) (vexpr typhash expr)
      | Blocking (Asgn1 (Dot1(Id lft, Id rght), expr)) -> fprintf fd "        %s.%s = %s; // 778	\n" lft rght (vexpr typhash expr)
      | Blocking (Asgn1 (id, expr)) -> fprintf fd "        %s = %s; // 777	\n" (vexpr typhash id) (vexpr typhash expr)
      | Blocking (FopAsgn (id, expr)) -> fprintf fd "        %s = %s; // 779	\n" (vexpr typhash id) (vexpr typhash expr)
      | Blocking (FopAsgn1 (id, id', id'', expr)) -> fprintf fd "        %s = %s; // 780	\n" (vexpr typhash id) (vexpr typhash expr)
      | Blocking (FopAsgnArrayMemSel (id, hi, lo, expr)) -> fprintf fd "        %s[%s : %s] = %s; // 781	\n" (vexpr typhash id) (vexpr typhash hi) (vexpr typhash lo) (vexpr typhash expr)
      | Blocking (FopAsgnConcat (idlst, expr)) -> fprintf fd "        %s = %s; // 782	\n" (String.concat ", " (List.map (vexpr typhash) idlst)) (vexpr typhash expr)
      | Blocking (FopAsgnArraySel (id, ix, expr)) -> fprintf fd "        %s[%s] = %s; // 783	\n" (vexpr typhash id) (vexpr typhash ix) (vexpr typhash expr)
      | Blocking (FopAsgnArrayWid (id, hi, lo, expr)) -> fprintf fd "        %s[%s : %s] = %s; // 784	\n" (vexpr typhash id) (vexpr typhash hi) (vexpr typhash lo) (vexpr typhash expr)
      | Blocking (FopAsgnArrayRange (id, hi, lo, expr)) -> fprintf fd "        %s[%s : %s] = %s; // 785	\n" (vexpr typhash id) (vexpr typhash hi) (vexpr typhash lo) (vexpr typhash expr)
      | Blocking (FopAsgnArrayRange2 (id, ix, ix', expr)) -> fprintf fd "        %s[%s][%s] = %s; // 786	\n" (vexpr typhash id) (vexpr typhash ix) (vexpr typhash ix') (vexpr typhash expr)
      | Blocking (FopAsgnArrayField (id,  ix, expr)) -> fprintf fd "        %s.%s = %s; // 787	\n" (vexpr typhash id) (vexpr typhash ix) (vexpr typhash expr)
(*
      | Blocking (FopAsgnArrayField2 (id, ArrayedColon(Id ix, hi, lo), expr)) -> fprintf fd "        %s.%s[%s : %s] = %s; // 788	\n" (vexpr typhash id) ix (vexpr typhash hi) (vexpr typhash lo) (vexpr typhash expr)
*)
      | Blocking (FopAsgnArrayField3 (id, sel, sel', expr)) ->
          fprintf fd "        %s[%s].%s = %s; // 790	\n" (vexpr typhash id) (vexpr typhash sel) (vexpr typhash sel') (vexpr typhash expr)
      | Blocking (FopAsgnArrayField4 (id, sel, id', sel', sel'', expr)) ->
          fprintf fd "        %s[%s].%s[%s] = %s; // 792	\n" (vexpr typhash id) (vexpr typhash sel) (vexpr typhash id') (vexpr typhash sel') (vexpr typhash expr)
      | Blocking (FopAsgnArrayField5 (id, sel, id', sel', expr)) ->
          fprintf fd "        %s[%s].%s[%s] = %s; // 794	\n" (vexpr typhash id) (vexpr typhash sel) (vexpr typhash id') (vexpr typhash sel') (vexpr typhash expr)
      | Blocking (FopAsgnArrayField6 (id, sel, sel', id', expr)) ->
          fprintf fd "        %s[%s][%s].%s = %s; // 796	\n" (vexpr typhash id) (vexpr typhash sel) (vexpr typhash sel') (vexpr typhash id') (vexpr typhash expr)
      | Blocking (FopAsgnArrayField7 (id, sel, sel', id', expr)) ->
          fprintf fd "        %s[%s][%s].%s = %s; // 798	\n" (vexpr typhash id) (vexpr typhash sel) (vexpr typhash sel') (vexpr typhash id') (vexpr typhash expr)
      | ForEach (ix, lst) ->
          fprintf fd "            foreach %s; // 800	\n" (vexpr typhash ix);
          List.iter (stmt_clause fd typhash) lst;
      | ForLoop (Asgn1 (Id ix, strt) :: [], Less (Id ix', limit), SideEffect (Id xi'', Atom "++"), seq) ->
          fprintf fd "            for %s; // 800	\n" ix;
          stmt_clause fd typhash seq;
      | ForLoop (Asgn1 (Id ix, strt) :: [], LtEq (Id ix', limit), Asgn1 (Id ix'', Add (Id ix''', Number (_, _, 1, _))), seq) ->
          fprintf fd "            for %s; // 803	\n" ix;
          stmt_clause fd typhash seq;
      | ForLoop ([Typ7 (ix, Atom ("int"|"unsigned_int"))], Less (Id ix', limit), Asgn1 (Id ix'', Add (Id ix''', Number (_, _, inc, _))), seq) ->
          fprintf fd "            for %s; // 806	\n" ix;
          stmt_clause fd typhash seq;
      | ForLoop ([Typ7 (ix, Atom ("int"|"unsigned_int"))], Less (Id ix', limit), SideEffect (Id xi'', Atom "++"), seq) ->
          fprintf fd "            for %s; // 809	\n" ix;
          stmt_clause fd typhash seq;
      | ForLoop ([Typ7 (ix, Atom ("int"|"unsigned_int"))], GtEq (Id ix', limit), SideEffect (Id xi'', Atom "--"), seq) ->
          fprintf fd "            for %s; // 812	\n" ix;
          stmt_clause fd typhash seq;
      | ForLoop ([Typ9 (ix, AnyRange(hi,lo) :: [], Atom ("logic"))], Less (Id ix', limit), SideEffect (Id xi'', Atom "++"), seq) ->
          fprintf fd "            for %s; // 815	\n" ix;
          stmt_clause fd typhash seq;
      | Equate (id,expr) -> fprintf fd "            %s <= %s; // 817	\n" (vexpr typhash id) (vexpr typhash expr);
      | EquateSlice (id,hi,lo,expr) -> fprintf fd "            %s[%s : %s] <= %s; // 818	\n" (vexpr typhash id) (vexpr typhash hi) (vexpr typhash lo) (vexpr typhash expr);
      | EquateSelect (id,ix,expr) -> fprintf fd "            %s[%s] <= %s; // 819	\n" (vexpr typhash id) (vexpr typhash ix) (vexpr typhash expr);
      | EquateSelect2 (id,ix,expr) -> fprintf fd "            %s[%s] <= %s; // 820	\n" (vexpr typhash id) (vexpr typhash ix) (vexpr typhash expr);
      | EquateArrayField (id,id',ix,ix',expr) -> fprintf fd "            %s.%s[%s][%s] <= %s; // 821	\n" (vexpr typhash id) (vexpr typhash id') (vexpr typhash ix) (vexpr typhash ix') (vexpr typhash expr);
      | CaseStart (CaseStart1 (sel), lst) ->
        fprintf fd "case (%s)\n" (vexpr typhash sel);
        List.iter (case_clause fd typhash) lst;
        fprintf fd "endcase; // 825	\n";
      | CaseStartInside (sel, lst) ->
        fprintf fd "case (%s) inside\n" (vexpr typhash sel);
        List.iter (case_clause fd typhash) lst;
        fprintf fd "endcase; // 829	\n";
      | CaseStartUniq (CaseStart1 (sel), lst) ->
        fprintf fd "unique case (%s)\n" (vexpr typhash sel);
        List.iter (case_clause fd typhash) lst;
        fprintf fd "endcase; // 833	\n";
      | CaseStartUniqInside (sel, lst) ->
        fprintf fd "unique case (%s) inside\n" (vexpr typhash sel);
        List.iter (case_clause fd typhash) lst;
        fprintf fd "endcase; // 841	\n";
      | Blocking (SideEffect (id, Atom "++")) -> fprintf fd "            %s <= %s+1; // 842	\n" (vexpr typhash id) (vexpr typhash id)
      | Blocking (SideEffect (id, Atom "--")) -> fprintf fd "            %s <= %s-1; // 843	\n" (vexpr typhash id) (vexpr typhash id)
      | DeclData _ -> ()
      | Blocking (BreakSemi) -> () (* placeholder *)
      | BreakSemi -> () (* placeholder *)
      | Atom ";" -> ()
      | TaskRef (tid, lst) -> () (* placeholder *)
      | TFBody (decls, lst) -> List.iter (stmt_clause fd typhash) lst
      | SysTaskCall (tid, args) -> fprintf fd "%s(...);\n" tid
      | EquateField (id, field, expr) ->  fprintf fd "            %s.%s <= %s; // 851	\n" (vexpr typhash id) (vexpr typhash field) (vexpr typhash expr)
      | DeclInt2 _ -> ()
      | DeclLogic2 _ -> ()
      | Return expr -> fprintf fd "return %s;\n" (vexpr typhash expr)
      | CaseStmt _ as x -> case_clause fd typhash x
      | oth -> dump_unhand := Some oth; failwith "stmt_clause"

and case_clause fd typhash = function
        | CaseStmt (lbls, body) ->
            fprintf fd "\t";
            if lbls <> [] then List.iter (function
               | Id lbl -> fprintf fd "%s" lbl
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

and iff_dump_template fd typhash = function
    | If1(condition, if_lst) -> fprintf fd "        if (%s) begin\n" (vexpr typhash condition);
      stmt_clause fd typhash if_lst;
      fprintf fd "end\n";
    | oth -> dump_unhand := Some oth; failwith "iff_dump_template"

let dump_enum fd typhash front lft rght elst back =
        fprintf fd "%senum logic [%s:%s] {\n\t%s\n} %s // 714\n" front (vexpr typhash lft) (vexpr typhash rght) (String.concat ",\n\t" (List.map (function
        | Id e -> e
        | EnumInit (e, expr) ->
	    let s = vexpr typhash expr in
	    let s' = sprintf "%s = %s" e s in
	    s'
	| oth -> dump_unhand := Some oth; failwith "TypEnum6") elst)) back

let rec decl_dump_template fd (typhash:(string,vtyp)Hashtbl.t) modules cnt = function
    | NetDecl (Atom "wire" :: [], wire_lst) -> List.iter (function
          | Id nam ->
	      fprintf fd "logic %s; // 610	\n" nam
	  | DeclAsgn (nam, AnyRange (hi, lo) :: []) ->
              fprintf fd "logic [%s:%s] %s; // 612	\n" (vexpr typhash hi) (vexpr typhash lo) (vexpr typhash nam)
          | InitSig (nam, expr) -> (function
	      | Id id -> fprintf fd "logic %s = %s; // 614	\n" (vexpr typhash nam) (vexpr typhash expr)
              | ExprOKL lbls -> fprintf fd "logic %s : %s; // 615	\n" (vexpr typhash nam) (String.concat "; " (List.map (vexpr typhash) lbls))
	      | SysFuncCall ("$random", [Deflt]) -> fprintf fd "    signal %s : $random; // 616	\n" (vexpr typhash nam)
              | Query _ as x -> fprintf fd "logic %s : %s; // 617	\n" (vexpr typhash nam) (vexpr typhash x)

	      | oth -> dump_unhand := Some oth; failwith "initsig") expr
	  | oth -> dump_unhand := Some oth; failwith "NetDecl'") wire_lst;
    | Itmlst (id_lst) -> List.iter (function
	  | Id nam -> fprintf fd "logic %s; // 622	\n" nam
	  | oth -> dump_unhand := Some oth; failwith "DeclLogic647"
        ) id_lst;
    | DeclLogic (reg_lst) -> List.iter (function
	  | Id nam ->  fprintf fd "logic %s; // 626	\n" nam
          | DeclAsgn (nam, AnyRange (hi, lo) :: []) ->
              fprintf fd "logic [%s : %s] %s; // 628\n" (vexpr typhash hi) (vexpr typhash lo) (vexpr typhash nam)
          | VarDeclAsgn (nam, expr) ->
              fprintf fd "logic %s = %s\n" (vexpr typhash nam) (vexpr typhash expr)
	  | oth -> dump_unhand := Some oth; failwith "DeclLogic651"
        ) reg_lst;
    | DeclLogic2 (wire_lst, AnyRange (hi, lo) :: []) -> List.iter (function
	  | Id nam ->
	  fprintf fd "logic [%s : %s] %s; // 635	\n" (vexpr typhash hi) (vexpr typhash lo) (nam)
	  | DeclAsgn (nam, AnyRange (hi, lo) :: []) ->
              fprintf fd "logic [%s : %s] %s ; // 637\n" (vexpr typhash hi) (vexpr typhash lo) (vexpr typhash nam)
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
    | DeclInt2 id_lst -> List.iter (function
	| Id itm -> fprintf fd "logic %s; // 672	\n" itm
        | VarDeclAsgn (id, expr) -> fprintf fd "logic %s = %s\n" (vexpr typhash id) (vexpr typhash expr)
        | oth -> dump_unhand := Some oth; failwith "DeclInt2") id_lst
    | InstDecl (typ, params, lst) -> List.iter (function
        | (InstNameParen1 _ | InstNameParen2 _) -> ()
        | Id _ as id -> fprintf fd "logic %s; // 677	\n" (vexpr typhash id)
        | oth -> dump_unhand := Some oth; failwith "InstDecl586") lst;
    | Typ2 ("bool_t", [], [Id "FALSE"; Id "TRUE"]) -> ()
    | Typ2 (nam, _, id :: []) ->
        let s = vexpr typhash id in  fprintf fd "logic %s : %s; // 681	\n" s nam
    | Typ2 (nam, _, id_lst) ->
        List.iter (function
	     | Id _ as itm -> let s = vexpr typhash itm in  fprintf fd "logic %s : %s; // 684	\n" s nam
             | DeclAsgn (id, AnyRange(lft,rght) :: AnyRange(lft',rght') :: []) ->
                fprintf fd "logic %s; // 686	\n" (vexpr typhash id)
             | oth -> dump_unhand := Some oth; failwith "Typ2") id_lst;
    | Typ3 (nam, id_lst) -> List.iter (fun _ -> ()) id_lst
    | Typ4 (nam, pkg, rng, id_lst) -> List.iter (fun _ -> ()) id_lst
    | Typ9 (orig_id, id_lst, Typ5 (TypEnum3 (AnyRange(lft,rght) :: []), elst)) -> List.iter (function
          | Id id -> fprintf fd "%s %s;\n" orig_id id
          | oth -> dump_unhand := Some oth; failwith "enum range") id_lst
    | Typ9 (orig_id, id_lst, Typ5 (Deflt, elst)) -> List.iter (function
          | Id id -> fprintf fd "%s %s;\n" orig_id id
            | oth -> dump_unhand := Some oth; failwith "enum range") id_lst
    | Typ5 (SUDecl (Atom "packed", lst), inst_lst) ->
        fprintf fd "typedef struct packed { // 703	\n";
        List.iter (dump_struct typhash) lst;
        fprintf fd "} %s;\n" (String.concat ", " (List.map (vexpr typhash) inst_lst));
    | Typ6 (SUDecl (Atom "packed", lst)) ->
        fprintf fd "typedef struct packed { // 703	\n";
        List.iter (dump_struct typhash) lst;
        fprintf fd "};\n"
    | Typ7 (nam, Typ5 (Atom "logic", AnyRange(lft,rght) :: AnyRange(lft',rght') :: AnyRange(lft'',rght'') :: [])) ->
        fprintf fd "logic %s; // 692	\n" nam
    | Typ7 (nam, Typ5 (Atom "logic", AnyRange(lft,rght) :: AnyRange(lft',rght') :: [])) ->
        fprintf fd "logic %s; // 694	\n" nam
    | Typ7 (nam, Typ5 (Atom "logic", AnyRange(lft,rght) :: [])) ->
        fprintf fd "typedef logic [%s:%s] %s; // 696	\n" (vexpr typhash lft) (vexpr typhash rght) nam
    | Typ7 (nam, Typ8 (SUDecl (Atom "packed", lst), Deflt)) ->
        fprintf fd "typedef struct packed { // 714	\n";
        List.iter (dump_struct typhash) lst;
        fprintf fd "} %s;\n" nam
    | Typ7 (id_t, Typ8 (Union (Atom "packed", lst), Deflt)) ->
        fprintf fd "typedef union packed { // 718	\n";
        List.iter (dump_struct typhash) lst;
        fprintf fd "} %s;\n" id_t
    | Typ7 (id_t, Typ8 (Itmlst lst, Deflt)) ->
        fprintf fd "typedef struct { // 722	\n";
        List.iter (dump_struct typhash) lst;
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
    | TypEnum6 (nam, TypEnum3 (AnyRange(lft,rght) :: []), id_lst) -> dump_enum fd typhash "typedef " lft rght id_lst (nam^"; ")
    | TypEnum6 (nam, TypEnum5 (Atom "logic"), id_lst) -> 
        fprintf fd "typedef enum logic {\n\t%s\n} %s; // 618\n" (String.concat ",\n\t" (List.map (function
        | Id e -> e
        | EnumInit (e, expr) ->
	    let s = vexpr typhash expr in
	    let s' = sprintf "%s = %s" e s in
	    s'
	| oth -> dump_unhand := Some oth; failwith "TypEnum6") id_lst)) nam
    | TypEnum6 (nam, Deflt, id_lst) -> 
        fprintf fd "typedef enum logic {\n\t%s\n} %s; // 626\n" (String.concat ",\n\t" (List.map (function
        | Id e -> e
        | EnumInit (e, expr) ->
	    let s = vexpr typhash expr in
	    let s' = sprintf "%s = %s" e s in
	    s'
	| oth -> dump_unhand := Some oth; failwith "TypEnum6") id_lst)) nam
    | TypEnum6 (nam, Typ8 (Atom "int", Atom "unsigned"), id_lst) -> 
        fprintf fd "typedef enum unsigned int {\n\t%s\n} %s; // 634\n" (String.concat ",\n\t" (List.map (function
        | Id e -> e
        | EnumInit (e, expr) ->
	    let s = vexpr typhash expr in
	    let s' = sprintf "%s = %s" e s in
	    s'
	| oth -> dump_unhand := Some oth; failwith "TypEnum6") id_lst)) nam
    | ParamDecl (Atom "localparam", [ParamAsgn1 (nam, expr)]) -> fprintf fd "    localparam %s = %s; // 740	\n" nam (vexpr typhash expr)
    | Typ6 (Atom "packed") -> () (* placeholder *)
    | Typ10 (id_t, AnyRange (lft, rght) :: [], id_t') -> () (* placeholder *)
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
    | ParamDecl (LocalParamTyp (Typ8 (Atom ("int"|"integer"|"longint" as kind), Atom kind')), [ParamAsgn1 (nam , expr)]) ->
        fprintf fd "localparam %s %s %s = %s; // 784\n" kind' kind nam (vexpr typhash expr)
    | FunDecl (fn, typ, FunGuts (ports, lst)) ->
        fprintf fd "// function %s ...\n" fn;
        List.iter (stmt_clause fd typhash) lst;
        fprintf fd "    endfunction\n";
    | AutoFunDecl (fn, typ, FunGuts (ports, lst)) ->
        fprintf fd "// function automatic %s ...\n" fn;
        List.iter (stmt_clause fd typhash) lst;
        fprintf fd "    endfunction\n";
    | PkgImport (Itmlst lst) -> List.iter (decl_dump_template fd typhash modules cnt) lst
    | PkgImportItm (pkg, Atom "*") -> ()
    | DeclData (Typ5 (Atom "logic", AnyRange (lft, rght) :: AnyRange (lft', rght') :: []), Deflt, VarDeclAsgn (mem, ExprOKL lst) :: []) -> () (* placeholder *)
    | AssertProperty -> ()
    | Port (dir, nam, [AnyRange (hi, lo)], Deflt) -> ()
    | Port (dir, nam, [AnyRange (hi, lo)], Atom "signed") -> ()
    | oth -> dump_unhand := Some oth; failwith "decl_dump_template"

let rec sent_dump_template fd typhash (clk:rw) = function
    | BeginBlock lst -> List.iter (sent_dump_template fd typhash clk) lst
    | Seq (lbl, lst) -> List.iter (sent_dump_template fd typhash clk) lst
    | If2 ((Equals (Id rst, lev)|Expression(Equals (Id rst, lev))), if_lst, else_lst) ->
      let clk = (vexpr typhash clk) in
  fprintf fd "        if (%s == %s) begin\n" rst (vexpr typhash lev);
    stmt_clause fd typhash if_lst;
  fprintf fd "        end else begin\n";
    stmt_clause fd typhash else_lst;
  fprintf fd "        end\n";
    | If2 (Id rst, if_lst, else_lst) ->
      let clk = (vexpr typhash clk) in
  fprintf fd "        if (%s == %s) begin\n" rst (vexpr typhash (Number (2, 1, 1, "1")));
    stmt_clause fd typhash if_lst;
  fprintf fd "        end else begin\n";
    stmt_clause fd typhash else_lst;
  fprintf fd "        end\n";
    | If2 ((Pling (Id rst)|Tilde (Id rst)), if_lst, else_lst) ->
      let clk = (vexpr typhash clk) in
  fprintf fd "        if (%s == %s) begin\n" rst (vexpr typhash (Number (2, 1, 0, "1")));
    stmt_clause fd typhash if_lst;
  fprintf fd "        end else begin\n";
    stmt_clause fd typhash else_lst;
  fprintf fd "        end\n";
    | If1 (cond, if_lst) ->
  fprintf fd "        if (%s) begin\n" (vexpr typhash cond);
    stmt_clause fd typhash if_lst;
  fprintf fd "        end\n";
    | If1 (cond, if_lst) ->
  fprintf fd "        if (%s) begin\n" (vexpr typhash cond);
    stmt_clause fd typhash if_lst;
    | If2 (cond, if_lst, else_lst) ->
  fprintf fd "        if (%s) begin\n" (vexpr typhash cond);
    stmt_clause fd typhash if_lst;
  fprintf fd "        end else begin\n";
    stmt_clause fd typhash else_lst;
  fprintf fd "        end\n";
    | Equate (lhs, rhs) -> fprintf fd "        %s <= %s; // 922	\n" (vexpr typhash lhs) (vexpr typhash rhs);
    | Blocking (FopAsgn (lhs, rhs)) -> fprintf fd "        %s <= %s; // 922	\n" (vexpr typhash lhs) (vexpr typhash rhs);
    | DeclData _ -> () 
    | (CaseStart _ | EquateSelect _) as x -> stmt_clause fd typhash x
    | oth -> dump_unhand := Some oth; failwith "sent_dump_template"

let dump_conn typhash = function
  | (Id _ | CellPinItem2 _ | CellPinItemImplied _ | CellPinItemNC _) as x -> vexpr typhash x
  | oth -> dump_unhand := Some oth; failwith "inst_arg"

let instance_dump_template fd typhash (typ:rw) params inst pinlst =
  match typ with 
    | Id typ ->
    output_string fd (typ ^ " " ^
                      inst^" (\n\t"^
                      String.concat ",\n\t" (List.map (dump_conn typhash) pinlst)^");\n")
    | oth -> failwith "instance type"

let rec proc_dump_template fd typhash cnt = function
    | DeclReg _ -> ()
    | DeclLogic _ -> ()
    | AlwaysFF (At (EventOr (Pos clk :: _ as dep_lst)), sent_lst) ->
  fprintf fd "    // clocked process %d description goes here\n" !cnt;
  incr cnt;
  dump_deps fd "always_ff" dep_lst;
  fprintf fd "    begin\n";
  sent_dump_template fd typhash clk sent_lst;       
  fprintf fd "    end; // 947	\n";
  fprintf fd "\n";
    | AlwaysLegacy (At (EventOr (Pos clk :: _ as dep_lst)), sent_lst) ->
  fprintf fd "    // clocked process %d description goes here\n" !cnt;
  incr cnt;
  dump_deps fd "always" dep_lst;
  fprintf fd "    begin\n";
  sent_dump_template fd typhash clk sent_lst;       
  fprintf fd "    end; // 947	\n";
  fprintf fd "\n";
    | AlwaysLegacy (At (EventOr dep_lst), sent_lst) ->
  fprintf fd "    // combinational process %d description goes here\n" !cnt;
  incr cnt;
  dump_deps_comb fd typhash "always" dep_lst;
  fprintf fd "    begin\n";
  stmt_clause fd typhash sent_lst;
  fprintf fd "    end; // 979	\n";
  fprintf fd "\n";
    | AlwaysLegacy (AtStar, sent_lst) ->
  fprintf fd "    // combinational process %d description goes here\n" !cnt;
  incr cnt;
  dump_deps_comb fd typhash "always" [AtStar];
  fprintf fd "    begin\n";
  stmt_clause fd typhash sent_lst;
  fprintf fd "    end; // 979	\n";
  fprintf fd "\n";
    | AlwaysComb2 ( (*DepLst dep_lst,*) sent_lst) -> let dep_lst = [] in
  fprintf fd "    // combinational process %d description goes here\n" !cnt;
  fprintf fd "    COMB%d: process (%s)\n" !cnt (String.concat ", " dep_lst);
  incr cnt;
  fprintf fd "    begin\n";
  stmt_clause fd typhash sent_lst;
  fprintf fd "    end; // 987	\n";
  fprintf fd "\n";
    | AlwaysLatch ( sent_lst ) ->
  fprintf fd "    // combinational latch process %d description goes here\n" !cnt;
  fprintf fd "    LATCH%d: process ()\n" !cnt;
  incr cnt;
  fprintf fd "    begin\n";
  stmt_clause fd typhash sent_lst;
  fprintf fd "    end; // 995	\n";
  fprintf fd "\n";
  (* elaboration case *)
   | CaseStart (Id id, (CaseItm (BeginBlock [] :: Unknown ("$error",_) :: Deflt :: []) :: [])) -> fprintf fd "// elaboration case %s\n" id;
    | ContAsgn lst -> List.iter (function
      | Asgn1 (lhs, expr) -> asgn fd typhash expr lhs
      | oth -> dump_unhand := Some oth; failwith "assign_dump_template") lst
    | Iff _ -> ()
    | InstDecl (typ, params, lst) -> List.iter (function
        | InstNameParen1 (inst, pins) -> instance_dump_template fd typhash typ (match params with Itmlst lst :: _ -> lst | _ -> []) inst pins
        | InstNameParen2 (inst, InstRange(lft,rght) :: []) -> instance_dump_template fd typhash typ (match params with Itmlst lst :: _ -> lst | _ -> []) inst []
        | Id id -> fprintf fd "    // %s\n" id
        | oth -> dump_unhand := Some oth; failwith "InstDecl829") lst;
    | TypEnum4 _ -> ()
    | TypEnum6 _ -> ()
    | Typ2 (typ, _, typ_lst) -> ()
    | Typ3 _ -> ()
    | Typ4 _ -> ()
    | Typ5 _ -> ()
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
    | oth -> dump_unhand := Some oth; failwith "proc_dump_template"

let dump_vdir = function
  | In -> "input logic"
  | Out -> "output logic"
  | Inout -> "inout wire"
  | Deflt -> "inout wire"
  | oth -> dump_unhand := Some oth; failwith "dump_vdir"

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

let dump_port typhash = function
    | Port ((In|Out|Inout|Deflt) as dir, nam, [], sgn) -> dump_port_single typhash nam dir (signcnv (sgn, Deflt))
    | Port (PortDir ((In|Out|Inout|Deflt) as dir, Atom ("wire"|"reg"|"logic")), nam, [], sgn) -> dump_port_single typhash nam dir (signcnv (sgn, Deflt))
    | Port ((In|Out|Inout) as dir, nam, AnyRange (hi, lo) :: [], sgn) -> dump_array_port typhash hi lo dir (Id nam)
    | Port (PortDir ((In|Out|Inout|Deflt) as dir, Atom ("wire"|"reg"|"logic")), nam, AnyRange (hi, lo) :: [], sgn) -> dump_array_port typhash hi lo dir (Id nam)
    | Port ((In|Out|Inout) as dir, nam, AnyRange (hi, lo) :: AnyRange(hi', lo') :: [], sgn) -> dump_array_port typhash hi lo dir (Id nam)
    | Port ((In|Out|Inout) as dir, nam, AnyRange (hi, lo) :: AnyRange(hi', lo') :: AnyRange(hi'', lo'') :: [], sgn) -> dump_array_port typhash hi lo dir (Id nam)
    | Port ((In|Out|Inout) as dir, nam, Typ6 (Atom primtyp) :: [], sgn) -> dump_port_single typhash nam dir Unsigned
    | Port (Deflt, nam, Typ2 (typ_t, PackageRef (pkg, Atom "::") :: [], []) :: AnyRange (hi, lo) :: [], sgn) ->
        dump_array_port typhash hi lo Inout (Id nam)
    | Port ((In|Out|Inout) as dir, nam, Typ2 (typ_t, [], []) :: AnyRange (hi, lo) :: [], sgn) ->
        dump_array_port typhash hi lo dir (Id nam)
    | Port ((In|Out|Inout) as dir, nam, Typ2 (typ_t, [], []) :: [], sgn) ->
        dump_port_single typhash nam dir (signcnv (sgn, Deflt))
    | Port ((In|Out|Inout) as dir, nam, Typ2 (typ_e, (PackageRef (pkg, Atom "::")) :: [], []) :: [], sgn) ->
        dump_port_single typhash nam dir (signcnv (sgn, Deflt))
    | Port ((In|Out|Inout) as dir, nam, Typ2 (typ_e, (PackageRef (pkg, Atom "::")) :: [], []) :: AnyRange (hi, lo) :: [], sgn) ->
        dump_array_port typhash hi lo dir (Id nam)
    | Dot3 (bus, dir, member) -> ""
    | DotBus (bus, dir, member, AnyRange(hi,lo) :: []) ->
        dump_array_port typhash hi lo Inout bus
    | Port ((In|Out|Inout) as dir, nam, [Typ5 (Typ8 (SUDecl (Atom "packed", memblst), Deflt), [])], Deflt) -> ""
    | Port ((In|Out|Inout) as dir, id_i, [Typ12 ([], Typ8 (SUDecl (Atom "packed", memblst), Deflt), [])], Deflt) ->
        ("struct port "^id_i)
    | Port ((In|Out|Inout) as dir, nam, [Typ8 (SUDecl (Atom "packed", memblst), Deflt)], Deflt) ->
        let sulst = List.flatten (List.map (dump_struct typhash) memblst) in
        let typ' = Vsu (Id nam, sulst) in
        List.iter (unpack_typ typhash nam) sulst;
        ""
    | oth -> dump_unhand := Some oth; failwith "component"

let dump_template fd modules = function
  | Modul(nam, parm_lst, port_lst, body_lst) as x -> let cnt = ref 1 in
  fprintf fd "//\n";
  fprintf fd "// This converter does not currently preserve comments and license information\n";
  fprintf fd "//\n";
  fprintf fd "\n";
  let bufh', typhash, ports' = Source_text_simplify.module_header [] x in
  fprintf fd "module %s (\n\t%s);\n" nam (String.concat ",\n\t" (List.map (dump_port typhash) port_lst));
(*
  parm_dump_template fd typhash parm_lst;
*)
  fprintf fd "    // Signals\n";
  let typlst, othlst = List.partition (function TypEnum6 _ -> true | _ -> false) body_lst in
  List.iter (decl_dump_template fd typhash modules cnt) (typlst);
  let components, othlst = List.partition (function InstDecl _ -> true | _ -> false) othlst in
  List.iter (decl_dump_template fd typhash modules cnt) (List.sort compare components);
  List.iter (decl_dump_template fd typhash modules cnt) othlst;
  List.iter (proc_dump_template fd typhash cnt) body_lst;
  fprintf fd "\n";
  fprintf fd "endmodule // 1055	\n";
  fprintf fd "\n";
  fprintf fd "\n";
  | PackageBody (pkg, body_lst) -> let cnt = ref 1 in
  fprintf fd "//\n";
  fprintf fd "// This converter does not currently preserve comments and license information\n";
  fprintf fd "//\n";
  fprintf fd "\n";
  fprintf fd "package %s; // 1065	\n" pkg;
  List.iter (decl_dump_template fd (Hashtbl.create 1) modules cnt) body_lst;
  fprintf fd "endpackage\n";
  | oth -> dump_unhand := Some oth; failwith "This template only handles modules/packages"
