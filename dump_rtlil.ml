open Source_text_rewrite_types
open Input_rewrite_types
open Source_text_lex
open Source_text
open Printf

type mem_opts = {off:int list; siz:int list; wid:int; tot:int}

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
  | Vmem of mem_opts

type bufh = {c:ilang list ref;
    i:ilang list ref;
    l:ilang list ref;
    w:ilang list ref}

let unhand = ref None
let update typhash id expr =
  Hashtbl.replace typhash id expr

let is_mem typhash id = match Hashtbl.find_opt typhash id with Some (Vmem _) -> true | _ -> false
let mem_opt typhash id = match Hashtbl.find_opt typhash id with Some (Vmem opt) -> opt | _ -> failwith "mem_opt"

let rec obin w n = 
  (if w > 1 then obin (w-1) (n lsr 1) else "")^string_of_int (n land 1)

let coth = ref None

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
| And (lhs, rhs) -> (ceval typhash lhs) land (ceval typhash rhs)
| Or (lhs, rhs) -> (ceval typhash lhs) lor (ceval typhash rhs)
| Or2 (lhs, rhs) -> (if ceval typhash lhs <> 0 then 1 else 0) lor (if ceval typhash rhs <> 0 then 1 else 0)
| Equals (lhs, rhs) -> if ceval typhash lhs = ceval typhash rhs then 1 else 0
| Greater (lhs, rhs) -> if ceval typhash lhs > ceval typhash rhs then 1 else 0
| Query (lhs, rhs, rhs') -> if ceval typhash lhs <> 0 then ceval typhash rhs else  ceval typhash rhs'
| StarStar (lhs, rhs) -> int_of_float(float_of_int (ceval typhash lhs) ** float_of_int(ceval typhash rhs))
| Sys ("$clog2", x) -> clog2 (ceval typhash x)
| Expression x -> ceval typhash x
| PackageBody (pkg, [id]) -> ceval typhash id (* placeholder *)
| Sys ("$bits", Typ1 id_t) -> csiz' typhash (match Hashtbl.find_opt typhash id_t with Some x -> x | None -> print_endline ("Not found: "^id_t); Std_logic)
| Sys ("$size", Dot1 _) -> 1 (* placeholder *)
| ExprOKL [] -> 0
| ExprOKL (hd::tl) -> ((ceval typhash hd) lsl (width typhash (ExprOKL tl))) + (ceval typhash (ExprOKL tl))
| oth -> unhand := Some oth; failwith "ceval"

and csiz' typhash = function
| Vint n -> 32
| Std_logic -> 1
| Std_logic_vector(hi,lo) -> ceval typhash hi - ceval typhash lo + 1
| Vtyp s -> print_endline ("Sizeof type "^s^" evaluated to zero"); 0
| Venum s -> 8
| Vemember(s, _, _) -> csiz' typhash (match Hashtbl.find_opt typhash s with Some x -> x | None -> print_endline ("Not found: "^s); Std_logic)
| Vmem {off;siz;wid;tot} -> tot * wid
| oth -> coth := Some oth; failwith "csiz'"

and width typhash = function
| Intgr n -> 32
| Number (_,w,_,_) -> w
| Id s -> 
  let siz = match Hashtbl.find_opt typhash s with
    | Some (Vmem {off;siz;wid}) -> wid
    | Some (MaybePort n) -> failwith ("width of "^s^", port type MaybePort "^string_of_int n^" evaluated")
    | Some x -> csiz' typhash x
    | None -> print_endline ("Not found: "^s); 1 in
  siz
| IdArrayed2 (IdArrayed2 (Id mem, Id addr), Id abit) -> 1
| IdArrayed2 (Id s, _) -> 1
(* csiz' typhash (match Hashtbl.find_opt typhash s with Some x -> x | None -> print_endline ("Not found: "^s); Std_logic) *)
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
| IdArrayedColon(_, hi, lo) -> ceval typhash hi - ceval typhash lo + 1
| GenBlock [] -> 0
| GenBlock (hd::tl) -> width typhash hd + width typhash (GenBlock tl)
| Concat [] -> 0
| Concat (hd::tl) -> width typhash hd + width typhash (Concat tl)
| RedOr _ -> 1
| RedAnd _ -> 1
| TildeAnd _ -> 1
| IdArrayedPlusColon (_, _, (Intgr w|Number(_,_,w,_))) -> w
| IdArrayed1 (Id id, addr, abit) -> 1
| IdArrayed2 (IdArrayed2 (Id id, addr), abot) -> 1
| oth -> unhand := Some oth; failwith "width"

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

let rec simplify = function
| Number(_, 32, n, _) -> Intgr n
| Add (Intgr lhs, Intgr rhs) -> Intgr (lhs + rhs)
| Sub (Intgr lhs, Intgr rhs) -> Intgr (lhs - rhs)
| Mult (Intgr lhs, Intgr rhs) -> Intgr (lhs * rhs)
| Mult (Intgr 0, rhs) -> Intgr 0
| Mult (Intgr 1, rhs) -> rhs
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

let rec recurs t (attr: Source_text_rewrite.attr) = function
  | If2(cond, if_clause, else_clause) ->
      CaseStart (CaseStart1 (cond),
       (CaseStmt ([Number(2,1,1,"")],
         recurs t attr if_clause :: []) ::
        CaseStmt ([Number(2,1,0,"")],
         recurs t attr else_clause :: []) ::
        []))
  | If1(cond, if_clause) ->
      CaseStart (CaseStart1 (recurs t attr cond),
       (CaseStmt ([Number(2,1,1,"")],
         recurs t attr if_clause :: []) ::
         []))
  | EquateSelect(lhs, sel, expr) ->
      let wid = width t (Id lhs) in
      if wid < 1 then failwith ("wid("^lhs^")="^string_of_int wid);
      let foreach = List.init wid (fun ix -> let sel = Number(2,clog2 wid,ix,"") in CaseStmt ([], EquateSelect(lhs, recurs t attr sel, recurs t attr expr) :: [])) in
      CaseStart (CaseStart1 (recurs t attr sel), foreach)
  | Id id -> (match Hashtbl.find_opt attr.subst id with None -> Id id | Some exp -> exp)
  | ForLoop ([Asgn1 (Id ix, strt)], Less (Id ix', stop), Asgn1 (Id ix'', Add (Id ix''', inc)), body) ->
	recurs t attr (iter t attr ix (ceval t strt) (ceval t stop) (ceval t inc) body)
  | BeginBlock lst -> Seq("", List.map (recurs t attr) lst)
  | Port (Deflt, nam, [], []) as x -> (match Hashtbl.find_opt attr.subst "" with Some (DeclData(dir, rng)) -> Port (dir, nam, rng, []) | _ -> x)
  | Port (dir, _, rng, _) as x -> Hashtbl.replace attr.subst "" (DeclData(dir, rng)); x
  | oth -> rfn t attr (simplify oth)

and rfn t attr x = Source_text_rewrite.descend' {attr with fn=recurs t} x

and iter t attr ix strt stop inc stmts = 
    let loopvar = ref strt in
    let block = ref [] in
    while (!loopvar <= stop) do
      begin
        Hashtbl.replace attr.Source_text_rewrite.subst ix (Intgr !loopvar);
        if verbose then print_endline (string_of_int !loopvar);
	let repl = recurs t attr stmts in
	block := repl :: !block;
	loopvar := !loopvar + inc;
      end
    done;
    Seq ("", List.rev !block)

let sub' typhash = List.map (recurs typhash {fn=recurs typhash; subst=Hashtbl.create 255})

let rec vexpr typhash = function
| oth -> unhand := Some oth; failwith "vexpr"

and cexpr typhash = function
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

let id_ix = ref 0

let newnam () = 
  incr id_ix;
  "$Id$"^string_of_int !id_ix

let addwire bufh typhash (options, nam) =
  print_endline ("addwire: "^nam);
  (match Hashtbl.find_opt typhash nam with Some (MaybePort _) -> update typhash nam Std_logic | _ -> ());
  if List.filter (function Wire_stmt (_, id) -> id=nam | _ -> false) !(bufh.w) <> [] then
    print_endline ("Warning: "^nam^" is already defined, could be obsolete syntax")
  else
    bufh.w := Wire_stmt(options, nam) :: !(bufh.w)

let rec memsiz typhash = function
| [] -> []
| (first,last) :: tl -> let off' = ceval typhash first in (off', ceval typhash last - off' + 1) :: memsiz typhash tl

let addmem bufh typhash first_last_lst wid' mem =
  if List.filter (function Memory_stmt39 (_, id) -> id=mem | _ -> false) !(bufh.w) <> [] then
    print_endline ("Warning: "^mem^" is already defined, could be obsolete syntax")
  else
    begin
    let (off', siz') = List.split (memsiz typhash first_last_lst) in
    let rec tot = function [] -> 1 | hd::tl -> hd * tot tl in
    let rec off = function [] -> 0 | hd::tl -> hd + tot tl in
    let tot' = tot siz' in
    let options = Memory_optionsoffset (off off') :: Memory_optionssize (tot') :: Memory_optionswidth wid' :: [] in
    update typhash mem (Vmem {off=off';siz=siz';wid=wid';tot=tot'});
    bufh.w := Memory_stmt39 (options, mem) :: !(bufh.w)
    end

let vsel' n = function Id lhs -> [Sigspec90 (lhs, n)] | oth -> unhand := Some oth; failwith "vsel'"

let rec tran' = function
   | Id id -> TokID id :: []
   | Number (b,w,n,_) -> TokVal (sprintf "%d'%s" w (obin w n)) :: []
   | Atom "default" -> []
   | Concat lst -> List.flatten (List.map tran' lst)
   | ExprOKL lst -> List.flatten (List.map tran' lst)
   | GenBlock lst -> Sigspec92 (List.flatten (List.map tran' lst)) :: []
   | IdArrayed2 (Id arr, Number (b, w, n, _)) -> vsel' n (Id arr)
   | IdArrayedColon (Id conn, (Intgr hi | Number(_,_,hi,_)), (Intgr lo | Number(_,_,lo,_))) -> Sigspecrange(conn,hi,lo) :: []
   | IdArrayedPlusColon (Id conn, (Intgr lo | Number(_,_,lo,_)), (Intgr wid | Number(_,_,wid,_))) -> Sigspecrange(conn,lo+wid-1,lo) :: []
   | oth -> unhand := Some oth; failwith "tran'"

let newid bufh typhash wid =
  let nam = newnam() in
  let rslt = Id nam in
  update typhash nam (Std_logic_vector(Intgr (wid-1),Intgr 0));
  addwire bufh typhash ([Wire_optionswidth wid], nam);
  rslt


let rec parm_map typhash = function  
  | Param (nam, (Intgr n | Number (_, _, n, _)), []) ->
      update typhash nam (Vint n);
      TokParam([TokID ("\\"^nam)], [TokInt n])
  | Param (nam, (Intgr n | Number (_, _, n, _)), AnyRange (left, rght) :: []) ->
      update typhash nam (Vint n);
      TokParam([TokID ("\\"^nam)], [TokInt n])
  | CellParamItem2 (nam, (Intgr n | Number (_, _, n, _))) ->
      update typhash nam (Vint n); 
      TokParam([TokID ("\\"^nam)], [TokInt n])
  | CellParamItem2 (nam, String s) ->
      TokParam([TokID ("\\"^nam)], [TokStr s])
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
		   | CellPinItem2 (pin, Intgr n) -> TokConn([TokID(pin)], [TokInt n])
		   | CellPinItem2 (pin, conn) -> TokConn([TokID(pin)], tran' conn)
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
  | TildeAnd expr -> unary bufh typhash wid "tilde_and" expr "AY"
  | Tilde expr -> unary bufh typhash wid "tilde" expr "AY"
  | Pling expr -> unary bufh typhash wid "not" expr "AY"
  | RedOr expr -> unary bufh typhash wid "reduce_or" expr "AY"
  | RedAnd expr -> unary bufh typhash wid "reduce_and" expr "AY"
  | IdArrayedColon (id, hi, lo) as x -> x
  | IdArrayedPlusColon (id, hi, lo) as x -> x
  | IdArrayed2 (Id id, sel) when is_mem typhash id -> memrd bufh typhash (mem_opt typhash id) id sel
  | IdArrayed2 (id, (Intgr _ | Number _)) as x -> x
  | IdArrayed2 (inner, expr) -> shiftx bufh typhash wid inner expr
  | Query (cond', ctrue', cfalse') -> ternary bufh typhash wid "mux" [cfalse'; ctrue'; cond'] "ABSY"
  | ExprOKL _ as x -> x
(*
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
  let args' = List.map (asgnexpr bufh typhash) args in
  let wid' ix arg =
     let w = width typhash arg in
     (match arg with Id s -> printf "Arg %d (%s), width = %d\n" ix s w | Number _ -> () | Intgr _ -> () | oth -> unhand := Some oth; failwith ("wid': "^Source_text_rewrite.getstr oth));
     flush stdout;
     CellParamItem2 (String.make 1 templ.[ix]^"_WIDTH", Number(10, 32, w, "")) in
  let conn' ix itm = CellPinItem2(String.make 1 templ.[ix], itm) in
  let widths = List.mapi wid' args' in
  dumpi bufh typhash (typ, [Itmlst (params@widths)], (InstNameParen1 (newnam(), Itmlst (List.mapi conn' args') :: []) :: []))

and unary bufh typhash wid func expr pnam =
  let rhs = asgnexpr bufh typhash expr and rslt = newid bufh typhash wid in
  addprim bufh typhash func [] [rhs;rslt] pnam;
  rslt

and dyadic bufh typhash wid func args pnam =
  let rslt = newid bufh typhash wid in
  addprim bufh typhash func (List.map (fun itm -> CellParamItem2 (itm^"_SIGNED", Number (10, 32, 0, ""))) ["A";"B"]) (args@[rslt]) pnam;
  rslt

and ternary bufh typhash wid func args pnam =
  let rslt = newid bufh typhash wid in
  addprim bufh typhash func (List.map (fun itm -> CellParamItem2 (itm^"_SIGNED", Number (10, 32, 0, ""))) ["A";"B";"S"]) (args@[rslt]) pnam;
  rslt

and memrd bufh typhash options id sel =
  let rslt = newid bufh typhash options.wid in
  let params = List.map (fun (parm,value) -> CellParamItem2 (parm, value)) [
  "ABITS", Intgr (clog2 options.tot) ;
  "CLK_ENABLE", Intgr 0 ;
  "CLK_POLARITY", Intgr 0 ;
  "MEMID", String ("\\\\"^id) ;
  "TRANSPARENT", Intgr 0 ;
  "WIDTH", Intgr options.wid ;
  ] in
  let pins = "ADDR" :: "CLK" :: "DATA" :: "EN" :: [] in
  let args = sel :: Number(2,1,0,"") :: rslt :: Number(2,1,1,"") :: [] in
  let conn' ix arg = let itm = asgnexpr bufh typhash arg in CellPinItem2(List.nth pins ix, itm) in
  dumpi bufh typhash ("memrd", [Itmlst params], (InstNameParen1 (newnam(), Itmlst (List.mapi conn' args) :: []) :: []));
  rslt

and shiftx bufh typhash wid lhs rhs = dyadic bufh typhash wid "shiftx" [lhs;rhs] "ABY"

and dumpi bufh typhash (typ, params, lst) = List.iter (function
        | InstNameParen1 (inst, Itmlst pins :: []) ->
	    bufh.i := (instance_template bufh typhash typ (match params with Itmlst lst :: _ -> lst | _ -> []) inst pins) :: !(bufh.i)
        | InstNameParen2 (inst, InstRange(lft,rght) :: []) ->
	    bufh.i := (instance_template bufh typhash typ (match params with Itmlst lst :: _ -> lst | _ -> []) inst []) :: !(bufh.i)
        | Id id -> addwire bufh typhash ([], id)
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
  else (print_endline (nam^": port "^string_of_int ix); update typhash nam (MaybePort ix); [])

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

let rec buffer bufh typhash wid = function
| ExprOKL lst -> ExprOKL (List.map (fun itm -> buffer bufh typhash (width typhash itm) itm) lst)
| Concat lst -> ExprOKL (List.map (fun itm -> buffer bufh typhash (width typhash itm) itm) lst)
| IdArrayed2 (IdArrayed2 (Id mem, addr) as inner, expr) -> shiftx bufh typhash wid (asgnexpr bufh typhash inner) expr
| IdArrayedColon (id, hi, lo) -> IdArrayedColon (buffer'' bufh typhash id, hi, lo)
| IdArrayedPlusColon (id, lo, wid') -> shiftx bufh typhash (ceval typhash wid') (buffer'' bufh typhash id) (buffer'' bufh typhash lo)
| expr -> asgnexpr' bufh typhash wid expr

and buffer'' bufh typhash x = buffer bufh typhash (width typhash x) x

let buffer' bufh typhash expr wid =
  let wid' = max wid (width typhash expr) in
  buffer bufh typhash wid' expr

let addconn bufh typhash  (nam, expr) =
    let rhs = buffer' bufh typhash expr 0 in
    bufh.i := TokConn (tran' nam, tran' rhs) :: !(bufh.i)

let asgn bufh typhash expr lhs =
let rhs = buffer' bufh typhash expr (width typhash lhs) in
bufh.c := Conn_stmt96(tran' lhs, tran' rhs) :: !(bufh.c)

(*
| Concat _ as lst -> bprintf buf' "            %s <= %s; // 385	\n" (vexpr typhash lst) (asgnexpr buf' typhash expr)
| Dot1 (lft, rght) -> bprintf buf' "            %s.%s <= %s; // 386	\n" (vexpr typhash lft) (vexpr typhash rght) (asgnexpr buf' typhash expr)
| IdArrayed2 (Id id, ix) -> bprintf buf' "            %s(%s) <= %s; // 387	\n" id (vexpr typhash ix) (asgnexpr buf' typhash expr)
| IdArrayedColon (Id id, hi, lo) -> bprintf buf' "            %s[%s : %s] <= %s; // 388	\n" id (vexpr typhash hi) (vexpr typhash lo) (asgnexpr buf' typhash expr)
| oth -> unhand := Some oth; failwith "asgn"
*)

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

let rec decl_template bufh typhash modules = function
    | Port(PortDir(dir, Atom kind), nam, [], []) -> addwire bufh typhash ([vdir (portpos typhash nam) dir], nam)
    | Port(dir, nam, [], []) -> addwire bufh typhash ([vdir (portpos typhash nam) dir], nam)
    | Itmlst (Port(dir, nam, rng, []) :: _ as lst) ->
        List.iter (function
		   | Port(PortDir(dir, Atom kind), nam, [], []) -> addwire bufh typhash ([vdir (portpos typhash nam) dir], nam)
		   | Port(dir, nam, [], []) -> addwire bufh typhash ([vdir (portpos typhash nam) dir], nam)
                   | Port(PortDir(dir, Atom kind), nam, [AnyRange _ as x], []) -> addwire bufh typhash (range typhash nam x :: [vdir (portpos typhash nam) dir], nam)
                   | Port(dir, nam, [AnyRange _ as x], []) -> addwire bufh typhash (range typhash nam x :: [vdir (portpos typhash nam) dir], nam)
                   | oth -> unhand := Some oth; failwith "Port Itmlst") lst
    | Port(PortDir(dir, Atom kind), nam, [AnyRange _ as x], []) -> addwire bufh typhash (range typhash nam x :: [vdir (portpos typhash nam) dir], nam)
    | Port(dir, nam, [AnyRange _ as x], []) -> addwire bufh typhash (range typhash nam x :: [vdir (portpos typhash nam) dir], nam)
    | DeclReg (reg_lst, [], []) -> List.iter (function
      | Id nam -> addwire bufh typhash ([], nam);
      | VarDeclAsgn (nam, expr) -> addwire bufh typhash ([], nam)
      | DeclAsgn (mem, (AnyRange (first,last) :: [])) -> addmem bufh typhash [first, last] 1 mem
      | oth -> unhand := Some oth; failwith "DeclReg550") reg_lst
    | DeclReg2 (reg_lst, (AnyRange (hi, lo) as x) :: []) -> List.iter (function
      | Id nam -> addwire bufh typhash ([range typhash nam x], nam);
      | DeclAsgn (mem, AnyRange(first,last) :: []) -> addmem bufh typhash [first, last] (ceval typhash hi - ceval typhash lo + 1) mem
      | DeclAsgn (mem, AnyRange(first,last) :: AnyRange(first',last') :: []) -> addmem bufh typhash [first, last; first', last'] (ceval typhash hi - ceval typhash lo + 1) mem
      | VarDeclAsgn (nam, expr) -> addwire bufh typhash ([], nam)
      | oth -> unhand := Some oth; failwith "DeclReg555") reg_lst;
    | TypEnum4 (Deflt, id_lst, [Id nam]) -> elabenum typhash nam id_lst
    | TaskDecl(nam, arg1, arg2, arg3) -> update typhash nam (Task(arg1,arg2,arg3))
    | ParamDecl (Atom "localparam", [ParamAsgn1 (nam, expr)]) -> update typhash nam (Vint (ceval typhash expr))
    | ParamDecl (Param ("localparam", Atom "implicit", AnyRange (hi, lo) :: []), [ParamAsgn1 (nam, expr)]) -> update typhash nam (Vint (ceval typhash expr))
    | NetDecl (Atom "wire", wire_lst) -> List.iter (function
          | Id nam -> update typhash nam Std_logic;
	      addwire bufh typhash ([], nam)
	  | DeclAsgn (nam, AnyRange (hi, lo) :: []) ->
              addwire bufh typhash ([Wire_optionswidth (ceval typhash hi - ceval typhash lo + 1)], nam)
          | InitSig (nam, expr) -> update typhash nam Std_logic; addconn bufh typhash (Id nam, expr)
	  | oth -> unhand := Some oth; failwith "NetDecl'") wire_lst;
    | DeclInt2 id_lst -> List.iter (function
	| Id nam -> addwire bufh typhash ([Wire_optionswidth 32], nam)
        | VarDeclAsgn (nam, expr) -> update typhash nam (Vint (ceval typhash expr))
        | oth -> unhand := Some oth; failwith "DeclInt2") id_lst
(*
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
    | PkgImport (Itmlst lst) -> List.iter (decl_template buf' typhash modules) lst
    | PkgImportItm (pkg, Atom "*") -> ()
    | DeclData (Typ5 (Atom "logic", AnyRange (lft, rght) :: AnyRange (lft', rght') :: []), VarDeclAsgn (mem, ExprOKL lst) :: []) -> () (* placeholder *)
    *)
    | ParamDecl (LocalParamTyp (Typ8 (Atom ("int"|"integer"|"longint"), Deflt)), [ParamAsgn1 (nam , expr)]) ->
        update typhash nam (Vint (ceval typhash expr))
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

let dbgcase = ref []

let restrict' typhash wid nam = function
  | Vemember(s, _, Number(b,w,n,_)) -> Number(b,min w wid,n mod (1 lsl wid),"")
  | Vemember(s, _, Intgr n) -> Number(2,wid,n mod (1 lsl wid),"")
  | Std_logic_vector ((Number _|Intgr _ as hi), (Number _|Intgr _ as lo)) ->
      let hi' = ceval typhash hi in
      let lo' = ceval typhash lo in
      let wid' = hi' - lo' + 1 in
      IdArrayedColon(Id nam, Intgr(if wid' > wid then lo' + wid-1 else hi'), Intgr(lo'))
  | oth -> coth := Some oth; failwith "restrict'"

let rec restrict typhash wid = function
  | Number(b,w,n,_) -> Number(b,min w wid,n mod (1 lsl wid),"")
  | Id s -> (match Hashtbl.find_opt typhash s with Some x -> restrict' typhash wid s x | None -> Id s)
  | Atom "default" as x -> x
  | Intgr n -> Number(2,wid,n mod (1 lsl wid),"")
  | ExprOKL lst -> ExprOKL (List.rev (restrict_lst typhash wid (List.rev lst)))
  | oth -> unhand := Some oth; failwith (sprintf "restrict (%d) %s (%d)" wid (Source_text_rewrite.getstr oth) (width typhash oth))

and restrict_lst typhash wid = function
  | [] -> []
  | hd :: tl -> let hdwid = width typhash hd in if hdwid = wid then hd :: [] else if hdwid < wid then hd :: restrict_lst typhash (wid-hdwid) tl else restrict typhash wid hd :: []

let dbgeq = ref None
let dbgcommon = ref None
  
let generate_assignment_common bufh typhash (lhs, rhs) update =
let lhswid = width typhash lhs in
let rhswid = width typhash rhs in
let rhs' = buffer' bufh typhash (if lhswid < rhswid then restrict typhash lhswid rhs
else if lhswid > rhswid then Concat (Number(2,lhswid-rhswid,0,"") :: rhs :: [])
else rhs) 0 in match lhs with
| IdArrayedPlusColon (IdArrayed2 (Id mem, (Id _ as addr)), lo', Intgr wid') ->
(match Hashtbl.find_opt typhash mem with
  | Some Vmem {off;siz;wid} ->
      let lo = match lo' with
         | Intgr lo' -> Number (2, wid, ((1 lsl wid)-1) lsl lo', "")
	 | Id mask -> Id mask
         | Add _ as x -> buffer' bufh typhash x 0 
         | oth -> unhand := Some oth; failwith "PlusColon" in
      Update_listmemwr("\\\\"^mem, tran' addr, tran' rhs', tran' lo, tran' (Number(2, 0, 0, "")))
  | _ -> if update then TokUpdate(tran' lhs, tran' rhs') else Assign_stmt67(tran' lhs, tran' rhs'))
| IdArrayedColon (IdArrayed2 (Id mem, (Id _ as addr)), (Intgr hi|Number(_,_,hi,_)), (Intgr lo|Number(_,_,lo,_))) ->
(match Hashtbl.find_opt typhash mem with
  | Some Vmem {off;siz;wid} ->
      let lo = Number (2, wid, ((1 lsl wid)-1) lsl lo, "") in
      Update_listmemwr("\\\\"^mem, tran' addr, tran' rhs', tran' lo, tran' (Number(2, 0, 0, "")))
  | _ -> if update then TokUpdate(tran' lhs, tran' rhs') else Assign_stmt67(tran' lhs, tran' rhs'))
| IdArrayed1 (Id mem, addr, Number(_,_,abit,_)) -> 
(match Hashtbl.find_opt typhash mem with
  | Some Vmem {off;siz;wid} ->
      dbgeq := Some (lhs,rhs');
      Update_listmemwr("\\\\"^mem, tran' addr, tran' rhs', tran' (Number (2, wid, (1 lsl abit), "")), tran' (Number(2, 0, 0, "")))
  | _ -> if update then TokUpdate(tran' lhs, tran' rhs') else Assign_stmt67(tran' lhs, tran' rhs'))
| IdArrayed1 (Id mem, addr, abit) -> 
(match Hashtbl.find_opt typhash mem with
  | Some Vmem {off;siz;wid} ->
      dbgeq := Some (lhs,rhs');
      Update_listmemwr("\\\\"^mem, tran' addr, tran' rhs', tran' abit, tran' (Number(2, 0, 0, ""))) (* work in progress *)
  | _ -> if update then TokUpdate(tran' lhs, tran' rhs') else Assign_stmt67(tran' lhs, tran' rhs'))
| IdArrayed2 (Id mem, addr) -> 
(match Hashtbl.find_opt typhash mem with
  | Some Vmem {off;siz;wid} ->
      dbgeq := Some (lhs,rhs');
      Update_listmemwr("\\\\"^mem, tran' addr, tran' rhs', tran' (Number (2, wid, (1 lsl wid)-1, "")), tran' (Number(2, 0, 0, ""))) (* work in progress *)
  | _ -> if update then TokUpdate(tran' lhs, tran' rhs') else Assign_stmt67(tran' lhs, tran' rhs'))
| oth -> dbgcommon := Some (lhs,rhs'); if update then TokUpdate(tran' lhs, tran' rhs') else Assign_stmt67(tran' lhs, tran' rhs')

let generate_assignment bufh typhash (lhs, rhs) = generate_assignment_common bufh typhash (lhs, rhs) false
let generate_update bufh typhash (lhs, rhs) = generate_assignment_common bufh typhash (lhs, rhs) true

let rec cnv' bufh dhash typhash inst = function
    | Atom ";" -> ([],[],[])
    | Seq ("", lst) ->
        let lst' = List.map (cnv' bufh dhash typhash inst) lst in
        (List.flatten (List.map (fun (p,u,d) -> p) lst'),
         List.flatten (List.map (fun (p,u,d) -> u) lst'),
         List.flatten (List.map (fun (p,u,d) -> d) lst'))
    | EquateConcat(lhslst, expr) ->
        let wid = ref 0 in
        let dlylst = List.map (fun lhs ->
          let dly = dlymemo bufh dhash typhash lhs in
	  wid := !wid + width typhash lhs;
          let lhs' = buffer' bufh typhash lhs (width typhash lhs) in
          (generate_assignment bufh typhash (dly, lhs'),
           generate_update bufh typhash (lhs, dly),
          dly)) lhslst in
        let rhs = buffer' bufh typhash expr !wid in
        List.map (fun (p,u,d) -> p) dlylst,
		  generate_assignment bufh typhash (GenBlock (List.map (fun (p,u,d) -> d) dlylst), rhs) :: [],
		  List.map (fun (p,u,d) -> u) dlylst
    | Equate (lhs', (Number _ as expr)) -> let lhs = Id lhs' in
        let dly = dlymemo bufh dhash typhash lhs in
        (generate_assignment bufh typhash (dly, lhs) :: [],
         generate_assignment bufh typhash (dly, expr) :: [],
         generate_update bufh typhash (lhs, dly) :: [])
    | Equate (lhs', expr) -> let lhs = Id lhs' in
        let dly = dlymemo bufh dhash typhash lhs in
        let rhs = buffer' bufh typhash expr (width typhash lhs) in
        (generate_assignment bufh typhash (dly, lhs) :: [],
         generate_assignment bufh typhash (dly, rhs) :: [],
         generate_update bufh typhash (lhs, dly) :: [])
    | EquateSelect (lhs', (Number _ as sel), expr) -> let lhs = Id lhs' in
        let dly = dlymemo bufh dhash typhash lhs in
        let rhs = buffer' bufh typhash expr 0 in
        (generate_assignment bufh typhash (dly, lhs) :: [],
         generate_assignment bufh typhash (IdArrayed2(dly,sel), rhs) :: [],
         generate_update bufh typhash (lhs, dly) :: [])
    | EquateSelect2 (IdArrayed2(lhs, sel'), sel, expr) as x ->
        let dly = dlymemo bufh dhash typhash lhs in
        let rhs = buffer' bufh typhash expr 0 in
        (generate_assignment bufh typhash (dly, lhs) :: [],
         generate_assignment bufh typhash (IdArrayed1(lhs,sel',sel), rhs) :: [],
         generate_update bufh typhash (lhs, dly) :: [])
    | EquateSlice (lhs, ((Number _| Intgr _) as hi), ((Number _| Intgr _) as lo), expr) ->
        let dly = dlymemo bufh dhash typhash lhs in
        let rhs = buffer' bufh typhash expr 0 in
        ([],
         generate_assignment bufh typhash (IdArrayedColon(lhs,hi,lo), rhs) :: [],
         [])
    | EquateSlicePlus (lhs, (lo), (Intgr _ | Number _ as wid), expr) ->
        let dly = dlymemo bufh dhash typhash lhs in
        let rhs = buffer' bufh typhash expr 0 in
        ([],
         generate_assignment bufh typhash (IdArrayedPlusColon(lhs,lo,wid), rhs) :: [],
         [])
    | Blocking (FopAsgn (lhs', (Number _ as expr))) -> let lhs = Id lhs' in
        let dly = dlymemo bufh dhash typhash lhs in
        (generate_assignment bufh typhash (dly, lhs) :: [],
         generate_assignment bufh typhash (dly, expr) :: [],
         generate_update bufh typhash (lhs, dly) :: [])
    | Blocking (FopAsgn (lhs', expr)) -> let lhs = Id lhs' in
        let dly = dlymemo bufh dhash typhash lhs in
        let rhs = buffer' bufh typhash expr (width typhash lhs) in
        (generate_assignment bufh typhash (dly, lhs) :: [],
         generate_assignment bufh typhash (dly, rhs) :: [],
         generate_update bufh typhash (lhs, dly) :: [])
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
  | oth -> unhand := Some oth; failwith "mapedge"

let rec proc_template bufh typhash = function
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
        | Id id -> addwire bufh typhash ([], id)
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
let port' = ref []
let dbgtyp = ref (Hashtbl.create 1)

let template modules = function
  | Modul(nam, parm_lst, port_lst, body_lst') ->
  print_endline ("Translating module: "^nam);
  let typhash = Hashtbl.create 255 in
  dbgtyp := typhash;
  let bufh = bufhash () in
  let bufm = ref [] in
  bufm := Attr_stmt ("\\cells_not_processed", [TokInt 1]) :: !(bufm);
  List.iter (fun itm -> let _ = parm_map typhash itm in ()) parm_lst;
  port' := port_lst;
  let port_lst' = sub' typhash port_lst in
  let ports' = List.flatten(List.mapi (fun ix itm -> ports typhash (ix+1) itm) port_lst') in
  let body_lst = sub' typhash body_lst' in
  body := body_lst;
  body' := body_lst';
  List.iter (decl_template bufh typhash modules) body_lst;
  List.iter (proc_template bufh typhash) body_lst;
  bufm := Module12 (nam, ports' @ catbuf bufh) :: !(bufm);
  List.rev (!bufm);
  | PackageBody (pkg, body_lst') ->
  let typhash = Hashtbl.create 255 in
  let body_lst = sub' typhash body_lst' in
  let bufh = bufhash () in
  List.iter (decl_template bufh typhash modules) body_lst;
  List.rev (catbuf bufh);
  | oth -> unhand := Some oth; failwith "This template only handles modules/packages"
