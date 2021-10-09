open Source_text_rewrite_types
open Input_rewrite_types
open Source_text_lex
open Source_text
open Printf

type mem_opts = {off:int list; siz:int list; wid:int; tot:int}

type vtyp =
  | Vint of int
  | Vpkg of string * string
  | Unsigned
  | Unsigned_vector of rw * rw
  | Signed
  | Signed_vector of rw * rw
  | Vsigtyp
  | Vdot
  | Vstr of string
  | Vtyp of string
  | Vfun of string
  | Venum of string
  | Vintf of rw
  | MaybePort of int * vtyp * rw
  | Vemember of string * string * rw
  | Task of rw * rw * rw
  | Vmem of mem_opts
  | InstArray of rw * rw * rw
  | Vreal of float
  | Vlocal of int * rw

type bufh = {c:ilang list ref;
    i:ilang list ref;
    l:ilang list ref;
    w:ilang list ref}

type dead = 
| Undecidable
| Always_false
| Always_true

let logf = open_out "logfile.txt"

let vtyp = function
  | Vint _ -> "Vint"
  | Vpkg _ -> "Vpkg"
  | Unsigned -> "Unsigned"
  | Unsigned_vector _ -> "Unsigned_vector"
  | Signed -> "Signed"
  | Signed_vector _ -> "Signed_vector"
  | Vsigtyp -> "Vsigtyp"
  | Vdot -> "Vdot"
  | Vstr _ -> "Vstr"
  | Vtyp _ -> "Vtyp"
  | Vfun _ -> "Vfun"
  | Venum _ -> "Venum"
  | Vintf _ -> "Vintf"
  | MaybePort _ -> "MaybePort"
  | Vemember _ -> "Vemember"
  | Task _ -> "Task"
  | Vmem _ -> "Vmem"
  | InstArray _ -> "InstArray"
  | Vreal _ -> "Vreal"
  | Vlocal _ -> "Vlocal"

let unhand = ref None
let unhand_typ = ref None
let unhand_lst = ref []
let coth = ref None

let backtrace msg = print_endline ("backtrace: "^(Printexc.get_callstack 5 |> Printexc.raw_backtrace_to_string)^msg)

(*
let print_endline = backtrace
*)

let update typhash id expr = match id with
  | Id id -> (* print_endline id; *) Hashtbl.replace typhash id expr
  | oth -> failwith ("Argument "^(Source_text_rewrite.getstr oth)^" of update must be Id")
  
let is_mem typhash id = match Hashtbl.find_opt typhash id with Some (Vmem _) -> true | _ -> false
let mem_opt typhash id = match Hashtbl.find_opt typhash id with Some (Vmem opt) -> opt | _ -> failwith "mem_opt"

let rec obin w n = 
  (if w > 1 then obin (w-1) (n lsr 1) else "")^string_of_int (n land 1)

let clog2 n = if n = 0 then 0 else int_of_float(ceil(log(float_of_int n)/.log 2.))

let rec ceval typhash = function
| Intgr n -> n
| Number (_,_,n,_) -> n
| Id s ->
  if verbose then print_endline ("Eval "^s^" as a constant");
  ceval' typhash (match Hashtbl.find_opt typhash s with Some x -> x | None -> print_endline ("Not found: "^s); Vint 0)
| Add (lhs, rhs) -> ceval typhash lhs + ceval typhash rhs
| Sub (lhs, rhs) -> ceval typhash lhs - ceval typhash rhs
| Mult (lhs, rhs) -> ceval typhash lhs * ceval typhash rhs
| Div (lhs, rhs) -> let divisor = ceval typhash rhs in if divisor = 0 then ceval typhash lhs else ceval typhash lhs / divisor
| And (lhs, rhs) -> (ceval typhash lhs) land (ceval typhash rhs)
| And2 (lhs, rhs) -> (if ceval typhash lhs <> 0 then 1 else 0) land (if ceval typhash rhs <> 0 then 1 else 0)
| Or (lhs, rhs) -> (ceval typhash lhs) lor (ceval typhash rhs)
| Or2 (lhs, rhs) -> (if ceval typhash lhs <> 0 then 1 else 0) lor (if ceval typhash rhs <> 0 then 1 else 0)
| Xor (lhs, rhs) -> (ceval typhash lhs) lxor (ceval typhash rhs)
| Less (lhs, rhs) -> if ceval typhash lhs < ceval typhash rhs then 1 else 0
| LtEq (lhs, rhs) -> if ceval typhash lhs <= ceval typhash rhs then 1 else 0
| Equals (lhs, rhs) -> if ceval typhash lhs = ceval typhash rhs then 1 else 0
| Equals3 (lhs, rhs) -> if ceval typhash lhs = ceval typhash rhs then 1 else 0
| Greater (lhs, rhs) -> if ceval typhash lhs > ceval typhash rhs then 1 else 0
| Query (lhs, rhs, rhs') -> if ceval typhash lhs <> 0 then ceval typhash rhs else  ceval typhash rhs'
| StarStar (lhs, rhs) -> int_of_float(float_of_int (ceval typhash lhs) ** float_of_int(ceval typhash rhs))
| UMinus rhs -> - (ceval typhash rhs)
| UPlus rhs -> + (ceval typhash rhs)
| Shiftl (lhs, rhs) -> ceval typhash lhs lsl ceval typhash rhs
| Shiftr (lhs, rhs) -> ceval typhash lhs lsr ceval typhash rhs
| Repl (Number (_, _, n, _), arg::[]) -> ceval typhash arg (* placeholder *)
| Sys ("$clog2", x) -> clog2 (ceval typhash x)
| RedAnd x -> ceval typhash x (* placeholder *)
| RedOr x -> ceval typhash x (* placeholder *)
| RedXor x -> ceval typhash x (* placeholder *)
| Tilde x -> ceval typhash x (* placeholder *)
| Pling x -> ceval typhash x (* placeholder *)
| TildeAnd x -> ceval typhash x (* placeholder *)
| TildeOr x -> ceval typhash x (* placeholder *)
| Expression x -> ceval typhash x
| PackageBody (pkg, [id]) -> ceval typhash id (* placeholder *)
| Sys ("$bits", Typ1 id_t) -> csiz' typhash (match Hashtbl.find_opt typhash id_t with Some x -> x | None -> print_endline ("Not found: "^id_t); Unsigned)
| Sys ("$size", Dot1 _) -> 1 (* placeholder *)
| ExprOKL [] -> 0
| ExprOKL (hd::tl) -> ((ceval typhash hd) lsl (width typhash (ExprOKL tl))) + (ceval typhash (ExprOKL tl))
| Float f -> int_of_float f
| oth -> unhand := Some oth; failwith "ceval"

and ceval' typhash = function
| Vint n -> n
| Unsigned -> print_endline ("Type std_logic evaluated to zero"); 0
| Unsigned_vector _ -> print_endline ("Type std_logic_vector evaluated to zero"); 0
| Vtyp s -> print_endline ("Type "^s^" evaluated to zero"); 0
| Vlocal (n, expr) ->
  if verbose then print_endline ("Evaluating local parameter");
  ceval typhash expr
| oth -> coth := Some oth; failwith "ceval'"

and csiz' typhash = function
| Vint _ -> 32
| Unsigned -> 1
| Unsigned_vector(hi,lo) -> ceval typhash hi - ceval typhash lo + 1
| Signed -> 1
| Signed_vector(hi,lo) -> ceval typhash hi - ceval typhash lo + 1
| Vtyp s -> print_endline ("Sizeof type "^s^" evaluated to zero"); 0
| Venum s -> 8
| Vemember(s, _, _) -> csiz' typhash (match Hashtbl.find_opt typhash s with Some x -> x | None -> print_endline ("Not found: "^s); Unsigned)
| Vmem {off;siz;wid;tot} -> tot * wid
| Vsigtyp -> 1 (* placeholder *)
| Vreal _ -> 64
| Vlocal (n,_) -> n
| oth -> coth := Some oth; failwith "csiz'"

and width typhash = function
| Intgr n -> 32
| Number (_,w,_,_) -> w
| Id s -> widthsel typhash s (Hashtbl.find_opt typhash s)
| IdArrayed2 (IdArrayed2 (Id mem, Id addr), Id abit) -> 1
| IdArrayed2 (Id s, _) -> widthsel' typhash s (Hashtbl.find_opt typhash s)
(* csiz' typhash (match Hashtbl.find_opt typhash s with Some x -> x | None -> print_endline ("Not found: "^s); Unsigned) *)
| Add (lhs, rhs) -> max (width typhash lhs) (width typhash rhs)
| Sub (lhs, rhs) -> max (width typhash lhs) (width typhash rhs)
| Mult (lhs, rhs) -> width typhash lhs + width typhash rhs
| Div (lhs, rhs) -> width typhash lhs
| Mod (lhs, (Id _ as rhs)) -> width typhash rhs
| And (lhs, rhs) -> min (width typhash lhs) (width typhash rhs)
| And2 _ -> 1
| Or (lhs, rhs) -> max (width typhash lhs) (width typhash rhs)
| Or2 _ -> 1
| Pling _ -> 1
| UPlus (lhs) -> width typhash lhs
| UMinus (lhs) -> width typhash lhs
| Shiftl(lhs, Expression arg) -> width typhash (Shiftl(lhs, arg))
| Shiftl(lhs, (Intgr n|Number(_,_,n,_))) -> width typhash lhs + n
| Shiftr(lhs, (Intgr n|Number(_,_,n,_))) -> width typhash lhs - n
| Shiftl(lhs, rhs) -> width typhash lhs + (1 lsl (width typhash rhs)) - 1
| Shiftr(lhs, _) -> width typhash lhs
| Shiftr3(lhs, _) -> width typhash lhs
| Xor (lhs, rhs) -> max (width typhash lhs) (width typhash rhs)
| Less (lhs, rhs) -> 1
| LtEq (lhs, rhs) -> 1
| Equals (lhs, rhs) -> 1
| NotEq (lhs, rhs) -> 1
| NotEq3 (lhs, rhs) -> 1
| GtEq (lhs, rhs) -> 1
| Greater (lhs, rhs) -> 1
| Atom "default" -> 1
| Query (lhs, rhs, rhs') -> max (width typhash rhs) (width typhash rhs')
| StarStar (lhs, rhs) -> (width typhash lhs) + ceval typhash rhs
| Sys ("$clog2", x) -> clog2 (width typhash x)
| Expression x -> width typhash x
| PackageBody (pkg, [id]) -> width typhash id (* placeholder *)
| Sys ("$bits", Typ1 id_t) -> csiz' typhash (match Hashtbl.find_opt typhash id_t with Some x -> x | None -> print_endline ("Not found: "^id_t); Unsigned)
| Sys ("$size", Dot1 _) -> 1 (* placeholder *)
| Sys ("$signed", rhs) -> width typhash rhs
| Sys ("$unsigned", rhs) -> width typhash rhs
| ExprOKL [] -> 0
| ExprOKL (hd::tl) -> width typhash hd + width typhash (ExprOKL tl)
| IdArrayedColon(_, hi, lo) -> ceval typhash hi - ceval typhash lo + 1
| GenBlock [] -> 0
| GenBlock (hd::tl) -> width typhash hd + width typhash (GenBlock tl)
| Concat [] -> 0
| Concat (hd::tl) -> width typhash hd + width typhash (Concat tl)
| RedAnd _ -> 1
| RedOr _ -> 1
| RedXor _ -> 1
| TildeAnd _ -> 1
| TildeOr _ -> 1
| CaretTilde x -> width typhash x
| Tilde x -> width typhash x
| Repl ((Intgr n|Number (_, _, n, _)), arg :: []) -> n * width typhash arg
| IdArrayedPlusColon (_, _, (Intgr w|Number(_,_,w,_))) -> w
| IdArrayed1 (Id id, addr, abit) -> 1
| IdArrayed2 (IdArrayed2 (Id id, addr), abot) -> 1
| Float f -> 64
| String s -> 8 * String.length s
| FunRef _ -> 1 (* placeholder *)
| oth -> unhand := Some oth; failwith "width"

and widthsel typhash s = function
    | Some (Vmem {off;siz;wid}) -> wid
    | Some (MaybePort (n,typ,_)) -> csiz' typhash typ
    | Some x -> csiz' typhash x
    | None -> print_endline ("Not found: "^s); 1

and widthsel' typhash s = function
    | Some (Vmem {off;siz;wid}) -> wid
    | Some x -> 1
    | None -> print_endline ("Not found: "^s); 1

and signof' = function
| (Signed, Signed) -> Signed
| _ -> Unsigned

and signoff'' s = function
    | Some (Signed|Signed_vector _) -> Signed
    | Some (Unsigned|Unsigned_vector _) -> Unsigned
    | Some MaybePort (_, typ, _) -> signoff'' s (Some typ)
    | Some x -> Unsigned
    | None -> print_endline ("Not found: "^s); Unsigned

and signof typhash = function
| Intgr n -> Signed
| Number _ -> Unsigned
| Id s -> signoff'' s (Hashtbl.find_opt typhash s)
| Add (lhs, rhs) -> signof' (signof typhash lhs, signof typhash rhs)
| Sub (lhs, rhs) -> signof' (signof typhash lhs, signof typhash rhs)
| Mult (lhs, rhs) -> signof' (signof typhash lhs, signof typhash rhs)
| Div (lhs, rhs) -> signof typhash lhs
| Mod (lhs, (Id _ as rhs)) -> signof typhash rhs
| And (lhs, rhs) -> signof' (signof typhash lhs, signof typhash rhs)
| And2 (lhs, rhs) -> signof' (signof typhash lhs, signof typhash rhs)
| Or (lhs, rhs) -> signof' (signof typhash lhs, signof typhash rhs)
| Or2 (lhs, rhs) -> signof' (signof typhash lhs, signof typhash rhs)
| Pling rhs -> signof typhash rhs
| UPlus (lhs) -> signof typhash lhs
| UMinus (lhs) -> signof typhash lhs
| Shiftl(lhs, rhs) -> signof typhash lhs
| Shiftr(lhs, rhs) -> signof typhash lhs
| Shiftr3(lhs, _) -> Signed
| Xor (lhs, rhs) -> signof' (signof typhash lhs, signof typhash rhs)
| Less (lhs, rhs) -> signof' (signof typhash lhs, signof typhash rhs)
| LtEq (lhs, rhs) -> signof' (signof typhash lhs, signof typhash rhs)
| Equals (lhs, rhs) -> signof' (signof typhash lhs, signof typhash rhs)
| NotEq (lhs, rhs) -> signof' (signof typhash lhs, signof typhash rhs)
| GtEq (lhs, rhs) -> signof' (signof typhash lhs, signof typhash rhs)
| Greater (lhs, rhs) -> signof' (signof typhash lhs, signof typhash rhs)
| Query (lhs, rhs, rhs') -> signof' (signof typhash rhs, signof typhash rhs')
| StarStar (lhs, rhs) -> signof' (signof typhash rhs, signof typhash rhs)
| Expression x -> signof typhash x
| PackageBody (pkg, [id]) -> signof typhash id (* placeholder *)
| Sys ("$bits", Typ1 id_t) -> Unsigned
| Sys ("$size", Dot1 _) -> Unsigned
| Sys ("$signed", rhs) -> Signed
| Sys ("$unsigned", rhs) -> Unsigned
| ExprOKL [] -> Signed
| ExprOKL (hd::tl) -> signof' (signof typhash hd, signof typhash (ExprOKL tl))
| IdArrayedColon(nam, hi, lo) -> signof typhash nam
| RedAnd rhs -> signof typhash rhs
| RedOr rhs -> signof typhash rhs
| RedXor rhs -> signof typhash rhs
| TildeAnd rhs -> signof typhash rhs
| TildeOr rhs -> signof typhash rhs
| CaretTilde x -> signof typhash x
| Tilde x -> signof typhash x
| Repl ((Intgr n | Number (_, _, n, _)), arg :: []) -> signof typhash arg
| IdArrayed2 (Id _ as s, _) -> signof typhash s
| IdArrayedPlusColon (Id _ as id, _, _) -> signof typhash id
| IdArrayed1 (Id _ as id, addr, abit) -> signof typhash id
| IdArrayed2 (IdArrayed2 (Id _ as id, _), _) -> signof typhash id
| Float f -> Signed
| String s -> Signed
| FunRef _ -> Signed (* placeholder *)
| oth -> unhand := Some oth; failwith "signof"

let is_const' = function
| Vint n -> true
| Venum _ -> true
| Unsigned -> false
| Unsigned_vector _ -> false
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

let othx = ref None
let dbgsplit = ref None
let dbgform = ref []
let dbgact = ref []
let dbgasgn = ref []
let dbgloop = ref None
let body' = ref []
let port' = ref []
let dbgm' = ref None
let dbgarr = ref None
let dbginst = ref None
let recurhash = ref (Hashtbl.create 1)
let dbgsub = ref []
let dbgsubst = ref []
let dbgeq = ref []
let dbgproc = ref None
let dbgcommon = ref None
let dbgdecl = ref []
let dbgports = ref []
let dbgtyp = ref (Hashtbl.create 1)
let otha = ref None
let dbgcase = ref []
let dbgwida = ref 0
let dbgwids = ref 0
let dbgmem = ref None
let dbgmem' = ref None
let dbgpar = ref []
let dbgatom = ref ""

let rec recurs1 (attr: Source_text_rewrite.attr) = function
  | Modul (nam, params, args, body) -> Modul (nam, List.map (recurs1 attr) params, (let _ = Hashtbl.remove attr.subst Deflt in List.map (recurs1 attr) args), List.map (recurs1 attr) body)
  | FunDecl(id, _, _) as x -> Hashtbl.replace attr.subst (FunRef (id,[])) x; x
  | AutoFunDecl(id, _, _) as x -> Hashtbl.replace attr.subst (FunRef (id,[])) x; x
  | TaskDecl(id, _, _, _) as x -> Hashtbl.replace attr.subst (TaskRef (id,[])) x; x
  | ParamDecl (Atom ("Parameter"|"localparam"), lst) as x ->
      List.iter (function ParamAsgn1 (p, n) -> Hashtbl.replace attr.subst (Id p) n | oth -> failwith "ParamDecl1") lst; x
  | ParamDecl (LocalParamTyp (Typ8 (Atom "integer", Deflt)), lst) as x ->
      List.iter (function ParamAsgn1 (p, n) -> Hashtbl.replace attr.subst (Id p) n | oth -> failwith "ParamDecl2") lst; x
  | ParamDecl (Param ("localparam", Deflt, [AnyRange _]), lst) as x ->
      List.iter (function ParamAsgn1 (p, n) -> Hashtbl.replace attr.subst (Id p) n | oth -> failwith "ParamDecl3") lst; x
  | ParamDecl _ as x -> othx := Some x; failwith "recurs1"
  | Param (nam, expr, _) as x -> Hashtbl.add attr.Source_text_rewrite.subst (Id nam) expr; x
  | oth -> Source_text_rewrite.descend' {attr with fn=recurs1} oth

let rec recurs2 (attr: Source_text_rewrite.attr) = function
  | Id _ as id -> (match Hashtbl.find_opt attr.subst id with None -> id | Some exp -> recurs2 attr exp)
  | ForLoop ([Asgn1 (Id ix, strt)], stop, Asgn1 (Id ix'', Add (Id ix''', inc)), body) ->
      recurs2 attr (iter attr ix ("ForLoop_"^ix) (elabeval attr strt) stop (elabeval attr inc) body)
  | ForLoop _ as x -> unhand := Some x; failwith "unroll"
  | LoopGen1 (Id ix, lbl, strt, stop, inc, body) ->
      recurs2 attr (iter attr ix lbl (elabeval attr strt) stop (elabeval attr inc) (Itmlst body))
  | LoopGen1 _ as oth -> unhand := Some oth; failwith "LoopGen1"
  | BeginBlock lst -> Seq("", List.map (recurs2 attr) lst)
  | ContAsgn (Asgn1 (Id _ as lhs, FunRef (id, arglst)) :: []) ->
      Hashtbl.add attr.subst (Id id) lhs;
      let rslt = AlwaysLegacy(AtStar, fsubst attr arglst id) in
      Hashtbl.remove attr.subst (Id id);
      rslt
  | CondGen1 (cond, true', false') -> recurs2 attr (if elabeval attr cond <> 0 then true' else false')
  | FunRef (id, arglst) -> fsubst attr arglst id
  | TaskRef (id, arglst) -> tsubst attr arglst id
(* *)
  | FunDecl(id, _, _) -> FunDecl(id, Deflt, Deflt)
  | AutoFunDecl(id, _, _) -> AutoFunDecl(id, Deflt, Deflt)
  | TaskDecl(id, _, _, _) -> TaskDecl(id, Deflt, Deflt, Deflt)
(* *)
  | Dot1 (IdArrayed2 (Id lbl, sel), Id id) -> let n = elabeval attr sel in Id (lbl^"["^string_of_int n^"]."^id)
(*
  | Blocking x -> (match x with
        | FopAsgn (lhs, rhs) -> recurs2 attr (Equate (lhs, rhs))
        | FopAsgnArraySel (out, sel, expr) -> recurs2 attr (EquateSelect (out, sel, expr))
        | FopAsgnConcat (idlst, expr) -> recurs2 attr (EquateConcat (idlst, expr))
	| oth -> unhand := Some oth; failwith "blocking not replaced")
*)
  | oth -> Source_text_rewrite.descend' {attr with fn=recurs2} (simplify oth)

and iter attr ix lbl strt stop inc stmts =
    print_endline ("iter: "^lbl);
    let loopvar = ref strt in
    let block = ref [] in
    let continue = match stop with
       | Less (Id ix', exp) -> (>) (elabeval attr exp)
       | LtEq (Id ix', exp) -> (>=) (elabeval attr exp)
       | oth -> unhand := Some oth; failwith "loop term" in
    dbgloop := Some (strt, stop, !loopvar);
    while continue !loopvar do
      begin
        Hashtbl.replace attr.Source_text_rewrite.subst (Id ix) (Intgr !loopvar);
        if verbose then print_endline (string_of_int !loopvar);
	let repl = recurs2 attr stmts in
	block := Seq(lbl^"["^string_of_int !loopvar^"]", match recurs2 attr repl with Itmlst lst -> lst | oth -> oth :: []) :: !block;
	loopvar := !loopvar + inc;
      end
    done;
    Itmlst (List.rev !block)

and fsubst' attr arglst = function
(* recursive functions are TBD *)
| AutoFunDecl (nam, Deflt, Deflt) -> Atom ";"
| AutoFunDecl (nam, Itmlst [AnyRange (hi', lo')], TFBody (formlst', body)) -> Atom ";"
| FunDecl (nam, Itmlst [AnyRange (hi', lo')], TFBody (formlst', body)) ->
  let formlst = List.flatten (List.map (function
    | TF_port_decl (In, [AnyRange (hi, lo)], formlst) -> List.map (function
          | TF_variable (Id _ as formal, Deflt, Deflt, Deflt) -> formal
          | oth -> unhand := Some oth; failwith "param1") formlst
    | TF_port_decl (In, [Atom "integer"], formlst) -> List.map (function
          | TF_variable (Id _ as formal, Deflt, Deflt, Deflt) -> formal
          | oth -> unhand := Some oth; failwith "param2") formlst
    | BlockItem (ParamDecl (Atom "Parameter", lst)) -> List.iter (function
	  | ParamAsgn1 (id, expr) -> Hashtbl.add attr.Source_text_rewrite.subst (Id id) expr
          | oth -> unhand := Some oth; failwith "param") lst; []
    | DeclInt2 formlst -> List.map (function
	  | Id _ as formal -> formal
          | oth -> unhand := Some oth; failwith "param3") formlst;
    | oth -> unhand := Some oth; failwith "fsubst'") formlst') in
  printf "fsubst1 nam: %s, len(arglst) = %d, len(formlst) = %d\n" nam (List.length arglst) (List.length formlst);
  dbgact := arglst;
  dbgform := formlst;
  List.iter2 (fun actual -> function
    | Id _ as formal -> Hashtbl.add attr.Source_text_rewrite.subst formal actual
    | oth -> unhand := Some oth; failwith "fsubst''") arglst formlst;
    (match body with
	   | Blocking (FopAsgn (nam', funexpr)) :: [] -> funexpr
           | Seq(lbl, lst) :: [] -> Seq(lbl, DeclReg([Id nam], [AnyRange (hi', lo')], Deflt) :: lst)
	   | oth -> unhand := Some (Itmlst oth); failwith "fsubst''")
| FunDecl (nam,
     Itmlst [AnyRange (hi, lo)],
     FunGuts (formlst',
      [Blocking
        (FopAsgn (nam', funexpr))])) ->
  let formlst = List.map (function
	   | PortFront (PortItemFront2 (dir, Deflt, rng), ItemAsgn(Id _ as formal)) -> formal
           | ItemAsgn (Id _ as formal) -> formal
	   | oth -> unhand := Some oth; failwith "formal") formlst' in
  printf "fsubst2 nam: %s, len(arglst) = %d, len(formlst) = %d\n" nam (List.length arglst) (List.length formlst);
  List.iter2 (fun actual -> function
    | Id _ as formal -> Hashtbl.add attr.subst formal actual
    | oth -> unhand := Some oth; failwith "fsubst'''") arglst formlst;
  funexpr
  | oth -> unhand := Some oth; failwith "fsubst"

and fsubst attr arglst id = match Hashtbl.find_opt attr.subst (FunRef (id, [])) with
  | Some (FunDecl _ as f) ->
     recurs2 attr (fsubst' attr arglst f)
  | Some (AutoFunDecl _ as f) ->
     recurs2 attr (fsubst' attr arglst f)
  | Some oth -> unhand := Some oth; failwith "FunRef"
  | None -> failwith ("No definition for function: "^id)

and tsubst' attr arglst = function
| TaskDecl (nam, Deflt, TFBody (formlst, body), Deflt) ->
  printf "nam: %s, len(arglst) = %d, len(formlst) = %d\n" nam (List.length arglst) (List.length formlst);
  dbgact := arglst;
  dbgform := formlst;
  List.iter2 (fun actual -> function
		  | TF_port_decl ((In|Out), [AnyRange (hi, lo)], [TF_variable (Id _ as formal, Deflt, Deflt, Deflt)]) -> Hashtbl.add attr.Source_text_rewrite.subst formal actual
		  | TF_port_decl ((In|Out), [], [TF_variable (Id _ as formal, Deflt, Deflt, Deflt)]) -> Hashtbl.add attr.Source_text_rewrite.subst formal actual
    | oth -> unhand := Some oth; failwith "tsubst'") arglst formlst;
    Seq(nam, body)
| TaskDecl (nam, Deflt, FunGuts (formlst', body), Deflt) ->
  let formlst = List.map (function
           | PortFront (PortItemFront2 ((In|Out), Deflt, rng), ItemAsgn (Id _ as formal)) -> formal
           | ItemAsgn (Id _ as formal) -> formal
	   | oth -> unhand := Some oth; failwith "formal") formlst' in
  printf "nam: %s, len(arglst) = %d, len(formlst) = %d\n" nam (List.length arglst) (List.length formlst);
  dbgact := arglst;
  dbgform := formlst;
  Seq(nam, body)
| oth -> unhand := Some oth; failwith "tsubst"

and tsubst attr arglst id = recurs2 attr (match Hashtbl.find_opt attr.subst (TaskRef (id,[])) with
  | Some (TaskDecl _ as f) -> tsubst' attr arglst f
  | Some oth -> unhand := Some oth; failwith "TaskRef"
  | None -> failwith ("No definition for task: "^id))

and dead_code attr = function
| Pling rhs -> dead_code attr rhs
| Less (Intgr n, Intgr m) -> if n < m then Always_true else Always_false
| LtEq (Intgr n, Intgr m) -> if n <= m then Always_true else Always_false
| Equals (Intgr n, Intgr m) -> if n = m then Always_true else Always_false
| NotEq (Intgr n, Intgr m) -> if n <> m then Always_true else Always_false
| GtEq (Intgr n, Intgr m) -> if n >= m then Always_true else Always_false
| Greater (Intgr n, Intgr m) -> if n > m then Always_true else Always_false
| Or2 (lhs, rhs) -> dead_code_or attr (dead_code attr lhs, dead_code attr rhs)
| And2 (lhs, rhs) -> dead_code_and attr (dead_code attr lhs, dead_code attr rhs)
| Id _ as id -> (match Hashtbl.find_opt attr.Source_text_rewrite.subst id with Some x -> dead_code_id attr x | None -> Undecidable)
| Intgr n -> if n <> 0 then Always_true else Always_false
| (TildeAnd _ | RedAnd _ | RedOr _ | IdArrayed2 _ | GtEq _ | Equals _ | Less _ | LtEq _ | NotEq _ | Greater _) -> Undecidable
(*
| oth -> unhand := Some oth; failwith "dead_code"
*)
| _ -> Undecidable

and dead_code_id attr = function
| oth -> Undecidable

and dead_code_and attr = function
| (Always_false,_)
| (_, Always_false) -> Always_false
| _ -> Undecidable

and dead_code_or attr = function
| (Always_true,_)
| (_, Always_true) -> Always_true
| _ -> Undecidable

and elabeval attr = function
| Intgr n -> n
| Number (_,_,n,_) -> n
| Id s as id -> elabeval attr (match Hashtbl.find_opt attr.subst id with Some x -> x | None -> failwith ("Not found: "^s))
| Add (lhs, rhs) -> elabeval attr lhs + elabeval attr rhs
| Sub (lhs, rhs) -> elabeval attr lhs - elabeval attr rhs
| Mult (lhs, rhs) -> elabeval attr lhs * elabeval attr rhs
| Div (lhs, rhs) -> let divisor = elabeval attr rhs in if divisor = 0 then elabeval attr lhs else elabeval attr lhs / divisor
| And (lhs, rhs) -> (elabeval attr lhs) land (elabeval attr rhs)
| And2 (lhs, rhs) -> (if elabeval attr lhs <> 0 then 1 else 0) land (if elabeval attr rhs <> 0 then 1 else 0)
| Or (lhs, rhs) -> (elabeval attr lhs) lor (elabeval attr rhs)
| Or2 (lhs, rhs) -> (if elabeval attr lhs <> 0 then 1 else 0) lor (if elabeval attr rhs <> 0 then 1 else 0)
| Xor (lhs, rhs) -> (elabeval attr lhs) lxor (elabeval attr rhs)
| Less (lhs, rhs) -> if elabeval attr lhs < elabeval attr rhs then 1 else 0
| LtEq (lhs, rhs) -> if elabeval attr lhs <= elabeval attr rhs then 1 else 0
| Equals (lhs, rhs) -> if elabeval attr lhs = elabeval attr rhs then 1 else 0
| Equals3 (lhs, rhs) -> if elabeval attr lhs = elabeval attr rhs then 1 else 0
| Greater (lhs, rhs) -> if elabeval attr lhs > elabeval attr rhs then 1 else 0
| Query (lhs, rhs, rhs') -> if elabeval attr lhs <> 0 then elabeval attr rhs else  elabeval attr rhs'
| StarStar (lhs, rhs) -> int_of_float(float_of_int (elabeval attr lhs) ** float_of_int(elabeval attr rhs))
| UMinus rhs -> - (elabeval attr rhs)
| UPlus rhs -> + (elabeval attr rhs)
| Shiftl (lhs, rhs) -> elabeval attr lhs lsl elabeval attr rhs
| Shiftr (lhs, rhs) -> elabeval attr lhs lsr elabeval attr rhs
| Repl (Number (_, _, n, _), arg::[]) -> elabeval attr arg (* placeholder *)
| Sys ("$clog2", x) -> clog2 (elabeval attr x)
| RedAnd x -> elabeval attr x (* placeholder *)
| RedOr x -> elabeval attr x (* placeholder *)
| RedXor x -> elabeval attr x (* placeholder *)
| Tilde x -> elabeval attr x (* placeholder *)
| Pling x -> elabeval attr x (* placeholder *)
| TildeAnd x -> elabeval attr x (* placeholder *)
| TildeOr x -> elabeval attr x (* placeholder *)
| Expression x -> elabeval attr x
| PackageBody (pkg, [id]) -> elabeval attr id (* placeholder *)
| oth -> unhand := Some oth; failwith "elabeval"

let newpth lbl = function
  | None -> Some lbl
  | Some old -> Some (old^"."^lbl)

let dot = function
  | ("", nam) -> nam
  | (old, nam) -> old^"."^nam

let npth attr nam =
  let pth = attr.Source_text_rewrite.pth in
  let npth' = dot (pth, nam) in
  if pth <> "" then Hashtbl.replace attr.Source_text_rewrite.subst (Id nam) (Id npth');
  npth'

let rec recurs3 (attr: Source_text_rewrite.attr) = function
  | If2(cond, if_clause, else_clause) -> (match dead_code attr cond with
     | Always_true -> recurs3 attr if_clause
     | Always_false -> recurs3 attr else_clause
     | Undecidable ->
      CaseStart (CaseStart1 (cond),
       (CaseStmt ([Number(2,1,1,"")],
         recurs3 attr if_clause :: []) ::
        CaseStmt ([],
         recurs3 attr else_clause :: []) ::
        [])))
  | If1(cond, if_clause) ->
      CaseStart (CaseStart1 (recurs3 attr cond),
       (CaseStmt ([Number(2,1,1,"")],
         recurs3 attr if_clause :: []) ::
         []))
  | Seq (lbl, lst) -> Seq (dot(attr.pth,lbl), let oldp = attr.pth in List.map (recurs3 {attr with pth=dot(oldp,lbl)}) lst)
  | NetDecl (arg1, lst) -> NetDecl (arg1, List.map (function
				     | InitSig (Id nam, arg2) -> let nam = npth attr nam in InitSig(Id nam, recurs3 attr arg2)
                                     | Id nam -> let nam = npth attr nam in Id nam
				     | oth' -> failwith (Source_text_rewrite.getstr oth')) lst)
  | Id _ as id -> (match Hashtbl.find_opt attr.subst id with None -> id | Some exp -> recurs3 attr exp)
  | oth -> Source_text_rewrite.descend' {attr with fn=recurs3} oth

let rec simplify (attr: Source_text_rewrite.attr) = function
| Add(Intgr lft, Intgr rght) -> Intgr (lft+rght)
| Add(x, Intgr 0) -> x
| Add(Intgr 0, x) -> x
| Sub(Intgr lft, Intgr rght) -> Intgr (lft-rght)
| Sub(x, Intgr 0) -> x
| Mult(Intgr lft, Intgr rght) -> Intgr (lft*rght)
| Mult(x, Intgr 1) -> x
| Mult(_, Intgr 0) -> Intgr 0
| Mult(Intgr 0, _) -> Intgr 0
| Mult(Intgr 1, x) -> x
| Add(lft,rght) -> Add(simplify attr lft, simplify attr rght)
| Sub(lft,rght) -> Sub(simplify attr lft, simplify attr rght)
| And2(Intgr lft, Intgr rght) -> Intgr (lft land rght)
| And2(x, Intgr 1) -> x
| And2(_, Intgr 0) -> Intgr 0
| And2(Intgr 0, _) -> Intgr 0
| And2(Intgr 1, x) -> x
| Or2(Intgr lft, Intgr rght) -> Intgr (lft lor rght)
| Or2(x, Intgr 0) -> x
| Or2(Intgr 0, x) -> x
| Or2(lft,rght) -> Or2(simplify attr lft, simplify attr rght)
| Query(Intgr cond, lft, rght) -> if cond <> 0 then simplify attr lft else simplify attr rght
| oth -> Source_text_rewrite.descend' {attr with fn=simplify} oth

let sub' x =
   let subst' = Hashtbl.create 255 in
   recurhash := subst';
   let pass1 = recurs1 {fn=recurs1; subst=subst'; pth=""} x in
   let pass2 = recurs2 {fn=recurs2; subst=subst'; pth=""} pass1 in
   let pass3 = recurs3 {fn=recurs3; subst=subst'; pth=""} pass2 in
   let attr = {Source_text_rewrite.fn=simplify; subst=subst'; pth=""} in
   let pass4 = simplify attr (simplify attr (simplify attr (simplify attr (simplify attr (pass3))))) in
   dbgsub := pass4 :: !dbgsub;
   dbgsubst := [];
   Hashtbl.iter (fun k x -> dbgsubst := (k,x) :: !dbgsubst) subst';
   pass4

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

let id_ix = ref 1000
let wire_lst = ref []
let typhist = ref []

let newnam () = 
  incr id_ix;
  "$Id$"^string_of_int !id_ix

let exists_wire bufh typhash options nam signage =
  wire_lst := (options,nam,signage) :: !wire_lst;
(*
  if nam = "mem_rdata_latched" then failwith nam;
*)
  if List.filter (function Wire_stmt (_, id) -> id=nam | _ -> false) !(bufh.w) <> [] then
    output_string logf ("Warning: "^nam^" is already defined, could be obsolete syntax\n")
  else
    bufh.w := Wire_stmt(options, nam) :: !(bufh.w);
  let typ = Hashtbl.find_opt typhash nam in
  typhist := (nam,typ) :: !typhist;
  match typ with
    | Some (MaybePort (ix,Vsigtyp,dir)) -> update typhash (Id nam) (MaybePort (ix, signage, dir))
    | Some (MaybePort (ix,(Unsigned|Signed),dir)) -> update typhash (Id nam) (MaybePort (ix, signage, dir))
    | Some (MaybePort (ix,_,dir)) -> ()
    | Some Unsigned -> ()
    | Some Unsigned_vector _ -> ()
    | None -> update typhash (Id nam) signage
    | oth -> unhand_typ := Some oth; failwith "exists_wire"

let addwire bufh typhash = function
| (options, Id nam, (Signed|Signed_vector _ as typ)) ->
  output_string logf ("add signed wire: "^nam^", "^vtyp typ^"\n");
  exists_wire bufh typhash options nam typ;
| (options, Id nam, (Unsigned|Unsigned_vector _ as typ)) ->
  output_string logf ("add unsigned wire: "^nam^", "^vtyp typ^"\n");
  exists_wire bufh typhash options nam typ;
| (options, Id nam, (Vlocal _ as typ)) ->
  output_string logf ("add localparam: "^nam^", "^vtyp typ^"\n");
  exists_wire bufh typhash options nam typ;
| (_, oth, _) -> failwith ("Argument "^(Source_text_rewrite.getstr oth)^" of addwire must be wire Id")

let rec memsiz typhash = function
| [] -> []
| (first',last') :: tl ->
  let (first,last) = if first' > last' then (last',first') else (first',last') in
  let off' = ceval typhash first in (off', ceval typhash last - off' + 1) :: memsiz typhash tl

let addmem bufh typhash first_last_lst wid' = function
| Id mem ->
  if List.filter (function Memory_stmt39 (_, id) -> id=mem | _ -> false) !(bufh.w) <> [] then
    print_endline ("Warning: "^mem^" is already defined, could be obsolete syntax")
  else
    begin
    dbgmem' := Some (mem, first_last_lst);
    let (off', siz') = List.split (memsiz typhash first_last_lst) in
    let rec tot = function [] -> 1 | hd::tl -> hd * tot tl in
    let rec off = function [] -> 0 | hd::tl -> hd + tot tl in
    let tot' = tot siz' in
    let options = Memory_optionsoffset (off off') :: Memory_optionssize (tot') :: Memory_optionswidth wid' :: [] in
    update typhash (Id mem) (Vmem {off=off';siz=siz';wid=wid';tot=tot'});
    bufh.w := Memory_stmt39 (options, mem) :: !(bufh.w)
    end
| oth -> failwith ("Argument "^(Source_text_rewrite.getstr oth)^" of addmem must be memory Id")

let vsel' n = function Id lhs -> [Sigspec90 (lhs, n)] | oth -> unhand := Some oth; failwith "vsel'"

let rec obin64 w n = 
  (if w > 1 then obin64 (w-1) (Int64.shift_right n 1) else "")^Int64.to_string (Int64.logand n 1L)

let str_to_bin s = let l = String.length s in 
  (l*8), String.concat "" (List.init l (fun ix -> obin 8 (int_of_char s.[ix])))

let rec tran' = function
   | Id id -> TokID id :: []
   | Number (b,w,n,_) -> TokVal (sprintf "%d'%s" w (obin w n)) :: []
   | Intgr n -> TokVal (sprintf "32'%s" (obin 32 n)) :: []
   | Float f -> TokVal ("64'"^obin64 64 (Int64.bits_of_float f)) :: []
   | String s -> TokVal (let sz, b = str_to_bin s in string_of_int sz^"'"^b) :: []
   | Atom "default" -> []
(*
   TokVal ("64'"^obin64 64 (Int64.bits_of_float f)) :: []
   | Concat lst -> List.flatten (List.map tran' lst)
*)
   | ExprOKL lst -> Sigspec92 (List.flatten (List.map tran' lst)) :: []
   | GenBlock lst -> Sigspec92 (List.flatten (List.map tran' lst)) :: []
   | IdArrayed2 (Id arr, (Number (_, _, n, _)|Intgr n)) -> vsel' n (Id arr)
   | IdArrayedColon (Intgr _ as n, _, _) -> tran' n
   | IdArrayedColon (Id conn, (Intgr hi | Number(_,_,hi,_)), (Intgr lo | Number(_,_,lo,_))) -> Sigspecrange(conn,hi,lo) :: []
   | IdArrayedPlusColon (Id conn, (Intgr lo | Number(_,_,lo,_)), (Intgr wid | Number(_,_,wid,_))) -> Sigspecrange(conn,lo+wid-1,lo) :: []
   | oth -> unhand := Some oth; failwith "tran'"

let newid bufh typhash wid =
  let nam = newnam() in
  let rslt = Id nam in
  addwire bufh typhash ([Wire_optionswidth wid], rslt, Unsigned_vector(Intgr (wid-1),Intgr 0));
  rslt

let rec parm_map typhash = function  
  | Param (nam, (Intgr n | Number (_, _, n, _)), []) ->
      update typhash (Id nam) (Vint n);
      TokParam([TokID ("\\"^nam)], [TokInt n])
  | Param (nam, (Intgr n | Number (_, _, n, _)), AnyRange (left, rght) :: []) ->
      update typhash (Id nam) (Vint n);
      TokParam([TokID ("\\"^nam)], [TokInt n])
  | CellParamItem2 (nam, (Intgr n | Number (_, _, n, _))) ->
      update typhash (Id nam) (Vint n); 
      TokParam([TokID ("\\"^nam)], [TokInt n])
  | CellParamItem2 (nam, (Id _ as s)) -> let n = ceval typhash s in
      TokParam([TokID ("\\"^nam)], [TokInt n])
  | CellParamItem2 (nam, String s) ->
      TokParam([TokID ("\\"^nam)], [TokStr s])
      (*
  | CellParamItem1 (nam, s) ->
      update typhash nam (match Hashtbl.find_opt typhash s with Some x -> x | None -> Unsigned);
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
      update typhash (Id nam) (Vint n);
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
        Cell_stmt (typ, inst, [],
        List.rev_map (parm_map typhash) params @
        List.rev (List.mapi (fun ix -> function
		   | CellPinItem2 (pin, Intgr n) -> TokConn([TokID(pin)], tran' (Number(2, 32, n, "")))
		   | CellPinItem2 (pin, conn) -> TokConn([TokID(pin)], tran' conn)
		   | CellPinItemImplied (pin) -> TokConn([TokID(pin)], tran' (Id pin))
		   | CellPinItemNC (pin) -> TokConn([TokID(pin)], tran' (newid bufh typhash 1))
                   | Id _ as conn -> TokConn([TokID("$"^string_of_int (ix+1))], tran' conn)
                   | oth -> unhand := Some oth; failwith "inst_arg") pinlst))

let memtest01 =
  [("\\memtest01",
    [Module12 ("\\memtest01",
      [Wire_stmt ([Wire_optionswidth 4], "$ADDR[3:0]");
       Wire_stmt ([Wire_optionswidth 8], "$DATA[7:0]");
       Wire_stmt ([Wire_optionswidth 8], "$EN[7:0]");
       Wire_stmt ([Wire_optionswidth 8], "$5_DATA");
       Wire_stmt ([Wire_optionswidth 4], "$3_ADDR");
       Wire_stmt ([Wire_optionswidth 8], "$3_DATA");
       Wire_stmt ([Wire_optionswidth 8], "$3_EN");
       Wire_stmt ([Wire_optionsinput 1], "\\clk");
       Wire_stmt ([Wire_optionswidth 4; Wire_optionsinput 5], "\\rd_addr");
       Wire_stmt ([Wire_optionswidth 8; Wire_optionsoutput 6], "\\rd_value");
       Wire_stmt ([Wire_optionswidth 4; Wire_optionsinput 3], "\\wr_addr");
       Wire_stmt ([Wire_optionsinput 2], "\\wr_en");
       Wire_stmt ([Wire_optionswidth 8; Wire_optionsinput 4], "\\wr_value");
       Memory_stmt39 ([Memory_optionswidth 8; Memory_optionssize 16],
        "\\data");
       Cell_stmt ("$memrd", "$15", [],
        [TokParam ([TokID "\\ABITS"], [TokInt 4]);
         TokParam ([TokID "\\CLK_ENABLE"], [TokInt 0]);
         TokParam ([TokID "\\CLK_POLARITY"], [TokInt 0]);
         TokParam ([TokID "\\MEMID"], [TokStr "\\\\data"]);
         TokParam ([TokID "\\TRANSPARENT"], [TokInt 0]);
         TokParam ([TokID "\\WIDTH"], [TokInt 8]);
         TokConn ([TokID "\\ADDR"], [TokID "\\rd_addr"]);
         TokConn ([TokID "\\CLK"], [TokVal "1'x"]);
         TokConn ([TokID "\\DATA"], [TokID "$5_DATA"]);
         TokConn ([TokID "\\EN"], [TokVal "1'x"])]);
       Cell_stmt ("$memwr", "$13", [],
        [TokParam ([TokID "\\ABITS"], [TokInt 4]);
         TokParam ([TokID "\\CLK_ENABLE"], [TokInt 0]);
         TokParam ([TokID "\\CLK_POLARITY"], [TokInt 0]);
         TokParam ([TokID "\\MEMID"], [TokStr "\\\\data"]);
         TokParam ([TokID "\\PRIORITY"], [TokInt 1038]);
         TokParam ([TokID "\\WIDTH"], [TokInt 8]);
         TokConn ([TokID "\\ADDR"], [TokID "$3_ADDR"]);
         TokConn ([TokID "\\CLK"], [TokVal "1'x"]);
         TokConn ([TokID "\\DATA"], [TokID "$3_DATA"]);
         TokConn ([TokID "\\EN"], [TokID "$3_EN"])]);
       Proc_stmt ("$proc", [],
        [Assign_stmt67 ([Sigspec92 []], [Sigspec92 []]);
         Assign_stmt67 ([Sigspec92 []], [Sigspec92 []]);
         Assign_stmt67 ([Sigspec92 []], [Sigspec92 []]);
         Assign_stmt67 ([TokID "$ADDR[3:0]"], [TokVal "4'xxxx"]);
         Assign_stmt67 ([TokID "$DATA[7:0]"], [TokVal "8'xxxxxxxx"]);
         Assign_stmt67 ([TokID "$EN[7:0]"], [TokVal "8'00000000"]);
         Switch_stmt ([TokID "\\wr_en"], [], [],
          [Switch_bodycase ([TokVal "1'1"], [],
            [Assign_stmt67 ([TokID "$ADDR[3:0]"], [TokID "\\wr_addr"]);
             Assign_stmt67 ([TokID "$DATA[7:0]"], [TokID "\\wr_value"]);
             Assign_stmt67 ([TokID "$EN[7:0]"], [TokVal "8'11111111"])]);
           Switch_bodycase ([], [], [])])],
        [Sync_list69 ([TokPos], [TokID "\\clk"], [],
          [TokUpdate ([TokID "$3_EN"], [TokID "$EN[7:0]"]);
           TokUpdate ([TokID "$3_DATA"], [TokID "$DATA[7:0]"]);
           TokUpdate ([TokID "$3_ADDR"], [TokID "$ADDR[3:0]"])])]);
       Conn_stmt96 ([TokID "\\rd_value"], [TokID "$5_DATA"])])])]

let buflst = ref []

(* certain primitives go wrong if we equalise argument widths *)

let width_mismatch_not_ok = function
| "eq" -> false
| "shl" -> false
| "shr" -> false
| oth -> true

let rec asgnexpr' bufh typhash wid = function
  | (Number _ | Intgr _) as x -> x
  | Add (lhs, rhs) -> dyadic bufh typhash wid "add" [lhs;rhs] "ABY"
  | Sub (lhs, rhs) -> dyadic bufh typhash wid "sub" [lhs;rhs] "ABY"
  | Mult (lhs, rhs) -> dyadic bufh typhash wid "mul" [lhs;rhs] "ABY"
  | Div (lhs, rhs) -> dyadic bufh typhash wid "div" [lhs;rhs] "ABY"
  | Mod (lhs, rhs) -> dyadic bufh typhash wid "mod" [lhs;rhs] "ABY"
  | StarStar (lhs, rhs) -> dyadic bufh typhash wid "pow" [lhs;rhs] "ABY"
  | And (lhs, rhs) -> dyadic bufh typhash wid "and" [lhs;rhs] "ABY"
  | And2 (lhs, rhs) -> dyadic bufh typhash wid "logic_and" [lhs;rhs] "ABY"
  | Or (lhs, rhs) -> dyadic bufh typhash wid "or" [lhs;rhs] "ABY"
  | Or2 (lhs, rhs) -> dyadic bufh typhash wid "logic_or" [lhs;rhs] "ABY"
  | Xor (lhs, rhs) -> dyadic bufh typhash wid "xor" [lhs;rhs] "ABY"
  | Less (lhs, rhs) -> dyadic bufh typhash wid "lt" [lhs;rhs] "ABY"
  | LtEq (lhs, rhs) -> dyadic bufh typhash wid "le" [lhs;rhs] "ABY"
  | Equals (lhs, rhs) -> dyadic bufh typhash wid "eq" [lhs;rhs] "ABY"
  | NotEq (lhs, rhs) -> dyadic bufh typhash wid "ne" [lhs;rhs] "ABY"
  | NotEq3 (lhs, rhs) -> dyadic bufh typhash wid "ne" [lhs;rhs] "ABY"
  | GtEq (lhs, rhs) -> dyadic bufh typhash wid "ge" [lhs;rhs] "ABY"
  | Greater (lhs, rhs) -> dyadic bufh typhash wid "gt" [lhs;rhs] "ABY"
  | Shiftl (lhs, rhs) -> dyadic bufh typhash wid "shl" [lhs;rhs] "ABY"
  | Shiftr (lhs, rhs) -> dyadic bufh typhash wid "shr" [lhs;rhs] "ABY"
  | Shiftr3 (lhs, rhs) -> dyadic bufh typhash wid "sshr" [lhs;rhs] "ABY"
  | Id s as x -> let oldw = width typhash x in if oldw < wid then Concat (Number(2,wid-oldw,0,"") :: Id s :: []) else Id s
  | Expression x -> asgnexpr bufh typhash x
  | UPlus x -> asgnexpr bufh typhash x
  | UMinus rhs -> let lhs = Number(2, wid, 0, "") in dyadic bufh typhash wid "sub" [lhs;rhs] "ABY"
  | TildeAnd expr -> unary bufh typhash wid "tilde_and" expr "AY"
  | TildeOr expr -> unary bufh typhash wid "tilde_or" expr "AY"
  | CaretTilde expr -> unary bufh typhash wid "caret_tilde" expr "AY"
  | Tilde expr -> unary bufh typhash wid "not" expr "AY"
  | Pling expr -> unary bufh typhash wid "logic_not" expr "AY"
  | RedAnd expr -> unary bufh typhash wid "reduce_and" expr "AY"
  | RedOr expr -> unary bufh typhash wid "reduce_or" expr "AY"
  | RedXor expr -> unary bufh typhash wid "reduce_xor" expr "AY"
  | Repl ((Intgr n|Number (_, _, n, _)), arg :: []) -> GenBlock (List.init n (fun _ -> asgnexpr bufh typhash arg))
  | IdArrayedColon (id, hi, lo) as x -> x
  | IdArrayedPlusColon (id, hi, lo) as x -> x
  | IdArrayed2 (Id id, sel) when is_mem typhash id -> memrd bufh typhash (mem_opt typhash id) id sel
  | IdArrayed2 (id, (Intgr _ | Number _)) as x -> x
  | IdArrayed2 (inner, expr) -> shiftx bufh typhash wid inner expr
  | Query (cond', ctrue', cfalse') -> ternary bufh typhash wid "mux" [cfalse'; ctrue'; cond'] "absy"
  | Sys ("$signed", rhs) -> asgnexpr bufh typhash rhs
  | Sys ("$unsigned", rhs) -> asgnexpr bufh typhash rhs
  | GenBlock _ as x -> x
  | ExprOKL _ as x -> x
  | Float _ as f -> f
  | String _ as s -> s
(*
  | Concat lst -> String.concat " & " (List.map (asgnexpr bufh typhash) lst)
  | StarStar (lhs, rhs) -> asgnexpr bufh typhash lhs ^ " ** " ^ asgnexpr bufh typhash rhs
  | UMinus (rhs) -> " - " ^ asgnexpr bufh typhash rhs
  | Unsigned expr -> "unsigned("^asgnexpr bufh typhash expr^")"
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
*)
  | oth -> unhand := Some oth; failwith "asgnexpr bufh"

and asgnexpr bufh typhash x = asgnexpr' bufh typhash (width typhash x) x

and signlst func typhash sgnlst =
  let sgn' = List.mapi (fun ix itm -> match func,ix,signof typhash itm with
    | "shr",1,_ -> 0 (* certain function/operand combinations cannot be signed in yosys *)
    | "shl",1,_ -> 0
    | "sub",_,_ -> 0
    | _,_,Signed -> 1
    | _,_,Unsigned -> 0
    | _ -> failwith "signing") sgnlst in
  (* if either input is unsigned, the whole thing becomes unsigned *)
  match sgn' with
  | [] -> []
  | s :: [] -> sgn'
  | a :: b :: [] when a=b -> sgn'
  | 0 :: b :: [] -> 0 :: 0 :: []
  | a :: 0 :: [] -> 0 :: 0 :: []
  | a :: b :: c :: [] when a=b && b=c -> sgn'
  | 0 :: b :: c :: [] -> 0 :: 0 :: 0 :: []
  | a :: 0 :: c :: [] -> 0 :: 0 :: 0 :: []
  | a :: b :: 0 :: [] -> 0 :: 0 :: 0 :: []
  | oth -> failwith "signlst"

and signparm = List.map (fun (sgn,itm) -> CellParamItem2 (itm^"_SIGNED", Number (10, 32, sgn, "")))

and addprim bufh typhash typ params args templ =
  output_string logf ("addprim: "^typ^"\n");
  let args' = List.map (buffer'' bufh typhash) args in
  let wid' ix arg =
     let w = width typhash arg in
     let u = Char.uppercase_ascii templ.[ix] in
     if u = templ.[ix] || ix = 0 then CellParamItem2 (((if u = templ.[ix] then String.make 1 u^"_" else "")^"WIDTH"), Number(10, 32, w, "")) :: [] else [] in
  let conn' ix itm = CellPinItem2(String.make 1 (Char.uppercase_ascii templ.[ix]), itm) in
  let args'' = match args' with
    | a :: b :: y :: tl when width_mismatch_not_ok typ ->
      let w0 = width typhash a and w1 = width typhash b in
      if w0 < w1 then ExprOKL (Number(2,w1-w0,0,"") :: a :: []) :: b :: y :: tl
      else if w0 > w1 then a :: ExprOKL (Number(2,w0-w1,0,"") :: b :: []) :: y :: tl
      else a :: b :: y :: tl
    | a :: b :: tl -> args'
    | a :: tl -> args'
    | oth -> unhand_lst := oth; failwith "addprim" in
  let widths = List.flatten (List.mapi wid' args'') in
  let signed = if List.length widths > 1 then params@widths else widths in
  dumpi bufh typhash (typ, [Itmlst signed], (InstNameParen1 (newnam(), Itmlst (List.mapi conn' args'') :: []) :: []))

and unary bufh typhash wid func expr pnam =
  let rhs = asgnexpr bufh typhash expr and rslt = newid bufh typhash wid in
  let signing = List.combine (signlst func typhash [rhs]) ["A"] in
  addprim bufh typhash func (signparm signing) [rhs;rslt] pnam;
  rslt

and dyadic bufh typhash wid func args pnam =
  let rslt = newid bufh typhash wid in
  let signing = List.combine (signlst func typhash args) ["A";"B"] in
  addprim bufh typhash func (signparm signing) (args@[rslt]) pnam;
  rslt

and ternary bufh typhash wid func args pnam =
  let rslt = newid bufh typhash wid in
  let signing = List.combine (signlst func typhash args) ["A";"B";"S"] in
  addprim bufh typhash func (signparm signing) (args@[rslt]) pnam;
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

and memwr bufh typhash options id addr data en =
  let abits = clog2 options.tot in
  let addr' = newid bufh typhash abits in
  let data' = newid bufh typhash options.wid in
  let en' = newid bufh typhash options.wid in
  let params = List.rev (List.sort compare (List.map (fun (parm,value) -> CellParamItem2 (parm, value)) [
  "ABITS", Intgr abits ;
  "CLK_ENABLE", Intgr 0 ;
  "CLK_POLARITY", Intgr 0 ;
  "MEMID", String ("\\\\"^id) ;
  "PRIORITY", Intgr 1038 ;
  "WIDTH", Intgr options.wid ;
  ])) in
  let pins = "ADDR" :: "CLK" :: "DATA" :: "EN" :: [] in
  let args = addr' :: Number(2,1,0,"") :: data' :: en' :: [] in
  let conn' ix arg = let itm = asgnexpr bufh typhash arg in CellPinItem2(List.nth pins ix, itm) in
  dumpi bufh typhash ("memwr", [Itmlst params], (InstNameParen1 (newnam(), Itmlst (List.mapi conn' args) :: []) :: []));
  TokUpdate (tran' en', tran' en) ::
  TokUpdate (tran' data', tran' data) ::
  TokUpdate (tran' addr', tran' addr) :: []

and shiftx bufh typhash wid lhs rhs = dyadic bufh typhash wid "shiftx" [lhs;rhs] "ABY"

and dumpi bufh typhash (typ, params, lst) = List.iter (function
        | InstNameParen1 (inst, Itmlst pins :: []) ->
	    bufh.i := (instance_template bufh typhash ("$"^typ) (match params with Itmlst lst :: _ -> lst | _ -> []) inst pins) :: !(bufh.i)
        | InstNameParen2 (inst, InstRange(lft,rght) :: []) ->
	    bufh.i := (instance_template bufh typhash ("$"^typ) (match params with Itmlst lst :: _ -> lst | _ -> []) inst []) :: !(bufh.i)
        | Id _ as id -> addwire bufh typhash ([], id, Unsigned)
        | oth -> unhand := Some oth; failwith "InstDecl'") lst

and buffer bufh typhash wid = function
| ExprOKL lst -> ExprOKL (List.map (fun itm -> buffer'' bufh typhash itm) lst)
| Concat lst -> ExprOKL (List.map (fun itm -> buffer'' bufh typhash itm) lst)
| IdArrayed2 (IdArrayed2 (Id mem, addr) as inner, expr) -> shiftx bufh typhash wid (asgnexpr bufh typhash inner) expr
| IdArrayedColon (id, hi, lo) -> IdArrayedColon (buffer'' bufh typhash id, hi, lo)
| IdArrayedPlusColon (id, lo, wid') -> shiftx bufh typhash (ceval typhash wid') (buffer'' bufh typhash id) (buffer'' bufh typhash lo)
| x -> record x (asgnexpr' bufh typhash wid x)

and buffer'' bufh typhash x = record x (buffer bufh typhash (width typhash x) x)

and record x y = if x <> y then buflst := (x,y) :: !buflst; y

(*
let sel_expr typhash x = match simplify attr x with Intgr _ -> "'0'" | oth -> vexpr typhash oth
*)

let vdir ix = function
  | In -> Wire_optionsinput ix
  | Out -> Wire_optionsoutput ix
  | Inout -> Wire_optionsinout ix
  | Deflt -> Wire_optionsinvalid
  | oth -> unhand := Some oth; failwith "vdir"

let array_port typhash ix hi lo dir nam =
  output_string logf (nam^": array port "^string_of_int ix^" ["^string_of_int (ceval typhash hi)^" : "^string_of_int (ceval typhash lo)^"]\n");
  update typhash (Id nam) (MaybePort (ix, Unsigned_vector(hi,lo), dir))

let maybe_port typhash nam ix dir typ =
  output_string logf (nam^": port "^string_of_int ix^"\n");
  update typhash (Id nam) (MaybePort (ix, typ, dir))

let signcnv = function
	| (Deflt,Deflt) -> Unsigned
        | (Atom "signed",Deflt) -> Signed
        | (Deflt,AnyRange(hi,lo)) -> Unsigned_vector(hi,lo)
        | (Atom "signed",AnyRange(hi,lo)) -> Signed_vector(hi,lo)
	| (Deflt,oth) -> failwith "signcnv'"
	| oth, _ -> unhand := Some oth; failwith "signcnv"

let ports typhash ix = function
    | Port ((In|Out|Inout|Deflt) as dir, nam, [], sgn) -> maybe_port typhash nam ix dir (signcnv (sgn, Deflt))
    | Port (PortDir ((In|Out|Inout) as dir, Atom ("wire"|"reg")), nam, [], sgn) -> maybe_port typhash nam ix dir (signcnv (sgn, Deflt))
    | Port ((In|Out|Inout) as dir, nam, AnyRange (hi, lo) :: [], sgn) -> array_port typhash ix hi lo dir nam
    | Port (PortDir ((In|Out|Inout) as dir, Atom ("wire"|"reg")), nam, AnyRange (hi, lo) :: [], sgn) -> array_port typhash ix hi lo dir nam
    | Port ((In|Out|Inout) as dir, nam, AnyRange (hi, lo) :: AnyRange(hi', lo') :: [], sgn) -> array_port typhash ix hi lo dir nam
    | Port ((In|Out|Inout) as dir, nam, AnyRange (hi, lo) :: AnyRange(hi', lo') :: AnyRange(hi'', lo'') :: [], sgn) -> array_port typhash ix hi lo dir nam
    | Port ((In|Out|Inout) as dir, nam, Typ6 (Atom primtyp) :: [], sgn) -> maybe_port typhash nam ix dir Unsigned
    | Port (Deflt, nam, Typ2 (typ_t, PackageBody (pkg, []) :: [], []) :: AnyRange (hi, lo) :: [], sgn) -> update typhash (Id nam) (Vtyp(typ_t));
        array_port typhash ix hi lo Inout nam
(*
    | Port ((In|Out|Inout) as dir, nam, Typ2 (typ_t, [], []) :: AnyRange (hi, lo) :: [], []) -> update typhash (Id nam) (Vtyp(typ_t));
        array_port typhash ix hi lo dir nam
    | Port ((In|Out|Inout) as dir, nam, Typ2 (typ_t, [], []) :: [], []) -> update typhash (Id nam) (Vtyp(typ_t));
        maybe_port typhash nam ix dir
    | Port ((In|Out|Inout) as dir, nam, Typ2 (typ_e, (PackageBody (pkg, [])) :: [], []) :: [], []) -> update typhash (Id nam) (Vpkg(pkg, typ_e));
        maybe_port typhash nam ix dir
    | Port ((In|Out|Inout) as dir, nam, Typ2 (typ_e, (PackageBody (pkg, [])) :: [], []) :: AnyRange (hi, lo) :: [], []) -> update typhash (Id nam) (Unsigned_vector(hi, lo));
        array_port typhash ix hi lo dir nam
    | Dot3 (bus, dir, member) -> update typhash member (Vintf bus);
        sprintf "  wire %s %s \\%s" member bus dir
    | DotBus (bus, dir, member, AnyRange(hi,lo) :: []) -> update typhash member (Vintf bus);
        array_port typhash ix hi lo Inout bus
*)
    | oth -> unhand := Some oth; failwith "component"

let rec parm_generic typhash = function
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
      update typhash (Id nam) (Vint n); 
      sprintf "%24s         : integer := %d" nam n
  | CellParamItem2 (nam, ((Add _|Sub _|Mult _|Div _|Sys _) as x)) ->
      let n = ceval typhash x in
      update typhash (Id nam) (Vint n);
      sprintf "%24s         : integer := %d" nam n
  | CellParamItem3 (nam, Typ1(id_t)) ->
      sprintf "%24s         : type := %s" nam id_t
  | CellParamItem3 (nam, Typ3(id_t, PackageBody (pkg, []) :: [])) ->
      sprintf "%24s         : type := %s" nam id_t
  | Param (nam, Number (_, _, n, _), []) ->
      update typhash (Id nam) (Vint n);
      sprintf "%24s         : integer := %d" nam n
  | PackageParam2 (id_t, nam, [], Id s) ->
      update typhash (Id nam) (Vtyp id_t);
      sprintf "%24s         => %s" id_t s
  | PackageParam2 (grp_e, nam, [PackageBody (pkg, [])], PackageBody (pkg', [Id s])) ->
      update typhash (Id nam) (Venum grp_e);
      sprintf "%24s         => %s" grp_e s
  | PackageParam2 (id_t, nam, [PackageBody (pkg, [])], Number (_, _, n, _)) ->
      update typhash (Id nam) (Vint n);
      sprintf "%24s         => %d" nam n
  | PackageParam2 (id_t, nam, [PackageBody (pkg, [])], AsgnPat [PatMemberDflt (Number (_, _, n, ""))]) ->
      update typhash (Id nam) (Vint n);
      sprintf "%24s         => %d" nam n
  | PackageParam2 (id_t, nam, [PackageBody (pkg, [])], AsgnPat [PatMemberDflt (PackageBody (pkg', [Id id]))]) ->
      update typhash (Id nam) (Vtyp id_t);
      sprintf "%24s         => %s" nam id
  | PackageParam2 (id_t, nam, [PackageBody (pkg, [])], ExprQuote1 (Typ3(id, PackageBody (pkg', []) :: []), expr)) ->
      update typhash (Id nam) (Vtyp id_t);
      sprintf "%24s         => %s" nam id
  | TypParam (nam, Atom typ, []) ->
      update typhash (Id nam) (Vtyp nam); 
      sprintf "%24s         => %s" nam typ
  | TypParam (nam, Id id_t, PackageBody (pkg, []) :: []) ->
      update typhash (Id nam) (Vtyp nam); 
      sprintf "%24s         => %s" nam id_t
  | TypParam (nam, Atom typ, AnyRange (lft, rght) :: []) ->
      update typhash (Id nam) (Unsigned_vector(lft,rght));
      sprintf "%24s         => %s[%s : %s]" nam typ (cexpr typhash lft) (cexpr typhash rght)
  | Param (nam, PackageBody (pkg, [Id id]), []) ->
      update typhash (Id nam) (Vtyp nam);
      sprintf "%24s         => %s" nam id
  | Param (nam, FunRef2 (fn, [PackageBody (pkg, [])], expr :: []), []) ->
      update typhash (Id nam) (Vfun fn);
      sprintf "%24s         => %s" nam (cexpr typhash expr)
  | Param (nam, String s, []) ->
      update typhash (Id nam) (Vstr s);
      sprintf "%24s         => %s" nam s
  | Param (nam, Dot1(lft,rght), []) ->
      update typhash (Id nam) (Vdot);
      sprintf "%24s         => %s.%s" nam (cexpr typhash lft) (cexpr typhash rght)
  | Param (nam, Number (_, _, n, _), AnyRange (left, rght) :: []) ->
      update typhash (Id nam) (Vint n);
      sprintf "%24s         => %d" nam n
  | Param (nam, UMinus (Number (_, _, n, _)), []) ->
      update typhash (Id nam) (Vint (-n));
      sprintf "%24s         => %d" nam (-n)
  | Param (nam, UMinus (Number (_, _, n, _)), AnyRange (lft, rght) :: []) ->
      update typhash (Id nam) (Vint (-n));
      sprintf "%24s         => %d" nam (-n)
  | Param (nam, ((Add _|Sub _|Mult _|Div _|StarStar _ |Sys _|Equals _|Query _) as x), []) ->
      let n = ceval typhash x in
      update typhash (Id nam) (Vint n);
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
  buffer bufh typhash wid' expr

(*
| Concat _ as lst -> bprintf buf' "            %s <= %s; // 385	\n" (vexpr typhash lst) (asgnexpr buf' typhash expr)
| Dot1 (lft, rght) -> bprintf buf' "            %s.%s <= %s; // 386	\n" (vexpr typhash lft) (vexpr typhash rght) (asgnexpr buf' typhash expr)
| IdArrayed2 (Id id, ix) -> bprintf buf' "            %s(%s) <= %s; // 387	\n" id (vexpr typhash ix) (asgnexpr buf' typhash expr)
| IdArrayedColon (Id id, hi, lo) -> bprintf buf' "            %s[%s : %s] <= %s; // 388	\n" id (vexpr typhash hi) (vexpr typhash lo) (asgnexpr buf' typhash expr)
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
      | TFBody (decls, lst) -> List.iter (stmt_clause buf' typhash) lst
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
    *)

let range typhash = function
    | Id nam, AnyRange(hi,lo) ->
        let wid = ceval typhash hi - ceval typhash lo + 1 in Wire_optionswidth wid
    | _, oth -> unhand := Some oth; failwith "range"

let addwire' bufh typhash nam = function
    | AnyRange(hi,lo) as x ->
      addwire bufh typhash (range typhash (Id nam, x) :: [], Id nam, Unsigned_vector(hi,lo))
    | oth -> unhand := Some oth; failwith "addwire'"

let atom_width = function
| "int" -> 32
| "integer" -> 32
| "longint" -> 64
| "real" -> 64
| oth -> dbgatom := oth; failwith "atom_width"

let portpos typhash nam =
  match Hashtbl.find_opt typhash nam with Some (MaybePort (ix,_,_)) -> ix | _ -> 0

let elabenum typhash nam id_lst = update typhash (Id nam) (Venum nam);
    let strt = ref 0 in List.iter (function
        | Id e -> update typhash (Id e) (Vemember(nam, e, Intgr(!strt)));
	    incr strt
        | EnumInit (e, expr) ->
	    strt := ceval typhash expr;
	    update typhash (Id e) (Vemember(nam, e, Intgr(!strt)));
            incr strt
	| oth -> unhand := Some oth; failwith "TypEnum6") id_lst

let rec restrict' typhash wid nam = function
  | Vemember(s, _, Number(b,w,n,_)) -> Number(b,min w wid,n mod (1 lsl wid),"")
  | Vemember(s, _, Intgr n) -> Number(2,wid,n mod (1 lsl wid),"")
  | Unsigned_vector ((Number _|Intgr _ as hi), (Number _|Intgr _ as lo))
  | Signed_vector ((Number _|Intgr _ as hi), (Number _|Intgr _ as lo)) ->
      let hi' = ceval typhash hi in
      let lo' = ceval typhash lo in
      let wid' = hi' - lo' + 1 in
      IdArrayedColon(Id nam, Intgr(if wid' > wid then lo' + wid-1 else hi'), Intgr(lo'))
  | MaybePort(ix, typ, _) -> restrict' typhash wid nam typ
  | (Signed | Unsigned) -> Id nam
  | Vint n -> Number(2,min 32 wid,n mod (1 lsl wid),"")
  | Vlocal (w', Number(b,w,n,_)) -> Number(b,min w wid,n mod (1 lsl wid),"")
  | oth -> coth := Some oth; failwith "restrict'"

let rec restrict typhash wid = function
  | Number(b,w,n,_) -> Number(b,min w wid,n mod (1 lsl wid),"")
  | Id s -> (match Hashtbl.find_opt typhash s with Some x -> restrict' typhash wid s x | None -> Id s)
  | IdArrayed2 (Id s,ix) -> (match Hashtbl.find_opt typhash s with Some x -> restrict' typhash wid s x | None -> Id s)
  | Atom "default" as x -> x
  | Intgr n -> Number(2,wid,n mod (1 lsl wid),"")
  | ExprOKL lst -> ExprOKL (List.rev (restrict_lst typhash wid (List.rev lst)))
  | String s -> let sz, b = str_to_bin s in Matchmly.widthnum (string_of_int wid^"'b"^String.sub b (sz-wid) wid)
  | oth -> unhand := Some oth; failwith (sprintf "restrict (%d) %s (%d)" wid (Source_text_rewrite.getstr oth) (width typhash oth))

and restrict_lst typhash wid = function
  | [] -> []
  | hd :: tl -> let hdwid = width typhash hd in if hdwid = wid then hd :: [] else if hdwid < wid then hd :: restrict_lst typhash (wid-hdwid) tl else restrict typhash wid hd :: []

let conn_lst  = ref []

let addconn bufh typhash  (nam, expr) =
    let lhsw = width typhash nam in
    let expr' = buffer' bufh typhash expr 0 in
    let expw = width typhash expr' in
    let rhs = restrict typhash lhsw expr' in
    let rhsw = width typhash rhs in
    conn_lst := (lhsw, expw, rhsw, nam, rhs) :: !conn_lst;
    bufh.i := TokConn (tran' nam, tran' rhs) :: !(bufh.i)

let rec decl_template bufh typhash modules pth = function
    | Seq (lbl, lst) -> List.iter (decl_template bufh typhash modules (newpth lbl pth)) lst
    | InstDecl (typ, params, lst) -> List.iter (function
        | (InstNameParen1 _ | InstNameParen2 _) -> ()
        | Id id -> ()
        | oth -> unhand := Some oth; failwith "InstDecl") lst;
    | InstArrayDecl (typ, params, inst, [InstRange(hi,lo)], arglst) -> update typhash inst (InstArray(typ,hi,lo))
    | Port(PortDir(dir, Atom kind), nam, [], signed) -> addwire bufh typhash ([vdir (portpos typhash nam) dir], Id nam, signcnv (signed, Deflt))
    | Port(dir, nam, [], signed) -> addwire bufh typhash ([vdir (portpos typhash nam) dir], Id nam, signcnv (signed, Deflt))
    | Itmlst lst -> List.iter (decl_template bufh typhash modules pth) lst
    | Port(PortDir(dir, Atom kind), nam, [AnyRange _ as x], signed) -> addwire bufh typhash (range typhash (Id nam, x) :: [vdir (portpos typhash nam) dir], Id nam, signcnv (signed, x))
    | Port(dir, nam, [AnyRange _ as x], signed) -> addwire bufh typhash (range typhash (Id nam, x) :: [vdir (portpos typhash nam) dir], Id nam, signcnv (signed, x))
    | DeclReg (reg_lst, [], signed) -> List.iter (function
      | Id _ as nam -> addwire bufh typhash ([], nam, signcnv (signed, Deflt));
      | VarDeclAsgn (nam, expr) -> addwire bufh typhash ([], nam, signcnv (signed, Deflt))
      | DeclAsgn (mem, (AnyRange (first,last) :: [])) -> addmem bufh typhash [first, last] 1 mem
      | oth -> unhand := Some oth; failwith "DeclReg550") reg_lst
    | DeclReg (reg_lst, (AnyRange (hi, lo) as x) :: [], signed) -> List.iter (function
      | Id _ as nam -> addwire bufh typhash ([range typhash (nam, x)], nam, signcnv (signed, x));
      | DeclAsgn (mem, AnyRange(first,last) :: []) -> addmem bufh typhash [first, last] (ceval typhash hi - ceval typhash lo + 1) mem
      | DeclAsgn (mem, AnyRange(first,last) :: AnyRange(first',last') :: []) -> addmem bufh typhash [first, last; first', last'] (ceval typhash hi - ceval typhash lo + 1) mem
      | VarDeclAsgn (nam, expr) -> addwire bufh typhash ([], nam, signcnv (signed, Deflt))
      | oth -> unhand := Some oth; failwith "DeclReg555") reg_lst;
    | TypEnum4 (Deflt, id_lst, [Id nam]) -> elabenum typhash nam id_lst
    | TaskDecl(nam, arg1, arg2, arg3) -> update typhash (Id nam) (Task(arg1,arg2,arg3))
    | ParamDecl (Atom ("Localparam_real"|"Parameter_real"), [ParamAsgn1 (nam, expr)]) -> update typhash (Id nam) (Vreal (match expr with Float f -> f | Number(_,_,n,_) -> float_of_int n | oth -> failwith "realparam"))
    | ParamDecl (Atom ("Parameter"|"localparam"), param_lst) -> List.iter (function
          | ParamAsgn1 (nam, expr) ->
              let wid = width typhash expr in
              dbgpar := (nam,expr,wid) :: !dbgpar;
              addwire bufh typhash ([Wire_optionswidth wid], Id nam, Vlocal(wid, expr)); 
              addconn bufh typhash (Id nam, expr)
	  | oth -> unhand := Some oth; failwith "localparam") param_lst;
    | ParamDecl (Param ("localparam", sgn, (AnyRange _ as x) :: []), param_lst) -> List.iter (function
          | ParamAsgn1 (nam, expr) ->
              let wid = width typhash expr in
              dbgpar := (nam,expr,wid) :: !dbgpar;
              addwire' bufh typhash nam x;
              addconn bufh typhash (Id nam, expr)
	  | oth -> unhand := Some oth; failwith "localparam") param_lst;
    | ParamDecl (LocalParamTyp (Typ8 (Atom kind, Deflt)), param_lst) -> List.iter (function
          | ParamAsgn1 (nam, expr) ->
              let wid = width typhash expr in
              dbgpar := (nam,expr,wid) :: !dbgpar;
              let wid = atom_width kind in
              addwire bufh typhash ([Wire_optionswidth wid], Id nam, Vlocal (wid, expr));
              addconn bufh typhash (Id nam, expr)
	  | oth -> unhand := Some oth; failwith "localparam_int") param_lst;
    | NetDecl (Atom "wire" :: [], wire_lst) -> List.iter (function
          | Id nam -> addwire bufh typhash ([], Id nam, Unsigned)
	  | DeclAsgn (Id nam, (AnyRange _ as x) :: []) ->
              addwire' bufh typhash nam x
          | InitSig (nam, expr) -> addwire bufh typhash ([], nam, Unsigned); addconn bufh typhash (nam, expr)
	  | oth -> unhand := Some oth; failwith "NetDecl'") wire_lst;
    | NetDecl (Atom "wire" :: (AnyRange _ as x) :: [], wire_lst) -> List.iter (function
          | Id nam -> addwire' bufh typhash nam x
          | InitSig (Id nam, expr) -> addwire' bufh typhash nam x; addconn bufh typhash (Id nam, expr)
	  | oth -> unhand := Some oth; failwith "NetDecl''") wire_lst;
    | DeclInt2 id_lst -> List.iter (function
	| Id _ as nam -> addwire bufh typhash ([Wire_optionswidth 32], nam, Unsigned_vector(Intgr 31, Intgr 0))
        | VarDeclAsgn (nam, expr) -> update typhash (nam) (Vint (ceval typhash expr))
        | Intgr _ -> () (* residue from loop unrolling can be ignored *)
        | oth -> unhand := Some oth; failwith "DeclInt2") id_lst
(*
    | Itmlst (id_lst) -> List.iter (function
	  | Id nam -> update typhash (Id nam) Unsigned; bprintf buf' "  wire %s; // 622	\n" nam
	  | oth -> unhand := Some oth; failwith "DeclLogic647"
        ) id_lst;
    | DeclLogic (reg_lst) -> List.iter (function
	  | Id nam -> update typhash (Id nam) Unsigned; bprintf buf' "  wire %s; // 626	\n" nam
          | DeclAsgn (nam, AnyRange (hi, lo) :: []) ->
              bprintf buf' "  wire [%s : %s] %s; // 628\n" (cexpr typhash hi) (cexpr typhash lo) nam
          | VarDeclAsgn (nam, expr) ->
              bprintf buf' "  wire %s = %s\n" nam (cexpr typhash expr)
	  | oth -> unhand := Some oth; failwith "DeclLogic651"
        ) reg_lst;
    | DeclLogic2 (wire_lst, AnyRange (hi, lo) :: []) -> List.iter (function
	  | Id nam -> update typhash (Id nam) (Unsigned_vector(hi,lo));
	  bprintf buf' "  wire [%s : %s] %s; // 635	\n" (cexpr typhash hi) (cexpr typhash lo) nam 
	  | DeclAsgn (nam, AnyRange (hi, lo) :: []) ->
              bprintf buf' "  wire [%s : %s] %s ; // 637\n" (cexpr typhash hi) (cexpr typhash lo) nam 
	  | oth -> unhand := Some oth; failwith "DeclLogic2") wire_lst
    | DeclLogic2 (wire_lst, AnyRange (hi, lo) :: AnyRange (hi', lo') :: []) -> List.iter (function
	  | Id nam -> update typhash (Id nam) (Unsigned_vector(hi,lo));
	  bprintf buf' "  wire [%s : %s] [%s : %s] %s ; // 641\n" (cexpr typhash hi) (cexpr typhash lo) (cexpr typhash hi') (cexpr typhash lo') nam
	  | oth -> unhand := Some oth; failwith "DeclWire") wire_lst
    | DeclLogic2 (wire_lst, AnyRange (hi, lo) :: AnyRange (hi', lo') :: AnyRange (hi'', lo'') :: []) -> List.iter (function
	  | Id nam -> update typhash (Id nam) (Unsigned_vector(hi,lo));
	  bprintf buf' "  wire [%s : %s] [%s : %s] [%s : %s] %s ; // 645\n" (cexpr typhash hi) (cexpr typhash lo) (cexpr typhash hi') (cexpr typhash lo') (cexpr typhash hi'') (cexpr typhash lo'') nam
	  | oth -> unhand := Some oth; failwith "DeclWire") wire_lst
    | InstDecl (typ, params, lst) -> List.iter (function
        | (InstNameParen1 _ | InstNameParen2 _) -> ()
        | Id id ->     bprintf buf' "  attribute \\wiretype \"\\\\%s\"\n  wire width 32 \\%s\n" typ id
        | oth -> unhand := Some oth; failwith "InstDecl") lst;
    | Typ2 ("bool_t", [], [Id "FALSE"; Id "TRUE"]) -> ()
    | Typ2 (nam, _, id :: []) ->  update typhash (Id nam) (Vtyp nam);
        let s = vexpr typhash id in update typhash s Vsigtyp; bprintf buf' "  wire %s : %s; // 681	\n" s nam
    | Typ2 (nam, _, id_lst) -> update typhash (Id nam) (Vtyp nam);
        List.iter (function
	     | Id _ as itm -> let s = vexpr typhash itm in update typhash s Vsigtyp; bprintf buf' "  wire %s : %s; // 684	\n" s nam
             | DeclAsgn (id, AnyRange(lft,rght) :: AnyRange(lft',rght') :: []) -> update typhash (Id nam) (Vtyp nam);
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
    | Typ7 (nam, Typ5 (Atom "logic", AnyRange(lft,rght) :: AnyRange(lft',rght') :: AnyRange(lft'',rght'') :: [])) -> update typhash (Id nam) (Vtyp nam);
        bprintf buf' "  wire %s; // 692	\n" nam
    | Typ7 (nam, Typ5 (Atom "logic", AnyRange(lft,rght) :: AnyRange(lft',rght') :: [])) -> update typhash (Id nam) (Vtyp nam);
        bprintf buf' "  wire %s; // 694	\n" nam
    | Typ7 (nam, Typ5 (Atom "logic", AnyRange(lft,rght) :: [])) -> update typhash (Id nam) (Vtyp nam);
        bprintf buf' "typedef logic [%s:%s] %s; // 696	\n" (cexpr typhash lft) (cexpr typhash rght) nam
    | Typ7 (nam, Typ8 (SUDecl (Atom "packed", lst), Deflt)) -> update typhash (Id nam) (Vtyp nam);
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
        let kind = Unsigned_vector(lft,rght) in
        List.iter (function Id nam -> update typhash (Id nam) kind | _ -> ()) id_lst';
        let f' itm = let s = vexpr typhash itm in update typhash s kind; s in
        let f'' = function Id nam -> bprintf buf' "typedef enum {%s} %s; // 705\n" (String.concat ", " (List.map f' id_lst)) nam | _ -> () in
        List.iter f'' id_lst'
    | TypEnum4 (TypEnum5 (Atom "logic"), id_lst, id_lst') ->
        let kind = Unsigned in
        List.iter (function Id nam -> update typhash (Id nam) kind | _ -> ()) id_lst';
        let f' itm = let s = vexpr typhash itm in update typhash s kind; s in
        let f'' = function Id nam -> bprintf buf' "    typedef enum {%s} %s; // 711\n" (String.concat ", " (List.map f' id_lst)) nam | _ -> () in
        List.iter f'' id_lst'
    | TypEnum6 (nam, TypEnum3 (AnyRange(lft,rght) :: []), id_lst) -> update typhash (Id nam) (Venum nam);
        bprintf buf' "typedef enum logic [%s:%s] {\n\t%s\n} %s; // 714\n" (cexpr typhash lft) (cexpr typhash rght) (String.concat ",\n\t" (List.map (function
        | Id e -> e
        | EnumInit (e, expr) ->
	    let s = cexpr typhash expr in
	    let s' = sprintf "%s = %s" e s in
	    update typhash s (Venum nam);
	    s'
	| oth -> unhand := Some oth; failwith "TypEnum6") id_lst)) nam
    | TypEnum6 (nam, TypEnum5 (Atom "logic"), id_lst) -> update typhash (Id nam) (Venum nam); ev' buf' typhash nam id_lst
    | TypEnum6 (nam, Deflt, id_lst) -> update typhash (Id nam) (Venum nam); ev' buf' typhash nam id_lst
    | TypEnum6 (nam, Typ8 (Atom "int", Atom "unsigned"), id_lst) -> update typhash (Id nam) (Venum nam); ev' buf' typhash nam id_lst
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
    | Genvar _ -> ()
    | AssertProperty -> ()
    | FunDecl _ -> ()
    | AutoFunDecl _ -> ()
    | Unimplemented _ -> ()
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

let rec generate_assignment_common update bufh typhash (lhs, rhs) : ilang list =
let lhswid = width typhash lhs in
let rhswid = width typhash rhs in
let rhs' = buffer' bufh typhash (if lhswid < rhswid then restrict typhash lhswid rhs
else if lhswid > rhswid then Concat (Number(2,lhswid-rhswid,0,"") :: rhs :: [])
else rhs) 0 in match lhs with
| IdArrayedPlusColon (IdArrayed2 (Id mem, (Id _ as addr)), lo', Intgr wid') ->
(match Hashtbl.find_opt typhash mem with
  | Some Vmem {off;siz;wid} ->
      dbgeq := (lhswid,rhswid,lhs,rhs') :: !dbgeq;
      let en = Number(2,wid,(1 lsl wid) - 1,"") in
      let lo = match lo' with
         | Intgr lo' -> Number (2, wid, ((1 lsl wid)-1) lsl lo', "")
	 | Id mask -> Id mask
         | Add _ as x -> buffer' bufh typhash x 0
         | oth -> unhand := Some oth; failwith "PlusColon" in
      if update then memwr bufh typhash (mem_opt typhash mem) mem addr rhs' en else []
  | _ -> if update then TokUpdate(tran' lhs, tran' rhs') :: [] else Assign_stmt67(tran' lhs, tran' rhs') :: [])
| IdArrayedColon (IdArrayed2 (Id mem, (Id _ as addr)), (Intgr hi|Number(_,_,hi,_)), (Intgr lo|Number(_,_,lo,_))) ->
(match Hashtbl.find_opt typhash mem with
  | Some Vmem {off;siz;wid} ->
      dbgeq := (lhswid,rhswid,lhs,rhs') :: !dbgeq;
      let en = Number(2,wid,(1 lsl wid) - 1,"") in
      let lo = Number (2, wid, ((1 lsl wid)-1) lsl lo, "") in
      if update then memwr bufh typhash (mem_opt typhash mem) mem addr rhs' en else []
  | _ -> if update then TokUpdate(tran' lhs, tran' rhs') :: [] else Assign_stmt67(tran' lhs, tran' rhs') :: [])
| IdArrayed1 (Id mem, addr, Number(_,_,abit,_)) ->
(match Hashtbl.find_opt typhash mem with
  | Some Vmem {off;siz;wid} ->
      dbgeq := (lhswid,rhswid,lhs,rhs') :: !dbgeq;
      let en = Number(2,wid,(1 lsl wid) - 1,"") in
      if update then memwr bufh typhash (mem_opt typhash mem) mem addr rhs' en else []
  | _ -> if update then TokUpdate(tran' lhs, tran' rhs') :: [] else Assign_stmt67(tran' lhs, tran' rhs') :: [])
| IdArrayed1 (Id mem, addr, abit) ->
(match Hashtbl.find_opt typhash mem with
  | Some Vmem {off;siz;wid} ->
      dbgeq := (lhswid,rhswid,lhs,rhs') :: !dbgeq;
      let en = Number(2,wid,(1 lsl wid) - 1,"") in
      if update then memwr bufh typhash (mem_opt typhash mem) mem addr rhs' en else []
  | _ -> if update then TokUpdate(tran' lhs, tran' rhs') :: [] else Assign_stmt67(tran' lhs, tran' rhs') :: [])
| IdArrayed2 (Id mem, (Number _ as addr)) ->
(match Hashtbl.find_opt typhash mem with
  | Some Vmem {off;siz;wid} ->
      dbgeq := (lhswid,rhswid,lhs,rhs') :: !dbgeq;
      let en = Number(2,wid,(1 lsl wid) - 1,"") in
      if update then memwr bufh typhash (mem_opt typhash mem) mem addr rhs' en else []
  | Some Unsigned_vector _ -> if update then TokUpdate(tran' lhs, tran' rhs') :: [] else Assign_stmt67(tran' lhs, tran' rhs') :: []
  | oth -> otha := Some oth; failwith "arr")
| IdArrayed2 (Id mem, addr) as x ->
dbgsplit := Some x;
(match Hashtbl.find_opt typhash mem with
  | Some Vmem {off;siz;wid} ->
      dbgeq := (lhswid,rhswid,lhs,rhs') :: !dbgeq;
      let en = Number(2,wid,(1 lsl wid) - 1,"") in
      if update then memwr bufh typhash (mem_opt typhash mem) mem addr rhs' en else []
  | Some Unsigned_vector _ ->
    let inst, (p,u,s) = asgn'' bufh typhash (Id mem) addr rhs' in
    dbgasgn := (p,u,s) :: !dbgasgn;
    if update then s else (p @ u)
  | oth -> otha := Some oth; failwith "arr")
| oth -> dbgcommon := Some (lhs,rhs'); if update then TokUpdate(tran' lhs, tran' rhs') :: [] else Assign_stmt67(tran' lhs, tran' rhs') :: []

and dlymapwid bufh dhash typhash wid lhs : ilang list * ilang list * rw =
  let dly = dlymemo bufh dhash typhash lhs in
  wid := !wid + width typhash lhs;
  let lhs' = buffer' bufh typhash lhs (width typhash lhs) in
  (generate_assignment_common false bufh typhash (dly, lhs'),
   generate_assignment_common true bufh typhash (lhs, dly),
   dly)

and cnv' bufh dhash typhash inst = function
    | Atom ";" -> ([Assign_stmt67([],[])],[Assign_stmt67([],[])],[Assign_stmt67([],[])])
    | TaskDecl _ -> ([],[],[])
    | DeclReg _ as x -> decl_template bufh typhash [] None x; ([],[],[])
    | DeclInt2 _ -> ([],[],[])
    | Itmlst lst ->
        let lst' = List.map (cnv' bufh dhash typhash inst) lst in
        (List.flatten (List.map (fun (p,u,d) -> p) lst'),
         List.flatten (List.map (fun (p,u,d) -> u) lst'),
         List.flatten (List.map (fun (p,u,d) -> d) lst'))
    | Seq (lbl, lst) ->
        let lst' = List.map (cnv' bufh dhash typhash inst) lst in
        (List.flatten (List.map (fun (p,u,d) -> p) lst'),
         List.flatten (List.map (fun (p,u,d) -> u) lst'),
         List.flatten (List.map (fun (p,u,d) -> d) lst'))
    | EquateConcat(lhslst, expr) ->
        let wid = ref 0 in
        let dlylst = List.map (dlymapwid bufh dhash typhash wid) lhslst in
        let rhs = buffer' bufh typhash expr !wid in
        List.flatten (List.map (fun (p,u,d) -> p) dlylst),
	generate_assignment_common false bufh typhash (GenBlock (List.map (fun (p,u,d) -> d) dlylst), rhs),
	List.flatten (List.map (fun (p,u,d) -> u) dlylst)
    | Equate (lhs, (Number _ as expr)) ->
        let dly = dlymemo bufh dhash typhash lhs in
        (generate_assignment_common false bufh typhash (dly, lhs),
         generate_assignment_common false bufh typhash (dly, expr),
         generate_assignment_common true bufh typhash (lhs, dly))
    | Equate (lhs, expr) ->
        let dly = dlymemo bufh dhash typhash lhs in
        let rhs = buffer' bufh typhash expr (width typhash lhs) in
        (generate_assignment_common false bufh typhash (dly, lhs),
         generate_assignment_common false bufh typhash (dly, rhs),
         generate_assignment_common true bufh typhash (lhs, dly))
    | EquateSelect (lhs, (Number _ as sel), expr) ->
        let dly = dlymemo bufh dhash typhash lhs in
        let rhs = buffer' bufh typhash expr 0 in
        (generate_assignment_common false bufh typhash (dly, lhs),
         generate_assignment_common false bufh typhash (IdArrayed2(dly,sel), rhs),
         generate_assignment_common true bufh typhash (lhs, dly))
    | EquateSelect (Id lhs, sel, expr) ->
        let rhs = buffer' bufh typhash expr 0 in
        let sel' = buffer' bufh typhash sel 0 in
        if is_mem typhash lhs then
        ([], [], generate_assignment_common true bufh typhash (IdArrayed2(Id lhs,sel'), rhs)) else
         asgn' bufh dhash typhash inst (Id lhs) sel expr
    | EquateSelect2 (IdArrayed2(lhs, sel'), sel, expr) ->
        let dly = dlymemo bufh dhash typhash lhs in
        let rhs = buffer' bufh typhash expr 0 in
        (generate_assignment_common false bufh typhash (dly, lhs),
         generate_assignment_common false bufh typhash (IdArrayed1(lhs,sel',sel), rhs),
         generate_assignment_common true bufh typhash (lhs, dly))
    | EquateSlice (lhs, ((Number _| Intgr _) as hi), ((Number _| Intgr _) as lo), expr) ->
         let rhs = buffer' bufh typhash expr 0 in
         ([],
         generate_assignment_common false bufh typhash (IdArrayedColon(lhs,hi,lo), rhs),
         [])
    | EquateSlicePlus (lhs, (lo), (Intgr _ | Number _ as wid), expr)
    | Blocking(FopAsgnArrayWid (lhs, (lo), (Intgr _ | Number _ as wid), expr)) ->
        let rhs = buffer' bufh typhash expr 0 in
        ([],
         [],
         generate_assignment_common false bufh typhash (IdArrayedPlusColon(lhs,lo,wid), rhs))
    | Blocking (FopAsgnArraySel(Id lhs, sel, expr)) ->
        let rhs = buffer' bufh typhash expr 0 in
        if is_mem typhash lhs then
        ([],
         generate_assignment_common false bufh typhash (IdArrayed2(Id lhs,sel), rhs),
         []) else
        ([],
         generate_assignment_common false bufh typhash (IdArrayed2(Id lhs,sel), rhs),
         [])
    | Blocking (FopAsgn (lhs, ((Number _ | Intgr _) as expr))) ->
        ([],
         generate_assignment_common false bufh typhash (lhs, expr),
         [])
    | Blocking (FopAsgn (lhs, expr)) ->
        let rhs = buffer' bufh typhash expr (width typhash lhs) in
        ([],
         generate_assignment_common false bufh typhash (lhs, rhs),
         [])
    | Blocking (FopAsgnConcat(lhslst, expr)) ->
        let wid = ref 0 in
        let dlylst = List.map (fun lhs ->
          let dly = dlymemo bufh dhash typhash lhs in
	  wid := !wid + width typhash lhs;
          let lhs' = buffer' bufh typhash lhs (width typhash lhs) in
          (generate_assignment_common false bufh typhash (dly, lhs'),
           generate_assignment_common true bufh typhash (lhs, dly),
          dly)) lhslst in
        let rhs = buffer' bufh typhash expr !wid in
        List.flatten (List.map (fun (p,u,d) -> p) dlylst),
		  generate_assignment_common false bufh typhash (GenBlock (List.map (fun (p,u,d) -> d) dlylst), rhs),
		  List.flatten (List.map (fun (p,u,d) -> u) dlylst)
    | CaseStmt (caselbl, itm_stmts) ->
        let lbl' = List.map (simplify_default bufh typhash) caselbl in
        let lst' = List.map (cnv' bufh dhash typhash inst) itm_stmts in
	let wid = width typhash inst in
        (List.flatten (List.map (fun (p,u,d) -> p) lst'),
         Switch_bodycase (List.flatten(List.map (fun itm -> tran' (restrict typhash wid itm)) lbl'), [], List.flatten (List.map (fun (p,u,d) -> u) lst')) :: [],
         List.flatten (List.map (fun (p,u,d) -> d) lst'))
    | CaseStart (CaseStart1 expr, stmts) ->
        let expr' = buffer'' bufh typhash expr in
        let lst' = List.map (cnv' bufh dhash typhash expr) stmts in
        dbgcase := lst';
        (List.flatten (List.map (fun (p,u,d) -> p) lst'),
         Switch_stmt (tran' expr', [], [], (List.flatten (List.map (fun (p,u,d) -> u) lst'))) :: [],
         List.flatten (List.map (fun (p,u,d) -> d) lst'))
    | Id t when Hashtbl.mem typhash t -> if verbose then print_endline ("executed task: "^t); ([],[],[])
    | SysTaskRef (Atom "$display", _) -> ([],[],[])
    | oth -> unhand := Some oth; failwith "cnv'"

and asgn bufh typhash expr = function
  | IdArrayed2 (Id _ as arr, (Id _ as sel)) ->
    let inst, (p,u,s) = asgn'' bufh typhash expr arr sel in
    let sync_lst = List.sort_uniq compare s in
    bufh.l := Proc_stmt (inst, [], (List.sort_uniq compare p) @ u, mapedge sync_lst AlwaysSync) :: !(bufh.l)
  | lhs ->
    let wid = width typhash lhs in
    let rhs = buffer' bufh typhash expr wid in
    let rhs' = restrict typhash wid rhs in
    bufh.c := Conn_stmt96(tran' lhs, tran' rhs') :: !(bufh.c)

and asgn' bufh dhash typhash inst arr sel expr =
    let wida = width typhash arr in
    let wids = width typhash sel in
    let wid = min wida (1 lsl wids) in
    dbgmem := Some arr;
    dbgwida := wid;
    dbgwids := wid;
    if (wid > 64) then failwith "asgn' width > 64";
    let body = CaseStart (CaseStart1 (sel), List.init wid (fun ix ->
        let num = Number(2,clog2 wid,ix,"") in
        (CaseStmt ([num], EquateSelect (arr, num, expr) :: [])))) in
    let (p,u,s) = cnv' bufh dhash typhash inst body in
    (List.sort_uniq compare p, List.sort_uniq compare u, List.sort_uniq compare s)

and asgn'' bufh typhash arr sel expr = 
    let dhash = Hashtbl.create 255 in
    let inst = newnam() in
    inst, asgn' bufh dhash typhash (Id inst) arr sel expr

and mapedge sync_lst = function
  | Pos (Id signal) -> Sync_list69 ([TokPos], [TokID signal], [], sync_lst) :: []
  | Neg (Id signal) -> Sync_list69 ([TokNeg], [TokID signal], [], sync_lst) :: []
  | AlwaysSync -> if sync_lst <> [] then Sync_listalways([], sync_lst) :: [] else []
  | oth -> unhand := Some oth; failwith "mapedge"

(*
and simplify_case bufh dhash typhash inst = function
| Itmlst _ as itm -> cnv' bufh dhash typhash inst itm
| Seq _ as itm -> cnv' bufh dhash typhash inst itm
| Equate (Id lhs, Id rhs) as itm -> cnv' bufh dhash typhash inst itm
| Blocking (FopAsgn (Id mem, expr)) as  itm -> cnv' bufh dhash typhash inst itm
| EquateSlice _ as itm -> cnv' bufh dhash typhash inst itm
| EquateSelect(Id lhs, Number _, _) as itm -> cnv' bufh dhash typhash inst itm
| oth -> unhand := Some oth; failwith "simplify_case"

and simplify_case' bufh dhash typhash inst = function
| CaseStmt (caseval, itms) as x ->
   dbgcaser := Some x;
   cnv' bufh dhash typhash inst (CaseStmt (List.map (simplify_default bufh typhash) caseval, itms))
| oth -> unhand := Some oth; failwith "simplify_case'"
*)

and simplify_default bufh typhash = function
| Atom "default" as x -> x
| oth -> buffer'' bufh typhash oth

(*
and simplify_case'' bufh dhash typhash inst = function
| Itmlst _ as itm -> cnv' bufh dhash typhash inst itm
| Seq _ as itm -> cnv' bufh dhash typhash inst itm
| Equate (Id lhs, expr) as itm -> cnv' bufh dhash typhash inst itm
| Blocking (FopAsgn (Id mem, expr)) as  itm -> cnv' bufh dhash typhash inst itm
| EquateSlice _ as itm -> cnv' bufh dhash typhash inst itm
| EquateSelect(Id lhs, Number _, _) as itm -> cnv' bufh dhash typhash inst itm
| Id "empty_statement" -> ([],[],[])
| SysTaskRef _ -> ([],[],[])
| CaseStart _ as itm -> cnv' bufh dhash typhash inst itm
| EquateSelect _ as itm -> cnv' bufh dhash typhash inst itm
| oth -> unhand := Some oth; failwith "simplify_case''"
*)

let module_header modules = function
| Modul (typ, parm_lst, port_lst, body_lst) ->
  let typhash = Hashtbl.create 255 in
  let bufh = bufhash () in
  dbgtyp := typhash;
  List.iter (fun itm -> let _ = parm_map typhash itm in ()) parm_lst;
  dbgports := [];
  List.iteri (fun ix itm -> dbgports := itm :: !dbgports; ports typhash (ix+1) itm) port_lst;
  dbgdecl := [];
  List.iter (fun itm -> dbgdecl := itm :: !dbgdecl; decl_template bufh typhash modules None itm) body_lst;
  Hashtbl.iter (fun nam -> function
    | MaybePort(ix, (Unsigned_vector(hi,lo)|Signed_vector(hi,lo) as v), dir) ->
        let wid = ceval typhash hi - ceval typhash lo + 1 in 
        addwire bufh typhash (Wire_optionswidth wid :: [vdir ix dir], Id nam, v)
    | MaybePort(ix, (Unsigned|Signed as signage), dir) ->
        addwire bufh typhash ([vdir ix dir], Id nam, signage)
    | Vint _ -> ()
    | Task _ -> ()
    | Vmem _ -> ()
    | Unsigned -> ()
    | Unsigned_vector _ -> ()
    | InstArray _ -> ()
    | oth -> coth := Some oth; output_string logf ("portdump: "^vtyp oth^"\n")) typhash;
  let ports' = List.mapi (fun ix -> function
              | Port (_, id, _, _) -> (match Hashtbl.find_opt typhash id with
                  | Some (MaybePort (n, sgn, rng)) when n=ix+1 -> (id, sgn, rng)
	          | Some oth -> coth := Some oth; failwith "modport"
	          | None -> failwith ("modport "^id^" not found"))
	      | oth -> unhand := Some oth; failwith (typ^": map_ports")) port_lst in
  List.iter (fun (id,sgn,rng) -> ()) ports';
  bufh, typhash, ports'
| oth -> unhand := Some oth; failwith "module_header only handles modules"

let rec proc_template bufh typhash modules = function
    | Seq(lbl, lst) -> List.iter (proc_template bufh typhash modules) lst
    | Itmlst lst -> List.iter (proc_template bufh typhash modules) lst
    | DeclReg _ -> ()
    | DeclLogic _ -> ()
    | AlwaysLegacy (At (EventOr ((Pos _|Neg _) :: _ as edglst)), body) ->
    let dhash = Hashtbl.create 255 in
    let inst = newnam() in
    dbgproc := Some (typhash, dhash, inst, edglst, body);
    let (p,u,s) = cnv' bufh dhash typhash (Id inst) body in
    let sync_lst = List.sort_uniq compare s in
    bufh.l := Proc_stmt (inst, [], (List.sort_uniq compare p) @ u, List.flatten (List.map (mapedge sync_lst) edglst)) :: !(bufh.l)
    | AlwaysLegacy (AtStar, body) ->
    let dhash = Hashtbl.create 255 in
    let inst = newnam() in
    dbgproc := Some (typhash, dhash, inst, [], body);
    let (p,u,s) = cnv' bufh dhash typhash (Id inst) body in
    let sync_lst = List.sort_uniq compare s in
    bufh.l := Proc_stmt (inst, [], (List.sort_uniq compare p) @ u, mapedge sync_lst AlwaysSync) :: !(bufh.l)
    | AlwaysLegacy (At (EventOr (Id _ :: _)), body) ->
    let dhash = Hashtbl.create 255 in
    let inst = newnam() in
    dbgproc := Some (typhash, dhash, inst, [], body);
    let (p,u,s) = cnv' bufh dhash typhash (Id inst) body in
    let sync_lst = List.sort_uniq compare s in
    bufh.l := Proc_stmt (inst, [], (List.sort_uniq compare p) @ u, mapedge sync_lst AlwaysSync) :: !(bufh.l)
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
    | InstDecl (Id typ, params, lst) -> List.iter (function
        | InstNameParen1 (inst, Itmlst pins :: []) -> bufh.i := instance_template bufh typhash ("\\"^typ) (match params with Itmlst lst :: _ -> lst | _ -> []) inst pins :: !(bufh.i)
        | InstNameParen2 (inst, InstRange(lft,rght) :: []) -> bufh.i := instance_template bufh typhash ("\\"^typ) (match params with Itmlst lst :: _ -> lst | _ -> []) inst [] :: !(bufh.i)
        | Id _ as id -> addwire bufh typhash ([], id, Unsigned)
        | oth -> unhand := Some oth; failwith "InstDecl") lst;
    | InstArrayDecl (Id typ, params, Id inst, [InstRange(hi,lo)], pinlst) as x -> (dbginst := Some x; match List.assoc_opt typ !modules with
        | Some m ->
            let bufh', typhash', ports' = module_header modules m in
            dbgarr := Some (typ, params, inst, hi, lo, bufh', ports');
            for ix = ceval typhash hi downto ceval typhash lo do
                bufh.i := Cell_stmt ("\\"^typ, inst^"["^string_of_int ix^"]", [],
                   List.rev_map (parm_map typhash) params @
                   List.rev_map2 (fun (pin,sgn,rng) -> function
		       | CellPinItem2 (pin, Intgr n) -> TokConn([TokID(pin)], tran' (Number(2, 32, n, "")))
		       | CellPinItem2 (pin, conn) -> TokConn([TokID(pin)], tran' conn)
                       | Id id as conn ->
		         let wid = width typhash conn in
		         print_endline (id^": "^string_of_int wid);
			 TokConn([TokID(pin)], tran' (match wid with 1 -> conn | _ -> IdArrayed2 (conn, Intgr ix)))
                       | oth -> unhand := Some oth; failwith "inst_array_arg") ports' pinlst) :: !(bufh.i);
                done;
        | None -> failwith ("InstArrayDecl: "^typ^" not found"))

    | TypEnum4 _ -> ()
    | TypEnum6 _ -> ()
    | Typ2 (typ, _, typ_lst) -> ()
    | Typ3 _ -> ()
    | Typ4 _ -> ()
    | Typ5 _ -> ()
    | Typ6 _ -> ()
    | Typ7 _ -> ()
    | DeclInt2 _ -> ()
    | DeclLogic2 _ -> ()
    | LoopGen1 _ -> () (* placeholder *)
    | CondGen1 _ -> () (* placeholder *)
    | GenItem _ -> () (* placeholder *)
    | Genvar _ -> () (* placeholder *)
    | ParamDecl _ -> ()
    | TaskDecl _ -> ()
    | AutoFunDecl _ -> () (* placeholder *)
    | Initial _ -> () (* bufh.l := Attr_stmt ("initial_is_not_implemented", []) :: !(bufh.l) *)
    | Final _ -> () (* bufh.l := Attr_stmt ("final_is_not_implemented", []) :: !(bufh.l) *)
    | PkgImport _ -> ()
    | DeclData _ -> ()
    | AssertProperty -> ()
    | Port _ -> ()
    | FunDecl _ -> ()
    | NetDecl _ -> ()
    | Unimplemented _ -> ()
    | oth -> unhand := Some oth; failwith "proc_template"

let template modules = function
  | Modul(nam, parm_lst, port_lst, body_lst) as m ->
  print_endline ("Translating module: "^nam);
  dbgm' := Some m;
  let bufh, typhash, ports' = module_header modules m in
  let bufm = ref [] in
  bufm := Attr_stmt ("\\cells_not_processed", [TokInt 1]) :: !(bufm);
  port' := ports';
  body' := body_lst;
  List.iter (proc_template bufh typhash modules) body_lst;
  bufm := Module12 (nam, catbuf bufh) :: !(bufm);
  List.rev (!bufm);
  | PackageBody (pkg, body_lst) ->
  let typhash = Hashtbl.create 255 in
  let bufh = bufhash () in
  List.iter (decl_template bufh typhash modules None) body_lst;
  List.rev (catbuf bufh);
  | oth -> unhand := Some oth; failwith "This template only handles modules/packages"

let template modules x = let t = template modules (sub' x) in flush logf; t
  
