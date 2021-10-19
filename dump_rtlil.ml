open Source_text_rewrite_types
open Input_rewrite_types
open Source_text_rewrite
open Source_text_split
open Source_text_lex
open Source_text
open Printf

type map = {nonblk:string; block:string; typh:(string,vtyp)Hashtbl.t}

let verbose = try int_of_string (Sys.getenv "DUMP_RTLIL_VERBOSE") with err -> 0
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
  | Vlong _ -> "Vlong"
  | Vreal _ -> "Vreal"
  | Vlocal _ -> "Vlocal"
  | Vsu _ -> "Vsu"
  | Vsua _ -> "Vsua"

let unhand = ref None
let unhand_lst = ref []
let unhand_typ = ref None
let unhand_pkg = ref None
let unhand_arg_lst = ref []
let unhand_inst_lst = ref []
let dbgfield = ref None
let dbgrepl = ref []
let dbgsu = ref None
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

let rec ceval typhash = function
| Intgr n -> n
| Number (_,_,n,_) -> n
| Id s ->
  if verbose > 0 then print_endline ("Eval "^s^" as a constant");
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
  if verbose > 0 then print_endline ("Evaluating local parameter");
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
| Vlong _ -> 64
| Vreal _ -> 64
| Vlocal (n,_) -> n
| Vsu (Id id, lst) -> List.fold_left (+) 0 (List.map (fun (_,arg) -> csiz' typhash arg) lst)
| Vsua (last, first, lst) -> (last - first + 1) * (List.fold_left (+) 0 (List.map (fun (_,arg) -> csiz' typhash arg) lst))
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
| InitPat [] -> 0
| InitPat (hd::tl) -> width typhash hd + width typhash (InitPat tl)
| PatMember1 (Id _, memb) -> width typhash memb
| Dot1 (Id id, Id fld) -> snd (off_width_field typhash id fld)
| Dot1 (Id id, IdArrayedColon (Id fld, hi, lo)) -> ceval typhash hi - ceval typhash lo + 1
| oth -> unhand := Some oth; failwith "width"

and search_field typhash off key = function
| [] -> (0,0)
| (field', typ') :: tl -> let wid = csiz' typhash typ' in if key = field' then (off,wid) else search_field typhash (off+wid) key tl

and off_width_field typhash id field =
     let field_lst = match Hashtbl.find_opt typhash id with
        | Some (Vsu (Id id', lst)) -> lst
        | Some (MaybePort(_, Vsu (Id id', lst), _)) -> lst
        | Some oth -> dbgfield := Some oth; failwith "struct/union field"
        | None -> failwith (id^" not found in typhash") in
     let off,wid = search_field typhash 0 field field_lst in
     (off,wid)

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
    | Some (Signed_vector _ | Signed) -> Signed
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
| Dot1(id,fld) -> signof typhash id
| oth -> unhand := Some oth; failwith "signof"

let is_const' = function
| Vint n -> true
| Venum _ -> true
| Unsigned -> false
| Unsigned_vector _ -> false
| MaybePort _ -> false
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

let othx = ref None
let dbgsplit = ref None
let dbgfind = ref []
let dbgform = ref []
let dbgact = ref []
let dbgasgn = ref []
let dbgloop = ref None
let body' = ref []
let port' = ref []
let dbgm' = ref None
let dbgarr = ref None
let dbginst = ref None
let dbgmodinst = ref []
let recurhash = ref (Hashtbl.create 1)
let dbgsub = ref None
let dbgsub' = ref None
let dbgsubst = ref []
let dbgeq = ref []
let dbgmem = ref []
let dbgproc = ref None
let dbgalw = ref []
let dbgcommon = ref None
let dbgdecl = ref []
let dbgports = ref []
let dbgtyp = ref (Hashtbl.create 1)
let otha = ref None
let dbgcase = ref []
let dbgcase' = ref []
let dbgwida = ref 0
let dbgwids = ref 0
let dbgmem' = ref None
let dbgmem'' = ref None
let dbgpar = ref []
let dbgatom = ref ""
let dbgpkg = ref None
let dbgpair = ref []
let dbgalweq = ref None

let alweq = function
| Id id, Id id' -> id=id'
| Id id, Expression (Equals (Id id', Number (2, 1, 1, "1"))) -> id=id'
| oth -> dbgalweq := Some oth; failwith "alweq"

let rec uniqid attr pref cnt lhs =
  let newid = Id (pref^string_of_int cnt^lhs) in
  if Hashtbl.mem attr.subst newid then
    uniqid attr pref (cnt+1) lhs
  else
    begin
    Hashtbl.add attr.subst (Id lhs) newid;
    newid
    end

let mapadd map (attr:attr) pref lhs expr =
  let typ' = match Hashtbl.find_opt map.typh lhs with
      | Some typ' -> typ'
      | None -> failwith "mapadd" in
  let newid = uniqid attr pref 1 lhs in
  Hashtbl.add attr.subst newid (Active(typ', newid, expr));
  newid

let map_id attr id = match Hashtbl.find_opt attr.subst id with Some repl -> repl | None -> id

let rec map_active_combined' (map:map) (attr: Source_text_rewrite.attr) : (rw -> rw * rw) = function
| Equate (Id lhs as lhs', expr) ->
  let expr',_ = map_active_combined' map attr expr in
  let lhs3 = mapadd map attr map.nonblk lhs expr' in
  (Blocking(FopAsgn(lhs3, expr')), Equate(lhs', lhs3))
| Blocking (FopAsgn (Id lhs as lhs', expr)) ->
  let expr',_ = map_active_combined' map attr expr in
  let lhs3 = mapadd map attr map.nonblk lhs expr' in
  (Blocking (FopAsgn(lhs3, expr')), Equate(lhs', lhs3))
| Id _ as id -> let rslt = map_id attr id in (rslt, rslt)
| If1(cond, if_clause) as x ->
    let p,q = match split_pair (Source_text_rewrite.descend' attr x) with
      | If1(cond', if_clause'), q -> if_clause', q
      | oth,_ -> unhand := Some oth; failwith "split_if1" in p,q
| oth -> split_pair (Source_text_rewrite.descend' attr oth)

let rec map_active_combined (map:map) (attr: Source_text_rewrite.attr) x =
  let (comb,seq) = map_active_combined' map attr x in
  Split (comb,seq)

let rec map_reset (map:map) (attr: Source_text_rewrite.attr) = function
| Blocking (FopAsgn (Id lhs, expr)) -> Equate(Id lhs, expr)
| oth -> Source_text_rewrite.descend' attr oth

let rec search_pkg rslt key = function
| Typ7 (id, contents) -> if key=id then rslt := Some contents
| ParamDecl (LocalParamTyp (Typ3 (req_t, [PackageRef (pkg_id, Atom "::")])), lst) ->
  let found = find_pkg pkg_id req_t in
  List.iter (function
      | ParamAsgn1 (cfg, contents) -> if key=cfg then rslt := Some (Split(found, contents))
      | oth -> ()) lst
| TypEnum6 (id, _, lst) as contents ->
    if key=id then rslt := Some contents;
    let enum = ref 0 in List.iter (function
        | EnumInit (id, (Number(b,w,n,_) as n')) ->
          if key=id then rslt := Some n';
          enum := n;
          incr enum;
        | Id id ->
          if key=id then rslt := Some (Intgr !enum);
          incr enum;
        | oth -> unhand := Some oth; failwith "search_pkg_enum") lst
| ParamDecl (Atom "localparam", lst) -> List.iter (function
    | ParamAsgn1 (id, contents) -> if key=id then rslt := Some contents
    | oth -> ()) lst
| ParamDecl (LocalParamTyp _, lst) -> List.iter (function
    | ParamAsgn1 (id, contents) -> if key=id then rslt := Some contents
    | oth -> ()) lst
| FunDecl (id, typ, contents) -> if key=id then rslt := Some contents
| AutoFunDecl (id, typ, contents) -> if key=id then rslt := Some contents
| oth -> unhand := Some oth; failwith "search_pkg"

and import_pkg pkg_id =
  match List.assoc pkg_id !(Matchmly.modules) with
      | PackageBody (pkg_id', lst) when pkg_id = pkg_id' -> lst
      | oth -> unhand := Some oth; failwith "import_pkg"

and find_pkg pkg_id req_t =
  if verbose > 1 then print_endline ("recursing into "^pkg_id);
  let pkgbody = import_pkg pkg_id in
  let rslt = ref None in
  List.iter (search_pkg rslt req_t) pkgbody;
  if verbose > 1 then print_endline ("leaving "^pkg_id);
  dbgpkg := Some (req_t,pkg_id,!rslt,pkgbody);
  match !rslt with
    | None -> (unhand_pkg := Some (req_t,pkg_id); failwith "find_pkg")
    | Some rslt ->
      let attr = {Source_text_rewrite.fn=recurs_pkg pkgbody; subst=Hashtbl.create 255; pth=""} in
      recurs_pkg pkgbody attr rslt

and recurs_pkg pkgbody (attr: Source_text_rewrite.attr) = function
  | Id req_t -> (match recurs_pkg' pkgbody req_t with None -> Id req_t | Some rslt -> Source_text_rewrite.descend' {attr with fn=recurs_pkg pkgbody} rslt)
  | Typ1 req_t -> (match recurs_pkg' pkgbody req_t with None -> Typ1 req_t | Some rslt -> Source_text_rewrite.descend' {attr with fn=recurs_pkg pkgbody} rslt)
  | Typ3 (req_t, [PackageRef (pkg_id, Atom "::")]) ->
    Source_text_rewrite.descend' {attr with fn=recurs_pkg pkgbody} (Typ6 (find_pkg pkg_id req_t))
  | IdArrayed3 (PackageRef (pkg_id, Atom "::") :: [], IdArrayedColon (Id nam, hi, lo)) ->
    Source_text_rewrite.descend' {attr with fn=recurs_pkg pkgbody} (IdArrayedColon (find_pkg pkg_id nam, hi, lo))
  | oth -> Source_text_rewrite.descend' {attr with fn=recurs_pkg pkgbody} oth

and recurs_pkg' pkgbody req_t =
    let rslt = ref None in
    List.iter (search_pkg rslt req_t) pkgbody;
    let rslt = !rslt in
    if not (List.mem (req_t,rslt) !dbgfind) then dbgfind := (req_t,rslt) :: !dbgfind;
    rslt

let rec recur_inst porthash = function
  | [] -> []
  | InstNameParen1 (inst, lst) :: tl -> InstNameParen1 (inst, recur_inst porthash lst) :: recur_inst porthash tl
  | Itmlst lst :: tl -> recur_inst porthash lst @ recur_inst porthash tl
  | CellPinItem2 (pin, PackageRef (pkg_id, Id req_t)) :: tl -> Hashtbl.remove porthash pin; CellPinItem2 (pin, find_pkg pkg_id req_t) :: recur_inst porthash tl
  | CellPinItem2 (pin, expr) :: tl -> Hashtbl.remove porthash pin; CellPinItem2 (pin, expr) :: recur_inst porthash tl
  | CellPinItemImplied pin :: tl -> CellPinItem2 (pin, Id pin) :: recur_inst porthash tl
  | CellPinItemNC _ as id :: tl -> id :: recur_inst porthash tl
  | Id _ as id :: tl -> id :: recur_inst porthash tl
  | Atom ".*" :: [] -> let residue = ref [] in Hashtbl.iter (fun k _ -> residue := CellPinItem2(k,Id k) :: !residue) porthash; dbgmodinst := !residue;  !residue
  | oth -> unhand_inst_lst := oth; failwith "recur_inst"

let rec recur_param = function
  | CellParamItem2 (cfg, PackageRef (pkg_id, Id req_t)) -> CellParamItem2 (cfg, find_pkg pkg_id req_t)
  | CellParamItem2 (cfg, expr) -> CellParamItem2 (cfg, expr)
  | Itmlst lst -> Itmlst (List.map (recur_param) lst)
  | oth -> unhand := Some oth; failwith "recur_param"

let rec recurs1 (attr: Source_text_rewrite.attr) = function
  | Modul (nam, params, args, body) -> Modul (nam, List.map (recurs1 attr) params, (let _ = Hashtbl.remove attr.subst Deflt in List.map (recurs1 attr) args), List.map (recurs1 attr) body)
  | PackageParam
      ([PkgImport (Itmlst [PkgImportItm (pkg_id, Atom "*")])],
      PackageParam2 (cfg_t, cfg, [PackageRef (pkg_id', Atom "::")],
       PackageRef (pkg_id'', Id req_t))) -> 
    let itms = import_pkg pkg_id in 
    print_endline ("Importing: "^pkg_id^" ("^string_of_int (List.length itms)^" items)");
    PackageParam
      (List.map (Source_text_rewrite.descend' {attr with fn=recurs1}) itms,
      (Source_text_rewrite.descend' {attr with fn=recurs1}) (PackageParam2 (cfg_t, cfg, find_pkg pkg_id' cfg_t :: [], find_pkg pkg_id'' req_t))) 
  | FunDecl(id, _, _) as x -> Hashtbl.replace attr.subst (FunRef (id,[])) x; (Source_text_rewrite.descend' {attr with fn=recurs1}) x
  | AutoFunDecl(id, _, _) as x -> Hashtbl.replace attr.subst (FunRef (id,[])) x; (Source_text_rewrite.descend' {attr with fn=recurs1}) x
  | TaskDecl(id, _, _, _) as x -> Hashtbl.replace attr.subst (TaskRef (id,[])) x; (Source_text_rewrite.descend' {attr with fn=recurs1}) x
  | ParamDecl (x, lst) ->
      ParamDecl ((Source_text_rewrite.descend' {attr with fn=recurs1}) x, List.map (function
          | ParamAsgn1 (p, n) as x ->
            Hashtbl.replace attr.subst (Id p) n;
            (Source_text_rewrite.descend' {attr with fn=recurs1}) x
          | oth -> failwith "ParamDecl1") lst)
  | Param (nam, expr, _) as x -> Hashtbl.add attr.Source_text_rewrite.subst (Id nam) expr; (Source_text_rewrite.descend' {attr with fn=recurs1}) x
  | ParamAsgn1 (p, n) as x -> Hashtbl.replace attr.subst (Id p) n; (Source_text_rewrite.descend' {attr with fn=recurs1}) x
  | PackageRef (pkg_id, Id req_t) -> (Source_text_rewrite.descend' {attr with fn=recurs1}) (find_pkg pkg_id req_t)
  | Typ3 (req_t, [PackageRef (pkg_id, Atom "::")]) -> (Source_text_rewrite.descend' {attr with fn=recurs1}) (find_pkg pkg_id req_t)
  | Typ4 (req_t, [PackageRef (pkg_id, Atom "::")], arg3, arg4) -> Typ11 (find_pkg pkg_id req_t, arg3, arg4)
  | Typ7 (nam, expr) as x -> Hashtbl.add attr.Source_text_rewrite.subst (Id nam) expr; (Source_text_rewrite.descend' {attr with fn=recurs1}) x
  | TypEnum6 (nam, typ, id_lst) as x -> Hashtbl.add attr.Source_text_rewrite.subst (Id nam) (Typ5(typ, id_lst)); (Source_text_rewrite.descend' {attr with fn=recurs1}) x
  | Seq ("", itm :: []) -> itm
  | oth -> Source_text_rewrite.descend' {attr with fn=recurs1} oth

let signcnv = function
        | (Deflt,AnyRange(hi,lo)) -> Unsigned_vector(hi,lo)
	| (Deflt,Deflt) -> Unsigned
        | (Atom "signed",Deflt) -> Signed
        | (Atom "signed",AnyRange(hi,lo)) -> Signed_vector(hi,lo)
	| (Deflt,oth) -> failwith "signcnv'"
	| oth, _ -> unhand := Some oth; failwith "signcnv"

let array_port typhash ix hi lo dir nam =
(*
  output_string logf (nam^": array port "^string_of_int ix^" ["^string_of_int (ceval typhash hi)^" : "^string_of_int (ceval typhash lo)^"]\n");
*)
  update typhash nam (MaybePort (ix, Unsigned_vector(hi,lo), dir))

let maybe_port typhash nam ix dir typ =
  output_string logf (nam^": port "^string_of_int ix^"\n");
  update typhash (Id nam) (MaybePort (ix, typ, dir))

let atom_cnv = function
| "int" -> Vint 0
| "logic" -> Unsigned
| "integer" -> Vint 0
| "longint" -> Vlong 0L
| "real" -> Vreal 0.0
| oth -> dbgatom := oth; failwith "atom_width"

let atom_width = function
| "logic" -> 1
| "int" -> 32
| "integer" -> 32
| "longint" -> 64
| "real" -> 64
| oth -> dbgatom := oth; failwith "atom_width"

let rec struct_union typhash = function
	   | SUMember (Typ6 (Atom ("logic"|"bit"|"byte"|"int"|"longint" as kind)), lst) -> List.map (function
               | Id id -> (id, atom_cnv kind)
	       | oth -> unhand := Some oth; failwith "SUMember5") lst
	   | SUMember (Typ5 (Atom kind, AnyRange (lft, rght) :: []), lst) -> List.map (function
               | Id id -> (id, Unsigned_vector(lft, rght))
	       | oth -> unhand := Some oth; failwith "SUMember2") lst
           | SUMember (Typ6 (Typ5 (Atom ("logic"), [AnyRange (lft, rght)])), id_lst) -> List.map (function
               | Id id -> (id, Unsigned_vector(lft, rght))
	       | oth -> unhand := Some oth; failwith "SUMember7") id_lst
           | SUMember (Typ5 (TypEnum3 [AnyRange (lft, rght)], eid_lst), id_lst) -> List.map (function
               | Id id -> (id, Unsigned_vector(lft, rght))
	       | oth -> unhand := Some oth; failwith "SUMember8") id_lst
           | SUMember (TypEnum6 (id_t, TypEnum3 [AnyRange (lft, rght)], eid_lst), id_lst) ->  List.map (function
               | Id id -> (id, Unsigned_vector(lft, rght))
	       | oth -> unhand := Some oth; failwith "SUMember9") id_lst
           | SUMember (Typ8 (SUDecl (Atom "packed", lst), Deflt), id_lst) -> List.map (function
               | Id id -> (id, Vsu (Id id, List.flatten (List.map (struct_union typhash) lst)))
	       | oth -> unhand := Some oth; failwith "SUMember9") id_lst
           | oth -> unhand := Some oth; failwith "SUMember"

let rec ports typhash ix = function
    | Port ((In|Out|Inout|Deflt) as dir, nam, [], sgn) -> maybe_port typhash nam ix dir (signcnv (sgn, Deflt))
    | Port (PortDir ((In|Out|Inout|Deflt) as dir, Atom ("wire"|"reg"|"logic")), nam, [], sgn) -> maybe_port typhash nam ix dir (signcnv (sgn, Deflt))
    | Port ((In|Out|Inout) as dir, nam, AnyRange (hi, lo) :: [], sgn) -> array_port typhash ix hi lo dir (Id nam)
    | Port (PortDir ((In|Out|Inout|Deflt) as dir, Atom ("wire"|"reg"|"logic")), nam, AnyRange (hi, lo) :: [], sgn) -> array_port typhash ix hi lo dir (Id nam)
    | Port ((In|Out|Inout) as dir, nam, AnyRange (hi, lo) :: AnyRange(hi', lo') :: [], sgn) -> array_port typhash ix hi lo dir (Id nam)
    | Port ((In|Out|Inout) as dir, nam, AnyRange (hi, lo) :: AnyRange(hi', lo') :: AnyRange(hi'', lo'') :: [], sgn) -> array_port typhash ix hi lo dir (Id nam)
    | Port ((In|Out|Inout) as dir, nam, Typ6 (Atom primtyp) :: [], sgn) -> maybe_port typhash nam ix dir Unsigned
    | Port (Deflt, nam, Typ2 (typ_t, PackageRef (pkg, Atom "::") :: [], []) :: AnyRange (hi, lo) :: [], sgn) -> update typhash (Id nam) (Vtyp(typ_t));
        array_port typhash ix hi lo Inout (Id nam)
    | Port ((In|Out|Inout) as dir, nam, Typ2 (typ_t, [], []) :: AnyRange (hi, lo) :: [], sgn) -> update typhash (Id nam) (Vtyp(typ_t));
        array_port typhash ix hi lo dir (Id nam)
    | Port ((In|Out|Inout) as dir, nam, Typ2 (typ_t, [], []) :: [], sgn) -> update typhash (Id nam) (Vtyp(typ_t));
        maybe_port typhash nam ix dir (signcnv (sgn, Deflt))
    | Port ((In|Out|Inout) as dir, nam, Typ2 (typ_e, (PackageRef (pkg, Atom "::")) :: [], []) :: [], sgn) -> update typhash (Id nam) (Vpkg(pkg, typ_e));
        maybe_port typhash nam ix dir (signcnv (sgn, Deflt))
    | Port ((In|Out|Inout) as dir, nam, Typ2 (typ_e, (PackageRef (pkg, Atom "::")) :: [], []) :: AnyRange (hi, lo) :: [], sgn) -> update typhash (Id nam) (Unsigned_vector(hi, lo));
        array_port typhash ix hi lo dir (Id nam)
    | Dot3 (bus, dir, member) -> update typhash member (Vintf bus);
    | DotBus (bus, dir, member, AnyRange(hi,lo) :: []) -> update typhash member (Vintf bus);
        array_port typhash ix hi lo Inout bus
    | Port ((In|Out|Inout) as dir, nam, [Typ5 (Typ8 (SUDecl (Atom "packed", memblst), Deflt), [])], Deflt) ->
        update typhash (Id nam) (MaybePort (ix, Unsigned, dir));
    | Port ((In|Out|Inout) as dir, id_i, [Typ12 ([], Typ8 (SUDecl (Atom "packed", memblst), Deflt), [])], Deflt) ->
        update typhash (Id id_i) (MaybePort (ix, Unsigned, dir));
        print_endline ("struct port "^id_i);
    | Port ((In|Out|Inout) as dir, nam, [Typ8 (SUDecl (Atom "packed", memblst), Deflt)], Deflt) ->
        let sulst = List.flatten (List.map (struct_union typhash) memblst) in
        let typ' = Vsu (Id nam, sulst) in
        update typhash (Id nam) (MaybePort (ix, typ', dir));
        List.iter (unpack_typ typhash nam) sulst;
    | oth -> unhand := Some oth; failwith "component"

and unpack_typ typhash stem = function
  | (nam, ((Unsigned | Signed | Unsigned_vector _ | Signed_vector _ ) as typ')) -> update typhash (Id (stem^"."^nam)) typ'
  | (nam, (Vsu (id, lst) as typ')) -> let stem' = stem^"."^nam in update typhash (Id stem') typ'; List.iter (unpack_typ typhash stem') lst
  | oth -> dbgsu := Some oth; failwith "unpack_typ"

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

let rec recurs2 (attr: Source_text_rewrite.attr) = function
  | Id _ as id -> (match Hashtbl.find_opt attr.subst id with None -> id | Some exp -> recurs2 attr exp)
  | ForLoop ([Asgn1 (Id ix, strt)], stop, Asgn1 (Id ix'', Add (Id ix''', inc)), body) ->
      recurs2 attr (iter attr ix ("ForLoop_"^ix) (elabeval attr strt) stop (elabeval attr inc) body)
  | ForLoop ([Typ9 (ix, [Atom "int"], strt)], stop, SideEffect (Id ix'', Atom "++"), body) ->
      recurs2 attr (iter attr ix ("ForLoop_"^ix) (elabeval attr strt) stop (elabeval attr (Intgr 1)) body)
  | ForLoop ([Typ9 (ix, [Atom "int"], strt)], stop, SideEffect (Id ix'', Atom "--"), body) ->
      recurs2 attr (iter attr ix ("ForLoop_"^ix) (elabeval attr strt) stop (elabeval attr (Intgr (-1))) body)
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
  | Typ2 (req_t, [PackageRef (pkg_id, Atom "::")], lst) ->
      let repl = find_pkg pkg_id req_t in
      dbgrepl := (req_t,(pkg_id,lst,repl)) :: !dbgrepl;
      Typ5 (repl, lst)
  | IdArrayed3 (PackageRef (pkg_id, Atom "::") :: [], IdArrayedColon (Id nam, hi, lo)) ->
    Source_text_rewrite.descend' {attr with fn=recurs2} (IdArrayedColon (find_pkg pkg_id nam, hi, lo))
  | InstDecl (Id kind, params, (InstNameParen1 _ :: _ as lst)) ->
      let modinst = match List.assoc_opt kind !(Matchmly.modules) with
          | Some (Modul(kind', params, ports, body)) -> ports
          | oth -> failwith ("modinst "^kind^" is missing") in
      let porthash = Hashtbl.create 255 in
      List.iteri (ports porthash) modinst;
      Source_text_rewrite.descend' {attr with fn=recurs2} (InstDecl (Id kind, List.map (recur_param) params, recur_inst porthash lst))
  | CellParamItem2 (cfg, PackageRef (pkg_id, Id req_t)) ->
      Source_text_rewrite.descend' {attr with fn=recurs2} (CellParamItem2 (cfg, find_pkg pkg_id req_t))
  | oth -> let attr = {attr with fn=recurs2} in Source_text_rewrite.descend' attr (simplify' attr oth)

and iter attr ix lbl strt stop inc stmts =
    print_endline ("iter: "^lbl);
    let loopvar = ref strt in
    let block = ref [] in
    let continue = match stop with
       | Less (Id ix', exp) -> (>) (elabeval attr exp)
       | LtEq (Id ix', exp) -> (>=) (elabeval attr exp)
       | GtEq (Id "i", exp) -> (<=) (elabeval attr exp)
       | oth -> unhand := Some oth; failwith "loop term" in
    dbgloop := Some (strt, stop, !loopvar);
    while continue !loopvar do
      begin
        Hashtbl.replace attr.Source_text_rewrite.subst (Id ix) (Intgr !loopvar);
        if verbose > 0 then print_endline (string_of_int !loopvar);
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

and dead_code = function
| Pling rhs -> dead_code rhs
| Less (Intgr n, Intgr m) -> if n < m then Always_true else Always_false
| LtEq (Intgr n, Intgr m) -> if n <= m then Always_true else Always_false
| Equals (Intgr n, Intgr m) -> if n = m then Always_true else Always_false
| NotEq (Intgr n, Intgr m) -> if n <> m then Always_true else Always_false
| GtEq (Intgr n, Intgr m) -> if n >= m then Always_true else Always_false
| Greater (Intgr n, Intgr m) -> if n > m then Always_true else Always_false
| Or2 (lhs, rhs) -> dead_code_or (dead_code lhs, dead_code rhs)
| And2 (lhs, rhs) -> dead_code_and (dead_code lhs, dead_code rhs)
| Id _ -> Undecidable
| Intgr n -> if n <> 0 then Always_true else Always_false
| (TildeAnd _ | RedAnd _ | RedOr _ | IdArrayed2 _ | GtEq _ | Equals _ | Less _ | LtEq _ | NotEq _ | Greater _) -> Undecidable
(*
| oth -> unhand := Some oth; failwith "dead_code"
*)
| _ -> Undecidable

and dead_code_id = function
| oth -> Undecidable

and dead_code_and = function
| (Always_false,_)
| (_, Always_false) -> Always_false
| _ -> Undecidable

and dead_code_or = function
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

and recurs3 (attr: Source_text_rewrite.attr) = function
  | CaseStart (CaseStart1 expr, stmts) ->
        let collapse = function [] -> failwith "collapse" | hd::[] -> hd | tl -> Seq ("", tl) in
        let op_dyadic = function
            | CaseStmt (lbls, body) -> fun else' -> collapse (List.map (fun caseval -> If2 (Equals (expr, caseval), collapse body, else')) lbls)
            | _ -> failwith "op_dyadic" in
        let op_terminate = function
            | CaseStmt (lbls, body) -> collapse (List.map (fun caseval -> If1 (Equals (expr, caseval), collapse body)) lbls)
            | _ -> failwith "op_terminate" in
        let hd, tl = match List.rev stmts with
            | hd::tl -> op_terminate hd, List.rev tl
            | [] -> failwith "empty statement" in
        let folded = List.fold_right op_dyadic tl hd in
        dbgcase := (expr, stmts, folded) :: !dbgcase;
        folded
  | Seq (lbl, lst) -> Seq (dot(attr.pth,lbl), let oldp = attr.pth in List.map (recurs3 {attr with pth=dot(oldp,lbl)}) lst)
  | NetDecl (arg1, lst) -> NetDecl (arg1, List.map (function
				     | InitSig (Id nam, arg2) -> let nam = npth attr nam in InitSig(Id nam, recurs3 attr arg2)
                                     | Id nam -> let nam = npth attr nam in Id nam
				     | oth' -> failwith (Source_text_rewrite.getstr oth')) lst)
  | Id _ as id -> (match Hashtbl.find_opt attr.subst id with None -> id | Some exp -> recurs3 attr exp)
  | Typ1 id_t -> (match Hashtbl.find_opt attr.subst (Id id_t) with None -> Typ1 id_t | Some exp -> recurs3 attr exp)
  | Typ3 (id_t, id_lst) -> (match Hashtbl.find_opt attr.subst (Id id_t) with None -> Typ3 (id_t, id_lst) | Some exp -> Typ9(id_t, id_lst, recurs3 attr exp))
  | Typ2 (id_t, rng, id_lst) -> (match Hashtbl.find_opt attr.subst (Id id_t) with None -> Typ2 (id_t, rng, id_lst) | Some exp -> Typ12(rng, recurs3 attr exp, id_lst))
  | PackageRef (pkg_id, Id req_t) -> (Source_text_rewrite.descend' {attr with fn=recurs3}) (find_pkg pkg_id req_t)
  | PackageRef _ as x -> othx := Some x; failwith "recurs3_pkg"
  | Port (dir, id, Typ2 (id_t, [], []) :: [], Deflt) as x -> (match Hashtbl.find_opt attr.subst (Id id_t) with
        | None -> x
        | Some exp -> Port(dir, id, recurs3 attr exp :: [], Deflt))
  | Port (dir, id, Typ2 (id_t, [], []) :: [], deflt) as x -> othx := Some x; failwith "recurs3_port"
  | oth -> Source_text_rewrite.descend' {attr with fn=recurs3} oth

and simplify' (attr: Source_text_rewrite.attr) = function
| Number(_, 32, n, _) -> Intgr n
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
| Add(lft,rght) -> Add(simplify' attr lft, simplify' attr rght)
| Sub(lft,rght) -> Sub(simplify' attr lft, simplify' attr rght)
| And2(Intgr lft, Intgr rght) -> Intgr (lft land rght)
| And2(x, Intgr 1) -> x
| And2(_, Intgr 0) -> Intgr 0
| And2(Intgr 0, _) -> Intgr 0
| And2(Intgr 1, x) -> x
| Or2(Intgr lft, Intgr rght) -> Intgr (lft lor rght)
| Or2(x, Intgr 0) -> x
| Or2(Intgr 0, x) -> x
| Or2(lft,rght) -> Or2(simplify' attr lft, simplify' attr rght)
| Query(Intgr cond, lft, rght) -> if cond <> 0 then simplify' attr lft else simplify' attr rght
| Equals(Intgr p, Intgr q) -> if p <> q then Intgr 0 else Intgr 1
| And (Intgr lhs, Intgr rhs) -> Intgr (lhs land rhs)
| Or (Intgr lhs, Intgr rhs) -> Intgr (lhs lor rhs)
| Xor (Intgr lhs, Intgr rhs) -> Intgr (lhs lxor rhs)
| Shiftl (Intgr lhs, Intgr rhs) -> Intgr (lhs lsl rhs)
| And (lhs, rhs) -> And (simplify' attr lhs, simplify' attr rhs)
| Or (lhs, rhs) -> Or (simplify' attr lhs, simplify' attr rhs)
| Xor (lhs, rhs) -> Xor (simplify' attr lhs, simplify' attr rhs)
| Shiftl (lhs, rhs) -> Shiftl (simplify' attr lhs, simplify' attr rhs)
| Expression (Intgr _ as x) -> x
| Expression (Shiftl (lhs, rhs)) -> Expression (simplify' attr (Shiftl (simplify' attr lhs, simplify' attr rhs)))
| Sys ("$clog2", Intgr n) -> Intgr (clog2 n)
| Div (Intgr lft, Intgr rght) when rght <> 0 -> Intgr (lft / rght )
| oth -> Source_text_rewrite.descend' {attr with fn=simplify'} oth
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
| (options, Id nam, (Vlocal _ as typ)) ->
  output_string logf ("add localparam: "^nam^", "^vtyp typ^"\n");
  exists_wire bufh typhash options nam typ;
| (options, Id nam, (Signed|Signed_vector _ as typ)) ->
  output_string logf ("add wire: "^nam^", "^vtyp typ^"\n");
  exists_wire bufh typhash options nam typ;
| (options, Id nam, (Unsigned|Unsigned_vector _ as typ)) ->
  output_string logf ("add wire: "^nam^", "^vtyp typ^"\n");
  exists_wire bufh typhash options nam typ;
| (_, oth, _) -> failwith ("Argument "^(Source_text_rewrite.getstr oth)^" of addwire must be wire Id")

let vdir ix = function
  | In -> Wire_optionsinput ix
  | Out -> Wire_optionsoutput ix
  | Inout -> Wire_optionsinout ix
  | Deflt -> Wire_optionsinvalid
  | oth -> unhand := Some oth; failwith "vdir"

let portpos typhash nam =
  match Hashtbl.find_opt typhash nam with Some (MaybePort (ix,_,_)) -> ix | _ -> 0

let range typhash = function
    | Id nam, AnyRange(hi,lo) ->
        let wid = ceval typhash hi - ceval typhash lo + 1 in Wire_optionswidth wid
    | _, oth -> unhand := Some oth; failwith "range"

let addwire' bufh typhash nam = function
    | AnyRange(hi,lo) as x ->
      addwire bufh typhash (range typhash (Id nam, x) :: [], Id nam, Unsigned_vector(hi,lo))
    | oth -> unhand := Some oth; failwith "addwire'"

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

let elabenum typhash nam id_lst =
  print_endline ("elabenum: "^nam);
  update typhash (Id nam) (Venum nam);
    let strt = ref 0 in
    List.iter (function
        | Id e -> update typhash (Id e) (Vemember(nam, e, Intgr(!strt)));
	    incr strt
        | EnumInit (e, expr) ->
	    strt := ceval typhash expr;
	    update typhash (Id e) (Vemember(nam, e, Intgr(!strt)));
            incr strt
	| oth -> unhand_lst := id_lst; failwith "TypEnum6") id_lst

let conn_lst  = ref []
let buflst = ref []

(* certain primitives go wrong if we equalise argument widths *)

let width_mismatch_not_ok = function
| "eq" -> false
| "shl" -> false
| "shr" -> false
| oth -> true

let newid bufh typhash wid =
  let nam = newnam() in
  let rslt = Id nam in
  addwire bufh typhash ([Wire_optionswidth wid], rslt, Unsigned_vector(Intgr (wid-1),Intgr 0));
  rslt

let vsel' n = function Id lhs -> [Sigspec90 (lhs, n)] | oth -> unhand := Some oth; failwith "vsel'"

let rec traneval' typhash id = function
| Vint n -> TokVal (int_to_bin n) :: []
| Unsigned -> TokID id :: []
| Signed -> TokID id :: []
| Unsigned_vector _ -> TokID id :: []
| Signed_vector _ -> TokID id :: []
| Vlocal (n, expr) -> TokInt (ceval typhash expr) :: []
| MaybePort (_, typ, _) -> traneval' typhash id typ
| Vemember (enum_id, id', Intgr n) when id=id' -> TokInt n :: []
| Venum kind ->
  if verbose > 2 then print_endline kind;
  TokID id :: [] (* place holder *)
| oth -> coth := Some oth; failwith ("traneval': "^id)

let rec tran' typhash = function
   | Id id -> traneval' typhash id (match Hashtbl.find_opt typhash id with Some x -> x | None -> print_endline ("Not found: "^id); Unsigned)
   | Number (b,w,n,_) -> TokVal (num_to_bin w n) :: []
   | Intgr n -> TokVal (int_to_bin n) :: []
   | Float f -> TokVal (flt_to_bin f) :: []
   | String s -> TokVal (let sz, b = str_to_bin s in string_of_int sz^"'"^b) :: []
   | Atom "default" -> []
(*
   TokVal (flt_to_bin f) :: []
   | Concat lst -> List.flatten (List.map tran' typhash lst)
*)
   | ExprOKL lst -> Sigspec92 (List.flatten (List.map (tran' typhash) lst)) :: []
   | GenBlock lst -> Sigspec92 (List.flatten (List.map (tran' typhash) lst)) :: []
   | IdArrayed2 (Id arr, (Number (_, _, n, _)|Intgr n)) -> vsel' n (Id arr)
   | IdArrayedColon (Intgr _ as n, _, _) -> tran' typhash n
   | IdArrayedColon (Id conn, (Intgr hi | Number(_,_,hi,_)), (Intgr lo | Number(_,_,lo,_))) -> Sigspecrange(conn,hi,lo) :: []
   | IdArrayedPlusColon (Id conn, (Intgr lo | Number(_,_,lo,_)), (Intgr wid | Number(_,_,wid,_))) -> Sigspecrange(conn,lo+wid-1,lo) :: []
   | oth -> unhand := Some oth; failwith "tran'"

let rec tran typhash = function
   | Dot1 (Id id, Id field) ->
     let off,wid = off_width_field typhash id field in
     if wid > 0 then tran' typhash (IdArrayedPlusColon (Id id, Intgr off, Intgr wid)) else failwith (id^"."^field)
   | (Id _ | Intgr _ | Number _ | IdArrayedColon _ | IdArrayed2 _ | ExprOKL _) as x -> tran' typhash x
   | oth -> unhand := Some oth; failwith "tran"

let rec parm_map_cell typhash = function
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
  | oth -> unhand := Some oth; failwith "parm_map_cell"

and instance_template bufh typhash typ params inst pinlst =
        Cell_stmt (typ, inst, [],
        List.rev_map (parm_map_cell typhash) params @
        List.rev (List.mapi (fun ix -> function
		   | CellPinItem2 (pin, Intgr n) -> TokConn([TokID(pin)], tran' typhash (Number(2, 32, n, "")))
		   | CellPinItem2 (pin, conn) -> TokConn([TokID(pin)], tran typhash conn)
		   | CellPinItemImplied (pin) -> TokConn([TokID(pin)], tran' typhash (Id pin))
		   | CellPinItemNC (pin) -> TokConn([TokID(pin)], tran' typhash (newid bufh typhash 1))
                   | Id _ as conn -> TokConn([TokID("$"^string_of_int (ix+1))], tran' typhash conn)
                   | oth -> unhand := Some oth; failwith "inst_arg") pinlst))

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
  | Id s as x ->
      let oldw = width typhash x in
      if oldw < wid then Concat (Number(2,wid-oldw,0,"") :: Id s :: []) else Id s
  | Dot1 (Id id, Id fld) ->
      let s = id^"."^fld in
      let oldw = width typhash (Id s) in
      if oldw < wid then Concat (Number(2,wid-oldw,0,"") :: Id s :: []) else Id s
  | Dot1 (Id id, IdArrayedColon (Id fld, hi, lo)) ->
      let s = id^"."^fld in
      let oldw = width typhash (Id s) in
      if oldw < wid then Concat (Number(2,wid-oldw,0,"") :: IdArrayedColon (Id s, hi, lo) :: []) else IdArrayedColon (Id s, hi, lo)
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
  | Typ3 (id_t, [PackageRef (pkg, Atom "::")]) -> pkg^"::"^id_t
  | PackageRef (pkg, Atom "::") -> pkg^"::"
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
    | oth -> unhand_arg_lst := oth; failwith "addprim" in
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

and memwr bufh typhash (options:mem_opts) id abits data en addr' data' en' sel' =
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
  TokUpdate (tran' typhash en', tran' typhash en) ::
  TokUpdate (tran' typhash data', tran' typhash data) ::
  TokUpdate (tran' typhash addr', tran' typhash sel') :: []

and memwr' bufh typhash addr data en addr' data' en' =
  Assign_stmt67 (tran' typhash en', tran' typhash en) ::
  Assign_stmt67 (tran' typhash data', tran' typhash data) ::
  Assign_stmt67 (tran' typhash addr', tran' typhash addr) :: []

and memwr'' update bufh typhash options mem addr data en = []

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

let buffer' bufh typhash expr wid =
  let wid' = max wid (width typhash expr) in
  buffer bufh typhash wid' expr

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
  | IdArrayedColon (Id s,hi,lo) -> (match Hashtbl.find_opt typhash s with Some x -> restrict' typhash wid s x | None -> Id s)
  | Atom "default" as x -> x
  | Intgr n -> Number(2,wid,n mod (1 lsl wid),"")
  | ExprOKL lst -> ExprOKL (List.rev (restrict_lst typhash wid (List.rev lst)))
  | String s -> let sz, b = str_to_bin s in Matchmly.widthnum (string_of_int wid^"'b"^String.sub b (sz-wid) wid)
  | oth -> unhand := Some oth; failwith (sprintf "restrict (%d) %s (%d)" wid (Source_text_rewrite.getstr oth) (width typhash oth))

and restrict_lst typhash wid = function
  | [] -> []
  | hd :: tl -> let hdwid = width typhash hd in if hdwid = wid then hd :: [] else if hdwid < wid then hd :: restrict_lst typhash (wid-hdwid) tl else restrict typhash wid hd :: []
                     
let addconn bufh typhash  (nam, expr) =
    let lhsw = width typhash nam in
    let expr' = buffer' bufh typhash expr 0 in
    let expw = width typhash expr' in
    let rhs = restrict typhash lhsw expr' in
    let rhsw = width typhash rhs in
    conn_lst := (lhsw, expw, rhsw, nam, rhs) :: !conn_lst;
    bufh.i := TokConn (tran' typhash nam, tran' typhash rhs) :: !(bufh.i)

let rec decl_template' bufh typhash modules pth = function
    | Seq (lbl, lst) -> List.iter (decl_template bufh typhash modules (newpth lbl pth)) lst
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
    | ParamDecl (LocalParamTyp (Typ1 id_t), param_lst) -> List.iter (function
          | ParamAsgn1 (nam, expr) ->
              let wid = width typhash expr in
              dbgpar := (nam,expr,wid) :: !dbgpar;
              let wid = 32 in (* place holder *)
              addwire bufh typhash ([Wire_optionswidth wid], Id nam, Vlocal (wid, expr));
              addconn bufh typhash (Id nam, expr)
	  | oth -> unhand := Some oth; failwith "localparam_int") param_lst;
    | ParamDecl (LocalParamTyp (Typ5 (Atom "logic", AnyRange (lft, rght) :: [])), param_lst) -> List.iter (function
          | ParamAsgn1 (nam, expr) ->
              let wid = width typhash expr in
              dbgpar := (nam,expr,wid) :: !dbgpar;
              let wid = 32 in (* place holder *)
              addwire bufh typhash ([Wire_optionswidth wid], Id nam, Vlocal (wid, expr));
              addconn bufh typhash (Id nam, expr)
	  | oth -> unhand := Some oth; failwith "localparam_int") param_lst;
    | ParamDecl (LocalParamTyp (Typ8 (Atom kind, signing)), param_lst) -> List.iter (function
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
    | TypEnum6 (nam, Deflt, id_lst) ->
      print_endline nam;
      elabenum typhash nam id_lst
    | TypEnum6 (nam, TypEnum3 (AnyRange(lft,rght) :: []), id_lst) ->
      print_endline nam;
      elabenum typhash nam id_lst
    | Typ5 (TypEnum6 (nam, TypEnum3 (AnyRange(lft,rght) :: []), id_lst), typid_lst) ->
      elabenum typhash nam id_lst
    | Typ7 (nam, Typ5 (Atom "logic", AnyRange(lft,rght) :: AnyRange(lft',rght') :: AnyRange(lft'',rght'') :: [])) -> update typhash (Id nam) (Vtyp nam);
    | Typ7 (nam, Typ5 (Atom "logic", AnyRange(lft,rght) :: [])) -> update typhash (Id nam) (Vtyp nam);
    | Typ7 (nam, Typ8 (SUDecl (Atom "packed", lst), Deflt)) ->
        update typhash (Id nam) (Vsu (Id nam, List.flatten (List.map (struct_union typhash) lst)))
    | Typ7 (nam, Typ8 (Union (Atom "packed", lst), Deflt)) ->
        update typhash (Id nam) (Vsu (Id nam, List.flatten (List.map (struct_union typhash) lst)))
    | Typ7 (nam, Typ8 (Itmlst lst, Deflt)) ->
      update typhash (Id nam) (Vsu (Id nam, List.flatten (List.map (struct_union typhash) lst)))
    | Typ5 (Typ8 (SUDecl (Atom "packed", lst), Deflt), decl_lst) ->
        List.iter (fun (nam) -> update typhash (nam) (Vsu (nam, List.flatten (List.map (struct_union typhash) lst)))) decl_lst
    | Typ5 (Typ5 (Atom "logic", AnyRange(lft,rght) :: []), id_lst) -> List.iter (function
          | Id nam -> update typhash (Id nam) (Vtyp nam);
	  | oth -> unhand := Some oth; failwith "DeclLogic1760") id_lst
    | Typ9 (old_id, id_lst, Typ5 (Deflt, elst)) -> List.iter (function
          | Id nam ->
            print_endline old_id;
            addwire bufh typhash ([], Id nam, Unsigned_vector(Intgr (clog2 (List.length elst) - 1), Intgr 0));
	  | oth -> unhand := Some oth; failwith "DeclLogic1760") id_lst
    | DeclLogic2 (wire_lst, AnyRange (hi, lo) :: [])  as x -> List.iter (function
	  | Id nam -> update typhash (Id nam) (Unsigned_vector(hi,lo));
          | DeclAsgn (nam, AnyRange(first,last) :: []) -> addwire bufh typhash ([range typhash (nam, x)], nam, Unsigned)
	  | oth -> unhand := Some oth; failwith "DeclLogic2") wire_lst
    | DeclLogic2 (wire_lst, AnyRange (hi, lo) :: AnyRange (hi', lo') :: []) -> List.iter (function
	  | Id nam -> update typhash (Id nam) (Unsigned_vector(hi,lo));
	  | oth -> unhand := Some oth; failwith "DeclWire") wire_lst
    | DeclData (Typ5 (Atom "logic", AnyRange (lft, rght) :: AnyRange (lft', rght') :: []), Deflt, VarDeclAsgn (mem, ExprOKL lst) :: []) -> () (* placeholder *)
    | DeclData (Atom "static", Typ8 (Atom "byte", Deflt), VarDeclAsgn (Id id, String s) :: []) -> () (* placeholder *)
(*
    | Typ7 (nam, Typ8 (SUDecl (Atom "packed", lst), Deflt)) ->
        update typhash (Id nam) (Vsu (Id nam, List.map (struct_union typhash) lst))
*)
    | Typ12 ([AnyRange (lft, rght)], Typ8 (SUDecl (Atom "packed", lst), Deflt), id_lst) ->
        let lft' = ceval typhash lft and rght' = ceval typhash rght in
        let first = min lft' rght' and last = max lft' rght' in
        List.iter (function
              | Id itm ->
                print_endline ("struct/union array: "^itm);
                let sulst = List.flatten (List.map (struct_union typhash) lst) in
                update typhash (Id itm) (Vsua (last, first, sulst));
                for i = first to last do
                  let stem = itm^"["^string_of_int i^"]" in
                  update typhash (Id stem) (Vsu (Id itm, sulst));
                  List.iter (unpack_typ typhash stem) sulst;
                done
              | oth -> unhand := Some oth; failwith "struct/union array") id_lst;
    | Typ11 (Typ8 (SUDecl (Atom "packed", lst), Deflt), [AnyRange (lft, rght)], id_lst) ->
        List.iter (fun nam -> update typhash (nam) (Vsu (nam, List.flatten (List.map (struct_union typhash) lst)))) id_lst
    | Typ12 ([AnyRange (lft, rght)], Typ5 (TypEnum3 [AnyRange (hi, lo)], elst), id_lst) ->
        List.iter (function
            | (Id nam) -> elabenum typhash nam elst
            | oth -> unhand := Some oth; failwith "enum array") id_lst
    | InstDecl (typ, params, lst) -> List.iter (function
        | (InstNameParen1 _ | InstNameParen2 _) -> ()
        | Id id ->
          print_endline ("InstDecl: "^id);
          addwire bufh typhash ([], Id id, Unsigned_vector(Intgr 31, Intgr 0));
        | oth -> unhand := Some oth; failwith "InstDecl1634") lst;
    | Typ9 (_, id_lst, Typ5 (TypEnum3 (AnyRange(lft,rght) :: []), elst)) ->
      List.iter (function
            | Id id ->
              print_endline ("Typ9(Enum3): "^id);
              addwire bufh typhash ([], Id id, Unsigned_vector(lft, rght));
            | oth -> unhand := Some oth; failwith "enum range") id_lst
    | DeclLogic (reg_lst) -> List.iter (function
	  | Id nam -> addwire bufh typhash ([], Id nam, Unsigned)
          | DeclAsgn (nam, AnyRange(first,last) :: []) -> addwire bufh typhash ([], nam, Unsigned_vector(first,last))
          | VarDeclAsgn (nam, expr) -> addwire bufh typhash ([], nam, Unsigned)
	  | oth -> unhand := Some oth; failwith "DeclLogic651"
        ) reg_lst;

(*
    | Itmlst (id_lst) -> List.iter (function
	  | Id nam -> update typhash (Id nam) Unsigned; bprintf buf' "  wire %s; // 622	\n" nam
	  | oth -> unhand := Some oth; failwith "DeclLogic647"
        ) id_lst;
    | DeclLogic2 (wire_lst, AnyRange (hi, lo) :: AnyRange (hi', lo') :: AnyRange (hi'', lo'') :: []) -> List.iter (function
	  | Id nam -> update typhash (Id nam) (Unsigned_vector(hi,lo));
	  bprintf buf' "  wire [%s : %s] [%s : %s] [%s : %s] %s ; // 645\n" (ceval typhash hi) (ceval typhash lo) (ceval typhash hi') (ceval typhash lo') (ceval typhash hi'') (ceval typhash lo'') nam
	  | oth -> unhand := Some oth; failwith "DeclWire") wire_lst
    | InstDecl (typ, params, lst) -> List.iter (function
        | (InstNameParen1 _ | InstNameParen2 _) -> ()
        | Id id ->     bprintf buf' "  attribute \\wiretype \"\\\\%s\"\n  wire width 32 \\%s\n" typ id
        | oth -> unhand := Some oth; failwith "InstDecl1750") lst;
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
        bprintf buf' "} %s;\n" (String.concat ", " (List.map (ceval typhash) inst_lst));
    | Typ6 (SUDecl (Atom "packed", lst)) ->
        bprintf buf' "typedef struct packed { // 703	\n";
        List.iter (struct_union buf' typhash) lst;
        bprintf buf' "};\n"
    | Typ7 (nam, Typ5 (Atom "logic", AnyRange(lft,rght) :: AnyRange(lft',rght') :: [])) -> update typhash (Id nam) (Vtyp nam);
        bprintf buf' "  wire %s; // 694	\n" nam
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
                   | AsgnPat lst -> List.iter (fun itm -> bprintf buf' "    parameter %s = %s; // 940\n" nam (ceval typhash itm)) lst
		   | PatMember1 (Id id, AsgnPat lst) -> List.iter (function
                      | PatMemberDflt expr -> bprintf buf' "    parameter %s = %s; // 941\n" nam (ceval typhash expr)
                      | AsgnPat (PatMemberDflt expr :: []) -> bprintf buf' "    parameter %s = %s; // 941\n" nam (ceval typhash expr)
		      | oth -> unhand := Some oth; failwith "ParamDecl'''") lst
		   | PatMember1 (Id id, (Id _ | Number _ | ExprOKL _ as x)) -> bprintf buf' "    %s: %s; // 942\n" id (ceval typhash x)
                   | (Number _ | Id _ as x) -> bprintf buf' "    parameter %s = %s; // 941\n" nam (ceval typhash x)
	           | oth -> unhand := Some oth; failwith "ParamDecl''") lst
           | (Number _ | Query _ | Expression _  as x) -> bprintf buf' "    parameter %s = %s; // 943\n" nam (ceval typhash x)
           | oth -> unhand := Some oth; failwith "ParamDecl'")
    | ParamDecl (LocalParamTyp (Typ3 (id, PackageRef (pkg, Atom "::") :: [])), ParamAsgn1 (nam, ceval) :: []) -> ()
    | ParamDecl (LocalParamTyp (Typ3 (id_t, AnyRange (lft, rght) :: [])), ParamAsgn1 (nam, InitPat lst) :: []) -> List.iter (function
        | AsgnPat lst -> List.iter (fun itm -> bprintf buf' "    parameter %s = %s; // 947\n" nam (ceval typhash itm)) lst
	| oth -> unhand := Some oth; failwith "ParamDecl") lst
    | ParamDecl (LocalParamTyp (Typ5 (Atom "logic", AnyRange (lft, rght) :: [])), [ParamAsgn2 (nam, [AnyRange (lft', rght')], InitPat lst)]) ->
        bprintf buf' "%s; // 773\n" nam
    | ParamDecl (LocalParamTyp (Typ5 (Atom "logic", AnyRange (lft, rght) :: AnyRange (lft', rght') :: [])), [ParamAsgn1 (nam, expr)]) ->
        bprintf buf' "%s = %s; // 775\n" nam (ceval typhash expr)
    | ParamDecl (LocalParamTyp (Typ6 (Atom ("bit"|"logic"))), lst) -> List.iter (function
	        | ParamAsgn1 (id, expr) -> ()
                | oth -> unhand := Some oth; failwith "param_asgn") lst
    | ParamDecl (LocalParamTyp (Typ8 (Atom ("int"|"integer"|"longint" as kind), Atom kind')), [ParamAsgn1 (nam , expr)]) ->
        bprintf buf' "localparam %s %s %s = %s; // 784\n" kind' kind nam (ceval typhash expr)
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

and decl_template bufh typhash modules pth itm =
    dbgdecl := itm :: !dbgdecl;
    decl_template' bufh typhash modules pth itm

let dbgnew = ref []

let split_always typhash' act_seq rst_seq evlst rst =
    let pref = {nonblk="nonblk1$";block="blk1$";typh=typhash'} in
    let pref' = {nonblk="nonblk2$";block="blk2$";typh=typhash'} in
    let rst' = {nonblk="$";block="$";typh=typhash'} in
    let subh = Hashtbl.create 255 in
    let attr = {Source_text_rewrite.fn=map_active_combined pref; subst=subh; pth=""} in
    let attr'' = {Source_text_rewrite.fn=map_reset pref'; subst=subh; pth=""} in
    let act_seq', act_seq'' = map_active_combined' pref attr act_seq in
    let rst_seq'' = map_reset rst' attr'' rst_seq in
    let newseq = AlwaysLegacy (AtStar, act_seq') in
    let newseq' = AlwaysLegacy (At (EventOr evlst), If2 (rst, rst_seq'', act_seq'')) in
    dbgalw := (act_seq', act_seq'', newseq, newseq') :: !dbgalw;
    dbgnew := [];
    let nlst = ref [] in
    Hashtbl.iter (fun k -> function
        | Id _ -> ()
        | Active (MaybePort (_, Unsigned_vector (hi, lo), dir), _, expr) as x ->
             dbgnew := (k,x) :: !dbgnew;
             nlst := DeclReg (k :: [], AnyRange (hi, lo) :: [], Deflt) :: !nlst;
        | Active (Unsigned_vector (hi, lo), _, expr) as x ->
             dbgnew := (k,x) :: !dbgnew;
             nlst := DeclReg (k :: [], AnyRange (hi, lo) :: [], Deflt) :: !nlst;
        | oth -> unhand := Some oth; failwith "split'") subh;
    List.sort compare !nlst @ newseq :: newseq' :: [] 

let rec split' typhash' = function
  | [] -> []
  | AlwaysLegacy (At (EventOr evlst), If2 (rst, rst_seq, act_seq)) as x :: tl ->
    split_always typhash' act_seq rst_seq evlst rst @ split' typhash' tl
  | AlwaysLegacy (At (EventOr ([Pos (clk); Pos (rst)] as evlst)), If2 (rst', rst_seq, act_seq)) as x :: tl when alweq (rst, rst') ->
    split_always typhash' act_seq rst_seq evlst rst @ split' typhash' tl
| oth :: tl -> oth :: split' typhash' tl

let bufhash () = {c=ref[];i=ref[];l=ref[];w=ref[]}

let dbgkind = ref ""
let dbgportlst = ref []

let rec parm_map typhash = function
  | PackageParam (lst, inner) -> ()
  | Param (nam, (Intgr n | Number (_, _, n, _)), []) ->
      update typhash (Id nam) (Vint n);
  | Param (nam, (Intgr n | Number (_, _, n, _)), AnyRange (left, rght) :: []) ->
      update typhash (Id nam) (Vint n);
  | Param (nam, String s, []) ->
      update typhash (Id nam) (Vstr s);
  | Param (nam, Dot1(lft,rght), []) ->
      update typhash (Id nam) (Vdot);
  | Param (nam, PackageBody (pkg, [Id id]), []) ->
      update typhash (Id nam) (Vtyp nam);
  | Param (nam, FunRef2 (fn, [PackageRef (pkg, Atom "::")], [Id id]), []) ->
      update typhash (Id nam) (Vfun fn);
  | Param (nam, UMinus (Number (_, _, n, _)), []) ->
      update typhash (Id nam) (Vint (-n));
  | Param (nam, UMinus (Number (_, _, n, _)), AnyRange (lft, rght) :: []) ->
      update typhash (Id nam) (Vint (-n));
  | Param (nam, ((Add _|Sub _|Mult _|Div _|StarStar _ |Sys _) as x), []) ->
      let n = ceval typhash x in
      update typhash (Id nam) (Vint n);
  | CellParamItem2 (nam, (Intgr n | Number (_, _, n, _))) ->
      update typhash (Id nam) (Vint n);
  | CellParamItem2 (nam, (Id _ as s)) -> let n = ceval typhash s in
      update typhash (Id nam) (Vint n);
  | CellParamItem2 (nam, String s) ->
      update typhash (Id nam) (Vstr s);
  | CellParamItem2 (nam, Split(Typ8 (SUDecl (Atom "packed", lft), Deflt), InitPat rght)) ->
      List.iter2 (fun lft rght -> dbgpair := (lft,rght) :: !dbgpair) lft rght;

      (*
      update typhash (Id nam) (Vstr s);

  | CellParamItem1 (nam, s) ->
      update typhash nam (match Hashtbl.find_opt typhash s with Some x -> x | None -> Unsigned);
      sprintf "%24s         : string := %s" nam s
  | CellParamItem2 (nam, Typ1 s) ->
      sprintf "%24s         => %s" nam s
  | CellParamItem2 (nam, Typ3(id_t, PackageBody (pkg,[]) :: [])) ->
      sprintf "%24s         : type := %s" nam id_t
  | CellParamItem2 (nam, Typ5(Atom "logic", AnyRange(lft,rght) :: [])) ->
      sprintf "%24s         => logic[%s : %s]" nam (ceval typhash lft) (ceval typhash rght)
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
  | CellParamItem3 (nam, Typ3(id_t, PackageRef (pkg, Atom "::") :: [])) ->
      sprintf "%24s         => %s" nam id_t
 | PackageParam (lst, inner) -> String.concat ", " (List.map (function PkgImport (Itmlst [PkgImportItm (pkg, Atom "*")]) -> parm_map typhash inner | _ -> "") lst)
  | PackageParam2 (grp_e, nam, [PackageRef (pkg, Atom "::")], PackageBody (pkg', [Id s])) ->
      update typhash nam (Venum grp_e);
      sprintf "%24s         => %s" grp_e s
  | PackageParam2 (id_t, nam, [PackageRef (pkg, Atom "::")], Number (_, _, n, _)) ->
      update typhash nam (Vint n);
      sprintf "%24s         => %d" nam n
  | PackageParam2 (id_t, nam, [PackageRef (pkg, Atom "::")], AsgnPat [PatMemberDflt (Number (_, _, n, ""))]) ->
      update typhash nam (Vint n);
      sprintf "%24s         => %d" nam n
  | PackageParam2 (id_t, nam, [PackageRef (pkg, Atom "::")], AsgnPat [PatMemberDflt (PackageBody (pkg', [Id id]))]) ->
      update typhash nam (Vtyp id_t);
      sprintf "%24s         => %s" nam id
  | PackageParam2 (id_t, nam, [PackageRef (pkg, Atom "::")], ExprQuote1 (Typ3(id, PackageBody (pkg', []) :: []), expr)) ->
      update typhash nam (Vtyp id_t);
      sprintf "%24s         => %s" nam id
  | TypParam (nam, Atom typ, []) ->
      update typhash nam (Vtyp nam); 
      sprintf "%24s         => %s" nam typ
  | TypParam (nam, Id id_t, PackageRef (pkg, Atom "::") :: []) ->
      update typhash nam (Vtyp nam); 
      sprintf "%24s         => %s" nam id_t
*)
  | oth -> unhand := Some oth; failwith "parm_map"

let module_header modules = function
| Modul (typ, parm_lst, port_lst, body_lst) ->
  let typhash = Hashtbl.create 255 in
  let bufh = bufhash () in
  dbgkind := typ;
  dbgportlst := port_lst;
  
  dbgtyp := typhash;
  List.iter (fun itm -> let _ = parm_map typhash itm in ()) parm_lst;
  dbgports := [];
  List.iteri (fun ix itm -> dbgports := itm :: !dbgports; ports typhash (ix+1) itm) port_lst;
  dbgdecl := [];
  List.iter (fun itm -> decl_template bufh typhash modules None itm) body_lst;
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

let typsplt = ref []

let split' = function
| Modul (a, b, c, lst) as x -> 
  let bufh', typhash', ports' = module_header [] x in
  typsplt := [];
  Hashtbl.iter (fun k x -> typsplt := (k,x) :: !typsplt) typhash';
  Modul (a, b, c, split' typhash' lst)
 | oth -> failwith "split'"

let sub' x =
   let subst' = Hashtbl.create 255 in
   recurhash := subst';
   let attr = {Source_text_rewrite.fn=recurs1; subst=subst'; pth=""} in
   let pass1 = Source_text_rewrite.descend' attr (recurs1 attr x) in
   dbgsub := Some pass1;
   let pass2 = recurs2 {fn=recurs2; subst=subst'; pth=""} pass1 in
   dbgsub' := Some pass2;
   let pass2' = split' pass2 in
   let pass3 = recurs3 {fn=recurs3; subst=subst'; pth=""} pass2' in
   let attr = {Source_text_rewrite.fn=simplify'; subst=subst'; pth=""} in
   let pass4 = simplify' attr (simplify' attr (simplify' attr (simplify' attr (simplify' attr (pass3))))) in
   dbgsub := Some pass4;
   dbgsubst := [];
   Hashtbl.iter (fun k x -> dbgsubst := (k,x) :: !dbgsubst) subst';
   pass4

let rec vexpr typhash = function
| oth -> unhand := Some oth; failwith "vexpr"

let funtyp typhash = function
| Atom primtyp -> primtyp
| Typ1 id_t -> id_t
| Typ3 (id_t, [PackageRef (pkg, Atom "::")]) -> pkg^"::"^id_t
| Typ5 (Atom primtyp, AnyRange (lft, rght) :: []) -> sprintf "%s [%d:%d]" primtyp (ceval typhash lft) (ceval typhash rght)
| Typ6 (Atom primtyp) -> primtyp
| Typ8 (Atom kind, Atom kind') -> kind' ^ kind
| Typ8 (Atom kind, Deflt) -> kind
| oth -> unhand := Some oth; failwith "funtyp"

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

(*
let sel_expr typhash x = match simplify attr x with Intgr _ -> "'0'" | oth -> vexpr typhash oth
*)

let rec parm_generic typhash = function
  | CellParamItem2 (nam, Typ1 s) ->
      sprintf "%24s         : type := %s" nam s
  | CellParamItem2 (nam, Typ3(id_t, PackageBody (pkg,[]) :: [])) ->
      sprintf "%24s         : type := %s" nam id_t
  | CellParamItem2 (nam, Typ5(Atom "logic", AnyRange(lft,rght) :: [])) ->
      sprintf "%24s         : type := logic [%d : %d]" nam (ceval typhash lft) (ceval typhash rght)
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
  | CellParamItem3 (nam, Typ3(id_t, PackageRef (pkg, Atom "::") :: [])) ->
      sprintf "%24s         : type := %s" nam id_t
  | Param (nam, Number (_, _, n, _), []) ->
      update typhash (Id nam) (Vint n);
      sprintf "%24s         : integer := %d" nam n
  | PackageParam2 (id_t, nam, [], Id s) ->
      update typhash (Id nam) (Vtyp id_t);
      sprintf "%24s         => %s" id_t s
  | PackageParam2 (grp_e, nam, [PackageRef (pkg, Atom "::")], PackageBody (pkg', [Id s])) ->
      update typhash (Id nam) (Venum grp_e);
      sprintf "%24s         => %s" grp_e s
  | PackageParam2 (id_t, nam, [PackageRef (pkg, Atom "::")], Number (_, _, n, _)) ->
      update typhash (Id nam) (Vint n);
      sprintf "%24s         => %d" nam n
  | PackageParam2 (id_t, nam, [PackageRef (pkg, Atom "::")], AsgnPat [PatMemberDflt (Number (_, _, n, ""))]) ->
      update typhash (Id nam) (Vint n);
      sprintf "%24s         => %d" nam n
  | PackageParam2 (id_t, nam, [PackageRef (pkg, Atom "::")], AsgnPat [PatMemberDflt (PackageBody (pkg', [Id id]))]) ->
      update typhash (Id nam) (Vtyp id_t);
      sprintf "%24s         => %s" nam id
  | PackageParam2 (id_t, nam, [PackageRef (pkg, Atom "::")], ExprQuote1 (Typ3(id, PackageBody (pkg', []) :: []), expr)) ->
      update typhash (Id nam) (Vtyp id_t);
      sprintf "%24s         => %s" nam id
  | TypParam (nam, Atom typ, []) ->
      update typhash (Id nam) (Vtyp nam); 
      sprintf "%24s         => %s" nam typ
  | TypParam (nam, Id id_t, PackageRef (pkg, Atom "::") :: []) ->
      update typhash (Id nam) (Vtyp nam); 
      sprintf "%24s         => %s" nam id_t
  | TypParam (nam, Atom typ, AnyRange (lft, rght) :: []) ->
      update typhash (Id nam) (Unsigned_vector(lft,rght));
      sprintf "%24s         => %s[%d : %d]" nam typ (ceval typhash lft) (ceval typhash rght)
  | Param (nam, PackageBody (pkg, [Id id]), []) ->
      update typhash (Id nam) (Vtyp nam);
      sprintf "%24s         => %s" nam id
  | Param (nam, FunRef2 (fn, [PackageRef (pkg, Atom "::")], expr :: []), []) ->
      update typhash (Id nam) (Vfun fn);
      sprintf "%24s         => %d" nam (ceval typhash expr)
  | Param (nam, String s, []) ->
      update typhash (Id nam) (Vstr s);
      sprintf "%24s         => %s" nam s
  | Param (nam, Dot1(lft,rght), []) ->
      update typhash (Id nam) (Vdot);
      sprintf "%24s         => %d.%d" nam (ceval typhash lft) (ceval typhash rght)
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
    bprintf buf' "    type Mem_Type%d is array [%d : %d] of logic[%d : %d]; // 591	\n" !cnt (ceval typhash last) (ceval typhash first) (ceval typhash hi) (ceval typhash lo);
    bprintf buf' "    signal %s : Mem_Type%d := (others => (others => '0')); // 592	\n" mem !cnt;
incr cnt

let fn_arg typhash = function
(*
		   | PortItem (Typ5 (Atom primtyp, AnyRange (lft, rght) :: []), ItemAsgn (Id id)) ->
                   sprintf "%s [%s:%s] %s" primtyp (ceval typhash lft) (ceval typhash rght) id
		   | PortItem (Typ1 (id_t), ItemAsgn (Id id)) ->
                   sprintf "%s %s" id_t id;
		   | PortItem (Typ6 (Atom primtyp), ItemAsgn (Id id)) ->
                   sprintf "%s %s" primtyp id;
		   | PortFront (PortItemFront (dir, Typ1 id_t), ItemAsgn (Id id)) ->
                   sprintf "%s %s %s" (vdir 0 dir) id_t id
                   | PortFront (PortItemFront (dir, Typ5 (Atom primtyp, AnyRange(lft,rght) :: [])), ItemAsgn (Id id)) ->
                   sprintf "%s %s [%s:%s] %s" (vdir 0 dir) primtyp (ceval typhash lft) (ceval typhash rght) id
		   | PortItem (Typ8 (Atom primtyp, kind'), ItemAsgn (Id id)) ->
                   sprintf "%s %s %s" (match kind' with Atom typ -> typ | oth -> "") primtyp id;
		   *)
		   | Deflt -> ""
                   | oth -> unhand := Some oth; failwith "fn_arg"

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
bprintf buf' "%s @(%s)\n" kind (String.concat " or " (List.map (ceval typhash) lst))

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

let catbuf bufh = !(bufh.w) @ !(bufh.i) @ !(bufh.c) @ !(bufh.l)

let oth_lab = ref []
let dbg_lbls = ref []
let dbg_chk = ref None
let dbgseq = ref None
let dbgedg = ref None
let dbgboth = ref None
let fail_lbls = ref 0

let rec chk_lbls checked = function
  | [] -> ()
  | [[]] -> ()
  | (TokVal _ :: _ as arglst) :: tl ->
    List.iter (function
          | TokVal arg ->
            if List.mem arg checked then
              begin
                dbg_chk := Some (arglst, tl, arg, checked);
                fail_lbls := 2054;
                print_endline "chk_lbls_2054";
              end
            else chk_lbls (arg :: checked) tl
          | oth -> fail_lbls := 2057; print_endline "chk_lbls_2057") arglst;
  | oth -> oth_lab := oth; failwith "chk_lbls_2058"

let proc_stmt = function
| (inst, plst, (Switch_stmt ([TokID rst'], _, _, _) :: _ as ulst),
    (Sync_list69 ([TokPos], [TokID clk], [], ulst') ::
     Sync_list69 ([TokPos], [TokID rst], [], ulst'') :: [] as edglst)) ->
  dbgboth := Some (clk, rst, rst', plst, ulst, ulst', ulst'');
  if rst <> rst' then
    let edglst' = Sync_list69 ([TokPos], [TokID clk], [], ulst') ::
                  Sync_list69 ([TokPos], [TokID rst'], [], ulst'') :: [] in
    Proc_stmt (inst, [], List.sort_uniq compare plst @ ulst, edglst')
  else
    Proc_stmt (inst, [], List.sort_uniq compare plst @ ulst, edglst)
| (inst, plst, ulst, edglst) ->
    dbgedg := Some (inst, plst, ulst, edglst);
    Proc_stmt (inst, [], List.sort_uniq compare plst @ ulst, edglst)

let rhswid bufh typhash lhs rhs =
  let lhswid = width typhash lhs in
  let rhswid = width typhash rhs in
  let minwid' = if lhswid < rhswid then restrict typhash lhswid rhs
                        else if lhswid > rhswid then Concat (Number(2,lhswid-rhswid,0,"") :: rhs :: [])
                        else rhs in
  let rhs' = buffer' bufh typhash minwid' 0 in
  lhswid, rhswid, rhs'

let rec generate_assignment_common update bufh typhash (lhs, rhs) : ilang list =
let lhswid, rhswid, rhs' = rhswid bufh typhash lhs rhs in
match lhs with
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
      memwr'' update bufh typhash (mem_opt typhash mem) mem addr rhs' en
  | _ -> if update then TokUpdate(tran' typhash lhs, tran' typhash rhs') :: [] else Assign_stmt67(tran' typhash lhs, tran' typhash rhs') :: [])
| IdArrayedColon (IdArrayed2 (Id mem, (Id _ as addr)), (Intgr hi|Number(_,_,hi,_)), (Intgr lo|Number(_,_,lo,_))) ->
(match Hashtbl.find_opt typhash mem with
  | Some Vmem {off;siz;wid} ->
      dbgeq := (lhswid,rhswid,lhs,rhs') :: !dbgeq;
      let en = Number(2,wid,(1 lsl wid) - 1,"") in
      let lo = Number (2, wid, ((1 lsl wid)-1) lsl lo, "") in
      memwr'' update bufh typhash (mem_opt typhash mem) mem addr rhs' en
  | _ -> if update then TokUpdate(tran' typhash lhs, tran' typhash rhs') :: [] else Assign_stmt67(tran' typhash lhs, tran' typhash rhs') :: [])
| IdArrayed1 (Id mem, addr, Number(_,_,abit,_)) ->
(match Hashtbl.find_opt typhash mem with
  | Some Vmem {off;siz;wid} ->
      dbgeq := (lhswid,rhswid,lhs,rhs') :: !dbgeq;
      let en = Number(2,wid,(1 lsl wid) - 1,"") in
      memwr'' update bufh typhash (mem_opt typhash mem) mem addr rhs' en
  | _ -> if update then TokUpdate(tran' typhash lhs, tran' typhash rhs') :: [] else Assign_stmt67(tran' typhash lhs, tran' typhash rhs') :: [])
| IdArrayed1 (Id mem, addr, abit) ->
(match Hashtbl.find_opt typhash mem with
  | Some Vmem {off;siz;wid} ->
      dbgeq := (lhswid,rhswid,lhs,rhs') :: !dbgeq;
      let en = Number(2,wid,(1 lsl wid) - 1,"") in
      memwr'' update bufh typhash (mem_opt typhash mem) mem addr rhs' en
  | _ -> if update then TokUpdate(tran' typhash lhs, tran' typhash rhs') :: [] else Assign_stmt67(tran' typhash lhs, tran' typhash rhs') :: [])
| IdArrayed2 (Id mem, (Number _ as addr)) ->
(match Hashtbl.find_opt typhash mem with
  | Some Vmem {off;siz;wid} ->
      dbgeq := (lhswid,rhswid,lhs,rhs') :: !dbgeq;
      let en = Number(2,wid,(1 lsl wid) - 1,"") in
      memwr'' update bufh typhash (mem_opt typhash mem) mem addr rhs' en
  | Some Unsigned_vector _ -> if update then TokUpdate(tran' typhash lhs, tran' typhash rhs') :: [] else Assign_stmt67(tran' typhash lhs, tran' typhash rhs') :: []
  | oth -> otha := Some oth; failwith "arr")
| IdArrayed2 (Id mem, addr) as x ->
dbgsplit := Some x;
(match Hashtbl.find_opt typhash mem with
  | Some Vmem {off;siz;wid} ->
      dbgeq := (lhswid,rhswid,lhs,rhs') :: !dbgeq;
      let en = Number(2,wid,(1 lsl wid) - 1,"") in
      memwr'' update bufh typhash (mem_opt typhash mem) mem addr rhs' en
  | Some Unsigned_vector _ ->
    let inst, (p,u,s) = asgn'' bufh typhash (Id mem) addr rhs' in
    dbgasgn := (p,u,s) :: !dbgasgn;
    if update then s else (p @ u)
  | oth -> otha := Some oth; failwith "arr")
| oth -> dbgcommon := Some (lhs,rhs'); if update then TokUpdate(tran' typhash lhs, tran' typhash rhs') :: [] else Assign_stmt67(tran' typhash lhs, tran' typhash rhs') :: []

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
    | DeclData _ as x -> decl_template bufh typhash [] None x; ([],[],[])
    | DeclInt2 _ -> ([],[],[])
    | Itmlst lst ->
        let lst' = List.map (cnv' bufh dhash typhash inst) lst in
        (List.flatten (List.map (fun (p,u,d) -> p) lst'),
         List.flatten (List.map (fun (p,u,d) -> u) lst'),
         List.flatten (List.map (fun (p,u,d) -> d) lst'))
    | Seq (lbl, lst) ->
        let lst' = List.map (fun itm ->
          let rslt = cnv' bufh dhash typhash inst itm in
          dbgseq := Some (itm,rslt);
          if !fail_lbls > 0 then failwith ("lbl: "^string_of_int !fail_lbls);
          rslt) lst in
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
    | EquateSelect (Id lhs', sel, expr) ->
        let rhs = buffer' bufh typhash expr 0 in
        let sel' = buffer' bufh typhash sel 0 in
        let lhs = IdArrayed2(Id lhs', sel') in
        let lhswid, rhswid, rhs' = rhswid bufh typhash lhs rhs in
        (match Hashtbl.find_opt typhash lhs' with
         | Some Vmem {off;siz;wid} ->
           let en = Number(2,wid,(1 lsl wid) - 1,"") in
           let options = mem_opt typhash lhs' in
           let abits = clog2 options.tot in
           let addr' = newid bufh typhash abits in
           let data' = newid bufh typhash options.wid in
           let en' = newid bufh typhash options.wid in
           let zen = Number(2,wid,0,"") in
           let zdata = Number(2,wid,0,"") in
           let zsel = Number(2,abits,0,"") in
           let tuple = (memwr' bufh typhash zsel zdata zen addr' data' en',
           ([]:ilang list),
            memwr bufh typhash options lhs' abits data' en addr' data' en' sel') in
           dbgmem := (lhswid,rhswid,lhs,rhs,rhs',tuple) :: !dbgmem;
           tuple
         | Some Unsigned_vector _ ->
           let dly = dlymemo bufh dhash typhash lhs in
           (generate_assignment_common false bufh typhash (dly, lhs),
            generate_assignment_common false bufh typhash (IdArrayed2(dly,sel), rhs),
            generate_assignment_common true bufh typhash (lhs, dly))
         | oth -> otha := Some oth; failwith "arr")
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
    | If2(cond, if_clause, else_clause) -> (match dead_code cond with
     | Always_true -> cnv' bufh dhash typhash inst if_clause
     | Always_false -> cnv' bufh dhash typhash inst else_clause
     | Undecidable ->
      cnv' bufh dhash typhash inst (CaseStart (CaseStart1 (cond),
       (CaseStmt ([Number(2,1,1,"")], if_clause :: []) ::
        CaseStmt ([], else_clause :: []) :: []))))
    | If1(cond, if_clause) ->
      cnv' bufh dhash typhash inst (CaseStart (CaseStart1 (cond),
       (CaseStmt ([Number(2,1,1,"")], if_clause :: []) :: [])))
    | CaseStmt (caselbl, itm_stmts) ->
        let lbl' = List.map (simplify_default bufh typhash) caselbl in
        let lst' = List.map (cnv' bufh dhash typhash inst) itm_stmts in
	let wid = width typhash inst in
        let case_args = List.flatten(List.map (fun itm -> tran' typhash (restrict typhash wid itm)) lbl') in
        (List.flatten (List.map (fun (p,u,d) -> p) lst'),
         Switch_bodycase (case_args, [], List.flatten (List.map (fun (p,u,d) -> u) lst')) :: [],
         List.flatten (List.map (fun (p,u,d) -> d) lst'))
    | CaseStart (CaseStart1 expr, stmts) ->
        let expr' = buffer'' bufh typhash expr in
        let lst' = List.map (cnv' bufh dhash typhash expr) stmts in
        dbgcase' := lst';
        let body_stmts = List.flatten (List.map (fun (p,u,d) -> u) lst') in
        let lbls = List.map (function Switch_bodycase (lbls, _, _) -> lbls | oth -> failwith "cases") body_stmts in
        dbg_lbls := lbls;
        chk_lbls [] lbls;
        (List.flatten (List.map (fun (p,u,d) -> p) lst'),
         Switch_stmt (tran' typhash expr', [], [], body_stmts) :: [],
         List.flatten (List.map (fun (p,u,d) -> d) lst'))
    | Id t when Hashtbl.mem typhash t -> if verbose > 0 then print_endline ("executed task: "^t); ([],[],[])
    | SysTaskRef (Atom ("$display"|"$fwrite"), _) -> ([],[],[])
    | oth -> unhand := Some oth; failwith "cnv'"

and asgn bufh typhash expr = function
  | IdArrayed2 (Id _ as arr, (Id _ as sel)) ->
    let inst, (p,u,s) = asgn'' bufh typhash expr arr sel in
    let sync_lst = List.sort_uniq compare s in
    bufh.l := proc_stmt (inst, p, u, mapedge sync_lst AlwaysSync) :: !(bufh.l)
  | lhs ->
    let wid = width typhash lhs in
    let rhs = buffer' bufh typhash expr wid in
    let rhs' = restrict typhash wid rhs in
    bufh.c := Conn_stmt96(tran' typhash lhs, tran' typhash rhs') :: !(bufh.c)

and asgn' bufh dhash typhash inst arr sel expr =
    let wida = width typhash arr in
    let wids = width typhash sel in
    let wid = min wida (1 lsl wids) in
    dbgmem'' := Some arr;
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

let rec proc_template bufh typhash modules = function
    | Seq(lbl, lst) -> List.iter (proc_template bufh typhash modules) lst
    | Itmlst lst -> List.iter (proc_template bufh typhash modules) lst
    | DeclReg _ -> ()
    | DeclLogic _ -> ()
    | AlwaysLegacy (At (EventOr ((Pos _|Neg _) :: _ as edglst)), body)
    | AlwaysFF (At (EventOr ((Pos _|Neg _) :: _ as edglst)), body) ->
    let dhash = Hashtbl.create 255 in
    let inst = newnam() in
    let (p,u,s) = cnv' bufh dhash typhash (Id inst) body in
    dbgproc := Some (typhash, dhash, inst, edglst, body, p, u, s);
    let sync_lst = List.sort_uniq compare s in
    bufh.l := proc_stmt (inst, p, u, List.flatten (List.map (mapedge sync_lst) edglst)) :: !(bufh.l)
    | AlwaysLegacy (AtStar, body) ->
    let dhash = Hashtbl.create 255 in
    let inst = newnam() in
    let (p,u,s) = cnv' bufh dhash typhash (Id inst) body in
    dbgproc := Some (typhash, dhash, inst, [], body, p, u, s);
    let sync_lst = List.sort_uniq compare s in
    bufh.l := proc_stmt (inst, p, u, mapedge sync_lst AlwaysSync) :: !(bufh.l)
    | AlwaysLegacy (At (EventOr (Id _ :: _)), body) ->
    let dhash = Hashtbl.create 255 in
    let inst = newnam() in
    let (p,u,s) = cnv' bufh dhash typhash (Id inst) body in
    dbgproc := Some (typhash, dhash, inst, [], body, p, u, s);
    let sync_lst = List.sort_uniq compare s in
    bufh.l := proc_stmt (inst, p, u, mapedge sync_lst AlwaysSync) :: !(bufh.l)
   (* elaboration case *)
   | CaseStart (Id id, (CaseItm (BeginBlock [] :: Unknown ("$error",_) :: Deflt :: []) :: [])) -> bufh.l := Switch_bodycase([], [], []) :: !(bufh.l)
    | ContAsgn lst -> List.iter (function
      | Asgn1 (lhs, expr) -> asgn bufh typhash expr lhs
      | oth -> unhand := Some oth; failwith "assign_template") lst
    | Iff _ -> ()
    | InstDecl (Typ5 (TypEnum3 [AnyRange (hi, lo)], elst), [], id_lst) -> List.iter (function
        | Id _ as id -> addwire bufh typhash ([], id, Unsigned)
        | oth -> unhand := Some oth; failwith "InstDecl2371") id_lst
    | InstDecl (Typ5 (Deflt, elst), [], id_lst) -> List.iter (function
        | Id _ as id -> addwire bufh typhash ([], id, Unsigned)
        | oth -> unhand := Some oth; failwith "InstDecl2374") id_lst
    | InstDecl (Id typ, params, lst) -> List.iter (function
        | InstNameParen1 (inst, pins) -> bufh.i := instance_template bufh typhash ("\\"^typ) (match params with Itmlst lst :: _ -> lst | _ -> []) inst pins :: !(bufh.i)
        | InstNameParen2 (inst, InstRange(lft,rght) :: []) -> bufh.i := instance_template bufh typhash ("\\"^typ) (match params with Itmlst lst :: _ -> lst | _ -> []) inst [] :: !(bufh.i)
        | Id _ as id -> addwire bufh typhash ([], id, Unsigned)
        | oth -> unhand := Some oth; failwith "InstDecl2260") lst;
    | InstArrayDecl (Id typ, params, Id inst, [InstRange(hi,lo)], pinlst) as x -> (dbginst := Some x; match List.assoc_opt typ !modules with
        | Some m ->
            let bufh', typhash', ports' = module_header modules m in
            dbgarr := Some (typ, params, inst, hi, lo, bufh', ports');
            for ix = ceval typhash hi downto ceval typhash lo do
              let _ = List.rev_map (parm_map typhash) params in
                bufh.i := Cell_stmt ("\\"^typ, inst^"["^string_of_int ix^"]", [],
                   List.rev_map2 (fun (pin,sgn,rng) -> function
		       | CellPinItem2 (pin, Intgr n) -> TokConn([TokID(pin)], tran' typhash (Number(2, 32, n, "")))
		       | CellPinItem2 (pin, conn) -> TokConn([TokID(pin)], tran' typhash conn)
                       | Id id as conn ->
		         let wid = width typhash conn in
		         print_endline (id^": "^string_of_int wid);
			 TokConn([TokID(pin)], tran' typhash (match wid with 1 -> conn | _ -> IdArrayed2 (conn, Intgr ix)))
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
    | Typ9 _ -> ()
    | Typ11 _ -> ()
    | Typ12 _ -> ()
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
  typhash, List.rev (!bufm);
  | PackageBody (pkg, body_lst) ->
  let typhash = Hashtbl.create 255 in
  let bufh = bufhash () in
  List.iter (decl_template bufh typhash modules None) body_lst;
  typhash, List.rev (catbuf bufh);
  | oth -> unhand := Some oth; failwith "This template only handles modules/packages"

let template modules = function
  | Modul _ as m ->
    let m' = sub' m in
    let typhash, t = template modules m' in flush logf;
    typhash, m', t
  | PackageBody (pkg, body_lst) as p ->
    if verbose > 1 then print_endline ("Package "^pkg^" is pending");
    Hashtbl.create 1, sub' p, []
  | oth -> unhand := Some oth; failwith "This template only handles modules/packages"

