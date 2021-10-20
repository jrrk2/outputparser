open Source_text_rewrite_types
open Source_text_rewrite
open Source_text_split
open Source_text_misc
open Source_text_lex
open Source_text
open Printf

type map = {nonblk:string; block:string; typh:(string,vtyp)Hashtbl.t; init:(rw,unit)Hashtbl.t}

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

let unhand_rtl = ref None
let unhand_lst = ref []
let unhand_pkg = ref None
let unhand_inst_lst = ref []
let dbgfield = ref None
let dbgrepl = ref []
let dbgsu = ref None
let coth = ref None
let (dbgtyp:(string,vtyp)Hashtbl.t ref)  = ref (Hashtbl.create 1)

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
| oth -> unhand_rtl := Some oth; failwith "ceval"

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
| Vemember(s, _, typ, _) -> csiz' typhash typ
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
| oth -> unhand_rtl := Some oth; failwith "width"

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
| oth -> unhand_rtl := Some oth; failwith "signof"

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
| oth -> unhand_rtl := Some oth; failwith "is_const"

let othx = ref None
let dbgfind = ref []
let dbgform = ref []
let dbgact = ref []
let dbgloop = ref None
let dbgmodinst = ref []
let recurhash = ref (Hashtbl.create 1)
let dbgsub = ref None
let dbgsub' = ref None
let dbgsubst = ref []
let dbgalw = ref []
let dbgdecl = ref []
let dbgcase = ref []
let dbgwida = ref 0
let dbgwids = ref 0
let dbgpar = ref []
let dbgatom = ref ""
let dbgpkg = ref None
let dbgpair = ref []
let dbgalweq = ref None
let dbgmap = ref None
let dbgtyph = ref (Hashtbl.create 1)

let alweq = function
| Id id, Id id' -> id=id'
| Id id, Expression (Equals (Id id', Number (2, 1, 1, "1"))) -> id=id'
| oth -> dbgalweq := Some oth; failwith "alweq"

let mapadd map (attr:attr) pref lhs expr =
  let typ' = match Hashtbl.find_opt map.typh lhs with
      | Some typ' -> typ'
      | None ->
        dbgtyph := map.typh;
        dbgmap := Some lhs;
        failwith "mapadd" in
  let newid = Id (pref^lhs) in
  Hashtbl.add attr.subst (Id lhs) newid;
  Hashtbl.add attr.subst newid (Active(typ', newid, expr));
  newid

let map_id attr id = match Hashtbl.find_opt attr.subst id with Some repl -> repl | None -> id

let rec map_active_combined' (map:map) (attr: Source_text_rewrite.attr) : (rw -> rw * rw) = function
| Equate (Id lhs as lhs', expr) ->
  let expr',_ = map_active_combined' map attr expr in
  let lhs3 = mapadd map attr map.nonblk lhs expr' in
  Hashtbl.add map.init (Blocking(FopAsgn(lhs3, lhs'))) ();
  (Blocking(FopAsgn(lhs3, expr')), Equate(lhs', lhs3))
| Blocking (FopAsgn (Id lhs as lhs', expr)) ->
  let expr',_ = map_active_combined' map attr expr in
  let lhs3 = mapadd map attr map.nonblk lhs expr' in
  Hashtbl.add map.init (Blocking(FopAsgn(lhs3, lhs'))) ();
  (Blocking (FopAsgn(lhs3, expr')), Equate(lhs', lhs3))
| Id _ as id -> let rslt = map_id attr id in (rslt, rslt)
| oth -> split_pair (Source_text_rewrite.descend' attr oth)

let rec map_active_combined (map:map) (attr: Source_text_rewrite.attr) x =
  let (comb, seq) = map_active_combined' map attr x in
  Split (comb, seq)

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
        | oth -> unhand_rtl := Some oth; failwith "search_pkg_enum") lst
| ParamDecl (Atom "localparam", lst) -> List.iter (function
    | ParamAsgn1 (id, contents) -> if key=id then rslt := Some contents
    | oth -> ()) lst
| ParamDecl (LocalParamTyp _, lst) -> List.iter (function
    | ParamAsgn1 (id, contents) -> if key=id then rslt := Some contents
    | oth -> ()) lst
| FunDecl (id, typ, contents) -> if key=id then rslt := Some contents
| AutoFunDecl (id, typ, contents) -> if key=id then rslt := Some contents
| oth -> unhand_rtl := Some oth; failwith "search_pkg"

and import_pkg pkg_id =
  match List.assoc pkg_id !(Matchmly.modules) with
      | PackageBody (pkg_id', lst) when pkg_id = pkg_id' -> lst
      | oth -> unhand_rtl := Some oth; failwith "import_pkg"

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
  | oth -> unhand_rtl := Some oth; failwith "recur_param"

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
	| oth, _ -> unhand_rtl := Some oth; failwith "signcnv"

let array_port typhash ix hi lo dir nam =
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
	       | oth -> unhand_rtl := Some oth; failwith "SUMember5") lst
	   | SUMember (Typ5 (Atom kind, AnyRange (lft, rght) :: []), lst) -> List.map (function
               | Id id -> (id, Unsigned_vector(lft, rght))
	       | oth -> unhand_rtl := Some oth; failwith "SUMember2") lst
           | SUMember (Typ6 (Typ5 (Atom ("logic"), [AnyRange (lft, rght)])), id_lst) -> List.map (function
               | Id id -> (id, Unsigned_vector(lft, rght))
	       | oth -> unhand_rtl := Some oth; failwith "SUMember7") id_lst
           | SUMember (Typ5 (TypEnum3 [AnyRange (lft, rght)], eid_lst), id_lst) -> List.map (function
               | Id id -> (id, Unsigned_vector(lft, rght))
	       | oth -> unhand_rtl := Some oth; failwith "SUMember8") id_lst
           | SUMember (TypEnum6 (id_t, TypEnum3 [AnyRange (lft, rght)], eid_lst), id_lst) ->  List.map (function
               | Id id -> (id, Unsigned_vector(lft, rght))
	       | oth -> unhand_rtl := Some oth; failwith "SUMember9") id_lst
           | SUMember (Typ8 (SUDecl (Atom "packed", lst), Deflt), id_lst) -> List.map (function
               | Id id -> (id, Vsu (Id id, List.flatten (List.map (struct_union typhash) lst)))
	       | oth -> unhand_rtl := Some oth; failwith "SUMember9") id_lst
           | oth -> unhand_rtl := Some oth; failwith "SUMember"

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
    | oth -> unhand_rtl := Some oth; failwith "component"

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
  | ForLoop _ as x -> unhand_rtl := Some x; failwith "unroll"
  | LoopGen1 (Id ix, lbl, strt, stop, inc, body) ->
      recurs2 attr (iter attr ix lbl (elabeval attr strt) stop (elabeval attr inc) (Itmlst body))
  | LoopGen1 _ as oth -> unhand_rtl := Some oth; failwith "LoopGen1"
  | BeginBlock lst -> Seq("", List.map (recurs2 attr) lst)
  | ContAsgn (Asgn1 (Id _ as lhs, FunRef (id, arglst)) :: []) ->
      Hashtbl.add attr.subst (Id id) lhs;
      let rslt = AlwaysLegacy(AtStar, fsubst attr arglst id) in
      Hashtbl.remove attr.subst (Id id);
      rslt
  | CondGen1 (cond, true', false') -> recurs2 attr (if elabeval attr cond <> 0 then true' else false')
  | FunRef (id, arglst) -> fsubst attr arglst id
  | TaskRef (id, arglst) -> tsubst attr arglst id
  | FunDecl(id, _, _) -> FunDecl(id, Deflt, Deflt)
  | AutoFunDecl(id, _, _) -> AutoFunDecl(id, Deflt, Deflt)
  | TaskDecl(id, _, _, _) -> TaskDecl(id, Deflt, Deflt, Deflt)
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
       | oth -> unhand_rtl := Some oth; failwith "loop term" in
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
          | oth -> unhand_rtl := Some oth; failwith "param1") formlst
    | TF_port_decl (In, [Atom "integer"], formlst) -> List.map (function
          | TF_variable (Id _ as formal, Deflt, Deflt, Deflt) -> formal
          | oth -> unhand_rtl := Some oth; failwith "param2") formlst
    | BlockItem (ParamDecl (Atom "Parameter", lst)) -> List.iter (function
	  | ParamAsgn1 (id, expr) -> Hashtbl.add attr.Source_text_rewrite.subst (Id id) expr
          | oth -> unhand_rtl := Some oth; failwith "param") lst; []
    | DeclInt2 formlst -> List.map (function
	  | Id _ as formal -> formal
          | oth -> unhand_rtl := Some oth; failwith "param3") formlst;
    | oth -> unhand_rtl := Some oth; failwith "fsubst'") formlst') in
  printf "fsubst1 nam: %s, len(arglst) = %d, len(formlst) = %d\n" nam (List.length arglst) (List.length formlst);
  dbgact := arglst;
  dbgform := formlst;
  List.iter2 (fun actual -> function
    | Id _ as formal -> Hashtbl.add attr.Source_text_rewrite.subst formal actual
    | oth -> unhand_rtl := Some oth; failwith "fsubst''") arglst formlst;
    (match body with
	   | Blocking (FopAsgn (nam', funexpr)) :: [] -> funexpr
           | Seq(lbl, lst) :: [] -> Seq(lbl, DeclReg([Id nam], [AnyRange (hi', lo')], Deflt) :: lst)
	   | oth -> unhand_rtl := Some (Itmlst oth); failwith "fsubst''")
| FunDecl (nam,
     Itmlst [AnyRange (hi, lo)],
     FunGuts (formlst',
      [Blocking
        (FopAsgn (nam', funexpr))])) ->
  let formlst = List.map (function
	   | PortFront (PortItemFront2 (dir, Deflt, rng), ItemAsgn(Id _ as formal)) -> formal
           | ItemAsgn (Id _ as formal) -> formal
	   | oth -> unhand_rtl := Some oth; failwith "formal") formlst' in
  printf "fsubst2 nam: %s, len(arglst) = %d, len(formlst) = %d\n" nam (List.length arglst) (List.length formlst);
  List.iter2 (fun actual -> function
    | Id _ as formal -> Hashtbl.add attr.subst formal actual
    | oth -> unhand_rtl := Some oth; failwith "fsubst'''") arglst formlst;
  funexpr
  | oth -> unhand_rtl := Some oth; failwith "fsubst"

and fsubst attr arglst id = match Hashtbl.find_opt attr.subst (FunRef (id, [])) with
  | Some (FunDecl _ as f) ->
     recurs2 attr (fsubst' attr arglst f)
  | Some (AutoFunDecl _ as f) ->
     recurs2 attr (fsubst' attr arglst f)
  | Some oth -> unhand_rtl := Some oth; failwith "FunRef"
  | None -> failwith ("No definition for function: "^id)

and tsubst' attr arglst = function
| TaskDecl (nam, Deflt, TFBody (formlst, body), Deflt) ->
  printf "nam: %s, len(arglst) = %d, len(formlst) = %d\n" nam (List.length arglst) (List.length formlst);
  dbgact := arglst;
  dbgform := formlst;
  List.iter2 (fun actual -> function
		  | TF_port_decl ((In|Out), [AnyRange (hi, lo)], [TF_variable (Id _ as formal, Deflt, Deflt, Deflt)]) -> Hashtbl.add attr.Source_text_rewrite.subst formal actual
		  | TF_port_decl ((In|Out), [], [TF_variable (Id _ as formal, Deflt, Deflt, Deflt)]) -> Hashtbl.add attr.Source_text_rewrite.subst formal actual
    | oth -> unhand_rtl := Some oth; failwith "tsubst'") arglst formlst;
    Seq(nam, body)
| TaskDecl (nam, Deflt, FunGuts (formlst', body), Deflt) ->
  let formlst = List.map (function
           | PortFront (PortItemFront2 ((In|Out), Deflt, rng), ItemAsgn (Id _ as formal)) -> formal
           | ItemAsgn (Id _ as formal) -> formal
	   | oth -> unhand_rtl := Some oth; failwith "formal") formlst' in
  printf "nam: %s, len(arglst) = %d, len(formlst) = %d\n" nam (List.length arglst) (List.length formlst);
  dbgact := arglst;
  dbgform := formlst;
  Seq(nam, body)
| oth -> unhand_rtl := Some oth; failwith "tsubst"

and tsubst attr arglst id = recurs2 attr (match Hashtbl.find_opt attr.subst (TaskRef (id,[])) with
  | Some (TaskDecl _ as f) -> tsubst' attr arglst f
  | Some oth -> unhand_rtl := Some oth; failwith "TaskRef"
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
| oth -> unhand_rtl := Some oth; failwith "elabeval"

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

let exists_wire bufh typhash options nam signage = update typhash (Id nam) signage

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
  | oth -> oth

let portpos typhash nam =
  match Hashtbl.find_opt typhash nam with Some (MaybePort (ix,_,_)) -> ix | _ -> 0

let range typhash = function
    | Id nam, AnyRange(hi,lo) ->
        Intgr (ceval typhash hi - ceval typhash lo + 1)
    | _, oth -> unhand_rtl := Some oth; failwith "range"

let addmem bufh typhash first_last_lst wid' = function
| oth -> failwith ("Argument "^(Source_text_rewrite.getstr oth)^" of addmem must be memory Id")

let elabenum typhash nam id_lst typ' =
  print_endline ("elabenum: "^nam);
  update typhash (Id nam) (Venum nam);
    let strt = ref 0 in
    List.iter (function
        | Id e -> update typhash (Id e) (Vemember(nam, e, typ', !strt));
	    incr strt
        | EnumInit (e, expr) ->
	    strt := ceval typhash expr;
	    update typhash (Id e) (Vemember(nam, e, typ', !strt));
            incr strt
	| oth -> unhand_lst := id_lst; failwith "TypEnum6") id_lst

let addwire' bufh typhash nam = function
    | AnyRange(hi,lo) as x ->
      addwire bufh typhash (range typhash (Id nam, x) :: [], Id nam, Unsigned_vector(hi,lo))
    | oth -> unhand_rtl := Some oth; failwith "addwire'"

let rec restrict typhash wid = function
x -> x

let buffer' bufh typhash expr wid = expr

let addconn bufh typhash  (nam, expr) =
    let lhsw = width typhash nam in
    let expr' = buffer' bufh typhash expr 0 in
    let expw = width typhash expr' in
    let rhs = restrict typhash lhsw expr' in
    let rhsw = width typhash rhs in
    ()

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
      | oth -> unhand_rtl := Some oth; failwith "DeclReg550") reg_lst
    | DeclReg (reg_lst, (AnyRange (hi, lo) as x) :: [], signed) -> List.iter (function
      | Id _ as nam -> addwire bufh typhash ([range typhash (nam, x)], nam, signcnv (signed, x));
      | DeclAsgn (mem, AnyRange(first,last) :: []) -> addmem bufh typhash [first, last] (ceval typhash hi - ceval typhash lo + 1) mem
      | DeclAsgn (mem, AnyRange(first,last) :: AnyRange(first',last') :: []) -> addmem bufh typhash [first, last; first', last'] (ceval typhash hi - ceval typhash lo + 1) mem
      | VarDeclAsgn (nam, expr) -> addwire bufh typhash ([], nam, signcnv (signed, Deflt))
      | oth -> unhand_rtl := Some oth; failwith "DeclReg555") reg_lst;
    | TypEnum4 (Deflt, id_lst, [Id nam]) -> elabenum typhash nam id_lst (Unsigned_vector(Intgr 31, Intgr 0))
    | TypEnum4 (TypEnum3 [AnyRange (hi, lo)], id_lst, [Id nam]) -> elabenum typhash nam id_lst (Unsigned_vector(hi,lo))
    | TaskDecl(nam, arg1, arg2, arg3) -> update typhash (Id nam) (Task(arg1,arg2,arg3))
    | ParamDecl (Atom ("Localparam_real"|"Parameter_real"), [ParamAsgn1 (nam, expr)]) -> update typhash (Id nam) (Vreal (match expr with Float f -> f | Number(_,_,n,_) -> float_of_int n | oth -> failwith "realparam"))
    | ParamDecl (Atom ("Parameter"|"localparam"), param_lst) -> List.iter (function
          | ParamAsgn1 (nam, expr) ->
              let wid = width typhash expr in
              dbgpar := (nam,expr,wid) :: !dbgpar;
              addwire bufh typhash ([wid], Id nam, Vlocal(wid, expr)); 
              addconn bufh typhash (Id nam, expr)
	  | oth -> unhand_rtl := Some oth; failwith "localparam") param_lst;
    | ParamDecl (Param ("localparam", sgn, (AnyRange _ as x) :: []), param_lst) -> List.iter (function
          | ParamAsgn1 (nam, expr) ->
              let wid = width typhash expr in
              dbgpar := (nam,expr,wid) :: !dbgpar;
              addwire' bufh typhash nam x;
              addconn bufh typhash (Id nam, expr)
	  | oth -> unhand_rtl := Some oth; failwith "localparam") param_lst;
    | ParamDecl (LocalParamTyp (Typ1 id_t), param_lst) -> List.iter (function
          | ParamAsgn1 (nam, expr) ->
              let wid = width typhash expr in
              dbgpar := (nam,expr,wid) :: !dbgpar;
              let wid = 32 in (* place holder *)
              addwire bufh typhash ([wid], Id nam, Vlocal (wid, expr));
              addconn bufh typhash (Id nam, expr)
	  | oth -> unhand_rtl := Some oth; failwith "localparam_int") param_lst;
    | ParamDecl (LocalParamTyp (Typ5 (Atom "logic", AnyRange (lft, rght) :: [])), param_lst) -> List.iter (function
          | ParamAsgn1 (nam, expr) ->
              let wid = width typhash expr in
              dbgpar := (nam,expr,wid) :: !dbgpar;
              let wid = 32 in (* place holder *)
              addwire bufh typhash ([wid], Id nam, Vlocal (wid, expr));
              addconn bufh typhash (Id nam, expr)
	  | oth -> unhand_rtl := Some oth; failwith "localparam_int") param_lst;
    | ParamDecl (LocalParamTyp (Typ8 (Atom kind, signing)), param_lst) -> List.iter (function
          | ParamAsgn1 (nam, expr) ->
              let wid = width typhash expr in
              dbgpar := (nam,expr,wid) :: !dbgpar;
              let wid = atom_width kind in
              addwire bufh typhash ([wid], Id nam, Vlocal (wid, expr));
              addconn bufh typhash (Id nam, expr)
	  | oth -> unhand_rtl := Some oth; failwith "localparam_int") param_lst;
    | NetDecl (Atom "wire" :: [], wire_lst) -> List.iter (function
          | Id nam -> addwire bufh typhash ([], Id nam, Unsigned)
	  | DeclAsgn (Id nam, (AnyRange _ as x) :: []) ->
              addwire' bufh typhash nam x
          | InitSig (nam, expr) -> addwire bufh typhash ([], nam, Unsigned); addconn bufh typhash (nam, expr)
	  | oth -> unhand_rtl := Some oth; failwith "NetDecl'") wire_lst;
    | NetDecl (Atom "wire" :: (AnyRange _ as x) :: [], wire_lst) -> List.iter (function
          | Id nam -> addwire' bufh typhash nam x
          | InitSig (Id nam, expr) -> addwire' bufh typhash nam x; addconn bufh typhash (Id nam, expr)
	  | oth -> unhand_rtl := Some oth; failwith "NetDecl''") wire_lst;
    | DeclInt2 id_lst -> List.iter (function
	| Id _ as nam -> addwire bufh typhash ([32], nam, Unsigned_vector(Intgr 31, Intgr 0))
        | VarDeclAsgn (nam, expr) -> update typhash (nam) (Vint (ceval typhash expr))
        | Intgr _ -> () (* residue from loop unrolling can be ignored *)
        | oth -> unhand_rtl := Some oth; failwith "DeclInt2") id_lst
    | TypEnum6 (nam, Deflt, id_lst) ->
      print_endline nam;
      elabenum typhash nam id_lst (Unsigned_vector (Intgr 31, Intgr 0))
    | TypEnum6 (nam, TypEnum3 (AnyRange(lft,rght) :: []), id_lst) ->
      print_endline nam;
      elabenum typhash nam id_lst (Unsigned_vector (lft, rght))
    | Typ5 (TypEnum6 (nam, TypEnum3 (AnyRange(lft,rght) :: []), id_lst), typid_lst) ->
      elabenum typhash nam id_lst (Unsigned_vector (lft, rght))
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
	  | oth -> unhand_rtl := Some oth; failwith "DeclLogic1760") id_lst
    | Typ9 (old_id, id_lst, Typ5 (Deflt, elst)) -> List.iter (function
          | Id nam ->
            print_endline old_id;
            addwire bufh typhash ([], Id nam, Unsigned_vector(Intgr (clog2 (List.length elst) - 1), Intgr 0));
	  | oth -> unhand_rtl := Some oth; failwith "DeclLogic1760") id_lst
    | DeclLogic2 (wire_lst, AnyRange (hi, lo) :: [])  as x -> List.iter (function
	  | Id nam -> update typhash (Id nam) (Unsigned_vector(hi,lo));
          | DeclAsgn (nam, AnyRange(first,last) :: []) -> addwire bufh typhash ([range typhash (nam, x)], nam, Unsigned)
	  | oth -> unhand_rtl := Some oth; failwith "DeclLogic2") wire_lst
    | DeclLogic2 (wire_lst, AnyRange (hi, lo) :: AnyRange (hi', lo') :: []) -> List.iter (function
	  | Id nam -> update typhash (Id nam) (Unsigned_vector(hi,lo));
	  | oth -> unhand_rtl := Some oth; failwith "DeclWire") wire_lst
    | DeclData (Typ5 (Atom "logic", AnyRange (lft, rght) :: AnyRange (lft', rght') :: []), Deflt, VarDeclAsgn (mem, ExprOKL lst) :: []) -> () (* placeholder *)
    | DeclData (Atom "static", Typ8 (Atom "byte", Deflt), VarDeclAsgn (Id id, String s) :: []) -> () (* placeholder *)
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
              | oth -> unhand_rtl := Some oth; failwith "struct/union array") id_lst;
    | Typ11 (Typ8 (SUDecl (Atom "packed", lst), Deflt), [AnyRange (lft, rght)], id_lst) ->
        List.iter (fun nam -> update typhash (nam) (Vsu (nam, List.flatten (List.map (struct_union typhash) lst)))) id_lst
    | Typ12 ([AnyRange (lft, rght)], Typ5 (TypEnum3 [AnyRange (hi, lo)], elst), id_lst) ->
        List.iter (function
            | (Id nam) -> elabenum typhash nam elst (Unsigned_vector (lft, rght))
            | oth -> unhand_rtl := Some oth; failwith "enum array") id_lst
    | InstDecl (typ, params, lst) -> List.iter (function
        | (InstNameParen1 _ | InstNameParen2 _) -> ()
        | Id id ->
          print_endline ("InstDecl: "^id);
          addwire bufh typhash ([], Id id, Unsigned_vector(Intgr 31, Intgr 0));
        | oth -> unhand_rtl := Some oth; failwith "InstDecl1634") lst;
    | Typ9 (_, id_lst, Typ5 (TypEnum3 (AnyRange(lft,rght) :: []), elst)) ->
      List.iter (function
            | Id id ->
              print_endline ("Typ9(Enum3): "^id);
              addwire bufh typhash ([], Id id, Unsigned_vector(lft, rght));
            | oth -> unhand_rtl := Some oth; failwith "enum range") id_lst
    | DeclLogic (reg_lst) -> List.iter (function
	  | Id nam -> addwire bufh typhash ([], Id nam, Unsigned)
          | DeclAsgn (nam, AnyRange(first,last) :: []) -> addwire bufh typhash ([], nam, Unsigned_vector(first,last))
          | VarDeclAsgn (nam, expr) -> addwire bufh typhash ([], nam, Unsigned)
	  | oth -> unhand_rtl := Some oth; failwith "DeclLogic651"
        ) reg_lst;
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
    | oth -> unhand_rtl := Some oth; failwith "decl_template"

and decl_template bufh typhash modules pth itm =
    dbgdecl := itm :: !dbgdecl;
    decl_template' bufh typhash modules pth itm

let dbgnew = ref []

let split_always typhash' act_seq rst_seq evlst rst =
    let init' = Hashtbl.create 255 in
    let pref = {nonblk="nonblk1$";block="blk1$";typh=typhash';init=init'} in
    let pref' = {nonblk="nonblk2$";block="blk2$";typh=typhash';init=init'} in
    let rst' = {nonblk="$";block="$";typh=typhash';init=init'} in
    let subh = Hashtbl.create 255 in
    let attr = {Source_text_rewrite.fn=map_active_combined pref; subst=subh; pth=""} in
    let attr'' = {Source_text_rewrite.fn=map_reset pref'; subst=subh; pth=""} in
    let act_seq', act_seq'' = map_active_combined' pref attr act_seq in
    let rst_seq'' = map_reset rst' attr'' rst_seq in
    let ilst = ref [] in
    Hashtbl.iter (fun k () -> match k with
        | Blocking (FopAsgn (Id _, Id _)) -> ilst := k :: !ilst
        | oth -> unhand_rtl := Some oth; failwith "init'") init';
    let ilst = List.sort_uniq compare !ilst in
    let newseq = AlwaysLegacy (AtStar, match act_seq' with
      | Seq(lbl, lst) -> Seq(lbl, ilst @ lst)
      | oth -> Seq("", ilst @ oth :: []) ) in
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
        | oth -> unhand_rtl := Some oth; failwith "split'") subh;
    print_endline (string_of_int (Hashtbl.length init'));
    List.sort_uniq compare !nlst @ newseq :: newseq' :: [] 

let rec split' typhash' = function
  | [] -> []
  | AlwaysLegacy (At (EventOr evlst), If2 (rst, rst_seq, act_seq)) as x :: tl ->
    split_always typhash' act_seq rst_seq evlst rst @ split' typhash' tl
  | AlwaysLegacy (At (EventOr ([Pos (clk); Pos (rst)] as evlst)), If2 (rst', rst_seq, act_seq)) as x :: tl when alweq (rst, rst') ->
    split_always typhash' act_seq rst_seq evlst rst @ split' typhash' tl
| oth :: tl -> oth :: split' typhash' tl

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

  | oth -> unhand_rtl := Some oth; failwith "parm_map"

let module_header modules = function
| Modul (typ, parm_lst, port_lst, body_lst) ->
  let typhash = Hashtbl.create 255 in
  let bufh = () in
  List.iter (fun itm -> let _ = parm_map typhash itm in ()) parm_lst;
  List.iteri (fun ix itm -> ports typhash (ix+1) itm) port_lst;
  List.iter (fun itm -> decl_template bufh typhash modules None itm) body_lst;
  bufh, typhash, port_lst
| oth -> unhand_rtl := Some oth; failwith "module_header only handles modules"

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
| oth -> unhand_rtl := Some oth; failwith "vexpr"

let funtyp typhash = function
| Atom primtyp -> primtyp
| Typ1 id_t -> id_t
| Typ3 (id_t, [PackageRef (pkg, Atom "::")]) -> pkg^"::"^id_t
| Typ5 (Atom primtyp, AnyRange (lft, rght) :: []) -> sprintf "%s [%d:%d]" primtyp (ceval typhash lft) (ceval typhash rght)
| Typ6 (Atom primtyp) -> primtyp
| Typ8 (Atom kind, Atom kind') -> kind' ^ kind
| Typ8 (Atom kind, Deflt) -> kind
| oth -> unhand_rtl := Some oth; failwith "funtyp"

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
  | oth -> unhand_rtl := Some oth; failwith "parm_generic"

let parm_template buf' typhash parm_lst =
  if parm_lst <> [] then
  bprintf buf' "    generic (\n%s\n    ); // 588	\n" (String.concat ";\n" (List.map (parm_generic typhash) parm_lst))

let decl_mem buf' typhash first last hi lo cnt mem =
    bprintf buf' "    type Mem_Type%d is array [%d : %d] of logic[%d : %d]; // 591	\n" !cnt (ceval typhash last) (ceval typhash first) (ceval typhash hi) (ceval typhash lo);
    bprintf buf' "    signal %s : Mem_Type%d := (others => (others => '0')); // 592	\n" mem !cnt;
incr cnt

let fn_arg typhash = function
		   | Deflt -> ""
                   | oth -> unhand_rtl := Some oth; failwith "fn_arg"


let template modules = function
  | Modul _ as m ->
    sub' m
  | PackageBody (pkg, body_lst) as p ->
    if verbose > 1 then print_endline ("Package "^pkg^" is pending");
    sub' p
  | oth -> unhand_rtl := Some oth; failwith "This template only handles modules/packages"

