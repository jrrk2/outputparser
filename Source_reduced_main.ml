open Printf
open Source_text
open Source_text_lex
open Source_text_rewrite
open Source_text_rewrite_types
open Input_rewrite_types
open Input_types
open Dump_types

let verbose = try int_of_string (Sys.getenv "CNF_VERBOSE") > 0 with err -> false
let sep_rtl = try int_of_string (Sys.getenv "CNF_SEP_RTL") > 0 with err -> false

let dbgx = ref []

let oth' = ref None
let othopt = ref None
let othconn = ref None

let cnv_sig = function
  | E.GND -> TokVal "1'0"
  | PWR -> TokVal "1'1"
  | SCALAR str -> TokID str
  | INDEXED (signal, ix) -> Sigspec90 (signal, ix)

let getw ind = function
  | E.GND -> Some F.f_false
  | PWR -> Some F.f_true
  | signal -> match Hashtbl.find_opt ind.wires signal with
    | Some x -> x
    | None -> failwith (E.string_of_signal signal^" not declared")

let addinp idx idx' ind signal =
  Hashtbl.add ind.inffop signal ();
  match Hashtbl.find_opt ind.wires signal with
    | Some x -> print_endline (E.string_of_signal signal^" redeclared")
    | None -> Hashtbl.add ind.wires signal ( Some ( atom signal ) )

let addoutp idx idx' ind signal =
  Hashtbl.add ind.inffop signal ();
  match Hashtbl.find_opt ind.wires signal with
    | Some x -> print_endline (E.string_of_signal signal^" redeclared")
    | None -> Hashtbl.add ind.wires signal None

let addff ind signal = let op = atom signal in Hashtbl.replace ind.wires signal ( Some op )

(* convert and print a cnf *)

let cnfpp q =
  let buf' = Buffer.create 1000 in
  let buf = Format.formatter_of_buffer buf' in
  List.iter (fun itm ->
    List.iter (fun itm -> E.pp buf itm; Format.pp_print_space buf ()) itm;
    Format.pp_print_flush buf ();
    ) (F.make_cnf q);
  print_endline (Buffer.contents buf')

let epp itm =
  let buf' = Buffer.create 1000 in
  let buf = Format.formatter_of_buffer buf' in
  E.pp buf itm;
  Format.pp_print_flush buf ();
  print_endline (Buffer.contents buf')

let stash' ind = function
  | TokConn ([TokID pin], [Sigspec90 (signal, ix)]) -> pin, idx signal ix
  | TokConn ([TokID pin], [TokID signal]) -> pin, scalar signal
  | TokConn ([TokID pin], [TokVal lev]) -> pin, cnv_pwr lev
  | oth -> othconn := Some oth; failwith "conn'"

let dbgstash = ref None

let stash_ops = function TokConn ([TokID pin], _) -> (match trim pin with "Y" -> true | "Q" -> true | _ -> false) | _ -> false

let stash ind kind inst conns =
  dbgstash := Some (kind, inst, conns);
  let pin, net = stash' ind (List.hd (List.filter stash_ops conns)) in
  if trim pin = "Q" then addff ind net; (* prevent infinite recursion *)
  if Hashtbl.mem ind.stash net then failwith ("Multiple gates driving: "^E.string_of_signal net);
  Hashtbl.replace ind.stash net (kind,inst,conns)

let othx = ref []
let othxlst = ref []
let othexp = ref None

let explode_const = function
  | n::str::[] -> List.init (int_of_string n) (fun ix -> cnv_pwr ("1'"^String.make 1 str.[ix]))
  | oth -> othxlst := oth; failwith "explode_const"

let rec explode_lst = function
  | [] -> []
  | Sigspecrange (lhs, hi, lo) :: tl -> List.init (hi-lo+1) (fun ix -> idx lhs (hi-ix)) @ explode_lst tl
  | Sigspec90 (lhs, ix) :: tl -> idx lhs ix :: explode_lst tl
  | TokVal tok :: tl -> explode_const (String.split_on_char '\'' tok) @ explode_lst tl
  | TokID id :: tl -> scalar id :: explode_lst tl
  | TokInt 1 :: tl -> E.PWR :: explode_lst tl
  | oth -> othx := oth; failwith "explode_lst"

let explode_signal ind = function
  | TokID id -> (match Hashtbl.find_opt ind.wid (trim id) with None -> scalar id :: [] | Some n -> List.init n (fun i -> idx id i))
  | (Sigspec90 _ | Sigspecrange _ | TokVal _ | TokInt _) as x -> explode_lst (x :: [])
  | Sigspec92 conc -> explode_lst conc
  | oth -> oth' := Some oth; failwith "explode_signal"

let rec cnv_ilang ind = function
| Autoidx_stmt26(int') -> ()
| Attr_stmt(string,ilang_lst') -> ()
| Module12(string,ilang_lst') -> print_endline string; List.iter (cnv_ilang ind) ilang_lst'
| Wire_stmt(options,string') -> let wid = ref None and fn = ref addwire in List.iter (function
    | Wire_optionswidth n -> wid := Some n; Hashtbl.replace ind.wid (trim string') n
    | Wire_optionsinput n -> fn := addinp n;
    | Wire_optionsoutput n -> fn := addoutp n;
    | Signed -> ()
    | oth -> othopt := Some oth; failwith "options") options;
    (match !wid with
      | None -> !fn 0 ind (scalar string')
      | Some n -> for i = 0 to n-1 do !fn (i+1) ind (idx string' i) done); ()
| Cell_stmt(kind,inst,params,conns) -> stash ind kind inst conns
| Conn_stmt96 ([lhs], [TokInt n]) -> () (* placeholder *)
| Conn_stmt96 ([(Sigspec92 _ | TokID _ | Sigspec90 _ | Sigspecrange _) as conc1], [(Sigspec92 _ | TokID _ | Sigspec90 _ | TokVal _ | Sigspecrange _) as conc2]) ->
  let lhs = explode_signal ind conc1 in
  let rhs = explode_signal ind conc2 in
  if List.length lhs <> List.length rhs then
    begin
      let othlst = ref [] in
      Hashtbl.iter (fun k x -> othlst := (k,x) :: !othlst) ind.wires;
      let signal' = match conc1 with TokID id -> Hashtbl.find_opt ind.wid id | _ -> None in
      othexp := Some (conc1,conc2,lhs,rhs,ind,signal');
      failwith "conn_stmt_fail"
    end;
  List.iter2 (fun lhs' rhs' -> stash ind "$_BUF_" ("$B"^string_of_int (Hashtbl.length ind.stash)) [ TokConn ([TokID "\\A"], [cnv_sig rhs']) ; TokConn ([TokID "\\Y"], [cnv_sig lhs']) ]) lhs rhs
| Param_defval_stmt24(string,ilang_lst') -> () (* does not seem to be used yet *)
(*
| Assign_stmt67(ilang_lst,ilang_lst') -> ()
| Attr_list56(ilang_lst,ilang_lst') -> ()
| Case_body63(ilang_lst,ilang_lst') -> ()
| Case_body64(ilang_lst,ilang_lst') -> ()
| Case_body65(ilang_lst,ilang_lst') -> ()
| Cell_bodyconnect(ilang_lst,string,ilang_lst',ilang_lst2) -> ()
| Cell_bodyparam(ilang_lst,string,ilang_lst',ilang_lst2) -> ()
| Cell_bodypreal(ilang_lst,string,ilang_lst',ilang_lst2) -> ()
| Cell_bodypsigned(ilang_lst,string,ilang_lst',ilang_lst2) -> ()
| Compare_list61(ilang_lst,ilang_lst') -> ()
| Design6(ilang_lst,ilang_lst') -> ()
| Design7(ilang_lst,ilang_lst') -> ()
| Design8(ilang_lst,ilang_lst') -> ()
| Input2(ilang_lst,ilang_lst') -> ()
| Memory_optionsoffset(int') -> ()
| Memory_optionssize(int') -> ()
| Memory_optionswidth(int') -> ()
| Memory_stmt39(ilang_lst,string') -> ()
| Module_body13(ilang_lst,ilang_lst') -> ()
| Param_stmt23(string,ilang_lst') -> ()
| Signed -> ()
| Sigspec90(string, int') -> ()
| Sigspec92(ilang_lst') -> ()
| Sigspec_list_reversed93(ilang_lst,ilang_lst') -> ()
| Sigspecrange(string,int,int') -> ()
| Switch_bodycase(ilang_lst,ilang_lst',ilang_lst2) -> ()
| Switch_stmt(ilang_lst,ilang_lst',ilang_lst2,ilang_lst3) -> ()
| Sync_list69(ilang_lst',ilang_lst2,ilang_lst3,ilang_lst4) -> ()
| Sync_listalways(ilang_lst',ilang_lst2) -> ()
| Sync_listglobal(ilang_lst',ilang_lst2) -> ()
| Sync_listinit(ilang_lst',ilang_lst2) -> ()
| Update_list82(ilang_lst',ilang_lst2) -> ()
| Update_listmemwr(string,ilang_lst2,ilang_lst3,ilang_lst4,ilang_lst5) -> ()
| Upto -> ()
| Wire_optionsinout(int) -> ()
| Wire_optionsinput(int) -> ()
| Wire_optionsinvalid -> ()
| Wire_optionsoffset(int') -> ()
| Wire_optionsoutput(int') -> ()
| Wire_optionswidth(int') -> ()
| TokCase(ilang_lst,ilang_lst') -> ()
| TokConn(ilang_lst,ilang_lst') -> ()
| TokParam(ilang_lst,ilang_lst') -> ()
| TokUpdate(ilang_lst,ilang_lst') -> ()
| TokInt(int) -> ()
| TokID(string) -> ()
| TokVal(string) -> ()
| TokStr(string) -> ()
| TokPos -> ()
| TokNeg -> ()
| TokEdge -> ()
*)
| oth -> oth' := Some oth; failwith "cnv_ilang"

let mycnf' = ref F.f_false
let mycnf = ref [[E.transparent (E.fresh ())]]
let othh = ref F.f_false

let ep form =
    if verbose then print_endline "Dumping cnf";
    mycnf' := form;
    if verbose then print_endline "Building cnf";
    let m = F.make_cnf form in
    mycnf := List.map (List.map E.transparent) m;
    let solver = Msat_sat_slit.create () in
    Msat_sat_slit.assume solver m ();
    match Msat_sat_slit.solve solver with
      | Msat_sat_slit.Sat _ -> if verbose then print_endline "SATISFIABLE (netlists mismatched)"; false
      | Msat_sat_slit.Unsat _ -> if verbose then print_endline "UNSATISFIABLE (netlists match)"; true

let cnv_sat_tree = List.map (fun (nam,itm) ->
      let wh = Hashtbl.create 255 in
      let wid = Hashtbl.create 255 in
      let ffh = Hashtbl.create 255 in
      let sh = Hashtbl.create 255 in
      let ind = {wires=wh;inffop=ffh;stash=sh;wid=wid} in
      print_endline ("Converting: "^nam);
      List.iter (cnv_ilang ind) itm;
      Hashtbl.iter (fun _ (kind,inst,conns) -> Convert.func ind [inst] kind conns) sh;
      let hlst=ref [] in
      Hashtbl.iter (fun k -> function
          | Some x -> othh := x; hlst := (k, fpp x) :: !hlst
          | None -> if verbose then print_endline (E.string_of_signal k^" is not used")) wh;
      let inffoplst=ref [] in
      Hashtbl.iter (fun k () -> inffoplst := (k, match Hashtbl.find wh k with
        | Some x -> x
        | None -> print_endline ("ffh: " ^ E.string_of_signal k^" is undefined"); atom (scalar "\\")) :: !inffoplst) ffh;
      let widlst=ref [] in
      Hashtbl.iter (fun k n -> widlst := (k, n) :: !widlst) wid;
      print_endline ("inffopslt length: "^string_of_int (List.length !inffoplst));
      !hlst, List.sort compare !inffoplst, !widlst
  )

let fail' = function
| ALWYS (string, rw_lst) -> failwith "ALWYS"
| ARG (rw_lst) -> failwith "ARG"
| ARITH (arithop, rw_lst) -> failwith "ARITH"
| ASEL (rw_lst) -> failwith "ASEL"
| ASGN (bool, string2, rw_lst) -> failwith "ASGN"
| BGN (string_opt, rw_lst) -> failwith "BGN"
| CA (string, rw_lst) -> failwith "CA"
| CAT (string, rw_lst) -> failwith "CAT"
| CELL (string, string2, string3, string4, rw_lst) -> failwith "CELL"
| CELLS (rw_lst, xmlattr) -> failwith "CELLS"
| CMP (cmpop, rw_lst) -> failwith "CMP"
| CND (string, rw_lst) -> failwith "CND"
| CNST ((int, cexp)) -> failwith "CNST"
| COMB -> failwith "COMB"
| CONSPACK (string, rw_lst) -> failwith "CONSPACK"
| CONSPACKMEMB (string, rw_lst) -> failwith "CONSPACKMEMB"
| CONTAINER (itms, rw) -> failwith "CONTAINER"
| CPS (string, rw_lst) -> failwith "CPS"
| CS (string, rw_lst) -> failwith "CS"
| CSITM (string, rw_lst) -> failwith "CSITM"
| DSPLY (string, string2, rw_lst) -> failwith "DSPLY"
| EITM (string, string2, string3, int, rw_lst) -> failwith "EITM"
| FIL (string, string2) -> failwith "FIL"
| FILS (string, rw_lst) -> failwith "FILS"
| FNC (string, string2, typetable_t, rw_lst) -> failwith "FNC"
| FORSTMT ((string, string2, cmpop, rw, (int, cexp), (int2, cexp2), (int3, cexp3), rw_lst)) -> failwith "FORSTMT"
| FRF (string, string2, rw_lst) -> failwith "FRF"
| IF (string, rw_lst) -> failwith "IF"
| IFC (string, string2, rw_lst) -> failwith "IFC"
| IMP (string, string2, rw_lst) -> failwith "IMP"
| IMRF (string, string2, dirop, rw_lst) -> failwith "IMRF"
| INIT (string, string2, rw_lst) -> failwith "INIT"
| INST (string, token, string2_lst, (string3, rw_lst)) -> failwith "INST"
| IO ("", nam::[], (_, _, _, _), dir, kind, _::_) -> failwith "IO"
| IO (string, string2_lst, typetable_t, dirop, string3, rw_lst) -> failwith "IO"
| IRNG (string, rw_lst) -> failwith "IRNG"
| ITM (string, string2, rw_lst) -> failwith "ITM"
| IVAR (string, string2, typetable_t, rw_lst, int) -> failwith "IVAR"
| JMPBLK (string, rw_lst) -> failwith "JMPBLK"
| JMPG (string, rw_lst) -> failwith "JMPG"
| JMPL (string, rw_lst) -> failwith "JMPL"
| LOGIC (logop, rw_lst) -> failwith "LOGIC"
| MODPORTFTR (string, string2) -> failwith "MODPORTFTR"
| MODUL (string, string2, rw_lst, lst) -> failwith "MODUL"
| NEGEDGE (string) -> failwith "NEGEDGE"
| NEGNEG (string, string2) -> failwith "NEGNEG"
| NTL (rw_lst) -> failwith "NTL"
| PKG (string, string2, rw_lst) -> failwith "PKG"
| PORT (string, string2, dirop, rw_lst) -> failwith "PORT"
| POSEDGE (string) -> failwith "POSEDGE"
| POSNEG (string, string2) -> failwith "POSNEG"
| POSPOS (string, string2) -> failwith "POSPOS"
| REPL (string, int, rw_lst) -> failwith "REPL"
| RNG (rw_lst) -> failwith "RNG"
| SCOPE (string) -> failwith "SCOPE"
| SEL (string, rw_lst) -> failwith "SEL"
| SFMT (string, rw_lst) -> failwith "SFMT"
| SNITM (string, rw_lst) -> failwith "SNITM"
| SNTRE (rw_lst) -> failwith "SNTRE"
| SYS (string, string2, rw_lst) -> failwith "SYS"
| TASKDEF (string, string2, rw_lst) -> failwith "TASKDEF"
| TASKRF (string, string2, rw_lst) -> failwith "TASKRF"
| TIM (string) -> failwith "TIM"
| TPLSRGS (string, string2, int, rw_lst) -> failwith "TPLSRGS"
| TYP (int, typ_t) -> failwith "TYP"
| TYPETABLE (typetable_t_arr) -> failwith "TYPETABLE"
| UNKNOWN -> failwith "UNKNOWN"
| UNRY (unaryop, rw_lst) -> failwith "UNRY"
| VAR (string, string2_lst, typetable_t, string2) -> failwith "VAR"
| VPLSRGS (string, int, rw_lst) -> failwith "VPLSRGS"
| VRF (string, typetable_t, rw_lst) -> failwith "VRF"
| WHL (rw_lst) -> failwith "WHL"
| XML (rw_lst) -> failwith "XML"
| XRF (string, string2, string3, string4, dirop) -> failwith "XRF"

let othx = ref UNKNOWN
let fail' x = othx := x; fail' x

let rec expr' itms = function
| UNKNOWN -> []
| VRF (nam, rng, []) -> VRF(nam, rng, []) :: []
| IF ("", rw_lst) -> IF ("", List.flatten (List.map (expr' itms) rw_lst)) :: []
| CNST ((wid, HEX n)) as c -> c :: []
| SNTRE (rw_lst) -> List.flatten (List.map (expr' itms) rw_lst)
| SNITM (edg, rw_lst) -> SNITM (edg, List.flatten (List.map (expr' itms) rw_lst)) :: []
| ASGN (nonblock, "", rw_lst) -> ASGN(nonblock, "", List.flatten (List.map (expr' itms) rw_lst)) :: []
| ARITH (arithop, rw_lst) -> ARITH(arithop, List.flatten (List.map (expr' itms) rw_lst)) :: []
| LOGIC (logicop, rw_lst) -> LOGIC(logicop, List.flatten (List.map (expr' itms) rw_lst)) :: []
| oth -> fail' oth

let cnv' itms = function
| IO ("", [nam], rng, dir, kind, []) -> itms.io := (nam, ("", rng, dir, kind, [])) :: !(itms.io)
| IO ("", nam::[], (_, _, _, _), dir, kind, _::_) -> failwith "IO"
| IO (string, string2_lst, typetable_t, dirop, string3, rw_lst) -> failwith "IO"
| ALWYS ("", SNTRE [SNITM ("POS", [VRF (clk, _, [])])] :: rw_lst) -> itms.alwys := ("", POSEDGE clk, List.flatten (List.map (expr' itms) rw_lst)) :: !(itms.alwys)
| ASGN (nonblock, "", rhs :: lhs :: []) ->
( match expr' itms lhs, expr' itms rhs with
  | lft::[], rght::[] -> itms.ca := ("", lft, rght) :: !(itms.ca)
  | oth -> failwith "match ASGN")
| oth -> fail' oth

let cnv = function
| MODUL (string, nam, io_lst, _) ->
    let itms = Input_dump.empty_itms [] in
    List.iter (cnv' itms) io_lst;
    itms.io := List.rev !(itms.io);
    (nam, ((), itms))
| oth -> fail' oth

let othrtl = ref None
let othcnv = ref None

let rewrite_rtlil v =
  let status = ref true in
  print_endline ("Parsing: "^v);
  Matchmly.modules := [];
  let p = Source_text_rewrite.parse_output_ast_from_pipe v in
  let p' = rw p in
  let x = Matchmly.mly p' in
  let modlst = ref [] in
  List.iter (fun (k, x) ->
                dbgx := x :: !dbgx;
		) !(Matchmly.modules);
  let goldlst = cnv_sat_tree Examples_count0_gold.arg in
  let revlst = cnv_sat_tree Examples_count0_dump.arg in
  List.iter2 (fun (hlst, inffoplst, wlst) (hlst', inffoplst', wlst') ->
  let inffoplst1,inffoplst2 = List.split inffoplst in
  let inffoplst1',inffoplst2' = List.split inffoplst' in
  print_endline ("Golden (yosys) primary inputs/flipflop inputs/final outputs: "^String.concat "; " (List.map E.string_of_signal inffoplst1));
  print_endline ("Revised (our) primary inputs/flipflop inputs/final outputs: "^String.concat "; " (List.map E.string_of_signal inffoplst1'));
  print_endline ("Endpoint comparison: "^String.concat "; " (List.map (fun (k, itm) ->
      let k' = E.string_of_signal k in
      match List.assoc_opt k inffoplst with
	| Some itm' -> 
	  let stat = ep (xor2 itm itm') in
	  if not stat then status := false;
	  k' ^ ": " ^ string_of_bool stat
	| None ->
	  status := false;
	  k' ^ ": not compared"
      ) inffoplst'))
    ) goldlst revlst;
  let lst = match x with Itmlst lst -> List.map (Dump_xml.template (ref [])) lst | _ -> [] in
  List.iter (fun itm -> print_endline (Input_dump.dumpitm itm)) lst;
  List.iter (fun itm ->
    let (nam, (_, itms) as cnvrted) = cnv itm in
    othcnv := Some cnvrted;
    let rtl = Input_hardcaml.cnv (nam, itms) in
    let fd = open_out "rtl.v" in output_string fd rtl; close_out fd;
    let tree = Source_text_rewrite.parse_output_ast_from_string rtl in
    othrtl := Some tree;
    Input_dump.dump' "_tmp" cnvrted;
    (* Rtl_map.map modnam rtl *) ) lst;
  !modlst, x, p, p', !status

let _ = if Array.length Sys.argv > 1 then Array.iteri (fun ix itm -> try
    if ix > 0 then let modlst,x,p,p',status = rewrite_rtlil itm in
    List.iter (fun (k,_) -> print_endline ((if status then "PASSED: " else "FAILED: ")^itm^"("^k^")")) modlst
    with err -> print_endline ("FAILED: "^itm)) Sys.argv
