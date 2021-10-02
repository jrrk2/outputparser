open Printf
open Source_text
open Source_text_lex
open Source_text_rewrite
open Input
open Input_rewrite_types

let verbose = try int_of_string (Sys.getenv "CNF_VERBOSE") > 0 with err -> false

let dbgx = ref None
let dbgfunc = ref []

module E = Msat_sat_slit.String_lit (* expressions *)
module F = Msat_tseitin.MakeCNF

type ind = {
  wires:(E.signal, F.t option) Hashtbl.t;
  inffop:(E.signal, unit) Hashtbl.t;
  stash:(E.signal, string * string * ilang list) Hashtbl.t;
  wid:(string, int) Hashtbl.t;
}

let oth' = ref None
let othst = ref []
let othopt = ref None
let othconn = ref None
let othstr = ref None

let trim s = if s.[0] = '\\' then String.sub s 1 (String.length s - 1) else s
let idx s i = E.INDEXED (trim s,i)
let scalar s = E.SCALAR (trim s)
let and2 a b = F.make_and [a;b]
let or2 a b = F.make_or [a;b]
let xor2 a b = F.make_xor a b
let knot a = F.make_not a
let mux2 a b s = or2 (and2 a (knot s)) (and2 b s)
let atom signal = F.make_atom ( E.make signal )

let cnv_pwr = function
  | "1'0" -> E.GND
  | "1'1" -> E.PWR
  | "1'x" -> E.GND
  | oth -> othstr := Some oth; failwith "cnv_pwr"

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

let addwire idx' ind signal =
  match Hashtbl.find_opt ind.wires signal with
    | Some x -> print_endline (E.string_of_signal signal^" redeclared")
    | None -> Hashtbl.add ind.wires signal None

let addfunc ind signal func =
  match Hashtbl.find_opt ind.wires signal with
    | Some None ->
        if verbose then print_endline ("Installed function for wire: "^E.string_of_signal signal);
        Hashtbl.replace ind.wires signal (Some func)
    | Some _ -> print_endline (E.string_of_signal signal ^" redeclared")
    | None -> print_endline (E.string_of_signal signal ^" undefined")

(*
let addconn ind lhs' rhs' =
  match getw ind rhs' with
   | Some func' -> addfunc ind lhs' func'
   | None -> Hashtbl.replace ind.conn (lhs',rhs') ()
*)

let addnxt' pat = function
| E.PWR -> E.SCALAR (pat^"$PWR")
| GND -> E.SCALAR (pat^"$GND")
| SCALAR string -> E.SCALAR (pat^"$"^string)
| INDEXED (string, int) -> E.INDEXED (pat^"$"^string, int)

let addnxt pat ind data d q =
   let lhs' = addnxt' pat q in
   addwire 0 ind lhs';
   Hashtbl.add ind.inffop lhs' ();
   addfunc ind lhs' d

let othlst = ref []
let othwlst = ref []

let notsupp kind lst = othlst := lst; failwith ("Not supported: "^kind)

(* dump a cnf in ASCII *)

let fpp q =
  let buf' = Buffer.create 1000 in
  let buf = Format.formatter_of_buffer buf' in
(*
  if verbose then F.mypp buf q;
*)
  Format.pp_print_flush buf ();
  Buffer.contents buf'

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

let rec func ind klst kind conns = match kind, pinmap ind klst conns with
  | "", ["",E.GND,None] -> failwith "GND" (* dummy to force type inference *)
  | "$_AND_",("\\A", _, Some a) :: ("\\B", _, Some b) :: ("\\Y", y, found) :: [] -> if found = None then addfunc ind y (and2 a b)
  | "$_ANDNOT_",("\\A", _, Some a) :: ("\\B", _, Some b) :: ("\\Y", y, found) :: [] -> if found = None then addfunc ind y (and2 a (knot b))
  | "$_BUF_",("\\A", _, Some a) :: ("\\Y", y, found) :: [] -> if found = None then addfunc ind y a
  | "$_MUX_",("\\A", _, Some a) :: ("\\B", _, Some b) :: ("\\S", _, Some s) :: ("\\Y", y, found) :: [] -> if found = None then addfunc ind y (mux2 a b s)
  | "$_NOT_",("\\A", _, Some a) :: ("\\Y", y, found) :: [] -> if found = None then addfunc ind y (knot a)
  | "$_DFF_P_",("\\C", clk, c) :: ("\\D", data, Some d) :: ("\\Q", q, Some _) :: [] ->
     addnxt "nxt" ind data d q;
  | "$_DFF_PP0_",("\\C", clk, c) :: ("\\D", data, Some d) :: ("\\Q", q, Some _) :: ("\\R", rst, Some r) :: [] ->
     addnxt "nxt" ind data (and2 r d) q;
  | "$_SDFF_PP0_",("\\C", clk, c) :: ("\\D", data, Some d) :: ("\\Q", q, Some _) :: ("\\R", rst, Some r) :: [] ->
     addnxt "nxt" ind data (and2 r d) q;
  | "$_SDFF_PN0_",("\\C", clk, c) :: ("\\D", data, Some d) :: ("\\Q", q, Some _) :: ("\\R", rst, Some r) :: [] ->
     addnxt "nxt" ind data (and2 r d) q;
  | "$_SDFF_PN1_",("\\C", clk, c) :: ("\\D", data, Some d) :: ("\\Q", q, Some _) :: ("\\R", rst, Some r) :: [] ->
     addnxt "nxt" ind data (and2 r d) q;
  | "$_SDFFE_PP0P_",("\\C", clk, c) :: ("\\D", data, Some d) :: ("\\E", enable, Some e) :: ("\\Q", q, Some _) :: ("\\R", rst, Some r) :: [] ->
      let mux = mux2 (atom q) d e in (* recirculate existing data when enable = lo *)
      addnxt "nxt" ind data (and2 r mux) q;
  | "$_SDFFCE_PP0P_",("\\C", clk, c) :: ("\\D", data, Some d) :: ("\\E", enable, Some e) :: ("\\Q", q, Some _) :: ("\\R", rst, Some r) :: [] ->
      let mux = mux2 (atom q) d e in (* recirculate existing data when enable = lo *)
      addnxt "nxt" ind data (and2 r mux) q;
  | "$_DFFE_PP_",("\\C", clk, c) :: ("\\D", data, Some d) :: ("\\E", enable, Some e) :: ("\\Q", q, Some _) :: [] ->
      let mux = mux2 (atom q) d e in (* recirculate existing data when enable = lo *)
      addnxt "nxt" ind data mux q;
  | "$_SDFFCE_PP1P_",("\\C", clk, c) :: ("\\D", data, Some d) :: ("\\E", enable, Some e) :: ("\\Q", q, Some _) :: ("\\R", rst, Some r) :: [] ->
      let mux = mux2 (atom q) d e in (* recirculate existing data when enable = lo *)
      addnxt "nxt" ind data (and2 r mux) q;
  | "$_SDFFCE_PN0P_",("\\C", clk, c) :: ("\\D", data, Some d) :: ("\\E", enable, Some e) :: ("\\Q", q, Some _) :: ("\\R", rst, Some r) :: [] ->
      let mux = mux2 (atom q) d e in (* recirculate existing data when enable = lo *)
      addnxt "nxt" ind data (and2 r mux) q;
  | "$_NAND_",("\\A", _, Some a) :: ("\\B", _, Some b) :: ("\\Y", y, found) :: [] -> if found = None then addfunc ind y (knot (and2 a b))
  | "$_NOR_",("\\A", _, Some a) :: ("\\B", _, Some b) :: ("\\Y", y, found) :: [] -> if found = None then addfunc ind y (knot (or2 a b))
  | "$_OR_",("\\A", _, Some a) :: ("\\B", _, Some b) :: ("\\Y", y, found) :: [] -> if found = None then addfunc ind y (or2 a b)
  | "$_ORNOT_",("\\A", _, Some a) :: ("\\B", _, Some b) :: ("\\Y", y, found) :: [] -> if found = None then addfunc ind y (or2 a (knot b))
  | "$_XOR_",("\\A", _, Some a) :: ("\\B", _, Some b) :: ("\\Y", y, found) :: [] -> if found = None then addfunc ind y (xor2 a b)
  | "$_XNOR_",("\\A", _, Some a) :: ("\\B", _, Some b) :: ("\\Y", y, found) :: [] -> if found = None then addfunc ind y (knot (xor2 a b))
  | "$_AOI3_",lst -> notsupp kind lst
  | "$_AOI4_",lst -> notsupp kind lst
  | "$_DFFE_NN0N_",lst -> notsupp kind lst
  | "$_DFFE_NN0P_",lst -> notsupp kind lst
  | "$_DFFE_NN1N_",lst -> notsupp kind lst
  | "$_DFFE_NN1P_",lst -> notsupp kind lst
  | "$_DFFE_NN_",lst -> notsupp kind lst
  | "$_DFFE_NP0N_",lst -> notsupp kind lst
  | "$_DFFE_NP0P_",lst -> notsupp kind lst
  | "$_DFFE_NP1N_",lst -> notsupp kind lst
  | "$_DFFE_NP1P_",lst -> notsupp kind lst
  | "$_DFFE_NP_",lst -> notsupp kind lst
  | "$_DFFE_PN0N_",lst -> notsupp kind lst
  | "$_DFFE_PN0P_",lst -> notsupp kind lst
  | "$_DFFE_PN1N_",lst -> notsupp kind lst
  | "$_DFFE_PN1P_",lst -> notsupp kind lst
  | "$_DFFE_PN_",lst -> notsupp kind lst
  | "$_DFFE_PP0N_",lst -> notsupp kind lst
  | "$_DFFE_PP0P_",lst -> notsupp kind lst
  | "$_DFFE_PP1N_",lst -> notsupp kind lst
  | "$_DFFSRE_NNNN_",lst -> notsupp kind lst
  | "$_DFFSRE_NNNP_",lst -> notsupp kind lst
  | "$_DFFSRE_NNPN_",lst -> notsupp kind lst
  | "$_DFFSRE_NNPP_",lst -> notsupp kind lst
  | "$_DFFSRE_NPNN_",lst -> notsupp kind lst
  | "$_DFFSRE_NPNP_",lst -> notsupp kind lst
  | "$_DFFSRE_NPPN_",lst -> notsupp kind lst
  | "$_DFFSRE_NPPP_",lst -> notsupp kind lst
  | "$_DFFSRE_PNNN_",lst -> notsupp kind lst
  | "$_DFFSRE_PNNP_",lst -> notsupp kind lst
  | "$_DFFSRE_PNPN_",lst -> notsupp kind lst
  | "$_DFFSRE_PNPP_",lst -> notsupp kind lst
  | "$_DFFSRE_PPNN_",lst -> notsupp kind lst
  | "$_DFFSRE_PPNP_",lst -> notsupp kind lst
  | "$_DFFSRE_PPPN_",lst -> notsupp kind lst
  | "$_DFFSRE_PPPP_",lst -> notsupp kind lst
  | "$_DFFSR_NNN_",lst -> notsupp kind lst
  | "$_DFFSR_NNP_",lst -> notsupp kind lst
  | "$_DFFSR_NPN_",lst -> notsupp kind lst
  | "$_DFFSR_NPP_",lst -> notsupp kind lst
  | "$_DFFSR_PNN_",lst -> notsupp kind lst
  | "$_DFFSR_PNP_",lst -> notsupp kind lst
  | "$_DFFSR_PPN_",lst -> notsupp kind lst
  | "$_DFFSR_PPP_",lst -> notsupp kind lst
  | "$_DFF_NN0_",lst -> notsupp kind lst
  | "$_DFF_NN1_",lst -> notsupp kind lst
  | "$_DFF_NP0_",lst -> notsupp kind lst
  | "$_DFF_NP1_",lst -> notsupp kind lst
  | "$_DFF_N_",lst -> notsupp kind lst
  | "$_DFF_PN0_",lst -> notsupp kind lst
  | "$_DFF_PN1_",lst -> notsupp kind lst
  | "$_DFF_PP1_",lst -> notsupp kind lst
  | "$_DFF_P_",lst -> notsupp kind lst
  | "$_DLATCHSR_NNN_",lst -> notsupp kind lst
  | "$_DLATCHSR_NNP_",lst -> notsupp kind lst
  | "$_DLATCHSR_NPN_",lst -> notsupp kind lst
  | "$_DLATCHSR_NPP_",lst -> notsupp kind lst
  | "$_DLATCHSR_PNN_",lst -> notsupp kind lst
  | "$_DLATCHSR_PNP_",lst -> notsupp kind lst
  | "$_DLATCHSR_PPN_",lst -> notsupp kind lst
  | "$_DLATCHSR_PPP_",lst -> notsupp kind lst
  | "$_DLATCH_NN0_",lst -> notsupp kind lst
  | "$_DLATCH_NN1_",lst -> notsupp kind lst
  | "$_DLATCH_NP0_",lst -> notsupp kind lst
  | "$_DLATCH_NP1_",lst -> notsupp kind lst
  | "$_DLATCH_N_",lst -> notsupp kind lst
  | "$_DLATCH_PN0_",lst -> notsupp kind lst
  | "$_DLATCH_PN1_",lst -> notsupp kind lst
  | "$_DLATCH_PP0_",lst -> notsupp kind lst
  | "$_DLATCH_PP1_",lst -> notsupp kind lst
  | "$_DLATCH_P_",lst -> notsupp kind lst
  | "$_FF_",lst -> notsupp kind lst
  | "$_MUX16_",lst -> notsupp kind lst
  | "$_MUX4_",lst -> notsupp kind lst
  | "$_MUX8_",lst -> notsupp kind lst
  | "$_NMUX_",lst -> notsupp kind lst
  | "$_OAI3_",lst -> notsupp kind lst
  | "$_OAI4_",lst -> notsupp kind lst
  | "$_SDFFCE_NN0N_",lst -> notsupp kind lst
  | "$_SDFFCE_NN0P_",lst -> notsupp kind lst
  | "$_SDFFCE_NN1N_",lst -> notsupp kind lst
  | "$_SDFFCE_NN1P_",lst -> notsupp kind lst
  | "$_SDFFCE_NP0N_",lst -> notsupp kind lst
  | "$_SDFFCE_NP0P_",lst -> notsupp kind lst
  | "$_SDFFCE_NP1N_",lst -> notsupp kind lst
  | "$_SDFFCE_NP1P_",lst -> notsupp kind lst
  | "$_SDFFCE_PN0N_",lst -> notsupp kind lst
  | "$_SDFFCE_PN0P_",lst -> notsupp kind lst
  | "$_SDFFCE_PN1N_",lst -> notsupp kind lst
  | "$_SDFFCE_PN1P_",lst -> notsupp kind lst
  | "$_SDFFCE_PP0N_",lst -> notsupp kind lst
  | "$_SDFFCE_PP1N_",lst -> notsupp kind lst
  | "$_SDFFCE_PP1P_",lst -> notsupp kind lst
  | "$_SDFFE_NN0N_",lst -> notsupp kind lst
  | "$_SDFFE_NN0P_",lst -> notsupp kind lst
  | "$_SDFFE_NN1N_",lst -> notsupp kind lst
  | "$_SDFFE_NN1P_",lst -> notsupp kind lst
  | "$_SDFFE_NP0N_",lst -> notsupp kind lst
  | "$_SDFFE_NP0P_",lst -> notsupp kind lst
  | "$_SDFFE_NP1N_",lst -> notsupp kind lst
  | "$_SDFFE_NP1P_",lst -> notsupp kind lst
  | "$_SDFFE_PN0N_",lst -> notsupp kind lst
  | "$_SDFFE_PN0P_",lst -> notsupp kind lst
  | "$_SDFFE_PN1N_",lst -> notsupp kind lst
  | "$_SDFFE_PN1P_",lst -> notsupp kind lst
  | "$_SDFFE_PP0N_",lst -> notsupp kind lst
  | "$_SDFFE_PP1N_",lst -> notsupp kind lst
  | "$_SDFFE_PP1P_",lst -> notsupp kind lst
  | "$_SDFF_NN0_",lst -> notsupp kind lst
  | "$_SDFF_NN1_",lst -> notsupp kind lst
  | "$_SDFF_NP0_",lst -> notsupp kind lst
  | "$_SDFF_NP1_",lst -> notsupp kind lst
  | "$_SDFF_PN0_",lst -> notsupp kind lst
  | "$_SDFF_PN1_",lst -> notsupp kind lst
  | "$_SDFF_PP1_",lst -> notsupp kind lst
  | "$_SR_NN_",lst -> notsupp kind lst
  | "$_SR_NP_",lst -> notsupp kind lst
  | "$_SR_PN_",lst -> notsupp kind lst
  | "$_SR_PP_",lst -> notsupp kind lst
  | "$_TBUF_",lst -> notsupp kind lst
  | oth,lst ->
      othlst := lst;
      othwlst := [];
      Hashtbl.iter (fun k -> function
          | Some x -> othwlst := (k,fpp x) :: !othwlst
          | None -> othwlst := (k,"") :: !othwlst) ind.wires;
      othwlst := List.sort compare !othwlst;
      othst := List.map (fun (pin, net, _) -> Hashtbl.find_opt ind.stash net) lst;
      failwith ("func: unmatched "^oth)

and recurse ind klst signal = function
| Some (kind, inst, conns) ->
  if not (List.mem inst klst) then
    begin
    if verbose then print_endline ("recurse into " ^ E.string_of_signal signal ^ " (instance: " ^ inst ^ ", kind: " ^ kind ^ " )");
    func ind (inst :: klst) kind conns;
    if verbose then print_endline ("recurse into "^E.string_of_signal signal^" returned from " ^ kind);
    end
  else
    begin
    dbgfunc := (kind,conns,signal,Hashtbl.find ind.wires signal) :: !dbgfunc;
    if verbose then print_endline ("instance: " ^ inst ^ " (kind : " ^ kind ^ " ) already searched")
    end;
  Hashtbl.find ind.wires signal
| None ->
  print_endline ("recurse into "^E.string_of_signal signal^" failed"); None

and getcon ind klst pin = function
  | E.GND -> Some F.f_false
  | PWR -> Some F.f_true
  | signal -> match Hashtbl.find_opt ind.wires signal with
    | Some (Some _ as x) -> x
    | Some None -> if pin <> "Y" then recurse ind klst signal (Hashtbl.find_opt ind.stash signal) else None
    | None -> failwith (E.string_of_signal signal ^" not declared")

and conn' ind klst = function
  | TokConn ([TokID pin], [Sigspec90 (signal, ix)]) -> let s = idx signal ix in pin, s, getcon ind klst pin s
  | TokConn ([TokID pin], [TokID signal]) -> let s = (scalar signal) in pin, s, getcon ind klst pin s
  | TokConn ([TokID pin], [TokVal lev]) -> let s = (cnv_pwr lev) in pin, s, getcon ind klst pin s
  | oth -> othconn := Some oth; failwith "conn'"

and pinmap ind klst conns = List.sort compare (List.map (conn' ind klst) conns)

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
| Conn_stmt96 ([(Sigspec92 _ | TokID _ | Sigspec90 _ | Sigspecrange _) as conc1], [(Sigspec92 _ | TokID _ | TokVal _ | Sigspecrange _) as conc2]) ->
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
| Proc_stmt(string,ilang_lst,ilang_lst',ilang_lst2) -> ()
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
| oth -> oth' := Some oth; failwith "cnv_ilst"

and cnv_ilst ind lst = List.iter (cnv_ilang ind) lst

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

let cnv_sat arg' =
  let wh = Hashtbl.create 255 in
  let wid = Hashtbl.create 255 in
  let ffh = Hashtbl.create 255 in
  let sh = Hashtbl.create 255 in
  print_endline ("Reading rtlil: "^arg');
  let _,arg = Input_rewrite.parse arg' in
  let ind = {wires=wh;inffop=ffh;stash=sh;wid=wid} in
  cnv_ilst ind arg;
  Hashtbl.iter (fun _ (kind,inst,conns) -> func ind [inst] kind conns) sh;
  let hlst=ref [] in
  Hashtbl.iter (fun k -> function
    | Some x -> othh := x; hlst := (k, fpp x) :: !hlst
    | None -> if verbose then print_endline (arg'^": "^E.string_of_signal k^" is not used")) wh;
  let inffoplst=ref [] in
  Hashtbl.iter (fun k () -> inffoplst := (k, match Hashtbl.find wh k with
    | Some x -> x
    | None -> print_endline ("ffh: " ^ E.string_of_signal k^" is undefined"); atom (scalar "\\")) :: !inffoplst) ffh;
  let widlst=ref [] in
  Hashtbl.iter (fun k n -> widlst := (k, n) :: !widlst) wid;
  !hlst, List.sort compare !inffoplst, !widlst

let rewrite_rtlil v =
  print_endline ("Parsing: "^v);
  Matchmly.modules := [];
  let p = parse v in
  let p' = rw p in
  let x = Matchmly.mly p' in
  let fnam = v^"_dump.rtlil" in
  let fd = open_out fnam in
  print_endline ("File: "^fnam);
  let modlst = ref [] in
  List.iter (fun (k, x) -> dbgx := Some x;
                let rtl = Dump_rtlil.template Matchmly.modules x in
		modlst := (k, rtl) :: !modlst;
                output_string fd "# Generated by Source_text_main intended for yosys consumption only, use at your own risk\n";
                List.iter (fun itm -> output_string fd (Input_dump.dump_ilang "" itm)) rtl
		) !(Matchmly.modules);
  close_out fd;
  let fnam = v^"_dump.ys" in
  let fd = open_out fnam in
  print_endline ("File: "^fnam);
  fprintf fd "read_ilang %s_dump.rtlil\n" v;
  fprintf fd "synth\n";
  fprintf fd "write_ilang %s_dump_synth.rtlil\n" v;
  fprintf fd "read_verilog -sv -overwrite %s\n" v;
  fprintf fd "synth\n";
  fprintf fd "write_ilang %s_golden_synth.rtlil\n" v;
  close_out fd;
  let _ = match Unix.system("yosys -q "^fnam) with
  | WEXITED errno -> if errno <> 0 then
    print_endline ("yosys failed with error code: "^(string_of_int errno))
    else
      begin
      print_endline "yosys succeeded";
      let hlst, inffoplst, wlst = cnv_sat (v^"_golden_synth.rtlil") in
      let hlst', inffoplst', wlst' = cnv_sat (v^"_dump_synth.rtlil") in
      let inffoplst1,inffoplst2 = List.split inffoplst in
      let inffoplst1',inffoplst2' = List.split inffoplst' in
      print_endline ("Golden (yosys) primary inputs/flipflop inputs/final outputs: "^String.concat "; " (List.map E.string_of_signal inffoplst1));
      print_endline ("Revised (our) primary inputs/flipflop inputs/final outputs: "^String.concat "; " (List.map E.string_of_signal inffoplst1'));
      print_endline ("Endpoint comparison: "^String.concat "; " (List.map (fun (k, itm) ->
          let k' = E.string_of_signal k in
          match List.assoc_opt k inffoplst with
            | Some itm' -> k' ^ ": " ^ string_of_bool ( ep (xor2 itm itm') )
            | None -> k' ^ ": not compared"
          ) inffoplst'))
(*
      let xorall = List.map2 xor2 inffoplst2 inffoplst2' in
      let mitre = if xorall <> [] then F.make_or xorall else failwith "mitre" in
      let res = ep mitre in ()
*)
      end;
    errno
  | WSIGNALED signal ->
    printf "yosys killed by signal %d\n" signal;
    signal
  | WSTOPPED signal ->
    printf "yosys killed by signal %d\n" signal;
    signal in
  !modlst, x, p, p'

let _ = if Array.length Sys.argv > 1 then Array.iteri (fun ix itm -> try
    if ix > 0 then let modlst,x,p,p' = rewrite_rtlil itm in
    List.iter (fun (k,_) -> print_endline ("PASSED: "^itm^"("^k^")")) modlst
    with err -> print_endline ("FAILED: "^itm)) Sys.argv

