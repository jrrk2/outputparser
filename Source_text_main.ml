open Printf
open Source_text
open Source_text_lex
open Source_text_rewrite
open Source_text_rewrite_types
open Input
open Input_rewrite_types

let verbose = try int_of_string (Sys.getenv "CNF_VERBOSE") with err -> 0
let sep_rtl = try int_of_string (Sys.getenv "CNF_SEP_RTL") > 0 with err -> false
let dumpver = try int_of_string (Sys.getenv "DUMP_VER") > 0 with err -> false

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
let othexp = ref None

let rec explode_lst = function
  | [] -> []
  | Sigspecrange (lhs, hi, lo) :: tl -> List.init (hi-lo+1) (fun ix -> idx lhs (hi-ix)) @ explode_lst tl
  | Sigspec90 (lhs, ix) :: tl -> idx lhs ix :: explode_lst tl
  | TokVal tok :: tl -> explode_const tok @ explode_lst tl
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
| Param_stmt23(string,ilang_lst') -> () (* placeholder *)

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
| oth -> oth' := Some oth; failwith "cnv_ilang"

let mycnf' = ref F.f_false
let mycnf = ref [[E.transparent (E.fresh ())]]
let othh = ref F.f_false

let ep form =
    if verbose > 1 then print_endline "Dumping cnf";
    mycnf' := form;
    if verbose > 1 then print_endline "Building cnf";
    let m = F.make_cnf form in
    mycnf := List.map (List.map E.transparent) m;
    let solver = Msat_sat_slit.create () in
    Msat_sat_slit.assume solver m ();
    match Msat_sat_slit.solve solver with
      | Msat_sat_slit.Sat _ -> if verbose > 1 then print_endline "SATISFIABLE (netlists mismatched)"; false
      | Msat_sat_slit.Unsat _ -> if verbose > 1 then print_endline "UNSATISFIABLE (netlists match)"; true

let cnv_sat arg' =
  if verbose > 1 then print_endline ("Reading rtlil: "^arg');
  let _,arg = Input_rewrite.parse arg' in
  List.map (fun (nam,itm) ->
      let wh = Hashtbl.create 255 in
      let wid = Hashtbl.create 255 in
      let ffh = Hashtbl.create 255 in
      let sh = Hashtbl.create 255 in
      let ind = {wires=wh;inffop=ffh;stash=sh;wid=wid} in
      if verbose > 1 then print_endline ("Converting: "^nam);
      List.iter (cnv_ilang ind) itm;
      Hashtbl.iter (fun _ (kind,inst,conns) -> Convert.func ind [inst] kind conns) sh;
      let hlst=ref [] in
      Hashtbl.iter (fun k -> function
          | Some x -> othh := x; hlst := (k, fpp x) :: !hlst
          | None -> if verbose > 0 then print_endline (arg'^": "^E.string_of_signal k^" is not used")) wh;
      let inffoplst=ref [] in
      Hashtbl.iter (fun k () -> inffoplst := (k, match Hashtbl.find wh k with
        | Some x -> x
        | None -> print_endline ("ffh: " ^ E.string_of_signal k^" is undefined"); atom (scalar "\\")) :: !inffoplst) ffh;
      let widlst=ref [] in
      Hashtbl.iter (fun k n -> widlst := (k, n) :: !widlst) wid;
      if verbose > 1 then print_endline ("inffopslt length: "^string_of_int (List.length !inffoplst));
      !hlst, List.sort compare !inffoplst, !widlst
  ) arg

let dbgtree = ref Source_text.EMPTY_TOKEN
let dbgmatch = ref Deflt

let rewrite_rtlil v =
  let status = ref true in
  if verbose > 1 then print_endline ("Parsing: "^v);
  Matchmly.modules := [];
(*
  let p = Source_text_preproc.parse' Source_text_rewrite.parse_output_ast_from_function v in
*)
  let p = Source_text_rewrite.parse_output_ast_from_pipe v in
  let p' = rw p in
  dbgtree := p';
  let x = Matchmly.mly p' in
  dbgmatch := x;
  let modlst = ref [] in
  let fnam = v^"_dump.ys" in
  let fd = open_out fnam in
  if verbose > 1 then print_endline ("Yosys command file: "^fnam);
  let fnam' = v^"_dump.v" in
  let fnam'' = v^"_dump.rtlil" in
  let fd' = open_out fnam' in
  let fd'' = open_out fnam'' in
  fprintf fd "read_verilog -sv -overwrite %s\n" v;
  fprintf fd "write_ilang %s_golden_proc.rtlil\n" v;
  fprintf fd "synth\n";
  fprintf fd "write_ilang %s_golden_synth.rtlil\n" v;
  if not sep_rtl then
    begin
      if verbose > 1 then print_endline ("File: "^fnam');
      if dumpver then
        begin
        fprintf fd "read_verilog -overwrite %s\n" fnam';
        fprintf fd "write_ilang %s.ilang\n" fnam';
        end
      else fprintf fd "read_ilang -overwrite %s\n" fnam'';
    end;
  List.iter (fun (k, x) ->
                dbgx := x :: !dbgx;
                let rtl = Dump_rtlil.template Matchmly.modules x in
		modlst := (k, rtl) :: !modlst;
                if sep_rtl then
                    begin
                      let fnam' = v^"_dump_"^k^".v" in
                      let fnam'' = v^"_dump_"^k^".rtlil" in
                      let fd' = open_out fnam' in
                      let fd'' = open_out fnam'' in
                      if dumpver then print_endline ("File: "^fnam')
                          else print_endline ("File: "^fnam'');
                      if dumpver then
                        begin
                        fprintf fd "read_verilog -overwrite %s\n" fnam';
                        fprintf fd "write_ilang %s.ilang\n" fnam';
                        end
                      else
                        fprintf fd "read_ilang -overwrite %s\n" fnam'';
                      Ver_dump.dump fd' rtl;
                      Input_dump.dump fd'' rtl;
                      close_out fd';
                      close_out fd'';
                      end
                else
                  begin
                      Ver_dump.dump fd' rtl;
                      Input_dump.dump fd'' rtl;
                    end
		) !(Matchmly.modules);
  if not sep_rtl then
    begin
      close_out fd'
    end;
  fprintf fd "synth\n";
  fprintf fd "write_ilang %s_dump_synth.rtlil\n" v;
  close_out fd;
  let script = "yosys "^(if verbose > 1 then "-X " else "-q ")^fnam in
  let _ = match Unix.system script with
  | WEXITED errno -> if errno <> 0 then
      begin
        print_endline ("yosys failed with error code: "^string_of_int errno^" (while executing "^script^")");
        status := false;
      end
    else
      begin
      if verbose > 1 then print_endline "yosys succeeded";
      let goldlst = cnv_sat (v^"_golden_synth.rtlil") in
      let revlst = cnv_sat (v^"_dump_synth.rtlil") in
      List.iter2 (fun (hlst, inffoplst, wlst) (hlst', inffoplst', wlst') ->
      let inffoplst1,inffoplst2 = List.split inffoplst in
      let inffoplst1',inffoplst2' = List.split inffoplst' in
      if verbose > 0 then begin
      print_endline ("Golden (yosys) primary inputs/flipflop inputs/final outputs: "^String.concat "; " (List.map E.string_of_signal inffoplst1));
      print_endline ("Revised (our) primary inputs/flipflop inputs/final outputs: "^String.concat "; " (List.map E.string_of_signal inffoplst1'));
      end;
      let ep_comparison = List.map (fun (k, itm) ->
          let k' = E.string_of_signal k in
          match List.assoc_opt k inffoplst with
            | Some itm' -> 
              let stat = ep (xor2 itm itm') in
              if not stat then status := false;
              k' ^ ": " ^ string_of_bool stat
            | None ->
              status := false;
              k' ^ ": not compared"
          ) inffoplst' in
      if verbose > 0 then print_endline ("Endpoint comparison: "^String.concat "; " ep_comparison;
        )) goldlst revlst;
      print_endline ("Overall comparison: "^ string_of_bool !status);
(*
      let xorall = List.map2 xor2 inffoplst2 inffoplst2' in
      let mitre = if xorall <> [] then F.make_or xorall else failwith "mitre" in
      let res = ep mitre in ()
*)
      end;
    errno
  | WSIGNALED signal ->
    print_endline ("yosys killed by signal "^string_of_int signal^" (while executing "^script^")");
    status := false;
    signal
  | WSTOPPED signal ->
    printf "yosys stopped by signal %d\n" signal;
    status := false;
    signal in
  !modlst, x, p, p', !status

let _ = if Array.length Sys.argv > 1 then Array.iteri (fun ix itm -> try
    if ix > 0 then let modlst,x,p,p',status = rewrite_rtlil itm in
    List.iter (fun (k,_) -> print_endline ((if status then "PASSED: " else "FAILED: ")^itm^"("^k^")")) modlst
    with err -> print_endline ("FAILED: "^itm)) Sys.argv

