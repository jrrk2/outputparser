open Printf
open Source_text
open Source_text_lex
open Source_text_rewrite
open Input
open Input_rewrite_types

let dbgx = ref None

module Sat = Msat_sat
module E = Sat.Int_lit (* expressions *)
module F = Msat_tseitin.Make(E)

type ind = {
  wires:(string, F.t option) Hashtbl.t;
  undef:(string * ilang list, unit) Hashtbl.t;
  conn:(string * string, unit) Hashtbl.t;
  ff:(string, unit) Hashtbl.t;
}

let oth' = ref None
let othopt = ref None
let othconn = ref None

let idx s i = s^"["^string_of_int i^"]"
let rec value str = Char.code str.[0] + 128 * (let len = String.length str in if len > 1 then value (String.sub str 1 (len-1)) else 0)

let getw ind string' =
  match Hashtbl.find_opt ind.wires string' with
    | Some x -> x
    | None -> failwith (string'^" not declared")

let addinp idx idx' ind string' =
  match Hashtbl.find_opt ind.wires string' with
    | Some x -> print_endline (string'^" redeclared")
    | None -> Hashtbl.add ind.wires string' ( Some ( F.make_atom ( E.make (idx*1000+idx') )) )

let addff ind string' = Hashtbl.replace ind.wires string' ( Some ( F.make_atom ( E.make (value string' ))))

let addwire idx' ind string' =
  match Hashtbl.find_opt ind.wires string' with
    | Some x -> print_endline (string'^" redeclared")
    | None -> Hashtbl.add ind.wires string' None

let addfunc ind string' func =
  match Hashtbl.find_opt ind.wires string' with
    | Some None -> Hashtbl.replace ind.wires string' (Some func)
    | Some _ -> print_endline (string'^" redeclared")
    | None -> print_endline (string'^" undefined")

let addconn ind lhs' rhs' =
  match getw ind rhs' with
   | Some func' -> addfunc ind lhs' func'
   | None -> Hashtbl.replace ind.conn (lhs',rhs') ()

let conn' ind = function
  | TokConn ([TokID pin], [Sigspec90 (signal, ix)]) -> let s = idx signal ix in pin, s, getw ind s
  | TokConn ([TokID pin], [TokID (signal)]) -> pin, signal, getw ind signal
  | oth -> othconn := Some oth; failwith "conn'"

let addnxt ind data d q =
   let lhs' = "nxt$"^q in
   addwire 0 ind lhs';
   Hashtbl.add ind.ff lhs' ();
   match d with
       | Some d -> addfunc ind lhs' d
       | None -> Hashtbl.replace ind.conn (lhs',data) ()

let pinmap ind conns = List.sort compare (List.map (conn' ind) conns)

let func ind kind conns = match kind, pinmap ind conns with
  | "$_AND_",("\\A", _, Some a) :: ("\\B", _, Some b) :: ("\\Y", y, None) :: [] -> addfunc ind y (F.make_and [a;b])
  | "$_NOT_",("\\A", _, Some a) :: ("\\Y", y, None) :: [] -> addfunc ind y (F.make_not a)
  | "$_SDFF_PP0_",("\\C", clk, c) :: ("\\D", data, d) :: ("\\Q", q, None) :: ("\\R", rst, r) :: [] ->
     addff ind q;
     addnxt ind data d q;
  | "$_XOR_",("\\A", _, Some a) :: ("\\B", _, Some b) :: ("\\Y", y, None) :: [] -> addfunc ind y (F.make_xor a b)
  | oth,_ -> Hashtbl.replace ind.undef (kind,conns) ()

let rec cnv_ilang ind = function
| Autoidx_stmt26(int') -> ()
| Attr_stmt(string,ilang_lst') -> ()
| Module12(string,ilang_lst') -> print_endline string; List.iter (cnv_ilang ind) ilang_lst'
| Wire_stmt(options,string') -> let wid = ref None and fn = ref addwire in List.iter (function
    | Wire_optionswidth n -> wid := Some n;
    | Wire_optionsinput n -> fn := addinp n;
    | Wire_optionsoutput _ -> ()
    | oth -> othopt := Some oth; failwith "options") options;
    (match !wid with
      | None -> !fn 0 ind string'
      | Some n -> for i = 0 to n-1 do !fn (i+1) ind (idx string' i) done); ()
| Cell_stmt(kind,inst,params,conns) -> func ind kind conns; ()
| Conn_stmt96 ([Sigspecrange (lhs, hi, lo)], [Sigspecrange (rhs, hi', lo')]) ->
for i = 0 to hi-lo do
  let lhs' = idx lhs (hi-i) in
  let rhs' = idx rhs (hi'-i) in
  addconn ind lhs' rhs'
done
| Conn_stmt96 ([Sigspec90 (lhs, ix)], [Sigspec90 (rhs, ix')]) ->
  let lhs' = idx lhs ix in
  let rhs' = idx rhs ix' in
  addconn ind lhs' rhs'
(*
| Conn_stmt96([lft],[rght]) -> ()
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
| Param_defval_stmt24(string,ilang_lst') -> ()
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

let pp q =
  let buf' = Buffer.create 1000 in
  let buf = Format.formatter_of_buffer buf' in
  List.iter (fun itm ->
    List.iter (fun itm -> E.pp buf itm; Format.pp_print_space buf ()) itm;
    Format.pp_print_flush buf ();
    ) (F.make_cnf q);
  print_endline (Buffer.contents buf')

let fpp q =
  let buf' = Buffer.create 1000 in
  let buf = Format.formatter_of_buffer buf' in
  F.pp buf q;
  Format.pp_print_flush buf ();
  print_endline (Buffer.contents buf')

let ep q =
    let m = F.make_cnf q in
    let solver = Sat.create () in
    Sat.assume solver m ();
    Sat.solve solver

let cnv_sat arg' =
  let ch = Hashtbl.create 255 in
  let ch' = Hashtbl.create 255 in
  let ch'' = Hashtbl.create 255 in
  let wh = Hashtbl.create 255 in
  let uh = Hashtbl.create 255 in
  let uh' = Hashtbl.create 255 in
  let uh'' = Hashtbl.create 255 in
  let ffh = Hashtbl.create 255 in
  let _,arg = Input_rewrite.parse arg' in
  cnv_ilst {wires=wh;undef=uh;conn=ch;ff=ffh} arg;
  Hashtbl.iter (fun (kind,conns) () -> func {wires=wh;undef=uh';conn=ch;ff=ffh} kind conns) uh;
  Hashtbl.iter (fun (lhs',rhs') () -> addconn {wires=wh;undef=uh';conn=ch';ff=ffh} lhs' rhs') ch;
  Hashtbl.iter (fun (kind,conns) () -> func {wires=wh;undef=uh'';conn=ch;ff=ffh} kind conns) uh';
  Hashtbl.iter (fun (lhs',rhs') () -> addconn {wires=wh;undef=uh';conn=ch'';ff=ffh} lhs' rhs') ch';
  let ulst=ref [] in
  Hashtbl.iter (fun (kind,conns) () -> ulst := (kind,pinmap {wires=wh;undef=uh';conn=ch;ff=ffh} conns) :: !ulst) uh'';
  let clst=ref [] in
  Hashtbl.iter (fun k x -> clst := (k,x) :: !clst) ch'';
  let hlst=ref [] in
  Hashtbl.iter (fun k -> function
    | Some x -> hlst := (k,x) :: !hlst
    | None -> hlst := (k,F.make_atom (E.fresh())) :: !hlst; print_endline (arg'^": "^k)) wh;
  let fflst=ref [] in
  Hashtbl.iter (fun k () -> fflst := (k,List.assoc k !hlst) :: !fflst) ffh;
  !clst, !hlst, !ulst, List.sort compare !fflst

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
(*
  fprintf fd "proc\n";
  fprintf fd "opt\n";
  fprintf fd "write_ilang %s_dump_proc.rtlil\n" v;
  fprintf fd "write_verilog %s_dump_proc.vsynth\n" v;
*)
  fprintf fd "synth\n";
  fprintf fd "write_ilang %s_dump_synth.rtlil\n" v;
(*
  fprintf fd "write_verilog %s_dump_synth.vsynth\n" v;
*)
  fprintf fd "read_verilog -overwrite %s\n" v;
(*
  fprintf fd "proc\n";
  fprintf fd "opt\n";
  fprintf fd "write_ilang %s_golden_proc.rtlil\n" v;
  fprintf fd "write_verilog %s_golden_proc.vsynth\n" v;
*)
  fprintf fd "synth\n";
  fprintf fd "write_ilang %s_golden_synth.rtlil\n" v;
(*
  fprintf fd "write_verilog %s_golden_synth.vsynth\n" v;
*)
  close_out fd;
  let _ = match Unix.system("yosys "^fnam) with
  | WEXITED errno -> if errno <> 0 then
    printf "yosys failed with error code %d\n" errno
    else
      begin
      let clst, hlst, ulst, fflst = cnv_sat (v^"_golden_synth.rtlil") in
      let clst', hlst', ulst', fflst' = cnv_sat (v^"_dump_synth.rtlil") in
(*
      let q = Array.init 3 (fun ix -> List.assoc (idx "nxt$\\q" ix) hlst) in
      let clk = List.assoc "\\clk" hlst in
      let q' = Array.init 3 (fun ix -> List.assoc (idx "nxt$\\q" ix) hlst') in
      let clk' = List.assoc "\\clk" hlst' in
      let mitre = F.make_or (List.map2 (F.make_xor) (Array.to_list q) (Array.to_list q')) in
*)
      let fflst1,fflst2 = List.split fflst in
      let fflst1',fflst2' = List.split fflst' in
      print_endline ("Golden (yosys) flipflop inputs: "^String.concat "; " fflst1);
      print_endline ("Revised (our) flipflop inputs: "^String.concat "; " fflst1');
      let mitre = F.make_or (List.map2 (F.make_xor) fflst2 fflst2') in
      let res = ep mitre in
      match res with
      | Sat.Sat _ -> print_endline "SATISFIABLE (netlists mismatched)"
      | Sat.Unsat _ -> print_endline "UNSATISFIABLE (netlists match)"
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

