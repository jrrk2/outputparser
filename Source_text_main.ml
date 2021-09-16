open Printf
open Source_text
open Source_text_lex
open Source_text_rewrite

(*
let rewrite_vhdl v =
  Matchmly.modules := [];
  let p = parse v in
  let p' = rw p in
  let x = Matchmly.mly p' in
  let fd = open_out (v^"_dump.vhd") in 
  let modlst = ref [] in
  List.iter (fun (k, x) ->
		modlst := k :: !modlst;
		Dump_vhdl.template fd Matchmly.modules x;
		) !(Matchmly.modules);
  close_out fd;
  let modlst = !modlst in
  modlst, x, p, p'

let rewrite_sysver v =
  Matchmly.modules := [];
  let p = parse v in
  let p' = rw p in
  let x = Matchmly.mly p' in
  let fd = open_out (v^"_dump.sv") in 
  let modlst = ref [] in
  List.iter (fun (k, x) ->
		modlst := k :: !modlst;
		Dump_sysver.template fd Matchmly.modules x;
		) !(Matchmly.modules);
  close_out fd;
  let modlst = !modlst in
  modlst, x, p, p'
  *)

let dbgx = ref None

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
                List.iter (fun itm -> output_string fd (Input_dump.dump_ilang "" itm)) rtl
		) !(Matchmly.modules);
  close_out fd;
  let fnam = v^"_dump.ys" in
  let fd = open_out fnam in
  print_endline ("File: "^fnam);
  fprintf fd "read_ilang %s_dump.rtlil\n" v;
  fprintf fd "proc\n";
  fprintf fd "write_verilog %s_dump.proc\n" v;
  fprintf fd "synth\n";
  fprintf fd "write_verilog %s_dump.synth\n" v;
  close_out fd;
  let _ = match Unix.system("yosys "^fnam) with
  | WEXITED errno -> if errno <> 0 then
    printf "yosys failed with error code %d\n" errno;
    errno
  | WSIGNALED signal ->
    printf "yosys killed by signal %d\n" signal;
    signal
  | WSTOPPED signal ->
    printf "yosys killed by signal %d\n" signal;
    signal in
  !modlst, x, p, p'

(*
open Vxml_types

let topxml = ref None

let rewrite_xml v top =
  Matchmly.modules := [];
  let p = parse v in
  let p' = rw p in
  let _ = Matchmly.mly p' in
  let modlst = ref [] in
  let xml = List.map (fun (k, x) ->
		let x' = Dump_xml.template Matchmly.modules x in
		modlst := (k,x') :: !modlst;
                x') !(Matchmly.modules) in
  let top' = List.assoc "apb_uart" !modlst in
  topxml := Some top';
  let xml = Dump_xml.template_header top' xml in
  let errlst = ref [] in
  let (x:typetable_t) = (Vxml_types.BASDTYP, "logic", Vxml_types.TYPNONE, []) in
  let rslt = Vxml.translate errlst (0,(0,0),xml,[]) top in
  !modlst, !errlst, rslt, xml, x, p, p'
*)

let _ = if Array.length Sys.argv > 1 then Array.iteri (fun ix itm -> try
    if ix > 0 then let modlst,x,p,p' = rewrite_rtlil itm in
    List.iter (fun (k,_) -> print_endline ("PASSED: "^itm^"("^k^")")) modlst
    with err -> print_endline ("FAILED: "^itm)) Sys.argv

