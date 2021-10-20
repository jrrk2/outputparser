open Printf
open Source_text
open Source_text_lex
open Source_text_misc
open Source_text_rewrite
open Source_text_rewrite_types
open Input
open Input_rewrite_types

let verbose = try int_of_string (Sys.getenv "CNF_VERBOSE") with err -> 0
let sep_rtl = try int_of_string (Sys.getenv "CNF_SEP_RTL") > 0 with err -> false
let dumpver = try int_of_string (Sys.getenv "DUMP_VER") > 0 with err -> false

let dbgx = ref []
let dbgopt = ref []

let dbgtree = ref Source_text.EMPTY_TOKEN
let dbgmatch = ref Deflt

let preproc_parse v =
  let p = Source_text_rewrite.parse_output_ast_from_pipe v in
  let p' = Source_text_rewrite.rw p in
  dbgtree := p';
  let x = Matchmly.mly p' in
  dbgmatch := x;
  x, p, p'

let rewrite_rtlil v =
  let status = ref true in
  if verbose > 1 then print_endline ("Parsing: "^v);
  Matchmly.modules := [];
(*
  let p = Source_text_preproc.parse' Source_text_rewrite.parse_output_ast_from_function v in
*)
  let x, p, p' = preproc_parse v in
  let optlst = ref [] in
  let fnam = v^"_dump.ys" in
  let fd = open_out fnam in
  if verbose > 1 then print_endline ("Yosys command file: "^fnam);
  let fnam' = v^"_dump.v" in
  let fnam'' = v^"_dump.rtlil" in
  let fd' = open_out fnam' in
  let fd'' = open_out fnam'' in
(* *)
  fprintf fd "read_verilog -sv -overwrite %s\n" v;
  fprintf fd "write_ilang %s_golden_proc.rtlil\n" v;
  fprintf fd "synth\n";
  fprintf fd "write_ilang %s_golden_synth.rtlil\n" v;
  fprintf fd "write_verilog %s_golden_synth.v\n" v;
(* *)
  List.iter (fun (k, x) ->
                dbgx := (k, x) :: !dbgx;
                let sub = Source_text_simplify.template Matchmly.modules x in
		optlst := (k, sub) :: !optlst;
		) !(Matchmly.modules);
  if not sep_rtl then
    begin
      close_out fd';
      close_out fd'';
    end;
  let optlst = !optlst in
  dbgopt := optlst;
  List.iter (fun (k,sub) ->
      let fnam3 = v^"_dump_"^k^".opt.v" in
      fprintf fd "read_verilog -sv -overwrite %s\n" fnam3;
      let fd3 = open_out fnam3 in
      print_endline ("Dumping: " ^ k ^ " to file: "^fnam3);
      Dump_sysver.dump_template fd3 optlst sub;
      close_out fd3) optlst;
  fprintf fd "write_ilang %s_opt_proc.rtlil\n" v;
  fprintf fd "synth\n";
  fprintf fd "write_ilang %s_opt_synth.rtlil\n" v;
  fprintf fd "write_verilog %s_opt_synth.v\n" v;
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
  fprintf fd "synth\n";
  fprintf fd "write_ilang %s_dump_synth.rtlil\n" v;
  fprintf fd "write_verilog %s_dump_synth.v\n" v;
  close_out fd;
  let script = "yosys "^(if verbose > 3 then "-X " else "-q ")^fnam in
  let _ = match Unix.system script with
  | WEXITED errno -> if errno <> 0 then
      begin
        print_endline ("yosys failed with error code: "^string_of_int errno^" (while executing "^script^")");
        status := false;
      end
    else
      begin
      if verbose > 1 then print_endline "yosys succeeded";
      let goldlst = cnv_sat Convert.func (v^"_golden_synth.rtlil") in
      let revlst = cnv_sat Convert.func (v^"_opt_synth.rtlil") in
      List.iter2 (fun (hlst, inffoplst, wlst) (hlst', inffoplst', wlst') ->
      let inffoplst1,inffoplst2 = List.split inffoplst in
      let inffoplst1',inffoplst2' = List.split inffoplst' in
      if verbose > 0 then begin
      print_endline ("Golden (yosys) primary inputs/flipflop inputs/final outputs: "^String.concat "; " (List.map string_of_signal inffoplst1));
      print_endline ("Revised (our) primary inputs/flipflop inputs/final outputs: "^String.concat "; " (List.map string_of_signal inffoplst1'));
      end;
      dumpitm := [];
      let ep_comparison = List.map (fun (k, itm) ->
          let k' = string_of_signal k in
          match List.assoc_opt k inffoplst with
            | Some itm' ->
              let stat = ep k' (xor2 itm itm') in
              if not stat then
                begin
                  dumpitm := (k, itm) :: !dumpitm;
                  dumpitm' := (k, itm') :: !dumpitm';
                  status := false;
                end;
              k' ^ ": " ^ string_of_bool stat
            | None ->
              status := false;
              k' ^ ": not compared"
          ) inffoplst' in
      dumpitm := List.rev !dumpitm;
      if verbose > 0 then print_endline ("Endpoint comparison: "^String.concat "; " ep_comparison;
        )) goldlst revlst;
      print_endline ("Overall comparison: "^ string_of_bool !status);
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
  optlst, x, p, p', !status

let _ = if Array.length Sys.argv > 1 then Array.iteri (fun ix itm -> try
    if ix > 0 then let optlst,x,p,p',status = rewrite_rtlil itm in
    List.iter (fun (k,_) -> print_endline ((if status then "PASSED: " else "FAILED: ")^itm^"("^k^")")) optlst
    with err -> print_endline ("FAILED: "^itm)) Sys.argv

