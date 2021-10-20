open Printf
open Source_text_misc
open Input
open Input_rewrite_types

let verbose = try int_of_string (Sys.getenv "CNF_VERBOSE") with err -> 0
let sep_rtl = try int_of_string (Sys.getenv "CNF_SEP_RTL") > 0 with err -> false

let compare v v' =
  let status = ref true in
  let fnam = v^"_dump.ys" in
  let fd = open_out fnam in
  if verbose > 1 then print_endline ("Yosys command file: "^fnam);
  if verbose > 1 then print_endline ("File: "^v);
  fprintf fd "read_verilog -sv -overwrite %s\n" v;
  fprintf fd "synth\n";
  fprintf fd "write_ilang %s_golden_synth.rtlil\n" v;
  if verbose > 1 then print_endline ("File: "^v');
  fprintf fd "read_verilog -overwrite %s\n" v';
  fprintf fd "synth\n";
  fprintf fd "write_ilang %s_opt_synth.rtlil\n" v';
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
      let revlst = cnv_sat Convert.func (v'^"_opt_synth.rtlil") in
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
  !status

let _ = if Array.length Sys.argv >= 3 then
    let status = compare Sys.argv.(1) Sys.argv.(2) in
    print_endline (if status then "PASSED: " else "FAILED: ")
