open Printf
open Source_text_misc
open Input
open Input_rewrite_types

let verbose = try int_of_string (Sys.getenv "CNF_VERBOSE") with err -> 0
let sep_rtl = try int_of_string (Sys.getenv "CNF_SEP_RTL") > 0 with err -> false

let sat_parse v =
  let chan = open_in v in
  let p = Source_text_rewrite.parse_output_ast_from_chan chan in
  let p' = Source_text_rewrite.rw p in
  let x = Matchmly.mly p' in
  match x with
    | Itmlst (Modul (nam, _, _, _) :: _ as x) -> cnv_satv' Convert.func [nam,x]
    | _ -> failwith "sat_parse"

let compare v v' =
  let status = ref true in
  let goldlst = sat_parse v in
  let revlst = sat_parse v' in
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
!status

let _ = if Array.length Sys.argv >= 3 then
    let status = compare Sys.argv.(1) Sys.argv.(2) in
    print_endline (if status then "PASSED" else "FAILED")
