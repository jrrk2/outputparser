open Input_rewrite_types
open Source_text_rewrite_types
open Source_text_rewrite

let verbose = try int_of_string (Sys.getenv "CNF_VERBOSE") > 0 with err -> false

let othst = ref []
let dbgfunc = ref []
let othwlst = ref []
let othconn = ref None

let rec func ind klst kind conns = match kind, pinmap ind klst conns with
  | "", ["",E.GND,None] -> failwith "GND" (* dummy to force type inference *)
  | "$_AND_",("\\A", _, Some a) :: ("\\B", _, Some b) :: ("\\Y", y, found) :: [] -> if found = None then addfunc ind y (and2 a b)
  | "$_ANDNOT_",("\\A", _, Some a) :: ("\\B", _, Some b) :: ("\\Y", y, found) :: [] -> if found = None then addfunc ind y (and2 a (knot b))
  | "$_BUF_",("\\A", _, Some a) :: ("\\Y", y, found) :: [] -> if found = None then addfunc ind y a
  | "$_MUX_",("\\A", _, Some a) :: ("\\B", _, Some b) :: ("\\S", _, Some s) :: ("\\Y", y, found) :: [] -> if found = None then addfunc ind y (mux2 a b s)
  | "$_NOT_",("\\A", _, Some a) :: ("\\Y", y, found) :: [] -> if found = None then addfunc ind y (knot a)
  | "$_DFF_P_",("\\C", clk, c) :: ("\\D", data, Some d) :: ("\\Q", q, Some _) :: [] -> addnxt "nxt" ind data d q
  | "$_DFF_PP0_",("\\C", clk, c) :: ("\\D", data, Some d) :: ("\\Q", q, Some _) :: ("\\R", rst, Some r) :: [] -> addnxt "nxt" ind data (and2 r d) q
  | "$_SDFF_PP0_",("\\C", clk, c) :: ("\\D", data, Some d) :: ("\\Q", q, Some _) :: ("\\R", rst, Some r) :: [] -> addnxt "nxt" ind data (and2 r d) q
  | "$_SDFF_PN0_",("\\C", clk, c) :: ("\\D", data, Some d) :: ("\\Q", q, Some _) :: ("\\R", rst, Some r) :: [] -> addnxt "nxt" ind data (and2 r d) q
  | "$_SDFF_PN1_",("\\C", clk, c) :: ("\\D", data, Some d) :: ("\\Q", q, Some _) :: ("\\R", rst, Some r) :: [] -> addnxt "nxt" ind data (and2 r d) q
  | "$_SDFFE_PP0P_",("\\C", clk, c) :: ("\\D", data, Some d) :: ("\\E", enable, Some e) :: ("\\Q", q, Some _) :: ("\\R", rst, Some r) :: [] -> addnxt "nxt" ind data (and2 r (mux2 (atom q) d e)) q
  | "$_SDFFE_PP0N_",("\\C", clk, c) :: ("\\D", data, Some d) :: ("\\E", enable, Some e) :: ("\\Q", q, Some _) :: ("\\R", rst, Some r) :: [] -> addnxt "nxt" ind data (and2 r (mux2 (atom q) d e)) q
  | "$_SDFFE_PP1P_",("\\C", clk, c) :: ("\\D", data, Some d) :: ("\\E", enable, Some e) :: ("\\Q", q, Some _) :: ("\\R", rst, Some r) :: [] -> addnxt "nxt" ind data (or2 r (mux2 (atom q) d e)) q
  | "$_SDFFCE_PP0P_",("\\C", clk, c) :: ("\\D", data, Some d) :: ("\\E", enable, Some e) :: ("\\Q", q, Some _) :: ("\\R", rst, Some r) :: [] -> addnxt "nxt" ind data (and2 (mux2 (atom q) d e) (knot r)) q
  | "$_DFFE_PP_",("\\C", clk, c) :: ("\\D", data, Some d) :: ("\\E", enable, Some e) :: ("\\Q", q, Some _) :: [] -> addnxt "nxt" ind data (mux2 (atom q) d e) q
  | "$_DFFE_PP0P_",("\\C", clk, c) :: ("\\D", data, Some d) :: ("\\E", enable, Some e) :: ("\\Q", q, Some _) :: ("\\R", rst, Some r) :: [] -> addnxt "nxt" ind data (and2 (mux2 (atom q) d e) (knot r)) q
  | "$_DFFE_PN_",("\\C", clk, c) :: ("\\D", data, Some d) :: ("\\E", enable, Some e) :: ("\\Q", q, Some _) :: [] -> addnxt "nxt" ind data (mux2 d (atom q) e) q
  | "$_SDFFCE_PP1P_",("\\C", clk, c) :: ("\\D", data, Some d) :: ("\\E", enable, Some e) :: ("\\Q", q, Some _) :: ("\\R", rst, Some r) :: [] -> addnxt "nxt" ind data (or2 r (mux2 (atom q) d e)) q
  | "$_SDFFCE_PN0P_",("\\C", clk, c) :: ("\\D", data, Some d) :: ("\\E", enable, Some e) :: ("\\Q", q, Some _) :: ("\\R", rst, Some r) :: [] -> addnxt "nxt" ind data (mux2 (atom q) (and2 r d) e) q
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
