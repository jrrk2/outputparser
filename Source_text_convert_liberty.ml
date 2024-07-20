open Printf
open Source_text_rewrite
open Source_text_rewrite_types

type dir =
  | In
  | Out

type model =
  | Int of int
  | Ident of string
  | Posedge of string
  | Negedge of string
  | InOut of dir * string
  | Reg of dir * string
  | Edge of model list * model list
  | Mux of model list * model list * model list
  | Assign of model list * model list
  | Eq of model list * model list
  | And2 of model list * model list
  | Or2 of model list * model list
  | Xor2 of model list * model list
  | Not of model list
  | Z

let verbose = try int_of_string (Sys.getenv "CONVERT_VERBOSE") > 0 with err -> false

let othx = ref None
let itmlst = ref []
let itms' = ref []
let othdump = ref None

let io_cnv = function
| Source_text_rewrite_types.In -> In
| Out -> Out
| oth -> othx := Some oth; failwith "io_cnv"

let rec scan_itms' = function
| Id id -> Ident id :: []
| Number (2, 1, 35, "z") -> Z :: []
| Number (b, w, n, _) -> Int n :: []
| Pos (Id id) -> Posedge id :: []
| Neg (Id id) -> Negedge id :: []
| Itmlst lst -> List.flatten (List.map (scan_itms) lst)
| Port (PortDir (dir, Atom "reg"), nam, [], Deflt) -> Reg(io_cnv dir,nam) :: []
| Port (dir, nam, [], Deflt) -> InOut(io_cnv dir, nam) :: []
| Seq ("", lst) -> List.flatten (List.map (scan_itms) lst)
(*
| If1 (cond, Seq(lbl, stmt :: [])) -
     Seq ("",
      [If2 (Equals (Id "R", Number (10, 32, 0, "")),
        Equate (Id "Q", Number (10, 32, 0, "")), Equate (Id "Q", Id "D"))]))
*)
| If1 (cond', Equate (q', rhsq')) ->
 let mux1 = Mux(scan_itms cond', scan_itms rhsq', scan_itms q') in
 Assign(scan_itms q', mux1 :: []) :: []
| If1 (cond,
     If2 (cond',
      Equate (q', rhsq'), Equate (q'', rhsq''))) when (q'=q'') ->
 let mux1 = Mux(scan_itms cond', scan_itms rhsq', scan_itms rhsq'') in
 let mux2 = Mux(scan_itms cond, mux1 :: [], scan_itms q') in
 Assign(scan_itms q', mux2 :: []) :: []
| If2 (cond,
     Equate (q, rhsq),
     If2 (cond',
      Equate (q', rhsq'),
      If1 (cond'', Equate (q'', rhsq'')))) when (q=q') && (q=q'') ->
 let mux0 = Mux(scan_itms cond'', scan_itms rhsq'', scan_itms q'') in
 let mux1 = Mux(scan_itms cond', scan_itms rhsq', mux0 :: []) in
 let mux2 = Mux(scan_itms cond, scan_itms rhsq, mux1 :: []) in
 Assign(scan_itms q', mux2 :: []) :: []
| If2 (cond,
     Equate (q, rhsq),
     If2 (cond',
      Equate (q', rhsq'), Equate (q'', rhsq''))) when (q=q') && (q=q'') ->
 let mux1 = Mux(scan_itms cond', scan_itms rhsq', scan_itms q') in
 let mux2 = Mux(scan_itms cond, scan_itms rhsq, mux1 :: []) in
 Assign(scan_itms q', mux2 :: []) :: []
| If2 (cond,
     Equate (q, rhsq),
     If1 (cond',
      Equate (q', rhsq')))  when q=q' ->
 let mux1 = Mux(scan_itms cond', scan_itms rhsq', scan_itms q') in
 let mux2 = Mux(scan_itms cond, scan_itms rhsq, mux1 :: []) in
 Assign(scan_itms q', mux2 :: []) :: []
| If2 (cond,
     Equate (q, rhsq),
     Equate (q', rhsq')) when q=q' ->
 let mux2 = Mux(scan_itms cond, scan_itms rhsq, scan_itms rhsq') in
 Assign(scan_itms q', mux2 :: []) :: []
| Query (cond, then', else') -> Mux(scan_itms cond, scan_itms then', scan_itms else') :: []
| Equate (q, expr) -> Assign(scan_itms q, scan_itms expr) :: []
| Pling (Id _ as id) -> Not (scan_itms id) :: []
| Tilde expr -> Not (scan_itms expr) :: []
| Expression expr -> scan_itms expr
| Equals (a, b) -> Eq (scan_itms a, scan_itms b) :: []
| And (a, b) -> And2 (scan_itms a, scan_itms b) :: []
| Or (a, b) -> Or2 (scan_itms a, scan_itms b) :: []
| Xor (a, b) -> Xor2 (scan_itms a, scan_itms b) :: []
| ContAsgn (Asgn1 (y, expr) :: []) -> Assign(scan_itms y, scan_itms expr) :: []
| AlwaysLegacy (At (EventOr ((Pos _ | Neg _) :: _ as edg)), stmts) -> Edge (List.flatten (List.map (scan_itms) edg), scan_itms stmts) :: []
| AlwaysLegacy (AtStar, stmt) -> Edge([], scan_itms stmt) :: []
| AlwaysLegacy (At (EventOr (Id _::_)), stmt) -> Edge([], scan_itms stmt) :: []
| oth -> othx := Some oth; failwith "scan_itms"

and opt = function
| If1 (cond, Seq (_, stmt :: [])) -> If1(cond, stmt)
| oth -> oth

and scan_itms x = scan_itms' (opt x)

let template = function
| Modul (nam, [], ports, itms) ->
let ports' = List.map (function
        | Port (Deflt, nam, [], Deflt) -> nam
        | oth -> othx := Some oth; failwith "ports") ports in
itms' := itms;
(List.sort compare ports', List.flatten (List.map (scan_itms) itms))
| oth -> othx := Some oth; failwith "template"

let rec dump_expr = function
| Int 0 -> "F.f_false"
| Int 1 -> "F.f_true"
| Ident "Q" -> "atom q"
| Ident id -> String.lowercase_ascii id
| Eq ([lhs], [Int 0]) -> "knot ("^dump_expr lhs^")"
| Eq ([lhs], [Int 1]) -> "("^dump_expr lhs^")"
| And2 ([lhs], [rhs]) -> "and2 ("^dump_expr lhs^") ("^dump_expr rhs^")"
| Or2 ([lhs], [rhs]) -> "or2 ("^dump_expr lhs^") ("^dump_expr rhs^")"
| Xor2 ([lhs], [rhs]) -> "xor2 ("^dump_expr lhs^") ("^dump_expr rhs^")"
| Mux ([sel'], [then'], [else']) -> "mux2 ("^dump_expr else'^") ("^dump_expr then'^") ("^dump_expr sel'^")"
| Not [rhs] -> "knot ("^dump_expr rhs^")"
| Z -> "F.make_atom (E.make (E.SCALAR \"Z\"))"
| oth -> othdump := Some oth; failwith "dump_expr"

let dump_sat buf (nam, (ports, itms)) =
  bprintf buf "  | \"%s\"," nam;
  itmlst := itms;
  let itms' = List.map (function
      | InOut (In, ipin) -> sprintf "(\"\\\\%s\", _, Some %s) :: " ipin (dump_expr (Ident ipin))
      | InOut (Out, opin) -> sprintf "(\"\\\\%s\", %s, found) :: " opin (dump_expr (Ident opin))
      | Reg (Out, "Q") -> sprintf "(\"\\\\Q\", q, Some _) :: "
      | Reg (Out, y) -> sprintf "(\"\\\\%s\", %s, found) :: " y (dump_expr (Ident y))
      | Assign ([y], [expr]) -> sprintf "[] -> if found = None then addfunc ind %s (%s)\n" (dump_expr y) (dump_expr expr);
      | Edge ((Posedge _ | Negedge _) :: _, [Assign ([Ident "Q"], [expr])]) -> sprintf "[] -> addnxt \"nxt\" ind (%s) (q)\n" (dump_expr expr)
      | Edge ([], [Assign ([Ident "Q"], [expr])]) -> sprintf "[] -> addnxt \"nxt\" ind (%s) (q)\n" (dump_expr expr)
      | oth -> othdump := Some oth; failwith "dump_sat") itms in
  let itms' = List.sort compare itms' in
  List.iter (Buffer.add_string buf) itms'

let parse arg =
  let ch = open_in arg in
  let rslt = parse_output_ast_from_chan ch in
  close_in ch;
  rslt

let othfunc' = ref None
let othmodlst = ref []
let othfun = ref None

let dir' = function
    | In -> "input; }"
    | Out -> "output;"

let rec func' = function
    | Ident id -> id
    | Not [x] -> "("^func' x ^ ")'"
    | And2 ([a], [b]) -> "("^func' a^"*"^func' b^")"
    | Or2 ([a], [b]) -> "("^func' a^"+"^func' b^")"
    | Xor2 ([a], [b]) -> "("^func' a^"^"^func' b^")"
    | Mux ([s], [n], [m]) -> "(("^func' s^" * "^func' m^") | ("^func' n^" * "^func' s^"'))"
    | Z -> "Z"
    | oth -> othfunc' := Some oth; failwith "func'"

let preset = function
    | 0 -> "clear"
    | 1 -> "preset"

let dump_liberty fd nam = function
(*
| [], _ ->
  output_string fd ("\tcell(DFF_N) {\n");
  output_string fd ("\t\tff(IQ, IQN) {\n");
  output_string fd ("\t\t\tclocked_on: \"!C\";\n");
  output_string fd ("\t\t\tnext_state: \"D\";\n");
  output_string fd ("\t\t}\n");
  output_string fd ("\t\tpin(D) { direction: input; }\n");
  output_string fd ("\t\tpin(C) { direction: input; clock: true; }\n");
  output_string fd ("\t\tpin(Q) { direction: output; function: \"IQ\"; }\n");
  output_string fd ("\t}\n");
| [], _ ->
  output_string fd ("\tcell(DFF_P) {\n");
  output_string fd ("\t\tff(IQ, IQN) {\n");
  output_string fd ("\t\t\tclocked_on: \"C\";\n");
  output_string fd ("\t\t\tnext_state: \"D\";\n");
  output_string fd ("\t\t}\n");
  output_string fd ("\t\tpin(D) { direction: input; }\n");
  output_string fd ("\t\tpin(C) { direction: input; clock: true; }\n");
  output_string fd ("\t\tpin(Q) { direction: output; function: \"IQ\"; }\n");
  output_string fd ("\t}\n");
| [], _ ->
  output_string fd ("\tcell(DFF_NN0) {\n");
  output_string fd ("\t\tff(IQ, IQN) {\n");
  output_string fd ("\t\t\tclocked_on: \"!C\";\n");
  output_string fd ("\t\t\tnext_state: \"D\";\n");
  output_string fd ("\t\t\tclear: \"!R\";\n");
  output_string fd ("\t\t}\n");
  output_string fd ("\t\tpin(D) { direction: input; }\n");
  output_string fd ("\t\tpin(R) { direction: input; }\n");
  output_string fd ("\t\tpin(C) { direction: input; clock: true; }\n");
  output_string fd ("\t\tpin(Q) { direction: output; function: \"IQ\"; }\n");
  output_string fd ("\t}\n");
| [], _ ->
  output_string fd ("\tcell(DFF_NN1) {\n");
  output_string fd ("\t\tff(IQ, IQN) {\n");
  output_string fd ("\t\t\tclocked_on: \"!C\";\n");
  output_string fd ("\t\t\tnext_state: \"D\";\n");
  output_string fd ("\t\t\tpreset: \"!R\";\n");
  output_string fd ("\t\t}\n");
  output_string fd ("\t\tpin(D) { direction: input; }\n");
  output_string fd ("\t\tpin(R) { direction: input; }\n");
  output_string fd ("\t\tpin(C) { direction: input; clock: true; }\n");
  output_string fd ("\t\tpin(Q) { direction: output; function: \"IQ\"; }\n");
  output_string fd ("\t}\n");
| [], _ ->
  output_string fd ("\tcell(DFF_NP0) {\n");
  output_string fd ("\t\tff(IQ, IQN) {\n");
  output_string fd ("\t\t\tclocked_on: \"!C\";\n");
  output_string fd ("\t\t\tnext_state: \"D\";\n");
  output_string fd ("\t\t\tclear: \"R\";\n");
  output_string fd ("\t\t}\n");
  output_string fd ("\t\tpin(D) { direction: input; }\n");
  output_string fd ("\t\tpin(R) { direction: input; }\n");
  output_string fd ("\t\tpin(C) { direction: input; clock: true; }\n");
  output_string fd ("\t\tpin(Q) { direction: output; function: \"IQ\"; }\n");
  output_string fd ("\t}\n");
| [], _ ->
  output_string fd ("\tcell(DFF_NP1) {\n");
  output_string fd ("\t\tff(IQ, IQN) {\n");
  output_string fd ("\t\t\tclocked_on: \"!C\";\n");
  output_string fd ("\t\t\tnext_state: \"D\";\n");
  output_string fd ("\t\t\tpreset: \"R\";\n");
  output_string fd ("\t\t}\n");
  output_string fd ("\t\tpin(D) { direction: input; }\n");
  output_string fd ("\t\tpin(R) { direction: input; }\n");
  output_string fd ("\t\tpin(C) { direction: input; clock: true; }\n");
  output_string fd ("\t\tpin(Q) { direction: output; function: \"IQ\"; }\n");
  output_string fd ("\t}\n");
| [], _ ->
  output_string fd ("\tcell(DFF_PN0) {\n");
  output_string fd ("\t\tff(IQ, IQN) {\n");
  output_string fd ("\t\t\tclocked_on: \"C\";\n");
  output_string fd ("\t\t\tnext_state: \"D\";\n");
  output_string fd ("\t\t\tclear: \"!R\";\n");
  output_string fd ("\t\t}\n");
  output_string fd ("\t\tpin(D) { direction: input; }\n");
  output_string fd ("\t\tpin(R) { direction: input; }\n");
  output_string fd ("\t\tpin(C) { direction: input; clock: true; }\n");
  output_string fd ("\t\tpin(Q) { direction: output; function: \"IQ\"; }\n");
  output_string fd ("\t}\n");
| [], _ ->
  output_string fd ("\tcell(DFF_PN1) {\n");
  output_string fd ("\t\tff(IQ, IQN) {\n");
  output_string fd ("\t\t\tclocked_on: \"C\";\n");
  output_string fd ("\t\t\tnext_state: \"D\";\n");
  output_string fd ("\t\t\tpreset: \"!R\";\n");
  output_string fd ("\t\t}\n");
  output_string fd ("\t\tpin(D) { direction: input; }\n");
  output_string fd ("\t\tpin(R) { direction: input; }\n");
  output_string fd ("\t\tpin(C) { direction: input; clock: true; }\n");
  output_string fd ("\t\tpin(Q) { direction: output; function: \"IQ\"; }\n");
  output_string fd ("\t}\n");
| [], _ ->
  output_string fd ("\tcell(DFF_PP0) {\n");
  output_string fd ("\t\tff(IQ, IQN) {\n");
  output_string fd ("\t\t\tclocked_on: \"C\";\n");
  output_string fd ("\t\t\tnext_state: \"D\";\n");
  output_string fd ("\t\t\tclear: \"R\";\n");
  output_string fd ("\t\t}\n");
  output_string fd ("\t\tpin(D) { direction: input; }\n");
  output_string fd ("\t\tpin(R) { direction: input; }\n");
  output_string fd ("\t\tpin(C) { direction: input; clock: true; }\n");
  output_string fd ("\t\tpin(Q) { direction: output; function: \"IQ\"; }\n");
  output_string fd ("\t}\n");
*)
| portlst,
  Edge ([Posedge c; Posedge r],
   [Assign ([Ident q],
     [Mux ([Eq ([Ident r'], [Int 1])], [Int rst], [Ident d])])]) :: funlst when r=r' ->
  output_string fd ("\tcell("^nam^") {\n");
  output_string fd ("\t\tff(IQ, IQN) {\n");
  output_string fd ("\t\t\tclocked_on: \""^c^"\";\n");
  output_string fd ("\t\t\tnext_state: \""^d^"\";\n");
  output_string fd ("\t\t\t"^preset rst^": \""^r^"\";\n");
  output_string fd ("\t\t}\n");
  output_string fd ("\t  area: 6;\n");
  List.iter (function
     | InOut (In, c') when c=c' -> output_string fd ("\t\tpin("^c^") { direction: input; clock: true; }\n");
     | InOut (dir, a) -> output_string fd ("\t  pin("^a^") { direction: "^dir' dir^"\n")
     | Reg (Out, q) -> output_string fd ("\t\tpin("^q^") { direction: output; function: \"IQ\"; }\n");
     | oth -> othfun := Some oth; failwith "funlst") (List.rev funlst);
  output_string fd ("\t}\n");

| portlst,
  Assign([Ident y], [Mux ([Ident e], [Ident a], [Z])]) :: funlst ->
  output_string fd ("\tcell("^nam^") {\n");
  output_string fd ("\t  area: 4;\n");

  List.iter (function
     | InOut (dir, a) -> output_string fd ("\t  pin("^a^") { direction: "^dir' dir^"\n")
     | oth -> othfun := Some oth; failwith "funlst") (List.rev funlst);
  output_string fd ("\t\t   function: \""^a^"\";\n\t\t   three_state: \""^e^"\"; }\n");
  output_string fd ("\t}\n");

| portlst,
  (Assign _ as asgn) :: funlst ->
  output_string fd ("\tcell("^nam^") {\n");
  output_string fd ("\t  area: 4;\n");

  List.iter (function
     | InOut (dir, a) -> output_string fd ("\t  pin("^a^") { direction: "^dir' dir^"\n")
     | Assign ([Ident y], func :: []) -> output_string fd ("\t\t   function: \""^func' func^"\"; }\n")
     | oth -> othfun := Some oth; failwith "funlst") (List.rev (asgn :: funlst));
  output_string fd ("\t}\n");

| _ -> print_endline ("Skipped: "^nam)
;;
let rewrite_sat v fil =
  print_endline ("Parsing: "^v);
  Matchmly.modules := [];
  let p = parse v in
  let p' = rw p in
  let x = Matchmly.mly p' in
  let modlst = ref [] in
  List.iter (fun (k, x) ->
                let rtl = template x in
		modlst := (k, rtl) :: !modlst;
		) !(Matchmly.modules);
  let modlst = List.sort compare !modlst in
  othmodlst := modlst;
  let fd = open_out fil in
  output_string fd ("library(yosys_cells) {\n");
  output_string fd ("  /* Threshold Definitions */\n");
  output_string fd ("  slew_lower_threshold_pct_fall         : 30.00 ;\n");
  output_string fd ("  slew_lower_threshold_pct_rise         : 30.00 ;\n");
  output_string fd ("  slew_upper_threshold_pct_fall         : 70.00 ;\n");
  output_string fd ("  slew_upper_threshold_pct_rise         : 70.00 ;\n");
  output_string fd ("  slew_derate_from_library              : 1.00 ;\n");
  output_string fd ("  input_threshold_pct_fall              : 50.00 ;\n");
  output_string fd ("  input_threshold_pct_rise              : 50.00 ;\n");
  output_string fd ("  output_threshold_pct_fall             : 50.00 ;\n");
  output_string fd ("  output_threshold_pct_rise             : 50.00 ;\n");
  output_string fd ("  default_leakage_power_density         : 0.00 ;\n");
  output_string fd ("  default_cell_leakage_power            : 0.00 ;\n");
  output_string fd ("\n");
  output_string fd ("  /* Default Pin Attributes */\n");
  output_string fd ("  default_inout_pin_cap                 : 1.000000;\n");
  output_string fd ("  default_input_pin_cap                 : 1.000000;\n");
  output_string fd ("  default_output_pin_cap                : 0.000000;\n");
  output_string fd ("  default_fanout_load                   : 1.000000;\n");
  output_string fd ("  default_max_transition                : 0.200000;\n");
  output_string fd ("\n");
  List.iter (fun (nam,(ports,func)) -> dump_liberty fd (String.concat "" (String.split_on_char '$' nam)) (ports,List.rev func)) modlst;
  output_string fd ("}\n");
  output_string fd ("\n");
  close_out fd;
  modlst

let modlst = try rewrite_sat "../../yosys/share/simcells.v" "simcells.lib" with err -> print_endline (Printexc.to_string err); []
