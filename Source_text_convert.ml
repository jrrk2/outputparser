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
  let buf = Buffer.create 1000 in
  let fd = open_out fil in
  bprintf buf "open Input_rewrite_types\n";
  bprintf buf "open Source_text_rewrite_types\n";
  bprintf buf "open Source_text_rewrite\n";
  bprintf buf "\n";
  bprintf buf "let verbose = try int_of_string (Sys.getenv \"CNF_VERBOSE\") > 0 with err -> false\n";
  bprintf buf "\n";
  bprintf buf "let dbgfunc = ref []\n";
  bprintf buf "let othst = ref []\n";
  bprintf buf "let othclst = ref []\n";
  bprintf buf "let othwlst = ref []\n";
  bprintf buf "let othconn = ref None\n";
  bprintf buf "\n";
  bprintf buf "let rec func ind klst kind conns = match kind, pinmap ind klst conns with\n";
  bprintf buf "  | \"\", [\"\",E.GND,None] -> failwith \"GND\" (* dummy to force type inference *)\n";
  bprintf buf "\n";
  List.iter (dump_sat buf) modlst;
  bprintf buf "  | oth,lst ->\n";
  bprintf buf "      othclst := lst;\n";
  bprintf buf "      othwlst := [];\n";
  bprintf buf "      Hashtbl.iter (fun k -> function\n";
  bprintf buf "          | Some x -> othwlst := (k,fpp x) :: !othwlst\n";
  bprintf buf "          | None -> othwlst := (k,\"\") :: !othwlst) ind.wires;\n";
  bprintf buf "      othwlst := List.sort compare !othwlst;\n";
  bprintf buf "      othst := List.map (fun (pin, net, _) -> Hashtbl.find_opt ind.stash net) lst;\n";
  bprintf buf "      failwith (\"func: unmatched \"^oth)\n";
  bprintf buf "\n";
  bprintf buf "and recurse ind klst signal = function\n";
  bprintf buf "| Some (kind, inst, conns) ->\n";
  bprintf buf "  if not (List.mem inst klst) then\n";
  bprintf buf "    begin\n";
  bprintf buf "    if verbose then print_endline (\"recurse into \" ^ E.string_of_signal signal ^ \" (instance: \" ^ inst ^ \", kind: \" ^ kind ^ \" )\");\n";
  bprintf buf "    func ind (inst :: klst) kind conns;\n";
  bprintf buf "    if verbose then print_endline (\"recurse into \"^E.string_of_signal signal^\" returned from \" ^ kind);\n";
  bprintf buf "    end\n";
  bprintf buf "  else\n";
  bprintf buf "    begin\n";
  bprintf buf "    dbgfunc := (kind,conns,signal,Hashtbl.find ind.wires signal) :: !dbgfunc;\n";
  bprintf buf "    if verbose then print_endline (\"instance: \" ^ inst ^ \" (kind : \" ^ kind ^ \" ) already searched\")\n";
  bprintf buf "    end;\n";
  bprintf buf "  Hashtbl.find ind.wires signal\n";
  bprintf buf "| None ->\n";
  bprintf buf "  print_endline (\"recurse into \"^E.string_of_signal signal^\" failed\"); None\n";
  bprintf buf "\n";
  bprintf buf "and getcon ind klst pin = function\n";
  bprintf buf "  | E.GND -> Some F.f_false\n";
  bprintf buf "  | PWR -> Some F.f_true\n";
  bprintf buf "  | signal -> match Hashtbl.find_opt ind.wires signal with\n";
  bprintf buf "    | Some (Some _ as x) -> x\n";
  bprintf buf "    | Some None -> if pin <> \"Y\" then recurse ind klst signal (Hashtbl.find_opt ind.stash signal) else None\n";
  bprintf buf "    | None -> failwith (E.string_of_signal signal ^\" not declared\")\n";
  bprintf buf "\n";
  bprintf buf "and conn' ind klst = function\n";
  bprintf buf "  | TokConn ([TokID pin], [Sigspec90 (signal, ix)]) -> let s = idx signal ix in pin, s, getcon ind klst pin s\n";
  bprintf buf "  | TokConn ([TokID pin], [TokID signal]) -> let s = (scalar signal) in pin, s, getcon ind klst pin s\n";
  bprintf buf "  | TokConn ([TokID pin], [TokVal lev]) -> let s = (cnv_pwr lev) in pin, s, getcon ind klst pin s\n";
  bprintf buf "  | oth -> othconn := Some oth; failwith \"conn'\"\n";
  bprintf buf "\n";
  bprintf buf "and pinmap ind klst conns = List.sort compare (List.map (conn' ind klst) conns)\n";
  bprintf buf "\n";
  Buffer.output_buffer fd buf;
  close_out fd;
  modlst, x, p, p', Buffer.contents buf

let modlst,x,p,p',status = if Array.length Sys.argv >= 3 then rewrite_sat Sys.argv.(1) Sys.argv.(2) else ([],Deflt,Default,Default,"")
