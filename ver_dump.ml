open Printf
open Input
open Input_rewrite_types

type ver_dump = 
| Oth of ilang list * string
| Input of int option * string
| Output of int option * string
| Wire of int option * string
| Cell of string * string * ilang list * ilang list
| Conn of ilang list * ilang list
| Proc of string * ilang list * ilang list * ilang list

type ind = {mod':string ref;
            ports:(int,ver_dump)Hashtbl.t;
            wires:(string,ver_dump)Hashtbl.t;
            regs:(string,unit)Hashtbl.t;
            cells:(string,ver_dump)Hashtbl.t;
            proc:(string,ver_dump)Hashtbl.t;
            conn:(ver_dump,unit)Hashtbl.t}

let othv = ref None
let othedg = ref None
let dbgrtl = ref []
let dbgports = ref []

let tokval s = String.concat "'b" (String.split_on_char '\'' s)

let rec dump_ver (ind:ind) = function
| Assign_stmt67(ilang_lst,ilang_lst') -> "assign "^dump_vlst ind ilang_lst^" "^dump_vlst ind ilang_lst'^"\n"
| Attr_list56(ilang_lst,ilang_lst') -> "Attr_list56("^dump_vlst ind ilang_lst^", "^dump_vlst ind ilang_lst' ^")"
| Attr_stmt(string,ilang_lst') -> "// attribute "^string^" "^dump_vlst ind ilang_lst'^"\n"
| Autoidx_stmt26(int') -> "autoidx "^string_of_int int'
| Case_body63(ilang_lst,ilang_lst') -> "Case_body63("^dump_vlst ind ilang_lst^", "^dump_vlst ind ilang_lst'^")"
| Case_body64(ilang_lst,ilang_lst') -> "Case_body64("^dump_vlst ind ilang_lst^", "^dump_vlst ind ilang_lst'^")"
| Case_body65(ilang_lst,ilang_lst') -> "Case_body65("^dump_vlst ind ilang_lst^", "^dump_vlst ind ilang_lst'^")"
| Cell_bodyconnect(ilang_lst,string,ilang_lst',ilang_lst2) -> "Cell_bodyconnect("^dump_vlst ind ilang_lst^", "^string^", "^dump_vlst ind ilang_lst'^", "^dump_vlst ind ilang_lst2^")"
| Cell_bodyparam(ilang_lst,string,ilang_lst',ilang_lst2) -> "Cell_bodyparam("^dump_vlst ind ilang_lst^", "^string^", "^dump_vlst ind ilang_lst'^", "^dump_vlst ind ilang_lst2 ^")"
| Cell_bodypreal(ilang_lst,string,ilang_lst',ilang_lst2) -> "Cell_bodypreal("^dump_vlst ind ilang_lst^", "^string^", "^dump_vlst ind ilang_lst'^", "^dump_vlst ind ilang_lst2 ^")"
| Cell_bodypsigned(ilang_lst,string,ilang_lst',ilang_lst2) -> "Cell_bodypsigned("^dump_vlst ind ilang_lst^", "^string^", "^dump_vlst ind ilang_lst'^", "^dump_vlst ind ilang_lst2^")"
| Cell_stmt(string,string',ilang_lst,ilang_lst') -> Hashtbl.add ind.cells string' (Cell (string, string', ilang_lst, List.sort compare ilang_lst')); string'
| Compare_list61(ilang_lst,ilang_lst') -> " "^dump_vlst ind ilang_lst^" , "^dump_vlst ind ilang_lst'^" "
| Conn_stmt96(ilang_lst,ilang_lst') -> Hashtbl.add ind.conn (Conn(ilang_lst,ilang_lst')) (); ""
| Design6(ilang_lst,ilang_lst') -> "Design6("^dump_vlst ind ilang_lst^", "^dump_vlst ind ilang_lst'^")"
| Design7(ilang_lst,ilang_lst') -> "Design7("^dump_vlst ind ilang_lst^", "^dump_vlst ind ilang_lst'^")"
| Design8(ilang_lst,ilang_lst') -> "Design8("^dump_vlst ind ilang_lst^", "^dump_vlst ind ilang_lst'^")"
| Input2(ilang_lst,ilang_lst') -> "Input2("^dump_vlst ind ilang_lst^", "^dump_vlst ind ilang_lst'^")"
| Memory_optionsoffset(int') -> " offset "^string_of_int int'
| Memory_optionssize(int') -> " size "^string_of_int int'
| Memory_optionswidth(int') -> " width "^string_of_int int'
| Memory_stmt39(ilang_lst,string') -> "memory "^dump_vlst ind ilang_lst^" "^string'^"\n"
| Module12(string,ilang_lst') -> ind.mod' := string; dump_vlst ind ilang_lst'
| Module_body13(ilang_lst,ilang_lst') -> "Module_body13("^dump_vlst ind ilang_lst^", "^dump_vlst ind ilang_lst ^")"
| Param_defval_stmt24(string,ilang_lst') -> "parameter "^string^" "^dump_vlst ind ilang_lst'^"\n"
| Param_stmt23(string,ilang_lst') -> "Param_stmt23("^string^", "^dump_vlst ind ilang_lst'^")"
| Proc_stmt(string,ilang_lst,ilang_lst',ilang_lst2) -> Hashtbl.add ind.proc string (Proc (string, ilang_lst,ilang_lst',ilang_lst2)); string
| Signed -> " signed"
| Sigspec90(string, int') -> string^" ["^string_of_int int'^"]"
| Sigspec92(ilang_lst') -> "{ "^dump_vlst ind ilang_lst'^" }"
| Sigspec_list_reversed93(ilang_lst,ilang_lst') -> "Sigspec_list_reversed93("^dump_vlst ind ilang_lst^", "^dump_vlst ind ilang_lst ^")"
| Sigspecrange(string,int,int') -> string^" ["^string_of_int int^":"^string_of_int int'^"]"
| Switch_bodycase(ilang_lst,ilang_lst',ilang_lst2) -> "case "^dump_vlst ind ilang_lst^dump_vlst ind ilang_lst'^dump_vlst ind ilang_lst2^"\n"
| Switch_stmt(ilang_lst,ilang_lst',ilang_lst2,ilang_lst3) -> "switch "^dump_vlst ind ilang_lst^"\n"^dump_vlst ind ilang_lst'^"\n"^dump_vlst ind ilang_lst2^"\n"^dump_vlst ind ilang_lst3^"\nend\n"
| Sync_list69(ilang_lst',ilang_lst2,ilang_lst3,ilang_lst4) -> "sync "^dump_vlst ind ilang_lst'^" "^dump_vlst ind ilang_lst2^" "^dump_vlst ind ilang_lst3^"\n    "^dump_vlst ind ilang_lst4^"\n"
| Sync_listalways(ilang_lst',ilang_lst2) -> "sync always\n  "^dump_vlst ind ilang_lst'^" "^dump_vlst ind ilang_lst2^"\n"
| Sync_listglobal(ilang_lst',ilang_lst2) -> "sync global "^dump_vlst ind ilang_lst'^" "^dump_vlst ind ilang_lst2
| Sync_listinit(ilang_lst',ilang_lst2) -> "sync init "^dump_vlst ind ilang_lst'^" "^dump_vlst ind ilang_lst2^"\n"
| Update_list82(ilang_lst',ilang_lst2) -> "update "^dump_vlst ind ilang_lst'^", "^dump_vlst ind ilang_lst2^"\n"
| Update_listmemwr(string,ilang_lst2,ilang_lst3,ilang_lst4,ilang_lst5) -> "memwr "^string^" "^dump_vlst ind ilang_lst2^" "^dump_vlst ind ilang_lst3^" "^dump_vlst ind ilang_lst4^" "^dump_vlst ind ilang_lst5^"\n"
| Upto -> " upto "
| Wire_optionsinout(int) -> " inout "^string_of_int int
| Wire_optionsinput(int) -> " input "^string_of_int int
| Wire_optionsinvalid -> " optionsinvalid"
| Wire_optionsoffset(int') -> " offset "^string_of_int int'
| Wire_optionsoutput(int') -> " output "^string_of_int int'
| Wire_optionswidth(int') -> " width "^string_of_int int'
| Wire_stmt([Wire_optionswidth w; Wire_optionsoutput ix], nam) -> Hashtbl.replace ind.ports ix (Output (Some w,nam)); nam
| Wire_stmt([Wire_optionswidth w; Wire_optionsinput ix], nam) -> Hashtbl.replace ind.ports ix (Input (Some w,nam)); nam
| Wire_stmt([Wire_optionsoutput ix], nam) -> Hashtbl.replace ind.ports ix (Output (None,nam)); nam
| Wire_stmt([Wire_optionsinput ix], nam) -> Hashtbl.replace ind.ports ix (Input (None,nam)); nam
| Wire_stmt([Wire_optionswidth w], nam) -> Hashtbl.replace ind.wires nam (Wire (Some w,nam)); nam
| Wire_stmt([], nam) -> Hashtbl.replace ind.wires nam (Wire (None,nam)); nam
| Wire_stmt(oth, nam) -> failwith nam
| TokCase(ilang_lst,ilang_lst') -> "TokCase("^dump_vlst ind ilang_lst^", "^dump_vlst ind ilang_lst'
| TokConn(ilang_lst,ilang_lst') -> Hashtbl.add ind.conn (Conn(ilang_lst,ilang_lst')) (); ""
| TokParam(TokID s::[],ilang_lst') -> "."^String.sub s 1 (String.length s - 1)^"("^dump_vlst ind ilang_lst'^")"
| TokUpdate(ilang_lst,ilang_lst') -> "update "^dump_vlst ind ilang_lst^" "^dump_vlst ind ilang_lst'^"\n"
| TokInt(int) -> string_of_int int
| TokID(string) -> string
| TokVal(string) -> tokval string
| TokStr(string) -> "\""^string^"\""
| TokPos -> "posedge"
| TokNeg -> "negedge"
| TokEdge -> "edge"
| oth -> othv := Some oth; failwith "dump_ver"

and dump_vlst ind lst = String.concat "\n" (List.map (dump_ver ind) lst)

let conn = ref None
let proc = ref None
let ops = ref []
let reg' = ref None

let rec reg ind = function
  | TokID q -> Hashtbl.replace ind.regs q ()
  | Sigspec90 (q,ix) -> Hashtbl.replace ind.regs q ()
  | Sigspecrange (q,hi,lo) -> Hashtbl.replace ind.regs q ()
  | Sigspec92 lst -> List.iter (reg ind) lst
  | oth -> reg' := Some oth; failwith "reg"

let tokkind = function
| "$add" -> "+"
| "$xor" -> "^"
| "$logic_and" -> "&&"
| "$logic_or" -> "||"
| "$le" -> "<="
| "$eq" -> "=="
| "$or" -> "|"
| "$gt" -> ">"
| "$ne" -> "!="
| "$sub" -> "-"
| "$mul" -> "*"
| "$ge" -> ">="
| "$lt" -> "<"
| "$and" -> "&"
| "$logic_not" -> "!"
| "$shiftx" -> ">>"
| "$tilde_and" -> "~&"
| "$sshr" -> ">>>"
| "$shr" -> ">>"
| "$reduce_and" -> " & "
| "$reduce_or" -> " | "
| "$shl" -> "<<"
| "$not" -> "~"
| oth -> if not (List.mem oth !ops) then ops := oth :: !ops; oth

let rec tokop = function
| TokID a -> "\\"^a^" "
| TokVal b -> tokval b
| Sigspec90 (id, ix) -> "\\"^id^" [ "^string_of_int ix^" ]"
| Sigspecrange (id, hi, lo) -> "\\"^id^" [ "^string_of_int hi^" : "^string_of_int lo^" ]"
| Sigspec92 lst -> " { " ^ String.concat ", " (List.map tokop lst) ^ " } "
| oth -> othv := Some oth; failwith "tokop"

let toksgn = function
| 1,1,a -> "$signed("^tokop a^")"
| _,_,a -> tokop a

let ixcomment ix = " /* "^string_of_int ix^" */"

let rec bodymap ind = function 
         | Assign_stmt67 ([lft], [rght]) -> reg ind lft; tokop lft^" = "^tokop rght
         | Switch_stmt ([sel], [], [], caselst) -> "case("^tokop sel^")\n"^String.concat "" (List.map (switchmap ind) caselst)^"endcase\n"
         | Assign_stmt67 ([], []) -> ""
         | oth -> othv := Some oth; failwith "bodymap"

and switchmap ind = function
         | Switch_bodycase ([], [], lst') -> "default: begin " ^ String.concat ";\n" (List.map (bodymap ind) lst') ^ "; end\n"
         | Switch_bodycase (caseval, [], lst') -> String.concat ", " (List.map tokop caseval) ^ " : begin " ^ String.concat ";\n" (List.map (bodymap ind) lst') ^ "; end\n"
         | oth -> othv := Some oth; failwith "switchmap"

let is_reg ind nam = if Hashtbl.mem ind.regs nam then "reg " else " "
let is_reg' ind nam = if Hashtbl.mem ind.regs nam then "reg " else "wire "
let cellh = Hashtbl.create 255

let dump_parm ind = function
  | TokParam(TokID s::[],ilang_lst') -> "."^String.sub s 1 (String.length s - 1)^"("^dump_vlst ind ilang_lst'^")"
  | oth -> othv := Some oth; failwith "dumpconn"

let dump_conn ind = function
  | TokConn(ilang_lst,ilang_lst') -> "."^dump_vlst ind ilang_lst^"("^dump_vlst ind ilang_lst'^")"
  | oth -> othv := Some oth; failwith "dumpconn"

let portmap ind = function
| Oth(lst,nam) -> failwith "oth"
| Input (Some n,nam) -> "input ["^string_of_int (n - 1)^":0] \\"^nam^" "
| Output (Some n,nam) -> "output "^is_reg ind nam^"["^string_of_int (n - 1)^":0] \\"^nam^" "
| Wire (Some n,nam) -> is_reg' ind nam ^"["^string_of_int (n - 1)^":0] \\"^nam^" "
| Input (None,nam) -> "input \\"^nam^" "
| Output (None,nam) -> "output "^is_reg ind nam^"\\"^nam^" "
| Wire (None,nam) -> is_reg' ind nam ^"\\"^nam^" "
| Conn ([lft], [rght]) as x -> conn := Some x; "assign "^tokop lft^" = "^tokop rght
| Cell (kind, inst, [],
     [TokConn ([TokID "A"], [a]);
      TokConn ([TokID "Y"], [y]);
      TokParam ([TokID "\\A_SIGNED"], [TokInt sa]);
      TokParam ([TokID "\\A_WIDTH"], [TokInt wa]);
      TokParam ([TokID "\\Y_WIDTH"], [TokInt wy])]) -> "assign "^tokop y^" = "^tokkind kind^" "^tokop a^" /* "^inst^" */"
| Cell (kind, inst, [],
     [TokConn ([TokID "A"], [a]);
      TokConn ([TokID "B"], [b]);
      TokConn ([TokID "Y"], [y]);
      TokParam ([TokID "\\A_SIGNED"], [TokInt sa]);
      TokParam ([TokID "\\A_WIDTH"], [TokInt wa]);
      TokParam ([TokID "\\B_SIGNED"], [TokInt sb]);
      TokParam ([TokID "\\B_WIDTH"], [TokInt wb]);
      TokParam ([TokID "\\Y_WIDTH"], [TokInt wy])]) -> "assign "^tokop y^" = "^toksgn (sa,sb,a)^" "^tokkind kind^" "^toksgn (sa,sb,b)^" /* "^inst^" */"
| Cell ("$mux", inst, [], (* the one and only ternary function *)
     [TokConn ([TokID "A"], [a]);
      TokConn ([TokID "B"], [b]);
      TokConn ([TokID "S"], [s]);
      TokConn ([TokID "Y"], [y]);
      TokParam ([TokID "\\WIDTH"], [TokInt wy])]) -> "assign "^tokop y^" = "^tokop s^" ? "^tokop b ^" : "^ tokop a ^" /* "^inst^" */"
| Cell ("$memrd", inst, [],
 [TokConn ([TokID "ADDR"], [adr]);
  TokConn ([TokID "CLK"], [clk]);
  TokConn ([TokID "DATA"], [data]);
  TokConn ([TokID "EN"], [en]);
  TokParam ([TokID "\\ABITS"], [TokInt abits]);
  TokParam ([TokID "\\CLK_ENABLE"], [TokInt clken]);
  TokParam ([TokID "\\CLK_POLARITY"], [TokInt clkpol]);
  TokParam ([TokID "\\MEMID"], [TokStr memid]);
  TokParam ([TokID "\\TRANSPARENT"], [TokInt transp]);
  TokParam ([TokID "\\WIDTH"], [TokInt width])]) -> "assign "^tokop data^" = "^memid^" [ "^tokop adr^" ] "
| Cell (kind, inst, params, conns') as x ->
let parms = List.filter (function TokParam _ -> true | _ -> false) conns' in
let conns = List.filter (function TokConn _ -> true | _ -> false) conns' in
 Hashtbl.replace cellh kind x; kind ^ (if parms <> [] then "  /* #(\n\t"^ String.concat ",\n\t" (List.map (dump_parm ind) parms) ^ ") */\n\t" else " ") ^
                                      inst^" (\n\t"^
                                      String.concat ",\n\t" (List.map (dump_conn ind) conns)^")"
| Proc (nam, [], lst1, lst2) as x -> proc := Some x;
  let edg,ulst = match lst2 with
    | Sync_list69 ([TokPos], [TokID clk], [], ulst) :: [] -> "\nalways @(posedge "^clk^")\n",ulst
    | Sync_listalways ([], ulst) :: [] -> "\nalways @*\n", ulst
    | Sync_list69 ([TokPos], [TokID clk], [], ulst) :: Sync_list69 ([TokPos], [TokID rst], [], ulst') :: [] -> "\nalways @(posedge "^clk^" or posedge "^rst^")\n",ulst
    | oth -> othedg := Some oth; failwith "procedge" in edg ^ "begin\n" ^
  String.concat ";\n" (List.mapi (fun ix itm -> bodymap ind itm ^ ixcomment ix) lst1) ^ ";\n" ^
  String.concat ";\n" (List.mapi (fun ix -> function
                                    | Assign_stmt67 ([], []) -> ""
                                    | TokUpdate ([lft], [rght]) -> reg ind lft; tokop lft^" = " ^ tokop rght ^ ixcomment ix
                                    | Update_listmemwr (mem, [adr], [data], [mask], [arg]) -> mem ^ " [ " ^ tokop adr ^ " ] = " ^ tokop data
                                    | oth -> othv := Some oth; failwith "update") ulst) ^ ";\nend\n"

let dump fd rtl =
  let ind = {mod'=ref "";ports=Hashtbl.create 255;wires=Hashtbl.create 255;regs=Hashtbl.create 255;cells=Hashtbl.create 255;conn=Hashtbl.create 255;proc=Hashtbl.create 255} in
  dbgrtl := rtl;
  output_string fd "// Generated by Source_text_main intended for yosys consumption only, use at your own risk\n";
  List.iter (fun itm -> ignore(dump_ver ind itm)) rtl;
  let ports = List.init (Hashtbl.length ind.ports) (fun ix -> Hashtbl.find ind.ports (ix+1)) in
  dbgports := ports;
  let buf = Buffer.create 10000 in
  Hashtbl.iter (fun k x -> Buffer.add_string buf (portmap ind x^"\n")) ind.proc;
  output_string fd ("module "^ !(ind.mod')^"("^String.concat ", " (List.map (portmap ind) ports)^");\n\n");
  Hashtbl.iter (fun k x -> output_string fd (portmap ind x^";\n")) ind.wires;
  Hashtbl.iter (fun k x -> output_string fd (portmap ind x^";\n")) ind.cells;
  Hashtbl.iter (fun x () -> output_string fd (portmap ind x^";\n")) ind.conn;
  output_string fd (Buffer.contents buf);
  output_string fd ("endmodule\n");
(*
  output_string fd "// copy of simlib.v from yosys follows\n\n";
  simlib fd;
*)
  ()

