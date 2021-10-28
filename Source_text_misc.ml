open Input_rewrite_types
open Source_text_misc_types
open Source_text_rewrite_types

let verbose = try int_of_string (Sys.getenv ("SAT_VERBOSE")) with _ -> 0

let id_ix = ref 1000
let newnam () = 
  incr id_ix;
  "$Id$"^string_of_int !id_ix

let trim s = if s.[0] = '\\' then String.sub s 1 (String.length s - 1) else s
let idx s i = E.INDEXED (trim s,i)
let scalar s = E.SCALAR (trim s)
let and2 a b = F.make_and [a;b]
let or2 a b = F.make_or [a;b]
let xor2 a b = F.make_xor a b
let knot a = F.make_not a
let mux2 a b s = or2 (and2 a (knot s)) (and2 b s)
let atom signal = F.make_atom ( E.make signal )
let string_of_signal = E.string_of_signal

let rec obin w n =
  (if w > 1 then obin (w-1) (n lsr 1) else "")^string_of_int (n land 1)

let rec obin64 w n =
  (if w > 1 then obin64 (w-1) (Int64.shift_right n 1) else "")^Int64.to_string (Int64.logand n 1L)

let str_to_bin s = let l = String.length s in
  (l*8), String.concat "" (List.init l (fun ix -> obin 8 (int_of_char s.[ix])))

let num_to_bin w n = Printf.sprintf "%d'%s" w (obin w n)
let int_to_bin = num_to_bin 32
let flt_to_bin f = "64'"^obin64 64 (Int64.bits_of_float f)

let othx = ref []
let oth' = ref None
let othn = ref None
let othexp = ref None
let othstr = ref None
let explst = ref []
let othxlst = ref []
let othopt = ref None
let othconn = ref None
let othh = ref F.f_false
let dumpitm = ref [E.GND,Msat_tseitin.MakeCNF.Lit (false, Made GND)]
let dumpitm' = ref [E.GND,Msat_tseitin.MakeCNF.Lit (false, Made GND)]

let (othlst:(string * E.signal * F.t option) list ref) = ref []

let cnv_pwr = function
  | "1'0" -> E.GND
  | "1'1" -> E.PWR
  | "1'x" -> E.GND
  | oth -> othstr := Some oth; failwith "cnv_pwr"

let stash' (ind:ind) = function
  | TokConn ([TokID pin], [Sigspec90 (signal, ix)]) -> pin, idx signal ix
  | TokConn ([TokID pin], [TokID signal]) -> pin, scalar signal
  | TokConn ([TokID pin], [TokVal lev]) -> pin, cnv_pwr lev
  | oth -> othconn := Some oth; failwith "stash'"

let dbgstash = ref []
let dbgfilt = ref []

let addff (ind:ind) signal = let op = atom signal in Hashtbl.replace ind.wires signal ( Some op )

let stash_ops = function TokConn ([TokID pin], _) -> (match trim pin with "Y" -> true | "Q" -> true | _ -> false) | _ -> false

let stash (ind:ind) kind inst conns =
  dbgstash := (kind, inst, conns) :: !dbgstash;
  let filt_ops = List.filter stash_ops conns in
  dbgfilt := filt_ops;
  if filt_ops <> [] then 
    begin
      let pin, net = stash' ind (List.hd filt_ops) in
      if trim pin = "Q" then addff ind net; (* prevent infinite recursion *)
      if Hashtbl.mem ind.stash net then failwith ("Multiple gates driving: "^E.string_of_signal net);
      Hashtbl.replace ind.stash net (kind,inst,conns)
    end
  else
    begin
      print_endline ("instance "^kind^" not translated")
    end

(* split a constant into individual bits *)

let explode_const = function
  | n::str::[] -> List.init (int_of_string n) (fun ix -> cnv_pwr ("1'"^String.make 1 str.[ix]))
  | oth -> othxlst := oth; failwith "explode_const"

let explode_const tok = explode_const (String.split_on_char '\'' tok)

let rec explode_lst (ind:ind) = function
  | [] -> []
  | Sigspecrange (lhs, hi, lo) :: tl -> List.init (hi-lo+1) (fun ix -> idx lhs (hi-ix)) @ explode_lst ind tl
  | Sigspec90 (lhs, ix) :: tl -> idx lhs ix :: explode_lst ind tl
  | TokVal tok :: tl -> explode_const tok @ explode_lst ind tl
  | TokID _ as id :: tl -> explode_signal ind id @ explode_lst ind tl
  | TokInt 1 :: tl -> E.PWR :: explode_lst ind tl
  | oth -> othx := oth; failwith "explode_lst"

and explode_signal (ind:ind) = function
  | TokID id ->
      let id' = trim id in
      let found = Hashtbl.find_opt ind.wid id' in
      explst := (id',found) :: !explst;
      (match found with None -> scalar id :: [] | Some n -> List.init n (fun i -> idx id (n-1-i)))
  | (Sigspec90 _ | Sigspecrange _ | TokVal _ | TokInt _) as x -> explode_lst ind (x :: [])
  | Sigspec92 conc -> explode_lst ind conc
  | oth -> oth' := Some oth; failwith "explode_signal"

let addwire ind signal =
  match Hashtbl.find_opt ind.wires signal with
    | Some x -> print_endline (E.string_of_signal signal^" redeclared")
    | None -> Hashtbl.add ind.wires signal None

let addfunc ind signal func =
  match Hashtbl.find_opt ind.wires signal with
    | Some None ->
        if verbose > 0 then print_endline ("Installed function for wire: "^E.string_of_signal signal);
        Hashtbl.replace ind.wires signal (Some func)
    | Some _ -> print_endline (E.string_of_signal signal ^" redeclared")
    | None -> print_endline (E.string_of_signal signal ^" undefined")

let addnxt' pat = function
| E.PWR -> E.SCALAR (pat^"$PWR")
| GND -> E.SCALAR (pat^"$GND")
| SCALAR string -> E.SCALAR (pat^"$"^string)
| INDEXED (string, int) -> E.INDEXED (pat^"$"^string, int)

let addnxt pat ind d q =
   let lhs' = addnxt' pat q in
   addwire ind lhs';
   Hashtbl.add ind.inffop lhs' ();
   addfunc ind lhs' d

(* dump a cnf in ASCII *)
let rec mypp fmt phi =
    match phi with
    | Msat_tseitin.MakeCNF.True -> Format.fprintf fmt "true"
    | Lit a -> E.pp fmt (E.opaque a)
    | Comb (Not, [f]) ->
      Format.fprintf fmt "not (%a)" mypp f
    | Comb (And, l) -> Format.fprintf fmt "(%a)" (mypp_list "and") l
    | Comb (Or, l) ->  Format.fprintf fmt "(%a)" (mypp_list "or") l
    | Comb (Imp, [f1; f2]) ->
      Format.fprintf fmt "(%a => %a)" mypp f1 mypp f2
    | _ -> assert false
  and mypp_list sep fmt = function
    | [] -> ()
    | [f] -> mypp fmt f
    | f::l -> Format.fprintf fmt "%a %s %a" mypp f sep (mypp_list sep) l

let fpp q =
  let buf' = Buffer.create 1000 in
  let buf = Format.formatter_of_buffer buf' in
  if verbose > 0 then mypp buf q;
  Format.pp_print_flush buf ();
  Buffer.contents buf'

let notsupp kind lst = failwith ("Not supported: "^kind)

let cnv_sig = function
  | E.GND -> TokVal "1'0"
  | PWR -> TokVal "1'1"
  | SCALAR str -> TokID str
  | INDEXED (signal, ix) -> Sigspec90 (signal, ix)

let getw (ind:ind) = function
  | E.GND -> Some F.f_false
  | PWR -> Some F.f_true
  | signal -> match Hashtbl.find_opt ind.wires signal with
    | Some x -> x
    | None -> failwith (E.string_of_signal signal^" not declared")

let addinp idx (ind:ind) signal =
  Hashtbl.add ind.inffop signal ();
  match Hashtbl.find_opt ind.wires signal with
    | Some x -> print_endline (E.string_of_signal signal^" redeclared")
    | None -> Hashtbl.add ind.wires signal ( Some ( atom signal ) )

let addoutp idx (ind:ind) signal =
  Hashtbl.add ind.inffop signal ();
  match Hashtbl.find_opt ind.wires signal with
    | Some x -> print_endline (E.string_of_signal signal^" redeclared")
    | None -> Hashtbl.add ind.wires signal None

let rec cnv_ilang (ind:ind) = function
| Autoidx_stmt26(int') -> ()
| Attr_stmt(string,ilang_lst') -> ()
| Module12(string,ilang_lst') -> print_endline string; List.iter (cnv_ilang ind) ilang_lst'
| Wire_stmt(options,string') -> let wid = ref None and fn = ref addwire in List.iter (function
    | Wire_optionswidth n -> wid := Some n; Hashtbl.replace ind.wid (trim string') n
    | Wire_optionsinput n -> fn := addinp n;
    | Wire_optionsinout n -> fn := addoutp n;
    | Wire_optionsoutput n -> fn := addoutp n;
    | Signed -> ()
    | oth -> othopt := Some oth; failwith "options") options;
    (match !wid with
      | None -> !fn ind (scalar string')
      | Some n -> for i = 0 to n-1 do !fn ind (idx string' i) done); ()
| Cell_stmt(kind,inst,params,conns) -> stash ind kind inst conns
| Conn_stmt96 ([lhs], [TokInt n]) -> () (* placeholder *)
| Conn_stmt96 ([(Sigspec92 _ | TokID _ | Sigspec90 _ | Sigspecrange _) as conc1], [(Sigspec92 _ | TokID _ | Sigspec90 _ | TokVal _ | Sigspecrange _) as conc2]) ->
  let lhs = explode_signal ind conc1 in
  let rhs = explode_signal ind conc2 in
  if List.length lhs <> List.length rhs then
    begin
      let widlst = ref [] in
      Hashtbl.iter (fun k x -> widlst := (k,x) :: !widlst) ind.wid;
      othexp := Some (conc1,conc2,lhs,rhs,!widlst);
      failwith "conn_stmt_fail"
    end;
  List.iter2 (fun lhs' rhs' -> stash ind "$_BUF_" ("$B"^string_of_int (Hashtbl.length ind.stash)) [ TokConn ([TokID "\\A"], [cnv_sig rhs']) ; TokConn ([TokID "\\Y"], [cnv_sig lhs']) ]) lhs rhs
| Param_defval_stmt24(string,ilang_lst') -> () (* does not seem to be used yet *)
| Param_stmt23(string,ilang_lst') -> () (* placeholder *)
| oth -> oth' := Some oth; failwith "cnv_ilang"

let rec conn_n ind lhs' = function
| (Id _ | IdArrayed2 _) as rhs' -> monadic "$_BUF_" ind lhs' rhs'
| And (expr1, expr2) -> dyadic "$_AND_" ind lhs' expr1 expr2
| Or (expr1, expr2) -> dyadic "$_OR_" ind lhs' expr1 expr2
| Xor (expr1, expr2) -> dyadic "$_XOR_" ind lhs' expr1 expr2
| Tilde expr -> monadic "$_NOT_" ind lhs' expr
| Expression expr -> conn_n ind lhs' expr
| Query (cond, expr1, expr2) -> ternary "$_MUX_" ind lhs' expr2 expr1 cond
| oth -> othn := Some oth; failwith "conn_netlist"

and monadic kind ind lhs expr =
  stash ind kind ("$I"^string_of_int (Hashtbl.length ind.stash)) ( TokConn ([TokID "\\A"], [e ind expr]) :: TokConn ([TokID "\\Y"], [lhs])  :: [])

and dyadic kind ind lhs expr1 expr2 =
  stash ind kind ("$I"^string_of_int (Hashtbl.length ind.stash)) ( TokConn ([TokID "\\A"], [e ind expr1]) :: TokConn ([TokID "\\B"], [e ind expr2]) :: TokConn ([TokID "\\Y"], [lhs]) :: [])

and ternary kind ind lhs expr1 expr2 cond =
  let pins =  TokConn ([TokID "\\A"], [e ind expr1]) ::
              TokConn ([TokID "\\B"], [e ind expr2]) ::
              TokConn ([TokID "\\S"], [e ind cond]) ::
              TokConn ([TokID "\\Y"], [lhs]) :: [] in
  stash ind kind ("$I"^string_of_int (Hashtbl.length ind.stash)) pins

and ff kind ind lhs data clk rst =
  let pins =  TokConn ([TokID "\\D"], [e ind data]) ::
              TokConn ([TokID "\\C"], [e ind clk]) ::
              TokConn ([TokID "\\R"], [e ind rst]) ::
              TokConn ([TokID "\\Q"], [lhs]) :: [] in
  stash ind kind ("$I"^string_of_int (Hashtbl.length ind.stash)) pins

and enff kind ind lhs data clk rst en =
  let pins =  TokConn ([TokID "\\D"], [e ind data]) ::
              TokConn ([TokID "\\C"], [e ind clk]) ::
              TokConn ([TokID "\\R"], [e ind rst]) ::
              TokConn ([TokID "\\E"], [e ind en]) ::
              TokConn ([TokID "\\Q"], [lhs]) :: [] in
  stash ind kind ("$I"^string_of_int (Hashtbl.length ind.stash)) pins

and e ind = function
| IdArrayed2 (Id id, Number (_,_,ix,_)) -> Sigspec90 (id,ix)
| Id id -> TokID id
| expr ->
  let expr' = newnam() in
  addwire ind (scalar expr');
  conn_n ind (TokID expr') expr;
  TokID expr'

let rstval = function
| None, Number (_, 1, 0, _) -> "$_DFF_PP0_"
| None, Number (_, 1, 1, _) -> "$_DFF_PP1_"
| Some (Id _), Number (_, 1, 0, _) -> "$_DFFE_PP0P_"
| Some (Id _), Number (_, 1, 1, _) -> "$_DFFE_PP1P_"
| _, oth -> othn := Some oth; failwith "rstval"

let rec cnv_netlist (ind:ind) = function
| Modul(string,parms,ports,itms) ->
  print_endline string;
  List.iteri (fun n -> function
        | Port (In, string', [], Deflt) ->
          addinp n ind (scalar string')
        | Port ((Inout|Out), string', [], Deflt) ->
          addoutp n ind (scalar string')
        | Port ((Inout|Out), string', [AnyRange (Number(_,_,hi,_),Number(_,_,lo,_))], Deflt) ->
          for i = lo to hi do addoutp n ind (idx string' i); done;
        | Port (In, string', [AnyRange (Number(_,_,hi,_),Number(_,_,lo,_))], Deflt) ->
          for i = lo to hi do addoutp n ind (idx string' i) done
        | oth -> othn := Some oth; failwith "cnv_netlist ports") ports;
  List.iter (cnv_netlist ind) itms
| NetDecl ([Atom "wire"], [Id id]) -> addwire ind (scalar id)
| NetDecl ([Atom "wire"; AnyRange (Number(_,_,hi,_),Number(_,_,lo,_))], [Id id]) ->
  for i = lo to hi do addwire ind (idx id i) done
| DeclReg ([Id id], [AnyRange (Number (_, _, hi, _), Number (_, _, lo, ""))], Deflt) ->
  for i = lo to hi do let signal = idx id i in if not (Hashtbl.mem ind.wires signal) then addwire ind signal done
| ContAsgn([Asgn1 (IdArrayed2 (Id lhs, Number(_,_,ix,_)), rhs)]) -> conn_n ind (Sigspec90(lhs,ix)) rhs
| ContAsgn([Asgn1 (Id lhs, rhs)]) -> conn_n ind (TokID lhs) rhs
| AlwaysLegacy (At (EventOr [Pos (Id _ as clk)]),
     If2 (rst,
      EquateSelect (Id lhs, Number(_,_,ix,_), rexpr),
      EquateSelect (Id lhs', Number (_, _, ix', _),
       expr))) when lhs=lhs' && ix=ix' ->
        let kind = rstval (None,rexpr) in
        ff kind ind (Sigspec90(lhs,ix)) expr clk rst
| AlwaysLegacy (At (EventOr [Pos (Id _ as clk)]),
     If2 (rst,
      EquateSelect (Id lhs, Number(_,_,ix,_), rexpr),
      If1 (en,
       EquateSelect (Id lhs', Number(_,_,ix',_), expr)))) when lhs=lhs' && ix=ix' ->
        let kind = rstval (Some en,rexpr) in 
        enff kind ind (Sigspec90(lhs,ix)) expr clk rst en
| oth -> othn := Some oth; failwith "cnv_netlist"

let cnv_satv' cnv' arg =
  List.map (fun (nam,itm) ->
      let wh = Hashtbl.create 255 in
      let wid = Hashtbl.create 255 in
      let ffh = Hashtbl.create 255 in
      let sh = Hashtbl.create 255 in
      let ind = {wires=wh;inffop=ffh;stash=sh;wid=wid} in
      if verbose > 1 then print_endline ("Converting: "^nam);
      List.iter (cnv_netlist ind) itm;
      let loopchk = ref [] in
      Hashtbl.iter (fun _ (kind,inst,conns) -> cnv' ind loopchk [inst] kind conns) sh;
      let hlst=ref [] in
      Hashtbl.iter (fun k -> function
          | Some x -> othh := x; hlst := (k, fpp x) :: !hlst
          | None -> if verbose > 0 then print_endline (nam^": "^E.string_of_signal k^" is not used")) wh;
      let inffoplst=ref [] in
      Hashtbl.iter (fun k () -> inffoplst := (k, match Hashtbl.find wh k with
        | Some x -> x
        | None -> print_endline ("ffh: " ^ E.string_of_signal k^" is undefined"); atom (scalar "\\")) :: !inffoplst) ffh;
      let widlst=ref [] in
      Hashtbl.iter (fun k n -> widlst := (k, n) :: !widlst) wid;
      if verbose > 1 then print_endline ("inffopslt length: "^string_of_int (List.length !inffoplst));
      !hlst, List.sort compare !inffoplst, !widlst
  ) arg

let cnv_sat' cnv' arg =
  List.map (fun (nam,itm) ->
      let wh = Hashtbl.create 255 in
      let wid = Hashtbl.create 255 in
      let ffh = Hashtbl.create 255 in
      let sh = Hashtbl.create 255 in
      let ind = {wires=wh;inffop=ffh;stash=sh;wid=wid} in
      if verbose > 1 then print_endline ("Converting: "^nam);
      List.iter (cnv_ilang ind) itm;
      let loopchk = ref [] in
      Hashtbl.iter (fun _ (kind,inst,conns) -> cnv' ind loopchk [inst] kind conns) sh;
      let hlst=ref [] in
      Hashtbl.iter (fun k -> function
          | Some x -> othh := x; hlst := (k, fpp x) :: !hlst
          | None -> if verbose > 0 then print_endline (nam^": "^E.string_of_signal k^" is not used")) wh;
      let inffoplst=ref [] in
      Hashtbl.iter (fun k () -> inffoplst := (k, match Hashtbl.find wh k with
        | Some x -> x
        | None -> print_endline ("ffh: " ^ E.string_of_signal k^" is undefined"); atom (scalar "\\")) :: !inffoplst) ffh;
      let widlst=ref [] in
      Hashtbl.iter (fun k n -> widlst := (k, n) :: !widlst) wid;
      if verbose > 1 then print_endline ("inffopslt length: "^string_of_int (List.length !inffoplst));
      !hlst, List.sort compare !inffoplst, !widlst
  ) arg

let cnv_sat cnv' arg' =
  if verbose > 1 then print_endline ("Reading rtlil: "^arg');
  let _,arg = Input_rewrite.parse arg' in
  cnv_sat' cnv' arg


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

let mycnf' = ref F.f_false
let mycnf = ref [[E.transparent (E.fresh ())]]

let ep k' form =
    if verbose > 2 then print_endline "Dumping cnf";
    mycnf' := form;
    if verbose > 2 then print_endline "Building cnf";
    let m = F.make_cnf form in
    mycnf := List.map (List.map E.transparent) m;
    let solver = Msat_sat_slit.create () in
    Msat_sat_slit.assume solver m ();
    match Msat_sat_slit.solve solver with
      | Msat_sat_slit.Sat _ -> if verbose > 1 then print_endline ("SATISFIABLE (endpoint "^k'^" mismatched)"); false
      | Msat_sat_slit.Unsat _ -> if verbose > 1 then print_endline ("UNSATISFIABLE (endpoint "^k'^" matched)"); true
