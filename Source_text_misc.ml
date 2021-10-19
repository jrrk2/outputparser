open Input_rewrite_types

module E = Msat_sat_slit.String_lit
module F = Msat_tseitin.MakeCNF

type ind = {
  wires:(E.signal, F.t option) Hashtbl.t;
  inffop:(E.signal, unit) Hashtbl.t;
  stash:(E.signal, string * string * Input_rewrite_types.ilang list) Hashtbl.t;
  wid:(string, int) Hashtbl.t;
}

let verbose = try int_of_string (Sys.getenv ("SAT_VERBOSE")) > 0 with _ -> false

let trim s = if s.[0] = '\\' then String.sub s 1 (String.length s - 1) else s
let idx s i = E.INDEXED (trim s,i)
let scalar s = E.SCALAR (trim s)
let and2 a b = F.make_and [a;b]
let or2 a b = F.make_or [a;b]
let xor2 a b = F.make_xor a b
let knot a = F.make_not a
let mux2 a b s = or2 (and2 a (knot s)) (and2 b s)
let atom signal = F.make_atom ( E.make signal )

let rec obin w n = 
  (if w > 1 then obin (w-1) (n lsr 1) else "")^string_of_int (n land 1)

let clog2 n = if n = 0 then 0 else int_of_float(ceil(log(float_of_int n)/.log 2.))

let rec obin64 w n = 
  (if w > 1 then obin64 (w-1) (Int64.shift_right n 1) else "")^Int64.to_string (Int64.logand n 1L)

let str_to_bin s = let l = String.length s in 
  (l*8), String.concat "" (List.init l (fun ix -> obin 8 (int_of_char s.[ix])))

let num_to_bin w n = Printf.sprintf "%d'%s" w (obin w n)
let int_to_bin = num_to_bin 32
let flt_to_bin f = "64'"^obin64 64 (Int64.bits_of_float f)

let othstr = ref None
let othxlst = ref []
let (othlst:(string * E.signal * F.t option) list ref) = ref []

let cnv_pwr = function
  | "1'0" -> E.GND
  | "1'1" -> E.PWR
  | "1'x" -> E.GND
  | oth -> othstr := Some oth; failwith "cnv_pwr"

(* split a constant into individual bits *)

let explode_const = function
  | n::str::[] -> List.init (int_of_string n) (fun ix -> cnv_pwr ("1'"^String.make 1 str.[ix]))
  | oth -> othxlst := oth; failwith "explode_const"

let explode_const tok = explode_const (String.split_on_char '\'' tok)

let addwire idx' ind signal =
  match Hashtbl.find_opt ind.wires signal with
    | Some x -> print_endline (E.string_of_signal signal^" redeclared")
    | None -> Hashtbl.add ind.wires signal None

let addfunc ind signal func =
  match Hashtbl.find_opt ind.wires signal with
    | Some None ->
        if verbose then print_endline ("Installed function for wire: "^E.string_of_signal signal);
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
   addwire 0 ind lhs';
   Hashtbl.add ind.inffop lhs' ();
   addfunc ind lhs' d

(* dump a cnf in ASCII *)

let fpp q =
  let buf' = Buffer.create 1000 in
  let buf = Format.formatter_of_buffer buf' in
(*
  if verbose then F.mypp buf q;
*)
  Format.pp_print_flush buf ();
  Buffer.contents buf'

let notsupp kind lst = failwith ("Not supported: "^kind)
