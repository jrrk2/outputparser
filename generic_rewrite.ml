open Source_text_rewrite_types

let verbose = try int_of_string (Sys.getenv ("DESCEND_VERBOSE")) > 0 with _ -> false

let trim s = if s.[0] = '\\' then String.sub s 1 (String.length s - 1) else s
let idx s i = E.INDEXED (trim s,i)
let scalar s = E.SCALAR (trim s)
let and2 a b = F.make_and [a;b]
let or2 a b = F.make_or [a;b]
let xor2 a b = F.make_xor a b
let knot a = F.make_not a
let mux2 a b s = or2 (and2 a (knot s)) (and2 b s)
let atom signal = F.make_atom ( E.make signal )

let othstr = ref None
let (othlst:(string * E.signal * F.t option) list ref) = ref []

let cnv_pwr = function
  | "1'0" -> E.GND
  | "1'1" -> E.PWR
  | "1'x" -> E.GND
  | oth -> othstr := Some oth; failwith "cnv_pwr"

let addwire idx' ind signal =
  match List.assoc_opt signal !(ind.wires) with
    | Some x -> print_endline (E.string_of_signal signal^" redeclared")
    | None -> ind.wires := (signal, None) :: !(ind.wires)

let addfunc ind signal func =
  match List.assoc_opt signal !(ind.wires) with
    | Some None ->
        if verbose then print_endline ("Installed function for wire: "^E.string_of_signal signal);
        ind.wires := (signal, Some func) :: !(ind.wires)
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
   ind.inffop := (lhs', ()) :: !(ind.inffop);
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

let getstr x = failwith "getstr"

let rec rw x = failwith ("rw fail")

(*
(* for plain Verilog reading without pre-proc only *)

let parse_output_ast_from_chan ch =
  let lb = Lexing.from_channel ch in
  let output = try
      ml_start token lb
  with
    | Parsing.Parse_error ->
      let n = Lexing.lexeme_start lb in
      failwith (Printf.sprintf "Output.parse: parse error at character %d" n);
  in
  output

(* for preprocessed input *)

let parse_output_ast_from_function f =
  let lb = Lexing.from_function f in
  let output = try
      ml_start token lb
  with
    | Parsing.Parse_error ->
      let n = Lexing.lexeme_start lb in
      failwith (Printf.sprintf "Output.parse: parse error at character %d" n);
  in
  output

(* for Verilator pre-processed input *)

let parse_output_ast_from_pipe v =
  let ch = Unix.open_process_in ("verilator -E "^v) in
  let lb = Lexing.from_channel ch in
  let output = try
      ml_start token lb
  with
    | Parsing.Parse_error ->
      let n = Lexing.lexeme_start lb in
      failwith (Printf.sprintf "Output.parse: parse error at character %d" n);
  in
  output

(* for plain Verilog reading without pre-proc only *)

let parse_output_ast_from_string s =
  let lb = Lexing.from_string s in
  let output = try
      ml_start token lb
  with
    | Parsing.Parse_error ->
      let n = Lexing.lexeme_start lb in
      failwith (Printf.sprintf "Output.parse: parse error at character %d" n);
  in
  output
*)
