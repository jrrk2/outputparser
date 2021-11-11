open Moore
open Moore_lex
open Moore_dump

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

let p' = ref []

let parse pipe fil =
  let ch = if pipe then Unix.open_process_in ("../moore/target/debug/moore --dump-ast "^fil) else open_in fil in
  let p = parse_output_ast_from_chan ch in
  close_in ch;
  match p with TLIST lst -> List.filter (function TLIST [] -> false | _ -> true) lst | oth -> [oth]

let dmp pipe ast =
  let p = parse pipe ast in
  p' := p;
  let d = dump p in
  p,d 


