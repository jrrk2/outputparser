open Struct
open Struct_lex
open Struct_dump

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

let parse arg =
  let fil = "../moore/src/svlog/syntax/"^arg^".rs" in
  let ch = open_in fil in
  let p = parse_output_ast_from_chan ch in
  close_in ch;
  match p with TLIST lst -> List.filter (function TLIST [] -> false | _ -> true) lst | oth -> [oth]

let dmp ast =
  let p = parse ast in
  p' := p;
  dump ast p;
  p
