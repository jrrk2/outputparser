
open Source_text_edited
open Source_text_lexer

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

let parse_raw arg =
  let ch = open_in arg in
  let rslt = parse_output_ast_from_chan ch in
  close_in ch;
  rslt

let rec rewrite = function
| TUPLE3(a, COMMA, b) -> (match rewrite a, rewrite b with
				| TLIST a', TLIST b' -> TLIST (a' @ b')
                                | TLIST a', b' -> TLIST (a' @ [b'])
				| a', TLIST b' -> TLIST (a' :: b')
                                | a', b' -> TLIST (a' :: b' :: []))
| TUPLE2(a,b) -> TUPLE2(rewrite a, rewrite b)
| TUPLE3(a,b,c) -> TUPLE3(rewrite a, rewrite b, rewrite c)
| TUPLE4(a,b,c,d) -> TUPLE4(rewrite a, rewrite b, rewrite c, rewrite d)
| TUPLE5(a,b,c,d,e) -> TUPLE5(rewrite a, rewrite b, rewrite c, rewrite d, rewrite e)
| TUPLE6(a,b,c,d,e,f) -> TUPLE6(rewrite a, rewrite b, rewrite c, rewrite d, rewrite e, rewrite f)
| TUPLE7(a,b,c,d,e,f,g) -> TUPLE7(rewrite a, rewrite b, rewrite c, rewrite d, rewrite e, rewrite f, rewrite g)
| TUPLE8(a,b,c,d,e,f,g,h) -> TUPLE8(rewrite a, rewrite b, rewrite c, rewrite d, rewrite e, rewrite f, rewrite g, rewrite h)
| TUPLE9(a,b,c,d,e,f,g,h,i) -> TUPLE9(rewrite a, rewrite b, rewrite c, rewrite d, rewrite e, rewrite f, rewrite g, rewrite h, rewrite i)
| TUPLE10(a,b,c,d,e,f,g,h,i,j) -> TUPLE10(rewrite a, rewrite b, rewrite c, rewrite d, rewrite e, rewrite f, rewrite g, rewrite h, rewrite i, rewrite j)

| oth -> oth
				
let parse f = rewrite (parse_raw f)
  
let _ = if Array.length Sys.argv > 1 then parse Sys.argv.(1) else ACCEPT
