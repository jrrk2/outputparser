
open Translation_unit_list
open Translation_unit_list_types
open Translation_unit_list_lex

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

let parse arg =
  Hashtbl.clear typehash;
  Hashtbl.add typehash "__builtin_va_list" ();
  let ch = open_in arg in
  let rslt = parse_output_ast_from_chan ch in
  close_in ch;
  rslt

let rec tolst = function
| CONS2(a,b) -> tlst b :: tolst a
| CONS3(a,COMMA,b) -> tlst b :: tolst a
| CONS4(a,COMMA,b,c) -> TUPLE2(tlst b, tlst c) :: tolst a
| TUPLE2(a,b) -> [TUPLE2(tlst a, tlst b)]
| TUPLE3(a,b,c) -> [TUPLE3(tlst a, tlst b, tlst c)]
| TUPLE4(a,b,c,d) -> [TUPLE4(tlst a, tlst b, tlst c, tlst d)]
| TUPLE5(a,b,c,d,e) -> [TUPLE5(tlst a, tlst b, tlst c, tlst d, tlst e)]
| oth -> [oth]

and tlst lst = match tolst lst with
| [] -> EMPTY_TOKEN
| hd :: [] -> hd
| hd :: tl -> TLIST (List.rev (tlst hd::List.flatten (List.map tolst tl)))

let errlst = ref []
let fns = Hashtbl.create 257
let enums = Hashtbl.create 257
let externs = Hashtbl.create 257
let structs = Hashtbl.create 257
let unions = Hashtbl.create 257
let ftypes = Hashtbl.create 257
let typedefs = Hashtbl.create 257
let inlines = Hashtbl.create 257
let globals = Hashtbl.create 257
let inits = Hashtbl.create 257

let rec dumptree = function
| CONS2(a,b) -> "CONS2 ("^dumptree a^", "^dumptree b^")"
| CONS3(a,b,c) -> "CONS3 ("^dumptree a^", "^dumptree b^", "^dumptree c^")"
| CONS4(a,b,c,d) -> "CONS4 ("^dumptree a^", "^dumptree b^", "^dumptree c^")"
| TUPLE2(a,b) -> "TUPLE2 ("^dumptree a^", "^dumptree b^")"
| TUPLE3(a,b,c) -> "TUPLE3 ("^dumptree a^", "^dumptree b^", "^dumptree c^")"
| TUPLE4(a,b,c,d) -> "TUPLE4 ("^dumptree a^", "^dumptree b^", "^dumptree c^", "^dumptree d^")"
| TYPE_NAME str -> "TYPE_NAME \""^str^"\""
| IDENTIFIER str -> "IDENTIFIER \""^str^"\""
| CONSTANT num -> "IDENTIFIER \""^num^"\""
| TLIST lst -> "TLIST ["^String.concat "; " (List.map dumptree lst)^"]"
| oth -> getstr oth

let failtree oth = failwith (dumptree oth)

let filt rslt = List.iter (function
| TUPLE3
  (typ,
   TUPLE4
    (IDENTIFIER fn, LPAREN,
     params,
     RPAREN),
   SEMICOLON) -> Hashtbl.add fns fn (typ,params)
| TUPLE3
  (TUPLE2 (STATIC, TUPLE2 (INLINE, typ)),
   TUPLE4
    (IDENTIFIER fn, LPAREN,
     params,
     RPAREN),
   TUPLE3
    (LBRACE, body, RBRACE)) -> Hashtbl.add fns fn (typ,params)
| TUPLE3
  (TUPLE2 (EXTERN, typ),
    TUPLE2(STAR, 
     TUPLE4
      (IDENTIFIER fn, LPAREN,
       params,
     RPAREN)),
   SEMICOLON) -> Hashtbl.add fns fn (typ,params)
| TUPLE3
  (TUPLE2 (EXTERN, typ),
   (TUPLE2
    (TUPLE2 (STAR, CONST), TUPLE3 (IDENTIFIER fn, LBRACK, RBRACK)) as params),
   SEMICOLON) -> Hashtbl.add fns fn (typ,params)
| TUPLE2
  (TUPLE5
    (STRUCT, IDENTIFIER sid, LBRACE,
     params,
     RBRACE),
   SEMICOLON) -> Hashtbl.add structs sid (params)
| TUPLE2
  (TUPLE4
    (ENUM, LBRACE,
     enumerations,
     RBRACE),
   SEMICOLON) -> Hashtbl.add enums "__anon__" (enumerations)
| TUPLE3
  (TUPLE2
    (TYPEDEF,
     TUPLE4
      (ENUM, LBRACE,
       enumerations,
       RBRACE)),
   IDENTIFIER id_t, SEMICOLON) -> Hashtbl.add enums id_t (enumerations)
| TUPLE3
  (TUPLE2 (TYPEDEF, typ),
   TUPLE4
    (TUPLE3 (LPAREN, TUPLE2 (STAR, IDENTIFIER fn_t), RPAREN),
     LPAREN,
     params,
     RPAREN),
   SEMICOLON) -> Hashtbl.add ftypes fn_t (typ,params)
| TUPLE2
  (TUPLE5
    (ENUM, IDENTIFIER enum_id, LBRACE,
     enumerations,
     RBRACE),
   SEMICOLON) -> Hashtbl.add enums enum_id (enumerations)
| TUPLE3
  (TUPLE2
    (TYPEDEF,
     TUPLE5
      (STRUCT, IDENTIFIER struct_id, LBRACE,
       items,
       RBRACE)),
   TUPLE2 (STAR, IDENTIFIER id_t), SEMICOLON) -> Hashtbl.add structs struct_id (items)
| TUPLE2 (TUPLE2 (STRUCT, IDENTIFIER struct_id), SEMICOLON) -> Hashtbl.add structs struct_id (EMPTY_TOKEN)
| TUPLE3
  (TUPLE2 (EXTERN, TUPLE2 (STRUCT, (IDENTIFIER struct_id|TYPE_NAME struct_id))),
   (TUPLE2 (STAR, IDENTIFIER nam) as item), SEMICOLON) -> Hashtbl.add structs struct_id (item)
| TUPLE3
  (TUPLE2 (TYPEDEF, typedef),
   IDENTIFIER id_t, SEMICOLON) -> Hashtbl.add typedefs id_t typedef
| TUPLE3
  (TUPLE2 (TYPEDEF, typedef),
   TUPLE2 (STAR, IDENTIFIER id_t), SEMICOLON) -> Hashtbl.add typedefs id_t typedef
| TUPLE3 (TUPLE2 (EXTERN, typ), IDENTIFIER nam, SEMICOLON) -> Hashtbl.add externs nam typ
| TUPLE3 (TUPLE2 (EXTERN, typ), TUPLE2(STAR, IDENTIFIER nam), SEMICOLON) -> Hashtbl.add externs nam typ
| TUPLE3 (TUPLE2 (EXTERN, typ), TLIST tlst, SEMICOLON) -> List.iter (function
    | TUPLE2(STAR, IDENTIFIER id) -> Hashtbl.add externs id typ
    | oth -> failtree oth) tlst
| TUPLE2
  (TUPLE5 (UNION, IDENTIFIER uid, LBRACE, TLIST ulst, RBRACE), SEMICOLON) -> Hashtbl.add unions uid ulst
| TUPLE3
  (typ,
   TUPLE4
    (IDENTIFIER fn, LPAREN,
     params,
     RPAREN),
   TUPLE3
    (LBRACE, body, RBRACE)) -> Hashtbl.add fns fn (typ,params)
| TUPLE3(typ,
   TUPLE2(STAR,
     TUPLE4
      (IDENTIFIER fn, LPAREN,
       params,
       RPAREN)),
   SEMICOLON) -> Hashtbl.add fns fn (typ,params)
| TUPLE3 (TUPLE2 (INLINE, TUPLE2 (STATIC, typ)),
    TUPLE2 (STAR, TUPLE4 (IDENTIFIER fn, LPAREN, VOID, _)),
    TUPLE3 (LBRACE, body, RBRACE)) -> Hashtbl.add inlines fn (typ,body)
| TUPLE3 (TYPE_NAME id_t as t, IDENTIFIER data, SEMICOLON) -> Hashtbl.add globals data t
| TUPLE3 (TUPLE2 (STATIC, TUPLE2 (CONST, typ)), TUPLE3 (IDENTIFIER data, EQUALS, CONSTANT num), SEMICOLON) ->
    Hashtbl.add inits data (typ,num)
| TUPLE3 (TUPLE2 (CONST, typ), TUPLE2 (STAR, TUPLE4 (IDENTIFIER fn, LPAREN, VOID, RPAREN)),
    TUPLE3 (LBRACE, body, RBRACE)) -> Hashtbl.add inlines fn (typ,body)
| oth -> errlst := oth :: !errlst; failtree oth) rslt

let getrslt arg =
   Printf.fprintf stderr "%s: " arg; flush stderr;
   match parse arg with
    | TUPLE2(tran,_) -> 
        filt (List.rev (tolst tran));
        let typlst = ref [] in
	Hashtbl.iter (fun k _ -> typlst := k :: !typlst) typehash;
	let fnlst = ref [] in
	Hashtbl.iter (fun k _ -> fnlst := k :: !fnlst) fns;
	let extlst = ref [] in
	Hashtbl.iter (fun k _ -> extlst := k :: !extlst) externs;
	let enumlst = ref [] in
	Hashtbl.iter (fun k _ -> enumlst := k :: !enumlst) enums;
	let structlst = ref [] in
	Hashtbl.iter (fun k _ -> structlst := k :: !structlst) structs;
	let unionlst = ref [] in
	Hashtbl.iter (fun k _ -> unionlst := k :: !unionlst) unions;
	let ftyplst = ref [] in
	Hashtbl.iter (fun k _ -> ftyplst := k :: !ftyplst) ftypes;
	let typlst = ref [] in
	Hashtbl.iter (fun k _ -> typlst := k :: !typlst) typedefs;
        Printf.fprintf stderr
"Types=%d, Functions=%d, externs=%d, enums=%d, structs=%d, unions=%d, ftyp=%d, types=%d\n"
(List.length !typlst)
(List.length !fnlst)
(List.length !extlst)
(List.length !enumlst)
(List.length !structlst)
(List.length !unionlst)
(List.length !ftyplst)
(List.length !typlst)
    | oth -> failtree oth

let _ = for i = 1 to Array.length Sys.argv - 1 do getrslt Sys.argv.(i) done

