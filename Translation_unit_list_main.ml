
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

let rec findlst2 = function
| CONS1 a -> a :: []
| CONS2(a,b) -> b :: findlst2 a
| oth -> oth :: []

let rec findlst3 = function
| CONS1 a -> a :: []
| CONS3(a,COMMA,b) -> b :: findlst3 a
| oth -> oth :: []

let rec findlst4 = function
| CONS1 a -> a :: []
| CONS4(a,COMMA,b,c) -> TUPLE2(b,c) :: findlst4 a
| oth -> oth :: []

let rec tolst = function
| CONS1(a) -> tolst a
| CONS2(a,b) -> TLIST (List.rev_map tolst (b :: findlst2 a))
| CONS3(a,COMMA,b) -> TLIST (List.rev_map tolst (tolst b :: findlst3 a))
| CONS4(a,COMMA,b,c) -> TLIST (List.rev_map tolst (TUPLE2(b, c) :: findlst4 a))
| TUPLE2(a,b) -> TUPLE2(tolst a, tolst b)
| TUPLE3(a,b,c) -> TUPLE3(tolst a, tolst b, tolst c)
| TUPLE4(a,b,c,d) -> TUPLE4(tolst a, tolst b, tolst c, tolst d)
| TUPLE5(a,b,c,d,e) -> TUPLE5(tolst a, tolst b, tolst c, tolst d, tolst e)
| TUPLE6(a,b,c,d,e,f) -> TUPLE6(tolst a, tolst b, tolst c, tolst d, tolst e, tolst f)
| TUPLE7(a,b,c,d,e,f,g) -> TUPLE7(tolst a, tolst b, tolst c, tolst d, tolst e, tolst f, tolst g)
| oth -> oth

let rec search fn = function
| CONS1(a) -> search fn a
| CONS2(a,b) -> (search fn) a || (search fn) b
| CONS3(a,b,c) -> (search fn) a || (search fn) b || (search fn) c
| CONS4(a,b,c,d) -> (search fn) a || (search fn) b || (search fn) c || (search fn) d
| TUPLE2(a,b) -> (search fn) a || (search fn) b
| TUPLE3(a,b,c) -> (search fn) a || (search fn) b || (search fn) c
| TUPLE4(a,b,c,d) -> (search fn) a || (search fn) b || (search fn) c || (search fn) d
| TUPLE5(a,b,c,d,e) -> (search fn) a || (search fn) b || (search fn) c || (search fn) d || (search fn) e
| oth -> fn oth

let othlst = ref []
(*
let fns = Hashtbl.create 257
*)
let enums = Hashtbl.create 257
let externs = Hashtbl.create 257
let structs = Hashtbl.create 257
let unions = Hashtbl.create 257
let ftypes = Hashtbl.create 257
let typedefs = Hashtbl.create 257
let inlines = Hashtbl.create 257
let globals = Hashtbl.create 257
let inits = Hashtbl.create 257
let fbody = Hashtbl.create 257

let redeflst = ref []

let ty_lookup ty =
  if Hashtbl.mem typedefs ty then
    Hashtbl.find typedefs ty
  else
    STRING_LITERAL (string_of_int (Hashtbl.hash ty)) (* make a unique incompatible result *)

let rec ty_incompat = function
| (TYPE_NAME a, TYPE_NAME b) -> TYPE_NAME a <>  ty_lookup b && ty_lookup a <> TYPE_NAME b && ty_incompat (ty_lookup a, ty_lookup b)
| (TUPLE2 (EXTERN, ty'), ty'') -> ty_incompat (ty', ty'')
| (ty', TUPLE2 (EXTERN, ty'')) -> ty_incompat (ty', ty'')
| (oldt,typ) -> oldt <> typ

let _globals key typ =
  if Hashtbl.mem globals key then
    begin
    let old = Hashtbl.find globals key in
    if old <> typ then (redeflst := ('g',key) :: !redeflst;Hashtbl.add globals key typ)
    end
  else Hashtbl.add globals key typ

let _fbody key (typ,params,body) =
  if Hashtbl.mem fbody key then
      begin
      assert(key <> "get_idstring");
      redeflst := ('b',key) :: !redeflst;
      Hashtbl.add fbody key (typ,params,body);
      end
  else Hashtbl.add fbody key (typ,params,body)

let _structs key params =
  if params <> EMPTY_TOKEN then
    begin
    if Hashtbl.mem structs key then
       begin
       let old = Hashtbl.find structs key in
       if old <> params then (
	   redeflst := ('s',key) :: !redeflst;
	   Hashtbl.add structs key params)
       end
     else Hashtbl.add structs key params
     end

let _enums key enumerations =
  if Hashtbl.mem enums key then
    begin
    let old = Hashtbl.find enums key in
    if old <> enumerations then (redeflst := ('e',key) :: !redeflst;Hashtbl.add enums key enumerations)
    end
  else Hashtbl.add enums key enumerations

let _ftypes key (typ,params) =
  if Hashtbl.mem ftypes key then
    begin
    let (oldt,oldp) = Hashtbl.find ftypes key in
    if ty_incompat (oldt, typ) || oldp <> params then (redeflst := ('T',key) :: !redeflst;Hashtbl.add ftypes key (typ,params))
    end
  else Hashtbl.add ftypes key (typ,params)

let _typedefs key typedef =
  if Hashtbl.mem typedefs key then
    begin
    let old = Hashtbl.find typedefs key in
    if ty_incompat (old, typedef) then (
        redeflst := ('t',key) :: !redeflst;
        assert(key<>"va_list");
        Hashtbl.add typedefs key typedef)
    end
  else Hashtbl.add typedefs key typedef

let _externs key typ =
  if Hashtbl.mem externs key then
    begin
    let old = Hashtbl.find externs key in
    if old <> typ then (redeflst := ('x',key) :: !redeflst; Hashtbl.add externs key typ)
    end
  else Hashtbl.add externs key typ

let _unions key ulst =
  if Hashtbl.mem unions key then
    begin
    let old = Hashtbl.find unions key in
    if old <> ulst then (redeflst := ('u',key) :: !redeflst; Hashtbl.add unions key ulst)
    end
  else Hashtbl.add unions key ulst

let _inlines key (typ,params,body) =
  if Hashtbl.mem inlines key then
    begin
    let old = Hashtbl.find inlines key in
    if old <> (typ,params,body) then (
	redeflst := ('I',key) :: !redeflst;
	Hashtbl.add inlines key (typ,params,body))
    end
  else Hashtbl.add inlines key (typ,params,body)

let _inits key (typ,num) =
  if Hashtbl.mem inits key then redeflst := ('i', key) :: !redeflst;
  Hashtbl.add inits key (typ,num)

let nxtlst = ref []
let errlst = ref []

let getrslt arg =
   Printf.fprintf stderr "%s: " arg; flush stderr;
   match parse arg with
    | TUPLE2(tran,_) -> 
        List.iter (fun itm ->
            nxtlst := itm :: !nxtlst;
            Translation_unit_list_filt.filt errlst _enums _externs _fbody _ftypes _globals _inits _inlines _structs _typedefs _unions itm) (match tolst tran with TLIST lst -> lst | oth -> []);
        let typlst = ref [] in
	Hashtbl.iter (fun k _ -> typlst := k :: !typlst) typehash;
	let fnlst = ref [] in
	Hashtbl.iter (fun k _ -> fnlst := k :: !fnlst) fbody;
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
"Types=%d, Fun=%d, ext=%d, enum=%d, struc=%d, union=%d, ftyp=%d, type=%d, unrecog=%d, redef=%d\n"
(List.length !typlst)
(List.length !fnlst)
(List.length !extlst)
(List.length !enumlst)
(List.length !structlst)
(List.length !unionlst)
(List.length !ftyplst)
(List.length !typlst)
(List.length !errlst)
(List.length !redeflst);
flush stderr
    | oth -> othlst := oth :: !othlst; Translation_unit_list_filt.failtree oth

type drefs = {
  frefs: string list ref;
}

let typlst = ref []

let rec dumptyp = function
| DOUBLE -> "double"
| CHAR -> "char"
| VOID -> "void"
| INT -> "int"
| TUPLE2 (EXTERN, typ) -> "extern "^dumptyp typ
| TUPLE2 (UNSIGNED, typ) -> "unsigned "^dumptyp typ
| TUPLE2 (CONST, typ) -> "const "^dumptyp typ
| TUPLE2 (typ, STAR) -> dumptyp typ^" *"
| TUPLE3 (IDENTIFIER array, LBRACK, RBRACK) -> array^"[]"
| TYPE_NAME nam -> nam^" "
| oth -> typlst := oth :: !typlst; Translation_unit_list_filt.dumptree oth

let rec dumparg = function
| ELLIPSIS -> "..."
| TUPLE2(TYPE_NAME typ, IDENTIFIER arg) -> typ^" "^arg
| TUPLE2(TYPE_NAME typ, TUPLE2 (STAR, IDENTIFIER arg)) -> typ^" *"^arg
| TUPLE2(TUPLE2(CONST, TYPE_NAME typ), TUPLE2 (STAR, IDENTIFIER arg)) -> "const "^typ^" *"^arg
| TUPLE2((VOID|CHAR|INT|DOUBLE) as typ, IDENTIFIER arg) -> dumptyp typ^" "^arg
| TUPLE2(TUPLE2(CONST, ((CHAR|INT|DOUBLE) as typ)), IDENTIFIER arg) -> "const "^dumptyp typ^" "^arg
| TUPLE2((VOID|CHAR|INT|DOUBLE) as typ, TUPLE2 (STAR, IDENTIFIER arg)) -> dumptyp typ^" *"^arg
| TUPLE2(TUPLE2(CONST, ((CHAR|INT|DOUBLE) as typ)), TUPLE2 (STAR, IDENTIFIER arg)) -> "const "^dumptyp typ^" *"^arg
| TUPLE2((CHAR|INT|DOUBLE) as typ, TUPLE2 (TUPLE2 (STAR, STAR), IDENTIFIER arg)) -> dumptyp typ^" **"^arg
| TUPLE2((CHAR|INT|DOUBLE) as typ, IDENTIFIER arg) -> dumptyp typ^" "^arg
| TUPLE2((VOID|CHAR|INT|DOUBLE) as typ, (TUPLE3 (IDENTIFIER _, LBRACK, RBRACK) as array)) -> dumptyp typ^" "^dumptyp array
| TUPLE2(ty, p) -> dumptyp ty^" "^Translation_unit_list_filt.dumptree p
| oth -> typlst := oth :: !typlst; Translation_unit_list_filt.dumptree oth

let mark refs fn =
  if not (List.mem fn !(refs.frefs)) then
    refs.frefs := fn :: !(refs.frefs)

let rec dumpc refs = function
| CONSTANT num -> num
| IDENTIFIER id -> id
| STRING_LITERAL str -> str
| TUPLE2 (STAR, IDENTIFIER ptr) -> " *"^ptr
| TUPLE3 ((VOID|CHAR|INT|DOUBLE) as typ, ptr, SEMICOLON) -> dumptyp typ^dumpc refs ptr^";"
| TUPLE3 (LPAREN, expr, RPAREN) -> "("^dumpc refs expr^")"
| TUPLE5 (expr, QUERY, expr1, COLON, expr2) -> dumpc refs expr^" ? "^dumpc refs expr1^" : "^dumpc refs expr2
| TUPLE4 (LPAREN, ((VOID|CHAR|INT|DOUBLE) as typ), RPAREN, expr) -> "("^dumptyp typ^") "^dumpc refs expr
| TUPLE4 (LPAREN, (TUPLE2 ((VOID|CHAR|INT|DOUBLE|TYPE_NAME _), STAR) as typ), RPAREN, expr) -> "("^dumptyp typ^") "^dumpc refs expr
| TUPLE3 ((VOID|CHAR|INT|DOUBLE|TUPLE2 (CONST, CHAR)|TYPE_NAME _) as typ, (TUPLE3 (_, EQUALS, _) as asgn), SEMICOLON) -> dumptyp typ^" "^dumpc refs asgn
| TUPLE3 ((VOID|CHAR|INT|DOUBLE|TYPE_NAME _) as typ, (TUPLE3 (TUPLE2 (STAR, IDENTIFIER lft), EQUALS, _) as asgn), SEMICOLON) -> dumptyp typ^" *"^dumpc refs asgn
| TUPLE3 (IDENTIFIER fn, LPAREN, RPAREN) -> mark refs fn; fn^"();"
| TUPLE4 (IDENTIFIER fn, LPAREN, args, RPAREN) -> mark refs fn; fn^"("^adump refs args^");"
| TUPLE4 (arr, LBRACK, expr, RBRACK) -> dumpc refs arr^"["^adump refs expr^"]"
| TUPLE2 (HYPHEN, rght) -> "-"^dumpc refs rght
| TUPLE2 (STAR, rght) -> "*"^dumpc refs rght
| TUPLE3 (lft, EQ_OP, rght) -> dumpc refs lft^"=="^dumpc refs rght
| TUPLE3 (lft, GE_OP, rght) -> dumpc refs lft^">="^dumpc refs rght
| TUPLE3 (lft, LE_OP, rght) -> dumpc refs lft^"<="^dumpc refs rght
| TUPLE3 (lft, GREATER, rght) -> dumpc refs lft^">"^dumpc refs rght
| TUPLE3 (lft, LESS, rght) -> dumpc refs lft^" < "^dumpc refs rght
| TUPLE3 (lft, PLUS, rght) -> dumpc refs lft^"+"^dumpc refs rght
| TUPLE3 (lft, HYPHEN, rght) -> dumpc refs lft^"-"^dumpc refs rght
| TUPLE3 (lft, STAR, rght) -> dumpc refs lft^"*"^dumpc refs rght
| TUPLE3 (lft, SLASH, rght) -> dumpc refs lft^"*"^dumpc refs rght
| TUPLE3 (lft, PERCENT, rght) -> dumpc refs lft^"%"^dumpc refs rght
| TUPLE3 (lft, VBAR, rght) -> dumpc refs lft^" | "^dumpc refs rght
| TUPLE5 (IF, LPAREN, cond, RPAREN, then') -> "if ("^dumpc refs cond^") "^dumpc refs then'
| TUPLE3 (LBRACE, body, RBRACE) -> "\n\t{\n\t"^dumpc refs body^"\n\t}\n"
| TLIST lst -> String.concat "\n" (List.map (dumpc refs) lst)
| TUPLE2 (stmt, SEMICOLON) -> dumpc refs stmt^"; "
| TUPLE3 (lft, EQUALS, expr) -> dumpc refs lft^" = "^dumpc refs expr
| TUPLE4 (LPAREN, TUPLE2 (DOUBLE, STAR), RPAREN, rhs) -> "(double *)"^dumpc refs rhs
| TUPLE3 (IDENTIFIER str, DOT, IDENTIFIER memb) -> str^"."^memb
| TUPLE3 (IDENTIFIER str, PTR_OP, IDENTIFIER memb) -> str^"->"^memb
| TUPLE4 (SIZEOF, LPAREN, typ, RPAREN) -> "sizeof("^dumptyp typ^")"
| TUPLE2 (AMPERSAND, compound) -> "&"^dumpc refs compound
| TUPLE7 (FOR, LPAREN, TUPLE3 (INT, initial, SEMICOLON), TUPLE2 (condition, SEMICOLON), inc, RPAREN, body) -> "for ("^dumpc refs initial^"; "^dumpc refs condition^"; "^dumpc refs inc^") "^dumpc refs body
| TUPLE7 (IF, LPAREN, expr, RPAREN, then', ELSE, else') -> "if ("^dumpc refs expr^") "^dumpc refs then'^" else "^dumpc refs else'
| TUPLE2 (lvalue, INC_OP) -> dumpc refs lvalue^"++"
| TUPLE3 (RETURN, arg, SEMICOLON) -> "return "^dumpc refs arg^"; "
| TUPLE3 (lvalue, ADD_ASSIGN, expr) -> dumpc refs lvalue^"+="^dumpc refs expr
| TUPLE3 (lvalue, SUB_ASSIGN, expr) -> dumpc refs lvalue^"-="^dumpc refs expr
| TUPLE3 (lvalue, MUL_ASSIGN, expr) -> dumpc refs lvalue^"*="^dumpc refs expr
| TUPLE3 (lvalue, DIV_ASSIGN, expr) -> dumpc refs lvalue^"/="^dumpc refs expr

| oth -> Translation_unit_list_filt.dumptree oth

and adump refs = function
| TLIST lst -> String.concat ", " (List.map (dumpc refs) lst)
| VOID -> ""
| oth -> dumpc refs oth

and pdump refs = function
| TLIST lst -> String.concat ", " (List.map (dumpc refs) lst)
| VOID -> ""
| oth -> dumpc refs oth

let dump refs key =
  print_endline ("********* "^key^" *********");
  if Hashtbl.mem fbody key then
    begin
    let (typ,paramlst,body) = Hashtbl.find fbody key in
    print_endline (dumptyp typ^" "^key^"("^String.concat ", " (List.map dumparg paramlst)^")\n{");
    List.iter (fun itm -> print_endline (dumpc refs itm)) body;
    print_endline "}";
    end
  else if Hashtbl.mem ftypes key then
    begin
    let (typ,paramlst) = Hashtbl.find ftypes key in
    print_endline (dumptyp typ^" "^key^"("^String.concat ", " (List.map dumparg paramlst)^");\n");
    end

let _ = for i = 1 to Array.length Sys.argv - 1 do getrslt Sys.argv.(i); done;
let refs = {frefs=ref []} in dump refs "main";
while (List.length !(refs.frefs) > 0) do
let refs' = List.sort compare (!(refs.frefs)) in
refs.frefs := [];
List.iter (fun itm -> dump refs itm) refs'
done

