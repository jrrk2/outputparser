
open Translation_unit_list
open Translation_unit_list_types
open Translation_unit_list_lex

let verbose = ref false

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
| TUPLE6(a,b,c,d,e,f) -> (search fn) a || (search fn) b || (search fn) c || (search fn) d || (search fn) e || (search fn) f
| TUPLE7(a,b,c,d,e,f,g) -> (search fn) a || (search fn) b || (search fn) c || (search fn) d || (search fn) e || (search fn) f || (search fn) g
| TLIST lst -> List.fold_left (||) false (List.map (search fn) lst)
| oth -> fn oth

let othlst = ref []
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
  if false then assert(key <> "xcalloc");
  if Hashtbl.mem fbody key then
      begin
      assert(key <> "get_idstring");
      redeflst := ('b',key) :: !redeflst;
      Hashtbl.add fbody key (typ,params,body);
      end
  else Hashtbl.add fbody key (typ,params,body)

let _structs key params =
  if params <> [] then
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

let getrslt parse arg =
   Printf.fprintf stderr "%s: " arg; flush stderr;
   match parse arg with
    | TUPLE2(tran,_) -> 
        List.iter (fun itm ->
            nxtlst := itm :: !nxtlst;
            Translation_unit_list_filt.filt errlst _enums _externs _fbody _ftypes _globals _inits _inlines _structs _typedefs _unions itm) (match tolst tran with TLIST lst -> lst | oth -> []);
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
flush stderr;
(!fnlst, !extlst, !enumlst, !structlst, !unionlst, !ftyplst, !typlst, !errlst, !redeflst)
    | oth -> othlst := oth :: !othlst; Translation_unit_list_filt.failtree oth

type deps = {
  elst: string list;
  flst: string list;
  ilst: string list;
  slst: string list;
  tlst: string list;
  ulst: string list;
  }

type drefs = {
  erefs: string list ref;
  frefs: string list ref;
  irefs: string list ref;
  srefs: string list ref;
  trefs: string list ref;
  urefs: string list ref;
  etab: (string,int*deps*string) Hashtbl.t;
  ftab: (string,int*deps*string) Hashtbl.t;
  itab: (string,int*deps*string) Hashtbl.t;
  stab: (string,int*deps*string) Hashtbl.t;
  ttab: (string,int*deps*string) Hashtbl.t;
  utab: (string,int*deps*string) Hashtbl.t;
}

let frefs () = {erefs=ref [];frefs=ref [];irefs=ref [];srefs=ref [];trefs=ref [];urefs=ref [];
	    etab=Hashtbl.create 257;ftab=Hashtbl.create 257;itab=Hashtbl.create 257;stab=Hashtbl.create 257;
            ttab=Hashtbl.create 257;utab=Hashtbl.create 257}

let typerrlst = ref []

let emark' refs key k = function
  | IDENTIFIER e when e=key ->
    if not (List.mem k !(refs.erefs)) then
    begin
    if !verbose then prerr_endline ("****** "^k^" ******");
    refs.erefs := k :: !(refs.erefs)
    end
  | oth -> ()

let emark refs = function
  | IDENTIFIER key ->
    Hashtbl.iter (fun k x -> match x with
      | TUPLE3 (e, EQUALS, CONSTANT _) -> emark' refs key k e
      | TLIST lst -> List.iter (emark' refs key k) lst
      | oth -> ()) enums;
    false
  | oth -> false

let lmark refs = function
  | IDENTIFIER key ->
    if not (List.mem key !(refs.irefs)) then
      begin
      refs.irefs := key :: !(refs.irefs)
      end;
    false
  | oth -> false

let fmark refs fn =
  if not (List.mem fn !(refs.frefs)) then
    begin
    refs.frefs := fn :: !(refs.frefs)
    end

let smark strong refs str =
  if not (List.mem str !(refs.srefs)) then
    begin
    if strong then refs.srefs := str :: !(refs.srefs)
    end

let umark refs str =
  if not (List.mem str !(refs.urefs)) then
    begin
    refs.urefs := str :: !(refs.urefs)
    end

let tmark refs str =
  if not (List.mem str !(refs.trefs)) then
    begin
    refs.trefs := str :: !(refs.trefs)
    end

let rec is_enum id = function
      | TUPLE3 (IDENTIFIER e, EQUALS, CONSTANT n) -> id=e
      | TLIST lst -> List.fold_left (||) false (List.map (is_enum id) lst)
      | oth -> false

let rec is_int = function
| CONSTANT n -> (try let _ = int_of_string n in true with _ -> false)
| IDENTIFIER id -> let found = ref false in
    Hashtbl.iter (fun _ x -> if is_enum id x then found := true) enums;
    !found
| TUPLE3 (LPAREN, cexpr, RPAREN) -> is_int cexpr
| TUPLE3 (lft, (PLUS|HYPHEN|STAR|SLASH), rght) when is_int lft && is_int rght -> true
| oth -> false

let rec cexpr_as_int = function
| CONSTANT n -> int_of_string n
| IDENTIFIER id -> let found = ref None in
    Hashtbl.iter (fun _ x -> as_enum found id x) enums;
    let f = function Some x -> x | None -> failwith "Called cexpr_as_int on non-int" in f !found
| TUPLE3 (LPAREN, cexpr, RPAREN) -> cexpr_as_int cexpr
| TUPLE3 (lft, PLUS, rght) -> cexpr_as_int lft + cexpr_as_int rght
| TUPLE3 (lft, HYPHEN, rght) -> cexpr_as_int lft - cexpr_as_int rght
| TUPLE3 (lft, STAR, rght) -> cexpr_as_int lft * cexpr_as_int rght
| TUPLE3 (lft, SLASH, rght) -> cexpr_as_int lft / cexpr_as_int rght
| TUPLE3 (lft, PERCENT, rght) -> cexpr_as_int lft mod cexpr_as_int rght
| oth -> failwith "Called cexpr_as_int on non-int"

and as_enum rslt id = function
      | TUPLE3 (IDENTIFIER e, EQUALS, (CONSTANT _ as n)) -> if id=e then rslt := Some (cexpr_as_int n)
      | TLIST lst -> List.iter (as_enum rslt id) lst
      | oth -> ()

let rec dumptyp refs = function
| DOUBLE -> "double"
| FLOAT -> "float"
| CHAR -> "char"
| VOID -> "void"
| INT -> "int"
| LONG -> "long"
| SHORT -> "short"
| CONST -> "const"
| EXTERN -> "extern"
| STATIC -> "static"
| STRUCT -> "struct"
| UNSIGNED -> "unsigned"
| SIGNED -> "signed"
| STAR -> " *"
| TUPLE3 (IDENTIFIER array, LBRACK, RBRACK) -> array^"[]"
| TUPLE2 (typ,typ') -> dumptyp refs typ^" "^dumptyp refs typ'
| TYPE_NAME nam -> tmark refs nam; nam^" "
| TLIST lst -> String.concat " " (List.map (dumptyp refs) lst)^" "
| oth -> typerrlst := oth :: !typerrlst; "/* 357 */"^Translation_unit_list_filt.dumptree oth

let rec dumparg refs = function
| ELLIPSIS -> "..."
| TUPLE2(TYPE_NAME typ, IDENTIFIER arg) -> tmark refs typ; typ^" "^arg
| TUPLE2(TYPE_NAME typ, TUPLE2 (STAR, IDENTIFIER arg)) -> tmark refs typ; typ^" *"^arg
| TUPLE2(TUPLE2(CONST, TYPE_NAME typ), TUPLE2 (STAR, IDENTIFIER arg)) -> tmark refs typ; "const "^typ^" *"^arg
| TUPLE2((VOID|CHAR|INT|DOUBLE) as typ, IDENTIFIER arg) -> dumptyp refs typ^" "^arg
| TUPLE2(TUPLE2(CONST, ((CHAR|INT|DOUBLE) as typ)), IDENTIFIER arg) -> "const "^dumptyp refs typ^" "^arg
| TUPLE2(TUPLE2(CONST, ((CHAR|INT|DOUBLE) as typ)), TUPLE2(STAR, RESTRICT)) -> "const "^dumptyp refs typ^" *"
| TUPLE2((VOID|CHAR|INT|DOUBLE) as typ, TUPLE2 (STAR, IDENTIFIER arg)) -> dumptyp refs typ^" *"^arg
| TUPLE2(TUPLE2(CONST, ((CHAR|INT|DOUBLE) as typ)), TUPLE2 (STAR, IDENTIFIER arg)) -> "const "^dumptyp refs typ^" *"^arg
| TUPLE2((CHAR|INT|DOUBLE) as typ, TUPLE2 (TUPLE2 (STAR, STAR), IDENTIFIER arg)) -> dumptyp refs typ^" **"^arg
| TUPLE2((VOID|CHAR|INT|DOUBLE) as typ, (TUPLE3 (IDENTIFIER _, LBRACK, RBRACK) as array)) -> dumptyp refs typ^" "^dumptyp refs array
| TUPLE2(TUPLE2(CONST, ((CHAR|INT|DOUBLE) as typ)), STAR) -> "const "^dumptyp refs typ^" *"
| TUPLE2(TUPLE2(UNSIGNED, INT) as typ, IDENTIFIER arg) -> dumptyp refs typ^" "^arg
| TUPLE2(CHAR, TUPLE2(STAR, (TUPLE3 (IDENTIFIER _, LBRACK, RBRACK) as array))) -> "char * "^dumptyp refs array
| TUPLE2(ty, p) -> "/* 339 */"^dumptyp refs ty^" "^Translation_unit_list_filt.dumptree p
| ty -> dumptyp refs ty

let rec dumpc refs = function
| CONSTANT num -> num
| IDENTIFIER id as s -> let _ = lmark refs s in id
| STRING_LITERAL str -> str
| TUPLE2 (STAR, IDENTIFIER ptr) -> " *"^ptr
| TUPLE2 (TILDE, expr) -> " ~"^dumpc refs expr
| TUPLE2 (RETURN, SEMICOLON) -> "return;"
| TUPLE2 (CONTINUE, SEMICOLON) -> "continue;"
| TUPLE3 ((VOID|BOOL|CHAR|INT|DOUBLE|TUPLE2 (CONST, CHAR)|TYPE_NAME _) as typ, TLIST lst, SEMICOLON) ->
    String.concat ";\n\t" (List.map (fun itm -> dumptyp refs typ^" "^dumpc refs itm) lst)^";"
| TUPLE3 ((VOID|BOOL|CHAR|INT|DOUBLE|TUPLE2 (CONST, CHAR)|TYPE_NAME _) as typ, ptr, SEMICOLON) -> dumptyp refs typ^" "^dumpc refs ptr^";"
| TUPLE3 (LPAREN, expr, RPAREN) -> "("^dumpc refs expr^")"
| TUPLE5 (expr, QUERY, expr1, COLON, expr2) -> dumpc refs expr^" ? "^dumpc refs expr1^" : "^dumpc refs expr2
| TUPLE4 (LPAREN, ((VOID|BOOL|CHAR|INT|DOUBLE) as typ), RPAREN, expr) -> "("^dumptyp refs typ^") "^dumpc refs expr
| TUPLE4 (LPAREN, (TUPLE2 ((VOID|BOOL|CHAR|INT|DOUBLE|TYPE_NAME _), STAR) as typ), RPAREN, expr) -> "("^dumptyp refs typ^") "^dumpc refs expr
| TUPLE3 (IDENTIFIER fn, LPAREN, RPAREN) -> fmark refs fn; fn^"()"
| TUPLE4 (IDENTIFIER fn, LPAREN, args, RPAREN) ->
    fmark refs fn;
    let _ = search (lmark refs) args in
    fn^"("^adump refs args^")"
| TUPLE4 (arr, LBRACK, expr, RBRACK) -> dumpc refs arr^"["^adump refs expr^"]"
| TUPLE2 (HYPHEN, rght) -> "-"^dumpc refs rght
| TUPLE2 (PLING, rght) -> "!"^dumpc refs rght
| TUPLE2 (STAR, rght) -> "*"^dumpc refs rght
| TUPLE2 (SIZEOF, rght) -> "sizeof("^dumpc refs rght^")"
| TUPLE3 (lft, EQ_OP, rght) -> dumpc refs lft^"=="^dumpc refs rght
| TUPLE3 (lft, NE_OP, rght) -> dumpc refs lft^"!="^dumpc refs rght
| TUPLE3 (lft, GE_OP, rght) -> dumpc refs lft^">="^dumpc refs rght
| TUPLE3 (lft, LE_OP, rght) -> dumpc refs lft^"<="^dumpc refs rght
| TUPLE3 (lft, GREATER, rght) -> dumpc refs lft^">"^dumpc refs rght
| TUPLE3 (lft, LESS, rght) -> dumpc refs lft^" < "^dumpc refs rght
| TUPLE3 (lft, PLUS, rght) -> dumpc refs lft^"+"^dumpc refs rght
| TUPLE3 (lft, HYPHEN, rght) -> dumpc refs lft^"-"^dumpc refs rght
| TUPLE3 (lft, STAR, rght) -> dumpc refs lft^"*"^dumpc refs rght
| TUPLE3 (lft, SLASH, rght) -> dumpc refs lft^"/"^dumpc refs rght
| TUPLE3 (lft, PERCENT, rght) -> dumpc refs lft^"%"^dumpc refs rght
| TUPLE3 (lft, LEFT_OP, rght) -> dumpc refs lft^" << "^dumpc refs rght
| TUPLE3 (lft, RIGHT_OP, rght) -> dumpc refs lft^" >> "^dumpc refs rght
| TUPLE3 (lft, VBAR, rght) -> dumpc refs lft^" | "^dumpc refs rght
| TUPLE3 (lft, CARET, rght) -> dumpc refs lft^" ^ "^dumpc refs rght
| TUPLE3 (lft, AMPERSAND, rght) -> dumpc refs lft^" & "^dumpc refs rght
| TUPLE3 (lft, OR_OP, rght) -> dumpc refs lft^" || "^dumpc refs rght
| TUPLE3 (lft, AND_OP, rght) -> dumpc refs lft^" && "^dumpc refs rght
| TUPLE5 (IF, LPAREN, cond, RPAREN, then') -> "if ("^dumpc refs cond^") "^dumpc refs then'
| TUPLE3 (LBRACE, body, RBRACE) -> "\n\t{\n\t"^dumpc refs body^"\n\t}\n"
| TLIST lst -> String.concat "\n" (List.map (dumpc refs) lst)
| TUPLE2 (stmt, SEMICOLON) -> dumpc refs stmt^"; "
| TUPLE3 (lft, EQUALS, TUPLE3(LBRACE, TLIST lst, RBRACE)) ->
    let _ = search (lmark refs) lft in
    let _ = search (lmark refs) (TLIST lst) in
    dumpc refs lft^" = {"^String.concat ", " (List.map (dumpc refs) lst)^"}"
| TUPLE3 (lft, EQUALS, expr) ->
    let _ = search (lmark refs) lft in
    let _ = search (lmark refs) expr in
    dumpc refs lft^" = "^dumpc refs expr
| TUPLE3 (IDENTIFIER str, DOT, IDENTIFIER memb) -> smark true refs str; str^"."^memb
| TUPLE3 (IDENTIFIER str, PTR_OP, IDENTIFIER memb) -> smark true refs str; str^"->"^memb
| TUPLE4 (SIZEOF, LPAREN, typ, RPAREN) -> "sizeof("^dumptyp refs typ^")"
| TUPLE2 (AMPERSAND, compound) -> "&"^dumpc refs compound
| TUPLE7 (FOR, LPAREN, TUPLE3 (typ, initial, SEMICOLON), TUPLE2 (condition, SEMICOLON), inc, RPAREN, body) -> "{for ("^dumptyp refs typ^" "^dumpc refs initial^"; "^dumpc refs condition^"; "^dumpc refs inc^") "^dumpc refs body^"}\n"
| TUPLE7 (FOR, LPAREN, TUPLE2 (initial, SEMICOLON), TUPLE2 (condition, SEMICOLON), inc, RPAREN, body) -> "{for ( "^dumpc refs initial^"; "^dumpc refs condition^"; "^dumpc refs inc^")\n\t{ "^dumpc refs body^" }}\n"
| TUPLE7 (FOR, LPAREN, SEMICOLON, TUPLE2 (condition, SEMICOLON), inc, RPAREN, TUPLE3 (LBRACE, body, RBRACE)) -> "{for ( ; "^dumpc refs condition^"; "^dumpc refs inc^")\n\t{ "^dumpc refs body^" }}\n"
| TUPLE7 (IF, LPAREN, expr, RPAREN, then', ELSE, else') -> "if ("^dumpc refs expr^") "^dumpc refs then'^" else "^dumpc refs else'
| TUPLE2 (lvalue, INC_OP) -> dumpc refs lvalue^"++"
| TUPLE2 (lvalue, DEC_OP) -> dumpc refs lvalue^"--"
| TUPLE3 (RETURN, arg, SEMICOLON) -> "return "^dumpc refs arg^"; "
| TUPLE3 (lvalue, ADD_ASSIGN, expr) -> dumpc refs lvalue^"+="^dumpc refs expr
| TUPLE3 (lvalue, SUB_ASSIGN, expr) -> dumpc refs lvalue^"-="^dumpc refs expr
| TUPLE3 (lvalue, MUL_ASSIGN, expr) -> dumpc refs lvalue^"*="^dumpc refs expr
| TUPLE3 (lvalue, DIV_ASSIGN, expr) -> dumpc refs lvalue^"/="^dumpc refs expr
| TUPLE3 (lvalue, AND_ASSIGN, expr) -> dumpc refs lvalue^"&="^dumpc refs expr
| TUPLE3 (lvalue, OR_ASSIGN, expr) -> dumpc refs lvalue^"|="^dumpc refs expr
| TUPLE3 (lvalue, XOR_ASSIGN, expr) -> dumpc refs lvalue^"^="^dumpc refs expr
| TUPLE3 (lvalue, LEFT_ASSIGN, expr) -> dumpc refs lvalue^"<<="^dumpc refs expr
| TUPLE3 (lvalue, RIGHT_ASSIGN, expr) -> dumpc refs lvalue^">>="^dumpc refs expr
| TUPLE3 (lft, COMMA, rght) -> dumpc refs lft^", "^dumpc refs rght
| TUPLE3 (lft, PTR_OP, rght) -> dumpc refs lft^"->"^dumpc refs rght
| TUPLE3 (TUPLE4 (lft, LBRACK, expr, RBRACK), DOT, IDENTIFIER field) -> dumpc refs lft^"["^dumpc refs expr^"]."^field
| TUPLE3 (TUPLE4 (UNION, LBRACE, TLIST lst, RBRACE), IDENTIFIER u, SEMICOLON) ->
   "union { "^String.concat "\n\t" (List.map (dumpc refs) lst)^" } "^u^";"
| TUPLE4 (LPAREN, TYPE_NAME name_t, RPAREN, expr) -> "("^name_t^")"^dumpc refs expr
| TUPLE3 (TUPLE2 (LONG, LONG), TUPLE3 (IDENTIFIER lhs, EQUALS, rhs), SEMICOLON) ->
   "long long "^lhs^" = "^dumpc refs rhs^";"
| TUPLE3 (TUPLE4 (UNION, LBRACE, TLIST lst, RBRACE), TUPLE3 (IDENTIFIER u, EQUALS, expr), SEMICOLON) ->
   "union { "^String.concat "\n\t" (List.map (dumpc refs) lst)^" } "^u^" = "^dumpc refs expr^";"
| TUPLE3 (IDENTIFIER id, LBRACK, RBRACK) -> id^"[]"
| TUPLE5 (SWITCH, LPAREN, swexpr, RPAREN, TUPLE3 (LBRACE, TLIST lst, RBRACE)) ->
   "switch("^dumpc refs swexpr^") {"^String.concat "\n\t" (List.map (dumpc refs) lst)^" }"
| TUPLE4 (CASE, cexpr, COLON, stmt) -> "case "^dumpc refs cexpr^": "^dumpc refs stmt
| TUPLE3 (DEFAULT, COLON, stmt) -> "default: "^dumpc refs stmt
| TUPLE7 (DO, TUPLE3 (LBRACE, TLIST lst, RBRACE), WHILE, LPAREN, doexpr, RPAREN, SEMICOLON) ->
   "do { "^String.concat "\n\t" (List.map (dumpc refs) lst)^" } while ("^dumpc refs doexpr^");"
| TUPLE5 (WHILE, LPAREN, whexpr, RPAREN, TUPLE3 (LBRACE, TLIST lst, RBRACE)) ->
   "while ("^dumpc refs whexpr^") { "^String.concat "\n\t" (List.map (dumpc refs) lst)^" };"
| TUPLE6 (LPAREN, TYPE_NAME name_t, RPAREN, LBRACE, cexpr, RBRACE) ->
   "("^name_t^") {"^dumpc refs cexpr^"} "
| TUPLE2 (TUPLE2 (STAR, CONST), TUPLE3 (IDENTIFIER ty, LBRACK, RBRACK)) -> "* const "^ty^"[] "
| TUPLE2 (LBRACE, RBRACE) -> "{ }"
| oth -> "/* 416 */"^Translation_unit_list_filt.dumptree oth

and adump refs = function
| TLIST lst -> String.concat ", " (List.map (dumpc refs) lst)
| VOID -> ""
| oth -> dumpc refs oth

and pdump refs = function
| TLIST lst -> String.concat ", " (List.map (dumpc refs) lst)
| VOID -> ""
| oth -> dumpc refs oth

let cdumplst = ref []
let simplify = try let _ = Sys.getenv "SIMPLIFY_CEXPR" in true with _ -> true

let rec cdump refs cexpr =
if simplify && is_int cexpr then
    (let rslt = cexpr_as_int cexpr in
    if not (List.mem (cexpr,rslt) !cdumplst) then
    cdumplst := (cexpr,rslt) :: !cdumplst; string_of_int rslt)
else dumpc refs cexpr

let edump' refs str fields =
    "enum "^str^"\n\t{\n\t"^String.concat ",\n\t" (List.map (function
      | IDENTIFIER field -> field
      | oth -> "/* 473 */"^Translation_unit_list_filt.dumptree oth) fields)^"\n\t} "

let sdump' refs str fields =
    "struct "^str^"\n\t{\n\t"^String.concat "\n\t" (List.map (function
      | TUPLE3 (TUPLE2 (STRUCT, IDENTIFIER str), TUPLE2 (STAR, IDENTIFIER field), SEMICOLON) ->
           smark false refs str; "struct "^str^" *"^field^";"
      | TUPLE3 (TUPLE2 (STRUCT, IDENTIFIER str), IDENTIFIER field, SEMICOLON) ->
           smark true refs str; "struct "^str^" "^field^";"
      | TUPLE3 (typ, TUPLE4 (TUPLE3 (LPAREN, TUPLE2 (STAR, IDENTIFIER fn), RPAREN), LPAREN, TLIST typlst, RPAREN), SEMICOLON) ->
           dumptyp refs typ^" (* "^fn^") ("^String.concat ", " (List.map (dumptyp refs) typlst)^");"
      | TUPLE3 (typ, TUPLE4 (TUPLE3 (LPAREN, TUPLE2 (STAR, IDENTIFIER fn), RPAREN), LPAREN, typ', RPAREN), SEMICOLON) ->
           dumptyp refs typ^" (* "^fn^") ("^dumptyp refs typ'^");"
      | TUPLE3 (typ, IDENTIFIER field, SEMICOLON) ->
           dumptyp refs typ^" "^field^";"
      | TUPLE3 (typ, TUPLE4 (IDENTIFIER field, LBRACK, cexpr, RBRACK), SEMICOLON) ->
           let _ = search (emark refs) cexpr in dumptyp refs typ^" "^field^"["^cdump refs cexpr^"];"
      | TUPLE3 (typ, TUPLE2 (STAR, IDENTIFIER field), SEMICOLON) ->
           dumptyp refs typ^" *"^field^";"
      | oth -> "/* 491 */"^Translation_unit_list_filt.dumptree oth) fields)^"\n\t} "

let sdump refs key =
  if Hashtbl.mem structs key then
    begin
    let fields = Hashtbl.find structs key in sdump' refs key fields^";\n"
    end
  else
    begin
    "//\n";
    end

let udump' refs str fields =
    "union "^str^"\n\t{\n\t"^String.concat "\n\t" (List.map (function
      | TUPLE3 (typ, IDENTIFIER field, SEMICOLON) ->
           dumptyp refs typ^" "^field^";"
      | TUPLE3 (typ, TUPLE4 (IDENTIFIER field, LBRACK, cexpr, RBRACK), SEMICOLON) ->
           let _ = search (emark refs) cexpr in dumptyp refs typ^" "^field^"["^cdump refs cexpr^"];"
      | oth -> "/* 509 */"^Translation_unit_list_filt.dumptree oth) fields)^"\n\t} "

let udump refs key =
  if Hashtbl.mem unions key then
    begin
    let fields = Hashtbl.find unions key in udump' refs key fields^";\n"
    end
  else
    begin
    ("union "^key^";");
    end

let edump refs key =
  if Hashtbl.mem enums key then
    begin
    let body = Hashtbl.find enums key in
    match body with
      | TUPLE5 (ENUM, IDENTIFIER str, LBRACE, TLIST fields, RBRACE) -> (edump' refs str fields)
      | TUPLE3 (IDENTIFIER e, EQUALS, CONSTANT n) -> "enum "^key^" {"^e^"="^n^"};\n"
      | oth -> ("/* 528 */"^Translation_unit_list_filt.dumptree oth)
    end
  else
    begin
    ("enum "^key^";");
    end

let tdump refs key =
  if Hashtbl.mem typedefs key then
    begin
    let body = Hashtbl.find typedefs key in
    match body with
      | TUPLE5 (ENUM, IDENTIFIER str, LBRACE, TLIST fields, RBRACE) -> 
           ("typedef "^edump' refs str fields^key^";\n")
      | TUPLE5 (STRUCT, IDENTIFIER str, LBRACE, TLIST fields, RBRACE) ->
           ("typedef "^sdump' refs str fields^key^";\n")
      | TUPLE5 (STRUCT, IDENTIFIER str, LBRACE, field, RBRACE) ->
           ("typedef "^sdump' refs str [field]^key^";\n")
      | TUPLE5 (UNION, IDENTIFIER str, LBRACE, TLIST fields, RBRACE) ->
           ("typedef "^udump' refs str fields^key^";\n")
      | TUPLE5 (UNION, IDENTIFIER str, LBRACE, field, RBRACE) ->
           ("typedef "^udump' refs str [field]^key^";\n")
      | TUPLE2 (STRUCT, IDENTIFIER str) -> smark true refs str;
           ("typedef struct "^str^" "^key^";\n")
      | TUPLE2 (UNION, IDENTIFIER str) -> umark refs str;
           ("typedef union "^str^" "^key^";\n")
      | typ ->
           ("typedef "^dumptyp refs typ^" "^key^";\n")
    end
  else
    begin
    ("struct "^key^";\n");
    end

let bodylst = ref []
let bodylst' = ref []
let rec dump_body refs = function
| CONSTANT str -> str
| STRING_LITERAL str -> str
| TUPLE2 (HYPHEN, CONSTANT str) -> "-"^str
| TLIST [TUPLE2 (TUPLE2 (DOT, IDENTIFIER field), EQUALS); CONSTANT num] -> "."^field^"="^num    
| TLIST (TUPLE2 (TUPLE2 (DOT, IDENTIFIER field), EQUALS) ::
        TUPLE3 (LBRACE, TLIST lst, RBRACE) :: []) ->
     "."^field^"={"^String.concat ",\n\t" (List.map (dump_body refs) lst)^" }"
| TUPLE2 (TUPLE2 (DOT, IDENTIFIER field), EQUALS) -> "."^field^"= "
| TUPLE2 (TUPLE2 (TUPLE2 (DOT, IDENTIFIER field), EQUALS), rslt) -> "."^field^"= "^dump_body refs rslt
| TUPLE3 (LBRACE, TLIST
    (TUPLE2 (TUPLE2 (DOT, IDENTIFIER field), EQUALS) :: TUPLE3 (LBRACE, TLIST clst, RBRACE) :: []), RBRACE) ->
        "{."^field^"={"^String.concat ",\n\t" (List.map (dump_body refs) clst)^" }}"
| TUPLE3 (LBRACE, TLIST lst, RBRACE) -> "{"^String.concat ",\n\t" (List.map (dump_body refs) lst)^"}"
| TUPLE3 (LBRACE, itm, RBRACE) -> dump_body refs (TUPLE3 (LBRACE, TLIST [itm], RBRACE))
| TUPLE2 (SIZEOF, TUPLE3 (LPAREN, (IDENTIFIER id as l), RPAREN)) ->
    let _ = lmark refs l in "sizeof ("^id^")"
| TUPLE2 (SIZEOF, TUPLE3 (LPAREN, TUPLE2 (STAR, (IDENTIFIER id as l)), RPAREN)) ->
    let _ = lmark refs l in "sizeof (*"^id^")"
| TLIST lst -> bodylst' := lst :: !bodylst';
    "{\n\t"^String.concat ",\n\t" (List.map (dump_body refs) lst)^"\n\t}"
| cexpr when is_int cexpr -> string_of_int (cexpr_as_int cexpr)
| TUPLE3 (lft, PLUS, rght) -> dump_body refs lft^"+"^dump_body refs rght
| TUPLE3 (lft, HYPHEN, rght) -> dump_body refs lft^"-"^dump_body refs rght
| TUPLE3 (lft, STAR, rght) -> dump_body refs lft^"*"^dump_body refs rght
| TUPLE3 (lft, SLASH, rght) -> dump_body refs lft^"/"^dump_body refs rght
| TUPLE3 (lft, PERCENT, rght) -> dump_body refs lft^"%"^dump_body refs rght
| body -> bodylst := body :: !bodylst; failwith "bodylst"

let idumplst = ref []
let idump refs key =
  if Hashtbl.mem inits key then
    begin
    let typ,body = Hashtbl.find inits key in
    (match typ with
      | TYPE_NAME id_t -> tmark refs id_t; id_t^" "^key^"="^dump_body refs body^";\n"
      | TUPLE2 (DOUBLE, STAR) -> dumptyp refs typ^" "^key^"="^dump_body refs body^";\n"
      | INT -> dumptyp refs typ^" "^key^"="^dump_body refs body^";\n"
      | DOUBLE -> dumptyp refs typ^" "^key^"["^cdump refs body^"]="^dump_body refs body^";\n"
      | oth -> "/* 595 */"^Translation_unit_list_filt.dumptree oth);
    end
  else if Hashtbl.mem globals key then
    begin
    let typ = Hashtbl.find globals key in
    (match typ with
      | TYPE_NAME id_t -> id_t^" "^key^";\n"
      | TUPLE2 (DOUBLE, STAR) -> dumptyp refs typ^" "^key^";\n"
      | TUPLE4 (IDENTIFIER array, LBRACK, cexpr, RBRACK) when is_int cexpr ->
          array^"["^string_of_int (cexpr_as_int cexpr)^"]"
(*
      | TUPLE4 (IDENTIFIER array, LBRACK, cexpr, RBRACK) ->
          array^"["^dumpc refs cexpr^"]"
*)
      | TUPLE2 (typ, TUPLE4 (IDENTIFIER array, LBRACK, cexpr, RBRACK)) ->
          dumptyp refs typ^" "^array^"["^dumpc refs cexpr^"];"
      | TUPLE2 (TUPLE2 (EXTERN, TUPLE2 (STRUCT, TYPE_NAME id_t)), STAR) -> "extern struct "^id_t^" *"^key^";\n"
      | typ -> idumplst := typ :: !idumplst; dumptyp refs typ^" "^key^";\n")
    end
  else "//\n"

let rdump refs key =
  if Hashtbl.mem typedefs key then tdump refs key
  else if Hashtbl.mem enums key then edump refs key
  else if Hashtbl.mem structs key then sdump refs key
  else if Hashtbl.mem unions key then udump refs key
  else "// rdump "^key^"\n"

let fdump refs key =
  if Hashtbl.mem fbody key then
    begin
    let (typ,paramlst,body) = Hashtbl.find fbody key in
    ("\n"^dumptyp refs typ^" "^key^"("^String.concat ", " (List.map (dumparg refs) paramlst)^")\n{")^
    String.concat "\n" (List.map (fun itm -> (dumpc refs itm)) body)^"}\n";
    end
  else if Hashtbl.mem inlines key then
    begin
    let (typ,paramlst,body) = Hashtbl.find inlines key in
    ("\n"^dumptyp refs typ^" "^key^"("^String.concat ", " (List.map (dumparg refs) paramlst)^")\n{")^
    String.concat "\n" (List.map (fun itm -> (dumpc refs itm)) body)^"}\n";
    end
  else if Hashtbl.mem ftypes key then
    begin
    let (typ,paramlst) = Hashtbl.find ftypes key in
    ("\n"^dumptyp refs typ^" "^key^"("^String.concat ", " (List.map (dumparg refs) paramlst)^");\n");
    end
  else "// fdump "^key^"\n"

let rec setlev maxlev lev refs tab dump itmlst =
    if lev > !maxlev then maxlev := lev;
    let itm = List.hd itmlst in
    let deps = if Hashtbl.mem tab itm then let (oldlev,deps,src) = Hashtbl.find tab itm in
      begin
      Hashtbl.replace tab itm (max lev oldlev,deps,src);
      if !verbose then prerr_endline ("setlev loop over "^itm^" at level: "^string_of_int lev);
      deps
      end
    else
      begin
      let refs = frefs() in
      let src = dump refs itm in
      let deps = {elst= !(refs.erefs);
                  flst= !(refs.frefs);
                  ilst= !(refs.irefs);
		  slst= !(refs.srefs);
		  tlst= !(refs.trefs);
		  ulst= !(refs.urefs)} in
      Hashtbl.add tab itm (lev,deps,src);
      deps
      end in
    List.iter (fun itm' -> 
		   if not (List.mem itm' itmlst) then setlev maxlev (lev+1) refs refs.ttab rdump (itm' :: itmlst)
		   ) deps.tlst;
    List.iter (fun itm' -> 
		   if not (List.mem itm' itmlst) then setlev maxlev (lev+1) refs refs.etab edump (itm' :: itmlst)
		   ) deps.elst;
    List.iter (fun itm' -> 
		   if not (List.mem itm' itmlst) then setlev maxlev (lev+1) refs refs.utab udump (itm' :: itmlst)
		   ) deps.ulst;
    List.iter (fun itm' -> 
		   if not (List.mem itm' itmlst) then setlev maxlev (lev+1) refs refs.stab sdump (itm' :: itmlst)
		   ) deps.slst;
    List.iter (fun itm' -> 
		   if not (List.mem itm' itmlst) then setlev maxlev (lev+1) refs refs.ftab fdump (itm' :: itmlst)
		   ) deps.flst;
    List.iter (fun itm' -> 
		   if not (List.mem itm' itmlst) then setlev maxlev (lev+1) refs refs.itab idump (itm' :: itmlst)
		   ) deps.ilst

let dump parse refs chan main argv =
  let rslts = ref [] in
  prerr_endline "/*";
  for i = 1 to Array.length argv - 1 do rslts := getrslt parse argv.(i) :: !rslts; done;
  prerr_endline "*/";
  let maxlev = ref 0 in
  setlev maxlev 0 refs refs.ftab fdump [main];
  let needed = ref [] in
  for i = !maxlev downto 0 do
  let print_uniq ch dump str (lev,_,itm) = if i = lev && itm <> "//\n" then
    begin
    needed := (dump,ch,lev,str) :: !needed;
    if !verbose then
        output_string chan ("/* "^String.make 1 ch^string_of_int lev^":"^str^" */\n"^itm)
    else
        output_string chan itm
    end in
  Hashtbl.iter (print_uniq 'e' (edump refs)) refs.etab;
  Hashtbl.iter (print_uniq 't' (tdump refs)) refs.ttab;
  Hashtbl.iter (print_uniq 'u' (udump refs)) refs.utab;
  Hashtbl.iter (print_uniq 's' (sdump refs)) refs.stab;
  Hashtbl.iter (print_uniq 'i' (idump refs)) refs.itab;
  Hashtbl.iter (print_uniq 'f' (fdump refs)) refs.ftab;
  done;
  List.rev !needed
