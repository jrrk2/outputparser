
open Translation_unit_list
open Translation_unit_list_types
open Translation_unit_list_lex

type citm =
  | B of token*token list*token list
  | E of token
  | F of token*token list
  | G of token
  | I of (token*token)
  | L of (token*token list*token list)
  | S of token list
  | T of token
  | U of token list
  | O of token

let verbose = ref true
let verbose2 = ref false

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
let names = Hashtbl.create 257
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
let others = Hashtbl.create 257

let redeflst = ref []

let ty_lookup ty =
  if Hashtbl.mem typedefs ty then
    let typ = match Hashtbl.find typedefs ty with T typ -> typ in typ
  else
    STRING_LITERAL (string_of_int (Hashtbl.hash ty)) (* make a unique incompatible result *)

let rec ty_incompat = function
| (TYPE_NAME a, TYPE_NAME b) -> TYPE_NAME a <>  ty_lookup b && ty_lookup a <> TYPE_NAME b && ty_incompat (ty_lookup a, ty_lookup b)
| (TUPLE2 (EXTERN, ty'), ty'') -> ty_incompat (ty', ty'')
| (ty', TUPLE2 (EXTERN, ty'')) -> ty_incompat (ty', ty'')
| (oldt,typ) -> oldt <> typ

let hashmem hash key =
  Hashtbl.mem hash key

let lookup key =
  List.map (fun tab -> 
    Hashtbl.find tab key) (Hashtbl.find_all names key)

let hashadd hash key (defn:citm) =
  Hashtbl.add names key hash;
  Hashtbl.add hash key defn

let _globals key typ =
  if hashmem globals key then
    begin
    match Hashtbl.find globals key with G old ->
    if old <> typ then (redeflst := ('g',key) :: !redeflst;hashadd globals key (G typ))
    end
  else hashadd globals key (G typ)

let _fbody key (typ,params,body) =
  if false then assert(key <> "xcalloc");
  if hashmem fbody key then
      begin
      assert(key <> "get_idstring");
      redeflst := ('b',key) :: !redeflst;
      hashadd fbody key (B (typ,params,body))
      end
  else hashadd fbody key (B (typ,params,body))

let _structs key params =
  if params <> [] then
    begin
    if hashmem structs key then
       begin
       match Hashtbl.find structs key with S old ->
       if old <> params then (
	   redeflst := ('s',key) :: !redeflst;
	   hashadd structs key (S params))
       end
     else hashadd structs key (S params)
     end

let _enums key enumerations =
  if hashmem enums key then
    begin
    match Hashtbl.find enums key with E old ->
    if old <> enumerations then (redeflst := ('e',key) :: !redeflst;hashadd enums key (E enumerations))
    end
  else hashadd enums key (E enumerations)

let _ftypes key (typ,params) =
  if hashmem ftypes key then
    begin
    match Hashtbl.find ftypes key with F (oldt,oldp) ->
    if ty_incompat (oldt, typ) || oldp <> params then (redeflst := ('T',key) :: !redeflst;hashadd ftypes key (F (typ,params)))
    end
  else hashadd ftypes key (F(typ,params))

let _typedefs key typedef =
  if hashmem typedefs key then
    begin
    match Hashtbl.find typedefs key with T old ->
    if ty_incompat (old, typedef) then (
        redeflst := ('t',key) :: !redeflst;
        assert(key<>"va_list");
        hashadd typedefs key (T typedef))
    end
  else hashadd typedefs key (T typedef)

let _externs key typ =
  if hashmem externs key then
    begin
    match Hashtbl.find externs key with E old ->
    if old <> typ then (redeflst := ('x',key) :: !redeflst; hashadd externs key (E typ))
    end
  else hashadd externs key (E typ)

let _unions key ulst =
  if hashmem unions key then
    begin
    match Hashtbl.find unions key with U old ->
    if old <> ulst then (redeflst := ('u',key) :: !redeflst; hashadd unions key (U ulst))
    end
  else hashadd unions key (U ulst)

let _others key oth =
  hashadd others key (O oth)

let _inlines key (typ,params,body) =
  if hashmem inlines key then
    begin
    match Hashtbl.find inlines key with L old ->
    if old <> (typ,params,body) then (
	redeflst := ('L',key) :: !redeflst;
	hashadd inlines key (L (typ,params,body)))
    end
  else hashadd inlines key (L (typ,params,body))

let _inits key (typ,num) =
  if hashmem inits key then redeflst := ('i', key) :: !redeflst;
  hashadd inits key (I (typ,num))

let nxtlst = ref []
let dbgtree = ref EOF_TOKEN
let otht = ref EOF_TOKEN

let rec truncate = function
| TUPLE3(STRING ("translation_unit_list232"|"block_item_list212"|"specifier_qualifier_list120"),lft,rght) -> (match truncate lft with TLIST arg -> TLIST (truncate rght :: arg) | oth -> TLIST (truncate rght :: oth :: []))
| TUPLE3(STRING "ml_start0", a, b) -> TUPLE2(truncate a, truncate b)
| TUPLE3(STRING ("parameter_declaration167"|"parameter_declaration168"|"declaration_specifiers80"|"declaration_specifiers82"|"declarator142"|"expression_statement216"|"unary_expression20"|"jump_statement228"|"type_name173") as t, a, b) -> TUPLE3(t, truncate a, truncate b)
| TUPLE3(STRING oth, a, b) as t -> otht := t; failwith oth
| TUPLE3((VOID|INT|TUPLE3 _) as t, a, b) -> TUPLE3(truncate t, truncate a, truncate b)
| TUPLE4(STRING ("initializer_list193"|"parameter_list166"|"argument_expression_list16"),lft,COMMA,rght) -> (match truncate lft with TLIST arg -> TLIST (truncate rght :: arg) | oth -> TLIST (truncate rght :: oth :: []))
| TUPLE4(STRING ("parameter_type_list164"|"direct_abstract_declarator177"|"init_declarator90"|"function_definition236"|"compound_statement210"|"jump_statement230"|"inclusive_or_expression54"|"shift_expression40"|"additive_expression37"|"primary_expression4"|"and_expression50"|"shift_expression39"|"assignment_expression62"|"additive_expression36"|"relational_expression42"|"relational_expression43"|"relational_expression44"|"relational_expression45"|"equality_expression47"|"equality_expression48"|"exclusive_or_expression52"|"multiplicative_expression32"|"multiplicative_expression34"|"direct_declarator145"|"multiplicative_expression33"|"direct_abstract_declarator179") as s, a, b, c) -> TUPLE4(s, truncate a, truncate b, truncate c)
| TUPLE5(STRING ("direct_declarator154"|"direct_abstract_declarator181"|"direct_declarator148"|"initializer190"|"postfix_expression8"|"cast_expression30"|"unary_expression22") as s, a, b, c, d) -> TUPLE5(s, truncate a, truncate b, truncate c, truncate d)
| TUPLE6(STRING ("selection_statement217"|"iteration_statement220") as s, a, b, c, d, e) -> TUPLE6(s, truncate a, truncate b, truncate c, truncate d, truncate e)
| TUPLE8(STRING ("iteration_statement223"|"selection_statement218") as s, a,b,c,d,e,f,g) -> TUPLE8 (s, truncate a, truncate b, truncate c, truncate d, truncate e, truncate f, truncate g)
(*
| TUPLE4(_, a, b, c) -> TUPLE3(truncate a, truncate b, truncate c)
| TUPLE5(_, a, b, c, d) -> TUPLE4(truncate a, truncate b, truncate c, truncate d)
| TUPLE6(_, a, b, c, d, e) -> TUPLE5(truncate a, truncate b, truncate c, truncate d, truncate e)
| TUPLE7(_, a,b,c,d,e,f) -> TUPLE6 (truncate a, truncate b, truncate c, truncate d, truncate e, truncate f)
*)
| TLIST lst -> TLIST (List.map truncate lst)
| ELIST lst -> ELIST (List.map truncate lst)
(*
| IDENTIFIER _ -> IDENTIFIER ""
| CONSTANT _ -> CONSTANT "1"
*)
  | XOR_ASSIGN
  | WHILE
  | VOLATILE
  | VOID
  | VBAR
  | UNSIGNED
  | UNION
  | UNDERSCORE
  | TYPEDEF
  | TILDE
  | SWITCH
  | SUB_ASSIGN
  | STRUCT
  | STRING_LITERAL _
  | STRING _
  | STATIC
  | STAR
  | SLIST _
  | SLASH
  | SIZEOF
  | SIGNED
  | SHORT
  | SEMICOLON
  | RPAREN
  | RIGHT_OP
  | RIGHT_ASSIGN
  | RETURN
  | RESTRICT
  | REGISTER
  | RBRACK
  | RBRACE
  | QUOTE
  | QUERY
  | PTR_OP
  | PLUS
  | PLING
  | PERCENT
  | OR_OP
  | OR_ASSIGN
  | NE_OP
  | MUL_ASSIGN
  | MOD_ASSIGN
  | LPAREN
  | LONG
  | LINEFEED
  | LE_OP
  | LESS
  | LEFT_OP
  | LEFT_ASSIGN
  | LBRACK
  | LBRACE
  | INT
  | INLINE
  | INC_OP
  | IMAGINARY
  | IF
  | HYPHEN
  | HASH
  | GREATER
  | GOTO
  | GE_OP
  | FOR
  | FLOAT
  | EXTERN
  | ERROR_TOKEN
  | ERROR
  | EQ_OP
  | EQUALS
  | EOF_TOKEN
  | ENUM
  | END
  | EMPTY_TOKEN
  | ELSE
  | ELLIPSIS
  | DOUBLEQUOTE
  | DOUBLE
  | DOT
  | DOLLAR
  | DO
  | DIV_ASSIGN
  | DEFAULT
  | DEC_OP
  | CONTINUE
  | CONST
  | CONS4 _
  | CONS3 _
  | CONS2 _
  | CONS1 _
  | COMPLEX
  | COMMA
  | COLON
  | CHAR
  | CASE
  | CARET
  | BREAK
  | BOOL
  | BACKSLASH
  | BACKQUOTE
  | AUTO
  | AT
  | AND_OP
  | AND_ASSIGN
  | AMPERSAND
  | ADD_ASSIGN
  | ACCEPT
| TYPE_NAME _
| IDENTIFIER _
| CONSTANT _ as tok -> tok
| oth -> otht := oth; failwith "truncate"

let getrslt parse arg =
   Printf.fprintf stderr "%s: " arg; flush stderr;
   dbgtree := truncate (parse arg);
   match !dbgtree with
    | TUPLE2(tran,_) -> 
        List.iter (fun itm ->
            nxtlst := itm :: !nxtlst;
            Translation_unit_list_filt.filt _enums _externs _fbody _ftypes _globals _inits _inlines _structs _typedefs _unions _others itm) (match tolst tran with TLIST lst -> lst | oth -> []);
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
"Types=%d, Fun=%d, ext=%d, enum=%d, struc=%d, union=%d, ftyp=%d, unrecog=%d, redef=%d\n"
(List.length !typlst)
(List.length !fnlst)
(List.length !extlst)
(List.length !enumlst)
(List.length !structlst)
(List.length !unionlst)
(List.length !ftyplst)
(List.length !redeflst);
flush stderr;
(!fnlst, !extlst, !enumlst, !structlst, !unionlst, !ftyplst, !typlst, !redeflst)
    | oth -> othlst := oth :: !othlst; Translation_unit_list_filt.failtree oth
									   
let emark' refs key k = function
  | IDENTIFIER e when e=key ->
    if not (List.mem k !(refs)) then
    begin
    if !verbose then prerr_endline ("****** "^k^" ******");
    refs := k :: !(refs)
    end
  | oth -> ()

let emark refs = function
  | IDENTIFIER key ->
    Hashtbl.iter (fun k x -> match x with
      | E (TUPLE3 (e, EQUALS, CONSTANT _)) -> emark' refs key k e
      | E (TLIST lst) -> List.iter (emark' refs key k) lst
      | oth -> ()) enums;
    false
  | oth -> false

let lmark refs = function
  | IDENTIFIER key ->
    if not (List.mem key !(refs)) then
      begin
      refs := key :: !(refs)
      end;
    false
  | oth -> false

let fmark refs fn =
  if not (List.mem fn !(refs)) then
    begin
    refs := fn :: !(refs)
    end

let smark strong refs str =
  if not (List.mem str !(refs)) then
    begin
    if strong then refs := str :: !(refs)
    end

let umark refs str =
  if not (List.mem str !(refs)) then
    begin
    refs := str :: !(refs)
    end

let tmark refs str =
  if not (List.mem str !(refs)) then
    begin
    refs := str :: !(refs)
    end

let rec is_enum id = function
      | TUPLE3 (IDENTIFIER e, EQUALS, CONSTANT n) -> id=e
      | TLIST lst -> List.fold_left (||) false (List.map (is_enum id) lst)
      | oth -> false

let rec is_int = function
| CONSTANT n -> (try let _ = int_of_string n in true with _ -> false)
| IDENTIFIER id -> let found = ref false in
    Hashtbl.iter (fun _ x -> match x with E x -> if is_enum id x then found := true) enums;
    !found
| TUPLE3 (LPAREN, cexpr, RPAREN) -> is_int cexpr
| TUPLE3 (lft, (PLUS|HYPHEN|STAR|SLASH), rght) when is_int lft && is_int rght -> true
| oth -> false

let rec cexpr_as_int = function
| CONSTANT n -> int_of_string n
| IDENTIFIER id -> let found = ref None in
    Hashtbl.iter (fun _ x -> match x with E x -> as_enum found id x) enums;
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
| BOOL -> "bool_t"
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
| TLIST lst -> String.concat "/* 351 */ " (List.map (dumptyp refs) lst)^" "
| oth -> "/* 357 */"^Translation_unit_list_filt.dumptree oth

let rec dumparg refs = function
| ELLIPSIS -> "..."
| TUPLE2(TYPE_NAME typ, IDENTIFIER arg) -> tmark refs typ; typ^" "^arg
| TUPLE2(TYPE_NAME typ, TUPLE2 (STAR, IDENTIFIER arg)) -> tmark refs typ; typ^" *"^arg
| TUPLE2(TUPLE2(CONST, TYPE_NAME typ), TUPLE2 (STAR, IDENTIFIER arg)) -> tmark refs typ; "const "^typ^" *"^arg
| TUPLE2((VOID|BOOL|CHAR|INT|DOUBLE) as typ, IDENTIFIER arg) -> dumptyp refs typ^" "^arg
| TUPLE2(TUPLE2(CONST, ((CHAR|INT|DOUBLE) as typ)), IDENTIFIER arg) -> "const "^dumptyp refs typ^" "^arg
| TUPLE2(TUPLE2(CONST, ((CHAR|INT|DOUBLE) as typ)), TUPLE2(STAR, RESTRICT)) -> "const "^dumptyp refs typ^" *"
| TUPLE2((VOID|BOOL|CHAR|INT|DOUBLE) as typ, TUPLE2 (STAR, IDENTIFIER arg)) -> dumptyp refs typ^" *"^arg
| TUPLE2(TUPLE2(CONST, ((CHAR|INT|DOUBLE) as typ)), TUPLE2 (STAR, IDENTIFIER arg)) -> "const "^dumptyp refs typ^" *"^arg
| TUPLE2((BOOL|CHAR|INT|DOUBLE) as typ, TUPLE2 (TUPLE2 (STAR, STAR), IDENTIFIER arg)) -> dumptyp refs typ^" **"^arg
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
    String.concat "/* 381 */;\n\t" (List.map (fun itm -> dumptyp refs typ^" "^dumpc refs itm) lst)^";"
| TUPLE3 ((VOID|BOOL|CHAR|INT|DOUBLE|TUPLE2 (CONST, CHAR)|TYPE_NAME _) as typ, ptr, SEMICOLON) -> dumptyp refs typ^" "^dumpc refs ptr^";"
| TUPLE3 (LPAREN, expr, RPAREN) -> "("^dumpc refs expr^")"
| TUPLE5 (expr, QUERY, expr1, COLON, expr2) -> dumpc refs expr^" ? "^dumpc refs expr1^" : "^dumpc refs expr2
| TUPLE4 (LPAREN, ((VOID|BOOL|CHAR|INT|DOUBLE) as typ), RPAREN, expr) -> "("^dumptyp refs typ^") "^dumpc refs expr
| TUPLE4 (LPAREN, (TUPLE2 ((VOID|BOOL|CHAR|INT|DOUBLE|TYPE_NAME _), STAR) as typ), RPAREN, expr) -> "("^dumptyp refs typ^") "^dumpc refs expr
| TUPLE3 (IDENTIFIER fn, LPAREN, RPAREN) -> fmark refs fn; fn^"()"
| TUPLE4 (IDENTIFIER fn, LPAREN, args, RPAREN) ->
    fmark refs fn;
    let _ = search (lmark refs) args in
    if fn <> "__assert_fail" then fn^"("^adump refs args^")" else "(void) (0)"
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
| TLIST lst -> String.concat "/* 417 */\n" (List.map (dumpc refs) lst)
| TUPLE2 (stmt, SEMICOLON) -> dumpc refs stmt^"; "
| TUPLE3 (lft, EQUALS, TUPLE3(LBRACE, TLIST lst, RBRACE)) ->
    let _ = search (lmark refs) lft in
    let _ = search (lmark refs) (TLIST lst) in
    dumpc refs lft^" = {"^String.concat "/* 422 */, " (List.map (dumpc refs) lst)^"}"
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
   "union { "^String.concat "/* 451 */\n\t" (List.map (dumpc refs) lst)^" } "^u^";"
| TUPLE4 (LPAREN, TYPE_NAME name_t, RPAREN, expr) -> "("^name_t^")"^dumpc refs expr
| TUPLE3 (TUPLE2 (LONG, LONG), TUPLE3 (IDENTIFIER lhs, EQUALS, rhs), SEMICOLON) ->
   "long long "^lhs^" = "^dumpc refs rhs^";"
| TUPLE3 (TUPLE4 (UNION, LBRACE, TLIST lst, RBRACE), TUPLE3 (IDENTIFIER u, EQUALS, expr), SEMICOLON) ->
   "union { "^String.concat "/* 456 */\n\t" (List.map (dumpc refs) lst)^" } "^u^" = "^dumpc refs expr^";"
| TUPLE3 (IDENTIFIER id, LBRACK, RBRACK) -> id^"[]"
| TUPLE5 (SWITCH, LPAREN, swexpr, RPAREN, TUPLE3 (LBRACE, TLIST lst, RBRACE)) ->
   "switch("^dumpc refs swexpr^") {"^String.concat "/* 459 */\n\t" (List.map (dumpc refs) lst)^" }"
| TUPLE4 (CASE, cexpr, COLON, stmt) -> "case "^dumpc refs cexpr^": "^dumpc refs stmt
| TUPLE3 (DEFAULT, COLON, stmt) -> "default: "^dumpc refs stmt
| TUPLE7 (DO, TUPLE3 (LBRACE, TLIST lst, RBRACE), WHILE, LPAREN, doexpr, RPAREN, SEMICOLON) ->
   "do { "^String.concat "/* 463 */\n\t" (List.map (dumpc refs) lst)^" } while ("^dumpc refs doexpr^");"
| TUPLE5 (WHILE, LPAREN, whexpr, RPAREN, TUPLE3 (LBRACE, TLIST lst, RBRACE)) ->
   "while ("^dumpc refs whexpr^") { "^String.concat "/* 465 */\n\t" (List.map (dumpc refs) lst)^" };"
| TUPLE6 (LPAREN, TYPE_NAME name_t, RPAREN, LBRACE, cexpr, RBRACE) ->
   "("^name_t^") {"^adump refs cexpr^"} "
| TUPLE2 (TUPLE2 (STAR, CONST), TUPLE3 (IDENTIFIER ty, LBRACK, RBRACK)) -> "* const "^ty^"[] "
| TUPLE2 (LBRACE, RBRACE) -> "{ }"
| TUPLE2 (TUPLE2 (DOT, IDENTIFIER field), EQUALS) -> "."^field^"="
| TUPLE4 (ENUM, LBRACE, TUPLE3 (IDENTIFIER id, EQUALS, init), RBRACE) -> "enum {"^id^"="^dumpc refs init^"}"
| oth -> "/* 416 */"^Translation_unit_list_filt.dumptree oth

and adump refs = function
  | TLIST lst -> String.concat "/* 474 */, " (List.map (dumpc refs) lst)
  | VOID -> ""
  | oth -> dumpc refs oth

and pdump refs = function
  | TLIST lst -> String.concat "/* 479 */, " (List.map (dumpc refs) lst)
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
    "enum "^str^"\n\t{\n\t"^String.concat "/* 494 */,\n\t" (List.map (function
    | IDENTIFIER field ->
       ignore (emark refs (IDENTIFIER field));
      field
    | oth -> "/* 473 */"^Translation_unit_list_filt.dumptree oth) fields)^"\n\t} "

let sdump' refs str fields =
  smark true refs str;
    "struct "^str^"\n\t{\n\t"^String.concat "/* 499 */\n\t" (List.map (function
      | TUPLE3 (TUPLE2 (STRUCT, IDENTIFIER str), TUPLE2 (STAR, IDENTIFIER field), SEMICOLON) ->
           smark false refs str; "struct "^str^" *"^field^";"
      | TUPLE3 (TUPLE2 (STRUCT, IDENTIFIER str), IDENTIFIER field, SEMICOLON) ->
           smark true refs str; "struct "^str^" "^field^";"
      | TUPLE3 (typ, TUPLE4 (TUPLE3 (LPAREN, TUPLE2 (STAR, IDENTIFIER fn), RPAREN), LPAREN, TLIST typlst, RPAREN), SEMICOLON) ->
           dumptyp refs typ^" (* "^fn^") ("^String.concat "/* 505 */, " (List.map (dumptyp refs) typlst)^");"
      | TUPLE3 (typ, TUPLE4 (TUPLE3 (LPAREN, TUPLE2 (STAR, IDENTIFIER fn), RPAREN), LPAREN, typ', RPAREN), SEMICOLON) ->
           dumptyp refs typ^" (* "^fn^") ("^dumptyp refs typ'^");"
      | TUPLE3 (typ, IDENTIFIER field, SEMICOLON) ->
           dumptyp refs typ^" "^field^";"
      | TUPLE3 (typ, TUPLE4 (IDENTIFIER field, LBRACK, cexpr, RBRACK), SEMICOLON) ->
           let _ = search (emark refs) cexpr in dumptyp refs typ^" "^field^"["^cdump refs cexpr^"];"
      | TUPLE3 (typ, TUPLE2 (STAR, IDENTIFIER field), SEMICOLON) ->
           dumptyp refs typ^" *"^field^";"
      | oth -> "/* 491 */"^Translation_unit_list_filt.dumptree oth) fields)^"\n\t} "

let udump' refs str fields =
  umark refs str;
    "union "^str^"\n\t{\n\t"^String.concat "/* 527 */\n\t" (List.map (function
      | TUPLE3 (typ, IDENTIFIER field, SEMICOLON) ->
           dumptyp refs typ^" "^field^";"
      | TUPLE3 (typ, TUPLE4 (IDENTIFIER field, LBRACK, cexpr, RBRACK), SEMICOLON) ->
           let _ = search (emark refs) cexpr in dumptyp refs typ^" "^field^"["^cdump refs cexpr^"];"
      | oth -> "/* 509 */"^Translation_unit_list_filt.dumptree oth) fields)^"\n\t} "

let bodylst = ref []
let bodylst' = ref []
let rec dump_body refs = function
| CONSTANT str -> str
| STRING_LITERAL str -> str
| TUPLE2 (HYPHEN, CONSTANT str) -> "-"^str
| TLIST [TUPLE2 (TUPLE2 (DOT, IDENTIFIER field), EQUALS); CONSTANT num] -> "."^field^"="^num    
| TLIST (TUPLE2 (TUPLE2 (DOT, IDENTIFIER field), EQUALS) ::
        TUPLE3 (LBRACE, TLIST lst, RBRACE) :: []) ->
     "."^field^"={"^String.concat "/* 594 */,\n\t" (List.map (dump_body refs) lst)^" }"
| TUPLE2 (TUPLE2 (DOT, IDENTIFIER field), EQUALS) -> "."^field^"= "
| TUPLE2 (TUPLE2 (TUPLE2 (DOT, IDENTIFIER field), EQUALS), rslt) -> "."^field^"= "^dump_body refs rslt
| TUPLE3 (LBRACE, TLIST
    (TUPLE2 (TUPLE2 (DOT, IDENTIFIER field), EQUALS) :: TUPLE3 (LBRACE, TLIST clst, RBRACE) :: []), RBRACE) ->
        "{."^field^"={"^String.concat "/* 599 */,\n\t" (List.map (dump_body refs) clst)^" }}"
| TUPLE3 (LBRACE, TLIST lst, RBRACE) -> "{"^String.concat "/* 600 */,\n\t" (List.map (dump_body refs) lst)^"}"
| TUPLE3 (LBRACE, itm, RBRACE) -> dump_body refs (TUPLE3 (LBRACE, TLIST [itm], RBRACE))
| TUPLE2 (SIZEOF, TUPLE3 (LPAREN, (IDENTIFIER id as l), RPAREN)) ->
    let _ = lmark refs l in "sizeof ("^id^")"
| TUPLE2 (SIZEOF, TUPLE3 (LPAREN, TUPLE2 (STAR, (IDENTIFIER id as l)), RPAREN)) ->
    let _ = lmark refs l in "sizeof (*"^id^")"
| TLIST lst -> bodylst' := lst :: !bodylst';
    "{\n\t"^String.concat "/* 607 */,\n\t" (List.map (dump_body refs) lst)^"\n\t}"
| cexpr when is_int cexpr -> string_of_int (cexpr_as_int cexpr)
| TUPLE3 (lft, PLUS, rght) -> dump_body refs lft^"+"^dump_body refs rght
| TUPLE3 (lft, HYPHEN, rght) -> dump_body refs lft^"-"^dump_body refs rght
| TUPLE3 (lft, STAR, rght) -> dump_body refs lft^"*"^dump_body refs rght
| TUPLE3 (lft, SLASH, rght) -> dump_body refs lft^"/"^dump_body refs rght
| TUPLE3 (lft, PERCENT, rght) -> dump_body refs lft^"%"^dump_body refs rght
| body -> bodylst := body :: !bodylst; failwith "bodylst"

let idumplst = ref []
let rec alldump refs key = function
  | I (typ,body) ->
    (if !verbose2 then "/* 571 "^Translation_unit_list_filt.dumptree typ^" */" else ""^match typ with
      | TYPE_NAME id_t -> tmark refs id_t; id_t
      | TUPLE2 (DOUBLE, STAR) -> dumptyp refs typ
      | (INT|UNSIGNED|DOUBLE) -> dumptyp refs typ
      | oth -> "/* 595 */"^Translation_unit_list_filt.dumptree oth)^" "^key^"="^dump_body refs body^";\n"
  | G typ ->
    (if !verbose2 then "/* 577 "^Translation_unit_list_filt.dumptree typ^" */" else ""^match typ with
      | TYPE_NAME id_t -> tmark refs id_t; id_t^" "^key^";\n"
      | TUPLE2 (DOUBLE, STAR) -> dumptyp refs typ^" "^key^";\n"
      | TUPLE4 (IDENTIFIER array, LBRACK, cexpr, RBRACK) when is_int cexpr ->
          array^"["^string_of_int (cexpr_as_int cexpr)^"]"
      | TUPLE2 (typ, TUPLE4 (IDENTIFIER array, LBRACK, cexpr, RBRACK)) ->
          dumptyp refs typ^" "^array^"["^adump refs cexpr^"];"
      | TUPLE2 (TUPLE2 (EXTERN, TUPLE2 (STRUCT, TYPE_NAME id_t)), STAR) -> "extern struct "^id_t^" *"^key^";\n"
      | TUPLE3 (typ, TUPLE3 (IDENTIFIER array, LBRACK, RBRACK), TLIST lst) ->
	 dumptyp refs typ^" "^array^"[] = {"^String.concat "/* 644 */,\n\t" (List.map (function
					   | CONSTANT str -> str
					   | oth -> Translation_unit_list_filt.dumptree oth) lst)^"};"
      | TUPLE2 (typ, TUPLE3 (TUPLE4 (IDENTIFIER array, LBRACK, CONSTANT len, RBRACK), EQUALS, init)) ->
	 dumptyp refs typ^" "^array^"[] = "^dump_body refs init^";\n"
      | TUPLE2 (typ, TUPLE3 (IDENTIFIER id, EQUALS, init)) -> dumptyp refs typ^" "^id^"[] = {"^dumpc refs init^"};"
      | TUPLE2 (typ, TLIST lst) -> String.concat "\n" (List.map (fun itm -> dumptyp refs typ^" "^adump refs itm^";") lst)
      | typ -> idumplst := typ :: !idumplst; "/* 647 */"^dumptyp refs typ^" "^key^";\n")
   | B (typ,paramlst,body) ->
      (if !verbose2 then "/* 598 "^Translation_unit_list_filt.dumptree typ^" */" else ""^"\n"^dumptyp refs typ^" "^key^"("^String.concat "/* 667 */, " (List.map (dumparg refs) paramlst)^")\n{")^
    String.concat "/* 668 */\n" (List.map (fun itm -> (dumpc refs itm)) body)^"}\n"
   | L (typ,paramlst,body) ->
      (if !verbose2 then "/* 601 "^Translation_unit_list_filt.dumptree typ^" */" else ""^"\n"^dumptyp refs typ^" "^key^"("^String.concat "/* 673 */, " (List.map (dumparg refs) paramlst)^")\n{")^
    String.concat "/* 674 */\n" (List.map (fun itm -> (dumpc refs itm)) body)^"}\n"
   | F (typ,paramlst) ->
      (if !verbose2 then "/* 604 "^Translation_unit_list_filt.dumptree typ^" */" else ""^"\n"^dumptyp refs typ^" "^key^"("^String.concat "/* 679 */, " (List.map (dumparg refs) paramlst)^");\n")
   | E body->
      (if !verbose2 then "/* 571 "^Translation_unit_list_filt.dumptree body^" */" else ""^match body with
      | TUPLE5 (ENUM, IDENTIFIER str, LBRACE, TLIST fields, RBRACE) -> (edump' refs str fields)
      | TUPLE3 (IDENTIFIER e, EQUALS, CONSTANT n) -> "enum "^key^" {"^e^"="^n^"};\n"
      | TUPLE2 (STRUCT, TYPE_NAME nam) -> "struct "^nam^";\n"
      | TUPLE2 (TYPE_NAME typ, IDENTIFIER id) -> tmark refs typ; typ^" "^id^";\n"
      | TYPE_NAME nam -> nam^";\n"
      | INT -> "int"
      | oth -> ("/* 528 */"^Translation_unit_list_filt.dumptree oth))
   | S fields -> sdump' refs key fields^";\n"
   | T body ->
      (if !verbose2 then "/* 616 "^Translation_unit_list_filt.dumptree body^" */" else ""^match body with
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
          ("typedef "^dumptyp refs typ^" "^key^";\n"))
   | U fields -> udump' refs key fields^";\n"

let rec getrefs refs tab = function
  | [] -> ()
  | itm :: reclst ->
     if !verbose2 then prerr_endline (string_of_int (List.length reclst)^": getrefs: "^itm);
      begin
	List.iter (fun entry ->
		   if not (hashmem tab itm) then
		     begin
		       let itm_refs = ref [] in
		       let src = alldump itm_refs itm entry in
		       let deps = !(itm_refs) in
		       List.iter (fun itm' -> 
				  if not (List.mem itm' (itm :: reclst)) then
				    begin
				      if !verbose2 then prerr_endline ("iter: "^itm);
				      getrefs itm_refs tab (itm' :: itm :: reclst)
				    end
				 ) deps;
		       let deps' = !(itm_refs) in
		       Hashtbl.add tab itm (deps',src);
		       List.iter (fun itm -> if not (List.mem itm !(refs)) then
					       refs := itm :: !(refs)) !(itm_refs)
		     end
	      ) (lookup itm)
      end
	
let rec print_uniq chan printed tab str (deps,itm) =
    begin
      List.iter (fun itm ->
		 if !verbose2 then prerr_endline itm;
		 if itm <> str && Hashtbl.mem tab itm then print_uniq chan printed tab itm (Hashtbl.find tab itm)
		) deps;
      if not (Hashtbl.mem printed str) then
	begin
	  Hashtbl.add printed str itm;
	  if !verbose then
            output_string chan ("/* "^str^" ["^String.concat ";" deps^"] */\n"^itm)
	  else
            output_string chan itm
	end    
    end

let dump parse chan main argv =
  let rslts = ref [] in
  prerr_endline "/*";
  for i = 1 to Array.length argv - 1 do rslts := getrslt parse argv.(i) :: !rslts; done;
  prerr_endline "*/";
  let refs = ref [] in
  let tab = Hashtbl.create 257 in
  getrefs refs tab [main];
  let printed = Hashtbl.create 257 in
  output_string chan ("typedef enum {false,true} bool_t;\n");
  Hashtbl.iter (print_uniq chan printed tab) tab;
  tab, printed, !rslts
