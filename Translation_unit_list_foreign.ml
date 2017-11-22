
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

let rec is_enum id = function
      | TUPLE3 (IDENTIFIER e, EQUALS, CONSTANT n) -> id=e
      | TLIST lst -> List.fold_left (||) false (List.map (is_enum id) lst)
      | oth -> false

let rec is_int = function
| CONSTANT n -> (try let _ = int_of_string n in true with _ -> false)
| IDENTIFIER id -> let found = ref false in
    Hashtbl.iter (fun _ x -> if is_enum id x then found := true) Translation_unit_list_transform.enums;
    !found
| TUPLE3 (LPAREN, cexpr, RPAREN) -> is_int cexpr
| TUPLE3 (lft, (PLUS|HYPHEN|STAR|SLASH), rght) when is_int lft && is_int rght -> true
| oth -> false

let rec cexpr_as_int = function
| CONSTANT n -> int_of_string n
| IDENTIFIER id -> let found = ref None in
    Hashtbl.iter (fun _ x -> as_enum found id x) Translation_unit_list_transform.enums;
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
| TYPE_NAME nam -> nam^" "
| TLIST lst -> String.concat " " (List.map (dumptyp refs) lst)^" "
| oth -> "/* 357 */"^Translation_unit_list_filt.dumptree oth

let rec dumparg refs = function
| ELLIPSIS -> "..."
| TUPLE2(TYPE_NAME typ, IDENTIFIER arg) -> typ^" "^arg
| TUPLE2(TYPE_NAME typ, TUPLE2 (STAR, IDENTIFIER arg)) -> typ^" *"^arg
| TUPLE2(TUPLE2(CONST, TYPE_NAME typ), TUPLE2 (STAR, IDENTIFIER arg)) -> "const "^typ^" *"^arg
| TUPLE2((VOID|CHAR|INT|DOUBLE) as typ, IDENTIFIER arg) -> dumptyp refs typ^" "^arg
| TUPLE2(TUPLE2(CONST, ((CHAR|INT|DOUBLE) as typ)), IDENTIFIER arg) -> "const "^dumptyp refs typ^" "^arg
| TUPLE2(TUPLE2(CONST, ((CHAR|INT|DOUBLE) as typ)), TUPLE2(STAR, RESTRICT)) -> "const "^dumptyp refs typ^" *"
| TUPLE2((VOID|CHAR|INT|DOUBLE) as typ, TUPLE2 (STAR, IDENTIFIER arg)) -> dumptyp refs typ^" *"^arg
| TUPLE2(TUPLE2(CONST, ((CHAR|INT|DOUBLE) as typ)), TUPLE2 (STAR, IDENTIFIER arg)) -> "const "^dumptyp refs typ^" *"^arg
| TUPLE2((CHAR|INT|DOUBLE) as typ, TUPLE2 (TUPLE2 (STAR, STAR), IDENTIFIER arg)) -> dumptyp refs typ^" **"^arg
| TUPLE2((VOID|CHAR|INT|DOUBLE) as typ, (TUPLE3 (IDENTIFIER _, LBRACK, RBRACK) as array)) -> dumptyp refs typ^" "^dumptyp refs array
| TUPLE2(TUPLE2(CONST, ((CHAR|INT|DOUBLE) as typ)), STAR) -> "const "^dumptyp refs typ^" *"
| TUPLE2(TUPLE2(UNSIGNED, INT) as typ, IDENTIFIER arg) -> dumptyp refs typ^" "^arg
| TUPLE2(ty, p) -> "/* 339 */"^dumptyp refs ty^" "^Translation_unit_list_filt.dumptree p
| ty -> dumptyp refs ty

let rec dumpc refs = function
| CONSTANT num -> num
| IDENTIFIER id -> id
| STRING_LITERAL str -> str
| TUPLE2 (STAR, IDENTIFIER ptr) -> " *"^ptr
| TUPLE2 (RETURN, SEMICOLON) -> "return;"
| TUPLE2 (CONTINUE, SEMICOLON) -> "continue;"
| TUPLE3 ((VOID|CHAR|INT|DOUBLE|TUPLE2 (CONST, CHAR)|TYPE_NAME _) as typ, TLIST lst, SEMICOLON) ->
    String.concat ";\n\t" (List.map (fun itm -> dumptyp refs typ^" "^dumpc refs itm) lst)^";"
| TUPLE3 ((VOID|CHAR|INT|DOUBLE|TUPLE2 (CONST, CHAR)|TYPE_NAME _) as typ, ptr, SEMICOLON) -> dumptyp refs typ^" "^dumpc refs ptr^";"
| TUPLE3 (LPAREN, expr, RPAREN) -> "("^dumpc refs expr^")"
| TUPLE5 (expr, QUERY, expr1, COLON, expr2) -> dumpc refs expr^" ? "^dumpc refs expr1^" : "^dumpc refs expr2
| TUPLE4 (LPAREN, ((VOID|CHAR|INT|DOUBLE) as typ), RPAREN, expr) -> "("^dumptyp refs typ^") "^dumpc refs expr
| TUPLE4 (LPAREN, (TUPLE2 ((VOID|CHAR|INT|DOUBLE|TYPE_NAME _), STAR) as typ), RPAREN, expr) -> "("^dumptyp refs typ^") "^dumpc refs expr
| TUPLE3 (IDENTIFIER fn, LPAREN, RPAREN) -> fn^"()"
| TUPLE4 (IDENTIFIER fn, LPAREN, args, RPAREN) ->
    fn^"("^adump refs args^")"
| TUPLE4 (arr, LBRACK, expr, RBRACK) -> dumpc refs arr^"["^adump refs expr^"]"
| TUPLE2 (HYPHEN, rght) -> "-"^dumpc refs rght
| TUPLE2 (PLING, rght) -> "!"^dumpc refs rght
| TUPLE2 (STAR, rght) -> "*"^dumpc refs rght
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
| TUPLE5 (IF, LPAREN, cond, RPAREN, then') -> "if ("^dumpc refs cond^") "^dumpc refs then'
| TUPLE3 (LBRACE, body, RBRACE) -> "\n\t{\n\t"^dumpc refs body^"\n\t}\n"
| TLIST lst -> String.concat "\n" (List.map (dumpc refs) lst)
| TUPLE2 (stmt, SEMICOLON) -> dumpc refs stmt^"; "
| TUPLE3 (lft, EQUALS, TUPLE3(LBRACE, TLIST lst, RBRACE)) ->
    dumpc refs lft^" = {"^String.concat ", " (List.map (dumpc refs) lst)^"}"
| TUPLE3 (lft, EQUALS, expr) ->
    dumpc refs lft^" = "^dumpc refs expr
| TUPLE3 (IDENTIFIER str, DOT, IDENTIFIER memb) -> str^"."^memb
| TUPLE3 (IDENTIFIER str, PTR_OP, IDENTIFIER memb) -> str^"->"^memb
| TUPLE4 (SIZEOF, LPAREN, typ, RPAREN) -> "sizeof("^dumptyp refs typ^")"
| TUPLE2 (AMPERSAND, compound) -> "&"^dumpc refs compound
| TUPLE7 (FOR, LPAREN, TUPLE3 (typ, initial, SEMICOLON), TUPLE2 (condition, SEMICOLON), inc, RPAREN, body) -> "for ("^dumptyp refs typ^" "^dumpc refs initial^"; "^dumpc refs condition^"; "^dumpc refs inc^") "^dumpc refs body
| TUPLE7 (FOR, LPAREN, TUPLE2 (initial, SEMICOLON), TUPLE2 (condition, SEMICOLON), inc, RPAREN, body) -> "for ( "^dumpc refs initial^"; "^dumpc refs condition^"; "^dumpc refs inc^") "^dumpc refs body
| TUPLE7 (FOR, LPAREN, SEMICOLON, TUPLE2 (condition, SEMICOLON), inc, RPAREN, TUPLE3 (LBRACE, body, RBRACE)) -> "for ( ; "^dumpc refs condition^"; "^dumpc refs inc^") "^dumpc refs body

| TUPLE7 (IF, LPAREN, expr, RPAREN, then', ELSE, else') -> "if ("^dumpc refs expr^") "^dumpc refs then'^" else "^dumpc refs else'
| TUPLE2 (lvalue, INC_OP) -> dumpc refs lvalue^"++"
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
| oth -> "/* 416 */"^Translation_unit_list_filt.dumptree oth

and adump refs = function
| TLIST lst -> String.concat ", " (List.map (dumpc refs) lst)
| VOID -> ""
| oth -> dumpc refs oth

and pdump refs = function
| TLIST lst -> String.concat ", " (List.map (dumpc refs) lst)
| VOID -> ""
| oth -> dumpc refs oth

let simplify = try let _ = Sys.getenv "SIMPLIFY_CEXPR" in true with _ -> true

let rec cdump refs cexpr =
if simplify && is_int cexpr then
    (let rslt = cexpr_as_int cexpr in
    string_of_int rslt)
else dumpc refs cexpr

let edump' refs str fields =
    "enum "^str^"\n\t{\n\t"^String.concat ",\n\t" (List.map (function
      | IDENTIFIER field -> field
      | oth -> "/* 473 */"^Translation_unit_list_filt.dumptree oth) fields)^"\n\t} "

let sdump' refs str fields =
    "struct "^str^"\n\t{\n\t"^String.concat "\n\t" (List.map (function
      | TUPLE3 (TUPLE2 (STRUCT, IDENTIFIER str), TUPLE2 (STAR, IDENTIFIER field), SEMICOLON) ->
           "struct "^str^" *"^field^";"
      | TUPLE3 (TUPLE2 (STRUCT, IDENTIFIER str), IDENTIFIER field, SEMICOLON) ->
           "struct "^str^" "^field^";"
      | TUPLE3 (typ, TUPLE4 (TUPLE3 (LPAREN, TUPLE2 (STAR, IDENTIFIER fn), RPAREN), LPAREN, TLIST typlst, RPAREN), SEMICOLON) ->
           dumptyp refs typ^" (* "^fn^") ("^String.concat ", " (List.map (dumptyp refs) typlst)^");"
      | TUPLE3 (typ, TUPLE4 (TUPLE3 (LPAREN, TUPLE2 (STAR, IDENTIFIER fn), RPAREN), LPAREN, typ', RPAREN), SEMICOLON) ->
           dumptyp refs typ^" (* "^fn^") ("^dumptyp refs typ'^");"
      | TUPLE3 (typ, IDENTIFIER field, SEMICOLON) ->
           dumptyp refs typ^" "^field^";"
      | TUPLE3 (typ, TUPLE4 (IDENTIFIER field, LBRACK, cexpr, RBRACK), SEMICOLON) ->
           dumptyp refs typ^" "^field^"["^cdump refs cexpr^"];"
      | TUPLE3 (typ, TUPLE2 (STAR, IDENTIFIER field), SEMICOLON) ->
           dumptyp refs typ^" *"^field^";"
      | oth -> "/* 491 */"^Translation_unit_list_filt.dumptree oth) fields)^"\n\t} "

let sdump refs key =
  if Hashtbl.mem Translation_unit_list_transform.structs key then
    begin
    let fields = Hashtbl.find Translation_unit_list_transform.structs key in sdump' refs key fields^";\n"
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
           dumptyp refs typ^" "^field^"["^cdump refs cexpr^"];"
      | oth -> "/* 509 */"^Translation_unit_list_filt.dumptree oth) fields)^"\n\t} "

let udump refs key =
  if Hashtbl.mem Translation_unit_list_transform.unions key then
    begin
    let fields = Hashtbl.find Translation_unit_list_transform.unions key in udump' refs key fields^";\n"
    end
  else
    begin
    ("union "^key^";");
    end

let edump refs key =
  if Hashtbl.mem Translation_unit_list_transform.enums key then
    begin
    let body = Hashtbl.find Translation_unit_list_transform.enums key in
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
  if Hashtbl.mem Translation_unit_list_transform.typedefs key then
    begin
    let body = Hashtbl.find Translation_unit_list_transform.typedefs key in
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
      | TUPLE2 (STRUCT, IDENTIFIER str) ->
           ("typedef struct "^str^" "^key^";\n")
      | TUPLE2 (UNION, IDENTIFIER str) ->
           ("typedef union "^str^" "^key^";\n")
      | typ ->
           ("typedef "^dumptyp refs typ^" "^key^";\n")
    end
  else
    begin
    ("struct "^key^";\n");
    end

let rec dump_body refs = function
| CONSTANT str -> str
| TUPLE2 (HYPHEN, CONSTANT str) -> "-"^str
| TLIST [TUPLE2 (TUPLE2 (DOT, IDENTIFIER field), EQUALS); CONSTANT num] -> "."^field^"="^num    
| TLIST (TUPLE2 (TUPLE2 (DOT, IDENTIFIER field), EQUALS) ::
        TUPLE3 (LBRACE, TLIST lst, RBRACE) :: []) ->
     "."^field^"={"^String.concat ",\n\t" (List.map (dump_body refs) lst)^" }"
| TUPLE2 (TUPLE2 (TUPLE2 (DOT, IDENTIFIER field), EQUALS), rslt) -> "."^field^"= "^dump_body refs rslt
| TUPLE3 (LBRACE, TLIST
    (TUPLE2 (TUPLE2 (DOT, IDENTIFIER field), EQUALS) :: TUPLE3 (LBRACE, TLIST clst, RBRACE) :: []), RBRACE) ->
        "{."^field^"={"^String.concat ",\n\t" (List.map (dump_body refs) clst)^" }}"
| TUPLE3 (LBRACE, TLIST lst, RBRACE) -> "{"^String.concat ",\n\t" (List.map (dump_body refs) lst)^"}"
| TUPLE3 (LBRACE, itm, RBRACE) -> dump_body refs (TUPLE3 (LBRACE, TLIST [itm], RBRACE))
| TUPLE2 (SIZEOF, TUPLE3 (LPAREN, IDENTIFIER id, RPAREN)) ->
    "sizeof ("^id^")"
| TUPLE2 (SIZEOF, TUPLE3 (LPAREN, TUPLE2 (STAR, IDENTIFIER id), RPAREN)) ->
    "sizeof (*"^id^")"
| TLIST lst -> "{\n\t"^String.concat ",\n\t" (List.map (dump_body refs) lst)^"\n\t}"
| cexpr when is_int cexpr -> string_of_int (cexpr_as_int cexpr)
| TUPLE3 (lft, PLUS, rght) -> dump_body refs lft^"+"^dump_body refs rght
| TUPLE3 (lft, HYPHEN, rght) -> dump_body refs lft^"-"^dump_body refs rght
| TUPLE3 (lft, STAR, rght) -> dump_body refs lft^"*"^dump_body refs rght
| TUPLE3 (lft, SLASH, rght) -> dump_body refs lft^"/"^dump_body refs rght
| TUPLE3 (lft, PERCENT, rght) -> dump_body refs lft^"%"^dump_body refs rght
| body -> failwith "bodylst"

let idump refs key =
  if Hashtbl.mem Translation_unit_list_transform.inits key then
    begin
    let typ,body = Hashtbl.find Translation_unit_list_transform.inits key in
    (match typ with
      | TYPE_NAME id_t -> id_t^" "^key^"="^dump_body refs body^";\n"
      | TUPLE2 (DOUBLE, STAR) -> dumptyp refs typ^" "^key^"="^dump_body refs body^";\n"
      | INT -> dumptyp refs typ^" "^key^"="^dump_body refs body^";\n"
      | DOUBLE -> dumptyp refs typ^" "^key^"["^cdump refs body^"]="^dump_body refs body^";\n"
      | oth -> "/* 595 */"^Translation_unit_list_filt.dumptree oth);
    end
  else if Hashtbl.mem Translation_unit_list_transform.globals key then
    begin
    let typ = Hashtbl.find Translation_unit_list_transform.globals key in
    (match typ with
      | TYPE_NAME id_t -> id_t^" "^key^";\n"
      | TUPLE2 (DOUBLE, STAR) -> dumptyp refs typ^" "^key^";\n"
      | TUPLE4 (IDENTIFIER array, LBRACK, cexpr, RBRACK) when is_int cexpr ->
          array^"["^string_of_int (cexpr_as_int cexpr)^"]"
      | TUPLE2 (typ, TUPLE4 (IDENTIFIER array, LBRACK, cexpr, RBRACK)) ->
          dumptyp refs typ^" "^array^"["^dumpc refs cexpr^"];"
      | TUPLE2 (TUPLE2 (EXTERN, TUPLE2 (STRUCT, TYPE_NAME id_t)), STAR) -> "extern struct "^id_t^" *"^key^";\n"
      | typ -> dumptyp refs typ^" "^key^";\n")
    end
  else "//\n"

let fdump refs key =
  if Hashtbl.mem Translation_unit_list_transform.fbody key then
    begin
    let (typ,paramlst,body) = Hashtbl.find Translation_unit_list_transform.fbody key in
    ("\n"^dumptyp refs typ^" "^key^"("^String.concat ", " (List.map (dumparg refs) paramlst)^")\n{")^
    String.concat "\n" (List.map (fun itm -> (dumpc refs itm)) body)^"}\n";
    end
  else if Hashtbl.mem Translation_unit_list_transform.inlines key then
    begin
    let (typ,paramlst,body) = Hashtbl.find Translation_unit_list_transform.inlines key in
    ("\n"^dumptyp refs typ^" "^key^"("^String.concat ", " (List.map (dumparg refs) paramlst)^")\n{")^
    String.concat "\n" (List.map (fun itm -> (dumpc refs itm)) body)^"}\n";
    end
  else if Hashtbl.mem Translation_unit_list_transform.ftypes key then
    begin
    let (typ,paramlst) = Hashtbl.find Translation_unit_list_transform.ftypes key in
    ("\n"^dumptyp refs typ^" "^key^"("^String.concat ", " (List.map (dumparg refs) paramlst)^");\n");
    end
  else "// fdump "^key^"\n"

let dump' refs (dump,ch,lev,str) = match ch with
  | 'e' -> edump refs str
  | 't' -> tdump refs str
  | 'u' -> udump refs str
  | 's' -> sdump refs str
  | 'i' -> idump refs str
  | 'f' -> fdump refs str
  | oth -> failwith ("dump' "^String.make 1 oth)

let dump parse chan needed =
  let refs =  Translation_unit_list_transform.frefs() in
  List.iter (fun itm -> output_string chan (dump' refs itm)) needed
