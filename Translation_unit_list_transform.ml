
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
	   
let verbose = ref true

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
let levels = Hashtbl.create 257
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

let level key =
  List.fold_left max 0 (Hashtbl.find_all levels key)

let setlevel key lev =
  Hashtbl.add levels key (List.fold_left max lev (Hashtbl.find_all levels key))

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
"Types=%d, Fun=%d, ext=%d, enum=%d, struc=%d, union=%d, ftyp=%d, unrecog=%d, redef=%d\n"
(List.length !typlst)
(List.length !fnlst)
(List.length !extlst)
(List.length !enumlst)
(List.length !structlst)
(List.length !unionlst)
(List.length !ftyplst)
(List.length !errlst)
(List.length !redeflst);
flush stderr;
(!fnlst, !extlst, !enumlst, !structlst, !unionlst, !ftyplst, !typlst, !errlst, !redeflst)
    | oth -> othlst := oth :: !othlst; Translation_unit_list_filt.failtree oth

type deps = {
  blst: string list;
  elst: string list;
  flst: string list;
  glst: string list;
  ilst: string list;
  llst: string list;
  slst: string list;
  tlst: string list;
  ulst: string list;
  }

type drefs = {
  brefs: string list ref;
  erefs: string list ref;
  frefs: string list ref;
  grefs: string list ref;
  irefs: string list ref;
  lrefs: string list ref;
  srefs: string list ref;
  trefs: string list ref;
  urefs: string list ref;
  btab: (string,int*deps*string) Hashtbl.t;
  etab: (string,int*deps*string) Hashtbl.t;
  ftab: (string,int*deps*string) Hashtbl.t;
  gtab: (string,int*deps*string) Hashtbl.t;
  itab: (string,int*deps*string) Hashtbl.t;
  ltab: (string,int*deps*string) Hashtbl.t;
  stab: (string,int*deps*string) Hashtbl.t;
  ttab: (string,int*deps*string) Hashtbl.t;
  utab: (string,int*deps*string) Hashtbl.t;
}

let empty_refs () = {brefs=ref [];erefs=ref [];frefs=ref [];grefs=ref [];
		irefs=ref [];lrefs=ref [];srefs=ref [];trefs=ref [];urefs=ref [];
		btab=Hashtbl.create 257;etab=Hashtbl.create 257;ftab=Hashtbl.create 257;gtab=Hashtbl.create 257;
		itab=Hashtbl.create 257;ltab=Hashtbl.create 257;stab=Hashtbl.create 257;
		ttab=Hashtbl.create 257;utab=Hashtbl.create 257}

let typerrlst = ref []
let markfile = open_out "markfile"
  
let emark' lev refs key k = function
  | IDENTIFIER e when e=key ->
    setlevel key lev;
    if not (List.mem k !(refs.erefs)) then
    begin
    if !verbose then prerr_endline ("****** "^k^" ******");
    refs.erefs := k :: !(refs.erefs)
    end
  | oth -> ()

let emark lev refs = function
  | IDENTIFIER key ->
    Hashtbl.iter (fun k x -> match x with
      | E (TUPLE3 (e, EQUALS, CONSTANT _)) -> emark' lev refs key k e
      | E (TLIST lst) -> List.iter (emark' lev refs key k) lst
      | oth -> ()) enums;
    false
  | oth -> false

let lmark lev refs = function
  | IDENTIFIER key ->
    setlevel key lev;
    if not (List.mem key !(refs.irefs)) then
      begin
      refs.irefs := key :: !(refs.irefs)
      end;
    false
  | oth -> false

let fmark lev refs fn =
  setlevel fn lev;
  if not (List.mem fn !(refs.frefs)) then
    begin
    refs.frefs := fn :: !(refs.frefs)
    end

let smark lev' strong refs str =
  let lev = lev' + 1 in
  setlevel str lev;
  if not (List.mem str !(refs.srefs)) then
    begin
    if strong then refs.srefs := str :: !(refs.srefs)
    end

let umark lev refs str =
  setlevel str lev;
  if not (List.mem str !(refs.urefs)) then
    begin
    refs.urefs := str :: !(refs.urefs)
    end

let tmark lev' refs str =
  let lev = lev' + 2 in
  setlevel str lev;
  output_string markfile ("tmark "^str^":"^string_of_int lev^"\n");
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

let rec dumptyp lev refs = function
| DOUBLE -> "double"
| FLOAT -> "float"
| CHAR -> "char"
| BOOL -> "bool"
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
| TUPLE2 (typ,typ') -> dumptyp lev refs typ^" "^dumptyp lev refs typ'
| TYPE_NAME nam -> tmark lev refs nam; nam^" "
| TLIST lst -> String.concat "/* 351 */ " (List.map (dumptyp lev refs) lst)^" "
| oth -> typerrlst := oth :: !typerrlst; "/* 357 */"^Translation_unit_list_filt.dumptree oth

let rec dumparg lev refs = function
| ELLIPSIS -> "..."
| TUPLE2(TYPE_NAME typ, IDENTIFIER arg) -> tmark lev refs typ; typ^" "^arg
| TUPLE2(TYPE_NAME typ, TUPLE2 (STAR, IDENTIFIER arg)) -> tmark lev refs typ; typ^" *"^arg
| TUPLE2(TUPLE2(CONST, TYPE_NAME typ), TUPLE2 (STAR, IDENTIFIER arg)) -> tmark lev refs typ; "const "^typ^" *"^arg
| TUPLE2((VOID|CHAR|INT|DOUBLE) as typ, IDENTIFIER arg) -> dumptyp lev refs typ^" "^arg
| TUPLE2(TUPLE2(CONST, ((CHAR|INT|DOUBLE) as typ)), IDENTIFIER arg) -> "const "^dumptyp lev refs typ^" "^arg
| TUPLE2(TUPLE2(CONST, ((CHAR|INT|DOUBLE) as typ)), TUPLE2(STAR, RESTRICT)) -> "const "^dumptyp lev refs typ^" *"
| TUPLE2((VOID|CHAR|INT|DOUBLE) as typ, TUPLE2 (STAR, IDENTIFIER arg)) -> dumptyp lev refs typ^" *"^arg
| TUPLE2(TUPLE2(CONST, ((CHAR|INT|DOUBLE) as typ)), TUPLE2 (STAR, IDENTIFIER arg)) -> "const "^dumptyp lev refs typ^" *"^arg
| TUPLE2((CHAR|INT|DOUBLE) as typ, TUPLE2 (TUPLE2 (STAR, STAR), IDENTIFIER arg)) -> dumptyp lev refs typ^" **"^arg
| TUPLE2((VOID|CHAR|INT|DOUBLE) as typ, (TUPLE3 (IDENTIFIER _, LBRACK, RBRACK) as array)) -> dumptyp lev refs typ^" "^dumptyp lev refs array
| TUPLE2(TUPLE2(CONST, ((CHAR|INT|DOUBLE) as typ)), STAR) -> "const "^dumptyp lev refs typ^" *"
| TUPLE2(TUPLE2(UNSIGNED, INT) as typ, IDENTIFIER arg) -> dumptyp lev refs typ^" "^arg
| TUPLE2(CHAR, TUPLE2(STAR, (TUPLE3 (IDENTIFIER _, LBRACK, RBRACK) as array))) -> "char * "^dumptyp lev refs array
| TUPLE2(ty, p) -> "/* 339 */"^dumptyp lev refs ty^" "^Translation_unit_list_filt.dumptree p
| ty -> dumptyp lev refs ty

let rec dumpc lev refs = function
| CONSTANT num -> num
| IDENTIFIER id as s -> let _ = lmark lev refs s in id
| STRING_LITERAL str -> str
| TUPLE2 (STAR, IDENTIFIER ptr) -> " *"^ptr
| TUPLE2 (TILDE, expr) -> " ~"^dumpc lev refs expr
| TUPLE2 (RETURN, SEMICOLON) -> "return;"
| TUPLE2 (CONTINUE, SEMICOLON) -> "continue;"
| TUPLE3 ((VOID|BOOL|CHAR|INT|DOUBLE|TUPLE2 (CONST, CHAR)|TYPE_NAME _) as typ, TLIST lst, SEMICOLON) ->
    String.concat "/* 381 */;\n\t" (List.map (fun itm -> dumptyp lev refs typ^" "^dumpc lev refs itm) lst)^";"
| TUPLE3 ((VOID|BOOL|CHAR|INT|DOUBLE|TUPLE2 (CONST, CHAR)|TYPE_NAME _) as typ, ptr, SEMICOLON) -> dumptyp lev refs typ^" "^dumpc lev refs ptr^";"
| TUPLE3 (LPAREN, expr, RPAREN) -> "("^dumpc lev refs expr^")"
| TUPLE5 (expr, QUERY, expr1, COLON, expr2) -> dumpc lev refs expr^" ? "^dumpc lev refs expr1^" : "^dumpc lev refs expr2
| TUPLE4 (LPAREN, ((VOID|BOOL|CHAR|INT|DOUBLE) as typ), RPAREN, expr) -> "("^dumptyp lev refs typ^") "^dumpc lev refs expr
| TUPLE4 (LPAREN, (TUPLE2 ((VOID|BOOL|CHAR|INT|DOUBLE|TYPE_NAME _), STAR) as typ), RPAREN, expr) -> "("^dumptyp lev refs typ^") "^dumpc lev refs expr
| TUPLE3 (IDENTIFIER fn, LPAREN, RPAREN) -> fmark lev refs fn; fn^"()"
| TUPLE4 (IDENTIFIER fn, LPAREN, args, RPAREN) ->
    fmark lev refs fn;
    let _ = search (lmark lev refs) args in
    fn^"("^adump lev refs args^")"
| TUPLE4 (arr, LBRACK, expr, RBRACK) -> dumpc lev refs arr^"["^adump lev refs expr^"]"
| TUPLE2 (HYPHEN, rght) -> "-"^dumpc lev refs rght
| TUPLE2 (PLING, rght) -> "!"^dumpc lev refs rght
| TUPLE2 (STAR, rght) -> "*"^dumpc lev refs rght
| TUPLE2 (SIZEOF, rght) -> "sizeof("^dumpc lev refs rght^")"
| TUPLE3 (lft, EQ_OP, rght) -> dumpc lev refs lft^"=="^dumpc lev refs rght
| TUPLE3 (lft, NE_OP, rght) -> dumpc lev refs lft^"!="^dumpc lev refs rght
| TUPLE3 (lft, GE_OP, rght) -> dumpc lev refs lft^">="^dumpc lev refs rght
| TUPLE3 (lft, LE_OP, rght) -> dumpc lev refs lft^"<="^dumpc lev refs rght
| TUPLE3 (lft, GREATER, rght) -> dumpc lev refs lft^">"^dumpc lev refs rght
| TUPLE3 (lft, LESS, rght) -> dumpc lev refs lft^" < "^dumpc lev refs rght
| TUPLE3 (lft, PLUS, rght) -> dumpc lev refs lft^"+"^dumpc lev refs rght
| TUPLE3 (lft, HYPHEN, rght) -> dumpc lev refs lft^"-"^dumpc lev refs rght
| TUPLE3 (lft, STAR, rght) -> dumpc lev refs lft^"*"^dumpc lev refs rght
| TUPLE3 (lft, SLASH, rght) -> dumpc lev refs lft^"/"^dumpc lev refs rght
| TUPLE3 (lft, PERCENT, rght) -> dumpc lev refs lft^"%"^dumpc lev refs rght
| TUPLE3 (lft, LEFT_OP, rght) -> dumpc lev refs lft^" << "^dumpc lev refs rght
| TUPLE3 (lft, RIGHT_OP, rght) -> dumpc lev refs lft^" >> "^dumpc lev refs rght
| TUPLE3 (lft, VBAR, rght) -> dumpc lev refs lft^" | "^dumpc lev refs rght
| TUPLE3 (lft, CARET, rght) -> dumpc lev refs lft^" ^ "^dumpc lev refs rght
| TUPLE3 (lft, AMPERSAND, rght) -> dumpc lev refs lft^" & "^dumpc lev refs rght
| TUPLE3 (lft, OR_OP, rght) -> dumpc lev refs lft^" || "^dumpc lev refs rght
| TUPLE3 (lft, AND_OP, rght) -> dumpc lev refs lft^" && "^dumpc lev refs rght
| TUPLE5 (IF, LPAREN, cond, RPAREN, then') -> "if ("^dumpc lev refs cond^") "^dumpc lev refs then'
| TUPLE3 (LBRACE, body, RBRACE) -> "\n\t{\n\t"^dumpc lev refs body^"\n\t}\n"
| TLIST lst -> String.concat "/* 417 */\n" (List.map (dumpc lev refs) lst)
| TUPLE2 (stmt, SEMICOLON) -> dumpc lev refs stmt^"; "
| TUPLE3 (lft, EQUALS, TUPLE3(LBRACE, TLIST lst, RBRACE)) ->
    let _ = search (lmark lev refs) lft in
    let _ = search (lmark lev refs) (TLIST lst) in
    dumpc lev refs lft^" = {"^String.concat "/* 422 */, " (List.map (dumpc lev refs) lst)^"}"
| TUPLE3 (lft, EQUALS, expr) ->
    let _ = search (lmark lev refs) lft in
    let _ = search (lmark lev refs) expr in
    dumpc lev refs lft^" = "^dumpc lev refs expr
| TUPLE3 (IDENTIFIER str, DOT, IDENTIFIER memb) -> smark lev true refs str; str^"."^memb
| TUPLE3 (IDENTIFIER str, PTR_OP, IDENTIFIER memb) -> smark lev true refs str; str^"->"^memb
| TUPLE4 (SIZEOF, LPAREN, typ, RPAREN) -> "sizeof("^dumptyp lev refs typ^")"
| TUPLE2 (AMPERSAND, compound) -> "&"^dumpc lev refs compound
| TUPLE7 (FOR, LPAREN, TUPLE3 (typ, initial, SEMICOLON), TUPLE2 (condition, SEMICOLON), inc, RPAREN, body) -> "{for ("^dumptyp lev refs typ^" "^dumpc lev refs initial^"; "^dumpc lev refs condition^"; "^dumpc lev refs inc^") "^dumpc lev refs body^"}\n"
| TUPLE7 (FOR, LPAREN, TUPLE2 (initial, SEMICOLON), TUPLE2 (condition, SEMICOLON), inc, RPAREN, body) -> "{for ( "^dumpc lev refs initial^"; "^dumpc lev refs condition^"; "^dumpc lev refs inc^")\n\t{ "^dumpc lev refs body^" }}\n"
| TUPLE7 (FOR, LPAREN, SEMICOLON, TUPLE2 (condition, SEMICOLON), inc, RPAREN, TUPLE3 (LBRACE, body, RBRACE)) -> "{for ( ; "^dumpc lev refs condition^"; "^dumpc lev refs inc^")\n\t{ "^dumpc lev refs body^" }}\n"
| TUPLE7 (IF, LPAREN, expr, RPAREN, then', ELSE, else') -> "if ("^dumpc lev refs expr^") "^dumpc lev refs then'^" else "^dumpc lev refs else'
| TUPLE2 (lvalue, INC_OP) -> dumpc lev refs lvalue^"++"
| TUPLE2 (lvalue, DEC_OP) -> dumpc lev refs lvalue^"--"
| TUPLE3 (RETURN, arg, SEMICOLON) -> "return "^dumpc lev refs arg^"; "
| TUPLE3 (lvalue, ADD_ASSIGN, expr) -> dumpc lev refs lvalue^"+="^dumpc lev refs expr
| TUPLE3 (lvalue, SUB_ASSIGN, expr) -> dumpc lev refs lvalue^"-="^dumpc lev refs expr
| TUPLE3 (lvalue, MUL_ASSIGN, expr) -> dumpc lev refs lvalue^"*="^dumpc lev refs expr
| TUPLE3 (lvalue, DIV_ASSIGN, expr) -> dumpc lev refs lvalue^"/="^dumpc lev refs expr
| TUPLE3 (lvalue, AND_ASSIGN, expr) -> dumpc lev refs lvalue^"&="^dumpc lev refs expr
| TUPLE3 (lvalue, OR_ASSIGN, expr) -> dumpc lev refs lvalue^"|="^dumpc lev refs expr
| TUPLE3 (lvalue, XOR_ASSIGN, expr) -> dumpc lev refs lvalue^"^="^dumpc lev refs expr
| TUPLE3 (lvalue, LEFT_ASSIGN, expr) -> dumpc lev refs lvalue^"<<="^dumpc lev refs expr
| TUPLE3 (lvalue, RIGHT_ASSIGN, expr) -> dumpc lev refs lvalue^">>="^dumpc lev refs expr
| TUPLE3 (lft, COMMA, rght) -> dumpc lev refs lft^", "^dumpc lev refs rght
| TUPLE3 (lft, PTR_OP, rght) -> dumpc lev refs lft^"->"^dumpc lev refs rght
| TUPLE3 (TUPLE4 (lft, LBRACK, expr, RBRACK), DOT, IDENTIFIER field) -> dumpc lev refs lft^"["^dumpc lev refs expr^"]."^field
| TUPLE3 (TUPLE4 (UNION, LBRACE, TLIST lst, RBRACE), IDENTIFIER u, SEMICOLON) ->
   "union { "^String.concat "/* 451 */\n\t" (List.map (dumpc lev refs) lst)^" } "^u^";"
| TUPLE4 (LPAREN, TYPE_NAME name_t, RPAREN, expr) -> "("^name_t^")"^dumpc lev refs expr
| TUPLE3 (TUPLE2 (LONG, LONG), TUPLE3 (IDENTIFIER lhs, EQUALS, rhs), SEMICOLON) ->
   "long long "^lhs^" = "^dumpc lev refs rhs^";"
| TUPLE3 (TUPLE4 (UNION, LBRACE, TLIST lst, RBRACE), TUPLE3 (IDENTIFIER u, EQUALS, expr), SEMICOLON) ->
   "union { "^String.concat "/* 456 */\n\t" (List.map (dumpc lev refs) lst)^" } "^u^" = "^dumpc lev refs expr^";"
| TUPLE3 (IDENTIFIER id, LBRACK, RBRACK) -> id^"[]"
| TUPLE5 (SWITCH, LPAREN, swexpr, RPAREN, TUPLE3 (LBRACE, TLIST lst, RBRACE)) ->
   "switch("^dumpc lev refs swexpr^") {"^String.concat "/* 459 */\n\t" (List.map (dumpc lev refs) lst)^" }"
| TUPLE4 (CASE, cexpr, COLON, stmt) -> "case "^dumpc lev refs cexpr^": "^dumpc lev refs stmt
| TUPLE3 (DEFAULT, COLON, stmt) -> "default: "^dumpc lev refs stmt
| TUPLE7 (DO, TUPLE3 (LBRACE, TLIST lst, RBRACE), WHILE, LPAREN, doexpr, RPAREN, SEMICOLON) ->
   "do { "^String.concat "/* 463 */\n\t" (List.map (dumpc lev refs) lst)^" } while ("^dumpc lev refs doexpr^");"
| TUPLE5 (WHILE, LPAREN, whexpr, RPAREN, TUPLE3 (LBRACE, TLIST lst, RBRACE)) ->
   "while ("^dumpc lev refs whexpr^") { "^String.concat "/* 465 */\n\t" (List.map (dumpc lev refs) lst)^" };"
| TUPLE6 (LPAREN, TYPE_NAME name_t, RPAREN, LBRACE, cexpr, RBRACE) ->
   "("^name_t^") {"^adump lev refs cexpr^"} "
| TUPLE2 (TUPLE2 (STAR, CONST), TUPLE3 (IDENTIFIER ty, LBRACK, RBRACK)) -> "* const "^ty^"[] "
| TUPLE2 (LBRACE, RBRACE) -> "{ }"
| TUPLE2 (TUPLE2 (DOT, IDENTIFIER field), EQUALS) -> "."^field^"="
| oth -> "/* 416 */"^Translation_unit_list_filt.dumptree oth

and adump lev refs = function
  | TLIST lst -> String.concat "/* 474 */, " (List.map (dumpc lev refs) lst)
  | VOID -> ""
  | oth -> dumpc lev refs oth

and pdump lev refs = function
  | TLIST lst -> String.concat "/* 479 */, " (List.map (dumpc lev refs) lst)
  | VOID -> ""
  | oth -> dumpc lev refs oth

let cdumplst = ref []
let simplify = try let _ = Sys.getenv "SIMPLIFY_CEXPR" in true with _ -> true

let rec cdump lev refs cexpr =
if simplify && is_int cexpr then
    (let rslt = cexpr_as_int cexpr in
    if not (List.mem (cexpr,rslt) !cdumplst) then
    cdumplst := (cexpr,rslt) :: !cdumplst; string_of_int rslt)
else dumpc lev refs cexpr

let edump' lev refs str fields =
    "enum "^str^"\n\t{\n\t"^String.concat "/* 494 */,\n\t" (List.map (function
    | IDENTIFIER field ->
       emark lev refs (IDENTIFIER field);
      field
    | oth -> "/* 473 */"^Translation_unit_list_filt.dumptree oth) fields)^"\n\t} "

let sdump' lev refs str fields =
  smark lev true refs str;
    "struct "^str^"\n\t{\n\t"^String.concat "/* 499 */\n\t" (List.map (function
      | TUPLE3 (TUPLE2 (STRUCT, IDENTIFIER str), TUPLE2 (STAR, IDENTIFIER field), SEMICOLON) ->
           smark lev false refs str; "struct "^str^" *"^field^";"
      | TUPLE3 (TUPLE2 (STRUCT, IDENTIFIER str), IDENTIFIER field, SEMICOLON) ->
           smark lev true refs str; "struct "^str^" "^field^";"
      | TUPLE3 (typ, TUPLE4 (TUPLE3 (LPAREN, TUPLE2 (STAR, IDENTIFIER fn), RPAREN), LPAREN, TLIST typlst, RPAREN), SEMICOLON) ->
           dumptyp lev refs typ^" (* "^fn^") ("^String.concat "/* 505 */, " (List.map (dumptyp lev refs) typlst)^");"
      | TUPLE3 (typ, TUPLE4 (TUPLE3 (LPAREN, TUPLE2 (STAR, IDENTIFIER fn), RPAREN), LPAREN, typ', RPAREN), SEMICOLON) ->
           dumptyp lev refs typ^" (* "^fn^") ("^dumptyp lev refs typ'^");"
      | TUPLE3 (typ, IDENTIFIER field, SEMICOLON) ->
           dumptyp lev refs typ^" "^field^";"
      | TUPLE3 (typ, TUPLE4 (IDENTIFIER field, LBRACK, cexpr, RBRACK), SEMICOLON) ->
           let _ = search (emark lev refs) cexpr in dumptyp lev refs typ^" "^field^"["^cdump lev refs cexpr^"];"
      | TUPLE3 (typ, TUPLE2 (STAR, IDENTIFIER field), SEMICOLON) ->
           dumptyp lev refs typ^" *"^field^";"
      | oth -> "/* 491 */"^Translation_unit_list_filt.dumptree oth) fields)^"\n\t} "

let udump' lev refs str fields =
  umark lev refs str;
    "union "^str^"\n\t{\n\t"^String.concat "/* 527 */\n\t" (List.map (function
      | TUPLE3 (typ, IDENTIFIER field, SEMICOLON) ->
           dumptyp lev refs typ^" "^field^";"
      | TUPLE3 (typ, TUPLE4 (IDENTIFIER field, LBRACK, cexpr, RBRACK), SEMICOLON) ->
           let _ = search (emark lev refs) cexpr in dumptyp lev refs typ^" "^field^"["^cdump lev refs cexpr^"];"
      | oth -> "/* 509 */"^Translation_unit_list_filt.dumptree oth) fields)^"\n\t} "

let bodylst = ref []
let bodylst' = ref []
let rec dump_body lev refs = function
| CONSTANT str -> str
| STRING_LITERAL str -> str
| TUPLE2 (HYPHEN, CONSTANT str) -> "-"^str
| TLIST [TUPLE2 (TUPLE2 (DOT, IDENTIFIER field), EQUALS); CONSTANT num] -> "."^field^"="^num    
| TLIST (TUPLE2 (TUPLE2 (DOT, IDENTIFIER field), EQUALS) ::
        TUPLE3 (LBRACE, TLIST lst, RBRACE) :: []) ->
     "."^field^"={"^String.concat "/* 594 */,\n\t" (List.map (dump_body lev refs) lst)^" }"
| TUPLE2 (TUPLE2 (DOT, IDENTIFIER field), EQUALS) -> "."^field^"= "
| TUPLE2 (TUPLE2 (TUPLE2 (DOT, IDENTIFIER field), EQUALS), rslt) -> "."^field^"= "^dump_body lev refs rslt
| TUPLE3 (LBRACE, TLIST
    (TUPLE2 (TUPLE2 (DOT, IDENTIFIER field), EQUALS) :: TUPLE3 (LBRACE, TLIST clst, RBRACE) :: []), RBRACE) ->
        "{."^field^"={"^String.concat "/* 599 */,\n\t" (List.map (dump_body lev refs) clst)^" }}"
| TUPLE3 (LBRACE, TLIST lst, RBRACE) -> "{"^String.concat "/* 600 */,\n\t" (List.map (dump_body lev refs) lst)^"}"
| TUPLE3 (LBRACE, itm, RBRACE) -> dump_body lev refs (TUPLE3 (LBRACE, TLIST [itm], RBRACE))
| TUPLE2 (SIZEOF, TUPLE3 (LPAREN, (IDENTIFIER id as l), RPAREN)) ->
    let _ = lmark lev refs l in "sizeof ("^id^")"
| TUPLE2 (SIZEOF, TUPLE3 (LPAREN, TUPLE2 (STAR, (IDENTIFIER id as l)), RPAREN)) ->
    let _ = lmark lev refs l in "sizeof (*"^id^")"
| TLIST lst -> bodylst' := lst :: !bodylst';
    "{\n\t"^String.concat "/* 607 */,\n\t" (List.map (dump_body lev refs) lst)^"\n\t}"
| cexpr when is_int cexpr -> string_of_int (cexpr_as_int cexpr)
| TUPLE3 (lft, PLUS, rght) -> dump_body lev refs lft^"+"^dump_body lev refs rght
| TUPLE3 (lft, HYPHEN, rght) -> dump_body lev refs lft^"-"^dump_body lev refs rght
| TUPLE3 (lft, STAR, rght) -> dump_body lev refs lft^"*"^dump_body lev refs rght
| TUPLE3 (lft, SLASH, rght) -> dump_body lev refs lft^"/"^dump_body lev refs rght
| TUPLE3 (lft, PERCENT, rght) -> dump_body lev refs lft^"%"^dump_body lev refs rght
| body -> bodylst := body :: !bodylst; failwith "bodylst"

let idumplst = ref []
let rec alldump lev refs key = function
  | I (typ,body) ->
    refs.itab, (match typ with
      | TYPE_NAME id_t -> tmark lev refs id_t; id_t^" "^key^"="^dump_body lev refs body^";\n"
      | TUPLE2 (DOUBLE, STAR) -> dumptyp lev refs typ^" "^key^"="^dump_body lev refs body^";\n"
      | INT -> dumptyp lev refs typ^" "^key^"="^dump_body lev refs body^";\n"
      | DOUBLE -> dumptyp lev refs typ^" "^key^"="^dump_body lev refs body^";\n"
      | oth -> "/* 595 */"^Translation_unit_list_filt.dumptree oth)
  | G typ ->
    refs.gtab, (match typ with
      | TYPE_NAME id_t -> id_t^" "^key^";\n"
      | TUPLE2 (DOUBLE, STAR) -> dumptyp lev refs typ^" "^key^";\n"
      | TUPLE4 (IDENTIFIER array, LBRACK, cexpr, RBRACK) when is_int cexpr ->
          array^"["^string_of_int (cexpr_as_int cexpr)^"]"
      | TUPLE2 (typ, TUPLE4 (IDENTIFIER array, LBRACK, cexpr, RBRACK)) ->
          dumptyp lev refs typ^" "^array^"["^adump lev refs cexpr^"];"
      | TUPLE2 (TUPLE2 (EXTERN, TUPLE2 (STRUCT, TYPE_NAME id_t)), STAR) -> "extern struct "^id_t^" *"^key^";\n"
      | TUPLE3 (typ, TUPLE3 (IDENTIFIER array, LBRACK, RBRACK), TLIST lst) ->
	 dumptyp lev refs typ^" "^array^"[] = {"^String.concat "/* 644 */,\n\t" (List.map (function
					   | CONSTANT str -> str
					   | oth -> Translation_unit_list_filt.dumptree oth) lst)^"};"
      | TUPLE2 (typ, TUPLE3 (TUPLE4 (IDENTIFIER array, LBRACK, CONSTANT len, RBRACK), EQUALS, init)) ->
	 dumptyp lev refs typ^" "^array^"[] = "^dump_body lev refs init^";\n"
      | TUPLE2 (typ, TUPLE3 (IDENTIFIER id, EQUALS, init)) -> ""
      | TUPLE2 (typ, TLIST (IDENTIFIER id :: tl)) -> ""
(*
      | TUPLE2 (typ, TUPLE4 (IDENTIFIER array, LBRACK, len, RBRACK)) -> ""
*)
      | typ -> idumplst := typ :: !idumplst; "/* 647 */"^dumptyp lev refs typ^" "^key^";\n")
   | B (typ,paramlst,body) ->
      refs.btab, ("\n"^dumptyp lev refs typ^" "^key^"("^String.concat "/* 667 */, " (List.map (dumparg lev refs) paramlst)^")\n{")^
    String.concat "/* 668 */\n" (List.map (fun itm -> (dumpc lev refs itm)) body)^"}\n"
   | L (typ,paramlst,body) ->
      refs.ltab, ("\n"^dumptyp lev refs typ^" "^key^"("^String.concat "/* 673 */, " (List.map (dumparg lev refs) paramlst)^")\n{")^
    String.concat "/* 674 */\n" (List.map (fun itm -> (dumpc lev refs itm)) body)^"}\n"
   | F (typ,paramlst) ->
      refs.ftab, ("\n"^dumptyp lev refs typ^" "^key^"("^String.concat "/* 679 */, " (List.map (dumparg lev refs) paramlst)^");\n")
   | E body->
      refs.etab, (match body with
      | TUPLE5 (ENUM, IDENTIFIER str, LBRACE, TLIST fields, RBRACE) -> (edump' lev refs str fields)
      | TUPLE3 (IDENTIFIER e, EQUALS, CONSTANT n) -> "enum "^key^" {"^e^"="^n^"};\n"
      | TUPLE2 (STRUCT, TYPE_NAME nam) -> "struct "^nam^";\n"
      | TUPLE2 (TYPE_NAME typ, IDENTIFIER id) -> typ^" "^id^";\n"
      | oth -> ("/* 528 */"^Translation_unit_list_filt.dumptree oth))
   | S fields -> refs.stab, sdump' lev refs key fields^";\n"
   | T body ->
      refs.ttab, (match body with
       | TUPLE5 (ENUM, IDENTIFIER str, LBRACE, TLIST fields, RBRACE) -> 
          ("typedef "^edump' lev refs str fields^key^";\n")
       | TUPLE5 (STRUCT, IDENTIFIER str, LBRACE, TLIST fields, RBRACE) ->
          ("typedef "^sdump' lev refs str fields^key^";\n")
       | TUPLE5 (STRUCT, IDENTIFIER str, LBRACE, field, RBRACE) ->
          ("typedef "^sdump' lev refs str [field]^key^";\n")
       | TUPLE5 (UNION, IDENTIFIER str, LBRACE, TLIST fields, RBRACE) ->
          ("typedef "^udump' lev refs str fields^key^";\n")
       | TUPLE5 (UNION, IDENTIFIER str, LBRACE, field, RBRACE) ->
          ("typedef "^udump' lev refs str [field]^key^";\n")
       | TUPLE2 (STRUCT, IDENTIFIER str) -> smark lev true refs str;
					    ("typedef struct "^str^" "^key^";\n")
       | TUPLE2 (UNION, IDENTIFIER str) -> umark lev refs str;
					   ("typedef union "^str^" "^key^";\n")
       | typ ->
          ("typedef "^dumptyp lev refs typ^" "^key^";\n"))
   | U fields -> refs.utab, udump' lev refs key fields^";\n"

let rec setlev maxlev lev0 refs = function
  | [] -> ()
  | itm :: reclst ->
     let outer_lev = if reclst = [] then 0 else level (List.hd reclst)  in
     let prev_lev = level itm in
     if prev_lev <= outer_lev then
      begin
	prerr_endline (itm^":"^string_of_int prev_lev^", "^if reclst = [] then "[]" else List.hd reclst^":"^string_of_int outer_lev);
	setlevel itm lev0;
	let lev = level itm in
	if lev > !maxlev then maxlev := lev;
	List.iter (fun entry ->
	       let tab, src = alldump lev refs itm entry in
	       let deps = {blst= !(refs.brefs);
			   elst= !(refs.erefs);
			   flst= !(refs.frefs);
			   glst= !(refs.grefs);
			   ilst= !(refs.irefs);
			   llst= !(refs.lrefs);
			   slst= !(refs.srefs);
			   tlst= !(refs.trefs);
			   ulst= !(refs.urefs)} in
	       if hashmem tab itm then let (oldlev,deps,src) = Hashtbl.find tab itm in
				       begin
					 Hashtbl.replace tab itm (max lev oldlev,deps,src);
					 if !verbose then
					   prerr_endline ("setlev loop over "^itm^" at level: "^string_of_int lev);
				       end
	       else
		 Hashtbl.add tab itm (lev,deps,src);
	       List.iter (
		   List.iter (fun itm' -> 
		     if not (List.mem itm' (itm :: reclst)) then
		       begin
			 prerr_endline itm';
			 setlev maxlev (lev+1) refs (itm' :: itm :: reclst)
		       end
		   )) [deps.blst;deps.elst;deps.ilst;deps.llst;deps.slst;deps.tlst;deps.ulst]
	      ) (lookup itm)
      end
	
let print_uniq chan i ch str (lev,_,itm) =
  if i = level str && itm <> "//\n" then
    begin
      if !verbose then
        output_string chan ("/* "^String.make 1 ch^string_of_int lev^":"^str^" */\n"^itm)
      else
        output_string chan itm
    end

let dump parse refs chan main argv =
  let rslts = ref [] in
  prerr_endline "/*";
  for i = 1 to Array.length argv - 1 do rslts := getrslt parse argv.(i) :: !rslts; done;
  prerr_endline "*/";
  let maxlev = ref 0 in
  setlev maxlev 0 refs [main];
  setlev maxlev 0 refs [main];
  let needed = ref [] in
  for i = !maxlev downto 0 do
  Hashtbl.iter (print_uniq chan i 'b') refs.btab;
  Hashtbl.iter (print_uniq chan i 'e') refs.etab;
  Hashtbl.iter (print_uniq chan i 'f') refs.ftab;
  Hashtbl.iter (print_uniq chan i 'g') refs.gtab;
  Hashtbl.iter (print_uniq chan i 'i') refs.itab;
  Hashtbl.iter (print_uniq chan i 'l') refs.ltab;
  Hashtbl.iter (print_uniq chan i 's') refs.stab;
  Hashtbl.iter (print_uniq chan i 't') refs.ttab;
  Hashtbl.iter (print_uniq chan i 'u') refs.utab;
  done;
  List.rev !needed, !rslts
