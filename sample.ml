open Translation_unit_list
(*
open Translation_unit_list_transform
*)

let fbody = Hashtbl.create 255
let globals = Hashtbl.create 255
let globlst = ref []

let othp = ref EOF_TOKEN
let othexpr = ref EOF_TOKEN
let othstmt = ref EOF_TOKEN
let othdecl = ref []
let othlst = ref []

let deplst = ref []
let depends x = if not (List.mem x !deplst) then deplst := x :: !deplst

let expr' id =  if id.[0] = '$' then "_'"^String.sub id 1 (String.length id - 1) else if id.[0] <= 'Z' then "_"^id else id

let othtyp = ref EOF_TOKEN

let ftyp' = function
| VOID -> "unit"
| TUPLE3 (STRING "declaration_specifiers82", UNSIGNED, CHAR) -> "char"
| INT -> "int"
| TUPLE3 (STRING "declaration_specifiers82", UNSIGNED,
    TUPLE3 (STRING "declaration_specifiers82", LONG, LONG)) -> "Int64.t"
| oth -> othtyp := oth; failwith "ftyp"

let gtyp' = function
TUPLE3 (STRING "declaration_specifiers82", LONG,
    TUPLE3 (STRING "declaration_specifiers82", LONG, CONST)) -> "Int64.t"
| TUPLE3 (STRING "declaration_specifiers82", UNSIGNED,
    TUPLE3 (STRING "declaration_specifiers82", LONG,
      TUPLE3 (STRING "declaration_specifiers82", LONG, CONST))) -> "Int64.t"
| TUPLE3 (STRING "declaration_specifiers82", UNSIGNED,
    TUPLE3 (STRING "declaration_specifiers82", CHAR, CONST)) -> "char"
| oth -> othstmt := oth; failwith "missing_gtyp"

let typ' locals id = List.assoc_opt (expr' id) !locals
let typ locals id = match typ' locals id with Some x -> x | None -> ""

let rec ldump = function
| [] -> ()
| (id,typ) :: tl -> print_endline ("local: "^id^": "^typ); ldump tl

let ctyp locals id = match typ' locals id with
      | Some  "Int64.t ref" -> "Int64.t"
      | Some  "Bytes.t" -> "char"
      | Some  "int ref" -> "int"
      | Some  "char ref" -> "char"
      | Some  "Int64.t array" -> "Int64.t"
      | Some  "Int64.t array array" -> "Int64.t array"
      | Some  oth -> ldump !locals; failwith ("unhand_ctyp_"^id)
      | None -> (match Hashtbl.find_opt fbody id with None -> ( match Hashtbl.find_opt globals id with
          | None -> "missing_"^id
          | Some (typ,_, _) -> if not (List.mem id !globlst) then globlst := id :: !globlst;  gtyp' typ
          | _ -> "unknown_glob_"^id)
        | Some (typ, plst, _) -> ftyp' typ | _ -> "unknown_ftyp")

let idtyp locals id = match typ' locals id with
      | Some  "Int64.t ref" -> expr' id
      | Some  "Bytes.t" -> expr' id
      | Some  "int ref" -> expr' id
      | Some  "int" -> expr' id
      | Some  "char ref" -> expr' id
      | Some  "Int64.t" -> expr' id
      | Some  "Int64.t array" -> id
      | Some  "Int64.t array array" -> id
      | Some  oth -> "unhand_"^id
      | None -> (match Hashtbl.find_opt fbody id with None -> ( match Hashtbl.find_opt globals id with
          | None -> "missing_"^id
          | Some (typ,_, _) -> if not (List.mem id !globlst) then globlst := id :: !globlst; gtyp' typ
          | _ -> "unknown_glob_"^id)
        | Some (typ, plst, _) -> ftyp' typ | _ -> "unknown_ftyp")

let otht = ref None
let dbgcall = Hashtbl.create 255
let othasgn = ref None

let rec texpr locals = function
| IDENTIFIER id -> idtyp locals id, typ locals id
| CONSTANT "1LL" -> "1L","Int64.t"
| CONSTANT n -> n,"int"
| STRING_LITERAL s -> s, "Bytes.t"
| TUPLE4 (STRING "additive_expression36", lft, PLUS, rght) -> (match texpr locals lft, texpr locals rght with
    | (lft,"int"),(rght,"int") -> lft^" + (*1*) "^rght,"int"
    | (lft,"int ref"),(rght,"int") -> "!"^lft^" + (*2*) "^rght,"int"
    | (lft,"char"),(rght,"int") -> "(int_of_char "^lft^") + (*3*) "^rght,"int"
    | (lft,"Int64.t"),(rght,"int") -> "(Int64.add (*4*) ("^lft^") (Int64.of_int ("^rght^")))","Int64.t"
    | (lft,"Int64.t ref"),(rght,"int") -> "(Int64.add (*5*) (!"^lft^") (Int64.of_int ("^rght^")))","Int64.t"
    | (lft,"Bytes.t"),(rght,"int") -> "("^lft^", (*6*) "^rght^")","Bytes.t"
    | (lft,"Bytes.t"),(rght,"Int64.t") -> "("^lft^", "^rght^")","Int64.t"
    | (lft,"int"),(rght,"Int64.t") -> "(Int64.add (*8*) (Int64.of_int ("^lft^")) ("^rght^"))","int"
    | (lft,"Int64.t"),(rght,"Int64.t") -> "Int64.add (*9*) ("^lft^") ("^rght^")","Int64.t"
    | (lft,"int"),(rght,"Int64.t ref") -> "Int64.add (*10*) (Int64.of_int ("^lft^")) !"^rght,"int"
    | (lft,"Int64.t"),(rght,"Int64.t ref") -> "Int64.add (*11*) ("^lft^") (!"^rght^")","Int64.t"
    | (lft,"Int64.t ref"),(rght,"Int64.t ref") -> "Int64.add (*12*) !"^lft^" !"^rght,"Int64.t"
    | (_,tlft),(_,trght) as unhand -> otht := Some (!locals,lft,rght,unhand); failwith (tlft^":"^trght^":68"))
| TUPLE4 (STRING "additive_expression37", lft, HYPHEN, rght) -> (match texpr locals lft, texpr locals rght with
    | (lft,"int"),(rght,"int") -> lft^"-"^rght,"int"
    | (lft,"int ref"),(rght,"int") -> "!"^lft^"-"^rght,"int"
    | (lft,"int"),(rght,"int ref") -> lft^" - !"^rght,"int"
    | (lft,"Int64.t"),(rght,"int") -> "Int64.sub ("^lft^") (Int64.of_int ("^rght^"))","Int64.t"
    | (lft,"Int64.t"),(rght,"Int64.t") -> "Int64.sub ("^lft^") ("^rght^")","Int64.t"
    | (lft,"Bytes.t"),(rght,"Int64.t") -> "Int64.sub ("^lft^") ("^rght^")","int"
    | (_,tlft),(_,trght) as unhand -> otht := Some (!locals,lft,rght,unhand); failwith (tlft^":"^trght^":76"))
| TUPLE4 (STRING "multiplicative_expression32", lft, STAR, rght) -> (match texpr locals lft, texpr locals rght with
    | (lft,"int"),(rght,"int") -> lft^" * "^rght,"int"
    | (lft,"int"),(rght,"int ref") -> lft^" * !"^rght,"int"
    | (lft,"char"),(rght,"int") -> "(int_of_char "^lft^") * "^rght,"int"
    | (lft,"char"),(rght,"Int64.t") -> "Int64.mul (Int64.of_int (int_of_char "^lft^")) ("^rght^")","int"
    | (lft,"int"),(rght,"Int64.t") -> "Int64.mul (Int64.of_int ("^lft^")) "^rght,"int"
    | (lft,"Int64.t"),(rght,"Int64.t") -> "Int64.mul "^lft^" "^rght,"int"
    | (_,tlft),(_,trght) as unhand -> otht := Some (!locals,lft,rght,unhand); failwith (tlft^":"^trght^":95"))
| TUPLE4 (STRING "multiplicative_expression33", lft, SLASH, rght) -> (match texpr locals lft, texpr locals rght with
    | (lft,"int"),(rght,"int") -> lft^" / "^rght,"int"
    | (lft,"int ref"),(rght,"int") -> "!"^lft^" / "^rght,"int"
    | (_,tlft),(_,trght) as unhand -> otht := Some (!locals,lft,rght,unhand); failwith (tlft^":"^trght^":98"))
| TUPLE4 (STRING "multiplicative_expression34", lft, PERCENT, rght) -> (match texpr locals lft, texpr locals rght with
    | (lft,"int"),(rght,"int") -> lft^" mod "^rght,"int"
    | (lft,"int ref"),(rght,"int") -> "!"^lft^" mod "^rght,"int"
    | (_,tlft),(_,trght) as unhand -> otht := Some (!locals,lft,rght,unhand); failwith (tlft^":"^trght^":109"))
| TUPLE3 (STRING "unary_expression20", STAR,
          TUPLE4
           (STRING "primary_expression4", LPAREN,
            TUPLE4
             (STRING "additive_expression36", IDENTIFIER id', PLUS, idx),
            RPAREN)) ->
    (match typ locals id' with
      | "Bytes.t" -> "Bytes.get "^expr' id'^" "^idxcast locals idx, "char"
      | _ -> expr' id'^".("^idxcast locals idx^")", ctyp locals id')
| TUPLE4 (STRING "and_expression50", lft, AMPERSAND, rght) -> (match texpr locals lft, texpr locals rght with
    | (lft,"int"),(rght,"int") -> "Int.logand ("^lft^") ("^rght^")","int"
    | (lft,"int"),(rght,"Int64.t") -> "Int64.logand (Int64.of_int ("^lft^")) ("^rght^")","Int64.t"
    | (lft,"int ref"),(rght,"int") -> "Int.logand (!"^lft^") ("^rght^")","int"
    | (lft,"char"),(rght,"int") -> "Int.logand (int_of_char "^lft^") ("^rght^")","int"
    | (lft,"Int64.t"),(rght,"int") -> "Int64.logand ("^lft^") (Int64.of_int ("^rght^"))","Int64.t"
    | (lft,"Int64.t"),(rght,"Int64.t") -> "Int64.logand ("^lft^") ("^rght^")","Int64.t"
    | (_,tlft),(_,trght) as unhand -> otht := Some (!locals,lft,rght,unhand); failwith (tlft^":"^trght^":128"))
| TUPLE4 (STRING "exclusive_or_expression52", lft, CARET, rght) -> (match texpr locals lft, texpr locals rght with
    | (lft,"int"),(rght,"int") -> "logxor ("^lft^") ("^rght^")","int"
    | (lft,"char"),(rght,"int") -> "logxor (int_of_char "^lft^") ("^rght^")","int"
    | (lft,"Int64.t"),(rght,"int") -> "Int64.logxor ("^lft^") (Int64.of_int ("^rght^"))","Int64.t"
    | (lft,"Int64.t"),(rght,"Int64.t") -> "Int64.logxor ("^lft^") ("^rght^")","Int64.t"
    | (lft,"Int64.t ref"),(rght,"Int64.t ref") -> "Int64.logxor (!"^lft^") (!"^rght^")","Int64.t"
    | (lft,"Int64.t"),(rght,"Int64.t ref") -> "Int64.logxor ("^lft^") (!"^rght^")","Int64.t"
    | (_,tlft),(_,trght) as unhand -> otht := Some (!locals,lft,rght,unhand); failwith (tlft^":"^trght^":135"))
| TUPLE4 (STRING "inclusive_or_expression54", lft, VBAR, rght) -> (match texpr locals lft, texpr locals rght with
    | (lft,"int"),(rght,"int") -> "Int.logor ("^lft^") ("^rght^")","int"
    | (lft,"Int64.t"),(rght,"Int64.t") -> "Int64.logor ("^lft^") ("^rght^")","Int64.t"
    | (lft,"int"),(rght,"Int64.t") -> "Int64.logor (Int64.of_int ("^lft^")) ("^rght^")","Int64.t"
    | (lft,"char"),(rght,"int") -> "Int.logor (int_of_char "^lft^") ("^rght^")","int"
    | (lft,"int"),(rght,"char") -> "Int.logor ("^lft^") (int_of_char "^rght^")","int"
    | (lft,"Int64.t"),(rght,"char") -> "Int64.logor ("^lft^") (Int64.of_int (int_of_char ("^rght^")))","Int64.t"
    | (_,tlft),(_,trght) as unhand -> otht := Some (!locals,lft,rght,unhand); failwith (tlft^":"^trght^":139"))
| TUPLE3 (STRING "unary_expression20", AMPERSAND, rght) -> (match texpr locals rght with
    | (rght,"int") -> "ref ("^rght^")","int"
    | (rght,"Int64.t ref") -> "ref ("^rght^")","int"
    | (_,trght) as unhand -> otht := Some (!locals,rght,rght,(unhand,unhand)); failwith (trght^":"^trght^":159"))
| TUPLE3 (STRING "unary_expression20", HYPHEN, rght) -> (match texpr locals rght with
    | (rght,"int") -> "- ("^rght^")","int"
    | (_,trght) as unhand -> otht := Some (!locals,rght,rght,(unhand,unhand)); failwith (trght^":"^trght^":162"))
| TUPLE3 (STRING "unary_expression20", PLING, rght) -> (match texpr locals rght with
    | (rght,"int") -> "not ("^rght^")","int"
    | (_,trght) as unhand -> otht := Some (!locals,rght,rght,(unhand,unhand)); failwith (trght^":"^trght^":165"))
| TUPLE3 (STRING "unary_expression20", TILDE, rght) -> (match texpr locals rght with
    | (rght,"int") -> "lognot ("^rght^")","int"
    | (rght,"Int64.t") -> "Int64.lognot ("^rght^")","Int64.t"
    | (_,trght) as unhand -> otht := Some (!locals,rght,rght,(unhand,unhand)); failwith (trght^":"^trght^":169"))
| TUPLE4 (STRING "equality_expression47", lft, EQ_OP, rght) -> (match texpr locals lft, texpr locals rght with
    | (lft,"int"),(rght,"int") -> lft^" == "^rght,"int"
    | (lft,"int ref"),(rght,"int") -> "!"^lft^" == "^rght,"int"
    | (_,tlft),(_,trght) as unhand -> otht := Some (!locals,lft,rght,unhand); failwith (tlft^":"^trght^":173"))
| TUPLE4 (STRING "equality_expression48", lft, NE_OP, rght) -> (match texpr locals lft, texpr locals rght with
    | (lft,"int"),(rght,"int") -> lft^" <> "^rght,"int"
    | (lft,"int ref"),(rght,"int") -> "!"^lft^" <> "^rght,"int"
    | (_,tlft),(_,trght) as unhand -> otht := Some (!locals,lft,rght,unhand); failwith (tlft^":"^trght^":177"))
| TUPLE4 (STRING "relational_expression42", lft, LESS, rght) -> (match texpr locals lft, texpr locals rght with
    | (lft,"int"),(rght,"int") -> lft^" < "^rght,"int"
    | (lft,"int ref"),(rght,"int") -> "!"^lft^" < "^rght,"int"
    | (lft,"Int64.t"),(rght,"Int64.t") -> lft^" < "^rght,"int"
    | (lft,"Int64.t ref"),(rght,"Int64.t") -> "!"^lft^" < "^rght,"int"
    | (lft,"Int64.t"),(rght,"int") -> lft^" < "^rght,"int"
    | (lft,"Int64.t ref"),(rght,"int") -> "!"^lft^" < (Int64.of_int ("^rght^"))","int"
    | (_,tlft),(_,trght) as unhand -> otht := Some (!locals,lft,rght,unhand); failwith (tlft^":"^trght^":134"))
| TUPLE4 (STRING "relational_expression45", lft, GE_OP, rght) -> (match texpr locals lft, texpr locals rght with
    | (lft,"int"),(rght,"int") -> lft^" >= "^rght,"int"
    | (lft,"int ref"),(rght,"int") -> "!"^lft^" >= "^rght,"int"
    | (lft,"Int64.t"),(rght,"int") -> lft^" >= Int64.of_int ("^rght^")","int"
    | (_,tlft),(_,trght) as unhand -> otht := Some (!locals,lft,rght,unhand); failwith (tlft^":"^trght^":167"))
| TUPLE4 (STRING "primary_expression4", LPAREN, expr', RPAREN) -> let id, typ = texpr locals expr' in "("^id^")",typ
| TUPLE5 (STRING "cast_expression30", LPAREN, TLIST [TLIST [LONG; LONG]; UNSIGNED], RPAREN, rght) -> (match texpr locals rght with
    | (rght,"int") -> "Int64.of_int ("^rght^")", "Int64.t"
    | (rght,"char") -> "Int64.of_int (int_of_char ("^rght^"))", "Int64.t"
    | (rght,"Int64.t ref") -> "(!"^rght^")", "Int64.t"
    | (_,trght) as unhand -> otht := Some (!locals,rght,rght,(unhand,unhand)); failwith (trght^":"^trght^":148"))
| TUPLE5 (STRING "cast_expression30", LPAREN, BOOL, RPAREN, rght) -> (match texpr locals rght with
    | (rght,"int") -> "Int64.of_int ("^rght^")", "Int64.t"
    | (rght,"char") -> "Int64.of_int (int_of_char ("^rght^"))", "Int64.t"
    | (rght,"Int64.t ref") -> "(!"^rght^")", "Int64.t"
    | (_,trght) as unhand -> otht := Some (!locals,rght,rght,(unhand,unhand)); failwith (trght^":"^trght^":192"))
| TUPLE5 (STRING "postfix_expression8", (IDENTIFIER fn), LPAREN, rght, RPAREN) as call ->
    let rght = match rght with TLIST rght -> rght | rght -> [rght] in
    Hashtbl.add dbgcall fn (rght, !locals);
    depends fn;
    let typ = match Hashtbl.find fbody fn with (typ, plst, bodylst) -> ftyp' typ in
    "( "^expr' fn^" "^String.concat " " (List.map (fun rght -> match texpr locals rght with
(* *)
    | (rght,any) -> " ("^rght^") (*"^any^"*) "
(* *)
    | (rght,"int") -> rght
    | (rght,"Int64.t") -> rght
    | (rght,"Int64.t array") -> rght
    | (_,trght) as unhand -> otht := Some (!locals,rght,rght,(unhand,unhand)); failwith (trght^":"^trght^":153")) (List.rev rght))^") ", typ
| TUPLE4 (STRING "shift_expression39", lft, LEFT_OP, rght) -> (match texpr locals lft, texpr locals rght with
    | (lft,"int"),(rght,"int") -> lft^" shl "^rght,"int"
    | (lft,"char ref"),(rght,"int") -> "(int_of_char !"^lft^") shl "^rght,"int"
    | (lft,"Int64.t"),(rght,"int") -> "(Int64.shift_left ("^lft^") ("^rght^"))","Int64.t"
    | (lft,"Int64.t ref"),(rght,"int") -> "(Int64.shift_left (!"^lft^") ("^rght^"))","Int64.t"
    | (_,tlft),(_,trght) as unhand -> otht := Some (!locals,lft,rght,unhand); failwith (tlft^":"^trght^":158"))
| TUPLE4 (STRING "shift_expression40", lft, RIGHT_OP, rght) -> (match texpr locals lft, texpr locals rght with
    | (lft,"int"),(rght,"int") -> lft^" shr "^rght,"int"
    | (lft,"char"),(rght,"int") -> "Int.shift_right (int_of_char ("^lft^")) ("^rght^")","int"
    | (lft,"Int64.t"),(rght,"int") -> "Int64.shift_right ("^lft^") ("^rght^")","Int64.t"
    | (lft,"Int64.t ref"),(rght,"int") -> "Int64.shift_right (!"^lft^") ("^rght^")","Int64.t"
    | (_,tlft),(_,trght) as unhand -> otht := Some (!locals,lft,rght,unhand); failwith (tlft^":"^trght^":162"))
| TLIST lst -> let typ = ref "" in String.concat ", " (List.map (fun itm -> let id,t = texpr locals itm in typ := t; id) lst), !typ
| oth -> othexpr := oth; failwith "expr"

and idxcast locals idx =
  let id,t = texpr locals idx in match t with
    | "int" -> id
    | "int ref" -> "(!"^id^")"
    | "Int64.t" -> "(Int64.to_int "^id^")"
    | "Int64.t ref" -> "(Int64.to_int !"^id^")"
    | oth -> failwith ("idxcast "^oth)

let decl = function
| (id,typ)::locals -> "let ("^id^": "^typ^")"
| oth -> othdecl := oth; failwith "decl"

let decl' locals id typ =
  locals := (expr' id, typ) :: !locals; decl !locals

let pdump = function
|  TUPLE3
   (STRING "parameter_declaration167",
    TUPLE3 (STRING "declaration_specifiers82", UNSIGNED, CHAR),
    TUPLE3 (STRING "declarator142", STAR, (IDENTIFIER id))) -> expr' id, "Bytes.t"
|  TUPLE3
   (STRING "parameter_declaration167",
    TUPLE3
     (STRING "declaration_specifiers82", UNSIGNED,
      TUPLE3 (STRING "declaration_specifiers82", LONG, LONG)),
    IDENTIFIER id) -> id, "Int64.t"
|  TUPLE3
   (STRING "parameter_declaration167",
    TUPLE3
     (STRING "declaration_specifiers82", UNSIGNED,
      TUPLE3 (STRING "declaration_specifiers82", LONG, LONG)),
    TUPLE3 (STRING "declarator142", STAR, (IDENTIFIER id))) -> expr' id, "Int64.t array"
| TUPLE3
   (STRING "parameter_declaration167",
    TUPLE3 (STRING "declaration_specifiers82", LONG, LONG),
    TUPLE5
     (STRING "direct_declarator148",
      TUPLE4
       (STRING "direct_declarator145", LPAREN,
        TUPLE3 (STRING "declarator142", STAR, (IDENTIFIER id)), RPAREN),
      LBRACK, CONSTANT siz, RBRACK)) -> expr' id, "Int64.t array"
| TUPLE3
   (STRING "parameter_declaration167",
    TUPLE3 (STRING "declaration_specifiers82", LONG, LONG),
    TUPLE3 (STRING "declarator142", STAR, (IDENTIFIER id))) -> expr' id, "Int64.t array"
| TUPLE3 (STRING "parameter_declaration167", INT, (IDENTIFIER id)) -> expr' id, "int"
| TUPLE3 (STRING "parameter_declaration167",
    TUPLE3 (STRING "declaration_specifiers82", UNSIGNED, CHAR),
    (IDENTIFIER id)) -> expr' id, "Bytes.t"
| VOID -> "()", "unit"
| oth -> othp := oth; failwith "pdump"

let expr locals x = fst(texpr locals x)

let rec sdump locals = function
|  TUPLE3
   (STRING "expression_statement216",
    TUPLE5
     (STRING "postfix_expression8", (IDENTIFIER fn),
      LPAREN,
      TLIST params,
      RPAREN),
    SEMICOLON) -> depends fn; expr' fn^" "^String.concat " " (List.map (fun itm -> let id,t = texpr locals itm in id) (List.rev params))^";"
|  TUPLE3 (STRING "expression_statement216",
    TUPLE5 (STRING "postfix_expression8", IDENTIFIER fn, LPAREN, param, RPAREN),
    SEMICOLON) -> depends fn; expr' fn^" "^fst(texpr locals param)^";"
| TUPLE3 (STRING "expression_statement216",
    TUPLE4 (STRING "assignment_expression62", (IDENTIFIER id), EQUALS, rhs), SEMICOLON) ->
    let lhs = expr' id in
    let rhs,trhs = texpr locals rhs in
    let rhs' = try match List.assoc lhs !locals, trhs with
      | "Bytes.t",_ -> "(* "^lhs^"' := "^rhs^"; *)"
      | "Int64.t ref", "int" -> lhs^" := Int64.of_int ("^rhs^");"
      | "Int64.t", "int" -> lhs^" := Int64.of_int ("^rhs^");"
(*
      | "Int64.t ref", "Int64.t" -> rhs
      | "int ref",_ -> rhs
      | "char ref",_ -> rhs
*)
      | _,_ -> lhs^" := "^rhs^";" with _ -> "(* "^lhs^"*)"^rhs in rhs'
| TUPLE6 (STRING "selection_statement217", IF, LPAREN, cond, RPAREN,
          TUPLE4 (STRING "compound_statement210", LBRACE, stmt, RBRACE)) -> "if "^expr locals cond^" then begin "^sdump locals stmt^" end;"
| TUPLE8 (STRING "selection_statement218", IF, LPAREN, cond, RPAREN,
          TUPLE4 (STRING "compound_statement210", LBRACE, stmt, RBRACE), ELSE,
          TUPLE4 (STRING "compound_statement210", LBRACE, stmt', RBRACE)) -> "if "^expr locals cond^" then begin "^sdump locals stmt^" end else begin "^sdump locals stmt'^" end;"
|  TUPLE8
   (STRING "iteration_statement223", FOR, LPAREN, SEMICOLON,
    TUPLE3 (STRING "expression_statement216", CONSTANT "1", SEMICOLON), inc,
    RPAREN,
    TUPLE4
     (STRING "compound_statement210", LBRACE, TLIST stmts', RBRACE)) -> let stmts = List.rev ( TUPLE3 (STRING "expression_statement216", inc, SEMICOLON) :: stmts') in othlst := stmts; loop locals stmts
| TUPLE3 (STRING "jump_statement228", BREAK, SEMICOLON) -> "break;"
| TUPLE4 (STRING "jump_statement230", RETURN, rslt, SEMICOLON) -> 
    let rslt = match texpr locals rslt with
       | x, "Int64.t" -> "let rslt = "^x^" in rslt"
       | x, "Int64.t ref" -> "let rslt = !"^x^" in rslt"
       | x, "int" -> "let rslt = !"^x^" in rslt"
       | _, oth -> failwith ("return "^oth) in rslt
|  TUPLE3
   (STRING "expression_statement216",
    TUPLE4
     (STRING "assignment_expression62",
      TUPLE3
       (STRING "unary_expression20", STAR,
        TUPLE4
         (STRING "primary_expression4", LPAREN,
          TUPLE4
           (STRING "additive_expression36", (IDENTIFIER id' as id), PLUS, idx),
          RPAREN)),
      EQUALS, rhs),
    SEMICOLON) -> (match texpr locals id, texpr locals rhs with
        | (_,"Int64.t array"),(_,"int") -> expr' id'^".("^idxcast locals idx^") <- "^expr locals rhs^";"
        | (_,"Int64.t array"),(_,"Int64.t") -> expr' id'^".("^idxcast locals idx^") <- "^expr locals rhs^";"
        | (_,"Int64.t array"),(_,"Int64.t ref") -> expr' id'^".("^idxcast locals idx^") <- !("^expr locals rhs^");"
        | (_,"Bytes.t"),(_,"char") -> "Bytes.set "^expr' id'^" ("^idxcast locals idx^") ("^expr locals rhs^");"
        | (_,"Bytes.t"),(_,"int") -> "Bytes.set "^expr' id'^" ("^idxcast locals idx^") ("^expr locals rhs^");"
        | (_,"Bytes.t"),(_,"Int64.t") -> "Bytes.set "^expr' id'^" ("^idxcast locals idx^") ("^expr locals rhs^");"
        | (lft,tlft),(rght,trght) -> othasgn := Some (!locals,lft,rght,tlft,trght); failwith (tlft^":"^trght^":335"))
|  TUPLE3
   (STRING "expression_statement216",
    TUPLE4
     (STRING "assignment_expression62",
      TUPLE3 (STRING "unary_expression20", STAR, IDENTIFIER lhs), EQUALS, rhs),
    SEMICOLON) -> lhs^".(0) <- "^expr locals rhs^";"
|  TUPLE3
   (TUPLE3 (STRING "declaration_specifiers82", LONG, LONG),
    TUPLE5
     (STRING "direct_declarator148",
      TUPLE5
       (STRING "direct_declarator148", IDENTIFIER id, LBRACK, CONSTANT siz,
        RBRACK),
      LBRACK, CONSTANT siz', RBRACK),
    SEMICOLON) -> decl' locals id "Int64.t array array"^" = Array.make_matrix "^siz^" "^siz'^" 0L in"
|  TUPLE3
   (TUPLE3 (STRING "declaration_specifiers82", LONG, LONG),
    TUPLE5
     (STRING "direct_declarator148", IDENTIFIER id, LBRACK, CONSTANT siz,
      RBRACK),
    SEMICOLON) -> decl' locals id "Int64.t array"^" = Array.make "^siz^" 0L in"
|  TUPLE3
   (TUPLE3 (STRING "declaration_specifiers82", LONG, LONG), IDENTIFIER id,
    SEMICOLON) -> decl' locals id "Int64.t"^" = 0L in"
|  TUPLE3
   (TUPLE3 (STRING "declaration_specifiers82", UNSIGNED, CHAR),
    TUPLE5
     (STRING "direct_declarator148", IDENTIFIER id, LBRACK, CONSTANT siz,
      RBRACK),
    SEMICOLON) -> decl' locals id "Bytes.t"^" = Bytes.make "^siz^" in"
| TUPLE3
   (TUPLE3
     (STRING "declaration_specifiers80", REGISTER,
      TUPLE3 (STRING "declaration_specifiers82", UNSIGNED, CHAR)),
    IDENTIFIER id, SEMICOLON) -> decl' locals id "char ref"^" = ref ' ' in"
| TUPLE3 (INT, IDENTIFIER id, SEMICOLON) -> decl' locals id "int ref"^" = ref 0 in"
| TUPLE3
   (TUPLE3 (STRING "declaration_specifiers80", REGISTER, INT),
    IDENTIFIER id, SEMICOLON) -> decl' locals id "int ref"^" = ref 0 in"
| TUPLE3
   (TUPLE3 (STRING "declaration_specifiers82", UNSIGNED, CHAR),
    (IDENTIFIER id), SEMICOLON) -> decl' locals id "char ref"^" = ref ' ' in"
| TUPLE3
   (TUPLE3
     (STRING "declaration_specifiers82", UNSIGNED,
      TUPLE3 (STRING "declaration_specifiers82", LONG, LONG)),
    (IDENTIFIER id), SEMICOLON) -> decl' locals id "Int64.t ref"^" = ref 0L in"
| TUPLE3
   (TUPLE3
     (STRING "declaration_specifiers82", UNSIGNED,
      TUPLE3 (STRING "declaration_specifiers82", LONG, LONG)),
    TUPLE5
     (STRING "direct_declarator148", (IDENTIFIER id), LBRACK, CONSTANT siz,
      RBRACK),
    SEMICOLON) -> decl' locals id "Int64.t array"^" = Array.make "^siz^" 0L in"
|  TUPLE3
   (TUPLE3
     (STRING "declaration_specifiers80", REGISTER,
      TUPLE3
       (STRING "declaration_specifiers82", UNSIGNED,
        TUPLE3 (STRING "declaration_specifiers82", LONG, LONG))),
    (IDENTIFIER id), SEMICOLON) -> decl' locals id "Int64.t ref"^" = ref 0L in"
| TUPLE6 (STRING "iteration_statement220", WHILE, LPAREN, CONSTANT "1", RPAREN,
    TUPLE4 (STRING "compound_statement210", LBRACE, TLIST stmts, RBRACE)) -> (match List.rev stmts with
  | TUPLE6 (STRING "selection_statement217", IF, LPAREN,
     TUPLE3 (STRING "unary_expression20", PLING,
       TUPLE4 (STRING "primary_expression4", LPAREN, cond, RPAREN)),
     RPAREN,
     TUPLE4 (STRING "compound_statement210", LBRACE,
       TUPLE3 (STRING "jump_statement228", BREAK, SEMICOLON), RBRACE)) :: stmts' -> othlst := stmts';
      "while "^expr locals cond^" do "^String.concat "\n" (List.map (sdump locals) stmts')^" done;"
  | oth -> othlst := oth; failwith "while")
| TLIST stmts -> String.concat "\n" (List.map (sdump locals) (List.rev stmts))
| oth -> othstmt := oth; failwith "sdump"

and loop locals = function
| TUPLE6
    (STRING "selection_statement217", IF, LPAREN,
     TUPLE3
      (STRING "unary_expression20", PLING,
       TUPLE4
        (STRING "primary_expression4", LPAREN, relat, RPAREN)),
     RPAREN,
     TUPLE4
      (STRING "compound_statement210", LBRACE,
       TUPLE3 (STRING "jump_statement228", BREAK, SEMICOLON), RBRACE)) :: stmts -> "while "^expr locals relat^" do "^String.concat "\n" (List.map (sdump locals) stmts)^" done;"
| stmts -> String.concat "\n" (List.map (sdump locals) stmts)

let dumpfunc nam = function
| (typ, plst, bodylst) ->
  let params = List.map pdump (List.rev plst) in
  let locals = ref params in
  let rslt = "and "^expr' nam^" "^String.concat " " (List.map (fun (k,t) -> match t with
    | "Bytes.t" -> "(("^k^": "^t^"), ("^k^"' : int))"
    | oth ->  "("^k^": "^t^")") params)^" : "^ftyp' typ^" =\n"^String.concat "\n" (List.map (sdump locals) (List.rev bodylst)) in
  let tail = match List.hd bodylst with
    | TUPLE4 (STRING "jump_statement230", RETURN, rslt, SEMICOLON) -> ""
    | _ -> "\n()" in rslt ^ tail ^ "\n"
| _ -> failwith "dumpfunc"

let prtlst = ref []

let rec dumpfunc' nam =
  prtlst := (nam, (if Hashtbl.mem fbody nam then dumpfunc nam (Hashtbl.find fbody nam) else ("(* skipped: "^nam^" *)"))) :: !prtlst;
  List.iter (fun itm -> if not (List.mem_assoc itm !prtlst) then (print_endline ("dump func: "^itm); dumpfunc' itm)) !deplst

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

let tparam = function
| TUPLE3 (STRING "parameter_declaration167",
          TUPLE3
           (STRING "declaration_specifiers82", UNSIGNED,
            TUPLE3 (STRING "declaration_specifiers82", LONG, LONG)),
          IDENTIFIER "u") -> ()
| TUPLE3 (STRING "parameter_declaration167",
          TUPLE3 (STRING "declaration_specifiers82", UNSIGNED, CHAR),
          TUPLE3 (STRING "declarator142", STAR, IDENTIFIER x)) -> ()
| TUPLE3 (STRING "parameter_declaration167",
        TUPLE3 (STRING "declaration_specifiers82", UNSIGNED, CHAR),
        TUPLE3 (STRING "declarator142", STAR, IDENTIFIER x)) -> ()
| TUPLE3 (STRING "declaration_specifiers82", UNSIGNED,
      TUPLE3 (STRING "declaration_specifiers82", LONG, LONG)) -> ()
| TUPLE3 (STRING "parameter_declaration167",
        TUPLE3 (STRING "declaration_specifiers82", UNSIGNED, CHAR),
        TUPLE3 (STRING "declarator142", STAR, IDENTIFIER x)) -> ()

let ttyp = function
| VOID -> "unit"
| TUPLE3 (STRING "declaration_specifiers82", UNSIGNED,
      TUPLE3 (STRING "declaration_specifiers82", LONG, LONG)) -> "Int64.t"
| TUPLE3 (STRING "declaration_specifiers82", UNSIGNED,
      TUPLE3
       (STRING "declaration_specifiers82", LONG,
        TUPLE3 (STRING "declaration_specifiers82", LONG, CONST))) -> "Int64.t"
| TUPLE3 (STRING "declaration_specifiers82", SIGNED,
      TUPLE3 (STRING "declaration_specifiers82", CHAR, CONST)) -> "char"

let titer' = function
| TUPLE4 (STRING "function_definition236", typ,
    TUPLE5 (STRING "direct_declarator154", IDENTIFIER fn, LPAREN, TLIST params, RPAREN),
    TUPLE4 (STRING "compound_statement210", LBRACE, TLIST body, RBRACE)) -> Hashtbl.add fbody fn (typ,params,body)
| TUPLE4 (STRING "function_definition236", typ,
    TUPLE5 (STRING "direct_declarator154", IDENTIFIER fn, LPAREN, TLIST params, RPAREN),
    TUPLE4 (STRING "compound_statement210", LBRACE, body, RBRACE)) -> Hashtbl.add fbody fn (typ,params,[body])
| TUPLE4 (STRING "function_definition236", typ,
    TUPLE5 (STRING "direct_declarator154", IDENTIFIER fn, LPAREN, param, RPAREN),
    TUPLE4 (STRING "compound_statement210", LBRACE, TLIST body, RBRACE)) -> Hashtbl.add fbody fn (typ,[param],body)
| TUPLE3 (typ,
    TUPLE4 (STRING "init_declarator90",
      TUPLE5 (STRING "direct_declarator148", IDENTIFIER id, LBRACK, CONSTANT siz, RBRACK),
      EQUALS,
      TUPLE5 (STRING "initializer190", LBRACE, TLIST init, COMMA, RBRACE)),
    SEMICOLON) -> Hashtbl.add globals id (typ,siz,init)
| TUPLE3 (typ,
    TUPLE4 (STRING "init_declarator90",
      TUPLE5 (STRING "direct_declarator148", IDENTIFIER id, LBRACK, CONSTANT siz, RBRACK),
      EQUALS,
      TUPLE5 (STRING "initializer190", LBRACE, (CONSTANT _ as init), COMMA, RBRACE)),
    SEMICOLON) -> Hashtbl.add globals id (typ,siz,[init])
| TUPLE3 (typ,
    TUPLE5 (STRING "direct_declarator148", IDENTIFIER id, LBRACK, CONSTANT siz, RBRACK),
    SEMICOLON) -> Hashtbl.add globals id (typ,siz,[])
| TUPLE3 (typ,
    TUPLE4 (STRING "init_declarator90",
      TUPLE5 (STRING "direct_declarator148", IDENTIFIER id, LBRACK, CONSTANT siz, RBRACK),
      EQUALS, (STRING_LITERAL _ as s)), SEMICOLON) -> Hashtbl.add globals id (typ,siz,[s])
| TUPLE3 (typ,
    TUPLE5 (STRING "direct_declarator154", IDENTIFIER id, LPAREN, params, RPAREN), SEMICOLON) -> ()
| TUPLE3
   (TUPLE3 (STRING "declaration_specifiers80", EXTERN, VOID),
    TUPLE3
     (STRING "declarator142", STAR,
      TUPLE5
       (STRING "direct_declarator154", IDENTIFIER "__compcert_va_composite",
        LPAREN, TLIST lst, RPAREN)), SEMICOLON) -> ()
| oth -> othstmt := oth; failwith "titer'"

let titer = function
| TUPLE2 (TLIST lst, EOF_TOKEN) -> List.iter (titer') lst
| oth -> othstmt := oth; failwith "titer"

let rec dumpg fd = function
| CONSTANT c ->
  let len = String.length c in if len > 2 && String.sub c (len - 2) 2 = "LL" then output_string fd (String.sub c 0 (len - 1)^";\n")
  else output_string fd (c^";\n")
| TUPLE3 (STRING "unary_expression20", HYPHEN, (CONSTANT _ as c)) ->
    output_string fd ("-");
    dumpg fd c
| oth -> othstmt := oth; failwith "dumpg"

let dumpfunc tree nam =
  prtlst := [];
  dbgtree := truncate tree;
  titer !dbgtree;
  dumpfunc' nam;
  let fd = open_out (nam^"_template.ml") in
  List.iter (fun itm -> let (typ,siz,init) = Hashtbl.find globals itm in
    output_string fd ("let "^expr' itm^" = [|\n");
    List.iter (dumpg fd) (List.rev init);
    output_string fd "|]\n\n") (List.sort compare !globlst);
  output_string fd "let rec omega n = omega n\n\n";
  List.iter (fun (k,x) -> output_string fd (x^"\n")) !prtlst;
  close_out fd
