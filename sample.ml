open Translation_unit_list
open Translation_unit_list_transform

let othp = ref EOF_TOKEN
let othexpr = ref EOF_TOKEN
let othstmt = ref EOF_TOKEN

let deplst = ref []
let depends x = if not (List.mem x !deplst) then deplst := x :: !deplst

let rec expr = function
| IDENTIFIER id -> if id.[0] = '$' then "_'"^String.sub id 1 (String.length id - 1) else if id.[0] <= 'Z' then "_"^id else id
| CONSTANT n -> n
| TUPLE4 (STRING "additive_expression36", lft, PLUS, rght) -> expr lft^" + "^expr rght
| TUPLE4 (STRING "additive_expression37", lft, HYPHEN, rght) -> expr lft^" - "^expr rght
| TUPLE4 (STRING "multiplicative_expression32", lft, STAR, rght) -> expr lft^"*"^expr rght
| TUPLE4 (STRING "multiplicative_expression33", lft, SLASH, rght) -> expr lft^"/"^expr rght
| TUPLE4 (STRING "multiplicative_expression34", lft, PERCENT, rght) -> expr lft^" mod "^expr rght
| TUPLE3 (STRING "unary_expression20", STAR,
          TUPLE4
           (STRING "primary_expression4", LPAREN,
            TUPLE4
             (STRING "additive_expression36", IDENTIFIER id', PLUS, idx),
            RPAREN)) -> id'^".("^expr idx^")"
| TUPLE4 (STRING "and_expression50", lft, AMPERSAND, rght) -> "bitand ("^expr lft^") ("^expr rght^")"
| TUPLE4 (STRING "exclusive_or_expression52", lft, CARET, rght) -> "bitxor ("^expr lft^") ("^expr rght^")"
| TUPLE4 (STRING "inclusive_or_expression54", lft, VBAR, rght) -> "bitor ("^expr lft^") ("^expr rght^")"
| TUPLE3 (STRING "unary_expression20", PLING, rhs) -> "not ("^expr rhs^")"
| TUPLE3 (STRING "unary_expression20", TILDE, rhs) -> "bitnot ("^expr rhs^")"
| TUPLE4 (STRING "equality_expression47", lft, EQ_OP, rght) -> expr lft^" == "^expr rght
| TUPLE4 (STRING "equality_expression48", lft, NE_OP, rght) -> expr lft^" != "^expr rght
| TUPLE4 (STRING "relational_expression42", lft, LESS, rght) -> expr lft^" < "^expr rght
| TUPLE4 (STRING "relational_expression45", lft, GE_OP, rght) -> expr lft^" >= "^expr rght
| TUPLE4 (STRING "primary_expression4", LPAREN, expr', RPAREN) -> "("^expr expr'^")"
| TUPLE5 (STRING "cast_expression30", LPAREN, TLIST [TLIST [LONG; LONG]; UNSIGNED], RPAREN, expr') -> "cast("^expr expr'^")"
| TUPLE5 (STRING "cast_expression30", LPAREN, BOOL, RPAREN, expr') -> "cast("^expr expr'^")"
| TUPLE5 (STRING "postfix_expression8", (IDENTIFIER x as fn), LPAREN, expr', RPAREN) -> depends x; "( "^expr fn^" "^expr expr'^")"
| TUPLE4 (STRING "shift_expression39", lft, LEFT_OP, rght) -> "("^expr lft^" shl "^expr rght^")"
| TUPLE4 (STRING "shift_expression40", lft, RIGHT_OP, rght) -> "("^expr lft^" shr "^expr rght^")"
| TLIST lst -> String.concat ", " (List.map expr lst)
| oth -> othexpr := oth; failwith "expr"

let pdump = function
|  TUPLE3
   (STRING "parameter_declaration167",
    TUPLE3 (STRING "declaration_specifiers82", UNSIGNED, CHAR),
    TUPLE3 (STRING "declarator142", STAR, IDENTIFIER id)) -> "("^id^": Bytes.t)"
|  TUPLE3
   (STRING "parameter_declaration167",
    TUPLE3
     (STRING "declaration_specifiers82", UNSIGNED,
      TUPLE3 (STRING "declaration_specifiers82", LONG, LONG)),
    IDENTIFIER id) -> "("^id^": Int64.t)"
|  TUPLE3
   (STRING "parameter_declaration167",
    TUPLE3
     (STRING "declaration_specifiers82", UNSIGNED,
      TUPLE3 (STRING "declaration_specifiers82", LONG, LONG)),
    TUPLE3 (STRING "declarator142", STAR, IDENTIFIER id)) -> "("^id^": Int64.t array)"
| TUPLE3
   (STRING "parameter_declaration167",
    TUPLE3 (STRING "declaration_specifiers82", LONG, LONG),
    TUPLE5
     (STRING "direct_declarator148",
      TUPLE4
       (STRING "direct_declarator145", LPAREN,
        TUPLE3 (STRING "declarator142", STAR, IDENTIFIER id), RPAREN),
      LBRACK, CONSTANT siz, RBRACK)) -> "("^id^": Int64.t array)"
| TUPLE3
   (STRING "parameter_declaration167",
    TUPLE3 (STRING "declaration_specifiers82", LONG, LONG),
    TUPLE3 (STRING "declarator142", STAR, IDENTIFIER id)) -> "("^id^": Int64.t)"
| TUPLE3 (STRING "parameter_declaration167", INT, (IDENTIFIER _ as id)) -> "("^expr id^": int)"
| TUPLE3 (STRING "parameter_declaration167",
    TUPLE3 (STRING "declaration_specifiers82", UNSIGNED, CHAR),
    (IDENTIFIER _ as id)) -> "("^expr id^": Bytes.t)"
| oth -> othp := oth; failwith "pdump"

let rec sdump = function
|  TUPLE3
   (STRING "expression_statement216",
    TUPLE5
     (STRING "postfix_expression8", (IDENTIFIER x as fn),
      LPAREN,
      TLIST params,
      RPAREN),
    SEMICOLON) -> depends x; "let _ = "^expr fn^" "^String.concat " " (List.map expr (List.rev params))^" in"
|  TUPLE3 (STRING "expression_statement216",
    TUPLE5 (STRING "postfix_expression8", IDENTIFIER fn, LPAREN, param, RPAREN),
    SEMICOLON) -> "let _ = "^fn^" "^expr param^" in"
| TUPLE3 (STRING "expression_statement216",
    TUPLE4 (STRING "assignment_expression62", (IDENTIFIER _ as id), EQUALS, rhs), SEMICOLON) -> expr id^" := "^expr rhs^";"
| TUPLE6 (STRING "selection_statement217", IF, LPAREN, cond, RPAREN,
          TUPLE4 (STRING "compound_statement210", LBRACE, stmt, RBRACE)) -> "if "^expr cond^" then begin "^sdump stmt^" end"
| TUPLE8 (STRING "selection_statement218", IF, LPAREN, cond, RPAREN,
          TUPLE4 (STRING "compound_statement210", LBRACE, stmt, RBRACE), ELSE,
          TUPLE4 (STRING "compound_statement210", LBRACE, stmt', RBRACE)) -> "if "^expr cond^" then begin "^sdump stmt^" end else begin "^sdump stmt'^" end"
|  TUPLE8
   (STRING "iteration_statement223", FOR, LPAREN, SEMICOLON,
    TUPLE3 (STRING "expression_statement216", CONSTANT "1", SEMICOLON), inc,
    RPAREN,
    TUPLE4
     (STRING "compound_statement210", LBRACE, TLIST stmts', RBRACE)) -> let stmts = List.rev ( TUPLE3 (STRING "expression_statement216", inc, SEMICOLON) :: stmts') in othlst := stmts; loop stmts
| TUPLE3 (STRING "jump_statement228", BREAK, SEMICOLON) -> "break;"
| TUPLE4 (STRING "jump_statement230", RETURN, rslt, SEMICOLON) -> "let rslt = "^expr rslt^" in rslt\n"
|  TUPLE3
   (STRING "expression_statement216",
    TUPLE4
     (STRING "assignment_expression62",
      TUPLE3
       (STRING "unary_expression20", STAR,
        TUPLE4
         (STRING "primary_expression4", LPAREN,
          TUPLE4
           (STRING "additive_expression36", IDENTIFIER id, PLUS, idx),
          RPAREN)),
      EQUALS, expr'),
    SEMICOLON) -> id^".("^expr idx^") <- "^expr expr'^";"
|  TUPLE3
   (STRING "expression_statement216",
    TUPLE4
     (STRING "assignment_expression62",
      TUPLE3 (STRING "unary_expression20", STAR, IDENTIFIER lhs), EQUALS, rhs),
    SEMICOLON) -> lhs^".(0) <- "^expr rhs^";"
|  TUPLE3
   (TUPLE3 (STRING "declaration_specifiers82", LONG, LONG),
    TUPLE5
     (STRING "direct_declarator148",
      TUPLE5
       (STRING "direct_declarator148", id, LBRACK, CONSTANT siz,
        RBRACK),
      LBRACK, CONSTANT siz', RBRACK),
    SEMICOLON) -> "let ("^expr id^":  = Array.make_matrix "^siz^" "^siz'^" in"
|  TUPLE3
   (TUPLE3 (STRING "declaration_specifiers82", LONG, LONG),
    TUPLE5
     (STRING "direct_declarator148", id, LBRACK, CONSTANT siz,
      RBRACK),
    SEMICOLON) -> "let ("^expr id^": Int64.t array) = Array.make "^siz^" 0L in"
|  TUPLE3
   (TUPLE3 (STRING "declaration_specifiers82", LONG, LONG), id,
    SEMICOLON) -> "let "^expr id^" = 0L in"
|  TUPLE3
   (TUPLE3 (STRING "declaration_specifiers82", UNSIGNED, CHAR),
    TUPLE5
     (STRING "direct_declarator148", id, LBRACK, CONSTANT siz,
      RBRACK),
    SEMICOLON) -> "let "^expr id^" = Bytes.make "^siz^" in"
| TUPLE3
   (TUPLE3
     (STRING "declaration_specifiers80", REGISTER,
      TUPLE3 (STRING "declaration_specifiers82", UNSIGNED, CHAR)),
    id, SEMICOLON) -> "let "^expr id^" = Bytes.make 1 in"
| TUPLE3 (INT, id, SEMICOLON) -> "let "^expr id^" = ref 0 in"
| TUPLE3
   (TUPLE3 (STRING "declaration_specifiers80", REGISTER, INT),
    id, SEMICOLON) -> "let "^expr id^" = ref 0 in"
| TUPLE3
   (TUPLE3 (STRING "declaration_specifiers82", UNSIGNED, CHAR),
    (IDENTIFIER _ as id), SEMICOLON) -> "let "^expr id^" = Bytes.make 1 ' ' in"
| TUPLE3
   (TUPLE3
     (STRING "declaration_specifiers82", UNSIGNED,
      TUPLE3 (STRING "declaration_specifiers82", LONG, LONG)),
    (IDENTIFIER _ as id), SEMICOLON) -> "let "^expr id^" = ref 0L in"
| TUPLE3
   (TUPLE3
     (STRING "declaration_specifiers82", UNSIGNED,
      TUPLE3 (STRING "declaration_specifiers82", LONG, LONG)),
    TUPLE5
     (STRING "direct_declarator148", (IDENTIFIER _ as id), LBRACK, CONSTANT siz,
      RBRACK),
    SEMICOLON) -> "let "^expr id^" = Array.make "^siz^" 0L in"
|  TUPLE3
   (TUPLE3
     (STRING "declaration_specifiers80", REGISTER,
      TUPLE3
       (STRING "declaration_specifiers82", UNSIGNED,
        TUPLE3 (STRING "declaration_specifiers82", LONG, LONG))),
    (IDENTIFIER _ as id), SEMICOLON) -> "let "^expr id^" = ref 0L in"
| TUPLE6 (STRING "iteration_statement220", WHILE, LPAREN, cond, RPAREN,
    TUPLE4 (STRING "compound_statement210", LBRACE, TLIST stmts, RBRACE)) ->
      "while "^expr cond^" do "^String.concat "\n" (List.map sdump (List.rev stmts))^" done;"
| TLIST stmts -> String.concat "\n" (List.map sdump (List.rev stmts))
| oth -> othstmt := oth; failwith "sdump"

and loop = function
| TUPLE6
    (STRING "selection_statement217", IF, LPAREN,
     TUPLE3
      (STRING "unary_expression20", PLING,
       TUPLE4
        (STRING "primary_expression4", LPAREN, relat, RPAREN)),
     RPAREN,
     TUPLE4
      (STRING "compound_statement210", LBRACE,
       TUPLE3 (STRING "jump_statement228", BREAK, SEMICOLON), RBRACE)) :: stmts -> "while "^expr relat^" do "^String.concat "\n" (List.map sdump stmts)^" done;"
| stmts -> String.concat "\n" (List.map sdump stmts)

let dumpfunc nam = function
| B (typ, plst, bodylst) -> "let "^nam^" "^String.concat " " (List.map (pdump) (List.rev plst))^" =\n"^String.concat "\n" (List.map (sdump) (List.rev bodylst))
| _ -> failwith "dumpfunc"

let prtlst = ref []

let rec dumpfunc' nam =
  prtlst := (nam, dumpfunc nam (Hashtbl.find fbody nam)) :: !prtlst;
  List.iter (fun itm -> if not (List.mem_assoc itm !prtlst) then (print_endline itm; dumpfunc' itm)) !deplst

let dumpfunc nam =
  prtlst := [];
  dumpfunc' nam;
  let fd = open_out (nam^"_template.ml") in
  List.iter (fun (k,x) -> output_string fd (x^"\n")) !prtlst;
  close_out fd
