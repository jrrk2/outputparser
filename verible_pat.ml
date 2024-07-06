     open Source_text_verible
     
     let othpat = ref End_of_file
     let patlst = ref []
     let othpatlst = ref []

let cnt = Array.init 800 (fun _ -> ref 0)

let  f104 () = incr cnt.(104)
let  f110 () = incr cnt.(110)
let  f114 () = incr cnt.(114)
let  f12 () = incr cnt.(12)
let  f120 () = incr cnt.(120)
let  f124 () = incr cnt.(124)
let  f127 () = incr cnt.(127)
let  f128 () = incr cnt.(128)
let  f129 () = incr cnt.(129)
let  f130 () = incr cnt.(130)
let  f131 () = incr cnt.(131)
let  f132 () = incr cnt.(132)
let  f133 () = incr cnt.(133)
let  f134 () = incr cnt.(134)
let  f138 () = incr cnt.(138)
let  f139 () = incr cnt.(139)
let  f140 () = incr cnt.(140)
let  f142 () = incr cnt.(142)
let  f144 () = incr cnt.(144)
let  f145 () = incr cnt.(145)
let  f149 () = incr cnt.(149)
let  f152 () = incr cnt.(152)
let  f156 () = incr cnt.(156)
let  f157 () = incr cnt.(157)
let  f159 () = incr cnt.(159)
let  f16 () = incr cnt.(16)
let  f163 () = incr cnt.(163)
let  f166 () = incr cnt.(166)
let  f169 () = incr cnt.(169)
let  f174 () = incr cnt.(174)
let  f175 () = incr cnt.(175)
let  f176 () = incr cnt.(176)
let  f177 () = incr cnt.(177)
let  f178 () = incr cnt.(178)
let  f179 () = incr cnt.(179)
let  f18 () = incr cnt.(18)
let  f180 () = incr cnt.(180)
let  f184 () = incr cnt.(184)
let  f187 () = incr cnt.(187)
let  f19 () = incr cnt.(19)
let  f191 () = incr cnt.(191)
let  f192 () = incr cnt.(192)
let  f193 () = incr cnt.(193)
let  f194 () = incr cnt.(194)
let  f195 () = incr cnt.(195)
let  f197 () = incr cnt.(197)
let  f200 () = incr cnt.(200)
let  f204 () = incr cnt.(204)
let  f208 () = incr cnt.(208)
let  f209 () = incr cnt.(209)
let  f21 () = incr cnt.(21)
let  f210 () = incr cnt.(210)
let  f211 () = incr cnt.(211)
let  f212 () = incr cnt.(212)
let  f213 () = incr cnt.(213)
let  f214 () = incr cnt.(214)
let  f215 () = incr cnt.(215)
let  f22 () = incr cnt.(22)
let  f220 () = incr cnt.(220)
let  f226 () = incr cnt.(226)
let  f232 () = incr cnt.(232)
let  f233 () = incr cnt.(233)
let  f234 () = incr cnt.(234)
let  f24 () = incr cnt.(24)
let  f241 () = incr cnt.(241)
let  f244 () = incr cnt.(244)
let  f245 () = incr cnt.(245)
let  f249 () = incr cnt.(249)
let  f25 () = incr cnt.(25)
let  f253 () = incr cnt.(253)
let  f256 () = incr cnt.(256)
let  f258 () = incr cnt.(258)
let  f260 () = incr cnt.(260)
let  f264 () = incr cnt.(264)
let  f271 () = incr cnt.(271)
let  f274 () = incr cnt.(274)
let  f277 () = incr cnt.(277)
let  f279 () = incr cnt.(279)
let  f282 () = incr cnt.(282)
let  f284 () = incr cnt.(284)
let  f286 () = incr cnt.(286)
let  f287 () = incr cnt.(287)
let  f289 () = incr cnt.(289)
let  f29 () = incr cnt.(29)
let  f291 () = incr cnt.(291)
let  f293 () = incr cnt.(293)
let  f294 () = incr cnt.(294)
let  f295 () = incr cnt.(295)
let  f296 () = incr cnt.(296)
let  f297 () = incr cnt.(297)
let  f298 () = incr cnt.(298)
let  f299 () = incr cnt.(299)
let  f300 () = incr cnt.(300)
let  f302 () = incr cnt.(302)
let  f306 () = incr cnt.(306)
let  f31 () = incr cnt.(31)
let  f310 s id lst lst' = incr cnt.(310); print_endline s; print_endline id
let  f315 () = incr cnt.(315)
let  f319 () = incr cnt.(319)
let  f320 () = incr cnt.(320)
let  f321 () = incr cnt.(321)
let  f322 ()  = incr cnt.(322)
let  f323 () = incr cnt.(323)
let  f324 () = incr cnt.(324)
let  f325 () = incr cnt.(325)
let  f326 () = incr cnt.(326)
let  f33 () = incr cnt.(33)
let  f338 () = incr cnt.(338)
let  f341 () = incr cnt.(341)
let  f348 () = incr cnt.(348)
let  f35 () = incr cnt.(35)
let  f351 () = incr cnt.(351)
let  f363 () = incr cnt.(363)
let  f37 () = incr cnt.(37)
let  f375 () = incr cnt.(375)
let  f382 () = incr cnt.(382)
let  f39 () = incr cnt.(39)
let  f391 () = incr cnt.(391)
let  f398 () = incr cnt.(398)
let  f406 () = incr cnt.(406)
let  f418 () = incr cnt.(418)
let  f427 () = incr cnt.(427)
let  f434 () = incr cnt.(434)
let  f442 () = incr cnt.(442)
let  f452 () = incr cnt.(452)
let  f46 () = incr cnt.(46)
let  f460 () = incr cnt.(460)
let  f472 () = incr cnt.(472)
let  f479 () = incr cnt.(479)
let  f486 () = incr cnt.(486)
let  f491 () = incr cnt.(491)
let  f494 () = incr cnt.(494)
let  f497 () = incr cnt.(497)
let  f499 () = incr cnt.(499)
let  f507 () = incr cnt.(507)
let  f513 () = incr cnt.(513)
let  f521 () = incr cnt.(521)
let  f527 () = incr cnt.(527)
let  f530 () = incr cnt.(530)
let  f54 () = incr cnt.(54)
let  f542 () = incr cnt.(542)
let  f552 () = incr cnt.(552)
let  f553 () = incr cnt.(553)
let  f556 () = incr cnt.(556)
let  f557 () = incr cnt.(557)
let  f561 () = incr cnt.(561)
let  f565 () = incr cnt.(565)
let  f568 () = incr cnt.(568)
let  f572 () = incr cnt.(572)
let  f576 () = incr cnt.(576)
let  f582 () = incr cnt.(582)
let  f588 () = incr cnt.(588)
let  f592 () = incr cnt.(592)
let  f596 () = incr cnt.(596)
let  f60 () = incr cnt.(60)
let  f600 () = incr cnt.(600)
let  f601 () = incr cnt.(601)
let  f609 () = incr cnt.(609)
let  f610 () = incr cnt.(610)
let  f612 () = incr cnt.(612)
let  f613 () = incr cnt.(613)
let  f614 () = incr cnt.(614)
let  f617 () = incr cnt.(617)
let  f622 () = incr cnt.(622)
let  f631 () = incr cnt.(631)
let  f639 () = incr cnt.(639)
let  f64 () = incr cnt.(64)
let  f646 () = incr cnt.(646)
let  f659 () = incr cnt.(659)
let  f667 ()  = incr cnt.(667)
let  f668 () = incr cnt.(668)
let  f67 () = incr cnt.(67)
let  f684 () = incr cnt.(684)
let  f69 ()  = incr cnt.(69)
let  f697 () = incr cnt.(697)
let  f70 () = incr cnt.(70)
let  f703 () = incr cnt.(703)
let  f706 () = incr cnt.(706)
let  f81 () = incr cnt.(81)
let  f89 () = incr cnt.(89)
let  f9 () = incr cnt.(9)
let  f96 () = incr cnt.(96)

let rec verible_pat' = function
     | TUPLE9 (STRING "task_declaration1", Task, EMPTY_TOKEN, SymbolIdentifier id,
   EMPTY_TOKEN, SEMICOLON, TLIST lst, Endtask, EMPTY_TOKEN) -> f9 (); List.iter verible_pat lst
    | TUPLE8 (STRING "case_statement1", EMPTY_TOKEN, Case, LPAREN,
       expr,
   RPAREN, case_items1, Endcase) -> f12 ()
    | TUPLE7 (STRING "conditional_statement2", EMPTY_TOKEN, If,
       expression_in_parens1,
       then_statement,
   Else, else_statement) -> f16 ()
    | TUPLE7 (STRING "expr_primary_braces2", LBRACE, braced, LBRACE,
  reference3, RBRACE, RBRACE) -> f18 ()
| TUPLE6 (STRING "cond_expr2", expr_primary, QUERY, expr_true, COLON, expr_false) -> f19 ()
    | TUPLE6 (STRING "continuous_assign1", Assign, EMPTY_TOKEN, dly, TLIST lst,
  SEMICOLON) -> f21 (); List.iter verible_pat lst
| TUPLE6 (STRING "decl_variable_dimension1", LBRACK, TK_DecNumber hi, COLON, TK_DecNumber lo, RBRACK) -> f22 ()
    | TUPLE6 (STRING "non_anonymous_gate_instance_or_register_variable2",
  SymbolIdentifier id, EMPTY_TOKEN, LPAREN, TLIST lst, RPAREN) -> f24 (); List.iter verible_pat lst
| TUPLE6 (STRING "nonblocking_assignment1", lhs, LT_EQ, EMPTY_TOKEN, rhs, SEMICOLON) -> f25 ()
    | TUPLE6 (STRING "param_type_followed_by_id_and_dimensions_opt4", (Logic|EMPTY_TOKEN),
       EMPTY_TOKEN, TUPLE6 (STRING "decl_variable_dimension1", LBRACK, TK_DecNumber hi, COLON,
         TK_DecNumber lo, RBRACK),
   SymbolIdentifier id, EMPTY_TOKEN) -> f29 ()
    | TUPLE6 (STRING "parameter_value_byname1", DOT, SymbolIdentifier id, LPAREN,
  TK_DecNumber num, RPAREN) -> f31 ()
    | TUPLE6 (STRING "parameter_value_byname1", DOT, SymbolIdentifier id, LPAREN,
   unqualified_id1, RPAREN) -> f33 ()
    | TUPLE6 (STRING "port_named1", DOT, SymbolIdentifier id, LPAREN,
  unqualified_id1, RPAREN) -> f35 ()
    | TUPLE6 (STRING "select_variable_dimension1", LBRACK, TK_DecNumber hi, COLON,
   TK_DecNumber lo, RBRACK) -> f37 ()
    | TUPLE6 (STRING "select_variable_dimension3", LBRACK, unqualified_id1,
  PLUS_COLON, unqualified_id2, RBRACK) -> f39 ()
    | TUPLE5 (STRING "any_param_declaration4", Localparam,
       TUPLE7
       (STRING "param_type_followed_by_id_and_dimensions_opt2", EMPTY_TOKEN,
         EMPTY_TOKEN, unqualified_id1, EMPTY_TOKEN, SymbolIdentifier id,
         EMPTY_TOKEN),
       TUPLE4 (STRING "trailing_assign1", EQUALS, TK_DecNumber num, EMPTY_TOKEN),
   SEMICOLON) -> f46 ()
    | TUPLE5 (STRING "any_param_declaration4", Localparam,
       TUPLE7
       (STRING "param_type_followed_by_id_and_dimensions_opt2", EMPTY_TOKEN,
         EMPTY_TOKEN, unqualified_id1, EMPTY_TOKEN, SymbolIdentifier id,
         EMPTY_TOKEN),
       TUPLE4
       (STRING "trailing_assign1", EQUALS, cond_expr2, EMPTY_TOKEN),
    SEMICOLON) -> f54 ()
    | TUPLE5 (STRING "any_param_declaration4", Localparam,
       TUPLE5
       (STRING "param_type_followed_by_id_and_dimensions_opt3", EMPTY_TOKEN,
         EMPTY_TOKEN, unqualified_id1, EMPTY_TOKEN),
       TUPLE4 (STRING "trailing_assign1", EQUALS, rhs, EMPTY_TOKEN),
    SEMICOLON) -> f60 ()
    | TUPLE5 (STRING "any_param_declaration4", Localparam,
       param_type_followed_by_id_and_dimensions_opt4,
       TUPLE4 (STRING "trailing_assign1", EQUALS, range_list_in_braces1, EMPTY_TOKEN),
    SEMICOLON) -> f64 (); verible_pat range_list_in_braces1
    | TUPLE5 (STRING "conditional_generate_construct1",
       TUPLE3 (STRING "generate_if1", If, expression_in_parens1),
   generate_block1, Else, conditional_generate_construct1) -> f67 ()
    | TUPLE5 (STRING "conditional_statement1", EMPTY_TOKEN, If,
   expression_in_parens1, seq_block1) -> f69 () ; verible_pat seq_block1
| TUPLE5 (STRING "event_control2", AT, LPAREN, TLIST lst, RPAREN) -> f70 (); List.iter verible_pat lst
    | TUPLE5 (STRING "net_declaration2", Wire,
       TUPLE3
       (STRING "data_type_or_implicit1",
         TUPLE6
         (STRING "decl_variable_dimension1", LBRACK, TK_DecNumber hi, COLON,
           TK_DecNumber lo, RBRACK),
         EMPTY_TOKEN),
       TUPLE4
       (STRING "net_decl_assign1", SymbolIdentifier id, EQUALS,
         unqualified_id1),
   SEMICOLON) -> f81 ()
    | TUPLE5 (STRING "net_declaration2", Wire,
       TUPLE3
       (STRING "data_type_or_implicit1",
         TUPLE6
         (STRING "decl_variable_dimension1", LBRACK, TK_DecNumber hi, COLON,
           TK_DecNumber lo, RBRACK),
         EMPTY_TOKEN),
    net_variable1, SEMICOLON) -> f89 ()
    | TUPLE5 (STRING "port_declaration_noattr1", Output, EMPTY_TOKEN,
       TUPLE4 (STRING "data_type_or_implicit_basic_followed_by_id_and_dimensions_opt1",
         TUPLE3 (STRING "data_type_primitive1",
           TUPLE3 (STRING "data_type_primitive_scalar1", (Reg|Logic), EMPTY_TOKEN),
           EMPTY_TOKEN),
         unqualified_id1, EMPTY_TOKEN),
   EMPTY_TOKEN) -> f96 ()
    | TUPLE5 (STRING "port_declaration_noattr1", Output, EMPTY_TOKEN,
       TUPLE4 (STRING "data_type_or_implicit_basic_followed_by_id_and_dimensions_opt1",
         TUPLE3 (STRING "data_type_primitive1",
          TUPLE3 (STRING "data_type_primitive_scalar1", (Reg|Logic), EMPTY_TOKEN),
          TUPLE6 (STRING "decl_variable_dimension1", LBRACK, TK_DecNumber hi, COLON,
            TK_DecNumber lo, RBRACK)),
        unqualified_id1, EMPTY_TOKEN),
    EMPTY_TOKEN) -> f104 ()
   | TUPLE5 (STRING "port_declaration_noattr1", Output, EMPTY_TOKEN,
      TUPLE4 (STRING "data_type_or_implicit_basic_followed_by_id_and_dimensions_opt4",
        TUPLE6 (STRING "decl_variable_dimension1", LBRACK, TK_DecNumber hi, COLON,
          TK_DecNumber lo, RBRACK),
        unqualified_id1, EMPTY_TOKEN),
   EMPTY_TOKEN) -> f110 ()
   | TUPLE5 (STRING "port_declaration_noattr1", Output, EMPTY_TOKEN,
      TUPLE3 (STRING "type_identifier_or_implicit_basic_followed_by_id_and_dimensions_opt4",
        unqualified_id1, EMPTY_TOKEN),
    EMPTY_TOKEN) -> f114 ()
   | TUPLE5 (STRING "port_declaration_noattr1", Input, EMPTY_TOKEN,
      TUPLE4 (STRING "data_type_or_implicit_basic_followed_by_id_and_dimensions_opt4",
        TUPLE6 (STRING "decl_variable_dimension1", LBRACK, TK_DecNumber hi, COLON,
          TK_DecNumber lo, RBRACK),
        unqualified_id1, EMPTY_TOKEN),
   EMPTY_TOKEN) -> f120 ()
   | TUPLE5 (STRING "port_declaration_noattr1", Input, EMPTY_TOKEN,
      TUPLE3 (STRING "type_identifier_or_implicit_basic_followed_by_id_and_dimensions_opt4",
        unqualified_id1, EMPTY_TOKEN),
    EMPTY_TOKEN) -> f124 ()
   | TUPLE4 (STRING "assignment_statement_no_expr1",
      TUPLE4 (STRING "range_list_in_braces1", LBRACE, TLIST lst, RBRACE), EQUALS,
   ELIST []) -> f127 (); List.iter verible_pat lst
| TUPLE4 (STRING "assignment_statement_no_expr1", reference, EQUALS, rhs) -> f128 ()
| TUPLE4 (STRING "call_base1", LPAREN, TLIST lst, RPAREN) -> f129 (); List.iter verible_pat lst
| TUPLE4 (STRING "case_item1", sel, COLON, seq_block) -> f130 (); verible_pat seq_block
| TUPLE4 (STRING "case_item2", Default, COLON, SEMICOLON) -> f131 ()
| TUPLE4 (STRING "case_item2", Default, COLON, seq_block) -> f132 (); verible_pat seq_block
| TUPLE4 (STRING "cont_assign1", unqualified_id1, EQUALS, expr) -> f133 ()
| TUPLE4 (STRING "expr_primary_parens1", LPAREN, TLIST lst, RPAREN) -> f134 (); List.iter verible_pat lst
   | TUPLE4 (STRING "expression_in_parens1", LPAREN,
      TUPLE3 (STRING "system_tf_call1", SystemTFIdentifier id,
        TUPLE4 (STRING "call_base1", LPAREN, TLIST lst, RPAREN)),
    RPAREN) -> f138 (); List.iter verible_pat lst
| TUPLE4 (STRING "expression_in_parens1", LPAREN, cond_expr2, RPAREN) -> f139 ()
| TUPLE4 (STRING "expression_list_proper1", unqualified_id1, COMMA, ELIST []) -> f140 ()
   | TUPLE4 (STRING "gate_instance_or_register_variable1", SymbolIdentifier id,
   EMPTY_TOKEN, EMPTY_TOKEN) -> f142 ()
   | TUPLE4 (STRING "generate_block1", begin1,
  TLIST lst, TUPLE3 (end1, End, EMPTY_TOKEN)) -> f144 (); List.iter verible_pat lst
| TUPLE4 (STRING "generate_region1", Generate, TLIST lst, Endgenerate) -> f145 (); List.iter verible_pat lst
   | TUPLE4 (STRING "module_parameter_port1", Parameter,
      TUPLE5 (STRING "param_type_followed_by_id_and_dimensions_opt3", EMPTY_TOKEN,
        EMPTY_TOKEN, unqualified_id1, EMPTY_TOKEN),
    TUPLE4 (STRING "trailing_assign1", EQUALS, TK_DecNumber num, EMPTY_TOKEN)) -> f149 ()
   | TUPLE4 (STRING "module_parameter_port1", Parameter,
      param_type_followed_by_id_and_dimensions_opt4,
   TUPLE4 (STRING "trailing_assign1", EQUALS, TK_DecNumber num, EMPTY_TOKEN)) -> f152 ()
   | TUPLE4 (STRING "net_declaration1", Wire,
      TUPLE4 (STRING "net_decl_assign1", SymbolIdentifier id, EQUALS,
        unary_prefix_expr2),
    SEMICOLON) -> f156 ()
| TUPLE4 (STRING "net_declaration1", Wire, net_variable1, SEMICOLON) -> f157 ()
   | TUPLE4 (STRING "non_anonymous_gate_instance_or_register_variable1",
  SymbolIdentifier id, EMPTY_TOKEN, EMPTY_TOKEN) -> f159 ()
   | TUPLE4 (STRING "non_anonymous_gate_instance_or_register_variable1",
      SymbolIdentifier id, EMPTY_TOKEN,
      TUPLE3 (STRING "trailing_decl_assignment2", EQUALS,
     TUPLE3 (STRING "dec_based_number1", TK_DecBase "", TK_DecDigits ""))) -> f163 ()
   | TUPLE4 (STRING "non_anonymous_gate_instance_or_register_variable1",
      SymbolIdentifier id, EMPTY_TOKEN,
   TUPLE3 (STRING "trailing_decl_assignment2", EQUALS, TK_DecNumber num)) -> f166 ()
   | TUPLE4 (STRING "non_anonymous_gate_instance_or_register_variable1",
      SymbolIdentifier id, EMPTY_TOKEN,
   TUPLE3 (STRING "trailing_decl_assignment2", EQUALS, unary_prefix_expr2)) -> f169 ()
   | TUPLE4 (STRING "non_anonymous_gate_instance_or_register_variable1",
      SymbolIdentifier id,
      TUPLE6 (STRING "decl_variable_dimension1", LBRACK, TK_DecNumber hi, COLON,
        ELIST [], RBRACK),
    EMPTY_TOKEN) -> f174 ()
| TUPLE4 (STRING "range_list_in_braces1", LBRACE, TLIST lst, RBRACE) -> f175 (); List.iter verible_pat lst
| TUPLE4(STRING "select_variable_dimension2", LBRACK, TK_DecNumber lo, RBRACK) -> f176 ()
| TUPLE4 (STRING "select_variable_dimension2", LBRACK, unqualified_id1, RBRACK) -> f177 ()
| TUPLE4 (STRING "seq_block1", begin1, EMPTY_TOKEN, TUPLE3 (end1, End, EMPTY_TOKEN)) -> f178 ()
| TUPLE4 (STRING "seq_block1", begin1, TLIST lst, TUPLE3 (end1, End, EMPTY_TOKEN)) -> f179 (); List.iter verible_pat lst
| TUPLE3 (STRING "always_construct1", Always, procedural_timing_control_statement2) -> f180 ()
   | TUPLE3 (STRING "any_argument_list_item_last2",
      TUPLE3 (STRING "any_argument_list_trailing_comma1", TLIST lst, COMMA),
      TUPLE3 (STRING "reference2", unqualified_id1,
    TUPLE3 (STRING "hierarchy_extension1", DOT, unqualified_id2))) -> f184 (); List.iter verible_pat lst
   | TUPLE3 (STRING "any_argument_list_item_last2",
      TUPLE3 (STRING "any_argument_list_trailing_comma1", TLIST lst, COMMA),
   cond_expr2) -> f187 (); List.iter verible_pat lst
   | TUPLE3 (STRING "any_port_list_item_last1",
      TUPLE3 (STRING "any_port_list_trailing_comma1", TLIST lst, COMMA),
      TUPLE6 (STRING "port_named1", DOT, SymbolIdentifier id, LPAREN,
     unqualified_id1, RPAREN)) -> f191 (); List.iter verible_pat lst
| TUPLE3 (STRING "begin1", Begin, EMPTY_TOKEN) -> f192 ()
| TUPLE3 (STRING "bin_based_number1", TK_BinBase base, TK_BinDigits digits) -> f193 ()
| TUPLE3 (STRING "block_item_or_statement_or_null6", unqualified_id1, SEMICOLON) -> f194 ()
| TUPLE3 (STRING "case_items1", case_item1, case_item2) -> f195 ()
   | TUPLE3 (STRING "data_declaration_or_module_instantiation1",
   instantiation_base1, SEMICOLON) -> f197 ()
   | TUPLE3 (STRING "data_type_primitive1",
      TUPLE3 (STRING "data_type_primitive_scalar1", Reg, EMPTY_TOKEN),
   EMPTY_TOKEN) -> f200 ()
   | TUPLE3 (STRING "data_type_primitive1",
      TUPLE3 (STRING "data_type_primitive_scalar1", Reg, EMPTY_TOKEN),
      TUPLE6 (STRING "decl_variable_dimension1", LBRACK, TK_DecNumber hi, COLON,
    TK_DecNumber lo, RBRACK)) -> f204 ()
   | TUPLE3 (STRING "data_type_primitive1",
      TUPLE3 (STRING "data_type_primitive_scalar1", Reg, EMPTY_TOKEN),
      TUPLE6 (STRING "decl_variable_dimension1", LBRACK, ELIST [], COLON,
     TK_DecNumber lo, RBRACK)) -> f208 ()
| TUPLE3 (STRING "end1", End, EMPTY_TOKEN) -> f209 ()
| TUPLE3 (STRING "event_control4", AT, STAR) -> f210 ()
| TUPLE3 (STRING "event_expression1", Posedge, unqualified_id1) -> f211 ()
| TUPLE3 (STRING "event_expression1", Negedge, unqualified_id1) -> f212 ()
| TUPLE3 (STRING "expression_or_dist1", cond_expr2, EMPTY_TOKEN) -> f213 ()
| TUPLE3 (STRING "generate_if1", If, expression_in_parens1) -> f214 ()
| TUPLE3 (STRING "initial_construct1", Initial, seq_block1) -> f215 (); verible_pat seq_block1
   | TUPLE3 (STRING "instantiation_base1",
      TUPLE3 (STRING "data_type_primitive1",
        TUPLE3 (STRING "data_type_primitive_scalar1", Reg, EMPTY_TOKEN),
        EMPTY_TOKEN),
   TLIST lst) -> f220 (); List.iter verible_pat lst
   | TUPLE3 (STRING "instantiation_base1",
      TUPLE3 (STRING "data_type_primitive1",
        TUPLE3 (STRING "data_type_primitive_scalar1", Reg, EMPTY_TOKEN),
        TUPLE6 (STRING "decl_variable_dimension1", LBRACK, TK_DecNumber hi, COLON,
          TK_DecNumber lo, RBRACK)),
    TLIST lst) -> f226 (); List.iter verible_pat lst
   | TUPLE3 (STRING "instantiation_base1",
      TUPLE3 (STRING "data_type_primitive1",
        TUPLE3 (STRING "data_type_primitive_scalar1", Reg, EMPTY_TOKEN),
        TUPLE6 (STRING "decl_variable_dimension1", LBRACK, ELIST [], COLON,
          TK_DecNumber lo, RBRACK)),
   TLIST lst) -> f232 (); List.iter verible_pat lst
| TUPLE3 (STRING "instantiation_base1", unqualified_id1, TLIST lst) -> f233 (); List.iter verible_pat lst
| TUPLE3 (STRING "ml_start1", TLIST lst, End_of_file) -> f234 (); List.iter verible_pat lst
   | TUPLE3 (STRING "module_parameter_port_list_item_last1",
      TUPLE3 (STRING "module_parameter_port_list_trailing_comma1", TLIST lst, COMMA),
      TUPLE4 (STRING "module_parameter_port1", Parameter,
        param_type_followed_by_id_and_dimensions_opt4,
        TUPLE4 (STRING "trailing_assign1", EQUALS,
          hex_based_number1,
       EMPTY_TOKEN))) -> f241 (); List.iter verible_pat lst
   | TUPLE3 (STRING "module_parameter_port_list_item_last1",
      TUPLE3 (STRING "module_parameter_port_list_trailing_comma1", TLIST lst, COMMA),
   TUPLE4 (STRING "module_parameter_port2", Parameter, Type, type_assignment1)) -> f244 (); List.iter verible_pat lst
| TUPLE3 (STRING "net_variable1", SymbolIdentifier id, EMPTY_TOKEN) -> f245 ()
   | TUPLE3 (STRING "parameter_value_byname_list_item_last2",
      TUPLE3 (STRING "parameter_value_byname_list_trailing_comma1", TLIST lst, COMMA),
      TUPLE6 (STRING "parameter_value_byname1", DOT, SymbolIdentifier id, LPAREN,
    TK_DecNumber num, RPAREN)) -> f249 (); List.iter verible_pat lst
   | TUPLE3 (STRING "parameter_value_byname_list_item_last2",
      TUPLE3 (STRING "parameter_value_byname_list_trailing_comma1", TLIST lst, COMMA),
      TUPLE6 (STRING "parameter_value_byname1", DOT, SymbolIdentifier id, LPAREN,
     unqualified_id1, RPAREN)) -> f253 (); List.iter verible_pat lst
   | TUPLE3 (STRING "port1",
      TUPLE3 (STRING "port_reference1", unqualified_id1, EMPTY_TOKEN),
   EMPTY_TOKEN) -> f256 ()
   | TUPLE3 (STRING "procedural_timing_control_statement2",
   TUPLE5 (STRING "event_control2", AT, LPAREN, TLIST lst, RPAREN), seq_block1) -> f258 (); List.iter verible_pat lst
   | TUPLE3 (STRING "procedural_timing_control_statement2",
   TUPLE3 (STRING "event_control4", AT, STAR), seq_block1) -> f260 (); verible_pat seq_block1
   | TUPLE3 (STRING "reference2",
      TUPLE3 (STRING "reference2", unqualified_id1,
        TUPLE3 (STRING "hierarchy_extension1", DOT, unqualified_id2)),
    TUPLE3 (STRING "hierarchy_extension1", DOT, unqualified_id3)) -> f264 ()
   | TUPLE3 (STRING "reference3",
      TUPLE3 (STRING "reference2",
        TUPLE3 (STRING "reference2", unqualified_id1,
          TUPLE3 (STRING "hierarchy_extension1", DOT, unqualified_id2)),
        TUPLE3 (STRING "hierarchy_extension1", DOT, unqualified_id3)),
      TUPLE6 (STRING "select_variable_dimension1", LBRACK, TK_DecNumber hi, COLON,
     TK_DecNumber lo, RBRACK)) -> f271 ()
   | TUPLE3 (STRING "reference3", reference3,
      TUPLE6 (STRING "select_variable_dimension1", LBRACK, TK_DecNumber hi, COLON,
    TK_DecNumber lo, RBRACK)) -> f274 ()
   | TUPLE3 (STRING "reference3", unqualified_id1,
      TUPLE6 (STRING "select_variable_dimension3", LBRACK, unqualified_id2,
    PLUS_COLON, unqualified_id3, RBRACK)) -> f277 ()
   | TUPLE3 (STRING "reference3", unqualified_id1,
   TUPLE4 (STRING "select_variable_dimension2", LBRACK, TK_DecNumber lo, RBRACK)) -> f279 ()
   | TUPLE3 (STRING "reference3", unqualified_id1,
      TUPLE4 (STRING "select_variable_dimension2", LBRACK, unqualified_id2,
    RBRACK)) -> f282 ()
   | TUPLE3 (STRING "reference_or_call_base1", unqualified_id1,
  TUPLE4 (STRING "call_base1", LPAREN, TLIST lst, RPAREN)) -> f284 (); List.iter verible_pat lst
   | TUPLE3 (STRING "sequence_repetition_expr1", expression_or_dist1,
   EMPTY_TOKEN) -> f286 ()
| TUPLE3 (STRING "statement3", reference, SEMICOLON) -> f287 ()
   | TUPLE3 (STRING "statement_item6", assignment_statement_no_expr1,
   SEMICOLON) -> f289 ()
   | TUPLE3 (STRING "system_tf_call1", SystemTFIdentifier id,
  TUPLE4 (STRING "call_base1", LPAREN, TLIST lst, RPAREN)) -> f291 (); List.iter verible_pat lst
   | TUPLE3 (STRING "unary_prefix_expr2", VBAR,
   TUPLE4 (STRING "range_list_in_braces1", LBRACE, TLIST lst, RBRACE)) -> f293 (); List.iter verible_pat lst
| TUPLE3 (STRING "unary_prefix_expr2", VBAR, reference3) -> f294 ()
| TUPLE3 (STRING "unary_prefix_expr2", TILDE, TK_DecNumber num) -> f295 ()
| TUPLE3 (STRING "unary_prefix_expr2", TILDE, unqualified_id1) -> f296 ()
| TUPLE3 (STRING "unary_prefix_expr2", PLING, reference3) -> f297 ()
| TUPLE3 (STRING "unary_prefix_expr2", HYPHEN, unqualified_id1) -> f298 ()
| TUPLE3 (STRING "unary_prefix_expr2", AMPERSAND, reference3) -> f299 ()
| TUPLE3 (STRING "unqualified_id1", SymbolIdentifier id, EMPTY_TOKEN) -> f300 ()
| TUPLE3 (STRING "unqualified_id1", SymbolIdentifier id,
   TUPLE5 (STRING "parameters2", HASH, LPAREN, TLIST lst, RPAREN)) -> f302 (); List.iter verible_pat lst
| TUPLE12 (STRING "module_or_interface_declaration1", Module, EMPTY_TOKEN,
      SymbolIdentifier id, EMPTY_TOKEN, EMPTY_TOKEN,
      TUPLE4 (STRING "module_port_list_opt1", LPAREN, TLIST lst, RPAREN),
   EMPTY_TOKEN, SEMICOLON, EMPTY_TOKEN, Endmodule, EMPTY_TOKEN) -> f306 (); List.iter verible_pat lst
| TUPLE12 (STRING ("module_or_interface_declaration1" as s), Module, EMPTY_TOKEN,
      SymbolIdentifier id, EMPTY_TOKEN, EMPTY_TOKEN,
      TUPLE4 (STRING "module_port_list_opt1", LPAREN, TLIST lst, RPAREN),
   EMPTY_TOKEN, SEMICOLON, TLIST lst', Endmodule, EMPTY_TOKEN) -> f310 s id lst lst'; List.iter verible_pat lst; List.iter verible_pat lst'
| TUPLE12 (STRING "module_or_interface_declaration1", Module, EMPTY_TOKEN,
      SymbolIdentifier id, EMPTY_TOKEN,
      TUPLE5 (STRING "module_parameter_port_list_opt1", HASH, LPAREN, TLIST lst, RPAREN),
      TUPLE4 (STRING "module_port_list_opt1", LPAREN, TLIST lst', RPAREN),
   EMPTY_TOKEN, SEMICOLON, TLIST lst'', Endmodule, EMPTY_TOKEN) -> f315 (); List.iter verible_pat lst; List.iter verible_pat lst; List.iter verible_pat lst''
   | TUPLE10 (STRING "loop_statement1", For, LPAREN,
      TUPLE4 (STRING "for_init_decl_or_assign1", unqualified_id1, EQUALS, TK_DecNumber num),
      SEMICOLON, ELIST elst, SEMICOLON, assignment_statement_no_expr1,
   RPAREN, seq_block) -> f319 (); List.iter verible_pat elst; verible_pat seq_block
| TUPLE6 (STRING "cast1", data_type_primitive1, QUOTE, LPAREN, reference3, RPAREN) -> f320 ()
| TLIST lst -> f321 (); List.iter verible_pat lst
| ELIST [] -> f322 () 
| TK_StringLiteral "" -> f323 ()
| TK_DecNumber num -> f324 ()
| SymbolIdentifier id -> f325 ()
| STRING _ | COMMA | LESS -> f326 ()
   | TUPLE6
     (STRING "param_type_followed_by_id_and_dimensions_opt4", Logic,
      EMPTY_TOKEN,
      TUPLE3
       (STRING "decl_dimensions2",
        TUPLE6
         (STRING "decl_variable_dimension1", LBRACK, TK_DecNumber _, COLON,
          TK_DecNumber _, RBRACK),
        TUPLE6
         (STRING "decl_variable_dimension1", LBRACK, TK_DecNumber _, COLON,
          TK_DecNumber _, RBRACK)),
   SymbolIdentifier _, EMPTY_TOKEN) -> f338 ()
   | TUPLE6
     (STRING "select_variable_dimension1", LBRACK, ELIST _, COLON,
   TK_DecNumber _, RBRACK) -> f341 ()
   | TUPLE6
     (STRING "simple_immediate_assertion_statement1", Assert, LPAREN,
      reference3, RPAREN,
      TUPLE3
       (STRING "action_block3", Else,
        TUPLE3
      (STRING "statement3", reference_or_call_base1, SEMICOLON))) -> f348 ()
   | TUPLE5
     (STRING "data_declaration_or_module_instantiation2", Automatic,
   EMPTY_TOKEN, instantiation_base1, SEMICOLON) -> f351 ()
   | TUPLE5
     (STRING "net_declaration2", Wire,
      TUPLE3
       (STRING "data_type_or_implicit1",
        TUPLE6
         (STRING "decl_variable_dimension1", LBRACK, TK_DecNumber _, COLON,
          TK_DecNumber _, RBRACK),
        TUPLE3 (STRING "delay31", HASH, TK_RealTime _)),
      TUPLE4
       (STRING "net_decl_assign1", SymbolIdentifier _, EQUALS,
        TUPLE4 (STRING "range_list_in_braces1", LBRACE, TLIST _, RBRACE)),
   SEMICOLON) -> f363 ()
   | TUPLE5
     (STRING "net_declaration2", Wire,
      TUPLE3
       (STRING "data_type_or_implicit1",
        TUPLE6
         (STRING "decl_variable_dimension1", LBRACK, TK_DecNumber _, COLON,
          TK_DecNumber _, RBRACK),
        TUPLE3 (STRING "delay31", HASH, TK_RealTime _)),
      TUPLE4
       (STRING "net_decl_assign1", SymbolIdentifier _, EQUALS,
        unqualified_id1),
   SEMICOLON) -> f375 ()
   | TUPLE5
     (STRING "net_declaration4", Wire,
      TUPLE3 (STRING "delay31", HASH, TK_RealTime _),
      TUPLE4
       (STRING "net_decl_assign1", SymbolIdentifier _, EQUALS,
        cond_expr2),
   SEMICOLON) -> f382 ()
   | TUPLE5
     (STRING "port_declaration_noattr1", Output, Wire,
      TUPLE4
       (STRING "data_type_or_implicit_basic_followed_by_id_and_dimensions_opt4",
        TUPLE6
         (STRING "decl_variable_dimension1", LBRACK, TK_DecNumber _, COLON,
          TK_DecNumber _, RBRACK),
        unqualified_id1, EMPTY_TOKEN),
   EMPTY_TOKEN) -> f391 ()
   | TUPLE5
     (STRING "port_declaration_noattr1", Output, Wire,
      TUPLE3
       (STRING
         "type_identifier_or_implicit_basic_followed_by_id_and_dimensions_opt4",
        unqualified_id1, EMPTY_TOKEN),
   EMPTY_TOKEN) -> f398 ()
   | TUPLE5
     (STRING "port_declaration_noattr1", Output, EMPTY_TOKEN,
      TUPLE5
       (STRING
         "type_identifier_or_implicit_basic_followed_by_id_and_dimensions_opt2",
        unqualified_id1, EMPTY_TOKEN, unqualified_id2,
        EMPTY_TOKEN),
   EMPTY_TOKEN) -> f406 ()
   | TUPLE5
     (STRING "port_declaration_noattr1", Output, EMPTY_TOKEN,
      TUPLE4
       (STRING "data_type_or_implicit_basic_followed_by_id_and_dimensions_opt1",
        TUPLE3
         (STRING "data_type_primitive1",
          TUPLE3 (STRING "data_type_primitive_scalar1", Logic, EMPTY_TOKEN),
          TUPLE6
           (STRING "decl_variable_dimension1", LBRACK, ELIST _, COLON,
            TK_DecNumber _, RBRACK)),
        unqualified_id1, EMPTY_TOKEN),
   EMPTY_TOKEN) -> f418 ()
   | TUPLE5
     (STRING "port_declaration_noattr1", Input, Wire,
      TUPLE4
       (STRING "data_type_or_implicit_basic_followed_by_id_and_dimensions_opt4",
        TUPLE6
         (STRING "decl_variable_dimension1", LBRACK, TK_DecNumber _, COLON,
          TK_DecNumber _, RBRACK),
        unqualified_id1, EMPTY_TOKEN),
   EMPTY_TOKEN) -> f427 ()
   | TUPLE5
     (STRING "port_declaration_noattr1", Input, Wire,
      TUPLE3
       (STRING
         "type_identifier_or_implicit_basic_followed_by_id_and_dimensions_opt4",
        unqualified_id1, EMPTY_TOKEN),
   EMPTY_TOKEN) -> f434 ()
   | TUPLE5
     (STRING "port_declaration_noattr1", Input, EMPTY_TOKEN,
      TUPLE5
       (STRING
         "type_identifier_or_implicit_basic_followed_by_id_and_dimensions_opt2",
        unqualified_id1, EMPTY_TOKEN, unqualified_id2,
        EMPTY_TOKEN),
   EMPTY_TOKEN) -> f442 ()
   | TUPLE5
     (STRING "port_declaration_noattr1", Input, EMPTY_TOKEN,
      TUPLE4
       (STRING "data_type_or_implicit_basic_followed_by_id_and_dimensions_opt1",
        TUPLE3
         (STRING "data_type_primitive1",
          TUPLE3 (STRING "data_type_primitive_scalar1", Logic, EMPTY_TOKEN),
          EMPTY_TOKEN),
        unqualified_id1, EMPTY_TOKEN),
   EMPTY_TOKEN) -> f452 ()
   | TUPLE5 (STRING "port_declaration_noattr1", Input, EMPTY_TOKEN,
      TUPLE4 (STRING "data_type_or_implicit_basic_followed_by_id_and_dimensions_opt1",
        TUPLE3 (STRING "data_type_primitive1",
          TUPLE3 (STRING "data_type_primitive_scalar1", Logic, EMPTY_TOKEN),
          TUPLE6 (STRING "decl_variable_dimension1", LBRACK, TK_DecNumber _, COLON,
            TK_DecNumber _, RBRACK)),
        unqualified_id1, EMPTY_TOKEN),
   EMPTY_TOKEN) -> f460 ()
   | TUPLE5
     (STRING "port_declaration_noattr1", Input, EMPTY_TOKEN,
      TUPLE4
       (STRING "data_type_or_implicit_basic_followed_by_id_and_dimensions_opt1",
        TUPLE3
         (STRING "data_type_primitive1",
          TUPLE3 (STRING "data_type_primitive_scalar1", Logic, EMPTY_TOKEN),
          TUPLE6
           (STRING "decl_variable_dimension1", LBRACK, ELIST _, COLON,
            TK_DecNumber _, RBRACK)),
        unqualified_id1, EMPTY_TOKEN),
   EMPTY_TOKEN) -> f472 ()
   | TUPLE5
     (STRING "port_declaration_noattr1", Inout, Wire,
      TUPLE3
       (STRING
         "type_identifier_or_implicit_basic_followed_by_id_and_dimensions_opt4",
        unqualified_id1, EMPTY_TOKEN),
   EMPTY_TOKEN) -> f479 ()
   | TUPLE5
     (STRING "port_declaration_noattr1", Inout, EMPTY_TOKEN,
      TUPLE3
       (STRING
         "type_identifier_or_implicit_basic_followed_by_id_and_dimensions_opt4",
        unqualified_id1, EMPTY_TOKEN),
   EMPTY_TOKEN) -> f486 ()
   | TUPLE4
     (STRING "expression_list_proper1",
      TUPLE3 (STRING "hex_based_number1", TK_HexBase _, TK_HexDigits _),
      COMMA,
   TUPLE3 (STRING "hex_based_number1", TK_HexBase _, TK_HexDigits _)) -> f491 ()
   | TUPLE4
     (STRING "expression_list_proper1", expression_list_proper1, COMMA,
   TUPLE3 (STRING "hex_based_number1", TK_HexBase _, TK_HexDigits _)) -> f494 ()
   | TUPLE4
     (STRING "generate_block1", begin1, EMPTY_TOKEN,
   TUPLE3 (STRING "end1", End, EMPTY_TOKEN)) -> f497 ()
   | TUPLE4
  (STRING "jump_statement4", Return, unqualified_id1, SEMICOLON) -> f499 ()
   | TUPLE4
     (STRING "module_parameter_port1", Parameter,
      TUPLE6
       (STRING "param_type_followed_by_id_and_dimensions_opt5", Int,
        EMPTY_TOKEN, EMPTY_TOKEN, SymbolIdentifier _, EMPTY_TOKEN),
      TUPLE4
       (STRING "trailing_assign1", EQUALS, unary_prefix_expr2,
     EMPTY_TOKEN)) -> f507 ()
   | TUPLE4
     (STRING "module_parameter_port1", Parameter,
      param_type_followed_by_id_and_dimensions_opt4,
      TUPLE4
       (STRING "trailing_assign1", EQUALS, bin_based_number1,
     EMPTY_TOKEN)) -> f513 ()
   | TUPLE4
     (STRING "module_parameter_port2", Parameter, Type,
      TUPLE4
       (STRING "type_assignment1", SymbolIdentifier _, EQUALS,
        TUPLE3
         (STRING "data_type_primitive1",
          TUPLE3 (STRING "data_type_primitive_scalar1", Logic, EMPTY_TOKEN),
       EMPTY_TOKEN))) -> f521 ()
   | TUPLE4
     (STRING "port_declaration_noattr4",
      TUPLE6
       (STRING "type_identifier_followed_by_id3", unqualified_id1, DOT,
        SymbolIdentifier _, EMPTY_TOKEN, SymbolIdentifier _),
   EMPTY_TOKEN, EMPTY_TOKEN) -> f527 ()
   | TUPLE4
     (STRING "qualified_id2", unqualified_id1, COLON_COLON,
   unqualified_id2) -> f530 ()
   | TUPLE4
     (STRING "tf_port_item1", EMPTY_TOKEN,
      TUPLE4
       (STRING "data_type_or_implicit_basic_followed_by_id_and_dimensions_opt1",
        TUPLE3
         (STRING "data_type_primitive1",
          TUPLE3 (STRING "data_type_primitive_scalar1", Logic, EMPTY_TOKEN),
          TUPLE6
           (STRING "decl_variable_dimension1", LBRACK, TK_DecNumber _, COLON,
            TK_DecNumber _, RBRACK)),
        unqualified_id1, EMPTY_TOKEN),
   EMPTY_TOKEN) -> f542 ()
   | TUPLE4
     (STRING "tf_port_item1", EMPTY_TOKEN,
      TUPLE4
       (STRING "data_type_or_implicit_basic_followed_by_id_and_dimensions_opt1",
        TUPLE3
         (STRING "data_type_primitive1",
          TUPLE3 (STRING "data_type_primitive_scalar5", Byte, EMPTY_TOKEN),
          EMPTY_TOKEN),
        unqualified_id1, EMPTY_TOKEN),
   EMPTY_TOKEN) -> f552 ()
| TUPLE3 (STRING "always_construct1", Always_latch, seq_block1) -> f553 ()
   | TUPLE3
     (STRING "always_construct1", Always_ff,
   procedural_timing_control_statement2) -> f556 ()
| TUPLE3 (STRING "always_construct1", Always_comb, seq_block1) -> f557 ()
   | TUPLE3
     (STRING "any_port_list_item_last1",
      TUPLE3 (STRING "any_port_list_trailing_comma1", TLIST _, COMMA),
   TUPLE5 (STRING "port_named2", DOT, SymbolIdentifier _, LPAREN, RPAREN)) -> f561 ()
   | TUPLE3
     (STRING "any_port_list_item_last1",
      TUPLE3 (STRING "any_port_list_trailing_comma1", TLIST _, COMMA),
   TUPLE3 (STRING "port_named3", DOT, SymbolIdentifier _)) -> f565 ()
   | TUPLE3
     (STRING "begin1", Begin,
   TUPLE3 (STRING "label_opt1", COLON, SymbolIdentifier _)) -> f568 ()
   | TUPLE3
     (STRING "conditional_generate_construct2",
      TUPLE3 (STRING "generate_if1", If, expression_in_parens1),
   generate_block1) -> f572 ()
   | TUPLE3
     (STRING "data_type_primitive1",
      TUPLE3 (STRING "data_type_primitive_scalar1", Logic, EMPTY_TOKEN),
   EMPTY_TOKEN) -> f576 ()
   | TUPLE3
     (STRING "data_type_primitive1",
      TUPLE3 (STRING "data_type_primitive_scalar1", Logic, EMPTY_TOKEN),
      TUPLE6
       (STRING "decl_variable_dimension1", LBRACK, TK_DecNumber _, COLON,
     TK_DecNumber _, RBRACK)) -> f582 ()
   | TUPLE3
     (STRING "data_type_primitive1",
      TUPLE3 (STRING "data_type_primitive_scalar1", Logic, EMPTY_TOKEN),
      TUPLE6
       (STRING "decl_variable_dimension1", LBRACK, unqualified_id1,
     COLON, TK_DecNumber _, RBRACK)) -> f588 ()
   | TUPLE3
     (STRING "data_type_primitive1",
      TUPLE3 (STRING "data_type_primitive_scalar1", Bit, EMPTY_TOKEN),
   EMPTY_TOKEN) -> f592 ()
   | TUPLE3
     (STRING "data_type_primitive1",
      TUPLE3 (STRING "data_type_primitive_scalar5", Int, EMPTY_TOKEN),
   EMPTY_TOKEN) -> f596 ()
   | TUPLE3
     (STRING "data_type_primitive1",
      TUPLE3 (STRING "data_type_primitive_scalar5", Byte, EMPTY_TOKEN),
   EMPTY_TOKEN) -> f600 ()
| TUPLE3 (STRING "dec_based_number1", TK_DecBase _, TK_DecDigits _) -> f601 ()
   | TUPLE3
     (STRING "decl_dimensions2",
      TUPLE6
       (STRING "decl_variable_dimension1", LBRACK, TK_DecNumber _, COLON,
        TK_DecNumber _, RBRACK),
      TUPLE6
       (STRING "decl_variable_dimension1", LBRACK, TK_DecNumber _, COLON,
     TK_DecNumber _, RBRACK)) -> f609 ()
| TUPLE3 (STRING "hex_based_number1", TK_HexBase _, TK_HexDigits _) -> f610 ()
   | TUPLE3
  (STRING "inc_or_dec_expression2", unqualified_id1, PLUS_PLUS) -> f612 ()
| TUPLE3 (STRING "label_opt1", COLON, SymbolIdentifier _) -> f613 ()
| TUPLE3 (STRING "port_named3", DOT, SymbolIdentifier _) -> f614 ()
   | TUPLE3
     (STRING "reference2", unqualified_id1,
   TUPLE3 (STRING "hierarchy_extension1", DOT, unqualified_id2)) -> f617 ()
   | TUPLE3
     (STRING "reference3", unqualified_id1,
      TUPLE6
       (STRING "select_variable_dimension1", LBRACK, ELIST _, COLON,
     TK_DecNumber _, RBRACK)) -> f622 ()
   | TUPLE12
     (STRING "module_or_interface_declaration1", Module, EMPTY_TOKEN,
      SymbolIdentifier _, EMPTY_TOKEN,
      TUPLE5
       (STRING "module_parameter_port_list_opt1", HASH, LPAREN, TLIST _,
        RPAREN),
      TUPLE4 (STRING "module_port_list_opt1", LPAREN, TLIST _, RPAREN),
      EMPTY_TOKEN, SEMICOLON, TLIST _, Endmodule,
   TUPLE3 (STRING "label_opt1", COLON, SymbolIdentifier _)) -> f631 ()
   | TUPLE12
     (STRING "module_or_interface_declaration1", Interface, EMPTY_TOKEN,
      SymbolIdentifier _, EMPTY_TOKEN,
      TUPLE5
       (STRING "module_parameter_port_list_opt1", HASH, LPAREN, TLIST _,
        RPAREN),
      TUPLE4 (STRING "module_port_list_opt1", LPAREN, TLIST _, RPAREN),
   EMPTY_TOKEN, SEMICOLON, EMPTY_TOKEN, Endinterface, EMPTY_TOKEN) -> f639 ()
   | TUPLE11
     (STRING "function_declaration1", Function, EMPTY_TOKEN,
      TUPLE4
       (STRING "data_type_or_implicit_basic_followed_by_id_and_dimensions_opt5",
        Void, unqualified_id1, EMPTY_TOKEN),
      LPAREN, TLIST _, RPAREN, SEMICOLON, TLIST _, Endfunction,
   TUPLE3 (STRING "label_opt1", COLON, SymbolIdentifier _)) -> f646 ()
   | TUPLE11
     (STRING "function_declaration1", Function, Automatic,
      TUPLE4
       (STRING "data_type_or_implicit_basic_followed_by_id_and_dimensions_opt1",
        TUPLE3
         (STRING "data_type_primitive1",
          TUPLE3 (STRING "data_type_primitive_scalar1", Logic, EMPTY_TOKEN),
          TUPLE6
           (STRING "decl_variable_dimension1", LBRACK, TK_DecNumber _, COLON,
            TK_DecNumber _, RBRACK)),
        unqualified_id1, EMPTY_TOKEN),
      LPAREN, TLIST _, RPAREN, SEMICOLON, TLIST _, Endfunction,
   TUPLE3 (STRING "label_opt1", COLON, SymbolIdentifier _)) -> f659 ()
   | TUPLE10 (STRING "loop_statement1", For, LPAREN,
      TUPLE5 (STRING "for_init_decl_or_assign2",
        TUPLE3 (STRING "data_type_primitive1",
          TUPLE3 (STRING "data_type_primitive_scalar5", Int, Unsigned),
          EMPTY_TOKEN),
        SymbolIdentifier _, EQUALS, TK_DecNumber _),
      SEMICOLON, ELIST _, SEMICOLON, inc_or_dec_expression2, RPAREN,
   STRING "seq_block1") -> f667 () 
| TUPLE4 (STRING "assignment_pattern1", QUOTE_LBRACE, TLIST lst, RBRACE) -> f668 (); List.iter verible_pat lst
   | TUPLE9
     (STRING "case_statement3", Unique, Case, LPAREN, STRING "unqualified_id1",
      RPAREN, Inside,
      TUPLE3
       (STRING "case_inside_items1",
        TUPLE4
         (STRING "case_inside_item1", TLIST _, COLON,
          TUPLE4
           (STRING "jump_statement4", Return, STRING "bin_based_number1",
            SEMICOLON)),
        TUPLE4
         (STRING "case_inside_item2", Default, COLON,
          TUPLE4
           (STRING "jump_statement4", Return, STRING "bin_based_number1",
            SEMICOLON))),
   Endcase) -> f684 ()
   | TUPLE9
     (STRING "case_statement3", EMPTY_TOKEN, Case, LPAREN,
      STRING "unqualified_id1", RPAREN, Inside,
      TUPLE3
       (STRING "case_inside_items1",
        TUPLE4
         (STRING "case_inside_item1", TLIST _, COLON, STRING "seq_block1"),
        TUPLE4
         (STRING "case_inside_item2", Default, COLON,
          TUPLE4
           (STRING "jump_statement4", Return, STRING "bin_based_number1",
            SEMICOLON))),
   Endcase) -> f697 ()
   | TUPLE8
     (STRING "case_statement1", Unique, Case, LPAREN,
      TUPLE3
       (STRING "reference2", STRING "unqualified_id1",
        TUPLE3 (STRING "hierarchy_extension1", DOT, STRING "unqualified_id1")),
   RPAREN, STRING "case_items1", Endcase) -> f703 ()
   | TUPLE8
     (STRING "case_statement1", Unique, Case, LPAREN, STRING "unqualified_id1",
   RPAREN, STRING "case_items1", Endcase) -> f706 ()
| oth -> othpatlst := oth :: !othpatlst; print_endline "verible_pat"
   
   and verible_pat itm = patlst := itm :: !patlst; verible_pat' itm; patlst := List.tl !patlst
