     open Source_text_verible

     let othpat = ref End_of_file
     let patlst = ref []
     let othpatlst = ref []

let cnt = Array.init 800 (fun _ -> ref 0)
(* let loglst = ref [] *)
let log lbl = lbl (* loglst := lbl :: !loglst *)

let mark sel = function
| `always_construct1 block as lbl -> log lbl
| `any_argument_list_item_last2 lst as lbl -> log lbl
| `any_param_declaration4 tok as lbl -> log lbl
| `any_port_list_item_last1 lst as lbl -> log lbl
| `assignment_pattern1 lst as lbl -> log lbl
| `assignment_statement_no_expr1 lst as lbl -> log lbl
| `begin1 as lbl -> log lbl
| `bin_based_number1 as lbl -> log lbl
| `block_item_or_statement_or_null6 as lbl -> log lbl
| `call_base1 lst as lbl -> log lbl
| `case_item1 block as lbl -> log lbl
| `case_item2 block as lbl -> log lbl
| `case_items1 as lbl -> log lbl
| `case_statement1 as lbl -> log lbl
| `case_statement3 as lbl -> log lbl
| `cast1 as lbl -> log lbl
| `comma as lbl -> log lbl
| `cond_expr2 as lbl -> log lbl
| `conditional_generate_construct1 as lbl -> log lbl
| `conditional_generate_construct2 as lbl -> log lbl
| `conditional_statement1 tok as lbl -> log lbl
| `conditional_statement2 as lbl -> log lbl
| `cont_assign1 as lbl -> log lbl
| `continuous_assign1 lst as lbl -> log lbl
| `data_declaration_or_module_instantiation1 as lbl -> log lbl
| `data_declaration_or_module_instantiation2 as lbl -> log lbl
| `data_type_primitive1 as lbl -> log lbl
| `dec_based_number1 as lbl -> log lbl
| `dec_num as lbl -> log lbl
| `decl_dimensions2 as lbl -> log lbl
| `decl_variable_dimension1 as lbl -> log lbl
| `elist as lbl -> log lbl
| `end1 as lbl -> log lbl
| `event_control2 lst as lbl -> log lbl
| `event_control4 as lbl -> log lbl
| `event_expression1 as lbl -> log lbl
| `expr_primary_braces2 as lbl -> log lbl
| `expr_primary_parens1 lst as lbl -> log lbl
| `expression_in_parens1 lst as lbl -> log lbl
| `expression_list_proper1 as lbl -> log lbl
| `expression_or_dist1 as lbl -> log lbl
| `function_declaration1 as lbl -> log lbl
| `gate_instance_or_register_variable1 as lbl -> log lbl
| `generate_block1 (block, lst) as lbl -> log lbl
| `generate_if1 as lbl -> log lbl
| `generate_region1 lst as lbl -> log lbl
| `hex_based_number1 as lbl -> log lbl
| `ident as lbl -> log lbl
| `inc_or_dec_expression2 as lbl -> log lbl
| `initial_construct1 block as lbl -> log lbl
| `instantiation_base1 lst as lbl -> log lbl
| `jump_statement4 as lbl -> log lbl
| `label_opt1 as lbl -> log lbl
| `less as lbl -> log lbl
| `loop_statement1 (elst, blk) as lbl -> log lbl
| `ml_start1 lst as lbl -> log lbl
| `module_or_interface_declaration1 (id, lst, lst') as lbl -> log lbl
| `module_parameter_port1 as lbl -> log lbl
| `module_parameter_port2 as lbl -> log lbl
| `module_parameter_port_list_item_last1 lst as lbl -> log lbl
| `module_port_list_opt1 (lst, lst', lst'') as lbl -> log lbl
| `net_declaration1 as lbl -> log lbl
| `net_declaration2 as lbl -> log lbl
| `net_declaration4 as lbl -> log lbl
| `net_variable1 as lbl -> log lbl
| `non_anonymous_gate_instance_or_register_variable1 as lbl -> log lbl
| `non_anonymous_gate_instance_or_register_variable2 lst as lbl -> log lbl
| `nonblocking_assignment1 as lbl -> log lbl
| `param_type_followed_by_id_and_dimensions_opt4 as lbl -> log lbl
| `parameter_value_byname1 as lbl -> log lbl
| `parameter_value_byname_list_item_last2 lst as lbl -> log lbl
| `parameters2 lst as lbl -> log lbl
| `port1 as lbl -> log lbl
| `port_declaration_noattr1 as lbl -> log lbl
| `port_declaration_noattr4 as lbl -> log lbl
| `port_named1 as lbl -> log lbl
| `port_named3 as lbl -> log lbl
| `procedural_timing_control_statement2 lst as lbl -> log lbl
| `qualified_id2 as lbl -> log lbl
| `range_list_in_braces1 lst as lbl -> log lbl
| `reference2 as lbl -> log lbl
| `reference3 as lbl -> log lbl
| `reference_or_call_base1 lst as lbl -> log lbl
| `select_variable_dimension1 as lbl -> log lbl
| `select_variable_dimension2 as lbl -> log lbl
| `select_variable_dimension3 as lbl -> log lbl
| `seq_block1 lst as lbl -> log lbl
| `sequence_repetition_expr1 as lbl -> log lbl
| `simple_immediate_assertion_statement1 as lbl -> log lbl
| `statement3 as lbl -> log lbl
| `statement_item6 stmt as lbl -> log lbl
| `string as lbl -> log lbl
| `string_lit as lbl -> log lbl
| `system_tf_call1 lst as lbl -> log lbl
| `task_declaration1 lst as lbl -> log lbl
| `tf_port_item1 as lbl -> log lbl
| `unary_prefix_expr2 lst as lbl -> log lbl
| `unqualified_id1 (id,parm) as lbl -> log lbl
| `tlist lst as lbl -> log lbl

let rec verible_pat' = function
| TLIST lst -> `tlist (List.map verible_pat lst)
| TUPLE9 (STRING "task_declaration1", Task, EMPTY_TOKEN, SymbolIdentifier id,
   EMPTY_TOKEN, SEMICOLON, TLIST lst, Endtask, EMPTY_TOKEN) -> f9 (`task_declaration1 lst)
| TUPLE8 (STRING "case_statement1", EMPTY_TOKEN, Case, LPAREN,
       expr,
   RPAREN, case_items1, Endcase) -> f12 `case_statement1
| TUPLE7 (STRING "conditional_statement2", EMPTY_TOKEN, If,
       expression_in_parens1,
       then_statement,
   Else, else_statement) -> f16 `conditional_statement2
| TUPLE7 (STRING "expr_primary_braces2", LBRACE, braced, LBRACE,
  reference3, RBRACE, RBRACE) -> f18 `expr_primary_braces2
| TUPLE6 (STRING "cond_expr2", expr_primary, QUERY, expr_true, COLON, expr_false) -> f19 `cond_expr2
| TUPLE6 (STRING "continuous_assign1", Assign, EMPTY_TOKEN, dly, TLIST lst,
  SEMICOLON) -> f21 (`continuous_assign1 lst)
| TUPLE6 (STRING "decl_variable_dimension1", LBRACK, TK_DecNumber hi, COLON, TK_DecNumber lo, RBRACK) -> f22 `decl_variable_dimension1
    | TUPLE6 (STRING "non_anonymous_gate_instance_or_register_variable2",
  SymbolIdentifier id, EMPTY_TOKEN, LPAREN, TLIST lst, RPAREN) -> f24 (`non_anonymous_gate_instance_or_register_variable2 lst)
| TUPLE6 (STRING "nonblocking_assignment1", lhs, LT_EQ, EMPTY_TOKEN, rhs, SEMICOLON) -> f25 `nonblocking_assignment1
    | TUPLE6 (STRING "param_type_followed_by_id_and_dimensions_opt4", (Logic|EMPTY_TOKEN),
       EMPTY_TOKEN, TUPLE6 (STRING "decl_variable_dimension1", LBRACK, TK_DecNumber hi, COLON,
         TK_DecNumber lo, RBRACK),
   SymbolIdentifier id, EMPTY_TOKEN) -> f29 `param_type_followed_by_id_and_dimensions_opt4
    | TUPLE6 (STRING "parameter_value_byname1", DOT, SymbolIdentifier id, LPAREN,
  TK_DecNumber num, RPAREN) -> f31 `parameter_value_byname1
    | TUPLE6 (STRING "parameter_value_byname1", DOT, SymbolIdentifier id, LPAREN,
   unqualified_id1, RPAREN) -> f33 `parameter_value_byname1
    | TUPLE6 (STRING "port_named1", DOT, SymbolIdentifier id, LPAREN,
  unqualified_id1, RPAREN) -> f35 `port_named1
    | TUPLE6 (STRING "select_variable_dimension1", LBRACK, TK_DecNumber hi, COLON,
   TK_DecNumber lo, RBRACK) -> f37 `select_variable_dimension1
| TUPLE6 (STRING "select_variable_dimension3", LBRACK, unqualified_id1,
  PLUS_COLON, unqualified_id2, RBRACK) -> f39 `select_variable_dimension3
| TUPLE5 (STRING "any_param_declaration4", Localparam,
       TUPLE7
       (STRING "param_type_followed_by_id_and_dimensions_opt2", EMPTY_TOKEN,
         EMPTY_TOKEN, unqualified_id1, EMPTY_TOKEN, SymbolIdentifier id,
         EMPTY_TOKEN),
       TUPLE4 (STRING "trailing_assign1", EQUALS, TK_DecNumber num, EMPTY_TOKEN),
   SEMICOLON) -> f46 (`any_param_declaration4 unqualified_id1)
| TUPLE5 (STRING "any_param_declaration4", Localparam,
       TUPLE7
       (STRING "param_type_followed_by_id_and_dimensions_opt2", EMPTY_TOKEN,
         EMPTY_TOKEN, unqualified_id1, EMPTY_TOKEN, SymbolIdentifier id,
         EMPTY_TOKEN),
       TUPLE4
       (STRING "trailing_assign1", EQUALS, cond_expr2, EMPTY_TOKEN),
    SEMICOLON) -> f54 (`any_param_declaration4 unqualified_id1)
| TUPLE5 (STRING "any_param_declaration4", Localparam,
       TUPLE5
       (STRING "param_type_followed_by_id_and_dimensions_opt3", EMPTY_TOKEN,
         EMPTY_TOKEN, unqualified_id1, EMPTY_TOKEN),
       TUPLE4 (STRING "trailing_assign1", EQUALS, rhs, EMPTY_TOKEN),
    SEMICOLON) -> f60 (`any_param_declaration4 unqualified_id1)
| TUPLE5 (STRING "any_param_declaration4", Localparam,
       param_type_followed_by_id_and_dimensions_opt4,
       TUPLE4 (STRING "trailing_assign1", EQUALS, range_list_in_braces1, EMPTY_TOKEN),
    SEMICOLON) -> f64 (`any_param_declaration4 range_list_in_braces1)
| TUPLE5 (STRING "conditional_generate_construct1",
       TUPLE3 (STRING "generate_if1", If, expression_in_parens1),
   generate_block1, Else, conditional_generate_construct1) -> f67 `conditional_generate_construct1
| TUPLE5 (STRING "conditional_statement1", EMPTY_TOKEN, If,
   expression_in_parens1, seq_block1) -> f69 (`conditional_statement1 seq_block1)
| TUPLE5 (STRING "event_control2", AT, LPAREN, TLIST lst, RPAREN) -> f70 (`event_control2 lst)
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
   SEMICOLON) -> f81 `net_declaration2
| TUPLE5 (STRING "net_declaration2", Wire,
       TUPLE3
       (STRING "data_type_or_implicit1",
         TUPLE6
         (STRING "decl_variable_dimension1", LBRACK, TK_DecNumber hi, COLON,
           TK_DecNumber lo, RBRACK),
         EMPTY_TOKEN),
    net_variable1, SEMICOLON) -> f89 `net_declaration2
| TUPLE5 (STRING "port_declaration_noattr1", Output, EMPTY_TOKEN,
       TUPLE4 (STRING "data_type_or_implicit_basic_followed_by_id_and_dimensions_opt1",
         TUPLE3 (STRING "data_type_primitive1",
           TUPLE3 (STRING "data_type_primitive_scalar1", (Reg|Logic), EMPTY_TOKEN),
           EMPTY_TOKEN),
         unqualified_id1, EMPTY_TOKEN),
   EMPTY_TOKEN) -> f96 `port_declaration_noattr1
| TUPLE5 (STRING "port_declaration_noattr1", Output, EMPTY_TOKEN,
       TUPLE4 (STRING "data_type_or_implicit_basic_followed_by_id_and_dimensions_opt1",
         TUPLE3 (STRING "data_type_primitive1",
          TUPLE3 (STRING "data_type_primitive_scalar1", (Reg|Logic), EMPTY_TOKEN),
          TUPLE6 (STRING "decl_variable_dimension1", LBRACK, TK_DecNumber hi, COLON,
            TK_DecNumber lo, RBRACK)),
        unqualified_id1, EMPTY_TOKEN),
    EMPTY_TOKEN) -> f104 `port_declaration_noattr1
| TUPLE5 (STRING "port_declaration_noattr1", Output, EMPTY_TOKEN,
      TUPLE4 (STRING "data_type_or_implicit_basic_followed_by_id_and_dimensions_opt4",
        TUPLE6 (STRING "decl_variable_dimension1", LBRACK, TK_DecNumber hi, COLON,
          TK_DecNumber lo, RBRACK),
        unqualified_id1, EMPTY_TOKEN),
   EMPTY_TOKEN) -> f110 `port_declaration_noattr1
| TUPLE5 (STRING "port_declaration_noattr1", Output, EMPTY_TOKEN,
      TUPLE3 (STRING "type_identifier_or_implicit_basic_followed_by_id_and_dimensions_opt4",
        unqualified_id1, EMPTY_TOKEN),
    EMPTY_TOKEN) -> f114 `port_declaration_noattr1
| TUPLE5 (STRING "port_declaration_noattr1", Input, EMPTY_TOKEN,
      TUPLE4 (STRING "data_type_or_implicit_basic_followed_by_id_and_dimensions_opt4",
        TUPLE6 (STRING "decl_variable_dimension1", LBRACK, TK_DecNumber hi, COLON,
          TK_DecNumber lo, RBRACK),
        unqualified_id1, EMPTY_TOKEN),
   EMPTY_TOKEN) -> f120 `port_declaration_noattr1
| TUPLE5 (STRING "port_declaration_noattr1", Input, EMPTY_TOKEN,
      TUPLE3 (STRING "type_identifier_or_implicit_basic_followed_by_id_and_dimensions_opt4",
        unqualified_id1, EMPTY_TOKEN),
    EMPTY_TOKEN) -> f124 `port_declaration_noattr1
| TUPLE4 (STRING "assignment_statement_no_expr1", range_list_in_braces1, EQUALS,
   expr') -> f127 (`assignment_statement_no_expr1 range_list_in_braces1)
| TUPLE4 (STRING "assignment_statement_no_expr1", reference, EQUALS, rhs) -> f128 (`assignment_statement_no_expr1 reference)
| TUPLE4 (STRING "call_base1", LPAREN, TLIST lst, RPAREN) -> f129 (`call_base1 lst)
| TUPLE4 (STRING "case_item1", sel, COLON, seq_block) -> f130 (`case_item1 seq_block)
| TUPLE4 (STRING "case_item2", Default, COLON, SEMICOLON) -> f131 (`case_item2 SEMICOLON)
| TUPLE4 (STRING "case_item2", Default, COLON, seq_block) -> f132 (`case_item2 seq_block)
| TUPLE4 (STRING "cont_assign1", unqualified_id1, EQUALS, expr) -> f133 `cont_assign1
| TUPLE4 (STRING "expr_primary_parens1", LPAREN, TLIST lst, RPAREN) -> f134 (`expr_primary_parens1 lst)
| TUPLE4 (STRING "expression_in_parens1", LPAREN, cond_expr2, RPAREN) -> f139 (`expression_in_parens1 cond_expr2)
| TUPLE3 (STRING "system_tf_call1", SystemTFIdentifier id,
        TUPLE4 (STRING "call_base1", LPAREN, TLIST lst, RPAREN)) -> f138 (`system_tf_call1 lst)
| TUPLE4 (STRING "expression_list_proper1", unqualified_id1, COMMA, expr') -> f140 `expression_list_proper1
   | TUPLE4 (STRING "gate_instance_or_register_variable1", SymbolIdentifier id,
   EMPTY_TOKEN, EMPTY_TOKEN) -> f142 `gate_instance_or_register_variable1
| TUPLE4 (STRING "generate_block1", begin1,
  TLIST lst, TUPLE3 (STRING "end1", End, EMPTY_TOKEN)) -> f144 (`generate_block1 (begin1, lst))
| TUPLE4 (STRING "generate_region1", Generate, TLIST lst, Endgenerate) -> f145 (`generate_region1 lst)
| TUPLE4 (STRING "module_parameter_port1", Parameter,
      TUPLE5 (STRING "param_type_followed_by_id_and_dimensions_opt3", EMPTY_TOKEN,
        EMPTY_TOKEN, unqualified_id1, EMPTY_TOKEN),
    TUPLE4 (STRING "trailing_assign1", EQUALS, TK_DecNumber num, EMPTY_TOKEN)) -> f149 `module_parameter_port1
| TUPLE4 (STRING "module_parameter_port1", Parameter,
      param_type_followed_by_id_and_dimensions_opt4,
   TUPLE4 (STRING "trailing_assign1", EQUALS, TK_DecNumber num, EMPTY_TOKEN)) -> f152 `module_parameter_port1
| TUPLE4 (STRING "net_declaration1", Wire,
      TUPLE4 (STRING "net_decl_assign1", SymbolIdentifier id, EQUALS,
        unary_prefix_expr2),
    SEMICOLON) -> f156 `net_declaration1
| TUPLE4 (STRING "net_declaration1", Wire, net_variable1, SEMICOLON) -> f157 `net_declaration1
   | TUPLE4 (STRING "non_anonymous_gate_instance_or_register_variable1",
  SymbolIdentifier id, EMPTY_TOKEN, EMPTY_TOKEN) -> f159 `non_anonymous_gate_instance_or_register_variable1
   | TUPLE4 (STRING "non_anonymous_gate_instance_or_register_variable1",
      SymbolIdentifier id, EMPTY_TOKEN,
      TUPLE3 (STRING "trailing_decl_assignment2", EQUALS,
     TUPLE3 (STRING "dec_based_number1", TK_DecBase "", TK_DecDigits ""))) -> f163 `non_anonymous_gate_instance_or_register_variable1
   | TUPLE4 (STRING "non_anonymous_gate_instance_or_register_variable1",
      SymbolIdentifier id, EMPTY_TOKEN,
   TUPLE3 (STRING "trailing_decl_assignment2", EQUALS, TK_DecNumber num)) -> f166 `non_anonymous_gate_instance_or_register_variable1
   | TUPLE4 (STRING "non_anonymous_gate_instance_or_register_variable1",
      SymbolIdentifier id, EMPTY_TOKEN,
   TUPLE3 (STRING "trailing_decl_assignment2", EQUALS, unary_prefix_expr2)) -> f169 `non_anonymous_gate_instance_or_register_variable1
   | TUPLE4 (STRING "non_anonymous_gate_instance_or_register_variable1",
      SymbolIdentifier id,
      TUPLE6 (STRING "decl_variable_dimension1", LBRACK, TK_DecNumber hi, COLON,
        expr', RBRACK),
    EMPTY_TOKEN) -> f174 `non_anonymous_gate_instance_or_register_variable1
| TUPLE4 (STRING "range_list_in_braces1", LBRACE, TLIST lst, RBRACE) -> f175 (`range_list_in_braces1 lst)
| TUPLE4(STRING "select_variable_dimension2", LBRACK, TK_DecNumber lo, RBRACK) -> f176 `select_variable_dimension2
| TUPLE4 (STRING "select_variable_dimension2", LBRACK, unqualified_id1, RBRACK) -> f177 `select_variable_dimension2
| TUPLE4 (STRING "seq_block1", begin1, EMPTY_TOKEN, TUPLE3 (end1, End, EMPTY_TOKEN)) -> f178 (`seq_block1 [])
| TUPLE4 (STRING "seq_block1", begin1, TLIST lst, TUPLE3 (end1, End, EMPTY_TOKEN)) -> f179 (`seq_block1 lst)
| TUPLE3 (STRING "always_construct1", Always, procedural_timing_control_statement2) -> f180 (`always_construct1 procedural_timing_control_statement2)
| TUPLE3 (STRING "any_argument_list_item_last2",
      TUPLE3 (STRING "any_argument_list_trailing_comma1", TLIST lst, COMMA),
      TUPLE3 (STRING "reference2", unqualified_id1,
    TUPLE3 (STRING "hierarchy_extension1", DOT, unqualified_id2))) -> f184 (`any_argument_list_item_last2 lst)
| TUPLE3 (STRING "any_argument_list_item_last2",
      TUPLE3 (STRING "any_argument_list_trailing_comma1", TLIST lst, COMMA),
   cond_expr2) -> f187 (`any_argument_list_item_last2 lst)
| TUPLE3 (STRING "any_port_list_item_last1",
      TUPLE3 (STRING "any_port_list_trailing_comma1", TLIST lst, COMMA),
      TUPLE6 (STRING "port_named1", DOT, SymbolIdentifier id, LPAREN,
     unqualified_id1, RPAREN)) -> f191 (`any_port_list_item_last1 lst)
| TUPLE3 (STRING "begin1", Begin, EMPTY_TOKEN) -> f192 `begin1
| TUPLE3 (STRING "bin_based_number1", TK_BinBase base, TK_BinDigits digits) -> f193 `bin_based_number1
| TUPLE3 (STRING "block_item_or_statement_or_null6", unqualified_id1, SEMICOLON) -> f194 `block_item_or_statement_or_null6
| TUPLE3 (STRING "case_items1", case_item1, case_item2) -> f195 (`case_items1)
   | TUPLE3 (STRING "data_declaration_or_module_instantiation1",
   instantiation_base1, SEMICOLON) -> f197 `data_declaration_or_module_instantiation1
   | TUPLE3 (STRING "data_type_primitive1",
      TUPLE3 (STRING "data_type_primitive_scalar1", Reg, EMPTY_TOKEN),
   EMPTY_TOKEN) -> f200 `data_type_primitive1
   | TUPLE3 (STRING "data_type_primitive1",
      TUPLE3 (STRING "data_type_primitive_scalar1", Reg, EMPTY_TOKEN),
      TUPLE6 (STRING "decl_variable_dimension1", LBRACK, TK_DecNumber hi, COLON,
    TK_DecNumber lo, RBRACK)) -> f204 `data_type_primitive1
   | TUPLE3 (STRING "data_type_primitive1",
      TUPLE3 (STRING "data_type_primitive_scalar1", Reg, EMPTY_TOKEN),
      TUPLE6 (STRING "decl_variable_dimension1", LBRACK, expr', COLON,
     TK_DecNumber lo, RBRACK)) -> f208 `data_type_primitive1
| TUPLE3 (STRING "end1", End, EMPTY_TOKEN) -> f209 `end1
| TUPLE3 (STRING "event_control4", AT, STAR) -> f210 `event_control4
| TUPLE3 (STRING "event_expression1", Posedge, unqualified_id1) -> f211 `event_expression1
| TUPLE3 (STRING "event_expression1", Negedge, unqualified_id1) -> f212 `event_expression1
| TUPLE3 (STRING "expression_or_dist1", cond_expr2, EMPTY_TOKEN) -> f213 `expression_or_dist1
| TUPLE3 (STRING "generate_if1", If, expression_in_parens1) -> f214 `generate_if1
| TUPLE3 (STRING "initial_construct1", Initial, seq_block1) -> f215 (`initial_construct1 seq_block1)
   | TUPLE3 (STRING "instantiation_base1",
      TUPLE3 (STRING "data_type_primitive1",
        TUPLE3 (STRING "data_type_primitive_scalar1", Reg, EMPTY_TOKEN),
        EMPTY_TOKEN),
   TLIST lst) -> f220 (`instantiation_base1 lst)
   | TUPLE3 (STRING "instantiation_base1",
      TUPLE3 (STRING "data_type_primitive1",
        TUPLE3 (STRING "data_type_primitive_scalar1", Reg, EMPTY_TOKEN),
        TUPLE6 (STRING "decl_variable_dimension1", LBRACK, TK_DecNumber hi, COLON,
          TK_DecNumber lo, RBRACK)),
    TLIST lst) -> f226 (`instantiation_base1 lst)
   | TUPLE3 (STRING "instantiation_base1",
      TUPLE3 (STRING "data_type_primitive1",
        TUPLE3 (STRING "data_type_primitive_scalar1", Reg, EMPTY_TOKEN),
        TUPLE6 (STRING "decl_variable_dimension1", LBRACK, expr', COLON,
          TK_DecNumber lo, RBRACK)),
   TLIST lst) -> f232 (`instantiation_base1 lst)
| TUPLE3 (STRING "instantiation_base1", unqualified_id1, TLIST lst) -> f233 (`instantiation_base1 lst)
| TUPLE3 (STRING "ml_start1", TLIST lst, End_of_file) -> f234 (`ml_start1 lst)
   | TUPLE3 (STRING "module_parameter_port_list_item_last1",
      TUPLE3 (STRING "module_parameter_port_list_trailing_comma1", TLIST lst, COMMA),
      TUPLE4 (STRING "module_parameter_port1", Parameter,
        param_type_followed_by_id_and_dimensions_opt4,
        TUPLE4 (STRING "trailing_assign1", EQUALS,
          hex_based_number1,
       EMPTY_TOKEN))) -> f241 (`module_parameter_port_list_item_last1 lst)
   | TUPLE3 (STRING "module_parameter_port_list_item_last1",
      TUPLE3 (STRING "module_parameter_port_list_trailing_comma1", TLIST lst, COMMA),
   TUPLE4 (STRING "module_parameter_port2", Parameter, Type, type_assignment1)) -> f244 (`module_parameter_port_list_item_last1 lst)
| TUPLE3 (STRING "net_variable1", SymbolIdentifier id, EMPTY_TOKEN) -> f245 `net_variable1
| TUPLE3 (STRING "parameter_value_byname_list_item_last2",
      TUPLE3 (STRING "parameter_value_byname_list_trailing_comma1", TLIST lst, COMMA),
      TUPLE6 (STRING "parameter_value_byname1", DOT, SymbolIdentifier id, LPAREN,
    TK_DecNumber num, RPAREN)) -> f249 (`parameter_value_byname_list_item_last2 lst)
| TUPLE3 (STRING "parameter_value_byname_list_item_last2",
      TUPLE3 (STRING "parameter_value_byname_list_trailing_comma1", TLIST lst, COMMA),
      TUPLE6 (STRING "parameter_value_byname1", DOT, SymbolIdentifier id, LPAREN,
     unqualified_id1, RPAREN)) -> f253 (`parameter_value_byname_list_item_last2 lst)
| TUPLE3 (STRING "port1",
      TUPLE3 (STRING "port_reference1", unqualified_id1, EMPTY_TOKEN),
   EMPTY_TOKEN) -> f256 `port1
| TUPLE3 (STRING "procedural_timing_control_statement2", stmt, seq_block) -> f258 (`procedural_timing_control_statement2 (stmt, seq_block))
| TUPLE5 (STRING "event_control2", AT, LPAREN, TLIST lst, RPAREN) -> f364 (`event_control2 lst)
| TUPLE3 (STRING "event_control4", AT, STAR) -> f260 (`event_control4)
| TUPLE3 (STRING "reference2",
      TUPLE3 (STRING "reference2", unqualified_id1,
        TUPLE3 (STRING "hierarchy_extension1", DOT, unqualified_id2)),
    TUPLE3 (STRING "hierarchy_extension1", DOT, unqualified_id3)) -> f264 `reference2
| TUPLE3 (STRING "reference3",
      TUPLE3 (STRING "reference2",
        TUPLE3 (STRING "reference2", unqualified_id1,
          TUPLE3 (STRING "hierarchy_extension1", DOT, unqualified_id2)),
        TUPLE3 (STRING "hierarchy_extension1", DOT, unqualified_id3)),
      TUPLE6 (STRING "select_variable_dimension1", LBRACK, TK_DecNumber hi, COLON,
     TK_DecNumber lo, RBRACK)) -> f271 `reference3
| TUPLE3 (STRING "reference3", reference3,
      TUPLE6 (STRING "select_variable_dimension1", LBRACK, TK_DecNumber hi, COLON,
    TK_DecNumber lo, RBRACK)) -> f274 `reference3
| TUPLE3 (STRING "reference3", unqualified_id1,
      TUPLE6 (STRING "select_variable_dimension3", LBRACK, unqualified_id2,
    PLUS_COLON, unqualified_id3, RBRACK)) -> f277 `reference3
| TUPLE3 (STRING "reference3", unqualified_id1,
   TUPLE4 (STRING "select_variable_dimension2", LBRACK, TK_DecNumber lo, RBRACK)) -> f279 `reference3
| TUPLE3 (STRING "reference3", unqualified_id1,
      TUPLE4 (STRING "select_variable_dimension2", LBRACK, unqualified_id2,
    RBRACK)) -> f282 `reference3
| TUPLE3 (STRING "reference_or_call_base1", unqualified_id1,
  TUPLE4 (STRING "call_base1", LPAREN, TLIST lst, RPAREN)) -> f284 (`reference_or_call_base1 lst)
| TUPLE3 (STRING "sequence_repetition_expr1", expression_or_dist1,
   EMPTY_TOKEN) -> f286 `sequence_repetition_expr1
| TUPLE3 (STRING "statement3", reference, SEMICOLON) -> f287 `statement3
| TUPLE3 (STRING "statement_item6", assignment_statement_no_expr1, SEMICOLON) -> f289 (`statement_item6 assignment_statement_no_expr1)
| TUPLE3 (STRING "system_tf_call1", SystemTFIdentifier id,
  TUPLE4 (STRING "call_base1", LPAREN, TLIST lst, RPAREN)) -> f291 (`system_tf_call1 lst)
| TUPLE3 (STRING "unary_prefix_expr2", VBAR, reference) -> f293 (`unary_prefix_expr2 reference)
| TUPLE3 (STRING "unary_prefix_expr2", TILDE, reference) -> f295 (`unary_prefix_expr2 reference)
| TUPLE3 (STRING "unary_prefix_expr2", PLING, reference) -> f297 (`unary_prefix_expr2 reference)
| TUPLE3 (STRING "unary_prefix_expr2", HYPHEN, reference) -> f298 (`unary_prefix_expr2 reference)
| TUPLE3 (STRING "unary_prefix_expr2", AMPERSAND, reference) -> f299 (`unary_prefix_expr2 reference)
| TUPLE3 (STRING "unqualified_id1", SymbolIdentifier id, param) -> f300 (`unqualified_id1 (id, param))
| TUPLE5 (STRING "parameters2", HASH, LPAREN, TLIST lst, RPAREN) -> f302 (`parameters2 lst)
| TUPLE12 (STRING "module_or_interface_declaration1", Module, EMPTY_TOKEN,
      SymbolIdentifier id, EMPTY_TOKEN, EMPTY_TOKEN,
      TUPLE4 (STRING "module_port_list_opt1", LPAREN, TLIST lst, RPAREN),
   EMPTY_TOKEN, SEMICOLON, EMPTY_TOKEN, Endmodule, EMPTY_TOKEN) -> f306 (`module_or_interface_declaration1 (id, lst, []))
| TUPLE12 (STRING "module_or_interface_declaration1", Module, EMPTY_TOKEN,
      SymbolIdentifier id, EMPTY_TOKEN, EMPTY_TOKEN,
      TUPLE4 (STRING "module_port_list_opt1", LPAREN, TLIST lst, RPAREN),
   EMPTY_TOKEN, SEMICOLON, TLIST lst', Endmodule, EMPTY_TOKEN) -> f310 (`module_or_interface_declaration1 (id, lst, lst'))
| TUPLE12 (STRING "module_or_interface_declaration1", Module, EMPTY_TOKEN,
      SymbolIdentifier id, EMPTY_TOKEN,
      TUPLE5 (STRING "module_parameter_port_list_opt1", HASH, LPAREN, TLIST lst, RPAREN),
      TUPLE4 (STRING "module_port_list_opt1", LPAREN, TLIST lst', RPAREN),
   EMPTY_TOKEN, SEMICOLON, TLIST lst'', Endmodule, EMPTY_TOKEN) -> f315 (`module_port_list_opt1 (lst, lst', lst''))
| TUPLE10 (STRING "loop_statement1", For, LPAREN,
      TUPLE4 (STRING "for_init_decl_or_assign1", unqualified_id1, EQUALS, TK_DecNumber num),
      SEMICOLON, ELIST elst, SEMICOLON, assignment_statement_no_expr1,
   RPAREN, seq_block) -> f319 (`loop_statement1 (elst, seq_block))
| TUPLE6 (STRING "cast1", data_type_primitive1, QUOTE, LPAREN, reference3, RPAREN) -> f320 `cast1
| ELIST [] -> f322 `elist
| TK_StringLiteral "" -> f323 `string_lit
| TK_DecNumber num -> f324 `dec_num
| SymbolIdentifier id -> f325 `ident
| STRING _ -> f432 `string
| COMMA -> f433 `comma
| LESS -> f326 `less
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
   SymbolIdentifier _, EMPTY_TOKEN) -> f338 `param_type_followed_by_id_and_dimensions_opt4
   | TUPLE6
     (STRING "select_variable_dimension1", LBRACK, ELIST _, COLON,
   TK_DecNumber _, RBRACK) -> f341 `select_variable_dimension1
   | TUPLE6
     (STRING "simple_immediate_assertion_statement1", Assert, LPAREN,
      reference3, RPAREN,
      TUPLE3
       (STRING "action_block3", Else,
        TUPLE3
      (STRING "statement3", reference_or_call_base1, SEMICOLON))) -> f348 `simple_immediate_assertion_statement1
   | TUPLE5
     (STRING "data_declaration_or_module_instantiation2", Automatic,
   EMPTY_TOKEN, instantiation_base1, SEMICOLON) -> f351 `data_declaration_or_module_instantiation2
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
   SEMICOLON) -> f363 `net_declaration2
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
   SEMICOLON) -> f375 `net_declaration2
   | TUPLE5
     (STRING "net_declaration4", Wire,
      TUPLE3 (STRING "delay31", HASH, TK_RealTime _),
      TUPLE4
       (STRING "net_decl_assign1", SymbolIdentifier _, EQUALS,
        cond_expr2),
   SEMICOLON) -> f382 `net_declaration4
   | TUPLE5
     (STRING "port_declaration_noattr1", Output, Wire,
      TUPLE4
       (STRING "data_type_or_implicit_basic_followed_by_id_and_dimensions_opt4",
        TUPLE6
         (STRING "decl_variable_dimension1", LBRACK, TK_DecNumber _, COLON,
          TK_DecNumber _, RBRACK),
        unqualified_id1, EMPTY_TOKEN),
   EMPTY_TOKEN) -> f391 `port_declaration_noattr1
   | TUPLE5
     (STRING "port_declaration_noattr1", Output, Wire,
      TUPLE3
       (STRING
         "type_identifier_or_implicit_basic_followed_by_id_and_dimensions_opt4",
        unqualified_id1, EMPTY_TOKEN),
   EMPTY_TOKEN) -> f398 `port_declaration_noattr1
   | TUPLE5
     (STRING "port_declaration_noattr1", Output, EMPTY_TOKEN,
      TUPLE5
       (STRING
         "type_identifier_or_implicit_basic_followed_by_id_and_dimensions_opt2",
        unqualified_id1, EMPTY_TOKEN, unqualified_id2,
        EMPTY_TOKEN),
   EMPTY_TOKEN) -> f406 `port_declaration_noattr1
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
   EMPTY_TOKEN) -> f418 `port_declaration_noattr1
   | TUPLE5
     (STRING "port_declaration_noattr1", Input, Wire,
      TUPLE4
       (STRING "data_type_or_implicit_basic_followed_by_id_and_dimensions_opt4",
        TUPLE6
         (STRING "decl_variable_dimension1", LBRACK, TK_DecNumber _, COLON,
          TK_DecNumber _, RBRACK),
        unqualified_id1, EMPTY_TOKEN),
   EMPTY_TOKEN) -> f427 `port_declaration_noattr1
   | TUPLE5
     (STRING "port_declaration_noattr1", Input, Wire,
      TUPLE3
       (STRING
         "type_identifier_or_implicit_basic_followed_by_id_and_dimensions_opt4",
        unqualified_id1, EMPTY_TOKEN),
   EMPTY_TOKEN) -> f434 `port_declaration_noattr1
   | TUPLE5
     (STRING "port_declaration_noattr1", Input, EMPTY_TOKEN,
      TUPLE5
       (STRING
         "type_identifier_or_implicit_basic_followed_by_id_and_dimensions_opt2",
        unqualified_id1, EMPTY_TOKEN, unqualified_id2,
        EMPTY_TOKEN),
   EMPTY_TOKEN) -> f442 `port_declaration_noattr1
   | TUPLE5
     (STRING "port_declaration_noattr1", Input, EMPTY_TOKEN,
      TUPLE4
       (STRING "data_type_or_implicit_basic_followed_by_id_and_dimensions_opt1",
        TUPLE3
         (STRING "data_type_primitive1",
          TUPLE3 (STRING "data_type_primitive_scalar1", Logic, EMPTY_TOKEN),
          EMPTY_TOKEN),
        unqualified_id1, EMPTY_TOKEN),
   EMPTY_TOKEN) -> f452 `port_declaration_noattr1
   | TUPLE5 (STRING "port_declaration_noattr1", Input, EMPTY_TOKEN,
      TUPLE4 (STRING "data_type_or_implicit_basic_followed_by_id_and_dimensions_opt1",
        TUPLE3 (STRING "data_type_primitive1",
          TUPLE3 (STRING "data_type_primitive_scalar1", Logic, EMPTY_TOKEN),
          TUPLE6 (STRING "decl_variable_dimension1", LBRACK, TK_DecNumber _, COLON,
            TK_DecNumber _, RBRACK)),
        unqualified_id1, EMPTY_TOKEN),
   EMPTY_TOKEN) -> f460 `port_declaration_noattr1
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
   EMPTY_TOKEN) -> f472 `port_declaration_noattr1
   | TUPLE5
     (STRING "port_declaration_noattr1", Inout, Wire,
      TUPLE3
       (STRING
         "type_identifier_or_implicit_basic_followed_by_id_and_dimensions_opt4",
        unqualified_id1, EMPTY_TOKEN),
   EMPTY_TOKEN) -> f479 `port_declaration_noattr1
   | TUPLE5
     (STRING "port_declaration_noattr1", Inout, EMPTY_TOKEN,
      TUPLE3
       (STRING
         "type_identifier_or_implicit_basic_followed_by_id_and_dimensions_opt4",
        unqualified_id1, EMPTY_TOKEN),
   EMPTY_TOKEN) -> f486 `port_declaration_noattr1
   | TUPLE4
     (STRING "expression_list_proper1",
      TUPLE3 (STRING "hex_based_number1", TK_HexBase _, TK_HexDigits _),
      COMMA,
   TUPLE3 (STRING "hex_based_number1", TK_HexBase _, TK_HexDigits _)) -> f491 `expression_list_proper1
   | TUPLE4 (STRING "expression_list_proper1", expression_list_proper1, COMMA,
   TUPLE3 (STRING "hex_based_number1", TK_HexBase _, TK_HexDigits _)) -> f494 `expression_list_proper1
   | TUPLE4 (STRING "generate_block1", begin1, EMPTY_TOKEN,
   TUPLE3 (STRING "end1", End, EMPTY_TOKEN)) -> f497 (`generate_block1 (begin1, []))
   | TUPLE4 (STRING "jump_statement4", Return, unqualified_id1, SEMICOLON) -> f499 `jump_statement4
   | TUPLE4 (STRING "module_parameter_port1", Parameter,
      TUPLE6 (STRING "param_type_followed_by_id_and_dimensions_opt5", Int,
        EMPTY_TOKEN, EMPTY_TOKEN, SymbolIdentifier _, EMPTY_TOKEN),
      TUPLE4 (STRING "trailing_assign1", EQUALS, unary_prefix_expr2,
     EMPTY_TOKEN)) -> f507 `module_parameter_port1
   | TUPLE4 (STRING "module_parameter_port1", Parameter,
      param_type_followed_by_id_and_dimensions_opt4,
      TUPLE4 (STRING "trailing_assign1", EQUALS, bin_based_number1,
     EMPTY_TOKEN)) -> f513 `module_parameter_port1
   | TUPLE4 (STRING "module_parameter_port2", Parameter, Type,
      TUPLE4 (STRING "type_assignment1", SymbolIdentifier _, EQUALS,
        TUPLE3 (STRING "data_type_primitive1",
          TUPLE3 (STRING "data_type_primitive_scalar1", Logic, EMPTY_TOKEN),
       EMPTY_TOKEN))) -> f521 `module_parameter_port2
   | TUPLE4 (STRING "port_declaration_noattr4",
      TUPLE6 (STRING "type_identifier_followed_by_id3", unqualified_id1, DOT,
        SymbolIdentifier _, EMPTY_TOKEN, SymbolIdentifier _),
   EMPTY_TOKEN, EMPTY_TOKEN) -> f527 `port_declaration_noattr4
   | TUPLE4 (STRING "qualified_id2", unqualified_id1, COLON_COLON,
   unqualified_id2) -> f530 `qualified_id2
   | TUPLE4 (STRING "tf_port_item1", EMPTY_TOKEN,
      TUPLE4 (STRING "data_type_or_implicit_basic_followed_by_id_and_dimensions_opt1",
        TUPLE3 (STRING "data_type_primitive1",
          TUPLE3 (STRING "data_type_primitive_scalar1", Logic, EMPTY_TOKEN),
          TUPLE6 (STRING "decl_variable_dimension1", LBRACK, TK_DecNumber _, COLON,
            TK_DecNumber _, RBRACK)),
        unqualified_id1, EMPTY_TOKEN),
   EMPTY_TOKEN) -> f542 `tf_port_item1
   | TUPLE4 (STRING "tf_port_item1", EMPTY_TOKEN,
      TUPLE4 (STRING "data_type_or_implicit_basic_followed_by_id_and_dimensions_opt1",
        TUPLE3 (STRING "data_type_primitive1",
          TUPLE3 (STRING "data_type_primitive_scalar5", Byte, EMPTY_TOKEN),
          EMPTY_TOKEN),
        unqualified_id1, EMPTY_TOKEN),
   EMPTY_TOKEN) -> f552 `tf_port_item1
| TUPLE3 (STRING "always_construct1", Always_latch, seq_block1) -> f553 (`always_construct1 seq_block1)
| TUPLE3 (STRING "always_construct1", Always_ff,
   procedural_timing_control_statement2) -> f556 (`always_construct1 procedural_timing_control_statement2)
| TUPLE3 (STRING "always_construct1", Always_comb, seq_block1) -> f557 (`always_construct1 seq_block1)
| TUPLE3 (STRING "any_port_list_item_last1",
      TUPLE3 (STRING "any_port_list_trailing_comma1", TLIST _, COMMA),
   TUPLE5 (STRING "port_named2", DOT, SymbolIdentifier _, LPAREN, RPAREN)) -> f561 (`any_port_list_item_last1 [])
| TUPLE3 (STRING "any_port_list_item_last1",
      TUPLE3 (STRING "any_port_list_trailing_comma1", TLIST lst, COMMA),
   TUPLE3 (STRING "port_named3", DOT, SymbolIdentifier _)) -> f565 (`any_port_list_item_last1 lst)
| TUPLE3 (STRING "begin1", Begin,
   TUPLE3 (STRING "label_opt1", COLON, SymbolIdentifier _)) -> f568 `begin1
| TUPLE3 (STRING "conditional_generate_construct2",
      TUPLE3 (STRING "generate_if1", If, expression_in_parens1),
   generate_block1) -> f572 `conditional_generate_construct2
| TUPLE3 (STRING "data_type_primitive1",
      TUPLE3 (STRING "data_type_primitive_scalar1", Logic, EMPTY_TOKEN),
   EMPTY_TOKEN) -> f576 `data_type_primitive1
| TUPLE3 (STRING "data_type_primitive1",
      TUPLE3 (STRING "data_type_primitive_scalar1", Logic, EMPTY_TOKEN),
      TUPLE6
       (STRING "decl_variable_dimension1", LBRACK, TK_DecNumber _, COLON,
     TK_DecNumber _, RBRACK)) -> f582 `data_type_primitive1
| TUPLE3 (STRING "data_type_primitive1",
      TUPLE3 (STRING "data_type_primitive_scalar1", Logic, EMPTY_TOKEN),
      TUPLE6
       (STRING "decl_variable_dimension1", LBRACK, unqualified_id1,
     COLON, TK_DecNumber _, RBRACK)) -> f588 `data_type_primitive1
| TUPLE3 (STRING "data_type_primitive1",
      TUPLE3 (STRING "data_type_primitive_scalar1", Bit, EMPTY_TOKEN),
   EMPTY_TOKEN) -> f592 `data_type_primitive1
| TUPLE3 (STRING "data_type_primitive1",
      TUPLE3 (STRING "data_type_primitive_scalar5", Int, EMPTY_TOKEN),
   EMPTY_TOKEN) -> f596 `data_type_primitive1
| TUPLE3 (STRING "data_type_primitive1",
      TUPLE3 (STRING "data_type_primitive_scalar5", Byte, EMPTY_TOKEN),
   EMPTY_TOKEN) -> f600 `data_type_primitive1
| TUPLE3 (STRING "dec_based_number1", TK_DecBase _, TK_DecDigits _) -> f601 `dec_based_number1
| TUPLE3 (STRING "decl_dimensions2",
      TUPLE6
       (STRING "decl_variable_dimension1", LBRACK, TK_DecNumber _, COLON,
        TK_DecNumber _, RBRACK),
      TUPLE6
       (STRING "decl_variable_dimension1", LBRACK, TK_DecNumber _, COLON,
     TK_DecNumber _, RBRACK)) -> f609 `decl_dimensions2
| TUPLE3 (STRING "hex_based_number1", TK_HexBase _, TK_HexDigits _) -> f610 `hex_based_number1
| TUPLE3 (STRING "inc_or_dec_expression2", unqualified_id1, PLUS_PLUS) -> f612 `inc_or_dec_expression2
| TUPLE3 (STRING "label_opt1", COLON, SymbolIdentifier _) -> f613 `label_opt1
| TUPLE3 (STRING "port_named3", DOT, SymbolIdentifier _) -> f614 `port_named3
| TUPLE3 (STRING "reference2", unqualified_id1,
   TUPLE3 (STRING "hierarchy_extension1", DOT, unqualified_id2)) -> f617 `reference2
| TUPLE3
     (STRING "reference3", unqualified_id1,
      TUPLE6
       (STRING "select_variable_dimension1", LBRACK, ELIST _, COLON,
     TK_DecNumber _, RBRACK)) -> f622 `reference3
| TUPLE12 (STRING "module_or_interface_declaration1", Module, EMPTY_TOKEN,
      SymbolIdentifier id, EMPTY_TOKEN,
      TUPLE5 (STRING "module_parameter_port_list_opt1", HASH, LPAREN, TLIST lst, RPAREN),
      TUPLE4 (STRING "module_port_list_opt1", LPAREN, TLIST lst', RPAREN),
      EMPTY_TOKEN, SEMICOLON, TLIST lst'', Endmodule,
   TUPLE3 (STRING "label_opt1", COLON, SymbolIdentifier _)) -> f631 (`module_or_interface_declaration1 (id, lst, lst'))
| TUPLE12 (STRING "module_or_interface_declaration1", Interface, EMPTY_TOKEN,
      SymbolIdentifier id, EMPTY_TOKEN,
      TUPLE5 (STRING "module_parameter_port_list_opt1", HASH, LPAREN, TLIST lst, RPAREN),
      TUPLE4 (STRING "module_port_list_opt1", LPAREN, TLIST lst', RPAREN),
   EMPTY_TOKEN, SEMICOLON, EMPTY_TOKEN, Endinterface, EMPTY_TOKEN) -> f639 (`module_or_interface_declaration1 (id, lst, lst'))
| TUPLE11
     (STRING "function_declaration1", Function, EMPTY_TOKEN,
      TUPLE4
       (STRING "data_type_or_implicit_basic_followed_by_id_and_dimensions_opt5",
        Void, unqualified_id1, EMPTY_TOKEN),
      LPAREN, TLIST _, RPAREN, SEMICOLON, TLIST _, Endfunction,
   TUPLE3 (STRING "label_opt1", COLON, SymbolIdentifier _)) -> f646 `function_declaration1
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
   TUPLE3 (STRING "label_opt1", COLON, SymbolIdentifier _)) -> f659 `function_declaration1
| TUPLE10 (STRING "loop_statement1", For, LPAREN,
      TUPLE5 (STRING "for_init_decl_or_assign2",
        TUPLE3 (STRING "data_type_primitive1",
          TUPLE3 (STRING "data_type_primitive_scalar5", Int, Unsigned),
          EMPTY_TOKEN),
        SymbolIdentifier _, EQUALS, TK_DecNumber _),
      SEMICOLON, ELIST elst, SEMICOLON, inc_or_dec_expression2, RPAREN,
   seq_block) -> f667 (`loop_statement1 (elst, seq_block))
| TUPLE4 (STRING "assignment_pattern1", QUOTE_LBRACE, TLIST lst, RBRACE) -> f668 (`assignment_pattern1 lst)
| TUPLE9 (STRING "case_statement3", Unique, Case, LPAREN, STRING "unqualified_id1",
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
   Endcase) -> f684 `case_statement3
| TUPLE9 (STRING "case_statement3", EMPTY_TOKEN, Case, LPAREN,
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
   Endcase) -> f697 `case_statement3
| TUPLE8 (STRING "case_statement1", Unique, Case, LPAREN,
      TUPLE3
       (STRING "reference2", STRING "unqualified_id1",
        TUPLE3 (STRING "hierarchy_extension1", DOT, STRING "unqualified_id1")),
   RPAREN, STRING "case_items1", Endcase) -> f703 `case_statement1
| TUPLE8 (STRING "case_statement1", Unique, Case, LPAREN, STRING "unqualified_id1",
   RPAREN, STRING "case_items1", Endcase) -> f706 `case_statement1
| oth -> othpatlst := oth :: !othpatlst; failwith "verible_pat"
   
and verible_pat itm = patlst := itm :: !patlst; let rslt = verible_pat' itm in patlst := List.tl !patlst; rslt

and  f104 lbl = mark (104) lbl
and  f110 lbl = mark (110) lbl
and  f114 lbl = mark (114) lbl
and  f12 lbl = mark (12) lbl
and  f120 lbl = mark (120) lbl
and  f124 lbl = mark (124) lbl
and  f127 lbl = mark (127) lbl
and  f128 lbl = mark (128) lbl
and  f129 lbl = mark (129) lbl
and  f130 lbl = mark (130) lbl
and  f131 lbl = mark (131) lbl
and  f132 lbl = mark (132) lbl
and  f133 lbl = mark (133) lbl
and  f134 lbl = mark (134) lbl
and  f138 lbl = mark (138) lbl
and  f139 lbl = mark (139) lbl
and  f140 lbl = mark (140) lbl
and  f142 lbl = mark (142) lbl
and  f144 lbl = mark (144) lbl
and  f145 lbl = mark (145) lbl
and  f149 lbl = mark (149) lbl
and  f152 lbl = mark (152) lbl
and  f156 lbl = mark (156) lbl
and  f157 lbl = mark (157) lbl
and  f159 lbl = mark (159) lbl
and  f16 lbl = mark (16) lbl
and  f163 lbl = mark (163) lbl
and  f166 lbl = mark (166) lbl
and  f169 lbl = mark (169) lbl
and  f174 lbl = mark (174) lbl
and  f175 lbl = mark (175) lbl
and  f176 lbl = mark (176) lbl
and  f177 lbl = mark (177) lbl
and  f178 lbl = mark (178) lbl
and  f179 lbl = mark (179) lbl
and  f18 lbl = mark (18) lbl
and  f180 lbl = mark (180) lbl
and  f184 lbl = mark (184) lbl
and  f187 lbl = mark (187) lbl
and  f19 lbl = mark (19) lbl
and  f191 lbl = mark (191) lbl
and  f192 lbl = mark (192) lbl
and  f193 lbl = mark (193) lbl
and  f194 lbl = mark (194) lbl
and  f195 lbl = mark (195) lbl
and  f197 lbl = mark (197) lbl
and  f200 lbl = mark (200) lbl
and  f204 lbl = mark (204) lbl
and  f208 lbl = mark (208) lbl
and  f209 lbl = mark (209) lbl
and  f21 lbl = mark (21) lbl
and  f210 lbl = mark (210) lbl
and  f211 lbl = mark (211) lbl
and  f212 lbl = mark (212) lbl
and  f213 lbl = mark (213) lbl
and  f214 lbl = mark (214) lbl
and  f215 lbl = mark (215) lbl
and  f22 lbl = mark (22) lbl
and  f220 lbl = mark (220) lbl
and  f226 lbl = mark (226) lbl
and  f232 lbl = mark (232) lbl
and  f233 lbl = mark (233) lbl
and  f234 lbl = mark (234) lbl
and  f24 lbl = mark (24) lbl
and  f241 lbl = mark (241) lbl
and  f244 lbl = mark (244) lbl
and  f245 lbl = mark (245) lbl
and  f249 lbl = mark (249) lbl
and  f25 lbl = mark (25) lbl
and  f253 lbl = mark (253) lbl
and  f256 lbl = mark (256) lbl
and  f258 lbl = mark (258) lbl
and  f260 lbl = mark (260) lbl
and  f264 lbl = mark (264) lbl
and  f271 lbl = mark (271) lbl
and  f274 lbl = mark (274) lbl
and  f277 lbl = mark (277) lbl
and  f279 lbl = mark (279) lbl
and  f282 lbl = mark (282) lbl
and  f284 lbl = mark (284) lbl
and  f286 lbl = mark (286) lbl
and  f287 lbl = mark (287) lbl
and  f289 lbl = mark (289) lbl
and  f29 lbl = mark (29) lbl
and  f291 lbl = mark (291) lbl
and  f293 lbl = mark (293) lbl
and  f294 lbl = mark (294) lbl
and  f295 lbl = mark (295) lbl
and  f296 lbl = mark (296) lbl
and  f297 lbl = mark (297) lbl
and  f298 lbl = mark (298) lbl
and  f299 lbl = mark (299) lbl
and  f300 lbl = mark (300) lbl
and  f302 lbl = mark (302) lbl
and  f306 lbl = mark (306) lbl
and  f31 lbl = mark (31) lbl
and  f310 lbl = mark (310) lbl
and  f315 lbl = mark (315) lbl
and  f319 lbl = mark (319) lbl
and  f320 lbl = mark (320) lbl
and  f321 lbl = mark (321) lbl
and  f322 lbl  = mark (322) lbl
and  f323 lbl = mark (323) lbl
and  f324 lbl = mark (324) lbl
and  f325 lbl = mark (325) lbl
and  f326 lbl = mark (326) lbl
and  f33 lbl = mark (33) lbl
and  f338 lbl = mark (338) lbl
and  f341 lbl = mark (341) lbl
and  f348 lbl = mark (348) lbl
and  f35 lbl = mark (35) lbl
and  f351 lbl = mark (351) lbl
and  f363 lbl = mark (363) lbl
and  f364 lbl = mark (363) lbl
and  f37 lbl = mark (37) lbl
and  f375 lbl = mark (375) lbl
and  f382 lbl = mark (382) lbl
and  f39 lbl = mark (39) lbl
and  f391 lbl = mark (391) lbl
and  f398 lbl = mark (398) lbl
and  f406 lbl = mark (406) lbl
and  f418 lbl = mark (418) lbl
and  f427 lbl = mark (427) lbl
and  f432 lbl = mark (434) lbl
and  f433 lbl = mark (434) lbl
and  f434 lbl = mark (434) lbl
and  f442 lbl = mark (442) lbl
and  f452 lbl = mark (452) lbl
and  f46 lbl = mark (46) lbl
and  f460 lbl = mark (460) lbl
and  f472 lbl = mark (472) lbl
and  f479 lbl = mark (479) lbl
and  f486 lbl = mark (486) lbl
and  f491 lbl = mark (491) lbl
and  f494 lbl = mark (494) lbl
and  f497 lbl = mark (497) lbl
and  f499 lbl = mark (499) lbl
and  f507 lbl = mark (507) lbl
and  f513 lbl = mark (513) lbl
and  f521 lbl = mark (521) lbl
and  f527 lbl = mark (527) lbl
and  f530 lbl = mark (530) lbl
and  f54 lbl = mark (54) lbl
and  f542 lbl = mark (542) lbl
and  f552 lbl = mark (552) lbl
and  f553 lbl = mark (553) lbl
and  f556 lbl = mark (556) lbl
and  f557 lbl = mark (557) lbl
and  f561 lbl = mark (561) lbl
and  f565 lbl = mark (565) lbl
and  f568 lbl = mark (568) lbl
and  f572 lbl = mark (572) lbl
and  f576 lbl = mark (576) lbl
and  f582 lbl = mark (582) lbl
and  f588 lbl = mark (588) lbl
and  f592 lbl = mark (592) lbl
and  f596 lbl = mark (596) lbl
and  f60 lbl = mark (60) lbl
and  f600 lbl = mark (600) lbl
and  f601 lbl = mark (601) lbl
and  f609 lbl = mark (609) lbl
and  f610 lbl = mark (610) lbl
and  f612 lbl = mark (612) lbl
and  f613 lbl = mark (613) lbl
and  f614 lbl = mark (614) lbl
and  f617 lbl = mark (617) lbl
and  f622 lbl = mark (622) lbl
and  f631 lbl = mark (631) lbl
and  f639 lbl = mark (639) lbl
and  f64 lbl = mark (64) lbl
and  f646 lbl = mark (646) lbl
and  f659 lbl = mark (659) lbl
and  f667 lbl  = mark (667) lbl
and  f668 lbl = mark (668) lbl
and  f67 lbl = mark (67) lbl
and  f684 lbl = mark (684) lbl
and  f69 lbl  = mark (69) lbl
and  f697 lbl = mark (697) lbl
and  f70 lbl = mark (70) lbl
and  f703 lbl = mark (703) lbl
and  f706 lbl = mark (706) lbl
and  f81 lbl = mark (81) lbl
and  f89 lbl = mark (89) lbl
and  f9 lbl = mark (9) lbl
and  f96 lbl = mark (96) lbl
