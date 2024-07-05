open Source_text_verible

let othpat = ref End_of_file
let patlst = ref []
let othpatlst = ref []

let rec verible_pat' = function
| TUPLE9 (STRING "task_declaration1", Task, EMPTY_TOKEN, SymbolIdentifier id,
   EMPTY_TOKEN, SEMICOLON, TLIST lst, Endtask, EMPTY_TOKEN) -> List.iter verible_pat lst
| TUPLE8 (STRING "case_statement1", EMPTY_TOKEN, Case, LPAREN,
   expr,
   RPAREN, case_items1, Endcase) -> ()
| TUPLE7 (STRING "conditional_statement2", EMPTY_TOKEN, If,
   expression_in_parens1,
   then_statement,
   Else, else_statement) -> ()
| TUPLE7 (STRING "expr_primary_braces2", LBRACE, braced, LBRACE,
	  reference3, RBRACE, RBRACE) -> ()
| TUPLE6 (STRING "cond_expr2", expr_primary, QUERY, expr_true, COLON, expr_false) -> ()
| TUPLE6 (STRING "continuous_assign1", Assign, EMPTY_TOKEN, dly, TLIST lst,
	  SEMICOLON) -> List.iter verible_pat lst
| TUPLE6 (STRING "decl_variable_dimension1", LBRACK, TK_DecNumber hi, COLON, TK_DecNumber lo, RBRACK) -> ()
| TUPLE6 (STRING "non_anonymous_gate_instance_or_register_variable2",
	  SymbolIdentifier id, EMPTY_TOKEN, LPAREN, TLIST lst, RPAREN) -> List.iter verible_pat lst
| TUPLE6 (STRING "nonblocking_assignment1", lhs, LT_EQ, EMPTY_TOKEN, rhs, SEMICOLON) -> ()
| TUPLE6 (STRING "param_type_followed_by_id_and_dimensions_opt4", (Logic|EMPTY_TOKEN),
   EMPTY_TOKEN, TUPLE6 (STRING "decl_variable_dimension1", LBRACK, TK_DecNumber hi, COLON,
     TK_DecNumber lo, RBRACK),
   SymbolIdentifier id, EMPTY_TOKEN) -> ()
| TUPLE6 (STRING "parameter_value_byname1", DOT, SymbolIdentifier id, LPAREN,
	  TK_DecNumber num, RPAREN) -> ()
| TUPLE6 (STRING "parameter_value_byname1", DOT, SymbolIdentifier id, LPAREN,
   unqualified_id1, RPAREN) -> ()
| TUPLE6 (STRING "port_named1", DOT, SymbolIdentifier id, LPAREN,
	  unqualified_id1, RPAREN) -> ()
| TUPLE6 (STRING "select_variable_dimension1", LBRACK, TK_DecNumber hi, COLON,
   TK_DecNumber lo, RBRACK) -> ()
| TUPLE6 (STRING "select_variable_dimension3", LBRACK, unqualified_id1,
	  PLUS_COLON, unqualified_id2, RBRACK) -> ()
| TUPLE5 (STRING "any_param_declaration4", Localparam,
   TUPLE7
   (STRING "param_type_followed_by_id_and_dimensions_opt2", EMPTY_TOKEN,
     EMPTY_TOKEN, unqualified_id1, EMPTY_TOKEN, SymbolIdentifier id,
     EMPTY_TOKEN),
   TUPLE4 (STRING "trailing_assign1", EQUALS, TK_DecNumber num, EMPTY_TOKEN),
   SEMICOLON) -> ()
| TUPLE5 (STRING "any_param_declaration4", Localparam,
   TUPLE7
   (STRING "param_type_followed_by_id_and_dimensions_opt2", EMPTY_TOKEN,
     EMPTY_TOKEN, unqualified_id1, EMPTY_TOKEN, SymbolIdentifier id,
     EMPTY_TOKEN),
   TUPLE4
   (STRING "trailing_assign1", EQUALS, cond_expr2, EMPTY_TOKEN),
    SEMICOLON) -> ()
| TUPLE5 (STRING "any_param_declaration4", Localparam,
   TUPLE5
   (STRING "param_type_followed_by_id_and_dimensions_opt3", EMPTY_TOKEN,
     EMPTY_TOKEN, unqualified_id1, EMPTY_TOKEN),
   TUPLE4 (STRING "trailing_assign1", EQUALS, rhs, EMPTY_TOKEN),
    SEMICOLON) -> ()
| TUPLE5 (STRING "any_param_declaration4", Localparam,
   param_type_followed_by_id_and_dimensions_opt4,
   TUPLE4 (STRING "trailing_assign1", EQUALS, range_list_in_braces1, EMPTY_TOKEN),
    SEMICOLON) -> verible_pat range_list_in_braces1
| TUPLE5 (STRING "conditional_generate_construct1",
   TUPLE3 (STRING "generate_if1", If, expression_in_parens1),
   generate_block1, Else, conditional_generate_construct1) -> ()
| TUPLE5 (STRING "conditional_statement1", EMPTY_TOKEN, If,
   expression_in_parens1, seq_block1) -> verible_pat seq_block1
| TUPLE5 (STRING "event_control2", AT, LPAREN, TLIST lst, RPAREN) -> List.iter verible_pat lst
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
   SEMICOLON) -> ()
| TUPLE5 (STRING "net_declaration2", Wire,
   TUPLE3
   (STRING "data_type_or_implicit1",
     TUPLE6
     (STRING "decl_variable_dimension1", LBRACK, TK_DecNumber hi, COLON,
       TK_DecNumber lo, RBRACK),
     EMPTY_TOKEN),
    net_variable1, SEMICOLON) -> ()
| TUPLE5 (STRING "port_declaration_noattr1", Output, EMPTY_TOKEN,
   TUPLE4 (STRING "data_type_or_implicit_basic_followed_by_id_and_dimensions_opt1",
     TUPLE3 (STRING "data_type_primitive1",
       TUPLE3 (STRING "data_type_primitive_scalar1", (Reg|Logic), EMPTY_TOKEN),
       EMPTY_TOKEN),
     unqualified_id1, EMPTY_TOKEN),
   EMPTY_TOKEN) -> ()
| TUPLE5 (STRING "port_declaration_noattr1", Output, EMPTY_TOKEN,
   TUPLE4 (STRING "data_type_or_implicit_basic_followed_by_id_and_dimensions_opt1",
     TUPLE3 (STRING "data_type_primitive1",
       TUPLE3 (STRING "data_type_primitive_scalar1", (Reg|Logic), EMPTY_TOKEN),
       TUPLE6 (STRING "decl_variable_dimension1", LBRACK, TK_DecNumber hi, COLON,
         TK_DecNumber lo, RBRACK)),
     unqualified_id1, EMPTY_TOKEN),
    EMPTY_TOKEN) -> ()
| TUPLE5 (STRING "port_declaration_noattr1", Output, EMPTY_TOKEN,
   TUPLE4 (STRING "data_type_or_implicit_basic_followed_by_id_and_dimensions_opt4",
     TUPLE6 (STRING "decl_variable_dimension1", LBRACK, TK_DecNumber hi, COLON,
       TK_DecNumber lo, RBRACK),
     unqualified_id1, EMPTY_TOKEN),
   EMPTY_TOKEN) -> ()
| TUPLE5 (STRING "port_declaration_noattr1", Output, EMPTY_TOKEN,
   TUPLE3 (STRING "type_identifier_or_implicit_basic_followed_by_id_and_dimensions_opt4",
     unqualified_id1, EMPTY_TOKEN),
    EMPTY_TOKEN) -> ()
| TUPLE5 (STRING "port_declaration_noattr1", Input, EMPTY_TOKEN,
   TUPLE4 (STRING "data_type_or_implicit_basic_followed_by_id_and_dimensions_opt4",
     TUPLE6 (STRING "decl_variable_dimension1", LBRACK, TK_DecNumber hi, COLON,
       TK_DecNumber lo, RBRACK),
     unqualified_id1, EMPTY_TOKEN),
   EMPTY_TOKEN) -> ()
| TUPLE5 (STRING "port_declaration_noattr1", Input, EMPTY_TOKEN,
   TUPLE3 (STRING "type_identifier_or_implicit_basic_followed_by_id_and_dimensions_opt4",
     unqualified_id1, EMPTY_TOKEN),
    EMPTY_TOKEN) -> ()
| TUPLE4 (STRING "assignment_statement_no_expr1",
   TUPLE4 (STRING "range_list_in_braces1", LBRACE, TLIST lst, RBRACE), EQUALS,
   ELIST []) -> List.iter verible_pat lst
| TUPLE4 (STRING "assignment_statement_no_expr1", reference, EQUALS, rhs) -> ()
| TUPLE4 (STRING "call_base1", LPAREN, TLIST lst, RPAREN) -> List.iter verible_pat lst
| TUPLE4 (STRING "case_item1", sel, COLON, seq_block) -> verible_pat seq_block
| TUPLE4 (STRING "case_item2", Default, COLON, SEMICOLON) -> ()
| TUPLE4 (STRING "case_item2", Default, COLON, seq_block) -> verible_pat seq_block
| TUPLE4 (STRING "cont_assign1", unqualified_id1, EQUALS, expr) -> ()
| TUPLE4 (STRING "expr_primary_parens1", LPAREN, TLIST lst, RPAREN) -> List.iter verible_pat lst
| TUPLE4 (STRING "expression_in_parens1", LPAREN,
   TUPLE3 (STRING "system_tf_call1", SystemTFIdentifier id,
     TUPLE4 (STRING "call_base1", LPAREN, TLIST lst, RPAREN)),
    RPAREN) -> List.iter verible_pat lst
| TUPLE4 (STRING "expression_in_parens1", LPAREN, cond_expr2, RPAREN) -> ()
| TUPLE4 (STRING "expression_list_proper1", unqualified_id1, COMMA, ELIST []) -> ()
| TUPLE4 (STRING "gate_instance_or_register_variable1", SymbolIdentifier id,
   EMPTY_TOKEN, EMPTY_TOKEN) -> ()
| TUPLE4 (STRING "generate_block1", begin1,
	  TLIST lst, TUPLE3 (end1, End, EMPTY_TOKEN)) -> List.iter verible_pat lst
| TUPLE4 (STRING "generate_region1", Generate, TLIST lst, Endgenerate) -> List.iter verible_pat lst
| TUPLE4 (STRING "module_parameter_port1", Parameter,
   TUPLE5 (STRING "param_type_followed_by_id_and_dimensions_opt3", EMPTY_TOKEN,
     EMPTY_TOKEN, unqualified_id1, EMPTY_TOKEN),
    TUPLE4 (STRING "trailing_assign1", EQUALS, TK_DecNumber num, EMPTY_TOKEN)) -> ()
| TUPLE4 (STRING "module_parameter_port1", Parameter,
   param_type_followed_by_id_and_dimensions_opt4,
   TUPLE4 (STRING "trailing_assign1", EQUALS, TK_DecNumber num, EMPTY_TOKEN)) -> ()
| TUPLE4 (STRING "net_declaration1", Wire,
   TUPLE4 (STRING "net_decl_assign1", SymbolIdentifier id, EQUALS,
     unary_prefix_expr2),
    SEMICOLON) -> ()
| TUPLE4 (STRING "net_declaration1", Wire, net_variable1, SEMICOLON) -> ()
| TUPLE4 (STRING "non_anonymous_gate_instance_or_register_variable1",
	  SymbolIdentifier id, EMPTY_TOKEN, EMPTY_TOKEN) -> ()
| TUPLE4 (STRING "non_anonymous_gate_instance_or_register_variable1",
   SymbolIdentifier id, EMPTY_TOKEN,
   TUPLE3 (STRING "trailing_decl_assignment2", EQUALS,
     TUPLE3 (STRING "dec_based_number1", TK_DecBase "", TK_DecDigits ""))) -> ()
| TUPLE4 (STRING "non_anonymous_gate_instance_or_register_variable1",
   SymbolIdentifier id, EMPTY_TOKEN,
   TUPLE3 (STRING "trailing_decl_assignment2", EQUALS, TK_DecNumber num)) -> ()
| TUPLE4 (STRING "non_anonymous_gate_instance_or_register_variable1",
   SymbolIdentifier id, EMPTY_TOKEN,
   TUPLE3 (STRING "trailing_decl_assignment2", EQUALS, unary_prefix_expr2)) -> ()
| TUPLE4 (STRING "non_anonymous_gate_instance_or_register_variable1",
   SymbolIdentifier id,
   TUPLE6 (STRING "decl_variable_dimension1", LBRACK, TK_DecNumber hi, COLON,
     ELIST [], RBRACK),
    EMPTY_TOKEN) -> ()
| TUPLE4 (STRING "range_list_in_braces1", LBRACE, TLIST lst, RBRACE) -> List.iter verible_pat lst
| TUPLE4(STRING "select_variable_dimension2", LBRACK, TK_DecNumber lo, RBRACK) -> ()
| TUPLE4 (STRING "select_variable_dimension2", LBRACK, unqualified_id1, RBRACK) -> ()
| TUPLE4 (STRING "seq_block1", begin1, EMPTY_TOKEN, TUPLE3 (end1, End, EMPTY_TOKEN)) -> ()
| TUPLE4 (STRING "seq_block1", begin1, TLIST lst, TUPLE3 (end1, End, EMPTY_TOKEN)) -> List.iter verible_pat lst
| TUPLE3 (STRING "always_construct1", Always, procedural_timing_control_statement2) -> ()
| TUPLE3 (STRING "any_argument_list_item_last2",
   TUPLE3 (STRING "any_argument_list_trailing_comma1", TLIST lst, COMMA),
   TUPLE3 (STRING "reference2", unqualified_id1,
	    TUPLE3 (STRING "hierarchy_extension1", DOT, unqualified_id2))) -> List.iter verible_pat lst
| TUPLE3 (STRING "any_argument_list_item_last2",
   TUPLE3 (STRING "any_argument_list_trailing_comma1", TLIST lst, COMMA),
   cond_expr2) -> List.iter verible_pat lst
| TUPLE3 (STRING "any_port_list_item_last1",
   TUPLE3 (STRING "any_port_list_trailing_comma1", TLIST lst, COMMA),
   TUPLE6 (STRING "port_named1", DOT, SymbolIdentifier id, LPAREN,
     unqualified_id1, RPAREN)) -> List.iter verible_pat lst
| TUPLE3 (STRING "begin1", Begin, EMPTY_TOKEN) -> ()
| TUPLE3 (STRING "bin_based_number1", TK_BinBase base, TK_BinDigits digits) -> ()
| TUPLE3 (STRING "block_item_or_statement_or_null6", unqualified_id1, SEMICOLON) -> ()
| TUPLE3 (STRING "case_items1", case_item1, case_item2) -> ()
| TUPLE3 (STRING "data_declaration_or_module_instantiation1",
   instantiation_base1, SEMICOLON) -> ()
| TUPLE3 (STRING "data_type_primitive1",
   TUPLE3 (STRING "data_type_primitive_scalar1", Reg, EMPTY_TOKEN),
   EMPTY_TOKEN) -> ()
| TUPLE3 (STRING "data_type_primitive1",
   TUPLE3 (STRING "data_type_primitive_scalar1", Reg, EMPTY_TOKEN),
   TUPLE6 (STRING "decl_variable_dimension1", LBRACK, TK_DecNumber hi, COLON,
	    TK_DecNumber lo, RBRACK)) -> ()
| TUPLE3 (STRING "data_type_primitive1",
   TUPLE3 (STRING "data_type_primitive_scalar1", Reg, EMPTY_TOKEN),
   TUPLE6 (STRING "decl_variable_dimension1", LBRACK, ELIST [], COLON,
     TK_DecNumber lo, RBRACK)) -> ()
| TUPLE3 (STRING "end1", End, EMPTY_TOKEN) -> ()
| TUPLE3 (STRING "event_control4", AT, STAR) -> ()
| TUPLE3 (STRING "event_expression1", Posedge, unqualified_id1) -> ()
| TUPLE3 (STRING "event_expression1", Negedge, unqualified_id1) -> ()
| TUPLE3 (STRING "expression_or_dist1", cond_expr2, EMPTY_TOKEN) -> ()
| TUPLE3 (STRING "generate_if1", If, expression_in_parens1) -> ()
| TUPLE3 (STRING "initial_construct1", Initial, seq_block1) -> verible_pat seq_block1
| TUPLE3 (STRING "instantiation_base1",
   TUPLE3 (STRING "data_type_primitive1",
     TUPLE3 (STRING "data_type_primitive_scalar1", Reg, EMPTY_TOKEN),
     EMPTY_TOKEN),
   TLIST lst) -> List.iter verible_pat lst
| TUPLE3 (STRING "instantiation_base1",
   TUPLE3 (STRING "data_type_primitive1",
     TUPLE3 (STRING "data_type_primitive_scalar1", Reg, EMPTY_TOKEN),
     TUPLE6 (STRING "decl_variable_dimension1", LBRACK, TK_DecNumber hi, COLON,
       TK_DecNumber lo, RBRACK)),
    TLIST lst) -> List.iter verible_pat lst
| TUPLE3 (STRING "instantiation_base1",
   TUPLE3 (STRING "data_type_primitive1",
     TUPLE3 (STRING "data_type_primitive_scalar1", Reg, EMPTY_TOKEN),
     TUPLE6 (STRING "decl_variable_dimension1", LBRACK, ELIST [], COLON,
       TK_DecNumber lo, RBRACK)),
   TLIST lst) -> List.iter verible_pat lst
| TUPLE3 (STRING "instantiation_base1", unqualified_id1, TLIST lst) -> List.iter verible_pat lst
| TUPLE3 (STRING "ml_start1", TLIST lst, End_of_file) -> List.iter verible_pat lst
| TUPLE3 (STRING "module_parameter_port_list_item_last1",
   TUPLE3 (STRING "module_parameter_port_list_trailing_comma1", TLIST lst, COMMA),
   TUPLE4 (STRING "module_parameter_port1", Parameter,
     param_type_followed_by_id_and_dimensions_opt4,
     TUPLE4 (STRING "trailing_assign1", EQUALS,
       hex_based_number1,
       EMPTY_TOKEN))) -> List.iter verible_pat lst
| TUPLE3 (STRING "module_parameter_port_list_item_last1",
   TUPLE3 (STRING "module_parameter_port_list_trailing_comma1", TLIST lst, COMMA),
   TUPLE4 (STRING "module_parameter_port2", Parameter, Type, type_assignment1)) -> List.iter verible_pat lst
| TUPLE3 (STRING "net_variable1", SymbolIdentifier id, EMPTY_TOKEN) -> ()
| TUPLE3 (STRING "parameter_value_byname_list_item_last2",
   TUPLE3 (STRING "parameter_value_byname_list_trailing_comma1", TLIST lst, COMMA),
   TUPLE6 (STRING "parameter_value_byname1", DOT, SymbolIdentifier id, LPAREN,
	    TK_DecNumber num, RPAREN)) -> List.iter verible_pat lst
| TUPLE3 (STRING "parameter_value_byname_list_item_last2",
   TUPLE3 (STRING "parameter_value_byname_list_trailing_comma1", TLIST lst, COMMA),
   TUPLE6 (STRING "parameter_value_byname1", DOT, SymbolIdentifier id, LPAREN,
     unqualified_id1, RPAREN)) -> List.iter verible_pat lst
| TUPLE3 (STRING "port1",
   TUPLE3 (STRING "port_reference1", unqualified_id1, EMPTY_TOKEN),
   EMPTY_TOKEN) -> ()
| TUPLE3 (STRING "procedural_timing_control_statement2",
   TUPLE5 (STRING "event_control2", AT, LPAREN, TLIST lst, RPAREN), seq_block1) -> List.iter verible_pat lst
| TUPLE3 (STRING "procedural_timing_control_statement2",
   TUPLE3 (STRING "event_control4", AT, STAR), seq_block1) -> verible_pat seq_block1
| TUPLE3 (STRING "reference2",
   TUPLE3 (STRING "reference2", unqualified_id1,
     TUPLE3 (STRING "hierarchy_extension1", DOT, unqualified_id2)),
    TUPLE3 (STRING "hierarchy_extension1", DOT, unqualified_id3)) -> ()
| TUPLE3 (STRING "reference3",
   TUPLE3 (STRING "reference2",
     TUPLE3 (STRING "reference2", unqualified_id1,
       TUPLE3 (STRING "hierarchy_extension1", DOT, unqualified_id2)),
     TUPLE3 (STRING "hierarchy_extension1", DOT, unqualified_id3)),
   TUPLE6 (STRING "select_variable_dimension1", LBRACK, TK_DecNumber hi, COLON,
     TK_DecNumber lo, RBRACK)) -> ()
| TUPLE3 (STRING "reference3", reference3,
   TUPLE6 (STRING "select_variable_dimension1", LBRACK, TK_DecNumber hi, COLON,
	    TK_DecNumber lo, RBRACK)) -> ()
| TUPLE3 (STRING "reference3", unqualified_id1,
   TUPLE6 (STRING "select_variable_dimension3", LBRACK, unqualified_id2,
	    PLUS_COLON, unqualified_id3, RBRACK)) -> ()
| TUPLE3 (STRING "reference3", unqualified_id1,
   TUPLE4 (STRING "select_variable_dimension2", LBRACK, TK_DecNumber lo, RBRACK)) -> ()
| TUPLE3 (STRING "reference3", unqualified_id1,
   TUPLE4 (STRING "select_variable_dimension2", LBRACK, unqualified_id2,
	    RBRACK)) -> ()
| TUPLE3 (STRING "reference_or_call_base1", unqualified_id1,
	  TUPLE4 (STRING "call_base1", LPAREN, TLIST lst, RPAREN)) -> List.iter verible_pat lst
| TUPLE3 (STRING "sequence_repetition_expr1", expression_or_dist1,
   EMPTY_TOKEN) -> ()
| TUPLE3 (STRING "statement3", reference, SEMICOLON) -> ()
| TUPLE3 (STRING "statement_item6", assignment_statement_no_expr1,
   SEMICOLON) -> ()
| TUPLE3 (STRING "system_tf_call1", SystemTFIdentifier id,
	  TUPLE4 (STRING "call_base1", LPAREN, TLIST lst, RPAREN)) -> List.iter verible_pat lst
| TUPLE3 (STRING "unary_prefix_expr2", VBAR,
   TUPLE4 (STRING "range_list_in_braces1", LBRACE, TLIST lst, RBRACE)) -> List.iter verible_pat lst
| TUPLE3 (STRING "unary_prefix_expr2", VBAR, reference3) -> ()
| TUPLE3 (STRING "unary_prefix_expr2", TILDE, TK_DecNumber num) -> ()
| TUPLE3 (STRING "unary_prefix_expr2", TILDE, unqualified_id1) -> ()
| TUPLE3 (STRING "unary_prefix_expr2", PLING, reference3) -> ()
| TUPLE3 (STRING "unary_prefix_expr2", HYPHEN, unqualified_id1) -> ()
| TUPLE3 (STRING "unary_prefix_expr2", AMPERSAND, reference3) -> ()
| TUPLE3 (STRING "unqualified_id1", SymbolIdentifier id, EMPTY_TOKEN) -> ()
| TUPLE3 (STRING "unqualified_id1", SymbolIdentifier id,
   TUPLE5 (STRING "parameters2", HASH, LPAREN, TLIST lst, RPAREN)) -> List.iter verible_pat lst
| TUPLE12 (STRING "module_or_interface_declaration1", Module, EMPTY_TOKEN,
   SymbolIdentifier id, EMPTY_TOKEN, EMPTY_TOKEN,
   TUPLE4 (STRING "module_port_list_opt1", LPAREN, TLIST lst, RPAREN),
   EMPTY_TOKEN, SEMICOLON, EMPTY_TOKEN, Endmodule, EMPTY_TOKEN) -> List.iter verible_pat lst
| TUPLE12 (STRING "module_or_interface_declaration1", Module, EMPTY_TOKEN,
   SymbolIdentifier id, EMPTY_TOKEN, EMPTY_TOKEN,
   TUPLE4 (STRING "module_port_list_opt1", LPAREN, TLIST lst, RPAREN),
   EMPTY_TOKEN, SEMICOLON, TLIST lst', Endmodule, EMPTY_TOKEN) -> List.iter verible_pat lst; List.iter verible_pat lst'
| TUPLE12 (STRING "module_or_interface_declaration1", Module, EMPTY_TOKEN,
   SymbolIdentifier id, EMPTY_TOKEN,
   TUPLE5 (STRING "module_parameter_port_list_opt1", HASH, LPAREN, TLIST lst, RPAREN),
   TUPLE4 (STRING "module_port_list_opt1", LPAREN, TLIST lst', RPAREN),
   EMPTY_TOKEN, SEMICOLON, TLIST lst'', Endmodule, EMPTY_TOKEN) -> List.iter verible_pat lst; List.iter verible_pat lst; List.iter verible_pat lst''
| TUPLE10 (STRING "loop_statement1", For, LPAREN,
   TUPLE4 (STRING "for_init_decl_or_assign1", unqualified_id1, EQUALS, TK_DecNumber num),
   SEMICOLON, ELIST elst, SEMICOLON, assignment_statement_no_expr1,
   RPAREN, seq_block) -> List.iter verible_pat elst; verible_pat seq_block
| TUPLE6 (STRING "cast1", data_type_primitive1, QUOTE, LPAREN, reference3, RPAREN) -> ()
| TLIST lst -> List.iter verible_pat lst
| ELIST [] -> () 
| TK_StringLiteral "" -> ()
| TK_DecNumber num -> ()
| SymbolIdentifier id -> ()
| STRING _ | COMMA | LESS -> ()
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
   SymbolIdentifier _, EMPTY_TOKEN) -> ()
| TUPLE6
  (STRING "select_variable_dimension1", LBRACK, ELIST _, COLON,
   TK_DecNumber _, RBRACK) -> ()
| TUPLE6
  (STRING "simple_immediate_assertion_statement1", Assert, LPAREN,
   reference3, RPAREN,
   TUPLE3
    (STRING "action_block3", Else,
     TUPLE3
      (STRING "statement3", reference_or_call_base1, SEMICOLON))) -> ()
| TUPLE5
  (STRING "data_declaration_or_module_instantiation2", Automatic,
   EMPTY_TOKEN, instantiation_base1, SEMICOLON) -> ()
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
   SEMICOLON) -> ()
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
   SEMICOLON) -> ()
| TUPLE5
  (STRING "net_declaration4", Wire,
   TUPLE3 (STRING "delay31", HASH, TK_RealTime _),
   TUPLE4
    (STRING "net_decl_assign1", SymbolIdentifier _, EQUALS,
     cond_expr2),
   SEMICOLON) -> ()
| TUPLE5
  (STRING "port_declaration_noattr1", Output, Wire,
   TUPLE4
    (STRING "data_type_or_implicit_basic_followed_by_id_and_dimensions_opt4",
     TUPLE6
      (STRING "decl_variable_dimension1", LBRACK, TK_DecNumber _, COLON,
       TK_DecNumber _, RBRACK),
     unqualified_id1, EMPTY_TOKEN),
   EMPTY_TOKEN) -> ()
| TUPLE5
  (STRING "port_declaration_noattr1", Output, Wire,
   TUPLE3
    (STRING
      "type_identifier_or_implicit_basic_followed_by_id_and_dimensions_opt4",
     unqualified_id1, EMPTY_TOKEN),
   EMPTY_TOKEN) -> ()
| TUPLE5
  (STRING "port_declaration_noattr1", Output, EMPTY_TOKEN,
   TUPLE5
    (STRING
      "type_identifier_or_implicit_basic_followed_by_id_and_dimensions_opt2",
     unqualified_id1, EMPTY_TOKEN, unqualified_id2,
     EMPTY_TOKEN),
   EMPTY_TOKEN) -> ()
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
   EMPTY_TOKEN) -> ()
| TUPLE5
  (STRING "port_declaration_noattr1", Input, Wire,
   TUPLE4
    (STRING "data_type_or_implicit_basic_followed_by_id_and_dimensions_opt4",
     TUPLE6
      (STRING "decl_variable_dimension1", LBRACK, TK_DecNumber _, COLON,
       TK_DecNumber _, RBRACK),
     unqualified_id1, EMPTY_TOKEN),
   EMPTY_TOKEN) -> ()
| TUPLE5
  (STRING "port_declaration_noattr1", Input, Wire,
   TUPLE3
    (STRING
      "type_identifier_or_implicit_basic_followed_by_id_and_dimensions_opt4",
     unqualified_id1, EMPTY_TOKEN),
   EMPTY_TOKEN) -> ()
| TUPLE5
  (STRING "port_declaration_noattr1", Input, EMPTY_TOKEN,
   TUPLE5
    (STRING
      "type_identifier_or_implicit_basic_followed_by_id_and_dimensions_opt2",
     unqualified_id1, EMPTY_TOKEN, unqualified_id2,
     EMPTY_TOKEN),
   EMPTY_TOKEN) -> ()
| TUPLE5
  (STRING "port_declaration_noattr1", Input, EMPTY_TOKEN,
   TUPLE4
    (STRING "data_type_or_implicit_basic_followed_by_id_and_dimensions_opt1",
     TUPLE3
      (STRING "data_type_primitive1",
       TUPLE3 (STRING "data_type_primitive_scalar1", Logic, EMPTY_TOKEN),
       EMPTY_TOKEN),
     unqualified_id1, EMPTY_TOKEN),
   EMPTY_TOKEN) -> ()
| TUPLE5 (STRING "port_declaration_noattr1", Input, EMPTY_TOKEN,
   TUPLE4 (STRING "data_type_or_implicit_basic_followed_by_id_and_dimensions_opt1",
     TUPLE3 (STRING "data_type_primitive1",
       TUPLE3 (STRING "data_type_primitive_scalar1", Logic, EMPTY_TOKEN),
       TUPLE6 (STRING "decl_variable_dimension1", LBRACK, TK_DecNumber _, COLON,
         TK_DecNumber _, RBRACK)),
     unqualified_id1, EMPTY_TOKEN),
   EMPTY_TOKEN) -> ()
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
   EMPTY_TOKEN) -> ()
| TUPLE5
  (STRING "port_declaration_noattr1", Inout, Wire,
   TUPLE3
    (STRING
      "type_identifier_or_implicit_basic_followed_by_id_and_dimensions_opt4",
     unqualified_id1, EMPTY_TOKEN),
   EMPTY_TOKEN) -> ()
| TUPLE5
  (STRING "port_declaration_noattr1", Inout, EMPTY_TOKEN,
   TUPLE3
    (STRING
      "type_identifier_or_implicit_basic_followed_by_id_and_dimensions_opt4",
     unqualified_id1, EMPTY_TOKEN),
   EMPTY_TOKEN) -> ()
| TUPLE4
  (STRING "expression_list_proper1",
   TUPLE3 (STRING "hex_based_number1", TK_HexBase _, TK_HexDigits _),
   COMMA,
   TUPLE3 (STRING "hex_based_number1", TK_HexBase _, TK_HexDigits _)) -> ()
| TUPLE4
  (STRING "expression_list_proper1", expression_list_proper1, COMMA,
   TUPLE3 (STRING "hex_based_number1", TK_HexBase _, TK_HexDigits _)) -> ()
| TUPLE4
  (STRING "generate_block1", begin1, EMPTY_TOKEN,
   TUPLE3 (STRING "end1", End, EMPTY_TOKEN)) -> ()
| TUPLE4
  (STRING "jump_statement4", Return, unqualified_id1, SEMICOLON) -> ()
| TUPLE4
  (STRING "module_parameter_port1", Parameter,
   TUPLE6
    (STRING "param_type_followed_by_id_and_dimensions_opt5", Int,
     EMPTY_TOKEN, EMPTY_TOKEN, SymbolIdentifier _, EMPTY_TOKEN),
   TUPLE4
    (STRING "trailing_assign1", EQUALS, unary_prefix_expr2,
     EMPTY_TOKEN)) -> ()
| TUPLE4
  (STRING "module_parameter_port1", Parameter,
   param_type_followed_by_id_and_dimensions_opt4,
   TUPLE4
    (STRING "trailing_assign1", EQUALS, bin_based_number1,
     EMPTY_TOKEN)) -> ()
| TUPLE4
  (STRING "module_parameter_port2", Parameter, Type,
   TUPLE4
    (STRING "type_assignment1", SymbolIdentifier _, EQUALS,
     TUPLE3
      (STRING "data_type_primitive1",
       TUPLE3 (STRING "data_type_primitive_scalar1", Logic, EMPTY_TOKEN),
       EMPTY_TOKEN))) -> ()
| TUPLE4
  (STRING "port_declaration_noattr4",
   TUPLE6
    (STRING "type_identifier_followed_by_id3", unqualified_id1, DOT,
     SymbolIdentifier _, EMPTY_TOKEN, SymbolIdentifier _),
   EMPTY_TOKEN, EMPTY_TOKEN) -> ()
| TUPLE4
  (STRING "qualified_id2", unqualified_id1, COLON_COLON,
   unqualified_id2) -> ()
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
   EMPTY_TOKEN) -> ()
| TUPLE4
  (STRING "tf_port_item1", EMPTY_TOKEN,
   TUPLE4
    (STRING "data_type_or_implicit_basic_followed_by_id_and_dimensions_opt1",
     TUPLE3
      (STRING "data_type_primitive1",
       TUPLE3 (STRING "data_type_primitive_scalar5", Byte, EMPTY_TOKEN),
       EMPTY_TOKEN),
     unqualified_id1, EMPTY_TOKEN),
   EMPTY_TOKEN) -> ()
| TUPLE3 (STRING "always_construct1", Always_latch, seq_block1) -> ()
| TUPLE3
  (STRING "always_construct1", Always_ff,
   procedural_timing_control_statement2) -> ()
| TUPLE3 (STRING "always_construct1", Always_comb, seq_block1) -> ()
| TUPLE3
  (STRING "any_port_list_item_last1",
   TUPLE3 (STRING "any_port_list_trailing_comma1", TLIST _, COMMA),
   TUPLE5 (STRING "port_named2", DOT, SymbolIdentifier _, LPAREN, RPAREN)) -> ()
| TUPLE3
  (STRING "any_port_list_item_last1",
   TUPLE3 (STRING "any_port_list_trailing_comma1", TLIST _, COMMA),
   TUPLE3 (STRING "port_named3", DOT, SymbolIdentifier _)) -> ()
| TUPLE3
  (STRING "begin1", Begin,
   TUPLE3 (STRING "label_opt1", COLON, SymbolIdentifier _)) -> ()
| TUPLE3
  (STRING "conditional_generate_construct2",
   TUPLE3 (STRING "generate_if1", If, expression_in_parens1),
   generate_block1) -> ()
| TUPLE3
  (STRING "data_type_primitive1",
   TUPLE3 (STRING "data_type_primitive_scalar1", Logic, EMPTY_TOKEN),
   EMPTY_TOKEN) -> ()
| TUPLE3
  (STRING "data_type_primitive1",
   TUPLE3 (STRING "data_type_primitive_scalar1", Logic, EMPTY_TOKEN),
   TUPLE6
    (STRING "decl_variable_dimension1", LBRACK, TK_DecNumber _, COLON,
     TK_DecNumber _, RBRACK)) -> ()
| TUPLE3
  (STRING "data_type_primitive1",
   TUPLE3 (STRING "data_type_primitive_scalar1", Logic, EMPTY_TOKEN),
   TUPLE6
    (STRING "decl_variable_dimension1", LBRACK, unqualified_id1,
     COLON, TK_DecNumber _, RBRACK)) -> ()
| TUPLE3
  (STRING "data_type_primitive1",
   TUPLE3 (STRING "data_type_primitive_scalar1", Bit, EMPTY_TOKEN),
   EMPTY_TOKEN) -> ()
| TUPLE3
  (STRING "data_type_primitive1",
   TUPLE3 (STRING "data_type_primitive_scalar5", Int, EMPTY_TOKEN),
   EMPTY_TOKEN) -> ()
| TUPLE3
  (STRING "data_type_primitive1",
   TUPLE3 (STRING "data_type_primitive_scalar5", Byte, EMPTY_TOKEN),
   EMPTY_TOKEN) -> ()
| TUPLE3 (STRING "dec_based_number1", TK_DecBase _, TK_DecDigits _) -> ()
| TUPLE3
  (STRING "decl_dimensions2",
   TUPLE6
    (STRING "decl_variable_dimension1", LBRACK, TK_DecNumber _, COLON,
     TK_DecNumber _, RBRACK),
   TUPLE6
    (STRING "decl_variable_dimension1", LBRACK, TK_DecNumber _, COLON,
     TK_DecNumber _, RBRACK)) -> ()
| TUPLE3 (STRING "hex_based_number1", TK_HexBase _, TK_HexDigits _) -> ()
| TUPLE3
  (STRING "inc_or_dec_expression2", unqualified_id1, PLUS_PLUS) -> ()
| TUPLE3 (STRING "label_opt1", COLON, SymbolIdentifier _) -> ()
| TUPLE3 (STRING "port_named3", DOT, SymbolIdentifier _) -> ()
| TUPLE3
  (STRING "reference2", unqualified_id1,
   TUPLE3 (STRING "hierarchy_extension1", DOT, unqualified_id2)) -> ()
| TUPLE3
  (STRING "reference3", unqualified_id1,
   TUPLE6
    (STRING "select_variable_dimension1", LBRACK, ELIST _, COLON,
     TK_DecNumber _, RBRACK)) -> ()
| TUPLE12
  (STRING "module_or_interface_declaration1", Module, EMPTY_TOKEN,
   SymbolIdentifier _, EMPTY_TOKEN,
   TUPLE5
    (STRING "module_parameter_port_list_opt1", HASH, LPAREN, TLIST _,
     RPAREN),
   TUPLE4 (STRING "module_port_list_opt1", LPAREN, TLIST _, RPAREN),
   EMPTY_TOKEN, SEMICOLON, TLIST _, Endmodule,
   TUPLE3 (STRING "label_opt1", COLON, SymbolIdentifier _)) -> ()
| TUPLE12
  (STRING "module_or_interface_declaration1", Interface, EMPTY_TOKEN,
   SymbolIdentifier _, EMPTY_TOKEN,
   TUPLE5
    (STRING "module_parameter_port_list_opt1", HASH, LPAREN, TLIST _,
     RPAREN),
   TUPLE4 (STRING "module_port_list_opt1", LPAREN, TLIST _, RPAREN),
   EMPTY_TOKEN, SEMICOLON, EMPTY_TOKEN, Endinterface, EMPTY_TOKEN) -> ()
| TUPLE11
  (STRING "function_declaration1", Function, EMPTY_TOKEN,
   TUPLE4
    (STRING "data_type_or_implicit_basic_followed_by_id_and_dimensions_opt5",
     Void, unqualified_id1, EMPTY_TOKEN),
   LPAREN, TLIST _, RPAREN, SEMICOLON, TLIST _, Endfunction,
   TUPLE3 (STRING "label_opt1", COLON, SymbolIdentifier _)) -> ()
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
   TUPLE3 (STRING "label_opt1", COLON, SymbolIdentifier _)) -> ()
| TUPLE10 (STRING "loop_statement1", For, LPAREN,
   TUPLE5 (STRING "for_init_decl_or_assign2",
     TUPLE3 (STRING "data_type_primitive1",
       TUPLE3 (STRING "data_type_primitive_scalar5", Int, Unsigned),
       EMPTY_TOKEN),
     SymbolIdentifier _, EQUALS, TK_DecNumber _),
   SEMICOLON, ELIST _, SEMICOLON, inc_or_dec_expression2, RPAREN,
   STRING "seq_block1") -> () 
| TUPLE4 (STRING "assignment_pattern1", QUOTE_LBRACE, TLIST lst, RBRACE) -> List.iter verible_pat lst
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
   Endcase) -> ()
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
   Endcase) -> ()
| TUPLE8
  (STRING "case_statement1", Unique, Case, LPAREN,
   TUPLE3
    (STRING "reference2", STRING "unqualified_id1",
     TUPLE3 (STRING "hierarchy_extension1", DOT, STRING "unqualified_id1")),
   RPAREN, STRING "case_items1", Endcase) -> ()
| TUPLE8
  (STRING "case_statement1", Unique, Case, LPAREN, STRING "unqualified_id1",
   RPAREN, STRING "case_items1", Endcase) -> ()
| oth -> othpatlst := oth :: !othpatlst; print_endline "verible_pat"

and verible_pat itm = patlst := itm :: !patlst; verible_pat' itm; patlst := List.tl !patlst
