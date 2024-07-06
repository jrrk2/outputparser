     open Source_text_verible

     let othpat' = ref End_of_file
     let othpatlst = ref []

let rec pat' = function
| TK_StringLiteral s -> `string_literal s
| TLIST lst -> `tlist (List.map pat lst)
| TUPLE9 (STRING "task_declaration1", Task, EMPTY_TOKEN, SymbolIdentifier id,
   EMPTY_TOKEN, SEMICOLON, TLIST lst, Endtask, EMPTY_TOKEN) -> (`task_declaration1 (patlst lst))
| TUPLE8 (STRING "case_statement1", EMPTY_TOKEN, Case, LPAREN,
       expr,
   RPAREN, case_items1, Endcase) -> `case_statement1
| TUPLE7 (STRING "conditional_statement2", EMPTY_TOKEN, If,
       expression_in_parens1,
       then_statement,
   Else, else_statement) -> `conditional_statement2 (pat expression_in_parens1, pat then_statement, pat else_statement)
| TUPLE7 (STRING "expr_primary_braces2", LBRACE, braced, LBRACE,
  reference3, RBRACE, RBRACE) -> `expr_primary_braces2
| TUPLE6 (STRING "cond_expr2", expr_primary, QUERY, expr_true, COLON, expr_false) -> `cond_expr2
| TUPLE6 (STRING "continuous_assign1", Assign, EMPTY_TOKEN, dly, TLIST lst,
  SEMICOLON) -> (`continuous_assign1 (patlst lst))
| TUPLE6 (STRING "decl_variable_dimension1", LBRACK, TK_DecNumber hi, COLON, TK_DecNumber lo, RBRACK) -> `decl_variable_dimension1 (hi,lo)
    | TUPLE6 (STRING "non_anonymous_gate_instance_or_register_variable2",
  SymbolIdentifier id, EMPTY_TOKEN, LPAREN, TLIST lst, RPAREN) -> (`non_anonymous_gate_instance_or_register_variable2 (patlst lst))
| TUPLE6 (STRING "nonblocking_assignment1", lhs, LT_EQ, EMPTY_TOKEN, rhs, SEMICOLON) -> (`nonblocking_assignment1 (pat lhs, pat rhs))
| TUPLE6 (STRING "param_type_followed_by_id_and_dimensions_opt4", (Logic|EMPTY_TOKEN),
       EMPTY_TOKEN, TUPLE6 (STRING "decl_variable_dimension1", LBRACK, TK_DecNumber hi, COLON,
         TK_DecNumber lo, RBRACK),
   SymbolIdentifier id, EMPTY_TOKEN) -> `param_type_followed_by_id_and_dimensions_opt4
| TUPLE6 (STRING "parameter_value_byname1", DOT, SymbolIdentifier id, LPAREN,
  TK_DecNumber num, RPAREN) -> `parameter_value_byname1
| TUPLE6 (STRING "parameter_value_byname1", DOT, SymbolIdentifier id, LPAREN,
   unqualified_id1, RPAREN) -> `parameter_value_byname1
| TUPLE6 (STRING "port_named1", DOT, SymbolIdentifier id, LPAREN,
  unqualified_id1, RPAREN) -> `port_named1
| TUPLE6 (STRING "select_variable_dimension1", LBRACK, TK_DecNumber hi, COLON,
   TK_DecNumber lo, RBRACK) -> `select_variable_dimension1
| TUPLE6 (STRING "select_variable_dimension3", LBRACK, unqualified_id1,
  PLUS_COLON, unqualified_id2, RBRACK) -> `select_variable_dimension3
| TUPLE7 (STRING "param_type_followed_by_id_and_dimensions_opt2", EMPTY_TOKEN,
         EMPTY_TOKEN, unqualified_id1, EMPTY_TOKEN, SymbolIdentifier id,
         EMPTY_TOKEN) -> `param_type_followed_by_id_and_dimensions_opt2 (pat unqualified_id1, id)
| TUPLE5 (STRING "any_param_declaration4", Localparam,
       param_type_followed_by_id_and_dimensions_opt2,
       TUPLE4 (STRING "trailing_assign1", EQUALS, expr', EMPTY_TOKEN),
   SEMICOLON) -> `any_param_declaration4 (pat param_type_followed_by_id_and_dimensions_opt2, pat expr')
| TUPLE5 (STRING "param_type_followed_by_id_and_dimensions_opt3", EMPTY_TOKEN,
         EMPTY_TOKEN, unqualified_id1, EMPTY_TOKEN)  -> (`param_type_followed_by_id_and_dimensions_opt3 (pat unqualified_id1))
| TUPLE5 (STRING "conditional_generate_construct1",
       TUPLE3 (STRING "generate_if1", If, expression_in_parens1),
   generate_block1, Else, conditional_generate_construct1) -> `conditional_generate_construct1 (pat expression_in_parens1, pat generate_block1, pat conditional_generate_construct1)
| TUPLE5 (STRING "conditional_statement1", EMPTY_TOKEN, If,
   expression_in_parens1, seq_block1) -> (`conditional_statement1 (pat expression_in_parens1, pat seq_block1))
| TUPLE5 (STRING "event_control2", AT, LPAREN, TLIST lst, RPAREN) -> (`event_control2 (patlst lst))
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
   SEMICOLON) -> `net_declaration2
| TUPLE5 (STRING "net_declaration2", Wire,
       TUPLE3
       (STRING "data_type_or_implicit1",
         TUPLE6
         (STRING "decl_variable_dimension1", LBRACK, TK_DecNumber hi, COLON,
           TK_DecNumber lo, RBRACK),
         EMPTY_TOKEN),
    net_variable1, SEMICOLON) -> `net_declaration2
| TUPLE5 (STRING "port_declaration_noattr1", Output, EMPTY_TOKEN, typ, EMPTY_TOKEN) -> `port_declaration_noattr1 (pat typ)
| TUPLE5 (STRING "port_declaration_noattr1", Output, EMPTY_TOKEN, typ, EMPTY_TOKEN) -> `port_declaration_noattr1 (pat typ)
(*
| TUPLE4 (STRING "data_type_or_implicit_basic_followed_by_id_and_dimensions_opt4",
        TUPLE6 (STRING "decl_variable_dimension1", LBRACK, TK_DecNumber hi, COLON,
          TK_DecNumber lo, RBRACK),
        unqualified_id1, EMPTY_TOKEN) -> `data_type_or_implicit_basic_followed_by_id_and_dimensions_opt4
*)
| TUPLE5 (STRING "port_declaration_noattr1", Output, EMPTY_TOKEN, typ, EMPTY_TOKEN) -> `port_declaration_noattr1 (pat typ)
| TUPLE5 (STRING "port_declaration_noattr1", Input, EMPTY_TOKEN, typ, EMPTY_TOKEN) -> `port_declaration_noattr1 (pat typ)
| TUPLE5 (STRING "port_declaration_noattr1", Input, EMPTY_TOKEN, typ, EMPTY_TOKEN) -> `port_declaration_noattr1 (pat typ)
| TUPLE4 (STRING "assignment_statement_no_expr1", range_list_in_braces1, EQUALS, expr') ->
    `assignment_statement_no_expr1 (pat range_list_in_braces1, pat expr')
| TUPLE4 (STRING "call_base1", LPAREN, TLIST lst, RPAREN) -> (`call_base1 (patlst lst))
| TUPLE4 (STRING "case_item1", sel, COLON, seq_block) -> `case_item1 (pat seq_block)
| TUPLE4 (STRING "case_item2", Default, COLON, SEMICOLON) -> `case_item2 `empty
| TUPLE4 (STRING "case_item2", Default, COLON, seq_block) -> `case_item2 (pat seq_block)
| TUPLE4 (STRING "cont_assign1", unqualified_id1, EQUALS, expr) -> `cont_assign1
| TUPLE4 (STRING "expr_primary_parens1", LPAREN, TLIST lst, RPAREN) -> (`expr_primary_parens1 (patlst lst))
| TUPLE4 (STRING "expression_in_parens1", LPAREN, cond_expr2, RPAREN) -> (`expression_in_parens1 (pat cond_expr2))
| TUPLE3 (STRING "system_tf_call1", SystemTFIdentifier id,
        TUPLE4 (STRING "call_base1", LPAREN, TLIST lst, RPAREN)) -> (`system_tf_call1 (patlst lst))
| TUPLE4 (STRING "expression_list_proper1", unqualified_id1, COMMA, expr') -> `expression_list_proper1
| TUPLE4 (STRING "gate_instance_or_register_variable1", SymbolIdentifier id,
   EMPTY_TOKEN, EMPTY_TOKEN) -> `gate_instance_or_register_variable1
| TUPLE4 (STRING "generate_block1", begin1,
  TLIST lst, TUPLE3 (STRING "end1", End, EMPTY_TOKEN)) -> (`generate_block1 (pat begin1, (patlst lst)))
| TUPLE4 (STRING "generate_region1", Generate, TLIST lst, Endgenerate) -> (`generate_region1 (patlst lst))
| TUPLE4 (STRING "module_parameter_port1", Parameter,
      TUPLE5 (STRING "param_type_followed_by_id_and_dimensions_opt3", EMPTY_TOKEN,
        EMPTY_TOKEN, unqualified_id1, EMPTY_TOKEN),
    TUPLE4 (STRING "trailing_assign1", EQUALS, TK_DecNumber num, EMPTY_TOKEN)) -> `module_parameter_port1
| TUPLE4 (STRING "module_parameter_port1", Parameter,
      param_type_followed_by_id_and_dimensions_opt4,
   TUPLE4 (STRING "trailing_assign1", EQUALS, TK_DecNumber num, EMPTY_TOKEN)) -> `module_parameter_port1
| TUPLE4 (STRING "net_declaration1", Wire,
      TUPLE4 (STRING "net_decl_assign1", SymbolIdentifier id, EQUALS,
        unary_prefix_expr2),
    SEMICOLON) -> `net_declaration1
| TUPLE4 (STRING "net_declaration1", Wire, net_variable1, SEMICOLON) -> `net_declaration1
   | TUPLE4 (STRING "non_anonymous_gate_instance_or_register_variable1",
  SymbolIdentifier id, EMPTY_TOKEN, EMPTY_TOKEN) -> `non_anonymous_gate_instance_or_register_variable1
   | TUPLE4 (STRING "non_anonymous_gate_instance_or_register_variable1",
      SymbolIdentifier id, EMPTY_TOKEN,
      TUPLE3 (STRING "trailing_decl_assignment2", EQUALS,
     TUPLE3 (STRING "dec_based_number1", TK_DecBase "", TK_DecDigits ""))) -> `non_anonymous_gate_instance_or_register_variable1
   | TUPLE4 (STRING "non_anonymous_gate_instance_or_register_variable1",
      SymbolIdentifier id, EMPTY_TOKEN,
   TUPLE3 (STRING "trailing_decl_assignment2", EQUALS, TK_DecNumber num)) -> `non_anonymous_gate_instance_or_register_variable1
   | TUPLE4 (STRING "non_anonymous_gate_instance_or_register_variable1",
      SymbolIdentifier id, EMPTY_TOKEN,
   TUPLE3 (STRING "trailing_decl_assignment2", EQUALS, unary_prefix_expr2)) -> `non_anonymous_gate_instance_or_register_variable1
   | TUPLE4 (STRING "non_anonymous_gate_instance_or_register_variable1",
      SymbolIdentifier id,
      TUPLE6 (STRING "decl_variable_dimension1", LBRACK, TK_DecNumber hi, COLON,
        expr', RBRACK),
    EMPTY_TOKEN) -> `non_anonymous_gate_instance_or_register_variable1
| TUPLE4 (STRING "range_list_in_braces1", LBRACE, TLIST lst, RBRACE) -> (`range_list_in_braces1 (patlst lst))
| TUPLE4(STRING "select_variable_dimension2", LBRACK, TK_DecNumber lo, RBRACK) -> `select_variable_dimension2
| TUPLE4 (STRING "select_variable_dimension2", LBRACK, unqualified_id1, RBRACK) -> `select_variable_dimension2
| TUPLE4 (STRING "seq_block1", begin1, EMPTY_TOKEN, TUPLE3 (end1, End, EMPTY_TOKEN)) -> (`seq_block1 (`tlist []))
| TUPLE4 (STRING "seq_block1", begin1, TLIST lst, TUPLE3 (end1, End, EMPTY_TOKEN)) -> (`seq_block1 (patlst lst))
| TUPLE3 (STRING "always_construct1", Always, procedural_timing_control_statement2) -> (`always_construct1 (pat procedural_timing_control_statement2))
| TUPLE3 (STRING "any_argument_list_item_last2",
      TUPLE3 (STRING "any_argument_list_trailing_comma1", TLIST lst, COMMA),
      TUPLE3 (STRING "reference2", unqualified_id1,
    TUPLE3 (STRING "hierarchy_extension1", DOT, unqualified_id2))) -> (`any_argument_list_item_last2 (patlst lst))
| TUPLE3 (STRING "any_argument_list_item_last2",
      TUPLE3 (STRING "any_argument_list_trailing_comma1", TLIST lst, COMMA),
   cond_expr2) -> (`any_argument_list_item_last2 (patlst lst))
| TUPLE3 (STRING "any_port_list_item_last1",
      TUPLE3 (STRING "any_port_list_trailing_comma1", TLIST lst, COMMA),
      TUPLE6 (STRING "port_named1", DOT, SymbolIdentifier id, LPAREN,
     unqualified_id1, RPAREN)) -> (`any_port_list_item_last1 (patlst lst))
| TUPLE3 (STRING "begin1", Begin, EMPTY_TOKEN) -> `begin1
| TUPLE3 (STRING "bin_based_number1", TK_BinBase base, TK_BinDigits digits) -> `bin_based_number1
| TUPLE3 (STRING "block_item_or_statement_or_null6", unqualified_id1, SEMICOLON) -> `block_item_or_statement_or_null6
| TUPLE3 (STRING "case_items1", case_item1, case_item2) -> (`case_items1)
| TUPLE3 (STRING "data_declaration_or_module_instantiation1",
   instantiation_base1, SEMICOLON) -> `data_declaration_or_module_instantiation1
| TUPLE3 (STRING "data_type_primitive1", typ, EMPTY_TOKEN) -> `data_type_primitive1
| TUPLE3 (STRING "data_type_primitive1",
      TUPLE3 (STRING "data_type_primitive_scalar1", Reg, EMPTY_TOKEN),
      TUPLE6 (STRING "decl_variable_dimension1", LBRACK, TK_DecNumber hi, COLON,
    TK_DecNumber lo, RBRACK)) -> `data_type_primitive1
| TUPLE3 (STRING "data_type_primitive1",
      TUPLE3 (STRING "data_type_primitive_scalar1", Reg, EMPTY_TOKEN),
      TUPLE6 (STRING "decl_variable_dimension1", LBRACK, expr', COLON,
     TK_DecNumber lo, RBRACK)) -> `data_type_primitive1
| TUPLE3 (STRING "end1", End, EMPTY_TOKEN) -> `end1
| TUPLE3 (STRING "event_control4", AT, STAR) -> `event_control4
| TUPLE3 (STRING "event_expression1", Posedge, unqualified_id1) -> `event_expression1
| TUPLE3 (STRING "event_expression1", Negedge, unqualified_id1) -> `event_expression1
| TUPLE3 (STRING "expression_or_dist1", cond_expr2, EMPTY_TOKEN) -> `expression_or_dist1
| TUPLE3 (STRING "generate_if1", If, expression_in_parens1) -> `generate_if1
| TUPLE3 (STRING "initial_construct1", Initial, seq_block1) -> `initial_construct1 (pat seq_block1)
   | TUPLE3 (STRING "instantiation_base1",
      TUPLE3 (STRING "data_type_primitive1",
        TUPLE3 (STRING "data_type_primitive_scalar1", Reg, EMPTY_TOKEN),
        EMPTY_TOKEN),
   TLIST lst) -> (`instantiation_base1 (patlst lst))
   | TUPLE3 (STRING "instantiation_base1",
      TUPLE3 (STRING "data_type_primitive1",
        TUPLE3 (STRING "data_type_primitive_scalar1", Reg, EMPTY_TOKEN),
        TUPLE6 (STRING "decl_variable_dimension1", LBRACK, TK_DecNumber hi, COLON,
          TK_DecNumber lo, RBRACK)),
    TLIST lst) -> (`instantiation_base1 (patlst lst))
   | TUPLE3 (STRING "instantiation_base1",
      TUPLE3 (STRING "data_type_primitive1",
        TUPLE3 (STRING "data_type_primitive_scalar1", Reg, EMPTY_TOKEN),
        TUPLE6 (STRING "decl_variable_dimension1", LBRACK, expr', COLON,
          TK_DecNumber lo, RBRACK)),
   TLIST lst) -> (`instantiation_base1 (patlst lst))
| TUPLE3 (STRING "instantiation_base1", unqualified_id1, TLIST lst) -> (`instantiation_base1 (patlst lst))
| TUPLE3 (STRING "ml_start1", TLIST lst, End_of_file) -> (`ml_start1 (patlst lst))
   | TUPLE3 (STRING "module_parameter_port_list_item_last1",
      TUPLE3 (STRING "module_parameter_port_list_trailing_comma1", TLIST lst, COMMA),
      TUPLE4 (STRING "module_parameter_port1", Parameter,
        param_type_followed_by_id_and_dimensions_opt4,
        TUPLE4 (STRING "trailing_assign1", EQUALS,
          hex_based_number1,
       EMPTY_TOKEN))) -> (`module_parameter_port_list_item_last1 (patlst lst))
   | TUPLE3 (STRING "module_parameter_port_list_item_last1",
      TUPLE3 (STRING "module_parameter_port_list_trailing_comma1", TLIST lst, COMMA),
   TUPLE4 (STRING "module_parameter_port2", Parameter, Type, type_assignment1)) -> (`module_parameter_port_list_item_last1 (patlst lst))
| TUPLE3 (STRING "net_variable1", SymbolIdentifier id, EMPTY_TOKEN) -> `net_variable1
| TUPLE3 (STRING "parameter_value_byname_list_item_last2",
      TUPLE3 (STRING "parameter_value_byname_list_trailing_comma1", TLIST lst, COMMA),
      TUPLE6 (STRING "parameter_value_byname1", DOT, SymbolIdentifier id, LPAREN,
    TK_DecNumber num, RPAREN)) -> (`parameter_value_byname_list_item_last2 (patlst lst))
| TUPLE3 (STRING "parameter_value_byname_list_item_last2",
      TUPLE3 (STRING "parameter_value_byname_list_trailing_comma1", TLIST lst, COMMA),
      TUPLE6 (STRING "parameter_value_byname1", DOT, SymbolIdentifier id, LPAREN,
     unqualified_id1, RPAREN)) -> (`parameter_value_byname_list_item_last2 (patlst lst))
| TUPLE3 (STRING "port1",
      TUPLE3 (STRING "port_reference1", unqualified_id1, EMPTY_TOKEN),
   EMPTY_TOKEN) -> `port1
| TUPLE3 (STRING "procedural_timing_control_statement2", stmt, seq_block) -> (`procedural_timing_control_statement2 (pat stmt, pat seq_block))
| TUPLE3 (STRING "reference2",
      TUPLE3 (STRING "reference2", unqualified_id1,
        TUPLE3 (STRING "hierarchy_extension1", DOT, unqualified_id2)),
    TUPLE3 (STRING "hierarchy_extension1", DOT, unqualified_id3)) -> `reference2
| TUPLE3 (STRING "reference3",
      TUPLE3 (STRING "reference2",
        TUPLE3 (STRING "reference2", unqualified_id1,
          TUPLE3 (STRING "hierarchy_extension1", DOT, unqualified_id2)),
        TUPLE3 (STRING "hierarchy_extension1", DOT, unqualified_id3)),
      TUPLE6 (STRING "select_variable_dimension1", LBRACK, TK_DecNumber hi, COLON,
     TK_DecNumber lo, RBRACK)) -> `reference3
| TUPLE3 (STRING "reference3", reference3,
      TUPLE6 (STRING "select_variable_dimension1", LBRACK, TK_DecNumber hi, COLON,
    TK_DecNumber lo, RBRACK)) -> `reference3
| TUPLE3 (STRING "reference3", unqualified_id1,
      TUPLE6 (STRING "select_variable_dimension3", LBRACK, unqualified_id2,
    PLUS_COLON, unqualified_id3, RBRACK)) -> `reference3
| TUPLE3 (STRING "reference3", unqualified_id1,
   TUPLE4 (STRING "select_variable_dimension2", LBRACK, TK_DecNumber lo, RBRACK)) -> `reference3
| TUPLE3 (STRING "reference3", unqualified_id1,
      TUPLE4 (STRING "select_variable_dimension2", LBRACK, unqualified_id2,
    RBRACK)) -> `reference3
| TUPLE3 (STRING "reference_or_call_base1", unqualified_id1,
  TUPLE4 (STRING "call_base1", LPAREN, TLIST lst, RPAREN)) -> (`reference_or_call_base1 (patlst lst))
| TUPLE3 (STRING "sequence_repetition_expr1", expression_or_dist1,
   EMPTY_TOKEN) -> `sequence_repetition_expr1
| TUPLE3 (STRING "statement3", reference, SEMICOLON) -> `statement3 (pat reference)
| TUPLE3 (STRING "statement_item6", assignment_statement_no_expr1, SEMICOLON) -> (`statement_item6 (pat assignment_statement_no_expr1))
| TUPLE3 (STRING "unary_prefix_expr2", VBAR, reference) -> (`unary_prefix_expr2 (pat reference))
| TUPLE3 (STRING "unary_prefix_expr2", TILDE, reference) -> (`unary_prefix_expr2 (pat reference))
| TUPLE3 (STRING "unary_prefix_expr2", PLING, reference) -> (`unary_prefix_expr2 (pat reference))
| TUPLE3 (STRING "unary_prefix_expr2", HYPHEN, reference) -> (`unary_prefix_expr2 (pat reference))
| TUPLE3 (STRING "unary_prefix_expr2", AMPERSAND, reference) -> (`unary_prefix_expr2 (pat reference))
| TUPLE3 (STRING "unqualified_id1", SymbolIdentifier id, param) -> (`unqualified_id1 (id, pat param))
| TUPLE5 (STRING "parameters2", HASH, LPAREN, TLIST lst, RPAREN) -> (`parameters2 (patlst lst))
| TUPLE12 (STRING "module_or_interface_declaration1", Module, EMPTY_TOKEN,
      SymbolIdentifier id, EMPTY_TOKEN, EMPTY_TOKEN,
      TUPLE4 (STRING "module_port_list_opt1", LPAREN, TLIST lst, RPAREN),
   EMPTY_TOKEN, SEMICOLON, EMPTY_TOKEN, Endmodule, EMPTY_TOKEN) -> (`module_or_interface_declaration1 (id, patlst lst, `tlist []))
| TUPLE12 (STRING "module_or_interface_declaration1", Module, EMPTY_TOKEN,
      SymbolIdentifier id, EMPTY_TOKEN, EMPTY_TOKEN,
      TUPLE4 (STRING "module_port_list_opt1", LPAREN, TLIST lst, RPAREN),
   EMPTY_TOKEN, SEMICOLON, TLIST lst', Endmodule, EMPTY_TOKEN) -> (`module_or_interface_declaration1 (id, patlst lst, patlst lst'))
| TUPLE12 (STRING "module_or_interface_declaration1", Module, EMPTY_TOKEN,
      SymbolIdentifier id, EMPTY_TOKEN,
      TUPLE5 (STRING "module_parameter_port_list_opt1", HASH, LPAREN, TLIST lst, RPAREN),
      TUPLE4 (STRING "module_port_list_opt1", LPAREN, TLIST lst', RPAREN),
   EMPTY_TOKEN, SEMICOLON, TLIST lst'', Endmodule, EMPTY_TOKEN) -> (`module_port_list_opt1 (patlst lst, patlst lst', patlst lst''))
| TUPLE6 (STRING "cast1", data_type_primitive1, QUOTE, LPAREN, reference3, RPAREN) -> `cast1
| TK_StringLiteral s -> `string_lit s
| TK_DecNumber num -> `dec_num num
| SymbolIdentifier id -> `ident
| STRING _ -> `string
| COMMA -> `comma
| LESS -> `less
| TUPLE6 (STRING "param_type_followed_by_id_and_dimensions_opt4", Logic,
      EMPTY_TOKEN,
      TUPLE3 (STRING "decl_dimensions2",
        TUPLE6 (STRING "decl_variable_dimension1", LBRACK, TK_DecNumber _, COLON,
          TK_DecNumber _, RBRACK),
        TUPLE6 (STRING "decl_variable_dimension1", LBRACK, TK_DecNumber _, COLON,
          TK_DecNumber _, RBRACK)),
   SymbolIdentifier _, EMPTY_TOKEN) -> `param_type_followed_by_id_and_dimensions_opt4
| TUPLE6
     (STRING "select_variable_dimension1", LBRACK, expr', COLON,
   TK_DecNumber _, RBRACK) -> `select_variable_dimension1
| TUPLE6
     (STRING "simple_immediate_assertion_statement1", Assert, LPAREN,
      reference3, RPAREN,
      TUPLE3
       (STRING "action_block3", Else,
        TUPLE3
      (STRING "statement3", reference_or_call_base1, SEMICOLON))) -> `simple_immediate_assertion_statement1
| TUPLE5
     (STRING "data_declaration_or_module_instantiation2", Automatic,
   EMPTY_TOKEN, instantiation_base1, SEMICOLON) -> `data_declaration_or_module_instantiation2
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
   SEMICOLON) -> `net_declaration2
| TUPLE5 (STRING "net_declaration2", Wire,
      TUPLE3 (STRING "data_type_or_implicit1",
        TUPLE6 (STRING "decl_variable_dimension1", LBRACK, TK_DecNumber _, COLON,
          TK_DecNumber _, RBRACK),
        TUPLE3 (STRING "delay31", HASH, TK_RealTime _)),
      TUPLE4 (STRING "net_decl_assign1", SymbolIdentifier _, EQUALS, unqualified_id1),
   SEMICOLON) -> `net_declaration2
| TUPLE5
     (STRING "net_declaration4", Wire,
      TUPLE3 (STRING "delay31", HASH, TK_RealTime _),
      TUPLE4
       (STRING "net_decl_assign1", SymbolIdentifier _, EQUALS,
        cond_expr2),
   SEMICOLON) -> `net_declaration4
| TUPLE4 (STRING "data_type_or_implicit_basic_followed_by_id_and_dimensions_opt1",
         arg2, arg3, EMPTY_TOKEN) -> `data_type_or_implicit_basic_followed_by_id_and_dimensions_opt1 (pat arg2, pat arg3)
| TUPLE4 (STRING "data_type_or_implicit_basic_followed_by_id_and_dimensions_opt4",
        decl_variable_dimension1,
        unqualified_id1, EMPTY_TOKEN) ->
  `data_type_or_implicit_basic_followed_by_id_and_dimensions_opt4 (pat decl_variable_dimension1, pat unqualified_id1)
| TUPLE5 (STRING "port_declaration_noattr1", Output, Wire, typ,
   EMPTY_TOKEN) -> `port_declaration_noattr1 (pat typ)
| TUPLE3 (STRING "type_identifier_or_implicit_basic_followed_by_id_and_dimensions_opt4", unqualified_id1, EMPTY_TOKEN) ->
   `type_identifier_or_implicit_basic_followed_by_id_and_dimensions_opt4 (pat unqualified_id1)
| TUPLE5 (STRING "port_declaration_noattr1", Output, Wire, typ, EMPTY_TOKEN) -> `port_declaration_noattr1 (pat typ)
| TUPLE5 (STRING "type_identifier_or_implicit_basic_followed_by_id_and_dimensions_opt2",
		 unqualified_id1, EMPTY_TOKEN, unqualified_id2, EMPTY_TOKEN) ->
		 `type_identifier_or_implicit_basic_followed_by_id_and_dimensions_opt2 (pat unqualified_id1, pat unqualified_id2)
| TUPLE5 (STRING "port_declaration_noattr1", Output, EMPTY_TOKEN, typ, EMPTY_TOKEN) -> `port_declaration_noattr1 (pat typ)
(*
 | TUPLE5 (STRING "port_declaration_noattr1", Output, EMPTY_TOKEN,
      TUPLE4
       (STRING "data_type_or_implicit_basic_followed_by_id_and_dimensions_opt1",
        TUPLE3
         (STRING "data_type_primitive1",
          TUPLE3 (STRING "data_type_primitive_scalar1", Logic, EMPTY_TOKEN),
          TUPLE6
           (STRING "decl_variable_dimension1", LBRACK, expr', COLON,
            TK_DecNumber _, RBRACK)),
        unqualified_id1, EMPTY_TOKEN),
   EMPTY_TOKEN) -> `port_declaration_noattr1
| TUPLE5
     (STRING "port_declaration_noattr1", Input, Wire,
      TUPLE4
       (STRING "data_type_or_implicit_basic_followed_by_id_and_dimensions_opt4",
        TUPLE6
         (STRING "decl_variable_dimension1", LBRACK, TK_DecNumber _, COLON,
          TK_DecNumber _, RBRACK),
        unqualified_id1, EMPTY_TOKEN),
   EMPTY_TOKEN) -> `port_declaration_noattr1
| TUPLE5
     (STRING "port_declaration_noattr1", Input, Wire,
      TUPLE3
       (STRING
         "type_identifier_or_implicit_basic_followed_by_id_and_dimensions_opt4",
        unqualified_id1, EMPTY_TOKEN),
   EMPTY_TOKEN) -> `port_declaration_noattr1
| TUPLE5
     (STRING "port_declaration_noattr1", Input, EMPTY_TOKEN,
      TUPLE5
       (STRING
         "type_identifier_or_implicit_basic_followed_by_id_and_dimensions_opt2",
        unqualified_id1, EMPTY_TOKEN, unqualified_id2,
        EMPTY_TOKEN),
   EMPTY_TOKEN) -> `port_declaration_noattr1
| TUPLE5 (STRING "port_declaration_noattr1", Input, EMPTY_TOKEN,
      TUPLE4 (STRING "data_type_or_implicit_basic_followed_by_id_and_dimensions_opt1",
        TUPLE3 (STRING "data_type_primitive1",
          TUPLE3 (STRING "data_type_primitive_scalar1", Logic, EMPTY_TOKEN),
          EMPTY_TOKEN),
        unqualified_id1, EMPTY_TOKEN),
   EMPTY_TOKEN) -> `port_declaration_noattr1
| TUPLE5 (STRING "port_declaration_noattr1", Input, EMPTY_TOKEN,
      TUPLE4 (STRING "data_type_or_implicit_basic_followed_by_id_and_dimensions_opt1",
        TUPLE3 (STRING "data_type_primitive1",
          TUPLE3 (STRING "data_type_primitive_scalar1", Logic, EMPTY_TOKEN),
          TUPLE6 (STRING "decl_variable_dimension1", LBRACK, TK_DecNumber _, COLON,
            TK_DecNumber _, RBRACK)),
        unqualified_id1, EMPTY_TOKEN),
   EMPTY_TOKEN) -> `port_declaration_noattr1
| TUPLE5 (STRING "port_declaration_noattr1", Input, EMPTY_TOKEN,
      TUPLE4
       (STRING "data_type_or_implicit_basic_followed_by_id_and_dimensions_opt1",
        TUPLE3
         (STRING "data_type_primitive1",
          TUPLE3 (STRING "data_type_primitive_scalar1", Logic, EMPTY_TOKEN),
          TUPLE6
           (STRING "decl_variable_dimension1", LBRACK, expr', COLON,
            TK_DecNumber _, RBRACK)),
        unqualified_id1, EMPTY_TOKEN),
   EMPTY_TOKEN) -> `port_declaration_noattr1
| TUPLE5 (STRING "port_declaration_noattr1", Inout, Wire,
      TUPLE3
       (STRING
         "type_identifier_or_implicit_basic_followed_by_id_and_dimensions_opt4",
        unqualified_id1, EMPTY_TOKEN),
   EMPTY_TOKEN) -> `port_declaration_noattr1
| TUPLE5 (STRING "port_declaration_noattr1", Inout, EMPTY_TOKEN,
      TUPLE3
       (STRING
         "type_identifier_or_implicit_basic_followed_by_id_and_dimensions_opt4",
        unqualified_id1, EMPTY_TOKEN),
   EMPTY_TOKEN) -> `port_declaration_noattr1
*)
   | TUPLE4 (STRING "generate_block1", begin1, EMPTY_TOKEN,
   TUPLE3 (STRING "end1", End, EMPTY_TOKEN)) -> (`generate_block1 (pat begin1, `tlist []))
   | TUPLE4 (STRING "jump_statement4", Return, unqualified_id1, SEMICOLON) -> `jump_statement4
   | TUPLE4 (STRING "module_parameter_port1", Parameter,
      TUPLE6 (STRING "param_type_followed_by_id_and_dimensions_opt5", Int,
        EMPTY_TOKEN, EMPTY_TOKEN, SymbolIdentifier _, EMPTY_TOKEN),
      TUPLE4 (STRING "trailing_assign1", EQUALS, unary_prefix_expr2,
     EMPTY_TOKEN)) -> `module_parameter_port1
   | TUPLE4 (STRING "module_parameter_port1", Parameter,
      param_type_followed_by_id_and_dimensions_opt4,
      TUPLE4 (STRING "trailing_assign1", EQUALS, bin_based_number1,
     EMPTY_TOKEN)) -> `module_parameter_port1
   | TUPLE4 (STRING "module_parameter_port2", Parameter, Type,
      TUPLE4 (STRING "type_assignment1", SymbolIdentifier _, EQUALS,
        TUPLE3 (STRING "data_type_primitive1",
          TUPLE3 (STRING "data_type_primitive_scalar1", Logic, EMPTY_TOKEN),
       EMPTY_TOKEN))) -> `module_parameter_port2
   | TUPLE4 (STRING "port_declaration_noattr4",
      TUPLE6 (STRING "type_identifier_followed_by_id3", unqualified_id1, DOT,
        SymbolIdentifier _, EMPTY_TOKEN, SymbolIdentifier _),
   EMPTY_TOKEN, EMPTY_TOKEN) -> `port_declaration_noattr4
   | TUPLE4 (STRING "qualified_id2", unqualified_id1, COLON_COLON,
   unqualified_id2) -> `qualified_id2
   | TUPLE4 (STRING "tf_port_item1", EMPTY_TOKEN,
      TUPLE4 (STRING "data_type_or_implicit_basic_followed_by_id_and_dimensions_opt1",
        TUPLE3 (STRING "data_type_primitive1",
          TUPLE3 (STRING "data_type_primitive_scalar1", Logic, EMPTY_TOKEN),
          TUPLE6 (STRING "decl_variable_dimension1", LBRACK, TK_DecNumber _, COLON,
            TK_DecNumber _, RBRACK)),
        unqualified_id1, EMPTY_TOKEN),
   EMPTY_TOKEN) -> `tf_port_item1
   | TUPLE4 (STRING "tf_port_item1", EMPTY_TOKEN,
      TUPLE4 (STRING "data_type_or_implicit_basic_followed_by_id_and_dimensions_opt1",
        TUPLE3 (STRING "data_type_primitive1",
          TUPLE3 (STRING "data_type_primitive_scalar5", Byte, EMPTY_TOKEN),
          EMPTY_TOKEN),
        unqualified_id1, EMPTY_TOKEN),
   EMPTY_TOKEN) -> `tf_port_item1
| TUPLE3 (STRING "always_construct1", Always_latch, seq_block1) -> (`always_construct1 (pat seq_block1))
| TUPLE3 (STRING "always_construct1", Always_ff,
   procedural_timing_control_statement2) -> (`always_construct1 (pat procedural_timing_control_statement2))
| TUPLE3 (STRING "always_construct1", Always_comb, seq_block1) -> (`always_construct1 (pat seq_block1))
| TUPLE3 (STRING "any_port_list_item_last1",
      TUPLE3 (STRING "any_port_list_trailing_comma1", TLIST _, COMMA),
   TUPLE5 (STRING "port_named2", DOT, SymbolIdentifier _, LPAREN, RPAREN)) -> (`any_port_list_item_last1 (`tlist []))
| TUPLE3 (STRING "any_port_list_item_last1",
      TUPLE3 (STRING "any_port_list_trailing_comma1", TLIST lst, COMMA),
   TUPLE3 (STRING "port_named3", DOT, SymbolIdentifier _)) -> (`any_port_list_item_last1 (patlst lst))
| TUPLE3 (STRING "begin1", Begin,
   TUPLE3 (STRING "label_opt1", COLON, SymbolIdentifier _)) -> `begin1
| TUPLE3 (STRING "conditional_generate_construct2",
      TUPLE3 (STRING "generate_if1", If, expression_in_parens1),
   generate_block1) -> `conditional_generate_construct2
| TUPLE3 (STRING "data_type_primitive1",
      TUPLE3 (STRING "data_type_primitive_scalar1", Logic, EMPTY_TOKEN),
   EMPTY_TOKEN) -> `data_type_primitive1
| TUPLE3 (STRING "data_type_primitive1",
      TUPLE3 (STRING "data_type_primitive_scalar1", Logic, EMPTY_TOKEN),
      TUPLE6
       (STRING "decl_variable_dimension1", LBRACK, TK_DecNumber _, COLON,
     TK_DecNumber _, RBRACK)) -> `data_type_primitive1
| TUPLE3 (STRING "data_type_primitive1",
      TUPLE3 (STRING "data_type_primitive_scalar1", Logic, EMPTY_TOKEN),
      TUPLE6
       (STRING "decl_variable_dimension1", LBRACK, unqualified_id1,
     COLON, TK_DecNumber _, RBRACK)) -> `data_type_primitive1
| TUPLE3 (STRING "data_type_primitive1",
      TUPLE3 (STRING "data_type_primitive_scalar1", Bit, EMPTY_TOKEN),
   EMPTY_TOKEN) -> `data_type_primitive1
| TUPLE3 (STRING "data_type_primitive1",
      TUPLE3 (STRING "data_type_primitive_scalar5", Int, EMPTY_TOKEN),
   EMPTY_TOKEN) -> `data_type_primitive1
| TUPLE3 (STRING "data_type_primitive1",
      TUPLE3 (STRING "data_type_primitive_scalar5", Byte, EMPTY_TOKEN),
   EMPTY_TOKEN) -> `data_type_primitive1
| TUPLE3 (STRING "dec_based_number1", TK_DecBase _, TK_DecDigits _) -> `dec_based_number1
| TUPLE3 (STRING "decl_dimensions2",
      TUPLE6
       (STRING "decl_variable_dimension1", LBRACK, TK_DecNumber _, COLON,
        TK_DecNumber _, RBRACK),
      TUPLE6
       (STRING "decl_variable_dimension1", LBRACK, TK_DecNumber _, COLON,
     TK_DecNumber _, RBRACK)) -> `decl_dimensions2
| TUPLE3 (STRING "hex_based_number1", TK_HexBase _, TK_HexDigits _) -> `hex_based_number1
| TUPLE3 (STRING "inc_or_dec_expression2", unqualified_id1, PLUS_PLUS) -> `inc_or_dec_expression2
| TUPLE3 (STRING "label_opt1", COLON, SymbolIdentifier _) -> `label_opt1
| TUPLE3 (STRING "port_named3", DOT, SymbolIdentifier _) -> `port_named3
| TUPLE3 (STRING "reference2", unqualified_id1,
   TUPLE3 (STRING "hierarchy_extension1", DOT, unqualified_id2)) -> `reference2
| TUPLE3
     (STRING "reference3", unqualified_id1,
      TUPLE6
       (STRING "select_variable_dimension1", LBRACK, expr', COLON,
     TK_DecNumber _, RBRACK)) -> `reference3
| TUPLE12 (STRING "module_or_interface_declaration1", Module, EMPTY_TOKEN,
      SymbolIdentifier id, EMPTY_TOKEN,
      TUPLE5 (STRING "module_parameter_port_list_opt1", HASH, LPAREN, TLIST lst, RPAREN),
      TUPLE4 (STRING "module_port_list_opt1", LPAREN, TLIST lst', RPAREN),
      EMPTY_TOKEN, SEMICOLON, TLIST lst'', Endmodule,
   TUPLE3 (STRING "label_opt1", COLON, SymbolIdentifier _)) -> (`module_or_interface_declaration1 (id, patlst lst, patlst lst'))
| TUPLE12 (STRING "module_or_interface_declaration1", Interface, EMPTY_TOKEN,
      SymbolIdentifier id, EMPTY_TOKEN,
      TUPLE5 (STRING "module_parameter_port_list_opt1", HASH, LPAREN, TLIST lst, RPAREN),
      TUPLE4 (STRING "module_port_list_opt1", LPAREN, TLIST lst', RPAREN),
   EMPTY_TOKEN, SEMICOLON, EMPTY_TOKEN, Endinterface, EMPTY_TOKEN) -> (`module_or_interface_declaration1 (id, patlst lst, patlst lst'))
| TUPLE11
     (STRING "function_declaration1", Function, EMPTY_TOKEN,
      TUPLE4
       (STRING "data_type_or_implicit_basic_followed_by_id_and_dimensions_opt5",
        Void, unqualified_id1, EMPTY_TOKEN),
      LPAREN, TLIST _, RPAREN, SEMICOLON, TLIST _, Endfunction,
   TUPLE3 (STRING "label_opt1", COLON, SymbolIdentifier _)) -> `function_declaration1
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
   TUPLE3 (STRING "label_opt1", COLON, SymbolIdentifier _)) -> `function_declaration1
| TUPLE10 (STRING "loop_statement1", For, LPAREN,
      TUPLE4 (STRING "for_init_decl_or_assign1", unqualified_id1, EQUALS, TK_DecNumber num),
      SEMICOLON, expr', SEMICOLON, assignment_statement_no_expr1,
   RPAREN, seq_block) -> (`loop_statement1 (pat expr', pat seq_block))
| TUPLE10 (STRING "loop_statement1", For, LPAREN,
      TUPLE5 (STRING "for_init_decl_or_assign2",
        TUPLE3 (STRING "data_type_primitive1",
          TUPLE3 (STRING "data_type_primitive_scalar5", Int, Unsigned),
          EMPTY_TOKEN),
        SymbolIdentifier _, EQUALS, TK_DecNumber _),
      SEMICOLON, expr', SEMICOLON, inc_or_dec_expression2, RPAREN,
   seq_block) -> (`loop_statement1 (pat expr', pat seq_block))
| TUPLE4 (STRING "assignment_pattern1", QUOTE_LBRACE, TLIST lst, RBRACE) -> (`assignment_pattern1 (patlst lst))
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
   Endcase) -> `case_statement3
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
   Endcase) -> `case_statement3
| TUPLE8 (STRING "case_statement1", Unique, Case, LPAREN,
      TUPLE3
       (STRING "reference2", STRING "unqualified_id1",
        TUPLE3 (STRING "hierarchy_extension1", DOT, STRING "unqualified_id1")),
   RPAREN, STRING "case_items1", Endcase) -> `case_statement1
| TUPLE8 (STRING "case_statement1", Unique, Case, LPAREN, STRING "unqualified_id1",
   RPAREN, STRING "case_items1", Endcase) -> `case_statement1
| TUPLE4 (STRING "comp_expr2", expr1, LESS, expr2) -> `less_expr2 (pat expr1, pat expr2)
| TUPLE4 (STRING "logand_expr2", expr1, AMPERSAND_AMPERSAND, expr2) -> `logand_expr2 (pat expr1, pat expr2)
| TUPLE4 (STRING "logor_expr2", expr1, VBAR_VBAR, expr2) -> `logor_expr2 (pat expr1, pat expr2)
| TUPLE4 (STRING "logeq_expr2", expr1, EQ_EQ, expr2) -> `logeq_expr2 (pat expr1, pat expr2)
| TUPLE4 (STRING "logeq_expr3", expr1, PLING_EQ, expr2) -> `plingeq_expr2 (pat expr1, pat expr2)
| TUPLE4 (STRING "bitand_expr2", expr1, AMPERSAND, expr2) -> `bitand_expr2 (pat expr1, pat expr2)
| TUPLE4 (STRING "bitor_expr2", expr1, VBAR, expr2) -> `bitor_expr2 (pat expr1, pat expr2)
| TUPLE4 (STRING "xor_expr2", expr1, CARET, expr2) -> `xor_expr2 (pat expr1, pat expr2)
| TUPLE4 (STRING "add_expr2", expr1, PLUS, expr2) -> `add_expr2 (pat expr1, pat expr2)
| TUPLE4 (STRING "add_expr3", expr1, HYPHEN, expr2) -> `sub_expr2 (pat expr1, pat expr2)
| TUPLE4 (STRING "mul_expr2", expr1, STAR, expr2) -> `mul_expr2 (pat expr1, pat expr2)
| TUPLE4 (STRING "shift_expr2", expr1, LT_LT, expr2) -> `shift_expr2 (pat expr1, pat expr2)
| TUPLE4 (STRING "shift_expr3", expr1, GT_GT, expr2) -> `shift_expr3 (pat expr1, pat expr2)
| TUPLE4 (STRING "shift_expr4", expr1, GT_GT_GT, expr2) -> `shift_expr4 (pat expr1, pat expr2)
| EMPTY_TOKEN -> `empty
| oth -> othpat' := oth; failwith "pat"

and patlst lst = `tlist (List.map pat lst)

and pat itm = othpatlst := itm :: !othpatlst; let rslt = pat' itm in othpatlst := List.tl !othpatlst; rslt
