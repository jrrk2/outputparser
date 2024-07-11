open Source_text_verible

type othtran =
          | Vadd_expr of othtran * othtran
          | Valways_construct1 of othtran
          | Vany_argument_list_item_last2 of othtran
          | Vany_param_declaration4 of othtran * othtran
          | Vany_port_list_item_last1 of othtran * othtran
          | Vany_port_list_trailing_comma1 of othtran
          | Vassignment_pattern1 of othtran
          | Vassignment_statement_no_expr1 of othtran * othtran
          | Vbegin1 of othtran
          | Vbin_based_number1 of string * string
          | Vbitand_expr of othtran * othtran
          | Vbitor_expr of othtran * othtran
          | Vblock_item_or_statement_or_null6
          | Vcall_base1 of othtran
          | Vcase_inside_item1
          | Vcase_inside_item2
          | Vcase_inside_items1
          | Vcase_item1 of othtran * othtran
          | Vcase_item2 of othtran
          | Vcase_items1 of othtran * othtran
          | Vcase_statement1 of othtran * othtran * othtran
          | Vcase_statement3
          | Vcast1
          | Vcomma
          | Vcond_expr of othtran * othtran * othtran
          | Vconditional_generate_construct1 of othtran * othtran * othtran
          | Vconditional_generate_construct2 of othtran * othtran
          | Vconditional_statement1 of othtran * othtran
          | Vconditional_statement2 of othtran * othtran * othtran
          | Vcont_assign1 of othtran * othtran
          | Vcontinuous_assign1 of othtran
          | Vdata_declaration_or_module_instantiation1 of othtran
          | Vdata_declaration_or_module_instantiation2
          | Vdata_type_or_implicit_basic_followed_by_id_and_dimensions_opt1 of othtran * othtran
          | Vdata_type_or_implicit_basic_followed_by_id_and_dimensions_opt4 of othtran * othtran
          | Vdata_type_primitive1 of othtran * othtran
          | Vdata_type_primitive_scalar1_logic
          | Vdata_type_primitive_scalar1_reg
          | Vdec_based_number1 of string * string
          | Vdec_num of string
          | Vdecl_dimensions2 of othtran * othtran
          | Vdecl_variable_dimension1 of othtran * othtran
          | Vdiv_expr of othtran * othtran
          | Vempty
          | Vend1
          | Vevent_control2 of othtran
          | Vevent_control4
          | Vevent_expression_negedge of othtran
          | Vevent_expression_posedge of othtran
          | Vexpr_primary_braces2 of othtran * othtran
          | Vexpr_primary_parens1 of othtran
          | Vexpression_in_parens1 of othtran
          | Vexpression_list_proper1
          | Vexpression_or_dist1 of othtran
          | Vfunction_declaration1
          | Vgate_instance_or_register_variable1
          | Vgenerate_block1 of othtran * othtran
          | Vgenerate_if1 of othtran
          | Vgenerate_region1 of othtran
          | Vgt_expr of othtran * othtran
          | Vgteq_expr of othtran * othtran
          | Vhex_based_number1 of string * string
          | Vhierarchy_extension1 of othtran
          | Vident
          | Videntifier_optional_unpacked_dimensions1 of string
          | Vinc_or_dec_expression2
          | Vinitial_construct1 of othtran
          | Vinput
          | Vinstantiation_base1 of othtran
          | Vinstantiation_base1_reg of othtran
          | Vjump_statement4
          | Vlabel_opt1 of string
          | Vless
          | Vlogand_expr of othtran * othtran
          | Vlogeq_expr of othtran * othtran
          | Vlogor_expr of othtran * othtran
          | Vloop_statement1 of othtran * othtran
          | Vlt_expr of othtran * othtran
          | Vlteq_expr of othtran * othtran
          | Vml_start1 of othtran
          | Vmod_expr of othtran * othtran
          | Vmodule_or_interface_declaration1 of string * othtran * othtran
          | Vmodule_parameter_port1
          | Vmodule_parameter_port2
          | Vmodule_parameter_port_list_item_last1 of othtran
          | Vmodule_port_declaration3 of othtran * othtran * othtran * othtran
          | Vmodule_port_declaration5 of othtran * othtran * othtran
          | Vmodule_port_declaration7_reg of othtran * othtran * string
          | Vmodule_port_list_opt1 of othtran * othtran * othtran
          | Vmul_expr of othtran * othtran
          | Vnet_declaration1
          | Vnet_declaration2
          | Vnet_declaration4
          | Vnet_variable1
          | Vnon_anonymous_gate_instance_or_register_variable1 of string * othtran
          | Vnon_anonymous_gate_instance_or_register_variable2 of othtran
          | Vnonblocking_assignment1 of othtran * othtran
          | Voutput
          | Vparam_type_followed_by_id_and_dimensions_opt2 of othtran * string
          | Vparam_type_followed_by_id_and_dimensions_opt3 of othtran
          | Vparam_type_followed_by_id_and_dimensions_opt4
          | Vparameter_value_byname1
          | Vparameter_value_byname_list_item_last2 of othtran
          | Vparameters2 of othtran
          | Vplingeq_expr of othtran * othtran
          | Vport1 of othtran
          | Vport_declaration_noattr1 of othtran * othtran
          | Vport_declaration_noattr4
          | Vport_named1 of string * othtran
          | Vport_named2 of string
          | Vport_named3 of string
          | Vpow_expr of othtran * othtran
          | Vprocedural_timing_control_statement2 of othtran * othtran
          | Vqualified_id2
          | Vrange_list_in_braces1 of othtran
          | Vreference2 of othtran * othtran
          | Vreference3 of othtran * othtran
          | Vreference_or_call_base1 of othtran
          | Vselect_variable_dimension1 of othtran * othtran
          | Vselect_variable_dimension2 of othtran
          | Vselect_variable_dimension3 of othtran * othtran
          | Vseq_block1 of othtran
          | Vsequence_repetition_expr1 of othtran
          | Vshift_expr2 of othtran * othtran
          | Vshift_expr3 of othtran * othtran
          | Vshift_expr4 of othtran * othtran
          | Vsigned
          | Vsimple_immediate_assertion_statement1
          | Vstatement3 of othtran
          | Vstatement_item6 of othtran
          | Vstring
          | Vstring_literal of string
          | Vsub_expr of othtran * othtran
          | Vsystem_tf_call1 of string * othtran
          | Vtask_declaration1 of othtran
          | Vtf_port_item1
          | Vtlist of othtran list
          | Vtrailing_decl_assignment2
          | Vtype_identifier_or_implicit_basic_followed_by_id_and_dimensions_opt2 of othtran * othtran
          | Vtype_identifier_or_implicit_basic_followed_by_id_and_dimensions_opt4 of othtran
          | Vunary_prefix_expr_and of othtran
          | Vunary_prefix_expr_nand of othtran
          | Vunary_prefix_expr_negate of othtran
          | Vunary_prefix_expr_nor of othtran
          | Vunary_prefix_expr_not of othtran
          | Vunary_prefix_expr_or of othtran
          | Vunary_prefix_expr_plus of othtran
          | Vunary_prefix_expr_tilde of othtran
          | Vunary_prefix_expr_xnor of othtran
          | Vunary_prefix_expr_xor of othtran
          | Vunique
          | Vunqualified_id1 of string * othtran
          | Vxor_expr of othtran * othtran

let fail' msg = function
      | Vadd_expr (othtran, othtran') -> failwith (msg^": add_expr")
      | Valways_construct1 (othtran') -> failwith (msg^": always_construct1")
      | Vany_argument_list_item_last2 (othtran_lst) -> failwith (msg^": any_argument_list_item_last2")
      | Vany_param_declaration4 (othtran, othtran') -> failwith (msg^": any_param_declaration4")
      | Vany_port_list_item_last1 (othtran, othtran') -> failwith (msg^": any_port_list_item_last1")
      | Vany_port_list_trailing_comma1 (othtran') -> failwith (msg^": any_port_list_trailing_comma1")
      | Vassignment_pattern1 (othtran') -> failwith (msg^": assignment_pattern1")
      | Vassignment_statement_no_expr1 (othtran, othtran') -> failwith (msg^": assignment_statement_no_expr1")
      | Vbegin1 (othtran') -> failwith (msg^": begin1")
      | Vbin_based_number1 (string, string') -> failwith (msg^": bin_based_number1")
      | Vbitand_expr (othtran, othtran') -> failwith (msg^": bitand_expr")
      | Vbitor_expr (othtran, othtran') -> failwith (msg^": bitor_expr")
      | Vblock_item_or_statement_or_null6 -> failwith (msg^": block_item_or_statement_or_null6")
      | Vcall_base1 (othtran') -> failwith (msg^": call_base1")
      | Vcase_inside_item1 -> failwith (msg^": case_inside_item1")
      | Vcase_inside_item2 -> failwith (msg^": case_inside_item2")
      | Vcase_inside_items1 -> failwith (msg^": case_inside_items1")
      | Vcase_item1 (othtran, othtran') -> failwith (msg^": case_item1")
      | Vcase_item2 (othtran') -> failwith (msg^": case_item2")
      | Vcase_items1 (othtran, othtran') -> failwith (msg^": case_items1")
      | Vcase_statement1 (othtran, othtran', othtran'') -> failwith (msg^": case_statement1")
      | Vcase_statement3 -> failwith (msg^": case_statement3")
      | Vcast1 -> failwith (msg^": cast1")
      | Vcomma -> failwith (msg^": comma")
      | Vcond_expr (othtran, othtran', othtran'') -> failwith (msg^": cond_expr")
      | Vconditional_generate_construct1 (othtran, othtran', othtran'') -> failwith (msg^": conditional_generate_construct1")
      | Vconditional_generate_construct2 (othtran, othtran') -> failwith (msg^": conditional_generate_construct2")
      | Vconditional_statement1 (othtran, othtran') -> failwith (msg^": conditional_statement1")
      | Vconditional_statement2 (othtran, othtran', othtran'') -> failwith (msg^": conditional_statement2")
      | Vcont_assign1(lhs,rhs) -> failwith (msg^": cont_assign1")
      | Vcontinuous_assign1 (othtran') -> failwith (msg^": continuous_assign1")
      | Vdata_declaration_or_module_instantiation1 (othtran') -> failwith (msg^": data_declaration_or_module_instantiation1")
      | Vdata_declaration_or_module_instantiation2 -> failwith (msg^": data_declaration_or_module_instantiation2")
      | Vdata_type_or_implicit_basic_followed_by_id_and_dimensions_opt1 (othtran, othtran') -> failwith (msg^": data_type_or_implicit_basic_followed_by_id_and_dimensions_opt1")
      | Vdata_type_or_implicit_basic_followed_by_id_and_dimensions_opt4 (othtran, othtran') -> failwith (msg^": data_type_or_implicit_basic_followed_by_id_and_dimensions_opt4")
      | Vdata_type_primitive1 (othtran, othtran') -> failwith (msg^": data_type_primitive1")
      | Vdata_type_primitive_scalar1_logic -> failwith (msg^": data_type_primitive_scalar1_logic")
      | Vdata_type_primitive_scalar1_reg -> failwith (msg^": data_type_primitive_scalar1_reg")
      | Vdec_based_number1 (string, string') -> failwith (msg^": dec_based_number1")
      | Vdec_num (string) -> failwith (msg^": dec_num (string)")
      | Vdecl_dimensions2 (othtran, othtran') -> failwith (msg^": decl_dimensions2")
      | Vdecl_variable_dimension1 (othtran, othtran') -> failwith (msg^": decl_variable_dimension1")
      | Vdiv_expr (othtran, othtran') -> failwith (msg^": div_expr")
      | Vempty -> failwith (msg^": empty")
      | Vend1 -> failwith (msg^": end1")
      | Vevent_control2 (othtran') -> failwith (msg^": event_control2")
      | Vevent_control4 -> failwith (msg^": event_control4")
      | Vevent_expression_negedge (othtran') -> failwith (msg^": event_expression_negedge")
      | Vevent_expression_posedge (othtran') -> failwith (msg^": event_expression_posedge")
      | Vexpr_primary_braces2 (othtran, othtran') -> failwith (msg^": expr_primary_braces2")
      | Vexpr_primary_parens1 (othtran') -> failwith (msg^": expr_primary_parens1")
      | Vexpression_in_parens1 (othtran') -> failwith (msg^": expression_in_parens1")
      | Vexpression_list_proper1 -> failwith (msg^": expression_list_proper1")
      | Vexpression_or_dist1 othtran -> failwith (msg^": expression_or_dist1")
      | Vfunction_declaration1 -> failwith (msg^": function_declaration1")
      | Vgate_instance_or_register_variable1 -> failwith (msg^": gate_instance_or_register_variable1")
      | Vgenerate_block1 (othtran, othtran') -> failwith (msg^": generate_block1")
      | Vgenerate_if1 (othtran') -> failwith (msg^": generate_if1")
      | Vgenerate_region1 (othtran') -> failwith (msg^": generate_region1")
      | Vgt_expr (othtran, othtran') -> failwith (msg^": gt_expr")
      | Vgteq_expr (othtran, othtran') -> failwith (msg^": gteq_expr")
      | Vhex_based_number1 (string, string') -> failwith (msg^": hex_based_number1")
      | Vhierarchy_extension1 (othtran') -> failwith (msg^": hierarchy_extension1")
      | Vident -> failwith (msg^": ident")
      | Videntifier_optional_unpacked_dimensions1 (string) -> failwith (msg^": identifier_optional_unpacked_dimensions1 (string)")
      | Vinc_or_dec_expression2 -> failwith (msg^": inc_or_dec_expression2")
      | Vinitial_construct1 (othtran') -> failwith (msg^": initial_construct1")
      | Vinput -> failwith (msg^": input")
      | Vinstantiation_base1 (othtran') -> failwith (msg^": instantiation_base1")
      | Vinstantiation_base1_reg (othtran') -> failwith (msg^": instantiation_base1_reg")
      | Vjump_statement4 -> failwith (msg^": jump_statement4")
      | Vlabel_opt1 (string) -> failwith (msg^": label_opt1 (string)")
      | Vless -> failwith (msg^": less")
      | Vlogand_expr (othtran, othtran') -> failwith (msg^": logand_expr")
      | Vlogeq_expr (othtran, othtran') -> failwith (msg^": logeq_expr")
      | Vlogor_expr (othtran, othtran') -> failwith (msg^": logor_expr")
      | Vloop_statement1 (othtran, othtran') -> failwith (msg^": loop_statement1")
      | Vlt_expr (othtran, othtran') -> failwith (msg^": lt_expr")
      | Vlteq_expr (othtran, othtran') -> failwith (msg^": lteq_expr")
      | Vml_start1 (othtran') -> failwith (msg^": ml_start1")
      | Vmod_expr (othtran, othtran') -> failwith (msg^": mod_expr")
      | Vmodule_or_interface_declaration1 (string, othtran, othtran') -> failwith (msg^": module_or_interface_declaration1 (string, ")
      | Vmodule_parameter_port1 -> failwith (msg^": module_parameter_port1")
      | Vmodule_parameter_port2 -> failwith (msg^": module_parameter_port2")
      | Vmodule_parameter_port_list_item_last1 (othtran') -> failwith (msg^": module_parameter_port_list_item_last1")
      | Vmodule_port_declaration3 (othtran, othtran', othtran'', othtran''') -> failwith (msg^": module_port_declaration3")
      | Vmodule_port_declaration5 (dir, othtran, othtran') -> failwith (msg^": module_port_declaration5")
      | Vmodule_port_declaration7_reg (dir, othtran, string) -> failwith (msg^": module_port_declaration7_reg")
      | Vmodule_port_list_opt1 (othtran, othtran', othtran'') -> failwith (msg^": module_port_list_opt1")
      | Vmul_expr (othtran, othtran') -> failwith (msg^": mul_expr")
      | Vnet_declaration1 -> failwith (msg^": net_declaration1")
      | Vnet_declaration2 -> failwith (msg^": net_declaration2")
      | Vnet_declaration4 -> failwith (msg^": net_declaration4")
      | Vnet_variable1 -> failwith (msg^": net_variable1")
      | Vnon_anonymous_gate_instance_or_register_variable1 (string, othtran') -> failwith (msg^": non_anonymous_gate_instance_or_register_variable1 (string, ")
      | Vnon_anonymous_gate_instance_or_register_variable2 (othtran') -> failwith (msg^": non_anonymous_gate_instance_or_register_variable2")
      | Vnonblocking_assignment1 (othtran, othtran') -> failwith (msg^": nonblocking_assignment1")
      | Voutput -> failwith (msg^": output")
      | Vparam_type_followed_by_id_and_dimensions_opt2 (othtran, string) -> failwith (msg^": param_type_followed_by_id_and_dimensions_opt2")
      | Vparam_type_followed_by_id_and_dimensions_opt3 (othtran') -> failwith (msg^": param_type_followed_by_id_and_dimensions_opt3")
      | Vparam_type_followed_by_id_and_dimensions_opt4 -> failwith (msg^": param_type_followed_by_id_and_dimensions_opt4")
      | Vparameter_value_byname1 -> failwith (msg^": parameter_value_byname1")
      | Vparameter_value_byname_list_item_last2 (othtran') -> failwith (msg^": parameter_value_byname_list_item_last2")
      | Vparameters2 (othtran') -> failwith (msg^": parameters2")
      | Vplingeq_expr (othtran, othtran') -> failwith (msg^": plingeq_expr")
      | Vport1 (othtran') -> failwith (msg^": port1")
      | Vport_declaration_noattr1 (othtran, othtran') -> failwith (msg^": port_declaration_noattr1")
      | Vport_declaration_noattr4 -> failwith (msg^": port_declaration_noattr4")
      | Vport_named1 (string, othtran') -> failwith (msg^": port_named1 (string, ")
      | Vport_named2 (string) -> failwith (msg^": port_named2 (string)")
      | Vport_named3 (string) -> failwith (msg^": port_named3 (string)")
      | Vpow_expr (othtran, othtran') -> failwith (msg^": pow_expr")
      | Vprocedural_timing_control_statement2 (othtran, othtran') -> failwith (msg^": procedural_timing_control_statement2")
      | Vqualified_id2 -> failwith (msg^": qualified_id2")
      | Vrange_list_in_braces1 (othtran') -> failwith (msg^": range_list_in_braces1")
      | Vreference2 (othtran, othtran') -> failwith (msg^": reference2")
      | Vreference3 (othtran, othtran') -> failwith (msg^": reference3")
      | Vreference_or_call_base1 (othtran') -> failwith (msg^": reference_or_call_base1")
      | Vselect_variable_dimension1 (othtran, othtran') -> failwith (msg^": select_variable_dimension1")
      | Vselect_variable_dimension2 (othtran') -> failwith (msg^": select_variable_dimension2")
      | Vselect_variable_dimension3 (othtran, othtran') -> failwith (msg^": select_variable_dimension3")
      | Vseq_block1 (othtran') -> failwith (msg^": seq_block1")
      | Vsequence_repetition_expr1 lst -> failwith (msg^": sequence_repetition_expr1")
      | Vshift_expr2 (othtran, othtran') -> failwith (msg^": shift_expr2")
      | Vshift_expr3 (othtran, othtran') -> failwith (msg^": shift_expr3")
      | Vshift_expr4 (othtran, othtran') -> failwith (msg^": shift_expr4")
      | Vsigned -> failwith (msg^": signed")
      | Vsimple_immediate_assertion_statement1 -> failwith (msg^": simple_immediate_assertion_statement1")
      | Vstatement3 (othtran') -> failwith (msg^": statement3")
      | Vstatement_item6 (othtran') -> failwith (msg^": statement_item6")
      | Vstring -> failwith (msg^": string")
      | Vstring_literal (string) -> failwith (msg^": string_literal (string)")
      | Vsub_expr (othtran, othtran') -> failwith (msg^": sub_expr")
      | Vsystem_tf_call1 (string, othtran') -> failwith (msg^": system_tf_call1 (string, ")
      | Vtask_declaration1 (othtran') -> failwith (msg^": task_declaration1")
      | Vtf_port_item1 -> failwith (msg^": tf_port_item1")
      | Vtlist (othtran_lst) -> failwith (msg^": tlist")
      | Vtrailing_decl_assignment2 -> failwith (msg^": trailing_decl_assignment2")
      | Vtype_identifier_or_implicit_basic_followed_by_id_and_dimensions_opt2 (othtran, othtran') -> failwith (msg^": type_identifier_or_implicit_basic_followed_by_id_and_dimensions_opt2")
      | Vtype_identifier_or_implicit_basic_followed_by_id_and_dimensions_opt4 (othtran') -> failwith (msg^": type_identifier_or_implicit_basic_followed_by_id_and_dimensions_opt4")
      | Vunary_prefix_expr_and (othtran') -> failwith (msg^": unary_prefix_expr_and")
      | Vunary_prefix_expr_nand (othtran') -> failwith (msg^": unary_prefix_expr_nand")
      | Vunary_prefix_expr_negate (othtran') -> failwith (msg^": unary_prefix_expr_negate")
      | Vunary_prefix_expr_nor (othtran') -> failwith (msg^": unary_prefix_expr_nor")
      | Vunary_prefix_expr_not (othtran') -> failwith (msg^": unary_prefix_expr_not")
      | Vunary_prefix_expr_or (othtran') -> failwith (msg^": unary_prefix_expr_or")
      | Vunary_prefix_expr_plus (othtran') -> failwith (msg^": unary_prefix_expr_plus")
      | Vunary_prefix_expr_tilde (othtran') -> failwith (msg^": unary_prefix_expr_tilde")
      | Vunary_prefix_expr_xnor (othtran') -> failwith (msg^": unary_prefix_expr_xnor")
      | Vunary_prefix_expr_xor (othtran') -> failwith (msg^": unary_prefix_expr_xor")
      | Vunique -> failwith (msg^": unique")
      | Vunqualified_id1 (string, othtran') -> failwith (msg^": unqualified_id1 (string, ")
      | Vxor_expr (othtran, othtran') -> failwith (msg^": xor_expr")

let othpat' = ref End_of_file
let othpatlst = ref []
let othtran = ref Vempty
let othcnst = ref (Input_types.CNST (32, ERR ""), Input_types.CNST (32, ERR ""))
let fail msg x = othtran := x; fail' msg x

let rec pat' = function
| TK_StringLiteral s -> Vstring_literal s
| TLIST lst -> Vtlist (List.map pat lst)
| TUPLE9 (STRING "task_declaration1", Task, EMPTY_TOKEN, SymbolIdentifier id,
   EMPTY_TOKEN, SEMICOLON, TLIST lst, Endtask, EMPTY_TOKEN) -> (Vtask_declaration1 (patlst lst))
| TUPLE7 (STRING "conditional_statement2", EMPTY_TOKEN, If,
       expression_in_parens1,
       then_statement,
   Else, else_statement) -> Vconditional_statement2 (pat expression_in_parens1, pat then_statement, pat else_statement)
| TUPLE7 (STRING "expr_primary_braces2", LBRACE, braced, LBRACE,
  reference3, RBRACE, RBRACE) -> Vexpr_primary_braces2 (pat braced, pat reference3)
| TUPLE6 (STRING "cond_expr2", expr_primary, QUERY, expr_true, COLON, expr_false) ->
  Vcond_expr (pat expr_primary, pat expr_true, pat expr_false)
| TUPLE6 (STRING "continuous_assign1", Assign, EMPTY_TOKEN, dly, TLIST lst,
  SEMICOLON) -> (Vcontinuous_assign1 (patlst lst))
| TUPLE6 (STRING "decl_variable_dimension1", LBRACK, hi, COLON, lo, RBRACK) -> Vdecl_variable_dimension1 (pat hi, pat lo)
| TUPLE6 (STRING "non_anonymous_gate_instance_or_register_variable2",
  SymbolIdentifier id, EMPTY_TOKEN, LPAREN, TLIST lst, RPAREN) -> (Vnon_anonymous_gate_instance_or_register_variable2 (patlst lst))
| TUPLE6 (STRING "nonblocking_assignment1", lhs, LT_EQ, EMPTY_TOKEN, rhs, SEMICOLON) -> (Vnonblocking_assignment1 (pat lhs, pat rhs))
| TUPLE6 (STRING "param_type_followed_by_id_and_dimensions_opt4", (Logic|EMPTY_TOKEN),
       EMPTY_TOKEN, TUPLE6 (STRING "decl_variable_dimension1", LBRACK, TK_DecNumber hi, COLON,
         TK_DecNumber lo, RBRACK),
   SymbolIdentifier id, EMPTY_TOKEN) -> Vparam_type_followed_by_id_and_dimensions_opt4
| TUPLE6 (STRING "parameter_value_byname1", DOT, SymbolIdentifier id, LPAREN,
  TK_DecNumber num, RPAREN) -> Vparameter_value_byname1
| TUPLE6 (STRING "parameter_value_byname1", DOT, SymbolIdentifier id, LPAREN,
   unqualified_id1, RPAREN) -> Vparameter_value_byname1
| TUPLE7 (STRING "param_type_followed_by_id_and_dimensions_opt2", EMPTY_TOKEN,
         EMPTY_TOKEN, unqualified_id1, EMPTY_TOKEN, SymbolIdentifier id,
         EMPTY_TOKEN) -> Vparam_type_followed_by_id_and_dimensions_opt2 (pat unqualified_id1, id)
| TUPLE5 (STRING "any_param_declaration4", Localparam,
       param_type_followed_by_id_and_dimensions_opt2,
       TUPLE4 (STRING "trailing_assign1", EQUALS, expr', EMPTY_TOKEN),
   SEMICOLON) -> Vany_param_declaration4 (pat param_type_followed_by_id_and_dimensions_opt2, pat expr')
| TUPLE5 (STRING "param_type_followed_by_id_and_dimensions_opt3", EMPTY_TOKEN,
         EMPTY_TOKEN, unqualified_id1, EMPTY_TOKEN)  -> (Vparam_type_followed_by_id_and_dimensions_opt3 (pat unqualified_id1))
| TUPLE5 (STRING "conditional_generate_construct1", expr,
   generate_block1, Else, conditional_generate_construct1) -> Vconditional_generate_construct1 (pat expr, pat generate_block1, pat conditional_generate_construct1)
| TUPLE5 (STRING "conditional_statement1", EMPTY_TOKEN, If,
   expression_in_parens1, seq_block1) -> (Vconditional_statement1 (pat expression_in_parens1, pat seq_block1))
| TUPLE5 (STRING "event_control2", AT, LPAREN, TLIST lst, RPAREN) -> (Vevent_control2 (patlst lst))
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
   SEMICOLON) -> Vnet_declaration2
| TUPLE5 (STRING "net_declaration2", Wire,
       TUPLE3
       (STRING "data_type_or_implicit1",
         TUPLE6
         (STRING "decl_variable_dimension1", LBRACK, TK_DecNumber hi, COLON,
           TK_DecNumber lo, RBRACK),
         EMPTY_TOKEN),
    net_variable1, SEMICOLON) -> Vnet_declaration2
| TUPLE5 (STRING "port_declaration_noattr1", dir, EMPTY_TOKEN, typ, EMPTY_TOKEN) -> Vport_declaration_noattr1 (pat dir, pat typ)
| TUPLE4 (STRING "assignment_statement_no_expr1", range_list_in_braces1, EQUALS, expr') ->
    Vassignment_statement_no_expr1 (pat range_list_in_braces1, pat expr')
| TUPLE4 (STRING "call_base1", LPAREN, TLIST lst, RPAREN) -> (Vcall_base1 (patlst lst))
| TUPLE4 (STRING "case_item1", sel, COLON, seq_block) -> Vcase_item1 (pat sel, pat seq_block)
| TUPLE4 (STRING "case_item2", Default, COLON, SEMICOLON) -> Vcase_item2 Vempty
| TUPLE4 (STRING "case_item2", Default, COLON, seq_block) -> Vcase_item2 (pat seq_block)
| TUPLE4 (STRING "cont_assign1", unqualified_id1, EQUALS, expr) -> Vcont_assign1 (pat unqualified_id1, pat expr)
| TUPLE4 (STRING "expr_primary_parens1", LPAREN, TLIST lst, RPAREN) -> (Vexpr_primary_parens1 (patlst lst))
| TUPLE4 (STRING "expression_in_parens1", LPAREN, cond_expr2, RPAREN) -> (Vexpression_in_parens1 (pat cond_expr2))
| TUPLE3 (STRING "system_tf_call1", SystemTFIdentifier id,
        TUPLE4 (STRING "call_base1", LPAREN, TLIST lst, RPAREN)) -> (Vsystem_tf_call1 (id, patlst lst))
| TUPLE4 (STRING "expression_list_proper1", unqualified_id1, COMMA, expr') -> Vexpression_list_proper1
| TUPLE4 (STRING "gate_instance_or_register_variable1", SymbolIdentifier id,
   EMPTY_TOKEN, EMPTY_TOKEN) -> Vgate_instance_or_register_variable1
| TUPLE4 (STRING "generate_block1", begin1,
  TLIST lst, TUPLE3 (STRING "end1", End, EMPTY_TOKEN)) -> (Vgenerate_block1 (pat begin1, (patlst lst)))
| TUPLE4 (STRING "generate_region1", Generate, TLIST lst, Endgenerate) -> (Vgenerate_region1 (patlst lst))
| TUPLE4 (STRING "module_parameter_port1", Parameter, param,
    TUPLE4 (STRING "trailing_assign1", EQUALS, TK_DecNumber num, EMPTY_TOKEN)) -> Vmodule_parameter_port1
| TUPLE4 (STRING "net_declaration1", Wire,
      TUPLE4 (STRING "net_decl_assign1", SymbolIdentifier id, EQUALS,
        unary_prefix_expr2),
    SEMICOLON) -> Vnet_declaration1
| TUPLE4 (STRING "net_declaration1", Wire, net_variable1, SEMICOLON) -> Vnet_declaration1
| TUPLE4 (STRING "non_anonymous_gate_instance_or_register_variable1",
  SymbolIdentifier id, EMPTY_TOKEN, asgn) -> Vnon_anonymous_gate_instance_or_register_variable1 (id, pat asgn)
| TUPLE3 (STRING "trailing_decl_assignment2", EQUALS, rhs) -> Vtrailing_decl_assignment2
| TUPLE4 (STRING "non_anonymous_gate_instance_or_register_variable1",
      SymbolIdentifier id, dim,
    EMPTY_TOKEN) -> Vnon_anonymous_gate_instance_or_register_variable1 (id, pat dim)
| TUPLE4 (STRING "range_list_in_braces1", LBRACE, TLIST lst, RBRACE) -> (Vrange_list_in_braces1 (patlst lst))
| TUPLE4 (STRING "seq_block1", begin1, EMPTY_TOKEN, TUPLE3 (end1, End, EMPTY_TOKEN)) -> (Vseq_block1 (Vtlist []))
| TUPLE4 (STRING "seq_block1", begin1, TLIST lst, TUPLE3 (end1, End, EMPTY_TOKEN)) -> (Vseq_block1 (patlst lst))
| TUPLE3 (STRING "always_construct1", Always, procedural_timing_control_statement2) -> (Valways_construct1 (pat procedural_timing_control_statement2))
| TUPLE3 (STRING "any_argument_list_item_last2",
      TUPLE3 (STRING "any_argument_list_trailing_comma1", TLIST lst, COMMA),
      TUPLE3 (STRING "reference2", unqualified_id1,
    TUPLE3 (STRING "hierarchy_extension1", DOT, unqualified_id2))) -> (Vany_argument_list_item_last2 (patlst lst))
| TUPLE3 (STRING "any_argument_list_item_last2",
      TUPLE3 (STRING "any_argument_list_trailing_comma1", TLIST lst, COMMA),
   cond_expr2) -> (Vany_argument_list_item_last2 (patlst lst))
| TUPLE3 (STRING "begin1", Begin, EMPTY_TOKEN) -> Vbegin1 Vempty
| TUPLE3 (STRING "bin_based_number1", TK_BinBase base, TK_BinDigits digits) -> Vbin_based_number1 (base, digits)
| TUPLE3 (STRING "block_item_or_statement_or_null6", unqualified_id1, SEMICOLON) -> Vblock_item_or_statement_or_null6
| TUPLE3 (STRING "case_items1", item1, item2) -> Vcase_items1 (pat item1, pat item2)
| TUPLE3 (STRING "data_declaration_or_module_instantiation1",
   instantiation_base1, SEMICOLON) -> Vdata_declaration_or_module_instantiation1 (pat instantiation_base1)
| TUPLE3 (STRING "end1", End, EMPTY_TOKEN) -> Vend1
| TUPLE3 (STRING "event_control4", AT, STAR) -> Vevent_control4
| TUPLE3 (STRING "event_expression1", Posedge, unqualified_id1) -> Vevent_expression_posedge (pat unqualified_id1)
| TUPLE3 (STRING "event_expression1", Negedge, unqualified_id1) -> Vevent_expression_negedge (pat unqualified_id1)
| TUPLE3 (STRING "expression_or_dist1", cond_expr2, EMPTY_TOKEN) -> Vexpression_or_dist1 (pat cond_expr2)
| TUPLE3 (STRING "initial_construct1", Initial, seq_block1) -> Vinitial_construct1 (pat seq_block1)
   | TUPLE3 (STRING "instantiation_base1",
      TUPLE3 (STRING "data_type_primitive1",
        TUPLE3 (STRING "data_type_primitive_scalar1", Reg, EMPTY_TOKEN),
        EMPTY_TOKEN),
   TLIST lst) -> (Vinstantiation_base1_reg (patlst lst))
| TUPLE3 (STRING "instantiation_base1",
      TUPLE3 (STRING "data_type_primitive1",
        TUPLE3 (STRING "data_type_primitive_scalar1", Reg, EMPTY_TOKEN),
        TUPLE6 (STRING "decl_variable_dimension1", LBRACK, TK_DecNumber hi, COLON,
          TK_DecNumber lo, RBRACK)),
    TLIST lst) -> (Vinstantiation_base1 (patlst lst))
| TUPLE3 (STRING "instantiation_base1",
      TUPLE3 (STRING "data_type_primitive1",
        TUPLE3 (STRING "data_type_primitive_scalar1", Reg, EMPTY_TOKEN),
        TUPLE6 (STRING "decl_variable_dimension1", LBRACK, expr', COLON,
          TK_DecNumber lo, RBRACK)),
   TLIST lst) -> (Vinstantiation_base1_reg (patlst lst))
| TUPLE3 (STRING "instantiation_base1", unqualified_id1, TLIST lst) -> (Vinstantiation_base1 (patlst lst))
| TUPLE3 (STRING "ml_start1", TLIST lst, End_of_file) -> (Vml_start1 (patlst lst))
   | TUPLE3 (STRING "module_parameter_port_list_item_last1",
      TUPLE3 (STRING "module_parameter_port_list_trailing_comma1", TLIST lst, COMMA),
      TUPLE4 (STRING "module_parameter_port1", Parameter,
        param_type_followed_by_id_and_dimensions_opt4,
        TUPLE4 (STRING "trailing_assign1", EQUALS,
          hex_based_number1,
       EMPTY_TOKEN))) -> (Vmodule_parameter_port_list_item_last1 (patlst lst))
   | TUPLE3 (STRING "module_parameter_port_list_item_last1",
      TUPLE3 (STRING "module_parameter_port_list_trailing_comma1", TLIST lst, COMMA),
   TUPLE4 (STRING "module_parameter_port2", Parameter, Type, type_assignment1)) -> (Vmodule_parameter_port_list_item_last1 (patlst lst))
| TUPLE3 (STRING "net_variable1", SymbolIdentifier id, EMPTY_TOKEN) -> Vnet_variable1
| TUPLE3 (STRING "parameter_value_byname_list_item_last2",
      TUPLE3 (STRING "parameter_value_byname_list_trailing_comma1", TLIST lst, COMMA),
      TUPLE6 (STRING "parameter_value_byname1", DOT, SymbolIdentifier id, LPAREN,
    TK_DecNumber num, RPAREN)) -> (Vparameter_value_byname_list_item_last2 (patlst lst))
| TUPLE3 (STRING "parameter_value_byname_list_item_last2",
      TUPLE3 (STRING "parameter_value_byname_list_trailing_comma1", TLIST lst, COMMA),
      TUPLE6 (STRING "parameter_value_byname1", DOT, SymbolIdentifier id, LPAREN,
     unqualified_id1, RPAREN)) -> (Vparameter_value_byname_list_item_last2 (patlst lst))
| TUPLE3 (STRING "port1", TUPLE3 (STRING "port_reference1", unqualified_id1, EMPTY_TOKEN), EMPTY_TOKEN) -> Vport1 (pat unqualified_id1)
| TUPLE3 (STRING "procedural_timing_control_statement2", stmt, seq_block) -> (Vprocedural_timing_control_statement2 (pat stmt, pat seq_block))
| TUPLE6 (STRING "select_variable_dimension1", LBRACK, hi, COLON, lo, RBRACK) -> Vselect_variable_dimension1 (pat hi, pat lo)
| TUPLE4 (STRING "select_variable_dimension2", LBRACK, ix, RBRACK) -> Vselect_variable_dimension2 (pat ix)
| TUPLE6 (STRING "select_variable_dimension3", LBRACK, unqualified_id2, PLUS_COLON, unqualified_id3, RBRACK) -> Vselect_variable_dimension3 (pat unqualified_id2, pat unqualified_id3)
| TUPLE3 (STRING "reference_or_call_base1", unqualified_id1,
  TUPLE4 (STRING "call_base1", LPAREN, TLIST lst, RPAREN)) -> (Vreference_or_call_base1 (patlst lst))
| TUPLE3 (STRING "sequence_repetition_expr1", expression_or_dist1,
   EMPTY_TOKEN) -> Vsequence_repetition_expr1 (pat expression_or_dist1)
| TUPLE3 (STRING "statement3", reference, SEMICOLON) -> Vstatement3 (pat reference)
| TUPLE3 (STRING "statement_item6", assignment_statement_no_expr1, SEMICOLON) -> (Vstatement_item6 (pat assignment_statement_no_expr1))
| TUPLE3 (STRING "unary_prefix_expr2", PLUS, reference) -> (Vunary_prefix_expr_plus (pat reference))
| TUPLE3 (STRING "unary_prefix_expr2", VBAR, reference) -> (Vunary_prefix_expr_or (pat reference))
| TUPLE3 (STRING "unary_prefix_expr2", CARET, reference) -> (Vunary_prefix_expr_xor (pat reference))
| TUPLE3 (STRING "unary_prefix_expr2", TILDE_CARET, reference) -> (Vunary_prefix_expr_xnor (pat reference))
| TUPLE3 (STRING "unary_prefix_expr2", TILDE_VBAR, reference) -> (Vunary_prefix_expr_nor (pat reference))
| TUPLE3 (STRING "unary_prefix_expr2", TILDE, reference) -> (Vunary_prefix_expr_tilde (pat reference))
| TUPLE3 (STRING "unary_prefix_expr2", PLING, reference) -> (Vunary_prefix_expr_not (pat reference))
| TUPLE3 (STRING "unary_prefix_expr2", HYPHEN, reference) -> (Vunary_prefix_expr_negate (pat reference))
| TUPLE3 (STRING "unary_prefix_expr2", AMPERSAND, reference) -> (Vunary_prefix_expr_and (pat reference))
| TUPLE3 (STRING "unary_prefix_expr2", TILDE_AMPERSAND, reference) -> (Vunary_prefix_expr_nand (pat reference))
| TUPLE3 (STRING "unqualified_id1", SymbolIdentifier id, param) -> (Vunqualified_id1 (id, pat param))
| TUPLE5 (STRING "parameters2", HASH, LPAREN, TLIST lst, RPAREN) -> (Vparameters2 (patlst lst))
| TUPLE12 (STRING "module_or_interface_declaration1", Module, EMPTY_TOKEN,
      SymbolIdentifier id, EMPTY_TOKEN, EMPTY_TOKEN,
      TUPLE4 (STRING "module_port_list_opt1", LPAREN, TLIST lst, RPAREN),
   EMPTY_TOKEN, SEMICOLON, EMPTY_TOKEN, Endmodule, EMPTY_TOKEN) -> (Vmodule_or_interface_declaration1 (id, patlst lst, Vtlist []))
| TUPLE12 (STRING "module_or_interface_declaration1", Module, EMPTY_TOKEN,
      SymbolIdentifier id, EMPTY_TOKEN, EMPTY_TOKEN,
      TUPLE4 (STRING "module_port_list_opt1", LPAREN, TLIST lst, RPAREN),
   EMPTY_TOKEN, SEMICOLON, TLIST lst', Endmodule, EMPTY_TOKEN) -> (Vmodule_or_interface_declaration1 (id, patlst lst, patlst lst'))
| TUPLE12 (STRING "module_or_interface_declaration1", Module, EMPTY_TOKEN,
      SymbolIdentifier id, EMPTY_TOKEN,
      TUPLE5 (STRING "module_parameter_port_list_opt1", HASH, LPAREN, TLIST lst, RPAREN),
      TUPLE4 (STRING "module_port_list_opt1", LPAREN, TLIST lst', RPAREN),
   EMPTY_TOKEN, SEMICOLON, TLIST lst'', Endmodule, EMPTY_TOKEN) -> (Vmodule_port_list_opt1 (patlst lst, patlst lst', patlst lst''))
| TUPLE6 (STRING "cast1", data_type_primitive1, QUOTE, LPAREN, reference3, RPAREN) -> Vcast1
| TK_DecNumber num -> Vdec_num num
| SymbolIdentifier id -> Vident
| STRING _ -> Vstring
| COMMA -> Vcomma
| LESS -> Vless
| TUPLE6 (STRING "param_type_followed_by_id_and_dimensions_opt4", Logic,
      EMPTY_TOKEN,
      TUPLE3 (STRING "decl_dimensions2",
        TUPLE6 (STRING "decl_variable_dimension1", LBRACK, TK_DecNumber _, COLON,
          TK_DecNumber _, RBRACK),
        TUPLE6 (STRING "decl_variable_dimension1", LBRACK, TK_DecNumber _, COLON,
          TK_DecNumber _, RBRACK)),
   SymbolIdentifier _, EMPTY_TOKEN) -> Vparam_type_followed_by_id_and_dimensions_opt4
| TUPLE6
     (STRING "simple_immediate_assertion_statement1", Assert, LPAREN,
      reference3, RPAREN,
      TUPLE3
       (STRING "action_block3", Else,
        TUPLE3
      (STRING "statement3", reference_or_call_base1, SEMICOLON))) -> Vsimple_immediate_assertion_statement1
| TUPLE5
     (STRING "data_declaration_or_module_instantiation2", Automatic,
   EMPTY_TOKEN, instantiation_base1, SEMICOLON) -> Vdata_declaration_or_module_instantiation2
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
   SEMICOLON) -> Vnet_declaration2
| TUPLE5 (STRING "net_declaration2", Wire,
      TUPLE3 (STRING "data_type_or_implicit1",
        TUPLE6 (STRING "decl_variable_dimension1", LBRACK, TK_DecNumber _, COLON,
          TK_DecNumber _, RBRACK),
        TUPLE3 (STRING "delay31", HASH, TK_RealTime _)),
      TUPLE4 (STRING "net_decl_assign1", SymbolIdentifier _, EQUALS, unqualified_id1),
   SEMICOLON) -> Vnet_declaration2
| TUPLE5
     (STRING "net_declaration4", Wire,
      TUPLE3 (STRING "delay31", HASH, TK_RealTime _),
      TUPLE4
       (STRING "net_decl_assign1", SymbolIdentifier _, EQUALS,
        cond_expr2),
   SEMICOLON) -> Vnet_declaration4
| TUPLE4 (STRING "data_type_or_implicit_basic_followed_by_id_and_dimensions_opt1",
         arg2, arg3, EMPTY_TOKEN) -> Vdata_type_or_implicit_basic_followed_by_id_and_dimensions_opt1 (pat arg2, pat arg3)
| TUPLE4 (STRING "data_type_or_implicit_basic_followed_by_id_and_dimensions_opt4",
        decl_variable_dimension1,
        unqualified_id1, EMPTY_TOKEN) ->
  Vdata_type_or_implicit_basic_followed_by_id_and_dimensions_opt4 (pat decl_variable_dimension1, pat unqualified_id1)
| TUPLE5 (STRING "port_declaration_noattr1", dir, Wire, typ, EMPTY_TOKEN) -> Vport_declaration_noattr1 (pat dir, pat typ)
| TUPLE3 (STRING "type_identifier_or_implicit_basic_followed_by_id_and_dimensions_opt4", unqualified_id1, EMPTY_TOKEN) ->
   Vtype_identifier_or_implicit_basic_followed_by_id_and_dimensions_opt4 (pat unqualified_id1)
| TUPLE5 (STRING "type_identifier_or_implicit_basic_followed_by_id_and_dimensions_opt2",
		 unqualified_id1, EMPTY_TOKEN, unqualified_id2, EMPTY_TOKEN) ->
		 Vtype_identifier_or_implicit_basic_followed_by_id_and_dimensions_opt2 (pat unqualified_id1, pat unqualified_id2)
| TUPLE4 (STRING "generate_block1", begin1, EMPTY_TOKEN,
   TUPLE3 (STRING "end1", End, EMPTY_TOKEN)) -> (Vgenerate_block1 (pat begin1, Vtlist []))
   | TUPLE4 (STRING "jump_statement4", Return, unqualified_id1, SEMICOLON) -> Vjump_statement4
   | TUPLE4 (STRING "module_parameter_port1", Parameter,
      TUPLE6 (STRING "param_type_followed_by_id_and_dimensions_opt5", Int,
        EMPTY_TOKEN, EMPTY_TOKEN, SymbolIdentifier _, EMPTY_TOKEN),
      TUPLE4 (STRING "trailing_assign1", EQUALS, unary_prefix_expr2,
     EMPTY_TOKEN)) -> Vmodule_parameter_port1
   | TUPLE4 (STRING "module_parameter_port1", Parameter,
      param_type_followed_by_id_and_dimensions_opt4,
      TUPLE4 (STRING "trailing_assign1", EQUALS, bin_based_number1,
     EMPTY_TOKEN)) -> Vmodule_parameter_port1
   | TUPLE4 (STRING "module_parameter_port2", Parameter, Type,
      TUPLE4 (STRING "type_assignment1", SymbolIdentifier _, EQUALS,
        TUPLE3 (STRING "data_type_primitive1",
          TUPLE3 (STRING "data_type_primitive_scalar1", Logic, EMPTY_TOKEN),
       EMPTY_TOKEN))) -> Vmodule_parameter_port2
   | TUPLE4 (STRING "port_declaration_noattr4",
      TUPLE6 (STRING "type_identifier_followed_by_id3", unqualified_id1, DOT,
        SymbolIdentifier _, EMPTY_TOKEN, SymbolIdentifier _),
   EMPTY_TOKEN, EMPTY_TOKEN) -> Vport_declaration_noattr4
   | TUPLE4 (STRING "qualified_id2", unqualified_id1, COLON_COLON,
   unqualified_id2) -> Vqualified_id2
   | TUPLE4 (STRING "tf_port_item1", EMPTY_TOKEN,
      TUPLE4 (STRING "data_type_or_implicit_basic_followed_by_id_and_dimensions_opt1",
        TUPLE3 (STRING "data_type_primitive1",
          TUPLE3 (STRING "data_type_primitive_scalar1", Logic, EMPTY_TOKEN),
          TUPLE6 (STRING "decl_variable_dimension1", LBRACK, TK_DecNumber _, COLON,
            TK_DecNumber _, RBRACK)),
        unqualified_id1, EMPTY_TOKEN),
   EMPTY_TOKEN) -> Vtf_port_item1
   | TUPLE4 (STRING "tf_port_item1", EMPTY_TOKEN,
      TUPLE4 (STRING "data_type_or_implicit_basic_followed_by_id_and_dimensions_opt1",
        TUPLE3 (STRING "data_type_primitive1",
          TUPLE3 (STRING "data_type_primitive_scalar5", Byte, EMPTY_TOKEN),
          EMPTY_TOKEN),
        unqualified_id1, EMPTY_TOKEN),
   EMPTY_TOKEN) -> Vtf_port_item1
| TUPLE3 (STRING "always_construct1", Always_latch, seq_block1) -> (Valways_construct1 (pat seq_block1))
| TUPLE3 (STRING "always_construct1", Always_ff,
   procedural_timing_control_statement2) -> (Valways_construct1 (pat procedural_timing_control_statement2))
| TUPLE3 (STRING "always_construct1", Always_comb, seq_block1) -> (Valways_construct1 (pat seq_block1))
| TUPLE3 (STRING "any_port_list_trailing_comma1", TLIST lst, COMMA) -> Vany_port_list_trailing_comma1 (patlst lst)
| TUPLE3 (STRING "any_port_list_item_last1", trailing, named) -> Vany_port_list_item_last1 (pat trailing, pat named)
| TUPLE6 (STRING "port_named1", DOT, SymbolIdentifier id, LPAREN, unqualified_id1, RPAREN) ->  Vport_named1 (id, pat unqualified_id1)
| TUPLE5 (STRING "port_named2", DOT, SymbolIdentifier id, LPAREN, RPAREN) -> Vport_named2 id
| TUPLE3 (STRING "port_named3", DOT, SymbolIdentifier id) -> Vport_named3 id
| TUPLE3 (STRING "label_opt1", COLON, SymbolIdentifier id) -> Vlabel_opt1 id
| TUPLE3 (STRING "begin1", Begin, label) -> Vbegin1 (pat label)
| TUPLE3 (STRING "generate_if1", If, expression_in_parens1) -> Vgenerate_if1 (pat expression_in_parens1)
| TUPLE3 (STRING "conditional_generate_construct2", expr, generate_block1) -> Vconditional_generate_construct2 (pat expr, pat generate_block1)
| TUPLE3 (STRING "data_type_primitive_scalar1", Reg, EMPTY_TOKEN) -> Vdata_type_primitive_scalar1_reg
| TUPLE3 (STRING "data_type_primitive_scalar1", Logic, EMPTY_TOKEN) -> Vdata_type_primitive_scalar1_logic
| TUPLE3 (STRING "data_type_primitive1", prim, dim) -> Vdata_type_primitive1 (pat prim, pat dim)
| TUPLE3 (STRING "dec_based_number1", TK_DecBase b, TK_DecDigits d) -> Vdec_based_number1 (b, d)
| TUPLE3 (STRING "decl_dimensions2", dim1, dim2) -> Vdecl_dimensions2 (pat dim1, pat dim2)
| TUPLE3 (STRING "hex_based_number1", TK_HexBase b, TK_HexDigits d) -> Vhex_based_number1 (b, d)
| TUPLE3 (STRING "inc_or_dec_expression2", unqualified_id1, PLUS_PLUS) -> Vinc_or_dec_expression2
| TUPLE3 (STRING "reference2", unqualified_id1, ext) -> Vreference2 (pat unqualified_id1, pat ext)
| TUPLE3 (STRING "hierarchy_extension1", DOT, unqualified_id2) -> Vhierarchy_extension1 (pat unqualified_id2)
| TUPLE3 (STRING "reference3", unqualified_id1, dim) -> Vreference3 (pat unqualified_id1, pat dim)
| TUPLE12 (STRING "module_or_interface_declaration1", Module, EMPTY_TOKEN,
      SymbolIdentifier id, EMPTY_TOKEN,
      TUPLE5 (STRING "module_parameter_port_list_opt1", HASH, LPAREN, TLIST lst, RPAREN),
      TUPLE4 (STRING "module_port_list_opt1", LPAREN, TLIST lst', RPAREN),
      EMPTY_TOKEN, SEMICOLON, TLIST lst'', Endmodule,
   TUPLE3 (STRING "label_opt1", COLON, SymbolIdentifier _)) -> (Vmodule_or_interface_declaration1 (id, patlst lst, patlst lst'))
| TUPLE12 (STRING "module_or_interface_declaration1", Interface, EMPTY_TOKEN,
      SymbolIdentifier id, EMPTY_TOKEN,
      TUPLE5 (STRING "module_parameter_port_list_opt1", HASH, LPAREN, TLIST lst, RPAREN),
      TUPLE4 (STRING "module_port_list_opt1", LPAREN, TLIST lst', RPAREN),
   EMPTY_TOKEN, SEMICOLON, EMPTY_TOKEN, Endinterface, EMPTY_TOKEN) -> (Vmodule_or_interface_declaration1 (id, patlst lst, patlst lst'))
| TUPLE11 (STRING "function_declaration1", Function, EMPTY_TOKEN,
      TUPLE4
       (STRING "data_type_or_implicit_basic_followed_by_id_and_dimensions_opt5",
        Void, unqualified_id1, EMPTY_TOKEN),
      LPAREN, TLIST _, RPAREN, SEMICOLON, TLIST _, Endfunction,
   TUPLE3 (STRING "label_opt1", COLON, SymbolIdentifier _)) -> Vfunction_declaration1
| TUPLE11 (STRING "function_declaration1", Function, Automatic,
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
   TUPLE3 (STRING "label_opt1", COLON, SymbolIdentifier _)) -> Vfunction_declaration1
| TUPLE10 (STRING "loop_statement1", For, LPAREN,
      TUPLE4 (STRING "for_init_decl_or_assign1", unqualified_id1, EQUALS, TK_DecNumber num),
      SEMICOLON, expr', SEMICOLON, assignment_statement_no_expr1,
   RPAREN, seq_block) -> (Vloop_statement1 (pat expr', pat seq_block))
| TUPLE10 (STRING "loop_statement1", For, LPAREN,
      TUPLE5 (STRING "for_init_decl_or_assign2",
        TUPLE3 (STRING "data_type_primitive1",
          TUPLE3 (STRING "data_type_primitive_scalar5", Int, Unsigned),
          EMPTY_TOKEN),
        SymbolIdentifier _, EQUALS, TK_DecNumber _),
      SEMICOLON, expr', SEMICOLON, inc_or_dec_expression2, RPAREN,
   seq_block) -> (Vloop_statement1 (pat expr', pat seq_block))
| TUPLE4 (STRING "assignment_pattern1", QUOTE_LBRACE, TLIST lst, RBRACE) -> (Vassignment_pattern1 (patlst lst))
| TUPLE3 (STRING "case_inside_items1",
        TUPLE4 (STRING "case_inside_item1", TLIST _, COLON,
          TUPLE4 (STRING "jump_statement4", Return, STRING "bin_based_number1", SEMICOLON)),
        TUPLE4 (STRING "case_inside_item2", Default, COLON,
          TUPLE4 (STRING "jump_statement4", Return, STRING "bin_based_number1", SEMICOLON))) -> Vcase_inside_items1
| TUPLE9 (STRING "case_statement3", Unique, Case, LPAREN, unqualified_id1, RPAREN, Inside, items, Endcase) ->
    Vcase_statement3
| TUPLE9 (STRING "case_statement3", EMPTY_TOKEN, Case, LPAREN, unqualified_id1, RPAREN, Inside, case_inside_items1, Endcase) ->
    Vcase_statement3
| TUPLE4 (STRING "case_inside_item1", TLIST _, COLON, STRING "seq_block1") -> Vcase_inside_item1
| TUPLE4 (STRING "case_inside_item2", Default, COLON, stmt) -> Vcase_inside_item2
| TUPLE8 (STRING "case_statement1", unique, Case, LPAREN, expr, RPAREN, items, Endcase) ->
    Vcase_statement1 (pat unique, pat expr, pat items)
| TUPLE4 (STRING "comp_expr2", expr1, LESS, expr2) -> Vlt_expr (pat expr1, pat expr2)
| TUPLE4 (STRING "comp_expr3", expr1, GREATER, expr2) -> Vgt_expr (pat expr1, pat expr2)
| TUPLE4 (STRING "comp_expr4", expr1, LT_EQ, expr2) -> Vlteq_expr (pat expr1, pat expr2)
| TUPLE4 (STRING "comp_expr5", expr1, GT_EQ, expr2) -> Vgteq_expr (pat expr1, pat expr2)
| TUPLE4 (STRING "logand_expr2", expr1, AMPERSAND_AMPERSAND, expr2) -> Vlogand_expr (pat expr1, pat expr2)
| TUPLE4 (STRING "logor_expr2", expr1, VBAR_VBAR, expr2) -> Vlogor_expr (pat expr1, pat expr2)
| TUPLE4 (STRING "logeq_expr2", expr1, EQ_EQ, expr2) -> Vlogeq_expr (pat expr1, pat expr2)
| TUPLE4 (STRING "logeq_expr3", expr1, PLING_EQ, expr2) -> Vplingeq_expr (pat expr1, pat expr2)
| TUPLE4 (STRING "bitand_expr2", expr1, AMPERSAND, expr2) -> Vbitand_expr (pat expr1, pat expr2)
| TUPLE4 (STRING "bitor_expr2", expr1, VBAR, expr2) -> Vbitor_expr (pat expr1, pat expr2)
| TUPLE4 (STRING "xor_expr2", expr1, CARET, expr2) -> Vxor_expr (pat expr1, pat expr2)
| TUPLE4 (STRING "add_expr2", expr1, PLUS, expr2) -> Vadd_expr (pat expr1, pat expr2)
| TUPLE4 (STRING "add_expr3", expr1, HYPHEN, expr2) -> Vsub_expr (pat expr1, pat expr2)
| TUPLE4 (STRING "mul_expr2", expr1, STAR, expr2) -> Vmul_expr (pat expr1, pat expr2)
| TUPLE4 (STRING "mul_expr3", expr1, SLASH, expr2) -> Vdiv_expr (pat expr1, pat expr2)
| TUPLE4 (STRING "mul_expr4", expr1, PERCENT, expr2) -> Vmod_expr (pat expr1, pat expr2)
| TUPLE4 (STRING "pow_expr2", expr1, STAR_STAR, expr2) -> Vpow_expr (pat expr1, pat expr2)
| TUPLE4 (STRING "shift_expr2", expr1, LT_LT, expr2) -> Vshift_expr2 (pat expr1, pat expr2)
| TUPLE4 (STRING "shift_expr3", expr1, GT_GT, expr2) -> Vshift_expr3 (pat expr1, pat expr2)
| TUPLE4 (STRING "shift_expr4", expr1, GT_GT_GT, expr2) -> Vshift_expr4 (pat expr1, pat expr2)
| TUPLE7 (STRING "module_port_declaration3", dir, signed, dim, EMPTY_TOKEN, TLIST dimlst, SEMICOLON) ->
    Vmodule_port_declaration3 (pat dir, pat signed, pat dim, patlst dimlst)
| TUPLE5 (STRING "module_port_declaration5", dir, signed, TLIST idlst, SEMICOLON) ->
    Vmodule_port_declaration5 (pat dir, pat signed, patlst idlst)
| TUPLE7 (STRING "module_port_declaration7", dir, Reg, EMPTY_TOKEN, dim, TLIST [SymbolIdentifier id], SEMICOLON) ->
    Vmodule_port_declaration7_reg (pat dir, pat dim, id)
| TUPLE3 (STRING "identifier_optional_unpacked_dimensions1", SymbolIdentifier id, EMPTY_TOKEN) ->
Videntifier_optional_unpacked_dimensions1 id
| EMPTY_TOKEN -> Vempty
| Signed -> Vsigned
| Unique -> Vunique
| Input -> Vinput
| Output -> Voutput
| oth -> othpat' := oth; failwith "pat"

and patlst lst = Vtlist (List.map pat lst)

and pat itm = othpatlst := itm :: !othpatlst; let rslt = pat' itm in othpatlst := List.tl !othpatlst; rslt

let rec bin_to_native x = match String.length x with
| 0 -> 0
| 1 -> Char.code (x.[0]) - Char.code '0'
| n -> bin_to_native (String.sub x 0 (n-1)) * 2 + bin_to_native (String.sub x (n-1) 1)

let trandir  = function
| Vinput -> Dump_types.Dinput
| Voutput -> Doutput
| oth -> fail "trandir" oth

let addio (itms:Input_types.itms) (id, itm) = if not (List.mem_assoc id !(itms.io)) then itms.io := (id, itm) :: !(itms.io)

let rec tran (itms:Input_types.itms) modnam = function
| Vml_start1 (Vtlist modlst) -> List.iter (tran itms modnam) modlst 
| Vmodule_or_interface_declaration1 (modnam', Vtlist declst, Vtlist bodylst) ->
    modnam := modnam';
    List.iter (tran itms modnam) (List.rev declst);
    List.iter (tran itms modnam) bodylst
| Vport_declaration_noattr1 (dir, Vdata_type_or_implicit_basic_followed_by_id_and_dimensions_opt1
       (Vdata_type_primitive1 (Vdata_type_primitive_scalar1_reg, typ),
        Vunqualified_id1 (id, Vempty))) ->
    addio itms (id, ("", (BASDTYP, "reg", trantyp typ, []), trandir dir, "wire", []))
| Vport_declaration_noattr1 (dir, Vdata_type_or_implicit_basic_followed_by_id_and_dimensions_opt4
       (Vdecl_variable_dimension1 (hi, lo), Vunqualified_id1 (id, Vempty))) ->
    addio itms (id, ("", (BASDTYP, "reg", TYPRNG (tran'  hi, tran'  lo), []), trandir dir, "wire", []))
| Vport_declaration_noattr1 (dir, Vtype_identifier_or_implicit_basic_followed_by_id_and_dimensions_opt4
       (Vunqualified_id1 (id, Vempty))) ->
    addio itms (id, ("", (BASDTYP, "wire", TYPNONE, []), trandir dir, "wire", []))
| Valways_construct1 (Vprocedural_timing_control_statement2 (Vevent_control2 (Vtlist
             [Vevent_expression_posedge (Vunqualified_id1 (clk, Vempty))]), body)) -> 
    itms.alwys := ("", Input_types.POSEDGE clk, tranlst'' body) :: !(itms.alwys)
| Valways_construct1 ((Vstatement_item6 _|Vcase_statement1 _) as stmt) ->
    itms.alwys := ("", Input_types.COMB, (SNTRE [] :: tran'' stmt :: [])) :: !(itms.alwys)
| Vcomma -> ()
| Vport1 (Vunqualified_id1 (id, Vempty)) -> print_endline ("Vport1: "^id)
| Vmodule_port_declaration7_reg (dir, typ, id) ->
    addio itms (id, ("", (BASDTYP, "reg", trantyp typ, []), trandir dir, "wire", []))
| Vmodule_port_declaration3 (dir, signed, Vdecl_variable_dimension1 (hi, lo), Vtlist iolst) -> List.iter (function
    | Videntifier_optional_unpacked_dimensions1 id -> 
        addio itms (id, ("", (BASDTYP, "reg", TYPRNG (tran'  hi, tran'  lo), transign signed), trandir  dir, "wire", []))
    | oth -> fail "tran iolst" oth) iolst
| Vmodule_port_declaration5 (dir, Vempty, Vtlist lst) ->
    List.iter (function
      | Vempty -> ()
      | Vunqualified_id1 (id, Vempty) -> addio itms (id, ("", (BASDTYP, "logic", TYPNONE, []), trandir dir, "logic", []))
      | oth -> fail "module_port_declaration5" oth) lst
| Vcontinuous_assign1 (Vtlist lst) -> List.iter (function
    | Vcont_assign1 (lhs, rhs) -> itms.ca := ("", tran'' lhs, tran'' rhs) :: !(itms.ca)
    | oth -> fail "cont_assign" oth) lst

| oth -> fail "tran" oth

and transign = function
| Vsigned -> [TYPSIGNED]
| Vempty -> []
| oth -> fail "transign" oth

and trantyp = function
| Vdecl_variable_dimension1 (hi, lo) -> TYPRNG (tran'  hi, tran'  lo)
| Vempty -> TYPNONE
| oth -> fail "trantyp" oth

and tran'  = function
| Vdec_num s -> HEX (int_of_string s)
| oth -> fail "tran'" oth

and tran'' = function
| Vbin_based_number1 (radix, bin) -> Scanf.sscanf radix "%d'b" (fun rad -> Input_types.CNST(rad, HEX (bin_to_native bin)))
| Vdec_based_number1 (radix, bin) -> Scanf.sscanf radix "%d'd" (fun rad -> Input_types.CNST(rad, HEX (int_of_string bin)))
| Vhex_based_number1 (radix, bin) -> Scanf.sscanf (radix^bin) "%d'h%x" (fun rad dig -> Input_types.CNST(rad, HEX dig))
| Vadd_expr (lhs, rhs) -> ARITH (Aadd "", tran'' lhs :: tran'' rhs :: [])
| Vsub_expr (lhs, rhs) -> ARITH (Asub, tran'' lhs :: tran'' rhs :: [])
| Vmul_expr (lhs, rhs) -> ARITH (Amul, tran'' lhs :: tran'' rhs :: [])
| Vdiv_expr (lhs, rhs) -> ARITH (Adiv, tran'' lhs :: tran'' rhs :: [])
| Vmod_expr (lhs, rhs) -> ARITH (Amod, tran'' lhs :: tran'' rhs :: [])
| Vpow_expr (lhs, rhs) -> ARITH (Apow, tran'' lhs :: tran'' rhs :: [])
| Vshift_expr2 (lhs, rhs) -> LOGIC (Lshiftl, tran'' lhs :: tran'' rhs :: [])
| Vshift_expr3 (lhs, rhs) -> LOGIC (Lshiftr, tran'' lhs :: tran'' rhs :: [])
| Vshift_expr4 (lhs, rhs) -> LOGIC (Lshiftrs, tran'' lhs :: tran'' rhs :: [])
| Vlogand_expr (lhs, rhs) -> LOGIC (Land, (LOGIC (Lredor, tran'' lhs :: [])) :: (LOGIC (Lredor, tran'' rhs :: [])) :: [])
| Vbitand_expr (lhs, rhs) -> LOGIC (Land, tran'' lhs :: tran'' rhs :: [])
| Vbitor_expr (lhs, rhs) -> LOGIC (Lor, tran'' lhs :: tran'' rhs :: [])
| Vxor_expr (lhs, rhs) -> LOGIC (Lxor, tran'' lhs :: tran'' rhs :: [])
| Vlogeq_expr (lhs, rhs) -> CMP (Ceq, tran'' lhs :: tran'' rhs :: [])
| Vlt_expr (lhs, rhs) -> CMP (Clt, tran'' lhs :: tran'' rhs :: [])
| Vgt_expr (lhs, rhs) -> CMP (Cgt, tran'' lhs :: tran'' rhs :: [])
| Vgteq_expr (lhs, rhs) -> CMP (Cgte, tran'' lhs :: tran'' rhs :: [])
| Vlteq_expr (lhs, rhs) -> CMP (Clte, tran'' lhs :: tran'' rhs :: [])
| Vplingeq_expr (lhs, rhs) -> CMP (Cneq, tran'' lhs :: tran'' rhs :: [])
| Vunqualified_id1 (id, Vempty) -> VRF (id, (BASDTYP, "wire", TYPNONE, []), [])
| Vconditional_statement2 (Vexpression_in_parens1 cond, then_, else_) ->
  Input_types.IF ("", tran'' cond :: tran'' then_ :: tran'' else_ :: [])
| Vstatement_item6 (Vassignment_statement_no_expr1 (rhs, lhs)) -> ASGN( false, "", tran'' lhs :: tran'' rhs :: [])
| Vcase_statement1 (Vempty, sel, Vtlist itmlst) -> CS ("", tran'' sel :: List.map (tran'' ) itmlst)
| Vcase_item1 (sel, stmt) -> CSITM ("", CNST(32, tran'  sel) :: tran'' stmt :: [])
| Vnonblocking_assignment1 (lhs, rhs) -> ASGN (true, "", tran'' rhs :: tran'' lhs :: [])
| Vunary_prefix_expr_and othtran -> LOGIC(Lredand, tran'' othtran :: [])
| Vunary_prefix_expr_nand othtran -> LOGIC(Lrednand, tran'' othtran :: [])
| Vunary_prefix_expr_or othtran -> LOGIC(Lredor, tran'' othtran :: [])
| Vunary_prefix_expr_nor othtran -> LOGIC(Lrednor, tran'' othtran :: [])
| Vunary_prefix_expr_xor othtran -> LOGIC(Lredxor, tran'' othtran :: [])
| Vunary_prefix_expr_xnor othtran -> LOGIC(Lredxnor, tran'' othtran :: [])
| Vunary_prefix_expr_tilde othtran -> UNRY(Ulognot, tran'' othtran :: [])
| Vunary_prefix_expr_negate othtran -> UNRY(Unegate, tran'' othtran :: [])
| Vunary_prefix_expr_not othtran -> UNRY(Unot, tran'' othtran :: [])
| Vunary_prefix_expr_plus othtran -> tran'' othtran
| Vexpr_primary_parens1 (Vtlist [Vsequence_repetition_expr1 arg]) -> tran'' arg
| Vexpression_or_dist1 othtran -> tran'' othtran
| Vcond_expr (cond, true_, false_) -> CND ("", tran'' cond :: tran'' true_ :: tran'' false_ :: [])
| Vdec_num ix -> Input_types.CNST(32, HEX (int_of_string ix))
| Vreference3 (vector, Vselect_variable_dimension2 ix) -> SEL ("", tran'' vector :: tran'' ix :: tran'' (Vdec_num "1") :: [])
| Vreference3 (vector, Vselect_variable_dimension1 (hi,lo)) ->
  SEL ("", tran'' vector :: tran'' lo :: trancnst (Vsub_expr (Vadd_expr (hi,Vdec_num "1"), lo)) :: [])
| Vrange_list_in_braces1 (Vtlist lst) -> CAT ("", List.map tran'' lst)
| Vexpr_primary_braces2 (Vdec_num repl, reference3) -> let r = tran'' reference3 in CAT ("", List.init (int_of_string repl) (fun _ -> r))
| oth -> fail "tran''" oth

and trancnst = function
| Vdec_num n -> tran'' (Vdec_num n)
| Vadd_expr (Vdec_num lhs, Vdec_num rhs) -> tran'' (Vdec_num (string_of_int (int_of_string lhs + int_of_string rhs)))
| Vadd_expr (lft, rght) -> (match trancnst lft, trancnst rght with
    | CNST (wlhs, HEX lhs), CNST (wrhs, HEX rhs) -> CNST (max wlhs wrhs, HEX (lhs+rhs))
    | lft', rght' -> othcnst := (lft', rght'); failwith "addtrancnst")
| Vsub_expr (Vdec_num lhs, Vdec_num rhs) -> tran'' (Vdec_num (string_of_int (int_of_string lhs - int_of_string rhs)))
| Vsub_expr (lft, rght) -> (match trancnst lft, trancnst rght with
    | CNST (wlhs, HEX lhs), CNST (wrhs, HEX rhs) -> CNST (max wlhs wrhs, HEX (lhs-rhs))
    | lft', rght' -> othcnst := (lft', rght'); failwith "subtrancnst'")
| oth -> fail "trancnst" oth

and tranlst'' = function
| Vseq_block1 (Vtlist lst) -> List.map (tran'' ) lst
| oth -> fail "tranlst''" oth

let cnv' othitms p'' =
let modnam = ref "" in
let uitms = Input_dump.empty_itms [] in
let _ = tran uitms modnam p'' in
othitms := uitms;
let _ = Input_dump.dump' "_check" (!modnam, ((), uitms)) in
let rtl = Input_hardcaml.cnv (!modnam, uitms) in
let fd = open_out "rtl.v" in output_string fd rtl; close_out fd;
(*
let tree = Source_text_rewrite.parse_output_ast_from_string rtl in
othrtl := Some tree;
Input_dump.dump' "_tmp" cnvrted;
*)
(!modnam, uitms)


