Modul ("ariane",
 [PkgImport (Itmlst [PkgImportItm ("ariane_pkg", Atom "*")]);
  PackageParam2 ("ariane_cfg_t", "ArianeCfg", [Id "ariane_pkg"],
   PackageRef ("ariane_pkg", Id "ArianeDefaultConfig"))],
 [Port (PortDir (In, Atom "logic"), "clk_i", [], Deflt);
  Port (PortDir (In, Atom "logic"), "rst_ni", [], Deflt);
  Port (In, "boot_addr_i",
   [AnyRange (Sub (PackageRef ("riscv", Id "VLEN"), Number (10, 32, 1, "")),
     Number (10, 32, 0, ""))],
   Deflt);
  Port (In, "hart_id_i",
   [AnyRange (Sub (PackageRef ("riscv", Id "XLEN"), Number (10, 32, 1, "")),
     Number (10, 32, 0, ""))],
   Deflt);
  Port (In, "irq_i",
   [AnyRange (Number (10, 32, 1, ""), Number (10, 32, 0, ""))], Deflt);
  Port (PortDir (In, Atom "logic"), "ipi_i", [], Deflt);
  Port (PortDir (In, Atom "logic"), "time_irq_i", [], Deflt);
  Port (PortDir (In, Atom "logic"), "debug_req_i", [], Deflt);
  Port (Out, "axi_req_o",
   [Typ2 ("req_t", [PackageRef ("ariane_axi", Atom "::")], [])], Deflt);
  Port (In, "axi_resp_i",
   [Typ2 ("resp_t", [PackageRef ("ariane_axi", Atom "::")], [])], Deflt)],
 [Typ2 ("priv_lvl_t", [PackageRef ("riscv", Atom "::")], [Id "priv_lvl"]);
  Typ3 ("exception_t", [Id "ex_commit"]);
  Typ3 ("bp_resolve_t", [Id "resolved_branch"]);
  DeclLogic2 ([Id "pc_commit"],
   [AnyRange (Sub (PackageRef ("riscv", Id "VLEN"), Number (10, 32, 1, "")),
     Number (10, 32, 0, ""))]);
  DeclLogic [Id "eret"];
  DeclLogic2 ([Id "commit_ack"],
   [AnyRange (Sub (Id "NR_COMMIT_PORTS", Number (10, 32, 1, "")),
     Number (10, 32, 0, ""))]);
  DeclLogic2 ([Id "trap_vector_base_commit_pcgen"],
   [AnyRange (Sub (PackageRef ("riscv", Id "VLEN"), Number (10, 32, 1, "")),
     Number (10, 32, 0, ""))]);
  DeclLogic2 ([Id "epc_commit_pcgen"],
   [AnyRange (Sub (PackageRef ("riscv", Id "VLEN"), Number (10, 32, 1, "")),
     Number (10, 32, 0, ""))]);
  Typ3 ("fetch_entry_t", [Id "fetch_entry_if_id"]);
  DeclLogic [Id "fetch_valid_if_id"]; DeclLogic [Id "fetch_ready_id_if"];
  Typ3 ("scoreboard_entry_t", [Id "issue_entry_id_issue"]);
  DeclLogic [Id "issue_entry_valid_id_issue"];
  DeclLogic [Id "is_ctrl_fow_id_issue"];
  DeclLogic [Id "issue_instr_issue_id"];
  DeclLogic2 ([Id "rs1_forwarding_id_ex"],
   [AnyRange (Sub (PackageRef ("riscv", Id "VLEN"), Number (10, 32, 1, "")),
     Number (10, 32, 0, ""))]);
  DeclLogic2 ([Id "rs2_forwarding_id_ex"],
   [AnyRange (Sub (PackageRef ("riscv", Id "VLEN"), Number (10, 32, 1, "")),
     Number (10, 32, 0, ""))]);
  Typ3 ("fu_data_t", [Id "fu_data_id_ex"]);
  DeclLogic2 ([Id "pc_id_ex"],
   [AnyRange (Sub (PackageRef ("riscv", Id "VLEN"), Number (10, 32, 1, "")),
     Number (10, 32, 0, ""))]);
  DeclLogic [Id "is_compressed_instr_id_ex"];
  DeclLogic [Id "flu_ready_ex_id"];
  DeclLogic2 ([Id "flu_trans_id_ex_id"],
   [AnyRange (Sub (Id "TRANS_ID_BITS", Number (10, 32, 1, "")),
     Number (10, 32, 0, ""))]);
  DeclLogic [Id "flu_valid_ex_id"];
  Typ2 ("xlen_t", [PackageRef ("riscv", Atom "::")], [Id "flu_result_ex_id"]);
  Typ3 ("exception_t", [Id "flu_exception_ex_id"]);
  DeclLogic [Id "alu_valid_id_ex"]; DeclLogic [Id "branch_valid_id_ex"];
  Typ3 ("branchpredict_sbe_t", [Id "branch_predict_id_ex"]);
  DeclLogic [Id "resolve_branch_ex_id"]; DeclLogic [Id "lsu_valid_id_ex"];
  DeclLogic [Id "lsu_ready_ex_id"];
  DeclLogic2 ([Id "load_trans_id_ex_id"],
   [AnyRange (Sub (Id "TRANS_ID_BITS", Number (10, 32, 1, "")),
     Number (10, 32, 0, ""))]);
  Typ2 ("xlen_t", [PackageRef ("riscv", Atom "::")],
   [Id "load_result_ex_id"]);
  DeclLogic [Id "load_valid_ex_id"];
  Typ3 ("exception_t", [Id "load_exception_ex_id"]);
  Typ2 ("xlen_t", [PackageRef ("riscv", Atom "::")],
   [Id "store_result_ex_id"]);
  DeclLogic2 ([Id "store_trans_id_ex_id"],
   [AnyRange (Sub (Id "TRANS_ID_BITS", Number (10, 32, 1, "")),
     Number (10, 32, 0, ""))]);
  DeclLogic [Id "store_valid_ex_id"];
  Typ3 ("exception_t", [Id "store_exception_ex_id"]);
  DeclLogic [Id "mult_valid_id_ex"]; DeclLogic [Id "fpu_ready_ex_id"];
  DeclLogic [Id "fpu_valid_id_ex"];
  DeclLogic2 ([Id "fpu_fmt_id_ex"],
   [AnyRange (Number (10, 32, 1, ""), Number (10, 32, 0, ""))]);
  DeclLogic2 ([Id "fpu_rm_id_ex"],
   [AnyRange (Number (10, 32, 2, ""), Number (10, 32, 0, ""))]);
  DeclLogic2 ([Id "fpu_trans_id_ex_id"],
   [AnyRange (Sub (Id "TRANS_ID_BITS", Number (10, 32, 1, "")),
     Number (10, 32, 0, ""))]);
  Typ2 ("xlen_t", [PackageRef ("riscv", Atom "::")], [Id "fpu_result_ex_id"]);
  DeclLogic [Id "fpu_valid_ex_id"];
  Typ3 ("exception_t", [Id "fpu_exception_ex_id"]);
  DeclLogic [Id "csr_valid_id_ex"]; DeclLogic [Id "csr_commit_commit_ex"];
  DeclLogic [Id "dirty_fp_state"]; DeclLogic [Id "lsu_commit_commit_ex"];
  DeclLogic [Id "lsu_commit_ready_ex_commit"];
  DeclLogic2 ([Id "lsu_commit_trans_id"],
   [AnyRange (Sub (Id "TRANS_ID_BITS", Number (10, 32, 1, "")),
     Number (10, 32, 0, ""))]);
  DeclLogic [Id "no_st_pending_ex"]; DeclLogic [Id "no_st_pending_commit"];
  DeclLogic [Id "amo_valid_commit"];
  Typ2 ("scoreboard_entry_t",
   [AnyRange (Sub (Id "NR_COMMIT_PORTS", Number (10, 32, 1, "")),
     Number (10, 32, 0, ""))],
   [Id "commit_instr_id_commit"]);
  DeclLogic2 ([Id "waddr_commit_id"],
   [AnyRange (Sub (Id "NR_COMMIT_PORTS", Number (10, 32, 1, "")),
     Number (10, 32, 0, ""));
    AnyRange (Number (10, 32, 4, ""), Number (10, 32, 0, ""))]);
  DeclLogic2 ([Id "wdata_commit_id"],
   [AnyRange (Sub (Id "NR_COMMIT_PORTS", Number (10, 32, 1, "")),
     Number (10, 32, 0, ""));
    AnyRange (Sub (PackageRef ("riscv", Id "XLEN"), Number (10, 32, 1, "")),
     Number (10, 32, 0, ""))]);
  DeclLogic2 ([Id "we_gpr_commit_id"],
   [AnyRange (Sub (Id "NR_COMMIT_PORTS", Number (10, 32, 1, "")),
     Number (10, 32, 0, ""))]);
  DeclLogic2 ([Id "we_fpr_commit_id"],
   [AnyRange (Sub (Id "NR_COMMIT_PORTS", Number (10, 32, 1, "")),
     Number (10, 32, 0, ""))]);
  DeclLogic2 ([Id "fflags_csr_commit"],
   [AnyRange (Number (10, 32, 4, ""), Number (10, 32, 0, ""))]);
  Typ2 ("xs_t", [PackageRef ("riscv", Atom "::")], [Id "fs"]);
  DeclLogic2 ([Id "frm_csr_id_issue_ex"],
   [AnyRange (Number (10, 32, 2, ""), Number (10, 32, 0, ""))]);
  DeclLogic2 ([Id "fprec_csr_ex"],
   [AnyRange (Number (10, 32, 6, ""), Number (10, 32, 0, ""))]);
  DeclLogic [Id "enable_translation_csr_ex"];
  DeclLogic [Id "en_ld_st_translation_csr_ex"];
  Typ2 ("priv_lvl_t", [PackageRef ("riscv", Atom "::")],
   [Id "ld_st_priv_lvl_csr_ex"]);
  DeclLogic [Id "sum_csr_ex"]; DeclLogic [Id "mxr_csr_ex"];
  DeclLogic2 ([Id "satp_ppn_csr_ex"],
   [AnyRange (Sub (PackageRef ("riscv", Id "PPNW"), Number (10, 32, 1, "")),
     Number (10, 32, 0, ""))]);
  DeclLogic2 ([Id "asid_csr_ex"],
   [AnyRange (Sub (Id "ASID_WIDTH", Number (10, 32, 1, "")),
     Number (10, 32, 0, ""))]);
  DeclLogic2 ([Id "csr_addr_ex_csr"],
   [AnyRange (Number (10, 32, 11, "1"), Number (10, 32, 0, ""))]);
  Typ3 ("fu_op", [Id "csr_op_commit_csr"]);
  Typ2 ("xlen_t", [PackageRef ("riscv", Atom "::")],
   [Id "csr_wdata_commit_csr"]);
  Typ2 ("xlen_t", [PackageRef ("riscv", Atom "::")],
   [Id "csr_rdata_csr_commit"]);
  Typ3 ("exception_t", [Id "csr_exception_csr_commit"]);
  DeclLogic [Id "tvm_csr_id"]; DeclLogic [Id "tw_csr_id"];
  DeclLogic [Id "tsr_csr_id"]; Typ3 ("irq_ctrl_t", [Id "irq_ctrl_csr_id"]);
  DeclLogic [Id "dcache_en_csr_nbdcache"];
  DeclLogic [Id "csr_write_fflags_commit_cs"];
  DeclLogic [Id "icache_en_csr"]; DeclLogic [Id "debug_mode"];
  DeclLogic [Id "single_step_csr_commit"];
  Typ4 ("pmpcfg_t", [PackageRef ("riscv", Atom "::")],
   [AnyRange (Number (10, 32, 15, "5"), Number (10, 32, 0, ""))],
   [Id "pmpcfg"]);
  DeclLogic2 ([Id "pmpaddr"],
   [AnyRange (Number (10, 32, 15, "5"), Number (10, 32, 0, ""));
    AnyRange (Sub (PackageRef ("riscv", Id "PLEN"), Number (10, 32, 3, "")),
     Number (10, 32, 0, ""))]);
  DeclLogic2 ([Id "addr_csr_perf"],
   [AnyRange (Number (10, 32, 4, ""), Number (10, 32, 0, ""))]);
  Typ2 ("xlen_t", [PackageRef ("riscv", Atom "::")],
   [Id "data_csr_perf"; Id "data_perf_csr"]);
  DeclLogic [Id "we_csr_perf"]; DeclLogic [Id "icache_flush_ctrl_cache"];
  DeclLogic [Id "itlb_miss_ex_perf"]; DeclLogic [Id "dtlb_miss_ex_perf"];
  DeclLogic [Id "dcache_miss_cache_perf"];
  DeclLogic [Id "icache_miss_cache_perf"];
  DeclLogic [Id "set_pc_ctrl_pcgen"]; DeclLogic [Id "flush_csr_ctrl"];
  DeclLogic [Id "flush_unissued_instr_ctrl_id"];
  DeclLogic [Id "flush_ctrl_if"]; DeclLogic [Id "flush_ctrl_id"];
  DeclLogic [Id "flush_ctrl_ex"]; DeclLogic [Id "flush_ctrl_bp"];
  DeclLogic [Id "flush_tlb_ctrl_ex"];
  DeclLogic [Id "fence_i_commit_controller"];
  DeclLogic [Id "fence_commit_controller"];
  DeclLogic [Id "sfence_vma_commit_controller"]; DeclLogic [Id "halt_ctrl"];
  DeclLogic [Id "halt_csr_ctrl"]; DeclLogic [Id "dcache_flush_ctrl_cache"];
  DeclLogic [Id "dcache_flush_ack_cache_ctrl"];
  DeclLogic [Id "set_debug_pc"]; DeclLogic [Id "flush_commit"];
  Typ3 ("icache_areq_i_t", [Id "icache_areq_ex_cache"]);
  Typ3 ("icache_areq_o_t", [Id "icache_areq_cache_ex"]);
  Typ3 ("icache_dreq_i_t", [Id "icache_dreq_if_cache"]);
  Typ3 ("icache_dreq_o_t", [Id "icache_dreq_cache_if"]);
  Typ3 ("amo_req_t", [Id "amo_req"]); Typ3 ("amo_resp_t", [Id "amo_resp"]);
  DeclLogic [Id "sb_full"];
  Typ2 ("dcache_req_i_t",
   [AnyRange (Number (10, 32, 2, ""), Number (10, 32, 0, ""))],
   [Id "dcache_req_ports_ex_cache"]);
  Typ2 ("dcache_req_o_t",
   [AnyRange (Number (10, 32, 2, ""), Number (10, 32, 0, ""))],
   [Id "dcache_req_ports_cache_ex"]);
  DeclLogic [Id "dcache_commit_wbuffer_empty"];
  DeclLogic [Id "dcache_commit_wbuffer_not_ni"];
  InstDecl (Id "frontend",
   [Itmlst [CellParamItem2 ("ArianeCfg", Id "ArianeCfg")]],
   [InstNameParen1 ("i_frontend",
     [Itmlst
       [CellPinItem2 ("flush_i", Id "flush_ctrl_if");
        CellPinItem2 ("flush_bp_i", Number (2, 1, 0, "0"));
        CellPinItem2 ("debug_mode_i", Id "debug_mode");
        CellPinItem2 ("boot_addr_i",
         IdArrayedColon (Id "boot_addr_i",
          Sub (PackageRef ("riscv", Id "VLEN"), Number (10, 32, 1, "")),
          Number (10, 32, 0, "")));
        CellPinItem2 ("icache_dreq_i", Id "icache_dreq_cache_if");
        CellPinItem2 ("icache_dreq_o", Id "icache_dreq_if_cache");
        CellPinItem2 ("resolved_branch_i", Id "resolved_branch");
        CellPinItem2 ("pc_commit_i", Id "pc_commit");
        CellPinItem2 ("set_pc_commit_i", Id "set_pc_ctrl_pcgen");
        CellPinItem2 ("set_debug_pc_i", Id "set_debug_pc");
        CellPinItem2 ("epc_i", Id "epc_commit_pcgen");
        CellPinItem2 ("eret_i", Id "eret");
        CellPinItem2 ("trap_vector_base_i",
         Id "trap_vector_base_commit_pcgen");
        CellPinItem2 ("ex_valid_i", Dot1 (Id "ex_commit", Id "valid"));
        CellPinItem2 ("fetch_entry_o", Id "fetch_entry_if_id");
        CellPinItem2 ("fetch_entry_valid_o", Id "fetch_valid_if_id");
        CellPinItem2 ("fetch_entry_ready_i", Id "fetch_ready_id_if");
        Atom ".*"]])]);
  InstDecl (Id "id_stage", [],
   [InstNameParen1 ("id_stage_i",
     [Itmlst
       [CellPinItemImplied "clk_i"; CellPinItemImplied "rst_ni";
        CellPinItem2 ("flush_i", Id "flush_ctrl_if");
        CellPinItemImplied "debug_req_i";
        CellPinItem2 ("fetch_entry_i", Id "fetch_entry_if_id");
        CellPinItem2 ("fetch_entry_valid_i", Id "fetch_valid_if_id");
        CellPinItem2 ("fetch_entry_ready_o", Id "fetch_ready_id_if");
        CellPinItem2 ("issue_entry_o", Id "issue_entry_id_issue");
        CellPinItem2 ("issue_entry_valid_o", Id "issue_entry_valid_id_issue");
        CellPinItem2 ("is_ctrl_flow_o", Id "is_ctrl_fow_id_issue");
        CellPinItem2 ("issue_instr_ack_i", Id "issue_instr_issue_id");
        CellPinItem2 ("priv_lvl_i", Id "priv_lvl");
        CellPinItem2 ("fs_i", Id "fs");
        CellPinItem2 ("frm_i", Id "frm_csr_id_issue_ex");
        CellPinItem2 ("irq_i", Id "irq_i");
        CellPinItem2 ("irq_ctrl_i", Id "irq_ctrl_csr_id");
        CellPinItem2 ("debug_mode_i", Id "debug_mode");
        CellPinItem2 ("tvm_i", Id "tvm_csr_id");
        CellPinItem2 ("tw_i", Id "tw_csr_id");
        CellPinItem2 ("tsr_i", Id "tsr_csr_id")]])]);
  InstDecl (Id "issue_stage",
   [Itmlst
     [CellParamItem2 ("NR_ENTRIES", Id "NR_SB_ENTRIES");
      CellParamItem2 ("NR_WB_PORTS", Id "NR_WB_PORTS");
      CellParamItem2 ("NR_COMMIT_PORTS", Id "NR_COMMIT_PORTS")]],
   [InstNameParen1 ("issue_stage_i",
     [Itmlst
       [CellPinItemImplied "clk_i"; CellPinItemImplied "rst_ni";
        CellPinItem2 ("sb_full_o", Id "sb_full");
        CellPinItem2 ("flush_unissued_instr_i",
         Id "flush_unissued_instr_ctrl_id");
        CellPinItem2 ("flush_i", Id "flush_ctrl_id");
        CellPinItem2 ("decoded_instr_i", Id "issue_entry_id_issue");
        CellPinItem2 ("decoded_instr_valid_i",
         Id "issue_entry_valid_id_issue");
        CellPinItem2 ("is_ctrl_flow_i", Id "is_ctrl_fow_id_issue");
        CellPinItem2 ("decoded_instr_ack_o", Id "issue_instr_issue_id");
        CellPinItem2 ("rs1_forwarding_o", Id "rs1_forwarding_id_ex");
        CellPinItem2 ("rs2_forwarding_o", Id "rs2_forwarding_id_ex");
        CellPinItem2 ("fu_data_o", Id "fu_data_id_ex");
        CellPinItem2 ("pc_o", Id "pc_id_ex");
        CellPinItem2 ("is_compressed_instr_o",
         Id "is_compressed_instr_id_ex");
        CellPinItem2 ("flu_ready_i", Id "flu_ready_ex_id");
        CellPinItem2 ("alu_valid_o", Id "alu_valid_id_ex");
        CellPinItem2 ("branch_valid_o", Id "branch_valid_id_ex");
        CellPinItem2 ("branch_predict_o", Id "branch_predict_id_ex");
        CellPinItem2 ("resolve_branch_i", Id "resolve_branch_ex_id");
        CellPinItem2 ("lsu_ready_i", Id "lsu_ready_ex_id");
        CellPinItem2 ("lsu_valid_o", Id "lsu_valid_id_ex");
        CellPinItem2 ("mult_valid_o", Id "mult_valid_id_ex");
        CellPinItem2 ("fpu_ready_i", Id "fpu_ready_ex_id");
        CellPinItem2 ("fpu_valid_o", Id "fpu_valid_id_ex");
        CellPinItem2 ("fpu_fmt_o", Id "fpu_fmt_id_ex");
        CellPinItem2 ("fpu_rm_o", Id "fpu_rm_id_ex");
        CellPinItem2 ("csr_valid_o", Id "csr_valid_id_ex");
        CellPinItem2 ("resolved_branch_i", Id "resolved_branch");
        CellPinItem2 ("trans_id_i",
         ExprOKL
          [Id "flu_trans_id_ex_id"; Id "load_trans_id_ex_id";
           Id "store_trans_id_ex_id"; Id "fpu_trans_id_ex_id"]);
        CellPinItem2 ("wbdata_i",
         ExprOKL
          [Id "flu_result_ex_id"; Id "load_result_ex_id";
           Id "store_result_ex_id"; Id "fpu_result_ex_id"]);
        CellPinItem2 ("ex_ex_i",
         ExprOKL
          [Id "flu_exception_ex_id"; Id "load_exception_ex_id";
           Id "store_exception_ex_id"; Id "fpu_exception_ex_id"]);
        CellPinItem2 ("wt_valid_i",
         ExprOKL
          [Id "flu_valid_ex_id"; Id "load_valid_ex_id";
           Id "store_valid_ex_id"; Id "fpu_valid_ex_id"]);
        CellPinItem2 ("waddr_i", Id "waddr_commit_id");
        CellPinItem2 ("wdata_i", Id "wdata_commit_id");
        CellPinItem2 ("we_gpr_i", Id "we_gpr_commit_id");
        CellPinItem2 ("we_fpr_i", Id "we_fpr_commit_id");
        CellPinItem2 ("commit_instr_o", Id "commit_instr_id_commit");
        CellPinItem2 ("commit_ack_i", Id "commit_ack"); Atom ".*"]])]);
  InstDecl (Id "ex_stage",
   [Itmlst
     [CellParamItem2 ("ASID_WIDTH", Id "ASID_WIDTH");
      CellParamItem2 ("ArianeCfg", Id "ArianeCfg")]],
   [InstNameParen1 ("ex_stage_i",
     [Itmlst
       [CellPinItem2 ("clk_i", Id "clk_i");
        CellPinItem2 ("rst_ni", Id "rst_ni");
        CellPinItem2 ("debug_mode_i", Id "debug_mode");
        CellPinItem2 ("flush_i", Id "flush_ctrl_ex");
        CellPinItem2 ("rs1_forwarding_i", Id "rs1_forwarding_id_ex");
        CellPinItem2 ("rs2_forwarding_i", Id "rs2_forwarding_id_ex");
        CellPinItem2 ("fu_data_i", Id "fu_data_id_ex");
        CellPinItem2 ("pc_i", Id "pc_id_ex");
        CellPinItem2 ("is_compressed_instr_i",
         Id "is_compressed_instr_id_ex");
        CellPinItem2 ("flu_result_o", Id "flu_result_ex_id");
        CellPinItem2 ("flu_trans_id_o", Id "flu_trans_id_ex_id");
        CellPinItem2 ("flu_valid_o", Id "flu_valid_ex_id");
        CellPinItem2 ("flu_exception_o", Id "flu_exception_ex_id");
        CellPinItem2 ("flu_ready_o", Id "flu_ready_ex_id");
        CellPinItem2 ("alu_valid_i", Id "alu_valid_id_ex");
        CellPinItem2 ("branch_valid_i", Id "branch_valid_id_ex");
        CellPinItem2 ("branch_predict_i", Id "branch_predict_id_ex");
        CellPinItem2 ("resolved_branch_o", Id "resolved_branch");
        CellPinItem2 ("resolve_branch_o", Id "resolve_branch_ex_id");
        CellPinItem2 ("csr_valid_i", Id "csr_valid_id_ex");
        CellPinItem2 ("csr_addr_o", Id "csr_addr_ex_csr");
        CellPinItem2 ("csr_commit_i", Id "csr_commit_commit_ex");
        CellPinItem2 ("mult_valid_i", Id "mult_valid_id_ex");
        CellPinItem2 ("lsu_ready_o", Id "lsu_ready_ex_id");
        CellPinItem2 ("lsu_valid_i", Id "lsu_valid_id_ex");
        CellPinItem2 ("load_result_o", Id "load_result_ex_id");
        CellPinItem2 ("load_trans_id_o", Id "load_trans_id_ex_id");
        CellPinItem2 ("load_valid_o", Id "load_valid_ex_id");
        CellPinItem2 ("load_exception_o", Id "load_exception_ex_id");
        CellPinItem2 ("store_result_o", Id "store_result_ex_id");
        CellPinItem2 ("store_trans_id_o", Id "store_trans_id_ex_id");
        CellPinItem2 ("store_valid_o", Id "store_valid_ex_id");
        CellPinItem2 ("store_exception_o", Id "store_exception_ex_id");
        CellPinItem2 ("lsu_commit_i", Id "lsu_commit_commit_ex");
        CellPinItem2 ("lsu_commit_ready_o", Id "lsu_commit_ready_ex_commit");
        CellPinItem2 ("commit_tran_id_i", Id "lsu_commit_trans_id");
        CellPinItem2 ("no_st_pending_o", Id "no_st_pending_ex");
        CellPinItem2 ("fpu_ready_o", Id "fpu_ready_ex_id");
        CellPinItem2 ("fpu_valid_i", Id "fpu_valid_id_ex");
        CellPinItem2 ("fpu_fmt_i", Id "fpu_fmt_id_ex");
        CellPinItem2 ("fpu_rm_i", Id "fpu_rm_id_ex");
        CellPinItem2 ("fpu_frm_i", Id "frm_csr_id_issue_ex");
        CellPinItem2 ("fpu_prec_i", Id "fprec_csr_ex");
        CellPinItem2 ("fpu_trans_id_o", Id "fpu_trans_id_ex_id");
        CellPinItem2 ("fpu_result_o", Id "fpu_result_ex_id");
        CellPinItem2 ("fpu_valid_o", Id "fpu_valid_ex_id");
        CellPinItem2 ("fpu_exception_o", Id "fpu_exception_ex_id");
        CellPinItem2 ("amo_valid_commit_i", Id "amo_valid_commit");
        CellPinItem2 ("amo_req_o", Id "amo_req");
        CellPinItem2 ("amo_resp_i", Id "amo_resp");
        CellPinItem2 ("itlb_miss_o", Id "itlb_miss_ex_perf");
        CellPinItem2 ("dtlb_miss_o", Id "dtlb_miss_ex_perf");
        CellPinItem2 ("enable_translation_i", Id "enable_translation_csr_ex");
        CellPinItem2 ("en_ld_st_translation_i",
         Id "en_ld_st_translation_csr_ex");
        CellPinItem2 ("flush_tlb_i", Id "flush_tlb_ctrl_ex");
        CellPinItem2 ("priv_lvl_i", Id "priv_lvl");
        CellPinItem2 ("ld_st_priv_lvl_i", Id "ld_st_priv_lvl_csr_ex");
        CellPinItem2 ("sum_i", Id "sum_csr_ex");
        CellPinItem2 ("mxr_i", Id "mxr_csr_ex");
        CellPinItem2 ("satp_ppn_i", Id "satp_ppn_csr_ex");
        CellPinItem2 ("asid_i", Id "asid_csr_ex");
        CellPinItem2 ("icache_areq_i", Id "icache_areq_cache_ex");
        CellPinItem2 ("icache_areq_o", Id "icache_areq_ex_cache");
        CellPinItem2 ("dcache_req_ports_i", Id "dcache_req_ports_cache_ex");
        CellPinItem2 ("dcache_req_ports_o", Id "dcache_req_ports_ex_cache");
        CellPinItem2 ("dcache_wbuffer_empty_i",
         Id "dcache_commit_wbuffer_empty");
        CellPinItem2 ("dcache_wbuffer_not_ni_i",
         Id "dcache_commit_wbuffer_not_ni");
        CellPinItem2 ("pmpcfg_i", Id "pmpcfg");
        CellPinItem2 ("pmpaddr_i", Id "pmpaddr")]])]);
  ContAsgn
   [Asgn1 (Id "no_st_pending_commit",
     Source_text_rewrite_types.And (Id "no_st_pending_ex",
      Id "dcache_commit_wbuffer_empty"))];
  InstDecl (Id "commit_stage",
   [Itmlst [CellParamItem2 ("NR_COMMIT_PORTS", Id "NR_COMMIT_PORTS")]],
   [InstNameParen1 ("commit_stage_i",
     [Itmlst
       [CellPinItemImplied "clk_i"; CellPinItemImplied "rst_ni";
        CellPinItem2 ("halt_i", Id "halt_ctrl");
        CellPinItem2 ("flush_dcache_i", Id "dcache_flush_ctrl_cache");
        CellPinItem2 ("exception_o", Id "ex_commit");
        CellPinItem2 ("dirty_fp_state_o", Id "dirty_fp_state");
        CellPinItem2 ("single_step_i", Id "single_step_csr_commit");
        CellPinItem2 ("commit_instr_i", Id "commit_instr_id_commit");
        CellPinItem2 ("commit_ack_o", Id "commit_ack");
        CellPinItem2 ("no_st_pending_i", Id "no_st_pending_commit");
        CellPinItem2 ("waddr_o", Id "waddr_commit_id");
        CellPinItem2 ("wdata_o", Id "wdata_commit_id");
        CellPinItem2 ("we_gpr_o", Id "we_gpr_commit_id");
        CellPinItem2 ("we_fpr_o", Id "we_fpr_commit_id");
        CellPinItem2 ("commit_lsu_o", Id "lsu_commit_commit_ex");
        CellPinItem2 ("commit_lsu_ready_i", Id "lsu_commit_ready_ex_commit");
        CellPinItem2 ("commit_tran_id_o", Id "lsu_commit_trans_id");
        CellPinItem2 ("amo_valid_commit_o", Id "amo_valid_commit");
        CellPinItem2 ("amo_resp_i", Id "amo_resp");
        CellPinItem2 ("commit_csr_o", Id "csr_commit_commit_ex");
        CellPinItem2 ("pc_o", Id "pc_commit");
        CellPinItem2 ("csr_op_o", Id "csr_op_commit_csr");
        CellPinItem2 ("csr_wdata_o", Id "csr_wdata_commit_csr");
        CellPinItem2 ("csr_rdata_i", Id "csr_rdata_csr_commit");
        CellPinItem2 ("csr_write_fflags_o", Id "csr_write_fflags_commit_cs");
        CellPinItem2 ("csr_exception_i", Id "csr_exception_csr_commit");
        CellPinItem2 ("fence_i_o", Id "fence_i_commit_controller");
        CellPinItem2 ("fence_o", Id "fence_commit_controller");
        CellPinItem2 ("sfence_vma_o", Id "sfence_vma_commit_controller");
        CellPinItem2 ("flush_commit_o", Id "flush_commit"); Atom ".*"]])]);
  InstDecl (Id "csr_regfile",
   [Itmlst
     [CellParamItem2 ("AsidWidth", Id "ASID_WIDTH");
      CellParamItem2 ("DmBaseAddress",
       Dot1 (Id "ArianeCfg", Id "DmBaseAddress"));
      CellParamItem2 ("NrCommitPorts", Id "NR_COMMIT_PORTS");
      CellParamItem2 ("NrPMPEntries",
       Dot1 (Id "ArianeCfg", Id "NrPMPEntries"))]],
   [InstNameParen1 ("csr_regfile_i",
     [Itmlst
       [CellPinItem2 ("flush_o", Id "flush_csr_ctrl");
        CellPinItem2 ("halt_csr_o", Id "halt_csr_ctrl");
        CellPinItem2 ("commit_instr_i", Id "commit_instr_id_commit");
        CellPinItem2 ("commit_ack_i", Id "commit_ack");
        CellPinItem2 ("boot_addr_i",
         IdArrayedColon (Id "boot_addr_i",
          Sub (PackageRef ("riscv", Id "VLEN"), Number (10, 32, 1, "")),
          Number (10, 32, 0, "")));
        CellPinItem2 ("hart_id_i",
         IdArrayedColon (Id "hart_id_i",
          Sub (PackageRef ("riscv", Id "XLEN"), Number (10, 32, 1, "")),
          Number (10, 32, 0, "")));
        CellPinItem2 ("ex_i", Id "ex_commit");
        CellPinItem2 ("csr_op_i", Id "csr_op_commit_csr");
        CellPinItem2 ("csr_write_fflags_i", Id "csr_write_fflags_commit_cs");
        CellPinItem2 ("dirty_fp_state_i", Id "dirty_fp_state");
        CellPinItem2 ("csr_addr_i", Id "csr_addr_ex_csr");
        CellPinItem2 ("csr_wdata_i", Id "csr_wdata_commit_csr");
        CellPinItem2 ("csr_rdata_o", Id "csr_rdata_csr_commit");
        CellPinItem2 ("pc_i", Id "pc_commit");
        CellPinItem2 ("csr_exception_o", Id "csr_exception_csr_commit");
        CellPinItem2 ("epc_o", Id "epc_commit_pcgen");
        CellPinItem2 ("eret_o", Id "eret");
        CellPinItem2 ("set_debug_pc_o", Id "set_debug_pc");
        CellPinItem2 ("trap_vector_base_o",
         Id "trap_vector_base_commit_pcgen");
        CellPinItem2 ("priv_lvl_o", Id "priv_lvl");
        CellPinItem2 ("fs_o", Id "fs");
        CellPinItem2 ("fflags_o", Id "fflags_csr_commit");
        CellPinItem2 ("frm_o", Id "frm_csr_id_issue_ex");
        CellPinItem2 ("fprec_o", Id "fprec_csr_ex");
        CellPinItem2 ("irq_ctrl_o", Id "irq_ctrl_csr_id");
        CellPinItem2 ("ld_st_priv_lvl_o", Id "ld_st_priv_lvl_csr_ex");
        CellPinItem2 ("en_translation_o", Id "enable_translation_csr_ex");
        CellPinItem2 ("en_ld_st_translation_o",
         Id "en_ld_st_translation_csr_ex");
        CellPinItem2 ("sum_o", Id "sum_csr_ex");
        CellPinItem2 ("mxr_o", Id "mxr_csr_ex");
        CellPinItem2 ("satp_ppn_o", Id "satp_ppn_csr_ex");
        CellPinItem2 ("asid_o", Id "asid_csr_ex");
        CellPinItem2 ("tvm_o", Id "tvm_csr_id");
        CellPinItem2 ("tw_o", Id "tw_csr_id");
        CellPinItem2 ("tsr_o", Id "tsr_csr_id");
        CellPinItem2 ("debug_mode_o", Id "debug_mode");
        CellPinItem2 ("single_step_o", Id "single_step_csr_commit");
        CellPinItem2 ("dcache_en_o", Id "dcache_en_csr_nbdcache");
        CellPinItem2 ("icache_en_o", Id "icache_en_csr");
        CellPinItem2 ("perf_addr_o", Id "addr_csr_perf");
        CellPinItem2 ("perf_data_o", Id "data_csr_perf");
        CellPinItem2 ("perf_data_i", Id "data_perf_csr");
        CellPinItem2 ("perf_we_o", Id "we_csr_perf");
        CellPinItem2 ("pmpcfg_o", Id "pmpcfg");
        CellPinItem2 ("pmpaddr_o", Id "pmpaddr");
        CellPinItemImplied "debug_req_i"; CellPinItemImplied "ipi_i";
        CellPinItemImplied "irq_i"; CellPinItemImplied "time_irq_i";
        Atom ".*"]])]);
  InstDecl (Id "perf_counters", [],
   [InstNameParen1 ("i_perf_counters",
     [Itmlst
       [CellPinItem2 ("clk_i", Id "clk_i");
        CellPinItem2 ("rst_ni", Id "rst_ni");
        CellPinItem2 ("debug_mode_i", Id "debug_mode");
        CellPinItem2 ("addr_i", Id "addr_csr_perf");
        CellPinItem2 ("we_i", Id "we_csr_perf");
        CellPinItem2 ("data_i", Id "data_csr_perf");
        CellPinItem2 ("data_o", Id "data_perf_csr");
        CellPinItem2 ("commit_instr_i", Id "commit_instr_id_commit");
        CellPinItem2 ("commit_ack_i", Id "commit_ack");
        CellPinItem2 ("l1_icache_miss_i", Id "icache_miss_cache_perf");
        CellPinItem2 ("l1_dcache_miss_i", Id "dcache_miss_cache_perf");
        CellPinItem2 ("itlb_miss_i", Id "itlb_miss_ex_perf");
        CellPinItem2 ("dtlb_miss_i", Id "dtlb_miss_ex_perf");
        CellPinItem2 ("sb_full_i", Id "sb_full");
        CellPinItem2 ("if_empty_i", Tilde (Id "fetch_valid_if_id"));
        CellPinItem2 ("ex_i", Id "ex_commit");
        CellPinItem2 ("eret_i", Id "eret");
        CellPinItem2 ("resolved_branch_i", Id "resolved_branch")]])]);
  InstDecl (Id "controller", [],
   [InstNameParen1 ("controller_i",
     [Itmlst
       [CellPinItem2 ("set_pc_commit_o", Id "set_pc_ctrl_pcgen");
        CellPinItem2 ("flush_unissued_instr_o",
         Id "flush_unissued_instr_ctrl_id");
        CellPinItem2 ("flush_if_o", Id "flush_ctrl_if");
        CellPinItem2 ("flush_id_o", Id "flush_ctrl_id");
        CellPinItem2 ("flush_ex_o", Id "flush_ctrl_ex");
        CellPinItem2 ("flush_bp_o", Id "flush_ctrl_bp");
        CellPinItem2 ("flush_tlb_o", Id "flush_tlb_ctrl_ex");
        CellPinItem2 ("flush_dcache_o", Id "dcache_flush_ctrl_cache");
        CellPinItem2 ("flush_dcache_ack_i", Id "dcache_flush_ack_cache_ctrl");
        CellPinItem2 ("halt_csr_i", Id "halt_csr_ctrl");
        CellPinItem2 ("halt_o", Id "halt_ctrl");
        CellPinItem2 ("eret_i", Id "eret");
        CellPinItem2 ("ex_valid_i", Dot1 (Id "ex_commit", Id "valid"));
        CellPinItem2 ("set_debug_pc_i", Id "set_debug_pc");
        CellPinItem2 ("flush_csr_i", Id "flush_csr_ctrl");
        CellPinItem2 ("resolved_branch_i", Id "resolved_branch");
        CellPinItem2 ("fence_i_i", Id "fence_i_commit_controller");
        CellPinItem2 ("fence_i", Id "fence_commit_controller");
        CellPinItem2 ("sfence_vma_i", Id "sfence_vma_commit_controller");
        CellPinItem2 ("flush_commit_i", Id "flush_commit");
        CellPinItem2 ("flush_icache_o", Id "icache_flush_ctrl_cache");
        Atom ".*"]])]);
  InstDecl (Id "wt_cache_subsystem",
   [Itmlst [CellParamItem2 ("ArianeCfg", Id "ArianeCfg")]],
   [InstNameParen1 ("i_cache_subsystem",
     [Itmlst
       [CellPinItem2 ("clk_i", Id "clk_i");
        CellPinItem2 ("rst_ni", Id "rst_ni");
        CellPinItem2 ("icache_en_i", Id "icache_en_csr");
        CellPinItem2 ("icache_flush_i", Id "icache_flush_ctrl_cache");
        CellPinItem2 ("icache_miss_o", Id "icache_miss_cache_perf");
        CellPinItem2 ("icache_areq_i", Id "icache_areq_ex_cache");
        CellPinItem2 ("icache_areq_o", Id "icache_areq_cache_ex");
        CellPinItem2 ("icache_dreq_i", Id "icache_dreq_if_cache");
        CellPinItem2 ("icache_dreq_o", Id "icache_dreq_cache_if");
        CellPinItem2 ("dcache_enable_i", Id "dcache_en_csr_nbdcache");
        CellPinItem2 ("dcache_flush_i", Id "dcache_flush_ctrl_cache");
        CellPinItem2 ("dcache_flush_ack_o", Id "dcache_flush_ack_cache_ctrl");
        CellPinItem2 ("dcache_amo_req_i", Id "amo_req");
        CellPinItem2 ("dcache_amo_resp_o", Id "amo_resp");
        CellPinItem2 ("dcache_miss_o", Id "dcache_miss_cache_perf");
        CellPinItem2 ("dcache_req_ports_i", Id "dcache_req_ports_ex_cache");
        CellPinItem2 ("dcache_req_ports_o", Id "dcache_req_ports_cache_ex");
        CellPinItem2 ("wbuffer_empty_o", Id "dcache_commit_wbuffer_empty");
        CellPinItem2 ("wbuffer_not_ni_o", Id "dcache_commit_wbuffer_not_ni");
        CellPinItem2 ("axi_req_o", Id "axi_req_o");
        CellPinItem2 ("axi_resp_i", Id "axi_resp_i")]])]);
  DeclInt2 [Id "f"];
  DeclLogic2 ([Id "cycles"],
   [AnyRange (Number (10, 32, 63, "3"), Number (10, 32, 0, ""))]);
  Source_text_rewrite_types.Initial
   [Itmlst
     [Blocking
       (Asgn1 (Id "f",
         Sys ("$fopen",
          Itmlst
           [Source_text_rewrite_types.String "w";
            Source_text_rewrite_types.String "trace_hart_00.dasm"])))]];
  AlwaysFF (At (EventOr [Pos (Id "clk_i"); Neg (Id "rst_ni")]),
   Seq ("",
    [If2 (Tilde (Id "rst_ni"),
      Seq ("", [Equate (Id "cycles", Number (10, 32, 0, ""))]),
      Seq ("",
       [DeclData (Atom "static", Typ8 (Atom "byte", Deflt),
         [VarDeclAsgn (Id "mode", Source_text_rewrite_types.String "")]);
        If2 (Id "debug_mode",
         Blocking (FopAsgn (Id "mode", Source_text_rewrite_types.String "D")),
         Seq ("",
          [CaseStart (CaseStart1 (Id "priv_lvl"),
            [CaseStmt ([PackageRef ("riscv", Id "PRIV_LVL_M")],
              [Blocking
                (FopAsgn (Id "mode", Source_text_rewrite_types.String "M"))]);
             CaseStmt ([PackageRef ("riscv", Id "PRIV_LVL_S")],
              [Blocking
                (FopAsgn (Id "mode", Source_text_rewrite_types.String "S"))]);
             CaseStmt ([PackageRef ("riscv", Id "PRIV_LVL_U")],
              [Blocking
                (FopAsgn (Id "mode", Source_text_rewrite_types.String "U"))])])]));
        ForLoop ([Typ9 ("i", [Atom "int"], Number (10, 32, 0, ""))],
         Less (Id "i", Id "NR_COMMIT_PORTS"), SideEffect (Id "i", Atom "++"),
         Seq ("",
          [If2
            (And2 (IdArrayed2 (Id "commit_ack", Id "i"),
              Pling
               (Dot1
                 (Dot1 (IdArrayed2 (Id "commit_instr_id_commit", Id "i"),
                   Id "ex"),
                 Id "valid"))),
            Seq ("",
             [SysTaskRef (Atom "$fwrite",
               [Id "f";
                Itmlst
                 [Source_text_rewrite_types.String
                   "%d 0x%0h %s (0x%h) DASM(%h)\\n";
                  Id "cycles";
                  Dot1 (IdArrayed2 (Id "commit_instr_id_commit", Id "i"),
                   Id "pc");
                  Id "mode";
                  Dot1
                   (Dot1 (IdArrayed2 (Id "commit_instr_id_commit", Id "i"),
                     Id "ex"),
                   IdArrayedColon (Id "tval", Number (10, 32, 31, "1"),
                    Number (10, 32, 0, "")));
                  Dot1
                   (Dot1 (IdArrayed2 (Id "commit_instr_id_commit", Id "i"),
                     Id "ex"),
                   IdArrayedColon (Id "tval", Number (10, 32, 31, "1"),
                    Number (10, 32, 0, "")))]])]),
            If1
             (And2 (IdArrayed2 (Id "commit_ack", Id "i"),
               Dot1
                (Dot1 (IdArrayed2 (Id "commit_instr_id_commit", Id "i"),
                  Id "ex"),
                Id "valid")),
             Seq ("",
              [If2
                (Equals
                  (Dot1
                    (Dot1 (IdArrayed2 (Id "commit_instr_id_commit", Id "i"),
                      Id "ex"),
                    Id "cause"),
                  Number (10, 32, 2, "")),
                Seq ("",
                 [SysTaskRef (Atom "$fwrite",
                   [Id "f";
                    Itmlst
                     [Source_text_rewrite_types.String
                       "Exception Cause: Illegal Instructions, DASM(%h) PC=%h\\n";
                      Dot1
                       (Dot1
                         (IdArrayed2 (Id "commit_instr_id_commit", Id "i"),
                         Id "ex"),
                       IdArrayedColon (Id "tval", Number (10, 32, 31, "1"),
                        Number (10, 32, 0, "")));
                      Dot1 (IdArrayed2 (Id "commit_instr_id_commit", Id "i"),
                       Id "pc")]])]),
                Seq ("",
                 [If2 (Id "debug_mode",
                   Seq ("",
                    [SysTaskRef (Atom "$fwrite",
                      [Id "f";
                       Itmlst
                        [Source_text_rewrite_types.String
                          "%d 0x%0h %s (0x%h) DASM(%h)\\n";
                         Id "cycles";
                         Dot1
                          (IdArrayed2 (Id "commit_instr_id_commit", Id "i"),
                          Id "pc");
                         Id "mode";
                         Dot1
                          (Dot1
                            (IdArrayed2 (Id "commit_instr_id_commit", Id "i"),
                            Id "ex"),
                          IdArrayedColon (Id "tval",
                           Number (10, 32, 31, "1"), Number (10, 32, 0, "")));
                         Dot1
                          (Dot1
                            (IdArrayed2 (Id "commit_instr_id_commit", Id "i"),
                            Id "ex"),
                          IdArrayedColon (Id "tval",
                           Number (10, 32, 31, "1"), Number (10, 32, 0, "")))]])]),
                   Seq ("",
                    [SysTaskRef (Atom "$fwrite",
                      [Id "f";
                       Itmlst
                        [Source_text_rewrite_types.String
                          "Exception Cause: %5d, DASM(%h) PC=%h\\n";
                         Dot1
                          (Dot1
                            (IdArrayed2 (Id "commit_instr_id_commit", Id "i"),
                            Id "ex"),
                          Id "cause");
                         Dot1
                          (Dot1
                            (IdArrayed2 (Id "commit_instr_id_commit", Id "i"),
                            Id "ex"),
                          IdArrayedColon (Id "tval",
                           Number (10, 32, 31, "1"), Number (10, 32, 0, "")));
                         Dot1
                          (IdArrayed2 (Id "commit_instr_id_commit", Id "i"),
                          Id "pc")]])]))]))])))]));
        Equate (Id "cycles", Add (Id "cycles", Number (10, 32, 1, "")))]))]));
  Source_text_rewrite_types.Final
   [Itmlst [SysTaskRef (Atom "$fclose", [Id "f"])]]])
