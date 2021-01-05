package swerv_types;
typedef struct packed {
                       logic [2:0] trace_rv_i_valid_ip;
                       logic [95:0] trace_rv_i_insn_ip;
                       logic [95:0] trace_rv_i_address_ip;
                       logic [2:0] trace_rv_i_exception_ip;
                       logic [4:0] trace_rv_i_ecause_ip;
                       logic [2:0] trace_rv_i_interrupt_ip;
                       logic [31:0] trace_rv_i_tval_ip;
                       } trace_pkt_t;
typedef enum logic [3:0] {
                          NULL     = 4'b0000,
                          MUL      = 4'b0001,
                          LOAD     = 4'b0010,
                          STORE    = 4'b0011,
                          ALU      = 4'b0100,
                          CSRREAD  = 4'b0101,
                          CSRWRITE = 4'b0110,
                          CSRRW    = 4'b0111,
                          EBREAK   = 4'b1000,
                          ECALL    = 4'b1001,
                          FENCE    = 4'b1010,
                          FENCEI   = 4'b1011,
                          MRET     = 4'b1100,
                          CONDBR   = 4'b1101,
                          JAL      = 4'b1110
                          } inst_t;
typedef struct packed {
                       logic [7:0] parity;
                       } icache_err_pkt_t;
typedef struct packed {
                       logic valid;
                       logic wb;
                       logic [3-1:0] tag;
                       logic [4:0] rd;
                       } load_cam_pkt_t;
typedef struct packed {
                       logic pc0_call;
                       logic pc0_ret;
                       logic pc0_pc4;
                       logic pc1_call;
                       logic pc1_ret;
                       logic pc1_pc4;
                       } rets_pkt_t;
typedef struct packed {
                       logic valid;
                       logic [11:0] toffset;
                       logic [1:0] hist;
                       logic br_error;
                       logic br_start_error;
                       logic [5:4] index;
                       logic [1:0] bank;
                       logic [31:1] prett;   
                       logic [4:0] fghr;
                       logic way;
                       logic ret;
                       logic [9-1:0] btag;
                       } br_pkt_t;
typedef struct packed {
                       logic valid;
                       logic [1:0] hist;
                       logic br_error;
                       logic br_start_error;
                       logic [5:4] index;
                       logic [1:0] bank;
                       logic [4:0] fghr;
                       logic way;
                       logic middle;
                       } br_tlu_pkt_t;
typedef struct packed {
                       logic misp;
                       logic ataken;
                       logic boffset;
                       logic pc4;
                       logic [1:0] hist;
                       logic [11:0] toffset;
                       logic [5:4] index;
                       logic [1:0] bank;
                       logic valid;
                       logic br_error;
                       logic br_start_error;
                       logic [31:1] prett;
                       logic pcall;
                       logic pret;
                       logic pja;
                       logic [9-1:0] btag;
                       logic [4:0] fghr;
                       logic way;
                       } predict_pkt_t;
typedef struct packed {
                       logic legal;
                       logic icaf;
                       logic icaf_second;
                       logic perr;
                       logic sbecc;
                       logic fence_i;
                       logic [3:0] i0trigger;
                       logic [3:0] i1trigger;
                       inst_t pmu_i0_itype;         
                       inst_t pmu_i1_itype;         
                       logic pmu_i0_br_unpred;      
                       logic pmu_i1_br_unpred;      
                       logic pmu_divide;
                       logic pmu_lsu_misaligned;
                       } trap_pkt_t;
typedef struct packed {
                       logic [4:0] i0rd;
                       logic i0mul;
                       logic i0load;
                       logic i0store;
                       logic i0div;
                       logic i0v;
                       logic i0valid;
                       logic i0secondary;
                       logic [1:0] i0rs1bype2;
                       logic [1:0] i0rs2bype2;
                       logic [3:0] i0rs1bype3;
                       logic [3:0] i0rs2bype3;
                       logic [4:0] i1rd;
                       logic i1mul;
                       logic i1load;
                       logic i1store;
                       logic i1v;
                       logic i1valid;
                       logic csrwen;
                       logic csrwonly;
                       logic [11:0] csrwaddr;
                       logic i1secondary;
                       logic [1:0] i1rs1bype2;
                       logic [1:0] i1rs2bype2;
                       logic [6:0] i1rs1bype3;
                       logic [6:0] i1rs2bype3;
                       } dest_pkt_t;
typedef struct packed {
                       logic mul;
                       logic load;
                       logic sec;
                       logic alu;
                       } class_pkt_t;
typedef struct packed {
                       logic [4:0] rs1;
                       logic [4:0] rs2;
                       logic [4:0] rd;
                       } reg_pkt_t;
typedef struct packed {
                       logic valid;
                       logic land;
                       logic lor;
                       logic lxor;
                       logic sll;
                       logic srl;
                       logic sra;
                       logic beq;
                       logic bne;
                       logic blt;
                       logic bge;
                       logic add;
                       logic sub;
                       logic slt;
                       logic unsign;
                       logic jal;
                       logic predict_t;
                       logic predict_nt;
                       logic csr_write;
                       logic csr_imm;
                       } alu_pkt_t;
typedef struct packed {
                       logic by;
                       logic half;
                       logic word;
                       logic dword;   
                       logic load;
                       logic store;
                       logic unsign;
                       logic dma;     
                       logic store_data_bypass_c1;
                       logic load_ldst_bypass_c1;
                       logic store_data_bypass_c2;
                       logic store_data_bypass_i0_e2_c2;
                       logic [1:0] store_data_bypass_e4_c1;
                       logic [1:0] store_data_bypass_e4_c2;
                       logic [1:0] store_data_bypass_e4_c3;
                       logic valid;
                       } lsu_pkt_t;
typedef struct packed {
                      logic exc_valid;
                      logic single_ecc_error;
                      logic inst_type;    
                      logic inst_pipe;    
                      logic dma_valid;
                      logic exc_type;     
                      logic [31:0] addr;
                      } lsu_error_pkt_t;
typedef struct packed {
                       logic alu;
                       logic rs1;
                       logic rs2;
                       logic imm12;
                       logic rd;
                       logic shimm5;
                       logic imm20;
                       logic pc;
                       logic load;
                       logic store;
                       logic lsu;
                       logic add;
                       logic sub;
                       logic land;
                       logic lor;
                       logic lxor;
                       logic sll;
                       logic sra;
                       logic srl;
                       logic slt;
                       logic unsign;
                       logic condbr;
                       logic beq;
                       logic bne;
                       logic bge;
                       logic blt;
                       logic jal;
                       logic by;
                       logic half;
                       logic word;
                       logic csr_read;
                       logic csr_clr;
                       logic csr_set;
                       logic csr_write;
                       logic csr_imm;
                       logic presync;
                       logic postsync;
                       logic ebreak;
                       logic ecall;
                       logic mret;
                       logic mul;
                       logic rs1_sign;
                       logic rs2_sign;
                       logic low;
                       logic div;
                       logic rem;
                       logic fence;
                       logic fence_i;
                       logic pm_alu;
                       logic legal;
                       } dec_pkt_t;
typedef struct packed {
                       logic valid;
                       logic rs1_sign;
                       logic rs2_sign;
                       logic low;
                       logic load_mul_rs1_bypass_e1;
                       logic load_mul_rs2_bypass_e1;
                       } mul_pkt_t;
typedef struct packed {
                       logic valid;
                       logic unsign;
                       logic rem;
                       } div_pkt_t;
typedef struct packed {
                        logic        select;
                        logic        match;
                        logic        store;
                        logic        load;
                        logic        execute;
                        logic        m;
                        logic [31:0] tdata2;
            } trigger_pkt_t;
typedef struct packed {
                        logic [33:0]  icache_wrdata;  
                        logic [18:2]  icache_dicawics;
                        logic         icache_rd_valid;
                        logic         icache_wr_valid;
            } cache_debug_pkt_t;
endpackage  
module swerv_wrapper
   import swerv_types::*;
(
   input logic                       clk,
   input logic                       rst_l,
   input logic                       dbg_rst_l,
   input logic [31:1]                rst_vec,
   input logic                       nmi_int,
   input logic [31:1]                nmi_vec,
   input logic [31:1]                jtag_id,
   output logic [63:0] trace_rv_i_insn_ip,
   output logic [63:0] trace_rv_i_address_ip,
   output logic [2:0]  trace_rv_i_valid_ip,
   output logic [2:0]  trace_rv_i_exception_ip,
   output logic [4:0]  trace_rv_i_ecause_ip,
   output logic [2:0]  trace_rv_i_interrupt_ip,
   output logic [31:0] trace_rv_i_tval_ip,
   output logic                            lsu_axi_awvalid,
   input  logic                            lsu_axi_awready,
   output logic [4-1:0]      lsu_axi_awid,
   output logic [31:0]                     lsu_axi_awaddr,
   output logic [3:0]                      lsu_axi_awregion,
   output logic [7:0]                      lsu_axi_awlen,
   output logic [2:0]                      lsu_axi_awsize,
   output logic [1:0]                      lsu_axi_awburst,
   output logic                            lsu_axi_awlock,
   output logic [3:0]                      lsu_axi_awcache,
   output logic [2:0]                      lsu_axi_awprot,
   output logic [3:0]                      lsu_axi_awqos,
   output logic                            lsu_axi_wvalid,
   input  logic                            lsu_axi_wready,
   output logic [63:0]                     lsu_axi_wdata,
   output logic [7:0]                      lsu_axi_wstrb,
   output logic                            lsu_axi_wlast,
   input  logic                            lsu_axi_bvalid,
   output logic                            lsu_axi_bready,
   input  logic [1:0]                      lsu_axi_bresp,
   input  logic [4-1:0]      lsu_axi_bid,
   output logic                            lsu_axi_arvalid,
   input  logic                            lsu_axi_arready,
   output logic [4-1:0]      lsu_axi_arid,
   output logic [31:0]                     lsu_axi_araddr,
   output logic [3:0]                      lsu_axi_arregion,
   output logic [7:0]                      lsu_axi_arlen,
   output logic [2:0]                      lsu_axi_arsize,
   output logic [1:0]                      lsu_axi_arburst,
   output logic                            lsu_axi_arlock,
   output logic [3:0]                      lsu_axi_arcache,
   output logic [2:0]                      lsu_axi_arprot,
   output logic [3:0]                      lsu_axi_arqos,
   input  logic                            lsu_axi_rvalid,
   output logic                            lsu_axi_rready,
   input  logic [4-1:0]      lsu_axi_rid,
   input  logic [63:0]                     lsu_axi_rdata,
   input  logic [1:0]                      lsu_axi_rresp,
   input  logic                            lsu_axi_rlast,
   output logic                            ifu_axi_awvalid,
   input  logic                            ifu_axi_awready,
   output logic [3-1:0]      ifu_axi_awid,
   output logic [31:0]                     ifu_axi_awaddr,
   output logic [3:0]                      ifu_axi_awregion,
   output logic [7:0]                      ifu_axi_awlen,
   output logic [2:0]                      ifu_axi_awsize,
   output logic [1:0]                      ifu_axi_awburst,
   output logic                            ifu_axi_awlock,
   output logic [3:0]                      ifu_axi_awcache,
   output logic [2:0]                      ifu_axi_awprot,
   output logic [3:0]                      ifu_axi_awqos,
   output logic                            ifu_axi_wvalid,
   input  logic                            ifu_axi_wready,
   output logic [63:0]                     ifu_axi_wdata,
   output logic [7:0]                      ifu_axi_wstrb,
   output logic                            ifu_axi_wlast,
   input  logic                            ifu_axi_bvalid,
   output logic                            ifu_axi_bready,
   input  logic [1:0]                      ifu_axi_bresp,
   input  logic [3-1:0]      ifu_axi_bid,
   output logic                            ifu_axi_arvalid,
   input  logic                            ifu_axi_arready,
   output logic [3-1:0]      ifu_axi_arid,
   output logic [31:0]                     ifu_axi_araddr,
   output logic [3:0]                      ifu_axi_arregion,
   output logic [7:0]                      ifu_axi_arlen,
   output logic [2:0]                      ifu_axi_arsize,
   output logic [1:0]                      ifu_axi_arburst,
   output logic                            ifu_axi_arlock,
   output logic [3:0]                      ifu_axi_arcache,
   output logic [2:0]                      ifu_axi_arprot,
   output logic [3:0]                      ifu_axi_arqos,
   input  logic                            ifu_axi_rvalid,
   output logic                            ifu_axi_rready,
   input  logic [3-1:0]      ifu_axi_rid,
   input  logic [63:0]                     ifu_axi_rdata,
   input  logic [1:0]                      ifu_axi_rresp,
   input  logic                            ifu_axi_rlast,
   output logic                            sb_axi_awvalid,
   input  logic                            sb_axi_awready,
   output logic [1-1:0]       sb_axi_awid,
   output logic [31:0]                     sb_axi_awaddr,
   output logic [3:0]                      sb_axi_awregion,
   output logic [7:0]                      sb_axi_awlen,
   output logic [2:0]                      sb_axi_awsize,
   output logic [1:0]                      sb_axi_awburst,
   output logic                            sb_axi_awlock,
   output logic [3:0]                      sb_axi_awcache,
   output logic [2:0]                      sb_axi_awprot,
   output logic [3:0]                      sb_axi_awqos,
   output logic                            sb_axi_wvalid,
   input  logic                            sb_axi_wready,
   output logic [63:0]                     sb_axi_wdata,
   output logic [7:0]                      sb_axi_wstrb,
   output logic                            sb_axi_wlast,
   input  logic                            sb_axi_bvalid,
   output logic                            sb_axi_bready,
   input  logic [1:0]                      sb_axi_bresp,
   input  logic [1-1:0]       sb_axi_bid,
   output logic                            sb_axi_arvalid,
   input  logic                            sb_axi_arready,
   output logic [1-1:0]       sb_axi_arid,
   output logic [31:0]                     sb_axi_araddr,
   output logic [3:0]                      sb_axi_arregion,
   output logic [7:0]                      sb_axi_arlen,
   output logic [2:0]                      sb_axi_arsize,
   output logic [1:0]                      sb_axi_arburst,
   output logic                            sb_axi_arlock,
   output logic [3:0]                      sb_axi_arcache,
   output logic [2:0]                      sb_axi_arprot,
   output logic [3:0]                      sb_axi_arqos,
   input  logic                            sb_axi_rvalid,
   output logic                            sb_axi_rready,
   input  logic [1-1:0]       sb_axi_rid,
   input  logic [63:0]                     sb_axi_rdata,
   input  logic [1:0]                      sb_axi_rresp,
   input  logic                            sb_axi_rlast,
   input  logic                         dma_axi_awvalid,
   output logic                         dma_axi_awready,
   input  logic [1-1:0]   dma_axi_awid,
   input  logic [31:0]                  dma_axi_awaddr,
   input  logic [2:0]                   dma_axi_awsize,
   input  logic [2:0]                   dma_axi_awprot,
   input  logic [7:0]                   dma_axi_awlen,
   input  logic [1:0]                   dma_axi_awburst,
   input  logic                         dma_axi_wvalid,
   output logic                         dma_axi_wready,
   input  logic [63:0]                  dma_axi_wdata,
   input  logic [7:0]                   dma_axi_wstrb,
   input  logic                         dma_axi_wlast,
   output logic                         dma_axi_bvalid,
   input  logic                         dma_axi_bready,
   output logic [1:0]                   dma_axi_bresp,
   output logic [1-1:0]   dma_axi_bid,
   input  logic                         dma_axi_arvalid,
   output logic                         dma_axi_arready,
   input  logic [1-1:0]   dma_axi_arid,
   input  logic [31:0]                  dma_axi_araddr,
   input  logic [2:0]                   dma_axi_arsize,
   input  logic [2:0]                   dma_axi_arprot,
   input  logic [7:0]                   dma_axi_arlen,
   input  logic [1:0]                   dma_axi_arburst,
   output logic                         dma_axi_rvalid,
   input  logic                         dma_axi_rready,
   output logic [1-1:0]   dma_axi_rid,
   output logic [63:0]                  dma_axi_rdata,
   output logic [1:0]                   dma_axi_rresp,
   output logic                         dma_axi_rlast,
   input logic                       lsu_bus_clk_en,  
   input logic                       ifu_bus_clk_en,  
   input logic                       dbg_bus_clk_en,  
   input logic                       dma_bus_clk_en,  
   input logic                       timer_int,
   input logic [8:1] extintsrc_req,
   output logic [1:0] dec_tlu_perfcnt0,  
   output logic [1:0] dec_tlu_perfcnt1,
   output logic [1:0] dec_tlu_perfcnt2,
   output logic [1:0] dec_tlu_perfcnt3,
   input logic                       jtag_tck,  
   input logic                       jtag_tms,  
   input logic                       jtag_tdi,  
   input logic                       jtag_trst_n,  
   output logic                      jtag_tdo,  
   input logic mpc_debug_halt_req,  
   input logic mpc_debug_run_req,  
   input logic mpc_reset_run_req,  
   output logic mpc_debug_halt_ack,  
   output logic mpc_debug_run_ack,  
   output logic debug_brkpt_status,  
   input logic                       i_cpu_halt_req,  
   output logic                      o_cpu_halt_ack,  
   output logic                      o_cpu_halt_status,  
   output logic                      o_debug_mode_status,  
   input logic                       i_cpu_run_req,  
   output logic                      o_cpu_run_ack,  
   input logic                       scan_mode,  
   input logic                       mbist_mode  
);
localparam TOTAL_INT        = 9;
localparam DCCM_BITS        = 16;
localparam DCCM_BANK_BITS   = 3;
localparam DCCM_NUM_BANKS   = 8;
localparam DCCM_DATA_WIDTH  = 32;
localparam DCCM_FDATA_WIDTH = 39;
localparam DCCM_BYTE_WIDTH  = 4;
localparam DCCM_ECC_WIDTH   = 7;
localparam LSU_RDBUF_DEPTH  = 8;
localparam DMA_BUF_DEPTH    = 4;
localparam LSU_STBUF_DEPTH  = 8;
localparam LSU_SB_BITS      = 16;
localparam DEC_INSTBUF_DEPTH = 4;
localparam ICCM_SIZE         = 512;
localparam ICCM_BITS         = 19;
localparam ICCM_NUM_BANKS    = 8;
localparam ICCM_BANK_BITS    = 3;
localparam ICCM_INDEX_BITS   = 14;
localparam ICCM_BANK_HI      = 4 + (3/4);
localparam ICACHE_TAG_HIGH  = 12;
localparam ICACHE_TAG_LOW   = 6;
localparam ICACHE_IC_DEPTH  = 8;
localparam ICACHE_TAG_DEPTH = 64;
localparam LSU_BUS_TAG     = 4;
localparam DMA_BUS_TAG     = 1;
localparam SB_BUS_TAG      = 1;
localparam IFU_BUS_TAG     = 3;
   logic         dccm_wren;
   logic         dccm_rden;
   logic [DCCM_BITS-1:0]  dccm_wr_addr;
   logic [DCCM_BITS-1:0]  dccm_rd_addr_lo;
   logic [DCCM_BITS-1:0]  dccm_rd_addr_hi;
   logic [DCCM_FDATA_WIDTH-1:0]  dccm_wr_data;
   logic [DCCM_FDATA_WIDTH-1:0]  dccm_rd_data_lo;
   logic [DCCM_FDATA_WIDTH-1:0]  dccm_rd_data_hi;
   logic         lsu_freeze_dc3;
   logic [31:2]  ic_rw_addr;
   logic [3:0]   ic_wr_en  ;      
   logic         ic_rd_en ;
   logic [3:0]   ic_tag_valid;    
   logic [3:0]   ic_rd_hit;       
   logic         ic_tag_perr;     
   logic [15:2]  ic_debug_addr;       
   logic         ic_debug_rd_en;      
   logic         ic_debug_wr_en;      
   logic         ic_debug_tag_array;  
   logic [3:0]   ic_debug_way;        
   logic [20:0]  ictag_debug_rd_data; 
   logic [67:0]  ic_wr_data;          
   logic [135:0] ic_rd_data;          
   logic [33:0]  ic_debug_wr_data;    
   logic [127:0] ic_premux_data;
   logic         ic_sel_premux_data;
   logic        core_rst_l;      
   logic        jtag_tdoEn;
   logic        dccm_clk_override;
   logic        icm_clk_override;
   logic        dec_tlu_core_ecc_disable;
   logic        dmi_reg_en;
   logic [6:0]  dmi_reg_addr;
   logic        dmi_reg_wr_en;
   logic [31:0] dmi_reg_wdata;
   logic [31:0] dmi_reg_rdata;
   logic        dmi_hard_reset;
   swerv swerv (
          .*
          );
   mem  mem (
        .rst_l(core_rst_l),
        .*
        );
   dmi_wrapper  dmi_wrapper (
           .scan_mode(scan_mode),            
           .trst_n(jtag_trst_n),            
           .tck   (jtag_tck),               
           .tms   (jtag_tms),               
           .tdi   (jtag_tdi),               
           .tdo   (jtag_tdo),               
           .tdoEnable (),                   
           .core_rst_n  (dbg_rst_l),      
           .core_clk    (clk),             
           .jtag_id     (jtag_id),         
           .rd_data     (dmi_reg_rdata),   
           .reg_wr_data (dmi_reg_wdata),   
           .reg_wr_addr (dmi_reg_addr),    
           .reg_en      (dmi_reg_en),      
           .reg_wr_en   (dmi_reg_wr_en),    
           .dmi_hard_reset   (dmi_hard_reset)    
);
endmodule
module mem
   import swerv_types::*;
(
   input logic         clk,
   input logic         rst_l,
   input logic         lsu_freeze_dc3,
   input logic         dccm_clk_override,
   input logic         icm_clk_override,
   input logic         dec_tlu_core_ecc_disable,
   input logic         dccm_wren,
   input logic         dccm_rden,
   input logic [16-1:0]  dccm_wr_addr,
   input logic [16-1:0]  dccm_rd_addr_lo,
   input logic [16-1:0]  dccm_rd_addr_hi,
   input logic [39-1:0]  dccm_wr_data,
   output logic [39-1:0]  dccm_rd_data_lo,
   output logic [39-1:0]  dccm_rd_data_hi,
   input  logic [31:2]  ic_rw_addr,
   input  logic [3:0]   ic_tag_valid,
   input  logic [3:0]   ic_wr_en,
   input  logic         ic_rd_en,
   input  logic [127:0] ic_premux_data,      
   input  logic         ic_sel_premux_data,  
   input  logic [67:0]               ic_wr_data,          
   input  logic [33:0]               ic_debug_wr_data,    
   input  logic [15:2]               ic_debug_addr,       
   input  logic                      ic_debug_rd_en,      
   input  logic                      ic_debug_wr_en,      
   input  logic                      ic_debug_tag_array,  
   input  logic [3:0]                ic_debug_way,        
   output logic [135:0]              ic_rd_data ,         
   output logic [20:0]               ictag_debug_rd_data, 
   output logic [3:0]   ic_rd_hit,
   output logic         ic_tag_perr,         
   input  logic         scan_mode
);
localparam TOTAL_INT        = 9;
localparam DCCM_BITS        = 16;
localparam DCCM_BANK_BITS   = 3;
localparam DCCM_NUM_BANKS   = 8;
localparam DCCM_DATA_WIDTH  = 32;
localparam DCCM_FDATA_WIDTH = 39;
localparam DCCM_BYTE_WIDTH  = 4;
localparam DCCM_ECC_WIDTH   = 7;
localparam LSU_RDBUF_DEPTH  = 8;
localparam DMA_BUF_DEPTH    = 4;
localparam LSU_STBUF_DEPTH  = 8;
localparam LSU_SB_BITS      = 16;
localparam DEC_INSTBUF_DEPTH = 4;
localparam ICCM_SIZE         = 512;
localparam ICCM_BITS         = 19;
localparam ICCM_NUM_BANKS    = 8;
localparam ICCM_BANK_BITS    = 3;
localparam ICCM_INDEX_BITS   = 14;
localparam ICCM_BANK_HI      = 4 + (3/4);
localparam ICACHE_TAG_HIGH  = 12;
localparam ICACHE_TAG_LOW   = 6;
localparam ICACHE_IC_DEPTH  = 8;
localparam ICACHE_TAG_DEPTH = 64;
localparam LSU_BUS_TAG     = 4;
localparam DMA_BUS_TAG     = 1;
localparam SB_BUS_TAG      = 1;
localparam IFU_BUS_TAG     = 3;
      localparam DCCM_ENABLE = 1'b1;
   logic free_clk;
   rvoclkhdr free_cg   ( .en(1'b1),         .l1clk(free_clk), .* );
   if (DCCM_ENABLE == 1) begin: Gen_dccm_enable
      lsu_dccm_mem dccm (
         .clk_override(dccm_clk_override),
         .*
      );
   end else begin: Gen_dccm_disable
      assign dccm_rd_data_lo = '0;
      assign dccm_rd_data_hi = '0;
   end
   ifu_ic_mem icm  (
      .clk_override(icm_clk_override),
      .*
   );
endmodule
module pic_ctrl
                  (
                     input  logic                   clk,                   
                     input  logic                   free_clk,              
                     input  logic                   active_clk,            
                     input  logic                   rst_l,                 
                     input  logic                   clk_override,          
                     input  logic                   lsu_freeze_dc3,        
                     input  logic [9-1:0]   extintsrc_req,         
                     input  logic [31:0]            picm_addr,             
                     input  logic [31:0]            picm_wr_data,          
                     input  logic                   picm_wren,             
                     input  logic                   picm_rden,             
                     input  logic                   picm_mken,             
                     input  logic [3:0]             meicurpl,              
                     input  logic [3:0]             meipt,                 
                     output logic                   mexintpend,            
                     output logic [7:0]             claimid,               
                     output logic [3:0]             pl,                    
                     output logic [31:0]            picm_rd_data,          
                     output logic                   mhwakeup,              
                     input  logic                   scan_mode              
);
localparam TOTAL_INT        = 9;
localparam DCCM_BITS        = 16;
localparam DCCM_BANK_BITS   = 3;
localparam DCCM_NUM_BANKS   = 8;
localparam DCCM_DATA_WIDTH  = 32;
localparam DCCM_FDATA_WIDTH = 39;
localparam DCCM_BYTE_WIDTH  = 4;
localparam DCCM_ECC_WIDTH   = 7;
localparam LSU_RDBUF_DEPTH  = 8;
localparam DMA_BUF_DEPTH    = 4;
localparam LSU_STBUF_DEPTH  = 8;
localparam LSU_SB_BITS      = 16;
localparam DEC_INSTBUF_DEPTH = 4;
localparam ICCM_SIZE         = 512;
localparam ICCM_BITS         = 19;
localparam ICCM_NUM_BANKS    = 8;
localparam ICCM_BANK_BITS    = 3;
localparam ICCM_INDEX_BITS   = 14;
localparam ICCM_BANK_HI      = 4 + (3/4);
localparam ICACHE_TAG_HIGH  = 12;
localparam ICACHE_TAG_LOW   = 6;
localparam ICACHE_IC_DEPTH  = 8;
localparam ICACHE_TAG_DEPTH = 64;
localparam LSU_BUS_TAG     = 4;
localparam DMA_BUS_TAG     = 1;
localparam SB_BUS_TAG      = 1;
localparam IFU_BUS_TAG     = 3;
localparam NUM_LEVELS            = $clog2(TOTAL_INT);
localparam INTPRIORITY_BASE_ADDR = 32'hf00c0000 ;
localparam INTPEND_BASE_ADDR     = 32'hf00c0000 + 32'h00001000 ;
localparam INTENABLE_BASE_ADDR   = 32'hf00c0000 + 32'h00002000 ;
localparam EXT_INTR_PIC_CONFIG   = 32'hf00c0000 + 32'h00003000 ;
localparam EXT_INTR_GW_CONFIG    = 32'hf00c0000 + 32'h00004000 ;
localparam EXT_INTR_GW_CLEAR     = 32'hf00c0000 + 32'h00005000 ;
localparam INTPEND_SIZE          = (TOTAL_INT < 32)  ? 32  :
                                   (TOTAL_INT < 64)  ? 64  :
                                   (TOTAL_INT < 128) ? 128 :
                                   (TOTAL_INT < 256) ? 256 :
                                   (TOTAL_INT < 512) ? 512 :  1024 ;
localparam INT_GRPS              =   INTPEND_SIZE / 32 ;
localparam INTPRIORITY_BITS      =  4 ;
localparam ID_BITS               =  8 ;
localparam int GW_CONFIG[TOTAL_INT-1:0] = '{default:0} ;
logic  addr_intpend_base_match;
logic  addr_intenable_base_match;
logic  addr_intpriority_base_match;
logic  addr_config_pic_match ;
logic  addr_config_gw_base_match ;
logic  addr_clear_gw_base_match ;
logic  mexintpend_in;
logic  mhwakeup_in ;
logic  intpend_reg_read ;
logic [31:0]                                 picm_rd_data_in, intpend_rd_out;
logic                                        intenable_rd_out ;
logic [INTPRIORITY_BITS-1:0]                 intpriority_rd_out;
logic [1:0]                                  gw_config_rd_out;
logic [TOTAL_INT-1:0] [INTPRIORITY_BITS-1:0] intpriority_reg;
logic [TOTAL_INT-1:0] [INTPRIORITY_BITS-1:0] intpriority_reg_inv;
logic [TOTAL_INT-1:0]                        intpriority_reg_we;
logic [TOTAL_INT-1:0]                        intpriority_reg_re;
logic [TOTAL_INT-1:0] [1:0]                  gw_config_reg;
logic [TOTAL_INT-1:0]                        intenable_reg;
logic [TOTAL_INT-1:0]                        intenable_reg_we;
logic [TOTAL_INT-1:0]                        intenable_reg_re;
logic [TOTAL_INT-1:0]                        gw_config_reg_we;
logic [TOTAL_INT-1:0]                        gw_config_reg_re;
logic [TOTAL_INT-1:0]                        gw_clear_reg_we;
logic [INTPEND_SIZE-1:0]                     intpend_reg_extended;
logic [TOTAL_INT-1:0] [INTPRIORITY_BITS-1:0] intpend_w_prior_en;
logic [TOTAL_INT-1:0] [ID_BITS-1:0]          intpend_id;
logic [INTPRIORITY_BITS-1:0]                 maxint;
logic [INTPRIORITY_BITS-1:0]                 selected_int_priority;
logic [INT_GRPS-1:0] [31:0]                  intpend_rd_part_out ;
logic                                        config_reg;
logic                                        intpriord;
logic                                        config_reg_we ;
logic                                        config_reg_re ;
logic                                        config_reg_in ;
logic                                        prithresh_reg_write , prithresh_reg_read;
logic                                        intpriority_reg_read ;
logic                                        intenable_reg_read   ;
logic                                        gw_config_reg_read   ;
logic                                        picm_wren_ff , picm_rden_ff ;
logic [31:0]                                 picm_addr_ff;
logic [31:0]                                 picm_wr_data_ff;
logic [3:0]                                  mask;
logic                                        picm_mken_ff;
logic [ID_BITS-1:0]                          claimid_in ;
logic [INTPRIORITY_BITS-1:0]                 pl_in ;
logic [INTPRIORITY_BITS-1:0]                 pl_in_q ;
   logic                                     picm_rden_in, picm_mken_in;
logic [TOTAL_INT-1:0]                        extintsrc_req_sync;
logic [TOTAL_INT-1:0]                        extintsrc_req_gw;
   logic                                     pic_addr_c1_clken;
   logic                                     pic_data_c1_clken;
   logic                                     pic_pri_c1_clken;
   logic                                     pic_int_c1_clken;
   logic                                     gw_config_c1_clken;
   logic                                     pic_addr_c1_clk;
   logic                                     pic_data_c1_clk;
   logic                                     pic_pri_c1_clk;
   logic                                     pic_int_c1_clk;
   logic                                     gw_config_c1_clk;
   assign pic_addr_c1_clken   = (picm_mken | picm_rden | picm_wren | clk_override) & ~lsu_freeze_dc3;
   assign pic_data_c1_clken   = picm_wren | clk_override;
   assign pic_pri_c1_clken    = (addr_intpriority_base_match & (picm_wren_ff | picm_rden_ff)) | clk_override;
   assign pic_int_c1_clken    = (addr_intenable_base_match & (picm_wren_ff | picm_rden_ff))   | clk_override;
   assign gw_config_c1_clken  = (addr_config_gw_base_match & (picm_wren_ff | picm_rden_ff))   | clk_override;
   rvoclkhdr pic_addr_c1_cgc   ( .en(pic_addr_c1_clken), .l1clk(pic_addr_c1_clk), .* );
   rvoclkhdr pic_data_c1_cgc   ( .en(pic_data_c1_clken), .l1clk(pic_data_c1_clk), .* );
   rvoclkhdr pic_pri_c1_cgc    ( .en(pic_pri_c1_clken),  .l1clk(pic_pri_c1_clk),  .* );
   rvoclkhdr pic_int_c1_cgc    ( .en(pic_int_c1_clken),  .l1clk(pic_int_c1_clk),  .* );
   rvoclkhdr gw_config_c1_cgc  ( .en(gw_config_c1_clken),  .l1clk(gw_config_c1_clk),  .* );
assign addr_intpend_base_match      = (picm_addr_ff[31:6]            == INTPEND_BASE_ADDR[31:6]) ;
assign addr_intenable_base_match    = (picm_addr_ff[31:NUM_LEVELS+2] == INTENABLE_BASE_ADDR[31:NUM_LEVELS+2]) ;
assign addr_intpriority_base_match  = (picm_addr_ff[31:NUM_LEVELS+2] == INTPRIORITY_BASE_ADDR[31:NUM_LEVELS+2]) ;
assign addr_config_pic_match        = (picm_addr_ff[31:0]            == EXT_INTR_PIC_CONFIG[31:0]) ;
assign addr_config_gw_base_match    = (picm_addr_ff[31:NUM_LEVELS+2] == EXT_INTR_GW_CONFIG[31:NUM_LEVELS+2]) ;
assign addr_clear_gw_base_match     = (picm_addr_ff[31:NUM_LEVELS+2] == EXT_INTR_GW_CLEAR[31:NUM_LEVELS+2]) ;
assign picm_rden_in = lsu_freeze_dc3 ? picm_rden_ff : picm_rden;
assign picm_mken_in = lsu_freeze_dc3 ? picm_mken_ff : picm_mken;
rvdff #(32)                picm_add_flop   (.*, .din (picm_addr),                    .dout(picm_addr_ff),         .clk(pic_addr_c1_clk));
rvdff  #(1)                picm_wre_flop   (.*, .din (picm_wren),                    .dout(picm_wren_ff),         .clk(active_clk));
rvdff  #(1)                picm_rde_flop   (.*, .din (picm_rden_in),                 .dout(picm_rden_ff),         .clk(active_clk));
rvdff  #(1)                picm_mke_flop   (.*, .din (picm_mken_in),                 .dout(picm_mken_ff),         .clk(active_clk));
rvdff #(32)                picm_dat_flop   (.*, .din (picm_wr_data[31:0]),           .dout(picm_wr_data_ff[31:0]), .clk(pic_data_c1_clk));
rvsyncss  #(TOTAL_INT-1) sync_inst
(
 .clk (free_clk),
 .dout(extintsrc_req_sync[TOTAL_INT-1:1]),
 .din (extintsrc_req[TOTAL_INT-1:1]),
 .*) ;
assign extintsrc_req_sync[0] = extintsrc_req[0];
genvar i, l, m , j, k;
for (i=0; i<TOTAL_INT ; i++) begin  : SETREG
 if (i > 0 ) begin : NON_ZERO_INT
     assign intpriority_reg_we[i] =  addr_intpriority_base_match & (picm_addr_ff[NUM_LEVELS+1:2] == i) & picm_wren_ff;
     assign intpriority_reg_re[i] =  addr_intpriority_base_match & (picm_addr_ff[NUM_LEVELS+1:2] == i) & picm_rden_ff;
     assign intenable_reg_we[i]   =  addr_intenable_base_match   & (picm_addr_ff[NUM_LEVELS+1:2] == i) & picm_wren_ff;
     assign intenable_reg_re[i]   =  addr_intenable_base_match   & (picm_addr_ff[NUM_LEVELS+1:2] == i) & picm_rden_ff;
     assign gw_config_reg_we[i]   =  addr_config_gw_base_match   & (picm_addr_ff[NUM_LEVELS+1:2] == i) & picm_wren_ff;
     assign gw_config_reg_re[i]   =  addr_config_gw_base_match   & (picm_addr_ff[NUM_LEVELS+1:2] == i) & picm_rden_ff;
     assign gw_clear_reg_we[i]    =  addr_clear_gw_base_match   & (picm_addr_ff[NUM_LEVELS+1:2] == i) & picm_wren_ff ;
     rvdffs #(INTPRIORITY_BITS) intpriority_ff  (.*, .en( intpriority_reg_we[i]), .din (picm_wr_data_ff[INTPRIORITY_BITS-1:0]), .dout(intpriority_reg[i]), .clk(pic_pri_c1_clk));
     rvdffs #(1)                 intenable_ff   (.*, .en( intenable_reg_we[i]),   .din (picm_wr_data_ff[0]),                    .dout(intenable_reg[i]),   .clk(pic_int_c1_clk));
        rvdffs #(2)                 gw_config_ff   (.*, .en( gw_config_reg_we[i]),   .din (picm_wr_data_ff[1:0]),                  .dout(gw_config_reg[i]),   .clk(gw_config_c1_clk));
        configurable_gw config_gw_inst(.*, .gw_clk(free_clk),
                         .extintsrc_req_sync(extintsrc_req_sync[i]) ,
                         .meigwctrl_polarity(gw_config_reg[i][0]) ,
                         .meigwctrl_type(gw_config_reg[i][1]) ,
                         .meigwclr(gw_clear_reg_we[i]) ,
                         .extintsrc_req_config(extintsrc_req_gw[i])
                            );
 end else begin : INT_ZERO
     assign intpriority_reg_we[i] =  1'b0 ;
     assign intpriority_reg_re[i] =  1'b0 ;
     assign intenable_reg_we[i]   =  1'b0 ;
     assign intenable_reg_re[i]   =  1'b0 ;
     assign gw_config_reg_we[i]   =  1'b0 ;
     assign gw_config_reg_re[i]   =  1'b0 ;
     assign gw_clear_reg_we[i]    =  1'b0 ;
     assign gw_config_reg[i]    = '0 ;
     assign intpriority_reg[i] = {INTPRIORITY_BITS{1'b0}} ;
     assign intenable_reg[i]   = 1'b0 ;
     assign extintsrc_req_gw[i] = 1'b0 ;
 end
    assign intpriority_reg_inv[i] =  intpriord ? ~intpriority_reg[i] : intpriority_reg[i] ;
    assign intpend_w_prior_en[i]  =  {INTPRIORITY_BITS{(extintsrc_req_gw[i] & intenable_reg[i])}} & intpriority_reg_inv[i] ;
    assign intpend_id[i]          =  i ;
end
        assign pl_in[INTPRIORITY_BITS-1:0]                  =      selected_int_priority[INTPRIORITY_BITS-1:0] ;
        logic [NUM_LEVELS:0] [TOTAL_INT+1:0] [INTPRIORITY_BITS-1:0] level_intpend_w_prior_en;
        logic [NUM_LEVELS:0] [TOTAL_INT+1:0] [ID_BITS-1:0]          level_intpend_id;
        assign level_intpend_w_prior_en[0][TOTAL_INT+1:0] = {{2*INTPRIORITY_BITS{1'b0}},intpend_w_prior_en[TOTAL_INT-1:0]} ;
        assign level_intpend_id[0][TOTAL_INT+1:0] = {{2*ID_BITS{1'b1}},intpend_id[TOTAL_INT-1:0]} ;
        for (l=0; l<NUM_LEVELS ; l++) begin : LEVEL
           for (m=0; m<=(TOTAL_INT)/(2**(l+1)) ; m++) begin : COMPARE
              if ( m == (TOTAL_INT)/(2**(l+1))) begin
                   assign level_intpend_w_prior_en[l+1][m+1] = '0 ;
                   assign level_intpend_id[l+1][m+1]         = '0 ;
              end
              cmp_and_mux  #(.ID_BITS(ID_BITS),
                             .INTPRIORITY_BITS(INTPRIORITY_BITS)) cmp_l1 (
                             .a_id(level_intpend_id[l][2*m]),
                             .a_priority(level_intpend_w_prior_en[l][2*m]),
                             .b_id(level_intpend_id[l][2*m+1]),
                             .b_priority(level_intpend_w_prior_en[l][2*m+1]),
                             .out_id(level_intpend_id[l+1][m]),
                             .out_priority(level_intpend_w_prior_en[l+1][m])) ;
           end
        end
               assign claimid_in[ID_BITS-1:0]                      =      level_intpend_id[NUM_LEVELS][0] ;    
               assign selected_int_priority[INTPRIORITY_BITS-1:0]  =      level_intpend_w_prior_en[NUM_LEVELS][0] ;
assign config_reg_we               =  addr_config_pic_match & picm_wren_ff;
assign config_reg_re               =  addr_config_pic_match & picm_rden_ff;
assign config_reg_in  =  picm_wr_data_ff[0] ;   
rvdffs #(1) config_reg_ff  (.*, .clk(free_clk), .en(config_reg_we), .din (config_reg_in), .dout(config_reg));
assign intpriord  = config_reg ;
assign pl_in_q[INTPRIORITY_BITS-1:0] = intpriord ? ~pl_in : pl_in ;
rvdff #(ID_BITS)          claimid_ff (.*,  .din (claimid_in[ID_BITS-1:00]),    .dout(claimid[ID_BITS-1:00]),         .clk(free_clk));
rvdff  #(INTPRIORITY_BITS) pl_ff      (.*, .din (pl_in_q[INTPRIORITY_BITS-1:0]), .dout(pl[INTPRIORITY_BITS-1:0]),         .clk(free_clk));
logic [INTPRIORITY_BITS-1:0] meipt_inv , meicurpl_inv ;
assign meipt_inv[INTPRIORITY_BITS-1:0]    = intpriord ? ~meipt[INTPRIORITY_BITS-1:0]    : meipt[INTPRIORITY_BITS-1:0] ;
assign meicurpl_inv[INTPRIORITY_BITS-1:0] = intpriord ? ~meicurpl[INTPRIORITY_BITS-1:0] : meicurpl[INTPRIORITY_BITS-1:0] ;
assign mexintpend_in = (( selected_int_priority[INTPRIORITY_BITS-1:0] > meipt_inv[INTPRIORITY_BITS-1:0]) &
                        ( selected_int_priority[INTPRIORITY_BITS-1:0] > meicurpl_inv[INTPRIORITY_BITS-1:0]) );
rvdff #(1) mexintpend_ff  (.*, .clk(free_clk), .din (mexintpend_in), .dout(mexintpend));
assign maxint[INTPRIORITY_BITS-1:0]      =  intpriord ? 0 : 15 ;
assign mhwakeup_in = ( pl_in_q[INTPRIORITY_BITS-1:0] == maxint) ;
rvdff #(1) wake_up_ff  (.*, .clk(free_clk), .din (mhwakeup_in), .dout(mhwakeup));
assign intpend_reg_read     =  addr_intpend_base_match     & picm_rden_ff ;
assign intpriority_reg_read =  addr_intpriority_base_match & picm_rden_ff;
assign intenable_reg_read   =  addr_intenable_base_match   & picm_rden_ff;
assign gw_config_reg_read   =  addr_config_gw_base_match   & picm_rden_ff;
assign intpend_reg_extended[INTPEND_SIZE-1:0]  = {{INTPEND_SIZE-TOTAL_INT{1'b0}},extintsrc_req_gw[TOTAL_INT-1:0]} ;
   for (i=0; i<(INT_GRPS); i++) begin
            assign intpend_rd_part_out[i] =  (({32{intpend_reg_read & picm_addr_ff[5:2] == i}}) & intpend_reg_extended[((32*i)+31):(32*i)]) ;
   end
   always_comb begin : INTPEND_RD
         intpend_rd_out =  '0 ;
         for (int i=0; i<INT_GRPS; i++) begin
               intpend_rd_out |=  intpend_rd_part_out[i] ;
         end
   end
   always_comb begin : INTEN_RD
         intenable_rd_out =  '0 ;
         intpriority_rd_out =  '0 ;
         gw_config_rd_out =  '0 ;
         for (int i=0; i<TOTAL_INT; i++) begin
              if (intenable_reg_re[i]) begin
               intenable_rd_out    =  intenable_reg[i]  ;
              end
              if (intpriority_reg_re[i]) begin
               intpriority_rd_out  =  intpriority_reg[i] ;
              end
              if (gw_config_reg_re[i]) begin
               gw_config_rd_out  =  gw_config_reg[i] ;
              end
         end
   end
 assign picm_rd_data_in[31:0] = ({32{intpend_reg_read      }} &   intpend_rd_out                                                    ) |
                                ({32{intpriority_reg_read  }} &  {{32-INTPRIORITY_BITS{1'b0}}, intpriority_rd_out                 } ) |
                                ({32{intenable_reg_read    }} &  {31'b0 , intenable_rd_out                                        } ) |
                                ({32{gw_config_reg_read    }} &  {30'b0 , gw_config_rd_out                                        } ) |
                                ({32{config_reg_re         }} &  {31'b0 , config_reg                                              } ) |
                                ({32{picm_mken_ff & mask[3]}} &  {30'b0 , 2'b11                                                   } ) |
                                ({32{picm_mken_ff & mask[2]}} &  {31'b0 , 1'b1                                                    } ) |
                                ({32{picm_mken_ff & mask[1]}} &  {28'b0 , 4'b1111                                                 } ) |
                                ({32{picm_mken_ff & mask[0]}} &   32'b0                                                             ) ;
assign picm_rd_data[31:0] = picm_rd_data_in[31:0] ;
logic [14:0] address;
assign address[14:0] = picm_addr_ff[14:0];
always_comb begin
  case (address[14:0])
    15'b011000000000000 : mask[3:0] = 4'b0100;
    15'b100000000000100 : mask[3:0] = 4'b1000;
    15'b100000000001000 : mask[3:0] = 4'b1000;
    15'b100000000001100 : mask[3:0] = 4'b1000;
    15'b100000000010000 : mask[3:0] = 4'b1000;
    15'b100000000010100 : mask[3:0] = 4'b1000;
    15'b100000000011000 : mask[3:0] = 4'b1000;
    15'b100000000011100 : mask[3:0] = 4'b1000;
    15'b100000000100000 : mask[3:0] = 4'b1000;
    15'b010000000000100 : mask[3:0] = 4'b0100;
    15'b010000000001000 : mask[3:0] = 4'b0100;
    15'b010000000001100 : mask[3:0] = 4'b0100;
    15'b010000000010000 : mask[3:0] = 4'b0100;
    15'b010000000010100 : mask[3:0] = 4'b0100;
    15'b010000000011000 : mask[3:0] = 4'b0100;
    15'b010000000011100 : mask[3:0] = 4'b0100;
    15'b010000000100000 : mask[3:0] = 4'b0100;
    15'b000000000000100 : mask[3:0] = 4'b0010;
    15'b000000000001000 : mask[3:0] = 4'b0010;
    15'b000000000001100 : mask[3:0] = 4'b0010;
    15'b000000000010000 : mask[3:0] = 4'b0010;
    15'b000000000010100 : mask[3:0] = 4'b0010;
    15'b000000000011000 : mask[3:0] = 4'b0010;
    15'b000000000011100 : mask[3:0] = 4'b0010;
    15'b000000000100000 : mask[3:0] = 4'b0010;
    default           : mask[3:0] = 4'b0001;
  endcase
end
endmodule
module cmp_and_mux #(parameter ID_BITS=8,
                               INTPRIORITY_BITS = 4)
                    (
                        input  logic [ID_BITS-1:0]       a_id,
                        input  logic [INTPRIORITY_BITS-1:0] a_priority,
                        input  logic [ID_BITS-1:0]       b_id,
                        input  logic [INTPRIORITY_BITS-1:0] b_priority,
                        output logic [ID_BITS-1:0]       out_id,
                        output logic [INTPRIORITY_BITS-1:0] out_priority
                    );
logic   a_is_lt_b ;
assign  a_is_lt_b  = ( a_priority[INTPRIORITY_BITS-1:0] < b_priority[INTPRIORITY_BITS-1:0] ) ;
assign  out_id[ID_BITS-1:0]                = a_is_lt_b ? b_id[ID_BITS-1:0] :
                                                         a_id[ID_BITS-1:0] ;
assign  out_priority[INTPRIORITY_BITS-1:0] = a_is_lt_b ? b_priority[INTPRIORITY_BITS-1:0] :
                                                         a_priority[INTPRIORITY_BITS-1:0] ;
endmodule  
module configurable_gw (
                             input logic gw_clk,
                             input logic rst_l,
                             input logic extintsrc_req_sync ,
                             input logic meigwctrl_polarity ,
                             input logic meigwctrl_type ,
                             input logic meigwclr ,
                             output logic extintsrc_req_config
                            );
  logic  gw_int_pending_in , gw_int_pending ;
  assign gw_int_pending_in =  (extintsrc_req_sync ^ meigwctrl_polarity) | (gw_int_pending & ~meigwclr) ;
  rvdff #(1) int_pend_ff        (.*, .clk(gw_clk), .din (gw_int_pending_in),     .dout(gw_int_pending));
  assign extintsrc_req_config =  meigwctrl_type ? ((extintsrc_req_sync ^  meigwctrl_polarity) | gw_int_pending) : (extintsrc_req_sync ^  meigwctrl_polarity) ;
endmodule  
module swerv
   import swerv_types::*;
(
   input logic                  clk,
   input logic                  rst_l,
   input logic                  dbg_rst_l,
   input logic [31:1]           rst_vec,
   input logic                  nmi_int,
   input logic [31:1]           nmi_vec,
   output logic                 core_rst_l,    
   output logic [63:0] trace_rv_i_insn_ip,
   output logic [63:0] trace_rv_i_address_ip,
   output logic [2:0]  trace_rv_i_valid_ip,
   output logic [2:0]  trace_rv_i_exception_ip,
   output logic [4:0]  trace_rv_i_ecause_ip,
   output logic [2:0]  trace_rv_i_interrupt_ip,
   output logic [31:0] trace_rv_i_tval_ip,
   output logic                 lsu_freeze_dc3,
   output logic                 dccm_clk_override,
   output logic                 icm_clk_override,
   output logic                 dec_tlu_core_ecc_disable,
   input logic  i_cpu_halt_req,     
   input logic  i_cpu_run_req,      
   output logic o_cpu_halt_ack,     
   output logic o_cpu_halt_status,  
   output logic o_cpu_run_ack,      
   output logic o_debug_mode_status,  
   input logic mpc_debug_halt_req,  
   input logic mpc_debug_run_req,  
   input logic mpc_reset_run_req,  
   output logic mpc_debug_halt_ack,  
   output logic mpc_debug_run_ack,  
   output logic debug_brkpt_status,  
   output logic [1:0] dec_tlu_perfcnt0,  
   output logic [1:0] dec_tlu_perfcnt1,
   output logic [1:0] dec_tlu_perfcnt2,
   output logic [1:0] dec_tlu_perfcnt3,
   output logic                          dccm_wren,
   output logic                          dccm_rden,
   output logic [16-1:0]          dccm_wr_addr,
   output logic [16-1:0]          dccm_rd_addr_lo,
   output logic [16-1:0]          dccm_rd_addr_hi,
   output logic [39-1:0]   dccm_wr_data,
   input logic [39-1:0]    dccm_rd_data_lo,
   input logic [39-1:0]    dccm_rd_data_hi,
   output logic [31:2]           ic_rw_addr,
   output logic [3:0]            ic_tag_valid,
   output logic [3:0]            ic_wr_en,
   output logic                  ic_rd_en,
   output logic [67:0]               ic_wr_data,          
   input  logic [135:0]              ic_rd_data ,         
   input  logic [20:0]               ictag_debug_rd_data, 
   output logic [33:0]               ic_debug_wr_data,    
   output logic [127:0]              ic_premux_data,      
   output logic                      ic_sel_premux_data,  
   output logic [15:2]               ic_debug_addr,       
   output logic                      ic_debug_rd_en,      
   output logic                      ic_debug_wr_en,      
   output logic                      ic_debug_tag_array,  
   output logic [3:0]                ic_debug_way,        
   input  logic [3:0]            ic_rd_hit,
   input  logic                  ic_tag_perr,         
   output logic                            lsu_axi_awvalid,
   input  logic                            lsu_axi_awready,
   output logic [4-1:0]      lsu_axi_awid,
   output logic [31:0]                     lsu_axi_awaddr,
   output logic [3:0]                      lsu_axi_awregion,
   output logic [7:0]                      lsu_axi_awlen,
   output logic [2:0]                      lsu_axi_awsize,
   output logic [1:0]                      lsu_axi_awburst,
   output logic                            lsu_axi_awlock,
   output logic [3:0]                      lsu_axi_awcache,
   output logic [2:0]                      lsu_axi_awprot,
   output logic [3:0]                      lsu_axi_awqos,
   output logic                            lsu_axi_wvalid,
   input  logic                            lsu_axi_wready,
   output logic [63:0]                     lsu_axi_wdata,
   output logic [7:0]                      lsu_axi_wstrb,
   output logic                            lsu_axi_wlast,
   input  logic                            lsu_axi_bvalid,
   output logic                            lsu_axi_bready,
   input  logic [1:0]                      lsu_axi_bresp,
   input  logic [4-1:0]      lsu_axi_bid,
   output logic                            lsu_axi_arvalid,
   input  logic                            lsu_axi_arready,
   output logic [4-1:0]      lsu_axi_arid,
   output logic [31:0]                     lsu_axi_araddr,
   output logic [3:0]                      lsu_axi_arregion,
   output logic [7:0]                      lsu_axi_arlen,
   output logic [2:0]                      lsu_axi_arsize,
   output logic [1:0]                      lsu_axi_arburst,
   output logic                            lsu_axi_arlock,
   output logic [3:0]                      lsu_axi_arcache,
   output logic [2:0]                      lsu_axi_arprot,
   output logic [3:0]                      lsu_axi_arqos,
   input  logic                            lsu_axi_rvalid,
   output logic                            lsu_axi_rready,
   input  logic [4-1:0]      lsu_axi_rid,
   input  logic [63:0]                     lsu_axi_rdata,
   input  logic [1:0]                      lsu_axi_rresp,
   input  logic                            lsu_axi_rlast,
   output logic                            ifu_axi_awvalid,
   input  logic                            ifu_axi_awready,
   output logic [3-1:0]      ifu_axi_awid,
   output logic [31:0]                     ifu_axi_awaddr,
   output logic [3:0]                      ifu_axi_awregion,
   output logic [7:0]                      ifu_axi_awlen,
   output logic [2:0]                      ifu_axi_awsize,
   output logic [1:0]                      ifu_axi_awburst,
   output logic                            ifu_axi_awlock,
   output logic [3:0]                      ifu_axi_awcache,
   output logic [2:0]                      ifu_axi_awprot,
   output logic [3:0]                      ifu_axi_awqos,
   output logic                            ifu_axi_wvalid,
   input  logic                            ifu_axi_wready,
   output logic [63:0]                     ifu_axi_wdata,
   output logic [7:0]                      ifu_axi_wstrb,
   output logic                            ifu_axi_wlast,
   input  logic                            ifu_axi_bvalid,
   output logic                            ifu_axi_bready,
   input  logic [1:0]                      ifu_axi_bresp,
   input  logic [3-1:0]      ifu_axi_bid,
   output logic                            ifu_axi_arvalid,
   input  logic                            ifu_axi_arready,
   output logic [3-1:0]      ifu_axi_arid,
   output logic [31:0]                     ifu_axi_araddr,
   output logic [3:0]                      ifu_axi_arregion,
   output logic [7:0]                      ifu_axi_arlen,
   output logic [2:0]                      ifu_axi_arsize,
   output logic [1:0]                      ifu_axi_arburst,
   output logic                            ifu_axi_arlock,
   output logic [3:0]                      ifu_axi_arcache,
   output logic [2:0]                      ifu_axi_arprot,
   output logic [3:0]                      ifu_axi_arqos,
   input  logic                            ifu_axi_rvalid,
   output logic                            ifu_axi_rready,
   input  logic [3-1:0]      ifu_axi_rid,
   input  logic [63:0]                     ifu_axi_rdata,
   input  logic [1:0]                      ifu_axi_rresp,
   input  logic                            ifu_axi_rlast,
   output logic                            sb_axi_awvalid,
   input  logic                            sb_axi_awready,
   output logic [1-1:0]       sb_axi_awid,
   output logic [31:0]                     sb_axi_awaddr,
   output logic [3:0]                      sb_axi_awregion,
   output logic [7:0]                      sb_axi_awlen,
   output logic [2:0]                      sb_axi_awsize,
   output logic [1:0]                      sb_axi_awburst,
   output logic                            sb_axi_awlock,
   output logic [3:0]                      sb_axi_awcache,
   output logic [2:0]                      sb_axi_awprot,
   output logic [3:0]                      sb_axi_awqos,
   output logic                            sb_axi_wvalid,
   input  logic                            sb_axi_wready,
   output logic [63:0]                     sb_axi_wdata,
   output logic [7:0]                      sb_axi_wstrb,
   output logic                            sb_axi_wlast,
   input  logic                            sb_axi_bvalid,
   output logic                            sb_axi_bready,
   input  logic [1:0]                      sb_axi_bresp,
   input  logic [1-1:0]       sb_axi_bid,
   output logic                            sb_axi_arvalid,
   input  logic                            sb_axi_arready,
   output logic [1-1:0]       sb_axi_arid,
   output logic [31:0]                     sb_axi_araddr,
   output logic [3:0]                      sb_axi_arregion,
   output logic [7:0]                      sb_axi_arlen,
   output logic [2:0]                      sb_axi_arsize,
   output logic [1:0]                      sb_axi_arburst,
   output logic                            sb_axi_arlock,
   output logic [3:0]                      sb_axi_arcache,
   output logic [2:0]                      sb_axi_arprot,
   output logic [3:0]                      sb_axi_arqos,
   input  logic                            sb_axi_rvalid,
   output logic                            sb_axi_rready,
   input  logic [1-1:0]       sb_axi_rid,
   input  logic [63:0]                     sb_axi_rdata,
   input  logic [1:0]                      sb_axi_rresp,
   input  logic                            sb_axi_rlast,
   input  logic                         dma_axi_awvalid,
   output logic                         dma_axi_awready,
   input  logic [1-1:0]   dma_axi_awid,
   input  logic [31:0]                  dma_axi_awaddr,
   input  logic [2:0]                   dma_axi_awsize,
   input  logic [2:0]                   dma_axi_awprot,
   input  logic [7:0]                   dma_axi_awlen,
   input  logic [1:0]                   dma_axi_awburst,
   input  logic                         dma_axi_wvalid,
   output logic                         dma_axi_wready,
   input  logic [63:0]                  dma_axi_wdata,
   input  logic [7:0]                   dma_axi_wstrb,
   input  logic                         dma_axi_wlast,
   output logic                         dma_axi_bvalid,
   input  logic                         dma_axi_bready,
   output logic [1:0]                   dma_axi_bresp,
   output logic [1-1:0]   dma_axi_bid,
   input  logic                         dma_axi_arvalid,
   output logic                         dma_axi_arready,
   input  logic [1-1:0]   dma_axi_arid,
   input  logic [31:0]                  dma_axi_araddr,
   input  logic [2:0]                   dma_axi_arsize,
   input  logic [2:0]                   dma_axi_arprot,
   input  logic [7:0]                   dma_axi_arlen,
   input  logic [1:0]                   dma_axi_arburst,
   output logic                         dma_axi_rvalid,
   input  logic                         dma_axi_rready,
   output logic [1-1:0]   dma_axi_rid,
   output logic [63:0]                  dma_axi_rdata,
   output logic [1:0]                   dma_axi_rresp,
   output logic                         dma_axi_rlast,
   input   logic                 lsu_bus_clk_en,
   input   logic                 ifu_bus_clk_en,
   input   logic                 dbg_bus_clk_en,
   input   logic                 dma_bus_clk_en,
   input  logic                  dmi_reg_en,
   input  logic [6:0]            dmi_reg_addr,
   input  logic                  dmi_reg_wr_en,
   input  logic [31:0]           dmi_reg_wdata,
   output logic [31:0]           dmi_reg_rdata,
   input  logic                  dmi_hard_reset,
   input logic [8:1]           extintsrc_req,
   input logic                   timer_int,
   input logic                   scan_mode
);
localparam TOTAL_INT        = 9;
localparam DCCM_BITS        = 16;
localparam DCCM_BANK_BITS   = 3;
localparam DCCM_NUM_BANKS   = 8;
localparam DCCM_DATA_WIDTH  = 32;
localparam DCCM_FDATA_WIDTH = 39;
localparam DCCM_BYTE_WIDTH  = 4;
localparam DCCM_ECC_WIDTH   = 7;
localparam LSU_RDBUF_DEPTH  = 8;
localparam DMA_BUF_DEPTH    = 4;
localparam LSU_STBUF_DEPTH  = 8;
localparam LSU_SB_BITS      = 16;
localparam DEC_INSTBUF_DEPTH = 4;
localparam ICCM_SIZE         = 512;
localparam ICCM_BITS         = 19;
localparam ICCM_NUM_BANKS    = 8;
localparam ICCM_BANK_BITS    = 3;
localparam ICCM_INDEX_BITS   = 14;
localparam ICCM_BANK_HI      = 4 + (3/4);
localparam ICACHE_TAG_HIGH  = 12;
localparam ICACHE_TAG_LOW   = 6;
localparam ICACHE_IC_DEPTH  = 8;
localparam ICACHE_TAG_DEPTH = 64;
localparam LSU_BUS_TAG     = 4;
localparam DMA_BUS_TAG     = 1;
localparam SB_BUS_TAG      = 1;
localparam IFU_BUS_TAG     = 3;
   logic [1:0]                   ifu_pmu_instr_aligned;
   logic                         ifu_pmu_align_stall;
   logic                         dma_slv_algn_err;
   logic [33:0] ifu_ic_debug_rd_data;  
   logic ifu_ic_debug_rd_data_valid;  
   cache_debug_pkt_t dec_tlu_ic_diag_pkt;  
   logic  [31:0] gpr_i0_rs1_d;
   logic  [31:0] gpr_i0_rs2_d;
   logic  [31:0] gpr_i1_rs1_d;
   logic  [31:0] gpr_i1_rs2_d;
   logic [31:0] i0_rs1_bypass_data_d;
   logic [31:0] i0_rs2_bypass_data_d;
   logic [31:0] i1_rs1_bypass_data_d;
   logic [31:0] i1_rs2_bypass_data_d;
   logic [31:0] exu_i0_result_e1, exu_i1_result_e1;
   logic  [31:1] exu_i0_pc_e1;
   logic  [31:1] exu_i1_pc_e1;   
   logic [31:1]  exu_npc_e4;
   logic [31:1] dec_tlu_i0_pc_e4;
   logic [31:1] dec_tlu_i1_pc_e4;
   alu_pkt_t  i0_ap, i1_ap;
   trigger_pkt_t [3:0]     trigger_pkt_any;
   logic [3:0]             lsu_trigger_match_dc3;
   logic        dec_ib3_valid_d, dec_ib2_valid_d;
   logic        dec_ib1_valid_eff_d;
   logic        dec_ib0_valid_eff_d;
   logic [31:0] dec_i0_immed_d;
   logic [31:0] dec_i1_immed_d;
   logic [12:1] dec_i0_br_immed_d;
   logic [12:1] dec_i1_br_immed_d;
   logic         dec_i0_select_pc_d;
   logic         dec_i1_select_pc_d;
   logic [31:1] dec_i0_pc_d, dec_i1_pc_d;
   logic        dec_i0_rs1_bypass_en_d;
   logic        dec_i0_rs2_bypass_en_d;
   logic        dec_i1_rs1_bypass_en_d;
   logic        dec_i1_rs2_bypass_en_d;
   logic         dec_i0_alu_decode_d;
   logic         dec_i1_alu_decode_d;
   rets_pkt_t exu_rets_e1_pkt;
   rets_pkt_t exu_rets_e4_pkt;
   logic         dec_tlu_cancel_e4;
   logic         ifu_miss_state_idle;
   logic         dec_tlu_flush_noredir_wb;
   logic         dec_tlu_flush_leak_one_wb;
   logic         dec_tlu_flush_err_wb;
   logic         ifu_i0_valid, ifu_i1_valid;
   logic [31:0]  ifu_i0_instr, ifu_i1_instr;
   logic [31:1]  ifu_i0_pc, ifu_i1_pc;
   logic        exu_i0_flush_final;
   logic        exu_i1_flush_final;
   logic        exu_flush_final;     
   logic        exu_flush_upper_e2;     
   logic [31:1] exu_flush_path_final;
   logic [31:0] exu_lsu_rs1_d;
   logic [31:0] exu_lsu_rs2_d;
   lsu_pkt_t    lsu_p;
   logic [11:0] dec_lsu_offset_d;
   logic        dec_i0_lsu_d;        
   logic        dec_i1_lsu_d;
   logic [31:0]  lsu_result_dc3;
   logic [31:0]  lsu_result_corr_dc4;     
   lsu_error_pkt_t lsu_error_pkt_dc3;
   logic         lsu_single_ecc_error_incr;     
   logic         lsu_freeze_external_ints_dc3;
   logic         lsu_imprecise_error_load_any;
   logic         lsu_imprecise_error_store_any;
   logic [31:0]  lsu_imprecise_error_addr_any;
   logic         lsu_load_stall_any;        
   logic         lsu_store_stall_any;        
   logic         lsu_load_ecc_stbuf_full_dc3;    
   logic         lsu_idle_any;
   logic         lsu_halt_idle_any;    
   logic                                  lsu_nonblock_load_valid_dc3;
   logic [3-1:0]   lsu_nonblock_load_tag_dc3;
   logic                                  lsu_nonblock_load_inv_dc5;
   logic [3-1:0]   lsu_nonblock_load_inv_tag_dc5;
   logic                                  lsu_nonblock_load_data_valid;
   logic [3-1:0]   lsu_nonblock_load_data_tag;
   logic [31:0]                           lsu_nonblock_load_data;
   logic        flush_final_e3;
   logic        i0_flush_final_e3;
   logic        dec_csr_ren_d;
   logic [31:0] exu_csr_rs1_e1;
   logic        dec_tlu_flush_lower_wb;
   logic        dec_tlu_i0_kill_writeb_wb;     
   logic        dec_tlu_i1_kill_writeb_wb;     
   logic        dec_tlu_fence_i_wb;            
   logic dec_tlu_i0_valid_e4;
   logic dec_tlu_i1_valid_e4;
   logic [31:1] dec_tlu_flush_path_wb;
   logic [31:0] dec_tlu_mrac_ff;         
   logic        ifu_i0_pc4, ifu_i1_pc4;
   mul_pkt_t  mul_p;
   logic [31:0] exu_mul_result_e3;
   logic dec_i0_mul_d;
   logic dec_i1_mul_d;
   div_pkt_t  div_p;
   logic [31:0] exu_div_result;
   logic exu_div_finish;
   logic exu_div_stall;
   logic dec_i0_div_d;
   logic dec_i1_div_d;
   logic [15:0] ifu_illegal_inst;
   logic        dec_i1_valid_e1;
   logic        dec_div_decode_e4;
   logic [31:1] pred_correct_npc_e2;
   logic [31:0] exu_i0_result_e4;
   logic [31:0] exu_i1_result_e4;
   logic        dec_i0_rs1_bypass_en_e3;
   logic        dec_i0_rs2_bypass_en_e3;
   logic        dec_i1_rs1_bypass_en_e3;
   logic        dec_i1_rs2_bypass_en_e3;
   logic [31:0] i0_rs1_bypass_data_e3;
   logic [31:0] i0_rs2_bypass_data_e3;
   logic [31:0] i1_rs1_bypass_data_e3;
   logic [31:0] i1_rs2_bypass_data_e3;
   logic        dec_i0_sec_decode_e3;
   logic        dec_i1_sec_decode_e3;
   logic [31:1] dec_i0_pc_e3;
   logic [31:1] dec_i1_pc_e3;
   logic        dec_i0_rs1_bypass_en_e2;
   logic        dec_i0_rs2_bypass_en_e2;
   logic        dec_i1_rs1_bypass_en_e2;
   logic        dec_i1_rs2_bypass_en_e2;
   logic [31:0] i0_rs1_bypass_data_e2;
   logic [31:0] i0_rs2_bypass_data_e2;
   logic [31:0] i1_rs1_bypass_data_e2;
   logic [31:0] i1_rs2_bypass_data_e2;
   logic        exu_i0_flush_lower_e4;      
   logic        exu_i1_flush_lower_e4;
   logic [31:1] exu_i0_flush_path_e4;
   logic [31:1] exu_i1_flush_path_e4;
   br_tlu_pkt_t dec_tlu_br0_wb_pkt;
   br_tlu_pkt_t dec_tlu_br1_wb_pkt;
   predict_pkt_t  exu_mp_pkt;
   logic [4:0]  exu_mp_eghr;
   logic [4:0]  exu_i0_br_fghr_e4;
   logic [1:0]  exu_i0_br_hist_e4;
   logic [1:0]  exu_i0_br_bank_e4;
   logic        exu_i0_br_error_e4;
   logic        exu_i0_br_start_error_e4;
   logic        exu_i0_br_valid_e4;
   logic        exu_i0_br_mp_e4;
   logic        exu_i0_br_ret_e4;
   logic        exu_i0_br_call_e4;
   logic        exu_i0_br_middle_e4;
   logic [4:0]  exu_i1_br_fghr_e4;
   logic [1:0]  exu_i1_br_hist_e4;
   logic [1:0]  exu_i1_br_bank_e4;
   logic        exu_i1_br_error_e4;
   logic        exu_i1_br_start_error_e4;
   logic        exu_i1_br_valid_e4;
   logic        exu_i1_br_mp_e4;
   logic        exu_i1_br_ret_e4;
   logic        exu_i1_br_call_e4;
   logic        exu_i1_br_middle_e4;
   logic        exu_i0_br_way_e4;
   logic        exu_i1_br_way_e4;
   logic        dec_i0_lsu_decode_d;
   logic [5:4] exu_i0_br_index_e4;
   logic [5:4] exu_i1_br_index_e4;
   logic        dma_dccm_req;
   logic        dma_iccm_req;
   logic [31:0] dma_mem_addr;
   logic [2:0]  dma_mem_sz;
   logic        dma_mem_write;
   logic [63:0] dma_mem_wdata;
   logic        dccm_dma_rvalid;
   logic        dccm_dma_ecc_error;
   logic [63:0] dccm_dma_rdata;
   logic        iccm_dma_rvalid;
   logic        iccm_dma_ecc_error;
   logic [63:0] iccm_dma_rdata;
   logic        dma_dccm_stall_any;        
   logic        dma_iccm_stall_any;        
   logic        dccm_ready;
   logic        iccm_ready;
   logic [31:0] i0_result_e4_eff;
   logic [31:0] i1_result_e4_eff;
   logic [31:0] i0_result_e2;
   logic        ifu_i0_icaf;
   logic        ifu_i1_icaf;
   logic        ifu_i0_icaf_second;
   logic        ifu_i1_icaf_second;
   logic        ifu_i0_perr;
   logic        ifu_i1_perr;
   logic        ifu_i0_sbecc;
   logic        ifu_i1_sbecc;
   logic        ifu_i0_dbecc;
   logic        ifu_i1_dbecc;
   logic        iccm_dma_sb_error;
   br_pkt_t i0_brp;
   br_pkt_t i1_brp;
   predict_pkt_t  i0_predict_p_d;
   predict_pkt_t  i1_predict_p_d;
   logic                  picm_wren;
   logic                  picm_rden;
   logic                  picm_mken;
   logic [31:0]           picm_addr;
   logic [31:0]           picm_wr_data;
   logic [31:0]           picm_rd_data;
   logic  dec_tlu_sec_alu_disable;
   logic  dec_tlu_non_blocking_disable;
   logic  dec_tlu_fast_div_disable;
   logic  dec_tlu_bpred_disable;
   logic  dec_tlu_wb_coalescing_disable;
   logic  dec_tlu_ld_miss_byp_wb_disable;
   logic  dec_tlu_sideeffect_posted_disable;
   logic [2:0] dec_tlu_dma_qos_prty;
   logic  dec_tlu_misc_clk_override;
   logic  dec_tlu_exu_clk_override;
   logic  dec_tlu_ifu_clk_override;
   logic  dec_tlu_lsu_clk_override;
   logic  dec_tlu_bus_clk_override;
   logic  dec_tlu_pic_clk_override;
   logic  dec_tlu_dccm_clk_override;
   logic  dec_tlu_icm_clk_override;
   assign        dccm_clk_override = dec_tlu_dccm_clk_override;    
   assign        icm_clk_override = dec_tlu_icm_clk_override;     
   logic [31:0]            dbg_cmd_addr;               
   logic [31:0]            dbg_cmd_wrdata;             
   logic                   dbg_cmd_valid;              
   logic                   dbg_cmd_write;              
   logic [1:0]             dbg_cmd_type;               
   logic [1:0]             dbg_cmd_size;               
   logic                   dbg_halt_req;               
   logic                   dbg_resume_req;             
   logic                   dbg_core_rst_l;             
   logic                   core_dbg_cmd_done;          
   logic                   core_dbg_cmd_fail;          
   logic [31:0]            core_dbg_rddata;            
   logic                   dma_dbg_cmd_done;           
   logic                   dma_dbg_cmd_fail;           
   logic [31:0]            dma_dbg_rddata;             
   logic                   dbg_dma_bubble;             
   logic                   dma_dbg_ready;              
   logic [31:0]            dec_dbg_rddata;             
   logic                   dec_dbg_cmd_done;           
   logic                   dec_dbg_cmd_fail;           
   logic                   dec_tlu_mpc_halted_only;    
   logic                   dec_tlu_dbg_halted;         
   logic                   dec_tlu_pmu_fw_halted;      
   logic                   dec_tlu_resume_ack;
   logic                   dec_tlu_debug_mode;         
   logic                   dec_debug_wdata_rs1_d;
   logic                   dec_tlu_stall_dma;          
   logic [4:2]             dec_i0_data_en;
   logic [4:1]             dec_i0_ctl_en;
   logic [4:2]             dec_i1_data_en;
   logic [4:1]             dec_i1_ctl_en;
   logic                   dec_nonblock_load_freeze_dc2;
   logic                   exu_pmu_i0_br_misp;
   logic                   exu_pmu_i0_br_ataken;
   logic                   exu_pmu_i0_pc4;
   logic                   exu_pmu_i1_br_misp;
   logic                   exu_pmu_i1_br_ataken;
   logic                   exu_pmu_i1_pc4;
   logic                   lsu_pmu_misaligned_dc3;
   logic                   lsu_pmu_bus_trxn;
   logic                   lsu_pmu_bus_misaligned;
   logic                   lsu_pmu_bus_error;
   logic                   lsu_pmu_bus_busy;
   logic                   ifu_pmu_fetch_stall;
   logic                   ifu_pmu_ic_miss;
   logic                   ifu_pmu_ic_hit;
   logic                   ifu_pmu_bus_error;
   logic                   ifu_pmu_bus_busy;
   logic                   ifu_pmu_bus_trxn;
   logic                   active_state;
   logic                   free_clk, active_clk;
   logic                   dec_pause_state_cg;
   logic                   lsu_nonblock_load_data_error;
   logic [15:0]            ifu_i0_cinst;
   logic [15:0]            ifu_i1_cinst;
   trace_pkt_t  trace_rv_trace_pkt;
   assign active_state = ~dec_pause_state_cg | dec_tlu_flush_lower_wb | dec_tlu_misc_clk_override;
   rvoclkhdr free_cg   ( .en(1'b1),         .l1clk(free_clk), .* );
   rvoclkhdr active_cg ( .en(active_state), .l1clk(active_clk), .* );
   assign core_dbg_cmd_done = dma_dbg_cmd_done | dec_dbg_cmd_done;
   assign core_dbg_cmd_fail = dma_dbg_cmd_fail | dec_dbg_cmd_fail;
   assign core_dbg_rddata[31:0] = dma_dbg_cmd_done ? dma_dbg_rddata[31:0] : dec_dbg_rddata[31:0];
   dbg dbg (
      .clk_override(dec_tlu_misc_clk_override),
      .*
   );
   assign core_rst_l = rst_l & (dbg_core_rst_l | scan_mode);
   ifu ifu (
       .clk_override(dec_tlu_ifu_clk_override),
       .rst_l(core_rst_l),
       .*
   );
   dec dec (
            .dbg_cmd_wrdata(dbg_cmd_wrdata[1:0]),
            .rst_l(core_rst_l),
            .*
            );
   exu exu (
      .clk_override(dec_tlu_exu_clk_override),
      .rst_l(core_rst_l),
      .*
   );
   lsu lsu (
      .clk_override(dec_tlu_lsu_clk_override),
      .rst_l(core_rst_l),
      .*
   );
   logic [7:0]  pic_claimid;
   logic [3:0]  pic_pl, dec_tlu_meicurpl, dec_tlu_meipt;
   logic        mexintpend;
   logic        mhwakeup;
   logic        dec_tlu_claim_ack_wb;
   pic_ctrl  pic_ctrl_inst (
                  .clk_override(dec_tlu_pic_clk_override),
                  .picm_mken (picm_mken),
                  .extintsrc_req({extintsrc_req[8:1],1'b0}),
                  .pl(pic_pl[3:0]),
                  .claimid(pic_claimid[7:0]),
                  .meicurpl(dec_tlu_meicurpl[3:0]),
                  .meipt(dec_tlu_meipt[3:0]),
                  .rst_l(core_rst_l),
                     .*);
   dma_ctrl dma_ctrl (
      .rst_l(core_rst_l),
      .clk_override(dec_tlu_misc_clk_override),
      .*
   );
      assign trace_rv_i_insn_ip[63:0]     = trace_rv_trace_pkt.trace_rv_i_insn_ip[63:0];
      assign trace_rv_i_address_ip[63:0]  = trace_rv_trace_pkt.trace_rv_i_address_ip[63:0];
      assign trace_rv_i_valid_ip[2:0]     = trace_rv_trace_pkt.trace_rv_i_valid_ip[2:0];
      assign trace_rv_i_exception_ip[2:0] = trace_rv_trace_pkt.trace_rv_i_exception_ip[2:0];
      assign trace_rv_i_ecause_ip[4:0]    = trace_rv_trace_pkt.trace_rv_i_ecause_ip[4:0];
      assign trace_rv_i_interrupt_ip[2:0] = trace_rv_trace_pkt.trace_rv_i_interrupt_ip[2:0];
      assign trace_rv_i_tval_ip[31:0]     = trace_rv_trace_pkt.trace_rv_i_tval_ip[31:0];
endmodule  
module dma_ctrl (
   input logic         clk,
   input logic         free_clk,
   input logic         rst_l,
   input logic         dma_bus_clk_en,  
   input logic         clk_override,
   input  logic                         dma_axi_awvalid,
   output logic                         dma_axi_awready,
   input  logic [1-1:0]   dma_axi_awid,
   input  logic [31:0]                  dma_axi_awaddr,
   input  logic [2:0]                   dma_axi_awsize,
   input  logic [2:0]                   dma_axi_awprot,
   input  logic [7:0]                   dma_axi_awlen,
   input  logic [1:0]                   dma_axi_awburst,
   input  logic                         dma_axi_wvalid,
   output logic                         dma_axi_wready,
   input  logic [63:0]                  dma_axi_wdata,
   input  logic [7:0]                   dma_axi_wstrb,
   input  logic                         dma_axi_wlast,
   output logic                         dma_axi_bvalid,
   input  logic                         dma_axi_bready,
   output logic [1:0]                   dma_axi_bresp,
   output logic [1-1:0]   dma_axi_bid,
   input  logic                         dma_axi_arvalid,
   output logic                         dma_axi_arready,
   input  logic [1-1:0]   dma_axi_arid,
   input  logic [31:0]                  dma_axi_araddr,
   input  logic [2:0]                   dma_axi_arsize,
   input  logic [2:0]                   dma_axi_arprot,
   input  logic [7:0]                   dma_axi_arlen,
   input  logic [1:0]                   dma_axi_arburst,
   output logic                         dma_axi_rvalid,
   input  logic                         dma_axi_rready,
   output logic [1-1:0]   dma_axi_rid,
   output logic [63:0]                  dma_axi_rdata,
   output logic [1:0]                   dma_axi_rresp,
   output logic                         dma_axi_rlast,
   output logic                        dma_slv_algn_err,
   input logic [31:0]  dbg_cmd_addr,
   input logic [31:0]  dbg_cmd_wrdata,
   input logic         dbg_cmd_valid,
   input logic         dbg_cmd_write,  
   input logic [1:0]   dbg_cmd_type,  
   input logic [1:0]   dbg_cmd_size,  
   input  logic        dbg_dma_bubble,    
   output logic        dma_dbg_ready,     
   output logic        dma_dbg_cmd_done,
   output logic        dma_dbg_cmd_fail,
   output logic [31:0] dma_dbg_rddata,
   output logic        dma_dccm_req,  
   output logic        dma_iccm_req,  
   output logic [31:0] dma_mem_addr,  
   output logic [2:0]  dma_mem_sz,  
   output logic        dma_mem_write,  
   output logic [63:0] dma_mem_wdata,  
   input logic         dccm_dma_rvalid,     
   input logic         dccm_dma_ecc_error,  
   input logic [63:0]  dccm_dma_rdata,      
   input logic         iccm_dma_rvalid,     
   input logic         iccm_dma_ecc_error,  
   input logic [63:0]  iccm_dma_rdata,      
   output logic        dma_dccm_stall_any,  
   output logic        dma_iccm_stall_any,  
   input logic         dccm_ready,  
   input logic         iccm_ready,  
   input logic         dec_tlu_stall_dma,  
   input logic [2:0]   dec_tlu_dma_qos_prty,     
   input logic         scan_mode
);
localparam TOTAL_INT        = 9;
localparam DCCM_BITS        = 16;
localparam DCCM_BANK_BITS   = 3;
localparam DCCM_NUM_BANKS   = 8;
localparam DCCM_DATA_WIDTH  = 32;
localparam DCCM_FDATA_WIDTH = 39;
localparam DCCM_BYTE_WIDTH  = 4;
localparam DCCM_ECC_WIDTH   = 7;
localparam LSU_RDBUF_DEPTH  = 8;
localparam DMA_BUF_DEPTH    = 4;
localparam LSU_STBUF_DEPTH  = 8;
localparam LSU_SB_BITS      = 16;
localparam DEC_INSTBUF_DEPTH = 4;
localparam ICCM_SIZE         = 512;
localparam ICCM_BITS         = 19;
localparam ICCM_NUM_BANKS    = 8;
localparam ICCM_BANK_BITS    = 3;
localparam ICCM_INDEX_BITS   = 14;
localparam ICCM_BANK_HI      = 4 + (3/4);
localparam ICACHE_TAG_HIGH  = 12;
localparam ICACHE_TAG_LOW   = 6;
localparam ICACHE_IC_DEPTH  = 8;
localparam ICACHE_TAG_DEPTH = 64;
localparam LSU_BUS_TAG     = 4;
localparam DMA_BUS_TAG     = 1;
localparam SB_BUS_TAG      = 1;
localparam IFU_BUS_TAG     = 3;
   localparam DEPTH = DMA_BUF_DEPTH;
   localparam DEPTH_PTR = $clog2(DEPTH);
   localparam NACK_COUNT = 7;
   logic [DEPTH-1:0]        fifo_valid;
   logic [DEPTH-1:0][1:0]   fifo_error;
   logic [DEPTH-1:0]        fifo_dccm_valid;
   logic [DEPTH-1:0]        fifo_iccm_valid;
   logic [DEPTH-1:0]        fifo_data_valid;
   logic [DEPTH-1:0]        fifo_data_bus_valid;
   logic [DEPTH-1:0]        fifo_error_bus;
   logic [DEPTH-1:0]        fifo_rpend;
   logic [DEPTH-1:0]        fifo_done;       
   logic [DEPTH-1:0]        fifo_done_bus;   
   logic [DEPTH-1:0]        fifo_rsp_done;   
   logic [DEPTH-1:0][31:0]  fifo_addr;
   logic [DEPTH-1:0][2:0]   fifo_sz;
   logic [DEPTH-1:0]        fifo_write;
   logic [DEPTH-1:0]        fifo_posted_write;
   logic [DEPTH-1:0]        fifo_dbg;
   logic [DEPTH-1:0][63:0]  fifo_data;
   logic [DEPTH-1:0][DMA_BUS_TAG-1:0]  fifo_tag;
   logic [DEPTH-1:0]        fifo_cmd_en;
   logic [DEPTH-1:0]        fifo_valid_en;
   logic [DEPTH-1:0]        fifo_data_en;
   logic [DEPTH-1:0]        fifo_data_bus_en;
   logic [DEPTH-1:0]        fifo_pend_en;
   logic [DEPTH-1:0]        fifo_done_en;
   logic [DEPTH-1:0]        fifo_done_bus_en;
   logic [DEPTH-1:0]        fifo_error_en;
   logic [DEPTH-1:0]        fifo_error_bus_en;
   logic [DEPTH-1:0]        fifo_reset;
   logic [DEPTH-1:0][1:0]   fifo_error_in;
   logic [DEPTH-1:0][63:0]  fifo_data_in;
   logic                    fifo_write_in;
   logic                    fifo_posted_write_in;
   logic                    fifo_dbg_in;
   logic [31:0]             fifo_addr_in;
   logic [2:0]              fifo_sz_in;
   logic [DEPTH_PTR-1:0]    RspPtr, PrevRspPtr, NxtRspPtr;
   logic [DEPTH_PTR-1:0]    WrPtr, NxtWrPtr;
   logic [DEPTH_PTR-1:0]    RdPtr, NxtRdPtr;
   logic [DEPTH_PTR-1:0]    RdPtr_Q1, RdPtr_Q2, RdPtr_Q3;
   logic                    WrPtrEn, RdPtrEn, RspPtrEn;
   logic [1:0]              dma_dbg_sz;
   logic [1:0]              dma_dbg_addr;
   logic [31:0]             dma_dbg_mem_rddata;
   logic [31:0]             dma_dbg_mem_wrdata;
   logic                    dma_dbg_cmd_error_in;
   logic                    dma_dbg_cmd_done_q;
   logic                    fifo_full, fifo_full_spec, fifo_empty;
   logic                    dma_address_error, dma_alignment_error;
   logic [3:0]              num_fifo_vld;
   logic                    dma_mem_req;
   logic                    dma_addr_in_dccm;
   logic                    dma_addr_in_iccm;
   logic                    dma_addr_in_pic;
   logic                    dma_addr_in_pic_region_nc;
   logic                    dma_addr_in_dccm_region_nc;
   logic                    dma_addr_in_iccm_region_nc;
   logic [2:0]              dma_nack_count_csr;
   logic [2:0]              dma_nack_count, dma_nack_count_d;
   logic                    dma_buffer_c1_clken;
   logic                    dma_free_clken;
   logic                    dma_buffer_c1_clk;
   logic                    dma_free_clk;
   logic                    dma_bus_clk;
   logic                    wrbuf_en, wrbuf_data_en;
   logic                    wrbuf_cmd_sent, wrbuf_rst, wrbuf_data_rst;
   logic                    wrbuf_vld;
   logic                    wrbuf_data_vld;
   logic                    wrbuf_posted;
   logic [DMA_BUS_TAG-1:0]  wrbuf_tag;
   logic [2:0]              wrbuf_size;
   logic [31:0]             wrbuf_addr;
   logic [63:0]             wrbuf_data;
   logic [7:0]              wrbuf_byteen;
   logic                    rdbuf_en;
   logic                    rdbuf_cmd_sent, rdbuf_rst;
   logic                    rdbuf_vld;
   logic [DMA_BUS_TAG-1:0]  rdbuf_tag;
   logic [2:0]              rdbuf_size;
   logic [31:0]             rdbuf_addr;
   logic                    axi_mstr_valid, axi_mstr_valid_q;
   logic                    axi_mstr_write;
   logic                    axi_mstr_posted_write;
   logic [DMA_BUS_TAG-1:0]  axi_mstr_tag;
   logic [31:0]             axi_mstr_addr;
   logic [2:0]              axi_mstr_size;
   logic [63:0]             axi_mstr_wdata;
   logic [7:0]              axi_mstr_wstrb;
   logic                    axi_mstr_prty_in, axi_mstr_prty_en;
   logic                    axi_mstr_priority;
   logic                    axi_mstr_sel;
   logic                    axi_slv_valid;
   logic                    axi_slv_sent, axi_slv_sent_q;
   logic                    axi_slv_write;
   logic                    axi_slv_posted_write;
   logic [DMA_BUS_TAG-1:0]  axi_slv_tag;
   logic [1:0]              axi_slv_error;
   logic [63:0]             axi_slv_rdata;
   logic                    dma_bus_clk_en_q;
   logic                    fifo_full_spec_bus;
   logic                    dbg_dma_bubble_bus;
   logic                    dec_tlu_stall_dma_bus;
   logic                    dma_fifo_ready;
   assign fifo_addr_in[31:0]    = dbg_cmd_valid ? dbg_cmd_addr[31:0] : axi_mstr_addr[31:0];
   assign fifo_sz_in[2:0]       = dbg_cmd_valid ? {1'b0,dbg_cmd_size[1:0]} : axi_mstr_size[2:0];
   assign fifo_write_in         = dbg_cmd_valid ? dbg_cmd_write : axi_mstr_write;
   assign fifo_posted_write_in  = axi_mstr_valid & axi_mstr_posted_write;
   assign fifo_dbg_in           = dbg_cmd_valid;
   for (genvar i=0 ;i<DEPTH; i++) begin: GenFifo
      assign fifo_valid_en[i] = axi_mstr_valid & (i == WrPtr[DEPTH_PTR-1:0]);
      assign fifo_cmd_en[i]   = ((axi_mstr_valid & dma_bus_clk_en) | (dbg_cmd_valid & dbg_cmd_type[1])) &
                                (i == WrPtr[DEPTH_PTR-1:0]);
      assign fifo_data_en[i] = (((axi_mstr_valid & (axi_mstr_write | dma_address_error | dma_alignment_error) & dma_bus_clk_en) |
                                 (dbg_cmd_valid & dbg_cmd_type[1] & dbg_cmd_write))  & (i == WrPtr[DEPTH_PTR-1:0])) |
                               ((dccm_dma_rvalid & (i == RdPtr_Q3[DEPTH_PTR-1:0]))| (iccm_dma_rvalid & (i == RdPtr_Q2[DEPTH_PTR-1:0])));
      assign fifo_data_bus_en[i] = (fifo_data_en[i] | fifo_data_valid[i]) & dma_bus_clk_en;
      assign fifo_pend_en[i] = (dma_dccm_req | dma_iccm_req) & ~dma_mem_write & (i == RdPtr[DEPTH_PTR-1:0]);
      assign fifo_error_en[i] = fifo_cmd_en[i] | (((dccm_dma_rvalid & dccm_dma_ecc_error & (i == RdPtr_Q3[DEPTH_PTR-1:0])) | (iccm_dma_rvalid & iccm_dma_ecc_error & (i == RdPtr_Q2[DEPTH_PTR-1:0]))));
      assign fifo_error_bus_en[i] = (((|fifo_error_in[i][1:0]) & fifo_error_en[i]) | (|fifo_error[i])) & dma_bus_clk_en;
      assign fifo_done_en[i] = (((|fifo_error[i]) | ((dma_dccm_req | dma_iccm_req) & dma_mem_write)) & (i == RdPtr[DEPTH_PTR-1:0])) |
                               ((dccm_dma_rvalid & (i == RdPtr_Q3[DEPTH_PTR-1:0])) | (iccm_dma_rvalid & (i == RdPtr_Q2[DEPTH_PTR-1:0])));
      assign fifo_done_bus_en[i] = (fifo_done_en[i] | fifo_done[i]) & dma_bus_clk_en;
      assign fifo_reset[i]   = ((axi_slv_sent & dma_bus_clk_en) | dma_dbg_cmd_done) & (i == RspPtr[DEPTH_PTR-1:0]);
      assign fifo_error_in[i]  = (dccm_dma_rvalid & (i == RdPtr_Q3[DEPTH_PTR-1:0])) ? {1'b0,dccm_dma_ecc_error} : (iccm_dma_rvalid & (i == RdPtr_Q2[DEPTH_PTR-1:0])) ? {1'b0,iccm_dma_ecc_error}  :
                                                                                                                                                {(dma_address_error | dma_alignment_error | dma_dbg_cmd_error_in), dma_alignment_error};
      assign fifo_data_in[i]   = (fifo_error_en[i] & (|fifo_error_in[i])) ? (fifo_cmd_en[i] ? {32'b0,axi_mstr_addr[31:0]} : {32'b0,fifo_addr[i]}) :
                                                                            ((dccm_dma_rvalid & (i == RdPtr_Q3[DEPTH_PTR-1:0]))  ? dccm_dma_rdata[63:0] : (iccm_dma_rvalid & (i == RdPtr_Q2[DEPTH_PTR-1:0])) ? iccm_dma_rdata[63:0] :
                                                                                                                                                       (dbg_cmd_valid ? {2{dma_dbg_mem_wrdata[31:0]}} : axi_mstr_wdata[63:0]));
      rvdffsc #(1) fifo_valid_dff (.din(1'b1), .dout(fifo_valid[i]), .en(fifo_cmd_en[i]), .clear(fifo_reset[i]), .clk(dma_free_clk), .*);
      rvdffsc #(2) fifo_error_dff (.din(fifo_error_in[i]), .dout(fifo_error[i]), .en(fifo_error_en[i]), .clear(fifo_reset[i]), .clk(dma_free_clk), .*);
      rvdffsc #(1) fifo_error_bus_dff (.din(1'b1), .dout(fifo_error_bus[i]), .en(fifo_error_bus_en[i]), .clear(fifo_reset[i]), .clk(dma_free_clk), .*);
      rvdffs  #(1) fifo_dccm_valid_dff (.din((dma_addr_in_dccm | dma_addr_in_pic)), .dout(fifo_dccm_valid[i]), .en(fifo_cmd_en[i]), .clk(dma_buffer_c1_clk), .*);
      rvdffs  #(1) fifo_iccm_valid_dff (.din(dma_addr_in_iccm), .dout(fifo_iccm_valid[i]), .en(fifo_cmd_en[i]), .clk(dma_buffer_c1_clk), .*);
      rvdffsc #(1) fifo_data_valid_dff (.din(1'b1), .dout(fifo_data_valid[i]), .en(fifo_data_en[i]), .clear(fifo_reset[i]), .clk(dma_free_clk), .*);
      rvdffsc #(1) fifo_data_bus_valid_dff (.din(1'b1), .dout(fifo_data_bus_valid[i]), .en(fifo_data_bus_en[i]), .clear(fifo_reset[i]), .clk(dma_free_clk), .*);
      rvdffsc #(1) fifo_rpend_dff (.din(1'b1), .dout(fifo_rpend[i]), .en(fifo_pend_en[i]), .clear(fifo_reset[i]), .clk(dma_free_clk), .*);
      rvdffsc #(1) fifo_done_dff (.din(1'b1), .dout(fifo_done[i]), .en(fifo_done_en[i]), .clear(fifo_reset[i]), .clk(dma_free_clk), .*);
      rvdffsc #(1) fifo_done_bus_dff (.din(1'b1), .dout(fifo_done_bus[i]), .en(fifo_done_bus_en[i]), .clear(fifo_reset[i]), .clk(dma_free_clk), .*);
      rvdffe  #(32) fifo_addr_dff (.din(fifo_addr_in[31:0]), .dout(fifo_addr[i]), .en(fifo_cmd_en[i]), .*);
      rvdffs  #(3) fifo_sz_dff (.din(fifo_sz_in[2:0]), .dout(fifo_sz[i]), .en(fifo_cmd_en[i]), .clk(dma_buffer_c1_clk), .*);
      rvdffs  #(1) fifo_write_dff (.din(fifo_write_in), .dout(fifo_write[i]), .en(fifo_cmd_en[i]), .clk(dma_buffer_c1_clk), .*);
      rvdffs  #(1) fifo_posted_write_dff (.din(fifo_posted_write_in), .dout(fifo_posted_write[i]), .en(fifo_cmd_en[i]), .clk(dma_buffer_c1_clk), .*);
      rvdffs  #(1) fifo_dbg_dff (.din(fifo_dbg_in), .dout(fifo_dbg[i]), .en(fifo_cmd_en[i]), .clk(dma_buffer_c1_clk), .*);
      rvdffe  #(64) fifo_data_dff (.din(fifo_data_in[i]), .dout(fifo_data[i]), .en(fifo_data_en[i]), .*);
      rvdffs  #(DMA_BUS_TAG) fifo_tag_dff(.din(axi_mstr_tag[DMA_BUS_TAG-1:0]), .dout(fifo_tag[i][DMA_BUS_TAG-1:0]), .en(fifo_cmd_en[i]), .clk(dma_buffer_c1_clk), .*);
   end
   assign NxtWrPtr[DEPTH_PTR-1:0] = WrPtr[DEPTH_PTR-1:0] + 1'b1;
   assign NxtRdPtr[DEPTH_PTR-1:0] = RdPtr[DEPTH_PTR-1:0] + 1'b1;
   assign NxtRspPtr[DEPTH_PTR-1:0] = RspPtr[DEPTH_PTR-1:0] + 1'b1;
   assign WrPtrEn = |fifo_cmd_en[DEPTH-1:0];
   assign RdPtrEn = (dma_dccm_req | dma_iccm_req) | ((|fifo_error[RdPtr]) & ~fifo_done[RdPtr]);
   assign RspPtrEn = (dma_dbg_cmd_done | (axi_slv_sent & dma_bus_clk_en));
   rvdffs #(DEPTH_PTR) WrPtr_dff(.din(NxtWrPtr[DEPTH_PTR-1:0]), .dout(WrPtr[DEPTH_PTR-1:0]), .en(WrPtrEn), .clk(dma_free_clk), .*);
   rvdffs #(DEPTH_PTR) RdPtr_dff(.din(NxtRdPtr[DEPTH_PTR-1:0]), .dout(RdPtr[DEPTH_PTR-1:0]), .en(RdPtrEn), .clk(dma_free_clk), .*);
   rvdffs #(DEPTH_PTR) RspPtr_dff(.din(NxtRspPtr[DEPTH_PTR-1:0]), .dout(RspPtr[DEPTH_PTR-1:0]), .en(RspPtrEn), .clk(dma_free_clk), .*);
   rvdff #(DEPTH_PTR) RdPtrQ1_dff(.din(RdPtr[DEPTH_PTR-1:0]),    .dout(RdPtr_Q1[DEPTH_PTR-1:0]), .clk(dma_free_clk), .*);
   rvdff #(DEPTH_PTR) RdPtrQ2_dff(.din(RdPtr_Q1[DEPTH_PTR-1:0]), .dout(RdPtr_Q2[DEPTH_PTR-1:0]), .clk(dma_free_clk), .*);
   rvdff #(DEPTH_PTR) RdPtrQ3_dff(.din(RdPtr_Q2[DEPTH_PTR-1:0]), .dout(RdPtr_Q3[DEPTH_PTR-1:0]), .clk(dma_free_clk), .*);
   assign fifo_full = fifo_full_spec_bus;
   always_comb begin
      num_fifo_vld[3:0] = 4'b0;
      for (int i=0; i<DEPTH; i++) begin
         num_fifo_vld[3:0] += {3'b0,(fifo_valid_en[i] | fifo_valid[i])};
      end
   end
   assign fifo_full_spec = ((num_fifo_vld[3:0] == DEPTH) & ~(|fifo_reset[DEPTH-1:0]));
   assign dma_fifo_ready = ~(fifo_full | dbg_dma_bubble_bus | dec_tlu_stall_dma_bus);
   assign dma_address_error = axi_mstr_valid & (~(dma_addr_in_dccm | dma_addr_in_iccm));     
   assign dma_alignment_error = axi_mstr_valid & ~dma_address_error &
                                (((axi_mstr_size[2:0] == 3'h1) & (axi_mstr_addr[0]      | (axi_mstr_write & (axi_mstr_wstrb[axi_mstr_addr[2:0]+:2] != 2'b11))))  |     
                                 ((axi_mstr_size[2:0] == 3'h2) & ((|axi_mstr_addr[1:0]) | (axi_mstr_write & (axi_mstr_wstrb[axi_mstr_addr[2:0]+:4] != 4'hf))))   |     
                                 ((axi_mstr_size[2:0] == 3'h3) & ((|axi_mstr_addr[2:0]) | (axi_mstr_write & (axi_mstr_wstrb[7:0] != 8'hff))))                    |     
                                 (dma_addr_in_iccm & ~((axi_mstr_size[1:0] == 2'b10) | (axi_mstr_size[1:0] == 2'b11)))                                           |     
                                 (dma_addr_in_dccm & axi_mstr_write & ~((axi_mstr_size[1:0] == 2'b10) | (axi_mstr_size[1:0] == 2'b11))));                              
   assign dma_dbg_ready    = fifo_empty & dbg_dma_bubble;
   assign dma_dbg_cmd_done = (fifo_valid[RspPtr] & fifo_dbg[RspPtr] & (fifo_write[RspPtr] | fifo_data_valid[RspPtr] | (|fifo_error[RspPtr])));
   assign dma_dbg_cmd_fail     = |fifo_error[RspPtr];
   assign dma_dbg_sz[1:0]          = fifo_sz[RspPtr][1:0];
   assign dma_dbg_addr[1:0]        = fifo_addr[RspPtr][1:0];
   assign dma_dbg_mem_rddata[31:0] = fifo_addr[RspPtr][2] ? fifo_data[RspPtr][63:32] : fifo_data[RspPtr][31:0];
   assign dma_dbg_rddata[31:0]     = ({32{(dma_dbg_sz[1:0] == 2'h0)}} & ((dma_dbg_mem_rddata[31:0] >> 8*dma_dbg_addr[1:0]) & 32'hff)) |
                                     ({32{(dma_dbg_sz[1:0] == 2'h1)}} & ((dma_dbg_mem_rddata[31:0] >> 16*dma_dbg_addr[1]) & 32'hffff)) |
                                     ({32{(dma_dbg_sz[1:0] == 2'h2)}} & dma_dbg_mem_rddata[31:0]);
   assign dma_dbg_cmd_error_in = dbg_cmd_valid & (dbg_cmd_type[1:0] == 2'b10) &
                                 ((~(dma_addr_in_dccm | dma_addr_in_iccm | dma_addr_in_pic)) |              
                                  ((dma_addr_in_iccm | dma_addr_in_pic) & (dbg_cmd_size[1:0] != 2'b10)));   
   assign dma_dbg_mem_wrdata[31:0] = ({32{dbg_cmd_size[1:0] == 2'h0}} & {4{dbg_cmd_wrdata[7:0]}}) |
                                     ({32{dbg_cmd_size[1:0] == 2'h1}} & {2{dbg_cmd_wrdata[15:0]}}) |
                                     ({32{dbg_cmd_size[1:0] == 2'h2}} & dbg_cmd_wrdata[31:0]);
   assign dma_dccm_stall_any = dma_mem_req & fifo_dccm_valid[RdPtr] & (dma_nack_count >= dma_nack_count_csr);
   assign dma_iccm_stall_any = dma_mem_req & fifo_iccm_valid[RdPtr] & (dma_nack_count >= dma_nack_count_csr);
   assign fifo_empty     = ~(|(fifo_valid_en[DEPTH-1:0] | fifo_valid[DEPTH-1:0]) | axi_mstr_valid | axi_slv_sent_q);   
   assign dma_nack_count_csr[2:0] = dec_tlu_dma_qos_prty[2:0];
   assign dma_nack_count_d[2:0] = (dma_nack_count[2:0] >= dma_nack_count_csr[2:0]) ? ({3{~(dma_dccm_req | dma_iccm_req)}} & dma_nack_count[2:0]) :
                                                                                    (dma_mem_req & ~(dma_dccm_req | dma_iccm_req)) ? (dma_nack_count[2:0] + 1'b1) : 3'b0;
   rvdffs #(3) nack_count_dff(.din(dma_nack_count_d[2:0]), .dout(dma_nack_count[2:0]), .en(dma_mem_req), .clk(dma_free_clk), .*);
   assign dma_mem_req = fifo_valid[RdPtr] & ~fifo_rpend[RdPtr] & ~fifo_done[RdPtr] & ~(|fifo_error[RdPtr]) & (~fifo_write[RdPtr] | fifo_data_valid[RdPtr]);
   assign dma_dccm_req = dma_mem_req & fifo_dccm_valid[RdPtr] & dccm_ready;
   assign dma_iccm_req = dma_mem_req & fifo_iccm_valid[RdPtr] & iccm_ready;
   assign dma_mem_addr[31:0] = fifo_addr[RdPtr];
   assign dma_mem_sz[2:0]    = fifo_sz[RdPtr];
   assign dma_mem_write      = fifo_write[RdPtr];
   assign dma_mem_wdata[63:0] = fifo_data[RdPtr];
   rvrangecheck #(.CCM_SADR(32'hf0040000),
                  .CCM_SIZE(64)) addr_dccm_rangecheck (
      .addr(fifo_addr_in[31:0]),
      .in_range(dma_addr_in_dccm),
      .in_region(dma_addr_in_dccm_region_nc)
   );
   assign dma_addr_in_iccm = '0;
   assign dma_addr_in_iccm_region_nc = '0;
   rvrangecheck #(.CCM_SADR(32'hf00c0000),
                  .CCM_SIZE(32)) addr_pic_rangecheck (
      .addr(fifo_addr_in[31:0]),
      .in_range(dma_addr_in_pic),
      .in_region(dma_addr_in_pic_region_nc)
    );
   rvdff #(1)  ahbs_bus_clken_ff (.din(dma_bus_clk_en), .dout(dma_bus_clk_en_q), .clk(free_clk), .*);
   rvdff #(1) dma_dbg_cmd_doneff (.din(dma_dbg_cmd_done), .dout(dma_dbg_cmd_done_q), .clk(free_clk), .*);
   assign dma_buffer_c1_clken = (axi_mstr_valid & dma_bus_clk_en) | dbg_cmd_valid | dec_tlu_stall_dma | clk_override;
   assign dma_free_clken = (axi_mstr_valid | axi_mstr_valid_q | axi_slv_valid | axi_slv_sent_q | dbg_cmd_valid | dma_dbg_cmd_done | dma_dbg_cmd_done_q | (|fifo_valid[DEPTH-1:0]) | wrbuf_vld | rdbuf_vld | dec_tlu_stall_dma | clk_override);
   rvoclkhdr dma_buffer_c1cgc ( .en(dma_buffer_c1_clken), .l1clk(dma_buffer_c1_clk), .* );
   rvoclkhdr dma_free_cgc (.en(dma_free_clken), .l1clk(dma_free_clk), .*);
   rvdffsc_fpga  #(1)  wrbuf_vldff     (.din(1'b1), .clear(wrbuf_rst),      .dout(wrbuf_vld),      .en(wrbuf_en),      .clk(dma_bus_clk), .clken(dma_bus_clk_en), .rawclk(clk), .*);
   rvdffsc_fpga  #(1)  wrbuf_data_vldff(.din(1'b1), .clear(wrbuf_data_rst), .dout(wrbuf_data_vld), .en(wrbuf_data_en), .clk(dma_bus_clk), .clken(dma_bus_clk_en), .rawclk(clk), .*);
   rvdffsc_fpga  #(1)  rdbuf_vldff     (.din(1'b1), .clear(rdbuf_rst),      .dout(rdbuf_vld),      .en(rdbuf_en),      .clk(dma_bus_clk), .clken(dma_bus_clk_en), .rawclk(clk), .*);
   rvdff_fpga  #(1)  fifo_full_bus_ff     (.din(fifo_full_spec),                .dout(fifo_full_spec_bus),                        .clk(dma_bus_clk), .clken(dma_bus_clk_en), .rawclk(clk), .*);
   rvdff_fpga  #(1)  dbg_dma_bubble_ff    (.din(dbg_dma_bubble),                .dout(dbg_dma_bubble_bus),                        .clk(dma_bus_clk), .clken(dma_bus_clk_en), .rawclk(clk), .*);
   rvdff_fpga  #(1)  dec_tlu_stall_dma_ff (.din(dec_tlu_stall_dma),             .dout(dec_tlu_stall_dma_bus),                     .clk(dma_bus_clk), .clken(dma_bus_clk_en), .rawclk(clk), .*);
   rvdffs_fpga #(1) wrbuf_postedff        (.din(1'b0),                          .dout(wrbuf_posted),               .en(wrbuf_en), .clk(dma_bus_clk), .clken(dma_bus_clk_en), .rawclk(clk), .*);
   rvdffs_fpga #(DMA_BUS_TAG) wrbuf_tagff (.din(dma_axi_awid[DMA_BUS_TAG-1:0]), .dout(wrbuf_tag[DMA_BUS_TAG-1:0]), .en(wrbuf_en), .clk(dma_bus_clk), .clken(dma_bus_clk_en), .rawclk(clk), .*);
   rvdffs_fpga #(3) wrbuf_sizeff          (.din(dma_axi_awsize[2:0]),           .dout(wrbuf_size[2:0]),            .en(wrbuf_en), .clk(dma_bus_clk), .clken(dma_bus_clk_en), .rawclk(clk), .*);
   rvdffs_fpga #(8) wrbuf_byteenff        (.din(dma_axi_wstrb[7:0]),            .dout(wrbuf_byteen[7:0]),     .en(wrbuf_data_en), .clk(dma_bus_clk), .clken(dma_bus_clk_en), .rawclk(clk), .*);
   rvdffs_fpga #(DMA_BUS_TAG) rdbuf_tagff (.din(dma_axi_arid[DMA_BUS_TAG-1:0]), .dout(rdbuf_tag[DMA_BUS_TAG-1:0]), .en(rdbuf_en), .clk(dma_bus_clk), .clken(dma_bus_clk_en), .rawclk(clk), .*);
   rvdffs_fpga #(3) rdbuf_sizeff          (.din(dma_axi_arsize[2:0]),           .dout(rdbuf_size[2:0]),            .en(rdbuf_en), .clk(dma_bus_clk), .clken(dma_bus_clk_en), .rawclk(clk), .*);
   rvdffs_fpga #(1) mstr_prtyff           (.din(axi_mstr_prty_in),              .dout(axi_mstr_priority),  .en(axi_mstr_prty_en), .clk(dma_bus_clk), .clken(dma_bus_clk_en), .rawclk(clk), .*);
   rvdff_fpga #(1) axi_mstr_validff       ( .din(axi_mstr_valid), .dout(axi_mstr_valid_q), .clk(dma_bus_clk), .clken(dma_bus_clk_en), .rawclk(clk), .*);
   rvdff_fpga #(1) axi_slv_sentff         ( .din(axi_slv_sent),   .dout(axi_slv_sent_q),   .clk(dma_bus_clk), .clken(dma_bus_clk_en), .rawclk(clk), .*);
   rvdffe   #(32) wrbuf_addrff(.din(dma_axi_awaddr[31:0]), .dout(wrbuf_addr[31:0]), .en(wrbuf_en & dma_bus_clk_en),      .*);
   rvdffe   #(64) wrbuf_dataff(.din(dma_axi_wdata[63:0]),  .dout(wrbuf_data[63:0]), .en(wrbuf_data_en & dma_bus_clk_en), .*);
   rvdffe   #(32) rdbuf_addrff(.din(dma_axi_araddr[31:0]), .dout(rdbuf_addr[31:0]), .en(rdbuf_en & dma_bus_clk_en),      .*);
   assign wrbuf_en       = dma_axi_awvalid & dma_axi_awready;
   assign wrbuf_data_en  = dma_axi_wvalid & dma_axi_wready;
   assign wrbuf_cmd_sent = axi_mstr_valid & axi_mstr_write;
   assign wrbuf_rst      = wrbuf_cmd_sent & ~wrbuf_en;
   assign wrbuf_data_rst = wrbuf_cmd_sent & ~wrbuf_data_en;
   assign rdbuf_en    = dma_axi_arvalid & dma_axi_arready;
   assign rdbuf_cmd_sent = axi_mstr_valid & ~axi_mstr_write & dma_fifo_ready;
   assign rdbuf_rst   = rdbuf_cmd_sent & ~rdbuf_en;
   assign dma_axi_awready = ~(wrbuf_vld & ~wrbuf_cmd_sent);
   assign dma_axi_wready  = ~(wrbuf_data_vld & ~wrbuf_cmd_sent);
   assign dma_axi_arready = ~(rdbuf_vld & ~rdbuf_cmd_sent);
   assign axi_mstr_valid                  = ((wrbuf_vld & wrbuf_data_vld) | rdbuf_vld) & dma_fifo_ready;
   assign axi_mstr_tag[DMA_BUS_TAG-1:0]   = axi_mstr_sel ? wrbuf_tag[DMA_BUS_TAG-1:0] : rdbuf_tag[DMA_BUS_TAG-1:0];
   assign axi_mstr_write                  = axi_mstr_sel;
   assign axi_mstr_posted_write           = axi_mstr_sel & wrbuf_posted;
   assign axi_mstr_addr[31:0]             = axi_mstr_sel ? wrbuf_addr[31:0] : rdbuf_addr[31:0];
   assign axi_mstr_size[2:0]              = axi_mstr_sel ? wrbuf_size[2:0] : rdbuf_size[2:0];
   assign axi_mstr_wdata[63:0]            = wrbuf_data[63:0];
   assign axi_mstr_wstrb[7:0]             = wrbuf_byteen[7:0];
   assign axi_mstr_sel     = (wrbuf_vld & wrbuf_data_vld & rdbuf_vld) ? axi_mstr_priority : (wrbuf_vld & wrbuf_data_vld);
   assign axi_mstr_prty_in = ~axi_mstr_priority;
   assign axi_mstr_prty_en = axi_mstr_valid;
   assign axi_slv_valid                  = fifo_valid[RspPtr] & ~fifo_dbg[RspPtr] & fifo_done_bus[RspPtr];
   assign axi_slv_tag[DMA_BUS_TAG-1:0]   = fifo_tag[RspPtr];
   assign axi_slv_rdata[63:0]            = fifo_data[RspPtr];
   assign axi_slv_write                  = fifo_write[RspPtr];
   assign axi_slv_posted_write           = axi_slv_write & fifo_posted_write[RspPtr];
   assign axi_slv_error[1:0]             = fifo_error[RspPtr][0] ? 2'b10 : (fifo_error[RspPtr][1] ? 2'b11 : 2'b0);
   assign dma_axi_bvalid                  = axi_slv_valid & axi_slv_write;
   assign dma_axi_bresp[1:0]              = axi_slv_error[1:0];
   assign dma_axi_bid[DMA_BUS_TAG-1:0]    = axi_slv_tag[DMA_BUS_TAG-1:0];
   assign dma_axi_rvalid                  = axi_slv_valid & ~axi_slv_write;
   assign dma_axi_rresp[1:0]              = axi_slv_error;
   assign dma_axi_rid[DMA_BUS_TAG-1:0]    = axi_slv_tag[DMA_BUS_TAG-1:0];
   assign dma_axi_rdata[63:0]             = axi_slv_rdata[63:0];
   assign dma_axi_rlast                   = 1'b1;
   assign axi_slv_sent       = (dma_axi_bvalid & dma_axi_bready) | (dma_axi_rvalid & dma_axi_rready);
   assign dma_slv_algn_err   = fifo_error[RspPtr][1];
endmodule  
module ifu_aln_ctl
   import swerv_types::*;
(
   input logic        active_clk,
   input logic        iccm_rd_ecc_single_err,          
   input logic [7:0]  iccm_rd_ecc_double_err,          
   input logic        ic_rd_parity_final_err,          
   input logic        ifu_icache_fetch_f2,
   input logic [7:0]  ic_access_fault_f2,              
   input logic [4:0]  ifu_bp_fghr_f2,    
   input logic [31:1] ifu_bp_btb_target_f2,            
   input logic [11:0] ifu_bp_poffset_f2,               
   input logic [7:0]  ifu_bp_hist0_f2,     
   input logic [7:0]  ifu_bp_hist1_f2,     
   input logic [7:0]  ifu_bp_pc4_f2,       
   input logic [7:0]  ifu_bp_way_f2,       
   input logic [7:0]  ifu_bp_valid_f2,     
   input logic [7:0]  ifu_bp_ret_f2,       
   input logic exu_flush_final,            
   input logic dec_ib3_valid_d,            
   input logic dec_ib2_valid_d,
   input logic dec_ib0_valid_eff_d,        
   input logic dec_ib1_valid_eff_d,
   input logic [127:0] ifu_fetch_data,       
   input icache_err_pkt_t ic_error_f2,       
   input logic [7:0]   ifu_fetch_val,        
   input logic [31:1]  ifu_fetch_pc,         
   input logic   rst_l,
   input logic   clk,
   input logic   dec_tlu_core_ecc_disable,   
   output logic ifu_i0_valid,             
   output logic ifu_i1_valid,             
   output logic ifu_i0_icaf,              
   output logic ifu_i1_icaf,              
   output logic ifu_i0_icaf_second,       
   output logic ifu_i1_icaf_second,       
   output logic ifu_i0_perr,              
   output logic ifu_i1_perr,              
   output logic ifu_i0_sbecc,             
   output logic ifu_i1_sbecc,             
   output logic ifu_i0_dbecc,             
   output logic ifu_i1_dbecc,             
   output logic [31:0] ifu_i0_instr,      
   output logic [31:0] ifu_i1_instr,      
   output logic [31:1] ifu_i0_pc,         
   output logic [31:1] ifu_i1_pc,         
   output logic ifu_i0_pc4,
   output logic ifu_i1_pc4,
   output logic ifu_fb_consume1,          
   output logic ifu_fb_consume2,          
   output logic [15:0] ifu_illegal_inst,  
   output br_pkt_t i0_brp,                
   output br_pkt_t i1_brp,                
   output logic [1:0] ifu_pmu_instr_aligned,          
   output logic       ifu_pmu_align_stall,            
   output logic [16:2] ifu_icache_error_index,    
   output logic        ifu_icache_error_val,      
   output logic        ifu_icache_sb_error_val,
   output logic [15:0] ifu_i0_cinst,                  
   output logic [15:0] ifu_i1_cinst,                  
   input  logic    scan_mode
   );
localparam TOTAL_INT        = 9;
localparam DCCM_BITS        = 16;
localparam DCCM_BANK_BITS   = 3;
localparam DCCM_NUM_BANKS   = 8;
localparam DCCM_DATA_WIDTH  = 32;
localparam DCCM_FDATA_WIDTH = 39;
localparam DCCM_BYTE_WIDTH  = 4;
localparam DCCM_ECC_WIDTH   = 7;
localparam LSU_RDBUF_DEPTH  = 8;
localparam DMA_BUF_DEPTH    = 4;
localparam LSU_STBUF_DEPTH  = 8;
localparam LSU_SB_BITS      = 16;
localparam DEC_INSTBUF_DEPTH = 4;
localparam ICCM_SIZE         = 512;
localparam ICCM_BITS         = 19;
localparam ICCM_NUM_BANKS    = 8;
localparam ICCM_BANK_BITS    = 3;
localparam ICCM_INDEX_BITS   = 14;
localparam ICCM_BANK_HI      = 4 + (3/4);
localparam ICACHE_TAG_HIGH  = 12;
localparam ICACHE_TAG_LOW   = 6;
localparam ICACHE_IC_DEPTH  = 8;
localparam ICACHE_TAG_DEPTH = 64;
localparam LSU_BUS_TAG     = 4;
localparam DMA_BUS_TAG     = 1;
localparam SB_BUS_TAG      = 1;
localparam IFU_BUS_TAG     = 3;
   logic         ifvalid;
   logic         shift_f1_f0, shift_f2_f0, shift_f2_f1;
   logic         fetch_to_f0, fetch_to_f1, fetch_to_f2;
   logic [7:0]   f2val_in, f2val;
   logic [7:0]   f1val_in, f1val;
   logic [7:0]   f0val_in, f0val;
   logic [7:0]   sf1val, sf0val;
   logic [31:1]  f2pc_in, f2pc;
   logic [31:1]  f1pc_in, f1pc;
   logic [31:1]  f0pc_in, f0pc;
   logic [31:1]  sf1pc, sf0pc;
   logic [63:0]  aligndata;
   logic         first4B, first2B;
   logic         second4B, second2B;
   logic         third4B, third2B;
   logic [31:0]  uncompress0, uncompress1, uncompress2;
   logic         ibuffer_room1_more;
   logic         ibuffer_room2_more;
   logic         i0_shift, i1_shift;
   logic         shift_2B, shift_4B, shift_6B, shift_8B;
   logic         f1_shift_2B, f1_shift_4B, f1_shift_6B;
   logic         f2_valid, sf1_valid, sf0_valid;
   logic [31:0]  ifirst, isecond, ithird;
   logic [31:1]  f0pc_plus1, f0pc_plus2, f0pc_plus3, f0pc_plus4;
   logic [31:1]  f1pc_plus1, f1pc_plus2, f1pc_plus3;
   logic [3:0]   alignval;
   logic [31:1]  firstpc, secondpc, thirdpc, fourthpc;
   logic [11:0]  f1poffset;
   logic [11:0]  f0poffset;
   logic [4:0]  f1fghr;
   logic [4:0]  f0fghr;
   logic [7:0]               f1hist1;
   logic [7:0]               f0hist1;
   logic [7:0]               f1hist0;
   logic [7:0]               f0hist0;
   logic [7:0]             f1pc4;
   logic [7:0]             f0pc4;
   logic [7:0]             f1ret;
   logic [7:0]             f0ret;
   logic [7:0]             f1way;
   logic [7:0]             f0way;
   logic [7:0]               f1brend;
   logic [7:0]               f0brend;
   logic [3:0]   alignbrend;
   logic [3:0]   alignpc4;
   logic [3:0]   alignparity;
   logic [3:0]   alignret;
   logic [3:0]   alignway;
   logic [3:0]   alignhist1;
   logic [3:0]   alignhist0;
   logic [3:1]   alignfromf1;
   logic         i0_ends_f1, i1_ends_f1;
   logic         i0_br_start_error, i1_br_start_error;
   logic [31:1]  f1prett;
   logic [31:1]  f0prett;
   logic [7:0]   f1dbecc;
   logic [7:0]   f0dbecc;
   logic         f1sbecc;
   logic         f0sbecc;
   logic         f1perr;
   logic         f0perr;
   logic         f1icfetch;
   logic         f0icfetch;
   logic [7:0]   f1icaf;
   logic [7:0]   f0icaf;
   logic [3:0]   alignicfetch;
   logic [3:0]   aligntagperr;
   logic [3:0]   aligndataperr;
   logic [3:0]   alignsbecc;
   logic [3:0]   aligndbecc;
   logic [3:0]   alignicaf;
   logic         i0_brp_pc4, i1_brp_pc4;
   logic [5:4] firstpc_hash, secondpc_hash, thirdpc_hash, fourthpc_hash;
   logic         i0_illegal, i1_illegal;
   logic         shift_illegal;
   logic         first_legal, second_legal, third_legal;
   logic [15:0]  illegal_inst;
   logic         illegal_inst_en;
   logic         illegal_lockout_in, illegal_lockout;
   logic [3:0]   alignfinalperr;
   logic f2_wr_en;
   assign f2_wr_en = fetch_to_f2;
   logic f0_shift_wr_en;
   assign f0_shift_wr_en = (fetch_to_f0 | shift_f2_f0 | shift_f1_f0 | shift_2B | shift_4B | shift_6B | shift_8B);
   logic f1_shift_wr_en;
   assign f1_shift_wr_en = (fetch_to_f1 | shift_f2_f1 | f1_shift_2B | f1_shift_4B | f1_shift_6B);
   logic [1:0] wrptr, wrptr_in;
   logic [1:0] rdptr, rdptr_in;
   logic [2:0] qwen;
   logic [127:0] q2,q1,q0;
   logic [2:0]   first_offset, second_offset;
   logic [2:0]   q2off_eff, q2off_in, q2off;
   logic [2:0]   q1off_eff, q1off_in, q1off;
   logic [2:0]   q0off_eff, q0off_in, q0off;
   logic         f0_shift_2B, f0_shift_4B, f0_shift_6B, f0_shift_8B;
   logic [127:0] q0eff;
   logic [127:0] q0final;
   logic [2:0]   q0ptr;
   logic [7:0]   q0sel;
   logic [127:0] q1eff;
   logic [127:0] q1final;
   logic [2:0]   q1ptr;
   logic [7:0]   q1sel;
   logic [2:0]   qren;
   logic         consume_fb1, consume_fb0;
   logic [3:0]   icaf_eff;
   logic [7:0]   q0parity, q1parity, q2parity;
   logic [7:0]   q0parityeff, q1parityeff;
   logic [7:0]   q0parityfinal, q1parityfinal;
   assign wrptr_in[1:0] =  (({2{wrptr[1:0]==2'b00 & ifvalid}} & 2'b01) |
                            ({2{wrptr[1:0]==2'b01 & ifvalid}} & 2'b10) |
                            ({2{wrptr[1:0]==2'b10 & ifvalid}} & 2'b00) |
                            ({2{~ifvalid}} & wrptr[1:0])) & ~{2{exu_flush_final}};
   rvdff #(2) wrpff (.*, .clk(active_clk), .din(wrptr_in[1:0]), .dout(wrptr[1:0]));
   assign rdptr_in[1:0] =  (({2{rdptr[1:0]==2'b00 & ifu_fb_consume1}} & 2'b01) |
                            ({2{rdptr[1:0]==2'b01 & ifu_fb_consume1}} & 2'b10) |
                            ({2{rdptr[1:0]==2'b10 & ifu_fb_consume1}} & 2'b00) |
                            ({2{rdptr[1:0]==2'b00 & ifu_fb_consume2}} & 2'b10) |
                            ({2{rdptr[1:0]==2'b01 & ifu_fb_consume2}} & 2'b00) |
                            ({2{rdptr[1:0]==2'b10 & ifu_fb_consume2}} & 2'b01) |
                            ({2{~ifu_fb_consume1&~ifu_fb_consume2}} & rdptr[1:0])) & ~{2{exu_flush_final}};
   rvdff #(2) rdpff (.*, .clk(active_clk), .din(rdptr_in[1:0]), .dout(rdptr[1:0]));
   assign qren[2:0] = { rdptr[1:0]==2'b10,
                        rdptr[1:0]==2'b01,
                        rdptr[1:0]==2'b00
                        };
   assign qwen[2:0] = { wrptr[1:0]==2'b10 & ifvalid,
                        wrptr[1:0]==2'b01 & ifvalid,
                        wrptr[1:0]==2'b00 & ifvalid
                        };
   assign first_offset[2:0]  = {f0_shift_8B,  f0_shift_6B|f0_shift_4B,  f0_shift_6B|f0_shift_2B };
   assign second_offset[2:0] = {1'b0,         f1_shift_6B|f1_shift_4B,  f1_shift_6B|f1_shift_2B };
   assign q2off_eff[2:0] = (rdptr[1:0]==2'd2) ? (q2off[2:0] + first_offset[2:0])  :
                           (rdptr[1:0]==2'd1) ? (q2off[2:0] + second_offset[2:0]) :
                                                 q2off[2:0];
   assign q2off_in[2:0] = (qwen[2]) ? ifu_fetch_pc[3:1] : q2off_eff[2:0];
   rvdff #(3) q2offsetff (.*, .clk(active_clk), .din(q2off_in[2:0]), .dout(q2off[2:0]));
   assign q1off_eff[2:0] = (rdptr[1:0]==2'd1) ? (q1off[2:0] + first_offset[2:0])  :
                           (rdptr[1:0]==2'd0) ? (q1off[2:0] + second_offset[2:0]) :
                                                 q1off[2:0];
   assign q1off_in[2:0] = (qwen[1]) ? ifu_fetch_pc[3:1] : q1off_eff[2:0];
   rvdff #(3) q1offsetff (.*, .clk(active_clk), .din(q1off_in[2:0]), .dout(q1off[2:0]));
   assign q0off_eff[2:0] = (rdptr[1:0]==2'd0) ? (q0off[2:0] + first_offset[2:0])  :
                           (rdptr[1:0]==2'd2) ? (q0off[2:0] + second_offset[2:0]) :
                                                 q0off[2:0];
   assign q0off_in[2:0] = (qwen[0]) ? ifu_fetch_pc[3:1] : q0off_eff[2:0];
   rvdff #(3) q0offsetff (.*, .clk(active_clk), .din(q0off_in[2:0]), .dout(q0off[2:0]));
   assign q0ptr[2:0] = (({3{rdptr[1:0]==2'b00}} & q0off[2:0]) |
                        ({3{rdptr[1:0]==2'b01}} & q1off[2:0]) |
                        ({3{rdptr[1:0]==2'b10}} & q2off[2:0]));
   assign q1ptr[2:0] = (({3{rdptr[1:0]==2'b00}} & q1off[2:0]) |
                        ({3{rdptr[1:0]==2'b01}} & q2off[2:0]) |
                        ({3{rdptr[1:0]==2'b10}} & q0off[2:0]));
   assign q0sel[7:0] = { q0ptr[2:0]==3'b111,
                         q0ptr[2:0]==3'b110,
                         q0ptr[2:0]==3'b101,
                         q0ptr[2:0]==3'b100,
                         q0ptr[2:0]==3'b011,
                         q0ptr[2:0]==3'b010,
                         q0ptr[2:0]==3'b001,
                         q0ptr[2:0]==3'b000
                         };
   assign q1sel[7:0] = { q1ptr[2:0]==3'b111,
                         q1ptr[2:0]==3'b110,
                         q1ptr[2:0]==3'b101,
                         q1ptr[2:0]==3'b100,
                         q1ptr[2:0]==3'b011,
                         q1ptr[2:0]==3'b010,
                         q1ptr[2:0]==3'b001,
                         q1ptr[2:0]==3'b000
                         };
   localparam MHI   = 45+5;
   localparam MSIZE = 46+5;
   logic [MHI:0] misc_data_in, misc2, misc1, misc0;
   logic [MHI:0] misc1eff, misc0eff;
   assign misc_data_in[MHI:0] = {
                                  iccm_rd_ecc_single_err,
                                  ifu_icache_fetch_f2,
                                  ic_rd_parity_final_err,
                                  ifu_bp_btb_target_f2[31:1],
                                  ifu_bp_poffset_f2[11:0],
                                  ifu_bp_fghr_f2[4:0]
                                  };
   rvdffe #(MSIZE) misc2ff (.*, .en(qwen[2]), .din(misc_data_in[MHI:0]), .dout(misc2[MHI:0]));
   rvdffe #(MSIZE) misc1ff (.*, .en(qwen[1]), .din(misc_data_in[MHI:0]), .dout(misc1[MHI:0]));
   rvdffe #(MSIZE) misc0ff (.*, .en(qwen[0]), .din(misc_data_in[MHI:0]), .dout(misc0[MHI:0]));
   assign {misc1eff[MHI:0],misc0eff[MHI:0]} = (({MSIZE*2{qren[0]}} & {misc1[MHI:0],misc0[MHI:0]}) |
                                               ({MSIZE*2{qren[1]}} & {misc2[MHI:0],misc1[MHI:0]}) |
                                               ({MSIZE*2{qren[2]}} & {misc0[MHI:0],misc2[MHI:0]}));
   assign {
            f1sbecc,
            f1icfetch,
            f1perr,
            f1prett[31:1],
            f1poffset[11:0],
            f1fghr[4:0]
            } = misc1eff[MHI:0];
   assign {
            f0sbecc,
            f0icfetch,
            f0perr,
            f0prett[31:1],
            f0poffset[11:0],
            f0fghr[4:0]
            } = misc0eff[MHI:0];
   localparam BRDATA_SIZE=64;
   localparam BRDATA_WIDTH = 8;
   logic [BRDATA_SIZE-1:0] brdata_in, brdata2, brdata1, brdata0;
   logic [BRDATA_SIZE-1:0] brdata1eff, brdata0eff;
   logic [BRDATA_SIZE-1:0] brdata1final, brdata0final;
   assign brdata_in[BRDATA_SIZE-1:0] = {
                              iccm_rd_ecc_double_err[7],ic_access_fault_f2[7],ifu_bp_hist1_f2[7],ifu_bp_hist0_f2[7],ifu_bp_pc4_f2[7],ifu_bp_way_f2[7],ifu_bp_valid_f2[7],ifu_bp_ret_f2[7],
                              iccm_rd_ecc_double_err[6],ic_access_fault_f2[6],ifu_bp_hist1_f2[6],ifu_bp_hist0_f2[6],ifu_bp_pc4_f2[6],ifu_bp_way_f2[6],ifu_bp_valid_f2[6],ifu_bp_ret_f2[6],
                              iccm_rd_ecc_double_err[5],ic_access_fault_f2[5],ifu_bp_hist1_f2[5],ifu_bp_hist0_f2[5],ifu_bp_pc4_f2[5],ifu_bp_way_f2[5],ifu_bp_valid_f2[5],ifu_bp_ret_f2[5],
                              iccm_rd_ecc_double_err[4],ic_access_fault_f2[4],ifu_bp_hist1_f2[4],ifu_bp_hist0_f2[4],ifu_bp_pc4_f2[4],ifu_bp_way_f2[4],ifu_bp_valid_f2[4],ifu_bp_ret_f2[4],
                              iccm_rd_ecc_double_err[3],ic_access_fault_f2[3],ifu_bp_hist1_f2[3],ifu_bp_hist0_f2[3],ifu_bp_pc4_f2[3],ifu_bp_way_f2[3],ifu_bp_valid_f2[3],ifu_bp_ret_f2[3],
                              iccm_rd_ecc_double_err[2],ic_access_fault_f2[2],ifu_bp_hist1_f2[2],ifu_bp_hist0_f2[2],ifu_bp_pc4_f2[2],ifu_bp_way_f2[2],ifu_bp_valid_f2[2],ifu_bp_ret_f2[2],
                              iccm_rd_ecc_double_err[1],ic_access_fault_f2[1],ifu_bp_hist1_f2[1],ifu_bp_hist0_f2[1],ifu_bp_pc4_f2[1],ifu_bp_way_f2[1],ifu_bp_valid_f2[1],ifu_bp_ret_f2[1],
                              iccm_rd_ecc_double_err[0],ic_access_fault_f2[0],ifu_bp_hist1_f2[0],ifu_bp_hist0_f2[0],ifu_bp_pc4_f2[0],ifu_bp_way_f2[0],ifu_bp_valid_f2[0],ifu_bp_ret_f2[0]
                              };
   rvdffe #(BRDATA_SIZE) brdata2ff (.*, .en(qwen[2]), .din(brdata_in[BRDATA_SIZE-1:0]), .dout(brdata2[BRDATA_SIZE-1:0]));
   rvdffe #(BRDATA_SIZE) brdata1ff (.*, .en(qwen[1]), .din(brdata_in[BRDATA_SIZE-1:0]), .dout(brdata1[BRDATA_SIZE-1:0]));
   rvdffe #(BRDATA_SIZE) brdata0ff (.*, .en(qwen[0]), .din(brdata_in[BRDATA_SIZE-1:0]), .dout(brdata0[BRDATA_SIZE-1:0]));
   assign {brdata1eff[BRDATA_SIZE-1:0],brdata0eff[BRDATA_SIZE-1:0]} = (({BRDATA_SIZE*2{qren[0]}} & {brdata1[BRDATA_SIZE-1:0],brdata0[BRDATA_SIZE-1:0]}) |
                                                                       ({BRDATA_SIZE*2{qren[1]}} & {brdata2[BRDATA_SIZE-1:0],brdata1[BRDATA_SIZE-1:0]}) |
                                                                       ({BRDATA_SIZE*2{qren[2]}} & {brdata0[BRDATA_SIZE-1:0],brdata2[BRDATA_SIZE-1:0]}));
   assign brdata0final[BRDATA_SIZE-1:0] = (({BRDATA_SIZE{q0sel[0]}} & {            brdata0eff[BRDATA_SIZE-1:0]}) |
                                ({BRDATA_SIZE{q0sel[1]}} & {{1*BRDATA_WIDTH{1'b0}},brdata0eff[BRDATA_SIZE-1:1*BRDATA_WIDTH]}) |
                                ({BRDATA_SIZE{q0sel[2]}} & {{2*BRDATA_WIDTH{1'b0}},brdata0eff[BRDATA_SIZE-1:2*BRDATA_WIDTH]}) |
                                ({BRDATA_SIZE{q0sel[3]}} & {{3*BRDATA_WIDTH{1'b0}},brdata0eff[BRDATA_SIZE-1:3*BRDATA_WIDTH]}) |
                                ({BRDATA_SIZE{q0sel[4]}} & {{4*BRDATA_WIDTH{1'b0}},brdata0eff[BRDATA_SIZE-1:4*BRDATA_WIDTH]}) |
                                ({BRDATA_SIZE{q0sel[5]}} & {{5*BRDATA_WIDTH{1'b0}},brdata0eff[BRDATA_SIZE-1:5*BRDATA_WIDTH]}) |
                                ({BRDATA_SIZE{q0sel[6]}} & {{6*BRDATA_WIDTH{1'b0}},brdata0eff[BRDATA_SIZE-1:6*BRDATA_WIDTH]}) |
                                ({BRDATA_SIZE{q0sel[7]}} & {{7*BRDATA_WIDTH{1'b0}},brdata0eff[BRDATA_SIZE-1:7*BRDATA_WIDTH]}));
   assign brdata1final[BRDATA_SIZE-1:0] = (({BRDATA_SIZE{q1sel[0]}} & {            brdata1eff[BRDATA_SIZE-1:0]}) |
                                ({BRDATA_SIZE{q1sel[1]}} & {{1*BRDATA_WIDTH{1'b0}},brdata1eff[BRDATA_SIZE-1:1*BRDATA_WIDTH]}) |
                                ({BRDATA_SIZE{q1sel[2]}} & {{2*BRDATA_WIDTH{1'b0}},brdata1eff[BRDATA_SIZE-1:2*BRDATA_WIDTH]}) |
                                ({BRDATA_SIZE{q1sel[3]}} & {{3*BRDATA_WIDTH{1'b0}},brdata1eff[BRDATA_SIZE-1:3*BRDATA_WIDTH]}) |
                                ({BRDATA_SIZE{q1sel[4]}} & {{4*BRDATA_WIDTH{1'b0}},brdata1eff[BRDATA_SIZE-1:4*BRDATA_WIDTH]}) |
                                ({BRDATA_SIZE{q1sel[5]}} & {{5*BRDATA_WIDTH{1'b0}},brdata1eff[BRDATA_SIZE-1:5*BRDATA_WIDTH]}) |
                                ({BRDATA_SIZE{q1sel[6]}} & {{6*BRDATA_WIDTH{1'b0}},brdata1eff[BRDATA_SIZE-1:6*BRDATA_WIDTH]}) |
                                ({BRDATA_SIZE{q1sel[7]}} & {{7*BRDATA_WIDTH{1'b0}},brdata1eff[BRDATA_SIZE-1:7*BRDATA_WIDTH]}));
   assign {
            f0dbecc[7],f0icaf[7],f0hist1[7],f0hist0[7],f0pc4[7],f0way[7],f0brend[7],f0ret[7],
            f0dbecc[6],f0icaf[6],f0hist1[6],f0hist0[6],f0pc4[6],f0way[6],f0brend[6],f0ret[6],
            f0dbecc[5],f0icaf[5],f0hist1[5],f0hist0[5],f0pc4[5],f0way[5],f0brend[5],f0ret[5],
            f0dbecc[4],f0icaf[4],f0hist1[4],f0hist0[4],f0pc4[4],f0way[4],f0brend[4],f0ret[4],
            f0dbecc[3],f0icaf[3],f0hist1[3],f0hist0[3],f0pc4[3],f0way[3],f0brend[3],f0ret[3],
            f0dbecc[2],f0icaf[2],f0hist1[2],f0hist0[2],f0pc4[2],f0way[2],f0brend[2],f0ret[2],
            f0dbecc[1],f0icaf[1],f0hist1[1],f0hist0[1],f0pc4[1],f0way[1],f0brend[1],f0ret[1],
            f0dbecc[0],f0icaf[0],f0hist1[0],f0hist0[0],f0pc4[0],f0way[0],f0brend[0],f0ret[0]
            } = brdata0final[BRDATA_SIZE-1:0];
   assign {
            f1dbecc[7],f1icaf[7],f1hist1[7],f1hist0[7],f1pc4[7],f1way[7],f1brend[7],f1ret[7],
            f1dbecc[6],f1icaf[6],f1hist1[6],f1hist0[6],f1pc4[6],f1way[6],f1brend[6],f1ret[6],
            f1dbecc[5],f1icaf[5],f1hist1[5],f1hist0[5],f1pc4[5],f1way[5],f1brend[5],f1ret[5],
            f1dbecc[4],f1icaf[4],f1hist1[4],f1hist0[4],f1pc4[4],f1way[4],f1brend[4],f1ret[4],
            f1dbecc[3],f1icaf[3],f1hist1[3],f1hist0[3],f1pc4[3],f1way[3],f1brend[3],f1ret[3],
            f1dbecc[2],f1icaf[2],f1hist1[2],f1hist0[2],f1pc4[2],f1way[2],f1brend[2],f1ret[2],
            f1dbecc[1],f1icaf[1],f1hist1[1],f1hist0[1],f1pc4[1],f1way[1],f1brend[1],f1ret[1],
            f1dbecc[0],f1icaf[0],f1hist1[0],f1hist0[0],f1pc4[0],f1way[0],f1brend[0],f1ret[0]
            } = brdata1final[BRDATA_SIZE-1:0];
   assign f2_valid = f2val[0];
   assign sf1_valid = sf1val[0];
   assign sf0_valid = sf0val[0];
   assign consume_fb0 = ~sf0val[0] & f0val[0];
   assign consume_fb1 = ~sf1val[0] & f1val[0];
   assign ifu_fb_consume1 = consume_fb0 & ~consume_fb1 & ~exu_flush_final;
   assign ifu_fb_consume2 = consume_fb0 &  consume_fb1 & ~exu_flush_final;
   assign ifvalid = ifu_fetch_val[0];
   assign shift_f1_f0 =  ~sf0_valid & sf1_valid;
   assign shift_f2_f0 =  ~sf0_valid & ~sf1_valid & f2_valid;
   assign shift_f2_f1 =  ~sf0_valid & sf1_valid & f2_valid;
   assign fetch_to_f0 =  ~sf0_valid & ~sf1_valid & ~f2_valid & ifvalid;
   assign fetch_to_f1 =  (~sf0_valid & ~sf1_valid &  f2_valid & ifvalid)  |
                         (~sf0_valid &  sf1_valid & ~f2_valid & ifvalid)  |
                         ( sf0_valid & ~sf1_valid & ~f2_valid & ifvalid);
   assign fetch_to_f2 =  (~sf0_valid &  sf1_valid &  f2_valid & ifvalid)  |
                         ( sf0_valid &  sf1_valid & ~f2_valid & ifvalid);
   assign f0pc_plus1[31:1] = f0pc[31:1] + 31'd1;
   assign f0pc_plus2[31:1] = f0pc[31:1] + 31'd2;
   assign f0pc_plus3[31:1] = f0pc[31:1] + 31'd3;
   assign f0pc_plus4[31:1] = f0pc[31:1] + 31'd4;
   assign f1pc_plus1[31:1] = f1pc[31:1] + 31'd1;
   assign f1pc_plus2[31:1] = f1pc[31:1] + 31'd2;
   assign f1pc_plus3[31:1] = f1pc[31:1] + 31'd3;
   assign f2pc_in[31:1] = ifu_fetch_pc[31:1];
   rvdffe #(31) f2pcff (.*, .en(f2_wr_en), .din(f2pc_in[31:1]), .dout(f2pc[31:1]));
   assign sf1pc[31:1] = ({31{f1_shift_2B}} & (f1pc_plus1[31:1])) |
                        ({31{f1_shift_4B}} & (f1pc_plus2[31:1])) |
                        ({31{f1_shift_6B}} & (f1pc_plus3[31:1])) |
                        ({31{~f1_shift_2B&~f1_shift_4B&~f1_shift_6B}} & f1pc[31:1]);
   assign f1pc_in[31:1] = ({31{fetch_to_f1}} & ifu_fetch_pc[31:1]) |
                          ({31{shift_f2_f1}} & f2pc[31:1]) |
                          ({31{~fetch_to_f1&~shift_f2_f1}} & sf1pc[31:1]);
   rvdffe #(31) f1pcff (.*, .en(f1_shift_wr_en), .din(f1pc_in[31:1]), .dout(f1pc[31:1]));
   assign sf0pc[31:1] = ({31{shift_2B}} & (f0pc_plus1[31:1])) |
                        ({31{shift_4B}} & (f0pc_plus2[31:1])) |
                        ({31{shift_6B}} & (f0pc_plus3[31:1])) |
                        ({31{shift_8B}} & (f0pc_plus4[31:1]));
   assign f0pc_in[31:1] = ({31{fetch_to_f0}} & ifu_fetch_pc[31:1]) |
                          ({31{shift_f2_f0}} & f2pc[31:1]) |
                          ({31{shift_f1_f0}} & sf1pc[31:1]) |
                          ({31{~fetch_to_f0&~shift_f2_f0&~shift_f1_f0}} & sf0pc[31:1]);
   rvdffe #(31) f0pcff (.*, .en(f0_shift_wr_en), .din(f0pc_in[31:1]), .dout(f0pc[31:1]));
   assign f2val_in[7:0] = (({8{fetch_to_f2}} & ifu_fetch_val[7:0]) |
                           ({8{~fetch_to_f2&~shift_f2_f1&~shift_f2_f0}} & f2val[7:0])) & ~{8{exu_flush_final}};
   rvdff #(8) f2valff (.*, .clk(active_clk), .din(f2val_in[7:0]), .dout(f2val[7:0]));
   assign sf1val[7:0] = ({8{f1_shift_2B}} & {1'b0,f1val[7:1]}) |
                        ({8{f1_shift_4B}} & {2'b0,f1val[7:2]}) |
                        ({8{f1_shift_6B}} & {3'b0,f1val[7:3]}) |
                        ({8{~f1_shift_2B&~f1_shift_4B&~f1_shift_6B}} & f1val[7:0]);
   assign f1val_in[7:0] = (({8{fetch_to_f1}} & ifu_fetch_val[7:0]) |
                           ({8{shift_f2_f1}} & f2val[7:0]) |
                           ({8{~fetch_to_f1&~shift_f2_f1&~shift_f1_f0}} & sf1val[7:0])) & ~{8{exu_flush_final}};
   rvdff #(8) f1valff (.*, .clk(active_clk), .din(f1val_in[7:0]), .dout(f1val[7:0]));
   assign sf0val[7:0] = ({8{shift_2B}} & {1'b0,f0val[7:1]}) |
                        ({8{shift_4B}} & {2'b0,f0val[7:2]}) |
                        ({8{shift_6B}} & {3'b0,f0val[7:3]}) |
                        ({8{shift_8B}} & {4'b0,f0val[7:4]}) |
                        ({8{~shift_2B&~shift_4B&~shift_6B&~shift_8B}} & f0val[7:0]);
   assign f0val_in[7:0] = (({8{fetch_to_f0}} & ifu_fetch_val[7:0]) |
                           ({8{shift_f2_f0}} & f2val[7:0]) |
                           ({8{shift_f1_f0}} & sf1val[7:0]) |
                           ({8{~fetch_to_f0&~shift_f2_f0&~shift_f1_f0}} & sf0val[7:0])) & ~{8{exu_flush_final}};
   rvdff #(8) f0valff (.*, .clk(active_clk), .din(f0val_in[7:0]), .dout(f0val[7:0]));
   rvdffe #(8) q2parityff (.*, .en(qwen[2]), .din(ic_error_f2.parity[7:0]), .dout(q2parity[7:0]));
   rvdffe #(8) q1parityff (.*, .en(qwen[1]), .din(ic_error_f2.parity[7:0]), .dout(q1parity[7:0]));
   rvdffe #(8) q0parityff (.*, .en(qwen[0]), .din(ic_error_f2.parity[7:0]), .dout(q0parity[7:0]));
   assign {q1parityeff[7:0],q0parityeff[7:0]} = (({16{qren[0]}} & {q1parity[7:0],q0parity[7:0]}) |
                                                 ({16{qren[1]}} & {q2parity[7:0],q1parity[7:0]}) |
                                                 ({16{qren[2]}} & {q0parity[7:0],q2parity[7:0]}));
   assign q0parityfinal[7:0] =   (({8{q0sel[0]}} & {     q0parityeff[7:0]}) |
                                  ({8{q0sel[1]}} & {1'b0,q0parityeff[7:1]}) |
                                  ({8{q0sel[2]}} & {2'b0,q0parityeff[7:2]}) |
                                  ({8{q0sel[3]}} & {3'b0,q0parityeff[7:3]}) |
                                  ({8{q0sel[4]}} & {4'b0,q0parityeff[7:4]}) |
                                  ({8{q0sel[5]}} & {5'b0,q0parityeff[7:5]}) |
                                  ({8{q0sel[6]}} & {6'b0,q0parityeff[7:6]}) |
                                  ({8{q0sel[7]}} & {7'b0,q0parityeff[7]}));
   assign q1parityfinal[7:0] =   (({8{q1sel[0]}} & {     q1parityeff[7:0]}) |
                                  ({8{q1sel[1]}} & {1'b0,q1parityeff[7:1]}) |
                                  ({8{q1sel[2]}} & {2'b0,q1parityeff[7:2]}) |
                                  ({8{q1sel[3]}} & {3'b0,q1parityeff[7:3]}) |
                                  ({8{q1sel[4]}} & {4'b0,q1parityeff[7:4]}) |
                                  ({8{q1sel[5]}} & {5'b0,q1parityeff[7:5]}) |
                                  ({8{q1sel[6]}} & {6'b0,q1parityeff[7:6]}) |
                                  ({8{q1sel[7]}} & {7'b0,q1parityeff[7]}));
   rvdffe #(128) q2ff (.*, .en(qwen[2]), .din(ifu_fetch_data[127:0]), .dout(q2[127:0]));
   rvdffe #(128) q1ff (.*, .en(qwen[1]), .din(ifu_fetch_data[127:0]), .dout(q1[127:0]));
   rvdffe #(128) q0ff (.*, .en(qwen[0]), .din(ifu_fetch_data[127:0]), .dout(q0[127:0]));
   assign {q1eff[127:0],q0eff[127:0]} = (({256{qren[0]}} & {q1[127:0],q0[127:0]}) |
                                         ({256{qren[1]}} & {q2[127:0],q1[127:0]}) |
                                         ({256{qren[2]}} & {q0[127:0],q2[127:0]}));
   assign q0final[127:0] = (({128{q0sel[0]}} & {             q0eff[8*16-1:16*0]}) |
                            ({128{q0sel[1]}} & {{16*1{1'b0}},q0eff[8*16-1:16*1]}) |
                            ({128{q0sel[2]}} & {{16*2{1'b0}},q0eff[8*16-1:16*2]}) |
                            ({128{q0sel[3]}} & {{16*3{1'b0}},q0eff[8*16-1:16*3]}) |
                            ({128{q0sel[4]}} & {{16*4{1'b0}},q0eff[8*16-1:16*4]}) |
                            ({128{q0sel[5]}} & {{16*5{1'b0}},q0eff[8*16-1:16*5]}) |
                            ({128{q0sel[6]}} & {{16*6{1'b0}},q0eff[8*16-1:16*6]}) |
                            ({128{q0sel[7]}} & {{16*7{1'b0}},q0eff[8*16-1:16*7]}));
   assign q1final[127:0] = (({128{q1sel[0]}} & {             q1eff[8*16-1:16*0]}) |
                            ({128{q1sel[1]}} & {{16*1{1'b0}},q1eff[8*16-1:16*1]}) |
                            ({128{q1sel[2]}} & {{16*2{1'b0}},q1eff[8*16-1:16*2]}) |
                            ({128{q1sel[3]}} & {{16*3{1'b0}},q1eff[8*16-1:16*3]}) |
                            ({128{q1sel[4]}} & {{16*4{1'b0}},q1eff[8*16-1:16*4]}) |
                            ({128{q1sel[5]}} & {{16*5{1'b0}},q1eff[8*16-1:16*5]}) |
                            ({128{q1sel[6]}} & {{16*6{1'b0}},q1eff[8*16-1:16*6]}) |
                            ({128{q1sel[7]}} & {{16*7{1'b0}},q1eff[8*16-1:16*7]}));
   assign aligndata[63:0] = ({64{(f0val[3])}} &                  {q0final[4*16-1:0]}) |
                            ({64{(f0val[2]&~f0val[3])}} &        {q1final[1*16-1:0],q0final[3*16-1:0]}) |
                            ({64{(f0val[1]&~f0val[2])}} &        {q1final[2*16-1:0],q0final[2*16-1:0]}) |
                            ({64{(f0val[0]&~f0val[1])}} &        {q1final[3*16-1:0],q0final[1*16-1:0]});
   assign alignval[3:0] =   ({4{(f0val[3])}} &                   4'b1111) |
                            ({4{(f0val[2]&~f0val[3])}} &        {f1val[0],3'b111}) |
                            ({4{(f0val[1]&~f0val[2])}} &        {f1val[1:0],2'b11}) |
                            ({4{(f0val[0]&~f0val[1])}} &        {f1val[2:0],1'b1});
   assign alignicaf[3:0] =   ({4{(f0val[3])}} &                   f0icaf[3:0]) |
                             ({4{(f0val[2]&~f0val[3])}} &        {f1icaf[0],f0icaf[2:0]}) |
                             ({4{(f0val[1]&~f0val[2])}} &        {f1icaf[1:0],f0icaf[1:0]}) |
                             ({4{(f0val[0]&~f0val[1])}} &        {f1icaf[2:0],f0icaf[0]});
   assign alignsbecc[3:0] =   ({4{(f0val[3])}} &                  {4{f0sbecc}}) |
                              ({4{(f0val[2]&~f0val[3])}} &        {{1{f1sbecc}},{3{f0sbecc}}}) |
                              ({4{(f0val[1]&~f0val[2])}} &        {{2{f1sbecc}},{2{f0sbecc}}}) |
                              ({4{(f0val[0]&~f0val[1])}} &        {{3{f1sbecc}},{1{f0sbecc}}});
   assign aligndbecc[3:0] =   ({4{(f0val[3])}} &                   f0dbecc[3:0]) |
                              ({4{(f0val[2]&~f0val[3])}} &        {f1dbecc[0],f0dbecc[2:0]}) |
                              ({4{(f0val[1]&~f0val[2])}} &        {f1dbecc[1:0],f0dbecc[1:0]}) |
                              ({4{(f0val[0]&~f0val[1])}} &        {f1dbecc[2:0],f0dbecc[0]});
   assign alignbrend[3:0] =   ({4{(f0val[3])}} &                   f0brend[3:0]) |
                              ({4{(f0val[2]&~f0val[3])}} &        {f1brend[0],f0brend[2:0]}) |
                              ({4{(f0val[1]&~f0val[2])}} &        {f1brend[1:0],f0brend[1:0]}) |
                              ({4{(f0val[0]&~f0val[1])}} &        {f1brend[2:0],f0brend[0]});
   assign alignpc4[3:0] =   ({4{(f0val[3])}} &                   f0pc4[3:0]) |
                            ({4{(f0val[2]&~f0val[3])}} &        {f1pc4[0],f0pc4[2:0]}) |
                            ({4{(f0val[1]&~f0val[2])}} &        {f1pc4[1:0],f0pc4[1:0]}) |
                            ({4{(f0val[0]&~f0val[1])}} &        {f1pc4[2:0],f0pc4[0]});
   assign alignparity[3:0] =   ({4{(f0val[3])}} &                                      q0parityfinal[3:0]) |
                               ({4{(f0val[2]&~f0val[3])}} &        {q1parityfinal[0],  q0parityfinal[2:0]}) |
                               ({4{(f0val[1]&~f0val[2])}} &        {q1parityfinal[1:0],q0parityfinal[1:0]}) |
                               ({4{(f0val[0]&~f0val[1])}} &        {q1parityfinal[2:0],q0parityfinal[0]});
   assign aligntagperr[3:0] =   ({4{(f0val[3])}} &                  {4{f0perr}}) |
                                ({4{(f0val[2]&~f0val[3])}} &        {{1{f1perr}},{3{f0perr}}}) |
                                ({4{(f0val[1]&~f0val[2])}} &        {{2{f1perr}},{2{f0perr}}}) |
                                ({4{(f0val[0]&~f0val[1])}} &        {{3{f1perr}},{1{f0perr}}});
   assign alignicfetch[3:0] =   ({4{(f0val[3])}} &                  {4{f0icfetch}}) |
                                ({4{(f0val[2]&~f0val[3])}} &        {{1{f1icfetch}},{3{f0icfetch}}}) |
                                ({4{(f0val[1]&~f0val[2])}} &        {{2{f1icfetch}},{2{f0icfetch}}}) |
                                ({4{(f0val[0]&~f0val[1])}} &        {{3{f1icfetch}},{1{f0icfetch}}});
   assign alignret[3:0] =   ({4{(f0val[3])}} &                   f0ret[3:0]) |
                            ({4{(f0val[2]&~f0val[3])}} &        {f1ret[0],f0ret[2:0]}) |
                            ({4{(f0val[1]&~f0val[2])}} &        {f1ret[1:0],f0ret[1:0]}) |
                            ({4{(f0val[0]&~f0val[1])}} &        {f1ret[2:0],f0ret[0]});
   assign alignway[3:0] =   ({4{(f0val[3])}} &                   f0way[3:0]) |
                            ({4{(f0val[2]&~f0val[3])}} &        {f1way[0],f0way[2:0]}) |
                            ({4{(f0val[1]&~f0val[2])}} &        {f1way[1:0],f0way[1:0]}) |
                            ({4{(f0val[0]&~f0val[1])}} &        {f1way[2:0],f0way[0]});
   assign alignhist1[3:0] =   ({4{(f0val[3])}} &                   f0hist1[3:0]) |
                              ({4{(f0val[2]&~f0val[3])}} &        {f1hist1[0],f0hist1[2:0]}) |
                              ({4{(f0val[1]&~f0val[2])}} &        {f1hist1[1:0],f0hist1[1:0]}) |
                              ({4{(f0val[0]&~f0val[1])}} &        {f1hist1[2:0],f0hist1[0]});
   assign alignhist0[3:0] =   ({4{(f0val[3])}} &                   f0hist0[3:0]) |
                              ({4{(f0val[2]&~f0val[3])}} &        {f1hist0[0],f0hist0[2:0]}) |
                              ({4{(f0val[1]&~f0val[2])}} &        {f1hist0[1:0],f0hist0[1:0]}) |
                              ({4{(f0val[0]&~f0val[1])}} &        {f1hist0[2:0],f0hist0[0]});
   assign alignfromf1[3:1] =     ({3{(f0val[3])}} &                   3'b0) |
                                 ({3{(f0val[2]&~f0val[3])}} &        {1'b1,2'b0}) |
                                 ({3{(f0val[1]&~f0val[2])}} &        {2'b11,1'b0}) |
                                 ({3{(f0val[0]&~f0val[1])}} &        {3'b111});
   assign { secondpc[31:1],
            thirdpc[31:1],
            fourthpc[31:1] } =   ({3*31{(f0val[3])}}           & {f0pc_plus1[31:1], f0pc_plus2[31:1], f0pc_plus3[31:1]}) |
                                 ({3*31{(f0val[2]&~f0val[3])}} & {f0pc_plus1[31:1], f0pc_plus2[31:1], f1pc[31:1]}) |
                                 ({3*31{(f0val[1]&~f0val[2])}} & {f0pc_plus1[31:1], f1pc[31:1],       f1pc_plus1[31:1]})   |
                                 ({3*31{(f0val[0]&~f0val[1])}} & {f1pc[31:1],       f1pc_plus1[31:1], f1pc_plus2[31:1]});
   assign ifu_i0_pc[31:1] = f0pc[31:1];
   assign firstpc[31:1] = f0pc[31:1];
   assign ifu_i1_pc[31:1] = (first2B) ? secondpc[31:1] : thirdpc[31:1];
   assign ifu_i0_pc4 = first4B;
   assign ifu_i1_pc4 = (first2B & second4B) |
                       (first4B & third4B);
  for (genvar i=0; i<4 ; i++) begin : ic_par_error
   rveven_paritycheck pchk (
                           .data_in(aligndata[16*(i+1)-1: 16*i]),
                           .parity_in(alignparity[i]),
                           .parity_err(aligndataperr[i])
                           );
  end
   assign ifu_i0_cinst[15:0] = aligndata[15:0];
   assign ifu_i1_cinst[15:0] = (first4B) ? aligndata[47:32] : aligndata[31:16];
   assign first4B = aligndata[16*0+1:16*0] == 2'b11;
   assign first2B = ~first4B;
   assign second4B = aligndata[16*1+1:16*1] == 2'b11;
   assign second2B = ~second4B;
   assign third4B = aligndata[16*2+1:16*2] == 2'b11;
   assign third2B = ~third4B;
   assign ifu_i0_valid = ((first4B & alignval[1]) |
                          (first2B & alignval[0])) & ~exu_flush_final;
   assign ifu_i1_valid = ((first4B & third4B & alignval[3])  |
                          (first4B & third2B & alignval[2])  |
                          (first2B & second4B & alignval[2]) |
                          (first2B & second2B & alignval[1])) & ~exu_flush_final;
   assign ifu_i0_icaf = ((first4B & (|alignicaf[1:0])) |
                         (first2B &   alignicaf[0])) & ~exu_flush_final;
   assign icaf_eff[3:0] = alignicaf[3:0] | aligndbecc[3:0];
   assign ifu_i0_icaf_second = first4B & ~icaf_eff[0] & icaf_eff[1];
   assign ifu_i1_icaf = ((first4B & third4B &  (|alignicaf[3:2])) |
                         (first4B & third2B &    alignicaf[2])    |
                         (first2B & second4B & (|alignicaf[2:1])) |
                         (first2B & second2B &   alignicaf[1])) & ~exu_flush_final;
   assign ifu_i1_icaf_second = (first4B & third4B  & ~icaf_eff[2] & icaf_eff[3]) |
                               (first2B & second4B & ~icaf_eff[1] & icaf_eff[2]);
   assign alignfinalperr[3:0] = (aligntagperr[3:0] | aligndataperr[3:0]) & alignicfetch[3:0];
   assign ifu_i0_perr = ((first4B & (|alignfinalperr[1:0])) |
                         (first2B &   alignfinalperr[0])) & ~exu_flush_final;
   assign ifu_i1_perr = ((first4B & third4B &  (|alignfinalperr[3:2])) |
                         (first4B & third2B &    alignfinalperr[2])    |
                         (first2B & second4B & (|alignfinalperr[2:1])) |
                         (first2B & second2B &   alignfinalperr[1])) & ~exu_flush_final;
   assign ifu_i0_sbecc = ((first4B & (|alignsbecc[1:0])) |
                          (first2B &   alignsbecc[0])) & ~exu_flush_final;
   assign ifu_i1_sbecc = ((first4B & third4B &  (|alignsbecc[3:2])) |
                          (first4B & third2B &    alignsbecc[2])    |
                          (first2B & second4B & (|alignsbecc[2:1])) |
                          (first2B & second2B &   alignsbecc[1])) & ~exu_flush_final;
   assign ifu_i0_dbecc = ((first4B & (|aligndbecc[1:0])) |
                          (first2B &   aligndbecc[0])) & ~exu_flush_final;
   assign ifu_i1_dbecc = ((first4B & third4B &  (|aligndbecc[3:2])) |
                          (first4B & third2B &    aligndbecc[2])    |
                          (first2B & second4B & (|aligndbecc[2:1])) |
                          (first2B & second2B &   aligndbecc[1])) & ~exu_flush_final;
   logic [2:0]  alignicerr;
   assign alignicerr[2:0] = alignfinalperr[2:0] | alignsbecc[2:0];
   assign ifu_icache_error_index[16:2] = (alignicerr[0]) ?  firstpc[16:2] :
                                          (alignicerr[1]) ? secondpc[16:2] :
                                          (alignicerr[2]) ?  thirdpc[16:2] :
                                                            fourthpc[16:2];
   assign ifu_icache_error_val = (i0_shift & ifu_i0_perr) |
                                 (i1_shift & ifu_i1_perr & ~ifu_i0_sbecc);
   assign ifu_icache_sb_error_val = (i0_shift & ifu_i0_sbecc) |
                                    (i1_shift & ifu_i1_sbecc & ~ifu_i0_perr);
   assign ifirst[31:0] =  aligndata[2*16-1:0*16];
   assign isecond[31:0] = aligndata[3*16-1:1*16];
   assign ithird[31:0] =  aligndata[4*16-1:2*16];
   assign ifu_i0_instr[31:0] = ({32{first4B}} & ifirst[31:0]) |
                               ({32{first2B}} & uncompress0[31:0]);
   assign ifu_i1_instr[31:0] = ({32{first4B & third4B}} & ithird[31:0]) |
                               ({32{first4B & third2B}} & uncompress2[31:0]) |
                               ({32{first2B & second4B}} & isecond[31:0]) |
                               ({32{first2B & second2B}} & uncompress1[31:0]);
   rvbtb_addr_hash firsthash(.pc(firstpc[31:1]), .hash(firstpc_hash[5:4]));
   rvbtb_addr_hash secondhash(.pc(secondpc[31:1]), .hash(secondpc_hash[5:4]));
   rvbtb_addr_hash thirdhash(.pc(thirdpc[31:1]), .hash(thirdpc_hash[5:4]));
   rvbtb_addr_hash fourthhash(.pc(fourthpc[31:1]), .hash(fourthpc_hash[5:4]));
   logic [9-1:0] firstbrtag_hash, secondbrtag_hash, thirdbrtag_hash, fourthbrtag_hash;
   rvbtb_tag_hash first_brhash(.pc(firstpc[31:1]), .hash(firstbrtag_hash[9-1:0]));
   rvbtb_tag_hash second_brhash(.pc(secondpc[31:1]), .hash(secondbrtag_hash[9-1:0]));
   rvbtb_tag_hash third_brhash(.pc(thirdpc[31:1]), .hash(thirdbrtag_hash[9-1:0]));
   rvbtb_tag_hash fourth_brhash(.pc(fourthpc[31:1]), .hash(fourthbrtag_hash[9-1:0]));
   always_comb begin
      i0_brp = '0;
      i0_br_start_error = (first4B & alignval[1] & alignbrend[0]);
      i0_brp.valid = (first2B & alignbrend[0]) |
                     (first4B & alignbrend[1]) |
                     i0_br_start_error;
      i0_brp_pc4 = (first2B & alignpc4[0]) |
                   (first4B & alignpc4[1]);
      i0_brp.ret = (first2B & alignret[0]) |
                   (first4B & alignret[1]);
      i0_brp.way = (first2B | alignbrend[0]) ? alignway[0] : alignway[1];
      i0_brp.hist[1] = (first2B & alignhist1[0]) |
                       (first4B & alignhist1[1]);
      i0_brp.hist[0] = (first2B & alignhist0[0]) |
                       (first4B & alignhist0[1]);
      i0_ends_f1 = (first4B & alignfromf1[1]);
      i0_brp.toffset[11:0] = (i0_ends_f1) ? f1poffset[11:0] : f0poffset[11:0];
      i0_brp.fghr[4:0] = (i0_ends_f1) ? f1fghr[4:0] : f0fghr[4:0];
      i0_brp.prett[31:1] = (i0_ends_f1) ? f1prett[31:1] : f0prett[31:1];
      i0_brp.br_start_error = i0_br_start_error;
      i0_brp.index[5:4] = (first2B | alignbrend[0]) ? firstpc_hash[5:4]:
                                                                                  secondpc_hash[5:4];
      i0_brp.btag[9-1:0] = (first2B | alignbrend[0]) ? firstbrtag_hash[9-1:0]:
                                                                       secondbrtag_hash[9-1:0];
      i0_brp.bank[1:0] = (first2B | alignbrend[0]) ? firstpc[3:2] :
                                                     secondpc[3:2];
      i0_brp.br_error = (i0_brp.valid &  i0_brp_pc4 &  first2B) |
                        (i0_brp.valid & ~i0_brp_pc4 &  first4B);
      i1_brp = '0;
      i1_br_start_error = (first2B & second4B & alignval[2] & alignbrend[1]) |
                          (first4B & third4B  & alignval[3] & alignbrend[2]);
      i1_brp.valid = (first4B & third2B & alignbrend[2]) |
                     (first4B & third4B & alignbrend[3]) |
                     (first2B & second2B & alignbrend[1]) |
                     (first2B & second4B & alignbrend[2]) |
                     i1_br_start_error;
      i1_brp_pc4 = (first4B & third2B & alignpc4[2]) |
                   (first4B & third4B & alignpc4[3]) |
                   (first2B & second2B & alignpc4[1]) |
                   (first2B & second4B & alignpc4[2]);
      i1_brp.ret = (first4B & third2B & alignret[2]) |
                   (first4B & third4B & alignret[3]) |
                   (first2B & second2B & alignret[1]) |
                   (first2B & second4B & alignret[2]);
      i1_brp.way = (first4B & third2B                   & alignway[2] ) |
                   (first4B & third4B &  alignbrend[2]  & alignway[2] ) |
                   (first4B & third4B & ~alignbrend[2]  & alignway[3] ) |
                   (first2B & second2B                  & alignway[1] ) |
                   (first2B & second4B &  alignbrend[1] & alignway[1] ) |
                   (first2B & second4B & ~alignbrend[1] & alignway[2] );
      i1_brp.hist[1] = (first4B & third2B & alignhist1[2]) |
                       (first4B & third4B & alignhist1[3]) |
                       (first2B & second2B & alignhist1[1]) |
                       (first2B & second4B & alignhist1[2]);
      i1_brp.hist[0] = (first4B & third2B & alignhist0[2]) |
                       (first4B & third4B & alignhist0[3]) |
                       (first2B & second2B & alignhist0[1]) |
                       (first2B & second4B & alignhist0[2]);
      i1_ends_f1 = (first4B & third2B & alignfromf1[2]) |
                   (first4B & third4B & alignfromf1[3]) |
                   (first2B & second2B & alignfromf1[1]) |
                   (first2B & second4B & alignfromf1[2]);
      i1_brp.toffset[11:0] = (i1_ends_f1) ? f1poffset[11:0] : f0poffset[11:0];
      i1_brp.fghr[4:0] = (i1_ends_f1) ? f1fghr[4:0] : f0fghr[4:0];
      i1_brp.prett[31:1] = (i1_ends_f1) ? f1prett[31:1] : f0prett[31:1];
      i1_brp.br_start_error = i1_br_start_error;
      i1_brp.index[5:4] = ({5-4+1{first4B & third2B }}                  & thirdpc_hash[5:4] ) |
                                                ({5-4+1{first4B & third4B &  alignbrend[2] }} & thirdpc_hash[5:4] ) |
                                                ({5-4+1{first4B & third4B & ~alignbrend[2] }} & fourthpc_hash[5:4] ) |
                                                ({5-4+1{first2B & second2B}}                  & secondpc_hash[5:4] ) |
                                                ({5-4+1{first2B & second4B &  alignbrend[1]}} & secondpc_hash[5:4] ) |
                                                ({5-4+1{first2B & second4B & ~alignbrend[1]}} & thirdpc_hash[5:4] );
      i1_brp.btag[9-1:0] = ({9{first4B & third2B }}                  &  thirdbrtag_hash[9-1:0] ) |
                                           ({9{first4B & third4B &  alignbrend[2] }} &  thirdbrtag_hash[9-1:0] ) |
                                           ({9{first4B & third4B & ~alignbrend[2] }} & fourthbrtag_hash[9-1:0] ) |
                                           ({9{first2B & second2B}}                  & secondbrtag_hash[9-1:0] ) |
                                           ({9{first2B & second4B &  alignbrend[1]}} & secondbrtag_hash[9-1:0] ) |
                                           ({9{first2B & second4B & ~alignbrend[1]}} &  thirdbrtag_hash[9-1:0] );
      i1_brp.bank[1:0] = ({2{first4B & third2B }}                  & thirdpc[3:2] ) |
                         ({2{first4B & third4B &  alignbrend[2] }} & thirdpc[3:2] ) |
                         ({2{first4B & third4B & ~alignbrend[2] }} & fourthpc[3:2] ) |
                         ({2{first2B & second2B}}                  & secondpc[3:2] ) |
                         ({2{first2B & second4B &  alignbrend[1]}} & secondpc[3:2] ) |
                         ({2{first2B & second4B & ~alignbrend[1]}} & thirdpc[3:2] );
      i1_brp.br_error = (i1_brp.valid &  i1_brp_pc4 & first4B & third2B ) |
                        (i1_brp.valid & ~i1_brp_pc4 & first4B & third4B ) |
                        (i1_brp.valid &  i1_brp_pc4 & first2B & second2B) |
                        (i1_brp.valid & ~i1_brp_pc4 & first2B & second4B);
   end
   assign i0_illegal = (first2B & ~first_legal);
   assign i1_illegal = (first2B & second2B & ~second_legal) |
                       (first4B & third2B & ~third_legal);
   assign shift_illegal = (i0_shift & i0_illegal) |
                          (i1_shift & i1_illegal);
   assign illegal_inst[15:0] =   (first2B & ~first_legal)             ? aligndata[1*16-1:0*16] :
                                ((first2B & second2B & ~second_legal) ? aligndata[2*16-1:1*16] : aligndata[3*16-1:2*16]);
   assign illegal_inst_en = shift_illegal & ~illegal_lockout;
   rvdffe #(16) illegal_any_ff (.*, .en(illegal_inst_en), .din(illegal_inst[15:0]), .dout(ifu_illegal_inst[15:0]));
   assign illegal_lockout_in = (shift_illegal | illegal_lockout) & ~exu_flush_final;
   rvdff #(1) illegal_lockout_any_ff (.*, .clk(active_clk), .din(illegal_lockout_in), .dout(illegal_lockout));
   ifu_compress_ctl compress0 (.din(aligndata[16*1-1:0*16]), .dout(uncompress0[31:0]), .legal(first_legal)  );
   ifu_compress_ctl compress1 (.din(aligndata[16*2-1:1*16]), .dout(uncompress1[31:0]), .legal(second_legal) );
   ifu_compress_ctl compress2 (.din(aligndata[16*3-1:2*16]), .dout(uncompress2[31:0]), .legal(third_legal)  );
   assign i0_shift = ifu_i0_valid & ibuffer_room1_more;
   assign i1_shift = ifu_i1_valid & ibuffer_room2_more;
   if (DEC_INSTBUF_DEPTH==4) begin
      assign ibuffer_room1_more = ~dec_ib3_valid_d;
      assign ibuffer_room2_more = ~dec_ib2_valid_d;
   end
   else begin
      assign ibuffer_room1_more = ~dec_ib0_valid_eff_d | ~dec_ib1_valid_eff_d;
      assign ibuffer_room2_more = ~dec_ib0_valid_eff_d & ~dec_ib1_valid_eff_d;
   end
   assign ifu_pmu_instr_aligned[1:0] = { i1_shift, i0_shift };
   assign ifu_pmu_align_stall = ifu_i0_valid & ~ibuffer_room1_more;
   assign shift_2B = i0_shift & ~i1_shift & first2B;
   assign shift_4B = (i0_shift & ~i1_shift & first4B) |
                     (i0_shift &  i1_shift & first2B & second2B);
   assign shift_6B = (i0_shift &  i1_shift & first2B & second4B) |
                     (i0_shift &  i1_shift & first4B & third2B);
   assign shift_8B = i0_shift &  i1_shift & first4B & third4B;
   assign f0_shift_2B = (shift_2B & f0val[0]) |
                        ((shift_4B | shift_6B | shift_8B) & f0val[0] & ~f0val[1]);
   assign f0_shift_4B = (shift_4B & f0val[1]) |
                        ((shift_6B & shift_8B) & f0val[1] & ~f0val[2]);
   assign f0_shift_6B = (shift_6B & f0val[2]) |
                        (shift_8B & f0val[2] & ~f0val[3]);
   assign f0_shift_8B =  shift_8B & f0val[3];
   assign f1_shift_2B = (f0val[2] & ~f0val[3] & shift_8B) |
                        (f0val[1] & ~f0val[2] & shift_6B) |
                        (f0val[0] & ~f0val[1] & shift_4B);
   assign f1_shift_4B = (f0val[1] & ~f0val[2] & shift_8B) |
                        (f0val[0] & ~f0val[1] & shift_6B);
   assign f1_shift_6B = (f0val[0] & ~f0val[1] & shift_8B);
endmodule
module ifu_compress_ctl
  (
   input  logic [15:0] din,
   output logic [31:0] dout,
   output logic legal
   );
   logic [15:0]  i;
   logic [31:0]  o,l1,l2,l3;
   assign i[15:0] = din[15:0];
   logic [4:0]   rs2d,rdd,rdpd,rs2pd;
logic rdrd;
logic rdrs1;
logic rs2rs2;
logic rdprd;
logic rdprs1;
logic rs2prs2;
logic rs2prd;
logic uimm9_2;
logic ulwimm6_2;
logic ulwspimm7_2;
logic rdeq2;
logic rdeq1;
logic rs1eq2;
logic sbroffset8_1;
logic simm9_4;
logic simm5_0;
logic sjaloffset11_1;
logic sluimm17_12;
logic uimm5_0;
logic uswimm6_2;
logic uswspimm7_2;
   assign rs2d[4:0] = i[6:2];
   assign rdd[4:0] = i[11:7];
   assign rdpd[4:0] = {2'b01, i[9:7]};
   assign rs2pd[4:0] = {2'b01, i[4:2]};
   assign l1[6:0] = o[6:0];
   assign l1[11:7] = o[11:7] |
                     ({5{rdrd}} & rdd[4:0]) |
                     ({5{rdprd}} & rdpd[4:0]) |
                     ({5{rs2prd}} & rs2pd[4:0]) |
                     ({5{rdeq1}} & 5'd1) |
                     ({5{rdeq2}} & 5'd2);
   assign l1[14:12] = o[14:12];
   assign l1[19:15] = o[19:15] |
                      ({5{rdrs1}} & rdd[4:0]) |
                      ({5{rdprs1}} & rdpd[4:0]) |
                      ({5{rs1eq2}} & 5'd2);
   assign l1[24:20] = o[24:20] |
                      ({5{rs2rs2}} & rs2d[4:0]) |
                      ({5{rs2prs2}} & rs2pd[4:0]);
   assign l1[31:25] = o[31:25];
   logic [5:0] simm5d;
   logic [9:2] uimm9d;
   logic [9:4] simm9d;
   logic [6:2] ulwimm6d;
   logic [7:2] ulwspimm7d;
   logic [5:0] uimm5d;
   logic [20:1] sjald;
   logic [31:12] sluimmd;
   assign simm5d[5:0] = { i[12], i[6:2] };
   assign uimm9d[9:2] = { i[10:7], i[12:11], i[5], i[6] };
   assign simm9d[9:4] = { i[12], i[4:3], i[5], i[2], i[6] };
   assign ulwimm6d[6:2] = { i[5], i[12:10], i[6] };
   assign ulwspimm7d[7:2] = { i[3:2], i[12], i[6:4] };
   assign uimm5d[5:0] = { i[12], i[6:2] };
   assign sjald[11:1] = { i[12], i[8], i[10:9], i[6], i[7], i[2], i[11], i[5:4], i[3] };
   assign sjald[20:12] =  {9{i[12]}};
   assign sluimmd[31:12] = { {15{i[12]}}, i[6:2] };
   assign l2[31:20] = ( l1[31:20] ) |
                      ( {12{simm5_0}}   &  {{7{simm5d[5]}},simm5d[4:0]} ) |
                      ( {12{uimm9_2}}   &  {2'b0,uimm9d[9:2],2'b0} ) |
                      ( {12{simm9_4}}   &   {{3{simm9d[9]}},simm9d[8:4],4'b0} ) |
                      ( {12{ulwimm6_2}} &   {5'b0,ulwimm6d[6:2],2'b0} ) |
                      ( {12{ulwspimm7_2}}  & {4'b0,ulwspimm7d[7:2],2'b0} ) |
                      ( {12{uimm5_0}}      &    {6'b0,uimm5d[5:0]} ) |
                      ( {12{sjaloffset11_1}} &  {sjald[20],sjald[10:1],sjald[11]} ) |
                      ( {12{sluimm17_12}}    &  sluimmd[31:20] );
   assign l2[19:12] = ( l1[19:12] ) |
                      ( {8{sjaloffset11_1}} & sjald[19:12] ) |
                      ( {8{sluimm17_12}} & sluimmd[19:12] );
   assign l2[11:0] = l1[11:0];
   logic [8:1]   sbr8d;
   logic [6:2]   uswimm6d;
   logic [7:2]   uswspimm7d;
   assign sbr8d[8:1] =   { i[12], i[6], i[5], i[2], i[11], i[10], i[4], i[3] };
   assign uswimm6d[6:2] = { i[5], i[12:10], i[6] };
   assign uswspimm7d[7:2] = { i[8:7], i[12:9] };
   assign l3[31:25] = ( l2[31:25] ) |
                      ( {7{sbroffset8_1}} & { {4{sbr8d[8]}},sbr8d[7:5] } ) |
                      ( {7{uswimm6_2}}    & { 5'b0, uswimm6d[6:5] } ) |
                      ( {7{uswspimm7_2}} & { 4'b0, uswspimm7d[7:5] } );
   assign l3[24:12] = l2[24:12];
   assign l3[11:7] = ( l2[11:7] ) |
                     ( {5{sbroffset8_1}} & { sbr8d[4:1], sbr8d[8] } ) |
                     ( {5{uswimm6_2}} & { uswimm6d[4:2], 2'b0 } ) |
                     ( {5{uswspimm7_2}} & { uswspimm7d[4:2], 2'b0 } );
   assign l3[6:0] = l2[6:0];
   assign dout[31:0] = l3[31:0] & {32{legal}};
assign rdrd = (!i[14]&i[6]&i[1]) | (!i[15]&i[14]&i[11]&i[0]) | (!i[14]&i[5]&i[1]) | (
    !i[15]&i[14]&i[10]&i[0]) | (!i[14]&i[4]&i[1]) | (!i[15]&i[14]&i[9]
    &i[0]) | (!i[14]&i[3]&i[1]) | (!i[15]&i[14]&!i[8]&i[0]) | (!i[14]
    &i[2]&i[1]) | (!i[15]&i[14]&i[7]&i[0]) | (!i[15]&i[1]) | (!i[15]
    &!i[13]&i[0]);
assign rdrs1 = (!i[14]&i[12]&i[11]&i[1]) | (!i[14]&i[12]&i[10]&i[1]) | (!i[14]
    &i[12]&i[9]&i[1]) | (!i[14]&i[12]&i[8]&i[1]) | (!i[14]&i[12]&i[7]
    &i[1]) | (!i[14]&!i[12]&!i[6]&!i[5]&!i[4]&!i[3]&!i[2]&i[1]) | (!i[14]
    &i[12]&i[6]&i[1]) | (!i[14]&i[12]&i[5]&i[1]) | (!i[14]&i[12]&i[4]
    &i[1]) | (!i[14]&i[12]&i[3]&i[1]) | (!i[14]&i[12]&i[2]&i[1]) | (
    !i[15]&!i[14]&!i[13]&i[0]) | (!i[15]&!i[14]&i[1]);
assign rs2rs2 = (i[15]&i[6]&i[1]) | (i[15]&i[5]&i[1]) | (i[15]&i[4]&i[1]) | (
    i[15]&i[3]&i[1]) | (i[15]&i[2]&i[1]) | (i[15]&i[14]&i[1]);
assign rdprd = (i[15]&!i[14]&!i[13]&i[0]);
assign rdprs1 = (i[15]&!i[13]&i[0]) | (i[15]&i[14]&i[0]) | (i[14]&!i[1]&!i[0]);
assign rs2prs2 = (i[15]&!i[14]&!i[13]&i[11]&i[10]&i[0]) | (i[15]&!i[1]&!i[0]);
assign rs2prd = (!i[15]&!i[1]&!i[0]);
assign uimm9_2 = (!i[14]&!i[1]&!i[0]);
assign ulwimm6_2 = (!i[15]&i[14]&!i[1]&!i[0]);
assign ulwspimm7_2 = (!i[15]&i[14]&i[1]);
assign rdeq2 = (!i[15]&i[14]&i[13]&!i[11]&!i[10]&!i[9]&i[8]&!i[7]);
assign rdeq1 = (!i[14]&i[12]&i[11]&!i[6]&!i[5]&!i[4]&!i[3]&!i[2]&i[1]) | (!i[14]
    &i[12]&i[10]&!i[6]&!i[5]&!i[4]&!i[3]&!i[2]&i[1]) | (!i[14]&i[12]&i[9]
    &!i[6]&!i[5]&!i[4]&!i[3]&!i[2]&i[1]) | (!i[14]&i[12]&i[8]&!i[6]&!i[5]
    &!i[4]&!i[3]&!i[2]&i[1]) | (!i[14]&i[12]&i[7]&!i[6]&!i[5]&!i[4]&!i[3]
    &!i[2]&i[1]) | (!i[15]&!i[14]&i[13]);
assign rs1eq2 = (!i[15]&i[14]&i[13]&!i[11]&!i[10]&!i[9]&i[8]&!i[7]) | (i[14]
    &i[1]) | (!i[14]&!i[1]&!i[0]);
assign sbroffset8_1 = (i[15]&i[14]&i[0]);
assign simm9_4 = (!i[15]&i[14]&i[13]&!i[11]&!i[10]&!i[9]&i[8]&!i[7]);
assign simm5_0 = (!i[14]&!i[13]&i[11]&!i[10]&i[0]) | (!i[15]&!i[13]&i[0]);
assign sjaloffset11_1 = (!i[14]&i[13]);
assign sluimm17_12 = (!i[15]&i[14]&i[13]&i[7]) | (!i[15]&i[14]&i[13]&!i[8]) | (
    !i[15]&i[14]&i[13]&i[9]) | (!i[15]&i[14]&i[13]&i[10]) | (!i[15]&i[14]
    &i[13]&i[11]);
assign uimm5_0 = (i[15]&!i[14]&!i[13]&!i[11]&i[0]) | (!i[15]&!i[14]&i[1]);
assign uswimm6_2 = (i[15]&!i[1]&!i[0]);
assign uswspimm7_2 = (i[15]&i[14]&i[1]);
assign o[31]  = 1'b0;
assign o[30] = (i[15]&!i[14]&!i[13]&i[10]&!i[6]&!i[5]&i[0]) | (i[15]&!i[14]
    &!i[13]&!i[11]&i[10]&i[0]);
assign o[29]  = 1'b0;
assign o[28]  = 1'b0;
assign o[27]  = 1'b0;
assign o[26]  = 1'b0;
assign o[25]  = 1'b0;
assign o[24]  = 1'b0;
assign o[23]  = 1'b0;
assign o[22]  = 1'b0;
assign o[21]  = 1'b0;
assign o[20] = (!i[14]&i[12]&!i[11]&!i[10]&!i[9]&!i[8]&!i[7]&!i[6]&!i[5]&!i[4]
    &!i[3]&!i[2]&i[1]);
assign o[19]  = 1'b0;
assign o[18]  = 1'b0;
assign o[17]  = 1'b0;
assign o[16]  = 1'b0;
assign o[15]  = 1'b0;
assign o[14] = (i[15]&!i[14]&!i[13]&!i[11]&i[0]) | (i[15]&!i[14]&!i[13]&!i[10]
    &i[0]) | (i[15]&!i[14]&!i[13]&i[6]&i[0]) | (i[15]&!i[14]&!i[13]&i[5]
    &i[0]);
assign o[13] = (i[15]&!i[14]&!i[13]&i[11]&!i[10]&i[0]) | (i[15]&!i[14]&!i[13]
    &i[11]&i[6]&i[0]) | (i[14]&!i[0]);
assign o[12] = (i[15]&!i[14]&!i[13]&i[6]&i[5]&i[0]) | (i[15]&!i[14]&!i[13]&!i[11]
    &i[0]) | (i[15]&!i[14]&!i[13]&!i[10]&i[0]) | (!i[15]&!i[14]&i[1]) | (
    i[15]&i[14]&i[13]);
assign o[11]  = 1'b0;
assign o[10]  = 1'b0;
assign o[9]  = 1'b0;
assign o[8]  = 1'b0;
assign o[7]  = 1'b0;
assign o[6] = (i[15]&!i[14]&!i[6]&!i[5]&!i[4]&!i[3]&!i[2]&!i[0]) | (!i[14]&i[13]) | (
    i[15]&i[14]&i[0]);
assign o[5] = (i[15]&!i[0]) | (i[15]&i[11]&i[10]) | (i[13]&!i[8]) | (i[13]&i[7]) | (
    i[13]&i[9]) | (i[13]&i[10]) | (i[13]&i[11]) | (!i[14]&i[13]) | (
    i[15]&i[14]);
assign o[4] = (!i[14]&!i[11]&!i[10]&!i[9]&!i[8]&!i[7]&!i[0]) | (!i[15]&!i[14]
    &!i[0]) | (!i[14]&i[6]&!i[0]) | (!i[15]&i[14]&i[0]) | (!i[14]&i[5]
    &!i[0]) | (!i[14]&i[4]&!i[0]) | (!i[14]&!i[13]&i[0]) | (!i[14]&i[3]
    &!i[0]) | (!i[14]&i[2]&!i[0]);
assign o[3] = (!i[14]&i[13]);
assign o[2] = (!i[14]&i[12]&i[11]&!i[6]&!i[5]&!i[4]&!i[3]&!i[2]&i[1]) | (!i[14]
    &i[12]&i[10]&!i[6]&!i[5]&!i[4]&!i[3]&!i[2]&i[1]) | (!i[14]&i[12]&i[9]
    &!i[6]&!i[5]&!i[4]&!i[3]&!i[2]&i[1]) | (!i[14]&i[12]&i[8]&!i[6]&!i[5]
    &!i[4]&!i[3]&!i[2]&i[1]) | (!i[14]&i[12]&i[7]&!i[6]&!i[5]&!i[4]&!i[3]
    &!i[2]&i[1]) | (i[15]&!i[14]&!i[12]&!i[6]&!i[5]&!i[4]&!i[3]&!i[2]
    &!i[0]) | (!i[15]&i[13]&!i[8]) | (!i[15]&i[13]&i[7]) | (!i[15]&i[13]
    &i[9]) | (!i[15]&i[13]&i[10]) | (!i[15]&i[13]&i[11]) | (!i[14]&i[13]);
assign o[1]  = 1'b1;
assign o[0]  = 1'b1;
assign legal = (!i[13]&!i[12]&i[11]&i[1]&!i[0]) | (!i[13]&!i[12]&i[6]&i[1]&!i[0]) | (
    !i[15]&!i[13]&i[11]&!i[1]) | (!i[13]&!i[12]&i[5]&i[1]&!i[0]) | (
    !i[13]&!i[12]&i[10]&i[1]&!i[0]) | (!i[15]&!i[13]&i[6]&!i[1]) | (
    i[15]&!i[12]&!i[1]&i[0]) | (!i[13]&!i[12]&i[9]&i[1]&!i[0]) | (!i[12]
    &i[6]&!i[1]&i[0]) | (!i[15]&!i[13]&i[5]&!i[1]) | (!i[13]&!i[12]&i[8]
    &i[1]&!i[0]) | (!i[12]&i[5]&!i[1]&i[0]) | (!i[15]&!i[13]&i[10]&!i[1]) | (
    !i[13]&!i[12]&i[7]&i[1]&!i[0]) | (i[12]&i[11]&!i[10]&!i[1]&i[0]) | (
    !i[15]&!i[13]&i[9]&!i[1]) | (!i[13]&!i[12]&i[4]&i[1]&!i[0]) | (i[13]
    &i[12]&!i[1]&i[0]) | (!i[15]&!i[13]&i[8]&!i[1]) | (!i[13]&!i[12]&i[3]
    &i[1]&!i[0]) | (i[13]&i[4]&!i[1]&i[0]) | (!i[13]&!i[12]&i[2]&i[1]
    &!i[0]) | (!i[15]&!i[13]&i[7]&!i[1]) | (i[13]&i[3]&!i[1]&i[0]) | (
    i[13]&i[2]&!i[1]&i[0]) | (i[14]&!i[13]&!i[1]) | (!i[14]&!i[12]&!i[1]
    &i[0]) | (i[15]&!i[13]&i[12]&i[1]&!i[0]) | (!i[15]&!i[13]&!i[12]&i[1]
    &!i[0]) | (!i[15]&!i[13]&i[12]&!i[1]) | (i[14]&!i[13]&!i[0]);
endmodule
module ifu_ifc_ctl
  (
   input logic clk,
   input logic free_clk,
   input logic active_clk,
   input logic clk_override,  
   input logic rst_l,  
   input logic scan_mode,  
   input logic ic_hit_f2,       
   input logic ic_crit_wd_rdy,  
   input logic ifu_ic_mb_empty,  
   input logic ifu_fb_consume1,   
   input logic ifu_fb_consume2,   
   input logic dec_tlu_flush_noredir_wb,  
   input logic dec_tlu_dbg_halted,  
   input logic dec_tlu_pmu_fw_halted,  
   input logic exu_flush_final,  
   input logic [31:1] exu_flush_path_final,  
   input logic ifu_bp_kill_next_f2,  
   input logic [31:1] ifu_bp_btb_target_f2,  
   input logic ic_dma_active,  
   input logic ic_write_stall,  
   input logic dma_iccm_stall_any,  
   input logic [31:0]  dec_tlu_mrac_ff ,    
   output logic  ifc_fetch_uncacheable_f1,  
   output logic [31:1] ifc_fetch_addr_f1,  
   output logic [31:1] ifc_fetch_addr_f2,   
   output logic  ifc_fetch_req_f1,   
   output logic  ifc_fetch_req_f1_raw,  
   output logic  ifc_fetch_req_f2,   
   output logic  ifu_pmu_fetch_stall,  
   output logic  ifc_iccm_access_f1,  
   output logic  ifc_region_acc_fault_f1,  
   output logic  ifc_dma_access_ok  
   );
   logic [31:1]  fetch_addr_bf,  miss_addr, ifc_fetch_addr_f1_raw;
   logic [31:1]  fetch_addr_next;
   logic [31:1]  miss_addr_ns;
   logic [4:0]   cacheable_select;
   logic [3:0]   fb_write_f1, fb_write_ns;
   logic         ifc_fetch_req_bf;
   logic         overflow_nc;
   logic         fb_full_f1_ns, fb_full_f1;
   logic         fb_right, fb_right2, fb_right3, fb_left, wfm, fetch_ns, idle;
   logic         fetch_req_f2_ns;
   logic         missff_en;
   logic         fetch_crit_word, ic_crit_wd_rdy_d1, fetch_crit_word_d1, fetch_crit_word_d2;
   logic         reset_delayed, reset_detect, reset_detected;
   logic         sel_last_addr_bf, sel_miss_addr_bf, sel_btb_addr_bf, sel_next_addr_bf;
   logic         miss_f2, miss_a;
   logic         flush_fb, dma_iccm_stall_any_f;
   logic         dec_tlu_halted_f;
   logic         mb_empty_mod, goto_idle, leave_idle;
   logic         ic_crit_wd_rdy_mod;
   logic         miss_sel_flush;
   logic         miss_sel_f2;
   logic         miss_sel_f1;
   logic         miss_sel_bf;
   logic         fetch_bf_en;
   logic         ifc_fetch_req_f2_raw;
   logic ifc_f2_clk;
   rvoclkhdr ifu_fa2_cgc ( .en(ifc_fetch_req_f1 | clk_override), .l1clk(ifc_f2_clk), .* );
   typedef enum  logic [1:0] { IDLE=2'b00, FETCH=2'b01, STALL=2'b10, WFM=2'b11} state_t;
   state_t state, next_state;
   logic dma_stall;
   assign dma_stall = ic_dma_active | dma_iccm_stall_any_f;
   rvdff #(2) reset_ff (.*, .clk(free_clk), .din({1'b1, reset_detect}), .dout({reset_detect, reset_detected}));
   assign reset_delayed = reset_detect ^ reset_detected;
   rvdff #(3) ran_ff (.*, .clk(free_clk), .din({dma_iccm_stall_any, dec_tlu_dbg_halted | dec_tlu_pmu_fw_halted, miss_f2}), .dout({dma_iccm_stall_any_f, dec_tlu_halted_f, miss_a}));
   assign ic_crit_wd_rdy_mod = ic_crit_wd_rdy & ~(fetch_crit_word_d2 & ~ifc_fetch_req_f2);
   assign fetch_crit_word = ic_crit_wd_rdy_mod & ~ic_crit_wd_rdy_d1 & ~exu_flush_final & ~ic_write_stall;
   assign missff_en = exu_flush_final | (~ic_hit_f2 & ifc_fetch_req_f2) | ifu_bp_kill_next_f2 | fetch_crit_word_d1 | ifu_bp_kill_next_f2 | (ifc_fetch_req_f2 & ~ifc_fetch_req_f1 & ~fetch_crit_word_d2);
   assign miss_sel_flush = exu_flush_final & (((wfm | idle) & ~fetch_crit_word_d1)  | dma_stall | ic_write_stall);
   assign miss_sel_f2 = ~exu_flush_final & ~ic_hit_f2 & ifc_fetch_req_f2;
   assign miss_sel_f1 = ~exu_flush_final & ~miss_sel_f2 & ~ifc_fetch_req_f1 & ifc_fetch_req_f2 & ~fetch_crit_word_d2 & ~ifu_bp_kill_next_f2;
   assign miss_sel_bf = ~miss_sel_f2 & ~miss_sel_f1 & ~miss_sel_flush;
   assign miss_addr_ns[31:1] = ( ({31{miss_sel_flush}} & exu_flush_path_final[31:1]) |
                                 ({31{miss_sel_f2}} & ifc_fetch_addr_f2[31:1]) |
                                 ({31{miss_sel_f1}} & ifc_fetch_addr_f1[31:1]) |
                                 ({31{miss_sel_bf}} & fetch_addr_bf[31:1]));
   rvdffe #(31) faddmiss_ff (.*, .en(missff_en), .din(miss_addr_ns[31:1]), .dout(miss_addr[31:1]));
   assign sel_last_addr_bf = ~miss_sel_flush & ~ifc_fetch_req_f1 & ifc_fetch_req_f2 & ~ifu_bp_kill_next_f2;
   assign sel_miss_addr_bf = ~miss_sel_flush & ~ifu_bp_kill_next_f2 & ~ifc_fetch_req_f1 & ~ifc_fetch_req_f2;
   assign sel_btb_addr_bf  = ~miss_sel_flush & ifu_bp_kill_next_f2;
   assign sel_next_addr_bf = ~miss_sel_flush & ifc_fetch_req_f1;
   assign fetch_addr_bf[31:1] = ( ({31{miss_sel_flush}} &  exu_flush_path_final[31:1]) |  
                                   ({31{sel_miss_addr_bf}} & miss_addr[31:1]) |  
                                   ({31{sel_btb_addr_bf}} & {ifu_bp_btb_target_f2[31:1]})|  
                                   ({31{sel_last_addr_bf}} & {ifc_fetch_addr_f1[31:1]})|  
                                   ({31{sel_next_addr_bf}} & {fetch_addr_next[31:1]}));  
   assign {overflow_nc, fetch_addr_next[31:1]} = {({1'b0, ifc_fetch_addr_f1[31:4]} + 29'b1), 3'b0};
   assign ifc_fetch_req_bf = (fetch_ns | fetch_crit_word) ;
   assign fetch_bf_en = (fetch_ns | fetch_crit_word);
   assign miss_f2 = ifc_fetch_req_f2 & ~ic_hit_f2;
   assign mb_empty_mod = (ifu_ic_mb_empty | exu_flush_final) & ~dma_stall & ~miss_f2 & ~miss_a;
   assign goto_idle = exu_flush_final & dec_tlu_flush_noredir_wb;
   assign leave_idle = exu_flush_final & ~dec_tlu_flush_noredir_wb & idle;
   assign next_state[1] = (~state[1] & state[0] & ~reset_delayed & miss_f2 & ~goto_idle) |
                          (state[1] & ~reset_delayed & ~mb_empty_mod & ~goto_idle);
   assign next_state[0] = (~goto_idle & leave_idle) | (state[0] & ~goto_idle) |
                          (reset_delayed);
   assign flush_fb = exu_flush_final;
   assign fb_right = (~ifu_fb_consume1 & ~ifu_fb_consume2 & miss_f2) |   
                     ( ifu_fb_consume1 & ~ifu_fb_consume2 & ~ifc_fetch_req_f1 & ~miss_f2) |  
                      (ifu_fb_consume2 &  ifc_fetch_req_f1 & ~miss_f2);  
   assign fb_right2 = (ifu_fb_consume1 & ~ifu_fb_consume2 & miss_f2) |  
                      (ifu_fb_consume2 & ~ifc_fetch_req_f1);  
   assign fb_right3 = (ifu_fb_consume2 & miss_f2);  
   assign fb_left = ifc_fetch_req_f1 & ~(ifu_fb_consume1 | ifu_fb_consume2) & ~miss_f2;
   assign fb_write_ns[3:0] = ( ({4{(flush_fb & ~ifc_fetch_req_f1)}} & 4'b0001) |
                               ({4{(flush_fb & ifc_fetch_req_f1)}} & 4'b0010) |
                               ({4{~flush_fb & fb_right }} & {1'b0, fb_write_f1[3:1]}) |
                               ({4{~flush_fb & fb_right2}} & {2'b0, fb_write_f1[3:2]}) |
                               ({4{~flush_fb & fb_right3}} & {3'b0, fb_write_f1[3]}  ) |
                               ({4{~flush_fb & fb_left  }} & {fb_write_f1[2:0], 1'b0}) |
                               ({4{~flush_fb & ~fb_right & ~fb_right2 & ~fb_left & ~fb_right3}}  & fb_write_f1[3:0]));
   assign fb_full_f1_ns = fb_write_ns[3];
   assign idle = state[1:0] == IDLE;
   assign wfm = state[1:0] == WFM;
   assign fetch_ns = next_state[1:0] == FETCH;
   rvdff #(2) fsm_ff (.*, .clk(active_clk), .din({next_state[1:0]}), .dout({state[1:0]}));
   rvdff #(5) fbwrite_ff (.*, .clk(active_clk), .din({fb_full_f1_ns, fb_write_ns[3:0]}), .dout({fb_full_f1, fb_write_f1[3:0]}));
   assign ifu_pmu_fetch_stall = wfm |
                                (ifc_fetch_req_f1_raw &
                                ( (fb_full_f1 & ~(ifu_fb_consume2 | ifu_fb_consume1 | exu_flush_final)) |
                                  dma_stall));
   assign ifc_fetch_req_f1 = ( ifc_fetch_req_f1_raw &
                               ~ifu_bp_kill_next_f2 &
                               ~(fb_full_f1 & ~(ifu_fb_consume2 | ifu_fb_consume1 | exu_flush_final)) &
                               ~dma_stall &
                               ~ic_write_stall &
                               ~dec_tlu_flush_noredir_wb );
   assign fetch_req_f2_ns = ifc_fetch_req_f1 & ~miss_f2;
   rvdff #(2) req_ff (.*, .clk(active_clk), .din({ifc_fetch_req_bf, fetch_req_f2_ns}), .dout({ifc_fetch_req_f1_raw, ifc_fetch_req_f2_raw}));
   assign ifc_fetch_req_f2 = ifc_fetch_req_f2_raw & ~exu_flush_final;
   rvdffe #(31) faddrf1_ff  (.*, .en(fetch_bf_en), .din(fetch_addr_bf[31:1]), .dout(ifc_fetch_addr_f1_raw[31:1]));
   rvdff #(31) faddrf2_ff (.*,  .clk(ifc_f2_clk), .din(ifc_fetch_addr_f1[31:1]), .dout(ifc_fetch_addr_f2[31:1]));
   assign ifc_fetch_addr_f1[31:1] = ( ({31{exu_flush_final}} & exu_flush_path_final[31:1]) |
                                      ({31{~exu_flush_final}} & ifc_fetch_addr_f1_raw[31:1]));
   rvdff #(3) iccrit_ff (.*, .clk(active_clk), .din({ic_crit_wd_rdy_mod, fetch_crit_word,    fetch_crit_word_d1}),
                                              .dout({ic_crit_wd_rdy_d1,  fetch_crit_word_d1, fetch_crit_word_d2}));
   assign ifc_iccm_access_f1 = 1'b0 ;
   assign ifc_dma_access_ok  = 1'b0 ;
   assign ifc_region_acc_fault_f1  = 1'b0 ;
   assign cacheable_select[4:0]    =  {ifc_fetch_addr_f1[31:28] , 1'b0 } ;
   assign ifc_fetch_uncacheable_f1 =  ~dec_tlu_mrac_ff[cacheable_select]  ;  
endmodule  
module ifu_bp_ctl
   import swerv_types::*;
(
   input logic clk,
   input logic active_clk,
   input logic clk_override,
   input logic rst_l,
   input logic ic_hit_f2,       
   input logic [31:1] ifc_fetch_addr_f1,  
   input logic [31:1] ifc_fetch_addr_f2,  
   input logic ifc_fetch_req_f1,   
   input logic ifc_fetch_req_f2,   
   input br_tlu_pkt_t dec_tlu_br0_wb_pkt,  
   input br_tlu_pkt_t dec_tlu_br1_wb_pkt,  
   input logic dec_tlu_flush_lower_wb,  
   input logic dec_tlu_flush_leak_one_wb,  
   input logic dec_tlu_bpred_disable,  
   input logic        exu_i0_br_ret_e4,  
   input logic        exu_i1_br_ret_e4,  
   input logic        exu_i0_br_call_e4,  
   input logic        exu_i1_br_call_e4,  
   input predict_pkt_t  exu_mp_pkt,  
   input rets_pkt_t exu_rets_e1_pkt,  
   input rets_pkt_t exu_rets_e4_pkt,  
   input logic [4:0] exu_mp_eghr,  
   input logic exu_flush_final,  
   input logic exu_flush_upper_e2,  
   output logic ifu_bp_kill_next_f2,  
   output logic [31:1] ifu_bp_btb_target_f2,  
   output logic [7:1] ifu_bp_inst_mask_f2,  
   output logic [4:0] ifu_bp_fghr_f2,  
   output logic [7:0] ifu_bp_way_f2,  
   output logic [7:0] ifu_bp_ret_f2,  
   output logic [7:0] ifu_bp_hist1_f2,  
   output logic [7:0] ifu_bp_hist0_f2,  
   output logic [11:0] ifu_bp_poffset_f2,  
   output logic [7:0] ifu_bp_pc4_f2,  
   output logic [7:0] ifu_bp_valid_f2,  
   input  logic       scan_mode
   );
   localparam PC4=4;
   localparam BOFF=3;
   localparam CALL=2;
   localparam RET=1;
   localparam BV=0;
   localparam LRU_SIZE=4;
   localparam NUM_BHT_LOOP = (16 > 16 ) ? 16 : 16;
   localparam NUM_BHT_LOOP_INNER_HI =  (16 > 16 ) ?4+3 : 7;
   localparam NUM_BHT_LOOP_OUTER_LO =  (16 > 16 ) ?4+4 : 4;
   localparam BHT_NO_ADDR_MATCH  = ( 16 <= 16 );
   logic exu_mp_valid_write;
   logic exu_mp_ataken;
   logic exu_mp_valid;  
   logic exu_mp_boffset;  
   logic exu_mp_pc4;  
   logic exu_mp_call;  
   logic exu_mp_ret;  
   logic exu_mp_ja;  
   logic [1:0] exu_mp_hist;  
   logic [11:0] exu_mp_tgt;  
   logic [5:4] exu_mp_addr;  
   logic [1:0]                             exu_mp_bank;  
   logic [9-1:0]           exu_mp_btag;  
   logic [4:0]               exu_mp_fghr;  
   logic                                   dec_tlu_br0_v_wb;  
   logic [1:0]                             dec_tlu_br0_hist_wb;  
   logic [5:4] dec_tlu_br0_addr_wb;  
   logic [1:0]                             dec_tlu_br0_bank_wb;  
   logic                                   dec_tlu_br0_error_wb;  
   logic                                   dec_tlu_br0_start_error_wb;  
   logic [4:0]               dec_tlu_br0_fghr_wb;
   logic                                   dec_tlu_br1_v_wb;  
   logic [1:0]                             dec_tlu_br1_hist_wb;  
   logic [5:4] dec_tlu_br1_addr_wb;  
   logic [1:0]                             dec_tlu_br1_bank_wb;  
   logic                                   dec_tlu_br1_error_wb;  
   logic                                   dec_tlu_br1_start_error_wb;  
   logic [4:0]               dec_tlu_br1_fghr_wb;
   logic [3:0]        use_mp_way;
   logic [4-1:0][31:1] rets_out, rets_in, e1_rets_out, e1_rets_in, e4_rets_out, e4_rets_in;
   logic [4-1:0]       rsenable;
   logic [11:0]       btb_rd_tgt_f2;
   logic              btb_rd_pc4_f2, btb_rd_boffset_f2,  btb_rd_call_f2, btb_rd_ret_f2;
   logic [3:1]        bp_total_branch_offset_f2;
   logic [31:1]       bp_btb_target_adder_f2;
   logic [31:1]       bp_rs_call_target_f2;
   logic              rs_push, rs_pop, rs_hold;
   logic [5:4] btb_rd_addr_f1, btb_wr_addr, btb_rd_addr_f2;
   logic [9-1:0] btb_wr_tag, fetch_rd_tag_f1, fetch_rd_tag_f2;
   logic [16+9:0]        btb_wr_data;
   logic [3:0]         btb_wr_en_way0, btb_wr_en_way1;
   logic               dec_tlu_error_wb, dec_tlu_all_banks_error_wb, btb_valid, dec_tlu_br0_middle_wb, dec_tlu_br1_middle_wb;
   logic [5:4]        btb_error_addr_wb;
   logic [1:0]         dec_tlu_error_bank_wb;
   logic branch_error_collision_f1, fetch_mp_collision_f1, fetch_mp_collision_f2;
   logic [6:0] fgmask_f2;
   logic [3:0] branch_error_bank_conflict_f1, branch_error_bank_conflict_f2;
   logic [4:0] merged_ghr, fghr_ns, fghr;
   logic [3:0] num_valids;
   logic [LRU_SIZE-1:0] btb_lru_b0_f, btb_lru_b0_hold, btb_lru_b0_ns, btb_lru_b1_f, btb_lru_b1_hold, btb_lru_b1_ns,
                        btb_lru_b2_f, btb_lru_b2_hold, btb_lru_b2_ns, btb_lru_b3_f, btb_lru_b3_hold, btb_lru_b3_ns,
                        fetch_wrindex_dec, fetch_wrlru_b0, fetch_wrlru_b1, fetch_wrlru_b2, fetch_wrlru_b3,
                        mp_wrindex_dec, mp_wrlru_b0, mp_wrlru_b1, mp_wrlru_b2, mp_wrlru_b3;
   logic [3:0]          btb_lru_rd_f2, mp_bank_decoded, mp_bank_decoded_f, lru_update_valid_f2;
   logic [3:0] tag_match_way0_f2, tag_match_way1_f2;
   logic [7:0] way_raw, bht_dir_f2, btb_sel_f2, wayhit_f2;
   logic [7:0] btb_sel_mask_f2, bht_valid_f2, bht_force_taken_f2;
   logic leak_one_f1, leak_one_f2, ifc_fetch_req_f2_raw;
   logic [LRU_SIZE-1:0][16+9:0]  btb_bank0_rd_data_way0_out ;
   logic [LRU_SIZE-1:0][16+9:0]  btb_bank1_rd_data_way0_out ;
   logic [LRU_SIZE-1:0][16+9:0]  btb_bank2_rd_data_way0_out ;
   logic [LRU_SIZE-1:0][16+9:0]  btb_bank3_rd_data_way0_out ;
   logic [LRU_SIZE-1:0][16+9:0]  btb_bank0_rd_data_way1_out ;
   logic [LRU_SIZE-1:0][16+9:0]  btb_bank1_rd_data_way1_out ;
   logic [LRU_SIZE-1:0][16+9:0]  btb_bank2_rd_data_way1_out ;
   logic [LRU_SIZE-1:0][16+9:0]  btb_bank3_rd_data_way1_out ;
   logic                [16+9:0] btb_bank0_rd_data_way0_f2_in ;
   logic                [16+9:0] btb_bank1_rd_data_way0_f2_in ;
   logic                [16+9:0] btb_bank2_rd_data_way0_f2_in ;
   logic                [16+9:0] btb_bank3_rd_data_way0_f2_in ;
   logic                [16+9:0] btb_bank0_rd_data_way1_f2_in ;
   logic                [16+9:0] btb_bank1_rd_data_way1_f2_in ;
   logic                [16+9:0] btb_bank2_rd_data_way1_f2_in ;
   logic                [16+9:0] btb_bank3_rd_data_way1_f2_in ;
   logic                [16+9:0] btb_bank0_rd_data_way0_f2 ;
   logic                [16+9:0] btb_bank1_rd_data_way0_f2 ;
   logic                [16+9:0] btb_bank2_rd_data_way0_f2 ;
   logic                [16+9:0] btb_bank3_rd_data_way0_f2 ;
   logic                [16+9:0] btb_bank0_rd_data_way1_f2 ;
   logic                [16+9:0] btb_bank1_rd_data_way1_f2 ;
   logic                [16+9:0] btb_bank2_rd_data_way1_f2 ;
   logic                [16+9:0] btb_bank3_rd_data_way1_f2 ;
   logic                                         final_h;
   logic                                         btb_fg_crossing_f2;
   logic                                         rs_correct;
   logic                                         middle_of_bank;
   logic exu_mp_way, exu_mp_way_f, dec_tlu_br0_way_wb, dec_tlu_br1_way_wb, dec_tlu_way_wb, dec_tlu_way_wb_f;
   logic                [16+9:0] btb_bank0e_rd_data_f2 ;
   logic                [16+9:0] btb_bank1e_rd_data_f2 ;
   logic                [16+9:0] btb_bank2e_rd_data_f2 ;
   logic                [16+9:0] btb_bank3e_rd_data_f2 ;
   logic                [16+9:0] btb_bank0o_rd_data_f2 ;
   logic                [16+9:0] btb_bank1o_rd_data_f2 ;
   logic                [16+9:0] btb_bank2o_rd_data_f2 ;
   logic                [16+9:0] btb_bank3o_rd_data_f2 ;
   logic [7:0] tag_match_way0_expanded_f2, tag_match_way1_expanded_f2;
    logic [1:0]                                  bht_bank0_rd_data_f2 ;
    logic [1:0]                                  bht_bank1_rd_data_f2 ;
    logic [1:0]                                  bht_bank2_rd_data_f2 ;
    logic [1:0]                                  bht_bank3_rd_data_f2 ;
    logic [1:0]                                  bht_bank4_rd_data_f2 ;
    logic [1:0]                                  bht_bank5_rd_data_f2 ;
    logic [1:0]                                  bht_bank6_rd_data_f2 ;
    logic [1:0]                                  bht_bank7_rd_data_f2 ;
   assign exu_mp_valid = exu_mp_pkt.misp & ~leak_one_f2;  
   assign exu_mp_boffset = exu_mp_pkt.boffset;   
   assign exu_mp_pc4 = exu_mp_pkt.pc4;   
   assign exu_mp_call = exu_mp_pkt.pcall;   
   assign exu_mp_ret = exu_mp_pkt.pret;   
   assign exu_mp_ja = exu_mp_pkt.pja;   
   assign exu_mp_way = exu_mp_pkt.way;   
   assign exu_mp_hist[1:0] = exu_mp_pkt.hist[1:0];   
   assign exu_mp_tgt[11:0]  = exu_mp_pkt.toffset[11:0] ;   
   assign exu_mp_addr[5:4]  = exu_mp_pkt.index[5:4] ;   
   assign exu_mp_bank[1:0]  = exu_mp_pkt.bank[1:0] ;   
   assign exu_mp_btag[9-1:0]  = exu_mp_pkt.btag[9-1:0] ;   
   assign exu_mp_fghr[4:0]  = exu_mp_pkt.fghr[4:0] ;   
   assign exu_mp_ataken = exu_mp_pkt.ataken;
   assign dec_tlu_br0_v_wb = dec_tlu_br0_wb_pkt.valid;
   assign dec_tlu_br0_hist_wb[1:0]  = dec_tlu_br0_wb_pkt.hist[1:0];
   assign dec_tlu_br0_addr_wb[5:4] = dec_tlu_br0_wb_pkt.index[5:4];
   assign dec_tlu_br0_bank_wb[1:0]  = dec_tlu_br0_wb_pkt.bank[1:0];
   assign dec_tlu_br0_error_wb = dec_tlu_br0_wb_pkt.br_error;
   assign dec_tlu_br0_middle_wb = dec_tlu_br0_wb_pkt.middle;
   assign dec_tlu_br0_way_wb = dec_tlu_br0_wb_pkt.way;
   assign dec_tlu_br0_start_error_wb = dec_tlu_br0_wb_pkt.br_start_error;
   assign dec_tlu_br0_fghr_wb[4:0] = dec_tlu_br0_wb_pkt.fghr[4:0];
   assign dec_tlu_br1_v_wb = dec_tlu_br1_wb_pkt.valid;
   assign dec_tlu_br1_hist_wb[1:0]  = dec_tlu_br1_wb_pkt.hist[1:0];
   assign dec_tlu_br1_addr_wb[5:4] = dec_tlu_br1_wb_pkt.index[5:4];
   assign dec_tlu_br1_bank_wb[1:0]  = dec_tlu_br1_wb_pkt.bank[1:0];
   assign dec_tlu_br1_middle_wb = dec_tlu_br1_wb_pkt.middle;
   assign dec_tlu_br1_error_wb = dec_tlu_br1_wb_pkt.br_error;
   assign dec_tlu_br1_way_wb = dec_tlu_br1_wb_pkt.way;
   assign dec_tlu_br1_start_error_wb = dec_tlu_br1_wb_pkt.br_start_error;
   assign  dec_tlu_br1_fghr_wb[4:0] = dec_tlu_br1_wb_pkt.fghr[4:0];
   rvbtb_addr_hash f1hash(.pc(ifc_fetch_addr_f1[31:1]), .hash(btb_rd_addr_f1[5:4]));
   rvbtb_addr_hash f2hash(.pc(ifc_fetch_addr_f2[31:1]), .hash(btb_rd_addr_f2[5:4]));
assign btb_sel_f2[7] = (~ifc_fetch_addr_f2[3] & ~ifc_fetch_addr_f2[2]
     & ~ifc_fetch_addr_f2[1] & ~bht_dir_f2[6] & ~bht_dir_f2[5] & ~bht_dir_f2[4]
     & ~bht_dir_f2[3] & ~bht_dir_f2[2] & ~bht_dir_f2[1] & ~bht_dir_f2[0]) | (
    ~ifc_fetch_addr_f2[3] & ~ifc_fetch_addr_f2[2] & ifc_fetch_addr_f2[1]
     & ~bht_dir_f2[6] & ~bht_dir_f2[5] & ~bht_dir_f2[4] & ~bht_dir_f2[3]
     & ~bht_dir_f2[2] & ~bht_dir_f2[1]) | (~ifc_fetch_addr_f2[3]
     & ifc_fetch_addr_f2[2] & ~ifc_fetch_addr_f2[1] & ~bht_dir_f2[6]
     & ~bht_dir_f2[5] & ~bht_dir_f2[4] & ~bht_dir_f2[3] & ~bht_dir_f2[2]) | (
    ~ifc_fetch_addr_f2[3] & ifc_fetch_addr_f2[2] & ifc_fetch_addr_f2[1]
     & ~bht_dir_f2[6] & ~bht_dir_f2[5] & ~bht_dir_f2[4] & ~bht_dir_f2[3]) | (
    ifc_fetch_addr_f2[3] & ~ifc_fetch_addr_f2[2] & ~ifc_fetch_addr_f2[1]
     & ~bht_dir_f2[6] & ~bht_dir_f2[5] & ~bht_dir_f2[4]) | (
    ifc_fetch_addr_f2[3] & ~ifc_fetch_addr_f2[2] & ifc_fetch_addr_f2[1]
     & ~bht_dir_f2[6] & ~bht_dir_f2[5]) | (ifc_fetch_addr_f2[3]
     & ifc_fetch_addr_f2[2] & ~ifc_fetch_addr_f2[1] & ~bht_dir_f2[6]) | (
    ifc_fetch_addr_f2[3] & ifc_fetch_addr_f2[2] & ifc_fetch_addr_f2[1]);
assign btb_sel_f2[6] = (~ifc_fetch_addr_f2[3] & ~ifc_fetch_addr_f2[2]
     & ifc_fetch_addr_f2[1] & bht_dir_f2[6] & ~bht_dir_f2[5] & ~bht_dir_f2[4]
     & ~bht_dir_f2[3] & ~bht_dir_f2[2] & ~bht_dir_f2[1]) | (
    ~ifc_fetch_addr_f2[3] & ~ifc_fetch_addr_f2[2] & ~ifc_fetch_addr_f2[1]
     & bht_dir_f2[6] & ~bht_dir_f2[5] & ~bht_dir_f2[4] & ~bht_dir_f2[3]
     & ~bht_dir_f2[2] & ~bht_dir_f2[1] & ~bht_dir_f2[0]) | (
    ~ifc_fetch_addr_f2[3] & ifc_fetch_addr_f2[2] & ~ifc_fetch_addr_f2[1]
     & bht_dir_f2[6] & ~bht_dir_f2[5] & ~bht_dir_f2[4] & ~bht_dir_f2[3]
     & ~bht_dir_f2[2]) | (~ifc_fetch_addr_f2[3] & ifc_fetch_addr_f2[2]
     & ifc_fetch_addr_f2[1] & bht_dir_f2[6] & ~bht_dir_f2[5] & ~bht_dir_f2[4]
     & ~bht_dir_f2[3]) | (ifc_fetch_addr_f2[3] & ~ifc_fetch_addr_f2[2]
     & ~ifc_fetch_addr_f2[1] & bht_dir_f2[6] & ~bht_dir_f2[5] & ~bht_dir_f2[4]) | (
    ifc_fetch_addr_f2[3] & ~ifc_fetch_addr_f2[2] & ifc_fetch_addr_f2[1]
     & bht_dir_f2[6] & ~bht_dir_f2[5]) | (ifc_fetch_addr_f2[3]
     & ifc_fetch_addr_f2[2] & ~ifc_fetch_addr_f2[1] & bht_dir_f2[6]);
assign btb_sel_f2[5] = (~ifc_fetch_addr_f2[3] & ~ifc_fetch_addr_f2[2]
     & ~ifc_fetch_addr_f2[1] & bht_dir_f2[5] & ~bht_dir_f2[4] & ~bht_dir_f2[3]
     & ~bht_dir_f2[2] & ~bht_dir_f2[1] & ~bht_dir_f2[0]) | (
    ~ifc_fetch_addr_f2[3] & ~ifc_fetch_addr_f2[2] & ifc_fetch_addr_f2[1]
     & bht_dir_f2[5] & ~bht_dir_f2[4] & ~bht_dir_f2[3] & ~bht_dir_f2[2]
     & ~bht_dir_f2[1]) | (~ifc_fetch_addr_f2[3] & ifc_fetch_addr_f2[2]
     & ~ifc_fetch_addr_f2[1] & bht_dir_f2[5] & ~bht_dir_f2[4] & ~bht_dir_f2[3]
     & ~bht_dir_f2[2]) | (~ifc_fetch_addr_f2[3] & ifc_fetch_addr_f2[2]
     & ifc_fetch_addr_f2[1] & bht_dir_f2[5] & ~bht_dir_f2[4] & ~bht_dir_f2[3]) | (
    ifc_fetch_addr_f2[3] & ~ifc_fetch_addr_f2[2] & ~ifc_fetch_addr_f2[1]
     & bht_dir_f2[5] & ~bht_dir_f2[4]) | (ifc_fetch_addr_f2[3]
     & ~ifc_fetch_addr_f2[2] & ifc_fetch_addr_f2[1] & bht_dir_f2[5]);
assign btb_sel_f2[4] = (~ifc_fetch_addr_f2[3] & ~ifc_fetch_addr_f2[2]
     & ~ifc_fetch_addr_f2[1] & bht_dir_f2[4] & ~bht_dir_f2[3] & ~bht_dir_f2[2]
     & ~bht_dir_f2[1] & ~bht_dir_f2[0]) | (~ifc_fetch_addr_f2[3]
     & ~ifc_fetch_addr_f2[2] & ifc_fetch_addr_f2[1] & bht_dir_f2[4]
     & ~bht_dir_f2[3] & ~bht_dir_f2[2] & ~bht_dir_f2[1]) | (
    ~ifc_fetch_addr_f2[3] & ifc_fetch_addr_f2[2] & ~ifc_fetch_addr_f2[1]
     & bht_dir_f2[4] & ~bht_dir_f2[3] & ~bht_dir_f2[2]) | (
    ~ifc_fetch_addr_f2[3] & ifc_fetch_addr_f2[2] & ifc_fetch_addr_f2[1]
     & bht_dir_f2[4] & ~bht_dir_f2[3]) | (ifc_fetch_addr_f2[3]
     & ~ifc_fetch_addr_f2[2] & ~ifc_fetch_addr_f2[1] & bht_dir_f2[4]);
assign btb_sel_f2[3] = (~ifc_fetch_addr_f2[3] & ~ifc_fetch_addr_f2[2]
     & ~ifc_fetch_addr_f2[1] & bht_dir_f2[3] & ~bht_dir_f2[2] & ~bht_dir_f2[1]
     & ~bht_dir_f2[0]) | (~ifc_fetch_addr_f2[3] & ~ifc_fetch_addr_f2[2]
     & ifc_fetch_addr_f2[1] & bht_dir_f2[3] & ~bht_dir_f2[2] & ~bht_dir_f2[1]) | (
    ~ifc_fetch_addr_f2[3] & ifc_fetch_addr_f2[2] & ~ifc_fetch_addr_f2[1]
     & bht_dir_f2[3] & ~bht_dir_f2[2]) | (~ifc_fetch_addr_f2[3]
     & ifc_fetch_addr_f2[2] & ifc_fetch_addr_f2[1] & bht_dir_f2[3]);
assign btb_sel_f2[2] = (~ifc_fetch_addr_f2[3] & ~ifc_fetch_addr_f2[2]
     & ~ifc_fetch_addr_f2[1] & bht_dir_f2[2] & ~bht_dir_f2[1] & ~bht_dir_f2[0]) | (
    ~ifc_fetch_addr_f2[3] & ~ifc_fetch_addr_f2[2] & ifc_fetch_addr_f2[1]
     & bht_dir_f2[2] & ~bht_dir_f2[1]) | (~ifc_fetch_addr_f2[3]
     & ifc_fetch_addr_f2[2] & ~ifc_fetch_addr_f2[1] & bht_dir_f2[2]);
assign btb_sel_f2[1] = (~ifc_fetch_addr_f2[3] & ~ifc_fetch_addr_f2[2]
     & ~ifc_fetch_addr_f2[1] & bht_dir_f2[1] & ~bht_dir_f2[0]) | (
    ~ifc_fetch_addr_f2[3] & ~ifc_fetch_addr_f2[2] & ifc_fetch_addr_f2[1]
     & bht_dir_f2[1]);
assign btb_sel_f2[0] = (~ifc_fetch_addr_f2[3] & ~ifc_fetch_addr_f2[2]
     & ~ifc_fetch_addr_f2[1] & bht_dir_f2[0]);
logic [7:0] btb_vmask_raw_f2;
assign btb_vmask_raw_f2[7] = (~ifc_fetch_addr_f2[3] & ~ifc_fetch_addr_f2[2]
     & ~ifc_fetch_addr_f2[1] & ~bht_dir_f2[6] & ~bht_dir_f2[5] & ~bht_dir_f2[4]
     & ~bht_dir_f2[3] & ~bht_dir_f2[2] & ~bht_dir_f2[1] & ~bht_dir_f2[0]);
assign btb_vmask_raw_f2[6] = (~ifc_fetch_addr_f2[3] & ~ifc_fetch_addr_f2[2]
     & ifc_fetch_addr_f2[1] & ~bht_dir_f2[6] & ~bht_dir_f2[5] & ~bht_dir_f2[4]
     & ~bht_dir_f2[3] & ~bht_dir_f2[2] & ~bht_dir_f2[1]) | (
    ~ifc_fetch_addr_f2[3] & ~ifc_fetch_addr_f2[2] & ~ifc_fetch_addr_f2[1]
     & bht_dir_f2[6] & ~bht_dir_f2[5] & ~bht_dir_f2[4] & ~bht_dir_f2[3]
     & ~bht_dir_f2[2] & ~bht_dir_f2[1] & ~bht_dir_f2[0]);
assign btb_vmask_raw_f2[5] = (~ifc_fetch_addr_f2[3] & ~ifc_fetch_addr_f2[2]
     & ifc_fetch_addr_f2[1] & bht_dir_f2[6] & ~bht_dir_f2[5] & ~bht_dir_f2[4]
     & ~bht_dir_f2[3] & ~bht_dir_f2[2] & ~bht_dir_f2[1]) | (
    ~ifc_fetch_addr_f2[3] & ~ifc_fetch_addr_f2[2] & ifc_fetch_addr_f2[1]
     & ~bht_dir_f2[6] & ~bht_dir_f2[5] & ~bht_dir_f2[4] & ~bht_dir_f2[3]
     & ~bht_dir_f2[2] & ~bht_dir_f2[1]) | (~ifc_fetch_addr_f2[3]
     & ifc_fetch_addr_f2[2] & ~ifc_fetch_addr_f2[1] & ~bht_dir_f2[6]
     & ~bht_dir_f2[5] & ~bht_dir_f2[4] & ~bht_dir_f2[3] & ~bht_dir_f2[2]) | (
    ~ifc_fetch_addr_f2[3] & ~ifc_fetch_addr_f2[2] & ~ifc_fetch_addr_f2[1]
     & bht_dir_f2[5] & ~bht_dir_f2[4] & ~bht_dir_f2[3] & ~bht_dir_f2[2]
     & ~bht_dir_f2[1] & ~bht_dir_f2[0]);
assign btb_vmask_raw_f2[4] = (~ifc_fetch_addr_f2[3] & ifc_fetch_addr_f2[2]
     & ifc_fetch_addr_f2[1] & ~bht_dir_f2[6] & ~bht_dir_f2[5] & ~bht_dir_f2[4]
     & ~bht_dir_f2[3]) | (~ifc_fetch_addr_f2[3] & ~ifc_fetch_addr_f2[2]
     & ~ifc_fetch_addr_f2[1] & bht_dir_f2[4] & ~bht_dir_f2[3] & ~bht_dir_f2[2]
     & ~bht_dir_f2[1] & ~bht_dir_f2[0]) | (~ifc_fetch_addr_f2[3]
     & ifc_fetch_addr_f2[2] & ~ifc_fetch_addr_f2[1] & bht_dir_f2[6]
     & ~bht_dir_f2[5] & ~bht_dir_f2[4] & ~bht_dir_f2[3] & ~bht_dir_f2[2]) | (
    ~ifc_fetch_addr_f2[3] & ~ifc_fetch_addr_f2[2] & ifc_fetch_addr_f2[1]
     & bht_dir_f2[5] & ~bht_dir_f2[4] & ~bht_dir_f2[3] & ~bht_dir_f2[2]
     & ~bht_dir_f2[1]);
assign btb_vmask_raw_f2[3] = (ifc_fetch_addr_f2[3] & ~ifc_fetch_addr_f2[2]
     & ~ifc_fetch_addr_f2[1] & ~bht_dir_f2[6] & ~bht_dir_f2[5] & ~bht_dir_f2[4]) | (
    ~ifc_fetch_addr_f2[3] & ~ifc_fetch_addr_f2[2] & ~ifc_fetch_addr_f2[1]
     & bht_dir_f2[3] & ~bht_dir_f2[2] & ~bht_dir_f2[1] & ~bht_dir_f2[0]) | (
    ~ifc_fetch_addr_f2[3] & ifc_fetch_addr_f2[2] & ifc_fetch_addr_f2[1]
     & bht_dir_f2[6] & ~bht_dir_f2[5] & ~bht_dir_f2[4] & ~bht_dir_f2[3]) | (
    ~ifc_fetch_addr_f2[3] & ifc_fetch_addr_f2[2] & ~ifc_fetch_addr_f2[1]
     & bht_dir_f2[5] & ~bht_dir_f2[4] & ~bht_dir_f2[3] & ~bht_dir_f2[2]) | (
    ~ifc_fetch_addr_f2[3] & ~ifc_fetch_addr_f2[2] & ifc_fetch_addr_f2[1]
     & bht_dir_f2[4] & ~bht_dir_f2[3] & ~bht_dir_f2[2] & ~bht_dir_f2[1]);
assign btb_vmask_raw_f2[2] = (ifc_fetch_addr_f2[3] & ~ifc_fetch_addr_f2[2]
     & ifc_fetch_addr_f2[1] & ~bht_dir_f2[6] & ~bht_dir_f2[5]) | (
    ifc_fetch_addr_f2[3] & ~ifc_fetch_addr_f2[2] & ~ifc_fetch_addr_f2[1]
     & bht_dir_f2[6] & ~bht_dir_f2[5] & ~bht_dir_f2[4]) | (
    ~ifc_fetch_addr_f2[3] & ~ifc_fetch_addr_f2[2] & ~ifc_fetch_addr_f2[1]
     & bht_dir_f2[2] & ~bht_dir_f2[1] & ~bht_dir_f2[0]) | (
    ~ifc_fetch_addr_f2[3] & ifc_fetch_addr_f2[2] & ifc_fetch_addr_f2[1]
     & bht_dir_f2[5] & ~bht_dir_f2[4] & ~bht_dir_f2[3]) | (
    ~ifc_fetch_addr_f2[3] & ~ifc_fetch_addr_f2[2] & ifc_fetch_addr_f2[1]
     & bht_dir_f2[3] & ~bht_dir_f2[2] & ~bht_dir_f2[1]) | (
    ~ifc_fetch_addr_f2[3] & ifc_fetch_addr_f2[2] & ~ifc_fetch_addr_f2[1]
     & bht_dir_f2[4] & ~bht_dir_f2[3] & ~bht_dir_f2[2]);
assign btb_vmask_raw_f2[1] = (ifc_fetch_addr_f2[3] & ifc_fetch_addr_f2[2]
     & ~ifc_fetch_addr_f2[1] & ~bht_dir_f2[6]) | (ifc_fetch_addr_f2[3]
     & ~ifc_fetch_addr_f2[2] & ifc_fetch_addr_f2[1] & bht_dir_f2[6]
     & ~bht_dir_f2[5]) | (ifc_fetch_addr_f2[3] & ~ifc_fetch_addr_f2[2]
     & ~ifc_fetch_addr_f2[1] & bht_dir_f2[5] & ~bht_dir_f2[4]) | (
    ~ifc_fetch_addr_f2[3] & ~ifc_fetch_addr_f2[2] & ~ifc_fetch_addr_f2[1]
     & bht_dir_f2[1] & ~bht_dir_f2[0]) | (~ifc_fetch_addr_f2[3]
     & ifc_fetch_addr_f2[2] & ifc_fetch_addr_f2[1] & bht_dir_f2[4]
     & ~bht_dir_f2[3]) | (~ifc_fetch_addr_f2[3] & ifc_fetch_addr_f2[2]
     & ~ifc_fetch_addr_f2[1] & bht_dir_f2[3] & ~bht_dir_f2[2]) | (
    ~ifc_fetch_addr_f2[3] & ~ifc_fetch_addr_f2[2] & ifc_fetch_addr_f2[1]
     & bht_dir_f2[2] & ~bht_dir_f2[1]);
   logic[7:1] btb_vmask_f2;
   assign btb_vmask_f2[7:1] = {btb_vmask_raw_f2[7],
                              |btb_vmask_raw_f2[7:6],
                              |btb_vmask_raw_f2[7:5],
                              |btb_vmask_raw_f2[7:4],
                              |btb_vmask_raw_f2[7:3],
                              |btb_vmask_raw_f2[7:2],
                              |btb_vmask_raw_f2[7:1]};
   assign branch_error_collision_f1 = dec_tlu_error_wb & (btb_error_addr_wb[5:4] == btb_rd_addr_f1[5:4]);
   assign branch_error_bank_conflict_f1[3:0] = {4{branch_error_collision_f1}} & (decode2_4(dec_tlu_error_bank_wb[1:0]) | {4{dec_tlu_all_banks_error_wb}});
   assign fetch_mp_collision_f1 = ( (exu_mp_btag[9-1:0] == fetch_rd_tag_f1[9-1:0]) &
                                    exu_mp_valid & ifc_fetch_req_f1 &
                                    (exu_mp_addr[5:4] == btb_rd_addr_f1[5:4])
                                    );
   assign leak_one_f1 = (dec_tlu_flush_leak_one_wb & dec_tlu_flush_lower_wb) | (leak_one_f2 & ~dec_tlu_flush_lower_wb);
   rvdff #(13) coll_ff (.*, .clk(active_clk),
                         .din({branch_error_bank_conflict_f1[3:0], fetch_mp_collision_f1, mp_bank_decoded[3:0], exu_mp_way, dec_tlu_way_wb, leak_one_f1, ifc_fetch_req_f1}),
                        .dout({branch_error_bank_conflict_f2[3:0], fetch_mp_collision_f2, mp_bank_decoded_f[3:0], exu_mp_way_f, dec_tlu_way_wb_f, leak_one_f2, ifc_fetch_req_f2_raw}));
   assign tag_match_way0_f2[3:0] = {btb_bank3_rd_data_way0_f2[BV] & (btb_bank3_rd_data_way0_f2[16+9:17] == fetch_rd_tag_f2[9-1:0]),
                                    btb_bank2_rd_data_way0_f2[BV] & (btb_bank2_rd_data_way0_f2[16+9:17] == fetch_rd_tag_f2[9-1:0]),
                                    btb_bank1_rd_data_way0_f2[BV] & (btb_bank1_rd_data_way0_f2[16+9:17] == fetch_rd_tag_f2[9-1:0]),
                                    btb_bank0_rd_data_way0_f2[BV] & (btb_bank0_rd_data_way0_f2[16+9:17] == fetch_rd_tag_f2[9-1:0])} &
                                   ~({4{~dec_tlu_way_wb_f}} & branch_error_bank_conflict_f2[3:0]) & {4{ifc_fetch_req_f2_raw & ~leak_one_f2}};
   assign tag_match_way1_f2[3:0] = {btb_bank3_rd_data_way1_f2[BV] & (btb_bank3_rd_data_way1_f2[16+9:17] == fetch_rd_tag_f2[9-1:0]),
                                    btb_bank2_rd_data_way1_f2[BV] & (btb_bank2_rd_data_way1_f2[16+9:17] == fetch_rd_tag_f2[9-1:0]),
                                    btb_bank1_rd_data_way1_f2[BV] & (btb_bank1_rd_data_way1_f2[16+9:17] == fetch_rd_tag_f2[9-1:0]),
                                    btb_bank0_rd_data_way1_f2[BV] & (btb_bank0_rd_data_way1_f2[16+9:17] == fetch_rd_tag_f2[9-1:0])} &
                                   ~({4{dec_tlu_way_wb_f}} & branch_error_bank_conflict_f2[3:0]) & {4{ifc_fetch_req_f2_raw & ~leak_one_f2}};
   assign tag_match_way0_expanded_f2[7:0] = {tag_match_way0_f2[3] &  (btb_bank3_rd_data_way0_f2[BOFF] ^ btb_bank3_rd_data_way0_f2[PC4]),
                                             tag_match_way0_f2[3] & ~(btb_bank3_rd_data_way0_f2[BOFF] ^ btb_bank3_rd_data_way0_f2[PC4]),
                                             tag_match_way0_f2[2] &  (btb_bank2_rd_data_way0_f2[BOFF] ^ btb_bank2_rd_data_way0_f2[PC4]),
                                             tag_match_way0_f2[2] & ~(btb_bank2_rd_data_way0_f2[BOFF] ^ btb_bank2_rd_data_way0_f2[PC4]),
                                             tag_match_way0_f2[1] &  (btb_bank1_rd_data_way0_f2[BOFF] ^ btb_bank1_rd_data_way0_f2[PC4]),
                                             tag_match_way0_f2[1] & ~(btb_bank1_rd_data_way0_f2[BOFF] ^ btb_bank1_rd_data_way0_f2[PC4]),
                                             tag_match_way0_f2[0] &  (btb_bank0_rd_data_way0_f2[BOFF] ^ btb_bank0_rd_data_way0_f2[PC4]),
                                             tag_match_way0_f2[0] & ~(btb_bank0_rd_data_way0_f2[BOFF] ^ btb_bank0_rd_data_way0_f2[PC4])};
   assign tag_match_way1_expanded_f2[7:0] = {tag_match_way1_f2[3] &  (btb_bank3_rd_data_way1_f2[BOFF] ^ btb_bank3_rd_data_way1_f2[PC4]),
                                             tag_match_way1_f2[3] & ~(btb_bank3_rd_data_way1_f2[BOFF] ^ btb_bank3_rd_data_way1_f2[PC4]),
                                             tag_match_way1_f2[2] &  (btb_bank2_rd_data_way1_f2[BOFF] ^ btb_bank2_rd_data_way1_f2[PC4]),
                                             tag_match_way1_f2[2] & ~(btb_bank2_rd_data_way1_f2[BOFF] ^ btb_bank2_rd_data_way1_f2[PC4]),
                                             tag_match_way1_f2[1] &  (btb_bank1_rd_data_way1_f2[BOFF] ^ btb_bank1_rd_data_way1_f2[PC4]),
                                             tag_match_way1_f2[1] & ~(btb_bank1_rd_data_way1_f2[BOFF] ^ btb_bank1_rd_data_way1_f2[PC4]),
                                             tag_match_way1_f2[0] &  (btb_bank0_rd_data_way1_f2[BOFF] ^ btb_bank0_rd_data_way1_f2[PC4]),
                                             tag_match_way1_f2[0] & ~(btb_bank0_rd_data_way1_f2[BOFF] ^ btb_bank0_rd_data_way1_f2[PC4])};
   assign wayhit_f2[7:0] = tag_match_way0_expanded_f2[7:0] | tag_match_way1_expanded_f2[7:0];
   assign btb_bank3o_rd_data_f2[16+9:0] = ( ({17+9{tag_match_way0_expanded_f2[7]}} & btb_bank3_rd_data_way0_f2[16+9:0]) |
                                                            ({17+9{tag_match_way1_expanded_f2[7]}} & btb_bank3_rd_data_way1_f2[16+9:0]) );
   assign btb_bank3e_rd_data_f2[16+9:0] = ( ({17+9{tag_match_way0_expanded_f2[6]}} & btb_bank3_rd_data_way0_f2[16+9:0]) |
                                                            ({17+9{tag_match_way1_expanded_f2[6]}} & btb_bank3_rd_data_way1_f2[16+9:0]) );
   assign btb_bank2o_rd_data_f2[16+9:0] = ( ({17+9{tag_match_way0_expanded_f2[5]}} & btb_bank2_rd_data_way0_f2[16+9:0]) |
                                                            ({17+9{tag_match_way1_expanded_f2[5]}} & btb_bank2_rd_data_way1_f2[16+9:0]) );
   assign btb_bank2e_rd_data_f2[16+9:0] = ( ({17+9{tag_match_way0_expanded_f2[4]}} & btb_bank2_rd_data_way0_f2[16+9:0]) |
                                                            ({17+9{tag_match_way1_expanded_f2[4]}} & btb_bank2_rd_data_way1_f2[16+9:0]) );
   assign btb_bank1o_rd_data_f2[16+9:0] = ( ({17+9{tag_match_way0_expanded_f2[3]}} & btb_bank1_rd_data_way0_f2[16+9:0]) |
                                                            ({17+9{tag_match_way1_expanded_f2[3]}} & btb_bank1_rd_data_way1_f2[16+9:0]) );
   assign btb_bank1e_rd_data_f2[16+9:0] = ( ({17+9{tag_match_way0_expanded_f2[2]}} & btb_bank1_rd_data_way0_f2[16+9:0]) |
                                                            ({17+9{tag_match_way1_expanded_f2[2]}} & btb_bank1_rd_data_way1_f2[16+9:0]) );
   assign btb_bank0o_rd_data_f2[16+9:0] = ( ({17+9{tag_match_way0_expanded_f2[1]}} & btb_bank0_rd_data_way0_f2[16+9:0]) |
                                                            ({17+9{tag_match_way1_expanded_f2[1]}} & btb_bank0_rd_data_way1_f2[16+9:0]) );
   assign btb_bank0e_rd_data_f2[16+9:0] = ( ({17+9{tag_match_way0_expanded_f2[0]}} & btb_bank0_rd_data_way0_f2[16+9:0]) |
                                                            ({17+9{tag_match_way1_expanded_f2[0]}} & btb_bank0_rd_data_way1_f2[16+9:0]) );
   assign mp_bank_decoded[3:0] = decode2_4(exu_mp_bank[1:0]);
   assign mp_wrindex_dec[LRU_SIZE-1:0] = {{LRU_SIZE-1{1'b0}},1'b1} <<  exu_mp_addr[5:4];
   assign fetch_wrindex_dec[LRU_SIZE-1:0] = {{LRU_SIZE-1{1'b0}},1'b1} <<  btb_rd_addr_f2[5:4];
   assign mp_wrlru_b0[LRU_SIZE-1:0] = mp_wrindex_dec[LRU_SIZE-1:0] & {LRU_SIZE{mp_bank_decoded[0] & exu_mp_valid}};
   assign mp_wrlru_b1[LRU_SIZE-1:0] = mp_wrindex_dec[LRU_SIZE-1:0] & {LRU_SIZE{mp_bank_decoded[1] & exu_mp_valid}};
   assign mp_wrlru_b2[LRU_SIZE-1:0] = mp_wrindex_dec[LRU_SIZE-1:0] & {LRU_SIZE{mp_bank_decoded[2] & exu_mp_valid}};
   assign mp_wrlru_b3[LRU_SIZE-1:0] = mp_wrindex_dec[LRU_SIZE-1:0] & {LRU_SIZE{mp_bank_decoded[3] & exu_mp_valid}};
   genvar     j, i;
   assign lru_update_valid_f2[3:0] = {((bht_valid_f2[6] & btb_sel_mask_f2[6]) | (bht_valid_f2[7] & btb_sel_mask_f2[7])) & ifc_fetch_req_f2 & ~leak_one_f2,
                                      ((bht_valid_f2[4] & btb_sel_mask_f2[4]) | (bht_valid_f2[5] & btb_sel_mask_f2[5])) & ifc_fetch_req_f2 & ~leak_one_f2,
                                      ((bht_valid_f2[2] & btb_sel_mask_f2[2]) | (bht_valid_f2[3] & btb_sel_mask_f2[3])) & ifc_fetch_req_f2 & ~leak_one_f2,
                                      ((bht_valid_f2[0] & btb_sel_mask_f2[0]) | (bht_valid_f2[1] & btb_sel_mask_f2[1])) & ifc_fetch_req_f2 & ~leak_one_f2};
   assign fetch_wrlru_b0[LRU_SIZE-1:0] = fetch_wrindex_dec[LRU_SIZE-1:0] &
                                         {LRU_SIZE{lru_update_valid_f2[0]}};
   assign fetch_wrlru_b1[LRU_SIZE-1:0] = fetch_wrindex_dec[LRU_SIZE-1:0] &
                                         {LRU_SIZE{lru_update_valid_f2[1]}};
   assign fetch_wrlru_b2[LRU_SIZE-1:0] = fetch_wrindex_dec[LRU_SIZE-1:0] &
                                         {LRU_SIZE{lru_update_valid_f2[2]}};
   assign fetch_wrlru_b3[LRU_SIZE-1:0] = fetch_wrindex_dec[LRU_SIZE-1:0] &
                                         {LRU_SIZE{lru_update_valid_f2[3]}};
   assign btb_lru_b0_hold[LRU_SIZE-1:0] = ~mp_wrlru_b0[LRU_SIZE-1:0] & ~fetch_wrlru_b0[LRU_SIZE-1:0];
   assign btb_lru_b1_hold[LRU_SIZE-1:0] = ~mp_wrlru_b1[LRU_SIZE-1:0] & ~fetch_wrlru_b1[LRU_SIZE-1:0];
   assign btb_lru_b2_hold[LRU_SIZE-1:0] = ~mp_wrlru_b2[LRU_SIZE-1:0] & ~fetch_wrlru_b2[LRU_SIZE-1:0];
   assign btb_lru_b3_hold[LRU_SIZE-1:0] = ~mp_wrlru_b3[LRU_SIZE-1:0] & ~fetch_wrlru_b3[LRU_SIZE-1:0];
   assign use_mp_way[3:0] = {4{fetch_mp_collision_f2}} & mp_bank_decoded_f[3:0];
   assign btb_lru_b0_ns[LRU_SIZE-1:0] = ( (btb_lru_b0_hold[LRU_SIZE-1:0] & btb_lru_b0_f[LRU_SIZE-1:0]) |
                                          (mp_wrlru_b0[LRU_SIZE-1:0] & {LRU_SIZE{~exu_mp_way}}) |
                                          (fetch_wrlru_b0[LRU_SIZE-1:0] & {LRU_SIZE{tag_match_way0_f2[0]}}) );
   assign btb_lru_b1_ns[LRU_SIZE-1:0] = ( (btb_lru_b1_hold[LRU_SIZE-1:0] & btb_lru_b1_f[LRU_SIZE-1:0]) |
                                          (mp_wrlru_b1[LRU_SIZE-1:0] & {LRU_SIZE{~exu_mp_way}}) |
                                          (fetch_wrlru_b1[LRU_SIZE-1:0] & {LRU_SIZE{tag_match_way0_f2[1]}}) );
   assign btb_lru_b2_ns[LRU_SIZE-1:0] = ( (btb_lru_b2_hold[LRU_SIZE-1:0] & btb_lru_b2_f[LRU_SIZE-1:0]) |
                                          (mp_wrlru_b2[LRU_SIZE-1:0] & {LRU_SIZE{~exu_mp_way}}) |
                                          (fetch_wrlru_b2[LRU_SIZE-1:0] & {LRU_SIZE{tag_match_way0_f2[2]}}) );
   assign btb_lru_b3_ns[LRU_SIZE-1:0] = ( (btb_lru_b3_hold[LRU_SIZE-1:0] & btb_lru_b3_f[LRU_SIZE-1:0]) |
                                          (mp_wrlru_b3[LRU_SIZE-1:0] & {LRU_SIZE{~exu_mp_way}}) |
                                          (fetch_wrlru_b3[LRU_SIZE-1:0] & {LRU_SIZE{tag_match_way0_f2[3]}}) );
   assign btb_lru_rd_f2[0] = use_mp_way[0] ? exu_mp_way_f : |(fetch_wrindex_dec[LRU_SIZE-1:0] & btb_lru_b0_f[LRU_SIZE-1:0]);
   assign btb_lru_rd_f2[1] = use_mp_way[1] ? exu_mp_way_f : |(fetch_wrindex_dec[LRU_SIZE-1:0] & btb_lru_b1_f[LRU_SIZE-1:0]);
   assign btb_lru_rd_f2[2] = use_mp_way[2] ? exu_mp_way_f : |(fetch_wrindex_dec[LRU_SIZE-1:0] & btb_lru_b2_f[LRU_SIZE-1:0]);
   assign btb_lru_rd_f2[3] = use_mp_way[3] ? exu_mp_way_f : |(fetch_wrindex_dec[LRU_SIZE-1:0] & btb_lru_b3_f[LRU_SIZE-1:0]);
   assign way_raw[7:0] =  tag_match_way1_expanded_f2[7:0] | (~wayhit_f2[7:0] & {{2{btb_lru_rd_f2[3]}}, {2{btb_lru_rd_f2[2]}}, {2{btb_lru_rd_f2[1]}}, {2{btb_lru_rd_f2[0]}}});
   rvdffe #(LRU_SIZE*4) btb_lru_ff (.*, .en(ifc_fetch_req_f2 | exu_mp_valid),
                                   .din({btb_lru_b0_ns[(LRU_SIZE)-1:0],
                                         btb_lru_b1_ns[(LRU_SIZE)-1:0],
                                         btb_lru_b2_ns[(LRU_SIZE)-1:0],
                                         btb_lru_b3_ns[(LRU_SIZE)-1:0]}),
                                   .dout({btb_lru_b0_f[(LRU_SIZE)-1:0],
                                          btb_lru_b1_f[(LRU_SIZE)-1:0],
                                          btb_lru_b2_f[(LRU_SIZE)-1:0],
                                          btb_lru_b3_f[(LRU_SIZE)-1:0]}));
   logic [16:1] btb_sel_data_f2;
   assign {
           btb_rd_tgt_f2[11:0],
           btb_rd_pc4_f2,
           btb_rd_boffset_f2,
           btb_rd_call_f2,
           btb_rd_ret_f2} = btb_sel_data_f2[16:1];
   assign btb_sel_data_f2[16:1] = ( ({16{btb_sel_f2[7]}} & btb_bank3o_rd_data_f2[16:1]) |
                                    ({16{btb_sel_f2[6]}} & btb_bank3e_rd_data_f2[16:1]) |
                                    ({16{btb_sel_f2[5]}} & btb_bank2o_rd_data_f2[16:1]) |
                                    ({16{btb_sel_f2[4]}} & btb_bank2e_rd_data_f2[16:1]) |
                                    ({16{btb_sel_f2[3]}} & btb_bank1o_rd_data_f2[16:1]) |
                                    ({16{btb_sel_f2[2]}} & btb_bank1e_rd_data_f2[16:1]) |
                                    ({16{btb_sel_f2[1]}} & btb_bank0o_rd_data_f2[16:1]) |
                                    ({16{btb_sel_f2[0]}} & btb_bank0e_rd_data_f2[16:1]) );
   logic [7:0] bp_valid_f2, bp_hist1_f2;
   assign ifu_bp_kill_next_f2 = |(bp_valid_f2[7:0] & bp_hist1_f2[7:0]) & ifc_fetch_req_f2 & ~leak_one_f2 & ~dec_tlu_bpred_disable;
   assign bht_force_taken_f2[7:0] = {(btb_bank3o_rd_data_f2[CALL] | btb_bank3o_rd_data_f2[RET]),
                                     (btb_bank3e_rd_data_f2[CALL] | btb_bank3e_rd_data_f2[RET]),
                                     (btb_bank2o_rd_data_f2[CALL] | btb_bank2o_rd_data_f2[RET]),
                                     (btb_bank2e_rd_data_f2[CALL] | btb_bank2e_rd_data_f2[RET]),
                                     (btb_bank1o_rd_data_f2[CALL] | btb_bank1o_rd_data_f2[RET]),
                                     (btb_bank1e_rd_data_f2[CALL] | btb_bank1e_rd_data_f2[RET]),
                                     (btb_bank0o_rd_data_f2[CALL] | btb_bank0o_rd_data_f2[RET]),
                                     (btb_bank0e_rd_data_f2[CALL] | btb_bank0e_rd_data_f2[RET])};
   assign bht_valid_f2[7:0] = wayhit_f2[7:0];
   assign bht_dir_f2[7:0] = {(bht_force_taken_f2[7] | bht_bank7_rd_data_f2[1]) & bht_valid_f2[7],
                             (bht_force_taken_f2[6] | bht_bank6_rd_data_f2[1]) & bht_valid_f2[6],
                             (bht_force_taken_f2[5] | bht_bank5_rd_data_f2[1]) & bht_valid_f2[5],
                             (bht_force_taken_f2[4] | bht_bank4_rd_data_f2[1]) & bht_valid_f2[4],
                             (bht_force_taken_f2[3] | bht_bank3_rd_data_f2[1]) & bht_valid_f2[3],
                             (bht_force_taken_f2[2] | bht_bank2_rd_data_f2[1]) & bht_valid_f2[2],
                             (bht_force_taken_f2[1] | bht_bank1_rd_data_f2[1]) & bht_valid_f2[1],
                             (bht_force_taken_f2[0] | bht_bank0_rd_data_f2[1]) & bht_valid_f2[0]};
   logic       minus1, plus1;
   assign plus1 = ( (~btb_rd_pc4_f2 &   btb_rd_boffset_f2 &  ~ifc_fetch_addr_f2[1]) |
                    ( btb_rd_pc4_f2 &  ~btb_rd_boffset_f2 &  ~ifc_fetch_addr_f2[1]) );
   assign minus1 = ( (~btb_rd_pc4_f2 &  ~btb_rd_boffset_f2 &   ifc_fetch_addr_f2[1]) |
                     ( btb_rd_pc4_f2 &   btb_rd_boffset_f2 &   ifc_fetch_addr_f2[1]) );
   assign ifu_bp_inst_mask_f2[7:1] = ( ({7{ ifu_bp_kill_next_f2}} & btb_vmask_f2[7:1]) |
                                       ({7{~ifu_bp_kill_next_f2}} & 7'b1111111) );
   logic [7:0] hist0_raw, hist1_raw, pc4_raw, pret_raw;
    assign hist1_raw[7:0] = bht_force_taken_f2[7:0] | {bht_bank7_rd_data_f2[1],
                                                      bht_bank6_rd_data_f2[1],
                                                      bht_bank5_rd_data_f2[1],
                                                      bht_bank4_rd_data_f2[1],
                                                      bht_bank3_rd_data_f2[1],
                                                      bht_bank2_rd_data_f2[1],
                                                      bht_bank1_rd_data_f2[1],
                                                      bht_bank0_rd_data_f2[1]};
   assign hist0_raw[7:0] = {bht_bank7_rd_data_f2[0],
                            bht_bank6_rd_data_f2[0],
                            bht_bank5_rd_data_f2[0],
                            bht_bank4_rd_data_f2[0],
                            bht_bank3_rd_data_f2[0],
                            bht_bank2_rd_data_f2[0],
                            bht_bank1_rd_data_f2[0],
                            bht_bank0_rd_data_f2[0]};
   assign pc4_raw[7:0] = {wayhit_f2[7] & btb_bank3o_rd_data_f2[PC4],
                          wayhit_f2[6] & btb_bank3e_rd_data_f2[PC4],
                          wayhit_f2[5] & btb_bank2o_rd_data_f2[PC4],
                          wayhit_f2[4] & btb_bank2e_rd_data_f2[PC4],
                          wayhit_f2[3] & btb_bank1o_rd_data_f2[PC4],
                          wayhit_f2[2] & btb_bank1e_rd_data_f2[PC4],
                          wayhit_f2[1] & btb_bank0o_rd_data_f2[PC4],
                          wayhit_f2[0] & btb_bank0e_rd_data_f2[PC4]};
   assign pret_raw[7:0] = {wayhit_f2[3] & ~btb_bank3o_rd_data_f2[CALL] & btb_bank3o_rd_data_f2[RET],
                           wayhit_f2[3] & ~btb_bank3e_rd_data_f2[CALL] & btb_bank3e_rd_data_f2[RET],
                           wayhit_f2[2] & ~btb_bank2o_rd_data_f2[CALL] & btb_bank2o_rd_data_f2[RET],
                           wayhit_f2[2] & ~btb_bank2e_rd_data_f2[CALL] & btb_bank2e_rd_data_f2[RET],
                           wayhit_f2[1] & ~btb_bank1o_rd_data_f2[CALL] & btb_bank1o_rd_data_f2[RET],
                           wayhit_f2[1] & ~btb_bank1e_rd_data_f2[CALL] & btb_bank1e_rd_data_f2[RET],
                           wayhit_f2[0] & ~btb_bank0o_rd_data_f2[CALL] & btb_bank0o_rd_data_f2[RET],
                           wayhit_f2[0] & ~btb_bank0e_rd_data_f2[CALL] & btb_bank0e_rd_data_f2[RET]};
assign fgmask_f2[6] = (~ifc_fetch_addr_f2[1]) | (~ifc_fetch_addr_f2[2]) | (
    ~ifc_fetch_addr_f2[3]);
assign fgmask_f2[5] = (~ifc_fetch_addr_f2[2]) | (~ifc_fetch_addr_f2[3]);
assign fgmask_f2[4] = (~ifc_fetch_addr_f2[2] & ~ifc_fetch_addr_f2[1]) | (
    ~ifc_fetch_addr_f2[3]);
assign fgmask_f2[3] = (~ifc_fetch_addr_f2[3]);
assign fgmask_f2[2] = (~ifc_fetch_addr_f2[3] & ~ifc_fetch_addr_f2[1]) | (
    ~ifc_fetch_addr_f2[3] & ~ifc_fetch_addr_f2[2]);
assign fgmask_f2[1] = (~ifc_fetch_addr_f2[3] & ~ifc_fetch_addr_f2[2]);
assign fgmask_f2[0] = (~ifc_fetch_addr_f2[3] & ~ifc_fetch_addr_f2[2]
     & ~ifc_fetch_addr_f2[1]);
   assign btb_sel_mask_f2[7:0] = {btb_sel_f2[7],
                                  |btb_sel_f2[7:6] & fgmask_f2[6],
                                  |btb_sel_f2[7:5] & fgmask_f2[5],
                                  |btb_sel_f2[7:4] & fgmask_f2[4],
                                  |btb_sel_f2[7:3] & fgmask_f2[3],
                                  |btb_sel_f2[7:2] & fgmask_f2[2],
                                  |btb_sel_f2[7:1] & fgmask_f2[1],
                                  |btb_sel_f2[7:0] & fgmask_f2[0]};
   assign num_valids[3:0] = countones(bht_valid_f2[7:0] & btb_sel_mask_f2[7:0]);
   assign final_h = |(btb_sel_f2[7:0] & bht_dir_f2[7:0]);
    assign merged_ghr[4:0] = ( ({5{num_valids[3:0] >= 4'h4}} & {fghr[4],3'b0,  final_h }) |  
                                            ({5{num_valids[3:0] == 4'h3}} & {fghr[4:3],2'b0, final_h}) |  
                                            ({5{num_valids[3:0] == 4'h2}} & {fghr[5-3:0], 1'b0, final_h}) |  
                                            ({5{num_valids[3:0] == 4'h1}} & {fghr[5-2:0], final_h}) |  
                                            ({5{num_valids[3:0] == 4'h0}} & {fghr[4:0]}) );  
   logic [4:0] exu_flush_ghr;
   assign exu_flush_ghr[4:0] = exu_mp_fghr[4:0];
   assign fghr_ns[4:0] = ( ({5{exu_flush_final}} & exu_flush_ghr[4:0]) |
                                         ({5{~exu_flush_final & ifc_fetch_req_f2_raw & ~leak_one_f2}} & merged_ghr[4:0]) |
                                         ({5{~exu_flush_final & ~(ifc_fetch_req_f2_raw & ~leak_one_f2)}} & fghr[4:0]));
   rvdff #(5) fetchghr (.*, .clk(active_clk), .din(fghr_ns[4:0]), .dout(fghr[4:0]));
   assign ifu_bp_fghr_f2[4:0] = fghr[4:0];
   assign ifu_bp_way_f2[7:0] = way_raw[7:0];
   assign ifu_bp_hist1_f2[7:0]    = hist1_raw[7:0];
   assign ifu_bp_hist0_f2[7:0]    = hist0_raw[7:0];
   assign ifu_bp_pc4_f2[7:0]     = pc4_raw[7:0];
   assign ifu_bp_valid_f2[7:0]   = wayhit_f2[7:0] & ~{8{dec_tlu_bpred_disable}};
   assign ifu_bp_ret_f2[7:0]     = pret_raw[7:0];
    always_comb begin
       casez(ifc_fetch_addr_f2[3:1])
        3'b000 : begin
           bp_hist1_f2[7:0]    = hist1_raw[7:0];
           bp_valid_f2[7:0]   = wayhit_f2[7:0];
        end
        3'b001 : begin
           bp_hist1_f2[7:0]    = {1'b0, hist1_raw[7:1]};
           bp_valid_f2[7:0]   = {1'b0, wayhit_f2[7:1]};
        end
        3'b010 : begin
           bp_hist1_f2[7:0]    = {2'b0, hist1_raw[7:2]};
           bp_valid_f2[7:0]   = {2'b0, wayhit_f2[7:2]};
        end
        3'b011 : begin
           bp_hist1_f2[7:0]    = {3'b0, hist1_raw[7:3]};
           bp_valid_f2[7:0]   = {3'b0, wayhit_f2[7:3]};
        end
        3'b100 : begin
           bp_hist1_f2[7:0]    = {4'b0, hist1_raw[7:4]};
           bp_valid_f2[7:0]   = {4'b0, wayhit_f2[7:4]};
        end
        3'b101 : begin
           bp_hist1_f2[7:0]    = {5'b0, hist1_raw[7:5]};
           bp_valid_f2[7:0]   = {5'b0, wayhit_f2[7:5]};
        end
        3'b110 : begin
           bp_hist1_f2[7:0]    = {6'b0, hist1_raw[7:6]};
           bp_valid_f2[7:0]   = {6'b0, wayhit_f2[7:6]};
           end
        3'b111 : begin
           bp_hist1_f2[7:0]    = {7'b0, hist1_raw[7]};
           bp_valid_f2[7:0]   = {7'b0, wayhit_f2[7]};
        end
        default: begin
          bp_hist1_f2[7:0] = hist1_raw[7:0];
          bp_valid_f2[7:0] = wayhit_f2[7:0];
        end
    endcase  
    end
    assign btb_fg_crossing_f2 = btb_sel_f2[0] & btb_rd_pc4_f2;
   wire [2:0] btb_sel_f2_enc, btb_sel_f2_enc_shift;
   assign btb_sel_f2_enc[2:0] = encode8_3(btb_sel_f2[7:0]);
   assign btb_sel_f2_enc_shift[2:0] = encode8_3({1'b0,btb_sel_f2[7:1]});
   assign bp_total_branch_offset_f2[3:1] =  (({3{ btb_rd_pc4_f2}} &  btb_sel_f2_enc_shift[2:0]) |
                                             ({3{~btb_rd_pc4_f2}} &  btb_sel_f2_enc[2:0]) |
                                             ({3{btb_fg_crossing_f2}}));
   logic [31:4] adder_pc_in_f2, ifc_fetch_adder_prior;
   rvdffe #(28) faddrf2_ff (.*, .en(ifc_fetch_req_f2 & ~ifu_bp_kill_next_f2 & ic_hit_f2), .din(ifc_fetch_addr_f2[31:4]), .dout(ifc_fetch_adder_prior[31:4]));
   assign ifu_bp_poffset_f2[11:0] = btb_rd_tgt_f2[11:0];
   assign adder_pc_in_f2[31:4] = ( ({28{ btb_fg_crossing_f2}} & ifc_fetch_adder_prior[31:4]) |
                                   ({28{~btb_fg_crossing_f2}} & ifc_fetch_addr_f2[31:4]));
   rvbradder predtgt_addr (.pc({adder_pc_in_f2[31:4], bp_total_branch_offset_f2[3:1]}),
                         .offset(btb_rd_tgt_f2[11:0]),
                         .dout(bp_btb_target_adder_f2[31:1])
                         );
   assign ifu_bp_btb_target_f2[31:1] = btb_rd_ret_f2 & ~btb_rd_call_f2 ? rets_out[0][31:1] : bp_btb_target_adder_f2[31:1];
   rvbradder rs_addr (.pc({adder_pc_in_f2[31:4], bp_total_branch_offset_f2[3:1]}),
                    .offset({10'b0, btb_rd_pc4_f2, ~btb_rd_pc4_f2}),
                    .dout(bp_rs_call_target_f2[31:1])
                         );
   logic rs_overpop_correct, rsoverpop_valid_ns, rsoverpop_valid_f;
   logic [31:1] rsoverpop_ns, rsoverpop_f;
   logic rsunderpop_valid_ns, rsunderpop_valid_f, rs_underpop_correct;
   assign rs_overpop_correct = 1'b0;
   assign rs_underpop_correct = 1'b0;
   assign rsoverpop_f[31:1]  = 'b0;
   logic e4_rs_correct;
   assign e4_rs_correct = 1'b0;
   assign rs_correct = 1'b0;
   assign rs_push = ((btb_rd_call_f2 & ~btb_rd_ret_f2 & ifu_bp_kill_next_f2) | (rs_overpop_correct & ~rs_underpop_correct)) & ~rs_correct & ~e4_rs_correct;
   assign rs_pop = ((btb_rd_ret_f2 & ~btb_rd_call_f2 & ifu_bp_kill_next_f2) | (rs_underpop_correct & ~rs_overpop_correct)) & ~rs_correct & ~e4_rs_correct;
   assign rs_hold = ~rs_push & ~rs_pop & ~rs_overpop_correct & ~rs_underpop_correct & ~rs_correct & ~e4_rs_correct;
   assign rets_in[0][31:1] = ( ({31{rs_overpop_correct & rs_underpop_correct}} & rsoverpop_f[31:1]) |
                               ({31{rs_push & rs_overpop_correct}} & rsoverpop_f[31:1]) |
                               ({31{rs_push & ~rs_overpop_correct}} & bp_rs_call_target_f2[31:1]) |
                               ({31{rs_pop}}  & rets_out[1][31:1]) );
   assign rsenable[0] = ~rs_hold;
   for (i=0; i<4; i++) begin : retstack
      if(i==4-1) begin
         assign rets_in[i][31:1] = rets_out[i-1][31:1];
         assign rsenable[i] = rs_push | rs_correct | e4_rs_correct;
      end
      else if(i>0) begin
        assign rets_in[i][31:1] = ( ({31{rs_push}} & rets_out[i-1][31:1]) |
                                    ({31{rs_pop}}  & rets_out[i+1][31:1]) );
         assign rsenable[i] = rs_push | rs_pop | rs_correct | e4_rs_correct;
      end
      rvdffe #(31) rets_ff (.*, .en(rsenable[i]), .din(rets_in[i][31:1]), .dout(rets_out[i][31:1]));
   end : retstack
   assign dec_tlu_error_wb = dec_tlu_br0_start_error_wb | dec_tlu_br0_error_wb | dec_tlu_br1_start_error_wb | dec_tlu_br1_error_wb;
   assign dec_tlu_all_banks_error_wb = dec_tlu_br0_start_error_wb | (~dec_tlu_br0_error_wb & dec_tlu_br1_start_error_wb);
   assign dec_tlu_error_bank_wb[1:0] = (dec_tlu_br0_error_wb | dec_tlu_br0_start_error_wb) ? dec_tlu_br0_bank_wb[1:0] : dec_tlu_br1_bank_wb[1:0];
   assign btb_error_addr_wb[5:4] = (dec_tlu_br0_error_wb | dec_tlu_br0_start_error_wb) ? dec_tlu_br0_addr_wb[5:4] : dec_tlu_br1_addr_wb[5:4];
   assign dec_tlu_way_wb = (dec_tlu_br0_error_wb | dec_tlu_br0_start_error_wb) ? dec_tlu_br0_way_wb : dec_tlu_br1_way_wb;
   assign btb_valid = exu_mp_valid & ~dec_tlu_error_wb;
   assign btb_wr_tag[9-1:0] = exu_mp_btag[9-1:0];
   rvbtb_tag_hash rdtagf1(.hash(fetch_rd_tag_f1[9-1:0]), .pc({ifc_fetch_addr_f1[31:4], 3'b0}));
   rvdff #(9) rdtagf (.*, .clk(active_clk), .din({fetch_rd_tag_f1[9-1:0]}), .dout({fetch_rd_tag_f2[9-1:0]}));
   assign btb_wr_data[16+9:0] = {btb_wr_tag[9-1:0], exu_mp_tgt[11:0], exu_mp_pc4, exu_mp_boffset, exu_mp_call | exu_mp_ja, exu_mp_ret | exu_mp_ja, btb_valid} ;
   assign exu_mp_valid_write = exu_mp_valid & exu_mp_ataken;
   assign btb_wr_en_way0[3:0] = ( ({4{~exu_mp_way & exu_mp_valid_write & ~dec_tlu_error_wb}} & decode2_4(exu_mp_bank[1:0])) |
                                  ({4{~dec_tlu_way_wb & dec_tlu_error_wb & ~dec_tlu_all_banks_error_wb}} & decode2_4(dec_tlu_error_bank_wb[1:0])) |
                                  ({4{~dec_tlu_way_wb & dec_tlu_all_banks_error_wb}}));
   assign btb_wr_en_way1[3:0] = ( ({4{exu_mp_way & exu_mp_valid_write & ~dec_tlu_error_wb}} & decode2_4(exu_mp_bank[1:0])) |
                                  ({4{dec_tlu_way_wb & dec_tlu_error_wb & ~dec_tlu_all_banks_error_wb}} & decode2_4(dec_tlu_error_bank_wb[1:0])) |
                                  ({4{dec_tlu_way_wb & dec_tlu_all_banks_error_wb}}));
   assign btb_wr_addr[5:4] = dec_tlu_error_wb ? btb_error_addr_wb[5:4] : exu_mp_addr[5:4];
   logic [1:0] bht_wr_data0, bht_wr_data1, bht_wr_data2;
   logic [7:0] bht_wr_en0, bht_wr_en1, bht_wr_en2;
   assign middle_of_bank = exu_mp_pc4 ^ exu_mp_boffset;
   assign bht_wr_en0[7:0] = {8{exu_mp_valid & ~exu_mp_call & ~exu_mp_ret & ~exu_mp_ja}} & decode3_8({exu_mp_bank[1:0], middle_of_bank});
   assign bht_wr_en1[7:0] = {8{dec_tlu_br1_v_wb}} & decode3_8({dec_tlu_br1_bank_wb[1:0], dec_tlu_br1_middle_wb});
   assign bht_wr_en2[7:0] = {8{dec_tlu_br0_v_wb}} & decode3_8({dec_tlu_br0_bank_wb[1:0], dec_tlu_br0_middle_wb});
   assign bht_wr_data0[1:0] = exu_mp_hist[1:0];  
   assign bht_wr_data1[1:0] = dec_tlu_br1_hist_wb[1:0];
   assign bht_wr_data2[1:0] = dec_tlu_br0_hist_wb[1:0];  
   logic [7:4] bht_rd_addr_f1, bht_wr_addr0, bht_wr_addr1, bht_wr_addr2;
   logic [7:4] mp_hashed, br0_hashed_wb, br1_hashed_wb, bht_rd_addr_hashed_f1;
   rvbtb_ghr_hash mpghrhs  (.hashin(exu_mp_addr[5:4]), .ghr(exu_mp_eghr[4:0]), .hash(mp_hashed[7:4]));
   rvbtb_ghr_hash br0ghrhs (.hashin(dec_tlu_br0_addr_wb[5:4]), .ghr(dec_tlu_br0_fghr_wb[4:0]), .hash(br0_hashed_wb[7:4]));
   rvbtb_ghr_hash br1ghrhs (.hashin(dec_tlu_br1_addr_wb[5:4]), .ghr(dec_tlu_br1_fghr_wb[4:0]), .hash(br1_hashed_wb[7:4]));
   rvbtb_ghr_hash fghrhs (.hashin(btb_rd_addr_f1[5:4]), .ghr(fghr_ns[4:0]), .hash(bht_rd_addr_hashed_f1[7:4]));
   assign bht_wr_addr0[7:4] = mp_hashed[7:4];
   assign bht_wr_addr1[7:4] = br1_hashed_wb[7:4];
   assign bht_wr_addr2[7:4] = br0_hashed_wb[7:4];
   assign bht_rd_addr_f1[7:4] = bht_rd_addr_hashed_f1[7:4];
    for (j=0 ; j<LRU_SIZE ; j++) begin : BTB_FLOPS
          rvdffe #(17+9) btb_bank0_way0 (.*,
                    .en(((btb_wr_addr[5:4] == j) & btb_wr_en_way0[0])),
                    .din        (btb_wr_data[16+9:0]),
                    .dout       (btb_bank0_rd_data_way0_out[j]));
          rvdffe #(17+9) btb_bank1_way0 (.*,
                    .en(((btb_wr_addr[5:4] == j) & btb_wr_en_way0[1])),
                    .din        (btb_wr_data[16+9:0]),
                    .dout       (btb_bank1_rd_data_way0_out[j]));
          rvdffe #(17+9) btb_bank2_way0 (.*,
                    .en(((btb_wr_addr[5:4] == j) & btb_wr_en_way0[2])),
                    .din        (btb_wr_data[16+9:0]),
                    .dout       (btb_bank2_rd_data_way0_out[j]));
          rvdffe #(17+9) btb_bank3_way0 (.*,
                    .en(((btb_wr_addr[5:4] == j) & btb_wr_en_way0[3])),
                    .din        (btb_wr_data[16+9:0]),
                    .dout       (btb_bank3_rd_data_way0_out[j]));
          rvdffe #(17+9) btb_bank0_way1 (.*,
                    .en(((btb_wr_addr[5:4] == j) & btb_wr_en_way1[0])),
                    .din        (btb_wr_data[16+9:0]),
                    .dout       (btb_bank0_rd_data_way1_out[j]));
          rvdffe #(17+9) btb_bank1_way1 (.*,
                    .en(((btb_wr_addr[5:4] == j) & btb_wr_en_way1[1])),
                    .din        (btb_wr_data[16+9:0]),
                    .dout       (btb_bank1_rd_data_way1_out[j]));
          rvdffe #(17+9) btb_bank2_way1 (.*,
                    .en(((btb_wr_addr[5:4] == j) & btb_wr_en_way1[2])),
                    .din        (btb_wr_data[16+9:0]),
                    .dout       (btb_bank2_rd_data_way1_out[j]));
          rvdffe #(17+9) btb_bank3_way1 (.*,
                    .en(((btb_wr_addr[5:4] == j) & btb_wr_en_way1[3])),
                    .din        (btb_wr_data[16+9:0]),
                    .dout       (btb_bank3_rd_data_way1_out[j]));
    end
   rvdffe #(17+9) btb_bank0_way0_data_out (.*,
                    .en(ifc_fetch_req_f1),
                    .din        (btb_bank0_rd_data_way0_f2_in[16+9:0]),
                    .dout       (btb_bank0_rd_data_way0_f2   [16+9:0]));
   rvdffe #(17+9) btb_bank1_way0_data_out (.*,
                    .en(ifc_fetch_req_f1),
                    .din        (btb_bank1_rd_data_way0_f2_in[16+9:0]),
                    .dout       (btb_bank1_rd_data_way0_f2   [16+9:0]));
   rvdffe #(17+9) btb_bank2_way0_data_out (.*,
                    .en(ifc_fetch_req_f1),
                    .din        (btb_bank2_rd_data_way0_f2_in[16+9:0]),
                    .dout       (btb_bank2_rd_data_way0_f2   [16+9:0]));
   rvdffe #(17+9) btb_bank3_way0_data_out (.*,
                    .en(ifc_fetch_req_f1),
                    .din        (btb_bank3_rd_data_way0_f2_in[16+9:0]),
                    .dout       (btb_bank3_rd_data_way0_f2   [16+9:0]));
   rvdffe #(17+9) btb_bank0_way1_data_out (.*,
                    .en(ifc_fetch_req_f1),
                    .din        (btb_bank0_rd_data_way1_f2_in[16+9:0]),
                    .dout       (btb_bank0_rd_data_way1_f2   [16+9:0]));
   rvdffe #(17+9) btb_bank1_way1_data_out (.*,
                    .en(ifc_fetch_req_f1),
                    .din        (btb_bank1_rd_data_way1_f2_in[16+9:0]),
                    .dout       (btb_bank1_rd_data_way1_f2   [16+9:0]));
   rvdffe #(17+9) btb_bank2_way1_data_out (.*,
                    .en(ifc_fetch_req_f1),
                    .din        (btb_bank2_rd_data_way1_f2_in[16+9:0]),
                    .dout       (btb_bank2_rd_data_way1_f2   [16+9:0]));
   rvdffe #(17+9) btb_bank3_way1_data_out (.*,
                    .en(ifc_fetch_req_f1),
                    .din        (btb_bank3_rd_data_way1_f2_in[16+9:0]),
                    .dout       (btb_bank3_rd_data_way1_f2   [16+9:0]));
    always_comb begin : BTB_rd_mux
        btb_bank0_rd_data_way0_f2_in[16+9:0] = '0 ;
        btb_bank1_rd_data_way0_f2_in[16+9:0] = '0 ;
        btb_bank2_rd_data_way0_f2_in[16+9:0] = '0 ;
        btb_bank3_rd_data_way0_f2_in[16+9:0] = '0 ;
        btb_bank0_rd_data_way1_f2_in[16+9:0] = '0 ;
        btb_bank1_rd_data_way1_f2_in[16+9:0] = '0 ;
        btb_bank2_rd_data_way1_f2_in[16+9:0] = '0 ;
        btb_bank3_rd_data_way1_f2_in[16+9:0] = '0 ;
        for (int j=0; j< LRU_SIZE; j++) begin
          if (btb_rd_addr_f1[5:4] == (5-4+1)'(j)) begin
           btb_bank0_rd_data_way0_f2_in[16+9:0] =  btb_bank0_rd_data_way0_out[j];
           btb_bank1_rd_data_way0_f2_in[16+9:0] =  btb_bank1_rd_data_way0_out[j];
           btb_bank2_rd_data_way0_f2_in[16+9:0] =  btb_bank2_rd_data_way0_out[j];
           btb_bank3_rd_data_way0_f2_in[16+9:0] =  btb_bank3_rd_data_way0_out[j];
           btb_bank0_rd_data_way1_f2_in[16+9:0] =  btb_bank0_rd_data_way1_out[j];
           btb_bank1_rd_data_way1_f2_in[16+9:0] =  btb_bank1_rd_data_way1_out[j];
           btb_bank2_rd_data_way1_f2_in[16+9:0] =  btb_bank2_rd_data_way1_out[j];
           btb_bank3_rd_data_way1_f2_in[16+9:0] =  btb_bank3_rd_data_way1_out[j];
          end
        end
    end
   logic [7:0] [(16/NUM_BHT_LOOP)-1:0][NUM_BHT_LOOP-1:0][1:0]      bht_bank_wr_data ;
   logic [7:0] [16-1:0] [1:0]                bht_bank_rd_data_out ;
   logic [1:0]                                                bht_bank0_rd_data_f2_in, bht_bank1_rd_data_f2_in, bht_bank2_rd_data_f2_in, bht_bank3_rd_data_f2_in;
   logic [1:0]                                                bht_bank4_rd_data_f2_in, bht_bank5_rd_data_f2_in, bht_bank6_rd_data_f2_in, bht_bank7_rd_data_f2_in;
   logic [7:0] [(16/NUM_BHT_LOOP)-1:0]                 bht_bank_clken ;
   logic [7:0] [(16/NUM_BHT_LOOP)-1:0]                 bht_bank_clk   ;
   logic [7:0] [(16/NUM_BHT_LOOP)-1:0][NUM_BHT_LOOP-1:0]           bht_bank_sel   ;
   for ( i=0; i<8; i++) begin : BANKS
     for (genvar k=0 ; k < (16)/NUM_BHT_LOOP ; k++) begin : BHT_CLK_GROUP
     assign bht_bank_clken[i][k]  = (bht_wr_en0[i] & ((bht_wr_addr0[7: NUM_BHT_LOOP_OUTER_LO]==k) |  BHT_NO_ADDR_MATCH)) |
                                    (bht_wr_en1[i] & ((bht_wr_addr1[7: NUM_BHT_LOOP_OUTER_LO]==k) |  BHT_NO_ADDR_MATCH)) |
                                    (bht_wr_en2[i] & ((bht_wr_addr2[7: NUM_BHT_LOOP_OUTER_LO]==k) |  BHT_NO_ADDR_MATCH));
     assign bht_bank_clk[i][k] = '0;
     for (j=0 ; j<NUM_BHT_LOOP ; j++) begin : BHT_FLOPS
       assign   bht_bank_sel[i][k][j]    = (bht_wr_en0[i] & (bht_wr_addr0[NUM_BHT_LOOP_INNER_HI :4] == j) & ((bht_wr_addr0[7: NUM_BHT_LOOP_OUTER_LO]==k) | BHT_NO_ADDR_MATCH)) |
                                           (bht_wr_en1[i] & (bht_wr_addr1[NUM_BHT_LOOP_INNER_HI :4] == j) & ((bht_wr_addr1[7: NUM_BHT_LOOP_OUTER_LO]==k) | BHT_NO_ADDR_MATCH)) |
                                           (bht_wr_en2[i] & (bht_wr_addr2[NUM_BHT_LOOP_INNER_HI :4] == j) & ((bht_wr_addr2[7: NUM_BHT_LOOP_OUTER_LO]==k) | BHT_NO_ADDR_MATCH)) ;
       assign bht_bank_wr_data[i][k][j]  = (bht_wr_en2[i] & (bht_wr_addr2[NUM_BHT_LOOP_INNER_HI:4] == j) & ((bht_wr_addr2[7: NUM_BHT_LOOP_OUTER_LO]==k) | BHT_NO_ADDR_MATCH)) ? bht_wr_data2[1:0] :
                                           (bht_wr_en1[i] & (bht_wr_addr1[NUM_BHT_LOOP_INNER_HI:4] == j) & ((bht_wr_addr1[7: NUM_BHT_LOOP_OUTER_LO]==k) | BHT_NO_ADDR_MATCH)) ? bht_wr_data1[1:0] :
                                                                                                                      bht_wr_data0[1:0]   ;
          rvdffs_fpga #(2) bht_bank (.*,
                    .clk        (bht_bank_clk[i][k]),
                    .clken      (bht_bank_sel[i][k][j]),
                    .rawclk     (clk),
                    .en         (bht_bank_sel[i][k][j]),
                    .din        (bht_bank_wr_data[i][k][j]),
                    .dout       (bht_bank_rd_data_out[i][(16*k)+j]));
      end  
   end  
 end  
    always_comb begin : BHT_rd_mux
     bht_bank0_rd_data_f2_in[1:0] = '0 ;
     bht_bank1_rd_data_f2_in[1:0] = '0 ;
     bht_bank2_rd_data_f2_in[1:0] = '0 ;
     bht_bank3_rd_data_f2_in[1:0] = '0 ;
     bht_bank4_rd_data_f2_in[1:0] = '0 ;
     bht_bank5_rd_data_f2_in[1:0] = '0 ;
     bht_bank6_rd_data_f2_in[1:0] = '0 ;
     bht_bank7_rd_data_f2_in[1:0] = '0 ;
     for (int j=0; j< 16; j++) begin
       if (bht_rd_addr_f1[7:4] == (7-4+1)'(j)) begin
         bht_bank0_rd_data_f2_in[1:0] = bht_bank_rd_data_out[0][j];
         bht_bank1_rd_data_f2_in[1:0] = bht_bank_rd_data_out[1][j];
         bht_bank2_rd_data_f2_in[1:0] = bht_bank_rd_data_out[2][j];
         bht_bank3_rd_data_f2_in[1:0] = bht_bank_rd_data_out[3][j];
         bht_bank4_rd_data_f2_in[1:0] = bht_bank_rd_data_out[4][j];
         bht_bank5_rd_data_f2_in[1:0] = bht_bank_rd_data_out[5][j];
         bht_bank6_rd_data_f2_in[1:0] = bht_bank_rd_data_out[6][j];
         bht_bank7_rd_data_f2_in[1:0] = bht_bank_rd_data_out[7][j];
       end
      end
    end  
   rvdffe #(16) bht_dataoutf (.*, .en         (ifc_fetch_req_f1),
                                 .din        ({bht_bank0_rd_data_f2_in[1:0],
                                               bht_bank1_rd_data_f2_in[1:0],
                                               bht_bank2_rd_data_f2_in[1:0],
                                               bht_bank3_rd_data_f2_in[1:0],
                                               bht_bank4_rd_data_f2_in[1:0],
                                               bht_bank5_rd_data_f2_in[1:0],
                                               bht_bank6_rd_data_f2_in[1:0],
                                               bht_bank7_rd_data_f2_in[1:0]
                                               }),
                                 .dout       ({bht_bank0_rd_data_f2   [1:0],
                                               bht_bank1_rd_data_f2   [1:0],
                                               bht_bank2_rd_data_f2   [1:0],
                                               bht_bank3_rd_data_f2   [1:0],
                                               bht_bank4_rd_data_f2   [1:0],
                                               bht_bank5_rd_data_f2   [1:0],
                                               bht_bank6_rd_data_f2   [1:0],
                                               bht_bank7_rd_data_f2   [1:0]
                                               }));
     function [2:0] encode8_3;
      input [7:0] in;
      encode8_3[2] = |in[7:4];
      encode8_3[1] = in[7] | in[6] | in[3] | in[2];
      encode8_3[0] = in[7] | in[5] | in[3] | in[1];
   endfunction
   function [7:0] decode3_8;
      input [2:0] in;
      decode3_8[7] =  in[2] &  in[1] &  in[0];
      decode3_8[6] =  in[2] &  in[1] & ~in[0];
      decode3_8[5] =  in[2] & ~in[1] &  in[0];
      decode3_8[4] =  in[2] & ~in[1] & ~in[0];
      decode3_8[3] = ~in[2] &  in[1] &  in[0];
      decode3_8[2] = ~in[2] &  in[1] & ~in[0];
      decode3_8[1] = ~in[2] & ~in[1] &  in[0];
      decode3_8[0] = ~in[2] & ~in[1] & ~in[0];
   endfunction
   function [3:0] decode2_4;
      input [1:0] in;
      decode2_4[3] =  in[1] &  in[0];
      decode2_4[2] =  in[1] & ~in[0];
      decode2_4[1] = ~in[1] &  in[0];
      decode2_4[0] = ~in[1] & ~in[0];
   endfunction
   function [3:0] countones;
      input [7:0] valid;
      begin
countones[3:0] = {3'b0, valid[7]} +
                 {3'b0, valid[6]} +
                 {3'b0, valid[5]} +
                 {3'b0, valid[4]} +
                 {3'b0, valid[3]} +
                 {3'b0, valid[2]} +
                 {3'b0, valid[1]} +
                 {3'b0, valid[0]};
      end
   endfunction
   function [2:0] newlru;  
      input [2:0] lru; 
      input [1:0] used; 
      begin
newlru[2] = (lru[2] & ~used[0]) | (~used[1] & ~used[0]);
newlru[1] = (~used[1] & ~used[0]) | (used[0]);
newlru[0] = (~lru[2] & lru[1] & ~used[1] & ~used[0]) | (~lru[1] & ~lru[0] & used[0]) | (
    ~lru[2] & lru[0] & used[0]) | (lru[0] & ~used[1] & ~used[0]);
      end
   endfunction 
   function [1:0] lru2way;  
      input [2:0] lru;  
      input [2:0] v;  
      begin
         lru2way[1] = (~lru[2] & lru[1] & ~lru[0] & v[1] & v[0]) | (lru[2] & lru[0] & v[1] & v[0]) | (~v[2] & v[1] & v[0]);
         lru2way[0] = (lru[2] & ~lru[0] & v[2] & v[0]) | (~v[1] & v[0]);
      end
   endfunction
endmodule  
module ifu_ic_mem
  (
      input logic free_clk,
      input logic clk,
      input logic rst_l,
      input logic clk_override,
      input logic dec_tlu_core_ecc_disable,
      input logic [31:2]  ic_rw_addr,
      input logic [3:0]   ic_wr_en  ,   
      input logic         ic_rd_en  ,   
      input logic [15:2]               ic_debug_addr,       
      input logic                      ic_debug_rd_en,      
      input logic                      ic_debug_wr_en,      
      input logic                      ic_debug_tag_array,  
      input logic [3:0]                ic_debug_way,        
      input logic [127:0]              ic_premux_data,      
      input logic                      ic_sel_premux_data,  
      input  logic [67:0]               ic_wr_data,          
      output logic [135:0]              ic_rd_data ,         
      output logic [20:0]               ictag_debug_rd_data, 
      input logic  [33:0]               ic_debug_wr_data,    
      input logic [3:0]   ic_tag_valid,   
      output logic [3:0]   ic_rd_hit,    
      output logic         ic_tag_perr,  
      input  logic         scan_mode
      ) ;
localparam TOTAL_INT        = 9;
localparam DCCM_BITS        = 16;
localparam DCCM_BANK_BITS   = 3;
localparam DCCM_NUM_BANKS   = 8;
localparam DCCM_DATA_WIDTH  = 32;
localparam DCCM_FDATA_WIDTH = 39;
localparam DCCM_BYTE_WIDTH  = 4;
localparam DCCM_ECC_WIDTH   = 7;
localparam LSU_RDBUF_DEPTH  = 8;
localparam DMA_BUF_DEPTH    = 4;
localparam LSU_STBUF_DEPTH  = 8;
localparam LSU_SB_BITS      = 16;
localparam DEC_INSTBUF_DEPTH = 4;
localparam ICCM_SIZE         = 512;
localparam ICCM_BITS         = 19;
localparam ICCM_NUM_BANKS    = 8;
localparam ICCM_BANK_BITS    = 3;
localparam ICCM_INDEX_BITS   = 14;
localparam ICCM_BANK_HI      = 4 + (3/4);
localparam ICACHE_TAG_HIGH  = 12;
localparam ICACHE_TAG_LOW   = 6;
localparam ICACHE_IC_DEPTH  = 8;
localparam ICACHE_TAG_DEPTH = 64;
localparam LSU_BUS_TAG     = 4;
localparam DMA_BUS_TAG     = 1;
localparam SB_BUS_TAG      = 1;
localparam IFU_BUS_TAG     = 3;
   IC_TAG #( .ICACHE_TAG_HIGH(ICACHE_TAG_HIGH) ,
             .ICACHE_TAG_LOW(ICACHE_TAG_LOW) ,
             .ICACHE_TAG_DEPTH(ICACHE_TAG_DEPTH)
             ) ic_tag_inst
          (
           .*,
           .ic_wr_en     (ic_wr_en[3:0]),
           .ic_debug_addr(ic_debug_addr[ICACHE_TAG_HIGH-1:2]),
           .ic_rw_addr   (ic_rw_addr[31:3])
           ) ;
   IC_DATA #( .ICACHE_TAG_HIGH(ICACHE_TAG_HIGH) ,
              .ICACHE_TAG_LOW(ICACHE_TAG_LOW) ,
              .ICACHE_IC_DEPTH(ICACHE_IC_DEPTH)
             ) ic_data_inst
          (
           .*,
           .ic_wr_en     (ic_wr_en[3:0]),
           .ic_debug_addr(ic_debug_addr[ICACHE_TAG_HIGH-1:2]),
           .ic_rw_addr   (ic_rw_addr[ICACHE_TAG_HIGH-1:2])
           ) ;
 endmodule
module IC_DATA #(parameter ICACHE_TAG_HIGH = 16 ,
                           ICACHE_TAG_LOW=6 ,
                           ICACHE_IC_DEPTH=1024
                                        )
     (
      input logic free_clk,
      input logic clk,
      input logic rst_l,
      input logic clk_override,
      input logic [ICACHE_TAG_HIGH-1:2]  ic_rw_addr,
      input logic [3:0]                  ic_wr_en,
      input logic                        ic_rd_en,   
      input  logic [67:0]               ic_wr_data,          
      output logic [135:0]              ic_rd_data ,         
      input  logic [33:0]               ic_debug_wr_data,    
      input logic [ICACHE_TAG_HIGH-1:2]  ic_debug_addr,       
      input logic                        ic_debug_rd_en,      
      input logic                        ic_debug_wr_en,      
      input logic                        ic_debug_tag_array,  
      input logic [3:0]                  ic_debug_way,        
      input logic [127:0]                ic_premux_data,      
      input logic                        ic_sel_premux_data,  
      input logic [3:0]          ic_rd_hit,
      input  logic               scan_mode
      ) ;
   logic [5:4]             ic_rw_addr_ff;
   logic [3:0][3:0]       ic_b_sb_wren, ic_bank_way_clken, ic_bank_way_clk;     
   logic                   ic_debug_sel_sb0 ;
   logic                   ic_debug_sel_sb1 ;
   logic                   ic_debug_sel_sb2 ;
   logic                   ic_debug_sel_sb3 ;
   logic [3:0] [135:0]     bank_set_dout;
   logic [3:0] [135:0]     wb_dout ;  
   logic [3:0] [33:0]      ic_sb_wr_data;
   logic                   ic_b_rden;
   logic [3:0]             ic_debug_rd_way_en;    
   logic [3:0]             ic_debug_rd_way_en_ff;    
   logic [3:0]             ic_debug_wr_way_en;    
   logic [ICACHE_TAG_HIGH-1:4]  ic_rw_addr_q;
   assign  ic_debug_rd_way_en[3:0] =  {4{ic_debug_rd_en & ~ic_debug_tag_array}} & ic_debug_way[3:0] ;
   assign  ic_debug_wr_way_en[3:0] =  {4{ic_debug_wr_en & ~ic_debug_tag_array}} & ic_debug_way[3:0] ;
   assign  ic_b_sb_wren[0][3:0]  = (ic_wr_en[3:0]           & {4{~ic_rw_addr[3]}} ) |
                                   (ic_debug_wr_way_en[3:0] & {4{ic_debug_addr[3:2] == 2'b00}}) ;
   assign  ic_b_sb_wren[1][3:0]  = (ic_wr_en[3:0]           & {4{~ic_rw_addr[3]}} ) |
                                   (ic_debug_wr_way_en[3:0] & {4{ic_debug_addr[3:2] == 2'b01}}) ;
   assign  ic_b_sb_wren[2][3:0]  = (ic_wr_en[3:0]           & {4{ic_rw_addr[3]}} ) |
                                   (ic_debug_wr_way_en[3:0] & {4{ic_debug_addr[3:2] == 2'b10}}) ;
   assign  ic_b_sb_wren[3][3:0]  = (ic_wr_en[3:0]           & {4{ic_rw_addr[3]}} ) |
                                   (ic_debug_wr_way_en[3:0] & {4{ic_debug_addr[3:2] == 2'b11}}) ;
   assign  ic_debug_sel_sb0       =  (ic_debug_addr[3:2] == 2'b00 ) ;
   assign  ic_debug_sel_sb1       =  (ic_debug_addr[3:2] == 2'b01 ) ;
   assign  ic_debug_sel_sb2       =  (ic_debug_addr[3:2] == 2'b10 ) ;
   assign  ic_debug_sel_sb3       =  (ic_debug_addr[3:2] == 2'b11 ) ;
   assign  ic_sb_wr_data[0][33:0]   =  (ic_debug_sel_sb0 & ic_debug_wr_en) ? ic_debug_wr_data[33:0] :
                                                                             ic_wr_data[33:0] ;
   assign  ic_sb_wr_data[1][33:0]   =  (ic_debug_sel_sb1 & ic_debug_wr_en) ? ic_debug_wr_data[33:0] :
                                                                             ic_wr_data[67:34] ;
   assign  ic_sb_wr_data[2][33:0]   =  (ic_debug_sel_sb2 & ic_debug_wr_en) ? ic_debug_wr_data[33:0] :
                                                                             ic_wr_data[33:0] ;
   assign  ic_sb_wr_data[3][33:0]   =  (ic_debug_sel_sb3 & ic_debug_wr_en) ? ic_debug_wr_data[33:0] :
                                                                             ic_wr_data[67:34] ;
   assign  ic_b_rden       = (ic_rd_en   | ic_debug_rd_en );
   logic [3:0] ic_bank_read;
   logic [3:0] ic_bank_read_ff;
   assign ic_bank_read[0] = (ic_b_rden) & (~|ic_rw_addr[3:2] | ic_debug_rd_en);
   assign ic_bank_read[1] = (ic_b_rden) & (~ic_rw_addr[3] | ic_debug_rd_en);
   assign ic_bank_read[2] = (ic_b_rden) & (~&ic_rw_addr[3:2] | ic_debug_rd_en);
   assign ic_bank_read[3] = (ic_b_rden) | ic_debug_rd_en;
    assign ic_rw_addr_q[ICACHE_TAG_HIGH-1:4] = (ic_debug_rd_en | ic_debug_wr_en) ?
                                                ic_debug_addr[ICACHE_TAG_HIGH-1:4] :
                                                ic_rw_addr[ICACHE_TAG_HIGH-1:4] ;
   logic ic_debug_rd_en_ff;
   rvdff #(2) adr_ff (.*,
                    .clk(free_clk),
                    .din ({ic_rw_addr_q[5:4]}),
                    .dout({ic_rw_addr_ff[5:4]}));
   rvdff #(4) bank_adr_ff (.*,
                    .clk(free_clk),
                    .din (ic_bank_read[3:0]),
                    .dout(ic_bank_read_ff[3:0]));
   rvdff #(5) debug_rd_wy_ff (.*,
                    .clk(free_clk),
                    .din ({ic_debug_rd_way_en[3:0], ic_debug_rd_en}),
                    .dout({ic_debug_rd_way_en_ff[3:0], ic_debug_rd_en_ff}));
localparam NUM_WAYS=4 ;
localparam NUM_SUBBANKS=4 ;
     for (genvar i=0; i<NUM_WAYS; i++) begin: WAYS
        for (genvar k=0; k<NUM_SUBBANKS; k++) begin: SUBBANKS    
           assign  ic_bank_way_clken[i][k]   = ic_bank_read[k] |  ic_b_sb_wren[k][i];
           rvoclkhdr bank_way_bank_c1_cgc  ( .en(ic_bank_way_clken[i][k] | clk_override), .l1clk(ic_bank_way_clk[i][k]), .* );
         ram_256x34  ic_bank_sb_way_data (
                                     .CLK(ic_bank_way_clk[i][k]),
                                     .WE (ic_b_sb_wren[k][i]),
                                     .D  (ic_sb_wr_data[k][33:0]),
                                     .ADR(ic_rw_addr_q[ICACHE_TAG_HIGH-1:4]),
                                     .Q  (wb_dout[i][(k+1)*34-1:k*34])
                                    );
        end  
      end
   logic [3:0] ic_rd_hit_q;
   assign ic_rd_hit_q[3:0] = ic_debug_rd_en_ff ? ic_debug_rd_way_en_ff[3:0] : ic_rd_hit[3:0] ;
   logic       [135:0] ic_premux_data_ext;
   logic [3:0] [135:0] wb_dout_way;
   logic [3:0] [135:0] wb_dout_way_with_premux;
   assign ic_premux_data_ext[135:0]   = {2'b0,ic_premux_data[127:96],2'b0,ic_premux_data[95:64] ,2'b0,ic_premux_data[63:32],2'b0,ic_premux_data[31:0]};
   assign wb_dout_way[0][135:0]       = wb_dout[0][135:0] &  {  {34{ic_bank_read_ff[3]}} ,  {34{ic_bank_read_ff[2]}}  ,  {34{ic_bank_read_ff[1]}}  ,  {34{ic_bank_read_ff[0]}} };
   assign wb_dout_way[1][135:0]       = wb_dout[1][135:0] &  {  {34{ic_bank_read_ff[3]}} ,  {34{ic_bank_read_ff[2]}}  ,  {34{ic_bank_read_ff[1]}}  ,  {34{ic_bank_read_ff[0]}} };
   assign wb_dout_way[2][135:0]       = wb_dout[2][135:0] &  {  {34{ic_bank_read_ff[3]}} ,  {34{ic_bank_read_ff[2]}}  ,  {34{ic_bank_read_ff[1]}}  ,  {34{ic_bank_read_ff[0]}} };
   assign wb_dout_way[3][135:0]       = wb_dout[3][135:0] &  {  {34{ic_bank_read_ff[3]}} ,  {34{ic_bank_read_ff[2]}}  ,  {34{ic_bank_read_ff[1]}}  ,  {34{ic_bank_read_ff[0]}} };
   assign wb_dout_way_with_premux[0][135:0]  =  ic_sel_premux_data ? ic_premux_data_ext[135:0] : wb_dout_way[0][135:0] ;
   assign wb_dout_way_with_premux[1][135:0]  =  ic_sel_premux_data ? ic_premux_data_ext[135:0] : wb_dout_way[1][135:0] ;
   assign wb_dout_way_with_premux[2][135:0]  =  ic_sel_premux_data ? ic_premux_data_ext[135:0] : wb_dout_way[2][135:0] ;
   assign wb_dout_way_with_premux[3][135:0]  =  ic_sel_premux_data ? ic_premux_data_ext[135:0] : wb_dout_way[3][135:0] ;
   assign ic_rd_data[135:0]       = ({136{ic_rd_hit_q[0] | ic_sel_premux_data}} &  wb_dout_way_with_premux[0][135:0]) |
                                    ({136{ic_rd_hit_q[1] | ic_sel_premux_data}} &  wb_dout_way_with_premux[1][135:0]) |
                                    ({136{ic_rd_hit_q[2] | ic_sel_premux_data}} &  wb_dout_way_with_premux[2][135:0]) |
                                    ({136{ic_rd_hit_q[3] | ic_sel_premux_data}} &  wb_dout_way_with_premux[3][135:0]) ;
 endmodule
module IC_TAG #(parameter ICACHE_TAG_HIGH = 16 ,
                          ICACHE_TAG_LOW=6 ,
                          ICACHE_TAG_DEPTH=1024
                                        )
     (
      input logic free_clk,
      input logic clk,
      input logic rst_l,
      input logic clk_override,
      input logic dec_tlu_core_ecc_disable,
      input logic [31:3]  ic_rw_addr,
      input logic [3:0]   ic_wr_en,   
      input logic [3:0]   ic_tag_valid,
      input logic         ic_rd_en,
      input logic [ICACHE_TAG_HIGH-1:2]  ic_debug_addr,       
      input logic                        ic_debug_rd_en,      
      input logic                        ic_debug_wr_en,      
      input logic                        ic_debug_tag_array,  
      input logic [3:0]                  ic_debug_way,        
      output logic [20:0]  ictag_debug_rd_data,
      input  logic [33:0]  ic_debug_wr_data,    
      output logic [3:0]   ic_rd_hit,
      output logic         ic_tag_perr,
      input  logic         scan_mode
      ) ;
   logic [3:0] [20:0] ic_tag_data_raw;
   logic [3:0] [32:ICACHE_TAG_HIGH] w_tout;
   logic [20:0] ic_tag_wr_data ;
   logic [3:0]  ic_tag_way_perr ;
   logic [3:0]  ic_debug_rd_way_en ;
   logic [3:0]  ic_debug_rd_way_en_ff ;
   logic [ICACHE_TAG_HIGH-1:6]  ic_rw_addr_q;
   logic [31:4]         ic_rw_addr_ff;
   logic [3:0]          ic_tag_wren   ;  
   logic [3:0]          ic_tag_wren_q   ;  
   logic [3:0]          ic_tag_clk ;
   logic [3:0]          ic_tag_clken ;
   logic [3:0]          ic_debug_wr_way_en;    
   assign  ic_tag_wren [3:0]  = ic_wr_en[3:0] & {4{ic_rw_addr[5:3] == 3'b111}} ;
   assign  ic_tag_clken[3:0]  = {4{ic_rd_en | clk_override}} | ic_wr_en[3:0] | ic_debug_wr_way_en[3:0] | ic_debug_rd_way_en[3:0];
   rvdff #(32-ICACHE_TAG_HIGH) adr_ff (.*,
                    .clk(free_clk),
                    .din ({ic_rw_addr[31:ICACHE_TAG_HIGH]}),
                    .dout({ic_rw_addr_ff[31:ICACHE_TAG_HIGH]}));
   localparam TOP_BITS = 21+ICACHE_TAG_HIGH-33 ;
   localparam NUM_WAYS=4 ;
   assign  ic_debug_rd_way_en[3:0] =  {4{ic_debug_rd_en & ic_debug_tag_array}} & ic_debug_way[3:0] ;
   assign  ic_debug_wr_way_en[3:0] =  {4{ic_debug_wr_en & ic_debug_tag_array}} & ic_debug_way[3:0] ;
   assign  ic_tag_wren_q[3:0]  =  ic_tag_wren[3:0]          |
                                  ic_debug_wr_way_en[3:0]   ;
if (ICACHE_TAG_HIGH == 12) begin: SMALLEST
   logic   ic_tag_parity ;
           rveven_paritygen #(32-ICACHE_TAG_HIGH) pargen  (.data_in   (ic_rw_addr[31:ICACHE_TAG_HIGH]),
                                                 .parity_out(ic_tag_parity));
   assign  ic_tag_wr_data[20:0] = (ic_debug_wr_en & ic_debug_tag_array) ?
                                  {ic_debug_wr_data[32], ic_debug_wr_data[31:12]} :
                                  {ic_tag_parity, ic_rw_addr[31:ICACHE_TAG_HIGH]} ;
end else begin: OTHERS
   logic   ic_tag_parity ;
           rveven_paritygen #(32-ICACHE_TAG_HIGH) pargen  (.data_in   (ic_rw_addr[31:ICACHE_TAG_HIGH]),
                                                 .parity_out(ic_tag_parity));
   assign  ic_tag_wr_data[20:0] = (ic_debug_wr_en & ic_debug_tag_array) ?
                                  {ic_debug_wr_data[32], ic_debug_wr_data[31:12]} :
                                  {ic_tag_parity, {TOP_BITS{1'b0}},ic_rw_addr[31:ICACHE_TAG_HIGH]} ;
end
    assign ic_rw_addr_q[ICACHE_TAG_HIGH-1:6] = (ic_debug_rd_en | ic_debug_wr_en) ?
                                                ic_debug_addr[ICACHE_TAG_HIGH-1:6] :
                                                ic_rw_addr[ICACHE_TAG_HIGH-1:6] ;
   rvdff #(4) tag_rd_wy_ff (.*,
                    .clk(free_clk),
                    .din ({ic_debug_rd_way_en[3:0]}),
                    .dout({ic_debug_rd_way_en_ff[3:0]}));
   for (genvar i=0; i<NUM_WAYS; i++) begin: WAYS
      rvoclkhdr ic_tag_c1_cgc  ( .en(ic_tag_clken[i]), .l1clk(ic_tag_clk[i]), .* );
     if (ICACHE_TAG_DEPTH == 64 ) begin : ICACHE_SZ_16
         ram_64x21  ic_way_tag (
                                     .CLK(ic_tag_clk[i]),
                                     .WE (ic_tag_wren_q[i]),
                                     .D  (ic_tag_wr_data[20:0]),
                                     .ADR(ic_rw_addr_q[ICACHE_TAG_HIGH-1:ICACHE_TAG_LOW]),
                                     .Q  (ic_tag_data_raw[i][20:0])
                                    );
         assign w_tout[i][31:ICACHE_TAG_HIGH] = ic_tag_data_raw[i][31-ICACHE_TAG_HIGH:0] ;
         assign w_tout[i][32]                 = ic_tag_data_raw[i][20] ;
         rveven_paritycheck #(32-ICACHE_TAG_HIGH) parcheck(.data_in   (w_tout[i][31:ICACHE_TAG_HIGH]),
                                                   .parity_in (w_tout[i][32]),
                                                   .parity_err(ic_tag_way_perr[i]));
   end  
   else begin : tag_not_64
        ram_64x21  ic_way_tag (
                                     .CLK(ic_tag_clk[i]),
                                     .WE (ic_tag_wren_q[i]),
                                     .D  (ic_tag_wr_data[20:0]),
                                     .ADR(ic_rw_addr_q[ICACHE_TAG_HIGH-1:ICACHE_TAG_LOW]),
                                     .Q  ({ic_tag_data_raw[i][20:0]})
                                    );
         assign w_tout[i][31:ICACHE_TAG_HIGH] = ic_tag_data_raw[i][31-ICACHE_TAG_HIGH:0] ;
         assign w_tout[i][32]                 = ic_tag_data_raw[i][20] ;
       rveven_paritycheck #(32-ICACHE_TAG_HIGH) parcheck(.data_in   (w_tout[i][31:ICACHE_TAG_HIGH]),
                                                   .parity_in (w_tout[i][32]),
                                                   .parity_err(ic_tag_way_perr[i]));
   end  
end  
   assign ictag_debug_rd_data[20:0] =  ({21{ic_debug_rd_way_en_ff[0]}} &  ic_tag_data_raw[0] ) |
                                       ({21{ic_debug_rd_way_en_ff[1]}} &  ic_tag_data_raw[1] ) |
                                       ({21{ic_debug_rd_way_en_ff[2]}} &  ic_tag_data_raw[2] ) |
                                       ({21{ic_debug_rd_way_en_ff[3]}} &  ic_tag_data_raw[3] ) ;
   assign ic_rd_hit[0] = (w_tout[0][31:ICACHE_TAG_HIGH] == ic_rw_addr_ff[31:ICACHE_TAG_HIGH]) & ic_tag_valid[0];
   assign ic_rd_hit[1] = (w_tout[1][31:ICACHE_TAG_HIGH] == ic_rw_addr_ff[31:ICACHE_TAG_HIGH]) & ic_tag_valid[1];
   assign ic_rd_hit[2] = (w_tout[2][31:ICACHE_TAG_HIGH] == ic_rw_addr_ff[31:ICACHE_TAG_HIGH]) & ic_tag_valid[2];
   assign ic_rd_hit[3] = (w_tout[3][31:ICACHE_TAG_HIGH] == ic_rw_addr_ff[31:ICACHE_TAG_HIGH]) & ic_tag_valid[3];
   assign  ic_tag_perr  = | (ic_tag_way_perr[3:0] & ic_tag_valid[3:0] ) ;
endmodule
module ifu_mem_ctl
   import swerv_types::*;
(
   input logic clk,
   input logic free_clk,                                             
   input logic active_clk,                                           
   input logic rst_l,
   input logic                       exu_flush_final,                
   input logic                       dec_tlu_flush_err_wb,           
   input logic [31:1]                fetch_addr_f1,                  
   input logic                       ifc_fetch_uncacheable_f1,       
   input logic                       ifc_fetch_req_f1,               
   input logic                       ifc_fetch_req_f1_raw,           
   input logic                       ifc_iccm_access_f1,             
   input logic                       ifc_region_acc_fault_f1,        
   input logic                       ifc_dma_access_ok,              
   input logic                       dec_tlu_fence_i_wb,             
   input logic [16:6]                ifu_icache_error_index,         
   input logic                       ifu_icache_error_val,           
   input logic                       ifu_icache_sb_error_val,        
   input logic [7:1]                 ifu_bp_inst_mask_f2,            
   output logic                      ifu_miss_state_idle,            
   output logic                      ifu_ic_mb_empty,                
   output logic                      ic_dma_active  ,                
   output logic                      ic_write_stall,                 
   output logic                      ifu_pmu_ic_miss,                
   output logic                      ifu_pmu_ic_hit,                 
   output logic                      ifu_pmu_bus_error,              
   output logic                      ifu_pmu_bus_busy,               
   output logic                      ifu_pmu_bus_trxn,               
   output logic                           ifu_axi_awvalid,
   input  logic                           ifu_axi_awready,
   output logic [3-1:0]     ifu_axi_awid,
   output logic [31:0]                    ifu_axi_awaddr,
   output logic [3:0]                     ifu_axi_awregion,
   output logic [7:0]                     ifu_axi_awlen,
   output logic [2:0]                     ifu_axi_awsize,
   output logic [1:0]                     ifu_axi_awburst,
   output logic                           ifu_axi_awlock,
   output logic [3:0]                     ifu_axi_awcache,
   output logic [2:0]                     ifu_axi_awprot,
   output logic [3:0]                     ifu_axi_awqos,
   output logic                           ifu_axi_wvalid,
   input  logic                           ifu_axi_wready,
   output logic [63:0]                    ifu_axi_wdata,
   output logic [7:0]                     ifu_axi_wstrb,
   output logic                           ifu_axi_wlast,
   input  logic                           ifu_axi_bvalid,
   output logic                           ifu_axi_bready,
   input  logic [1:0]                     ifu_axi_bresp,
   input  logic [3-1:0]     ifu_axi_bid,
   output logic                           ifu_axi_arvalid,
   input  logic                           ifu_axi_arready,
   output logic [3-1:0]     ifu_axi_arid,
   output logic [31:0]                    ifu_axi_araddr,
   output logic [3:0]                     ifu_axi_arregion,
   output logic [7:0]                     ifu_axi_arlen,
   output logic [2:0]                     ifu_axi_arsize,
   output logic [1:0]                     ifu_axi_arburst,
   output logic                           ifu_axi_arlock,
   output logic [3:0]                     ifu_axi_arcache,
   output logic [2:0]                     ifu_axi_arprot,
   output logic [3:0]                     ifu_axi_arqos,
   input  logic                           ifu_axi_rvalid,
   output logic                           ifu_axi_rready,
   input  logic [3-1:0]     ifu_axi_rid,
   input  logic [63:0]                    ifu_axi_rdata,
   input  logic [1:0]                     ifu_axi_rresp,
   input  logic                           ifu_axi_rlast,
    input  logic                     ifu_bus_clk_en,
   input  logic                      dma_iccm_req,       
   input  logic [31:0]               dma_mem_addr,       
   input  logic [2:0]                dma_mem_sz,         
   input  logic                      dma_mem_write,      
   input  logic [63:0]               dma_mem_wdata,      
   output logic                      iccm_dma_ecc_error, 
   output logic                      iccm_dma_rvalid,    
   output logic [63:0]               iccm_dma_rdata,     
   output logic                      iccm_ready,         
   output logic [31:2]               ic_rw_addr,          
   output logic [3:0]                ic_wr_en,            
   output logic                      ic_rd_en,            
   output logic [67:0]               ic_wr_data,          
   input  logic [135:0]              ic_rd_data ,         
   input  logic [20:0]               ictag_debug_rd_data, 
   output logic [33:0]               ic_debug_wr_data,    
   output logic [33:0]               ifu_ic_debug_rd_data,        
   output logic [15:2]               ic_debug_addr,       
   output logic                      ic_debug_rd_en,      
   output logic                      ic_debug_wr_en,      
   output logic                      ic_debug_tag_array,  
   output logic [3:0]                ic_debug_way,        
   output logic [3:0]                ic_tag_valid,        
   input  logic [3:0]                ic_rd_hit,           
   input  logic                      ic_tag_perr,         
   output logic                      ic_hit_f2,               
   output logic                      ic_crit_wd_rdy,          
   output logic  [7:0]               ic_access_fault_f2,      
   output logic                      ic_rd_parity_final_err,  
   output logic                      iccm_rd_ecc_single_err,  
   output logic  [7:0]               iccm_rd_ecc_double_err,  
   output logic                      iccm_dma_sb_error,       
   output logic [7:0]                ic_fetch_val_f2,         
   output logic [127:0]              ic_data_f2,              
   output icache_err_pkt_t           ic_error_f2 ,            
   output logic                      ifu_icache_fetch_f2  ,
   output logic [127:0]              ic_premux_data,          
   output logic                      ic_sel_premux_data,      
   input  cache_debug_pkt_t          dec_tlu_ic_diag_pkt ,        
   input  logic                      dec_tlu_core_ecc_disable,    
   output logic                      ifu_ic_debug_rd_data_valid,  
   input  logic         scan_mode
   );
localparam TOTAL_INT        = 9;
localparam DCCM_BITS        = 16;
localparam DCCM_BANK_BITS   = 3;
localparam DCCM_NUM_BANKS   = 8;
localparam DCCM_DATA_WIDTH  = 32;
localparam DCCM_FDATA_WIDTH = 39;
localparam DCCM_BYTE_WIDTH  = 4;
localparam DCCM_ECC_WIDTH   = 7;
localparam LSU_RDBUF_DEPTH  = 8;
localparam DMA_BUF_DEPTH    = 4;
localparam LSU_STBUF_DEPTH  = 8;
localparam LSU_SB_BITS      = 16;
localparam DEC_INSTBUF_DEPTH = 4;
localparam ICCM_SIZE         = 512;
localparam ICCM_BITS         = 19;
localparam ICCM_NUM_BANKS    = 8;
localparam ICCM_BANK_BITS    = 3;
localparam ICCM_INDEX_BITS   = 14;
localparam ICCM_BANK_HI      = 4 + (3/4);
localparam ICACHE_TAG_HIGH  = 12;
localparam ICACHE_TAG_LOW   = 6;
localparam ICACHE_IC_DEPTH  = 8;
localparam ICACHE_TAG_DEPTH = 64;
localparam LSU_BUS_TAG     = 4;
localparam DMA_BUS_TAG     = 1;
localparam SB_BUS_TAG      = 1;
localparam IFU_BUS_TAG     = 3;
 localparam   NUM_OF_BEATS = 8 ;
   logic [31:3]    ifu_ic_req_addr_f2;
   logic           uncacheable_miss_in ;
   logic           uncacheable_miss_ff;
   logic           ifu_wr_en_new  ;
   logic           ifu_wr_en_new_q  ;
   logic [63:0]    ifu_wr_data_new ;
   logic           axi_ifu_wr_en_new  ;
   logic           axi_ifu_wr_en_new_q  ;
   logic           axi_ifu_wr_en_new_wo_err  ;
   logic [63:0]    axi_ifu_wr_data_new ;
   logic [3:0]     axi_ic_wr_en ;
   logic           reset_tag_valid_for_miss  ;
   logic [2:0]     way_status;
   logic [2:0]     way_status_mb_in;
   logic [2:0]     way_status_rep_new;
   logic [2:0]     way_status_mb_ff;
   logic [2:0]     way_status_new;
   logic [2:0]     way_status_hit_new;
   logic [2:0]     way_status_new_w_debug;
   logic [3:0]     tagv_mb_in;
   logic [3:0]     tagv_mb_ff;
   logic           ifu_wr_data_comb_err ;
   logic           ifu_wr_data_error;
   logic  [7:0]    ifu_byp_data_err;
   logic           ifu_wr_cumulative_err_data;
   logic           ifu_wr_cumulative_err;
   logic           ifu_wr_data_comb_err_ff;
   logic           write_even_beat;
   logic           ifc_dma_access_q_ok;
   logic           ifc_iccm_access_f2 ;
   logic           ifc_region_acc_fault_f2;
   logic  [7:0]    ifc_bus_acc_fault_f2;
   logic           ic_act_miss_f2;
   logic           ic_miss_under_miss_f2;
   logic           ic_act_hit_f2;
   logic           miss_pending;
   logic [31:1]    imb_in , imb_ff  ;
   logic           flush_final_f2;
   logic           ifc_fetch_req_f2;
   logic           ifc_fetch_req_f2_raw;
   logic           fetch_req_f2_qual   ;
   logic           ifc_fetch_req_qual_f1 ;
   logic [3:0]     replace_way_mb_any;
   logic           last_beat;
   logic           reset_beat_cnt  ;
   logic [2:0]     req_addr_count ;
   logic [5:3]     ic_req_addr_bits_5_3 ;
   logic [5:3]     ic_wr_addr_bits_5_3 ;
   logic [31:1]    ifu_fetch_addr_int_f2 ;
   logic [31:1]    ifu_ic_rw_int_addr ;
   logic           ic_crit_wd_rdy_in ;
   logic           crit_wd_byp_ok_ff ;
   logic           ic_crit_wd_rdy_ff;
   logic           ic_byp_hit_f2 ;
   logic           ic_valid ;
   logic           ic_valid_ff;
   logic           reset_all_tags;
   logic           ic_valid_w_debug;
   logic [3:0]     ifu_tag_wren,ifu_tag_wren_ff;
   logic [3:0]     ic_debug_tag_wr_en;
   logic [3:0]     ifu_tag_wren_w_debug;
   logic [3:0]     ic_debug_way_ff;
   logic           ic_debug_rd_en_ff   ;
   logic           write_bypass_data;
   logic           fetch_f1_f2_c1_clken ;
   logic           fetch_f1_f2_c1_clk;
   logic           debug_c1_clken;
   logic           debug_c1_clk;
   logic           reset_ic_in ;
   logic           reset_ic_ff ;
   logic [3:1]     vaddr_f2 ;
   logic [31:1]    ifu_status_wr_addr;
   logic           sel_fetch_u_miss;
   logic           sel_fetch_u_miss_ff;
   logic           sel_mb_addr ;
   logic           sel_mb_addr_ff ;
   logic [127:0]   ic_final_data;
   logic [ICACHE_TAG_HIGH-1:ICACHE_TAG_LOW] ifu_ic_rw_int_addr_ff ;
   logic [ICACHE_TAG_HIGH-1:ICACHE_TAG_LOW] ifu_status_wr_addr_ff ;
   logic [ICACHE_TAG_HIGH-1:ICACHE_TAG_LOW] ifu_ic_rw_int_addr_w_debug ;
   logic [ICACHE_TAG_HIGH-1:ICACHE_TAG_LOW] ifu_status_wr_addr_w_debug ;
   logic [2:0]                              way_status_new_ff ;
   logic                                    way_status_wr_en_ff ;
   logic [ICACHE_TAG_DEPTH-1:0][2:0]        way_status_out ;
   logic [1:0]                              ic_debug_way_enc;
   logic [127:0]                            ic_byp_data_only;
   logic [127:0]                            ic_rd_data_only;
   logic [3-1:0]             ifu_axi_rid_ff;
   logic         fetch_req_icache_f2;
   logic         fetch_req_iccm_f2;
   logic         ic_iccm_hit_f2;
   logic         fetch_uncacheable_ff;
   logic         way_status_wr_en;
   logic         sel_byp_data;
   logic         sel_ic_data;
   logic         sel_iccm_data;
   logic         ic_rd_parity_final_err_ff;
   logic         ic_act_miss_f2_delayed;
   logic         axi_ifu_wr_data_error;
   logic         way_status_wr_en_w_debug;
   logic         ic_debug_tag_val_rd_out;
   logic         ifu_pmu_ic_miss_in;
   logic         ifu_pmu_ic_hit_in;
   logic         ifu_pmu_bus_error_in;
   logic         ifu_pmu_bus_trxn_in;
   logic         ifu_pmu_bus_busy_in;
   logic         ic_debug_ict_array_sel_in;
   logic         ic_debug_ict_array_sel_ff;
   logic         debug_data_clk;
   logic         debug_data_clken;
   logic                   ifc_region_acc_fault_final_f1, ifc_region_acc_fault_memory, ifc_region_acc_okay;
   logic [3:0]   ic_wr_parity;
   assign ifu_axi_awvalid                        = '0;
   assign ifu_axi_awid[3-1:0]      = '0;
   assign ifu_axi_awaddr[31:0]                   = '0;
   assign ifu_axi_awsize[2:0]                    = '0;
   assign ifu_axi_awprot[2:0]                    = '0;
   assign ifu_axi_awcache[3:0]                   = '0;
   assign ifu_axi_awregion[3:0]                  = '0;
   assign ifu_axi_awlen[7:0]                     = '0;
   assign ifu_axi_awburst[1:0]                   = '0;
   assign ifu_axi_awqos[3:0]                     = '0;
   assign ifu_axi_awlock                         = '0;
   assign ifu_axi_wvalid                         = '0;
   assign ifu_axi_wdata[63:0]                    = '0;
   assign ifu_axi_wstrb[7:0]                     = '0;
   assign ifu_axi_wlast                          = '1;
   assign ifu_axi_bready                         = '1;
   assign fetch_f1_f2_c1_clken  = ifc_fetch_req_f1_raw | ifc_fetch_req_f2 | miss_pending | exu_flush_final ;
   assign debug_c1_clken        = ic_debug_rd_en | ic_debug_wr_en ;
   logic [ICCM_BITS-1:2]                iccm_ecc_corr_index_ff;
   logic [38:0]                         iccm_ecc_corr_data_ff;
   logic                                iccm_ecc_write_status     ;
   logic                                iccm_correct_ecc     ;
   logic                                iccm_rd_ecc_single_err_ff   ;
   logic                                perr_state_en;
   logic [7:0] fetch_mask, ic_fetch_mem_val, bp_mask, ic_bp_mem_mask, ic_fetch_val_mem_f2;
   logic                                dma_iccm_rd_req_f1;
   logic                                dma_iccm_rd_req_f2;
   logic [3:0]                          iccm_single_ecc_error;
   assign dma_iccm_rd_req_f1  = (dma_iccm_req & ~dma_mem_write) ;
   rvdff #(1)  dma_iccm_req_ff (.*, .clk(free_clk), .din (dma_iccm_rd_req_f1), .dout(dma_iccm_rd_req_f2));
   assign iccm_dma_sb_error  = (|iccm_single_ecc_error ) & dma_iccm_rd_req_f2;
   typedef enum logic [2:0] {ERR_IDLE=3'b000, PERR_WFF=3'b001 , ECC_WFF=3'b010 , ECC_CORR=3'b011, DMA_SB_ERR=3'b100} perr_state_t;
   perr_state_t perr_state, perr_nxtstate;
   assign ic_dma_active =  iccm_correct_ecc | (perr_state == DMA_SB_ERR) | (dec_tlu_flush_err_wb & (perr_state == ECC_WFF));
   logic   miss_state_en;
   typedef enum logic [2:0] {IDLE=3'b000, CRIT_BYP_OK=3'b001, HIT_U_MISS=3'b010, MISS_WAIT=3'b011,CRIT_WRD_RDY=3'b100,SCND_MISS=3'b101} miss_state_t;
   miss_state_t miss_state, miss_nxtstate;
   always_comb begin : MISS_SM
      miss_nxtstate   = IDLE;
      miss_state_en   = 1'b0;
      case (miss_state)
         IDLE: begin : idle
                  miss_nxtstate = (ic_act_miss_f2 & ~exu_flush_final) ? CRIT_BYP_OK : HIT_U_MISS ;
                  miss_state_en = ic_act_miss_f2;
         end
         CRIT_BYP_OK: begin : crit_byp_ok
                  miss_nxtstate = ( ic_byp_hit_f2 &  ~exu_flush_final & ~(ifu_wr_en_new & last_beat) & ~uncacheable_miss_ff) ? MISS_WAIT :
                                  ( ic_byp_hit_f2                                                    &  uncacheable_miss_ff) ? IDLE :
                                  (~ic_byp_hit_f2 &  ~exu_flush_final &  (ifu_wr_en_new & last_beat) &  uncacheable_miss_ff) ? CRIT_WRD_RDY :
                                  (                                      (ifu_wr_en_new & last_beat) & ~uncacheable_miss_ff) ? IDLE :
                                  (                   exu_flush_final & ~(ifu_wr_en_new & last_beat)                       ) ? HIT_U_MISS : IDLE;
                  miss_state_en =  exu_flush_final | ic_byp_hit_f2 | (ifu_wr_en_new & last_beat)  ;
         end
         CRIT_WRD_RDY: begin : crit_wrd_rdy
                  miss_nxtstate =  IDLE ;
                  miss_state_en =  exu_flush_final | flush_final_f2 | ic_byp_hit_f2   ;
         end
         MISS_WAIT: begin : miss_wait
                  miss_nxtstate =  (exu_flush_final & ~(ifu_wr_en_new & last_beat)) ? HIT_U_MISS  : IDLE ;
                  miss_state_en =   exu_flush_final | (ifu_wr_en_new & last_beat)  ;
         end
         HIT_U_MISS: begin : hit_u_miss
                  miss_nxtstate =  ic_miss_under_miss_f2 & ~(ifu_wr_en_new & last_beat) ? SCND_MISS :  IDLE  ;
                  miss_state_en = (ifu_wr_en_new & last_beat) | ic_miss_under_miss_f2;
         end
         SCND_MISS: begin : scnd_miss
                  miss_nxtstate =  IDLE  ;
                  miss_state_en = (ifu_wr_en_new & last_beat)  ;
         end
         default: begin : def_case
                  miss_nxtstate   = IDLE;
                  miss_state_en   = 1'b0;
         end
      endcase
   end
   rvdffs #(($bits(miss_state_t))) miss_state_ff (.clk(free_clk), .din(miss_nxtstate), .dout({miss_state}), .en(miss_state_en),   .*);
   logic    sel_hold_imb     ;
   assign miss_pending       =  (miss_state != IDLE) ;
   assign crit_wd_byp_ok_ff  =  (miss_state == CRIT_BYP_OK) | ((miss_state == CRIT_WRD_RDY) & ~flush_final_f2);
   assign sel_hold_imb       =  (miss_pending & ~(ifu_wr_en_new & last_beat) & ~((miss_state == CRIT_WRD_RDY) & exu_flush_final)) | ic_act_miss_f2 |
                                (miss_pending & (miss_nxtstate == CRIT_WRD_RDY)) ;
   assign ic_req_addr_bits_5_3[5:3] = req_addr_count[2:0] ;
   assign ic_wr_addr_bits_5_3[5:3]  = ifu_axi_rid_ff[2:0] ;
   assign fetch_req_icache_f2   = ifc_fetch_req_f2 & ~ifc_iccm_access_f2 & ~ifc_region_acc_fault_f2;
   assign fetch_req_iccm_f2     = ifc_fetch_req_f2 &  ifc_iccm_access_f2;
   assign ic_iccm_hit_f2        = fetch_req_iccm_f2  &  (~miss_pending | (miss_state==HIT_U_MISS));
   assign ic_byp_hit_f2         = ic_crit_wd_rdy  & fetch_req_icache_f2 &  miss_pending ;
   assign ic_act_hit_f2         = (|ic_rd_hit[3:0]) & fetch_req_icache_f2 & ~reset_all_tags & (~miss_pending | (miss_state==HIT_U_MISS)) & ~sel_mb_addr_ff;
   assign ic_act_miss_f2        = (~(|ic_rd_hit[3:0]) | reset_all_tags) & fetch_req_icache_f2 & ~miss_pending & ~ifc_region_acc_fault_f2;
   assign ic_miss_under_miss_f2 = (~(|ic_rd_hit[3:0]) | reset_all_tags) & fetch_req_icache_f2 & (miss_state == HIT_U_MISS) ;
   assign ic_hit_f2             =  ic_act_hit_f2 | ic_byp_hit_f2 | ic_iccm_hit_f2 | (ifc_region_acc_fault_f2 & ifc_fetch_req_f2 & ~((miss_state == CRIT_BYP_OK) | (miss_state == SCND_MISS)));
   assign uncacheable_miss_in   = sel_hold_imb ? uncacheable_miss_ff : ifc_fetch_uncacheable_f1 ;
   assign imb_in[31:1]          = sel_hold_imb ? imb_ff[31:1] : {fetch_addr_f1[31:1]} ;
   assign way_status_mb_in[2:0] = ( miss_pending) ? way_status_mb_ff[2:0] : {way_status[2:0]} ;
   assign tagv_mb_in[3:0]       = ( miss_pending) ? tagv_mb_ff[3:0]       : {ic_tag_valid[3:0]} ;
   assign reset_ic_in           = miss_pending  &  (reset_all_tags |  reset_ic_ff) ;
   rvdff #(1)  reset_ic_f (.*, .clk(free_clk), .din (reset_ic_in), .dout(reset_ic_ff));
   rvdff #(1)  uncache_ff (.*, .clk(active_clk), .din (ifc_fetch_uncacheable_f1), .dout(fetch_uncacheable_ff));
   rvdffe #(31) ifu_fetch_addr_f2_ff (.*,
                    .en (fetch_f1_f2_c1_clken),
                    .din ({fetch_addr_f1[31:1]}),
                    .dout({ifu_fetch_addr_int_f2[31:1]}));
   assign vaddr_f2[3:1] = ifu_fetch_addr_int_f2[3:1] ;
   rvdff_fpga #(1)  unc_miss_ff        (.*, .clk(fetch_f1_f2_c1_clk), .clken(fetch_f1_f2_c1_clken), .rawclk(clk), .din (uncacheable_miss_in),     .dout(uncacheable_miss_ff));
   rvdff_fpga #(3)  mb_rep_wayf2_ff    (.*, .clk(fetch_f1_f2_c1_clk), .clken(fetch_f1_f2_c1_clken), .rawclk(clk), .din ({way_status_mb_in[2:0]}), .dout({way_status_mb_ff[2:0]}));
   rvdff_fpga #(4)  mb_tagv_ff         (.*, .clk(fetch_f1_f2_c1_clk), .clken(fetch_f1_f2_c1_clken), .rawclk(clk), .din ({tagv_mb_in[3:0]}),       .dout({tagv_mb_ff[3:0]}));
   rvdff_fpga #(1) ifu_iccm_acc_ff     (.*, .clk(fetch_f1_f2_c1_clk), .clken(fetch_f1_f2_c1_clken), .rawclk(clk), .din(ifc_iccm_access_f1),       .dout(ifc_iccm_access_f2));
   rvdff_fpga #(1) ifu_iccm_reg_acc_ff (.*, .clk(fetch_f1_f2_c1_clk), .clken(fetch_f1_f2_c1_clken), .rawclk(clk), .din(ifc_region_acc_fault_final_f1), .dout(ifc_region_acc_fault_f2));
   rvdffe #(31) imb_f2_ff       (.*, .en(fetch_f1_f2_c1_clken), .din ({imb_in[31:1]}), .dout({imb_ff[31:1]}));
   assign ifc_fetch_req_qual_f1  = ifc_fetch_req_f1  & ~((miss_state == CRIT_WRD_RDY) & flush_final_f2) ; 
   rvdff #(1) fetch_req_f2_ff  (.*, .clk(active_clk),  .din(ifc_fetch_req_qual_f1), .dout(ifc_fetch_req_f2_raw));
   assign ifc_fetch_req_f2       = ifc_fetch_req_f2_raw & ~exu_flush_final ;
   assign ifu_ic_req_addr_f2[31:3] = {imb_ff[31:6] , ic_req_addr_bits_5_3[5:3] };
   assign ifu_ic_mb_empty          = ((miss_state == HIT_U_MISS) & ~(ifu_wr_en_new & last_beat)) |  ~miss_pending ;
   assign ifu_miss_state_idle      = (miss_state == IDLE) ;
   assign replace_way_mb_any[3] = ( way_status_mb_ff[2]  & way_status_mb_ff[0] & (&tagv_mb_ff[3:0])) |
                                  (~tagv_mb_ff[3]& tagv_mb_ff[2] &  tagv_mb_ff[1] &  tagv_mb_ff[0]) ;
   assign replace_way_mb_any[2] = (~way_status_mb_ff[2]  & way_status_mb_ff[0] & (&tagv_mb_ff[3:0])) |
                                  (~tagv_mb_ff[2]& tagv_mb_ff[1] &  tagv_mb_ff[0]) ;
   assign replace_way_mb_any[1] = ( way_status_mb_ff[1] & ~way_status_mb_ff[0] & (&tagv_mb_ff[3:0])) |
                                  (~tagv_mb_ff[1]& tagv_mb_ff[0] ) ;
   assign replace_way_mb_any[0] = (~way_status_mb_ff[1] & ~way_status_mb_ff[0] & (&tagv_mb_ff[3:0])) |
                                  (~tagv_mb_ff[0] ) ;
  assign way_status_hit_new[2:0] = ({3{ic_rd_hit[0]}} & {way_status[2] , 1'b1 , 1'b1}) |
                                   ({3{ic_rd_hit[1]}} & {way_status[2] , 1'b0 , 1'b1}) |
                                   ({3{ic_rd_hit[2]}} & {1'b1 ,way_status[1]  , 1'b0}) |
                                   ({3{ic_rd_hit[3]}} & {1'b0 ,way_status[1]  , 1'b0}) ;
  assign way_status_rep_new[2:0] = ({3{replace_way_mb_any[0]}} & {way_status_mb_ff[2] , 1'b1 , 1'b1}) |
                                   ({3{replace_way_mb_any[1]}} & {way_status_mb_ff[2] , 1'b0 , 1'b1}) |
                                   ({3{replace_way_mb_any[2]}} & {1'b1 ,way_status_mb_ff[1]  , 1'b0}) |
                                   ({3{replace_way_mb_any[3]}} & {1'b0 ,way_status_mb_ff[1]  , 1'b0}) ;
  assign way_status_new[2:0]     = (ifu_wr_en_new_q  )  ? way_status_rep_new[2:0] :
                                                          way_status_hit_new[2:0] ;
  assign way_status_wr_en  = (ifu_wr_en_new_q  ) | ic_act_hit_f2;
   assign sel_fetch_u_miss  =  ((miss_state == HIT_U_MISS) & ifc_fetch_req_f1 ) ;
   rvdff #(1) sel_f_u_m_ff (.*, .clk(free_clk), .din (sel_fetch_u_miss), .dout(sel_fetch_u_miss_ff));
   assign sel_mb_addr  = ((miss_pending & ifu_wr_en_new ) | reset_tag_valid_for_miss) ;
   assign ifu_ic_rw_int_addr[31:1] = ({31{ sel_mb_addr}}  &  {imb_ff[31:6] , ic_wr_addr_bits_5_3[5:3] , imb_ff[2:1]})  |
                                     ({31{~sel_mb_addr}}  &  fetch_addr_f1[31:1] )   ;
   assign ifu_status_wr_addr[31:1] = ({31{ sel_mb_addr}}  &  {imb_ff[31:6] , ic_wr_addr_bits_5_3[5:3] , imb_ff[2:1]})  |
                                     ({31{~sel_mb_addr}}  &  ifu_fetch_addr_int_f2[31:1] )   ;
   rvdff #(1) sel_mb_addr_flop (.*, .clk(free_clk), .din({sel_mb_addr}), .dout({sel_mb_addr_ff}));
  assign ic_rw_addr[31:2]      = ifu_ic_rw_int_addr[31:2] ;
genvar i ;
for (i=0 ; i < 4 ; i++) begin : DATA_PGEN
       rveven_paritygen #(16) parlo  (.data_in   (ifu_wr_data_new[((16*i)+15):(16*i)]),
                                      .parity_out(ic_wr_parity[i]));
end
  assign ifu_wr_data_comb_err       =  ifu_wr_data_error ;
  assign ifu_wr_cumulative_err      = (ifu_wr_data_comb_err | ifu_wr_data_comb_err_ff) & ~reset_beat_cnt;
  assign ifu_wr_cumulative_err_data =  ifu_wr_data_comb_err | ifu_wr_data_comb_err_ff ;
  rvdff #(1) cumul_err_ff (.*, .clk(free_clk),  .din (ifu_wr_cumulative_err), .dout(ifu_wr_data_comb_err_ff));
   assign ic_rd_data_only[127:0]  = {ic_rd_data [133:102], ic_rd_data [99:68] , ic_rd_data [65:34] ,ic_rd_data [31:0]} ;
   assign ic_error_f2.parity[7:0] = {ic_rd_data[135:134] , ic_rd_data[101:100], ic_rd_data [67:66] ,ic_rd_data [33:32]};
   assign ic_wr_data[67:0] = {ic_wr_parity[3:2],
                              ifu_wr_data_new[63:32],
                              ic_wr_parity[1:0],
                              ifu_wr_data_new[31:0]} ;
  assign sel_byp_data     =  ic_crit_wd_rdy  ; 
  assign sel_ic_data      = ~ic_crit_wd_rdy & ~fetch_req_iccm_f2 ;
    assign ic_final_data  = ({128{sel_byp_data | sel_ic_data}} & {ic_rd_data_only[127:0]} ) ;
  assign ic_premux_data = ({128{sel_byp_data }} & {ic_byp_data_only[127:0]} ) ;
  assign ic_sel_premux_data =  sel_byp_data ;
 assign ifu_icache_fetch_f2   =  sel_ic_data ;
  assign ifc_bus_acc_fault_f2[7:0]   =  {8{ic_byp_hit_f2}} & ifu_byp_data_err[7:0] ;
  assign ic_data_f2[127:0]      =  ic_final_data[127:0];
rvdff #(1) flush_final_ff (.*, .clk(free_clk), .din({exu_flush_final}), .dout({flush_final_f2}));
assign fetch_req_f2_qual       = ic_hit_f2 & ~exu_flush_final;
assign ic_access_fault_f2[7:0]  = ({8{ifc_region_acc_fault_f2}} | ifc_bus_acc_fault_f2[7:0])  & {8{~exu_flush_final}};
assign ic_fetch_val_f2[7] = fetch_req_f2_qual & ifu_bp_inst_mask_f2[7] & ((!vaddr_f2[3]&!vaddr_f2[2]&!vaddr_f2[1]));
assign ic_fetch_val_f2[6] = fetch_req_f2_qual & ifu_bp_inst_mask_f2[6] & ((!vaddr_f2[3]&!vaddr_f2[2]));
assign ic_fetch_val_f2[5] = fetch_req_f2_qual & ifu_bp_inst_mask_f2[5] & ((!vaddr_f2[3]&!vaddr_f2[1]) | (!vaddr_f2[3]&!vaddr_f2[2]));
assign ic_fetch_val_f2[4] = fetch_req_f2_qual & ifu_bp_inst_mask_f2[4] & ((!vaddr_f2[3]));
assign ic_fetch_val_f2[3] = fetch_req_f2_qual & ifu_bp_inst_mask_f2[3] & ((!vaddr_f2[2]&!vaddr_f2[1]) | (!vaddr_f2[3]));
assign ic_fetch_val_f2[2] = fetch_req_f2_qual & ifu_bp_inst_mask_f2[2] & ((!vaddr_f2[2]) | (!vaddr_f2[3]));
assign ic_fetch_val_f2[1] = fetch_req_f2_qual & ifu_bp_inst_mask_f2[1] & ((!vaddr_f2[1]) | (!vaddr_f2[2]) | (!vaddr_f2[3])) ;
assign ic_fetch_val_f2[0] = fetch_req_f2_qual ;
   assign fetch_mask[7:0] = {vaddr_f2[3:1]==3'b111,vaddr_f2[3:1]==3'b110,vaddr_f2[3:1]==3'b101,vaddr_f2[3:1]==3'b100,vaddr_f2[3:1]==3'b011,vaddr_f2[3:1]==3'b010,vaddr_f2[3:1]==3'b001,vaddr_f2[3:1]==3'b000};
   assign ic_fetch_mem_val[7:0] = { 1'b1, |fetch_mask[6:0], |fetch_mask[5:0], |fetch_mask[4:0], |fetch_mask[3:0], |fetch_mask[2:0], |fetch_mask[1:0], fetch_mask[0] };
   assign bp_mask[7:0] = {ifu_bp_inst_mask_f2[7:1], 1'b1};
   assign ic_bp_mem_mask[7:0] = ({8{fetch_mask[0]}} &  bp_mask[7:0]) |             
                                ({8{fetch_mask[1]}} & {bp_mask[6:0],1'b0}) |
                                ({8{fetch_mask[2]}} & {bp_mask[5:0],2'b0}) |
                                ({8{fetch_mask[3]}} & {bp_mask[4:0],3'b0}) |
                                ({8{fetch_mask[4]}} & {bp_mask[3:0],4'b0}) |
                                ({8{fetch_mask[5]}} & {bp_mask[2:0],5'b0}) |
                                ({8{fetch_mask[6]}} & {bp_mask[1:0],6'b0}) |
                                ({8{fetch_mask[7]}} & {bp_mask[0]  ,7'b0});
   assign ic_fetch_val_mem_f2[7:0] = {8{fetch_req_f2_qual}} & ic_bp_mem_mask[7:0] & ic_fetch_mem_val[7:0];
  logic                   write_byp_first_data ;
  logic                   write_byp_second_data ;
  logic [63:0]            ifu_byp_data_first_half;
  logic [63:0]            ifu_byp_data_second_half;
  logic                   ifu_byp_data_error_first_half_in;
  logic                   ifu_byp_data_error_first_half;
  logic                   ifu_byp_data_error_second_half_in;
  logic                   ifu_byp_data_error_second_half;
  logic                   ifu_byp_data_first_half_valid_in ;
  logic                   ifu_byp_data_first_half_valid    ;
  logic                   ifu_byp_data_second_half_valid_in ;
  logic                   ifu_byp_data_second_half_valid    ;
  logic                   ic_crit_wd_complete    ;
  logic [IFU_BUS_TAG-1:0] byp_tag_ff;
  logic                   byp_data_first_c1_clken ;
  logic                   byp_data_first_c1_clk;
  logic                   byp_data_second_c1_clken ;
  logic                   byp_data_second_c1_clk;
  assign   byp_data_first_c1_clken  = write_byp_first_data;
  assign   byp_data_second_c1_clken = write_byp_second_data;
  assign byp_tag_ff[IFU_BUS_TAG-1:0]  =  IFU_BUS_TAG'({imb_ff[5:4] , 1'b0});
  assign write_byp_first_data         =   axi_ifu_wr_en_new & ({byp_tag_ff[IFU_BUS_TAG-1:1],1'b0}  == ifu_axi_rid_ff[IFU_BUS_TAG-1:0]);
  assign write_byp_second_data        =   axi_ifu_wr_en_new & ({byp_tag_ff[IFU_BUS_TAG-1:1],1'b1}  == ifu_axi_rid_ff[IFU_BUS_TAG-1:0]);
  rvdffe #(64) byp_data_first_half (.*,
            .en(byp_data_first_c1_clken),
            .din (ifu_wr_data_new[63:0]),
            .dout(ifu_byp_data_first_half[63:0]));
  assign ifu_byp_data_error_first_half_in = write_byp_first_data ? ifu_wr_data_error : (ifu_byp_data_error_first_half  & ~ic_act_miss_f2) ;
  rvdff #(1) byp_data_first_half_err (.*,
            .clk(free_clk),
            .din (ifu_byp_data_error_first_half_in),
            .dout(ifu_byp_data_error_first_half));
  assign ifu_byp_data_first_half_valid_in = write_byp_first_data ? 1'b1  : (ifu_byp_data_first_half_valid  & ~ic_act_miss_f2) ;
  rvdff #(1) byp_data_first_half_val (.*,
            .clk(free_clk),
            .din (ifu_byp_data_first_half_valid_in),
            .dout(ifu_byp_data_first_half_valid));
  rvdffe #(64) byp_data_second_half (.*,
            .en(byp_data_second_c1_clken),
            .din (ifu_wr_data_new[63:0]),
            .dout(ifu_byp_data_second_half[63:0]));
  assign ifu_byp_data_error_second_half_in = write_byp_second_data ? ifu_wr_data_error : (ifu_byp_data_error_second_half  & ~ic_act_miss_f2) ;
  rvdff #(1) byp_data_second_half_err (.*,
                   .clk(free_clk),
                   .din (ifu_byp_data_error_second_half_in),
                   .dout(ifu_byp_data_error_second_half));
  assign ifu_byp_data_second_half_valid_in = write_byp_second_data ? 1'b1  : (ifu_byp_data_second_half_valid  & ~ic_act_miss_f2) ;
  rvdff #(1) byp_data_second_half_val (.*,
                   .clk(free_clk),
                    .din (ifu_byp_data_second_half_valid_in),
                    .dout(ifu_byp_data_second_half_valid));
  assign ic_byp_data_only[127:0] = { ifu_byp_data_second_half[63:0] , ifu_byp_data_first_half[63:0] } ;
  assign ifu_byp_data_err[7:0]  =  {{4{ ifu_byp_data_error_second_half}} , {4{ifu_byp_data_error_first_half}}} ;
   assign    ic_crit_wd_complete = (write_byp_first_data  & ifu_byp_data_second_half_valid) |
                                   (write_byp_second_data & ifu_byp_data_first_half_valid)  ;
   assign    ic_crit_wd_rdy_in = (ic_crit_wd_complete & crit_wd_byp_ok_ff & ~exu_flush_final ) |
                                 (ic_crit_wd_rdy_ff & ~fetch_req_icache_f2 & crit_wd_byp_ok_ff & ~exu_flush_final) ;
   rvdff #(1)           crit_wd_ff      (.*, .clk(free_clk),  .din(ic_crit_wd_rdy_in),   .dout(ic_crit_wd_rdy_ff));
assign ic_rd_parity_final_err = ic_tag_perr & ifu_icache_fetch_f2  ;
logic [16:6]                                  ifu_ic_rw_int_addr_f2_Q ;
logic [3:0]                                   perr_err_inv_way;
logic [16:6]                                  perr_ic_index_ff;
logic                                         perr_sel_invalidate;
logic                                         perr_sb_write_status   ;
logic                                         ifu_icache_sb_error_val_ff   ;
         rvdff #(11) ic_index_q (.*,
                   .clk(active_clk),
                   .din(ifu_icache_error_index[16:6]),
                   .dout(ifu_ic_rw_int_addr_f2_Q[16:6]));
   rvdff  #((1))  perr_err_ff    (.clk(active_clk), .din(ifu_icache_error_val),   .dout(ic_rd_parity_final_err_ff),   .*);
   rvdff  #((1))  sbiccm_err_ff  (.clk(active_clk), .din(ifu_icache_sb_error_val),       .dout(ifu_icache_sb_error_val_ff),   .*);
   rvdffs #((11)) perr_dat_ff    (.clk(active_clk), .din(ifu_ic_rw_int_addr_f2_Q[16:6]), .dout(perr_ic_index_ff), .en(perr_sb_write_status),  .*);
   assign perr_err_inv_way[3:0]   =  {4{perr_sel_invalidate}} ;
   assign iccm_correct_ecc = (perr_state == ECC_CORR);
   always_comb begin  : ERROR_SM
      perr_nxtstate            = ERR_IDLE;
      perr_state_en            = 1'b0;
      perr_sb_write_status     = 1'b0;
      perr_sel_invalidate      = 1'b0;
      case (perr_state)
         ERR_IDLE: begin : err_idle
                  perr_nxtstate         =  iccm_dma_sb_error ? DMA_SB_ERR : (ic_rd_parity_final_err_ff & ~exu_flush_final) ? PERR_WFF : ECC_WFF;
                  perr_state_en         =  ((ic_rd_parity_final_err_ff | ifu_icache_sb_error_val_ff) & ~exu_flush_final) | iccm_dma_sb_error;
                  perr_sb_write_status  =  perr_state_en;
         end
         PERR_WFF: begin : perr_wff
                  perr_nxtstate       =  ERR_IDLE ;
                  perr_state_en       =   exu_flush_final  ;
                  perr_sel_invalidate =  (dec_tlu_flush_err_wb &  exu_flush_final);
         end
         ECC_WFF: begin : ecc_wff
                  perr_nxtstate       =  (~dec_tlu_flush_err_wb &  exu_flush_final ) ? ERR_IDLE : ECC_CORR ;
                  perr_state_en       =   exu_flush_final   ;
         end
         DMA_SB_ERR : begin : dma_sb_ecc
                 perr_nxtstate       = ECC_CORR;
                 perr_state_en       = 1'b1;
         end
         ECC_CORR: begin : ecc_corr
                  perr_nxtstate       =  ERR_IDLE  ;
                  perr_state_en       =   1'b1   ;
         end
         default: begin : def_case
                  perr_nxtstate            = ERR_IDLE;
                  perr_state_en            = 1'b0;
                  perr_sb_write_status     = 1'b0;
                  perr_sel_invalidate      = 1'b0;
         end
      endcase
   end
   rvdffs #(($bits(perr_state_t))) perr_state_ff (.clk(free_clk), .din(perr_nxtstate), .dout({perr_state}), .en(perr_state_en),   .*);
assign iccm_rd_ecc_single_err     = 1'b0 ;
assign iccm_rd_ecc_double_err     = '0 ;
assign iccm_rd_ecc_single_err_ff  = 1'b0 ;
assign iccm_ecc_corr_index_ff[ICCM_BITS-1:2]  =  '0;
assign iccm_ecc_corr_data_ff[38:0]            =  '0;
assign iccm_ecc_write_status                  =  '0;
assign iccm_single_ecc_error                  =  '0;
   logic        axiclk;
   logic        axiclk_reset;
   logic        axi_ifu_bus_clk_en_ff;
   logic        axi_ifu_bus_clk_en ;
   logic        ifc_axi_ic_req_ff_in;
   logic        ifc_axi_ic_req_ff2 ;
   logic        axi_inc_data_beat_cnt     ;
   logic        axi_reset_data_beat_cnt   ;
   logic        axi_hold_data_beat_cnt    ;
   logic        axi_inc_cmd_beat_cnt     ;
   logic        axi_reset_cmd_beat_cnt_0   ;
   logic        axi_reset_cmd_beat_cnt_6   ;
   logic        axi_hold_cmd_beat_cnt    ;
   logic [2:0]  axi_new_data_beat_count  ;
   logic [2:0]  axi_data_beat_count      ;
   logic [2:0]  axi_new_cmd_beat_count  ;
   logic [2:0]  axi_cmd_beat_count      ;
   logic        axi_inc_rd_addr_cnt  ;
   logic        axi_set_rd_addr_cnt  ;
   logic        axi_reset_rd_addr_cnt;
   logic        axi_hold_rd_addr_cnt ;
   logic [2:0]  axi_new_rd_addr_count;
   logic [2:0]  axi_rd_addr_count;
   logic        axi_cmd_sent           ;
   logic        axi_last_data_beat     ;
   logic        axi_wrap_addr          ;
   logic        ifu_axi_rvalid_ff        ;
   logic        ifu_axi_rvalid_unq_ff    ;
   logic        ifu_axi_arready_unq_ff    ;
   logic        ifu_axi_arvalid_ff        ;
   logic        ifu_axi_arready_ff        ;
   logic [63:0] ifu_axi_rdata_ff        ;
   logic [1:0]  ifu_axi_rresp_ff          ;
   logic        axi_w0_wren            ;
   logic        axi_w1_wren            ;
   logic        axi_w2_wren            ;
   logic        axi_w3_wren            ;
   logic        axi_w0_wren_last       ;
   logic        axi_w1_wren_last       ;
   logic        axi_w2_wren_last       ;
   logic        axi_w3_wren_last       ;
   logic        w0_wren_reset_miss      ;
   logic        w1_wren_reset_miss      ;
   logic        w2_wren_reset_miss      ;
   logic        w3_wren_reset_miss      ;
   logic        ifc_dma_access_ok_d;
   logic        ifc_dma_access_ok_prev;
assign axi_ifu_bus_clk_en =  ifu_bus_clk_en ;
   rvdff #(1)           axi_clken_ff  (.*, .clk(free_clk), .din(axi_ifu_bus_clk_en), .dout(axi_ifu_bus_clk_en_ff));
   logic   axi_cmd_req_in ;
   logic   axi_cmd_req_hold ;
   assign  ifc_axi_ic_req_ff_in  = (ic_act_miss_f2 | axi_cmd_req_hold | ifc_axi_ic_req_ff2) & ~((axi_cmd_beat_count==3'b111) & ifu_axi_arvalid & ifu_axi_arready & miss_pending);
   assign    axi_cmd_req_in  = (ic_act_miss_f2 | axi_cmd_req_hold) & ~axi_cmd_sent  ;  
   rvdff #(1)  axi_cmd_req_ff  (.*,  .clk(free_clk), .din(axi_cmd_req_in), .dout(axi_cmd_req_hold));
   assign    ifu_axi_arvalid                 = ifc_axi_ic_req_ff2 ;
   assign    ifu_axi_arid[IFU_BUS_TAG-1:0]   = IFU_BUS_TAG'(axi_rd_addr_count[2:0]);
   assign    ifu_axi_araddr[31:0]            = {ifu_ic_req_addr_f2[31:3],3'b0} ;
   assign    ifu_axi_rready                  = 1'b1;
   assign    ifu_axi_arsize[2:0]             = 3'b011;
   assign    ifu_axi_arcache[3:0]            = 4'b1111;
   assign    ifu_axi_arprot[2:0]             = 3'b100;
   assign    ifu_axi_arregion[3:0]           = ifu_axi_araddr[31:28];
   assign    ifu_axi_arlen[7:0]              = '0;
   assign    ifu_axi_arburst[1:0]            = 2'b01;
   assign    ifu_axi_arqos[3:0]              = '0;
   assign    ifu_axi_arlock                  = '0;
   rvdff_fpga #(1)            axi_ic_req_ff2   (.*, .clk(axiclk), .clken(axi_ifu_bus_clk_en), .rawclk(clk), .din(ifc_axi_ic_req_ff_in),          .dout(ifc_axi_ic_req_ff2));
   rvdff_fpga #(1)            axi_rdy_ff       (.*, .clk(axiclk), .clken(axi_ifu_bus_clk_en), .rawclk(clk), .din(ifu_axi_arready),               .dout(ifu_axi_arready_unq_ff));
   rvdff_fpga #(1)            axi_rsp_vld_ff   (.*, .clk(axiclk), .clken(axi_ifu_bus_clk_en), .rawclk(clk), .din(ifu_axi_rvalid),                .dout(ifu_axi_rvalid_unq_ff));
   rvdff_fpga #(1)            axi_cmd_ff       (.*, .clk(axiclk), .clken(axi_ifu_bus_clk_en), .rawclk(clk), .din(ifu_axi_arvalid),               .dout(ifu_axi_arvalid_ff));
   rvdff_fpga #(2)            scvi_rsp_cmd_ff  (.*, .clk(axiclk), .clken(axi_ifu_bus_clk_en), .rawclk(clk), .din(ifu_axi_rresp[1:0]),            .dout(ifu_axi_rresp_ff[1:0]));
   rvdff_fpga #(IFU_BUS_TAG)  scvi_rsp_tag_ff  (.*, .clk(axiclk), .clken(axi_ifu_bus_clk_en), .rawclk(clk), .din(ifu_axi_rid[IFU_BUS_TAG-1:0]),  .dout(ifu_axi_rid_ff[IFU_BUS_TAG-1:0]));
   rvdff_fpga #(64)           axi_data_ff      (.*, .clk(axiclk), .clken(axi_ifu_bus_clk_en), .rawclk(clk), .din(ifu_axi_rdata[63:0]),           .dout(ifu_axi_rdata_ff[63:0]));
   assign    ifu_axi_arready_ff   =  ifu_axi_arready_unq_ff & axi_ifu_bus_clk_en_ff ;
   assign    ifu_axi_rvalid_ff   =  ifu_axi_rvalid_unq_ff & axi_ifu_bus_clk_en_ff ;
   assign    axi_cmd_sent        =  ifu_axi_arvalid & ifu_axi_arready & miss_pending & axi_ifu_bus_clk_en;
   assign axi_inc_data_beat_cnt         = (axi_ifu_wr_en_new & ~axi_last_data_beat) ;
   assign axi_reset_data_beat_cnt       =  ic_act_miss_f2 | (axi_ifu_wr_en_new &  axi_last_data_beat) ;
   assign axi_hold_data_beat_cnt        = ~axi_inc_data_beat_cnt & ~axi_reset_data_beat_cnt ;
   assign axi_new_data_beat_count[2:0] = ({3{axi_reset_data_beat_cnt}} & 3'b000 ) |
                                     ({3{axi_inc_data_beat_cnt}}   & (axi_data_beat_count[2:0] + 3'b001)) |
                                     ({3{axi_hold_data_beat_cnt}}  &  axi_data_beat_count[2:0]) ;
   rvdff #(3)  axi_mb_beat_count_ff (.*, .clk(free_clk), .din ({axi_new_data_beat_count[2:0]}), .dout({axi_data_beat_count[2:0]}));
   assign axi_inc_rd_addr_cnt     = axi_cmd_sent;
   assign axi_set_rd_addr_cnt     = ic_act_miss_f2  ;
   assign axi_hold_rd_addr_cnt    = ~axi_inc_rd_addr_cnt & ~axi_set_rd_addr_cnt;
   assign axi_new_rd_addr_count[2:0] = ~miss_pending ? {imb_ff[5:4],1'b0} :  axi_inc_rd_addr_cnt ? (axi_rd_addr_count[2:0] + 3'b001) : axi_rd_addr_count[2:0];
   rvdff_fpga  #(3)  axi_cmd_beat_ff (.*,           .clk(axiclk_reset), .clken(axi_ifu_bus_clk_en | ic_act_miss_f2), .rawclk(clk), .din ({axi_new_cmd_beat_count[2:0]}), .dout({axi_cmd_beat_count[2:0]}));
   rvdffs_fpga #(3)  axi_rd_addr_ff (.*, .en(1'b1), .clk(axiclk_reset), .clken(axi_ifu_bus_clk_en | ic_act_miss_f2), .rawclk(clk), .din ({axi_new_rd_addr_count[2:0]}), .dout({axi_rd_addr_count[2:0]}));
   assign axi_inc_cmd_beat_cnt     =  ifu_axi_arvalid         &  ifu_axi_arready & miss_pending;
   assign axi_reset_cmd_beat_cnt_0 =  ic_act_miss_f2        & ~uncacheable_miss_in ;
   assign axi_reset_cmd_beat_cnt_6 =  ic_act_miss_f2        &  uncacheable_miss_in ;
   assign axi_hold_cmd_beat_cnt    = ~axi_inc_cmd_beat_cnt & ~ic_act_miss_f2 ;
   assign axi_new_cmd_beat_count[2:0] =  ({3{axi_reset_cmd_beat_cnt_0}} & 3'b000 ) |
                                          ({3{axi_reset_cmd_beat_cnt_6}} & 3'b110 ) |
                                          ({3{axi_inc_cmd_beat_cnt}}   & (axi_cmd_beat_count[2:0] + 3'b001)) |
                                          ({3{axi_hold_cmd_beat_cnt}}  &  axi_cmd_beat_count[2:0]) ;
   assign    req_addr_count[2:0]    = axi_rd_addr_count[2:0] ;
    assign axi_last_data_beat     =  uncacheable_miss_ff ? (axi_data_beat_count[2:0] == 3'b001) : (axi_data_beat_count[2:0] == 3'b111);
    assign axi_wrap_addr          =  (axi_rd_addr_count[2:0] == 3'b111);
   assign  axi_ifu_wr_en_new         =  ifu_axi_rvalid_ff  & miss_pending ;
   assign  axi_ifu_wr_en_new_q       =  ifu_axi_rvalid_ff  & miss_pending & ~uncacheable_miss_ff & ~(|ifu_axi_rresp_ff[1:0]);  
   assign  axi_ifu_wr_en_new_wo_err  =  ifu_axi_rvalid_ff & miss_pending &  ~uncacheable_miss_ff;
   assign  axi_ifu_wr_data_new[63:0] =  ifu_axi_rdata_ff[63:0] ;
   assign axi_w0_wren = axi_ifu_wr_en_new_q & (replace_way_mb_any[3:0] == 4'b0001) & miss_pending ;
   assign axi_w1_wren = axi_ifu_wr_en_new_q & (replace_way_mb_any[3:0] == 4'b0010) & miss_pending ;
   assign axi_w2_wren = axi_ifu_wr_en_new_q & (replace_way_mb_any[3:0] == 4'b0100) & miss_pending ;
   assign axi_w3_wren = axi_ifu_wr_en_new_q & (replace_way_mb_any[3:0] == 4'b1000) & miss_pending ;
   assign axi_ic_wr_en[3:0] = {axi_w3_wren , axi_w2_wren , axi_w1_wren , axi_w0_wren} ;
   assign axi_w0_wren_last = axi_ifu_wr_en_new_wo_err & (replace_way_mb_any[3:0] == 4'b0001) & miss_pending & axi_last_data_beat;
   assign axi_w1_wren_last = axi_ifu_wr_en_new_wo_err & (replace_way_mb_any[3:0] == 4'b0010) & miss_pending & axi_last_data_beat;
   assign axi_w2_wren_last = axi_ifu_wr_en_new_wo_err & (replace_way_mb_any[3:0] == 4'b0100) & miss_pending & axi_last_data_beat;
   assign axi_w3_wren_last = axi_ifu_wr_en_new_wo_err & (replace_way_mb_any[3:0] == 4'b1000) & miss_pending & axi_last_data_beat;
   rvdff #(1)  act_miss_ff (.*, .clk(free_clk), .din (ic_act_miss_f2), .dout(ic_act_miss_f2_delayed));
   assign reset_tag_valid_for_miss = ic_act_miss_f2_delayed & (miss_state == CRIT_BYP_OK) ;
   assign w0_wren_reset_miss    = (replace_way_mb_any[3:0] == 4'b0001) & reset_tag_valid_for_miss ;
   assign w1_wren_reset_miss    = (replace_way_mb_any[3:0] == 4'b0010) & reset_tag_valid_for_miss ;
   assign w2_wren_reset_miss    = (replace_way_mb_any[3:0] == 4'b0100) & reset_tag_valid_for_miss ;
   assign w3_wren_reset_miss    = (replace_way_mb_any[3:0] == 4'b1000) & reset_tag_valid_for_miss ;
   assign    axi_ifu_wr_data_error = |ifu_axi_rresp_ff[1:0] &  ifu_axi_rvalid_ff  & miss_pending;
   rvdff #(1)           dma_ok_prev_ff  (.*, .clk(free_clk),  .din(ifc_dma_access_ok_d), .dout(ifc_dma_access_ok_prev));
   assign ic_crit_wd_rdy   = ic_crit_wd_rdy_ff ;
   assign last_beat        =  axi_last_data_beat ;
   assign ifu_wr_data_error = axi_ifu_wr_data_error ;
   assign reset_beat_cnt    = axi_reset_data_beat_cnt ;
   assign ifc_dma_access_ok_d  = ifc_dma_access_ok &  ~iccm_correct_ecc;
   assign ifc_dma_access_q_ok  = ifc_dma_access_ok &  ~iccm_correct_ecc & ifc_dma_access_ok_prev &  (perr_state == ERR_IDLE) ;
   assign iccm_ready           = ifc_dma_access_q_ok ;
         assign iccm_dma_rvalid = 1'b0 ;
         assign iccm_dma_ecc_error = 1'b0 ;
         assign iccm_dma_rdata[63:0] = '0 ;
assign   ic_rd_en    = ifc_fetch_req_f1 & ~ifc_fetch_uncacheable_f1;
assign ifu_tag_wren[0] = axi_w0_wren_last | w0_wren_reset_miss;
assign ifu_tag_wren[1] = axi_w1_wren_last | w1_wren_reset_miss;
assign ifu_tag_wren[2] = axi_w2_wren_last | w2_wren_reset_miss;
assign ifu_tag_wren[3] = axi_w3_wren_last | w3_wren_reset_miss;
assign ifu_wr_en_new   = axi_ifu_wr_en_new;
assign ifu_wr_en_new_q = axi_ifu_wr_en_new_q;
assign ifu_wr_data_new[63:0]=  axi_ifu_wr_data_new[63:0];
assign ic_wr_en[3:0]   =  axi_ic_wr_en[3:0];
assign ic_write_stall  = ifu_wr_en_new &  ~(((miss_state== CRIT_BYP_OK) & ~(ifu_wr_en_new & last_beat)));
   rvdff #(1) reset_all_tag_ff (.*, .clk(active_clk),  .din(dec_tlu_fence_i_wb), .dout(reset_all_tags));
   assign  ic_valid  = ~ifu_wr_cumulative_err_data & ~(reset_ic_in | reset_ic_ff) & ~reset_tag_valid_for_miss;
   assign ifu_status_wr_addr_w_debug[ICACHE_TAG_HIGH-1:ICACHE_TAG_LOW] = ((ic_debug_rd_en | ic_debug_wr_en ) & ic_debug_tag_array) ?
                                                                           ic_debug_addr[ICACHE_TAG_HIGH-1:ICACHE_TAG_LOW] :
                                                                           ifu_status_wr_addr[ICACHE_TAG_HIGH-1:ICACHE_TAG_LOW];
         rvdff #(ICACHE_TAG_HIGH-ICACHE_TAG_LOW) status_wr_addr_ff (.*,  .clk(free_clk), .din(ifu_status_wr_addr_w_debug[ICACHE_TAG_HIGH-1:ICACHE_TAG_LOW]),
                   .dout(ifu_status_wr_addr_ff[ICACHE_TAG_HIGH-1:ICACHE_TAG_LOW]));
         assign way_status_wr_en_w_debug = way_status_wr_en | (ic_debug_wr_en  & ic_debug_tag_array);
         rvdff #(1) status_wren_ff (.*, .clk(free_clk),  .din(way_status_wr_en_w_debug), .dout(way_status_wr_en_ff));
         assign way_status_new_w_debug[2:0]  = (ic_debug_wr_en  & ic_debug_tag_array) ? ic_debug_wr_data[6:4] :
                                                way_status_new[2:0] ;
         rvdff #(3) status_data_ff (.*,  .clk(free_clk), .din(way_status_new_w_debug[2:0]), .dout(way_status_new_ff[2:0]));
   logic [(ICACHE_TAG_DEPTH/8)-1 : 0] way_status_clken;
   logic [(ICACHE_TAG_DEPTH/8)-1 : 0] way_status_clk;
   genvar  j;
   for (i=0 ; i<ICACHE_TAG_DEPTH/8 ; i++) begin : CLK_GRP_WAY_STATUS
      assign way_status_clken[i] = (ifu_status_wr_addr_ff[ICACHE_TAG_HIGH-1:ICACHE_TAG_LOW+3] == i );
      for (j=0 ; j<8 ; j++) begin : WAY_STATUS
         rvdffs_fpga #(3) ic_way_status (.*,
                   .clk(way_status_clk[i]),
                   .clken(way_status_clken[i]),
                   .rawclk(clk),
                   .en(((ifu_status_wr_addr_ff[ICACHE_TAG_LOW+2:ICACHE_TAG_LOW] == j) & way_status_wr_en_ff)),
                   .din(way_status_new_ff[2:0]),
                   .dout(way_status_out[8*i+j]));
      end   
   end   
  always_comb begin : way_status_out_mux
      way_status[2:0] = 3'b0 ;
      for (int j=0; j< ICACHE_TAG_DEPTH; j++) begin : status_mux_loop
        if (ifu_ic_rw_int_addr_ff[ICACHE_TAG_HIGH-1:ICACHE_TAG_LOW] == (ICACHE_TAG_HIGH-ICACHE_TAG_LOW)'(j)) begin : mux_out
         way_status[2:0] =  way_status_out[j];
        end
      end
  end
assign ifu_ic_rw_int_addr_w_debug[ICACHE_TAG_HIGH-1:ICACHE_TAG_LOW] = ((ic_debug_rd_en | ic_debug_wr_en ) & ic_debug_tag_array) ?
                                                                        ic_debug_addr[ICACHE_TAG_HIGH-1:ICACHE_TAG_LOW] :
                                                                        ifu_ic_rw_int_addr[ICACHE_TAG_HIGH-1:ICACHE_TAG_LOW];
         rvdff #(ICACHE_TAG_HIGH-ICACHE_TAG_LOW) tag_addr_ff (.*, .clk(free_clk),
                   .din(ifu_ic_rw_int_addr_w_debug[ICACHE_TAG_HIGH-1:ICACHE_TAG_LOW]),
                   .dout(ifu_ic_rw_int_addr_ff[ICACHE_TAG_HIGH-1:ICACHE_TAG_LOW]));
         assign ifu_tag_wren_w_debug[3:0] = ifu_tag_wren[3:0] | ic_debug_tag_wr_en[3:0] ;
         rvdff #(4) tag_v_we_ff (.*, .clk(free_clk),
                   .din(ifu_tag_wren_w_debug[3:0]),
                   .dout(ifu_tag_wren_ff[3:0]));
         assign ic_valid_w_debug = (ic_debug_wr_en & ic_debug_tag_array) ? ic_debug_wr_data[0] : ic_valid;
         rvdff #(1) tag_v_ff (.*, .clk(free_clk),
                   .din(ic_valid_w_debug),
                   .dout(ic_valid_ff));
   logic [3:0] [ICACHE_TAG_DEPTH-1:0] ic_tag_valid_out ;
   logic [(ICACHE_TAG_DEPTH/32)-1:0]  tag_valid_w0_clken ;
   logic [(ICACHE_TAG_DEPTH/32)-1:0]  tag_valid_w1_clken ;
   logic [(ICACHE_TAG_DEPTH/32)-1:0]  tag_valid_w2_clken ;
   logic [(ICACHE_TAG_DEPTH/32)-1:0]  tag_valid_w3_clken ;
   logic [(ICACHE_TAG_DEPTH/32)-1:0]  tag_valid_w0_clk   ;
   logic [(ICACHE_TAG_DEPTH/32)-1:0]  tag_valid_w1_clk   ;
   logic [(ICACHE_TAG_DEPTH/32)-1:0]  tag_valid_w2_clk   ;
   logic [(ICACHE_TAG_DEPTH/32)-1:0]  tag_valid_w3_clk   ;
   for (i=0 ; i<ICACHE_TAG_DEPTH/32 ; i++) begin : CLK_GRP_TAG_VALID
      assign tag_valid_w0_clken[i] = (((ifu_ic_rw_int_addr_ff[ICACHE_TAG_HIGH-1:ICACHE_TAG_LOW+5] == i ) &  ifu_tag_wren_ff[0] ) |
                                      ((perr_ic_index_ff     [ICACHE_TAG_HIGH-1:ICACHE_TAG_LOW+5] == i ) &  perr_err_inv_way[0]) | reset_all_tags);
      assign tag_valid_w1_clken[i] = (((ifu_ic_rw_int_addr_ff[ICACHE_TAG_HIGH-1:ICACHE_TAG_LOW+5] == i ) &  ifu_tag_wren_ff[1] ) |
                                      ((perr_ic_index_ff     [ICACHE_TAG_HIGH-1:ICACHE_TAG_LOW+5] == i ) &  perr_err_inv_way[1]) | reset_all_tags);
      assign tag_valid_w2_clken[i] = (((ifu_ic_rw_int_addr_ff[ICACHE_TAG_HIGH-1:ICACHE_TAG_LOW+5] == i ) &  ifu_tag_wren_ff[2] ) |
                                      ((perr_ic_index_ff     [ICACHE_TAG_HIGH-1:ICACHE_TAG_LOW+5] == i ) &  perr_err_inv_way[2]) | reset_all_tags);
      assign tag_valid_w3_clken[i] = (((ifu_ic_rw_int_addr_ff[ICACHE_TAG_HIGH-1:ICACHE_TAG_LOW+5] == i ) &  ifu_tag_wren_ff[3] ) |
                                      ((perr_ic_index_ff     [ICACHE_TAG_HIGH-1:ICACHE_TAG_LOW+5] == i ) &  perr_err_inv_way[3]) | reset_all_tags);
    for (j=0 ; j<32 ; j++) begin : TAG_VALID
         rvdffs_fpga #(1) ic_way0_tagvalid_dup (.*,
                   .clk(tag_valid_w0_clk[i]),
                   .clken(tag_valid_w0_clken[i]),
                   .rawclk(clk),
                   .en(((ifu_ic_rw_int_addr_ff[ICACHE_TAG_LOW+4:ICACHE_TAG_LOW] == j) & ifu_tag_wren_ff[0] ) |
                        ((perr_ic_index_ff     [ICACHE_TAG_LOW+4:ICACHE_TAG_LOW] == j) & perr_err_inv_way[0]) | reset_all_tags),
                   .din(ic_valid_ff & ~reset_all_tags & ~perr_sel_invalidate),
                   .dout(ic_tag_valid_out[0][32*i+j]));
         rvdffs_fpga #(1) ic_way1_tagvalid_dup (.*,
                   .clk(tag_valid_w1_clk[i]),
                   .clken(tag_valid_w1_clken[i]),
                   .rawclk(clk),
                   .en(((ifu_ic_rw_int_addr_ff[ICACHE_TAG_LOW+4:ICACHE_TAG_LOW] == j) & ifu_tag_wren_ff[1] ) |
                        ((perr_ic_index_ff     [ICACHE_TAG_LOW+4:ICACHE_TAG_LOW] == j) & perr_err_inv_way[1]) | reset_all_tags),
                   .din(ic_valid_ff & ~reset_all_tags & ~perr_sel_invalidate),
                   .dout(ic_tag_valid_out[1][32*i+j]));
         rvdffs_fpga #(1) ic_way2_tagvalid_dup (.*,
                   .clk(tag_valid_w2_clk[i]),
                   .clken(tag_valid_w2_clken[i]),
                   .rawclk(clk),
                   .en(((ifu_ic_rw_int_addr_ff[ICACHE_TAG_LOW+4:ICACHE_TAG_LOW] == j) & ifu_tag_wren_ff[2] ) |
                        ((perr_ic_index_ff     [ICACHE_TAG_LOW+4:ICACHE_TAG_LOW] == j) & perr_err_inv_way[2]) | reset_all_tags),
                   .din(ic_valid_ff & ~reset_all_tags & ~perr_sel_invalidate),
                   .dout(ic_tag_valid_out[2][32*i+j]));
         rvdffs_fpga #(1) ic_way3_tagvalid_dup (.*,
                   .clk(tag_valid_w3_clk[i]),
                   .clken(tag_valid_w3_clken[i]),
                   .rawclk(clk),
                   .en(((ifu_ic_rw_int_addr_ff[ICACHE_TAG_LOW+4:ICACHE_TAG_LOW] == j) & ifu_tag_wren_ff[3] ) |
                        ((perr_ic_index_ff     [ICACHE_TAG_LOW+4:ICACHE_TAG_LOW] == j) & perr_err_inv_way[3]) | reset_all_tags),
                   .din(ic_valid_ff & ~reset_all_tags & ~perr_sel_invalidate),
                   .dout(ic_tag_valid_out[3][32*i+j]));
    end  
   end   
  logic [3:0] ic_tag_valid_unq;
  always_comb begin : tag_valid_out_mux
      ic_tag_valid_unq[3:0] = 4'b0 ;
      for (int j=0; j< ICACHE_TAG_DEPTH; j++) begin : tag_valid_loop
        if (ifu_ic_rw_int_addr_ff[ICACHE_TAG_HIGH-1:ICACHE_TAG_LOW] == (ICACHE_TAG_HIGH-ICACHE_TAG_LOW)'(j)) begin : valid_out
         ic_tag_valid_unq[3:0] =  {ic_tag_valid_out[3][j] , ic_tag_valid_out[2][j] , ic_tag_valid_out[1][j], ic_tag_valid_out[0][j]};
        end
      end
  end
   assign ic_tag_valid[3:0] = ic_tag_valid_unq[3:0] & {4{(~fetch_uncacheable_ff & ifc_fetch_req_f2) }} ;
   assign ic_debug_tag_val_rd_out = |(ic_tag_valid_unq[3:0] &  ic_debug_way_ff[3:00] & {4{ic_debug_rd_en_ff}}) ;
 assign ifu_pmu_ic_miss_in   = ic_act_miss_f2 ;
 assign ifu_pmu_ic_hit_in    = ic_act_hit_f2  ;
 assign ifu_pmu_bus_error_in = |ifc_bus_acc_fault_f2;
 assign ifu_pmu_bus_trxn_in  = axi_cmd_sent ;
 assign ifu_pmu_bus_busy_in  = ifu_axi_arvalid_ff & ~ifu_axi_arready_ff & miss_pending ;
   rvdff #(5) ifu_pmu_sigs_ff (.*,
                    .clk (active_clk),
                    .din ({ifu_pmu_ic_miss_in,
                           ifu_pmu_ic_hit_in,
                           ifu_pmu_bus_error_in,
                           ifu_pmu_bus_busy_in,
                           ifu_pmu_bus_trxn_in
                          }),
                    .dout({ifu_pmu_ic_miss,
                           ifu_pmu_ic_hit,
                           ifu_pmu_bus_error,
                           ifu_pmu_bus_busy,
                           ifu_pmu_bus_trxn
                           }));
logic ic_debug_ic_array_sel_word0_in;
logic ic_debug_ic_array_sel_word1_in;
logic ic_debug_ic_array_sel_word2_in;
logic ic_debug_ic_array_sel_word3_in;
logic ic_debug_ic_array_sel_word0   ;
logic ic_debug_ic_array_sel_word1   ;
logic ic_debug_ic_array_sel_word2   ;
logic ic_debug_ic_array_sel_word3   ;
assign ic_debug_addr[15:02]     = dec_tlu_ic_diag_pkt.icache_dicawics[15:2] ;
assign ic_debug_way_enc[01:00]  = dec_tlu_ic_diag_pkt.icache_dicawics[17:16] ;
assign ic_debug_wr_data[33:0]   = dec_tlu_ic_diag_pkt.icache_wrdata[33:0] ;
assign ic_debug_tag_array       = dec_tlu_ic_diag_pkt.icache_dicawics[18] ;
assign ic_debug_rd_en           = dec_tlu_ic_diag_pkt.icache_rd_valid ;
assign ic_debug_wr_en           = dec_tlu_ic_diag_pkt.icache_wr_valid ;
assign ic_debug_way[3:0]        = {(ic_debug_way_enc[1:0] == 2'b11),
                                   (ic_debug_way_enc[1:0] == 2'b10),
                                   (ic_debug_way_enc[1:0] == 2'b01),
                                   (ic_debug_way_enc[1:0] == 2'b00) };
assign ic_debug_tag_wr_en[3:0] = {4{ic_debug_wr_en & ic_debug_tag_array}} & ic_debug_way[3:0] ;
assign ic_debug_ic_array_sel_word0_in = (ic_debug_addr[3:2] == 2'b00) & ic_debug_rd_en & ~ic_debug_tag_array ;
assign ic_debug_ic_array_sel_word1_in = (ic_debug_addr[3:2] == 2'b01) & ic_debug_rd_en & ~ic_debug_tag_array ;
assign ic_debug_ic_array_sel_word2_in = (ic_debug_addr[3:2] == 2'b10) & ic_debug_rd_en & ~ic_debug_tag_array ;
assign ic_debug_ic_array_sel_word3_in = (ic_debug_addr[3:2] == 2'b11) & ic_debug_rd_en & ~ic_debug_tag_array ;
assign ic_debug_ict_array_sel_in      =  ic_debug_rd_en & ic_debug_tag_array ;
rvdffe #(09) ifu_debug_sel_ff (.*,
                    .en (debug_c1_clken),
                    .din ({ic_debug_ic_array_sel_word0_in,
                           ic_debug_ic_array_sel_word1_in,
                           ic_debug_ic_array_sel_word2_in,
                           ic_debug_ic_array_sel_word3_in,
                           ic_debug_ict_array_sel_in,
                           ic_debug_way[3:0]
                          }),
                    .dout({ic_debug_ic_array_sel_word0,
                           ic_debug_ic_array_sel_word1,
                           ic_debug_ic_array_sel_word2,
                           ic_debug_ic_array_sel_word3,
                           ic_debug_ict_array_sel_ff,
                           ic_debug_way_ff[3:0]
                           }));
rvdff #(1) ifu_debug_rd_en_ff (.*,.clk(free_clk),
                    .din ({
                           ic_debug_rd_en
                          }),
                    .dout({
                           ic_debug_rd_en_ff
                           }));
logic [33:0] ifu_ic_debug_rd_data_in   ;
assign ifu_ic_debug_rd_data_in[33:0] = ( {34{ic_debug_ict_array_sel_ff   }} &  {1'b0,ictag_debug_rd_data[20], ictag_debug_rd_data[19:0] ,5'b0, way_status[2:0], 3'b0, ic_debug_tag_val_rd_out} ) |
                                       ( {34{ic_debug_ic_array_sel_word0 }} &  {ic_rd_data [33:0]}  )  |
                                       ( {34{ic_debug_ic_array_sel_word1 }} &  {ic_rd_data [67:34]} )  |
                                       ( {34{ic_debug_ic_array_sel_word2 }} &  {ic_rd_data [101:68]})  |
                                       ( {34{ic_debug_ic_array_sel_word3 }} &  {ic_rd_data [135:102]}) ;
rvdffe #(34) ifu_debug_data_ff (.*,
                    .en (debug_data_clken),
                    .din ({
                           ifu_ic_debug_rd_data_in[33:0]
                          }),
                    .dout({
                           ifu_ic_debug_rd_data
                           }));
assign debug_data_clken  =  ic_debug_rd_en_ff;
rvdff #(1) ifu_debug_valid_ff (.*, .clk(free_clk),
                    .din ({
                           ic_debug_rd_en_ff
                          }),
                    .dout({
                           ifu_ic_debug_rd_data_valid
                           }));
   assign ifc_region_acc_okay           =  (~(|{1'h0,1'h0,1'h0,1'h0,1'h0,1'h0,1'h0,1'h0})) |
                                           (1'h0 & (({fetch_addr_f1[31:1],1'b0} | 'hffffffff) == ('h00000000 | 'hffffffff))) |
                                           (1'h0 & (({fetch_addr_f1[31:1],1'b0} | 'hffffffff) == ('h00000000 | 'hffffffff))) |
                                           (1'h0 & (({fetch_addr_f1[31:1],1'b0} | 'hffffffff) == ('h00000000 | 'hffffffff))) |
                                           (1'h0 & (({fetch_addr_f1[31:1],1'b0} | 'hffffffff) == ('h00000000 | 'hffffffff))) |
                                           (1'h0 & (({fetch_addr_f1[31:1],1'b0} | 'hffffffff) == ('h00000000 | 'hffffffff))) |
                                           (1'h0 & (({fetch_addr_f1[31:1],1'b0} | 'hffffffff) == ('h00000000 | 'hffffffff))) |
                                           (1'h0 & (({fetch_addr_f1[31:1],1'b0} | 'hffffffff) == ('h00000000 | 'hffffffff))) |
                                           (1'h0 & (({fetch_addr_f1[31:1],1'b0} | 'hffffffff) == ('h00000000 | 'hffffffff)));
   assign ifc_region_acc_fault_memory   =  ~ifc_iccm_access_f1 & ~ifc_region_acc_okay & ifc_fetch_req_f1;
   assign ifc_region_acc_fault_final_f1 = ifc_region_acc_fault_f1 | ifc_region_acc_fault_memory;
endmodule   
module ifu_iccm_mem
   import swerv_types::*;
(
   input logic         clk,
   input logic         free_clk,
   input logic         rst_l,
   input logic         clk_override,
   input logic          iccm_wren,
   input logic          iccm_rden,
   input logic [19-1:2]   iccm_rw_addr,
   input logic [2:0]    iccm_wr_size,
   input logic [77:0]   iccm_wr_data,
   output logic [155:0] iccm_rd_data,
   input  logic         scan_mode
);
localparam TOTAL_INT        = 9;
localparam DCCM_BITS        = 16;
localparam DCCM_BANK_BITS   = 3;
localparam DCCM_NUM_BANKS   = 8;
localparam DCCM_DATA_WIDTH  = 32;
localparam DCCM_FDATA_WIDTH = 39;
localparam DCCM_BYTE_WIDTH  = 4;
localparam DCCM_ECC_WIDTH   = 7;
localparam LSU_RDBUF_DEPTH  = 8;
localparam DMA_BUF_DEPTH    = 4;
localparam LSU_STBUF_DEPTH  = 8;
localparam LSU_SB_BITS      = 16;
localparam DEC_INSTBUF_DEPTH = 4;
localparam ICCM_SIZE         = 512;
localparam ICCM_BITS         = 19;
localparam ICCM_NUM_BANKS    = 8;
localparam ICCM_BANK_BITS    = 3;
localparam ICCM_INDEX_BITS   = 14;
localparam ICCM_BANK_HI      = 4 + (3/4);
localparam ICACHE_TAG_HIGH  = 12;
localparam ICACHE_TAG_LOW   = 6;
localparam ICACHE_IC_DEPTH  = 8;
localparam ICACHE_TAG_DEPTH = 64;
localparam LSU_BUS_TAG     = 4;
localparam DMA_BUS_TAG     = 1;
localparam SB_BUS_TAG      = 1;
localparam IFU_BUS_TAG     = 3;
   logic [ICCM_NUM_BANKS/4-1:0]               wren_bank;
   logic [ICCM_NUM_BANKS/4-1:0]               rden_bank;
   logic [ICCM_NUM_BANKS/4-1:0]               iccm_hi0_clken;
   logic [ICCM_NUM_BANKS/4-1:0]               iccm_hi1_clken;
   logic [ICCM_NUM_BANKS/4-1:0]               iccm_lo0_clken;
   logic [ICCM_NUM_BANKS/4-1:0]               iccm_lo1_clken;
   logic [ICCM_NUM_BANKS/4-1:0]               iccm_hi0_clk  ;
   logic [ICCM_NUM_BANKS/4-1:0]               iccm_hi1_clk  ;
   logic [ICCM_NUM_BANKS/4-1:0]               iccm_lo0_clk  ;
   logic [ICCM_NUM_BANKS/4-1:0]               iccm_lo1_clk  ;
   logic [ICCM_NUM_BANKS/4-1:0]               wren_bank_hi0;
   logic [ICCM_NUM_BANKS/4-1:0]               wren_bank_lo0;
   logic [ICCM_NUM_BANKS/4-1:0]               wren_bank_hi1;
   logic [ICCM_NUM_BANKS/4-1:0]               wren_bank_lo1;
   logic [ICCM_NUM_BANKS/4-1:0] [ICCM_INDEX_BITS-1:0] addr_bank;
   logic [ICCM_NUM_BANKS/4-1:0] [77:0]   iccm_bank_dout_hi;
   logic [ICCM_NUM_BANKS/4-1:0] [77:0]   iccm_bank_dout_lo;
   logic [5:4]                           iccm_rw_addr_q;
   for (genvar i=0; i<ICCM_NUM_BANKS/4; i++) begin: mem_bank
      assign  wren_bank[i]         = iccm_wren & ( (iccm_rw_addr[ICCM_BANK_HI:4] == i) | (ICCM_BANK_BITS == 2));
      assign  rden_bank[i]         = iccm_rden & ( (iccm_rw_addr[ICCM_BANK_HI:4] == i) | (ICCM_BANK_BITS == 2));
      assign  wren_bank_hi0[i]      = wren_bank[i] &  iccm_rw_addr[3] & (~iccm_rw_addr[2] | (iccm_wr_size[1:0] == 2'b11));
      assign  wren_bank_hi1[i]      = wren_bank[i] &  iccm_rw_addr[3] & ( iccm_rw_addr[2] | (iccm_wr_size[1:0] == 2'b11));
      assign  wren_bank_lo0[i]      = wren_bank[i] & ~iccm_rw_addr[3] & (~iccm_rw_addr[2] | (iccm_wr_size[1:0] == 2'b11));
      assign  wren_bank_lo1[i]      = wren_bank[i] & ~iccm_rw_addr[3] & ( iccm_rw_addr[2] | (iccm_wr_size[1:0] == 2'b11));
      assign iccm_hi0_clken[i]      =  wren_bank_hi0[i] |  (rden_bank[i] | clk_override);    
      assign iccm_hi1_clken[i]      =  wren_bank_hi1[i] |  (rden_bank[i] | clk_override);    
      assign iccm_lo0_clken[i]      =  wren_bank_lo0[i] |  (rden_bank[i] | clk_override);    
      assign iccm_lo1_clken[i]      =  wren_bank_lo1[i] |  (rden_bank[i] | clk_override);    
      rvoclkhdr iccm_hi0_c1_cgc  ( .en(iccm_hi0_clken[i]), .l1clk(iccm_hi0_clk[i]), .* );
      rvoclkhdr iccm_hi1_c1_cgc  ( .en(iccm_hi1_clken[i]), .l1clk(iccm_hi1_clk[i]), .* );
      rvoclkhdr iccm_lo0_c1_cgc  ( .en(iccm_lo0_clken[i]), .l1clk(iccm_lo0_clk[i]), .* );
      rvoclkhdr iccm_lo1_c1_cgc  ( .en(iccm_lo1_clken[i]), .l1clk(iccm_lo1_clk[i]), .* );
      assign  addr_bank[i][ICCM_INDEX_BITS-1:0] = iccm_rw_addr[ICCM_BITS-1:(ICCM_BANK_BITS+2)];
         ram_16384x39 iccm_bank_hi0 (
                                     .CLK(iccm_hi0_clk[i]),
                                     .WE(wren_bank_hi0[i]),
                                     .ADR(addr_bank[i]),
                                     .D(iccm_wr_data[38:0]),
                                     .Q(iccm_bank_dout_hi[i][38:0])
                                      );
          ram_16384x39 iccm_bank_hi1 (
                                     .CLK(iccm_hi1_clk[i]),
                                     .WE(wren_bank_hi1[i]),
                                     .ADR(addr_bank[i]),
                                     .D(iccm_wr_data[77:39]),
                                     .Q(iccm_bank_dout_hi[i][77:39])
                                      );
          ram_16384x39 iccm_bank_lo0 (
                                     .CLK(iccm_lo0_clk[i]),
                                     .WE(wren_bank_lo0[i]),
                                     .ADR(addr_bank[i]),
                                     .D(iccm_wr_data[38:0]),
                                     .Q(iccm_bank_dout_lo[i][38:0])
                                      );
         ram_16384x39 iccm_bank_lo1 (
                                     .CLK(iccm_lo1_clk[i]),
                                     .WE(wren_bank_lo1[i]),
                                     .ADR(addr_bank[i]),
                                     .D(iccm_wr_data[77:39]),
                                     .Q(iccm_bank_dout_lo[i][77:39])
                                      );
   end : mem_bank
   assign iccm_rd_data[155:0] = (ICCM_BANK_BITS == 2) ?  {iccm_bank_dout_hi[0][77:0], iccm_bank_dout_lo[0][77:0]}   :
                                                           { iccm_bank_dout_hi[iccm_rw_addr_q[ICCM_BANK_HI:4]][77:0], iccm_bank_dout_lo[iccm_rw_addr_q[ICCM_BANK_HI:4]][77:0] };
 if (ICCM_BANK_BITS == 2) begin
    assign iccm_rw_addr_q[5:4] = '0;
 end
 else begin
  rvdff  #(2) rd_addr_ff (.*, .clk(free_clk), .din(iccm_rw_addr[5:4]), .dout(iccm_rw_addr_q[5:4]) );
 end
endmodule  
module ifu
   import swerv_types::*;
(
   input logic free_clk,
   input logic active_clk,
   input logic clk,
   input logic clk_override,
   input logic rst_l,
   input logic dec_ib3_valid_d, dec_ib2_valid_d,  
   input logic dec_ib0_valid_eff_d,    
   input logic dec_ib1_valid_eff_d,    
   input logic        exu_i0_br_ret_e4,  
   input logic        exu_i1_br_ret_e4,  
   input logic        exu_i0_br_call_e4,  
   input logic        exu_i1_br_call_e4,  
   input logic exu_flush_final,  
   input logic dec_tlu_flush_err_wb ,  
   input logic dec_tlu_flush_noredir_wb,  
   input logic dec_tlu_dbg_halted,  
   input logic dec_tlu_pmu_fw_halted,  
   input logic [31:1] exu_flush_path_final,  
   input logic        exu_flush_upper_e2,     
   input logic [31:0]  dec_tlu_mrac_ff , 
   input logic         dec_tlu_fence_i_wb,  
   input logic         dec_tlu_flush_leak_one_wb,  
   input logic                       dec_tlu_bpred_disable,  
   input logic                       dec_tlu_core_ecc_disable,   
   output logic                           ifu_axi_awvalid,
   input  logic                           ifu_axi_awready,
   output logic [3-1:0]     ifu_axi_awid,
   output logic [31:0]                    ifu_axi_awaddr,
   output logic [3:0]                     ifu_axi_awregion,
   output logic [7:0]                     ifu_axi_awlen,
   output logic [2:0]                     ifu_axi_awsize,
   output logic [1:0]                     ifu_axi_awburst,
   output logic                           ifu_axi_awlock,
   output logic [3:0]                     ifu_axi_awcache,
   output logic [2:0]                     ifu_axi_awprot,
   output logic [3:0]                     ifu_axi_awqos,
   output logic                           ifu_axi_wvalid,
   input  logic                           ifu_axi_wready,
   output logic [63:0]                    ifu_axi_wdata,
   output logic [7:0]                     ifu_axi_wstrb,
   output logic                           ifu_axi_wlast,
   input  logic                           ifu_axi_bvalid,
   output logic                           ifu_axi_bready,
   input  logic [1:0]                     ifu_axi_bresp,
   input  logic [3-1:0]     ifu_axi_bid,
   output logic                           ifu_axi_arvalid,
   input  logic                           ifu_axi_arready,
   output logic [3-1:0]     ifu_axi_arid,
   output logic [31:0]                    ifu_axi_araddr,
   output logic [3:0]                     ifu_axi_arregion,
   output logic [7:0]                     ifu_axi_arlen,
   output logic [2:0]                     ifu_axi_arsize,
   output logic [1:0]                     ifu_axi_arburst,
   output logic                           ifu_axi_arlock,
   output logic [3:0]                     ifu_axi_arcache,
   output logic [2:0]                     ifu_axi_arprot,
   output logic [3:0]                     ifu_axi_arqos,
   input  logic                           ifu_axi_rvalid,
   output logic                           ifu_axi_rready,
   input  logic [3-1:0]     ifu_axi_rid,
   input  logic [63:0]                    ifu_axi_rdata,
   input  logic [1:0]                     ifu_axi_rresp,
   input  logic                           ifu_axi_rlast,
   input  logic                      ifu_bus_clk_en,
   input  logic                      dma_iccm_req,
   input  logic                      dma_iccm_stall_any,
   input  logic [31:0]               dma_mem_addr,
   input  logic [2:0]                dma_mem_sz,
   input  logic                      dma_mem_write,
   input  logic [63:0]               dma_mem_wdata,
   output logic                      iccm_dma_ecc_error,
   output logic                      iccm_dma_rvalid,
   output logic [63:0]               iccm_dma_rdata,
   output logic                      iccm_ready,
   output logic [1:0] ifu_pmu_instr_aligned,
   output logic       ifu_pmu_align_stall,
   output logic       ifu_pmu_fetch_stall,
   output logic [31:2]               ic_rw_addr,          
   output logic [3:0]                ic_wr_en,            
   output logic                      ic_rd_en,            
   output logic [67:0]               ic_wr_data,          
   input  logic [135:0]              ic_rd_data ,         
   input  logic [20:0]               ictag_debug_rd_data, 
   output logic [33:0]               ic_debug_wr_data,    
   output logic [33:0]               ifu_ic_debug_rd_data,
   output logic [127:0]              ic_premux_data,      
   output logic                      ic_sel_premux_data,  
   output logic [15:2]               ic_debug_addr,       
   output logic                      ic_debug_rd_en,      
   output logic                      ic_debug_wr_en,      
   output logic                      ic_debug_tag_array,  
   output logic [3:0]                ic_debug_way,        
   output logic [3:0]                ic_tag_valid,        
   input  logic [3:0]                ic_rd_hit,           
   input  logic                      ic_tag_perr,         
   output logic       ifu_pmu_ic_miss,  
   output logic       ifu_pmu_ic_hit,  
   output logic       ifu_pmu_bus_error,  
   output logic       ifu_pmu_bus_busy,   
   output logic       ifu_pmu_bus_trxn,  
   output logic  ifu_i0_valid,         
   output logic  ifu_i1_valid,         
   output logic  ifu_i0_icaf,          
   output logic  ifu_i1_icaf,          
   output logic  ifu_i0_icaf_second,       
   output logic  ifu_i1_icaf_second,       
   output logic  ifu_i0_perr,          
   output logic  ifu_i1_perr,          
   output logic  ifu_i0_sbecc,         
   output logic  ifu_i1_sbecc,         
   output logic  ifu_i0_dbecc,         
   output logic  ifu_i1_dbecc,         
   output logic  iccm_dma_sb_error,    
   output logic[31:0] ifu_i0_instr,    
   output logic[31:0] ifu_i1_instr,    
   output logic[31:1] ifu_i0_pc,       
   output logic[31:1] ifu_i1_pc,       
   output logic ifu_i0_pc4,            
   output logic ifu_i1_pc4,            
   output logic [15:0] ifu_illegal_inst,  
   output logic ifu_miss_state_idle,    
   output br_pkt_t i0_brp,            
   output br_pkt_t i1_brp,            
   input predict_pkt_t  exu_mp_pkt,  
   input logic [4:0] exu_mp_eghr,  
   input br_tlu_pkt_t dec_tlu_br0_wb_pkt,  
   input br_tlu_pkt_t dec_tlu_br1_wb_pkt,  
   input dec_tlu_flush_lower_wb,
   input rets_pkt_t exu_rets_e1_pkt,  
   input rets_pkt_t exu_rets_e4_pkt,  
   output logic [15:0] ifu_i0_cinst,
   output logic [15:0] ifu_i1_cinst,
   input  cache_debug_pkt_t        dec_tlu_ic_diag_pkt ,
   output logic                    ifu_ic_debug_rd_data_valid,
   input logic scan_mode
   );
   localparam TAGWIDTH = 2 ;
   localparam IDWIDTH  = 2 ;
   logic                   ifu_fb_consume1, ifu_fb_consume2;
   logic [31:1]            ifc_fetch_addr_f2;
   logic                   ifc_fetch_uncacheable_f1;
   logic [7:0]   ifu_fetch_val;   
   logic [31:1]  ifu_fetch_pc;    
   logic [31:1] ifc_fetch_addr_f1;
   logic        ic_crit_wd_rdy;
   logic        ic_write_stall;
   logic        ic_dma_active;
   logic        ifc_dma_access_ok;
   logic        ifc_iccm_access_f1;
   logic        ifc_region_acc_fault_f1;
   logic [7:0]  ic_access_fault_f2;
   logic        ifu_ic_mb_empty;
   logic ic_hit_f2;
   ifu_ifc_ctl ifc (.*
                    );
   logic [7:0]  ifu_bp_way_f2;  
   logic  ifu_bp_kill_next_f2;  
   logic [31:1] ifu_bp_btb_target_f2;  
   logic [7:1]  ifu_bp_inst_mask_f2;  
   logic [7:0]  ifu_bp_hist1_f2;  
   logic [7:0]  ifu_bp_hist0_f2;  
   logic [11:0] ifu_bp_poffset_f2;  
   logic [7:0]  ifu_bp_ret_f2;  
   logic [7:0]  ifu_bp_pc4_f2;  
   logic [7:0]  ifu_bp_valid_f2;  
   logic [4:0] ifu_bp_fghr_f2;
   ifu_bp_ctl bp (.*);
   logic [7:0]   ic_fetch_val_f2;
   logic [127:0] ic_data_f2;
   logic [127:0] ifu_fetch_data;
   logic ifc_fetch_req_f1_raw, ifc_fetch_req_f1, ifc_fetch_req_f2;
   logic ic_rd_parity_final_err;   
   logic iccm_rd_ecc_single_err;   
   logic [7:0] iccm_rd_ecc_double_err;   
   icache_err_pkt_t ic_error_f2;
   logic         ifu_icache_fetch_f2 ;
   logic [16:2]  ifu_icache_error_index;        
   logic         ifu_icache_error_val;    
   logic         ifu_icache_sb_error_val;
   assign ifu_fetch_data[127:0] = ic_data_f2[127:0];
   assign ifu_fetch_val[7:0] = ic_fetch_val_f2[7:0];
   assign ifu_fetch_pc[31:1] = ifc_fetch_addr_f2[31:1];
   ifu_aln_ctl aln (.*
                    );
   ifu_mem_ctl mem_ctl
     (.*,
      .fetch_addr_f1(ifc_fetch_addr_f1),
      .ifu_icache_error_index(ifu_icache_error_index[16:6]),
      .ic_hit_f2(ic_hit_f2),
      .ic_data_f2(ic_data_f2[127:0])
      );
endmodule  
module dec_decode_ctl
   import swerv_types::*;
(
   input logic [15:0] dec_i0_cinst_d,          
   input logic [15:0] dec_i1_cinst_d,
   output logic [31:0] dec_i0_inst_wb1,        
   output logic [31:0] dec_i1_inst_wb1,
   output logic [31:1] dec_i0_pc_wb1,          
   output logic [31:1] dec_i1_pc_wb1,
   input logic                                lsu_nonblock_load_valid_dc3,      
   input logic [3-1:0] lsu_nonblock_load_tag_dc3,        
   input logic                                lsu_nonblock_load_inv_dc5,        
   input logic [3-1:0] lsu_nonblock_load_inv_tag_dc5,    
   input logic                                lsu_nonblock_load_data_valid,     
   input logic                                lsu_nonblock_load_data_error,     
   input logic [3-1:0] lsu_nonblock_load_data_tag,       
   input logic [3:0] dec_i0_trigger_match_d,           
   input logic [3:0] dec_i1_trigger_match_d,           
   input logic dec_tlu_wr_pause_wb,                    
   input logic dec_tlu_pipelining_disable,             
   input logic dec_tlu_dual_issue_disable,             
   input logic dec_tlu_sec_alu_disable,                
   input logic [3:0]  lsu_trigger_match_dc3,           
   input logic lsu_pmu_misaligned_dc3,                 
   input logic dec_tlu_debug_stall,                    
   input logic dec_tlu_flush_leak_one_wb,              
   input logic dec_debug_fence_d,                      
   input logic [1:0] dbg_cmd_wrdata,                   
   input logic dec_i0_icaf_d,                          
   input logic dec_i1_icaf_d,
   input logic dec_i0_icaf_second_d,                       
   input logic dec_i0_perr_d,                          
   input logic dec_i1_perr_d,
   input logic dec_i0_sbecc_d,                         
   input logic dec_i1_sbecc_d,
   input logic dec_i0_dbecc_d,                         
   input logic dec_i1_dbecc_d,
   input br_pkt_t dec_i0_brp,                          
   input br_pkt_t dec_i1_brp,
   input logic [15:0] ifu_illegal_inst,                
   input logic [31:1] dec_i0_pc_d,                     
   input logic lsu_freeze_dc3,                         
   input logic lsu_halt_idle_any,                      
   input logic lsu_load_stall_any,                     
   input logic lsu_store_stall_any,                    
   input logic dma_dccm_stall_any,                     
   input logic exu_div_finish,                         
   input logic exu_div_stall,                          
   input logic [31:0] exu_div_result,                  
   input logic dec_tlu_i0_kill_writeb_wb,     
   input logic dec_tlu_i1_kill_writeb_wb,     
   input logic dec_tlu_flush_lower_wb,           
   input logic dec_tlu_flush_pause_wb,           
   input logic dec_tlu_presync_d,                
   input logic dec_tlu_postsync_d,               
   input logic [31:0] exu_mul_result_e3,         
   input logic dec_i0_pc4_d,                
   input logic dec_i1_pc4_d,
   input logic [31:0] dec_csr_rddata_d,     
   input logic dec_csr_legal_d,             
   input logic [31:0] exu_csr_rs1_e1,       
   input logic [31:0] lsu_result_dc3,       
   input logic [31:0] lsu_result_corr_dc4,  
   input logic exu_i0_flush_final,          
   input logic exu_i1_flush_final,          
   input logic [31:1] exu_i0_pc_e1,         
   input logic [31:1] exu_i1_pc_e1,
   input logic [31:0] dec_i0_instr_d,       
   input logic [31:0] dec_i1_instr_d,
   input logic  dec_ib0_valid_d,           
   input logic  dec_ib1_valid_d,
   input logic [31:0] exu_i0_result_e1,     
   input logic [31:0] exu_i1_result_e1,
   input logic [31:0] exu_i0_result_e4,     
   input logic [31:0] exu_i1_result_e4,
   input logic  clk,                        
   input logic  active_clk,                 
   input logic  free_clk,                   
   input logic  clk_override,               
   input logic  rst_l,
   output logic         dec_i0_rs1_en_d,    
   output logic         dec_i0_rs2_en_d,
   output logic [4:0] dec_i0_rs1_d,         
   output logic [4:0] dec_i0_rs2_d,
   output logic [31:0] dec_i0_immed_d,      
   output logic          dec_i1_rs1_en_d,
   output logic          dec_i1_rs2_en_d,
   output logic [4:0]  dec_i1_rs1_d,
   output logic [4:0]  dec_i1_rs2_d,
   output logic [31:0] dec_i1_immed_d,
   output logic [12:1] dec_i0_br_immed_d,     
   output logic [12:1] dec_i1_br_immed_d,
   output alu_pkt_t i0_ap,                    
   output alu_pkt_t i1_ap,
   output logic          dec_i0_decode_d,     
   output logic          dec_i1_decode_d,
   output logic          dec_ib0_valid_eff_d,    
   output logic          dec_ib1_valid_eff_d,
   output logic          dec_i0_alu_decode_d,    
   output logic          dec_i1_alu_decode_d,
   output logic [31:0] i0_rs1_bypass_data_d,     
   output logic [31:0] i0_rs2_bypass_data_d,     
   output logic [31:0] i1_rs1_bypass_data_d,
   output logic [31:0] i1_rs2_bypass_data_d,
   output logic [4:0]  dec_i0_waddr_wb,          
   output logic          dec_i0_wen_wb,          
   output logic [31:0] dec_i0_wdata_wb,          
   output logic [4:0]  dec_i1_waddr_wb,
   output logic          dec_i1_wen_wb,
   output logic [31:0] dec_i1_wdata_wb,
   output logic          dec_i0_select_pc_d,     
   output logic          dec_i1_select_pc_d,
   output logic dec_i0_rs1_bypass_en_d,          
   output logic dec_i0_rs2_bypass_en_d,          
   output logic dec_i1_rs1_bypass_en_d,
   output logic dec_i1_rs2_bypass_en_d,
   output lsu_pkt_t    lsu_p,                    
   output mul_pkt_t    mul_p,                    
   output div_pkt_t    div_p,                    
   output logic [11:0] dec_lsu_offset_d,
   output logic        dec_i0_lsu_d,         
   output logic        dec_i1_lsu_d,
   output logic        dec_i0_mul_d,         
   output logic        dec_i1_mul_d,
   output logic        dec_i0_div_d,         
   output logic        dec_i1_div_d,
   output logic        flush_final_e3,       
   output logic        i0_flush_final_e3,    
   output logic        dec_csr_ren_d,        
   output logic        dec_csr_wen_unq_d,        
   output logic        dec_csr_any_unq_d,        
   output logic        dec_csr_wen_wb,       
   output logic [11:0] dec_csr_rdaddr_d,       
   output logic [11:0] dec_csr_wraddr_wb,      
   output logic [31:0] dec_csr_wrdata_wb,    
   output logic        dec_csr_stall_int_ff,  
   output              dec_tlu_i0_valid_e4,   
   output              dec_tlu_i1_valid_e4,
   output              trap_pkt_t dec_tlu_packet_e4,    
   output logic        dec_fence_pending,  
   output logic [31:1] dec_tlu_i0_pc_e4,   
   output logic [31:1] dec_tlu_i1_pc_e4,
   output logic [31:0] dec_illegal_inst,         
   output logic        dec_i1_valid_e1,          
   output logic        dec_div_decode_e4,        
   output logic [31:1] pred_correct_npc_e2,      
   output logic        dec_i0_rs1_bypass_en_e3,  
   output logic        dec_i0_rs2_bypass_en_e3,  
   output logic        dec_i1_rs1_bypass_en_e3,
   output logic        dec_i1_rs2_bypass_en_e3,
   output logic [31:0] i0_rs1_bypass_data_e3,    
   output logic [31:0] i0_rs2_bypass_data_e3,    
   output logic [31:0] i1_rs1_bypass_data_e3,
   output logic [31:0] i1_rs2_bypass_data_e3,
   output logic        dec_i0_sec_decode_e3,     
   output logic        dec_i1_sec_decode_e3,     
   output logic [31:1] dec_i0_pc_e3,             
   output logic [31:1] dec_i1_pc_e3,             
   output logic        dec_i0_rs1_bypass_en_e2,  
   output logic        dec_i0_rs2_bypass_en_e2,  
   output logic        dec_i1_rs1_bypass_en_e2,
   output logic        dec_i1_rs2_bypass_en_e2,
   output logic [31:0] i0_rs1_bypass_data_e2,    
   output logic [31:0] i0_rs2_bypass_data_e2,    
   output logic [31:0] i1_rs1_bypass_data_e2,
   output logic [31:0] i1_rs2_bypass_data_e2,
   output predict_pkt_t  i0_predict_p_d,         
   output predict_pkt_t  i1_predict_p_d,
   output logic          dec_i0_lsu_decode_d,    
   output logic [31:0] i0_result_e4_eff,         
   output logic [31:0] i1_result_e4_eff,
   output logic [31:0] i0_result_e2,             
   output logic [4:2] dec_i0_data_en,            
   output logic [4:1] dec_i0_ctl_en,
   output logic [4:2] dec_i1_data_en,
   output logic [4:1] dec_i1_ctl_en,
   output logic [1:0] dec_pmu_instr_decoded,     
   output logic       dec_pmu_decode_stall,      
   output logic       dec_pmu_presync_stall,     
   output logic       dec_pmu_postsync_stall,    
   output logic       dec_nonblock_load_wen,         
   output logic [4:0] dec_nonblock_load_waddr,       
   output logic       dec_nonblock_load_freeze_dc2,  
   output logic       dec_pause_state,               
   output logic       dec_pause_state_cg,            
   output logic       dec_i0_load_e4,           
   input  logic        scan_mode
   );
   dec_pkt_t i0_dp_raw, i0_dp;
   dec_pkt_t i1_dp_raw, i1_dp;
   logic [31:0]        i0, i1;
   logic               i0_valid_d, i1_valid_d;
   logic [31:0]        i0_result_e1, i1_result_e1;
   logic [31:0]                      i1_result_e2;
   logic [31:0]        i0_result_e3, i1_result_e3;
   logic [31:0]        i0_result_e4, i1_result_e4;
   logic [31:0]        i0_result_wb, i1_result_wb;
   logic [31:1]        i0_pc_e1, i1_pc_e1;
   logic [31:1]        i0_pc_e2, i1_pc_e2;
   logic [31:1]        i0_pc_e3, i1_pc_e3;
   logic [31:1]        i0_pc_e4, i1_pc_e4;
   logic [9:0]         i0_rs1bypass, i0_rs2bypass;
   logic [9:0]         i1_rs1bypass, i1_rs2bypass;
   logic               i0_jalimm20, i1_jalimm20;
   logic               i0_uiimm20, i1_uiimm20;
   logic               lsu_decode_d;
   logic [31:0]        i0_immed_d;
   logic               i0_presync;
   logic               i0_postsync;
   logic               postsync_stall;
   logic               ps_stall;
   logic               prior_inflight, prior_inflight_e1e4, prior_inflight_wb;
   logic               csr_clr_d, csr_set_d, csr_write_d;
   logic        csr_clr_e1,csr_set_e1,csr_write_e1,csr_imm_e1;
   logic [31:0] csr_mask_e1;
   logic [31:0] write_csr_data_e1;
   logic [31:0] write_csr_data_in;
   logic [31:0] write_csr_data;
   logic               csr_data_wen;
   logic [4:0]         csrimm_e1;
   logic [31:0]        csr_rddata_e1;
   logic               flush_lower_wb;
   logic               i1_load_block_d;
   logic               i1_mul_block_d;
   logic               i1_load2_block_d;
   logic               i1_mul2_block_d;
   logic               mul_decode_d;
   logic               div_decode_d;
   logic [31:1]        div_pc;
   logic               div_stall, div_stall_ff;
   logic [3:0]         div_trigger;
   logic               i0_legal;
   logic               shift_illegal;
   logic               illegal_inst_en;
   logic [31:0]        illegal_inst;
   logic               illegal_lockout_in, illegal_lockout;
   logic               i0_legal_decode_d;
   logic               i1_flush_final_e3;
   logic [31:0]        i0_result_e3_final, i1_result_e3_final;
   logic [31:0]        i0_result_wb_raw,   i1_result_wb_raw;
   logic [12:1] last_br_immed_d;
   logic        i1_depend_i0_d;
   logic        i0_rs1_depend_i0_e1, i0_rs1_depend_i0_e2, i0_rs1_depend_i0_e3, i0_rs1_depend_i0_e4, i0_rs1_depend_i0_wb;
   logic        i0_rs1_depend_i1_e1, i0_rs1_depend_i1_e2, i0_rs1_depend_i1_e3, i0_rs1_depend_i1_e4, i0_rs1_depend_i1_wb;
   logic        i0_rs2_depend_i0_e1, i0_rs2_depend_i0_e2, i0_rs2_depend_i0_e3, i0_rs2_depend_i0_e4, i0_rs2_depend_i0_wb;
   logic        i0_rs2_depend_i1_e1, i0_rs2_depend_i1_e2, i0_rs2_depend_i1_e3, i0_rs2_depend_i1_e4, i0_rs2_depend_i1_wb;
   logic        i1_rs1_depend_i0_e1, i1_rs1_depend_i0_e2, i1_rs1_depend_i0_e3, i1_rs1_depend_i0_e4, i1_rs1_depend_i0_wb;
   logic        i1_rs1_depend_i1_e1, i1_rs1_depend_i1_e2, i1_rs1_depend_i1_e3, i1_rs1_depend_i1_e4, i1_rs1_depend_i1_wb;
   logic        i1_rs2_depend_i0_e1, i1_rs2_depend_i0_e2, i1_rs2_depend_i0_e3, i1_rs2_depend_i0_e4, i1_rs2_depend_i0_wb;
   logic        i1_rs2_depend_i1_e1, i1_rs2_depend_i1_e2, i1_rs2_depend_i1_e3, i1_rs2_depend_i1_e4, i1_rs2_depend_i1_wb;
   logic        i1_rs1_depend_i0_d, i1_rs2_depend_i0_d;
   logic        i0_secondary_d, i1_secondary_d;
   logic        i0_secondary_block_d, i1_secondary_block_d;
   logic        non_block_case_d;
   logic        i0_div_decode_d;
   logic [31:0] i0_result_e4_final, i1_result_e4_final;
   logic        i0_load_block_d;
   logic        i0_mul_block_d;
   logic [3:0]  i0_rs1_depth_d, i0_rs2_depth_d;
   logic [3:0]  i1_rs1_depth_d, i1_rs2_depth_d;
   logic        i0_rs1_match_e1_e2, i0_rs1_match_e1_e3;
   logic        i0_rs2_match_e1_e2, i0_rs2_match_e1_e3;
   logic        i1_rs1_match_e1_e2, i1_rs1_match_e1_e3;
   logic        i1_rs2_match_e1_e2, i1_rs2_match_e1_e3;
   logic        i0_load_stall_d,  i1_load_stall_d;
   logic        i0_store_stall_d, i1_store_stall_d;
   logic        i0_predict_nt, i0_predict_t;
   logic        i1_predict_nt, i1_predict_t;
   logic        i0_notbr_error, i0_br_toffset_error;
   logic        i1_notbr_error, i1_br_toffset_error;
   logic        i0_ret_error,   i1_ret_error;
   logic        i0_br_error, i1_br_error;
   logic        i0_br_error_all, i1_br_error_all;
   logic [11:0] i0_br_offset, i1_br_offset;
   logic        freeze;
   logic [20:1] i0_pcall_imm, i1_pcall_imm;     
   logic        i0_pcall_12b_offset, i1_pcall_12b_offset;
   logic        i0_pcall_raw,   i1_pcall_raw;
   logic        i0_pcall_case,  i1_pcall_case;
   logic        i0_pcall,  i1_pcall;
   logic        i0_pja_raw,   i1_pja_raw;
   logic        i0_pja_case,  i1_pja_case;
   logic        i0_pja,  i1_pja;
   logic        i0_pret_case, i1_pret_case;
   logic        i0_pret_raw, i0_pret;
   logic        i1_pret_raw, i1_pret;
   logic        i0_jal, i1_jal;   
   logic        i0_predict_br, i1_predict_br;
   logic        freeze_prior1, freeze_prior2;
   logic [31:0] i0_result_e4_freeze, i1_result_e4_freeze;
   logic [31:0] i0_result_wb_freeze, i1_result_wb_freeze;
   logic [31:0] i1_result_wb_eff, i0_result_wb_eff;
   logic [2:0]  i1rs1_intra, i1rs2_intra;
   logic        i1_rs1_intra_bypass, i1_rs2_intra_bypass;
   logic        store_data_bypass_c1, store_data_bypass_c2;
   logic [1:0]  store_data_bypass_e4_c1, store_data_bypass_e4_c2, store_data_bypass_e4_c3;
   logic        store_data_bypass_i0_e2_c2;
   class_pkt_t i0_rs1_class_d, i0_rs2_class_d;
   class_pkt_t i1_rs1_class_d, i1_rs2_class_d;
   class_pkt_t i0_dc, i0_e1c, i0_e2c, i0_e3c, i0_e4c, i0_wbc;
   class_pkt_t i1_dc, i1_e1c, i1_e2c, i1_e3c, i1_e4c, i1_wbc;
   logic i0_rs1_match_e1, i0_rs1_match_e2, i0_rs1_match_e3;
   logic i1_rs1_match_e1, i1_rs1_match_e2, i1_rs1_match_e3;
   logic i0_rs2_match_e1, i0_rs2_match_e2, i0_rs2_match_e3;
   logic i1_rs2_match_e1, i1_rs2_match_e2, i1_rs2_match_e3;
   logic       i0_secondary_stall_d;
   logic       i0_ap_pc2, i0_ap_pc4;
   logic       i1_ap_pc2, i1_ap_pc4;
   logic        div_wen_wb;
   logic        i0_rd_en_d;
   logic        i1_rd_en_d;
   logic [4:0]  i1_rd_d;
   logic [4:0]  i0_rd_d;
   logic        load_ldst_bypass_c1;
   logic        load_mul_rs1_bypass_e1;
   logic        load_mul_rs2_bypass_e1;
   logic        leak1_i0_stall_in, leak1_i0_stall;
   logic        leak1_i1_stall_in, leak1_i1_stall;
   logic        leak1_mode;
   logic        i0_csr_write_only_d;
   logic        prior_inflight_e1e3, prior_inflight_eff;
   logic        any_csr_d;
   logic        prior_csr_write;
   logic [5:0] i0_pipe_en;
   logic       i0_e1_ctl_en, i0_e2_ctl_en, i0_e3_ctl_en, i0_e4_ctl_en, i0_wb_ctl_en;
   logic       i0_e1_data_en, i0_e2_data_en, i0_e3_data_en, i0_e4_data_en, i0_wb_data_en, i0_wb1_data_en;
   logic [5:0] i1_pipe_en;
   logic       i1_e1_ctl_en, i1_e2_ctl_en, i1_e3_ctl_en, i1_e4_ctl_en, i1_wb_ctl_en;
   logic       i1_e1_data_en, i1_e2_data_en, i1_e3_data_en, i1_e4_data_en, i1_wb_data_en, i1_wb1_data_en;
   logic debug_fence_i;
   logic debug_fence;
   logic i0_csr_write;
   logic presync_stall;
   logic i0_instr_error;
   logic i0_icaf_d;
   logic i1_icaf_d;
   logic i0_not_alu_eff, i1_not_alu_eff;
   logic disable_secondary;
   logic clear_pause;
   logic pause_state_in, pause_state;
   logic pause_stall;
   logic [31:1] i1_pc_wb;
   logic        i0_brp_valid;
   logic        nonblock_load_cancel;
   logic        lsu_idle;
   logic        csr_read_e1;
   logic        i0_block_d;
   logic        i1_block_d;
   logic        ps_stall_in;
   logic        freeze_after_unfreeze1;
   logic        freeze_after_unfreeze2;
   logic        unfreeze_cycle1;
   logic        unfreeze_cycle2;
   logic        tlu_wr_pause_wb1, tlu_wr_pause_wb2;
   localparam NBLOAD_SIZE     = 8;
   localparam NBLOAD_SIZE_MSB = 8-1;
   localparam NBLOAD_TAG_MSB  = 3-1;
   logic                     cam_write, cam_inv_reset, cam_data_reset;
   logic [NBLOAD_TAG_MSB:0]  cam_write_tag, cam_inv_reset_tag, cam_data_reset_tag;
   logic [NBLOAD_SIZE_MSB:0] cam_wen;
   logic [NBLOAD_TAG_MSB:0]  load_data_tag;
   logic [NBLOAD_SIZE_MSB:0] nonblock_load_write;
   load_cam_pkt_t [NBLOAD_SIZE_MSB:0] cam;
   load_cam_pkt_t [NBLOAD_SIZE_MSB:0] cam_in;
   logic [4:0] nonblock_load_rd;
   logic i1_nonblock_load_stall,     i0_nonblock_load_stall;
   logic i1_nonblock_boundary_stall, i0_nonblock_boundary_stall;
   logic i0_depend_load_e1_d, i0_depend_load_e2_d;
   logic i1_depend_load_e1_d, i1_depend_load_e2_d;
   logic    depend_load_e1_d,  depend_load_e2_d,  depend_load_same_cycle_d;
   logic    depend_load_e2_e1,                    depend_load_same_cycle_e1;
   logic                                          depend_load_same_cycle_e2;
   logic nonblock_load_valid_dc4, nonblock_load_valid_wb;
   logic i0_load_kill_wen, i1_load_kill_wen;
   logic found;
   logic cam_reset_same_dest_wb;
   logic [NBLOAD_SIZE_MSB:0] cam_inv_reset_val, cam_data_reset_val;
   logic       i1_wen_wb, i0_wen_wb;
   inst_t i0_itype, i1_itype;
   logic csr_read, csr_write;
   logic i0_br_unpred, i1_br_unpred;
   logic debug_fence_raw;
   logic freeze_before;
   logic freeze_e3, freeze_e4;
   logic [3:0] e4t_i0trigger;
   logic [3:0] e4t_i1trigger;
   logic       e4d_i0load;
   logic [4:0] div_waddr_wb;
   logic [12:1] last_br_immed_e1, last_br_immed_e2;
   logic [31:0]        i0_inst_d, i1_inst_d;
   logic [31:0]        i0_inst_e1, i1_inst_e1;
   logic [31:0]        i0_inst_e2, i1_inst_e2;
   logic [31:0]        i0_inst_e3, i1_inst_e3;
   logic [31:0]        i0_inst_e4, i1_inst_e4;
   logic [31:0]        i0_inst_wb, i1_inst_wb;
   logic [31:0]        i0_inst_wb1,i1_inst_wb1;
   logic [31:0]        div_inst;
   logic [31:1] i0_pc_wb, i0_pc_wb1;
   logic [31:1]           i1_pc_wb1;
   logic [31:1] last_pc_e2;
   reg_pkt_t i0r, i1r;
   trap_pkt_t   dt, e1t_in, e1t, e2t_in, e2t, e3t_in, e3t, e4t;
   class_pkt_t i0_e4c_in, i1_e4c_in;
   dest_pkt_t  dd, e1d, e2d, e3d, e4d, wbd;
   dest_pkt_t e1d_in, e2d_in, e3d_in, e4d_in;
   assign freeze = lsu_freeze_dc3;
   assign disable_secondary        = dec_tlu_sec_alu_disable;
   assign i0_brp_valid = dec_i0_brp.valid & ~leak1_mode;
   assign      i0_predict_p_d.misp    =  '0;
   assign      i0_predict_p_d.ataken  =  '0;
   assign      i0_predict_p_d.boffset =  '0;
   assign      i0_predict_p_d.pcall  =  i0_pcall;   
   assign      i0_predict_p_d.pja    =  i0_pja;
   assign      i0_predict_p_d.pret   =  i0_pret;
   assign      i0_predict_p_d.prett[31:1] = dec_i0_brp.prett[31:1];
   assign      i0_predict_p_d.pc4 = dec_i0_pc4_d;
   assign      i0_predict_p_d.hist[1:0] = dec_i0_brp.hist[1:0];
   assign      i0_predict_p_d.valid = i0_brp_valid & i0_legal_decode_d;
   assign      i0_notbr_error = i0_brp_valid & ~(i0_dp_raw.condbr | i0_pcall_raw | i0_pja_raw | i0_pret_raw);
   assign      i0_br_toffset_error = i0_brp_valid & dec_i0_brp.hist[1] & (dec_i0_brp.toffset[11:0] != i0_br_offset[11:0]) & !i0_pret_raw;
   assign      i0_ret_error = i0_brp_valid & dec_i0_brp.ret & ~i0_pret_raw;
   assign      i0_br_error =  dec_i0_brp.br_error | i0_notbr_error | i0_br_toffset_error | i0_ret_error;
   assign      i0_predict_p_d.br_error = i0_br_error & i0_legal_decode_d & ~leak1_mode;
   assign      i0_predict_p_d.br_start_error = dec_i0_brp.br_start_error & i0_legal_decode_d & ~leak1_mode;
   assign      i0_predict_p_d.index[5:4] = dec_i0_brp.index[5:4];
   assign      i0_predict_p_d.bank[1:0] = dec_i0_brp.bank[1:0];
   assign      i0_predict_p_d.btag[9-1:0] = dec_i0_brp.btag[9-1:0];
   assign      i0_br_error_all = (i0_br_error | dec_i0_brp.br_start_error) & ~leak1_mode;
   assign      i0_predict_p_d.toffset[11:0] = i0_br_offset[11:0];
   assign      i0_predict_p_d.fghr[4:0] = dec_i0_brp.fghr[4:0];
   assign      i0_predict_p_d.way = dec_i0_brp.way;
   assign      i1_predict_p_d.misp    =  '0;
   assign      i1_predict_p_d.ataken  =  '0;
   assign      i1_predict_p_d.boffset =  '0;
   assign      i1_predict_p_d.pcall  =  i1_pcall;
   assign      i1_predict_p_d.pja    =  i1_pja;
   assign      i1_predict_p_d.pret   =  i1_pret;
   assign      i1_predict_p_d.prett[31:1] = dec_i1_brp.prett[31:1];
   assign      i1_predict_p_d.pc4 = dec_i1_pc4_d;
   assign      i1_predict_p_d.hist[1:0] = dec_i1_brp.hist[1:0];
   assign      i1_predict_p_d.valid = dec_i1_brp.valid & dec_i1_decode_d;
   assign      i1_notbr_error = dec_i1_brp.valid & ~(i1_dp_raw.condbr | i1_pcall_raw | i1_pja_raw | i1_pret_raw);
   assign      i1_br_toffset_error = dec_i1_brp.valid & dec_i1_brp.hist[1] & (dec_i1_brp.toffset[11:0] != i1_br_offset[11:0]) & !i1_pret_raw;
   assign      i1_ret_error = dec_i1_brp.valid & dec_i1_brp.ret & ~i1_pret_raw;
   assign      i1_br_error = dec_i1_brp.br_error | i1_notbr_error | i1_br_toffset_error | i1_ret_error;
   assign      i1_predict_p_d.br_error = i1_br_error & dec_i1_decode_d;
   assign      i1_predict_p_d.br_start_error = dec_i1_brp.br_start_error & dec_i1_decode_d;
   assign      i1_predict_p_d.index[5:4] = dec_i1_brp.index[5:4];
   assign      i1_predict_p_d.bank[1:0] = dec_i1_brp.bank[1:0];
   assign      i1_predict_p_d.btag[9-1:0] = dec_i1_brp.btag[9-1:0];
   assign      i1_br_error_all = (i1_br_error | dec_i1_brp.br_start_error);
   assign      i1_predict_p_d.toffset[11:0] = i1_br_offset[11:0];
   assign      i1_predict_p_d.fghr[4:0] = dec_i1_brp.fghr[4:0];
   assign      i1_predict_p_d.way = dec_i1_brp.way;
   assign i0_icaf_d = dec_i0_icaf_d | dec_i0_dbecc_d;
   assign i1_icaf_d = dec_i1_icaf_d | dec_i1_dbecc_d;
   assign i0_instr_error = i0_icaf_d | dec_i0_perr_d | dec_i0_sbecc_d;
   always_comb begin
      i0_dp = i0_dp_raw;
      if (i0_br_error_all | i0_instr_error) begin
         i0_dp = '0;
         i0_dp.alu = 1'b1;
         i0_dp.rs1 = 1'b1;
         i0_dp.rs2 = 1'b1;
         i0_dp.lor = 1'b1;
         i0_dp.legal = 1'b1;
         i0_dp.postsync = 1'b1;
      end
      i1_dp = i1_dp_raw;
      if (i1_br_error_all) begin
         i1_dp = '0;
         i1_dp.alu = 1'b1;
         i1_dp.rs1 = 1'b1;
         i1_dp.rs2 = 1'b1;
         i1_dp.lor = 1'b1;
         i1_dp.legal = 1'b1;
         i1_dp.postsync = 1'b1;
      end
   end
   assign flush_lower_wb = dec_tlu_flush_lower_wb;
   assign i0[31:0] = dec_i0_instr_d[31:0];
   assign i1[31:0] = dec_i1_instr_d[31:0];
   assign dec_i0_select_pc_d = i0_dp.pc;
   assign dec_i1_select_pc_d = i1_dp.pc;
   assign i0_predict_br =  i0_dp.condbr | i0_pcall | i0_pja | i0_pret;
   assign i1_predict_br =  i1_dp.condbr | i1_pcall | i1_pja | i1_pret;
   assign i0_predict_nt = ~(dec_i0_brp.hist[1] & i0_brp_valid) & i0_predict_br;
   assign i0_predict_t  =  (dec_i0_brp.hist[1] & i0_brp_valid) & i0_predict_br;
   assign i0_ap.valid =  (i0_dc.sec | i0_dc.alu | i0_dp.alu );
   assign i0_ap.add =    i0_dp.add;
   assign i0_ap.sub =    i0_dp.sub;
   assign i0_ap.land =   i0_dp.land;
   assign i0_ap.lor =    i0_dp.lor;
   assign i0_ap.lxor =   i0_dp.lxor;
   assign i0_ap.sll =    i0_dp.sll;
   assign i0_ap.srl =    i0_dp.srl;
   assign i0_ap.sra =    i0_dp.sra;
   assign i0_ap.slt =    i0_dp.slt;
   assign i0_ap.unsign = i0_dp.unsign;
   assign i0_ap.beq =    i0_dp.beq;
   assign i0_ap.bne =    i0_dp.bne;
   assign i0_ap.blt =    i0_dp.blt;
   assign i0_ap.bge =    i0_dp.bge;
   assign i0_ap.csr_write = i0_csr_write_only_d;
   assign i0_ap.csr_imm = i0_dp.csr_imm;
   assign i0_ap.jal    =  i0_jal;
   assign i0_ap_pc2 = ~dec_i0_pc4_d;
   assign i0_ap_pc4 =  dec_i0_pc4_d;
   assign i0_ap.predict_nt = i0_predict_nt;
   assign i0_ap.predict_t  = i0_predict_t;
   assign i1_predict_nt = ~(dec_i1_brp.hist[1] & dec_i1_brp.valid) & i1_predict_br;
   assign i1_predict_t  =  (dec_i1_brp.hist[1] & dec_i1_brp.valid) & i1_predict_br;
   assign i1_ap.valid =  (i1_dc.sec | i1_dc.alu | i1_dp.alu);
   assign i1_ap.add =    i1_dp.add;
   assign i1_ap.sub =    i1_dp.sub;
   assign i1_ap.land =   i1_dp.land;
   assign i1_ap.lor =    i1_dp.lor;
   assign i1_ap.lxor =   i1_dp.lxor;
   assign i1_ap.sll =    i1_dp.sll;
   assign i1_ap.srl =    i1_dp.srl;
   assign i1_ap.sra =    i1_dp.sra;
   assign i1_ap.slt =    i1_dp.slt;
   assign i1_ap.unsign = i1_dp.unsign;
   assign i1_ap.beq =    i1_dp.beq;
   assign i1_ap.bne =    i1_dp.bne;
   assign i1_ap.blt =    i1_dp.blt;
   assign i1_ap.bge =    i1_dp.bge;
   assign i1_ap.csr_write = 1'b0;
   assign i1_ap.csr_imm   = 1'b0;
   assign i1_ap.jal    =    i1_jal;
   assign i1_ap_pc2 = ~dec_i1_pc4_d;
   assign i1_ap_pc4 =  dec_i1_pc4_d;
   assign i1_ap.predict_nt = i1_predict_nt;
   assign i1_ap.predict_t  = i1_predict_t;
   always_comb begin
      found = 0;
      cam_wen[NBLOAD_SIZE_MSB:0] = '0;
      for (int i=0; i<NBLOAD_SIZE; i++) begin
         if (~found) begin
            if (~cam[i].valid) begin
               cam_wen[i] = cam_write;
               found = 1'b1;
            end
         end
      end
   end
   assign cam_reset_same_dest_wb = wbd.i0v & wbd.i1v & (wbd.i0rd[4:0] == wbd.i1rd[4:0]) &
                                   wbd.i0load & nonblock_load_valid_wb & ~dec_tlu_i0_kill_writeb_wb & ~dec_tlu_i1_kill_writeb_wb;
   assign cam_write          = lsu_nonblock_load_valid_dc3;
   assign cam_write_tag[NBLOAD_TAG_MSB:0] = lsu_nonblock_load_tag_dc3[NBLOAD_TAG_MSB:0];
   assign cam_inv_reset          = lsu_nonblock_load_inv_dc5 | cam_reset_same_dest_wb;
   assign cam_inv_reset_tag[NBLOAD_TAG_MSB:0] = lsu_nonblock_load_inv_tag_dc5[NBLOAD_TAG_MSB:0];
   assign cam_data_reset          = lsu_nonblock_load_data_valid | lsu_nonblock_load_data_error;
   assign cam_data_reset_tag[NBLOAD_TAG_MSB:0] = lsu_nonblock_load_data_tag[NBLOAD_TAG_MSB:0];
   assign nonblock_load_rd[4:0] = (e3d.i0load) ? e3d.i0rd[4:0] : e3d.i1rd[4:0];   
   for (genvar i=0; i<NBLOAD_SIZE; i++) begin : cam_array
      assign cam_inv_reset_val[i] = cam_inv_reset   & (cam_inv_reset_tag[NBLOAD_TAG_MSB:0]  == cam[i].tag[NBLOAD_TAG_MSB:0]) & cam[i].valid;
      assign cam_data_reset_val[i] = cam_data_reset & (cam_data_reset_tag[NBLOAD_TAG_MSB:0] == cam[i].tag[NBLOAD_TAG_MSB:0]) & cam[i].valid;
      always_comb begin
         cam_in[i] = '0;
         if (cam_wen[i]) begin
            cam_in[i].valid    = 1'b1;
            cam_in[i].wb       = 1'b0;
            cam_in[i].tag[NBLOAD_TAG_MSB:0] = cam_write_tag[NBLOAD_TAG_MSB:0];
            cam_in[i].rd[4:0]  = nonblock_load_rd[4:0];
         end
         else if ( (cam_inv_reset_val[i]) |
                   (cam_data_reset_val[i]) |
                   (i0_wen_wb & (wbd.i0rd[4:0] == cam[i].rd[4:0]) & cam[i].wb) |
                   (i1_wen_wb & (wbd.i1rd[4:0] == cam[i].rd[4:0]) & cam[i].wb) )
           cam_in[i].valid = 1'b0;
         else
           cam_in[i] = cam[i];
         if (nonblock_load_valid_wb & (lsu_nonblock_load_inv_tag_dc5[NBLOAD_TAG_MSB:0]==cam[i].tag[NBLOAD_TAG_MSB:0]) & cam[i].valid)
           cam_in[i].wb = 1'b1;
      end
   rvdff #( $bits(load_cam_pkt_t) ) cam_ff (.*, .clk(free_clk), .din(cam_in[i]), .dout(cam[i]));
   assign nonblock_load_write[i] = (load_data_tag[NBLOAD_TAG_MSB:0] == cam[i].tag[NBLOAD_TAG_MSB:0]) & cam[i].valid;
end : cam_array
   assign load_data_tag[NBLOAD_TAG_MSB:0] = lsu_nonblock_load_data_tag[NBLOAD_TAG_MSB:0];
   assign nonblock_load_cancel = ((wbd.i0rd[4:0] == dec_nonblock_load_waddr[4:0]) & i0_wen_wb) |     
                                 ((wbd.i1rd[4:0] == dec_nonblock_load_waddr[4:0]) & i1_wen_wb);
   assign dec_nonblock_load_wen = lsu_nonblock_load_data_valid & |nonblock_load_write[NBLOAD_SIZE_MSB:0] & ~nonblock_load_cancel;
   always_comb begin
      dec_nonblock_load_waddr[4:0] = '0;
      i0_nonblock_load_stall = i0_nonblock_boundary_stall;
      i1_nonblock_load_stall = i1_nonblock_boundary_stall;
      for (int i=0; i<NBLOAD_SIZE; i++) begin
         dec_nonblock_load_waddr[4:0] |= ({5{nonblock_load_write[i]}} & cam[i].rd[4:0]);
         i0_nonblock_load_stall |= dec_i0_rs1_en_d & cam[i].valid & (cam[i].rd[4:0] == i0r.rs1[4:0]);
         i0_nonblock_load_stall |= dec_i0_rs2_en_d & cam[i].valid & (cam[i].rd[4:0] == i0r.rs2[4:0]);
         i1_nonblock_load_stall |= dec_i1_rs1_en_d & cam[i].valid & (cam[i].rd[4:0] == i1r.rs1[4:0]);
         i1_nonblock_load_stall |= dec_i1_rs2_en_d & cam[i].valid & (cam[i].rd[4:0] == i1r.rs2[4:0]);
      end
   end
   assign i0_nonblock_boundary_stall = ((nonblock_load_rd[4:0]==i0r.rs1[4:0]) & lsu_nonblock_load_valid_dc3 & dec_i0_rs1_en_d) |
                                       ((nonblock_load_rd[4:0]==i0r.rs2[4:0]) & lsu_nonblock_load_valid_dc3 & dec_i0_rs2_en_d);
   assign i1_nonblock_boundary_stall = ((nonblock_load_rd[4:0]==i1r.rs1[4:0]) & lsu_nonblock_load_valid_dc3 & dec_i1_rs1_en_d) |
                                       ((nonblock_load_rd[4:0]==i1r.rs2[4:0]) & lsu_nonblock_load_valid_dc3 & dec_i1_rs2_en_d);
   assign i0_depend_load_e1_d = ((i0_rs1_class_d.load & (i0_rs1_depth_d[3:0]==4'd1 | i0_rs1_depth_d[3:0]==4'd2)) |
                                 (i0_rs2_class_d.load & (i0_rs2_depth_d[3:0]==4'd1 | i0_rs2_depth_d[3:0]==4'd2))) & dec_i0_decode_d;
   assign i0_depend_load_e2_d = ((i0_rs1_class_d.load & (i0_rs1_depth_d[3:0]==4'd3 | i0_rs1_depth_d[3:0]==4'd4)) |
                                 (i0_rs2_class_d.load & (i0_rs2_depth_d[3:0]==4'd3 | i0_rs2_depth_d[3:0]==4'd4))) & dec_i0_decode_d;
   assign i1_depend_load_e1_d = ((i1_rs1_class_d.load & (i1_rs1_depth_d[3:0]==4'd1 | i1_rs1_depth_d[3:0]==4'd2)) |
                                 (i1_rs2_class_d.load & (i1_rs2_depth_d[3:0]==4'd1 | i1_rs2_depth_d[3:0]==4'd2))) & dec_i1_decode_d;
   assign i1_depend_load_e2_d = ((i1_rs1_class_d.load & (i1_rs1_depth_d[3:0]==4'd3 | i1_rs1_depth_d[3:0]==4'd4)) |
                                 (i1_rs2_class_d.load & (i1_rs2_depth_d[3:0]==4'd3 | i1_rs2_depth_d[3:0]==4'd4))) & dec_i1_decode_d;
   assign depend_load_e1_d = i0_depend_load_e1_d | i1_depend_load_e1_d;
   assign depend_load_e2_d = i0_depend_load_e2_d | i1_depend_load_e2_d;
   assign depend_load_same_cycle_d = i1_depend_i0_d & i0_dp.load & dec_i1_decode_d;
   rvdffs #(2) e1loadff (.*,
                         .clk(active_clk),
                         .en(i0_e1_ctl_en),
                         .din( {depend_load_e1_d,  depend_load_same_cycle_d}),
                         .dout({depend_load_e2_e1, depend_load_same_cycle_e1})
                         );
   rvdffs #(1) e2loadff (.*,
                         .clk(active_clk),
                         .en(i0_e2_ctl_en),
                         .din( depend_load_same_cycle_e1),
                         .dout(depend_load_same_cycle_e2)
                         );
   assign dec_nonblock_load_freeze_dc2 = depend_load_e2_d | depend_load_e2_e1 | depend_load_same_cycle_e2;
   rvdffs #(1) e4nbloadff (.*, .clk(active_clk), .en(i0_e4_ctl_en), .din(lsu_nonblock_load_valid_dc3),  .dout(nonblock_load_valid_dc4) );
   rvdffs #(1) wbnbloadff (.*, .clk(active_clk), .en(i0_wb_ctl_en), .din(    nonblock_load_valid_dc4),  .dout(nonblock_load_valid_wb) );
   assign i0_load_kill_wen = nonblock_load_valid_wb &  wbd.i0load;
   assign i1_load_kill_wen = nonblock_load_valid_wb &  wbd.i1load;
   assign csr_read = dec_csr_ren_d;
   assign csr_write = dec_csr_wen_unq_d;
   assign i0_br_unpred = (i0_dp.condbr | i0_dp.jal) & ~i0_predict_br;
   assign i1_br_unpred = (i1_dp.condbr | i1_dp.jal) & ~i1_predict_br;
   always_comb begin
      i0_itype = NULL;
      i1_itype = NULL;
      if (i0_legal_decode_d) begin
         if (i0_dp.mul)                  i0_itype = MUL;
         if (i0_dp.load)                 i0_itype = LOAD;
         if (i0_dp.store)                i0_itype = STORE;
         if (i0_dp.pm_alu)               i0_itype = ALU;
         if ( csr_read & ~csr_write)     i0_itype = CSRREAD;
         if (~csr_read &  csr_write)     i0_itype = CSRWRITE;
         if ( csr_read &  csr_write)     i0_itype = CSRRW;
         if (i0_dp.ebreak)               i0_itype = EBREAK;
         if (i0_dp.ecall)                i0_itype = ECALL;
         if (i0_dp.fence)                i0_itype = FENCE;
         if (i0_dp.fence_i)              i0_itype = FENCEI;   
         if (i0_dp.mret)                 i0_itype = MRET;
         if (i0_dp.condbr)               i0_itype = CONDBR;
         if (i0_dp.jal)                  i0_itype = JAL;
      end
      if (dec_i1_decode_d) begin
         if (i1_dp.mul)                  i1_itype = MUL;
         if (i1_dp.load)                 i1_itype = LOAD;
         if (i1_dp.store)                i1_itype = STORE;
         if (i1_dp.pm_alu)               i1_itype = ALU;
         if (i1_dp.condbr)               i1_itype = CONDBR;
         if (i1_dp.jal)                  i1_itype = JAL;
      end
   end
   dec_dec_ctl i0_dec (.inst(i0[31:0]),.out(i0_dp_raw));
   dec_dec_ctl i1_dec (.inst(i1[31:0]),.out(i1_dp_raw));
   rvdff #(1) lsu_idle_ff (.*, .clk(active_clk), .din(lsu_halt_idle_any), .dout(lsu_idle));
   assign leak1_i1_stall_in = (dec_tlu_flush_leak_one_wb | (leak1_i1_stall & ~dec_tlu_flush_lower_wb));
   rvdff #(1) leak1_i1_stall_ff (.*, .clk(free_clk), .din(leak1_i1_stall_in), .dout(leak1_i1_stall));
   assign leak1_mode = leak1_i1_stall;
   assign leak1_i0_stall_in = ((dec_i0_decode_d & leak1_i1_stall) | (leak1_i0_stall & ~dec_tlu_flush_lower_wb));
   rvdff #(1) leak1_i0_stall_ff (.*, .clk(free_clk), .din(leak1_i0_stall_in), .dout(leak1_i0_stall));
   assign i0_pcall_imm[20:1] = {i0[31],i0[19:12],i0[20],i0[30:21]};
   assign i0_pcall_12b_offset = (i0_pcall_imm[12]) ? (i0_pcall_imm[20:13] == 8'hff) : (i0_pcall_imm[20:13] == 8'h0);
   assign i0_pcall_case  = i0_pcall_12b_offset & i0_dp_raw.imm20 & i0r.rd[4:0]!=5'b0;
   assign i0_pja_case    = i0_pcall_12b_offset & i0_dp_raw.imm20 & i0r.rd[4:0]==5'b0;
   assign i1_pcall_imm[20:1] = {i1[31],i1[19:12],i1[20],i1[30:21]};
   assign i1_pcall_12b_offset = (i1_pcall_imm[12]) ? (i1_pcall_imm[20:13] == 8'hff) : (i1_pcall_imm[20:13] == 8'h0);
   assign i1_pcall_case  = i1_pcall_12b_offset & i1_dp_raw.imm20 & i1r.rd[4:0]!=5'b0;
   assign i1_pja_case    = i1_pcall_12b_offset & i1_dp_raw.imm20 & i1r.rd[4:0]==5'b0;
   assign i0_pcall_raw = i0_dp_raw.jal &   i0_pcall_case;    
   assign i0_pcall     = i0_dp.jal     &   i0_pcall_case;
   assign i1_pcall_raw = i1_dp_raw.jal &   i1_pcall_case;
   assign i1_pcall     = i1_dp.jal     &   i1_pcall_case;
   assign i0_pja_raw = i0_dp_raw.jal &   i0_pja_case;
   assign i0_pja     = i0_dp.jal     &   i0_pja_case;
   assign i1_pja_raw = i1_dp_raw.jal &   i1_pja_case;
   assign i1_pja     = i1_dp.jal     &   i1_pja_case;
   assign i0_br_offset[11:0] = (i0_pcall_raw | i0_pja_raw) ? i0_pcall_imm[12:1] : {i0[31],i0[7],i0[30:25],i0[11:8]};
   assign i1_br_offset[11:0] = (i1_pcall_raw | i1_pja_raw) ? i1_pcall_imm[12:1] : {i1[31],i1[7],i1[30:25],i1[11:8]};
   assign i0_pret_case = (i0_dp_raw.jal & i0_dp_raw.imm12 & i0r.rd[4:0]==5'b0 & i0r.rs1[4:0]==5'b1);   
   assign i1_pret_case = (i1_dp_raw.jal & i1_dp_raw.imm12 & i1r.rd[4:0]==5'b0 & i1r.rs1[4:0]==5'b1);
   assign i0_pret_raw = i0_dp_raw.jal &   i0_pret_case;
   assign i0_pret    = i0_dp.jal     &   i0_pret_case;
   assign i1_pret_raw = i1_dp_raw.jal &   i1_pret_case;
   assign i1_pret     = i1_dp.jal     &   i1_pret_case;
   assign i0_jal    = i0_dp.jal  &  ~i0_pcall_case & ~i0_pja_case & ~i0_pret_case;
   assign i1_jal    = i1_dp.jal  &  ~i1_pcall_case & ~i1_pja_case & ~i1_pret_case;
   assign dec_lsu_offset_d[11:0] =
                                   ({12{ i0_dp.lsu & i0_dp.load}} &               i0[31:20]) |
                                   ({12{~i0_dp.lsu & i1_dp.lsu & i1_dp.load}} &   i1[31:20]) |
                                   ({12{ i0_dp.lsu & i0_dp.store}} &             {i0[31:25],i0[11:7]}) |
                                   ({12{~i0_dp.lsu & i1_dp.lsu & i1_dp.store}} & {i1[31:25],i1[11:7]});
   assign dec_i0_lsu_d = i0_dp.lsu;
   assign dec_i1_lsu_d = i1_dp.lsu;
   assign dec_i0_mul_d = i0_dp.mul;
   assign dec_i1_mul_d = i1_dp.mul;
   assign dec_i0_div_d = i0_dp.div;
   assign dec_i1_div_d = i1_dp.div;
   assign div_p.valid = div_decode_d;
   assign div_p.unsign = (i0_dp.div) ? i0_dp.unsign :   i1_dp.unsign;
   assign div_p.rem  =   (i0_dp.div) ? i0_dp.rem    :   i1_dp.rem;
   assign mul_p.valid = mul_decode_d;
   assign mul_p.rs1_sign =   (i0_dp.mul) ? i0_dp.rs1_sign :   i1_dp.rs1_sign;
   assign mul_p.rs2_sign =   (i0_dp.mul) ? i0_dp.rs2_sign :   i1_dp.rs2_sign;
   assign mul_p.low      =   (i0_dp.mul) ? i0_dp.low      :   i1_dp.low;
   assign mul_p.load_mul_rs1_bypass_e1 = load_mul_rs1_bypass_e1;
   assign mul_p.load_mul_rs2_bypass_e1 = load_mul_rs2_bypass_e1;
   assign lsu_p.valid = lsu_decode_d;
   assign lsu_p.load =   (i0_dp.lsu) ? i0_dp.load :   i1_dp.load;
   assign lsu_p.store =  (i0_dp.lsu) ? i0_dp.store :  i1_dp.store;
   assign lsu_p.by =     (i0_dp.lsu) ? i0_dp.by :     i1_dp.by;
   assign lsu_p.half =   (i0_dp.lsu) ? i0_dp.half :   i1_dp.half;
   assign lsu_p.word =   (i0_dp.lsu) ? i0_dp.word :   i1_dp.word;
   assign lsu_p.dword = '0;
   assign lsu_p.dma = '0;
   assign lsu_p.store_data_bypass_i0_e2_c2   = store_data_bypass_i0_e2_c2;   
   assign lsu_p.load_ldst_bypass_c1 = load_ldst_bypass_c1;
   assign lsu_p.store_data_bypass_c1 = store_data_bypass_c1 & ~store_data_bypass_i0_e2_c2;
   assign lsu_p.store_data_bypass_c2 = store_data_bypass_c2 & ~store_data_bypass_i0_e2_c2;
   assign lsu_p.store_data_bypass_e4_c1[1:0] = store_data_bypass_e4_c1[1:0] & ~{2{store_data_bypass_i0_e2_c2}};
   assign lsu_p.store_data_bypass_e4_c2[1:0] = store_data_bypass_e4_c2[1:0] & ~{2{store_data_bypass_i0_e2_c2}};
   assign lsu_p.store_data_bypass_e4_c3[1:0] = store_data_bypass_e4_c3[1:0] & ~{2{store_data_bypass_i0_e2_c2}};
   assign lsu_p.unsign = (i0_dp.lsu) ? i0_dp.unsign : i1_dp.unsign;
   assign i0r.rs1[4:0] = i0[19:15];
   assign i0r.rs2[4:0] = i0[24:20];
   assign i0r.rd[4:0] = i0[11:7];
   assign i1r.rs1[4:0] = i1[19:15];
   assign i1r.rs2[4:0] = i1[24:20];
   assign i1r.rd[4:0] = i1[11:7];
   assign dec_i0_rs1_en_d = i0_dp.rs1 & (i0r.rs1[4:0] != 5'd0);   
   assign dec_i0_rs2_en_d = i0_dp.rs2 & (i0r.rs2[4:0] != 5'd0);
   assign i0_rd_en_d =  i0_dp.rd & (i0r.rd[4:0] != 5'd0);
   assign dec_i0_rs1_d[4:0] = i0r.rs1[4:0];
   assign dec_i0_rs2_d[4:0] = i0r.rs2[4:0];
   assign i0_rd_d[4:0] = i0r.rd[4:0];
   assign i0_jalimm20 = i0_dp.jal & i0_dp.imm20;    
   assign i1_jalimm20 = i1_dp.jal & i1_dp.imm20;
   assign i0_uiimm20 = ~i0_dp.jal & i0_dp.imm20;
   assign i1_uiimm20 = ~i1_dp.jal & i1_dp.imm20;
   assign dec_csr_ren_d = i0_dp.csr_read & i0_legal_decode_d;
   assign csr_clr_d =   i0_dp.csr_clr   & i0_legal_decode_d;
   assign csr_set_d   = i0_dp.csr_set   & i0_legal_decode_d;
   assign csr_write_d = i0_csr_write    & i0_legal_decode_d;
   assign i0_csr_write_only_d = i0_csr_write & ~i0_dp.csr_read;
   assign dec_csr_wen_unq_d = (i0_dp.csr_clr | i0_dp.csr_set | i0_csr_write);    
   assign dec_csr_any_unq_d = any_csr_d;
   assign dec_csr_rdaddr_d[11:0] = i0[31:20];
   assign dec_csr_wraddr_wb[11:0] = wbd.csrwaddr[11:0];
   assign dec_csr_wen_wb = wbd.csrwen & wbd.i0valid & ~dec_tlu_i0_kill_writeb_wb;
   assign dec_csr_stall_int_ff = ((e4d.csrwaddr[11:0] == 12'h300) | (e4d.csrwaddr[11:0] == 12'h304)) & e4d.csrwen & e4d.i0valid & ~dec_tlu_i0_kill_writeb_wb;
   rvdffs #(5) csrmiscff (.*,
                        .en(~freeze),
                        .clk(active_clk),
                        .din({ dec_csr_ren_d,  csr_clr_d,  csr_set_d,  csr_write_d,  i0_dp.csr_imm}),
                        .dout({csr_read_e1,    csr_clr_e1, csr_set_e1, csr_write_e1, csr_imm_e1})
                       );
   rvdffe #(37) csr_data_e1ff (.*, .en(i0_e1_data_en), .din( {i0[19:15],dec_csr_rddata_d[31:0]}), .dout({csrimm_e1[4:0],csr_rddata_e1[31:0]}));
   assign csr_mask_e1[31:0] = ({32{ csr_imm_e1}} & {27'b0,csrimm_e1[4:0]}) |
                              ({32{~csr_imm_e1}} &  exu_csr_rs1_e1[31:0]);
   assign write_csr_data_e1[31:0] = ({32{csr_clr_e1}}   & (csr_rddata_e1[31:0] & ~csr_mask_e1[31:0])) |
                                    ({32{csr_set_e1}}   & (csr_rddata_e1[31:0] |  csr_mask_e1[31:0])) |
                                    ({32{csr_write_e1}} & (                       csr_mask_e1[31:0]));
   assign clear_pause = (dec_tlu_flush_lower_wb & ~dec_tlu_flush_pause_wb) |
                        (pause_state & (write_csr_data[31:1] == 31'b0));         
   assign pause_state_in = (dec_tlu_wr_pause_wb | pause_state) & ~clear_pause;
   rvdff #(1) pause_state_f (.*, .clk(free_clk), .din(pause_state_in), .dout(pause_state));
   assign dec_pause_state = pause_state;
   rvdff #(2) pause_state_wb_ff (.*, .clk(free_clk), .din({dec_tlu_wr_pause_wb,tlu_wr_pause_wb1}), .dout({tlu_wr_pause_wb1,tlu_wr_pause_wb2}));
   assign dec_pause_state_cg = pause_state & ~tlu_wr_pause_wb1 & ~tlu_wr_pause_wb2;
   assign csr_data_wen = ((csr_clr_e1 | csr_set_e1 | csr_write_e1) & csr_read_e1 & ~freeze) | dec_tlu_wr_pause_wb | pause_state;
   assign write_csr_data_in[31:0] = (pause_state)         ? (write_csr_data[31:0] - 32'b1) :
                                    (dec_tlu_wr_pause_wb) ? dec_csr_wrdata_wb[31:0] : write_csr_data_e1[31:0];
   rvdffe #(32) write_csr_ff (.*, .en(csr_data_wen), .din(write_csr_data_in[31:0]), .dout(write_csr_data[31:0]));
   assign pause_stall = pause_state;
   assign dec_csr_wrdata_wb[31:0] = (wbd.csrwonly) ? i0_result_wb[31:0] : write_csr_data[31:0];
   assign dec_i0_immed_d[31:0] = ({32{ i0_dp.csr_read}} & dec_csr_rddata_d[31:0]) |
                                 ({32{~i0_dp.csr_read}} & i0_immed_d[31:0]);
   assign     i0_immed_d[31:0] = ({32{i0_dp.imm12}} &   { {20{i0[31]}},i0[31:20] }) |   
                                 ({32{i0_dp.shimm5}} &    {27'b0, i0[24:20]}) |
                                 ({32{i0_jalimm20}} &   { {12{i0[31]}},i0[19:12],i0[20],i0[30:21],1'b0}) |
                                 ({32{i0_uiimm20}}  &     {i0[31:12],12'b0 }) |
                                 ({32{i0_csr_write_only_d & i0_dp.csr_imm}} & {27'b0,i0[19:15]});   
   assign dec_i0_br_immed_d[12:1] = (i0_ap.predict_nt & ~i0_dp.jal) ? i0_br_offset[11:0] : {10'b0,i0_ap_pc4,i0_ap_pc2};
   assign dec_i1_rs1_en_d = i1_dp.rs1 & (i1r.rs1[4:0] != 5'd0);
   assign dec_i1_rs2_en_d = i1_dp.rs2 & (i1r.rs2[4:0] != 5'd0);
   assign i1_rd_en_d =  i1_dp.rd & (i1r.rd[4:0] != 5'd0);
   assign dec_i1_rs1_d[4:0] = i1r.rs1[4:0];
   assign dec_i1_rs2_d[4:0] = i1r.rs2[4:0];
   assign i1_rd_d[4:0] = i1r.rd[4:0];
   assign dec_i1_immed_d[31:0] = ({32{i1_dp.imm12}} &   { {20{i1[31]}},i1[31:20] }) |
                                 ({32{i1_dp.shimm5}} &    {27'b0, i1[24:20]}) |
                                 ({32{i1_jalimm20}} &   { {12{i1[31]}},i1[19:12],i1[20],i1[30:21],1'b0}) |
                                 ({32{i1_uiimm20}}  &     {i1[31:12],12'b0 });
   assign dec_i1_br_immed_d[12:1] = (i1_ap.predict_nt & ~i1_dp.jal) ? i1_br_offset[11:0] : {10'b0,i1_ap_pc4,i1_ap_pc2};
   assign last_br_immed_d[12:1] = (dec_i1_decode_d) ?
                                  ((i1_ap.predict_nt) ? {10'b0,i1_ap_pc4,i1_ap_pc2} : i1_br_offset[11:0] ) :
                                  ((i0_ap.predict_nt) ? {10'b0,i0_ap_pc4,i0_ap_pc2} : i0_br_offset[11:0] );
   assign i0_valid_d = dec_ib0_valid_d;
   assign i1_valid_d = dec_ib1_valid_d;
   assign i0_load_stall_d = i0_dp.load & (lsu_load_stall_any | dma_dccm_stall_any);
   assign i1_load_stall_d = i1_dp.load & (lsu_load_stall_any | dma_dccm_stall_any);
   assign i0_store_stall_d =  i0_dp.store & (lsu_store_stall_any | dma_dccm_stall_any);
   assign i1_store_stall_d =  i1_dp.store & (lsu_store_stall_any | dma_dccm_stall_any);
   assign i1_depend_i0_d = (dec_i1_rs1_en_d & i0_dp.rd & (i1r.rs1[4:0] == i0r.rd[4:0])) |
                           (dec_i1_rs2_en_d & i0_dp.rd & (i1r.rs2[4:0] == i0r.rd[4:0]));
   assign i1_load2_block_d = i1_dp.lsu & i0_dp.lsu;
   assign i0_presync = i0_dp.presync | dec_tlu_presync_d | debug_fence_i | debug_fence_raw | dec_tlu_pipelining_disable;   
   assign i0_postsync = i0_dp.postsync | dec_tlu_postsync_d | debug_fence_i |  
                        (i0_csr_write_only_d & (i0[31:20] == 12'h7c2));    
   assign i1_mul2_block_d  = i1_dp.mul & i0_dp.mul;
   assign debug_fence_i     = dec_debug_fence_d & dbg_cmd_wrdata[0];
   assign debug_fence_raw   = dec_debug_fence_d & dbg_cmd_wrdata[1];
   assign debug_fence = debug_fence_raw | debug_fence_i;     
   assign i0_csr_write = i0_dp.csr_write & ~dec_debug_fence_d;
   assign presync_stall = (i0_presync & prior_inflight_eff);
   assign prior_inflight_eff = (i0_dp.div) ? prior_inflight_e1e3 : prior_inflight;
   assign dec_fence_pending = (i0_valid_d & i0_dp.fence) | debug_fence;
   assign i0_block_d = (i0_dp.csr_read & prior_csr_write) |
                       pause_stall |
                       leak1_i0_stall |
                       dec_tlu_debug_stall |
                       postsync_stall |
                       presync_stall  |
                       ((i0_dp.fence | debug_fence) & ~lsu_idle) |
                       i0_nonblock_load_stall |
                       i0_load_block_d |
                       i0_mul_block_d |
                       i0_store_stall_d |
                       i0_load_stall_d |
                       i0_secondary_stall_d |   
                       i0_secondary_block_d;
   assign i1_block_d = leak1_i1_stall |
                      (i0_jal) |             
              (((|dec_i0_trigger_match_d[3:0]) | ((i0_dp.condbr | i0_dp.jal) & i0_secondary_d)) & i1_dp.load ) |  
                       i0_presync | i0_postsync |
                       i1_dp.presync | i1_dp.postsync |
                       i1_icaf_d |         
                       dec_i1_perr_d |     
                       dec_i1_sbecc_d |
                       i0_dp.csr_read |
                       i0_dp.csr_write |
                       i1_dp.csr_read |
                       i1_dp.csr_write |   
                       i1_nonblock_load_stall |
                       i1_store_stall_d |
                       i1_load_block_d |     
                       i1_mul_block_d |      
                       (i1_depend_i0_d & ~non_block_case_d & ~store_data_bypass_i0_e2_c2) |
                       i1_load2_block_d |   
                       i1_mul2_block_d |
                       i1_load_stall_d |   
                       i1_secondary_block_d |  
                       dec_tlu_dual_issue_disable;  
   assign prior_csr_write = e1d.csrwonly |
                            e2d.csrwonly |
                            e3d.csrwonly |
                            e4d.csrwonly |
                            wbd.csrwonly;
   assign any_csr_d = i0_dp.csr_read | i0_csr_write;
   assign i0_legal = i0_dp.legal & (~any_csr_d | dec_csr_legal_d);
   assign shift_illegal = dec_i0_decode_d & ~i0_legal;
   assign illegal_inst_en = shift_illegal & ~illegal_lockout & ~freeze;
   assign illegal_inst[31:0] = (dec_i0_pc4_d) ? i0[31:0] : { 16'b0, ifu_illegal_inst[15:0] };
   rvdffe #(32) illegal_any_ff (.*, .en(illegal_inst_en), .din(illegal_inst[31:0]), .dout(dec_illegal_inst[31:0]));
   assign illegal_lockout_in = (shift_illegal | illegal_lockout) & ~flush_final_e3;
   rvdffs #(1) illegal_lockout_any_ff (.*, .clk(active_clk), .en(~freeze), .din(illegal_lockout_in), .dout(illegal_lockout));
   assign dec_i0_decode_d = i0_valid_d & ~i0_block_d & ~flush_lower_wb & ~flush_final_e3 & ~freeze;
   assign i0_legal_decode_d = dec_i0_decode_d & i0_legal & ~freeze;
   assign dec_i1_decode_d = i0_legal_decode_d & i1_valid_d & i1_dp.legal & ~i1_block_d & ~freeze;
   assign dec_ib0_valid_eff_d = i0_valid_d & ~dec_i0_decode_d;
   assign dec_ib1_valid_eff_d = i1_valid_d & ~dec_i1_decode_d;
   assign dec_pmu_instr_decoded[1:0] = { dec_i1_decode_d, dec_i0_decode_d };
   assign dec_pmu_decode_stall = i0_valid_d & ~dec_i0_decode_d;
   assign dec_pmu_postsync_stall = postsync_stall;
   assign dec_pmu_presync_stall  = presync_stall;
   assign ps_stall_in =  (dec_i0_decode_d & (i0_jal | (i0_postsync) | ~i0_legal))  |
                         (dec_i1_decode_d &  i1_jal ) |
                         ((ps_stall & prior_inflight_e1e4) & ~div_wen_wb);
    rvdffs #(1) postsync_stallff (.*, .clk(free_clk), .en(~freeze), .din(ps_stall_in), .dout(ps_stall));
   assign postsync_stall = (ps_stall | div_stall);
   assign prior_inflight_e1e3 =       |{ e1d.i0valid,
                                         e2d.i0valid,
                                         e3d.i0valid,
                                         e1d.i1valid,
                                         e2d.i1valid,
                                         e3d.i1valid
                                         };
   assign prior_inflight_e1e4 =       |{ e1d.i0valid,
                                         e2d.i0valid,
                                         e3d.i0valid,
                                         e4d.i0valid,
                                         e1d.i1valid,
                                         e2d.i1valid,
                                         e3d.i1valid,
                                         e4d.i1valid
                                         };
   assign prior_inflight_wb =            |{
                                           wbd.i0valid,
                                           wbd.i1valid
                                           };
   assign prior_inflight = prior_inflight_e1e4 | prior_inflight_wb;
   assign dec_i0_alu_decode_d = i0_legal_decode_d & i0_dp.alu & ~i0_secondary_d;
   assign dec_i1_alu_decode_d = dec_i1_decode_d & i1_dp.alu & ~i1_secondary_d;
   assign dec_i0_lsu_decode_d = i0_legal_decode_d & i0_dp.lsu;
   assign lsu_decode_d = (i0_legal_decode_d & i0_dp.lsu) |
                         (dec_i1_decode_d & i1_dp.lsu);
   assign mul_decode_d = (i0_legal_decode_d & i0_dp.mul) |
                         (dec_i1_decode_d & i1_dp.mul);
   assign div_decode_d = (i0_legal_decode_d & i0_dp.div) |
                         (dec_i1_decode_d & i1_dp.div);
   rvdffs #(2) flushff (.*, .en(~freeze), .clk(free_clk), .din({exu_i0_flush_final,exu_i1_flush_final}), .dout({i0_flush_final_e3, i1_flush_final_e3}));
   assign flush_final_e3 = i0_flush_final_e3 | i1_flush_final_e3;
   assign i0_rs1_depend_i0_e1 = dec_i0_rs1_en_d & e1d.i0v & (e1d.i0rd[4:0] == i0r.rs1[4:0]);
   assign i0_rs1_depend_i0_e2 = dec_i0_rs1_en_d & e2d.i0v & (e2d.i0rd[4:0] == i0r.rs1[4:0]);
   assign i0_rs1_depend_i0_e3 = dec_i0_rs1_en_d & e3d.i0v & (e3d.i0rd[4:0] == i0r.rs1[4:0]);
   assign i0_rs1_depend_i0_e4 = dec_i0_rs1_en_d & e4d.i0v & (e4d.i0rd[4:0] == i0r.rs1[4:0]);
   assign i0_rs1_depend_i0_wb = dec_i0_rs1_en_d & wbd.i0v & (wbd.i0rd[4:0] == i0r.rs1[4:0]);
   assign i0_rs1_depend_i1_e1 = dec_i0_rs1_en_d & e1d.i1v & (e1d.i1rd[4:0] == i0r.rs1[4:0]);
   assign i0_rs1_depend_i1_e2 = dec_i0_rs1_en_d & e2d.i1v & (e2d.i1rd[4:0] == i0r.rs1[4:0]);
   assign i0_rs1_depend_i1_e3 = dec_i0_rs1_en_d & e3d.i1v & (e3d.i1rd[4:0] == i0r.rs1[4:0]);
   assign i0_rs1_depend_i1_e4 = dec_i0_rs1_en_d & e4d.i1v & (e4d.i1rd[4:0] == i0r.rs1[4:0]);
   assign i0_rs1_depend_i1_wb = dec_i0_rs1_en_d & wbd.i1v & (wbd.i1rd[4:0] == i0r.rs1[4:0]);
   assign i0_rs2_depend_i0_e1 = dec_i0_rs2_en_d & e1d.i0v & (e1d.i0rd[4:0] == i0r.rs2[4:0]);
   assign i0_rs2_depend_i0_e2 = dec_i0_rs2_en_d & e2d.i0v & (e2d.i0rd[4:0] == i0r.rs2[4:0]);
   assign i0_rs2_depend_i0_e3 = dec_i0_rs2_en_d & e3d.i0v & (e3d.i0rd[4:0] == i0r.rs2[4:0]);
   assign i0_rs2_depend_i0_e4 = dec_i0_rs2_en_d & e4d.i0v & (e4d.i0rd[4:0] == i0r.rs2[4:0]);
   assign i0_rs2_depend_i0_wb = dec_i0_rs2_en_d & wbd.i0v & (wbd.i0rd[4:0] == i0r.rs2[4:0]);
   assign i0_rs2_depend_i1_e1 = dec_i0_rs2_en_d & e1d.i1v & (e1d.i1rd[4:0] == i0r.rs2[4:0]);
   assign i0_rs2_depend_i1_e2 = dec_i0_rs2_en_d & e2d.i1v & (e2d.i1rd[4:0] == i0r.rs2[4:0]);
   assign i0_rs2_depend_i1_e3 = dec_i0_rs2_en_d & e3d.i1v & (e3d.i1rd[4:0] == i0r.rs2[4:0]);
   assign i0_rs2_depend_i1_e4 = dec_i0_rs2_en_d & e4d.i1v & (e4d.i1rd[4:0] == i0r.rs2[4:0]);
   assign i0_rs2_depend_i1_wb = dec_i0_rs2_en_d & wbd.i1v & (wbd.i1rd[4:0] == i0r.rs2[4:0]);
   assign i1_rs1_depend_i0_e1 = dec_i1_rs1_en_d & e1d.i0v & (e1d.i0rd[4:0] == i1r.rs1[4:0]);
   assign i1_rs1_depend_i0_e2 = dec_i1_rs1_en_d & e2d.i0v & (e2d.i0rd[4:0] == i1r.rs1[4:0]);
   assign i1_rs1_depend_i0_e3 = dec_i1_rs1_en_d & e3d.i0v & (e3d.i0rd[4:0] == i1r.rs1[4:0]);
   assign i1_rs1_depend_i0_e4 = dec_i1_rs1_en_d & e4d.i0v & (e4d.i0rd[4:0] == i1r.rs1[4:0]);
   assign i1_rs1_depend_i0_wb = dec_i1_rs1_en_d & wbd.i0v & (wbd.i0rd[4:0] == i1r.rs1[4:0]);
   assign i1_rs1_depend_i1_e1 = dec_i1_rs1_en_d & e1d.i1v & (e1d.i1rd[4:0] == i1r.rs1[4:0]);
   assign i1_rs1_depend_i1_e2 = dec_i1_rs1_en_d & e2d.i1v & (e2d.i1rd[4:0] == i1r.rs1[4:0]);
   assign i1_rs1_depend_i1_e3 = dec_i1_rs1_en_d & e3d.i1v & (e3d.i1rd[4:0] == i1r.rs1[4:0]);
   assign i1_rs1_depend_i1_e4 = dec_i1_rs1_en_d & e4d.i1v & (e4d.i1rd[4:0] == i1r.rs1[4:0]);
   assign i1_rs1_depend_i1_wb = dec_i1_rs1_en_d & wbd.i1v & (wbd.i1rd[4:0] == i1r.rs1[4:0]);
   assign i1_rs2_depend_i0_e1 = dec_i1_rs2_en_d & e1d.i0v & (e1d.i0rd[4:0] == i1r.rs2[4:0]);
   assign i1_rs2_depend_i0_e2 = dec_i1_rs2_en_d & e2d.i0v & (e2d.i0rd[4:0] == i1r.rs2[4:0]);
   assign i1_rs2_depend_i0_e3 = dec_i1_rs2_en_d & e3d.i0v & (e3d.i0rd[4:0] == i1r.rs2[4:0]);
   assign i1_rs2_depend_i0_e4 = dec_i1_rs2_en_d & e4d.i0v & (e4d.i0rd[4:0] == i1r.rs2[4:0]);
   assign i1_rs2_depend_i0_wb = dec_i1_rs2_en_d & wbd.i0v & (wbd.i0rd[4:0] == i1r.rs2[4:0]);
   assign i1_rs2_depend_i1_e1 = dec_i1_rs2_en_d & e1d.i1v & (e1d.i1rd[4:0] == i1r.rs2[4:0]);
   assign i1_rs2_depend_i1_e2 = dec_i1_rs2_en_d & e2d.i1v & (e2d.i1rd[4:0] == i1r.rs2[4:0]);
   assign i1_rs2_depend_i1_e3 = dec_i1_rs2_en_d & e3d.i1v & (e3d.i1rd[4:0] == i1r.rs2[4:0]);
   assign i1_rs2_depend_i1_e4 = dec_i1_rs2_en_d & e4d.i1v & (e4d.i1rd[4:0] == i1r.rs2[4:0]);
   assign i1_rs2_depend_i1_wb = dec_i1_rs2_en_d & wbd.i1v & (wbd.i1rd[4:0] == i1r.rs2[4:0]);
   rvdff #(2) freezeff (.*, .clk(active_clk), .din({freeze,freeze_prior1}), .dout({freeze_prior1,freeze_prior2}));
   assign freeze_after_unfreeze1 = freeze & ~freeze_prior1;
   assign freeze_after_unfreeze2 = freeze & ~freeze_prior1 & ~freeze_prior2;
   assign unfreeze_cycle1 = ~freeze & freeze_prior1;
   assign unfreeze_cycle2 = ~freeze & ~freeze_prior1 & freeze_prior2;
   rvdffe #(32) freeze_i0_e4ff (.*, .en(freeze_after_unfreeze1), .din(i0_result_e4_final[31:0]), .dout(i0_result_e4_freeze[31:0]));
   rvdffe #(32) freeze_i1_e4ff (.*, .en(freeze_after_unfreeze1), .din(i1_result_e4_final[31:0]), .dout(i1_result_e4_freeze[31:0]));
   rvdffe #(32) freeze_i0_wbff (.*,
                              .en(freeze_after_unfreeze1),
                              .din( (freeze_after_unfreeze2) ? i0_result_wb[31:0] : i0_result_e4_freeze[31:0]),
                              .dout(i0_result_wb_freeze[31:0])
                              );
   rvdffe #(32) freeze_i1_wbff (.*,
                              .en(freeze_after_unfreeze1),
                              .din( (freeze_after_unfreeze2) ? i1_result_wb[31:0] : i1_result_e4_freeze[31:0]),
                              .dout(i1_result_wb_freeze[31:0])
                              );
   assign dd.i0rs1bype2[1:0] = {  i0_dp.alu & i0_rs1_depth_d[3:0] == 4'd5 & i0_rs1_class_d.sec,
                                  i0_dp.alu & i0_rs1_depth_d[3:0] == 4'd6 & i0_rs1_class_d.sec };
   assign dd.i0rs2bype2[1:0] = {  i0_dp.alu & i0_rs2_depth_d[3:0] == 4'd5 & i0_rs2_class_d.sec,
                                  i0_dp.alu & i0_rs2_depth_d[3:0] == 4'd6 & i0_rs2_class_d.sec };
   assign dd.i1rs1bype2[1:0] = {  i1_dp.alu & i1_rs1_depth_d[3:0] == 4'd5 & i1_rs1_class_d.sec,
                                  i1_dp.alu & i1_rs1_depth_d[3:0] == 4'd6 & i1_rs1_class_d.sec };
   assign dd.i1rs2bype2[1:0] = {  i1_dp.alu & i1_rs2_depth_d[3:0] == 4'd5 & i1_rs2_class_d.sec,
                                  i1_dp.alu & i1_rs2_depth_d[3:0] == 4'd6 & i1_rs2_class_d.sec };
   assign i1_result_wb_eff[31:0] = (unfreeze_cycle1) ? i1_result_wb_freeze[31:0] :
                                   (unfreeze_cycle2) ? i1_result_e4_freeze[31:0] :
                                   i1_result_wb[31:0];
   assign i0_result_wb_eff[31:0] = (unfreeze_cycle1) ? i0_result_wb_freeze[31:0] :
                                   (unfreeze_cycle2) ? i0_result_e4_freeze[31:0] :
                                   i0_result_wb[31:0];
   assign i0_rs1_bypass_data_e2[31:0] = ({32{e2d.i0rs1bype2[1]}} & i1_result_wb_eff[31:0]) |
                                        ({32{e2d.i0rs1bype2[0]}} & i0_result_wb_eff[31:0]);
   assign i0_rs2_bypass_data_e2[31:0] = ({32{e2d.i0rs2bype2[1]}} & i1_result_wb_eff[31:0]) |
                                        ({32{e2d.i0rs2bype2[0]}} & i0_result_wb_eff[31:0]);
   assign i1_rs1_bypass_data_e2[31:0] = ({32{e2d.i1rs1bype2[1]}} & i1_result_wb_eff[31:0]) |
                                        ({32{e2d.i1rs1bype2[0]}} & i0_result_wb_eff[31:0]);
   assign i1_rs2_bypass_data_e2[31:0] = ({32{e2d.i1rs2bype2[1]}} & i1_result_wb_eff[31:0]) |
                                        ({32{e2d.i1rs2bype2[0]}} & i0_result_wb_eff[31:0]);
   assign dec_i0_rs1_bypass_en_e2 = |e2d.i0rs1bype2[1:0];
   assign dec_i0_rs2_bypass_en_e2 = |e2d.i0rs2bype2[1:0];
   assign dec_i1_rs1_bypass_en_e2 = |e2d.i1rs1bype2[1:0];
   assign dec_i1_rs2_bypass_en_e2 = |e2d.i1rs2bype2[1:0];
   assign i1_rs1_depend_i0_d = dec_i1_rs1_en_d & i0_dp.rd & (i1r.rs1[4:0] == i0r.rd[4:0]);
   assign i1_rs2_depend_i0_d = dec_i1_rs2_en_d & i0_dp.rd & (i1r.rs2[4:0] == i0r.rd[4:0]);
   assign dd.i0rs1bype3[3:0] = { i0_dp.alu & i0_rs1_depth_d[3:0]==4'd1 & (i0_rs1_class_d.sec | i0_rs1_class_d.load | i0_rs1_class_d.mul),
                                 i0_dp.alu & i0_rs1_depth_d[3:0]==4'd2 & (i0_rs1_class_d.sec | i0_rs1_class_d.load | i0_rs1_class_d.mul),
                                 i0_dp.alu & i0_rs1_depth_d[3:0]==4'd3 & (i0_rs1_class_d.sec | i0_rs1_class_d.load | i0_rs1_class_d.mul),
                                 i0_dp.alu & i0_rs1_depth_d[3:0]==4'd4 & (i0_rs1_class_d.sec | i0_rs1_class_d.load | i0_rs1_class_d.mul) };
   assign dd.i0rs2bype3[3:0] = { i0_dp.alu & i0_rs2_depth_d[3:0]==4'd1 & (i0_rs2_class_d.sec | i0_rs2_class_d.load | i0_rs2_class_d.mul),
                                 i0_dp.alu & i0_rs2_depth_d[3:0]==4'd2 & (i0_rs2_class_d.sec | i0_rs2_class_d.load | i0_rs2_class_d.mul),
                                 i0_dp.alu & i0_rs2_depth_d[3:0]==4'd3 & (i0_rs2_class_d.sec | i0_rs2_class_d.load | i0_rs2_class_d.mul),
                                 i0_dp.alu & i0_rs2_depth_d[3:0]==4'd4 & (i0_rs2_class_d.sec | i0_rs2_class_d.load | i0_rs2_class_d.mul) };
   assign i1rs1_intra[2:0] = {   i1_dp.alu & i0_dp.alu  & i1_rs1_depend_i0_d,
                                 i1_dp.alu & i0_dp.mul  & i1_rs1_depend_i0_d,
                                 i1_dp.alu & i0_dp.load & i1_rs1_depend_i0_d
                                 };
   assign i1rs2_intra[2:0] = {   i1_dp.alu & i0_dp.alu  & i1_rs2_depend_i0_d,
                                 i1_dp.alu & i0_dp.mul  & i1_rs2_depend_i0_d,
                                 i1_dp.alu & i0_dp.load & i1_rs2_depend_i0_d
                                 };
   assign i1_rs1_intra_bypass = |i1rs1_intra[2:0];
   assign i1_rs2_intra_bypass = |i1rs2_intra[2:0];
   assign dd.i1rs1bype3[6:0] = { i1rs1_intra[2:0],
                                 i1_dp.alu & i1_rs1_depth_d[3:0]==4'd1 & (i1_rs1_class_d.sec | i1_rs1_class_d.load | i1_rs1_class_d.mul) & ~i1_rs1_intra_bypass,
                                 i1_dp.alu & i1_rs1_depth_d[3:0]==4'd2 & (i1_rs1_class_d.sec | i1_rs1_class_d.load | i1_rs1_class_d.mul) & ~i1_rs1_intra_bypass,
                                 i1_dp.alu & i1_rs1_depth_d[3:0]==4'd3 & (i1_rs1_class_d.sec | i1_rs1_class_d.load | i1_rs1_class_d.mul) & ~i1_rs1_intra_bypass,
                                 i1_dp.alu & i1_rs1_depth_d[3:0]==4'd4 & (i1_rs1_class_d.sec | i1_rs1_class_d.load | i1_rs1_class_d.mul) & ~i1_rs1_intra_bypass };
   assign dd.i1rs2bype3[6:0] = { i1rs2_intra[2:0],
                                 i1_dp.alu & i1_rs2_depth_d[3:0]==4'd1 & (i1_rs2_class_d.sec | i1_rs2_class_d.load | i1_rs2_class_d.mul) & ~i1_rs2_intra_bypass,
                                 i1_dp.alu & i1_rs2_depth_d[3:0]==4'd2 & (i1_rs2_class_d.sec | i1_rs2_class_d.load | i1_rs2_class_d.mul) & ~i1_rs2_intra_bypass,
                                 i1_dp.alu & i1_rs2_depth_d[3:0]==4'd3 & (i1_rs2_class_d.sec | i1_rs2_class_d.load | i1_rs2_class_d.mul) & ~i1_rs2_intra_bypass,
                                 i1_dp.alu & i1_rs2_depth_d[3:0]==4'd4 & (i1_rs2_class_d.sec | i1_rs2_class_d.load | i1_rs2_class_d.mul) & ~i1_rs2_intra_bypass };
   assign dec_i0_rs1_bypass_en_e3 = |e3d.i0rs1bype3[3:0];
   assign dec_i0_rs2_bypass_en_e3 = |e3d.i0rs2bype3[3:0];
   assign dec_i1_rs1_bypass_en_e3 = |e3d.i1rs1bype3[6:0];
   assign dec_i1_rs2_bypass_en_e3 = |e3d.i1rs2bype3[6:0];
   assign i1_result_e4_eff[31:0] = (unfreeze_cycle1) ? i1_result_e4_freeze[31:0] :
                                   i1_result_e4_final[31:0];
   assign i0_result_e4_eff[31:0] = (unfreeze_cycle1) ? i0_result_e4_freeze[31:0] :
                                   i0_result_e4_final[31:0];
   assign i0_rs1_bypass_data_e3[31:0] = ({32{e3d.i0rs1bype3[3]}} & i1_result_e4_eff[31:0]) |
                                        ({32{e3d.i0rs1bype3[2]}} & i0_result_e4_eff[31:0]) |
                                        ({32{e3d.i0rs1bype3[1]}} & i1_result_wb_eff[31:0]) |
                                        ({32{e3d.i0rs1bype3[0]}} & i0_result_wb_eff[31:0]);
   assign i0_rs2_bypass_data_e3[31:0] = ({32{e3d.i0rs2bype3[3]}} & i1_result_e4_eff[31:0]) |
                                        ({32{e3d.i0rs2bype3[2]}} & i0_result_e4_eff[31:0]) |
                                        ({32{e3d.i0rs2bype3[1]}} & i1_result_wb_eff[31:0]) |
                                        ({32{e3d.i0rs2bype3[0]}} & i0_result_wb_eff[31:0]);
   assign i1_rs1_bypass_data_e3[31:0] = ({32{e3d.i1rs1bype3[6]}} & i0_result_e3[31:0]) |
                                        ({32{e3d.i1rs1bype3[5]}} & exu_mul_result_e3[31:0]) |
                                        ({32{e3d.i1rs1bype3[4]}} & lsu_result_dc3[31:0]) |
                                        ({32{e3d.i1rs1bype3[3]}} & i1_result_e4_eff[31:0]) |
                                        ({32{e3d.i1rs1bype3[2]}} & i0_result_e4_eff[31:0]) |
                                        ({32{e3d.i1rs1bype3[1]}} & i1_result_wb_eff[31:0]) |
                                        ({32{e3d.i1rs1bype3[0]}} & i0_result_wb_eff[31:0]);
   assign i1_rs2_bypass_data_e3[31:0] = ({32{e3d.i1rs2bype3[6]}} & i0_result_e3[31:0]) |
                                        ({32{e3d.i1rs2bype3[5]}} & exu_mul_result_e3[31:0]) |
                                        ({32{e3d.i1rs2bype3[4]}} & lsu_result_dc3[31:0]) |
                                        ({32{e3d.i1rs2bype3[3]}} & i1_result_e4_eff[31:0]) |
                                        ({32{e3d.i1rs2bype3[2]}} & i0_result_e4_eff[31:0]) |
                                        ({32{e3d.i1rs2bype3[1]}} & i1_result_wb_eff[31:0]) |
                                        ({32{e3d.i1rs2bype3[0]}} & i0_result_wb_eff[31:0]);
   assign {i0_rs1_class_d, i0_rs1_depth_d[3:0]} =
                                                  (i0_rs1_depend_i1_e1) ? { i1_e1c, 4'd1 } :
                                                  (i0_rs1_depend_i0_e1) ? { i0_e1c, 4'd2 } :
                                                  (i0_rs1_depend_i1_e2) ? { i1_e2c, 4'd3 } :
                                                  (i0_rs1_depend_i0_e2) ? { i0_e2c, 4'd4 } :
                                                  (i0_rs1_depend_i1_e3) ? { i1_e3c, 4'd5 } :
                                                  (i0_rs1_depend_i0_e3) ? { i0_e3c, 4'd6 } :
                                                  (i0_rs1_depend_i1_e4) ? { i1_e4c, 4'd7 } :
                                                  (i0_rs1_depend_i0_e4) ? { i0_e4c, 4'd8 } :
                                                  (i0_rs1_depend_i1_wb) ? { i1_wbc, 4'd9 } :
                                                  (i0_rs1_depend_i0_wb) ? { i0_wbc, 4'd10 } : '0;
   assign {i0_rs2_class_d, i0_rs2_depth_d[3:0]} =
                                                  (i0_rs2_depend_i1_e1) ? { i1_e1c, 4'd1 } :
                                                  (i0_rs2_depend_i0_e1) ? { i0_e1c, 4'd2 } :
                                                  (i0_rs2_depend_i1_e2) ? { i1_e2c, 4'd3 } :
                                                  (i0_rs2_depend_i0_e2) ? { i0_e2c, 4'd4 } :
                                                  (i0_rs2_depend_i1_e3) ? { i1_e3c, 4'd5 } :
                                                  (i0_rs2_depend_i0_e3) ? { i0_e3c, 4'd6 } :
                                                  (i0_rs2_depend_i1_e4) ? { i1_e4c, 4'd7 } :
                                                  (i0_rs2_depend_i0_e4) ? { i0_e4c, 4'd8 } :
                                                  (i0_rs2_depend_i1_wb) ? { i1_wbc, 4'd9 } :
                                                  (i0_rs2_depend_i0_wb) ? { i0_wbc, 4'd10 } : '0;
   assign {i1_rs1_class_d, i1_rs1_depth_d[3:0]} =
                                                  (i1_rs1_depend_i1_e1) ? { i1_e1c, 4'd1 } :
                                                  (i1_rs1_depend_i0_e1) ? { i0_e1c, 4'd2 } :
                                                  (i1_rs1_depend_i1_e2) ? { i1_e2c, 4'd3 } :
                                                  (i1_rs1_depend_i0_e2) ? { i0_e2c, 4'd4 } :
                                                  (i1_rs1_depend_i1_e3) ? { i1_e3c, 4'd5 } :
                                                  (i1_rs1_depend_i0_e3) ? { i0_e3c, 4'd6 } :
                                                  (i1_rs1_depend_i1_e4) ? { i1_e4c, 4'd7 } :
                                                  (i1_rs1_depend_i0_e4) ? { i0_e4c, 4'd8 } :
                                                  (i1_rs1_depend_i1_wb) ? { i1_wbc, 4'd9 } :
                                                  (i1_rs1_depend_i0_wb) ? { i0_wbc, 4'd10 } : '0;
   assign {i1_rs2_class_d, i1_rs2_depth_d[3:0]} =
                                                  (i1_rs2_depend_i1_e1) ? { i1_e1c, 4'd1 } :
                                                  (i1_rs2_depend_i0_e1) ? { i0_e1c, 4'd2 } :
                                                  (i1_rs2_depend_i1_e2) ? { i1_e2c, 4'd3 } :
                                                  (i1_rs2_depend_i0_e2) ? { i0_e2c, 4'd4 } :
                                                  (i1_rs2_depend_i1_e3) ? { i1_e3c, 4'd5 } :
                                                  (i1_rs2_depend_i0_e3) ? { i0_e3c, 4'd6 } :
                                                  (i1_rs2_depend_i1_e4) ? { i1_e4c, 4'd7 } :
                                                  (i1_rs2_depend_i0_e4) ? { i0_e4c, 4'd8 } :
                                                  (i1_rs2_depend_i1_wb) ? { i1_wbc, 4'd9 } :
                                                  (i1_rs2_depend_i0_wb) ? { i0_wbc, 4'd10 } : '0;
   assign i0_rs1_match_e1 = (i0_rs1_depth_d[3:0] == 4'd1 |
                             i0_rs1_depth_d[3:0] == 4'd2);
   assign i0_rs1_match_e2 = (i0_rs1_depth_d[3:0] == 4'd3 |
                             i0_rs1_depth_d[3:0] == 4'd4);
   assign i0_rs1_match_e3 = (i0_rs1_depth_d[3:0] == 4'd5 |
                             i0_rs1_depth_d[3:0] == 4'd6);
   assign i0_rs2_match_e1 = (i0_rs2_depth_d[3:0] == 4'd1 |
                             i0_rs2_depth_d[3:0] == 4'd2);
   assign i0_rs2_match_e2 = (i0_rs2_depth_d[3:0] == 4'd3 |
                             i0_rs2_depth_d[3:0] == 4'd4);
   assign i0_rs2_match_e3 = (i0_rs2_depth_d[3:0] == 4'd5 |
                             i0_rs2_depth_d[3:0] == 4'd6);
   assign i0_rs1_match_e1_e2 = i0_rs1_match_e1 | i0_rs1_match_e2;
   assign i0_rs1_match_e1_e3 = i0_rs1_match_e1 | i0_rs1_match_e2 | i0_rs1_match_e3;
   assign i0_rs2_match_e1_e2 = i0_rs2_match_e1 | i0_rs2_match_e2;
   assign i0_rs2_match_e1_e3 = i0_rs2_match_e1 | i0_rs2_match_e2 | i0_rs2_match_e3;
   assign i0_secondary_d = ((i0_dp.alu & (i0_rs1_class_d.load | i0_rs1_class_d.mul) & i0_rs1_match_e1_e2) |
                            (i0_dp.alu & (i0_rs2_class_d.load | i0_rs2_class_d.mul) & i0_rs2_match_e1_e2) |
                            (i0_dp.alu & i0_rs1_class_d.sec & i0_rs1_match_e1_e3) |
                            (i0_dp.alu & i0_rs2_class_d.sec & i0_rs2_match_e1_e3)) & ~disable_secondary;
   assign i0_secondary_stall_d = ((i0_dp.alu & i1_rs1_depend_i0_d & ~i1_dp.alu & i0_secondary_d) |
                                  (i0_dp.alu & i1_rs2_depend_i0_d & ~i1_dp.alu & ~i1_dp.store & i0_secondary_d)) & ~disable_secondary;
   assign i1_rs1_match_e1 = (i1_rs1_depth_d[3:0] == 4'd1 |
                             i1_rs1_depth_d[3:0] == 4'd2);
   assign i1_rs1_match_e2 = (i1_rs1_depth_d[3:0] == 4'd3 |
                             i1_rs1_depth_d[3:0] == 4'd4);
   assign i1_rs1_match_e3 = (i1_rs1_depth_d[3:0] == 4'd5 |
                             i1_rs1_depth_d[3:0] == 4'd6);
   assign i1_rs2_match_e1 = (i1_rs2_depth_d[3:0] == 4'd1 |
                             i1_rs2_depth_d[3:0] == 4'd2);
   assign i1_rs2_match_e2 = (i1_rs2_depth_d[3:0] == 4'd3 |
                             i1_rs2_depth_d[3:0] == 4'd4);
   assign i1_rs2_match_e3 = (i1_rs2_depth_d[3:0] == 4'd5 |
                             i1_rs2_depth_d[3:0] == 4'd6);
   assign i1_rs1_match_e1_e2 = i1_rs1_match_e1 | i1_rs1_match_e2;
   assign i1_rs1_match_e1_e3 = i1_rs1_match_e1 | i1_rs1_match_e2 | i1_rs1_match_e3;
   assign i1_rs2_match_e1_e2 = i1_rs2_match_e1 | i1_rs2_match_e2;
   assign i1_rs2_match_e1_e3 = i1_rs2_match_e1 | i1_rs2_match_e2 | i1_rs2_match_e3;
   assign i1_secondary_d = ((i1_dp.alu & (i1_rs1_class_d.load | i1_rs1_class_d.mul) & i1_rs1_match_e1_e2) |
                            (i1_dp.alu & (i1_rs2_class_d.load | i1_rs2_class_d.mul) & i1_rs2_match_e1_e2) |
                            (i1_dp.alu & (i1_rs1_class_d.sec) & i1_rs1_match_e1_e3) |
                            (i1_dp.alu & (i1_rs2_class_d.sec) & i1_rs2_match_e1_e3) |
                            (non_block_case_d & i1_depend_i0_d)) & ~disable_secondary;
   assign store_data_bypass_i0_e2_c2 = i0_dp.alu & ~i0_secondary_d & i1_rs2_depend_i0_d & ~i1_rs1_depend_i0_d & i1_dp.store;
   assign non_block_case_d = (   
                                (i1_dp.alu & i0_dp.load) |
                                (i1_dp.alu & i0_dp.mul)
                                ) & ~disable_secondary;
   assign store_data_bypass_c2 =  ((             i0_dp.store & i0_rs2_depth_d[3:0] == 4'd1 & i0_rs2_class_d.load) |
                              (             i0_dp.store & i0_rs2_depth_d[3:0] == 4'd2 & i0_rs2_class_d.load) |
                              (~i0_dp.lsu & i1_dp.store & i1_rs2_depth_d[3:0] == 4'd1 & i1_rs2_class_d.load) |
                              (~i0_dp.lsu & i1_dp.store & i1_rs2_depth_d[3:0] == 4'd2 & i1_rs2_class_d.load));
   assign store_data_bypass_c1 =  ((             i0_dp.store & i0_rs2_depth_d[3:0] == 4'd3 & i0_rs2_class_d.load) |
                              (             i0_dp.store & i0_rs2_depth_d[3:0] == 4'd4 & i0_rs2_class_d.load) |
                              (~i0_dp.lsu & i1_dp.store & i1_rs2_depth_d[3:0] == 4'd3 & i1_rs2_class_d.load) |
                              (~i0_dp.lsu & i1_dp.store & i1_rs2_depth_d[3:0] == 4'd4 & i1_rs2_class_d.load));
   assign load_ldst_bypass_c1 =  ((         (i0_dp.load | i0_dp.store) & i0_rs1_depth_d[3:0] == 4'd3 & i0_rs1_class_d.load) |
                              (             (i0_dp.load | i0_dp.store) & i0_rs1_depth_d[3:0] == 4'd4 & i0_rs1_class_d.load) |
                              (~i0_dp.lsu & (i1_dp.load | i1_dp.store) & i1_rs1_depth_d[3:0] == 4'd3 & i1_rs1_class_d.load) |
                              (~i0_dp.lsu & (i1_dp.load | i1_dp.store) & i1_rs1_depth_d[3:0] == 4'd4 & i1_rs1_class_d.load));
   assign load_mul_rs1_bypass_e1 =  ((             (i0_dp.mul) & i0_rs1_depth_d[3:0] == 4'd3 & i0_rs1_class_d.load) |
                                     (             (i0_dp.mul) & i0_rs1_depth_d[3:0] == 4'd4 & i0_rs1_class_d.load) |
                                     (~i0_dp.mul & (i1_dp.mul) & i1_rs1_depth_d[3:0] == 4'd3 & i1_rs1_class_d.load) |
                                     (~i0_dp.mul & (i1_dp.mul) & i1_rs1_depth_d[3:0] == 4'd4 & i1_rs1_class_d.load));
   assign load_mul_rs2_bypass_e1 =  ((             (i0_dp.mul) & i0_rs2_depth_d[3:0] == 4'd3 & i0_rs2_class_d.load) |
                                     (             (i0_dp.mul) & i0_rs2_depth_d[3:0] == 4'd4 & i0_rs2_class_d.load) |
                                     (~i0_dp.mul & (i1_dp.mul) & i1_rs2_depth_d[3:0] == 4'd3 & i1_rs2_class_d.load) |
                                     (~i0_dp.mul & (i1_dp.mul) & i1_rs2_depth_d[3:0] == 4'd4 & i1_rs2_class_d.load));
   assign store_data_bypass_e4_c3[1:0] = {
                                      ( ~i0_dp.lsu & i1_dp.store & i1_rs2_depth_d[3:0] == 4'd1 & i1_rs2_class_d.sec ) |
                                      (              i0_dp.store & i0_rs2_depth_d[3:0] == 4'd1 & i0_rs2_class_d.sec ),
                                      ( ~i0_dp.lsu & i1_dp.store & i1_rs2_depth_d[3:0] == 4'd2 & i1_rs2_class_d.sec ) |
                                      (              i0_dp.store & i0_rs2_depth_d[3:0] == 4'd2 & i0_rs2_class_d.sec )
                                     };
   assign store_data_bypass_e4_c2[1:0] = {
                                      ( ~i0_dp.lsu & i1_dp.store & i1_rs2_depth_d[3:0] == 4'd3 & i1_rs2_class_d.sec ) |
                                      (              i0_dp.store & i0_rs2_depth_d[3:0] == 4'd3 & i0_rs2_class_d.sec ),
                                      ( ~i0_dp.lsu & i1_dp.store & i1_rs2_depth_d[3:0] == 4'd4 & i1_rs2_class_d.sec ) |
                                      (              i0_dp.store & i0_rs2_depth_d[3:0] == 4'd4 & i0_rs2_class_d.sec )
                                     };
   assign store_data_bypass_e4_c1[1:0] = {
                                      ( ~i0_dp.lsu & i1_dp.store & i1_rs2_depth_d[3:0] == 4'd5 & i1_rs2_class_d.sec ) |
                                      (              i0_dp.store & i0_rs2_depth_d[3:0] == 4'd5 & i0_rs2_class_d.sec ),
                                      ( ~i0_dp.lsu & i1_dp.store & i1_rs2_depth_d[3:0] == 4'd6 & i1_rs2_class_d.sec ) |
                                      (              i0_dp.store & i0_rs2_depth_d[3:0] == 4'd6 & i0_rs2_class_d.sec )
                                     };
   assign i0_not_alu_eff = (~i0_dp.alu | disable_secondary);
   assign i1_not_alu_eff = (~i1_dp.alu | disable_secondary);
   assign i0_load_block_d = (i0_not_alu_eff & i0_rs1_class_d.load & i0_rs1_match_e1) |
                            (i0_not_alu_eff & i0_rs1_class_d.load & i0_rs1_match_e2 & ~i0_dp.load & ~i0_dp.store & ~i0_dp.mul) |  
                            (i0_not_alu_eff & i0_rs2_class_d.load & i0_rs2_match_e1 & ~i0_dp.store) |
                            (i0_not_alu_eff & i0_rs2_class_d.load & i0_rs2_match_e2 & ~i0_dp.store & ~i0_dp.mul);
   assign i1_load_block_d = (i1_not_alu_eff & i1_rs1_class_d.load & i1_rs1_match_e1) |
                            (i1_not_alu_eff & i1_rs1_class_d.load & i1_rs1_match_e2 & ~i1_dp.load & ~i1_dp.store & ~i1_dp.mul) |
                            (i1_not_alu_eff & i1_rs2_class_d.load & i1_rs2_match_e1 & ~i1_dp.store) |
                            (i1_not_alu_eff & i1_rs2_class_d.load & i1_rs2_match_e2 & ~i1_dp.store & ~i1_dp.mul);
   assign i0_mul_block_d = (i0_not_alu_eff & i0_rs1_class_d.mul & i0_rs1_match_e1_e2) |
                           (i0_not_alu_eff & i0_rs2_class_d.mul & i0_rs2_match_e1_e2);
   assign i1_mul_block_d = (i1_not_alu_eff & i1_rs1_class_d.mul & i1_rs1_match_e1_e2) |
                           (i1_not_alu_eff & i1_rs2_class_d.mul & i1_rs2_match_e1_e2);
   assign i0_secondary_block_d = ((~i0_dp.alu & i0_rs1_class_d.sec & i0_rs1_match_e1_e3) |
                                  (~i0_dp.alu & i0_rs2_class_d.sec & i0_rs2_match_e1_e3 & ~i0_dp.store)) & ~disable_secondary;
   assign i1_secondary_block_d = ((~i1_dp.alu & i1_rs1_class_d.sec & i1_rs1_match_e1_e3) |
                                  (~i1_dp.alu & i1_rs2_class_d.sec & i1_rs2_match_e1_e3 & ~i1_dp.store) & ~disable_secondary);
   assign dec_div_decode_e4 = e4d.i0div;
   assign dec_tlu_i0_valid_e4 = (e4d.i0valid & ~e4d.i0div & ~flush_lower_wb) | exu_div_finish;
   assign dec_tlu_i1_valid_e4 = e4d.i1valid & ~flush_lower_wb;
   assign dt.legal     =  i0_legal_decode_d                ;
   assign dt.icaf      =  i0_icaf_d & i0_legal_decode_d;             
   assign dt.icaf_second   =  dec_i0_icaf_second_d & i0_legal_decode_d;      
   assign dt.perr      =   dec_i0_perr_d & i0_legal_decode_d;
   assign dt.sbecc     =   dec_i0_sbecc_d & i0_legal_decode_d;
   assign dt.fence_i   = (i0_dp.fence_i | debug_fence_i) & i0_legal_decode_d;
   assign dt.pmu_i0_itype = i0_itype;
   assign dt.pmu_i1_itype = i1_itype;
   assign dt.pmu_i0_br_unpred = i0_br_unpred;
   assign dt.pmu_i1_br_unpred = i1_br_unpred;
   assign dt.pmu_divide = 1'b0;
   assign dt.pmu_lsu_misaligned = 1'b0;
   assign dt.i0trigger[3:0] = dec_i0_trigger_match_d[3:0] & {4{dec_i0_decode_d & ~i0_div_decode_d}};
   assign dt.i1trigger[3:0] = dec_i1_trigger_match_d[3:0] & {4{dec_i1_decode_d}};
   rvdffe #( $bits(trap_pkt_t) ) trap_e1ff (.*, .en(i0_e1_ctl_en), .din( dt),  .dout(e1t));
   always_comb begin
      e1t_in = e1t;
      e1t_in.i0trigger[3:0] = e1t.i0trigger & ~{4{flush_final_e3}};
      e1t_in.i1trigger[3:0] = e1t.i1trigger & ~{4{flush_final_e3}};
   end
   rvdffe #( $bits(trap_pkt_t) ) trap_e2ff (.*, .en(i0_e2_ctl_en), .din(e1t_in),  .dout(e2t));
   always_comb begin
      e2t_in = e2t;
      e2t_in.i0trigger[3:0] = e2t.i0trigger & ~{4{flush_final_e3 | flush_lower_wb}};
      e2t_in.i1trigger[3:0] = e2t.i1trigger & ~{4{flush_final_e3 | flush_lower_wb}};
   end
   rvdffe  #($bits(trap_pkt_t) ) trap_e3ff (.*, .en(i0_e3_ctl_en), .din(e2t_in),  .dout(e3t));
    always_comb begin
      e3t_in = e3t;
       e3t_in.i0trigger[3:0] = ({4{(e3d.i0load | e3d.i0store)}} & lsu_trigger_match_dc3[3:0]) | e3t.i0trigger[3:0];
       e3t_in.i1trigger[3:0] = ~{4{i0_flush_final_e3}} & (({4{~(e3d.i0load | e3d.i0store)}} & lsu_trigger_match_dc3[3:0]) | e3t.i1trigger[3:0]);
       e3t_in.pmu_lsu_misaligned = lsu_pmu_misaligned_dc3;    
      if (freeze | flush_lower_wb) e3t_in = '0 ;
   end
   rvdffe #( $bits(trap_pkt_t) ) trap_e4ff (.*, .en(i0_e4_ctl_en), .din(e3t_in),  .dout(e4t));
   assign freeze_e3 = freeze & ~freeze_before;
   rvdff #(1) freeze_before_ff (.*, .clk(active_clk), .din(freeze), .dout(freeze_before));
   rvdff #(1) freeze_e4_ff     (.*, .clk(active_clk), .din(freeze_e3), .dout(freeze_e4));
   rvdffe #(9) e4_trigger_ff   (.*, .en(freeze_e3), .din({e3d.i0load,e3t.i0trigger[3:0],e3t.i1trigger[3:0]}), .dout({e4d_i0load,e4t_i0trigger[3:0],e4t_i1trigger[3:0]}));
   always_comb begin
      if (exu_div_finish)     
        dec_tlu_packet_e4 = '0;
      else
        dec_tlu_packet_e4 = e4t;
      dec_tlu_packet_e4.legal = e4t.legal | exu_div_finish;
      dec_tlu_packet_e4.i0trigger[3:0] = (exu_div_finish) ? div_trigger[3:0] : e4t.i0trigger[3:0];
      dec_tlu_packet_e4.pmu_divide = exu_div_finish;
      if (freeze_e4) begin   
         dec_tlu_packet_e4.i0trigger[3:0] = e4t_i0trigger[3:0];
         dec_tlu_packet_e4.i1trigger[3:0] = e4t_i1trigger[3:0];
      end
   end
   assign dec_i0_load_e4 = e4d_i0load;
   assign i0_dc.mul   = i0_dp.mul & i0_legal_decode_d;
   assign i0_dc.load  = i0_dp.load & i0_legal_decode_d;
   assign i0_dc.sec = i0_dp.alu &  i0_secondary_d & i0_legal_decode_d;
   assign i0_dc.alu = i0_dp.alu & ~i0_secondary_d & i0_legal_decode_d;
   rvdffs #( $bits(class_pkt_t) ) i0_e1c_ff (.*, .en(i0_e1_ctl_en), .clk(active_clk), .din(i0_dc),   .dout(i0_e1c));
   rvdffs #( $bits(class_pkt_t) ) i0_e2c_ff (.*, .en(i0_e2_ctl_en), .clk(active_clk), .din(i0_e1c),  .dout(i0_e2c));
   rvdffs #( $bits(class_pkt_t) ) i0_e3c_ff (.*, .en(i0_e3_ctl_en), .clk(active_clk), .din(i0_e2c),  .dout(i0_e3c));
   assign i0_e4c_in = (freeze) ? '0 : i0_e3c;
   rvdffs  #( $bits(class_pkt_t) ) i0_e4c_ff (.*, .en(i0_e4_ctl_en),              .clk(active_clk), .din(i0_e4c_in), .dout(i0_e4c));
   rvdffs  #( $bits(class_pkt_t) ) i0_wbc_ff (.*, .en(i0_wb_ctl_en),              .clk(active_clk), .din(i0_e4c),    .dout(i0_wbc));
   assign i1_dc.mul   = i1_dp.mul & dec_i1_decode_d;
   assign i1_dc.load  = i1_dp.load & dec_i1_decode_d;
   assign i1_dc.sec = i1_dp.alu &  i1_secondary_d & dec_i1_decode_d;
   assign i1_dc.alu = i1_dp.alu & ~i1_secondary_d & dec_i1_decode_d;
   rvdffs #( $bits(class_pkt_t) ) i1_e1c_ff (.*, .en(i0_e1_ctl_en), .clk(active_clk), .din(i1_dc),   .dout(i1_e1c));
   rvdffs #( $bits(class_pkt_t) ) i1_e2c_ff (.*, .en(i0_e2_ctl_en), .clk(active_clk), .din(i1_e1c),  .dout(i1_e2c));
   rvdffs #( $bits(class_pkt_t) ) i1_e3c_ff (.*, .en(i0_e3_ctl_en), .clk(active_clk), .din(i1_e2c),  .dout(i1_e3c));
   assign i1_e4c_in = (freeze) ? '0 : i1_e3c;
   rvdffs #( $bits(class_pkt_t) ) i1_e4c_ff (.*, .en(i0_e4_ctl_en), .clk(active_clk), .din(i1_e4c_in), .dout(i1_e4c));
   rvdffs #( $bits(class_pkt_t) ) i1_wbc_ff (.*, .en(i0_wb_ctl_en), .clk(active_clk), .din(i1_e4c),    .dout(i1_wbc));
   assign dd.i0rd[4:0] = i0r.rd[4:0];
   assign dd.i0v = i0_rd_en_d & i0_legal_decode_d;
   assign dd.i0valid =              dec_i0_decode_d;   
   assign dd.i0mul = i0_dp.mul & i0_legal_decode_d;
   assign dd.i0load = i0_dp.load & i0_legal_decode_d;
   assign dd.i0store = i0_dp.store & i0_legal_decode_d;
   assign dd.i0div = i0_dp.div & i0_legal_decode_d;
   assign dd.i0secondary = i0_secondary_d & i0_legal_decode_d;
   assign dd.i1rd[4:0] = i1r.rd[4:0];
   assign dd.i1v = i1_rd_en_d & dec_i1_decode_d;
   assign dd.i1valid =              dec_i1_decode_d;
   assign dd.i1mul = i1_dp.mul;
   assign dd.i1load = i1_dp.load;
   assign dd.i1store = i1_dp.store;
   assign dd.i1secondary = i1_secondary_d & dec_i1_decode_d;
   assign dd.csrwen = dec_csr_wen_unq_d & i0_legal_decode_d;
   assign dd.csrwonly = i0_csr_write_only_d & dec_i0_decode_d;
   assign dd.csrwaddr[11:0] = i0[31:20];     
   assign i0_pipe_en[5] = dec_i0_decode_d;
   rvdffs #(3) i0cg0ff (.*, .clk(active_clk), .en(~freeze), .din(i0_pipe_en[5:3]), .dout(i0_pipe_en[4:2]));
   rvdff  #(2) i0cg1ff (.*, .clk(active_clk),               .din(i0_pipe_en[2:1]), .dout(i0_pipe_en[1:0]));
   assign i0_e1_ctl_en = (|i0_pipe_en[5:4] | clk_override) & ~freeze;
   assign i0_e2_ctl_en = (|i0_pipe_en[4:3] | clk_override) & ~freeze;
   assign i0_e3_ctl_en = (|i0_pipe_en[3:2] | clk_override) & ~freeze;
   assign i0_e4_ctl_en = (|i0_pipe_en[2:1] | clk_override);
   assign i0_wb_ctl_en = (|i0_pipe_en[1:0] | clk_override);
   assign i0_e1_data_en = (i0_pipe_en[5] | clk_override) & ~freeze;
   assign i0_e2_data_en = (i0_pipe_en[4] | clk_override) & ~freeze;
   assign i0_e3_data_en = (i0_pipe_en[3] | clk_override) & ~freeze;
   assign i0_e4_data_en = (i0_pipe_en[2] | clk_override);
   assign i0_wb_data_en = (i0_pipe_en[1] | clk_override);
   assign i0_wb1_data_en = (i0_pipe_en[0] | clk_override);
   assign dec_i0_data_en[4:2] = {i0_e1_data_en, i0_e2_data_en, i0_e3_data_en};
   assign dec_i0_ctl_en[4:1]  = {i0_e1_ctl_en, i0_e2_ctl_en, i0_e3_ctl_en, i0_e4_ctl_en};
   assign i1_pipe_en[5] = dec_i1_decode_d;
   rvdffs #(3) i1cg0ff (.*, .clk(free_clk), .en(~freeze), .din(i1_pipe_en[5:3]), .dout(i1_pipe_en[4:2]));
   rvdff  #(2) i1cg1ff (.*, .clk(free_clk),               .din(i1_pipe_en[2:1]), .dout(i1_pipe_en[1:0]));
   assign i1_e1_ctl_en = (|i1_pipe_en[5:4] | clk_override) & ~freeze;
   assign i1_e2_ctl_en = (|i1_pipe_en[4:3] | clk_override) & ~freeze;
   assign i1_e3_ctl_en = (|i1_pipe_en[3:2] | clk_override) & ~freeze;
   assign i1_e4_ctl_en = (|i1_pipe_en[2:1] | clk_override);
   assign i1_wb_ctl_en = (|i1_pipe_en[1:0] | clk_override);
   assign i1_e1_data_en = (i1_pipe_en[5] | clk_override) & ~freeze;
   assign i1_e2_data_en = (i1_pipe_en[4] | clk_override) & ~freeze;
   assign i1_e3_data_en = (i1_pipe_en[3] | clk_override) & ~freeze;
   assign i1_e4_data_en = (i1_pipe_en[2] | clk_override);
   assign i1_wb_data_en = (i1_pipe_en[1] | clk_override);
   assign i1_wb1_data_en = (i1_pipe_en[0] | clk_override);
   assign dec_i1_data_en[4:2] = {i1_e1_data_en, i1_e2_data_en, i1_e3_data_en};
   assign dec_i1_ctl_en[4:1]  = {i1_e1_ctl_en, i1_e2_ctl_en, i1_e3_ctl_en, i1_e4_ctl_en};
   rvdffe #( $bits(dest_pkt_t) ) e1ff (.*, .en(i0_e1_ctl_en), .din(dd),  .dout(e1d));
   always_comb begin
      e1d_in = e1d;
      e1d_in.i0v = e1d.i0v & ~flush_final_e3;
      e1d_in.i1v = e1d.i1v & ~flush_final_e3;
      e1d_in.i0valid = e1d.i0valid & ~flush_final_e3;
      e1d_in.i1valid = e1d.i1valid & ~flush_final_e3;
      e1d_in.i0secondary = e1d.i0secondary & ~flush_final_e3;
      e1d_in.i1secondary = e1d.i1secondary & ~flush_final_e3;
   end
   assign dec_i1_valid_e1 = e1d.i1valid;
   rvdffe #( $bits(dest_pkt_t) ) e2ff (.*, .en(i0_e2_ctl_en), .din(e1d_in), .dout(e2d));
   always_comb begin
      e2d_in = e2d;
      e2d_in.i0v = e2d.i0v &         ~flush_final_e3 & ~flush_lower_wb;
      e2d_in.i1v = e2d.i1v &         ~flush_final_e3 & ~flush_lower_wb;
      e2d_in.i0valid = e2d.i0valid & ~flush_final_e3 & ~flush_lower_wb;
      e2d_in.i1valid = e2d.i1valid & ~flush_final_e3 & ~flush_lower_wb;
      e2d_in.i0secondary = e2d.i0secondary & ~flush_final_e3 & ~flush_lower_wb;
      e2d_in.i1secondary = e2d.i1secondary & ~flush_final_e3 & ~flush_lower_wb;
   end
   rvdffe #( $bits(dest_pkt_t) ) e3ff (.*, .en(i0_e3_ctl_en), .din(e2d_in), .dout(e3d));
   always_comb begin
      e3d_in = e3d;
      e3d_in.i0v = e3d.i0v                              & ~flush_lower_wb;
      e3d_in.i0valid = e3d.i0valid                      & ~flush_lower_wb;
      e3d_in.i0secondary = e3d.i0secondary & ~flush_lower_wb;
      e3d_in.i1v = e3d.i1v         & ~i0_flush_final_e3 & ~flush_lower_wb;
      e3d_in.i1valid = e3d.i1valid & ~i0_flush_final_e3 & ~flush_lower_wb;
      e3d_in.i1secondary = e3d.i1secondary & ~i0_flush_final_e3 & ~flush_lower_wb;
      if (freeze) e3d_in = '0;
   end
   assign dec_i0_sec_decode_e3 = e3d.i0secondary & ~flush_lower_wb & ~freeze;
   assign dec_i1_sec_decode_e3 = e3d.i1secondary & ~i0_flush_final_e3 & ~flush_lower_wb & ~freeze;
   rvdffe #( $bits(dest_pkt_t) ) e4ff (.*, .en(i0_e4_ctl_en), .din(e3d_in), .dout(e4d));
   always_comb begin
      if (exu_div_finish)     
        e4d_in = '0;
      else
        e4d_in = e4d;
      e4d_in.i0rd[4:0] = (exu_div_finish) ? div_waddr_wb[4:0] : e4d.i0rd[4:0];
      e4d_in.i0v = (e4d.i0v         & ~e4d.i0div & ~flush_lower_wb) | (exu_div_finish & div_waddr_wb[4:0]!=5'b0);
      e4d_in.i0valid = (e4d.i0valid              & ~flush_lower_wb) | exu_div_finish;
      e4d_in.i0secondary = e4d.i0secondary & ~flush_lower_wb & ~exu_div_finish;
      e4d_in.i0load = e4d.i0load & ~flush_lower_wb & ~exu_div_finish;
      e4d_in.i0store = e4d.i0store & ~flush_lower_wb & ~exu_div_finish;
      e4d_in.i1v = e4d.i1v         & ~flush_lower_wb;
      e4d_in.i1valid = e4d.i1valid & ~flush_lower_wb;
      e4d_in.i1secondary = e3d.i1secondary & ~flush_lower_wb;
   end
   rvdffe #( $bits(dest_pkt_t) ) wbff (.*, .en(i0_wb_ctl_en | exu_div_finish | div_wen_wb), .din(e4d_in), .dout(wbd));
   assign dec_i0_waddr_wb[4:0] = wbd.i0rd[4:0];
   assign     i0_wen_wb = wbd.i0v & ~(~dec_tlu_i1_kill_writeb_wb & ~i1_load_kill_wen & wbd.i0v & wbd.i1v & (wbd.i0rd[4:0] == wbd.i1rd[4:0])) & ~dec_tlu_i0_kill_writeb_wb;
   assign dec_i0_wen_wb = i0_wen_wb & ~i0_load_kill_wen;   
   assign dec_i0_wdata_wb[31:0] = i0_result_wb[31:0];
   assign dec_i1_waddr_wb[4:0] = wbd.i1rd[4:0];
   assign     i1_wen_wb = wbd.i1v & ~dec_tlu_i1_kill_writeb_wb;
   assign dec_i1_wen_wb = i1_wen_wb & ~i1_load_kill_wen;
   assign dec_i1_wdata_wb[31:0] = i1_result_wb[31:0];
   assign div_stall = exu_div_stall | div_stall_ff;    
   rvdff  #(1) divstallff (.*, .clk(active_clk), .din(exu_div_stall), .dout(div_stall_ff));
   assign i0_div_decode_d = i0_legal_decode_d & i0_dp.div;
   rvdffe #(31) divpcff (.*, .en(i0_div_decode_d), .din(dec_i0_pc_d[31:1]), .dout(div_pc[31:1]));
   rvdffs #(4) divtriggerff (.*, .en(i0_div_decode_d), .clk(active_clk), .din(dec_i0_trigger_match_d[3:0]), .dout(div_trigger[3:0]));
   rvdffs #(5) divwbaddrff (.*, .en(i0_div_decode_d), .clk(active_clk), .din(i0r.rd[4:0]), .dout(div_waddr_wb[4:0]));
   rvdff  #(1) divwbff (.*, .clk(active_clk), .din(exu_div_finish), .dout(div_wen_wb));
   assign i0_result_e1[31:0] = exu_i0_result_e1[31:0];
   assign i1_result_e1[31:0] = exu_i1_result_e1[31:0];
   rvdffe #(32) i0e2resultff (.*, .en(i0_e2_data_en), .din(i0_result_e1[31:0]), .dout(i0_result_e2[31:0]));
   rvdffe #(32) i1e2resultff (.*, .en(i1_e2_data_en), .din(i1_result_e1[31:0]), .dout(i1_result_e2[31:0]));
   rvdffe #(32) i0e3resultff (.*, .en(i0_e3_data_en), .din(i0_result_e2[31:0]), .dout(i0_result_e3[31:0]));
   rvdffe #(32) i1e3resultff (.*, .en(i1_e3_data_en), .din(i1_result_e2[31:0]), .dout(i1_result_e3[31:0]));
   assign i0_result_e3_final[31:0] = (e3d.i0v & e3d.i0load) ? lsu_result_dc3[31:0] : (e3d.i0v & e3d.i0mul) ? exu_mul_result_e3[31:0] : i0_result_e3[31:0];
   assign i1_result_e3_final[31:0] = (e3d.i1v & e3d.i1load) ? lsu_result_dc3[31:0] : (e3d.i1v & e3d.i1mul) ? exu_mul_result_e3[31:0] : i1_result_e3[31:0];
   rvdffe #(32) i0e4resultff (.*, .en(i0_e4_data_en), .din(i0_result_e3_final[31:0]), .dout(i0_result_e4[31:0]));
   rvdffe #(32) i1e4resultff (.*, .en(i1_e4_data_en), .din(i1_result_e3_final[31:0]), .dout(i1_result_e4[31:0]));
   assign i0_result_e4_final[31:0] =
                                     (          e4d.i0secondary) ? exu_i0_result_e4[31:0] : (e4d.i0v & e4d.i0load) ? lsu_result_corr_dc4[31:0] : i0_result_e4[31:0];
   assign i1_result_e4_final[31:0] =
                                     (e4d.i1v & e4d.i1secondary) ? exu_i1_result_e4[31:0] : (e4d.i1v & e4d.i1load) ? lsu_result_corr_dc4[31:0] :i1_result_e4[31:0];
   rvdffe #(32) i0wbresultff (.*, .en(i0_wb_data_en), .din(i0_result_e4_final[31:0]), .dout(i0_result_wb_raw[31:0]));
   rvdffe #(32) i1wbresultff (.*, .en(i1_wb_data_en), .din(i1_result_e4_final[31:0]), .dout(i1_result_wb_raw[31:0]));
   assign i0_result_wb[31:0] = (div_wen_wb) ? exu_div_result[31:0] : i0_result_wb_raw[31:0];
   assign i1_result_wb[31:0] = i1_result_wb_raw[31:0];
   rvdffe #(12) e1brpcff (.*, .en(i0_e1_data_en), .din(last_br_immed_d[12:1] ), .dout(last_br_immed_e1[12:1]));
   rvdffe #(12) e2brpcff (.*, .en(i0_e2_data_en), .din(last_br_immed_e1[12:1]), .dout(last_br_immed_e2[12:1]));
   rvdffe #(32) divinstff   (.*, .en(i0_div_decode_d), .din(i0_inst_d[31:0]), .dout(div_inst[31:0]));
   assign i0_inst_d[31:0] = (dec_i0_pc4_d) ? i0[31:0] : {16'b0, dec_i0_cinst_d[15:0] };
   rvdffe #(32) i0e1instff  (.*, .en(i0_e1_data_en), .din(i0_inst_d[31:0]),  .dout(i0_inst_e1[31:0]));
   rvdffe #(32) i0e2instff  (.*, .en(i0_e2_data_en), .din(i0_inst_e1[31:0]), .dout(i0_inst_e2[31:0]));
   rvdffe #(32) i0e3instff  (.*, .en(i0_e3_data_en), .din(i0_inst_e2[31:0]), .dout(i0_inst_e3[31:0]));
   rvdffe #(32) i0e4instff  (.*, .en(i0_e4_data_en), .din(i0_inst_e3[31:0]), .dout(i0_inst_e4[31:0]));
   rvdffe #(32) i0wbinstff  (.*, .en(i0_wb_data_en | exu_div_finish), .din( (exu_div_finish) ? div_inst[31:0] : i0_inst_e4[31:0]), .dout(i0_inst_wb[31:0]));
   rvdffe #(32) i0wb1instff (.*, .en(i0_wb1_data_en | div_wen_wb),    .din(i0_inst_wb[31:0]),                                      .dout(i0_inst_wb1[31:0]));
   assign i1_inst_d[31:0] = (dec_i1_pc4_d) ? i1[31:0] : {16'b0, dec_i1_cinst_d[15:0] };
   rvdffe #(32) i1e1instff  (.*, .en(i1_e1_data_en), .din(i1_inst_d[31:0]),  .dout(i1_inst_e1[31:0]));
   rvdffe #(32) i1e2instff  (.*, .en(i1_e2_data_en), .din(i1_inst_e1[31:0]), .dout(i1_inst_e2[31:0]));
   rvdffe #(32) i1e3instff  (.*, .en(i1_e3_data_en), .din(i1_inst_e2[31:0]), .dout(i1_inst_e3[31:0]));
   rvdffe #(32) i1e4instff  (.*, .en(i1_e4_data_en), .din(i1_inst_e3[31:0]), .dout(i1_inst_e4[31:0]));
   rvdffe #(32) i1wbinstff  (.*, .en(i1_wb_data_en), .din(i1_inst_e4[31:0]), .dout(i1_inst_wb[31:0]));
   rvdffe #(32) i1wb1instff (.*, .en(i1_wb1_data_en),.din(i1_inst_wb[31:0]), .dout(i1_inst_wb1[31:0]));
   assign dec_i0_inst_wb1[31:0] = i0_inst_wb1[31:0];
   assign dec_i1_inst_wb1[31:0] = i1_inst_wb1[31:0];
   rvdffe #(31) i0wbpcff  (.*, .en(i0_wb_data_en | exu_div_finish), .din(dec_tlu_i0_pc_e4[31:1]), .dout(i0_pc_wb[31:1]));
   rvdffe #(31) i0wb1pcff (.*, .en(i0_wb1_data_en | div_wen_wb),    .din(i0_pc_wb[31:1]),         .dout(i0_pc_wb1[31:1]));
   rvdffe #(31) i1wb1pcff (.*, .en(i1_wb1_data_en),.din(i1_pc_wb[31:1]),         .dout(i1_pc_wb1[31:1]));
   assign dec_i0_pc_wb1[31:1] = i0_pc_wb1[31:1];
   assign dec_i1_pc_wb1[31:1] = i1_pc_wb1[31:1];
   assign i0_pc_e1[31:1] = exu_i0_pc_e1[31:1];
   assign i1_pc_e1[31:1] = exu_i1_pc_e1[31:1];
   rvdffe #(31) i0e2pcff (.*, .en(i0_e2_data_en), .din(i0_pc_e1[31:1]), .dout(i0_pc_e2[31:1]));
   rvdffe #(31) i0e3pcff (.*, .en(i0_e3_data_en), .din(i0_pc_e2[31:1]), .dout(i0_pc_e3[31:1]));
   rvdffe #(31) i0e4pcff (.*, .en(i0_e4_data_en), .din(i0_pc_e3[31:1]), .dout(i0_pc_e4[31:1]));
   rvdffe #(31) i1e2pcff (.*, .en(i1_e2_data_en), .din(i1_pc_e1[31:1]), .dout(i1_pc_e2[31:1]));
   rvdffe #(31) i1e3pcff (.*, .en(i1_e3_data_en), .din(i1_pc_e2[31:1]), .dout(i1_pc_e3[31:1]));
   rvdffe #(31) i1e4pcff (.*, .en(i1_e4_data_en), .din(i1_pc_e3[31:1]), .dout(i1_pc_e4[31:1]));
   assign dec_i0_pc_e3[31:1] = i0_pc_e3[31:1];
   assign dec_i1_pc_e3[31:1] = i1_pc_e3[31:1];
   assign dec_tlu_i0_pc_e4[31:1] = (exu_div_finish) ? div_pc[31:1] : i0_pc_e4[31:1];
   assign dec_tlu_i1_pc_e4[31:1] = i1_pc_e4[31:1];
   assign last_pc_e2[31:1] = (e2d.i1valid) ? i1_pc_e2[31:1] : i0_pc_e2[31:1];
   rvbradder ibradder_correct (
                     .pc(last_pc_e2[31:1]),
                     .offset(last_br_immed_e2[12:1]),
                     .dout(pred_correct_npc_e2[31:1])
                     );
   rvdffe #(31) i1wbpcff (.*, .en(i1_wb_data_en), .din(dec_tlu_i1_pc_e4[31:1]), .dout(i1_pc_wb[31:1]));
   assign i0_rs1bypass[9:0] = {   i0_rs1_depth_d[3:0] == 4'd1  &  i0_rs1_class_d.alu,
                                  i0_rs1_depth_d[3:0] == 4'd2  &  i0_rs1_class_d.alu,
                                  i0_rs1_depth_d[3:0] == 4'd3  &  i0_rs1_class_d.alu,
                                  i0_rs1_depth_d[3:0] == 4'd4  &  i0_rs1_class_d.alu,
                                  i0_rs1_depth_d[3:0] == 4'd5  & (i0_rs1_class_d.alu | i0_rs1_class_d.load | i0_rs1_class_d.mul),
                                  i0_rs1_depth_d[3:0] == 4'd6  & (i0_rs1_class_d.alu | i0_rs1_class_d.load | i0_rs1_class_d.mul),
                                  i0_rs1_depth_d[3:0] == 4'd7  & (i0_rs1_class_d.alu | i0_rs1_class_d.load | i0_rs1_class_d.mul | i0_rs1_class_d.sec),
                                  i0_rs1_depth_d[3:0] == 4'd8  & (i0_rs1_class_d.alu | i0_rs1_class_d.load | i0_rs1_class_d.mul | i0_rs1_class_d.sec),
                                  i0_rs1_depth_d[3:0] == 4'd9  & (i0_rs1_class_d.alu | i0_rs1_class_d.load | i0_rs1_class_d.mul | i0_rs1_class_d.sec),
                                  i0_rs1_depth_d[3:0] == 4'd10 & (i0_rs1_class_d.alu | i0_rs1_class_d.load | i0_rs1_class_d.mul | i0_rs1_class_d.sec) };
   assign i0_rs2bypass[9:0] = {   i0_rs2_depth_d[3:0] == 4'd1  &  i0_rs2_class_d.alu,
                                  i0_rs2_depth_d[3:0] == 4'd2  &  i0_rs2_class_d.alu,
                                  i0_rs2_depth_d[3:0] == 4'd3  &  i0_rs2_class_d.alu,
                                  i0_rs2_depth_d[3:0] == 4'd4  &  i0_rs2_class_d.alu,
                                  i0_rs2_depth_d[3:0] == 4'd5  & (i0_rs2_class_d.alu | i0_rs2_class_d.load | i0_rs2_class_d.mul),
                                  i0_rs2_depth_d[3:0] == 4'd6  & (i0_rs2_class_d.alu | i0_rs2_class_d.load | i0_rs2_class_d.mul),
                                  i0_rs2_depth_d[3:0] == 4'd7  & (i0_rs2_class_d.alu | i0_rs2_class_d.load | i0_rs2_class_d.mul | i0_rs2_class_d.sec),
                                  i0_rs2_depth_d[3:0] == 4'd8  & (i0_rs2_class_d.alu | i0_rs2_class_d.load | i0_rs2_class_d.mul | i0_rs2_class_d.sec),
                                  i0_rs2_depth_d[3:0] == 4'd9  & (i0_rs2_class_d.alu | i0_rs2_class_d.load | i0_rs2_class_d.mul | i0_rs2_class_d.sec),
                                  i0_rs2_depth_d[3:0] == 4'd10 & (i0_rs2_class_d.alu | i0_rs2_class_d.load | i0_rs2_class_d.mul | i0_rs2_class_d.sec) };
   assign i1_rs1bypass[9:0] = {   i1_rs1_depth_d[3:0] == 4'd1  &  i1_rs1_class_d.alu,
                                  i1_rs1_depth_d[3:0] == 4'd2  &  i1_rs1_class_d.alu,
                                  i1_rs1_depth_d[3:0] == 4'd3  &  i1_rs1_class_d.alu,
                                  i1_rs1_depth_d[3:0] == 4'd4  &  i1_rs1_class_d.alu,
                                  i1_rs1_depth_d[3:0] == 4'd5  & (i1_rs1_class_d.alu | i1_rs1_class_d.load | i1_rs1_class_d.mul),
                                  i1_rs1_depth_d[3:0] == 4'd6  & (i1_rs1_class_d.alu | i1_rs1_class_d.load | i1_rs1_class_d.mul),
                                  i1_rs1_depth_d[3:0] == 4'd7  & (i1_rs1_class_d.alu | i1_rs1_class_d.load | i1_rs1_class_d.mul | i1_rs1_class_d.sec),
                                  i1_rs1_depth_d[3:0] == 4'd8  & (i1_rs1_class_d.alu | i1_rs1_class_d.load | i1_rs1_class_d.mul | i1_rs1_class_d.sec),
                                  i1_rs1_depth_d[3:0] == 4'd9  & (i1_rs1_class_d.alu | i1_rs1_class_d.load | i1_rs1_class_d.mul | i1_rs1_class_d.sec),
                                  i1_rs1_depth_d[3:0] == 4'd10 & (i1_rs1_class_d.alu | i1_rs1_class_d.load | i1_rs1_class_d.mul | i1_rs1_class_d.sec) };
   assign i1_rs2bypass[9:0] = {   i1_rs2_depth_d[3:0] == 4'd1  &  i1_rs2_class_d.alu,
                                  i1_rs2_depth_d[3:0] == 4'd2  &  i1_rs2_class_d.alu,
                                  i1_rs2_depth_d[3:0] == 4'd3  &  i1_rs2_class_d.alu,
                                  i1_rs2_depth_d[3:0] == 4'd4  &  i1_rs2_class_d.alu,
                                  i1_rs2_depth_d[3:0] == 4'd5  & (i1_rs2_class_d.alu | i1_rs2_class_d.load | i1_rs2_class_d.mul),
                                  i1_rs2_depth_d[3:0] == 4'd6  & (i1_rs2_class_d.alu | i1_rs2_class_d.load | i1_rs2_class_d.mul),
                                  i1_rs2_depth_d[3:0] == 4'd7  & (i1_rs2_class_d.alu | i1_rs2_class_d.load | i1_rs2_class_d.mul | i1_rs2_class_d.sec),
                                  i1_rs2_depth_d[3:0] == 4'd8  & (i1_rs2_class_d.alu | i1_rs2_class_d.load | i1_rs2_class_d.mul | i1_rs2_class_d.sec),
                                  i1_rs2_depth_d[3:0] == 4'd9  & (i1_rs2_class_d.alu | i1_rs2_class_d.load | i1_rs2_class_d.mul | i1_rs2_class_d.sec),
                                  i1_rs2_depth_d[3:0] == 4'd10 & (i1_rs2_class_d.alu | i1_rs2_class_d.load | i1_rs2_class_d.mul | i1_rs2_class_d.sec) };
   assign dec_i0_rs1_bypass_en_d = |i0_rs1bypass[9:0];
   assign dec_i0_rs2_bypass_en_d = |i0_rs2bypass[9:0];
   assign dec_i1_rs1_bypass_en_d = |i1_rs1bypass[9:0];
   assign dec_i1_rs2_bypass_en_d = |i1_rs2bypass[9:0];
   assign i0_rs1_bypass_data_d[31:0] = ({32{i0_rs1bypass[9]}} & i1_result_e1[31:0]) |
                                       ({32{i0_rs1bypass[8]}} & i0_result_e1[31:0]) |
                                       ({32{i0_rs1bypass[7]}} & i1_result_e2[31:0]) |
                                       ({32{i0_rs1bypass[6]}} & i0_result_e2[31:0]) |
                                       ({32{i0_rs1bypass[5]}} & i1_result_e3_final[31:0]) |
                                       ({32{i0_rs1bypass[4]}} & i0_result_e3_final[31:0]) |
                                       ({32{i0_rs1bypass[3]}} & i1_result_e4_final[31:0]) |
                                       ({32{i0_rs1bypass[2]}} & i0_result_e4_final[31:0]) |
                                       ({32{i0_rs1bypass[1]}} & i1_result_wb[31:0]) |
                                       ({32{i0_rs1bypass[0]}} & i0_result_wb[31:0]);
   assign i0_rs2_bypass_data_d[31:0] = ({32{i0_rs2bypass[9]}} & i1_result_e1[31:0]) |
                                       ({32{i0_rs2bypass[8]}} & i0_result_e1[31:0]) |
                                       ({32{i0_rs2bypass[7]}} & i1_result_e2[31:0]) |
                                       ({32{i0_rs2bypass[6]}} & i0_result_e2[31:0]) |
                                       ({32{i0_rs2bypass[5]}} & i1_result_e3_final[31:0]) |
                                       ({32{i0_rs2bypass[4]}} & i0_result_e3_final[31:0]) |
                                       ({32{i0_rs2bypass[3]}} & i1_result_e4_final[31:0]) |
                                       ({32{i0_rs2bypass[2]}} & i0_result_e4_final[31:0]) |
                                       ({32{i0_rs2bypass[1]}} & i1_result_wb[31:0]) |
                                       ({32{i0_rs2bypass[0]}} & i0_result_wb[31:0]);
   assign i1_rs1_bypass_data_d[31:0] = ({32{i1_rs1bypass[9]}} & i1_result_e1[31:0]) |
                                       ({32{i1_rs1bypass[8]}} & i0_result_e1[31:0]) |
                                       ({32{i1_rs1bypass[7]}} & i1_result_e2[31:0]) |
                                       ({32{i1_rs1bypass[6]}} & i0_result_e2[31:0]) |
                                       ({32{i1_rs1bypass[5]}} & i1_result_e3_final[31:0]) |
                                       ({32{i1_rs1bypass[4]}} & i0_result_e3_final[31:0]) |
                                       ({32{i1_rs1bypass[3]}} & i1_result_e4_final[31:0]) |
                                       ({32{i1_rs1bypass[2]}} & i0_result_e4_final[31:0]) |
                                       ({32{i1_rs1bypass[1]}} & i1_result_wb[31:0]) |
                                       ({32{i1_rs1bypass[0]}} & i0_result_wb[31:0]);
   assign i1_rs2_bypass_data_d[31:0] = ({32{i1_rs2bypass[9]}} & i1_result_e1[31:0]) |
                                       ({32{i1_rs2bypass[8]}} & i0_result_e1[31:0]) |
                                       ({32{i1_rs2bypass[7]}} & i1_result_e2[31:0]) |
                                       ({32{i1_rs2bypass[6]}} & i0_result_e2[31:0]) |
                                       ({32{i1_rs2bypass[5]}} & i1_result_e3_final[31:0]) |
                                       ({32{i1_rs2bypass[4]}} & i0_result_e3_final[31:0]) |
                                       ({32{i1_rs2bypass[3]}} & i1_result_e4_final[31:0]) |
                                       ({32{i1_rs2bypass[2]}} & i0_result_e4_final[31:0]) |
                                       ({32{i1_rs2bypass[1]}} & i1_result_wb[31:0]) |
                                       ({32{i1_rs2bypass[0]}} & i0_result_wb[31:0]);
endmodule
module dec_dec_ctl
   import swerv_types::*;
(
   input logic [31:0] inst,
   output dec_pkt_t out
   );
   logic [31:0] i;
   assign i[31:0] = inst[31:0];
assign out.alu = (i[2]) | (i[6]) | (!i[25]&i[4]) | (!i[5]&i[4]);
assign out.rs1 = (!i[14]&!i[13]&!i[2]) | (!i[13]&i[11]&!i[2]) | (i[19]&i[13]&!i[2]) | (
    !i[13]&i[10]&!i[2]) | (i[18]&i[13]&!i[2]) | (!i[13]&i[9]&!i[2]) | (
    i[17]&i[13]&!i[2]) | (!i[13]&i[8]&!i[2]) | (i[16]&i[13]&!i[2]) | (
    !i[13]&i[7]&!i[2]) | (i[15]&i[13]&!i[2]) | (!i[4]&!i[3]) | (!i[6]
    &!i[2]);
assign out.rs2 = (i[5]&!i[4]&!i[2]) | (!i[6]&i[5]&!i[2]);
assign out.imm12 = (!i[4]&!i[3]&i[2]) | (i[13]&!i[5]&i[4]&!i[2]) | (!i[13]&!i[12]
    &i[6]&i[4]) | (!i[12]&!i[5]&i[4]&!i[2]);
assign out.rd = (!i[5]&!i[2]) | (i[5]&i[2]) | (i[4]);
assign out.shimm5 = (!i[13]&i[12]&!i[5]&i[4]&!i[2]);
assign out.imm20 = (i[5]&i[3]) | (i[4]&i[2]);
assign out.pc = (!i[5]&!i[3]&i[2]) | (i[5]&i[3]);
assign out.load = (!i[5]&!i[4]&!i[2]);
assign out.store = (!i[6]&i[5]&!i[4]);
assign out.lsu = (!i[6]&!i[4]&!i[2]);
assign out.add = (!i[14]&!i[13]&!i[12]&!i[5]&i[4]) | (!i[5]&!i[3]&i[2]) | (!i[30]
    &!i[25]&!i[14]&!i[13]&!i[12]&!i[6]&i[4]&!i[2]);
assign out.sub = (i[30]&!i[12]&!i[6]&i[5]&i[4]&!i[2]) | (!i[25]&!i[14]&i[13]&!i[6]
    &i[4]&!i[2]) | (!i[14]&i[13]&!i[5]&i[4]&!i[2]) | (i[6]&!i[4]&!i[2]);
assign out.land = (i[14]&i[13]&i[12]&!i[5]&!i[2]) | (!i[25]&i[14]&i[13]&i[12]&!i[6]
    &!i[2]);
assign out.lor = (!i[6]&i[3]) | (!i[25]&i[14]&i[13]&!i[12]&i[4]&!i[2]) | (i[5]&i[4]
    &i[2]) | (!i[12]&i[6]&i[4]) | (i[13]&i[6]&i[4]) | (i[14]&i[13]&!i[12]
    &!i[5]&!i[2]) | (i[7]&i[6]&i[4]) | (i[8]&i[6]&i[4]) | (i[9]&i[6]&i[4]) | (
    i[10]&i[6]&i[4]) | (i[11]&i[6]&i[4]);
assign out.lxor = (!i[25]&i[14]&!i[13]&!i[12]&i[4]&!i[2]) | (i[14]&!i[13]&!i[12]
    &!i[5]&i[4]&!i[2]);
assign out.sll = (!i[25]&!i[14]&!i[13]&i[12]&!i[6]&i[4]&!i[2]);
assign out.sra = (i[30]&!i[13]&i[12]&!i[6]&i[4]&!i[2]);
assign out.srl = (!i[30]&!i[25]&i[14]&!i[13]&i[12]&!i[6]&i[4]&!i[2]);
assign out.slt = (!i[25]&!i[14]&i[13]&!i[6]&i[4]&!i[2]) | (!i[14]&i[13]&!i[5]&i[4]
    &!i[2]);
assign out.unsign = (!i[14]&i[13]&i[12]&!i[5]&!i[2]) | (i[13]&i[6]&!i[4]&!i[2]) | (
    i[14]&!i[5]&!i[4]) | (!i[25]&!i[14]&i[13]&i[12]&!i[6]&!i[2]) | (
    i[25]&i[14]&i[12]&!i[6]&i[5]&!i[2]);
assign out.condbr = (i[6]&!i[4]&!i[2]);
assign out.beq = (!i[14]&!i[12]&i[6]&!i[4]&!i[2]);
assign out.bne = (!i[14]&i[12]&i[6]&!i[4]&!i[2]);
assign out.bge = (i[14]&i[12]&i[5]&!i[4]&!i[2]);
assign out.blt = (i[14]&!i[12]&i[5]&!i[4]&!i[2]);
assign out.jal = (i[6]&i[2]);
assign out.by = (!i[13]&!i[12]&!i[6]&!i[4]&!i[2]);
assign out.half = (i[12]&!i[6]&!i[4]&!i[2]);
assign out.word = (i[13]&!i[6]&!i[4]);
assign out.csr_read = (i[13]&i[6]&i[4]) | (i[7]&i[6]&i[4]) | (i[8]&i[6]&i[4]) | (
    i[9]&i[6]&i[4]) | (i[10]&i[6]&i[4]) | (i[11]&i[6]&i[4]);
assign out.csr_clr = (i[15]&i[13]&i[12]&i[6]&i[4]) | (i[16]&i[13]&i[12]&i[6]&i[4]) | (
    i[17]&i[13]&i[12]&i[6]&i[4]) | (i[18]&i[13]&i[12]&i[6]&i[4]) | (
    i[19]&i[13]&i[12]&i[6]&i[4]);
assign out.csr_set = (i[15]&!i[12]&i[6]&i[4]) | (i[16]&!i[12]&i[6]&i[4]) | (i[17]
    &!i[12]&i[6]&i[4]) | (i[18]&!i[12]&i[6]&i[4]) | (i[19]&!i[12]&i[6]
    &i[4]);
assign out.csr_write = (!i[13]&i[12]&i[6]&i[4]);
assign out.csr_imm = (i[14]&!i[13]&i[6]&i[4]) | (i[15]&i[14]&i[6]&i[4]) | (i[16]
    &i[14]&i[6]&i[4]) | (i[17]&i[14]&i[6]&i[4]) | (i[18]&i[14]&i[6]&i[4]) | (
    i[19]&i[14]&i[6]&i[4]);
assign out.presync = (!i[5]&i[3]) | (i[25]&i[14]&!i[6]&i[5]&!i[2]) | (!i[13]&i[7]
    &i[6]&i[4]) | (!i[13]&i[8]&i[6]&i[4]) | (!i[13]&i[9]&i[6]&i[4]) | (
    !i[13]&i[10]&i[6]&i[4]) | (!i[13]&i[11]&i[6]&i[4]) | (i[15]&i[13]
    &i[6]&i[4]) | (i[16]&i[13]&i[6]&i[4]) | (i[17]&i[13]&i[6]&i[4]) | (
    i[18]&i[13]&i[6]&i[4]) | (i[19]&i[13]&i[6]&i[4]);
assign out.postsync = (i[12]&!i[5]&i[3]) | (!i[22]&!i[13]&!i[12]&i[6]&i[4]) | (
    i[25]&i[14]&!i[6]&i[5]&!i[2]) | (!i[13]&i[7]&i[6]&i[4]) | (!i[13]
    &i[8]&i[6]&i[4]) | (!i[13]&i[9]&i[6]&i[4]) | (!i[13]&i[10]&i[6]&i[4]) | (
    !i[13]&i[11]&i[6]&i[4]) | (i[15]&i[13]&i[6]&i[4]) | (i[16]&i[13]&i[6]
    &i[4]) | (i[17]&i[13]&i[6]&i[4]) | (i[18]&i[13]&i[6]&i[4]) | (i[19]
    &i[13]&i[6]&i[4]);
assign out.ebreak = (!i[22]&i[20]&!i[13]&!i[12]&i[6]&i[4]);
assign out.ecall = (!i[21]&!i[20]&!i[13]&!i[12]&i[6]&i[4]);
assign out.mret = (i[29]&!i[13]&!i[12]&i[6]&i[4]);
assign out.mul = (i[25]&!i[14]&!i[6]&i[5]&i[4]&!i[2]);
assign out.rs1_sign = (i[25]&!i[14]&i[13]&!i[12]&!i[6]&i[5]&i[4]&!i[2]) | (i[25]
    &!i[14]&!i[13]&i[12]&!i[6]&i[4]&!i[2]);
assign out.rs2_sign = (i[25]&!i[14]&!i[13]&i[12]&!i[6]&i[4]&!i[2]);
assign out.low = (i[25]&!i[14]&!i[13]&!i[12]&i[5]&i[4]&!i[2]);
assign out.div = (i[25]&i[14]&!i[6]&i[5]&!i[2]);
assign out.rem = (i[25]&i[14]&i[13]&!i[6]&i[5]&!i[2]);
assign out.fence = (!i[5]&i[3]);
assign out.fence_i = (i[12]&!i[5]&i[3]);
assign out.pm_alu = (i[28]&i[22]&!i[13]&!i[12]&i[4]) | (i[4]&i[2]) | (!i[25]&!i[6]
    &i[4]) | (!i[5]&i[4]);
assign out.legal = (!i[31]&!i[30]&i[29]&i[28]&!i[27]&!i[26]&!i[25]&!i[24]&!i[23]
    &!i[22]&i[21]&!i[20]&!i[19]&!i[18]&!i[17]&!i[16]&!i[15]&!i[14]&!i[11]
    &!i[10]&!i[9]&!i[8]&!i[7]&i[6]&i[5]&i[4]&!i[3]&!i[2]&i[1]&i[0]) | (
    !i[31]&!i[30]&!i[29]&i[28]&!i[27]&!i[26]&!i[25]&!i[24]&!i[23]&i[22]
    &!i[21]&i[20]&!i[19]&!i[18]&!i[17]&!i[16]&!i[15]&!i[14]&!i[11]&!i[10]
    &!i[9]&!i[8]&!i[7]&i[6]&i[5]&i[4]&!i[3]&!i[2]&i[1]&i[0]) | (!i[31]
    &!i[30]&!i[29]&!i[28]&!i[27]&!i[26]&!i[25]&!i[24]&!i[23]&!i[22]&!i[21]
    &!i[19]&!i[18]&!i[17]&!i[16]&!i[15]&!i[14]&!i[11]&!i[10]&!i[9]&!i[8]
    &!i[7]&i[5]&i[4]&!i[3]&!i[2]&i[1]&i[0]) | (!i[31]&!i[30]&!i[29]&!i[28]
    &!i[27]&!i[26]&!i[25]&!i[6]&i[4]&!i[3]&i[1]&i[0]) | (!i[31]&!i[29]
    &!i[28]&!i[27]&!i[26]&!i[25]&!i[14]&!i[13]&!i[12]&!i[6]&!i[3]&!i[2]
    &i[1]&i[0]) | (!i[31]&!i[29]&!i[28]&!i[27]&!i[26]&!i[25]&i[14]&!i[13]
    &i[12]&!i[6]&i[4]&!i[3]&i[1]&i[0]) | (!i[31]&!i[30]&!i[29]&!i[28]
    &!i[27]&!i[26]&!i[6]&i[5]&i[4]&!i[3]&i[1]&i[0]) | (!i[14]&!i[13]
    &!i[12]&i[6]&i[5]&!i[4]&!i[3]&i[1]&i[0]) | (i[14]&i[6]&i[5]&!i[4]
    &!i[3]&!i[2]&i[1]&i[0]) | (!i[12]&!i[6]&!i[5]&i[4]&!i[3]&i[1]&i[0]) | (
    !i[14]&!i[13]&i[5]&!i[4]&!i[3]&!i[2]&i[1]&i[0]) | (i[12]&i[6]&i[5]
    &i[4]&!i[3]&!i[2]&i[1]&i[0]) | (!i[31]&!i[30]&!i[29]&!i[28]&!i[27]
    &!i[26]&!i[25]&!i[24]&!i[23]&!i[22]&!i[21]&!i[20]&!i[19]&!i[18]&!i[17]
    &!i[16]&!i[15]&!i[14]&!i[13]&!i[11]&!i[10]&!i[9]&!i[8]&!i[7]&!i[6]
    &!i[5]&!i[4]&i[3]&i[2]&i[1]&i[0]) | (!i[31]&!i[30]&!i[29]&!i[28]
    &!i[19]&!i[18]&!i[17]&!i[16]&!i[15]&!i[14]&!i[13]&!i[12]&!i[11]&!i[10]
    &!i[9]&!i[8]&!i[7]&!i[6]&!i[5]&!i[4]&i[3]&i[2]&i[1]&i[0]) | (i[13]
    &i[6]&i[5]&i[4]&!i[3]&!i[2]&i[1]&i[0]) | (!i[13]&!i[6]&!i[5]&!i[4]
    &!i[3]&!i[2]&i[1]&i[0]) | (i[6]&i[5]&!i[4]&i[3]&i[2]&i[1]&i[0]) | (
    i[13]&!i[6]&!i[5]&i[4]&!i[3]&i[1]&i[0]) | (!i[14]&!i[12]&!i[6]&!i[4]
    &!i[3]&!i[2]&i[1]&i[0]) | (!i[6]&i[4]&!i[3]&i[2]&i[1]&i[0]);
endmodule
module dec_gpr_ctl #(parameter GPR_BANKS      = 1,
                               GPR_BANKS_LOG2 = 1)  (
    input logic active_clk,
    input logic [4:0] raddr0,   
    input logic [4:0] raddr1,
    input logic [4:0] raddr2,
    input logic [4:0] raddr3,
    input logic       rden0,    
    input logic       rden1,
    input logic       rden2,
    input logic       rden3,
    input logic [4:0] waddr0,   
    input logic [4:0] waddr1,
    input logic [4:0] waddr2,
    input logic wen0,           
    input logic wen1,
    input logic wen2,
    input logic [31:0] wd0,     
    input logic [31:0] wd1,
    input logic [31:0] wd2,
    input logic                      wen_bank_id,   
    input logic [GPR_BANKS_LOG2-1:0] wr_bank_id,    
    input logic       clk,
    input logic       rst_l,
    output logic [31:0] rd0,   
    output logic [31:0] rd1,
    output logic [31:0] rd2,
    output logic [31:0] rd3,
    input  logic        scan_mode
);
   logic [GPR_BANKS-1:0][31:1] [31:0] gpr_out;      
   logic [31:1] [31:0] gpr_in;
   logic [31:1] w0v,w1v,w2v;
   logic [31:1] gpr_wr_en;
   logic [GPR_BANKS-1:0][31:1] gpr_bank_wr_en;
   logic [GPR_BANKS_LOG2-1:0] gpr_bank_id;
   rvdffs #(GPR_BANKS_LOG2) bankid_ff (.*, .clk(active_clk), .en(wen_bank_id), .din(wr_bank_id[GPR_BANKS_LOG2-1:0]), .dout(gpr_bank_id[GPR_BANKS_LOG2-1:0]));
   assign gpr_wr_en[31:1] = (w0v[31:1] | w1v[31:1] | w2v[31:1]);
   for (genvar i=0; i<GPR_BANKS; i++) begin: gpr_banks
      assign gpr_bank_wr_en[i][31:1] = gpr_wr_en[31:1] & {31{gpr_bank_id[GPR_BANKS_LOG2-1:0] == i}};
      for ( genvar j=1; j<32; j++ )  begin : gpr
         rvdffe #(32) gprff (.*, .en(gpr_bank_wr_en[i][j]), .din(gpr_in[j][31:0]), .dout(gpr_out[i][j][31:0]));
      end : gpr
   end: gpr_banks
   always_comb begin
      rd0[31:0] = 32'b0;
      rd1[31:0] = 32'b0;
      rd2[31:0] = 32'b0;
      rd3[31:0] = 32'b0;
      w0v[31:1] = 31'b0;
      w1v[31:1] = 31'b0;
      w2v[31:1] = 31'b0;
      gpr_in[31:1] = '0;
      for (int i=0; i<GPR_BANKS; i++) begin
         for (int j=1; j<32; j++ )  begin
            rd0[31:0] |= ({32{rden0 & (raddr0[4:0]== 5'(j)) & (gpr_bank_id[GPR_BANKS_LOG2-1:0] == 1'(i))}} & gpr_out[i][j][31:0]);
            rd1[31:0] |= ({32{rden1 & (raddr1[4:0]== 5'(j)) & (gpr_bank_id[GPR_BANKS_LOG2-1:0] == 1'(i))}} & gpr_out[i][j][31:0]);
            rd2[31:0] |= ({32{rden2 & (raddr2[4:0]== 5'(j)) & (gpr_bank_id[GPR_BANKS_LOG2-1:0] == 1'(i))}} & gpr_out[i][j][31:0]);
            rd3[31:0] |= ({32{rden3 & (raddr3[4:0]== 5'(j)) & (gpr_bank_id[GPR_BANKS_LOG2-1:0] == 1'(i))}} & gpr_out[i][j][31:0]);
        end
     end
     for (int j=1; j<32; j++ )  begin
         w0v[j]     = wen0  & (waddr0[4:0]== 5'(j) );
         w1v[j]     = wen1  & (waddr1[4:0]== 5'(j) );
         w2v[j]     = wen2  & (waddr2[4:0]== 5'(j) );
         gpr_in[j]  =    ({32{w0v[j]}} & wd0[31:0]) |
                         ({32{w1v[j]}} & wd1[31:0]) |
                         ({32{w2v[j]}} & wd2[31:0]);
     end
   end  
endmodule
module dec_ib_ctl
   import swerv_types::*;
(
   input logic   free_clk,                     
   input logic   active_clk,                   
   input logic                 dbg_cmd_valid,   
   input logic                 dbg_cmd_write,   
   input logic [1:0]           dbg_cmd_type,    
   input logic [1:0]           dbg_cmd_size,    
   input logic [31:0]          dbg_cmd_addr,    
   input logic exu_flush_final,                 
   input logic          dec_ib0_valid_eff_d,    
   input logic          dec_ib1_valid_eff_d,
   input br_pkt_t i0_brp,                       
   input br_pkt_t i1_brp,
   input logic   ifu_i0_pc4,                    
   input logic   ifu_i1_pc4,
   input logic   ifu_i0_valid,                  
   input logic   ifu_i1_valid,
   input logic   ifu_i0_icaf,                   
   input logic   ifu_i1_icaf,
   input logic   ifu_i0_icaf_second,                
   input logic   ifu_i1_icaf_second,
   input logic   ifu_i0_perr,                   
   input logic   ifu_i1_perr,
   input logic   ifu_i0_sbecc,                  
   input logic   ifu_i1_sbecc,
   input logic   ifu_i0_dbecc,                  
   input logic   ifu_i1_dbecc,
   input logic [31:0]  ifu_i0_instr,            
   input logic [31:0]  ifu_i1_instr,
   input logic [31:1]  ifu_i0_pc,               
   input logic [31:1] ifu_i1_pc,
   input logic   dec_i0_decode_d,               
   input logic   dec_i1_decode_d,
   input logic   rst_l,                         
   input logic   clk,
   output logic dec_ib3_valid_d,                
   output logic dec_ib2_valid_d,                
   output logic dec_ib1_valid_d,                
   output logic dec_ib0_valid_d,                
   output logic [31:0] dec_i0_instr_d,          
   output logic [31:0] dec_i1_instr_d,          
   output logic [31:1] dec_i0_pc_d,             
   output logic [31:1] dec_i1_pc_d,
   output logic dec_i0_pc4_d,                   
   output logic dec_i1_pc4_d,
   output br_pkt_t dec_i0_brp,                  
   output br_pkt_t dec_i1_brp,
   output logic dec_i0_icaf_d,                  
   output logic dec_i1_icaf_d,
   output logic dec_i0_icaf_second_d,               
   output logic dec_i0_perr_d,                  
   output logic dec_i1_perr_d,
   output logic dec_i0_sbecc_d,                 
   output logic dec_i1_sbecc_d,
   output logic dec_i0_dbecc_d,                 
   output logic dec_i1_dbecc_d,
   output logic dec_debug_wdata_rs1_d,          
   output logic dec_debug_fence_d,              
   input logic [15:0] ifu_i0_cinst,             
   input logic [15:0] ifu_i1_cinst,
   output logic [15:0] dec_i0_cinst_d,          
   output logic [15:0] dec_i1_cinst_d,
   input  logic scan_mode
   );
localparam TOTAL_INT        = 9;
localparam DCCM_BITS        = 16;
localparam DCCM_BANK_BITS   = 3;
localparam DCCM_NUM_BANKS   = 8;
localparam DCCM_DATA_WIDTH  = 32;
localparam DCCM_FDATA_WIDTH = 39;
localparam DCCM_BYTE_WIDTH  = 4;
localparam DCCM_ECC_WIDTH   = 7;
localparam LSU_RDBUF_DEPTH  = 8;
localparam DMA_BUF_DEPTH    = 4;
localparam LSU_STBUF_DEPTH  = 8;
localparam LSU_SB_BITS      = 16;
localparam DEC_INSTBUF_DEPTH = 4;
localparam ICCM_SIZE         = 512;
localparam ICCM_BITS         = 19;
localparam ICCM_NUM_BANKS    = 8;
localparam ICCM_BANK_BITS    = 3;
localparam ICCM_INDEX_BITS   = 14;
localparam ICCM_BANK_HI      = 4 + (3/4);
localparam ICACHE_TAG_HIGH  = 12;
localparam ICACHE_TAG_LOW   = 6;
localparam ICACHE_IC_DEPTH  = 8;
localparam ICACHE_TAG_DEPTH = 64;
localparam LSU_BUS_TAG     = 4;
localparam DMA_BUS_TAG     = 1;
localparam SB_BUS_TAG      = 1;
localparam IFU_BUS_TAG     = 3;
   logic         flush_final;
   logic [3:0]   ibval_in, ibval;
   logic [31:0]  ib3_in, ib2_in, ib1_in, ib0_in;
   logic [31:0]  ib3, ib2, ib1, ib0;
   logic [36:0]  pc3_in, pc2_in, pc1_in, pc0_in;
   logic [36:0]  pc3, pc2, pc1, pc0;
   logic [15:0]  cinst3_in, cinst2_in, cinst1_in, cinst0_in;
   logic [15:0]  cinst3, cinst2, cinst1, cinst0;
   logic         write_i1_ib3, write_i0_ib3;
   logic         write_i1_ib2, write_i0_ib2;
   logic         write_i1_ib1, write_i0_ib1;
   logic         write_i0_ib0;
   logic         shift2, shift1, shift0;
   logic         shift_ib1_ib0, shift_ib2_ib1, shift_ib3_ib2;
   logic         shift_ib2_ib0;
   logic         shift_ib3_ib1;
   logic         ifu_i0_val, ifu_i1_val;
   logic         debug_valid;
   logic [4:0]   dreg;
   logic [11:0]  dcsr;
   logic [31:0]  ib0_debug_in;
   logic         debug_read;
   logic         debug_write;
   logic         debug_read_gpr;
   logic         debug_write_gpr;
   logic         debug_read_csr;
   logic         debug_write_csr;
   rvdff #(1) flush_upperff (.*, .clk(free_clk), .din(exu_flush_final), .dout(flush_final));
   logic [3:0]   ibvalid;
   logic [3:0]   i0_wen;
   logic [3:1]   i1_wen;
   logic [3:0]   shift_ibval;
   logic [3:0]   ibwrite;
   assign ibvalid[3:0] = ibval[3:0] | i0_wen[3:0] | {i1_wen[3:1],1'b0};
   assign ibval_in[3:0] = (({4{shift0}} & ibvalid[3:0]) |
                           ({4{shift1}} & {1'b0, ibvalid[3:1]}) |
                           ({4{shift2}} & {2'b0, ibvalid[3:2]})) & ~{4{flush_final}};
   rvdff #(4) ibvalff (.*, .clk(active_clk), .din(ibval_in[3:0]), .dout(ibval[3:0]));
   if (DEC_INSTBUF_DEPTH==4) begin
      assign ifu_i0_val = ifu_i0_valid & ~ibval[3] & ~flush_final;
      assign ifu_i1_val = ifu_i1_valid & ~ibval[2] & ~flush_final;
   end
   else begin
      assign ifu_i0_val = ifu_i0_valid & (~dec_ib0_valid_eff_d | ~dec_ib1_valid_eff_d) & ~flush_final;
      assign ifu_i1_val = ifu_i1_valid & (~dec_ib0_valid_eff_d & ~dec_ib1_valid_eff_d) & ~flush_final;
   end
   assign i0_wen[0] = ~ibval[0]             & (ifu_i0_val | debug_valid);
   assign i0_wen[1] =  ibval[0] & ~ibval[1] & ifu_i0_val;
   assign i0_wen[2] =  ibval[1] & ~ibval[2] & ifu_i0_val;
   assign i0_wen[3] =  ibval[2] & ~ibval[3] & ifu_i0_val;
   assign i1_wen[1] = ~ibval[0]             & ifu_i1_val;
   assign i1_wen[2] =  ibval[0] & ~ibval[1] & ifu_i1_val;
   assign i1_wen[3] =  ibval[1] & ~ibval[2] & ifu_i1_val;
   if (DEC_INSTBUF_DEPTH==4) begin
      assign cinst3_in[15:0] = ({16{write_i0_ib3}} & ifu_i0_cinst[15:0]) |
                               ({16{write_i1_ib3}} & ifu_i1_cinst[15:0]);
      rvdffe #(16) cinst3ff (.*, .en(ibwrite[3]), .din(cinst3_in[15:0]), .dout(cinst3[15:0]));
      assign cinst2_in[15:0] = ({16{write_i0_ib2}} & ifu_i0_cinst[15:0]) |
                               ({16{write_i1_ib2}} & ifu_i1_cinst[15:0]) |
                               ({16{shift_ib3_ib2}} & cinst3[15:0]);
      rvdffe #(16) cinst2ff (.*, .en(ibwrite[2]), .din(cinst2_in[15:0]), .dout(cinst2[15:0]));
   end  
   else begin
      assign cinst3 = '0;
      assign cinst2 = '0;
   end
   assign cinst1_in[15:0] = ({16{write_i0_ib1}} & ifu_i0_cinst[15:0]) |
                            ({16{write_i1_ib1}} & ifu_i1_cinst[15:0]) |
                            ({16{shift_ib2_ib1}} & cinst2[15:0]) |
                            ({16{shift_ib3_ib1}} & cinst3[15:0]);
   rvdffe #(16) cinst1ff (.*, .en(ibwrite[1]), .din(cinst1_in[15:0]), .dout(cinst1[15:0]));
   assign cinst0_in[15:0] = ({16{write_i0_ib0}} & ifu_i0_cinst[15:0]) |
                            ({16{shift_ib1_ib0}} & cinst1[15:0]) |
                            ({16{shift_ib2_ib0}} & cinst2[15:0]);
   rvdffe #(16) cinst0ff (.*, .en(ibwrite[0]), .din(cinst0_in[15:0]), .dout(cinst0[15:0]));
   assign dec_i0_cinst_d[15:0] = cinst0[15:0];
   assign dec_i1_cinst_d[15:0] = cinst1[15:0];
   assign ibwrite[3:0] = {  write_i0_ib3 | write_i1_ib3,
                            write_i0_ib2 | write_i1_ib2 | shift_ib3_ib2,
                            write_i0_ib1 | write_i1_ib1 | shift_ib2_ib1 | shift_ib3_ib1,
                            write_i0_ib0 | shift_ib1_ib0 | shift_ib2_ib0
                            };
   logic [36:0]  ifu_i1_pcdata, ifu_i0_pcdata;
   assign ifu_i1_pcdata[36:0] = { ifu_i1_icaf_second, ifu_i1_dbecc, ifu_i1_sbecc, ifu_i1_perr, ifu_i1_icaf,
                                  ifu_i1_pc[31:1], ifu_i1_pc4 };
   assign ifu_i0_pcdata[36:0] = { ifu_i0_icaf_second, ifu_i0_dbecc, ifu_i0_sbecc, ifu_i0_perr, ifu_i0_icaf,
                                  ifu_i0_pc[31:1], ifu_i0_pc4 };
   if (DEC_INSTBUF_DEPTH==4) begin
      assign pc3_in[36:0] = ({37{write_i0_ib3}} & ifu_i0_pcdata[36:0]) |
                            ({37{write_i1_ib3}} & ifu_i1_pcdata[36:0]);
      rvdffe #(37) pc3ff (.*, .en(ibwrite[3]), .din(pc3_in[36:0]), .dout(pc3[36:0]));
      assign pc2_in[36:0] = ({37{write_i0_ib2}} & ifu_i0_pcdata[36:0]) |
                            ({37{write_i1_ib2}} & ifu_i1_pcdata[36:0]) |
                            ({37{shift_ib3_ib2}} & pc3[36:0]);
      rvdffe #(37) pc2ff (.*, .en(ibwrite[2]), .din(pc2_in[36:0]), .dout(pc2[36:0]));
   end  
   else begin
      assign pc3 = '0;
      assign pc2 = '0;
   end
   assign pc1_in[36:0] = ({37{write_i0_ib1}} & ifu_i0_pcdata[36:0]) |
                         ({37{write_i1_ib1}} & ifu_i1_pcdata[36:0]) |
                         ({37{shift_ib2_ib1}} & pc2[36:0]) |
                         ({37{shift_ib3_ib1}} & pc3[36:0]);
   rvdffe #(37) pc1ff (.*, .en(ibwrite[1]), .din(pc1_in[36:0]), .dout(pc1[36:0]));
   assign pc0_in[36:0] = ({37{write_i0_ib0}} & ifu_i0_pcdata[36:0]) |
                         ({37{shift_ib1_ib0}} & pc1[36:0]) |
                         ({37{shift_ib2_ib0}} & pc2[36:0]);
   rvdffe #(37) pc0ff (.*, .en(ibwrite[0]), .din(pc0_in[36:0]), .dout(pc0[36:0]));
   assign dec_i0_icaf_second_d = pc0[36];    
   assign dec_i1_dbecc_d = pc1[35];
   assign dec_i0_dbecc_d = pc0[35];
   assign dec_i1_sbecc_d = pc1[34];
   assign dec_i0_sbecc_d = pc0[34];
   assign dec_i1_perr_d = pc1[33];
   assign dec_i0_perr_d = pc0[33];
   assign dec_i1_icaf_d = pc1[32];
   assign dec_i0_icaf_d = pc0[32];
   assign dec_i1_pc_d[31:1] = pc1[31:1];
   assign dec_i0_pc_d[31:1] = pc0[31:1];
   assign dec_i1_pc4_d = pc1[0];
   assign dec_i0_pc4_d = pc0[0];
   logic [$bits(br_pkt_t)-1:0] bp3_in,bp3,bp2_in,bp2,bp1_in,bp1,bp0_in,bp0;
   if (DEC_INSTBUF_DEPTH==4) begin
      assign bp3_in = ({$bits(br_pkt_t){write_i0_ib3}} & i0_brp) |
                      ({$bits(br_pkt_t){write_i1_ib3}} & i1_brp);
      rvdffe #($bits(br_pkt_t)) bp3ff (.*, .en(ibwrite[3]), .din(bp3_in), .dout(bp3));
      assign bp2_in = ({$bits(br_pkt_t){write_i0_ib2}} & i0_brp) |
                      ({$bits(br_pkt_t){write_i1_ib2}} & i1_brp) |
                      ({$bits(br_pkt_t){shift_ib3_ib2}} & bp3);
      rvdffe #($bits(br_pkt_t)) bp2ff (.*, .en(ibwrite[2]), .din(bp2_in), .dout(bp2));
   end  
   else begin
      assign bp3 = '0;
      assign bp2 = '0;
   end
   assign bp1_in = ({$bits(br_pkt_t){write_i0_ib1}} & i0_brp) |
                   ({$bits(br_pkt_t){write_i1_ib1}} & i1_brp) |
                   ({$bits(br_pkt_t){shift_ib2_ib1}} & bp2) |
                   ({$bits(br_pkt_t){shift_ib3_ib1}} & bp3);
   rvdffe #($bits(br_pkt_t)) bp1ff (.*, .en(ibwrite[1]), .din(bp1_in), .dout(bp1));
   assign bp0_in = ({$bits(br_pkt_t){write_i0_ib0}} & i0_brp) |
                   ({$bits(br_pkt_t){shift_ib1_ib0}} & bp1) |
                   ({$bits(br_pkt_t){shift_ib2_ib0}} & bp2);
   rvdffe #($bits(br_pkt_t)) bp0ff (.*, .en(ibwrite[0]), .din(bp0_in), .dout(bp0));
   if (DEC_INSTBUF_DEPTH==4) begin
      assign ib3_in[31:0] = ({32{write_i0_ib3}} & ifu_i0_instr[31:0]) |
                            ({32{write_i1_ib3}} & ifu_i1_instr[31:0]);
      rvdffe #(32) ib3ff (.*, .en(ibwrite[3]), .din(ib3_in[31:0]), .dout(ib3[31:0]));
      assign ib2_in[31:0] = ({32{write_i0_ib2}} & ifu_i0_instr[31:0]) |
                            ({32{write_i1_ib2}} & ifu_i1_instr[31:0]) |
                            ({32{shift_ib3_ib2}} & ib3[31:0]);
      rvdffe #(32) ib2ff (.*, .en(ibwrite[2]), .din(ib2_in[31:0]), .dout(ib2[31:0]));
   end  
   else begin
      assign ib3 = '0;
      assign ib2 = '0;
   end
   assign ib1_in[31:0] = ({32{write_i0_ib1}} & ifu_i0_instr[31:0]) |
                         ({32{write_i1_ib1}} & ifu_i1_instr[31:0]) |
                         ({32{shift_ib2_ib1}} & ib2[31:0]) |
                         ({32{shift_ib3_ib1}} & ib3[31:0]);
   rvdffe #(32) ib1ff (.*, .en(ibwrite[1]), .din(ib1_in[31:0]), .dout(ib1[31:0]));
   assign debug_valid = dbg_cmd_valid & (dbg_cmd_type[1:0] != 2'h2);
   assign debug_read  = debug_valid & ~dbg_cmd_write;
   assign debug_write = debug_valid &  dbg_cmd_write;
   assign debug_read_gpr  = debug_read  & (dbg_cmd_type[1:0]==2'h0);
   assign debug_write_gpr = debug_write & (dbg_cmd_type[1:0]==2'h0);
   assign debug_read_csr  = debug_read  & (dbg_cmd_type[1:0]==2'h1);
   assign debug_write_csr = debug_write & (dbg_cmd_type[1:0]==2'h1);
   assign dreg[4:0]  = dbg_cmd_addr[4:0];
   assign dcsr[11:0] = dbg_cmd_addr[11:0];
   assign ib0_debug_in[31:0] = ({32{debug_read_gpr}}  & {12'b000000000000,dreg[4:0],15'b110000000110011}) |
                               ({32{debug_write_gpr}} & {20'b00000000000000000110,dreg[4:0],7'b0110011}) |
                               ({32{debug_read_csr}}  & {dcsr[11:0],20'b00000010000001110011}) |
                               ({32{debug_write_csr}} & {dcsr[11:0],20'b00000001000001110011});
   rvdff #(1) debug_wdata_rs1ff (.*, .clk(free_clk), .din(debug_write_gpr | debug_write_csr), .dout(dec_debug_wdata_rs1_d));
   logic                       debug_fence_in;
   assign debug_fence_in = debug_write_csr & (dcsr[11:0] == 12'h7c4);
   rvdff #(1) debug_fence_ff (.*,  .clk(free_clk), .din(debug_fence_in),  .dout(dec_debug_fence_d));
   assign ib0_in[31:0] = ({32{write_i0_ib0}} & ((debug_valid) ? ib0_debug_in[31:0] : ifu_i0_instr[31:0])) |
                         ({32{shift_ib1_ib0}} & ib1[31:0]) |
                         ({32{shift_ib2_ib0}} & ib2[31:0]);
   rvdffe #(32) ib0ff (.*, .en(ibwrite[0]), .din(ib0_in[31:0]), .dout(ib0[31:0]));
   assign dec_ib3_valid_d = ibval[3];
   assign dec_ib2_valid_d = ibval[2];
   assign dec_ib1_valid_d = ibval[1];
   assign dec_ib0_valid_d = ibval[0];
   assign dec_i0_instr_d[31:0] = ib0[31:0];
   assign dec_i1_instr_d[31:0] = ib1[31:0];
   assign dec_i0_brp = bp0;
   assign dec_i1_brp = bp1;
   assign shift1 = dec_i0_decode_d & ~dec_i1_decode_d;
   assign shift2 = dec_i0_decode_d & dec_i1_decode_d;
   assign shift0 = ~dec_i0_decode_d;
   assign shift_ibval[3:0] = ({4{shift1}} & {1'b0, ibval[3:1] }) |
                             ({4{shift2}} & {2'b0, ibval[3:2]}) |
                             ({4{shift0}} & ibval[3:0]);
   assign write_i0_ib0 = ~shift_ibval[0]                & (ifu_i0_val | debug_valid);
   assign write_i0_ib1 =  shift_ibval[0] & ~shift_ibval[1] & ifu_i0_val;
   assign write_i0_ib2 =  shift_ibval[1] & ~shift_ibval[2] & ifu_i0_val;
   assign write_i0_ib3 =  shift_ibval[2] & ~shift_ibval[3] & ifu_i0_val;
   assign write_i1_ib1 = ~shift_ibval[0]                & ifu_i1_val;
   assign write_i1_ib2 =  shift_ibval[0] & ~shift_ibval[1] & ifu_i1_val;
   assign write_i1_ib3 =  shift_ibval[1] & ~shift_ibval[2] & ifu_i1_val;
   assign shift_ib1_ib0 = shift1 & ibval[1];
   assign shift_ib2_ib1 = shift1 & ibval[2];
   assign shift_ib3_ib2 = shift1 & ibval[3];
   assign shift_ib2_ib0 = shift2 & ibval[2];
   assign shift_ib3_ib1 = shift2 & ibval[3];
endmodule
module dec_tlu_ctl
   import swerv_types::*;
(
   input logic clk,
   input logic active_clk,
   input logic free_clk,
   input logic rst_l,
   input logic scan_mode,
   input logic [31:1] rst_vec,  
   input logic        nmi_int,  
   input logic [31:1] nmi_vec,  
   input logic  i_cpu_halt_req,     
   input logic  i_cpu_run_req,      
   input logic mpc_debug_halt_req,  
   input logic mpc_debug_run_req,  
   input logic mpc_reset_run_req,  
   input logic [1:0] ifu_pmu_instr_aligned,    
   input logic       ifu_pmu_align_stall,   
   input logic       ifu_pmu_fetch_stall,  
   input logic       ifu_pmu_ic_miss,  
   input logic       ifu_pmu_ic_hit,  
   input logic       ifu_pmu_bus_error,  
   input logic       ifu_pmu_bus_busy,  
   input logic       ifu_pmu_bus_trxn,  
   input logic [1:0] dec_pmu_instr_decoded,  
   input logic       dec_pmu_decode_stall,  
   input logic       dec_pmu_presync_stall,  
   input logic       dec_pmu_postsync_stall, 
   input logic       lsu_freeze_dc3,          
   input logic       lsu_store_stall_any,     
   input logic       dma_dccm_stall_any,      
   input logic       dma_iccm_stall_any,      
   input logic       exu_pmu_i0_br_misp,      
   input logic       exu_pmu_i0_br_ataken,    
   input logic       exu_pmu_i0_pc4,          
   input logic       exu_pmu_i1_br_misp,      
   input logic       exu_pmu_i1_br_ataken,    
   input logic       exu_pmu_i1_pc4,          
   input logic       lsu_pmu_bus_trxn,        
   input logic       lsu_pmu_bus_misaligned,  
   input logic       lsu_pmu_bus_error,       
   input logic       lsu_pmu_bus_busy,        
   input logic       iccm_dma_sb_error,       
   input    lsu_error_pkt_t lsu_error_pkt_dc3,  
   input logic         lsu_single_ecc_error_incr,     
   input logic lsu_load_ecc_stbuf_full_dc3,     
   input logic dec_pause_state,  
   input logic         lsu_imprecise_error_store_any,       
   input logic         lsu_imprecise_error_load_any,       
   input logic [31:0]  lsu_imprecise_error_addr_any,  
   input logic         lsu_freeze_external_ints_dc3,  
   input logic        dec_csr_wen_unq_d,        
   input logic        dec_csr_any_unq_d,        
   input logic        dec_csr_wen_wb,       
   input logic [11:0] dec_csr_rdaddr_d,       
   input logic [11:0] dec_csr_wraddr_wb,       
   input logic [31:0] dec_csr_wrdata_wb,    
   input logic        dec_csr_stall_int_ff,  
   input logic dec_tlu_i0_valid_e4,  
   input logic dec_tlu_i1_valid_e4,  
   input logic dec_i0_load_e4,  
   input logic dec_fence_pending,  
   input logic [31:1] exu_npc_e4,  
   input logic exu_i0_flush_lower_e4,        
   input logic exu_i1_flush_lower_e4,        
   input logic [31:1] exu_i0_flush_path_e4,  
   input logic [31:1] exu_i1_flush_path_e4,  
   input logic [31:1] dec_tlu_i0_pc_e4,  
   input logic [31:1] dec_tlu_i1_pc_e4,  
   input trap_pkt_t dec_tlu_packet_e4,  
   input logic [31:0] dec_illegal_inst,  
   input logic        dec_i0_decode_d,   
   input logic [5:4] exu_i0_br_index_e4,  
   input logic [1:0]  exu_i0_br_hist_e4,  
   input logic [1:0]  exu_i0_br_bank_e4,  
   input logic        exu_i0_br_error_e4,  
   input logic        exu_i0_br_start_error_e4,  
   input logic        exu_i0_br_valid_e4,  
   input logic        exu_i0_br_mp_e4,  
   input logic        exu_i0_br_middle_e4,  
   input logic [4:0]  exu_i0_br_fghr_e4,  
   input logic [5:4] exu_i1_br_index_e4,  
   input logic [1:0]  exu_i1_br_hist_e4,  
   input logic [1:0]  exu_i1_br_bank_e4,  
   input logic        exu_i1_br_error_e4,  
   input logic        exu_i1_br_start_error_e4,  
   input logic        exu_i1_br_valid_e4,  
   input logic        exu_i1_br_mp_e4,  
   input logic        exu_i1_br_middle_e4,  
   input logic [4:0]  exu_i1_br_fghr_e4,  
   input logic        exu_i1_br_way_e4,  
   input logic        exu_i0_br_way_e4,  
   output logic dec_dbg_cmd_done,  
   output logic dec_dbg_cmd_fail,  
   output logic dec_tlu_flush_noredir_wb ,  
   output logic dec_tlu_mpc_halted_only,  
   output logic dec_tlu_dbg_halted,  
   output logic dec_tlu_pmu_fw_halted,  
   output logic dec_tlu_debug_mode,  
   output logic dec_tlu_resume_ack,  
   output logic dec_tlu_debug_stall,  
   output logic dec_tlu_flush_leak_one_wb,  
   output logic dec_tlu_flush_err_wb,  
   output logic dec_tlu_stall_dma,  
   input  logic dbg_halt_req,  
   input  logic dbg_resume_req,  
   input  logic ifu_miss_state_idle,  
   input  logic lsu_halt_idle_any,  
   output trigger_pkt_t  [3:0] trigger_pkt_any,  
   input logic [33:0] ifu_ic_debug_rd_data,  
   input logic ifu_ic_debug_rd_data_valid,  
   output cache_debug_pkt_t dec_tlu_ic_diag_pkt,  
   input logic [7:0] pic_claimid,  
   input logic [3:0] pic_pl,  
   input logic       mhwakeup,  
   input logic mexintpend,  
   input logic timer_int,  
   output logic o_cpu_halt_status,  
   output logic o_cpu_halt_ack,  
   output logic o_cpu_run_ack,  
   output logic o_debug_mode_status,  
   output logic mpc_debug_halt_ack,  
   output logic mpc_debug_run_ack,  
   output logic debug_brkpt_status,  
   output logic [3:0] dec_tlu_meicurpl,  
   output logic [3:0] dec_tlu_meipt,  
   output br_tlu_pkt_t dec_tlu_br0_wb_pkt,  
   output br_tlu_pkt_t dec_tlu_br1_wb_pkt,  
   output logic [31:0] dec_csr_rddata_d,       
   output logic dec_csr_legal_d,               
   output logic dec_tlu_i0_kill_writeb_wb,     
   output logic dec_tlu_i1_kill_writeb_wb,     
   output logic dec_tlu_flush_lower_wb,        
   output logic [31:1] dec_tlu_flush_path_wb,  
   output logic dec_tlu_fence_i_wb,            
   output logic dec_tlu_presync_d,             
   output logic dec_tlu_postsync_d,            
   output logic [31:0] dec_tlu_mrac_ff,         
   output logic dec_tlu_cancel_e4,              
   output logic dec_tlu_wr_pause_wb,            
   output logic dec_tlu_flush_pause_wb,         
   output logic [1:0] dec_tlu_perfcnt0,  
   output logic [1:0] dec_tlu_perfcnt1,  
   output logic [1:0] dec_tlu_perfcnt2,  
   output logic [1:0] dec_tlu_perfcnt3,  
   output logic dec_tlu_i0_valid_wb1,   
   output logic dec_tlu_i1_valid_wb1,   
   output logic dec_tlu_i0_exc_valid_wb1,  
   output logic dec_tlu_i1_exc_valid_wb1,  
   output logic dec_tlu_int_valid_wb1,  
   output logic [4:0] dec_tlu_exc_cause_wb1,  
   output logic [31:0] dec_tlu_mtval_wb1,  
   output logic  dec_tlu_sideeffect_posted_disable,     
   output logic  dec_tlu_dual_issue_disable,  
   output logic  dec_tlu_core_ecc_disable,  
   output logic  dec_tlu_sec_alu_disable,  
   output logic  dec_tlu_non_blocking_disable,     
   output logic  dec_tlu_fast_div_disable,         
   output logic  dec_tlu_bpred_disable,            
   output logic  dec_tlu_wb_coalescing_disable,    
   output logic  dec_tlu_ld_miss_byp_wb_disable,   
   output logic  dec_tlu_pipelining_disable,       
   output logic [2:0]  dec_tlu_dma_qos_prty,     
   output logic  dec_tlu_misc_clk_override,  
   output logic  dec_tlu_dec_clk_override,   
   output logic  dec_tlu_exu_clk_override,   
   output logic  dec_tlu_ifu_clk_override,   
   output logic  dec_tlu_lsu_clk_override,   
   output logic  dec_tlu_bus_clk_override,   
   output logic  dec_tlu_pic_clk_override,   
   output logic  dec_tlu_dccm_clk_override,  
   output logic  dec_tlu_icm_clk_override    
   );
   logic         dec_csr_wen_wb_mod, clk_override, e4e5_int_clk, nmi_lsu_load_type, nmi_lsu_store_type, nmi_int_detected_f, nmi_lsu_load_type_f,
                 nmi_lsu_store_type_f, allow_dbg_halt_csr_write, dbg_cmd_done_ns, i_cpu_run_req_d1_raw, debug_mode_status, lsu_single_ecc_error_wb,
                 i0_mp_e4, i1_mp_e4, sel_npc_e4, sel_npc_wb, ce_int, mtval_capture_lsu_wb, wr_mdeau_wb, micect_cout_nc, miccmect_cout_nc,
                 mdccmect_cout_nc, nmi_in_debug_mode, dpc_capture_npc, dpc_capture_pc, tdata_load, tdata_opcode, tdata_action, perfcnt_halted;
   logic reset_delayed, reset_detect, reset_detected;
   logic wr_mstatus_wb, wr_mtvec_wb, wr_mie_wb, wr_mcyclel_wb, wr_mcycleh_wb,
         wr_minstretl_wb, wr_minstreth_wb, wr_mscratch_wb, wr_mepc_wb, wr_mcause_wb, wr_mtval_wb,
         wr_mrac_wb, wr_meihap_wb, wr_meicurpl_wb, wr_meipt_wb, wr_dcsr_wb,
         wr_dpc_wb, wr_meicidpl_wb, wr_meivt_wb, wr_meicpct_wb, wr_micect_wb, wr_miccmect_wb,
         wr_mdccmect_wb,wr_mhpme3_wb, wr_mhpme4_wb, wr_mhpme5_wb, wr_mhpme6_wb;
   logic wr_mgpmc_wb, mgpmc_b, mgpmc;
   logic wr_mtsel_wb, wr_mtdata1_t0_wb, wr_mtdata1_t1_wb, wr_mtdata1_t2_wb, wr_mtdata1_t3_wb, wr_mtdata2_t0_wb, wr_mtdata2_t1_wb, wr_mtdata2_t2_wb, wr_mtdata2_t3_wb;
   logic [31:0] mtdata2_t0, mtdata2_t1, mtdata2_t2, mtdata2_t3, mtdata2_tsel_out, mtdata1_tsel_out;
   logic [9:0]  mtdata1_t0_ns, mtdata1_t0, mtdata1_t1_ns, mtdata1_t1, mtdata1_t2_ns, mtdata1_t2, mtdata1_t3_ns, mtdata1_t3;
   logic [27:0] tdata_wrdata_wb;
   logic [1:0] mtsel_ns, mtsel;
   logic tlu_i0_kill_writeb_e4, tlu_i1_kill_writeb_e4;
   logic [1:0]  mstatus_ns, mstatus;
   logic mstatus_mie_ns;
   logic [30:0] mtvec_ns, mtvec;
   logic [15:2] dcsr_ns, dcsr;
   logic [5:0] mip_ns, mip;
   logic [5:0] mie_ns, mie;
   logic [31:0] mcyclel_ns, mcyclel;
   logic [31:0] mcycleh_ns, mcycleh;
   logic [31:0] minstretl_ns, minstretl;
   logic [31:0] minstreth_ns, minstreth;
   logic [31:0] micect_ns, micect, miccmect_ns, miccmect, mdccmect_ns, mdccmect;
   logic [26:0] micect_inc, miccmect_inc, mdccmect_inc;
   logic [31:0] mscratch;
   logic [31:0] mhpmc3, mhpmc3_ns, mhpmc4, mhpmc4_ns, mhpmc5, mhpmc5_ns, mhpmc6, mhpmc6_ns;
   logic [31:0] mhpmc3h, mhpmc3h_ns, mhpmc4h, mhpmc4h_ns, mhpmc5h, mhpmc5h_ns, mhpmc6h, mhpmc6h_ns;
   logic [5:0]  mhpme3, mhpme4, mhpme5, mhpme6;
   logic [31:0] mrac;
   logic [9:2] meihap;
   logic [31:10] meivt;
   logic [3:0] meicurpl_ns, meicurpl;
   logic [3:0] meicidpl_ns, meicidpl;
   logic [3:0] meipt_ns, meipt;
   logic [31:0] mdseac;
   logic mdseac_locked_ns, mdseac_locked_f, mdseac_en, nmi_lsu_detected;
   logic [31:1] mepc_ns, mepc;
   logic [31:1] dpc_ns, dpc;
   logic [31:0] mcause_ns, mcause;
   logic [31:0] mtval_ns, mtval;
   logic       mret_wb;
   logic dec_pause_state_f, dec_tlu_wr_pause_wb_f, pause_expired_e4, pause_expired_wb;
   logic       tlu_flush_lower_e4, tlu_flush_lower_wb;
   logic [31:1] tlu_flush_path_e4, tlu_flush_path_wb;
   logic i0_valid_wb, i1_valid_wb;
   logic [5:0] vectored_cause;
   logic       vpath_overflow_nc;
   logic [31:1] vectored_path, interrupt_path;
   logic [18:2] dicawics_ns, dicawics;
   logic        wr_dicawics_wb, wr_dicad0_wb, wr_dicad1_wb;
   logic [31:0] dicad0_ns, dicad0;
   logic [1:0]  dicad1_ns, dicad1;
   logic        ebreak_e4, ebreak_to_debug_mode_e4, ecall_e4, illegal_e4, illegal_e4_qual, mret_e4, inst_acc_e4, fence_i_e4,
                ic_perr_e4, iccm_sbecc_e4, ebreak_to_debug_mode_wb, kill_ebreak_count_wb, inst_acc_second_e4;
   logic        ebreak_wb, ecall_wb, illegal_wb,  illegal_raw_wb, inst_acc_wb, inst_acc_second_wb, fence_i_wb, ic_perr_wb, iccm_sbecc_wb;
   logic ce_int_ready, ext_int_ready, timer_int_ready, int_timer0_int_ready, int_timer1_int_ready, mhwakeup_ready,
         take_ext_int, take_ce_int, take_timer_int, take_int_timer0_int, take_int_timer1_int, take_nmi, take_nmi_wb, int_timer0_int_possible, int_timer1_int_possible;
   logic i0_exception_valid_e4, interrupt_valid, i0_exception_valid_wb, interrupt_valid_wb, exc_or_int_valid, exc_or_int_valid_wb, mdccme_ce_req, miccme_ce_req, mice_ce_req;
   logic synchronous_flush_e4;
   logic [4:0] exc_cause_e4, exc_cause_wb;
   logic        mcyclel_cout, mcyclel_cout_f;
   logic [31:0] mcyclel_inc;
   logic        mcycleh_cout_nc;
   logic [31:0] mcycleh_inc;
   logic        minstretl_cout, minstretl_cout_f, minstret_enable;
   logic [31:0] minstretl_inc, minstretl_read;
   logic        minstreth_cout_nc;
   logic [31:0] minstreth_inc, minstreth_read;
   logic [31:1] pc_e4, pc_wb, npc_e4, npc_wb;
   logic        mtval_capture_pc_wb, mtval_capture_inst_wb, mtval_clear_wb, mtval_capture_pc_plus2_wb;
   logic valid_csr;
   logic [5:4] dec_tlu_br0_addr_e4, dec_tlu_br1_addr_e4;
   logic [1:0]  dec_tlu_br0_bank_e4, dec_tlu_br1_bank_e4;
   logic rfpc_i0_e4, rfpc_i1_e4;
   logic lsu_i0_rfnpc_dc4, lsu_i1_rfnpc_dc4;
   logic lsu_i0_rfpc_dc4, lsu_i1_rfpc_dc4;
   logic dec_tlu_br0_error_e4, dec_tlu_br0_start_error_e4, dec_tlu_br0_v_e4;
   logic dec_tlu_br1_error_e4, dec_tlu_br1_start_error_e4, dec_tlu_br1_v_e4;
   logic lsu_i0_exc_dc4, lsu_i1_exc_dc4, lsu_i0_exc_dc4_raw, lsu_i1_exc_dc4_raw, lsu_exc_ma_dc4, lsu_exc_acc_dc4, lsu_exc_st_dc4,
         lsu_exc_valid_e4, lsu_exc_valid_e4_raw, lsu_exc_valid_wb, lsu_i0_exc_wb,
         block_interrupts, lsu_block_interrupts_dc3, lsu_block_interrupts_e4, lsu_load_ecc_stbuf_full_dc4;
   logic tlu_i0_commit_cmt, tlu_i1_commit_cmt;
   logic i0_trigger_eval_e4, i1_trigger_eval_e4, lsu_freeze_e4, lsu_freeze_pulse_e3, lsu_freeze_pulse_e4;
   logic request_debug_mode_e4, request_debug_mode_wb, request_debug_mode_done, request_debug_mode_done_f;
   logic take_halt, take_halt_f, halt_taken, halt_taken_f, internal_dbg_halt_mode, dbg_tlu_halted_f, take_reset,
         dbg_tlu_halted, core_empty, lsu_halt_idle_any_f, ifu_miss_state_idle_f, resume_ack_ns,
          debug_halt_req_f, debug_resume_req_f_raw, debug_resume_req_f, enter_debug_halt_req, dcsr_single_step_done, dcsr_single_step_done_f,
          debug_halt_req_d1, debug_halt_req_ns, dcsr_single_step_running, dcsr_single_step_running_f, internal_dbg_halt_timers;
   logic [3:0] i0_trigger_e4, i1_trigger_e4, trigger_action, trigger_enabled,
               i0_trigger_chain_masked_e4, i1_trigger_chain_masked_e4;
   logic [2:0] trigger_chain;
   logic       i0_trigger_hit_e4, i0_trigger_hit_raw_e4, i0_trigger_action_e4,
               trigger_hit_e4, trigger_hit_wb, i0_trigger_hit_wb,
               mepc_trigger_hit_sel_pc_e4,
               mepc_trigger_hit_sel_pc_wb;
   logic       i1_trigger_hit_e4, i1_trigger_hit_raw_e4, i1_trigger_action_e4;
   logic [3:0] update_hit_bit_e4, update_hit_bit_wb, i0_iside_trigger_has_pri_e4, i1_iside_trigger_has_pri_e4,
               i0_lsu_trigger_has_pri_e4, i1_lsu_trigger_has_pri_e4;
   logic cpu_halt_status, cpu_halt_ack, cpu_run_ack, ext_halt_pulse, i_cpu_halt_req_d1, i_cpu_run_req_d1;
   logic inst_acc_e4_raw, trigger_hit_dmode_e4, trigger_hit_dmode_wb, trigger_hit_for_dscr_cause_wb;
   logic wr_mcgc_wb, wr_mfdc_wb;
   logic [8:0] mcgc;
   logic [18:0] mfdc;
   logic [13:0] mfdc_int, mfdc_ns;
   logic i_cpu_halt_req_sync_qual, i_cpu_run_req_sync_qual, pmu_fw_halt_req_ns, pmu_fw_halt_req_f, int_timer_stalled,
         fw_halt_req, enter_pmu_fw_halt_req, pmu_fw_tlu_halted, pmu_fw_tlu_halted_f, internal_pmu_fw_halt_mode,
         internal_pmu_fw_halt_mode_f, int_timer0_int_hold, int_timer1_int_hold, int_timer0_int_hold_f, int_timer1_int_hold_f;
   logic dcsr_single_step_running_ff;
   logic nmi_int_delayed, nmi_int_detected;
   logic [3:0] trigger_execute, trigger_data, trigger_store;
   logic mpc_run_state_ns, debug_brkpt_status_ns, mpc_debug_halt_ack_ns, mpc_debug_run_ack_ns, dbg_halt_state_ns, dbg_run_state_ns,
         dbg_halt_state_f, mpc_debug_halt_req_sync_f, mpc_debug_run_req_sync_f, mpc_halt_state_f, mpc_halt_state_ns, mpc_run_state_f, debug_brkpt_status_f,
         mpc_debug_halt_ack_f, mpc_debug_run_ack_f, dbg_run_state_f, dbg_halt_state_ff, mpc_debug_halt_req_sync_pulse,
         mpc_debug_run_req_sync_pulse, debug_brkpt_valid, debug_halt_req, debug_resume_req, dec_tlu_mpc_halted_only_ns;
   logic wr_mpmc_wb, set_mie_pmu_fw_halt;
   logic [1:1] mpmc_b_ns, mpmc, mpmc_b;
   logic [31:0] dec_timer_rddata_d;
   logic dec_timer_read_d, dec_timer_t0_pulse, dec_timer_t1_pulse;
   dec_timer_ctl int_timers(.*);
   assign clk_override = dec_tlu_dec_clk_override;
   logic nmi_int_sync, timer_int_sync, i_cpu_halt_req_sync, i_cpu_run_req_sync, mpc_debug_halt_req_sync, mpc_debug_run_req_sync;
   rvsyncss #(6) syncro_ff(.*,
                           .clk(free_clk),
                           .din ({nmi_int,      timer_int,      i_cpu_halt_req,      i_cpu_run_req,      mpc_debug_halt_req,      mpc_debug_run_req}),
                           .dout({nmi_int_sync, timer_int_sync, i_cpu_halt_req_sync, i_cpu_run_req_sync, mpc_debug_halt_req_sync, mpc_debug_run_req_sync}));
   logic csr_wr_clk;
   rvoclkhdr csrwr_wb_cgc ( .en(dec_csr_wen_wb_mod | clk_override), .l1clk(csr_wr_clk), .* );
   logic lsu_e3_e4_clk, lsu_e4_e5_clk;
   rvoclkhdr lsu_e3_e4_cgc ( .en(lsu_error_pkt_dc3.exc_valid | lsu_error_pkt_dc4.exc_valid | lsu_error_pkt_dc3.single_ecc_error | lsu_error_pkt_dc4.single_ecc_error | clk_override), .l1clk(lsu_e3_e4_clk), .* );
   rvoclkhdr lsu_e4_e5_cgc ( .en(lsu_error_pkt_dc4.exc_valid | lsu_exc_valid_wb | clk_override), .l1clk(lsu_e4_e5_clk), .* );
   logic e4e5_clk, e4_valid, e5_valid, e4e5_valid, internal_dbg_halt_mode_f, internal_dbg_halt_mode_f2, internal_dbg_halt_mode_f3;
   assign e4_valid = dec_tlu_i0_valid_e4 | dec_tlu_i1_valid_e4;
   assign e4e5_valid = e4_valid | e5_valid;
   rvoclkhdr e4e5_cgc ( .en(e4e5_valid | clk_override), .l1clk(e4e5_clk), .* );
   rvoclkhdr e4e5_int_cgc ( .en(e4e5_valid | internal_dbg_halt_mode_f | i_cpu_run_req_d1 | interrupt_valid | interrupt_valid_wb | reset_delayed | pause_expired_e4 | pause_expired_wb | clk_override), .l1clk(e4e5_int_clk), .* );
   assign lsu_freeze_pulse_e3 = lsu_freeze_dc3 & ~lsu_freeze_e4;
   rvdff #(10)  freeff (.*,   .clk(free_clk),
                        .din({internal_dbg_halt_mode_f2,internal_dbg_halt_mode_f, lsu_freeze_dc3, lsu_freeze_pulse_e3,
                              e4_valid, lsu_block_interrupts_dc3, internal_dbg_halt_mode, tlu_flush_lower_e4, tlu_i0_kill_writeb_e4, tlu_i1_kill_writeb_e4 }),
                        .dout({internal_dbg_halt_mode_f3, internal_dbg_halt_mode_f2, lsu_freeze_e4, lsu_freeze_pulse_e4,
                               e5_valid, lsu_block_interrupts_e4, internal_dbg_halt_mode_f, tlu_flush_lower_wb, dec_tlu_i0_kill_writeb_wb, dec_tlu_i1_kill_writeb_wb}));
   rvdff #(2) reset_ff (.*, .clk(free_clk), .din({1'b1, reset_detect}), .dout({reset_detect, reset_detected}));
   assign reset_delayed = reset_detect ^ reset_detected;
   rvdff #(4) nmi_ff (.*, .clk(free_clk), .din({nmi_int_sync, nmi_int_detected, nmi_lsu_load_type, nmi_lsu_store_type}), .dout({nmi_int_delayed, nmi_int_detected_f, nmi_lsu_load_type_f, nmi_lsu_store_type_f}));
   assign nmi_lsu_detected = ~mdseac_locked_f & (lsu_imprecise_error_load_any | lsu_imprecise_error_store_any);
   assign nmi_int_detected = (nmi_int_sync & ~nmi_int_delayed) | nmi_lsu_detected | (nmi_int_detected_f & ~take_nmi_wb);
   assign nmi_lsu_load_type = (nmi_lsu_detected & lsu_imprecise_error_load_any & ~(nmi_int_detected_f & ~take_nmi_wb)) | (nmi_lsu_load_type_f & ~take_nmi_wb);
   assign nmi_lsu_store_type = (nmi_lsu_detected & lsu_imprecise_error_store_any & ~(nmi_int_detected_f & ~take_nmi_wb)) | (nmi_lsu_store_type_f & ~take_nmi_wb);
    rvdff #(11)  mpvhalt_ff (.*, .clk(free_clk),
                                 .din({mpc_debug_halt_req_sync, mpc_debug_run_req_sync,
                                       mpc_halt_state_ns, mpc_run_state_ns, debug_brkpt_status_ns,
                                       mpc_debug_halt_ack_ns, mpc_debug_run_ack_ns,
                                       dbg_halt_state_ns, dbg_run_state_ns, dbg_halt_state_f,
                                       dec_tlu_mpc_halted_only_ns}),
                                .dout({mpc_debug_halt_req_sync_f, mpc_debug_run_req_sync_f,
                                       mpc_halt_state_f, mpc_run_state_f, debug_brkpt_status_f,
                                       mpc_debug_halt_ack_f, mpc_debug_run_ack_f,
                                       dbg_halt_state_f, dbg_run_state_f, dbg_halt_state_ff,
                                       dec_tlu_mpc_halted_only}));
   assign mpc_debug_halt_req_sync_pulse = mpc_debug_halt_req_sync & ~mpc_debug_halt_req_sync_f;
   assign mpc_debug_run_req_sync_pulse = mpc_debug_run_req_sync & ~mpc_debug_run_req_sync_f;
   assign mpc_halt_state_ns = (mpc_halt_state_f | mpc_debug_halt_req_sync_pulse | (reset_delayed & ~mpc_reset_run_req)) & ~mpc_debug_run_req_sync;
   assign mpc_run_state_ns = (mpc_run_state_f | (mpc_debug_run_req_sync_pulse & ~mpc_debug_run_ack_f)) & (internal_dbg_halt_mode_f & ~dcsr_single_step_running_f);
   assign dbg_halt_state_ns = (dbg_halt_state_f | (dbg_halt_req | dcsr_single_step_done_f | trigger_hit_dmode_wb | ebreak_to_debug_mode_wb)) & ~dbg_resume_req;
   assign dbg_run_state_ns = (dbg_run_state_f | dbg_resume_req) & (internal_dbg_halt_mode_f & ~dcsr_single_step_running_f);
   assign dec_tlu_mpc_halted_only_ns = ~dbg_halt_state_f & mpc_halt_state_f;
   assign debug_brkpt_valid = ebreak_to_debug_mode_wb | trigger_hit_dmode_wb;
   assign debug_brkpt_status_ns = (debug_brkpt_valid | debug_brkpt_status_f) & (internal_dbg_halt_mode & ~dcsr_single_step_running_f);
   assign mpc_debug_halt_ack_ns = mpc_halt_state_f & internal_dbg_halt_mode_f & mpc_debug_halt_req_sync & core_empty;
   assign mpc_debug_run_ack_ns = (mpc_debug_run_req_sync & ~dbg_halt_state_ns & ~mpc_debug_halt_req_sync) | (mpc_debug_run_ack_f & mpc_debug_run_req_sync) ;
   assign mpc_debug_halt_ack = mpc_debug_halt_ack_f;
   assign mpc_debug_run_ack = mpc_debug_run_ack_f;
   assign debug_brkpt_status = debug_brkpt_status_f;
   assign debug_halt_req = (dbg_halt_req | mpc_debug_halt_req_sync | (reset_delayed & ~mpc_reset_run_req)) & ~internal_dbg_halt_mode_f;
   assign debug_resume_req = ~debug_resume_req_f &   
                             ((mpc_run_state_ns & ~dbg_halt_state_ns) |   
                              (dbg_run_state_ns & ~mpc_halt_state_ns));  
   assign take_halt = (debug_halt_req_f | pmu_fw_halt_req_f) & ~lsu_block_interrupts_e4 & ~synchronous_flush_e4 & ~mret_e4 & ~halt_taken_f & ~dec_tlu_flush_noredir_wb & ~take_reset;
   assign halt_taken = (dec_tlu_flush_noredir_wb & ~dec_tlu_flush_pause_wb) | (halt_taken_f & ~dbg_tlu_halted_f & ~pmu_fw_tlu_halted_f & ~interrupt_valid_wb);
   assign core_empty = lsu_halt_idle_any & lsu_halt_idle_any_f & ifu_miss_state_idle & ifu_miss_state_idle_f & ~debug_halt_req & ~debug_halt_req_d1;
   assign enter_debug_halt_req = (~internal_dbg_halt_mode_f & debug_halt_req) | dcsr_single_step_done_f | trigger_hit_dmode_wb | ebreak_to_debug_mode_wb;
   assign internal_dbg_halt_mode = debug_halt_req_ns | (internal_dbg_halt_mode_f & ~(debug_resume_req_f & ~dcsr[2]));
   assign allow_dbg_halt_csr_write = internal_dbg_halt_mode_f & ~dcsr_single_step_running_f;
   assign debug_halt_req_ns = enter_debug_halt_req | (debug_halt_req_f & ~dbg_tlu_halted);
   assign dbg_tlu_halted = (debug_halt_req_f & core_empty & halt_taken) | (dbg_tlu_halted_f & ~debug_resume_req_f);
   assign resume_ack_ns = (debug_resume_req_f & dbg_tlu_halted_f & dbg_run_state_ns);
   assign dcsr_single_step_done = dec_tlu_i0_valid_e4 & ~dec_tlu_dbg_halted & dcsr[2] & ~rfpc_i0_e4;
   assign dcsr_single_step_running = (debug_resume_req_f & dcsr[2]) | (dcsr_single_step_running_f & ~dcsr_single_step_done_f);
   assign dbg_cmd_done_ns = dec_tlu_i0_valid_e4 & dec_tlu_dbg_halted;
   assign request_debug_mode_e4 = (trigger_hit_dmode_e4 | ebreak_to_debug_mode_e4) | (request_debug_mode_wb & ~dec_tlu_flush_lower_wb);
   assign request_debug_mode_done = (request_debug_mode_wb | request_debug_mode_done_f) & ~dbg_tlu_halted_f;
    rvdff #(22)  halt_ff (.*, .clk(free_clk), .din({halt_taken, take_halt, lsu_halt_idle_any, ifu_miss_state_idle, dbg_tlu_halted,
                                  resume_ack_ns, dbg_cmd_done_ns, debug_halt_req_ns, debug_resume_req, trigger_hit_dmode_e4,
                                  dcsr_single_step_done, debug_halt_req,  update_hit_bit_e4[3:0], dec_tlu_wr_pause_wb, dec_pause_state,
                                  request_debug_mode_e4, request_debug_mode_done, dcsr_single_step_running, dcsr_single_step_running_f}),
                           .dout({halt_taken_f, take_halt_f, lsu_halt_idle_any_f, ifu_miss_state_idle_f, dbg_tlu_halted_f,
                                  dec_tlu_resume_ack, dec_dbg_cmd_done, debug_halt_req_f, debug_resume_req_f_raw, trigger_hit_dmode_wb,
                                  dcsr_single_step_done_f, debug_halt_req_d1, update_hit_bit_wb[3:0], dec_tlu_wr_pause_wb_f, dec_pause_state_f,
                                  request_debug_mode_wb, request_debug_mode_done_f, dcsr_single_step_running_f, dcsr_single_step_running_ff}));
   assign debug_resume_req_f = debug_resume_req_f_raw & ~dbg_halt_req;
   assign dec_tlu_debug_stall = debug_halt_req_f;
   assign dec_tlu_dbg_halted = dbg_tlu_halted_f;
   assign dec_tlu_debug_mode = internal_dbg_halt_mode_f;
   assign dec_tlu_pmu_fw_halted = pmu_fw_tlu_halted_f;
   assign dec_tlu_flush_noredir_wb = take_halt_f | (fence_i_wb & internal_dbg_halt_mode_f) | dec_tlu_flush_pause_wb | (trigger_hit_wb & trigger_hit_dmode_wb);
   assign dec_tlu_flush_pause_wb = dec_tlu_wr_pause_wb_f & ~interrupt_valid_wb;
   assign pause_expired_e4 = ~dec_pause_state & dec_pause_state_f & ~(ext_int_ready | ce_int_ready | timer_int_ready | int_timer0_int_hold_f | int_timer1_int_hold_f | nmi_int_detected) & ~interrupt_valid_wb & ~debug_halt_req_f & ~pmu_fw_halt_req_f & ~halt_taken_f;
   assign dec_tlu_stall_dma = dec_fence_pending;
   assign dec_tlu_flush_leak_one_wb = dec_tlu_flush_lower_wb  & dcsr[2] & (dec_tlu_resume_ack | dcsr_single_step_running);
   assign dec_tlu_flush_err_wb = dec_tlu_flush_lower_wb & (ic_perr_wb | iccm_sbecc_wb);
   assign dec_dbg_cmd_fail = illegal_raw_wb & dec_dbg_cmd_done;
   assign trigger_execute[3:0] = {mtdata1_t3[2], mtdata1_t2[2], mtdata1_t1[2], mtdata1_t0[2]};
   assign trigger_data[3:0] = {mtdata1_t3[7], mtdata1_t2[7], mtdata1_t1[7], mtdata1_t0[7]};
   assign trigger_store[3:0] = {mtdata1_t3[1], mtdata1_t2[1], mtdata1_t1[1], mtdata1_t0[1]};
   assign trigger_enabled[3:0] = {(mtdata1_t3[6] | mstatus[0]) & mtdata1_t3[3],
                                  (mtdata1_t2[6] | mstatus[0]) & mtdata1_t2[3],
                                  (mtdata1_t1[6] | mstatus[0]) & mtdata1_t1[3],
                                  (mtdata1_t0[6] | mstatus[0]) & mtdata1_t0[3]};
   assign i0_iside_trigger_has_pri_e4[3:0] = ~( (trigger_execute[3:0] & trigger_data[3:0] & {4{inst_acc_e4_raw}}) |  
                                                ({4{exu_i0_br_error_e4 | exu_i0_br_start_error_e4}}));               
   assign i1_iside_trigger_has_pri_e4[3:0] = ~( ({4{exu_i1_br_error_e4 | exu_i1_br_start_error_e4}}) );  
   assign i0_lsu_trigger_has_pri_e4[3:0] = ~(trigger_store[3:0] & trigger_data[3:0] & {4{lsu_i0_exc_dc4_raw}});
   assign i1_lsu_trigger_has_pri_e4[3:0] = ~(trigger_store[3:0] & trigger_data[3:0] & {4{lsu_i1_exc_dc4_raw}});
   assign i0_trigger_eval_e4 = dec_tlu_i0_valid_e4 | ( dec_i0_load_e4 & lsu_freeze_pulse_e4);
   assign i1_trigger_eval_e4 = dec_tlu_i1_valid_e4 | (~dec_i0_load_e4 & lsu_freeze_pulse_e4);
   assign i0_trigger_e4[3:0] = {4{i0_trigger_eval_e4}} & dec_tlu_packet_e4.i0trigger[3:0] & i0_iside_trigger_has_pri_e4[3:0] & i0_lsu_trigger_has_pri_e4[3:0] & trigger_enabled[3:0];
   assign i1_trigger_e4[3:0] = {4{i1_trigger_eval_e4}} & dec_tlu_packet_e4.i1trigger[3:0] & i1_iside_trigger_has_pri_e4[3:0] & i1_lsu_trigger_has_pri_e4[3:0] & trigger_enabled[3:0];
   assign trigger_chain[2:0] = {mtdata1_t2[5], mtdata1_t1[5], mtdata1_t0[5]};
   assign i0_trigger_chain_masked_e4[3:0] = {i0_trigger_e4[3] & (~trigger_chain[2] | i0_trigger_e4[2]),
                                             i0_trigger_e4[2] & (~trigger_chain[2] | i0_trigger_e4[3]),
                                             i0_trigger_e4[1] & (~trigger_chain[0] | i0_trigger_e4[0]),
                                             i0_trigger_e4[0] & (~trigger_chain[0] | i0_trigger_e4[1])};
   assign i1_trigger_chain_masked_e4[3:0] = {i1_trigger_e4[3] & (~trigger_chain[2] | i1_trigger_e4[2]),
                                             i1_trigger_e4[2] & (~trigger_chain[2] | i1_trigger_e4[3]),
                                             i1_trigger_e4[1] & (~trigger_chain[0] | i1_trigger_e4[0]),
                                             i1_trigger_e4[0] & (~trigger_chain[0] | i1_trigger_e4[1])};
   assign i0_trigger_hit_raw_e4 = |i0_trigger_chain_masked_e4[3:0];
   assign i1_trigger_hit_raw_e4 = |i1_trigger_chain_masked_e4[3:0];
   assign i0_trigger_hit_e4 = ~(dec_tlu_flush_lower_wb | dec_tlu_dbg_halted | lsu_freeze_pulse_e4) & i0_trigger_hit_raw_e4;
   assign i1_trigger_hit_e4 = ~(dec_tlu_flush_lower_wb | ~tlu_i0_commit_cmt | exu_i0_br_mp_e4 | dec_tlu_dbg_halted | lsu_freeze_pulse_e4 | lsu_i0_rfnpc_dc4) & i1_trigger_hit_raw_e4;
   assign dec_tlu_cancel_e4 = (i0_trigger_hit_raw_e4 | i1_trigger_hit_raw_e4) & lsu_freeze_pulse_e4;
   assign trigger_action[3:0] = {mtdata1_t3[6] & mtdata1_t3[9],
                                 mtdata1_t2[6] & mtdata1_t2[9],
                                 mtdata1_t1[6] & mtdata1_t1[9],
                                 mtdata1_t0[6] & mtdata1_t0[9]};
   assign update_hit_bit_e4[3:0] = ({4{i0_trigger_hit_e4                     }} & i0_trigger_chain_masked_e4[3:0]) |
                                   ({4{i1_trigger_hit_e4 & ~i0_trigger_hit_e4}} & i1_trigger_chain_masked_e4[3:0]);
   assign i0_trigger_action_e4 = |(i0_trigger_chain_masked_e4[3:0] & trigger_action[3:0]);
   assign i1_trigger_action_e4 = |(i1_trigger_chain_masked_e4[3:0] & trigger_action[3:0]);
   assign trigger_hit_e4 = i0_trigger_hit_e4 | i1_trigger_hit_e4;
   assign trigger_hit_dmode_e4 = (i0_trigger_hit_e4 & i0_trigger_action_e4) | (i1_trigger_hit_e4 & ~i0_trigger_hit_e4 & i1_trigger_action_e4);
   assign mepc_trigger_hit_sel_pc_e4 = trigger_hit_e4 & ~trigger_hit_dmode_e4;
   assign i_cpu_halt_req_sync_qual = i_cpu_halt_req_sync & ~dec_tlu_debug_mode;
   assign i_cpu_run_req_sync_qual = i_cpu_run_req_sync & ~dec_tlu_debug_mode & pmu_fw_tlu_halted_f;
   rvdff #(10) exthaltff (.*, .clk(free_clk), .din({i_cpu_halt_req_sync_qual, i_cpu_run_req_sync_qual,   cpu_halt_status,
                                                   cpu_halt_ack,   cpu_run_ack, internal_pmu_fw_halt_mode,
                                                   pmu_fw_halt_req_ns, pmu_fw_tlu_halted,
                                                   int_timer0_int_hold, int_timer1_int_hold}),
                                            .dout({i_cpu_halt_req_d1,        i_cpu_run_req_d1_raw,      o_cpu_halt_status,
                                                   o_cpu_halt_ack, o_cpu_run_ack, internal_pmu_fw_halt_mode_f,
                                                   pmu_fw_halt_req_f, pmu_fw_tlu_halted_f,
                                                   int_timer0_int_hold_f, int_timer1_int_hold_f}));
   assign ext_halt_pulse = i_cpu_halt_req_sync_qual & ~i_cpu_halt_req_d1;
   assign enter_pmu_fw_halt_req =  ext_halt_pulse | fw_halt_req;
   assign pmu_fw_halt_req_ns = (enter_pmu_fw_halt_req | (pmu_fw_halt_req_f & ~pmu_fw_tlu_halted)) & ~debug_halt_req_f;
   assign internal_pmu_fw_halt_mode = pmu_fw_halt_req_ns | (internal_pmu_fw_halt_mode_f & ~i_cpu_run_req_d1 & ~debug_halt_req_f);
   assign pmu_fw_tlu_halted = ((pmu_fw_halt_req_f & core_empty & halt_taken & ~enter_debug_halt_req) | (pmu_fw_tlu_halted_f & ~i_cpu_run_req_d1)) & ~debug_halt_req_f;
   assign cpu_halt_ack = i_cpu_halt_req_d1 & pmu_fw_tlu_halted_f;
   assign cpu_halt_status = (pmu_fw_tlu_halted_f & ~i_cpu_run_req_d1) | (o_cpu_halt_status & ~i_cpu_run_req_d1 & ~internal_dbg_halt_mode_f);
   assign cpu_run_ack = (o_cpu_halt_status & i_cpu_run_req_sync_qual) | (o_cpu_run_ack & i_cpu_run_req_sync_qual);
   assign debug_mode_status = internal_dbg_halt_mode_f;
   assign o_debug_mode_status = debug_mode_status; 
   assign i_cpu_run_req_d1 = i_cpu_run_req_d1_raw | ((nmi_int_detected | timer_int_ready | int_timer0_int_hold_f | int_timer1_int_hold_f | (mhwakeup & mhwakeup_ready)) & o_cpu_halt_status);
   lsu_error_pkt_t lsu_error_pkt_dc4;
   rvdff #( $bits(lsu_error_pkt_t)+1 ) lsu_error_dc4ff (.*, .clk(lsu_e3_e4_clk), .din({lsu_error_pkt_dc3, lsu_load_ecc_stbuf_full_dc3}),  .dout({lsu_error_pkt_dc4, lsu_load_ecc_stbuf_full_dc4}));
   logic lsu_single_ecc_error_wb_ns;
   assign lsu_single_ecc_error_wb_ns = lsu_single_ecc_error_incr;
   rvdff #(2) lsu_dccm_errorff (.*, .clk(free_clk), .din({mdseac_locked_ns, lsu_single_ecc_error_wb_ns}), .dout({mdseac_locked_f, lsu_single_ecc_error_wb}));
   logic [31:0] lsu_error_pkt_addr_dc4, lsu_error_pkt_addr_wb;
   assign lsu_error_pkt_addr_dc4[31:0] = lsu_error_pkt_dc4.addr[31:0];
   rvdff #(34) lsu_error_wbff (.*, .clk(lsu_e4_e5_clk), .din({lsu_error_pkt_addr_dc4[31:0], lsu_exc_valid_e4, lsu_i0_exc_dc4}),  .dout({lsu_error_pkt_addr_wb[31:0], lsu_exc_valid_wb, lsu_i0_exc_wb}));
   assign lsu_exc_valid_e4_raw = lsu_error_pkt_dc4.exc_valid & ~(lsu_error_pkt_dc4.inst_pipe & (rfpc_i0_e4 | i0_exception_valid_e4 | exu_i0_br_mp_e4)) & ~dec_tlu_flush_lower_wb;
   assign lsu_i0_exc_dc4_raw =  lsu_error_pkt_dc4.exc_valid & ~lsu_error_pkt_dc4.inst_pipe;
   assign lsu_i1_exc_dc4_raw = lsu_error_pkt_dc4.exc_valid &  lsu_error_pkt_dc4.inst_pipe;
   assign lsu_i0_exc_dc4 = lsu_i0_exc_dc4_raw & lsu_exc_valid_e4_raw & ~i0_trigger_hit_e4;
   assign lsu_i1_exc_dc4 = lsu_i1_exc_dc4_raw & lsu_exc_valid_e4_raw & ~trigger_hit_e4;
   assign lsu_exc_valid_e4 = lsu_i0_exc_dc4 | lsu_i1_exc_dc4;
   assign lsu_exc_ma_dc4 = (lsu_i0_exc_dc4 | lsu_i1_exc_dc4) & ~lsu_error_pkt_dc4.exc_type;
   assign lsu_exc_acc_dc4 = (lsu_i0_exc_dc4 | lsu_i1_exc_dc4) & lsu_error_pkt_dc4.exc_type;
   assign lsu_exc_st_dc4 = (lsu_i0_exc_dc4 | lsu_i1_exc_dc4) & lsu_error_pkt_dc4.inst_type;
   assign lsu_i0_rfnpc_dc4 = dec_tlu_i0_valid_e4 & ~lsu_error_pkt_dc4.inst_pipe & ~lsu_error_pkt_dc4.inst_type &
                             lsu_error_pkt_dc4.single_ecc_error & ~lsu_error_pkt_dc4.dma_valid & ~i0_trigger_hit_e4 & ~lsu_load_ecc_stbuf_full_dc4;
   assign lsu_i1_rfnpc_dc4 = dec_tlu_i1_valid_e4 &  lsu_error_pkt_dc4.inst_pipe & ~lsu_error_pkt_dc4.inst_type &
                             lsu_error_pkt_dc4.single_ecc_error & ~lsu_error_pkt_dc4.dma_valid & ~i0_trigger_hit_e4 & ~i1_trigger_hit_e4 & ~lsu_load_ecc_stbuf_full_dc4;
   assign lsu_i0_rfpc_dc4 = dec_tlu_i0_valid_e4 & ~lsu_error_pkt_dc4.inst_pipe & ~lsu_error_pkt_dc4.inst_type &
                            lsu_error_pkt_dc4.single_ecc_error & ~lsu_error_pkt_dc4.dma_valid & lsu_load_ecc_stbuf_full_dc4;
   assign lsu_i1_rfpc_dc4 = dec_tlu_i1_valid_e4 &  lsu_error_pkt_dc4.inst_pipe & ~lsu_error_pkt_dc4.inst_type &
                             lsu_error_pkt_dc4.single_ecc_error & ~lsu_error_pkt_dc4.dma_valid & lsu_load_ecc_stbuf_full_dc4;
   assign dec_tlu_br0_addr_e4[5:4] = exu_i0_br_index_e4[5:4];
   assign dec_tlu_br0_bank_e4[1:0] = exu_i0_br_bank_e4[1:0];
   assign dec_tlu_br1_addr_e4[5:4] = exu_i1_br_index_e4[5:4];
   assign dec_tlu_br1_bank_e4[1:0] = exu_i1_br_bank_e4[1:0];
   assign tlu_i0_commit_cmt = dec_tlu_i0_valid_e4 &
                              ~rfpc_i0_e4 &
                              ~lsu_i0_exc_dc4 &
                              ~inst_acc_e4 &
                              ~dec_tlu_dbg_halted &
                              ~request_debug_mode_wb &
                              ~i0_trigger_hit_e4;
   assign tlu_i1_commit_cmt = dec_tlu_i1_valid_e4 &
                              ~rfpc_i0_e4 & ~rfpc_i1_e4 &
                              ~exu_i0_br_mp_e4 &
                              ~lsu_i0_exc_dc4 & ~lsu_i1_exc_dc4 &
                              ~lsu_i0_rfnpc_dc4 &
                              ~inst_acc_e4 &
                              ~request_debug_mode_wb &
                              ~trigger_hit_e4;
   assign tlu_i0_kill_writeb_e4 = rfpc_i0_e4 | lsu_i0_exc_dc4 | inst_acc_e4 | (illegal_e4 & dec_tlu_dbg_halted) | i0_trigger_hit_e4 ;
   assign tlu_i1_kill_writeb_e4 = rfpc_i0_e4 | rfpc_i1_e4 | lsu_exc_valid_e4 | exu_i0_br_mp_e4 | inst_acc_e4 | (illegal_e4 & dec_tlu_dbg_halted) | trigger_hit_e4 | lsu_i0_rfnpc_dc4;
   assign rfpc_i0_e4 = dec_tlu_i0_valid_e4 & ~tlu_flush_lower_wb & (exu_i0_br_error_e4 | exu_i0_br_start_error_e4 | ic_perr_e4 | iccm_sbecc_e4 | lsu_i0_rfpc_dc4) & ~i0_trigger_hit_e4;
   assign rfpc_i1_e4 = dec_tlu_i1_valid_e4 & ~tlu_flush_lower_wb & ~i0_exception_valid_e4 & ~exu_i0_br_mp_e4 & ~lsu_i0_exc_dc4 & ~lsu_i0_rfnpc_dc4 &
                       ~(exu_i0_br_error_e4 | exu_i0_br_start_error_e4 | ic_perr_e4 | iccm_sbecc_e4 | lsu_i0_rfpc_dc4) &
                       (exu_i1_br_error_e4 | exu_i1_br_start_error_e4 | lsu_i1_rfpc_dc4) &
                       ~trigger_hit_e4;
   assign dec_tlu_br0_error_e4 = exu_i0_br_error_e4 & dec_tlu_i0_valid_e4 & ~tlu_flush_lower_wb;
   assign dec_tlu_br0_start_error_e4 = exu_i0_br_start_error_e4 & dec_tlu_i0_valid_e4 & ~tlu_flush_lower_wb;
   assign dec_tlu_br0_v_e4 = exu_i0_br_valid_e4 & dec_tlu_i0_valid_e4 & ~tlu_flush_lower_wb & ~exu_i0_br_mp_e4;
   assign dec_tlu_br1_error_e4 = exu_i1_br_error_e4 & dec_tlu_i1_valid_e4 & ~tlu_flush_lower_wb & ~exu_i0_br_mp_e4;
   assign dec_tlu_br1_start_error_e4 = exu_i1_br_start_error_e4 & dec_tlu_i1_valid_e4 & ~tlu_flush_lower_wb & ~exu_i0_br_mp_e4;
   assign dec_tlu_br1_v_e4 = exu_i1_br_valid_e4 & ~tlu_flush_lower_wb & dec_tlu_i1_valid_e4 & ~exu_i0_br_mp_e4 & ~exu_i1_br_mp_e4;
   rvdff #(18)
   bp_wb_ff (.*, .clk(e4e5_clk),
                            .din({exu_i0_br_hist_e4[1:0],
                                  dec_tlu_br0_error_e4,
                                  dec_tlu_br0_start_error_e4,
                                  dec_tlu_br0_v_e4,
                                  exu_i1_br_hist_e4[1:0],
                                  dec_tlu_br1_error_e4,
                                  dec_tlu_br1_start_error_e4,
                                  dec_tlu_br1_v_e4,
                                  dec_tlu_br0_bank_e4[1:0],
                                  dec_tlu_br1_bank_e4[1:0],
                                  exu_i0_br_way_e4,
                                  exu_i1_br_way_e4,
                                  exu_i0_br_middle_e4,
                                  exu_i1_br_middle_e4
                                  }),
                           .dout({dec_tlu_br0_wb_pkt.hist[1:0],
                                  dec_tlu_br0_wb_pkt.br_error,
                                  dec_tlu_br0_wb_pkt.br_start_error,
                                  dec_tlu_br0_wb_pkt.valid,
                                  dec_tlu_br1_wb_pkt.hist[1:0],
                                  dec_tlu_br1_wb_pkt.br_error,
                                  dec_tlu_br1_wb_pkt.br_start_error,
                                  dec_tlu_br1_wb_pkt.valid,
                                  dec_tlu_br0_wb_pkt.bank[1:0],
                                  dec_tlu_br1_wb_pkt.bank[1:0],
                                  dec_tlu_br0_wb_pkt.way,
                                  dec_tlu_br1_wb_pkt.way,
                                  dec_tlu_br0_wb_pkt.middle,
                                  dec_tlu_br1_wb_pkt.middle
                                  }));
   rvdff #(5*2)  bp_wb_ghrff (.*,  .clk(e4e5_clk),
                                               .din({exu_i0_br_fghr_e4[4:0],
                                                     exu_i1_br_fghr_e4[4:0]
                                                     }),
                                              .dout({dec_tlu_br0_wb_pkt.fghr[4:0],
                                                     dec_tlu_br1_wb_pkt.fghr[4:0]
                                                     }));
   rvdff #(2*$bits(dec_tlu_br0_addr_e4[5:4]))
        bp_wb_index_ff (.*,  .clk(e4e5_clk),
                            .din({dec_tlu_br0_addr_e4[5:4],
                                  dec_tlu_br1_addr_e4[5:4]}),
                           .dout({dec_tlu_br0_wb_pkt.index[5:4],
                                  dec_tlu_br1_wb_pkt.index[5:4]}));
   assign       ebreak_e4    =  (dec_tlu_packet_e4.pmu_i0_itype == EBREAK)  & dec_tlu_i0_valid_e4 & ~i0_trigger_hit_e4 & ~dcsr[15];
   assign       ecall_e4     =  (dec_tlu_packet_e4.pmu_i0_itype == ECALL)   & dec_tlu_i0_valid_e4 & ~i0_trigger_hit_e4;
   assign       illegal_e4   =  ~dec_tlu_packet_e4.legal   & dec_tlu_i0_valid_e4 & ~i0_trigger_hit_e4;
   assign       mret_e4      =  (dec_tlu_packet_e4.pmu_i0_itype == MRET)    & dec_tlu_i0_valid_e4 & ~i0_trigger_hit_e4;
   assign       fence_i_e4   =  (dec_tlu_packet_e4.fence_i & dec_tlu_i0_valid_e4 & ~i0_trigger_hit_e4);  
   assign       ic_perr_e4    =  dec_tlu_packet_e4.perr    & dec_tlu_i0_valid_e4 & ~i0_trigger_hit_e4;
   assign       iccm_sbecc_e4 =  dec_tlu_packet_e4.sbecc   & dec_tlu_i0_valid_e4 & ~i0_trigger_hit_e4;
   assign       inst_acc_e4_raw  =  dec_tlu_packet_e4.icaf & dec_tlu_i0_valid_e4;
   assign       inst_acc_e4 = inst_acc_e4_raw & ~rfpc_i0_e4 & ~i0_trigger_hit_e4;
   assign       inst_acc_second_e4 = dec_tlu_packet_e4.icaf_second;
   assign       ebreak_to_debug_mode_e4 = (dec_tlu_packet_e4.pmu_i0_itype == EBREAK)  & dec_tlu_i0_valid_e4 & ~i0_trigger_hit_e4 & dcsr[15];
   assign illegal_e4_qual = illegal_e4 & ~dec_tlu_dbg_halted;
   rvdff #(11)  exctype_wb_ff (.*, .clk(e4e5_clk),
                                .din({ic_perr_e4, iccm_sbecc_e4, ebreak_e4, ebreak_to_debug_mode_e4, ecall_e4, illegal_e4,
                                      illegal_e4_qual,  inst_acc_e4, inst_acc_second_e4, fence_i_e4, mret_e4}),
                               .dout({ic_perr_wb, iccm_sbecc_wb, ebreak_wb, ebreak_to_debug_mode_wb, ecall_wb, illegal_raw_wb,
                                      illegal_wb,       inst_acc_wb, inst_acc_second_wb, fence_i_wb, mret_wb}));
   assign dec_tlu_fence_i_wb = fence_i_wb;
   assign i0_exception_valid_e4 = (ebreak_e4 | ecall_e4 | illegal_e4 | inst_acc_e4) & ~rfpc_i0_e4 & ~dec_tlu_dbg_halted;
   assign exc_cause_e4[4:0] = ( ({5{take_ext_int}}        & 5'h0b) |
                                ({5{take_timer_int}}      & 5'h07) |
                                ({5{take_int_timer0_int}} & 5'h1d) |
                                ({5{take_int_timer1_int}} & 5'h1c) |
                                ({5{take_ce_int}}         & 5'h1e) |
                                ({5{illegal_e4}}          & 5'h02) |
                                ({5{ecall_e4}}            & 5'h0b) |
                                ({5{inst_acc_e4}}         & 5'h01) |
                                ({5{ebreak_e4 | trigger_hit_e4}}        & 5'h03) |
                                ({5{lsu_exc_ma_dc4 & ~lsu_exc_st_dc4}}  & 5'h04) |
                                ({5{lsu_exc_acc_dc4 & ~lsu_exc_st_dc4}} & 5'h05) |
                                ({5{lsu_exc_ma_dc4 & lsu_exc_st_dc4}}   & 5'h06) |
                                ({5{lsu_exc_acc_dc4 & lsu_exc_st_dc4}}  & 5'h07)
                                ) & ~{5{take_nmi}};
   assign mhwakeup_ready =  ~dec_csr_stall_int_ff & mstatus_mie_ns & mip[2]   & mie_ns[2];
   assign ext_int_ready   = ~dec_csr_stall_int_ff & mstatus_mie_ns & mip[2]   & mie_ns[2];
   assign ce_int_ready    = ~dec_csr_stall_int_ff & mstatus_mie_ns & mip[5]  & mie_ns[5];
   assign timer_int_ready = ~dec_csr_stall_int_ff & mstatus_mie_ns & mip[1]   & mie_ns[1];
   assign int_timer0_int_possible = mstatus_mie_ns & mie_ns[4];
   assign int_timer0_int_ready = mip[4] & int_timer0_int_possible;
   assign int_timer1_int_possible = mstatus_mie_ns & mie_ns[3];
   assign int_timer1_int_ready = mip[3] & int_timer1_int_possible;
   assign int_timer_stalled = dec_csr_stall_int_ff | synchronous_flush_e4 | exc_or_int_valid_wb | mret_wb | mret_e4;
   assign int_timer0_int_hold = (int_timer0_int_ready & (pmu_fw_tlu_halted_f | int_timer_stalled)) | (int_timer0_int_possible & int_timer0_int_hold_f & ~interrupt_valid & ~internal_dbg_halt_mode_f);
   assign int_timer1_int_hold = (int_timer1_int_ready & (pmu_fw_tlu_halted_f | int_timer_stalled)) | (int_timer1_int_possible & int_timer1_int_hold_f & ~interrupt_valid & ~internal_dbg_halt_mode_f);
   assign i0_mp_e4 = exu_i0_flush_lower_e4 & ~i0_trigger_hit_e4;
   assign i1_mp_e4 = exu_i1_flush_lower_e4 & ~trigger_hit_e4 & ~lsu_i0_rfnpc_dc4;
   assign internal_dbg_halt_timers = internal_dbg_halt_mode_f & ~dcsr_single_step_running;
   assign block_interrupts = ( (lsu_block_interrupts_e4 & ~dec_tlu_flush_lower_wb) |  
                               (internal_dbg_halt_mode & (~dcsr_single_step_running | dec_tlu_i0_valid_e4)) |  
                               internal_pmu_fw_halt_mode | i_cpu_halt_req_d1 | 
                               take_nmi |  
                               ebreak_to_debug_mode_e4 |  
                               synchronous_flush_e4 |  
                               exc_or_int_valid_wb |  
                               mret_wb |  
                               mret_e4   
                               );
   assign take_ext_int = ext_int_ready & ~block_interrupts;
   assign take_ce_int  = ce_int_ready & ~ext_int_ready & ~block_interrupts;
   assign take_timer_int = timer_int_ready & ~ext_int_ready & ~ce_int_ready & ~block_interrupts;
   assign take_int_timer0_int = (int_timer0_int_ready | int_timer0_int_hold_f) & int_timer0_int_possible & ~dec_csr_stall_int_ff & ~timer_int_ready & ~ext_int_ready & ~ce_int_ready & ~block_interrupts;
   assign take_int_timer1_int = (int_timer1_int_ready | int_timer1_int_hold_f) & int_timer1_int_possible & ~dec_csr_stall_int_ff & ~(int_timer0_int_ready | int_timer0_int_hold_f) & ~timer_int_ready & ~ext_int_ready & ~ce_int_ready & ~block_interrupts;
   assign take_reset = reset_delayed & mpc_reset_run_req;
   assign take_nmi = nmi_int_detected & ~internal_pmu_fw_halt_mode & (~internal_dbg_halt_mode | (dcsr_single_step_running_f & dcsr[11] & ~dec_tlu_i0_valid_e4 & ~dcsr_single_step_done_f)) & ~synchronous_flush_e4 & ~mret_e4 & ~take_reset & ~ebreak_to_debug_mode_e4;
   assign interrupt_valid = take_ext_int | take_timer_int | take_nmi | take_ce_int | take_int_timer0_int | take_int_timer1_int;
   assign vectored_cause[5:0] = ({1'b0, exc_cause_e4[4:0]} << 1);
   assign {vpath_overflow_nc, vectored_path[31:1]} = {mtvec[30:1], 1'b0} + {25'b0, vectored_cause[5:0]};
   assign interrupt_path[31:1] = take_nmi ? nmi_vec[31:1] : ((mtvec[0] == 1'b1) ? vectored_path[31:1] : {mtvec[30:1], 1'b0});
   assign sel_npc_e4 = lsu_i0_rfnpc_dc4 | (lsu_i1_rfnpc_dc4 & tlu_i1_commit_cmt) | fence_i_e4 | (i_cpu_run_req_d1 & ~interrupt_valid);
   assign sel_npc_wb = (i_cpu_run_req_d1 & pmu_fw_tlu_halted_f) | pause_expired_e4;
   assign synchronous_flush_e4 = i0_exception_valid_e4 |  
                                 i0_mp_e4 | i1_mp_e4 |   
                                 rfpc_i0_e4 | rfpc_i1_e4 |  
                                 lsu_exc_valid_e4 |   
                                 fence_i_e4 |   
                                 lsu_i0_rfnpc_dc4 | lsu_i1_rfnpc_dc4 |
                                 debug_resume_req_f |  
                                 sel_npc_wb |   
                                 dec_tlu_wr_pause_wb |  
                                 trigger_hit_e4;  
   assign tlu_flush_lower_e4 = interrupt_valid | mret_e4 | synchronous_flush_e4 | take_halt | take_reset;
   assign tlu_flush_path_e4[31:1] = take_reset ? rst_vec[31:1] :
                                     ( ({31{~take_nmi & i0_mp_e4}} & exu_i0_flush_path_e4[31:1]) |
                                      ({31{~take_nmi & ~i0_mp_e4 & i1_mp_e4 & ~rfpc_i0_e4 & ~lsu_i0_exc_dc4}} & exu_i1_flush_path_e4[31:1]) |
                                      ({31{~take_nmi & sel_npc_e4}} & npc_e4[31:1]) |
                                      ({31{~take_nmi & rfpc_i0_e4}} & dec_tlu_i0_pc_e4[31:1]) |
                                      ({31{~take_nmi & rfpc_i1_e4}} & dec_tlu_i1_pc_e4[31:1]) |
                                      ({31{interrupt_valid}} & interrupt_path[31:1]) |
                                      ({31{(i0_exception_valid_e4 | lsu_exc_valid_e4 | (trigger_hit_e4 & ~trigger_hit_dmode_e4)) & ~interrupt_valid}} & {mtvec[30:1],1'b0}) |
                                      ({31{~take_nmi & mret_e4 & ~wr_mepc_wb}} & mepc[31:1]) |
                                      ({31{~take_nmi & debug_resume_req_f}} & dpc[31:1]) |
                                      ({31{~take_nmi & sel_npc_wb}} & npc_wb[31:1]) |
                                      ({31{~take_nmi & mret_e4 & wr_mepc_wb}} & dec_csr_wrdata_wb[31:1]) );
   rvdff #(31)  flush_lower_ff (.*, .clk(e4e5_int_clk),
                                  .din({tlu_flush_path_e4[31:1]}),
                                 .dout({tlu_flush_path_wb[31:1]}));
   assign dec_tlu_flush_lower_wb = tlu_flush_lower_wb;
   assign dec_tlu_flush_path_wb[31:1] = tlu_flush_path_wb[31:1];
   assign exc_or_int_valid = lsu_exc_valid_e4 | i0_exception_valid_e4 | interrupt_valid | (trigger_hit_e4 & ~trigger_hit_dmode_e4);
   assign lsu_block_interrupts_dc3 = lsu_freeze_external_ints_dc3 & ~dec_tlu_flush_lower_wb;
   rvdff #(15)  excinfo_wb_ff (.*, .clk(e4e5_int_clk),
                                .din({interrupt_valid, i0_exception_valid_e4, exc_or_int_valid,
                                      exc_cause_e4[4:0], tlu_i0_commit_cmt & ~illegal_e4, tlu_i1_commit_cmt,
                                       mepc_trigger_hit_sel_pc_e4, trigger_hit_e4, i0_trigger_hit_e4,
                                      take_nmi, pause_expired_e4 }),
                               .dout({interrupt_valid_wb, i0_exception_valid_wb, exc_or_int_valid_wb,
                                      exc_cause_wb[4:0], i0_valid_wb, i1_valid_wb,
                                       mepc_trigger_hit_sel_pc_wb, trigger_hit_wb, i0_trigger_hit_wb,
                                      take_nmi_wb, pause_expired_wb}));
   assign dec_csr_wen_wb_mod = dec_csr_wen_wb & ~trigger_hit_wb;
   assign wr_mstatus_wb = dec_csr_wen_wb_mod & (dec_csr_wraddr_wb[11:0] == 12'h300);
   assign set_mie_pmu_fw_halt = ~mpmc_b_ns[1] & wr_mpmc_wb & dec_csr_wrdata_wb[0] & ~internal_dbg_halt_mode_f3;
   assign mstatus_ns[1:0] = ( ({2{~wr_mstatus_wb & exc_or_int_valid_wb}} & {(mstatus[0] | set_mie_pmu_fw_halt), 1'b0}) |
                              ({2{ wr_mstatus_wb & exc_or_int_valid_wb}} & {dec_csr_wrdata_wb[3], 1'b0}) |
                              ({2{mret_wb & ~exc_or_int_valid_wb}} & {1'b1, mstatus[1]}) |
                              ({2{set_mie_pmu_fw_halt & ~exc_or_int_valid_wb}} & {mstatus[1], 1'b1}) |
                              ({2{wr_mstatus_wb & ~exc_or_int_valid_wb}} & {dec_csr_wrdata_wb[7], dec_csr_wrdata_wb[3]}) |
                              ({2{~wr_mstatus_wb & ~exc_or_int_valid_wb & ~mret_wb & ~set_mie_pmu_fw_halt}} & mstatus[1:0]) );
   assign mstatus_mie_ns = mstatus_ns[0] & (~dcsr_single_step_running_f | dcsr[11]);
   rvdff #(2)  mstatus_ff (.*, .clk(free_clk), .din(mstatus_ns[1:0]), .dout(mstatus[1:0]));
   assign wr_mtvec_wb = dec_csr_wen_wb_mod & (dec_csr_wraddr_wb[11:0] == 12'h305);
   assign mtvec_ns[30:0] = {dec_csr_wrdata_wb[31:2], dec_csr_wrdata_wb[0]} ;
   rvdffe #(31)  mtvec_ff (.*, .en(wr_mtvec_wb), .din(mtvec_ns[30:0]), .dout(mtvec[30:0]));
   assign ce_int = (mdccme_ce_req | miccme_ce_req | mice_ce_req);
   assign mip_ns[5:0] = {ce_int, dec_timer_t0_pulse, dec_timer_t1_pulse, mexintpend, timer_int_sync, mip[0]};
   rvdff #(6)  mip_ff (.*, .clk(free_clk), .din(mip_ns[5:0]), .dout(mip[5:0]));
   assign wr_mie_wb = dec_csr_wen_wb_mod & (dec_csr_wraddr_wb[11:0] == 12'h304);
   assign mie_ns[5:0] = wr_mie_wb ? {dec_csr_wrdata_wb[30:28], dec_csr_wrdata_wb[11], dec_csr_wrdata_wb[7], dec_csr_wrdata_wb[3]} : mie[5:0];
   rvdff #(6)  mie_ff (.*, .clk(csr_wr_clk), .din(mie_ns[5:0]), .dout(mie[5:0]));
   assign wr_mcyclel_wb = dec_csr_wen_wb_mod & (dec_csr_wraddr_wb[11:0] == 12'hb00);
   logic mcyclel_cout_in;
   assign kill_ebreak_count_wb = ebreak_to_debug_mode_wb & dcsr[10];
   assign mcyclel_cout_in = ~(kill_ebreak_count_wb | (dec_tlu_dbg_halted & dcsr[10]) | dec_tlu_pmu_fw_halted);
   assign {mcyclel_cout, mcyclel_inc[31:0]} = mcyclel[31:0] + {31'b0, mcyclel_cout_in};
   assign mcyclel_ns[31:0] = wr_mcyclel_wb ? dec_csr_wrdata_wb[31:0] : mcyclel_inc[31:0];
   rvdffe #(32) mcyclel_ff      (.*, .en(wr_mcyclel_wb | mcyclel_cout_in), .din(mcyclel_ns[31:0]), .dout(mcyclel[31:0]));
   rvdff   #(1) mcyclef_cout_ff (.*, .clk(free_clk), .din(mcyclel_cout & ~wr_mcycleh_wb), .dout(mcyclel_cout_f));
   assign wr_mcycleh_wb = dec_csr_wen_wb_mod & (dec_csr_wraddr_wb[11:0] == 12'hb80);
   assign {mcycleh_cout_nc, mcycleh_inc[31:0]} = mcycleh[31:0] + {31'b0, mcyclel_cout_f};
   assign mcycleh_ns[31:0] = wr_mcycleh_wb ? dec_csr_wrdata_wb[31:0] : mcycleh_inc[31:0];
   rvdffe #(32)  mcycleh_ff (.*, .en(wr_mcycleh_wb | mcyclel_cout_f), .din(mcycleh_ns[31:0]), .dout(mcycleh[31:0]));
   logic i0_valid_no_ebreak_ecall_wb;
   assign i0_valid_no_ebreak_ecall_wb = i0_valid_wb & ~(ebreak_wb | ecall_wb | ebreak_to_debug_mode_wb);
   assign wr_minstretl_wb = dec_csr_wen_wb_mod & (dec_csr_wraddr_wb[11:0] == 12'hb02);
   assign {minstretl_cout, minstretl_inc[31:0]} = minstretl[31:0] + {31'b0,i0_valid_no_ebreak_ecall_wb} + {31'b0,i1_valid_wb};
   assign minstret_enable = i0_valid_no_ebreak_ecall_wb | i1_valid_wb;
   assign minstretl_ns[31:0] = wr_minstretl_wb ? dec_csr_wrdata_wb[31:0] : minstretl_inc[31:0];
   rvdffe #(32)  minstretl_ff (.*, .en(minstret_enable | wr_minstretl_wb), .din(minstretl_ns[31:0]), .dout(minstretl[31:0]));
   logic minstret_enable_f;
   rvdff #(2) minstretf_cout_ff (.*, .clk(free_clk), .din({minstret_enable, minstretl_cout & ~wr_minstreth_wb}), .dout({minstret_enable_f, minstretl_cout_f}));
   assign minstretl_read[31:0] = minstretl[31:0];
   assign wr_minstreth_wb = dec_csr_wen_wb_mod & (dec_csr_wraddr_wb[11:0] == 12'hb82);
   assign {minstreth_cout_nc, minstreth_inc[31:0]} = minstreth[31:0] + {31'b0, minstretl_cout_f};
   assign minstreth_ns[31:0] = wr_minstreth_wb ? dec_csr_wrdata_wb[31:0] : minstreth_inc[31:0];
   rvdffe #(32)  minstreth_ff (.*, .en(minstret_enable_f | wr_minstreth_wb), .din(minstreth_ns[31:0]), .dout(minstreth[31:0]));
   assign minstreth_read[31:0] = minstreth_inc[31:0];
   assign wr_mscratch_wb = dec_csr_wen_wb_mod & (dec_csr_wraddr_wb[11:0] == 12'h340);
   rvdffe #(32)  mscratch_ff (.*, .en(wr_mscratch_wb), .din(dec_csr_wrdata_wb[31:0]), .dout(mscratch[31:0]));
   logic sel_exu_npc_e4, sel_flush_npc_e4, sel_i0_npc_e4, sel_hold_npc_e4;
   assign sel_exu_npc_e4 = ~dec_tlu_dbg_halted & ~tlu_flush_lower_wb & (dec_tlu_i0_valid_e4 | dec_tlu_i1_valid_e4) & ~(dec_tlu_i1_valid_e4 & lsu_i0_rfnpc_dc4);
   assign sel_i0_npc_e4 = ~dec_tlu_dbg_halted & ~tlu_flush_lower_wb & dec_tlu_i0_valid_e4 & lsu_i0_rfnpc_dc4 & dec_tlu_i1_valid_e4;
   assign sel_flush_npc_e4 = ~dec_tlu_dbg_halted & tlu_flush_lower_wb & ~dec_tlu_flush_noredir_wb;
   assign sel_hold_npc_e4 = ~sel_exu_npc_e4 & ~sel_flush_npc_e4 & ~sel_i0_npc_e4;
   assign npc_e4[31:1] = ( ({31{sel_exu_npc_e4}} & exu_npc_e4[31:1]) |
                           ({31{sel_i0_npc_e4}} & dec_tlu_i1_pc_e4[31:1]) |
                           ({31{~mpc_reset_run_req & reset_delayed}} & rst_vec[31:1]) |  
                           ({31{(sel_flush_npc_e4)}} & tlu_flush_path_wb[31:1]) |
                           ({31{(sel_hold_npc_e4)}} & npc_wb[31:1]) );
   rvdffe #(31)  npwbc_ff (.*, .en(sel_i0_npc_e4 | sel_exu_npc_e4 | sel_flush_npc_e4 | reset_delayed), .din(npc_e4[31:1]), .dout(npc_wb[31:1]));
   logic pc0_valid_e4, pc1_valid_e4;
   assign pc0_valid_e4 = ~dec_tlu_dbg_halted & dec_tlu_i0_valid_e4;
   assign pc1_valid_e4 = ~dec_tlu_dbg_halted & dec_tlu_i0_valid_e4 & dec_tlu_i1_valid_e4 & ~lsu_i0_exc_dc4 & ~rfpc_i0_e4 & ~inst_acc_e4 & ~i0_trigger_hit_e4;
   assign pc_e4[31:1] = ( ({31{ pc0_valid_e4 & ~pc1_valid_e4}} & dec_tlu_i0_pc_e4[31:1]) |
                          ({31{ pc1_valid_e4}} & dec_tlu_i1_pc_e4[31:1]) |
                          ({31{~pc0_valid_e4 & ~pc1_valid_e4}} & pc_wb[31:1])
                          );
   rvdffe #(31)  pwbc_ff (.*, .en(pc0_valid_e4 | pc1_valid_e4), .din(pc_e4[31:1]), .dout(pc_wb[31:1]));
   assign wr_mepc_wb = dec_csr_wen_wb_mod & (dec_csr_wraddr_wb[11:0] == 12'h341);
   assign mepc_ns[31:1] = ( ({31{i0_exception_valid_wb | lsu_exc_valid_wb | mepc_trigger_hit_sel_pc_wb}} & pc_wb[31:1]) |
                            ({31{interrupt_valid_wb}} & npc_wb[31:1]) |
                            ({31{wr_mepc_wb & ~exc_or_int_valid_wb}} & dec_csr_wrdata_wb[31:1]) |
                            ({31{~wr_mepc_wb & ~exc_or_int_valid_wb}} & mepc[31:1]) );
   rvdff #(31)  mepc_ff (.*, .clk(e4e5_int_clk), .din(mepc_ns[31:1]), .dout(mepc[31:1]));
   assign wr_mcause_wb = dec_csr_wen_wb_mod & (dec_csr_wraddr_wb[11:0] == 12'h342);
   assign mcause_ns[31:0] = ( ({32{exc_or_int_valid_wb & take_nmi_wb & nmi_lsu_store_type_f}} & {32'hf000_0000}) |
                              ({32{exc_or_int_valid_wb & take_nmi_wb & nmi_lsu_load_type_f}} & {32'hf000_0001}) |
                              ({32{exc_or_int_valid_wb & ~take_nmi_wb}} & {interrupt_valid_wb, 26'b0, exc_cause_wb[4:0]}) |
                              ({32{wr_mcause_wb & ~exc_or_int_valid_wb}} & dec_csr_wrdata_wb[31:0]) |
                              ({32{~wr_mcause_wb & ~exc_or_int_valid_wb}} & mcause[31:0]) );
   rvdff #(32)  mcause_ff (.*, .clk(e4e5_int_clk), .din(mcause_ns[31:0]), .dout(mcause[31:0]));
   assign wr_mtval_wb = dec_csr_wen_wb_mod & (dec_csr_wraddr_wb[11:0] == 12'h343);
   assign mtval_capture_pc_wb = exc_or_int_valid_wb & (ebreak_wb | (inst_acc_wb & ~inst_acc_second_wb) | mepc_trigger_hit_sel_pc_wb) & ~take_nmi_wb;
   assign mtval_capture_pc_plus2_wb = exc_or_int_valid_wb & (inst_acc_wb & inst_acc_second_wb) & ~take_nmi_wb;
   assign mtval_capture_inst_wb = exc_or_int_valid_wb & illegal_wb & ~take_nmi_wb;
   assign mtval_capture_lsu_wb = exc_or_int_valid_wb & lsu_exc_valid_wb & ~take_nmi_wb;
   assign mtval_clear_wb = exc_or_int_valid_wb & ~mtval_capture_pc_wb & ~mtval_capture_inst_wb & ~mtval_capture_lsu_wb & ~mepc_trigger_hit_sel_pc_wb;
   assign mtval_ns[31:0] = (({32{mtval_capture_pc_wb}} & {pc_wb[31:1], 1'b0}) |
                            ({32{mtval_capture_pc_plus2_wb}} & {pc_wb[31:1] + 31'b1, 1'b0}) |
                            ({32{mtval_capture_inst_wb}} & dec_illegal_inst[31:0]) |
                            ({32{mtval_capture_lsu_wb}} & lsu_error_pkt_addr_wb[31:0]) |
                            ({32{wr_mtval_wb & ~interrupt_valid_wb}} & dec_csr_wrdata_wb[31:0]) |
                            ({32{~take_nmi_wb & ~wr_mtval_wb & ~mtval_capture_pc_wb & ~mtval_capture_inst_wb & ~mtval_clear_wb & ~mtval_capture_lsu_wb}} & mtval[31:0]) );
   rvdff #(32)  mtval_ff (.*, .clk(e4e5_int_clk), .din(mtval_ns[31:0]), .dout(mtval[31:0]));
   assign wr_mcgc_wb = dec_csr_wen_wb_mod & (dec_csr_wraddr_wb[11:0] == 12'h7f8);
   rvdffe #(9)  mcgc_ff (.*, .en(wr_mcgc_wb), .din(dec_csr_wrdata_wb[8:0]), .dout(mcgc[8:0]));
   assign dec_tlu_misc_clk_override = mcgc[8];
   assign dec_tlu_dec_clk_override  = mcgc[7];
   assign dec_tlu_exu_clk_override  = mcgc[6];
   assign dec_tlu_ifu_clk_override  = mcgc[5];
   assign dec_tlu_lsu_clk_override  = mcgc[4];
   assign dec_tlu_bus_clk_override  = mcgc[3];
   assign dec_tlu_pic_clk_override  = mcgc[2];
   assign dec_tlu_dccm_clk_override = mcgc[1];
   assign dec_tlu_icm_clk_override  = mcgc[0];
   assign wr_mfdc_wb = dec_csr_wen_wb_mod & (dec_csr_wraddr_wb[11:0] == 12'h7f9);
   rvdffe #(14)  mfdc_ff (.*, .en(wr_mfdc_wb), .din(mfdc_ns[13:0]), .dout(mfdc_int[13:0]));
   assign mfdc_ns[13:0] = {~dec_csr_wrdata_wb[18:16],dec_csr_wrdata_wb[10:7], ~dec_csr_wrdata_wb[6], dec_csr_wrdata_wb[5:0]};
   assign mfdc[18:0] = {~mfdc_int[13:11], 5'b0, mfdc_int[10:7], ~mfdc_int[6], mfdc_int[5:0]};
   assign dec_tlu_dma_qos_prty[2:0] = mfdc[18:16];
   assign dec_tlu_dual_issue_disable = mfdc[10];
   assign dec_tlu_core_ecc_disable = mfdc[8];
   assign dec_tlu_sec_alu_disable = mfdc[7];
   assign dec_tlu_sideeffect_posted_disable = mfdc[6];
   assign dec_tlu_non_blocking_disable = mfdc[5];
   assign dec_tlu_fast_div_disable = mfdc[4];
   assign dec_tlu_bpred_disable = mfdc[3];
   assign dec_tlu_wb_coalescing_disable = mfdc[2];
   assign dec_tlu_ld_miss_byp_wb_disable = mfdc[1];
   assign dec_tlu_pipelining_disable = mfdc[0];
   assign dec_tlu_wr_pause_wb = dec_csr_wen_wb_mod & (dec_csr_wraddr_wb[11:0] == 12'h7c2) & ~interrupt_valid_wb;
   assign wr_mrac_wb = dec_csr_wen_wb_mod & (dec_csr_wraddr_wb[11:0] == 12'h7c0);
   logic [31:0] mrac_in;
   assign mrac_in[31:0] = {dec_csr_wrdata_wb[31], dec_csr_wrdata_wb[30] & ~dec_csr_wrdata_wb[31],
                           dec_csr_wrdata_wb[29], dec_csr_wrdata_wb[28] & ~dec_csr_wrdata_wb[29],
                           dec_csr_wrdata_wb[27], dec_csr_wrdata_wb[26] & ~dec_csr_wrdata_wb[27],
                           dec_csr_wrdata_wb[25], dec_csr_wrdata_wb[24] & ~dec_csr_wrdata_wb[25],
                           dec_csr_wrdata_wb[23], dec_csr_wrdata_wb[22] & ~dec_csr_wrdata_wb[23],
                           dec_csr_wrdata_wb[21], dec_csr_wrdata_wb[20] & ~dec_csr_wrdata_wb[21],
                           dec_csr_wrdata_wb[19], dec_csr_wrdata_wb[18] & ~dec_csr_wrdata_wb[19],
                           dec_csr_wrdata_wb[17], dec_csr_wrdata_wb[16] & ~dec_csr_wrdata_wb[17],
                           dec_csr_wrdata_wb[15], dec_csr_wrdata_wb[14] & ~dec_csr_wrdata_wb[15],
                           dec_csr_wrdata_wb[13], dec_csr_wrdata_wb[12] & ~dec_csr_wrdata_wb[13],
                           dec_csr_wrdata_wb[11], dec_csr_wrdata_wb[10] & ~dec_csr_wrdata_wb[11],
                           dec_csr_wrdata_wb[9], dec_csr_wrdata_wb[8] & ~dec_csr_wrdata_wb[9],
                           dec_csr_wrdata_wb[7], dec_csr_wrdata_wb[6] & ~dec_csr_wrdata_wb[7],
                           dec_csr_wrdata_wb[5], dec_csr_wrdata_wb[4] & ~dec_csr_wrdata_wb[5],
                           dec_csr_wrdata_wb[3], dec_csr_wrdata_wb[2] & ~dec_csr_wrdata_wb[3],
                           dec_csr_wrdata_wb[1], dec_csr_wrdata_wb[0] & ~dec_csr_wrdata_wb[1]};
   rvdffe #(32)  mrac_ff (.*, .en(wr_mrac_wb), .din(mrac_in[31:0]), .dout(mrac[31:0]));
   assign dec_tlu_mrac_ff[31:0] = mrac[31:0];
   assign wr_mdeau_wb = dec_csr_wen_wb_mod & (dec_csr_wraddr_wb[11:0] == 12'hbc0);
   assign mdseac_locked_ns = mdseac_en | (mdseac_locked_f & ~wr_mdeau_wb);
   assign mdseac_en = (lsu_imprecise_error_store_any | lsu_imprecise_error_load_any) & ~mdseac_locked_f;
   rvdffe #(32)  mdseac_ff (.*, .en(mdseac_en), .din(lsu_imprecise_error_addr_any[31:0]), .dout(mdseac[31:0]));
   assign wr_mpmc_wb = dec_csr_wen_wb_mod & (dec_csr_wraddr_wb[11:0] == 12'h7c6);
   assign fw_halt_req = wr_mpmc_wb & dec_csr_wrdata_wb[0] & ~internal_dbg_halt_mode_f3 & ~interrupt_valid_wb;
   assign mpmc_b_ns[1] = wr_mpmc_wb ? ~dec_csr_wrdata_wb[1] : ~mpmc[1];
   rvdff #(1)  mpmc_ff (.*, .clk(csr_wr_clk), .din(mpmc_b_ns[1]), .dout(mpmc_b[1]));
   assign mpmc[1] = ~mpmc_b[1];
   logic [31:27] csr_sat;
   assign csr_sat[31:27] = (dec_csr_wrdata_wb[31:27] > 5'd26) ? 5'd26 : dec_csr_wrdata_wb[31:27];
   assign wr_micect_wb = dec_csr_wen_wb_mod & (dec_csr_wraddr_wb[11:0] == 12'h7f0);
   assign {micect_cout_nc, micect_inc[26:0]} = micect[26:0] + {26'b0, ic_perr_wb};
   assign micect_ns =  wr_micect_wb ? {csr_sat[31:27], dec_csr_wrdata_wb[26:0]} : {micect[31:27], micect_inc[26:0]};
   rvdffe #(32)  micect_ff (.*, .en(wr_micect_wb | ic_perr_wb), .din(micect_ns[31:0]), .dout(micect[31:0]));
   assign mice_ce_req = |({32'b1 << micect[31:27]} & {5'b0, micect[26:0]});
   assign wr_miccmect_wb = dec_csr_wen_wb_mod & (dec_csr_wraddr_wb[11:0] == 12'h7f1);
   assign {miccmect_cout_nc, miccmect_inc[26:0]} = miccmect[26:0] + {26'b0, iccm_sbecc_wb | iccm_dma_sb_error};
   assign miccmect_ns =  wr_miccmect_wb ? {csr_sat[31:27], dec_csr_wrdata_wb[26:0]} : {miccmect[31:27], miccmect_inc[26:0]};
   rvdffe #(32)  miccmect_ff (.*, .en(wr_miccmect_wb | iccm_sbecc_wb | iccm_dma_sb_error), .din(miccmect_ns[31:0]), .dout(miccmect[31:0]));
   assign miccme_ce_req = |({32'b1 << miccmect[31:27]} & {5'b0, miccmect[26:0]});
   assign wr_mdccmect_wb = dec_csr_wen_wb_mod & (dec_csr_wraddr_wb[11:0] == 12'h7f2);
   assign {mdccmect_cout_nc, mdccmect_inc[26:0]} = mdccmect[26:0] + {26'b0, lsu_single_ecc_error_wb};
   assign mdccmect_ns =  wr_mdccmect_wb ? {csr_sat[31:27], dec_csr_wrdata_wb[26:0]} : {mdccmect[31:27], mdccmect_inc[26:0]};
   rvdffe #(32)  mdccmect_ff (.*, .en(wr_mdccmect_wb | lsu_single_ecc_error_wb), .din(mdccmect_ns[31:0]), .dout(mdccmect[31:0]));
   assign mdccme_ce_req = |({32'b1 << mdccmect[31:27]} & {5'b0, mdccmect[26:0]});
   assign wr_meivt_wb = dec_csr_wen_wb_mod & (dec_csr_wraddr_wb[11:0] == 12'hbc8);
   rvdffe #(22)  meivt_ff (.*, .en(wr_meivt_wb), .din(dec_csr_wrdata_wb[31:10]), .dout(meivt[31:10]));
   assign wr_meihap_wb = wr_meicpct_wb;
   rvdffe #(8)  meihap_ff (.*, .en(wr_meihap_wb), .din(pic_claimid[7:0]), .dout(meihap[9:2]));
   assign wr_meicurpl_wb = dec_csr_wen_wb_mod & (dec_csr_wraddr_wb[11:0] == 12'hbcc);
   assign meicurpl_ns[3:0] = wr_meicurpl_wb ? dec_csr_wrdata_wb[3:0] : meicurpl[3:0];
   rvdff #(4)  meicurpl_ff (.*, .clk(csr_wr_clk), .din(meicurpl_ns[3:0]), .dout(meicurpl[3:0]));
   assign dec_tlu_meicurpl[3:0] = meicurpl[3:0];
   assign wr_meicidpl_wb = (dec_csr_wen_wb_mod & (dec_csr_wraddr_wb[11:0] == 12'hbcb));
   assign meicidpl_ns[3:0] = wr_meicpct_wb ? pic_pl[3:0] : (wr_meicidpl_wb ? dec_csr_wrdata_wb[3:0] : meicidpl[3:0]);
   rvdff #(4)  meicidpl_ff (.*, .clk(csr_wr_clk), .din(meicidpl_ns[3:0]), .dout(meicidpl[3:0]));
   assign wr_meicpct_wb = dec_csr_wen_wb_mod & (dec_csr_wraddr_wb[11:0] == 12'hbca);
   assign wr_meipt_wb = dec_csr_wen_wb_mod & (dec_csr_wraddr_wb[11:0] == 12'hbc9);
   assign meipt_ns[3:0] = wr_meipt_wb ? dec_csr_wrdata_wb[3:0] : meipt[3:0];
   rvdff #(4)  meipt_ff (.*, .clk(active_clk), .din(meipt_ns[3:0]), .dout(meipt[3:0]));
   assign dec_tlu_meipt[3:0] = meipt[3:0];
   logic [8:6] dcsr_cause;
   assign trigger_hit_for_dscr_cause_wb = trigger_hit_dmode_wb | (trigger_hit_wb & dcsr_single_step_done_f);
   assign dcsr_cause[8:6] = ( ({3{dcsr_single_step_done_f & ~ebreak_to_debug_mode_wb & ~trigger_hit_for_dscr_cause_wb & ~debug_halt_req}} & 3'b100) |
                              ({3{debug_halt_req & ~ebreak_to_debug_mode_wb & ~trigger_hit_for_dscr_cause_wb}} &  3'b011) |
                              ({3{ebreak_to_debug_mode_wb & ~trigger_hit_for_dscr_cause_wb}} &  3'b001) |
                              ({3{trigger_hit_for_dscr_cause_wb}} & 3'b010));
   assign wr_dcsr_wb = allow_dbg_halt_csr_write & dec_csr_wen_wb_mod & (dec_csr_wraddr_wb[11:0] == 12'h7b0);
   logic enter_debug_halt_req_le, dcsr_cause_upgradeable;
   assign dcsr_cause_upgradeable = internal_dbg_halt_mode_f & (dcsr[8:6] == 3'b011);
   assign enter_debug_halt_req_le = enter_debug_halt_req & (~dbg_tlu_halted | dcsr_cause_upgradeable);
   assign nmi_in_debug_mode = nmi_int_detected_f & internal_dbg_halt_mode_f;
   assign dcsr_ns[15:2] = enter_debug_halt_req_le ? {dcsr[15:9], dcsr_cause[8:6], dcsr[5:2]} :
                          (wr_dcsr_wb ? {dec_csr_wrdata_wb[15], 3'b0, dec_csr_wrdata_wb[11:10], 1'b0, dcsr[8:6], 2'b00, nmi_in_debug_mode | dcsr[3], dec_csr_wrdata_wb[2]} :
                           {dcsr[15:4], nmi_in_debug_mode, dcsr[2]});
   rvdffe #(14)  dcsr_ff (.*, .en(enter_debug_halt_req_le | wr_dcsr_wb | internal_dbg_halt_mode | take_nmi_wb), .din(dcsr_ns[15:2]), .dout(dcsr[15:2]));
   assign wr_dpc_wb = allow_dbg_halt_csr_write & dec_csr_wen_wb_mod & (dec_csr_wraddr_wb[11:0] == 12'h7b1);
   assign dpc_capture_npc = dbg_tlu_halted & ~dbg_tlu_halted_f & ~request_debug_mode_done_f;
   assign dpc_capture_pc = request_debug_mode_wb;
   assign dpc_ns[31:1] = ( ({31{~dpc_capture_pc & ~dpc_capture_npc & wr_dpc_wb}} & dec_csr_wrdata_wb[31:1]) |
                           ({31{dpc_capture_pc}} & pc_wb[31:1]) |
                           ({31{~dpc_capture_pc & dpc_capture_npc}} & npc_wb[31:1]) );
   rvdffe #(31)  dpc_ff (.*, .en(wr_dpc_wb | dpc_capture_pc | dpc_capture_npc), .din(dpc_ns[31:1]), .dout(dpc[31:1]));
   assign dicawics_ns[18:2] = {dec_csr_wrdata_wb[24], dec_csr_wrdata_wb[21:20], dec_csr_wrdata_wb[15:2]};
   assign wr_dicawics_wb = allow_dbg_halt_csr_write & dec_csr_wen_wb_mod & (dec_csr_wraddr_wb[11:0] == 12'h7c8);
   rvdffe #(17)  dicawics_ff (.*, .en(wr_dicawics_wb), .din(dicawics_ns[18:2]), .dout(dicawics[18:2]));
   assign dicad0_ns[31:0] = wr_dicad0_wb ? dec_csr_wrdata_wb[31:0] : ifu_ic_debug_rd_data[31:0];
   assign wr_dicad0_wb = allow_dbg_halt_csr_write & dec_csr_wen_wb_mod & (dec_csr_wraddr_wb[11:0] == 12'h7c9);
   rvdffe #(32)  dicad0_ff (.*, .en(wr_dicad0_wb | ifu_ic_debug_rd_data_valid), .din(dicad0_ns[31:0]), .dout(dicad0[31:0]));
   assign dicad1_ns[1:0] = wr_dicad1_wb ? dec_csr_wrdata_wb[1:0] : ifu_ic_debug_rd_data[33:32];
   assign wr_dicad1_wb = allow_dbg_halt_csr_write & dec_csr_wen_wb_mod & (dec_csr_wraddr_wb[11:0] == 12'h7ca);
   rvdffs #(2)  dicad1_ff (.*, .clk(active_clk), .en(wr_dicad1_wb | ifu_ic_debug_rd_data_valid), .din(dicad1_ns[1:0]), .dout(dicad1[1:0]));
   assign dec_tlu_ic_diag_pkt.icache_wrdata[33:0] = {dicad1[1:0], dicad0[31:0]};
   assign dec_tlu_ic_diag_pkt.icache_dicawics[18:2] = dicawics[18:2];
   logic icache_rd_valid, icache_wr_valid, icache_rd_valid_f, icache_wr_valid_f;
   assign icache_rd_valid = allow_dbg_halt_csr_write & dec_csr_any_unq_d & dec_i0_decode_d & ~dec_csr_wen_unq_d & (dec_csr_rdaddr_d[11:0] == 12'h7cb);
   assign icache_wr_valid = allow_dbg_halt_csr_write & dec_csr_wen_wb_mod & (dec_csr_wraddr_wb[11:0] == 12'h7cb);
   rvdff #(2)  dicgo_ff (.*, .clk(active_clk), .din({icache_rd_valid, icache_wr_valid}), .dout({icache_rd_valid_f, icache_wr_valid_f}));
   assign dec_tlu_ic_diag_pkt.icache_rd_valid = icache_rd_valid_f;
   assign dec_tlu_ic_diag_pkt.icache_wr_valid = icache_wr_valid_f;
   assign wr_mtsel_wb = dec_csr_wen_wb_mod & (dec_csr_wraddr_wb[11:0] == 12'h7a0);
   assign mtsel_ns[1:0] = wr_mtsel_wb ? {dec_csr_wrdata_wb[1:0]} : mtsel[1:0];
   rvdff #(2)  mtsel_ff (.*, .clk(csr_wr_clk), .din(mtsel_ns[1:0]), .dout(mtsel[1:0]));
   assign tdata_load = dec_csr_wrdata_wb[0] & ~dec_csr_wrdata_wb[19];
   assign tdata_opcode = dec_csr_wrdata_wb[2] & ~dec_csr_wrdata_wb[19];
   assign tdata_action = (dec_csr_wrdata_wb[27] & dbg_tlu_halted_f) & dec_csr_wrdata_wb[12];
   assign tdata_wrdata_wb[9:0]  = {dec_csr_wrdata_wb[27] & dbg_tlu_halted_f,
                                   dec_csr_wrdata_wb[20:19],
                                   tdata_action,
                                   dec_csr_wrdata_wb[11],
                                   dec_csr_wrdata_wb[7:6],
                                   tdata_opcode,
                                   dec_csr_wrdata_wb[1],
                                   tdata_load};
   assign wr_mtdata1_t0_wb = dec_csr_wen_wb_mod & (dec_csr_wraddr_wb[11:0] == 12'h7a1) & (mtsel[1:0] == 2'b0) & (~mtdata1_t0[9] | dbg_tlu_halted_f);
   assign mtdata1_t0_ns[9:0] = wr_mtdata1_t0_wb ? tdata_wrdata_wb[9:0] :
                                {mtdata1_t0[9], update_hit_bit_wb[0] | mtdata1_t0[8], mtdata1_t0[7:0]};
   assign wr_mtdata1_t1_wb = dec_csr_wen_wb_mod & (dec_csr_wraddr_wb[11:0] == 12'h7a1) & (mtsel[1:0] == 2'b01) & (~mtdata1_t1[9] | dbg_tlu_halted_f);
   assign mtdata1_t1_ns[9:0] = wr_mtdata1_t1_wb ? tdata_wrdata_wb[9:0] :
                                {mtdata1_t1[9], update_hit_bit_wb[1] | mtdata1_t1[8], mtdata1_t1[7:0]};
   assign wr_mtdata1_t2_wb = dec_csr_wen_wb_mod & (dec_csr_wraddr_wb[11:0] == 12'h7a1) & (mtsel[1:0] == 2'b10) & (~mtdata1_t2[9] | dbg_tlu_halted_f);
   assign mtdata1_t2_ns[9:0] = wr_mtdata1_t2_wb ? tdata_wrdata_wb[9:0] :
                                {mtdata1_t2[9], update_hit_bit_wb[2] | mtdata1_t2[8], mtdata1_t2[7:0]};
   assign wr_mtdata1_t3_wb = dec_csr_wen_wb_mod & (dec_csr_wraddr_wb[11:0] == 12'h7a1) & (mtsel[1:0] == 2'b11) & (~mtdata1_t3[9] | dbg_tlu_halted_f);
   assign mtdata1_t3_ns[9:0] = wr_mtdata1_t3_wb ? tdata_wrdata_wb[9:0] :
                                {mtdata1_t3[9], update_hit_bit_wb[3] | mtdata1_t3[8], mtdata1_t3[7:0]};
   rvdff #(10)  mtdata1_t0_ff (.*, .clk(active_clk), .din(mtdata1_t0_ns[9:0]), .dout(mtdata1_t0[9:0]));
   rvdff #(10)  mtdata1_t1_ff (.*, .clk(active_clk), .din(mtdata1_t1_ns[9:0]), .dout(mtdata1_t1[9:0]));
   rvdff #(10)  mtdata1_t2_ff (.*, .clk(active_clk), .din(mtdata1_t2_ns[9:0]), .dout(mtdata1_t2[9:0]));
   rvdff #(10)  mtdata1_t3_ff (.*, .clk(active_clk), .din(mtdata1_t3_ns[9:0]), .dout(mtdata1_t3[9:0]));
   assign mtdata1_tsel_out[31:0] = ( ({32{(mtsel[1:0] == 2'b00)}} & {4'h2, mtdata1_t0[9], 6'b011111, mtdata1_t0[8:7], 6'b0, mtdata1_t0[6:5], 3'b0, mtdata1_t0[4:3], 3'b0, mtdata1_t0[2:0]}) |
                                     ({32{(mtsel[1:0] == 2'b01)}} & {4'h2, mtdata1_t1[9], 6'b011111, mtdata1_t1[8:7], 6'b0, mtdata1_t1[6:5], 3'b0, mtdata1_t1[4:3], 3'b0, mtdata1_t1[2:0]}) |
                                     ({32{(mtsel[1:0] == 2'b10)}} & {4'h2, mtdata1_t2[9], 6'b011111, mtdata1_t2[8:7], 6'b0, mtdata1_t2[6:5], 3'b0, mtdata1_t2[4:3], 3'b0, mtdata1_t2[2:0]}) |
                                     ({32{(mtsel[1:0] == 2'b11)}} & {4'h2, mtdata1_t3[9], 6'b011111, mtdata1_t3[8:7], 6'b0, mtdata1_t3[6:5], 3'b0, mtdata1_t3[4:3], 3'b0, mtdata1_t3[2:0]}));
   assign trigger_pkt_any[0].select = mtdata1_t0[7];
   assign trigger_pkt_any[0].match = mtdata1_t0[4];
   assign trigger_pkt_any[0].store = mtdata1_t0[1];
   assign trigger_pkt_any[0].load = mtdata1_t0[0];
   assign trigger_pkt_any[0].execute = mtdata1_t0[2];
   assign trigger_pkt_any[0].m = mtdata1_t0[3];
   assign trigger_pkt_any[1].select = mtdata1_t1[7];
   assign trigger_pkt_any[1].match = mtdata1_t1[4];
   assign trigger_pkt_any[1].store = mtdata1_t1[1];
   assign trigger_pkt_any[1].load = mtdata1_t1[0];
   assign trigger_pkt_any[1].execute = mtdata1_t1[2];
   assign trigger_pkt_any[1].m = mtdata1_t1[3];
   assign trigger_pkt_any[2].select = mtdata1_t2[7];
   assign trigger_pkt_any[2].match = mtdata1_t2[4];
   assign trigger_pkt_any[2].store = mtdata1_t2[1];
   assign trigger_pkt_any[2].load = mtdata1_t2[0];
   assign trigger_pkt_any[2].execute = mtdata1_t2[2];
   assign trigger_pkt_any[2].m = mtdata1_t2[3];
   assign trigger_pkt_any[3].select = mtdata1_t3[7];
   assign trigger_pkt_any[3].match = mtdata1_t3[4];
   assign trigger_pkt_any[3].store = mtdata1_t3[1];
   assign trigger_pkt_any[3].load = mtdata1_t3[0];
   assign trigger_pkt_any[3].execute = mtdata1_t3[2];
   assign trigger_pkt_any[3].m = mtdata1_t3[3];
   assign wr_mtdata2_t0_wb = dec_csr_wen_wb_mod & (dec_csr_wraddr_wb[11:0] == 12'h7a2) & (mtsel[1:0] == 2'b0)  & (~mtdata1_t0[9] | dbg_tlu_halted_f);
   assign wr_mtdata2_t1_wb = dec_csr_wen_wb_mod & (dec_csr_wraddr_wb[11:0] == 12'h7a2) & (mtsel[1:0] == 2'b01) & (~mtdata1_t1[9] | dbg_tlu_halted_f);
   assign wr_mtdata2_t2_wb = dec_csr_wen_wb_mod & (dec_csr_wraddr_wb[11:0] == 12'h7a2) & (mtsel[1:0] == 2'b10) & (~mtdata1_t2[9] | dbg_tlu_halted_f);
   assign wr_mtdata2_t3_wb = dec_csr_wen_wb_mod & (dec_csr_wraddr_wb[11:0] == 12'h7a2) & (mtsel[1:0] == 2'b11) & (~mtdata1_t3[9] | dbg_tlu_halted_f);
   rvdffe #(32)  mtdata2_t0_ff (.*, .en(wr_mtdata2_t0_wb), .din(dec_csr_wrdata_wb[31:0]), .dout(mtdata2_t0[31:0]));
   rvdffe #(32)  mtdata2_t1_ff (.*, .en(wr_mtdata2_t1_wb), .din(dec_csr_wrdata_wb[31:0]), .dout(mtdata2_t1[31:0]));
   rvdffe #(32)  mtdata2_t2_ff (.*, .en(wr_mtdata2_t2_wb), .din(dec_csr_wrdata_wb[31:0]), .dout(mtdata2_t2[31:0]));
   rvdffe #(32)  mtdata2_t3_ff (.*, .en(wr_mtdata2_t3_wb), .din(dec_csr_wrdata_wb[31:0]), .dout(mtdata2_t3[31:0]));
   assign mtdata2_tsel_out[31:0] = ( ({32{(mtsel[1:0] == 2'b00)}} & mtdata2_t0[31:0]) |
                                     ({32{(mtsel[1:0] == 2'b01)}} & mtdata2_t1[31:0]) |
                                     ({32{(mtsel[1:0] == 2'b10)}} & mtdata2_t2[31:0]) |
                                     ({32{(mtsel[1:0] == 2'b11)}} & mtdata2_t3[31:0]));
   assign trigger_pkt_any[0].tdata2[31:0] = mtdata2_t0[31:0];
   assign trigger_pkt_any[1].tdata2[31:0] = mtdata2_t1[31:0];
   assign trigger_pkt_any[2].tdata2[31:0] = mtdata2_t2[31:0];
   assign trigger_pkt_any[3].tdata2[31:0] = mtdata2_t3[31:0];
   logic [3:0][1:0] mhpmc_inc_e4, mhpmc_inc_wb;
   logic [3:0][5:0] mhpme_vec;
   logic            mhpmc3_wr_en0, mhpmc3_wr_en1, mhpmc3_wr_en;
   logic            mhpmc4_wr_en0, mhpmc4_wr_en1, mhpmc4_wr_en;
   logic            mhpmc5_wr_en0, mhpmc5_wr_en1, mhpmc5_wr_en;
   logic            mhpmc6_wr_en0, mhpmc6_wr_en1, mhpmc6_wr_en;
   logic            mhpmc3h_wr_en0, mhpmc3h_wr_en;
   logic            mhpmc4h_wr_en0, mhpmc4h_wr_en;
   logic            mhpmc5h_wr_en0, mhpmc5h_wr_en;
   logic            mhpmc6h_wr_en0, mhpmc6h_wr_en;
   logic [63:0]     mhpmc3_incr, mhpmc4_incr, mhpmc5_incr, mhpmc6_incr;
   assign mhpme_vec[0][5:0] = mhpme3[5:0];
   assign mhpme_vec[1][5:0] = mhpme4[5:0];
   assign mhpme_vec[2][5:0] = mhpme5[5:0];
   assign mhpme_vec[3][5:0] = mhpme6[5:0];
   logic [3:0] pmu_i0_itype_qual, pmu_i1_itype_qual;
   assign pmu_i0_itype_qual[3:0] = dec_tlu_packet_e4.pmu_i0_itype[3:0] & {4{tlu_i0_commit_cmt}};
   assign pmu_i1_itype_qual[3:0] = dec_tlu_packet_e4.pmu_i1_itype[3:0] & {4{tlu_i1_commit_cmt}};
   for (genvar i=0 ; i < 4; i++) begin
      assign mhpmc_inc_e4[i][1:0] =  {2{mgpmc}} &
           (
             ({2{(mhpme_vec[i][5:0] == 6'd1      )}} & 2'b01) |
             ({2{(mhpme_vec[i][5:0] == 6'd2      )}} & {1'b0, ifu_pmu_ic_hit}) |
             ({2{(mhpme_vec[i][5:0] == 6'd3     )}} & {1'b0, ifu_pmu_ic_miss}) |
             ({2{(mhpme_vec[i][5:0] == 6'd4     )}} & {tlu_i1_commit_cmt, tlu_i0_commit_cmt & ~illegal_e4}) |
             ({2{(mhpme_vec[i][5:0] == 6'd5 )}} & {tlu_i1_commit_cmt & ~exu_pmu_i1_pc4, tlu_i0_commit_cmt & ~exu_pmu_i0_pc4 & ~illegal_e4}) |
             ({2{(mhpme_vec[i][5:0] == 6'd6 )}} & {tlu_i1_commit_cmt &  exu_pmu_i1_pc4, tlu_i0_commit_cmt &  exu_pmu_i0_pc4 & ~illegal_e4}) |
             ({2{(mhpme_vec[i][5:0] == 6'd7    )}} & ifu_pmu_instr_aligned[1:0])  |
             ({2{(mhpme_vec[i][5:0] == 6'd8    )}} & dec_pmu_instr_decoded[1:0])  |
             ({2{(mhpme_vec[i][5:0] == 6'd29     )}} & {1'b0,ifu_pmu_align_stall})  |
             ({2{(mhpme_vec[i][5:0] == 6'd30    )}} & {1'b0,dec_pmu_decode_stall}) |
             ({2{(mhpme_vec[i][5:0] == 6'd9        )}} & {(pmu_i1_itype_qual[3:0] == MUL),     (pmu_i0_itype_qual[3:0] == MUL)})     |
             ({2{(mhpme_vec[i][5:0] == 6'd10        )}} & {1'b0, dec_tlu_packet_e4.pmu_divide & tlu_i0_commit_cmt})     |
             ({2{(mhpme_vec[i][5:0] == 6'd11       )}} & {(pmu_i1_itype_qual[3:0] == LOAD),    (pmu_i0_itype_qual[3:0] == LOAD)})    |
             ({2{(mhpme_vec[i][5:0] == 6'd12      )}} & {(pmu_i1_itype_qual[3:0] == STORE),   (pmu_i0_itype_qual[3:0] == STORE)})   |
             ({2{(mhpme_vec[i][5:0] == 6'd13     )}} & {(pmu_i1_itype_qual[3:0] == LOAD),    (pmu_i0_itype_qual[3:0] == LOAD)} &
                                                                      {2{dec_tlu_packet_e4.pmu_lsu_misaligned}})    |
             ({2{(mhpme_vec[i][5:0] == 6'd14    )}} & {(pmu_i1_itype_qual[3:0] == STORE),   (pmu_i0_itype_qual[3:0] == STORE)} &
                                                                      {2{dec_tlu_packet_e4.pmu_lsu_misaligned}})    |
             ({2{(mhpme_vec[i][5:0] == 6'd15        )}} & {(pmu_i1_itype_qual[3:0] == ALU),     (pmu_i0_itype_qual[3:0] == ALU)})     |
             ({2{(mhpme_vec[i][5:0] == 6'd16    )}} & {1'b0, (pmu_i0_itype_qual[3:0] == CSRREAD)}) |
             ({2{(mhpme_vec[i][5:0] == 6'd18   )}} & {1'b0, (pmu_i0_itype_qual[3:0] == CSRWRITE)})|
             ({2{(mhpme_vec[i][5:0] == 6'd17      )}} & {1'b0, (pmu_i0_itype_qual[3:0] == CSRRW)})   |
             ({2{(mhpme_vec[i][5:0] == 6'd19     )}} & {1'b0, (pmu_i0_itype_qual[3:0] == EBREAK)})  |
             ({2{(mhpme_vec[i][5:0] == 6'd20      )}} & {1'b0, (pmu_i0_itype_qual[3:0] == ECALL)})   |
             ({2{(mhpme_vec[i][5:0] == 6'd21      )}} & {1'b0, (pmu_i0_itype_qual[3:0] == FENCE)})   |
             ({2{(mhpme_vec[i][5:0] == 6'd22     )}} & {1'b0, (pmu_i0_itype_qual[3:0] == FENCEI)})  |
             ({2{(mhpme_vec[i][5:0] == 6'd23       )}} & {1'b0, (pmu_i0_itype_qual[3:0] == MRET)})    |
             ({2{(mhpme_vec[i][5:0] == 6'd24     )}} & {((pmu_i1_itype_qual[3:0] == CONDBR) | (pmu_i1_itype_qual[3:0] == JAL)),
                                                                     ((pmu_i0_itype_qual[3:0] == CONDBR) | (pmu_i0_itype_qual[3:0] == JAL))})   |
             ({2{(mhpme_vec[i][5:0] == 6'd25       )}} & {exu_pmu_i1_br_misp & tlu_i1_commit_cmt, exu_pmu_i0_br_misp & tlu_i0_commit_cmt}) |
             ({2{(mhpme_vec[i][5:0] == 6'd26    )}} & {exu_pmu_i1_br_ataken & tlu_i1_commit_cmt, exu_pmu_i0_br_ataken & tlu_i0_commit_cmt}) |
             ({2{(mhpme_vec[i][5:0] == 6'd27     )}} & {dec_tlu_packet_e4.pmu_i1_br_unpred & tlu_i1_commit_cmt, dec_tlu_packet_e4.pmu_i0_br_unpred & tlu_i0_commit_cmt}) |
             ({2{(mhpme_vec[i][5:0] == 6'd28     )}} & {1'b0, ifu_pmu_fetch_stall}) |
             ({2{(mhpme_vec[i][5:0] == 6'd29     )}} & {1'b0, ifu_pmu_align_stall}) |
             ({2{(mhpme_vec[i][5:0] == 6'd30    )}} & {1'b0, dec_pmu_decode_stall}) |
             ({2{(mhpme_vec[i][5:0] == 6'd31  )}} & {1'b0,dec_pmu_postsync_stall}) |
             ({2{(mhpme_vec[i][5:0] == 6'd32   )}} & {1'b0,dec_pmu_presync_stall}) |
             ({2{(mhpme_vec[i][5:0] == 6'd33      )}} & {1'b0, lsu_freeze_dc3}) |
             ({2{(mhpme_vec[i][5:0] == 6'd34 )}} & {1'b0, lsu_store_stall_any}) |
             ({2{(mhpme_vec[i][5:0] == 6'd35  )}} & {1'b0, dma_dccm_stall_any}) |
             ({2{(mhpme_vec[i][5:0] == 6'd36  )}} & {1'b0, dma_iccm_stall_any}) |
             ({2{(mhpme_vec[i][5:0] == 6'd37       )}} & {1'b0, (i0_exception_valid_e4 | trigger_hit_e4 | lsu_exc_valid_e4)}) |
             ({2{(mhpme_vec[i][5:0] == 6'd38 )}} & {1'b0, take_timer_int | take_int_timer0_int | take_int_timer1_int}) |
             ({2{(mhpme_vec[i][5:0] == 6'd39   )}} & {1'b0, take_ext_int}) |
             ({2{(mhpme_vec[i][5:0] == 6'd40     )}} & {1'b0, tlu_flush_lower_e4}) |
             ({2{(mhpme_vec[i][5:0] == 6'd41        )}} & {(dec_tlu_br1_error_e4 | dec_tlu_br1_start_error_e4) & rfpc_i1_e4, (dec_tlu_br0_error_e4 | dec_tlu_br0_start_error_e4) & rfpc_i0_e4}) |
             ({2{(mhpme_vec[i][5:0] == 6'd42      )}} & {1'b0, ifu_pmu_bus_trxn}) |
             ({2{(mhpme_vec[i][5:0] == 6'd43      )}} & {1'b0, lsu_pmu_bus_trxn}) |
             ({2{(mhpme_vec[i][5:0] == 6'd44   )}} & {1'b0, lsu_pmu_bus_misaligned}) |
             ({2{(mhpme_vec[i][5:0] == 6'd45      )}} & {1'b0, ifu_pmu_bus_error}) |
             ({2{(mhpme_vec[i][5:0] == 6'd46      )}} & {1'b0, lsu_pmu_bus_error}) |
             ({2{(mhpme_vec[i][5:0] == 6'd47      )}} & {1'b0, ifu_pmu_bus_busy}) |
             ({2{(mhpme_vec[i][5:0] == 6'd48      )}} & {1'b0, lsu_pmu_bus_busy}) |
             ({2{(mhpme_vec[i][5:0] == 6'd49    )}} & {1'b0, ~mstatus[0]}) |
             ({2{(mhpme_vec[i][5:0] == 6'd50     )}} & {1'b0, ~mstatus[0] & |(mip[5:0] & mie[5:0])})
             );
   end
   rvdff #(2) pmu0inc_ff (.*, .clk(free_clk), .din(mhpmc_inc_e4[0][1:0]), .dout(mhpmc_inc_wb[0][1:0]));
   rvdff #(2) pmu1inc_ff (.*, .clk(free_clk), .din(mhpmc_inc_e4[1][1:0]), .dout(mhpmc_inc_wb[1][1:0]));
   rvdff #(2) pmu2inc_ff (.*, .clk(free_clk), .din(mhpmc_inc_e4[2][1:0]), .dout(mhpmc_inc_wb[2][1:0]));
   rvdff #(2) pmu3inc_ff (.*, .clk(free_clk), .din(mhpmc_inc_e4[3][1:0]), .dout(mhpmc_inc_wb[3][1:0]));
   assign perfcnt_halted = ((dec_tlu_dbg_halted & dcsr[10]) | dec_tlu_pmu_fw_halted);
   assign dec_tlu_perfcnt0[1:0] = mhpmc_inc_wb[0][1:0] & ~{2{perfcnt_halted}};
   assign dec_tlu_perfcnt1[1:0] = mhpmc_inc_wb[1][1:0] & ~{2{perfcnt_halted}};
   assign dec_tlu_perfcnt2[1:0] = mhpmc_inc_wb[2][1:0] & ~{2{perfcnt_halted}};
   assign dec_tlu_perfcnt3[1:0] = mhpmc_inc_wb[3][1:0] & ~{2{perfcnt_halted}};
   assign mhpmc3_wr_en0 = dec_csr_wen_wb_mod & (dec_csr_wraddr_wb[11:0] == 12'hB03);
   assign mhpmc3_wr_en1 = ~perfcnt_halted & (|(mhpmc_inc_wb[0][1:0]));
   assign mhpmc3_wr_en  = mhpmc3_wr_en0 | mhpmc3_wr_en1;
   assign mhpmc3_incr[63:0] = {mhpmc3h[31:0],mhpmc3[31:0]} + {63'b0,mhpmc_inc_wb[0][1]} + {63'b0,mhpmc_inc_wb[0][0]};
   assign mhpmc3_ns[31:0] = mhpmc3_wr_en0 ? dec_csr_wrdata_wb[31:0] : mhpmc3_incr[31:0];
   rvdffe #(32)  mhpmc3_ff (.*, .en(mhpmc3_wr_en), .din(mhpmc3_ns[31:0]), .dout(mhpmc3[31:0]));
   assign mhpmc3h_wr_en0 = dec_csr_wen_wb_mod & (dec_csr_wraddr_wb[11:0] == 12'hB83);
   assign mhpmc3h_wr_en  = mhpmc3h_wr_en0 | mhpmc3_wr_en1;
   assign mhpmc3h_ns[31:0] = mhpmc3h_wr_en0 ? dec_csr_wrdata_wb[31:0] : mhpmc3_incr[63:32];
   rvdffe #(32)  mhpmc3h_ff (.*, .en(mhpmc3h_wr_en), .din(mhpmc3h_ns[31:0]), .dout(mhpmc3h[31:0]));
   assign mhpmc4_wr_en0 = dec_csr_wen_wb_mod & (dec_csr_wraddr_wb[11:0] == 12'hB04);
   assign mhpmc4_wr_en1 = ~perfcnt_halted & (|(mhpmc_inc_wb[1][1:0]));
   assign mhpmc4_wr_en  = mhpmc4_wr_en0 | mhpmc4_wr_en1;
   assign mhpmc4_incr[63:0] = {mhpmc4h[31:0],mhpmc4[31:0]} + {63'b0,mhpmc_inc_wb[1][1]} + {63'b0,mhpmc_inc_wb[1][0]};
   assign mhpmc4_ns[31:0] = mhpmc4_wr_en0 ? dec_csr_wrdata_wb[31:0] : mhpmc4_incr[31:0];
   rvdffe #(32)  mhpmc4_ff (.*, .en(mhpmc4_wr_en), .din(mhpmc4_ns[31:0]), .dout(mhpmc4[31:0]));
   assign mhpmc4h_wr_en0 = dec_csr_wen_wb_mod & (dec_csr_wraddr_wb[11:0] == 12'hB84);
   assign mhpmc4h_wr_en  = mhpmc4h_wr_en0 | mhpmc4_wr_en1;
   assign mhpmc4h_ns[31:0] = mhpmc4h_wr_en0 ? dec_csr_wrdata_wb[31:0] : mhpmc4_incr[63:32];
   rvdffe #(32)  mhpmc4h_ff (.*, .en(mhpmc4h_wr_en), .din(mhpmc4h_ns[31:0]), .dout(mhpmc4h[31:0]));
   assign mhpmc5_wr_en0 = dec_csr_wen_wb_mod & (dec_csr_wraddr_wb[11:0] == 12'hB05);
   assign mhpmc5_wr_en1 = ~perfcnt_halted & (|(mhpmc_inc_wb[2][1:0]));
   assign mhpmc5_wr_en  = mhpmc5_wr_en0 | mhpmc5_wr_en1;
   assign mhpmc5_incr[63:0] = {mhpmc5h[31:0],mhpmc5[31:0]} + {63'b0,mhpmc_inc_wb[2][1]} + {63'b0,mhpmc_inc_wb[2][0]};
   assign mhpmc5_ns[31:0] = mhpmc5_wr_en0 ? dec_csr_wrdata_wb[31:0] : mhpmc5_incr[31:0];
   rvdffe #(32)  mhpmc5_ff (.*, .en(mhpmc5_wr_en), .din(mhpmc5_ns[31:0]), .dout(mhpmc5[31:0]));
   assign mhpmc5h_wr_en0 = dec_csr_wen_wb_mod & (dec_csr_wraddr_wb[11:0] == 12'hB85);
   assign mhpmc5h_wr_en  = mhpmc5h_wr_en0 | mhpmc5_wr_en1;
   assign mhpmc5h_ns[31:0] = mhpmc5h_wr_en0 ? dec_csr_wrdata_wb[31:0] : mhpmc5_incr[63:32];
   rvdffe #(32)  mhpmc5h_ff (.*, .en(mhpmc5h_wr_en), .din(mhpmc5h_ns[31:0]), .dout(mhpmc5h[31:0]));
   assign mhpmc6_wr_en0 = dec_csr_wen_wb_mod & (dec_csr_wraddr_wb[11:0] == 12'hB06);
   assign mhpmc6_wr_en1 = ~perfcnt_halted & (|(mhpmc_inc_wb[3][1:0]));
   assign mhpmc6_wr_en  = mhpmc6_wr_en0 | mhpmc6_wr_en1;
   assign mhpmc6_incr[63:0] = {mhpmc6h[31:0],mhpmc6[31:0]} + {63'b0,mhpmc_inc_wb[3][1]} + {63'b0,mhpmc_inc_wb[3][0]};
   assign mhpmc6_ns[31:0] = mhpmc6_wr_en0 ? dec_csr_wrdata_wb[31:0] : mhpmc6_incr[31:0];
   rvdffe #(32)  mhpmc6_ff (.*, .en(mhpmc6_wr_en), .din(mhpmc6_ns[31:0]), .dout(mhpmc6[31:0]));
   assign mhpmc6h_wr_en0 = dec_csr_wen_wb_mod & (dec_csr_wraddr_wb[11:0] == 12'hB86);
   assign mhpmc6h_wr_en  = mhpmc6h_wr_en0 | mhpmc6_wr_en1;
   assign mhpmc6h_ns[31:0] = mhpmc6h_wr_en0 ? dec_csr_wrdata_wb[31:0] : mhpmc6_incr[63:32];
   rvdffe #(32)  mhpmc6h_ff (.*, .en(mhpmc6h_wr_en), .din(mhpmc6h_ns[31:0]), .dout(mhpmc6h[31:0]));
   logic [5:0] event_saturate_wb;
   assign event_saturate_wb[5:0] = ((dec_csr_wrdata_wb[5:0] > 6'd50) | (|dec_csr_wrdata_wb[31:6])) ? 6'd50 : dec_csr_wrdata_wb[5:0];
   assign wr_mhpme3_wb = dec_csr_wen_wb_mod & (dec_csr_wraddr_wb[11:0] == 12'h323);
   rvdffs #(6)  mhpme3_ff (.*, .clk(active_clk), .en(wr_mhpme3_wb), .din(event_saturate_wb[5:0]), .dout(mhpme3[5:0]));
   assign wr_mhpme4_wb = dec_csr_wen_wb_mod & (dec_csr_wraddr_wb[11:0] == 12'h324);
   rvdffs #(6)  mhpme4_ff (.*, .clk(active_clk), .en(wr_mhpme4_wb), .din(event_saturate_wb[5:0]), .dout(mhpme4[5:0]));
   assign wr_mhpme5_wb = dec_csr_wen_wb_mod & (dec_csr_wraddr_wb[11:0] == 12'h325);
   rvdffs #(6)  mhpme5_ff (.*, .clk(active_clk), .en(wr_mhpme5_wb), .din(event_saturate_wb[5:0]), .dout(mhpme5[5:0]));
   assign wr_mhpme6_wb = dec_csr_wen_wb_mod & (dec_csr_wraddr_wb[11:0] == 12'h326);
   rvdffs #(6)  mhpme6_ff (.*, .clk(active_clk), .en(wr_mhpme6_wb), .din(event_saturate_wb[5:0]), .dout(mhpme6[5:0]));
   assign wr_mgpmc_wb = dec_csr_wen_wb_mod & (dec_csr_wraddr_wb[11:0] == 12'h7d0);
   rvdffs #(1)  mgpmc_ff (.*, .clk(active_clk), .en(wr_mgpmc_wb), .din(~dec_csr_wrdata_wb[0]), .dout(mgpmc_b));
   assign mgpmc = ~mgpmc_b;
   logic usoc_tclk;
   rvoclkhdr usoctrace_cgc ( .en(i0_valid_wb | exc_or_int_valid_wb | interrupt_valid_wb | dec_tlu_i0_valid_wb1 |
                                dec_tlu_i0_exc_valid_wb1 | dec_tlu_i1_exc_valid_wb1 | dec_tlu_int_valid_wb1 | clk_override), .l1clk(usoc_tclk), .* );
   rvdff #(10)  traceff (.*,   .clk(usoc_tclk),
                        .din ({i0_valid_wb, i1_valid_wb,
                               i0_exception_valid_wb | lsu_i0_exc_wb | (i0_trigger_hit_wb & ~trigger_hit_dmode_wb),
                               ~(i0_exception_valid_wb | lsu_i0_exc_wb | i0_trigger_hit_wb) & exc_or_int_valid_wb & ~interrupt_valid_wb,
                               exc_cause_wb[4:0],
                               interrupt_valid_wb}),
                        .dout({dec_tlu_i0_valid_wb1, dec_tlu_i1_valid_wb1,
                               dec_tlu_i0_exc_valid_wb1, dec_tlu_i1_exc_valid_wb1,
                               dec_tlu_exc_cause_wb1[4:0],
                               dec_tlu_int_valid_wb1}));
   assign dec_tlu_mtval_wb1  = mtval[31:0];
logic csr_misa;
logic csr_mvendorid;
logic csr_marchid;
logic csr_mimpid;
logic csr_mhartid;
logic csr_mstatus;
logic csr_mtvec;
logic csr_mip;
logic csr_mie;
logic csr_mcyclel;
logic csr_mcycleh;
logic csr_minstretl;
logic csr_minstreth;
logic csr_mscratch;
logic csr_mepc;
logic csr_mcause;
logic csr_mtval;
logic csr_mrac;
logic csr_dmst;
logic csr_mdseac;
logic csr_meihap;
logic csr_meivt;
logic csr_meipt;
logic csr_meicurpl;
logic csr_meicidpl;
logic csr_dcsr;
logic csr_mpmc;
logic csr_mcgc;
logic csr_mcpc;
logic csr_mfdc;
logic csr_dpc;
logic csr_mtsel;
logic csr_mtdata1;
logic csr_mtdata2;
logic csr_mhpmc3;
logic csr_mhpmc4;
logic csr_mhpmc5;
logic csr_mhpmc6;
logic csr_mhpmc3h;
logic csr_mhpmc4h;
logic csr_mhpmc5h;
logic csr_mhpmc6h;
logic csr_mhpme3;
logic csr_mhpme4;
logic csr_mhpme5;
logic csr_mhpme6;
logic csr_mgpmc;
logic csr_micect;
logic csr_miccmect;
logic csr_mdccmect;
logic csr_dicawics;
logic csr_dicad0;
logic csr_dicad1;
logic csr_dicago;
logic presync;
logic postsync;
assign csr_misa = (!dec_csr_rdaddr_d[11]&!dec_csr_rdaddr_d[6]
    &!dec_csr_rdaddr_d[5]&!dec_csr_rdaddr_d[2]&dec_csr_rdaddr_d[0]);
assign csr_mvendorid = (dec_csr_rdaddr_d[10]&!dec_csr_rdaddr_d[7]
    &!dec_csr_rdaddr_d[1]&dec_csr_rdaddr_d[0]);
assign csr_marchid = (dec_csr_rdaddr_d[10]&!dec_csr_rdaddr_d[7]
    &dec_csr_rdaddr_d[1]&!dec_csr_rdaddr_d[0]);
assign csr_mimpid = (dec_csr_rdaddr_d[10]&!dec_csr_rdaddr_d[6]
    &dec_csr_rdaddr_d[1]&dec_csr_rdaddr_d[0]);
assign csr_mhartid = (dec_csr_rdaddr_d[10]&!dec_csr_rdaddr_d[7]
    &dec_csr_rdaddr_d[2]);
assign csr_mstatus = (!dec_csr_rdaddr_d[11]&!dec_csr_rdaddr_d[6]
    &!dec_csr_rdaddr_d[5]&!dec_csr_rdaddr_d[2]&!dec_csr_rdaddr_d[0]);
assign csr_mtvec = (!dec_csr_rdaddr_d[11]&!dec_csr_rdaddr_d[5]
    &!dec_csr_rdaddr_d[4]&dec_csr_rdaddr_d[2]&dec_csr_rdaddr_d[0]);
assign csr_mip = (!dec_csr_rdaddr_d[7]&dec_csr_rdaddr_d[6]&dec_csr_rdaddr_d[2]);
assign csr_mie = (!dec_csr_rdaddr_d[11]&!dec_csr_rdaddr_d[6]&!dec_csr_rdaddr_d[5]
    &dec_csr_rdaddr_d[2]&!dec_csr_rdaddr_d[0]);
assign csr_mcyclel = (dec_csr_rdaddr_d[11]&!dec_csr_rdaddr_d[7]
    &!dec_csr_rdaddr_d[4]&!dec_csr_rdaddr_d[3]&!dec_csr_rdaddr_d[2]
    &!dec_csr_rdaddr_d[1]);
assign csr_mcycleh = (dec_csr_rdaddr_d[7]&!dec_csr_rdaddr_d[6]
    &!dec_csr_rdaddr_d[5]&!dec_csr_rdaddr_d[4]&!dec_csr_rdaddr_d[3]
    &!dec_csr_rdaddr_d[2]&!dec_csr_rdaddr_d[1]);
assign csr_minstretl = (!dec_csr_rdaddr_d[7]&!dec_csr_rdaddr_d[6]
    &!dec_csr_rdaddr_d[4]&!dec_csr_rdaddr_d[3]&!dec_csr_rdaddr_d[2]
    &dec_csr_rdaddr_d[1]&!dec_csr_rdaddr_d[0]);
assign csr_minstreth = (!dec_csr_rdaddr_d[10]&dec_csr_rdaddr_d[7]
    &!dec_csr_rdaddr_d[4]&!dec_csr_rdaddr_d[3]&!dec_csr_rdaddr_d[2]
    &dec_csr_rdaddr_d[1]&!dec_csr_rdaddr_d[0]);
assign csr_mscratch = (!dec_csr_rdaddr_d[7]&dec_csr_rdaddr_d[6]
    &!dec_csr_rdaddr_d[2]&!dec_csr_rdaddr_d[1]&!dec_csr_rdaddr_d[0]);
assign csr_mepc = (!dec_csr_rdaddr_d[7]&dec_csr_rdaddr_d[6]&!dec_csr_rdaddr_d[1]
    &dec_csr_rdaddr_d[0]);
assign csr_mcause = (!dec_csr_rdaddr_d[7]&dec_csr_rdaddr_d[6]
    &dec_csr_rdaddr_d[1]&!dec_csr_rdaddr_d[0]);
assign csr_mtval = (!dec_csr_rdaddr_d[7]&dec_csr_rdaddr_d[6]&dec_csr_rdaddr_d[1]
    &dec_csr_rdaddr_d[0]);
assign csr_mrac = (!dec_csr_rdaddr_d[11]&dec_csr_rdaddr_d[7]&dec_csr_rdaddr_d[6]
    &!dec_csr_rdaddr_d[4]&!dec_csr_rdaddr_d[3]&!dec_csr_rdaddr_d[2]
    &!dec_csr_rdaddr_d[1]);
assign csr_dmst = (dec_csr_rdaddr_d[10]&!dec_csr_rdaddr_d[4]&dec_csr_rdaddr_d[2]
    &!dec_csr_rdaddr_d[1]);
assign csr_mdseac = (dec_csr_rdaddr_d[11]&dec_csr_rdaddr_d[10]
    &!dec_csr_rdaddr_d[4]&!dec_csr_rdaddr_d[3]);
assign csr_meihap = (dec_csr_rdaddr_d[11]&dec_csr_rdaddr_d[10]
    &dec_csr_rdaddr_d[3]);
assign csr_meivt = (!dec_csr_rdaddr_d[10]&dec_csr_rdaddr_d[6]
    &dec_csr_rdaddr_d[3]&!dec_csr_rdaddr_d[2]&!dec_csr_rdaddr_d[1]
    &!dec_csr_rdaddr_d[0]);
assign csr_meipt = (dec_csr_rdaddr_d[11]&dec_csr_rdaddr_d[6]&!dec_csr_rdaddr_d[1]
    &dec_csr_rdaddr_d[0]);
assign csr_meicurpl = (dec_csr_rdaddr_d[6]&dec_csr_rdaddr_d[3]
    &dec_csr_rdaddr_d[2]);
assign csr_meicidpl = (dec_csr_rdaddr_d[11]&dec_csr_rdaddr_d[6]
    &dec_csr_rdaddr_d[1]&dec_csr_rdaddr_d[0]);
assign csr_dcsr = (dec_csr_rdaddr_d[10]&!dec_csr_rdaddr_d[6]&dec_csr_rdaddr_d[5]
    &dec_csr_rdaddr_d[4]&!dec_csr_rdaddr_d[0]);
assign csr_mpmc = (dec_csr_rdaddr_d[6]&!dec_csr_rdaddr_d[4]&dec_csr_rdaddr_d[2]
    &dec_csr_rdaddr_d[1]);
assign csr_mcgc = (dec_csr_rdaddr_d[10]&dec_csr_rdaddr_d[4]&dec_csr_rdaddr_d[3]
    &!dec_csr_rdaddr_d[0]);
assign csr_mcpc = (dec_csr_rdaddr_d[10]&dec_csr_rdaddr_d[6]&!dec_csr_rdaddr_d[4]
    &!dec_csr_rdaddr_d[3]&!dec_csr_rdaddr_d[2]&dec_csr_rdaddr_d[1]);
assign csr_mfdc = (dec_csr_rdaddr_d[10]&dec_csr_rdaddr_d[4]&dec_csr_rdaddr_d[3]
    &dec_csr_rdaddr_d[0]);
assign csr_dpc = (dec_csr_rdaddr_d[10]&!dec_csr_rdaddr_d[6]&dec_csr_rdaddr_d[5]
    &dec_csr_rdaddr_d[4]&dec_csr_rdaddr_d[0]);
assign csr_mtsel = (dec_csr_rdaddr_d[10]&dec_csr_rdaddr_d[5]&!dec_csr_rdaddr_d[4]
    &!dec_csr_rdaddr_d[1]&!dec_csr_rdaddr_d[0]);
assign csr_mtdata1 = (dec_csr_rdaddr_d[10]&!dec_csr_rdaddr_d[4]
    &!dec_csr_rdaddr_d[3]&dec_csr_rdaddr_d[0]);
assign csr_mtdata2 = (dec_csr_rdaddr_d[10]&dec_csr_rdaddr_d[5]
    &!dec_csr_rdaddr_d[4]&dec_csr_rdaddr_d[1]);
assign csr_mhpmc3 = (dec_csr_rdaddr_d[11]&!dec_csr_rdaddr_d[7]
    &!dec_csr_rdaddr_d[4]&!dec_csr_rdaddr_d[3]&!dec_csr_rdaddr_d[2]
    &dec_csr_rdaddr_d[0]);
assign csr_mhpmc4 = (dec_csr_rdaddr_d[11]&!dec_csr_rdaddr_d[7]
    &!dec_csr_rdaddr_d[4]&!dec_csr_rdaddr_d[3]&dec_csr_rdaddr_d[2]
    &!dec_csr_rdaddr_d[1]&!dec_csr_rdaddr_d[0]);
assign csr_mhpmc5 = (dec_csr_rdaddr_d[11]&!dec_csr_rdaddr_d[7]
    &!dec_csr_rdaddr_d[4]&!dec_csr_rdaddr_d[3]&!dec_csr_rdaddr_d[1]
    &dec_csr_rdaddr_d[0]);
assign csr_mhpmc6 = (!dec_csr_rdaddr_d[7]&!dec_csr_rdaddr_d[5]
    &!dec_csr_rdaddr_d[4]&!dec_csr_rdaddr_d[3]&dec_csr_rdaddr_d[2]
    &dec_csr_rdaddr_d[1]&!dec_csr_rdaddr_d[0]);
assign csr_mhpmc3h = (dec_csr_rdaddr_d[7]&!dec_csr_rdaddr_d[4]
    &!dec_csr_rdaddr_d[3]&!dec_csr_rdaddr_d[2]&dec_csr_rdaddr_d[1]
    &dec_csr_rdaddr_d[0]);
assign csr_mhpmc4h = (dec_csr_rdaddr_d[7]&!dec_csr_rdaddr_d[6]
    &!dec_csr_rdaddr_d[4]&!dec_csr_rdaddr_d[3]&dec_csr_rdaddr_d[2]
    &!dec_csr_rdaddr_d[1]&!dec_csr_rdaddr_d[0]);
assign csr_mhpmc5h = (dec_csr_rdaddr_d[7]&!dec_csr_rdaddr_d[4]
    &!dec_csr_rdaddr_d[3]&dec_csr_rdaddr_d[2]&!dec_csr_rdaddr_d[1]
    &dec_csr_rdaddr_d[0]);
assign csr_mhpmc6h = (dec_csr_rdaddr_d[7]&!dec_csr_rdaddr_d[6]
    &!dec_csr_rdaddr_d[4]&!dec_csr_rdaddr_d[3]&dec_csr_rdaddr_d[2]
    &dec_csr_rdaddr_d[1]&!dec_csr_rdaddr_d[0]);
assign csr_mhpme3 = (!dec_csr_rdaddr_d[7]&dec_csr_rdaddr_d[5]
    &!dec_csr_rdaddr_d[4]&!dec_csr_rdaddr_d[3]&!dec_csr_rdaddr_d[2]);
assign csr_mhpme4 = (dec_csr_rdaddr_d[5]&!dec_csr_rdaddr_d[4]
    &!dec_csr_rdaddr_d[3]&dec_csr_rdaddr_d[2]&!dec_csr_rdaddr_d[1]
    &!dec_csr_rdaddr_d[0]);
assign csr_mhpme5 = (dec_csr_rdaddr_d[5]&!dec_csr_rdaddr_d[4]
    &!dec_csr_rdaddr_d[3]&dec_csr_rdaddr_d[2]&!dec_csr_rdaddr_d[1]
    &dec_csr_rdaddr_d[0]);
assign csr_mhpme6 = (dec_csr_rdaddr_d[5]&!dec_csr_rdaddr_d[4]
    &!dec_csr_rdaddr_d[3]&dec_csr_rdaddr_d[2]&dec_csr_rdaddr_d[1]
    &!dec_csr_rdaddr_d[0]);
assign csr_mgpmc = (dec_csr_rdaddr_d[6]&!dec_csr_rdaddr_d[5]&dec_csr_rdaddr_d[4]
    &!dec_csr_rdaddr_d[2]&!dec_csr_rdaddr_d[1]);
assign csr_micect = (dec_csr_rdaddr_d[6]&dec_csr_rdaddr_d[5]&!dec_csr_rdaddr_d[3]
    &!dec_csr_rdaddr_d[1]&!dec_csr_rdaddr_d[0]);
assign csr_miccmect = (dec_csr_rdaddr_d[6]&dec_csr_rdaddr_d[5]
    &!dec_csr_rdaddr_d[3]&dec_csr_rdaddr_d[0]);
assign csr_mdccmect = (dec_csr_rdaddr_d[6]&dec_csr_rdaddr_d[5]
    &dec_csr_rdaddr_d[1]);
assign csr_dicawics = (!dec_csr_rdaddr_d[11]&!dec_csr_rdaddr_d[5]
    &dec_csr_rdaddr_d[3]&!dec_csr_rdaddr_d[1]&!dec_csr_rdaddr_d[0]);
assign csr_dicad0 = (dec_csr_rdaddr_d[10]&!dec_csr_rdaddr_d[4]
    &dec_csr_rdaddr_d[3]&!dec_csr_rdaddr_d[1]&dec_csr_rdaddr_d[0]);
assign csr_dicad1 = (dec_csr_rdaddr_d[10]&dec_csr_rdaddr_d[3]
    &dec_csr_rdaddr_d[1]&!dec_csr_rdaddr_d[0]);
assign csr_dicago = (dec_csr_rdaddr_d[10]&dec_csr_rdaddr_d[3]
    &dec_csr_rdaddr_d[1]&dec_csr_rdaddr_d[0]);
assign presync = (dec_csr_rdaddr_d[10]&dec_csr_rdaddr_d[4]&dec_csr_rdaddr_d[3]
    &dec_csr_rdaddr_d[0]) | (dec_csr_rdaddr_d[6]&!dec_csr_rdaddr_d[5]
    &dec_csr_rdaddr_d[4]&!dec_csr_rdaddr_d[2]&!dec_csr_rdaddr_d[1]) | (
    !dec_csr_rdaddr_d[6]&!dec_csr_rdaddr_d[5]&!dec_csr_rdaddr_d[4]
    &!dec_csr_rdaddr_d[3]&!dec_csr_rdaddr_d[2]&dec_csr_rdaddr_d[1]) | (
    dec_csr_rdaddr_d[11]&!dec_csr_rdaddr_d[4]&!dec_csr_rdaddr_d[3]
    &dec_csr_rdaddr_d[2]&!dec_csr_rdaddr_d[1]) | (dec_csr_rdaddr_d[11]
    &!dec_csr_rdaddr_d[4]&!dec_csr_rdaddr_d[3]&dec_csr_rdaddr_d[1]
    &!dec_csr_rdaddr_d[0]) | (dec_csr_rdaddr_d[7]&!dec_csr_rdaddr_d[5]
    &!dec_csr_rdaddr_d[4]&!dec_csr_rdaddr_d[3]&!dec_csr_rdaddr_d[2]
    &dec_csr_rdaddr_d[1]);
assign postsync = (dec_csr_rdaddr_d[10]&dec_csr_rdaddr_d[4]&dec_csr_rdaddr_d[3]
    &dec_csr_rdaddr_d[0]) | (!dec_csr_rdaddr_d[11]&!dec_csr_rdaddr_d[5]
    &!dec_csr_rdaddr_d[4]&dec_csr_rdaddr_d[2]&dec_csr_rdaddr_d[0]) | (
    !dec_csr_rdaddr_d[11]&!dec_csr_rdaddr_d[6]&!dec_csr_rdaddr_d[5]
    &!dec_csr_rdaddr_d[2]&!dec_csr_rdaddr_d[0]) | (!dec_csr_rdaddr_d[7]
    &dec_csr_rdaddr_d[6]&!dec_csr_rdaddr_d[1]&dec_csr_rdaddr_d[0]) | (
    dec_csr_rdaddr_d[10]&!dec_csr_rdaddr_d[4]&!dec_csr_rdaddr_d[3]
    &dec_csr_rdaddr_d[0]) | (dec_csr_rdaddr_d[10]&!dec_csr_rdaddr_d[4]
    &dec_csr_rdaddr_d[2]) | (!dec_csr_rdaddr_d[11]&dec_csr_rdaddr_d[7]
    &!dec_csr_rdaddr_d[5]&!dec_csr_rdaddr_d[3]&!dec_csr_rdaddr_d[2]
    &!dec_csr_rdaddr_d[1]) | (dec_csr_rdaddr_d[10]&!dec_csr_rdaddr_d[4]
    &!dec_csr_rdaddr_d[3]&dec_csr_rdaddr_d[1]);
logic legal_csr;
assign legal_csr = (!dec_csr_rdaddr_d[11]&!dec_csr_rdaddr_d[10]&dec_csr_rdaddr_d[9]
    &dec_csr_rdaddr_d[8]&!dec_csr_rdaddr_d[7]&!dec_csr_rdaddr_d[6]
    &!dec_csr_rdaddr_d[5]&!dec_csr_rdaddr_d[4]&!dec_csr_rdaddr_d[3]
    &!dec_csr_rdaddr_d[1]) | (dec_csr_rdaddr_d[11]&dec_csr_rdaddr_d[9]
    &dec_csr_rdaddr_d[8]&dec_csr_rdaddr_d[7]&dec_csr_rdaddr_d[6]
    &!dec_csr_rdaddr_d[5]&!dec_csr_rdaddr_d[4]&!dec_csr_rdaddr_d[2]
    &!dec_csr_rdaddr_d[1]&!dec_csr_rdaddr_d[0]) | (dec_csr_rdaddr_d[11]
    &!dec_csr_rdaddr_d[10]&dec_csr_rdaddr_d[9]&dec_csr_rdaddr_d[8]
    &!dec_csr_rdaddr_d[6]&!dec_csr_rdaddr_d[5]&!dec_csr_rdaddr_d[0]) | (
    !dec_csr_rdaddr_d[11]&dec_csr_rdaddr_d[10]&dec_csr_rdaddr_d[9]
    &dec_csr_rdaddr_d[8]&dec_csr_rdaddr_d[7]&dec_csr_rdaddr_d[6]
    &dec_csr_rdaddr_d[5]&dec_csr_rdaddr_d[4]&!dec_csr_rdaddr_d[2]
    &!dec_csr_rdaddr_d[1]) | (!dec_csr_rdaddr_d[11]&!dec_csr_rdaddr_d[10]
    &dec_csr_rdaddr_d[9]&dec_csr_rdaddr_d[8]&!dec_csr_rdaddr_d[7]
    &!dec_csr_rdaddr_d[6]&dec_csr_rdaddr_d[5]&dec_csr_rdaddr_d[2]) | (
    !dec_csr_rdaddr_d[11]&dec_csr_rdaddr_d[10]&dec_csr_rdaddr_d[9]
    &dec_csr_rdaddr_d[8]&dec_csr_rdaddr_d[7]&dec_csr_rdaddr_d[6]
    &dec_csr_rdaddr_d[4]&!dec_csr_rdaddr_d[3]&!dec_csr_rdaddr_d[2]
    &!dec_csr_rdaddr_d[0]) | (!dec_csr_rdaddr_d[11]&dec_csr_rdaddr_d[10]
    &dec_csr_rdaddr_d[9]&dec_csr_rdaddr_d[8]&dec_csr_rdaddr_d[7]
    &!dec_csr_rdaddr_d[6]&dec_csr_rdaddr_d[5]&!dec_csr_rdaddr_d[3]
    &!dec_csr_rdaddr_d[2]&!dec_csr_rdaddr_d[1]) | (dec_csr_rdaddr_d[11]
    &dec_csr_rdaddr_d[9]&dec_csr_rdaddr_d[8]&!dec_csr_rdaddr_d[7]
    &!dec_csr_rdaddr_d[6]&!dec_csr_rdaddr_d[5]&dec_csr_rdaddr_d[4]
    &!dec_csr_rdaddr_d[3]&!dec_csr_rdaddr_d[2]&dec_csr_rdaddr_d[0]) | (
    !dec_csr_rdaddr_d[11]&dec_csr_rdaddr_d[10]&dec_csr_rdaddr_d[9]
    &dec_csr_rdaddr_d[8]&dec_csr_rdaddr_d[7]&dec_csr_rdaddr_d[6]
    &!dec_csr_rdaddr_d[5]&dec_csr_rdaddr_d[4]&!dec_csr_rdaddr_d[3]
    &dec_csr_rdaddr_d[2]) | (dec_csr_rdaddr_d[11]&dec_csr_rdaddr_d[9]
    &dec_csr_rdaddr_d[8]&!dec_csr_rdaddr_d[7]&!dec_csr_rdaddr_d[6]
    &!dec_csr_rdaddr_d[5]&dec_csr_rdaddr_d[4]&!dec_csr_rdaddr_d[3]
    &dec_csr_rdaddr_d[2]&!dec_csr_rdaddr_d[1]&!dec_csr_rdaddr_d[0]) | (
    dec_csr_rdaddr_d[11]&!dec_csr_rdaddr_d[10]&dec_csr_rdaddr_d[9]
    &dec_csr_rdaddr_d[8]&dec_csr_rdaddr_d[7]&!dec_csr_rdaddr_d[5]
    &!dec_csr_rdaddr_d[4]&dec_csr_rdaddr_d[3]&!dec_csr_rdaddr_d[2]) | (
    !dec_csr_rdaddr_d[11]&dec_csr_rdaddr_d[10]&dec_csr_rdaddr_d[9]
    &dec_csr_rdaddr_d[8]&dec_csr_rdaddr_d[7]&dec_csr_rdaddr_d[6]
    &!dec_csr_rdaddr_d[5]&!dec_csr_rdaddr_d[4]&dec_csr_rdaddr_d[3]
    &!dec_csr_rdaddr_d[2]) | (!dec_csr_rdaddr_d[11]&!dec_csr_rdaddr_d[10]
    &dec_csr_rdaddr_d[9]&dec_csr_rdaddr_d[8]&!dec_csr_rdaddr_d[7]
    &!dec_csr_rdaddr_d[6]&dec_csr_rdaddr_d[5]&dec_csr_rdaddr_d[1]
    &dec_csr_rdaddr_d[0]) | (dec_csr_rdaddr_d[11]&!dec_csr_rdaddr_d[10]
    &dec_csr_rdaddr_d[9]&dec_csr_rdaddr_d[8]&dec_csr_rdaddr_d[7]
    &!dec_csr_rdaddr_d[5]&!dec_csr_rdaddr_d[4]&dec_csr_rdaddr_d[3]
    &!dec_csr_rdaddr_d[1]&!dec_csr_rdaddr_d[0]) | (dec_csr_rdaddr_d[11]
    &dec_csr_rdaddr_d[9]&dec_csr_rdaddr_d[8]&!dec_csr_rdaddr_d[7]
    &!dec_csr_rdaddr_d[6]&!dec_csr_rdaddr_d[5]&dec_csr_rdaddr_d[4]
    &!dec_csr_rdaddr_d[3]&!dec_csr_rdaddr_d[2]&dec_csr_rdaddr_d[1]) | (
    !dec_csr_rdaddr_d[11]&dec_csr_rdaddr_d[10]&dec_csr_rdaddr_d[9]
    &dec_csr_rdaddr_d[8]&dec_csr_rdaddr_d[7]&dec_csr_rdaddr_d[6]
    &!dec_csr_rdaddr_d[5]&dec_csr_rdaddr_d[4]&!dec_csr_rdaddr_d[3]
    &dec_csr_rdaddr_d[1]) | (dec_csr_rdaddr_d[11]&!dec_csr_rdaddr_d[10]
    &dec_csr_rdaddr_d[9]&dec_csr_rdaddr_d[8]&!dec_csr_rdaddr_d[6]
    &!dec_csr_rdaddr_d[5]&dec_csr_rdaddr_d[2]) | (!dec_csr_rdaddr_d[11]
    &dec_csr_rdaddr_d[10]&dec_csr_rdaddr_d[9]&dec_csr_rdaddr_d[8]
    &dec_csr_rdaddr_d[7]&!dec_csr_rdaddr_d[6]&dec_csr_rdaddr_d[5]
    &!dec_csr_rdaddr_d[4]&!dec_csr_rdaddr_d[3]&!dec_csr_rdaddr_d[2]
    &!dec_csr_rdaddr_d[0]) | (dec_csr_rdaddr_d[11]&!dec_csr_rdaddr_d[10]
    &dec_csr_rdaddr_d[9]&dec_csr_rdaddr_d[8]&!dec_csr_rdaddr_d[6]
    &!dec_csr_rdaddr_d[5]&dec_csr_rdaddr_d[1]) | (!dec_csr_rdaddr_d[11]
    &dec_csr_rdaddr_d[10]&dec_csr_rdaddr_d[9]&dec_csr_rdaddr_d[8]
    &dec_csr_rdaddr_d[7]&dec_csr_rdaddr_d[6]&!dec_csr_rdaddr_d[5]
    &!dec_csr_rdaddr_d[3]&!dec_csr_rdaddr_d[0]) | (!dec_csr_rdaddr_d[11]
    &!dec_csr_rdaddr_d[10]&dec_csr_rdaddr_d[9]&dec_csr_rdaddr_d[8]
    &!dec_csr_rdaddr_d[7]&dec_csr_rdaddr_d[6]&!dec_csr_rdaddr_d[5]
    &!dec_csr_rdaddr_d[4]&!dec_csr_rdaddr_d[3]&!dec_csr_rdaddr_d[2]) | (
    !dec_csr_rdaddr_d[11]&!dec_csr_rdaddr_d[10]&dec_csr_rdaddr_d[9]
    &dec_csr_rdaddr_d[8]&!dec_csr_rdaddr_d[7]&!dec_csr_rdaddr_d[5]
    &!dec_csr_rdaddr_d[4]&!dec_csr_rdaddr_d[3]&!dec_csr_rdaddr_d[1]
    &!dec_csr_rdaddr_d[0]) | (!dec_csr_rdaddr_d[11]&!dec_csr_rdaddr_d[10]
    &dec_csr_rdaddr_d[9]&dec_csr_rdaddr_d[8]&!dec_csr_rdaddr_d[7]
    &!dec_csr_rdaddr_d[6]&dec_csr_rdaddr_d[5]&dec_csr_rdaddr_d[3]) | (
    dec_csr_rdaddr_d[11]&!dec_csr_rdaddr_d[10]&dec_csr_rdaddr_d[9]
    &dec_csr_rdaddr_d[8]&!dec_csr_rdaddr_d[6]&!dec_csr_rdaddr_d[5]
    &dec_csr_rdaddr_d[3]) | (!dec_csr_rdaddr_d[11]&!dec_csr_rdaddr_d[10]
    &dec_csr_rdaddr_d[9]&dec_csr_rdaddr_d[8]&!dec_csr_rdaddr_d[7]
    &!dec_csr_rdaddr_d[6]&dec_csr_rdaddr_d[5]&dec_csr_rdaddr_d[4]) | (
    dec_csr_rdaddr_d[11]&!dec_csr_rdaddr_d[10]&dec_csr_rdaddr_d[9]
    &dec_csr_rdaddr_d[8]&!dec_csr_rdaddr_d[6]&!dec_csr_rdaddr_d[5]
    &dec_csr_rdaddr_d[4]);
assign dec_tlu_presync_d = presync & dec_csr_any_unq_d & ~dec_csr_wen_unq_d;
assign dec_tlu_postsync_d = postsync & dec_csr_any_unq_d;
assign valid_csr = ( legal_csr & (~(csr_dcsr | csr_dpc | csr_dmst | csr_dicawics | csr_dicad0 | csr_dicad1 | csr_dicago) | dbg_tlu_halted_f));
assign dec_csr_legal_d = ( dec_csr_any_unq_d &
                           valid_csr &           
                           ~(dec_csr_wen_unq_d & (csr_mvendorid | csr_marchid | csr_mimpid | csr_mhartid | csr_mdseac | csr_meihap))  
                           );
assign dec_csr_rddata_d[31:0] = ( ({32{csr_misa}}      & 32'h40001104) |
                                  ({32{csr_mvendorid}} & 32'h00000045) |
                                  ({32{csr_marchid}}   & 32'h0000000b) |
                                  ({32{csr_mimpid}}    & 32'h5) |
                                  ({32{csr_mstatus}}   & {19'b0, 2'b11, 3'b0, mstatus[1], 3'b0, mstatus[0], 3'b0}) |
                                  ({32{csr_mtvec}}     & {mtvec[30:1], 1'b0, mtvec[0]}) |
                                  ({32{csr_mip}}       & {1'b0, mip[5:3], 16'b0, mip[2], 3'b0, mip[1], 3'b0, mip[0], 3'b0}) |
                                  ({32{csr_mie}}       & {1'b0, mie[5:3], 16'b0, mie[2], 3'b0, mie[1], 3'b0, mie[0], 3'b0}) |
                                  ({32{csr_mcyclel}}   & mcyclel[31:0]) |
                                  ({32{csr_mcycleh}}   & mcycleh_inc[31:0]) |
                                  ({32{csr_minstretl}} & minstretl_read[31:0]) |
                                  ({32{csr_minstreth}} & minstreth_read[31:0]) |
                                  ({32{csr_mscratch}}  & mscratch[31:0]) |
                                  ({32{csr_mepc}}      & {mepc[31:1], 1'b0}) |
                                  ({32{csr_mcause}}    & mcause[31:0]) |
                                  ({32{csr_mtval}}     & mtval[31:0]) |
                                  ({32{csr_mrac}}      & mrac[31:0]) |
                                  ({32{csr_mdseac}}    & mdseac[31:0]) |
                                  ({32{csr_meivt}}     & {meivt[31:10], 10'b0}) |
                                  ({32{csr_meihap}}    & {meivt[31:10], meihap[9:2], 2'b0}) |
                                  ({32{csr_meicurpl}}  & {28'b0, meicurpl[3:0]}) |
                                  ({32{csr_meicidpl}}  & {28'b0, meicidpl[3:0]}) |
                                  ({32{csr_meipt}}     & {28'b0, meipt[3:0]}) |
                                  ({32{csr_mcgc}}      & {23'b0, mcgc[8:0]}) |
                                  ({32{csr_mfdc}}      & {13'b0, mfdc[18:0]}) |
                                  ({32{csr_dcsr}}      & {16'h4000, dcsr[15:2], 2'b11}) |
                                  ({32{csr_dpc}}       & {dpc[31:1], 1'b0}) |
                                  ({32{csr_dicad0}}    & dicad0[31:0]) |
                                  ({32{csr_dicad1}}    & {30'b0, dicad1[1:0]}) |
                                  ({32{csr_dicawics}}  & {7'b0, dicawics[18], 2'b0, dicawics[17:16], 4'b0, dicawics[15:2], 2'b0}) |
                                  ({32{csr_mtsel}}     & {30'b0, mtsel[1:0]}) |
                                  ({32{csr_mtdata1}}   & {mtdata1_tsel_out[31:0]}) |
                                  ({32{csr_mtdata2}}   & {mtdata2_tsel_out[31:0]}) |
                                  ({32{csr_micect}}    & {micect[31:0]}) |
                                  ({32{csr_miccmect}}  & {miccmect[31:0]}) |
                                  ({32{csr_mdccmect}}  & {mdccmect[31:0]}) |
                                  ({32{csr_mhpmc3}}    & mhpmc3[31:0]) |
                                  ({32{csr_mhpmc4}}    & mhpmc4[31:0]) |
                                  ({32{csr_mhpmc5}}    & mhpmc5[31:0]) |
                                  ({32{csr_mhpmc6}}    & mhpmc6[31:0]) |
                                  ({32{csr_mhpmc3h}}   & mhpmc3h[31:0]) |
                                  ({32{csr_mhpmc4h}}   & mhpmc4h[31:0]) |
                                  ({32{csr_mhpmc5h}}   & mhpmc5h[31:0]) |
                                  ({32{csr_mhpmc6h}}   & mhpmc6h[31:0]) |
                                  ({32{csr_mhpme3}}    & {26'b0,mhpme3[5:0]}) |
                                  ({32{csr_mhpme4}}    & {26'b0,mhpme4[5:0]}) |
                                  ({32{csr_mhpme5}}    & {26'b0,mhpme5[5:0]}) |
                                  ({32{csr_mhpme6}}    & {26'b0,mhpme6[5:0]}) |
                                  ({32{csr_mpmc}}      & {30'b0, mpmc[1], 1'b0}) |
                                  ({32{csr_mgpmc}}     & {31'b0, mgpmc}) |
                                  ({32{dec_timer_read_d}} & dec_timer_rddata_d[31:0])
                                  );
endmodule  
module dec_timer_ctl
  (
   input logic clk,
   input logic free_clk,
   input logic rst_l,
   input logic        dec_csr_wen_wb_mod,       
   input logic [11:0] dec_csr_rdaddr_d,       
   input logic [11:0] dec_csr_wraddr_wb,       
   input logic [31:0] dec_csr_wrdata_wb,    
   input logic dec_pause_state,  
   input logic dec_tlu_pmu_fw_halted,  
   input logic internal_dbg_halt_timers,  
   output logic [31:0] dec_timer_rddata_d,  
   output logic        dec_timer_read_d,  
   output logic        dec_timer_t0_pulse,  
   output logic        dec_timer_t1_pulse,  
   input  logic        scan_mode
   );
   logic [31:0] mitcnt0_ns, mitcnt0, mitcnt1_ns, mitcnt1, mitb0, mitb1, mitb0_b, mitb1_b, mitcnt0_inc, mitcnt1_inc;
   logic [2:0] mitctl0_ns, mitctl0, mitctl1_ns, mitctl1;
   logic wr_mitcnt0_wb, wr_mitcnt1_wb, wr_mitb0_wb, wr_mitb1_wb, wr_mitctl0_wb, wr_mitctl1_wb;
   logic mitcnt0_inc_ok, mitcnt1_inc_ok, mitcnt0_cout_nc, mitcnt1_cout_nc;
 logic mit0_match_ns;
 logic mit1_match_ns;
 logic mitctl0_0_b_ns;
 logic mitctl0_0_b;
 logic mitctl1_0_b_ns;
 logic mitctl1_0_b;
   assign mit0_match_ns = (mitcnt0[31:0] >= mitb0[31:0]);
   assign mit1_match_ns = (mitcnt1[31:0] >= mitb1[31:0]);
   assign dec_timer_t0_pulse = mit0_match_ns;
   assign dec_timer_t1_pulse = mit1_match_ns;
   assign wr_mitcnt0_wb = dec_csr_wen_wb_mod & (dec_csr_wraddr_wb[11:0] == 12'h7d2);
   assign mitcnt0_inc_ok = mitctl0[0] & (~dec_pause_state | mitctl0[2]) & (~dec_tlu_pmu_fw_halted | mitctl0[1]) & ~internal_dbg_halt_timers;
   assign {mitcnt0_cout_nc, mitcnt0_inc[31:0]} = mitcnt0[31:0] + {31'b0, 1'b1};
   assign mitcnt0_ns[31:0] = mit0_match_ns ? 'b0 : wr_mitcnt0_wb ? dec_csr_wrdata_wb[31:0] : mitcnt0_inc[31:0];
   rvdffe #(32) mitcnt0_ff      (.*, .en(wr_mitcnt0_wb | mitcnt0_inc_ok | mit0_match_ns), .din(mitcnt0_ns[31:0]), .dout(mitcnt0[31:0]));
   assign wr_mitcnt1_wb = dec_csr_wen_wb_mod & (dec_csr_wraddr_wb[11:0] == 12'h7d5);
   assign mitcnt1_inc_ok = mitctl1[0] & (~dec_pause_state | mitctl1[2]) & (~dec_tlu_pmu_fw_halted | mitctl1[1]) & ~internal_dbg_halt_timers;
   assign {mitcnt1_cout_nc, mitcnt1_inc[31:0]} = mitcnt1[31:0] + {31'b0, 1'b1};
   assign mitcnt1_ns[31:0] = mit1_match_ns ? 'b0 :  wr_mitcnt1_wb ? dec_csr_wrdata_wb[31:0] : mitcnt1_inc[31:0];
   rvdffe #(32) mitcnt1_ff      (.*, .en(wr_mitcnt1_wb | mitcnt1_inc_ok | mit1_match_ns), .din(mitcnt1_ns[31:0]), .dout(mitcnt1[31:0]));
   assign wr_mitb0_wb = dec_csr_wen_wb_mod & (dec_csr_wraddr_wb[11:0] == 12'h7d3);
   rvdffe #(32) mitb0_ff      (.*, .en(wr_mitb0_wb), .din(~dec_csr_wrdata_wb[31:0]), .dout(mitb0_b[31:0]));
   assign mitb0[31:0] = ~mitb0_b[31:0];
   assign wr_mitb1_wb = dec_csr_wen_wb_mod & (dec_csr_wraddr_wb[11:0] == 12'h7d6);
   rvdffe #(32) mitb1_ff      (.*, .en(wr_mitb1_wb), .din(~dec_csr_wrdata_wb[31:0]), .dout(mitb1_b[31:0]));
   assign mitb1[31:0] = ~mitb1_b[31:0];
   assign wr_mitctl0_wb = dec_csr_wen_wb_mod & (dec_csr_wraddr_wb[11:0] == 12'h7d4);
   assign mitctl0_ns[2:0] = wr_mitctl0_wb ? {dec_csr_wrdata_wb[2:0]} : {mitctl0[2:0]};
   assign mitctl0_0_b_ns = ~mitctl0_ns[0];
   rvdff #(3) mitctl0_ff      (.*, .clk(free_clk), .din({mitctl0_ns[2:1], mitctl0_0_b_ns}), .dout({mitctl0[2:1], mitctl0_0_b}));
   assign mitctl0[0] = ~mitctl0_0_b;
   assign wr_mitctl1_wb = dec_csr_wen_wb_mod & (dec_csr_wraddr_wb[11:0] == 12'h7d7);
   assign mitctl1_ns[2:0] = wr_mitctl1_wb ? {dec_csr_wrdata_wb[2:0]} : {mitctl1[2:0]};
   assign mitctl1_0_b_ns = ~mitctl1_ns[0];
   rvdff #(3) mitctl1_ff      (.*, .clk(free_clk), .din({mitctl1_ns[2:1], mitctl1_0_b_ns}), .dout({mitctl1[2:1], mitctl1_0_b}));
   assign mitctl1[0] = ~mitctl1_0_b;
logic csr_mitctl0;
logic csr_mitctl1;
logic csr_mitb0;
logic csr_mitb1;
logic csr_mitcnt0;
logic csr_mitcnt1;
assign csr_mitctl0 = (dec_csr_rdaddr_d[6]&dec_csr_rdaddr_d[4]
    &dec_csr_rdaddr_d[2]&!dec_csr_rdaddr_d[1]&!dec_csr_rdaddr_d[0]);
assign csr_mitctl1 = (dec_csr_rdaddr_d[6]&dec_csr_rdaddr_d[2]
    &dec_csr_rdaddr_d[1]&dec_csr_rdaddr_d[0]);
assign csr_mitb0 = (dec_csr_rdaddr_d[6]&!dec_csr_rdaddr_d[5]&dec_csr_rdaddr_d[4]
    &!dec_csr_rdaddr_d[2]&dec_csr_rdaddr_d[0]);
assign csr_mitb1 = (dec_csr_rdaddr_d[6]&dec_csr_rdaddr_d[4]&dec_csr_rdaddr_d[2]
    &dec_csr_rdaddr_d[1]&!dec_csr_rdaddr_d[0]);
assign csr_mitcnt0 = (dec_csr_rdaddr_d[6]&!dec_csr_rdaddr_d[5]
    &dec_csr_rdaddr_d[4]&!dec_csr_rdaddr_d[2]&dec_csr_rdaddr_d[1]
    &!dec_csr_rdaddr_d[0]);
assign csr_mitcnt1 = (dec_csr_rdaddr_d[6]&dec_csr_rdaddr_d[2]
    &!dec_csr_rdaddr_d[1]&dec_csr_rdaddr_d[0]);
   assign dec_timer_read_d = csr_mitcnt1 | csr_mitcnt0 | csr_mitb1 | csr_mitb0 | csr_mitctl0 | csr_mitctl1;
   assign dec_timer_rddata_d[31:0] = ( ({32{csr_mitcnt0}}      & mitcnt0[31:0]) |
                                       ({32{csr_mitcnt1}}      & mitcnt1[31:0]) |
                                       ({32{csr_mitb0}}        & mitb0[31:0]) |
                                       ({32{csr_mitb1}}        & mitb1[31:0]) |
                                       ({32{csr_mitctl0}}      & {29'b0, mitctl0[2:0]}) |
                                       ({32{csr_mitctl1}}      & {29'b0, mitctl1[2:0]})
                                       );
endmodule  
module dec_trigger
   import swerv_types::*;
(
   input logic         clk,
   input logic         rst_l,
   input trigger_pkt_t [3:0] trigger_pkt_any,            
   input logic [31:1]  dec_i0_pc_d,                      
   input logic [31:1]  dec_i1_pc_d,                      
   output logic [3:0] dec_i0_trigger_match_d,
   output logic [3:0] dec_i1_trigger_match_d
);
   logic [3:0][31:0]  dec_i0_match_data;
   logic [3:0]        dec_i0_trigger_data_match;
   logic [3:0][31:0]  dec_i1_match_data;
   logic [3:0]        dec_i1_trigger_data_match;
   for (genvar i=0; i<4; i++) begin
      assign dec_i0_match_data[i][31:0] = ({32{~trigger_pkt_any[i].select & trigger_pkt_any[i].execute}} & {dec_i0_pc_d[31:1], trigger_pkt_any[i].tdata2[0]});       
      assign dec_i1_match_data[i][31:0] = ({32{~trigger_pkt_any[i].select & trigger_pkt_any[i].execute}} & {dec_i1_pc_d[31:1], trigger_pkt_any[i].tdata2[0]} );      
      rvmaskandmatch trigger_i0_match (.mask(trigger_pkt_any[i].tdata2[31:0]), .data(dec_i0_match_data[i][31:0]), .masken(trigger_pkt_any[i].match), .match(dec_i0_trigger_data_match[i]));
      rvmaskandmatch trigger_i1_match (.mask(trigger_pkt_any[i].tdata2[31:0]), .data(dec_i1_match_data[i][31:0]), .masken(trigger_pkt_any[i].match), .match(dec_i1_trigger_data_match[i]));
      assign dec_i0_trigger_match_d[i] = trigger_pkt_any[i].execute & trigger_pkt_any[i].m & dec_i0_trigger_data_match[i];
      assign dec_i1_trigger_match_d[i] = trigger_pkt_any[i].execute & trigger_pkt_any[i].m & dec_i1_trigger_data_match[i];
   end
endmodule  
module dec
   import swerv_types::*;
(
   input logic clk,
   input logic free_clk,
   input logic active_clk,
   output logic       dec_pause_state_cg,            
   input logic rst_l,                         
   input logic [31:1] rst_vec,                
   input logic        nmi_int,                
   input logic [31:1] nmi_vec,                
   input logic  i_cpu_halt_req,               
   input logic  i_cpu_run_req,                
   output logic o_cpu_halt_status,            
   output logic o_cpu_halt_ack,               
   output logic o_cpu_run_ack,                
   output logic o_debug_mode_status,          
   input logic mpc_debug_halt_req,  
   input logic mpc_debug_run_req,  
   input logic mpc_reset_run_req,  
   output logic mpc_debug_halt_ack,  
   output logic mpc_debug_run_ack,  
   output logic debug_brkpt_status,  
   output logic dec_ib0_valid_eff_d,          
   output logic dec_ib1_valid_eff_d,
   input logic       exu_pmu_i0_br_misp,      
   input logic       exu_pmu_i0_br_ataken,    
   input logic       exu_pmu_i0_pc4,          
   input logic       exu_pmu_i1_br_misp,      
   input logic       exu_pmu_i1_br_ataken,    
   input logic       exu_pmu_i1_pc4,          
   input logic                                 lsu_nonblock_load_valid_dc3,       
   input logic [3-1:0]  lsu_nonblock_load_tag_dc3,         
   input logic                                 lsu_nonblock_load_inv_dc5,         
   input logic [3-1:0]  lsu_nonblock_load_inv_tag_dc5,     
   input logic                                 lsu_nonblock_load_data_valid,      
   input logic                                 lsu_nonblock_load_data_error,      
   input logic [3-1:0]  lsu_nonblock_load_data_tag,        
   input logic [31:0]                          lsu_nonblock_load_data,            
   input logic       lsu_pmu_bus_trxn,        
   input logic       lsu_pmu_bus_misaligned,  
   input logic       lsu_pmu_bus_error,       
   input logic       lsu_pmu_bus_busy,        
   input logic       lsu_pmu_misaligned_dc3,  
   input logic [1:0] ifu_pmu_instr_aligned,   
   input logic       ifu_pmu_align_stall,     
   input logic       ifu_pmu_fetch_stall,     
   input logic       ifu_pmu_ic_miss,         
   input logic       ifu_pmu_ic_hit,          
   input logic       ifu_pmu_bus_error,       
   input logic       ifu_pmu_bus_busy,        
   input logic       ifu_pmu_bus_trxn,        
   input logic [3:0]  lsu_trigger_match_dc3,
   input logic        dbg_cmd_valid,    
   input logic  [1:0] dbg_cmd_size,     
   input logic        dbg_cmd_write,    
   input logic  [1:0] dbg_cmd_type,     
   input logic [31:0] dbg_cmd_addr,     
   input logic  [1:0] dbg_cmd_wrdata,   
   input logic   ifu_i0_icaf,           
   input logic   ifu_i1_icaf,
   input logic   ifu_i0_icaf_second,        
   input logic   ifu_i1_icaf_second,
   input logic   ifu_i0_perr,           
   input logic   ifu_i1_perr,
   input logic   ifu_i0_sbecc,          
   input logic   ifu_i1_sbecc,
   input logic   ifu_i0_dbecc,          
   input logic   ifu_i1_dbecc,
   input logic lsu_freeze_dc3,          
   input logic lsu_idle_any,       
   input logic lsu_halt_idle_any,       
   input br_pkt_t i0_brp,               
   input br_pkt_t i1_brp,
   input    lsu_error_pkt_t lsu_error_pkt_dc3,  
   input logic         lsu_single_ecc_error_incr,     
   input logic lsu_load_ecc_stbuf_full_dc3,     
   input logic         lsu_imprecise_error_load_any,    
   input logic         lsu_imprecise_error_store_any,   
   input logic [31:0]  lsu_imprecise_error_addr_any,    
   input logic         lsu_freeze_external_ints_dc3,    
   input logic exu_i0_flush_lower_e4,        
   input logic exu_i1_flush_lower_e4,        
   input logic [31:1] exu_i0_flush_path_e4,  
   input logic [31:1] exu_i1_flush_path_e4,  
   input logic [15:0] ifu_illegal_inst,      
   input logic exu_div_stall,                
   input logic [31:0]  exu_div_result,       
   input logic exu_div_finish,               
   input logic [31:0] exu_mul_result_e3,     
   input logic [31:0] exu_csr_rs1_e1,        
   input logic [31:0] lsu_result_dc3,        
   input logic [31:0] lsu_result_corr_dc4,  
   input logic        lsu_load_stall_any,    
   input logic        lsu_store_stall_any,   
   input logic        dma_dccm_stall_any,    
   input logic        dma_iccm_stall_any,    
   input logic       iccm_dma_sb_error,      
   input logic exu_i0_flush_final,           
   input logic exu_i1_flush_final,           
   input logic [31:1] exu_npc_e4,            
   input logic exu_flush_final,              
   input logic [31:0] exu_i0_result_e1,      
   input logic [31:0] exu_i1_result_e1,
   input logic [31:0] exu_i0_result_e4,      
   input logic [31:0] exu_i1_result_e4,
   input logic         ifu_i0_valid, ifu_i1_valid,     
   input logic [31:0]  ifu_i0_instr, ifu_i1_instr,     
   input logic [31:1]  ifu_i0_pc, ifu_i1_pc,           
   input logic         ifu_i0_pc4, ifu_i1_pc4,         
   input logic  [31:1] exu_i0_pc_e1,                   
   input logic  [31:1] exu_i1_pc_e1,
   input logic mexintpend,                             
   input logic timer_int,                              
   input logic [7:0] pic_claimid,                      
   input logic [3:0] pic_pl,                           
   input logic       mhwakeup,                         
   output logic [3:0] dec_tlu_meicurpl,                
   output logic [3:0] dec_tlu_meipt,                   
   input logic [33:0] ifu_ic_debug_rd_data,            
   input logic ifu_ic_debug_rd_data_valid,             
   output cache_debug_pkt_t dec_tlu_ic_diag_pkt,       
   input logic dbg_halt_req,                  
   input logic dbg_resume_req,                
   input logic ifu_miss_state_idle,           
  output logic dec_tlu_flush_noredir_wb ,     
   output logic dec_tlu_mpc_halted_only,  
   output logic dec_tlu_dbg_halted,           
   output logic dec_tlu_pmu_fw_halted,        
   output logic dec_tlu_debug_mode,           
   output logic dec_tlu_resume_ack,           
   output logic dec_tlu_flush_leak_one_wb,    
   output logic dec_tlu_flush_err_wb,         
   output logic dec_tlu_stall_dma,            
   output logic dec_debug_wdata_rs1_d,        
   output logic [31:0] dec_dbg_rddata,        
   output logic dec_dbg_cmd_done,             
   output logic dec_dbg_cmd_fail,             
   output trigger_pkt_t  [3:0] trigger_pkt_any,  
   input logic [5:4] exu_i0_br_index_e4,  
   input logic [1:0]  exu_i0_br_hist_e4,                              
   input logic [1:0]  exu_i0_br_bank_e4,                              
   input logic        exu_i0_br_error_e4,                             
   input logic        exu_i0_br_start_error_e4,                       
   input logic        exu_i0_br_valid_e4,                             
   input logic        exu_i0_br_mp_e4,                                
   input logic        exu_i0_br_middle_e4,                            
   input logic [4:0]  exu_i0_br_fghr_e4,                
   input logic [5:4] exu_i1_br_index_e4,  
   input logic [1:0]  exu_i1_br_hist_e4,                              
   input logic [1:0]  exu_i1_br_bank_e4,                              
   input logic        exu_i1_br_error_e4,                             
   input logic        exu_i1_br_start_error_e4,                       
   input logic        exu_i1_br_valid_e4,                             
   input logic        exu_i1_br_mp_e4,                                
   input logic        exu_i1_br_middle_e4,                            
   input logic [4:0]  exu_i1_br_fghr_e4,                
   input logic        exu_i1_br_way_e4,              
   input logic        exu_i0_br_way_e4,              
   output logic  [31:0] gpr_i0_rs1_d,                
   output logic  [31:0] gpr_i0_rs2_d,                
   output logic  [31:0] gpr_i1_rs1_d,
   output logic  [31:0] gpr_i1_rs2_d,
   output logic [31:0] dec_i0_immed_d,               
   output logic [31:0] dec_i1_immed_d,
   output logic [12:1] dec_i0_br_immed_d,            
   output logic [12:1] dec_i1_br_immed_d,
   output        alu_pkt_t i0_ap,                    
   output        alu_pkt_t i1_ap,
   output logic          dec_i0_alu_decode_d,        
   output logic          dec_i1_alu_decode_d,
   output logic          dec_i0_select_pc_d,         
   output logic          dec_i1_select_pc_d,
   output logic [31:1] dec_i0_pc_d, dec_i1_pc_d,     
   output logic         dec_i0_rs1_bypass_en_d,      
   output logic         dec_i0_rs2_bypass_en_d,      
   output logic         dec_i1_rs1_bypass_en_d,
   output logic         dec_i1_rs2_bypass_en_d,
   output logic [31:0] i0_rs1_bypass_data_d,        
   output logic [31:0] i0_rs2_bypass_data_d,        
   output logic [31:0] i1_rs1_bypass_data_d,
   output logic [31:0] i1_rs2_bypass_data_d,
   output logic         dec_ib3_valid_d,            
   output logic         dec_ib2_valid_d,            
   output lsu_pkt_t    lsu_p,                       
   output mul_pkt_t    mul_p,                       
   output div_pkt_t    div_p,                       
   output logic [11:0] dec_lsu_offset_d,            
   output logic        dec_i0_lsu_d,                
   output logic        dec_i1_lsu_d,
   output logic        flush_final_e3,              
   output logic        i0_flush_final_e3,           
   output logic        dec_csr_ren_d,               
   output logic        dec_tlu_cancel_e4,           
   output logic        dec_tlu_flush_lower_wb,      
   output logic [31:1] dec_tlu_flush_path_wb,       
   output logic        dec_tlu_i0_kill_writeb_wb,   
   output logic        dec_tlu_i1_kill_writeb_wb,   
   output logic        dec_tlu_fence_i_wb,          
   output logic        dec_i0_mul_d,                
   output logic        dec_i1_mul_d,
   output logic        dec_i0_div_d,                
   output logic        dec_i1_div_d,
   output logic        dec_i1_valid_e1,             
   output logic        dec_div_decode_e4,           
   output logic [31:1] pred_correct_npc_e2,         
   output logic        dec_i0_rs1_bypass_en_e3,     
   output logic        dec_i0_rs2_bypass_en_e3,     
   output logic        dec_i1_rs1_bypass_en_e3,
   output logic        dec_i1_rs2_bypass_en_e3,
   output logic [31:0] i0_rs1_bypass_data_e3,       
   output logic [31:0] i0_rs2_bypass_data_e3,       
   output logic [31:0] i1_rs1_bypass_data_e3,
   output logic [31:0] i1_rs2_bypass_data_e3,
   output logic        dec_i0_sec_decode_e3,        
   output logic        dec_i1_sec_decode_e3,
   output logic [31:1] dec_i0_pc_e3,                
   output logic [31:1] dec_i1_pc_e3,
   output logic        dec_i0_rs1_bypass_en_e2,     
   output logic        dec_i0_rs2_bypass_en_e2,     
   output logic        dec_i1_rs1_bypass_en_e2,
   output logic        dec_i1_rs2_bypass_en_e2,
   output logic [31:0] i0_rs1_bypass_data_e2,       
   output logic [31:0] i0_rs2_bypass_data_e2,       
   output logic [31:0] i1_rs1_bypass_data_e2,
   output logic [31:0] i1_rs2_bypass_data_e2,
   output br_tlu_pkt_t dec_tlu_br0_wb_pkt,          
   output br_tlu_pkt_t dec_tlu_br1_wb_pkt,          
   output logic [1:0] dec_tlu_perfcnt0,                   
   output logic [1:0] dec_tlu_perfcnt1,                   
   output logic [1:0] dec_tlu_perfcnt2,                   
   output logic [1:0] dec_tlu_perfcnt3,                   
   output predict_pkt_t  i0_predict_p_d,            
   output predict_pkt_t  i1_predict_p_d,
   output logic dec_i0_lsu_decode_d,                
   output logic [31:0] i0_result_e4_eff,            
   output logic [31:0] i1_result_e4_eff,
   output   logic dec_tlu_i0_valid_e4,              
   output   logic dec_tlu_i1_valid_e4,              
   output logic [31:0] i0_result_e2,                
   output logic [31:0] dec_tlu_mrac_ff,             
   output logic [31:1] dec_tlu_i0_pc_e4,            
   output logic [31:1] dec_tlu_i1_pc_e4,
   output logic [4:2] dec_i0_data_en,               
   output logic [4:1] dec_i0_ctl_en,
   output logic [4:2] dec_i1_data_en,
   output logic [4:1] dec_i1_ctl_en,
   output logic       dec_nonblock_load_freeze_dc2,   
   input logic [15:0] ifu_i0_cinst,                   
   input logic [15:0] ifu_i1_cinst,
   output trace_pkt_t  trace_rv_trace_pkt,              
   output logic  dec_tlu_sideeffect_posted_disable,     
   output logic  dec_tlu_core_ecc_disable,            
   output logic  dec_tlu_sec_alu_disable,             
   output logic  dec_tlu_non_blocking_disable,        
   output logic  dec_tlu_fast_div_disable,            
   output logic  dec_tlu_bpred_disable,               
   output logic  dec_tlu_wb_coalescing_disable,       
   output logic  dec_tlu_ld_miss_byp_wb_disable,      
   output logic [2:0]  dec_tlu_dma_qos_prty,          
   output logic  dec_tlu_misc_clk_override,           
   output logic  dec_tlu_exu_clk_override,            
   output logic  dec_tlu_ifu_clk_override,            
   output logic  dec_tlu_lsu_clk_override,            
   output logic  dec_tlu_bus_clk_override,            
   output logic  dec_tlu_pic_clk_override,            
   output logic  dec_tlu_dccm_clk_override,           
   output logic  dec_tlu_icm_clk_override,            
   input  logic        scan_mode
   );
   localparam GPR_BANKS = 1;
   localparam GPR_BANKS_LOG2 = (GPR_BANKS == 1) ? 1 : $clog2(GPR_BANKS);
   logic  dec_tlu_dec_clk_override;  
   logic  clk_override;
   logic               dec_ib1_valid_d;
   logic               dec_ib0_valid_d;
   logic [1:0]         dec_pmu_instr_decoded;
   logic               dec_pmu_decode_stall;
   logic               dec_pmu_presync_stall;
   logic               dec_pmu_postsync_stall;
   logic dec_tlu_wr_pause_wb;            
   logic        dec_i0_rs1_en_d;
   logic        dec_i0_rs2_en_d;
   logic        dec_fence_pending;  
   logic [4:0]  dec_i0_rs1_d;
   logic [4:0]  dec_i0_rs2_d;
   logic        dec_i1_rs1_en_d;
   logic        dec_i1_rs2_en_d;
   logic [4:0]  dec_i1_rs1_d;
   logic [4:0]  dec_i1_rs2_d;
   logic [31:0] dec_i0_instr_d, dec_i1_instr_d;
   logic  dec_tlu_pipelining_disable;
   logic  dec_tlu_dual_issue_disable;
   logic [4:0]  dec_i0_waddr_wb;
   logic        dec_i0_wen_wb;
   logic [31:0] dec_i0_wdata_wb;
   logic [4:0]  dec_i1_waddr_wb;
   logic        dec_i1_wen_wb;
   logic [31:0] dec_i1_wdata_wb;
   logic        dec_csr_wen_wb;       
   logic [11:0] dec_csr_rdaddr_d;       
   logic [11:0] dec_csr_wraddr_wb;       
   logic [31:0] dec_csr_wrdata_wb;     
   logic [31:0] dec_csr_rddata_d;     
   logic        dec_csr_legal_d;             
   logic        dec_csr_wen_unq_d;        
   logic        dec_csr_any_unq_d;        
   logic        dec_csr_stall_int_ff;     
   trap_pkt_t dec_tlu_packet_e4;
   logic        dec_i0_pc4_d, dec_i1_pc4_d;
   logic        dec_tlu_presync_d;
   logic        dec_tlu_postsync_d;
   logic        dec_tlu_debug_stall;
   logic [31:0] dec_illegal_inst;
   logic                      wen_bank_id;
   logic [GPR_BANKS_LOG2-1:0] wr_bank_id;
   logic                      dec_i0_icaf_d;
   logic                      dec_i1_icaf_d;
   logic                      dec_i0_perr_d;
   logic                      dec_i1_perr_d;
   logic                      dec_i0_sbecc_d;
   logic                      dec_i1_sbecc_d;
   logic                      dec_i0_dbecc_d;
   logic                      dec_i1_dbecc_d;
   logic                      dec_i0_icaf_second_d;
   logic                      dec_i0_decode_d;
   logic                      dec_i1_decode_d;
   logic [3:0]                dec_i0_trigger_match_d;
   logic [3:0]                dec_i1_trigger_match_d;
   logic                      dec_debug_fence_d;
   logic                      dec_nonblock_load_wen;
   logic [4:0]                dec_nonblock_load_waddr;
   logic                      dec_tlu_flush_pause_wb;
   logic                      dec_i0_load_e4;
   logic                      dec_pause_state;
   br_pkt_t dec_i0_brp;
   br_pkt_t dec_i1_brp;
   assign clk_override = dec_tlu_dec_clk_override;
   assign dec_dbg_rddata[31:0] = dec_i0_wdata_wb[31:0];
   dec_ib_ctl instbuff (.*
                        );
   dec_decode_ctl decode (.*);
   dec_tlu_ctl tlu (.*);
   assign wen_bank_id = '0;
   assign wr_bank_id  = '0;
   dec_gpr_ctl #(.GPR_BANKS(GPR_BANKS),
                 .GPR_BANKS_LOG2(GPR_BANKS_LOG2)) arf (.*,
                    .raddr0(dec_i0_rs1_d[4:0]), .rden0(dec_i0_rs1_en_d),
                    .raddr1(dec_i0_rs2_d[4:0]), .rden1(dec_i0_rs2_en_d),
                    .raddr2(dec_i1_rs1_d[4:0]), .rden2(dec_i1_rs1_en_d),
                    .raddr3(dec_i1_rs2_d[4:0]), .rden3(dec_i1_rs2_en_d),
                    .waddr0(dec_i0_waddr_wb[4:0]),         .wen0(dec_i0_wen_wb),         .wd0(dec_i0_wdata_wb[31:0]),
                    .waddr1(dec_i1_waddr_wb[4:0]),         .wen1(dec_i1_wen_wb),         .wd1(dec_i1_wdata_wb[31:0]),
                    .waddr2(dec_nonblock_load_waddr[4:0]), .wen2(dec_nonblock_load_wen), .wd2(lsu_nonblock_load_data[31:0]),
                    .rd0(gpr_i0_rs1_d[31:0]), .rd1(gpr_i0_rs2_d[31:0]),
                    .rd2(gpr_i1_rs1_d[31:0]), .rd3(gpr_i1_rs2_d[31:0])
                    );
   dec_trigger dec_trigger (.*);
   logic [15:0] dec_i0_cinst_d;
   logic [15:0] dec_i1_cinst_d;
   logic [31:0]               dec_i0_inst_wb1;
   logic [31:0]               dec_i1_inst_wb1;
   logic [31:1]               dec_i0_pc_wb1;
   logic [31:1]               dec_i1_pc_wb1;
   logic dec_tlu_i1_valid_wb1, dec_tlu_i0_valid_wb1,  dec_tlu_int_valid_wb1;
   logic [4:0] dec_tlu_exc_cause_wb1;
   logic [31:0] dec_tlu_mtval_wb1;
   logic        dec_tlu_i0_exc_valid_wb1, dec_tlu_i1_exc_valid_wb1;
   assign trace_rv_trace_pkt.trace_rv_i_insn_ip    = { 32'b0, dec_i1_inst_wb1[31:0], dec_i0_inst_wb1[31:0] };
   assign trace_rv_trace_pkt.trace_rv_i_address_ip = { 32'b0, dec_i1_pc_wb1[31:1], 1'b0, dec_i0_pc_wb1[31:1], 1'b0 };
   assign trace_rv_trace_pkt.trace_rv_i_valid_ip =     {dec_tlu_int_valid_wb1,    
                                                    dec_tlu_i1_valid_wb1 | dec_tlu_i1_exc_valid_wb1,   
                                                    dec_tlu_i0_valid_wb1 | dec_tlu_i0_exc_valid_wb1
                                                    };
   assign trace_rv_trace_pkt.trace_rv_i_exception_ip = {dec_tlu_int_valid_wb1, dec_tlu_i1_exc_valid_wb1, dec_tlu_i0_exc_valid_wb1};
   assign trace_rv_trace_pkt.trace_rv_i_ecause_ip =     dec_tlu_exc_cause_wb1[4:0];   
   assign trace_rv_trace_pkt.trace_rv_i_interrupt_ip = {dec_tlu_int_valid_wb1,2'b0};
   assign trace_rv_trace_pkt.trace_rv_i_tval_ip =    dec_tlu_mtval_wb1[31:0];         
endmodule  
module exu_alu_ctl
   import swerv_types::*;
(
   input logic  clk,                       
   input logic  active_clk,                
   input logic  rst_l,                     
   input logic  scan_mode,                 
   input predict_pkt_t  predict_p,         
   input logic freeze,                     
   input logic [31:0] a,                   
   input logic [31:0] b,                   
   input logic [31:1] pc,                  
   input logic valid,                      
   input logic flush,                      
   input logic [12:1] brimm,               
   input alu_pkt_t ap,                     
   input logic  enable,                    
   output logic [31:0] out,                
   output logic        flush_upper,        
   output logic [31:1] flush_path,         
   output logic [31:1] pc_ff,              
   output logic pred_correct,              
   output predict_pkt_t predict_p_ff       
  );
   logic        [31:0]  aout,bm;
   logic                cout,ov,neg;
   logic        [3:1]   logic_sel;
   logic        [31:0]  lout;
   logic        [31:0]  sout;
   logic                sel_logic,sel_shift,sel_adder;
   logic                slt_one;
   logic                actual_taken;
   logic signed [31:0]  a_ff;
   logic [31:0]         b_ff;
   logic [12:1]         brimm_ff;
   logic [31:1]         pcout;
   logic                valid_ff;
   logic [31:0]         ashift;
   logic                cond_mispredict;
   logic                target_mispredict;
   logic                eq, ne, lt, ge;
   rvdffs #(1)  validff (.*, .clk(active_clk), .en(~freeze), .din(valid & ~flush), .dout(valid_ff));
   rvdffe #(32) aff (.*, .en(enable & valid), .din(a[31:0]), .dout(a_ff[31:0]));
   rvdffe #(32) bff (.*, .en(enable & valid), .din(b[31:0]), .dout(b_ff[31:0]));
   rvdffe #(31) pcff (.*, .en(enable), .din(pc[31:1]), .dout(pc_ff[31:1]));
   rvdffe #(12) brimmff (.*, .en(enable), .din(brimm[12:1]), .dout(brimm_ff[12:1]));
   predict_pkt_t pp_ff;
   rvdffe #($bits(predict_pkt_t)) predictpacketff (.*,
                           .en(enable),
                           .din(predict_p),
                           .dout(pp_ff)
                           );
   assign bm[31:0] = ( ap.sub ) ? ~b_ff[31:0] : b_ff[31:0];
   assign {cout, aout[31:0]} = {1'b0, a_ff[31:0]} + {1'b0, bm[31:0]} + {32'b0, ap.sub};
   assign ov = (~a_ff[31] & ~bm[31] &  aout[31]) |
               ( a_ff[31] &  bm[31] & ~aout[31] );
   assign neg = aout[31];
   assign eq = a_ff[31:0] == b_ff[31:0];
   assign ne = ~eq;
   assign logic_sel[3] = ap.land | ap.lor;
   assign logic_sel[2] = ap.lor | ap.lxor;
   assign logic_sel[1] = ap.lor | ap.lxor;
   assign lout[31:0] =  (  a_ff[31:0] &  b_ff[31:0] & {32{logic_sel[3]}} ) |
                        (  a_ff[31:0] & ~b_ff[31:0] & {32{logic_sel[2]}} ) |
                        ( ~a_ff[31:0] &  b_ff[31:0] & {32{logic_sel[1]}} );
   assign ashift[31:0] = a_ff >>> b_ff[4:0];
   assign sout[31:0] = ( {32{ap.sll}} & (a_ff[31:0] <<  b_ff[4:0]) ) |
                       ( {32{ap.srl}} & (a_ff[31:0] >>  b_ff[4:0]) ) |
                       ( {32{ap.sra}} &  ashift[31:0]              );
   assign sel_logic = |{ap.land,ap.lor,ap.lxor};
   assign sel_shift = |{ap.sll,ap.srl,ap.sra};
   assign sel_adder = (ap.add | ap.sub) & ~ap.slt;
   assign lt = (~ap.unsign & (neg ^ ov)) |
               ( ap.unsign & ~cout);
   assign ge = ~lt;
   assign slt_one = (ap.slt & lt);
   assign out[31:0] = ({32{sel_logic}} & lout[31:0]) |
                      ({32{sel_shift}} & sout[31:0]) |
                      ({32{sel_adder}} & aout[31:0]) |
                      ({32{ap.jal | pp_ff.pcall | pp_ff.pja | pp_ff.pret}} & {pcout[31:1],1'b0}) |
                      ({32{ap.csr_write}} & ((ap.csr_imm) ? b_ff[31:0] : a_ff[31:0])) |                 
                      ({31'b0, slt_one});
   logic                any_jal;
   assign any_jal =       ap.jal |
                          pp_ff.pcall |
                          pp_ff.pja   |
                          pp_ff.pret;
   assign actual_taken = (ap.beq & eq) |
                         (ap.bne & ne) |
                         (ap.blt & lt) |
                         (ap.bge & ge) |
                         (any_jal);
   rvbradder ibradder (
                     .pc(pc_ff[31:1]),
                     .offset(brimm_ff[12:1]),
                     .dout(pcout[31:1])
                      );
   assign pred_correct = ((ap.predict_nt & ~actual_taken) |
                          (ap.predict_t  &  actual_taken)) & ~any_jal;
   assign flush_path[31:1] = (any_jal) ? aout[31:1] : pcout[31:1];
   assign cond_mispredict = (ap.predict_t & ~actual_taken) |
                            (ap.predict_nt & actual_taken);
   assign target_mispredict = pp_ff.pret & (pp_ff.prett[31:1] != aout[31:1]);
   assign flush_upper = ( ap.jal | cond_mispredict | target_mispredict) & valid_ff & ~flush & ~freeze;
   logic [1:0]          newhist;
   assign newhist[1] = (pp_ff.hist[1]&pp_ff.hist[0]) | (!pp_ff.hist[0]&actual_taken);
   assign newhist[0] = (!pp_ff.hist[1]&!actual_taken) | (pp_ff.hist[1]&actual_taken);
   always_comb begin
      predict_p_ff = pp_ff;
      predict_p_ff.misp    = (valid_ff) ? (cond_mispredict | target_mispredict) & ~flush : pp_ff.misp;
      predict_p_ff.ataken  = (valid_ff) ? actual_taken : pp_ff.ataken;
      predict_p_ff.hist[1] = (valid_ff) ? newhist[1] : pp_ff.hist[1];
      predict_p_ff.hist[0] = (valid_ff) ? newhist[0] : pp_ff.hist[0];
   end
endmodule  
module exu_mul_ctl
   import swerv_types::*;
(
   input logic         clk,               
   input logic         active_clk,        
   input logic         clk_override,      
   input logic         rst_l,             
   input logic         scan_mode,         
   input logic [31:0]  a,                 
   input logic [31:0]  b,                 
   input logic [31:0]  lsu_result_dc3,    
   input logic         freeze,            
   input mul_pkt_t     mp,                
   output logic [31:0] out                
   );
   logic                valid_e1, valid_e2;
   logic                mul_c1_e1_clken,   mul_c1_e2_clken,   mul_c1_e3_clken;
   logic                exu_mul_c1_e1_clk, exu_mul_c1_e2_clk, exu_mul_c1_e3_clk;
   logic        [31:0]  a_ff_e1, a_e1;
   logic        [31:0]  b_ff_e1, b_e1;
   logic                load_mul_rs1_bypass_e1, load_mul_rs2_bypass_e1;
   logic                rs1_sign_e1, rs1_neg_e1;
   logic                rs2_sign_e1, rs2_neg_e1;
   logic signed [32:0]  a_ff_e2, b_ff_e2;
   logic        [63:0]  prod_e3;
   logic                low_e1, low_e2, low_e3;
   assign mul_c1_e1_clken        = (mp.valid | clk_override) & ~freeze;
   assign mul_c1_e2_clken        = (valid_e1 | clk_override) & ~freeze;
   assign mul_c1_e3_clken        = (valid_e2 | clk_override) & ~freeze;
   rvdffs      #(1)  valid_e1_ff      (.*, .din(mp.valid),                  .dout(valid_e1),               .clk(active_clk),        .en(~freeze));
   rvdff_fpga  #(1)  rs1_sign_e1_ff   (.*, .din(mp.rs1_sign),               .dout(rs1_sign_e1),            .clk(exu_mul_c1_e1_clk), .clken(mul_c1_e1_clken), .rawclk(clk));
   rvdff_fpga  #(1)  rs2_sign_e1_ff   (.*, .din(mp.rs2_sign),               .dout(rs2_sign_e1),            .clk(exu_mul_c1_e1_clk), .clken(mul_c1_e1_clken), .rawclk(clk));
   rvdff_fpga  #(1)  low_e1_ff        (.*, .din(mp.low),                    .dout(low_e1),                 .clk(exu_mul_c1_e1_clk), .clken(mul_c1_e1_clken), .rawclk(clk));
   rvdff_fpga  #(1)  ld_rs1_byp_e1_ff (.*, .din(mp.load_mul_rs1_bypass_e1), .dout(load_mul_rs1_bypass_e1), .clk(exu_mul_c1_e1_clk), .clken(mul_c1_e1_clken), .rawclk(clk));
   rvdff_fpga  #(1)  ld_rs2_byp_e1_ff (.*, .din(mp.load_mul_rs2_bypass_e1), .dout(load_mul_rs2_bypass_e1), .clk(exu_mul_c1_e1_clk), .clken(mul_c1_e1_clken), .rawclk(clk));
   rvdffe  #(32) a_e1_ff          (.*, .din(a[31:0]),                   .dout(a_ff_e1[31:0]),          .en(mul_c1_e1_clken));
   rvdffe  #(32) b_e1_ff          (.*, .din(b[31:0]),                   .dout(b_ff_e1[31:0]),          .en(mul_c1_e1_clken));
   assign a_e1[31:0]             = (load_mul_rs1_bypass_e1)  ?  lsu_result_dc3[31:0]  :  a_ff_e1[31:0];
   assign b_e1[31:0]             = (load_mul_rs2_bypass_e1)  ?  lsu_result_dc3[31:0]  :  b_ff_e1[31:0];
   assign rs1_neg_e1             =  rs1_sign_e1 & a_e1[31];
   assign rs2_neg_e1             =  rs2_sign_e1 & b_e1[31];
   rvdffs       #(1)  valid_e2_ff      (.*, .din(valid_e1),                  .dout(valid_e2),          .clk(active_clk),        .en(~freeze));
   rvdff_fpga   #(1)    low_e2_ff      (.*, .din(low_e1),                    .dout(low_e2),            .clk(exu_mul_c1_e2_clk), .clken(mul_c1_e2_clken), .rawclk(clk));
   rvdffe  #(33) a_e2_ff          (.*, .din({rs1_neg_e1, a_e1[31:0]}),  .dout(a_ff_e2[32:0]),          .en(mul_c1_e2_clken));
   rvdffe  #(33) b_e2_ff          (.*, .din({rs2_neg_e1, b_e1[31:0]}),  .dout(b_ff_e2[32:0]),          .en(mul_c1_e2_clken));
   logic signed [65:0]  prod_e2;
   assign prod_e2[65:0]          =  a_ff_e2  *  b_ff_e2;
   rvdff_fpga  #(1)    low_e3_ff      (.*, .din(low_e2),                    .dout(low_e3),                 .clk(exu_mul_c1_e3_clk), .clken(mul_c1_e3_clken), .rawclk(clk));
   rvdffe      #(64) prod_e3_ff       (.*, .din(prod_e2[63:0]),             .dout(prod_e3[63:0]),          .en(mul_c1_e3_clken));
   assign out[31:0]            = low_e3  ?  prod_e3[31:0]  :  prod_e3[63:32];
endmodule  
module exu_div_ctl
   import swerv_types::*;
(
   input logic         clk,                        
   input logic         active_clk,                 
   input logic         rst_l,                      
   input logic         scan_mode,                  
   input logic         dec_tlu_fast_div_disable,   
   input logic  [31:0] dividend,                   
   input logic  [31:0] divisor,                    
   input div_pkt_t     dp,                         
   input logic         flush_lower,                
   output logic        valid_ff_e1,                
   output logic        finish_early,               
   output logic        finish,                     
   output logic        div_stall,                  
   output logic [31:0] out                         
  );
   logic         run_in, run_state;
   logic [5:0]   count_in, count;
   logic [32:0]  m_ff;
   logic         qff_enable;
   logic         aff_enable;
   logic [32:0]  q_in, q_ff;
   logic [32:0]  a_in, a_ff;
   logic [32:0]  m_eff;
   logic [32:0]  a_shift;
   logic         dividend_neg_ff, divisor_neg_ff;
   logic [31:0]  dividend_comp;
   logic [31:0]  dividend_eff;
   logic [31:0]  q_ff_comp;
   logic [31:0]  q_ff_eff;
   logic [31:0]  a_ff_comp;
   logic [31:0]  a_ff_eff;
   logic         sign_ff, sign_eff;
   logic         rem_ff;
   logic         add;
   logic [32:0]  a_eff;
   logic [64:0]  a_eff_shift;
   logic         rem_correct;
   logic         flush_lower_ff;
   logic         valid_e1;
   logic         smallnum_case, smallnum_case_ff;
   logic [3:0]   smallnum, smallnum_ff;
   logic         m_already_comp;
   rvdff  #(1)  flush_any_ff      (.*, .clk(active_clk), .din(flush_lower),                                .dout(flush_lower_ff));
   rvdff  #(1)  e1val_ff          (.*, .clk(active_clk), .din(dp.valid & ~flush_lower_ff),                 .dout(valid_ff_e1));
   rvdff  #(1)  runff             (.*, .clk(active_clk), .din(run_in),                                     .dout(run_state));
   rvdff  #(6)  countff           (.*, .clk(active_clk), .din(count_in[5:0]),                              .dout(count[5:0]));
   rvdffs #(4)  miscf             (.*, .clk(active_clk), .din({dividend[31],divisor[31],sign_eff,dp.rem}), .dout({dividend_neg_ff,divisor_neg_ff,sign_ff,rem_ff}), .en(dp.valid));
   rvdff  #(5)  smallnumff        (.*, .clk(active_clk), .din({smallnum_case,smallnum[3:0]}),              .dout({smallnum_case_ff,smallnum_ff[3:0]}));
   rvdffe #(33) mff               (.*, .en(dp.valid),    .din({ ~dp.unsign & divisor[31], divisor[31:0]}), .dout(m_ff[32:0]));
   rvdffe #(33) qff               (.*, .en(qff_enable),  .din(q_in[32:0]),                                 .dout(q_ff[32:0]));
   rvdffe #(33) aff               (.*, .en(aff_enable),  .din(a_in[32:0]),                                 .dout(a_ff[32:0]));
   rvtwoscomp #(32) dividend_c    (.din(q_ff[31:0]), .dout(dividend_comp[31:0]));
   rvtwoscomp #(32) q_ff_c        (.din(q_ff[31:0]), .dout(q_ff_comp[31:0]));
   rvtwoscomp #(32) a_ff_c        (.din(a_ff[31:0]), .dout(a_ff_comp[31:0]));
   assign valid_e1                = valid_ff_e1 & ~flush_lower_ff;
   assign smallnum_case           = ((q_ff[31:4] == 28'b0) & (m_ff[31:4] == 28'b0) & (m_ff[31:0] != 32'b0) & ~rem_ff & valid_e1 & ~dec_tlu_fast_div_disable) |
                                    ((q_ff[31:0] == 32'b0) &                         (m_ff[31:0] != 32'b0) & ~rem_ff & valid_e1 & ~dec_tlu_fast_div_disable);
   assign smallnum[3]             = ( q_ff[3] &                                  ~m_ff[3] & ~m_ff[2] & ~m_ff[1]           );
   assign smallnum[2]             = ( q_ff[3] &                                  ~m_ff[3] & ~m_ff[2] &            ~m_ff[0]) |
                                    ( q_ff[2] &                                  ~m_ff[3] & ~m_ff[2] & ~m_ff[1]           ) |
                                    ( q_ff[3] &  q_ff[2] &                       ~m_ff[3] & ~m_ff[2]                      );
   assign smallnum[1]             = ( q_ff[2] &                                  ~m_ff[3] & ~m_ff[2] &            ~m_ff[0]) |
                                    (                       q_ff[1] &            ~m_ff[3] & ~m_ff[2] & ~m_ff[1]           ) |
                                    ( q_ff[3] &                                  ~m_ff[3] &            ~m_ff[1] & ~m_ff[0]) |
                                    ( q_ff[3] & ~q_ff[2] &                       ~m_ff[3] & ~m_ff[2] &  m_ff[1] &  m_ff[0]) |
                                    (~q_ff[3] &  q_ff[2] &  q_ff[1] &            ~m_ff[3] & ~m_ff[2]                      ) |
                                    ( q_ff[3] &  q_ff[2] &                       ~m_ff[3] &                       ~m_ff[0]) |
                                    ( q_ff[3] &  q_ff[2] &                       ~m_ff[3] &  m_ff[2] & ~m_ff[1]           ) |
                                    ( q_ff[3] &             q_ff[1] & ~m_ff[3] &                       ~m_ff[1]           ) |
                                    ( q_ff[3] &  q_ff[2] &  q_ff[1] &            ~m_ff[3] &  m_ff[2]                      );
   assign smallnum[0]             = (            q_ff[2] &  q_ff[1] &  q_ff[0] & ~m_ff[3] &            ~m_ff[1]           ) |
                                    ( q_ff[3] & ~q_ff[2] &  q_ff[0] &            ~m_ff[3] &             m_ff[1] &  m_ff[0]) |
                                    (            q_ff[2] &                       ~m_ff[3] &            ~m_ff[1] & ~m_ff[0]) |
                                    (                       q_ff[1] &            ~m_ff[3] & ~m_ff[2] &            ~m_ff[0]) |
                                    (                                  q_ff[0] & ~m_ff[3] & ~m_ff[2] & ~m_ff[1]           ) |
                                    (~q_ff[3] &  q_ff[2] & ~q_ff[1] &            ~m_ff[3] & ~m_ff[2] &  m_ff[1] &  m_ff[0]) |
                                    (~q_ff[3] &  q_ff[2] &  q_ff[1] &            ~m_ff[3] &                       ~m_ff[0]) |
                                    ( q_ff[3] &                                             ~m_ff[2] & ~m_ff[1] & ~m_ff[0]) |
                                    ( q_ff[3] & ~q_ff[2] &                       ~m_ff[3] &  m_ff[2] &  m_ff[1]           ) |
                                    (~q_ff[3] &  q_ff[2] &  q_ff[1] &            ~m_ff[3] &  m_ff[2] & ~m_ff[1]           ) |
                                    (~q_ff[3] &  q_ff[2] &             q_ff[0] & ~m_ff[3] &            ~m_ff[1]           ) |
                                    ( q_ff[3] & ~q_ff[2] & ~q_ff[1] &            ~m_ff[3] &  m_ff[2] &             m_ff[0]) |
                                    (           ~q_ff[2] &  q_ff[1] &  q_ff[0] & ~m_ff[3] & ~m_ff[2]                      ) |
                                    ( q_ff[3] &  q_ff[2] &                                             ~m_ff[1] & ~m_ff[0]) |
                                    ( q_ff[3] &             q_ff[1] &                       ~m_ff[2] &            ~m_ff[0]) |
                                    (~q_ff[3] &  q_ff[2] &  q_ff[1] &  q_ff[0] & ~m_ff[3] &  m_ff[2]                      ) |
                                    ( q_ff[3] &  q_ff[2] &                        m_ff[3] & ~m_ff[2]                      ) |
                                    ( q_ff[3] &             q_ff[1] &             m_ff[3] & ~m_ff[2] & ~m_ff[1]           ) |
                                    ( q_ff[3] &                        q_ff[0] &            ~m_ff[2] & ~m_ff[1]           ) |
                                    ( q_ff[3] &            ~q_ff[1] &            ~m_ff[3] &  m_ff[2] &  m_ff[1] &  m_ff[0]) |
                                    ( q_ff[3] &  q_ff[2] &  q_ff[1] &             m_ff[3] &                       ~m_ff[0]) |
                                    ( q_ff[3] &  q_ff[2] &  q_ff[1] &             m_ff[3] &            ~m_ff[1]           ) |
                                    ( q_ff[3] &  q_ff[2] &             q_ff[0] &  m_ff[3] &            ~m_ff[1]           ) |
                                    ( q_ff[3] & ~q_ff[2] &  q_ff[1] &            ~m_ff[3] &             m_ff[1]           ) |
                                    ( q_ff[3] &             q_ff[1] &  q_ff[0] &            ~m_ff[2]                      ) |
                                    ( q_ff[3] &  q_ff[2] &  q_ff[1] &  q_ff[0] &  m_ff[3]                                 );
   logic [4:0]   a_cls;
   logic [4:0]   b_cls;
   logic [5:0]   shortq;
   logic [5:0]   shortq_shift;
   logic [5:0]   shortq_shift_ff;
   logic         shortq_enable;
   logic         shortq_enable_ff;
   logic [32:0]  short_dividend;
   assign short_dividend[31:0]    =  q_ff[31:0];
   assign short_dividend[32]      =  sign_ff & q_ff[31];
   logic [3:0]   shortq_raw;
   logic [3:0]   shortq_shift_xx;
   assign a_cls[4:3]              =  2'b0;
   assign a_cls[2]                =  (~short_dividend[32] & (short_dividend[31:24] != {8{1'b0}})) | ( short_dividend[32] & (short_dividend[31:23] != {9{1'b1}}));
   assign a_cls[1]                =  (~short_dividend[32] & (short_dividend[23:16] != {8{1'b0}})) | ( short_dividend[32] & (short_dividend[22:15] != {8{1'b1}}));
   assign a_cls[0]                =  (~short_dividend[32] & (short_dividend[15:08] != {8{1'b0}})) | ( short_dividend[32] & (short_dividend[14:07] != {8{1'b1}}));
   assign b_cls[4:3]              =  2'b0;
   assign b_cls[2]                =  (~m_ff[32]           & (          m_ff[31:24] != {8{1'b0}})) | ( m_ff[32]           & (          m_ff[31:24] != {8{1'b1}}));
   assign b_cls[1]                =  (~m_ff[32]           & (          m_ff[23:16] != {8{1'b0}})) | ( m_ff[32]           & (          m_ff[23:16] != {8{1'b1}}));
   assign b_cls[0]                =  (~m_ff[32]           & (          m_ff[15:08] != {8{1'b0}})) | ( m_ff[32]           & (          m_ff[15:08] != {8{1'b1}}));
   assign shortq_raw[3]           = ( (a_cls[2:1] == 2'b01 ) & (b_cls[2]   == 1'b1  ) ) |    
                                    ( (a_cls[2:0] == 3'b001) & (b_cls[2]   == 1'b1  ) ) |
                                    ( (a_cls[2:0] == 3'b000) & (b_cls[2]   == 1'b1  ) ) |
                                    ( (a_cls[2:0] == 3'b001) & (b_cls[2:1] == 2'b01 ) ) |
                                    ( (a_cls[2:0] == 3'b000) & (b_cls[2:1] == 2'b01 ) ) |
                                    ( (a_cls[2:0] == 3'b000) & (b_cls[2:0] == 3'b001) );
   assign shortq_raw[2]           = ( (a_cls[2]   == 1'b1  ) & (b_cls[2]   == 1'b1  ) ) |    
                                    ( (a_cls[2:1] == 2'b01 ) & (b_cls[2:1] == 2'b01 ) ) |
                                    ( (a_cls[2:0] == 3'b001) & (b_cls[2:0] == 3'b001) ) |
                                    ( (a_cls[2:0] == 3'b000) & (b_cls[2:0] == 3'b000) );
   assign shortq_raw[1]           = ( (a_cls[2]   == 1'b1  ) & (b_cls[2:1] == 2'b01 ) ) |    
                                    ( (a_cls[2:1] == 2'b01 ) & (b_cls[2:0] == 3'b001) ) |
                                    ( (a_cls[2:0] == 3'b001) & (b_cls[2:0] == 3'b000) );
   assign shortq_raw[0]           = ( (a_cls[2]   == 1'b1  ) & (b_cls[2:0] == 3'b001) ) |    
                                    ( (a_cls[2:1] == 2'b01 ) & (b_cls[2:0] == 3'b000) );
   assign shortq_enable           =  valid_ff_e1 & (m_ff[31:0] != 32'b0) & (shortq_raw[3:0] != 4'b0);
   assign shortq_shift[3:0]       = ({4{shortq_enable}} & shortq_raw[3:0]);
   rvdff  #(5)  i_shortq_ff       (.*, .clk(active_clk), .din({shortq_enable,shortq_shift[3:0]}), .dout({shortq_enable_ff,shortq_shift_xx[3:0]}));
   assign shortq_shift_ff[5:0]    = ({6{shortq_shift_xx[3]}} & 6'b01_1111) |    
                                    ({6{shortq_shift_xx[2]}} & 6'b01_1000) |    
                                    ({6{shortq_shift_xx[1]}} & 6'b01_0000) |    
                                    ({6{shortq_shift_xx[0]}} & 6'b00_1000);     
   assign div_stall               =  run_state;
   assign run_in                  = (dp.valid | run_state) & ~finish & ~flush_lower_ff;
   assign count_in[5:0]           = {6{run_state & ~finish & ~flush_lower_ff & ~shortq_enable}} & (count[5:0] + shortq_shift_ff[5:0] + 6'd1);
   assign finish_early            =  smallnum_case;
   assign finish                  = (smallnum_case | ((~rem_ff) ? (count[5:0] == 6'd32) : (count[5:0] == 6'd33))) & ~flush_lower & ~flush_lower_ff;
   assign sign_eff                = ~dp.unsign & (divisor[31:0] != 32'b0);
   assign q_in[32:0]              = ({33{~run_state                                   }} &  {1'b0,dividend[31:0]}) |
                                    ({33{ run_state &  (valid_ff_e1 | shortq_enable_ff)}} &  ({dividend_eff[31:0], ~a_in[32]} << shortq_shift_ff[5:0])) |
                                    ({33{ run_state & ~(valid_ff_e1 | shortq_enable_ff)}} &  {q_ff[31:0], ~a_in[32]});
   assign qff_enable              =  dp.valid | (run_state & ~shortq_enable);
   assign dividend_eff[31:0]      = (sign_ff & dividend_neg_ff) ? dividend_comp[31:0] : q_ff[31:0];
   assign m_eff[32:0]             = (add) ? m_ff[32:0] : ~m_ff[32:0];
   assign a_eff_shift[64:0]       = {33'b0, dividend_eff[31:0]} << shortq_shift_ff[5:0];
   assign a_eff[32:0]             = ({33{ rem_correct                    }} &  a_ff[32:0]           ) |
                                    ({33{~rem_correct & ~shortq_enable_ff}} & {a_ff[31:0], q_ff[32]}) |
                                    ({33{~rem_correct &  shortq_enable_ff}} &  a_eff_shift[64:32]   );
   assign a_shift[32:0]           = {33{run_state}} & a_eff[32:0];
   assign a_in[32:0]              = {33{run_state}} & (a_shift[32:0] + m_eff[32:0] + {32'b0,~add});
   assign aff_enable              =  dp.valid | (run_state & ~shortq_enable & (count[5:0]!=6'd33)) | rem_correct;
   assign m_already_comp          = (divisor_neg_ff & sign_ff);
   assign add                     = (a_ff[32] | rem_correct) ^ m_already_comp;
   assign rem_correct             = (count[5:0] == 6'd33) & rem_ff & a_ff[32];
   assign q_ff_eff[31:0]          = (sign_ff & (dividend_neg_ff ^ divisor_neg_ff)) ? q_ff_comp[31:0] : q_ff[31:0];
   assign a_ff_eff[31:0]          = (sign_ff &  dividend_neg_ff) ? a_ff_comp[31:0] : a_ff[31:0];
   assign out[31:0]               = ({32{ smallnum_case_ff          }} & {28'b0, smallnum_ff[3:0]}) |
                                    ({32{                     rem_ff}} &  a_ff_eff[31:0]          ) |
                                    ({32{~smallnum_case_ff & ~rem_ff}} &  q_ff_eff[31:0]          );
endmodule  
module exu
   import swerv_types::*;
(
   input logic clk,                                                     
   input logic active_clk,                                              
   input logic clk_override,                                            
   input logic rst_l,                                                   
   input logic scan_mode,                                               
   input logic lsu_freeze_dc3,                                          
   input logic  dec_tlu_fast_div_disable,                               
   input logic [4:2] dec_i0_data_en,                                    
   input logic [4:1] dec_i0_ctl_en,                                     
   input logic [4:2] dec_i1_data_en,                                    
   input logic [4:1] dec_i1_ctl_en,                                     
   input logic dec_debug_wdata_rs1_d,                                   
   input logic [31:0] dbg_cmd_wrdata,                                   
   input logic [31:0] lsu_result_dc3,                                   
   input predict_pkt_t  i0_predict_p_d,                                 
   input predict_pkt_t  i1_predict_p_d,                                 
   input logic        dec_i0_rs1_bypass_en_e2,                          
   input logic        dec_i0_rs2_bypass_en_e2,                          
   input logic        dec_i1_rs1_bypass_en_e2,                          
   input logic        dec_i1_rs2_bypass_en_e2,                          
   input logic [31:0] i0_rs1_bypass_data_e2,                            
   input logic [31:0] i0_rs2_bypass_data_e2,                            
   input logic [31:0] i1_rs1_bypass_data_e2,                            
   input logic [31:0] i1_rs2_bypass_data_e2,                            
   input logic        dec_i0_rs1_bypass_en_e3,                          
   input logic        dec_i0_rs2_bypass_en_e3,                          
   input logic        dec_i1_rs1_bypass_en_e3,                          
   input logic        dec_i1_rs2_bypass_en_e3,                          
   input logic [31:0] i0_rs1_bypass_data_e3,                            
   input logic [31:0] i0_rs2_bypass_data_e3,                            
   input logic [31:0] i1_rs1_bypass_data_e3,                            
   input logic [31:0] i1_rs2_bypass_data_e3,                            
   input logic        dec_i0_sec_decode_e3,                             
   input logic        dec_i1_sec_decode_e3,                             
   input logic [31:1] dec_i0_pc_e3,                                     
   input logic [31:1] dec_i1_pc_e3,                                     
   input logic [31:1] pred_correct_npc_e2,                              
   input logic        dec_i1_valid_e1,                                  
   input logic        dec_i0_mul_d,                                     
   input logic        dec_i1_mul_d,                                     
   input logic        dec_i0_div_d,                                     
   input logic        dec_i1_div_d,                                     
   input logic [31:0] gpr_i0_rs1_d,                                     
   input logic [31:0] gpr_i0_rs2_d,                                     
   input logic [31:0] dec_i0_immed_d,                                   
   input logic [31:0] gpr_i1_rs1_d,                                     
   input logic [31:0] gpr_i1_rs2_d,                                     
   input logic [31:0] dec_i1_immed_d,                                   
   input logic [31:0] i0_rs1_bypass_data_d,                             
   input logic [31:0] i0_rs2_bypass_data_d,                             
   input logic [31:0] i1_rs1_bypass_data_d,                             
   input logic [31:0] i1_rs2_bypass_data_d,                             
   input logic [12:1] dec_i0_br_immed_d,                                
   input logic [12:1] dec_i1_br_immed_d,                                
   input         alu_pkt_t i0_ap,                                       
   input         alu_pkt_t i1_ap,                                       
   input logic   dec_i0_alu_decode_d,                                   
   input logic   dec_i1_alu_decode_d,                                   
   input logic   dec_i0_select_pc_d,                                    
   input logic   dec_i1_select_pc_d,                                    
   input logic [31:1] dec_i0_pc_d, dec_i1_pc_d,                         
   input logic  dec_i0_rs1_bypass_en_d,                                 
   input logic  dec_i0_rs2_bypass_en_d,                                 
   input logic  dec_i1_rs1_bypass_en_d,                                 
   input logic  dec_i1_rs2_bypass_en_d,                                 
   input logic        dec_tlu_flush_lower_wb,                           
   input logic [31:1] dec_tlu_flush_path_wb,                            
   input logic dec_tlu_i0_valid_e4,                                     
   input logic dec_tlu_i1_valid_e4,                                     
   output logic [31:0] exu_i0_result_e1,                                
   output logic [31:0] exu_i1_result_e1,                                
   output logic [31:1] exu_i0_pc_e1,                                    
   output logic [31:1] exu_i1_pc_e1,                                    
   output logic [31:0] exu_i0_result_e4,                                
   output logic [31:0] exu_i1_result_e4,                                
   output logic        exu_i0_flush_final,                              
   output logic        exu_i1_flush_final,                              
   input mul_pkt_t  mul_p,                                              
   input div_pkt_t  div_p,                                              
   input logic   dec_i0_lsu_d,                                          
   input logic   dec_i1_lsu_d,                                          
   input logic   dec_csr_ren_d,                                         
   output logic [31:0] exu_lsu_rs1_d,                                   
   output logic [31:0] exu_lsu_rs2_d,                                   
   output logic [31:0] exu_csr_rs1_e1,                                  
   output logic         exu_flush_final,                                
   output logic [31:1]  exu_flush_path_final,                           
   output logic [31:0] exu_mul_result_e3,                               
   output logic [31:0]  exu_div_result,                                 
   output logic exu_div_finish,                                         
   output logic exu_div_stall,                                          
   output logic [31:1] exu_npc_e4,                                      
   output logic exu_i0_flush_lower_e4,                                  
   output logic exu_i1_flush_lower_e4,                                  
   output logic [31:1] exu_i0_flush_path_e4,                            
   output logic [31:1] exu_i1_flush_path_e4,                            
   output predict_pkt_t exu_mp_pkt,                                     
   output logic [4:0]  exu_mp_eghr,                       
   output logic [1:0]  exu_i0_br_hist_e4,                               
   output logic [1:0]  exu_i0_br_bank_e4,                               
   output logic        exu_i0_br_error_e4,                              
   output logic        exu_i0_br_start_error_e4,                        
   output logic [5:4] exu_i0_br_index_e4,   
   output logic        exu_i0_br_valid_e4,                              
   output logic        exu_i0_br_mp_e4,                                 
   output logic        exu_i0_br_way_e4,                                
   output logic        exu_i0_br_middle_e4,                             
   output logic [4:0]  exu_i0_br_fghr_e4,                 
   output logic        exu_i0_br_ret_e4,                                
   output logic        exu_i0_br_call_e4,                               
   output logic [1:0]  exu_i1_br_hist_e4,                               
   output logic [1:0]  exu_i1_br_bank_e4,                               
   output logic        exu_i1_br_error_e4,                              
   output logic        exu_i1_br_start_error_e4,                        
   output logic [5:4] exu_i1_br_index_e4,   
   output logic        exu_i1_br_valid_e4,                              
   output logic        exu_i1_br_mp_e4,                                 
   output logic        exu_i1_br_way_e4,                                
   output logic        exu_i1_br_middle_e4,                             
   output logic [4:0]  exu_i1_br_fghr_e4,                 
   output logic        exu_i1_br_ret_e4,                                
   output logic        exu_i1_br_call_e4,                               
   output logic        exu_flush_upper_e2,                              
   output rets_pkt_t exu_rets_e1_pkt,                                   
   output rets_pkt_t exu_rets_e4_pkt,                                   
   output logic exu_pmu_i0_br_misp,                                     
   output logic exu_pmu_i0_br_ataken,                                   
   output logic exu_pmu_i0_pc4,                                         
   output logic exu_pmu_i1_br_misp,                                     
   output logic exu_pmu_i1_br_ataken,                                   
   output logic exu_pmu_i1_pc4                                          
   );
   logic [31:0] i0_rs1_d,i0_rs2_d,i1_rs1_d,i1_rs2_d;
   logic        exu_i0_flush_upper_e1;
   logic [31:1] exu_i0_flush_path_e1;
   logic        exu_i1_flush_upper_e1;
   logic [31:1] exu_i1_flush_path_e1;
   logic [31:0] i0_rs1_final_d;
   logic [31:1]  exu_flush_path_e2;
   logic [31:0]  mul_rs1_d, mul_rs2_d;
   logic [31:0]  div_rs1_d, div_rs2_d;
   logic        i1_valid_e2;
   logic [31:1] npc_e4;
   logic [31:1] div_npc;
   logic [31:0] i0_rs1_e1, i0_rs2_e1;
   logic [31:0] i0_rs1_e2, i0_rs2_e2;
   logic [31:0] i0_rs1_e3, i0_rs2_e3;
   logic [12:1] i0_br_immed_e1, i0_br_immed_e2, i0_br_immed_e3;
   logic [31:0] i1_rs1_e1, i1_rs2_e1;
   logic [31:0] i1_rs1_e2, i1_rs2_e2;
   logic [31:0] i1_rs1_e3, i1_rs2_e3;
   logic [12:1] i1_br_immed_e1, i1_br_immed_e2, i1_br_immed_e3;
   logic [31:0] i0_rs1_e2_final, i0_rs2_e2_final;
   logic [31:0] i1_rs1_e2_final, i1_rs2_e2_final;
   logic [31:0] i0_rs1_e3_final, i0_rs2_e3_final;
   logic [31:0] i1_rs1_e3_final, i1_rs2_e3_final;
   logic [31:1] i0_alu_pc_nc, i1_alu_pc_nc;
   logic [31:1] exu_flush_path_e1;
   logic        exu_i0_flush_upper_e2, exu_i1_flush_upper_e2;
   logic        i1_valid_e3, i1_valid_e4;
   logic [31:1] pred_correct_npc_e3, pred_correct_npc_e4;
   logic                                  exu_i0_flush_upper_e3;
   logic                                  exu_i0_flush_upper_e4;
   logic        i1_pred_correct_upper_e1, i0_pred_correct_upper_e1;
   logic        i1_pred_correct_upper_e2, i0_pred_correct_upper_e2;
   logic        i1_pred_correct_upper_e3, i0_pred_correct_upper_e3;
   logic        i1_pred_correct_upper_e4, i0_pred_correct_upper_e4;
   logic        i1_pred_correct_lower_e4, i0_pred_correct_lower_e4;
   logic        i1_valid_e4_eff;
   logic        i1_sec_decode_e4, i0_sec_decode_e4;
   logic        i1_pred_correct_e4_eff, i0_pred_correct_e4_eff;
   logic [31:1] i1_flush_path_e4_eff, i0_flush_path_e4_eff;
   logic [31:0] csr_rs1_in_d;
   logic [31:1] i1_flush_path_upper_e2, i0_flush_path_upper_e2;
   logic [31:1] i1_flush_path_upper_e3, i0_flush_path_upper_e3;
   logic [31:1] i1_flush_path_upper_e4, i0_flush_path_upper_e4;
   logic        div_valid_e1;
   logic        div_finish_early;
   logic        freeze;
   alu_pkt_t i0_ap_e1, i0_ap_e2, i0_ap_e3, i0_ap_e4;
   alu_pkt_t i1_ap_e1, i1_ap_e2, i1_ap_e3, i1_ap_e4;
   assign freeze = lsu_freeze_dc3;
   assign i0_rs1_d[31:0] = ({32{~dec_i0_rs1_bypass_en_d}} & ((dec_debug_wdata_rs1_d) ? dbg_cmd_wrdata[31:0] : gpr_i0_rs1_d[31:0])) |
                           ({32{~dec_i0_rs1_bypass_en_d   & dec_i0_select_pc_d}} & { dec_i0_pc_d[31:1], 1'b0}) |     
                           ({32{ dec_i0_rs1_bypass_en_d}} & i0_rs1_bypass_data_d[31:0]);
   assign i0_rs1_final_d[31:0] = ({32{~dec_csr_ren_d}} & i0_rs1_d[31:0]);
   assign i0_rs2_d[31:0]       = ({32{~dec_i0_rs2_bypass_en_d}} & gpr_i0_rs2_d[31:0]) |
                                 ({32{~dec_i0_rs2_bypass_en_d}} & dec_i0_immed_d[31:0]) |
                                 ({32{ dec_i0_rs2_bypass_en_d}} & i0_rs2_bypass_data_d[31:0]);
   assign i1_rs1_d[31:0]       = ({32{~dec_i1_rs1_bypass_en_d}} & gpr_i1_rs1_d[31:0]) |
                                 ({32{~dec_i1_rs1_bypass_en_d   & dec_i1_select_pc_d}} & { dec_i1_pc_d[31:1], 1'b0}) |   
                                 ({32{ dec_i1_rs1_bypass_en_d}} & i1_rs1_bypass_data_d[31:0]);
   assign i1_rs2_d[31:0]       = ({32{~dec_i1_rs2_bypass_en_d}} & gpr_i1_rs2_d[31:0]) |
                                 ({32{~dec_i1_rs2_bypass_en_d}} & dec_i1_immed_d[31:0]) |
                                 ({32{ dec_i1_rs2_bypass_en_d}} & i1_rs2_bypass_data_d[31:0]);
   assign exu_lsu_rs1_d[31:0]  = ({32{ ~dec_i0_rs1_bypass_en_d &  dec_i0_lsu_d               }} & gpr_i0_rs1_d[31:0]        ) |
                                 ({32{ ~dec_i1_rs1_bypass_en_d & ~dec_i0_lsu_d & dec_i1_lsu_d}} & gpr_i1_rs1_d[31:0]        ) |
                                 ({32{  dec_i0_rs1_bypass_en_d &  dec_i0_lsu_d               }} & i0_rs1_bypass_data_d[31:0]) |
                                 ({32{  dec_i1_rs1_bypass_en_d & ~dec_i0_lsu_d & dec_i1_lsu_d}} & i1_rs1_bypass_data_d[31:0]);
   assign exu_lsu_rs2_d[31:0]  = ({32{ ~dec_i0_rs2_bypass_en_d &  dec_i0_lsu_d               }} & gpr_i0_rs2_d[31:0]        ) |
                                 ({32{ ~dec_i1_rs2_bypass_en_d & ~dec_i0_lsu_d & dec_i1_lsu_d}} & gpr_i1_rs2_d[31:0]        ) |
                                 ({32{  dec_i0_rs2_bypass_en_d &  dec_i0_lsu_d               }} & i0_rs2_bypass_data_d[31:0]) |
                                 ({32{  dec_i1_rs2_bypass_en_d & ~dec_i0_lsu_d & dec_i1_lsu_d}} & i1_rs2_bypass_data_d[31:0]);
   assign mul_rs1_d[31:0]      = ({32{ ~dec_i0_rs1_bypass_en_d &  dec_i0_mul_d               }} & gpr_i0_rs1_d[31:0]        ) |
                                 ({32{ ~dec_i1_rs1_bypass_en_d & ~dec_i0_mul_d & dec_i1_mul_d}} & gpr_i1_rs1_d[31:0]        ) |
                                 ({32{  dec_i0_rs1_bypass_en_d &  dec_i0_mul_d               }} & i0_rs1_bypass_data_d[31:0]) |
                                 ({32{  dec_i1_rs1_bypass_en_d & ~dec_i0_mul_d & dec_i1_mul_d}} & i1_rs1_bypass_data_d[31:0]);
   assign mul_rs2_d[31:0]      = ({32{ ~dec_i0_rs2_bypass_en_d &  dec_i0_mul_d               }} & gpr_i0_rs2_d[31:0]        ) |
                                 ({32{ ~dec_i1_rs2_bypass_en_d & ~dec_i0_mul_d & dec_i1_mul_d}} & gpr_i1_rs2_d[31:0]        ) |
                                 ({32{  dec_i0_rs2_bypass_en_d &  dec_i0_mul_d               }} & i0_rs2_bypass_data_d[31:0]) |
                                 ({32{  dec_i1_rs2_bypass_en_d & ~dec_i0_mul_d & dec_i1_mul_d}} & i1_rs2_bypass_data_d[31:0]);
   assign div_rs1_d[31:0]      = ({32{ ~dec_i0_rs1_bypass_en_d &  dec_i0_div_d               }} & gpr_i0_rs1_d[31:0]) |
                                 ({32{ ~dec_i1_rs1_bypass_en_d & ~dec_i0_div_d & dec_i1_div_d}} & gpr_i1_rs1_d[31:0]) |
                                 ({32{  dec_i0_rs1_bypass_en_d &  dec_i0_div_d               }} & i0_rs1_bypass_data_d[31:0]) |
                                 ({32{  dec_i1_rs1_bypass_en_d & ~dec_i0_div_d & dec_i1_div_d}} & i1_rs1_bypass_data_d[31:0]);
   assign div_rs2_d[31:0]      = ({32{ ~dec_i0_rs2_bypass_en_d &  dec_i0_div_d               }} & gpr_i0_rs2_d[31:0]) |
                                 ({32{ ~dec_i1_rs2_bypass_en_d & ~dec_i0_div_d & dec_i1_div_d}} & gpr_i1_rs2_d[31:0]) |
                                 ({32{  dec_i0_rs2_bypass_en_d &  dec_i0_div_d               }} & i0_rs2_bypass_data_d[31:0]) |
                                 ({32{  dec_i1_rs2_bypass_en_d & ~dec_i0_div_d & dec_i1_div_d}} & i1_rs2_bypass_data_d[31:0]);
   assign csr_rs1_in_d[31:0] = (dec_csr_ren_d) ? i0_rs1_d[31:0] : exu_csr_rs1_e1[31:0];
   logic       i0_e1_data_en, i0_e2_data_en, i0_e3_data_en;
   logic       i0_e1_ctl_en,  i0_e2_ctl_en,  i0_e3_ctl_en,  i0_e4_ctl_en;
   assign {i0_e1_data_en, i0_e2_data_en, i0_e3_data_en }                = dec_i0_data_en[4:2];
   assign {i0_e1_ctl_en,  i0_e2_ctl_en,  i0_e3_ctl_en,  i0_e4_ctl_en }  = dec_i0_ctl_en[4:1];
   logic       i1_e1_data_en, i1_e2_data_en, i1_e3_data_en;
   logic       i1_e1_ctl_en,  i1_e2_ctl_en,  i1_e3_ctl_en,  i1_e4_ctl_en;
   assign {i1_e1_data_en, i1_e2_data_en, i1_e3_data_en}                = dec_i1_data_en[4:2];
   assign {i1_e1_ctl_en,  i1_e2_ctl_en,  i1_e3_ctl_en,  i1_e4_ctl_en}  = dec_i1_ctl_en[4:1];
   rvdffe #(32) csr_rs1_ff (.*, .en(i0_e1_data_en), .din(csr_rs1_in_d[31:0]), .dout(exu_csr_rs1_e1[31:0]));
   exu_mul_ctl mul_e1    (.*,
                          .clk_override  ( clk_override                ),    
                          .freeze        ( freeze                      ),    
                          .mp            ( mul_p                       ),    
                          .a             ( mul_rs1_d[31:0]             ),    
                          .b             ( mul_rs2_d[31:0]             ),    
                          .out           ( exu_mul_result_e3[31:0]     ));   
   exu_div_ctl div_e1    (.*,
                          .flush_lower   ( dec_tlu_flush_lower_wb      ),    
                          .dp            ( div_p                       ),    
                          .dividend      ( div_rs1_d[31:0]             ),    
                          .divisor       ( div_rs2_d[31:0]             ),    
                          .valid_ff_e1   ( div_valid_e1                ),    
                          .div_stall     ( exu_div_stall               ),    
                          .finish_early  ( div_finish_early            ),    
                          .finish        ( exu_div_finish              ),    
                          .out           ( exu_div_result[31:0]        ));   
   predict_pkt_t i0_predict_newp_d, i1_predict_newp_d;
   always_comb begin
      i0_predict_newp_d = i0_predict_p_d;
      i0_predict_newp_d.boffset = dec_i0_pc_d[1];   
      i0_predict_newp_d.index[5:4] = i0_predict_p_d.index[5:4];  
      i0_predict_newp_d.bank[1:0] = i0_predict_p_d.bank[1:0];
      i1_predict_newp_d = i1_predict_p_d;
      i1_predict_newp_d.boffset = dec_i1_pc_d[1];
      i1_predict_newp_d.index[5:4] = i1_predict_p_d.index[5:4];
      i1_predict_newp_d.bank[1:0] = i1_predict_p_d.bank[1:0];
   end
   predict_pkt_t i0_predict_p_e1, i0_predict_p_e4;
   predict_pkt_t i1_predict_p_e1, i1_predict_p_e4;
   assign exu_pmu_i0_br_misp   = i0_predict_p_e4.misp   & ~exu_div_finish;   
   assign exu_pmu_i0_br_ataken = i0_predict_p_e4.ataken & ~exu_div_finish;   
   assign exu_pmu_i0_pc4       = i0_predict_p_e4.pc4 | exu_div_finish;       
   assign exu_pmu_i1_br_misp   = i1_predict_p_e4.misp;
   assign exu_pmu_i1_br_ataken = i1_predict_p_e4.ataken;
   assign exu_pmu_i1_pc4       = i1_predict_p_e4.pc4;
   exu_alu_ctl i0_alu_e1 (.*,
                          .freeze        ( freeze                      ),    
                          .enable        ( i0_e1_ctl_en                ),    
                          .predict_p     ( i0_predict_newp_d           ),    
                          .valid         ( dec_i0_alu_decode_d         ),    
                          .flush         ( exu_flush_final             ),    
                          .a             ( i0_rs1_final_d[31:0]        ),    
                          .b             ( i0_rs2_d[31:0]              ),    
                          .pc            ( dec_i0_pc_d[31:1]           ),    
                          .brimm         ( dec_i0_br_immed_d[12:1]     ),    
                          .ap            ( i0_ap_e1                    ),    
                          .out           ( exu_i0_result_e1[31:0]      ),    
                          .flush_upper   ( exu_i0_flush_upper_e1       ),    
                          .flush_path    ( exu_i0_flush_path_e1[31:1]  ),    
                          .predict_p_ff  ( i0_predict_p_e1             ),    
                          .pc_ff         ( exu_i0_pc_e1[31:1]          ),    
                          .pred_correct  ( i0_pred_correct_upper_e1    )     
                          );
   exu_alu_ctl i1_alu_e1 (.*,
                          .freeze        ( freeze                      ),    
                          .enable        ( i1_e1_ctl_en                ),    
                          .predict_p     ( i1_predict_newp_d           ),    
                          .valid         ( dec_i1_alu_decode_d         ),    
                          .flush         ( exu_flush_final             ),    
                          .a             ( i1_rs1_d[31:0]              ),    
                          .b             ( i1_rs2_d[31:0]              ),    
                          .pc            ( dec_i1_pc_d[31:1]           ),    
                          .brimm         ( dec_i1_br_immed_d[12:1]     ),    
                          .ap            ( i1_ap_e1                    ),    
                          .out           ( exu_i1_result_e1[31:0]      ),    
                          .flush_upper   ( exu_i1_flush_upper_e1       ),    
                          .flush_path    ( exu_i1_flush_path_e1[31:1]  ),    
                          .predict_p_ff  ( i1_predict_p_e1             ),    
                          .pc_ff         ( exu_i1_pc_e1[31:1]          ),    
                          .pred_correct  ( i1_pred_correct_upper_e1    )     
                          );
   predict_pkt_t i0_pp_e2, i0_pp_e3, i0_pp_e4_in;
   rvdffe #($bits(predict_pkt_t)) i0_pp_e2_ff (.*, .en(i0_e2_ctl_en), .din(i0_predict_p_e1),.dout(i0_pp_e2) );
   rvdffe #($bits(predict_pkt_t)) i0_pp_e3_ff (.*, .en(i0_e3_ctl_en), .din(i0_pp_e2),.dout(i0_pp_e3) );
   predict_pkt_t i1_pp_e2, i1_pp_e3, i1_pp_e4_in;
   rvdffe #($bits(predict_pkt_t)) i1_pp_e2_ff (.*, .en(i1_e2_ctl_en), .din(i1_predict_p_e1),.dout(i1_pp_e2) );
   rvdffe #($bits(predict_pkt_t)) i1_pp_e3_ff (.*, .en(i1_e3_ctl_en), .din(i1_pp_e2),.dout(i1_pp_e3) );
   assign i0_pp_e4_in = (freeze) ? '0 : i0_pp_e3;
   assign i1_pp_e4_in = (freeze) ? '0 : i1_pp_e3;
   rvdffe #($bits(alu_pkt_t)) i0_ap_e1_ff (.*,  .en(i0_e1_ctl_en), .din(i0_ap),   .dout(i0_ap_e1) );
   rvdffe #($bits(alu_pkt_t)) i0_ap_e2_ff (.*,  .en(i0_e2_ctl_en), .din(i0_ap_e1),.dout(i0_ap_e2) );
   rvdffe #($bits(alu_pkt_t)) i0_ap_e3_ff (.*,  .en(i0_e3_ctl_en), .din(i0_ap_e2),.dout(i0_ap_e3) );
   rvdffe #($bits(alu_pkt_t)) i0_ap_e4_ff (.*,  .en(i0_e4_ctl_en), .din(i0_ap_e3),.dout(i0_ap_e4) );
   rvdffe #($bits(alu_pkt_t)) i1_ap_e1_ff (.*,  .en(i1_e1_ctl_en), .din(i1_ap),   .dout(i1_ap_e1) );
   rvdffe #($bits(alu_pkt_t)) i1_ap_e2_ff (.*,  .en(i1_e2_ctl_en), .din(i1_ap_e1),.dout(i1_ap_e2) );
   rvdffe #($bits(alu_pkt_t)) i1_ap_e3_ff (.*,  .en(i1_e3_ctl_en), .din(i1_ap_e2),.dout(i1_ap_e3) );
   rvdffe #($bits(alu_pkt_t)) i1_ap_e4_ff (.*,  .en(i1_e4_ctl_en), .din(i1_ap_e3),.dout(i1_ap_e4) );
   assign exu_rets_e1_pkt.pc0_call = i0_predict_p_e1.pcall & i0_predict_p_e1.valid & ~i0_predict_p_e1.br_error;
   assign exu_rets_e1_pkt.pc1_call = i1_predict_p_e1.pcall & i1_predict_p_e1.valid & ~i1_predict_p_e1.br_error;
   assign exu_rets_e1_pkt.pc0_ret  = i0_predict_p_e1.pret  & i0_predict_p_e1.valid & ~i0_predict_p_e1.br_error;
   assign exu_rets_e1_pkt.pc1_ret  = i1_predict_p_e1.pret  & i1_predict_p_e1.valid & ~i1_predict_p_e1.br_error;
   assign exu_rets_e1_pkt.pc0_pc4  = i0_predict_p_e1.pc4;
   assign exu_rets_e1_pkt.pc1_pc4  = i1_predict_p_e1.pc4;
   rvdffe #(76) i0_src_e1_ff (.*,
                            .en(i0_e1_data_en),
                            .din( {i0_rs1_d[31:0], i0_rs2_d[31:0], dec_i0_br_immed_d[12:1]}),
                            .dout({i0_rs1_e1[31:0], i0_rs2_e1[31:0], i0_br_immed_e1[12:1]})
                            );
   rvdffe #(76) i0_src_e2_ff (.*,
                            .en(i0_e2_data_en),
                            .din( {i0_rs1_e1[31:0], i0_rs2_e1[31:0], i0_br_immed_e1[12:1]}),
                            .dout({i0_rs1_e2[31:0], i0_rs2_e2[31:0], i0_br_immed_e2[12:1]})
                            );
   rvdffe #(76) i0_src_e3_ff (.*,
                            .en(i0_e3_data_en),
                            .din( {i0_rs1_e2_final[31:0], i0_rs2_e2_final[31:0], i0_br_immed_e2[12:1]}),
                            .dout({i0_rs1_e3[31:0], i0_rs2_e3[31:0], i0_br_immed_e3[12:1]})
                            );
   rvdffe #(76) i1_src_e1_ff (.*,
                            .en(i1_e1_data_en),
                            .din( {i1_rs1_d[31:0], i1_rs2_d[31:0], dec_i1_br_immed_d[12:1]}),
                            .dout({i1_rs1_e1[31:0], i1_rs2_e1[31:0], i1_br_immed_e1[12:1]})
                            );
   rvdffe #(76) i1_src_e2_ff (.*,
                            .en(i1_e2_data_en),
                            .din( {i1_rs1_e1[31:0], i1_rs2_e1[31:0], i1_br_immed_e1[12:1]}),
                            .dout({i1_rs1_e2[31:0], i1_rs2_e2[31:0], i1_br_immed_e2[12:1]})
                            );
   rvdffe #(76) i1_src_e3_ff (.*,
                            .en(i1_e3_data_en),
                            .din( {i1_rs1_e2_final[31:0], i1_rs2_e2_final[31:0], i1_br_immed_e2[12:1]}),
                            .dout({i1_rs1_e3[31:0], i1_rs2_e3[31:0], i1_br_immed_e3[12:1]})
                            );
   assign i0_rs1_e2_final[31:0] = (dec_i0_rs1_bypass_en_e2) ? i0_rs1_bypass_data_e2[31:0] : i0_rs1_e2[31:0];
   assign i0_rs2_e2_final[31:0] = (dec_i0_rs2_bypass_en_e2) ? i0_rs2_bypass_data_e2[31:0] : i0_rs2_e2[31:0];
   assign i1_rs1_e2_final[31:0] = (dec_i1_rs1_bypass_en_e2) ? i1_rs1_bypass_data_e2[31:0] : i1_rs1_e2[31:0];
   assign i1_rs2_e2_final[31:0] = (dec_i1_rs2_bypass_en_e2) ? i1_rs2_bypass_data_e2[31:0] : i1_rs2_e2[31:0];
   assign i0_rs1_e3_final[31:0] = (dec_i0_rs1_bypass_en_e3) ? i0_rs1_bypass_data_e3[31:0] : i0_rs1_e3[31:0];
   assign i0_rs2_e3_final[31:0] = (dec_i0_rs2_bypass_en_e3) ? i0_rs2_bypass_data_e3[31:0] : i0_rs2_e3[31:0];
   assign i1_rs1_e3_final[31:0] = (dec_i1_rs1_bypass_en_e3) ? i1_rs1_bypass_data_e3[31:0] : i1_rs1_e3[31:0];
   assign i1_rs2_e3_final[31:0] = (dec_i1_rs2_bypass_en_e3) ? i1_rs2_bypass_data_e3[31:0] : i1_rs2_e3[31:0];
   logic [4:0] ghr_e4_ns, ghr_e4;
   logic [4:0] ghr_e1_ns, ghr_e1;
   logic i0_taken_e1, i1_taken_e1, dec_i0_alu_decode_e1, dec_i1_alu_decode_e1, i0_valid_e1, i1_valid_e1, i0_ataken_e1, i1_ataken_e1, exu_flush_final_f;
   assign i0_valid_e1 = ~exu_flush_final & ~exu_flush_final_f & (i0_predict_p_e1.valid | i0_predict_p_e1.misp);
   assign i1_valid_e1 = ~exu_flush_final & ~exu_flush_final_f & (i1_predict_p_e1.valid | i1_predict_p_e1.misp) & ~exu_i0_flush_upper_e1;
   assign i0_ataken_e1 = i0_predict_p_e1.ataken;
   assign i1_ataken_e1 = i1_predict_p_e1.ataken;
   assign i0_taken_e1 = (i0_ataken_e1 & dec_i0_alu_decode_e1) | (i0_predict_p_e1.hist[1] & ~dec_i0_alu_decode_e1);
   assign i1_taken_e1= (i1_ataken_e1 & dec_i1_alu_decode_e1) | (i1_predict_p_e1.hist[1] & ~dec_i1_alu_decode_e1);
    assign ghr_e1_ns[4:0] = ( ({5{~dec_tlu_flush_lower_wb & i0_valid_e1 & (i0_predict_p_e1.misp | ~i1_valid_e1)}} & {ghr_e1[5-2:0], i0_taken_e1}) |
                                           ({5{~dec_tlu_flush_lower_wb & i0_valid_e1 & ~i0_predict_p_e1.misp &  i1_valid_e1}} & {ghr_e1[5-3:0], i0_taken_e1, i1_taken_e1}) |
                                           ({5{~dec_tlu_flush_lower_wb & ~i0_valid_e1 & ~i0_predict_p_e1.br_error & i1_valid_e1}} & {ghr_e1[5-2:0], i1_taken_e1}) |
                                           ({5{dec_tlu_flush_lower_wb}} & ghr_e4[4:0]) |
                                           ({5{~dec_tlu_flush_lower_wb & ~i0_valid_e1 & ~i1_valid_e1}} & ghr_e1[4:0]) );
   rvdffs #(5) e1ghrff (.*, .clk(active_clk), .en(~freeze), .din({ghr_e1_ns[4:0]}), .dout({ghr_e1[4:0]}));
   rvdffs #(2)             e1ghrdecff (.*, .clk(active_clk), .en(~freeze), .din({dec_i0_alu_decode_d, dec_i1_alu_decode_d}), .dout({dec_i0_alu_decode_e1, dec_i1_alu_decode_e1}));
   logic i0_valid_e4, i1_pred_valid_e4;
   assign i0_valid_e4 = dec_tlu_i0_valid_e4 & ((i0_predict_p_e4.valid) | i0_predict_p_e4.misp);
   assign i1_pred_valid_e4 = dec_tlu_i1_valid_e4 & ((i1_predict_p_e4.valid) | i1_predict_p_e4.misp) & ~exu_i0_flush_upper_e4;
   assign ghr_e4_ns[4:0]  = ( ({5{i0_valid_e4 & (i0_predict_p_e4.misp | ~i1_pred_valid_e4)}} & {ghr_e4[5-2:0], i0_predict_p_e4.ataken}) |
                                           ({5{i0_valid_e4  & ~i0_predict_p_e4.misp &  i1_pred_valid_e4}} & {ghr_e4[5-3:0], i0_predict_p_e4.ataken, i1_predict_p_e4.ataken}) |
                                           ({5{~i0_valid_e4 & ~i0_predict_p_e4.br_error & i1_pred_valid_e4}} & {ghr_e4[5-2:0], i1_predict_p_e4.ataken}) |
                                           ({5{~i0_valid_e4 & ~i1_pred_valid_e4}} & ghr_e4[4:0]) );
   rvdff #(5) e4ghrff (.*, .clk(active_clk), .din({ghr_e4_ns[4:0]}),
                                                            .dout({ghr_e4[4:0]}));
   rvdff #(1)           e4ghrflushff (.*, .clk(active_clk), .din({exu_flush_final}),
                                                            .dout({exu_flush_final_f}));
   exu_alu_ctl i0_alu_e4 (.*,
                          .freeze        ( 1'b0                        ),    
                          .enable        ( i0_e4_ctl_en                ),    
                          .predict_p     ( i0_pp_e4_in                 ),    
                          .valid         ( dec_i0_sec_decode_e3        ),    
                          .flush         ( dec_tlu_flush_lower_wb      ),    
                          .a             ( i0_rs1_e3_final[31:0]       ),    
                          .b             ( i0_rs2_e3_final[31:0]       ),    
                          .pc            ( dec_i0_pc_e3[31:1]          ),    
                          .brimm         ( i0_br_immed_e3[12:1]        ),    
                          .ap            ( i0_ap_e4                    ),    
                          .out           ( exu_i0_result_e4[31:0]      ),    
                          .flush_upper   ( exu_i0_flush_lower_e4       ),    
                          .flush_path    ( exu_i0_flush_path_e4[31:1]  ),    
                          .predict_p_ff  ( i0_predict_p_e4             ),    
                          .pc_ff         ( i0_alu_pc_nc[31:1]          ),    
                          .pred_correct  ( i0_pred_correct_lower_e4    )     
                          );
   exu_alu_ctl i1_alu_e4 (.*,
                          .freeze        ( 1'b0                        ),    
                          .enable        ( i1_e4_ctl_en                ),    
                          .predict_p     ( i1_pp_e4_in                 ),    
                          .valid         ( dec_i1_sec_decode_e3        ),    
                          .flush         ( dec_tlu_flush_lower_wb      ),    
                          .a             ( i1_rs1_e3_final[31:0]       ),    
                          .b             ( i1_rs2_e3_final[31:0]       ),    
                          .pc            ( dec_i1_pc_e3[31:1]          ),    
                          .brimm         ( i1_br_immed_e3[12:1]        ),    
                          .ap            ( i1_ap_e4                    ),    
                          .out           ( exu_i1_result_e4[31:0]      ),    
                          .flush_upper   ( exu_i1_flush_lower_e4       ),    
                          .flush_path    ( exu_i1_flush_path_e4[31:1]  ),    
                          .predict_p_ff  ( i1_predict_p_e4             ),    
                          .pc_ff         ( i1_alu_pc_nc[31:1]          ),    
                          .pred_correct  ( i1_pred_correct_lower_e4    )     
                          );
   assign exu_i0_br_hist_e4[1:0]               = i0_predict_p_e4.hist[1:0];
   assign exu_i0_br_bank_e4[1:0]               = i0_predict_p_e4.bank[1:0];
   assign exu_i0_br_error_e4                   = i0_predict_p_e4.br_error;
   assign exu_i0_br_fghr_e4[4:0] = i0_predict_p_e4.fghr[4:0];
   assign exu_i0_br_middle_e4                  = i0_predict_p_e4.pc4 ^ i0_predict_p_e4.boffset;
   assign exu_i0_br_start_error_e4     = i0_predict_p_e4.br_start_error;
   assign exu_i0_br_index_e4[5:4]     = i0_predict_p_e4.index[5:4];
   assign exu_i0_br_valid_e4           = i0_predict_p_e4.valid;
   assign exu_i0_br_mp_e4              = i0_predict_p_e4.misp;  
   assign exu_i0_br_ret_e4             = i0_predict_p_e4.pret;
   assign exu_i0_br_call_e4            = i0_predict_p_e4.pcall;
   assign exu_i0_br_way_e4             = i0_predict_p_e4.way;
   assign exu_i1_br_hist_e4[1:0]               = i1_predict_p_e4.hist[1:0];
   assign exu_i1_br_bank_e4[1:0]               = i1_predict_p_e4.bank[1:0];
   assign exu_i1_br_fghr_e4[4:0] = i1_predict_p_e4.fghr[4:0];
   assign exu_i1_br_middle_e4                  = i1_predict_p_e4.pc4 ^ i1_predict_p_e4.boffset;
   assign exu_i1_br_error_e4                   = i1_predict_p_e4.br_error;
   assign exu_i1_br_index_e4[5:4]     = i1_predict_p_e4.index[5:4];
   assign exu_i1_br_start_error_e4     = i1_predict_p_e4.br_start_error;
   assign exu_i1_br_valid_e4           = i1_predict_p_e4.valid;
   assign exu_i1_br_mp_e4              = i1_predict_p_e4.misp;
   assign exu_i1_br_way_e4             = i1_predict_p_e4.way;
   assign exu_i1_br_ret_e4             = i1_predict_p_e4.pret;
   assign exu_i1_br_call_e4            = i1_predict_p_e4.pcall;
   assign exu_rets_e4_pkt.pc0_call = i0_predict_p_e4.pcall & i0_predict_p_e4.valid & ~i0_predict_p_e4.br_error;
   assign exu_rets_e4_pkt.pc1_call = i1_predict_p_e4.pcall & i1_predict_p_e4.valid & ~i1_predict_p_e4.br_error;
   assign exu_rets_e4_pkt.pc0_ret = i0_predict_p_e4.pret & i0_predict_p_e4.valid & ~i0_predict_p_e4.br_error;
   assign exu_rets_e4_pkt.pc1_ret = i1_predict_p_e4.pret & i1_predict_p_e4.valid & ~i1_predict_p_e4.br_error;
   assign exu_rets_e4_pkt.pc0_pc4 = i0_predict_p_e4.pc4;
   assign exu_rets_e4_pkt.pc1_pc4 = i1_predict_p_e4.pc4;
   predict_pkt_t final_predict_mp, final_predict_mp_ff;
   logic fp_enable, fp_enable_ff;
   assign fp_enable = exu_i0_flush_lower_e4 | exu_i1_flush_lower_e4 |
                      exu_i0_flush_upper_e1 | exu_i1_flush_upper_e1;
   rvdff #(1) final_predict_ff (.*, .clk(active_clk), .din(fp_enable), .dout(fp_enable_ff));
   assign final_predict_mp = (exu_i0_flush_lower_e4) ? i0_predict_p_e4 :
                             (exu_i1_flush_lower_e4) ? i1_predict_p_e4 :
                             (exu_i0_flush_upper_e1) ? i0_predict_p_e1 :
                             (exu_i1_flush_upper_e1) ? i1_predict_p_e1 : '0;
   rvdffe #($bits(predict_pkt_t)) predict_mp_ff (.*, .en(fp_enable | fp_enable_ff), .din(final_predict_mp), .dout(final_predict_mp_ff));
   logic [4:0] final_eghr, after_flush_eghr;
   assign final_eghr[4:0] = ((exu_i0_flush_upper_e1 | exu_i1_flush_upper_e1) & ~dec_tlu_flush_lower_wb & ~exu_i0_flush_lower_e4 & ~exu_i1_flush_lower_e4 ) ? ghr_e1[4:0] : ghr_e4[4:0];
   assign after_flush_eghr[4:0] = ((exu_i0_flush_upper_e2 | exu_i1_flush_upper_e2) & ~dec_tlu_flush_lower_wb) ? ghr_e1[4:0] : ghr_e4[4:0];
   assign exu_mp_pkt.way                                    = final_predict_mp_ff.way;
   assign exu_mp_pkt.misp                                   = final_predict_mp_ff.misp;
   assign exu_mp_pkt.pcall                                  = final_predict_mp_ff.pcall;
   assign exu_mp_pkt.pja                                    = final_predict_mp_ff.pja;
   assign exu_mp_pkt.pret                                   = final_predict_mp_ff.pret;
   assign exu_mp_pkt.ataken                                 = final_predict_mp_ff.ataken;
   assign exu_mp_pkt.boffset                                = final_predict_mp_ff.boffset;
   assign exu_mp_pkt.pc4                                    = final_predict_mp_ff.pc4;
   assign exu_mp_pkt.hist[1:0]                              = final_predict_mp_ff.hist[1:0];
   assign exu_mp_pkt.toffset[11:0]                          = final_predict_mp_ff.toffset[11:0];
   assign exu_mp_pkt.index[5:4] = final_predict_mp_ff.index[5:4];
   assign exu_mp_pkt.bank[1:0]                              = final_predict_mp_ff.bank[1:0];
   assign exu_mp_pkt.btag[9-1:0]            = final_predict_mp_ff.btag[9-1:0];
   assign exu_mp_pkt.fghr[4:0]                = after_flush_eghr[4:0];      
   assign exu_mp_eghr[4:0] = final_predict_mp_ff.fghr[4:0];  
   rvdffe #(32) i0_upper_flush_e2_ff (.*,
                                    .en(i0_e2_ctl_en),
                                    .din({
                                          exu_i0_flush_path_e1[31:1],
                                          exu_i0_flush_upper_e1}),
                                    .dout({
                                           i0_flush_path_upper_e2[31:1],
                                           exu_i0_flush_upper_e2})
                                    );
   rvdffe #(33) i1_upper_flush_e2_ff (.*,
                                    .en(i1_e2_ctl_en),
                                    .din({dec_i1_valid_e1,
                                          exu_i1_flush_path_e1[31:1],
                                          exu_i1_flush_upper_e1}),
                                    .dout({i1_valid_e2,
                                           i1_flush_path_upper_e2[31:1],
                                           exu_i1_flush_upper_e2})
                                    );
   assign exu_flush_path_e2[31:1] = (exu_i0_flush_upper_e2) ? i0_flush_path_upper_e2[31:1] : i1_flush_path_upper_e2[31:1];
   assign exu_i0_flush_final = dec_tlu_flush_lower_wb | (exu_i0_flush_upper_e2 & ~freeze);
   assign exu_i1_flush_final = dec_tlu_flush_lower_wb | (exu_i1_flush_upper_e2 & ~freeze);
   assign exu_flush_upper_e2 = (exu_i0_flush_upper_e2 | exu_i1_flush_upper_e2) & ~freeze;
   assign exu_flush_final = dec_tlu_flush_lower_wb | exu_flush_upper_e2;
   assign exu_flush_path_final[31:1] = (dec_tlu_flush_lower_wb) ? dec_tlu_flush_path_wb[31:1] : exu_flush_path_e2[31:1];
   rvdffe #(63) i0_upper_flush_e3_ff (.*,
                                    .en(i0_e3_ctl_en),
                                    .din({i0_flush_path_upper_e2[31:1],
                                          pred_correct_npc_e2[31:1],
                                          exu_i0_flush_upper_e2}),
                                    .dout({
                                           i0_flush_path_upper_e3[31:1],
                                           pred_correct_npc_e3[31:1],
                                           exu_i0_flush_upper_e3})
                                    );
   rvdffe #(32) i1_upper_flush_e3_ff (.*,
                                    .en(i1_e3_ctl_en),
                                    .din({i1_valid_e2,
                                          i1_flush_path_upper_e2[31:1]
                                          }),
                                    .dout({i1_valid_e3,
                                           i1_flush_path_upper_e3[31:1]})
                                    );
   rvdffe #(63) i0_upper_flush_e4_ff (.*,
                                    .en(i0_e4_ctl_en),
                                    .din({
                                          i0_flush_path_upper_e3[31:1],
                                          pred_correct_npc_e3[31:1],
                                          exu_i0_flush_upper_e3 & ~freeze}),
                                    .dout({
                                           i0_flush_path_upper_e4[31:1],
                                           pred_correct_npc_e4[31:1],
                                           exu_i0_flush_upper_e4})
                                    );
   rvdffe #(32) i1_upper_flush_e4_ff (.*,
                                    .en(i1_e4_ctl_en),
                                    .din({i1_valid_e3 & ~freeze,
                                          i1_flush_path_upper_e3[31:1]}),
                                    .dout({i1_valid_e4,
                                           i1_flush_path_upper_e4[31:1]})
                                    );
   rvdffs #(2) pred_correct_upper_e2_ff  (.*,
                                       .clk(active_clk),
                                       .en(~freeze),
                                       .din({i1_pred_correct_upper_e1,i0_pred_correct_upper_e1}),
                                       .dout({i1_pred_correct_upper_e2,i0_pred_correct_upper_e2})
                                       );
   rvdffs #(2) pred_correct_upper_e3_ff  (.*,
                                       .clk(active_clk),
                                       .en(~freeze),
                                       .din({i1_pred_correct_upper_e2,i0_pred_correct_upper_e2}),
                                       .dout({i1_pred_correct_upper_e3,i0_pred_correct_upper_e3})
                                       );
   rvdff #(2) pred_correct_upper_e4_ff  (.*,
                                       .clk(active_clk),
                                       .din({i1_pred_correct_upper_e3,i0_pred_correct_upper_e3}),
                                       .dout({i1_pred_correct_upper_e4,i0_pred_correct_upper_e4})
                                       );
   rvdff #(2) sec_decode_e4_ff  (.*,
                                       .clk(active_clk),
                                       .din({dec_i0_sec_decode_e3,dec_i1_sec_decode_e3}),
                                       .dout({i0_sec_decode_e4,i1_sec_decode_e4})
                               );
   assign i1_valid_e4_eff = i1_valid_e4 & ~((i0_sec_decode_e4) ? exu_i0_flush_lower_e4 : exu_i0_flush_upper_e4);
   assign i1_pred_correct_e4_eff = (i1_sec_decode_e4) ? i1_pred_correct_lower_e4 : i1_pred_correct_upper_e4;
   assign i0_pred_correct_e4_eff = (i0_sec_decode_e4) ? i0_pred_correct_lower_e4 : i0_pred_correct_upper_e4;
   assign i1_flush_path_e4_eff[31:1] = (i1_sec_decode_e4) ? exu_i1_flush_path_e4[31:1] : i1_flush_path_upper_e4[31:1];
   assign i0_flush_path_e4_eff[31:1] = (i0_sec_decode_e4) ? exu_i0_flush_path_e4[31:1] : i0_flush_path_upper_e4[31:1];
   assign npc_e4[31:1] = (i1_valid_e4_eff) ? ((i1_pred_correct_e4_eff) ? pred_correct_npc_e4[31:1] : i1_flush_path_e4_eff[31:1]) :
                                             ((i0_pred_correct_e4_eff) ? pred_correct_npc_e4[31:1] : i0_flush_path_e4_eff[31:1]);
   assign exu_npc_e4[31:1] = (div_finish_early) ? exu_i0_flush_path_e1[31:1] :
                             (exu_div_finish)   ? div_npc[31:1]              :
                                                  npc_e4[31:1];
   rvdffe #(31) npc_any_ff (.*, .en(div_valid_e1), .din(exu_i0_flush_path_e1[31:1]),    .dout(div_npc[31:1]));
endmodule  
module lsu
   import swerv_types::*;
(
   input logic [31:0]                      i0_result_e4_eff,  
   input logic [31:0]                      i1_result_e4_eff,  
   input logic [31:0]                      i0_result_e2,      
   input logic                             flush_final_e3,     
   input logic                             i0_flush_final_e3,  
   input logic                             dec_tlu_flush_lower_wb,     
   input logic                             dec_tlu_i0_kill_writeb_wb,  
   input logic                             dec_tlu_i1_kill_writeb_wb,  
   input logic                             dec_tlu_cancel_e4,          
   input logic                             dec_tlu_non_blocking_disable,     
   input logic                             dec_tlu_wb_coalescing_disable,    
   input logic                             dec_tlu_ld_miss_byp_wb_disable,   
   input logic                             dec_tlu_sideeffect_posted_disable,   
   input logic                             dec_tlu_core_ecc_disable,         
   input logic [31:0]                      exu_lsu_rs1_d,       
   input logic [31:0]                      exu_lsu_rs2_d,       
   input logic [11:0]                      dec_lsu_offset_d,    
   input                                   lsu_pkt_t lsu_p,      
   input logic                             dec_i0_lsu_decode_d,  
   input logic [31:0]                      dec_tlu_mrac_ff,      
   output logic [31:0]                     lsu_result_dc3,       
   output logic                            lsu_single_ecc_error_incr,      
   output logic [31:0]                     lsu_result_corr_dc4,  
   output logic                            lsu_freeze_dc3,       
   output logic                            lsu_load_stall_any,  
   output logic                            lsu_store_stall_any,  
   output logic                            lsu_load_ecc_stbuf_full_dc3,  
   output logic                            lsu_idle_any,         
   output logic                            lsu_halt_idle_any,    
   output lsu_error_pkt_t                  lsu_error_pkt_dc3,              
   output logic                            lsu_freeze_external_ints_dc3,   
   output logic                            lsu_imprecise_error_load_any,   
   output logic                            lsu_imprecise_error_store_any,  
   output logic [31:0]                     lsu_imprecise_error_addr_any,   
   input  logic                                 dec_nonblock_load_freeze_dc2,   
   output logic                                 lsu_nonblock_load_valid_dc3,     
   output logic [3-1:0]  lsu_nonblock_load_tag_dc3,       
   output logic                                 lsu_nonblock_load_inv_dc5,       
   output logic [3-1:0]  lsu_nonblock_load_inv_tag_dc5,   
   output logic                                 lsu_nonblock_load_data_valid,    
   output logic                                 lsu_nonblock_load_data_error,    
   output logic [3-1:0]  lsu_nonblock_load_data_tag,      
   output logic [31:0]                          lsu_nonblock_load_data,          
   output logic                            lsu_pmu_misaligned_dc3,          
   output logic                            lsu_pmu_bus_trxn,                
   output logic                            lsu_pmu_bus_misaligned,          
   output logic                            lsu_pmu_bus_error,               
   output logic                            lsu_pmu_bus_busy,                
   input                                   trigger_pkt_t [3:0] trigger_pkt_any,  
   output logic [3:0]                      lsu_trigger_match_dc3,                
   output logic                            dccm_wren,        
   output logic                            dccm_rden,        
   output logic [16-1:0]        dccm_wr_addr,     
   output logic [16-1:0]        dccm_rd_addr_lo,  
   output logic [16-1:0]        dccm_rd_addr_hi,  
   output logic [39-1:0] dccm_wr_data,     
   input logic [39-1:0]  dccm_rd_data_lo,  
   input logic [39-1:0]  dccm_rd_data_hi,  
   output logic                            picm_wren,     
   output logic                            picm_rden,     
   output logic                            picm_mken,     
   output logic [31:0]                     picm_addr,     
   output logic [31:0]                     picm_wr_data,  
   input logic [31:0]                      picm_rd_data,  
   output logic                            lsu_axi_awvalid,
   input  logic                            lsu_axi_awready,
   output logic [4-1:0]      lsu_axi_awid,
   output logic [31:0]                     lsu_axi_awaddr,
   output logic [3:0]                      lsu_axi_awregion,
   output logic [7:0]                      lsu_axi_awlen,
   output logic [2:0]                      lsu_axi_awsize,
   output logic [1:0]                      lsu_axi_awburst,
   output logic                            lsu_axi_awlock,
   output logic [3:0]                      lsu_axi_awcache,
   output logic [2:0]                      lsu_axi_awprot,
   output logic [3:0]                      lsu_axi_awqos,
   output logic                            lsu_axi_wvalid,
   input  logic                            lsu_axi_wready,
   output logic [63:0]                     lsu_axi_wdata,
   output logic [7:0]                      lsu_axi_wstrb,
   output logic                            lsu_axi_wlast,
   input  logic                            lsu_axi_bvalid,
   output logic                            lsu_axi_bready,
   input  logic [1:0]                      lsu_axi_bresp,
   input  logic [4-1:0]      lsu_axi_bid,
   output logic                            lsu_axi_arvalid,
   input  logic                            lsu_axi_arready,
   output logic [4-1:0]      lsu_axi_arid,
   output logic [31:0]                     lsu_axi_araddr,
   output logic [3:0]                      lsu_axi_arregion,
   output logic [7:0]                      lsu_axi_arlen,
   output logic [2:0]                      lsu_axi_arsize,
   output logic [1:0]                      lsu_axi_arburst,
   output logic                            lsu_axi_arlock,
   output logic [3:0]                      lsu_axi_arcache,
   output logic [2:0]                      lsu_axi_arprot,
   output logic [3:0]                      lsu_axi_arqos,
   input  logic                            lsu_axi_rvalid,
   output logic                            lsu_axi_rready,
   input  logic [4-1:0]      lsu_axi_rid,
   input  logic [63:0]                     lsu_axi_rdata,
   input  logic [1:0]                      lsu_axi_rresp,
   input  logic                            lsu_axi_rlast,
   input logic                             lsu_bus_clk_en,     
   input logic                             dma_dccm_req,        
   input logic [31:0]                      dma_mem_addr,        
   input logic [2:0]                       dma_mem_sz,          
   input logic                             dma_mem_write,       
   input logic [63:0]                      dma_mem_wdata,       
   output logic                            dccm_dma_rvalid,      
   output logic                            dccm_dma_ecc_error,   
   output logic [63:0]                     dccm_dma_rdata,       
   output logic                            dccm_ready,           
   input logic                             clk_override,         
   input logic                             scan_mode,            
   input logic                             clk,
   input logic                             free_clk,
   input logic                             rst_l
   );
localparam TOTAL_INT        = 9;
localparam DCCM_BITS        = 16;
localparam DCCM_BANK_BITS   = 3;
localparam DCCM_NUM_BANKS   = 8;
localparam DCCM_DATA_WIDTH  = 32;
localparam DCCM_FDATA_WIDTH = 39;
localparam DCCM_BYTE_WIDTH  = 4;
localparam DCCM_ECC_WIDTH   = 7;
localparam LSU_RDBUF_DEPTH  = 8;
localparam DMA_BUF_DEPTH    = 4;
localparam LSU_STBUF_DEPTH  = 8;
localparam LSU_SB_BITS      = 16;
localparam DEC_INSTBUF_DEPTH = 4;
localparam ICCM_SIZE         = 512;
localparam ICCM_BITS         = 19;
localparam ICCM_NUM_BANKS    = 8;
localparam ICCM_BANK_BITS    = 3;
localparam ICCM_INDEX_BITS   = 14;
localparam ICCM_BANK_HI      = 4 + (3/4);
localparam ICACHE_TAG_HIGH  = 12;
localparam ICACHE_TAG_LOW   = 6;
localparam ICACHE_IC_DEPTH  = 8;
localparam ICACHE_TAG_DEPTH = 64;
localparam LSU_BUS_TAG     = 4;
localparam DMA_BUS_TAG     = 1;
localparam SB_BUS_TAG      = 1;
localparam IFU_BUS_TAG     = 3;
   logic        lsu_dccm_rden_dc3;
   logic [63:0] store_data_dc2;
   logic [63:0] store_data_dc3;
   logic [31:0] store_data_dc4;
   logic [31:0] store_data_dc5;
   logic [31:0] store_ecc_datafn_hi_dc3;
   logic [31:0] store_ecc_datafn_lo_dc3;
   logic        single_ecc_error_hi_dc3, single_ecc_error_lo_dc3;
   logic        lsu_single_ecc_error_dc3, lsu_single_ecc_error_dc4, lsu_single_ecc_error_dc5;
   logic        lsu_double_ecc_error_dc3;
   logic [31:0] dccm_data_hi_dc3;
   logic [31:0] dccm_data_lo_dc3;
   logic [6:0]  dccm_data_ecc_hi_dc3;
   logic [6:0]  dccm_data_ecc_lo_dc3;
   logic [31:0] lsu_ld_data_dc3;
   logic [31:0] lsu_ld_data_corr_dc3;
   logic [31:0] picm_mask_data_dc3;
   logic [31:0] lsu_addr_dc1, lsu_addr_dc2, lsu_addr_dc3, lsu_addr_dc4, lsu_addr_dc5;
   logic [31:0] end_addr_dc1, end_addr_dc2, end_addr_dc3, end_addr_dc4, end_addr_dc5;
   lsu_pkt_t    lsu_pkt_dc1, lsu_pkt_dc2, lsu_pkt_dc3, lsu_pkt_dc4, lsu_pkt_dc5;
   logic        lsu_i0_valid_dc1, lsu_i0_valid_dc2, lsu_i0_valid_dc3, lsu_i0_valid_dc4, lsu_i0_valid_dc5;
   logic        isldst_dc1, dccm_ldst_dc2, dccm_ldst_dc3;
   logic        store_stbuf_reqvld_dc3;
   logic        load_stbuf_reqvld_dc3;
   logic        ldst_stbuf_reqvld_dc3;
   logic        lsu_commit_dc5;
   logic        lsu_exc_dc2;
   logic        addr_in_dccm_dc1, addr_in_dccm_dc2, addr_in_dccm_dc3;
   logic        addr_in_pic_dc1, addr_in_pic_dc2, addr_in_pic_dc3;
   logic        addr_external_dc2, addr_external_dc3, addr_external_dc4, addr_external_dc5;
   logic                       stbuf_reqvld_any;
   logic                       stbuf_reqvld_flushed_any;
   logic                       stbuf_addr_in_pic_any;
   logic [DCCM_BYTE_WIDTH-1:0] stbuf_byteen_any;
   logic [LSU_SB_BITS-1:0]     stbuf_addr_any;
   logic [DCCM_DATA_WIDTH-1:0] stbuf_data_any;
   logic [(DCCM_FDATA_WIDTH-DCCM_DATA_WIDTH-1):0] stbuf_ecc_any;
   logic                       lsu_cmpen_dc2;
   logic [DCCM_DATA_WIDTH-1:0] stbuf_fwddata_hi_dc3;
   logic [DCCM_DATA_WIDTH-1:0] stbuf_fwddata_lo_dc3;
   logic [DCCM_BYTE_WIDTH-1:0] stbuf_fwdbyteen_hi_dc3;
   logic [DCCM_BYTE_WIDTH-1:0] stbuf_fwdbyteen_lo_dc3;
   logic        lsu_stbuf_commit_any;
   logic        lsu_stbuf_empty_any;
   logic        lsu_stbuf_nodma_empty_any;    
   logic        lsu_stbuf_full_any;
   logic        lsu_busreq_dc5;
   logic        lsu_bus_buffer_pend_any;
   logic        lsu_bus_buffer_empty_any;
   logic        lsu_bus_buffer_full_any;
   logic        lsu_busreq_dc2;
   logic [31:0] bus_read_data_dc3;
   logic        ld_bus_error_dc3;
   logic [31:0] ld_bus_error_addr_dc3;
   logic        flush_dc2_up, flush_dc3, flush_dc4, flush_dc5, flush_prior_dc5;
   logic        is_sideeffects_dc2, is_sideeffects_dc3;
   logic        ldst_nodma_dc1todc3;
   logic        lsu_c1_dc3_clk, lsu_c1_dc4_clk, lsu_c1_dc5_clk;
   logic        lsu_c2_dc3_clk, lsu_c2_dc4_clk, lsu_c2_dc5_clk;
   logic        lsu_freeze_c1_dc2_clk, lsu_freeze_c1_dc3_clk;
   logic        lsu_freeze_c1_dc1_clken, lsu_freeze_c1_dc2_clken, lsu_freeze_c1_dc3_clken;
   logic        lsu_freeze_c2_dc1_clken, lsu_freeze_c2_dc2_clken, lsu_freeze_c2_dc3_clken, lsu_freeze_c2_dc4_clken;
   logic        lsu_store_c1_dc1_clken, lsu_store_c1_dc2_clken, lsu_store_c1_dc3_clken, lsu_store_c1_dc4_clk, lsu_store_c1_dc5_clk;
   logic        lsu_freeze_c2_dc1_clk, lsu_freeze_c2_dc2_clk, lsu_freeze_c2_dc3_clk, lsu_freeze_c2_dc4_clk;
   logic        lsu_stbuf_c1_clk;
   logic        lsu_bus_ibuf_c1_clk, lsu_bus_obuf_c1_clk, lsu_bus_buf_c1_clk;
   logic        lsu_dccm_c1_dc3_clk, lsu_dccm_c1_dc3_clken, lsu_pic_c1_dc3_clken;
   logic        lsu_busm_clk;
   logic        lsu_free_c2_clk;
   lsu_lsc_ctl lsu_lsc_ctl(.*);
   assign lsu_store_stall_any = lsu_stbuf_full_any | lsu_bus_buffer_full_any;
   assign lsu_load_stall_any  = lsu_bus_buffer_full_any;
   assign ldst_nodma_dc1todc3 = (lsu_pkt_dc1.valid & ~lsu_pkt_dc1.dma) | (lsu_pkt_dc2.valid & ~lsu_pkt_dc2.dma) | (lsu_pkt_dc3.valid & ~lsu_pkt_dc3.dma);
   assign dccm_ready = ~(lsu_p.valid | lsu_stbuf_full_any | lsu_freeze_dc3 | ldst_nodma_dc1todc3);
   assign flush_dc2_up = flush_final_e3 | i0_flush_final_e3 | dec_tlu_flush_lower_wb;
   assign flush_dc3    = (flush_final_e3 & i0_flush_final_e3) | dec_tlu_flush_lower_wb;
   assign flush_dc4    = dec_tlu_flush_lower_wb;
   assign flush_dc5    = (dec_tlu_i0_kill_writeb_wb | (dec_tlu_i1_kill_writeb_wb & ~lsu_i0_valid_dc5));
   assign flush_prior_dc5 = dec_tlu_i0_kill_writeb_wb & ~lsu_i0_valid_dc5;     
   assign lsu_idle_any = ~(lsu_pkt_dc1.valid | lsu_pkt_dc2.valid | lsu_pkt_dc3.valid | lsu_pkt_dc4.valid | lsu_pkt_dc5.valid) &
                         lsu_bus_buffer_empty_any & lsu_stbuf_empty_any;
   assign lsu_halt_idle_any = ~((lsu_pkt_dc1.valid & ~lsu_pkt_dc1.dma) |
                                (lsu_pkt_dc2.valid & ~lsu_pkt_dc2.dma) |
                                (lsu_pkt_dc3.valid & ~lsu_pkt_dc3.dma) |
                                (lsu_pkt_dc4.valid & ~lsu_pkt_dc4.dma) |
                                (lsu_pkt_dc5.valid & ~lsu_pkt_dc5.dma)) &
                               lsu_bus_buffer_empty_any & lsu_stbuf_nodma_empty_any;
   assign store_stbuf_reqvld_dc3 = lsu_pkt_dc3.valid & lsu_pkt_dc3.store & (addr_in_dccm_dc3 | addr_in_pic_dc3) & (~flush_dc3 | lsu_pkt_dc3.dma) & ~lsu_freeze_dc3;
   assign load_stbuf_reqvld_dc3  = lsu_pkt_dc3.valid & lsu_pkt_dc3.load  & (addr_in_dccm_dc3 | addr_in_pic_dc3) & lsu_single_ecc_error_dc3 & (~flush_dc3 | lsu_pkt_dc3.dma) & ~lsu_freeze_dc3;
   assign isldst_dc1 = lsu_pkt_dc1.valid & (lsu_pkt_dc1.load | lsu_pkt_dc1.store);
   assign dccm_ldst_dc2 = lsu_pkt_dc2.valid & (lsu_pkt_dc2.load | lsu_pkt_dc2.store) & (addr_in_dccm_dc2 | addr_in_pic_dc2);
   assign dccm_ldst_dc3 = lsu_pkt_dc3.valid & (lsu_pkt_dc3.load | lsu_pkt_dc3.store) & (addr_in_dccm_dc3 | addr_in_pic_dc3);
   assign lsu_cmpen_dc2 = lsu_pkt_dc2.valid & (lsu_pkt_dc2.load | lsu_pkt_dc2.store) & (addr_in_dccm_dc2 | addr_in_pic_dc2);
   assign lsu_busreq_dc2 = lsu_pkt_dc2.valid & (lsu_pkt_dc2.load | lsu_pkt_dc2.store) & addr_external_dc2 & ~flush_dc2_up & ~lsu_exc_dc2;
   assign lsu_pmu_misaligned_dc3 = lsu_pkt_dc3.valid & ((lsu_pkt_dc3.half & lsu_addr_dc3[0]) | (lsu_pkt_dc3.word & (|lsu_addr_dc3[1:0])));
   lsu_dccm_ctl dccm_ctl (
      .lsu_addr_dc1(lsu_addr_dc1[31:0]),
      .end_addr_dc1(end_addr_dc1[DCCM_BITS-1:0]),
      .lsu_addr_dc3(lsu_addr_dc3[DCCM_BITS-1:0]),
      .*
   );
   lsu_stbuf stbuf(
      .lsu_addr_dc1(lsu_addr_dc1[LSU_SB_BITS-1:0]),
      .end_addr_dc1(end_addr_dc1[LSU_SB_BITS-1:0]),
      .lsu_addr_dc2(lsu_addr_dc2[LSU_SB_BITS-1:0]),
      .end_addr_dc2(end_addr_dc2[LSU_SB_BITS-1:0]),
      .lsu_addr_dc3(lsu_addr_dc3[LSU_SB_BITS-1:0]),
      .end_addr_dc3(end_addr_dc3[LSU_SB_BITS-1:0]),
      .*
   );
   lsu_ecc ecc (
      .lsu_addr_dc3(lsu_addr_dc3[DCCM_BITS-1:0]),
      .end_addr_dc3(end_addr_dc3[DCCM_BITS-1:0]),
      .*
   );
   lsu_trigger trigger (
      .store_data_dc3(store_data_dc3[31:0]),
      .*
   );
   lsu_clkdomain clkdomain (.*);
   lsu_bus_intf bus_intf (.*);
   rvdff_fpga #(1) lsu_i0_valid_dc1ff    (.*, .din(dec_i0_lsu_decode_d), .dout(lsu_i0_valid_dc1), .clk(lsu_freeze_c2_dc1_clk), .clken(lsu_freeze_c2_dc1_clken), .rawclk(clk));
   rvdff_fpga #(1) lsu_i0_valid_dc2ff    (.*, .din(lsu_i0_valid_dc1),    .dout(lsu_i0_valid_dc2), .clk(lsu_freeze_c2_dc2_clk), .clken(lsu_freeze_c2_dc2_clken), .rawclk(clk));
   rvdff_fpga #(1) lsu_i0_valid_dc3ff    (.*, .din(lsu_i0_valid_dc2),    .dout(lsu_i0_valid_dc3), .clk(lsu_freeze_c2_dc3_clk), .clken(lsu_freeze_c2_dc3_clken), .rawclk(clk));
   rvdff_fpga #(1) lsu_i0_valid_dc4ff    (.*, .din(lsu_i0_valid_dc3),    .dout(lsu_i0_valid_dc4), .clk(lsu_freeze_c2_dc4_clk), .clken(lsu_freeze_c2_dc4_clken), .rawclk(clk));
   rvdff #(1) lsu_i0_valid_dc5ff    (.*, .din(lsu_i0_valid_dc4),    .dout(lsu_i0_valid_dc5), .clk(lsu_c2_dc5_clk));
   rvdff #(1) lsu_single_ecc_err_dc4(.*, .din(lsu_single_ecc_error_dc3), .dout(lsu_single_ecc_error_dc4), .clk(lsu_c2_dc4_clk));
   rvdff #(1) lsu_single_ecc_err_dc5(.*, .din(lsu_single_ecc_error_dc4), .dout(lsu_single_ecc_error_dc5), .clk(lsu_c2_dc5_clk));
endmodule  
module lsu_clkdomain
   import swerv_types::*;
(
   input logic      clk,                                
   input logic      free_clk,                           
   input logic      rst_l,                              
   input logic      clk_override,                       
   input logic      lsu_freeze_dc3,                     
   input logic      addr_in_dccm_dc2,                   
   input logic      addr_in_pic_dc2,                    
   input logic      dma_dccm_req,                       
   input logic      dma_mem_write,                      
   input logic      load_stbuf_reqvld_dc3,              
   input logic      store_stbuf_reqvld_dc3,             
   input logic      stbuf_reqvld_any,                   
   input logic      stbuf_reqvld_flushed_any,           
   input logic      lsu_busreq_dc5,                     
   input logic      lsu_bus_buffer_pend_any,            
   input logic      lsu_bus_buffer_empty_any,           
   input logic      lsu_stbuf_empty_any,                
   input logic      lsu_bus_clk_en,                
   input lsu_pkt_t  lsu_p,                              
   input lsu_pkt_t  lsu_pkt_dc1,                        
   input lsu_pkt_t  lsu_pkt_dc2,                        
   input lsu_pkt_t  lsu_pkt_dc3,                        
   input lsu_pkt_t  lsu_pkt_dc4,                        
   input lsu_pkt_t  lsu_pkt_dc5,                        
   output logic     lsu_c1_dc3_clk,                     
   output logic     lsu_c1_dc4_clk,                     
   output logic     lsu_c1_dc5_clk,                     
   output logic     lsu_c2_dc3_clk,                     
   output logic     lsu_c2_dc4_clk,                     
   output logic     lsu_c2_dc5_clk,                     
   output logic     lsu_store_c1_dc1_clken,               
   output logic     lsu_store_c1_dc2_clken,               
   output logic     lsu_store_c1_dc3_clken,               
   output logic     lsu_store_c1_dc4_clk,               
   output logic     lsu_store_c1_dc5_clk,               
   output logic     lsu_freeze_c1_dc1_clken,              
   output logic     lsu_freeze_c1_dc2_clken,              
   output logic     lsu_freeze_c1_dc3_clken,              
   output logic     lsu_freeze_c1_dc2_clk,              
   output logic     lsu_freeze_c1_dc3_clk,              
   output logic     lsu_freeze_c2_dc1_clk,
   output logic     lsu_freeze_c2_dc2_clk,
   output logic     lsu_freeze_c2_dc3_clk,
   output logic     lsu_freeze_c2_dc4_clk,
   output logic     lsu_freeze_c2_dc1_clken,
   output logic     lsu_freeze_c2_dc2_clken,
   output logic     lsu_freeze_c2_dc3_clken,
   output logic     lsu_freeze_c2_dc4_clken,
   output logic     lsu_dccm_c1_dc3_clk,                
   output logic     lsu_dccm_c1_dc3_clken,
   output logic     lsu_pic_c1_dc3_clken,               
   output logic     lsu_stbuf_c1_clk,
   output logic     lsu_bus_obuf_c1_clk,                
   output logic     lsu_bus_ibuf_c1_clk,                
   output logic     lsu_bus_buf_c1_clk,                 
   output logic     lsu_busm_clk,                       
   output logic     lsu_free_c2_clk,
   input  logic     scan_mode
);
   logic lsu_c1_dc1_clken, lsu_c1_dc2_clken, lsu_c1_dc3_clken, lsu_c1_dc4_clken, lsu_c1_dc5_clken;
   logic lsu_c2_dc3_clken, lsu_c2_dc4_clken, lsu_c2_dc5_clken;
   logic lsu_c1_dc1_clken_q, lsu_c1_dc2_clken_q, lsu_c1_dc3_clken_q, lsu_c1_dc4_clken_q, lsu_c1_dc5_clken_q;
   logic lsu_store_c1_dc4_clken, lsu_store_c1_dc5_clken;
   logic lsu_freeze_c1_dc4_clken;
   logic lsu_freeze_c1_dc1_clken_q, lsu_freeze_c1_dc2_clken_q, lsu_freeze_c1_dc3_clken_q, lsu_freeze_c1_dc4_clken_q;
   logic lsu_stbuf_c1_clken;
   logic lsu_bus_ibuf_c1_clken, lsu_bus_obuf_c1_clken, lsu_bus_buf_c1_clken;
   logic lsu_free_c1_clken, lsu_free_c1_clken_q, lsu_free_c2_clken;
   logic lsu_bus_valid_clken;
   assign lsu_c1_dc1_clken = lsu_p.valid | dma_dccm_req | clk_override;
   assign lsu_c1_dc2_clken = lsu_pkt_dc1.valid | lsu_c1_dc1_clken_q | clk_override;
   assign lsu_c1_dc3_clken = lsu_pkt_dc2.valid | lsu_c1_dc2_clken_q | clk_override;
   assign lsu_c1_dc4_clken = lsu_pkt_dc3.valid | lsu_c1_dc3_clken_q | clk_override;
   assign lsu_c1_dc5_clken = lsu_pkt_dc4.valid | lsu_c1_dc4_clken_q | clk_override;
   assign lsu_c2_dc3_clken = lsu_c1_dc3_clken | lsu_c1_dc3_clken_q | clk_override;
   assign lsu_c2_dc4_clken = lsu_c1_dc4_clken | lsu_c1_dc4_clken_q | clk_override;
   assign lsu_c2_dc5_clken = lsu_c1_dc5_clken | lsu_c1_dc5_clken_q | clk_override;
   assign lsu_store_c1_dc1_clken = ((lsu_c1_dc1_clken & (lsu_p.store | dma_mem_write)) | clk_override) & ~lsu_freeze_dc3;
   assign lsu_store_c1_dc2_clken = ((lsu_c1_dc2_clken & lsu_pkt_dc1.store) | clk_override) & ~lsu_freeze_dc3;
   assign lsu_store_c1_dc3_clken = ((lsu_c1_dc3_clken & lsu_pkt_dc2.store) | clk_override) & ~lsu_freeze_dc3;
   assign lsu_store_c1_dc4_clken = (lsu_c1_dc4_clken & lsu_pkt_dc3.store) | clk_override;
   assign lsu_store_c1_dc5_clken = (lsu_c1_dc5_clken & lsu_pkt_dc4.store) | clk_override;
   assign lsu_freeze_c1_dc1_clken = (lsu_p.valid | dma_dccm_req | clk_override) & ~lsu_freeze_dc3;
   assign lsu_freeze_c1_dc2_clken = (lsu_pkt_dc1.valid | clk_override) & ~lsu_freeze_dc3;
   assign lsu_freeze_c1_dc3_clken = (lsu_pkt_dc2.valid | clk_override) & ~lsu_freeze_dc3;
   assign lsu_freeze_c1_dc4_clken = (lsu_pkt_dc3.valid | clk_override) & ~lsu_freeze_dc3;
   assign lsu_freeze_c2_dc1_clken = (lsu_freeze_c1_dc1_clken | lsu_freeze_c1_dc1_clken_q | clk_override) & ~lsu_freeze_dc3;
   assign lsu_freeze_c2_dc2_clken = (lsu_freeze_c1_dc2_clken | lsu_freeze_c1_dc2_clken_q | clk_override) & ~lsu_freeze_dc3;
   assign lsu_freeze_c2_dc3_clken = (lsu_freeze_c1_dc3_clken | lsu_freeze_c1_dc3_clken_q | clk_override) & ~lsu_freeze_dc3;
   assign lsu_freeze_c2_dc4_clken = (lsu_freeze_c1_dc4_clken | lsu_freeze_c1_dc4_clken_q | clk_override) & ~lsu_freeze_dc3;
   assign lsu_stbuf_c1_clken = load_stbuf_reqvld_dc3 | store_stbuf_reqvld_dc3 | stbuf_reqvld_any | stbuf_reqvld_flushed_any | clk_override;
   assign lsu_bus_ibuf_c1_clken = lsu_busreq_dc5 | clk_override;
   assign lsu_bus_obuf_c1_clken = ((lsu_bus_buffer_pend_any | lsu_busreq_dc5) & lsu_bus_clk_en) | clk_override;
   assign lsu_bus_buf_c1_clken  = ~lsu_bus_buffer_empty_any | lsu_busreq_dc5 | clk_override;
   assign lsu_dccm_c1_dc3_clken = ((lsu_c1_dc3_clken & addr_in_dccm_dc2) | clk_override) & ~lsu_freeze_dc3;
   assign lsu_pic_c1_dc3_clken  = ((lsu_c1_dc3_clken & addr_in_pic_dc2) | clk_override) & ~lsu_freeze_dc3;
   assign lsu_free_c1_clken = (lsu_p.valid | lsu_pkt_dc1.valid | lsu_pkt_dc2.valid | lsu_pkt_dc3.valid | lsu_pkt_dc4.valid | lsu_pkt_dc5.valid) |
                              ~lsu_bus_buffer_empty_any | ~lsu_stbuf_empty_any | clk_override;
   assign lsu_free_c2_clken = lsu_free_c1_clken | lsu_free_c1_clken_q | clk_override;
   rvdff #(1) lsu_free_c1_clkenff (.din(lsu_free_c1_clken), .dout(lsu_free_c1_clken_q), .clk(free_clk), .*);
   rvdff #(1) lsu_c1_dc1_clkenff (.din(lsu_c1_dc1_clken), .dout(lsu_c1_dc1_clken_q), .clk(lsu_free_c2_clk), .*);
   rvdff #(1) lsu_c1_dc2_clkenff (.din(lsu_c1_dc2_clken), .dout(lsu_c1_dc2_clken_q), .clk(lsu_free_c2_clk), .*);
   rvdff #(1) lsu_c1_dc3_clkenff (.din(lsu_c1_dc3_clken), .dout(lsu_c1_dc3_clken_q), .clk(lsu_free_c2_clk), .*);
   rvdff #(1) lsu_c1_dc4_clkenff (.din(lsu_c1_dc4_clken), .dout(lsu_c1_dc4_clken_q), .clk(lsu_free_c2_clk), .*);
   rvdff #(1) lsu_c1_dc5_clkenff (.din(lsu_c1_dc5_clken), .dout(lsu_c1_dc5_clken_q), .clk(lsu_free_c2_clk), .*);
   rvdff_fpga #(1) lsu_freeze_c1_dc1_clkenff (.din(lsu_freeze_c1_dc1_clken), .dout(lsu_freeze_c1_dc1_clken_q), .clk(lsu_freeze_c2_dc1_clk), .clken(lsu_freeze_c2_dc1_clken), .rawclk(clk), .*);
   rvdff_fpga #(1) lsu_freeze_c1_dc2_clkenff (.din(lsu_freeze_c1_dc2_clken), .dout(lsu_freeze_c1_dc2_clken_q), .clk(lsu_freeze_c2_dc2_clk), .clken(lsu_freeze_c2_dc2_clken), .rawclk(clk), .*);
   rvdff_fpga #(1) lsu_freeze_c1_dc3_clkenff (.din(lsu_freeze_c1_dc3_clken), .dout(lsu_freeze_c1_dc3_clken_q), .clk(lsu_freeze_c2_dc3_clk), .clken(lsu_freeze_c2_dc3_clken), .rawclk(clk), .*);
   rvdff_fpga #(1) lsu_freeze_c1_dc4_clkenff (.din(lsu_freeze_c1_dc4_clken), .dout(lsu_freeze_c1_dc4_clken_q), .clk(lsu_freeze_c2_dc4_clk), .clken(lsu_freeze_c2_dc4_clken), .rawclk(clk), .*);
   rvoclkhdr lsu_c1dc3_cgc ( .en(lsu_c1_dc3_clken), .l1clk(lsu_c1_dc3_clk), .* );
   rvoclkhdr lsu_c1dc4_cgc ( .en(lsu_c1_dc4_clken), .l1clk(lsu_c1_dc4_clk), .* );
   rvoclkhdr lsu_c1dc5_cgc ( .en(lsu_c1_dc5_clken), .l1clk(lsu_c1_dc5_clk), .* );
   rvoclkhdr lsu_c2dc3_cgc ( .en(lsu_c2_dc3_clken), .l1clk(lsu_c2_dc3_clk), .* );
   rvoclkhdr lsu_c2dc4_cgc ( .en(lsu_c2_dc4_clken), .l1clk(lsu_c2_dc4_clk), .* );
   rvoclkhdr lsu_c2dc5_cgc ( .en(lsu_c2_dc5_clken), .l1clk(lsu_c2_dc5_clk), .* );
   rvoclkhdr lsu_store_c1dc4_cgc (.en(lsu_store_c1_dc4_clken), .l1clk(lsu_store_c1_dc4_clk), .*);
   rvoclkhdr lsu_store_c1dc5_cgc (.en(lsu_store_c1_dc5_clken), .l1clk(lsu_store_c1_dc5_clk), .*);
   assign lsu_freeze_c1_dc2_clk = 1'b0;
   assign lsu_freeze_c1_dc3_clk = 1'b0;
   assign lsu_freeze_c2_dc1_clk = 1'b0;
   assign lsu_freeze_c2_dc2_clk = 1'b0;
   assign lsu_freeze_c2_dc3_clk = 1'b0;
   assign lsu_freeze_c2_dc4_clk = 1'b0;
   assign lsu_busm_clk = 1'b0;
   assign lsu_dccm_c1_dc3_clk = 1'b0;
   rvoclkhdr lsu_stbuf_c1_cgc ( .en(lsu_stbuf_c1_clken), .l1clk(lsu_stbuf_c1_clk), .* );
   rvoclkhdr lsu_bus_ibuf_c1_cgc ( .en(lsu_bus_ibuf_c1_clken), .l1clk(lsu_bus_ibuf_c1_clk), .* );
   rvoclkhdr lsu_bus_obuf_c1_cgc ( .en(lsu_bus_obuf_c1_clken), .l1clk(lsu_bus_obuf_c1_clk), .* );
   rvoclkhdr lsu_bus_buf_c1_cgc  ( .en(lsu_bus_buf_c1_clken),  .l1clk(lsu_bus_buf_c1_clk), .* );
   rvoclkhdr lsu_free_cgc (.en(lsu_free_c2_clken), .l1clk(lsu_free_c2_clk), .*);
endmodule
module lsu_addrcheck
   import swerv_types::*;
(
   input logic         lsu_freeze_c2_dc2_clk,        
   input logic         lsu_freeze_c2_dc3_clk,
   input logic         lsu_freeze_c2_dc2_clken,
   input logic         lsu_freeze_c2_dc3_clken,
   input logic         rst_l,                        
   input logic         clk,
   input logic [31:0]  start_addr_dc1,               
   input logic [31:0]  end_addr_dc1,                 
   input lsu_pkt_t     lsu_pkt_dc1,                  
   input logic [31:0]  dec_tlu_mrac_ff,              
   output logic        is_sideeffects_dc2,           
   output logic        is_sideeffects_dc3,
   output logic        addr_in_dccm_dc1,             
   output logic        addr_in_pic_dc1,              
   output logic        addr_external_dc1,            
   output logic        access_fault_dc1,             
   output logic        misaligned_fault_dc1,         
   input  logic        scan_mode
);
localparam TOTAL_INT        = 9;
localparam DCCM_BITS        = 16;
localparam DCCM_BANK_BITS   = 3;
localparam DCCM_NUM_BANKS   = 8;
localparam DCCM_DATA_WIDTH  = 32;
localparam DCCM_FDATA_WIDTH = 39;
localparam DCCM_BYTE_WIDTH  = 4;
localparam DCCM_ECC_WIDTH   = 7;
localparam LSU_RDBUF_DEPTH  = 8;
localparam DMA_BUF_DEPTH    = 4;
localparam LSU_STBUF_DEPTH  = 8;
localparam LSU_SB_BITS      = 16;
localparam DEC_INSTBUF_DEPTH = 4;
localparam ICCM_SIZE         = 512;
localparam ICCM_BITS         = 19;
localparam ICCM_NUM_BANKS    = 8;
localparam ICCM_BANK_BITS    = 3;
localparam ICCM_INDEX_BITS   = 14;
localparam ICCM_BANK_HI      = 4 + (3/4);
localparam ICACHE_TAG_HIGH  = 12;
localparam ICACHE_TAG_LOW   = 6;
localparam ICACHE_IC_DEPTH  = 8;
localparam ICACHE_TAG_DEPTH = 64;
localparam LSU_BUS_TAG     = 4;
localparam DMA_BUS_TAG     = 1;
localparam SB_BUS_TAG      = 1;
localparam IFU_BUS_TAG     = 3;
   localparam DCCM_REGION = 4'hf;
   localparam PIC_REGION  = 4'hf;
   localparam ICCM_REGION = 4'he;
       localparam ICCM_ENABLE = 1'b0;
      localparam DCCM_ENABLE = 1'b1;
   logic        is_sideeffects_dc1, is_aligned_dc1;
   logic        start_addr_in_dccm_dc1, end_addr_in_dccm_dc1;
   logic        start_addr_in_dccm_region_dc1, end_addr_in_dccm_region_dc1;
   logic        start_addr_in_pic_dc1, end_addr_in_pic_dc1;
   logic        start_addr_in_pic_region_dc1, end_addr_in_pic_region_dc1;
   logic [4:0]  csr_idx;
   logic        addr_in_iccm;
   logic        non_dccm_access_ok;
   if (DCCM_ENABLE == 1) begin: Gen_dccm_enable
      rvrangecheck #(.CCM_SADR(32'hf0040000),
                     .CCM_SIZE(64)) start_addr_dccm_rangecheck (
         .addr(start_addr_dc1[31:0]),
         .in_range(start_addr_in_dccm_dc1),
         .in_region(start_addr_in_dccm_region_dc1)
      );
      rvrangecheck #(.CCM_SADR(32'hf0040000),
                     .CCM_SIZE(64)) end_addr_dccm_rangecheck (
         .addr(end_addr_dc1[31:0]),
         .in_range(end_addr_in_dccm_dc1),
         .in_region(end_addr_in_dccm_region_dc1)
      );
   end else begin: Gen_dccm_disable  
      assign start_addr_in_dccm_dc1 = '0;
      assign start_addr_in_dccm_region_dc1 = '0;
      assign end_addr_in_dccm_dc1 = '0;
      assign end_addr_in_dccm_region_dc1 = '0;
   end
    if (ICCM_ENABLE == 1) begin : check_iccm
     assign addr_in_iccm =  (start_addr_dc1[31:28] == ICCM_REGION);
  end
  else begin
     assign addr_in_iccm = 1'b0;
  end
   rvrangecheck #(.CCM_SADR(32'hf00c0000),
                  .CCM_SIZE(32)) start_addr_pic_rangecheck (
      .addr(start_addr_dc1[31:0]),
      .in_range(start_addr_in_pic_dc1),
      .in_region(start_addr_in_pic_region_dc1)
   );
   rvrangecheck #(.CCM_SADR(32'hf00c0000),
                  .CCM_SIZE(32)) end_addr_pic_rangecheck (
      .addr(end_addr_dc1[31:0]),
      .in_range(end_addr_in_pic_dc1),
      .in_region(end_addr_in_pic_region_dc1)
   );
   assign addr_in_dccm_dc1        = (start_addr_in_dccm_dc1 & end_addr_in_dccm_dc1);
   assign addr_in_pic_dc1         = (start_addr_in_pic_dc1 & end_addr_in_pic_dc1);
   assign addr_external_dc1   = ~(addr_in_dccm_dc1 | addr_in_pic_dc1);   
   assign csr_idx[4:0]       = {start_addr_dc1[31:28], 1'b1};
   assign is_sideeffects_dc1 = dec_tlu_mrac_ff[csr_idx] & ~(start_addr_in_dccm_region_dc1 | start_addr_in_pic_region_dc1 | addr_in_iccm);   
   assign is_aligned_dc1    = (lsu_pkt_dc1.word & (start_addr_dc1[1:0] == 2'b0)) |
                              (lsu_pkt_dc1.half & (start_addr_dc1[0] == 1'b0)) |
                              lsu_pkt_dc1.by;
    assign non_dccm_access_ok = (~(|{1'h0,1'h0,1'h0,1'h0,1'h0,1'h0,1'h0,1'h0})) |
                             (((1'h0 & ((start_addr_dc1[31:0] | 'hffffffff)) == ('h00000000 | 'hffffffff)) |
                               (1'h0 & ((start_addr_dc1[31:0] | 'hffffffff)) == ('h00000000 | 'hffffffff)) |
                               (1'h0 & ((start_addr_dc1[31:0] | 'hffffffff)) == ('h00000000 | 'hffffffff)) |
                               (1'h0 & ((start_addr_dc1[31:0] | 'hffffffff)) == ('h00000000 | 'hffffffff)) |
                               (1'h0 & ((start_addr_dc1[31:0] | 'hffffffff)) == ('h00000000 | 'hffffffff)) |
                               (1'h0 & ((start_addr_dc1[31:0] | 'hffffffff)) == ('h00000000 | 'hffffffff)) |
                               (1'h0 & ((start_addr_dc1[31:0] | 'hffffffff)) == ('h00000000 | 'hffffffff)) |
                               (1'h0 & ((start_addr_dc1[31:0] | 'hffffffff)) == ('h00000000 | 'hffffffff)))        &
                              ((1'h0 & ((end_addr_dc1[31:0]   | 'hffffffff)) == ('h00000000 | 'hffffffff)) |
                               (1'h0 & ((end_addr_dc1[31:0]   | 'hffffffff)) == ('h00000000 | 'hffffffff)) |
                               (1'h0 & ((end_addr_dc1[31:0]   | 'hffffffff)) == ('h00000000 | 'hffffffff)) |
                               (1'h0 & ((end_addr_dc1[31:0]   | 'hffffffff)) == ('h00000000 | 'hffffffff)) |
                               (1'h0 & ((end_addr_dc1[31:0]   | 'hffffffff)) == ('h00000000 | 'hffffffff)) |
                               (1'h0 & ((end_addr_dc1[31:0]   | 'hffffffff)) == ('h00000000 | 'hffffffff)) |
                               (1'h0 & ((end_addr_dc1[31:0]   | 'hffffffff)) == ('h00000000 | 'hffffffff)) |
                               (1'h0 & ((end_addr_dc1[31:0]   | 'hffffffff)) == ('h00000000 | 'hffffffff))));
   if (DCCM_REGION == PIC_REGION) begin
      assign access_fault_dc1  = ((start_addr_in_dccm_region_dc1 & ~(start_addr_in_dccm_dc1 | start_addr_in_pic_dc1)) |
                                  (end_addr_in_dccm_region_dc1 & ~(end_addr_in_dccm_dc1 | end_addr_in_pic_dc1))       |
                                  (start_addr_in_dccm_dc1 & end_addr_in_pic_dc1)                                      |
                                  (start_addr_in_pic_dc1  & end_addr_in_dccm_dc1)                                     |
                                  ((addr_in_pic_dc1 & ((start_addr_dc1[1:0] != 2'b0) | ~lsu_pkt_dc1.word))) |
                                  (~start_addr_in_dccm_region_dc1 & ~non_dccm_access_ok)) & lsu_pkt_dc1.valid & ~lsu_pkt_dc1.dma;
   end else begin
      assign access_fault_dc1  = ((start_addr_in_dccm_region_dc1 & ~start_addr_in_dccm_dc1) |
                                  (end_addr_in_dccm_region_dc1 & ~end_addr_in_dccm_dc1)     |
                                  (start_addr_in_pic_region_dc1 & ~start_addr_in_pic_dc1)   |
                                  (end_addr_in_pic_region_dc1 & ~end_addr_in_pic_dc1)       |
                                  ((addr_in_pic_dc1 & ((start_addr_dc1[1:0] != 2'b0) | ~lsu_pkt_dc1.word))) |
                                  (~start_addr_in_pic_region_dc1 & ~start_addr_in_dccm_region_dc1 & ~non_dccm_access_ok)) & lsu_pkt_dc1.valid & ~lsu_pkt_dc1.dma;
   end
   assign misaligned_fault_dc1 = ((start_addr_dc1[31:28] != end_addr_dc1[31:28]) |
                                  (is_sideeffects_dc1 & ~is_aligned_dc1)) & addr_external_dc1 & lsu_pkt_dc1.valid & ~lsu_pkt_dc1.dma;
   rvdff_fpga #(.WIDTH(1)) is_sideeffects_dc2ff (.din(is_sideeffects_dc1), .dout(is_sideeffects_dc2), .clk(lsu_freeze_c2_dc2_clk), .clken(lsu_freeze_c2_dc2_clken), .rawclk(clk), .*);
   rvdff_fpga #(.WIDTH(1)) is_sideeffects_dc3ff (.din(is_sideeffects_dc2), .dout(is_sideeffects_dc3), .clk(lsu_freeze_c2_dc3_clk), .clken(lsu_freeze_c2_dc3_clken), .rawclk(clk), .*);
endmodule  
module lsu_lsc_ctl
   import swerv_types::*;
(
   input logic         rst_l,
   input logic         clk,
   input logic        lsu_c1_dc4_clk,
   input logic        lsu_c1_dc5_clk,
   input logic        lsu_c2_dc4_clk,
   input logic        lsu_c2_dc5_clk,
   input logic        lsu_freeze_c1_dc1_clken,
   input logic        lsu_freeze_c1_dc2_clken,
   input logic        lsu_freeze_c1_dc2_clk,
   input logic        lsu_freeze_c1_dc3_clken,
   input logic        lsu_freeze_c1_dc3_clk,
   input logic        lsu_freeze_c2_dc1_clk,
   input logic        lsu_freeze_c2_dc2_clk,
   input logic        lsu_freeze_c2_dc3_clk,
   input logic        lsu_freeze_c2_dc1_clken,
   input logic        lsu_freeze_c2_dc2_clken,
   input logic        lsu_freeze_c2_dc3_clken,
   input logic        lsu_store_c1_dc1_clken,
   input logic        lsu_store_c1_dc2_clken,
   input logic        lsu_store_c1_dc3_clken,
   input logic        lsu_store_c1_dc4_clk,
   input logic        lsu_store_c1_dc5_clk,
   input logic [31:0] i0_result_e4_eff,
   input logic [31:0] i1_result_e4_eff,
   input logic [31:0] i0_result_e2,
   input logic         ld_bus_error_dc3,
   input logic [31:0]  ld_bus_error_addr_dc3,
   input logic         lsu_single_ecc_error_dc3,
   input logic         lsu_single_ecc_error_dc5,
   input logic         lsu_double_ecc_error_dc3,
   input logic         lsu_freeze_dc3,
   input logic         lsu_i0_valid_dc3,
   input logic         flush_dc2_up,
   input logic         flush_dc3,
   input logic         flush_dc4,
   input logic         flush_dc5,
   input logic [31:0]  exu_lsu_rs1_d,  
   input logic [31:0]  exu_lsu_rs2_d,  
   input lsu_pkt_t     lsu_p,          
   input logic [11:0]  dec_lsu_offset_d,
   input  logic [31:0] picm_mask_data_dc3,
   input  logic [31:0] lsu_ld_data_dc3,
   input  logic [31:0] lsu_ld_data_corr_dc3,
   input  logic [31:0]  bus_read_data_dc3,
   output logic [31:0] lsu_result_dc3,
   output logic [31:0] lsu_result_corr_dc4,    
   output logic [31:0] lsu_addr_dc1,
   output logic [31:0] lsu_addr_dc2,
   output logic [31:0] lsu_addr_dc3,
   output logic [31:0] lsu_addr_dc4,
   output logic [31:0] lsu_addr_dc5,
   output logic [31:0] end_addr_dc1,
   output logic [31:0] end_addr_dc2,
   output logic [31:0] end_addr_dc3,
   output logic [31:0] end_addr_dc4,
   output logic [31:0] end_addr_dc5,
   output logic [63:0] store_data_dc2,
   output logic [63:0] store_data_dc3,
   output logic [31:0] store_data_dc4,
   output logic [31:0] store_data_dc5,
   input  logic [31:0]    dec_tlu_mrac_ff,
   output logic           lsu_exc_dc2,
   output lsu_error_pkt_t lsu_error_pkt_dc3,
   output logic           lsu_single_ecc_error_incr,     
   output logic           lsu_freeze_external_ints_dc3,
   output logic           is_sideeffects_dc2,
   output logic           is_sideeffects_dc3,
   output logic           lsu_commit_dc5,
   output logic        addr_in_dccm_dc1,
   output logic        addr_in_dccm_dc2,
   output logic        addr_in_dccm_dc3,
   output logic        addr_in_pic_dc1,
   output logic        addr_in_pic_dc2,
   output logic        addr_in_pic_dc3,
   output logic        addr_external_dc2,
   output logic        addr_external_dc3,
   output logic        addr_external_dc4,
   output logic        addr_external_dc5,
   input logic        dma_dccm_req,
   input logic [31:0] dma_mem_addr,
   input logic [2:0]  dma_mem_sz,
   input logic        dma_mem_write,
   input logic [63:0] dma_mem_wdata,
   output lsu_pkt_t    lsu_pkt_dc1,
   output lsu_pkt_t    lsu_pkt_dc2,
   output lsu_pkt_t    lsu_pkt_dc3,
   output lsu_pkt_t    lsu_pkt_dc4,
   output lsu_pkt_t    lsu_pkt_dc5,
   input  logic        scan_mode
   );
localparam TOTAL_INT        = 9;
localparam DCCM_BITS        = 16;
localparam DCCM_BANK_BITS   = 3;
localparam DCCM_NUM_BANKS   = 8;
localparam DCCM_DATA_WIDTH  = 32;
localparam DCCM_FDATA_WIDTH = 39;
localparam DCCM_BYTE_WIDTH  = 4;
localparam DCCM_ECC_WIDTH   = 7;
localparam LSU_RDBUF_DEPTH  = 8;
localparam DMA_BUF_DEPTH    = 4;
localparam LSU_STBUF_DEPTH  = 8;
localparam LSU_SB_BITS      = 16;
localparam DEC_INSTBUF_DEPTH = 4;
localparam ICCM_SIZE         = 512;
localparam ICCM_BITS         = 19;
localparam ICCM_NUM_BANKS    = 8;
localparam ICCM_BANK_BITS    = 3;
localparam ICCM_INDEX_BITS   = 14;
localparam ICCM_BANK_HI      = 4 + (3/4);
localparam ICACHE_TAG_HIGH  = 12;
localparam ICACHE_TAG_LOW   = 6;
localparam ICACHE_IC_DEPTH  = 8;
localparam ICACHE_TAG_DEPTH = 64;
localparam LSU_BUS_TAG     = 4;
localparam DMA_BUS_TAG     = 1;
localparam SB_BUS_TAG      = 1;
localparam IFU_BUS_TAG     = 3;
   logic [31:0]        full_addr_dc1;
   logic [31:0]        full_end_addr_dc1;
   logic [31:0]        lsu_rs1_d;
   logic [11:0]        lsu_offset_d;
   logic [31:0]        rs1_dc1;
   logic [11:0]        offset_dc1;
   logic [12:0]        end_addr_offset_dc1;
   logic [31:0]        lsu_ld_datafn_dc3;
   logic [31:0]        lsu_ld_datafn_corr_dc3;
   logic [31:0]        lsu_result_corr_dc3;
   logic [2:0]         addr_offset_dc1;
   logic [63:0]        dma_mem_wdata_shifted;
   logic               addr_external_dc1;
   logic               access_fault_dc1, misaligned_fault_dc1;
   logic               access_fault_dc2, misaligned_fault_dc2;
   logic               access_fault_dc3, misaligned_fault_dc3;
   logic [63:0]        store_data_d;
   logic [63:0]        store_data_dc1;
   logic [63:0]        store_data_pre_dc2;
   logic [63:0]        store_data_pre_dc3;
   logic [63:0]        store_data_dc2_in;
   logic [31:0]        rs1_dc1_raw;
   lsu_pkt_t           dma_pkt_d;
   lsu_pkt_t           lsu_pkt_dc1_in, lsu_pkt_dc2_in, lsu_pkt_dc3_in, lsu_pkt_dc4_in, lsu_pkt_dc5_in;
   assign lsu_rs1_d[31:0] = dma_dccm_req ? dma_mem_addr[31:0] : exu_lsu_rs1_d[31:0];
   assign lsu_offset_d[11:0] = dec_lsu_offset_d[11:0] & ~{12{dma_dccm_req}};
   rvdffe #(32) rs1ff (.*, .din(lsu_rs1_d[31:0]), .dout(rs1_dc1_raw[31:0]), .en(lsu_freeze_c1_dc1_clken));
   rvdffe #(12) offsetff (.*, .din(lsu_offset_d[11:0]), .dout(offset_dc1[11:0]), .en(lsu_freeze_c1_dc1_clken));
   assign rs1_dc1[31:0] = (lsu_pkt_dc1.load_ldst_bypass_c1) ? lsu_result_dc3[31:0] : rs1_dc1_raw[31:0];
   rvlsadder   lsadder  (.rs1(rs1_dc1[31:0]),
                       .offset(offset_dc1[11:0]),
                       .dout(full_addr_dc1[31:0])
                       );
   lsu_addrcheck addrcheck (
              .start_addr_dc1(full_addr_dc1[31:0]),
              .end_addr_dc1(full_end_addr_dc1[31:0]),
              .*
  );
   assign addr_offset_dc1[2:0]      = ({3{lsu_pkt_dc1.half}} & 3'b01) | ({3{lsu_pkt_dc1.word}} & 3'b11) | ({3{lsu_pkt_dc1.dword}} & 3'b111);
   assign end_addr_offset_dc1[12:0] = {offset_dc1[11],offset_dc1[11:0]} + {9'b0,addr_offset_dc1[2:0]};
   assign full_end_addr_dc1[31:0]   = rs1_dc1[31:0] + {{19{end_addr_offset_dc1[12]}},end_addr_offset_dc1[12:0]};
   assign end_addr_dc1[31:0]        = full_end_addr_dc1[31:0];
   assign lsu_exc_dc2               = access_fault_dc2 | misaligned_fault_dc2;
   assign lsu_freeze_external_ints_dc3 = lsu_freeze_dc3 & is_sideeffects_dc3;
   assign lsu_single_ecc_error_incr = lsu_single_ecc_error_dc5 & (lsu_commit_dc5 | lsu_pkt_dc5.dma);
   assign lsu_error_pkt_dc3.exc_valid = (access_fault_dc3 | misaligned_fault_dc3 | ld_bus_error_dc3 | lsu_double_ecc_error_dc3) & lsu_pkt_dc3.valid & ~lsu_pkt_dc3.dma & ~flush_dc3;
   assign lsu_error_pkt_dc3.single_ecc_error = lsu_single_ecc_error_dc3 & ~(access_fault_dc3 | misaligned_fault_dc3 | lsu_double_ecc_error_dc3);
   assign lsu_error_pkt_dc3.inst_type = lsu_pkt_dc3.store;
   assign lsu_error_pkt_dc3.dma_valid = lsu_pkt_dc3.dma;
   assign lsu_error_pkt_dc3.inst_pipe = ~lsu_i0_valid_dc3;
   assign lsu_error_pkt_dc3.exc_type  = ~misaligned_fault_dc3;
   assign lsu_error_pkt_dc3.addr[31:0] = lsu_addr_dc3[31:0];
   assign dma_pkt_d.valid   = dma_dccm_req;
   assign dma_pkt_d.dma     = 1'b1;
   assign dma_pkt_d.unsign  = '0;
   assign dma_pkt_d.store   = dma_mem_write;
   assign dma_pkt_d.load    = ~dma_mem_write;
   assign dma_pkt_d.by      = (dma_mem_sz[2:0] == 3'b0);
   assign dma_pkt_d.half    = (dma_mem_sz[2:0] == 3'b1);
   assign dma_pkt_d.word    = (dma_mem_sz[2:0] == 3'b10);
   assign dma_pkt_d.dword   = (dma_mem_sz[2:0] == 3'b11);
   assign dma_pkt_d.load_ldst_bypass_c1  = '0;
   assign dma_pkt_d.store_data_bypass_c1 = '0;
   assign dma_pkt_d.store_data_bypass_c2 = '0;
   assign dma_pkt_d.store_data_bypass_i0_e2_c2 = '0;
   assign dma_pkt_d.store_data_bypass_e4_c1 = '0;
   assign dma_pkt_d.store_data_bypass_e4_c2 = '0;
   assign dma_pkt_d.store_data_bypass_e4_c3 = '0;
   always_comb begin
      lsu_pkt_dc1_in = dma_dccm_req ? dma_pkt_d : lsu_p;
      lsu_pkt_dc2_in = lsu_pkt_dc1;
      lsu_pkt_dc3_in = lsu_pkt_dc2;
      lsu_pkt_dc4_in = lsu_pkt_dc3;
      lsu_pkt_dc5_in = lsu_pkt_dc4;
      lsu_pkt_dc1_in.valid = (lsu_p.valid & ~flush_dc2_up) | dma_dccm_req;
      lsu_pkt_dc2_in.valid = lsu_pkt_dc1.valid & ~(flush_dc2_up & ~lsu_pkt_dc1.dma);
      lsu_pkt_dc3_in.valid = lsu_pkt_dc2.valid & ~(flush_dc2_up & ~lsu_pkt_dc2.dma);
      lsu_pkt_dc4_in.valid = lsu_pkt_dc3.valid & ~(flush_dc3 & ~lsu_pkt_dc3.dma) & ~lsu_freeze_dc3;
      lsu_pkt_dc5_in.valid = lsu_pkt_dc4.valid & ~(flush_dc4 & ~lsu_pkt_dc4.dma);
   end
   rvdff #(1) lsu_pkt_vlddc4ff (.*, .din(lsu_pkt_dc4_in.valid), .dout(lsu_pkt_dc4.valid), .clk(lsu_c2_dc4_clk));
   rvdff #(1) lsu_pkt_vlddc5ff (.*, .din(lsu_pkt_dc5_in.valid), .dout(lsu_pkt_dc5.valid), .clk(lsu_c2_dc5_clk));
   rvdffe #($bits(lsu_pkt_t)-1) lsu_pkt_dc1ff (.*, .din(lsu_pkt_dc1_in[$bits(lsu_pkt_t)-1:1]), .dout(lsu_pkt_dc1[$bits(lsu_pkt_t)-1:1]), .en(lsu_freeze_c1_dc1_clken));
   rvdffe #($bits(lsu_pkt_t)-1) lsu_pkt_dc2ff (.*, .din(lsu_pkt_dc2_in[$bits(lsu_pkt_t)-1:1]), .dout(lsu_pkt_dc2[$bits(lsu_pkt_t)-1:1]), .en(lsu_freeze_c1_dc2_clken));
   rvdffe #($bits(lsu_pkt_t)-1) lsu_pkt_dc3ff (.*, .din(lsu_pkt_dc3_in[$bits(lsu_pkt_t)-1:1]), .dout(lsu_pkt_dc3[$bits(lsu_pkt_t)-1:1]), .en(lsu_freeze_c1_dc3_clken));
   rvdff #($bits(lsu_pkt_t)-1) lsu_pkt_dc4ff (.*, .din(lsu_pkt_dc4_in[$bits(lsu_pkt_t)-1:1]), .dout(lsu_pkt_dc4[$bits(lsu_pkt_t)-1:1]), .clk(lsu_c1_dc4_clk));
   rvdff #($bits(lsu_pkt_t)-1) lsu_pkt_dc5ff (.*, .din(lsu_pkt_dc5_in[$bits(lsu_pkt_t)-1:1]), .dout(lsu_pkt_dc5[$bits(lsu_pkt_t)-1:1]), .clk(lsu_c1_dc5_clk));
   assign lsu_ld_datafn_dc3[31:0] = addr_external_dc3 ? bus_read_data_dc3[31:0] : lsu_ld_data_dc3[31:0];
   assign lsu_ld_datafn_corr_dc3[31:0] = addr_external_dc3 ? bus_read_data_dc3[31:0] : lsu_ld_data_corr_dc3[31:0];
   assign lsu_result_dc3[31:0] = ({32{ lsu_pkt_dc3.unsign & lsu_pkt_dc3.by  }} & {24'b0,lsu_ld_datafn_dc3[7:0]}) |
                                 ({32{ lsu_pkt_dc3.unsign & lsu_pkt_dc3.half}} & {16'b0,lsu_ld_datafn_dc3[15:0]}) |
                                 ({32{~lsu_pkt_dc3.unsign & lsu_pkt_dc3.by  }} & {{24{  lsu_ld_datafn_dc3[7]}}, lsu_ld_datafn_dc3[7:0]}) |
                                 ({32{~lsu_pkt_dc3.unsign & lsu_pkt_dc3.half}} & {{16{  lsu_ld_datafn_dc3[15]}},lsu_ld_datafn_dc3[15:0]}) |
                                 ({32{lsu_pkt_dc3.word}} &                       lsu_ld_datafn_dc3[31:0]);
   assign lsu_result_corr_dc3[31:0] = ({32{ lsu_pkt_dc3.unsign & lsu_pkt_dc3.by  }} & {24'b0,lsu_ld_datafn_corr_dc3[7:0]}) |
                                      ({32{ lsu_pkt_dc3.unsign & lsu_pkt_dc3.half}} & {16'b0,lsu_ld_datafn_corr_dc3[15:0]}) |
                                      ({32{~lsu_pkt_dc3.unsign & lsu_pkt_dc3.by  }} & {{24{  lsu_ld_datafn_corr_dc3[7]}}, lsu_ld_datafn_corr_dc3[7:0]}) |
                                      ({32{~lsu_pkt_dc3.unsign & lsu_pkt_dc3.half}} & {{16{  lsu_ld_datafn_corr_dc3[15]}},lsu_ld_datafn_corr_dc3[15:0]}) |
                                      ({32{lsu_pkt_dc3.word}} &                       lsu_ld_datafn_corr_dc3[31:0]);
   assign lsu_addr_dc1[31:0] = full_addr_dc1[31:0];
   assign lsu_commit_dc5 = lsu_pkt_dc5.valid & (lsu_pkt_dc5.store | lsu_pkt_dc5.load) & ~flush_dc5 & ~lsu_pkt_dc5.dma;
   assign dma_mem_wdata_shifted[63:0] = dma_mem_wdata[63:0] >> {dma_mem_addr[2:0], 3'b000};    
   assign store_data_d[63:0] = dma_dccm_req ? dma_mem_wdata_shifted[63:0] : {32'b0,exu_lsu_rs2_d[31:0]};
   assign store_data_dc2_in[63:32] = store_data_dc1[63:32];
   assign store_data_dc2_in[31:0] = (lsu_pkt_dc1.store_data_bypass_c1) ? lsu_result_dc3[31:0] :
                                    (lsu_pkt_dc1.store_data_bypass_e4_c1[1]) ? i1_result_e4_eff[31:0] :
                                    (lsu_pkt_dc1.store_data_bypass_e4_c1[0]) ? i0_result_e4_eff[31:0] : store_data_dc1[31:0];
   assign store_data_dc2[63:32] = store_data_pre_dc2[63:32];
   assign store_data_dc2[31:0] = (lsu_pkt_dc2.store_data_bypass_i0_e2_c2) ? i0_result_e2[31:0]     :
                                 (lsu_pkt_dc2.store_data_bypass_c2)       ? lsu_result_dc3[31:0]   :
                                 (lsu_pkt_dc2.store_data_bypass_e4_c2[1]) ? i1_result_e4_eff[31:0] :
                                 (lsu_pkt_dc2.store_data_bypass_e4_c2[0]) ? i0_result_e4_eff[31:0] : store_data_pre_dc2[31:0];
   assign store_data_dc3[63:32] = store_data_pre_dc3[63:32];
   assign store_data_dc3[31:0] = (picm_mask_data_dc3[31:0] | {32{~addr_in_pic_dc3}}) &
                                 ((lsu_pkt_dc3.store_data_bypass_e4_c3[1]) ? i1_result_e4_eff[31:0] :
                                  (lsu_pkt_dc3.store_data_bypass_e4_c3[0]) ? i0_result_e4_eff[31:0] : store_data_pre_dc3[31:0]);
   rvdff #(32) lsu_result_corr_dc4ff (.*, .din(lsu_result_corr_dc3[31:0]), .dout(lsu_result_corr_dc4[31:0]), .clk(lsu_c1_dc4_clk));
   rvdffe #(64) sddc1ff (.*, .din(store_data_d[63:0]),  .dout(store_data_dc1[63:0]), .en(lsu_store_c1_dc1_clken));
   rvdffe #(64) sddc2ff (.*, .din(store_data_dc2_in[63:0]), .dout(store_data_pre_dc2[63:0]), .en(lsu_store_c1_dc2_clken));
   rvdffe #(64) sddc3ff (.*, .din(store_data_dc2[63:0]), .dout(store_data_pre_dc3[63:0]), .en(~lsu_freeze_dc3 & lsu_store_c1_dc3_clken) );
   rvdff #(32) sddc4ff (.*, .din(store_data_dc3[31:0]), .dout(store_data_dc4[31:0]), .clk(lsu_store_c1_dc4_clk));
   rvdff #(32) sddc5ff (.*, .din(store_data_dc4[31:0]), .dout(store_data_dc5[31:0]), .clk(lsu_store_c1_dc5_clk));
   rvdffe #(32) sadc2ff (.*, .din(lsu_addr_dc1[31:0]), .dout(lsu_addr_dc2[31:0]), .en(lsu_freeze_c1_dc2_clken));
   rvdffe #(32) sadc3ff (.*, .din(lsu_addr_dc2[31:0]), .dout(lsu_addr_dc3[31:0]), .en(lsu_freeze_c1_dc3_clken));
   rvdff #(32) sadc4ff (.*, .din(lsu_addr_dc3[31:0]), .dout(lsu_addr_dc4[31:0]), .clk(lsu_c1_dc4_clk));
   rvdff #(32) sadc5ff (.*, .din(lsu_addr_dc4[31:0]), .dout(lsu_addr_dc5[31:0]), .clk(lsu_c1_dc5_clk));
   rvdffe #(32) end_addr_dc2ff (.*, .din(end_addr_dc1[31:0]), .dout(end_addr_dc2[31:0]), .en(lsu_freeze_c1_dc2_clken));
   rvdffe #(32) end_addr_dc3ff (.*, .din(end_addr_dc2[31:0]), .dout(end_addr_dc3[31:0]), .en(lsu_freeze_c1_dc3_clken));
   rvdff #(32) end_addr_dc4ff (.*, .din(end_addr_dc3[31:0]), .dout(end_addr_dc4[31:0]), .clk(lsu_c1_dc4_clk));
   rvdff #(32) end_addr_dc5ff (.*, .din(end_addr_dc4[31:0]), .dout(end_addr_dc5[31:0]), .clk(lsu_c1_dc5_clk));
   rvdff_fpga #(1) addr_in_dccm_dc2ff     (.din(addr_in_dccm_dc1),     .dout(addr_in_dccm_dc2),     .clk(lsu_freeze_c1_dc2_clk), .rawclk(clk), .clken(lsu_freeze_c1_dc2_clken), .*);
   rvdff_fpga #(1) addr_in_dccm_dc3ff     (.din(addr_in_dccm_dc2),     .dout(addr_in_dccm_dc3),     .clk(lsu_freeze_c1_dc3_clk), .rawclk(clk), .clken(lsu_freeze_c1_dc3_clken), .*);
   rvdff_fpga #(1) addr_in_pic_dc2ff      (.din(addr_in_pic_dc1),      .dout(addr_in_pic_dc2),      .clk(lsu_freeze_c1_dc2_clk), .rawclk(clk), .clken(lsu_freeze_c1_dc2_clken), .*);
   rvdff_fpga #(1) addr_in_pic_dc3ff      (.din(addr_in_pic_dc2),      .dout(addr_in_pic_dc3),      .clk(lsu_freeze_c1_dc3_clk), .rawclk(clk), .clken(lsu_freeze_c1_dc3_clken), .*);
   rvdff_fpga #(1) access_fault_dc2ff     (.din(access_fault_dc1),     .dout(access_fault_dc2),     .clk(lsu_freeze_c1_dc2_clk), .rawclk(clk), .clken(lsu_freeze_c1_dc2_clken), .*);
   rvdff_fpga #(1) access_fault_dc3ff     (.din(access_fault_dc2),     .dout(access_fault_dc3),     .clk(lsu_freeze_c1_dc3_clk), .rawclk(clk), .clken(lsu_freeze_c1_dc3_clken), .*);
   rvdff_fpga #(1) addr_external_dc2ff    (.din(addr_external_dc1),    .dout(addr_external_dc2),    .clk(lsu_freeze_c1_dc2_clk), .rawclk(clk), .clken(lsu_freeze_c1_dc2_clken), .*);
   rvdff_fpga #(1) addr_external_dc3ff    (.din(addr_external_dc2),    .dout(addr_external_dc3),    .clk(lsu_freeze_c1_dc3_clk), .rawclk(clk), .clken(lsu_freeze_c1_dc3_clken), .*);
   rvdff_fpga #(1) misaligned_fault_dc2ff (.din(misaligned_fault_dc1), .dout(misaligned_fault_dc2), .clk(lsu_freeze_c1_dc2_clk), .rawclk(clk), .clken(lsu_freeze_c1_dc2_clken), .*);
   rvdff_fpga #(1) misaligned_fault_dc3ff (.din(misaligned_fault_dc2), .dout(misaligned_fault_dc3), .clk(lsu_freeze_c1_dc3_clk), .rawclk(clk), .clken(lsu_freeze_c1_dc3_clken), .*);
   rvdff_fpga #(1) lsu_pkt_vlddc1ff       (.din(lsu_pkt_dc1_in.valid), .dout(lsu_pkt_dc1.valid),    .clk(lsu_freeze_c2_dc1_clk), .rawclk(clk), .clken(lsu_freeze_c2_dc1_clken), .*);
   rvdff_fpga #(1) lsu_pkt_vlddc2ff       (.din(lsu_pkt_dc2_in.valid), .dout(lsu_pkt_dc2.valid),    .clk(lsu_freeze_c2_dc2_clk), .rawclk(clk), .clken(lsu_freeze_c2_dc2_clken), .*);
   rvdff_fpga #(1) lsu_pkt_vlddc3ff       (.din(lsu_pkt_dc3_in.valid), .dout(lsu_pkt_dc3.valid),    .clk(lsu_freeze_c2_dc3_clk), .rawclk(clk), .clken(lsu_freeze_c2_dc3_clken), .*);
   rvdff #(1) addr_external_dc4ff(.din(addr_external_dc3), .dout(addr_external_dc4), .clk(lsu_c1_dc4_clk), .*);
   rvdff #(1) addr_external_dc5ff(.din(addr_external_dc4), .dout(addr_external_dc5), .clk(lsu_c1_dc5_clk), .*);
endmodule
module lsu_stbuf
   import swerv_types::*;
(
   input logic        clk,                                 
   input logic        rst_l,                               
   input logic        lsu_freeze_c2_dc2_clk,               
   input logic        lsu_freeze_c2_dc3_clk,               
   input logic        lsu_freeze_c1_dc2_clk,               
   input logic        lsu_freeze_c1_dc3_clk,               
   input logic        lsu_freeze_c2_dc2_clken,
   input logic        lsu_freeze_c2_dc3_clken,
   input logic        lsu_freeze_c1_dc2_clken,
   input logic        lsu_freeze_c1_dc3_clken,
   input logic        lsu_c1_dc4_clk,                      
   input logic        lsu_c1_dc5_clk,                      
   input logic        lsu_c2_dc4_clk,                      
   input logic        lsu_c2_dc5_clk,                      
   input logic        lsu_stbuf_c1_clk,                    
   input logic        lsu_free_c2_clk,                     
   input logic        load_stbuf_reqvld_dc3,              
   input logic        store_stbuf_reqvld_dc3,              
   input logic        addr_in_pic_dc2,                     
   input logic        addr_in_pic_dc3,                     
   input logic        addr_in_dccm_dc2,                     
   input logic        addr_in_dccm_dc3,                     
   input logic [32-1:0] store_ecc_datafn_hi_dc3,    
   input logic [32-1:0] store_ecc_datafn_lo_dc3,    
   input logic        isldst_dc1,                          
   input logic        dccm_ldst_dc2,                          
   input logic        dccm_ldst_dc3,                          
   input logic        single_ecc_error_hi_dc3,             
   input logic        single_ecc_error_lo_dc3,             
   input logic        lsu_single_ecc_error_dc5,            
   input logic        lsu_commit_dc5,                      
   input logic        lsu_freeze_dc3,                      
   input logic        flush_prior_dc5,                     
   output logic                  stbuf_reqvld_any,          
   output logic                  stbuf_reqvld_flushed_any,  
   output logic                  stbuf_addr_in_pic_any,     
   output logic [4-1:0] stbuf_byteen_any,  
   output logic [16-1:0]  stbuf_addr_any,         
   output logic [32-1:0] stbuf_data_any,    
   input  logic       lsu_stbuf_commit_any,                  
   output logic       lsu_stbuf_full_any,                    
   output logic       lsu_stbuf_empty_any,                   
   output logic       lsu_stbuf_nodma_empty_any,             
   output logic       lsu_load_ecc_stbuf_full_dc3,           
   input logic [16-1:0] lsu_addr_dc1,             
   input logic [16-1:0] lsu_addr_dc2,
   input logic [16-1:0] lsu_addr_dc3,
   input logic [16-1:0] end_addr_dc1,             
   input logic [16-1:0] end_addr_dc2,
   input logic [16-1:0] end_addr_dc3,
   input logic        lsu_cmpen_dc2,                        
   input lsu_pkt_t    lsu_pkt_dc2,
   input lsu_pkt_t    lsu_pkt_dc3,
   input lsu_pkt_t    lsu_pkt_dc5,
   output logic [32-1:0] stbuf_fwddata_hi_dc3,      
   output logic [32-1:0] stbuf_fwddata_lo_dc3,
   output logic [4-1:0] stbuf_fwdbyteen_hi_dc3,
   output logic [4-1:0] stbuf_fwdbyteen_lo_dc3,
   input  logic       scan_mode
);
localparam TOTAL_INT        = 9;
localparam DCCM_BITS        = 16;
localparam DCCM_BANK_BITS   = 3;
localparam DCCM_NUM_BANKS   = 8;
localparam DCCM_DATA_WIDTH  = 32;
localparam DCCM_FDATA_WIDTH = 39;
localparam DCCM_BYTE_WIDTH  = 4;
localparam DCCM_ECC_WIDTH   = 7;
localparam LSU_RDBUF_DEPTH  = 8;
localparam DMA_BUF_DEPTH    = 4;
localparam LSU_STBUF_DEPTH  = 8;
localparam LSU_SB_BITS      = 16;
localparam DEC_INSTBUF_DEPTH = 4;
localparam ICCM_SIZE         = 512;
localparam ICCM_BITS         = 19;
localparam ICCM_NUM_BANKS    = 8;
localparam ICCM_BANK_BITS    = 3;
localparam ICCM_INDEX_BITS   = 14;
localparam ICCM_BANK_HI      = 4 + (3/4);
localparam ICACHE_TAG_HIGH  = 12;
localparam ICACHE_TAG_LOW   = 6;
localparam ICACHE_IC_DEPTH  = 8;
localparam ICACHE_TAG_DEPTH = 64;
localparam LSU_BUS_TAG     = 4;
localparam DMA_BUS_TAG     = 1;
localparam SB_BUS_TAG      = 1;
localparam IFU_BUS_TAG     = 3;
   localparam DEPTH = LSU_STBUF_DEPTH;
   localparam DATA_WIDTH = DCCM_DATA_WIDTH;
   localparam BYTE_WIDTH = DCCM_BYTE_WIDTH;
   localparam DEPTH_LOG2 = $clog2(DEPTH);
   logic [DEPTH-1:0]       stbuf_data_vld;
   logic [DEPTH-1:0]       stbuf_drain_vld;
   logic [DEPTH-1:0]       stbuf_flush_vld;
   logic [DEPTH-1:0]       stbuf_addr_in_pic;
   logic [DEPTH-1:0]       stbuf_dma;
   logic [DEPTH-1:0][LSU_SB_BITS-1:0] stbuf_addr;
   logic [DEPTH-1:0][BYTE_WIDTH-1:0]  stbuf_byteen;
   logic [DEPTH-1:0][DATA_WIDTH-1:0] stbuf_data;
   logic [DEPTH-1:0]       sel_lo;
   logic [DEPTH-1:0]       stbuf_wr_en;
   logic [DEPTH-1:0]       stbuf_data_en;
   logic [DEPTH-1:0]       stbuf_drain_or_flush_en;
   logic [DEPTH-1:0]       stbuf_flush_en;
   logic [DEPTH-1:0]       stbuf_drain_en;
   logic [DEPTH-1:0]       stbuf_reset;
   logic [DEPTH-1:0][LSU_SB_BITS-1:0] stbuf_addrin;
   logic [DEPTH-1:0][DATA_WIDTH-1:0] stbuf_datain;
   logic [DEPTH-1:0][BYTE_WIDTH-1:0]  stbuf_byteenin;
   logic [7:0]             ldst_byteen_dc3;
   logic [7:0]             store_byteen_ext_dc3;
   logic [BYTE_WIDTH-1:0]  store_byteen_hi_dc3;
   logic [BYTE_WIDTH-1:0]  store_byteen_lo_dc3;
   logic                   ldst_stbuf_reqvld_dc3;
   logic                   dual_ecc_error_dc3;
   logic                   dual_stbuf_write_dc3;
   logic                   WrPtrEn, RdPtrEn;
   logic [DEPTH_LOG2-1:0]  WrPtr, RdPtr;
   logic [DEPTH_LOG2-1:0]  NxtWrPtr, NxtRdPtr;
   logic [DEPTH_LOG2-1:0]  WrPtrPlus1, WrPtrPlus1_dc5, WrPtrPlus2, RdPtrPlus1;
   logic [DEPTH_LOG2-1:0]  WrPtr_dc3, WrPtr_dc4, WrPtr_dc5;
   logic                   ldst_dual_dc1, ldst_dual_dc2, ldst_dual_dc3, ldst_dual_dc4, ldst_dual_dc5;
   logic                   ldst_stbuf_reqvld_dc4, ldst_stbuf_reqvld_dc5;
   logic                   dual_stbuf_write_dc4, dual_stbuf_write_dc5;
   logic [3:0]             stbuf_numvld_any, stbuf_specvld_any;
   logic [1:0]             stbuf_specvld_dc1, stbuf_specvld_dc2, stbuf_specvld_dc3;
   logic                   stbuf_oneavl_any, stbuf_twoavl_any;
   logic                   cmpen_hi_dc2, cmpen_lo_dc2, jit_in_same_region;
   logic [LSU_SB_BITS-1:$clog2(BYTE_WIDTH)]  cmpaddr_hi_dc2, cmpaddr_lo_dc2;
   logic                   stbuf_ldmatch_hi_hi, stbuf_ldmatch_hi_lo;
   logic                   stbuf_ldmatch_lo_hi, stbuf_ldmatch_lo_lo;
   logic [BYTE_WIDTH-1:0]  stbuf_fwdbyteen_hi_hi, stbuf_fwdbyteen_hi_lo;
   logic [BYTE_WIDTH-1:0]  stbuf_fwdbyteen_lo_hi, stbuf_fwdbyteen_lo_lo;
   logic [DATA_WIDTH-1:0]  stbuf_fwddata_hi_hi, stbuf_fwddata_hi_lo;
   logic [DATA_WIDTH-1:0]  stbuf_fwddata_lo_hi, stbuf_fwddata_lo_lo;
   logic [DEPTH-1:0]                 stbuf_ldmatch_hi, stbuf_ldmatch_lo;
   logic [DEPTH-1:0][BYTE_WIDTH-1:0] stbuf_fwdbyteenvec_hi, stbuf_fwdbyteenvec_lo;
   logic [DEPTH-1:0][DATA_WIDTH-1:0] stbuf_fwddatavec_hi, stbuf_fwddatavec_lo;
   logic [DATA_WIDTH-1:0]            stbuf_fwddata_hi_dc2, stbuf_fwddata_lo_dc2;
   logic [DATA_WIDTH-1:0]            stbuf_fwddata_hi_fn_dc2, stbuf_fwddata_lo_fn_dc2;
   logic [BYTE_WIDTH-1:0]            stbuf_fwdbyteen_hi_dc2, stbuf_fwdbyteen_lo_dc2;
   logic [BYTE_WIDTH-1:0]            stbuf_fwdbyteen_hi_fn_dc2, stbuf_fwdbyteen_lo_fn_dc2;
   logic                             stbuf_load_repair_dc5;
   assign ldst_byteen_dc3[7:0] = ({8{lsu_pkt_dc3.by}}   & 8'b0000_0001) |
                                 ({8{lsu_pkt_dc3.half}} & 8'b0000_0011) |
                                 ({8{lsu_pkt_dc3.word}} & 8'b0000_1111) |
                                 ({8{lsu_pkt_dc3.dword}} & 8'b1111_1111);
   assign store_byteen_ext_dc3[7:0] = ldst_byteen_dc3[7:0] << lsu_addr_dc3[1:0];
   assign store_byteen_hi_dc3[BYTE_WIDTH-1:0] = store_byteen_ext_dc3[7:4];
   assign store_byteen_lo_dc3[BYTE_WIDTH-1:0] = store_byteen_ext_dc3[3:0];
   assign RdPtrPlus1[DEPTH_LOG2-1:0] = RdPtr[DEPTH_LOG2-1:0] + 1'b1;
   assign WrPtrPlus1[DEPTH_LOG2-1:0] = WrPtr[DEPTH_LOG2-1:0] + 1'b1;
   assign WrPtrPlus2[DEPTH_LOG2-1:0] = WrPtr[DEPTH_LOG2-1:0] + 2'b10;
   assign WrPtrPlus1_dc5[DEPTH_LOG2-1:0] = WrPtr_dc5[DEPTH_LOG2-1:0] + 1'b1;
   assign ldst_dual_dc1      = (lsu_addr_dc1[2] != end_addr_dc1[2]);
   assign dual_ecc_error_dc3 = (single_ecc_error_hi_dc3 & single_ecc_error_lo_dc3);
   assign dual_stbuf_write_dc3   = ldst_dual_dc3 & (store_stbuf_reqvld_dc3 | dual_ecc_error_dc3);
   assign ldst_stbuf_reqvld_dc3  = store_stbuf_reqvld_dc3 |
                                   (load_stbuf_reqvld_dc3 & (dual_ecc_error_dc3 ? stbuf_twoavl_any : stbuf_oneavl_any));    
   assign stbuf_load_repair_dc5 = lsu_single_ecc_error_dc5 & (lsu_pkt_dc5.valid & lsu_pkt_dc5.load & ~flush_prior_dc5);
   for (genvar i=0; i<DEPTH; i++) begin: GenStBuf
      assign stbuf_wr_en[i] = ldst_stbuf_reqvld_dc3 & ((i == WrPtr[DEPTH_LOG2-1:0]) |
                                                       (i == WrPtrPlus1[DEPTH_LOG2-1:0] & dual_stbuf_write_dc3));
      assign stbuf_data_en[i] = stbuf_wr_en[i];
      assign stbuf_drain_or_flush_en[i] = ldst_stbuf_reqvld_dc5 & ~lsu_pkt_dc5.dma & ((i == WrPtr_dc5[DEPTH_LOG2-1:0]) |
                                                                                      (i == WrPtrPlus1_dc5[DEPTH_LOG2-1:0] & dual_stbuf_write_dc5));
      assign stbuf_drain_en[i] = (stbuf_drain_or_flush_en[i] & lsu_commit_dc5) | (stbuf_wr_en[i] & lsu_pkt_dc3.dma);
      assign stbuf_flush_en[i] = stbuf_drain_or_flush_en[i] & ~lsu_commit_dc5;
      assign stbuf_reset[i] = (lsu_stbuf_commit_any | stbuf_reqvld_flushed_any) & (i == RdPtr[DEPTH_LOG2-1:0]);
      assign sel_lo[i] = (~ldst_dual_dc3 | (store_stbuf_reqvld_dc3 | single_ecc_error_lo_dc3)) & (i == WrPtr[DEPTH_LOG2-1:0]);
      assign stbuf_addrin[i][LSU_SB_BITS-1:0]  = sel_lo[i] ? lsu_addr_dc3[LSU_SB_BITS-1:0] : end_addr_dc3[LSU_SB_BITS-1:0];
      assign stbuf_byteenin[i][BYTE_WIDTH-1:0] = sel_lo[i] ? store_byteen_lo_dc3[BYTE_WIDTH-1:0] : store_byteen_hi_dc3[BYTE_WIDTH-1:0];
      assign stbuf_datain[i][DATA_WIDTH-1:0]  = sel_lo[i] ? store_ecc_datafn_lo_dc3[DATA_WIDTH-1:0] : store_ecc_datafn_hi_dc3[DATA_WIDTH-1:0];
      rvdffsc #(.WIDTH(1)) stbuf_data_vldff (.din(1'b1), .dout(stbuf_data_vld[i]), .en(stbuf_wr_en[i]), .clear(stbuf_reset[i]), .clk(lsu_stbuf_c1_clk), .*);
      rvdffsc #(.WIDTH(1)) stbuf_drain_vldff (.din(1'b1), .dout(stbuf_drain_vld[i]), .en(stbuf_drain_en[i]), .clear(stbuf_reset[i]), .clk(lsu_free_c2_clk), .*);
      rvdffsc #(.WIDTH(1)) stbuf_flush_vldff (.din(1'b1), .dout(stbuf_flush_vld[i]), .en(stbuf_flush_en[i]), .clear(stbuf_reset[i]), .clk(lsu_free_c2_clk), .*);
      rvdffs #(.WIDTH(1)) stbuf_dma_picff (.din(lsu_pkt_dc3.dma), .dout(stbuf_dma[i]), .en(stbuf_wr_en[i]), .clk(lsu_stbuf_c1_clk), .*);
      rvdffs #(.WIDTH(1)) stbuf_addr_in_picff (.din(addr_in_pic_dc3), .dout(stbuf_addr_in_pic[i]), .en(stbuf_wr_en[i]), .clk(lsu_stbuf_c1_clk), .*);
      rvdffe #(.WIDTH(LSU_SB_BITS)) stbuf_addrff (.din(stbuf_addrin[i][LSU_SB_BITS-1:0]), .dout(stbuf_addr[i][LSU_SB_BITS-1:0]), .en(stbuf_wr_en[i]), .*);
      rvdffs #(.WIDTH(BYTE_WIDTH)) stbuf_byteenff (.din(stbuf_byteenin[i][BYTE_WIDTH-1:0]), .dout(stbuf_byteen[i][BYTE_WIDTH-1:0]), .en(stbuf_wr_en[i]), .clk(lsu_stbuf_c1_clk), .*);
      rvdffe #(.WIDTH(DATA_WIDTH)) stbuf_dataff (.din(stbuf_datain[i][DATA_WIDTH-1:0]), .dout(stbuf_data[i][DATA_WIDTH-1:0]), .en(stbuf_data_en[i]), .*);
   end
   assign WrPtr_dc3[DEPTH_LOG2-1:0] = WrPtr[DEPTH_LOG2-1:0];
   rvdff #(.WIDTH(DEPTH_LOG2)) WrPtr_dc4ff (.din(WrPtr_dc3[DEPTH_LOG2-1:0]), .dout(WrPtr_dc4[DEPTH_LOG2-1:0]), .clk(lsu_c1_dc4_clk), .*);
   rvdff #(.WIDTH(DEPTH_LOG2)) WrPtr_dc5ff (.din(WrPtr_dc4[DEPTH_LOG2-1:0]), .dout(WrPtr_dc5[DEPTH_LOG2-1:0]), .clk(lsu_c1_dc5_clk), .*);
   rvdff_fpga #(.WIDTH(1)) ldst_dual_dc2ff (.din(ldst_dual_dc1), .dout(ldst_dual_dc2), .clk(lsu_freeze_c1_dc2_clk), .clken(lsu_freeze_c1_dc2_clken), .rawclk(clk), .*);
   rvdff_fpga #(.WIDTH(1)) ldst_dual_dc3ff (.din(ldst_dual_dc2), .dout(ldst_dual_dc3), .clk(lsu_freeze_c1_dc3_clk), .clken(lsu_freeze_c1_dc3_clken), .rawclk(clk), .*);
   rvdff_fpga #(.WIDTH(BYTE_WIDTH)) stbuf_fwdbyteen_hi_dc3ff (.din(stbuf_fwdbyteen_hi_fn_dc2[BYTE_WIDTH-1:0]), .dout(stbuf_fwdbyteen_hi_dc3[BYTE_WIDTH-1:0]), .clk(lsu_freeze_c1_dc3_clk), .clken(lsu_freeze_c1_dc3_clken), .rawclk(clk), .*);
   rvdff_fpga #(.WIDTH(BYTE_WIDTH)) stbuf_fwdbyteen_lo_dc3ff (.din(stbuf_fwdbyteen_lo_fn_dc2[BYTE_WIDTH-1:0]), .dout(stbuf_fwdbyteen_lo_dc3[BYTE_WIDTH-1:0]), .clk(lsu_freeze_c1_dc3_clk), .clken(lsu_freeze_c1_dc3_clken), .rawclk(clk), .*);
   rvdff #(.WIDTH(1)) ldst_dual_dc4ff (.din(ldst_dual_dc3), .dout(ldst_dual_dc4), .clk(lsu_c1_dc4_clk), .*);
   rvdff #(.WIDTH(1)) ldst_dual_dc5ff (.din(ldst_dual_dc4), .dout(ldst_dual_dc5), .clk(lsu_c1_dc5_clk), .*);
   rvdff #(.WIDTH(1)) dual_stbuf_write_dc4ff (.din(dual_stbuf_write_dc3), .dout(dual_stbuf_write_dc4), .clk(lsu_c1_dc4_clk), .*);
   rvdff #(.WIDTH(1)) dual_stbuf_write_dc5ff (.din(dual_stbuf_write_dc4), .dout(dual_stbuf_write_dc5), .clk(lsu_c1_dc5_clk), .*);
   rvdff #(.WIDTH(1)) ldst_reqvld_dc4ff (.din(ldst_stbuf_reqvld_dc3), .dout(ldst_stbuf_reqvld_dc4), .clk(lsu_c2_dc4_clk), .*);
   rvdff #(.WIDTH(1)) ldst_reqvld_dc5ff (.din(ldst_stbuf_reqvld_dc4), .dout(ldst_stbuf_reqvld_dc5), .clk(lsu_c2_dc5_clk), .*);
   assign stbuf_reqvld_flushed_any = stbuf_flush_vld[RdPtr];
   assign stbuf_reqvld_any = stbuf_drain_vld[RdPtr];
   assign stbuf_addr_in_pic_any = stbuf_addr_in_pic[RdPtr];
   assign stbuf_addr_any[LSU_SB_BITS-1:0] = stbuf_addr[RdPtr][LSU_SB_BITS-1:0];
   assign stbuf_byteen_any[BYTE_WIDTH-1:0] = stbuf_byteen[RdPtr][BYTE_WIDTH-1:0];     
   assign stbuf_data_any[DATA_WIDTH-1:0] = stbuf_data[RdPtr][DATA_WIDTH-1:0];
   assign WrPtrEn = ldst_stbuf_reqvld_dc3;
   assign NxtWrPtr[DEPTH_LOG2-1:0] = (ldst_stbuf_reqvld_dc3 & dual_stbuf_write_dc3) ? WrPtrPlus2[DEPTH_LOG2-1:0] : WrPtrPlus1[DEPTH_LOG2-1:0];
   assign RdPtrEn = lsu_stbuf_commit_any | stbuf_reqvld_flushed_any;
   assign NxtRdPtr[DEPTH_LOG2-1:0] = RdPtrPlus1[DEPTH_LOG2-1:0];
   always_comb begin
      stbuf_numvld_any[3:0] = '0;
      for (int i=0; i<DEPTH; i++) begin
         stbuf_numvld_any[3:0] += {3'b0, stbuf_data_vld[i]};
      end
   end
   assign stbuf_specvld_dc1[1:0] = {1'b0,isldst_dc1} << (isldst_dc1 & ldst_dual_dc1);     
   assign stbuf_specvld_dc2[1:0] = {1'b0,dccm_ldst_dc2} << (dccm_ldst_dc2 & ldst_dual_dc2);
   assign stbuf_specvld_dc3[1:0] = {1'b0,dccm_ldst_dc3} << (dccm_ldst_dc3 & ldst_dual_dc3);
   assign stbuf_specvld_any[3:0] = stbuf_numvld_any[3:0] +  {2'b0, stbuf_specvld_dc1[1:0]} + {2'b0, stbuf_specvld_dc2[1:0]} + {2'b0, stbuf_specvld_dc3[1:0]};
   assign lsu_stbuf_full_any = (stbuf_specvld_any[3:0] > (DEPTH - 2));
   assign lsu_stbuf_empty_any = (stbuf_numvld_any[3:0] == 4'b0);
   assign lsu_stbuf_nodma_empty_any = ~(|(stbuf_data_vld[DEPTH-1:0] & ~stbuf_dma[DEPTH-1:0]));
   assign lsu_load_ecc_stbuf_full_dc3 = load_stbuf_reqvld_dc3 & ~ldst_stbuf_reqvld_dc3;
   assign stbuf_oneavl_any = (stbuf_numvld_any[3:0] < DEPTH);
   assign stbuf_twoavl_any = (stbuf_numvld_any[3:0] < (DEPTH - 1));
   assign cmpen_hi_dc2 = lsu_cmpen_dc2 & ldst_dual_dc2;
   assign cmpaddr_hi_dc2[LSU_SB_BITS-1:$clog2(BYTE_WIDTH)] = end_addr_dc2[LSU_SB_BITS-1:$clog2(BYTE_WIDTH)];
   assign cmpen_lo_dc2 = lsu_cmpen_dc2;
   assign cmpaddr_lo_dc2[LSU_SB_BITS-1:$clog2(BYTE_WIDTH)] = lsu_addr_dc2[LSU_SB_BITS-1:$clog2(BYTE_WIDTH)];
   assign jit_in_same_region = (addr_in_pic_dc2 & addr_in_pic_dc3) | (addr_in_dccm_dc2 & addr_in_dccm_dc3);
   assign stbuf_ldmatch_hi_hi = (end_addr_dc3[LSU_SB_BITS-1:$clog2(BYTE_WIDTH)] == cmpaddr_hi_dc2[LSU_SB_BITS-1:$clog2(BYTE_WIDTH)]) & ~(cmpen_hi_dc2 & lsu_pkt_dc2.dma & ~lsu_pkt_dc3.dma) & jit_in_same_region;
   assign stbuf_ldmatch_hi_lo = (lsu_addr_dc3[LSU_SB_BITS-1:$clog2(BYTE_WIDTH)] == cmpaddr_hi_dc2[LSU_SB_BITS-1:$clog2(BYTE_WIDTH)]) & ~(cmpen_hi_dc2 & lsu_pkt_dc2.dma & ~lsu_pkt_dc3.dma) & jit_in_same_region;
   assign stbuf_ldmatch_lo_hi = (end_addr_dc3[LSU_SB_BITS-1:$clog2(BYTE_WIDTH)] == cmpaddr_lo_dc2[LSU_SB_BITS-1:$clog2(BYTE_WIDTH)]) & ~(cmpen_lo_dc2 & lsu_pkt_dc2.dma & ~lsu_pkt_dc3.dma) & jit_in_same_region;
   assign stbuf_ldmatch_lo_lo = (lsu_addr_dc3[LSU_SB_BITS-1:$clog2(BYTE_WIDTH)] == cmpaddr_lo_dc2[LSU_SB_BITS-1:$clog2(BYTE_WIDTH)]) & ~(cmpen_lo_dc2 & lsu_pkt_dc2.dma & ~lsu_pkt_dc3.dma) & jit_in_same_region;
   for (genvar i=0; i<BYTE_WIDTH; i++) begin
      assign stbuf_fwdbyteen_hi_hi[i] = stbuf_ldmatch_hi_hi & store_byteen_hi_dc3[i] & ldst_stbuf_reqvld_dc3 & dual_stbuf_write_dc3;
      assign stbuf_fwdbyteen_hi_lo[i] = stbuf_ldmatch_hi_lo & store_byteen_lo_dc3[i] & ldst_stbuf_reqvld_dc3;
      assign stbuf_fwdbyteen_lo_hi[i] = stbuf_ldmatch_lo_hi & store_byteen_hi_dc3[i] & ldst_stbuf_reqvld_dc3 & dual_stbuf_write_dc3;
      assign stbuf_fwdbyteen_lo_lo[i] = stbuf_ldmatch_lo_lo & store_byteen_lo_dc3[i] & ldst_stbuf_reqvld_dc3;
      assign stbuf_fwddata_hi_hi[(8*i)+7:(8*i)] = {8{stbuf_fwdbyteen_hi_hi[i]}} & store_ecc_datafn_hi_dc3[(8*i)+7:(8*i)];
      assign stbuf_fwddata_hi_lo[(8*i)+7:(8*i)] = {8{stbuf_fwdbyteen_hi_lo[i]}} & store_ecc_datafn_lo_dc3[(8*i)+7:(8*i)];
      assign stbuf_fwddata_lo_hi[(8*i)+7:(8*i)] = {8{stbuf_fwdbyteen_lo_hi[i]}} & store_ecc_datafn_hi_dc3[(8*i)+7:(8*i)];
      assign stbuf_fwddata_lo_lo[(8*i)+7:(8*i)] = {8{stbuf_fwdbyteen_lo_lo[i]}} & store_ecc_datafn_lo_dc3[(8*i)+7:(8*i)];
   end
   always_comb begin: GenLdFwd
      stbuf_fwdbyteen_hi_dc2[BYTE_WIDTH-1:0]   = '0;
      stbuf_fwdbyteen_lo_dc2[BYTE_WIDTH-1:0]   = '0;
      for (int i=0; i<DEPTH; i++) begin
         stbuf_ldmatch_hi[i] = (stbuf_addr[i][LSU_SB_BITS-1:$clog2(BYTE_WIDTH)] == cmpaddr_hi_dc2[LSU_SB_BITS-1:$clog2(BYTE_WIDTH)]) &
                               (stbuf_drain_vld[i] | ~lsu_pkt_dc2.dma) & ~stbuf_flush_vld[i] & ((stbuf_addr_in_pic[i] & addr_in_pic_dc2) | (~stbuf_addr_in_pic[i] & addr_in_dccm_dc2));
         stbuf_ldmatch_lo[i] = (stbuf_addr[i][LSU_SB_BITS-1:$clog2(BYTE_WIDTH)] == cmpaddr_lo_dc2[LSU_SB_BITS-1:$clog2(BYTE_WIDTH)]) &
                               (stbuf_drain_vld[i] | ~lsu_pkt_dc2.dma) & ~stbuf_flush_vld[i] & ((stbuf_addr_in_pic[i] & addr_in_pic_dc2) | (~stbuf_addr_in_pic[i] & addr_in_dccm_dc2));
         for (int j=0; j<BYTE_WIDTH; j++) begin
            stbuf_fwdbyteenvec_hi[i][j] = stbuf_ldmatch_hi[i] & stbuf_byteen[i][j] & stbuf_data_vld[i];
            stbuf_fwdbyteen_hi_dc2[j] |= stbuf_fwdbyteenvec_hi[i][j];
            stbuf_fwdbyteenvec_lo[i][j] = stbuf_ldmatch_lo[i] & stbuf_byteen[i][j] & stbuf_data_vld[i];
            stbuf_fwdbyteen_lo_dc2[j] |= stbuf_fwdbyteenvec_lo[i][j];
         end
      end
   end  
   for (genvar i=0; i<DEPTH; i++) begin
      for (genvar j=0; j<BYTE_WIDTH; j++) begin
         assign stbuf_fwddatavec_hi[i][(8*j)+7:(8*j)] = {8{stbuf_fwdbyteenvec_hi[i][j]}} & stbuf_data[i][(8*j)+7:(8*j)];
         assign stbuf_fwddatavec_lo[i][(8*j)+7:(8*j)] = {8{stbuf_fwdbyteenvec_lo[i][j]}} & stbuf_data[i][(8*j)+7:(8*j)];
      end
   end
   always_comb begin
      stbuf_fwddata_hi_dc2[DATA_WIDTH-1:0] = '0;
      stbuf_fwddata_lo_dc2[DATA_WIDTH-1:0] = '0;
      for (int i=0; i<DEPTH; i++) begin
         if (stbuf_fwdbyteenvec_hi[DEPTH_LOG2'(WrPtr[DEPTH_LOG2-1:0] + DEPTH_LOG2'(i))][0]) begin
            stbuf_fwddata_hi_dc2[7:0] = stbuf_fwddatavec_hi[DEPTH_LOG2'(WrPtr[DEPTH_LOG2-1:0] + DEPTH_LOG2'(i))][7:0];
         end
         if (stbuf_fwdbyteenvec_lo[DEPTH_LOG2'(WrPtr[DEPTH_LOG2-1:0] + DEPTH_LOG2'(i))][0]) begin
            stbuf_fwddata_lo_dc2[7:0] = stbuf_fwddatavec_lo[DEPTH_LOG2'(WrPtr[DEPTH_LOG2-1:0] + DEPTH_LOG2'(i))][7:0];
         end
         if (stbuf_fwdbyteenvec_hi[DEPTH_LOG2'(WrPtr[DEPTH_LOG2-1:0] + DEPTH_LOG2'(i))][1]) begin
            stbuf_fwddata_hi_dc2[15:8] = stbuf_fwddatavec_hi[DEPTH_LOG2'(WrPtr[DEPTH_LOG2-1:0] + DEPTH_LOG2'(i))][15:8];
         end
         if (stbuf_fwdbyteenvec_lo[DEPTH_LOG2'(WrPtr[DEPTH_LOG2-1:0] + DEPTH_LOG2'(i))][1]) begin
            stbuf_fwddata_lo_dc2[15:8] = stbuf_fwddatavec_lo[DEPTH_LOG2'(WrPtr[DEPTH_LOG2-1:0] + DEPTH_LOG2'(i))][15:8];
         end
         if (stbuf_fwdbyteenvec_hi[DEPTH_LOG2'(WrPtr[DEPTH_LOG2-1:0] + DEPTH_LOG2'(i))][2]) begin
            stbuf_fwddata_hi_dc2[23:16] = stbuf_fwddatavec_hi[DEPTH_LOG2'(WrPtr[DEPTH_LOG2-1:0] + DEPTH_LOG2'(i))][23:16];
         end
         if (stbuf_fwdbyteenvec_lo[DEPTH_LOG2'(WrPtr[DEPTH_LOG2-1:0] + DEPTH_LOG2'(i))][2]) begin
            stbuf_fwddata_lo_dc2[23:16] = stbuf_fwddatavec_lo[DEPTH_LOG2'(WrPtr[DEPTH_LOG2-1:0] + DEPTH_LOG2'(i))][23:16];
         end
         if (stbuf_fwdbyteenvec_hi[DEPTH_LOG2'(WrPtr[DEPTH_LOG2-1:0] + DEPTH_LOG2'(i))][3]) begin
            stbuf_fwddata_hi_dc2[31:24] = stbuf_fwddatavec_hi[DEPTH_LOG2'(WrPtr[DEPTH_LOG2-1:0] + DEPTH_LOG2'(i))][31:24];
         end
         if (stbuf_fwdbyteenvec_lo[DEPTH_LOG2'(WrPtr[DEPTH_LOG2-1:0] + DEPTH_LOG2'(i))][3]) begin
            stbuf_fwddata_lo_dc2[31:24] = stbuf_fwddatavec_lo[DEPTH_LOG2'(WrPtr[DEPTH_LOG2-1:0] + DEPTH_LOG2'(i))][31:24];
         end
      end
   end
   for (genvar i=0; i<BYTE_WIDTH; i++) begin
      assign stbuf_fwdbyteen_hi_fn_dc2[i] = stbuf_fwdbyteen_hi_hi[i] | stbuf_fwdbyteen_hi_lo[i] | stbuf_fwdbyteen_hi_dc2[i];
      assign stbuf_fwdbyteen_lo_fn_dc2[i] = stbuf_fwdbyteen_lo_hi[i] | stbuf_fwdbyteen_lo_lo[i] | stbuf_fwdbyteen_lo_dc2[i];
      assign stbuf_fwddata_hi_fn_dc2[(8*i)+7:(8*i)] = (stbuf_fwdbyteen_hi_hi[i] | stbuf_fwdbyteen_hi_lo[i]) ?
                                                                         (stbuf_fwddata_hi_hi[(8*i)+7:(8*i)] | stbuf_fwddata_hi_lo[(8*i)+7:(8*i)]) :
                                                                         stbuf_fwddata_hi_dc2[(8*i)+7:(8*i)];
      assign stbuf_fwddata_lo_fn_dc2[(8*i)+7:(8*i)] = (stbuf_fwdbyteen_lo_hi[i] | stbuf_fwdbyteen_lo_lo[i]) ?
                                                                         (stbuf_fwddata_lo_hi[(8*i)+7:(8*i)] | stbuf_fwddata_lo_lo[(8*i)+7:(8*i)]) :
                                                                         stbuf_fwddata_lo_dc2[(8*i)+7:(8*i)];
   end
   rvdffs #(.WIDTH(DEPTH_LOG2)) WrPtrff (.din(NxtWrPtr[DEPTH_LOG2-1:0]), .dout(WrPtr[DEPTH_LOG2-1:0]), .en(WrPtrEn), .clk(lsu_stbuf_c1_clk), .*);
   rvdffs #(.WIDTH(DEPTH_LOG2)) RdPtrff (.din(NxtRdPtr[DEPTH_LOG2-1:0]), .dout(RdPtr[DEPTH_LOG2-1:0]), .en(RdPtrEn), .clk(lsu_stbuf_c1_clk), .*);
   rvdffe #(.WIDTH(DATA_WIDTH)) stbuf_fwddata_hi_dc3ff (.din(stbuf_fwddata_hi_fn_dc2[DATA_WIDTH-1:0]), .dout(stbuf_fwddata_hi_dc3[DATA_WIDTH-1:0]), .en(lsu_freeze_c1_dc3_clken), .*);
   rvdffe #(.WIDTH(DATA_WIDTH)) stbuf_fwddata_lo_dc3ff (.din(stbuf_fwddata_lo_fn_dc2[DATA_WIDTH-1:0]), .dout(stbuf_fwddata_lo_dc3[DATA_WIDTH-1:0]), .en(lsu_freeze_c1_dc3_clken), .*);
endmodule
function automatic logic [2:0] f_Enc8to3;
   input logic [7:0] Dec_value;
   logic [2:0]       Enc_value;
   Enc_value[0] = Dec_value[1] | Dec_value[3] | Dec_value[5] | Dec_value[7];
   Enc_value[1] = Dec_value[2] | Dec_value[3] | Dec_value[6] | Dec_value[7];
   Enc_value[2] = Dec_value[4] | Dec_value[5] | Dec_value[6] | Dec_value[7];
   return Enc_value[2:0];
endfunction  
module lsu_bus_buffer
   import swerv_types::*;
(
   input logic                          clk,
   input logic                          rst_l,
   input logic                          scan_mode,
   input logic                          dec_tlu_non_blocking_disable,      
   input logic                          dec_tlu_wb_coalescing_disable,     
   input logic                          dec_tlu_ld_miss_byp_wb_disable,    
   input logic                          dec_tlu_sideeffect_posted_disable,   
   input logic                          lsu_c1_dc3_clk,
   input logic                          lsu_c1_dc4_clk,
   input logic                          lsu_c1_dc5_clk,
   input logic                          lsu_c2_dc3_clk,
   input logic                          lsu_c2_dc4_clk,
   input logic                          lsu_c2_dc5_clk,
   input logic                          lsu_freeze_c1_dc2_clk,
   input logic                          lsu_freeze_c1_dc3_clk,
   input logic                          lsu_freeze_c2_dc2_clk,
   input logic                          lsu_freeze_c2_dc3_clk,
   input logic                          lsu_freeze_c2_dc3_clken,
   input logic                          lsu_bus_ibuf_c1_clk,
   input logic                          lsu_bus_obuf_c1_clk,
   input logic                          lsu_bus_buf_c1_clk,
   input logic                          lsu_free_c2_clk,
   input logic                          lsu_busm_clk,
   input                                lsu_pkt_t lsu_pkt_dc1,             
   input                                lsu_pkt_t lsu_pkt_dc2,             
   input                                lsu_pkt_t lsu_pkt_dc3,             
   input                                lsu_pkt_t lsu_pkt_dc4,             
   input                                lsu_pkt_t lsu_pkt_dc5,             
   input logic [31:0]                   lsu_addr_dc2,                      
   input logic [31:0]                   end_addr_dc2,                      
   input logic [31:0]                   lsu_addr_dc5,                      
   input logic [31:0]                   end_addr_dc5,                      
   input logic [31:0]                   store_data_dc5,                    
   input logic                          no_word_merge_dc5,                 
   input logic                          no_dword_merge_dc5,                
   input logic                          lsu_busreq_dc2,                    
   output logic                         lsu_busreq_dc3,                    
   output logic                         lsu_busreq_dc4,                    
   output logic                         lsu_busreq_dc5,                    
   input logic                          ld_full_hit_dc2,                   
   input logic                          flush_dc2_up,                      
   input logic                          flush_dc3,                         
   input logic                          flush_dc4,                         
   input logic                          flush_dc5,                         
   input logic                          lsu_freeze_dc3,
   input logic                          dec_tlu_cancel_e4,                 
   input logic                          lsu_commit_dc5,                    
   input logic                          is_sideeffects_dc2,                
   input logic                          is_sideeffects_dc5,                
   input logic                          ldst_dual_dc1,                     
   input logic                          ldst_dual_dc2,                     
   input logic                          ldst_dual_dc3,                     
   input logic                          ldst_dual_dc4,                     
   input logic                          ldst_dual_dc5,                     
   input logic [7:0]                   ldst_byteen_ext_dc2,
   output logic                         ld_freeze_dc3,                     
   output logic                         lsu_bus_buffer_pend_any,           
   output logic                         lsu_bus_buffer_full_any,           
   output logic                         lsu_bus_buffer_empty_any,          
   output logic                         ld_bus_error_dc3,                  
   output logic [31:0]                  ld_bus_error_addr_dc3,             
   output logic [31:0]                  ld_bus_data_dc3,                  
   output logic [3:0]                   ld_byte_hit_buf_lo, ld_byte_hit_buf_hi,     
   output logic [31:0]                  ld_fwddata_buf_lo, ld_fwddata_buf_hi,       
   output logic                         lsu_imprecise_error_load_any,      
   output logic                         lsu_imprecise_error_store_any,     
   output logic [31:0]                  lsu_imprecise_error_addr_any,      
   input  logic                                dec_nonblock_load_freeze_dc2,
   output logic                                lsu_nonblock_load_valid_dc3,      
   output logic [3-1:0] lsu_nonblock_load_tag_dc3,        
   output logic                                lsu_nonblock_load_inv_dc5,        
   output logic [3-1:0] lsu_nonblock_load_inv_tag_dc5,    
   output logic                                lsu_nonblock_load_data_valid,     
   output logic                                lsu_nonblock_load_data_error,     
   output logic [3-1:0] lsu_nonblock_load_data_tag,       
   output logic [31:0]                         lsu_nonblock_load_data,           
   output logic                         lsu_pmu_bus_trxn,
   output logic                         lsu_pmu_bus_misaligned,
   output logic                         lsu_pmu_bus_error,
   output logic                         lsu_pmu_bus_busy,
   output logic                            lsu_axi_awvalid,
   input  logic                            lsu_axi_awready,
   output logic [4-1:0]      lsu_axi_awid,
   output logic [31:0]                     lsu_axi_awaddr,
   output logic [3:0]                      lsu_axi_awregion,
   output logic [7:0]                      lsu_axi_awlen,
   output logic [2:0]                      lsu_axi_awsize,
   output logic [1:0]                      lsu_axi_awburst,
   output logic                            lsu_axi_awlock,
   output logic [3:0]                      lsu_axi_awcache,
   output logic [2:0]                      lsu_axi_awprot,
   output logic [3:0]                      lsu_axi_awqos,
   output logic                            lsu_axi_wvalid,
   input  logic                            lsu_axi_wready,
   output logic [63:0]                     lsu_axi_wdata,
   output logic [7:0]                      lsu_axi_wstrb,
   output logic                            lsu_axi_wlast,
   input  logic                            lsu_axi_bvalid,
   output logic                            lsu_axi_bready,
   input  logic [1:0]                      lsu_axi_bresp,
   input  logic [4-1:0]      lsu_axi_bid,
   output logic                            lsu_axi_arvalid,
   input  logic                            lsu_axi_arready,
   output logic [4-1:0]      lsu_axi_arid,
   output logic [31:0]                     lsu_axi_araddr,
   output logic [3:0]                      lsu_axi_arregion,
   output logic [7:0]                      lsu_axi_arlen,
   output logic [2:0]                      lsu_axi_arsize,
   output logic [1:0]                      lsu_axi_arburst,
   output logic                            lsu_axi_arlock,
   output logic [3:0]                      lsu_axi_arcache,
   output logic [2:0]                      lsu_axi_arprot,
   output logic [3:0]                      lsu_axi_arqos,
   input  logic                            lsu_axi_rvalid,
   output logic                            lsu_axi_rready,
   input  logic [4-1:0]      lsu_axi_rid,
   input  logic [63:0]                     lsu_axi_rdata,
   input  logic [1:0]                      lsu_axi_rresp,
   input  logic                            lsu_axi_rlast,
   input logic                          lsu_bus_clk_en,
   input logic                          lsu_bus_clk_en_q
);
localparam TOTAL_INT        = 9;
localparam DCCM_BITS        = 16;
localparam DCCM_BANK_BITS   = 3;
localparam DCCM_NUM_BANKS   = 8;
localparam DCCM_DATA_WIDTH  = 32;
localparam DCCM_FDATA_WIDTH = 39;
localparam DCCM_BYTE_WIDTH  = 4;
localparam DCCM_ECC_WIDTH   = 7;
localparam LSU_RDBUF_DEPTH  = 8;
localparam DMA_BUF_DEPTH    = 4;
localparam LSU_STBUF_DEPTH  = 8;
localparam LSU_SB_BITS      = 16;
localparam DEC_INSTBUF_DEPTH = 4;
localparam ICCM_SIZE         = 512;
localparam ICCM_BITS         = 19;
localparam ICCM_NUM_BANKS    = 8;
localparam ICCM_BANK_BITS    = 3;
localparam ICCM_INDEX_BITS   = 14;
localparam ICCM_BANK_HI      = 4 + (3/4);
localparam ICACHE_TAG_HIGH  = 12;
localparam ICACHE_TAG_LOW   = 6;
localparam ICACHE_IC_DEPTH  = 8;
localparam ICACHE_TAG_DEPTH = 64;
localparam LSU_BUS_TAG     = 4;
localparam DMA_BUS_TAG     = 1;
localparam SB_BUS_TAG      = 1;
localparam IFU_BUS_TAG     = 3;
   typedef enum logic [2:0] {IDLE=3'b000, WAIT=3'b001, CMD=3'b010, RESP=3'b011, DONE=3'b100} state_t;
   localparam DEPTH     = 8;
   localparam DEPTH_LOG2 = 3;
   localparam TIMER     = 8;    
   localparam TIMER_LOG2 = (TIMER < 2) ? 1 : $clog2(TIMER);
   localparam TIMER_MAX = (TIMER == 0) ? TIMER_LOG2'(0) : TIMER_LOG2'(TIMER - 1);   
   logic [3:0]                          ldst_byteen_hi_dc2, ldst_byteen_lo_dc2;
   logic [DEPTH-1:0]                    ld_addr_hitvec_lo, ld_addr_hitvec_hi;
   logic [3:0][DEPTH-1:0]               ld_byte_hitvec_lo, ld_byte_hitvec_hi;
   logic [3:0][DEPTH-1:0]               ld_byte_hitvecfn_lo, ld_byte_hitvecfn_hi;
   logic                                ld_addr_ibuf_hit_lo, ld_addr_ibuf_hit_hi;
   logic [3:0]                          ld_byte_ibuf_hit_lo, ld_byte_ibuf_hit_hi;
   logic [3:0]                          ldst_byteen_dc5;
   logic [7:0]                          ldst_byteen_ext_dc5;
   logic [3:0]                          ldst_byteen_hi_dc5, ldst_byteen_lo_dc5;
   logic [31:0]                         store_data_hi_dc5, store_data_lo_dc5;
   logic                                ldst_samedw_dc5;
   logic                                lsu_nonblock_load_valid_dc4,lsu_nonblock_load_valid_dc5;
   logic [31:0]                         lsu_nonblock_load_data_hi, lsu_nonblock_load_data_lo, lsu_nonblock_data_unalgn;
   logic [1:0]                          lsu_nonblock_addr_offset;
   logic [1:0]                          lsu_nonblock_sz;
   logic                                lsu_nonblock_load_data_valid_hi, lsu_nonblock_load_data_valid_lo;
   logic                                lsu_nonblock_load_data_error_hi, lsu_nonblock_load_data_error_lo;
   logic                                lsu_nonblock_unsign, lsu_nonblock_dual;
   logic                                dec_nonblock_load_freeze_dc3;
   logic                                ld_precise_bus_error;
   logic [DEPTH_LOG2-1:0]               lsu_imprecise_error_load_tag;
   logic [31:0]                         ld_block_bus_data;
   logic [DEPTH-1:0]                    CmdPtr0Dec, CmdPtr1Dec;
   logic [DEPTH_LOG2-1:0]               CmdPtr0, CmdPtr1;
   logic [DEPTH_LOG2-1:0]               WrPtr0_dc3, WrPtr0_dc4, WrPtr0_dc5;
   logic [DEPTH_LOG2-1:0]               WrPtr1_dc3, WrPtr1_dc4, WrPtr1_dc5;
   logic                                found_wrptr0, found_wrptr1, found_cmdptr0, found_cmdptr1;
   logic [3:0]                          buf_numvld_any, buf_numvld_wrcmd_any, buf_numvld_pend_any, buf_numvld_cmd_any;
   logic                                bus_sideeffect_pend;
   logic                                bus_coalescing_disable;
   logic                                ld_freeze_en, ld_freeze_rst;
   logic                                FreezePtrEn;
   logic [DEPTH_LOG2-1:0]               FreezePtr;
   logic [DEPTH_LOG2-1:0]               lsu_imprecise_error_store_tag;
   logic                                bus_addr_match_pending;
   logic                                bus_cmd_sent, bus_cmd_ready;
   logic                                bus_wcmd_sent, bus_wdata_sent;
   logic                                bus_rsp_read, bus_rsp_write;
   logic [LSU_BUS_TAG-1:0]              bus_rsp_read_tag, bus_rsp_write_tag;
   logic                                bus_rsp_read_error, bus_rsp_write_error;
   logic [63:0]                         bus_rsp_rdata;
   state_t [DEPTH-1:0]                  buf_state;
   logic   [DEPTH-1:0][2:0]             buf_state_out;
   logic   [DEPTH-1:0][1:0]             buf_sz;
   logic   [DEPTH-1:0][31:0]            buf_addr;
   logic   [DEPTH-1:0][3:0]             buf_byteen;
   logic   [DEPTH-1:0]                  buf_sideeffect;
   logic   [DEPTH-1:0]                  buf_write;
   logic   [DEPTH-1:0]                  buf_unsign;
   logic   [DEPTH-1:0]                  buf_dual;
   logic   [DEPTH-1:0]                  buf_samedw;
   logic   [DEPTH-1:0]                  buf_nomerge;
   logic   [DEPTH-1:0]                  buf_dualhi;
   logic   [DEPTH-1:0][DEPTH_LOG2-1:0]  buf_dualtag;
   logic   [DEPTH-1:0]                  buf_nb;
   logic   [DEPTH-1:0]                  buf_error;
   logic   [DEPTH-1:0][31:0]            buf_data;
   logic   [DEPTH-1:0][DEPTH-1:0]       buf_age, buf_age_younger, buf_age_temp;
   state_t [DEPTH-1:0]                  buf_nxtstate;
   logic   [DEPTH-1:0]                  buf_rst;
   logic   [DEPTH-1:0]                  buf_state_en;
   logic   [DEPTH-1:0]                  buf_cmd_state_bus_en;
   logic   [DEPTH-1:0]                  buf_resp_state_bus_en;
   logic   [DEPTH-1:0]                  buf_state_bus_en;
   logic   [DEPTH-1:0]                  buf_dual_in;
   logic   [DEPTH-1:0]                  buf_samedw_in;
   logic   [DEPTH-1:0]                  buf_nomerge_in;
   logic   [DEPTH-1:0]                  buf_nb_in;
   logic   [DEPTH-1:0]                  buf_sideeffect_in;
   logic   [DEPTH-1:0]                  buf_unsign_in;
   logic   [DEPTH-1:0][1:0]             buf_sz_in;
   logic   [DEPTH-1:0]                  buf_write_in;
   logic   [DEPTH-1:0]                  buf_wr_en;
   logic   [DEPTH-1:0]                  buf_dualhi_in;
   logic   [DEPTH-1:0][DEPTH_LOG2-1:0]  buf_dualtag_in;
   logic   [DEPTH-1:0][3:0]             buf_byteen_in;
   logic   [DEPTH-1:0][31:0]            buf_addr_in;
   logic   [DEPTH-1:0][31:0]            buf_data_in;
   logic   [DEPTH-1:0]                  buf_error_en;
   logic   [DEPTH-1:0]                  buf_data_en;
   logic   [DEPTH-1:0][DEPTH-1:0]       buf_age_in;
   logic   [DEPTH-1:0][DEPTH-1:0]       buf_ageQ;
   logic                               ibuf_valid;
   logic                               ibuf_dual;
   logic                               ibuf_samedw;
   logic                               ibuf_nomerge;
   logic [DEPTH_LOG2-1:0]              ibuf_tag;
   logic [DEPTH_LOG2-1:0]              ibuf_dualtag;
   logic                               ibuf_nb;
   logic                               ibuf_sideeffect;
   logic                               ibuf_unsign;
   logic                               ibuf_write;
   logic [1:0]                         ibuf_sz;
   logic [3:0]                         ibuf_byteen;
   logic [31:0]                        ibuf_addr;
   logic [31:0]                        ibuf_data;
   logic [TIMER_LOG2-1:0]              ibuf_timer;
   logic                               ibuf_byp;
   logic                               ibuf_wr_en;
   logic                               ibuf_rst;
   logic                               ibuf_force_drain;
   logic                               ibuf_drain_vld;
   logic [DEPTH-1:0]                   ibuf_drainvec_vld;
   logic [DEPTH_LOG2-1:0]              ibuf_tag_in;
   logic [DEPTH_LOG2-1:0]              ibuf_dualtag_in;
   logic [1:0]                         ibuf_sz_in;
   logic [31:0]                        ibuf_addr_in;
   logic [3:0]                         ibuf_byteen_in;
   logic [31:0]                        ibuf_data_in;
   logic [TIMER_LOG2-1:0]              ibuf_timer_in;
   logic [3:0]                         ibuf_byteen_out;
   logic [31:0]                        ibuf_data_out;
   logic                               ibuf_merge_en, ibuf_merge_in;
   logic                               obuf_valid;
   logic                               obuf_write;
   logic                               obuf_sideeffect;
   logic [31:0]                        obuf_addr;
   logic [63:0]                        obuf_data;
   logic [1:0]                         obuf_sz;
   logic [7:0]                         obuf_byteen;
   logic                               obuf_merge;
   logic                               obuf_cmd_done, obuf_data_done;
   logic [LSU_BUS_TAG-1:0]             obuf_tag0;
   logic [LSU_BUS_TAG-1:0]             obuf_tag1;
   logic                               ibuf_buf_byp;
   logic                               obuf_force_wr_en;
   logic                               obuf_wr_wait;
   logic                               obuf_wr_en, obuf_wr_enQ;
   logic                               obuf_rst;
   logic                               obuf_write_in;
   logic                               obuf_sideeffect_in;
   logic [31:0]                        obuf_addr_in;
   logic [63:0]                        obuf_data_in;
   logic [1:0]                         obuf_sz_in;
   logic [7:0]                         obuf_byteen_in;
   logic                               obuf_merge_in;
   logic                               obuf_cmd_done_in, obuf_data_done_in;
   logic [LSU_BUS_TAG-1:0]             obuf_tag0_in;
   logic [LSU_BUS_TAG-1:0]             obuf_tag1_in;
   logic                               obuf_merge_en;
   logic [TIMER_LOG2-1:0]              obuf_wr_timer, obuf_wr_timer_in;
   logic [7:0]                         obuf_byteen0_in, obuf_byteen1_in;
   logic [63:0]                        obuf_data0_in, obuf_data1_in;
   logic                   lsu_axi_awvalid_q, lsu_axi_awready_q;
   logic                   lsu_axi_wvalid_q, lsu_axi_wready_q;
   logic                   lsu_axi_arvalid_q, lsu_axi_arready_q;
   logic                   lsu_axi_bvalid_q, lsu_axi_bready_q;
   logic                   lsu_axi_rvalid_q, lsu_axi_rready_q;
   logic [LSU_BUS_TAG-1:0] lsu_axi_bid_q, lsu_axi_rid_q;
   logic [1:0]             lsu_axi_bresp_q, lsu_axi_rresp_q;
   logic [63:0]            lsu_axi_rdata_q;
   assign ldst_byteen_hi_dc2[3:0]   = ldst_byteen_ext_dc2[7:4];
   assign ldst_byteen_lo_dc2[3:0]   = ldst_byteen_ext_dc2[3:0];
   for (genvar i=0; i<DEPTH; i++) begin
      assign ld_addr_hitvec_lo[i] = (lsu_addr_dc2[31:2] == buf_addr[i][31:2]) & buf_write[i] & ((buf_state[i] == WAIT) | (buf_state[i] == CMD)) & lsu_busreq_dc2;
      assign ld_addr_hitvec_hi[i] = (end_addr_dc2[31:2] == buf_addr[i][31:2]) & buf_write[i] & ((buf_state[i] == WAIT) | (buf_state[i] == CMD)) & lsu_busreq_dc2;
   end
   for (genvar j=0; j<4; j++) begin
     assign ld_byte_hit_buf_lo[j] = |(ld_byte_hitvecfn_lo[j]) | ld_byte_ibuf_hit_lo[j];
     assign ld_byte_hit_buf_hi[j] = |(ld_byte_hitvecfn_hi[j]) | ld_byte_ibuf_hit_hi[j];
     for (genvar i=0; i<DEPTH; i++) begin
         assign ld_byte_hitvec_lo[j][i] = ld_addr_hitvec_lo[i] & buf_byteen[i][j] & ldst_byteen_lo_dc2[j];
         assign ld_byte_hitvec_hi[j][i] = ld_addr_hitvec_hi[i] & buf_byteen[i][j] & ldst_byteen_hi_dc2[j];
         assign ld_byte_hitvecfn_lo[j][i] = ld_byte_hitvec_lo[j][i] & ~(|(ld_byte_hitvec_lo[j] & buf_age_younger[i])) & ~ld_byte_ibuf_hit_lo[j];   
         assign ld_byte_hitvecfn_hi[j][i] = ld_byte_hitvec_hi[j][i] & ~(|(ld_byte_hitvec_hi[j] & buf_age_younger[i])) & ~ld_byte_ibuf_hit_hi[j];   
      end
   end
   assign ld_addr_ibuf_hit_lo = (lsu_addr_dc2[31:2] == ibuf_addr[31:2]) & ibuf_write & ibuf_valid & lsu_busreq_dc2;
   assign ld_addr_ibuf_hit_hi = (end_addr_dc2[31:2] == ibuf_addr[31:2]) & ibuf_write & ibuf_valid & lsu_busreq_dc2;
   for (genvar i=0; i<4; i++) begin
      assign ld_byte_ibuf_hit_lo[i] = ld_addr_ibuf_hit_lo & ibuf_byteen[i] & ldst_byteen_lo_dc2[i];
      assign ld_byte_ibuf_hit_hi[i] = ld_addr_ibuf_hit_hi & ibuf_byteen[i] & ldst_byteen_hi_dc2[i];
   end
   always_comb begin
      ld_fwddata_buf_lo[31:0] = {{8{ld_byte_ibuf_hit_lo[3]}},{8{ld_byte_ibuf_hit_lo[2]}},{8{ld_byte_ibuf_hit_lo[1]}},{8{ld_byte_ibuf_hit_lo[0]}}} & ibuf_data[31:0];
      ld_fwddata_buf_hi[31:0] = {{8{ld_byte_ibuf_hit_hi[3]}},{8{ld_byte_ibuf_hit_hi[2]}},{8{ld_byte_ibuf_hit_hi[1]}},{8{ld_byte_ibuf_hit_hi[0]}}} & ibuf_data[31:0];
      for (int i=0; i<DEPTH; i++) begin
         ld_fwddata_buf_lo[7:0]   |= {8{ld_byte_hitvecfn_lo[0][i]}} & buf_data[i][7:0];
         ld_fwddata_buf_lo[15:8]  |= {8{ld_byte_hitvecfn_lo[1][i]}} & buf_data[i][15:8];
         ld_fwddata_buf_lo[23:16] |= {8{ld_byte_hitvecfn_lo[2][i]}} & buf_data[i][23:16];
         ld_fwddata_buf_lo[31:24] |= {8{ld_byte_hitvecfn_lo[3][i]}} & buf_data[i][31:24];
         ld_fwddata_buf_hi[7:0]   |= {8{ld_byte_hitvecfn_hi[0][i]}} & buf_data[i][7:0];
         ld_fwddata_buf_hi[15:8]  |= {8{ld_byte_hitvecfn_hi[1][i]}} & buf_data[i][15:8];
         ld_fwddata_buf_hi[23:16] |= {8{ld_byte_hitvecfn_hi[2][i]}} & buf_data[i][23:16];
         ld_fwddata_buf_hi[31:24] |= {8{ld_byte_hitvecfn_hi[3][i]}} & buf_data[i][31:24];
      end
   end
   assign bus_coalescing_disable = dec_tlu_wb_coalescing_disable;
   assign ldst_byteen_dc5[3:0] = ({4{lsu_pkt_dc5.by}}   & 4'b0001) |
                                 ({4{lsu_pkt_dc5.half}} & 4'b0011) |
                                 ({4{lsu_pkt_dc5.word}} & 4'b1111);
   assign {ldst_byteen_hi_dc5[3:0], ldst_byteen_lo_dc5[3:0]} = {4'b0,ldst_byteen_dc5[3:0]} << lsu_addr_dc5[1:0];
   assign {store_data_hi_dc5[31:0], store_data_lo_dc5[31:0]} = {32'b0,store_data_dc5[31:0]} << {lsu_addr_dc5[1:0],3'b0};
   assign ldst_samedw_dc5 = (lsu_addr_dc5[3] == end_addr_dc5[3]);
   assign ibuf_byp   = lsu_busreq_dc5 & ((lsu_pkt_dc5.load  | no_word_merge_dc5) & ~ibuf_valid);     
   assign ibuf_wr_en = lsu_busreq_dc5 & (lsu_commit_dc5 | lsu_freeze_dc3) & ~ibuf_byp;
   assign ibuf_rst   = ibuf_drain_vld & ~ibuf_wr_en;
   assign ibuf_force_drain = lsu_busreq_dc2 & ~lsu_busreq_dc3 & ~lsu_busreq_dc4 & ~lsu_busreq_dc5 & ibuf_valid & (lsu_pkt_dc2.load | (ibuf_addr[31:2] != lsu_addr_dc2[31:2]));   
   assign ibuf_drain_vld = ibuf_valid & (((ibuf_wr_en | (ibuf_timer == TIMER_MAX)) & ~(ibuf_merge_en & ibuf_merge_in)) | ibuf_byp | ibuf_force_drain | ibuf_sideeffect | ~ibuf_write | bus_coalescing_disable);
   assign ibuf_tag_in[DEPTH_LOG2-1:0] = (ibuf_merge_en & ibuf_merge_in) ? ibuf_tag[DEPTH_LOG2-1:0] : (ldst_dual_dc5 ? WrPtr1_dc5 : WrPtr0_dc5);
   assign ibuf_dualtag_in[DEPTH_LOG2-1:0] = WrPtr0_dc5;
   assign ibuf_sz_in[1:0]   = {lsu_pkt_dc5.word, lsu_pkt_dc5.half};  
   assign ibuf_addr_in[31:0] = ldst_dual_dc5 ? end_addr_dc5[31:0] : lsu_addr_dc5[31:0];
   assign ibuf_byteen_in[3:0] = (ibuf_merge_en & ibuf_merge_in) ? (ibuf_byteen[3:0] | ldst_byteen_lo_dc5[3:0]) : (ldst_dual_dc5 ? ldst_byteen_hi_dc5[3:0] : ldst_byteen_lo_dc5[3:0]);
   for (genvar i=0; i<4; i++) begin
      assign ibuf_data_in[(8*i)+7:(8*i)] = (ibuf_merge_en & ibuf_merge_in) ? (ldst_byteen_lo_dc5[i] ? store_data_lo_dc5[(8*i)+7:(8*i)] : ibuf_data[(8*i)+7:(8*i)]) :
                                                                             (ldst_dual_dc5 ? store_data_hi_dc5[(8*i)+7:(8*i)] : store_data_lo_dc5[(8*i)+7:(8*i)]);
   end
   assign ibuf_timer_in = ibuf_wr_en ? '0 : (ibuf_timer < TIMER_MAX) ? (ibuf_timer + 1'b1) : ibuf_timer;
   assign ibuf_merge_en = lsu_busreq_dc5 & lsu_commit_dc5 & lsu_pkt_dc5.store & ibuf_valid & ibuf_write & (lsu_addr_dc5[31:2] == ibuf_addr[31:2]) & ~is_sideeffects_dc5 & ~bus_coalescing_disable;
   assign ibuf_merge_in = ~ldst_dual_dc5;    
   for (genvar i=0; i<4; i++) begin
      assign ibuf_byteen_out[i] = (ibuf_merge_en & ~ibuf_merge_in) ? (ibuf_byteen[i] | ldst_byteen_lo_dc5[i]) : ibuf_byteen[i];
      assign ibuf_data_out[(8*i)+7:(8*i)] = (ibuf_merge_en & ~ibuf_merge_in) ? (ldst_byteen_lo_dc5[i] ? store_data_lo_dc5[(8*i)+7:(8*i)] : ibuf_data[(8*i)+7:(8*i)]) :
                                                                                                        ibuf_data[(8*i)+7:(8*i)];
   end
   rvdffsc #(.WIDTH(1))              ibuf_valid_ff     (.din(1'b1),                        .dout(ibuf_valid),      .en(ibuf_wr_en), .clear(ibuf_rst), .clk(lsu_free_c2_clk), .*);
   rvdffs  #(.WIDTH(DEPTH_LOG2))     ibuf_tagff        (.din(ibuf_tag_in),                 .dout(ibuf_tag),        .en(ibuf_wr_en),                   .clk(lsu_bus_ibuf_c1_clk), .*);
   rvdffs  #(.WIDTH(DEPTH_LOG2))     ibuf_dualtagff    (.din(ibuf_dualtag_in),             .dout(ibuf_dualtag),    .en(ibuf_wr_en),                   .clk(lsu_bus_ibuf_c1_clk), .*);
   rvdffs  #(.WIDTH(1))              ibuf_dualff       (.din(ldst_dual_dc5),               .dout(ibuf_dual),       .en(ibuf_wr_en),                   .clk(lsu_bus_ibuf_c1_clk), .*);
   rvdffs  #(.WIDTH(1))              ibuf_samedwff     (.din(ldst_samedw_dc5),             .dout(ibuf_samedw),     .en(ibuf_wr_en),                   .clk(lsu_bus_ibuf_c1_clk), .*);
   rvdffs  #(.WIDTH(1))              ibuf_nomergeff    (.din(no_dword_merge_dc5),          .dout(ibuf_nomerge),    .en(ibuf_wr_en),                   .clk(lsu_bus_ibuf_c1_clk), .*);
   rvdffs  #(.WIDTH(1))              ibuf_nbff         (.din(lsu_nonblock_load_valid_dc5), .dout(ibuf_nb),         .en(ibuf_wr_en),                   .clk(lsu_bus_ibuf_c1_clk), .*);
   rvdffs  #(.WIDTH(1))              ibuf_sideeffectff (.din(is_sideeffects_dc5),          .dout(ibuf_sideeffect), .en(ibuf_wr_en),                   .clk(lsu_bus_ibuf_c1_clk), .*);
   rvdffs  #(.WIDTH(1))              ibuf_unsignff     (.din(lsu_pkt_dc5.unsign),          .dout(ibuf_unsign),     .en(ibuf_wr_en),                   .clk(lsu_bus_ibuf_c1_clk), .*);
   rvdffs  #(.WIDTH(1))              ibuf_writeff      (.din(lsu_pkt_dc5.store),           .dout(ibuf_write),      .en(ibuf_wr_en),                   .clk(lsu_bus_ibuf_c1_clk), .*);
   rvdffs  #(.WIDTH(2))              ibuf_szff         (.din(ibuf_sz_in[1:0]),             .dout(ibuf_sz),         .en(ibuf_wr_en),                   .clk(lsu_bus_ibuf_c1_clk), .*);
   rvdffe  #(.WIDTH(32))             ibuf_addrff       (.din(ibuf_addr_in[31:0]),          .dout(ibuf_addr),       .en(ibuf_wr_en),                                              .*);
   rvdffs  #(.WIDTH(4))              ibuf_byteenff     (.din(ibuf_byteen_in[3:0]),         .dout(ibuf_byteen),     .en(ibuf_wr_en),                   .clk(lsu_bus_ibuf_c1_clk), .*);
   rvdffe  #(.WIDTH(32))             ibuf_dataff       (.din(ibuf_data_in[31:0]),          .dout(ibuf_data),       .en(ibuf_wr_en),                                              .*);
   rvdff   #(.WIDTH(TIMER_LOG2))     ibuf_timerff      (.din(ibuf_timer_in),               .dout(ibuf_timer),                                         .clk(lsu_free_c2_clk),     .*);
   assign obuf_wr_wait = (buf_numvld_wrcmd_any[3:0] == 4'b1) & (buf_numvld_cmd_any[3:0] == 4'b1) & (obuf_wr_timer != TIMER_MAX) & ~bus_coalescing_disable & ~buf_nomerge[CmdPtr0] & ~obuf_force_wr_en;
   assign obuf_wr_timer_in = obuf_wr_en ? 3'b0: (((buf_numvld_cmd_any > 4'b0) & (obuf_wr_timer < TIMER_MAX)) ? (obuf_wr_timer + 1'b1) : obuf_wr_timer);
   assign obuf_force_wr_en = lsu_busreq_dc2 & ~lsu_busreq_dc3 & ~lsu_busreq_dc4 & ~lsu_busreq_dc5 & ~ibuf_valid & (buf_numvld_cmd_any[3:0] == 4'b1) & (lsu_addr_dc2[31:2] != buf_addr[CmdPtr0][31:2]);    
   assign ibuf_buf_byp = ibuf_byp & (buf_numvld_pend_any[3:0] == 4'b0) & ~ldst_dual_dc5 & lsu_pkt_dc5.store;
   assign obuf_wr_en = (lsu_bus_clk_en & ((ibuf_buf_byp & lsu_commit_dc5) |
                                          ((buf_state[CmdPtr0] == CMD) & found_cmdptr0 & ~buf_cmd_state_bus_en[CmdPtr0] &
                                          (~(buf_dual[CmdPtr0] & buf_samedw[CmdPtr0] & ~buf_write[CmdPtr0]) | found_cmdptr1 | buf_nomerge[CmdPtr0] | obuf_force_wr_en)))) &
                       (bus_cmd_ready | ~obuf_valid) & ~obuf_wr_wait & ~bus_sideeffect_pend & ~bus_addr_match_pending;
   assign obuf_rst   = bus_cmd_sent & ~obuf_wr_en;
   assign obuf_write_in = ibuf_buf_byp ? lsu_pkt_dc5.store : buf_write[CmdPtr0];
   assign obuf_sideeffect_in = ibuf_buf_byp ? is_sideeffects_dc5 : buf_sideeffect[CmdPtr0];
   assign obuf_addr_in[31:0] = ibuf_buf_byp ? lsu_addr_dc5[31:0] : buf_addr[CmdPtr0];
   assign obuf_sz_in[1:0]    = ibuf_buf_byp ? {lsu_pkt_dc5.word, lsu_pkt_dc5.half} : buf_sz[CmdPtr0];
   assign obuf_merge_in      = obuf_merge_en;
   assign obuf_tag0_in[LSU_BUS_TAG-1:0] = ibuf_buf_byp ? LSU_BUS_TAG'(WrPtr0_dc5) : LSU_BUS_TAG'(CmdPtr0);
   assign obuf_tag1_in[LSU_BUS_TAG-1:0] = LSU_BUS_TAG'(CmdPtr1);
   assign obuf_cmd_done_in    = ~(obuf_wr_en | obuf_rst) & (obuf_cmd_done | bus_wcmd_sent);
   assign obuf_data_done_in   = ~(obuf_wr_en | obuf_rst) & (obuf_data_done | bus_wdata_sent);
   assign obuf_byteen0_in[7:0] = ibuf_buf_byp ? (lsu_addr_dc5[2] ? {ldst_byteen_lo_dc5[3:0],4'b0} : {4'b0,ldst_byteen_lo_dc5[3:0]}) :
                                                (buf_addr[CmdPtr0][2] ? {buf_byteen[CmdPtr0],4'b0} : {4'b0,buf_byteen[CmdPtr0]});
   assign obuf_byteen1_in[7:0] = buf_addr[CmdPtr1][2] ? {buf_byteen[CmdPtr1],4'b0} : {4'b0,buf_byteen[CmdPtr1]};
   assign obuf_data0_in[63:0]  = ibuf_buf_byp ? (lsu_addr_dc5[2] ? {store_data_lo_dc5[31:0],32'b0} : {32'b0,store_data_lo_dc5[31:0]}) :
                                                (buf_addr[CmdPtr0][2] ? {buf_data[CmdPtr0],32'b0} : {32'b0,buf_data[CmdPtr0]});
   assign obuf_data1_in[63:0]  = buf_addr[CmdPtr1][2] ? {buf_data[CmdPtr1],32'b0} : {32'b0,buf_data[CmdPtr1]};
   for (genvar i=0 ;i<8; i++) begin
      assign obuf_byteen_in[i] = obuf_byteen0_in[i] | (obuf_merge_en & obuf_byteen1_in[i]);
      assign obuf_data_in[(8*i)+7:(8*i)] = (obuf_merge_en & obuf_byteen1_in[i]) ? obuf_data1_in[(8*i)+7:(8*i)] : obuf_data0_in[(8*i)+7:(8*i)];
   end
   assign obuf_merge_en = (CmdPtr0 != CmdPtr1) & found_cmdptr0 & found_cmdptr1 & (buf_state[CmdPtr0] == CMD) & (buf_state[CmdPtr1] == CMD) & ~buf_cmd_state_bus_en[CmdPtr0] & ~buf_sideeffect[CmdPtr0] &
                          (~buf_write[CmdPtr0] & buf_dual[CmdPtr0] & ~buf_dualhi[CmdPtr0] & buf_samedw[CmdPtr0]);   
   rvdff_fpga   #(1)              obuf_wren_ff      (.din(obuf_wr_en),        .dout(obuf_wr_enQ),                                        .clk(lsu_busm_clk), .clken(lsu_bus_clk_en), .rawclk(clk), .*);
   rvdff_fpga   #(1)              obuf_cmd_done_ff  (.din(obuf_cmd_done_in),  .dout(obuf_cmd_done),                                      .clk(lsu_busm_clk), .clken(lsu_bus_clk_en), .rawclk(clk), .*);
   rvdff_fpga   #(1)              obuf_data_done_ff (.din(obuf_data_done_in), .dout(obuf_data_done),                                     .clk(lsu_busm_clk), .clken(lsu_bus_clk_en), .rawclk(clk), .*);
   rvdffsc_fpga #(1)              obuf_valid_ff     (.din(1'b1),              .dout(obuf_valid),     .clear(obuf_rst),  .en(obuf_wr_en), .clk(lsu_busm_clk), .clken(lsu_bus_clk_en), .rawclk(clk), .*);
   rvdff_fpga   #(TIMER_LOG2)     obuf_timerff      (.din(obuf_wr_timer_in),  .dout(obuf_wr_timer),                                      .clk(lsu_busm_clk), .clken(lsu_bus_clk_en), .rawclk(clk), .*);
   rvdff_fpga #(1)            lsu_axi_awvalid_ff (.din(lsu_axi_awvalid),                .dout(lsu_axi_awvalid_q),                .clk(lsu_busm_clk), .clken(lsu_bus_clk_en), .rawclk(clk), .*);
   rvdff_fpga #(1)            lsu_axi_awready_ff (.din(lsu_axi_awready),                .dout(lsu_axi_awready_q),                .clk(lsu_busm_clk), .clken(lsu_bus_clk_en), .rawclk(clk), .*);
   rvdff_fpga #(1)            lsu_axi_wvalid_ff  (.din(lsu_axi_wvalid),                 .dout(lsu_axi_wvalid_q),                 .clk(lsu_busm_clk), .clken(lsu_bus_clk_en), .rawclk(clk), .*);
   rvdff_fpga #(1)            lsu_axi_wready_ff  (.din(lsu_axi_wready),                 .dout(lsu_axi_wready_q),                 .clk(lsu_busm_clk), .clken(lsu_bus_clk_en), .rawclk(clk), .*);
   rvdff_fpga #(1)            lsu_axi_arvalid_ff (.din(lsu_axi_arvalid),                .dout(lsu_axi_arvalid_q),                .clk(lsu_busm_clk), .clken(lsu_bus_clk_en), .rawclk(clk), .*);
   rvdff_fpga #(1)            lsu_axi_arready_ff (.din(lsu_axi_arready),                .dout(lsu_axi_arready_q),                .clk(lsu_busm_clk), .clken(lsu_bus_clk_en), .rawclk(clk), .*);
   rvdff_fpga  #(1)           lsu_axi_bvalid_ff  (.din(lsu_axi_bvalid),                 .dout(lsu_axi_bvalid_q),                 .clk(lsu_busm_clk), .clken(lsu_bus_clk_en), .rawclk(clk), .*);
   rvdff_fpga  #(1)           lsu_axi_bready_ff  (.din(lsu_axi_bready),                 .dout(lsu_axi_bready_q),                 .clk(lsu_busm_clk), .clken(lsu_bus_clk_en), .rawclk(clk), .*);
   rvdff_fpga  #(2)           lsu_axi_bresp_ff   (.din(lsu_axi_bresp[1:0]),             .dout(lsu_axi_bresp_q[1:0]),             .clk(lsu_busm_clk), .clken(lsu_bus_clk_en), .rawclk(clk), .*);
   rvdff_fpga  #(LSU_BUS_TAG) lsu_axi_bid_ff     (.din(lsu_axi_bid[LSU_BUS_TAG-1:0]),   .dout(lsu_axi_bid_q[LSU_BUS_TAG-1:0]),   .clk(lsu_busm_clk), .clken(lsu_bus_clk_en), .rawclk(clk), .*);
   rvdff_fpga  #(1)           lsu_axi_rvalid_ff  (.din(lsu_axi_rvalid),                 .dout(lsu_axi_rvalid_q),                 .clk(lsu_busm_clk), .clken(lsu_bus_clk_en), .rawclk(clk), .*);
   rvdff_fpga  #(1)           lsu_axi_rready_ff  (.din(lsu_axi_rready),                 .dout(lsu_axi_rready_q),                 .clk(lsu_busm_clk), .clken(lsu_bus_clk_en), .rawclk(clk), .*);
   rvdff_fpga  #(2)           lsu_axi_rresp_ff   (.din(lsu_axi_rresp[1:0]),             .dout(lsu_axi_rresp_q[1:0]),             .clk(lsu_busm_clk), .clken(lsu_bus_clk_en), .rawclk(clk), .*);
   rvdff_fpga  #(LSU_BUS_TAG) lsu_axi_rid_ff     (.din(lsu_axi_rid[LSU_BUS_TAG-1:0]),   .dout(lsu_axi_rid_q[LSU_BUS_TAG-1:0]),   .clk(lsu_busm_clk), .clken(lsu_bus_clk_en), .rawclk(clk), .*);
   rvdffs  #(.WIDTH(LSU_BUS_TAG))    obuf_tag0ff       (.din(obuf_tag0_in),                .dout(obuf_tag0),       .en(obuf_wr_en),                   .clk(lsu_bus_obuf_c1_clk), .*);
   rvdffs  #(.WIDTH(LSU_BUS_TAG))    obuf_tag1ff       (.din(obuf_tag1_in),                .dout(obuf_tag1),       .en(obuf_wr_en),                   .clk(lsu_bus_obuf_c1_clk), .*);
   rvdffs  #(.WIDTH(1))              obuf_mergeff      (.din(obuf_merge_in),               .dout(obuf_merge),      .en(obuf_wr_en),                   .clk(lsu_bus_obuf_c1_clk), .*);
   rvdffs  #(.WIDTH(1))              obuf_writeff      (.din(obuf_write_in),               .dout(obuf_write),      .en(obuf_wr_en),                   .clk(lsu_bus_obuf_c1_clk), .*);
   rvdffs  #(.WIDTH(1))              obuf_sideeffectff (.din(obuf_sideeffect_in),          .dout(obuf_sideeffect), .en(obuf_wr_en),                   .clk(lsu_bus_obuf_c1_clk), .*);
   rvdffs  #(.WIDTH(2))              obuf_szff         (.din(obuf_sz_in[1:0]),             .dout(obuf_sz),         .en(obuf_wr_en),                   .clk(lsu_bus_obuf_c1_clk), .*);
   rvdffe  #(.WIDTH(32))             obuf_addrff       (.din(obuf_addr_in[31:0]),          .dout(obuf_addr),       .en(obuf_wr_en),                                              .*);
   rvdffs  #(.WIDTH(8))              obuf_byteenff     (.din(obuf_byteen_in[7:0]),         .dout(obuf_byteen),     .en(obuf_wr_en),                   .clk(lsu_bus_obuf_c1_clk), .*);
   rvdffe  #(.WIDTH(64))             obuf_dataff       (.din(obuf_data_in[63:0]),          .dout(obuf_data),       .en(obuf_wr_en),                                              .*);
   always_comb begin
      WrPtr0_dc3[DEPTH_LOG2-1:0] = '0;
      WrPtr1_dc3[DEPTH_LOG2-1:0] = '0;
      found_wrptr0  = '0;
      found_wrptr1  = '0;
      for (int i=0; i<DEPTH; i++) begin
         if (~found_wrptr0) begin
            WrPtr0_dc3[DEPTH_LOG2-1:0] = DEPTH_LOG2'(i);
            found_wrptr0 = (buf_state[i] == IDLE) & ~((ibuf_valid & (ibuf_tag == DEPTH_LOG2'(i)))                                               |
                                                      (lsu_busreq_dc4 & ((WrPtr0_dc4 == DEPTH_LOG2'(i)) | (ldst_dual_dc4 & (WrPtr1_dc4 == DEPTH_LOG2'(i))))) |
                                                      (lsu_busreq_dc5 & ((WrPtr0_dc5 == DEPTH_LOG2'(i)) | (ldst_dual_dc5 & (WrPtr1_dc5 == DEPTH_LOG2'(i))))));
         end
      end
      for (int i=0; i<DEPTH; i++) begin
         if (~found_wrptr1) begin
            WrPtr1_dc3[DEPTH_LOG2-1:0] = DEPTH_LOG2'(i);
            found_wrptr1 = (buf_state[i] == IDLE) & ~((ibuf_valid & (ibuf_tag == DEPTH_LOG2'(i)))                                               |
                                                      (lsu_busreq_dc3 & (WrPtr0_dc3 == DEPTH_LOG2'(i)))                                         |
                                                      (lsu_busreq_dc4 & ((WrPtr0_dc4 == DEPTH_LOG2'(i)) | (ldst_dual_dc4 & (WrPtr1_dc4 == DEPTH_LOG2'(i))))) |
                                                      (lsu_busreq_dc5 & ((WrPtr0_dc5 == DEPTH_LOG2'(i)) | (ldst_dual_dc5 & (WrPtr1_dc5 == DEPTH_LOG2'(i))))));
         end
      end
   end
   for (genvar i=0; i<DEPTH; i++) begin
      assign CmdPtr0Dec[i] = ~(|buf_age[i]) & (buf_state[i] == CMD) & ~buf_cmd_state_bus_en[i];
      assign CmdPtr1Dec[i] = ~(|(buf_age[i] & ~CmdPtr0Dec)) & ~CmdPtr0Dec[i] & (buf_state[i] == CMD) & ~buf_cmd_state_bus_en[i];
   end
   assign found_cmdptr0 = |CmdPtr0Dec;
   assign found_cmdptr1 = |CmdPtr1Dec;
   assign CmdPtr0 = f_Enc8to3(8'(CmdPtr0Dec[DEPTH-1:0]));
   assign CmdPtr1 = f_Enc8to3(8'(CmdPtr1Dec[DEPTH-1:0]));
   for (genvar i=0; i<DEPTH; i++) begin: GenAgeVec
      for (genvar j=0; j<DEPTH; j++) begin
         assign buf_age_in[i][j] = (((buf_state[i] == IDLE) & buf_state_en[i]) &
                                           (((buf_state[j] == WAIT) | ((buf_state[j] == CMD) & ~buf_cmd_state_bus_en[j]))            |        
                                            (ibuf_drain_vld & lsu_busreq_dc5 & (ibuf_byp | ldst_dual_dc5) & (DEPTH_LOG2'(i) == WrPtr0_dc5) & (DEPTH_LOG2'(j) == ibuf_tag))  |        
                                            (ibuf_byp & lsu_busreq_dc5 & ldst_dual_dc5 & (DEPTH_LOG2'(i) == WrPtr1_dc5) & (DEPTH_LOG2'(j) == WrPtr0_dc5))))      |      
                                   buf_age[i][j];
         assign buf_age[i][j]    = buf_ageQ[i][j] & ~((buf_state[j] == CMD) & buf_cmd_state_bus_en[j]);   
         assign buf_age_younger[i][j] = (i == j) ? 1'b0: (~buf_age[i][j] & (buf_state[j] != IDLE));    
         assign buf_age_temp[i][j] = buf_age[i][j] & ~(CmdPtr0 == DEPTH_LOG2'(j));    
      end
   end
   for (genvar i=0; i<DEPTH; i++) begin
      assign ibuf_drainvec_vld[i] = (ibuf_drain_vld & (i == ibuf_tag));
      assign buf_byteen_in[i]     = ibuf_drainvec_vld[i] ? ibuf_byteen_out[3:0] : ((ibuf_byp & ldst_dual_dc5 & (i == WrPtr1_dc5)) ? ldst_byteen_hi_dc5[3:0] : ldst_byteen_lo_dc5[3:0]);
      assign buf_addr_in[i]       = ibuf_drainvec_vld[i] ? ibuf_addr[31:0] : ((ibuf_byp & ldst_dual_dc5 & (i == WrPtr1_dc5)) ? end_addr_dc5[31:0] : lsu_addr_dc5[31:0]);
      assign buf_dual_in[i]       = ibuf_drainvec_vld[i] ? ibuf_dual : ldst_dual_dc5;
      assign buf_samedw_in[i]     = ibuf_drainvec_vld[i] ? ibuf_samedw : ldst_samedw_dc5;
      assign buf_nomerge_in[i]    = ibuf_drainvec_vld[i] ? (ibuf_nomerge | ibuf_force_drain) : no_dword_merge_dc5;
      assign buf_dualhi_in[i]     = ibuf_drainvec_vld[i] ? ibuf_dual : (ibuf_byp & ldst_dual_dc5 & (i == WrPtr1_dc5));    
      assign buf_dualtag_in[i]    = ibuf_drainvec_vld[i] ? ibuf_dualtag : ((ibuf_byp & ldst_dual_dc5 & (i == WrPtr1_dc5)) ? WrPtr0_dc5 : WrPtr1_dc5);
      assign buf_nb_in[i]         = ibuf_drainvec_vld[i] ? ibuf_nb : lsu_nonblock_load_valid_dc5;
      assign buf_sideeffect_in[i] = ibuf_drainvec_vld[i] ? ibuf_sideeffect : is_sideeffects_dc5;
      assign buf_unsign_in[i]     = ibuf_drainvec_vld[i] ? ibuf_unsign : lsu_pkt_dc5.unsign;
      assign buf_sz_in[i]         = ibuf_drainvec_vld[i] ? ibuf_sz : {lsu_pkt_dc5.word, lsu_pkt_dc5.half};
      assign buf_write_in[i]      = ibuf_drainvec_vld[i] ? ibuf_write : lsu_pkt_dc5.store;
      always_comb begin
         buf_nxtstate[i]          = IDLE;
         buf_state_en[i]          = '0;
         buf_cmd_state_bus_en[i]  = '0;
         buf_resp_state_bus_en[i] = '0;
         buf_state_bus_en[i]      = '0;
         buf_wr_en[i]             = '0;
         buf_data_in[i]           = '0;
         buf_data_en[i]           = '0;
         buf_error_en[i]          = '0;
         buf_rst[i]               = '0;
         case (buf_state[i])
            IDLE: begin
                     buf_nxtstate[i] = lsu_bus_clk_en ? CMD : WAIT;
                     buf_state_en[i] = (lsu_busreq_dc5 & (lsu_commit_dc5 | lsu_freeze_dc3) & (((ibuf_byp | ldst_dual_dc5) & ~ibuf_merge_en & (i == WrPtr0_dc5)) | (ibuf_byp & ldst_dual_dc5 & (i == WrPtr1_dc5)))) |
                                       (ibuf_drain_vld & (i == ibuf_tag));
                     buf_wr_en[i]    = buf_state_en[i];
                     buf_data_en[i]  = buf_state_en[i];
                     buf_data_in[i]   = (ibuf_drain_vld & (i == ibuf_tag)) ? ibuf_data_out[31:0] : store_data_lo_dc5[31:0];
            end
            WAIT: begin
                     buf_nxtstate[i] = CMD;
                     buf_state_en[i] = lsu_bus_clk_en;
            end
            CMD: begin
                     buf_nxtstate[i]          = RESP;
                     buf_cmd_state_bus_en[i]  = ((obuf_tag0 == i) | (obuf_merge & (obuf_tag1 == i))) & obuf_valid & obuf_wr_enQ;   
                     buf_state_bus_en[i]      = buf_cmd_state_bus_en[i];
                     buf_state_en[i]          = buf_state_bus_en[i] & lsu_bus_clk_en;
            end
            RESP: begin
                     buf_nxtstate[i]           = (buf_write[i] & ~bus_rsp_write_error) ? IDLE : DONE;    
                     buf_resp_state_bus_en[i]  = (bus_rsp_write & (bus_rsp_write_tag == LSU_BUS_TAG'(i))) |
                                                 (bus_rsp_read  & ((bus_rsp_read_tag == LSU_BUS_TAG'(i)) | (buf_dual[i] & buf_dualhi[i] & ~buf_write[i] & buf_samedw[i] & (bus_rsp_read_tag == LSU_BUS_TAG'(buf_dualtag[i])))));
                     buf_state_bus_en[i]       = buf_resp_state_bus_en[i];
                     buf_state_en[i]           = buf_state_bus_en[i] & lsu_bus_clk_en;
                     buf_data_en[i]            = buf_state_bus_en[i] & ~buf_write[i] & bus_rsp_read & lsu_bus_clk_en;
                     buf_error_en[i]           = buf_state_bus_en[i] & lsu_bus_clk_en & ((bus_rsp_read_error  & (bus_rsp_read_tag  == LSU_BUS_TAG'(i))) |
                                                                                         (bus_rsp_write_error & (bus_rsp_write_tag == LSU_BUS_TAG'(i))));
                     buf_data_in[i][31:0]      = (buf_state_en[i] & ~buf_error_en[i]) ? (buf_addr[i][2] ? bus_rsp_rdata[63:32] : bus_rsp_rdata[31:0]) : bus_rsp_rdata[31:0];
            end
            DONE: begin
                     buf_nxtstate[i]           = IDLE;
                     buf_rst[i]                = lsu_bus_clk_en_q & (buf_write[i] | ~buf_dual[i] | (buf_state[buf_dualtag[i]] == DONE));
                     buf_state_en[i]           = buf_rst[i];
            end
            default : begin
                     buf_nxtstate[i]          = IDLE;
                     buf_state_en[i]          = '0;
                     buf_cmd_state_bus_en[i]  = '0;
                     buf_resp_state_bus_en[i] = '0;
                     buf_state_bus_en[i]      = '0;
                     buf_wr_en[i]             = '0;
                     buf_data_in[i]           = '0;
                     buf_data_en[i]           = '0;
                     buf_error_en[i]          = '0;
                     buf_rst[i]               = '0;
            end
         endcase
      end
      assign buf_state[i] = state_t'(buf_state_out[i]);
      rvdffs  #(.WIDTH($bits(state_t))) buf_state_ff     (.din(buf_nxtstate[i]),             .dout(buf_state_out[i]),    .en(buf_state_en[i]),                                        .clk(lsu_bus_buf_c1_clk), .*);
      rvdff   #(.WIDTH(DEPTH))          buf_ageff        (.din(buf_age_in[i]),               .dout(buf_ageQ[i]),                                                                    .clk(lsu_bus_buf_c1_clk), .*);
      rvdffs  #(.WIDTH(DEPTH_LOG2))     buf_dualtagff    (.din(buf_dualtag_in[i]),           .dout(buf_dualtag[i]),    .en(buf_wr_en[i]),                                           .clk(lsu_bus_buf_c1_clk), .*);
      rvdffs  #(.WIDTH(1))              buf_dualff       (.din(buf_dual_in[i]),              .dout(buf_dual[i]),       .en(buf_wr_en[i]),                                           .clk(lsu_bus_buf_c1_clk), .*);
      rvdffs  #(.WIDTH(1))              buf_samedwff     (.din(buf_samedw_in[i]),            .dout(buf_samedw[i]),     .en(buf_wr_en[i]),                                           .clk(lsu_bus_buf_c1_clk), .*);
      rvdffs  #(.WIDTH(1))              buf_nomergeff    (.din(buf_nomerge_in[i]),           .dout(buf_nomerge[i]),    .en(buf_wr_en[i]),                                           .clk(lsu_bus_buf_c1_clk), .*);
      rvdffs  #(.WIDTH(1))              buf_dualhiff     (.din(buf_dualhi_in[i]),            .dout(buf_dualhi[i]),     .en(buf_wr_en[i]),                                           .clk(lsu_bus_buf_c1_clk), .*);
      rvdffs  #(.WIDTH(1))              buf_nbff         (.din(buf_nb_in[i]),                .dout(buf_nb[i]),         .en(buf_wr_en[i]),                                           .clk(lsu_bus_buf_c1_clk), .*);
      rvdffs  #(.WIDTH(1))              buf_sideeffectff (.din(buf_sideeffect_in[i]),        .dout(buf_sideeffect[i]), .en(buf_wr_en[i]),                                           .clk(lsu_bus_buf_c1_clk), .*);
      rvdffs  #(.WIDTH(1))              buf_unsignff     (.din(buf_unsign_in[i]),            .dout(buf_unsign[i]),     .en(buf_wr_en[i]),                                           .clk(lsu_bus_buf_c1_clk), .*);
      rvdffs  #(.WIDTH(1))              buf_writeff      (.din(buf_write_in[i]),             .dout(buf_write[i]),      .en(buf_wr_en[i]),                                           .clk(lsu_bus_buf_c1_clk), .*);
      rvdffs  #(.WIDTH(2))              buf_szff         (.din(buf_sz_in[i]),                .dout(buf_sz[i]),         .en(buf_wr_en[i]),                                           .clk(lsu_bus_buf_c1_clk), .*);
      rvdffe  #(.WIDTH(32))             buf_addrff       (.din(buf_addr_in[i][31:0]),        .dout(buf_addr[i]),       .en(buf_wr_en[i]),                                                                     .*);
      rvdffs  #(.WIDTH(4))              buf_byteenff     (.din(buf_byteen_in[i][3:0]),       .dout(buf_byteen[i]),     .en(buf_wr_en[i]),                                           .clk(lsu_bus_buf_c1_clk), .*);
      rvdffe  #(.WIDTH(32))             buf_dataff       (.din(buf_data_in[i][31:0]),        .dout(buf_data[i]),       .en(buf_data_en[i]),                                                                   .*);
      rvdffsc #(.WIDTH(1))              buf_errorff      (.din(1'b1),                        .dout(buf_error[i]),      .en(buf_error_en[i]),                    .clear(buf_rst[i]), .clk(lsu_bus_buf_c1_clk), .*);
   end
   always_comb begin
      buf_numvld_any[3:0] =  ({3'b0,(lsu_pkt_dc1.valid & ~lsu_pkt_dc1.dma)} << (lsu_pkt_dc1.valid & ldst_dual_dc1)) +
                             ({3'b0,lsu_busreq_dc2} << ldst_dual_dc2) +
                             ({3'b0,lsu_busreq_dc3} << ldst_dual_dc3) +
                             ({3'b0,lsu_busreq_dc4} << ldst_dual_dc4) +
                             ({3'b0,lsu_busreq_dc5} << ldst_dual_dc5) +
                             {3'b0,ibuf_valid};
      buf_numvld_wrcmd_any[3:0] = 4'b0;
      buf_numvld_cmd_any[3:0] = 4'b0;
      buf_numvld_pend_any[3:0] = 4'b0;
      for (int i=0; i<DEPTH; i++) begin
         buf_numvld_any[3:0] += {3'b0, (buf_state[i] != IDLE)};
         buf_numvld_wrcmd_any[3:0] += {3'b0, (buf_write[i] & (buf_state[i] == CMD) & ~buf_cmd_state_bus_en[i])};
         buf_numvld_cmd_any[3:0]   += {3'b0, ((buf_state[i] == CMD) & ~buf_cmd_state_bus_en[i])};
         buf_numvld_pend_any[3:0]   += {3'b0, ((buf_state[i] == WAIT) | ((buf_state[i] == CMD) & ~buf_cmd_state_bus_en[i]))};
      end
   end
   assign lsu_bus_buffer_pend_any = (buf_numvld_pend_any != 0);
   assign lsu_bus_buffer_full_any = (buf_numvld_any[3:0] >= (DEPTH-1));
   assign lsu_bus_buffer_empty_any = ~(|buf_state[DEPTH-1:0]) & ~ibuf_valid & ~obuf_valid;
   assign FreezePtrEn  = lsu_busreq_dc3 & lsu_pkt_dc3.load & ld_freeze_dc3;
   assign ld_freeze_en = (is_sideeffects_dc2 | dec_nonblock_load_freeze_dc2 | dec_tlu_non_blocking_disable) & lsu_busreq_dc2 & lsu_pkt_dc2.load & ~lsu_freeze_dc3 & ~flush_dc2_up & ~ld_full_hit_dc2;
   always_comb begin
      ld_freeze_rst = flush_dc3 | (dec_tlu_cancel_e4 & ld_freeze_dc3);
      for (int i=0; i<DEPTH; i++) begin
         ld_freeze_rst |= (buf_rst[i] & (DEPTH_LOG2'(i) == FreezePtr) & ~FreezePtrEn & ld_freeze_dc3);
      end
   end
   assign ld_block_bus_data[31:0]     = 32'(({({32{buf_dual[FreezePtr]}} & buf_data[buf_dualtag[FreezePtr]]), buf_data[FreezePtr][31:0]}) >> (8*buf_addr[FreezePtr][1:0]));
   assign ld_precise_bus_error         = (buf_error[FreezePtr] | (buf_dual[FreezePtr] & buf_error[buf_dualtag[FreezePtr]])) & ~buf_write[FreezePtr] & buf_rst[FreezePtr] & lsu_freeze_dc3 & ld_freeze_rst & ~flush_dc3;    
   assign ld_bus_error_addr_dc3[31:0]  = buf_addr[FreezePtr][31:0];
   assign lsu_nonblock_load_valid_dc3 = lsu_busreq_dc3 & lsu_pkt_dc3.valid & lsu_pkt_dc3.load & ~flush_dc3 & ~dec_nonblock_load_freeze_dc3 & ~lsu_freeze_dc3 & ~dec_tlu_non_blocking_disable;
   assign lsu_nonblock_load_tag_dc3[DEPTH_LOG2-1:0] = WrPtr0_dc3[DEPTH_LOG2-1:0];
   assign lsu_nonblock_load_inv_dc5 = lsu_nonblock_load_valid_dc5 & ~lsu_commit_dc5;
   assign lsu_nonblock_load_inv_tag_dc5[DEPTH_LOG2-1:0] = WrPtr0_dc5[DEPTH_LOG2-1:0];       
   always_comb begin
      lsu_nonblock_load_data_valid_lo = '0;
      lsu_nonblock_load_data_valid_hi = '0;
      lsu_nonblock_load_data_error_lo = '0;
      lsu_nonblock_load_data_error_hi = '0;
      lsu_nonblock_load_data_tag[DEPTH_LOG2-1:0] = '0;
      lsu_nonblock_load_data_lo[31:0] = '0;
      lsu_nonblock_load_data_hi[31:0] = '0;
      for (int i=0; i<DEPTH; i++) begin
          lsu_nonblock_load_data_valid_hi      |= lsu_bus_clk_en_q & (buf_state[i] == DONE) & buf_rst[i] & ~dec_tlu_non_blocking_disable & buf_nb[i] & ~buf_error[i] & (buf_dual[i] & buf_dualhi[i]);
          lsu_nonblock_load_data_valid_lo      |= lsu_bus_clk_en_q & (buf_state[i] == DONE) & buf_rst[i] & ~dec_tlu_non_blocking_disable & buf_nb[i] & ~buf_error[i] & (~buf_dual[i] | ~buf_dualhi[i]);
          lsu_nonblock_load_data_error_hi      |= lsu_bus_clk_en_q & (buf_state[i] == DONE) & buf_rst[i] & ~dec_tlu_non_blocking_disable & buf_error[i] & buf_nb[i] & (buf_dual[i] & buf_dualhi[i]);
          lsu_nonblock_load_data_error_lo      |= lsu_bus_clk_en_q & (buf_state[i] == DONE) & buf_rst[i] & ~dec_tlu_non_blocking_disable & buf_error[i] & buf_nb[i] & (~buf_dual[i] | ~buf_dualhi[i]);
          lsu_nonblock_load_data_tag[DEPTH_LOG2-1:0]   |= DEPTH_LOG2'(i) & {DEPTH_LOG2{((buf_state[i] == DONE) & buf_nb[i] & buf_rst[i] & (~buf_dual[i] | ~buf_dualhi[i]))}};
          lsu_nonblock_load_data_lo[31:0]      |= buf_data[i][31:0] & {32{((buf_state[i] == DONE) & buf_nb[i] & buf_rst[i] & (~buf_dual[i] | ~buf_dualhi[i]))}};
          lsu_nonblock_load_data_hi[31:0]      |= buf_data[i][31:0] & {32{((buf_state[i] == DONE) & buf_nb[i] & buf_rst[i] & (buf_dual[i] & buf_dualhi[i]))}};
      end
   end
   assign lsu_nonblock_addr_offset[1:0] = buf_addr[lsu_nonblock_load_data_tag][1:0];
   assign lsu_nonblock_sz[1:0]          = buf_sz[lsu_nonblock_load_data_tag][1:0];
   assign lsu_nonblock_unsign           = buf_unsign[lsu_nonblock_load_data_tag];
   assign lsu_nonblock_dual             = buf_dual[lsu_nonblock_load_data_tag];
   assign lsu_nonblock_data_unalgn[31:0] = 32'({lsu_nonblock_load_data_hi[31:0], lsu_nonblock_load_data_lo[31:0]} >> 8*lsu_nonblock_addr_offset[1:0]);
   assign lsu_nonblock_load_data_valid = lsu_nonblock_load_data_valid_lo & (~lsu_nonblock_dual | lsu_nonblock_load_data_valid_hi);
   assign lsu_nonblock_load_data_error = lsu_nonblock_load_data_error_lo | (lsu_nonblock_dual & lsu_nonblock_load_data_error_hi);
   assign lsu_nonblock_load_data = ({32{ lsu_nonblock_unsign & (lsu_nonblock_sz[1:0] == 2'b00)}} & {24'b0,lsu_nonblock_data_unalgn[7:0]}) |
                                   ({32{ lsu_nonblock_unsign & (lsu_nonblock_sz[1:0] == 2'b01)}} & {16'b0,lsu_nonblock_data_unalgn[15:0]}) |
                                   ({32{~lsu_nonblock_unsign & (lsu_nonblock_sz[1:0] == 2'b00)}} & {{24{lsu_nonblock_data_unalgn[7]}}, lsu_nonblock_data_unalgn[7:0]}) |
                                   ({32{~lsu_nonblock_unsign & (lsu_nonblock_sz[1:0] == 2'b01)}} & {{16{lsu_nonblock_data_unalgn[15]}},lsu_nonblock_data_unalgn[15:0]}) |
                                   ({32{(lsu_nonblock_sz[1:0] == 2'b10)}} & lsu_nonblock_data_unalgn[31:0]);
   always_comb begin
      bus_sideeffect_pend = obuf_valid & obuf_sideeffect & dec_tlu_sideeffect_posted_disable;
      for (int i=0; i<DEPTH; i++) begin
         bus_sideeffect_pend |= ((buf_state[i] == RESP) & buf_sideeffect[i] & dec_tlu_sideeffect_posted_disable);
      end
   end
   assign lsu_imprecise_error_load_any       = lsu_nonblock_load_data_error;    
   always_comb begin
      bus_addr_match_pending = '0;
      for (int i=0; i<DEPTH; i++) begin
         bus_addr_match_pending |= (obuf_valid & (obuf_addr[31:3] == buf_addr[i][31:3]) & (buf_state[i] == RESP) & ~((obuf_tag0 == LSU_BUS_TAG'(i)) | (obuf_merge & (obuf_tag1 == LSU_BUS_TAG'(i)))));
      end
   end
   always_comb begin
      lsu_imprecise_error_store_any = '0;
      lsu_imprecise_error_store_tag = '0;
      for (int i=0; i<DEPTH; i++) begin
         lsu_imprecise_error_store_any |= lsu_bus_clk_en_q & (buf_state[i] == DONE) & buf_error[i] & buf_write[i];
         lsu_imprecise_error_store_tag |= DEPTH_LOG2'(i) & {DEPTH_LOG2{((buf_state[i] == DONE) & buf_error[i] & buf_write[i])}};
      end
   end
   assign lsu_imprecise_error_addr_any[31:0] = lsu_imprecise_error_load_any ? buf_addr[lsu_nonblock_load_data_tag] : buf_addr[lsu_imprecise_error_store_tag];
   assign bus_cmd_ready                      = obuf_write ? ((obuf_cmd_done | obuf_data_done) ? (obuf_cmd_done ? lsu_axi_wready : lsu_axi_awready) : (lsu_axi_awready & lsu_axi_wready)) : lsu_axi_arready;
   assign bus_wcmd_sent                      = lsu_axi_awvalid & lsu_axi_awready;
   assign bus_wdata_sent                     = lsu_axi_wvalid & lsu_axi_wready;
   assign bus_cmd_sent                       = ((obuf_cmd_done | bus_wcmd_sent) & (obuf_data_done | bus_wdata_sent)) | (lsu_axi_arvalid & lsu_axi_arready);
   assign bus_rsp_read                       = lsu_axi_rvalid_q & lsu_axi_rready_q;
   assign bus_rsp_write                      = lsu_axi_bvalid_q & lsu_axi_bready_q;
   assign bus_rsp_read_tag[LSU_BUS_TAG-1:0]  = lsu_axi_rid_q[LSU_BUS_TAG-1:0];
   assign bus_rsp_write_tag[LSU_BUS_TAG-1:0] = lsu_axi_bid_q[LSU_BUS_TAG-1:0];
   assign bus_rsp_write_error                = bus_rsp_write & (lsu_axi_bresp_q[1:0] != 2'b0);
   assign bus_rsp_read_error                 = bus_rsp_read  & (lsu_axi_rresp_q[1:0] != 2'b0);
   assign bus_rsp_rdata[63:0]                = lsu_axi_rdata_q[63:0];
   assign lsu_axi_awvalid               = obuf_valid & obuf_write & ~obuf_cmd_done & ~bus_addr_match_pending;
   assign lsu_axi_awid[LSU_BUS_TAG-1:0] = LSU_BUS_TAG'(obuf_tag0);
   assign lsu_axi_awaddr[31:0]          = obuf_sideeffect ? obuf_addr[31:0] : {obuf_addr[31:3],3'b0};
   assign lsu_axi_awsize[2:0]           = obuf_sideeffect ? {1'b0, obuf_sz[1:0]} : 3'b011;
   assign lsu_axi_awprot[2:0]           = '0;
   assign lsu_axi_awcache[3:0]          = obuf_sideeffect ? 4'b0 : 4'b1111;
   assign lsu_axi_awregion[3:0]         = obuf_addr[31:28];
   assign lsu_axi_awlen[7:0]            = '0;
   assign lsu_axi_awburst[1:0]          = 2'b01;
   assign lsu_axi_awqos[3:0]            = '0;
   assign lsu_axi_awlock                = '0;
   assign lsu_axi_wvalid                = obuf_valid & obuf_write & ~obuf_data_done & ~bus_addr_match_pending;
   assign lsu_axi_wstrb[7:0]            = obuf_byteen[7:0] & {8{obuf_write}};
   assign lsu_axi_wdata[63:0]           = obuf_data[63:0];
   assign lsu_axi_wlast                 = '1;
   assign lsu_axi_arvalid               = obuf_valid & ~obuf_write & ~bus_addr_match_pending;
   assign lsu_axi_arid[LSU_BUS_TAG-1:0] = LSU_BUS_TAG'(obuf_tag0);
   assign lsu_axi_araddr[31:0]          = obuf_sideeffect ? obuf_addr[31:0] : {obuf_addr[31:3],3'b0};
   assign lsu_axi_arsize[2:0]           = obuf_sideeffect ? {1'b0, obuf_sz[1:0]} : 3'b011;
   assign lsu_axi_arprot[2:0]           = '0;
   assign lsu_axi_arcache[3:0]          = obuf_sideeffect ? 4'b0 : 4'b1111;
   assign lsu_axi_arregion[3:0]         = obuf_addr[31:28];
   assign lsu_axi_arlen[7:0]            = '0;
   assign lsu_axi_arburst[1:0]          = 2'b01;
   assign lsu_axi_arqos[3:0]            = '0;
   assign lsu_axi_arlock                = '0;
   assign lsu_axi_bready = 1;
   assign lsu_axi_rready = 1;
   assign lsu_pmu_bus_trxn  = (lsu_axi_awvalid_q & lsu_axi_awready_q) | (lsu_axi_wvalid_q & lsu_axi_wready_q) | (lsu_axi_arvalid_q & lsu_axi_arready_q);
   assign lsu_pmu_bus_misaligned = lsu_busreq_dc2 & ldst_dual_dc2;
   assign lsu_pmu_bus_error = ld_bus_error_dc3 | lsu_imprecise_error_load_any | lsu_imprecise_error_store_any;
   assign lsu_pmu_bus_busy  = (lsu_axi_awvalid_q & ~lsu_axi_awready_q) | (lsu_axi_wvalid_q & ~lsu_axi_wready_q) | (lsu_axi_arvalid_q & ~lsu_axi_arready_q);
   rvdffe #(.WIDTH(64))          lsu_axi_rdata_ff   (.din(lsu_axi_rdata[63:0]),            .dout(lsu_axi_rdata_q[63:0]),            .en(lsu_axi_rvalid & lsu_bus_clk_en), .*);
   rvdffsc #(.WIDTH(1))          ld_freezeff            (.din(1'b1),                    .dout(ld_freeze_dc3),       .en(ld_freeze_en), .clear(ld_freeze_rst), .clk(lsu_free_c2_clk), .*);
   rvdffs  #(.WIDTH(DEPTH_LOG2)) lsu_FreezePtrff        (.din(WrPtr0_dc3),              .dout(FreezePtr),           .en(FreezePtrEn),                         .clk(lsu_free_c2_clk), .*);
   rvdff   #(.WIDTH(1))          ld_bus_errorff         (.din(ld_precise_bus_error),    .dout(ld_bus_error_dc3),                                              .clk(lsu_free_c2_clk), .*);
   rvdff   #(.WIDTH(32))         ld_bus_dataff          (.din(ld_block_bus_data[31:0]), .dout(ld_bus_data_dc3[31:0]),                                         .clk(lsu_free_c2_clk), .*);
   rvdff #(.WIDTH(DEPTH_LOG2)) lsu_WrPtr0_dc4ff (.din(WrPtr0_dc3), .dout(WrPtr0_dc4), .clk(lsu_c2_dc4_clk), .*);
   rvdff #(.WIDTH(DEPTH_LOG2)) lsu_WrPtr0_dc5ff (.din(WrPtr0_dc4), .dout(WrPtr0_dc5), .clk(lsu_c2_dc5_clk), .*);
   rvdff #(.WIDTH(DEPTH_LOG2)) lsu_WrPtr1_dc4ff (.din(WrPtr1_dc3), .dout(WrPtr1_dc4), .clk(lsu_c2_dc4_clk), .*);
   rvdff #(.WIDTH(DEPTH_LOG2)) lsu_WrPtr1_dc5ff (.din(WrPtr1_dc4), .dout(WrPtr1_dc5), .clk(lsu_c2_dc5_clk), .*);
   rvdff #(.WIDTH(1)) lsu_busreq_dc3ff (.din(lsu_busreq_dc2 & ~lsu_freeze_dc3 & ~ld_full_hit_dc2), .dout(lsu_busreq_dc3), .clk(lsu_c2_dc3_clk), .*);   
   rvdff #(.WIDTH(1)) lsu_busreq_dc4ff (.din(lsu_busreq_dc3 & ~flush_dc4),      .dout(lsu_busreq_dc4), .clk(lsu_c2_dc4_clk), .*);
   rvdff #(.WIDTH(1)) lsu_busreq_dc5ff (.din(lsu_busreq_dc4 & ~flush_dc5),      .dout(lsu_busreq_dc5), .clk(lsu_c2_dc5_clk), .*);
   rvdff_fpga #(.WIDTH(1)) dec_nonblock_load_freeze_dc3ff (.din(dec_nonblock_load_freeze_dc2), .dout(dec_nonblock_load_freeze_dc3), .clk(lsu_freeze_c2_dc3_clk), .clken(lsu_freeze_c2_dc3_clken), .rawclk(clk), .*);
   rvdff #(.WIDTH(1)) lsu_nonblock_load_valid_dc4ff  (.din(lsu_nonblock_load_valid_dc3),  .dout(lsu_nonblock_load_valid_dc4), .clk(lsu_c2_dc4_clk), .*);
   rvdff #(.WIDTH(1)) lsu_nonblock_load_valid_dc5ff  (.din(lsu_nonblock_load_valid_dc4),  .dout(lsu_nonblock_load_valid_dc5), .clk(lsu_c2_dc5_clk), .*);
endmodule  
module lsu_bus_intf
   import swerv_types::*;
(
   input logic                          clk,
   input logic                          rst_l,
   input logic                          scan_mode,
   input logic                          dec_tlu_non_blocking_disable,      
   input logic                          dec_tlu_wb_coalescing_disable,     
   input logic                          dec_tlu_ld_miss_byp_wb_disable,    
   input logic                          dec_tlu_sideeffect_posted_disable,   
   input logic                          lsu_c1_dc3_clk,
   input logic                          lsu_c1_dc4_clk,
   input logic                          lsu_c1_dc5_clk,
   input logic                          lsu_c2_dc3_clk,
   input logic                          lsu_c2_dc4_clk,
   input logic                          lsu_c2_dc5_clk,
   input logic                          lsu_freeze_c1_dc2_clk,
   input logic                          lsu_freeze_c1_dc3_clk,
   input logic                          lsu_freeze_c2_dc2_clk,
   input logic                          lsu_freeze_c2_dc3_clk,
   input logic                          lsu_freeze_c1_dc2_clken,
   input logic                          lsu_freeze_c1_dc3_clken,
   input logic                          lsu_freeze_c2_dc2_clken,
   input logic                          lsu_freeze_c2_dc3_clken,
   input logic                          lsu_bus_ibuf_c1_clk,
   input logic                          lsu_bus_obuf_c1_clk,
   input logic                          lsu_bus_buf_c1_clk,
   input logic                          lsu_free_c2_clk,
   input logic                          free_clk,
   input logic                          lsu_busm_clk,
   input logic                          lsu_busreq_dc2,                    
   input                                lsu_pkt_t lsu_pkt_dc1,             
   input                                lsu_pkt_t lsu_pkt_dc2,             
   input                                lsu_pkt_t lsu_pkt_dc3,             
   input                                lsu_pkt_t lsu_pkt_dc4,             
   input                                lsu_pkt_t lsu_pkt_dc5,             
   input logic [31:0]                   lsu_addr_dc1,                      
   input logic [31:0]                   lsu_addr_dc2,                      
   input logic [31:0]                   lsu_addr_dc3,                      
   input logic [31:0]                   lsu_addr_dc4,                      
   input logic [31:0]                   lsu_addr_dc5,                      
   input logic [31:0]                   end_addr_dc1,                      
   input logic [31:0]                   end_addr_dc2,                      
   input logic [31:0]                   end_addr_dc3,                      
   input logic [31:0]                   end_addr_dc4,                      
   input logic [31:0]                   end_addr_dc5,                      
   input logic                          addr_external_dc2,                 
   input logic                          addr_external_dc3,                 
   input logic                          addr_external_dc4,                 
   input logic                          addr_external_dc5,                 
   input logic [63:0]                   store_data_dc2,                    
   input logic [63:0]                   store_data_dc3,                    
   input logic [31:0]                   store_data_dc4,                    
   input logic [31:0]                   store_data_dc5,                    
   input logic                          lsu_commit_dc5,                    
   input logic                          is_sideeffects_dc2,                
   input logic                          is_sideeffects_dc3,                
   input logic                          flush_dc2_up,                      
   input logic                          flush_dc3,                         
   input logic                          flush_dc4,                         
   input logic                          flush_dc5,                         
   input logic                          dec_tlu_cancel_e4,                 
   output logic                         lsu_freeze_dc3,                    
   output logic                         lsu_busreq_dc5,                    
   output logic                         lsu_bus_buffer_pend_any,           
   output logic                         lsu_bus_buffer_full_any,           
   output logic                         lsu_bus_buffer_empty_any,          
   output logic [31:0]                  bus_read_data_dc3,                 
   output logic                         ld_bus_error_dc3,                  
   output logic [31:0]                  ld_bus_error_addr_dc3,             
   output logic                         lsu_imprecise_error_load_any,      
   output logic                         lsu_imprecise_error_store_any,     
   output logic [31:0]                  lsu_imprecise_error_addr_any,      
   input  logic                                dec_nonblock_load_freeze_dc2,
   output logic                                lsu_nonblock_load_valid_dc3,      
   output logic [3-1:0] lsu_nonblock_load_tag_dc3,        
   output logic                                lsu_nonblock_load_inv_dc5,        
   output logic [3-1:0] lsu_nonblock_load_inv_tag_dc5,    
   output logic                                lsu_nonblock_load_data_valid,     
   output logic                                lsu_nonblock_load_data_error,     
   output logic [3-1:0] lsu_nonblock_load_data_tag,       
   output logic [31:0]                         lsu_nonblock_load_data,           
   output logic                         lsu_pmu_bus_trxn,
   output logic                         lsu_pmu_bus_misaligned,
   output logic                         lsu_pmu_bus_error,
   output logic                         lsu_pmu_bus_busy,
   output logic                            lsu_axi_awvalid,
   input  logic                            lsu_axi_awready,
   output logic [4-1:0]      lsu_axi_awid,
   output logic [31:0]                     lsu_axi_awaddr,
   output logic [3:0]                      lsu_axi_awregion,
   output logic [7:0]                      lsu_axi_awlen,
   output logic [2:0]                      lsu_axi_awsize,
   output logic [1:0]                      lsu_axi_awburst,
   output logic                            lsu_axi_awlock,
   output logic [3:0]                      lsu_axi_awcache,
   output logic [2:0]                      lsu_axi_awprot,
   output logic [3:0]                      lsu_axi_awqos,
   output logic                            lsu_axi_wvalid,
   input  logic                            lsu_axi_wready,
   output logic [63:0]                     lsu_axi_wdata,
   output logic [7:0]                      lsu_axi_wstrb,
   output logic                            lsu_axi_wlast,
   input  logic                            lsu_axi_bvalid,
   output logic                            lsu_axi_bready,
   input  logic [1:0]                      lsu_axi_bresp,
   input  logic [4-1:0]      lsu_axi_bid,
   output logic                            lsu_axi_arvalid,
   input  logic                            lsu_axi_arready,
   output logic [4-1:0]      lsu_axi_arid,
   output logic [31:0]                     lsu_axi_araddr,
   output logic [3:0]                      lsu_axi_arregion,
   output logic [7:0]                      lsu_axi_arlen,
   output logic [2:0]                      lsu_axi_arsize,
   output logic [1:0]                      lsu_axi_arburst,
   output logic                            lsu_axi_arlock,
   output logic [3:0]                      lsu_axi_arcache,
   output logic [2:0]                      lsu_axi_arprot,
   output logic [3:0]                      lsu_axi_arqos,
   input  logic                            lsu_axi_rvalid,
   output logic                            lsu_axi_rready,
   input  logic [4-1:0]      lsu_axi_rid,
   input  logic [63:0]                     lsu_axi_rdata,
   input  logic [1:0]                      lsu_axi_rresp,
   input  logic                            lsu_axi_rlast,
   input logic                          lsu_bus_clk_en
);
localparam TOTAL_INT        = 9;
localparam DCCM_BITS        = 16;
localparam DCCM_BANK_BITS   = 3;
localparam DCCM_NUM_BANKS   = 8;
localparam DCCM_DATA_WIDTH  = 32;
localparam DCCM_FDATA_WIDTH = 39;
localparam DCCM_BYTE_WIDTH  = 4;
localparam DCCM_ECC_WIDTH   = 7;
localparam LSU_RDBUF_DEPTH  = 8;
localparam DMA_BUF_DEPTH    = 4;
localparam LSU_STBUF_DEPTH  = 8;
localparam LSU_SB_BITS      = 16;
localparam DEC_INSTBUF_DEPTH = 4;
localparam ICCM_SIZE         = 512;
localparam ICCM_BITS         = 19;
localparam ICCM_NUM_BANKS    = 8;
localparam ICCM_BANK_BITS    = 3;
localparam ICCM_INDEX_BITS   = 14;
localparam ICCM_BANK_HI      = 4 + (3/4);
localparam ICACHE_TAG_HIGH  = 12;
localparam ICACHE_TAG_LOW   = 6;
localparam ICACHE_IC_DEPTH  = 8;
localparam ICACHE_TAG_DEPTH = 64;
localparam LSU_BUS_TAG     = 4;
localparam DMA_BUS_TAG     = 1;
localparam SB_BUS_TAG      = 1;
localparam IFU_BUS_TAG     = 3;
   logic              ld_freeze_dc3;
   logic              lsu_bus_clk_en_q;
   logic              ldst_dual_dc1, ldst_dual_dc2, ldst_dual_dc3, ldst_dual_dc4, ldst_dual_dc5;
   logic              lsu_busreq_dc3, lsu_busreq_dc4;
   logic [3:0]        ldst_byteen_dc2, ldst_byteen_dc3, ldst_byteen_dc4, ldst_byteen_dc5;
   logic [7:0]        ldst_byteen_ext_dc2, ldst_byteen_ext_dc3, ldst_byteen_ext_dc4, ldst_byteen_ext_dc5;
   logic [3:0]        ldst_byteen_hi_dc2, ldst_byteen_hi_dc3, ldst_byteen_hi_dc4, ldst_byteen_hi_dc5;
   logic [3:0]        ldst_byteen_lo_dc2, ldst_byteen_lo_dc3, ldst_byteen_lo_dc4, ldst_byteen_lo_dc5;
   logic              is_sideeffects_dc4, is_sideeffects_dc5;
   logic [63:0]       store_data_ext_dc3, store_data_ext_dc4, store_data_ext_dc5;
   logic [31:0]       store_data_hi_dc3, store_data_hi_dc4, store_data_hi_dc5;
   logic [31:0]       store_data_lo_dc3, store_data_lo_dc4, store_data_lo_dc5;
   logic              addr_match_dw_lo_dc5_dc4, addr_match_dw_lo_dc5_dc3, addr_match_dw_lo_dc5_dc2;
   logic              addr_match_word_lo_dc5_dc4, addr_match_word_lo_dc5_dc3, addr_match_word_lo_dc5_dc2;
   logic              no_word_merge_dc5, no_dword_merge_dc5;
   logic              ld_addr_dc3hit_lo_lo, ld_addr_dc3hit_hi_lo, ld_addr_dc3hit_lo_hi, ld_addr_dc3hit_hi_hi;
   logic              ld_addr_dc4hit_lo_lo, ld_addr_dc4hit_hi_lo, ld_addr_dc4hit_lo_hi, ld_addr_dc4hit_hi_hi;
   logic              ld_addr_dc5hit_lo_lo, ld_addr_dc5hit_hi_lo, ld_addr_dc5hit_lo_hi, ld_addr_dc5hit_hi_hi;
   logic [3:0]        ld_byte_dc3hit_lo_lo, ld_byte_dc3hit_hi_lo, ld_byte_dc3hit_lo_hi, ld_byte_dc3hit_hi_hi;
   logic [3:0]        ld_byte_dc4hit_lo_lo, ld_byte_dc4hit_hi_lo, ld_byte_dc4hit_lo_hi, ld_byte_dc4hit_hi_hi;
   logic [3:0]        ld_byte_dc5hit_lo_lo, ld_byte_dc5hit_hi_lo, ld_byte_dc5hit_lo_hi, ld_byte_dc5hit_hi_hi;
   logic [3:0]        ld_byte_hit_lo, ld_byte_dc3hit_lo, ld_byte_dc4hit_lo, ld_byte_dc5hit_lo;
   logic [3:0]        ld_byte_hit_hi, ld_byte_dc3hit_hi, ld_byte_dc4hit_hi, ld_byte_dc5hit_hi;
   logic [31:0]       ld_fwddata_dc3pipe_lo, ld_fwddata_dc4pipe_lo, ld_fwddata_dc5pipe_lo;
   logic [31:0]       ld_fwddata_dc3pipe_hi, ld_fwddata_dc4pipe_hi, ld_fwddata_dc5pipe_hi;
   logic [3:0]        ld_byte_hit_buf_lo, ld_byte_hit_buf_hi;
   logic [31:0]       ld_fwddata_buf_lo, ld_fwddata_buf_hi;
   logic              ld_hit_rdbuf_hi, ld_hit_rdbuf_lo;
   logic [31:0]       ld_fwddata_rdbuf_hi, ld_fwddata_rdbuf_lo;
   logic [63:0]       ld_fwddata_lo, ld_fwddata_hi;
   logic [31:0]       ld_fwddata_dc2, ld_fwddata_dc3;
   logic [31:0]       ld_bus_data_dc3;
   logic              ld_full_hit_hi_dc2, ld_full_hit_lo_dc2;
   logic              ld_hit_dc2, ld_full_hit_dc2, ld_full_hit_dc3;
   logic              is_aligned_dc5;
   logic [63:32]     ld_fwddata_dc2_nc;
   logic              lsu_write_buffer_empty_any;
   assign lsu_write_buffer_empty_any = 1'b1;
   assign ldst_byteen_dc2[3:0] = ({4{lsu_pkt_dc2.by}}   & 4'b0001) |
                                 ({4{lsu_pkt_dc2.half}} & 4'b0011) |
                                 ({4{lsu_pkt_dc2.word}} & 4'b1111);
   assign ldst_dual_dc1 = (lsu_addr_dc1[2] != end_addr_dc1[2]);
   assign lsu_freeze_dc3 = ld_freeze_dc3 & ~(flush_dc4 | flush_dc5);
   assign is_aligned_dc5  = (lsu_pkt_dc5.word & (lsu_addr_dc5[1:0] == 2'b0)) |
                            (lsu_pkt_dc5.half & (lsu_addr_dc5[0] == 1'b0));
   lsu_bus_buffer bus_buffer (
      .*
   );
   assign addr_match_dw_lo_dc5_dc4 = (lsu_addr_dc5[31:3] == lsu_addr_dc4[31:3]);
   assign addr_match_dw_lo_dc5_dc3 = (lsu_addr_dc5[31:3] == lsu_addr_dc3[31:3]);
   assign addr_match_dw_lo_dc5_dc2 = (lsu_addr_dc5[31:3] == lsu_addr_dc2[31:3]);
   assign addr_match_word_lo_dc5_dc4 = addr_match_dw_lo_dc5_dc4 & ~(lsu_addr_dc5[2]^lsu_addr_dc4[2]);
   assign addr_match_word_lo_dc5_dc3 = addr_match_dw_lo_dc5_dc3 & ~(lsu_addr_dc5[2]^lsu_addr_dc3[2]);
   assign addr_match_word_lo_dc5_dc2 = addr_match_dw_lo_dc5_dc2 & ~(lsu_addr_dc5[2]^lsu_addr_dc2[2]);
   assign no_word_merge_dc5  = lsu_busreq_dc5 & ~ldst_dual_dc5 &
                               ((lsu_busreq_dc4 & (lsu_pkt_dc4.load | ~addr_match_word_lo_dc5_dc4)) |
                                (lsu_busreq_dc3 & ~lsu_busreq_dc4 & (lsu_pkt_dc3.load | ~addr_match_word_lo_dc5_dc3)) |
                                (lsu_busreq_dc2 & ~lsu_busreq_dc3 & ~lsu_busreq_dc4 & (lsu_pkt_dc2.load | ~addr_match_word_lo_dc5_dc2)));
   assign no_dword_merge_dc5  = lsu_busreq_dc5 & ~ldst_dual_dc5 &
                                ((lsu_busreq_dc4 & (lsu_pkt_dc4.load | ~addr_match_dw_lo_dc5_dc4)) |
                                 (lsu_busreq_dc3 & ~lsu_busreq_dc4 & (lsu_pkt_dc3.load | ~addr_match_dw_lo_dc5_dc3)) |
                                 (lsu_busreq_dc2 & ~lsu_busreq_dc3 & ~lsu_busreq_dc4 & (lsu_pkt_dc2.load | ~addr_match_dw_lo_dc5_dc2)));
   assign ldst_byteen_ext_dc2[7:0] = {4'b0,ldst_byteen_dc2[3:0]} << lsu_addr_dc2[1:0];
   assign ldst_byteen_ext_dc3[7:0] = {4'b0,ldst_byteen_dc3[3:0]} << lsu_addr_dc3[1:0];
   assign ldst_byteen_ext_dc4[7:0] = {4'b0,ldst_byteen_dc4[3:0]} << lsu_addr_dc4[1:0];
   assign ldst_byteen_ext_dc5[7:0] = {4'b0,ldst_byteen_dc5[3:0]} << lsu_addr_dc5[1:0];
   assign store_data_ext_dc3[63:0] = {32'b0,store_data_dc3[31:0]} << {lsu_addr_dc3[1:0],3'b0};
   assign store_data_ext_dc4[63:0] = {32'b0,store_data_dc4[31:0]} << {lsu_addr_dc4[1:0],3'b0};
   assign store_data_ext_dc5[63:0] = {32'b0,store_data_dc5[31:0]} << {lsu_addr_dc5[1:0],3'b0};
   assign ldst_byteen_hi_dc2[3:0]   = ldst_byteen_ext_dc2[7:4];
   assign ldst_byteen_lo_dc2[3:0]   = ldst_byteen_ext_dc2[3:0];
   assign ldst_byteen_hi_dc3[3:0]   = ldst_byteen_ext_dc3[7:4];
   assign ldst_byteen_lo_dc3[3:0]   = ldst_byteen_ext_dc3[3:0];
   assign ldst_byteen_hi_dc4[3:0]   = ldst_byteen_ext_dc4[7:4];
   assign ldst_byteen_lo_dc4[3:0]   = ldst_byteen_ext_dc4[3:0];
   assign ldst_byteen_hi_dc5[3:0]   = ldst_byteen_ext_dc5[7:4];
   assign ldst_byteen_lo_dc5[3:0]   = ldst_byteen_ext_dc5[3:0];
   assign store_data_hi_dc3[31:0]   = store_data_ext_dc3[63:32];
   assign store_data_lo_dc3[31:0]   = store_data_ext_dc3[31:0];
   assign store_data_hi_dc4[31:0]   = store_data_ext_dc4[63:32];
   assign store_data_lo_dc4[31:0]   = store_data_ext_dc4[31:0];
   assign store_data_hi_dc5[31:0]   = store_data_ext_dc5[63:32];
   assign store_data_lo_dc5[31:0]   = store_data_ext_dc5[31:0];
   assign ld_addr_dc3hit_lo_lo = (lsu_addr_dc2[31:2] == lsu_addr_dc3[31:2]) & lsu_pkt_dc3.valid & lsu_pkt_dc3.store & lsu_busreq_dc2;
   assign ld_addr_dc3hit_lo_hi = (end_addr_dc2[31:2] == lsu_addr_dc3[31:2]) & lsu_pkt_dc3.valid & lsu_pkt_dc3.store & lsu_busreq_dc2;
   assign ld_addr_dc3hit_hi_lo = (lsu_addr_dc2[31:2] == end_addr_dc3[31:2]) & lsu_pkt_dc3.valid & lsu_pkt_dc3.store & lsu_busreq_dc2;
   assign ld_addr_dc3hit_hi_hi = (end_addr_dc2[31:2] == end_addr_dc3[31:2]) & lsu_pkt_dc3.valid & lsu_pkt_dc3.store & lsu_busreq_dc2;
   assign ld_addr_dc4hit_lo_lo = (lsu_addr_dc2[31:2] == lsu_addr_dc4[31:2]) & lsu_pkt_dc4.valid & lsu_pkt_dc4.store & lsu_busreq_dc2;
   assign ld_addr_dc4hit_lo_hi = (end_addr_dc2[31:2] == lsu_addr_dc4[31:2]) & lsu_pkt_dc4.valid & lsu_pkt_dc4.store & lsu_busreq_dc2;
   assign ld_addr_dc4hit_hi_lo = (lsu_addr_dc2[31:2] == end_addr_dc4[31:2]) & lsu_pkt_dc4.valid & lsu_pkt_dc4.store & lsu_busreq_dc2;
   assign ld_addr_dc4hit_hi_hi = (end_addr_dc2[31:2] == end_addr_dc4[31:2]) & lsu_pkt_dc4.valid & lsu_pkt_dc4.store & lsu_busreq_dc2;
   assign ld_addr_dc5hit_lo_lo = (lsu_addr_dc2[31:2] == lsu_addr_dc5[31:2]) & lsu_pkt_dc5.valid & lsu_pkt_dc5.store & lsu_busreq_dc2;
   assign ld_addr_dc5hit_lo_hi = (end_addr_dc2[31:2] == lsu_addr_dc5[31:2]) & lsu_pkt_dc5.valid & lsu_pkt_dc5.store & lsu_busreq_dc2;
   assign ld_addr_dc5hit_hi_lo = (lsu_addr_dc2[31:2] == end_addr_dc5[31:2]) & lsu_pkt_dc5.valid & lsu_pkt_dc5.store & lsu_busreq_dc2;
   assign ld_addr_dc5hit_hi_hi = (end_addr_dc2[31:2] == end_addr_dc5[31:2]) & lsu_pkt_dc5.valid & lsu_pkt_dc5.store & lsu_busreq_dc2;
   for (genvar i=0; i<4; i++) begin
      assign ld_byte_dc3hit_lo_lo[i] = ld_addr_dc3hit_lo_lo & ldst_byteen_lo_dc3[i] & ldst_byteen_lo_dc2[i];
      assign ld_byte_dc3hit_lo_hi[i] = ld_addr_dc3hit_lo_hi & ldst_byteen_lo_dc3[i] & ldst_byteen_hi_dc2[i];
      assign ld_byte_dc3hit_hi_lo[i] = ld_addr_dc3hit_hi_lo & ldst_byteen_hi_dc3[i] & ldst_byteen_lo_dc2[i];
      assign ld_byte_dc3hit_hi_hi[i] = ld_addr_dc3hit_hi_hi & ldst_byteen_hi_dc3[i] & ldst_byteen_hi_dc2[i];
      assign ld_byte_dc4hit_lo_lo[i] = ld_addr_dc4hit_lo_lo & ldst_byteen_lo_dc4[i] & ldst_byteen_lo_dc2[i];
      assign ld_byte_dc4hit_lo_hi[i] = ld_addr_dc4hit_lo_hi & ldst_byteen_lo_dc4[i] & ldst_byteen_hi_dc2[i];
      assign ld_byte_dc4hit_hi_lo[i] = ld_addr_dc4hit_hi_lo & ldst_byteen_hi_dc4[i] & ldst_byteen_lo_dc2[i];
      assign ld_byte_dc4hit_hi_hi[i] = ld_addr_dc4hit_hi_hi & ldst_byteen_hi_dc4[i] & ldst_byteen_hi_dc2[i];
      assign ld_byte_dc5hit_lo_lo[i] = ld_addr_dc5hit_lo_lo & ldst_byteen_lo_dc5[i] & ldst_byteen_lo_dc2[i];
      assign ld_byte_dc5hit_lo_hi[i] = ld_addr_dc5hit_lo_hi & ldst_byteen_lo_dc5[i] & ldst_byteen_hi_dc2[i];
      assign ld_byte_dc5hit_hi_lo[i] = ld_addr_dc5hit_hi_lo & ldst_byteen_hi_dc5[i] & ldst_byteen_lo_dc2[i];
      assign ld_byte_dc5hit_hi_hi[i] = ld_addr_dc5hit_hi_hi & ldst_byteen_hi_dc5[i] & ldst_byteen_hi_dc2[i];
      assign ld_byte_hit_lo[i] = ld_byte_dc3hit_lo_lo[i] | ld_byte_dc3hit_hi_lo[i] |
                                 ld_byte_dc4hit_lo_lo[i] | ld_byte_dc4hit_hi_lo[i] |
                                 ld_byte_dc5hit_lo_lo[i] | ld_byte_dc5hit_hi_lo[i] |
                                 ld_byte_hit_buf_lo[i];
      assign ld_byte_hit_hi[i] = ld_byte_dc3hit_lo_hi[i] | ld_byte_dc3hit_hi_hi[i] |
                                 ld_byte_dc4hit_lo_hi[i] | ld_byte_dc4hit_hi_hi[i] |
                                 ld_byte_dc5hit_lo_hi[i] | ld_byte_dc5hit_hi_hi[i] |
                                 ld_byte_hit_buf_hi[i];
      assign ld_byte_dc3hit_lo[i] = ld_byte_dc3hit_lo_lo[i] | ld_byte_dc3hit_hi_lo[i];
      assign ld_byte_dc4hit_lo[i] = ld_byte_dc4hit_lo_lo[i] | ld_byte_dc4hit_hi_lo[i];
      assign ld_byte_dc5hit_lo[i] = ld_byte_dc5hit_lo_lo[i] | ld_byte_dc5hit_hi_lo[i];
      assign ld_byte_dc3hit_hi[i] = ld_byte_dc3hit_lo_hi[i] | ld_byte_dc3hit_hi_hi[i];
      assign ld_byte_dc4hit_hi[i] = ld_byte_dc4hit_lo_hi[i] | ld_byte_dc4hit_hi_hi[i];
      assign ld_byte_dc5hit_hi[i] = ld_byte_dc5hit_lo_hi[i] | ld_byte_dc5hit_hi_hi[i];
      assign ld_fwddata_dc3pipe_lo[(8*i)+7:(8*i)] = ({8{ld_byte_dc3hit_lo_lo[i]}} & store_data_lo_dc3[(8*i)+7:(8*i)]) |
                                                    ({8{ld_byte_dc3hit_hi_lo[i]}} & store_data_hi_dc3[(8*i)+7:(8*i)]);
      assign ld_fwddata_dc4pipe_lo[(8*i)+7:(8*i)] = ({8{ld_byte_dc4hit_lo_lo[i]}} & store_data_lo_dc4[(8*i)+7:(8*i)]) |
                                                    ({8{ld_byte_dc4hit_hi_lo[i]}} & store_data_hi_dc4[(8*i)+7:(8*i)]);
      assign ld_fwddata_dc5pipe_lo[(8*i)+7:(8*i)] = ({8{ld_byte_dc5hit_lo_lo[i]}} & store_data_lo_dc5[(8*i)+7:(8*i)]) |
                                                    ({8{ld_byte_dc5hit_hi_lo[i]}} & store_data_hi_dc5[(8*i)+7:(8*i)]);
      assign ld_fwddata_dc3pipe_hi[(8*i)+7:(8*i)] = ({8{ld_byte_dc3hit_lo_hi[i]}} & store_data_lo_dc3[(8*i)+7:(8*i)]) |
                                                    ({8{ld_byte_dc3hit_hi_hi[i]}} & store_data_hi_dc3[(8*i)+7:(8*i)]);
      assign ld_fwddata_dc4pipe_hi[(8*i)+7:(8*i)] = ({8{ld_byte_dc4hit_lo_hi[i]}} & store_data_lo_dc4[(8*i)+7:(8*i)]) |
                                                    ({8{ld_byte_dc4hit_hi_hi[i]}} & store_data_hi_dc4[(8*i)+7:(8*i)]);
      assign ld_fwddata_dc5pipe_hi[(8*i)+7:(8*i)] = ({8{ld_byte_dc5hit_lo_hi[i]}} & store_data_lo_dc5[(8*i)+7:(8*i)]) |
                                                    ({8{ld_byte_dc5hit_hi_hi[i]}} & store_data_hi_dc5[(8*i)+7:(8*i)]);
      assign ld_fwddata_lo[(8*i)+7:(8*i)] = ld_byte_dc3hit_lo[i]    ? ld_fwddata_dc3pipe_lo[(8*i)+7:(8*i)] :
                                            ld_byte_dc4hit_lo[i]    ? ld_fwddata_dc4pipe_lo[(8*i)+7:(8*i)] :
                                            ld_byte_dc5hit_lo[i]    ? ld_fwddata_dc5pipe_lo[(8*i)+7:(8*i)] :
                                                                      ld_fwddata_buf_lo[(8*i)+7:(8*i)];
      assign ld_fwddata_hi[(8*i)+7:(8*i)] = ld_byte_dc3hit_hi[i]    ? ld_fwddata_dc3pipe_hi[(8*i)+7:(8*i)] :
                                            ld_byte_dc4hit_hi[i]    ? ld_fwddata_dc4pipe_hi[(8*i)+7:(8*i)] :
                                            ld_byte_dc5hit_hi[i]    ? ld_fwddata_dc5pipe_hi[(8*i)+7:(8*i)] :
                                                                      ld_fwddata_buf_hi[(8*i)+7:(8*i)];
   end
   always_comb begin
      ld_full_hit_lo_dc2 = 1'b1;
      ld_full_hit_hi_dc2 = 1'b1;
      for (int i=0; i<4; i++) begin
         ld_full_hit_lo_dc2 &= (ld_byte_hit_lo[i] | ~ldst_byteen_lo_dc2[i]);
         ld_full_hit_hi_dc2 &= (ld_byte_hit_hi[i] | ~ldst_byteen_hi_dc2[i]);
      end
   end
   assign ld_hit_dc2 = (|ld_byte_hit_lo[3:0]) | (|ld_byte_hit_hi[3:0]);
   assign ld_full_hit_dc2 = ld_full_hit_lo_dc2 & ld_full_hit_hi_dc2 & lsu_busreq_dc2 & lsu_pkt_dc2.load & ~is_sideeffects_dc2;
   assign {ld_fwddata_dc2_nc[63:32], ld_fwddata_dc2[31:0]} = {ld_fwddata_hi[31:0], ld_fwddata_lo[31:0]} >> (8*lsu_addr_dc2[1:0]);
   assign bus_read_data_dc3[31:0]                           = ld_full_hit_dc3 ? ld_fwddata_dc3[31:0] : ld_bus_data_dc3[31:0];
   rvdff #(.WIDTH(1)) clken_ff (.din(lsu_bus_clk_en), .dout(lsu_bus_clk_en_q), .clk(free_clk), .*);
   rvdff_fpga #(.WIDTH(1)) ldst_dual_dc2ff    (.din(ldst_dual_dc1),        .dout(ldst_dual_dc2),        .clk(lsu_freeze_c1_dc2_clk), .clken(lsu_freeze_c1_dc2_clken), .rawclk(clk), .*);
   rvdff_fpga #(.WIDTH(1)) lsu_full_hit_dc3ff (.din(ld_full_hit_dc2),      .dout(ld_full_hit_dc3),      .clk(lsu_freeze_c2_dc3_clk), .clken(lsu_freeze_c2_dc3_clken), .rawclk(clk), .*);
   rvdff_fpga #(.WIDTH(1)) ldst_dual_dc3ff    (.din(ldst_dual_dc2),        .dout(ldst_dual_dc3),        .clk(lsu_freeze_c1_dc3_clk), .clken(lsu_freeze_c1_dc3_clken), .rawclk(clk), .*);
   rvdff_fpga #(4)         lsu_byten_dc3ff    (.din(ldst_byteen_dc2[3:0]), .dout(ldst_byteen_dc3[3:0]), .clk(lsu_freeze_c1_dc3_clk), .clken(lsu_freeze_c1_dc3_clken), .rawclk(clk), .*);
   rvdff #(.WIDTH(32)) lsu_fwddata_dc3ff (.din(ld_fwddata_dc2[31:0]), .dout(ld_fwddata_dc3[31:0]), .clk(lsu_c1_dc3_clk), .*);
   rvdff #(.WIDTH(1)) ldst_dual_dc4ff (.din(ldst_dual_dc3), .dout(ldst_dual_dc4), .clk(lsu_c1_dc4_clk), .*);
   rvdff #(.WIDTH(1)) ldst_dual_dc5ff (.din(ldst_dual_dc4), .dout(ldst_dual_dc5), .clk(lsu_c1_dc5_clk), .*);
   rvdff #(.WIDTH(1)) is_sideeffects_dc4ff (.din(is_sideeffects_dc3), .dout(is_sideeffects_dc4), .clk(lsu_c1_dc4_clk), .*);
   rvdff #(.WIDTH(1)) is_sideeffects_dc5ff (.din(is_sideeffects_dc4), .dout(is_sideeffects_dc5), .clk(lsu_c1_dc5_clk), .*);
   rvdff #(4) lsu_byten_dc4ff (.*, .din(ldst_byteen_dc3[3:0]), .dout(ldst_byteen_dc4[3:0]), .clk(lsu_c1_dc4_clk));
   rvdff #(4) lsu_byten_dc5ff (.*, .din(ldst_byteen_dc4[3:0]), .dout(ldst_byteen_dc5[3:0]), .clk(lsu_c1_dc5_clk));
endmodule  
module lsu_ecc
   import swerv_types::*;
(
   input logic        lsu_c2_dc4_clk,                       
   input logic        lsu_c1_dc4_clk,
   input logic        lsu_c1_dc5_clk,
   input logic        clk,
   input logic        rst_l,
   input lsu_pkt_t    lsu_pkt_dc3,                         
   input logic        lsu_dccm_rden_dc3,                   
   input logic        addr_in_dccm_dc3,                    
   input logic [16-1:0]       lsu_addr_dc3,     
   input logic [16-1:0]       end_addr_dc3,     
   input logic [63:0]                    store_data_dc3,   
   input logic [32-1:0] stbuf_data_any,
   input logic [32-1:0] stbuf_fwddata_hi_dc3,   
   input logic [32-1:0] stbuf_fwddata_lo_dc3,   
   input logic [4-1:0] stbuf_fwdbyteen_hi_dc3, 
   input logic [4-1:0] stbuf_fwdbyteen_lo_dc3, 
   input logic [32-1:0] dccm_data_hi_dc3,      
   input logic [32-1:0] dccm_data_lo_dc3,      
   input logic [7-1:0]  dccm_data_ecc_hi_dc3,  
   input logic [7-1:0]  dccm_data_ecc_lo_dc3,  
   input logic                           dec_tlu_core_ecc_disable,   
   output logic [32-1:0] store_ecc_datafn_hi_dc3,   
   output logic [32-1:0] store_ecc_datafn_lo_dc3,
   output logic [7-1:0] stbuf_ecc_any,
   output logic        single_ecc_error_hi_dc3,                    
   output logic        single_ecc_error_lo_dc3,                    
   output logic        lsu_single_ecc_error_dc3,                   
   output logic        lsu_double_ecc_error_dc3,                   
   input logic         scan_mode
 );
localparam TOTAL_INT        = 9;
localparam DCCM_BITS        = 16;
localparam DCCM_BANK_BITS   = 3;
localparam DCCM_NUM_BANKS   = 8;
localparam DCCM_DATA_WIDTH  = 32;
localparam DCCM_FDATA_WIDTH = 39;
localparam DCCM_BYTE_WIDTH  = 4;
localparam DCCM_ECC_WIDTH   = 7;
localparam LSU_RDBUF_DEPTH  = 8;
localparam DMA_BUF_DEPTH    = 4;
localparam LSU_STBUF_DEPTH  = 8;
localparam LSU_SB_BITS      = 16;
localparam DEC_INSTBUF_DEPTH = 4;
localparam ICCM_SIZE         = 512;
localparam ICCM_BITS         = 19;
localparam ICCM_NUM_BANKS    = 8;
localparam ICCM_BANK_BITS    = 3;
localparam ICCM_INDEX_BITS   = 14;
localparam ICCM_BANK_HI      = 4 + (3/4);
localparam ICACHE_TAG_HIGH  = 12;
localparam ICACHE_TAG_LOW   = 6;
localparam ICACHE_IC_DEPTH  = 8;
localparam ICACHE_TAG_DEPTH = 64;
localparam LSU_BUS_TAG     = 4;
localparam DMA_BUS_TAG     = 1;
localparam SB_BUS_TAG      = 1;
localparam IFU_BUS_TAG     = 3;
      localparam DCCM_ENABLE = 1'b1;
   logic [DCCM_DATA_WIDTH-1:0] sec_data_hi_dc3;
   logic [DCCM_DATA_WIDTH-1:0] sec_data_lo_dc3;
   logic        double_ecc_error_hi_dc3, double_ecc_error_lo_dc3;
   logic        ldst_dual_dc3;
   logic        is_ldst_dc3;
   logic        is_ldst_hi_dc3, is_ldst_lo_dc3;
   logic [7:0]  ldst_byteen_dc3;
   logic [7:0]  store_byteen_dc3;
   logic [7:0]  store_byteen_ext_dc3;
   logic [DCCM_BYTE_WIDTH-1:0]  store_byteen_hi_dc3, store_byteen_lo_dc3;
   logic [163:0] store_data_ext_dc3;
   logic [DCCM_DATA_WIDTH-1:0]  store_data_hi_dc3, store_data_lo_dc3;
   logic [6:0]                  ecc_out_hi_nc, ecc_out_lo_nc;
   assign ldst_dual_dc3 = (lsu_addr_dc3[2] != end_addr_dc3[2]);
   assign is_ldst_dc3 = lsu_pkt_dc3.valid & (lsu_pkt_dc3.load | lsu_pkt_dc3.store) & addr_in_dccm_dc3 & lsu_dccm_rden_dc3;
   assign is_ldst_lo_dc3 = is_ldst_dc3 & ~dec_tlu_core_ecc_disable;
   assign is_ldst_hi_dc3 = is_ldst_dc3 & ldst_dual_dc3 & ~dec_tlu_core_ecc_disable;
   assign ldst_byteen_dc3[7:0] = ({8{lsu_pkt_dc3.by}}   & 8'b0000_0001) |
                                 ({8{lsu_pkt_dc3.half}} & 8'b0000_0011) |
                                 ({8{lsu_pkt_dc3.word}} & 8'b0000_1111) |
                                 ({8{lsu_pkt_dc3.dword}} & 8'b1111_1111);
   assign store_byteen_dc3[7:0] = ldst_byteen_dc3[7:0] & {8{lsu_pkt_dc3.store}};
   assign store_byteen_ext_dc3[7:0] = store_byteen_dc3[7:0] << lsu_addr_dc3[1:0];
   assign store_byteen_hi_dc3[DCCM_BYTE_WIDTH-1:0] = store_byteen_ext_dc3[7:4];
   assign store_byteen_lo_dc3[DCCM_BYTE_WIDTH-1:0] = store_byteen_ext_dc3[3:0];
   assign store_data_ext_dc3[63:0] = store_data_dc3[63:0] << {lsu_addr_dc3[1:0], 3'b000};
   assign store_data_hi_dc3[DCCM_DATA_WIDTH-1:0]  = store_data_ext_dc3[63:32];
   assign store_data_lo_dc3[DCCM_DATA_WIDTH-1:0]  = store_data_ext_dc3[31:0];
   for (genvar i=0; i<DCCM_BYTE_WIDTH; i++) begin
      assign store_ecc_datafn_hi_dc3[(8*i)+7:(8*i)] = store_byteen_hi_dc3[i] ? store_data_hi_dc3[(8*i)+7:(8*i)] :
                                                                               (stbuf_fwdbyteen_hi_dc3[i] ? stbuf_fwddata_hi_dc3[(8*i)+7:(8*i)] : sec_data_hi_dc3[(8*i)+7:(8*i)]);
      assign store_ecc_datafn_lo_dc3[(8*i)+7:(8*i)] = store_byteen_lo_dc3[i] ? store_data_lo_dc3[(8*i)+7:(8*i)] :
                                                                               (stbuf_fwdbyteen_lo_dc3[i] ? stbuf_fwddata_lo_dc3[(8*i)+7:(8*i)] : sec_data_lo_dc3[(8*i)+7:(8*i)]);
   end
   if (DCCM_ENABLE == 1) begin: Gen_dccm_enable
      rvecc_decode lsu_ecc_decode_hi (
         .en(is_ldst_hi_dc3),
         .sed_ded (1'b0),     
         .din(dccm_data_hi_dc3[DCCM_DATA_WIDTH-1:0]),
         .ecc_in(dccm_data_ecc_hi_dc3[DCCM_ECC_WIDTH-1:0]),
         .dout(sec_data_hi_dc3[DCCM_DATA_WIDTH-1:0]),
         .ecc_out (ecc_out_hi_nc[6:0]),
         .single_ecc_error(single_ecc_error_hi_dc3),
         .double_ecc_error(double_ecc_error_hi_dc3),
         .*
      );
      rvecc_decode lsu_ecc_decode_lo (
         .en(is_ldst_lo_dc3),
         .sed_ded (1'b0),     
         .din(dccm_data_lo_dc3[DCCM_DATA_WIDTH-1:0] ),
         .ecc_in(dccm_data_ecc_lo_dc3[DCCM_ECC_WIDTH-1:0]),
         .dout(sec_data_lo_dc3[DCCM_DATA_WIDTH-1:0]),
         .ecc_out (ecc_out_lo_nc[6:0]),
         .single_ecc_error(single_ecc_error_lo_dc3),
         .double_ecc_error(double_ecc_error_lo_dc3),
         .*
      );
      rvecc_encode lsu_ecc_encode (
         .din(stbuf_data_any[DCCM_DATA_WIDTH-1:0]),
         .ecc_out(stbuf_ecc_any[DCCM_ECC_WIDTH-1:0]),
         .*
      );
   end else begin: Gen_dccm_disable  
      assign sec_data_hi_dc3[DCCM_DATA_WIDTH-1:0] = '0;
      assign sec_data_lo_dc3[DCCM_DATA_WIDTH-1:0] = '0;
      assign single_ecc_error_hi_dc3 = '0;
      assign double_ecc_error_hi_dc3 = '0;
      assign single_ecc_error_lo_dc3 = '0;
      assign double_ecc_error_lo_dc3 = '0;
      assign stbuf_ecc_any[DCCM_ECC_WIDTH-1:0] = '0;
   end
   assign lsu_single_ecc_error_dc3 = single_ecc_error_hi_dc3 | single_ecc_error_lo_dc3;
   assign lsu_double_ecc_error_dc3 = double_ecc_error_hi_dc3 | double_ecc_error_lo_dc3;
endmodule  
module lsu_dccm_mem
   import swerv_types::*;
(
   input logic         clk,                                               
   input logic         free_clk,                                          
   input logic         rst_l,
   input logic         lsu_freeze_dc3,                                    
   input logic         clk_override,                                      
   input logic         dccm_wren,                                         
   input logic         dccm_rden,                                         
   input logic [16-1:0]  dccm_wr_addr,                         
   input logic [16-1:0]  dccm_rd_addr_lo,                      
   input logic [16-1:0]  dccm_rd_addr_hi,                      
   input logic [39-1:0]  dccm_wr_data,                  
   output logic [39-1:0] dccm_rd_data_lo,               
   output logic [39-1:0] dccm_rd_data_hi,               
   input  logic         scan_mode
);
localparam TOTAL_INT        = 9;
localparam DCCM_BITS        = 16;
localparam DCCM_BANK_BITS   = 3;
localparam DCCM_NUM_BANKS   = 8;
localparam DCCM_DATA_WIDTH  = 32;
localparam DCCM_FDATA_WIDTH = 39;
localparam DCCM_BYTE_WIDTH  = 4;
localparam DCCM_ECC_WIDTH   = 7;
localparam LSU_RDBUF_DEPTH  = 8;
localparam DMA_BUF_DEPTH    = 4;
localparam LSU_STBUF_DEPTH  = 8;
localparam LSU_SB_BITS      = 16;
localparam DEC_INSTBUF_DEPTH = 4;
localparam ICCM_SIZE         = 512;
localparam ICCM_BITS         = 19;
localparam ICCM_NUM_BANKS    = 8;
localparam ICCM_BANK_BITS    = 3;
localparam ICCM_INDEX_BITS   = 14;
localparam ICCM_BANK_HI      = 4 + (3/4);
localparam ICACHE_TAG_HIGH  = 12;
localparam ICACHE_TAG_LOW   = 6;
localparam ICACHE_IC_DEPTH  = 8;
localparam ICACHE_TAG_DEPTH = 64;
localparam LSU_BUS_TAG     = 4;
localparam DMA_BUS_TAG     = 1;
localparam SB_BUS_TAG      = 1;
localparam IFU_BUS_TAG     = 3;
   localparam DCCM_WIDTH_BITS = $clog2(DCCM_BYTE_WIDTH);
   localparam DCCM_INDEX_BITS = (DCCM_BITS - DCCM_BANK_BITS - DCCM_WIDTH_BITS);
   logic [DCCM_NUM_BANKS-1:0]         wren_bank;
   logic [DCCM_NUM_BANKS-1:0]         rden_bank;
   logic [DCCM_NUM_BANKS-1:0] [DCCM_BITS-1:(DCCM_BANK_BITS+2)]   addr_bank;
   logic [DCCM_BITS-1:(DCCM_BANK_BITS+DCCM_WIDTH_BITS)] rd_addr_even, rd_addr_odd;
   logic                              rd_unaligned;
   logic [DCCM_NUM_BANKS-1:0] [DCCM_FDATA_WIDTH-1:0]   dccm_bank_dout;
   logic [DCCM_FDATA_WIDTH-1:0]       wrdata;
   logic [DCCM_NUM_BANKS-1:0]         wren_bank_q;
   logic [DCCM_NUM_BANKS-1:0]         rden_bank_q;
   logic [DCCM_NUM_BANKS-1:0][DCCM_BITS-1:(DCCM_BANK_BITS+2)]    addr_bank_q;
   logic [DCCM_FDATA_WIDTH-1:0]       dccm_wr_data_q;
   logic [(DCCM_WIDTH_BITS+DCCM_BANK_BITS-1):DCCM_WIDTH_BITS]      dccm_rd_addr_lo_q;
   logic [(DCCM_WIDTH_BITS+DCCM_BANK_BITS-1):DCCM_WIDTH_BITS]      dccm_rd_addr_hi_q;
   logic [DCCM_NUM_BANKS-1:0]            dccm_clk;
   logic [DCCM_NUM_BANKS-1:0]            dccm_clken;
   logic [DCCM_FDATA_WIDTH-1:0]          dccm_rd_data_lo_q, dccm_rd_data_hi_q;
   logic                                 lsu_freeze_dc3_q;
   assign rd_unaligned = (dccm_rd_addr_lo[DCCM_WIDTH_BITS+:DCCM_BANK_BITS] != dccm_rd_addr_hi[DCCM_WIDTH_BITS+:DCCM_BANK_BITS]);
   assign dccm_rd_data_lo[DCCM_FDATA_WIDTH-1:0]  = lsu_freeze_dc3_q ? dccm_rd_data_lo_q[DCCM_FDATA_WIDTH-1:0] : dccm_bank_dout[dccm_rd_addr_lo_q[DCCM_WIDTH_BITS+:DCCM_BANK_BITS]][DCCM_FDATA_WIDTH-1:0];
   assign dccm_rd_data_hi[DCCM_FDATA_WIDTH-1:0]  = lsu_freeze_dc3_q ? dccm_rd_data_hi_q[DCCM_FDATA_WIDTH-1:0] : dccm_bank_dout[dccm_rd_addr_hi_q[DCCM_WIDTH_BITS+:DCCM_BANK_BITS]][DCCM_FDATA_WIDTH-1:0];
   rvdff  #(1) lsu_freeze_dc3ff(.din(lsu_freeze_dc3), .dout(lsu_freeze_dc3_q), .clk(free_clk), .*);
   rvdffe #(DCCM_FDATA_WIDTH) dccm_rd_data_loff(.din(dccm_rd_data_lo), .dout(dccm_rd_data_lo_q), .en(~lsu_freeze_dc3_q), .*);
   rvdffe #(DCCM_FDATA_WIDTH) dccm_rd_data_hiff(.din(dccm_rd_data_hi), .dout(dccm_rd_data_hi_q), .en(~lsu_freeze_dc3_q), .*);
   for (genvar i=0; i<DCCM_NUM_BANKS; i++) begin: mem_bank
      assign  wren_bank[i]        = dccm_wren & (dccm_wr_addr[2+:DCCM_BANK_BITS] == i);
      assign  rden_bank[i]        = dccm_rden & ((dccm_rd_addr_hi[2+:DCCM_BANK_BITS] == i) | (dccm_rd_addr_lo[2+:DCCM_BANK_BITS] == i));
      assign  addr_bank[i][(DCCM_BANK_BITS+DCCM_WIDTH_BITS)+:DCCM_INDEX_BITS] = wren_bank[i] ? dccm_wr_addr[(DCCM_BANK_BITS+DCCM_WIDTH_BITS)+:DCCM_INDEX_BITS] :
                                                                                (((dccm_rd_addr_hi[2+:DCCM_BANK_BITS] == i) & rd_unaligned) ?
                                                                                                    dccm_rd_addr_hi[(DCCM_BANK_BITS+DCCM_WIDTH_BITS)+:DCCM_INDEX_BITS] :
                                                                                                    dccm_rd_addr_lo[(DCCM_BANK_BITS+DCCM_WIDTH_BITS)+:DCCM_INDEX_BITS]);
      assign  dccm_clken[i] = (wren_bank[i] | rden_bank[i] | clk_override) & ~lsu_freeze_dc3;
      rvoclkhdr lsu_dccm_cgc (.en(dccm_clken[i]), .l1clk(dccm_clk[i]), .*);
      ram_2048x39  dccm_bank (
                                     .CLK(dccm_clk[i]),
                                     .WE(wren_bank[i]),
                                     .ADR(addr_bank[i]),
                                     .D(dccm_wr_data[DCCM_FDATA_WIDTH-1:0]),
                                     .Q(dccm_bank_dout[i][DCCM_FDATA_WIDTH-1:0])
                                    );
   end : mem_bank
   rvdffs  #(DCCM_BANK_BITS) rd_addr_lo_ff (.*, .din(dccm_rd_addr_lo[DCCM_WIDTH_BITS+:DCCM_BANK_BITS]), .dout(dccm_rd_addr_lo_q[DCCM_WIDTH_BITS+:DCCM_BANK_BITS]), .en(~lsu_freeze_dc3), .clk(free_clk));
   rvdffs  #(DCCM_BANK_BITS) rd_addr_hi_ff (.*, .din(dccm_rd_addr_hi[DCCM_WIDTH_BITS+:DCCM_BANK_BITS]), .dout(dccm_rd_addr_hi_q[DCCM_WIDTH_BITS+:DCCM_BANK_BITS]), .en(~lsu_freeze_dc3), .clk(free_clk));
endmodule  
module lsu_dccm_ctl
   import swerv_types::*;
(
   input logic                             lsu_freeze_c2_dc2_clk,      
   input logic                             lsu_freeze_c2_dc3_clk,
   input logic                             lsu_freeze_c2_dc2_clken,      
   input logic                             lsu_freeze_c2_dc3_clken,
   input logic                             lsu_dccm_c1_dc3_clk,
   input logic                             lsu_dccm_c1_dc3_clken,
   input logic                             lsu_pic_c1_dc3_clken,
   input logic                             rst_l,
   input logic                             clk,
   input logic                             lsu_freeze_dc3,             
   input                                   lsu_pkt_t lsu_pkt_dc3,      
   input                                   lsu_pkt_t lsu_pkt_dc1,
   input logic                             addr_in_dccm_dc1,           
   input logic                             addr_in_pic_dc1,            
   input logic                             addr_in_pic_dc3,            
   input logic [31:0]                      lsu_addr_dc1,               
   input logic [16-1:0]         end_addr_dc1,               
   input logic [16-1:0]         lsu_addr_dc3,               
   input logic                             stbuf_reqvld_any,           
   input logic                             stbuf_addr_in_pic_any,      
   input logic [16-1:0]       stbuf_addr_any,             
   input logic [32-1:0]   stbuf_data_any,             
   input logic [7-1:0]    stbuf_ecc_any,              
   input logic [32-1:0]   stbuf_fwddata_hi_dc3,       
   input logic [32-1:0]   stbuf_fwddata_lo_dc3,       
   input logic [4-1:0]   stbuf_fwdbyteen_hi_dc3,     
   input logic [4-1:0]   stbuf_fwdbyteen_lo_dc3,     
   input logic                             lsu_double_ecc_error_dc3,   
   input logic [32-1:0]   store_ecc_datafn_hi_dc3,    
   input logic [32-1:0]   store_ecc_datafn_lo_dc3,    
   output logic [32-1:0]  dccm_data_hi_dc3,           
   output logic [32-1:0]  dccm_data_lo_dc3,           
   output logic [7-1:0]   dccm_data_ecc_hi_dc3,       
   output logic [7-1:0]   dccm_data_ecc_lo_dc3,
   output logic [32-1:0]  lsu_ld_data_dc3,            
   output logic [32-1:0]  lsu_ld_data_corr_dc3,       
   output logic [31:0]                     picm_mask_data_dc3,         
   output logic                            lsu_stbuf_commit_any,       
   output logic                            lsu_dccm_rden_dc3,          
   output logic                            dccm_dma_rvalid,            
   output logic                            dccm_dma_ecc_error,         
   output logic [63:0]                     dccm_dma_rdata,             
   output logic                            dccm_wren,                 
   output logic                            dccm_rden,                 
   output logic [16-1:0]        dccm_wr_addr,              
   output logic [16-1:0]        dccm_rd_addr_lo,           
   output logic [16-1:0]        dccm_rd_addr_hi,           
   output logic [39-1:0] dccm_wr_data,              
   input logic [39-1:0]  dccm_rd_data_lo,           
   input logic [39-1:0]  dccm_rd_data_hi,           
   output logic                            picm_wren,           
   output logic                            picm_rden,           
   output logic                            picm_mken,           
   output logic [31:0]                     picm_addr,           
   output logic [31:0]                     picm_wr_data,        
   input logic [31:0]                      picm_rd_data,        
   input logic                             scan_mode            
);
localparam TOTAL_INT        = 9;
localparam DCCM_BITS        = 16;
localparam DCCM_BANK_BITS   = 3;
localparam DCCM_NUM_BANKS   = 8;
localparam DCCM_DATA_WIDTH  = 32;
localparam DCCM_FDATA_WIDTH = 39;
localparam DCCM_BYTE_WIDTH  = 4;
localparam DCCM_ECC_WIDTH   = 7;
localparam LSU_RDBUF_DEPTH  = 8;
localparam DMA_BUF_DEPTH    = 4;
localparam LSU_STBUF_DEPTH  = 8;
localparam LSU_SB_BITS      = 16;
localparam DEC_INSTBUF_DEPTH = 4;
localparam ICCM_SIZE         = 512;
localparam ICCM_BITS         = 19;
localparam ICCM_NUM_BANKS    = 8;
localparam ICCM_BANK_BITS    = 3;
localparam ICCM_INDEX_BITS   = 14;
localparam ICCM_BANK_HI      = 4 + (3/4);
localparam ICACHE_TAG_HIGH  = 12;
localparam ICACHE_TAG_LOW   = 6;
localparam ICACHE_IC_DEPTH  = 8;
localparam ICACHE_TAG_DEPTH = 64;
localparam LSU_BUS_TAG     = 4;
localparam DMA_BUS_TAG     = 1;
localparam SB_BUS_TAG      = 1;
localparam IFU_BUS_TAG     = 3;
      localparam DCCM_ENABLE = 1'b1;
   localparam DCCM_WIDTH_BITS = $clog2(DCCM_BYTE_WIDTH);
   localparam PIC_BITS        =15;
   logic                        lsu_dccm_rden_dc1, lsu_dccm_rden_dc2;
   logic [DCCM_DATA_WIDTH-1:0]  dccm_data_hi_dc2, dccm_data_lo_dc2;
   logic [DCCM_ECC_WIDTH-1:0]   dccm_data_ecc_hi_dc2, dccm_data_ecc_lo_dc2;
   logic [63:0]  dccm_dout_dc3, dccm_corr_dout_dc3;
   logic [63:0]  stbuf_fwddata_dc3;
   logic [7:0]   stbuf_fwdbyteen_dc3;
   logic [63:0]  lsu_rdata_dc3, lsu_rdata_corr_dc3;
   logic [63:0]  picm_rd_data_dc3;
   logic [31:0]  picm_rd_data_lo_dc3;
   logic [63:32] lsu_ld_data_dc3_nc, lsu_ld_data_corr_dc3_nc;
   assign dccm_dma_rvalid      = lsu_pkt_dc3.valid & lsu_pkt_dc3.load & lsu_pkt_dc3.dma;
   assign dccm_dma_ecc_error   = lsu_double_ecc_error_dc3;
   assign dccm_dma_rdata[63:0] = lsu_pkt_dc3.dword ? lsu_rdata_corr_dc3[63:0] : {2{lsu_rdata_corr_dc3[31:0]}};  
   assign {lsu_ld_data_dc3_nc[63:32],      lsu_ld_data_dc3[31:0]}      = lsu_rdata_dc3[63:0] >> 8*lsu_addr_dc3[1:0];
   assign {lsu_ld_data_corr_dc3_nc[63:32], lsu_ld_data_corr_dc3[31:0]} = lsu_rdata_corr_dc3[63:0] >> 8*lsu_addr_dc3[1:0];
   assign dccm_dout_dc3[63:0] = {dccm_data_hi_dc3[DCCM_DATA_WIDTH-1:0], dccm_data_lo_dc3[DCCM_DATA_WIDTH-1:0]};
   assign dccm_corr_dout_dc3[63:0] = {store_ecc_datafn_hi_dc3[DCCM_DATA_WIDTH-1:0], store_ecc_datafn_lo_dc3[DCCM_DATA_WIDTH-1:0]};
   assign stbuf_fwddata_dc3[63:0] = {stbuf_fwddata_hi_dc3[DCCM_DATA_WIDTH-1:0], stbuf_fwddata_lo_dc3[DCCM_DATA_WIDTH-1:0]};
   assign stbuf_fwdbyteen_dc3[7:0] = {stbuf_fwdbyteen_hi_dc3[DCCM_BYTE_WIDTH-1:0], stbuf_fwdbyteen_lo_dc3[DCCM_BYTE_WIDTH-1:0]};
   for (genvar i=0; i<8; i++) begin: GenLoop
      assign lsu_rdata_dc3[(8*i)+7:8*i]      = stbuf_fwdbyteen_dc3[i] ? stbuf_fwddata_dc3[(8*i)+7:8*i] :
                                                                        (addr_in_pic_dc3 ? picm_rd_data_dc3[(8*i)+7:8*i] :  dccm_dout_dc3[(8*i)+7:8*i]);
      assign lsu_rdata_corr_dc3[(8*i)+7:8*i] = stbuf_fwdbyteen_dc3[i] ? stbuf_fwddata_dc3[(8*i)+7:8*i] :
                                                                        (addr_in_pic_dc3 ? picm_rd_data_dc3[(8*i)+7:8*i] :  dccm_corr_dout_dc3[(8*i)+7:8*i]);
   end
   assign lsu_stbuf_commit_any = stbuf_reqvld_any & ~lsu_freeze_dc3 & (
                                 (~(lsu_dccm_rden_dc1 | picm_rden | picm_mken)) |
                                 ((picm_rden | picm_mken) & ~stbuf_addr_in_pic_any) |
                                 (lsu_dccm_rden_dc1 & (stbuf_addr_in_pic_any | (~((stbuf_addr_any[DCCM_WIDTH_BITS+:DCCM_BANK_BITS] == lsu_addr_dc1[DCCM_WIDTH_BITS+:DCCM_BANK_BITS]) |
                                                                                  (stbuf_addr_any[DCCM_WIDTH_BITS+:DCCM_BANK_BITS] == end_addr_dc1[DCCM_WIDTH_BITS+:DCCM_BANK_BITS]))))));
   assign lsu_dccm_rden_dc1 = lsu_pkt_dc1.valid & (lsu_pkt_dc1.load | (lsu_pkt_dc1.store & (~(lsu_pkt_dc1.word | lsu_pkt_dc1.dword) | (lsu_addr_dc1[1:0] != 2'b0)))) & addr_in_dccm_dc1;
   assign dccm_wren = lsu_stbuf_commit_any & ~stbuf_addr_in_pic_any;
   assign dccm_rden = lsu_dccm_rden_dc1 & addr_in_dccm_dc1;
   assign dccm_wr_addr[DCCM_BITS-1:0] = stbuf_addr_any[DCCM_BITS-1:0];
   assign dccm_rd_addr_lo[DCCM_BITS-1:0] = lsu_addr_dc1[DCCM_BITS-1:0];
   assign dccm_rd_addr_hi[DCCM_BITS-1:0] = end_addr_dc1[DCCM_BITS-1:0];
   assign dccm_wr_data[DCCM_FDATA_WIDTH-1:0] = {stbuf_ecc_any[DCCM_ECC_WIDTH-1:0],stbuf_data_any[DCCM_DATA_WIDTH-1:0]};
   assign dccm_data_lo_dc2[DCCM_DATA_WIDTH-1:0] = dccm_rd_data_lo[DCCM_DATA_WIDTH-1:0];
   assign dccm_data_hi_dc2[DCCM_DATA_WIDTH-1:0] = dccm_rd_data_hi[DCCM_DATA_WIDTH-1:0];
   assign dccm_data_ecc_lo_dc2[DCCM_ECC_WIDTH-1:0] = dccm_rd_data_lo[DCCM_FDATA_WIDTH-1:DCCM_DATA_WIDTH];
   assign dccm_data_ecc_hi_dc2[DCCM_ECC_WIDTH-1:0] = dccm_rd_data_hi[DCCM_FDATA_WIDTH-1:DCCM_DATA_WIDTH];
   assign picm_wren = lsu_stbuf_commit_any & stbuf_addr_in_pic_any;
   assign picm_rden = lsu_pkt_dc1.valid & lsu_pkt_dc1.load & addr_in_pic_dc1;
   assign picm_mken = lsu_pkt_dc1.valid & lsu_pkt_dc1.store & addr_in_pic_dc1;   
   assign picm_addr[31:0] = (picm_rden | picm_mken) ? (32'hf00c0000 | {17'b0,lsu_addr_dc1[14:0]}) : (32'hf00c0000 | {{32-PIC_BITS{1'b0}},stbuf_addr_any[15-1:0]});
   assign picm_wr_data[31:0] = stbuf_data_any[31:0];
   assign picm_mask_data_dc3[31:0] = picm_rd_data_lo_dc3[31:0];
   assign picm_rd_data_dc3[63:0] = {picm_rd_data_lo_dc3[31:0], picm_rd_data_lo_dc3[31:0]} ;
   rvdffe #(32) picm_data_ff (.*, .din(picm_rd_data[31:0]), .dout(picm_rd_data_lo_dc3[31:0]), .en(lsu_pic_c1_dc3_clken));
   if (DCCM_ENABLE == 1) begin: Gen_dccm_enable
      rvdff_fpga #(1) dccm_rden_dc2ff (.*, .din(lsu_dccm_rden_dc1), .dout(lsu_dccm_rden_dc2), .clk(lsu_freeze_c2_dc2_clk), .clken(lsu_freeze_c2_dc2_clken), .rawclk(clk));
      rvdff_fpga #(1) dccm_rden_dc3ff (.*, .din(lsu_dccm_rden_dc2), .dout(lsu_dccm_rden_dc3), .clk(lsu_freeze_c2_dc3_clk), .clken(lsu_freeze_c2_dc3_clken), .rawclk(clk));
      rvdff_fpga #(DCCM_DATA_WIDTH) dccm_data_hi_ff    (.*, .din(dccm_data_hi_dc2[DCCM_DATA_WIDTH-1:0]),    .dout(dccm_data_hi_dc3[DCCM_DATA_WIDTH-1:0]),    .clk(lsu_dccm_c1_dc3_clk), .clken(lsu_dccm_c1_dc3_clken), .rawclk(clk));
      rvdff_fpga #(DCCM_DATA_WIDTH) dccm_data_lo_ff    (.*, .din(dccm_data_lo_dc2[DCCM_DATA_WIDTH-1:0]),    .dout(dccm_data_lo_dc3[DCCM_DATA_WIDTH-1:0]),    .clk(lsu_dccm_c1_dc3_clk), .clken(lsu_dccm_c1_dc3_clken), .rawclk(clk));
      rvdff_fpga #(DCCM_ECC_WIDTH) dccm_data_ecc_hi_ff (.*, .din(dccm_data_ecc_hi_dc2[DCCM_ECC_WIDTH-1:0]), .dout(dccm_data_ecc_hi_dc3[DCCM_ECC_WIDTH-1:0]), .clk(lsu_dccm_c1_dc3_clk), .clken(lsu_dccm_c1_dc3_clken), .rawclk(clk));
      rvdff_fpga #(DCCM_ECC_WIDTH) dccm_data_ecc_lo_ff (.*, .din(dccm_data_ecc_lo_dc2[DCCM_ECC_WIDTH-1:0]), .dout(dccm_data_ecc_lo_dc3[DCCM_ECC_WIDTH-1:0]), .clk(lsu_dccm_c1_dc3_clk), .clken(lsu_dccm_c1_dc3_clken), .rawclk(clk));
   end else begin: Gen_dccm_disable
      assign lsu_dccm_rden_dc2 = '0;
      assign lsu_dccm_rden_dc3 = '0;
      assign dccm_data_hi_dc3[DCCM_DATA_WIDTH-1:0] = '0;
      assign dccm_data_lo_dc3[DCCM_DATA_WIDTH-1:0] = '0;
      assign dccm_data_ecc_hi_dc3[DCCM_ECC_WIDTH-1:0] = '0;
      assign dccm_data_ecc_lo_dc3[DCCM_ECC_WIDTH-1:0] = '0;
   end
endmodule
module lsu_trigger
   import swerv_types::*;
(
   input logic         clk,                           
   input logic         lsu_free_c2_clk,               
   input logic         rst_l,                         
   input trigger_pkt_t [3:0] trigger_pkt_any,         
   input lsu_pkt_t     lsu_pkt_dc3,                   
   input logic [31:0]  lsu_addr_dc3,                  
   input logic [31:0]  lsu_result_dc3,                
   input logic [31:0]  store_data_dc3,                
   output logic [3:0] lsu_trigger_match_dc3           
);
   logic [3:0][31:0]  lsu_match_data;
   logic [3:0]        lsu_trigger_data_match;
   logic [31:0]       store_data_trigger_dc3;
   assign store_data_trigger_dc3[31:0] = { ({16{lsu_pkt_dc3.word}} & store_data_dc3[31:16]) , ({8{(lsu_pkt_dc3.half | lsu_pkt_dc3.word)}} & store_data_dc3[15:8]), store_data_dc3[7:0]};
   for (genvar i=0; i<4; i++) begin
      assign lsu_match_data[i][31:0] = ({32{~trigger_pkt_any[i].select}} & lsu_addr_dc3[31:0]) |
                                       ({32{trigger_pkt_any[i].select & trigger_pkt_any[i].store}} & store_data_trigger_dc3[31:0]);
      rvmaskandmatch trigger_match (.mask(trigger_pkt_any[i].tdata2[31:0]), .data(lsu_match_data[i][31:0]), .masken(trigger_pkt_any[i].match), .match(lsu_trigger_data_match[i]));
      assign lsu_trigger_match_dc3[i] = lsu_pkt_dc3.valid & ~lsu_pkt_dc3.dma &
                                        ((trigger_pkt_any[i].store & lsu_pkt_dc3.store) | (trigger_pkt_any[i].load & lsu_pkt_dc3.load & ~trigger_pkt_any[i].select)) &
                                        lsu_trigger_data_match[i];
   end
endmodule  
module dbg (
   output logic [31:0]                 dbg_cmd_addr,
   output logic [31:0]                 dbg_cmd_wrdata,
   output logic                        dbg_cmd_valid,
   output logic                        dbg_cmd_write,  
   output logic [1:0]                  dbg_cmd_type,  
   output logic [1:0]                  dbg_cmd_size,  
   output logic                        dbg_core_rst_l,  
   input logic [31:0]                  core_dbg_rddata,
   input logic                         core_dbg_cmd_done,  
   input logic                         core_dbg_cmd_fail,  
   output logic                        dbg_dma_bubble,    
   input  logic                        dma_dbg_ready,     
   output logic                        dbg_halt_req,  
   output logic                        dbg_resume_req,  
   input  logic                        dec_tlu_debug_mode,         
   input  logic                        dec_tlu_dbg_halted,  
   input  logic                        dec_tlu_mpc_halted_only,    
   input  logic                        dec_tlu_resume_ack,  
   input logic                         dmi_reg_en,  
   input logic [6:0]                   dmi_reg_addr,  
   input logic                         dmi_reg_wr_en,  
   input logic [31:0]                  dmi_reg_wdata,  
   output logic [31:0]                 dmi_reg_rdata,  
   output logic                        sb_axi_awvalid,
   input  logic                        sb_axi_awready,
   output logic [1-1:0]   sb_axi_awid,
   output logic [31:0]                 sb_axi_awaddr,
   output logic [3:0]                  sb_axi_awregion,
   output logic [7:0]                  sb_axi_awlen,
   output logic [2:0]                  sb_axi_awsize,
   output logic [1:0]                  sb_axi_awburst,
   output logic                        sb_axi_awlock,
   output logic [3:0]                  sb_axi_awcache,
   output logic [2:0]                  sb_axi_awprot,
   output logic [3:0]                  sb_axi_awqos,
   output logic                        sb_axi_wvalid,
   input  logic                        sb_axi_wready,
   output logic [63:0]                 sb_axi_wdata,
   output logic [7:0]                  sb_axi_wstrb,
   output logic                        sb_axi_wlast,
   input  logic                        sb_axi_bvalid,
   output logic                        sb_axi_bready,
   input  logic [1:0]                  sb_axi_bresp,
   input  logic [1-1:0]   sb_axi_bid,
   output logic                        sb_axi_arvalid,
   input  logic                        sb_axi_arready,
   output logic [1-1:0]   sb_axi_arid,
   output logic [31:0]                 sb_axi_araddr,
   output logic [3:0]                  sb_axi_arregion,
   output logic [7:0]                  sb_axi_arlen,
   output logic [2:0]                  sb_axi_arsize,
   output logic [1:0]                  sb_axi_arburst,
   output logic                        sb_axi_arlock,
   output logic [3:0]                  sb_axi_arcache,
   output logic [2:0]                  sb_axi_arprot,
   output logic [3:0]                  sb_axi_arqos,
   input  logic                        sb_axi_rvalid,
   output logic                        sb_axi_rready,
   input  logic [1-1:0]   sb_axi_rid,
   input  logic [63:0]                 sb_axi_rdata,
   input  logic [1:0]                  sb_axi_rresp,
   input  logic                        sb_axi_rlast,
   input logic                         dbg_bus_clk_en,
   input logic                         clk,
   input logic                         free_clk,
   input logic                         rst_l,
   input logic                         dbg_rst_l,
   input logic                         clk_override,
   input logic                         scan_mode
);
localparam TOTAL_INT        = 9;
localparam DCCM_BITS        = 16;
localparam DCCM_BANK_BITS   = 3;
localparam DCCM_NUM_BANKS   = 8;
localparam DCCM_DATA_WIDTH  = 32;
localparam DCCM_FDATA_WIDTH = 39;
localparam DCCM_BYTE_WIDTH  = 4;
localparam DCCM_ECC_WIDTH   = 7;
localparam LSU_RDBUF_DEPTH  = 8;
localparam DMA_BUF_DEPTH    = 4;
localparam LSU_STBUF_DEPTH  = 8;
localparam LSU_SB_BITS      = 16;
localparam DEC_INSTBUF_DEPTH = 4;
localparam ICCM_SIZE         = 512;
localparam ICCM_BITS         = 19;
localparam ICCM_NUM_BANKS    = 8;
localparam ICCM_BANK_BITS    = 3;
localparam ICCM_INDEX_BITS   = 14;
localparam ICCM_BANK_HI      = 4 + (3/4);
localparam ICACHE_TAG_HIGH  = 12;
localparam ICACHE_TAG_LOW   = 6;
localparam ICACHE_IC_DEPTH  = 8;
localparam ICACHE_TAG_DEPTH = 64;
localparam LSU_BUS_TAG     = 4;
localparam DMA_BUS_TAG     = 1;
localparam SB_BUS_TAG      = 1;
localparam IFU_BUS_TAG     = 3;
   typedef enum logic [3:0] {IDLE=4'h0, HALTING=4'h1, HALTED=4'h2, CORE_CMD_START=4'h3, CORE_CMD_WAIT=4'h4, SB_CMD_START=4'h5, SB_CMD_SEND=4'h6, SB_CMD_RESP=4'h7, CMD_DONE=4'h8, RESUMING=4'h9} state_t;
   typedef enum logic [3:0] {SBIDLE=4'h0, WAIT_RD=4'h1, WAIT_WR=4'h2, CMD_RD=4'h3, CMD_WR=4'h4, CMD_WR_ADDR=4'h5, CMD_WR_DATA=4'h6, RSP_RD=4'h7, RSP_WR=4'h8, DONE=4'h9} sb_state_t;
       localparam ICCM_ENABLE = 1'b0;
      localparam DCCM_ENABLE = 1'b1;
   state_t       dbg_state;
   state_t       dbg_nxtstate;
   logic         dbg_state_en;
   logic [31:0]  dmstatus_reg;         
   logic [31:0]  dmcontrol_reg;        
   logic [31:0]  command_reg;
   logic [31:0]  abstractcs_reg;       
   logic [31:0]  haltsum0_reg;
   logic [31:0]  data0_reg;
   logic [31:0]  data1_reg;
   logic [31:0]  data0_din;
   logic         data0_reg_wren, data0_reg_wren0, data0_reg_wren1, data0_reg_wren2;
   logic [31:0]  data1_din;
   logic         data1_reg_wren, data1_reg_wren0, data1_reg_wren1;
   logic         abstractcs_busy_wren;
   logic         abstractcs_busy_din;
   logic [2:0]   abstractcs_error_din;
   logic         abstractcs_error_sel0, abstractcs_error_sel1, abstractcs_error_sel2, abstractcs_error_sel3, abstractcs_error_sel4, abstractcs_error_sel5, abstractcs_error_sel6;
   logic         dbg_sb_bus_error;
   logic         abstractauto_reg_wren;
   logic [1:0]   abstractauto_reg;
   logic         dmstatus_dmerr_wren;
   logic         dmstatus_resumeack_wren;
   logic         dmstatus_resumeack_din;
   logic         dmstatus_havereset_wren;
   logic         dmstatus_havereset_rst;
   logic         dmstatus_resumeack;
   logic         dmstatus_unavail;
   logic         dmstatus_running;
   logic         dmstatus_halted;
   logic         dmstatus_havereset;
   logic         resumereq;
   logic         dmcontrol_wren, dmcontrol_wren_Q;
   logic         execute_command_ns, execute_command;
   logic         command_wren, command_regno_wren;
   logic         command_transfer_din;
   logic         command_postexec_din;
   logic [31:0]  command_din;
   logic [3:0]   dbg_cmd_addr_incr;
   logic [31:0]  dbg_cmd_curr_addr;
   logic [31:0]  dbg_cmd_next_addr;
   logic  [31:0] dmi_reg_rdata_din;
   sb_state_t    sb_state;
   sb_state_t    sb_nxtstate;
   logic         sb_state_en;
   logic              sbcs_wren;
   logic              sbcs_sbbusy_wren;
   logic              sbcs_sbbusy_din;
   logic              sbcs_sbbusyerror_wren;
   logic              sbcs_sbbusyerror_din;
   logic              sbcs_sberror_wren;
   logic [2:0]        sbcs_sberror_din;
   logic              sbcs_unaligned;
   logic              sbcs_illegal_size;
   logic [19:15]      sbcs_reg_int;
   logic              sbdata0_reg_wren0;
   logic              sbdata0_reg_wren1;
   logic              sbdata0_reg_wren;
   logic [31:0]       sbdata0_din;
   logic              sbdata1_reg_wren0;
   logic              sbdata1_reg_wren1;
   logic              sbdata1_reg_wren;
   logic [31:0]       sbdata1_din;
   logic              sbaddress0_reg_wren0;
   logic              sbaddress0_reg_wren1;
   logic              sbaddress0_reg_wren;
   logic [31:0]       sbaddress0_reg_din;
   logic [3:0]        sbaddress0_incr;
   logic              sbreadonaddr_access;
   logic              sbreadondata_access;
   logic              sbdata0wr_access;
   logic              sb_abmem_cmd_done_in, sb_abmem_data_done_in;
   logic              sb_abmem_cmd_done_en, sb_abmem_data_done_en;
   logic              sb_abmem_cmd_done, sb_abmem_data_done;
   logic [31:0]       abmem_addr;
   logic              abmem_addr_in_dccm_region, abmem_addr_in_iccm_region, abmem_addr_in_pic_region;
   logic              abmem_addr_core_local;
   logic              abmem_addr_external;
   logic              sb_cmd_pending, sb_abmem_cmd_pending;
   logic              sb_abmem_cmd_arvalid, sb_abmem_cmd_awvalid, sb_abmem_cmd_wvalid;
   logic              sb_abmem_read_pend;
   logic              sb_abmem_cmd_write;
   logic [2:0]        sb_abmem_cmd_size;
   logic [31:0]       sb_abmem_cmd_addr;
   logic [31:0]       sb_abmem_cmd_wdata;
   logic              sb_cmd_awvalid, sb_cmd_wvalid, sb_cmd_arvalid;
   logic              sb_read_pend;
   logic [2:0]        sb_cmd_size;
   logic [31:0]       sb_cmd_addr;
   logic [63:0]       sb_cmd_wdata;
   logic [31:0]       sb_axi_addr;
   logic [63:0]       sb_axi_wrdata;
   logic [2:0]        sb_axi_size;
   logic              sb_axi_awvalid_q, sb_axi_awready_q;
   logic              sb_axi_wvalid_q, sb_axi_wready_q;
   logic              sb_axi_arvalid_q, sb_axi_arready_q;
   logic              sb_axi_bvalid_q, sb_axi_bready_q;
   logic              sb_axi_rvalid_q, sb_axi_rready_q;
   logic [1:0]        sb_axi_bresp_q, sb_axi_rresp_q;
   logic [63:0]       sb_bus_rdata;
   logic [31:0]       sbcs_reg;
   logic [31:0]       sbaddress0_reg;
   logic [31:0]       sbdata0_reg;
   logic [31:0]       sbdata1_reg;
   logic              dbg_dm_rst_l;
   logic              dbg_free_clken;
   logic              dbg_free_clk;
   logic              sb_free_clken;
   logic              sb_free_clk;
   logic              bus_clken;
   logic              bus_clk;
   assign dbg_free_clken  = dmi_reg_en | execute_command | (dbg_state != IDLE) | dbg_state_en | dec_tlu_dbg_halted | clk_override;
   assign sb_free_clken = dmi_reg_en | execute_command | sb_state_en | (sb_state != SBIDLE) | clk_override;
   assign bus_clken = (sb_axi_awvalid | sb_axi_wvalid | sb_axi_arvalid | sb_axi_bvalid | sb_axi_rvalid | clk_override) & dbg_bus_clk_en;
   rvoclkhdr dbg_free_cgc     (.en(dbg_free_clken), .l1clk(dbg_free_clk), .*);
   rvoclkhdr sb_free_cgc     (.en(sb_free_clken), .l1clk(sb_free_clk), .*);
   assign dbg_dm_rst_l = dbg_rst_l & (dmcontrol_reg[0] | scan_mode);
   assign dbg_core_rst_l = ~dmcontrol_reg[1];
   assign        sbcs_reg[31:29] = 3'b1;
   assign        sbcs_reg[28:23] = '0;
   assign        sbcs_reg[19:15] = {sbcs_reg_int[19], ~sbcs_reg_int[18], sbcs_reg_int[17:15]};
   assign        sbcs_reg[11:5]  = 7'h20;
   assign        sbcs_reg[4:0]   = 5'b01111;
   assign        sbcs_wren = (dmi_reg_addr ==  7'h38) & dmi_reg_en & dmi_reg_wr_en & (sb_state == SBIDLE);  
   assign        sbcs_sbbusyerror_wren = (sbcs_wren & dmi_reg_wdata[22]) |
                                         ((sb_state != SBIDLE) & dmi_reg_en & ((dmi_reg_addr == 7'h39) | (dmi_reg_addr == 7'h3c) | (dmi_reg_addr == 7'h3d)));
   assign        sbcs_sbbusyerror_din = ~(sbcs_wren & dmi_reg_wdata[22]);    
   rvdffs #(1) sbcs_sbbusyerror_reg  (.din(sbcs_sbbusyerror_din),  .dout(sbcs_reg[22]),    .en(sbcs_sbbusyerror_wren), .rst_l(dbg_dm_rst_l), .clk(sb_free_clk));
   rvdffs #(1) sbcs_sbbusy_reg       (.din(sbcs_sbbusy_din),       .dout(sbcs_reg[21]),    .en(sbcs_sbbusy_wren),      .rst_l(dbg_dm_rst_l), .clk(sb_free_clk));
   rvdffs #(1) sbcs_sbreadonaddr_reg (.din(dmi_reg_wdata[20]),     .dout(sbcs_reg[20]),    .en(sbcs_wren),             .rst_l(dbg_dm_rst_l), .clk(sb_free_clk));
   rvdffs #(5) sbcs_misc_reg         (.din({dmi_reg_wdata[19],~dmi_reg_wdata[18],dmi_reg_wdata[17:15]}),
                                      .dout(sbcs_reg_int[19:15]), .en(sbcs_wren),             .rst_l(dbg_dm_rst_l), .clk(sb_free_clk));
   rvdffs #(3) sbcs_error_reg        (.din(sbcs_sberror_din[2:0]), .dout(sbcs_reg[14:12]), .en(sbcs_sberror_wren),     .rst_l(dbg_dm_rst_l), .clk(sb_free_clk));
   assign sbcs_unaligned =    ((sbcs_reg[19:17] == 3'b001) &  sbaddress0_reg[0]) |
                              ((sbcs_reg[19:17] == 3'b010) &  (|sbaddress0_reg[1:0])) |
                              ((sbcs_reg[19:17] == 3'b011) &  (|sbaddress0_reg[2:0]));
   assign sbcs_illegal_size = sbcs_reg[19];     
   assign sbaddress0_incr[3:0] = ({4{(sbcs_reg[19:17] == 3'h0)}} &  4'b0001) |
                                 ({4{(sbcs_reg[19:17] == 3'h1)}} &  4'b0010) |
                                 ({4{(sbcs_reg[19:17] == 3'h2)}} &  4'b0100) |
                                 ({4{(sbcs_reg[19:17] == 3'h3)}} &  4'b1000);
   assign        sbdata0_reg_wren0   = dmi_reg_en & dmi_reg_wr_en & (dmi_reg_addr == 7'h3c);    
   assign        sbdata0_reg_wren1   = (sb_state == RSP_RD) & sb_state_en & ~sbcs_sberror_wren;
   assign        sbdata0_reg_wren    = sbdata0_reg_wren0 | sbdata0_reg_wren1;
   assign        sbdata1_reg_wren0   = dmi_reg_en & dmi_reg_wr_en & (dmi_reg_addr == 7'h3d);    
   assign        sbdata1_reg_wren1   = (sb_state == RSP_RD) & sb_state_en & ~sbcs_sberror_wren;
   assign        sbdata1_reg_wren    = sbdata1_reg_wren0 | sbdata1_reg_wren1;
   assign        sbdata0_din[31:0]   = ({32{sbdata0_reg_wren0}} & dmi_reg_wdata[31:0]) |
                                       ({32{sbdata0_reg_wren1}} & sb_bus_rdata[31:0]);
   assign        sbdata1_din[31:0]   = ({32{sbdata1_reg_wren0}} & dmi_reg_wdata[31:0]) |
                                       ({32{sbdata1_reg_wren1}} & sb_bus_rdata[63:32]);
   rvdffe #(32)    dbg_sbdata0_reg    (.*, .din(sbdata0_din[31:0]), .dout(sbdata0_reg[31:0]), .en(sbdata0_reg_wren), .rst_l(dbg_dm_rst_l));
   rvdffe #(32)    dbg_sbdata1_reg    (.*, .din(sbdata1_din[31:0]), .dout(sbdata1_reg[31:0]), .en(sbdata1_reg_wren), .rst_l(dbg_dm_rst_l));
   assign        sbaddress0_reg_wren0   = dmi_reg_en & dmi_reg_wr_en & (dmi_reg_addr == 7'h39);
   assign        sbaddress0_reg_wren    = sbaddress0_reg_wren0 | sbaddress0_reg_wren1;
   assign        sbaddress0_reg_din[31:0]= ({32{sbaddress0_reg_wren0}} & dmi_reg_wdata[31:0]) |
                                           ({32{sbaddress0_reg_wren1}} & (sbaddress0_reg[31:0] + {28'b0,sbaddress0_incr[3:0]}));
   rvdffe #(32)    dbg_sbaddress0_reg    (.*, .din(sbaddress0_reg_din[31:0]), .dout(sbaddress0_reg[31:0]), .en(sbaddress0_reg_wren), .rst_l(dbg_dm_rst_l));
   assign sbreadonaddr_access = dmi_reg_en & dmi_reg_wr_en & (dmi_reg_addr == 7'h39) & sbcs_reg[20];    
   assign sbreadondata_access = dmi_reg_en & ~dmi_reg_wr_en & (dmi_reg_addr == 7'h3c) & sbcs_reg[15];   
   assign sbdata0wr_access  = dmi_reg_en &  dmi_reg_wr_en & (dmi_reg_addr == 7'h3c);                    
   assign dmcontrol_wren      = (dmi_reg_addr ==  7'h10) & dmi_reg_en & dmi_reg_wr_en;
   assign dmcontrol_reg[29]   = '0;
   assign dmcontrol_reg[27:2] = '0;
   assign resumereq           = dmcontrol_reg[30] & ~dmcontrol_reg[31] & dmcontrol_wren_Q;
   rvdffs #(4) dmcontrolff (.din({dmi_reg_wdata[31:30],dmi_reg_wdata[28],dmi_reg_wdata[1]}), .dout({dmcontrol_reg[31:30], dmcontrol_reg[28], dmcontrol_reg[1]}), .en(dmcontrol_wren), .rst_l(dbg_dm_rst_l), .clk(dbg_free_clk));
   rvdffs #(1) dmcontrol_dmactive_ff (.din(dmi_reg_wdata[0]), .dout(dmcontrol_reg[0]), .en(dmcontrol_wren), .rst_l(dbg_rst_l), .clk(dbg_free_clk));
   rvdff  #(1) dmcontrol_wrenff(.din(dmcontrol_wren), .dout(dmcontrol_wren_Q), .rst_l(dbg_dm_rst_l), .clk(dbg_free_clk));
   assign dmstatus_reg[31:20] = '0;
   assign dmstatus_reg[19:18] = {2{dmstatus_havereset}};
   assign dmstatus_reg[15:14] = '0;
   assign dmstatus_reg[7]     = '1;
   assign dmstatus_reg[6:4]   = '0;
   assign dmstatus_reg[17:16] = {2{dmstatus_resumeack}};
   assign dmstatus_reg[13:12] = {2{dmstatus_unavail}};
   assign dmstatus_reg[11:10] = {2{dmstatus_running}};
   assign dmstatus_reg[9:8]   = {2{dmstatus_halted}};
   assign dmstatus_reg[3:0]   = 4'h2;
   assign dmstatus_resumeack_wren = ((dbg_state == RESUMING) & dec_tlu_resume_ack) | (dmstatus_resumeack & resumereq & dmstatus_halted);
   assign dmstatus_resumeack_din  = (dbg_state == RESUMING) & dec_tlu_resume_ack;
   assign dmstatus_havereset_wren = (dmi_reg_addr == 7'h10) & dmi_reg_wdata[1] & dmi_reg_en & dmi_reg_wr_en;
   assign dmstatus_havereset_rst  = (dmi_reg_addr == 7'h10) & dmi_reg_wdata[28] & dmi_reg_en & dmi_reg_wr_en;
   assign dmstatus_unavail = dmcontrol_reg[1] | ~rst_l;
   assign dmstatus_running = ~(dmstatus_unavail | dmstatus_halted);
   rvdffs  #(1) dmstatus_resumeack_reg (.din(dmstatus_resumeack_din), .dout(dmstatus_resumeack), .en(dmstatus_resumeack_wren), .rst_l(dbg_dm_rst_l), .clk(dbg_free_clk));
   rvdff   #(1) dmstatus_halted_reg    (.din(dec_tlu_dbg_halted & ~dec_tlu_mpc_halted_only),     .dout(dmstatus_halted), .rst_l(dbg_dm_rst_l), .clk(dbg_free_clk));
   rvdffsc #(1) dmstatus_havereset_reg (.din(1'b1), .dout(dmstatus_havereset), .en(dmstatus_havereset_wren), .clear(dmstatus_havereset_rst), .rst_l(dbg_dm_rst_l), .clk(dbg_free_clk));
   assign haltsum0_reg[31:1] = '0;
   assign haltsum0_reg[0]    = dmstatus_halted;
   assign        abstractcs_reg[31:13] = '0;
   assign        abstractcs_reg[11]    = '0;
   assign        abstractcs_reg[7:4]   = '0;
   assign        abstractcs_reg[3:0]   = 4'h2;     
   assign        abstractcs_error_sel0 = abstractcs_reg[12] & ~(|abstractcs_reg[10:8]) & dmi_reg_en & ((dmi_reg_wr_en & ((dmi_reg_addr == 7'h16) | (dmi_reg_addr == 7'h17)) | (dmi_reg_addr == 7'h18)) |
                                                                                                       (dmi_reg_addr == 7'h4) | (dmi_reg_addr == 7'h5));
   assign        abstractcs_error_sel1 = execute_command & ~(|abstractcs_reg[10:8]) &
                                         ((~((command_reg[31:24] == 8'b0) | (command_reg[31:24] == 8'h2)))                      |    
                                          (((command_reg[22:20] == 3'b011) | (command_reg[22])) & (command_reg[31:24] == 8'h2)) |    
                                          ((command_reg[22:20] != 3'b010) & ((command_reg[31:24] == 8'h0) & command_reg[17]))   |    
                                          ((command_reg[31:24] == 8'h0) & command_reg[18]));                                           
   assign        abstractcs_error_sel2 = ((core_dbg_cmd_done & core_dbg_cmd_fail) |                    
                                          (execute_command & (command_reg[31:24] == 8'h0) &   
                                                (((command_reg[15:12] == 4'h1) & (command_reg[11:5] != 0)) | (command_reg[15:13] != 0)))) & ~(|abstractcs_reg[10:8]);
   assign        abstractcs_error_sel3 = execute_command & (dbg_state != HALTED) & ~(|abstractcs_reg[10:8]);
   assign        abstractcs_error_sel4 = dbg_sb_bus_error & dbg_bus_clk_en & ~(|abstractcs_reg[10:8]); 
   assign        abstractcs_error_sel5 = execute_command & (command_reg[31:24] == 8'h2) & ~(|abstractcs_reg[10:8]) &
                                         (((command_reg[22:20] == 3'b001) & data1_reg[0]) | ((command_reg[22:20] == 3'b010) & (|data1_reg[1:0])));   
   assign        abstractcs_error_sel6 = (dmi_reg_addr ==  7'h16) & dmi_reg_en & dmi_reg_wr_en;
   assign        abstractcs_error_din[2:0]  = abstractcs_error_sel0 ? 3'b001 :                   
                                                 abstractcs_error_sel1 ? 3'b010 :                
                                                    abstractcs_error_sel2 ? 3'b011 :             
                                                       abstractcs_error_sel3 ? 3'b100 :          
                                                          abstractcs_error_sel4 ? 3'b101 :       
                                                             abstractcs_error_sel5 ? 3'b111 :    
                                                                abstractcs_error_sel6 ? (~dmi_reg_wdata[10:8] & abstractcs_reg[10:8]) :    
                                                                                        abstractcs_reg[10:8];                              
   rvdffs #(1) dmabstractcs_busy_reg  (.din(abstractcs_busy_din), .dout(abstractcs_reg[12]), .en(abstractcs_busy_wren), .rst_l(dbg_dm_rst_l), .clk(dbg_free_clk));
   rvdff  #(3) dmabstractcs_error_reg (.din(abstractcs_error_din[2:0]), .dout(abstractcs_reg[10:8]), .rst_l(dbg_dm_rst_l), .clk(dbg_free_clk));
   assign abstractauto_reg_wren  = dmi_reg_en & dmi_reg_wr_en & (dmi_reg_addr == 7'h18) & ~abstractcs_reg[12];
   rvdffs #(2) dbg_abstractauto_reg (.*, .din(dmi_reg_wdata[1:0]), .dout(abstractauto_reg[1:0]), .en(abstractauto_reg_wren), .rst_l(dbg_dm_rst_l), .clk(dbg_free_clk));
   assign execute_command_ns = command_wren |
                               (dmi_reg_en & ~abstractcs_reg[12] & (((dmi_reg_addr == 7'h4) & abstractauto_reg[0]) | ((dmi_reg_addr == 7'h5) & abstractauto_reg[1])));
   assign command_wren = (dmi_reg_addr ==  7'h17) & dmi_reg_en & dmi_reg_wr_en;
   assign command_regno_wren = command_wren | ((command_reg[31:24] == 8'h0) & command_reg[19] & (dbg_state == CMD_DONE) & ~(|abstractcs_reg[10:8]));   
   assign command_postexec_din = (dmi_reg_wdata[31:24] == 8'h0) & dmi_reg_wdata[18];
   assign command_transfer_din = (dmi_reg_wdata[31:24] == 8'h0) & dmi_reg_wdata[17];
   assign command_din[31:16] = {dmi_reg_wdata[31:24],1'b0,dmi_reg_wdata[22:19],command_postexec_din,command_transfer_din, dmi_reg_wdata[16]};
   assign command_din[15:0] =  command_wren ? dmi_reg_wdata[15:0] : dbg_cmd_next_addr[15:0];
   rvdff  #(1)  execute_commandff   (.*, .din(execute_command_ns), .dout(execute_command), .clk(dbg_free_clk), .rst_l(dbg_dm_rst_l));
   rvdffe #(16) dmcommand_reg       (.*, .din(command_din[31:16]), .dout(command_reg[31:16]), .en(command_wren), .rst_l(dbg_dm_rst_l));
   rvdffe #(16) dmcommand_regno_reg (.*, .din(command_din[15:0]),  .dout(command_reg[15:0]),  .en(command_regno_wren), .rst_l(dbg_dm_rst_l));
   assign data0_reg_wren0   = (dmi_reg_en & dmi_reg_wr_en & (dmi_reg_addr == 7'h4) & (dbg_state == HALTED) & ~abstractcs_reg[12]);
   assign data0_reg_wren1   = core_dbg_cmd_done & (dbg_state == CORE_CMD_WAIT) & ~command_reg[16];
   assign data0_reg_wren    = data0_reg_wren0 | data0_reg_wren1 | data0_reg_wren2;
   assign data0_din[31:0]   = ({32{data0_reg_wren0}} & dmi_reg_wdata[31:0])   |
                              ({32{data0_reg_wren1}} & core_dbg_rddata[31:0]) |
                              ({32{data0_reg_wren2}} & sb_bus_rdata[31:0]);
   rvdffe #(32) dbg_data0_reg (.*, .din(data0_din[31:0]), .dout(data0_reg[31:0]), .en(data0_reg_wren), .rst_l(dbg_dm_rst_l));
   assign data1_reg_wren0   = (dmi_reg_en & dmi_reg_wr_en & (dmi_reg_addr == 7'h5) & (dbg_state == HALTED) & ~abstractcs_reg[12]);
   assign data1_reg_wren1   = (dbg_state == CMD_DONE) & (command_reg[31:24] == 8'h2) & command_reg[19] & ~(|abstractcs_reg[10:8]);    
   assign data1_reg_wren    = data1_reg_wren0 | data1_reg_wren1;
   assign data1_din[31:0]   = ({32{data1_reg_wren0}} & dmi_reg_wdata[31:0]) |
                              ({32{data1_reg_wren1}} & dbg_cmd_next_addr[31:0]);
   rvdffe #(32)    dbg_data1_reg    (.*, .din(data1_din[31:0]), .dout(data1_reg[31:0]), .en(data1_reg_wren), .rst_l(dbg_dm_rst_l));
   rvdffs #(1) sb_abmem_cmd_doneff  (.din(sb_abmem_cmd_done_in),  .dout(sb_abmem_cmd_done),  .en(sb_abmem_cmd_done_en),  .clk(dbg_free_clk), .rst_l(dbg_dm_rst_l), .*);
   rvdffs #(1) sb_abmem_data_doneff (.din(sb_abmem_data_done_in), .dout(sb_abmem_data_done), .en(sb_abmem_data_done_en), .clk(dbg_free_clk), .rst_l(dbg_dm_rst_l), .*);
   always_comb begin
      dbg_nxtstate            = IDLE;
      dbg_state_en            = 1'b0;
      abstractcs_busy_wren    = 1'b0;
      abstractcs_busy_din     = 1'b0;
      dbg_halt_req            = dmcontrol_wren_Q & dmcontrol_reg[31] & ~dmcontrol_reg[1];       
      dbg_resume_req          = 1'b0;          
      dbg_sb_bus_error        = 1'b0;
      data0_reg_wren2         = 1'b0;
      sb_abmem_cmd_done_in    = 1'b0;
      sb_abmem_data_done_in   = 1'b0;
      sb_abmem_cmd_done_en    = 1'b0;
      sb_abmem_data_done_en   = 1'b0;
       case (dbg_state)
            IDLE: begin
                     dbg_nxtstate         = (dmstatus_reg[9] | dec_tlu_mpc_halted_only) ? HALTED : HALTING;          
                     dbg_state_en         = ((dmcontrol_reg[31] & ~dec_tlu_debug_mode) | dmstatus_reg[9] | dec_tlu_mpc_halted_only) & ~dmcontrol_reg[1];       
                     dbg_halt_req         = dmcontrol_reg[31] & ~dmcontrol_reg[1];                         
            end
            HALTING : begin
                     dbg_nxtstate         = dmcontrol_reg[1] ? IDLE : HALTED;                              
                     dbg_state_en         = dmstatus_reg[9] | dmcontrol_reg[1];                            
            end
            HALTED: begin
                     dbg_nxtstate         = (dmstatus_reg[9] & ~dmcontrol_reg[1]) ? (resumereq ? RESUMING : (((command_reg[31:24] == 8'h2) & abmem_addr_external) ? SB_CMD_START : CORE_CMD_START)) :
                                                                                    (dmcontrol_reg[31] ? HALTING : IDLE);        
                     dbg_state_en         = (dmstatus_reg[9] & resumereq) | execute_command | dmcontrol_reg[1] | ~(dmstatus_reg[9] | dec_tlu_mpc_halted_only);
                     abstractcs_busy_wren = dbg_state_en & ((dbg_nxtstate == CORE_CMD_START) | (dbg_nxtstate == SB_CMD_START));                  
                     abstractcs_busy_din  = 1'b1;
                     dbg_resume_req       = dbg_state_en & (dbg_nxtstate == RESUMING);                        
            end
            CORE_CMD_START: begin
                     dbg_nxtstate         = dmcontrol_reg[1] ? IDLE : ((|abstractcs_reg[10:8]) | ((command_reg[31:24] == 8'h0) & ~command_reg[17])) ? CMD_DONE : CORE_CMD_WAIT;      
                     dbg_state_en         = dbg_cmd_valid | (|abstractcs_reg[10:8]) | ((command_reg[31:24] == 8'h0) & ~command_reg[17]) | dmcontrol_reg[1];
            end
            CORE_CMD_WAIT: begin
                     dbg_nxtstate         = dmcontrol_reg[1] ? IDLE : CMD_DONE;
                     dbg_state_en         = core_dbg_cmd_done | dmcontrol_reg[1];               
            end
            SB_CMD_START: begin
                     dbg_nxtstate         = dmcontrol_reg[1] ? IDLE : (|abstractcs_reg[10:8]) ? CMD_DONE : SB_CMD_SEND;
                     dbg_state_en         = (dbg_bus_clk_en & ~sb_cmd_pending) | (|abstractcs_reg[10:8]) | dmcontrol_reg[1];
            end
            SB_CMD_SEND: begin
                     sb_abmem_cmd_done_in = 1'b1;
                     sb_abmem_data_done_in= 1'b1;
                     sb_abmem_cmd_done_en = ((sb_axi_awvalid & sb_axi_awready) | (sb_axi_arvalid & sb_axi_arready)) & dbg_bus_clk_en;
                     sb_abmem_data_done_en= ((sb_axi_wvalid  & sb_axi_wready)  | (sb_axi_arvalid & sb_axi_arready)) & dbg_bus_clk_en;
                     dbg_nxtstate         = dmcontrol_reg[1] ? IDLE : SB_CMD_RESP;
                     dbg_state_en         = (sb_abmem_cmd_done | sb_abmem_cmd_done_en) & (sb_abmem_data_done | sb_abmem_data_done_en) & dbg_bus_clk_en;
            end
            SB_CMD_RESP: begin
                     dbg_nxtstate         = dmcontrol_reg[1] ? IDLE : CMD_DONE;
                     dbg_state_en         = ((sb_axi_rvalid & sb_axi_rready) | (sb_axi_bvalid & sb_axi_bready)) & dbg_bus_clk_en;
                     dbg_sb_bus_error     = ((sb_axi_rvalid & sb_axi_rready & sb_axi_rresp[1]) | (sb_axi_bvalid & sb_axi_bready & sb_axi_bresp[1])) & dbg_bus_clk_en;
                     data0_reg_wren2      = dbg_state_en & ~sb_abmem_cmd_write & ~dbg_sb_bus_error;
            end
            CMD_DONE: begin
                     dbg_nxtstate         = dmcontrol_reg[1] ? IDLE : HALTED;
                     dbg_state_en         = 1'b1;
                     abstractcs_busy_wren = dbg_state_en;                     
                     abstractcs_busy_din  = 1'b0;
                     sb_abmem_cmd_done_in = 1'b0;
                     sb_abmem_data_done_in= 1'b0;
                     sb_abmem_cmd_done_en = 1'b1;
                     sb_abmem_data_done_en= 1'b1;
            end
            RESUMING : begin
                     dbg_nxtstate            = IDLE;
                     dbg_state_en            = dmstatus_reg[17] | dmcontrol_reg[1];              
           end
           default : begin
                     dbg_nxtstate            = IDLE;
                     dbg_state_en            = 1'b0;
                     abstractcs_busy_wren    = 1'b0;
                     abstractcs_busy_din     = 1'b0;
                     dbg_halt_req            = 1'b0;          
                     dbg_resume_req          = 1'b0;          
          end
         endcase
   end  
   assign dmi_reg_rdata_din[31:0] = ({32{dmi_reg_addr == 7'h4}}  & data0_reg[31:0])      |
                                    ({32{dmi_reg_addr == 7'h5}}  & data1_reg[31:0])      |
                                    ({32{dmi_reg_addr == 7'h10}} & {2'b0,dmcontrol_reg[29],1'b0,dmcontrol_reg[27:0]})  |   
                                    ({32{dmi_reg_addr == 7'h11}} & dmstatus_reg[31:0])   |
                                    ({32{dmi_reg_addr == 7'h16}} & abstractcs_reg[31:0]) |
                                    ({32{dmi_reg_addr == 7'h17}} & command_reg[31:0])    |
                                    ({32{dmi_reg_addr == 7'h18}} & {30'h0,abstractauto_reg[1:0]})    |
                                    ({32{dmi_reg_addr == 7'h40}} & haltsum0_reg[31:0])   |
                                    ({32{dmi_reg_addr == 7'h38}} & sbcs_reg[31:0])       |
                                    ({32{dmi_reg_addr == 7'h39}} & sbaddress0_reg[31:0]) |
                                    ({32{dmi_reg_addr == 7'h3c}} & sbdata0_reg[31:0])    |
                                    ({32{dmi_reg_addr == 7'h3d}} & sbdata1_reg[31:0]);
   rvdffs #($bits(state_t)) dbg_state_reg    (.din(dbg_nxtstate), .dout({dbg_state}), .en(dbg_state_en), .rst_l(dbg_dm_rst_l & rst_l), .clk(dbg_free_clk));   
   rvdffs  #(32) dmi_rddata_reg(.din(dmi_reg_rdata_din), .dout(dmi_reg_rdata), .en(dmi_reg_en), .rst_l(dbg_dm_rst_l), .clk(dbg_free_clk));
   assign abmem_addr[31:0] = data1_reg[31:0];
   assign abmem_addr_core_local = (abmem_addr_in_dccm_region | abmem_addr_in_iccm_region | abmem_addr_in_pic_region);
   assign abmem_addr_external   = ~abmem_addr_core_local;
   assign abmem_addr_in_dccm_region = (abmem_addr[31:28] == 4'hf) & DCCM_ENABLE;
   assign abmem_addr_in_iccm_region = (abmem_addr[31:28] == 4'he) & ICCM_ENABLE;
   assign abmem_addr_in_pic_region  = (abmem_addr[31:28] == 4'hf);
   assign dbg_cmd_addr[31:0]    = (command_reg[31:24] == 8'h2) ? data1_reg[31:0]  : {20'b0, command_reg[11:0]};
   assign dbg_cmd_wrdata[31:0]  = data0_reg[31:0];
   assign dbg_cmd_valid         = (dbg_state == CORE_CMD_START) & ~((|abstractcs_reg[10:8]) | ((command_reg[31:24] == 8'h0) & ~command_reg[17]) | ((command_reg[31:24] == 8'h2) & abmem_addr_external)) & dma_dbg_ready;
   assign dbg_cmd_write         = command_reg[16];
   assign dbg_cmd_type[1:0]     = (command_reg[31:24] == 8'h2) ? 2'b10 : {1'b0, (command_reg[15:12] == 4'b0)};
   assign dbg_cmd_size[1:0]     = command_reg[21:20];
   assign dbg_cmd_addr_incr[3:0]  = (command_reg[31:24] == 8'h2) ? (4'h1 << sb_abmem_cmd_size[1:0]) : 4'h1;
   assign dbg_cmd_curr_addr[31:0] = (command_reg[31:24] == 8'h2) ? data1_reg[31:0]  : {16'b0, command_reg[15:0]};
   assign dbg_cmd_next_addr[31:0] = dbg_cmd_curr_addr[31:0] + {28'h0,dbg_cmd_addr_incr[3:0]};
   assign sb_abmem_cmd_awvalid    = (dbg_state == SB_CMD_SEND) & sb_abmem_cmd_write & ~sb_abmem_cmd_done;
   assign sb_abmem_cmd_wvalid     = (dbg_state == SB_CMD_SEND) & sb_abmem_cmd_write & ~sb_abmem_data_done;
   assign sb_abmem_cmd_arvalid    = (dbg_state == SB_CMD_SEND) & ~sb_abmem_cmd_write & ~sb_abmem_cmd_done & ~sb_abmem_data_done;
   assign sb_abmem_read_pend      = (dbg_state == SB_CMD_RESP) & ~sb_abmem_cmd_write;
   assign sb_abmem_cmd_write      = command_reg[16];
   assign sb_abmem_cmd_size[2:0]  = {1'b0, command_reg[21:20]};
   assign sb_abmem_cmd_addr[31:0] = abmem_addr[31:0];
   assign sb_abmem_cmd_wdata[31:0] = data0_reg[31:0];
   assign dbg_dma_bubble = ((dbg_state == CORE_CMD_START) & ~(|abstractcs_reg[10:8])) | (dbg_state == CORE_CMD_WAIT);
   assign sb_cmd_pending       = (sb_state == CMD_RD) | (sb_state == CMD_WR) | (sb_state == CMD_WR_ADDR) | (sb_state == CMD_WR_DATA) | (sb_state == RSP_RD) | (sb_state == RSP_WR);
   assign sb_abmem_cmd_pending = (dbg_state == SB_CMD_START) | (dbg_state == SB_CMD_SEND) | (dbg_state== SB_CMD_RESP);
  always_comb begin
      sb_nxtstate            = SBIDLE;
      sb_state_en            = 1'b0;
      sbcs_sbbusy_wren       = 1'b0;
      sbcs_sbbusy_din        = 1'b0;
      sbcs_sberror_wren      = 1'b0;
      sbcs_sberror_din[2:0]  = 3'b0;
      sbaddress0_reg_wren1   = 1'b0;
      case (sb_state)
            SBIDLE: begin
                     sb_nxtstate            = sbdata0wr_access ? WAIT_WR : WAIT_RD;
                     sb_state_en            = (sbdata0wr_access | sbreadondata_access | sbreadonaddr_access) & ~(|sbcs_reg[14:12]);
                     sbcs_sbbusy_wren       = sb_state_en;                                                  
                     sbcs_sbbusy_din        = 1'b1;
                     sbcs_sberror_wren      = sbcs_wren & (|dmi_reg_wdata[14:12]);                                             
                     sbcs_sberror_din[2:0]  = ~dmi_reg_wdata[14:12] & sbcs_reg[14:12];
            end
            WAIT_RD: begin
                     sb_nxtstate           = (sbcs_unaligned | sbcs_illegal_size) ? DONE : CMD_RD;
                     sb_state_en           = (dbg_bus_clk_en & ~sb_abmem_cmd_pending) | sbcs_unaligned | sbcs_illegal_size;
                     sbcs_sberror_wren     = sbcs_unaligned | sbcs_illegal_size;
                     sbcs_sberror_din[2:0] = sbcs_unaligned ? 3'b011 : 3'b100;
            end
            WAIT_WR: begin
                     sb_nxtstate           = (sbcs_unaligned | sbcs_illegal_size) ? DONE : CMD_WR;
                     sb_state_en           = (dbg_bus_clk_en & ~sb_abmem_cmd_pending) | sbcs_unaligned | sbcs_illegal_size;
                     sbcs_sberror_wren     = sbcs_unaligned | sbcs_illegal_size;
                     sbcs_sberror_din[2:0] = sbcs_unaligned ? 3'b011 : 3'b100;
            end
            CMD_RD : begin
                     sb_nxtstate           = RSP_RD;
                     sb_state_en           = sb_axi_arvalid & sb_axi_arready & dbg_bus_clk_en;
            end
            CMD_WR : begin
                     sb_nxtstate           = (sb_axi_awready & sb_axi_wready) ? RSP_WR : (sb_axi_awready ? CMD_WR_DATA : CMD_WR_ADDR);
                     sb_state_en           = ((sb_axi_awvalid & sb_axi_awready) | (sb_axi_wvalid & sb_axi_wready)) & dbg_bus_clk_en;
            end
            CMD_WR_ADDR : begin
                     sb_nxtstate           = RSP_WR;
                     sb_state_en           = sb_axi_awvalid & sb_axi_awready & dbg_bus_clk_en;
            end
            CMD_WR_DATA : begin
                     sb_nxtstate           = RSP_WR;
                     sb_state_en           = sb_axi_wvalid & sb_axi_wready & dbg_bus_clk_en;
            end
            RSP_RD: begin
                     sb_nxtstate           = DONE;
                     sb_state_en           = sb_axi_rvalid & sb_axi_rready & dbg_bus_clk_en;
                     sbcs_sberror_wren     = sb_state_en & sb_axi_rresp[1];
                     sbcs_sberror_din[2:0] = 3'b010;
            end
            RSP_WR: begin
                     sb_nxtstate           = DONE;
                     sb_state_en           = sb_axi_bvalid & sb_axi_bready & dbg_bus_clk_en;
                     sbcs_sberror_wren     = sb_state_en & sb_axi_bresp[1];
                     sbcs_sberror_din[2:0] = 3'b010;
            end
            DONE: begin
                     sb_nxtstate            = SBIDLE;
                     sb_state_en            = 1'b1;
                     sbcs_sbbusy_wren       = 1'b1;                            
                     sbcs_sbbusy_din        = 1'b0;
                     sbaddress0_reg_wren1   = sbcs_reg[16] & (sbcs_reg[14:12] == 3'b0);     
            end
            default : begin
                     sb_nxtstate            = SBIDLE;
                     sb_state_en            = 1'b0;
                     sbcs_sbbusy_wren       = 1'b0;
                     sbcs_sbbusy_din        = 1'b0;
                     sbcs_sberror_wren      = 1'b0;
                     sbcs_sberror_din[2:0]  = 3'b0;
                     sbaddress0_reg_wren1   = 1'b0;
           end
         endcase
   end  
   rvdffs #($bits(sb_state_t)) sb_state_reg (.din(sb_nxtstate), .dout({sb_state}), .en(sb_state_en), .rst_l(dbg_dm_rst_l), .clk(sb_free_clk));
   rvdff_fpga  #(2) axi_bresp_ff (.din(sb_axi_bresp[1:0]), .dout(sb_axi_bresp_q[1:0]), .rst_l(dbg_dm_rst_l), .clk(bus_clk), .clken(bus_clken), .rawclk(clk), .*);
   rvdff_fpga  #(2) axi_rresp_ff (.din(sb_axi_rresp[1:0]), .dout(sb_axi_rresp_q[1:0]), .rst_l(dbg_dm_rst_l), .clk(bus_clk), .clken(bus_clken), .rawclk(clk), .*);
   rvdffs #(.WIDTH(1)) axi_awvalid_ff (.din(sb_axi_awvalid), .dout(sb_axi_awvalid_q), .en(dbg_bus_clk_en), .rst_l(dbg_dm_rst_l), .clk(sb_free_clk), .*);
   rvdffs #(.WIDTH(1)) axi_awready_ff (.din(sb_axi_awready), .dout(sb_axi_awready_q), .en(dbg_bus_clk_en), .rst_l(dbg_dm_rst_l), .clk(sb_free_clk), .*);
   rvdffs #(.WIDTH(1)) axi_wvalid_ff (.din(sb_axi_wvalid), .dout(sb_axi_wvalid_q), .en(dbg_bus_clk_en), .rst_l(dbg_dm_rst_l), .clk(sb_free_clk), .*);
   rvdffs #(.WIDTH(1)) axi_wready_ff (.din(sb_axi_wready), .dout(sb_axi_wready_q), .en(dbg_bus_clk_en), .rst_l(dbg_dm_rst_l), .clk(sb_free_clk), .*);
   rvdffs #(.WIDTH(1)) axi_arvalid_ff (.din(sb_axi_arvalid), .dout(sb_axi_arvalid_q), .en(dbg_bus_clk_en), .rst_l(dbg_dm_rst_l), .clk(sb_free_clk), .*);
   rvdffs #(.WIDTH(1)) axi_arready_ff (.din(sb_axi_arready), .dout(sb_axi_arready_q), .en(dbg_bus_clk_en), .rst_l(dbg_dm_rst_l), .clk(sb_free_clk), .*);
   rvdffs #(.WIDTH(1)) axi_bvalid_ff (.din(sb_axi_bvalid), .dout(sb_axi_bvalid_q), .en(dbg_bus_clk_en), .rst_l(dbg_dm_rst_l), .clk(sb_free_clk), .*);
   rvdffs #(.WIDTH(1)) axi_bready_ff (.din(sb_axi_bready), .dout(sb_axi_bready_q), .en(dbg_bus_clk_en), .rst_l(dbg_dm_rst_l), .clk(sb_free_clk), .*);
   rvdffs #(.WIDTH(1)) axi_rvalid_ff (.din(sb_axi_rvalid), .dout(sb_axi_rvalid_q), .en(dbg_bus_clk_en), .rst_l(dbg_dm_rst_l), .clk(sb_free_clk), .*);
   rvdffs #(.WIDTH(1)) axi_rready_ff (.din(sb_axi_rready), .dout(sb_axi_rready_q), .en(dbg_bus_clk_en), .rst_l(dbg_dm_rst_l), .clk(sb_free_clk), .*);
   assign sb_cmd_awvalid     = ((sb_state == CMD_WR) | (sb_state == CMD_WR_ADDR));
   assign sb_cmd_wvalid      = ((sb_state == CMD_WR) | (sb_state == CMD_WR_DATA));
   assign sb_cmd_arvalid     = (sb_state == CMD_RD);
   assign sb_read_pend       = (sb_state == RSP_RD);
   assign sb_cmd_size[2:0]   = sbcs_reg[19:17];
   assign sb_cmd_wdata[63:0] = {sbdata1_reg[31:0], sbdata0_reg[31:0]};
   assign sb_cmd_addr[31:0]  = sbaddress0_reg[31:0];
   assign sb_axi_size[2:0]    = (sb_abmem_cmd_awvalid | sb_abmem_cmd_wvalid | sb_abmem_cmd_arvalid | sb_abmem_read_pend) ? sb_abmem_cmd_size[2:0] : sb_cmd_size[2:0];
   assign sb_axi_addr[31:0]   = (sb_abmem_cmd_awvalid | sb_abmem_cmd_wvalid | sb_abmem_cmd_arvalid | sb_abmem_read_pend) ? sb_abmem_cmd_addr[31:0] : sb_cmd_addr[31:0];
   assign sb_axi_wrdata[63:0] = (sb_abmem_cmd_awvalid | sb_abmem_cmd_wvalid) ? {2{sb_abmem_cmd_wdata[31:0]}} : sb_cmd_wdata[63:0];
   assign sb_axi_awvalid              = sb_abmem_cmd_awvalid | sb_cmd_awvalid;
   assign sb_axi_awaddr[31:0]         = sb_axi_addr[31:0];
   assign sb_axi_awid[SB_BUS_TAG-1:0] = '0;
   assign sb_axi_awsize[2:0]          = sb_axi_size[2:0];
   assign sb_axi_awprot[2:0]          = '0;
   assign sb_axi_awcache[3:0]         = 4'b1111;
   assign sb_axi_awregion[3:0]        = sb_axi_addr[31:28];
   assign sb_axi_awlen[7:0]           = '0;
   assign sb_axi_awburst[1:0]         = 2'b01;
   assign sb_axi_awqos[3:0]           = '0;
   assign sb_axi_awlock               = '0;
   assign sb_axi_wvalid       = sb_abmem_cmd_wvalid | sb_cmd_wvalid;
   assign sb_axi_wdata[63:0]  = ({64{(sb_axi_size[2:0] == 3'h0)}} & {8{sb_axi_wrdata[7:0]}}) |
                                ({64{(sb_axi_size[2:0] == 3'h1)}} & {4{sb_axi_wrdata[15:0]}}) |
                                ({64{(sb_axi_size[2:0] == 3'h2)}} & {2{sb_axi_wrdata[31:0]}}) |
                                ({64{(sb_axi_size[2:0] == 3'h3)}} & {sb_axi_wrdata[63:0]});
   assign sb_axi_wstrb[7:0]   = ({8{(sb_axi_size[2:0] == 3'h0)}} & (8'h1 << sb_axi_addr[2:0])) |
                                ({8{(sb_axi_size[2:0] == 3'h1)}} & (8'h3 << {sb_axi_addr[2:1],1'b0})) |
                                ({8{(sb_axi_size[2:0] == 3'h2)}} & (8'hf << {sb_axi_addr[2],2'b0})) |
                                ({8{(sb_axi_size[2:0] == 3'h3)}} & 8'hff);
   assign sb_axi_wlast        = '1;
   assign sb_axi_arvalid              = sb_abmem_cmd_arvalid | sb_cmd_arvalid;
   assign sb_axi_araddr[31:0]         = sb_axi_addr[31:0];
   assign sb_axi_arid[SB_BUS_TAG-1:0] = '0;
   assign sb_axi_arsize[2:0]          = sb_axi_size[2:0];
   assign sb_axi_arprot[2:0]          = '0;
   assign sb_axi_arcache[3:0]         = 4'b0;
   assign sb_axi_arregion[3:0]        = sb_axi_addr[31:28];
   assign sb_axi_arlen[7:0]           = '0;
   assign sb_axi_arburst[1:0]         = 2'b01;
   assign sb_axi_arqos[3:0]           = '0;
   assign sb_axi_arlock               = '0;
   assign sb_axi_bready = 1'b1;
   assign sb_axi_rready = 1'b1;
   assign sb_bus_rdata[63:0] = ({64{sb_axi_size == 3'h0}} & ((sb_axi_rdata[63:0] >>  8*sb_axi_addr[2:0]) & 64'hff))       |
                               ({64{sb_axi_size == 3'h1}} & ((sb_axi_rdata[63:0] >> 16*sb_axi_addr[2:1]) & 64'hffff))    |
                               ({64{sb_axi_size == 3'h2}} & ((sb_axi_rdata[63:0] >> 32*sb_axi_addr[2]) & 64'hffff_ffff)) |
                               ({64{sb_axi_size == 3'h3}} & sb_axi_rdata[63:0]);
endmodule
module dmi_wrapper(
  input              scan_mode,            
  input              trst_n,               
  input              tck,                  
  input              tms,                  
  input              tdi,                  
  output             tdo,                  
  output             tdoEnable,            
  input              core_rst_n,           
  input              core_clk,             
  input [31:1]       jtag_id,              
  input [31:0]       rd_data,              
  output [31:0]      reg_wr_data,          
  output [6:0]       reg_wr_addr,          
  output             reg_en,               
  output             reg_wr_en,            
  output             dmi_hard_reset  
);
  wire                     rd_en;
  wire                     wr_en;
  wire                     dmireset;
 rvjtag_tap i_jtag_tap(
   .trst(trst_n),                       
   .tck(tck),                           
   .tms(tms),                           
   .tdi(tdi),                           
   .tdo(tdo),                           
   .tdoEnable(tdoEnable),               
   .wr_data(reg_wr_data),               
   .wr_addr(reg_wr_addr),               
   .rd_en(rd_en),                       
   .wr_en(wr_en),                       
   .rd_data(rd_data),                   
   .rd_status(2'b0),
   .idle(3'h0),                          
   .dmi_stat(2'b0),                      
   .version(4'h1),                       
   .jtag_id(jtag_id),
   .dmi_hard_reset(dmi_hard_reset),
   .dmi_reset(dmireset)
);
  dmi_jtag_to_core_sync i_dmi_jtag_to_core_sync(
    .wr_en(wr_en),                           
    .rd_en(rd_en),                           
    .rst_n(core_rst_n),
    .clk(core_clk),
    .reg_en(reg_en),                           
    .reg_wr_en(reg_wr_en)                           
  );
endmodule
module dmi_jtag_to_core_sync (
   input                       rd_en,        
   input                       wr_en,        
   input                       rst_n,        
   input                       clk,          
   output                      reg_en,       
   output                      reg_wr_en     
);
  wire                         c_rd_en;
  wire                         c_wr_en;
  assign reg_en = c_wr_en | c_rd_en;
  assign reg_wr_en = c_wr_en;
  reg [2:0] rden, wren;
always @ ( posedge clk or negedge rst_n) begin
    if(!rst_n) begin
        rden <= 3'b0;
        wren <= 3'b0;
    end
    else begin
        rden <= {rden[1:0], rd_en};
        wren <= {wren[1:0], wr_en};
    end
end
assign c_rd_en = rden[1] & ~rden[2];
assign c_wr_en = wren[1] & ~wren[2];
endmodule
module rvjtag_tap #(
parameter AWIDTH = 7
)
(
input               trst,
input               tck,
input               tms,
input               tdi,
output   reg        tdo,
output              tdoEnable,
output [31:0]       wr_data,
output [AWIDTH-1:0] wr_addr,
output              wr_en,
output              rd_en,
input   [31:0]      rd_data,
input   [1:0]       rd_status,
output  reg         dmi_reset,
output  reg         dmi_hard_reset,
input   [2:0]       idle,
input   [1:0]       dmi_stat,
input   [31:1]      jtag_id,
input   [3:0]       version
);
localparam USER_DR_LENGTH = AWIDTH + 34;
reg [USER_DR_LENGTH-1:0] sr, nsr, dr;
logic[3:0] state, nstate;
logic [4:0] ir;
wire jtag_reset;
wire shift_dr;
wire pause_dr;
wire update_dr;
wire capture_dr;
wire shift_ir;
wire pause_ir ;
wire update_ir ;
wire capture_ir;
wire[1:0] dr_en;
wire devid_sel;
wire [5:0] abits;
assign abits = AWIDTH[5:0];
localparam TEST_LOGIC_RESET_STATE = 0;
localparam RUN_TEST_IDLE_STATE    = 1;
localparam SELECT_DR_SCAN_STATE   = 2;
localparam CAPTURE_DR_STATE       = 3;
localparam SHIFT_DR_STATE         = 4;
localparam EXIT1_DR_STATE         = 5;
localparam PAUSE_DR_STATE         = 6;
localparam EXIT2_DR_STATE         = 7;
localparam UPDATE_DR_STATE        = 8;
localparam SELECT_IR_SCAN_STATE   = 9;
localparam CAPTURE_IR_STATE       = 10;
localparam SHIFT_IR_STATE         = 11;
localparam EXIT1_IR_STATE         = 12;
localparam PAUSE_IR_STATE         = 13;
localparam EXIT2_IR_STATE         = 14;
localparam UPDATE_IR_STATE        = 15;
always_comb  begin
    nstate = state;
    case(state)
    TEST_LOGIC_RESET_STATE: nstate = tms ? TEST_LOGIC_RESET_STATE : RUN_TEST_IDLE_STATE;
    RUN_TEST_IDLE_STATE:    nstate = tms ? SELECT_DR_SCAN_STATE   : RUN_TEST_IDLE_STATE;
    SELECT_DR_SCAN_STATE:   nstate = tms ? SELECT_IR_SCAN_STATE   : CAPTURE_DR_STATE;
    CAPTURE_DR_STATE:       nstate = tms ? EXIT1_DR_STATE         : SHIFT_DR_STATE;
    SHIFT_DR_STATE:         nstate = tms ? EXIT1_DR_STATE         : SHIFT_DR_STATE;
    EXIT1_DR_STATE:         nstate = tms ? UPDATE_DR_STATE        : PAUSE_DR_STATE;
    PAUSE_DR_STATE:         nstate = tms ? EXIT2_DR_STATE         : PAUSE_DR_STATE;
    EXIT2_DR_STATE:         nstate = tms ? UPDATE_DR_STATE        : SHIFT_DR_STATE;
    UPDATE_DR_STATE:        nstate = tms ? SELECT_DR_SCAN_STATE   : RUN_TEST_IDLE_STATE;
    SELECT_IR_SCAN_STATE:   nstate = tms ? TEST_LOGIC_RESET_STATE : CAPTURE_IR_STATE;
    CAPTURE_IR_STATE:       nstate = tms ? EXIT1_IR_STATE         : SHIFT_IR_STATE;
    SHIFT_IR_STATE:         nstate = tms ? EXIT1_IR_STATE         : SHIFT_IR_STATE;
    EXIT1_IR_STATE:         nstate = tms ? UPDATE_IR_STATE        : PAUSE_IR_STATE;
    PAUSE_IR_STATE:         nstate = tms ? EXIT2_IR_STATE         : PAUSE_IR_STATE;
    EXIT2_IR_STATE:         nstate = tms ? UPDATE_IR_STATE        : SHIFT_IR_STATE;
    UPDATE_IR_STATE:        nstate = tms ? SELECT_DR_SCAN_STATE   : RUN_TEST_IDLE_STATE;
    default:                nstate = TEST_LOGIC_RESET_STATE;
    endcase
end
always @ (posedge tck or negedge trst) begin
    if(!trst) state <= TEST_LOGIC_RESET_STATE;
    else state <= nstate;
end
assign jtag_reset = state == TEST_LOGIC_RESET_STATE;
assign shift_dr   = state == SHIFT_DR_STATE;
assign pause_dr   = state == PAUSE_DR_STATE;
assign update_dr  = state == UPDATE_DR_STATE;
assign capture_dr = state == CAPTURE_DR_STATE;
assign shift_ir   = state == SHIFT_IR_STATE;
assign pause_ir   = state == PAUSE_IR_STATE;
assign update_ir  = state == UPDATE_IR_STATE;
assign capture_ir = state == CAPTURE_IR_STATE;
assign tdoEnable = shift_dr | shift_ir;
always @ (negedge tck or negedge trst) begin
   if (!trst) ir <= 5'b1;
   else begin
      if (jtag_reset) ir <= 5'b1;
      else if (update_ir) ir <= (sr[4:0] == '0) ? 5'h1f :sr[4:0];
   end
end
assign devid_sel  = ir == 5'b00001;
assign dr_en[0]   = ir == 5'b10000;
assign dr_en[1]   = ir == 5'b10001;
always @ (posedge tck or negedge trst) begin
    if(!trst)begin
        sr <= '0;
    end
    else begin
        sr <= nsr;
    end
end
always_comb begin
    nsr = sr;
    case(1)
    shift_dr:   begin
                    case(1)
                    dr_en[1]:   nsr = {tdi, sr[USER_DR_LENGTH-1:1]};
                    dr_en[0],
                    devid_sel:  nsr = {{USER_DR_LENGTH-32{1'b0}},tdi, sr[31:1]};
                    default:    nsr = {{USER_DR_LENGTH-1{1'b0}},tdi};  
                    endcase
                end
    capture_dr: begin
                    nsr[0] = 1'b0;
                    case(1)
                    dr_en[0]:   nsr = {{USER_DR_LENGTH-15{1'b0}}, idle, dmi_stat, abits, version};
                    dr_en[1]:   nsr = {{AWIDTH{1'b0}}, rd_data, rd_status};
                    devid_sel:  nsr = {{USER_DR_LENGTH-32{1'b0}}, jtag_id, 1'b1};
                    endcase
                end
    shift_ir:   nsr = {{USER_DR_LENGTH-5{1'b0}},tdi, sr[4:1]};
    capture_ir: nsr = {{USER_DR_LENGTH-1{1'b0}},1'b1};
    endcase
end
always @ (negedge tck ) tdo <= sr[0];
always @ (posedge tck or negedge trst) begin
    if(!trst) begin
        dmi_hard_reset <= 1'b0;
        dmi_reset      <= 1'b0;
    end
    else if (update_dr & dr_en[0]) begin
        dmi_hard_reset <= sr[17];
        dmi_reset      <= sr[16];
    end
    else begin
        dmi_hard_reset <= 1'b0;
        dmi_reset      <= 1'b0;
    end
end
always @ (posedge tck or negedge trst) begin
    if(!trst)
        dr <=  '0;
    else begin
        if (update_dr & dr_en[1])
            dr <= sr;
        else
            dr <= {dr[USER_DR_LENGTH-1:2],2'b0};
    end
end
assign {wr_addr, wr_data, wr_en, rd_en} = dr;
endmodule
module ahb_to_axi4 #(parameter TAG  = 1) (
   input                   clk,
   input                   rst_l,
   input                   scan_mode,
   input                   bus_clk_en,
   input                   clk_override,
   output logic            axi_awvalid,
   input  logic            axi_awready,
   output logic [TAG-1:0]  axi_awid,
   output logic [31:0]     axi_awaddr,
   output logic [2:0]      axi_awsize,
   output logic [2:0]      axi_awprot,
   output logic [7:0]      axi_awlen,
   output logic [1:0]      axi_awburst,
   output logic            axi_wvalid,
   input  logic            axi_wready,
   output logic [63:0]     axi_wdata,
   output logic [7:0]      axi_wstrb,
   output logic            axi_wlast,
   input  logic            axi_bvalid,
   output logic            axi_bready,
   input  logic [1:0]      axi_bresp,
   input  logic [TAG-1:0]  axi_bid,
   output logic            axi_arvalid,
   input  logic            axi_arready,
   output logic [TAG-1:0]  axi_arid,
   output logic [31:0]     axi_araddr,
   output logic [2:0]      axi_arsize,
   output logic [2:0]      axi_arprot,
   output logic [7:0]      axi_arlen,
   output logic [1:0]      axi_arburst,
   input  logic            axi_rvalid,
   output logic            axi_rready,
   input  logic [TAG-1:0]  axi_rid,
   input  logic [63:0]     axi_rdata,
   input  logic [1:0]      axi_rresp,
   input logic [31:0]      ahb_haddr,      
   input logic [2:0]       ahb_hburst,     
   input logic             ahb_hmastlock,  
   input logic [3:0]       ahb_hprot,      
   input logic [2:0]       ahb_hsize,      
   input logic [1:0]       ahb_htrans,     
   input logic             ahb_hwrite,     
   input logic [63:0]      ahb_hwdata,     
   input logic             ahb_hsel,       
   input logic             ahb_hreadyin,   
   output logic [63:0]      ahb_hrdata,       
   output logic             ahb_hreadyout,    
   output logic             ahb_hresp         
);
   logic [7:0]       master_wstrb;
 typedef enum logic [1:0] {   IDLE   = 2'b00,     
                              WR     = 2'b01,     
                              RD     = 2'b10,     
                              PEND   = 2'b11      
                            } state_t;
   state_t      buf_state, buf_nxtstate;
   logic        buf_state_en;
   logic                    buf_read_error_in, buf_read_error;
   logic [63:0]             buf_rdata;
   logic                    ahb_hready;
   logic                    ahb_hready_q;
   logic [1:0]              ahb_htrans_in, ahb_htrans_q;
   logic [2:0]              ahb_hsize_q;
   logic                    ahb_hwrite_q;
   logic [31:0]             ahb_haddr_q;
   logic [63:0]             ahb_hwdata_q;
   logic                    ahb_hresp_q;
   logic                    ahb_addr_in_dccm, ahb_addr_in_iccm, ahb_addr_in_pic;
   logic                    ahb_addr_in_dccm_region_nc, ahb_addr_in_iccm_region_nc, ahb_addr_in_pic_region_nc;
   logic                    buf_rdata_en;
   logic                    ahb_bus_addr_clk_en, buf_rdata_clk_en;
   logic                    ahb_clk, ahb_addr_clk, buf_rdata_clk;
   logic                    cmdbuf_wr_en, cmdbuf_rst;
   logic                    cmdbuf_full;
   logic                    cmdbuf_vld, cmdbuf_write;
   logic [1:0]              cmdbuf_size;
   logic [7:0]              cmdbuf_wstrb;
   logic [31:0]             cmdbuf_addr;
   logic [63:0]             cmdbuf_wdata;
   logic                    bus_clk;
   always_comb begin
      buf_nxtstate      = IDLE;
      buf_state_en      = 1'b0;
      buf_rdata_en      = 1'b0;               
      buf_read_error_in = 1'b0;               
      cmdbuf_wr_en      = 1'b0;               
      case (buf_state)
         IDLE: begin   
                  buf_nxtstate      = ahb_hwrite ? WR : RD;
                  buf_state_en      = ahb_hready & ahb_htrans[1] & ahb_hsel;                  
          end
         WR: begin  
                  buf_nxtstate      = (ahb_hresp | (ahb_htrans[1:0] == 2'b0) | ~ahb_hsel) ? IDLE : (ahb_hwrite ? WR : RD);
                  buf_state_en      = (~cmdbuf_full | ahb_hresp) ;
                  cmdbuf_wr_en      = ~cmdbuf_full & ~(ahb_hresp | ((ahb_htrans[1:0] == 2'b01) & ahb_hsel));    
         end
         RD: begin  
                 buf_nxtstate      = ahb_hresp ? IDLE :PEND;                                        
                 buf_state_en      = (~cmdbuf_full | ahb_hresp);                                    
                 cmdbuf_wr_en      = ~ahb_hresp & ~cmdbuf_full;                                     
         end
         PEND: begin  
                 buf_nxtstate      = IDLE;                                                           
                 buf_state_en      = axi_rvalid & ~cmdbuf_write;                                     
                 buf_rdata_en      = buf_state_en;                                                   
                 buf_read_error_in = buf_state_en & |axi_rresp[1:0];                                 
         end
     endcase
   end  
   assign master_wstrb[7:0]   = ({8{ahb_hsize_q[2:0] == 3'b0}}  & (8'b1    << ahb_haddr_q[2:0])) |
                                ({8{ahb_hsize_q[2:0] == 3'b1}}  & (8'b11   << ahb_haddr_q[2:0])) |
                                ({8{ahb_hsize_q[2:0] == 3'b10}} & (8'b1111 << ahb_haddr_q[2:0])) |
                                ({8{ahb_hsize_q[2:0] == 3'b11}} & 8'b1111_1111);
   assign ahb_hreadyout       = ahb_hresp ? (ahb_hresp_q & ~ahb_hready_q) :
                                         ((~cmdbuf_full | (buf_state == IDLE)) & ~(buf_state == RD | buf_state == PEND)  & ~buf_read_error);
   assign ahb_hready          = ahb_hreadyout & ahb_hreadyin;
   assign ahb_htrans_in[1:0]  = {2{ahb_hsel}} & ahb_htrans[1:0];
   assign ahb_hrdata[63:0]    = buf_rdata[63:0];
   assign ahb_hresp        = ((ahb_htrans_q[1:0] != 2'b0) & (buf_state != IDLE)  &
                             ((~(ahb_addr_in_dccm | ahb_addr_in_iccm)) |                                                                                    
                             ((ahb_addr_in_iccm | (ahb_addr_in_dccm &  ahb_hwrite_q)) & ~((ahb_hsize_q[1:0] == 2'b10) | (ahb_hsize_q[1:0] == 2'b11))) |     
                             ((ahb_hsize_q[2:0] == 3'h1) & ahb_haddr_q[0])   |                                                                              
                             ((ahb_hsize_q[2:0] == 3'h2) & (|ahb_haddr_q[1:0])) |                                                                           
                             ((ahb_hsize_q[2:0] == 3'h3) & (|ahb_haddr_q[2:0])))) |                                                                         
                             buf_read_error |                                                                                                               
                             (ahb_hresp_q & ~ahb_hready_q);                                                                                                 
   rvdff_fpga  #(.WIDTH(1))    hresp_ff  (.din(ahb_hresp),          .dout(ahb_hresp_q),       .clk(ahb_clk),      .clken(bus_clk_en), .rawclk(clk), .*);
   rvdff_fpga  #(.WIDTH(1))    hready_ff (.din(ahb_hready),         .dout(ahb_hready_q),      .clk(ahb_clk),      .clken(bus_clk_en), .rawclk(clk), .*);
   rvdff_fpga  #(.WIDTH(2))    htrans_ff (.din(ahb_htrans_in[1:0]), .dout(ahb_htrans_q[1:0]), .clk(ahb_clk),      .clken(bus_clk_en), .rawclk(clk), .*);
   rvdff_fpga  #(.WIDTH(3))    hsize_ff  (.din(ahb_hsize[2:0]),     .dout(ahb_hsize_q[2:0]),  .clk(ahb_addr_clk), .clken(ahb_bus_addr_clk_en), .rawclk(clk), .*);
   rvdff_fpga  #(.WIDTH(1))    hwrite_ff (.din(ahb_hwrite),         .dout(ahb_hwrite_q),      .clk(ahb_addr_clk), .clken(ahb_bus_addr_clk_en), .rawclk(clk), .*);
   rvdff_fpga  #(.WIDTH(32))   haddr_ff  (.din(ahb_haddr[31:0]),    .dout(ahb_haddr_q[31:0]), .clk(ahb_addr_clk), .clken(ahb_bus_addr_clk_en), .rawclk(clk), .*);
   rvdffs_fpga #($bits(state_t)) state_reg (.din(buf_nxtstate),     .dout({buf_state}), .en(buf_state_en), .clk(ahb_clk), .clken(bus_clk_en),  .rawclk(clk), .*);
   rvdff_fpga  #(.WIDTH(64)) buf_rdata_ff  (.din(axi_rdata[63:0]),  .dout(buf_rdata[63:0]),   .clk(buf_rdata_clk), .clken(buf_rdata_clk_en),   .rawclk(clk), .*);
   rvdff_fpga  #(.WIDTH(1))  buf_read_error_ff(.din(buf_read_error_in),  .dout(buf_read_error),.clk(ahb_clk),       .clken(bus_clk_en),        .rawclk(clk), .*);
   assign ahb_bus_addr_clk_en = bus_clk_en & (ahb_hready & ahb_htrans[1]);
   assign buf_rdata_clk_en    = bus_clk_en & buf_rdata_en;
   assign ahb_clk = 1'b0;
   assign ahb_addr_clk = 1'b0;
   assign buf_rdata_clk = 1'b0;
   assign bus_clk = 1'b0;
   rvrangecheck #(.CCM_SADR(32'hf0040000),
                  .CCM_SIZE(64)) addr_dccm_rangecheck (
      .addr(ahb_haddr_q[31:0]),
      .in_range(ahb_addr_in_dccm),
      .in_region(ahb_addr_in_dccm_region_nc)
   );
   assign ahb_addr_in_iccm = '0;
   assign ahb_addr_in_iccm_region_nc = '0;
   rvrangecheck #(.CCM_SADR(32'hf00c0000),
                  .CCM_SIZE(32)) addr_pic_rangecheck (
      .addr(ahb_haddr_q[31:0]),
      .in_range(ahb_addr_in_pic),
      .in_region(ahb_addr_in_pic_region_nc)
   );
   assign cmdbuf_rst         = (((axi_awvalid & axi_awready) | (axi_arvalid & axi_arready)) & ~cmdbuf_wr_en) | (ahb_hresp & ~cmdbuf_write);
   assign cmdbuf_full        = (cmdbuf_vld & ~((axi_awvalid & axi_awready) | (axi_arvalid & axi_arready)));
   rvdffsc_fpga #(.WIDTH(1))   cmdbuf_vldff      (.din(1'b1), .clear(cmdbuf_rst), .dout(cmdbuf_vld),        .en(cmdbuf_wr_en), .clk(bus_clk), .clken(bus_clk_en), .rawclk(clk), .*);
   rvdffs_fpga  #(.WIDTH(1))   cmdbuf_writeff    (.din(ahb_hwrite_q),             .dout(cmdbuf_write),      .en(cmdbuf_wr_en), .clk(bus_clk), .clken(bus_clk_en), .rawclk(clk), .*);
   rvdffs_fpga  #(.WIDTH(2))   cmdbuf_sizeff     (.din(ahb_hsize_q[1:0]),         .dout(cmdbuf_size[1:0]),  .en(cmdbuf_wr_en), .clk(bus_clk), .clken(bus_clk_en), .rawclk(clk), .*);
   rvdffs_fpga  #(.WIDTH(8))   cmdbuf_wstrbff    (.din(master_wstrb[7:0]),        .dout(cmdbuf_wstrb[7:0]), .en(cmdbuf_wr_en), .clk(bus_clk), .clken(bus_clk_en), .rawclk(clk), .*);
   rvdffe  #(.WIDTH(32))  cmdbuf_addrff     (.din(ahb_haddr_q[31:0]),   .dout(cmdbuf_addr[31:0]),   .en(cmdbuf_wr_en & bus_clk_en), .*);
   rvdffe  #(.WIDTH(64))  cmdbuf_wdataff    (.din(ahb_hwdata[63:0]),    .dout(cmdbuf_wdata[63:0]),  .en(cmdbuf_wr_en & bus_clk_en), .*);
   assign axi_awvalid           = cmdbuf_vld & cmdbuf_write;
   assign axi_awid[TAG-1:0]     = '0;
   assign axi_awaddr[31:0]      = cmdbuf_addr[31:0];
   assign axi_awsize[2:0]       = {1'b0, cmdbuf_size[1:0]};
   assign axi_awprot[2:0]       = 3'b0;
   assign axi_awlen[7:0]        = '0;
   assign axi_awburst[1:0]      = 2'b01;
   assign axi_wvalid            = cmdbuf_vld & cmdbuf_write;
   assign axi_wdata[63:0]       = cmdbuf_wdata[63:0];
   assign axi_wstrb[7:0]        = cmdbuf_wstrb[7:0];
   assign axi_wlast             = 1'b1;
   assign axi_bready            = 1'b1;
   assign axi_arvalid           = cmdbuf_vld & ~cmdbuf_write;
   assign axi_arid[TAG-1:0]     = '0;
   assign axi_araddr[31:0]      = cmdbuf_addr[31:0];
   assign axi_arsize[2:0]       = {1'b0, cmdbuf_size[1:0]};
   assign axi_arprot            = 3'b0;
   assign axi_arlen[7:0]        = '0;
   assign axi_arburst[1:0]      = 2'b01;
   assign axi_rready            = 1'b1;
endmodule  
module axi4_to_ahb #(parameter TAG  = 1) (
   input                   clk,
   input                   rst_l,
   input                   scan_mode,
   input                   bus_clk_en,
   input                   clk_override,
   input  logic            axi_awvalid,
   output logic            axi_awready,
   input  logic [TAG-1:0]  axi_awid,
   input  logic [31:0]     axi_awaddr,
   input  logic [2:0]      axi_awsize,
   input  logic [2:0]      axi_awprot,
   input  logic            axi_wvalid,
   output logic            axi_wready,
   input  logic [63:0]     axi_wdata,
   input  logic [7:0]      axi_wstrb,
   input  logic            axi_wlast,
   output logic            axi_bvalid,
   input  logic            axi_bready,
   output logic [1:0]      axi_bresp,
   output logic [TAG-1:0]  axi_bid,
   input  logic            axi_arvalid,
   output logic            axi_arready,
   input  logic [TAG-1:0]  axi_arid,
   input  logic [31:0]     axi_araddr,
   input  logic [2:0]      axi_arsize,
   input  logic [2:0]      axi_arprot,
   output logic            axi_rvalid,
   input  logic            axi_rready,
   output logic [TAG-1:0]  axi_rid,
   output logic [63:0]     axi_rdata,
   output logic [1:0]      axi_rresp,
   output logic            axi_rlast,
   output logic [31:0]     ahb_haddr,        
   output logic [2:0]      ahb_hburst,       
   output logic            ahb_hmastlock,    
   output logic [3:0]      ahb_hprot,        
   output logic [2:0]      ahb_hsize,        
   output logic [1:0]      ahb_htrans,       
   output logic            ahb_hwrite,       
   output logic [63:0]     ahb_hwdata,       
   input logic [63:0]      ahb_hrdata,       
   input logic             ahb_hready,       
   input logic             ahb_hresp         
);
   localparam ID   = 1;
   localparam PRTY = 1;
   typedef enum logic [2:0] {IDLE=3'b000, CMD_RD=3'b001, CMD_WR=3'b010, DATA_RD=3'b011, DATA_WR=3'b100, DONE=3'b101, STREAM_RD=3'b110, STREAM_ERR_RD=3'b111} state_t;
   state_t buf_state, buf_nxtstate;
   logic             slave_valid;
   logic             slave_ready;
   logic [TAG-1:0]   slave_tag;
   logic [63:0]      slave_rdata;
   logic [3:0]       slave_opc;
   logic             wrbuf_en, wrbuf_data_en;
   logic             wrbuf_cmd_sent, wrbuf_rst;
   logic             wrbuf_vld;
   logic             wrbuf_data_vld;
   logic [TAG-1:0]   wrbuf_tag;
   logic [2:0]       wrbuf_size;
   logic [31:0]      wrbuf_addr;
   logic [63:0]      wrbuf_data;
   logic [7:0]       wrbuf_byteen;
   logic             bus_write_clk_en;
   logic             bus_clk, bus_write_clk;
   logic             master_valid;
   logic             master_ready;
   logic [TAG-1:0]   master_tag;
   logic [31:0]      master_addr;
   logic [63:0]      master_wdata;
   logic [2:0]       master_size;
   logic [2:0]       master_opc;
   logic [31:0]                buf_addr;
   logic [1:0]                 buf_size;
   logic                       buf_write;
   logic [7:0]                 buf_byteen;
   logic                       buf_aligned;
   logic [63:0]                buf_data;
   logic [TAG-1:0]             buf_tag;
   logic                       buf_rst;
   logic [TAG-1:0]             buf_tag_in;
   logic [31:0]                buf_addr_in;
   logic [7:0]                 buf_byteen_in;
   logic [63:0]                buf_data_in;
   logic                       buf_write_in;
   logic                       buf_aligned_in;
   logic [2:0]                 buf_size_in;
   logic                       buf_state_en;
   logic                       buf_wr_en;
   logic                       buf_data_wr_en;
   logic                       slvbuf_error_en;
   logic                       wr_cmd_vld;
   logic                       cmd_done_rst, cmd_done, cmd_doneQ;
   logic                       trxn_done;
   logic [2:0]                 buf_cmd_byte_ptr, buf_cmd_byte_ptrQ, buf_cmd_nxtbyte_ptr;
   logic                       buf_cmd_byte_ptr_en;
   logic                       found;
   logic                       slave_valid_pre;
   logic                       ahb_hready_q;
   logic                       ahb_hresp_q;
   logic [1:0]                 ahb_htrans_q;
   logic                       ahb_hwrite_q;
   logic [63:0]                ahb_hrdata_q;
   logic                       slvbuf_write;
   logic                       slvbuf_error;
   logic [TAG-1:0]             slvbuf_tag;
   logic                       slvbuf_error_in;
   logic                       slvbuf_wr_en;
   logic                       bypass_en;
   logic                       rd_bypass_idle;
   logic                       last_addr_en;
   logic [31:0]                last_bus_addr;
   logic                       buf_clken, slvbuf_clken;
   logic                       ahbm_addr_clken;
   logic                       ahbm_data_clken;
   logic                       buf_clk, slvbuf_clk;
   logic                       ahbm_clk;
   logic                       ahbm_addr_clk;
   logic                       ahbm_data_clk;
   function automatic logic [1:0] get_write_size;
      input logic [7:0] byteen;
      logic [1:0]       size;
      size[1:0] = (2'b11 & {2{(byteen[7:0] == 8'hff)}}) |
                  (2'b10 & {2{((byteen[7:0] == 8'hf0) | (byteen[7:0] == 8'h0f))}}) |
                  (2'b01 & {2{((byteen[7:0] == 8'hc0) | (byteen[7:0] == 8'h30) | (byteen[7:0] == 8'h0c) | (byteen[7:0] == 8'h03))}});
      return size[1:0];
   endfunction  
   function automatic logic [2:0] get_write_addr;
      input logic [7:0] byteen;
      logic [2:0]       addr;
      addr[2:0] = (3'h0 & {3{((byteen[7:0] == 8'hff) | (byteen[7:0] == 8'h0f) | (byteen[7:0] == 8'h03))}}) |
                  (3'h2 & {3{(byteen[7:0] == 8'h0c)}})                                                     |
                  (3'h4 & {3{((byteen[7:0] == 8'hf0) | (byteen[7:0] == 8'h03))}})                          |
                  (3'h6 & {3{(byteen[7:0] == 8'hc0)}});
      return addr[2:0];
   endfunction  
   function automatic logic [2:0] get_nxtbyte_ptr (logic [2:0] current_byte_ptr, logic [7:0] byteen, logic get_next);
      logic [2:0] start_ptr;
      logic       found;
      found = '0;
      start_ptr[2:0] = get_next ? (current_byte_ptr[2:0] + 3'b1) : current_byte_ptr[2:0];
      for (int j=0; j<8; j++) begin
         if (~found) begin
            get_nxtbyte_ptr[2:0] = 3'(j);
            found |= (byteen[j] & (3'(j) >= start_ptr[2:0])) ;
         end
      end
   endfunction  
   assign wrbuf_en       = axi_awvalid & axi_awready & master_ready;
   assign wrbuf_data_en  = axi_wvalid & axi_wready & master_ready;
   assign wrbuf_cmd_sent = master_valid & master_ready & (master_opc[2:1] == 2'b01);
   assign wrbuf_rst      = wrbuf_cmd_sent & ~wrbuf_en;
   assign axi_awready = ~(wrbuf_vld & ~wrbuf_cmd_sent) & master_ready;
   assign axi_wready  = ~(wrbuf_data_vld & ~wrbuf_cmd_sent) & master_ready;
   assign axi_arready = ~(wrbuf_vld & wrbuf_data_vld) & master_ready;
   assign axi_rlast   = 1'b1;
   assign wr_cmd_vld          = (wrbuf_vld & wrbuf_data_vld);
   assign master_valid          = wr_cmd_vld | axi_arvalid;
   assign master_tag[TAG-1:0]   = wr_cmd_vld ? wrbuf_tag[TAG-1:0] : axi_arid[TAG-1:0];
   assign master_opc[2:0]       = wr_cmd_vld ? 3'b011 : 3'b0;
   assign master_addr[31:0]     = wr_cmd_vld ? wrbuf_addr[31:0] : axi_araddr[31:0];
   assign master_size[2:0]    = wr_cmd_vld ? wrbuf_size[2:0] : axi_arsize[2:0];
   assign master_wdata[63:0]    = wrbuf_data[63:0];
   assign axi_bvalid       = slave_valid & slave_ready & slave_opc[3];
   assign axi_bresp[1:0]   = slave_opc[0] ? 2'b10 : (slave_opc[1] ? 2'b11 : 2'b0);
   assign axi_bid[TAG-1:0] = slave_tag[TAG-1:0];
   assign axi_rvalid       = slave_valid & slave_ready & (slave_opc[3:2] == 2'b0);
   assign axi_rresp[1:0]   = slave_opc[0] ? 2'b10 : (slave_opc[1] ? 2'b11 : 2'b0);
   assign axi_rid[TAG-1:0] = slave_tag[TAG-1:0];
   assign axi_rdata[63:0]  = slave_rdata[63:0];
   assign slave_ready        = axi_bready & axi_rready;
   assign bus_write_clk_en = bus_clk_en & ((axi_awvalid & axi_awready) | (axi_wvalid & axi_wready));
   assign bus_clk = 1'b0;
   assign bus_write_clk = 1'b0;
   assign buf_clk = 1'b0;
   assign ahbm_clk = 1'b0;
   assign ahbm_addr_clk = 1'b0;
   assign ahbm_data_clk = 1'b0;
   always_comb begin
      buf_nxtstate   = IDLE;
      buf_state_en   = 1'b0;
      buf_wr_en      = 1'b0;
      buf_data_wr_en = 1'b0;
      slvbuf_error_in   = 1'b0;
      slvbuf_error_en   = 1'b0;
      buf_write_in   = 1'b0;
      cmd_done       = 1'b0;
      trxn_done      = 1'b0;
      buf_cmd_byte_ptr_en = 1'b0;
      buf_cmd_byte_ptr[2:0] = '0;
      slave_valid_pre   = 1'b0;
      master_ready   = 1'b0;
      ahb_htrans[1:0]  = 2'b0;
      slvbuf_wr_en     = 1'b0;
      bypass_en        = 1'b0;
      rd_bypass_idle   = 1'b0;
      case (buf_state)
         IDLE: begin
                  master_ready   = 1'b1;
                  buf_write_in = (master_opc[2:1] == 2'b01);
                  buf_nxtstate = buf_write_in ? CMD_WR : CMD_RD;
                  buf_state_en = master_valid & master_ready;
                  buf_wr_en    = buf_state_en;
                  buf_data_wr_en = buf_state_en & (buf_nxtstate == CMD_WR);
                  buf_cmd_byte_ptr_en   = buf_state_en;
                  buf_cmd_byte_ptr[2:0] = buf_write_in ? get_nxtbyte_ptr(3'b0,buf_byteen_in[7:0],1'b0) : master_addr[2:0];
                  bypass_en       = buf_state_en;
                  rd_bypass_idle  = bypass_en & (buf_nxtstate == CMD_RD);
                  ahb_htrans[1:0] = {2{bypass_en}} & 2'b10;
          end
         CMD_RD: begin
                  buf_nxtstate    = (master_valid & (master_opc[2:0] == 3'b000))? STREAM_RD : DATA_RD;
                  buf_state_en    = ahb_hready_q & (ahb_htrans_q[1:0] != 2'b0) & ~ahb_hwrite_q;
                  cmd_done        = buf_state_en & ~master_valid;
                  slvbuf_wr_en    = buf_state_en;
                  master_ready  = buf_state_en & (buf_nxtstate == STREAM_RD);
                  buf_wr_en       = master_ready;
                  bypass_en       = master_ready & master_valid;
                  buf_cmd_byte_ptr[2:0] = bypass_en ? master_addr[2:0] : buf_addr[2:0];
                  ahb_htrans[1:0] = 2'b10 & {2{~buf_state_en | bypass_en}};
         end
         STREAM_RD: begin
                  master_ready  =  (ahb_hready_q & ~ahb_hresp_q) & ~(master_valid & master_opc[2:1] == 2'b01);
                  buf_wr_en       = (master_valid & master_ready & (master_opc[2:0] == 3'b000));  
                  buf_nxtstate    = ahb_hresp_q ? STREAM_ERR_RD : (buf_wr_en ? STREAM_RD : DATA_RD);             
                  buf_state_en    = (ahb_hready_q | ahb_hresp_q);
                  buf_data_wr_en  = buf_state_en;
                  slvbuf_error_in = ahb_hresp_q;
                  slvbuf_error_en = buf_state_en;
                  slave_valid_pre  = buf_state_en & ~ahb_hresp_q;              
                  cmd_done        = buf_state_en & ~master_valid;                      
                  bypass_en       = master_ready & master_valid & (buf_nxtstate == STREAM_RD) & buf_state_en;
                  buf_cmd_byte_ptr[2:0] = bypass_en ? master_addr[2:0] : buf_addr[2:0];
                  ahb_htrans[1:0] = 2'b10 & {2{~((buf_nxtstate != STREAM_RD) & buf_state_en)}};
                  slvbuf_wr_en    = buf_wr_en;                                          
         end  
         STREAM_ERR_RD: begin
                  buf_nxtstate = DATA_RD;
                  buf_state_en = ahb_hready_q & (ahb_htrans_q[1:0] != 2'b0) & ~ahb_hwrite_q;
                  slave_valid_pre = buf_state_en;
                  slvbuf_wr_en   = buf_state_en;      
                  buf_cmd_byte_ptr[2:0] = buf_addr[2:0];
                  ahb_htrans[1:0] = 2'b10 & {2{~buf_state_en}};
         end
         DATA_RD: begin
                  buf_nxtstate   = DONE;
                  buf_state_en   = (ahb_hready_q | ahb_hresp_q);
                  buf_data_wr_en = buf_state_en;
                  slvbuf_error_in= ahb_hresp_q;
                  slvbuf_error_en= buf_state_en;
                  slvbuf_wr_en   = buf_state_en;
         end
         CMD_WR: begin
                  buf_nxtstate = DATA_WR;
                  trxn_done    = ahb_hready_q & ahb_hwrite_q & (ahb_htrans_q[1:0] != 2'b0);
                  buf_state_en = trxn_done;
                  buf_cmd_byte_ptr_en = buf_state_en;
                  slvbuf_wr_en    = buf_state_en;
                  buf_cmd_byte_ptr    = trxn_done ? get_nxtbyte_ptr(buf_cmd_byte_ptrQ[2:0],buf_byteen[7:0],1'b1) : buf_cmd_byte_ptrQ;
                  cmd_done            = trxn_done & (buf_aligned | (buf_cmd_byte_ptrQ == 3'b111) |
                                                     (buf_byteen[get_nxtbyte_ptr(buf_cmd_byte_ptrQ[2:0],buf_byteen[7:0],1'b1)] == 1'b0));
                  ahb_htrans[1:0] = {2{~(cmd_done | cmd_doneQ)}} & 2'b10;
         end
         DATA_WR: begin
                  buf_state_en = (cmd_doneQ & ahb_hready_q) | ahb_hresp_q;
                  master_ready = buf_state_en & ~ahb_hresp_q & slave_ready;    
                  buf_nxtstate = (ahb_hresp_q | ~slave_ready) ? DONE :
                                  ((master_valid & master_ready) ? ((master_opc[2:1] == 2'b01) ? CMD_WR : CMD_RD) : IDLE);
                  slvbuf_error_in = ahb_hresp_q;
                  slvbuf_error_en = buf_state_en;
                  buf_write_in = (master_opc[2:1] == 2'b01);
                  buf_wr_en = buf_state_en & ((buf_nxtstate == CMD_WR) | (buf_nxtstate == CMD_RD));
                  buf_data_wr_en = buf_wr_en;
                  cmd_done     = (ahb_hresp_q | (ahb_hready_q & (ahb_htrans_q[1:0] != 2'b0) &
                                 ((buf_cmd_byte_ptrQ == 3'b111) | (buf_byteen[get_nxtbyte_ptr(buf_cmd_byte_ptrQ[2:0],buf_byteen[7:0],1'b1)] == 1'b0))));
                  bypass_en       = buf_state_en & buf_write_in & (buf_nxtstate == CMD_WR);    
                  ahb_htrans[1:0] = {2{(~(cmd_done | cmd_doneQ) | bypass_en)}} & 2'b10;
                  slave_valid_pre  = buf_state_en & (buf_nxtstate != DONE);
                  trxn_done = ahb_hready_q & ahb_hwrite_q & (ahb_htrans_q[1:0] != 2'b0);
                  buf_cmd_byte_ptr_en = trxn_done | bypass_en;
                  buf_cmd_byte_ptr = bypass_en ? get_nxtbyte_ptr(3'b0,buf_byteen_in[7:0],1'b0) :
                                                 trxn_done ? get_nxtbyte_ptr(buf_cmd_byte_ptrQ[2:0],buf_byteen[7:0],1'b1) : buf_cmd_byte_ptrQ;
            end
         DONE: begin
                  buf_nxtstate = IDLE;
                  buf_state_en = slave_ready;
                  slvbuf_error_en = 1'b1;
                  slave_valid_pre = 1'b1;
         end
      endcase
   end
   assign buf_rst              = 1'b0;
   assign cmd_done_rst         = slave_valid_pre;
   assign buf_addr_in[2:0]     = (buf_aligned_in & (master_opc[2:1] == 2'b01)) ? get_write_addr(wrbuf_byteen[7:0]) : master_addr[2:0];
   assign buf_addr_in[31:3]    = master_addr[31:3];
   assign buf_tag_in[TAG-1:0]  = master_tag[TAG-1:0];
   assign buf_byteen_in[7:0]   = wrbuf_byteen[7:0];
   assign buf_data_in[63:0]    = (buf_state == DATA_RD) ? ahb_hrdata_q[63:0] : master_wdata[63:0];
   assign buf_size_in[1:0]     = (buf_aligned_in & (master_size[1:0] == 2'b11) & (master_opc[2:1] == 2'b01)) ? get_write_size(wrbuf_byteen[7:0]) : master_size[1:0];
   assign buf_aligned_in       = (master_opc[2:0] == 3'b0)    |    
                                 (master_size[1:0] == 2'b0) |  (master_size[1:0] == 2'b01) | (master_size[1:0] == 2'b10) |  
                                 ((master_size[1:0] == 2'b11) &
                                  ((wrbuf_byteen[7:0] == 8'h3)  | (wrbuf_byteen[7:0] == 8'hc)   | (wrbuf_byteen[7:0] == 8'h30) | (wrbuf_byteen[7:0] == 8'hc0) |
                                   (wrbuf_byteen[7:0] == 8'hf)  | (wrbuf_byteen[7:0] == 8'hf0)  | (wrbuf_byteen[7:0] == 8'hff)));
   assign ahb_haddr[31:3] = bypass_en ? master_addr[31:3]  : buf_addr[31:3];
   assign ahb_haddr[2:0]  = {3{(ahb_htrans == 2'b10)}} & buf_cmd_byte_ptr[2:0];     
   assign ahb_hsize[2:0]  = bypass_en ? {1'b0, ({2{buf_aligned_in}} & buf_size_in[1:0])} :
                                        {1'b0, ({2{buf_aligned}} & buf_size[1:0])};    
   assign ahb_hburst[2:0] = 3'b0;
   assign ahb_hmastlock   = 1'b0;
   assign ahb_hprot[3:0]  = {3'b001,~axi_arprot[2]};
   assign ahb_hwrite      = bypass_en ? (master_opc[2:1] == 2'b01) : buf_write;
   assign ahb_hwdata[63:0] = buf_data[63:0];
   assign slave_valid          = slave_valid_pre;
   assign slave_opc[3:2]       = slvbuf_write ? 2'b11 : 2'b00;
   assign slave_opc[1:0]       = {2{slvbuf_error}} & 2'b10;
   assign slave_rdata[63:0]    = slvbuf_error ? {2{last_bus_addr[31:0]}} : ((buf_state == DONE) ? buf_data[63:0] : ahb_hrdata_q[63:0]);
   assign slave_tag[TAG-1:0]   = slvbuf_tag[TAG-1:0];
   assign last_addr_en = (ahb_htrans[1:0] != 2'b0) & ahb_hready & ahb_hwrite ;
   rvdffsc_fpga  #(1)   wrbuf_vldff              (.din(1'b1), .clear(wrbuf_rst),       .dout(wrbuf_vld),          .en(wrbuf_en),      .clk(bus_clk), .clken(bus_clk_en), .rawclk(clk), .*);
   rvdffsc_fpga  #(1)   wrbuf_data_vldff         (.din(1'b1), .clear(wrbuf_rst),       .dout(wrbuf_data_vld),     .en(wrbuf_data_en), .clk(bus_clk), .clken(bus_clk_en), .rawclk(clk), .*);
   rvdffs_fpga   #(TAG) wrbuf_tagff              (.din(axi_awid[TAG-1:0]),             .dout(wrbuf_tag[TAG-1:0]), .en(wrbuf_en),      .clk(bus_clk), .clken(bus_clk_en), .rawclk(clk), .*);
   rvdffs_fpga   #(3)   wrbuf_sizeff             (.din(axi_awsize[2:0]),               .dout(wrbuf_size[2:0]),    .en(wrbuf_en),      .clk(bus_clk), .clken(bus_clk_en), .rawclk(clk), .*);
   rvdffs_fpga   #(8)   wrbuf_byteenff           (.din(axi_wstrb[7:0]),                .dout(wrbuf_byteen[7:0]),  .en(wrbuf_data_en), .clk(bus_clk), .clken(bus_clk_en), .rawclk(clk), .*);
   rvdffsc_fpga #($bits(state_t)) buf_state_ff   (.din(buf_nxtstate), .clear(buf_rst), .dout({buf_state}),        .en(buf_state_en),  .clk(ahbm_clk),.clken(bus_clk_en), .rawclk(clk), .*);
   rvdffs_fpga #(1)   buf_writeff    (.din(buf_write_in),        .dout(buf_write),           .en(buf_wr_en),    .clk(buf_clk), .clken(buf_clken), .rawclk(clk), .*);
   rvdffs_fpga #(TAG) buf_tagff      (.din(buf_tag_in[TAG-1:0]), .dout(buf_tag[TAG-1:0]),    .en(buf_wr_en),    .clk(buf_clk), .clken(buf_clken), .rawclk(clk), .*);
   rvdffs_fpga #(2)   buf_sizeff     (.din(buf_size_in[1:0]),    .dout(buf_size[1:0]),       .en(buf_wr_en),    .clk(buf_clk), .clken(buf_clken), .rawclk(clk), .*);
   rvdffs_fpga #(1)   buf_alignedff  (.din(buf_aligned_in),      .dout(buf_aligned),         .en(buf_wr_en),    .clk(buf_clk), .clken(buf_clken), .rawclk(clk), .*);
   rvdffs_fpga #(8)   buf_byteenff   (.din(buf_byteen_in[7:0]),  .dout(buf_byteen[7:0]),     .en(buf_wr_en),    .clk(buf_clk), .clken(buf_clken), .rawclk(clk), .*);
   rvdffs_fpga #(1)   slvbuf_writeff (.din(buf_write),           .dout(slvbuf_write),        .en(slvbuf_wr_en), .clk(buf_clk), .clken(buf_clken), .rawclk(clk), .*);
   rvdffs_fpga #(TAG) slvbuf_tagff   (.din(buf_tag[TAG-1:0]),    .dout(slvbuf_tag[TAG-1:0]), .en(slvbuf_wr_en), .clk(buf_clk), .clken(buf_clken), .rawclk(clk), .*);
   rvdffs_fpga #(1)   slvbuf_errorff    (.din(slvbuf_error_in),       .dout(slvbuf_error),  .en(slvbuf_error_en),              .clk(ahbm_clk), .clken(bus_clk_en), .rawclk(clk), .*);
   rvdffsc_fpga #(1)  buf_cmd_doneff    (.din(1'b1), .clear(cmd_done_rst), .dout(cmd_doneQ),.en(cmd_done),                     .clk(ahbm_clk), .clken(bus_clk_en), .rawclk(clk), .*);
   rvdffs_fpga #(3)  buf_cmd_byte_ptrff (.din(buf_cmd_byte_ptr[2:0]), .dout(buf_cmd_byte_ptrQ[2:0]), .en(buf_cmd_byte_ptr_en), .clk(ahbm_clk), .clken(bus_clk_en), .rawclk(clk), .*);
   rvdff_fpga #(1)  hready_ff           (.din(ahb_hready),            .dout(ahb_hready_q),                                     .clk(ahbm_clk), .clken(bus_clk_en), .rawclk(clk), .*);
   rvdff_fpga #(2)  htrans_ff           (.din(ahb_htrans[1:0]),       .dout(ahb_htrans_q[1:0]),                                .clk(ahbm_clk), .clken(bus_clk_en), .rawclk(clk), .*);
   rvdff_fpga #(1)  hwrite_ff (.din(ahb_hwrite), .dout(ahb_hwrite_q), .clk(ahbm_addr_clk), .clken(ahbm_addr_clken), .rawclk(clk), .*);
   rvdff_fpga #(1)  hresp_ff  (.din(ahb_hresp),  .dout(ahb_hresp_q),  .clk(ahbm_clk),      .clken(bus_clk_en), .rawclk(clk), .*);
   rvdffe #(.WIDTH(32))   last_bus_addrff (.din(ahb_haddr[31:0]),  .dout(last_bus_addr[31:0]), .en(last_addr_en & bus_clk_en), .*);
   rvdffe #(.WIDTH(64))    buf_dataff    (.din(buf_data_in[63:0]), .dout(buf_data[63:0]),   .en(buf_data_wr_en & bus_clk_en), .*);
   rvdffe #(.WIDTH(32))  wrbuf_addrff    (.din(axi_awaddr[31:0]),  .dout(wrbuf_addr[31:0]), .en(wrbuf_en & bus_clk_en), .*);
   rvdffe #(.WIDTH(64))  wrbuf_dataff    (.din(axi_wdata[63:0]),   .dout(wrbuf_data[63:0]), .en(wrbuf_data_en & bus_clk_en), .*);
   rvdffe #(.WIDTH(64)) hrdata_ff     (.din(ahb_hrdata[63:0]),  .dout(ahb_hrdata_q[63:0]),  .en(ahbm_data_clken), .*);
   rvdffe #(.WIDTH(32)) buf_addrff    (.din(buf_addr_in[31:0]), .dout(buf_addr[31:0]),      .en(buf_wr_en & bus_clk_en), .*);
   assign buf_clken       = bus_clk_en & (buf_wr_en | slvbuf_wr_en | clk_override);
   assign ahbm_addr_clken = bus_clk_en & ((ahb_hready & ahb_htrans[1]) | clk_override);
   assign ahbm_data_clken = bus_clk_en & ((buf_state != IDLE) | clk_override);
endmodule  
module rvdff #( parameter WIDTH=1 )
   (
     input logic [WIDTH-1:0] din,
     input logic           clk,
     input logic                   rst_l,
     output logic [WIDTH-1:0] dout
     );
   always_ff @(posedge clk or negedge rst_l) begin
      if (rst_l == 0)
        dout[WIDTH-1:0] <= 0;
      else
        dout[WIDTH-1:0] <= din[WIDTH-1:0];
   end
endmodule
module rvdffs #( parameter WIDTH=1 )
   (
     input logic [WIDTH-1:0] din,
     input logic             en,
     input logic           clk,
     input logic                   rst_l,
     output logic [WIDTH-1:0] dout
     );
   rvdff #(WIDTH) dffs (.din((en) ? din[WIDTH-1:0] : dout[WIDTH-1:0]), .*);
endmodule
module rvdffsc #( parameter WIDTH=1 )
   (
     input logic [WIDTH-1:0] din,
     input logic             en,
     input logic             clear,
     input logic           clk,
     input logic                   rst_l,
     output logic [WIDTH-1:0] dout
     );
   logic [WIDTH-1:0]          din_new;
   assign din_new = {WIDTH{~clear}} & (en ? din[WIDTH-1:0] : dout[WIDTH-1:0]);
   rvdff #(WIDTH) dffsc (.din(din_new[WIDTH-1:0]), .*);
endmodule
module rvdff_fpga #( parameter WIDTH=1 )
   (
     input logic [WIDTH-1:0] din,
     input logic           clk,
     input logic           clken,
     input logic           rawclk,
     input logic           rst_l,
     input logic           scan_mode,
     output logic [WIDTH-1:0] dout
     );
    rvdffs #(WIDTH) dffs (.clk(rawclk), .en(clken), .*);
endmodule
module rvdffs_fpga #( parameter WIDTH=1 )
   (
     input logic [WIDTH-1:0] din,
     input logic             en,
     input logic           clk,
     input logic           clken,
     input logic           rawclk,
     input logic           rst_l,
     input logic           scan_mode,
     output logic [WIDTH-1:0] dout
     );
   rvdffs #(WIDTH)   dffs (.clk(rawclk), .en(clken & en), .*);
endmodule
module rvdffsc_fpga #( parameter WIDTH=1 )
   (
     input logic [WIDTH-1:0] din,
     input logic             en,
     input logic             clear,
     input logic             clk,
     input logic             clken,
     input logic             rawclk,
     input logic             rst_l,
     input logic             scan_mode,
     output logic [WIDTH-1:0] dout
     );
   rvdffs  #(WIDTH)   dffs  (.clk(rawclk), .din(din[WIDTH-1:0] & {WIDTH{~clear}}),.en((en | clear) & clken), .*);
endmodule
module clockhdr
  (
   input logic TE, E, CP,
   output Q
   );
   logic  en_ff;
   logic  enable;
   assign      enable = E | TE;
   always @(negedge CP) begin
      en_ff <= enable;
   end
   assign Q = CP & en_ff;
endmodule
module rvoclkhdr
  (
   input  logic en,
   input  logic clk,
   input  logic scan_mode,
   output logic l1clk
   );
   logic        TE;
   assign       TE = scan_mode;
   assign l1clk = clk;
endmodule
module rvdffe #( parameter WIDTH=1, parameter OVERRIDE=0 )
   (
     input  logic [WIDTH-1:0] din,
     input  logic           en,
     input  logic           clk,
     input  logic           rst_l,
     input  logic             scan_mode,
     output logic [WIDTH-1:0] dout
     );
   logic                      l1clk;
   if (WIDTH >= 8 || OVERRIDE==1) begin: genblock
      rvdffs #(WIDTH) dff ( .* );
   end
   else
      $error("%m: rvdffe width must be >= 8");
endmodule  
module rvsyncss #(parameter WIDTH = 251)
   (
     input  logic                 clk,
     input  logic                 rst_l,
     input  logic [WIDTH-1:0]     din,
     output logic [WIDTH-1:0]     dout
     );
   logic [WIDTH-1:0]              din_ff1;
   rvdff #(WIDTH) sync_ff1  (.*, .din (din[WIDTH-1:0]),     .dout(din_ff1[WIDTH-1:0]));
   rvdff #(WIDTH) sync_ff2  (.*, .din (din_ff1[WIDTH-1:0]), .dout(dout[WIDTH-1:0]));
endmodule  
module rvlsadder
  (
    input logic [31:0] rs1,
    input logic [11:0] offset,
    output logic [31:0] dout
    );
   logic                cout;
   logic                sign;
   logic [31:12]        rs1_inc;
   logic [31:12]        rs1_dec;
   assign {cout,dout[11:0]} = {1'b0,rs1[11:0]} + {1'b0,offset[11:0]};
   assign rs1_inc[31:12] = rs1[31:12] + 1;
   assign rs1_dec[31:12] = rs1[31:12] - 1;
   assign sign = offset[11];
   assign dout[31:12] = ({20{  sign ^~  cout}} &     rs1[31:12]) |
                        ({20{ ~sign &   cout}}  & rs1_inc[31:12]) |
                        ({20{  sign &  ~cout}}  & rs1_dec[31:12]);
endmodule  
module rvbradder
  (
    input [31:1] pc,
    input [12:1] offset,
    output [31:1] dout
    );
   logic          cout;
   logic          sign;
   logic [31:13]  pc_inc;
   logic [31:13]  pc_dec;
   assign {cout,dout[12:1]} = {1'b0,pc[12:1]} + {1'b0,offset[12:1]};
   assign pc_inc[31:13] = pc[31:13] + 1;
   assign pc_dec[31:13] = pc[31:13] - 1;
   assign sign = offset[12];
   assign dout[31:13] = ({19{  sign ^~  cout}} &     pc[31:13]) |
                        ({19{ ~sign &   cout}}  & pc_inc[31:13]) |
                        ({19{  sign &  ~cout}}  & pc_dec[31:13]);
endmodule  
module rvtwoscomp #( parameter WIDTH=32 )
   (
     input logic [WIDTH-1:0] din,
     output logic [WIDTH-1:0] dout
     );
   logic [WIDTH-1:1]          dout_temp;    
   genvar                     i;
   for ( i = 1; i < WIDTH; i++ )  begin : flip_after_first_one
      assign dout_temp[i] = (|din[i-1:0]) ? ~din[i] : din[i];
   end : flip_after_first_one
   assign dout[WIDTH-1:0]  = { dout_temp[WIDTH-1:1], din[0] };
endmodule   
module rvfindfirst1 #( parameter WIDTH=32, SHIFT=$clog2(WIDTH) )
   (
     input logic [WIDTH-1:0] din,
     output logic [SHIFT-1:0] dout
     );
   logic                      done;
   always_comb begin
      dout[SHIFT-1:0] = {SHIFT{1'b0}};
      done    = 1'b0;
      for ( int i = WIDTH-1; i > 0; i-- )  begin : find_first_one
         done |= din[i];
         dout[SHIFT-1:0] += done ? 1'b0 : 1'b1;
      end : find_first_one
   end
endmodule  
module rvfindfirst1hot #( parameter WIDTH=32 )
   (
     input logic [WIDTH-1:0] din,
     output logic [WIDTH-1:0] dout
     );
   logic                      done;
   always_comb begin
      dout[WIDTH-1:0] = {WIDTH{1'b0}};
      done    = 1'b0;
      for ( int i = 0; i < WIDTH; i++ )  begin : find_first_one
         dout[i] = ~done & din[i];
         done   |= din[i];
      end : find_first_one
   end
endmodule  
module rvmaskandmatch #( parameter WIDTH=32 )
   (
     input  logic [WIDTH-1:0] mask,      
     input  logic [WIDTH-1:0] data,      
     input  logic             masken,    
     output logic             match
     );
   logic [WIDTH-1:0]          matchvec;
   logic                      masken_or_fullmask;
   assign masken_or_fullmask = masken &  ~(&mask[WIDTH-1:0]);
   assign matchvec[0]        = masken_or_fullmask | (mask[0] == data[0]);
   genvar                     i;
   for ( i = 1; i < WIDTH; i++ )  begin : match_after_first_zero
      assign matchvec[i] = (&mask[i-1:0] & masken_or_fullmask) ? 1'b1 : (mask[i] == data[i]);
   end : match_after_first_zero
   assign match  = &matchvec[WIDTH-1:0];     
endmodule  
module rvbtb_tag_hash (
                       input logic [31:1] pc,
                       output logic [9-1:0] hash
                       );
    assign hash = {(
                   pc[5+9+9:5+9+1] ^
                   pc[5+9:5+1])};
endmodule
module rvbtb_addr_hash (
                        input logic [31:1] pc,
                        output logic [5:4] hash
                        );
   assign hash[5:4] = pc[5:4] ^
                                                  pc[7:6] ^
                                                  pc[9:8];
endmodule
module rvbtb_ghr_hash (
                       input logic [5:4] hashin,
                       input logic [4:0] ghr,
                       output logic [7:4] hash
                       );
   assign hash[7:4] = {ghr[3:2] ^ {ghr[3+1], {4-1-2{1'b0} } },hashin[5:4]^ghr[2-1:0]};
endmodule
module rvrangecheck  #(CCM_SADR = 32'h0,
                       CCM_SIZE  = 128) (
   input  logic [31:0]   addr,                              
   output logic          in_range,                             
   output logic          in_region
);
   localparam REGION_BITS = 4;
   localparam MASK_BITS = 10 + $clog2(CCM_SIZE);
   logic [31:0]          start_addr;
   logic [3:0]           region;
   assign start_addr[31:0]        = CCM_SADR;
   assign region[REGION_BITS-1:0] = start_addr[31:(32-REGION_BITS)];
   assign in_region = (addr[31:(32-REGION_BITS)] == region[REGION_BITS-1:0]);
   if (CCM_SIZE  == 48)
    assign in_range  = (addr[31:MASK_BITS] == start_addr[31:MASK_BITS]) & ~(&addr[MASK_BITS-1 : MASK_BITS-2]);
   else
    assign in_range  = (addr[31:MASK_BITS] == start_addr[31:MASK_BITS]);
endmodule   
module rveven_paritygen #(WIDTH = 16)  (
                                         input  logic [WIDTH-1:0]  data_in,          
                                         output logic              parity_out        
                                         );
   assign  parity_out =  ^(data_in[WIDTH-1:0]) ;
endmodule   
module rveven_paritycheck #(WIDTH = 16)  (
                                           input  logic [WIDTH-1:0]  data_in,          
                                           input  logic              parity_in,
                                           output logic              parity_err        
                                           );
   assign  parity_err =  ^(data_in[WIDTH-1:0]) ^ parity_in ;
endmodule   
module rvecc_encode  (
                      input [31:0] din,
                      output [6:0] ecc_out
                      );
logic [5:0] ecc_out_temp;
   assign ecc_out_temp[0] = din[0]^din[1]^din[3]^din[4]^din[6]^din[8]^din[10]^din[11]^din[13]^din[15]^din[17]^din[19]^din[21]^din[23]^din[25]^din[26]^din[28]^din[30];
   assign ecc_out_temp[1] = din[0]^din[2]^din[3]^din[5]^din[6]^din[9]^din[10]^din[12]^din[13]^din[16]^din[17]^din[20]^din[21]^din[24]^din[25]^din[27]^din[28]^din[31];
   assign ecc_out_temp[2] = din[1]^din[2]^din[3]^din[7]^din[8]^din[9]^din[10]^din[14]^din[15]^din[16]^din[17]^din[22]^din[23]^din[24]^din[25]^din[29]^din[30]^din[31];
   assign ecc_out_temp[3] = din[4]^din[5]^din[6]^din[7]^din[8]^din[9]^din[10]^din[18]^din[19]^din[20]^din[21]^din[22]^din[23]^din[24]^din[25];
   assign ecc_out_temp[4] = din[11]^din[12]^din[13]^din[14]^din[15]^din[16]^din[17]^din[18]^din[19]^din[20]^din[21]^din[22]^din[23]^din[24]^din[25];
   assign ecc_out_temp[5] = din[26]^din[27]^din[28]^din[29]^din[30]^din[31];
   assign ecc_out[6:0] = {(^din[31:0])^(^ecc_out_temp[5:0]),ecc_out_temp[5:0]};
endmodule  
module rvecc_decode  (
                      input         en,
                      input [31:0]  din,
                      input [6:0]   ecc_in,
                      input         sed_ded,     
                      output [31:0] dout,
                      output [6:0]  ecc_out,
                      output        single_ecc_error,
                      output        double_ecc_error
                      );
   logic [6:0]                      ecc_check;
   logic [38:0]                     error_mask;
   logic [38:0]                     din_plus_parity, dout_plus_parity;
   assign ecc_check[0] = ecc_in[0]^din[0]^din[1]^din[3]^din[4]^din[6]^din[8]^din[10]^din[11]^din[13]^din[15]^din[17]^din[19]^din[21]^din[23]^din[25]^din[26]^din[28]^din[30];
   assign ecc_check[1] = ecc_in[1]^din[0]^din[2]^din[3]^din[5]^din[6]^din[9]^din[10]^din[12]^din[13]^din[16]^din[17]^din[20]^din[21]^din[24]^din[25]^din[27]^din[28]^din[31];
   assign ecc_check[2] = ecc_in[2]^din[1]^din[2]^din[3]^din[7]^din[8]^din[9]^din[10]^din[14]^din[15]^din[16]^din[17]^din[22]^din[23]^din[24]^din[25]^din[29]^din[30]^din[31];
   assign ecc_check[3] = ecc_in[3]^din[4]^din[5]^din[6]^din[7]^din[8]^din[9]^din[10]^din[18]^din[19]^din[20]^din[21]^din[22]^din[23]^din[24]^din[25];
   assign ecc_check[4] = ecc_in[4]^din[11]^din[12]^din[13]^din[14]^din[15]^din[16]^din[17]^din[18]^din[19]^din[20]^din[21]^din[22]^din[23]^din[24]^din[25];
   assign ecc_check[5] = ecc_in[5]^din[26]^din[27]^din[28]^din[29]^din[30]^din[31];
   assign ecc_check[6] = ((^din[31:0])^(^ecc_in[6:0])) & ~sed_ded;
   assign single_ecc_error = en & (ecc_check[6:0] != 0) & ecc_check[6];    
   assign double_ecc_error = en & (ecc_check[6:0] != 0) & ~ecc_check[6];   
   for (genvar i=1; i<40; i++) begin
      assign error_mask[i-1] = (ecc_check[5:0] == i);
   end
   assign din_plus_parity[38:0] = {ecc_in[6], din[31:26], ecc_in[5], din[25:11], ecc_in[4], din[10:4], ecc_in[3], din[3:1], ecc_in[2], din[0], ecc_in[1:0]};
   assign dout_plus_parity[38:0] = single_ecc_error ? (error_mask[38:0] ^ din_plus_parity[38:0]) : din_plus_parity[38:0];
   assign dout[31:0]             = {dout_plus_parity[37:32], dout_plus_parity[30:16], dout_plus_parity[14:8], dout_plus_parity[6:4], dout_plus_parity[2]};
   assign ecc_out[6:0]           = {(dout_plus_parity[38] ^ (ecc_check[6:0] == 7'b1000000)), dout_plus_parity[31], dout_plus_parity[15], dout_plus_parity[7], dout_plus_parity[3], dout_plus_parity[1:0]};
endmodule  
module ram_32768x39
  ( input logic CLK,
    input logic [14:0] ADR,
    input logic [38:0] D,
    output logic [38:0] Q,
    input logic WE );
   reg [38:0]   ram_core [32767:0];
   always @(posedge CLK) begin
      if (WE) begin 
         ram_core[ADR] <= D; Q <= 'x; end else
           Q <= ram_core[ADR];
   end
endmodule  
module ram_16384x39
  ( input logic CLK,
    input logic [13:0] ADR,
    input logic [38:0] D,
    output logic [38:0] Q,
    input logic WE );
   reg [38:0]   ram_core [16383:0];
   always @(posedge CLK) begin
      if (WE) begin 
         ram_core[ADR] <= D; Q <= 'x; end else
           Q <= ram_core[ADR];
   end
endmodule  
module ram_8192x39
  ( input logic CLK,
    input logic [12:0] ADR,
    input logic [38:0] D,
    output logic [38:0] Q,
    input logic WE );
   reg [38:0]   ram_core [8191:0];
   always @(posedge CLK) begin
      if (WE) begin 
         ram_core[ADR] <= D; Q <= 'x; end else
           Q <= ram_core[ADR];
   end
endmodule  
module ram_4096x39
  ( input logic CLK,
    input logic [11:0] ADR,
    input logic [38:0] D,
    output logic [38:0] Q,
    input logic WE );
   reg [38:0]   ram_core [4095:0];
   always @(posedge CLK) begin
      if (WE) begin 
         ram_core[ADR] <= D; Q <= 'x; end else
           Q <= ram_core[ADR];
   end
endmodule  
module ram_3072x39
  ( input logic CLK,
    input logic [11:0] ADR,
    input logic [38:0] D,
    output logic [38:0] Q,
    input logic WE );
   reg [38:0]   ram_core [3071:0];
   always @(posedge CLK) begin
      if (WE) begin 
         ram_core[ADR] <= D; Q <= 'x; end else
           Q <= ram_core[ADR];
   end
endmodule  
module ram_2048x39
  ( input logic CLK,
    input logic [10:0] ADR,
    input logic [38:0] D,
    output logic [38:0] Q,
    input logic WE );
   reg [38:0]   ram_core [2047:0];
   always @(posedge CLK) begin
      if (WE) begin 
         ram_core[ADR] <= D; Q <= 'x; end else
           Q <= ram_core[ADR];
   end
endmodule  
module ram_1536x39      
  ( input logic CLK,
    input logic [10:0] ADR,
    input logic [38:0] D,
    output logic [38:0] Q,
    input logic WE );
   reg [38:0]   ram_core [1535:0];
   always @(posedge CLK) begin
      if (WE) begin 
         ram_core[ADR] <= D; Q <= 'x; end else
           Q <= ram_core[ADR];
   end
endmodule  
module ram_1024x39
  ( input logic CLK,
    input logic [9:0] ADR,
    input logic [38:0] D,
    output logic [38:0] Q,
    input logic WE );
   reg [38:0]   ram_core [1023:0];
   always @(posedge CLK) begin
      if (WE) begin 
         ram_core[ADR] <= D; Q <= 'x; end else
           Q <= ram_core[ADR];
   end
endmodule  
module ram_768x39
  ( input logic CLK,
    input logic [9:0] ADR,
    input logic [38:0] D,
    output logic [38:0] Q,
    input logic WE );
   reg [38:0]   ram_core [767:0];
   always @(posedge CLK) begin
      if (WE) begin 
         ram_core[ADR] <= D; Q <= 'x; end else
           Q <= ram_core[ADR];
   end
endmodule  
module ram_512x39
  ( input logic CLK,
    input logic [8:0] ADR,
    input logic [38:0] D,
    output logic [38:0] Q,
    input logic WE );
   reg [38:0]   ram_core [511:0];
   always @(posedge CLK) begin
      if (WE) begin 
         ram_core[ADR] <= D; Q <= 'x; end else
           Q <= ram_core[ADR];
   end
endmodule  
module ram_256x39
  ( input logic CLK,
    input logic [7:0] ADR,
    input logic [38:0] D,
    output logic [38:0] Q,
    input logic WE );
   reg [38:0]   ram_core [255:0];
   always @(posedge CLK) begin
      if (WE) begin 
         ram_core[ADR] <= D; Q <= 'x; end else
           Q <= ram_core[ADR];
   end
endmodule  
module ram_128x39
  ( input logic CLK,
    input logic [6:0] ADR,
    input logic [38:0] D,
    output logic [38:0] Q,
    input logic WE );
   reg [38:0]   ram_core [127:0];
   always @(posedge CLK) begin
      if (WE) begin 
         ram_core[ADR] <= D; Q <= 'x; end else
           Q <= ram_core[ADR];
   end
endmodule  
module ram_1024x20
  ( input logic CLK,
    input logic [9:0] ADR,
    input logic [19:0] D,
    output logic [19:0] Q,
    input logic WE );
   reg [19:0]   ram_core [1023:0];
   always @(posedge CLK) begin
      if (WE) begin 
         ram_core[ADR] <= D; Q <= 'x; end else
           Q <= ram_core[ADR];
   end
endmodule  
module ram_512x20
  ( input logic CLK,
    input logic [8:0] ADR,
    input logic [19:0] D,
    output logic [19:0] Q,
    input logic WE );
   reg [19:0]   ram_core [511:0];
   always @(posedge CLK) begin
      if (WE) begin 
         ram_core[ADR] <= D; Q <= 'x; end else
           Q <= ram_core[ADR];
   end
endmodule  
module ram_256x20
  ( input logic CLK,
    input logic [7:0] ADR,
    input logic [19:0] D,
    output logic [19:0] Q,
    input logic WE );
   reg [19:0]   ram_core [255:0];
   always @(posedge CLK) begin
      if (WE) begin 
         ram_core[ADR] <= D; Q <= 'x; end else
           Q <= ram_core[ADR];
   end
endmodule  
module ram_128x20
  ( input logic CLK,
    input logic [6:0] ADR,
    input logic [19:0] D,
    output logic [19:0] Q,
    input logic WE );
   reg [19:0]   ram_core [127:0];
   always @(posedge CLK) begin
      if (WE) begin 
         ram_core[ADR] <= D; Q <= 'x; end else
           Q <= ram_core[ADR];
   end
endmodule  
module ram_64x20
  ( input logic CLK,
    input logic [5:0] ADR,
    input logic [19:0] D,
    output logic [19:0] Q,
    input logic WE );
   reg [19:0]   ram_core [63:0];
   always @(posedge CLK) begin
      if (WE) begin 
         ram_core[ADR] <= D; Q <= 'x; end else
           Q <= ram_core[ADR];
   end
endmodule  
module ram_4096x34
  ( input logic CLK,
    input logic [11:0] ADR,
    input logic [33:0] D,
    output logic [33:0] Q,
    input logic WE );
   reg [33:0]   ram_core [4095:0];
   always @(posedge CLK) begin
      if (WE) begin 
         ram_core[ADR] <= D; Q <= 'x; end else
           Q <= ram_core[ADR];
   end
endmodule  
module ram_2048x34
  ( input logic CLK,
    input logic [10:0] ADR,
    input logic [33:0] D,
    output logic [33:0] Q,
    input logic WE );
   reg [33:0]   ram_core [2047:0];
   always @(posedge CLK) begin
      if (WE) begin 
         ram_core[ADR] <= D; Q <= 'x; end else
           Q <= ram_core[ADR];
   end
endmodule  
module ram_1024x34
  ( input logic CLK,
    input logic [9:0] ADR,
    input logic [33:0] D,
    output logic [33:0] Q,
    input logic WE );
   reg [33:0]   ram_core [1023:0];
   always @(posedge CLK) begin
      if (WE) begin 
         ram_core[ADR] <= D; Q <= 'x; end else
           Q <= ram_core[ADR];
   end
endmodule  
module ram_512x34
  ( input logic CLK,
    input logic [8:0] ADR,
    input logic [33:0] D,
    output logic [33:0] Q,
    input logic WE );
   reg [33:0]   ram_core [511:0];
   always @(posedge CLK) begin
      if (WE) begin 
         ram_core[ADR] <= D; Q <= 'x; end else
           Q <= ram_core[ADR];
   end
endmodule  
module ram_256x34
  ( input logic CLK,
    input logic [7:0] ADR,
    input logic [33:0] D,
    output logic [33:0] Q,
    input logic WE );
   reg [33:0]   ram_core [255:0];
   always @(posedge CLK) begin
      if (WE) begin 
         ram_core[ADR] <= D; Q <= 'x; end else
           Q <= ram_core[ADR];
   end
endmodule  
module ram_128x34
  ( input logic CLK,
    input logic [6:0] ADR,
    input logic [33:0] D,
    output logic [33:0] Q,
    input logic WE );
   reg [33:0]   ram_core [127:0];
   always @(posedge CLK) begin
      if (WE) begin 
         ram_core[ADR] <= D; Q <= 'x; end else
           Q <= ram_core[ADR];
   end
endmodule  
module ram_64x34
  ( input logic CLK,
    input logic [5:0] ADR,
    input logic [33:0] D,
    output logic [33:0] Q,
    input logic WE );
   reg [33:0]   ram_core [63:0];
   always @(posedge CLK) begin
      if (WE) begin 
         ram_core[ADR] <= D; Q <= 'x; end else
           Q <= ram_core[ADR];
   end
endmodule  
module ram_4096x42
  ( input logic CLK,
    input logic [11:0] ADR,
    input logic [41:0] D,
    output logic [41:0] Q,
    input logic WE );
   reg [41:0]   ram_core [4095:0];
   always @(posedge CLK) begin
      if (WE) begin 
         ram_core[ADR] <= D; Q <= 'x; end else
           Q <= ram_core[ADR];
   end
endmodule  
module ram_2048x42
  ( input logic CLK,
    input logic [10:0] ADR,
    input logic [41:0] D,
    output logic [41:0] Q,
    input logic WE );
   reg [41:0]   ram_core [2047:0];
   always @(posedge CLK) begin
      if (WE) begin 
         ram_core[ADR] <= D; Q <= 'x; end else
           Q <= ram_core[ADR];
   end
endmodule  
module ram_1024x42
  ( input logic CLK,
    input logic [9:0] ADR,
    input logic [41:0] D,
    output logic [41:0] Q,
    input logic WE );
   reg [41:0]   ram_core [1023:0];
   always @(posedge CLK) begin
      if (WE) begin 
         ram_core[ADR] <= D; Q <= 'x; end else
           Q <= ram_core[ADR];
   end
endmodule  
module ram_512x42
  ( input logic CLK,
    input logic [8:0] ADR,
    input logic [41:0] D,
    output logic [41:0] Q,
    input logic WE );
   reg [41:0]   ram_core [511:0];
   always @(posedge CLK) begin
      if (WE) begin 
         ram_core[ADR] <= D; Q <= 'x; end else
           Q <= ram_core[ADR];
   end
endmodule  
module ram_256x42
  ( input logic CLK,
    input logic [7:0] ADR,
    input logic [41:0] D,
    output logic [41:0] Q,
    input logic WE );
   reg [41:0]   ram_core [255:0];
   always @(posedge CLK) begin
      if (WE) begin 
         ram_core[ADR] <= D; Q <= 'x; end else
           Q <= ram_core[ADR];
   end
endmodule  
module ram_128x42
  ( input logic CLK,
    input logic [6:0] ADR,
    input logic [41:0] D,
    output logic [41:0] Q,
    input logic WE );
   reg [41:0]   ram_core [127:0];
   always @(posedge CLK) begin
      if (WE) begin 
         ram_core[ADR] <= D; Q <= 'x; end else
           Q <= ram_core[ADR];
   end
endmodule  
module ram_64x42
  ( input logic CLK,
    input logic [5:0] ADR,
    input logic [41:0] D,
    output logic [41:0] Q,
    input logic WE );
   reg [41:0]   ram_core [63:0];
   always @(posedge CLK) begin
      if (WE) begin 
         ram_core[ADR] <= D; Q <= 'x; end else
           Q <= ram_core[ADR];
   end
endmodule  
module ram_1024x21
  ( input logic CLK,
    input logic [9:0] ADR,
    input logic [20:0] D,
    output logic [20:0] Q,
    input logic WE );
   reg [20:0]   ram_core [1023:0];
   always @(posedge CLK) begin
      if (WE) begin 
         ram_core[ADR] <= D; Q <= 'x; end else
           Q <= ram_core[ADR];
   end
endmodule  
module ram_512x21
  ( input logic CLK,
    input logic [8:0] ADR,
    input logic [20:0] D,
    output logic [20:0] Q,
    input logic WE );
   reg [20:0]   ram_core [511:0];
   always @(posedge CLK) begin
      if (WE) begin 
         ram_core[ADR] <= D; Q <= 'x; end else
           Q <= ram_core[ADR];
   end
endmodule  
module ram_256x21
  ( input logic CLK,
    input logic [7:0] ADR,
    input logic [20:0] D,
    output logic [20:0] Q,
    input logic WE );
   reg [20:0]   ram_core [255:0];
   always @(posedge CLK) begin
      if (WE) begin 
         ram_core[ADR] <= D; Q <= 'x; end else
           Q <= ram_core[ADR];
   end
endmodule  
module ram_128x21
  ( input logic CLK,
    input logic [6:0] ADR,
    input logic [20:0] D,
    output logic [20:0] Q,
    input logic WE );
   reg [20:0]   ram_core [127:0];
   always @(posedge CLK) begin
      if (WE) begin 
         ram_core[ADR] <= D; Q <= 'x; end else
           Q <= ram_core[ADR];
   end
endmodule  
module ram_64x21
  ( input logic CLK,
    input logic [5:0] ADR,
    input logic [20:0] D,
    output logic [20:0] Q,
    input logic WE );
   reg [20:0]   ram_core [63:0];
   always @(posedge CLK) begin
      if (WE) begin 
         ram_core[ADR] <= D; Q <= 'x; end else
           Q <= ram_core[ADR];
   end
endmodule  
module ram_1024x25
  ( input logic CLK,
    input logic [9:0] ADR,
    input logic [24:0] D,
    output logic [24:0] Q,
    input logic WE );
   reg [24:0]   ram_core [1023:0];
   always @(posedge CLK) begin
      if (WE) begin 
         ram_core[ADR] <= D; Q <= 'x; end else
           Q <= ram_core[ADR];
   end
endmodule  
module ram_512x25
  ( input logic CLK,
    input logic [8:0] ADR,
    input logic [24:0] D,
    output logic [24:0] Q,
    input logic WE );
   reg [24:0]   ram_core [511:0];
   always @(posedge CLK) begin
      if (WE) begin 
         ram_core[ADR] <= D; Q <= 'x; end else
           Q <= ram_core[ADR];
   end
endmodule  
module ram_256x25
  ( input logic CLK,
    input logic [7:0] ADR,
    input logic [24:0] D,
    output logic [24:0] Q,
    input logic WE );
   reg [24:0]   ram_core [255:0];
   always @(posedge CLK) begin
      if (WE) begin 
         ram_core[ADR] <= D; Q <= 'x; end else
           Q <= ram_core[ADR];
   end
endmodule  
module ram_128x25
  ( input logic CLK,
    input logic [6:0] ADR,
    input logic [24:0] D,
    output logic [24:0] Q,
    input logic WE );
   reg [24:0]   ram_core [127:0];
   always @(posedge CLK) begin
      if (WE) begin 
         ram_core[ADR] <= D; Q <= 'x; end else
           Q <= ram_core[ADR];
   end
endmodule  
module ram_64x25
  ( input logic CLK,
    input logic [5:0] ADR,
    input logic [24:0] D,
    output logic [24:0] Q,
    input logic WE );
   reg [24:0]   ram_core [63:0];
   always @(posedge CLK) begin
      if (WE) begin 
         ram_core[ADR] <= D; Q <= 'x; end else
           Q <= ram_core[ADR];
   end
endmodule  
