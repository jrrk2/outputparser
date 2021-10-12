package riscv;
    typedef enum logic [3:0] {
       ModeOff  = 0,
       ModeSv32 = 1,
       ModeSv39 = 8,
       ModeSv48 = 9,
       ModeSv57 = 10,
       ModeSv64 = 11
    } vm_mode_t;
    localparam XLEN = 64;
    localparam VLEN       = (XLEN == 32) ? 32 : 64;     
    localparam PLEN       = (XLEN == 32) ? 34 : 56;     
    localparam IS_XLEN32  = (XLEN == 32) ? 1'b1 : 1'b0;
    localparam IS_XLEN64  = (XLEN == 32) ? 1'b0 : 1'b1;
    localparam ModeW      = (XLEN == 32) ? 1 : 4;
    localparam ASIDW      = (XLEN == 32) ? 9 : 16;
    localparam PPNW       = (XLEN == 32) ? 22 : 44;
    localparam vm_mode_t MODE_SV = (XLEN == 32) ? ModeSv32 : ModeSv39;
    localparam SV         = (MODE_SV == ModeSv32) ? 32 : 39;
    localparam VPN2       = (VLEN-31 < 8) ? VLEN-31 : 8;
    localparam  FPU_EN     = 1'b1;  
    typedef logic [XLEN-1:0] xlen_t;
    typedef enum logic[1:0] {
      PRIV_LVL_M = 2'b11,
      PRIV_LVL_S = 2'b01,
      PRIV_LVL_U = 2'b00
    } priv_lvl_t;
    typedef enum logic [1:0] {
        XLEN_32  = 2'b01,
        XLEN_64  = 2'b10,
        XLEN_128 = 2'b11
    } xlen_e;
    typedef enum logic [1:0] {
        Off     = 2'b00,
        Initial = 2'b01,
        Clean   = 2'b10,
        Dirty   = 2'b11
    } xs_t;
    typedef struct packed {
        logic         sd;      
        logic [62:36] wpri4;   
        xlen_e        sxl;     
        xlen_e        uxl;     
        logic [8:0]   wpri3;   
        logic         tsr;     
        logic         tw;      
        logic         tvm;     
        logic         mxr;     
        logic         sum;     
        logic         mprv;    
        xs_t          xs;      
        xs_t          fs;      
        priv_lvl_t    mpp;     
        logic [1:0]   wpri2;   
        logic         spp;     
        logic         mpie;    
        logic         wpri1;   
        logic         spie;    
        logic         upie;    
        logic         mie;     
        logic         wpri0;   
        logic         sie;     
        logic         uie;     
    } status_rv_t;
    typedef struct packed {
        logic [ModeW-1:0] mode;
        logic [ASIDW-1:0] asid;
        logic [PPNW-1:0]  ppn;
    } satp_t;
    typedef struct packed {
        logic [31:25] funct7;
        logic [24:20] rs2;
        logic [19:15] rs1;
        logic [14:12] funct3;
        logic [11:7]  rd;
        logic [6:0]   opcode;
    } rtype_t;
    typedef struct packed {
        logic [31:27] rs3;
        logic [26:25] funct2;
        logic [24:20] rs2;
        logic [19:15] rs1;
        logic [14:12] funct3;
        logic [11:7]  rd;
        logic [6:0]   opcode;
    } r4type_t;
    typedef struct packed {
        logic [31:27] funct5;
        logic [26:25] fmt;
        logic [24:20] rs2;
        logic [19:15] rs1;
        logic [14:12] rm;
        logic [11:7]  rd;
        logic [6:0]   opcode;
    } rftype_t;  
    typedef struct packed {
        logic [31:30] funct2;
        logic [29:25] vecfltop;
        logic [24:20] rs2;
        logic [19:15] rs1;
        logic [14:14] repl;
        logic [13:12] vfmt;
        logic [11:7]  rd;
        logic [6:0]   opcode;
    } rvftype_t;  
    typedef struct packed {
        logic [31:20] imm;
        logic [19:15] rs1;
        logic [14:12] funct3;
        logic [11:7]  rd;
        logic [6:0]   opcode;
    } itype_t;
    typedef struct packed {
        logic [31:25] imm;
        logic [24:20] rs2;
        logic [19:15] rs1;
        logic [14:12] funct3;
        logic [11:7]  imm0;
        logic [6:0]   opcode;
    } stype_t;
    typedef struct packed {
        logic [31:12] funct3;
        logic [11:7]  rd;
        logic [6:0]   opcode;
    } utype_t;
    typedef struct packed {
        logic [31:27] funct5;
        logic         aq;
        logic         rl;
        logic [24:20] rs2;
        logic [19:15] rs1;
        logic [14:12] funct3;
        logic [11:7]  rd;
        logic [6:0]   opcode;
    } atype_t;
    typedef union packed {
        logic [31:0]   instr;
        rtype_t        rtype;
        r4type_t       r4type;
        rftype_t       rftype;
        rvftype_t      rvftype;
        itype_t        itype;
        stype_t        stype;
        utype_t        utype;
        atype_t        atype;
    } instruction_t;
    localparam OpcodeLoad      = 7'b00_000_11;
    localparam OpcodeLoadFp    = 7'b00_001_11;
    localparam OpcodeCustom0   = 7'b00_010_11;
    localparam OpcodeMiscMem   = 7'b00_011_11;
    localparam OpcodeOpImm     = 7'b00_100_11;
    localparam OpcodeAuipc     = 7'b00_101_11;
    localparam OpcodeOpImm32   = 7'b00_110_11;
    localparam OpcodeStore     = 7'b01_000_11;
    localparam OpcodeStoreFp   = 7'b01_001_11;
    localparam OpcodeCustom1   = 7'b01_010_11;
    localparam OpcodeAmo       = 7'b01_011_11;
    localparam OpcodeOp        = 7'b01_100_11;
    localparam OpcodeLui       = 7'b01_101_11;
    localparam OpcodeOp32      = 7'b01_110_11;
    localparam OpcodeMadd      = 7'b10_000_11;
    localparam OpcodeMsub      = 7'b10_001_11;
    localparam OpcodeNmsub     = 7'b10_010_11;
    localparam OpcodeNmadd     = 7'b10_011_11;
    localparam OpcodeOpFp      = 7'b10_100_11;
    localparam OpcodeRsrvd1    = 7'b10_101_11;
    localparam OpcodeCustom2   = 7'b10_110_11;
    localparam OpcodeBranch    = 7'b11_000_11;
    localparam OpcodeJalr      = 7'b11_001_11;
    localparam OpcodeRsrvd2    = 7'b11_010_11;
    localparam OpcodeJal       = 7'b11_011_11;
    localparam OpcodeSystem    = 7'b11_100_11;
    localparam OpcodeRsrvd3    = 7'b11_101_11;
    localparam OpcodeCustom3   = 7'b11_110_11;
    localparam OpcodeC0             = 2'b00;
    localparam OpcodeC0Addi4spn     = 3'b000;
    localparam OpcodeC0Fld          = 3'b001;
    localparam OpcodeC0Lw           = 3'b010;
    localparam OpcodeC0Ld           = 3'b011;
    localparam OpcodeC0Rsrvd        = 3'b100;
    localparam OpcodeC0Fsd          = 3'b101;
    localparam OpcodeC0Sw           = 3'b110;
    localparam OpcodeC0Sd           = 3'b111;
    localparam OpcodeC1             = 2'b01;
    localparam OpcodeC1Addi         = 3'b000;
    localparam OpcodeC1Addiw        = 3'b001;  
    localparam OpcodeC1Jal          = 3'b001;  
    localparam OpcodeC1Li           = 3'b010;
    localparam OpcodeC1LuiAddi16sp  = 3'b011;
    localparam OpcodeC1MiscAlu      = 3'b100;
    localparam OpcodeC1J            = 3'b101;
    localparam OpcodeC1Beqz         = 3'b110;
    localparam OpcodeC1Bnez         = 3'b111;
    localparam OpcodeC2             = 2'b10;
    localparam OpcodeC2Slli         = 3'b000;
    localparam OpcodeC2Fldsp        = 3'b001;
    localparam OpcodeC2Lwsp         = 3'b010;
    localparam OpcodeC2Ldsp         = 3'b011;
    localparam OpcodeC2JalrMvAdd    = 3'b100;
    localparam OpcodeC2Fsdsp        = 3'b101;
    localparam OpcodeC2Swsp         = 3'b110;
    localparam OpcodeC2Sdsp         = 3'b111;
    typedef struct packed {
        logic [9:0]  reserved;
        logic [44-1:0] ppn;  
        logic [1:0]  rsw;
        logic d;
        logic a;
        logic g;
        logic u;
        logic x;
        logic w;
        logic r;
        logic v;
    } pte_t;
    typedef struct packed {
        logic [22-1:0] ppn;  
        logic [1:0]  rsw;
        logic d;
        logic a;
        logic g;
        logic u;
        logic x;
        logic w;
        logic r;
        logic v;
    } pte_sv32_t;
    localparam logic [XLEN-1:0] INSTR_ADDR_MISALIGNED = 0;
    localparam logic [XLEN-1:0] INSTR_ACCESS_FAULT    = 1;   
    localparam logic [XLEN-1:0] ILLEGAL_INSTR         = 2;
    localparam logic [XLEN-1:0] BREAKPOINT            = 3;
    localparam logic [XLEN-1:0] LD_ADDR_MISALIGNED    = 4;
    localparam logic [XLEN-1:0] LD_ACCESS_FAULT       = 5;   
    localparam logic [XLEN-1:0] ST_ADDR_MISALIGNED    = 6;
    localparam logic [XLEN-1:0] ST_ACCESS_FAULT       = 7;   
    localparam logic [XLEN-1:0] ENV_CALL_UMODE        = 8;   
    localparam logic [XLEN-1:0] ENV_CALL_SMODE        = 9;   
    localparam logic [XLEN-1:0] ENV_CALL_MMODE        = 11;  
    localparam logic [XLEN-1:0] INSTR_PAGE_FAULT      = 12;  
    localparam logic [XLEN-1:0] LOAD_PAGE_FAULT       = 13;  
    localparam logic [XLEN-1:0] STORE_PAGE_FAULT      = 15;  
    localparam logic [XLEN-1:0] DEBUG_REQUEST         = 24;  
    localparam int unsigned IRQ_S_SOFT  = 1;
    localparam int unsigned IRQ_M_SOFT  = 3;
    localparam int unsigned IRQ_S_TIMER = 5;
    localparam int unsigned IRQ_M_TIMER = 7;
    localparam int unsigned IRQ_S_EXT   = 9;
    localparam int unsigned IRQ_M_EXT   = 11;
    localparam logic [XLEN-1:0] MIP_SSIP = 1 << IRQ_S_SOFT;
    localparam logic [XLEN-1:0] MIP_MSIP = 1 << IRQ_M_SOFT;
    localparam logic [XLEN-1:0] MIP_STIP = 1 << IRQ_S_TIMER;
    localparam logic [XLEN-1:0] MIP_MTIP = 1 << IRQ_M_TIMER;
    localparam logic [XLEN-1:0] MIP_SEIP = 1 << IRQ_S_EXT;
    localparam logic [XLEN-1:0] MIP_MEIP = 1 << IRQ_M_EXT;
    localparam logic [XLEN-1:0] S_SW_INTERRUPT    = (1 << (XLEN-1)) | IRQ_S_SOFT;
    localparam logic [XLEN-1:0] M_SW_INTERRUPT    = (1 << (XLEN-1)) | IRQ_M_SOFT;
    localparam logic [XLEN-1:0] S_TIMER_INTERRUPT = (1 << (XLEN-1)) | IRQ_S_TIMER;
    localparam logic [XLEN-1:0] M_TIMER_INTERRUPT = (1 << (XLEN-1)) | IRQ_M_TIMER;
    localparam logic [XLEN-1:0] S_EXT_INTERRUPT   = (1 << (XLEN-1)) | IRQ_S_EXT;
    localparam logic [XLEN-1:0] M_EXT_INTERRUPT   = (1 << (XLEN-1)) | IRQ_M_EXT;
    typedef enum logic [11:0] {
        CSR_FFLAGS         = 12'h001,
        CSR_FRM            = 12'h002,
        CSR_FCSR           = 12'h003,
        CSR_FTRAN          = 12'h800,
        CSR_SSTATUS        = 12'h100,
        CSR_SIE            = 12'h104,
        CSR_STVEC          = 12'h105,
        CSR_SCOUNTEREN     = 12'h106,
        CSR_SSCRATCH       = 12'h140,
        CSR_SEPC           = 12'h141,
        CSR_SCAUSE         = 12'h142,
        CSR_STVAL          = 12'h143,
        CSR_SIP            = 12'h144,
        CSR_SATP           = 12'h180,
        CSR_MSTATUS        = 12'h300,
        CSR_MISA           = 12'h301,
        CSR_MEDELEG        = 12'h302,
        CSR_MIDELEG        = 12'h303,
        CSR_MIE            = 12'h304,
        CSR_MTVEC          = 12'h305,
        CSR_MCOUNTEREN     = 12'h306,
        CSR_MSCRATCH       = 12'h340,
        CSR_MEPC           = 12'h341,
        CSR_MCAUSE         = 12'h342,
        CSR_MTVAL          = 12'h343,
        CSR_MIP            = 12'h344,
        CSR_PMPCFG0        = 12'h3A0,
        CSR_PMPCFG1        = 12'h3A1,
        CSR_PMPCFG2        = 12'h3A2,
        CSR_PMPCFG3        = 12'h3A3,
        CSR_PMPADDR0       = 12'h3B0,
        CSR_PMPADDR1       = 12'h3B1,
        CSR_PMPADDR2       = 12'h3B2,
        CSR_PMPADDR3       = 12'h3B3,
        CSR_PMPADDR4       = 12'h3B4,
        CSR_PMPADDR5       = 12'h3B5,
        CSR_PMPADDR6       = 12'h3B6,
        CSR_PMPADDR7       = 12'h3B7,
        CSR_PMPADDR8       = 12'h3B8,
        CSR_PMPADDR9       = 12'h3B9,
        CSR_PMPADDR10      = 12'h3BA,
        CSR_PMPADDR11      = 12'h3BB,
        CSR_PMPADDR12      = 12'h3BC,
        CSR_PMPADDR13      = 12'h3BD,
        CSR_PMPADDR14      = 12'h3BE,
        CSR_PMPADDR15      = 12'h3BF,
        CSR_MVENDORID      = 12'hF11,
        CSR_MARCHID        = 12'hF12,
        CSR_MIMPID         = 12'hF13,
        CSR_MHARTID        = 12'hF14,
        CSR_MCYCLE         = 12'hB00,
        CSR_MINSTRET       = 12'hB02,
        CSR_ML1_ICACHE_MISS = 12'hB03,   
        CSR_ML1_DCACHE_MISS = 12'hB04,   
        CSR_MITLB_MISS      = 12'hB05,   
        CSR_MDTLB_MISS      = 12'hB06,   
        CSR_MLOAD           = 12'hB07,   
        CSR_MSTORE          = 12'hB08,   
        CSR_MEXCEPTION      = 12'hB09,   
        CSR_MEXCEPTION_RET  = 12'hB0A,   
        CSR_MBRANCH_JUMP    = 12'hB0B,   
        CSR_MCALL           = 12'hB0C,   
        CSR_MRET            = 12'hB0D,   
        CSR_MMIS_PREDICT    = 12'hB0E,   
        CSR_MSB_FULL        = 12'hB0F,   
        CSR_MIF_EMPTY       = 12'hB10,   
        CSR_MHPM_COUNTER_17 = 12'hB11,   
        CSR_MHPM_COUNTER_18 = 12'hB12,   
        CSR_MHPM_COUNTER_19 = 12'hB13,   
        CSR_MHPM_COUNTER_20 = 12'hB14,   
        CSR_MHPM_COUNTER_21 = 12'hB15,   
        CSR_MHPM_COUNTER_22 = 12'hB16,   
        CSR_MHPM_COUNTER_23 = 12'hB17,   
        CSR_MHPM_COUNTER_24 = 12'hB18,   
        CSR_MHPM_COUNTER_25 = 12'hB19,   
        CSR_MHPM_COUNTER_26 = 12'hB1A,   
        CSR_MHPM_COUNTER_27 = 12'hB1B,   
        CSR_MHPM_COUNTER_28 = 12'hB1C,   
        CSR_MHPM_COUNTER_29 = 12'hB1D,   
        CSR_MHPM_COUNTER_30 = 12'hB1E,   
        CSR_MHPM_COUNTER_31 = 12'hB1F,   
        CSR_DCACHE         = 12'h701,
        CSR_ICACHE         = 12'h700,
        CSR_TSELECT        = 12'h7A0,
        CSR_TDATA1         = 12'h7A1,
        CSR_TDATA2         = 12'h7A2,
        CSR_TDATA3         = 12'h7A3,
        CSR_TINFO          = 12'h7A4,
        CSR_DCSR           = 12'h7b0,
        CSR_DPC            = 12'h7b1,
        CSR_DSCRATCH0      = 12'h7b2,  
        CSR_DSCRATCH1      = 12'h7b3,  
        CSR_CYCLE          = 12'hC00,
        CSR_TIME           = 12'hC01,
        CSR_INSTRET        = 12'hC02,
        CSR_L1_ICACHE_MISS = 12'hC03,   
        CSR_L1_DCACHE_MISS = 12'hC04,   
        CSR_ITLB_MISS      = 12'hC05,   
        CSR_DTLB_MISS      = 12'hC06,   
        CSR_LOAD           = 12'hC07,   
        CSR_STORE          = 12'hC08,   
        CSR_EXCEPTION      = 12'hC09,   
        CSR_EXCEPTION_RET  = 12'hC0A,   
        CSR_BRANCH_JUMP    = 12'hC0B,   
        CSR_CALL           = 12'hC0C,   
        CSR_RET            = 12'hC0D,   
        CSR_MIS_PREDICT    = 12'hC0E,   
        CSR_SB_FULL        = 12'hC0F,   
        CSR_IF_EMPTY       = 12'hC10,   
        CSR_HPM_COUNTER_17 = 12'hC11,   
        CSR_HPM_COUNTER_18 = 12'hC12,   
        CSR_HPM_COUNTER_19 = 12'hC13,   
        CSR_HPM_COUNTER_20 = 12'hC14,   
        CSR_HPM_COUNTER_21 = 12'hC15,   
        CSR_HPM_COUNTER_22 = 12'hC16,   
        CSR_HPM_COUNTER_23 = 12'hC17,   
        CSR_HPM_COUNTER_24 = 12'hC18,   
        CSR_HPM_COUNTER_25 = 12'hC19,   
        CSR_HPM_COUNTER_26 = 12'hC1A,   
        CSR_HPM_COUNTER_27 = 12'hC1B,   
        CSR_HPM_COUNTER_28 = 12'hC1C,   
        CSR_HPM_COUNTER_29 = 12'hC1D,   
        CSR_HPM_COUNTER_30 = 12'hC1E,   
        CSR_HPM_COUNTER_31 = 12'hC1F   
    } csr_reg_t;
    localparam logic [63:0] SSTATUS_UIE  = 'h00000001;
    localparam logic [63:0] SSTATUS_SIE  = 'h00000002;
    localparam logic [63:0] SSTATUS_SPIE = 'h00000020;
    localparam logic [63:0] SSTATUS_SPP  = 'h00000100;
    localparam logic [63:0] SSTATUS_FS   = 'h00006000;
    localparam logic [63:0] SSTATUS_XS   = 'h00018000;
    localparam logic [63:0] SSTATUS_SUM  = 'h00040000;
    localparam logic [63:0] SSTATUS_MXR  = 'h00080000;
    localparam logic [63:0] SSTATUS_UPIE = 'h00000010;
    localparam logic [63:0] SSTATUS_UXL  = 64'h0000000300000000;
    localparam logic [63:0] SSTATUS_SD   = {IS_XLEN64, 31'h00000000, ~IS_XLEN64, 31'h00000000};
    localparam logic [63:0] MSTATUS_UIE  = 'h00000001;
    localparam logic [63:0] MSTATUS_SIE  = 'h00000002;
    localparam logic [63:0] MSTATUS_HIE  = 'h00000004;
    localparam logic [63:0] MSTATUS_MIE  = 'h00000008;
    localparam logic [63:0] MSTATUS_UPIE = 'h00000010;
    localparam logic [63:0] MSTATUS_SPIE = 'h00000020;
    localparam logic [63:0] MSTATUS_HPIE = 'h00000040;
    localparam logic [63:0] MSTATUS_MPIE = 'h00000080;
    localparam logic [63:0] MSTATUS_SPP  = 'h00000100;
    localparam logic [63:0] MSTATUS_HPP  = 'h00000600;
    localparam logic [63:0] MSTATUS_MPP  = 'h00001800;
    localparam logic [63:0] MSTATUS_FS   = 'h00006000;
    localparam logic [63:0] MSTATUS_XS   = 'h00018000;
    localparam logic [63:0] MSTATUS_MPRV = 'h00020000;
    localparam logic [63:0] MSTATUS_SUM  = 'h00040000;
    localparam logic [63:0] MSTATUS_MXR  = 'h00080000;
    localparam logic [63:0] MSTATUS_TVM  = 'h00100000;
    localparam logic [63:0] MSTATUS_TW   = 'h00200000;
    localparam logic [63:0] MSTATUS_TSR  = 'h00400000;
    localparam logic [63:0] MSTATUS_UXL  = {30'h0000000, IS_XLEN64, IS_XLEN64, 32'h00000000};
    localparam logic [63:0] MSTATUS_SXL  = {28'h0000000, IS_XLEN64, IS_XLEN64, 34'h00000000};
    localparam logic [63:0] MSTATUS_SD   = {IS_XLEN64, 31'h00000000, ~IS_XLEN64, 31'h00000000};
    typedef enum logic [2:0] {
        CSRRW  = 3'h1,
        CSRRS  = 3'h2,
        CSRRC  = 3'h3,
        CSRRWI = 3'h5,
        CSRRSI = 3'h6,
        CSRRCI = 3'h7
    } csr_op_t;
    typedef struct packed {
        logic [1:0]  rw;
        priv_lvl_t   priv_lvl;
        logic  [7:0] address;
    } csr_addr_t;
    typedef union packed {
        csr_reg_t   address;
        csr_addr_t  csr_decode;
    } csr_t;
    typedef struct packed {
        logic [31:15] reserved;   
        logic [6:0]   fprec;      
        logic [2:0]   frm;        
        logic [4:0]   fflags;     
    } fcsr_t;
    typedef enum logic [1:0] {
        OFF   = 2'b00,
        TOR   = 2'b01,
        NA4   = 2'b10,
        NAPOT = 2'b11
    } pmp_addr_mode_t;
    typedef enum logic [2:0] {
        ACCESS_NONE  = 3'b000,
        ACCESS_READ  = 3'b001,
        ACCESS_WRITE = 3'b010,
        ACCESS_EXEC  = 3'b100
    } pmp_access_t;
    typedef struct packed {
        logic           x;
        logic           w;
        logic           r;
    } pmpcfg_access_t;
    typedef struct packed {
        logic           locked;      
        logic [1:0]     reserved;
        pmp_addr_mode_t addr_mode;   
        pmpcfg_access_t access_type;
    } pmpcfg_t;
    typedef struct packed {
        logic [31:28]     xdebugver;
        logic [27:16]     zero2;
        logic             ebreakm;
        logic             zero1;
        logic             ebreaks;
        logic             ebreaku;
        logic             stepie;
        logic             stopcount;
        logic             stoptime;
        logic [8:6]       cause;
        logic             zero0;
        logic             mprven;
        logic             nmip;
        logic             step;
        priv_lvl_t        prv;
    } dcsr_t;
    function automatic logic [31:0] jal (logic[4:0] rd, logic [20:0] imm);
        return {imm[20], imm[10:1], imm[11], imm[19:12], rd, 7'h6f};
    endfunction
    function automatic logic [31:0] jalr (logic[4:0] rd, logic[4:0] rs1, logic [11:0] offset);
        return {offset[11:0], rs1, 3'b0, rd, 7'h67};
    endfunction
    function automatic logic [31:0] andi (logic[4:0] rd, logic[4:0] rs1, logic [11:0] imm);
        return {imm[11:0], rs1, 3'h7, rd, 7'h13};
    endfunction
    function automatic logic [31:0] slli (logic[4:0] rd, logic[4:0] rs1, logic [5:0] shamt);
        return {6'b0, shamt[5:0], rs1, 3'h1, rd, 7'h13};
    endfunction
    function automatic logic [31:0] srli (logic[4:0] rd, logic[4:0] rs1, logic [5:0] shamt);
        return {6'b0, shamt[5:0], rs1, 3'h5, rd, 7'h13};
    endfunction
    function automatic logic [31:0] load (logic [2:0] size, logic[4:0] dest, logic[4:0] base, logic [11:0] offset);
        return {offset[11:0], base, size, dest, 7'h03};
    endfunction
    function automatic logic [31:0] auipc (logic[4:0] rd, logic [20:0] imm);
        return {imm[20], imm[10:1], imm[11], imm[19:12], rd, 7'h17};
    endfunction
    function automatic logic [31:0] store (logic [2:0] size, logic[4:0] src, logic[4:0] base, logic [11:0] offset);
        return {offset[11:5], src, base, size, offset[4:0], 7'h23};
    endfunction
    function automatic logic [31:0] float_load (logic [2:0] size, logic[4:0] dest, logic[4:0] base, logic [11:0] offset);
        return {offset[11:0], base, size, dest, 7'b00_001_11};
    endfunction
    function automatic logic [31:0] float_store (logic [2:0] size, logic[4:0] src, logic[4:0] base, logic [11:0] offset);
        return {offset[11:5], src, base, size, offset[4:0], 7'b01_001_11};
    endfunction
    function automatic logic [31:0] csrw (csr_reg_t csr, logic[4:0] rs1);
        return {csr, rs1, 3'h1, 5'h0, 7'h73};
    endfunction
    function automatic logic [31:0] csrr (csr_reg_t csr, logic [4:0] dest);
        return {csr, 5'h0, 3'h2, dest, 7'h73};
    endfunction
    function automatic logic [31:0] branch(logic [4:0] src2, logic [4:0] src1, logic [2:0] funct3, logic [11:0] offset);
        return {offset[11], offset[9:4], src2, src1, funct3, offset[3:0], offset[10], 7'b11_000_11};
    endfunction
    function automatic logic [31:0] ebreak ();
        return 32'h00100073;
    endfunction
    function automatic logic [31:0] wfi ();
        return 32'h10500073;
    endfunction
    function automatic logic [31:0] nop ();
        return 32'h00000013;
    endfunction
    function automatic logic [31:0] illegal ();
        return 32'h00000000;
    endfunction
    function string spikeCommitLog(logic [63:0] pc, priv_lvl_t priv_lvl, logic [31:0] instr, logic [4:0] rd, logic [63:0] result, logic rd_fpr);
        string rd_s;
        string instr_word;
        automatic string rf_s = rd_fpr ? "f" : "x";
        if (instr[1:0] != 2'b11) begin
          instr_word = $sformatf("(0x%h)", instr[15:0]);
        end else begin
          instr_word = $sformatf("(0x%h)", instr);
        end
        if (rd < 10) rd_s = $sformatf("%s %0d", rf_s, rd);
        else rd_s = $sformatf("%s%0d", rf_s, rd);
        if (rd_fpr || rd != 0) begin
            return $sformatf("%d 0x%h %s %s 0x%h\n", priv_lvl, pc, instr_word, rd_s, result);
        end else begin
            return $sformatf("%d 0x%h %s\n", priv_lvl, pc, instr_word);
        end
    endfunction
    typedef struct {
        byte priv;
        longint unsigned pc;
        byte is_fp;
        byte rd;
        longint unsigned data;
        int unsigned instr;
        byte was_exception;
    } commit_log_t;
endpackage

package dm;
    localparam logic [3:0] DbgVersion013 = 4'h2;
    localparam logic [4:0] ProgBufSize   = 5'h8;
    localparam logic [3:0] DataCount     = 4'h2;
    localparam logic [63:0] HaltAddress = 64'h800;
    localparam logic [63:0] ResumeAddress = HaltAddress + 4;
    localparam logic [63:0] ExceptionAddress = HaltAddress + 8;
    localparam logic [11:0] DataAddr = 12'h380;  
    typedef enum logic [7:0] {
        Data0        = 8'h04,
        Data1        = 8'h05,
        Data2        = 8'h06,
        Data3        = 8'h07,
        Data4        = 8'h08,
        Data5        = 8'h09,
        Data6        = 8'h0A,
        Data7        = 8'h0B,
        Data8        = 8'h0C,
        Data9        = 8'h0D,
        Data10       = 8'h0E,
        Data11       = 8'h0F,
        DMControl    = 8'h10,
        DMStatus     = 8'h11,  
        Hartinfo     = 8'h12,
        HaltSum1     = 8'h13,
        HAWindowSel  = 8'h14,
        HAWindow     = 8'h15,
        AbstractCS   = 8'h16,
        Command      = 8'h17,
        AbstractAuto = 8'h18,
        DevTreeAddr0 = 8'h19,
        DevTreeAddr1 = 8'h1A,
        DevTreeAddr2 = 8'h1B,
        DevTreeAddr3 = 8'h1C,
        NextDM       = 8'h1D,
        ProgBuf0     = 8'h20,
        ProgBuf15    = 8'h2F,
        AuthData     = 8'h30,
        HaltSum2     = 8'h34,
        HaltSum3     = 8'h35,
        SBAddress3   = 8'h37,
        SBCS         = 8'h38,
        SBAddress0   = 8'h39,
        SBAddress1   = 8'h3A,
        SBAddress2   = 8'h3B,
        SBData0      = 8'h3C,
        SBData1      = 8'h3D,
        SBData2      = 8'h3E,
        SBData3      = 8'h3F,
        HaltSum0     = 8'h40
    } dm_csr_e;
    localparam logic [2:0] CauseBreakpoint = 3'h1;
    localparam logic [2:0] CauseTrigger    = 3'h2;
    localparam logic [2:0] CauseRequest    = 3'h3;
    localparam logic [2:0] CauseSingleStep = 3'h4;
    typedef struct packed {
        logic [31:23] zero1;
        logic         impebreak;
        logic [21:20] zero0;
        logic         allhavereset;
        logic         anyhavereset;
        logic         allresumeack;
        logic         anyresumeack;
        logic         allnonexistent;
        logic         anynonexistent;
        logic         allunavail;
        logic         anyunavail;
        logic         allrunning;
        logic         anyrunning;
        logic         allhalted;
        logic         anyhalted;
        logic         authenticated;
        logic         authbusy;
        logic         hasresethaltreq;
        logic         devtreevalid;
        logic [3:0]   version;
    } dmstatus_t;
    typedef struct packed {
        logic         haltreq;
        logic         resumereq;
        logic         hartreset;
        logic         ackhavereset;
        logic         zero1;
        logic         hasel;
        logic [25:16] hartsello;
        logic [15:6]  hartselhi;
        logic [5:4]   zero0;
        logic         setresethaltreq;
        logic         clrresethaltreq;
        logic         ndmreset;
        logic         dmactive;
    } dmcontrol_t;
    typedef struct packed {
        logic [31:24] zero1;
        logic [23:20] nscratch;
        logic [19:17] zero0;
        logic         dataaccess;
        logic [15:12] datasize;
        logic [11:0]  dataaddr;
    } hartinfo_t;
    typedef enum logic [2:0] {  CmdErrNone, CmdErrBusy, CmdErrNotSupported,
                                CmdErrorException, CmdErrorHaltResume,
                                CmdErrorBus, CmdErrorOther = 7
                             } cmderr_e;
    typedef struct packed {
        logic [31:29] zero3;
        logic [28:24] progbufsize;
        logic [23:13] zero2;
        logic         busy;
        logic         zero1;
        cmderr_e      cmderr;
        logic [7:4]   zero0;
        logic [3:0]   datacount;
    } abstractcs_t;
    typedef enum logic [7:0] {
                                 AccessRegister = 8'h0,
                                 QuickAccess    = 8'h1,
                                 AccessMemory   = 8'h2
                             } cmd_e;
    typedef struct packed {
        cmd_e        cmdtype;
        logic [23:0] control;
    } command_t;
    typedef struct packed {
        logic [31:16] autoexecprogbuf;
        logic [15:12] zero0;
        logic [11:0]  autoexecdata;
    } abstractauto_t;
    typedef struct packed {
        logic         zero1;
        logic [22:20] aarsize;
        logic         aarpostincrement;
        logic         postexec;
        logic         transfer;
        logic         write;
        logic [15:0]  regno;
    } ac_ar_cmd_t;
    typedef enum logic [1:0] {
        DTM_NOP   = 2'h0,
        DTM_READ  = 2'h1,
        DTM_WRITE = 2'h2
    } dtm_op_e;
    typedef struct packed {
        logic [31:29] sbversion;
        logic [28:23] zero0;
        logic         sbbusyerror;
        logic         sbbusy;
        logic         sbreadonaddr;
        logic [19:17] sbaccess;
        logic         sbautoincrement;
        logic         sbreadondata;
        logic [14:12] sberror;
        logic [11:5]  sbasize;
        logic         sbaccess128;
        logic         sbaccess64;
        logic         sbaccess32;
        logic         sbaccess16;
        logic         sbaccess8;
    } sbcs_t;
    localparam logic[1:0] DTM_SUCCESS = 2'h0;
    typedef struct packed {
        logic [6:0]  addr;
        dtm_op_e     op;
        logic [31:0] data;
    } dmi_req_t;
    typedef struct packed  {
        logic [31:0] data;
        logic [1:0]  resp;
    } dmi_resp_t;
    typedef enum logic[1:0] {
      PRIV_LVL_M = 2'b11,
      PRIV_LVL_S = 2'b01,
      PRIV_LVL_U = 2'b00
    } priv_lvl_t;
    typedef struct packed {
        logic [31:28]     xdebugver;
        logic [27:16]     zero2;
        logic             ebreakm;
        logic             zero1;
        logic             ebreaks;
        logic             ebreaku;
        logic             stepie;
        logic             stopcount;
        logic             stoptime;
        logic [8:6]       cause;
        logic             zero0;
        logic             mprven;
        logic             nmip;
        logic             step;
        priv_lvl_t        prv;
    } dcsr_t;
    typedef enum logic [11:0] {
        CSR_FFLAGS         = 12'h001,
        CSR_FRM            = 12'h002,
        CSR_FCSR           = 12'h003,
        CSR_FTRAN          = 12'h800,
        CSR_SSTATUS        = 12'h100,
        CSR_SIE            = 12'h104,
        CSR_STVEC          = 12'h105,
        CSR_SCOUNTEREN     = 12'h106,
        CSR_SSCRATCH       = 12'h140,
        CSR_SEPC           = 12'h141,
        CSR_SCAUSE         = 12'h142,
        CSR_STVAL          = 12'h143,
        CSR_SIP            = 12'h144,
        CSR_SATP           = 12'h180,
        CSR_MSTATUS        = 12'h300,
        CSR_MISA           = 12'h301,
        CSR_MEDELEG        = 12'h302,
        CSR_MIDELEG        = 12'h303,
        CSR_MIE            = 12'h304,
        CSR_MTVEC          = 12'h305,
        CSR_MCOUNTEREN     = 12'h306,
        CSR_MSCRATCH       = 12'h340,
        CSR_MEPC           = 12'h341,
        CSR_MCAUSE         = 12'h342,
        CSR_MTVAL          = 12'h343,
        CSR_MIP            = 12'h344,
        CSR_PMPCFG0        = 12'h3A0,
        CSR_PMPADDR0       = 12'h3B0,
        CSR_MVENDORID      = 12'hF11,
        CSR_MARCHID        = 12'hF12,
        CSR_MIMPID         = 12'hF13,
        CSR_MHARTID        = 12'hF14,
        CSR_MCYCLE         = 12'hB00,
        CSR_MINSTRET       = 12'hB02,
        CSR_DCACHE         = 12'h701,
        CSR_ICACHE         = 12'h700,
        CSR_TSELECT        = 12'h7A0,
        CSR_TDATA1         = 12'h7A1,
        CSR_TDATA2         = 12'h7A2,
        CSR_TDATA3         = 12'h7A3,
        CSR_TINFO          = 12'h7A4,
        CSR_DCSR           = 12'h7b0,
        CSR_DPC            = 12'h7b1,
        CSR_DSCRATCH0      = 12'h7b2,  
        CSR_DSCRATCH1      = 12'h7b3,  
        CSR_CYCLE          = 12'hC00,
        CSR_TIME           = 12'hC01,
        CSR_INSTRET        = 12'hC02
    } csr_reg_t;
    function automatic logic [31:0] jal (logic[4:0] rd, logic [20:0] imm);
        return {imm[20], imm[10:1], imm[11], imm[19:12], rd, 7'h6f};
    endfunction
    function automatic logic [31:0] jalr (logic[4:0] rd, logic[4:0] rs1, logic [11:0] offset);
        return {offset[11:0], rs1, 3'b0, rd, 7'h67};
    endfunction
    function automatic logic [31:0] andi (logic[4:0] rd, logic[4:0] rs1, logic [11:0] imm);
        return {imm[11:0], rs1, 3'h7, rd, 7'h13};
    endfunction
    function automatic logic [31:0] slli (logic[4:0] rd, logic[4:0] rs1, logic [5:0] shamt);
        return {6'b0, shamt[5:0], rs1, 3'h1, rd, 7'h13};
    endfunction
    function automatic logic [31:0] srli (logic[4:0] rd, logic[4:0] rs1, logic [5:0] shamt);
        return {6'b0, shamt[5:0], rs1, 3'h5, rd, 7'h13};
    endfunction
    function automatic logic [31:0] load (logic [2:0] size, logic[4:0] dest, logic[4:0] base, logic [11:0] offset);
        return {offset[11:0], base, size, dest, 7'h03};
    endfunction
    function automatic logic [31:0] auipc (logic[4:0] rd, logic [20:0] imm);
        return {imm[20], imm[10:1], imm[11], imm[19:12], rd, 7'h17};
    endfunction
    function automatic logic [31:0] store (logic [2:0] size, logic[4:0] src, logic[4:0] base, logic [11:0] offset);
        return {offset[11:5], src, base, size, offset[4:0], 7'h23};
    endfunction
    function automatic logic [31:0] float_load (logic [2:0] size, logic[4:0] dest, logic[4:0] base, logic [11:0] offset);
        return {offset[11:0], base, size, dest, 7'b00_001_11};
    endfunction
    function automatic logic [31:0] float_store (logic [2:0] size, logic[4:0] src, logic[4:0] base, logic [11:0] offset);
        return {offset[11:5], src, base, size, offset[4:0], 7'b01_001_11};
    endfunction
    function automatic logic [31:0] csrw (csr_reg_t csr, logic[4:0] rs1);
        return {csr, rs1, 3'h1, 5'h0, 7'h73};
    endfunction
    function automatic logic [31:0] csrr (csr_reg_t csr, logic [4:0] dest);
        return {csr, 5'h0, 3'h2, dest, 7'h73};
    endfunction
    function automatic logic [31:0] branch(logic [4:0] src2, logic [4:0] src1, logic [2:0] funct3, logic [11:0] offset);
        return {offset[11], offset[9:4], src2, src1, funct3, offset[3:0], offset[10], 7'b11_000_11};
    endfunction
    function automatic logic [31:0] ebreak ();
        return 32'h00100073;
    endfunction
    function automatic logic [31:0] wfi ();
        return 32'h10500073;
    endfunction
    function automatic logic [31:0] nop ();
        return 32'h00000013;
    endfunction
    function automatic logic [31:0] illegal ();
        return 32'h00000000;
    endfunction
endpackage
   
   package ariane_pkg;
    localparam NrMaxRules = 16;
    typedef struct packed {
      int                               RASDepth;
      int                               BTBEntries;
      int                               BHTEntries;
      int unsigned                      NrNonIdempotentRules;   
      logic [NrMaxRules-1:0][63:0]      NonIdempotentAddrBase;  
      logic [NrMaxRules-1:0][63:0]      NonIdempotentLength;    
      int unsigned                      NrExecuteRegionRules;   
      logic [NrMaxRules-1:0][63:0]      ExecuteRegionAddrBase;  
      logic [NrMaxRules-1:0][63:0]      ExecuteRegionLength;    
      int unsigned                      NrCachedRegionRules;    
      logic [NrMaxRules-1:0][63:0]      CachedRegionAddrBase;   
      logic [NrMaxRules-1:0][63:0]      CachedRegionLength;     
      bit                               Axi64BitCompliant;      
      bit                               SwapEndianess;          
      logic [63:0]                      DmBaseAddress;          
      int unsigned                      NrPMPEntries;           
    } ariane_cfg_t;
    localparam ariane_cfg_t ArianeDefaultConfig = '{
      RASDepth: 2,
      BTBEntries: 32,
      BHTEntries: 128,
      NrNonIdempotentRules: 2,
      NonIdempotentAddrBase: {64'b0, 64'b0},
      NonIdempotentLength:   {64'b0, 64'b0},
      NrExecuteRegionRules: 3,
      ExecuteRegionAddrBase: {64'h8000_0000, 64'h1_0000, 64'h0},
      ExecuteRegionLength:   {64'h40000000,  64'h10000,  64'h1000},
      NrCachedRegionRules:    1,
      CachedRegionAddrBase:  {64'h8000_0000},
      CachedRegionLength:    {64'h40000000},
      Axi64BitCompliant:      1'b1,
      SwapEndianess:          1'b0,
      DmBaseAddress:          64'h0,
      NrPMPEntries:           8
    };
    function automatic void check_cfg (ariane_cfg_t Cfg);
    endfunction
    function automatic logic range_check(logic[63:0] base, logic[63:0] len, logic[63:0] address);
      return (address >= base) && (address < (base+len));
    endfunction : range_check
    function automatic logic is_inside_nonidempotent_regions (ariane_cfg_t Cfg, logic[63:0] address);
      logic[NrMaxRules-1:0] pass;
      pass = '0;
      for (int unsigned k = 0; k < Cfg.NrNonIdempotentRules; k++) begin
        pass[k] = range_check(Cfg.NonIdempotentAddrBase[k], Cfg.NonIdempotentLength[k], address);
      end
      return |pass;
    endfunction : is_inside_nonidempotent_regions
    function automatic logic is_inside_execute_regions (ariane_cfg_t Cfg, logic[63:0] address);
      logic[NrMaxRules-1:0] pass;
      pass = '0;
      for (int unsigned k = 0; k < Cfg.NrExecuteRegionRules; k++) begin
        pass[k] = range_check(Cfg.ExecuteRegionAddrBase[k], Cfg.ExecuteRegionLength[k], address);
      end
      return |pass;
    endfunction : is_inside_execute_regions
    function automatic logic is_inside_cacheable_regions (ariane_cfg_t Cfg, logic[63:0] address);
      automatic logic[NrMaxRules-1:0] pass;
      pass = '0;
      for (int unsigned k = 0; k < Cfg.NrCachedRegionRules; k++) begin
        pass[k] = range_check(Cfg.CachedRegionAddrBase[k], Cfg.CachedRegionLength[k], address);
      end
      return |pass;
    endfunction : is_inside_cacheable_regions
    localparam NR_SB_ENTRIES = 8;  
    localparam TRANS_ID_BITS = $clog2(NR_SB_ENTRIES);  
    localparam ASID_WIDTH    = (riscv::XLEN == 64) ? 16 : 1;
    localparam BITS_SATURATION_COUNTER = 2;
    localparam NR_COMMIT_PORTS = 2;
    localparam ENABLE_RENAME = 1'b0;
    localparam ISSUE_WIDTH = 1;
    localparam int unsigned NR_LOAD_PIPE_REGS = 1;
    localparam int unsigned NR_STORE_PIPE_REGS = 0;
    localparam int unsigned DEPTH_SPEC   = 4;
    localparam int unsigned DEPTH_COMMIT = 4;
    localparam bit RVF = (riscv::IS_XLEN64 | riscv::IS_XLEN32) & riscv::FPU_EN;  
    localparam bit RVD = (riscv::IS_XLEN64 ? 1:0) & riscv::FPU_EN;               
    localparam bit RVA = 1'b1;  
    localparam bit XF16    = 1'b0;  
    localparam bit XF16ALT = 1'b0;  
    localparam bit XF8     = 1'b0;  
    localparam bit XFVEC   = 1'b0;  
    localparam int unsigned LAT_COMP_FP32    = 'd2;
    localparam int unsigned LAT_COMP_FP64    = 'd3;
    localparam int unsigned LAT_COMP_FP16    = 'd1;
    localparam int unsigned LAT_COMP_FP16ALT = 'd1;
    localparam int unsigned LAT_COMP_FP8     = 'd1;
    localparam int unsigned LAT_DIVSQRT      = 'd2;
    localparam int unsigned LAT_NONCOMP      = 'd1;
    localparam int unsigned LAT_CONV         = 'd2;
    localparam bit FP_PRESENT = RVF | RVD | XF16 | XF16ALT | XF8;
    localparam FLEN    = RVD     ? 64 :  
                         RVF     ? 32 :  
                         XF16    ? 16 :  
                         XF16ALT ? 16 :  
                         XF8     ? 8 :   
                         1;              
    localparam bit NSX = XF16 | XF16ALT | XF8 | XFVEC;  
    localparam bit RVFVEC     = RVF     & XFVEC & FLEN>32;  
    localparam bit XF16VEC    = XF16    & XFVEC & FLEN>16;  
    localparam bit XF16ALTVEC = XF16ALT & XFVEC & FLEN>16;  
    localparam bit XF8VEC     = XF8     & XFVEC & FLEN>8;   
    localparam riscv::xlen_t ARIANE_MARCHID = {{riscv::XLEN-32{1'b0}}, 32'd3};
    localparam riscv::xlen_t ISA_CODE = (RVA <<  0)   
                                     | (1   <<  2)   
                                     | (RVD <<  3)   
                                     | (RVF <<  5)   
                                     | (1   <<  8)   
                                     | (1   << 12)   
                                     | (0   << 13)   
                                     | (1   << 18)   
                                     | (1   << 20)   
                                     | (NSX << 23)   
                                     | ((riscv::XLEN == 64 ? 2 : 1) << riscv::XLEN-2);   
    localparam REG_ADDR_SIZE = 6;
    localparam NR_WB_PORTS = 4;
    localparam dm::hartinfo_t DebugHartInfo = '{
                                                zero1:        '0,
                                                nscratch:      2,  
                                                zero0:        '0,
                                                dataaccess: 1'b1,  
                                                datasize: dm::DataCount,
                                                dataaddr: dm::DataAddr
                                              };
    localparam bit ENABLE_SPIKE_COMMIT_LOG = 1'b1;
    localparam logic INVALIDATE_ON_FLUSH = 1'b1;
    localparam bit ENABLE_CYCLE_COUNT = 1'b1;
    localparam bit ENABLE_WFI = 1'b1;
    localparam bit ZERO_TVAL = 1'b0;
    localparam logic [63:0] SMODE_STATUS_READ_MASK = riscv::SSTATUS_UIE
                                                   | riscv::SSTATUS_SIE
                                                   | riscv::SSTATUS_SPIE
                                                   | riscv::SSTATUS_SPP
                                                   | riscv::SSTATUS_FS
                                                   | riscv::SSTATUS_XS
                                                   | riscv::SSTATUS_SUM
                                                   | riscv::SSTATUS_MXR
                                                   | riscv::SSTATUS_UPIE
                                                   | riscv::SSTATUS_SPIE
                                                   | riscv::SSTATUS_UXL
                                                   | riscv::SSTATUS_SD;
    localparam logic [63:0] SMODE_STATUS_WRITE_MASK = riscv::SSTATUS_SIE
                                                    | riscv::SSTATUS_SPIE
                                                    | riscv::SSTATUS_SPP
                                                    | riscv::SSTATUS_FS
                                                    | riscv::SSTATUS_SUM
                                                    | riscv::SSTATUS_MXR;
    localparam int unsigned FETCH_FIFO_DEPTH  = 4;
    localparam int unsigned FETCH_WIDTH       = 32;
    localparam int unsigned INSTR_PER_FETCH = FETCH_WIDTH / 16;
    typedef struct packed {
         riscv::xlen_t       cause;  
         riscv::xlen_t       tval;   
         logic        valid;
    } exception_t;
    typedef enum logic [2:0] {
      NoCF,    
      Branch,  
      Jump,    
      JumpR,   
      Return   
    } cf_t;
    typedef struct packed {
        logic                   valid;            
        logic [riscv::VLEN-1:0] pc;               
        logic [riscv::VLEN-1:0] target_address;   
        logic                   is_mispredict;    
        logic                   is_taken;         
        cf_t                    cf_type;          
    } bp_resolve_t;
    typedef struct packed {
        cf_t                    cf;               
        logic [riscv::VLEN-1:0] predict_address;  
    } branchpredict_sbe_t;
    typedef struct packed {
        logic                   valid;
        logic [riscv::VLEN-1:0] pc;              
        logic [riscv::VLEN-1:0] target_address;
    } btb_update_t;
    typedef struct packed {
        logic                   valid;
        logic [riscv::VLEN-1:0] target_address;
    } btb_prediction_t;
    typedef struct packed {
        logic                   valid;
        logic [riscv::VLEN-1:0] ra;
    } ras_t;
    typedef struct packed {
        logic                   valid;
        logic [riscv::VLEN-1:0] pc;           
        logic                   taken;
    } bht_update_t;
    typedef struct packed {
        logic       valid;
        logic       taken;
    } bht_prediction_t;
    typedef enum logic[3:0] {
        NONE,       
        LOAD,       
        STORE,      
        ALU,        
        CTRL_FLOW,  
        MULT,       
        CSR,        
        FPU,        
        FPU_VEC     
    } fu_t;
    localparam EXC_OFF_RST      = 8'h80;
    localparam SupervisorIrq = 1;
    localparam MachineIrq = 0;
    typedef struct packed {
      riscv::xlen_t       mie;
      riscv::xlen_t       mip;
      riscv::xlen_t       mideleg;
      logic        sie;
      logic        global_enable;
    } irq_ctrl_t;
		localparam int unsigned CONFIG_L1I_SIZE    = 16*1024;
    localparam int unsigned ICACHE_SET_ASSOC   = 4;  
    localparam int unsigned ICACHE_INDEX_WIDTH = $clog2(CONFIG_L1I_SIZE / ICACHE_SET_ASSOC);   
    localparam int unsigned ICACHE_TAG_WIDTH   = riscv::PLEN-ICACHE_INDEX_WIDTH;   
    localparam int unsigned ICACHE_LINE_WIDTH  = 128;  
		localparam int unsigned CONFIG_L1D_SIZE    = 32*1024;
	  localparam int unsigned DCACHE_SET_ASSOC   = 8;  
    localparam int unsigned DCACHE_INDEX_WIDTH = $clog2(CONFIG_L1D_SIZE / DCACHE_SET_ASSOC);   
    localparam int unsigned DCACHE_TAG_WIDTH   = riscv::PLEN-DCACHE_INDEX_WIDTH;   
    localparam int unsigned DCACHE_LINE_WIDTH  = 128;  
    typedef enum logic [6:0] {  
                               ADD, SUB, ADDW, SUBW,
                               XORL, ORL, ANDL,
                               SRA, SRL, SLL, SRLW, SLLW, SRAW,
                               LTS, LTU, GES, GEU, EQ, NE,
                               JALR, BRANCH,
                               SLTS, SLTU,
                               MRET, SRET, DRET, ECALL, WFI, FENCE, FENCE_I, SFENCE_VMA, CSR_WRITE, CSR_READ, CSR_SET, CSR_CLEAR,
                               LD, SD, LW, LWU, SW, LH, LHU, SH, LB, SB, LBU,
                               AMO_LRW, AMO_LRD, AMO_SCW, AMO_SCD,
                               AMO_SWAPW, AMO_ADDW, AMO_ANDW, AMO_ORW, AMO_XORW, AMO_MAXW, AMO_MAXWU, AMO_MINW, AMO_MINWU,
                               AMO_SWAPD, AMO_ADDD, AMO_ANDD, AMO_ORD, AMO_XORD, AMO_MAXD, AMO_MAXDU, AMO_MIND, AMO_MINDU,
                               MUL, MULH, MULHU, MULHSU, MULW,
                               DIV, DIVU, DIVW, DIVUW, REM, REMU, REMW, REMUW,
                               FLD, FLW, FLH, FLB, FSD, FSW, FSH, FSB,
                               FADD, FSUB, FMUL, FDIV, FMIN_MAX, FSQRT, FMADD, FMSUB, FNMSUB, FNMADD,
                               FCVT_F2I, FCVT_I2F, FCVT_F2F, FSGNJ, FMV_F2X, FMV_X2F,
                               FCMP,
                               FCLASS,
                               VFMIN, VFMAX, VFSGNJ, VFSGNJN, VFSGNJX, VFEQ, VFNE, VFLT, VFGE, VFLE, VFGT, VFCPKAB_S, VFCPKCD_S, VFCPKAB_D, VFCPKCD_D
                             } fu_op;
    typedef struct packed {
        fu_t                      fu;
        fu_op                     operator;
        riscv::xlen_t             operand_a;
        riscv::xlen_t             operand_b;
        riscv::xlen_t             imm;
        logic [TRANS_ID_BITS-1:0] trans_id;
    } fu_data_t;
    function automatic logic op_is_branch (input fu_op op);
        unique case (op) inside
            EQ, NE, LTS, GES, LTU, GEU: return 1'b1;
            default                   : return 1'b0;  
        endcase
    endfunction
    function automatic logic is_rs1_fpr (input fu_op op);
        if (FP_PRESENT) begin  
            unique case (op) inside
                [FMUL:FNMADD],                    
                FCVT_F2I,                         
                FCVT_F2F,                         
                FSGNJ,                            
                FMV_F2X,                          
                FCMP,                             
                FCLASS,                           
                [VFMIN:VFCPKCD_D] : return 1'b1;  
                default           : return 1'b0;  
            endcase
        end else
            return 1'b0;
    endfunction
    function automatic logic is_rs2_fpr (input fu_op op);
        if (FP_PRESENT) begin  
            unique case (op) inside
                [FSD:FSB],                        
                [FADD:FMIN_MAX],                  
                [FMADD:FNMADD],                   
                FCVT_F2F,                         
                [FSGNJ:FMV_F2X],                  
                FCMP,                             
                [VFMIN:VFCPKCD_D] : return 1'b1;  
                default           : return 1'b0;  
            endcase
        end else
            return 1'b0;
    endfunction
    function automatic logic is_imm_fpr (input fu_op op);
        if (FP_PRESENT) begin  
            unique case (op) inside
                [FADD:FSUB],                          
                [FMADD:FNMADD],                       
                [VFCPKAB_S:VFCPKCD_D] : return 1'b1;  
                default               : return 1'b0;  
            endcase
        end else
            return 1'b0;
    endfunction
    function automatic logic is_rd_fpr (input fu_op op);
        if (FP_PRESENT) begin  
            unique case (op) inside
                [FLD:FLB],                            
                [FADD:FNMADD],                        
                FCVT_I2F,                             
                FCVT_F2F,                             
                FSGNJ,                                
                FMV_X2F,                              
                [VFMIN:VFSGNJX],                      
                [VFCPKAB_S:VFCPKCD_D] : return 1'b1;  
                default               : return 1'b0;  
            endcase
        end else
            return 1'b0;
    endfunction
    function automatic logic is_amo (fu_op op);
        case (op) inside
            [AMO_LRW:AMO_MINDU]: begin
                return 1'b1;
            end
            default: return 1'b0;
        endcase
    endfunction
    typedef struct packed {
        logic                     valid;
        logic [riscv::VLEN-1:0]   vaddr;
        logic                     overflow;
        logic [63:0]              data;
        logic [7:0]               be;
        fu_t                      fu;
        fu_op                     operator;
        logic [TRANS_ID_BITS-1:0] trans_id;
    } lsu_ctrl_t;
    typedef struct packed {
        logic [riscv::VLEN-1:0] address;         
        logic [31:0]            instruction;     
        branchpredict_sbe_t     branch_predict;  
        exception_t             ex;              
    } fetch_entry_t;
    typedef struct packed {
        logic [riscv::VLEN-1:0]   pc;             
        logic [TRANS_ID_BITS-1:0] trans_id;       
        fu_t                      fu;             
        fu_op                     op;             
        logic [REG_ADDR_SIZE-1:0] rs1;            
        logic [REG_ADDR_SIZE-1:0] rs2;            
        logic [REG_ADDR_SIZE-1:0] rd;             
        riscv::xlen_t             result;         
        logic                     valid;          
        logic                     use_imm;        
        logic                     use_zimm;       
        logic                     use_pc;         
        exception_t               ex;             
        branchpredict_sbe_t       bp;             
        logic                     is_compressed;  
    } scoreboard_entry_t;
     localparam bit MMU_PRESENT = 1'b1;   
    typedef enum logic [3:0] {
        AMO_NONE =4'b0000,
        AMO_LR   =4'b0001,
        AMO_SC   =4'b0010,
        AMO_SWAP =4'b0011,
        AMO_ADD  =4'b0100,
        AMO_AND  =4'b0101,
        AMO_OR   =4'b0110,
        AMO_XOR  =4'b0111,
        AMO_MAX  =4'b1000,
        AMO_MAXU =4'b1001,
        AMO_MIN  =4'b1010,
        AMO_MINU =4'b1011,
        AMO_CAS1 =4'b1100,  
        AMO_CAS2 =4'b1101   
    } amo_t;
    typedef struct packed {
        logic                  valid;       
        logic                  is_2M;      
        logic                  is_1G;      
        logic [27-1:0]         vpn;         
        logic [ASID_WIDTH-1:0] asid;
        riscv::pte_t           content;
    } tlb_update_t;
    localparam PPN4K_WIDTH = 38;
    typedef struct packed {
        logic                  valid;       
        logic                  is_4M;      
        logic [20-1:0]         vpn;         
        logic [9-1:0]          asid;        
        riscv::pte_sv32_t      content;
    } tlb_update_sv32_t;
    typedef enum logic [1:0] {
      FE_NONE,
      FE_INSTR_ACCESS_FAULT,
      FE_INSTR_PAGE_FAULT
    } frontend_exception_t;
    typedef struct packed {
        logic                     fetch_valid;      
        logic [riscv::PLEN-1:0]   fetch_paddr;      
        exception_t               fetch_exception;  
    } icache_areq_i_t;
    typedef struct packed {
        logic                     fetch_req;        
        logic [riscv::VLEN-1:0]   fetch_vaddr;      
    } icache_areq_o_t;
    typedef struct packed {
        logic                     req;                     
        logic                     kill_s1;                 
        logic                     kill_s2;                 
        logic                     spec;                    
        logic [riscv::VLEN-1:0]   vaddr;                   
    } icache_dreq_i_t;
    typedef struct packed {
        logic                     ready;                   
        logic                     valid;                   
        logic [FETCH_WIDTH-1:0]   data;                    
        logic [riscv::VLEN-1:0]   vaddr;                   
        exception_t               ex;                      
    } icache_dreq_o_t;
    typedef struct packed {
        logic        req;        
        amo_t        amo_op;     
        logic [1:0]  size;       
        logic [63:0] operand_a;  
        logic [63:0] operand_b;  
    } amo_req_t;
    typedef struct packed {
        logic        ack;     
        logic [63:0] result;  
    } amo_resp_t;
    typedef struct packed {
        logic [DCACHE_INDEX_WIDTH-1:0] address_index;
        logic [DCACHE_TAG_WIDTH-1:0]   address_tag;
        logic [63:0]                   data_wdata;
        logic                          data_req;
        logic                          data_we;
        logic [7:0]                    data_be;
        logic [1:0]                    data_size;
        logic                          kill_req;
        logic                          tag_valid;
    } dcache_req_i_t;
    typedef struct packed {
        logic                          data_gnt;
        logic                          data_rvalid;
        logic [63:0]                   data_rdata;
    } dcache_req_o_t;
    function automatic riscv::xlen_t sext32 (logic [31:0] operand);
        return {{riscv::XLEN-32{operand[31]}}, operand[31:0]};
    endfunction
    function automatic logic [riscv::VLEN-1:0] uj_imm (logic [31:0] instruction_i);
        return { {44+riscv::VLEN-64 {instruction_i[31]}}, instruction_i[19:12], instruction_i[20], instruction_i[30:21], 1'b0 };
    endfunction
    function automatic logic [riscv::VLEN-1:0] i_imm (logic [31:0] instruction_i);
        return { {52+riscv::VLEN-64 {instruction_i[31]}}, instruction_i[31:20] };
    endfunction
    function automatic logic [riscv::VLEN-1:0] sb_imm (logic [31:0] instruction_i);
        return { {51+riscv::VLEN-64 {instruction_i[31]}}, instruction_i[31], instruction_i[7], instruction_i[30:25], instruction_i[11:8], 1'b0 };
    endfunction
    function automatic riscv::xlen_t data_align (logic [2:0] addr, logic [63:0] data);
        logic [2:0] addr_tmp = {(addr[2] && riscv::IS_XLEN64), addr[1:0]};
        logic [63:0] data_tmp = {64{1'b0}};
        case (addr_tmp)
            3'b000: data_tmp[riscv::XLEN-1:0] = {data[riscv::XLEN-1:0]};
            3'b001: data_tmp[riscv::XLEN-1:0] = {data[riscv::XLEN-9:0],  data[riscv::XLEN-1:riscv::XLEN-8]};
            3'b010: data_tmp[riscv::XLEN-1:0] = {data[riscv::XLEN-17:0], data[riscv::XLEN-1:riscv::XLEN-16]};
            3'b011: data_tmp[riscv::XLEN-1:0] = {data[riscv::XLEN-25:0], data[riscv::XLEN-1:riscv::XLEN-24]};
            3'b100: data_tmp = {data[31:0], data[63:32]};
            3'b101: data_tmp = {data[23:0], data[63:24]};
            3'b110: data_tmp = {data[15:0], data[63:16]};
            3'b111: data_tmp = {data[7:0],  data[63:8]};
        endcase
        return data_tmp[riscv::XLEN-1:0];
    endfunction
    function automatic logic [7:0] be_gen(logic [2:0] addr, logic [1:0] size);
        case (size)
            2'b11: begin
                return 8'b1111_1111;
            end
            2'b10: begin
                case (addr[2:0])
                    3'b000: return 8'b0000_1111;
                    3'b001: return 8'b0001_1110;
                    3'b010: return 8'b0011_1100;
                    3'b011: return 8'b0111_1000;
                    3'b100: return 8'b1111_0000;
                endcase
            end
            2'b01: begin
                case (addr[2:0])
                    3'b000: return 8'b0000_0011;
                    3'b001: return 8'b0000_0110;
                    3'b010: return 8'b0000_1100;
                    3'b011: return 8'b0001_1000;
                    3'b100: return 8'b0011_0000;
                    3'b101: return 8'b0110_0000;
                    3'b110: return 8'b1100_0000;
                endcase
            end
            2'b00: begin
                case (addr[2:0])
                    3'b000: return 8'b0000_0001;
                    3'b001: return 8'b0000_0010;
                    3'b010: return 8'b0000_0100;
                    3'b011: return 8'b0000_1000;
                    3'b100: return 8'b0001_0000;
                    3'b101: return 8'b0010_0000;
                    3'b110: return 8'b0100_0000;
                    3'b111: return 8'b1000_0000;
                endcase
            end
        endcase
        return 8'b0;
    endfunction
    function automatic logic [1:0] extract_transfer_size(fu_op op);
        case (op)
            LD, SD, FLD, FSD,
            AMO_LRD,   AMO_SCD,
            AMO_SWAPD, AMO_ADDD,
            AMO_ANDD,  AMO_ORD,
            AMO_XORD,  AMO_MAXD,
            AMO_MAXDU, AMO_MIND,
            AMO_MINDU: begin
                return 2'b11;
            end
            LW, LWU, SW, FLW, FSW,
            AMO_LRW,   AMO_SCW,
            AMO_SWAPW, AMO_ADDW,
            AMO_ANDW,  AMO_ORW,
            AMO_XORW,  AMO_MAXW,
            AMO_MAXWU, AMO_MINW,
            AMO_MINWU: begin
                return 2'b10;
            end
            LH, LHU, SH, FLH, FSH: return 2'b01;
            LB, LBU, SB, FLB, FSB: return 2'b00;
            default:     return 2'b11;
        endcase
    endfunction
endpackage

package ariane_soc;
  localparam int unsigned NumTargets = 2;
  localparam int unsigned NumSources = 30;
  localparam int unsigned MaxPriority = 7;
  localparam NrSlaves = 2;  
  localparam IdWidth   = 4;
  localparam IdWidthSlave = IdWidth + $clog2(NrSlaves);
  typedef enum int unsigned {
    DRAM     = 0,
    GPIO     = 1,
    Ethernet = 2,
    SPI      = 3,
    Timer    = 4,
    UART     = 5,
    PLIC     = 6,
    CLINT    = 7,
    ROM      = 8,
    Debug    = 9
  } axi_slaves_t;
  localparam NB_PERIPHERALS = Debug + 1;
  localparam logic[63:0] DebugLength    = 64'h1000;
  localparam logic[63:0] ROMLength      = 64'h10000;
  localparam logic[63:0] CLINTLength    = 64'hC0000;
  localparam logic[63:0] PLICLength     = 64'h3FF_FFFF;
  localparam logic[63:0] UARTLength     = 64'h1000;
  localparam logic[63:0] TimerLength    = 64'h1000;
  localparam logic[63:0] SPILength      = 64'h800000;
  localparam logic[63:0] EthernetLength = 64'h10000;
  localparam logic[63:0] GPIOLength     = 64'h1000;
  localparam logic[63:0] DRAMLength     = 64'h40000000;  
  localparam logic[63:0] SRAMLength     = 64'h1800000;   
  localparam bit GenProtocolChecker = 1'b0;
  typedef enum logic [63:0] {
    DebugBase    = 64'h0000_0000,
    ROMBase      = 64'h0001_0000,
    CLINTBase    = 64'h0200_0000,
    PLICBase     = 64'h0C00_0000,
    UARTBase     = 64'h1000_0000,
    TimerBase    = 64'h1800_0000,
    SPIBase      = 64'h2000_0000,
    EthernetBase = 64'h3000_0000,
    GPIOBase     = 64'h4000_0000,
    DRAMBase     = 64'h8000_0000
  } soc_bus_start_t;
  localparam NrRegion = 1;
  localparam logic [NrRegion-1:0][NB_PERIPHERALS-1:0] ValidRule = {{NrRegion * NB_PERIPHERALS}{1'b1}};
  localparam ariane_pkg::ariane_cfg_t ArianeSocCfg = '{
    RASDepth: 2,
    BTBEntries: 32,
    BHTEntries: 128,
    NrNonIdempotentRules:  1,
    NonIdempotentAddrBase: {64'b0},
    NonIdempotentLength:   {DRAMBase},
    NrExecuteRegionRules:  3,
    ExecuteRegionAddrBase: {DRAMBase,   ROMBase,   DebugBase},
    ExecuteRegionLength:   {DRAMLength, ROMLength, DebugLength},
    NrCachedRegionRules:    1,
    CachedRegionAddrBase:  {DRAMBase},
    CachedRegionLength:    {DRAMLength},
    Axi64BitCompliant:      1'b1,
    SwapEndianess:          1'b0,
    DmBaseAddress:          DebugBase,
    NrPMPEntries:           8
  };
endpackage

   package axi_pkg;
  typedef logic [1:0] burst_t;
  typedef logic [1:0] resp_t;
  typedef logic [3:0] cache_t;
  typedef logic [2:0] prot_t;
  typedef logic [3:0] qos_t;
  typedef logic [3:0] region_t;
  typedef logic [7:0] len_t;
  typedef logic [2:0] size_t;
  typedef logic [5:0] atop_t;  
  typedef logic [3:0] nsaid_t;  
  localparam BURST_FIXED = 2'b00;
  localparam BURST_INCR  = 2'b01;
  localparam BURST_WRAP  = 2'b10;
  localparam RESP_OKAY   = 2'b00;
  localparam RESP_EXOKAY = 2'b01;
  localparam RESP_SLVERR = 2'b10;
  localparam RESP_DECERR = 2'b11;
  localparam CACHE_BUFFERABLE = 4'b0001;
  localparam CACHE_MODIFIABLE = 4'b0010;
  localparam CACHE_RD_ALLOC   = 4'b0100;
  localparam CACHE_WR_ALLOC   = 4'b1000;
  localparam ATOP_ATOMICSWAP  = 6'b110000;
  localparam ATOP_ATOMICCMP   = 6'b110001;
  localparam ATOP_NONE        = 2'b00;
  localparam ATOP_ATOMICSTORE = 2'b01;
  localparam ATOP_ATOMICLOAD  = 2'b10;
  localparam ATOP_LITTLE_END  = 1'b0;
  localparam ATOP_BIG_END     = 1'b1;
  localparam ATOP_ADD   = 3'b000;
  localparam ATOP_CLR   = 3'b001;
  localparam ATOP_EOR   = 3'b010;
  localparam ATOP_SET   = 3'b011;
  localparam ATOP_SMAX  = 3'b100;
  localparam ATOP_SMIN  = 3'b101;
  localparam ATOP_UMAX  = 3'b110;
  localparam ATOP_UMIN  = 3'b111;
  localparam IdWidth   = 4;
  localparam UserWidth = 1;
  localparam AddrWidth = 64;
  localparam DataWidth = 64;
  localparam StrbWidth = DataWidth / 8;
  typedef logic [IdWidth-1:0]   id_t;
  typedef logic [AddrWidth-1:0] addr_t;
  typedef logic [DataWidth-1:0] data_t;
  typedef logic [StrbWidth-1:0] strb_t;
  typedef logic [UserWidth-1:0] user_t;
  typedef struct packed {
      id_t     id;
      addr_t   addr;
      len_t    len;
      size_t   size;
      burst_t  burst;
      logic   lock;
      cache_t  cache;
      prot_t   prot;
      qos_t    qos;
      region_t region;
      atop_t   atop;
  } aw_chan_t;
  typedef struct packed {
      data_t data;
      strb_t strb;
      logic  last;
  } w_chan_t;
  typedef struct packed {
      id_t   id;
      resp_t resp;
  } b_chan_t;
  typedef struct packed {
      id_t     id;
      addr_t   addr;
      len_t    len;
      size_t   size;
      burst_t  burst;
      logic    lock;
      cache_t  cache;
      prot_t   prot;
      qos_t    qos;
      region_t region;
  } ar_chan_t;
  typedef struct packed {
      id_t   id;
      data_t data;
      resp_t resp;
      logic  last;
  } r_chan_t;
endpackage

   package ariane_axi;
    typedef enum logic { SINGLE_REQ, CACHE_LINE_REQ } ad_req_t;
    localparam IdWidth   = 4;  
    localparam UserWidth = 1;
    localparam AddrWidth = 64;
    localparam DataWidth = 64;
    localparam StrbWidth = DataWidth / 8;
    typedef logic   [IdWidth-1:0]   id_t;
    typedef logic [AddrWidth-1:0] addr_t;
    typedef logic [DataWidth-1:0] data_t;
    typedef logic [StrbWidth-1:0] strb_t;
    typedef logic [UserWidth-1:0] user_t;
    typedef struct packed {
        id_t              id;
        addr_t            addr;
        axi_pkg::len_t    len;
        axi_pkg::size_t   size;
        axi_pkg::burst_t  burst;
        logic             lock;
        axi_pkg::cache_t  cache;
        axi_pkg::prot_t   prot;
        axi_pkg::qos_t    qos;
        axi_pkg::region_t region;
        axi_pkg::atop_t   atop;
        user_t            user;
    } aw_chan_t;
    typedef struct packed {
        data_t data;
        strb_t strb;
        logic  last;
        user_t user;
    } w_chan_t;
    typedef struct packed {
        id_t            id;
        axi_pkg::resp_t resp;
        user_t          user;
    } b_chan_t;
    typedef struct packed {
        id_t             id;
        addr_t            addr;
        axi_pkg::len_t    len;
        axi_pkg::size_t   size;
        axi_pkg::burst_t  burst;
        logic             lock;
        axi_pkg::cache_t  cache;
        axi_pkg::prot_t   prot;
        axi_pkg::qos_t    qos;
        axi_pkg::region_t region;
        user_t            user;
    } ar_chan_t;
    typedef struct packed {
        id_t            id;
        data_t          data;
        axi_pkg::resp_t resp;
        logic           last;
        user_t          user;
    } r_chan_t;
    typedef struct packed {
        aw_chan_t aw;
        logic     aw_valid;
        w_chan_t  w;
        logic     w_valid;
        logic     b_ready;
        ar_chan_t ar;
        logic     ar_valid;
        logic     r_ready;
    } req_t;
    typedef struct packed {
        logic     aw_ready;
        logic     ar_ready;
        logic     w_ready;
        logic     b_valid;
        b_chan_t  b;
        logic     r_valid;
        r_chan_t  r;
    } resp_t;
endpackage
package ariane_axi_soc;
    typedef enum logic { SINGLE_REQ, CACHE_LINE_REQ } ad_req_t;
    localparam UserWidth = 1;
    localparam AddrWidth = 64;
    localparam DataWidth = 64;
    localparam StrbWidth = DataWidth / 8;
    typedef logic [ariane_soc::IdWidth-1:0]      id_t;
    typedef logic [ariane_soc::IdWidthSlave-1:0] id_slv_t;
    typedef logic [AddrWidth-1:0] addr_t;
    typedef logic [DataWidth-1:0] data_t;
    typedef logic [StrbWidth-1:0] strb_t;
    typedef logic [UserWidth-1:0] user_t;
    typedef struct packed {
        id_t              id;
        addr_t            addr;
        axi_pkg::len_t    len;
        axi_pkg::size_t   size;
        axi_pkg::burst_t  burst;
        logic             lock;
        axi_pkg::cache_t  cache;
        axi_pkg::prot_t   prot;
        axi_pkg::qos_t    qos;
        axi_pkg::region_t region;
        axi_pkg::atop_t   atop;
        user_t            user;
    } aw_chan_t;
    typedef struct packed {
        id_slv_t          id;
        addr_t            addr;
        axi_pkg::len_t    len;
        axi_pkg::size_t   size;
        axi_pkg::burst_t  burst;
        logic             lock;
        axi_pkg::cache_t  cache;
        axi_pkg::prot_t   prot;
        axi_pkg::qos_t    qos;
        axi_pkg::region_t region;
        axi_pkg::atop_t   atop;
        user_t            user;
    } aw_chan_slv_t;
    typedef struct packed {
        data_t data;
        strb_t strb;
        logic  last;
        user_t user;
    } w_chan_t;
    typedef struct packed {
        id_t            id;
        axi_pkg::resp_t resp;
        user_t          user;
    } b_chan_t;
    typedef struct packed {
        id_slv_t        id;
        axi_pkg::resp_t resp;
        user_t          user;
    } b_chan_slv_t;
    typedef struct packed {
        id_t             id;
        addr_t            addr;
        axi_pkg::len_t    len;
        axi_pkg::size_t   size;
        axi_pkg::burst_t  burst;
        logic             lock;
        axi_pkg::cache_t  cache;
        axi_pkg::prot_t   prot;
        axi_pkg::qos_t    qos;
        axi_pkg::region_t region;
        user_t            user;
    } ar_chan_t;
    typedef struct packed {
        id_slv_t          id;
        addr_t            addr;
        axi_pkg::len_t    len;
        axi_pkg::size_t   size;
        axi_pkg::burst_t  burst;
        logic             lock;
        axi_pkg::cache_t  cache;
        axi_pkg::prot_t   prot;
        axi_pkg::qos_t    qos;
        axi_pkg::region_t region;
        user_t            user;
    } ar_chan_slv_t;
    typedef struct packed {
        id_t            id;
        data_t          data;
        axi_pkg::resp_t resp;
        logic           last;
        user_t          user;
    } r_chan_t;
    typedef struct packed {
        id_slv_t        id;
        data_t          data;
        axi_pkg::resp_t resp;
        logic           last;
        user_t          user;
    } r_chan_slv_t;
    typedef struct packed {
        aw_chan_t aw;
        logic     aw_valid;
        w_chan_t  w;
        logic     w_valid;
        logic     b_ready;
        ar_chan_t ar;
        logic     ar_valid;
        logic     r_ready;
    } req_t;
    typedef struct packed {
        logic     aw_ready;
        logic     ar_ready;
        logic     w_ready;
        logic     b_valid;
        b_chan_t  b;
        logic     r_valid;
        r_chan_t  r;
    } resp_t;
    typedef struct packed {
        aw_chan_slv_t aw;
        logic         aw_valid;
        w_chan_t      w;
        logic         w_valid;
        logic         b_ready;
        ar_chan_slv_t ar;
        logic         ar_valid;
        logic         r_ready;
    } req_slv_t;
    typedef struct packed {
        logic         aw_ready;
        logic         ar_ready;
        logic         w_ready;
        logic         b_valid;
        b_chan_slv_t  b;
        logic         r_valid;
        r_chan_slv_t  r;
    } resp_slv_t;
endpackage

module ariane_testharness #(
  parameter int unsigned AXI_USER_WIDTH    = 1,
  parameter int unsigned AXI_ADDRESS_WIDTH = 64,
  parameter int unsigned AXI_DATA_WIDTH    = 64,
  parameter bit          InclSimDTM        = 1'b1,
  parameter int unsigned NUM_WORDS         = 2**25,          
  parameter bit          StallRandomOutput = 1'b0,
  parameter bit          StallRandomInput  = 1'b0
) (
  input logic         clk_i,
  input logic         rtc_i,
  input logic         rst_ni,
  input logic [1:0]   irqs,
  input logic         ipi,
  input logic         timer_irq,
  input logic         debug_req_core,
  output logic [31:0] exit_o
);
   ariane_axi_soc::req_t    axi_ariane_req;
   ariane_axi_soc::resp_t   axi_ariane_resp;
   
  ariane_dummy #(
    .ArianeCfg  ( ariane_soc::ArianeSocCfg )
  ) i_ariane (
    .clk_i                ( clk_i               ),
    .rst_ni               ( rst_ni              ),
    .boot_addr_i          ( ariane_soc::ROMBase ),  
    .hart_id_i            ( '0                  ),
    .irq_i                ( irqs                ),
    .ipi_i                ( ipi                 ),
    .time_irq_i           ( timer_irq           ),
    .debug_req_i          ( debug_req_core      ),
    .axi_req_o            ( axi_ariane_req      ),
    .axi_resp_i           ( axi_ariane_resp     )
  );
endmodule // ariane_testharness

module ariane_dummy import ariane_pkg::*; #(
  parameter ariane_pkg::ariane_cfg_t ArianeCfg     = ariane_pkg::ArianeDefaultConfig
) (
  input  logic                         clk_i,
  input  logic                         rst_ni,
  input  logic [riscv::VLEN-1:0]       boot_addr_i,   
  input  logic [riscv::XLEN-1:0]       hart_id_i,     
  input  logic [1:0]                   irq_i,         
  input  logic                         ipi_i,         
  input  logic                         time_irq_i,    
  input  logic                         debug_req_i,   
  output ariane_axi::req_t             axi_req_o,
  input  ariane_axi::resp_t            axi_resp_i
);
  riscv::priv_lvl_t           priv_lvl;
  exception_t                 ex_commit;  
  bp_resolve_t                resolved_branch;
  logic [riscv::VLEN-1:0]     pc_commit;
  logic                       eret;
  logic [NR_COMMIT_PORTS-1:0] commit_ack;
  logic [riscv::VLEN-1:0]     trap_vector_base_commit_pcgen;
  logic [riscv::VLEN-1:0]     epc_commit_pcgen;
  fetch_entry_t             fetch_entry_if_id;
  logic                     fetch_valid_if_id;
  logic                     fetch_ready_id_if;
  scoreboard_entry_t        issue_entry_id_issue;
  logic                     issue_entry_valid_id_issue;
  logic                     is_ctrl_fow_id_issue;
  logic                     issue_instr_issue_id;
   logic [riscv::VLEN-1:0] rs1_forwarding_id_ex;  
   logic [riscv::VLEN-1:0] rs2_forwarding_id_ex;  
  fu_data_t                 fu_data_id_ex;
  logic [riscv::VLEN-1:0]   pc_id_ex;
  logic                     is_compressed_instr_id_ex;
  logic                     flu_ready_ex_id;
  logic [TRANS_ID_BITS-1:0] flu_trans_id_ex_id;
  logic                     flu_valid_ex_id;
  riscv::xlen_t             flu_result_ex_id;
  exception_t               flu_exception_ex_id;
  logic                     alu_valid_id_ex;
  logic                     branch_valid_id_ex;
  branchpredict_sbe_t       branch_predict_id_ex;
  logic                     resolve_branch_ex_id;
  logic                     lsu_valid_id_ex;
  logic                     lsu_ready_ex_id;
  logic [TRANS_ID_BITS-1:0] load_trans_id_ex_id;
  riscv::xlen_t             load_result_ex_id;
  logic                     load_valid_ex_id;
  exception_t               load_exception_ex_id;
  riscv::xlen_t             store_result_ex_id;
  logic [TRANS_ID_BITS-1:0] store_trans_id_ex_id;
  logic                     store_valid_ex_id;
  exception_t               store_exception_ex_id;
  logic                     mult_valid_id_ex;
  logic                     fpu_ready_ex_id;
  logic                     fpu_valid_id_ex;
  logic [1:0]               fpu_fmt_id_ex;
  logic [2:0]               fpu_rm_id_ex;
  logic [TRANS_ID_BITS-1:0] fpu_trans_id_ex_id;
  riscv::xlen_t             fpu_result_ex_id;
  logic                     fpu_valid_ex_id;
  exception_t               fpu_exception_ex_id;
  logic                     csr_valid_id_ex;
  logic                     csr_commit_commit_ex;
  logic                     dirty_fp_state;
  logic                     lsu_commit_commit_ex;
  logic                     lsu_commit_ready_ex_commit;
  logic [TRANS_ID_BITS-1:0] lsu_commit_trans_id;
  logic                     no_st_pending_ex;
  logic                     no_st_pending_commit;
  logic                     amo_valid_commit;
  scoreboard_entry_t [NR_COMMIT_PORTS-1:0] commit_instr_id_commit;
  logic [NR_COMMIT_PORTS-1:0][4:0]  waddr_commit_id;
  logic [NR_COMMIT_PORTS-1:0][riscv::XLEN-1:0] wdata_commit_id;
  logic [NR_COMMIT_PORTS-1:0]       we_gpr_commit_id;
  logic [NR_COMMIT_PORTS-1:0]       we_fpr_commit_id;
  logic [4:0]               fflags_csr_commit;
  riscv::xs_t               fs;
  logic [2:0]               frm_csr_id_issue_ex;
  logic [6:0]               fprec_csr_ex;
  logic                     enable_translation_csr_ex;
  logic                     en_ld_st_translation_csr_ex;
  riscv::priv_lvl_t         ld_st_priv_lvl_csr_ex;
  logic                     sum_csr_ex;
  logic                     mxr_csr_ex;
  logic [riscv::PPNW-1:0]   satp_ppn_csr_ex;
  logic [ASID_WIDTH-1:0]    asid_csr_ex;
  logic [11:0]              csr_addr_ex_csr;
  fu_op                     csr_op_commit_csr;
  riscv::xlen_t             csr_wdata_commit_csr;
  riscv::xlen_t             csr_rdata_csr_commit;
  exception_t               csr_exception_csr_commit;
  logic                     tvm_csr_id;
  logic                     tw_csr_id;
  logic                     tsr_csr_id;
  irq_ctrl_t                irq_ctrl_csr_id;
  logic                     dcache_en_csr_nbdcache;
  logic                     csr_write_fflags_commit_cs;
  logic                     icache_en_csr;
  logic                     debug_mode;
  logic                     single_step_csr_commit;
  riscv::pmpcfg_t [15:0]    pmpcfg;
  logic [15:0][riscv::PLEN-3:0] pmpaddr;
  logic [4:0]               addr_csr_perf;
  riscv::xlen_t             data_csr_perf, data_perf_csr;
  logic                     we_csr_perf;
  logic                     icache_flush_ctrl_cache;
  logic                     itlb_miss_ex_perf;
  logic                     dtlb_miss_ex_perf;
  logic                     dcache_miss_cache_perf;
  logic                     icache_miss_cache_perf;
  logic                     set_pc_ctrl_pcgen;
  logic                     flush_csr_ctrl;
  logic                     flush_unissued_instr_ctrl_id;
  logic                     flush_ctrl_if;
  logic                     flush_ctrl_id;
  logic                     flush_ctrl_ex;
  logic                     flush_ctrl_bp;
  logic                     flush_tlb_ctrl_ex;
  logic                     fence_i_commit_controller;
  logic                     fence_commit_controller;
  logic                     sfence_vma_commit_controller;
  logic                     halt_ctrl;
  logic                     halt_csr_ctrl;
  logic                     dcache_flush_ctrl_cache;
  logic                     dcache_flush_ack_cache_ctrl;
  logic                     set_debug_pc;
  logic                     flush_commit;
  icache_areq_i_t           icache_areq_ex_cache;
  icache_areq_o_t           icache_areq_cache_ex;
  icache_dreq_i_t           icache_dreq_if_cache;
  icache_dreq_o_t           icache_dreq_cache_if;
  amo_req_t                 amo_req;
  amo_resp_t                amo_resp;
  logic                     sb_full;
  dcache_req_i_t [2:0]      dcache_req_ports_ex_cache;
  dcache_req_o_t [2:0]      dcache_req_ports_cache_ex;
  logic                     dcache_commit_wbuffer_empty;
  logic                     dcache_commit_wbuffer_not_ni;
  frontend #(
    .ArianeCfg ( ArianeCfg )
  ) i_frontend (
    .flush_i             ( flush_ctrl_if                 ),  
    .flush_bp_i          ( 1'b0                          ),
    .debug_mode_i        ( debug_mode                    ),
    .boot_addr_i         ( boot_addr_i[riscv::VLEN-1:0]  ),
    .icache_dreq_i       ( icache_dreq_cache_if          ),
    .icache_dreq_o       ( icache_dreq_if_cache          ),
    .resolved_branch_i   ( resolved_branch               ),
    .pc_commit_i         ( pc_commit                     ),
    .set_pc_commit_i     ( set_pc_ctrl_pcgen             ),
    .set_debug_pc_i      ( set_debug_pc                  ),
    .epc_i               ( epc_commit_pcgen              ),
    .eret_i              ( eret                          ),
    .trap_vector_base_i  ( trap_vector_base_commit_pcgen ),
    .ex_valid_i          ( ex_commit.valid               ),
    .fetch_entry_o       ( fetch_entry_if_id             ),
    .fetch_entry_valid_o ( fetch_valid_if_id             ),
    .fetch_entry_ready_i ( fetch_ready_id_if             ),
    .*
  );
   /*
  id_stage id_stage_i (
    .clk_i,
    .rst_ni,
    .flush_i                    ( flush_ctrl_if              ),
    .debug_req_i,
    .fetch_entry_i              ( fetch_entry_if_id          ),
    .fetch_entry_valid_i        ( fetch_valid_if_id          ),
    .fetch_entry_ready_o        ( fetch_ready_id_if          ),
    .issue_entry_o              ( issue_entry_id_issue       ),
    .issue_entry_valid_o        ( issue_entry_valid_id_issue ),
    .is_ctrl_flow_o             ( is_ctrl_fow_id_issue       ),
    .issue_instr_ack_i          ( issue_instr_issue_id       ),
    .priv_lvl_i                 ( priv_lvl                   ),
    .fs_i                       ( fs                         ),
    .frm_i                      ( frm_csr_id_issue_ex        ),
    .irq_i                      ( irq_i                      ),
    .irq_ctrl_i                 ( irq_ctrl_csr_id            ),
    .debug_mode_i               ( debug_mode                 ),
    .tvm_i                      ( tvm_csr_id                 ),
    .tw_i                       ( tw_csr_id                  ),
    .tsr_i                      ( tsr_csr_id                 )
  );
  issue_stage #(
    .NR_ENTRIES                 ( NR_SB_ENTRIES                ),
    .NR_WB_PORTS                ( NR_WB_PORTS                  ),
    .NR_COMMIT_PORTS            ( NR_COMMIT_PORTS              )
  ) issue_stage_i (
    .clk_i,
    .rst_ni,
    .sb_full_o                  ( sb_full                      ),
    .flush_unissued_instr_i     ( flush_unissued_instr_ctrl_id ),
    .flush_i                    ( flush_ctrl_id                ),
    .decoded_instr_i            ( issue_entry_id_issue         ),
    .decoded_instr_valid_i      ( issue_entry_valid_id_issue   ),
    .is_ctrl_flow_i             ( is_ctrl_fow_id_issue         ),
    .decoded_instr_ack_o        ( issue_instr_issue_id         ),
    .rs1_forwarding_o           ( rs1_forwarding_id_ex         ),
    .rs2_forwarding_o           ( rs2_forwarding_id_ex         ),
    .fu_data_o                  ( fu_data_id_ex                ),
    .pc_o                       ( pc_id_ex                     ),
    .is_compressed_instr_o      ( is_compressed_instr_id_ex    ),
    .flu_ready_i                ( flu_ready_ex_id              ),
    .alu_valid_o                ( alu_valid_id_ex              ),
    .branch_valid_o             ( branch_valid_id_ex           ),  
    .branch_predict_o           ( branch_predict_id_ex         ),  
    .resolve_branch_i           ( resolve_branch_ex_id         ),  
    .lsu_ready_i                ( lsu_ready_ex_id              ),
    .lsu_valid_o                ( lsu_valid_id_ex              ),
    .mult_valid_o               ( mult_valid_id_ex             ),
    .fpu_ready_i                ( fpu_ready_ex_id              ),
    .fpu_valid_o                ( fpu_valid_id_ex              ),
    .fpu_fmt_o                  ( fpu_fmt_id_ex                ),
    .fpu_rm_o                   ( fpu_rm_id_ex                 ),
    .csr_valid_o                ( csr_valid_id_ex              ),
    .resolved_branch_i          ( resolved_branch              ),
    .trans_id_i                 ( {flu_trans_id_ex_id,  load_trans_id_ex_id,  store_trans_id_ex_id,   fpu_trans_id_ex_id }),
    .wbdata_i                   ( {flu_result_ex_id,    load_result_ex_id,    store_result_ex_id,       fpu_result_ex_id }),
    .ex_ex_i                    ( {flu_exception_ex_id, load_exception_ex_id, store_exception_ex_id, fpu_exception_ex_id }),
    .wt_valid_i                 ( {flu_valid_ex_id,     load_valid_ex_id,     store_valid_ex_id,         fpu_valid_ex_id }),
    .waddr_i                    ( waddr_commit_id              ),
    .wdata_i                    ( wdata_commit_id              ),
    .we_gpr_i                   ( we_gpr_commit_id             ),
    .we_fpr_i                   ( we_fpr_commit_id             ),
    .commit_instr_o             ( commit_instr_id_commit       ),
    .commit_ack_i               ( commit_ack                   ),
    .*
  );
  ex_stage #(
    .ASID_WIDTH ( ASID_WIDTH ),
    .ArianeCfg  ( ArianeCfg  )
  ) ex_stage_i (
    .clk_i                  ( clk_i                       ),
    .rst_ni                 ( rst_ni                      ),
    .debug_mode_i           ( debug_mode                  ),
    .flush_i                ( flush_ctrl_ex               ),
    .rs1_forwarding_i       ( rs1_forwarding_id_ex        ),
    .rs2_forwarding_i       ( rs2_forwarding_id_ex        ),
    .fu_data_i              ( fu_data_id_ex               ),
    .pc_i                   ( pc_id_ex                    ),
    .is_compressed_instr_i  ( is_compressed_instr_id_ex   ),
    .flu_result_o           ( flu_result_ex_id            ),
    .flu_trans_id_o         ( flu_trans_id_ex_id          ),
    .flu_valid_o            ( flu_valid_ex_id             ),
    .flu_exception_o        ( flu_exception_ex_id         ),
    .flu_ready_o            ( flu_ready_ex_id             ),
    .alu_valid_i            ( alu_valid_id_ex             ),
    .branch_valid_i         ( branch_valid_id_ex          ),
    .branch_predict_i       ( branch_predict_id_ex        ),  
    .resolved_branch_o      ( resolved_branch             ),
    .resolve_branch_o       ( resolve_branch_ex_id        ),
    .csr_valid_i            ( csr_valid_id_ex             ),
    .csr_addr_o             ( csr_addr_ex_csr             ),
    .csr_commit_i           ( csr_commit_commit_ex        ),  
    .mult_valid_i           ( mult_valid_id_ex            ),
    .lsu_ready_o            ( lsu_ready_ex_id             ),
    .lsu_valid_i            ( lsu_valid_id_ex             ),
    .load_result_o          ( load_result_ex_id           ),
    .load_trans_id_o        ( load_trans_id_ex_id         ),
    .load_valid_o           ( load_valid_ex_id            ),
    .load_exception_o       ( load_exception_ex_id        ),
    .store_result_o         ( store_result_ex_id          ),
    .store_trans_id_o       ( store_trans_id_ex_id        ),
    .store_valid_o          ( store_valid_ex_id           ),
    .store_exception_o      ( store_exception_ex_id       ),
    .lsu_commit_i           ( lsu_commit_commit_ex        ),  
    .lsu_commit_ready_o     ( lsu_commit_ready_ex_commit  ),  
    .commit_tran_id_i       ( lsu_commit_trans_id         ),  
    .no_st_pending_o        ( no_st_pending_ex            ),
    .fpu_ready_o            ( fpu_ready_ex_id             ),
    .fpu_valid_i            ( fpu_valid_id_ex             ),
    .fpu_fmt_i              ( fpu_fmt_id_ex               ),
    .fpu_rm_i               ( fpu_rm_id_ex                ),
    .fpu_frm_i              ( frm_csr_id_issue_ex         ),
    .fpu_prec_i             ( fprec_csr_ex                ),
    .fpu_trans_id_o         ( fpu_trans_id_ex_id          ),
    .fpu_result_o           ( fpu_result_ex_id            ),
    .fpu_valid_o            ( fpu_valid_ex_id             ),
    .fpu_exception_o        ( fpu_exception_ex_id         ),
    .amo_valid_commit_i     ( amo_valid_commit            ),
    .amo_req_o              ( amo_req                     ),
    .amo_resp_i             ( amo_resp                    ),
    .itlb_miss_o            ( itlb_miss_ex_perf           ),
    .dtlb_miss_o            ( dtlb_miss_ex_perf           ),
    .enable_translation_i   ( enable_translation_csr_ex   ),  
    .en_ld_st_translation_i ( en_ld_st_translation_csr_ex ),
    .flush_tlb_i            ( flush_tlb_ctrl_ex           ),
    .priv_lvl_i             ( priv_lvl                    ),  
    .ld_st_priv_lvl_i       ( ld_st_priv_lvl_csr_ex       ),  
    .sum_i                  ( sum_csr_ex                  ),  
    .mxr_i                  ( mxr_csr_ex                  ),  
    .satp_ppn_i             ( satp_ppn_csr_ex             ),  
    .asid_i                 ( asid_csr_ex                 ),  
    .icache_areq_i          ( icache_areq_cache_ex        ),
    .icache_areq_o          ( icache_areq_ex_cache        ),
    .dcache_req_ports_i     ( dcache_req_ports_cache_ex   ),
    .dcache_req_ports_o     ( dcache_req_ports_ex_cache   ),
    .dcache_wbuffer_empty_i ( dcache_commit_wbuffer_empty ),
    .dcache_wbuffer_not_ni_i ( dcache_commit_wbuffer_not_ni ),
    .pmpcfg_i               ( pmpcfg                      ),
    .pmpaddr_i              ( pmpaddr                     )
  );
  assign no_st_pending_commit = no_st_pending_ex & dcache_commit_wbuffer_empty;
  commit_stage #(
    .NR_COMMIT_PORTS ( NR_COMMIT_PORTS )
  ) commit_stage_i (
    .clk_i,
    .rst_ni,
    .halt_i                 ( halt_ctrl                     ),
    .flush_dcache_i         ( dcache_flush_ctrl_cache       ),
    .exception_o            ( ex_commit                     ),
    .dirty_fp_state_o       ( dirty_fp_state                ),
    .single_step_i          ( single_step_csr_commit        ),
    .commit_instr_i         ( commit_instr_id_commit        ),
    .commit_ack_o           ( commit_ack                    ),
    .no_st_pending_i        ( no_st_pending_commit          ),
    .waddr_o                ( waddr_commit_id               ),
    .wdata_o                ( wdata_commit_id               ),
    .we_gpr_o               ( we_gpr_commit_id              ),
    .we_fpr_o               ( we_fpr_commit_id              ),
    .commit_lsu_o           ( lsu_commit_commit_ex          ),
    .commit_lsu_ready_i     ( lsu_commit_ready_ex_commit    ),
    .commit_tran_id_o       ( lsu_commit_trans_id           ),
    .amo_valid_commit_o     ( amo_valid_commit              ),
    .amo_resp_i             ( amo_resp                      ),
    .commit_csr_o           ( csr_commit_commit_ex          ),
    .pc_o                   ( pc_commit                     ),
    .csr_op_o               ( csr_op_commit_csr             ),
    .csr_wdata_o            ( csr_wdata_commit_csr          ),
    .csr_rdata_i            ( csr_rdata_csr_commit          ),
    .csr_write_fflags_o     ( csr_write_fflags_commit_cs    ),
    .csr_exception_i        ( csr_exception_csr_commit      ),
    .fence_i_o              ( fence_i_commit_controller     ),
    .fence_o                ( fence_commit_controller       ),
    .sfence_vma_o           ( sfence_vma_commit_controller  ),
    .flush_commit_o         ( flush_commit                  ),
    .*
  );
  csr_regfile #(
    .AsidWidth              ( ASID_WIDTH                    ),
    .DmBaseAddress          ( ArianeCfg.DmBaseAddress       ),
    .NrCommitPorts          ( NR_COMMIT_PORTS               ),
    .NrPMPEntries           ( ArianeCfg.NrPMPEntries        )
  ) csr_regfile_i (
    .flush_o                ( flush_csr_ctrl                ),
    .halt_csr_o             ( halt_csr_ctrl                 ),
    .commit_instr_i         ( commit_instr_id_commit        ),
    .commit_ack_i           ( commit_ack                    ),
    .boot_addr_i            ( boot_addr_i[riscv::VLEN-1:0]  ),
    .hart_id_i              ( hart_id_i[riscv::XLEN-1:0]    ),
    .ex_i                   ( ex_commit                     ),
    .csr_op_i               ( csr_op_commit_csr             ),
    .csr_write_fflags_i     ( csr_write_fflags_commit_cs    ),
    .dirty_fp_state_i       ( dirty_fp_state                ),
    .csr_addr_i             ( csr_addr_ex_csr               ),
    .csr_wdata_i            ( csr_wdata_commit_csr          ),
    .csr_rdata_o            ( csr_rdata_csr_commit          ),
    .pc_i                   ( pc_commit                     ),
    .csr_exception_o        ( csr_exception_csr_commit      ),
    .epc_o                  ( epc_commit_pcgen              ),
    .eret_o                 ( eret                          ),
    .set_debug_pc_o         ( set_debug_pc                  ),
    .trap_vector_base_o     ( trap_vector_base_commit_pcgen ),
    .priv_lvl_o             ( priv_lvl                      ),
    .fs_o                   ( fs                            ),
    .fflags_o               ( fflags_csr_commit             ),
    .frm_o                  ( frm_csr_id_issue_ex           ),
    .fprec_o                ( fprec_csr_ex                  ),
    .irq_ctrl_o             ( irq_ctrl_csr_id               ),
    .ld_st_priv_lvl_o       ( ld_st_priv_lvl_csr_ex         ),
    .en_translation_o       ( enable_translation_csr_ex     ),
    .en_ld_st_translation_o ( en_ld_st_translation_csr_ex   ),
    .sum_o                  ( sum_csr_ex                    ),
    .mxr_o                  ( mxr_csr_ex                    ),
    .satp_ppn_o             ( satp_ppn_csr_ex               ),
    .asid_o                 ( asid_csr_ex                   ),
    .tvm_o                  ( tvm_csr_id                    ),
    .tw_o                   ( tw_csr_id                     ),
    .tsr_o                  ( tsr_csr_id                    ),
    .debug_mode_o           ( debug_mode                    ),
    .single_step_o          ( single_step_csr_commit        ),
    .dcache_en_o            ( dcache_en_csr_nbdcache        ),
    .icache_en_o            ( icache_en_csr                 ),
    .perf_addr_o            ( addr_csr_perf                 ),
    .perf_data_o            ( data_csr_perf                 ),
    .perf_data_i            ( data_perf_csr                 ),
    .perf_we_o              ( we_csr_perf                   ),
    .pmpcfg_o               ( pmpcfg                        ),
    .pmpaddr_o              ( pmpaddr                       ),
    .debug_req_i,
    .ipi_i,
    .irq_i,
    .time_irq_i,
    .*
  );
  perf_counters i_perf_counters (
    .clk_i             ( clk_i                  ),
    .rst_ni            ( rst_ni                 ),
    .debug_mode_i      ( debug_mode             ),
    .addr_i            ( addr_csr_perf          ),
    .we_i              ( we_csr_perf            ),
    .data_i            ( data_csr_perf          ),
    .data_o            ( data_perf_csr          ),
    .commit_instr_i    ( commit_instr_id_commit ),
    .commit_ack_i      ( commit_ack             ),
    .l1_icache_miss_i  ( icache_miss_cache_perf ),
    .l1_dcache_miss_i  ( dcache_miss_cache_perf ),
    .itlb_miss_i       ( itlb_miss_ex_perf      ),
    .dtlb_miss_i       ( dtlb_miss_ex_perf      ),
    .sb_full_i         ( sb_full                ),
    .if_empty_i        ( ~fetch_valid_if_id     ),
    .ex_i              ( ex_commit              ),
    .eret_i            ( eret                   ),
    .resolved_branch_i ( resolved_branch        )
  );
  controller controller_i (
    .set_pc_commit_o        ( set_pc_ctrl_pcgen             ),
    .flush_unissued_instr_o ( flush_unissued_instr_ctrl_id  ),
    .flush_if_o             ( flush_ctrl_if                 ),
    .flush_id_o             ( flush_ctrl_id                 ),
    .flush_ex_o             ( flush_ctrl_ex                 ),
    .flush_bp_o             ( flush_ctrl_bp                 ),
    .flush_tlb_o            ( flush_tlb_ctrl_ex             ),
    .flush_dcache_o         ( dcache_flush_ctrl_cache       ),
    .flush_dcache_ack_i     ( dcache_flush_ack_cache_ctrl   ),
    .halt_csr_i             ( halt_csr_ctrl                 ),
    .halt_o                 ( halt_ctrl                     ),
    .eret_i                 ( eret                          ),
    .ex_valid_i             ( ex_commit.valid               ),
    .set_debug_pc_i         ( set_debug_pc                  ),
    .flush_csr_i            ( flush_csr_ctrl                ),
    .resolved_branch_i      ( resolved_branch               ),
    .fence_i_i              ( fence_i_commit_controller     ),
    .fence_i                ( fence_commit_controller       ),
    .sfence_vma_i           ( sfence_vma_commit_controller  ),
    .flush_commit_i         ( flush_commit                  ),
    .flush_icache_o         ( icache_flush_ctrl_cache       ),
    .*
  );
  wt_cache_subsystem #(
    .ArianeCfg            ( ArianeCfg     )
  ) i_cache_subsystem (
    .clk_i                 ( clk_i                       ),
    .rst_ni                ( rst_ni                      ),
    .icache_en_i           ( icache_en_csr               ),
    .icache_flush_i        ( icache_flush_ctrl_cache     ),
    .icache_miss_o         ( icache_miss_cache_perf      ),
    .icache_areq_i         ( icache_areq_ex_cache        ),
    .icache_areq_o         ( icache_areq_cache_ex        ),
    .icache_dreq_i         ( icache_dreq_if_cache        ),
    .icache_dreq_o         ( icache_dreq_cache_if        ),
    .dcache_enable_i       ( dcache_en_csr_nbdcache      ),
    .dcache_flush_i        ( dcache_flush_ctrl_cache     ),
    .dcache_flush_ack_o    ( dcache_flush_ack_cache_ctrl ),
    .dcache_amo_req_i      ( amo_req                     ),
    .dcache_amo_resp_o     ( amo_resp                    ),
    .dcache_miss_o         ( dcache_miss_cache_perf      ),
    .dcache_req_ports_i    ( dcache_req_ports_ex_cache   ),
    .dcache_req_ports_o    ( dcache_req_ports_cache_ex   ),
    .wbuffer_empty_o       ( dcache_commit_wbuffer_empty ),
    .wbuffer_not_ni_o      ( dcache_commit_wbuffer_not_ni ),
    .axi_req_o             ( axi_req_o                   ),
    .axi_resp_i            ( axi_resp_i                  )
  );
    */
  int f;
  logic [63:0] cycles;
  initial begin
    f = $fopen("trace_hart_00.dasm", "w");
  end
  always_ff @(posedge clk_i or negedge rst_ni) begin
    if (~rst_ni) begin
      cycles <= 0;
    end else begin
      static byte mode = "";
      if (debug_mode) mode = "D";
      else begin
        case (priv_lvl)
        riscv::PRIV_LVL_M: mode = "M";
        riscv::PRIV_LVL_S: mode = "S";
        riscv::PRIV_LVL_U: mode = "U";
        endcase
      end
      for (int i = 0; i < NR_COMMIT_PORTS; i++) begin
        if (commit_ack[i] && !commit_instr_id_commit[i].ex.valid) begin
          $fwrite(f, "%d 0x%0h %s (0x%h) DASM(%h)\n", cycles, commit_instr_id_commit[i].pc, mode, commit_instr_id_commit[i].ex.tval[31:0], commit_instr_id_commit[i].ex.tval[31:0]);
        end else if (commit_ack[i] && commit_instr_id_commit[i].ex.valid) begin
          if (commit_instr_id_commit[i].ex.cause == 2) begin
            $fwrite(f, "Exception Cause: Illegal Instructions, DASM(%h) PC=%h\n", commit_instr_id_commit[i].ex.tval[31:0], commit_instr_id_commit[i].pc);
          end else begin
            if (debug_mode) begin
              $fwrite(f, "%d 0x%0h %s (0x%h) DASM(%h)\n", cycles, commit_instr_id_commit[i].pc, mode, commit_instr_id_commit[i].ex.tval[31:0], commit_instr_id_commit[i].ex.tval[31:0]);
            end else begin
              $fwrite(f, "Exception Cause: %5d, DASM(%h) PC=%h\n", commit_instr_id_commit[i].ex.cause, commit_instr_id_commit[i].ex.tval[31:0], commit_instr_id_commit[i].pc);
            end
          end
        end
      end
        cycles <= cycles + 1;
    end
  end
  final begin
    $fclose(f);
  end
endmodule  

module frontend import ariane_pkg::*; #(
  parameter ariane_pkg::ariane_cfg_t ArianeCfg = ariane_pkg::ArianeDefaultConfig
) (
  input  logic               clk_i,               
  input  logic               rst_ni,              
  input  logic               flush_i,             
  input  logic               flush_bp_i,          
  input  logic               debug_mode_i,
  input  logic [riscv::VLEN-1:0]        boot_addr_i,
  input  bp_resolve_t        resolved_branch_i,   
  input  logic               set_pc_commit_i,     
  input  logic [riscv::VLEN-1:0] pc_commit_i,         
  input  logic [riscv::VLEN-1:0] epc_i,               
  input  logic               eret_i,              
  input  logic [riscv::VLEN-1:0] trap_vector_base_i,  
  input  logic               ex_valid_i,          
  input  logic               set_debug_pc_i,      
  output icache_dreq_i_t     icache_dreq_o,
  input  icache_dreq_o_t     icache_dreq_i,
  output fetch_entry_t       fetch_entry_o,        
  output logic               fetch_entry_valid_o,  
  input  logic               fetch_entry_ready_i   
);
    logic [FETCH_WIDTH-1:0] icache_data_q;
    logic                   icache_valid_q;
    ariane_pkg::frontend_exception_t icache_ex_valid_q;
    logic [riscv::VLEN-1:0] icache_vaddr_q;
    logic                   instr_queue_ready;
    logic [ariane_pkg::INSTR_PER_FETCH-1:0] instr_queue_consumed;
    btb_prediction_t        btb_q;
    bht_prediction_t        bht_q;
    logic                   if_ready;
    logic [riscv::VLEN-1:0] npc_d, npc_q;  
    logic                   npc_rst_load_q;
    logic                   replay;
    logic [riscv::VLEN-1:0] replay_addr;
    logic [$clog2(ariane_pkg::INSTR_PER_FETCH)-1:0] shamt;
    assign shamt = icache_dreq_i.vaddr[$clog2(ariane_pkg::INSTR_PER_FETCH):1];
    logic [INSTR_PER_FETCH-1:0]       rvi_return, rvi_call, rvi_branch,
                                      rvi_jalr, rvi_jump;
    logic [INSTR_PER_FETCH-1:0][riscv::VLEN-1:0] rvi_imm;
    logic [INSTR_PER_FETCH-1:0]       rvc_branch, rvc_jump, rvc_jr, rvc_return,
                                      rvc_jalr, rvc_call;
    logic [INSTR_PER_FETCH-1:0][riscv::VLEN-1:0] rvc_imm;
    logic [INSTR_PER_FETCH-1:0][31:0] instr;
    logic [INSTR_PER_FETCH-1:0][riscv::VLEN-1:0] addr;
    logic [INSTR_PER_FETCH-1:0]       instruction_valid;
    bht_prediction_t [INSTR_PER_FETCH-1:0] bht_prediction;
    btb_prediction_t [INSTR_PER_FETCH-1:0] btb_prediction;
    bht_prediction_t [INSTR_PER_FETCH-1:0] bht_prediction_shifted;
    btb_prediction_t [INSTR_PER_FETCH-1:0] btb_prediction_shifted;
    ras_t            ras_predict;
    logic            is_mispredict;
    logic            ras_push, ras_pop;
    logic [riscv::VLEN-1:0]     ras_update;
    logic [riscv::VLEN-1:0]                 predict_address;
    cf_t  [ariane_pkg::INSTR_PER_FETCH-1:0] cf_type;
    logic [ariane_pkg::INSTR_PER_FETCH-1:0] taken_rvi_cf;
    logic [ariane_pkg::INSTR_PER_FETCH-1:0] taken_rvc_cf;
    logic serving_unaligned;
    instr_realign i_instr_realign (
      .clk_i               ( clk_i                 ),
      .rst_ni              ( rst_ni                ),
      .flush_i             ( icache_dreq_o.kill_s2 ),
      .valid_i             ( icache_valid_q        ),
      .serving_unaligned_o ( serving_unaligned     ),
      .address_i           ( icache_vaddr_q        ),
      .data_i              ( icache_data_q         ),
      .valid_o             ( instruction_valid     ),
      .addr_o              ( addr                  ),
      .instr_o             ( instr                 )
    );
    assign bht_prediction_shifted[0] = (serving_unaligned) ? bht_q : bht_prediction[0];
    assign btb_prediction_shifted[0] = (serving_unaligned) ? btb_q : btb_prediction[0];
    for (genvar i = 1; i < INSTR_PER_FETCH; i++) begin : gen_prediction_address
      assign bht_prediction_shifted[i] = bht_prediction[addr[i][$clog2(INSTR_PER_FETCH):1]];
      assign btb_prediction_shifted[i] = btb_prediction[addr[i][$clog2(INSTR_PER_FETCH):1]];
    end
    logic bp_valid;
    logic [INSTR_PER_FETCH-1:0] is_branch;
    logic [INSTR_PER_FETCH-1:0] is_call;
    logic [INSTR_PER_FETCH-1:0] is_jump;
    logic [INSTR_PER_FETCH-1:0] is_return;
    logic [INSTR_PER_FETCH-1:0] is_jalr;
    for (genvar i = 0; i < INSTR_PER_FETCH; i++) begin
      assign is_branch[i] =  instruction_valid[i] & (rvi_branch[i] | rvc_branch[i]);
      assign is_call[i] = instruction_valid[i] & (rvi_call[i] | rvc_call[i]);
      assign is_return[i] = instruction_valid[i] & (rvi_return[i] | rvc_return[i]);
      assign is_jump[i] = instruction_valid[i] & (rvi_jump[i] | rvc_jump[i]);
      assign is_jalr[i] = instruction_valid[i] & ~is_return[i] & ~is_call[i] & (rvi_jalr[i] | rvc_jalr[i] | rvc_jr[i]);
    end
    always_comb begin
      taken_rvi_cf = '0;
      taken_rvc_cf = '0;
      predict_address = '0;
      for (int i = 0; i < INSTR_PER_FETCH; i++)  cf_type[i] = ariane_pkg::NoCF;
      ras_push = 1'b0;
      ras_pop = 1'b0;
      ras_update = '0;
      for (int i = INSTR_PER_FETCH - 1; i >= 0 ; i--) begin
        unique case ({is_branch[i], is_return[i], is_jump[i], is_jalr[i]})
          4'b0000:;  
          4'b0001: begin
            ras_pop = 1'b0;
            ras_push = 1'b0;
            if (btb_prediction_shifted[i].valid) begin
              predict_address = btb_prediction_shifted[i].target_address;
              cf_type[i] = ariane_pkg::JumpR;
            end
          end
          4'b0010: begin
            ras_pop = 1'b0;
            ras_push = 1'b0;
            taken_rvi_cf[i] = rvi_jump[i];
            taken_rvc_cf[i] = rvc_jump[i];
            cf_type[i] = ariane_pkg::Jump;
          end
          4'b0100: begin
            ras_pop = ras_predict.valid & instr_queue_consumed[i];
            ras_push = 1'b0;
            predict_address = ras_predict.ra;
            cf_type[i] = ariane_pkg::Return;
          end
          4'b1000: begin
            ras_pop = 1'b0;
            ras_push = 1'b0;
            if (bht_prediction_shifted[i].valid) begin
              taken_rvi_cf[i] = rvi_branch[i] & bht_prediction_shifted[i].taken;
              taken_rvc_cf[i] = rvc_branch[i] & bht_prediction_shifted[i].taken;
            end else begin
              taken_rvi_cf[i] = rvi_branch[i] & rvi_imm[i][riscv::VLEN-1];
              taken_rvc_cf[i] = rvc_branch[i] & rvc_imm[i][riscv::VLEN-1];
            end
            if (taken_rvi_cf[i] || taken_rvc_cf[i]) cf_type[i] = ariane_pkg::Branch;
          end
          default:;
        endcase
          if (is_call[i]) begin
            ras_push = instr_queue_consumed[i];
            ras_update = addr[i] + (rvc_call[i] ? 2 : 4);
          end
          if (taken_rvc_cf[i] || taken_rvi_cf[i]) begin
            predict_address = addr[i] + (taken_rvc_cf[i] ? rvc_imm[i] : rvi_imm[i]);
          end
      end
    end
    always_comb begin
      bp_valid = 1'b0;
      for (int i = 0; i < INSTR_PER_FETCH; i++) bp_valid |= ((cf_type[i] != NoCF & cf_type[i] != Return) | ((cf_type[i] == Return) & ras_predict.valid));
    end
    assign is_mispredict = resolved_branch_i.valid & resolved_branch_i.is_mispredict;
    assign icache_dreq_o.req = instr_queue_ready;
    assign if_ready = icache_dreq_i.ready & instr_queue_ready;
    assign icache_dreq_o.kill_s1 = is_mispredict | flush_i | replay;
    assign icache_dreq_o.kill_s2 = icache_dreq_o.kill_s1 | bp_valid;
    bht_update_t bht_update;
    btb_update_t btb_update;
    logic speculative_q,speculative_d;
    assign speculative_d = (speculative_q && !resolved_branch_i.valid || |is_branch || |is_return || |is_jalr) && !flush_i;
    assign icache_dreq_o.spec = speculative_d;
    assign bht_update.valid = resolved_branch_i.valid
                                & (resolved_branch_i.cf_type == ariane_pkg::Branch);
    assign bht_update.pc    = resolved_branch_i.pc;
    assign bht_update.taken = resolved_branch_i.is_taken;
    assign btb_update.valid = resolved_branch_i.valid
                                & resolved_branch_i.is_mispredict
                                & (resolved_branch_i.cf_type == ariane_pkg::JumpR);
    assign btb_update.pc    = resolved_branch_i.pc;
    assign btb_update.target_address = resolved_branch_i.target_address;
    always_comb begin : npc_select
      automatic logic [riscv::VLEN-1:0] fetch_address;
      if (npc_rst_load_q) begin
        npc_d         = boot_addr_i;
        fetch_address = boot_addr_i;
      end else begin
        fetch_address    = npc_q;
        npc_d            = npc_q;
      end
      if (bp_valid) begin
        fetch_address = predict_address;
        npc_d = predict_address;
      end
      if (if_ready) npc_d = {fetch_address[riscv::VLEN-1:2], 2'b0}  + 'h4;
      if (replay) npc_d = replay_addr;
      if (is_mispredict) npc_d = resolved_branch_i.target_address;
      if (eret_i) npc_d = epc_i;
      if (ex_valid_i) npc_d = trap_vector_base_i;
      if (set_pc_commit_i) npc_d = pc_commit_i + {{riscv::VLEN-3{1'b0}}, 3'b100};
      if (set_debug_pc_i) npc_d = ArianeCfg.DmBaseAddress[riscv::VLEN-1:0] + dm::HaltAddress[riscv::VLEN-1:0];
      icache_dreq_o.vaddr = fetch_address;
    end
    logic [FETCH_WIDTH-1:0] icache_data;
    assign icache_data = icache_dreq_i.data >> {shamt, 4'b0};
    always_ff @(posedge clk_i or negedge rst_ni) begin
      if (!rst_ni) begin
        npc_rst_load_q    <= 1'b1;
        npc_q             <= '0;
        speculative_q     <= '0;
        icache_data_q     <= '0;
        icache_valid_q    <= 1'b0;
        icache_vaddr_q    <= 'b0;
        icache_ex_valid_q <= ariane_pkg::FE_NONE;
        btb_q             <= '0;
        bht_q             <= '0;
      end else begin
        npc_rst_load_q    <= 1'b0;
        npc_q             <= npc_d;
        speculative_q    <= speculative_d;
        icache_valid_q    <= icache_dreq_i.valid;
        if (icache_dreq_i.valid) begin
          icache_data_q        <= icache_data;
          icache_vaddr_q       <= icache_dreq_i.vaddr;
          if (icache_dreq_i.ex.cause == riscv::INSTR_PAGE_FAULT) begin
            icache_ex_valid_q <= ariane_pkg::FE_INSTR_PAGE_FAULT;
          end else if (icache_dreq_i.ex.cause == riscv::INSTR_ACCESS_FAULT) begin
            icache_ex_valid_q <= ariane_pkg::FE_INSTR_ACCESS_FAULT;
          end else icache_ex_valid_q <= ariane_pkg::FE_NONE;
          btb_q                <= btb_prediction[INSTR_PER_FETCH-1];
          bht_q                <= bht_prediction[INSTR_PER_FETCH-1];
        end
      end
    end // always_ff @ (posedge clk_i or negedge rst_ni)
/*   
    ras #(
      .DEPTH  ( ArianeCfg.RASDepth  )
    ) i_ras (
      .clk_i,
      .rst_ni,
      .flush_i( flush_bp_i  ),
      .push_i ( ras_push    ),
      .pop_i  ( ras_pop     ),
      .data_i ( ras_update  ),
      .data_o ( ras_predict )
    );
    btb #(
      .NR_ENTRIES       ( ArianeCfg.BTBEntries   )
    ) i_btb (
      .clk_i,
      .rst_ni,
      .flush_i          ( flush_bp_i       ),
      .debug_mode_i,
      .vpc_i            ( icache_vaddr_q   ),
      .btb_update_i     ( btb_update       ),
      .btb_prediction_o ( btb_prediction   )
    );
    bht #(
      .NR_ENTRIES       ( ArianeCfg.BHTEntries   )
    ) i_bht (
      .clk_i,
      .rst_ni,
      .flush_i          ( flush_bp_i       ),
      .debug_mode_i,
      .vpc_i            ( icache_vaddr_q   ),
      .bht_update_i     ( bht_update       ),
      .bht_prediction_o ( bht_prediction   )
    );
    for (genvar i = 0; i < INSTR_PER_FETCH; i++) begin : gen_instr_scan
      instr_scan i_instr_scan (
        .instr_i      ( instr[i]      ),
        .rvi_return_o ( rvi_return[i] ),
        .rvi_call_o   ( rvi_call[i]   ),
        .rvi_branch_o ( rvi_branch[i] ),
        .rvi_jalr_o   ( rvi_jalr[i]   ),
        .rvi_jump_o   ( rvi_jump[i]   ),
        .rvi_imm_o    ( rvi_imm[i]    ),
        .rvc_branch_o ( rvc_branch[i] ),
        .rvc_jump_o   ( rvc_jump[i]   ),
        .rvc_jr_o     ( rvc_jr[i]     ),
        .rvc_return_o ( rvc_return[i] ),
        .rvc_jalr_o   ( rvc_jalr[i]   ),
        .rvc_call_o   ( rvc_call[i]   ),
        .rvc_imm_o    ( rvc_imm[i]    )
      );
    end
    instr_queue i_instr_queue (
      .clk_i               ( clk_i                ),
      .rst_ni              ( rst_ni               ),
      .flush_i             ( flush_i              ),
      .instr_i             ( instr                ),  
      .addr_i              ( addr                 ),  
      .exception_i         ( icache_ex_valid_q    ),  
      .exception_addr_i    ( icache_vaddr_q       ),
      .predict_address_i   ( predict_address      ),
      .cf_type_i           ( cf_type              ),
      .valid_i             ( instruction_valid    ),  
      .consumed_o          ( instr_queue_consumed ),
      .ready_o             ( instr_queue_ready    ),
      .replay_o            ( replay               ),
      .replay_addr_o       ( replay_addr          ),
      .fetch_entry_o       ( fetch_entry_o        ),  
      .fetch_entry_valid_o ( fetch_entry_valid_o  ),  
      .fetch_entry_ready_i ( fetch_entry_ready_i  )   
    );
 */
endmodule
module instr_realign import ariane_pkg::*; (
    input  logic                              clk_i,
    input  logic                              rst_ni,
    input  logic                              flush_i,
    input  logic                              valid_i,
    output logic                              serving_unaligned_o,  
    input  logic [riscv::VLEN-1:0]            address_i,
    input  logic [FETCH_WIDTH-1:0]            data_i,
    output logic [INSTR_PER_FETCH-1:0]        valid_o,
    output logic [INSTR_PER_FETCH-1:0][riscv::VLEN-1:0]  addr_o,
    output logic [INSTR_PER_FETCH-1:0][31:0]  instr_o
);
    logic [3:0] instr_is_compressed;
    for (genvar i = 0; i < INSTR_PER_FETCH; i ++) begin
        assign instr_is_compressed[i] = ~&data_i[i * 16 +: 2];
    end
    logic [15:0] unaligned_instr_d,   unaligned_instr_q;
    logic        unaligned_d,         unaligned_q;
    logic [riscv::VLEN-1:0] unaligned_address_d, unaligned_address_q;
    assign serving_unaligned_o = unaligned_q;
    if (FETCH_WIDTH == 32) begin : realign_bp_32
        always_comb begin : re_align
            unaligned_d = unaligned_q;
            unaligned_address_d = {address_i[riscv::VLEN-1:2], 2'b10};
            unaligned_instr_d = data_i[31:16];
            valid_o[0] = valid_i;
            instr_o[0] = (unaligned_q) ? {data_i[15:0], unaligned_instr_q} : data_i[31:0];
            addr_o[0]  = (unaligned_q) ? unaligned_address_q : address_i;
            valid_o[1] = 1'b0;
            instr_o[1] = '0;
            addr_o[1]  = {address_i[riscv::VLEN-1:2], 2'b10};
            if (instr_is_compressed[0] || unaligned_q) begin
                if (instr_is_compressed[1]) begin
                    unaligned_d = 1'b0;
                    valid_o[1] = valid_i;
                    instr_o[1] = {16'b0, data_i[31:16]};
                end else begin
                    unaligned_d = 1'b1;
                    unaligned_instr_d = data_i[31:16];
                    unaligned_address_d = {address_i[riscv::VLEN-1:2], 2'b10};
                end
            end  
            if (valid_i && address_i[1]) begin
                if (!instr_is_compressed[0]) begin
                    valid_o = '0;
                    unaligned_d = 1'b1;
                    unaligned_address_d = {address_i[riscv::VLEN-1:2], 2'b10};
                    unaligned_instr_d = data_i[15:0];
                end else begin
                    valid_o = 1'b1;
                end
            end
        end
    end else if (FETCH_WIDTH == 64) begin : realign_bp_64
        initial begin
          $error("Not propperly implemented");
        end
        always_comb begin : re_align
            unaligned_d = unaligned_q;
            unaligned_address_d = unaligned_address_q;
            unaligned_instr_d = unaligned_instr_q;
            valid_o    = '0;
            valid_o[0] = valid_i;
            instr_o[0] = data_i[31:0];
            addr_o[0]  = address_i;
            instr_o[1] = '0;
            addr_o[1]  = {address_i[riscv::VLEN-1:3], 3'b010};
            instr_o[2] = {16'b0, data_i[47:32]};
            addr_o[2]  = {address_i[riscv::VLEN-1:3], 3'b100};
            instr_o[3] = {16'b0, data_i[63:48]};
            addr_o[3]  = {address_i[riscv::VLEN-1:3], 3'b110};
            if (unaligned_q) begin
                instr_o[0] = {data_i[15:0], unaligned_instr_q};
                addr_o[0] = unaligned_address_q;
                if (instr_is_compressed[1]) begin
                    instr_o[1] = {16'b0, data_i[31:16]};
                    valid_o[1] = valid_i;
                    if (instr_is_compressed[2]) begin
                        if (instr_is_compressed[3]) begin
                            unaligned_d = 1'b0;
                            valid_o[3] = valid_i;
                        end else begin
                        end
                    end else begin
                        unaligned_d = 1'b0;
                        instr_o[2] = data_i[63:32];
                        valid_o[2] = valid_i;
                    end
                end else begin
                    instr_o[1] = data_i[47:16];
                    valid_o[1] = valid_i;
                    addr_o[2] = {address_i[riscv::VLEN-1:3], 3'b110};
                    if (instr_is_compressed[2]) begin
                        unaligned_d = 1'b0;
                        instr_o[2] = {16'b0, data_i[63:48]};
                        valid_o[2] = valid_i;
                    end else begin
                    end
                end
            end else if (instr_is_compressed[0]) begin  
                if (instr_is_compressed[1]) begin
                    instr_o[1] = {16'b0, data_i[31:16]};
                    valid_o[1] = valid_i;
                    if (instr_is_compressed[2]) begin
                        valid_o[2] = valid_i;
                        if (instr_is_compressed[3]) begin
                            valid_o[3] = valid_i;
                        end else begin
                            unaligned_d = 1'b1;
                            unaligned_instr_d = data_i[63:48];
                            unaligned_address_d = addr_o[3];
                        end
                    end else begin
                        instr_o[2] = data_i[63:32];
                        valid_o[2] = valid_i;
                    end
                end else begin
                    instr_o[1] = data_i[47:16];
                    valid_o[1] = valid_i;
                    addr_o[2] = {address_i[riscv::VLEN-1:3], 3'b110};
                    if (instr_is_compressed[3]) begin
                        instr_o[2] = data_i[63:48];
                        valid_o[2] = valid_i;
                    end else begin
                        unaligned_d = 1'b1;
                        unaligned_instr_d = data_i[63:48];
                        unaligned_address_d = addr_o[2];
                    end
                end
            end else begin
                addr_o[1] = {address_i[riscv::VLEN-1:3], 3'b100};
                if (instr_is_compressed[2]) begin
                    instr_o[1] = {16'b0, data_i[47:32]};
                    valid_o[1] = valid_i;
                    addr_o[2] = {address_i[riscv::VLEN-1:3], 3'b110};
                    if (instr_is_compressed[3]) begin
                        valid_o[2] = valid_i;
                        addr_o[2] = {16'b0, data_i[63:48]};
                    end else begin
                        unaligned_d = 1'b1;
                        unaligned_instr_d = data_i[63:48];
                        unaligned_address_d = addr_o[2];
                    end
                end else begin
                    instr_o[1] = data_i[63:32];
                    valid_o[1] = valid_i;
                end
            end
            case (address_i[2:1])
                2'b01: begin
                    addr_o[0] = {address_i[riscv::VLEN-1:3], 3'b010};
                    if (instr_is_compressed[1]) begin
                        instr_o[0] = {16'b0, data_i[31:16]};
                        valid_o[0] = valid_i;
                        if (instr_is_compressed[2]) begin
                            valid_o[1] = valid_i;
                            instr_o[1] = {16'b0, data_i[47:32]};
                            addr_o[1] = {address_i[riscv::VLEN-1:3], 3'b100};
                            if (instr_is_compressed[3]) begin
                                instr_o[2] = {16'b0, data_i[63:48]};
                                addr_o[2] = {address_i[riscv::VLEN-1:3], 3'b110};
                                valid_o[2] = valid_i;
                            end else begin
                                unaligned_d = 1'b1;
                                unaligned_instr_d = data_i[63:48];
                                unaligned_address_d = addr_o[3];
                            end
                        end else begin
                            instr_o[1] = data_i[63:32];
                            addr_o[1] = {address_i[riscv::VLEN-1:3], 3'b100};
                            valid_o[1] = valid_i;
                        end
                    end else begin
                        instr_o[0] = data_i[47:16];
                        valid_o[0] = valid_i;
                        addr_o[1] = {address_i[riscv::VLEN-1:3], 3'b110};
                        if (instr_is_compressed[3]) begin
                            instr_o[1] = data_i[63:48];
                            valid_o[1] = valid_i;
                        end else begin
                            unaligned_d = 1'b1;
                            unaligned_instr_d = data_i[63:48];
                            unaligned_address_d = addr_o[1];
                        end
                    end
                end
                2'b10: begin
                    valid_o = '0;
                    if (instr_is_compressed[2]) begin
                        valid_o[0] = valid_i;
                        instr_o[0] = data_i[47:32];
                        if (instr_is_compressed[3]) begin
                            valid_o[1] = valid_i;
                            instr_o[1] = data_i[63:48];
                        end else begin
                            unaligned_d = 1'b1;
                            unaligned_address_d = {address_i[riscv::VLEN-1:3], 3'b110};
                            unaligned_instr_d = data_i[63:48];
                        end
                    end else begin
                        valid_o[0] = valid_i;
                        instr_o[0] = data_i[63:32];
                        addr_o[0] = address_i;
                    end
                end
                2'b11: begin
                    valid_o = '0;
                    if (!instr_is_compressed[3]) begin
                        unaligned_d = 1'b1;
                        unaligned_address_d = {address_i[riscv::VLEN-1:3], 3'b110};
                        unaligned_instr_d = data_i[63:48];
                    end else begin
                        valid_o[3] = valid_i;
                    end
                end
            endcase
        end
    end
    always_ff @(posedge clk_i or negedge rst_ni) begin
        if (~rst_ni) begin
            unaligned_q         <= 1'b0;
            unaligned_address_q <= '0;
            unaligned_instr_q   <= '0;
        end else begin
            if (valid_i) begin
                unaligned_address_q <= unaligned_address_d;
                unaligned_instr_q   <= unaligned_instr_d;
            end
            if (flush_i) begin
                unaligned_q <= 1'b0;
            end else if (valid_i) begin
                unaligned_q <= unaligned_d;
            end
        end
    end
endmodule
