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
    typedef struct packed {
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
package std_cache_pkg;
    localparam DCACHE_BYTE_OFFSET = $clog2(ariane_pkg::DCACHE_LINE_WIDTH/8);
    localparam DCACHE_NUM_WORDS   = 2**(ariane_pkg::DCACHE_INDEX_WIDTH-DCACHE_BYTE_OFFSET);
    localparam DCACHE_DIRTY_WIDTH = ariane_pkg::DCACHE_SET_ASSOC*2;
    typedef struct packed {
        logic [1:0]      id;      
        logic            valid;
        logic            we;
        logic [55:0]     addr;
        logic [7:0][7:0] wdata;
        logic [7:0]      be;
    } mshr_t;
    typedef struct packed {
        logic         valid;
        logic [63:0]  addr;
        logic [7:0]   be;
        logic [1:0]   size;
        logic         we;
        logic [63:0]  wdata;
        logic         bypass;
    } miss_req_t;
    typedef struct packed {
        logic [ariane_pkg::DCACHE_TAG_WIDTH-1:0]  tag;     
        logic [ariane_pkg::DCACHE_LINE_WIDTH-1:0] data;    
        logic                                     valid;   
        logic                                     dirty;   
    } cache_line_t;
    typedef struct packed {
        logic [(ariane_pkg::DCACHE_TAG_WIDTH+7)/8-1:0]  tag;     
        logic [(ariane_pkg::DCACHE_LINE_WIDTH+7)/8-1:0] data;    
        logic [ariane_pkg::DCACHE_SET_ASSOC-1:0]        vldrty;  
    } cl_be_t;
    function automatic logic [$clog2(ariane_pkg::DCACHE_SET_ASSOC)-1:0] one_hot_to_bin (
        input logic [ariane_pkg::DCACHE_SET_ASSOC-1:0] in
    );
        for (int unsigned i = 0; i < ariane_pkg::DCACHE_SET_ASSOC; i++) begin
            if (in[i])
                return i;
        end
    endfunction
    function automatic logic [ariane_pkg::DCACHE_SET_ASSOC-1:0] get_victim_cl (
        input logic [ariane_pkg::DCACHE_SET_ASSOC-1:0] valid_dirty
    );
        logic [ariane_pkg::DCACHE_SET_ASSOC-1:0] oh = '0;
        for (int unsigned i = 0; i < ariane_pkg::DCACHE_SET_ASSOC; i++) begin
            if (valid_dirty[i]) begin
                oh[i] = 1'b1;
                return oh;
            end
        end
    endfunction
endpackage : std_cache_pkg
package wt_cache_pkg;
  localparam L15_SET_ASSOC           = ariane_pkg::DCACHE_SET_ASSOC; 
  localparam L15_TID_WIDTH           = 2;
  localparam L15_TLB_CSM_WIDTH       = 33;
  localparam L15_WAY_WIDTH           = $clog2(L15_SET_ASSOC);
  localparam L1I_WAY_WIDTH           = $clog2(ariane_pkg::ICACHE_SET_ASSOC);
  localparam L1D_WAY_WIDTH           = $clog2(ariane_pkg::DCACHE_SET_ASSOC);
  localparam ADAPTER_REQ_FIFO_DEPTH  = 2;
  localparam ADAPTER_RTRN_FIFO_DEPTH = 2;
  localparam ICACHE_OFFSET_WIDTH     = $clog2(ariane_pkg::ICACHE_LINE_WIDTH/8);
  localparam ICACHE_NUM_WORDS        = 2**(ariane_pkg::ICACHE_INDEX_WIDTH-ICACHE_OFFSET_WIDTH);
  localparam ICACHE_CL_IDX_WIDTH     = $clog2(ICACHE_NUM_WORDS); 
  localparam DCACHE_OFFSET_WIDTH     = $clog2(ariane_pkg::DCACHE_LINE_WIDTH/8);
  localparam DCACHE_NUM_WORDS        = 2**(ariane_pkg::DCACHE_INDEX_WIDTH-DCACHE_OFFSET_WIDTH);
  localparam DCACHE_CL_IDX_WIDTH     = $clog2(DCACHE_NUM_WORDS); 
  localparam DCACHE_NUM_BANKS        = ariane_pkg::DCACHE_LINE_WIDTH/64;
  localparam DCACHE_NUM_BANKS_WIDTH  = $clog2(DCACHE_NUM_BANKS);
  localparam DCACHE_WBUF_DEPTH       = 8;
  localparam DCACHE_MAX_TX           = 2**L15_TID_WIDTH;
  localparam CACHE_ID_WIDTH          = L15_TID_WIDTH;
  typedef struct packed {
    logic [ariane_pkg::DCACHE_INDEX_WIDTH+ariane_pkg::DCACHE_TAG_WIDTH-1:0] wtag;
    logic [63:0]                                                            data;
    logic [7:0]                                                             dirty;    
    logic [7:0]                                                             valid;    
    logic [7:0]                                                             txblock;  
    logic                                                                   checked;  
    logic [ariane_pkg::DCACHE_SET_ASSOC-1:0]                                hit_oh;   
  } wbuffer_t;
  typedef struct packed {
    logic                                 vld;
    logic [7:0]                           be;
    logic [$clog2(DCACHE_WBUF_DEPTH)-1:0] ptr;
  } tx_stat_t;
  typedef enum logic [1:0] {
    DCACHE_STORE_REQ,
    DCACHE_LOAD_REQ,
    DCACHE_ATOMIC_REQ,
    DCACHE_INT_REQ
  }  dcache_out_t;
  typedef enum logic [2:0] {
    DCACHE_INV_REQ,   
    DCACHE_STORE_ACK, 
    DCACHE_LOAD_ACK,
    DCACHE_ATOMIC_ACK,
    DCACHE_INT_ACK
  }  dcache_in_t;
  typedef enum logic [0:0] {
    ICACHE_INV_REQ,  
    ICACHE_IFILL_ACK
  } icache_in_t;
  typedef struct packed {
    logic                                            vld;          
    logic                                            all;          
    logic [ariane_pkg::ICACHE_INDEX_WIDTH-1:0]       idx;          
    logic [L15_WAY_WIDTH-1:0]                        way;          
  } icache_inval_t;
  typedef struct packed {
    logic [$clog2(ariane_pkg::ICACHE_SET_ASSOC)-1:0] way;          
    logic [riscv::PLEN-1:0]                          paddr;        
    logic                                            nc;           
    logic [CACHE_ID_WIDTH-1:0]                       tid;          
  } icache_req_t;
  typedef struct packed {
    icache_in_t                                      rtype;        
    logic [ariane_pkg::ICACHE_LINE_WIDTH-1:0]        data;         
    icache_inval_t                                   inv;          
    logic [CACHE_ID_WIDTH-1:0]                       tid;          
  } icache_rtrn_t;
  typedef struct packed {
    logic                                            vld;          
    logic                                            all;          
    logic [ariane_pkg::DCACHE_INDEX_WIDTH-1:0]       idx;          
    logic [L15_WAY_WIDTH-1:0]                        way;          
  } dcache_inval_t;
  typedef struct packed {
    dcache_out_t                                     rtype;        
    logic [2:0]                                      size;         
    logic [L1D_WAY_WIDTH-1:0]                        way;          
    logic [riscv::PLEN-1:0]                          paddr;        
    logic [63:0]                                     data;         
    logic                                            nc;           
    logic [CACHE_ID_WIDTH-1:0]                       tid;          
    ariane_pkg::amo_t                                amo_op;       
  } dcache_req_t;
  typedef struct packed {
    dcache_in_t                                      rtype;        
    logic [ariane_pkg::DCACHE_LINE_WIDTH-1:0]        data;         
    dcache_inval_t                                   inv;          
    logic [CACHE_ID_WIDTH-1:0]                       tid;          
  } dcache_rtrn_t;
  typedef enum logic [4:0] {
    L15_LOAD_RQ     = 5'b00000,  
    L15_IMISS_RQ    = 5'b10000,  
    L15_STORE_RQ    = 5'b00001,  
    L15_ATOMIC_RQ   = 5'b00110,  
    L15_STRLOAD_RQ  = 5'b00100,  
    L15_STRST_RQ    = 5'b00101,  
    L15_STQ_RQ      = 5'b00111,  
    L15_INT_RQ      = 5'b01001,  
    L15_FWD_RQ      = 5'b01101,  
    L15_FWD_RPY     = 5'b01110,  
    L15_RSVD_RQ     = 5'b11111   
  } l15_reqtypes_t;
  typedef enum logic [3:0] {
    L15_LOAD_RET               = 4'b0000,  
    L15_ST_ACK                 = 4'b0100,  
    L15_INT_RET                = 4'b0111,  
    L15_TEST_RET               = 4'b0101,  
    L15_FP_RET                 = 4'b1000,  
    L15_IFILL_RET              = 4'b0001,  
    L15_EVICT_REQ              = 4'b0011,  
    L15_ERR_RET                = 4'b1100,  
    L15_STRLOAD_RET            = 4'b0010,  
    L15_STRST_ACK              = 4'b0110,  
    L15_FWD_RQ_RET             = 4'b1010,  
    L15_FWD_RPY_RET            = 4'b1011,  
    L15_RSVD_RET               = 4'b1111,  
    L15_CPX_RESTYPE_ATOMIC_RES = 4'b1110   
  } l15_rtrntypes_t;
  typedef struct packed {
    logic                              l15_val;                    
    logic                              l15_req_ack;                
    l15_reqtypes_t                     l15_rqtype;                 
    logic                              l15_nc;                     
    logic [2:0]                        l15_size;                   
    logic [L15_TID_WIDTH-1:0]          l15_threadid;               
    logic                              l15_prefetch;               
    logic                              l15_invalidate_cacheline;   
    logic                              l15_blockstore;             
    logic                              l15_blockinitstore;         
    logic [L15_WAY_WIDTH-1:0]          l15_l1rplway;               
    logic [39:0]                       l15_address;                
    logic [63:0]                       l15_data;                   
    logic [63:0]                       l15_data_next_entry;        
    logic [L15_TLB_CSM_WIDTH-1:0]      l15_csm_data;               
    logic [3:0]                        l15_amo_op;                 
  } l15_req_t;
  typedef struct packed {
    logic                              l15_ack;                    
    logic                              l15_header_ack;             
    logic                              l15_val;                    
    l15_rtrntypes_t                    l15_returntype;             
    logic                              l15_l2miss;                 
    logic [1:0]                        l15_error;                  
    logic                              l15_noncacheable;           
    logic                              l15_atomic;                 
    logic [L15_TID_WIDTH-1:0]          l15_threadid;               
    logic                              l15_prefetch;               
    logic                              l15_f4b;                    
    logic [63:0]                       l15_data_0;                 
    logic [63:0]                       l15_data_1;                 
    logic [63:0]                       l15_data_2;                 
    logic [63:0]                       l15_data_3;                 
    logic                              l15_inval_icache_all_way;   
    logic                              l15_inval_dcache_all_way;   
    logic [15:4]                       l15_inval_address_15_4;     
    logic                              l15_cross_invalidate;       
    logic [L15_WAY_WIDTH-1:0]          l15_cross_invalidate_way;   
    logic                              l15_inval_dcache_inval;     
    logic                              l15_inval_icache_inval;     
    logic [L15_WAY_WIDTH-1:0]          l15_inval_way;              
    logic                              l15_blockinitstore;         
  } l15_rtrn_t;
  function automatic logic[63:0] swendian64(input logic[63:0] in);
    automatic logic[63:0] out;
    for(int k=0; k<64;k+=8)begin
        out[k +: 8] = in[63-k -: 8];
    end
    return out;
  endfunction
  function automatic logic [ariane_pkg::ICACHE_SET_ASSOC-1:0] icache_way_bin2oh (
    input logic [L1I_WAY_WIDTH-1:0] in
  );
    logic [ariane_pkg::ICACHE_SET_ASSOC-1:0] out;
    out     = '0;
    out[in] = 1'b1;
    return out;
  endfunction
  function automatic logic [ariane_pkg::DCACHE_SET_ASSOC-1:0] dcache_way_bin2oh (
    input logic [L1D_WAY_WIDTH-1:0] in
  );
    logic [ariane_pkg::DCACHE_SET_ASSOC-1:0] out;
    out     = '0;
    out[in] = 1'b1;
    return out;
  endfunction
  function automatic logic [DCACHE_NUM_BANKS-1:0] dcache_cl_bin2oh (
    input logic [DCACHE_NUM_BANKS_WIDTH-1:0] in
  );
    logic [DCACHE_NUM_BANKS-1:0] out;
    out     = '0;
    out[in] = 1'b1;
    return out;
  endfunction
  function automatic logic [5:0] popcnt64 (
    input logic [63:0] in
  );
    logic [5:0] cnt= 0;
    foreach (in[k]) begin
      cnt += 6'(in[k]);
    end
    return cnt;
  endfunction : popcnt64
  function automatic logic [7:0] toByteEnable8(
    input logic [2:0] offset,
    input logic [1:0] size
  );
    logic [7:0] be;
    be = '0;
    unique case(size)
      2'b00:   be[offset]       = '1;  
      2'b01:   be[offset +:2 ]  = '1;  
      2'b10:   be[offset +:4 ]  = '1;  
      default: be               = '1;  
    endcase  
    return be;
  endfunction : toByteEnable8
  function automatic logic [63:0] repData64(
    input logic [63:0] data,
    input logic [2:0]  offset,
    input logic [1:0]  size
  );
    logic [63:0] out;
    unique case(size)
      2'b00:   for(int k=0; k<8; k++) out[k*8  +: 8]    = data[offset*8 +: 8];   
      2'b01:   for(int k=0; k<4; k++) out[k*16 +: 16]   = data[offset*8 +: 16];  
      2'b10:   for(int k=0; k<2; k++) out[k*32 +: 32]   = data[offset*8 +: 32];  
      default: out   = data;  
    endcase  
    return out;
  endfunction : repData64
  function automatic logic [1:0] toSize64(
    input logic  [7:0] be
  );
    logic [1:0] size;
    unique case(be)
      8'b1111_1111:                                           size = 2'b11;   
      8'b0000_1111, 8'b1111_0000:                             size = 2'b10;  
      8'b1100_0000, 8'b0011_0000, 8'b0000_1100, 8'b0000_0011: size = 2'b01;  
      default:                                                size = 2'b00;  
    endcase  
    return size;
  endfunction : toSize64
  function automatic logic [riscv::PLEN-1:0] paddrSizeAlign(
    input logic [riscv::PLEN-1:0] paddr,
    input logic [2:0]  size
  );
    logic [riscv::PLEN-1:0] out;
    out = paddr;
    unique case (size)
      3'b001: out[0:0]                     = '0;
      3'b010: out[1:0]                     = '0;
      3'b011: out[2:0]                     = '0;
      3'b111: out[DCACHE_OFFSET_WIDTH-1:0] = '0;
      default: ;
    endcase
    return out;
  endfunction : paddrSizeAlign
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
package reg_intf;
    typedef struct packed {
        logic [31:0] addr;
        logic        write;
        logic [31:0] wdata;
        logic [3:0]  wstrb;
        logic        valid;
    } reg_intf_req_a32_d32;
    typedef struct packed {
        logic [31:0] addr;
        logic        write;
        logic [63:0] wdata;
        logic [7:0]  wstrb;
        logic        valid;
    } reg_intf_req_a32_d64;
    typedef struct packed {
        logic [31:0] rdata;
        logic        error;
        logic        ready;
    } reg_intf_resp_d32;
    typedef struct packed {
        logic [63:0] rdata;
        logic        error;
        logic        ready;
    } reg_intf_resp_d64;
endpackage

interface REG_BUS #(
  parameter int ADDR_WIDTH = -1,
  parameter int DATA_WIDTH = -1
)(
  input logic clk_i
);
  logic [ADDR_WIDTH-1:0]   addr;
  logic                    write;  
  logic [DATA_WIDTH-1:0]   rdata;
  logic [DATA_WIDTH-1:0]   wdata;
  logic [DATA_WIDTH/8-1:0] wstrb;  
  logic                    error;  
  logic                    valid;
  logic                    ready;
  modport in  (input  addr, write, wdata, wstrb, valid, output rdata, error, ready);
  modport out (output addr, write, wdata, wstrb, valid, input  rdata, error, ready);
endinterface // REG_BUS
   
interface AXI_BUS #(
  parameter AXI_ADDR_WIDTH = -1,
  parameter AXI_DATA_WIDTH = -1,
  parameter AXI_ID_WIDTH   = -1,
  parameter AXI_USER_WIDTH = -1
);
  import axi_pkg::*;
  localparam AXI_STRB_WIDTH = AXI_DATA_WIDTH / 8;
  typedef logic [AXI_ID_WIDTH-1:0]   id_t;
  typedef logic [AXI_ADDR_WIDTH-1:0] addr_t;
  typedef logic [AXI_DATA_WIDTH-1:0] data_t;
  typedef logic [AXI_STRB_WIDTH-1:0] strb_t;
  typedef logic [AXI_USER_WIDTH-1:0] user_t;
  typedef logic [5:0] atop_t;
  id_t        aw_id;
  addr_t      aw_addr;
  logic [7:0] aw_len;
  logic [2:0] aw_size;
  burst_t     aw_burst;
  logic       aw_lock;
  cache_t     aw_cache;
  prot_t      aw_prot;
  qos_t       aw_qos;
  atop_t      aw_atop;
  region_t    aw_region;
  user_t      aw_user;
  logic       aw_valid;
  logic       aw_ready;
  data_t      w_data;
  strb_t      w_strb;
  logic       w_last;
  user_t      w_user;
  logic       w_valid;
  logic       w_ready;
  id_t        b_id;
  resp_t      b_resp;
  user_t      b_user;
  logic       b_valid;
  logic       b_ready;
  id_t        ar_id;
  addr_t      ar_addr;
  logic [7:0] ar_len;
  logic [2:0] ar_size;
  burst_t     ar_burst;
  logic       ar_lock;
  cache_t     ar_cache;
  prot_t      ar_prot;
  qos_t       ar_qos;
  region_t    ar_region;
  user_t      ar_user;
  logic       ar_valid;
  logic       ar_ready;
  id_t        r_id;
  data_t      r_data;
  resp_t      r_resp;
  logic       r_last;
  user_t      r_user;
  logic       r_valid;
  logic       r_ready;
  modport Master (
    output aw_id, aw_addr, aw_len, aw_size, aw_burst, aw_lock, aw_cache, aw_prot, aw_qos, aw_atop, aw_region, aw_user, aw_valid, input aw_ready,
    output w_data, w_strb, w_last, w_user, w_valid, input w_ready,
    input b_id, b_resp, b_user, b_valid, output b_ready,
    output ar_id, ar_addr, ar_len, ar_size, ar_burst, ar_lock, ar_cache, ar_prot, ar_qos, ar_region, ar_user, ar_valid, input ar_ready,
    input r_id, r_data, r_resp, r_last, r_user, r_valid, output r_ready
  );
  modport Slave (
    input aw_id, aw_addr, aw_len, aw_size, aw_burst, aw_lock, aw_cache, aw_prot, aw_qos, aw_atop, aw_region, aw_user, aw_valid, output aw_ready,
    input w_data, w_strb, w_last, w_user, w_valid, output w_ready,
    output b_id, b_resp, b_user, b_valid, input b_ready,
    input ar_id, ar_addr, ar_len, ar_size, ar_burst, ar_lock, ar_cache, ar_prot, ar_qos, ar_region, ar_user, ar_valid, output ar_ready,
    output r_id, r_data, r_resp, r_last, r_user, r_valid, input r_ready
  );
endinterface // AXI_BUS

interface uart_bus #(
    parameter int unsigned BAUD_RATE = 115200,
    parameter int unsigned PARITY_EN = 0
)(
    input  logic rx,
    output logic tx,
    input  logic rx_en
);
endinterface
   
module axi2apb_64_32 #(
    parameter int unsigned AXI4_ADDRESS_WIDTH = 32,
    parameter int unsigned AXI4_RDATA_WIDTH   = 64,
    parameter int unsigned AXI4_WDATA_WIDTH   = 64,
    parameter int unsigned AXI4_ID_WIDTH      = 16,
    parameter int unsigned AXI4_USER_WIDTH    = 10,
    parameter int unsigned AXI_NUMBYTES       = AXI4_WDATA_WIDTH/8,
    parameter int unsigned BUFF_DEPTH_SLAVE   = 4,
    parameter int unsigned APB_NUM_SLAVES     = 8,
    parameter int unsigned APB_ADDR_WIDTH     = 12
)
(
    input logic                           ACLK,
    input logic                           ARESETn,
    input logic                           test_en_i,
    input  logic [AXI4_ID_WIDTH-1:0]       AWID_i     ,
    input  logic [AXI4_ADDRESS_WIDTH-1:0]  AWADDR_i   ,
    input  logic [ 7:0]                    AWLEN_i    ,
    input  logic [ 2:0]                    AWSIZE_i   ,
    input  logic [ 1:0]                    AWBURST_i  ,
    input  logic                           AWLOCK_i   ,
    input  logic [ 3:0]                    AWCACHE_i  ,
    input  logic [ 2:0]                    AWPROT_i   ,
    input  logic [ 3:0]                    AWREGION_i ,
    input  logic [ AXI4_USER_WIDTH-1:0]    AWUSER_i   ,
    input  logic [ 3:0]                    AWQOS_i    ,
    input  logic                           AWVALID_i  ,
    output logic                           AWREADY_o  ,
    input  logic [AXI_NUMBYTES-1:0][7:0]   WDATA_i    ,
    input  logic [AXI_NUMBYTES-1:0]        WSTRB_i    ,
    input  logic                           WLAST_i    ,
    input  logic [AXI4_USER_WIDTH-1:0]     WUSER_i    ,
    input  logic                           WVALID_i   ,
    output logic                           WREADY_o   ,
    output logic   [AXI4_ID_WIDTH-1:0]     BID_o      ,
    output logic   [ 1:0]                  BRESP_o    ,
    output logic                           BVALID_o   ,
    output logic   [AXI4_USER_WIDTH-1:0]   BUSER_o    ,
    input  logic                           BREADY_i   ,
    input  logic [AXI4_ID_WIDTH-1:0]       ARID_i     ,
    input  logic [AXI4_ADDRESS_WIDTH-1:0]  ARADDR_i   ,
    input  logic [ 7:0]                    ARLEN_i    ,
    input  logic [ 2:0]                    ARSIZE_i   ,
    input  logic [ 1:0]                    ARBURST_i  ,
    input  logic                           ARLOCK_i   ,
    input  logic [ 3:0]                    ARCACHE_i  ,
    input  logic [ 2:0]                    ARPROT_i   ,
    input  logic [ 3:0]                    ARREGION_i ,
    input  logic [ AXI4_USER_WIDTH-1:0]    ARUSER_i   ,
    input  logic [ 3:0]                    ARQOS_i    ,
    input  logic                           ARVALID_i  ,
    output logic                           ARREADY_o  ,
    output  logic [AXI4_ID_WIDTH-1:0]      RID_o      ,
    output  logic [AXI4_RDATA_WIDTH-1:0]   RDATA_o    ,
    output  logic [ 1:0]                   RRESP_o    ,
    output  logic                          RLAST_o    ,
    output  logic [AXI4_USER_WIDTH-1:0]    RUSER_o    ,
    output  logic                          RVALID_o   ,
    input   logic                          RREADY_i   ,
    output logic                           PENABLE    ,
    output logic                           PWRITE     ,
    output logic [APB_ADDR_WIDTH-1:0]      PADDR      ,
    output logic                           PSEL       ,
    output logic [31:0]                    PWDATA     ,
    input  logic [31:0]                    PRDATA     ,
    input  logic                           PREADY     ,
    input  logic                           PSLVERR
);
    logic [AXI4_ID_WIDTH-1:0]      AWID;
    logic [AXI4_ADDRESS_WIDTH-1:0] AWADDR;
    logic [ 7:0]                   AWLEN;
    logic [ 2:0]                   AWSIZE;
    logic [ 1:0]                   AWBURST;
    logic                          AWLOCK;
    logic [ 3:0]                   AWCACHE;
    logic [ 2:0]                   AWPROT;
    logic [ 3:0]                   AWREGION;
    logic [ AXI4_USER_WIDTH-1:0]   AWUSER;
    logic [ 3:0]                   AWQOS;
    logic                          AWVALID;
    logic                          AWREADY;
    logic [1:0][31:0]              WDATA;   
    logic [AXI_NUMBYTES-1:0]       WSTRB;   
    logic                          WLAST;   
    logic [AXI4_USER_WIDTH-1:0]    WUSER;   
    logic                          WVALID;  
    logic                          WREADY;  
    logic [AXI4_ID_WIDTH-1:0]      BID;
    logic [ 1:0]                   BRESP;
    logic                          BVALID;
    logic [AXI4_USER_WIDTH-1:0]    BUSER;
    logic                          BREADY;
    logic [AXI4_ID_WIDTH-1:0]      ARID;
    logic [AXI4_ADDRESS_WIDTH-1:0] ARADDR;
    logic [ 7:0]                   ARLEN;
    logic [ 2:0]                   ARSIZE;
    logic [ 1:0]                   ARBURST;
    logic                          ARLOCK;
    logic [ 3:0]                   ARCACHE;
    logic [ 2:0]                   ARPROT;
    logic [ 3:0]                   ARREGION;
    logic [ AXI4_USER_WIDTH-1:0]   ARUSER;
    logic [ 3:0]                   ARQOS;
    logic                          ARVALID;
    logic                          ARREADY;
    logic [AXI4_ID_WIDTH-1:0]    RID;
    logic [1:0][31:0]            RDATA;
    logic [ 1:0]                 RRESP;
    logic                        RLAST;
    logic [AXI4_USER_WIDTH-1:0]  RUSER;
    logic                        RVALID;
    logic                        RREADY;
    enum logic [3:0] { IDLE,
                       SINGLE_RD, SINGLE_RD_64,
                       BURST_RD_1, BURST_RD, BURST_RD_64,
                       BURST_WR, BURST_WR_64,
                       SINGLE_WR,SINGLE_WR_64,
                       WAIT_R_PREADY, WAIT_W_PREADY
                      } CS, NS;
    logic        W_word_sel;
    logic [APB_ADDR_WIDTH-1:0] address;
    logic        read_req;
    logic        write_req;
    logic        sample_AR;
    logic [8:0]  ARLEN_Q;
    logic        decr_ARLEN;
    logic        sample_AW;
    logic [8:0]  AWLEN_Q;
    logic        decr_AWLEN;
    logic [AXI4_ADDRESS_WIDTH-1:0] ARADDR_Q;
    logic                          incr_ARADDR;
    logic [AXI4_ADDRESS_WIDTH-1:0] AWADDR_Q;
    logic                          incr_AWADDR;
    logic        sample_RDATA_0;  
    logic        sample_RDATA_1;  
    logic [31:0] RDATA_Q_0;
    logic [31:0] RDATA_Q_1;
    assign PENABLE = write_req | read_req;
    assign PWRITE  = write_req;
    assign PADDR   = address[APB_ADDR_WIDTH-1:0];
    assign PWDATA  = WDATA[W_word_sel];
    assign PSEL    = 1'b1;
    axi_aw_buffer #(
        .ID_WIDTH     ( AXI4_ID_WIDTH      ),
        .ADDR_WIDTH   ( AXI4_ADDRESS_WIDTH ),
        .USER_WIDTH   ( AXI4_USER_WIDTH    ),
        .BUFFER_DEPTH ( BUFF_DEPTH_SLAVE   )
    ) slave_aw_buffer_i (
       .clk_i           ( ACLK        ),
       .rst_ni          ( ARESETn     ),
       .test_en_i       ( test_en_i   ),
       .slave_valid_i   ( AWVALID_i   ),
       .slave_addr_i    ( AWADDR_i    ),
       .slave_prot_i    ( AWPROT_i    ),
       .slave_region_i  ( AWREGION_i  ),
       .slave_len_i     ( AWLEN_i     ),
       .slave_size_i    ( AWSIZE_i    ),
       .slave_burst_i   ( AWBURST_i   ),
       .slave_lock_i    ( AWLOCK_i    ),
       .slave_cache_i   ( AWCACHE_i   ),
       .slave_qos_i     ( AWQOS_i     ),
       .slave_id_i      ( AWID_i      ),
       .slave_user_i    ( AWUSER_i    ),
       .slave_ready_o   ( AWREADY_o   ),
       .master_valid_o  ( AWVALID     ),
       .master_addr_o   ( AWADDR      ),
       .master_prot_o   ( AWPROT      ),
       .master_region_o ( AWREGION    ),
       .master_len_o    ( AWLEN       ),
       .master_size_o   ( AWSIZE      ),
       .master_burst_o  ( AWBURST     ),
       .master_lock_o   ( AWLOCK      ),
       .master_cache_o  ( AWCACHE     ),
       .master_qos_o    ( AWQOS       ),
       .master_id_o     ( AWID        ),
       .master_user_o   ( AWUSER      ),
       .master_ready_i  ( AWREADY     )
    );
    axi_ar_buffer #(
        .ID_WIDTH       ( AXI4_ID_WIDTH      ),
        .ADDR_WIDTH     ( AXI4_ADDRESS_WIDTH ),
        .USER_WIDTH     ( AXI4_USER_WIDTH    ),
        .BUFFER_DEPTH   ( BUFF_DEPTH_SLAVE   )
    ) slave_ar_buffer_i (
       .clk_i           ( ACLK       ),
       .rst_ni          ( ARESETn    ),
       .test_en_i       ( test_en_i  ),
       .slave_valid_i   ( ARVALID_i  ),
       .slave_addr_i    ( ARADDR_i   ),
       .slave_prot_i    ( ARPROT_i   ),
       .slave_region_i  ( ARREGION_i ),
       .slave_len_i     ( ARLEN_i    ),
       .slave_size_i    ( ARSIZE_i   ),
       .slave_burst_i   ( ARBURST_i  ),
       .slave_lock_i    ( ARLOCK_i   ),
       .slave_cache_i   ( ARCACHE_i  ),
       .slave_qos_i     ( ARQOS_i    ),
       .slave_id_i      ( ARID_i     ),
       .slave_user_i    ( ARUSER_i   ),
       .slave_ready_o   ( ARREADY_o  ),
       .master_valid_o  ( ARVALID    ),
       .master_addr_o   ( ARADDR     ),
       .master_prot_o   ( ARPROT     ),
       .master_region_o ( ARREGION   ),
       .master_len_o    ( ARLEN      ),
       .master_size_o   ( ARSIZE     ),
       .master_burst_o  ( ARBURST    ),
       .master_lock_o   ( ARLOCK     ),
       .master_cache_o  ( ARCACHE    ),
       .master_qos_o    ( ARQOS      ),
       .master_id_o     ( ARID       ),
       .master_user_o   ( ARUSER     ),
       .master_ready_i  ( ARREADY    )
    );
    axi_w_buffer #(
        .DATA_WIDTH   ( AXI4_WDATA_WIDTH ),
        .USER_WIDTH   ( AXI4_USER_WIDTH  ),
        .BUFFER_DEPTH ( BUFF_DEPTH_SLAVE )
    ) slave_w_buffer_i (
         .clk_i          ( ACLK      ),
         .rst_ni         ( ARESETn   ),
         .test_en_i      ( test_en_i ),
         .slave_valid_i  ( WVALID_i  ),
         .slave_data_i   ( WDATA_i   ),
         .slave_strb_i   ( WSTRB_i   ),
         .slave_user_i   ( WUSER_i   ),
         .slave_last_i   ( WLAST_i   ),
         .slave_ready_o  ( WREADY_o  ),
         .master_valid_o ( WVALID    ),
         .master_data_o  ( WDATA     ),
         .master_strb_o  ( WSTRB     ),
         .master_user_o  ( WUSER     ),
         .master_last_o  ( WLAST     ),
         .master_ready_i ( WREADY    )
    );
    axi_r_buffer #(
         .ID_WIDTH     ( AXI4_ID_WIDTH    ),
         .DATA_WIDTH   ( AXI4_RDATA_WIDTH ),
         .USER_WIDTH   ( AXI4_USER_WIDTH  ),
         .BUFFER_DEPTH ( BUFF_DEPTH_SLAVE )
    ) slave_r_buffer_i (
         .clk_i          ( ACLK       ),
         .rst_ni         ( ARESETn    ),
         .test_en_i      ( test_en_i  ),
         .slave_valid_i  ( RVALID     ),
         .slave_data_i   ( RDATA      ),
         .slave_resp_i   ( RRESP      ),
         .slave_user_i   ( RUSER      ),
         .slave_id_i     ( RID        ),
         .slave_last_i   ( RLAST      ),
         .slave_ready_o  ( RREADY     ),
         .master_valid_o ( RVALID_o   ),
         .master_data_o  ( RDATA_o    ),
         .master_resp_o  ( RRESP_o    ),
         .master_user_o  ( RUSER_o    ),
         .master_id_o    ( RID_o      ),
         .master_last_o  ( RLAST_o    ),
         .master_ready_i ( RREADY_i   )
    );
    axi_b_buffer #(
        .ID_WIDTH       ( AXI4_ID_WIDTH    ),
        .USER_WIDTH     ( AXI4_USER_WIDTH  ),
        .BUFFER_DEPTH   ( BUFF_DEPTH_SLAVE )
    ) slave_b_buffer_i (
        .clk_i          ( ACLK      ),
        .rst_ni         ( ARESETn   ),
        .test_en_i      ( test_en_i ),
        .slave_valid_i  ( BVALID    ),
        .slave_resp_i   ( BRESP     ),
        .slave_id_i     ( BID       ),
        .slave_user_i   ( BUSER     ),
        .slave_ready_o  ( BREADY    ),
        .master_valid_o ( BVALID_o  ),
        .master_resp_o  ( BRESP_o   ),
        .master_id_o    ( BID_o     ),
        .master_user_o  ( BUSER_o   ),
        .master_ready_i ( BREADY_i  )
    );
    always_comb begin
        read_req   = 1'b0;
        write_req  = 1'b0;
        W_word_sel = 1'b0;  
        sample_AW  = 1'b0;
        decr_AWLEN = 1'b0;
        sample_AR  = 1'b0;
        decr_ARLEN = 1'b0;
        incr_AWADDR = 1'b0;
        incr_ARADDR = 1'b0;
        sample_RDATA_0 = 1'b0;
        sample_RDATA_1 = 1'b0;
        ARREADY = 1'b0;
        AWREADY = 1'b0;
        WREADY  = 1'b0;
        RDATA   = '0;
        BVALID = 1'b0;
        BRESP  = 2'b00;
        BID    = AWID;
        BUSER  = AWUSER;
        RVALID = 1'b0;
        RLAST  = 1'b0;
        RID    = ARID;
        RUSER  = ARUSER;
        RRESP  = 2'b00;
        case(CS)
            WAIT_R_PREADY: begin
                sample_AR = 1'b0;
                read_req  = 1'b1;
                address   = ARADDR;
                if (PREADY == 1'b1) begin 
                    if (ARLEN == 0) begin
                        case (ARSIZE)
                            3'h3: begin
                                NS = SINGLE_RD_64;
                                if (ARADDR[2:0] == 3'h4)
                                    sample_RDATA_1 = 1'b1;
                                else  sample_RDATA_0 = 1'b1;
                            end
                            default: begin
                                NS = SINGLE_RD;
                                if (ARADDR[2:0] == 3'h4)
                                    sample_RDATA_1 = 1'b1;
                                else
                                    sample_RDATA_0 = 1'b1;
                                end
                            endcase
                    end else begin  
                       NS             = BURST_RD_64;
                       sample_RDATA_0 = 1'b1;
                       decr_ARLEN     = 1'b1;
                       incr_ARADDR    = 1'b1;
                    end
                end else begin  
                    NS = WAIT_R_PREADY;
                end
            end
            WAIT_W_PREADY: begin
                address   = AWADDR;
                write_req = 1'b1;
                if (AWADDR[2:0] == 3'h4)
                    W_word_sel = 1'b1;
                else
                    W_word_sel = 1'b0;
                if (PREADY == 1'b1) begin  
                    if (AWLEN == 0) begin  
                        case (AWSIZE)
                            3'h3: NS = SINGLE_WR_64;
                            default: NS = SINGLE_WR;
                        endcase
                    end else begin  
                        sample_AW = 1'b1;
                        NS        = BURST_WR_64;
                    end
                end else begin  
                    NS = WAIT_W_PREADY;
                end
            end
            IDLE: begin
                if (ARVALID == 1'b1)  begin
                    sample_AR = 1'b1;
                    read_req  = 1'b1;
                    address   = ARADDR;
                    if (PREADY == 1'b1) begin  
                        if (ARLEN == 0) begin
                            case (ARSIZE)
                                3'h3: begin
                                    NS = SINGLE_RD_64;
                                    if (ARADDR[2:0] == 4)
                                        sample_RDATA_1 = 1'b1;
                                    else
                                        sample_RDATA_0 = 1'b1;
                                end
                                default: begin
                                    NS = SINGLE_RD;
                                    if (ARADDR[2:0] == 4)
                                        sample_RDATA_1 = 1'b1;
                                    else
                                        sample_RDATA_0 = 1'b1;
                                    end
                            endcase end else begin  
                            NS             = BURST_RD_64;
                            sample_RDATA_0 = 1'b1;
                        end
                    end else begin  
                        NS = WAIT_R_PREADY;
                    end
                end else begin
                    if (AWVALID) begin  
                        if (WVALID) begin  
                            write_req = 1'b1;
                            address   = AWADDR;
                            if (AWADDR[2:0] == 3'h4)
                                W_word_sel = 1'b1;
                            else
                                W_word_sel = 1'b0;
                            if (PREADY == 1'b1) begin 
                                  if(AWLEN == 0) begin  
                                        case(AWSIZE)
                                            3'h3: NS = SINGLE_WR_64;
                                            default: NS = SINGLE_WR;
                                        endcase
                                  end else begin  
                                        sample_AW   = 1'b1;
                                        if ((AWADDR[2:0] == 3'h4) && (WSTRB[7:4] == 0))
                                          incr_AWADDR = 1'b0;
                                        else
                                          incr_AWADDR = 1'b1;
                                        NS = BURST_WR_64;
                                  end
                            end else begin 
                                NS = WAIT_W_PREADY;
                            end
                        end else begin  
                            write_req = 1'b0;
                            address   = '0;
                            NS        = IDLE;
                        end
                    end else begin 
                        NS = IDLE;
                        address =  '0;
                    end
                end
            end
            SINGLE_WR_64: begin
                address    = AWADDR + 4;
                W_word_sel = 1'b1;  
                write_req  = WVALID;
                if (WVALID) begin
                    if (PREADY == 1'b1)
                        NS = SINGLE_WR;
                    else
                        NS = SINGLE_WR_64;
                end else begin
                    NS = SINGLE_WR_64;
                end
            end
            SINGLE_WR:  begin
                BVALID   = 1'b1;
                address  = '0;
                if (BREADY)  begin
                    NS      = IDLE;
                    AWREADY = 1'b1;
                    WREADY  = 1'b1;
                end else begin
                    NS = SINGLE_WR;
                end
            end
            BURST_WR_64: begin
                W_word_sel = 1'b1;  
                write_req  = WVALID & (|WSTRB[7:4]);
                address    = AWADDR_Q;  
                if (WVALID) begin
                    if (&WSTRB[7:4]) begin
                        if(PREADY == 1'b1) begin
                            NS          = BURST_WR;
                            WREADY      = 1'b1;  
                            decr_AWLEN  = 1'b1;  
                            incr_AWADDR = 1'b1;  
                        end else begin
                            NS = BURST_WR_64;
                        end
                    end else begin
                        NS = BURST_WR;
                        WREADY      = 1'b1;  
                        decr_AWLEN  = 1'b1;  
                        incr_AWADDR = 1'b1;  
                    end
                end else begin
                    NS = BURST_WR_64;
                end
            end
            BURST_WR: begin
                address = AWADDR_Q;  
                if (AWLEN_Q == 0) begin  
                    BVALID = 1'b1;
                    if (BREADY) begin
                      NS      = IDLE;
                      AWREADY = 1'b1;
                    end else
                      NS = BURST_WR;
                end else begin  
                    W_word_sel = 1'b0;  
                    write_req  = WVALID & (&WSTRB[3:0]);
                    if (WVALID) begin
                        if (PREADY == 1'b1) begin
                            NS          = BURST_WR_64;
                            incr_AWADDR = 1'b1;
                            decr_AWLEN  = 1'b1;  
                          end else
                            NS = BURST_WR;
                    end else begin
                        NS = BURST_WR_64;
                    end
              end
            end
            BURST_RD_64: begin
               read_req = 1'b1;
               address  = ARADDR_Q;
                if (ARLEN_Q == 0) begin  
                    NS      = IDLE;
                    ARREADY = 1'b1;
                end else begin
                    if (PREADY == 1'b1) begin  
                        decr_ARLEN     = 1'b1;
                        sample_RDATA_1 = 1'b1;
                        NS = BURST_RD;
                        if (ARADDR_Q[2:0] == 3'h4)
                          incr_ARADDR = 1'b1;
                        else
                          incr_ARADDR = 1'b0;
                      end
                    else  begin
                        NS = BURST_RD_64;
                    end
                 end
            end
            BURST_RD: begin
                RVALID   = 1'b1;
                RDATA[0] = RDATA_Q_0;
                RDATA[1] = RDATA_Q_1;
                RLAST    = (ARLEN_Q == 0) ? 1'b1 : 1'b0;
                address  = ARADDR_Q;
                if (RREADY) begin  
                    if (ARLEN_Q == 0) begin  
                        NS      = IDLE;
                        ARREADY = 1'b1;
                    end else begin  
                        read_req = 1'b1;
                        if (PREADY == 1'b1) begin  
                            sample_RDATA_0 = 1'b1;
                            NS             = BURST_RD_64;
                            incr_ARADDR    = 1'b1;
                            decr_ARLEN     = 1'b1;
                        end else begin
                            NS = BURST_RD_1;
                        end
                    end
                end else begin  
                    NS = BURST_RD;
                end
            end
            BURST_RD_1: begin
                read_req = 1'b1;
                address  = ARADDR_Q;
                if (PREADY == 1'b1) begin  
                    sample_RDATA_0 = 1'b1;
                    NS             = BURST_RD_64;
                    incr_ARADDR    = 1'b1;
                    decr_ARLEN     = 1'b1;
                end else begin
                    NS = BURST_RD_1;
                end
            end
            SINGLE_RD: begin
                RVALID   = 1'b1;
                RDATA[0] = RDATA_Q_0;
                RDATA[1] = RDATA_Q_1;
                RLAST    = 1;
                address  = '0;
                if (RREADY) begin  
                    NS      = IDLE;
                    ARREADY = 1'b1;
                end else begin  
                    NS = SINGLE_RD;
                end
            end
            SINGLE_RD_64: begin
                read_req       = 1'b1;
                address        = ARADDR + 4;
                if (PREADY == 1'b1) begin  
                    NS = SINGLE_RD;
                    if(ARADDR[2:0] == 3'h4)
                        sample_RDATA_0 = 1'b1;
                    else
                        sample_RDATA_1 = 1'b1;
                end else begin
                  NS = SINGLE_RD_64;
                end
            end
            default: begin
                NS      = IDLE;
                address = '0;
            end
        endcase
    end
    always_ff @(posedge ACLK, negedge ARESETn) begin
        if (ARESETn == 1'b0) begin
            CS        <= IDLE;
            ARLEN_Q   <= '0;
            AWADDR_Q  <= '0;
            AWLEN_Q   <= '0;
            RDATA_Q_0 <= '0;
            RDATA_Q_1 <= '0;
            ARADDR_Q  <= '0;
        end else  begin
            CS <= NS;
            if (sample_AR) begin
                ARLEN_Q <= {ARLEN,1'b0} + 2;
            end else if (decr_ARLEN) begin
                ARLEN_Q <= ARLEN_Q - 1;
            end
            if (sample_RDATA_0)
                RDATA_Q_0 <= PRDATA;
            if (sample_RDATA_1)
                RDATA_Q_1 <= PRDATA;
            case ({sample_AW, decr_AWLEN})
                2'b00: AWLEN_Q <= AWLEN_Q;
                2'b01: AWLEN_Q <= AWLEN_Q - 1;
                2'b10: AWLEN_Q <= {AWLEN, 1'b0} + 1;
                2'b11: AWLEN_Q <= {AWLEN, 1'b0};
            endcase
            case ({sample_AW, incr_AWADDR})
                2'b00: AWADDR_Q <= AWADDR_Q;
                2'b01: AWADDR_Q <= AWADDR_Q + 4;
                2'b10: AWADDR_Q <= {AWADDR[AXI4_ADDRESS_WIDTH-1:3], 3'b000};
                2'b11: AWADDR_Q <= {AWADDR[AXI4_ADDRESS_WIDTH-1:3], 3'b000} + 4;
            endcase
            case({sample_AR, incr_ARADDR})
                2'b00: ARADDR_Q <= ARADDR_Q;
                2'b01: ARADDR_Q <= ARADDR_Q + 4;
                2'b10: ARADDR_Q <= {ARADDR[AXI4_ADDRESS_WIDTH-1:3], 3'b000};
                2'b11: ARADDR_Q <= {ARADDR[AXI4_ADDRESS_WIDTH-1:3], 3'b000} + 4;
            endcase
        end
    end
endmodule
module axi_ar_buffer #(
    parameter int ID_WIDTH     = -1,
    parameter int ADDR_WIDTH   = -1,
    parameter int USER_WIDTH   = -1,
    parameter int BUFFER_DEPTH = -1
)(
    input logic                   clk_i,
    input logic                   rst_ni,
    input logic                   test_en_i,
    input  logic                  slave_valid_i,
    input  logic [ADDR_WIDTH-1:0] slave_addr_i,
    input  logic [2:0]            slave_prot_i,
    input  logic [3:0]            slave_region_i,
    input  logic [7:0]            slave_len_i,
    input  logic [2:0]            slave_size_i,
    input  logic [1:0]            slave_burst_i,
    input  logic                  slave_lock_i,
    input  logic [3:0]            slave_cache_i,
    input  logic [3:0]            slave_qos_i,
    input  logic [ID_WIDTH-1:0]   slave_id_i,
    input  logic [USER_WIDTH-1:0] slave_user_i,
    output logic                  slave_ready_o,
    output logic                  master_valid_o,
    output logic [ADDR_WIDTH-1:0] master_addr_o,
    output logic [2:0]            master_prot_o,
    output logic [3:0]            master_region_o,
    output logic [7:0]            master_len_o,
    output logic [2:0]            master_size_o,
    output logic [1:0]            master_burst_o,
    output logic                  master_lock_o,
    output logic [3:0]            master_cache_o,
    output logic [3:0]            master_qos_o,
    output logic [ID_WIDTH-1:0]   master_id_o,
    output logic [USER_WIDTH-1:0] master_user_o,
    input  logic                  master_ready_i
);
   logic [29+ADDR_WIDTH+USER_WIDTH+ID_WIDTH-1:0] s_data_in;
   logic [29+ADDR_WIDTH+USER_WIDTH+ID_WIDTH-1:0] s_data_out;
   assign s_data_in = {slave_cache_i,  slave_prot_i,  slave_lock_i,  slave_burst_i,  slave_size_i,  slave_len_i,  slave_qos_i,  slave_region_i,  slave_addr_i,  slave_user_i,  slave_id_i} ;
   assign             {master_cache_o, master_prot_o, master_lock_o, master_burst_o, master_size_o, master_len_o, master_qos_o, master_region_o, master_addr_o, master_user_o, master_id_o} =  s_data_out;
  axi_single_slice #(.BUFFER_DEPTH(BUFFER_DEPTH), .DATA_WIDTH(29+ADDR_WIDTH+USER_WIDTH+ID_WIDTH)) i_axi_single_slice (
    .clk_i      ( clk_i          ),
    .rst_ni     ( rst_ni         ),
    .testmode_i ( test_en_i      ),
    .valid_i    ( slave_valid_i  ),
    .ready_o    ( slave_ready_o  ),
    .data_i     ( s_data_in      ),
    .ready_i    ( master_ready_i ),
    .valid_o    ( master_valid_o ),
    .data_o     ( s_data_out     )
  );
endmodule
module axi_aw_buffer #(
    parameter int ID_WIDTH     = -1,
    parameter int ADDR_WIDTH   = -1,
    parameter int USER_WIDTH   = -1,
    parameter int BUFFER_DEPTH = -1
)(
    input logic                   clk_i,
    input logic                   rst_ni,
    input logic                   test_en_i,
    input  logic                  slave_valid_i,
    input  logic [ADDR_WIDTH-1:0] slave_addr_i,
    input  logic [2:0]            slave_prot_i,
    input  logic [3:0]            slave_region_i,
    input  logic [7:0]            slave_len_i,
    input  logic [2:0]            slave_size_i,
    input  logic [1:0]            slave_burst_i,
    input  logic                  slave_lock_i,
    input  logic [3:0]            slave_cache_i,
    input  logic [3:0]            slave_qos_i,
    input  logic [ID_WIDTH-1:0]   slave_id_i,
    input  logic [USER_WIDTH-1:0] slave_user_i,
    output logic                  slave_ready_o,
    output logic                  master_valid_o,
    output logic [ADDR_WIDTH-1:0] master_addr_o,
    output logic [2:0]            master_prot_o,
    output logic [3:0]            master_region_o,
    output logic [7:0]            master_len_o,
    output logic [2:0]            master_size_o,
    output logic [1:0]            master_burst_o,
    output logic                  master_lock_o,
    output logic [3:0]            master_cache_o,
    output logic [3:0]            master_qos_o,
    output logic [ID_WIDTH-1:0]   master_id_o,
    output logic [USER_WIDTH-1:0] master_user_o,
    input  logic                  master_ready_i
);
   logic [29+ADDR_WIDTH+USER_WIDTH+ID_WIDTH-1:0] s_data_in;
   logic [29+ADDR_WIDTH+USER_WIDTH+ID_WIDTH-1:0] s_data_out;
   assign s_data_in = {slave_cache_i,  slave_prot_i,  slave_lock_i,  slave_burst_i,  slave_size_i,  slave_len_i,  slave_qos_i,  slave_region_i,  slave_addr_i,  slave_user_i,  slave_id_i};
   assign             {master_cache_o, master_prot_o, master_lock_o, master_burst_o, master_size_o, master_len_o, master_qos_o, master_region_o, master_addr_o, master_user_o, master_id_o} = s_data_out;
    axi_single_slice #(.BUFFER_DEPTH(BUFFER_DEPTH), .DATA_WIDTH(29+ADDR_WIDTH+USER_WIDTH+ID_WIDTH)) i_axi_single_slice (
      .clk_i      ( clk_i          ),
      .rst_ni     ( rst_ni         ),
      .testmode_i ( test_en_i      ),
      .valid_i    ( slave_valid_i  ),
      .ready_o    ( slave_ready_o  ),
      .data_i     ( s_data_in      ),
      .ready_i    ( master_ready_i ),
      .valid_o    ( master_valid_o ),
      .data_o     ( s_data_out     )
    );
endmodule
module axi_b_buffer #(
    parameter int ID_WIDTH     = -1,
    parameter int USER_WIDTH   = -1,
    parameter int BUFFER_DEPTH = -1
)(
   input logic                   clk_i,
   input logic                   rst_ni,
   input logic                   test_en_i,
   input logic                   slave_valid_i,
   input logic  [1:0]            slave_resp_i,
   input logic  [ID_WIDTH-1:0]   slave_id_i,
   input logic  [USER_WIDTH-1:0] slave_user_i,
   output logic                  slave_ready_o,
   output logic                  master_valid_o,
   output logic [1:0]            master_resp_o,
   output logic [ID_WIDTH-1:0]   master_id_o,
   output logic [USER_WIDTH-1:0] master_user_o,
   input  logic                  master_ready_i
);
    logic [2+USER_WIDTH+ID_WIDTH-1:0] s_data_in;
    logic [2+USER_WIDTH+ID_WIDTH-1:0] s_data_out;
    assign s_data_in = {slave_id_i,  slave_user_i,  slave_resp_i};
    assign             {master_id_o, master_user_o, master_resp_o} = s_data_out;
    axi_single_slice #(.BUFFER_DEPTH(BUFFER_DEPTH), .DATA_WIDTH(2+USER_WIDTH+ID_WIDTH)) i_axi_single_slice (
      .clk_i      ( clk_i          ),
      .rst_ni     ( rst_ni         ),
      .testmode_i ( test_en_i      ),
      .valid_i    ( slave_valid_i  ),
      .ready_o    ( slave_ready_o  ),
      .data_i     ( s_data_in      ),
      .ready_i    ( master_ready_i ),
      .valid_o    ( master_valid_o ),
      .data_o     ( s_data_out     )
    );
endmodule
module axi_r_buffer #(
   parameter ID_WIDTH      = 4,
   parameter DATA_WIDTH    = 64,
   parameter USER_WIDTH    = 6,
   parameter BUFFER_DEPTH  = 8,
   parameter STRB_WIDTH    = DATA_WIDTH/8    
)(
   input logic                   clk_i,
   input logic                   rst_ni,
   input logic                   test_en_i,
   input logic                   slave_valid_i,
   input logic  [DATA_WIDTH-1:0] slave_data_i,
   input logic  [1:0]            slave_resp_i,
   input logic  [USER_WIDTH-1:0] slave_user_i,
   input logic  [ID_WIDTH-1:0]   slave_id_i,
   input logic                   slave_last_i,
   output logic                  slave_ready_o,
   output logic                  master_valid_o,
   output logic [DATA_WIDTH-1:0] master_data_o,
   output logic [1:0]            master_resp_o,
   output logic [USER_WIDTH-1:0] master_user_o,
   output logic [ID_WIDTH-1:0]   master_id_o,
   output logic                  master_last_o,
   input  logic                  master_ready_i
);
   logic [2+DATA_WIDTH+USER_WIDTH+ID_WIDTH:0] s_data_in;
   logic [2+DATA_WIDTH+USER_WIDTH+ID_WIDTH:0] s_data_out;
   assign s_data_in =  {slave_id_i,  slave_user_i,  slave_data_i,  slave_resp_i,  slave_last_i};
   assign              {master_id_o, master_user_o, master_data_o, master_resp_o, master_last_o} = s_data_out;
   axi_single_slice #(.BUFFER_DEPTH(BUFFER_DEPTH), .DATA_WIDTH(3+DATA_WIDTH+USER_WIDTH+ID_WIDTH)) i_axi_single_slice (
     .clk_i      ( clk_i          ),
     .rst_ni     ( rst_ni         ),
     .testmode_i ( test_en_i      ),
     .valid_i    ( slave_valid_i  ),
     .ready_o    ( slave_ready_o  ),
     .data_i     ( s_data_in      ),
     .ready_i    ( master_ready_i ),
     .valid_o    ( master_valid_o ),
     .data_o     ( s_data_out     )
   );
endmodule
module axi_single_slice #(
    parameter int BUFFER_DEPTH = -1,
    parameter int DATA_WIDTH   = -1
) (
    input  logic                  clk_i,     
    input  logic                  rst_ni,   
    input  logic                  testmode_i,
    input  logic                  valid_i,
    output logic                  ready_o,
    input  logic [DATA_WIDTH-1:0] data_i,
    input  logic                  ready_i,
    output logic                  valid_o,
    output logic [DATA_WIDTH-1:0] data_o
);
    logic full, empty;
    assign ready_o = ~full;
    assign valid_o = ~empty;
    fifo #(
        .FALL_THROUGH ( 1'b0         ),
        .DATA_WIDTH   ( DATA_WIDTH   ),
        .DEPTH        ( BUFFER_DEPTH )
    ) i_fifo (
        .clk_i      ( clk_i             ),
        .rst_ni     ( rst_ni            ),
        .flush_i    ( 1'b0              ),
        .threshold_o (),  
        .testmode_i ( testmode_i        ),
        .full_o     ( full              ),
        .empty_o    ( empty             ),
        .data_i     ( data_i            ),
        .push_i     ( valid_i & ready_o ),
        .data_o     ( data_o            ),
        .pop_i      ( ready_i & valid_o )
    );
endmodule
module axi_w_buffer #(
    parameter int DATA_WIDTH   = -1,
    parameter int USER_WIDTH   = -1,
    parameter int BUFFER_DEPTH = -1,
    parameter int STRB_WIDTH   = DATA_WIDTH/8    
)(
    input logic                   clk_i,
    input logic                   rst_ni,
    input logic                   test_en_i,
    input logic                   slave_valid_i,
    input logic  [DATA_WIDTH-1:0] slave_data_i,
    input logic  [STRB_WIDTH-1:0] slave_strb_i,
    input logic  [USER_WIDTH-1:0] slave_user_i,
    input logic                   slave_last_i,
    output logic                  slave_ready_o,
    output logic                  master_valid_o,
    output logic [DATA_WIDTH-1:0] master_data_o,
    output logic [STRB_WIDTH-1:0] master_strb_o,
    output logic [USER_WIDTH-1:0] master_user_o,
    output logic                  master_last_o,
    input  logic                  master_ready_i
);
    logic [DATA_WIDTH+STRB_WIDTH+USER_WIDTH:0] s_data_in;
    logic [DATA_WIDTH+STRB_WIDTH+USER_WIDTH:0] s_data_out;
    assign s_data_in = { slave_user_i,  slave_strb_i,  slave_data_i,  slave_last_i  };
    assign             { master_user_o, master_strb_o, master_data_o, master_last_o } = s_data_out;
    axi_single_slice #(.BUFFER_DEPTH(BUFFER_DEPTH), .DATA_WIDTH(1+DATA_WIDTH+STRB_WIDTH+USER_WIDTH)) i_axi_single_slice (
      .clk_i      ( clk_i          ),
      .rst_ni     ( rst_ni         ),
      .testmode_i ( test_en_i      ),
      .valid_i    ( slave_valid_i  ),
      .ready_o    ( slave_ready_o  ),
      .data_i     ( s_data_in      ),
      .ready_i    ( master_ready_i ),
      .valid_o    ( master_valid_o ),
      .data_o     ( s_data_out     )
    );
endmodule
module SimDTM(
  input clk,
  input reset,
  output        debug_req_valid,
  input         debug_req_ready,
  output [ 6:0] debug_req_bits_addr,
  output [ 1:0] debug_req_bits_op,
  output [31:0] debug_req_bits_data,
  input         debug_resp_valid,
  output        debug_resp_ready,
  input  [ 1:0] debug_resp_bits_resp,
  input  [31:0] debug_resp_bits_data,
  output [31:0] exit
);
  bit r_reset;
  wire #0.1 __debug_req_ready = debug_req_ready;
  wire #0.1 __debug_resp_valid = debug_resp_valid;
  wire [31:0] #0.1 __debug_resp_bits_resp = {30'b0, debug_resp_bits_resp};
  wire [31:0] #0.1 __debug_resp_bits_data = debug_resp_bits_data;
  bit __debug_req_valid;
  int __debug_req_bits_addr;
  int __debug_req_bits_op;
  int __debug_req_bits_data;
  bit __debug_resp_ready;
  int __exit;
  assign #0.1 debug_req_valid = __debug_req_valid;
  assign #0.1 debug_req_bits_addr = __debug_req_bits_addr[6:0];
  assign #0.1 debug_req_bits_op = __debug_req_bits_op[1:0];
  assign #0.1 debug_req_bits_data = __debug_req_bits_data[31:0];
  assign #0.1 debug_resp_ready = __debug_resp_ready;
  assign #0.1 exit = __exit;
  always @(posedge clk)
  begin
    r_reset <= reset;
    if (reset || r_reset)
    begin
      __debug_req_valid = 0;
      __debug_resp_ready = 0;
      __exit = 0;
    end
    else
    begin
    end
  end
endmodule
module SimJTAG #(
                 parameter TICK_DELAY = 50
                 )(
                   input         clock,
                   input         reset,
                   input         enable,
                   input         init_done,
                   output        jtag_TCK,
                   output        jtag_TMS,
                   output        jtag_TDI,
                   output        jtag_TRSTn,
                   input         jtag_TDO_data,
                   input         jtag_TDO_driven,
                   output [31:0] exit
                   );
   reg [31:0]                    tickCounterReg;
   wire [31:0]                   tickCounterNxt;
   assign tickCounterNxt = (tickCounterReg == 0) ? TICK_DELAY :  (tickCounterReg - 1);
   bit          r_reset;
   wire         #0.1 __jtag_TDO = jtag_TDO_driven ?
                jtag_TDO_data : 1'b0;
   bit          __jtag_TCK;
   bit          __jtag_TMS;
   bit          __jtag_TDI;
   bit          __jtag_TRSTn;
   int          __exit;
   reg          init_done_sticky;
   assign #0.1 jtag_TCK   = __jtag_TCK;
   assign #0.1 jtag_TMS   = __jtag_TMS;
   assign #0.1 jtag_TDI   = __jtag_TDI;
   assign #0.1 jtag_TRSTn = __jtag_TRSTn;
   assign #0.1 exit = __exit;
   always @(posedge clock) begin
      r_reset <= reset;
      if (reset || r_reset) begin
         __exit = 0;
         tickCounterReg <= TICK_DELAY;
         init_done_sticky <= 1'b0;
      end else begin
         init_done_sticky <= init_done | init_done_sticky;
         if (enable && init_done_sticky) begin
            tickCounterReg <= tickCounterNxt;
            if (tickCounterReg == 0) begin
            end
         end  
      end  
   end  
endmodule
module axi_adapter #(
  parameter int unsigned DATA_WIDTH            = 256,
  parameter logic        CRITICAL_WORD_FIRST   = 0,  
  parameter int unsigned AXI_ID_WIDTH          = 10,
  parameter int unsigned CACHELINE_BYTE_OFFSET = 8
)(
  input  logic                             clk_i,   
  input  logic                             rst_ni,  
  input  logic                             req_i,
  input  ariane_axi::ad_req_t              type_i,
  output logic                             gnt_o,
  output logic [AXI_ID_WIDTH-1:0]          gnt_id_o,
  input  logic [63:0]                      addr_i,
  input  logic                             we_i,
  input  logic [(DATA_WIDTH/64)-1:0][63:0] wdata_i,
  input  logic [(DATA_WIDTH/64)-1:0][7:0]  be_i,
  input  logic [1:0]                       size_i,
  input  logic [AXI_ID_WIDTH-1:0]          id_i,
  output logic                             valid_o,
  output logic [(DATA_WIDTH/64)-1:0][63:0] rdata_o,
  output logic [AXI_ID_WIDTH-1:0]          id_o,
  output logic [63:0]                      critical_word_o,
  output logic                             critical_word_valid_o,
  output ariane_axi::req_t                 axi_req_o,
  input  ariane_axi::resp_t                axi_resp_i
);
  localparam BURST_SIZE = DATA_WIDTH/64-1;
  localparam ADDR_INDEX = ($clog2(DATA_WIDTH/64) > 0) ? $clog2(DATA_WIDTH/64) : 1;
  enum logic [3:0] {
    IDLE, WAIT_B_VALID, WAIT_AW_READY, WAIT_LAST_W_READY, WAIT_LAST_W_READY_AW_READY, WAIT_AW_READY_BURST,
    WAIT_R_VALID, WAIT_R_VALID_MULTIPLE, COMPLETE_READ
  } state_q, state_d;
  logic [ADDR_INDEX-1:0] cnt_d, cnt_q;
  logic [(DATA_WIDTH/64)-1:0][63:0] cache_line_d, cache_line_q;
  logic [(DATA_WIDTH/64)-1:0] addr_offset_d, addr_offset_q;
  logic [AXI_ID_WIDTH-1:0]    id_d, id_q;
  logic [ADDR_INDEX-1:0]      index;
  always_comb begin : axi_fsm
    axi_req_o.aw_valid  = 1'b0;
    axi_req_o.aw.addr   = addr_i;
    axi_req_o.aw.prot   = 3'b0;
    axi_req_o.aw.region = 4'b0;
    axi_req_o.aw.len    = 8'b0;
    axi_req_o.aw.size   = {1'b0, size_i};  
    axi_req_o.aw.burst  = axi_pkg::BURST_INCR;  
    axi_req_o.aw.lock   = 1'b0;
    axi_req_o.aw.cache  = 4'b0;
    axi_req_o.aw.qos    = 4'b0;
    axi_req_o.aw.id     = id_i;
    axi_req_o.aw.atop   = '0;  
    axi_req_o.aw.user   = '0;
    axi_req_o.ar_valid  = 1'b0;
    axi_req_o.ar.addr   = (CRITICAL_WORD_FIRST || type_i == ariane_axi::SINGLE_REQ) ? addr_i : { addr_i[63:CACHELINE_BYTE_OFFSET], {{CACHELINE_BYTE_OFFSET}{1'b0}}};
    axi_req_o.ar.prot   = 3'b0;
    axi_req_o.ar.region = 4'b0;
    axi_req_o.ar.len    = 8'b0;
    axi_req_o.ar.size   = {1'b0, size_i};  
    axi_req_o.ar.burst  = (CRITICAL_WORD_FIRST ? axi_pkg::BURST_WRAP : axi_pkg::BURST_INCR);  
    axi_req_o.ar.lock   = 1'b0;
    axi_req_o.ar.cache  = 4'b0;
    axi_req_o.ar.qos    = 4'b0;
    axi_req_o.ar.id     = id_i;
    axi_req_o.ar.user   = '0;
    axi_req_o.w_valid   = 1'b0;
    axi_req_o.w.data    = wdata_i[0];
    axi_req_o.w.strb    = be_i[0];
    axi_req_o.w.last    = 1'b0;
    axi_req_o.w.user    = '0;
    axi_req_o.b_ready   = 1'b0;
    axi_req_o.r_ready   = 1'b0;
    gnt_o    = 1'b0;
    gnt_id_o = id_i;
    valid_o  = 1'b0;
    id_o     = axi_resp_i.r.id;
    critical_word_o       = axi_resp_i.r.data;
    critical_word_valid_o = 1'b0;
    rdata_o               = cache_line_q;
    state_d       = state_q;
    cnt_d         = cnt_q;
    cache_line_d  = cache_line_q;
    addr_offset_d = addr_offset_q;
    id_d          = id_q;
    index         = '0;
    case (state_q)
      IDLE: begin
        cnt_d = '0;
        if (req_i) begin
          if (we_i) begin
            axi_req_o.aw_valid = 1'b1;
            axi_req_o.w_valid  = 1'b1;
            if (type_i == ariane_axi::SINGLE_REQ) begin
              axi_req_o.w.last   = 1'b1;
              gnt_o = axi_resp_i.aw_ready & axi_resp_i.w_ready;
              case ({axi_resp_i.aw_ready, axi_resp_i.w_ready})
                2'b11: state_d = WAIT_B_VALID;
                2'b01: state_d = WAIT_AW_READY;
                2'b10: state_d = WAIT_LAST_W_READY;
                default: state_d = IDLE;
              endcase
            end else begin
              axi_req_o.aw.len = BURST_SIZE;  
              axi_req_o.w.data = wdata_i[0];
              axi_req_o.w.strb = be_i[0];
              if (axi_resp_i.w_ready)
                cnt_d = BURST_SIZE - 1;
              else
                cnt_d = BURST_SIZE;
              case ({axi_resp_i.aw_ready, axi_resp_i.w_ready})
                2'b11: state_d = WAIT_LAST_W_READY;
                2'b01: state_d = WAIT_LAST_W_READY_AW_READY;
                2'b10: state_d = WAIT_LAST_W_READY;
                default:;
              endcase
            end
          end else begin
            axi_req_o.ar_valid = 1'b1;
            gnt_o = axi_resp_i.ar_ready;
            if (type_i != ariane_axi::SINGLE_REQ) begin
              axi_req_o.ar.len = BURST_SIZE;
              cnt_d = BURST_SIZE;
            end
            if (axi_resp_i.ar_ready) begin
              state_d = (type_i == ariane_axi::SINGLE_REQ) ? WAIT_R_VALID : WAIT_R_VALID_MULTIPLE;
              addr_offset_d = addr_i[ADDR_INDEX-1+3:3];
            end
          end
        end
      end
      WAIT_AW_READY: begin
        axi_req_o.aw_valid = 1'b1;
        if (axi_resp_i.aw_ready) begin
          gnt_o   = 1'b1;
          state_d = WAIT_B_VALID;
        end
      end
      WAIT_LAST_W_READY_AW_READY: begin
        axi_req_o.w_valid  = 1'b1;
        axi_req_o.w.last   = (cnt_q == '0);
        if (type_i == ariane_axi::SINGLE_REQ) begin
          axi_req_o.w.data = wdata_i[0];
          axi_req_o.w.strb = be_i[0];
        end else begin
          axi_req_o.w.data = wdata_i[BURST_SIZE-cnt_q];
          axi_req_o.w.strb = be_i[BURST_SIZE-cnt_q];
        end
        axi_req_o.aw_valid = 1'b1;
        axi_req_o.aw.len   = BURST_SIZE;
        case ({axi_resp_i.aw_ready, axi_resp_i.w_ready})
          2'b01: begin
            if (cnt_q == 0)
              state_d = WAIT_AW_READY_BURST;
            else  
              cnt_d = cnt_q - 1;
          end
          2'b10: state_d = WAIT_LAST_W_READY;
          2'b11: begin
            if (cnt_q == 0) begin
              state_d = WAIT_B_VALID;
              gnt_o   = 1'b1;
            end else begin
              state_d = WAIT_LAST_W_READY;
              cnt_d   = cnt_q - 1;
            end
          end
          default:;
         endcase
      end
      WAIT_AW_READY_BURST: begin
        axi_req_o.aw_valid = 1'b1;
        axi_req_o.aw.len   = BURST_SIZE;
        if (axi_resp_i.aw_ready) begin
          state_d  = WAIT_B_VALID;
          gnt_o    = 1'b1;
        end
      end
      WAIT_LAST_W_READY: begin
        axi_req_o.w_valid = 1'b1;
        if (type_i != ariane_axi::SINGLE_REQ) begin
          axi_req_o.w.data = wdata_i[BURST_SIZE-cnt_q];
          axi_req_o.w.strb = be_i[BURST_SIZE-cnt_q];
        end
        if (cnt_q == '0) begin
          axi_req_o.w.last = 1'b1;
          if (axi_resp_i.w_ready) begin
            state_d = WAIT_B_VALID;
            gnt_o   = 1'b1;
          end
        end else if (axi_resp_i.w_ready) begin
          cnt_d = cnt_q - 1;
        end
      end
      WAIT_B_VALID: begin
        axi_req_o.b_ready = 1'b1;
        id_o = axi_resp_i.b.id;
        if (axi_resp_i.b_valid) begin
          state_d = IDLE;
          valid_o = 1'b1;
        end
      end
      WAIT_R_VALID_MULTIPLE, WAIT_R_VALID: begin
        if (CRITICAL_WORD_FIRST)
          index = addr_offset_q + (BURST_SIZE-cnt_q);
        else
          index = BURST_SIZE-cnt_q;
        axi_req_o.r_ready = 1'b1;
        if (axi_resp_i.r_valid) begin
          if (CRITICAL_WORD_FIRST) begin
            if (state_q == WAIT_R_VALID_MULTIPLE && cnt_q == BURST_SIZE) begin
              critical_word_valid_o = 1'b1;
              critical_word_o       = axi_resp_i.r.data;
            end
          end else begin
            if (index == addr_offset_q) begin
              critical_word_valid_o = 1'b1;
              critical_word_o       = axi_resp_i.r.data;
            end
          end
          if (axi_resp_i.r.last) begin
            id_d    = axi_resp_i.r.id;
            state_d = COMPLETE_READ;
          end
          if (state_q == WAIT_R_VALID_MULTIPLE) begin
            cache_line_d[index] = axi_resp_i.r.data;
          end else
            cache_line_d[0] = axi_resp_i.r.data;
          cnt_d = cnt_q - 1;
        end
      end
      COMPLETE_READ: begin
        valid_o = 1'b1;
        state_d = IDLE;
        id_o    = id_q;
      end
    endcase
  end
  always_ff @(posedge clk_i or negedge rst_ni) begin
    if (~rst_ni) begin
      state_q       <= IDLE;
      cnt_q         <= '0;
      cache_line_q  <= '0;
      addr_offset_q <= '0;
      id_q          <= '0;
    end else begin
      state_q       <= state_d;
      cnt_q         <= cnt_d;
      cache_line_q  <= cache_line_d;
      addr_offset_q <= addr_offset_d;
      id_q          <= id_d;
    end
  end
endmodule
module bootrom (
   input  logic         clk_i,
   input  logic         req_i,
   input  logic [63:0]  addr_i,
   output logic [63:0]  rdata_o
);
    localparam int RomSize = 201;
    const logic [RomSize-1:0][63:0] mem = {
        64'h00000000_00000068,
        64'h74646977_2d6f692d,
        64'h67657200_74666968,
        64'h732d6765_72007374,
        64'h70757272_65746e69,
        64'h00646565_70732d74,
        64'h6e657272_75630073,
        64'h656d616e_2d676572,
        64'h00646564_6e657478,
        64'h652d7374_70757272,
        64'h65746e69_00736567,
        64'h6e617200_656c646e,
        64'h61687000_72656c6c,
        64'h6f72746e_6f632d74,
        64'h70757272_65746e69,
        64'h00736c6c_65632d74,
        64'h70757272_65746e69,
        64'h23007469_6c70732d,
        64'h626c7400_65707974,
        64'h2d756d6d_00617369,
        64'h2c766373_69720073,
        64'h75746174_73006765,
        64'h72006570_79745f65,
        64'h63697665_64007963,
        64'h6e657571_6572662d,
        64'h6b636f6c_63007963,
        64'h6e657571_6572662d,
        64'h65736162_656d6974,
        64'h006c6564_6f6d0065,
        64'h6c626974_61706d6f,
        64'h6300736c_6c65632d,
        64'h657a6973_2300736c,
        64'h6c65632d_73736572,
        64'h64646123_09000000,
        64'h02000000_02000000,
        64'h02000000_006c6f72,
        64'h746e6f63_cc000000,
        64'h08000000_03000000,
        64'h00100000_00000000,
        64'h00000018_00000000,
        64'h5b000000_10000000,
        64'h03000000_07000000,
        64'h06000000_05000000,
        64'h04000000_e4000000,
        64'h10000000_03000000,
        64'h00007265_6d69745f,
        64'h6270612c_706c7570,
        64'h1b000000_0f000000,
        64'h03000000_00003030,
        64'h30303030_38314072,
        64'h656d6974_01000000,
        64'h02000000_04000000,
        64'hf9000000_04000000,
        64'h03000000_02000000,
        64'hef000000_04000000,
        64'h03000000_01000000,
        64'he4000000_04000000,
        64'h03000000_00c20100,
        64'hd6000000_04000000,
        64'h03000000_80f0fa02,
        64'h3f000000_04000000,
        64'h03000000_00100000,
        64'h00000000_00000010,
        64'h00000000_5b000000,
        64'h10000000_03000000,
        64'h00303537_3631736e,
        64'h1b000000_08000000,
        64'h03000000_00000030,
        64'h30303030_30303140,
        64'h74726175_01000000,
        64'h02000000_006c6f72,
        64'h746e6f63_cc000000,
        64'h08000000_03000000,
        64'h00100000_00000000,
        64'h00000000_00000000,
        64'h5b000000_10000000,
        64'h03000000_ffff0000,
        64'h01000000_b8000000,
        64'h08000000_03000000,
        64'h00333130_2d677562,
        64'h65642c76_63736972,
        64'h1b000000_10000000,
        64'h03000000_00003040,
        64'h72656c6c_6f72746e,
        64'h6f632d67_75626564,
        64'h01000000_02000000,
        64'h006c6f72_746e6f63,
        64'hcc000000_08000000,
        64'h03000000_00000c00,
        64'h00000000_00000002,
        64'h00000000_5b000000,
        64'h10000000_03000000,
        64'h07000000_01000000,
        64'h03000000_01000000,
        64'hb8000000_10000000,
        64'h03000000_00000000,
        64'h30746e69_6c632c76,
        64'h63736972_1b000000,
        64'h0d000000_03000000,
        64'h00000030_30303030,
        64'h30324074_6e696c63,
        64'h01000000_b1000000,
        64'h00000000_03000000,
        64'h00007375_622d656c,
        64'h706d6973_00636f73,
        64'h2d657261_622d656e,
        64'h61697261_2c687465,
        64'h1b000000_1f000000,
        64'h03000000_02000000,
        64'h0f000000_04000000,
        64'h03000000_02000000,
        64'h00000000_04000000,
        64'h03000000_00636f73,
        64'h01000000_02000000,
        64'h00000010_00000000,
        64'h00000080_00000000,
        64'h5b000000_10000000,
        64'h03000000_00007972,
        64'h6f6d656d_4f000000,
        64'h07000000_03000000,
        64'h00303030_30303030,
        64'h38407972_6f6d656d,
        64'h01000000_02000000,
        64'h02000000_02000000,
        64'h01000000_a9000000,
        64'h04000000_03000000,
        64'h00006374_6e692d75,
        64'h70632c76_63736972,
        64'h1b000000_0f000000,
        64'h03000000_94000000,
        64'h00000000_03000000,
        64'h01000000_83000000,
        64'h04000000_03000000,
        64'h00000000_72656c6c,
        64'h6f72746e_6f632d74,
        64'h70757272_65746e69,
        64'h01000000_79000000,
        64'h00000000_03000000,
        64'h00003933_76732c76,
        64'h63736972_70000000,
        64'h0b000000_03000000,
        64'h00006364_66616d69,
        64'h34367672_66000000,
        64'h0b000000_03000000,
        64'h00000076_63736972,
        64'h00656e61_69726120,
        64'h2c687465_1b000000,
        64'h12000000_03000000,
        64'h00000000_79616b6f,
        64'h5f000000_05000000,
        64'h03000000_00000000,
        64'h5b000000_04000000,
        64'h03000000_00757063,
        64'h4f000000_04000000,
        64'h03000000_80f0fa02,
        64'h3f000000_04000000,
        64'h03000000_00000030,
        64'h40757063_01000000,
        64'h00800000_2c000000,
        64'h04000000_03000000,
        64'h00000000_0f000000,
        64'h04000000_03000000,
        64'h01000000_00000000,
        64'h04000000_03000000,
        64'h00000000_73757063,
        64'h01000000_00657261,
        64'h622d656e_61697261,
        64'h2c687465_26000000,
        64'h10000000_03000000,
        64'h00766564_2d657261,
        64'h622d656e_61697261,
        64'h2c687465_1b000000,
        64'h14000000_03000000,
        64'h02000000_0f000000,
        64'h04000000_03000000,
        64'h02000000_00000000,
        64'h04000000_03000000,
        64'h00000000_01000000,
        64'h00000000_00000000,
        64'h00000000_00000000,
        64'h84040000_06010000,
        64'h00000000_10000000,
        64'h11000000_28000000,
        64'hbc040000_38000000,
        64'hc2050000_edfe0dd0,
        64'h00000000_00000000,
        64'h00000000_00000000,
        64'h00000000_00000000,
        64'h00000000_00000000,
        64'h00000000_00000000,
        64'h00000000_ffdff06f,
        64'h10500073_03c58593,
        64'h00000597_f1402573,
        64'h00000000_00000000,
        64'h00000000_00000000,
        64'h00000000_00000000,
        64'h00000000_00000000,
        64'h00000000_00000000,
        64'h00040067_07458593,
        64'h00000597_f1402573,
        64'h01f41413_00100413
    };
    logic [$clog2(RomSize)-1:0] addr_q;
    always_ff @(posedge clk_i) begin
        if (req_i) begin
            addr_q <= addr_i[$clog2(RomSize)-1+3:3];
        end
    end
    assign rdata_o = (addr_q < RomSize) ? mem[addr_q] : '0;
endmodule
module sram #(
    parameter DATA_WIDTH = 64,
    parameter NUM_WORDS  = 1024,
    parameter OUT_REGS   = 0,     
    parameter DROMAJO_RAM  = 0
)(
   input  logic                          clk_i,
   input  logic                          rst_ni,
   input  logic                          req_i,
   input  logic                          we_i,
   input  logic [$clog2(NUM_WORDS)-1:0]  addr_i,
   input  logic [DATA_WIDTH-1:0]         wdata_i,
   input  logic [(DATA_WIDTH+7)/8-1:0]   be_i,
   output logic [DATA_WIDTH-1:0]         rdata_o
);
localparam DATA_WIDTH_ALIGNED = ((DATA_WIDTH+63)/64)*64;
localparam BE_WIDTH_ALIGNED   = (((DATA_WIDTH+7)/8+7)/8)*8;
logic [DATA_WIDTH_ALIGNED-1:0]  wdata_aligned;
logic [BE_WIDTH_ALIGNED-1:0]    be_aligned;
logic [DATA_WIDTH_ALIGNED-1:0]  rdata_aligned;
always_comb begin : p_align
    wdata_aligned                    ='0;
    be_aligned                       ='0;
    wdata_aligned[DATA_WIDTH-1:0]    = wdata_i;
    be_aligned[BE_WIDTH_ALIGNED-1:0] = be_i;
    rdata_o = rdata_aligned[DATA_WIDTH-1:0];
end
  for (genvar k = 0; k<(DATA_WIDTH+63)/64; k++) begin : gen_cut
    if (DROMAJO_RAM) begin : gen_dromajo
      dromajo_ram #(
        .ADDR_WIDTH($clog2(NUM_WORDS)),
        .DATA_DEPTH(NUM_WORDS),
        .OUT_REGS (0)
      ) i_ram (
          .Clk_CI    ( clk_i                     ),
          .Rst_RBI   ( rst_ni                    ),
          .CSel_SI   ( req_i                     ),
          .WrEn_SI   ( we_i                      ),
          .BEn_SI    ( be_aligned[k*8 +: 8]      ),
          .WrData_DI ( wdata_aligned[k*64 +: 64] ),
          .Addr_DI   ( addr_i                    ),
          .RdData_DO ( rdata_aligned[k*64 +: 64] )
      );
    end else begin : gen_mem
      SyncSpRamBeNx64 #(
        .ADDR_WIDTH($clog2(NUM_WORDS)),
        .DATA_DEPTH(NUM_WORDS),
        .OUT_REGS (0),
        .SIM_INIT (1)
      ) i_ram (
          .Clk_CI    ( clk_i                     ),
          .Rst_RBI   ( rst_ni                    ),
          .CSel_SI   ( req_i                     ),
          .WrEn_SI   ( we_i                      ),
          .BEn_SI    ( be_aligned[k*8 +: 8]      ),
          .WrData_DI ( wdata_aligned[k*64 +: 64] ),
          .Addr_DI   ( addr_i                    ),
          .RdData_DO ( rdata_aligned[k*64 +: 64] )
      );
    end
  end
endmodule : sram
module clint #(
    parameter int unsigned AXI_ADDR_WIDTH = 64,
    parameter int unsigned AXI_DATA_WIDTH = 64,
    parameter int unsigned AXI_ID_WIDTH   = 10,
    parameter int unsigned NR_CORES       = 1  
) (
    input  logic                clk_i,        
    input  logic                rst_ni,       
    input  logic                testmode_i,
    input  ariane_axi::req_t    axi_req_i,
    output ariane_axi::resp_t   axi_resp_o,
    input  logic                rtc_i,        
    output logic [NR_CORES-1:0] timer_irq_o,  
    output logic [NR_CORES-1:0] ipi_o         
);
    localparam logic [15:0] MSIP_BASE     = 16'h0;
    localparam logic [15:0] MTIMECMP_BASE = 16'h4000;
    localparam logic [15:0] MTIME_BASE    = 16'hbff8;
    localparam AddrSelWidth = (NR_CORES == 1) ? 1 : $clog2(NR_CORES);
    logic [AXI_ADDR_WIDTH-1:0] address;
    logic                      en;
    logic                      we;
    logic [63:0] wdata;
    logic [63:0] rdata;
    logic [15:0] register_address;
    assign register_address = address[15:0];
    logic [63:0]               mtime_n, mtime_q;
    logic [NR_CORES-1:0][63:0] mtimecmp_n, mtimecmp_q;
    logic [NR_CORES-1:0]       msip_n, msip_q;
    logic increase_timer;
    axi_lite_interface #(
        .AXI_ADDR_WIDTH ( AXI_ADDR_WIDTH ),
        .AXI_DATA_WIDTH ( AXI_DATA_WIDTH ),
        .AXI_ID_WIDTH   ( AXI_ID_WIDTH    )
    ) axi_lite_interface_i (
        .clk_i      ( clk_i      ),
        .rst_ni     ( rst_ni     ),
        .axi_req_i  ( axi_req_i  ),
        .axi_resp_o ( axi_resp_o ),
        .address_o  ( address    ),
        .en_o       ( en         ),
        .we_o       ( we         ),
        .data_i     ( rdata      ),
        .data_o     ( wdata      )
    );
    always_comb begin
        mtime_n    = mtime_q;
        mtimecmp_n = mtimecmp_q;
        msip_n     = msip_q;
        if (increase_timer)
            mtime_n = mtime_q + 1;
        if (en && we) begin
            case (register_address) inside
                [MSIP_BASE:MSIP_BASE+4*NR_CORES]: begin
                    msip_n[$unsigned(address[AddrSelWidth-1+2:2])] = wdata[32*address[2]];
                end
                [MTIMECMP_BASE:MTIMECMP_BASE+8*NR_CORES]: begin
                    mtimecmp_n[$unsigned(address[AddrSelWidth-1+3:3])] = wdata;
                end
                MTIME_BASE: begin
                    mtime_n = wdata;
                end
                default:;
            endcase
        end
    end
    always_comb begin
        rdata = 'b0;
        if (en && !we) begin
            case (register_address) inside
                [MSIP_BASE:MSIP_BASE+4*NR_CORES]: begin
                    rdata = msip_q[$unsigned(address[AddrSelWidth-1+2:2])];
                end
                [MTIMECMP_BASE:MTIMECMP_BASE+8*NR_CORES]: begin
                    rdata = mtimecmp_q[$unsigned(address[AddrSelWidth-1+3:3])];
                end
                MTIME_BASE: begin
                    rdata = mtime_q;
                end
                default:;
            endcase
        end
    end
    always_comb begin : irq_gen
        for (int unsigned i = 0; i < NR_CORES; i++) begin
            if (mtime_q >= mtimecmp_q[i]) begin
                timer_irq_o[i] = 1'b1;
            end else begin
                timer_irq_o[i] = 1'b0;
            end
        end
    end
    clint_sync_wedge i_sync_edge (
        .clk_i,
        .rst_ni,
        .serial_i  ( rtc_i          ),
        .r_edge_o  ( increase_timer ),
        .f_edge_o  (                ),  
        .serial_o  (                )   
    );
    always_ff @(posedge clk_i or negedge rst_ni) begin
        if (~rst_ni) begin
            mtime_q    <= 64'b0;
            mtimecmp_q <= 'b0;
            msip_q     <= '0;
        end else begin
            mtime_q    <= mtime_n;
            mtimecmp_q <= mtimecmp_n;
            msip_q     <= msip_n;
        end
    end
    assign ipi_o = msip_q;
endmodule
module axi_lite_interface #(
    parameter int unsigned AXI_ADDR_WIDTH = 64,
    parameter int unsigned AXI_DATA_WIDTH = 64,
    parameter int unsigned AXI_ID_WIDTH   = 10
) (
    input logic                       clk_i,     
    input logic                       rst_ni,   
    input  ariane_axi::req_t          axi_req_i,
    output ariane_axi::resp_t         axi_resp_o,
    output logic [AXI_ADDR_WIDTH-1:0] address_o,
    output logic                      en_o,         
    output logic                      we_o,         
    input  logic [AXI_DATA_WIDTH-1:0] data_i,       
    output logic [AXI_DATA_WIDTH-1:0] data_o
);
    enum logic [1:0] { IDLE, READ, WRITE, WRITE_B } state_q, state_d;
    logic [AXI_ID_WIDTH-1:0]   trans_id_n, trans_id_q;
    logic [AXI_ADDR_WIDTH-1:0] address_n,  address_q;
    assign axi_resp_o.r.data = data_i;
    assign axi_resp_o.r.id = trans_id_q;
    assign axi_resp_o.b.id = trans_id_q;
    assign axi_resp_o.r.last = 1'b1;
    assign axi_resp_o.b.resp = 2'b0;
    assign axi_resp_o.r.resp = 2'b0;
    assign data_o = axi_req_i.w.data;
    always_comb begin
        state_d    = state_q;
        address_n  = address_q;
        trans_id_n = trans_id_q;
        axi_resp_o.aw_ready = 1'b0;
        axi_resp_o.w_ready  = 1'b0;
        axi_resp_o.b_valid  = 1'b0;
        axi_resp_o.ar_ready = 1'b0;
        axi_resp_o.r_valid  = 1'b0;
        address_o      = '0;
        we_o           = 1'b0;
        en_o           = 1'b0;
        case (state_q)
            IDLE: begin
                if (axi_req_i.aw_valid) begin
                    axi_resp_o.aw_ready = 1'b1;
                    state_d = WRITE;
                    address_n = axi_req_i.aw.addr;
                    trans_id_n = axi_req_i.aw.id;
                end else if (axi_req_i.ar_valid) begin
                    axi_resp_o.ar_ready = 1'b1;
                    state_d = READ;
                    address_n = axi_req_i.ar.addr;
                    trans_id_n = axi_req_i.ar.id;
                end
            end
            READ: begin
                en_o       = 1'b1;
                address_o = address_q;
                axi_resp_o.r_valid = 1'b1;
                if (axi_req_i.r_ready)
                    state_d = IDLE;
            end
            WRITE: begin
                if (axi_req_i.w_valid) begin
                    axi_resp_o.w_ready = 1'b1;
                    address_o = address_q;
                    en_o = 1'b1;
                    we_o = 1'b1;
                    state_d = WRITE_B;
                end
            end
            WRITE_B: begin
                axi_resp_o.b_valid  = 1'b1;
                if (axi_req_i.b_ready)
                    state_d = IDLE;
            end
            default:;
        endcase
    end
    always_ff @(posedge clk_i or negedge rst_ni) begin
        if (!rst_ni) begin
            state_q    <= IDLE;
            address_q  <= '0;
            trans_id_q <= '0;
        end else begin
            state_q    <= state_d;
            address_q  <= address_n;
            trans_id_q <= trans_id_n;
        end
    end
endmodule
module clint_sync_wedge #(
    parameter int unsigned STAGES = 2
) (
    input  logic clk_i,
    input  logic rst_ni,
    input  logic serial_i,
    output logic r_edge_o,
    output logic f_edge_o,
    output logic serial_o
);
    logic serial, serial_q;
    assign serial_o =  serial_q;
    assign f_edge_o = (~serial) & serial_q;
    assign r_edge_o =  serial & (~serial_q);
    clint_sync #(
        .STAGES (STAGES)
    ) i_sync (
        .clk_i,
        .rst_ni,
        .serial_i,
        .serial_o ( serial )
    );
    always_ff @(posedge clk_i, negedge rst_ni) begin
        if (!rst_ni) begin
            serial_q <= 1'b0;
        end else begin
            serial_q <= serial;
        end
    end
endmodule
module clint_sync #(
    parameter int unsigned STAGES = 2
) (
    input  logic clk_i,
    input  logic rst_ni,
    input  logic serial_i,
    output logic serial_o
);
   logic [STAGES-1:0] reg_q;
    always_ff @(posedge clk_i, negedge rst_ni) begin
        if (!rst_ni) begin
            reg_q <= 'h0;
        end else begin
            reg_q <= {reg_q[STAGES-2:0], serial_i};
        end
    end
    assign serial_o = reg_q[STAGES-1];
endmodule

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

endmodule // ariane

module ariane_testharness #(
  parameter int unsigned AXI_USER_WIDTH    = 1,
  parameter int unsigned AXI_ADDRESS_WIDTH = 64,
  parameter int unsigned AXI_DATA_WIDTH    = 64,
  parameter bit          InclSimDTM        = 1'b1,
  parameter int unsigned NUM_WORDS         = 2**25,          
  parameter bit          StallRandomOutput = 1'b0,
  parameter bit          StallRandomInput  = 1'b0
) (
  input  logic                           clk_i,
  input  logic                           rtc_i,
  input  logic                           rst_ni,
  output logic [31:0]                    exit_o
);
  logic        test_en;
  logic        ndmreset;
  logic        ndmreset_n;
  logic        debug_req_core;
  int          jtag_enable;
  logic        init_done;
  logic [31:0] jtag_exit, dmi_exit;
  logic        jtag_TCK;
  logic        jtag_TMS;
  logic        jtag_TDI;
  logic        jtag_TRSTn;
  logic        jtag_TDO_data;
  logic        jtag_TDO_driven;
  logic        debug_req_valid;
  logic        debug_req_ready;
  logic        debug_resp_valid;
  logic        debug_resp_ready;
  logic        jtag_req_valid;
  logic [6:0]  jtag_req_bits_addr;
  logic [1:0]  jtag_req_bits_op;
  logic [31:0] jtag_req_bits_data;
  logic        jtag_resp_ready;
  logic        jtag_resp_valid;
  logic        dmi_req_valid;
  logic        dmi_resp_ready;
  logic        dmi_resp_valid;
  dm::dmi_req_t  jtag_dmi_req;
  dm::dmi_req_t  dmi_req;
  dm::dmi_req_t  debug_req;
  dm::dmi_resp_t debug_resp;
  assign test_en = 1'b0;
  AXI_BUS #(
    .AXI_ADDR_WIDTH ( AXI_ADDRESS_WIDTH   ),
    .AXI_DATA_WIDTH ( AXI_DATA_WIDTH      ),
    .AXI_ID_WIDTH   ( ariane_soc::IdWidth ),
    .AXI_USER_WIDTH ( AXI_USER_WIDTH      )
  ) slave[ariane_soc::NrSlaves-1:0]();
  AXI_BUS #(
    .AXI_ADDR_WIDTH ( AXI_ADDRESS_WIDTH        ),
    .AXI_DATA_WIDTH ( AXI_DATA_WIDTH           ),
    .AXI_ID_WIDTH   ( ariane_soc::IdWidthSlave ),
    .AXI_USER_WIDTH ( AXI_USER_WIDTH           )
  ) master[ariane_soc::NB_PERIPHERALS-1:0]();
  rstgen i_rstgen_main (
    .clk_i        ( clk_i                ),
    .rst_ni       ( rst_ni & (~ndmreset) ),
    .test_mode_i  ( test_en              ),
    .rst_no       ( ndmreset_n           ),
    .init_no      (                      )  
  );
  assign init_done = rst_ni;
  initial begin
    if (!$value$plusargs("jtag_rbb_enable=%b", jtag_enable)) jtag_enable = 'h0;
    if (riscv::XLEN != 32 & riscv::XLEN != 64) $error("XLEN different from 32 and 64");
  end
  assign debug_req_valid     = (jtag_enable[0]) ? jtag_req_valid     : dmi_req_valid;
  assign debug_resp_ready    = (jtag_enable[0]) ? jtag_resp_ready    : dmi_resp_ready;
  assign debug_req           = (jtag_enable[0]) ? jtag_dmi_req       : dmi_req;
  assign exit_o              = (jtag_enable[0]) ? jtag_exit          : dmi_exit;
  assign jtag_resp_valid     = (jtag_enable[0]) ? debug_resp_valid   : 1'b0;
  assign dmi_resp_valid      = (jtag_enable[0]) ? 1'b0               : debug_resp_valid;
  SimJTAG i_SimJTAG (
    .clock                ( clk_i                ),
    .reset                ( ~rst_ni              ),
    .enable               ( jtag_enable[0]       ),
    .init_done            ( init_done            ),
    .jtag_TCK             ( jtag_TCK             ),
    .jtag_TMS             ( jtag_TMS             ),
    .jtag_TDI             ( jtag_TDI             ),
    .jtag_TRSTn           ( jtag_TRSTn           ),
    .jtag_TDO_data        ( jtag_TDO_data        ),
    .jtag_TDO_driven      ( jtag_TDO_driven      ),
    .exit                 ( jtag_exit            )
  );
  dmi_jtag i_dmi_jtag (
    .clk_i            ( clk_i           ),
    .rst_ni           ( rst_ni          ),
    .testmode_i       ( test_en         ),
    .dmi_req_o        ( jtag_dmi_req    ),
    .dmi_req_valid_o  ( jtag_req_valid  ),
    .dmi_req_ready_i  ( debug_req_ready ),
    .dmi_resp_i       ( debug_resp      ),
    .dmi_resp_ready_o ( jtag_resp_ready ),
    .dmi_resp_valid_i ( jtag_resp_valid ),
    .dmi_rst_no       (                 ),  
    .tck_i            ( jtag_TCK        ),
    .tms_i            ( jtag_TMS        ),
    .trst_ni          ( jtag_TRSTn      ),
    .td_i             ( jtag_TDI        ),
    .td_o             ( jtag_TDO_data   ),
    .tdo_oe_o         ( jtag_TDO_driven )
  );
  logic [1:0] debug_req_bits_op;
  assign dmi_req.op = dm::dtm_op_e'(debug_req_bits_op);
  if (InclSimDTM) begin
    SimDTM i_SimDTM (
      .clk                  ( clk_i                 ),
      .reset                ( ~rst_ni               ),
      .debug_req_valid      ( dmi_req_valid         ),
      .debug_req_ready      ( debug_req_ready       ),
      .debug_req_bits_addr  ( dmi_req.addr          ),
      .debug_req_bits_op    ( debug_req_bits_op     ),
      .debug_req_bits_data  ( dmi_req.data          ),
      .debug_resp_valid     ( dmi_resp_valid        ),
      .debug_resp_ready     ( dmi_resp_ready        ),
      .debug_resp_bits_resp ( debug_resp.resp       ),
      .debug_resp_bits_data ( debug_resp.data       ),
      .exit                 ( dmi_exit              )
    );
  end else begin
    assign dmi_req_valid = '0;
    assign debug_req_bits_op = '0;
    assign dmi_exit = 1'b0;
  end
  localparam int unsigned DmiDelCycles = 500;
  logic debug_req_core_ungtd;
  int dmi_del_cnt_d, dmi_del_cnt_q;
  assign dmi_del_cnt_d  = (dmi_del_cnt_q) ? dmi_del_cnt_q - 1 : 0;
  assign debug_req_core = (dmi_del_cnt_q) ? 1'b0 : debug_req_core_ungtd;
  always_ff @(posedge clk_i or negedge rst_ni) begin : p_dmi_del_cnt
    if(!rst_ni) begin
      dmi_del_cnt_q <= DmiDelCycles;
    end else begin
      dmi_del_cnt_q <= dmi_del_cnt_d;
    end
  end
  ariane_axi_soc::req_t    dm_axi_m_req;
  ariane_axi_soc::resp_t   dm_axi_m_resp;
  logic                dm_slave_req;
  logic                dm_slave_we;
  logic [64-1:0]       dm_slave_addr;
  logic [64/8-1:0]     dm_slave_be;
  logic [64-1:0]       dm_slave_wdata;
  logic [64-1:0]       dm_slave_rdata;
  logic                dm_master_req;
  logic [64-1:0]       dm_master_add;
  logic                dm_master_we;
  logic [64-1:0]       dm_master_wdata;
  logic [64/8-1:0]     dm_master_be;
  logic                dm_master_gnt;
  logic                dm_master_r_valid;
  logic [64-1:0]       dm_master_r_rdata;
  dm_top #(
    .NrHarts              ( 1                           ),
    .BusWidth             ( AXI_DATA_WIDTH              ),
    .SelectableHarts      ( 1'b1                        )
  ) i_dm_top (
    .clk_i                ( clk_i                       ),
    .rst_ni               ( rst_ni                      ),  
    .testmode_i           ( test_en                     ),
    .ndmreset_o           ( ndmreset                    ),
    .dmactive_o           (                             ),  
    .debug_req_o          ( debug_req_core_ungtd        ),
    .unavailable_i        ( '0                          ),
    .hartinfo_i           ( {ariane_pkg::DebugHartInfo} ),
    .slave_req_i          ( dm_slave_req                ),
    .slave_we_i           ( dm_slave_we                 ),
    .slave_addr_i         ( dm_slave_addr               ),
    .slave_be_i           ( dm_slave_be                 ),
    .slave_wdata_i        ( dm_slave_wdata              ),
    .slave_rdata_o        ( dm_slave_rdata              ),
    .master_req_o         ( dm_master_req               ),
    .master_add_o         ( dm_master_add               ),
    .master_we_o          ( dm_master_we                ),
    .master_wdata_o       ( dm_master_wdata             ),
    .master_be_o          ( dm_master_be                ),
    .master_gnt_i         ( dm_master_gnt               ),
    .master_r_valid_i     ( dm_master_r_valid           ),
    .master_r_rdata_i     ( dm_master_r_rdata           ),
    .dmi_rst_ni           ( rst_ni                      ),
    .dmi_req_valid_i      ( debug_req_valid             ),
    .dmi_req_ready_o      ( debug_req_ready             ),
    .dmi_req_i            ( debug_req                   ),
    .dmi_resp_valid_o     ( debug_resp_valid            ),
    .dmi_resp_ready_i     ( debug_resp_ready            ),
    .dmi_resp_o           ( debug_resp                  )
  );
  axi2mem #(
    .AXI_ID_WIDTH   ( ariane_soc::IdWidthSlave ),
    .AXI_ADDR_WIDTH ( AXI_ADDRESS_WIDTH        ),
    .AXI_DATA_WIDTH ( AXI_DATA_WIDTH           ),
    .AXI_USER_WIDTH ( AXI_USER_WIDTH           )
  ) i_dm_axi2mem (
    .clk_i      ( clk_i                     ),
    .rst_ni     ( rst_ni                    ),
    .slave      ( master[ariane_soc::Debug] ),
    .req_o      ( dm_slave_req              ),
    .we_o       ( dm_slave_we               ),
    .addr_o     ( dm_slave_addr             ),
    .be_o       ( dm_slave_be               ),
    .data_o     ( dm_slave_wdata            ),
    .data_i     ( dm_slave_rdata            )
  );
  axi_master_connect i_dm_axi_master_connect (
    .axi_req_i(dm_axi_m_req),
    .axi_resp_o(dm_axi_m_resp),
    .master(slave[1])
  );
  axi_adapter #(
    .DATA_WIDTH            ( AXI_DATA_WIDTH            )
  ) i_dm_axi_master (
    .clk_i                 ( clk_i                     ),
    .rst_ni                ( rst_ni                    ),
    .req_i                 ( dm_master_req             ),
    .type_i                ( ariane_axi::SINGLE_REQ    ),
    .gnt_o                 ( dm_master_gnt             ),
    .gnt_id_o              (                           ),
    .addr_i                ( dm_master_add             ),
    .we_i                  ( dm_master_we              ),
    .wdata_i               ( dm_master_wdata           ),
    .be_i                  ( dm_master_be              ),
    .size_i                ( 2'b11                     ),  
    .id_i                  ( '0                        ),
    .valid_o               ( dm_master_r_valid         ),
    .rdata_o               ( dm_master_r_rdata         ),
    .id_o                  (                           ),
    .critical_word_o       (                           ),
    .critical_word_valid_o (                           ),
    .axi_req_o             ( dm_axi_m_req              ),
    .axi_resp_i            ( dm_axi_m_resp             )
  );
  logic                         rom_req;
  logic [AXI_ADDRESS_WIDTH-1:0] rom_addr;
  logic [AXI_DATA_WIDTH-1:0]    rom_rdata;
  axi2mem #(
    .AXI_ID_WIDTH   ( ariane_soc::IdWidthSlave ),
    .AXI_ADDR_WIDTH ( AXI_ADDRESS_WIDTH        ),
    .AXI_DATA_WIDTH ( AXI_DATA_WIDTH           ),
    .AXI_USER_WIDTH ( AXI_USER_WIDTH           )
  ) i_axi2rom (
    .clk_i  ( clk_i                   ),
    .rst_ni ( ndmreset_n              ),
    .slave  ( master[ariane_soc::ROM] ),
    .req_o  ( rom_req                 ),
    .we_o   (                         ),
    .addr_o ( rom_addr                ),
    .be_o   (                         ),
    .data_o (                         ),
    .data_i ( rom_rdata               )
  );
  bootrom i_bootrom (
    .clk_i      ( clk_i     ),
    .req_i      ( rom_req   ),
    .addr_i     ( rom_addr  ),
    .rdata_o    ( rom_rdata )
  );
  AXI_BUS #(
    .AXI_ADDR_WIDTH ( AXI_ADDRESS_WIDTH        ),
    .AXI_DATA_WIDTH ( AXI_DATA_WIDTH           ),
    .AXI_ID_WIDTH   ( ariane_soc::IdWidthSlave ),
    .AXI_USER_WIDTH ( AXI_USER_WIDTH           )
  ) dram();
  logic                         req;
  logic                         we;
  logic [AXI_ADDRESS_WIDTH-1:0] addr;
  logic [AXI_DATA_WIDTH/8-1:0]  be;
  logic [AXI_DATA_WIDTH-1:0]    wdata;
  logic [AXI_DATA_WIDTH-1:0]    rdata;
  axi_riscv_atomics_wrap #(
    .AXI_ADDR_WIDTH ( AXI_ADDRESS_WIDTH        ),
    .AXI_DATA_WIDTH ( AXI_DATA_WIDTH           ),
    .AXI_ID_WIDTH   ( ariane_soc::IdWidthSlave ),
    .AXI_USER_WIDTH ( AXI_USER_WIDTH           ),
    .AXI_MAX_WRITE_TXNS ( 1  ),
    .RISCV_WORD_WIDTH   ( 64 )
  ) i_axi_riscv_atomics (
    .clk_i,
    .rst_ni ( ndmreset_n               ),
    .slv    ( master[ariane_soc::DRAM] ),
    .mst    ( dram                     )
  );
  AXI_BUS #(
    .AXI_ADDR_WIDTH ( AXI_ADDRESS_WIDTH        ),
    .AXI_DATA_WIDTH ( AXI_DATA_WIDTH           ),
    .AXI_ID_WIDTH   ( ariane_soc::IdWidthSlave ),
    .AXI_USER_WIDTH ( AXI_USER_WIDTH           )
  ) dram_delayed();
  ariane_axi_soc::aw_chan_slv_t aw_chan_i;
  ariane_axi_soc::w_chan_t      w_chan_i;
  ariane_axi_soc::b_chan_slv_t  b_chan_o;
  ariane_axi_soc::ar_chan_slv_t ar_chan_i;
  ariane_axi_soc::r_chan_slv_t  r_chan_o;
  ariane_axi_soc::aw_chan_slv_t aw_chan_o;
  ariane_axi_soc::w_chan_t      w_chan_o;
  ariane_axi_soc::b_chan_slv_t  b_chan_i;
  ariane_axi_soc::ar_chan_slv_t ar_chan_o;
  ariane_axi_soc::r_chan_slv_t  r_chan_i;
  axi_delayer #(
    .aw_t              ( ariane_axi_soc::aw_chan_slv_t ),
    .w_t               ( ariane_axi_soc::w_chan_t      ),
    .b_t               ( ariane_axi_soc::b_chan_slv_t  ),
    .ar_t              ( ariane_axi_soc::ar_chan_slv_t ),
    .r_t               ( ariane_axi_soc::r_chan_slv_t  ),
    .StallRandomOutput ( StallRandomOutput         ),
    .StallRandomInput  ( StallRandomInput          ),
    .FixedDelayInput   ( 0                         ),
    .FixedDelayOutput  ( 0                         )
  ) i_axi_delayer (
    .clk_i      ( clk_i                 ),
    .rst_ni     ( ndmreset_n            ),
    .aw_valid_i ( dram.aw_valid         ),
    .aw_chan_i  ( aw_chan_i             ),
    .aw_ready_o ( dram.aw_ready         ),
    .w_valid_i  ( dram.w_valid          ),
    .w_chan_i   ( w_chan_i              ),
    .w_ready_o  ( dram.w_ready          ),
    .b_valid_o  ( dram.b_valid          ),
    .b_chan_o   ( b_chan_o              ),
    .b_ready_i  ( dram.b_ready          ),
    .ar_valid_i ( dram.ar_valid         ),
    .ar_chan_i  ( ar_chan_i             ),
    .ar_ready_o ( dram.ar_ready         ),
    .r_valid_o  ( dram.r_valid          ),
    .r_chan_o   ( r_chan_o              ),
    .r_ready_i  ( dram.r_ready          ),
    .aw_valid_o ( dram_delayed.aw_valid ),
    .aw_chan_o  ( aw_chan_o             ),
    .aw_ready_i ( dram_delayed.aw_ready ),
    .w_valid_o  ( dram_delayed.w_valid  ),
    .w_chan_o   ( w_chan_o              ),
    .w_ready_i  ( dram_delayed.w_ready  ),
    .b_valid_i  ( dram_delayed.b_valid  ),
    .b_chan_i   ( b_chan_i              ),
    .b_ready_o  ( dram_delayed.b_ready  ),
    .ar_valid_o ( dram_delayed.ar_valid ),
    .ar_chan_o  ( ar_chan_o             ),
    .ar_ready_i ( dram_delayed.ar_ready ),
    .r_valid_i  ( dram_delayed.r_valid  ),
    .r_chan_i   ( r_chan_i              ),
    .r_ready_o  ( dram_delayed.r_ready  )
  );
  assign aw_chan_i.atop = dram.aw_atop;
  assign aw_chan_i.id = dram.aw_id;
  assign aw_chan_i.addr = dram.aw_addr;
  assign aw_chan_i.len = dram.aw_len;
  assign aw_chan_i.size = dram.aw_size;
  assign aw_chan_i.burst = dram.aw_burst;
  assign aw_chan_i.lock = dram.aw_lock;
  assign aw_chan_i.cache = dram.aw_cache;
  assign aw_chan_i.prot = dram.aw_prot;
  assign aw_chan_i.qos = dram.aw_qos;
  assign aw_chan_i.region = dram.aw_region;
  assign ar_chan_i.id = dram.ar_id;
  assign ar_chan_i.addr = dram.ar_addr;
  assign ar_chan_i.len = dram.ar_len;
  assign ar_chan_i.size = dram.ar_size;
  assign ar_chan_i.burst = dram.ar_burst;
  assign ar_chan_i.lock = dram.ar_lock;
  assign ar_chan_i.cache = dram.ar_cache;
  assign ar_chan_i.prot = dram.ar_prot;
  assign ar_chan_i.qos = dram.ar_qos;
  assign ar_chan_i.region = dram.ar_region;
  assign w_chan_i.data = dram.w_data;
  assign w_chan_i.strb = dram.w_strb;
  assign w_chan_i.last = dram.w_last;
  assign dram.r_id = r_chan_o.id;
  assign dram.r_data = r_chan_o.data;
  assign dram.r_resp = r_chan_o.resp;
  assign dram.r_last = r_chan_o.last;
  assign dram.b_id = b_chan_o.id;
  assign dram.b_resp = b_chan_o.resp;
  assign dram_delayed.aw_id = aw_chan_o.id;
  assign dram_delayed.aw_addr = aw_chan_o.addr;
  assign dram_delayed.aw_len = aw_chan_o.len;
  assign dram_delayed.aw_size = aw_chan_o.size;
  assign dram_delayed.aw_burst = aw_chan_o.burst;
  assign dram_delayed.aw_lock = aw_chan_o.lock;
  assign dram_delayed.aw_cache = aw_chan_o.cache;
  assign dram_delayed.aw_prot = aw_chan_o.prot;
  assign dram_delayed.aw_qos = aw_chan_o.qos;
  assign dram_delayed.aw_atop = aw_chan_o.atop;
  assign dram_delayed.aw_region = aw_chan_o.region;
  assign dram_delayed.aw_user = '0;
  assign dram_delayed.ar_id = ar_chan_o.id;
  assign dram_delayed.ar_addr = ar_chan_o.addr;
  assign dram_delayed.ar_len = ar_chan_o.len;
  assign dram_delayed.ar_size = ar_chan_o.size;
  assign dram_delayed.ar_burst = ar_chan_o.burst;
  assign dram_delayed.ar_lock = ar_chan_o.lock;
  assign dram_delayed.ar_cache = ar_chan_o.cache;
  assign dram_delayed.ar_prot = ar_chan_o.prot;
  assign dram_delayed.ar_qos = ar_chan_o.qos;
  assign dram_delayed.ar_region = ar_chan_o.region;
  assign dram_delayed.ar_user = '0;
  assign dram_delayed.w_data = w_chan_o.data;
  assign dram_delayed.w_strb = w_chan_o.strb;
  assign dram_delayed.w_last = w_chan_o.last;
  assign dram_delayed.w_user = '0;
  assign r_chan_i.id = dram_delayed.r_id;
  assign r_chan_i.data = dram_delayed.r_data;
  assign r_chan_i.resp = dram_delayed.r_resp;
  assign r_chan_i.last = dram_delayed.r_last;
  assign dram.r_user = '0;
  assign b_chan_i.id = dram_delayed.b_id;
  assign b_chan_i.resp = dram_delayed.b_resp;
  assign dram.b_user = '0;
  axi2mem #(
    .AXI_ID_WIDTH   ( ariane_soc::IdWidthSlave ),
    .AXI_ADDR_WIDTH ( AXI_ADDRESS_WIDTH        ),
    .AXI_DATA_WIDTH ( AXI_DATA_WIDTH           ),
    .AXI_USER_WIDTH ( AXI_USER_WIDTH           )
  ) i_axi2mem (
    .clk_i  ( clk_i        ),
    .rst_ni ( ndmreset_n   ),
    .slave  ( dram_delayed ),
    .req_o  ( req          ),
    .we_o   ( we           ),
    .addr_o ( addr         ),
    .be_o   ( be           ),
    .data_o ( wdata        ),
    .data_i ( rdata        )
  );
  sram #(
    .DATA_WIDTH ( AXI_DATA_WIDTH ),
    .NUM_WORDS  ( NUM_WORDS      )
  ) i_sram (
    .clk_i      ( clk_i                                                                       ),
    .rst_ni     ( rst_ni                                                                      ),
    .req_i      ( req                                                                         ),
    .we_i       ( we                                                                          ),
    .addr_i     ( addr[$clog2(NUM_WORDS)-1+$clog2(AXI_DATA_WIDTH/8):$clog2(AXI_DATA_WIDTH/8)] ),
    .wdata_i    ( wdata                                                                       ),
    .be_i       ( be                                                                          ),
    .rdata_o    ( rdata                                                                       )
  );
  typedef logic [ariane_soc::NrRegion-1:0][ariane_soc::NB_PERIPHERALS-1:0][AXI_ADDRESS_WIDTH-1:0] addr_map_t;
  axi_node_intf_wrap #(
    .NB_SLAVE           ( ariane_soc::NrSlaves       ),
    .NB_MASTER          ( ariane_soc::NB_PERIPHERALS ),
    .NB_REGION          ( ariane_soc::NrRegion       ),
    .AXI_ADDR_WIDTH     ( AXI_ADDRESS_WIDTH          ),
    .AXI_DATA_WIDTH     ( AXI_DATA_WIDTH             ),
    .AXI_USER_WIDTH     ( AXI_USER_WIDTH             ),
    .AXI_ID_WIDTH       ( ariane_soc::IdWidth        )
  ) i_axi_xbar (
    .clk          ( clk_i      ),
    .rst_n        ( ndmreset_n ),
    .test_en_i    ( test_en    ),
    .slave        ( slave      ),
    .master       ( master     ),
    .start_addr_i ({
      ariane_soc::DebugBase,
      ariane_soc::ROMBase,
      ariane_soc::CLINTBase,
      ariane_soc::PLICBase,
      ariane_soc::UARTBase,
      ariane_soc::TimerBase,
      ariane_soc::SPIBase,
      ariane_soc::EthernetBase,
      ariane_soc::GPIOBase,
      ariane_soc::DRAMBase
    }),
    .end_addr_i   ({
      ariane_soc::DebugBase    + ariane_soc::DebugLength - 1,
      ariane_soc::ROMBase      + ariane_soc::ROMLength - 1,
      ariane_soc::CLINTBase    + ariane_soc::CLINTLength - 1,
      ariane_soc::PLICBase     + ariane_soc::PLICLength - 1,
      ariane_soc::UARTBase     + ariane_soc::UARTLength - 1,
      ariane_soc::TimerBase    + ariane_soc::TimerLength - 1,
      ariane_soc::SPIBase      + ariane_soc::SPILength - 1,
      ariane_soc::EthernetBase + ariane_soc::EthernetLength -1,
      ariane_soc::GPIOBase     + ariane_soc::GPIOLength - 1,
      ariane_soc::DRAMBase     + ariane_soc::DRAMLength - 1
    }),
    .valid_rule_i (ariane_soc::ValidRule)
  );
  logic ipi;
  logic timer_irq;
  ariane_axi_soc::req_t    axi_clint_req;
  ariane_axi_soc::resp_t   axi_clint_resp;
  clint #(
    .AXI_ADDR_WIDTH ( AXI_ADDRESS_WIDTH        ),
    .AXI_DATA_WIDTH ( AXI_DATA_WIDTH           ),
    .AXI_ID_WIDTH   ( ariane_soc::IdWidthSlave ),
    .NR_CORES       ( 1                        )
  ) i_clint (
    .clk_i       ( clk_i          ),
    .rst_ni      ( ndmreset_n     ),
    .testmode_i  ( test_en        ),
    .axi_req_i   ( axi_clint_req  ),
    .axi_resp_o  ( axi_clint_resp ),
    .rtc_i       ( rtc_i          ),
    .timer_irq_o ( timer_irq      ),
    .ipi_o       ( ipi            )
  );
  axi_slave_connect i_axi_slave_connect_clint (
    .axi_req_o(axi_clint_req),
    .axi_resp_i(axi_clint_resp),
    .slave(master[ariane_soc::CLINT])
  );
  logic tx, rx;
  logic [1:0] irqs;
  ariane_peripherals #(
    .AxiAddrWidth ( AXI_ADDRESS_WIDTH        ),
    .AxiDataWidth ( AXI_DATA_WIDTH           ),
    .AxiIdWidth   ( ariane_soc::IdWidthSlave ),
    .InclUART     ( 1'b0                     ),
    .InclSPI      ( 1'b0                     ),
    .InclEthernet ( 1'b0                     )
  ) i_ariane_peripherals (
    .clk_i     ( clk_i                        ),
    .rst_ni    ( ndmreset_n                   ),
    .plic      ( master[ariane_soc::PLIC]     ),
    .uart      ( master[ariane_soc::UART]     ),
    .spi       ( master[ariane_soc::SPI]      ),
    .ethernet  ( master[ariane_soc::Ethernet] ),
    .timer     ( master[ariane_soc::Timer]    ),
    .irq_o     ( irqs                         ),
    .rx_i      ( rx                           ),
    .tx_o      ( tx                           ),
    .eth_txck  ( ),
    .eth_rxck  ( ),
    .eth_rxctl ( ),
    .eth_rxd   ( ),
    .eth_rst_n ( ),
    .eth_tx_en ( ),
    .eth_txd   ( ),
    .phy_mdio  ( ),
    .eth_mdc   ( ),
    .mdio      ( ),
    .mdc       ( ),
    .spi_clk_o ( ),
    .spi_mosi  ( ),
    .spi_miso  ( ),
    .spi_ss    ( )
  );
  uart_bus #(.BAUD_RATE(115200), .PARITY_EN(0)) i_uart_bus (.rx(tx), .tx(rx), .rx_en(1'b1));
  ariane_axi_soc::req_t    axi_ariane_req;
  ariane_axi_soc::resp_t   axi_ariane_resp;
  ariane_dummy #(
    .ArianeCfg  ( ariane_soc::ArianeSocCfg )
  ) i_ariane (
    .clk_i                ( clk_i               ),
    .rst_ni               ( ndmreset_n          ),
    .boot_addr_i          ( ariane_soc::ROMBase ),  
    .hart_id_i            ( '0                  ),
    .irq_i                ( irqs                ),
    .ipi_i                ( ipi                 ),
    .time_irq_i           ( timer_irq           ),
    .debug_req_i          ( debug_req_core      ),
    .axi_req_o            ( axi_ariane_req      ),
    .axi_resp_i           ( axi_ariane_resp     )
  );
  axi_master_connect i_axi_master_connect_ariane (
    .axi_req_i(axi_ariane_req),
    .axi_resp_o(axi_ariane_resp),
    .master(slave[0])
  );
  always_ff @(posedge clk_i) begin : p_assert
    if (axi_ariane_req.r_ready &&
      axi_ariane_resp.r_valid &&
      axi_ariane_resp.r.resp inside {axi_pkg::RESP_DECERR, axi_pkg::RESP_SLVERR}) begin
      $warning("R Response Errored");
    end
    if (axi_ariane_req.b_ready &&
      axi_ariane_resp.b_valid &&
      axi_ariane_resp.b.resp inside {axi_pkg::RESP_DECERR, axi_pkg::RESP_SLVERR}) begin
      $warning("B Response Errored");
    end
  end
endmodule
module axi_AR_allocator
#(
      parameter AXI_ADDRESS_W  = 32,
      parameter AXI_USER_W     = 6,
      parameter N_TARG_PORT    = 7,
      parameter LOG_N_TARG     = $clog2(N_TARG_PORT),
      parameter AXI_ID_IN      = 16,
      parameter AXI_ID_OUT     = AXI_ID_IN + $clog2(N_TARG_PORT)
)
(
      input  logic                                          clk,
      input  logic                                          rst_n,
      input  logic [N_TARG_PORT-1:0][AXI_ID_IN-1:0]         arid_i,
      input  logic [N_TARG_PORT-1:0][AXI_ADDRESS_W-1:0]     araddr_i,
      input  logic [N_TARG_PORT-1:0][ 7:0]                  arlen_i,    
      input  logic [N_TARG_PORT-1:0][ 2:0]                  arsize_i,   
      input  logic [N_TARG_PORT-1:0][ 1:0]                  arburst_i,  
      input  logic [N_TARG_PORT-1:0]                        arlock_i,   
      input  logic [N_TARG_PORT-1:0][ 3:0]                  arcache_i,
      input  logic [N_TARG_PORT-1:0][ 2:0]                  arprot_i,
      input  logic [N_TARG_PORT-1:0][ 3:0]                  arregion_i,     
      input  logic [N_TARG_PORT-1:0][ AXI_USER_W-1:0]       aruser_i,       
      input  logic [N_TARG_PORT-1:0][ 3:0]                  arqos_i,        
      input  logic [N_TARG_PORT-1:0]                        arvalid_i,  
      output logic [N_TARG_PORT-1:0]                        arready_o,  
      output  logic [AXI_ID_OUT-1:0]                        arid_o,
      output  logic [AXI_ADDRESS_W-1:0]                     araddr_o,
      output  logic [ 7:0]                                  arlen_o,    
      output  logic [ 2:0]                                  arsize_o,   
      output  logic [ 1:0]                                  arburst_o,  
      output  logic                                         arlock_o,   
      output  logic [ 3:0]                                  arcache_o,
      output  logic [ 2:0]                                  arprot_o,
      output  logic [ 3:0]                                  arregion_o,     
      output  logic [ AXI_USER_W-1:0]                       aruser_o,       
      output  logic [ 3:0]                                  arqos_o,        
      output  logic                                         arvalid_o,  
      input   logic                                         arready_i  
);
localparam      AUX_WIDTH = AXI_ID_IN + AXI_ADDRESS_W + 8 + 3 + 2 + 1 + 4 + 3 +    4 + AXI_USER_W + 4;
logic [N_TARG_PORT-1:0][AUX_WIDTH-1:0]                          AUX_VECTOR_IN;
logic [AUX_WIDTH-1:0]                                           AUX_VECTOR_OUT;
logic [N_TARG_PORT-1:0][LOG_N_TARG+N_TARG_PORT-1:0]             ID_in;
logic [LOG_N_TARG+N_TARG_PORT-1:0]                              ID_int;
genvar i;
assign        { arqos_o, aruser_o, arregion_o, arprot_o, arcache_o, arlock_o, arburst_o, arsize_o, arlen_o, araddr_o, arid_o[AXI_ID_IN-1:0] }  =  AUX_VECTOR_OUT;
assign        arid_o[AXI_ID_OUT-1:AXI_ID_IN] = ID_int[LOG_N_TARG+N_TARG_PORT-1:N_TARG_PORT];
generate
  for(i=0;i<N_TARG_PORT;i++)
  begin : AUX_VECTOR_BINDING
      assign AUX_VECTOR_IN[i] =  { arqos_i[i], aruser_i[i], arregion_i[i], arprot_i[i], arcache_i[i], arlock_i[i], arburst_i[i], arsize_i[i], arlen_i[i], araddr_i[i], arid_i[i]};
  end
  for(i=0;i<N_TARG_PORT;i++)
  begin : ID_VECTOR_BINDING
      assign ID_in[i][N_TARG_PORT-1:0] =  2**i;                    
      assign ID_in[i][LOG_N_TARG+N_TARG_PORT-1:N_TARG_PORT] =  i;  
  end
endgenerate
  axi_node_arbiter #(
    .AUX_WIDTH  (AUX_WIDTH),
    .ID_WIDTH   (LOG_N_TARG+N_TARG_PORT),
    .N_MASTER   (N_TARG_PORT)
  ) i_arbiter (
    .clk_i        (clk),
    .rst_ni       (rst_n),
    .inp_id_i     (ID_in),
    .inp_aux_i    (AUX_VECTOR_IN),
    .inp_valid_i  (arvalid_i),
    .inp_ready_o  (arready_o),
    .oup_id_o     (ID_int),
    .oup_aux_o    (AUX_VECTOR_OUT),
    .oup_valid_o  (arvalid_o),
    .oup_ready_i  (arready_i)
  );
endmodule
module axi_AW_allocator #(
  parameter AXI_ADDRESS_W = 32,
  parameter AXI_USER_W    = 6,
  parameter N_TARG_PORT   = 7,
  parameter LOG_N_TARG    = $clog2(N_TARG_PORT),
  parameter AXI_ID_IN     = 16,
  parameter AXI_ID_OUT    = AXI_ID_IN + $clog2(N_TARG_PORT)
) (
  input  logic                                        clk,
  input  logic                                        rst_n,
  input  logic [N_TARG_PORT-1:0][AXI_ID_IN-1:0]       awid_i,
  input  logic [N_TARG_PORT-1:0][AXI_ADDRESS_W-1:0]   awaddr_i,
  input  logic [N_TARG_PORT-1:0][7:0]                 awlen_i,
  input  logic [N_TARG_PORT-1:0][2:0]                 awsize_i,
  input  logic [N_TARG_PORT-1:0][1:0]                 awburst_i,
  input  logic [N_TARG_PORT-1:0]                      awlock_i,
  input  logic [N_TARG_PORT-1:0][3:0]                 awcache_i,
  input  logic [N_TARG_PORT-1:0][2:0]                 awprot_i,
  input  logic [N_TARG_PORT-1:0][3:0]                 awregion_i,
  input  logic [N_TARG_PORT-1:0][5:0]                 awatop_i,
  input  logic [N_TARG_PORT-1:0][AXI_USER_W-1:0]      awuser_i,
  input  logic [N_TARG_PORT-1:0][3:0]                 awqos_i,
  input  logic [N_TARG_PORT-1:0]                      awvalid_i,
  output logic [N_TARG_PORT-1:0]                      awready_o,
  output logic [AXI_ID_OUT-1:0]                       awid_o,
  output logic [AXI_ADDRESS_W-1:0]                    awaddr_o,
  output logic [7:0]                                  awlen_o,
  output logic [2:0]                                  awsize_o,
  output logic [1:0]                                  awburst_o,
  output logic                                        awlock_o,
  output logic [3:0]                                  awcache_o,
  output logic [2:0]                                  awprot_o,
  output logic [3:0]                                  awregion_o,
  output logic [5:0]                                  awatop_o,
  output logic [AXI_USER_W-1:0]                       awuser_o,
  output logic [3:0]                                  awqos_o,
  output logic                                        awvalid_o,
  input  logic                                        awready_i,
  output logic                                        push_ID_o,
  output logic [LOG_N_TARG+N_TARG_PORT-1:0]           ID_o,  
  input  logic                                        grant_FIFO_ID_i
);
  typedef struct packed {
    logic [AXI_ID_IN-1:0]     id;
    logic [AXI_ADDRESS_W-1:0] addr;
    logic [7:0]               len;
    logic [2:0]               size;
    logic [1:0]               burst;
    logic                     lock;
    logic [3:0]               cache;
    logic [2:0]               prot;
    logic [3:0]               region;
    logic [5:0]               atop;
    logic [AXI_USER_W-1:0]    user;
    logic [3:0]               qos;
  } aw_t;
  logic                                                 awvalid_int;
  aw_t [N_TARG_PORT-1:0]  aw_inp;
  aw_t                    aw_oup;
  logic [N_TARG_PORT-1:0][LOG_N_TARG+N_TARG_PORT-1:0]   id_int;
  logic [N_TARG_PORT-1:0]                               awready_int;
  assign awid_o     = {ID_o[LOG_N_TARG+N_TARG_PORT-1:N_TARG_PORT], aw_oup.id};
  assign awaddr_o   = aw_oup.addr;
  assign awlen_o    = aw_oup.len;
  assign awsize_o   = aw_oup.size;
  assign awburst_o  = aw_oup.burst;
  assign awlock_o   = aw_oup.lock;
  assign awcache_o  = aw_oup.cache;
  assign awprot_o   = aw_oup.prot;
  assign awregion_o = aw_oup.region;
  assign awatop_o   = aw_oup.atop;
  assign awuser_o   = aw_oup.user;
  assign awqos_o    = aw_oup.qos;
  for (genvar i = 0; i < N_TARG_PORT; i++) begin: gen_aw_inp
    assign aw_inp[i].id     = awid_i[i];
    assign aw_inp[i].addr   = awaddr_i[i];
    assign aw_inp[i].len    = awlen_i[i];
    assign aw_inp[i].size   = awsize_i[i];
    assign aw_inp[i].burst  = awburst_i[i];
    assign aw_inp[i].lock   = awlock_i[i];
    assign aw_inp[i].cache  = awcache_i[i];
    assign aw_inp[i].prot   = awprot_i[i];
    assign aw_inp[i].region = awregion_i[i];
    assign aw_inp[i].atop   = awatop_i[i];
    assign aw_inp[i].user   = awuser_i[i];
    assign aw_inp[i].qos    = awqos_i[i];
  end
  assign awready_o  = {N_TARG_PORT{grant_FIFO_ID_i}} & awready_int;
  assign awvalid_o  = awvalid_int & grant_FIFO_ID_i;
  assign push_ID_o  = awvalid_o & awready_i & grant_FIFO_ID_i;
  for (genvar i = 0; i < N_TARG_PORT; i++) begin: gen_id_int
    assign id_int[i][N_TARG_PORT-1:0] = 2**i;                      
    assign id_int[i][LOG_N_TARG+N_TARG_PORT-1:N_TARG_PORT] = i;    
  end
  axi_node_arbiter #(
    .AUX_WIDTH  ($bits(aw_t)),
    .ID_WIDTH   (LOG_N_TARG+N_TARG_PORT),
    .N_MASTER   (N_TARG_PORT)
  ) i_arbiter (
    .clk_i        (clk),
    .rst_ni       (rst_n),
    .inp_id_i     (id_int),
    .inp_aux_i    (aw_inp),
    .inp_valid_i  (awvalid_i),
    .inp_ready_o  (awready_int),
    .oup_id_o     (ID_o),
    .oup_aux_o    (aw_oup),
    .oup_valid_o  (awvalid_int),
    .oup_ready_i  (awready_i)
  );
endmodule
module axi_BR_allocator #(
    parameter                   AXI_USER_W     = 6,
    parameter                   N_INIT_PORT    = 1,
    parameter                   N_TARG_PORT    = 7,
    parameter                   AXI_DATA_W     = 64,
    parameter                   AXI_ID_IN      = 16,
    parameter                   LOG_N_TARG     = $clog2(N_TARG_PORT),
    parameter                   LOG_N_INIT     = $clog2(N_INIT_PORT),
    parameter                   AXI_ID_OUT     = AXI_ID_IN + $clog2(N_TARG_PORT)
)(
  input  logic                                                          clk,
  input  logic                                                          rst_n,
  input  logic [N_INIT_PORT-1:0][AXI_ID_OUT-1:0]                        rid_i,
  input  logic [N_INIT_PORT-1:0][AXI_DATA_W-1:0]                        rdata_i,
  input  logic [N_INIT_PORT-1:0][ 1:0]                                  rresp_i,
  input  logic [N_INIT_PORT-1:0]                                        rlast_i,    
  input  logic [N_INIT_PORT-1:0][AXI_USER_W-1:0]                        ruser_i,    
  input  logic [N_INIT_PORT-1:0]                                        rvalid_i,   
  output logic [N_INIT_PORT-1:0]                                        rready_o,    
  output  logic [AXI_ID_IN-1:0]                                         rid_o,
  output  logic [AXI_DATA_W-1:0]                                        rdata_o,
  output  logic [ 1:0]                                                  rresp_o,
  output  logic                                                         rlast_o,    
  output  logic [AXI_USER_W-1:0]                                        ruser_o,    
  output  logic                                                         rvalid_o,   
  input   logic                                                         rready_i,    
  input   logic                                                         incr_req_i,
  output  logic                                                         full_counter_o,
  output  logic                                                         outstanding_trans_o,
  input   logic                                                         error_req_i,
  output  logic                                                         error_gnt_o,
  input   logic [ 7:0]                                                  error_len_i,
  input   logic [AXI_USER_W-1:0]                                        error_user_i,
  input   logic [AXI_ID_IN-1:0]                                         error_id_i,
  input  logic                                                          sample_ardata_info_i
);
localparam      AUX_WIDTH = AXI_DATA_W + 2 + 1 + AXI_USER_W;
logic [N_INIT_PORT-1:0][AUX_WIDTH-1:0]                          AUX_VECTOR_IN;
logic [AUX_WIDTH-1:0]                                           AUX_VECTOR_OUT;
logic [N_INIT_PORT-1:0][AXI_ID_IN-1:0]                          rid_int;
genvar i;
logic   [9:0]                                                   outstanding_counter;
logic                                                           decr_req;
enum logic [1:0]                                                {OPERATIVE, ERROR_SINGLE, ERROR_BURST, GO_ERROR} CS, NS;
logic   [7:0]                                                   CounterBurstCS, CounterBurstNS;
logic   [ 7:0]                                                  error_len_S;
logic   [AXI_USER_W-1:0]                                        error_user_S;
logic   [AXI_ID_IN-1:0]                                         error_id_S;
logic [AXI_ID_IN-1:0]                                           arb_rid;
logic [AXI_DATA_W-1:0]                                          arb_rdata;
logic [1:0]                                                     arb_rresp;
logic                                                           arb_rlast;
logic [AXI_USER_W-1:0]                                          arb_ruser;
logic                                                           arb_rvalid;
logic                                                           arb_rready;
assign outstanding_trans_o = (outstanding_counter == '0) ? 1'b0 : 1'b1;
assign decr_req = arb_rvalid & arb_rready & arb_rlast;
assign full_counter_o = (outstanding_counter == '1) ? 1'b1 : 1'b0;
always_ff @(posedge clk, negedge rst_n)
begin
    if(rst_n == 1'b0)
      outstanding_counter  <= '0;
    else
    begin
      case({incr_req_i, decr_req})
        2'b00: begin  outstanding_counter  <= outstanding_counter; end
        2'b01:
        begin
                if(outstanding_counter != '0)
                    outstanding_counter  <= outstanding_counter - 1'b1;
                else
                    outstanding_counter  <= '0;
        end
        2'b10:
        begin
                if(outstanding_counter != '1)
                    outstanding_counter  <= outstanding_counter + 1'b1;
                else
                    outstanding_counter  <= '1;
        end
        2'b11: begin  outstanding_counter  <= outstanding_counter; end
      endcase
    end
end
always_ff @(posedge clk, negedge rst_n)
begin
  if(rst_n == 1'b0)
  begin
    CS             <= OPERATIVE;
    CounterBurstCS <= '0;
    error_user_S   <= '0;
    error_id_S     <= '0;
    error_len_S    <= '0;
  end
  else
  begin
    CS <= NS;
    CounterBurstCS <= CounterBurstNS;
    if(sample_ardata_info_i)
    begin
        error_user_S  <= error_user_i;
        error_id_S    <= error_id_i;
        error_len_S   <= error_len_i;
    end
  end
end
always_comb
begin
  rid_o       = arb_rid;
  rdata_o     = arb_rdata;
  rresp_o     = arb_rresp;
  rlast_o     = arb_rlast;
  ruser_o     = arb_ruser;
  rvalid_o    = arb_rvalid;
  arb_rready  = rready_i;
  CounterBurstNS = CounterBurstCS;
  error_gnt_o      = 1'b0;
  case(CS)
    OPERATIVE :
    begin
        CounterBurstNS   = '0;
        arb_rready       = rready_i;
        error_gnt_o      = 1'b0;
        if((error_req_i == 1'b1))
        begin
          if(outstanding_trans_o == 1'b0)
          begin
              if(error_len_i == '0)
                NS = ERROR_SINGLE;
              else
                NS = ERROR_BURST;
          end
          else
          begin
              NS = GO_ERROR;
          end
        end
        else
        begin
          NS = OPERATIVE;
        end
    end
    GO_ERROR:
    begin
          CounterBurstNS   = '0;
          arb_rready       = rready_i;
          error_gnt_o      = 1'b0;
          if(outstanding_trans_o == 1'b0)
          begin
              if(error_len_S == '0)
                NS = ERROR_SINGLE;
              else
                NS = ERROR_BURST;
          end
          else
          begin
              NS = GO_ERROR;
          end
    end
    ERROR_SINGLE :
    begin
        arb_rready = 1'b0;
        CounterBurstNS = '0;
        error_gnt_o = 1'b1;
        rresp_o     = axi_pkg::RESP_DECERR;
        rdata_o     = { (AXI_DATA_W/32) {32'hDEADBEEF}};
        rvalid_o    = 1'b1;
        ruser_o     = error_user_S;
        rlast_o     = 1'b1;
        rid_o       = error_id_S;
        if(rready_i)
          NS = OPERATIVE;
        else
          NS = ERROR_SINGLE;
    end
    ERROR_BURST :
    begin
        arb_rready = 1'b0;
        rresp_o     = axi_pkg::RESP_DECERR;
        rdata_o     = { (AXI_DATA_W/32) {32'hDEADBEEF}};
        rvalid_o    = 1'b1;
        ruser_o     = error_user_S;
        rid_o       = error_id_S;
        if(rready_i)
        begin
            if(CounterBurstCS < error_len_i)
            begin
              CounterBurstNS = CounterBurstCS + 1'b1;
              error_gnt_o    = 1'b0;
              rlast_o        = 1'b0;
              NS             = ERROR_BURST;
            end
            else
            begin
              error_gnt_o    = 1'b1;
              CounterBurstNS = '0;
              NS             = OPERATIVE;
              rlast_o        = 1'b1;
            end
        end
        else
        begin
            NS = ERROR_BURST;
            error_gnt_o      = 1'b0;
        end
    end
    default :
    begin
        CounterBurstNS = '0;
        NS             = OPERATIVE;
        error_gnt_o      = 1'b0;
    end
  endcase
end
assign {arb_ruser, arb_rlast, arb_rresp, arb_rdata} = AUX_VECTOR_OUT;
generate
  for(i=0;i<N_INIT_PORT;i++)
  begin : AUX_VECTOR_BINDING
      assign AUX_VECTOR_IN[i] =  { ruser_i[i], rlast_i[i], rresp_i[i], rdata_i[i]};
  end
  for(i=0;i<N_INIT_PORT;i++)
  begin : RID_VECTOR_BINDING
      assign rid_int[i] =  rid_i[i][AXI_ID_IN-1:0];
  end
if(N_INIT_PORT == 1)
begin : DIRECT_BINDING
    assign arb_rvalid = rvalid_i;
    assign AUX_VECTOR_OUT = AUX_VECTOR_IN;
    assign arb_rid = rid_int;
    assign rready_o = arb_rready;
end
else
begin : ARBITER
  axi_node_arbiter #(
    .AUX_WIDTH  (AUX_WIDTH),
    .ID_WIDTH   (AXI_ID_IN),
    .N_MASTER   (N_INIT_PORT)
  ) i_arbiter (
    .clk_i        (clk),
    .rst_ni       (rst_n),
    .inp_id_i     (rid_int),
    .inp_aux_i    (AUX_VECTOR_IN),
    .inp_valid_i  (rvalid_i),
    .inp_ready_o  (rready_o),
    .oup_id_o     (arb_rid),
    .oup_aux_o    (AUX_VECTOR_OUT),
    .oup_valid_o  (arb_rvalid),
    .oup_ready_i  (arb_rready)
  );
end
endgenerate
endmodule
module axi_BW_allocator #(
    parameter                   AXI_USER_W     = 6,
    parameter                   N_INIT_PORT    = 1,
    parameter                   N_TARG_PORT    = 7,
    parameter                   AXI_DATA_W     = 64,
    parameter                   AXI_ID_IN      = 16,
    parameter                   AXI_ID_OUT     = AXI_ID_IN + $clog2(N_TARG_PORT)
)(
  input  logic                                                          clk,
  input  logic                                                          rst_n,
  input  logic [N_INIT_PORT-1:0][AXI_ID_OUT-1:0]                        bid_i,
  input  logic [N_INIT_PORT-1:0][ 1:0]                                  bresp_i,
  input  logic [N_INIT_PORT-1:0][AXI_USER_W-1:0]                        buser_i,    
  input  logic [N_INIT_PORT-1:0]                                        bvalid_i,   
  output logic [N_INIT_PORT-1:0]                                        bready_o,    
  output  logic [AXI_ID_IN-1:0]                                         bid_o,
  output  logic [ 1:0]                                                  bresp_o,
  output  logic [AXI_USER_W-1:0]                                        buser_o,    
  output  logic                                                         bvalid_o,   
  input   logic                                                         bready_i,    
  input   logic                                                         incr_req_i,
  output  logic                                                         full_counter_o,
  output  logic                                                         outstanding_trans_o,
  input   logic                                                         sample_awdata_info_i,
  input   logic                                                         error_req_i,
  output  logic                                                         error_gnt_o,
  input   logic [AXI_USER_W-1:0]                                        error_user_i,
  input   logic [AXI_ID_IN-1:0]                                         error_id_i
);
localparam      AUX_WIDTH = 2 + AXI_USER_W;
logic [N_INIT_PORT-1:0][AUX_WIDTH-1:0]                                  AUX_VECTOR_IN;
logic [AUX_WIDTH-1:0]                                                   AUX_VECTOR_OUT;
logic [N_INIT_PORT-1:0][AXI_ID_IN-1:0]                                  bid_int;
genvar i;
logic [9:0]                                                             outstanding_counter;
logic                                                                   decr_req;
logic   [AXI_USER_W-1:0]                                                error_user_S;
logic   [AXI_ID_IN-1:0]                                                 error_id_S;
enum logic [1:0]                                                {OPERATIVE, ERROR_SINGLE, ERROR_BURST} CS, NS;
logic [AXI_ID_IN-1:0]                                                   arb_bid;
logic [1:0]                                                             arb_bresp;
logic [AXI_USER_W-1:0]                                                  arb_buser;
logic                                                                   arb_bvalid;
logic                                                                   arb_bready;
assign {arb_buser, arb_bresp} = AUX_VECTOR_OUT;
assign outstanding_trans_o = (outstanding_counter == '0) ? 1'b0 : 1'b1;
assign decr_req = bvalid_o & bready_i;
assign full_counter_o = (outstanding_counter == '1) ? 1'b1 : 1'b0;
always_ff @(posedge clk, negedge rst_n)
begin
    if(rst_n == 1'b0)
      outstanding_counter  <= '0;
    else
    begin
      case({incr_req_i, decr_req})
        2'b00: begin  outstanding_counter  <= outstanding_counter; end
        2'b01:
        begin
                if(outstanding_counter != '0)
                    outstanding_counter  <= outstanding_counter - 1'b1;
                else
                    outstanding_counter  <= '0;
        end
        2'b10:
        begin
                if(outstanding_counter != '1)
                    outstanding_counter  <= outstanding_counter + 1'b1;
                else
                    outstanding_counter  <= '1;
        end
        2'b11: begin  outstanding_counter  <= outstanding_counter; end
      endcase
    end
end
always_ff @(posedge clk, negedge rst_n)
begin
  if(rst_n == 1'b0)
  begin
    error_user_S <= '0;
    error_id_S   <= '0;
  end
  else
  begin
    if(sample_awdata_info_i)
    begin
      error_user_S <= error_user_i;
      error_id_S   <= error_id_i;
    end
  end
end
always_ff @(posedge clk, negedge rst_n)
begin
  if(rst_n == 1'b0)
  begin
    CS <= OPERATIVE;
  end
  else
  begin
    CS <= NS;
  end
end
always_comb
begin
  bid_o       = arb_bid;
  bresp_o     = arb_bresp;
  buser_o     = arb_buser;
  bvalid_o    = arb_bvalid;
  arb_bready  = bready_i;
  error_gnt_o      = 1'b0;
  case(CS)
    OPERATIVE :
    begin
        error_gnt_o = 1'b0;
        if((error_req_i == 1'b1) && (outstanding_trans_o == 1'b0))
        begin
            NS = ERROR_SINGLE;
        end
        else
        begin
          NS = OPERATIVE;
        end
    end
    ERROR_SINGLE :
    begin
        arb_bready  = 1'b0;
        error_gnt_o = 1'b1;
        bresp_o     = axi_pkg::RESP_DECERR;
        bvalid_o    = 1'b1;
        buser_o     = error_user_S;
        bid_o       = error_id_S;
        if(bready_i)
          NS = OPERATIVE;
        else
          NS = ERROR_SINGLE;
    end
    default :
    begin
        NS             = OPERATIVE;
        error_gnt_o      = 1'b0;
    end
  endcase
end
generate
  for(i=0;i<N_INIT_PORT;i++)
  begin : AUX_VECTOR_BINDING
      assign AUX_VECTOR_IN[i] =  { buser_i[i], bresp_i[i]};
  end
  for(i=0;i<N_INIT_PORT;i++)
  begin : BID_VECTOR_BINDING
      assign bid_int[i] =  bid_i[i][AXI_ID_IN-1:0];
  end
if(N_INIT_PORT == 1)
begin : DIRECT_BINDING
    assign arb_bvalid     = bvalid_i;
    assign AUX_VECTOR_OUT = AUX_VECTOR_IN;
    assign arb_bid        = bid_int;
    assign bready_o       = bready_i;
end
else
begin : ARBITER
  axi_node_arbiter #(
    .AUX_WIDTH  (AUX_WIDTH),
    .ID_WIDTH   (AXI_ID_IN),
    .N_MASTER   (N_INIT_PORT)
  ) i_arbiter (
    .clk_i        (clk),
    .rst_ni       (rst_n),
    .inp_id_i     (bid_int),
    .inp_aux_i    (AUX_VECTOR_IN),
    .inp_valid_i  (bvalid_i),
    .inp_ready_o  (bready_o),
    .oup_id_o     (arb_bid),
    .oup_aux_o    (AUX_VECTOR_OUT),
    .oup_valid_o  (arb_bvalid),
    .oup_ready_i  (arb_bready)
  );
end
endgenerate
endmodule
module axi_DW_allocator
#(
    parameter                   AXI_USER_W     = 6,
    parameter                   N_TARG_PORT    = 7,
    parameter                   LOG_N_TARG     = $clog2(N_TARG_PORT),
    parameter                   FIFO_DEPTH     = 8,
    parameter                   AXI_DATA_W     = 64,
    parameter                   AXI_NUMBYTES   = AXI_DATA_W/8
)
(
  input  logic                                                          clk,
  input  logic                                                          rst_n,
  input  logic                                                          test_en_i,
  input  logic [N_TARG_PORT-1:0] [AXI_DATA_W-1:0]                       wdata_i,
  input  logic [N_TARG_PORT-1:0] [AXI_NUMBYTES-1:0]                     wstrb_i,    
  input  logic [N_TARG_PORT-1:0]                                        wlast_i,    
  input  logic [N_TARG_PORT-1:0][AXI_USER_W-1:0]                        wuser_i,    
  input  logic [N_TARG_PORT-1:0]                                        wvalid_i,   
  output logic [N_TARG_PORT-1:0]                                        wready_o,   
  output logic  [AXI_DATA_W-1:0]                                        wdata_o,
  output logic  [AXI_NUMBYTES-1:0]                                      wstrb_o,    
  output logic                                                          wlast_o,    
  output logic  [AXI_USER_W-1:0]                                        wuser_o,    
  output logic                                                          wvalid_o,   
  input  logic                                                          wready_i,   
  input  logic                                                          push_ID_i,
  input  logic [LOG_N_TARG+N_TARG_PORT-1:0]                             ID_i,  
  output logic                                                          grant_FIFO_ID_o
);
localparam      AUX_WIDTH = AXI_DATA_W + AXI_NUMBYTES + 1 + AXI_USER_W;
logic                                                           pop_from_ID_FIFO;
logic                                                           valid_ID;
logic [LOG_N_TARG+N_TARG_PORT-1:0]                              ID_int;
logic [LOG_N_TARG-1:0]                                          ID_int_BIN;
logic [N_TARG_PORT-1:0]                                         ID_int_OH;
logic [AUX_WIDTH-1:0]                                           AUX_VECTOR_OUT;
logic [N_TARG_PORT-1:0][AUX_WIDTH-1:0]                          AUX_VECTOR_IN;
enum logic { SINGLE_IDLE, BURST }                               CS, NS;
genvar i;
generate
  for(i=0; i<N_TARG_PORT; i++)
  begin : AUX_VECTOR_BINDING
      assign  AUX_VECTOR_IN[i] = { wdata_i[i], wstrb_i[i], wlast_i[i], wuser_i[i] };
  end
endgenerate
assign {wdata_o,wstrb_o,wlast_o,wuser_o} = AUX_VECTOR_OUT;
logic empty, full;
fifo_v2 #(
    .FALL_THROUGH(1'b0),
    .DATA_WIDTH(LOG_N_TARG+N_TARG_PORT),
    .DEPTH(FIFO_DEPTH)
)
MASTER_ID_FIFO
(
    .clk_i        (clk              ),
    .rst_ni       (rst_n            ),
    .flush_i      ( 1'b0            ),
    .testmode_i   (test_en_i        ),
    .alm_empty_o  ( ),  
    .alm_full_o   ( ),  
    .data_i       (ID_i             ),
    .push_i       (push_ID_i        ),
    .full_o       (full             ),
    .data_o       (ID_int           ),
    .empty_o      (empty            ),
    .pop_i        (pop_from_ID_FIFO )
);
assign valid_ID = ~empty;
assign grant_FIFO_ID_o = ~full;
  assign  ID_int_BIN = ID_int[LOG_N_TARG+N_TARG_PORT-1:N_TARG_PORT];
  assign  ID_int_OH  = ID_int[N_TARG_PORT-1:0];
  always_ff @(posedge clk, negedge rst_n)
  begin : UPDATE_STATE_FSM
      if(rst_n == 1'b0)
      begin
        CS             <= SINGLE_IDLE;
      end
      else
      begin
        CS             <= NS;
      end
  end
  always_comb
  begin : NEXT_STATE_FSM
      pop_from_ID_FIFO = 1'b0;
      wvalid_o          = 1'b0;
      wready_o          = '0;
      case(CS)
          SINGLE_IDLE :
          begin : _CS_IN_SINGLE_IDLE
                if(valid_ID)
                begin : _valid_ID
                      wvalid_o   = wvalid_i[ID_int_BIN] ;
                      wready_o   = {N_TARG_PORT{wready_i}} & ID_int_OH;
                      if(wvalid_i[ID_int_BIN] & wready_i)
                      begin : _granted_request
                        if(wlast_i[ID_int_BIN])
                        begin : _last_packet
                          NS               = SINGLE_IDLE;
                          pop_from_ID_FIFO = 1'b1;
                        end
                        else
                        begin : _payload_packet
                          NS = BURST;
                          pop_from_ID_FIFO = 1'b0;
                        end
                      end
                      else
                      begin : _not_granted_request
                           NS                 = SINGLE_IDLE;
                           pop_from_ID_FIFO   = 1'b0;
                      end
                end
                else  
                begin : _not_valid_ID
                           NS                 = SINGLE_IDLE;
                           pop_from_ID_FIFO   = 1'b0;
                           wvalid_o           = 1'b0;
                           wready_o           = '0;
                end
          end
          BURST : begin : _CS_IN_BUSRT
                      wvalid_o    = wvalid_i[ID_int_BIN];
                      wready_o   = {N_TARG_PORT{wready_i}} & ID_int_OH & {N_TARG_PORT{valid_ID}};
                      if(wvalid_i[ID_int_BIN] & wready_i)
                      begin
                        if(wlast_i[ID_int_BIN])
                        begin
                          NS               = SINGLE_IDLE;
                          pop_from_ID_FIFO = 1'b1;
                        end
                        else
                        begin
                          NS = BURST;
                          pop_from_ID_FIFO = 1'b0;
                        end
                      end
                      else
                      begin
                           NS                 = BURST;
                           pop_from_ID_FIFO   = 1'b0;
                      end
          end
          default : begin
                          NS               = SINGLE_IDLE;
                          pop_from_ID_FIFO = 1'b0;
                          wvalid_o         = 1'b0;
                          wready_o          = '0;
          end
          endcase
  end
  axi_multiplexer
  #(
    .DATA_WIDTH(AUX_WIDTH),
    .N_IN(N_TARG_PORT)
  )
  WRITE_DATA_MUX
  (
    .IN_DATA(AUX_VECTOR_IN),
    .OUT_DATA(AUX_VECTOR_OUT),
    .SEL(ID_int_BIN)
  );
endmodule
module axi_address_decoder_AR
#(
    parameter  ADDR_WIDTH     = 32,
    parameter  N_INIT_PORT    = 8,
    parameter  N_REGION       = 4
)
(
    input  logic                                                        clk,
    input  logic                                                        rst_n,
    input  logic                                                        arvalid_i,
    input  logic [ADDR_WIDTH-1:0]                                       araddr_i,
    output logic                                                        arready_o,
    output logic [N_INIT_PORT-1:0]                                      arvalid_o,
    input  logic [N_INIT_PORT-1:0]                                      arready_i,
    input  logic [N_REGION-1:0][N_INIT_PORT-1:0][ADDR_WIDTH-1:0]        START_ADDR_i,
    input  logic [N_REGION-1:0][N_INIT_PORT-1:0][ADDR_WIDTH-1:0]        END_ADDR_i,
    input  logic [N_REGION-1:0][N_INIT_PORT-1:0]                        enable_region_i,
    input  logic [N_INIT_PORT-1:0]                                      connectivity_map_i,
    output logic                                                        incr_req_o,
    input  logic                                                        full_counter_i,
    input  logic                                                        outstanding_trans_i,
    output logic                                                        error_req_o,
    input  logic                                                        error_gnt_i,
    output logic                                                        sample_ardata_info_o
);
  logic [N_INIT_PORT-1:0]                               match_region;
  logic [N_INIT_PORT:0]                                 match_region_masked;
  logic [N_REGION-1:0][N_INIT_PORT-1:0]                 match_region_int;
  logic [N_INIT_PORT-1:0][N_REGION-1:0]                 match_region_rev;
  logic                                                 arready_int;
  logic [N_INIT_PORT-1:0]                               arvalid_int;
  genvar i,j;
  enum logic    {OPERATIVE, ERROR} CS, NS;
  generate
      for(j=0;j<N_REGION;j++)
      begin
           for(i=0;i<N_INIT_PORT;i++)
           begin
              assign match_region_int[j][i]  =  (enable_region_i[j][i] == 1'b1 ) ? (araddr_i >= START_ADDR_i[j][i]) && (araddr_i <= END_ADDR_i[j][i]) : 1'b0;
           end
      end
      for(j=0;j<N_INIT_PORT;j++)
      begin
           for(i=0;i<N_REGION;i++)
           begin
             assign match_region_rev[j][i] = match_region_int[i][j];
           end
      end
      for(i=0;i<N_INIT_PORT;i++)
      begin
        assign match_region[i]  =  | match_region_rev[i];
      end
      assign match_region_masked[N_INIT_PORT-1:0] = match_region & connectivity_map_i;
      assign match_region_masked[N_INIT_PORT] = ~(|match_region_masked[N_INIT_PORT-1:0]);
  endgenerate
 always_comb
 begin
    if(arvalid_i)
    begin
        {error_req_o,arvalid_int} = {N_INIT_PORT+1{arvalid_i} } & match_region_masked;
    end
    else
    begin
        arvalid_int = '0;
        error_req_o = 1'b0;
    end
    arready_int = |({error_gnt_i,arready_i} & match_region_masked);
 end
  always_ff @(posedge clk, negedge rst_n)
  begin
    if(rst_n == 1'b0)
    begin
        CS <= OPERATIVE;
    end
    else
    begin
        CS <= NS;
    end
  end
  always_comb
  begin
      arready_o = 1'b0;
      arvalid_o = arvalid_int;
      sample_ardata_info_o = 1'b0;
      incr_req_o = 1'b0;
      case(CS)
          OPERATIVE:
          begin
              if(error_req_o)
              begin
                NS = ERROR;
                arready_o = 1'b1;  
                sample_ardata_info_o = 1'b1;
                arvalid_o = '0;
              end
              else
              begin
                NS = OPERATIVE;
                arready_o = arready_int;
                sample_ardata_info_o = 1'b0;
                incr_req_o = |(arvalid_o & arready_i);
                arvalid_o = arvalid_int;
              end
          end
          ERROR:
          begin
              arready_o = 1'b0;
              arvalid_o = '0;
              if(outstanding_trans_i)
              begin
                NS = ERROR;
              end
              else
              begin
                if(error_gnt_i)
                begin
                  NS        = OPERATIVE;
                end
                else
                begin
                  NS        = ERROR;
                end
              end
          end
          default :
          begin
              NS        = OPERATIVE;
              arready_o = arready_int;
          end
      endcase
  end
endmodule
module axi_address_decoder_AW
#(
    parameter  ADDR_WIDTH     = 32,
    parameter  N_INIT_PORT    = 8,
    parameter  N_REGION       = 2
)
(
    input  logic                                                        clk,
    input  logic                                                        rst_n,
    input  logic                                                        awvalid_i,
    input  logic [ADDR_WIDTH-1:0]                                       awaddr_i,
    output logic                                                        awready_o,
    output logic [N_INIT_PORT-1:0]                                      awvalid_o,
    input  logic [N_INIT_PORT-1:0]                                      awready_i,
    input  logic                                                        grant_FIFO_DEST_i,
    output logic [N_INIT_PORT-1:0]                                      DEST_o,
    output logic                                                        push_DEST_o,
    input  logic [N_REGION-1:0][N_INIT_PORT-1:0][ADDR_WIDTH-1:0]        START_ADDR_i,
    input  logic [N_REGION-1:0][N_INIT_PORT-1:0][ADDR_WIDTH-1:0]        END_ADDR_i,
    input  logic [N_REGION-1:0][N_INIT_PORT-1:0]                        enable_region_i,
    input  logic [N_INIT_PORT-1:0]                                      connectivity_map_i,
    output logic                                                        incr_req_o,
    input  logic                                                        full_counter_i,
    input  logic                                                        outstanding_trans_i,
    output logic                                                        error_req_o,
    input  logic                                                        error_gnt_i,
    output logic                                                        handle_error_o,
    input  logic                                                        wdata_error_completed_i,
    output logic                                                        sample_awdata_info_o
);
  logic [N_INIT_PORT-1:0]                                               match_region;  
  logic [N_INIT_PORT:0]                                                 match_region_masked;
  logic [N_REGION-1:0][N_INIT_PORT-1:0]                                 match_region_int;
  logic [N_INIT_PORT-1:0][N_REGION-1:0]                                 match_region_rev;
  logic                                                                 awready_int;
  logic [N_INIT_PORT-1:0]                                               awvalid_int;
  logic                                                                 error_detected;
  logic                                                                 local_increm;
  genvar i,j;
  assign DEST_o      = match_region[N_INIT_PORT-1:0];
  assign push_DEST_o = |(awvalid_i & awready_o) & ~error_detected;
  enum logic [1:0]      { OPERATIVE, COMPLETE_PENDING, ACCEPT_WDATA , COMPLETE_ERROR_RESP } CS, NS;
  generate
      for(j=0;j<N_REGION;j++)
      begin
           for(i=0;i<N_INIT_PORT;i++)
           begin
              assign match_region_int[j][i]  =  (enable_region_i[j][i] == 1'b1 ) ? (awaddr_i >= START_ADDR_i[j][i]) && (awaddr_i <= END_ADDR_i[j][i]) : 1'b0;
           end
      end
      for(j=0;j<N_INIT_PORT;j++)
      begin
           for(i=0;i<N_REGION;i++)
           begin
             assign match_region_rev[j][i] = match_region_int[i][j];
           end
      end
      for(i=0;i<N_INIT_PORT;i++)
      begin
        assign match_region[i]  =  | match_region_rev[i];
      end
      assign match_region_masked[N_INIT_PORT-1:0] = match_region & connectivity_map_i;
      assign match_region_masked[N_INIT_PORT] = ~(|match_region_masked[N_INIT_PORT-1:0]);
  endgenerate
  always_comb
  begin
      if(grant_FIFO_DEST_i == 1'b1)
      begin
            if(awvalid_i)
            begin
                {error_detected,awvalid_int} = {N_INIT_PORT+1{awvalid_i}} & match_region_masked;
            end
            else
            begin
                awvalid_int      = '0;
                error_detected = 1'b0;
            end
            awready_int = |({error_gnt_i,awready_i} & match_region_masked);
      end
      else
      begin
          awvalid_int       = '0;
          awready_int     = 1'b0;
          error_detected  = 1'b0;  
      end
  end
  always_ff @(posedge clk, negedge rst_n)
  begin
    if(rst_n == 1'b0)
    begin
        CS <= OPERATIVE;
    end
    else
    begin
        CS <= NS;
    end
  end
  assign local_increm =  |(awvalid_o & awready_i);
  always_comb
  begin
      awready_o            = 1'b0;
      handle_error_o       = 1'b0;
      sample_awdata_info_o = 1'b0;
      error_req_o          = 1'b0;
      incr_req_o           = 1'b0;
      awvalid_o            = '0;
      case(CS)
          OPERATIVE:
          begin
              handle_error_o  = 1'b0;
              incr_req_o =   local_increm;
              if(error_detected)
              begin
                NS = COMPLETE_PENDING;
                awready_o = 1'b1;
                sample_awdata_info_o = 1'b1;
                awvalid_o            = '0;
              end
              else
              begin
                NS = OPERATIVE;
                awready_o = awready_int;
                awvalid_o = awvalid_int;
              end
          end
          COMPLETE_PENDING:
          begin
              awready_o     = 1'b0;
              handle_error_o  = 1'b0;
              if(outstanding_trans_i)
              begin
                NS        = COMPLETE_PENDING;
                awready_o = 1'b0;
              end
              else
              begin  
                awready_o = 1'b0;
                NS = ACCEPT_WDATA;
              end
          end
          ACCEPT_WDATA :
          begin
              awready_o     = 1'b0;
              handle_error_o  = 1'b1;
              if(wdata_error_completed_i)
                NS = COMPLETE_ERROR_RESP;
              else
                NS = ACCEPT_WDATA;
          end
          COMPLETE_ERROR_RESP :
          begin
              handle_error_o  = 1'b0;
              error_req_o     = 1'b1;
              if(error_gnt_i)
                NS = OPERATIVE;
              else
                NS = COMPLETE_ERROR_RESP;
          end
          default :
          begin
              NS              = OPERATIVE;
              awready_o       = awready_int;
              handle_error_o  = 1'b0;
          end
      endcase
  end
endmodule
module axi_address_decoder_BR
#(
   parameter int unsigned N_TARG_PORT     = 8,
   parameter int unsigned AXI_ID_IN       = 16,
   parameter int unsigned AXI_ID_OUT      = AXI_ID_IN+$clog2(N_TARG_PORT)
)
(
   input  logic [AXI_ID_OUT-1:0]          rid_i,
   input  logic                           rvalid_i,
   output logic                           rready_o,
   output logic [N_TARG_PORT-1:0]         rvalid_o,
   input  logic [N_TARG_PORT-1:0]         rready_i
);
   logic [N_TARG_PORT-1:0]                req_mask;
   logic [$clog2(N_TARG_PORT)-1:0]        ROUTING;
   assign ROUTING = rid_i[AXI_ID_IN + $clog2(N_TARG_PORT) - 1:AXI_ID_IN];
   always_comb begin
      req_mask = '0;
      req_mask[ROUTING] = 1'b1;
   end
   always_comb begin
      if (rvalid_i) begin
         rvalid_o = {N_TARG_PORT{rvalid_i}} & req_mask;
      end else begin
         rvalid_o = '0;
      end
      rready_o = |(rready_i & req_mask);
   end
 endmodule
module axi_address_decoder_BW
#(
    parameter  N_TARG_PORT     = 3,
    parameter  AXI_ID_IN       = 3,
    parameter  AXI_ID_OUT      = AXI_ID_IN+$clog2(N_TARG_PORT)
)
(
  input  logic [AXI_ID_OUT-1:0]            bid_i,
  input  logic                             bvalid_i,
  output logic                             bready_o,
  output logic [N_TARG_PORT-1:0]           bvalid_o,
  input  logic [N_TARG_PORT-1:0]           bready_i
);
  logic [N_TARG_PORT-1:0]                  req_mask;
  logic [$clog2(N_TARG_PORT)-1:0]          ROUTING;
  assign ROUTING = bid_i[AXI_ID_IN+ $clog2(N_TARG_PORT)-1: AXI_ID_IN];
  always_comb
  begin
      req_mask = '0;
      req_mask[ROUTING] = 1'b1;
  end
  always_comb
  begin
      if(bvalid_i)
      begin
      bvalid_o = {N_TARG_PORT{bvalid_i}} & req_mask;
      end
      else
      begin
      bvalid_o = '0;
      end
      bready_o = |(bready_i & req_mask);
  end
 endmodule
module axi_address_decoder_DW
#(
    parameter  N_INIT_PORT    = 4,
    parameter  FIFO_DEPTH     = 8
)
(
    input  logic                       clk,
    input  logic                       rst_n,
    input  logic                       test_en_i,
    input  logic                       wvalid_i,
    input  logic                       wlast_i,
    output logic                       wready_o,
    output logic [N_INIT_PORT-1:0]     wvalid_o,
    input  logic [N_INIT_PORT-1:0]     wready_i,
    output logic                       grant_FIFO_DEST_o,
    input  logic [N_INIT_PORT-1:0]     DEST_i,
    input  logic                       push_DEST_i,
    input  logic                       handle_error_i,
    output logic                       wdata_error_completed_o
);
  logic                                valid_DEST;
  logic                                pop_from_DEST_FIFO;
  logic [N_INIT_PORT-1:0]              DEST_int;
  logic empty, full;
  fifo_v2 #(
      .FALL_THROUGH(1'b0),
      .DATA_WIDTH(N_INIT_PORT),
      .DEPTH(FIFO_DEPTH)
  ) MASTER_ID_FIFO (
      .clk_i        (clk              ),
      .rst_ni       (rst_n            ),
      .testmode_i   (test_en_i        ),
      .flush_i      (1'b0             ),
      .alm_empty_o  ( ),  
      .alm_full_o   ( ),  
      .data_i       (DEST_i             ),
      .push_i       (push_DEST_i        ),
      .full_o       (full               ),
      .data_o       (DEST_int           ),
      .empty_o      (empty              ),
      .pop_i        (pop_from_DEST_FIFO )
  );
  assign grant_FIFO_DEST_o = ~full;
  assign valid_DEST = ~empty;
  assign pop_from_DEST_FIFO = wlast_i & wvalid_i & wready_o & valid_DEST;
  always_comb
  begin
      if(handle_error_i)
      begin
          wready_o = 1'b1;
          wvalid_o = '0;
          wdata_error_completed_o = wlast_i & wvalid_i;
      end
      else
      begin
          wready_o = |(wready_i & DEST_int);
          wdata_error_completed_o           = 1'b0;
          if(wvalid_i & valid_DEST)
          begin
              wvalid_o  = {N_INIT_PORT{wvalid_i}} & DEST_int;
          end
          else
          begin
              wvalid_o  = '0;
          end
      end
  end
endmodule
module axi_multiplexer
#(
    parameter DATA_WIDTH = 64,
    parameter N_IN       = 16,
    parameter SEL_WIDTH  = $clog2(N_IN)
)
(
    input  logic [N_IN-1:0][DATA_WIDTH-1:0]   IN_DATA,
    output logic [DATA_WIDTH-1:0]             OUT_DATA,
    input  logic [SEL_WIDTH-1:0]              SEL
);
  assign OUT_DATA = IN_DATA[SEL];
endmodule
module axi_node #(
   parameter                   AXI_ADDRESS_W      = 32,
   parameter                   AXI_DATA_W         = 64,
   parameter                   AXI_NUMBYTES       = AXI_DATA_W/8,
   parameter                   AXI_USER_W         = 6,
   parameter                   N_MASTER_PORT      = 8,
   parameter                   N_SLAVE_PORT       = 4,
   parameter                   AXI_ID_IN          = 16,
   parameter                   AXI_ID_OUT         = AXI_ID_IN + $clog2(N_SLAVE_PORT),
   parameter                   FIFO_DEPTH_DW      = 8,
   parameter                   N_REGION           = 2
)(
   input logic                                                           clk,
   input logic                                                           rst_n,
   input logic                                                           test_en_i,
   input  logic [N_SLAVE_PORT-1:0][AXI_ID_IN-1:0]                        slave_awid_i,   
   input  logic [N_SLAVE_PORT-1:0][AXI_ADDRESS_W-1:0]                    slave_awaddr_i, 
   input  logic [N_SLAVE_PORT-1:0][ 7:0]                                 slave_awlen_i,           
   input  logic [N_SLAVE_PORT-1:0][ 2:0]                                 slave_awsize_i,          
   input  logic [N_SLAVE_PORT-1:0][ 1:0]                                 slave_awburst_i,         
   input  logic [N_SLAVE_PORT-1:0]                                       slave_awlock_i,          
   input  logic [N_SLAVE_PORT-1:0][ 3:0]                                 slave_awcache_i,        
   input  logic [N_SLAVE_PORT-1:0][ 2:0]                                 slave_awprot_i, 
   input  logic [N_SLAVE_PORT-1:0][ 3:0]                                 slave_awregion_i,       
   input  logic [N_SLAVE_PORT-1:0][ 5:0]                                 slave_awatop_i,
   input  logic [N_SLAVE_PORT-1:0][ AXI_USER_W-1:0]                      slave_awuser_i, 
   input  logic [N_SLAVE_PORT-1:0][ 3:0]                                 slave_awqos_i,  
   input  logic [N_SLAVE_PORT-1:0]                                       slave_awvalid_i,         
   output logic [N_SLAVE_PORT-1:0]                                       slave_awready_o,         
   input  logic [N_SLAVE_PORT-1:0] [AXI_DATA_W-1:0]                      slave_wdata_i,
   input  logic [N_SLAVE_PORT-1:0] [AXI_NUMBYTES-1:0]                    slave_wstrb_i,    
   input  logic [N_SLAVE_PORT-1:0]                                       slave_wlast_i,    
   input  logic [N_SLAVE_PORT-1:0][AXI_USER_W-1:0]                       slave_wuser_i,    
   input  logic [N_SLAVE_PORT-1:0]                                       slave_wvalid_i,   
   output logic [N_SLAVE_PORT-1:0]                                       slave_wready_o,   
   output  logic [N_SLAVE_PORT-1:0]  [AXI_ID_IN-1:0]                     slave_bid_o,
   output  logic [N_SLAVE_PORT-1:0]  [ 1:0]                              slave_bresp_o,
   output  logic [N_SLAVE_PORT-1:0]                                      slave_bvalid_o,
   output  logic [N_SLAVE_PORT-1:0]  [AXI_USER_W-1:0]                    slave_buser_o,    
   input   logic [N_SLAVE_PORT-1:0]                                      slave_bready_i,
   input  logic [N_SLAVE_PORT-1:0][AXI_ID_IN-1:0]                        slave_arid_i,
   input  logic [N_SLAVE_PORT-1:0][AXI_ADDRESS_W-1:0]                    slave_araddr_i,
   input  logic [N_SLAVE_PORT-1:0][ 7:0]                                 slave_arlen_i,    
   input  logic [N_SLAVE_PORT-1:0][ 2:0]                                 slave_arsize_i,   
   input  logic [N_SLAVE_PORT-1:0][ 1:0]                                 slave_arburst_i,  
   input  logic [N_SLAVE_PORT-1:0]                                       slave_arlock_i,   
   input  logic [N_SLAVE_PORT-1:0][ 3:0]                                 slave_arcache_i,
   input  logic [N_SLAVE_PORT-1:0][ 2:0]                                 slave_arprot_i,
   input  logic [N_SLAVE_PORT-1:0][ 3:0]                                 slave_arregion_i,       
   input  logic [N_SLAVE_PORT-1:0][ AXI_USER_W-1:0]                      slave_aruser_i, 
   input  logic [N_SLAVE_PORT-1:0][ 3:0]                                 slave_arqos_i,  
   input  logic [N_SLAVE_PORT-1:0]                                       slave_arvalid_i,  
   output logic [N_SLAVE_PORT-1:0]                                       slave_arready_o,  
   output logic [N_SLAVE_PORT-1:0][AXI_ID_IN-1:0]                        slave_rid_o,
   output logic [N_SLAVE_PORT-1:0][AXI_DATA_W-1:0]                       slave_rdata_o,
   output logic [N_SLAVE_PORT-1:0][ 1:0]                                 slave_rresp_o,
   output logic [N_SLAVE_PORT-1:0]                                       slave_rlast_o,    
   output logic [N_SLAVE_PORT-1:0][AXI_USER_W-1:0]                       slave_ruser_o,    
   output logic [N_SLAVE_PORT-1:0]                                       slave_rvalid_o,   
   input  logic [N_SLAVE_PORT-1:0]                                       slave_rready_i,    
   output logic [N_MASTER_PORT-1:0][AXI_ID_OUT-1:0]                      master_awid_o,  
   output logic [N_MASTER_PORT-1:0][AXI_ADDRESS_W-1:0]                   master_awaddr_o,        
   output logic [N_MASTER_PORT-1:0][ 7:0]                                master_awlen_o,          
   output logic [N_MASTER_PORT-1:0][ 2:0]                                master_awsize_o,         
   output logic [N_MASTER_PORT-1:0][ 1:0]                                master_awburst_o,        
   output logic [N_MASTER_PORT-1:0]                                      master_awlock_o,         
   output logic [N_MASTER_PORT-1:0][ 3:0]                                master_awcache_o,       
   output logic [N_MASTER_PORT-1:0][ 2:0]                                master_awprot_o,        
   output logic [N_MASTER_PORT-1:0][ 3:0]                                master_awregion_o,      
   output logic [N_MASTER_PORT-1:0][ 5:0]                                master_awatop_o,
   output logic [N_MASTER_PORT-1:0][ AXI_USER_W-1:0]                     master_awuser_o,        
   output logic [N_MASTER_PORT-1:0][ 3:0]                                master_awqos_o, 
   output logic [N_MASTER_PORT-1:0]                                      master_awvalid_o,        
   input  logic [N_MASTER_PORT-1:0]                                      master_awready_i,        
   output logic [N_MASTER_PORT-1:0] [AXI_DATA_W-1:0]                     master_wdata_o,
   output logic [N_MASTER_PORT-1:0] [AXI_NUMBYTES-1:0]                   master_wstrb_o,    
   output logic [N_MASTER_PORT-1:0]                                      master_wlast_o,    
   output logic [N_MASTER_PORT-1:0] [ AXI_USER_W-1:0]                    master_wuser_o,    
   output logic [N_MASTER_PORT-1:0]                                      master_wvalid_o,   
   input  logic [N_MASTER_PORT-1:0]                                      master_wready_i,   
   input  logic [N_MASTER_PORT-1:0] [AXI_ID_OUT-1:0]                     master_bid_i,
   input  logic [N_MASTER_PORT-1:0] [ 1:0]                               master_bresp_i,
   input  logic [N_MASTER_PORT-1:0] [ AXI_USER_W-1:0]                    master_buser_i,
   input  logic [N_MASTER_PORT-1:0]                                      master_bvalid_i,
   output logic [N_MASTER_PORT-1:0]                                      master_bready_o,
   output  logic [N_MASTER_PORT-1:0][AXI_ID_OUT-1:0]                     master_arid_o,
   output  logic [N_MASTER_PORT-1:0][AXI_ADDRESS_W-1:0]                  master_araddr_o,
   output  logic [N_MASTER_PORT-1:0][ 7:0]                               master_arlen_o,    
   output  logic [N_MASTER_PORT-1:0][ 2:0]                               master_arsize_o,   
   output  logic [N_MASTER_PORT-1:0][ 1:0]                               master_arburst_o,  
   output  logic [N_MASTER_PORT-1:0]                                     master_arlock_o,   
   output  logic [N_MASTER_PORT-1:0][ 3:0]                               master_arcache_o,
   output  logic [N_MASTER_PORT-1:0][ 2:0]                               master_arprot_o,
   output  logic [N_MASTER_PORT-1:0][ 3:0]                               master_arregion_o,      
   output  logic [N_MASTER_PORT-1:0][ AXI_USER_W-1:0]                    master_aruser_o,        
   output  logic [N_MASTER_PORT-1:0][ 3:0]                               master_arqos_o, 
   output  logic [N_MASTER_PORT-1:0]                                     master_arvalid_o,  
   input logic [N_MASTER_PORT-1:0]                                       master_arready_i,  
   input  logic [N_MASTER_PORT-1:0][AXI_ID_OUT-1:0]                      master_rid_i,
   input  logic [N_MASTER_PORT-1:0][AXI_DATA_W-1:0]                      master_rdata_i,
   input  logic [N_MASTER_PORT-1:0][ 1:0]                                master_rresp_i,
   input  logic [N_MASTER_PORT-1:0]                                      master_rlast_i,    
   input  logic [N_MASTER_PORT-1:0][ AXI_USER_W-1:0]                     master_ruser_i,
   input  logic [N_MASTER_PORT-1:0]                                      master_rvalid_i,   
   output logic [N_MASTER_PORT-1:0]                                      master_rready_o,    
   input  logic [N_REGION-1:0][N_MASTER_PORT-1:0][AXI_ADDRESS_W-1:0]         cfg_START_ADDR_i,
   input  logic [N_REGION-1:0][N_MASTER_PORT-1:0][AXI_ADDRESS_W-1:0]         cfg_END_ADDR_i,
   input  logic [N_REGION-1:0][N_MASTER_PORT-1:0]                            cfg_valid_rule_i,
   input  logic [N_SLAVE_PORT-1:0][N_MASTER_PORT-1:0]                        cfg_connectivity_map_i
);
genvar i,j,k;
logic  [N_SLAVE_PORT-1:0][N_MASTER_PORT-1:0]                    arvalid_int;
logic  [N_MASTER_PORT-1:0][N_SLAVE_PORT-1:0]                    arready_int;
logic  [N_MASTER_PORT-1:0][N_SLAVE_PORT-1:0]                    arvalid_int_reverse;
logic  [N_SLAVE_PORT-1:0][N_MASTER_PORT-1:0]                    arready_int_reverse;
logic  [N_SLAVE_PORT-1:0][N_MASTER_PORT-1:0]                    awvalid_int;
logic  [N_MASTER_PORT-1:0][N_SLAVE_PORT-1:0]                    awready_int;
logic  [N_MASTER_PORT-1:0][N_SLAVE_PORT-1:0]                    awvalid_int_reverse;
logic  [N_SLAVE_PORT-1:0][N_MASTER_PORT-1:0]                    awready_int_reverse;
logic  [N_SLAVE_PORT-1:0][N_MASTER_PORT-1:0]                    wvalid_int;
logic  [N_MASTER_PORT-1:0][N_SLAVE_PORT-1:0]                    wready_int;
logic  [N_MASTER_PORT-1:0][N_SLAVE_PORT-1:0]                    wvalid_int_reverse;
logic  [N_SLAVE_PORT-1:0][N_MASTER_PORT-1:0]                    wready_int_reverse;
logic [N_MASTER_PORT-1:0][N_SLAVE_PORT-1:0]                     bvalid_int;
logic [N_SLAVE_PORT-1:0][N_MASTER_PORT-1:0]                     bready_int;
logic [N_SLAVE_PORT-1:0][N_MASTER_PORT-1:0]                     bvalid_int_reverse;
logic [N_MASTER_PORT-1:0][N_SLAVE_PORT-1:0]                     bready_int_reverse;
logic [N_MASTER_PORT-1:0][N_SLAVE_PORT-1:0]                     rvalid_int;
logic [N_SLAVE_PORT-1:0][N_MASTER_PORT-1:0]                     rready_int;
logic [N_SLAVE_PORT-1:0][N_MASTER_PORT-1:0]                     rvalid_int_reverse;
logic [N_MASTER_PORT-1:0][N_SLAVE_PORT-1:0]                     rready_int_reverse;
logic [N_REGION-1:0][N_MASTER_PORT-1:0][AXI_ADDRESS_W-1:0]      START_ADDR;
logic [N_REGION-1:0][N_MASTER_PORT-1:0][AXI_ADDRESS_W-1:0]      END_ADDR;
logic [N_REGION-1:0][N_MASTER_PORT-1:0]                         valid_rule;
logic [N_SLAVE_PORT-1:0][N_MASTER_PORT-1:0]                     connectivity_map;
generate
for(i=0;i<N_MASTER_PORT;i++)
begin : _REVERSING_VALID_READY_MASTER
    for(j=0;j<N_SLAVE_PORT;j++)
    begin : _REVERSING_VALID_READY_SLAVE
      assign arvalid_int_reverse[i][j] = arvalid_int[j][i];
      assign awvalid_int_reverse[i][j] = awvalid_int[j][i];
      assign wvalid_int_reverse[i][j]  = wvalid_int[j][i];
      assign bvalid_int_reverse[j][i]  = bvalid_int[i][j];
      assign rvalid_int_reverse[j][i]  = rvalid_int[i][j];
      assign arready_int_reverse[j][i] = arready_int[i][j];
      assign awready_int_reverse[j][i] = awready_int[i][j];
      assign wready_int_reverse[j][i]  = wready_int[i][j];
      assign bready_int_reverse[i][j]  = bready_int[j][i];
      assign rready_int_reverse[i][j]  = rready_int[j][i];
    end
end
for(i=0; i<N_MASTER_PORT; i++)
begin : _REQ_BLOCK_GEN
   axi_request_block
   #(
       .AXI_ADDRESS_W  (  AXI_ADDRESS_W   ),
       .AXI_DATA_W     (  AXI_DATA_W      ),
       .AXI_USER_W     (  AXI_USER_W      ),
       .N_INIT_PORT    (  N_MASTER_PORT   ),
       .N_TARG_PORT    (  N_SLAVE_PORT    ),
       .FIFO_DW_DEPTH  (  FIFO_DEPTH_DW   ),
       .AXI_ID_IN      (  AXI_ID_IN       )
   )
   REQ_BLOCK
   (
     .clk         (   clk                   ),
     .rst_n       (   rst_n                 ),
     .test_en_i   (   test_en_i             ),
     .awid_i      (  slave_awid_i           ), 
     .awaddr_i    (  slave_awaddr_i         ), 
     .awlen_i     (  slave_awlen_i          ),  
     .awsize_i    (  slave_awsize_i         ),  
     .awburst_i   (  slave_awburst_i        ),  
     .awlock_i    (  slave_awlock_i         ),  
     .awcache_i   (  slave_awcache_i        ), 
     .awprot_i    (  slave_awprot_i         ), 
     .awregion_i  (  slave_awregion_i       ), 
     .awatop_i    (  slave_awatop_i         ), 
     .awuser_i    (  slave_awuser_i         ), 
     .awqos_i     (  slave_awqos_i          ), 
     .awvalid_i   (  awvalid_int_reverse[i] ),  
     .awready_o   (  awready_int[i]         ),  
     .wdata_i    (  slave_wdata_i           ),
     .wstrb_i    (  slave_wstrb_i           ),  
     .wlast_i    (  slave_wlast_i           ),  
     .wuser_i    (  slave_wuser_i           ),
     .wvalid_i   (  wvalid_int_reverse[i]   ),  
     .wready_o   (  wready_int[i]           ),  
     .arid_i     (  slave_arid_i            ),
     .araddr_i   (  slave_araddr_i          ),
     .arlen_i    (  slave_arlen_i           ),  
     .arsize_i   (  slave_arsize_i          ),  
     .arburst_i  (  slave_arburst_i         ),  
     .arlock_i   (  slave_arlock_i          ),  
     .arcache_i  (  slave_arcache_i         ),
     .arprot_i   (  slave_arprot_i          ),
     .arregion_i (  slave_arregion_i        ), 
     .aruser_i   (  slave_aruser_i          ), 
     .arqos_i    (  slave_arqos_i           ), 
     .arvalid_i  (  arvalid_int_reverse[i]  ),  
     .arready_o  (  arready_int[i]          ),  
     .bid_i      (  master_bid_i[i]         ),
     .bvalid_i   (  master_bvalid_i[i]      ),
     .bready_o   (  master_bready_o[i]      ),
     .bvalid_o   (  bvalid_int[i]           ),
     .bready_i   (  bready_int_reverse[i]   ),
     .rid_i     (  master_rid_i[i]          ),
     .rvalid_i  (  master_rvalid_i[i]       ),    
     .rready_o  (  master_rready_o[i]       ),    
     .rvalid_o  (  rvalid_int[i]            ),
     .rready_i  (  rready_int_reverse[i]    ),
     .awid_o    (  master_awid_o[i]         ), 
     .awaddr_o  (  master_awaddr_o[i]       ), 
     .awlen_o   (  master_awlen_o[i]        ),  
     .awsize_o  (  master_awsize_o[i]       ),  
     .awburst_o (  master_awburst_o[i]      ),  
     .awlock_o  (  master_awlock_o[i]       ),  
     .awcache_o (  master_awcache_o[i]      ), 
     .awprot_o  (  master_awprot_o[i]       ), 
     .awregion_o(  master_awregion_o[i]     ), 
     .awatop_o  (  master_awatop_o[i]       ), 
     .awuser_o  (  master_awuser_o[i]       ), 
     .awqos_o   (  master_awqos_o[i]        ), 
     .awvalid_o (  master_awvalid_o[i]      ),  
     .awready_i (  master_awready_i[i]      ),  
     .wdata_o  (  master_wdata_o[i]         ),
     .wstrb_o  (  master_wstrb_o[i]         ),  
     .wlast_o  (  master_wlast_o[i]         ),  
     .wuser_o  (  master_wuser_o[i]         ),
     .wvalid_o (  master_wvalid_o[i]        ),  
     .wready_i (  master_wready_i[i]        ),  
     .arid_o    (  master_arid_o[i]         ),
     .araddr_o  (  master_araddr_o[i]       ),
     .arlen_o   (  master_arlen_o[i]        ),  
     .arsize_o  (  master_arsize_o[i]       ),  
     .arburst_o (  master_arburst_o[i]      ),  
     .arlock_o  (  master_arlock_o[i]       ),  
     .arcache_o (  master_arcache_o[i]      ),
     .arprot_o  (  master_arprot_o[i]       ),
     .arregion_o(  master_arregion_o[i]     ), 
     .aruser_o  (  master_aruser_o[i]       ), 
     .arqos_o   (  master_arqos_o[i]        ), 
     .arvalid_o (  master_arvalid_o[i]      ),  
     .arready_i (  master_arready_i[i]      )   
   );
end
for (i = 0; i < N_SLAVE_PORT; i++) begin : _RESP_BLOCK_GEN
axi_response_block
#(
    .AXI_ADDRESS_W  (AXI_ADDRESS_W ),
    .AXI_DATA_W     (AXI_DATA_W    ),
    .AXI_USER_W     (AXI_USER_W    ),
    .N_INIT_PORT    (N_MASTER_PORT ),
    .N_TARG_PORT    (N_SLAVE_PORT  ),
    .FIFO_DEPTH_DW  (FIFO_DEPTH_DW ),
    .AXI_ID_IN      (AXI_ID_IN     ),
    .N_REGION       (N_REGION      )
)
RESP_BLOCK
(
   .clk                (  clk                     ),
   .rst_n              (  rst_n                   ),
   .test_en_i          (  test_en_i               ),
   .rid_i              (  master_rid_i            ),
   .rdata_i            (  master_rdata_i          ),
   .rresp_i            (  master_rresp_i          ),
   .rlast_i            (  master_rlast_i          ),     
   .ruser_i            (  master_ruser_i          ),     
   .rvalid_i           (  rvalid_int_reverse[i]   ),     
   .rready_o           (  rready_int[i]           ),     
   .bid_i              (  master_bid_i            ),
   .bresp_i            (  master_bresp_i          ),
   .buser_i            (  master_buser_i          ),     
   .bvalid_i           (  bvalid_int_reverse[i]   ),     
   .bready_o           (  bready_int[i]           ),     
   .rid_o              (  slave_rid_o[i]          ),
   .rdata_o            (  slave_rdata_o[i]        ),
   .rresp_o            (  slave_rresp_o[i]        ),
   .rlast_o            (  slave_rlast_o[i]        ),     
   .ruser_o            (  slave_ruser_o[i]        ),
   .rvalid_o           (  slave_rvalid_o[i]       ),     
   .rready_i           (  slave_rready_i[i]       ),     
   .bid_o              (  slave_bid_o[i]          ),
   .bresp_o            (  slave_bresp_o[i]        ),
   .buser_o            (  slave_buser_o[i]        ),     
   .bvalid_o           (  slave_bvalid_o[i]       ),     
   .bready_i           (  slave_bready_i[i]       ),     
   .arvalid_i          (  slave_arvalid_i[i]      ),
   .araddr_i           (  slave_araddr_i[i]       ),
   .arready_o          (  slave_arready_o[i]      ),
   .arlen_i            (  slave_arlen_i[i]        ),
   .aruser_i           (  slave_aruser_i[i]       ),
   .arid_i             (  slave_arid_i[i]         ),
   .arvalid_o          (  arvalid_int[i]          ),
   .arready_i          (  arready_int_reverse[i]  ),
   .awvalid_i          (  slave_awvalid_i[i]      ),
   .awaddr_i           (  slave_awaddr_i[i]       ),
   .awready_o          (  slave_awready_o[i]      ),
   .awuser_i           (  slave_awuser_i[i]       ),
   .awid_i             (  slave_awid_i[i]         ),
   .awvalid_o          (  awvalid_int[i]          ),
   .awready_i          (  awready_int_reverse[i]  ),
   .wvalid_i           (  slave_wvalid_i[i]       ),
   .wlast_i            (  slave_wlast_i[i]        ),
   .wready_o           (  slave_wready_o[i]       ),
   .wvalid_o           (  wvalid_int[i]           ),
   .wready_i           (  wready_int_reverse[i]   ),
   .START_ADDR_i       ( START_ADDR               ),
   .END_ADDR_i         ( END_ADDR                 ),
   .enable_region_i    ( valid_rule               ),
   .connectivity_map_i ( connectivity_map[i]      )
);
end
endgenerate
    assign START_ADDR       = cfg_START_ADDR_i;
    assign END_ADDR         = cfg_END_ADDR_i;
    assign connectivity_map = cfg_connectivity_map_i;
    generate
      for (i = 0; i < N_REGION; i++) begin : _VALID_RULE_REGION
        for (j = 0; j < N_MASTER_PORT; j++) begin : _VALID_RULE_MASTER
          assign valid_rule[i][j] = cfg_valid_rule_i[i][j];
        end
      end
    endgenerate
endmodule
module axi_node_arbiter #(
  parameter int unsigned AUX_WIDTH = 0,  
  parameter int unsigned ID_WIDTH = 0,
  parameter int unsigned N_MASTER = 0
) (
  input  logic                                clk_i,
  input  logic                                rst_ni,
  input  logic [N_MASTER-1:0][ID_WIDTH-1:0]   inp_id_i,
  input  logic [N_MASTER-1:0][AUX_WIDTH-1:0]  inp_aux_i,
  input  logic [N_MASTER-1:0]                 inp_valid_i,
  output logic [N_MASTER-1:0]                 inp_ready_o,
  output logic [ID_WIDTH-1:0]                 oup_id_o,
  output logic [AUX_WIDTH-1:0]                oup_aux_o,
  output logic                                oup_valid_o,
  input  logic                                oup_ready_i
);
  typedef struct packed {
    logic [AUX_WIDTH-1:0] aux;
    logic [ID_WIDTH-1:0]  id;
  } axi_meta_t;
  logic 		  dummy;
  axi_meta_t [N_MASTER-1:0] inp_meta;
  axi_meta_t oup_meta;
  for (genvar i = 0; i < N_MASTER; i++) begin: gen_inp_meta
    assign inp_meta[i].aux = inp_aux_i[i];
    assign inp_meta[i].id = inp_id_i[i];
  end
  stream_arbiter #(
    .DATA_T (axi_meta_t),
    .N_INP  (N_MASTER)
  ) i_arb_inp (
    .clk_i        (clk_i),
    .rst_ni       (rst_ni),
    .inp_data_i   (inp_meta),
    .inp_valid_i  (inp_valid_i),
    .inp_ready_o  (inp_ready_o),
    .oup_data_o   (oup_meta),
    .oup_valid_o  (oup_valid_o),
    .oup_ready_i  (oup_ready_i)
  );
  assign oup_id_o = oup_meta.id;
  assign oup_aux_o = oup_meta.aux;
endmodule
module axi_node_intf_wrap #(
    parameter NB_MASTER      = 4,
    parameter NB_SLAVE       = 4,
    parameter NB_REGION      = 1,
    parameter AXI_ADDR_WIDTH = 32,
    parameter AXI_DATA_WIDTH = 32,
    parameter AXI_ID_WIDTH   = 10,
    parameter AXI_USER_WIDTH = 0
  )(
    input logic clk,
    input logic rst_n,
    input logic test_en_i,
    AXI_BUS.Slave slave[NB_SLAVE-1:0],
    AXI_BUS.Master master[NB_MASTER-1:0],
    input  logic [NB_REGION-1:0][NB_MASTER-1:0][AXI_ADDR_WIDTH-1:0]  start_addr_i,
    input  logic [NB_REGION-1:0][NB_MASTER-1:0][AXI_ADDR_WIDTH-1:0]  end_addr_i,
    input  logic [NB_REGION-1:0][NB_MASTER-1:0]                      valid_rule_i
  );
  localparam AXI_STRB_WIDTH = AXI_DATA_WIDTH/8;
  localparam AXI_ID_WIDTH_TARG =   AXI_ID_WIDTH;
  localparam AXI_ID_WIDTH_INIT =   AXI_ID_WIDTH_TARG + $clog2(NB_SLAVE);
  logic [NB_MASTER-1:0][AXI_ID_WIDTH_INIT-1:0] s_master_aw_id;
  logic [NB_MASTER-1:0][AXI_ADDR_WIDTH-1:0]    s_master_aw_addr;
  logic [NB_MASTER-1:0][7:0]                   s_master_aw_len;
  logic [NB_MASTER-1:0][2:0]                   s_master_aw_size;
  logic [NB_MASTER-1:0][1:0]                   s_master_aw_burst;
  logic [NB_MASTER-1:0]                        s_master_aw_lock;
  logic [NB_MASTER-1:0][3:0]                   s_master_aw_cache;
  logic [NB_MASTER-1:0][2:0]                   s_master_aw_prot;
  logic [NB_MASTER-1:0][3:0]                   s_master_aw_region;
  logic [NB_MASTER-1:0][5:0]                   s_master_aw_atop;
  logic [NB_MASTER-1:0][AXI_USER_WIDTH-1:0]    s_master_aw_user;
  logic [NB_MASTER-1:0][3:0]                   s_master_aw_qos;
  logic [NB_MASTER-1:0]                        s_master_aw_valid;
  logic [NB_MASTER-1:0]                        s_master_aw_ready;
  logic [NB_MASTER-1:0][AXI_ID_WIDTH_INIT-1:0] s_master_ar_id;
  logic [NB_MASTER-1:0][AXI_ADDR_WIDTH-1:0]    s_master_ar_addr;
  logic [NB_MASTER-1:0][7:0]                   s_master_ar_len;
  logic [NB_MASTER-1:0][2:0]                   s_master_ar_size;
  logic [NB_MASTER-1:0][1:0]                   s_master_ar_burst;
  logic [NB_MASTER-1:0]                        s_master_ar_lock;
  logic [NB_MASTER-1:0][3:0]                   s_master_ar_cache;
  logic [NB_MASTER-1:0][2:0]                   s_master_ar_prot;
  logic [NB_MASTER-1:0][3:0]                   s_master_ar_region;
  logic [NB_MASTER-1:0][AXI_USER_WIDTH-1:0]    s_master_ar_user;
  logic [NB_MASTER-1:0][3:0]                   s_master_ar_qos;
  logic [NB_MASTER-1:0]                        s_master_ar_valid;
  logic [NB_MASTER-1:0]                        s_master_ar_ready;
  logic [NB_MASTER-1:0][AXI_DATA_WIDTH-1:0]    s_master_w_data;
  logic [NB_MASTER-1:0][AXI_STRB_WIDTH-1:0]    s_master_w_strb;
  logic [NB_MASTER-1:0]                        s_master_w_last;
  logic [NB_MASTER-1:0][AXI_USER_WIDTH-1:0]    s_master_w_user;
  logic [NB_MASTER-1:0]                        s_master_w_valid;
  logic [NB_MASTER-1:0]                        s_master_w_ready;
  logic [NB_MASTER-1:0][AXI_ID_WIDTH_INIT-1:0] s_master_b_id;
  logic [NB_MASTER-1:0][1:0]                   s_master_b_resp;
  logic [NB_MASTER-1:0]                        s_master_b_valid;
  logic [NB_MASTER-1:0][AXI_USER_WIDTH-1:0]    s_master_b_user;
  logic [NB_MASTER-1:0]                        s_master_b_ready;
  logic [NB_MASTER-1:0][AXI_ID_WIDTH_INIT-1:0] s_master_r_id;
  logic [NB_MASTER-1:0][AXI_DATA_WIDTH-1:0]    s_master_r_data;
  logic [NB_MASTER-1:0][1:0]                   s_master_r_resp;
  logic [NB_MASTER-1:0]                        s_master_r_last;
  logic [NB_MASTER-1:0][AXI_USER_WIDTH-1:0]    s_master_r_user;
  logic [NB_MASTER-1:0]                        s_master_r_valid;
  logic [NB_MASTER-1:0]                        s_master_r_ready;
  logic [NB_SLAVE-1:0][AXI_ID_WIDTH_TARG-1:0] s_slave_aw_id;
  logic [NB_SLAVE-1:0][AXI_ADDR_WIDTH-1:0]    s_slave_aw_addr;
  logic [NB_SLAVE-1:0][7:0]                   s_slave_aw_len;
  logic [NB_SLAVE-1:0][2:0]                   s_slave_aw_size;
  logic [NB_SLAVE-1:0][1:0]                   s_slave_aw_burst;
  logic [NB_SLAVE-1:0]                        s_slave_aw_lock;
  logic [NB_SLAVE-1:0][3:0]                   s_slave_aw_cache;
  logic [NB_SLAVE-1:0][2:0]                   s_slave_aw_prot;
  logic [NB_SLAVE-1:0][3:0]                   s_slave_aw_region;
  logic [NB_SLAVE-1:0][5:0]                   s_slave_aw_atop;
  logic [NB_SLAVE-1:0][AXI_USER_WIDTH-1:0]    s_slave_aw_user;
  logic [NB_SLAVE-1:0][3:0]                   s_slave_aw_qos;
  logic [NB_SLAVE-1:0]                        s_slave_aw_valid;
  logic [NB_SLAVE-1:0]                        s_slave_aw_ready;
  logic [NB_SLAVE-1:0][AXI_ID_WIDTH_TARG-1:0] s_slave_ar_id;
  logic [NB_SLAVE-1:0][AXI_ADDR_WIDTH-1:0]    s_slave_ar_addr;
  logic [NB_SLAVE-1:0][7:0]                   s_slave_ar_len;
  logic [NB_SLAVE-1:0][2:0]                   s_slave_ar_size;
  logic [NB_SLAVE-1:0][1:0]                   s_slave_ar_burst;
  logic [NB_SLAVE-1:0]                        s_slave_ar_lock;
  logic [NB_SLAVE-1:0][3:0]                   s_slave_ar_cache;
  logic [NB_SLAVE-1:0][2:0]                   s_slave_ar_prot;
  logic [NB_SLAVE-1:0][3:0]                   s_slave_ar_region;
  logic [NB_SLAVE-1:0][AXI_USER_WIDTH-1:0]    s_slave_ar_user;
  logic [NB_SLAVE-1:0][3:0]                   s_slave_ar_qos;
  logic [NB_SLAVE-1:0]                        s_slave_ar_valid;
  logic [NB_SLAVE-1:0]                        s_slave_ar_ready;
  logic [NB_SLAVE-1:0][AXI_DATA_WIDTH-1:0]    s_slave_w_data;
  logic [NB_SLAVE-1:0][AXI_STRB_WIDTH-1:0]    s_slave_w_strb;
  logic [NB_SLAVE-1:0]                        s_slave_w_last;
  logic [NB_SLAVE-1:0][AXI_USER_WIDTH-1:0]    s_slave_w_user;
  logic [NB_SLAVE-1:0]                        s_slave_w_valid;
  logic [NB_SLAVE-1:0]                        s_slave_w_ready;
  logic [NB_SLAVE-1:0][AXI_ID_WIDTH_TARG-1:0] s_slave_b_id;
  logic [NB_SLAVE-1:0][1:0]                   s_slave_b_resp;
  logic [NB_SLAVE-1:0]                        s_slave_b_valid;
  logic [NB_SLAVE-1:0][AXI_USER_WIDTH-1:0]    s_slave_b_user;
  logic [NB_SLAVE-1:0]                        s_slave_b_ready;
  logic [NB_SLAVE-1:0][AXI_ID_WIDTH_TARG-1:0] s_slave_r_id;
  logic [NB_SLAVE-1:0][AXI_DATA_WIDTH-1:0]    s_slave_r_data;
  logic [NB_SLAVE-1:0][1:0]                   s_slave_r_resp;
  logic [NB_SLAVE-1:0]                        s_slave_r_last;
  logic [NB_SLAVE-1:0][AXI_USER_WIDTH-1:0]    s_slave_r_user;
  logic [NB_SLAVE-1:0]                        s_slave_r_valid;
  logic [NB_SLAVE-1:0]                        s_slave_r_ready;
  generate
    genvar i;
    for(i = 0; i < NB_MASTER; i++)
    begin
      assign                        master[i].aw_id[AXI_ID_WIDTH_INIT-1:0] = s_master_aw_id[i];
      assign                        master[i].aw_addr                      = s_master_aw_addr[i];
      assign                        master[i].aw_len                       = s_master_aw_len[i];
      assign                        master[i].aw_size                      = s_master_aw_size[i];
      assign                        master[i].aw_burst                     = s_master_aw_burst[i];
      assign                        master[i].aw_lock                      = s_master_aw_lock[i];
      assign                        master[i].aw_cache                     = s_master_aw_cache[i];
      assign                        master[i].aw_prot                      = s_master_aw_prot[i];
      assign                        master[i].aw_region                    = s_master_aw_region[i];
      assign                        master[i].aw_atop                      = s_master_aw_atop[i];
      assign                        master[i].aw_user                      = s_master_aw_user[i];
      assign                        master[i].aw_qos                       = s_master_aw_qos[i];
      assign                        master[i].aw_valid                     = s_master_aw_valid[i];
      assign s_master_aw_ready[i] = master[i].aw_ready;
      assign                        master[i].ar_id[AXI_ID_WIDTH_INIT-1:0] = s_master_ar_id[i];
      assign                        master[i].ar_addr                      = s_master_ar_addr[i];
      assign                        master[i].ar_len                       = s_master_ar_len[i];
      assign                        master[i].ar_size                      = s_master_ar_size[i];
      assign                        master[i].ar_burst                     = s_master_ar_burst[i];
      assign                        master[i].ar_lock                      = s_master_ar_lock[i];
      assign                        master[i].ar_cache                     = s_master_ar_cache[i];
      assign                        master[i].ar_prot                      = s_master_ar_prot[i];
      assign                        master[i].ar_region                    = s_master_ar_region[i];
      assign                        master[i].ar_user                      = s_master_ar_user[i];
      assign                        master[i].ar_qos                       = s_master_ar_qos[i];
      assign                        master[i].ar_valid                     = s_master_ar_valid[i];
      assign s_master_ar_ready[i] = master[i].ar_ready;
      assign                        master[i].w_data  = s_master_w_data[i];
      assign                        master[i].w_strb  = s_master_w_strb[i];
      assign                        master[i].w_last  = s_master_w_last[i];
      assign                        master[i].w_user  = s_master_w_user[i];
      assign                        master[i].w_valid = s_master_w_valid[i];
      assign s_master_w_ready[i]  = master[i].w_ready;
      assign s_master_b_id[i]     = master[i].b_id[AXI_ID_WIDTH_INIT-1:0];
      assign s_master_b_resp[i]   = master[i].b_resp;
      assign s_master_b_valid[i]  = master[i].b_valid;
      assign s_master_b_user[i]   = master[i].b_user;
      assign                        master[i].b_ready = s_master_b_ready[i];
      assign s_master_r_id[i]     = master[i].r_id[AXI_ID_WIDTH_INIT-1:0];
      assign s_master_r_data[i]   = master[i].r_data;
      assign s_master_r_resp[i]   = master[i].r_resp;
      assign s_master_r_last[i]   = master[i].r_last;
      assign s_master_r_user[i]   = master[i].r_user;
      assign s_master_r_valid[i]  = master[i].r_valid;
      assign                        master[i].r_ready = s_master_r_ready[i];
    end
  endgenerate
  generate
    genvar j;
    for(j = 0; j < NB_SLAVE; j++)
    begin
      assign s_slave_aw_id[j]     = slave[j].aw_id[AXI_ID_WIDTH_TARG-1:0];
      assign s_slave_aw_addr[j]   = slave[j].aw_addr;
      assign s_slave_aw_len[j]    = slave[j].aw_len;
      assign s_slave_aw_size[j]   = slave[j].aw_size;
      assign s_slave_aw_burst[j]  = slave[j].aw_burst;
      assign s_slave_aw_lock[j]   = slave[j].aw_lock;
      assign s_slave_aw_cache[j]  = slave[j].aw_cache;
      assign s_slave_aw_prot[j]   = slave[j].aw_prot;
      assign s_slave_aw_region[j] = slave[j].aw_region;
      assign s_slave_aw_atop[j]   = slave[j].aw_atop;
      assign s_slave_aw_user[j]   = slave[j].aw_user;
      assign s_slave_aw_qos[j]    = slave[j].aw_qos;
      assign s_slave_aw_valid[j]  = slave[j].aw_valid;
      assign                        slave[j].aw_ready = s_slave_aw_ready[j];
      assign s_slave_ar_id[j]     = slave[j].ar_id[AXI_ID_WIDTH_TARG-1:0];
      assign s_slave_ar_addr[j]   = slave[j].ar_addr;
      assign s_slave_ar_len[j]    = slave[j].ar_len;
      assign s_slave_ar_size[j]   = slave[j].ar_size;
      assign s_slave_ar_burst[j]  = slave[j].ar_burst;
      assign s_slave_ar_lock[j]   = slave[j].ar_lock;
      assign s_slave_ar_cache[j]  = slave[j].ar_cache;
      assign s_slave_ar_prot[j]   = slave[j].ar_prot;
      assign s_slave_ar_region[j] = slave[j].ar_region;
      assign s_slave_ar_user[j]   = slave[j].ar_user;
      assign s_slave_ar_qos[j]    = slave[j].ar_qos;
      assign s_slave_ar_valid[j]  = slave[j].ar_valid;
      assign                        slave[j].ar_ready = s_slave_ar_ready[j];
      assign s_slave_w_data[j]    = slave[j].w_data;
      assign s_slave_w_strb[j]    = slave[j].w_strb;
      assign s_slave_w_last[j]    = slave[j].w_last;
      assign s_slave_w_user[j]    = slave[j].w_user;
      assign s_slave_w_valid[j]   = slave[j].w_valid;
      assign                        slave[j].w_ready = s_slave_w_ready[j];
      assign                        slave[j].b_id[AXI_ID_WIDTH_TARG-1:0] = s_slave_b_id[j];
      assign                        slave[j].b_resp                      = s_slave_b_resp[j];
      assign                        slave[j].b_valid                     = s_slave_b_valid[j];
      assign                        slave[j].b_user                      = s_slave_b_user[j];
      assign s_slave_b_ready[j]   = slave[j].b_ready;
      assign                        slave[j].r_id[AXI_ID_WIDTH_TARG-1:0] = s_slave_r_id[j];
      assign                        slave[j].r_data                      = s_slave_r_data[j];
      assign                        slave[j].r_resp                      = s_slave_r_resp[j];
      assign                        slave[j].r_last                      = s_slave_r_last[j];
      assign                        slave[j].r_user                      = s_slave_r_user[j];
      assign                        slave[j].r_valid                     = s_slave_r_valid[j];
      assign s_slave_r_ready[j]   = slave[j].r_ready;
    end
  endgenerate
  axi_node
  #(
    .AXI_ADDRESS_W      ( AXI_ADDR_WIDTH    ),
    .AXI_DATA_W         ( AXI_DATA_WIDTH    ),
    .N_MASTER_PORT      ( NB_MASTER         ),
    .N_SLAVE_PORT       ( NB_SLAVE          ),
    .AXI_ID_IN          ( AXI_ID_WIDTH_TARG ),
    .AXI_USER_W         ( AXI_USER_WIDTH    ),
    .N_REGION           ( NB_REGION         )
  )
  axi_node_i
  (
    .clk                    ( clk                ),
    .rst_n                  ( rst_n              ),
    .test_en_i              ( test_en_i          ),
    .slave_awid_i           ( s_slave_aw_id      ),
    .slave_awaddr_i         ( s_slave_aw_addr    ),
    .slave_awlen_i          ( s_slave_aw_len     ),
    .slave_awsize_i         ( s_slave_aw_size    ),
    .slave_awburst_i        ( s_slave_aw_burst   ),
    .slave_awlock_i         ( s_slave_aw_lock    ),
    .slave_awcache_i        ( s_slave_aw_cache   ),
    .slave_awprot_i         ( s_slave_aw_prot    ),
    .slave_awregion_i       ( s_slave_aw_region  ),
    .slave_awatop_i         ( s_slave_aw_atop    ),
    .slave_awqos_i          ( s_slave_aw_qos     ),
    .slave_awuser_i         ( s_slave_aw_user    ),
    .slave_awvalid_i        ( s_slave_aw_valid   ),
    .slave_awready_o        ( s_slave_aw_ready   ),
    .slave_wdata_i          ( s_slave_w_data     ),
    .slave_wstrb_i          ( s_slave_w_strb     ),
    .slave_wlast_i          ( s_slave_w_last     ),
    .slave_wuser_i          ( s_slave_w_user     ),
    .slave_wvalid_i         ( s_slave_w_valid    ),
    .slave_wready_o         ( s_slave_w_ready    ),
    .slave_bid_o            ( s_slave_b_id       ),
    .slave_bresp_o          ( s_slave_b_resp     ),
    .slave_buser_o          ( s_slave_b_user     ),
    .slave_bvalid_o         ( s_slave_b_valid    ),
    .slave_bready_i         ( s_slave_b_ready    ),
    .slave_arid_i           ( s_slave_ar_id      ),
    .slave_araddr_i         ( s_slave_ar_addr    ),
    .slave_arlen_i          ( s_slave_ar_len     ),
    .slave_arsize_i         ( s_slave_ar_size    ),
    .slave_arburst_i        ( s_slave_ar_burst   ),
    .slave_arlock_i         ( s_slave_ar_lock    ),
    .slave_arcache_i        ( s_slave_ar_cache   ),
    .slave_arprot_i         ( s_slave_ar_prot    ),
    .slave_arregion_i       ( s_slave_ar_region  ),
    .slave_aruser_i         ( s_slave_ar_user    ),
    .slave_arqos_i          ( s_slave_ar_qos     ),
    .slave_arvalid_i        ( s_slave_ar_valid   ),
    .slave_arready_o        ( s_slave_ar_ready   ),
    .slave_rid_o            ( s_slave_r_id       ),
    .slave_rdata_o          ( s_slave_r_data     ),
    .slave_rresp_o          ( s_slave_r_resp     ),
    .slave_rlast_o          ( s_slave_r_last     ),
    .slave_ruser_o          ( s_slave_r_user     ),
    .slave_rvalid_o         ( s_slave_r_valid    ),
    .slave_rready_i         ( s_slave_r_ready    ),
    .master_awid_o          ( s_master_aw_id     ),
    .master_awaddr_o        ( s_master_aw_addr   ),
    .master_awlen_o         ( s_master_aw_len    ),
    .master_awsize_o        ( s_master_aw_size   ),
    .master_awburst_o       ( s_master_aw_burst  ),
    .master_awlock_o        ( s_master_aw_lock   ),
    .master_awcache_o       ( s_master_aw_cache  ),
    .master_awprot_o        ( s_master_aw_prot   ),
    .master_awregion_o      ( s_master_aw_region ),
    .master_awatop_o        ( s_master_aw_atop   ),
    .master_awqos_o         ( s_master_aw_qos    ),
    .master_awuser_o        ( s_master_aw_user   ),
    .master_awvalid_o       ( s_master_aw_valid  ),
    .master_awready_i       ( s_master_aw_ready  ),
    .master_wdata_o         ( s_master_w_data    ),
    .master_wstrb_o         ( s_master_w_strb    ),
    .master_wlast_o         ( s_master_w_last    ),
    .master_wuser_o         ( s_master_w_user    ),
    .master_wvalid_o        ( s_master_w_valid   ),
    .master_wready_i        ( s_master_w_ready   ),
    .master_bid_i           ( s_master_b_id      ),
    .master_bresp_i         ( s_master_b_resp    ),
    .master_buser_i         ( s_master_b_user    ),
    .master_bvalid_i        ( s_master_b_valid   ),
    .master_bready_o        ( s_master_b_ready   ),
    .master_arid_o          ( s_master_ar_id     ),
    .master_araddr_o        ( s_master_ar_addr   ),
    .master_arlen_o         ( s_master_ar_len    ),
    .master_arsize_o        ( s_master_ar_size   ),
    .master_arburst_o       ( s_master_ar_burst  ),
    .master_arlock_o        ( s_master_ar_lock   ),
    .master_arcache_o       ( s_master_ar_cache  ),
    .master_arprot_o        ( s_master_ar_prot   ),
    .master_arregion_o      ( s_master_ar_region ),
    .master_aruser_o        ( s_master_ar_user   ),
    .master_arqos_o         ( s_master_ar_qos    ),
    .master_arvalid_o       ( s_master_ar_valid  ),
    .master_arready_i       ( s_master_ar_ready  ),
    .master_rid_i           ( s_master_r_id      ),
    .master_rdata_i         ( s_master_r_data    ),
    .master_rresp_i         ( s_master_r_resp    ),
    .master_rlast_i         ( s_master_r_last    ),
    .master_ruser_i         ( s_master_r_user    ),
    .master_rvalid_i        ( s_master_r_valid   ),
    .master_rready_o        ( s_master_r_ready   ),
    .cfg_START_ADDR_i       ( start_addr_i       ),
    .cfg_END_ADDR_i         ( end_addr_i         ),
    .cfg_valid_rule_i       ( valid_rule_i       ),
    .cfg_connectivity_map_i ( {NB_MASTER*NB_SLAVE{1'b1}} )  
  );
endmodule
module axi_request_block
#(
    parameter                   AXI_ADDRESS_W  = 32,
    parameter                   AXI_DATA_W     = 64,
    parameter                   AXI_NUMBYTES   = AXI_DATA_W/8,
    parameter                   AXI_USER_W     = 6,
    parameter                   N_INIT_PORT    = 5,
    parameter                   N_TARG_PORT    = 8,
    parameter                   FIFO_DW_DEPTH  = 8,
    parameter                   AXI_ID_IN      = 16,
    parameter                   LOG_N_TARG     = $clog2(N_TARG_PORT),
    parameter                   AXI_ID_OUT     = AXI_ID_IN + LOG_N_TARG
)
(
  input logic                                                           clk,
  input logic                                                           rst_n,
  input logic                                                           test_en_i,
  input  logic [N_TARG_PORT-1:0][AXI_ID_IN-1:0]                         awid_i,         
  input  logic [N_TARG_PORT-1:0][AXI_ADDRESS_W-1:0]                     awaddr_i,       
  input  logic [N_TARG_PORT-1:0][ 7:0]                                  awlen_i,         
  input  logic [N_TARG_PORT-1:0][ 2:0]                                  awsize_i,        
  input  logic [N_TARG_PORT-1:0][ 1:0]                                  awburst_i,       
  input  logic [N_TARG_PORT-1:0]                                        awlock_i,        
  input  logic [N_TARG_PORT-1:0][ 3:0]                                  awcache_i,      
  input  logic [N_TARG_PORT-1:0][ 2:0]                                  awprot_i,       
  input  logic [N_TARG_PORT-1:0][ 3:0]                                  awregion_i,     
  input  logic [N_TARG_PORT-1:0][ 5:0]                                  awatop_i,       
  input  logic [N_TARG_PORT-1:0][ AXI_USER_W-1:0]                       awuser_i,       
  input  logic [N_TARG_PORT-1:0][ 3:0]                                  awqos_i,        
  input  logic [N_TARG_PORT-1:0]                                        awvalid_i,       
  output logic [N_TARG_PORT-1:0]                                        awready_o,       
  input  logic [N_TARG_PORT-1:0] [AXI_DATA_W-1:0]                       wdata_i,
  input  logic [N_TARG_PORT-1:0] [AXI_NUMBYTES-1:0]                     wstrb_i,         
  input  logic [N_TARG_PORT-1:0]                                        wlast_i,         
  input  logic [N_TARG_PORT-1:0] [AXI_USER_W-1:0]                       wuser_i,
  input  logic [N_TARG_PORT-1:0]                                        wvalid_i,        
  output logic [N_TARG_PORT-1:0]                                        wready_o,        
  input  logic [N_TARG_PORT-1:0][ AXI_ID_IN-1:0]                        arid_i,
  input  logic [N_TARG_PORT-1:0][ AXI_ADDRESS_W-1:0]                    araddr_i,
  input  logic [N_TARG_PORT-1:0][ 7:0]                                  arlen_i,         
  input  logic [N_TARG_PORT-1:0][ 2:0]                                  arsize_i,        
  input  logic [N_TARG_PORT-1:0][ 1:0]                                  arburst_i,       
  input  logic [N_TARG_PORT-1:0]                                        arlock_i,        
  input  logic [N_TARG_PORT-1:0][ 3:0]                                  arcache_i,
  input  logic [N_TARG_PORT-1:0][ 2:0]                                  arprot_i,
  input  logic [N_TARG_PORT-1:0][ 3:0]                                  arregion_i,     
  input  logic [N_TARG_PORT-1:0][ AXI_USER_W-1:0]                       aruser_i,       
  input  logic [N_TARG_PORT-1:0][ 3:0]                                  arqos_i,        
  input  logic [N_TARG_PORT-1:0]                                        arvalid_i,       
  output logic [N_TARG_PORT-1:0]                                        arready_o,       
  input  logic [AXI_ID_OUT-1:0]                                         bid_i,
  input  logic                                                          bvalid_i,
  output logic                                                          bready_o,
  output logic [N_TARG_PORT-1:0]                                        bvalid_o,
  input  logic [N_TARG_PORT-1:0]                                        bready_i,
  input  logic [AXI_ID_OUT-1:0]                                         rid_i,
  input  logic                                                          rvalid_i,   
  output logic                                                          rready_o,   
  output logic [N_TARG_PORT-1:0]                                        rvalid_o,
  input  logic [N_TARG_PORT-1:0]                                        rready_i,
  output  logic [AXI_ID_OUT-1:0]                                        awid_o,         
  output  logic [AXI_ADDRESS_W-1:0]                                     awaddr_o,       
  output  logic [ 7:0]                                                  awlen_o,         
  output  logic [ 2:0]                                                  awsize_o,        
  output  logic [ 1:0]                                                  awburst_o,       
  output  logic                                                         awlock_o,        
  output  logic [ 3:0]                                                  awcache_o,      
  output  logic [ 2:0]                                                  awprot_o,       
  output  logic [ 3:0]                                                  awregion_o,     
  output  logic [ 5:0]                                                  awatop_o,       
  output  logic [ AXI_USER_W-1:0]                                       awuser_o,       
  output  logic [ 3:0]                                                  awqos_o,        
  output  logic                                                         awvalid_o,       
  input   logic                                                         awready_i,       
  output  logic  [AXI_DATA_W-1:0]                                       wdata_o,
  output  logic  [AXI_NUMBYTES-1:0]                                     wstrb_o,         
  output  logic                                                         wlast_o,         
  output  logic [AXI_USER_W-1:0]                                        wuser_o,
  output  logic                                                         wvalid_o,        
  input   logic                                                         wready_i,        
  output  logic [ AXI_ID_OUT-1:0]                                       arid_o,
  output  logic [ AXI_ADDRESS_W-1:0]                                    araddr_o,
  output  logic [ 7:0]                                                  arlen_o,         
  output  logic [ 2:0]                                                  arsize_o,        
  output  logic [ 1:0]                                                  arburst_o,       
  output  logic                                                         arlock_o,        
  output  logic [ 3:0]                                                  arcache_o,
  output  logic [ 2:0]                                                  arprot_o,
  output  logic [ 3:0]                                                  arregion_o,     
  output  logic [ AXI_USER_W-1:0]                                       aruser_o,       
  output  logic [ 3:0]                                                  arqos_o,        
  output  logic                                                         arvalid_o,       
  input   logic                                                         arready_i        
);
logic                                           push_ID;
logic [LOG_N_TARG+N_TARG_PORT-1:0]              ID;
logic                                           grant_FIFO_ID;
axi_AR_allocator
#(
    .AXI_ADDRESS_W (  AXI_ADDRESS_W  ),
    .AXI_USER_W    (  AXI_USER_W     ),
    .N_TARG_PORT   (  N_TARG_PORT    ),
    .AXI_ID_IN     (  AXI_ID_IN      )
)
AR_ALLOCATOR
(
  .clk       (  clk        ),
  .rst_n     (  rst_n      ),
  .arid_i    (  arid_i     ),  
  .araddr_i  (  araddr_i   ),  
  .arlen_i   (  arlen_i    ),   
  .arsize_i  (  arsize_i   ),   
  .arburst_i (  arburst_i  ),   
  .arlock_i  (  arlock_i   ),   
  .arcache_i (  arcache_i  ),  
  .arprot_i  (  arprot_i   ),  
  .arregion_i(  arregion_i ),  
  .aruser_i  (  aruser_i   ),  
  .arqos_i   (  arqos_i    ),  
  .arvalid_i (  arvalid_i  ),   
  .arready_o (  arready_o  ),   
  .arid_o    (  arid_o    ),
  .araddr_o  (  araddr_o  ),
  .arlen_o   (  arlen_o   ),    
  .arsize_o  (  arsize_o  ),   
  .arburst_o (  arburst_o ),  
  .arlock_o  (  arlock_o  ),   
  .arcache_o (  arcache_o ),
  .arprot_o  (  arprot_o  ),
  .arregion_o(  arregion_o),      
  .aruser_o  (  aruser_o  ),      
  .arqos_o   (  arqos_o   ),      
  .arvalid_o (  arvalid_o ),  
  .arready_i (  arready_i )   
);
axi_AW_allocator
#(
    .AXI_ADDRESS_W  (  AXI_ADDRESS_W ),
    .AXI_USER_W     (  AXI_USER_W    ),
    .N_TARG_PORT    (  N_TARG_PORT   ),
    .AXI_ID_IN      (  AXI_ID_IN     )
)
AW_ALLOCATOR
(
  .clk        (  clk        ),
  .rst_n      (  rst_n      ),
  .awid_i     (  awid_i     ),    
  .awaddr_i   (  awaddr_i   ),    
  .awlen_i    (  awlen_i    ),     
  .awsize_i   (  awsize_i   ),     
  .awburst_i  (  awburst_i  ),     
  .awlock_i   (  awlock_i   ),     
  .awcache_i  (  awcache_i  ),    
  .awprot_i   (  awprot_i   ),    
  .awregion_i (  awregion_i ),    
  .awatop_i   (  awatop_i   ),    
  .awuser_i   (  awuser_i   ),    
  .awqos_i    (  awqos_i    ),    
  .awvalid_i  (  awvalid_i  ),     
  .awready_o  (  awready_o  ),     
  .awid_o     (  awid_o    ),      
  .awaddr_o   (  awaddr_o  ),     
  .awlen_o    (  awlen_o   ),      
  .awsize_o   (  awsize_o  ),      
  .awburst_o  (  awburst_o ),      
  .awlock_o   (  awlock_o  ),    
  .awcache_o  (  awcache_o ),  
  .awprot_o   (  awprot_o  ),     
  .awregion_o (  awregion_o ),    
  .awatop_o   (  awatop_o   ),    
  .awuser_o   (  awuser_o   ),    
  .awqos_o    (  awqos_o    ),    
  .awvalid_o  (  awvalid_o  ),     
  .awready_i  (  awready_i  ),     
  .push_ID_o  (  push_ID),
  .ID_o       (  ID     ),   
  .grant_FIFO_ID_i ( grant_FIFO_ID )
);
axi_DW_allocator
#(
    .AXI_USER_W     (AXI_USER_W),
    .N_TARG_PORT    (N_TARG_PORT),
    .FIFO_DEPTH     (FIFO_DW_DEPTH),
    .AXI_DATA_W     (AXI_DATA_W)
)
DW_ALLOC
(
  .clk        (  clk       ),
  .rst_n      (  rst_n     ),
  .test_en_i  ( test_en_i  ),
  .wdata_i    (  wdata_i   ),
  .wstrb_i    (  wstrb_i   ),     
  .wlast_i    (  wlast_i   ),     
  .wuser_i    (  wuser_i   ),     
  .wvalid_i   (  wvalid_i  ),    
  .wready_o   (  wready_o  ),    
  .wdata_o    (  wdata_o   ),
  .wstrb_o    (  wstrb_o   ),     
  .wlast_o    (  wlast_o   ),     
  .wuser_o    (  wuser_o   ),     
  .wvalid_o   (  wvalid_o  ),    
  .wready_i   (  wready_i  ),    
  .push_ID_i        (  push_ID  ),
  .ID_i             (  ID       ),   
  .grant_FIFO_ID_o  (  grant_FIFO_ID )
);
axi_address_decoder_BW
#(
   .N_TARG_PORT  (N_TARG_PORT ),
   .AXI_ID_IN    (AXI_ID_IN )
)
BW_DECODER
(
  .bid_i(bid_i),
  .bvalid_i(bvalid_i),
  .bready_o(bready_o),
  .bvalid_o(bvalid_o),
  .bready_i(bready_i)
);
axi_address_decoder_BR
#(
   .N_TARG_PORT  (N_TARG_PORT ),
   .AXI_ID_IN    (AXI_ID_IN )
)
BR_DECODER
(
  .rid_i(rid_i),
  .rvalid_i(rvalid_i),
  .rready_o(rready_o),
  .rvalid_o(rvalid_o),
  .rready_i(rready_i)
);
endmodule
module axi_response_block
#(
   parameter           AXI_ADDRESS_W  = 32,
   parameter           AXI_DATA_W     = 64,
   parameter           AXI_USER_W     = 6,
   parameter           N_INIT_PORT    = 4,
   parameter           N_TARG_PORT    = 8,
   parameter           FIFO_DEPTH_DW  = 8,
   parameter           AXI_ID_IN      = 16,
   parameter           AXI_ID_OUT     = AXI_ID_IN + $clog2(N_TARG_PORT),
   parameter           N_REGION       = 2
)
(
   input logic                                                       clk,
   input logic                                                       rst_n,
   input logic                                                       test_en_i,
   input  logic [N_INIT_PORT-1:0][AXI_ID_OUT-1:0]                    rid_i,
   input  logic [N_INIT_PORT-1:0][AXI_DATA_W-1:0]                    rdata_i,
   input  logic [N_INIT_PORT-1:0][ 1:0]                              rresp_i,
   input  logic [N_INIT_PORT-1:0]                                    rlast_i,    
   input  logic [N_INIT_PORT-1:0][AXI_USER_W-1:0]                    ruser_i,    
   input  logic [N_INIT_PORT-1:0]                                    rvalid_i,   
   output logic [N_INIT_PORT-1:0]                                    rready_o,    
   input  logic [N_INIT_PORT-1:0][AXI_ID_OUT-1:0]                    bid_i,
   input  logic [N_INIT_PORT-1:0][ 1:0]                              bresp_i,
   input  logic [N_INIT_PORT-1:0][AXI_USER_W-1:0]                    buser_i,    
   input  logic [N_INIT_PORT-1:0]                                    bvalid_i,   
   output logic [N_INIT_PORT-1:0]                                    bready_o,    
   output  logic [AXI_ID_IN-1:0]                                     rid_o,
   output  logic [AXI_DATA_W-1:0]                                    rdata_o,
   output  logic [ 1:0]                                              rresp_o,
   output  logic                                                     rlast_o,    
   output  logic [AXI_USER_W-1:0]                                    ruser_o,
   output  logic                                                     rvalid_o,   
   input   logic                                                     rready_i,    
   output  logic [AXI_ID_IN-1:0]                                     bid_o,
   output  logic [ 1:0]                                              bresp_o,
   output  logic [AXI_USER_W-1:0]                                    buser_o,    
   output  logic                                                     bvalid_o,   
   input   logic                                                     bready_i,    
   input  logic                                                      arvalid_i,
   input  logic [AXI_ADDRESS_W-1:0]                                  araddr_i,
   output logic                                                      arready_o,
   input  logic [AXI_ID_IN-1:0]                                      arid_i,
   input  logic [ 7:0]                                               arlen_i,
   input  logic [ AXI_USER_W-1:0]                                    aruser_i,
   output logic [N_INIT_PORT-1:0]                                    arvalid_o,
   input  logic [N_INIT_PORT-1:0]                                    arready_i,
   input  logic                                                      awvalid_i,
   input  logic [AXI_ADDRESS_W-1:0]                                  awaddr_i,
   output logic                                                      awready_o,
   input  logic [AXI_ID_IN-1:0]                                      awid_i,
   input  logic [AXI_USER_W-1:0]                                     awuser_i,
   output logic [N_INIT_PORT-1:0]                                    awvalid_o,
   input  logic [N_INIT_PORT-1:0]                                    awready_i,
   input  logic                                                      wvalid_i,
   input  logic                                                      wlast_i,
   output logic                                                      wready_o,
   output logic [N_INIT_PORT-1:0]                                    wvalid_o,
   input  logic [N_INIT_PORT-1:0]                                    wready_i,
   input  logic [N_REGION-1:0][N_INIT_PORT-1:0][AXI_ADDRESS_W-1:0]   START_ADDR_i,
   input  logic [N_REGION-1:0][N_INIT_PORT-1:0][AXI_ADDRESS_W-1:0]   END_ADDR_i,
   input  logic [N_REGION-1:0][N_INIT_PORT-1:0]                      enable_region_i,
   input  logic [N_INIT_PORT-1:0]                                    connectivity_map_i
);
logic               push_DEST_DW;
logic               grant_FIFO_DEST_DW;
logic  [N_INIT_PORT-1:0]    DEST_DW;
logic                               incr_ar_req;
logic                               full_counter_ar;
logic                               outstanding_trans_ar;
logic                               error_ar_req;
logic                               error_ar_gnt;
logic                               incr_aw_req;
logic                               full_counter_aw;
logic                               outstanding_trans_aw;
logic                               handle_error_aw;
logic                               wdata_error_completed;
logic                               sample_awdata_info;
logic                               sample_ardata_info;
logic                               error_aw_req;
logic                               error_aw_gnt;
axi_BW_allocator
#(
      .AXI_USER_W  ( AXI_USER_W   ),
      .N_INIT_PORT ( N_INIT_PORT  ),
      .N_TARG_PORT ( N_TARG_PORT  ),
      .AXI_DATA_W  ( AXI_DATA_W   ),
      .AXI_ID_IN   ( AXI_ID_IN    )
)
BW_ALLOC
(
      .clk                  (  clk                  ),
      .rst_n                (  rst_n                ),
      .bid_i                (  bid_i                ),
      .bresp_i              (  bresp_i              ),
      .buser_i              (  buser_i              ),    
      .bvalid_i             (  bvalid_i             ),   
      .bready_o             (  bready_o             ),    
      .bid_o                (  bid_o                ),
      .bresp_o              (  bresp_o              ),
      .buser_o              (  buser_o              ),   
      .bvalid_o             (  bvalid_o             ),   
      .bready_i             (  bready_i             ),   
      .incr_req_i           (  incr_aw_req          ),
      .full_counter_o       (  full_counter_aw      ),
      .outstanding_trans_o  (  outstanding_trans_aw ),
      .sample_awdata_info_i (  sample_awdata_info   ),
      .error_req_i          (  error_aw_req         ),
      .error_gnt_o          (  error_aw_gnt         ),
      .error_user_i         (  awuser_i             ),
      .error_id_i           (  awid_i               )
);
axi_BR_allocator
#(
      .AXI_USER_W  ( AXI_USER_W   ),
      .N_INIT_PORT ( N_INIT_PORT  ),
      .N_TARG_PORT ( N_TARG_PORT  ),
      .AXI_DATA_W  ( AXI_DATA_W   ),
      .AXI_ID_IN   ( AXI_ID_IN    )
)
BR_ALLOC
(
      .clk                  ( clk                   ),
      .rst_n                ( rst_n                 ),
      .rid_i                ( rid_i                 ),
      .rdata_i              ( rdata_i               ),
      .rresp_i              ( rresp_i               ),
      .rlast_i              ( rlast_i               ),
      .ruser_i              ( ruser_i               ),    
      .rvalid_i             ( rvalid_i              ),   
      .rready_o             ( rready_o              ),    
      .rid_o                ( rid_o                 ),
      .rdata_o              ( rdata_o               ),
      .rresp_o              ( rresp_o               ),
      .rlast_o              ( rlast_o               ),
      .ruser_o              ( ruser_o               ),    
      .rvalid_o             ( rvalid_o              ),   
      .rready_i             ( rready_i              ),    
      .incr_req_i           ( incr_ar_req           ),
      .full_counter_o       ( full_counter_ar       ),
      .outstanding_trans_o  ( outstanding_trans_ar  ),
      .error_req_i          ( error_ar_req          ),
      .error_gnt_o          ( error_ar_gnt          ),
      .error_len_i          ( arlen_i               ),
      .error_user_i         ( aruser_i              ),
      .error_id_i           ( arid_i                ),
      .sample_ardata_info_i ( sample_ardata_info    )
);
axi_address_decoder_AR
#(
    .ADDR_WIDTH   (  AXI_ADDRESS_W   ),
    .N_INIT_PORT  (  N_INIT_PORT     ),
    .N_REGION     (  N_REGION        )
)
AR_ADDR_DEC
(
    .clk                   ( clk                   ),
    .rst_n                 ( rst_n                 ),
    .arvalid_i             ( arvalid_i             ),
    .araddr_i              ( araddr_i              ),
    .arready_o             ( arready_o             ),
    .arvalid_o             ( arvalid_o             ),  
    .arready_i             ( arready_i             ),  
    .START_ADDR_i          ( START_ADDR_i          ),
    .END_ADDR_i            ( END_ADDR_i            ),
    .enable_region_i       ( enable_region_i       ),
    .connectivity_map_i    ( connectivity_map_i    ),
    .incr_req_o            ( incr_ar_req           ),
    .full_counter_i        ( full_counter_ar       ),
    .outstanding_trans_i   ( outstanding_trans_ar  ),
    .error_req_o           ( error_ar_req          ),
    .error_gnt_i           ( error_ar_gnt          ),
    .sample_ardata_info_o  ( sample_ardata_info    )
);
axi_address_decoder_AW
#(
    .ADDR_WIDTH      (  AXI_ADDRESS_W     ),
    .N_INIT_PORT     (  N_INIT_PORT       ),
    .N_REGION        (  N_REGION          )
)
AW_ADDR_DEC
(
    .clk                      ( clk                   ),
    .rst_n                    ( rst_n                 ),
    .awvalid_i                ( awvalid_i             ),
    .awaddr_i                 ( awaddr_i              ),
    .awready_o                ( awready_o             ),
    .awvalid_o                ( awvalid_o             ),  
    .awready_i                ( awready_i             ),  
    .grant_FIFO_DEST_i        ( grant_FIFO_DEST_DW    ),
    .DEST_o                   ( DEST_DW               ),
    .push_DEST_o              ( push_DEST_DW          ),
    .START_ADDR_i             ( START_ADDR_i          ),
    .END_ADDR_i               ( END_ADDR_i            ),
    .enable_region_i          ( enable_region_i       ),
    .connectivity_map_i       ( connectivity_map_i    ),
    .incr_req_o               ( incr_aw_req           ),
    .full_counter_i           ( full_counter_aw       ),
    .outstanding_trans_i      ( outstanding_trans_aw  ),
    .error_req_o              ( error_aw_req          ),
    .error_gnt_i              ( error_aw_gnt          ),
    .handle_error_o           ( handle_error_aw       ),
    .wdata_error_completed_i  ( wdata_error_completed ),
    .sample_awdata_info_o     ( sample_awdata_info    )
);
axi_address_decoder_DW
#(
    .N_INIT_PORT       (  N_INIT_PORT       ),
    .FIFO_DEPTH        (  FIFO_DEPTH_DW     )
)
DW_ADDR_DEC
(
    .clk                      ( clk                    ),
    .rst_n                    ( rst_n                  ),
    .test_en_i                ( test_en_i              ),
    .wvalid_i                 ( wvalid_i               ),
    .wlast_i                  ( wlast_i                ),
    .wready_o                 ( wready_o               ),
    .wvalid_o                 ( wvalid_o               ),
    .wready_i                 ( wready_i               ),
    .grant_FIFO_DEST_o        ( grant_FIFO_DEST_DW     ),
    .DEST_i                   ( DEST_DW                ),
    .push_DEST_i              ( push_DEST_DW           ),
    .handle_error_i           ( handle_error_aw        ),
    .wdata_error_completed_o  ( wdata_error_completed  )
);
endmodule
module axi_res_tbl #(
    parameter int unsigned AXI_ADDR_WIDTH = 0,
    parameter int unsigned AXI_ID_WIDTH = 0
) (
    input  logic                        clk_i,
    input  logic                        rst_ni,
    input  logic [AXI_ADDR_WIDTH-1:0]   clr_addr_i,
    input  logic                        clr_req_i,
    output logic                        clr_gnt_o,
    input  logic [AXI_ADDR_WIDTH-1:0]   set_addr_i,
    input  logic [AXI_ID_WIDTH-1:0]     set_id_i,
    input  logic                        set_req_i,
    output logic                        set_gnt_o,
    input  logic [AXI_ADDR_WIDTH-1:0]   check_addr_i,
    input  logic [AXI_ID_WIDTH-1:0]     check_id_i,
    output logic                        check_res_o,
    input  logic                        check_req_i,
    output logic                        check_gnt_o
);
    localparam integer N_IDS = 2**AXI_ID_WIDTH;
    logic [N_IDS-1:0][AXI_ADDR_WIDTH-1:0]   tbl_d,                      tbl_q;
    logic                                   clr,
                                            set;
    generate for (genvar i = 0; i < N_IDS; ++i) begin: gen_tbl
        always_comb begin
            tbl_d[i] = tbl_q[i];
            if (set && i == set_id_i) begin
                tbl_d[i] = set_addr_i;
            end else if (clr && tbl_q[i] == clr_addr_i) begin
                tbl_d[i] = '0;
            end
        end
    end endgenerate
    always_comb begin
        clr         = 1'b0;
        set         = 1'b0;
        clr_gnt_o   = 1'b0;
        set_gnt_o   = 1'b0;
        check_res_o = 1'b0;
        check_gnt_o = 1'b0;
        if (clr_req_i) begin
            clr         = 1'b1;
            clr_gnt_o   = 1'b1;
        end else if (set_req_i) begin
            set         = 1'b1;
            set_gnt_o   = 1'b1;
        end else if (check_req_i) begin
            check_res_o = (tbl_q[check_id_i] == check_addr_i);
            check_gnt_o = 1'b1;
        end
    end
    always_ff @(posedge clk_i, negedge rst_ni) begin
        if (~rst_ni) begin
            tbl_q   <= '0;
        end else begin
            tbl_q   <= tbl_d;
        end
    end
endmodule
module axi_riscv_amos #(
    parameter int unsigned AXI_ADDR_WIDTH       = 0,
    parameter int unsigned AXI_DATA_WIDTH       = 0,
    parameter int unsigned AXI_ID_WIDTH         = 0,
    parameter int unsigned AXI_USER_WIDTH       = 0,
    parameter int unsigned AXI_MAX_WRITE_TXNS   = 0,
    parameter int unsigned RISCV_WORD_WIDTH     = 0,
    localparam int unsigned AXI_STRB_WIDTH      = AXI_DATA_WIDTH / 8
) (
    input  logic                        clk_i,
    input  logic                        rst_ni,
    input  logic [AXI_ADDR_WIDTH-1:0]   slv_aw_addr_i,
    input  logic [2:0]                  slv_aw_prot_i,
    input  logic [3:0]                  slv_aw_region_i,
    input  logic [5:0]                  slv_aw_atop_i,
    input  logic [7:0]                  slv_aw_len_i,
    input  logic [2:0]                  slv_aw_size_i,
    input  logic [1:0]                  slv_aw_burst_i,
    input  logic                        slv_aw_lock_i,
    input  logic [3:0]                  slv_aw_cache_i,
    input  logic [3:0]                  slv_aw_qos_i,
    input  logic [AXI_ID_WIDTH-1:0]     slv_aw_id_i,
    input  logic [AXI_USER_WIDTH-1:0]   slv_aw_user_i,
    output logic                        slv_aw_ready_o,
    input  logic                        slv_aw_valid_i,
    input  logic [AXI_ADDR_WIDTH-1:0]   slv_ar_addr_i,
    input  logic [2:0]                  slv_ar_prot_i,
    input  logic [3:0]                  slv_ar_region_i,
    input  logic [7:0]                  slv_ar_len_i,
    input  logic [2:0]                  slv_ar_size_i,
    input  logic [1:0]                  slv_ar_burst_i,
    input  logic                        slv_ar_lock_i,
    input  logic [3:0]                  slv_ar_cache_i,
    input  logic [3:0]                  slv_ar_qos_i,
    input  logic [AXI_ID_WIDTH-1:0]     slv_ar_id_i,
    input  logic [AXI_USER_WIDTH-1:0]   slv_ar_user_i,
    output logic                        slv_ar_ready_o,
    input  logic                        slv_ar_valid_i,
    input  logic [AXI_DATA_WIDTH-1:0]   slv_w_data_i,
    input  logic [AXI_STRB_WIDTH-1:0]   slv_w_strb_i,
    input  logic [AXI_USER_WIDTH-1:0]   slv_w_user_i,
    input  logic                        slv_w_last_i,
    output logic                        slv_w_ready_o,
    input  logic                        slv_w_valid_i,
    output logic [AXI_DATA_WIDTH-1:0]   slv_r_data_o,
    output logic [1:0]                  slv_r_resp_o,
    output logic                        slv_r_last_o,
    output logic [AXI_ID_WIDTH-1:0]     slv_r_id_o,
    output logic [AXI_USER_WIDTH-1:0]   slv_r_user_o,
    input  logic                        slv_r_ready_i,
    output logic                        slv_r_valid_o,
    output logic [1:0]                  slv_b_resp_o,
    output logic [AXI_ID_WIDTH-1:0]     slv_b_id_o,
    output logic [AXI_USER_WIDTH-1:0]   slv_b_user_o,
    input  logic                        slv_b_ready_i,
    output logic                        slv_b_valid_o,
    output logic [AXI_ADDR_WIDTH-1:0]   mst_aw_addr_o,
    output logic [2:0]                  mst_aw_prot_o,
    output logic [3:0]                  mst_aw_region_o,
    output logic [5:0]                  mst_aw_atop_o,
    output logic [7:0]                  mst_aw_len_o,
    output logic [2:0]                  mst_aw_size_o,
    output logic [1:0]                  mst_aw_burst_o,
    output logic                        mst_aw_lock_o,
    output logic [3:0]                  mst_aw_cache_o,
    output logic [3:0]                  mst_aw_qos_o,
    output logic [AXI_ID_WIDTH-1:0]     mst_aw_id_o,
    output logic [AXI_USER_WIDTH-1:0]   mst_aw_user_o,
    input  logic                        mst_aw_ready_i,
    output logic                        mst_aw_valid_o,
    output logic [AXI_ADDR_WIDTH-1:0]   mst_ar_addr_o,
    output logic [2:0]                  mst_ar_prot_o,
    output logic [3:0]                  mst_ar_region_o,
    output logic [7:0]                  mst_ar_len_o,
    output logic [2:0]                  mst_ar_size_o,
    output logic [1:0]                  mst_ar_burst_o,
    output logic                        mst_ar_lock_o,
    output logic [3:0]                  mst_ar_cache_o,
    output logic [3:0]                  mst_ar_qos_o,
    output logic [AXI_ID_WIDTH-1:0]     mst_ar_id_o,
    output logic [AXI_USER_WIDTH-1:0]   mst_ar_user_o,
    input  logic                        mst_ar_ready_i,
    output logic                        mst_ar_valid_o,
    output logic [AXI_DATA_WIDTH-1:0]   mst_w_data_o,
    output logic [AXI_STRB_WIDTH-1:0]   mst_w_strb_o,
    output logic [AXI_USER_WIDTH-1:0]   mst_w_user_o,
    output logic                        mst_w_last_o,
    input  logic                        mst_w_ready_i,
    output logic                        mst_w_valid_o,
    input  logic [AXI_DATA_WIDTH-1:0]   mst_r_data_i,
    input  logic [1:0]                  mst_r_resp_i,
    input  logic                        mst_r_last_i,
    input  logic [AXI_ID_WIDTH-1:0]     mst_r_id_i,
    input  logic [AXI_USER_WIDTH-1:0]   mst_r_user_i,
    output logic                        mst_r_ready_o,
    input  logic                        mst_r_valid_i,
    input  logic [1:0]                  mst_b_resp_i,
    input  logic [AXI_ID_WIDTH-1:0]     mst_b_id_i,
    input  logic [AXI_USER_WIDTH-1:0]   mst_b_user_i,
    output logic                        mst_b_ready_o,
    input  logic                        mst_b_valid_i
);
    localparam int unsigned OUTSTND_BURSTS_WIDTH = $clog2(AXI_MAX_WRITE_TXNS+1);
    localparam int unsigned AXI_ALU_RATIO        = AXI_DATA_WIDTH/RISCV_WORD_WIDTH;
    typedef enum logic [1:0] { FEEDTHROUGH_AW, WAIT_RESULT_AW, SEND_AW } aw_state_t;
    aw_state_t   aw_state_d, aw_state_q;
    typedef enum logic [2:0] { FEEDTHROUGH_W, WAIT_DATA_W, WAIT_RESULT_W, WAIT_CHANNEL_W, SEND_W } w_state_t;
    w_state_t    w_state_d, w_state_q;
    typedef enum logic [1:0] { FEEDTHROUGH_B, WAIT_COMPLETE_B, WAIT_CHANNEL_B, SEND_B } b_state_t;
    b_state_t    b_state_d, b_state_q;
    typedef enum logic [1:0] { FEEDTHROUGH_AR, WAIT_CHANNEL_AR, SEND_AR } ar_state_t;
    ar_state_t   ar_state_d, ar_state_q;
    typedef enum logic [1:0] { FEEDTHROUGH_R, WAIT_DATA_R, WAIT_CHANNEL_R, SEND_R } r_state_t;
    r_state_t    r_state_d, r_state_q;
    typedef enum logic [1:0] { NONE, INVALID, LOAD, STORE } atop_req_t;
    atop_req_t   atop_valid_d, atop_valid_q;
    logic [AXI_ADDR_WIDTH-1:0]          addr_d,         addr_q;
    logic [AXI_ID_WIDTH-1:0]            id_d,           id_q;
    logic [AXI_STRB_WIDTH-1:0]          strb_d,         strb_q;
    logic [2:0]                         size_d,         size_q;
    logic [5:0]                         atop_d,         atop_q;
    logic [3:0]                         cache_d,        cache_q;
    logic [2:0]                         prot_d,         prot_q;
    logic [3:0]                         qos_d,          qos_q;
    logic [3:0]                         region_d,       region_q;
    logic [1:0]                         r_resp_d,       r_resp_q;
    logic [AXI_USER_WIDTH-1:0]          aw_user_d,      aw_user_q,
                                        w_user_d,       w_user_q,
                                        r_user_d,       r_user_q;
    logic [AXI_DATA_WIDTH-1:0]          w_data_d,       w_data_q;        
    logic [AXI_DATA_WIDTH-1:0]          r_data_d,       r_data_q;        
    logic [AXI_DATA_WIDTH-1:0]          result_d,       result_q;        
    logic                               w_d_valid_d,    w_d_valid_q,     
                                        r_d_valid_d,    r_d_valid_q;     
    logic [OUTSTND_BURSTS_WIDTH-1:0]    w_cnt_d,        w_cnt_q;         
    logic [OUTSTND_BURSTS_WIDTH-1:0]    w_cnt_req_d,    w_cnt_req_q;     
    logic [OUTSTND_BURSTS_WIDTH-1:0]    w_cnt_inj_d,    w_cnt_inj_q;     
    logic                               adapter_ready;
    logic                               transaction_collision;
    logic                               aw_valid,       aw_ready,       aw_free,
                                        w_valid,        w_ready,        w_free,
                                        b_valid,        b_ready,        b_free,
                                        ar_valid,       ar_ready,       ar_free,
                                        r_valid,        r_ready,        r_free;
    logic [RISCV_WORD_WIDTH-1:0]                        alu_operand_a;
    logic [RISCV_WORD_WIDTH-1:0]                        alu_operand_b;
    logic [RISCV_WORD_WIDTH-1:0]                        alu_result;
    logic [AXI_DATA_WIDTH-1:0]                          alu_result_ext;
    logic [AXI_ALU_RATIO-1:0][RISCV_WORD_WIDTH-1:0]     op_a;
    logic [AXI_ALU_RATIO-1:0][RISCV_WORD_WIDTH-1:0]     op_b;
    logic [AXI_ALU_RATIO-1:0][RISCV_WORD_WIDTH-1:0]     op_a_sign_ext;
    logic [AXI_ALU_RATIO-1:0][RISCV_WORD_WIDTH-1:0]     op_b_sign_ext;
    logic [AXI_ALU_RATIO-1:0][RISCV_WORD_WIDTH-1:0]     res;
    logic [AXI_STRB_WIDTH-1:0][7:0]                     strb_ext;
    logic                                               sign_a;
    logic                                               sign_b;
    assign adapter_ready = (aw_state_q == FEEDTHROUGH_AW) &&
                           ( w_state_q == FEEDTHROUGH_W ) &&
                           ( b_state_q == FEEDTHROUGH_B ) &&
                           (ar_state_q == FEEDTHROUGH_AR) &&
                           ( r_state_q == FEEDTHROUGH_R );
    assign aw_free = ~aw_valid | aw_ready;
    assign  w_free = ~ w_valid |  w_ready;
    assign  b_free = ~ b_valid |  b_ready;
    assign ar_free = ~ar_valid | ar_ready;
    assign  r_free = ~ r_valid |  r_ready;
    always_ff @(posedge clk_i or negedge rst_ni) begin
        if(~rst_ni) begin
            aw_valid <= 0;
            aw_ready <= 0;
            w_valid  <= 0;
            w_ready  <= 0;
            b_valid  <= 0;
            b_ready  <= 0;
            ar_valid <= 0;
            ar_ready <= 0;
            r_valid  <= 0;
            r_ready  <= 0;
        end else begin
            aw_valid <= mst_aw_valid_o;
            aw_ready <= mst_aw_ready_i;
            w_valid  <= mst_w_valid_o;
            w_ready  <= mst_w_ready_i;
            b_valid  <= slv_b_valid_o;
            b_ready  <= slv_b_ready_i;
            ar_valid <= mst_ar_valid_o;
            ar_ready <= mst_ar_ready_i;
            r_valid  <= slv_r_valid_o;
            r_ready  <= slv_r_ready_i;
        end
    end
    assign transaction_collision = (slv_aw_addr_i < (     addr_q + (8'h01 <<      size_q))) &
                                   (     addr_q < (slv_aw_addr_i + (8'h01 << slv_aw_size_i)));
    always_comb begin : calc_atop_valid
        atop_valid_d = atop_valid_q;
        if (adapter_ready) begin
            atop_valid_d = NONE;
            if (slv_aw_valid_i && slv_aw_atop_i) begin
                atop_valid_d = INVALID;
                if ((slv_aw_atop_i      ==  axi_pkg::ATOP_ATOMICSWAP) ||
                    (slv_aw_atop_i[5:3] == {axi_pkg::ATOP_ATOMICLOAD , axi_pkg::ATOP_LITTLE_END})) begin
                    atop_valid_d = LOAD;
                end
                if (slv_aw_atop_i[5:3] == {axi_pkg::ATOP_ATOMICSTORE, axi_pkg::ATOP_LITTLE_END}) begin
                    atop_valid_d = STORE;
                end
                if (slv_aw_len_i | slv_aw_lock_i) begin
                    atop_valid_d = INVALID;
                end
                if (slv_aw_size_i > $clog2(RISCV_WORD_WIDTH/8)) begin
                    atop_valid_d = INVALID;
                end
            end
        end
    end
    always_ff @(posedge clk_i or negedge rst_ni) begin : proc_atop_valid
        if(~rst_ni) begin
            atop_valid_q <= NONE;
        end else begin
            atop_valid_q <= atop_valid_d;
        end
    end
    always_comb begin : axi_aw_channel
        mst_aw_id_o     = slv_aw_id_i;
        mst_aw_addr_o   = slv_aw_addr_i;
        mst_aw_len_o    = slv_aw_len_i;
        mst_aw_size_o   = slv_aw_size_i;
        mst_aw_burst_o  = slv_aw_burst_i;
        mst_aw_lock_o   = slv_aw_lock_i;
        mst_aw_cache_o  = slv_aw_cache_i;
        mst_aw_prot_o   = slv_aw_prot_i;
        mst_aw_qos_o    = slv_aw_qos_i;
        mst_aw_region_o = slv_aw_region_i;
        mst_aw_atop_o   = 6'b0;
        mst_aw_user_o   = slv_aw_user_i;
        addr_d          = addr_q;
        id_d            = id_q;
        size_d          = size_q;
        atop_d          = atop_q;
        cache_d         = cache_q;
        prot_d          = prot_q;
        qos_d           = qos_q;
        region_d        = region_q;
        aw_user_d       = aw_user_q;
        w_cnt_inj_d     = w_cnt_inj_q;
        aw_state_d      = aw_state_q;
        if (slv_aw_valid_i && slv_aw_atop_i) begin
            mst_aw_valid_o = 1'b0;
            slv_aw_ready_o = 1'b0;
        end else if (w_cnt_q == AXI_MAX_WRITE_TXNS) begin
            mst_aw_valid_o = 1'b0;
            slv_aw_ready_o = 1'b0;
        end else if (slv_aw_valid_i && transaction_collision && !adapter_ready) begin
            mst_aw_valid_o = 1'b0;
            slv_aw_ready_o = 1'b0;
        end else begin
            mst_aw_valid_o  = slv_aw_valid_i;
            slv_aw_ready_o  = mst_aw_ready_i;
        end
        if (w_cnt_inj_q && mst_w_valid_o && mst_w_ready_i && mst_w_last_o) begin
            w_cnt_inj_d = w_cnt_inj_q - 1;
        end
        unique case (aw_state_q)
            FEEDTHROUGH_AW: begin
                if (slv_aw_valid_i && slv_aw_atop_i && adapter_ready) begin
                    slv_aw_ready_o = 1'b1;
                    atop_d    = slv_aw_atop_i;
                    addr_d    = slv_aw_addr_i;
                    id_d      = slv_aw_id_i;
                    size_d    = slv_aw_size_i;
                    cache_d   = slv_aw_cache_i;
                    prot_d    = slv_aw_prot_i;
                    qos_d     = slv_aw_qos_i;
                    region_d  = slv_aw_region_i;
                    aw_user_d = slv_aw_user_i;
                    if (atop_valid_d != INVALID) begin
                        aw_state_d = WAIT_RESULT_AW;
                    end
                end
            end  
            WAIT_RESULT_AW, SEND_AW: begin
                if ((r_d_valid_q && w_d_valid_q && aw_free) || (aw_state_q == SEND_AW)) begin
                    slv_aw_ready_o  = 1'b0;
                    mst_aw_valid_o  = 1'b1;
                    mst_aw_addr_o   = addr_q;
                    mst_aw_len_o    = 8'h00;
                    mst_aw_id_o     = id_q;
                    mst_aw_size_o   = size_q;
                    mst_aw_burst_o  = 2'b00;
                    mst_aw_lock_o   = 1'b0;
                    mst_aw_cache_o  = cache_q;
                    mst_aw_prot_o   = prot_q;
                    mst_aw_qos_o    = qos_q;
                    mst_aw_region_o = region_q;
                    mst_aw_user_o   = aw_user_q;
                    if (mst_aw_ready_i) begin
                        aw_state_d = FEEDTHROUGH_AW;
                    end else begin
                        aw_state_d = SEND_AW;
                    end
                    if (aw_state_q == WAIT_RESULT_AW) begin
                        if (w_cnt_q && mst_w_valid_o && mst_w_ready_i && mst_w_last_o) begin
                            w_cnt_inj_d = w_cnt_q - 1;
                        end else begin
                            w_cnt_inj_d = w_cnt_q;
                        end
                    end
                end
            end  
            default: aw_state_d = FEEDTHROUGH_AW;
        endcase
    end  
    always_comb begin : axi_w_channel
        mst_w_data_o = slv_w_data_i;
        mst_w_strb_o = slv_w_strb_i;
        mst_w_last_o = slv_w_last_i;
        mst_w_user_o = slv_w_user_i;
        strb_d       = strb_q;
        w_user_d     = w_user_q;
        w_data_d     = w_data_q;
        result_d     = result_q;
        w_d_valid_d  = w_d_valid_q;
        w_cnt_req_d  = w_cnt_req_q;
        w_state_d    = w_state_q;
        if (w_cnt_q == 0) begin
            slv_w_ready_o = 1'b0;
            mst_w_valid_o = 1'b0;
        end else begin
            mst_w_valid_o = slv_w_valid_i;
            slv_w_ready_o = mst_w_ready_i;
        end
        unique case (w_state_q)
            FEEDTHROUGH_W: begin
                if (adapter_ready) begin
                    w_d_valid_d = 1'b0;
                    result_d    = '0;
                    if (atop_valid_d != NONE) begin
                        if (w_cnt_q == 0) begin
                            mst_w_valid_o = 1'b0;
                            slv_w_ready_o  = 1'b1;
                            if (slv_w_valid_i) begin
                                if (atop_valid_d != INVALID) begin
                                    w_data_d    = slv_w_data_i;
                                    strb_d      = slv_w_strb_i;
                                    w_user_d    = slv_w_user_i;
                                    w_d_valid_d = 1'b1;
                                    w_state_d   = WAIT_RESULT_W;
                                end
                            end else begin
                                w_cnt_req_d = '0;
                                w_state_d   = WAIT_DATA_W;
                            end
                        end else begin
                            if (mst_w_valid_o && mst_w_ready_i && mst_w_last_o) begin
                                w_cnt_req_d = w_cnt_q - 1;
                            end else begin
                                w_cnt_req_d = w_cnt_q;
                            end
                            w_state_d   = WAIT_DATA_W;
                        end
                    end
                end
            end  
            WAIT_DATA_W: begin
                if (w_cnt_req_q == 0) begin
                    mst_w_valid_o = 1'b0;
                    slv_w_ready_o = 1'b1;
                    if (slv_w_valid_i) begin
                        if (atop_valid_q == INVALID) begin
                            w_state_d    = FEEDTHROUGH_W;
                        end else begin
                            w_data_d    = slv_w_data_i;
                            strb_d      = slv_w_strb_i;
                            w_user_d    = slv_w_user_i;
                            w_d_valid_d = 1'b1;
                            w_state_d   = WAIT_RESULT_W;
                        end
                    end
                end else if (mst_w_valid_o && mst_w_ready_i && mst_w_last_o) begin
                    w_cnt_req_d = w_cnt_req_q - 1;
                end
            end  
            WAIT_RESULT_W: begin
                if (r_d_valid_q && w_d_valid_q && aw_free) begin
                    result_d = alu_result_ext;
                    if (w_free && w_cnt_q == 0) begin
                        slv_w_ready_o = 1'b0;
                        mst_w_valid_o = 1'b1;
                        mst_w_data_o  = alu_result_ext;
                        mst_w_last_o  = 1'b1;
                        mst_w_strb_o  = strb_q;
                        mst_w_user_o  = w_user_q;
                        if (mst_w_ready_i) begin
                            w_state_d = FEEDTHROUGH_W;
                        end else begin
                            w_state_d = SEND_W;
                        end
                    end else begin
                        w_state_d = WAIT_CHANNEL_W;
                    end
                end
            end  
            WAIT_CHANNEL_W, SEND_W: begin
                if ((w_free && w_cnt_inj_q == 0) || (w_state_q == SEND_W)) begin
                    slv_w_ready_o = 1'b0;
                    mst_w_valid_o = 1'b1;
                    mst_w_data_o  = result_q;
                    mst_w_last_o  = 1'b1;
                    mst_w_strb_o  = strb_q;
                    mst_w_user_o  = w_user_q;
                    if (mst_w_ready_i) begin
                        w_state_d = FEEDTHROUGH_W;
                    end else begin
                        w_state_d = SEND_W;
                    end
                end
            end  
            default: w_state_d = FEEDTHROUGH_W;
        endcase
    end  
    always_comb begin : axi_b_channel
        mst_b_ready_o = slv_b_ready_i;
        slv_b_id_o    = mst_b_id_i;
        slv_b_resp_o  = mst_b_resp_i;
        slv_b_user_o  = mst_b_user_i;
        slv_b_valid_o = mst_b_valid_i;
        b_state_d     = b_state_q;
        unique case (b_state_q)
            FEEDTHROUGH_B: begin
                if (adapter_ready) begin
                    if (atop_valid_d == LOAD || atop_valid_d == STORE) begin
                        b_state_d = WAIT_COMPLETE_B;
                    end else if (atop_valid_d == INVALID) begin
                        if (b_free) begin
                            mst_b_ready_o = 1'b0;
                            slv_b_valid_o = 1'b1;
                            slv_b_id_o    = slv_aw_id_i;
                            slv_b_resp_o  = axi_pkg::RESP_SLVERR;
                            slv_b_user_o  = '0;
                            if (!slv_b_ready_i) begin
                                b_state_d = SEND_B;
                            end
                        end else begin
                            b_state_d = WAIT_CHANNEL_B;
                        end
                    end
                end
            end  
            WAIT_CHANNEL_B, SEND_B: begin
                if (b_free || (b_state_q == SEND_B)) begin
                    mst_b_ready_o = 1'b0;
                    slv_b_valid_o = 1'b1;
                    slv_b_id_o    = id_q;
                    slv_b_resp_o  = axi_pkg::RESP_SLVERR;
                    slv_b_user_o  = '0;
                    if (slv_b_ready_i) begin
                        b_state_d = FEEDTHROUGH_B;
                    end else begin
                        b_state_d = SEND_B;
                    end
                end
            end  
            WAIT_COMPLETE_B: begin
                if (mst_b_valid_i && (mst_b_id_i == id_q)) begin
                    b_state_d = FEEDTHROUGH_B;
                end
            end  
            default: b_state_d = FEEDTHROUGH_B;
        endcase
    end  
    always_comb begin
        w_cnt_d = w_cnt_q;
        if (mst_aw_valid_o && mst_aw_ready_i) begin
            w_cnt_d += 1;
        end
        if (mst_w_valid_o && mst_w_ready_i && mst_w_last_o) begin
            w_cnt_d -= 1;
        end
    end
    always_ff @(posedge clk_i or negedge rst_ni) begin : axi_write_channel_ff
        if(~rst_ni) begin
            aw_state_q  <= FEEDTHROUGH_AW;
            w_state_q   <= FEEDTHROUGH_W;
            b_state_q   <= FEEDTHROUGH_B;
            w_cnt_q     <= '0;
            w_cnt_req_q <= '0;
            w_cnt_inj_q <= '0;
            addr_q      <= '0;
            id_q        <= '0;
            size_q      <= '0;
            strb_q      <= '0;
            cache_q     <= '0;
            prot_q      <= '0;
            qos_q       <= '0;
            region_q    <= '0;
            aw_user_q   <= '0;
            w_user_q    <= '0;
            w_data_q    <= '0;
            result_q    <= '0;
            w_d_valid_q <= '0;
            atop_q      <= 6'b0;
        end else begin
            aw_state_q  <= aw_state_d;
            w_state_q   <= w_state_d;
            b_state_q   <= b_state_d;
            w_cnt_q     <= w_cnt_d;
            w_cnt_req_q <= w_cnt_req_d;
            w_cnt_inj_q <= w_cnt_inj_d;
            addr_q      <= addr_d;
            id_q        <= id_d;
            size_q      <= size_d;
            strb_q      <= strb_d;
            cache_q     <= cache_d;
            prot_q      <= prot_d;
            qos_q       <= qos_d;
            region_q    <= region_d;
            aw_user_q   <= aw_user_d;
            w_user_q    <= w_user_d;
            w_data_q    <= w_data_d;
            result_q    <= result_d;
            w_d_valid_q <= w_d_valid_d;
            atop_q      <= atop_d;
        end
    end
    always_comb begin : axi_ar_channel
        mst_ar_id_o     = slv_ar_id_i;
        mst_ar_addr_o   = slv_ar_addr_i;
        mst_ar_len_o    = slv_ar_len_i;
        mst_ar_size_o   = slv_ar_size_i;
        mst_ar_burst_o  = slv_ar_burst_i;
        mst_ar_lock_o   = slv_ar_lock_i;
        mst_ar_cache_o  = slv_ar_cache_i;
        mst_ar_prot_o   = slv_ar_prot_i;
        mst_ar_qos_o    = slv_ar_qos_i;
        mst_ar_region_o = slv_ar_region_i;
        mst_ar_user_o   = slv_ar_user_i;
        mst_ar_valid_o  = 1'b0;
        slv_ar_ready_o  = 1'b0;
        ar_state_d      = ar_state_q;
        unique case (ar_state_q)
            FEEDTHROUGH_AR: begin
                mst_ar_valid_o = slv_ar_valid_i;
                slv_ar_ready_o = mst_ar_ready_i;
                if (adapter_ready) begin
                    if (atop_valid_d == LOAD | atop_valid_d == STORE) begin
                        if (ar_free) begin
                            slv_ar_ready_o  = 1'b0;
                            mst_ar_valid_o  = 1'b1;
                            mst_ar_addr_o   = slv_aw_addr_i;
                            mst_ar_id_o     = slv_aw_id_i;
                            mst_ar_len_o    = 8'h00;
                            mst_ar_size_o   = slv_aw_size_i;
                            mst_ar_burst_o  = 2'b00;
                            mst_ar_lock_o   = 1'h0;
                            mst_ar_cache_o  = slv_aw_cache_i;
                            mst_ar_prot_o   = slv_aw_prot_i;
                            mst_ar_qos_o    = slv_aw_qos_i;
                            mst_ar_region_o = slv_aw_region_i;
                            mst_ar_user_o   = slv_aw_user_i;
                            if (!mst_ar_ready_i) begin
                                ar_state_d = SEND_AR;
                            end
                        end else begin
                            ar_state_d   = WAIT_CHANNEL_AR;
                        end
                    end
                end
            end  
            WAIT_CHANNEL_AR, SEND_AR: begin
                if (ar_free || (ar_state_q == SEND_AR)) begin
                    mst_ar_valid_o  = 1'b1;
                    mst_ar_addr_o   = addr_q;
                    mst_ar_id_o     = id_q;
                    mst_ar_len_o    = 8'h00;
                    mst_ar_size_o   = size_q;
                    mst_ar_burst_o  = 2'b00;
                    mst_ar_lock_o   = 1'h0;
                    mst_ar_cache_o  = cache_q;
                    mst_ar_prot_o   = prot_q;
                    mst_ar_qos_o    = qos_q;
                    mst_ar_region_o = region_q;
                    mst_ar_user_o   = aw_user_q;
                    if (mst_ar_ready_i) begin
                        ar_state_d = FEEDTHROUGH_AR;
                    end else begin
                        ar_state_d = SEND_AR;
                    end
                end else begin
                    mst_ar_valid_o = slv_ar_valid_i;
                    slv_ar_ready_o = mst_ar_ready_i;
                end
            end  
            default: ar_state_d = FEEDTHROUGH_AR;
        endcase
    end  
    always_comb begin : axi_r_channel
        mst_r_ready_o = slv_r_ready_i;
        slv_r_id_o    = mst_r_id_i;
        slv_r_data_o  = mst_r_data_i;
        slv_r_resp_o  = mst_r_resp_i;
        slv_r_last_o  = mst_r_last_i;
        slv_r_user_o  = mst_r_user_i;
        slv_r_valid_o = mst_r_valid_i;
        r_data_d      = r_data_q;
        r_resp_d      = r_resp_q;
        r_user_d      = r_user_q;
        r_d_valid_d   = r_d_valid_q;
        r_state_d     = r_state_q;
        unique case (r_state_q)
            FEEDTHROUGH_R: begin
                if (adapter_ready) begin
                    r_d_valid_d = 1'b0;
                    if (atop_valid_d == LOAD || atop_valid_d == STORE) begin
                        r_state_d = WAIT_DATA_R;
                    end else if (atop_valid_d == INVALID) begin
                        if (r_free) begin
                            mst_r_ready_o = 1'b0;
                            slv_r_valid_o = 1'b1;
                            slv_r_data_o  = '0;
                            slv_r_id_o    = slv_aw_id_i;
                            slv_r_last_o  = 1'b1;
                            slv_r_resp_o  = axi_pkg::RESP_SLVERR;
                            slv_r_user_o  = '0;
                            if (!slv_r_ready_i) begin
                                r_state_d = SEND_R;
                            end
                        end else begin
                            r_state_d = WAIT_CHANNEL_R;
                        end
                    end
                end
            end  
            WAIT_DATA_R: begin
                if (mst_r_valid_i && (mst_r_id_i == id_q)) begin
                    mst_r_ready_o = 1'b1;
                    slv_r_valid_o = 1'b0;
                    r_data_d    = mst_r_data_i;
                    r_resp_d    = mst_r_resp_i;
                    r_user_d    = mst_r_user_i;
                    r_d_valid_d = 1'b1;
                    if (atop_valid_q == STORE) begin
                        r_state_d = FEEDTHROUGH_R;
                    end else begin
                        r_state_d = WAIT_CHANNEL_R;
                    end
                end
            end  
            WAIT_CHANNEL_R, SEND_R: begin
                if ((r_free && (b_state_q != WAIT_COMPLETE_B)) || (r_state_q == SEND_R)) begin
                    mst_r_ready_o = 1'b0;
                    slv_r_valid_o = 1'b1;
                    slv_r_data_o  = r_data_q;
                    slv_r_id_o    = id_q;
                    slv_r_last_o  = 1'b1;
                    slv_r_resp_o  = r_resp_q;
                    slv_r_user_o  = r_user_q;
                    if (atop_valid_q == INVALID) begin
                        slv_r_data_o = '0;
                        slv_r_resp_o = axi_pkg::RESP_SLVERR;
                        slv_r_user_o = '0;
                    end
                    if (slv_r_ready_i) begin
                        r_state_d = FEEDTHROUGH_R;
                    end else begin
                        r_state_d = SEND_R;
                    end
                end
            end  
            default: r_state_d = FEEDTHROUGH_R;
        endcase
    end  
    always_ff @(posedge clk_i or negedge rst_ni) begin : axi_read_channel_ff
        if(~rst_ni) begin
            ar_state_q  <= FEEDTHROUGH_AR;
            r_state_q   <= FEEDTHROUGH_R;
            r_data_q    <= '0;
            r_resp_q    <= '0;
            r_user_q    <= '0;
            r_d_valid_q <= 1'b0;
        end else begin
            ar_state_q  <= ar_state_d;
            r_state_q   <= r_state_d;
            r_data_q    <= r_data_d;
            r_resp_q    <= r_resp_d;
            r_user_q    <= r_user_d;
            r_d_valid_q <= r_d_valid_d;
        end
    end
    assign op_a           = r_data_q & strb_ext;
    assign op_b           = w_data_q & strb_ext;
    assign sign_a         = |(op_a & ~(strb_ext >> 1));
    assign sign_b         = |(op_b & ~(strb_ext >> 1));
    assign alu_result_ext = res;
    generate
        if (AXI_ALU_RATIO == 1 && RISCV_WORD_WIDTH == 32) begin
            assign alu_operand_a  = op_a;
            assign alu_operand_b  = op_b;
            assign res            = alu_result;
        end else if (AXI_ALU_RATIO == 1 && RISCV_WORD_WIDTH == 64) begin
            assign res        = alu_result;
            always_comb begin
                op_a_sign_ext = op_a | ({AXI_ALU_RATIO*RISCV_WORD_WIDTH{sign_a}} & ~strb_ext);
                op_b_sign_ext = op_b | ({AXI_ALU_RATIO*RISCV_WORD_WIDTH{sign_b}} & ~strb_ext);
                if (atop_q[2:0] == axi_pkg::ATOP_SMAX || atop_q[2:0] == axi_pkg::ATOP_SMIN) begin
                    alu_operand_a = op_a_sign_ext;
                    alu_operand_b = op_b_sign_ext;
                end else begin
                    alu_operand_a = op_a;
                    alu_operand_b = op_b;
                end
            end
        end else begin
            always_comb begin
                op_a_sign_ext = op_a | ({AXI_ALU_RATIO*RISCV_WORD_WIDTH{sign_a}} & ~strb_ext);
                op_b_sign_ext = op_b | ({AXI_ALU_RATIO*RISCV_WORD_WIDTH{sign_b}} & ~strb_ext);
                if (atop_q[2:0] == axi_pkg::ATOP_SMAX || atop_q[2:0] == axi_pkg::ATOP_SMIN) begin
                    alu_operand_a = op_a_sign_ext[addr_q[$clog2(AXI_DATA_WIDTH/8)-1:$clog2(RISCV_WORD_WIDTH/8)]];
                    alu_operand_b = op_b_sign_ext[addr_q[$clog2(AXI_DATA_WIDTH/8)-1:$clog2(RISCV_WORD_WIDTH/8)]];
                end else begin
                    alu_operand_a = op_a[addr_q[$clog2(AXI_DATA_WIDTH/8)-1:$clog2(RISCV_WORD_WIDTH/8)]];
                    alu_operand_b = op_b[addr_q[$clog2(AXI_DATA_WIDTH/8)-1:$clog2(RISCV_WORD_WIDTH/8)]];
                end
                res = '0;
                res[addr_q[$clog2(AXI_DATA_WIDTH/8)-1:$clog2(RISCV_WORD_WIDTH/8)]] = alu_result;
            end
        end
    endgenerate
    generate
        for (genvar i = 0; i < AXI_STRB_WIDTH; i++) begin
            always_comb begin
                if (strb_q[i]) begin
                    strb_ext[i] = 8'hFF;
                end else begin
                    strb_ext[i] = 8'h00;
                end
            end
        end
    endgenerate
    axi_riscv_amos_alu #(
        .DATA_WIDTH ( RISCV_WORD_WIDTH )
    ) i_amo_alu (
        .amo_op_i           ( atop_q        ),
        .amo_operand_a_i    ( alu_operand_a ),
        .amo_operand_b_i    ( alu_operand_b ),
        .amo_result_o       ( alu_result    )
    );
endmodule
module axi_riscv_amos_alu # (
    parameter int unsigned DATA_WIDTH = 0
) (
    input  logic [5:0]              amo_op_i,
    input  logic [DATA_WIDTH-1:0]   amo_operand_a_i,
    input  logic [DATA_WIDTH-1:0]   amo_operand_b_i,
    output logic [DATA_WIDTH-1:0]   amo_result_o
);
    logic [DATA_WIDTH:0] adder_sum;
    logic [DATA_WIDTH:0] adder_operand_a, adder_operand_b;
    assign adder_sum = adder_operand_a + adder_operand_b;
    always_comb begin
        adder_operand_a = $signed(amo_operand_a_i);
        adder_operand_b = $signed(amo_operand_b_i);
        amo_result_o = amo_operand_a_i;
        if (amo_op_i == axi_pkg::ATOP_ATOMICSWAP) begin
            amo_result_o = amo_operand_b_i;
        end else if ((amo_op_i[5:4] == axi_pkg::ATOP_ATOMICLOAD) | (amo_op_i[5:4] == axi_pkg::ATOP_ATOMICSTORE)) begin
            unique case (amo_op_i[2:0])
                axi_pkg::ATOP_ADD: amo_result_o = adder_sum[DATA_WIDTH-1:0];
                axi_pkg::ATOP_CLR: amo_result_o = amo_operand_a_i & (~amo_operand_b_i);
                axi_pkg::ATOP_SET: amo_result_o = amo_operand_a_i | amo_operand_b_i;
                axi_pkg::ATOP_EOR: amo_result_o = amo_operand_a_i ^ amo_operand_b_i;
                axi_pkg::ATOP_SMAX: begin
                    adder_operand_b = -$signed(amo_operand_b_i);
                    amo_result_o = adder_sum[DATA_WIDTH] ? amo_operand_b_i : amo_operand_a_i;
                end
                axi_pkg::ATOP_SMIN: begin
                    adder_operand_b = -$signed(amo_operand_b_i);
                    amo_result_o = adder_sum[DATA_WIDTH] ? amo_operand_a_i : amo_operand_b_i;
                end
                axi_pkg::ATOP_UMAX: begin
                    adder_operand_a = $unsigned(amo_operand_a_i);
                    adder_operand_b = -$unsigned(amo_operand_b_i);
                    amo_result_o = adder_sum[DATA_WIDTH] ? amo_operand_b_i : amo_operand_a_i;
                end
                axi_pkg::ATOP_UMIN: begin
                    adder_operand_a = $unsigned(amo_operand_a_i);
                    adder_operand_b = -$unsigned(amo_operand_b_i);
                    amo_result_o = adder_sum[DATA_WIDTH] ? amo_operand_a_i : amo_operand_b_i;
                end
                default: amo_result_o = '0;
            endcase
        end
    end
endmodule
module axi_riscv_atomics #(
    parameter int unsigned AXI_ADDR_WIDTH = 0,
    parameter int unsigned AXI_DATA_WIDTH = 0,
    parameter int unsigned AXI_ID_WIDTH = 0,
    parameter int unsigned AXI_USER_WIDTH = 0,
    parameter int unsigned AXI_MAX_WRITE_TXNS = 0,
    parameter int unsigned RISCV_WORD_WIDTH = 0,
    localparam int unsigned AXI_STRB_WIDTH = AXI_DATA_WIDTH / 8
) (
    input logic                         clk_i,
    input logic                         rst_ni,
    input  logic [AXI_ADDR_WIDTH-1:0]   slv_aw_addr_i,
    input  logic [2:0]                  slv_aw_prot_i,
    input  logic [3:0]                  slv_aw_region_i,
    input  logic [5:0]                  slv_aw_atop_i,
    input  logic [7:0]                  slv_aw_len_i,
    input  logic [2:0]                  slv_aw_size_i,
    input  logic [1:0]                  slv_aw_burst_i,
    input  logic                        slv_aw_lock_i,
    input  logic [3:0]                  slv_aw_cache_i,
    input  logic [3:0]                  slv_aw_qos_i,
    input  logic [AXI_ID_WIDTH-1:0]     slv_aw_id_i,
    input  logic [AXI_USER_WIDTH-1:0]   slv_aw_user_i,
    output logic                        slv_aw_ready_o,
    input  logic                        slv_aw_valid_i,
    input  logic [AXI_ADDR_WIDTH-1:0]   slv_ar_addr_i,
    input  logic [2:0]                  slv_ar_prot_i,
    input  logic [3:0]                  slv_ar_region_i,
    input  logic [7:0]                  slv_ar_len_i,
    input  logic [2:0]                  slv_ar_size_i,
    input  logic [1:0]                  slv_ar_burst_i,
    input  logic                        slv_ar_lock_i,
    input  logic [3:0]                  slv_ar_cache_i,
    input  logic [3:0]                  slv_ar_qos_i,
    input  logic [AXI_ID_WIDTH-1:0]     slv_ar_id_i,
    input  logic [AXI_USER_WIDTH-1:0]   slv_ar_user_i,
    output logic                        slv_ar_ready_o,
    input  logic                        slv_ar_valid_i,
    input  logic [AXI_DATA_WIDTH-1:0]   slv_w_data_i,
    input  logic [AXI_STRB_WIDTH-1:0]   slv_w_strb_i,
    input  logic [AXI_USER_WIDTH-1:0]   slv_w_user_i,
    input  logic                        slv_w_last_i,
    output logic                        slv_w_ready_o,
    input  logic                        slv_w_valid_i,
    output logic [AXI_DATA_WIDTH-1:0]   slv_r_data_o,
    output logic [1:0]                  slv_r_resp_o,
    output logic                        slv_r_last_o,
    output logic [AXI_ID_WIDTH-1:0]     slv_r_id_o,
    output logic [AXI_USER_WIDTH-1:0]   slv_r_user_o,
    input  logic                        slv_r_ready_i,
    output logic                        slv_r_valid_o,
    output logic [1:0]                  slv_b_resp_o,
    output logic [AXI_ID_WIDTH-1:0]     slv_b_id_o,
    output logic [AXI_USER_WIDTH-1:0]   slv_b_user_o,
    input  logic                        slv_b_ready_i,
    output logic                        slv_b_valid_o,
    output logic [AXI_ADDR_WIDTH-1:0]   mst_aw_addr_o,
    output logic [2:0]                  mst_aw_prot_o,
    output logic [3:0]                  mst_aw_region_o,
    output logic [5:0]                  mst_aw_atop_o,
    output logic [7:0]                  mst_aw_len_o,
    output logic [2:0]                  mst_aw_size_o,
    output logic [1:0]                  mst_aw_burst_o,
    output logic                        mst_aw_lock_o,
    output logic [3:0]                  mst_aw_cache_o,
    output logic [3:0]                  mst_aw_qos_o,
    output logic [AXI_ID_WIDTH-1:0]     mst_aw_id_o,
    output logic [AXI_USER_WIDTH-1:0]   mst_aw_user_o,
    input  logic                        mst_aw_ready_i,
    output logic                        mst_aw_valid_o,
    output logic [AXI_ADDR_WIDTH-1:0]   mst_ar_addr_o,
    output logic [2:0]                  mst_ar_prot_o,
    output logic [3:0]                  mst_ar_region_o,
    output logic [7:0]                  mst_ar_len_o,
    output logic [2:0]                  mst_ar_size_o,
    output logic [1:0]                  mst_ar_burst_o,
    output logic                        mst_ar_lock_o,
    output logic [3:0]                  mst_ar_cache_o,
    output logic [3:0]                  mst_ar_qos_o,
    output logic [AXI_ID_WIDTH-1:0]     mst_ar_id_o,
    output logic [AXI_USER_WIDTH-1:0]   mst_ar_user_o,
    input  logic                        mst_ar_ready_i,
    output logic                        mst_ar_valid_o,
    output logic [AXI_DATA_WIDTH-1:0]   mst_w_data_o,
    output logic [AXI_STRB_WIDTH-1:0]   mst_w_strb_o,
    output logic [AXI_USER_WIDTH-1:0]   mst_w_user_o,
    output logic                        mst_w_last_o,
    input  logic                        mst_w_ready_i,
    output logic                        mst_w_valid_o,
    input  logic [AXI_DATA_WIDTH-1:0]   mst_r_data_i,
    input  logic [1:0]                  mst_r_resp_i,
    input  logic                        mst_r_last_i,
    input  logic [AXI_ID_WIDTH-1:0]     mst_r_id_i,
    input  logic [AXI_USER_WIDTH-1:0]   mst_r_user_i,
    output logic                        mst_r_ready_o,
    input  logic                        mst_r_valid_i,
    input  logic [1:0]                  mst_b_resp_i,
    input  logic [AXI_ID_WIDTH-1:0]     mst_b_id_i,
    input  logic [AXI_USER_WIDTH-1:0]   mst_b_user_i,
    output logic                        mst_b_ready_o,
    input  logic                        mst_b_valid_i
);
    localparam longint unsigned ADDR_BEGIN  = '0;
    localparam longint unsigned ADDR_END    = {AXI_ADDR_WIDTH{1'b1}};
    logic [AXI_ADDR_WIDTH-1:0]   int_axi_aw_addr;
    logic [2:0]                  int_axi_aw_prot;
    logic [3:0]                  int_axi_aw_region;
    logic [5:0]                  int_axi_aw_atop;
    logic [7:0]                  int_axi_aw_len;
    logic [2:0]                  int_axi_aw_size;
    logic [1:0]                  int_axi_aw_burst;
    logic                        int_axi_aw_lock;
    logic [3:0]                  int_axi_aw_cache;
    logic [3:0]                  int_axi_aw_qos;
    logic [AXI_ID_WIDTH-1:0]     int_axi_aw_id;
    logic [AXI_USER_WIDTH-1:0]   int_axi_aw_user;
    logic                        int_axi_aw_ready;
    logic                        int_axi_aw_valid;
    logic [AXI_ADDR_WIDTH-1:0]   int_axi_ar_addr;
    logic [2:0]                  int_axi_ar_prot;
    logic [3:0]                  int_axi_ar_region;
    logic [7:0]                  int_axi_ar_len;
    logic [2:0]                  int_axi_ar_size;
    logic [1:0]                  int_axi_ar_burst;
    logic                        int_axi_ar_lock;
    logic [3:0]                  int_axi_ar_cache;
    logic [3:0]                  int_axi_ar_qos;
    logic [AXI_ID_WIDTH-1:0]     int_axi_ar_id;
    logic [AXI_USER_WIDTH-1:0]   int_axi_ar_user;
    logic                        int_axi_ar_ready;
    logic                        int_axi_ar_valid;
    logic [AXI_DATA_WIDTH-1:0]   int_axi_w_data;
    logic [AXI_STRB_WIDTH-1:0]   int_axi_w_strb;
    logic [AXI_USER_WIDTH-1:0]   int_axi_w_user;
    logic                        int_axi_w_last;
    logic                        int_axi_w_ready;
    logic                        int_axi_w_valid;
    logic [AXI_DATA_WIDTH-1:0]   int_axi_r_data;
    logic [1:0]                  int_axi_r_resp;
    logic                        int_axi_r_last;
    logic [AXI_ID_WIDTH-1:0]     int_axi_r_id;
    logic [AXI_USER_WIDTH-1:0]   int_axi_r_user;
    logic                        int_axi_r_ready;
    logic                        int_axi_r_valid;
    logic [1:0]                  int_axi_b_resp;
    logic [AXI_ID_WIDTH-1:0]     int_axi_b_id;
    logic [AXI_USER_WIDTH-1:0]   int_axi_b_user;
    logic                        int_axi_b_ready;
    logic                        int_axi_b_valid;
    axi_riscv_amos #(
        .AXI_ADDR_WIDTH     (AXI_ADDR_WIDTH),
        .AXI_DATA_WIDTH     (AXI_DATA_WIDTH),
        .AXI_ID_WIDTH       (AXI_ID_WIDTH),
        .AXI_USER_WIDTH     (AXI_USER_WIDTH),
        .AXI_MAX_WRITE_TXNS (AXI_MAX_WRITE_TXNS),
        .RISCV_WORD_WIDTH   (RISCV_WORD_WIDTH)
    ) i_amos (
        .clk_i              ( clk_i             ),
        .rst_ni             ( rst_ni            ),
        .slv_aw_addr_i      ( slv_aw_addr_i     ),
        .slv_aw_prot_i      ( slv_aw_prot_i     ),
        .slv_aw_region_i    ( slv_aw_region_i   ),
        .slv_aw_atop_i      ( slv_aw_atop_i     ),
        .slv_aw_len_i       ( slv_aw_len_i      ),
        .slv_aw_size_i      ( slv_aw_size_i     ),
        .slv_aw_burst_i     ( slv_aw_burst_i    ),
        .slv_aw_lock_i      ( slv_aw_lock_i     ),
        .slv_aw_cache_i     ( slv_aw_cache_i    ),
        .slv_aw_qos_i       ( slv_aw_qos_i      ),
        .slv_aw_id_i        ( slv_aw_id_i       ),
        .slv_aw_user_i      ( slv_aw_user_i     ),
        .slv_aw_ready_o     ( slv_aw_ready_o    ),
        .slv_aw_valid_i     ( slv_aw_valid_i    ),
        .slv_ar_addr_i      ( slv_ar_addr_i     ),
        .slv_ar_prot_i      ( slv_ar_prot_i     ),
        .slv_ar_region_i    ( slv_ar_region_i   ),
        .slv_ar_len_i       ( slv_ar_len_i      ),
        .slv_ar_size_i      ( slv_ar_size_i     ),
        .slv_ar_burst_i     ( slv_ar_burst_i    ),
        .slv_ar_lock_i      ( slv_ar_lock_i     ),
        .slv_ar_cache_i     ( slv_ar_cache_i    ),
        .slv_ar_qos_i       ( slv_ar_qos_i      ),
        .slv_ar_id_i        ( slv_ar_id_i       ),
        .slv_ar_user_i      ( slv_ar_user_i     ),
        .slv_ar_ready_o     ( slv_ar_ready_o    ),
        .slv_ar_valid_i     ( slv_ar_valid_i    ),
        .slv_w_data_i       ( slv_w_data_i      ),
        .slv_w_strb_i       ( slv_w_strb_i      ),
        .slv_w_user_i       ( slv_w_user_i      ),
        .slv_w_last_i       ( slv_w_last_i      ),
        .slv_w_ready_o      ( slv_w_ready_o     ),
        .slv_w_valid_i      ( slv_w_valid_i     ),
        .slv_r_data_o       ( slv_r_data_o      ),
        .slv_r_resp_o       ( slv_r_resp_o      ),
        .slv_r_last_o       ( slv_r_last_o      ),
        .slv_r_id_o         ( slv_r_id_o        ),
        .slv_r_user_o       ( slv_r_user_o      ),
        .slv_r_ready_i      ( slv_r_ready_i     ),
        .slv_r_valid_o      ( slv_r_valid_o     ),
        .slv_b_resp_o       ( slv_b_resp_o      ),
        .slv_b_id_o         ( slv_b_id_o        ),
        .slv_b_user_o       ( slv_b_user_o      ),
        .slv_b_ready_i      ( slv_b_ready_i     ),
        .slv_b_valid_o      ( slv_b_valid_o     ),
        .mst_aw_addr_o      ( int_axi_aw_addr   ),
        .mst_aw_prot_o      ( int_axi_aw_prot   ),
        .mst_aw_region_o    ( int_axi_aw_region ),
        .mst_aw_atop_o      ( int_axi_aw_atop   ),
        .mst_aw_len_o       ( int_axi_aw_len    ),
        .mst_aw_size_o      ( int_axi_aw_size   ),
        .mst_aw_burst_o     ( int_axi_aw_burst  ),
        .mst_aw_lock_o      ( int_axi_aw_lock   ),
        .mst_aw_cache_o     ( int_axi_aw_cache  ),
        .mst_aw_qos_o       ( int_axi_aw_qos    ),
        .mst_aw_id_o        ( int_axi_aw_id     ),
        .mst_aw_user_o      ( int_axi_aw_user   ),
        .mst_aw_ready_i     ( int_axi_aw_ready  ),
        .mst_aw_valid_o     ( int_axi_aw_valid  ),
        .mst_ar_addr_o      ( int_axi_ar_addr   ),
        .mst_ar_prot_o      ( int_axi_ar_prot   ),
        .mst_ar_region_o    ( int_axi_ar_region ),
        .mst_ar_len_o       ( int_axi_ar_len    ),
        .mst_ar_size_o      ( int_axi_ar_size   ),
        .mst_ar_burst_o     ( int_axi_ar_burst  ),
        .mst_ar_lock_o      ( int_axi_ar_lock   ),
        .mst_ar_cache_o     ( int_axi_ar_cache  ),
        .mst_ar_qos_o       ( int_axi_ar_qos    ),
        .mst_ar_id_o        ( int_axi_ar_id     ),
        .mst_ar_user_o      ( int_axi_ar_user   ),
        .mst_ar_ready_i     ( int_axi_ar_ready  ),
        .mst_ar_valid_o     ( int_axi_ar_valid  ),
        .mst_w_data_o       ( int_axi_w_data    ),
        .mst_w_strb_o       ( int_axi_w_strb    ),
        .mst_w_user_o       ( int_axi_w_user    ),
        .mst_w_last_o       ( int_axi_w_last    ),
        .mst_w_ready_i      ( int_axi_w_ready   ),
        .mst_w_valid_o      ( int_axi_w_valid   ),
        .mst_r_data_i       ( int_axi_r_data    ),
        .mst_r_resp_i       ( int_axi_r_resp    ),
        .mst_r_last_i       ( int_axi_r_last    ),
        .mst_r_id_i         ( int_axi_r_id      ),
        .mst_r_user_i       ( int_axi_r_user    ),
        .mst_r_ready_o      ( int_axi_r_ready   ),
        .mst_r_valid_i      ( int_axi_r_valid   ),
        .mst_b_resp_i       ( int_axi_b_resp    ),
        .mst_b_id_i         ( int_axi_b_id      ),
        .mst_b_user_i       ( int_axi_b_user    ),
        .mst_b_ready_o      ( int_axi_b_ready   ),
        .mst_b_valid_i      ( int_axi_b_valid   )
    );
    axi_riscv_lrsc #(
        .ADDR_BEGIN     (ADDR_BEGIN),
        .ADDR_END       (ADDR_END),
        .AXI_ADDR_WIDTH (AXI_ADDR_WIDTH),
        .AXI_DATA_WIDTH (AXI_DATA_WIDTH),
        .AXI_ID_WIDTH   (AXI_ID_WIDTH),
        .AXI_USER_WIDTH (AXI_USER_WIDTH)
    ) i_lrsc (
        .clk_i              ( clk_i             ),
        .rst_ni             ( rst_ni            ),
        .slv_aw_addr_i      ( int_axi_aw_addr   ),
        .slv_aw_prot_i      ( int_axi_aw_prot   ),
        .slv_aw_region_i    ( int_axi_aw_region ),
        .slv_aw_atop_i      ( int_axi_aw_atop   ),
        .slv_aw_len_i       ( int_axi_aw_len    ),
        .slv_aw_size_i      ( int_axi_aw_size   ),
        .slv_aw_burst_i     ( int_axi_aw_burst  ),
        .slv_aw_lock_i      ( int_axi_aw_lock   ),
        .slv_aw_cache_i     ( int_axi_aw_cache  ),
        .slv_aw_qos_i       ( int_axi_aw_qos    ),
        .slv_aw_id_i        ( int_axi_aw_id     ),
        .slv_aw_user_i      ( int_axi_aw_user   ),
        .slv_aw_ready_o     ( int_axi_aw_ready  ),
        .slv_aw_valid_i     ( int_axi_aw_valid  ),
        .slv_ar_addr_i      ( int_axi_ar_addr   ),
        .slv_ar_prot_i      ( int_axi_ar_prot   ),
        .slv_ar_region_i    ( int_axi_ar_region ),
        .slv_ar_len_i       ( int_axi_ar_len    ),
        .slv_ar_size_i      ( int_axi_ar_size   ),
        .slv_ar_burst_i     ( int_axi_ar_burst  ),
        .slv_ar_lock_i      ( int_axi_ar_lock   ),
        .slv_ar_cache_i     ( int_axi_ar_cache  ),
        .slv_ar_qos_i       ( int_axi_ar_qos    ),
        .slv_ar_id_i        ( int_axi_ar_id     ),
        .slv_ar_user_i      ( int_axi_ar_user   ),
        .slv_ar_ready_o     ( int_axi_ar_ready  ),
        .slv_ar_valid_i     ( int_axi_ar_valid  ),
        .slv_w_data_i       ( int_axi_w_data    ),
        .slv_w_strb_i       ( int_axi_w_strb    ),
        .slv_w_user_i       ( int_axi_w_user    ),
        .slv_w_last_i       ( int_axi_w_last    ),
        .slv_w_ready_o      ( int_axi_w_ready   ),
        .slv_w_valid_i      ( int_axi_w_valid   ),
        .slv_r_data_o       ( int_axi_r_data    ),
        .slv_r_resp_o       ( int_axi_r_resp    ),
        .slv_r_last_o       ( int_axi_r_last    ),
        .slv_r_id_o         ( int_axi_r_id      ),
        .slv_r_user_o       ( int_axi_r_user    ),
        .slv_r_ready_i      ( int_axi_r_ready   ),
        .slv_r_valid_o      ( int_axi_r_valid   ),
        .slv_b_resp_o       ( int_axi_b_resp    ),
        .slv_b_id_o         ( int_axi_b_id      ),
        .slv_b_user_o       ( int_axi_b_user    ),
        .slv_b_ready_i      ( int_axi_b_ready   ),
        .slv_b_valid_o      ( int_axi_b_valid   ),
        .mst_aw_addr_o      ( mst_aw_addr_o     ),
        .mst_aw_prot_o      ( mst_aw_prot_o     ),
        .mst_aw_region_o    ( mst_aw_region_o   ),
        .mst_aw_atop_o      ( mst_aw_atop_o     ),
        .mst_aw_len_o       ( mst_aw_len_o      ),
        .mst_aw_size_o      ( mst_aw_size_o     ),
        .mst_aw_burst_o     ( mst_aw_burst_o    ),
        .mst_aw_lock_o      ( mst_aw_lock_o     ),
        .mst_aw_cache_o     ( mst_aw_cache_o    ),
        .mst_aw_qos_o       ( mst_aw_qos_o      ),
        .mst_aw_id_o        ( mst_aw_id_o       ),
        .mst_aw_user_o      ( mst_aw_user_o     ),
        .mst_aw_ready_i     ( mst_aw_ready_i    ),
        .mst_aw_valid_o     ( mst_aw_valid_o    ),
        .mst_ar_addr_o      ( mst_ar_addr_o     ),
        .mst_ar_prot_o      ( mst_ar_prot_o     ),
        .mst_ar_region_o    ( mst_ar_region_o   ),
        .mst_ar_len_o       ( mst_ar_len_o      ),
        .mst_ar_size_o      ( mst_ar_size_o     ),
        .mst_ar_burst_o     ( mst_ar_burst_o    ),
        .mst_ar_lock_o      ( mst_ar_lock_o     ),
        .mst_ar_cache_o     ( mst_ar_cache_o    ),
        .mst_ar_qos_o       ( mst_ar_qos_o      ),
        .mst_ar_id_o        ( mst_ar_id_o       ),
        .mst_ar_user_o      ( mst_ar_user_o     ),
        .mst_ar_ready_i     ( mst_ar_ready_i    ),
        .mst_ar_valid_o     ( mst_ar_valid_o    ),
        .mst_w_data_o       ( mst_w_data_o      ),
        .mst_w_strb_o       ( mst_w_strb_o      ),
        .mst_w_user_o       ( mst_w_user_o      ),
        .mst_w_last_o       ( mst_w_last_o      ),
        .mst_w_ready_i      ( mst_w_ready_i     ),
        .mst_w_valid_o      ( mst_w_valid_o     ),
        .mst_r_data_i       ( mst_r_data_i      ),
        .mst_r_resp_i       ( mst_r_resp_i      ),
        .mst_r_last_i       ( mst_r_last_i      ),
        .mst_r_id_i         ( mst_r_id_i        ),
        .mst_r_user_i       ( mst_r_user_i      ),
        .mst_r_ready_o      ( mst_r_ready_o     ),
        .mst_r_valid_i      ( mst_r_valid_i     ),
        .mst_b_resp_i       ( mst_b_resp_i      ),
        .mst_b_id_i         ( mst_b_id_i        ),
        .mst_b_user_i       ( mst_b_user_i      ),
        .mst_b_ready_o      ( mst_b_ready_o     ),
        .mst_b_valid_i      ( mst_b_valid_i     )
    );
endmodule
module axi_riscv_atomics_wrap #(
    parameter int unsigned AXI_ADDR_WIDTH = 0,
    parameter int unsigned AXI_DATA_WIDTH = 0,
    parameter int unsigned AXI_ID_WIDTH = 0,
    parameter int unsigned AXI_USER_WIDTH = 0,
    parameter int unsigned AXI_MAX_WRITE_TXNS = 0,
    parameter int unsigned RISCV_WORD_WIDTH = 0,
    localparam int unsigned AXI_STRB_WIDTH = AXI_DATA_WIDTH / 8
) (
    input  logic    clk_i,
    input  logic    rst_ni,
    AXI_BUS.Master  mst,
    AXI_BUS.Slave   slv
);
    axi_riscv_atomics #(
        .AXI_ADDR_WIDTH     (AXI_ADDR_WIDTH),
        .AXI_DATA_WIDTH     (AXI_DATA_WIDTH),
        .AXI_ID_WIDTH       (AXI_ID_WIDTH),
        .AXI_USER_WIDTH     (AXI_USER_WIDTH),
        .AXI_MAX_WRITE_TXNS (AXI_MAX_WRITE_TXNS),
        .RISCV_WORD_WIDTH   (RISCV_WORD_WIDTH)
    ) i_atomics (
        .clk_i           ( clk_i         ),
        .rst_ni          ( rst_ni        ),
        .slv_aw_addr_i   ( slv.aw_addr   ),
        .slv_aw_prot_i   ( slv.aw_prot   ),
        .slv_aw_region_i ( slv.aw_region ),
        .slv_aw_atop_i   ( slv.aw_atop   ),
        .slv_aw_len_i    ( slv.aw_len    ),
        .slv_aw_size_i   ( slv.aw_size   ),
        .slv_aw_burst_i  ( slv.aw_burst  ),
        .slv_aw_lock_i   ( slv.aw_lock   ),
        .slv_aw_cache_i  ( slv.aw_cache  ),
        .slv_aw_qos_i    ( slv.aw_qos    ),
        .slv_aw_id_i     ( slv.aw_id     ),
        .slv_aw_user_i   ( slv.aw_user   ),
        .slv_aw_ready_o  ( slv.aw_ready  ),
        .slv_aw_valid_i  ( slv.aw_valid  ),
        .slv_ar_addr_i   ( slv.ar_addr   ),
        .slv_ar_prot_i   ( slv.ar_prot   ),
        .slv_ar_region_i ( slv.ar_region ),
        .slv_ar_len_i    ( slv.ar_len    ),
        .slv_ar_size_i   ( slv.ar_size   ),
        .slv_ar_burst_i  ( slv.ar_burst  ),
        .slv_ar_lock_i   ( slv.ar_lock   ),
        .slv_ar_cache_i  ( slv.ar_cache  ),
        .slv_ar_qos_i    ( slv.ar_qos    ),
        .slv_ar_id_i     ( slv.ar_id     ),
        .slv_ar_user_i   ( slv.ar_user   ),
        .slv_ar_ready_o  ( slv.ar_ready  ),
        .slv_ar_valid_i  ( slv.ar_valid  ),
        .slv_w_data_i    ( slv.w_data    ),
        .slv_w_strb_i    ( slv.w_strb    ),
        .slv_w_user_i    ( slv.w_user    ),
        .slv_w_last_i    ( slv.w_last    ),
        .slv_w_ready_o   ( slv.w_ready   ),
        .slv_w_valid_i   ( slv.w_valid   ),
        .slv_r_data_o    ( slv.r_data    ),
        .slv_r_resp_o    ( slv.r_resp    ),
        .slv_r_last_o    ( slv.r_last    ),
        .slv_r_id_o      ( slv.r_id      ),
        .slv_r_user_o    ( slv.r_user    ),
        .slv_r_ready_i   ( slv.r_ready   ),
        .slv_r_valid_o   ( slv.r_valid   ),
        .slv_b_resp_o    ( slv.b_resp    ),
        .slv_b_id_o      ( slv.b_id      ),
        .slv_b_user_o    ( slv.b_user    ),
        .slv_b_ready_i   ( slv.b_ready   ),
        .slv_b_valid_o   ( slv.b_valid   ),
        .mst_aw_addr_o   ( mst.aw_addr   ),
        .mst_aw_prot_o   ( mst.aw_prot   ),
        .mst_aw_region_o ( mst.aw_region ),
        .mst_aw_atop_o   ( mst.aw_atop   ),
        .mst_aw_len_o    ( mst.aw_len    ),
        .mst_aw_size_o   ( mst.aw_size   ),
        .mst_aw_burst_o  ( mst.aw_burst  ),
        .mst_aw_lock_o   ( mst.aw_lock   ),
        .mst_aw_cache_o  ( mst.aw_cache  ),
        .mst_aw_qos_o    ( mst.aw_qos    ),
        .mst_aw_id_o     ( mst.aw_id     ),
        .mst_aw_user_o   ( mst.aw_user   ),
        .mst_aw_ready_i  ( mst.aw_ready  ),
        .mst_aw_valid_o  ( mst.aw_valid  ),
        .mst_ar_addr_o   ( mst.ar_addr   ),
        .mst_ar_prot_o   ( mst.ar_prot   ),
        .mst_ar_region_o ( mst.ar_region ),
        .mst_ar_len_o    ( mst.ar_len    ),
        .mst_ar_size_o   ( mst.ar_size   ),
        .mst_ar_burst_o  ( mst.ar_burst  ),
        .mst_ar_lock_o   ( mst.ar_lock   ),
        .mst_ar_cache_o  ( mst.ar_cache  ),
        .mst_ar_qos_o    ( mst.ar_qos    ),
        .mst_ar_id_o     ( mst.ar_id     ),
        .mst_ar_user_o   ( mst.ar_user   ),
        .mst_ar_ready_i  ( mst.ar_ready  ),
        .mst_ar_valid_o  ( mst.ar_valid  ),
        .mst_w_data_o    ( mst.w_data    ),
        .mst_w_strb_o    ( mst.w_strb    ),
        .mst_w_user_o    ( mst.w_user    ),
        .mst_w_last_o    ( mst.w_last    ),
        .mst_w_ready_i   ( mst.w_ready   ),
        .mst_w_valid_o   ( mst.w_valid   ),
        .mst_r_data_i    ( mst.r_data    ),
        .mst_r_resp_i    ( mst.r_resp    ),
        .mst_r_last_i    ( mst.r_last    ),
        .mst_r_id_i      ( mst.r_id      ),
        .mst_r_user_i    ( mst.r_user    ),
        .mst_r_ready_o   ( mst.r_ready   ),
        .mst_r_valid_i   ( mst.r_valid   ),
        .mst_b_resp_i    ( mst.b_resp    ),
        .mst_b_id_i      ( mst.b_id      ),
        .mst_b_user_i    ( mst.b_user    ),
        .mst_b_ready_o   ( mst.b_ready   ),
        .mst_b_valid_i   ( mst.b_valid   )
    );
endmodule
module axi_riscv_lrsc #(
    parameter longint unsigned ADDR_BEGIN = 0,
    parameter longint unsigned ADDR_END = 0,
    parameter int unsigned AXI_ADDR_WIDTH = 0,
    parameter int unsigned AXI_DATA_WIDTH = 0,
    parameter int unsigned AXI_ID_WIDTH = 0,
    parameter int unsigned AXI_USER_WIDTH = 0,
    localparam int unsigned AXI_STRB_WIDTH = AXI_DATA_WIDTH / 8
) (
    input logic                         clk_i,
    input logic                         rst_ni,
    input  logic [AXI_ADDR_WIDTH-1:0]   slv_aw_addr_i,
    input  logic [2:0]                  slv_aw_prot_i,
    input  logic [3:0]                  slv_aw_region_i,
    input  logic [5:0]                  slv_aw_atop_i,
    input  logic [7:0]                  slv_aw_len_i,
    input  logic [2:0]                  slv_aw_size_i,
    input  logic [1:0]                  slv_aw_burst_i,
    input  logic                        slv_aw_lock_i,
    input  logic [3:0]                  slv_aw_cache_i,
    input  logic [3:0]                  slv_aw_qos_i,
    input  logic [AXI_ID_WIDTH-1:0]     slv_aw_id_i,
    input  logic [AXI_USER_WIDTH-1:0]   slv_aw_user_i,
    output logic                        slv_aw_ready_o,
    input  logic                        slv_aw_valid_i,
    input  logic [AXI_ADDR_WIDTH-1:0]   slv_ar_addr_i,
    input  logic [2:0]                  slv_ar_prot_i,
    input  logic [3:0]                  slv_ar_region_i,
    input  logic [7:0]                  slv_ar_len_i,
    input  logic [2:0]                  slv_ar_size_i,
    input  logic [1:0]                  slv_ar_burst_i,
    input  logic                        slv_ar_lock_i,
    input  logic [3:0]                  slv_ar_cache_i,
    input  logic [3:0]                  slv_ar_qos_i,
    input  logic [AXI_ID_WIDTH-1:0]     slv_ar_id_i,
    input  logic [AXI_USER_WIDTH-1:0]   slv_ar_user_i,
    output logic                        slv_ar_ready_o,
    input  logic                        slv_ar_valid_i,
    input  logic [AXI_DATA_WIDTH-1:0]   slv_w_data_i,
    input  logic [AXI_STRB_WIDTH-1:0]   slv_w_strb_i,
    input  logic [AXI_USER_WIDTH-1:0]   slv_w_user_i,
    input  logic                        slv_w_last_i,
    output logic                        slv_w_ready_o,
    input  logic                        slv_w_valid_i,
    output logic [AXI_DATA_WIDTH-1:0]   slv_r_data_o,
    output logic [1:0]                  slv_r_resp_o,
    output logic                        slv_r_last_o,
    output logic [AXI_ID_WIDTH-1:0]     slv_r_id_o,
    output logic [AXI_USER_WIDTH-1:0]   slv_r_user_o,
    input  logic                        slv_r_ready_i,
    output logic                        slv_r_valid_o,
    output logic [1:0]                  slv_b_resp_o,
    output logic [AXI_ID_WIDTH-1:0]     slv_b_id_o,
    output logic [AXI_USER_WIDTH-1:0]   slv_b_user_o,
    input  logic                        slv_b_ready_i,
    output logic                        slv_b_valid_o,
    output logic [AXI_ADDR_WIDTH-1:0]   mst_aw_addr_o,
    output logic [2:0]                  mst_aw_prot_o,
    output logic [3:0]                  mst_aw_region_o,
    output logic [5:0]                  mst_aw_atop_o,
    output logic [7:0]                  mst_aw_len_o,
    output logic [2:0]                  mst_aw_size_o,
    output logic [1:0]                  mst_aw_burst_o,
    output logic                        mst_aw_lock_o,
    output logic [3:0]                  mst_aw_cache_o,
    output logic [3:0]                  mst_aw_qos_o,
    output logic [AXI_ID_WIDTH-1:0]     mst_aw_id_o,
    output logic [AXI_USER_WIDTH-1:0]   mst_aw_user_o,
    input  logic                        mst_aw_ready_i,
    output logic                        mst_aw_valid_o,
    output logic [AXI_ADDR_WIDTH-1:0]   mst_ar_addr_o,
    output logic [2:0]                  mst_ar_prot_o,
    output logic [3:0]                  mst_ar_region_o,
    output logic [7:0]                  mst_ar_len_o,
    output logic [2:0]                  mst_ar_size_o,
    output logic [1:0]                  mst_ar_burst_o,
    output logic                        mst_ar_lock_o,
    output logic [3:0]                  mst_ar_cache_o,
    output logic [3:0]                  mst_ar_qos_o,
    output logic [AXI_ID_WIDTH-1:0]     mst_ar_id_o,
    output logic [AXI_USER_WIDTH-1:0]   mst_ar_user_o,
    input  logic                        mst_ar_ready_i,
    output logic                        mst_ar_valid_o,
    output logic [AXI_DATA_WIDTH-1:0]   mst_w_data_o,
    output logic [AXI_STRB_WIDTH-1:0]   mst_w_strb_o,
    output logic [AXI_USER_WIDTH-1:0]   mst_w_user_o,
    output logic                        mst_w_last_o,
    input  logic                        mst_w_ready_i,
    output logic                        mst_w_valid_o,
    input  logic [AXI_DATA_WIDTH-1:0]   mst_r_data_i,
    input  logic [1:0]                  mst_r_resp_i,
    input  logic                        mst_r_last_i,
    input  logic [AXI_ID_WIDTH-1:0]     mst_r_id_i,
    input  logic [AXI_USER_WIDTH-1:0]   mst_r_user_i,
    output logic                        mst_r_ready_o,
    input  logic                        mst_r_valid_i,
    input  logic [1:0]                  mst_b_resp_i,
    input  logic [AXI_ID_WIDTH-1:0]     mst_b_id_i,
    input  logic [AXI_USER_WIDTH-1:0]   mst_b_user_i,
    output logic                        mst_b_ready_o,
    input  logic                        mst_b_valid_i
);
    logic [AXI_ID_WIDTH-1:0]        art_check_id,
                                    art_set_id,
                                    w_id_d,                     w_id_q;
    logic [AXI_ADDR_WIDTH-1:0]      art_check_addr,
                                    art_clr_addr,
                                    art_set_addr,
                                    rd_clr_addr,
                                    wr_clr_addr,
                                    w_addr_d,                   w_addr_q;
    logic                           art_check_req,              art_check_gnt,
                                    art_clr_req,                art_clr_gnt,
                                    art_set_req,                art_set_gnt,
                                    rd_clr_req,                 rd_clr_gnt,
                                    wr_clr_req,                 wr_clr_gnt;
    logic                           art_check_res;
    logic                           b_excl_d,                   b_excl_q,
                                    r_excl_d,                   r_excl_q;
    typedef enum logic [1:0]    {R_IDLE, R_WAIT_AR, R_WAIT_R} r_state_t;
    r_state_t                       r_state_d,                  r_state_q;
    typedef enum logic [2:0]    {AW_IDLE, W_FORWARD, W_BYPASS, W_WAIT_ART_CLR, W_DROP, B_FORWARD,
                                B_INJECT} w_state_t;
    w_state_t                       w_state_d,                  w_state_q;
    assign mst_ar_addr_o      = slv_ar_addr_i;
    assign mst_ar_prot_o      = slv_ar_prot_i;
    assign mst_ar_region_o    = slv_ar_region_i;
    assign mst_ar_len_o       = slv_ar_len_i;
    assign mst_ar_size_o      = slv_ar_size_i;
    assign mst_ar_burst_o     = slv_ar_burst_i;
    assign mst_ar_lock_o      = 1'b0;
    assign mst_ar_cache_o     = slv_ar_cache_i;
    assign mst_ar_qos_o       = slv_ar_qos_i;
    assign mst_ar_id_o        = slv_ar_id_i;
    assign mst_ar_user_o      = slv_ar_user_i;
    assign slv_r_data_o       = mst_r_data_i;
    assign slv_r_last_o       = mst_r_last_i;
    assign slv_r_id_o         = mst_r_id_i;
    assign slv_r_user_o       = mst_r_user_i;
    always_comb begin
        mst_ar_valid_o  = 1'b0;
        slv_ar_ready_o  = 1'b0;
        mst_r_ready_o   = 1'b0;
        slv_r_valid_o   = 1'b0;
        slv_r_resp_o    = '0;
        art_set_addr    = '0;
        art_set_id      = '0;
        art_set_req     = 1'b0;
        rd_clr_addr     = '0;
        rd_clr_req      = 1'b0;
        r_excl_d        = r_excl_q;
        r_state_d       = r_state_q;
        case (r_state_q)
            R_IDLE: begin
                if (slv_ar_valid_i) begin
                    if (slv_ar_addr_i >= ADDR_BEGIN && slv_ar_addr_i <= ADDR_END && slv_ar_lock_i &&
                            slv_ar_len_i == 8'h00) begin
                        art_set_addr    = slv_ar_addr_i;
                        art_set_id      = slv_ar_id_i;
                        art_set_req     = 1'b1;
                        r_excl_d        = 1'b1;
                        if (art_set_gnt) begin
                            mst_ar_valid_o = 1'b1;
                            if (mst_ar_ready_i) begin
                                slv_ar_ready_o = 1'b1;
                                r_state_d = R_WAIT_R;
                            end else begin
                                r_state_d = R_WAIT_AR;
                            end
                        end
                    end else begin
                        r_excl_d = 1'b0;
                        mst_ar_valid_o = 1'b1;
                        if (mst_ar_ready_i) begin
                            slv_ar_ready_o = 1'b1;
                            r_state_d = R_WAIT_R;
                        end else begin
                            r_state_d = R_WAIT_AR;
                        end
                    end
                end
            end
            R_WAIT_AR: begin
                mst_ar_valid_o = slv_ar_valid_i;
                slv_ar_ready_o = mst_ar_ready_i;
                if (mst_ar_ready_i && mst_ar_valid_o) begin
                    r_state_d = R_WAIT_R;
                end
            end
            R_WAIT_R: begin
                mst_r_ready_o = slv_r_ready_i;
                slv_r_valid_o = mst_r_valid_i;
                if (mst_r_resp_i[1] == 1'b0) begin
                    slv_r_resp_o = {1'b0, r_excl_q};
                end else begin
                    slv_r_resp_o = mst_r_resp_i;
                end
                if (mst_r_valid_i && mst_r_ready_o && mst_r_last_i) begin
                    r_excl_d    = 1'b0;
                    r_state_d   = R_IDLE;
                end
            end
            default: begin
                r_state_d = R_IDLE;
            end
        endcase
    end
    assign mst_aw_addr_o    = slv_aw_addr_i;
    assign mst_aw_prot_o    = slv_aw_prot_i;
    assign mst_aw_region_o  = slv_aw_region_i;
    assign mst_aw_atop_o    = slv_aw_atop_i;
    assign mst_aw_len_o     = slv_aw_len_i;
    assign mst_aw_size_o    = slv_aw_size_i;
    assign mst_aw_burst_o   = slv_aw_burst_i;
    assign mst_aw_lock_o    = 1'b0;
    assign mst_aw_cache_o   = slv_aw_cache_i;
    assign mst_aw_qos_o     = slv_aw_qos_i;
    assign mst_aw_id_o      = slv_aw_id_i;
    assign mst_aw_user_o    = slv_aw_user_i;
    assign mst_w_data_o     = slv_w_data_i;
    assign mst_w_strb_o     = slv_w_strb_i;
    assign mst_w_user_o     = slv_w_user_i;
    assign mst_w_last_o     = slv_w_last_i;
    always_comb begin
        w_addr_d    = w_addr_q;
        w_id_d      = w_id_q;
        if (slv_aw_valid_i && slv_aw_ready_o) begin
            w_addr_d    = slv_aw_addr_i;
            w_id_d      = slv_aw_id_i;
        end
    end
    always_comb begin
        mst_aw_valid_o  = 1'b0;
        slv_aw_ready_o  = 1'b0;
        mst_w_valid_o   = 1'b0;
        slv_w_ready_o   = 1'b0;
        slv_b_valid_o   = 1'b0;
        mst_b_ready_o   = 1'b0;
        slv_b_resp_o    = '0;
        slv_b_id_o      = '0;
        slv_b_user_o    = '0;
        art_check_addr  = '0;
        art_check_id    = '0;
        art_check_req   = 1'b0;
        wr_clr_addr     = '0;
        wr_clr_req      = 1'b0;
        b_excl_d        = b_excl_q;
        w_state_d       = w_state_q;
        case (w_state_q)
            AW_IDLE: begin
                if (slv_aw_valid_i) begin
                    if (slv_aw_addr_i >= ADDR_BEGIN && slv_aw_addr_i <= ADDR_END) begin
                        if (slv_aw_lock_i && slv_aw_len_i == 8'h00) begin
                            art_check_addr  = slv_aw_addr_i;
                            art_check_id    = slv_aw_id_i;
                            art_check_req   = 1'b1;
                            if (art_check_gnt) begin
                                if (art_check_res) begin
                                    mst_aw_valid_o = 1'b1;
                                    if (mst_aw_ready_i) begin
                                        slv_aw_ready_o    = 1'b1;
                                        b_excl_d        = 1'b1;
                                        w_state_d       = W_FORWARD;
                                    end
                                end else begin
                                    slv_aw_ready_o    = 1'b1;
                                    w_state_d       = W_DROP;
                                end
                            end
                        end else begin
                            mst_aw_valid_o = 1'b1;
                            if (mst_aw_ready_i) begin
                                slv_aw_ready_o    = 1'b1;
                                w_state_d       = W_FORWARD;
                            end
                        end
                    end else begin
                        mst_aw_valid_o = 1'b1;
                        slv_aw_ready_o = mst_aw_ready_i;
                        if (slv_aw_ready_o) begin
                            w_state_d = W_BYPASS;
                        end
                    end
                end
            end
            W_FORWARD: begin
                mst_w_valid_o = slv_w_valid_i;
                slv_w_ready_o = mst_w_ready_i;
                if (slv_w_valid_i && slv_w_ready_o && slv_w_last_i) begin
                    wr_clr_addr = w_addr_q;
                    wr_clr_req  = 1'b1;
                    if (wr_clr_gnt) begin
                        w_state_d = B_FORWARD;
                    end else begin
                        w_state_d = W_WAIT_ART_CLR;
                    end
                end
            end
            W_BYPASS: begin
                mst_w_valid_o = slv_w_valid_i;
                slv_w_ready_o = mst_w_ready_i;
                if (slv_w_valid_i && slv_w_ready_o && slv_w_last_i) begin
                    w_state_d = B_FORWARD;
                end
            end
            W_WAIT_ART_CLR: begin
                wr_clr_addr = w_addr_q;
                wr_clr_req  = 1'b1;
                if (wr_clr_gnt) begin
                    w_state_d = B_FORWARD;
                end
            end
            W_DROP: begin
                slv_w_ready_o = 1'b1;
                if (slv_w_valid_i && slv_w_last_i) begin
                    w_state_d = B_INJECT;
                end
            end
            B_FORWARD: begin
                mst_b_ready_o   = slv_b_ready_i;
                slv_b_valid_o   = mst_b_valid_i;
                slv_b_resp_o[1] = mst_b_resp_i[1];
                slv_b_resp_o[0] = (mst_b_resp_i[1] == 1'b0) ? b_excl_q : mst_b_resp_i[0];
                slv_b_user_o    = mst_b_user_i;
                slv_b_id_o      = mst_b_id_i;
                if (slv_b_valid_o && slv_b_ready_i) begin
                    b_excl_d    = 1'b0;
                    w_state_d   = AW_IDLE;
                end
            end
            B_INJECT: begin
                slv_b_id_o = w_id_q;
                slv_b_resp_o = 2'b00;
                slv_b_valid_o = 1'b1;
                if (slv_b_ready_i) begin
                    w_state_d = AW_IDLE;
                end
            end
            default: begin
                w_state_d = AW_IDLE;
            end
        endcase
    end
    axi_res_tbl #(
        .AXI_ADDR_WIDTH (AXI_ADDR_WIDTH),
        .AXI_ID_WIDTH   (AXI_ID_WIDTH)
    ) i_art (
        .clk_i                  (clk_i),
        .rst_ni                 (rst_ni),
        .clr_addr_i             (art_clr_addr),
        .clr_req_i              (art_clr_req),
        .clr_gnt_o              (art_clr_gnt),
        .set_addr_i             (art_set_addr),
        .set_id_i               (art_set_id),
        .set_req_i              (art_set_req),
        .set_gnt_o              (art_set_gnt),
        .check_addr_i           (art_check_addr),
        .check_id_i             (art_check_id),
        .check_res_o            (art_check_res),
        .check_req_i            (art_check_req),
        .check_gnt_o            (art_check_gnt)
    );
    stream_arbiter #(
        .DATA_T     (logic[AXI_ADDR_WIDTH-1:0]),
        .N_INP      (2)
    ) i_non_excl_acc_arb (
        .clk_i          (clk_i),
        .rst_ni         (rst_ni),
        .inp_data_i     ({rd_clr_addr,  wr_clr_addr}),
        .inp_valid_i    ({rd_clr_req,   wr_clr_req}),
        .inp_ready_o    ({rd_clr_gnt,   wr_clr_gnt}),
        .oup_data_o     (art_clr_addr),
        .oup_valid_o    (art_clr_req),
        .oup_ready_i    (art_clr_gnt)
    );
    always_ff @(posedge clk_i, negedge rst_ni) begin
        if (~rst_ni) begin
            b_excl_q    <= 1'b0;
            r_excl_q    <= 1'b0;
            r_state_q   <= R_IDLE;
            w_addr_q    <= '0;
            w_id_q      <= '0;
            w_state_q   <= AW_IDLE;
        end else begin
            b_excl_q    <= b_excl_d;
            r_excl_q    <= r_excl_d;
            r_state_q   <= r_state_d;
            w_addr_q    <= w_addr_d;
            w_id_q      <= w_id_d;
            w_state_q   <= w_state_d;
        end
    end
endmodule
module axi2mem #(
    parameter int unsigned AXI_ID_WIDTH      = 10,
    parameter int unsigned AXI_ADDR_WIDTH    = 64,
    parameter int unsigned AXI_DATA_WIDTH    = 64,
    parameter int unsigned AXI_USER_WIDTH    = 10
)(
    input logic                         clk_i,     
    input logic                         rst_ni,   
    AXI_BUS.Slave                       slave,
    output logic                        req_o,
    output logic                        we_o,
    output logic [AXI_ADDR_WIDTH-1:0]   addr_o,
    output logic [AXI_DATA_WIDTH/8-1:0] be_o,
    output logic [AXI_DATA_WIDTH-1:0]   data_o,
    input  logic [AXI_DATA_WIDTH-1:0]   data_i
);
    typedef enum logic [1:0] { FIXED = 2'b00, INCR = 2'b01, WRAP = 2'b10} axi_burst_t;
    localparam LOG_NR_BYTES = $clog2(AXI_DATA_WIDTH/8);
    typedef struct packed {
        logic [AXI_ID_WIDTH-1:0]   id;
        logic [AXI_ADDR_WIDTH-1:0] addr;
        logic [7:0]                len;
        logic [2:0]                size;
        axi_burst_t                burst;
    } ax_req_t;
    enum logic [2:0] { IDLE, READ, WRITE, SEND_B, WAIT_WVALID }  state_d, state_q;
    ax_req_t                   ax_req_d, ax_req_q;
    logic [AXI_ADDR_WIDTH-1:0] req_addr_d, req_addr_q;
    logic [7:0]                cnt_d, cnt_q;
    function automatic logic [AXI_ADDR_WIDTH-1:0] get_wrap_bounadry (input logic [AXI_ADDR_WIDTH-1:0] unaligned_address, input logic [7:0] len);
        logic [AXI_ADDR_WIDTH-1:0] warp_address = '0;
        if (len == 4'b1)
            warp_address[AXI_ADDR_WIDTH-1:1+LOG_NR_BYTES] = unaligned_address[AXI_ADDR_WIDTH-1:1+LOG_NR_BYTES];
        else if (len == 4'b11)
            warp_address[AXI_ADDR_WIDTH-1:2+LOG_NR_BYTES] = unaligned_address[AXI_ADDR_WIDTH-1:2+LOG_NR_BYTES];
        else if (len == 4'b111)
            warp_address[AXI_ADDR_WIDTH-1:3+LOG_NR_BYTES] = unaligned_address[AXI_ADDR_WIDTH-3:2+LOG_NR_BYTES];
        else if (len == 4'b1111)
            warp_address[AXI_ADDR_WIDTH-1:4+LOG_NR_BYTES] = unaligned_address[AXI_ADDR_WIDTH-3:4+LOG_NR_BYTES];
        return warp_address;
    endfunction
    logic [AXI_ADDR_WIDTH-1:0] aligned_address;
    logic [AXI_ADDR_WIDTH-1:0] wrap_boundary;
    logic [AXI_ADDR_WIDTH-1:0] upper_wrap_boundary;
    logic [AXI_ADDR_WIDTH-1:0] cons_addr;
    always_comb begin
        aligned_address = {ax_req_q.addr[AXI_ADDR_WIDTH-1:LOG_NR_BYTES], {{LOG_NR_BYTES}{1'b0}}};
        wrap_boundary = get_wrap_bounadry(ax_req_q.addr, ax_req_q.len);
        upper_wrap_boundary = wrap_boundary + ((ax_req_q.len + 1) << LOG_NR_BYTES);
        cons_addr = aligned_address + (cnt_q << LOG_NR_BYTES);
        state_d    = state_q;
        ax_req_d   = ax_req_q;
        req_addr_d = req_addr_q;
        cnt_d      = cnt_q;
        data_o = slave.w_data;
        be_o   = slave.w_strb;
        we_o   = 1'b0;
        req_o  = 1'b0;
        addr_o = '0;
        slave.aw_ready = 1'b0;
        slave.ar_ready = 1'b0;
        slave.r_valid  = 1'b0;
        slave.r_data   = data_i;
        slave.r_resp   = '0;
        slave.r_last   = '0;
        slave.r_id     = ax_req_q.id;
        slave.r_user   = '0;
        slave.w_ready  = 1'b0;
        slave.b_valid  = 1'b0;
        slave.b_resp   = 1'b0;
        slave.b_id     = 1'b0;
        slave.b_user   = 1'b0;
        case (state_q)
            IDLE: begin
                if (slave.ar_valid) begin
                    slave.ar_ready = 1'b1;
                    ax_req_d       = {slave.ar_id, slave.ar_addr, slave.ar_len, slave.ar_size, slave.ar_burst};
                    state_d        = READ;
                    req_o          = 1'b1;
                    addr_o         = slave.ar_addr;
                    req_addr_d     = slave.ar_addr;
                    cnt_d          = 1;
                end else if (slave.aw_valid) begin
                    slave.aw_ready = 1'b1;
                    slave.w_ready  = 1'b1;
                    addr_o         = slave.aw_addr;
                    ax_req_d       = {slave.aw_id, slave.aw_addr, slave.aw_len, slave.aw_size, slave.aw_burst};
                    if (slave.w_valid) begin
                        req_o          = 1'b1;
                        we_o           = 1'b1;
                        state_d        = (slave.w_last) ? SEND_B : WRITE;
                        cnt_d          = 1;
                    end else
                        state_d = WAIT_WVALID;
                end
            end
            WAIT_WVALID: begin
                slave.w_ready = 1'b1;
                addr_o = ax_req_q.addr;
                if (slave.w_valid) begin
                    req_o          = 1'b1;
                    we_o           = 1'b1;
                    state_d        = (slave.w_last) ? SEND_B : WRITE;
                    cnt_d          = 1;
                end
            end
            READ: begin
                req_o  = 1'b1;
                addr_o = req_addr_q;
                slave.r_valid = 1'b1;
                slave.r_data  = data_i;
                slave.r_id    = ax_req_q.id;
                slave.r_last  = (cnt_q == ax_req_q.len + 1);
                if (slave.r_ready) begin
                    case (ax_req_q.burst)
                        FIXED, INCR: addr_o = cons_addr;
                        WRAP:  begin
                            if (cons_addr == upper_wrap_boundary) begin
                                addr_o = wrap_boundary;
                            end else if (cons_addr > upper_wrap_boundary) begin
                                addr_o = ax_req_q.addr + ((cnt_q - ax_req_q.len) << LOG_NR_BYTES);
                            end else begin
                                addr_o = cons_addr;
                            end
                        end
                    endcase
                    if (slave.r_last) begin
                        state_d = IDLE;
                        req_o = 1'b0;
                    end
                    req_addr_d = addr_o;
                    cnt_d = cnt_q + 1;
                end
            end
            WRITE: begin
                slave.w_ready = 1'b1;
                if (slave.w_valid) begin
                    req_o         = 1'b1;
                    we_o          = 1'b1;
                    case (ax_req_q.burst)
                        FIXED, INCR: addr_o = cons_addr;
                        WRAP:  begin
                            if (cons_addr == upper_wrap_boundary) begin
                                addr_o = wrap_boundary;
                            end else if (cons_addr > upper_wrap_boundary) begin
                                addr_o = ax_req_q.addr + ((cnt_q - ax_req_q.len) << LOG_NR_BYTES);
                            end else begin
                                addr_o = cons_addr;
                            end
                        end
                    endcase
                    req_addr_d = addr_o;
                    cnt_d = cnt_q + 1;
                    if (slave.w_last)
                        state_d = SEND_B;
                end
            end
            SEND_B: begin
                slave.b_valid = 1'b1;
                slave.b_id    = ax_req_q.id;
                if (slave.b_ready)
                    state_d = IDLE;
            end
        endcase
    end
    always_ff @(posedge clk_i or negedge rst_ni) begin
        if (~rst_ni) begin
            state_q    <= IDLE;
            ax_req_q  <= '0;
            req_addr_q <= '0;
            cnt_q      <= '0;
        end else begin
            state_q    <= state_d;
            ax_req_q   <= ax_req_d;
            req_addr_q <= req_addr_d;
            cnt_q      <= cnt_d;
        end
    end
endmodule
module rv_plic_target #(
  parameter int N_SOURCE = 32,
  parameter int MAX_PRIO = 7,
  parameter     ALGORITHM = "SEQUENTIAL",  
  parameter int unsigned SRCW  = $clog2(N_SOURCE+1),
  parameter int unsigned PRIOW = $clog2(MAX_PRIO+1)  
) (
  input clk_i,
  input rst_ni,
  input [N_SOURCE-1:0] ip,
  input [N_SOURCE-1:0] ie,
  input [N_SOURCE-1:0][PRIOW-1:0] prio,
  input [PRIOW-1:0] threshold,
  output logic            irq,
  output logic [SRCW-1:0] irq_id
);
if (ALGORITHM == "SEQUENTIAL") begin : gen_sequential
  logic [PRIOW-1:0] max_prio;
  logic irq_next;
  logic [SRCW-1:0] irq_id_next;
  always_comb begin
    max_prio = threshold + 1'b1;  
    irq_id_next = '0;  
    irq_next = 1'b0;
    for (int i = N_SOURCE-1 ; i >= 0 ; i--) begin
      if ((ip[i] & ie[i]) == 1'b1 && prio[i] >= max_prio) begin
        max_prio = prio[i];
        irq_id_next = SRCW'(i+1);
        irq_next = 1'b1;
      end
    end  
  end
  always_ff @(posedge clk_i, negedge rst_ni) begin
    if (!rst_ni) begin
      irq <= 1'b0;
      irq_id <= '0;
    end else begin
      irq <= irq_next;
      irq_id <= irq_id_next;
    end
  end
end else if (ALGORITHM == "MATRIX") begin : gen_mat
  logic [N_SOURCE-1:0] is;
  logic [N_SOURCE-1:0][N_SOURCE-1:0] mat;
  logic [N_SOURCE-1:0] merged_row;
  assign is = ip & ie;
  always_comb begin
    merged_row[N_SOURCE-1] = is[N_SOURCE-1] & (prio[N_SOURCE-1] > threshold);
    for (int i = 0 ; i < N_SOURCE-1 ; i++) begin
      merged_row[i] = 1'b1;
      for (int j = i+1 ; j < N_SOURCE ; j++) begin
        mat[i][j] = (prio[i] <= threshold) ? 1'b0 :          
                    (is[i] & is[j]) ? prio[i] >= prio[j] :
                    (is[i]) ? 1'b 1 : 1'b 0 ;
        merged_row[i] = merged_row[i] & mat[i][j];  
      end  
    end  
  end  
  logic [N_SOURCE-1:0] lod;
  assign lod = merged_row & (~merged_row + 1'b1);
  always_ff @(posedge clk_i, negedge rst_ni) begin
    if (!rst_ni) begin
      irq <= 1'b0;
      irq_id <= '0;  
    end else if (|lod) begin
      for (int i = N_SOURCE-1 ; i >= 0 ; i--) begin
        if (lod[i] == 1'b1) begin
          irq <= 1'b 1;
          irq_id <= SRCW'(i + 1);
        end
      end  
    end else begin
      irq <= 1'b0;
      irq_id <= '0;
    end
  end  
end  
endmodule
module rv_plic_gateway #(
  parameter int N_SOURCE = 32
) (
  input clk_i,
  input rst_ni,
  input [N_SOURCE-1:0] src,
  input [N_SOURCE-1:0] le,       
  input [N_SOURCE-1:0] claim,  
  input [N_SOURCE-1:0] complete,  
  output logic [N_SOURCE-1:0] ip
);
logic [N_SOURCE-1:0] ia;     
logic [N_SOURCE-1:0] set;    
logic [N_SOURCE-1:0] src_d;
always_ff @(posedge clk_i, negedge rst_ni) begin
  if (!rst_ni) src_d <= '0;
  else         src_d <= src;
end
always_comb begin
  for (int i = 0 ; i < N_SOURCE; i++) begin
    set[i] = (le[i]) ? src[i] & ~src_d[i] : src[i] ;
  end
end
always_ff @(posedge clk_i, negedge rst_ni) begin
  if (!rst_ni) begin
    ip <= '0;
  end else begin
    ip <= (ip | (set & ~ia & ~ip)) & (~claim);
  end
end
always_ff @(posedge clk_i, negedge rst_ni) begin
  if (!rst_ni) begin
    ia <= '0;
  end else begin
    ia <= (ia | (set & ~ia)) & (~complete);
  end
end
endmodule
module plic_regs (
  input logic [30:0][2:0] prio_i,
  output logic [30:0][2:0] prio_o,
  output logic [30:0] prio_we_o,
  output logic [30:0] prio_re_o,
  input logic [0:0][30:0] ip_i,
  output logic [0:0] ip_re_o,
  input logic [1:0][30:0] ie_i,
  output logic [1:0][30:0] ie_o,
  output logic [1:0] ie_we_o,
  output logic [1:0] ie_re_o,
  input logic [1:0][2:0] threshold_i,
  output logic [1:0][2:0] threshold_o,
  output logic [1:0] threshold_we_o,
  output logic [1:0] threshold_re_o,
  input logic [1:0][4:0] cc_i,
  output logic [1:0][4:0] cc_o,
  output logic [1:0] cc_we_o,
  output logic [1:0] cc_re_o,
  input  reg_intf::reg_intf_req_a32_d32 req_i,
  output reg_intf::reg_intf_resp_d32    resp_o
);
always_comb begin
  resp_o.ready = 1'b1;
  resp_o.rdata = '0;
  resp_o.error = '0;
  prio_o = '0;
  prio_we_o = '0;
  prio_re_o = '0;
  ie_o = '0;
  ie_we_o = '0;
  ie_re_o = '0;
  threshold_o = '0;
  threshold_we_o = '0;
  threshold_re_o = '0;
  cc_o = '0;
  cc_we_o = '0;
  cc_re_o = '0;
  if (req_i.valid) begin
    if (req_i.write) begin
      unique case(req_i.addr)
        32'hc000000: begin
          prio_o[0][2:0] = req_i.wdata[2:0];
          prio_we_o[0] = 1'b1;
        end
        32'hc000004: begin
          prio_o[1][2:0] = req_i.wdata[2:0];
          prio_we_o[1] = 1'b1;
        end
        32'hc000008: begin
          prio_o[2][2:0] = req_i.wdata[2:0];
          prio_we_o[2] = 1'b1;
        end
        32'hc00000c: begin
          prio_o[3][2:0] = req_i.wdata[2:0];
          prio_we_o[3] = 1'b1;
        end
        32'hc000010: begin
          prio_o[4][2:0] = req_i.wdata[2:0];
          prio_we_o[4] = 1'b1;
        end
        32'hc000014: begin
          prio_o[5][2:0] = req_i.wdata[2:0];
          prio_we_o[5] = 1'b1;
        end
        32'hc000018: begin
          prio_o[6][2:0] = req_i.wdata[2:0];
          prio_we_o[6] = 1'b1;
        end
        32'hc00001c: begin
          prio_o[7][2:0] = req_i.wdata[2:0];
          prio_we_o[7] = 1'b1;
        end
        32'hc000020: begin
          prio_o[8][2:0] = req_i.wdata[2:0];
          prio_we_o[8] = 1'b1;
        end
        32'hc000024: begin
          prio_o[9][2:0] = req_i.wdata[2:0];
          prio_we_o[9] = 1'b1;
        end
        32'hc000028: begin
          prio_o[10][2:0] = req_i.wdata[2:0];
          prio_we_o[10] = 1'b1;
        end
        32'hc00002c: begin
          prio_o[11][2:0] = req_i.wdata[2:0];
          prio_we_o[11] = 1'b1;
        end
        32'hc000030: begin
          prio_o[12][2:0] = req_i.wdata[2:0];
          prio_we_o[12] = 1'b1;
        end
        32'hc000034: begin
          prio_o[13][2:0] = req_i.wdata[2:0];
          prio_we_o[13] = 1'b1;
        end
        32'hc000038: begin
          prio_o[14][2:0] = req_i.wdata[2:0];
          prio_we_o[14] = 1'b1;
        end
        32'hc00003c: begin
          prio_o[15][2:0] = req_i.wdata[2:0];
          prio_we_o[15] = 1'b1;
        end
        32'hc000040: begin
          prio_o[16][2:0] = req_i.wdata[2:0];
          prio_we_o[16] = 1'b1;
        end
        32'hc000044: begin
          prio_o[17][2:0] = req_i.wdata[2:0];
          prio_we_o[17] = 1'b1;
        end
        32'hc000048: begin
          prio_o[18][2:0] = req_i.wdata[2:0];
          prio_we_o[18] = 1'b1;
        end
        32'hc00004c: begin
          prio_o[19][2:0] = req_i.wdata[2:0];
          prio_we_o[19] = 1'b1;
        end
        32'hc000050: begin
          prio_o[20][2:0] = req_i.wdata[2:0];
          prio_we_o[20] = 1'b1;
        end
        32'hc000054: begin
          prio_o[21][2:0] = req_i.wdata[2:0];
          prio_we_o[21] = 1'b1;
        end
        32'hc000058: begin
          prio_o[22][2:0] = req_i.wdata[2:0];
          prio_we_o[22] = 1'b1;
        end
        32'hc00005c: begin
          prio_o[23][2:0] = req_i.wdata[2:0];
          prio_we_o[23] = 1'b1;
        end
        32'hc000060: begin
          prio_o[24][2:0] = req_i.wdata[2:0];
          prio_we_o[24] = 1'b1;
        end
        32'hc000064: begin
          prio_o[25][2:0] = req_i.wdata[2:0];
          prio_we_o[25] = 1'b1;
        end
        32'hc000068: begin
          prio_o[26][2:0] = req_i.wdata[2:0];
          prio_we_o[26] = 1'b1;
        end
        32'hc00006c: begin
          prio_o[27][2:0] = req_i.wdata[2:0];
          prio_we_o[27] = 1'b1;
        end
        32'hc000070: begin
          prio_o[28][2:0] = req_i.wdata[2:0];
          prio_we_o[28] = 1'b1;
        end
        32'hc000074: begin
          prio_o[29][2:0] = req_i.wdata[2:0];
          prio_we_o[29] = 1'b1;
        end
        32'hc000078: begin
          prio_o[30][2:0] = req_i.wdata[2:0];
          prio_we_o[30] = 1'b1;
        end
        32'hc002000: begin
          ie_o[0][30:0] = req_i.wdata[30:0];
          ie_we_o[0] = 1'b1;
        end
        32'hc002080: begin
          ie_o[1][30:0] = req_i.wdata[30:0];
          ie_we_o[1] = 1'b1;
        end
        32'hc200000: begin
          threshold_o[0][2:0] = req_i.wdata[2:0];
          threshold_we_o[0] = 1'b1;
        end
        32'hc201000: begin
          threshold_o[1][2:0] = req_i.wdata[2:0];
          threshold_we_o[1] = 1'b1;
        end
        32'hc200004: begin
          cc_o[0][4:0] = req_i.wdata[4:0];
          cc_we_o[0] = 1'b1;
        end
        32'hc201004: begin
          cc_o[1][4:0] = req_i.wdata[4:0];
          cc_we_o[1] = 1'b1;
        end
        default: resp_o.error = 1'b1;
      endcase
    end else begin
      unique case(req_i.addr)
        32'hc000000: begin
          resp_o.rdata[2:0] = prio_i[0][2:0];
          prio_re_o[0] = 1'b1;
        end
        32'hc000004: begin
          resp_o.rdata[2:0] = prio_i[1][2:0];
          prio_re_o[1] = 1'b1;
        end
        32'hc000008: begin
          resp_o.rdata[2:0] = prio_i[2][2:0];
          prio_re_o[2] = 1'b1;
        end
        32'hc00000c: begin
          resp_o.rdata[2:0] = prio_i[3][2:0];
          prio_re_o[3] = 1'b1;
        end
        32'hc000010: begin
          resp_o.rdata[2:0] = prio_i[4][2:0];
          prio_re_o[4] = 1'b1;
        end
        32'hc000014: begin
          resp_o.rdata[2:0] = prio_i[5][2:0];
          prio_re_o[5] = 1'b1;
        end
        32'hc000018: begin
          resp_o.rdata[2:0] = prio_i[6][2:0];
          prio_re_o[6] = 1'b1;
        end
        32'hc00001c: begin
          resp_o.rdata[2:0] = prio_i[7][2:0];
          prio_re_o[7] = 1'b1;
        end
        32'hc000020: begin
          resp_o.rdata[2:0] = prio_i[8][2:0];
          prio_re_o[8] = 1'b1;
        end
        32'hc000024: begin
          resp_o.rdata[2:0] = prio_i[9][2:0];
          prio_re_o[9] = 1'b1;
        end
        32'hc000028: begin
          resp_o.rdata[2:0] = prio_i[10][2:0];
          prio_re_o[10] = 1'b1;
        end
        32'hc00002c: begin
          resp_o.rdata[2:0] = prio_i[11][2:0];
          prio_re_o[11] = 1'b1;
        end
        32'hc000030: begin
          resp_o.rdata[2:0] = prio_i[12][2:0];
          prio_re_o[12] = 1'b1;
        end
        32'hc000034: begin
          resp_o.rdata[2:0] = prio_i[13][2:0];
          prio_re_o[13] = 1'b1;
        end
        32'hc000038: begin
          resp_o.rdata[2:0] = prio_i[14][2:0];
          prio_re_o[14] = 1'b1;
        end
        32'hc00003c: begin
          resp_o.rdata[2:0] = prio_i[15][2:0];
          prio_re_o[15] = 1'b1;
        end
        32'hc000040: begin
          resp_o.rdata[2:0] = prio_i[16][2:0];
          prio_re_o[16] = 1'b1;
        end
        32'hc000044: begin
          resp_o.rdata[2:0] = prio_i[17][2:0];
          prio_re_o[17] = 1'b1;
        end
        32'hc000048: begin
          resp_o.rdata[2:0] = prio_i[18][2:0];
          prio_re_o[18] = 1'b1;
        end
        32'hc00004c: begin
          resp_o.rdata[2:0] = prio_i[19][2:0];
          prio_re_o[19] = 1'b1;
        end
        32'hc000050: begin
          resp_o.rdata[2:0] = prio_i[20][2:0];
          prio_re_o[20] = 1'b1;
        end
        32'hc000054: begin
          resp_o.rdata[2:0] = prio_i[21][2:0];
          prio_re_o[21] = 1'b1;
        end
        32'hc000058: begin
          resp_o.rdata[2:0] = prio_i[22][2:0];
          prio_re_o[22] = 1'b1;
        end
        32'hc00005c: begin
          resp_o.rdata[2:0] = prio_i[23][2:0];
          prio_re_o[23] = 1'b1;
        end
        32'hc000060: begin
          resp_o.rdata[2:0] = prio_i[24][2:0];
          prio_re_o[24] = 1'b1;
        end
        32'hc000064: begin
          resp_o.rdata[2:0] = prio_i[25][2:0];
          prio_re_o[25] = 1'b1;
        end
        32'hc000068: begin
          resp_o.rdata[2:0] = prio_i[26][2:0];
          prio_re_o[26] = 1'b1;
        end
        32'hc00006c: begin
          resp_o.rdata[2:0] = prio_i[27][2:0];
          prio_re_o[27] = 1'b1;
        end
        32'hc000070: begin
          resp_o.rdata[2:0] = prio_i[28][2:0];
          prio_re_o[28] = 1'b1;
        end
        32'hc000074: begin
          resp_o.rdata[2:0] = prio_i[29][2:0];
          prio_re_o[29] = 1'b1;
        end
        32'hc000078: begin
          resp_o.rdata[2:0] = prio_i[30][2:0];
          prio_re_o[30] = 1'b1;
        end
        32'hc001000: begin
          resp_o.rdata[30:0] = ip_i[0][30:0];
          ip_re_o[0] = 1'b1;
        end
        32'hc002000: begin
          resp_o.rdata[30:0] = ie_i[0][30:0];
          ie_re_o[0] = 1'b1;
        end
        32'hc002080: begin
          resp_o.rdata[30:0] = ie_i[1][30:0];
          ie_re_o[1] = 1'b1;
        end
        32'hc200000: begin
          resp_o.rdata[2:0] = threshold_i[0][2:0];
          threshold_re_o[0] = 1'b1;
        end
        32'hc201000: begin
          resp_o.rdata[2:0] = threshold_i[1][2:0];
          threshold_re_o[1] = 1'b1;
        end
        32'hc200004: begin
          resp_o.rdata[4:0] = cc_i[0][4:0];
          cc_re_o[0] = 1'b1;
        end
        32'hc201004: begin
          resp_o.rdata[4:0] = cc_i[1][4:0];
          cc_re_o[1] = 1'b1;
        end
        default: resp_o.error = 1'b1;
      endcase
    end
  end
end
endmodule
module plic_top #(
  parameter int N_SOURCE    = 30,
  parameter int N_TARGET    = 2,
  parameter int MAX_PRIO    = 7,
  parameter int SRCW        = $clog2(N_SOURCE+1)
) (
  input  logic clk_i,     
  input  logic rst_ni,   
  input  reg_intf::reg_intf_req_a32_d32 req_i,
  output reg_intf::reg_intf_resp_d32    resp_o,
  input logic [N_SOURCE-1:0] le_i,  
  input  logic [N_SOURCE-1:0] irq_sources_i,
  output logic [N_TARGET-1:0] eip_targets_o
);
  localparam PRIOW = $clog2(MAX_PRIO+1);
  logic [N_SOURCE-1:0] ip;
  logic [N_TARGET-1:0][PRIOW-1:0]    threshold_q;
  logic [N_TARGET-1:0]               claim_re;  
  logic [N_TARGET-1:0][SRCW-1:0]     claim_id;
  logic [N_SOURCE-1:0]               claim;  
  logic [N_TARGET-1:0]               complete_we;  
  logic [N_TARGET-1:0][SRCW-1:0]     complete_id;
  logic [N_SOURCE-1:0]               complete;  
  logic [N_SOURCE-1:0][PRIOW-1:0]    prio_q;
  logic [N_TARGET-1:0][N_SOURCE-1:0] ie_q;
  always_comb begin
    claim = '0;
    complete = '0;
    for (int i = 0 ; i < N_TARGET ; i++) begin
      if (claim_re[i] && claim_id[i] != 0) claim[claim_id[i]-1] = 1'b1;
      if (complete_we[i] && complete_id[i] != 0) complete[complete_id[i]-1] = 1'b1;
    end
  end
  rv_plic_gateway #(
    .N_SOURCE (N_SOURCE)
  ) i_rv_plic_gateway (
    .clk_i,
    .rst_ni,
    .src(irq_sources_i),
    .le(le_i),
    .claim(claim),
    .complete(complete),
    .ip(ip)
  );
  for (genvar i = 0 ; i < N_TARGET; i++) begin : gen_target
    rv_plic_target #(
      .N_SOURCE  ( N_SOURCE ),
      .MAX_PRIO  ( MAX_PRIO ),
      .ALGORITHM ( "SEQUENTIAL" )
    ) i_target (
      .clk_i,
      .rst_ni,
      .ip(ip),
      .ie(ie_q[i]),
      .prio(prio_q),
      .threshold(threshold_q[i]),
      .irq(eip_targets_o[i]),
      .irq_id(claim_id[i])
    );
  end
  logic [N_TARGET-1:0] threshold_we_o;
  logic [N_TARGET-1:0][PRIOW-1:0] threshold_o;
  logic [N_SOURCE:0][PRIOW-1:0] prio_i, prio_o;
  logic [N_SOURCE:0] prio_we_o;
  logic [N_TARGET-1:0][N_SOURCE:0] ie_i, ie_o;
  logic [N_TARGET-1:0] ie_we_o;
  plic_regs i_plic_regs (
    .prio_i(prio_i),
    .prio_o(prio_o),
    .prio_we_o(prio_we_o),
    .prio_re_o(),  
    .ip_i({ip, 1'b0}),
    .ip_re_o(),  
    .ie_i(ie_i),
    .ie_o(ie_o),
    .ie_we_o(ie_we_o),
    .ie_re_o(),  
    .threshold_i(threshold_q),
    .threshold_o(threshold_o),
    .threshold_we_o(threshold_we_o),
    .threshold_re_o(),  
    .cc_i(claim_id),
    .cc_o(complete_id),
    .cc_we_o(complete_we),
    .cc_re_o(claim_re),
    .req_i,
    .resp_o
  );
  assign prio_i[0] = '0;
  for (genvar i = 0; i < N_TARGET; i++) begin
    assign ie_i[i] = {ie_q[i][N_SOURCE-1:0], 1'b0};
  end
  for (genvar i = 1; i < N_SOURCE + 1; i++) begin
    assign prio_i[i] = prio_q[i - 1];
  end
  always_ff @(posedge clk_i or negedge rst_ni) begin
    if (~rst_ni) begin
      prio_q <= '0;
      ie_q <= '0;
      threshold_q <= '0;
    end else begin
      for (int i = 0; i < N_SOURCE; i++) begin
        prio_q[i] <= prio_we_o[i + 1] ? prio_o[i + 1] : prio_q[i];
      end
      for (int i = 0; i < N_TARGET; i++) begin
        threshold_q[i] <= threshold_we_o[i] ? threshold_o[i] : threshold_q[i];
        ie_q[i] <= ie_we_o[i] ? ie_o[i][N_SOURCE:1] : ie_q[i];
      end
    end
  end
endmodule
module dmi_cdc (
    input  logic             tck_i,
    input  logic             trst_ni,
    input  dm::dmi_req_t     jtag_dmi_req_i,
    output logic             jtag_dmi_ready_o,
    input  logic             jtag_dmi_valid_i,
    output dm::dmi_resp_t    jtag_dmi_resp_o,
    output logic             jtag_dmi_valid_o,
    input  logic             jtag_dmi_ready_i,
    input  logic             clk_i,
    input  logic             rst_ni,
    output dm::dmi_req_t     core_dmi_req_o,
    output logic             core_dmi_valid_o,
    input  logic             core_dmi_ready_i,
    input dm::dmi_resp_t     core_dmi_resp_i,
    output logic             core_dmi_ready_o,
    input  logic             core_dmi_valid_i
  );
  cdc_2phase #(.T(dm::dmi_req_t)) i_cdc_req (
    .src_rst_ni  ( trst_ni          ),
    .src_clk_i   ( tck_i            ),
    .src_data_i  ( jtag_dmi_req_i   ),
    .src_valid_i ( jtag_dmi_valid_i ),
    .src_ready_o ( jtag_dmi_ready_o ),
    .dst_rst_ni  ( rst_ni           ),
    .dst_clk_i   ( clk_i            ),
    .dst_data_o  ( core_dmi_req_o   ),
    .dst_valid_o ( core_dmi_valid_o ),
    .dst_ready_i ( core_dmi_ready_i )
  );
  cdc_2phase #(.T(dm::dmi_resp_t)) i_cdc_resp (
    .src_rst_ni  ( rst_ni           ),
    .src_clk_i   ( clk_i            ),
    .src_data_i  ( core_dmi_resp_i  ),
    .src_valid_i ( core_dmi_valid_i ),
    .src_ready_o ( core_dmi_ready_o ),
    .dst_rst_ni  ( trst_ni          ),
    .dst_clk_i   ( tck_i            ),
    .dst_data_o  ( jtag_dmi_resp_o  ),
    .dst_valid_o ( jtag_dmi_valid_o ),
    .dst_ready_i ( jtag_dmi_ready_i )
  );
endmodule
module dmi_jtag #(
    parameter logic [31:0] IdcodeValue = 32'h00000001
) (
    input  logic         clk_i,       
    input  logic         rst_ni,      
    input  logic         testmode_i,
    output logic         dmi_rst_no,  
    output dm::dmi_req_t dmi_req_o,
    output logic         dmi_req_valid_o,
    input  logic         dmi_req_ready_i,
    input dm::dmi_resp_t dmi_resp_i,
    output logic         dmi_resp_ready_o,
    input  logic         dmi_resp_valid_i,
    input  logic         tck_i,     
    input  logic         tms_i,     
    input  logic         trst_ni,   
    input  logic         td_i,      
    output logic         td_o,      
    output logic         tdo_oe_o   
);
    assign       dmi_rst_no = rst_ni;
    logic        test_logic_reset;
    logic        shift_dr;
    logic        update_dr;
    logic        capture_dr;
    logic        dmi_access;
    logic        dtmcs_select;
    logic        dmi_reset;
    logic        dmi_tdi;
    logic        dmi_tdo;
    dm::dmi_req_t  dmi_req;
    logic          dmi_req_ready;
    logic          dmi_req_valid;
    dm::dmi_resp_t dmi_resp;
    logic          dmi_resp_valid;
    logic          dmi_resp_ready;
    typedef struct packed {
        logic [6:0]  address;
        logic [31:0] data;
        logic [1:0]  op;
    } dmi_t;
    typedef enum logic [1:0] {
                                DMINoError = 2'h0, DMIReservedError = 2'h1,
                                DMIOPFailed = 2'h2, DMIBusy = 2'h3
                             } dmi_error_e;
    typedef enum logic [2:0] { Idle, Read, WaitReadValid, Write, WaitWriteValid } state_e;
    state_e state_d, state_q;
    logic [$bits(dmi_t)-1:0] dr_d, dr_q;
    logic [6:0] address_d, address_q;
    logic [31:0] data_d, data_q;
    dmi_t  dmi;
    assign dmi          = dmi_t'(dr_q);
    assign dmi_req.addr = address_q;
    assign dmi_req.data = data_q;
    assign dmi_req.op   = (state_q == Write) ? dm::DTM_WRITE : dm::DTM_READ;
    assign dmi_resp_ready = 1'b1;
    logic error_dmi_busy;
    dmi_error_e error_d, error_q;
    always_comb begin
        error_dmi_busy = 1'b0;
        state_d   = state_q;
        address_d = address_q;
        data_d    = data_q;
        error_d   = error_q;
        dmi_req_valid = 1'b0;
        case (state_q)
            Idle: begin
                if (dmi_access && update_dr && (error_q == DMINoError)) begin
                    address_d = dmi.address;
                    data_d = dmi.data;
                    if (dm::dtm_op_e'(dmi.op) == dm::DTM_READ) begin
                        state_d = Read;
                    end else if (dm::dtm_op_e'(dmi.op) == dm::DTM_WRITE) begin
                        state_d = Write;
                    end
                end
            end
            Read: begin
                dmi_req_valid = 1'b1;
                if (dmi_req_ready) begin
                    state_d = WaitReadValid;
                end
            end
            WaitReadValid: begin
                if (dmi_resp_valid) begin
                    data_d = dmi_resp.data;
                    state_d = Idle;
                end
            end
            Write: begin
                dmi_req_valid = 1'b1;
                if (dmi_req_ready) begin
                    state_d = Idle;
                end
            end
            WaitWriteValid: begin
                if (dmi_resp_valid) begin
                    state_d = Idle;
                end
            end
        endcase
        if (update_dr && state_q != Idle) begin
            error_dmi_busy = 1'b1;
        end
        if (capture_dr && state_q inside {Read, WaitReadValid}) begin
            error_dmi_busy = 1'b1;
        end
        if (error_dmi_busy) begin
            error_d = DMIBusy;
        end
        if (dmi_reset && dtmcs_select) begin
            error_d = DMINoError;
        end
    end
    assign dmi_tdo = dr_q[0];
    always_comb begin
        dr_d    = dr_q;
        if (capture_dr) begin
            if (dmi_access) begin
                if (error_q == DMINoError && !error_dmi_busy) begin
                    dr_d = {address_q, data_q, DMINoError};
                end else if (error_q == DMIBusy || error_dmi_busy) begin
                    dr_d = {address_q, data_q, DMIBusy};
                end
            end
        end
        if (shift_dr) begin
            if (dmi_access) dr_d = {dmi_tdi, dr_q[$bits(dr_q)-1:1]};
        end
        if (test_logic_reset) begin
            dr_d = '0;
        end
    end
    always_ff @(posedge tck_i or negedge trst_ni) begin
        if (!trst_ni) begin
            dr_q      <= '0;
            state_q   <= Idle;
            address_q <= '0;
            data_q    <= '0;
            error_q   <= DMINoError;
        end else begin
            dr_q      <= dr_d;
            state_q   <= state_d;
            address_q <= address_d;
            data_q    <= data_d;
            error_q   <= error_d;
        end
    end
    dmi_jtag_tap #(
        .IrLength (5),
        .IdcodeValue(IdcodeValue)
    ) i_dmi_jtag_tap (
        .tck_i,
        .tms_i,
        .trst_ni,
        .td_i,
        .td_o,
        .tdo_oe_o,
        .testmode_i         ( testmode_i       ),
        .test_logic_reset_o ( test_logic_reset ),
        .shift_dr_o         ( shift_dr         ),
        .update_dr_o        ( update_dr        ),
        .capture_dr_o       ( capture_dr       ),
        .dmi_access_o       ( dmi_access       ),
        .dtmcs_select_o     ( dtmcs_select     ),
        .dmi_reset_o        ( dmi_reset        ),
        .dmi_error_i        ( error_q          ),
        .dmi_tdi_o          ( dmi_tdi          ),
        .dmi_tdo_i          ( dmi_tdo          )
    );
    dmi_cdc i_dmi_cdc (
        .tck_i,
        .trst_ni,
        .jtag_dmi_req_i    ( dmi_req          ),
        .jtag_dmi_ready_o  ( dmi_req_ready    ),
        .jtag_dmi_valid_i  ( dmi_req_valid    ),
        .jtag_dmi_resp_o   ( dmi_resp         ),
        .jtag_dmi_valid_o  ( dmi_resp_valid   ),
        .jtag_dmi_ready_i  ( dmi_resp_ready   ),
        .clk_i,
        .rst_ni,
        .core_dmi_req_o    ( dmi_req_o        ),
        .core_dmi_valid_o  ( dmi_req_valid_o  ),
        .core_dmi_ready_i  ( dmi_req_ready_i  ),
        .core_dmi_resp_i   ( dmi_resp_i       ),
        .core_dmi_ready_o  ( dmi_resp_ready_o ),
        .core_dmi_valid_i  ( dmi_resp_valid_i )
    );
endmodule
module dmi_jtag_tap #(
    parameter int IrLength = 5,
    parameter logic [31:0] IdcodeValue = 32'h00000001
)(
    input  logic        tck_i,     
    input  logic        tms_i,     
    input  logic        trst_ni,   
    input  logic        td_i,      
    output logic        td_o,      
    output logic        tdo_oe_o,  
    input  logic        testmode_i,
    output logic        test_logic_reset_o,
    output logic        shift_dr_o,
    output logic        update_dr_o,
    output logic        capture_dr_o,
    output logic        dmi_access_o,
    output logic        dtmcs_select_o,
    output logic        dmi_reset_o,
    input  logic [1:0]  dmi_error_i,
    output logic        dmi_tdi_o,
    input  logic        dmi_tdo_i
);
    assign dmi_tdi_o = td_i;
    typedef enum logic [3:0] { TestLogicReset, RunTestIdle, SelectDrScan,
                     CaptureDr, ShiftDr, Exit1Dr, PauseDr, Exit2Dr,
                     UpdateDr, SelectIrScan, CaptureIr, ShiftIr,
                     Exit1Ir, PauseIr, Exit2Ir, UpdateIr } tap_state_e;
    tap_state_e tap_state_q, tap_state_d;
    typedef enum logic [IrLength-1:0] {
        BYPASS0   = 'h0,
        IDCODE    = 'h1,
        DTMCSR    = 'h10,
        DMIACCESS = 'h11,
        BYPASS1   = 'h1f
    } ir_reg_e;
    typedef struct packed {
        logic [31:18] zero1;
        logic         dmihardreset;
        logic         dmireset;
        logic         zero0;
        logic [14:12] idle;
        logic [11:10] dmistat;
        logic [9:4]   abits;
        logic [3:0]   version;
    } dtmcs_t;
    logic [IrLength-1:0]  jtag_ir_shift_d, jtag_ir_shift_q;  
    ir_reg_e              jtag_ir_d, jtag_ir_q;  
    logic capture_ir, shift_ir, pause_ir, update_ir;
    always_comb begin
        jtag_ir_shift_d = jtag_ir_shift_q;
        jtag_ir_d       = jtag_ir_q;
        if (shift_ir) begin
            jtag_ir_shift_d = {td_i, jtag_ir_shift_q[IrLength-1:1]};
        end
        if (capture_ir) begin
            jtag_ir_shift_d =  'b0101;
        end
        if (update_ir) begin
            jtag_ir_d = ir_reg_e'(jtag_ir_shift_q);
        end
        if (test_logic_reset_o) begin
            jtag_ir_shift_d = '0;
            jtag_ir_d       = IDCODE;
        end
    end
    always_ff @(posedge tck_i, negedge trst_ni) begin
        if (!trst_ni) begin
            jtag_ir_shift_q <= '0;
            jtag_ir_q       <= IDCODE;
        end else begin
            jtag_ir_shift_q <= jtag_ir_shift_d;
            jtag_ir_q       <= jtag_ir_d;
        end
    end
    logic [31:0] idcode_d, idcode_q;
    logic        idcode_select;
    logic        bypass_select;
    dtmcs_t      dtmcs_d, dtmcs_q;
    logic        bypass_d, bypass_q;   
    assign dmi_reset_o = dtmcs_q.dmireset;
    always_comb begin
        idcode_d = idcode_q;
        bypass_d = bypass_q;
        dtmcs_d  = dtmcs_q;
        if (capture_dr_o) begin
            if (idcode_select) idcode_d = IdcodeValue;
            if (bypass_select) bypass_d = 1'b0;
            if (dtmcs_select_o) begin
                dtmcs_d  = '{
                                zero1        : '0,
                                dmihardreset : 1'b0,
                                dmireset     : 1'b0,
                                zero0        : '0,
                                idle         : 'd1,          
                                dmistat      : dmi_error_i,  
                                abits        : 'd7,  
                                version      : 'd1   
                            };
            end
        end
        if (shift_dr_o) begin
            if (idcode_select)  idcode_d = {td_i, idcode_q[31:1]};
            if (bypass_select)  bypass_d = td_i;
            if (dtmcs_select_o) dtmcs_d  = {td_i, dtmcs_q[31:1]};
        end
        if (test_logic_reset_o) begin
            idcode_d = IdcodeValue;
            bypass_d = 1'b0;
        end
    end
    always_comb begin
        dmi_access_o   = 1'b0;
        dtmcs_select_o = 1'b0;
        idcode_select  = 1'b0;
        bypass_select  = 1'b0;
        case (jtag_ir_q)
            BYPASS0:   bypass_select  = 1'b1;
            IDCODE:    idcode_select  = 1'b1;
            DTMCSR:    dtmcs_select_o = 1'b1;
            DMIACCESS: dmi_access_o   = 1'b1;
            BYPASS1:   bypass_select  = 1'b1;
            default:   bypass_select  = 1'b1;
        endcase
    end
    logic tdo_mux;
    always_comb begin
        if (shift_ir) begin
            tdo_mux = jtag_ir_shift_q[0];
        end else begin
          case (jtag_ir_q)     
            IDCODE:         tdo_mux = idcode_q[0];      
            DTMCSR:         tdo_mux = dtmcs_q[0];
            DMIACCESS:      tdo_mux = dmi_tdo_i;        
            default:        tdo_mux = bypass_q;       
          endcase
        end
    end
    logic tck_n, tck_ni;
    cluster_clock_inverter i_tck_inv (
        .clk_i ( tck_i  ),
        .clk_o ( tck_ni )
    );
    pulp_clock_mux2 i_dft_tck_mux (
        .clk0_i    ( tck_ni     ),
        .clk1_i    ( tck_i      ),  
        .clk_sel_i ( testmode_i ),
        .clk_o     ( tck_n      )
    );
    always_ff @(posedge tck_n, negedge trst_ni) begin
        if (!trst_ni) begin
            td_o     <= 1'b0;
            tdo_oe_o <= 1'b0;
        end else begin
            td_o     <= tdo_mux;
            tdo_oe_o <= (shift_ir | shift_dr_o);
        end
    end
    always_comb begin
        test_logic_reset_o = 1'b0;
        capture_dr_o       = 1'b0;
        shift_dr_o         = 1'b0;
        update_dr_o        = 1'b0;
        capture_ir         = 1'b0;
        shift_ir           = 1'b0;
        pause_ir           = 1'b0;
        update_ir          = 1'b0;
        case (tap_state_q)
            TestLogicReset: begin
                tap_state_d = (tms_i) ? TestLogicReset : RunTestIdle;
                test_logic_reset_o = 1'b1;
            end
            RunTestIdle: begin
                tap_state_d = (tms_i) ? SelectDrScan : RunTestIdle;
            end
            SelectDrScan: begin
                tap_state_d = (tms_i) ? SelectIrScan : CaptureDr;
            end
            CaptureDr: begin
                capture_dr_o = 1'b1;
                tap_state_d = (tms_i) ? Exit1Dr : ShiftDr;
            end
            ShiftDr: begin
                shift_dr_o = 1'b1;
                tap_state_d = (tms_i) ? Exit1Dr : ShiftDr;
            end
            Exit1Dr: begin
                tap_state_d = (tms_i) ? UpdateDr : PauseDr;
            end
            PauseDr: begin
                tap_state_d = (tms_i) ? Exit2Dr : PauseDr;
            end
            Exit2Dr: begin
                tap_state_d = (tms_i) ? UpdateDr : ShiftDr;
            end
            UpdateDr: begin
                update_dr_o = 1'b1;
                tap_state_d = (tms_i) ? SelectDrScan : RunTestIdle;
            end
            SelectIrScan: begin
                tap_state_d = (tms_i) ? TestLogicReset : CaptureIr;
            end
            CaptureIr: begin
                capture_ir = 1'b1;
                tap_state_d = (tms_i) ? Exit1Ir : ShiftIr;
            end
            ShiftIr: begin
                shift_ir = 1'b1;
                tap_state_d = (tms_i) ? Exit1Ir : ShiftIr;
            end
            Exit1Ir: begin
                tap_state_d = (tms_i) ? UpdateIr : PauseIr;
            end
            PauseIr: begin
                pause_ir = 1'b1;
                tap_state_d = (tms_i) ? Exit2Ir : PauseIr;
            end
            Exit2Ir: begin
                tap_state_d = (tms_i) ? UpdateIr : ShiftIr;
            end
            UpdateIr: begin
                update_ir = 1'b1;
                tap_state_d = (tms_i) ? SelectDrScan : RunTestIdle;
            end
            default: tap_state_d = TestLogicReset;   
      endcase
    end
    always_ff @(posedge tck_i or negedge trst_ni) begin
        if (!trst_ni) begin
            tap_state_q <= RunTestIdle;
            idcode_q    <= IdcodeValue;
            bypass_q    <= 1'b0;
            dtmcs_q     <= '0;
        end else begin
            tap_state_q <= tap_state_d;
            idcode_q    <= idcode_d;
            bypass_q    <= bypass_d;
            dtmcs_q     <= dtmcs_d;
        end
    end
endmodule
module dm_csrs #(
    parameter int                 NrHarts          = 1,
    parameter int                 BusWidth         = 32,
    parameter logic [NrHarts-1:0] SelectableHarts  = 1
) (
    input  logic                              clk_i,            
    input  logic                              rst_ni,           
    input  logic                              testmode_i,
    input  logic                              dmi_rst_ni,       
    input  logic                              dmi_req_valid_i,
    output logic                              dmi_req_ready_o,
    input  dm::dmi_req_t                      dmi_req_i,
    output logic                              dmi_resp_valid_o,
    input  logic                              dmi_resp_ready_i,
    output dm::dmi_resp_t                     dmi_resp_o,
    output logic                              ndmreset_o,       
    output logic                              dmactive_o,       
    input  dm::hartinfo_t [NrHarts-1:0]       hartinfo_i,       
    input  logic [NrHarts-1:0]                halted_i,         
    input  logic [NrHarts-1:0]                unavailable_i,    
    input  logic [NrHarts-1:0]                resumeack_i,      
    output logic [19:0]                       hartsel_o,        
    output logic [NrHarts-1:0]                haltreq_o,        
    output logic [NrHarts-1:0]                resumereq_o,      
    output logic                              clear_resumeack_o,
    output logic                              cmd_valid_o,        
    output dm::command_t                      cmd_o,              
    input  logic                              cmderror_valid_i,   
    input  dm::cmderr_e                       cmderror_i,         
    input  logic                              cmdbusy_i,          
    output logic [dm::ProgBufSize-1:0][31:0]  progbuf_o,  
    output logic [dm::DataCount-1:0][31:0]    data_o,
    input  logic [dm::DataCount-1:0][31:0]    data_i,
    input  logic                              data_valid_i,
    output logic [BusWidth-1:0]               sbaddress_o,
    input  logic [BusWidth-1:0]               sbaddress_i,
    output logic                              sbaddress_write_valid_o,
    output logic                              sbreadonaddr_o,
    output logic                              sbautoincrement_o,
    output logic [2:0]                        sbaccess_o,
    output logic                              sbreadondata_o,
    output logic [BusWidth-1:0]               sbdata_o,
    output logic                              sbdata_read_valid_o,
    output logic                              sbdata_write_valid_o,
    input  logic [BusWidth-1:0]               sbdata_i,
    input  logic                              sbdata_valid_i,
    input  logic                              sbbusy_i,
    input  logic                              sberror_valid_i,  
    input  logic [2:0]                        sberror_i  
);
    localparam HartSelLen = (NrHarts == 1) ? 1 : $clog2(NrHarts);
    dm::dtm_op_e dtm_op;
    assign dtm_op = dm::dtm_op_e'(dmi_req_i.op);
    logic        resp_queue_full;
    logic        resp_queue_empty;
    logic        resp_queue_push;
    logic        resp_queue_pop;
    logic [31:0] resp_queue_data;
    localparam dm::dm_csr_e DataEnd = dm::dm_csr_e'((dm::Data0 + {4'b0, dm::DataCount}));
    localparam dm::dm_csr_e ProgBufEnd = dm::dm_csr_e'((dm::ProgBuf0 + {4'b0, dm::ProgBufSize}));
    logic [31:0] haltsum0, haltsum1, haltsum2, haltsum3;
    logic [NrHarts/2**5 :0][31:0] halted_reshaped0;
    logic [NrHarts/2**10:0][31:0] halted_reshaped1;
    logic [NrHarts/2**15:0][31:0] halted_reshaped2;
    logic [(NrHarts/2**10+1)*32-1:0] halted_flat1;
    logic [(NrHarts/2**15+1)*32-1:0] halted_flat2;
    logic [32-1:0] halted_flat3;
    assign halted_reshaped0 = halted_i;
    assign haltsum0         = halted_reshaped0[hartsel_o[19:5]];
    always_comb begin : p_reduction1
        halted_flat1 = '0;
        for (int k=0; k<NrHarts/2**5+1; k++) begin
            halted_flat1[k] = |halted_reshaped0[k];
        end
        halted_reshaped1 = halted_flat1;
        haltsum1         = halted_reshaped1[hartsel_o[19:10]];
    end
    always_comb begin : p_reduction2
        halted_flat2 = '0;
        for (int k=0; k<NrHarts/2**10+1; k++) begin
            halted_flat2[k] = |halted_reshaped1[k];
        end
        halted_reshaped2 = halted_flat2;
        haltsum2         = halted_reshaped2[hartsel_o[19:15]];
    end
    always_comb begin : p_reduction3
        halted_flat3 = '0;
        for (int k=0; k<NrHarts/2**15+1; k++) begin
            halted_flat3[k] = |halted_reshaped2[k];
        end
        haltsum3 = halted_flat3;
    end
    dm::dmstatus_t      dmstatus;
    dm::dmcontrol_t     dmcontrol_d, dmcontrol_q;
    dm::abstractcs_t    abstractcs;
    dm::cmderr_e        cmderr_d, cmderr_q;
    dm::command_t       command_d, command_q;
    logic               cmd_valid_d, cmd_valid_q;
    dm::abstractauto_t  abstractauto_d, abstractauto_q;
    dm::sbcs_t          sbcs_d, sbcs_q;
    logic [63:0]        sbaddr_d, sbaddr_q;
    logic [63:0]        sbdata_d, sbdata_q;
    logic [NrHarts-1:0] havereset_d, havereset_q;
    logic [dm::ProgBufSize-1:0][31:0] progbuf_d, progbuf_q;
    logic [({3'b0, dm::DataCount} + dm::Data0 - 1):(dm::Data0)][31:0] data_d, data_q;
    logic [HartSelLen-1:0] selected_hart;
    assign dmi_resp_o.resp = dm::DTM_SUCCESS;
    assign dmi_resp_valid_o     = ~resp_queue_empty;
    assign dmi_req_ready_o      = ~resp_queue_full;
    assign resp_queue_push      = dmi_req_valid_i & dmi_req_ready_o;
    assign sbautoincrement_o = sbcs_q.sbautoincrement;
    assign sbreadonaddr_o    = sbcs_q.sbreadonaddr;
    assign sbreadondata_o    = sbcs_q.sbreadondata;
    assign sbaccess_o        = sbcs_q.sbaccess;
    assign sbdata_o          = sbdata_q[BusWidth-1:0];
    assign sbaddress_o       = sbaddr_q[BusWidth-1:0];
    assign hartsel_o         = {dmcontrol_q.hartselhi, dmcontrol_q.hartsello};
    always_comb begin : csr_read_write
        dmstatus    = '0;
        dmstatus.version = dm::DbgVersion013;
        dmstatus.authenticated = 1'b1;
        dmstatus.hasresethaltreq = 1'b0;
        dmstatus.allhavereset = havereset_q[selected_hart];
        dmstatus.anyhavereset = havereset_q[selected_hart];
        dmstatus.allresumeack = resumeack_i[selected_hart];
        dmstatus.anyresumeack = resumeack_i[selected_hart];
        dmstatus.allunavail   = unavailable_i[selected_hart];
        dmstatus.anyunavail   = unavailable_i[selected_hart];
        dmstatus.allnonexistent = (hartsel_o > (NrHarts[19:0] - 1)) ? 1'b1 : 1'b0;
        dmstatus.anynonexistent = (hartsel_o > (NrHarts[19:0] - 1)) ? 1'b1 : 1'b0;
        dmstatus.allhalted    = halted_i[selected_hart] & ~unavailable_i[selected_hart];
        dmstatus.anyhalted    = halted_i[selected_hart] & ~unavailable_i[selected_hart];
        dmstatus.allrunning   = ~halted_i[selected_hart] & ~unavailable_i[selected_hart];
        dmstatus.anyrunning   = ~halted_i[selected_hart] & ~unavailable_i[selected_hart];
        abstractcs = '0;
        abstractcs.datacount = dm::DataCount;
        abstractcs.progbufsize = dm::ProgBufSize;
        abstractcs.busy = cmdbusy_i;
        abstractcs.cmderr = cmderr_q;
        abstractauto_d = abstractauto_q;
        abstractauto_d.zero0 = '0;
        havereset_d = havereset_q;
        dmcontrol_d = dmcontrol_q;
        cmderr_d    = cmderr_q;
        command_d   = command_q;
        progbuf_d   = progbuf_q;
        data_d      = data_q;
        sbcs_d      = sbcs_q;
        sbaddr_d    = sbaddress_i;
        sbdata_d    = sbdata_q;
        resp_queue_data         = 32'b0;
        cmd_valid_d             = 1'b0;
        sbaddress_write_valid_o = 1'b0;
        sbdata_read_valid_o     = 1'b0;
        sbdata_write_valid_o    = 1'b0;
        clear_resumeack_o       = 1'b0;
        if (dmi_req_ready_o && dmi_req_valid_i && dtm_op == dm::DTM_READ) begin
            unique case ({1'b0, dmi_req_i.addr}) inside
                [(dm::Data0):DataEnd]: begin
                    if (dm::DataCount > 0) begin
                        resp_queue_data = data_q[dmi_req_i.addr[4:0]];
                    end
                    if (!cmdbusy_i) begin
                        cmd_valid_d = abstractauto_q.autoexecdata[dmi_req_i.addr[3:0] -
                                      int'(dm::Data0)];
                    end
                end
                dm::DMControl:    resp_queue_data = dmcontrol_q;
                dm::DMStatus:     resp_queue_data = dmstatus;
                dm::Hartinfo:     resp_queue_data = hartinfo_i[selected_hart];
                dm::AbstractCS:   resp_queue_data = abstractcs;
                dm::AbstractAuto: resp_queue_data = abstractauto_q;
                dm::Command:    resp_queue_data = '0;
                [(dm::ProgBuf0):ProgBufEnd]: begin
                    resp_queue_data = progbuf_q[dmi_req_i.addr[4:0]];
                    if (!cmdbusy_i) begin
                        cmd_valid_d = abstractauto_q.autoexecprogbuf[dmi_req_i.addr[3:0]+16];
                    end
                end
                dm::HaltSum0: resp_queue_data = haltsum0;
                dm::HaltSum1: resp_queue_data = haltsum1;
                dm::HaltSum2: resp_queue_data = haltsum2;
                dm::HaltSum3: resp_queue_data = haltsum3;
                dm::SBCS: begin
                    resp_queue_data = sbcs_q;
                end
                dm::SBAddress0: begin
                    if (sbbusy_i) begin
                       sbcs_d.sbbusyerror = 1'b1;
                    end else begin
                        resp_queue_data = sbaddr_q[31:0];
                    end
                end
                dm::SBAddress1: begin
                    if (sbbusy_i) begin
                       sbcs_d.sbbusyerror = 1'b1;
                    end else begin
                        resp_queue_data = sbaddr_q[63:32];
                    end
                end
                dm::SBData0: begin
                    if (sbbusy_i) begin
                       sbcs_d.sbbusyerror = 1'b1;
                    end else begin
                        sbdata_read_valid_o = (sbcs_q.sberror == '0);
                        resp_queue_data = sbdata_q[31:0];
                    end
                end
                dm::SBData1: begin
                    if (sbbusy_i) begin
                       sbcs_d.sbbusyerror = 1'b1;
                    end else begin
                        resp_queue_data = sbdata_q[63:32];
                    end
                end
                default:;
            endcase
        end
        if (dmi_req_ready_o && dmi_req_valid_i && dtm_op == dm::DTM_WRITE) begin
            unique case (dm::dm_csr_e'({1'b0, dmi_req_i.addr})) inside
                [(dm::Data0):DataEnd]: begin
                    if (!cmdbusy_i && dm::DataCount > 0) begin
                        data_d[dmi_req_i.addr[4:0]] = dmi_req_i.data;
                        cmd_valid_d = abstractauto_q.autoexecdata[dmi_req_i.addr[3:0] -
                                      int'(dm::Data0)];
                    end
                end
                dm::DMControl: begin
                    automatic dm::dmcontrol_t dmcontrol;
                    dmcontrol = dm::dmcontrol_t'(dmi_req_i.data);
                    if (dmcontrol.ackhavereset) begin
                        havereset_d[selected_hart] = 1'b0;
                    end
                    dmcontrol_d = dmi_req_i.data;
                end
                dm::DMStatus:;  
                dm::Hartinfo:;  
                dm::AbstractCS: begin  
                    automatic dm::abstractcs_t a_abstractcs;
                    a_abstractcs = dm::abstractcs_t'(dmi_req_i.data);
                    if (!cmdbusy_i) begin
                        cmderr_d = dm::cmderr_e'(~a_abstractcs.cmderr & cmderr_q);
                    end else if (cmderr_q == dm::CmdErrNone) begin
                        cmderr_d = dm::CmdErrBusy;
                    end
                end
                dm::Command: begin
                    if (!cmdbusy_i) begin
                        cmd_valid_d = 1'b1;
                        command_d = dm::command_t'(dmi_req_i.data);
                    end else if (cmderr_q == dm::CmdErrNone) begin
                        cmderr_d = dm::CmdErrBusy;
                    end
                end
                dm::AbstractAuto: begin
                    if (!cmdbusy_i) begin
                        abstractauto_d                 = 32'b0;
                        abstractauto_d.autoexecdata    = dmi_req_i.data[dm::DataCount-1:0];
                        abstractauto_d.autoexecprogbuf = dmi_req_i.data[dm::ProgBufSize-1+16:16];
                    end else if (cmderr_q == dm::CmdErrNone) begin
                        cmderr_d = dm::CmdErrBusy;
                    end
                end
                [(dm::ProgBuf0):ProgBufEnd]: begin
                    if (!cmdbusy_i) begin
                        progbuf_d[dmi_req_i.addr[4:0]] = dmi_req_i.data;
                        cmd_valid_d = abstractauto_q.autoexecprogbuf[dmi_req_i.addr[3:0]+16];
                    end
                end
                dm::SBCS: begin
                    if (sbbusy_i) begin
                        sbcs_d.sbbusyerror = 1'b1;
                    end else begin
                        automatic dm::sbcs_t sbcs = dm::sbcs_t'(dmi_req_i.data);
                        sbcs_d = sbcs;
                        sbcs_d.sbbusyerror = sbcs_q.sbbusyerror & (~sbcs.sbbusyerror);
                        sbcs_d.sberror     = sbcs_q.sberror     & (~sbcs.sberror);
                    end
                end
                dm::SBAddress0: begin
                    if (sbbusy_i) begin
                       sbcs_d.sbbusyerror = 1'b1;
                    end else begin
                        sbaddr_d[31:0] = dmi_req_i.data;
                        sbaddress_write_valid_o = (sbcs_q.sberror == '0);
                    end
                end
                dm::SBAddress1: begin
                    if (sbbusy_i) begin
                       sbcs_d.sbbusyerror = 1'b1;
                    end else begin
                        sbaddr_d[63:32] = dmi_req_i.data;
                    end
                end
                dm::SBData0: begin
                    if (sbbusy_i) begin
                       sbcs_d.sbbusyerror = 1'b1;
                    end else begin
                        sbdata_d[31:0] = dmi_req_i.data;
                        sbdata_write_valid_o = (sbcs_q.sberror == '0);
                    end
                end
                dm::SBData1: begin
                    if (sbbusy_i) begin
                       sbcs_d.sbbusyerror = 1'b1;
                    end else begin
                        sbdata_d[63:32] = dmi_req_i.data;
                    end
                end
                default:;
            endcase
        end
        if (cmderror_valid_i) begin
            cmderr_d = cmderror_i;
        end
        if (data_valid_i)
            data_d = data_i;
        if (ndmreset_o) begin
            havereset_d = '1;
        end
        if (sberror_valid_i) begin
            sbcs_d.sberror = sberror_i;
        end
        if (sbdata_valid_i) begin
            sbdata_d = sbdata_i;
        end
        dmcontrol_d.hasel           = 1'b0;
        dmcontrol_d.hartreset       = 1'b0;
        dmcontrol_d.setresethaltreq = 1'b0;
        dmcontrol_d.clrresethaltreq = 1'b0;
        dmcontrol_d.zero1           = '0;
        dmcontrol_d.zero0           = '0;
        dmcontrol_d.ackhavereset    = 1'b0;
        if (!dmcontrol_q.resumereq && dmcontrol_d.resumereq) begin
            clear_resumeack_o = 1'b1;
        end
        if (dmcontrol_q.resumereq && resumeack_i) begin
            dmcontrol_d.resumereq = 1'b0;
        end
        sbcs_d.sbversion            = 3'b1;
        sbcs_d.sbbusy               = sbbusy_i;
        sbcs_d.sbasize              = BusWidth;
        sbcs_d.sbaccess128          = 1'b0;
        sbcs_d.sbaccess64           = BusWidth == 64;
        sbcs_d.sbaccess32           = BusWidth == 32;
        sbcs_d.sbaccess16           = 1'b0;
        sbcs_d.sbaccess8            = 1'b0;
        sbcs_d.sbaccess             = BusWidth == 64 ? 2'd3 : 2'd2;
    end
    always_comb begin
        selected_hart = hartsel_o[HartSelLen-1:0];
        haltreq_o = '0;
        resumereq_o = '0;
        haltreq_o[selected_hart] = dmcontrol_q.haltreq;
        resumereq_o[selected_hart] = dmcontrol_q.resumereq;
    end
    assign dmactive_o  = dmcontrol_q.dmactive;
    assign cmd_o       = command_q;
    assign cmd_valid_o = cmd_valid_q;
    assign progbuf_o   = progbuf_q;
    assign data_o      = data_q;
    assign resp_queue_pop = dmi_resp_ready_i & ~resp_queue_empty;
    assign ndmreset_o = dmcontrol_q.ndmreset;
    fifo_v2 #(
        .dtype            ( logic [31:0]         ),
        .DEPTH            ( 2                    )
    ) i_fifo (
        .clk_i            ( clk_i                ),
        .rst_ni           ( dmi_rst_ni           ),  
        .flush_i          ( 1'b0                 ),  
        .testmode_i       ( testmode_i           ),
        .full_o           ( resp_queue_full      ),
        .empty_o          ( resp_queue_empty     ),
        .alm_full_o       (                      ),
        .alm_empty_o      (                      ),
        .data_i           ( resp_queue_data      ),
        .push_i           ( resp_queue_push      ),
        .data_o           ( dmi_resp_o.data      ),
        .pop_i            ( resp_queue_pop       )
    );
    always_ff @(posedge clk_i or negedge rst_ni) begin
        if (!rst_ni) begin
            dmcontrol_q    <= '0;
            cmderr_q       <= dm::CmdErrNone;
            command_q      <= '0;
            abstractauto_q <= '0;
            progbuf_q      <= '0;
            data_q         <= '0;
            sbcs_q         <= '0;
            sbaddr_q       <= '0;
            sbdata_q       <= '0;
        end else begin
            if (!dmcontrol_q.dmactive) begin
                dmcontrol_q.haltreq          <= '0;
                dmcontrol_q.resumereq        <= '0;
                dmcontrol_q.hartreset        <= '0;
                dmcontrol_q.zero1            <= '0;
                dmcontrol_q.hasel            <= '0;
                dmcontrol_q.hartsello        <= '0;
                dmcontrol_q.hartselhi        <= '0;
                dmcontrol_q.zero0            <= '0;
                dmcontrol_q.setresethaltreq  <= '0;
                dmcontrol_q.clrresethaltreq  <= '0;
                dmcontrol_q.ndmreset         <= '0;
                dmcontrol_q.dmactive         <= dmcontrol_d.dmactive;
                cmderr_q                     <= dm::CmdErrNone;
                command_q                    <= '0;
                cmd_valid_q                  <= '0;
                abstractauto_q               <= '0;
                progbuf_q                    <= '0;
                data_q                       <= '0;
                sbcs_q                       <= '0;
                sbaddr_q                     <= '0;
                sbdata_q                     <= '0;
            end else begin
                dmcontrol_q                  <= dmcontrol_d;
                cmderr_q                     <= cmderr_d;
                command_q                    <= command_d;
                cmd_valid_q                  <= cmd_valid_d;
                abstractauto_q               <= abstractauto_d;
                progbuf_q                    <= progbuf_d;
                data_q                       <= data_d;
                sbcs_q                       <= sbcs_d;
                sbaddr_q                     <= sbaddr_d;
                sbdata_q                     <= sbdata_d;
            end
        end
    end
    for (genvar k = 0; k < NrHarts; k++) begin : gen_havereset
        always_ff @(posedge clk_i or negedge rst_ni) begin
            if (!rst_ni) begin
                havereset_q[k] <= 1'b1;
            end else begin
                havereset_q[k] <= SelectableHarts[k] ? havereset_d[k] : 1'b0;
            end
        end
    end
endmodule
module dm_mem #(
    parameter int                 NrHarts          = -1,
    parameter int                 BusWidth         = -1,
    parameter logic [NrHarts-1:0] SelectableHarts  = -1
)(
    input  logic                             clk_i,        
    input  logic                             rst_ni,       
    output logic [NrHarts-1:0]               debug_req_o,
    input  logic [19:0]                      hartsel_i,
    input  logic [NrHarts-1:0]               haltreq_i,
    input  logic [NrHarts-1:0]               resumereq_i,
    input  logic                             clear_resumeack_i,
    output logic [NrHarts-1:0]               halted_o,     
    output logic [NrHarts-1:0]               resuming_o,   
    input  logic [dm::ProgBufSize-1:0][31:0] progbuf_i,     
    input  logic [dm::DataCount-1:0][31:0]   data_i,        
    output logic [dm::DataCount-1:0][31:0]   data_o,        
    output logic                             data_valid_o,  
    input  logic                             cmd_valid_i,
    input  dm::command_t                     cmd_i,
    output logic                             cmderror_valid_o,
    output dm::cmderr_e                      cmderror_o,
    output logic                             cmdbusy_o,
    input  logic                             req_i,
    input  logic                             we_i,
    input  logic [BusWidth-1:0]              addr_i,
    input  logic [BusWidth-1:0]              wdata_i,
    input  logic [BusWidth/8-1:0]            be_i,
    output logic [BusWidth-1:0]              rdata_o
);
    localparam int HartSelLen = (NrHarts == 1) ? 1 : $clog2(NrHarts);
    localparam int MaxAar = (BusWidth == 64) ? 4 : 3;
    localparam DbgAddressBits = 12;
    localparam logic [DbgAddressBits-1:0] DataBase = (dm::DataAddr);
    localparam logic [DbgAddressBits-1:0] DataEnd = (dm::DataAddr + 4*dm::DataCount);
    localparam logic [DbgAddressBits-1:0] ProgBufBase = (dm::DataAddr - 4*dm::ProgBufSize);
    localparam logic [DbgAddressBits-1:0] ProgBufEnd = (dm::DataAddr - 1);
    localparam logic [DbgAddressBits-1:0] AbstractCmdBase = (ProgBufBase - 4*10);
    localparam logic [DbgAddressBits-1:0] AbstractCmdEnd = (ProgBufBase - 1);
    localparam logic [DbgAddressBits-1:0] WhereTo   = 'h300;
    localparam logic [DbgAddressBits-1:0] FlagsBase = 'h400;
    localparam logic [DbgAddressBits-1:0] FlagsEnd  = 'h7FF;
    localparam logic [DbgAddressBits-1:0] Halted    = 'h100;
    localparam logic [DbgAddressBits-1:0] Going     = 'h104;
    localparam logic [DbgAddressBits-1:0] Resuming  = 'h108;
    localparam logic [DbgAddressBits-1:0] Exception = 'h10C;
    logic [dm::ProgBufSize/2-1:0][63:0]   progbuf;
    logic [4:0][63:0]   abstract_cmd;
    logic [NrHarts-1:0] halted_d, halted_q;
    logic [NrHarts-1:0] resuming_d, resuming_q;
    logic               resume, go, going;
    logic [NrHarts-1:0] halted;
    logic [HartSelLen-1:0] hart_sel;
    logic exception;
    logic unsupported_command;
    logic [63:0] rom_rdata;
    logic [63:0] rdata_d, rdata_q;
    logic        word_enable32_q;
    logic fwd_rom_d, fwd_rom_q;
    dm::ac_ar_cmd_t ac_ar;
    assign ac_ar       = dm::ac_ar_cmd_t'(cmd_i.control);
    assign hart_sel    = wdata_i[HartSelLen-1:0];
    assign debug_req_o = haltreq_i;
    assign halted_o    = halted_q;
    assign resuming_o  = resuming_q;
    assign progbuf = progbuf_i;
    typedef enum logic [1:0] { Idle, Go, Resume, CmdExecuting } state_e;
    state_e state_d, state_q;
    always_comb begin
        cmderror_valid_o = 1'b0;
        cmderror_o       = dm::CmdErrNone;
        state_d          = state_q;
        go               = 1'b0;
        resume           = 1'b0;
        cmdbusy_o        = 1'b1;
        case (state_q)
            Idle: begin
                cmdbusy_o = 1'b0;
                if (cmd_valid_i && halted_q[hartsel_i]) begin
                    state_d = Go;
                end else if (cmd_valid_i) begin
                    cmderror_valid_o = 1'b1;
                    cmderror_o = dm::CmdErrorHaltResume;
                end
                if (resumereq_i[hartsel_i] && !resuming_q[hartsel_i] &&
                     !haltreq_i[hartsel_i] &&    halted_q[hartsel_i]) begin
                    state_d = Resume;
                end
            end
            Go: begin
                cmdbusy_o = 1'b1;
                go        = 1'b1;
                if (going)
                    state_d = CmdExecuting;
            end
            Resume: begin
                cmdbusy_o = 1'b1;
                resume = 1'b1;
                if (resuming_o[hartsel_i])
                    state_d = Idle;
            end
            CmdExecuting: begin
                cmdbusy_o = 1'b1;
                go        = 1'b0;
                if (halted[hartsel_i]) begin
                    state_d = Idle;
                end
            end
        endcase
        if (unsupported_command && cmd_valid_i) begin
            cmderror_valid_o = 1'b1;
            cmderror_o = dm::CmdErrNotSupported;
        end
        if (exception) begin
            cmderror_valid_o = 1'b1;
            cmderror_o = dm::CmdErrorException;
        end
    end
    always_comb begin
        automatic logic [63:0] data_bits;
        halted_d     = halted_q;
        resuming_d   = resuming_q;
        rdata_o      = (BusWidth == 64) ?
                          (fwd_rom_q ? rom_rdata : rdata_q) :
                          (word_enable32_q ?
                              (fwd_rom_q ? rom_rdata[63:32] : rdata_q[63:32]) :
                              (fwd_rom_q ? rom_rdata[31: 0] : rdata_q[31: 0]));
        rdata_d      = rdata_q;
        data_bits    = data_i;
        data_valid_o = 1'b0;
        exception    = 1'b0;
        halted       = '0;
        going        = 1'b0;
        if (clear_resumeack_i) begin
            resuming_d[hartsel_i] = 1'b0;
        end
        if (req_i) begin
            if (we_i) begin
                unique case (addr_i[DbgAddressBits-1:0]) inside
                    Halted: begin
                        halted[hart_sel] = 1'b1;
                        halted_d[hart_sel] = 1'b1;
                    end
                    Going: begin
                        going = 1'b1;
                    end
                    Resuming: begin
                        halted_d[hart_sel] = 1'b0;
                        resuming_d[hart_sel] = 1'b1;
                    end
                    Exception: exception = 1'b1;
                    [(dm::DataAddr):DataEnd]: begin
                        data_valid_o = 1'b1;
                        for (int i = 0; i < $bits(be_i); i++) begin
                            if (be_i[i]) begin
                                data_bits[i*8+:8] = wdata_i[i*8+:8];
                            end
                        end
                    end
                    default ;
                endcase
            end else begin
                unique case (addr_i[DbgAddressBits-1:0]) inside
                    WhereTo: begin
                        if (resumereq_i[hart_sel]) begin
                            rdata_d = {32'b0, dm::jal('0, dm::ResumeAddress[11:0]-WhereTo)};
                        end
                        if (cmdbusy_o) begin
                            if (cmd_i.cmdtype == dm::AccessRegister &&
                                !ac_ar.transfer && ac_ar.postexec) begin
                                rdata_d = {32'b0, dm::jal('0, ProgBufBase-WhereTo)};
                            end else begin
                                rdata_d = {32'b0, dm::jal('0, AbstractCmdBase-WhereTo)};
                            end
                        end
                    end
                    [DataBase:DataEnd]: begin
                        rdata_d = {
                                  data_i[(addr_i[DbgAddressBits-1:3] - DataBase[DbgAddressBits-1:3] + 1)],
                                  data_i[(addr_i[DbgAddressBits-1:3] - DataBase[DbgAddressBits-1:3])]
                                  };
                    end
                    [ProgBufBase:ProgBufEnd]: begin
                        rdata_d = progbuf[(addr_i[DbgAddressBits-1:3] -
                                      ProgBufBase[DbgAddressBits-1:3])];
                    end
                    [AbstractCmdBase:AbstractCmdEnd]: begin
                        rdata_d = abstract_cmd[(addr_i[DbgAddressBits-1:3] -
                                       AbstractCmdBase[DbgAddressBits-1:3])];
                    end
                    [FlagsBase:FlagsEnd]: begin
                        automatic logic [7:0][7:0] rdata;
                        rdata = '0;
                        if (({addr_i[DbgAddressBits-1:3], 3'b0} - FlagsBase[DbgAddressBits-1:0]) ==
                          {hartsel_i[DbgAddressBits-1:3], 3'b0}) begin
                            rdata[hartsel_i[2:0]] = {6'b0, resume, go};
                        end
                        rdata_d = rdata;
                    end
                    default: ;
                endcase
            end
        end
        data_o = data_bits;
    end
    always_comb begin : abstract_cmd_rom
        unsupported_command = 1'b0;
        abstract_cmd[0][31:0]  = dm::illegal();
        abstract_cmd[0][63:32] = dm::auipc(5'd10, '0);
        abstract_cmd[1][31:0]  = dm::srli(5'd10, 5'd10, 6'd12);  
        abstract_cmd[1][63:32] = dm::slli(5'd10, 5'd10, 6'd12);
        abstract_cmd[2][31:0]  = dm::nop();
        abstract_cmd[2][63:32] = dm::nop();
        abstract_cmd[3][31:0]  = dm::nop();
        abstract_cmd[3][63:32] = dm::nop();
        abstract_cmd[4][31:0]  = dm::csrr(dm::CSR_DSCRATCH1, 5'd10);
        abstract_cmd[4][63:32] = dm::ebreak();
        unique case (cmd_i.cmdtype)
            dm::AccessRegister: begin
                if (ac_ar.aarsize < MaxAar && ac_ar.transfer && ac_ar.write) begin
                    abstract_cmd[0][31:0] = dm::csrw(dm::CSR_DSCRATCH1, 5'd10);
                    if (ac_ar.regno[15:14] != '0) begin
                        abstract_cmd[0][31:0] = dm::ebreak();  
                        unsupported_command = 1'b1;
                    end else if (ac_ar.regno[12] && (!ac_ar.regno[5]) &&
                                (ac_ar.regno[4:0] == 5'd10)) begin
                        abstract_cmd[2][31:0]  = dm::csrw(dm::CSR_DSCRATCH0, 5'd8);
                        abstract_cmd[2][63:32] = dm::load(ac_ar.aarsize, 5'd8, 5'd10, dm::DataAddr);
                        abstract_cmd[3][31:0]  = dm::csrw(dm::CSR_DSCRATCH1, 5'd8);
                        abstract_cmd[3][63:32] = dm::csrr(dm::CSR_DSCRATCH0, 5'd8);
                    end else if (ac_ar.regno[12]) begin
                        if (ac_ar.regno[5]) begin
                            abstract_cmd[2][31:0] =
                                dm::float_load(ac_ar.aarsize, ac_ar.regno[4:0], 5'd10, dm::DataAddr);
                        end else begin
                            abstract_cmd[2][31:0] =
                                dm::load(ac_ar.aarsize, ac_ar.regno[4:0], 5'd10, dm::DataAddr);
                        end
                    end else begin
                        abstract_cmd[2][31:0]  = dm::csrw(dm::CSR_DSCRATCH0, 5'd8);
                        abstract_cmd[2][63:32] = dm::load(ac_ar.aarsize, 5'd8, 5'd10, dm::DataAddr);
                        abstract_cmd[3][31:0]  = dm::csrw(dm::csr_reg_t'(ac_ar.regno[11:0]), 5'd8);
                        abstract_cmd[3][63:32] = dm::csrr(dm::CSR_DSCRATCH0, 5'd8);
                    end
                end else if (ac_ar.aarsize < MaxAar && ac_ar.transfer && !ac_ar.write) begin
                    abstract_cmd[0][31:0]  = dm::csrw(dm::CSR_DSCRATCH1, 5'd10);
                    if (ac_ar.regno[15:14] != '0) begin
                        abstract_cmd[0][31:0] = dm::ebreak();  
                        unsupported_command = 1'b1;
                    end else if (ac_ar.regno[12] && (!ac_ar.regno[5]) &&
                                (ac_ar.regno[4:0] == 5'd10)) begin
                        abstract_cmd[2][31:0]  = dm::csrw(dm::CSR_DSCRATCH0, 5'd8);
                        abstract_cmd[2][63:32] = dm::csrr(dm::CSR_DSCRATCH1, 5'd8);
                        abstract_cmd[3][31:0]  = dm::store(ac_ar.aarsize, 5'd8, 5'd10, dm::DataAddr);
                        abstract_cmd[3][63:32] = dm::csrr(dm::CSR_DSCRATCH0, 5'd8);
                    end else if (ac_ar.regno[12]) begin
                        if (ac_ar.regno[5]) begin
                            abstract_cmd[2][31:0] =
                                dm::float_store(ac_ar.aarsize, ac_ar.regno[4:0], 5'd10, dm::DataAddr);
                        end else begin
                            abstract_cmd[2][31:0] =
                                dm::store(ac_ar.aarsize, ac_ar.regno[4:0], 5'd10, dm::DataAddr);
                        end
                    end else begin
                        abstract_cmd[2][31:0]  = dm::csrw(dm::CSR_DSCRATCH0, 5'd8);
                        abstract_cmd[2][63:32] = dm::csrr(dm::csr_reg_t'(ac_ar.regno[11:0]), 5'd8);
                        abstract_cmd[3][31:0]  = dm::store(ac_ar.aarsize, 5'd8, 5'd10, dm::DataAddr);
                        abstract_cmd[3][63:32] = dm::csrr(dm::CSR_DSCRATCH0, 5'd8);
                    end
                end else if (ac_ar.aarsize >= MaxAar || ac_ar.aarpostincrement == 1'b1) begin
                    abstract_cmd[0][31:0] = dm::ebreak();  
                    unsupported_command = 1'b1;
                end
                if (ac_ar.postexec && !unsupported_command) begin
                    abstract_cmd[4][63:32] = dm::nop();
                end
            end
            default: begin
                abstract_cmd[0][31:0] = dm::ebreak();
                unsupported_command = 1'b1;
            end
        endcase
    end
    logic [63:0] rom_addr;
    assign rom_addr = addr_i;
    debug_rom i_debug_rom (
        .clk_i,
        .req_i,
        .addr_i  ( rom_addr  ),
        .rdata_o ( rom_rdata )
    );
    assign fwd_rom_d = (addr_i[DbgAddressBits-1:0] >= dm::HaltAddress[DbgAddressBits-1:0]) ?
                           1'b1 : 1'b0;
    always_ff @(posedge clk_i or negedge rst_ni) begin
        if (!rst_ni) begin
            fwd_rom_q       <= 1'b0;
            rdata_q         <= '0;
            state_q         <= Idle;
            word_enable32_q <= 1'b0;
        end else begin
            fwd_rom_q       <= fwd_rom_d;
            rdata_q         <= rdata_d;
            state_q         <= state_d;
            word_enable32_q <= addr_i[2];
        end
    end
    for (genvar k = 0; k < NrHarts; k++) begin : gen_halted
        always_ff @(posedge clk_i or negedge rst_ni) begin
            if (!rst_ni) begin
                halted_q[k]   <= 1'b0;
                resuming_q[k] <= 1'b0;
            end else begin
                halted_q[k]   <= SelectableHarts[k] ? halted_d[k]   : 1'b0;
                resuming_q[k] <= SelectableHarts[k] ? resuming_d[k] : 1'b0;
            end
        end
    end
endmodule
module dm_sba #(
    parameter int BusWidth = -1
) (
    input  logic                   clk_i,        
    input  logic                   rst_ni,
    input  logic                   dmactive_i,   
    output logic                   master_req_o,
    output logic [BusWidth-1:0]    master_add_o,
    output logic                   master_we_o,
    output logic [BusWidth-1:0]    master_wdata_o,
    output logic [BusWidth/8-1:0]  master_be_o,
    input  logic                   master_gnt_i,
    input  logic                   master_r_valid_i,
    input  logic [BusWidth-1:0]    master_r_rdata_i,
    input  logic [BusWidth-1:0]    sbaddress_i,
    input  logic                   sbaddress_write_valid_i,
    input  logic                   sbreadonaddr_i,
    output logic [BusWidth-1:0]    sbaddress_o,
    input  logic                   sbautoincrement_i,
    input  logic [2:0]             sbaccess_i,
    input  logic                   sbreadondata_i,
    input  logic [BusWidth-1:0]    sbdata_i,
    input  logic                   sbdata_read_valid_i,
    input  logic                   sbdata_write_valid_i,
    output logic [BusWidth-1:0]    sbdata_o,
    output logic                   sbdata_valid_o,
    output logic                   sbbusy_o,
    output logic                   sberror_valid_o,  
    output logic [2:0]             sberror_o  
);
    typedef enum logic [2:0] { Idle, Read, Write, WaitRead, WaitWrite } state_e;
    state_e state_d, state_q;
    logic [BusWidth-1:0]   address;
    logic                  req;
    logic                  gnt;
    logic                  we;
    logic [BusWidth/8-1:0] be;
    assign sbbusy_o = (state_q != Idle) ? 1'b1 : 1'b0;
    always_comb begin
        req     = 1'b0;
        address = sbaddress_i;
        we      = 1'b0;
        be      = '0;
        sberror_o       = '0;
        sberror_valid_o = 1'b0;
        sbaddress_o     = sbaddress_i;
        state_d = state_q;
        case (state_q)
            Idle: begin
                if (sbaddress_write_valid_i && sbreadonaddr_i)  state_d = Read;
                if (sbdata_write_valid_i) state_d = Write;
                if (sbdata_read_valid_i && sbreadondata_i) state_d = Read;
            end
            Read: begin
                req = 1'b1;
                if (gnt) state_d = WaitRead;
            end
            Write: begin
                req = 1'b1;
                we  = 1'b1;
                case (sbaccess_i)
                    3'b000: begin
                        if (BusWidth == 64) be[ sbaddress_i[2:0]] = '1;
                        else                be[ sbaddress_i[1:0]] = '1;
                    end
                    3'b001: begin
                        if (BusWidth == 64) be[{sbaddress_i[2:1], 1'b0} +: 2] = '1;
                        else                be[{sbaddress_i[1:1], 1'b0} +: 2] = '1;
                    end
                    3'b010: begin
                        if (BusWidth == 64) be[{sbaddress_i[2:2], 2'b0} +: 4] = '1;
                        else                be = '1;
                    end
                    3'b011: be = '1;
                    default:;
                endcase
                if (gnt) state_d = WaitWrite;
            end
            WaitRead: begin
                if (sbdata_valid_o) begin
                    state_d = Idle;
                    if (sbautoincrement_i) sbaddress_o = sbaddress_i + (1'b1 << sbaccess_i);
                end
            end
            WaitWrite: begin
                if (sbdata_valid_o) begin
                    state_d = Idle;
                    if (sbautoincrement_i) sbaddress_o = sbaddress_i + (1'b1 << sbaccess_i);
                end
            end
            default:;
        endcase
        if (sbaccess_i > 3 && state_q != Idle) begin
            req             = 1'b0;
            state_d         = Idle;
            sberror_valid_o = 1'b1;
            sberror_o       = 3'd3;
        end
    end
    always_ff @(posedge clk_i or negedge rst_ni) begin
        if (!rst_ni) begin
            state_q <= Idle;
        end else begin
            state_q <= state_d;
        end
    end
    assign master_req_o    = req;
    assign master_add_o    = address[BusWidth-1:0];
    assign master_we_o     = we;
    assign master_wdata_o  = sbdata_i[BusWidth-1:0];
    assign master_be_o     = be[BusWidth/8-1:0];
    assign gnt             = master_gnt_i;
    assign sbdata_valid_o  = master_r_valid_i;
    assign sbdata_o        = master_r_rdata_i[BusWidth-1:0];
endmodule
module dm_top #(
    parameter int                 NrHarts          = 1,
    parameter int                 BusWidth         = 32,
    parameter logic [NrHarts-1:0] SelectableHarts  = 1   
) (
    input  logic                  clk_i,        
    input  logic                  rst_ni,       
    input  logic                  testmode_i,
    output logic                  ndmreset_o,   
    output logic                  dmactive_o,   
    output logic [NrHarts-1:0]    debug_req_o,  
    input  logic [NrHarts-1:0]    unavailable_i,  
    dm::hartinfo_t [NrHarts-1:0]  hartinfo_i,
    input  logic                  slave_req_i,
    input  logic                  slave_we_i,
    input  logic [BusWidth-1:0]   slave_addr_i,
    input  logic [BusWidth/8-1:0] slave_be_i,
    input  logic [BusWidth-1:0]   slave_wdata_i,
    output logic [BusWidth-1:0]   slave_rdata_o,
    output logic                  master_req_o,
    output logic [BusWidth-1:0]   master_add_o,
    output logic                  master_we_o,
    output logic [BusWidth-1:0]   master_wdata_o,
    output logic [BusWidth/8-1:0] master_be_o,
    input  logic                  master_gnt_i,
    input  logic                  master_r_valid_i,
    input  logic [BusWidth-1:0]   master_r_rdata_i,
    input  logic                  dmi_rst_ni,
    input  logic                  dmi_req_valid_i,
    output logic                  dmi_req_ready_o,
    input  dm::dmi_req_t          dmi_req_i,
    output logic                  dmi_resp_valid_o,
    input  logic                  dmi_resp_ready_i,
    output dm::dmi_resp_t         dmi_resp_o
);
    logic [NrHarts-1:0]               halted;
    logic [NrHarts-1:0]               resumeack;
    logic [NrHarts-1:0]               haltreq;
    logic [NrHarts-1:0]               resumereq;
    logic                             clear_resumeack;
    logic                             cmd_valid;
    dm::command_t                     cmd;
    logic                             cmderror_valid;
    dm::cmderr_e                      cmderror;
    logic                             cmdbusy;
    logic [dm::ProgBufSize-1:0][31:0] progbuf;
    logic [dm::DataCount-1:0][31:0]   data_csrs_mem;
    logic [dm::DataCount-1:0][31:0]   data_mem_csrs;
    logic                             data_valid;
    logic [19:0]                      hartsel;
    logic [BusWidth-1:0]              sbaddress_csrs_sba;
    logic [BusWidth-1:0]              sbaddress_sba_csrs;
    logic                             sbaddress_write_valid;
    logic                             sbreadonaddr;
    logic                             sbautoincrement;
    logic [2:0]                       sbaccess;
    logic                             sbreadondata;
    logic [BusWidth-1:0]              sbdata_write;
    logic                             sbdata_read_valid;
    logic                             sbdata_write_valid;
    logic [BusWidth-1:0]              sbdata_read;
    logic                             sbdata_valid;
    logic                             sbbusy;
    logic                             sberror_valid;
    logic [2:0]                       sberror;
    dm_csrs #(
        .NrHarts(NrHarts),
        .BusWidth(BusWidth),
        .SelectableHarts(SelectableHarts)
    ) i_dm_csrs (
        .clk_i                   ( clk_i                 ),
        .rst_ni                  ( rst_ni                ),
        .testmode_i              ( testmode_i            ),
        .dmi_rst_ni,
        .dmi_req_valid_i,
        .dmi_req_ready_o,
        .dmi_req_i,
        .dmi_resp_valid_o,
        .dmi_resp_ready_i,
        .dmi_resp_o,
        .ndmreset_o              ( ndmreset_o            ),
        .dmactive_o              ( dmactive_o            ),
        .hartsel_o               ( hartsel               ),
        .hartinfo_i              ( hartinfo_i            ),
        .halted_i                ( halted                ),
        .unavailable_i,
        .resumeack_i             ( resumeack             ),
        .haltreq_o               ( haltreq               ),
        .resumereq_o             ( resumereq             ),
        .clear_resumeack_o       ( clear_resumeack       ),
        .cmd_valid_o             ( cmd_valid             ),
        .cmd_o                   ( cmd                   ),
        .cmderror_valid_i        ( cmderror_valid        ),
        .cmderror_i              ( cmderror              ),
        .cmdbusy_i               ( cmdbusy               ),
        .progbuf_o               ( progbuf               ),
        .data_i                  ( data_mem_csrs         ),
        .data_valid_i            ( data_valid            ),
        .data_o                  ( data_csrs_mem         ),
        .sbaddress_o             ( sbaddress_csrs_sba    ),
        .sbaddress_i             ( sbaddress_sba_csrs    ),
        .sbaddress_write_valid_o ( sbaddress_write_valid ),
        .sbreadonaddr_o          ( sbreadonaddr          ),
        .sbautoincrement_o       ( sbautoincrement       ),
        .sbaccess_o              ( sbaccess              ),
        .sbreadondata_o          ( sbreadondata          ),
        .sbdata_o                ( sbdata_write          ),
        .sbdata_read_valid_o     ( sbdata_read_valid     ),
        .sbdata_write_valid_o    ( sbdata_write_valid    ),
        .sbdata_i                ( sbdata_read           ),
        .sbdata_valid_i          ( sbdata_valid          ),
        .sbbusy_i                ( sbbusy                ),
        .sberror_valid_i         ( sberror_valid         ),
        .sberror_i               ( sberror               )
    );
    dm_sba #(
        .BusWidth(BusWidth)
    ) i_dm_sba (
        .clk_i                   ( clk_i                 ),
        .rst_ni                  ( rst_ni                ),
        .dmactive_i              ( dmactive_o            ),
        .master_req_o            ( master_req_o          ),
        .master_add_o            ( master_add_o          ),
        .master_we_o             ( master_we_o           ),
        .master_wdata_o          ( master_wdata_o        ),
        .master_be_o             ( master_be_o           ),
        .master_gnt_i            ( master_gnt_i          ),
        .master_r_valid_i        ( master_r_valid_i      ),
        .master_r_rdata_i        ( master_r_rdata_i      ),
        .sbaddress_i             ( sbaddress_csrs_sba    ),
        .sbaddress_o             ( sbaddress_sba_csrs    ),
        .sbaddress_write_valid_i ( sbaddress_write_valid ),
        .sbreadonaddr_i          ( sbreadonaddr          ),
        .sbautoincrement_i       ( sbautoincrement       ),
        .sbaccess_i              ( sbaccess              ),
        .sbreadondata_i          ( sbreadondata          ),
        .sbdata_i                ( sbdata_write          ),
        .sbdata_read_valid_i     ( sbdata_read_valid     ),
        .sbdata_write_valid_i    ( sbdata_write_valid    ),
        .sbdata_o                ( sbdata_read           ),
        .sbdata_valid_o          ( sbdata_valid          ),
        .sbbusy_o                ( sbbusy                ),
        .sberror_valid_o         ( sberror_valid         ),
        .sberror_o               ( sberror               )
    );
    dm_mem #(
        .NrHarts(NrHarts),
        .BusWidth(BusWidth),
        .SelectableHarts(SelectableHarts)
    ) i_dm_mem (
        .clk_i                   ( clk_i                 ),
        .rst_ni                  ( rst_ni                ),
        .debug_req_o             ( debug_req_o           ),
        .hartsel_i               ( hartsel               ),
        .haltreq_i               ( haltreq               ),
        .resumereq_i             ( resumereq             ),
        .clear_resumeack_i       ( clear_resumeack       ),
        .halted_o                ( halted                ),
        .resuming_o              ( resumeack             ),
        .cmd_valid_i             ( cmd_valid             ),
        .cmd_i                   ( cmd                   ),
        .cmderror_valid_o        ( cmderror_valid        ),
        .cmderror_o              ( cmderror              ),
        .cmdbusy_o               ( cmdbusy               ),
        .progbuf_i               ( progbuf               ),
        .data_i                  ( data_csrs_mem         ),
        .data_o                  ( data_mem_csrs         ),
        .data_valid_o            ( data_valid            ),
        .req_i                   ( slave_req_i           ),
        .we_i                    ( slave_we_i            ),
        .addr_i                  ( slave_addr_i          ),
        .wdata_i                 ( slave_wdata_i         ),
        .be_i                    ( slave_be_i            ),
        .rdata_o                 ( slave_rdata_o         )
    );
endmodule
module debug_rom (
   input  logic         clk_i,
   input  logic         req_i,
   input  logic [63:0]  addr_i,
   output logic [63:0]  rdata_o
);
    localparam int RomSize = 19;
    const logic [RomSize-1:0][63:0] mem = {
        64'h00000000_7b200073,
        64'h7b302573_7b202473,
        64'h10852423_f1402473,
        64'ha85ff06f_7b302573,
        64'h7b202473_10052223,
        64'h00100073_7b302573,
        64'h10052623_00c51513,
        64'h00c55513_00000517,
        64'h7b351073_fd5ff06f,
        64'hfa041ce3_00247413,
        64'h40044403_00a40433,
        64'hf1402473_02041c63,
        64'h00147413_40044403,
        64'h00a40433_10852023,
        64'hf1402473_00c51513,
        64'h00c55513_00000517,
        64'h7b351073_7b241073,
        64'h0ff0000f_04c0006f,
        64'h07c0006f_00c0006f
    };
    logic [$clog2(RomSize)-1:0] addr_q;
    always_ff @(posedge clk_i) begin
        if (req_i) begin
            addr_q <= addr_i[$clog2(RomSize)-1+3:3];
        end
    end
    assign rdata_o = (addr_q < RomSize) ? mem[addr_q] : '0;
endmodule
module apb_to_reg (
  input  logic          clk_i,
  input  logic          rst_ni,
  input  logic          penable_i,
  input  logic          pwrite_i,
  input  logic [31:0]   paddr_i,
  input  logic          psel_i,
  input  logic [31:0]   pwdata_i,
  output logic [31:0]   prdata_o,
  output logic          pready_o,
  output logic          pslverr_o,
  REG_BUS.out  reg_o
);
  always_comb begin
    reg_o.addr = paddr_i;
    reg_o.write = pwrite_i;
    reg_o.wdata = pwdata_i;
    reg_o.wstrb = '1;
    reg_o.valid = psel_i & penable_i;
    pready_o = reg_o.ready;
    pslverr_o = reg_o.error;
    prdata_o = reg_o.rdata;
  end
endmodule
import axi_pkg::*;
module rstgen_bypass #(
    parameter NumRegs = 4
) (
    input  logic clk_i,
    input  logic rst_ni,
    input  logic rst_test_mode_ni,
    input  logic test_mode_i,
    output logic rst_no,
    output logic init_no
);
    logic rst_n;
    logic [NumRegs-1:0] synch_regs_q;
    always_comb begin
        if (test_mode_i == 1'b0) begin
            rst_n   = rst_ni;
            rst_no  = synch_regs_q[NumRegs-1];
            init_no = synch_regs_q[NumRegs-1];
        end else begin
            rst_n   = rst_test_mode_ni;
            rst_no  = rst_test_mode_ni;
            init_no = 1'b1;
        end
    end
    always @(posedge clk_i or negedge rst_n) begin
        if (~rst_n) begin
            synch_regs_q <= 0;
        end else begin
            synch_regs_q <= {synch_regs_q[NumRegs-2:0], 1'b1};
        end
    end
    initial begin : p_assertions
        if (NumRegs < 1) $fatal(1, "At least one register is required.");
    end
endmodule
module rstgen (
    input  logic clk_i,
    input  logic rst_ni,
    input  logic test_mode_i,
    output logic rst_no,
    output logic init_no
);
    rstgen_bypass i_rstgen_bypass (
        .clk_i            ( clk_i       ),
        .rst_ni           ( rst_ni      ),
        .rst_test_mode_ni ( rst_ni      ),
        .test_mode_i      ( test_mode_i ),
        .rst_no           ( rst_no      ),
        .init_no          ( init_no     )
    );
endmodule
module axi_master_connect (
    input  ariane_axi::req_t    axi_req_i,
    output ariane_axi::resp_t   axi_resp_o,
    AXI_BUS.Master master
);
    assign master.aw_id         = axi_req_i.aw.id;
    assign master.aw_addr       = axi_req_i.aw.addr;
    assign master.aw_len        = axi_req_i.aw.len;
    assign master.aw_size       = axi_req_i.aw.size;
    assign master.aw_burst      = axi_req_i.aw.burst;
    assign master.aw_lock       = axi_req_i.aw.lock;
    assign master.aw_cache      = axi_req_i.aw.cache;
    assign master.aw_prot       = axi_req_i.aw.prot;
    assign master.aw_qos        = axi_req_i.aw.qos;
    assign master.aw_atop       = axi_req_i.aw.atop;
    assign master.aw_region     = axi_req_i.aw.region;
    assign master.aw_user       = '0;
    assign master.aw_valid      = axi_req_i.aw_valid;
    assign axi_resp_o.aw_ready  = master.aw_ready;
    assign master.w_data        = axi_req_i.w.data;
    assign master.w_strb        = axi_req_i.w.strb;
    assign master.w_last        = axi_req_i.w.last;
    assign master.w_user        = '0;
    assign master.w_valid       = axi_req_i.w_valid;
    assign axi_resp_o.w_ready   = master.w_ready;
    assign axi_resp_o.b.id      = master.b_id;
    assign axi_resp_o.b.resp    = master.b_resp;
    assign axi_resp_o.b_valid   = master.b_valid;
    assign master.b_ready       = axi_req_i.b_ready;
    assign master.ar_id         = axi_req_i.ar.id;
    assign master.ar_addr       = axi_req_i.ar.addr;
    assign master.ar_len        = axi_req_i.ar.len;
    assign master.ar_size       = axi_req_i.ar.size;
    assign master.ar_burst      = axi_req_i.ar.burst;
    assign master.ar_lock       = axi_req_i.ar.lock;
    assign master.ar_cache      = axi_req_i.ar.cache;
    assign master.ar_prot       = axi_req_i.ar.prot;
    assign master.ar_qos        = axi_req_i.ar.qos;
    assign master.ar_region     = axi_req_i.ar.region;
    assign master.ar_user       = '0;
    assign master.ar_valid      = axi_req_i.ar_valid;
    assign axi_resp_o.ar_ready  = master.ar_ready;
    assign axi_resp_o.r.id      = master.r_id;
    assign axi_resp_o.r.data    = master.r_data;
    assign axi_resp_o.r.resp    = master.r_resp;
    assign axi_resp_o.r.last    = master.r_last;
    assign axi_resp_o.r_valid   = master.r_valid;
    assign master.r_ready       = axi_req_i.r_ready;
endmodule
module axi_slave_connect (
    output ariane_axi::req_t    axi_req_o,
    input  ariane_axi::resp_t   axi_resp_i,
    AXI_BUS.Slave slave
);
    assign  axi_req_o.aw.id      = slave.aw_id;
    assign  axi_req_o.aw.addr    = slave.aw_addr;
    assign  axi_req_o.aw.len     = slave.aw_len;
    assign  axi_req_o.aw.size    = slave.aw_size;
    assign  axi_req_o.aw.burst   = slave.aw_burst;
    assign  axi_req_o.aw.lock    = slave.aw_lock;
    assign  axi_req_o.aw.cache   = slave.aw_cache;
    assign  axi_req_o.aw.prot    = slave.aw_prot;
    assign  axi_req_o.aw.qos     = slave.aw_qos;
    assign  axi_req_o.aw.atop    = slave.aw_atop;
    assign  axi_req_o.aw.region  = slave.aw_region;
    assign  axi_req_o.aw_valid   = slave.aw_valid;
    assign  slave.aw_ready       = axi_resp_i.aw_ready;
    assign  axi_req_o.w.data     = slave.w_data;
    assign  axi_req_o.w.strb     = slave.w_strb;
    assign  axi_req_o.w.last     = slave.w_last;
    assign  axi_req_o.w_valid    = slave.w_valid;
    assign  slave.w_ready        = axi_resp_i.w_ready;
    assign  slave.b_id           = axi_resp_i.b.id;
    assign  slave.b_resp         = axi_resp_i.b.resp;
    assign  slave.b_valid        = axi_resp_i.b_valid;
    assign  slave.b_user         = 1'b0;
    assign  axi_req_o.b_ready    = slave.b_ready;
    assign  axi_req_o.ar.id      = slave.ar_id;
    assign  axi_req_o.ar.addr    = slave.ar_addr;
    assign  axi_req_o.ar.len     = slave.ar_len;
    assign  axi_req_o.ar.size    = slave.ar_size;
    assign  axi_req_o.ar.burst   = slave.ar_burst;
    assign  axi_req_o.ar.lock    = slave.ar_lock;
    assign  axi_req_o.ar.cache   = slave.ar_cache;
    assign  axi_req_o.ar.prot    = slave.ar_prot;
    assign  axi_req_o.ar.qos     = slave.ar_qos;
    assign  axi_req_o.ar.region  = slave.ar_region;
    assign  axi_req_o.ar_valid   = slave.ar_valid;
    assign  slave.ar_ready       = axi_resp_i.ar_ready;
    assign  slave.r_id           = axi_resp_i.r.id;
    assign  slave.r_data         = axi_resp_i.r.data;
    assign  slave.r_resp         = axi_resp_i.r.resp;
    assign  slave.r_last         = axi_resp_i.r.last;
    assign  slave.r_valid        = axi_resp_i.r_valid;
    assign  slave.r_user         = 1'b0;
    assign  axi_req_o.r_ready    = slave.r_ready;
endmodule
import axi_pkg::*;
module axi_delayer #(
    parameter type aw_t = logic,
    parameter type w_t  = logic,
    parameter type b_t  = logic,
    parameter type ar_t = logic,
    parameter type r_t  = logic,
    parameter bit StallRandomOutput = 0,
    parameter bit StallRandomInput  = 0,
    parameter int FixedDelayInput   = 1,
    parameter int FixedDelayOutput  = 1
) (
    input  logic clk_i,    
    input  logic rst_ni,   
    input  logic aw_valid_i,
    input  aw_t  aw_chan_i,
    output logic aw_ready_o,
    input  logic w_valid_i,
    input  w_t   w_chan_i,
    output logic w_ready_o,
    output logic b_valid_o,
    output b_t   b_chan_o,
    input  logic b_ready_i,
    input  logic ar_valid_i,
    input  ar_t  ar_chan_i,
    output logic ar_ready_o,
    output logic r_valid_o,
    output r_t   r_chan_o,
    input  logic r_ready_i,
    output logic aw_valid_o,
    output aw_t  aw_chan_o,
    input  logic aw_ready_i,
    output logic w_valid_o,
    output w_t   w_chan_o,
    input  logic w_ready_i,
    input  logic b_valid_i,
    input  b_t   b_chan_i,
    output logic b_ready_o,
    output logic ar_valid_o,
    output ar_t  ar_chan_o,
    input  logic ar_ready_i,
    input  logic r_valid_i,
    input  r_t   r_chan_i,
    output logic r_ready_o
);
    stream_delay #(
      .StallRandom ( StallRandomInput ),
      .FixedDelay  ( FixedDelayInput  ),
      .payload_t   ( aw_t             )
    ) i_stream_delay_aw (
      .clk_i     ( clk_i      ),
      .rst_ni    ( rst_ni     ),
      .payload_i ( aw_chan_i  ),
      .ready_o   ( aw_ready_o ),
      .valid_i   ( aw_valid_i ),
      .payload_o ( aw_chan_o  ),
      .ready_i   ( aw_ready_i ),
      .valid_o   ( aw_valid_o )
    );
    stream_delay #(
      .StallRandom ( StallRandomInput ),
      .FixedDelay  ( FixedDelayInput  ),
      .payload_t   ( ar_t             )
    ) i_stream_delay_ar (
      .clk_i     ( clk_i      ),
      .rst_ni    ( rst_ni     ),
      .payload_i ( ar_chan_i  ),
      .ready_o   ( ar_ready_o ),
      .valid_i   ( ar_valid_i ),
      .payload_o ( ar_chan_o  ),
      .ready_i   ( ar_ready_i ),
      .valid_o   ( ar_valid_o )
    );
    stream_delay #(
      .StallRandom ( StallRandomInput ),
      .FixedDelay  ( FixedDelayInput  ),
      .payload_t   ( w_t              )
    ) i_stream_delay_w (
      .clk_i     ( clk_i      ),
      .rst_ni    ( rst_ni     ),
      .payload_i ( w_chan_i   ),
      .ready_o   ( w_ready_o  ),
      .valid_i   ( w_valid_i  ),
      .payload_o ( w_chan_o   ),
      .ready_i   ( w_ready_i  ),
      .valid_o   ( w_valid_o  )
    );
    stream_delay #(
      .StallRandom ( StallRandomOutput ),
      .FixedDelay  ( FixedDelayOutput  ),
      .payload_t   ( b_t               )
    ) i_stream_delay_b (
      .clk_i     ( clk_i      ),
      .rst_ni    ( rst_ni     ),
      .payload_i ( b_chan_i   ),
      .ready_o   ( b_ready_o  ),
      .valid_i   ( b_valid_i  ),
      .payload_o ( b_chan_o   ),
      .ready_i   ( b_ready_i  ),
      .valid_o   ( b_valid_o  )
    );
     stream_delay #(
      .StallRandom ( StallRandomOutput ),
      .FixedDelay  ( FixedDelayOutput  ),
      .payload_t   ( r_t               )
    ) i_stream_delay_r (
      .clk_i     ( clk_i      ),
      .rst_ni    ( rst_ni     ),
      .payload_i ( r_chan_i   ),
      .ready_o   ( r_ready_o  ),
      .valid_i   ( r_valid_i  ),
      .payload_o ( r_chan_o   ),
      .ready_i   ( r_ready_i  ),
      .valid_o   ( r_valid_o  )
    );
endmodule
module SyncSpRamBeNx64
#(
  parameter ADDR_WIDTH = 10,
  parameter DATA_DEPTH = 1024,  
  parameter OUT_REGS   = 0,     
  parameter SIM_INIT   = 0      
)(
  input  logic                  Clk_CI,
  input  logic                  Rst_RBI,
  input  logic                  CSel_SI,
  input  logic                  WrEn_SI,
  input  logic [7:0]            BEn_SI,
  input  logic [63:0]           WrData_DI,
  input  logic [ADDR_WIDTH-1:0] Addr_DI,
  output logic [63:0]           RdData_DO
);
  localparam DATA_BYTES = 8;
  logic [DATA_BYTES*8-1:0] RdData_DN;
  logic [DATA_BYTES*8-1:0] RdData_DP;
    logic [DATA_BYTES*8-1:0] Mem_DP[DATA_DEPTH-1:0];
    always_ff @(posedge Clk_CI) begin
      automatic logic [63:0] val;
      if(Rst_RBI == 1'b0 && SIM_INIT>0) begin
        for(int k=0; k<DATA_DEPTH;k++) begin
          if(SIM_INIT==1) val = '0;
          else val = 64'hdeadbeefdeadbeef;
          Mem_DP[k] = val;
        end
      end else
      if(CSel_SI) begin
        if(WrEn_SI) begin
          if(BEn_SI[0]) Mem_DP[Addr_DI][7:0]   <= WrData_DI[7:0];
          if(BEn_SI[1]) Mem_DP[Addr_DI][15:8]  <= WrData_DI[15:8];
          if(BEn_SI[2]) Mem_DP[Addr_DI][23:16] <= WrData_DI[23:16];
          if(BEn_SI[3]) Mem_DP[Addr_DI][31:24] <= WrData_DI[31:24];
          if(BEn_SI[4]) Mem_DP[Addr_DI][39:32] <= WrData_DI[39:32];
          if(BEn_SI[5]) Mem_DP[Addr_DI][47:40] <= WrData_DI[47:40];
          if(BEn_SI[6]) Mem_DP[Addr_DI][55:48] <= WrData_DI[55:48];
          if(BEn_SI[7]) Mem_DP[Addr_DI][63:56] <= WrData_DI[63:56];
        end
        RdData_DN <= Mem_DP[Addr_DI];
      end
    end
  generate
    if (OUT_REGS>0) begin : g_outreg
      always_ff @(posedge Clk_CI or negedge Rst_RBI) begin
        if(Rst_RBI == 1'b0)
        begin
          RdData_DP  <= 0;
        end
        else
        begin
          RdData_DP  <= RdData_DN;
        end
      end
    end
  endgenerate  
  generate
    if (OUT_REGS==0) begin : g_oureg_byp
      assign RdData_DP  = RdData_DN;
    end
  endgenerate 
  assign RdData_DO = RdData_DP;
  assert property
    (@(posedge Clk_CI) (longint'(2)**longint'(ADDR_WIDTH) >= longint'(DATA_DEPTH)))
    else $error("depth out of bounds");
endmodule  
/*verilator lint_off UNUSED*/ 
/*verilator lint_on UNUSED*/ 
/*verilator lint_off DECLFILENAME*/ 
module cdc_2phase #(
  parameter type T = logic
)(
  input  logic src_rst_ni,
  input  logic src_clk_i,
  input  T     src_data_i,
  input  logic src_valid_i,
  output logic src_ready_o,
  input  logic dst_rst_ni,
  input  logic dst_clk_i,
  output T     dst_data_o,
  output logic dst_valid_o,
  input  logic dst_ready_i
);
  (* dont_touch = "true" *) logic async_req;
  (* dont_touch = "true" *) logic async_ack;
  (* dont_touch = "true" *) T async_data;
  cdc_2phase_src #(.T(T)) i_src (
    .rst_ni       ( src_rst_ni  ),
    .clk_i        ( src_clk_i   ),
    .data_i       ( src_data_i  ),
    .valid_i      ( src_valid_i ),
    .ready_o      ( src_ready_o ),
    .async_req_o  ( async_req   ),
    .async_ack_i  ( async_ack   ),
    .async_data_o ( async_data  )
  );
  cdc_2phase_dst #(.T(T)) i_dst (
    .rst_ni       ( dst_rst_ni  ),
    .clk_i        ( dst_clk_i   ),
    .data_o       ( dst_data_o  ),
    .valid_o      ( dst_valid_o ),
    .ready_i      ( dst_ready_i ),
    .async_req_i  ( async_req   ),
    .async_ack_o  ( async_ack   ),
    .async_data_i ( async_data  )
  );
endmodule
module cdc_2phase_src #(
  parameter type T = logic
)(
  input  logic rst_ni,
  input  logic clk_i,
  input  T     data_i,
  input  logic valid_i,
  output logic ready_o,
  output logic async_req_o,
  input  logic async_ack_i,
  output T     async_data_o
);
  (* dont_touch = "true" *)
  logic req_src_q, ack_src_q, ack_q;
  (* dont_touch = "true" *)
  T data_src_q;
  always_ff @(posedge clk_i or negedge rst_ni) begin
    if (!rst_ni) begin
      req_src_q  <= 0;
      data_src_q <= '0;
    end else if (valid_i && ready_o) begin
      req_src_q  <= ~req_src_q;
      data_src_q <= data_i;
    end
  end
  always_ff @(posedge clk_i or negedge rst_ni) begin
    if (!rst_ni) begin
      ack_src_q <= 0;
      ack_q     <= 0;
    end else begin
      ack_src_q <= async_ack_i;
      ack_q     <= ack_src_q;
    end
  end
  assign ready_o = (req_src_q == ack_q);
  assign async_req_o = req_src_q;
  assign async_data_o = data_src_q;
endmodule
module cdc_2phase_dst #(
  parameter type T = logic
)(
  input  logic rst_ni,
  input  logic clk_i,
  output T     data_o,
  output logic valid_o,
  input  logic ready_i,
  input  logic async_req_i,
  output logic async_ack_o,
  input  T     async_data_i
);
  (* dont_touch = "true" *)
  (* async_reg = "true" *)
  logic req_dst_q, req_q0, req_q1, ack_dst_q;
  (* dont_touch = "true" *)
  T data_dst_q;
  always_ff @(posedge clk_i or negedge rst_ni) begin
    if (!rst_ni) begin
      ack_dst_q  <= 0;
    end else if (valid_o && ready_i) begin
      ack_dst_q  <= ~ack_dst_q;
    end
  end
  always_ff @(posedge clk_i or negedge rst_ni) begin
    if (!rst_ni) begin
      data_dst_q <= '0;
    end else if (req_q0 != req_q1 && !valid_o) begin
      data_dst_q <= async_data_i;
    end
  end
  always_ff @(posedge clk_i or negedge rst_ni) begin
    if (!rst_ni) begin
      req_dst_q <= 0;
      req_q0    <= 0;
      req_q1    <= 0;
    end else begin
      req_dst_q <= async_req_i;
      req_q0    <= req_dst_q;
      req_q1    <= req_q0;
    end
  end
  assign valid_o = (ack_dst_q != req_q1);
  assign data_o = data_dst_q;
  assign async_ack_o = ack_dst_q;
endmodule
/*verilator lint_on DECLFILENAME*/ 
module stream_arbiter #(
    parameter type      DATA_T = logic,    
    parameter integer   N_INP = -1,        
    parameter           ARBITER = "rr"     
) (
    input  logic              clk_i,
    input  logic              rst_ni,
    input  DATA_T [N_INP-1:0] inp_data_i,
    input  logic  [N_INP-1:0] inp_valid_i,
    output logic  [N_INP-1:0] inp_ready_o,
    output DATA_T             oup_data_o,
    output logic              oup_valid_o,
    input  logic              oup_ready_i
);
  stream_arbiter_flushable #(
    .DATA_T   (DATA_T),
    .N_INP    (N_INP),
    .ARBITER  (ARBITER)
  ) i_arb (
    .clk_i        (clk_i),
    .rst_ni       (rst_ni),
    .flush_i      (1'b0),
    .inp_data_i   (inp_data_i),
    .inp_valid_i  (inp_valid_i),
    .inp_ready_o  (inp_ready_o),
    .oup_data_o   (oup_data_o),
    .oup_valid_o  (oup_valid_o),
    .oup_ready_i  (oup_ready_i)
  );
endmodule
module stream_arbiter_flushable #(
    parameter type      DATA_T = logic,    
    parameter integer   N_INP = -1,        
    parameter           ARBITER = "rr"     
) (
    input  logic              clk_i,
    input  logic              rst_ni,
    input  logic              flush_i,
    input  DATA_T [N_INP-1:0] inp_data_i,
    input  logic  [N_INP-1:0] inp_valid_i,
    output logic  [N_INP-1:0] inp_ready_o,
    output DATA_T             oup_data_o,
    output logic              oup_valid_o,
    input  logic              oup_ready_i
);
  if (ARBITER == "rr") begin : gen_rr_arb
    rr_arb_tree #(
      .NumIn      (N_INP),
      .DataType   (DATA_T),
      .ExtPrio    (1'b0),
      .AxiVldRdy  (1'b1),
      .LockIn     (1'b1)
    ) i_arbiter (
      .clk_i,
      .rst_ni,
      .flush_i,
      .rr_i   ('0),
      .req_i  (inp_valid_i),
      .gnt_o  (inp_ready_o),
      .data_i (inp_data_i),
      .gnt_i  (oup_ready_i),
      .req_o  (oup_valid_o),
      .data_o (oup_data_o),
      .idx_o  ()
    );
  end else if (ARBITER == "prio") begin : gen_prio_arb
    rr_arb_tree #(
      .NumIn      (N_INP),
      .DataType   (DATA_T),
      .ExtPrio    (1'b1),
      .AxiVldRdy  (1'b1),
      .LockIn     (1'b1)
    ) i_arbiter (
      .clk_i,
      .rst_ni,
      .flush_i,
      .rr_i   ('0),
      .req_i  (inp_valid_i),
      .gnt_o  (inp_ready_o),
      .data_i (inp_data_i),
      .gnt_i  (oup_ready_i),
      .req_o  (oup_valid_o),
      .data_o (oup_data_o),
      .idx_o  ()
    );
  end else begin : gen_arb_error
    $fatal(1, "Invalid value for parameter 'ARBITER'!");
  end
endmodule
/*verilator lint_off DECLFILENAME*/ 
module fifo #(
    parameter bit          FALL_THROUGH = 1'b0,  
    parameter int unsigned DATA_WIDTH   = 32,    
    parameter int unsigned DEPTH        = 8,     
    parameter int unsigned THRESHOLD    = 1,     
    parameter type dtype                = logic [DATA_WIDTH-1:0]
)(
    input  logic  clk_i,             
    input  logic  rst_ni,            
    input  logic  flush_i,           
    input  logic  testmode_i,        
    output logic  full_o,            
    output logic  empty_o,           
    output logic  threshold_o,       
    input  dtype  data_i,            
    input  logic  push_i,            
    output dtype  data_o,            
    input  logic  pop_i              
);
    fifo_v2 #(
        .FALL_THROUGH ( FALL_THROUGH ),
        .DATA_WIDTH   ( DATA_WIDTH   ),
        .DEPTH        ( DEPTH        ),
        .ALM_FULL_TH  ( THRESHOLD    ),
        .dtype        ( dtype        )
    ) impl (
        .clk_i       ( clk_i       ),
        .rst_ni      ( rst_ni      ),
        .flush_i     ( flush_i     ),
        .testmode_i  ( testmode_i  ),
        .full_o      ( full_o      ),
        .empty_o     ( empty_o     ),
        .alm_full_o  ( threshold_o ),
        .alm_empty_o (             ),
        .data_i      ( data_i      ),
        .push_i      ( push_i      ),
        .data_o      ( data_o      ),
        .pop_i       ( pop_i       )
    );
endmodule
/*verilator lint_on DECLFILENAME*/ 
module fifo_v2 #(
    parameter bit          FALL_THROUGH = 1'b0,  
    parameter int unsigned DATA_WIDTH   = 32,    
    parameter int unsigned DEPTH        = 8,     
    parameter int unsigned ALM_EMPTY_TH = 1,     
    parameter int unsigned ALM_FULL_TH  = 1,     
    parameter type dtype                = logic [DATA_WIDTH-1:0],
    parameter int unsigned ADDR_DEPTH   = (DEPTH > 1) ? $clog2(DEPTH) : 1
)(
    input  logic  clk_i,             
    input  logic  rst_ni,            
    input  logic  flush_i,           
    input  logic  testmode_i,        
    output logic  full_o,            
    output logic  empty_o,           
    output logic  alm_full_o,        
    output logic  alm_empty_o,       
    input  dtype  data_i,            
    input  logic  push_i,            
    output dtype  data_o,            
    input  logic  pop_i              
);
    logic [ADDR_DEPTH-1:0] usage;
    if (DEPTH == 0) begin
        assign alm_full_o  = 1'b0;  
        assign alm_empty_o = 1'b0;  
    end else begin
        assign alm_full_o   = (usage >= ALM_FULL_TH[ADDR_DEPTH-1:0]);
        assign alm_empty_o  = (usage <= ALM_EMPTY_TH[ADDR_DEPTH-1:0]);
    end
    fifo_v3 #(
        .FALL_THROUGH ( FALL_THROUGH ),
        .DATA_WIDTH   ( DATA_WIDTH   ),
        .DEPTH        ( DEPTH        ),
        .dtype        ( dtype        )
    ) i_fifo_v3 (
        .clk_i,
        .rst_ni,
        .flush_i,
        .testmode_i,
        .full_o,
        .empty_o,
        .usage_o (usage),
        .data_i,
        .push_i,
        .data_o,
        .pop_i
    );
endmodule  
module fifo_v3 #(
    parameter bit          FALL_THROUGH = 1'b0,  
    parameter int unsigned DATA_WIDTH   = 32,    
    parameter int unsigned DEPTH        = 8,     
    parameter type dtype                = logic [DATA_WIDTH-1:0],
    parameter int unsigned ADDR_DEPTH   = (DEPTH > 1) ? $clog2(DEPTH) : 1
)(
    input  logic  clk_i,             
    input  logic  rst_ni,            
    input  logic  flush_i,           
    input  logic  testmode_i,        
    output logic  full_o,            
    output logic  empty_o,           
    output logic  [ADDR_DEPTH-1:0] usage_o,   
    input  dtype  data_i,            
    input  logic  push_i,            
    output dtype  data_o,            
    input  logic  pop_i              
);
    localparam int unsigned FIFO_DEPTH = (DEPTH > 0) ? DEPTH : 1;
    logic gate_clock;
    logic [ADDR_DEPTH - 1:0] read_pointer_n, read_pointer_q, write_pointer_n, write_pointer_q;
    logic [ADDR_DEPTH:0] status_cnt_n, status_cnt_q;  
    dtype [FIFO_DEPTH - 1:0] mem_n, mem_q;
    assign usage_o = status_cnt_q[ADDR_DEPTH-1:0];
    if (DEPTH == 0) begin
        assign empty_o     = ~push_i;
        assign full_o      = ~pop_i;
    end else begin
        assign full_o       = (status_cnt_q == FIFO_DEPTH[ADDR_DEPTH:0]);
        assign empty_o      = (status_cnt_q == 0) & ~(FALL_THROUGH & push_i);
    end
    always_comb begin : read_write_comb
        read_pointer_n  = read_pointer_q;
        write_pointer_n = write_pointer_q;
        status_cnt_n    = status_cnt_q;
        data_o          = (DEPTH == 0) ? data_i : mem_q[read_pointer_q];
        mem_n           = mem_q;
        gate_clock      = 1'b1;
        if (push_i && ~full_o) begin
            mem_n[write_pointer_q] = data_i;
            gate_clock = 1'b0;
            if (write_pointer_q == FIFO_DEPTH[ADDR_DEPTH-1:0] - 1)
                write_pointer_n = '0;
            else
                write_pointer_n = write_pointer_q + 1;
            status_cnt_n    = status_cnt_q + 1;
        end
        if (pop_i && ~empty_o) begin
            if (read_pointer_n == FIFO_DEPTH[ADDR_DEPTH-1:0] - 1)
                read_pointer_n = '0;
            else
                read_pointer_n = read_pointer_q + 1;
            status_cnt_n   = status_cnt_q - 1;
        end
        if (push_i && pop_i &&  ~full_o && ~empty_o)
            status_cnt_n   = status_cnt_q;
        if (FALL_THROUGH && (status_cnt_q == 0) && push_i) begin
            data_o = data_i;
            if (pop_i) begin
                status_cnt_n = status_cnt_q;
                read_pointer_n = read_pointer_q;
                write_pointer_n = write_pointer_q;
            end
        end
    end
    always_ff @(posedge clk_i or negedge rst_ni) begin
        if(~rst_ni) begin
            read_pointer_q  <= '0;
            write_pointer_q <= '0;
            status_cnt_q    <= '0;
        end else begin
            if (flush_i) begin
                read_pointer_q  <= '0;
                write_pointer_q <= '0;
                status_cnt_q    <= '0;
             end else begin
                read_pointer_q  <= read_pointer_n;
                write_pointer_q <= write_pointer_n;
                status_cnt_q    <= status_cnt_n;
            end
        end
    end
    always_ff @(posedge clk_i or negedge rst_ni) begin
        if(~rst_ni) begin
            mem_q <= '0;
        end else if (!gate_clock) begin
            mem_q <= mem_n;
        end
    end
endmodule  
module rr_arb_tree #(
  parameter int unsigned NumIn      = 64,
  parameter int unsigned DataWidth  = 32,
  parameter type         DataType   = logic [DataWidth-1:0],
  parameter bit          ExtPrio    = 1'b0,  
  parameter bit          AxiVldRdy  = 1'b0,  
  parameter bit          LockIn     = 1'b0   
) (
  input  logic                             clk_i,
  input  logic                             rst_ni,
  input  logic                             flush_i,  
  input  logic [$clog2(NumIn)-1:0]         rr_i,     
  input  logic [NumIn-1:0]                 req_i,
  /*verilator lint_off UNOPTFLAT*/ 
  output logic [NumIn-1:0]                 gnt_o,
  /*verilator lint_on UNOPTFLAT*/ 
  input  DataType [NumIn-1:0]              data_i,
  input  logic                             gnt_i,
  output logic                             req_o,
  output DataType                          data_o,
  output logic [$clog2(NumIn)-1:0]         idx_o
);
  if (NumIn == unsigned'(1)) begin
    assign req_o    = req_i[0];
    assign gnt_o[0] = gnt_i;
    assign data_o   = data_i[0];
    assign idx_o    = '0;
  end else begin
    localparam int unsigned NumLevels = $clog2(NumIn);
    /*verilator lint_off UNOPTFLAT*/ 
    logic [2**NumLevels-2:0][NumLevels-1:0]  index_nodes;  
    DataType [2**NumLevels-2:0]              data_nodes;   
    logic [2**NumLevels-2:0]                 gnt_nodes;    
    logic [2**NumLevels-2:0]                 req_nodes;    
    logic [NumLevels-1:0]                    rr_q;
    logic [NumIn-1:0]                        req_d;
    assign req_o        = req_nodes[0];
    assign data_o       = data_nodes[0];
    assign idx_o        = index_nodes[0];
    if (ExtPrio) begin : gen_ext_rr
      assign rr_q       = rr_i;
      assign req_d      = req_i;
    end else begin : gen_int_rr
      logic [NumLevels-1:0] rr_d;
      if (LockIn) begin : gen_lock
        logic  lock_d, lock_q;
        logic [NumIn-1:0]     req_q;
        assign lock_d     = req_o & ~gnt_i;
        assign req_d      = (lock_q) ? req_q : req_i;
        always_ff @(posedge clk_i or negedge rst_ni) begin : p_lock_reg
          if (!rst_ni) begin
            lock_q <= '0;
          end else begin
            if (flush_i) begin
              lock_q <= '0;
            end else begin
              lock_q <= lock_d;
            end
          end
        end
        always_ff @(posedge clk_i or negedge rst_ni) begin : p_req_regs
          if (!rst_ni) begin
            req_q  <= '0;
          end else begin
            if (flush_i) begin
              req_q  <= '0;
            end else begin
              req_q  <= req_d;
            end
          end
        end
      end else begin : gen_no_lock
        assign req_d = req_i;
      end
      assign rr_d       = (gnt_i && req_o) ? ((rr_q == NumLevels'(NumIn-1)) ? '0 : rr_q + 1'b1) : rr_q;
      always_ff @(posedge clk_i or negedge rst_ni) begin : p_rr_regs
        if (!rst_ni) begin
          rr_q   <= '0;
        end else begin
          if (flush_i) begin
            rr_q   <= '0;
          end else begin
            rr_q   <= rr_d;
          end
        end
      end
    end
    assign gnt_nodes[0] = gnt_i;
    for (genvar level = 0; unsigned'(level) < NumLevels; level++) begin : gen_levels
      for (genvar l = 0; l < 2**level; l++) begin : gen_level
        logic sel;
        localparam int unsigned idx0 = 2**level-1+l; 
        localparam int unsigned idx1 = 2**(level+1)-1+l*2;
        if (unsigned'(level) == NumLevels-1) begin : gen_first_level
          if (unsigned'(l) * 2 < NumIn-1) begin
            assign req_nodes[idx0]   = req_d[l*2] | req_d[l*2+1];
            assign sel =  ~req_d[l*2] | req_d[l*2+1] & rr_q[NumLevels-1-level];
            assign index_nodes[idx0] = NumLevels'(sel);
            assign data_nodes[idx0]  = (sel) ? data_i[l*2+1] : data_i[l*2];
            assign gnt_o[l*2]        = gnt_nodes[idx0] & (AxiVldRdy | req_d[l*2])   & ~sel;
            assign gnt_o[l*2+1]      = gnt_nodes[idx0] & (AxiVldRdy | req_d[l*2+1]) & sel;
          end
          if (unsigned'(l) * 2 == NumIn-1) begin
            assign req_nodes[idx0]   = req_d[l*2];
            assign index_nodes[idx0] = '0; 
            assign data_nodes[idx0]  = data_i[l*2];
            assign gnt_o[l*2]        = gnt_nodes[idx0] & (AxiVldRdy | req_d[l*2]);
          end
          if (unsigned'(l) * 2 > NumIn-1) begin
            assign req_nodes[idx0]   = 1'b0;
            assign index_nodes[idx0] = DataType'('0);
            assign data_nodes[idx0]  = DataType'('0);
          end
        end else begin : gen_other_levels
          assign req_nodes[idx0]   = req_nodes[idx1] | req_nodes[idx1+1];
          assign sel =  ~req_nodes[idx1] | req_nodes[idx1+1] & rr_q[NumLevels-1-level];
          assign index_nodes[idx0] = (sel) ? NumLevels'({1'b1, index_nodes[idx1+1][NumLevels-unsigned'(level)-2:0]}) :
                                             NumLevels'({1'b0, index_nodes[idx1][NumLevels-unsigned'(level)-2:0]});
          assign data_nodes[idx0]  = (sel) ? data_nodes[idx1+1] : data_nodes[idx1];
          assign gnt_nodes[idx1]   = gnt_nodes[idx0] & ~sel;
          assign gnt_nodes[idx1+1] = gnt_nodes[idx0] & sel;
        end
      end
    end
  end
endmodule : rr_arb_tree
module stream_delay #(
    parameter bit   StallRandom = 0,
    parameter int   FixedDelay  = 1,
    parameter type  payload_t  = logic
)(
    input  logic     clk_i,
    input  logic     rst_ni,
    input  payload_t payload_i,
    output logic     ready_o,
    input  logic     valid_i,
    output payload_t payload_o,
    input  logic     ready_i,
    output logic     valid_o
);
    if (FixedDelay == 0 && !StallRandom) begin : pass_through
        assign ready_o = ready_i;
        assign valid_o = valid_i;
        assign payload_o = payload_i;
    end else begin
        localparam COUNTER_BITS = 4;
        typedef enum logic [1:0] {
            Idle, Valid, Ready
        } state_e;
        state_e state_d, state_q;
        logic       load;
        logic [3:0] count_out;
        logic       en;
        logic [COUNTER_BITS-1:0] counter_load;
        assign payload_o = payload_i;
        always_comb begin
            state_d = state_q;
            valid_o = 1'b0;
            ready_o = 1'b0;
            load    = 1'b0;
            en      = 1'b0;
            unique case (state_q)
                Idle: begin
                    if (valid_i) begin
                        load = 1'b1;
                        state_d = Valid;
                        if (FixedDelay == 1 || (StallRandom && counter_load == 1)) begin
                            state_d = Ready;
                        end
                        if (StallRandom && counter_load == 0) begin
                            valid_o = 1'b1;
                            ready_o = ready_i;
                            if (ready_i) state_d = Idle;
                            else state_d = Ready;
                        end
                    end
                end
                Valid: begin
                    en = 1'b1;
                    if (count_out == 0) begin
                        state_d = Ready;
                    end
                end
                Ready: begin
                    valid_o = 1'b1;
                    ready_o = ready_i;
                    if (ready_i) state_d = Idle;
                end
                default :  ;
            endcase
        end
        if (StallRandom) begin : random_stall
            lfsr_16bit #(
              .WIDTH ( 16 )
            ) i_lfsr_16bit (
                .clk_i          ( clk_i        ),
                .rst_ni         ( rst_ni       ),
                .en_i           ( load         ),
                .refill_way_oh  (              ),
                .refill_way_bin ( counter_load )
            );
        end else begin
            assign counter_load = FixedDelay;
        end
        counter #(
            .WIDTH      ( COUNTER_BITS )
        ) i_counter (
            .clk_i      ( clk_i        ),
            .rst_ni     ( rst_ni       ),
            .clear_i    ( 1'b0         ),
            .en_i       ( en           ),
            .load_i     ( load         ),
            .down_i     ( 1'b1         ),
            .d_i        ( counter_load ),
            .q_o        ( count_out    ),
            .overflow_o (              )
        );
        always_ff @(posedge clk_i or negedge rst_ni) begin
            if (~rst_ni) begin
                state_q <= Idle;
            end else begin
                state_q <= state_d;
            end
        end
    end
endmodule
module lfsr_16bit #(
    parameter logic [15:0] SEED  = 8'b0,
    parameter int unsigned WIDTH = 16
)(
    input  logic                      clk_i,
    input  logic                      rst_ni,
    input  logic                      en_i,
    output logic [WIDTH-1:0]          refill_way_oh,
    output logic [$clog2(WIDTH)-1:0]  refill_way_bin
);
    localparam int unsigned LOG_WIDTH = $clog2(WIDTH);
    logic [15:0] shift_d, shift_q;
    always_comb begin
        automatic logic shift_in;
        shift_in = !(shift_q[15] ^ shift_q[12] ^ shift_q[5] ^ shift_q[1]);
        shift_d = shift_q;
        if (en_i)
            shift_d = {shift_q[14:0], shift_in};
        refill_way_oh = 'b0;
        refill_way_oh[shift_q[LOG_WIDTH-1:0]] = 1'b1;
        refill_way_bin = shift_q;
    end
    always_ff @(posedge clk_i or negedge rst_ni) begin : proc_
        if(~rst_ni) begin
            shift_q <= SEED;
        end else begin
            shift_q <= shift_d;
        end
    end
    initial begin
        assert (WIDTH <= 16) else $fatal(1, "WIDTH needs to be less than 16 because of the 16-bit LFSR");
    end
endmodule
module delta_counter #(
    parameter int unsigned WIDTH = 4,
    parameter bit STICKY_OVERFLOW = 1'b0
)(
    input  logic             clk_i,
    input  logic             rst_ni,
    input  logic             clear_i,  
    input  logic             en_i,     
    input  logic             load_i,   
    input  logic             down_i,   
    input  logic [WIDTH-1:0] delta_i,
    input  logic [WIDTH-1:0] d_i,
    output logic [WIDTH-1:0] q_o,
    output logic             overflow_o
);
    logic [WIDTH:0] counter_q, counter_d;
    if (STICKY_OVERFLOW) begin : gen_sticky_overflow
        logic overflow_d, overflow_q;
        always_ff @(posedge clk_i or negedge rst_ni) overflow_q <= ~rst_ni ? 1'b0 : overflow_d;
        always_comb begin
            overflow_d = overflow_q;
            if (clear_i || load_i) begin
                overflow_d = 1'b0;
            end else if (!overflow_q && en_i) begin
                if (down_i) begin
                    overflow_d = delta_i > counter_q[WIDTH-1:0];
                end else begin
                    overflow_d = counter_q[WIDTH-1:0] > ({WIDTH{1'b1}} - delta_i);
                end
            end
        end
        assign overflow_o = overflow_q;
    end else begin : gen_transient_overflow
        assign overflow_o = counter_q[WIDTH];
    end
    assign q_o = counter_q[WIDTH-1:0];
    always_comb begin
        counter_d = counter_q;
        if (clear_i) begin
            counter_d = '0;
        end else if (load_i) begin
            counter_d = {1'b0, d_i};
        end else if (en_i) begin
            if (down_i) begin
                counter_d = counter_q - delta_i;
            end else begin
                counter_d = counter_q + delta_i;
            end
        end
    end
    always_ff @(posedge clk_i or negedge rst_ni) begin
        if (!rst_ni) begin
           counter_q <= '0;
        end else begin
           counter_q <= counter_d;
        end
    end
endmodule
module counter #(
    parameter int unsigned WIDTH = 4,
    parameter bit STICKY_OVERFLOW = 1'b0
)(
    input  logic             clk_i,
    input  logic             rst_ni,
    input  logic             clear_i,  
    input  logic             en_i,     
    input  logic             load_i,   
    input  logic             down_i,   
    input  logic [WIDTH-1:0] d_i,
    output logic [WIDTH-1:0] q_o,
    output logic             overflow_o
);
    delta_counter #(
        .WIDTH          (WIDTH),
        .STICKY_OVERFLOW (STICKY_OVERFLOW)
    ) i_counter (
        .clk_i,
        .rst_ni,
        .clear_i,
        .en_i,
        .load_i,
        .down_i,
        .delta_i({{WIDTH-1{1'b0}}, 1'b1}),
        .d_i,
        .q_o,
        .overflow_o
    );
endmodule
module cluster_clock_inverter
  (
   input  logic clk_i,
   output logic clk_o
   );
   assign clk_o = ~clk_i;
endmodule
module pulp_clock_mux2
  (
   input  logic clk0_i,
   input  logic clk1_i,
   input  logic clk_sel_i,
   output logic clk_o
   );
   always_comb
     begin
	if (clk_sel_i == 1'b0)
	  clk_o = clk0_i;
	else
	  clk_o = clk1_i;
     end
endmodule
module mock_uart (
    input  logic          clk_i,
    input  logic          rst_ni,
    input  logic          penable_i,
    input  logic          pwrite_i,
    input  logic [31:0]   paddr_i,
    input  logic          psel_i,
    input  logic [31:0]   pwdata_i,
    output logic [31:0]   prdata_o,
    output logic          pready_o,
    output logic          pslverr_o
);
    localparam RBR = 0;
    localparam THR = 0;
    localparam IER = 1;
    localparam IIR = 2;
    localparam FCR = 2;
    localparam LCR = 3;
    localparam MCR = 4;
    localparam LSR = 5;
    localparam MSR = 6;
    localparam SCR = 7;
    localparam DLL = 0;
    localparam DLM = 1;
    localparam THRE = 5;  
    localparam TEMT = 6;  
    byte lcr = 0;
    byte dlm = 0;
    byte dll = 0;
    byte mcr = 0;
    byte lsr = 0;
    byte ier = 0;
    byte msr = 0;
    byte scr = 0;
    logic fifo_enabled = 1'b0;
    assign pready_o = 1'b1;
    assign pslverr_o = 1'b0;
    function void uart_tx(byte ch);
        $write("%c", ch);
    endfunction : uart_tx
    always_ff @(posedge clk_i or negedge rst_ni) begin
        if (rst_ni) begin
            if (psel_i & penable_i & pwrite_i) begin
                case ((paddr_i >> 'h2) & 'h7)
                    THR: begin
                        if (lcr & 'h80) dll <= byte'(pwdata_i[7:0]);
                        else uart_tx(byte'(pwdata_i[7:0]));
                    end
                    IER: begin
                        if (lcr & 'h80) dlm <= byte'(pwdata_i[7:0]);
                        else ier <= byte'(pwdata_i[7:0] & 'hF);
                    end
                    FCR: begin
                        if (pwdata_i[0]) fifo_enabled <= 1'b1;
                        else fifo_enabled <= 1'b0;
                    end
                    LCR: lcr <= byte'(pwdata_i[7:0]);
                    MCR: mcr <= byte'(pwdata_i[7:0] & 'h1F);
                    LSR: lsr <= byte'(pwdata_i[7:0]);
                    MSR: msr <= byte'(pwdata_i[7:0]);
                    SCR: scr <= byte'(pwdata_i[7:0]);
                    default:;
                endcase
            end
        end
    end
    always_comb begin
        prdata_o = '0;
        if (psel_i & penable_i & ~pwrite_i) begin
            case ((paddr_i >> 'h2) & 'h7)
                THR: begin
                    if (lcr & 'h80) prdata_o = {24'b0, dll};
                end
                IER: begin
                    if (lcr & 'h80) prdata_o = {24'b0, dlm};
                    else prdata_o = {24'b0, ier};
                end
                IIR: begin
                    if (fifo_enabled) prdata_o = {24'b0, 8'hc0};
                    else prdata_o = {24'b0, 8'b0};
                end
                LCR: prdata_o = {24'b0, lcr};
                MCR: prdata_o = {24'b0, mcr};
                LSR: prdata_o = {24'b0, (lsr | (1'b1 << THRE) | (1'b1 << TEMT))};
                MSR: prdata_o = {24'b0, msr};
                SCR: prdata_o = {24'b0, scr};
                default:;
            endcase
        end
    end
endmodule

module ariane_peripherals #(
    parameter int AxiAddrWidth = -1,
    parameter int AxiDataWidth = -1,
    parameter int AxiIdWidth   = -1,
    parameter int AxiUserWidth = 1,
    parameter bit InclUART     = 1,
    parameter bit InclSPI      = 0,
    parameter bit InclEthernet = 0,
    parameter bit InclGPIO     = 0,
    parameter bit InclTimer    = 1
) (
    input  logic       clk_i           ,  
    input  logic       rst_ni          ,  
    AXI_BUS.Slave      plic            ,
    AXI_BUS.Slave      uart            ,
    AXI_BUS.Slave      spi             ,
    AXI_BUS.Slave      ethernet        ,
    AXI_BUS.Slave      timer           ,
    output logic [1:0] irq_o           ,
    input  logic       rx_i            ,
    output logic       tx_o            ,
    input  wire        eth_txck        ,
    input  wire        eth_rxck        ,
    input  wire        eth_rxctl       ,
    input  wire [3:0]  eth_rxd         ,
    output wire        eth_rst_n       ,
    output wire        eth_tx_en       ,
    output wire [3:0]  eth_txd         ,
    inout  wire        phy_mdio        ,
    output logic       eth_mdc         ,
    inout              mdio            ,
    output             mdc             ,
    output logic       spi_clk_o       ,
    output logic       spi_mosi        ,
    input  logic       spi_miso        ,
    output logic       spi_ss
);
    logic [ariane_soc::NumSources-1:0] irq_sources;
    assign irq_sources[ariane_soc::NumSources-1:7] = '0;
    REG_BUS #(
        .ADDR_WIDTH ( 32 ),
        .DATA_WIDTH ( 32 )
    ) reg_bus (clk_i);
    logic         plic_penable;
    logic         plic_pwrite;
    logic [31:0]  plic_paddr;
    logic         plic_psel;
    logic [31:0]  plic_pwdata;
    logic [31:0]  plic_prdata;
    logic         plic_pready;
    logic         plic_pslverr;
    axi2apb_64_32 #(
        .AXI4_ADDRESS_WIDTH ( AxiAddrWidth  ),
        .AXI4_RDATA_WIDTH   ( AxiDataWidth  ),
        .AXI4_WDATA_WIDTH   ( AxiDataWidth  ),
        .AXI4_ID_WIDTH      ( AxiIdWidth    ),
        .AXI4_USER_WIDTH    ( AxiUserWidth  ),
        .BUFF_DEPTH_SLAVE   ( 2             ),
        .APB_ADDR_WIDTH     ( 32            )
    ) i_axi2apb_64_32_plic (
        .ACLK      ( clk_i          ),
        .ARESETn   ( rst_ni         ),
        .test_en_i ( 1'b0           ),
        .AWID_i    ( plic.aw_id     ),
        .AWADDR_i  ( plic.aw_addr   ),
        .AWLEN_i   ( plic.aw_len    ),
        .AWSIZE_i  ( plic.aw_size   ),
        .AWBURST_i ( plic.aw_burst  ),
        .AWLOCK_i  ( plic.aw_lock   ),
        .AWCACHE_i ( plic.aw_cache  ),
        .AWPROT_i  ( plic.aw_prot   ),
        .AWREGION_i( plic.aw_region ),
        .AWUSER_i  ( plic.aw_user   ),
        .AWQOS_i   ( plic.aw_qos    ),
        .AWVALID_i ( plic.aw_valid  ),
        .AWREADY_o ( plic.aw_ready  ),
        .WDATA_i   ( plic.w_data    ),
        .WSTRB_i   ( plic.w_strb    ),
        .WLAST_i   ( plic.w_last    ),
        .WUSER_i   ( plic.w_user    ),
        .WVALID_i  ( plic.w_valid   ),
        .WREADY_o  ( plic.w_ready   ),
        .BID_o     ( plic.b_id      ),
        .BRESP_o   ( plic.b_resp    ),
        .BVALID_o  ( plic.b_valid   ),
        .BUSER_o   ( plic.b_user    ),
        .BREADY_i  ( plic.b_ready   ),
        .ARID_i    ( plic.ar_id     ),
        .ARADDR_i  ( plic.ar_addr   ),
        .ARLEN_i   ( plic.ar_len    ),
        .ARSIZE_i  ( plic.ar_size   ),
        .ARBURST_i ( plic.ar_burst  ),
        .ARLOCK_i  ( plic.ar_lock   ),
        .ARCACHE_i ( plic.ar_cache  ),
        .ARPROT_i  ( plic.ar_prot   ),
        .ARREGION_i( plic.ar_region ),
        .ARUSER_i  ( plic.ar_user   ),
        .ARQOS_i   ( plic.ar_qos    ),
        .ARVALID_i ( plic.ar_valid  ),
        .ARREADY_o ( plic.ar_ready  ),
        .RID_o     ( plic.r_id      ),
        .RDATA_o   ( plic.r_data    ),
        .RRESP_o   ( plic.r_resp    ),
        .RLAST_o   ( plic.r_last    ),
        .RUSER_o   ( plic.r_user    ),
        .RVALID_o  ( plic.r_valid   ),
        .RREADY_i  ( plic.r_ready   ),
        .PENABLE   ( plic_penable   ),
        .PWRITE    ( plic_pwrite    ),
        .PADDR     ( plic_paddr     ),
        .PSEL      ( plic_psel      ),
        .PWDATA    ( plic_pwdata    ),
        .PRDATA    ( plic_prdata    ),
        .PREADY    ( plic_pready    ),
        .PSLVERR   ( plic_pslverr   )
    );
    apb_to_reg i_apb_to_reg (
        .clk_i     ( clk_i        ),
        .rst_ni    ( rst_ni       ),
        .penable_i ( plic_penable ),
        .pwrite_i  ( plic_pwrite  ),
        .paddr_i   ( plic_paddr   ),
        .psel_i    ( plic_psel    ),
        .pwdata_i  ( plic_pwdata  ),
        .prdata_o  ( plic_prdata  ),
        .pready_o  ( plic_pready  ),
        .pslverr_o ( plic_pslverr ),
        .reg_o     ( reg_bus      )
    );
    reg_intf::reg_intf_resp_d32 plic_resp;
    reg_intf::reg_intf_req_a32_d32 plic_req;
    assign plic_req.addr  = reg_bus.addr;
    assign plic_req.write = reg_bus.write;
    assign plic_req.wdata = reg_bus.wdata;
    assign plic_req.wstrb = reg_bus.wstrb;
    assign plic_req.valid = reg_bus.valid;
    assign reg_bus.rdata = plic_resp.rdata;
    assign reg_bus.error = plic_resp.error;
    assign reg_bus.ready = plic_resp.ready;
    plic_top #(
      .N_SOURCE    ( ariane_soc::NumSources  ),
      .N_TARGET    ( ariane_soc::NumTargets  ),
      .MAX_PRIO    ( ariane_soc::MaxPriority )
    ) i_plic (
      .clk_i,
      .rst_ni,
      .req_i         ( plic_req    ),
      .resp_o        ( plic_resp   ),
      .le_i          ( '0          ),  
      .irq_sources_i ( irq_sources ),
      .eip_targets_o ( irq_o       )
    );
    logic         uart_penable;
    logic         uart_pwrite;
    logic [31:0]  uart_paddr;
    logic         uart_psel;
    logic [31:0]  uart_pwdata;
    logic [31:0]  uart_prdata;
    logic         uart_pready;
    logic         uart_pslverr;
    axi2apb_64_32 #(
        .AXI4_ADDRESS_WIDTH ( AxiAddrWidth ),
        .AXI4_RDATA_WIDTH   ( AxiDataWidth ),
        .AXI4_WDATA_WIDTH   ( AxiDataWidth ),
        .AXI4_ID_WIDTH      ( AxiIdWidth   ),
        .AXI4_USER_WIDTH    ( AxiUserWidth ),
        .BUFF_DEPTH_SLAVE   ( 2            ),
        .APB_ADDR_WIDTH     ( 32           )
    ) i_axi2apb_64_32_uart (
        .ACLK      ( clk_i          ),
        .ARESETn   ( rst_ni         ),
        .test_en_i ( 1'b0           ),
        .AWID_i    ( uart.aw_id     ),
        .AWADDR_i  ( uart.aw_addr   ),
        .AWLEN_i   ( uart.aw_len    ),
        .AWSIZE_i  ( uart.aw_size   ),
        .AWBURST_i ( uart.aw_burst  ),
        .AWLOCK_i  ( uart.aw_lock   ),
        .AWCACHE_i ( uart.aw_cache  ),
        .AWPROT_i  ( uart.aw_prot   ),
        .AWREGION_i( uart.aw_region ),
        .AWUSER_i  ( uart.aw_user   ),
        .AWQOS_i   ( uart.aw_qos    ),
        .AWVALID_i ( uart.aw_valid  ),
        .AWREADY_o ( uart.aw_ready  ),
        .WDATA_i   ( uart.w_data    ),
        .WSTRB_i   ( uart.w_strb    ),
        .WLAST_i   ( uart.w_last    ),
        .WUSER_i   ( uart.w_user    ),
        .WVALID_i  ( uart.w_valid   ),
        .WREADY_o  ( uart.w_ready   ),
        .BID_o     ( uart.b_id      ),
        .BRESP_o   ( uart.b_resp    ),
        .BVALID_o  ( uart.b_valid   ),
        .BUSER_o   ( uart.b_user    ),
        .BREADY_i  ( uart.b_ready   ),
        .ARID_i    ( uart.ar_id     ),
        .ARADDR_i  ( uart.ar_addr   ),
        .ARLEN_i   ( uart.ar_len    ),
        .ARSIZE_i  ( uart.ar_size   ),
        .ARBURST_i ( uart.ar_burst  ),
        .ARLOCK_i  ( uart.ar_lock   ),
        .ARCACHE_i ( uart.ar_cache  ),
        .ARPROT_i  ( uart.ar_prot   ),
        .ARREGION_i( uart.ar_region ),
        .ARUSER_i  ( uart.ar_user   ),
        .ARQOS_i   ( uart.ar_qos    ),
        .ARVALID_i ( uart.ar_valid  ),
        .ARREADY_o ( uart.ar_ready  ),
        .RID_o     ( uart.r_id      ),
        .RDATA_o   ( uart.r_data    ),
        .RRESP_o   ( uart.r_resp    ),
        .RLAST_o   ( uart.r_last    ),
        .RUSER_o   ( uart.r_user    ),
        .RVALID_o  ( uart.r_valid   ),
        .RREADY_i  ( uart.r_ready   ),
        .PENABLE   ( uart_penable   ),
        .PWRITE    ( uart_pwrite    ),
        .PADDR     ( uart_paddr     ),
        .PSEL      ( uart_psel      ),
        .PWDATA    ( uart_pwdata    ),
        .PRDATA    ( uart_prdata    ),
        .PREADY    ( uart_pready    ),
        .PSLVERR   ( uart_pslverr   )
    );
        assign irq_sources[0] = 1'b0;
        mock_uart i_mock_uart (
            .clk_i     ( clk_i        ),
            .rst_ni    ( rst_ni       ),
            .penable_i ( uart_penable ),
            .pwrite_i  ( uart_pwrite  ),
            .paddr_i   ( uart_paddr   ),
            .psel_i    ( uart_psel    ),
            .pwdata_i  ( uart_pwdata  ),
            .prdata_o  ( uart_prdata  ),
            .pready_o  ( uart_pready  ),
            .pslverr_o ( uart_pslverr )
        );
        assign spi_clk_o = 1'b0;
        assign spi_mosi = 1'b0;
        assign spi_ss = 1'b0;
        assign irq_sources [1] = 1'b0;
        assign spi.aw_ready = 1'b1;
        assign spi.ar_ready = 1'b1;
        assign spi.w_ready = 1'b1;
        assign spi.b_valid = spi.aw_valid;
        assign spi.b_id = spi.aw_id;
        assign spi.b_resp = axi_pkg::RESP_SLVERR;
        assign spi.b_user = '0;
        assign spi.r_valid = spi.ar_valid;
        assign spi.r_resp = axi_pkg::RESP_SLVERR;
        assign spi.r_data = 'hdeadbeef;
        assign spi.r_last = 1'b1;

        assign irq_sources [2] = 1'b0;
        assign ethernet.aw_ready = 1'b1;
        assign ethernet.ar_ready = 1'b1;
        assign ethernet.w_ready = 1'b1;
        assign ethernet.b_valid = ethernet.aw_valid;
        assign ethernet.b_id = ethernet.aw_id;
        assign ethernet.b_resp = axi_pkg::RESP_SLVERR;
        assign ethernet.b_user = '0;
        assign ethernet.r_valid = ethernet.ar_valid;
        assign ethernet.r_resp = axi_pkg::RESP_SLVERR;
        assign ethernet.r_data = 'hdeadbeef;
        assign ethernet.r_last = 1'b1;
endmodule
