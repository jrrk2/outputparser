
module regindex_bits #(
	parameter [ 0:0] ENABLE_REGS_16_31 = 1,
	parameter [ 0:0] ENABLE_PCPI = 0,
	parameter [ 0:0] ENABLE_MUL = 0,
	parameter [ 0:0] ENABLE_FAST_MUL = 0,
	parameter [ 0:0] ENABLE_DIV = 0,
	parameter [ 0:0] ENABLE_IRQ = 0,
	parameter [ 0:0] ENABLE_IRQ_QREGS = 1,
	parameter [ 0:0] ENABLE_IRQ_TIMER = 1
) (
	input clk, resetn,
	output reg trap,

	// Trace Interface
	output reg        trace_valid,
	output reg [35:0] trace_data
);
	localparam integer irqregs_offset = ENABLE_REGS_16_31 ? 32 : 16;
	localparam integer regfile_size = (ENABLE_REGS_16_31 ? 32 : 16) + 4*ENABLE_IRQ*ENABLE_IRQ_QREGS;
	localparam integer regindex_bits = (ENABLE_REGS_16_31 ? 5 : 4) + ENABLE_IRQ*ENABLE_IRQ_QREGS;

	reg [regindex_bits-1:0] decoded_rd, decoded_rs1, decoded_rs2;

   assign trace_data = regindex_bits;
   
endmodule
