module shiftx(
	input             clk,
	input       [7:0] opcode,
	input       [2:0] sel,
 	output reg        rslt
);

always @(posedge clk)
	rslt = opcode[sel];

endmodule
