module array(
	input             clk,
	input       [7:0] araddr,
	input       [2:0] sel,
 	output reg        rslt
);

   reg [7:0] 		  mem[0:255];
   
always @(posedge clk)
	rslt = mem[araddr][sel];

endmodule
