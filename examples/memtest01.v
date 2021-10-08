
module memtest01(clk, wr_en, wr_addr, wr_value, rd_addr, rd_value);

input clk, wr_en;
input [1:0] wr_addr, rd_addr;
input [3:0] wr_value;
output [3:0] rd_value;

reg [3:0] data [3:0];

always @(posedge clk)
	if (wr_en)
		data[wr_addr] <= wr_value;

assign rd_value = data[rd_addr];

endmodule
