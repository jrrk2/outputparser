module count4(input clk, input rst, output reg [7:0] q);

   always @(posedge clk)
     q = rst ? 8'b0 : q + 8'b1;

endmodule
