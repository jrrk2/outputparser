module count(input clk, input rst, output reg [7:0] q);

   always @(posedge clk)
     if (rst)
       q = 8'b0;
     else
       q = q + 8'b1;

endmodule // query
