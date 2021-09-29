module count(input clk, input rst, output reg [2:0] q);

   always @(posedge clk)
     if (rst)
       q <= 3'b0;
     else
       q <= q + 3'b1;

endmodule // query
