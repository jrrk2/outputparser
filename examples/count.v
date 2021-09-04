module count(input clk, input rst, output reg [7:0] p);

   always @(posedge clk)
     if (rst)
       p <= 8'b0;
     else
       p <= p + 8'b1;

endmodule // query
