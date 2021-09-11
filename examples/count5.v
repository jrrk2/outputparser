module count5(input clk, input rst, output reg [7:0] q);

   always @(posedge clk or posedge rst)
     if (rst)
       q = 8'b0;
     else q = q + 8'b1;

endmodule
