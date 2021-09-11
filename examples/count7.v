module count7(clk, rst, q);

   input clk;
   input rst;
   output [7:0] q;
   reg [7:0] q;

   always @(posedge clk)
     case (rst)
       1'b1 : q = 8'b0;
       default: q = q + 8'b1;
     endcase // case (rst)
   
endmodule // count
