module count(clk, rst, q);

   input clk;
   input rst;
   output [7:0] q;
   reg [7:0] q;

   reg [7:0] nxt;
   
   always @(posedge clk)
     if (rst)
       q = 8'b0;
     else
       begin
	  nxt = q + 8'b1;
	  q = nxt;
	  
       end

endmodule // count
