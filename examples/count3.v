module count(input clk, input rst, input en, output reg [7:0] q);

   enum {zero,one,two} e_t;

   reg [1:0] state;
   
   always @(posedge clk)
     if (rst)
       q = 8'b0;
     else
       begin
       case (state)
	  zero:
	     q = q + 8'b1;
	  one:
	     q = q + 8'b1;
	  two:;
	  endcase  
       end
   
endmodule // query
